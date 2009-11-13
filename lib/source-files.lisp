;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

;; If we're reloading this file, don't want to be calling functions from here with
;; only some of them redefined.  So revert to the bootstrapping version until the end.
(fset 'record-source-file #'level-1-record-source-file)

(defvar *source-files-lock* (make-lock "Source Files Lock"))

(defvar *unique-setf-names* (make-hash-table :test #'eq))

(defun canonical-maybe-setf-name (name)
  (if (setf-function-name-p name)
    (let ((tem (%setf-method (%cadr name))))
      (if (non-nil-symbol-p tem) ;; e.g. (setf car) => set-car
        tem
        (or (gethash (%cadr name) *unique-setf-names*)
            (setf (gethash (%cadr name) *unique-setf-names*) (list 'setf (%cadr name))))))
    name))

(defgeneric name-of (thing)
  (:method ((thing t)) thing)
  (:method ((thing method-function)) (name-of (%method-function-method thing)))
  (:method ((thing function)) (name-of (function-name thing)))
  (:method ((thing method)) `(:method ,(method-name thing) ,@(method-qualifiers thing) ,(method-specializers thing)))
  (:method ((thing class)) (class-name thing))
  (:method ((thing method-combination)) (method-combination-name thing))
  (:method ((thing package)) (package-name thing))
  (:method ((thing eql-specializer)) `(eql ,(eql-specializer-object thing))))

;; This used to be weak, but the keys are symbols-with-definitions, so why bother.
;; Set a high rehash threshold because space matters more than speed here.
;; Do not use lock-free hash tables, because they optimize reads at the expense of
;; writes/rehashes.  Writes/rehashes affect file-compilation speed, which matters.
(defvar %source-files% (make-hash-table :test #'eq
                                        :size 14000
                                        :rehash-size 1.8 ;; compensate for high threshold
                                        :rehash-threshold .95
                                        :lock-free nil))



(defvar *direct-methods-only* t
  "If true, method name source location lookup will find direct methods only.  If false,
   include all applicable methods")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Definition types
;;
;; Definition types are uniquely identified by a symbol, but are implemented as
;; classes so they can inherit/customize behavior.  They have no instances other
;; than the class prototype, which is used to invoke methods.
;;

(defgeneric definition-type-name (def-type)
  (:documentation "The preferred user-visible name of the def-type.  Used for
error messages etc.  The default method returns the name specified in
define-definition-type."))

(defclass definition-type ()
  ((name :allocation :class :reader definition-type-name :initform t))
  (:documentation "Superclass of all definition types"))

(defgeneric definition-base-name (def-type def)
  ;; Note that a def can have multiple base names, but each one needs a different def-type
  (:documentation "Return the name that, when the user asks for all definitions of that
name, this def should be included.  Typically this is a symbol.  It's used as a key in
an EQ hash table, so must return EQ values for equivalent definitions.
The default method returns the rightmost atom in name")
  (:method ((dt definition-type) name)
    (while (consp name)
      (let ((x (last name)))
        (setq name (or (cdr x) (car x)))))
    name))

(defgeneric definition-same-p (def-type def1 def2)
  (:documentation "Returns true if the two definitions are equivalent, i.e. one should
replace the other.  The default method calls EQUAL.")
  (:method ((dt definition-type) name1 name2)
    (equal name1 name2)))

(defgeneric definition-bound-p (def-type def)
  (:documentation "Returns true if def is currently defined.  Used to decide whether to issue
redefinition warnings.  The default method returns T.")
  (:method ((dt definition-type) name)
    (declare (ignore name))
    t))

;;;;;;;;;;

(defvar *definition-types* ()
  "alist of all known definition type names and their class prototypes")

(defmethod print-object ((dt definition-type) stream)
  (if *print-escape*
    (let ((definedp (class-name (class-of dt))))
      (print-unreadable-object (dt stream :type definedp :identity t)
        (unless definedp
          (format stream "#:~s " 'definition-type)) ;; subtly indicate it's a subclass...
        (format stream "~s" (definition-type-name dt))))
    (format stream "~s" (definition-type-name dt))))

(defmethod name-of ((thing definition-type))
  (definition-type-name thing))

(defmacro define-definition-type (name supers &rest options)
  "Defines a class named name-DEFINITION-TYPE and registers it as the class of
definition type NAME"
  (loop with known-keys = '( ;; Backward compatibility
                            #+ccl-0711 :default-name-function)
        for (key . nil) in options
        unless (memq key known-keys)
          do (signal-program-error "Unknown option ~s" key))
  (let ((class-name (intern (%str-cat (symbol-name name) "-DEFINITION-TYPE"))))
    `(progn
       (defclass ,class-name ,(or supers '(definition-type))
         ((name :allocation :class :initform ',name)))
       (record-source-file ',name 'definition-type)
       (register-definition-type (find-class ',class-name) '(,name)))))

(defun register-definition-type (class names)
  (let ((instance (class-prototype class)))
    (with-lock-grabbed (*source-files-lock*)
      ;; If had a previous definition, the defclass will signal any duplicate
      ;; definition warnings, so here just silently replace previous one.
      (without-interrupts
        (setq *definition-types*
              (remove instance *definition-types* :key #'cdr)))
      (loop for name in names
            unless (without-interrupts
                     (unless (assq name *definition-types*)
                       (push (cons name instance) *definition-types*)))
              do (error "There is already a different definition type ~s named ~s"
                        (cdr (assq name *definition-types*))
                        name)))
    ;; Return instance for use in make-load-form
    instance))

(defun auto-create-definition-type (name)
  ;; Use an anonymous class, so this means can't write methods on it.
  ;; If you want to write methods on it, use define-definition-type first.
  (let* ((super (find-class 'definition-type))
         (new-class (make-instance (class-of super)
                      :direct-superclasses (list super)
                      :direct-slots `((:name name
                                       :allocation :class
                                       :initform ',name
                                       :initfunction ,(constantly name))))))
    (register-definition-type new-class (list name))
    (class-prototype new-class)))

(defmethod definition-type-instance ((dt definition-type) &key (if-does-not-exist :error))
  (if (rassoc dt *definition-types* :test #'eq)
    dt
    (ecase if-does-not-exist
      ((nil) nil)
      ((:error) (error "~s is not a known definition-type" dt)))))

(defmethod definition-type-instance ((name symbol) &key (if-does-not-exist :error))
  (or (cdr (assq name *definition-types*))
      (ecase if-does-not-exist
        ((nil) nil)
        ((:error) (error "~s is not a known definition-type" name))
        ((:create) (auto-create-definition-type name)))))

(defmethod definition-type-instance ((class class) &key (if-does-not-exist :error))
  (definition-type-instance (class-prototype class) :if-does-not-exist if-does-not-exist))

(defmethod make-load-form ((dt definition-type) &optional env)
  (declare (ignore env))
  (let ((names (loop for (name . instance) in *definition-types*
                     when (eq dt instance) collect name)))
    `(register-definition-type ',(class-of dt) ',names)))


(register-definition-type (find-class 'definition-type) '(t))

(defparameter *t-definition-type* (definition-type-instance 't))

(define-definition-type function ())

(defparameter *function-definition-type* (definition-type-instance 'function))

(defmethod definition-base-name ((dt function-definition-type) name)
  (while (and (consp name) (not (setf-function-name-p name)))
    (let ((x (last name)))
      (or (setq name (cdr x))
          ;; Try to detect the (:internal .... <hairy-method-name>) case
          (when (and (setq name (car x))
                     ;;check for plausible method name
                     (setq x (method-def-parameters name))
                     (neq x 'setf)
                     (not (keywordp x)))
            (setq name x)))))
  (canonical-maybe-setf-name name))

(defmethod definition-bound-p ((dt function-definition-type) name)
  (and (or (symbolp name) (setf-function-name-p name))
       (or (fboundp name)
           ;; treat long-form setf expanders like macros.
           (and (consp name) (functionp (%setf-method (cadr name)))))))

(define-definition-type macro (function-definition-type))

(define-definition-type compiler-macro (macro-definition-type))

(define-definition-type symbol-macro (macro-definition-type))

(define-definition-type setf-expander (macro-definition-type))

(define-definition-type generic-function (function-definition-type))

(define-definition-type method ())

(defparameter *method-definition-type* (definition-type-instance 'method))

(defmethod definition-base-name ((dt method-definition-type) (name cons))
  (if (setf-function-name-p name)
    (canonical-maybe-setf-name name)
    (definition-base-name *function-definition-type* (car name))))

;; defmethod passes the actual method into record-source-file
(defmethod definition-base-name ((dt method-definition-type) (method method))
  (definition-base-name dt (method-name method)))

(defmethod definition-base-name ((dt method-definition-type) (fn method-function))
  (definition-base-name dt (function-name fn)))

(defmethod definition-same-p ((dt method-definition-type) m1 m2)
  (multiple-value-bind (n1 q1 s1) (method-def-parameters m1)
    (multiple-value-bind (n2 q2 s2) (method-def-parameters m2)
      (and (definition-same-p *function-definition-type* n1 n2)
           (equal q1 q2)
           (eql (length s1) (length s2))
           (every #'(lambda (s1 s2)
                      (or (equal s1 s2)
                          (progn
                            (when (symbolp s2) (rotatef s1 s2))
                            (and (symbolp s1)
                                 (classp s2)
                                 (or (eq (find-class s1 nil) s2)
                                     (eq s1 (class-name s2)))))))
                  s1 s2)))))

(defmethod definition-bound-p ((dt method-definition-type) meth &aux fn)
  (when (setq fn (method-def-parameters meth))
    (loop for m in (and (setq fn (fboundp fn))
                        (typep fn 'generic-function)
                        (generic-function-methods fn))
          thereis (definition-same-p dt meth m))))

(define-definition-type reader-method (method-definition-type))

(define-definition-type writer-method (method-definition-type))

(define-definition-type callback (function-definition-type))

(define-definition-type structure-accessor (function-definition-type))

(define-definition-type type ())

(define-definition-type class ())

(defmethod definition-bound-p ((dt class-definition-type) name)
  (and (non-nil-symbol-p name) (find-class name nil)))

(define-definition-type condition (class-definition-type))

(define-definition-type structure ())

(define-definition-type definition-type ())

(defmethod definition-bound-p ((dt definition-type-definition-type) name)
  (definition-type-instance name :if-does-not-exist nil))

(define-definition-type method-combination ())

(define-definition-type variable ())

(defmethod definition-bound-p ((dt variable-definition-type) name)
  (and (non-nil-symbol-p name) (boundp name)))

(define-definition-type constant (variable-definition-type))

(define-definition-type package ())

(defmethod definition-base-name ((dt package-definition-type) name)
  (if (or (stringp name) (non-nil-symbol-p name))
    (intern (string name) :keyword)
    name))

(defmethod definition-bound-p ((dt package-definition-type) name)
  (and (or (stringp name) (symbolp name))
       (find-package (string name))))

(defmethod definition-same-p ((dt package-definition-type) d1 d2)
  (and (or (stringp d1) (symbolp d1))
       (or (stringp d2) (symbolp d2))
       (equal (string d1) (string d2))))


;;;;;;;;;;;

(declaim (inline default-definition-type))

(defun default-definition-type (name)
  (if (typep name 'method)
    *method-definition-type*
    *function-definition-type*))

;; remember & reuse last few (TYPE . file) entries
(let ((cache (make-list 10 :initial-element nil)))
  (defun type-file-cons (type files)
    (loop for prev = nil then p for p = cache then (cdr p)
          do (when (or (and (eq type (caar p)) (equal files (cdar p)))
                       (and (null (cdr p))
                            (setf (car p) (cons type files))))
               (when prev ;; move to front unless already there
                 (setf (cdr prev) (cdr p))
                 (setf (cdr p) cache)
                 (setq cache p))
               (return (car p))))))

(defun %source-file-entries (key)
  (let ((data (gethash key %source-files%)))
    (if (and (listp data)
             (listp (%cdr data)))
      data
      (list data))))

(defun %set-source-file-entries (key list &aux data)
  (setf (gethash key %source-files%)
        (if (and list
                 (null (cdr list))
                 ;; One element, but make sure can recognize it.
                 (not (and (listp (%car list))
                           (listp (%cdar data)))))
          (car list)
          list)))

(defun make-def-source-entry (key type name files)
  (setq files (if (or (%cdr files) (listp (%car files))) files (%car files)))
  (cond ((eq type (default-definition-type name))
         (if (and (eq name key) (atom files))
           files
           (cons name files)))
        ((eq name key)
         (type-file-cons type files))
        (t
         (cons (cons type name) files))))

(defun decode-def-source-entry (key entry)
  (if (atom entry)
    (and entry (values (default-definition-type key) key (list entry)))
    (let* ((file-or-files (%cdr entry))
           (files (if (consp file-or-files) file-or-files (list file-or-files))))
      (cond ((typep (%car entry) 'definition-type)
             (values (%car entry) key files))
            ((and (consp (%car entry)) (typep (%caar entry) 'definition-type))
             (values (%caar entry) (%cdar entry) files))
            (t
             (values (default-definition-type (%car entry)) (%car entry) files))))))

(defun def-source-entry.name (key entry)
  (assert (not (null entry)))
  (cond ((atom entry) key)
        ((typep (%car entry) 'definition-type) key)
        ((and (consp (%car entry)) (typep (%caar entry) 'definition-type))
         (%cdar entry))
        (t
         (%car entry))))

(defun def-source-entry.type (key entry)
  (cond ((atom entry) (default-definition-type key))
        ((typep (%car entry) 'definition-type) (%car entry))
        ((and (consp (%car entry)) (typep (%caar entry) 'definition-type))
         (%caar entry))
        (t
         (default-definition-type (%car entry)))))

(defun def-source-entry.sources (key entry)
  (declare (ignore key))
  (cond ((consp entry)
         (if (consp (%cdr entry)) (%cdr entry) (list (%cdr entry))))
        (entry (list entry))
        (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 


;; Some objects (specifically functions) have source location information associated with the
;; object itself, in addition to any source locations associated with its definition.  This
;; allows us to find source for, e.g., anonymous functions.
(defgeneric get-object-sources (thing)
  ;; returns a list of entries ((a-type . a-name) source . previous-sources)
  (:method ((thing t)) nil)
  (:method ((fn function))
    (let ((source (function-source-note fn)))
      (when source
        (list (list* (cons *function-definition-type* (or (name-of fn) fn)) source nil)))))
  (:method ((fn method-function))
    (let ((source (function-source-note fn)))
      (when source
        (list (list* (cons *method-definition-type* (%method-function-method fn)) source nil)))))
  (:method ((m method))
    (get-object-sources (method-function m))))

(defun find-definition-sources (name &optional (type t))
  "Returns a list of entries ((a-type . a-name) source . previous-sources), where
a-type is a subtype of TYPE, and a-name is either NAME or it's a special case of
NAME (e.g. if NAME is the name of generic function, a-name could be a method of NAME).

If NAME is not a cons or symbol, it's assumed to be an object (e.g. class or
function) whose source location we try to heuristically locate, usually by looking up
the sources of its name.

If NAME is a method name and *DIRECT-METHODS-ONLY* is false, will also locate all
applicable methods.

The returned list is guaranteed freshly consed (ie suitable for nconc'ing)."

  (let* ((dt-class (class-of (definition-type-instance type)))
         (matches (get-object-sources name)))
    (if matches
      (setq matches (delete-if-not (lambda (info) (typep (caar info) dt-class)) matches))
      ;; No intrinsic source info for the thing itself, look it up by name.
      (let (seen-dts implicit-type implicit-dt-class implicit-name)
        (typecase name
          (method
             (setq implicit-type 'method implicit-name name))
          (method-function
             (setq implicit-type 'method implicit-name (%method-function-method name)))
          (function
             (setq implicit-type 'function implicit-name (name-of name)))
          (method-combination
             (setq implicit-type 'method-combination implicit-name (name-of name)))
          (package
             (setq implicit-type 'package implicit-name (name-of name)))
          (class
             (setq implicit-type 'class implicit-name (name-of name)))
          (t
           (locally
               (declare (ftype function xref-entry-p xref-entry-full-name xref-entry-type))
             (if (and (find-class 'xref-entry nil)
                      (xref-entry-p name))
               (setq implicit-type (xref-entry-type name) implicit-name (xref-entry-full-name name))
               (setq implicit-type t implicit-name name)))))
        (setq implicit-dt-class (class-of (definition-type-instance implicit-type)))
        (with-lock-grabbed (*source-files-lock*)
          (loop for (nil . dt) in *definition-types*
                when (and (typep dt dt-class) (typep dt implicit-dt-class) (not (memq dt seen-dts)))
                  do (let* ((key (definition-base-name dt implicit-name))
                            (all (%source-file-entries key)))
                       (push dt seen-dts)
                       (loop for entry in all
                             when (and (eq dt (def-source-entry.type key entry))
                                       (or (eq implicit-name key) ;; e.g. all methods on a gf
                                           (definition-same-p dt implicit-name (def-source-entry.name key entry))))
                               do (multiple-value-bind (type name files)
                                      (decode-def-source-entry key entry)
                                    (push (cons (cons type name) files) matches))))))))

    ;; include indirect applicable methods.  Who uses this case?
    (when (and (eq type 'method)
               (not (typep name 'method))
               (not *direct-methods-only*))
      (multiple-value-bind (sym qualifiers specializers) (method-def-parameters name)
        (when sym
          (loop for m in (find-applicable-methods sym specializers qualifiers)
                unless (definition-same-p *method-definition-type* m name)
                  do (setq matches (nconc (find-definition-sources m 'method) matches))))))
    matches))

;;; backward compatibility

;;; modified version of %method-applicable-p - args are class names
;;; not instances
(defun %my-method-applicable-p (method args cpls)
  (do* ((specs (%method-specializers method) (%cdr specs))
        (args args (%cdr args))
        (cpls cpls (%cdr cpls)))
      ((null args) t)
    (let ((spec (%car specs))
          (arg (%car args)))
      (if (typep spec 'eql-specializer)
        (if (consp arg)
          (unless (eql (cadr arg) (eql-specializer-object spec))
            (return nil))
          (if (typep (eql-specializer-object spec) arg)
            ;(unless (eq arg *null-class*) (return :undecidable))
            t  ;; include if it's at all possible it might be applicable.
            (return nil)))
        (unless (memq spec (%car cpls))
          (return nil))))))

;;; modified version of %compute-applicable-methods*
;;; omit errors and args are class names not instances
;;; returns a new list.
(defun find-applicable-methods (name args qualifiers)
  (let ((gf (fboundp name)))
    (when (and gf (typep gf 'standard-generic-function))
      (let* ((methods (or (%gf-methods gf)
                          (return-from find-applicable-methods nil)))
             (arg-count (length (%method-specializers (car methods))))
             (args-length (length args))
             (bits (inner-lfun-bits gf))
             res)
        (unless (or (logbitp $lfbits-rest-bit bits)
                    (logbitp $lfbits-restv-bit bits)
                    (logbitp $lfbits-keys-bit bits)
                    (<= args-length 
                        (+ (ldb $lfbits-numreq bits) (ldb $lfbits-numopt bits))))
                                        ;(error "Too many args for ~s" gf)
          (return-from find-applicable-methods))
        (when (< arg-count args-length)
          (setq args (subseq args 0 (setq args-length arg-count))))
        (setq args (mapcar (lambda (arg)
                             (typecase arg
                               (eql-specializer `(eql ,(eql-specializer-object arg)))
                               (class arg)
                               (symbol (or (find-class (or arg t) nil)
                                           ;;(error "Invalid class name ~s" arg)
                                           (return-from find-applicable-methods)))
                               (t
                                  (unless (and (consp arg) (eql (car arg) 'eql) (null (cddr arg)))
                                    ;;(error "Invalid specializer ~s" arg)
                                    (return-from find-applicable-methods))
                                  arg)))
                           args))
        (let ((cpls (make-list args-length)))
          (declare (dynamic-extent cpls))
          (do ((args-tail args (cdr args-tail))
               (cpls-tail cpls (cdr cpls-tail)))
              ((null cpls-tail))
            (declare (type list args-tail cpls-tail))
            (let ((arg (car args-tail)))
              (setf (car cpls-tail)
                    (%class-precedence-list (if (consp arg)
                                              (class-of (cadr arg))
                                              arg)))))
          (dolist (m methods)
            (when (%my-method-applicable-p m args cpls)
              (push m res)))
          (let ((methods (sort-methods res cpls (%gf-precedence-list gf))))
            (when (eq (generic-function-method-combination gf)
                      *standard-method-combination*)
                                        ; around* (befores) (afters) primaries*
              (setq methods (compute-method-list methods))
              (when methods
                (setq methods
                      (if (not (consp methods))
                        (list methods)
                        (let ((afters (cadr (member-if #'listp methods))))
                          (when afters (nremove afters methods))
                          (nconc
                           (mapcan #'(lambda (x)(if (listp x) x (cons x nil)))
                                   methods)
                           afters))))))
            (if (and qualifiers (neq qualifiers t))
              (delete-if #'(lambda (m)(not (equal qualifiers (%method-qualifiers m))))
                         methods)
              methods)))))))

;;; Do this just in case record source file doesn't remember the right
;;; definition
(defun methods-match-p (x y)  
  (or (eq x y)
      (and (typep x 'method)
           (typep y 'method)
           (equal (method-name x)
                  (method-name y))
           (equal (method-specializers x)
                  (method-specializers y))
           (equal (method-qualifiers x)
                  (method-qualifiers y)))))

(defun edit-definition-p (name &optional (type t)) ;exported
  (let ((specs (get-source-files-with-types name type)))
    (when (and (null specs)
               (symbolp name))
      (let* ((str (symbol-name name))
             (len (length str)))
        (when (and (> len 0) (memq (char str (1- len)) '(#\. #\, #\:)))
          (let ((newsym (find-symbol (%substr str 0 (1- len)) (symbol-package name))))
            (when newsym
              (setq specs (get-source-files-with-types newsym type)))))))
    specs))

(defun get-source-files-with-types (name &optional (type t))
  (let ((list (find-definition-sources name type)))
    ;; Convert to old format, (type-or-name . file)
    (loop for ((dt . full-name) . sources) in list
          as spec = (if (eq full-name name) (definition-type-name dt) full-name)
          nconc (mapcan (lambda (s)
                          (when s (list (cons spec (source-note-filename s)))))
                        sources))))


;; For ilisp.
(defun %source-files (name)
  (let ((type-list ())
        (meth-list ()))
    (loop for ((dt . full-name) . sources) in (find-definition-sources name t)
          as files = (mapcan #'(lambda (s)
                                 (and s (setq s (source-note-filename s)) (list s)))
                             sources)
          when files
            do (if (typep dt 'method-definition-type)
                 (dolist (file files)
                   (push (cons full-name file) meth-list))
                 (push (cons (definition-type-name dt) files) type-list)))
    (when meth-list
      (push (cons 'method meth-list) type-list))
    type-list))

;; For CVS slime as of 11/15/2008.
(defun get-source-files-with-types&classes (sym &optional (type t) classes qualifiers the-method)
  (let* ((name (or the-method
                   (and (or (eq type 'method) classes qualifiers)
                        `(sym ,@qualifiers ,classes))
                   sym)))
    (get-source-files-with-types name type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; record-source-file

;; Returns nil if not a method/method name
(defun method-def-parameters (m)
  (when (typep m 'method-function)
    (setq m (%method-function-method m)))
  (if (typep m 'method)
    (values (method-name m)
            (method-qualifiers m)
            (method-specializers m))
    (let (name quals specs data last)
      (when (consp m)
        (when (eq (car m) :method) (setq m (cdr m)))
        ;; (name spec1 .. specn) or (name qual1 .. qualn (spec1 ... specn))
        (setq data (cdr m) last (last data))
        (when (null (cdr last))
          (setq last (car last))
          (if (and (listp last) (neq (car last) 'eql))
            (setq quals (butlast data) specs last)
            (setq specs data))
          (setq name (car m))
          (when (and (or (non-nil-symbol-p name) (setf-function-name-p name))
                     (every #'(lambda (q) (not (listp q))) quals)
                     (every #'(lambda (s)
                                (or (non-nil-symbol-p s)
                                    (classp s)
                                    (and (consp s)
                                         (consp (cdr s))
                                        (null (cddr s))
                                         (eq (car s) 'eql))))
                            specs))
            (values name quals specs)))))))

(defmethod record-definition-source ((dt definition-type) name source)
  (let* ((key (definition-base-name dt name))
         (all (%source-file-entries key))
         (e-loc nil)
         (e-files nil))
    (loop for ptr on all as entry = (car ptr)
          do (when (and (eq dt (def-source-entry.type key entry))
                        (definition-same-p dt name (def-source-entry.name key entry)))
               (setq e-files (def-source-entry.sources key entry))
               (let ((old (flet ((same-file (x y)
                                   (setq x (source-note-filename x))
                                   (setq y (source-note-filename y))
                                   (or (equal x y)
                                       (and x
                                            y
                                            (or (stringp x) (pathnamep x))
                                            (or (stringp y) (pathnamep y))
                                            (equal
                                             (or (probe-file x) (full-pathname x))
                                             (or (probe-file y) (full-pathname y)))))))
                            (member source e-files :test #'same-file))))
                 (when (and old (neq source (car e-files))) ;; move to front
                   (setq e-files (cons source (remove (car old) e-files :test #'eq)))))
               (return (setq e-loc ptr))))
    (unless (and e-files (eq source (car e-files)))
      ;; Never previously defined in this file
      (when (and (car e-files)            ; don't warn if last defined interactively
                 *warn-if-redefine*
                 (definition-bound-p dt name))
        (warn "~A ~S previously defined in: ~A is now being redefined in: ~A~%"
              (definition-type-name dt)
              name
              (source-note-filename (car e-files))
              (or (source-note-filename source) "{No file}")))
      (setq e-files (cons source e-files)))
    (let ((entry (make-def-source-entry key dt name e-files)))
      (if e-loc
        (setf (car e-loc) entry)
        (push entry all))
      (%set-source-file-entries key all))
    name))

(defmethod record-definition-source ((dt method-definition-type) (m method) source)
  ;; In cases of non-toplevel method definitions, as in the expansion of defgeneric,
  ;; the method function note has more specific info than *loading-toplevel-location*.
  (call-next-method dt m (or (function-source-note (method-function m)) source)))

;;; avoid hanging onto beezillions of pathnames
(defparameter *last-back-translated-name* (cons nil nil))

;; Define the real record-source-file, which will be the last defn handled by the
;; bootstrapping record-source-file, so convert all queued up data right afterwards.
(progn

(defun record-source-file (name def-type &optional (source (or *loading-toplevel-location*
                                                               *loading-file-source-file*)))
  (when (and source *record-source-file*)
    (with-lock-grabbed (*source-files-lock*)
      (let ((file-name (source-note-filename source)))
        (when file-name
          (unless (equalp file-name (car *last-back-translated-name*))
            (setf (car *last-back-translated-name*) file-name)
            (setf (cdr *last-back-translated-name*)
                  (if (physical-pathname-p file-name)
                    (namestring (back-translate-pathname file-name))
                    file-name)))
          (setq file-name (cdr *last-back-translated-name*))
          (if (source-note-p source)
            (setf (source-note-filename source) file-name)
            (setq source file-name))))
      (when (eq def-type 't) (report-bad-arg def-type '(not (eql t))))
      (record-definition-source (definition-type-instance def-type
                                    :if-does-not-exist :create)
                                name
                                source))))

;; Collect level-0 source file info
(do-all-symbols (s)
  (let ((f (get s 'bootstrapping-source-files)))
    (when f
      (if (consp f)
        (destructuring-bind ((type . source)) f
          (when source (record-source-file s type source)))
        (record-source-file s 'function f))
      (remprop s 'bootstrapping-source-files))))

;; Collect level-1 source file info
(when (consp *record-source-file*)
  (let ((list (nreverse (shiftf *record-source-file* t))))
    (while list
      (apply #'record-source-file (pop list)))))
)
