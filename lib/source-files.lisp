;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(defvar %source-files% (let ((a (make-hash-table :test #'eq
                                                 :weak t
                                                 :size 7000
                                                 :rehash-threshold .9)))
                         (do-all-symbols (s)
                           (let ((f (get s 'bootstrapping-source-files)))
                             (when f
                               (setf (gethash s a) f)
                               (remprop s 'bootstrapping-source-files))))
                         a))

(%fhave '%source-files (qlfun %source-files (name)
                         (gethash name %source-files%)))
(%fhave '%set-source-files (qlfun %set-source-files (name value)
                             (puthash name %source-files% value)))

;;; modified version of %method-applicable-p - args are class names
;;; not instances
(defun %my-method-applicable-p (method args cpls)
  (do ((specs (%method-specializers method) (cdr specs))
       (args args (cdr args))
       (cpls cpls (cdr cpls)))
      ((null specs) t)
    (declare (type list specs args cpls))
    (let ((spec (car specs)))
      (if (listp spec)
        (unless (equal (car args) spec)
          (return nil))
        (unless (memq spec (car cpls))
          (return nil))))))

;;; modified version of %compute-applicable-methods*
;;; omit errors and args are class names not instances
;;; returns a new list.
(defun find-applicable-methods (name args qualifiers)
  (let ((gf (fboundp name)))
    (when (and gf (typep gf 'standard-generic-function))
      (let* ((methods (%gf-methods gf))
             (args-length (length args))
             (bits (lfun-bits (closure-function gf)))  ; <<
             arg-count res)
        (when methods
          (setq arg-count (length (%method-specializers (car methods))))
          (unless (or (logbitp $lfbits-rest-bit bits)
                      (logbitp $lfbits-keys-bit bits)
                      (<= args-length 
                          (+ (ldb $lfbits-numreq bits) (ldb $lfbits-numopt bits))))
            (return-from find-applicable-methods))
          (cond 
           ((null args)
            (dolist (m methods res)
              (when (or (eq qualifiers t)
                        (equal qualifiers (%method-qualifiers m))) 
                (push m res))))
           ((%i< args-length arg-count)
            (let (spectails)
              (dolist (m methods)
                (let ((mtail (nthcdr args-length (%method-specializers m))))
                  (pushnew mtail spectails :test #'equal)))
              (dolist (tail spectails)
                (setq res 
                      (nconc res (find-applicable-methods 
                                  name 
                                  (append args (mapcar 
                                                #'(lambda (x) (if (consp x) x (class-name x)))
                                                tail))
                                  qualifiers))))
              (if (%cdr spectails)
                (delete-duplicates res :from-end t :test #'eq)
                res)))
           (t 
            (let ((cpls (make-list arg-count)))
              (declare (dynamic-extent cpls))
              (do ((args-tail args (cdr args-tail))
                   (cpls-tail cpls (cdr cpls-tail)))
                  ((null cpls-tail))
                (declare (type list args-tail cpls-tail))
                (let ((arg (car args-tail)) thing)
                  (if (consp arg)
                    (setq thing (class-of (cadr arg)))
                    (setq thing (find-class (or arg t) nil)))
                  (when thing
                    (setf (car cpls-tail)                
                          (%class-precedence-list thing)))))
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
                  methods))))))))))

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

(defun source-files-like-em (classes qualifiers method)
  (and (equal (canonicalize-specializers classes)
              (%method-specializers method))
       (or (eq qualifiers t)
           (equal qualifiers (%method-qualifiers method)))))

(defun parse-definition-spec (form)
  (let ((type t)
        name classes qualifiers)
    (cond
     ((consp form)
      (cond ((eq (car form) 'setf)
             (setq name form))
            (t (setq name (car form))
               (let ((last (car (last (cdr form)))))
                 (cond ((and (listp last)(or (null last)(neq (car last) 'eql)))
                        (setq classes last)
                        (setq qualifiers (butlast (cdr form))))
                       (t (setq classes (cdr form)))))                   
               (cond ((null qualifiers)
                      (setq qualifiers t))
                     ((equal qualifiers '(:primary))
                      (setq qualifiers nil))))))
     (t (setq name form)))
    (when (and (consp name)(eq (car name) 'setf))
        (setq name (or (%setf-method (cadr name)) name))) ; e.g. rplacd
    (when (not (or (symbolp name)
                   (setf-function-name-p name)))
      (return-from parse-definition-spec))
    (when (consp qualifiers)
      (mapc #'(lambda (q)
                (when (listp q)
                  (return-from parse-definition-spec)))
          qualifiers))
    (when classes
      (mapc #'(lambda (c)
                (when (not (and c (or (symbolp c)(and (consp c)(eq (car c) 'eql)))))
                  (return-from parse-definition-spec)))
            classes))            
    (when (or (consp classes)(consp qualifiers))(setq type 'method))
    (values type name classes qualifiers)))

(defun edit-definition-p (name &optional (type t) &aux specializers qualifiers the-method)
  (when (consp name)
    (multiple-value-setq (type name specializers qualifiers)
      (parse-definition-spec name)))
  (when (and specializers (consp specializers)) (setq type 'method))
  ; might be a method-function whose name is the method
  (when (typep name 'function)(setq name (function-name name)))
  (when (typep name 'method)
     (setq qualifiers (%method-qualifiers name)
           specializers (mapcar #'(lambda (s)
                                    (if (typep s 'class)
                                      (class-name s)
                                      s))
                                (%method-specializers name))
           the-method name
           name (%method-name name)
           type 'method))
  (let (files str newname)    
    (setq files (or (get-source-files-with-types&classes name type specializers qualifiers the-method)
                    (and 
                     (not the-method)
                     (symbolp name)
                     (or (and
                          (setq str (symbol-name name))
                          (memq (schar str (1- (length str))) '(#\.  #\, #\:))
                          (setq newname
                                (find-symbol (%substr str 0 (1- (length str)))
                                             (symbol-package name)))
                          (get-source-files-with-types&classes newname type specializers qualifiers))
))))         
  (when (and files newname) (setq name newname))
  (values files name type specializers qualifiers)))



;;; sym can be (setf blah)
(defun get-source-files-with-types&classes (sym &optional (type t) classes qualifiers the-method)
  (labels 
    ((merge-types (l)
       (let ((ftype (car l)))
         (cond
          ((eq ftype 'setf) ; it's (setf (function . file))
           (let ((res (mapcan #'merge-types (cdr l))))
             (if (typep (caar res) 'method)
               res
               (mapcar #'(lambda (x)(cons 'setf (cdr x))) res))))
          ((or (eq type t)(eq ftype type))
           (let* ((foo #'(lambda (x)
                           (when x
                             ; if x is consp it's (<method> file file ..)
                             (cond 
                              ((consp x)
                               (when (or (not (or classes qualifiers))
                                         (if the-method 
                                           (methods-match-p (car x) the-method)
                                           (source-files-like-em classes qualifiers
                                                                 (car x))))
                                 (merge-class x)))
                              (t (list (cons ftype x))))))))
             (declare (dynamic-extent foo))
             (mapcan foo (if (consp (cdr l)) (cdr l)(list (cdr l)))))))))
     (merge-class (l)
       (if (consp (cdr l))
         (mapcan 
          #'(lambda (x) 
              (when x (list (cons (car l) x))))
          (cdr l))
         (list l))))
    (declare (dynamic-extent #'merge-types)(special *direct-methods-only*))
    (let (files)
      (when (and (not the-method)(eq type 'method) classes (not *direct-methods-only*))
        (let ((methods (find-applicable-methods sym classes qualifiers)))          
          (when methods            
            (setq files (mapcan
                         #'(lambda (m)
                             (or (edit-definition-p m)(list (list m))))
                         methods)))))
      (if files files
          (let (setf-p result)
            (if (and (consp sym)(eq (car sym) 'setf))
              (setq sym (cadr sym) setf-p t))
            (setq result (%source-files sym))
            (if (not (consp result))
              (setq result
                    (if (not setf-p)
                      (if (or (eq type t)(eq type 'function))
                        `((function . ,result)))))
              (if setf-p (setq result (list (assq 'setf result)))))
            (mapcan #'merge-types result))))))
