;;;-*-Mode: LISP; Package: CCL -*-
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

;;; Defstruct.lisp

(eval-when (eval compile)
  (require 'defstruct-macros)

)

(defvar %structure-refs% (make-hash-table :test #'eq))
(defvar %defstructs% (make-hash-table :test #'eq))

(defun make-ssd (name initform offset r/o &optional (type t))
  (let ((refinfo (%ilogior2 offset (if r/o #x1000000 0))))
    (list* name initform
           (if (eq type 't)
             refinfo
             (cons type refinfo)))))

(declaim (inline type-and-refinfo-p))
(defun type-and-refinfo-p (object)
  (or (fixnump object) (consp object)))

(defun ssd-set-reftype (ssd reftype)
  (ssd-update-refinfo (ssd refinfo)
                      (%ilogior2 (%ilogand2 #x300FFFF refinfo)
                                 (%ilsl 16 reftype))))

(defun ssd-set-r/o (ssd) 
  (ssd-update-refinfo (ssd refinfo)
                      (%ilogior2 #x1000000 refinfo)))

(defun ssd-set-inherited (ssd)
  (ssd-update-refinfo (ssd refinfo)
		       (bitset $struct-inherited refinfo)))

(defun copy-ssd (ssd)
  (let* ((cdr (cdr ssd))
         (cddr (cdr cdr)))
    (list* (%car ssd) (%car cdr)
           (if (consp cddr)
             (list* (%car cddr) (%cdr cddr))
             cddr))))

(declaim (inline ssd-type-and-refinfo))
(defun ssd-type-and-refinfo (ssd)
  (cddr ssd))

(defun ssd-type (ssd)
  (let ((type-and-refinfo (ssd-type-and-refinfo ssd)))
    (if (consp type-and-refinfo)
      (%car type-and-refinfo)
      't)))

(defun ssd-refinfo (ssd)
  (let ((type-and-refinfo (ssd-type-and-refinfo ssd)))
    (if (consp type-and-refinfo) (%cdr type-and-refinfo) type-and-refinfo)))

(defun %structure-class-of (thing)
  (let* ((cell (car (uvref thing 0))))
    (or (class-cell-class cell)
        (setf (class-cell-class cell)
              (find-class (class-cell-name cell))))))

;These might want to compiler-transform into non-typechecking versions...
(defun struct-ref (struct offset)
  (if (structurep struct) (uvref struct offset)
      (report-bad-arg struct 'structure-object)))

(defun struct-set (struct offset value)
  (if (structurep struct) (uvset struct offset value)
      (report-bad-arg struct 'structure-object)))

(defsetf struct-ref struct-set)


; things for defstruct to do - at load time
(defun %defstruct-do-load-time (sd predicate &optional doc &aux (name (sd-name sd)))
  ;(declare (ignore refnames))
  (when (null (sd-type sd))
    (%define-structure-class sd))
  (when (and doc *save-doc-strings*)
    (set-documentation name 'type doc))  
  (puthash name %defstructs% sd)
  (record-source-file name 'structure)
  (when (and predicate (null (sd-type sd)))
    (puthash predicate %structure-refs% name))  
  (when *fasload-print* (format t "~&~S~%" name))
  name)

(defun %defstruct-set-print-function (sd print-function print-object-p)
  (sd-set-print-function sd (if print-object-p
			      (list print-function)
			      print-function)))


(defun sd-refname-pos-in-included-struct (sd name)
  (dolist (included-type (cdr (sd-superclasses sd)))
    (let ((sub-sd (gethash included-type %defstructs%)))
      (when sub-sd
        (let ((refnames (sd-refnames sub-sd)))
          (if refnames
            (let ((pos (position name refnames :test 'eq)))
              (and pos (1+ pos)))
            (dolist (slot (sd-slots sub-sd))
              (let ((ssd-name (ssd-name slot)))
                (unless (fixnump ssd-name)
                  (when (eq name ssd-name)
                    (return-from sd-refname-pos-in-included-struct
                      (ssd-offset slot))))))))))))

;;; return stuff for defstruct to compile
(defun %defstruct-compile (sd refnames env)
  (let ((stuff))    
    (dolist (slot (sd-slots sd))
      (unless (fixnump (ssd-name slot))
        (let* ((accessor (if refnames (pop refnames) (ssd-name slot)))
               (pos (sd-refname-pos-in-included-struct sd accessor)))
          (if pos
            (let ((offset (ssd-offset slot)))
              (unless (eql pos offset)
                ; This should be a style-warning
                (warn "Accessor ~s at different position than in included structure"
                      accessor)))
            (let ((fn (slot-accessor-fn slot accessor env)))
              (push
               `(progn
                  ,.fn
                  (puthash ',accessor %structure-refs% ',(ssd-type-and-refinfo slot))
                  (record-source-file ',accessor 'structure-accessor))
               stuff))))))
    (nreverse stuff)))


; no #. for cross compile
(defvar *struct-ref-vector* 
  (vector #'(lambda (x) (struct-ref x 0))
          #'(lambda (x) (struct-ref x 1))
          #'(lambda (x) (struct-ref x 2))
          #'(lambda (x) (struct-ref x 3))
          #'(lambda (x) (struct-ref x 4))
          #'(lambda (x) (struct-ref x 5))
          #'(lambda (x) (struct-ref x 6))
          #'(lambda (x) (struct-ref x 7))
          #'(lambda (x) (struct-ref x 8))
          #'(lambda (x) (struct-ref x 9))))

(defvar *svref-vector*
  (vector #'(lambda (x) (svref x 0))
          #'(lambda (x) (svref x 1))
          #'(lambda (x) (svref x 2))
          #'(lambda (x) (svref x 3))
          #'(lambda (x) (svref x 4))
          #'(lambda (x) (svref x 5))
          #'(lambda (x) (svref x 6))
          #'(lambda (x) (svref x 7))
          #'(lambda (x) (svref x 8))
          #'(lambda (x) (svref x 9))))


;;; too bad there isnt a way to suppress generating these darn
;;; functions when you dont want them.  Makes no sense to fetch
;;; functions from a vector of 68K functions and send them over to
;;; PPC.  So can use that space optimization iff host and target are
;;; the same.


(defparameter *defstruct-share-accessor-functions* t)   ;; TODO: isn't it time to get rid of this?

(defun slot-accessor-fn (slot name env &aux (ref (ssd-reftype slot)) (offset (ssd-offset slot)))
  (cond ((eq ref $defstruct-nth)
         (if (and  (%i< offset 10) *defstruct-share-accessor-functions*)
           `((eval-when (:compile-toplevel)
               (record-function-info ',name ',*one-arg-defun-def-info* ,env))
              (fset ',name
                    ,(symbol-function
                      (%svref '#(first second third fourth fifth
                                 sixth seventh eighth ninth tenth) offset))))
           `((defun ,name (x)  (nth ,offset x)))))
        ((eq ref $defstruct-struct)
         (if (and (%i< offset 10) *defstruct-share-accessor-functions*)
           `((eval-when (:compile-toplevel)
               (record-function-info ',name ',*one-arg-defun-def-info* ,env))                
             (fset ',name , (%svref *struct-ref-vector* offset)))
           `((defun ,name (x)  (struct-ref x ,offset)))))
        ((or (eq ref target::subtag-simple-vector)
             (eq ref $defstruct-simple-vector))
         (if (and (%i< offset 10) *defstruct-share-accessor-functions*)
           `((eval-when (:compile-toplevel)
               (record-function-info ',name ',*one-arg-defun-def-info* ,env))
             (fset ',name ,(%svref *svref-vector* offset)))
           `((defun ,name (x)  (svref x ,offset)))))
        (t `((defun ,name (x) (uvref x ,offset))))))

(defun defstruct-reftype (type)
  (cond ((null type) $defstruct-struct)
        ((eq type 'list) $defstruct-nth)
        (t (element-type-subtype (cadr type)))))

(defun defstruct-slot-defs (sd refnames env)
  (declare (ignore env))
  (let ((ref (defstruct-reftype (sd-type sd))) name defs)
    (dolist (slot (sd-slots sd))
      (ssd-set-reftype slot ref)
      (unless (fixnump (setq name (ssd-name slot))) ;Ignore fake 'name' slots
        (when refnames (setq name (pop refnames)))
        (unless (sd-refname-pos-in-included-struct sd name)
          (push name defs))))
    (setq defs (nreverse defs))
    `((declaim (inline ,@defs)))))

;;;Used by nx-transform, setf, and whatever...
(defun defstruct-ref-transform (predicate-or-type-and-refinfo args &optional env)
  (if (type-and-refinfo-p predicate-or-type-and-refinfo)
    (multiple-value-bind (type refinfo)
                         (if (consp predicate-or-type-and-refinfo)
                           (values (%car predicate-or-type-and-refinfo)
                                   (%cdr predicate-or-type-and-refinfo))
                           (values 't predicate-or-type-and-refinfo))
      (let* ((offset (refinfo-offset refinfo))
             (ref (refinfo-reftype refinfo))
             (accessor
              (cond ((eq ref $defstruct-nth)
                     `(nth ,offset ,@args))
                    ((eq ref $defstruct-struct)
                     `(struct-ref ,@args ,offset))
                    ((eq ref target::subtag-simple-vector)
                     `(svref ,@args ,offset))
                    (ref
                     `(aref (the (simple-array ,(element-subtype-type ref) (*))
                                 ,@args) ,offset))
                    (t `(uvref ,@args ,offset)))))
        (if (eq type 't)
          accessor
          (if (specifier-type-if-known type env)
            `(the ,type ,accessor)
            (if (nx-declarations-typecheck env)
              `(require-type ,accessor ',type)
              ;; Otherwise just ignore the type, it's most likely a forward reference,
              ;; and while it means we might be missing out on a possible optimization,
              ;; most of the time it's not worth warning about.
              accessor)))))
    `(structure-typep ,@args ',predicate-or-type-and-refinfo)))

;;; Should probably remove the constructor, copier, and predicate as
;;; well. Can't remove the inline proclamations for the refnames,
;;; as the user may have explicitly said this. Questionable - but surely
;;; must delete the inline definitions.
;;; Doesn't remove the copier because we don't know for sure what it's name is
(defmethod change-class ((from structure-class)
			 (to class)
			  &rest initargs &key &allow-other-keys)
  (declare (dynamic-extent initargs))
  (let ((class-name (class-name from)))
    (unless (eq from to)                  ; shouldn't be
      (remove-structure-defs class-name)
      (remhash class-name %defstructs%)))
  (%change-class from to initargs))

;;; if redefining a structure as another structure or redefining a
;;; structure as a class
(defun remove-structure-defs (class-name)
  (let ((sd (gethash class-name %defstructs%)))
    (when sd
      (dolist (refname (sd-refnames sd))
        (remhash refname %structure-refs%)
        (let ((def (assq refname *nx-globally-inline*)))
          (when def (set-function-info refname nil)))
        (when (symbolp refname)(fmakunbound refname)))
      (let ((print-fn (sd-print-function sd)))
        (when (symbolp print-fn) (fmakunbound print-fn)))
      (let ((constructor (sd-constructor sd)))
        (when (symbolp constructor) (fmakunbound constructor)))
      (let ((delete-match #'(lambda (pred struct-name)
                              (when (eq struct-name class-name)
                                (remhash pred %structure-refs%)
                                (fmakunbound pred)))))
        (declare (dynamic-extent delete-match))
        ; get rid of the predicate
        (maphash delete-match %structure-refs%)))))

(defun copy-structure (source)
  "Return a copy of STRUCTURE with the same (EQL) slot values."
  (copy-uvector (require-type source 'structure-object)))

(provide 'defstruct)

; End of defstruct.lisp
