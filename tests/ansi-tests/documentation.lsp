;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 14 07:30:01 2004
;;;; Contains: Tests of DOCUMENTATION

(in-package :cl-test)

;;; documentation (x function) (doc-type (eql 't))

(deftest documentation.function.t.1
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (documentation (symbol-function sym) t))
  nil)

(deftest documentation.function.t.2
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (let ((fn (symbol-function sym))
	  (doc "FOO1"))
      (multiple-value-prog1
       (setf (documentation fn t) (copy-seq doc))
       (assert (or (null (documentation fn t))
		   (equal doc (documentation fn t)))))))
  "FOO1")

(deftest documentation.function.t.3
  (let* ((sym (gensym)))
    (eval `(defmacro ,sym () nil))
    (documentation (macro-function sym) t))
  nil)

(deftest documentation.function.t.4
  (let* ((sym (gensym)))
    (eval `(defmacro ,sym () nil))
    (let ((fn (macro-function sym))
	  (doc "FOO2"))
      (multiple-value-prog1
       (setf (documentation fn t) (copy-seq doc))
       (assert (or (null (documentation fn t))
		   (equal doc (documentation fn t)))))))
  "FOO2")

(deftest documentation.function.t.6
  (let* ((sym (gensym))
	 (fn (eval `#'(lambda () ',sym)))
	 (doc "FOO3"))
    (multiple-value-prog1
       (setf (documentation fn t) (copy-seq doc))
       (assert (or (null (documentation fn t))
		   (equal doc (documentation fn t))))))
  "FOO3")

(deftest documentation.function.t.6a
  (let* ((sym (gensym))
	 (fn (compile nil `(lambda () ',sym)))
	 (doc "FOO3A"))
    (multiple-value-prog1
       (setf (documentation fn t) (copy-seq doc))
       (assert (or (null (documentation fn t))
		   (equal doc (documentation fn t))))))
  "FOO3A")

;; Reorder 5, 5a and 6, 6a to expose possible interaction bug

(deftest documentation.function.t.5
  (let* ((sym (gensym))
	 (fn (eval `#'(lambda () ',sym))))
    (documentation fn t))
  nil)

(deftest documentation.function.t.5a
  (let* ((sym (gensym))
	 (fn (compile nil `(lambda () ',sym))))
    (documentation fn t))
  nil)

(deftest documentation.function.t.7
  (let* ((sym (gensym))
	 (fn (eval `(defgeneric ,sym (x)))))
    (documentation fn t))
  nil)
    
(deftest documentation.function.t.8
  (let* ((sym (gensym))
	 (fn (eval `(defgeneric ,sym (x))))
	 (doc "FOO4"))
    (multiple-value-prog1
       (setf (documentation fn t) (copy-seq doc))
       (assert (or (null (documentation fn t))
		   (equal doc (documentation fn t))))))
  "FOO4")

(deftest documentation.function.t.9
  (loop for s in *cl-function-symbols*
	for fn = (symbol-function s)
	for doc = (documentation fn t)
	unless (or (null doc) (string doc))
	collect (list s doc))
  nil)

(deftest documentation.function.t.10
  (loop for s in *cl-accessor-symbols*
	for fn = (symbol-function s)
	for doc = (documentation fn t)
	unless (or (null doc) (string doc))
	collect (list s doc))
  nil)

(deftest documentation.function.t.11
  (loop for s in *cl-macro-symbols*
	for fn = (macro-function s)
	for doc = (documentation fn t)
	unless (or (null doc) (string doc))
	collect (list s doc))
  nil)

(deftest documentation.function.t.12
  (loop for s in *cl-standard-generic-function-symbols*
	for fn = (symbol-function s)
	for doc = (documentation fn t)
	unless (or (null doc) (string doc))
	collect (list s doc))
  nil)

;;; documentation (x function) (doc-type (eql 'function))

(deftest documentation.function.function.1
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (documentation (symbol-function sym) 'function))
  nil)

(deftest documentation.function.function.2
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (let ((fn (symbol-function sym))
	  (doc "FOO5"))
      (multiple-value-prog1
       (setf (documentation fn 'function) (copy-seq doc))
       (assert (or (null (documentation fn 'function))
		   (equal doc (documentation fn 'function)))))))
  "FOO5")

(deftest documentation.function.function.3
  (let* ((sym (gensym)))
    (eval `(defmacro ,sym () nil))
    (documentation (macro-function sym) 'function))
  nil)

(deftest documentation.function.function.4
  (let* ((sym (gensym)))
    (eval `(defmacro ,sym () nil))
    (let ((fn (macro-function sym))
	  (doc "FOO6"))
      (multiple-value-prog1
       (setf (documentation fn t) (copy-seq doc))
       (assert (or (null (documentation fn 'function))
		   (equal doc (documentation fn 'function)))))))
  "FOO6")

(deftest documentation.function.function.5
  (let* ((sym (gensym))
	 (fn (eval `(defgeneric ,sym (x)))))
    (documentation fn 'function))
  nil)
    
(deftest documentation.function.function.8
  (let* ((sym (gensym))
	 (fn (eval `(defgeneric ,sym (x))))
	 (doc "FOO4A"))
    (multiple-value-prog1
       (setf (documentation fn t) (copy-seq doc))
       (assert (or (null (documentation fn 'function))
		   (equal doc (documentation fn 'function))))))
  "FOO4A")

;;; documentation (x list) (doc-type (eql 'function))

(deftest documentation.list.function.1
  (let* ((sym (gensym)))
    (eval `(defun (setf ,sym) (&rest args) (declare (ignore args)) nil))
    (documentation `(setf ,sym) 'function))
  nil)

(deftest documentation.list.function.2
  (let* ((sym (gensym)))
    (eval `(defun (setf ,sym) (&rest args) (declare (ignore args)) nil))
    (let ((fn `(setf ,sym))
	  (doc "FOO7"))
      (multiple-value-prog1
       (setf (documentation fn 'function) (copy-seq doc))
       (assert (or (null (documentation fn 'function))
		   (equal doc (documentation fn 'function)))))))
  "FOO7")

;;; documentation (x list) (doc-type (eql 'compiler-macro))

(deftest documentation.list.compiler-macro.1
  (let* ((sym (gensym)))
    (eval `(define-compiler-macro (setf ,sym) (&rest args) (declare (ignore args)) nil))
    (documentation `(setf ,sym) 'compiler-macro))
  nil)

(deftest documentation.list.compiler-macro.2
  (let* ((sym (gensym)))
    (eval `(define-compiler-macro (setf ,sym) (&rest args) (declare (ignore args)) nil))
    (let ((fn `(setf ,sym))
	  (doc "FOO8"))
      (multiple-value-prog1
       (setf (documentation fn 'compiler-macro) (copy-seq doc))
       (assert (or (null (documentation fn 'function))
		   (equal doc (documentation fn 'compiler-macro)))))))
  "FOO8")

;;; documentation (x symbol) (doc-type (eql 'function))

(deftest documentation.symbol.function.1
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (documentation sym 'function))
  nil)

(deftest documentation.symbol.function.2
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (let ((doc "FOO9"))
      (multiple-value-prog1
       (setf (documentation sym 'function) (copy-seq doc))
       (assert (or (null (documentation sym 'function))
		   (equal doc (documentation sym 'function)))))))
  "FOO9")

(deftest documentation.symbol.function.3
  (let* ((sym (gensym)))
    (eval `(defmacro ,sym () nil))
    (documentation sym 'function))
  nil)

(deftest documentation.symbol.function.4
  (let* ((sym (gensym)))
    (eval `(defmacro ,sym () nil))
    (let ((doc "FOO9A"))
      (multiple-value-prog1
       (setf (documentation sym 'function) (copy-seq doc))
       (assert (or (null (documentation sym 'function))
		   (equal doc (documentation sym 'function)))))))
  "FOO9A")

(deftest documentation.symbol.function.5
  (let* ((sym (gensym)))
    (eval `(defgeneric ,sym (x)))
    (documentation sym 'function))
  nil)

(deftest documentation.symbol.function.6
  (let* ((sym (gensym)))
    (eval `(defgeneric ,sym (x)))
    (let ((doc "FOO9B"))
      (multiple-value-prog1
       (setf (documentation sym 'function) (copy-seq doc))
       (assert (or (null (documentation sym 'function))
		   (equal doc (documentation sym 'function)))))))
  "FOO9B")

(deftest documentation.symbol.function.7
  (loop for s in *cl-special-operator-symbols*
	for doc = (documentation s 'function)
	unless (or (null doc) (stringp doc))
	collect (list s doc))
  nil)

(deftest documentation.symbol.function.8
  (loop for s in *cl-function-or-accessor-symbols*
	for doc = (documentation s 'function)
	unless (or (null doc) (stringp doc))
	collect (list s doc))
  nil)

(deftest documentation.symbol.function.9
  (loop for s in *cl-macro-symbols*
	for doc = (documentation s 'function)
	unless (or (null doc) (stringp doc))
	collect (list s doc))
  nil)

;;; documentation (x symbol) (doc-type (eql 'compiler-macro))

(deftest documentation.symbol.compiler-macro.1
  (let* ((sym (gensym)))
    (eval `(define-compiler-macro ,sym (&rest args) (declare (ignore args)) nil))
    (documentation sym 'compiler-macro))
  nil)

(deftest documentation.symbol.compiler-macro.2
  (let* ((sym (gensym)))
    (eval `(define-compiler-macro ,sym (&rest args) (declare (ignore args)) nil))
    (let ((doc "FOO10"))
      (multiple-value-prog1
       (setf (documentation sym 'compiler-macro) (copy-seq doc))
       (assert (or (null (documentation sym 'compiler-macro))
		   (equal doc (documentation sym 'compiler-macro)))))))
  "FOO10")
	
;;; documentation (x symbol) (doc-type (eql 'setf))

(deftest documentation.symbol.setf.1
  (let* ((sym (gensym))
	 (doc "FOO11"))
    (eval `(defun ,sym () (declare (special *x*)) *x*))
    (eval `(define-setf-expander ,sym ()
	     (let ((g (gemsym)))
	       (values nil nil (list g) `(locally (declare (special *x*)) (setf *x* ,g))
		       '(locally (declare (special *x*)) *x*)))))
    (multiple-value-prog1
     (values
      (documentation sym 'setf)
      (setf (documentation sym 'setf) (copy-seq doc)))
     (assert (or (null (documentation sym 'setf))
		 (equal doc (documentation sym 'setf))))))
  nil "FOO11")

(deftest documentation.symbol.setf.2
  (let* ((sym (gensym))
	 (doc "FOO12"))
    (eval `(defmacro ,sym () `(locally (declare (special *x*)) *x*)))
    (eval `(define-setf-expander ,sym ()
	     (let ((g (gemsym)))
	       (values nil nil (list g) `(locally (declare (special *x*)) (setf *x* ,g))
		       '(locally (declare (special *x*)) *x*)))))
    (multiple-value-prog1
     (values
      (documentation sym 'setf)
      (setf (documentation sym 'setf) (copy-seq doc)))
     (assert (or (null (documentation sym 'setf))
		 (equal doc (documentation sym 'setf))))))
  nil "FOO12")

;;; documentation (x method-combination) (doc-type (eql 't))
;;; documentation (x method-combination) (doc-type (eql 'method-combination))
;;; There's no portable way to test those, since there's no portable way to
;;; get a method combination object

;;; documentation (x symbol) (doc-type (eql 'method-combination))

(deftest documentation.symbol.method-combination.1
  (let* ((sym (gensym))
	 (doc "FOO13"))
    (eval `(define-method-combination ,sym :identity-with-one-argument t))
    (multiple-value-prog1
     (values
      (documentation sym 'method-combination)
      (setf (documentation sym 'method-combination) (copy-seq doc)))
     (assert (or (null (documentation sym 'method-combination))
		 (equal doc (documentation sym 'method-combination))))))
  nil "FOO13")

;;; documentation (x standard-method) (doc-type (eql 't))

(deftest documentation.standard-method.t.1
  (let* ((sym (gensym))
	 (doc "FOO14"))
    (eval `(defgeneric ,sym (x)))
    (let ((method (eval `(defmethod ,sym ((x t)) nil))))
      (multiple-value-prog1
       (values
	(documentation method t)
	(setf (documentation method t) (copy-seq doc)))
       (assert (or (null (documentation method 't))
		   (equal doc (documentation method 't)))))))
  nil "FOO14")

;;; documentation (x package) (doc-type (eql 't))

(deftest documentation.package.t.1
  (let ((package-name "PACKAGE-NAME-FOR-DOCUMENATION-TESTS-1"))
    (unwind-protect
	(progn
	  (eval `(defpackage ,package-name (:use)))
	  (let ((pkg (find-package package-name))
		(doc "FOO15"))
	    (assert pkg)
	    (multiple-value-prog1
	     (values
	      (documentation pkg t)
	      (setf (documentation pkg t) (copy-seq doc)))
	     (assert (or (null (documentation pkg t))
			 (equal doc (documentation pkg t)))))))
      (delete-package package-name)))
  nil "FOO15")

;;; documentation (x standard-class) (doc-type (eql 't))

(deftest documentation.standard-class.t.1
  (let* ((sym (gensym))
	 (class-form `(defclass ,sym () ())))
    (eval class-form)
    (let ((class (find-class sym))
	  (doc "FOO16"))
      (multiple-value-prog1
       (values
	(documentation class t)
	(setf (documentation class t) (copy-seq doc)))
       (assert (or (null (documentation class t))
		   (equal doc (documentation class t)))))))
  nil "FOO16")

;;; documentation (x standard-class) (doc-type (eql 'type))

(deftest documentation.standard-class.type.1
  (let* ((sym (gensym))
	 (class-form `(defclass ,sym () ())))
    (eval class-form)
    (let ((class (find-class sym))
	  (doc "FOO17"))
      (multiple-value-prog1
       (values
	(documentation class 'type)
	(setf (documentation class 'type) (copy-seq doc)))
       (assert (or (null (documentation class 'type))
		   (equal doc (documentation class 'type)))))))
  nil "FOO17")


;;; documentation (x structure-class) (doc-type (eql 't))
	
(deftest documentation.struct-class.t.1
  (let* ((sym (gensym))
	 (class-form `(defstruct ,sym a b c)))
    (eval class-form)
    (let ((class (find-class sym))
	  (doc "FOO18"))
      (multiple-value-prog1
       (values
	(documentation class t)
	(setf (documentation class t) (copy-seq doc)))
       (assert (or (null (documentation class t))
		   (equal doc (documentation class t)))))))
  nil "FOO18")

;;; documentation (x structure-class) (doc-type (eql 'type))

(deftest documentation.struct-class.type.1
  (let* ((sym (gensym))
	 (class-form `(defstruct ,sym a b c)))
    (eval class-form)
    (let ((class (find-class sym))
	  (doc "FOO19"))
      (multiple-value-prog1
       (values
	(documentation class 'type)
	(setf (documentation class 'type) (copy-seq doc)))
       (assert (or (null (documentation class 'type))
		   (equal doc (documentation class 'type)))))))
  nil "FOO19")

;;; documentation (x symbol) (doc-type (eql 'type))

(deftest documentation.symbol.type.1
  (let* ((sym (gensym))
	 (class-form `(defclass ,sym () ()))
	 (doc "FOO20"))
    (eval class-form)
    (multiple-value-prog1
     (values
      (documentation sym 'type)
      (setf (documentation sym 'type) (copy-seq doc)))
     (assert (or (null (documentation sym 'type))
		 (equal doc (documentation sym 'type))))))
  nil "FOO20")

(deftest documentation.symbol.type.2
  (let* ((sym (gensym))
	 (class-form `(defstruct ,sym a b c))
	 (doc "FOO21"))
    (eval class-form)
    (multiple-value-prog1
     (values
      (documentation sym 'type)
      (setf (documentation sym 'type) (copy-seq doc)))
     (assert (or (null (documentation sym 'type))
		 (equal doc (documentation sym 'type))))))
  nil "FOO21")

(deftest documentation.symbol.type.3
  (let* ((sym (gensym))
	 (type-form `(deftype ,sym () t))
	 (doc "FOO21A"))
    (eval type-form)
    (multiple-value-prog1
     (values
      (documentation sym 'type)
      (setf (documentation sym 'type) (copy-seq doc)))
     (assert (or (null (documentation sym 'type))
		 (equal doc (documentation sym 'type))))))
  nil "FOO21A")

(deftest documentation.symbol.type.4
  (loop for s in *cl-all-type-symbols*
	for doc = (documentation s 'type)
	unless (or (null doc) (stringp doc))
	collect (list doc))
  nil)


;;; documentation (x symbol) (doc-type (eql 'structure))

(deftest documentation.symbol.structure.1
  (let* ((sym (gensym))
	 (class-form `(defstruct ,sym a b c))
	 (doc "FOO22"))
    (eval class-form)
    (multiple-value-prog1
     (values
      (documentation sym 'structure)
      (setf (documentation sym 'structure) (copy-seq doc)))
     (assert (or (null (documentation sym 'structure))
		 (equal doc (documentation sym 'structure))))))
  nil "FOO22")

(deftest documentation.symbol.structure.2
  (let* ((sym (gensym))
	 (class-form `(defstruct (,sym (:type list)) a b c))
	 (doc "FOO23"))
    (eval class-form)
    (multiple-value-prog1
     (values
      (documentation sym 'structure)
      (setf (documentation sym 'structure) (copy-seq doc)))
     (assert (or (null (documentation sym 'structure))
		 (equal doc (documentation sym 'structure))))))
  nil "FOO23")

(deftest documentation.symbol.structure.3
  (let* ((sym (gensym))
	 (class-form `(defstruct (,sym (:type vector)) a b c))
	 (doc "FOO24"))
    (eval class-form)
    (multiple-value-prog1
     (values
      (documentation sym 'structure)
      (setf (documentation sym 'structure) (copy-seq doc)))
     (assert (or (null (documentation sym 'structure))
		 (equal doc (documentation sym 'structure))))))
  nil "FOO24")

;;; documentation (x symbol) (doc-type (eql 'variable))

(deftest documentation.symbol.variable.1
  (let* ((sym (gensym))
	 (form `(defvar ,sym))
	 (doc "FOO25"))
    (eval form)
    (multiple-value-prog1
     (values
      (documentation sym 'variable)
      (setf (documentation sym 'variable) (copy-seq doc)))
     (assert (or (null (documentation sym 'variable))
		 (equal doc (documentation sym 'variable))))))
  nil "FOO25")

(deftest documentation.symbol.variable.2
  (let* ((sym (gensym))
	 (form `(defvar ,sym t))
	 (doc "FOO26"))
    (eval form)
    (multiple-value-prog1
     (values
      (documentation sym 'variable)
      (setf (documentation sym 'variable) (copy-seq doc)))
     (assert (or (null (documentation sym 'variable))
		 (equal doc (documentation sym 'variable))))))
  nil "FOO26")

(deftest documentation.symbol.variable.3
  (let* ((sym (gensym))
	 (form `(defparameter ,sym t))
	 (doc "FOO27"))
    (eval form)
    (multiple-value-prog1
     (values
      (documentation sym 'variable)
      (setf (documentation sym 'variable) (copy-seq doc)))
     (assert (or (null (documentation sym 'variable))
		 (equal doc (documentation sym 'variable))))))
  nil "FOO27")

(deftest documentation.symbol.variable.4
  (let* ((sym (gensym))
	 (form `(defconstant ,sym t))
	 (doc "FOO27"))
    (eval form)
    (multiple-value-prog1
     (values
      (documentation sym 'variable)
      (setf (documentation sym 'variable) (copy-seq doc)))
     (assert (or (null (documentation sym 'variable))
		 (equal doc (documentation sym 'variable))))))
  nil "FOO27")

(deftest documentation.symbol.variable.5
  (loop for s in *cl-variable-symbols*
	for doc = (documentation s 'variable)
	unless (or (null doc) (stringp doc))
	collect (list s doc))
  nil)

(deftest documentation.symbol.variable.6
  (loop for s in *cl-constant-symbols*
	for doc = (documentation s 'variable)
	unless (or (null doc) (stringp doc))
	collect (list s doc))
  nil)

;;; Defining new methods for DOCUMENTATION

(ignore-errors
  (defgeneric documentation-test-class-1-doc-accessor (obj))
  (defgeneric (setf documentation-test-class-1-doc-accessor) (newdoc obj))

  (defclass documentation-test-class-1 () ((my-doc :accessor documentation-test-class-1-doc-accessor
						 :type (or null string)
						 :initform nil)))
  
  (defmethod documentation-test-class-1-doc-accessor ((obj documentation-test-class-1) )
    (slot-value obj 'my-doc))
  (defmethod (setf documentation-test-class-1-doc-accessor) ((newdoc string) (obj documentation-test-class-1))
    (setf (slot-value obj 'my-doc) newdoc))
  
  (defmethod documentation ((obj documentation-test-class-1) (doctype (eql t)))
    (documentation-test-class-1-doc-accessor obj))

  (defmethod (setf documentation) ((newdoc string) (obj documentation-test-class-1) (doctype (eql t)))
    (setf (documentation-test-class-1-doc-accessor obj) newdoc)))

(deftest documentation.new-method.1
  (let ((obj (make-instance 'documentation-test-class-1)))
    (values
     (documentation obj t)
     (setf (documentation obj t) "FOO28")
     (documentation obj t)))
  nil "FOO28" "FOO28")

