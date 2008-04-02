;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:33:02 2003
;;;; Contains: Tests of DEFINE-COMPILER-MACRO

(in-package :cl-test)

;;; Need to add non-error tests

(deftest define-compiler-macro.error.1
  (signals-error (funcall (macro-function 'define-compiler-macro))
		 program-error)
  t)

(deftest define-compiler-macro.error.2
  (signals-error (funcall (macro-function 'define-compiler-macro)
			   '(definee-compiler-macro nonexistent-function ()))
		 program-error)
  t)

(deftest define-compiler-macro.error.3
  (signals-error (funcall (macro-function 'define-compiler-macro)
			   '(definee-compiler-macro nonexistent-function ())
			   nil nil)
		 program-error)
  t)

;;; Non-error tests

(deftest define-compiler-macro.1
  (let* ((sym (gensym))
	 (macro-def-form
	  `(define-compiler-macro ,sym (x y)
	     (declare (special *x*))
	     (setf *x* t)
	     `(+ ,x ,y 1)))
	 (fun-def-form
	  `(defun ,sym (x y) (+ x y 1))))
    (values
     (equalt (list sym) (multiple-value-list (eval fun-def-form)))
     (equalt (list sym) (multiple-value-list (eval macro-def-form)))
     (notnot (typep (compiler-macro-function sym) 'function))
     (eval `(,sym 6 19))
     (let ((fn (compile nil `(lambda (a b) (,sym a b)))))
       (let ((*x* nil))
	 (declare (special *x*))
	 (list (funcall fn 12 123) *x*)))))
  t t t 26 (136 nil))

(deftest define-compiler-macro.2
  (let* ((sym (gensym))
	 (macro-def-form
	  `(define-compiler-macro ,sym (&whole form &rest args)
	     (declare (special *x*) (ignore args))
	     (setf *x* t)
	     (return-from ,sym form)))
	 (fun-def-form
	  `(defun ,sym (x) x)))
    (values
     (equalt (list sym) (multiple-value-list (eval fun-def-form)))
     (equalt (list sym) (multiple-value-list (eval macro-def-form)))
     (notnot (typep (compiler-macro-function sym) 'function))
     (eval `(,sym 'a))
     (let ((fn (compile nil `(lambda (a) (,sym a)))))
       (let ((*x* nil))
	 (declare (special *x*))
	 (list (funcall fn 'b) *x*)))))
  t t t a (b nil))

(deftest define-compiler-macro.3
  (let* ((sym (gensym))
	 (macro-def-form
	  `(define-compiler-macro ,sym (&whole form &rest args)
	     (declare (special *x*) (ignore args))
	     (setf *x* t)
	     (return-from ,sym form)))
	 (ordinary-macro-def-form
	  `(defmacro ,sym (x) x)))
    (values
     (equalt (list sym) (multiple-value-list (eval ordinary-macro-def-form)))
     (equalt (list sym) (multiple-value-list (eval macro-def-form)))
     (notnot (typep (compiler-macro-function sym) 'function))
     (eval `(,sym 'a))
     (let ((fn (compile nil `(lambda (a) (,sym a)))))
       (let ((*x* nil))
	 (declare (special *x*))
	 (list (funcall fn 'b) *x*)))))
  t t t a (b nil))

;;; Compiler macros on setf functions

(deftest define-compiler-macro.4
  (let* ((sym (gensym))
	 (fun-def-form `(defun ,sym (x) (car x)))
	 (setf-fun-def-form `(defun (setf ,sym) (newval x) (setf (car x) newval)))
	 (setf-compiler-macro-def-form
	  `(define-compiler-macro (setf ,sym) (newval x)
	     (declare (special *x*))
	     (setf *x* t)
	     (return-from ,sym `(setf (car ,x) ,newval)))))
    (values
     (equalt (list sym) (multiple-value-list (eval fun-def-form)))
     (equalt `((setf ,sym)) (multiple-value-list (eval setf-fun-def-form)))
     (equalt `((setf ,sym)) (multiple-value-list (eval setf-compiler-macro-def-form)))
     (notnot (typep (compiler-macro-function `(setf ,sym)) 'function))
     (eval `(,sym (list 'a 'b)))
     (eval `(let ((arg (list 1 2)))
	      (list (setf (,sym arg) 'z) arg)))
     (let ((fn (compile nil `(lambda (u v) (setf (,sym u) v)))))
       (let ((*x* nil)
	     (arg (list 1 2)))
	 (declare (special *x*))
	 (list (funcall fn arg 'y) arg)))))
  t t t t a (z (z 2)) (y (y 2)))

;;; Test of documentation

(deftest define-compiler-macro.5
  (let* ((sym (gensym))
	 (form `(define-compiler-macro ,sym (x) "DCM.5" x))
	 (form2 `(defun ,sym (x) "DCM.5-WRONG" x)))
    (eval form)
    (eval form2)
    (or (documentation sym 'compiler-macro) "DCM.5"))
  "DCM.5")

(deftest define-compiler-macro.6
  (let* ((sym (gensym))
	 (form `(define-compiler-macro ,sym (x) "DCM.6" x))
	 (form2 `(defun ,sym (x) "DCM.6-WRONG" x)))
    (eval form2)
    (eval form)
    (or (documentation sym 'compiler-macro) "DCM.6"))
  "DCM.6")

;;; NOTINLINE turns off a compiler macro

(deftest define-compiler-macro.7
  (let* ((sym (gensym))
	 (form `(define-compiler-macro ,sym (x y)
		  (declare (special *x*))
		  (setf *x* :bad)
		  `(list ,x ,y)))
	 (form2 `(defun ,sym (x y) (list x y))))
    (eval form)
    (eval form2)
    (compile sym)
    (let ((*x* :good))
      (declare (special *x*))
      (values
       (funcall (compile nil `(lambda (a b)
				(declare (notinline ,sym))
				(,sym a b)))
		5 11)
       *x*)))
  (5 11) :good)

(deftest define-compiler-macro.8
  (let* ((sym (gensym))
	 (form `(define-compiler-macro ,sym (x y)
		  (declare (special *x*))
		  (setf *x* :bad)
		  `(list ,x ,y)))
	 (form2 `(defmacro ,sym (x y) `(list ,x ,y))))
    (eval form)
    (eval form2)
    (let ((*x* :good))
      (declare (special *x*))
      (values
       (funcall (compile nil `(lambda (a b)
				(declare (notinline ,sym))
				(,sym a b)))
		7 23)
       *x*)))
  (7 23) :good)
