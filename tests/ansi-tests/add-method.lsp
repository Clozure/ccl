;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jun  4 19:12:25 2003
;;;; Contains: Tests for ADD-METHOD

(in-package :cl-test)

(defgeneric add-method-gf-01 (x)
  (:method ((x t)) 'a))

(defgeneric add-method-gf-02 (x))

;;; Cannot add a method that's already in another method

(deftest add-method.error.1
  (let ((method (find-method #'add-method-gf-01 nil (list (find-class t)))))
    (handler-case
     (add-method #'add-method-gf-02 method)
     (error () :error)))
  :error)

;;; The lambda lists must be congruent

(deftest add-method.error.2
  (let* ((gf (eval '(defgeneric add-method-gf-03 (x)
		      (:method ((x t)) 'a))))
	 (method (find-method #'add-method-gf-03 nil (list (find-class t))))
	 (gf2 (eval '(defgeneric add-method-gf-04 (x y)))))
    (handler-case
     (add-method gf2 method)
     (error () :error)))
  :error)

(deftest add-method.error.3
  (let* ((gf (eval '(defgeneric add-method-gf-05 (x &optional y)
		      (:method ((x t) &optional y) 'a))))
	 (method (find-method #'add-method-gf-05 nil (list (find-class t))))
	 (gf2 (eval '(defgeneric add-method-gf-06 (x y)))))
    (handler-case
     (add-method gf2 method)
     (error () :error)))
  :error)

(deftest add-method.error.4
  (signals-error (add-method) program-error)
  t)

(deftest add-method.error.5
  (signals-error (add-method #'add-method-gf-01) program-error)
  t)

(deftest add-method.error.6
  (signals-error
   (let* ((gf (eval '(defgeneric add-method-gf-07 (x)
		       (:method ((x t)) 'a))))
	  (method (find-method #'add-method-gf-07 nil (list (find-class t))))
	  (gf2 (eval '(defgeneric add-method-gf-08 (x)))))
     (remove-method gf method)
     (add-method gf2 method nil))
   program-error)
  t)

(deftest add-method.error.7
  (let* ((gf (eval '(defgeneric add-method-gf-09 (x y)
		      (:method ((x t) (y t)) 'a))))
	 (method (find-method #'add-method-gf-09 nil (list (find-class t)
							   (find-class t))))
	 (gf2 (eval '(defgeneric add-method-gf-10 (x &optional y)))))
     (remove-method gf method)
     (handler-case
      (add-method gf2 method)
      (error () :error)))
  :error)

(deftest add-method.error.8
  (let* ((gf (eval '(defgeneric add-method-gf-11 (x &key y)
		      (:method ((x t) &key y) 'a))))
	 (method (find-method #'add-method-gf-11 nil (list (find-class t))))
	 (gf2 (eval '(defgeneric add-method-gf-12 (x)))))
    (remove-method gf method)
    (handler-case
     (add-method gf2 method)
     (error () :error)))
  :error)


;;; Non-error tests

(deftest add-method.1
  (let* ((gf (eval '(defgeneric add-method-gf-13 (x)
		      (:method ((x integer)) 'a)
		      (:method ((x t)) 'b))))
	 (method (find-method #'add-method-gf-13
			      nil (list (find-class 'integer))))
	 (gf2 (eval '(defgeneric add-method-gf-14 (x)))))
    (declare (type generic-function gf gf2))
    (values
     (funcall gf 0)
     (funcall gf 'x)
     (eqt gf (remove-method gf method))
     (eqt gf2 (add-method gf2 method))
     (funcall gf 0)
     (funcall gf 'x)
     (funcall gf2 0)))
  a b t t b b a)

;;; An existing method is replaced.

(deftest add-method.2
  (let* ((specializers (list (find-class 'integer)))
	 (gf (eval '(defgeneric add-method-gf-15 (x)
		      (:method ((x integer)) 'a)
		      (:method ((x t)) 'b))))
	 (method (find-method gf nil specializers))
	 (gf2 (eval '(defgeneric add-method-gf-16 (x)
		       (:method ((x integer)) 'c)
		       (:method ((x t)) 'd))))
	 (method2 (find-method gf2 nil specializers)))
    (declare (type generic-function gf gf2))
    (values
     (funcall gf 0)
     (funcall gf 'x)
     (funcall gf2 0)
     (funcall gf2 'x)
     (eqt gf (remove-method gf method))
     (eqt gf2 (add-method gf2 method))
     (eqt method (find-method gf2 nil specializers))
     (eqt method2 (find-method gf2 nil specializers))
     (funcall gf 0)
     (funcall gf 'x)
     (funcall gf2 0)
     (funcall gf2 'x)))
  a b c d t t t nil b b a d)

;;; Must add tests for: :around methods, :before methods, :after methods,
;;; nonstandard method combinations
