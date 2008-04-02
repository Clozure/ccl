;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 22:14:23 2003
;;;; Contains: Tests of FILL-POINTER

(in-package :cl-test)

;;; More tests are in make-array.lsp

(deftest fill-pointer.1
  (fill-pointer (make-array '(10) :fill-pointer 5))
  5)

(deftest fill-pointer.2
  (fill-pointer (make-array '(10) :fill-pointer t))
  10)

(deftest fill-pointer.3
  (let ((a (make-array '(10) :fill-pointer 5
		       :initial-contents '(1 2 3 4 5 6 7 8 9 10))))
    (values
     (fill-pointer a)
     (setf (fill-pointer a) 6)
     a))
  5 6 #(1 2 3 4 5 6))

(deftest fill-pointer.order.1
  (let ((i 0)
	(a (make-array '(10) :fill-pointer 5)))
    (values
     (fill-pointer (progn (incf i) a))
     i))
  5 1)

(deftest fill-pointer.order.2
  (let ((i 0) x y
	(a (make-array '(10) :fill-pointer 5
		       :initial-contents '(1 2 3 4 5 6 7 8 9 10))))
    (values
     i
     (setf (fill-pointer (progn (setf x (incf i)) a))
	   (progn (setf y (incf i)) 6))
     a
     i x y))
  0 6 #(1 2 3 4 5 6) 2 1 2)

;;; Error tests

(deftest fill-pointer.error.1
  (signals-error (fill-pointer) program-error)
  t)

(deftest fill-pointer.error.2
  (signals-error (fill-pointer (make-array '(10) :fill-pointer 4) nil)
		 program-error)
  t)

(deftest fill-pointer.error.3
  (let ((a (make-array '(10) :fill-pointer nil)))
    (if (array-has-fill-pointer-p a)
	t
      (eval `(signals-error (fill-pointer ',a) type-error))))
  t)

(deftest fill-pointer.error.4
  (signals-error (fill-pointer #0aNIL) type-error)
  t)

(deftest fill-pointer.error.5
  (signals-error (fill-pointer #2a((a b c)(d e f))) type-error)
  t)

(deftest fill-pointer.error.6
  (check-type-error #'fill-pointer #'(lambda (x) (and (vectorp x)
						      (array-has-fill-pointer-p x))))
  nil)

(deftest fill-pointer.error.7
  (signals-error (locally (fill-pointer #2a((a b c)(d e f))) t)
		 type-error)
  t)
