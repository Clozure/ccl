;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 12:51:59 2005
;;;; Contains: Tests of the declaration SPECIAL

(in-package :cl-test)

;;; Many tests for this declaration are in the tests
;;; for specific binding forms.

(deftest special.1
  (let ((f 1))
    (declare (special f))
    (flet ((f () :good))
      (flet ((g () (f)))
	(flet ((f () :bad))
	  (g)))))
  :good)

(deftest special.2
  (let ((x 'a))
    (declare (special x))
    (let ((x 'b))
      (values x (locally (declare (special x)) x) x)))
  b a b)

(deftest special.3
  (flet ((%f () (declare (special x10)) x10))
    (let ((x10 'a))
      (declare (special x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12))
      (%f)))
  a)

