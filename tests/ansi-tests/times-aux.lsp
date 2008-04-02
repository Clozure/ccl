;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Aug 28 11:23:40 2003
;;;; Contains: Auxiliary functions for testing the multiplication operator *

(in-package :cl-test)

(defun integer-times (x y)
  (assert (integerp x))
  (assert (integerp y))
  (let (neg)
    (when (< x 0)
      (setq neg t x (- x)))
    (let ((result (nat-times x y)))
      (if neg (- result) result))))

(defun nat-times (x y)
  ;; Assumes x >= 0
  (if (= x 0)
      0
    (let ((lo (if (oddp x) y 0))
	  (hi (nat-times (ash x -1) y)))
      (+ lo (+ hi hi)))))

(defun rat-times (x y)
  (/ (integer-times (numerator x) (numerator y))
     (integer-times (denominator x) (denominator y))))
