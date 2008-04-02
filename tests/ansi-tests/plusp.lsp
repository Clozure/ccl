;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug  4 21:42:14 2003
;;;; Contains: Tests for PLUSP

(in-package :cl-test)

;;; Error tests

(deftest plusp.error.1
  (signals-error (plusp) program-error)
  t)

(deftest plusp.error.2
  (signals-error (plusp 0 0) program-error)
  t)

(deftest plusp.error.3
  (signals-error (plusp 0 nil) program-error)
  t)

(deftest plusp.error.4
  (check-type-error #'plusp #'realp)
  nil)

;;; Non-error tests

(deftest plusp.1
  (plusp 0)
  nil)

(deftest plusp.2
  (plusp -1)
  nil)

(deftest plusp.3
  (notnot-mv (plusp 1))
  t)

(deftest plusp.4
  (loop for x in *reals*
	when (if (plusp x) (<= x 0) (> x 0))
	collect x)
  nil)

(deftest plusp.5
  (some #'plusp '(-0.0s0 -0.0f0 -0.0d0 -0.0l0))
  nil)

(deftest plusp.6
  (some #'plusp '(0.0s0 0.0f0 0.0d0 0.0l0))
  nil)

(deftest plusp.7
  (remove-if #'plusp
	     (list least-positive-short-float
		   least-positive-normalized-short-float
		   least-positive-single-float
		   least-positive-normalized-single-float
		   least-positive-double-float
		   least-positive-normalized-double-float
		   least-positive-long-float
		   least-positive-normalized-long-float
		   most-positive-short-float
		   most-positive-single-float
		   most-positive-double-float
		   most-positive-long-float))
  nil)

