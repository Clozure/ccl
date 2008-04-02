;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug  4 21:47:34 2003
;;;; Contains: Tests of ZEROP

(in-package :cl-test)

(deftest zerop.error.1
  (signals-error (zerop) program-error)
  t)

(deftest zerop.error.2
  (signals-error (zerop 0 1) program-error)
  t)

(deftest zerop.error.3
  (signals-error (zerop 1 0) program-error)
  t)

(deftest zerop.error.4
  (check-type-error #'zerop #'numberp)
  nil)

(deftest zerop.1
  (loop for x in *numbers*
	when (if (zerop x) (/= x 0) (= x 0))
	collect x)
  nil)

(deftest zerop.2
  (zerop 1)
  nil)

(deftest zerop.3
  (zerop -1)
  nil)

(deftest zerop.4
  (notnot-mv (zerop 0))
  t)

(deftest zerop.5
  (notnot-mv (zerop 0.0s0))
  t)

(deftest zerop.6
  (notnot-mv (zerop 0.0f0))
  t)

(deftest zerop.7
  (notnot-mv (zerop 0.0d0))
  t)

(deftest zerop.7a
  (notnot-mv (zerop 0.0l0))
  t)

(deftest zerop.8
  (remove-if-not #'zerop
		 (list least-negative-short-float
		       least-negative-normalized-short-float
		       least-negative-single-float
		       least-negative-normalized-single-float
		       least-negative-double-float
		       least-negative-normalized-double-float
		       least-negative-long-float
		       least-negative-normalized-long-float
		       most-negative-short-float
		       most-negative-single-float
		       most-negative-double-float
		       most-negative-long-float))
  nil)

(deftest zerop.9
  (remove-if-not #'zerop
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

(deftest zerop.10
  (notevery #'zerop (list -0.0s0 -0.0f0 -0.0d0 -0.0l0))
  nil)

