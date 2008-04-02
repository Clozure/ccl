;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 21:07:36 2003
;;;; Contains: Tests of CONJUGATE

(in-package :cl-test)

;;; Error tests

(deftest conjugate.error.1
  (signals-error (conjugate) program-error)
  t)

(deftest conjugate.error.2
  (signals-error (conjugate 0 0) program-error)
  t)

;;; Non-error tests

(deftest conjugate.1
  (loop for x in *reals*
	for vals = (multiple-value-list (conjugate x))
	for xc = (car vals)
	always (and (= (length vals) 1)
		    (eql x xc)))
  t)

(deftest conjugate.2
  (loop for x in *complexes*
	for vals = (multiple-value-list (conjugate x))
	for xc = (car vals)
	always (and (= (length vals) 1)
		    (eql (realpart x) (realpart xc))
		    (eql (- (imagpart x)) (imagpart xc))))
  t)

(deftest conjugate.3
  (eqlt (conjugate #c(0.0s0 0.0s0)) #c(0.0s0 -0.0s0))
  t)

(deftest conjugate.4
  (eqlt (conjugate #c(1.0s0 0.0s0)) #c(1.0s0 -0.0s0))
  t)

(deftest conjugate.5
  (eqlt (conjugate #c(0.0f0 0.0f0)) #c(0.0f0 -0.0f0))
  t)

(deftest conjugate.6
  (eqlt (conjugate #c(1.0f0 0.0f0)) #c(1.0f0 -0.0f0))
  t)

(deftest conjugate.7
  (eqlt (conjugate #c(0.0d0 0.0d0)) #c(0.0d0 -0.0d0))
  t)

(deftest conjugate.8
  (eqlt (conjugate #c(1.0d0 0.0d0)) #c(1.0d0 -0.0d0))
  t)

(deftest conjugate.9
  (eqlt (conjugate #c(0.0l0 0.0l0)) #c(0.0l0 -0.0l0))
  t)

(deftest conjugate.10
  (eqlt (conjugate #c(1.0l0 0.0l0)) #c(1.0l0 -0.0l0))
  t)
