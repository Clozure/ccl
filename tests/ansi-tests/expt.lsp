;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  2 19:36:22 2003
;;;; Contains: Tests of EXPT

(in-package :cl-test)

;;; Error tests

(deftest expt.error.1
  (signals-error (expt) program-error)
  t)

(deftest expt.error.2
  (signals-error (expt 1 1 1) program-error)
  t)

(deftest expt.error.3
  (signals-error (expt 1 1 nil nil) program-error)
  t)

(deftest expt.error.4
  (signals-error (expt most-positive-short-float 2) floating-point-overflow)
  t)

(deftest expt.error.5
  (signals-error (expt most-positive-single-float 2) floating-point-overflow)
  t)

(deftest expt.error.6
  (signals-error (expt most-positive-double-float 2) floating-point-overflow)
  t)

(deftest expt.error.7
  (signals-error (expt most-positive-long-float 2) floating-point-overflow)
  t)

(deftest expt.error.8
  (signals-error (expt least-positive-short-float 2) floating-point-underflow)
  t)

(deftest expt.error.9
  (signals-error (expt least-positive-single-float 2) floating-point-underflow)
  t)

(deftest expt.error.10
  (signals-error (expt least-positive-double-float 2) floating-point-underflow)
  t)

(deftest expt.error.11
  (signals-error (expt least-positive-long-float 2) floating-point-underflow)
  t)




;;; Non-error tests

(deftest expt.1
  (expt 0 0)
  1)

(deftest expt.2
  (loop for i from -1000 to 1000
	always (eql (expt i 0) 1))
  t)

(deftest expt.3
  (loop for i = (random 1.0s3)
	repeat 1000
	always (eql (expt i 0) 1.0s0))
  t)

(deftest expt.4
  (loop for i = (random 1.0f6)
	repeat 1000
	always (eql (expt i 0) 1.0f0))
  t)

(deftest expt.5
  (loop for i = (random 1.0d10)
	repeat 1000
	always (eql (expt i 0) 1.0d0))
  t)

(deftest expt.6
  (loop for i = (random 1.0l10)
	repeat 1000
	always (eql (expt i 0) 1.0l0))
  t)

(deftest expt.7
  (loop for i from -1000 to 1000
	for c = (complex i i)
	always (eql (expt c 0) 1))
  t)

(deftest expt.8
  (loop for i = (random 1.0s3)
	for c = (complex i i)
	repeat 1000
	always (eql (expt c 0) #c(1.0s0 0.0s0)))
  t)

(deftest expt.9
  (loop for i = (random 1.0f6)
	for c = (complex i i)
	repeat 1000
	always (eql (expt c 0) #c(1.0f0 0.0f0)))
  t)

(deftest expt.10
  (loop for i = (random 1.0d10)
	for c = (complex i i)
	repeat 1000
	always (eql (expt c 0) #c(1.0d0 0.0d0)))
  t)

(deftest expt.11
  (loop for i = (random 1.0l10)
	for c = (complex i i)
	repeat 1000
	always (eql (expt c 0) #c(1.0l0 0.0l0)))
  t)

(deftest expt.12
  (loop for x in *numbers*
	unless (or (floatp (realpart x))
		   (eql (expt x 1) x))
	collect x)
  nil)

(deftest expt.13
  (loop for x in *rationals*
	unless (and (eql (expt x 2) (* x x))
		    (or (zerop x)
			(eql (expt x -1) (/ x))))
	collect x)
  nil)

(deftest expt.14
  (expt #c(0 2) 2)
  -4)

(deftest expt.15
  (expt #c(1 1) 2)
  #c(0 2))

(deftest expt.16
  (expt #c(1/2 1/3) 3)
  #c(-1/24 23/108))

(deftest expt.17
  (expt #c(1 1) -2)
  #c(0 -1/2))

(deftest expt.18
  (loop
   for zero in '(0.0s0 0.0f0 0.0d0 0.0l0)
   always
   (loop for i from -1000 to 1000
	 always (or (zerop i)
		    (eql (expt i zero) (float 1 zero)))))
  t)

(deftest expt.19
  (loop
   for zero in '(0.0s0 0.0f0 0.0d0 0.0l0)
   always
   (loop for i from -1000 to 1000
	 always (or (zerop i)
		    (eql (expt (float i 0.0s0) zero) (float 1 zero)))))
  t)

(deftest expt.20
  (loop
   for zero in '(0.0f0 0.0d0 0.0l0)
   always
   (loop for i from -1000 to 1000
	 always (or (zerop i)
		    (eql (expt (float i 0.0f0) zero) (float 1 zero)))))
  t)

(deftest expt.21
  (loop
   for zero in '(0.0d0 0.0l0)
   always
   (loop for i from -1000 to 1000
	 always (or (zerop i)
		    (eql (expt (float i 0.0d0) zero) (float 1 zero)))))
  t)

(deftest expt.22
  (expt 2.0f0 0.0s0)
  1.0f0)

(deftest expt.23
  (expt 2.0d0 0.0s0)
  1.0d0)

(deftest expt.24
  (expt 2.0l0 0.0s0)
  1.0l0)

(deftest expt.25
  (expt 2.0d0 0.0f0)
  1.0d0)

(deftest expt.26
  (expt 2.0l0 0.0f0)
  1.0l0)

(deftest expt.27
  (expt 2.0l0 0.0d0)
  1.0l0)

(deftest expt.28
  (<= (realpart (expt -8 1/3)) 0.0)
  nil)

#|
;;; FIXME
;;; I need to think more about how to do approximate float
;;; equality in a principled way.

(deftest expt.29
  (loop for bound in '(1.0s4 1.0f6 1.0d8 1.0l8)
	for ebound in (list short-float-epsilon single-float-epsilon
			    double-float-epsilon long-float-epsilon)
	for ebound2 = (max (* 2 ebound) (/ bound))
	nconc
	(loop for x = (1+ (random 1.0f6))
	      for s1 = (sqrt x)
	      for s2 = (expt x 1/2)
	      for error = (/ (abs (- s2 s2)) x)
	      repeat 1000
	      unless (< error ebound2)
	      collect (list x s1 s2)))
  nil)

(deftest expt.30
  (loop for bound in '(1.0s4 1.0f6 1.0d8 1.0l8)
	for ebound in (list short-float-epsilon single-float-epsilon
			    double-float-epsilon long-float-epsilon)
	for ebound2 = (max (* 2 ebound) (/ bound))
	nconc
	(loop for x = (- (1+ (random 1.0f6)))
	      for s1 = (sqrt x)
	      for s2 = (expt x 1/2)
	      for error = (/ (abs (- s2 s2)) x)
	      repeat 1000
	      unless (< error ebound2)
	      collect (list x s1 s2)))
  nil)
|#
