;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 10:54:17 2003
;;;; Contains: Tests of SQRT

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest sqrt.error.1
  (signals-error (sqrt) program-error)
  t)

(deftest sqrt.error.2
  (signals-error (sqrt 0 nil) program-error)
  t)

(deftest sqrt.error.3
  (check-type-error #'sqrt #'numberp)
  nil)

(deftest sqrt.1
  (let ((s (sqrt 0)))
    (and (realp s)
	 (=t s 0)))
  t)

(deftest sqrt.2
  (let ((s (sqrt 1)))
    (and (realp s)
	 (=t s 1)))
  t)

(deftest sqrt.3
  (loop for x in '(0.0s0 1.0s0 0.0f0 1.0f0 0.0d0 1.0d0 0.0l0 1.0l0)
	for s = (sqrt x)
	unless (eql s x)
	collect (list x s))
  nil)

(deftest sqrt.4
  (loop for x in '(0.0s0 1.0s0 0.0f0 1.0f0 0.0d0 1.0d0 0.0l0 1.0l0)
	for c = (complex x 0)
	for s = (sqrt c)
	unless (eql s c)
	collect (list x c s))
  nil)

(deftest sqrt.5
  (loop for x in '(-1.0s0 -1.0f0 -1.0d0 -1.0l0)
	for s = (sqrt x)
	unless (eql s (complex 0 (- x)))
	collect (list x s))
  nil)

;;; (deftest sqrt.6
;;;  (let ((result (sqrt (ash 1 10000))))
;;;    (if (integerp result)
;;;	(=t result (ash 1 5000))
;;;      (=t result (float (ash 1 5000) result))))
;;;  t)

(deftest sqrt.7
  (let ((result (sqrt -1)))
    (or (eqlt result #c(0 1))
	(eqlt result #c(0.0 1.0))))
  t)

(deftest sqrt.8
  (loop for x in *floats*
	for s = (sqrt x)
	unless (cond
		((zerop x) (=t x 0))
		((plusp x) (and (eqlt (float s x) s)
				(eqlt (float x s) x)))
		(t (complexp s)))
	collect (list x s))
  nil)

(deftest sqrt.9
  (let ((upper (rational most-positive-double-float))
	(lower (rational most-negative-double-float)))
    (loop for x = (random-fixnum)
	  repeat 1000
	  unless (or (< x lower)
		     (> x upper)
		     (let ((s (sqrt x)))
		       (or (and (rationalp s)
				(>= s 0)
				(eql (* s s) x))
			   (and (floatp s) (>= x 0))
			   (and (complexp s)
				(zerop (realpart s))
				(> (imagpart s) 0)
				(< x 0)))))
	  collect (list x (sqrt x))))
  nil)

(deftest sqrt.10
  (loop for x from 1 to 1000
	for x2 = (* x x)
	for s = (sqrt x2)
	unless (if (rationalp s) (eql x s)
		 (and (typep s 'single-float)
		      (= x s)))
	collect (list x s))
  nil)

(deftest sqrt.11
  (loop for x from 1 to 1000
	for x2 = (* x x)
	for s = (sqrt (- x2))
	unless (and (complexp s)
		    (zerop (realpart s))
		    (let ((i (imagpart s)))
		      (if (rationalp i)
			  (eql i x)
			(= i x))))
	collect (list x s))
  nil)

;;; Tests of the branch cut
(deftest sqrt.12
  (loop for xr = (random-fixnum)
	for xi = (random-fixnum)
	for c = (complex xr xi)
	for s = (sqrt c)
	repeat 1000
	unless (or (> (realpart s) 0)
		   (and (= (realpart s) 0)
			(>= (imagpart s) 0)))
	collect (list c s))
  nil)

(deftest sqrt.13
  (loop for xr = (random-from-interval 1.0f6 -1.0f6)
	for xi = (random-from-interval 1.0f6 -1.0f6)
	for c = (complex xr xi)
	for s = (sqrt c)
	repeat 1000
	unless (or (> (realpart s) 0)
		   (and (= (realpart s) 0)
			(>= (imagpart s) 0)))
	collect (list c s))
  nil)
			
(deftest sqrt.14
  (loop for xr = (random-from-interval 1.0s3 -1.0s3)
	for xi = (random-from-interval 1.0s3 -1.0s3)
	for c = (complex xr xi)
	for s = (sqrt c)
	repeat 1000
	unless (or (> (realpart s) 0)
		   (and (= (realpart s) 0)
			(>= (imagpart s) 0)))
	collect (list c s))
  nil)
			
(deftest sqrt.15
  (loop for xr = (random-from-interval 1.0d7 -1.0d7)
	for xi = (random-from-interval 1.0d7 -1.0d7)
	for c = (complex xr xi)
	for s = (sqrt c)
	repeat 1000
	unless (or (> (realpart s) 0)
		   (and (= (realpart s) 0)
			(>= (imagpart s) 0)))
	collect (list c s))
  nil)

(deftest sqrt.16
  (loop for xr = (random-from-interval 1.0l9 -1.0l9)
	for xi = (random-from-interval 1.0l9 -1.0l9)
	for c = (complex xr xi)
	for s = (sqrt c)
	repeat 1000
	unless (or (> (realpart s) 0)
		   (and (= (realpart s) 0)
			(>= (imagpart s) 0)))
	collect (list c s))
  nil)

(deftest sqrt.17
  (let ((b1 (find-largest-exactly-floatable-integer most-positive-fixnum)))
    (loop for i = (random-from-interval (* b1 b1) 0)
	  repeat 1000
	  unless (>= (sqrt i) (isqrt i))
	  collect i))
  nil)

(deftest sqrt.18
  (loop for x = (random-from-interval 1.0f6 0.0f0)
	repeat 1000
	unless (>= (sqrt x) (isqrt (floor x)))
	collect x)
  nil)

(deftest sqrt.19
  (loop for x in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for s = (sqrt x)
	unless (= s x)
	collect (list x s))
  nil)

