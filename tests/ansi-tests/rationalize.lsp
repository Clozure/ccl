;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  1 14:00:45 2003
;;;; Contains: Tests of RATIONALIZE

(in-package :cl-test)

(deftest rationalize.error.1
  (signals-error (rationalize) program-error)
  t)

(deftest rationalize.error.2
  (signals-error (rationalize 0 nil) program-error)
  t)

(deftest rationalize.error.3
  (signals-error (rationalize 0 0) program-error)
  t)

(deftest rationalize.error.4
  (check-type-error #'rationalize #'realp)
  nil)

(deftest rationalize.1
  (loop for x in (loop for r in *reals*
		       when (or (not (floatp r))
				(<= -1000 (nth-value 1 (integer-decode-float r)) 1000))
		       collect r)
	for r = (rationalize x)
	unless (and (rationalp r)
		    (if (floatp x)
			(= (float r x) x)
		      (eql x r)))
	collect (list x r))
  nil)

(deftest rationalize.2
  (loop for type in '(short-float single-float double-float long-float)
	collect
	(loop for i from -10000 to 10000
	      for x = (coerce i type)
	      for r = (rationalize x)
	      count (not (eql r i))))
  (0 0 0 0))

(deftest rationalize.3
  (loop for type in '(short-float single-float double-float long-float)
	for bound in '(1.0s5 1.0f10 1.0d20 1.0l30)
	nconc
	(loop for x = (random-from-interval bound)
	      for r = (rationalize x)
	      for x2 = (float r x)
	      repeat 1000
	      unless (and (rationalp r) (= x x2))
	      collect (list x r x2)))
  nil)
