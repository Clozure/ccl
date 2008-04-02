;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  1 13:49:18 2003
;;;; Contains: Tests of RATIONAL

(in-package :cl-test)

(deftest rational.error.1
  (signals-error (rational) program-error)
  t)

(deftest rational.error.2
  (signals-error (rational 0 nil) program-error)
  t)

(deftest rational.error.3
  (signals-error (rational 0 0) program-error)
  t)

(deftest rational.error.4
  (check-type-error #'rational #'realp)
  nil)

(deftest rational.1
  (loop for x in (loop for r in *reals*
		       when (or (not (floatp r))
				(<= -1000 (nth-value 1 (integer-decode-float r)) 1000))
		       collect r)
	for r = (rational x)
	unless (and (rationalp r)
		    (if (floatp x)
			(= (float r x) x)
		      (eql x r)))
	collect (list x r))
  nil)

(deftest rational.2
  (loop for type in '(short-float single-float double-float long-float)
	collect
	(loop for i from -10000 to 10000
	      for x = (coerce i type)
	      for r = (rational x)
	      count (not (eql r i))))
  (0 0 0 0))

(deftest rational.3
  (loop for type in '(short-float single-float double-float long-float)
	for bound in '(1.0s5 1.0f10 1.0d20 1.0l30)
	nconc
	(loop for x = (random-from-interval bound)
	      for r = (rational x)
	      for x2 = (float r x)
	      repeat 1000
	      unless (and (rationalp r) (= x x2))
	      collect (list x r x2)))
  nil)