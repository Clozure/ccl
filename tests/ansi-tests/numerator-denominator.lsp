;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 08:24:57 2003
;;;; Contains: Tests of NUMERATOR, DENOMINATOR

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest numerator.error.1
  (signals-error (numerator) program-error)
  t)

(deftest numerator.error.2
  (signals-error (numerator 1/2 nil) program-error)
  t)

(deftest denominator.error.1
  (signals-error (denominator) program-error)
  t)

(deftest denominator.error.2
  (signals-error (denominator 1/2 nil) program-error)
  t)

(deftest numerator-denominator.1
  (loop for n = (abs (random-fixnum))
	for d = (1+ (abs (random-fixnum)))
	for g = (gcd n d)
	for n1 = (/ n g)
	for d1 = (/ d g)
	for r = (/ n d)
	for n2 = (numerator r)
	for d2 = (denominator r)
	repeat 1000
	unless (and (eql (gcd n1 d1) 1)
		    (>= n1 0)
		    (>= d1 1)
		    (eql n1 n2)
		    (eql d1 d2))
	collect (list n1 d1 r n2 d2))
  nil)

(deftest numerator-denominator.2
  (let ((bound (expt 10 20)))
    (loop
     for n = (random-from-interval bound 0)
     for d = (random-from-interval bound 1)
     for g = (gcd n d)
     for n1 = (/ n g)
     for d1 = (/ d g)
     for r = (/ n d)
     for n2 = (numerator r)
     for d2 = (denominator r)
     repeat 1000
     unless (and (eql (gcd n1 d1) 1)
		 (>= n1 0)
		 (>= d1 1)
		 (eql n1 n2)
		 (eql d1 d2))
     collect (list n1 d1 r n2 d2)))
  nil)

(deftest numerator-denominator.3
  (loop for n = (abs (random-fixnum))
	for d = (1+ (abs (random-fixnum)))
	for g = (gcd n d)
	for n1 = (/ n g)
	for d1 = (/ d g)
	for r = (/ n (- d))
	for n2 = (numerator r)
	for d2 = (denominator r)
	repeat 1000
	unless (and (eql (gcd n1 d1) 1)
		    (>= n1 0)
		    (>= d1 1)
		    (eql n1 (- n2))
		    (eql d1 d2))
	collect (list n1 d1 r n2 d2))
  nil)

(deftest numerator-denominator.4
  (let ((bound (expt 10 20)))
    (loop
     for n = (random-from-interval bound 0)
     for d = (random-from-interval bound 1)
     for g = (gcd n d)
     for n1 = (/ n g)
     for d1 = (/ d g)
     for r = (/ n (- d))
     for n2 = (numerator r)
     for d2 = (denominator r)
     repeat 1000
     unless (and (eql (gcd n1 d1) 1)
		 (>= n1 0)
		 (>= d1 1)
		 (eql n1 (- n2))
		 (eql d1 d2))
     collect (list n1 d1 r n2 d2)))
  nil)
	
(deftest numerator-denominator.5
  (loop for r in *rationals*
	for n = (numerator r)
	for d = (denominator r)
	unless (and (integerp n)
		    (integerp d)
		    (eql (gcd n d) 1)
		    (>= d 1)
		    (eql (/ n d) r))
	collect (list r n d))
  nil)
