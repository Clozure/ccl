;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb 11 05:59:43 2004
;;;; Contains: Tests for ASIN

(in-package :cl-test)

(deftest asin.1
  (loop for i from -1000 to 1000
	for rlist = (multiple-value-list (asin i))
	for y = (car rlist)
	always (and (null (cdr rlist))
		    (numberp y)))
  t)

(deftest asin.2
  (loop for type in '(short-float single-float double-float long-float)
	collect
	(let ((a (coerce 2000 type))
	      (b (coerce -1000 type)))
	  (loop for x = (- (random a) b)
		for rlist = (multiple-value-list (asin x))
		for y = (car rlist)
		repeat 1000
		always (and (null (cdr rlist))
			    (numberp y)))))
  (t t t t))

(deftest asin.3
  (loop for type in '(integer short-float single-float double-float long-float)
	collect
	(let ((a (coerce 2000 type))
	      (b (coerce -1000 type)))
	  (loop for x = (- (random a) b)
		for rlist = (multiple-value-list (asin (complex 0 x)))
		for y = (car rlist)
		repeat 1000
		always (and (null (cdr rlist))
			    (numberp y)))))
  (t t t t t))

(deftest asin.4
  (loop for type in '(integer short-float single-float double-float long-float)
	collect
	(let ((a (coerce 2000 type))
	      (b (coerce -1000 type)))
	  (loop for x1 = (- (random a) b)
		for x2 = (- (random a) b)
		for rlist = (multiple-value-list (asin (complex x1 x2)))
		for y = (car rlist)
		repeat 1000
		always (and (null (cdr rlist))
			    (numberp y)))))
  (t t t t t))

(deftest asin.5
  (approx= (asin 1) (coerce (/ pi 2) 'single-float))
  t)

(deftest asin.6
  (loop for type in '(single-float short-float double-float long-float)
	unless (approx= (asin (coerce 1 type))
			(coerce (/ pi 2) type))
	collect type)
  nil)

(deftest asin.7
  (loop for type in '(single-float short-float double-float long-float)
	unless (approx= (asin (coerce 0 type))
			(coerce 0 type))
	collect type)
  nil)

(deftest asin.8
  (loop for type in '(single-float short-float double-float long-float)
	unless (approx= (asin (coerce -1 type))
			(coerce (/ pi -2) type))
	collect type)
  nil)

(deftest asin.9
  (macrolet ((%m (z) z)) (asin (expand-in-current-env (%m 0.0))))
  0.0)

;;; FIXME
;;; Add accuracy tests

;;; Error tests

(deftest asin.error.1
  (signals-error (asin) program-error)
  t)

(deftest asin.error.2
  (signals-error (asin 0.0 0.0) program-error)
  t)

(deftest asin.error.3
  (check-type-error #'asin #'numberp)
  nil)





