;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Feb 10 05:39:24 2004
;;;; Contains: Tess of ACOS

(in-package :cl-test)

(deftest acos.1
  (loop for i from -1000 to 1000
	for rlist = (multiple-value-list (acos i))
	for y = (car rlist)
	always (and (null (cdr rlist))
		    (numberp y)))
  t)

(deftest acos.2
  (loop for type in '(short-float single-float double-float long-float)
	collect
	(let ((a (coerce 2000 type))
	      (b (coerce -1000 type)))
	  (loop for x = (- (random a) b)
		for rlist = (multiple-value-list (acos x))
		for y = (car rlist)
		repeat 1000
		always (and (null (cdr rlist))
			    (numberp y)))))
  (t t t t))

(deftest acos.3
  (loop for type in '(integer short-float single-float double-float long-float)
	collect
	(let ((a (coerce 2000 type))
	      (b (coerce -1000 type)))
	  (loop for x = (- (random a) b)
		for rlist = (multiple-value-list (acos (complex 0 x)))
		for y = (car rlist)
		repeat 1000
		always (and (null (cdr rlist))
			    (numberp y)))))
  (t t t t t))

(deftest acos.4
  (loop for type in '(integer short-float single-float double-float long-float)
	collect
	(let ((a (coerce 2000 type))
	      (b (coerce -1000 type)))
	  (loop for x1 = (- (random a) b)
		for x2 = (- (random a) b)
		for rlist = (multiple-value-list (acos (complex x1 x2)))
		for y = (car rlist)
		repeat 1000
		always (and (null (cdr rlist))
			    (numberp y)))))
  (t t t t t))

(deftest acos.5
  (approx= (acos 0) (coerce (/ pi 2) 'single-float))
  t)

(deftest acos.6
  (loop for type in '(single-float short-float double-float long-float)
	unless (approx= (acos (coerce 0 type))
			(coerce (/ pi 2) type))
	collect type)
  nil)

(deftest acos.7
  (loop for type in '(single-float short-float double-float long-float)
	unless (approx= (acos (coerce 1 type))
			(coerce 0 type))
	collect type)
  nil)

(deftest acos.8
  (loop for type in '(single-float short-float double-float long-float)
	unless (approx= (acos (coerce -1 type))
			(coerce pi type))
	collect type)
  nil)

(deftest acos.9
  (macrolet ((%m (z) z)) (not (not (> (acos (expand-in-current-env (%m 0))) 0))))
  t)

;;; FIXME
;;; Add accuracy tests

;;; Error tests

(deftest acos.error.1
  (signals-error (acos) program-error)
  t)

(deftest acos.error.2
  (signals-error (acos 0.0 0.0) program-error)
  t)

(deftest acos.error.3
  (check-type-error #'acos #'numberp)
  nil)
