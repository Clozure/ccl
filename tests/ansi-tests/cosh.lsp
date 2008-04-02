;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb 11 06:54:15 2004
;;;; Contains: Tests of COSH

(in-package :cl-test)

(deftest cosh.1
  (let ((result (cosh 0)))
    (or (eqlt result 1)
	(eqlt result 1.0)))
  t)

(deftest cosh.2
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	for one = (coerce 1 type)
	unless (equal (multiple-value-list (cosh zero))
		      (list one))
	collect type)
  nil)

(deftest cosh.3
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 `(complex ,type))
	for one = (coerce 1 `(complex ,type))
	unless (equal (multiple-value-list (cosh zero))
		      (list one))
	collect type)
  nil)

(deftest cosh.4
  (loop for den = (1+ (random 10000))
	for num = (random (* 10 den))
	for x = (/ num den)
	for rlist = (multiple-value-list (cosh x))
	for y = (car rlist)
	repeat 1000
	unless (and (null (cdr rlist))
		    (numberp y))
	collect (list x rlist))
  nil)

(deftest cosh.5
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x = (- (random (coerce 20 type)) 10)
	 for rlist = (multiple-value-list (cosh x))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y type))
	 collect (list x rlist)))
  nil)

(deftest cosh.6
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x1 = (- (random (coerce 20 type)) 10)
	 for x2 = (- (random (coerce 20 type)) 10)
	 for rlist = (multiple-value-list (cosh (complex x1 x2)))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y `(complex ,type)))
	 collect (list x1 x2 rlist)))
  nil)

;;; FIXME
;;; Add accuracy tests here

;;; Error tests

(deftest cosh.error.1
  (signals-error (cosh) program-error)
  t)

(deftest cosh.error.2
  (signals-error (cosh 1.0 1.0) program-error)
  t)

(deftest cosh.error.3
  (check-type-error #'cosh #'numberp)
  nil)





  
			    
  

