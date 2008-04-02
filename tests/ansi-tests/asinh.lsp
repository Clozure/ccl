;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb 11 19:19:02 2004
;;;; Contains: Tests of ASINH

(in-package :cl-test)

(deftest asinh.1
  (let ((result (asinh 0)))
    (or (eqlt result 0)
	(eqlt result 0.0)))
  t)

(deftest asinh.2
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	unless (equal (multiple-value-list (asinh zero))
		      (list zero))
	collect type)
  nil)

(deftest asinh.3
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 `(complex ,type))
	unless (equal (multiple-value-list (asinh zero))
		      (list zero))
	collect type)
  nil)

(deftest asinh.4
  (loop for den = (1+ (random 10000))
	for num = (random (* 10 den))
	for x = (/ num den)
	for rlist = (multiple-value-list (asinh x))
	for y = (car rlist)
	repeat 1000
	unless (and (null (cdr rlist))
		    (numberp y))
	collect (list x rlist))
  nil)

(deftest asinh.5
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x = (- (random (coerce 20 type)) 10)
	 for rlist = (multiple-value-list (asinh x))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y type))
	 collect (list x rlist)))
  nil)

(deftest asinh.6
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x1 = (- (random (coerce 20 type)) 10)
	 for x2 = (- (random (coerce 20 type)) 10)
	 for rlist = (multiple-value-list (asinh (complex x1 x2)))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y `(complex ,type)))
	 collect (list x1 x2 rlist)))
  nil)

(deftest asinh.7
  (macrolet ((%m (z) z)) (asinh (expand-in-current-env (%m 0.0))))
  0.0)

;;; FIXME
;;; Add accuracy tests here

;;; Error tests

(deftest asinh.error.1
  (signals-error (asinh) program-error)
  t)

(deftest asinh.error.2
  (signals-error (asinh 1.0 1.0) program-error)
  t)

(deftest asinh.error.3
  (check-type-error #'asinh #'numberp)
  nil)





  
			    
  

