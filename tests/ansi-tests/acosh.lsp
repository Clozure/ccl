;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb 11 19:20:53 2004
;;;; Contains: Tests of ACOSH

(in-package :cl-test)

(deftest acosh.1
  (let ((result (acosh 1)))
    (or (eqlt result 0)
	(eqlt result 0.0)))
  t)

(deftest acosh.2
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	for one = (coerce 1 type)
	unless (equal (multiple-value-list (acosh one))
		      (list zero))
	collect type)
  nil)

(deftest acosh.3
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 `(complex ,type))
	for one = (coerce 1 `(complex ,type))
	unless (equal (multiple-value-list (acosh one))
		      (list zero))
	collect type)
  nil)

(deftest acosh.4
  (loop for den = (1+ (random 10000))
	for num = (random (* 10 den))
	for x = (/ num den)
	for rlist = (multiple-value-list (acosh x))
	for y = (car rlist)
	repeat 1000
	unless (and (null (cdr rlist))
		    (numberp y))
	collect (list x rlist))
  nil)

(deftest acosh.5
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x = (1+ (random (coerce 1000 type)))
	 for rlist = (multiple-value-list (acosh x))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y type))
	 collect (list x rlist)))
  nil)

(deftest acosh.6
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x1 = (- (random (coerce 20 type)) 10)
	 for x2 = (- (random (coerce 20 type)) 10)
	 for rlist = (multiple-value-list (acosh (complex x1 x2)))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y `(complex ,type)))
	 collect (list x1 x2 rlist)))
  nil)

(deftest acosh.7
  (macrolet ((%m (z) z)) (not (not (complexp (acosh (expand-in-current-env (%m 0)))))))
  t)

;;; FIXME
;;; Add accuracy tests here

;;; Error tests

(deftest acosh.error.1
  (signals-error (acosh) program-error)
  t)

(deftest acosh.error.2
  (signals-error (acosh 1.0 1.0) program-error)
  t)

(deftest acosh.error.3
  (check-type-error #'acosh #'numberp)
  nil)





  
			    
  


