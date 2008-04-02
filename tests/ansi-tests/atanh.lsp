;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb 11 19:26:25 2004
;;;; Contains: Tests of ATANH

(in-package :cl-test)

(deftest atanh.1
  (let ((result (atanh 0)))
    (or (eqlt result 0)
	(eqlt result 0.0)))
  t)

(deftest atanh.2
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	unless (equal (multiple-value-list (atanh zero))
		      (list zero))
	collect type)
  nil)

(deftest atanh.3
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 `(complex ,type))
	unless (equal (multiple-value-list (atanh zero))
		      (list zero))
	collect type)
  nil)

(deftest atanh.4
  (loop for den = (1+ (random 10000))
	for num = (random den)
	for x = (/ num den)
	for rlist = (multiple-value-list (atanh x))
	for y = (car rlist)
	repeat 1000
	unless (and (null (cdr rlist))
		    (numberp y))
	collect (list x rlist))
  nil)

(deftest atanh.5
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x = (if (eql (random 2) 0)
		     (+ 2 (random (coerce 1000 type)))
		   (- -2 (random (coerce 1000 type))))
	 for rlist = (multiple-value-list (atanh x))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y `(complex ,type)))
	 collect (list x rlist)))
  nil)

(deftest atanh.5a
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x = (- (random (coerce 1.9998s0 type)) 0.9999s0)
	 for rlist = (multiple-value-list (atanh x))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y type))
	 collect (list x rlist)))
  nil)

(deftest atanh.6
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x1 = (- (random (coerce 1.9998s0 type)) 0.9999s0)
	 for rlist = (multiple-value-list (atanh (complex x1 0.0s0)))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y `(complex ,type)))
	 collect (list x1 rlist)))
  nil)

(deftest atanh.7
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x1 = (- (random (coerce 1.9998s0 type)) 0.9999s0)
	 for rlist = (multiple-value-list (atanh (complex 0.0s0 x1)))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y `(complex ,type)))
	 collect (list x1 rlist)))
  nil)

(deftest atanh.8
  (macrolet ((%m (z) z)) (atanh (expand-in-current-env (%m 0.0))))
  0.0)


;;; FIXME
;;; Add accuracy tests here

;;; Error tests

(deftest atanh.error.1
  (signals-error (atanh) program-error)
  t)

(deftest atanh.error.2
  (signals-error (atanh 1.0 1.0) program-error)
  t)

(deftest atanh.error.3
  (check-type-error #'atanh #'numberp)
  nil)
