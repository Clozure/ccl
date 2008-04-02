;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb 11 19:16:35 2004
;;;; Contains: Tests of TANH

(in-package :cl-test)

(deftest tanh.1
  (let ((result (tanh 0)))
    (or (eqlt result 0)
	(eqlt result 0.0)))
  t)

(deftest tanh.2
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	unless (equal (multiple-value-list (tanh zero))
		      (list zero))
	collect type)
  nil)

(deftest tanh.3
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 `(complex ,type))
	unless (equal (multiple-value-list (tanh zero))
		      (list zero))
	collect type)
  nil)

(deftest tanh.4
  (loop for den = (1+ (random 10000))
	for num = (random (* 10 den))
	for x = (/ num den)
	for rlist = (multiple-value-list (tanh x))
	for y = (car rlist)
	repeat 1000
	unless (and (null (cdr rlist))
		    (numberp y))
	collect (list x rlist))
  nil)

(deftest tanh.5
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x = (- (random (coerce 20 type)) 10)
	 for rlist = (multiple-value-list (tanh x))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y type))
	 collect (list x rlist)))
  nil)

(deftest tanh.6
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x1 = (- (random (coerce 20 type)) 10)
	 for x2 = (- (random (coerce 20 type)) 10)
	 for rlist = (multiple-value-list (tanh (complex x1 x2)))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y `(complex ,type)))
	 collect (list x1 x2 rlist)))
  nil)

;;; FIXME
;;; Add accuracy tests here

;;; Error tests

(deftest tanh.error.1
  (signals-error (tanh) program-error)
  t)

(deftest tanh.error.2
  (signals-error (tanh 1.0 1.0) program-error)
  t)

(deftest tanh.error.3
  (check-type-error #'tanh #'numberp)
  nil)
