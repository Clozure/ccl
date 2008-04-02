;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb 11 19:53:33 2004
;;;; Contains: Tests of LOG

(in-package :cl-test)

(deftest log.1
  (let ((result (log 1)))
    (or (eqlt result 0)
	(eqlt result 0.0)))
  t)

(deftest log.2
  (mapcar #'log '(1.0s0 1.0f0 1.0d0 1.0l0))
  (0.0s0 0.0f0 0.0d0 0.0l0))

(deftest log.3
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x = (+ (random (coerce 1 type)) (/ 1 1000))
	 for rlist = (multiple-value-list (log x))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y type))
	 collect (list x rlist)))
  nil)

(deftest log.4
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(loop
	 for x = (1+ (random (coerce 1000000 type)))
	 for rlist = (multiple-value-list (log x))
	 for y = (car rlist)
	 repeat 1000
	 unless (and (null (cdr rlist))
		     (typep y type))
	 collect (list x rlist)))
  nil)

(deftest log.5
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	nconc
	(loop
	 for x = (- (random (coerce 1 type)))
	 for rlist = (and (/= x zero) (multiple-value-list (log x)))
	 for y = (car rlist)
	 repeat 1000
	 unless (or (= x zero)
		    (and (null (cdr rlist))
			 (typep y `(complex ,type))))
	 collect (list x rlist)))
  nil)

(deftest log.6
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	nconc
	(loop
	 for x = (- (random (coerce 1000000 type)))
	 for rlist = (and (/= x zero) (multiple-value-list (log x)))
	 for y = (car rlist)
	 repeat 1000
	 unless (or (= x zero)
		    (and (null (cdr rlist))
			 (typep y `(complex ,type))))
	 collect (list x rlist)))
  nil)

(deftest log.7
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	nconc
	(loop
	 for x1 = (- (random (coerce 2000 type)) 1000)
	 for x2 = (1+ (random (coerce 1000 type)))
	 for rlist = (and (/= x1 zero)
			  (multiple-value-list (log (complex x1 x2))))
	 for y = (car rlist)
	 repeat 1000
	 unless (or (= x1 zero)
		    (and (null (cdr rlist))
			 (typep y `(complex ,type))))
	 collect (list x1 x2 rlist)))
  nil)

(deftest log.8
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	nconc
	(loop
	 for x1 = (- (random (coerce 2000 type)) 1000)
	 for x2 = (- -1 (random (coerce 1000 type)))
	 for rlist = (and (/= x1 zero)
			  (multiple-value-list (log (complex x1 x2))))
	 for y = (car rlist)
	 repeat 1000
	 unless (or (= x1 zero)
		    (and (null (cdr rlist))
			 (typep y `(complex ,type))))
	 collect (list x1 x2 rlist)))
  nil)

;;; FIXME
;;; Add tests for two-arg calls
	 
;;; FIXME
;;; More accuracy tests here

;;; Error tests

(deftest log.error.1
  (signals-error (log) program-error)
  t)

(deftest log.error.2
  (signals-error (log 1.0 2.0 3.0) program-error)
  t)

