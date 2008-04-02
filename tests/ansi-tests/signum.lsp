;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep  4 22:29:09 2003
;;;; Contains: Tests of SIGNUM

(in-package :cl-test)

(deftest signum.error.1
  (signals-error (signum) program-error)
  t)

(deftest signum.error.2
  (signals-error (signum 1 1) program-error)
  t)

(deftest signum.error.3
  (signals-error (signum 1 nil) program-error)
  t)

(deftest signum.1
  (signum 0)
  0)

(deftest signum.2
  (signum 123)
  1)

(deftest signum.3
  (signum -123123)
  -1)

(deftest signum.4
  (loop for i in *rationals*
	for s = (signum i)
	unless (cond
		((zerop i) (eql s 0))
		((plusp i) (eql s 1))
		(t (eql s -1)))
	collect (list i s))
  nil)

(deftest signum.5
  (loop for x in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for one = (float 1 x)
	for y = (float 13122 x)
	for s1 = (signum x)
	for s2 = (signum y)
	for s3 = (signum (- y))
	unless (and (eql s1 x)
		    (eql s2 one)
		    (eql s3 (- one)))
	collect (list x one y s1 s2 s3))
  nil)

(deftest signum.6
  (loop
   for tp in '(short-float single-float double-float long-float)
   for z = (coerce 0 tp)
   for mz = (- z)
   nconc
   (loop for x in (list z mz)
	 nconc
	 (loop for y in (list z mz)
	       for c = (complex z mz)
	       for s = (signum c)
	       unless (eql c s)
	       collect (list c s))))
  nil)

(deftest signum.7
  (loop
   for tp in '(short-float single-float double-float long-float)
   for z = (coerce 0 tp)
   for one = (coerce 1 tp)
   for onem = (coerce -1 tp)
   for c1 = (complex one z)
   for c2 = (complex onem z)
   for c3 = (complex z one)
   for c4 = (complex z onem)
   unless (eql c1 (signum c1))
   collect (list c1 (signum c1))
   unless (eql c2 (signum c2))
   collect (list c2 (signum c2))
   unless (eql c3 (signum c3))
   collect (list c3 (signum c3))
   unless (eql c4 (signum c4))
   collect (list c4 (signum c4)))
  nil)

(deftest signum.8
  (let* ((c (complex 0 1))
	 (s (signum c)))
    (or (eqlt c s)
	(eqlt s #c(0.0 1.0))))
  t)

(deftest signum.9
  (let* ((c (complex 0 -1))
	 (s (signum c)))
    (or (eqlt c s)
	(eqlt s #c(0.0 -1.0))))
  t)

(deftest signum.10
  (let* ((c (complex 3/5 4/5))
	 (s (signum c)))
    (or (eqlt c s)
	(eqlt s (complex (float 3/5) (float 4/5)))))
  t)

(deftest signum.11
  (let ((i 0)) (values (signum (the (integer 1 1) (incf i))) i))
  1 1)

