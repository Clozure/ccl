;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 21:53:51 2003
;;;; Contains: Tests of FLOAT

(in-package :cl-test)

(deftest float.error.1
  (signals-error (float) program-error)
  t)

(deftest float.error.2
  (signals-error (float 0 0.0 nil) program-error)
  t)

;;;

(deftest float.1
  (notnot (member (float 0) '(0.0f0 -0.0f0)))
  t)
  
(deftest float.2
  (float 1)
  1.0f0)

(deftest float.3
  (float -1)
  -1.0f0)

(deftest float.4
  (loop for i from -1000 to 1000
	always
	(loop for x in '(0.0s0 0.0f0 0.0d0 0.0l0)
	      for tp in '(short-float single-float double-float long-float)
	      for y = (float i x)
	      always (and (= i y) (typep y tp))))
  t)

(deftest float.5
  (loop for x in *reals*
	always (or (not (floatp x))
		   (eql (float x) x)))
  t)

(deftest float.6
  (loop for x in *reals*
	unless (handler-case
		(or (not (typep x 'short-float))
		   (let ((y (float x 0.0f0)))
		     (and (typep y 'single-float)
			  (= x y))))
		(arithmetic-error () t))
	collect x)
  nil)
  
(deftest float.7
  (loop for x in *reals*
	unless (or (not (typep x 'short-float))
		   (let ((y (float x 0.0d0)))
		     (and (typep y 'double-float)
			  (= x y))))
	collect x)
  nil)

(deftest float.8
  (loop for x in *reals*
	unless (or (not (typep x 'short-float))
		   (let ((y (float x 0.0l0)))
		     (and (typep y 'long-float)
			  (= x y))))
	collect x)
  nil)

(deftest float.9
  (loop for x in *reals*
	unless (or (not (typep x 'single-float))
		   (let ((y (float x 0.0d0)))
		     (and (typep y 'double-float)
			  (= x y))))
	collect x)
  nil)

(deftest float.10
  (loop for x in *reals*
	unless (or (not (typep x 'single-float))
		   (let ((y (float x 0.0l0)))
		     (and (typep y 'long-float)
			  (= x y))))
	collect x)
  nil)
  
(deftest float.11
  (loop for x in *reals*
	unless (or (not (typep x 'double-float))
		   (let ((y (float x 0.0l0)))
		     (and (typep y 'long-float)
			  (= x y))))
	collect x)
  nil)



  
