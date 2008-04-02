;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug  4 21:24:45 2003
;;;; Contains: Tests of MIN

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest min.error.1
  (signals-error (min) program-error)
  t)

(deftest min.error.2
  (check-type-error #'min #'realp)
  nil)

(deftest min.error.3
  (check-type-error #'(lambda (x) (min 0 x)) #'realp)
  nil)

(deftest min.1
  (loop for n in *reals*
	when (or (not (eql (min n) n))
		 (not (eql (min n n) n))
		 (not (eql (min n n n) n))
		 (not (eql (apply #'min (make-list
					 (min 256 (1- call-arguments-limit))
					 :initial-element n))
			   n)))
	collect n)
  nil)

(deftest min.2
  (min.2-fn)
  nil)

(deftest min.3
  (loop for x = (- (random 60000) 30000)
	for y = (- (random 60000) 30000)
	for m = (min x y)
	for m2 = (if (<= x y) x y)
	repeat 1000
	unless (eql m m2)
	collect (list x y m m2))
  nil)

(deftest min.4
  (loop for x = (- (random 6000000) 3000000)
	for y = (- (random 6000000) 3000000)
	for m = (min x y)
	for m2 = (if (<= x y) x y)
	repeat 1000
	unless (eql m m2)
	collect (list x y m m2))
  nil)

(deftest min.5
  (loop for x = (- (random 1000000000000) 500000000000)
	for y = (- (random 1000000000000) 500000000000)
	for m = (min x y)
	for m2 = (if (<= x y) x y)
	repeat 1000
	unless (eql m m2)
	collect (list x y m m2))
  nil)

(deftest min.6
  (let ((m (min 0 1.0s0)))
    (or (eqlt m 0)
	(eqlt m 0.0s0)))
  t)

(deftest min.7
  (min 2 1.0s0)
  1.0s0)

(deftest min.8
  (let ((m (min 2 3.0f0)))
    (or (eqlt m 2)
	(eqlt m 2.0f0)))
  t)

(deftest min.9
  (min 2 1.0f0)
  1.0f0)

(deftest min.10
  (let ((m (min 2 10.0d0)))
    (or (eqlt m 2)
	(eqlt m 2.0d0)))
  t)

(deftest min.11
  (min 100 1.0d0)
  1.0d0)

(deftest min.12
  (let ((m (min 2 17.25l0)))
    (or (eqlt m 2)
	(eqlt m 2.0l0)))
  t)

(deftest min.13
  (min 2 1.0l0)
  1.0l0)

(deftest min.15
  (let ((m (min 1.0s0 2.0f0)))
    (or (eqlt m 1.0s0)
	(eqlt m 1.0f0)))
  t)

(deftest min.16
  (min 3.0s0 1.0f0)
  1.0f0)

(deftest min.17
  (let ((m (min 1.0s0 2.0d0)))
    (or (eqlt m 1.0s0)
	(eqlt m 1.0d0)))
  t)

(deftest min.18
  (min 5.0s0 1.0d0)
  1.0d0)

(deftest min.19
  (let ((m (min 1.0s0 2.0l0)))
    (or (eqlt m 1.0s0)
	(eqlt m 1.0l0)))
  t)

(deftest min.20
  (min 2.0s0 1.0l0)
  1.0l0)

(deftest min.21
  (let ((m (min 1.0f0 2.0d0)))
    (or (eqlt m 1.0f0)
	(eqlt m 1.0d0)))
  t)

(deftest min.22
  (min 18.0f0 1.0d0)
  1.0d0)

(deftest min.23
  (let ((m (min 1.0f0 100.0l0)))
    (or (eqlt m 1.0f0)
	(eqlt m 1.0l0)))
  t)

(deftest min.24
  (min 19.0f0 1.0l0)
  1.0l0)

(deftest min.25
  (let ((m (min 1.0d0 12.0l0)))
    (or (eqlt m 1.0d0)
	(eqlt m 1.0l0)))
  t)

(deftest min.26
  (min 15.0d0 1.0l0)
  1.0l0)

(deftest min.27
  (loop for i from 1 to (min 256 (1- call-arguments-limit))
	for x = (make-list i :initial-element 1)
	do (setf (elt x (random i)) 0)
	unless (eql (apply #'min x) 0)
	collect x)
  nil)

(deftest min.28
  (let ((m (min 1/3 0.8s0)))
    (or (eqlt m 1/3)
	(eqlt m (float 1/3 0.8s0))))
  t)

(deftest min.29
  (let ((m (min 1.0s0 -3 2.0f0)))
    (or (eqlt m -3)
	(eqlt m -3.0f0)))
  t)

(deftest min.30
  (let ((m (min 1.0d0 -3 2.0f0)))
    (or (eqlt m -3)
	(eqlt m -3.0d0)))
  t)

(deftest min.31
  (let ((m (min 1.0s0 -3 2.0l0)))
    (or (eqlt m -3)
	(eqlt m -3.0l0)))
  t)

(deftest min.32
  (let ((m (min 1.0l0 -3 2.0s0)))
    (or (eqlt m -3)
	(eqlt m -3.0l0)))
  t)

(deftest min.33
  (let ((m (min 1.0d0 -3 2.0l0)))
    (or (eqlt m -3)
	(eqlt m -3.0l0)))
  t)

(deftest min.34
  (let ((m (min 1.0l0 -3 2.0d0)))
    (or (eqlt m -3)
	(eqlt m -3.0l0)))
  t)

(deftest min.order.1
  (let ((i 0) x y)
    (values
     (min (progn (setf x (incf i)) 10)
	  (progn (setf y (incf i)) 20))
     i x y))
  10 2 1 2)

(deftest min.order.2
  (let ((i 0) x y z)
    (values
     (min (progn (setf x (incf i)) 10)
	  (progn (setf y (incf i)) 20)
	  (progn (setf z (incf i)) 30))
     i x y z))
  10 3 1 2 3)

(deftest min.order.3
  (let ((i 0) u v w x y z)
    (values
     (min (progn (setf u (incf i)) 10)
	  (progn (setf v (incf i)) 20)
	  (progn (setf w (incf i)) 30)
	  (progn (setf x (incf i)) 10)
	  (progn (setf y (incf i)) 20)
	  (progn (setf z (incf i)) 30))
     i u v w x y z))
  10 6 1 2 3 4 5 6)
