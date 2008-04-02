;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug  3 15:55:17 2003
;;;; Contains: Tests of MAX

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest max.error.1
  (signals-error (max) program-error)
  t)

(deftest max.error.2
  (check-type-error #'max #'realp)
  nil)

(deftest max.error.3
  (check-type-error #'(lambda (x) (max 0 x)) #'realp)
  nil)

;;; Non-error tests

(deftest max.1
  (loop for n in *reals*
	when (or (not (eql (max n) n))
		 (not (eql (max n n) n))
		 (not (eql (max n n n) n))
		 (not (eql (apply #'max (make-list
					 (min 256 (1- call-arguments-limit))
					 :initial-element n))
			   n)))
	collect n)
  nil)

(deftest max.2
  (max.2-fn)
  nil)

(deftest max.3
  (loop for x = (- (random 60000) 30000)
	for y = (- (random 60000) 30000)
	for m = (max x y)
	for m2 = (if (>= x y) x y)
	repeat 1000
	unless (eql m m2)
	collect (list x y m m2))
  nil)

(deftest max.4
  (loop for x = (- (random 6000000) 3000000)
	for y = (- (random 6000000) 3000000)
	for m = (max x y)
	for m2 = (if (>= x y) x y)
	repeat 1000
	unless (eql m m2)
	collect (list x y m m2))
  nil)

(deftest max.5
  (loop for x = (- (random 1000000000000) 500000000000)
	for y = (- (random 1000000000000) 500000000000)
	for m = (max x y)
	for m2 = (if (>= x y) x y)
	repeat 1000
	unless (eql m m2)
	collect (list x y m m2))
  nil)

(deftest max.6
  (let ((m (max 2 1.0s0)))
    (or (eqlt m 2)
	(eqlt m 2.0s0)))
  t)

(deftest max.7
  (max 0 1.0s0)
  1.0s0)

(deftest max.8
  (let ((m (max 2 1.0f0)))
    (or (eqlt m 2)
	(eqlt m 2.0f0)))
  t)

(deftest max.9
  (max 0 1.0f0)
  1.0f0)

(deftest max.10
  (let ((m (max 2 1.0d0)))
    (or (eqlt m 2)
	(eqlt m 2.0d0)))
  t)

(deftest max.11
  (max 0 1.0d0)
  1.0d0)

(deftest max.12
  (let ((m (max 2 1.0l0)))
    (or (eqlt m 2)
	(eqlt m 2.0l0)))
  t)

(deftest max.13
  (max 0 1.0l0)
  1.0l0)

(deftest max.15
  (let ((m (max 1.0s0 0.0f0)))
    (or (eqlt m 1.0s0)
	(eqlt m 1.0f0)))
  t)

(deftest max.16
  (max 0.0s0 1.0f0)
  1.0f0)

(deftest max.17
  (let ((m (max 1.0s0 0.0d0)))
    (or (eqlt m 1.0s0)
	(eqlt m 1.0d0)))
  t)

(deftest max.18
  (max 0.0s0 1.0d0)
  1.0d0)

(deftest max.19
  (let ((m (max 1.0s0 0.0l0)))
    (or (eqlt m 1.0s0)
	(eqlt m 1.0l0)))
  t)

(deftest max.20
  (max 0.0s0 1.0l0)
  1.0l0)

(deftest max.21
  (let ((m (max 1.0f0 0.0d0)))
    (or (eqlt m 1.0f0)
	(eqlt m 1.0d0)))
  t)

(deftest max.22
  (max 0.0f0 1.0d0)
  1.0d0)

(deftest max.23
  (let ((m (max 1.0f0 0.0l0)))
    (or (eqlt m 1.0f0)
	(eqlt m 1.0l0)))
  t)

(deftest max.24
  (max 0.0f0 1.0l0)
  1.0l0)

(deftest max.25
  (let ((m (max 1.0d0 0.0l0)))
    (or (eqlt m 1.0d0)
	(eqlt m 1.0l0)))
  t)

(deftest max.26
  (max 0.0d0 1.0l0)
  1.0l0)

(deftest max.27
  (loop for i from 1 to (min 256 (1- call-arguments-limit))
	for x = (make-list i :initial-element 0)
	do (setf (elt x (random i)) 1)
	unless (eql (apply #'max x) 1)
	collect x)
  nil)

(deftest max.28
  (let ((m (max 1/3 0.2s0)))
    (or (eqlt m 1/3)
	(eqlt m (float 1/3 0.2s0))))
  t)

(deftest max.29
  (let ((m (max 1.0s0 3 2.0f0)))
    (or (eqlt m 3)
	(eqlt m 3.0f0)))
  t)

(deftest max.30
  (let ((m (max 1.0d0 3 2.0f0)))
    (or (eqlt m 3)
	(eqlt m 3.0d0)))
  t)

(deftest max.31
  (let ((m (max 1.0s0 3 2.0l0)))
    (or (eqlt m 3)
	(eqlt m 3.0l0)))
  t)

(deftest max.32
  (let ((m (max 1.0l0 3 2.0s0)))
    (or (eqlt m 3)
	(eqlt m 3.0l0)))
  t)

(deftest max.33
  (let ((m (max 1.0d0 3 2.0l0)))
    (or (eqlt m 3)
	(eqlt m 3.0l0)))
  t)

(deftest max.34
  (let ((m (max 1.0l0 3 2.0d0)))
    (or (eqlt m 3)
	(eqlt m 3.0l0)))
  t)

(deftest max.order.1
  (let ((i 0) x y)
    (values
     (max (progn (setf x (incf i)) 10)
	  (progn (setf y (incf i)) 20))
     i x y))
  20 2 1 2)

(deftest max.order.2
  (let ((i 0) x y z)
    (values
     (max (progn (setf x (incf i)) 10)
	  (progn (setf y (incf i)) 20)
	  (progn (setf z (incf i)) 30))
     i x y z))
  30 3 1 2 3)

(deftest max.order.3
  (let ((i 0) u v w x y z)
    (values
     (max (progn (setf u (incf i)) 10)
	  (progn (setf v (incf i)) 20)
	  (progn (setf w (incf i)) 30)
	  (progn (setf x (incf i)) 10)
	  (progn (setf y (incf i)) 20)
	  (progn (setf z (incf i)) 30))
     i u v w x y z))
  30 6 1 2 3 4 5 6)
