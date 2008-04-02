;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep  4 22:03:21 2003
;;;; Contains: Tests of LCM

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "gcd-aux.lsp")

(deftest lcm.error.1
  (check-type-error #'lcm #'integerp)
  nil)

(deftest lcm.1
  (lcm)
  1)

(deftest lcm.2
  (loop for i = (random-fixnum)
	for a = (abs i)
	repeat 1000
	unless (and (eql a (lcm i))
		    (eql a (lcm 1 i)))
	collect i)
  nil)

(deftest lcm.3
  (loop for i = (random-from-interval 10000000000000000)
	for a = (abs i)
	repeat 1000
	unless (and (eql a (lcm i))
		    (eql a (lcm i 1)))
	collect i)
  nil)

(deftest lcm.4
  (loop for i = (random-fixnum)
	for j = (random-fixnum)
	repeat 1000
	unless (eql (my-lcm i j) (lcm i j))
	collect (list i j))
  nil)

(deftest lcm.5
  (let ((bound (ash 1 200)))
    (loop for i = (random-from-interval bound)
	  for j = (random-from-interval bound)
	  repeat 1000
	  unless (eql (my-lcm i j) (lcm i j))
	  collect (list i j)))
  nil)

(deftest lcm.6
  (loop for i = (random-fixnum)
	for j = (random-fixnum)
	for k = (random-fixnum)
	repeat 1000
	unless (eql (my-lcm i (my-lcm j k)) (lcm i j k))
	collect (list i j k))
  nil)

(deftest lcm.7
  (loop for i = (random-fixnum)
	for j = (random-fixnum)
	for k = (random-fixnum)
	for n = (random-fixnum)
	repeat 1000
	unless (eql (my-lcm (my-lcm i j) (my-lcm k n)) (lcm i j k n))
	collect (list i j k n))
  nil)

(deftest lcm.8
  (loop for i from 1 to (min 256 (1- call-arguments-limit))
	always (eql (apply #'lcm (make-list i :initial-element 1)) 1))
  t)

(deftest lcm.9
  (lcm 0 0)
  0)

(deftest lcm.10
  (lcm 1 0 0)
  0)

(deftest lcm.11
  (lcm 0 1 0)
  0)

(deftest lcm.12
  (lcm 0 0 1)
  0)


(deftest lcm.order.1
  (let ((i 0) x y)
    (values
     (lcm (progn (setf x (incf i)) 15)
	  (progn (setf y (incf i)) 25))
     i x y))
  75 2 1 2)

(deftest lcm.order.2
  (let ((i 0) x y)
    (values
     (lcm (progn (setf x (incf i)) 0)
	  (progn (setf y (incf i)) 10))
     i x y))
  0 2 1 2)

(deftest lcm.order.3
  (let ((i 0))
    (values
     (lcm (progn (incf i) 0))
     i))
  0 1)


