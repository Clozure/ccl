;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Sep  3 06:51:03 2003
;;;; Contains: Tests of GCD

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "gcd-aux.lsp")

;;; Error tests

(deftest gcd.error.1
  (check-type-error #'gcd #'integerp)
  nil)

;;; Non-error tests

(deftest gcd.1
  (gcd)
  0)

(deftest gcd.2
  (loop for i = (random-fixnum)
	for a = (abs i)
	repeat 10000
	unless (and (eql a (gcd i))
		    (eql a (gcd 0 i)))
	collect i)
  nil)

(deftest gcd.3
  (loop for i = (random-from-interval 10000000000000000)
	for a = (abs i)
	repeat 10000
	unless (and (eql a (gcd i))
		    (eql a (gcd i 0)))
	collect i)
  nil)

(deftest gcd.4
  (loop for i = (random-fixnum)
	for j = (random-fixnum)
	repeat 1000
	unless (eql (my-gcd i j) (gcd i j))
	collect (list i j))
  nil)

(deftest gcd.5
  (let ((bound (ash 1 200)))
    (loop for i = (random-from-interval bound)
	  for j = (random-from-interval bound)
	  repeat 1000
	  unless (eql (my-gcd i j) (gcd i j))
	  collect (list i j)))
  nil)

(deftest gcd.6
  (loop for i = (random-fixnum)
	for j = (random-fixnum)
	for k = (random-fixnum)
	repeat 1000
	unless (eql (my-gcd i (my-gcd j k)) (gcd i j k))
	collect (list i j k))
  nil)

(deftest gcd.7
  (loop for i = (random-fixnum)
	for j = (random-fixnum)
	for k = (random-fixnum)
	for n = (random-fixnum)
	repeat 1000
	unless (eql (my-gcd (my-gcd i j) (my-gcd k n)) (gcd i j k n))
	collect (list i j k))
  nil)

(deftest gcd.8
  (loop for i from 1 to (min 256 (1- call-arguments-limit))
	always (eql (apply #'gcd (make-list i :initial-element 1)) 1))
  t)

(deftest gcd.order.1
  (let ((i 0) x y)
    (values
     (gcd (progn (setf x (incf i)) 15)
	  (progn (setf y (incf i)) 25))
     i x y))
  5 2 1 2)

(deftest gcd.order.2
  (let ((i 0) x y)
    (values
     (gcd (progn (setf x (incf i)) 0)
	  (progn (setf y (incf i)) 10))
     i x y))
  10 2 1 2)

(deftest gcd.order.3
  (let ((i 0))
    (values
     (gcd (progn (incf i) 0))
     i))
  0 1)

    