;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  8 21:47:22 2003
;;;; Contains: Tests of LOGANDC1

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest logandc1.error.1
  (check-type-error #'(lambda (x) (logandc1 x 0)) #'integerp)
  nil)

(deftest logandc1.error.2
  (check-type-error #'(lambda (x) (logandc1 0 x)) #'integerp)
  nil)

(deftest logandc1.error.3
  (signals-error (logandc1) program-error)
  t)

(deftest logandc1.error.4
  (signals-error (logandc1 0) program-error)
  t)

(deftest logandc1.error.5
  (signals-error (logandc1 1 2 3) program-error)
  t)

;;; Non-error tests

(deftest logandc1.1
  (logandc1 0 0)
  0)

(deftest logandc1.2
  (logandc1 0 -1)
  -1)

(deftest logandc1.3
  (logandc1 0 123)
  123)

(deftest logandc1.4
  (loop for x in *integers*
	always (and (eql x (logandc1 0 x))
		    (eql 0 (logandc1 x x))
		    (eql x (logandc1 (lognot x) x))
		    (eql (lognot x) (logandc1 x (lognot x)))))
  t)

(deftest logandc1.5
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql x (logandc1 xc x))
	collect x)
  nil)

(deftest logandc1.6
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (logandc1 x y)
	repeat 1000
	unless (and (if (and (>= x 0) (< y 0))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (and (not (logbitp i x))
					  (logbitp i y))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest logandc1.order.1
  (let ((i 0) a b)
    (values
     (logandc1 (progn (setf a (incf i)) 0)
	       (progn (setf b (incf i)) -1))
     i a b))
  -1 2 1 2)
