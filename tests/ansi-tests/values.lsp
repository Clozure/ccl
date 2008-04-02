;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 19 08:18:50 2002
;;;; Contains: Tests of VALUES

(in-package :cl-test)

(deftest values.0
  (values))

(deftest values.1
  (values 1)
  1)

(deftest values.2
  (values 1 2)
  1 2)

(deftest values.3
  (values 1 2 3)
  1 2 3)

(deftest values.4
  (values 1 2 3 4)
  1 2 3 4)

(deftest values.10
  (values 1 2 3 4 5 6 7 8 9 10)
  1 2 3 4 5 6 7 8 9 10)

(deftest values.15
  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)

(deftest values.19
  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)

(deftest values.20
  (let ((a t) (b t) (c t) (d t) (e t) (f t))
    (setf (values a (values b c) (values d) (values e f)) (values 0 1 2 3 4 5 6))
    (list a b c d e f))
  (0 1 nil 2 3 nil))

(deftest values.21
  (let (a b c d e f)
    (setf (values a (values b c) (values d) (values e f)) (values 0 1 2 3 4 5 6)))
  0 1 2 3)

(deftest values.A
  (values (values 1 2) (values 3 4 5) (values) (values 10))
  1 3 nil 10)

(deftest values.B
  (funcall #'values 1 2 3 4)
  1 2 3 4)

(deftest values.C
  (let ((x (loop for i from 1 to (min 1000
				      (1- call-arguments-limit)
				      (1- multiple-values-limit))
		 collect i)))
    (equalt x
	    (multiple-value-list (apply #'values x))))
  t)

(deftest values.order.1
  (let ((i 0) a b c)
    (values (multiple-value-list
	     (values (setf a (incf i)) (setf b (incf i)) (setf c (incf i))))
	    i a b c))
  (1 2 3) 3 1 2 3)
  
