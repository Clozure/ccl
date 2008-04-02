;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  8 21:23:22 2003
;;;; Contains: Tests of LOGAND

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest logand.error.1
  (check-type-error #'logand #'integerp)
  nil)

(deftest logand.error.2
  (check-type-error #'(lambda (x) (logand 0 x)) #'integerp)
  nil)

(deftest logand.error.3
  (check-type-error #'(lambda (x) (logand x 1)) #'integerp)
  nil)

;;; Non-error tests

(deftest logand.1
  (logand)
  -1)

(deftest logand.2
  (logand 1231)
  1231)

(deftest logand.3
  (logand -198)
  -198)

(deftest logand.4
  (loop for x in *integers*
	always (eql x (logand x)))
  t)

(deftest logand.5
  (loop for x in *integers*
	always (eql 0 (logand x (lognot x))))
  t)

(deftest logand.6
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql 0 (logand x xc))
	collect x)
  nil)

(deftest logand.7
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (logand x y)
	repeat 1000
	unless (and (if (and (< x 0) (< y 0))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (and (logbitp i x)
					  (logbitp i y))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest logand.8
  (loop for i from 1 to (min 256 (1- call-arguments-limit))
	for args = (nconc (make-list (1- i) :initial-element -1)
			  (list 183))
	always (eql (apply #'logand args) 183))
  t)

(deftest logand.9
  (loop for i from -1 to 0 always
	(loop for j from -1 to 0 always
	      (locally (declare (type (integer -1 0) i j))
		       (eql (logand i j) (if (or (zerop i) (zerop j)) 0 -1)))))
  t)

(deftest logand.order.1
  (let ((i 0) a b)
    (values
     (logand (progn (setf a (incf i)) #b11011)
	     (progn (setf b (incf i)) #b10110))
     i a b))
  #b10010 2 1 2)

(deftest logand.order.2
  (let ((i 0) a b c)
    (values
     (logand (progn (setf a (incf i))  #b11011)
	     (progn (setf b (incf i))  #b10110)
	     (progn (setf c (incf i)) #b110101))
     i a b c))
  #b10000 3 1 2 3)
