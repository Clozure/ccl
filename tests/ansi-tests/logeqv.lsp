;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 05:55:23 2003
;;;; Contains: Tests of LOGEQV

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest logeqv.error.1
  (check-type-error #'logeqv #'integerp)
  nil)

(deftest logeqv.error.2
  (check-type-error #'(lambda (x) (logeqv 0 x)) #'integerp)
  nil)

;;; Non-error tests

(deftest logeqv.1
  (logeqv)
  -1)

(deftest logeqv.2
  (logeqv 1231)
  1231)

(deftest logeqv.3
  (logeqv -198)
  -198)

(deftest logeqv.4
  (loop for x in *integers*
	always (eql x (logeqv x)))
  t)

(deftest logeqv.5
  (loop for x in *integers*
	always (eql 0 (logeqv x (lognot x))))
  t)

(deftest logeqv.6
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql 0 (logeqv x xc))
	collect x)
  nil)

(deftest logeqv.7
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (logeqv x y)
	repeat 1000
	unless (and (if (or (and (< x 0) (< y 0))
			    (and (>= x 0) (>= y 0)))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (or (and (logbitp i x)
					      (logbitp i y))
					 (and (not (logbitp i x))
					      (not (logbitp i y))))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest logeqv.8
  (loop for i from 1 to (min 256 (1- call-arguments-limit))
	for args = (nconc (make-list (1- i) :initial-element -1)
			  (list 7131))
	always (eql (apply #'logeqv args) 7131))
  t)	

(deftest logeqv.order.1
  (let ((i 0) a b)
    (values
     (logeqv (progn (setf a (incf i)) #b11011)
	     (progn (setf b (incf i)) (lognot #b10110)))
     i a b))
  #b1101 2 1 2)
  

(deftest logeqv.order.2
  (let ((i 0) a b c)
    (values
     (logeqv (progn (setf a (incf i))  #b11011)
	     (progn (setf b (incf i))  #b10110)
	     (progn (setf c (incf i)) #b110101))
     i a b c))
  #b111000 3 1 2 3)
