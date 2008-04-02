;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 05:52:31 2003
;;;; Contains: Tests of LOGANDC2

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest logandc2.error.1
  (check-type-error #'(lambda (x) (logandc2 x 0)) #'integerp)
  nil)

(deftest logandc2.error.2
  (check-type-error #'(lambda (x) (logandc2 0 x)) #'integerp)
  nil)

(deftest logandc2.error.3
  (signals-error (logandc2) program-error)
  t)

(deftest logandc2.error.4
  (signals-error (logandc2 0) program-error)
  t)

(deftest logandc2.error.5
  (signals-error (logandc2 1 2 3) program-error)
  t)

;;; Non-error tests

(deftest logandc2.1
  (logandc2 0 0)
  0)

(deftest logandc2.2
  (logandc2 -1 0)
  -1)

(deftest logandc2.3
  (logandc2 (1+ most-positive-fixnum) 0)
  #.(1+ most-positive-fixnum))

(deftest logandc2.4
  (loop for x in *integers*
	always (and (eql x (logandc2 x 0))
		    (eql 0 (logandc2 x x))
		    (eql x (logandc2 x (lognot x)))
		    (eql (lognot x) (logandc2 (lognot x) x))))
  t)

(deftest logandc2.5
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql x (logandc2 x xc))
	collect x)
  nil)

(deftest logandc2.6
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (logandc2 x y)
	repeat 1000
	unless (and (if (and (< x 0) (>= y 0))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (and (not (logbitp i y))
					  (logbitp i x))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest logandc2.order.1
  (let ((i 0) a b)
    (values
     (logandc2 (progn (setf a (incf i)) -1)
	       (progn (setf b (incf i)) 0))
     i a b))
  -1 2 1 2)
