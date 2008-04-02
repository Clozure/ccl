;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 06:11:12 2003
;;;; Contains: Tests of LOGNAND

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest lognand.error.1
  (check-type-error #'(lambda (x) (lognand x 0)) #'integerp)
  nil)

(deftest lognand.error.2
  (check-type-error #'(lambda (x) (lognand 0 x)) #'integerp)
  nil)

(deftest lognand.error.3
  (signals-error (lognand) program-error)
  t)

(deftest lognand.error.4
  (signals-error (lognand 0) program-error)
  t)

(deftest lognand.error.5
  (signals-error (lognand 1 2 3) program-error)
  t)

;;; Non-error tests

(deftest lognand.1
  (lognand 0 0)
  -1)

(deftest lognand.2
  (lognand 0 -1)
  -1)

(deftest lognand.3
  (lognand -1 123)
  -124)

(deftest lognand.4
  (loop for x in *integers*
	always (and (eql -1 (lognand 0 x))
		    (eql (lognot x) (lognand x x))
		    (eql -1 (lognand (lognot x) x))
		    (eql -1 (lognand x (lognot x)))))
  t)

(deftest lognand.5
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql -1 (lognand xc x))
	collect x)
  nil)

(deftest lognand.6
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (lognand x y)
	repeat 1000
	unless (and (if (or (>= x 0) (>= y 0))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (not (and (logbitp i x)
					       (logbitp i y)))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest lognand.order.1
  (let ((i 0) a b)
    (values
     (lognand (progn (setf a (incf i)) -2)
	      (progn (setf b (incf i)) -3))
     i a b))
  3 2 1 2)

