;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 06:14:35 2003
;;;; Contains: Tests of LOGNOR

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest lognor.error.1
  (check-type-error #'(lambda (x) (lognor x 0)) #'integerp)
  nil)

(deftest lognor.error.2
  (check-type-error #'(lambda (x) (lognor 0 x)) #'integerp)
  nil)

(deftest lognor.error.3
  (signals-error (lognor) program-error)
  t)

(deftest lognor.error.4
  (signals-error (lognor 0) program-error)
  t)

(deftest lognor.error.5
  (signals-error (lognor 1 2 3) program-error)
  t)

;;; Non-error tests

(deftest lognor.1
  (lognor 0 0)
  -1)

(deftest lognor.2
  (lognor 0 -1)
  0)

(deftest lognor.3
  (lognor -1 123)
  0)

(deftest lognor.4
  (loop for x in *integers*
	always (and (eql (lognot x) (lognor 0 x))
		    (eql (lognot x) (lognor x x))
		    (eql 0 (lognor (lognot x) x))
		    (eql 0 (lognor x (lognot x)))))
  t)

(deftest lognor.5
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql 0 (lognor xc x))
	collect x)
  nil)

(deftest lognor.6
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (lognor x y)
	repeat 1000
	unless (and (if (and (>= x 0) (>= y 0))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (not (or (logbitp i x)
					      (logbitp i y)))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest lognor.order.1
  (let ((i 0) a b)
    (values
     (lognor (progn (setf a (incf i)) -2)
	     (progn (setf b (incf i)) -3))
     i a b))
  0 2 1 2)


