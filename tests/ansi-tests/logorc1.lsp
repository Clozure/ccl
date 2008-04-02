;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 06:23:43 2003
;;;; Contains: Tests of LOGORC1

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest logorc1.error.1
  (check-type-error #'(lambda (x) (logorc1 x 0)) #'integerp)
  nil)

(deftest logorc1.error.2
  (check-type-error #'(lambda (x) (logorc1 0 x)) #'integerp)
  nil)

(deftest logorc1.error.3
  (signals-error (logorc1) program-error)
  t)

(deftest logorc1.error.4
  (signals-error (logorc1 0) program-error)
  t)

(deftest logorc1.error.5
  (signals-error (logorc1 1 2 3) program-error)
  t)

;;; Non-error tests

(deftest logorc1.1
  (logorc1 0 0)
  -1)

(deftest logorc1.2
  (logorc1 0 -1)
  -1)

(deftest logorc1.2a
  (logorc1 -1 0)
  0)

(deftest logorc1.3
  (logorc1 123 0)
  -124)

(deftest logorc1.4
  (loop for x in *integers*
	always (and (eql -1 (logorc1 0 x))
		    (eql x (logorc1 -1 x))
		    (eql -1 (logorc1 x x))
		    (eql x (logorc1 (lognot x) x))
		    (eql (lognot x) (logorc1 x (lognot x)))))
  t)

(deftest logorc1.5
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql x (logorc1 xc x))
	collect x)
  nil)

(deftest logorc1.6
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (logorc1 x y)
	repeat 1000
	unless (and (if (or (>= x 0) (< y 0))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (or (not (logbitp i x))
					 (logbitp i y))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest logorc1.order.1
  (let ((i 0) a b)
    (values
     (logorc1 (progn (setf a (incf i)) -3)
	      (progn (setf b (incf i)) 17))
     i a b))
  19 2 1 2)
