;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 06:27:45 2003
;;;; Contains: Tests of LOGORC2

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest logorc2.error.1
  (check-type-error #'(lambda (x) (logorc2 x 0)) #'integerp)
  nil)

(deftest logorc2.error.2
  (check-type-error #'(lambda (x) (logorc2 0 x)) #'integerp)
  nil)

(deftest logorc2.error.3
  (signals-error (logorc2) program-error)
  t)

(deftest logorc2.error.4
  (signals-error (logorc2 0) program-error)
  t)

(deftest logorc2.error.5
  (signals-error (logorc2 1 2 3) program-error)
  t)

;;; Non-error tests

(deftest logorc2.1
  (logorc2 0 0)
  -1)

(deftest logorc2.2
  (logorc2 -1 0)
  -1)

(deftest logorc2.2a
  (logorc2 0 -1)
  0)

(deftest logorc2.3
  (logorc2 0 123)
  -124)

(deftest logorc2.4
  (loop for x in *integers*
	always (and (eql -1 (logorc2 x 0))
		    (eql x (logorc2 x -1))
		    (eql -1 (logorc2 x x))
		    (eql x (logorc2 x (lognot x)))
		    (eql (lognot x) (logorc2 (lognot x) x))))
  t)

(deftest logorc2.5
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql x (logorc2 x xc))
	collect x)
  nil)

(deftest logorc2.6
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (logorc2 x y)
	repeat 1000
	unless (and (if (or (< x 0) (>= y 0))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (or (not (logbitp i y))
					 (logbitp i x))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest logorc2.order.1
  (let ((i 0) a b)
    (values
     (logorc2 (progn (setf a (incf i)) 27)
	      (progn (setf b (incf i)) -1))
     i a b))
  27 2 1 2)

