;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 06:30:57 2003
;;;; Contains: Tests of LOGXOR

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest logxor.error.1
  (check-type-error #'logxor #'integerp)
  nil)

(deftest logxor.error.2
  (check-type-error #'(lambda (x) (logxor 0 x)) #'integerp)
  nil)


;;; Non-error tests

(deftest logxor.1
  (logxor)
  0)

(deftest logxor.2
  (logxor 1231)
  1231)

(deftest logxor.3
  (logxor -198)
  -198)

(deftest logxor.4
  (loop for x in *integers*
	always (eql x (logxor x)))
  t)

(deftest logxor.5
  (loop for x in *integers*
	always (and (eql -1 (logxor x (lognot x)))
		    (eql 0 (logxor x x))
		    (eql x (logxor x x x))))
  t)

(deftest logxor.6
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql -1 (logxor x xc))
	collect x)
  nil)

(deftest logxor.7
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (logxor x y)
	repeat 1000
	unless (and (if (or (and (< x 0) (>= y 0))
			    (and (>= x 0) (< y 0)))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (or (and (logbitp i x)
					      (not (logbitp i y)))
					 (and (not (logbitp i x))
					      (logbitp i y)))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest logxor.8
  (loop for i from 1 to (min 256 (1- call-arguments-limit))
	for args = (nconc (make-list (1- i) :initial-element 0)
			  (list 7131))
	always (eql (apply #'logxor args) 7131))
  t)

(deftest logxor.order.1
  (let ((i 0) a b)
    (values
     (logxor (progn (setf a (incf i)) #b11011)
	     (progn (setf b (incf i)) #b10110))
     i a b))
  #b1101 2 1 2)
  

(deftest logxor.order.2
  (let ((i 0) a b c)
    (values
     (logxor (progn (setf a (incf i))  #b11011)
	     (progn (setf b (incf i))  #b10110)
	     (progn (setf c (incf i)) #b110101))
     i a b c))
  #b111000 3 1 2 3)


