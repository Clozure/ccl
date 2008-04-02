;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 06:08:21 2003
;;;; Contains: Tests of LOGIOR

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest logior.error.1
  (check-type-error #'logior #'integerp)
  nil)

(deftest logior.error.2
  (check-type-error #'(lambda (x) (logior 0 x)) #'integerp)
  nil)

;;; Non-error tests

(deftest logior.1
  (logior)
  0)

(deftest logior.2
  (logior 1231)
  1231)

(deftest logior.3
  (logior -198)
  -198)

(deftest logior.4
  (loop for x in *integers*
	always (eql x (logior x)))
  t)

(deftest logior.5
  (loop for x in *integers*
	always (eql -1 (logior x (lognot x))))
  t)

(deftest logior.6
  (loop for x = (random-fixnum)
	for xc = (lognot x)
	repeat 1000
	unless (eql -1 (logior x xc))
	collect x)
  nil)

(deftest logior.7
  (loop for x = (random-from-interval (ash 1 (random 200)))
	for y = (random-from-interval (ash 1 (random 200)))
	for z = (logior x y)
	repeat 1000
	unless (and (if (or (< x 0) (< y 0))
			(< z 0)
		      (>= z 0))
		    (loop for i from 1 to 210
			  always (if (or (logbitp i x)
					 (logbitp i y))
				     (logbitp i z)
				   (not (logbitp i z)))))
	collect (list x y z))
  nil)

(deftest logior.8
  (loop for i from 1 to (min 256 (1- call-arguments-limit))
	for args = (nconc (make-list (1- i) :initial-element 0)
			  (list -21231))
	always (eql (apply #'logior args) -21231))
  t)

(deftest logior.order.1
  (let ((i 0) a b)
    (values
     (logior (progn (setf a (incf i)) #b11010)
	     (progn (setf b (incf i)) #b10110))
     i a b))
  #b11110 2 1 2)

(deftest logior.order.2
  (let ((i 0) a b c)
    (values
     (logior (progn (setf a (incf i))  #b10011)
	     (progn (setf b (incf i))  #b10110)
	     (progn (setf c (incf i)) #b110101))
     i a b c))
  #b110111 3 1 2 3)


