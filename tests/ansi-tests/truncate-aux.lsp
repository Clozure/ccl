;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 20 05:15:17 2003
;;;; Contains: Aux. functions associated with tests of TRUNCATE

(in-package :cl-test)

(defun truncate.1-fn ()
  (loop for n = (- (random 2000000000)
		   1000000000)
	for d = (1+ (random 10000))
	for vals = (multiple-value-list (truncate n d))
	for (q r) = vals
	for n2 = (+ (* q d) r)
	repeat 1000
	unless (and (eql (length vals) 2)
		    (integerp q)
		    (= n n2)
		    (integerp r)
		    (if (>= n 0) (< -1 r d)
		      (< (- d) r 1)))
	collect (list n d q r n2)))

(defun truncate.2-fn ()
  (loop for num = (random 1000000000)
	for denom = (1+ (random 1000))
	for n = (/ num denom)
	for d = (1+ (random 10000))
	for vals = (multiple-value-list (truncate n d))
	for (q r) = vals
	for n2 = (+ (* q d) r)
	repeat 1000
	unless (and (eql (length vals) 2)
		    (integerp q)
		    (<= 0 r)
		    (< r d)
		    (= n n2))
	collect (list n d q r n2)))

(defun truncate.3-fn (width)
  (loop for n = (- (random width) (/ width 2))
	for vals = (multiple-value-list (truncate n))
	for (q r) = vals
	for n2 = (+ q r)
	repeat 1000
	unless (and (eql (length vals) 2)
		    (integerp q)
		    (= n n2)
		    (if (>= n 0)
			(and (<= 0 r) (< r 1))
		      (and (< -1 r) (<= r 0)))
		    )
	collect (list n q r n2)))

(defun truncate.7-fn ()
  (loop for numerator = (- (random 10000000000) 5000000000)
	for denominator = (1+ (random 100000))
	for n = (/ numerator denominator)
	for vals = (multiple-value-list (truncate n))
	for (q r) = vals
	for n2 = (+ q r)
	repeat 1000
	unless (and (eql (length vals) 2)
		    (integerp q)
		    (rationalp r)
		    (= n n2)
		    (if (>= n 0)
			(and (<= 0 r) (< r 1))
		      (and (< -1 r) (<= r 0)))
		    )
	collect (list n q r n2)))

(defun truncate.8-fn ()
  (loop for num1 = (- (random 10000000000) 5000000000)
	for den1 = (1+ (random 100000))
	for n = (/ num1 den1)
	for num2 = (- (1+ (random 1000000)))
	for den2 = (1+ (random 1000000))
	for d = (/ num2 den2)
	for vals = (multiple-value-list (truncate n d))
	for (q r) = vals
	for n2 = (+ (* q d) r)
	repeat 1000
	unless (and (eql (length vals) 2)
		    (integerp q)
		    (rationalp r)
		    (if (> n 0)
			(and (<= 0 r) (< r (- d)))
		      (and (>= 0 r) (> r d)))
		    (= n n2))
	collect (list n q d r n2)))

(defun truncate.9-fn ()
  (loop for num1 = (- (random 1000000000000000) 500000000000000)
	for den1 = (1+ (random 10000000000))
	for n = (/ num1 den1)
	for num2 = (- (1+ (random 1000000000)))
	for den2 = (1+ (random 10000000))
	for d = (/ num2 den2)
	for vals = (multiple-value-list (truncate n d))
	for (q r) = vals
	for n2 = (+ (* q d) r)
	repeat 1000
	unless (and (eql (length vals) 2)
		    (integerp q)
		    (rationalp r)
		    (if (> n 0)
			(and (<= 0 r) (< r (- d)))
		      (and (>= 0 r) (> r d)))
		    (= n n2))
	collect (list n q d r n2)))


