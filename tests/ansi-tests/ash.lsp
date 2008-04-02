;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 08:43:03 2003
;;;; Contains: Tests of ASH

(in-package :cl-test)

;;; Error tests

(deftest ash.error.1
  (signals-error (ash) program-error)
  t)

(deftest ash.error.2
  (signals-error (ash 1 1 1) program-error)
  t)

(deftest ash.error.3
  (signals-error (ash 1 1 nil) program-error)
  t)

(deftest ash.error.4
  (check-type-error #'(lambda (x) (ash x 0)) #'integerp)
  nil)

(deftest ash.error.5
  (check-type-error #'(lambda (x) (ash 0 x)) #'integerp)
  nil)

;;; Non-error tests

(deftest ash.1
  (loop for x in *integers*
	always (eql (ash x 0) x))
  t)

(deftest ash.2
  (loop for i = (random-fixnum)
	for s = (random-from-interval 40)
	for ishifted = (ash i s)
	repeat 1000
	always (eql (floor (* i (expt 2 s))) ishifted))
  t)

(deftest ash.3
  (let* ((nbits 100)
	 (bound (expt 2 nbits)))
    (loop for i = (random-from-interval bound)
	  for s = (random-from-interval (+ nbits 20))
	  for ishifted = (ash i s)
	  repeat 1000
	  always (eql (floor (* i (expt 2 s))) ishifted)))
  t)

(deftest ash.4
  (loop for i from -1 downto -1000
	always (eql (ash i i) -1))
  t)

(deftest ash.5
  (loop for i from 1 to 100
	for j = (- (ash 1 i))
	always (eql (ash j j) -1))
  t)

(deftest ash.6
  (macrolet
   ((%m (z) z))
   (values
    (ash (expand-in-current-env (%m 3)) 1)
    (ash 1 (expand-in-current-env (%m 3)))))
  6 8)

(deftest ash.order.1
  (let ((i 0) x y)
    (values (ash (progn (setf x (incf i)) 1)
		 (progn (setf y (incf i)) 2))
	    i x y))
  4 2 1 2)
