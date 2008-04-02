;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep  4 20:01:15 2003
;;;; Contains: Tests of INCF

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest incf.1
  (let ((x 12))
    (values
     (incf x)
     x))
  13 13)

(deftest incf.2
  (let ((x 3.0s0))
    (values
     (incf x)
     x))
  4.0s0 4.0s0)

(deftest incf.3
  (let ((x 19.0f0))
    (values
     (incf x)
     x))
  20.0f0 20.0f0)

(deftest incf.4
  (let ((x 813.0d0))
    (values
     (incf x)
     x))
  814.0d0 814.0d0)

(deftest incf.5
  (let ((x -17.0l0))
    (values
     (incf x)
     x))
  -16.0l0 -16.0l0)

(deftest incf.6
  (loop for x from 1 to 5
	collect (let ((y x))
		  (list (incf y) y)))
  ((2 2) (3 3) (4 4) (5 5) (6 6)))

(deftest incf.7
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	collect (let ((y x))
		  (list (incf y) y)))
  ((2.0s0 2.0s0) (2.0f0 2.0f0) (2.0d0 2.0d0) (2.0l0 2.0l0)))

(deftest incf.8
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0f0)
	for y = (complex x 0)
	for z = (incf y)
	for x1c = (complex (1+ x) 0)
	unless (and (eql y z) (eql x1c y))
	collect (list x y z x1c))
  nil)

(deftest incf.9
  (let ((x most-positive-fixnum))
    (values (incf x) x))
  #.(1+ most-positive-fixnum) #.(1+ most-positive-fixnum))

(deftest incf.10
  (let ((x (1+ most-positive-fixnum)))
    (values (incf x) x))
  #.(+ 2 most-positive-fixnum) #.(+ 2 most-positive-fixnum))

(deftest incf.11
  (loop for x in *numbers*
	unless (let* ((y x)
		      (z (incf y)))
		 (and (eql y (1+ x))
		      (eql y z)))
	collect x)
  nil)

;;; Increment by other than 1

(deftest incf.12
  (loop for x in *numbers*
	unless (let* ((y x) (z (incf y 0)))
		 (and (eql x y) (eql y z)))
	collect x)
  nil)

(deftest incf.13
  (loop for x in *numbers*
	nconc
	(loop for r = (random-from-interval 1000000)
	      repeat 100
	      when (let* ((y x) (z (incf y r)))
			(and (not (and (eql (+ x r) y) (eql y z)))
			     (list x y r)))
	      collect it))
  nil)

(deftest incf.14
  (let ((x 1))
    (values (incf x 0.0s0) x))
  1.0s0 1.0s0)

(deftest incf.15
  (let ((x 1))
    (values (incf x 0.0f0) x))
  1.0f0 1.0f0)

(deftest incf.16
  (let ((x 2))
    (values (incf x 0.0d0) x))
  2.0d0 2.0d0)

(deftest incf.17
  (let ((x 10))
    (values (incf x 0.0l0) x))
  10.0l0 10.0l0)

(deftest incf.18
  (let ((x 1))
    (values (incf x #c(0.0s0 0.0s0)) x))
  #c(1.0s0 0.0s0) #c(1.0s0 0.0s0))

(deftest incf.19
  (let ((x 1))
    (values (incf x #c(0.0f0 2.0f0)) x))
  #c(1.0f0 2.0f0) #c(1.0f0 2.0f0))

(deftest incf.20
  (let ((x 1))
    (values (incf x #c(0.0d0 2.0d0)) x))
  #c(1.0d0 2.0d0) #c(1.0d0 2.0d0))

(deftest incf.21
  (let ((x 1))
    (values (incf x #c(0.0l0 -2.0l0)) x))
  #c(1.0l0 -2.0l0) #c(1.0l0 -2.0l0))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest incf.22
  (macrolet
   ((%m (z) z))
   (let ((x 2))
     (values
      (incf (expand-in-current-env (%m x)))
      x)))
  3 3)

(deftest incf.23
  (macrolet
   ((%m (z) z))
   (let ((x 2))
     (values
      (incf x (expand-in-current-env (%m 4)))
      x)))
  6 6)

(deftest incf.order.2
  (let ((a (vector 1 2 3 4))
	(i 0) x y z)
    (values
     (incf (aref (progn (setf x (incf i)) a)
		 (progn (setf y (incf i)) 0))
	   (progn (setf z (incf i)) 17))
     i x y z a))
  18 3 1 2 3 #(18 2 3 4))

(deftest incf.order.3
  (let ((a (vector 10 2 3 4))
	(i 0) x y)
    (values
     (incf (aref (progn (setf x (incf i)) a)
		 (progn (setf y (incf i)) 0)))
     i x y a))
  11 2 1 2 #(11 2 3 4))

(deftest incf.order.4
  (let ((x 0))
    (progn
      "See CLtS 5.1.3"
      (values
       (incf x (setf x 1))
       x)))
  2 2)

