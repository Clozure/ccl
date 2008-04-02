;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep  4 20:50:54 2003
;;;; Contains: Tests of DECF

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest decf.1
  (let ((x 12))
    (values
     (decf x)
     x))
  11 11)

(deftest decf.2
  (let ((x 3.0s0))
    (values
     (decf x)
     x))
  2.0s0 2.0s0)

(deftest decf.3
  (let ((x 19.0f0))
    (values
     (decf x)
     x))
  18.0f0 18.0f0)

(deftest decf.4
  (let ((x 813.0d0))
    (values
     (decf x)
     x))
  812.0d0 812.0d0)

(deftest decf.5
  (let ((x -17.0l0))
    (values
     (decf x)
     x))
  -18.0l0 -18.0l0)

(deftest decf.6
  (loop for x from 1 to 5
	collect (let ((y x))
		  (list (decf y) y)))
  ((0 0) (1 1) (2 2) (3 3) (4 4)))

(deftest decf.7
  (loop for x in '(3.0s0 3.0f0 3.0d0 3.0l0)
	collect (let ((y x))
		  (list (decf y) y)))
  ((2.0s0 2.0s0) (2.0f0 2.0f0) (2.0d0 2.0d0) (2.0l0 2.0l0)))

(deftest decf.8
  (loop for x in '(3.0s0 3.0f0 3.0d0 3.0f0)
	for y = (complex x 0)
	for z = (decf y)
	for x1c = (complex (1- x) 0)
	unless (and (eql y z) (eql x1c y))
	collect (list x y z x1c))
  nil)

(deftest decf.9
  (let ((x most-negative-fixnum))
    (values (decf x) x))
  #.(1- most-negative-fixnum) #.(1- most-negative-fixnum))

(deftest decf.10
  (let ((x (1- most-negative-fixnum)))
    (values (decf x) x))
  #.(- most-negative-fixnum 2) #.(- most-negative-fixnum 2))

(deftest decf.11
  (loop for x in *numbers*
	unless (let* ((y x)
		      (z (decf y)))
		 (and (eql y (1- x))
		      (eql y z)))
	collect x)
  nil)

;;; Increment by other than 1

(deftest decf.12
  (loop for x in *numbers*
	unless (let* ((y x) (z (decf y 0)))
		 (and (eql x y) (eql y z)))
	collect x)
  nil)

(deftest decf.13
  (loop for x in *numbers*
	nconc
	(loop for r = (random-from-interval 1000000)
	      repeat 100
	      when (let* ((y x) (z (decf y r)))
			(and (not (and (eql (- x r) y) (eql y z)))
			     (list x y r)))
	      collect it))
  nil)

(deftest decf.14
  (let ((x 1))
    (values (decf x 0.0s0) x))
  1.0s0 1.0s0)

(deftest decf.15
  (let ((x 1))
    (values (decf x 0.0f0) x))
  1.0f0 1.0f0)

(deftest decf.16
  (let ((x 2))
    (values (decf x 0.0d0) x))
  2.0d0 2.0d0)

(deftest decf.17
  (let ((x 10))
    (values (decf x 0.0l0) x))
  10.0l0 10.0l0)

(deftest decf.18
  (let ((x 1))
    (values (decf x #c(0.0s0 10.0s0)) x))
  #c(1.0s0 -10.0s0) #c(1.0s0 -10.0s0))

(deftest decf.19
  (let ((x 1))
    (values (decf x #c(0.0f0 2.0f0)) x))
  #c(1.0f0 -2.0f0) #c(1.0f0 -2.0f0))

(deftest decf.20
  (let ((x 1))
    (values (decf x #c(0.0d0 2.0d0)) x))
  #c(1.0d0 -2.0d0) #c(1.0d0 -2.0d0))

(deftest decf.21
  (let ((x 1))
    (values (decf x #c(0.0l0 -2.0l0)) x))
  #c(1.0l0 2.0l0) #c(1.0l0 2.0l0))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest decf.22
  (macrolet
   ((%m (z) z))
   (let ((x 10))
     (values
      (decf (expand-in-current-env (%m x)))
      x)))
  9 9)

(deftest decf.23
  (macrolet
   ((%m (z) z))
   (let ((x 5))
     (values
      (decf x (expand-in-current-env (%m 3)))
      x)))
  2 2)

(deftest decf.order.2
  (let ((a (vector 1 2 3 4))
	(i 0) x y z)
    (values
     (decf (aref (progn (setf x (incf i)) a)
		 (progn (setf y (incf i)) 0))
	   (progn (setf z (incf i)) 17))
     i x y z a))
  -16 3 1 2 3 #(-16 2 3 4))

(deftest decf.order.3
  (let ((a (vector 10 2 3 4))
	(i 0) x y)
    (values
     (decf (aref (progn (setf x (incf i)) a)
		 (progn (setf y (incf i)) 0)))
     i x y a))
  9 2 1 2 #(9 2 3 4))

(deftest decf.order.4
  (let ((x 0))
    (progn
      "See CLtS 5.1.3"
      (values
       (decf x (setf x 1))
       x)))
  0 0)
