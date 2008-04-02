;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 13:22:59 2003
;;;; Contains: Tests for accessor BIT

(in-package :cl-test)

(deftest bit.1
  (bit #*0010 2)
  1)

(deftest bit.2
  (let ((a #*00000000))
    (loop for i from 0 below (length a)
	  collect (let ((b (copy-seq a)))
		    (setf (bit b i) 1)
		    b)))
  (#*10000000
   #*01000000
   #*00100000
   #*00010000
   #*00001000
   #*00000100
   #*00000010
   #*00000001))

(deftest bit.3
  (let ((a #*11111111))
    (loop for i from 0 below (length a)
	  collect (let ((b (copy-seq a)))
		    (setf (bit b i) 0)
		    b)))
  (#*01111111
   #*10111111
   #*11011111
   #*11101111
   #*11110111
   #*11111011
   #*11111101
   #*11111110))

(deftest bit.4
  (let ((a (make-array nil :element-type 'bit :initial-element 0)))
    (values
     (aref a)
     (bit a)
     (setf (bit a) 1)
     (aref a)
     (bit a)))
  0 0 1 1 1)

(deftest bit.5
  (let ((a (make-array '(1 1) :element-type 'bit :initial-element 0)))
    (values
     (aref a 0 0)
     (bit a 0 0)
     (setf (bit a 0 0) 1)
     (aref a 0 0)
     (bit a 0 0)))
  0 0 1 1 1)

(deftest bit.6
  (let ((a (make-array '(10 10) :element-type 'bit :initial-element 0)))
    (values
     (aref a 5 5)
     (bit a 5 5)
     (setf (bit a 5 5) 1)
     (aref a 5 5)
     (bit a 5 5)))
  0 0 1 1 1)

;;; Check that the fill pointer is ignored

(deftest bit.7
  (let ((a (make-array '(10) :initial-contents '(0 1 1 0 0 1 1 1 0 0)
		       :element-type 'bit
		       :fill-pointer 5)))
    (values
     (coerce a 'list)
     (loop for i from 0 below 10 collect (bit a i))
     (loop for i from 0 below 10
	   collect (setf (bit a i) (- 1 (bit a i))))
     (coerce a 'list)
     (loop for i from 0 below 10 collect (bit a i))
     (fill-pointer a)))
  (0 1 1 0 0)
  (0 1 1 0 0 1 1 1 0 0)
  (1 0 0 1 1 0 0 0 1 1)
  (1 0 0 1 1)
  (1 0 0 1 1 0 0 0 1 1)
  5)

;;; Check that adjustability is not relevant

(deftest bit.8
  (let ((a (make-array '(10) :initial-contents '(0 1 1 0 0 1 1 1 0 0)
		       :element-type 'bit
		       :adjustable t)))
    (values
     (coerce a 'list)
     (loop for i from 0 below 10 collect (bit a i))
     (loop for i from 0 below 10
	   collect (setf (bit a i) (- 1 (bit a i))))
     (coerce a 'list)
     (loop for i from 0 below 10 collect (bit a i))))
  (0 1 1 0 0 1 1 1 0 0)
  (0 1 1 0 0 1 1 1 0 0)
  (1 0 0 1 1 0 0 0 1 1)
  (1 0 0 1 1 0 0 0 1 1)
  (1 0 0 1 1 0 0 0 1 1))

;;; Order of evaluation tests

(deftest bit.order.1
  (let ((x 0) y z
	(b (copy-seq #*01010)))
    (values
     (bit (progn (setf y (incf x)) b)
	  (progn (setf z (incf x)) 1))
     x y z))
  1 2 1 2)

(deftest bit.order.2
  (let ((x 0) y z w
	(b (copy-seq #*01010)))
    (values
     (setf (bit (progn (setf y (incf x)) b)
		(progn (setf z (incf x)) 1))
	   (progn (setf w (incf x)) 0))
     b
     x y z w))
  0 #*00010 3 1 2 3)

(deftest bit.error.1
  (signals-error (bit) program-error)
  t)
