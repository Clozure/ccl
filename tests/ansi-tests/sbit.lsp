;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 15:30:31 2003
;;;; Contains: Tests for SBIT

(in-package :cl-test)

(deftest sbit.1
  (sbit #*0010 2)
  1)

(deftest sbit.2
  (let ((a #*00000000))
    (loop for i from 0 below (length a)
	  collect (let ((b (copy-seq a)))
		    (setf (sbit b i) 1)
		    b)))
  (#*10000000
   #*01000000
   #*00100000
   #*00010000
   #*00001000
   #*00000100
   #*00000010
   #*00000001))

(deftest sbit.3
  (let ((a #*11111111))
    (loop for i from 0 below (length a)
	  collect (let ((b (copy-seq a)))
		    (setf (sbit b i) 0)
		    b)))
  (#*01111111
   #*10111111
   #*11011111
   #*11101111
   #*11110111
   #*11111011
   #*11111101
   #*11111110))

(deftest sbit.4
  (let ((a (make-array nil :element-type 'bit :initial-element 0)))
    (values
     (aref a)
     (sbit a)
     (setf (sbit a) 1)
     (aref a)
     (sbit a)))
  0 0 1 1 1)

(deftest sbit.5
  (let ((a (make-array '(1 1) :element-type 'bit :initial-element 0)))
    (values
     (aref a 0 0)
     (sbit a 0 0)
     (setf (sbit a 0 0) 1)
     (aref a 0 0)
     (sbit a 0 0)))
  0 0 1 1 1)

(deftest sbit.6
  (let ((a (make-array '(10 10) :element-type 'bit :initial-element 0)))
    (values
     (aref a 5 5)
     (sbit a 5 5)
     (setf (sbit a 5 5) 1)
     (aref a 5 5)
     (sbit a 5 5)))
  0 0 1 1 1)

(deftest sbit.order.1
  (let ((i 0) a b)
    (values
     (sbit (progn (setf a (incf i)) #*001001)
	   (progn (setf b (incf i)) 1))
     i a b))
  0 2 1 2)

(deftest sbit.order.2
  (let ((i 0) a b c
	(v (copy-seq #*001001)))
    (values
     (setf (sbit (progn (setf a (incf i)) v)
		 (progn (setf b (incf i)) 1))
	   (progn (setf c (incf i)) 1))
     v i a b c))
  1 #*011001 3 1 2 3)

(deftest sbit.error.1
  (signals-error (sbit) program-error)
  t)

  
  