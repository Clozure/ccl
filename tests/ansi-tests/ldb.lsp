;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 20:45:17 2003
;;;; Contains: Tests of LDB

(in-package :cl-test)

;;; Error tests

(deftest ldb.error.1
  (signals-error (ldb) program-error)
  t)

(deftest ldb.error.2
  (signals-error (ldb (byte 1 1)) program-error)
  t)

(deftest ldb.error.3
  (signals-error (ldb (byte 1 1) -1 0) program-error)
  t)

;;; Non-error tests

(deftest ldb.1
  (loop for x = (random-fixnum)
	for pos = (random 30)
	for size = (random 30)
	repeat 10000
	unless (eql (ldb (byte size pos) x)
		    (logand (1- (ash 1 size))
			    (ash x (- pos))))
	collect (list x pos size))
  nil)


(deftest ldb.2
  (let ((bound (ash 1 300)))
    (loop for x = (random-from-interval bound)
	  for pos = (random 300)
	  for size = (random 300)
	  repeat 1000
	  unless (eql (ldb (byte size pos) x)
		      (logand (1- (ash 1 size))
			      (ash x (- pos))))
	  collect (list x pos size)))
  nil)

(deftest ldb.3
  (loop for i of-type fixnum from -1000 to 1000
	always (eql (ldb (byte 0 0) i) 0))
  t)

(deftest ldb.order.1
  (let ((i 0) a b c d)
    (values
     (ldb (progn (setf a (incf i))
		 (byte (progn (setf b (incf i)) 3)
		       (progn (setf c (incf i)) 1)))
	  (progn (setf d (incf i)) -1))
     i a b c d))
  7 4 1 2 3 4)

;;; ldb on places

(deftest ldb.place.1
  (let ((x 0))
    (values
     (setf (ldb (byte 4 1) x) -1)
     x))
  -1 30)

(deftest ldb.place.2
  (loop for pos from 0 to 100
	always
	(loop for size from 0 to 100
	      always
	      (let ((x 0))
		(and (eql (setf (ldb (byte size pos) x) -1) -1)
		     (eql x (ash (1- (ash 1 size)) pos))))))
  t)

(deftest ldb.place.order.1
  (let ((i 0) a b c d e f (x (copy-seq #(63))))
    (values
     (setf (ldb (progn (setf a (incf i))
		       (byte (progn (setf b (incf i)) 3)
			     (progn (setf c (incf i)) 1)))
		(aref (progn (setf d (incf i)) x)
		      (progn (setf e (incf i)) 0)))
	   (progn (setf f (incf i)) 0))
     x
     i a b c d e f))
  0 #(49) 6 1 2 3 4 5 6)
