;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 21:27:13 2003
;;;; Contains: Tests of MASK-FIELD

(in-package :cl-test)

;;; Error tests

(deftest mask-field.error.1
  (signals-error (mask-field) program-error)
  t)

(deftest mask-field.error.2
  (signals-error (mask-field (byte 1 1)) program-error)
  t)

(deftest mask-field.error.3
  (signals-error (mask-field (byte 1 1) -1 0) program-error)
  t)

;;; Non-error tests

(deftest mask-field.1
  (loop for x = (random-fixnum)
	for pos = (random 30)
	for size = (random 30)
	repeat 10000
	unless (eql (mask-field (byte size pos) x)
		    (logand (ash (1- (ash 1 size)) pos) x))
	collect (list x pos size))
  nil)


(deftest mask-field.2
  (let ((bound (ash 1 300)))
    (loop for x = (random-from-interval bound)
	  for pos = (random 300)
	  for size = (random 300)
	  repeat 1000
	  unless (eql (mask-field (byte size pos) x)
		      (logand (ash (1- (ash 1 size)) pos) x))
	  collect (list x pos size)))
  nil)

(deftest mask-field.3
  (loop for i of-type fixnum from -1000 to 1000
	always (eql (mask-field (byte 0 0) i) 0))
  t)

(deftest mask-field.order.1
  (let ((i 0) a b c d)
    (values
     (mask-field (progn (setf a (incf i))
		 (byte (progn (setf b (incf i)) 3)
		       (progn (setf c (incf i)) 1)))
	  (progn (setf d (incf i)) -1))
     i a b c d))
  14 4 1 2 3 4)

;;; mask-field on places

(deftest mask-field.place.1
  (let ((x 0))
    (values
     (setf (mask-field (byte 4 1) x) -1)
     x))
  -1 30)

(deftest mask-field.place.2
  (loop for pos from 0 to 100
	always
	(loop for size from 0 to 100
	      always
	      (let ((x 0)
		    (field (ash 1 pos)))
		(and (eql (setf (mask-field (byte size pos) x) field) field)
		     (if (> size 0) (eql x field) (eql x 0))
		     ))))
  t)

(deftest mask-field.place.order.1
  (let ((i 0) a b c d e f (x (copy-seq #(63))))
    (values
     (setf (mask-field (progn (setf a (incf i))
		       (byte (progn (setf b (incf i)) 3)
			     (progn (setf c (incf i)) 1)))
		(aref (progn (setf d (incf i)) x)
		      (progn (setf e (incf i)) 0)))
	   (progn (setf f (incf i)) (lognot 14)))
     x
     i a b c d e f))
  -15 #(49) 6 1 2 3 4 5 6)
