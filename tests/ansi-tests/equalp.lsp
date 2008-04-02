;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 22:14:42 2002
;;;; Contains: Tests for EQUALP

(in-package :cl-test)

(compile-and-load "random-aux.lsp")

(deftest equalp.1
  (loop for c across +base-chars+
	always (loop for d across +base-chars+
		     always (if (char-equal c d) (equalpt c d)
			      (not (equalpt c d)))))
  t)

(deftest equalp.2
  (loop for i from 1 to 100
	always (loop for j from 1 to 100
		     always (if (eqlt i j) (equalpt i j)
			      (not (equalpt i j)))))
  t)

(deftest equalp.3
  (equalpt "abc" "ABC")
  t)

(deftest equalp.4
  (equalpt "abc" "abd")
  nil)

(deftest equalp.5
  :notes (:allow-nil-arrays)
  (equalpt (make-array '(0) :element-type nil) #())
  t)

(deftest equalp.6
  :notes (:allow-nil-arrays)
  (equalpt (make-array '(0) :element-type nil) "")
  t)

(deftest equalp.7
  (loop for nbits from 1 to 100
	for type = `(unsigned-byte ,nbits)
	for bound = (ash 1 nbits)
	for val = (random bound)
	for a1 = (make-array nil :initial-element val :element-type type)
	for a2 = (make-array nil :initial-element val)
	unless (equalp a1 a2)
	collect (list nbits type val))
  nil)

(deftest equalp.8
  (loop for nbits from 1 to 100
	for type = `(unsigned-byte ,nbits)
	for bound = (ash 1 nbits)
	for n = (1+ (random 20))
	for vals = (loop repeat n collect (random bound))
	for a1 = (make-array n :initial-contents vals :element-type type)
	for a2 = (make-array n :initial-contents vals)
	unless (equalp a1 a2)
	collect (list nbits type vals))
  nil)

(deftest equalp.9
  (loop for nbits from 1 to 100
	for type = `(signed-byte ,nbits)
	for bound = (ash 1 nbits)
	for n = (1+ (random 20))
	for vals = (loop repeat n collect (- (random bound) (/ bound 2)))
	for a1 = (make-array n :initial-contents vals :element-type type)
	for a2 = (make-array n :initial-contents vals)
	unless (equalp a1 a2)
	collect (list nbits type vals))
  nil)

(deftest equalp.10
  (equalpt #*0010 #(0 0 1 0))
  t)

(deftest equalp.11
  (let ((v1 #(1 2 3))
	(v2 (make-array 8 :initial-contents '(1 2 3 4 5 6 7 8)
			:fill-pointer 3)))
    (equalpt v1 v2))
  t)

(deftest equalp.12
  (equalpt '(#\a #\b) "ab")
  nil)

(deftest equalp.13
  (equalpt '(#\a #\b) '(#\A #\B))
  t)

(deftest equalp.14
  (let ((s1 (make-array '(4) :initial-contents '(#\a #\b #\c #\d)
			:element-type 'base-char))
	(s2 (make-array '(4) :initial-contents '(#\a #\b #\c #\d)
			:element-type 'character)))
    (equalpt s1 s2))
  t)

(deftest equalp.15
  (let ((bv (make-array '(4) :initial-contents '(0 0 1 0)
			:element-type 'bit))
	(v #(0 0 1 0)))
    (equalpt bv v))
  t)

(defstruct equalp-struct-16
  a b c)

(defstruct equalp-struct-16-alt
  a b c)

(deftest equalp.16
  (let ((s1 (make-equalp-struct-16 :a 1 :b 2 :c #\a))
	(s2 (make-equalp-struct-16 :a 1.0 :b 2.0 :c #\A))
	(s3 (make-equalp-struct-16-alt :a 1.0 :b 2.0 :c #\A)))
    (values (equalpt s1 s2)
	    (equalpt s1 s3)
	    (equalpt s2 s3)))
  t nil nil)

(deftest equalp.17
  (loop for i below 8192
	for f = (float i 1.0s0)
	repeat 1000
	unless (equalp i f)
	collect (list i f))
  nil)

(deftest equalp.18
  (loop for i = (- (random 10000000) 5000000)
	for f = (float i 1.0f0)
	repeat 1000
	unless (equalp i f)
	collect (list i f))
  nil)

(deftest equalp.19
  (loop for i = (- (random 10000000) 5000000)
	for f = (float i 1.0d0)
	repeat 1000
	unless (equalp i f)
	collect (list i f))
  nil)

(deftest equalp.20
  (loop for i = (- (random 10000000) 5000000)
	for f = (float i 1.0l0)
	repeat 1000
	unless (equalp i f)
	collect (list i f))
  nil)

(deftest equalp.21
  (let ((ht1 (make-hash-table :test #'eq))
	(ht2 (make-hash-table :test #'eql))
	(ht3 (make-hash-table :test #'equal))
	(ht4 (make-hash-table :test #'equalp)))
    (values (equalpt ht1 ht2)
	    (equalpt ht1 ht3)
	    (equalpt ht1 ht4)
	    (equalpt ht2 ht3)
	    (equalpt ht2 ht4)
	    (equalpt ht3 ht4)))
  nil nil nil nil nil nil)

(deftest equalp.22
  (equalpt (make-hash-table :test 'eq)
	   (make-hash-table :test #'eq))
  t)

(deftest equalp.23
  (equalpt (make-hash-table :test 'eql)
	   (make-hash-table :test #'eql))
  t)

(deftest equalp.24
  (equalpt (make-hash-table :test 'equal)
	   (make-hash-table :test #'equal))
  t)

(deftest equalp.25
  (equalpt (make-hash-table :test 'equalp)
	   (make-hash-table :test #'equalp))
  t)

(deftest equalp.26
  (let ((ht1 (make-hash-table :test #'eq))
	(ht2 (make-hash-table :test #'eq)))
    (setf (gethash #\a ht1) t)
    (setf (gethash #\A ht2) t)
    (equalpt ht1 ht2))
  nil)

(deftest equalp.27
  (let ((ht1 (make-hash-table :test #'eq))
	(ht2 (make-hash-table :test #'eq)))
    (setf (gethash 'a ht1) #\a)
    (setf (gethash 'a ht2) #\A)
    (equalpt ht1 ht2))
  t)

(deftest equalp.28
  (let ((ht1 (make-hash-table :test #'eql))
	(ht2 (make-hash-table :test #'eql)))
    (setf (gethash #\a ht1) t)
    (setf (gethash #\A ht2) t)
    (equalpt ht1 ht2))
  nil)

(deftest equalp.29
  (let ((ht1 (make-hash-table :test #'eql))
	(ht2 (make-hash-table :test #'eql)))
    (setf (gethash #\a ht1) "a")
    (setf (gethash #\a ht2) "A")
    (equalpt ht1 ht2))
  t)

(deftest equalp.30
  (let ((ht1 (make-hash-table :test #'equal))
	(ht2 (make-hash-table :test #'equal)))
    (setf (gethash #\a ht1) t)
    (setf (gethash #\A ht2) t)
    (equalpt ht1 ht2))
  nil)

(deftest equalp.31
  (let ((ht1 (make-hash-table :test #'equal))
	(ht2 (make-hash-table :test #'equal)))
    (setf (gethash #\a ht1) "a")
    (setf (gethash #\a ht2) "A")
    (equalpt ht1 ht2))
  t)

(deftest equalp.32
  (let ((ht1 (make-hash-table :test #'equalp))
	(ht2 (make-hash-table :test #'equalp)))
    (setf (gethash #\a ht1) t)
    (setf (gethash #\A ht2) t)
    (equalpt ht1 ht2))
  t)

(deftest equalp.33
  (let ((ht1 (make-hash-table :test #'equalp))
	(ht2 (make-hash-table :test #'equalp)))
    (setf (gethash #\a ht1) "a")
    (setf (gethash #\a ht2) "A")
    (equalpt ht1 ht2))
  t)

(deftest equalp.34
  (let ((ht1 (make-hash-table :test #'equalp))
	(ht2 (make-hash-table :test #'equalp)))
    (setf (gethash '#:a ht1) t)
    (setf (gethash '#:a ht2) t)
    (equalpt ht1 ht2))
  nil)

(deftest equalp.35
  (loop for test in '(eq eql equal equalp)
	collect
	(flet ((%make-table
		()
		(apply #'make-hash-table
		       :test test
		       `(,@(when (coin)
			     (list :size (random 100)))
			   ,@(when (coin)
			       (list :rehash-size (1+ (random 50))))
			   ,@(when (coin)
			       (list :rehash-threshold (random 1.0)) )))))
	  (loop repeat 200
		count
		(let ((ht1 (%make-table))
		      (ht2 (%make-table))
		      (pairs (loop for i below (random 100) collect (cons (gensym) i))))
		  (loop for (k . v) in pairs do (setf (gethash k ht1) v))
		  (setf pairs (random-permute pairs))
		  (loop for (k . v) in pairs do (setf (gethash k ht2) v))
		  (not (equalp ht1 ht2))))))
  (0 0 0 0))

(deftest equalp.order.1
  (let ((i 0) x y)
    (values
     (equalp (setf x (incf i)) (setf y (incf i)))
     i x y))
  nil 2 1 2)

;;; Error tests

(deftest equalp.error.1
  (signals-error (equalp) program-error)
  t)

(deftest equalp.error.2
  (signals-error (equalp nil) program-error)
  t)

(deftest equalp.error.3
  (signals-error (equalp nil nil nil) program-error)
  t)
