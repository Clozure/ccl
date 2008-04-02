;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 13:12:07 2003
;;;; Contains: Tests for type SIMPLE-BIT-VECTOR

(in-package :cl-test)

(deftest simple-bit-vector.2
  (notnot-mv (typep #* 'simple-bit-vector))
  t)
  
(deftest simple-bit-vector.3
  (notnot-mv (typep #*00101 'simple-bit-vector))
  t)

(deftest simple-bit-vector.4
  (typep #(0 1 1 1 0 0) 'simple-bit-vector)
  nil)

(deftest simple-bit-vector.5
  (typep "011100" 'simple-bit-vector)
  nil)

(deftest simple-bit-vector.6
  (typep 0 'simple-bit-vector)
  nil)

(deftest simple-bit-vector.7
  (typep 1 'simple-bit-vector)
  nil)

(deftest simple-bit-vector.8
  (typep nil 'simple-bit-vector)
  nil)

(deftest simple-bit-vector.9
  (typep 'x 'simple-bit-vector)
  nil)

(deftest simple-bit-vector.10
  (typep '(0 1 1 0) 'simple-bit-vector)
  nil)

(deftest simple-bit-vector.11
  (typep (make-array '(2 2) :element-type 'bit
		     :initial-element 0)
	 'simple-bit-vector)
  nil)

(deftest simple-bit-vector.12
  (notnot-mv (typep #* '(simple-bit-vector *)))
  t)

(deftest simple-bit-vector.13
  (notnot-mv (typep #*01101 '(simple-bit-vector *)))
  t)

(deftest simple-bit-vector.14
  (notnot-mv (typep #* '(simple-bit-vector 0)))
  t)

(deftest simple-bit-vector.15
  (typep #*01101 '(simple-bit-vector 0))
  nil)

(deftest simple-bit-vector.16
  (typep #* '(simple-bit-vector 5))
  nil)

(deftest simple-bit-vector.17
  (notnot-mv (typep #*01101 '(simple-bit-vector 5)))
  t)
