;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 20:20:27 2003
;;;; Contains: Tests of SIMPLE-BIT-VECTOR-P

(in-package :cl-test)

(deftest simple-bit-vector-p.2
  (notnot-mv (simple-bit-vector-p #*))
  t)
  
(deftest simple-bit-vector-p.3
  (notnot-mv (simple-bit-vector-p #*00101))
  t)

(deftest simple-bit-vector-p.4
  (simple-bit-vector-p #(0 1 1 1 0 0))
  nil)

(deftest simple-bit-vector-p.5
  (simple-bit-vector-p "011100")
  nil)

(deftest simple-bit-vector-p.6
  (simple-bit-vector-p 0)
  nil)

(deftest simple-bit-vector-p.7
  (simple-bit-vector-p 1)
  nil)

(deftest simple-bit-vector-p.8
  (simple-bit-vector-p nil)
  nil)

(deftest simple-bit-vector-p.9
  (simple-bit-vector-p 'x)
  nil)

(deftest simple-bit-vector-p.10
  (simple-bit-vector-p '(0 1 1 0))
  nil)

(deftest simple-bit-vector-p.11
  (simple-bit-vector-p (make-array '(2 2) :element-type 'bit
				   :initial-element 0))
  nil)

(deftest simple-bit-vector-p.12
  (check-type-predicate #'simple-bit-vector-p 'simple-bit-vector)
  nil)

(deftest simple-bit-vector-p.error.1
  (signals-error (simple-bit-vector-p) program-error)
  t)

(deftest simple-bit-vector-p.error.2
  (signals-error (simple-bit-vector-p #* #*) program-error)
  t)
