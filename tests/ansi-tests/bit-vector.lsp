;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 13:03:22 2003
;;;; Contains: Tests of type BIT-VECTOR

(in-package :cl-test)

(deftest bit-vector.1
  (notnot-mv (find-class 'bit-vector))
  t)

(deftest bit-vector.2
  (notnot-mv (typep #* 'bit-vector))
  t)
  
(deftest bit-vector.3
  (notnot-mv (typep #*00101 'bit-vector))
  t)

(deftest bit-vector.4
  (typep #(0 1 1 1 0 0) 'bit-vector)
  nil)

(deftest bit-vector.5
  (typep "011100" 'bit-vector)
  nil)

(deftest bit-vector.6
  (typep 0 'bit-vector)
  nil)

(deftest bit-vector.7
  (typep 1 'bit-vector)
  nil)

(deftest bit-vector.8
  (typep nil 'bit-vector)
  nil)

(deftest bit-vector.9
  (typep 'x 'bit-vector)
  nil)

(deftest bit-vector.10
  (typep '(0 1 1 0) 'bit-vector)
  nil)

(deftest bit-vector.11
  (typep (make-array '(2 2) :element-type 'bit
		     :initial-element 0)
	 'bit-vector)
  nil)

(deftest bit-vector.12
  (notnot-mv (typep #* '(bit-vector *)))
  t)

(deftest bit-vector.13
  (notnot-mv (typep #*01101 '(bit-vector *)))
  t)

(deftest bit-vector.14
  (notnot-mv (typep #* '(bit-vector 0)))
  t)

(deftest bit-vector.15
  (typep #*01101 '(bit-vector 0))
  nil)

(deftest bit-vector.16
  (typep #* '(bit-vector 5))
  nil)

(deftest bit-vector.17
  (notnot-mv (typep #*01101 '(bit-vector 5)))
  t)


;;; Tests of typep on the class named bit-vector

(deftest bit-vector.class.2
  (notnot-mv (typep #* (find-class 'bit-vector)))
  t)
  
(deftest bit-vector.class.3
  (notnot-mv (typep #*00101 (find-class 'bit-vector)))
  t)

(deftest bit-vector.class.4
  (typep #(0 1 1 1 0 0) (find-class 'bit-vector))
  nil)

(deftest bit-vector.class.5
  (typep "011100" (find-class 'bit-vector))
  nil)

(deftest bit-vector.class.6
  (typep 0 (find-class 'bit-vector))
  nil)

(deftest bit-vector.class.7
  (typep 1 (find-class 'bit-vector))
  nil)

(deftest bit-vector.class.8
  (typep nil (find-class 'bit-vector))
  nil)

(deftest bit-vector.class.9
  (typep 'x (find-class 'bit-vector))
  nil)

(deftest bit-vector.class.10
  (typep '(0 1 1 0) (find-class 'bit-vector))
  nil)

(deftest bit-vector.class.11
  (typep (make-array '(2 2) :element-type 'bit
		     :initial-element 0)
	 (find-class 'bit-vector))
  nil)
