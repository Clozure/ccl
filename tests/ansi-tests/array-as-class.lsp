;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 07:45:25 2003
;;;; Contains: Tests for ARRAY as a class

(in-package :cl-test)

(deftest array-as-class.1
  (notnot-mv (typep #() (find-class 'array)))
  t)
  
(deftest array-as-class.2
  (notnot-mv (typep #(a b c) (find-class 'array)))
  t)
  
(deftest array-as-class.3
  (notnot-mv (typep #0aNIL (find-class 'array)))
  t)
  
(deftest array-as-class.4
  (notnot-mv (typep #2a((a b)(c d)) (find-class 'array)))
  t)
  
(deftest array-as-class.5
  (notnot-mv (typep "abcde" (find-class 'array)))
  t)
  
(deftest array-as-class.6
  (notnot-mv (typep #*0011101 (find-class 'array)))
  t)

(deftest array-as-class.7
  (subtypep* 'array (find-class 'array))
  t t)

(deftest array-as-class.8
  (subtypep* (find-class 'array) 'array)
  t t)

(deftest array-as-class.9
  (typep nil (find-class 'array))
  nil)

(deftest array-as-class.10
  (typep 'x (find-class 'array))
  nil)

(deftest array-as-class.11
  (typep '(a b c) (find-class 'array))
  nil)

(deftest array-as-class.12
  (typep 10.0 (find-class 'array))
  nil)

(deftest array-as-class.13
  (typep #'(lambda (x) (cons x nil)) (find-class 'array))
  nil)

(deftest array-as-class.14
  (typep 1 (find-class 'array))
  nil)

(deftest array-as-class.15
  (typep (1+ most-positive-fixnum) (find-class 'array))
  nil)
