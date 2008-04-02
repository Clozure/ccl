;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 13:17:05 2003
;;;; Contains: Tests for VECTORP

(in-package :cl-test)

(deftest vectorp.1
  (vectorp 1)
  nil)

(deftest vectorp.2
  (vectorp (1+ most-positive-fixnum))
  nil)

(deftest vectorp.3
  (vectorp #\a)
  nil)

(deftest vectorp.4
  (vectorp 10.0)
  nil)

(deftest vectorp.5
  (vectorp #'(lambda (x y) (cons y x)))
  nil)

(deftest vectorp.6
  (vectorp '(a b))
  nil)

(deftest vectorp.7
  (vectorp #0aT)
  nil)

(deftest vectorp.8
  (vectorp #2a((a b)(c d)))
  nil)

(deftest vectorp.9
  (notnot-mv (vectorp "abcd"))
  t)

(deftest vectorp.10
  (notnot-mv (vectorp #*))
  t)

(deftest vectorp.11
  (notnot-mv (vectorp #*1101))
  t)

(deftest vectorp.12
  (notnot-mv (vectorp ""))
  t)

(deftest vectorp.13
  (notnot-mv (vectorp #(1 2 3)))
  t)

(deftest vectorp.14
  (notnot-mv (vectorp #()))
  t)

(deftest vectorp.15
  (vectorp #b11010)
  nil)

;;; Error tests

(deftest vectorp.error.1
  (signals-error (vectorp) program-error)
  t)

(deftest vectorp.error.2
  (signals-error (vectorp #() #()) program-error)
  t)






