;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 18:20:36 2003
;;;; Contains: Tests of NUMBERP

(in-package :cl-test)

(deftest numberp.error.1
  (signals-error (numberp) program-error)
  t)

(deftest numberp.error.2
  (signals-error (numberp 0 nil) program-error)
  t)

(deftest numberp.error.3
  (signals-error (numberp 'a nil nil) program-error)
  t)

(deftest numberp.1
  (check-type-predicate #'numberp 'number)
  nil)
