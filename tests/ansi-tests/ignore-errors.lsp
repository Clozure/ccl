;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar  2 20:38:25 2003
;;;; Contains: Tests of IGNORE-ERRORS

(in-package :cl-test)

(deftest ignore-errors.1
  (ignore-errors)
  nil)

(deftest ignore-errors.2
  (ignore-errors 'a)
  a)

(deftest ignore-errors.3
  (ignore-errors (values 1 2 3 4 5 6 7 8))
  1 2 3 4 5 6 7 8)

(deftest ignore-errors.4
  (multiple-value-bind (val cond)
      (ignore-errors (error "foo"))
    (and (null val)
	 (typep cond 'simple-error)
	 t))
  t)

(deftest ignore-errors.5
  (handler-case
   (ignore-errors (signal "foo"))
   (condition () 'good))
  good)

(deftest ignore-errors.6
  (handler-case
   (ignore-errors (signal "foo"))
   (simple-condition () 'good))
  good)
