;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 23:21:11 2002
;;;; Contains: Tests for IDENTITY

(in-package :cl-test)

(deftest identity.1
  (check-predicate #'(lambda (x) (eqlt x (check-values (identity x)))))
  nil)

(deftest identity.2
  (let ((x (ash 1 100)))
    (eqlt x (check-values (identity x))))
  t)

(deftest identity.3
  (let ((x 1.00000001))
    (eqlt x (check-values (identity x))))
  t)

(deftest identity.order.1
  (let ((i 0))
    (values (identity (incf i)) i))
  1 1)

(deftest identity.error.1
  (signals-error (identity) program-error)
  t)

(deftest identity.error.2
  (signals-error (identity 'a 'a) program-error)
  t)

