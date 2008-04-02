;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 22:08:21 2003
;;;; Contains: Tests of ARRAYP

(in-package :cl-test)

;;; Also tested by make-array.lsp

(deftest arrayp.1
  (notnot-mv (arrayp #(a b c)))
  t)

(deftest arrayp.2
  (notnot-mv (arrayp "abcd"))
  t)

(deftest arrayp.3
  (notnot-mv (arrayp #*001110101))
  t)

(deftest arrayp.4
  (notnot-mv (arrayp #0aNIL))
  t)

(deftest arrayp.5
  (notnot-mv (arrayp #2a((1 2 3)(4 5 6))))
  t)

(deftest arrayp.6
  (check-type-predicate #'arrayp 'array)
  nil)

(deftest arrayp.7
  (macrolet ((%m (z) z)) (arrayp (expand-in-current-env (%m 0))))
  nil)

(deftest arrayp.order.1
  (let ((i 0) a)
    (values
     (arrayp (progn (setf a (incf i)) nil))
     i a))
  nil 1 1)

;;; Error tests

(deftest arrayp.error.1
  (signals-error (arrayp) program-error)
  t)

(deftest arrayp.error.2
  (signals-error (arrayp #(a b c) nil) program-error)
  t)
