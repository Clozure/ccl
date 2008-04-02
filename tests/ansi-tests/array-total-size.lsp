;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 22:01:09 2003
;;;; Contains: Tests of ARRAY-TOTAL-SIZE

(in-package :cl-test)

;;; More tests of ARRAY-TOTAL-SIZE are in make-array.lsp

(deftest array-total-size.1
  (array-total-size #0aNIL)
  1)

(deftest array-total-size.2
  (array-total-size "abcdef")
  6)

(deftest array-total-size.3
  (array-total-size #(a b c))
  3)

(deftest array-total-size.4
  (array-total-size #*0011010)
  7)

(deftest array-total-size.5
  (array-total-size #2a((1 2 3)(4 5 6)(7 8 9)(a b c)))
  12)

(deftest array-total-size.6
  (macrolet ((%m (z) z))
	    (array-total-size (expand-in-current-env (%m #(a b c)))))
  3)

(deftest array-total-size.order.1
  (let ((i 0) a)
    (values
     (array-total-size (progn (setf a (incf i)) #(a b c d)))
     i a))
  4 1 1)

;;; Error tests

(deftest array-total-size.error.1
  (signals-error (array-total-size) program-error)
  t)

(deftest array-total-size.error.2
  (signals-error (array-total-size #(a b c) nil) program-error)
  t)

(deftest array-total-size.error.3
  (check-type-error #'array-total-size #'arrayp)
  nil)

(deftest array-total-size.error.4
  (signals-error (array-total-size 0) type-error)
  t)

(deftest array-total-size.error.5
  (signals-type-error x 0 (locally (array-total-size x) t))
  t)

