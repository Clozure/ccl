;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 06:59:37 2003
;;;; Contains: Tests of ARRAY-DIMENSIONS

(in-package :cl-test)

;;; The tests in make-array.lsp also test this function

(deftest array-dimensions.1
  (array-dimensions #0aX)
  nil)

(deftest array-dimensions.2
  (array-dimensions #(a b c d))
  (4))

(deftest array-dimensions.3
  (array-dimensions #*0011011011)
  (10))

(deftest array-dimensions.4
  (array-dimensions "abcdef")
  (6))

(deftest array-dimensions.5
  (array-dimensions #2a((1 2 3)(4 5 6)(7 8 9)(10 11 12)))
  (4 3))

(deftest array-dimensions.6
  (let ((a (make-array '(2 3 4) :adjustable t)))
    (values (array-dimension a 0)
	    (array-dimension a 1)
	    (array-dimension a 2)))
  2 3 4)

(deftest array-dimensions.7
  (let ((a (make-array '(10) :fill-pointer 5)))
    (array-dimension a 0))
  10)

(deftest array-dimensions.8
  (macrolet ((%m (z) z)) (array-dimensions
			  (expand-in-current-env (%m #2a((a b)(c d)(e f))))))
  (3 2))

;;; Error tests

(deftest array-dimensions.error.1
  (signals-error (array-dimensions) program-error)
  t)

(deftest array-dimensions.error.2
  (signals-error (array-dimensions #(a b c) nil)
		 program-error)
  t)

(deftest array-dimensions.error.3
  (check-type-error #'array-dimensions #'arrayp)
  nil)

(deftest array-dimensions.error.4
  (signals-type-error x nil (array-dimensions x))
  t)

(deftest array-dimensions.error.5
  (signals-error (locally (array-dimensions nil))
		 type-error)
  t)
