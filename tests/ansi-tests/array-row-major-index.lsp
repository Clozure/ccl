;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 21:37:03 2003
;;;; Contains: Tests of ARRAY-ROW-MAJOR-INDEX

(in-package :cl-test)

;;; More array-row-major-index tests are in make-array.lsp

(deftest array-row-major-index.1
  (array-row-major-index #0aNIL)
  0)

(deftest array-row-major-index.2
  (loop for i from 0 to 4
	collect (array-row-major-index #(a b c d e) i))
  (0 1 2 3 4))

(deftest array-row-major-index.3
  (let ((a (make-array '(5) :fill-pointer 1)))
    (loop for i from 0 to 4
	  collect (array-row-major-index a i)))
  (0 1 2 3 4))

(deftest array-row-major-index.4
  (macrolet
   ((%m (z) z))
   (array-row-major-index (expand-in-current-env (%m #(a b c))) 1))
  1)

(deftest array-row-major-index.5
  (macrolet
   ((%m (z) z))
   (array-row-major-index #(a b c) (expand-in-current-env (%m 1))))
  1)

(deftest array-row-major-index.order.1
  (let ((x 0) y z
	(a #(a b c d e f)))
    (values
     (array-row-major-index
      (progn (setf y (incf x)) a)
      (progn (setf z (incf x)) 0))
     x y z))
  0 2 1 2)

;;; Error tests

(deftest array-row-major-index.error.1
  (signals-error (array-row-major-index) program-error)
  t)

