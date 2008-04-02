;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Tests of the function ARRAY-ELEMENT-TYPE

(in-package :cl-test)

;;; Mosts tests are in other files, incidental to testing of
;;; other things

(deftest array-element-type.1
  (macrolet ((%m (z) z))
	    (notnot (array-element-type (expand-in-current-env (%m #(a b c))))))
  t)

(deftest array-element-type.order.1
  (let ((i 0))
    (array-element-type (progn (incf i) #(a b c)))
    i)
  1)

;;; Error tests

(deftest array-element-type.error.1
  (signals-error (array-element-type) program-error)
  t)

(deftest array-element-type.error.2
  (signals-error (array-element-type #(a b c) nil) program-error)
  t)

(deftest array-element-type.error.3
  (check-type-error #'array-element-type #'arrayp)
  nil)

(deftest array-element-type.error.4
  (signals-type-error x nil (array-element-type x))
  t)


