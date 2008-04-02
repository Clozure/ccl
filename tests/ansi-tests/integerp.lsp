;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 10:18:34 2003
;;;; Contains: Tests for INTEGERP

(in-package :cl-test)

(deftest integerp.error.1
  (signals-error (integerp) program-error)
  t)

(deftest integerp.error.2
  (signals-error (integerp 0 0) program-error)
  t)

(deftest integerp.error.3
  (signals-error (integerp nil nil) program-error)
  t)

(deftest integerp.1
  (loop for i in *integers*
	for vals = (multiple-value-list (integerp i))
	unless (and (= (length vals) 1)
		    (first vals))
	collect (cons i vals))
  nil)

(deftest integerp.2
  (check-type-predicate #'integerp 'integer)
  nil)





