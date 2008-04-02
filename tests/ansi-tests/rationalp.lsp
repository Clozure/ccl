;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 08:36:31 2003
;;;; Contains: Tests of RATIONALP

(in-package :cl-test)

(deftest rationalp.error.1
  (signals-error (rationalp) program-error)
  t)

(deftest rationalp.error.2
  (signals-error (rationalp 0 nil) program-error)
  t)

(deftest rationalp.error.3
  (signals-error (rationalp 'a 0) program-error)
  t)

(deftest rationalp.1
  (loop for x in *rationals*
	for vals = (multiple-value-list (rationalp x))
	unless (and (= (length vals) 1)
		    (first vals))
	collect (cons x vals))
  nil)

(deftest rationalp.2
  (loop for x in (set-difference *universe* *rationals*)
	for vals = (multiple-value-list (rationalp x))
	unless (and (= (length vals) 1)
		    (null (first vals)))
	collect (cons x vals))
  nil)

(deftest rationalp.3
  (check-type-predicate #'rationalp 'rational)
  nil)


