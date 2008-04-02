;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 08:22:06 2003
;;;; Contains: Tests of REALP

(in-package :cl-test)

(deftest realp.error.1
  (signals-error (realp) program-error)
  t)

(deftest realp.error.2
  (signals-error (realp 0 nil) program-error)
  t)

(deftest realp.error.3
  (signals-error (realp nil nil) program-error)
  t)

(deftest realp.1
  (notnot-mv (realp 0))
  t)
  
(deftest realp.2
  (notnot-mv (realp 0.0))
  t)
  
(deftest realp.3
  (realp #c(1 2))
  nil)

(deftest realp.4
  (notnot-mv (realp 17/13))
  t)

(deftest realp.5
  (realp 'a)
  nil)

(deftest realp.6
  (check-type-predicate #'realp 'real)
  nil)



