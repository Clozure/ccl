;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 23:07:33 2003
;;;; Contains: Tests of FLOATP

(in-package :cl-test)

;;; Error tests

(deftest floatp.error.1
  (signals-error (floatp) program-error)
  t)

(deftest floatp.error.2
  (signals-error (floatp 1.0 nil) program-error)
  t)

;;; Non-error tests

(deftest floatp.1
  (notnot-mv (floatp 1.0))
  t)

(deftest floatp.2
  (floatp nil)
  nil)

(deftest floatp.3
  (check-type-predicate #'floatp 'float)
  nil)

