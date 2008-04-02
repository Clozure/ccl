;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 06:44:45 2002
;;;; Contains: Tests of T

(in-package :cl-test)

(deftest t.1
  t t)

(deftest t.2
  (not-mv (constantp t))
  nil)

(deftest t.3
  (eqt t 't)
  t)

(deftest t.4
  (symbol-value t)
  t)

;;; Tests for use of T in case forms, as a stream designator, or as a class
;;; designator will be elsewhere
