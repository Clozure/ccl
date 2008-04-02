;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 14 05:45:55 2003
;;;; Contains: Tests of SYMBOL-NAME

(in-package :cl-test)

(deftest symbol-name.1
  (symbol-name '|ABCD|)
  "ABCD")

(deftest symbol-name.2
  (symbol-name '|1234abcdABCD|)
  "1234abcdABCD")

(deftest symbol-name.3
  (symbol-name :|abcdefg|)
  "abcdefg")

;;; Error tests

(deftest symbol-name.error.1
  (signals-error (symbol-name) program-error)
  t)

(deftest symbol-name.error.2
  (signals-error (symbol-name 'a 'b) program-error)
  t)

(deftest symbol-name.error.3
  (check-type-error #'symbol-name #'symbolp)
  nil)


