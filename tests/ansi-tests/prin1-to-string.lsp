;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jul 26 12:18:22 2004
;;;; Contains: Tests of PRIN1-TO-STRING

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest prin1-to-string.1
  (random-prin1-to-string-test 5)
  nil)

(deftest prin1-to-string.2
  (with-standard-io-syntax (prin1-to-string 2))
  "2")

;;; Error tests

(deftest prin1-to-string.error.1
  (signals-error (prin1-to-string) program-error)
  t)

(deftest prin1-to-string.error.2
  (signals-error (prin1-to-string nil nil) program-error)
  t)
