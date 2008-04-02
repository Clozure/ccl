;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jul 26 12:19:32 2004
;;;; Contains: Tests of PRINC-TO-STRING

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest princ-to-string.1
  (random-princ-to-string-test 1000)
  nil)

(deftest princ-to-string.2
  (with-standard-io-syntax (princ-to-string 2))
  "2")

;;; Error tests

(deftest princ-to-string.error.1
  (signals-error (princ-to-string) program-error)
  t)

(deftest princ-to-string.error.2
  (signals-error (princ-to-string nil nil) program-error)
  t)

