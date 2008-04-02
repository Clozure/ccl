;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul 25 12:53:11 2004
;;;; Contains: Tests of WRITE-TO-STRING

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; This function is extensively used elsewhere

(deftest write-to-string.1
  (random-write-to-string-test 1000)
  nil)

(deftest write-to-string.2
  (with-standard-io-syntax
   (write-to-string 2 :allow-other-keys nil))
  "2")

(deftest write-to-string.3
  (with-standard-io-syntax
   (write-to-string 3 :allow-other-keys t '#.(gensym) 0))
  "3")

(deftest write-to-string.4
  (with-standard-io-syntax
   (write-to-string 4 :base 10 :base 2))
  "4")

;;; Error tests

(deftest write-to-string.error.1
  (signals-error (write-to-string) program-error)
  t)

(deftest write-to-string.error.2
  (signals-error (write-to-string nil '#.(gensym) nil) program-error)
  t)

(deftest write-to-string.error.3
  (signals-error (write-to-string nil :radix) program-error)
  t)
