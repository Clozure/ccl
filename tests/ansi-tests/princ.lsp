;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul 25 11:40:37 2004
;;;; Contains: Tests of PRINC

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; This function is mostly tested elsewhere

(deftest princ.1
  (random-princ-test 5)
  nil)

(deftest princ.2
  (with-standard-io-syntax
   (with-output-to-string
     (os)
     (with-input-from-string
      (is "")
      (with-open-stream (*terminal-io* (make-two-way-stream is os))
			(princ 2 t)))))
  "2")

(deftest princ.3
  (with-standard-io-syntax
   (with-output-to-string
     (*standard-output*)
     (princ 3 nil)))
  "3")

;;; Error tests

(deftest princ.error.1
  (signals-error
   (with-output-to-string (*standard-output*) (princ))
   program-error)
  t)

(deftest princ.error.2
  (signals-error
   (with-output-to-string (s) (princ nil s nil))
   program-error)
  t)
