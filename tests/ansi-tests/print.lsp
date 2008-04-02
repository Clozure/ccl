;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul 25 11:41:16 2004
;;;; Contains: Tests of PRINT

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; This function is mostly tested elsewhere

(deftest print.1
  (random-print-test 1000)
  nil)

(deftest print.2
  (with-standard-io-syntax
   (with-output-to-string
     (os)
     (with-input-from-string
      (is "")
      (with-open-stream (*terminal-io* (make-two-way-stream is os))
			(print 2 t)))))
  "
2 ")

(deftest print.3
  (with-standard-io-syntax
   (with-output-to-string
     (*standard-output*)
     (print 3 nil)))
  "
3 ")

;;; Error tests

(deftest print.error.1
  (signals-error
   (with-output-to-string (*standard-output*) (print))
   program-error)
  t)

(deftest print.error.2
  (signals-error
   (with-output-to-string (s) (print nil s nil))
   program-error)
  t)

