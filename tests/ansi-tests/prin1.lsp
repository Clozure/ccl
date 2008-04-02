;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul 25 11:33:40 2004
;;;; Contains: Tests of PRIN1

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; This function is mostly tested elsewhere

(deftest prin1.1
  (random-prin1-test 1000)
  nil)

(deftest prin1.2
  (with-standard-io-syntax
   (with-output-to-string
     (os)
     (with-input-from-string
      (is "")
      (with-open-stream (*terminal-io* (make-two-way-stream is os))
			(prin1 2 t)))))
  "2")

(deftest prin1.3
  (with-standard-io-syntax
   (with-output-to-string
     (*standard-output*)
     (prin1 3 nil)))
  "3")

;;; Error tests

(deftest prin1.error.1
  (signals-error
   (with-output-to-string (*standard-output*) (prin1))
   program-error)
  t)

(deftest prin1.error.2
  (signals-error
   (with-output-to-string (s) (prin1 nil s nil))
   program-error)
  t)
