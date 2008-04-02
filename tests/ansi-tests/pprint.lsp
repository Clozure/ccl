;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul 25 11:42:48 2004
;;;; Contains: Tests of PPRINT

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; This function is mostly tested elsewhere

(deftest pprint.1
  (random-pprint-test 1000)
  nil)

(deftest pprint.2
  (with-standard-io-syntax
   (with-output-to-string
     (os)
     (with-input-from-string
      (is "")
      (with-open-stream (*terminal-io* (make-two-way-stream is os))
			(pprint 2 t)))))
  "
2")

(deftest pprint.3
  (with-standard-io-syntax
   (with-output-to-string
     (*standard-output*)
     (pprint 3 nil)))
  "
3")

;;; Error tests

(deftest pprint.error.1
  (signals-error
   (with-output-to-string (*standard-output*) (pprint))
   program-error)
  t)

(deftest pprint.error.2
  (signals-error
   (with-output-to-string (s) (pprint nil s nil))
   program-error)
  t)


