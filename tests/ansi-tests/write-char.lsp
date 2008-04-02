;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 18 20:50:31 2004
;;;; Contains: Tests of WRITE-CHAR

(in-package :cl-test)

(deftest write-char.1
  (loop for i from 0 to 255
	for c = (code-char i)
	when c
	unless (string= (with-output-to-string
			  (*standard-output*)
			  (write-char c))
			(string c))
	collect c)
  nil)

(deftest write-char.2
  (with-input-from-string
   (is "abcd")
   (with-output-to-string
     (os)
     (let ((*terminal-io* (make-two-way-stream is os)))
       (write-char #\$ t)
       (close *terminal-io*))))
  "$")

(deftest write-char.3
  (with-output-to-string
    (*standard-output*)
    (write-char #\: nil))
  ":")

;;; Error tests

(deftest write-char.error.1
  (signals-error (write-char) program-error)
  t)

(deftest write-char.error.2
  (signals-error
   (with-output-to-string
     (s)
     (write-char #\a s nil))
   program-error)
  t)

;;; More tests in other files


