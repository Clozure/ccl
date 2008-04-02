;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 14 09:48:46 2004
;;;; Contains: Tests of GET-OUTPUT-STREAM-STRING

(in-package :cl-test)

;; this function is used extensively elsewhere in the test suite

(deftest get-output-stream-string.1
  (let ((s (make-string-output-stream)))
    (values
     (get-output-stream-string s)
     (write-string "abc" s)
     (write-string "def" s)
     (get-output-stream-string s)
     (get-output-stream-string s)))
  "" "abc" "def" "abcdef" "")

;;; Error cases

(deftest get-output-stream-string.error.1
  (signals-error (get-output-stream-string) t)
  t)

(deftest get-output-stream-string.error.2
  (signals-error (get-output-stream-string (make-string-output-stream) nil) t)
  t)



     
