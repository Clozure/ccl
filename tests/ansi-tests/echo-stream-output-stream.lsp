;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Feb 12 04:32:33 2004
;;;; Contains: Tests off ECHO-STREAM-OUTPUT-STREAM

(in-package :cl-test)

(deftest echo-stream-output-stream.1
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (equalt (multiple-value-list (echo-stream-output-stream s))
	    (list os)))
  t)

(deftest echo-stream-output-stream.error.1
  (signals-error (echo-stream-output-stream) program-error)
  t)

(deftest echo-stream-output-stream.error.2
  (signals-error (let* ((is (make-string-input-stream "foo"))
			(os (make-string-output-stream))
			(s (make-echo-stream is os)))
		   (echo-stream-output-stream s nil))
		 program-error)
  t)
