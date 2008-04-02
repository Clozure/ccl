;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Feb 12 04:30:40 2004
;;;; Contains: Tests of ECHO-STREAM-INPUT-STREAM

(in-package :cl-test)

(deftest echo-stream-input-stream.1
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (equalt (multiple-value-list (echo-stream-input-stream s))
	    (list is)))
  t)

(deftest echo-stream-input-stream.error.1
  (signals-error (echo-stream-input-stream) program-error)
  t)

(deftest echo-stream-input-stream.error.2
  (signals-error (let* ((is (make-string-input-stream "foo"))
			(os (make-string-output-stream))
			(s (make-echo-stream is os)))
		   (echo-stream-input-stream s nil))
		 program-error)
  t)

