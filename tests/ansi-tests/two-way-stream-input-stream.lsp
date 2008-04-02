;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Feb 12 04:22:50 2004
;;;; Contains: Tests of TWO-WAY-STREAM-INPUT-STREAM

(in-package :cl-test)

(deftest two-way-stream-input-stream.1
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-two-way-stream is os)))
    (equalt (multiple-value-list (two-way-stream-input-stream s))
	    (list is)))
  t)

(deftest two-way-stream-input-stream.error.1
  (signals-error (two-way-stream-input-stream) program-error)
  t)

(deftest two-way-stream-input-stream.error.2
  (signals-error (let* ((is (make-string-input-stream "foo"))
			(os (make-string-output-stream))
			(s (make-two-way-stream is os)))
		   (two-way-stream-input-stream s nil))
		 program-error)
  t)
