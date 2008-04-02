;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Feb 12 04:25:59 2004
;;;; Contains: Tests off TWO-WAY-STREAM-OUTPUT-STREAM

(in-package :cl-test)

(deftest two-way-stream-output-stream.1
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-two-way-stream is os)))
    (equalt (multiple-value-list (two-way-stream-output-stream s))
	    (list os)))
  t)

(deftest two-way-stream-output-stream.error.1
  (signals-error (two-way-stream-output-stream) program-error)
  t)

(deftest two-way-stream-output-stream.error.2
  (signals-error (let* ((is (make-string-input-stream "foo"))
			(os (make-string-output-stream))
			(s (make-two-way-stream is os)))
		   (two-way-stream-output-stream s nil))
		 program-error)
  t)
