;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 14 20:51:33 2004
;;;; Contains: Tests of STREAM-ERROR-STREAM

(in-package :cl-test)

(deftest stream-error-stream.1
  (with-input-from-string
   (s "")
   (handler-case
    (read-char s)
    (stream-error (c) (eqlt (stream-error-stream c) s))))
  t)

;;; Error tests

(deftest stream-error-stream.error.1
  (signals-error (stream-error-stream) program-error)
  t)


(deftest stream-error-stream.error.2
  (signals-error
   (with-input-from-string
    (s "")
    (handler-case
     (read-char s)
     (stream-error (c) (stream-error-stream c nil))))
   program-error)
  t)


			  
