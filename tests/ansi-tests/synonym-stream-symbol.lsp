;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan 29 21:21:06 2004
;;;; Contains: Tests of SYNONYM-STREAM-SYMBOL

(in-package :cl-test)

(deftest synonym-stream-symbol.1
  (synonym-stream-symbol (make-synonym-stream '*standard-input*))
  *standard-input*)

(deftest synonym-stream-symbol.error.1
  (signals-error (synonym-stream-symbol) program-error)
  t)

(deftest synonym-stream-symbol.error.2
  (signals-error (synonym-stream-symbol
		  (make-synonym-stream '*terminal-io*)
		  nil)
		 program-error)
  t)


