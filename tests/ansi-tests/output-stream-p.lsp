;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 13 19:46:12 2004
;;;; Contains: Tests of OUTPUT-STREAM-P

(in-package :cl-test)

(deftest output-stream-p.1
  (notnot-mv (output-stream-p *standard-output*))
  t)

(deftest output-stream-p.2
  (notnot-mv (output-stream-p *terminal-io*))
  t)

(deftest output-stream-p.3
  (with-open-file (s "output-stream-p.lsp" :direction :input)
		  (output-stream-p s))
  nil)

(deftest output-stream-p.4
  (with-open-file (s "foo.txt" :direction :output
		     :if-exists :supersede)
		  (notnot-mv (output-stream-p s)))
  t)

;;; Error tests

(deftest output-stream-p.error.1
  (signals-error (output-stream-p) program-error)
  t)

(deftest output-stream-p.error.2
  (signals-error (output-stream-p *standard-output* nil) program-error)
  t)

(deftest output-stream-p.error.3
  (check-type-error #'output-stream-p #'streamp)
  nil)
