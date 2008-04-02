;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 13 19:39:27 2004
;;;; Contains: Tests for INPUT-STREAM-P

(in-package :cl-test)

(deftest input-stream-p.1
  (notnot-mv (input-stream-p *standard-input*))
  t)

(deftest input-stream-p.2
  (notnot-mv (input-stream-p *terminal-io*))
  t)

(deftest input-stream-p.3
  (with-open-file (s "input-stream-p.lsp" :direction :input)
		  (notnot-mv (input-stream-p s)))
  t)

(deftest input-stream-p.4
  (with-open-file (s "foo.txt" :direction :output
		     :if-exists :supersede)
		  (input-stream-p s))
  nil)

;;; Error tests

(deftest input-stream-p.error.1
  (signals-error (input-stream-p) program-error)
  t)

(deftest input-stream-p.error.2
  (signals-error (input-stream-p *standard-input* nil)
		 program-error)
  t)

(deftest input-stream-p.error.3
  (check-type-error #'input-stream-p #'streamp)
  nil)
