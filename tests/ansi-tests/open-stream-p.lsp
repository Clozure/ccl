;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 13 19:52:30 2004
;;;; Contains: Tests of OPEN-STREAM-P

(in-package :cl-test)

(deftest open-stream-p.1
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-input* *standard-output*
		       *trace-output* *terminal-io*)
	for results = (multiple-value-list (open-stream-p s))
	unless (and (eql (length results) 1)
		    (car results))
	collect s)
  nil)

(deftest open-stream-p.2
  (with-open-file (s "open-stream-p.lsp" :direction :input)
		  (notnot-mv (open-stream-p s)))
  t)

(deftest open-stream-p.3
  (with-open-file (s "foo.txt" :direction :output
		     :if-exists :supersede)
		  (notnot-mv (open-stream-p s)))
  t)

(deftest open-stream-p.4
  (let ((s (open "open-stream-p.lsp" :direction :input)))
    (close s)
    (open-stream-p s))
  nil)

(deftest open-stream-p.5
  (let ((s (open "foo.txt" :direction :output
		 :if-exists :supersede)))
    (close s)
    (open-stream-p s))
  nil)

;;; error tests

(deftest open-stream-p.error.1
  (signals-error (open-stream-p) program-error)
  t)

(deftest open-stream-p.error.2
  (signals-error (open-stream-p *standard-input* nil) program-error)
  t)

(deftest open-stream-p.error.3
  (check-type-error #'open-stream-p #'streamp)
  nil)
