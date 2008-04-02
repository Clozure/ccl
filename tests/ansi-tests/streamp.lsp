;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 17 17:12:38 2004
;;;; Contains: Tests for STREAMP

(in-package :cl-test)

(deftest streamp.1
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-input* *standard-output*
		       *trace-output* *terminal-io*)
	unless (equal (multiple-value-list (notnot-mv (streamp s)))
		      '(t))
	collect s)
  nil)

(deftest streamp.2
  (check-type-predicate #'streamp 'stream)
  nil)

(deftest streamp.3
  (let ((s (open "foo.txt" :direction :output
		 :if-exists :supersede)))
    (close s)
    (notnot-mv (streamp s)))
  t)

(deftest streamp.4
  (let ((s (open "foo.txt" :direction :output
		 :if-exists :supersede)))
    (unwind-protect
	(notnot-mv (streamp s))
      (close s)))
  t)

;;; Error tests

(deftest streamp.error.1
  (signals-error (streamp) program-error)
  t)

(deftest streamp.error.2
  (signals-error (streamp *standard-input* nil) program-error)
  t)
