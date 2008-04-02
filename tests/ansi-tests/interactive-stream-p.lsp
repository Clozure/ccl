;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 13 19:47:59 2004
;;;; Contains: Tests of INTERACTIVE-STREAM-P

(in-package :cl-test)

(deftest interactive-stream-p.1
  (let ((streams (list *debug-io* *error-output* *query-io*
		       *standard-input* *standard-output*
		       *trace-output* *terminal-io*)))
    (mapc #'interactive-stream-p streams)
    ;; no error should occur
    nil)
  nil)

(deftest interactive-stream-p.error.1
  (check-type-error #'interactive-stream-p #'streamp)
  nil)

(deftest interactive-stream-p.error.2
  (signals-error (interactive-stream-p) program-error)
  t)

(deftest interactive-stream-p.error.3
  (signals-error (interactive-stream-p *terminal-io* nil)
		 program-error)
  t)
