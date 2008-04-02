;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 28 06:41:46 2004
;;;; Contains: Tests of FORCE-OUTPUT

(in-package :cl-test)

(deftest force-output.1
  (force-output)
  nil)

(deftest force-output.2
  (force-output t)
  nil)

(deftest force-output.3
  (force-output nil)
  nil)

(deftest force-output.4
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-output* *trace-output* *terminal-io*)
	for results = (multiple-value-list (force-output s))
	unless (equal results '(nil))
	collect s)
  nil)

(deftest force-output.5
  (let ((os (make-string-output-stream)))
    (let ((*terminal-io* (make-two-way-stream (make-string-input-stream "")
					      os)))
      (force-output t)))
  nil)

(deftest force-output.6
  (let ((*standard-output* (make-string-output-stream)))
    (force-output nil))
  nil)


;;; Error tests

(deftest force-output.error.1
  (signals-error (force-output nil nil) program-error)
  t)

(deftest force-output.error.2
  (signals-error (force-output t nil) program-error)
  t)

(deftest force-output.error.3
  (check-type-error #'force-output
		    #'(lambda (x) (typep x '(or stream (member nil t)))))
  nil)


