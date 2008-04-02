;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 28 06:43:17 2004
;;;; Contains: Tests of CLEAR-OUTPUT

(in-package :cl-test)

(deftest clear-output.1
  (progn (finish-output) (clear-output))
  nil)

(deftest clear-output.2
  (progn (finish-output) (clear-output t))
  nil)

(deftest clear-output.3
  (progn (finish-output) (clear-output nil))
  nil)

(deftest clear-output.4
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-output* *trace-output* *terminal-io*)
	for dummy = (finish-output s)
	for results = (multiple-value-list (clear-output s))
	unless (equal results '(nil))
	collect s)
  nil)

(deftest clear-output.5
  (let ((os (make-string-output-stream)))
    (let ((*terminal-io* (make-two-way-stream (make-string-input-stream "")
					      os)))
      (clear-output t)))
  nil)

(deftest clear-output.6
  (let ((*standard-output* (make-string-output-stream)))
    (clear-output nil))
  nil)

;;; Error tests

(deftest clear-output.error.1
  (signals-error (clear-output nil nil) program-error)
  t)

(deftest clear-output.error.2
  (signals-error (clear-output t nil) program-error)
  t)

(deftest clear-output.error.3
  (check-type-error #'clear-output #'(lambda (x) (typep x '(or stream (member nil t)))))
  nil)
