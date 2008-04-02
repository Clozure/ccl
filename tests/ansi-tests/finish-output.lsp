;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 28 06:38:20 2004
;;;; Contains: Tests of FINISH-OUTPUT

(in-package :cl-test)

(deftest finish-output.1
  (finish-output)
  nil)

(deftest finish-output.2
  (finish-output t)
  nil)

(deftest finish-output.3
  (finish-output nil)
  nil)

(deftest finish-output.4
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-output* *trace-output* *terminal-io*)
	for results = (multiple-value-list (finish-output s))
	unless (equal results '(nil))
	collect s)
  nil)

(deftest finish-output.5
  (let ((os (make-string-output-stream)))
    (let ((*terminal-io* (make-two-way-stream (make-string-input-stream "")
					      os)))
      (finish-output t)))
  nil)

(deftest finish-output.6
  (let ((*standard-output* (make-string-output-stream)))
    (finish-output nil))
  nil)

;;; Error tests

(deftest finish-output.error.1
  (signals-error (finish-output nil nil) program-error)
  t)

(deftest finish-output.error.2
  (signals-error (finish-output t nil) program-error)
  t)

(deftest finish-output.error.3
  (check-type-error #'finish-output
		    #'(lambda (x) (typep x '(or stream (member nil t)))))
  nil)

