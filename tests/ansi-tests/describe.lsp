;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Dec 12 13:22:13 2004
;;;; Contains: Tests of DESCRIBE

(in-package :cl-test)

(defun harness-for-describe (fn)
  (let (s1 s2)
    (with-open-stream
     (*standard-output* (make-string-output-stream))
     (with-open-stream
      (tio-input (make-string-input-stream "X"))
      (with-open-stream
       (tio-output (make-string-output-stream))
       (with-open-stream
	(*terminal-io* (make-two-way-stream tio-input tio-output))
	(let ((*print-circle* t)
	      (*print-readably* nil))
	  (assert (null (multiple-value-list (funcall fn))))))
       (setq s2 (get-output-stream-string tio-output)))
      (assert (equal (read-char tio-input) #\X)))
     (setq s1 (get-output-stream-string *standard-output*)))
    (values s1 s2)))

(deftest describe.1
  (loop for x in *universe*
	for (s1 s2) = (multiple-value-list (harness-for-describe #'(lambda () (describe x))))
	when (and (equal s1 "") (equal s2 ""))
	collect x)
  nil)

(deftest describe.2
  (loop for x in *universe*
	for s1 = nil
	for s2 = nil
	for s3 = (with-output-to-string (s)
		     (setf (values s1 s2) (harness-for-describe #'(lambda () (describe x s)))))
	when (or (equal s3 "") (not (equal "" s2)) (not (equal "" s1)))
	collect (list x s1 s2 s3))
  nil)

(deftest describe.3
  (loop for x in *universe*
	for (s1 s2) = (multiple-value-list (harness-for-describe #'(lambda () (describe x t))))
	when (or (equal "" s2) (not (equal "" s1)))
	collect (list x s1 s2))
  nil)

(deftest describe.4
  (loop for x in *universe*
	for (s1 s2) = (multiple-value-list (harness-for-describe #'(lambda () (describe x nil))))
	when (or (equal "" s1) (not (equal "" s2)))
	collect (list x s1 s2))
  nil)

;;; Defining methods for describe-object

(defclass describe-object-test-class-01 () ((s1 :initarg :s1) (s2 :initarg :s2) (s3 :initarg :s3)))

(defmethod describe-object ((obj describe-object-test-class-01) stream)
  (format stream "ABCDE ~A ~A ~A XYZ" (slot-value obj 's1) (slot-value obj 's2) (slot-value obj 's3)))

(deftest describe.5
  (let ((obj (make-instance 'describe-object-test-class-01 :s1 2 :s2 6 :s3 17)))
    (multiple-value-bind (str1 str2) (harness-for-describe #'(lambda () (describe obj)))
      (if (or (search "ABCDE 2 6 17 XYZ" str1)
	      (search "ABCDE 2 6 17 XYZ" str2))
	  :good
	(list str1 str2))))
  :good)

;;; Error cases

(deftest describe.error.1
  (signals-error (describe) program-error)
  t)

(deftest describe.error.2
  (signals-error (with-output-to-string (s) (describe nil s nil)) program-error)
  t)

