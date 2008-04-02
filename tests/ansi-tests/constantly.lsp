;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct  6 19:47:16 2002
;;;; Contains: Tests for CONSTANTLY

(in-package :cl-test)

(deftest constantly.1
  (let ((fn (cl:constantly 10))
	(x nil))
    (loop for i from 0 to (min 256 (1- call-arguments-limit))
	  always (prog1 (eql (apply fn x) 10)
		   (push 'a x))))
  t)

(deftest constantly.2
  (notnot-mv (cl:constantly 1))
  t)

(deftest constantly.3
  (let ((i 0))
    (let ((fn (cl:constantly (progn (incf i) 'a))))
      (values
       i
       (mapcar fn '(1 2 3 4))
       i)))
  1 (a a a a) 1)

(deftest constantly.error.1
  (signals-error (cl:constantly) program-error)
  t)

;;; The next test fails in CMUCL, which has non-conformantly extended
;;; the syntax of constantly.
(deftest constantly.error.2
  (signals-error (cl:constantly 1 1) program-error)
  t)
