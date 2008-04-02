;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 05:56:22 2003
;;;; Contains: Tests for HASH-TABLE-TEST

(in-package :cl-test)

(deftest hash-table-test.1
  (hash-table-test (make-hash-table))
  eql)

(deftest hash-table-test.2
  (loop for test in '(eq eql equal equalp)
	unless (eq (hash-table-test (make-hash-table :test test)) test)
	collect test)
  nil)

(deftest hash-table-test.3
  (loop for test in '(eq eql equal equalp)
	unless (eq (hash-table-test (make-hash-table
				     :test (symbol-function test)))
		   test)
	collect test)
  nil)

(deftest hash-table-test.4
  (loop for test in '(eq eql equal equalp)
	unless (eq (hash-table-test (make-hash-table
				     :test (eval `(function ,test))))
		   test)
	collect test)
  nil)

;;; Error cases

(deftest hash-table-test.error.1
  (signals-error (hash-table-test) program-error)
  t)

(deftest hash-table-test.error.2
  (signals-error (hash-table-test (make-hash-table) nil)  program-error)
  t)

(deftest hash-table-test.error.3
  (check-type-error #'hash-table-test #'hash-table-p)
  nil)
