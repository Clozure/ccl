;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 05:52:52 2003
;;;; Contains: Tests of HASH-TABLE-REHASH-THRESHOLD

(in-package :cl-test)

(deftest hash-table-rehash-threshold.1
  (typep* (hash-table-rehash-threshold (make-hash-table))
	  '(real 0 1))
  t)

(deftest hash-table-rehash-threshold.2
  (loop for test in '(eq eql equal equalp)
	unless (typep* (hash-table-rehash-threshold (make-hash-table :test test))
		       '(real 0 1))
	collect test)
  nil)

(deftest hash-table-rehash-threshold.3
  (loop for test in '(eq eql equal equalp)
	for fn = (symbol-function test)
	unless (typep* (hash-table-rehash-threshold (make-hash-table :test fn))
		       '(real 0 1))
	collect test)
  nil)

(deftest hash-table-rehash-threshold.error.1
  (signals-error (hash-table-rehash-threshold) program-error)
  t)

(deftest hash-table-rehash-threshold.error.2
  (signals-error (hash-table-rehash-threshold (make-hash-table) nil)
		 program-error)
  t)

(deftest hash-table-rehash-threshold.error.3
  (check-type-error #'hash-table-rehash-threshold #'hash-table-p)
  nil)
