;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 05:47:24 2003
;;;; Contains: Tests for HASH-TABLE-REHASH-SIZE

(in-package :cl-test)

(deftest hash-table-rehash-size.1
  (typep* (hash-table-rehash-size (make-hash-table))
	  '(or (integer 1 *) (float (1.0) *)))
  t)

(deftest hash-table-rehash-size.2
  (loop for test in '(eq eql equal equalp)
	unless (typep* (hash-table-rehash-size (make-hash-table :test test))
		       '(or (integer 1 *) (float (1.0) *)))
	collect test)
  nil)

(deftest hash-table-rehash-size.3
  (loop for test in '(eq eql equal equalp)
	for fn = (symbol-function test)
	unless (typep* (hash-table-rehash-size (make-hash-table :test fn))
		       '(or (integer 1 *) (float (1.0) *)))
	collect test)
  nil)

(deftest hash-table-rehash-size.error.1
  (signals-error (hash-table-rehash-size) program-error)
  t)

(deftest hash-table-rehash-size.error.2
  (signals-error (hash-table-rehash-size (make-hash-table) nil)
		 program-error)
  t)

(deftest hash-table-rehash-size.error.3
  (check-type-error #'hash-table-rehash-size #'hash-table-p)
  nil)
