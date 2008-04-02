;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 05:23:45 2003
;;;; Contains: Tests for HASH-TABLE-SIZE

(in-package :cl-test)

(deftest hash-table-size.error.1
  (signals-error (hash-table-size) program-error)
  t)

(deftest hash-table-size.error.2
  (signals-error (hash-table-size (make-hash-table) nil)
		 program-error)
  t)

(deftest hash-table-size.error.3
  (check-type-error #'hash-table-size #'hash-table-p)
  nil)
