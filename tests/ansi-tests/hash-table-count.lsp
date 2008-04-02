;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 05:14:25 2003
;;;; Contains: Tests of HASH-TABLE-COUNT

(in-package :cl-test)

(deftest hash-table-count.1
  (hash-table-count (make-hash-table))
  0)

(deftest hash-table-count.2
  (hash-table-count (make-hash-table :test 'eq))
  0)

(deftest hash-table-count.3
  (hash-table-count (make-hash-table :test 'eql))
  0)

(deftest hash-table-count.4
  (hash-table-count (make-hash-table :test 'equal))
  0)

(deftest hash-table-count.5
  (hash-table-count (make-hash-table :test 'equalp))
  0)

(deftest hash-table-count.6
  (hash-table-count (make-hash-table :test #'eq))
  0)

(deftest hash-table-count.7
  (hash-table-count (make-hash-table :test #'eql))
  0)

(deftest hash-table-count.8
  (hash-table-count (make-hash-table :test #'equal))
  0)

(deftest hash-table-count.9
  (hash-table-count (make-hash-table :test #'equalp))
  0)

(deftest hash-table-count.10
  (hash-table-count (let ((table (make-hash-table)))
		      (setf (gethash 'x table) 1)
		      table))
  1)

(deftest hash-table-count.11
  (let ((table (make-hash-table)))
    (setf (gethash 'x table) 1)
    (values (hash-table-count table)
	    (progn
	      (remhash 'x table)
	      (hash-table-count table))))
  1 0)

;; This function is mostly tested by calls to test-hash-table-1

(deftest hash-table-count.error.1
  (signals-error (hash-table-count) program-error)
  t)

(deftest hash-table-count.error.2
  (signals-error (hash-table-count (make-hash-table) nil)
		 program-error)
  t)
