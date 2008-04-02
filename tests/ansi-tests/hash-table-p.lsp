;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep 16 21:58:37 2003
;;;; Contains: Tests for HASH-TABLE-P

(in-package :cl-test)

(deftest hash-table-p.1
  (loop for e in '(nil t 1 10.0 (a b c) #(a b c) #*1011
		       #0aNIL #2a((a b)(c d)) #p"foo"
		       "bar" #\a 3/5 #c(1.0 2.0))
	when (hash-table-p e)
	collect e)
  nil)

(deftest hash-table-p.2
  (check-type-predicate #'hash-table-p 'hash-table)
  nil)

(deftest hash-table-p.3
  (let ((i 0))
    (values (hash-table-p (incf i)) i))
  nil 1)

(deftest hash-table-p.4
  (hash-table-p t)
  nil)

(deftest hash-table-p.5
  (notnot-mv (hash-table-p (make-hash-table)))
  t)

(deftest hash-table-p.error.1
  (signals-error (hash-table-p) program-error)
  t)

(deftest hash-table-p.error.2
  (signals-error (let ((h (make-hash-table))) (hash-table-p h nil))
		 program-error)
  t)

