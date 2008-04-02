;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 21:30:42 2003
;;;; Contains: Tests of HASH-TABLE and related interface

(in-package :cl-test)

(deftest hash-table.1
  (notnot-mv (find-class 'hash-table))
  t)

(deftest hash-table.2
  (loop for e in '(nil t 1 10.0 (a b c) #(a b c) #*1011
		       #0aNIL #2a((a b)(c d)) #p"foo"
		       "bar" #\a 3/5 #c(1.0 2.0))
	when (typep e 'hash-table)
	collect e)
  nil)

(deftest hash-table.3
  (let ((c (find-class 'hash-table)))
    (loop for e in '(nil t 1 10.0 (a b c) #(a b c) #*1011
			 #0aNIL #2a((a b)(c d)) #p"foo"
			 "bar" #\a 3/5 #c(1.0 2.0))
	  when (typep e c)
	  collect e))
  nil)

(deftest hash-table.4
  (notnot-mv (typep (make-hash-table) 'hash-table))
  t)

(deftest hash-table.5
  (notnot-mv (typep (make-hash-table) (find-class 'hash-table)))
  t)



	     


