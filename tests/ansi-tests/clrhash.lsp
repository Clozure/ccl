;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 09:33:40 2003
;;;; Contains: Tests of CLRHASH

(in-package :cl-test)

(deftest clrhash.1
  (let ((table (make-hash-table)))
    (setf (gethash 'a table) 'b)
    (values
     (hash-table-count table)
     (equalt (multiple-value-list (clrhash table))
	     (list table))
     (hash-table-count table)))
  1 t 0)

(deftest clrhash.2
  (let ((table (make-hash-table :test 'eq)))
    (setf (gethash 'a table) 'b)
    (values
     (hash-table-count table)
     (equalt (multiple-value-list (clrhash table))
	     (list table))
     (hash-table-count table)))
  1 t 0)

(deftest clrhash.3
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash 'a table) 'b)
    (values
     (hash-table-count table)
     (equalt (multiple-value-list (clrhash table))
	     (list table))
     (hash-table-count table)))
  1 t 0)

(deftest clrhash.4
  (let ((table (make-hash-table :test 'equalp)))
    (setf (gethash 'a table) 'b)
    (values
     (hash-table-count table)
     (equalt (multiple-value-list (clrhash table))
	     (list table))
     (hash-table-count table)))
  1 t 0)

(deftest clrhash.5
  (let ((table (make-hash-table :test 'eql)))
    (setf (gethash 'a table) 'b)
    (values
     (hash-table-count table)
     (equalt (multiple-value-list (clrhash table))
	     (list table))
     (hash-table-count table)))
  1 t 0)

;;;

(deftest clrhash.error.1
  (signals-error (clrhash) program-error)
  t)

(deftest clrhash.error.2
  (signals-error (clrhash (make-hash-table) nil)
		 program-error)
  t)


