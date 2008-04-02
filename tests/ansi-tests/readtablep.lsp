;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  1 19:19:42 2005
;;;; Contains: Tests of READTABLEP

(in-package :cl-test)

(deftest readtablep.1
    (and (not (readtablep nil))
	 (not (readtablep 'a))
	 (not (readtablep 0))
	 (not (readtablep 1/2))
	 (not (readtablep 1.2))
	 (not (readtablep 1.2s2))
	 (not (readtablep 1.2f3))
	 (not (readtablep 1.2e2))
	 (not (readtablep 1.2d2))
	 (not (readtablep (list 'a)))
	 (not (readtablep "abcde"))
	 (not (readtablep t))
	 (not (readtablep '*readtable*))
	 (not (readtablep (make-array '(10))))
	 (not (readtablep (make-array '(10) :element-type 'fixnum)))
	 (not (readtablep (make-array '(10) :element-type 'float)))
	 (not (readtablep (make-array '(10) :element-type 'double-float)))
	 (not (readtablep (make-array '(10) :element-type 'string)))
	 (not (readtablep (make-array '(10) :element-type 'character)))
	 (not (readtablep (make-array '(10) :element-type 'bit)))
	 (not (readtablep (make-array '(10) :element-type 'boolean)))
	 (not (not (readtablep (copy-readtable))))
	 (not (readtablep #'car))
	 )
  t)

(deftest readtablep.2
  (check-type-predicate #'readtablep 'readtable)
  nil)

(deftest readtablep.3
  (notnot-mv (readtablep *readtable*))
  t)

(deftest readtablep.4
  (notnot-mv (readtablep (copy-readtable)))
  t)

;;; Error tests

(deftest readtablep.error.1
  (signals-error (readtablep) program-error)
  t)

(deftest readtablep.error.2
  (signals-error (readtablep *readtable* nil) program-error)
  t)

(deftest readtablep.error.3
  (signals-error (readtablep *readtable* nil t t t t) program-error)
  t)
