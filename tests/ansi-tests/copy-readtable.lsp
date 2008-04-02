;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Dec 31 07:15:35 2004
;;;; Contains: Tests of COPY-READTABLE

(in-package :cl-test)

(deftest copy-readtable.1
  (notnot-mv (typep (copy-readtable) 'readtable))
  t)

(deftest copy-readtable.2
  (notnot-mv (typep (copy-readtable *readtable*) 'readtable))
  t)

(deftest copy-readtable.3
  (notnot-mv (typep (copy-readtable *readtable* nil) 'readtable))
  t)

(deftest copy-readtable.4
  (let ((rt (copy-readtable *readtable*)))
    (eql rt *readtable*))
  nil)

(deftest copy-readtable.5
  (let ((rt (copy-readtable *readtable* nil)))
    (eql rt *readtable*))
  nil)

(deftest copy-readtable.6
  (let* ((rt (copy-readtable))
	 (rt2 (copy-readtable *readtable* rt)))
    (notnot (eql rt rt2)))
  t)

;;; NIL as a readtable designator indicating the standard readtable
(deftest copy-readtable.7
  (let ((rt (copy-readtable nil)))
    (values
     (notnot rt)
     (notnot (readtablep rt))
     (not (eql rt *readtable*))))
  t t t)

;;; Error tests

(deftest copy-readtable.error.1
  (signals-error (copy-readtable *readtable* nil nil) program-error)
  t)





