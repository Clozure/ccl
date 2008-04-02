;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:58:43 2003
;;;; Contains: Tests for subtype relationships on EQL types

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

(deftest subtypep.eql.1
  (let ((s1 (copy-seq "abc"))
	(s2 (copy-seq "abc")))
    (let ((t1 `(eql ,s1))
	  (t2 `(eql ,s2)))
      (cond
       ((subtypep t1 t2) "T1 is subtype of T2")
       ((subtypep t2 t1) "T2 is subtype of T1")
       (t (check-disjointness t1 t2)))))
  nil)
    
(deftest subtypep.eql.2
  (let ((s1 (copy-seq '(a b c)))
	(s2 (copy-seq '(a b c))))
    (let ((t1 `(eql ,s1))
	  (t2 `(eql ,s2)))
      (cond
       ((subtypep t1 t2) "T1 is subtype of T2")
       ((subtypep t2 t1) "T2 is subtype of T1")
       (t (check-disjointness t1 t2)))))
  nil)

(deftest subtypep.eql.3
  (let ((i1 (1+ most-positive-fixnum))
	(i2 (1+ most-positive-fixnum)))
    (check-equivalence `(eql ,i1) `(eql ,i2)))
  nil)

(deftest subtypep.eql.4
  (check-equivalence '(and (eql a) (eql b)) nil)
  nil)

(deftest subtypep.eql.5
  (check-all-subtypep '(eql a) '(satisfies symbolp))
  nil)

(deftest subtypep.eql.6
  (check-disjointness '(eql 17) '(satisfies symbolp))
  nil)

(deftest subtypep.eql.7
  (check-all-subtypep '(eql nil) '(satisfies symbolp))
  nil)

(deftest subtypep.eql.8
  (check-all-not-subtypep '(satisfies symbolp) '(eql a))
  nil)
