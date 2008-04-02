;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Feb 18 18:38:55 2003
;;;; Contains: Tests of SUBTYPEP on REAL types.

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

;;; SUBTYPEP on real types

(deftest subtypep.real.1
  (loop for tp1 in '((real 10) (real 10 *)
		     (real 10 20)
		     (real (10) 20)
		     (real 10 (20))
		     (real (10) (20))
		     (real 10 1000000000000000)
		     (real (10)) (real (10) *))
	append
	(loop for tp2 in '(real (real) (real *)
			   (real * *) (real 10) (real 10 *)
			   (real 0) (real 0 *)
			   (real 19/2) (real 19/2 *)
			   (real 9.5) (real 9.5 *)
			   (real -1000000000000000))
	      unless (equal (multiple-value-list
			     (subtypep* tp1 tp2))
			    '(t t))
	      collect (list tp1 tp2)))
  nil)

(deftest subtypep.real.2
  (loop for tp1 in '((real * 10)
		     (real 0 10)
		     (real 0 (10))
		     (real (0) 10)
		     (real (0) (10))
		     (real -1000000000000000 10)
		     (real * (10)))
	append
	(loop for tp2 in '(real (real) (real *)
			   (real * *) (real * 10)
			   (real * 21/2)
			   (real * 10.5)
			   (real * 1000000000000000))
	      unless (equal (multiple-value-list
			     (subtypep* tp1 tp2))
			    '(t t))
	      collect (list tp1 tp2)))
  nil)

(deftest subtypep.real.3
  (loop for tp1 in '((real 10) (real 10 *)
		     (real 10 20)
		     (real 10 (21))
		     (real 10 1000000000000000))
	append
	(loop for tp2 in '((real 11) (real 11 *)
			   (real (10)) (real (10) *)
			   (integer 10) (integer 10 *)
			   (real 11)
			   (real (10))
			   (real 11 *)
			   (real (10) *)
			   (real * (20))
			   (real * 19)
			   (real * (20))
			   (real * 19))
	      unless (equal (multiple-value-list
			     (subtypep* tp1 tp2))
			    '(nil t))
	      collect (list tp1 tp2)))
  nil)

(deftest subtypep.real.4
  (loop for tp1 in '((real * 10)
		     (real 0 10)
		     (real (0) 10)
		     (real -1000000000000000 10))
	append
	(loop for tp2 in '((real * 9)
			   (real * (10))
			   (integer * 10)
			   (real * 9)
			   (real * (10)))
	      unless (equal (multiple-value-list
			     (subtypep* tp1 tp2))
			    '(nil t))
	      collect (list tp1 tp2)))
  nil)

(deftest subtypep.real.5
  (check-equivalence
   '(or (real 0 0) (real (0)))
   '(real 0))
  nil)

(deftest subtypep.real.6
  (check-equivalence
   '(and (real 0 10) (real 5 15))
   '(real 5 10))
  nil)

(deftest subtypep.real.7
  (check-equivalence
   '(and (real (0) 10) (real 5 15))
   '(real 5 10))
  nil)

(deftest subtypep.real.8
  (check-equivalence
   '(and (real 0 (10)) (real 5 15))
   '(real 5 (10)))
  nil)

(deftest subtypep.real.9
  (check-equivalence
   '(and (real (0) (10)) (real 5 15))
   '(real 5 (10)))
  nil)

(deftest subtypep.real.10
  (check-equivalence
   '(and (real 0 10) (real (5) 15))
   '(real (5) 10))
  nil)

(deftest subtypep.real.11
  (check-equivalence
   '(and (real 0 (10)) (real (5) 15))
   '(real (5) (10)))
  nil)

(deftest subtypep.real.12
  (check-equivalence
   '(and integer (real 0 10) (not (real (0) (10))))
   '(member 0 10))
  nil)

(deftest subtypep.real.13
  (check-equivalence '(and integer (real -1/2 1/2))
		     '(integer 0 0))
  nil)

(deftest subtypep.real.14
  (check-equivalence '(and integer (real -1/2 1/2))
		     '(eql 0))
  nil)

(deftest subtypep.real.15
  (check-equivalence '(and integer (real (-1/2) 1/2))
		     '(integer 0 0))
  nil)

(deftest subtypep.real.16
  (check-equivalence '(and integer (real (-1/2) (1/2)))
		     '(integer 0 0))
  nil)

(deftest subtypep.real.17
  (check-equivalence '(real 0 10) '(real 0.0 10.0))
  nil)

(deftest subtypep.real.18
  (check-equivalence '(and rational (real 0 10))
		     '(rational 0 10))
  nil)

(deftest subtypep.real.19
  (check-equivalence '(and rational (real 0 (10)))
		     '(rational 0 (10)))
  nil)

(deftest subtypep.real.20
  (check-equivalence '(and rational (real (0) (10)))
		     '(rational (0) (10)))
  nil)

(deftest subtypep.real.21
  (check-equivalence '(and rational (real 1/2 7/3))
		     '(rational 1/2 7/3))
  nil)

(deftest subtypep.real.22
  (check-equivalence '(and rational (real (1/11) (8/37)))
		     '(rational (1/11) (8/37)))
  nil)

(deftest subtypep.real.23
  (check-all-subtypep '(not (real -1/2 1/2)) '(not (integer 0 0)))
  nil)

(deftest subtypep.real.24
  (check-all-subtypep '(not (real -1/2 1/2)) '(not (eql 0)))
  nil)

(deftest subtypep.real.25
  (check-all-subtypep t '(or (not (real 0 10)) (not (real -100 -50))))
  nil)

