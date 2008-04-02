;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:56:19 2003
;;;; Contains: Tests for subtype relationships on rational types

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

;;; SUBTYPEP on rational types

(deftest subtypep.rational.1
  (loop for tp1 in '((rational 10) (rational 10 *)
		     (rational 10 20)
		     (rational (10) 20)
		     (rational 10 (20))
		     (rational (10) (20))
		     (rational 10 1000000000000000)
		     (rational (10)) (rational (10) *))
	append
	(loop for tp2 in '(rational (rational) (rational *)
			   (rational * *) (rational 10) (rational 10 *)
			   (rational 0) (rational 0 *)
			   (rational 19/2) (rational 19/2 *)
			   (rational -1000000000000000)
			   real (real) (real *)
			   (real * *) (real 10) (real 10 *)
			   (real 0) (real 0 *)
			   (real 19/2) (real 19/2 *)
			   (real -1000000000000000))
	      unless (equal (multiple-value-list
			     (subtypep* tp1 tp2))
			    '(t t))
	      collect (list tp1 tp2)))
  nil)

(deftest subtypep.rational.2
  (loop for tp1 in '((rational * 10)
		     (rational 0 10)
		     (rational 0 (10))
		     (rational (0) 10)
		     (rational (0) (10))
		     (rational -1000000000000000 10)
		     (rational * (10)))
	append
	(loop for tp2 in '(rational (rational) (rational *)
			   (rational * *) (rational * 10)
			   (rational * 21/2)
			   (rational * 1000000000000000)
			   real (real) (real *)
			   (real * *) (real * 10)
			   (real * 21/2)
			   (real * 1000000000000000))
	      unless (equal (multiple-value-list
			     (subtypep* tp1 tp2))
			    '(t t))
	      collect (list tp1 tp2)))
  nil)

(deftest subtypep.rational.3
  (loop for tp1 in '((rational 10) (rational 10 *)
		     (rational 10 20)
		     (rational 10 (21))
		     (rational 10 1000000000000000))
	append
	(loop for tp2 in '((rational 11) (rational 11 *)
			   (rational (10)) (rational (10) *)
			   (integer 10) (integer 10 *)
			   (real 11)
			   (real (10))
			   (real 11 *)
			   (real (10) *)
			   (rational * (20))
			   (rational * 19)
			   (real * (20))
			   (real * 19))
	      unless (equal (multiple-value-list
			     (subtypep* tp1 tp2))
			    '(nil t))
	      collect (list tp1 tp2)))
  nil)

(deftest subtypep.rational.4
  (loop for tp1 in '((rational * 10)
		     (rational 0 10)
		     (rational (0) 10)
		     (rational -1000000000000000 10))
	append
	(loop for tp2 in '((rational * 9)
			   (rational * (10))
			   (integer * 10)
			   (real * 9)
			   (real * (10)))
	      unless (equal (multiple-value-list
			     (subtypep* tp1 tp2))
			    '(nil t))
	      collect (list tp1 tp2)))
  nil)

(deftest subtypep.rational.5
  (check-equivalence
   '(or (rational 0 0) (rational (0)))
   '(rational 0))
  nil)

(deftest subtypep.rational.6
  (check-equivalence
   '(and (rational 0 10) (rational 5 15))
   '(rational 5 10))
  nil)

(deftest subtypep.rational.7
  (check-equivalence
   '(and (rational (0) 10) (rational 5 15))
   '(rational 5 10))
  nil)

(deftest subtypep.rational.8
  (check-equivalence
   '(and (rational 0 (10)) (rational 5 15))
   '(rational 5 (10)))
  nil)

(deftest subtypep.rational.9
  (check-equivalence
   '(and (rational (0) (10)) (rational 5 15))
   '(rational 5 (10)))
  nil)

(deftest subtypep.rational.10
  (check-equivalence
   '(and (rational 0 10) (rational (5) 15))
   '(rational (5) 10))
  nil)

(deftest subtypep.rational.11
  (check-equivalence
   '(and (rational 0 (10)) (rational (5) 15))
   '(rational (5) (10)))
  nil)

(deftest subtypep.rational.12
  (check-equivalence
   '(and integer (rational 0 10) (not (rational (0) (10))))
   '(member 0 10))
  nil)

(deftest subtypep.rational.13
  (check-equivalence '(and integer (rational -1/2 1/2))
		     '(integer 0 0))
  nil)

(deftest subtypep.rational.14
  (check-equivalence '(and integer (rational -1/2 1/2))
		     '(eql 0))
  nil)

(deftest subtypep.rational.15
  (check-equivalence '(and integer (rational (-1/2) 1/2))
		     '(integer 0 0))
  nil)

(deftest subtypep.rational.16
  (check-equivalence '(and integer (rational (-1/2) (1/2)))
		     '(integer 0 0))
  nil)

(deftest subtypep.rational.17
  (check-all-subtypep '(not (rational -1/2 1/2)) '(not (integer 0 0)))
  nil)

(deftest subtypep.rational.18
  (check-all-subtypep '(not (rational -1/2 1/2)) '(not (eql 0)))
  nil)

