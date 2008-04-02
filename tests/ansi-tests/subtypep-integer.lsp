;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:54:05 2003
;;;; Contains: Tests for subtype relationships on integer types

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

(deftest subtypep.fixnum-or-bignum
  (check-equivalence '(or fixnum bignum) 'integer)
  nil)

(deftest subtypep.fixnum.integer
  (check-equivalence `(integer ,most-negative-fixnum ,most-positive-fixnum)
		     'fixnum)
  nil)

(deftest subtypep.bignum.integer
  (check-equivalence
   `(or (integer * (,most-negative-fixnum))
	(integer (,most-positive-fixnum) *))
   'bignum)
  nil)

;;;;;;;

(deftest subtypep.integer.1
  (subtypep* '(integer 0 10) '(integer 0 20))
  t t)

(deftest subtypep.integer.2
  (subtypep* '(integer 0 10) '(integer 0 (10)))
  nil t)

(deftest subtypep.integer.3
  (subtypep* '(integer 10 100) 'integer)
  t t)

(deftest subtypep.integer.3a
  (subtypep* '(integer 10 100) '(integer))
  t t)

(deftest subtypep.integer.3b
  (subtypep* '(integer 10 100) '(integer *))
  t t)

(deftest subtypep.integer.3c
  (subtypep* '(integer 10 100) '(integer * *))
  t t)

(deftest subtypep.integer.4
  (subtypep* 'integer '(integer 10 100))
  nil t)

(deftest subtypep.integer.4a
  (subtypep* '(integer) '(integer 10 100))
  nil t)

(deftest subtypep.integer.4b
  (subtypep* '(integer *) '(integer 10 100))
  nil t)

(deftest subtypep.integer.4c
  (subtypep* '(integer * *) '(integer 10 100))
  nil t)

(deftest subtypep.integer.5
  (subtypep* '(integer 10 *) 'integer)
  t t)

(deftest subtypep.integer.5a
  (subtypep* '(integer 10 *) '(integer))
  t t)

(deftest subtypep.integer.5b
  (subtypep* '(integer 10 *) '(integer *))
  t t)

(deftest subtypep.integer.5c
  (subtypep* '(integer 10 *) '(integer * *))
  t t)

(deftest subtypep.integer.6
  (subtypep* 'integer '(integer 10 *))
  nil t)

(deftest subtypep.integer.6a
  (subtypep* '(integer) '(integer 10 *))
  nil t)

(deftest subtypep.integer.6b
  (subtypep* '(integer *) '(integer 10 *))
  nil t)

(deftest subtypep.integer.6c
  (subtypep* '(integer * *) '(integer 10 *))
  nil t)

(deftest subtypep.integer.7
  (subtypep* '(integer 10) 'integer)
  t t)

(deftest subtypep.integer.7a
  (subtypep* '(integer 10) '(integer))
  t t)

(deftest subtypep.integer.7b
  (subtypep* '(integer 10) '(integer *))
  t t)

(deftest subtypep.integer.7c
  (subtypep* '(integer 10) '(integer * *))
  t t)

(deftest subtypep.integer.8
  (subtypep* 'integer '(integer 10))
  nil t)

(deftest subtypep.integer.8a
  (subtypep* '(integer) '(integer 10))
  nil t)

(deftest subtypep.integer.8b
  (subtypep* '(integer *) '(integer 10))
  nil t)

(deftest subtypep.integer.8c
  (subtypep* '(integer * *) '(integer 10))
  nil t)

(deftest subtypep.integer.9
  (subtypep* '(integer * 10) 'integer)
  t t)

(deftest subtypep.integer.9a
  (subtypep* '(integer * 10) '(integer))
  t t)

(deftest subtypep.integer.9b
  (subtypep* '(integer * 10) '(integer *))
  t t)

(deftest subtypep.integer.9c
  (subtypep* '(integer * 10) '(integer * *))
  t t)

(deftest subtypep.integer.10
  (subtypep* 'integer '(integer * 10))
  nil t)

(deftest subtypep.integer.10a
  (subtypep* '(integer) '(integer * 10))
  nil t)

(deftest subtypep.integer.10b
  (subtypep* '(integer *) '(integer * 10))
  nil t)

(deftest subtypep.integer.10c
  (subtypep* '(integer * *) '(integer * 10))
  nil t)

(deftest subtypep.integer.11
  (subtypep* '(integer 10) '(integer 5))
  t t)

(deftest subtypep.integer.12
  (subtypep* '(integer 5) '(integer 10))
  nil t)

(deftest subtypep.integer.13
  (subtypep* '(integer 10 *) '(integer 5))
  t t)

(deftest subtypep.integer.14
  (subtypep* '(integer 5) '(integer 10 *))
  nil t)

(deftest subtypep.integer.15
  (subtypep* '(integer 10) '(integer 5 *))
  t t)

(deftest subtypep.integer.16
  (subtypep* '(integer 5 *) '(integer 10))
  nil t)

(deftest subtypep.integer.17
  (subtypep* '(integer 10 *) '(integer 5 *))
  t t)

(deftest subtypep.integer.18
  (subtypep* '(integer 5 *) '(integer 10 *))
  nil t)

(deftest subtypep.integer.19
  (subtypep* '(integer * 5) '(integer * 10))
  t t)

(deftest subtypep.integer.20
  (subtypep* '(integer * 10) '(integer * 5))
  nil t)

(deftest subtypep.integer.21
  (subtypep* '(integer 10 *) '(integer * 10))
  nil t)

(deftest subtypep.integer.22
  (subtypep* '(integer * 10) '(integer 10 *))
  nil t)

(deftest subtypep.integer.23
  (check-equivalence '(integer (9)) '(integer 10))
  nil)

(deftest subtypep.integer.24
  (check-equivalence '(integer * (11)) '(integer * 10))
  nil)

(deftest subtypep.integer.25
  (check-equivalence
   '(and (or (integer 0 10) (integer 20 30))
	 (or (integer 5 15) (integer 25 35)))
   '(or (integer 5 10) (integer 25 30)))
  nil)

(deftest subtypep.integer.26
  (check-equivalence
   '(and (integer 0 10) (integer 5 15))
   '(integer 5 10))
  nil)

(deftest subtypep.integer.27
  (check-equivalence
   '(or (integer 0 10) (integer 5 15))
   '(integer 0 15))
  nil)

(deftest subtypep.integer.28
  (check-equivalence
   '(and integer (not (eql 10)))
   '(or (integer * 9) (integer 11 *)))
  nil)

(deftest subtypep.integer.29
  (check-equivalence
   '(and integer (not (integer 1 10)))
   '(or (integer * 0) (integer 11 *)))
  nil)

(deftest subtypep.integer.30
  (check-equivalence
   '(and (integer -100 100) (not (integer 1 10)))
   '(or (integer -100 0) (integer 11 100)))
  nil)

;;;  Relations between integer and real types

(deftest subtypep.integer.real.1
  (check-equivalence
   '(and integer (real 4 10))
   '(integer 4 10))
  nil)

(deftest subtypep.integer.real.2
  (check-equivalence
   '(and (integer 4 *) (real * 10))
   '(integer 4 10))
  nil)

(deftest subtypep.integer.real.3
  (check-equivalence
   '(and (integer * 10) (real 4))
   '(integer 4 10))
  nil)

(deftest subtypep.integer.real.4
  (loop for int-type in '(integer (integer) (integer *) (integer * *))
	append (loop for real-type in '(real (real) (real *) (real * *))
		     unless (equal (multiple-value-list
				    (subtypep* int-type real-type))
				   '(t t))
		     collect (list int-type real-type)))
  nil)

(deftest subtypep.integer.real.5
  (loop for int-type in '((integer 10) (integer 10 *))
	append (loop for real-type in '(real (real) (real *) (real * *)
					     (real 10.0) (real 10.0 *)
					     (real 10) (real 10 *))
		     unless (equal (multiple-value-list
				    (subtypep* int-type real-type))
				   '(t t))
		     collect (list int-type real-type)))
  nil)

(deftest subtypep.integer.real.6
  (loop for int-type in '((integer * 10) (integer * 5))
	append (loop for real-type in '(real (real) (real *) (real * *)
					     (real * 10.0)
					    (real * 10) (real * 1000000000000))
		     unless (equal (multiple-value-list
				    (subtypep* int-type real-type))
				   '(t t))
		     collect (list int-type real-type)))
  nil)

(deftest subtypep.integer.real.7
  (loop for int-type in '((integer 0 10) (integer 2 5))
	append (loop for real-type in '(real (real) (real *) (real * *)
					    (real * 10) (real * 1000000000000)
					    (real -10) (real -10.0)
					    (real -10 *) (real -10.0 *)
					    (real 0) (real 0.0)
					    (real 0 10) (real * 10)
					    (real 0 *) (real 0 10))
		     unless (equal (multiple-value-list
				    (subtypep* int-type real-type))
				   '(t t))
		     collect (list int-type real-type)))
  nil)

(deftest subtypep.integer.real.8
  (check-equivalence
   '(and (integer 4) (real * 10))
   '(integer 4 10))
  nil)

(deftest subtypep.integer.real.9
  (check-equivalence
   '(and (integer * 10) (real 4))
   '(integer 4 10))
  nil)

(deftest subtypep.integer.real.10
  (check-equivalence
   '(and (integer 4) (real * (10)))
   '(integer 4 9))
  nil)

(deftest subtypep.integer.real.11
  (check-equivalence
   '(and (integer * 10) (real (4)))
   '(integer 5 10))
  nil)


;;; Between integer and rational types

(deftest subtypep.integer.rational.1
  (check-equivalence
   '(and integer (rational 4 10))
   '(integer 4 10))
  nil)

(deftest subtypep.integer.rational.2
  (check-equivalence
   '(and (integer 4 *) (rational * 10))
   '(integer 4 10))
  nil)

(deftest subtypep.integer.rational.3
  (check-equivalence
   '(and (integer * 10) (rational 4))
   '(integer 4 10))
  nil)



(deftest subtypep.integer.rational.4
  (loop for int-type in '(integer (integer) (integer *) (integer * *))
	append (loop for rational-type
		     in '(rational (rational) (rational *) (rational * *))
		     unless (equal (multiple-value-list
				    (subtypep* int-type rational-type))
				   '(t t))
		     collect (list int-type rational-type)))
  nil)

(deftest subtypep.integer.rational.5
  (loop for int-type in '((integer 10) (integer 10 *))
	append (loop for rational-type
		     in '(rational (rational) (rational *) (rational * *)
				   (rational 19/2) (rational 19/2 *)
				   (rational 10) (rational 10 *))
		     unless (equal (multiple-value-list
				    (subtypep* int-type rational-type))
				   '(t t))
		     collect (list int-type rational-type)))
  nil)

(deftest subtypep.integer.rational.6
  (loop for int-type in '((integer * 10) (integer * 5))
	append (loop for rational-type
		     in '(rational (rational) (rational *) (rational * *)
				   (rational * 21/2)
				   (rational * 10) (rational * 1000000000000))
		     unless (equal (multiple-value-list
				    (subtypep* int-type rational-type))
				   '(t t))
		     collect (list int-type rational-type)))
  nil)

(deftest subtypep.integer.rational.7
  (loop for int-type in '((integer 0 10) (integer 2 5))
	append (loop for rational-type in
		     '(rational (rational) (rational *) (rational * *)
				(rational * 10) (rational * 1000000000000)
				(rational -1) (rational -1/2)
				(rational -1 *) (rational -1/2 *)
				(rational 0)
				(rational 0 10) (rational * 10)
				(rational 0 *) (rational 0 10))
		     unless (equal (multiple-value-list
				    (subtypep* int-type rational-type))
				   '(t t))
		     collect (list int-type rational-type)))
  nil)

(deftest subtypep.integer.rational.8
  (check-equivalence
   '(and integer (rational (4) 10))
   '(integer 5 10))
  nil)

(deftest subtypep.integer.rational.9
  (check-equivalence
   '(and (integer 4 *) (rational * (10)))
   '(integer 4 9))
  nil)

(deftest subtypep.integer.rational.10
  (check-equivalence
   '(and (integer * 10) (rational (4)))
   '(integer 5 10))
  nil)
