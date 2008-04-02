;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:57:03 2003
;;;; Contains: Tests for subtype relationships on cons types

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

;;; SUBTYPEP on CONS types

(defvar *cons-types*
  '(cons (cons) (cons *) (cons * *) (cons t) (cons t t)
	 (cons t *) (cons * t)))

(deftest subtypep.cons.1
  (loop for t1 in *cons-types*
	append (loop for t2 in *cons-types*
		     unless (equal (mapcar #'notnot
					   (multiple-value-list
					    (subtypep t1 t2)))
				   '(t t))
		     collect (list t1 t2)))
  nil)

(deftest subtypep.cons.2
  (loop for t1 in '((cons nil) (cons nil *) (cons nil t)
		    (cons * nil) (cons t nil) (cons nil nil))
	unless (subtypep t1 nil)
	collect t1)
  nil)

(deftest subtypep.cons.3
  (check-equivalence '(and (cons symbol *) (cons * symbol))
		     '(cons symbol symbol))
  nil)

(deftest subtypep.cons.4
  (check-equivalence '(and (cons (integer 0 10) *)
			   (cons (integer 5 15) (integer 10 20))
			   (cons * (integer 15 25)))
		     '(cons (integer 5 10) (integer 15 20)))
  nil)

(deftest subtypep.cons.5
  (check-equivalence
   '(and cons (not (cons symbol symbol)))
   '(or (cons (not symbol) *)
	(cons * (not symbol))))
  nil)

(deftest subtypep.cons.6
  (check-equivalence
   '(or (cons integer symbol) (cons integer integer)
	(cons symbol integer) (cons symbol symbol))
   '(cons (or integer symbol) (or integer symbol)))
  nil)

(deftest subtypep.cons.7
  (check-equivalence
   '(or (cons (integer 0 8) (integer 5 15))
	(cons (integer 0 7) (integer 0 6))
	(cons (integer 6 15) (integer 0 9))
	(cons (integer 3 15) (integer 4 15)))
   '(cons (integer 0 15) (integer 0 15)))
  nil)

(deftest subtypep.cons.8
  (check-equivalence
   '(or
     (cons integer (cons symbol integer))
     (cons symbol (cons integer symbol))
     (cons symbol (cons symbol integer))
     (cons symbol (cons integer integer))
     (cons integer (cons integer symbol))
     (cons symbol (cons symbol symbol))
     (cons integer (cons integer integer))
     (cons integer (cons symbol symbol)))
   '(cons (or symbol integer)
	  (cons (or symbol integer) (or symbol integer))))
  nil)

(deftest subtypep.cons.9
  (check-equivalence
   '(or
     (cons (integer 0 (3)) (integer 0 (6)))
     (cons (integer 3 (9)) (integer 0 (3)))
     (cons (integer 0 (6)) (integer 6 (9)))
     (cons (integer 6 (9)) (integer 3 (9)))
     (cons (integer 3 (6)) (integer 3 (6))))
   '(cons (integer 0 (9)) (integer 0 (9))))
  nil)

(deftest subtypep.cons.10
  (check-equivalence
   '(or
     (cons (rational 0 (3)) (rational 0 (6)))
     (cons (rational 3 (9)) (rational 0 (3)))
     (cons (rational 0 (6)) (rational 6 (9)))
     (cons (rational 6 (9)) (rational 3 (9)))
     (cons (rational 3 (6)) (rational 3 (6))))
   '(cons (rational 0 (9)) (rational 0 (9))))
  nil)

(deftest subtypep.cons.11
  (check-equivalence
   '(or
     (cons (real 0 (3)) (real 0 (6)))
     (cons (real 3 (9)) (real 0 (3)))
     (cons (real 0 (6)) (real 6 (9)))
     (cons (real 6 (9)) (real 3 (9)))
     (cons (real 3 (6)) (real 3 (6))))
   '(cons (real 0 (9)) (real 0 (9))))
  nil)

;;; Test suggested by C.R.
(deftest subtypep.cons.12
  (check-all-not-subtypep
   '(cons (or integer symbol)
	  (or integer symbol))
   '(or (cons integer symbol)
	(cons symbol integer)))
  nil)

(deftest subtypep.cons.13
  (check-all-not-subtypep '(not list) 'cons)
  nil)


;;; a -> b, a ==> b
(deftest subtypep.cons.14
  (check-all-subtypep
   '(and (or (cons (not symbol)) (cons * integer))
	 (cons symbol))
   '(cons * integer))
  nil)

;;; a -> b, not b ==> not a
(deftest subtypep.cons.15
  (check-all-subtypep
   '(and (or (cons (not symbol)) (cons * integer))
	 (cons * (not integer)))
   '(cons (not symbol)))
  nil)

;;; (and (or a b) (or (not b) c)) ==> (or a c)
(deftest subtypep.cons.16
  (check-all-subtypep
   '(and (or (cons symbol (cons * *))
	     (cons * (cons integer *)))
	 (or (cons * (cons (not integer) *))
	     (cons * (cons * float))))
   '(or (cons symbol (cons * *))
	(cons * (cons * float))))
  nil)

(deftest subtypep.cons.17
  (check-all-subtypep
   '(and (or (cons symbol (cons * *))
	     (cons * (cons integer *)))
	 (or (cons * (cons (not integer)))
	     (cons * (cons * float)))
	 (or (cons * (cons * (not float)))
	     (cons symbol (cons * *))))
   '(cons symbol))
  nil)

(deftest subtypep.cons.18
  (check-all-subtypep
   '(cons symbol)
   '(or (cons symbol (not integer))
	(cons * integer)))
  nil)

(deftest subtypep.cons.19
  (check-equivalence
   '(or
     (cons (eql a) (eql x))
     (cons (eql b) (eql y))
     (cons (eql c) (eql z))
     (cons (eql a) (eql y))
     (cons (eql b) (eql z))
     (cons (eql c) (eql x))
     (cons (eql a) (eql z))
     (cons (eql b) (eql x))
     (cons (eql c) (eql y)))
   '(cons (member a b c) (member x y z)))
  nil)

(deftest subtypep.cons.20
  (check-equivalence
   '(or
     (cons (eql a) (eql x))
     (cons (eql b) (eql y))
     (cons (eql a) (eql y))
     (cons (eql b) (eql z))
     (cons (eql c) (eql x))
     (cons (eql a) (eql z))
     (cons (eql b) (eql x))
     (cons (eql c) (eql y)))
   '(and (cons (member a b c) (member x y z))
	 (not (cons (eql c) (eql z)))))
  nil)

;;; Test case that came up in SBCL
(deftest subtypep.cons.21
  (check-all-subtypep
   '(cons integer single-float)
   '(or (cons fixnum single-float) (cons bignum single-float)))
  nil)

(deftest subtypep.cons.22
  (check-all-subtypep
   '(cons single-float integer)
   '(or (cons single-float fixnum) (cons single-float bignum)))
  nil)

;;; More test cases from SBCL, CMUCL, culled from random test failures

(deftest subtype.cons.23
  (let ((t1 '(cons t (cons (not long-float) symbol)))
	(t2 '(not (cons symbol (cons integer integer)))))
    (subtypep-and-contrapositive-are-consistent t1 t2))
  t)

(deftest subtype.cons.24
  (let ((t1 '(cons (eql 3671) (cons short-float (eql -663423073525))))
	(t2 '(not (cons t (cons (not complex) (cons integer t))))))
    (subtypep-and-contrapositive-are-consistent t1 t2))
  t)

(deftest subtype.cons.25
  (let ((t1 '(cons t (cons (not long-float) (integer 44745969 61634129))))
	(t2 '(not (cons (eql -3) (cons short-float (cons t float))))))
    (subtypep-and-contrapositive-are-consistent t1 t2))
  t)

(deftest subtype.cons.26
  (let ((t1 '(cons integer (cons single-float (cons t t))))
	(t2 '(cons t (cons (not complex) (not (eql 8))))))
    (subtypep-and-contrapositive-are-consistent t1 t2))
  t)

(deftest subtype.cons.27
  (let ((t1 '(cons (not (integer -27 30))
		   (cons rational (cons integer integer))))
	(t2 '(not (cons integer (cons integer (eql 378132631))))))
    (subtypep-and-contrapositive-are-consistent t1 t2))
  t)

(deftest subtype.cons.28
  (let ((t1 '(cons (integer -1696888 -1460338)
		   (cons single-float symbol)))
	(t2 '(not (cons (not (integer -14 20))
			(cons (not integer) cons)))))
    (subtypep-and-contrapositive-are-consistent t1 t2))
  t)

(deftest subtypep.cons.29
  (let ((t2 '(or (not (cons unsigned-byte cons))
		 (not (cons (integer -6 22) rational)))))
    (subtypep-and-contrapositive-are-consistent 'cons t2))
  t)

(deftest subtypep.cons.30
  (let ((t1 '(not (cons t (cons t (cons cons t)))))
	(t2 '(or (or (cons (cons t integer) t)
		     (not (cons t (cons t cons))))
		 (not (cons (cons (eql -27111309) t)
			    (cons t (eql 1140730)))))))
    (subtypep-and-contrapositive-are-consistent t1 t2))
  t)

(deftest subtypep.cons.31
  (let ((t2 '(or
	      (not
	       (cons (or (cons t ratio) (cons short-float t))
		     (cons (cons (eql -7418623) (integer -9 53))
			   (cons cons t))))
	      (not
	       (cons (cons t (eql -265039))
		     (cons (cons t cons) t))))))
    (subtypep-and-contrapositive-are-consistent 'cons t2))
  t)

(deftest subtypep.cons.32
  (let ((t2 '(cons t
		   (or (not (cons integer (eql 0)))
		       (not (cons (or float (eql 0)) cons))))))
    (subtypep-and-contrapositive-are-consistent 'cons t2))
  t)

(deftest subtypep.cons.33
  (let ((t2 '(or (not (cons (cons t cons) (cons t (cons unsigned-byte t))))
		 (not (cons (cons integer t) (cons t (cons cons t)))))))
    (subtypep-and-contrapositive-are-consistent 'cons t2))
  t)

(deftest subtypep.cons.34
  (let ((t2 '(or (not (cons (or (eql 0) ratio) (not cons)))
		 (not (cons integer cons)))))
    (subtypep-and-contrapositive-are-consistent 'cons t2))
  t)

(deftest subtypep.cons.35
  (notnot-mv (subtypep '(cons nil t) 'float))
  t t)

(deftest subtypep.cons.36
  (notnot-mv (subtypep '(cons t nil) 'symbol))
  t t)

(deftest subtypep.cons.37
  (notnot-mv (subtypep '(cons nil nil) 'real))
  t t)

(deftest subtypep.cons.38
  (let ((t1 '(cons t (complex (real -32 0))))
	(t2 `(not (cons t (complex (integer * -500))))))
    (subtypep-and-contrapositive-are-consistent t1 t2))
  t)

;;; From GCL

(deftest subtypep.cons.39
  (values (subtypep t '(and (not (cons cons (cons cons t))) (not (cons t cons)))))
  nil)

(deftest subtypep.cons.40
  (let ((type1 '(cons (eql 0) cons))
	(type2 '(cons unsigned-byte symbol)))
    (values
     (subtypep* type1 type2)
     (subtypep* `(not ,type2) `(not ,type1))))
  nil nil)

;;; From sbcl 0.9.5.31

(deftest subtypep.cons.41
  (let ((type1 '(cons t (complex (real -10 -4))))
	(type2 '(not (cons t (complex (integer -200 -100))))))
    (multiple-value-bind (sub1 success1)
	(subtypep* type1 type2)
      (multiple-value-bind (sub2 success2)
	  (subtypep* `(not ,type2) `(not ,type1))
	(if (and success1 success2 (not (eq sub1 sub2)))
	    (values sub1 sub2)
	    nil))))
  nil)

(deftest subtypep.cons.42
    (let ((t1 '(cons (cons (cons (real -744833699 -744833699) cons) (integer -234496 215373))
		integer))
	  (t2 '(cons (cons (cons integer integer) (integer -234496 215373)) t)))
      (values (subtypep `(not ,t2) `(not ,t1))))
  nil)

;;;; From sbcl 0.9.6.57

(deftest subtypep.cons.43
  (let* ((n -3.926510009989861d7)
	 (t1 '(not (cons float t)))
	 (t2 `(or (not (cons (eql 0) (real ,n ,n)))
		  (not (cons t (eql 0))))))
    (multiple-value-bind 
     (sub1 good1)
     (subtypep* t1 t2)
     (multiple-value-bind
      (sub2 good2)
      (subtypep* `(not ,t2) `(not ,t1))
      (or (not good1)
	  (not good2)
	  (and sub1 sub2)
	  (and (not sub1) (not sub2))))))
  t)

