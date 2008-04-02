;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb  5 21:20:05 2003
;;;; Contains: More tests of types and classes

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

;;; Union of a type with its complement is universal

(deftest type-or-not-type-is-everything
  (loop for l in *disjoint-types-list2*
	append
	(loop
	 for type in l
	 append (check-subtypep t `(or ,type (not ,type)) t)
	 append (check-subtypep t `(or (not ,type) ,type) t)))
  nil)

(defclass tac-1-class () (a b c))
(defclass tac-1a-class (tac-1-class) (d e))
(defclass tac-1b-class (tac-1-class) (f g))

(deftest user-class-disjointness
  (loop for l in *disjoint-types-list2*
	append
	(loop
	 for type in l
	 append (classes-are-disjoint type 'tac-1-class)))
  nil)

(deftest user-class-disjointness-2
  (check-disjointness 'tac-1a-class 'tac-1b-class)
  nil)

(defstruct tac-2-struct a b c)
(defstruct (tac-2a-struct (:include tac-2-struct)) d e)
(defstruct (tac-2b-struct (:include tac-2-struct)) f g)

(deftest user-struct-disjointness
  (loop for l in *disjoint-types-list2*
	append
	(loop
	 for type in l
	 append (check-disjointness type 'tac-2-struct)))
  nil)

(deftest user-struct-disjointness-2
  (check-disjointness 'tac-2a-struct 'tac-2b-struct)
  nil)

(defclass tac-3-a () (x))
(defclass tac-3-b () (y))
(defclass tac-3-c () (z))

(defclass tac-3-ab (tac-3-a tac-3-b) ())
(defclass tac-3-ac (tac-3-a tac-3-c) ())
(defclass tac-3-bc (tac-3-b tac-3-c) ())

(defclass tac-3-abc (tac-3-ab tac-3-ac tac-3-bc) ())

(deftest tac-3.1
  (subtypep* 'tac-3-ab 'tac-3-a)
  t t)

(deftest tac-3.2
  (subtypep* 'tac-3-ab 'tac-3-b)
  t t)

(deftest tac-3.3
  (subtypep* 'tac-3-ab 'tac-3-c)
  nil t)

(deftest tac-3.4
  (subtypep* 'tac-3-a 'tac-3-ab)
  nil t)

(deftest tac-3.5
  (subtypep* 'tac-3-b 'tac-3-ab)
  nil t)

(deftest tac-3.6
  (subtypep* 'tac-3-c 'tac-3-ab)
  nil t)

(deftest tac-3.7
  (subtypep* 'tac-3-abc 'tac-3-a)
  t t)

(deftest tac-3.8
  (subtypep* 'tac-3-abc 'tac-3-b)
  t t)

(deftest tac-3.9
  (subtypep* 'tac-3-abc 'tac-3-c)
  t t)

(deftest tac-3.10
  (subtypep* 'tac-3-abc 'tac-3-ab)
  t t)

(deftest tac-3.11
  (subtypep* 'tac-3-abc 'tac-3-ac)
  t t)

(deftest tac-3.12
  (subtypep* 'tac-3-abc 'tac-3-bc)
  t t)

(deftest tac-3.13
  (subtypep* 'tac-3-ab 'tac-3-abc)
  nil t)

(deftest tac-3.14
  (subtypep* 'tac-3-ac 'tac-3-abc)
  nil t)

(deftest tac-3.15
  (subtypep* 'tac-3-bc 'tac-3-abc)
  nil t)

(deftest tac-3.16
  (check-equivalence '(and tac-3-a tac-3-b) 'tac-3-ab)
  nil)

(deftest tac-3.17
  (check-equivalence '(and (or tac-3-a tac-3-b)
			   (or (not tac-3-a) (not tac-3-b))
			   (or tac-3-a tac-3-c)
			   (or (not tac-3-a) (not tac-3-c))
			   (or tac-3-b tac-3-c)
			   (or (not tac-3-b) (not tac-3-c)))
		     nil)
  nil)

;;;
;;; Check that disjointness of types in *disjoint-types-list*
;;; is respected by all the elements of *universe*
;;;
(deftest universe-elements-in-at-most-one-disjoint-type
  (loop for e in *universe*
	for types = (remove-if-not #'(lambda (x) (typep e x))
				   *disjoint-types-list*)
	when (> (length types) 1)
	collect (list e types))
  nil)



;;;;;

(deftest integer-and-ratio-are-disjoint
  (classes-are-disjoint 'integer 'ratio)
  nil)

(deftest bignum-and-ratio-are-disjoint
  (classes-are-disjoint 'bignum 'ratio)
  nil)

(deftest bignum-and-fixnum-are-disjoint
  (classes-are-disjoint 'bignum 'fixnum)
  nil)

(deftest fixnum-and-ratio-are-disjoint
  (classes-are-disjoint 'fixnum 'ratio)
  nil)

(deftest byte8-and-ratio-are-disjoint
  (classes-are-disjoint '(unsigned-byte 8) 'ratio)
  nil)

(deftest bit-and-ratio-are-disjoint
  (classes-are-disjoint 'bit 'ratio)
  nil)

(deftest integer-and-float-are-disjoint
  (classes-are-disjoint 'integer 'float)
  nil)

(deftest ratio-and-float-are-disjoint
  (classes-are-disjoint 'ratio 'float)
  nil)

(deftest complex-and-float-are-disjoint
  (classes-are-disjoint 'complex 'float)
  nil)

(deftest integer-subranges-are-disjoint
  (classes-are-disjoint '(integer 0 (10)) '(integer 10 (20)))
  nil)

(deftest keyword-and-null-are-disjoint
  (classes-are-disjoint 'keyword 'null)
  nil)

(deftest keyword-and-boolean-are-disjoint
  (classes-are-disjoint 'keyword 'boolean)
  nil)
