;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:58:06 2003
;;;; Contains: Tests for subtype relationships on member types

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

;;; SUBTYPEP on MEMBER types

(deftest subtypep.member.1
  (check-all-subtypep '(member a b c) '(member a b c d))
  nil)

(deftest subtypep.member.2
  (check-all-not-subtypep '(member a b c) '(member a b))
  nil)

(deftest subtypep.member.3
  (check-equivalence '(member) nil)
  nil)

(deftest subtypep.member.4
  (check-all-subtypep '(eql b) '(member a b c))
  nil)

(deftest subtypep.member.5
  (check-all-subtypep '(member a b c d e) 'symbol)
  nil)

(deftest subtypep.member.6
  (check-all-not-subtypep '(member a b 10 d e) 'symbol)
  nil)

(deftest subtypep.member.7
  (check-all-subtypep 'null '(member a b nil c d e))
  nil)

(deftest subtypep.member.8
  (check-all-not-subtypep 'null '(member a b c d e))
  nil)

(deftest subtypep.member.9
  (let ((b1 (1+ most-positive-fixnum))
	(b2 (1+ most-positive-fixnum)))
    (check-all-subtypep `(member 10 ,b1 20) `(member 10 20 ,b2)))
  nil)

(deftest subtypep.member.10
  (check-all-subtypep '(member :a :b :c) 'keyword)
  nil)

(deftest subtypep.member.11
  (let ((b1 (copy-list '(a)))
	(b2 (copy-list '(a))))
    (check-all-not-subtypep `(member 10 ,b1 20) `(member 10 20 ,b2)))
  nil)

(deftest subtypep.member.12
  (let ((b1 '(a)))
    (check-all-subtypep `(member 10 ,b1 20) `(member 10 20 ,b1)))
  nil)

(deftest subtypep.member.13
  (check-all-subtypep '(member 10 20 30) '(integer 0 100))
  nil)

(deftest subtypep.member.14
  (check-all-subtypep '(integer 3 6) '(member 0 1 2 3 4 5 6 7 8 100))
  nil)

(deftest subtypep.member.15
  (check-all-not-subtypep '(integer 3 6) '(member 0 1 2 3 5 6 7 8))
  nil)

(deftest subtypep.member.16
  (check-equivalence '(integer 2 5) '(member 2 5 4 3))
  nil)

(deftest subtypep.member.17
  (let ((s1 (copy-seq "abc"))
	(s2 (copy-seq "abc")))
    (let ((t1 `(member ,s1))
	  (t2 `(member ,s2)))
      (cond
       ((subtypep t1 t2) "T1 is subtype of T2")
       ((subtypep t2 t1) "T2 is subtype of T1")
       (t (check-disjointness t1 t2)))))
  nil)

(deftest subtypep.member.18
  (let ((s1 (copy-seq '(a b c)))
	(s2 (copy-seq '(a b c))))
    (let ((t1 `(member ,s1))
	  (t2 `(member ,s2)))
      (cond
       ((subtypep t1 t2) "T1 is subtype of T2")
       ((subtypep t2 t1) "T2 is subtype of T1")
       (t (check-disjointness t1 t2)))))
  nil)

(deftest subtypep.member.19
  (let ((i1 (1+ most-positive-fixnum))
	(i2 (1+ most-positive-fixnum)))
    (check-equivalence `(member 0 ,i1) `(member 0 ,i2)))
  nil)

(deftest subtypep.member.20
  (check-equivalence '(and (member a b c d) (member e d b f g))
		     '(member b d))
  nil)

(deftest subtypep.member.21
  (check-equivalence '(and (member a b c d) (member e d f g))
		     '(eql d))
  nil)

(deftest subtypep.member.22
  (check-equivalence '(and (member a b c d) (member e f g))
		     nil)
  nil)

(deftest subtypep.member.23
  (check-equivalence '(or (member a b c) (member z b w))
		     '(member z a b w c))
  nil)

(deftest subtypep.member.24
  (check-equivalence '(or (member a b c) (eql d))
		     '(member d c b a))
  nil)

(deftest subtypep.member.25
  (check-equivalence 'boolean '(member nil t))
  nil)

(deftest subtypep.member.26
  (check-equivalence '(or (eql a) (eql b))
		     '(member a b))
  nil)

(deftest subtypep.member.27
  (check-all-subtypep '(member a b c d) '(satisfies symbolp))
  nil)

(deftest subtypep.member.28
  (check-all-subtypep '(member a b c d) t)
  nil)

(deftest subtypep.member.29
  (check-all-not-subtypep '(member a b 10 z) '(satisfies symbolp))
  nil)

(deftest subtypep.member.30
  (check-disjointness '(member 1 6 10) '(satisfies symbolp))
  nil)

(deftest subtypep.member.31
  (check-equivalence '(member a b c d) '(member c d b a))
  nil)

(deftest subtypep.member.32
  (check-all-not-subtypep '(not (member a b 10 z)) '(satisfies symbolp))
  nil)

(deftest subtypep.member.33
  (check-all-not-subtypep  '(satisfies symbolp) '(member a b 10 z))
  nil)

(deftest subtypep.member.34
  (check-all-not-subtypep '(member a b 10 z) '(not (satisfies symbolp)))
  nil)

(deftest subtypep.member.35
  (check-all-not-subtypep  '(satisfies symbolp) '(member a b c d))
  nil)

(deftest subtypep.member.36
  (check-disjointness '(eql a) '(or (member b c d) (eql e)))
  nil)

(deftest subtypep.member.37
  (check-equivalence
   '(and (member a b c d) (not (eql c)))
   '(member a b d))
  nil)

(deftest subtypep.member.38
  (check-equivalence
   '(and (member a b c d e f g)
	 (not (member b f)))
   '(member a c d e g))
  nil)

(deftest subtypep.member.39
  (check-equivalence
   '(and (not (member b d e f g))
	 (not (member x y b z d)))
   '(not (member b d e f g x y z)))
  nil)

(deftest subtypep.member.40
  (check-equivalence
   '(and (not (eql a)) (not (eql b)))
   '(not (member a b)))
  nil)

(deftest subtypep.member.41
  (check-equivalence
   '(and (not (eql a)) (not (eql b)) (not (eql c)))
   '(not (member c b a)))
  nil)

(deftest subtypep.member.42
  (check-equivalence
   '(and (not (member a b)) (not (member b c)))
   '(not (member c b a)))
  nil)

(deftest subtypep.member.43
  (check-equivalence
   '(and (not (member a g b k e)) (not (member b h k c f)))
   '(not (member c b k a e f g h)))
  nil)

(deftest subtypep.member.44
  (check-equivalence
   '(and (integer 0 30) (not (member 3 4 5 9 10 11 17 18 19)))
   '(or (integer 0 2) (integer 6 8) (integer 12 16) (integer 20 30)))
  nil)
