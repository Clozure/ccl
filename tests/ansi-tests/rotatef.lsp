;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 15:44:38 2003
;;;; Contains: Tests for ROTATEF

(in-package :cl-test)

(deftest rotatef-order.1
  (let ((x (vector 'a 'b 'c 'd 'e 'f))
	(i 2))
    (values
     (rotatef (aref x (incf i)) (aref x (incf i)))
     x i))
  nil
  #(a b c e d f)
  4)

(deftest rotatef-order.2
  (let ((x (vector 'a 'b 'c 'd 'e 'f))
	(i 2))
    (values
     (rotatef (aref x (incf i)) (aref x (incf i)) (aref x (incf i)))
     x i))
  nil
  #(a b c e f d)
  5)

(deftest rotatef.1
  (let ((x (vector 0 1 2)))
    (values
     (rotatef (aref x (aref x 0)) (aref x (aref x 1)) (aref x (aref x 2)))
     x))
  nil
  #(1 2 0))

(deftest rotatef.2
  (let ((x (vector 0 1 2 3 4 5 6 7 8 9)))
    (values
     (rotatef (aref x (aref x 0))
	      (aref x (aref x 1))
	      (aref x (aref x 2))
	      (aref x (aref x 3))
	      (aref x (aref x 4))
	      (aref x (aref x 5))
	      (aref x (aref x 6))
	      (aref x (aref x 7))
	      (aref x (aref x 8))
	      (aref x (aref x 9)))
     x))
  nil
  #(1 2 3 4 5 6 7 8 9 0))

(deftest rotatef.3
  (rotatef)
  nil)

(deftest rotatef.4
  (let ((x 10))
    (values
     x
     (rotatef x)
     x))
  10 nil 10)

(deftest rotatef.5
  (let ((x 'a) (y 'b))
    (values x y (rotatef x y) x y))
  a b nil b a)
  

;;; ROTATEF is a good testbed for finding conflicts in setf expansions
;;; These tests apply rotatef to various accessors

(deftest rotatef.6
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd))
	 (z 'e))
    (rotatef (car x) (car y) z)
    (values x y z))
  (c b) (e d) a)

(deftest rotatef.7
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd))
	 (z 'e))
    (rotatef (first x) (first y) z)
    (values x y z))
  (c b) (e d) a)

(deftest rotatef.8
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd))
	 (z '(e)))
    (rotatef (cdr x) (cdr y) z)
    (values x y z))
  (a d) (c e) (b))

(deftest rotatef.9
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd))
	 (z '(e)))
    (rotatef (rest x) (rest y) z)
    (values x y z))
  (a d) (c e) (b))

(deftest rotatef.10
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd))
	 (z 'e))
    (rotatef (cadr x) (cadr y) z)
    (values x y z))
  (a d) (c e) b)

(deftest rotatef.11
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd))
	 (z 'e))
    (rotatef (second x) (second y) z)
    (values x y z))
  (a d) (c e) b)

(deftest rotatef.12
  (let* ((x (list 'a 'b 'c))
	 (y (list 'd 'e 'f))
	 (z (list 'g)))
    (rotatef (cddr x) (cddr y) z)
    (values x y z))
  (a b f) (d e g) (c))

(deftest rotatef.13
  (let* ((x (list (list 'a)))
	 (y (list (list 'c)))
	 (z 'e))
    (rotatef (caar x) (caar y) z)
    (values x y z))
  ((c)) ((e)) a)

(deftest rotatef.14
  (let* ((x (list (list 'a 'b)))
	 (y (list (list 'c 'd)))
	 (z (list 'e)))
    (rotatef (cdar x) (cdar y) z)
    (values x y z))
  ((a d)) ((c e)) (b))

;;; TODO: c*r accessors with > 2 a/d
;;; TODO: third,...,tenth

(deftest rotatef.15
  (let* ((x (vector 'a 'b))
	 (y (vector 'c 'd))
	 (z 'e))
    (rotatef (aref x 0) (aref y 0) z)
    (values x y z))
  #(c b) #(e d) a)

(deftest rotatef.16
  (let* ((x (vector 'a 'b))
	 (y (vector 'c 'd))
	 (z 'e))
    (rotatef (svref x 0) (svref y 0) z)
    (values x y z))
  #(c b) #(e d) a)

(deftest rotatef.17
  (let* ((x (copy-seq #*11000))
	 (y (copy-seq #*11100))
	 (z 1))
    (rotatef (bit x 1) (bit y 3) z)
    (values x y z))
  #*10000 #*11110 1)

(deftest rotatef.18
  (let* ((x (copy-seq "abcde"))
	 (y (copy-seq "fghij"))
	 (z #\X))
    (rotatef (char x 1) (char y 2) z)
    (values x y z))
  "ahcde" "fgXij" #\b)

(deftest rotatef.21
  (let* ((x (copy-seq #*11000))
	 (y (copy-seq #*11100))
	 (z 1))
    (rotatef (bit x 1) (bit y 3) z)
    (values x y z))
  #*10000 #*11110 1)  

(deftest rotatef.22
  (let* ((x (copy-seq "abcde"))
	 (y (copy-seq "fghij"))
	 (z #\X))
    (rotatef (char x 1) (char y 2) z)
    (values x y z))
  "ahcde" "fgXij" #\b)

(deftest rotatef.23
  (let* ((x (copy-seq '(a b c d e)))
	 (y (copy-seq '(f g h i j)))
	 (z 'k))
    (rotatef (elt x 1) (elt y 2) z)
    (values x y z))
  (a h c d e) (f g k i j) b)

(deftest rotatef.24
  (let ((x #b01010101)
	(y #b1111)
	(z 0))
    (rotatef (ldb (byte 4 2) x)
	     (ldb (byte 4 1) y)
	     z)
    (values x y z))
  #b01011101
  1
  #b0101)

(deftest rotatef.25
  (let* ((f1 (gensym))
	 (f2 (gensym))
	 (fn1 (constantly :foo))
	 (fn2 (constantly :bar))
	 (fn3 (constantly :zzz)))
    (setf (fdefinition f1) fn1
	  (fdefinition f2) fn2)
    (rotatef (fdefinition f1)
	     (fdefinition f2)
	     fn3)
    (values (funcall f1) (funcall f2) (funcall fn3)))
  :bar :zzz :foo)

(deftest rotatef.26
  (let* ((a1 (make-array '(10) :fill-pointer 5))
	 (a2 (make-array '(20) :fill-pointer 7))
	 (z 3))
    (rotatef (fill-pointer a1) (fill-pointer a2) z)
    (values (fill-pointer a1) (fill-pointer a2) z))
  7 3 5)

(deftest rotatef.27
  (let* ((x (list 'a 'b 'c 'd))
	 (y (list 'd 'e 'f 'g))
	 (n1 1) (n2 2)
	 (z 'h))
    (rotatef (nth n1 x) (nth n2 y) z)
    (values x y z))
  (a f c d)
  (d e h g)
  b)

(deftest rotatef.28
  (let* ((f1 (gensym))
	 (f2 (gensym))
	 (fn1 (constantly :foo))
	 (fn2 (constantly :bar))
	 (fn3 (constantly :zzz)))
    (setf (symbol-function f1) fn1
	  (symbol-function f2) fn2)
    (rotatef (symbol-function f1) (symbol-function f2) fn3)
    (values (funcall f1) (funcall f2) (funcall fn3)))
  :bar :zzz :foo)

(deftest rotatef.29
  (let* ((s1 (gensym))
	 (s2 (gensym))
	 (z 1))
    (setf (symbol-value s1) :foo
	  (symbol-value s2) :bar)
    (rotatef (symbol-value s1)
	     (symbol-value s2)
	     z)
    (values (symbol-value s1) (symbol-value s2) z))
  :bar 1 :foo)

(deftest rotatef.30
  (let* ((s1 (gensym))
	 (s2 (gensym))
	 (v1 (list :foo 1))
	 (v2 (list :bar 2))
	 (z nil))
    (setf (symbol-plist s1) v1
	  (symbol-plist s2) v2)
    (rotatef (symbol-plist s1) (symbol-plist s2) z)
    (values (symbol-plist s1) (symbol-plist s2) z))
  (:bar 2) nil (:foo 1))

(deftest rotatef.31
  (let* ((x (list 'a 'b 'c 'd 'e))
	 (y (list 'f 'g 'h 'i 'j))
	 (p1 1) (p2 2) (len 3)
	 (z '(10 11 12)))
    (rotatef (subseq x p1 (+ p1 len))
	     (subseq y p2 (+ p2 len))
	     z)
    (values x y z))
  (a h i j e)
  (f g 10 11 12)
  (b c d))

(deftest rotatef.32
  (let* ((x (gensym))
	 (y (gensym))
	 (k1 :foo)
	 (k2 :bar)
	 (v1 1)
	 (v2 2)
	 (z 17))
    (setf (get x k1) v1 (get y k2) v2)
    (rotatef (get x k1) (get y k2) z)
    (values (symbol-plist x) (symbol-plist y) z))
  (:foo 2) (:bar 17) 1)

(deftest rotatef.33
  (let* ((x nil)
	 (y nil)
	 (k1 :foo)
	 (k2 :bar)
	 (v1 1)
	 (v2 2)
	 (z 21))
    (setf (getf x k1) v1 (getf y k2) v2)
    (rotatef (getf x k1) (getf y k2) z)
    (values x y z))
  (:foo 2) (:bar 21) 1)

(deftest rotatef.34
  (let* ((ht1 (make-hash-table))
	 (ht2 (make-hash-table))
	 (k1 :foo) (v1 1)
	 (k2 :bar) (v2 2)
	 (z 3))
    (setf (gethash k1 ht1) v1
	  (gethash k2 ht2) v2)
    (rotatef z (gethash k1 ht1) (gethash k2 ht2))
    (values z (gethash k1 ht1) (gethash k2 ht2)))
  1 2 3)

(deftest rotatef.35
  (let ((n1 (gensym))
	(n2 (gensym))
	(n3 (gensym))
	(n4 (gensym)))
    (eval `(defclass ,n1 () ()))
    (eval `(defclass ,n2 () ()))
    (setf (find-class n3) (find-class n1)
	  (find-class n4) (find-class n2))
    (rotatef (find-class n3) (find-class n4))
    (values (eqlt (find-class n1) (find-class n4))
	    (eqlt (find-class n2) (find-class n3))))
  t t)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest rotatef.36
  (macrolet
   ((%m (z) z))
   (let ((x 1) (y 2))
     (rotatef (expand-in-current-env (%m x)) y)
     (values x y)))
  2 1)

(deftest rotatef.37
  (macrolet
   ((%m (z) z))
   (let ((x 1) (y 2))
     (rotatef x (expand-in-current-env (%m y)))
     (values x y)))
  2 1)

;;; TODO: macro-function, mask-field, row-major-aref,
;;;   logical-pathname-translations, readtable-case
