;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 15:38:30 2003
;;;; Contains: Tests of PSETF

(in-package :cl-test)

(deftest psetf.order.1
  (let ((x (vector nil nil nil nil))
	(i 0))
    (psetf (aref x (incf i)) (incf i))
    (values x i))
  #(nil 2 nil nil) 2)

(deftest psetf.order.2
  (let ((x (vector nil nil nil nil))
	(i 0))
    (psetf (aref x (incf i)) (incf i)
	   (aref x (incf i)) (incf i 10))
    (values x i))
  #(nil 2 nil 13) 13)

(deftest psetf.1
  (psetf)
  nil)

(deftest psetf.2
  (let ((x 0))
    (values (psetf x 1) x))
  nil 1)

(deftest psetf.3
  (let ((x 0) (y 1))
    (values (psetf x y y x) x y))
  nil 1 0)

(deftest psetf.4
  (let ((x 0))
    (values
     (symbol-macrolet ((x y))
       (let ((y 1))
	 (psetf x 2)
	 y))
     x))
  2 0)

(deftest psetf.5
  (let ((w (list nil)))
    (values
     (symbol-macrolet ((x (car w)))
       (psetf x 2))
     w))
  nil (2))

(deftest psetf.6
  (let ((c 0) x y)
    (psetf x (incf c)
	   y (incf c))
    (values c x y))
  2 1 2)

;;; According to the standard, the forms to be assigned and
;;; the subforms in the places to be assigned to are evaluated
;;; from left to right.  Therefore, PSETF.7 and PSETF.8 should
;;; do the same thing to A as PSETF.9 does.
;;; (See the page for PSETF)

(deftest psetf.7
  (symbol-macrolet ((x (aref a (incf i)))
		    (y (aref a (incf i))))
    (let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9)))
	  (i 0))
      (psetf x (aref a (incf i))
	     y (aref a (incf i)))
      (values a i)))
  #(0 2 2 4 4 5 6 7 8 9)
  4)

(deftest psetf.8
  (let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9)))
	(i 0))
    (psetf (aref a (incf i)) (aref a (incf i))
	   (aref a (incf i)) (aref a (incf i)))
    (values a i))
  #(0 2 2 4 4 5 6 7 8 9)
  4)

(deftest psetf.9
  (let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9))))
    (psetf (aref a 1) (aref a 2)
	   (aref a 3) (aref a 4))
    a)
  #(0 2 2 4 4 5 6 7 8 9))

(deftest psetf.10
  (let ((*x* 0) (*y* 10))
    (declare (special *x* *y*))
    (values
     *x* *y*
     (psetf *x* 6
	    *y* 15)
     *x* *y*))
  0 10 nil 6 15)

(deftest psetf.11
  (let ((*x* 0) (*y* 10))
    (declare (special *x* *y*))
    (values
     *x* *y*
     (psetf *x* *y*
	    *y* *x*)
     *x* *y*))
  0 10 nil 10 0)

(def-macro-test psetf.error.1 (psetf))

;;; PSETF is a good testbed for finding conflicts in setf expansions
;;; These tests apply psetf to various accessors

(deftest psetf.12
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd)))
    (psetf (car x) 1 (car y) 2)
    (values x y))
  (1 b) (2 d))

(deftest psetf.12a
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd)))
    (psetf (first x) 1 (first y) 2)
    (values x y))
  (1 b) (2 d))

(deftest psetf.13
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd)))
    (psetf (cdr x) 1 (cdr y) 2)
    (values x y))
  (a . 1) (c . 2))

(deftest psetf.13a
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd)))
    (psetf (rest x) 1 (rest y) 2)
    (values x y))
  (a . 1) (c . 2))

(deftest psetf.14
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd)))
    (psetf (cadr x) 1 (cadr y) 2)
    (values x y))
  (a 1) (c 2))

(deftest psetf.15
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd)))
    (psetf (cddr x) 1 (cddr y) 2)
    (values x y))
  (a b . 1) (c d . 2))

(deftest psetf.16
  (let* ((x (list (list 'a)))
	 (y (list (list 'c))))
    (psetf (caar x) 1 (caar y) 2)
    (values x y))
  ((1)) ((2)))

(deftest psetf.17
  (let* ((x (list (list 'a)))
	 (y (list (list 'c))))
    (psetf (cdar x) 1 (cdar y) 2)
    (values x y))
  ((a . 1)) ((c . 2)))

;;; TODO: c*r accessors with > 2 a/d
;;; TODO: third,...,tenth

(deftest psetf.18
  (let* ((x (vector 'a 'b))
	 (y (vector 'c 'd)))
    (psetf (aref x 0) 1 (aref y 0) 2)
    (values x y))
  #(1 b) #(2 d))

(deftest psetf.18a
  (let* ((x (vector 'a 'b))
	 (y (vector 'c 'd)))
    (psetf (svref x 0) 1 (svref y 0) 2)
    (values x y))
  #(1 b) #(2 d))

(deftest psetf.19
  (let* ((x (copy-seq #*11000))
	 (y (copy-seq #*11100)))
    (psetf (bit x 1) 0 (bit x 2) 1 (bit y 4) 1 (bit y 0) 0)
    (values x y))
  #*10100 #*01101)

(deftest psetf.20
  (let* ((x (copy-seq "abcde"))
	 (y (copy-seq "fghij")))
    (psetf (char x 1) #\X (char y 2) #\Y)
    (values x y))
  "aXcde" "fgYij")

(deftest psetf.21
  (let* ((x (copy-seq #*11000))
	 (y (copy-seq #*11100)))
    (psetf (sbit x 1) 0 (sbit x 2) 1 (sbit y 4) 1 (sbit y 0) 0)
    (values x y))
  #*10100 #*01101)

(deftest psetf.22
  (let* ((x (copy-seq "abcde"))
	 (y (copy-seq "fghij")))
    (psetf (schar x 1) #\X (schar y 2) #\Y)
    (values x y))
  "aXcde" "fgYij")

(deftest psetf.23
  (let* ((x (copy-seq '(a b c d e)))
	 (y (copy-seq '(f g h i j))))
    (psetf (elt x 1) 'u (elt y 2) 'v)
    (values x y))
  (a u c d e) (f g v i j))

(deftest psetf.24
  (let ((x #b110110001)
	(y #b101001100))
    (psetf (ldb (byte 5 1) x) #b10110
	   (ldb (byte 3 6) y) #b10)
    (values x y))
  #b110101101
  #b010001100)

(deftest psetf.25
  (let* ((f1 (gensym))
	 (f2 (gensym))
	 (fn1 (constantly :foo))
	 (fn2 (constantly :bar)))
    (psetf (fdefinition f1) fn1
	   (fdefinition f2) fn2)
    (values (funcall f1) (funcall f2)))
  :foo :bar)

(deftest psetf.26
  (let* ((a1 (make-array '(10) :fill-pointer 5))
	 (a2 (make-array '(20) :fill-pointer 7)))
    (psetf (fill-pointer a1) (1+ (fill-pointer a2))
	   (fill-pointer a2) (1- (fill-pointer a1)))
    (values (fill-pointer a1) (fill-pointer a2)))
  8 4)

(deftest psetf.27
  (let* ((x (list 'a 'b 'c 'd))
	 (y (list 'd 'e 'f 'g))
	 (n1 1) (n2 2)
	 (v1 :foo) (v2 :bar))
    (psetf (nth n1 x) v1
	   (nth n2 y) v2)
    (values x y))
  (a :foo c d)
  (d e :bar g))

(deftest psetf.28
  (let* ((f1 (gensym))
	 (f2 (gensym))
	 (fn1 (constantly :foo))
	 (fn2 (constantly :bar)))
    (psetf (symbol-function f1) fn1
	   (symbol-function f2) fn2)
    (values (funcall f1) (funcall f2)))
  :foo :bar)

(deftest psetf.29
  (let* ((s1 (gensym))
	 (s2 (gensym))
	 (v1 :foo)
	 (v2 :bar))
    (psetf (symbol-value s1) v1
	   (symbol-value s2) v2)
    (values (symbol-value s1) (symbol-value s2)))
  :foo :bar)

(deftest psetf.30
  (let* ((s1 (gensym))
	 (s2 (gensym))
	 (v1 (list :foo 1))
	 (v2 (list :bar 2)))
    (psetf (symbol-plist s1) v1
	   (symbol-plist s2) v2)
    (values (symbol-plist s1) (symbol-plist s2)))
  (:foo 1) (:bar 2))

(deftest psetf.31
  (let* ((x (list 'a 'b 'c 'd 'e))
	 (y (list 'f 'g 'h 'i 'j))
	 (v1 (list 1 2))
	 (v2 (list 3 4 5))
	 (p1 1) (p2 2)
	 (l1 (length v1))
	 (l2 (length v2)))
    (psetf (subseq x p1 (+ p1 l1)) v1
	   (subseq y p2 (+ p2 l2)) v2)
    (values x y))
  (a 1 2 d e)
  (f g 3 4 5))

(deftest psetf.32
  (let* ((x (gensym))
	 (y (gensym))
	 (k1 :foo)
	 (k2 :bar)
	 (v1 1)
	 (v2 2))
    (psetf (get x k1) v1 (get y k2) v2)
    (values (symbol-plist x) (symbol-plist y)))
  (:foo 1) (:bar 2))

(deftest psetf.33
  (let* ((x nil)
	 (y nil)
	 (k1 :foo)
	 (k2 :bar)
	 (v1 1)
	 (v2 2))
    (psetf (getf x k1) v1 (getf y k2) v2)
    (values x y))
  (:foo 1) (:bar 2))

(deftest psetf.34
  (let* ((ht1 (make-hash-table))
	 (ht2 (make-hash-table))
	 (k1 :foo) (v1 1)
	 (k2 :bar) (v2 2))
    (psetf (gethash k1 ht1) v1
	   (gethash k2 ht2) v2)
    (values (gethash k1 ht1) (gethash k2 ht2)))
  1 2)

(deftest psetf.35
  (let ((n1 (gensym))
	(n2 (gensym))
	(n3 (gensym))
	(n4 (gensym)))
    (eval `(defclass ,n1 () ()))
    (eval `(defclass ,n2 () ()))
    (psetf (find-class n3) (find-class n1)
	   (find-class n4) (find-class n2))
    (values (eqlt (find-class n1) (find-class n3))
	    (eqlt (find-class n2) (find-class n4))))
  t t)

(deftest psetf.36
  (let ((fn1 (constantly :foo))
	(fn2 (constantly :bar))
	(n1 (gensym))
	(n2 (gensym)))
    (psetf (macro-function n1) fn1
	   (macro-function n2) fn2)
    (values (eval `(,n1)) (eval `(,n2))))
  :foo :bar)

(deftest psetf.37
  (let ((b1 (byte 3 1))
	(b2 (byte 4 2))
	(x #b1100101011010101)
	(y #b11010101000110)
	(m1 #b101010101101101)
	(m2 #b11110010110101))
    (psetf (mask-field b1 x) m1
	   (mask-field b2 y) m2)
    (values x y))
  #b1100101011011101
  #b11010101110110)

(deftest psetf.38
  (let* ((a1 (make-array '(2 3) :initial-contents '((a b c)(d e f))))
	 (a2 (make-array '(3 4) :initial-contents
			 '((1 2 3 4) (5 6 7 8) (9 10 11 12))))
	 (i1 2) (i2 5)
	 (v1 'u) (v2 'v))
    (psetf (row-major-aref a1 i1) v1
	   (row-major-aref a2 i2) v2)
    (values a1 a2))
  #2a((a b u)(d e f))
  #2a((1 2 3 4)(5 v 7 8)(9 10 11 12)))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest psetf.39
  (macrolet
   ((%m (z) z))
   (let ((x 1) (y 2))
     (values
      (psetf (expand-in-current-env (%m x)) y
	     y x)
      x y)))
  nil 2 1)

(deftest psetf.40
  (macrolet
   ((%m (z) z))
   (let ((x 1) (y 2))
     (values
      (psetf x (expand-in-current-env (%m y))
	     y x)
      x y)))
  nil 2 1)

;;; TODO: logical-pathname-translations, readtable-case
