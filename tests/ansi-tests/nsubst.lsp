;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:49:58 2003
;;;; Contains: Tests of NSUBST

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(defvar *nsubst-tree-1* '(10 (30 20 10) (20 10) (10 20 30 40)))

(deftest nsubst.1
  (check-nsubst "Z" 30 (copy-tree *nsubst-tree-1*))
  (10 ("Z" 20 10) (20 10) (10 20 "Z" 40)))

(deftest nsubst.2
  (check-nsubst "A" 0 (copy-tree *nsubst-tree-1*))
  (10 (30 20 10) (20 10) (10 20 30 40)))

(deftest nsubst.3
  (check-nsubst "Z" 100 (copy-tree *nsubst-tree-1*) :test-not #'eql)
  "Z")

(deftest nsubst.4
  (check-nsubst 'grape 'dick
		'(melville wrote (moby dick)))
  (melville wrote (moby grape)))

(deftest nsubst.5
  (check-nsubst 'cha-cha-cha 'nil '(melville wrote (moby dick)))
  (melville wrote (moby dick . cha-cha-cha) . cha-cha-cha))

(deftest nsubst.6
  (check-nsubst
   '(1 2) '(foo . bar)
   '((foo . baz) (foo . bar) (bar . foo) (baz foo . bar))
   :test #'equal)
  ((foo . baz) (1 2) (bar . foo) (baz 1 2)))

(deftest nsubst.7
  (check-nsubst
   'foo "aaa"
   '((1 . 2) (4 . 5) (6 7 8 9 10 (11 12)))
   :key #'(lambda (x) (if (and (numberp x) (evenp x))
			  "aaa"
			nil))
   :test #'string=)
  ((1 . foo) (foo . 5) (foo 7 foo 9 foo (11 foo))))

(deftest nsubst.8
  (check-nsubst
   'foo nil
   '((1 . 2) (4 . 5) (6 7 8 9 10 (11 12)))
   :key #'(lambda (x) (if (and (numberp x) (evenp x))
			  (copy-seq "aaa")
			nil))
   :test-not #'equal)
  ((1 . foo) (foo . 5) (foo 7 foo 9 foo (11 foo))))

(deftest nsubst.9
  (check-nsubst 'a 'b
		(copy-tree '(a b c d a b))
		:key nil)
  (a a c d a a))

(deftest nsubst.10
  (check-nsubst 'x 10 (copy-tree '(1 2 10 20 30 4))
		:test #'(lambda (x y) (and (realp x) (realp y) (< x y))))
  (1 2 10 x x 4))

(deftest nsubst.11
  (check-nsubst 'x 10 (copy-tree '(1 2 10 20 30 4))
		:test-not #'(lambda (x y)
			      (not (and (realp x) (realp y) (< x y)))))
  (1 2 10 x x 4))

(defharmless nsubset.test-and-test-not.1
  (nsubst 'a 'b (list 'a 'b 'c 'd 'e) :test #'eq :test-not #'eq))

(defharmless nsubset.test-and-test-not.2
  (nsubst 'a 'b (list 'a 'b 'c 'd 'e) :test-not #'eq :test #'eq))

;;; Order of argument evaluation
(deftest nsubst.order.1
  (let ((i 0) v w x y z)
    (values
     (nsubst (progn (setf v (incf i)) 'b)
	     (progn (setf w (incf i)) 'a)
	     (progn (setf x (incf i)) (copy-tree '((10 a . a) a b c ((a)) z)))
	     :key (progn (setf y (incf i)) #'identity)
	     :test (progn (setf z (incf i)) #'eql))
     i v w x y z))
  ((10 b . b) b b c ((b)) z)
  5 1 2 3 4 5)

(deftest nsubst.order.2
  (let ((i 0) v w x y z)
    (values
     (nsubst (progn (setf v (incf i)) 'b)
	     (progn (setf w (incf i)) 'a)
	     (progn (setf x (incf i)) (copy-tree '((10 a . a) a b c ((a)) z)))
	     :test-not (progn (setf y (incf i)) (complement #'eql))
	     :key (progn (setf z (incf i)) #'identity)
	     )
     i v w x y z))
  ((10 b . b) b b c ((b)) z)
  5 1 2 3 4 5)

;;; Keyword tests for nsubst

(deftest nsubst.allow-other-keys.1
  (nsubst 'a 'b (list 'a 'b 'c) :bad t :allow-other-keys t)
  (a a c))

(deftest nsubst.allow-other-keys.2
  (nsubst 'a 'b (list 'a 'b 'c) :allow-other-keys t)
  (a a c))

(deftest nsubst.allow-other-keys.3
  (nsubst 'a 'b (list 'a 'b 'c) :allow-other-keys nil)
  (a a c))

(deftest nsubst.allow-other-keys.4
  (nsubst 'a 'b (list 'a 'b 'c) :allow-other-keys t :bad t)
  (a a c))

(deftest nsubst.allow-other-keys.5
  (nsubst 'a 'b (list 'a 'b 'c) :allow-other-keys t :allow-other-keys nil
	 :bad t)
  (a a c))

(deftest nsubst.keywords.6
  (nsubst 'a 'b (list 'a 'b 'c) :test #'eq :test (complement #'eq))
  (a a c))

;;; Error cases

(deftest nsubst.error.1
  (signals-error (nsubst) program-error)
  t)

(deftest nsubst.error.2
  (signals-error (nsubst 'a) program-error)
  t)

(deftest nsubst.error.3
  (signals-error (nsubst 'a 'b) program-error)
  t)

(deftest nsubst.error.4
  (signals-error (nsubst 'a 'b nil :foo nil) program-error)
  t)

(deftest nsubst.error.5
  (signals-error (nsubst 'a 'b nil :test) program-error)
  t)

(deftest nsubst.error.6
  (signals-error (nsubst 'a 'b nil 1) program-error)
  t)

(deftest nsubst.error.7
  (signals-error (nsubst 'a 'b nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest nsubst.error.8
  (signals-error (nsubst 'a 'b (list 'a 'b) :test #'identity) program-error)
  t)

(deftest nsubst.error.9
  (signals-error (nsubst 'a 'b (list 'a 'b) :test-not #'identity) program-error)
  t)

(deftest nsubst.error.10
  (signals-error (nsubst 'a 'b (list 'a 'b) :key #'equal) program-error)
  t)
