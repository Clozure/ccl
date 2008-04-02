;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:37:56 2003
;;;; Contains: Tests of SUBST

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(defvar *subst-tree-1* '(10 (30 20 10) (20 10) (10 20 30 40)))

(deftest subst.1
  (check-subst "Z" 30 (copy-tree *subst-tree-1*))
  (10 ("Z" 20 10) (20 10) (10 20 "Z" 40)))

(deftest subst.2
  (check-subst "A" 0 (copy-tree *subst-tree-1*))
  (10 (30 20 10) (20 10) (10 20 30 40)))

(deftest subst.3
  (check-subst "Z" 100 (copy-tree *subst-tree-1*) :test-not #'eql)
  "Z")

(deftest subst.4
  (check-subst 'grape 'dick
	       '(melville wrote (moby dick)))
  (melville wrote (moby grape)))

(deftest subst.5
  (check-subst 'cha-cha-cha 'nil '(melville wrote (moby dick)))
  (melville wrote (moby dick . cha-cha-cha) . cha-cha-cha))

(deftest subst.6
  (check-subst
   '(1 2) '(foo . bar)
   '((foo . baz) (foo . bar) (bar . foo) (baz foo . bar))
   :test #'equal)
  ((foo . baz) (1 2) (bar . foo) (baz 1 2)))

(deftest subst.7
  (check-subst
   'foo "aaa"
   '((1 . 2) (4 . 5) (6 7 8 9 10 (11 12)))
   :key #'(lambda (x) (if (and (numberp x) (evenp x))
			  "aaa"
			nil))
   :test #'string=)
  ((1 . foo) (foo . 5) (foo 7 foo 9 foo (11 foo))))

(deftest subst.8
  (check-subst
   'foo nil
   '((1 . 2) (4 . 5) (6 7 8 9 10 (11 12)))
   :key #'(lambda (x) (if (and (numberp x) (evenp x))
			  (copy-seq "aaa")
			nil))
   :test-not #'equal)
  ((1 . foo) (foo . 5) (foo 7 foo 9 foo (11 foo))))

(deftest subst.9
  (check-subst 'a 'b
	       (copy-tree '(a b c d a b))
	       :key nil)
  (a a c d a a))

(deftest subst.10
  (check-subst 'x 10 (copy-tree '(1 2 10 20 30 4))
	       :test #'(lambda (x y) (and (realp x) (realp y) (< x y))))
  (1 2 10 x x 4))

(deftest subst.11
  (check-subst 'x 10 (copy-tree '(1 2 10 20 30 4))
	       :test-not #'(lambda (x y)
			     (not (and (realp x) (realp y) (< x y)))))
  (1 2 10 x x 4))

(defharmless subset.test-and-test-not.1
  (subst 'a 'b (list 'a 'b 'c 'd 'e) :test #'eq :test-not #'eq))

(defharmless subset.test-and-test-not.2
  (subst 'a 'b (list 'a 'b 'c 'd 'e) :test-not #'eq :test #'eq))


;;; Order of argument evaluation
(deftest subst.order.1
  (let ((i 0) v w x y z)
    (values
     (subst (progn (setf v (incf i)) 'b)
	    (progn (setf w (incf i)) 'a)
	    (progn (setf x (incf i)) (copy-tree '((10 a . a) a b c ((a)) z)))
	    :key (progn (setf y (incf i)) #'identity)
	    :test (progn (setf z (incf i)) #'eql))
     i v w x y z))
  ((10 b . b) b b c ((b)) z)
  5 1 2 3 4 5)

(deftest subst.order.2
  (let ((i 0) v w x y z)
    (values
     (subst (progn (setf v (incf i)) 'b)
	    (progn (setf w (incf i)) 'a)
	    (progn (setf x (incf i)) (copy-tree '((10 a . a) a b c ((a)) z)))
	    :test-not (progn (setf y (incf i)) (complement #'eql))
	    :key (progn (setf z (incf i)) #'identity)
	    )
     i v w x y z))
  ((10 b . b) b b c ((b)) z)
  5 1 2 3 4 5)

;;; Const fold tests

(def-fold-test subst.fold.1 (subst 'a 'b '(a b c (a . b) . a)))

;;; Keyword tests for subst

(deftest subst.allow-other-keys.1
  (subst 'a 'b (list 'a 'b 'c) :bad t :allow-other-keys t)
  (a a c))

(deftest subst.allow-other-keys.2
  (subst 'a 'b (list 'a 'b 'c) :allow-other-keys t)
  (a a c))

(deftest subst.allow-other-keys.3
  (subst 'a 'b (list 'a 'b 'c) :allow-other-keys nil)
  (a a c))

(deftest subst.allow-other-keys.4
  (subst 'a 'b (list 'a 'b 'c) :allow-other-keys t :bad t)
  (a a c))

(deftest subst.allow-other-keys.5
  (subst 'a 'b (list 'a 'b 'c) :allow-other-keys t :allow-other-keys nil
	 :bad t)
  (a a c))

(deftest subst.keywords.6
  (subst 'a 'b (list 'a 'b 'c) :test #'eq :test (complement #'eq))
  (a a c))

(deftest subst.error.1
  (signals-error (subst) program-error)
  t)

(deftest subst.error.2
  (signals-error (subst 'a) program-error)
  t)

(deftest subst.error.3
  (signals-error (subst 'a 'b) program-error)
  t)

(deftest subst.error.4
  (signals-error (subst 'a 'b nil :foo nil) program-error)
  t)

(deftest subst.error.5
  (signals-error (subst 'a 'b nil :test) program-error)
  t)

(deftest subst.error.6
  (signals-error (subst 'a 'b nil 1) program-error)
  t)

(deftest subst.error.7
  (signals-error (subst 'a 'b nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest subst.error.8
  (signals-error (subst 'a 'b (list 'a 'b) :test #'identity) program-error)
  t)

(deftest subst.error.9
  (signals-error (subst 'a 'b (list 'a 'b) :test-not #'identity) program-error)
  t)

(deftest subst.error.10
  (signals-error (subst 'a 'b (list 'a 'b) :key #'equal) program-error)
  t)
