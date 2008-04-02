;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:33:20 1998
;;;; Contains:  Tests of ADJOIN

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest adjoin.1
  (adjoin 'a nil)
  (a))

(deftest adjoin.2
  (adjoin nil nil)
  (nil))

(deftest adjoin.3
  (adjoin 'a '(a))
  (a))

;; Check that a NIL :key argument is the same as no key argument at all
(deftest adjoin.4
  (adjoin 'a '(a) :key nil)
  (a))

(deftest adjoin.5
  (adjoin 'a '(a) :key #'identity)
  (a))

(deftest adjoin.6
  (adjoin 'a '(a) :key 'identity)
  (a))

(deftest adjoin.7
  (adjoin (1+ 11) '(4 3 12 2 1))
  (4 3 12 2 1))

;; Check that the test is EQL, not EQ (by adjoining a bignum)
(deftest adjoin.8
  (adjoin (1+ 999999999999) '(4 1 1000000000000 3816734 a "aa"))
  (4 1 1000000000000 3816734 a "aa"))

(deftest adjoin.9
  (adjoin (copy-seq "aaa") '(aaa "AAA" "aaa" #\a))
  ("aaa" aaa "AAA" "aaa" #\a))

(deftest adjoin.10
  (adjoin (copy-seq "aaa") '(aaa "AAA" "aaa" #\a) :test #'equal)
  (aaa "AAA" "aaa" #\a))

(deftest adjoin.11
  (adjoin (copy-seq "aaa") '(aaa "AAA" "aaa" #\a) :test 'equal)
  (aaa "AAA" "aaa" #\a))

(deftest adjoin.12
  (adjoin (copy-seq "aaa") '(aaa "AAA" "aaa" #\a)
	  :test-not (complement #'equal))
  (aaa "AAA" "aaa" #\a))

(deftest adjoin.14
  (adjoin (copy-seq "aaa") '(aaa "AAA" "aaa" #\a)
	  :test #'equal :key #'identity)
  (aaa "AAA" "aaa" #\a))

(deftest adjoin.15
  (adjoin (copy-seq "aaa") '(aaa "AAA" "aaa" #\a)
	  :test 'equal :key #'identity)
  (aaa "AAA" "aaa" #\a))

;; Test that a :key of NIL is the same as no key at all
(deftest adjoin.16
  (adjoin (copy-seq "aaa") '(aaa "AAA" "aaa" #\a)
	  :test #'equal :key nil)
  (aaa "AAA" "aaa" #\a))

;; Test that a :key of NIL is the same as no key at all
(deftest adjoin.17
  (adjoin (copy-seq "aaa") '(aaa "AAA" "aaa" #\a)
	  :test 'equal :key nil)
  (aaa "AAA" "aaa" #\a))

;; Test that a :key of NIL is the same as no key at all
(deftest adjoin.18
  (adjoin (copy-seq "aaa") '(aaa "AAA" "aaa" #\a)
	  :test-not (complement #'equal) :key nil)
  (aaa "AAA" "aaa" #\a))

;;; Ordering in comparison function

(deftest adjoin.19
  (adjoin 10 '(1 2 3) :test #'<)
  (10 1 2 3))

(deftest adjoin.20
  (adjoin 10 '(1 2 3) :test #'>)
  (1 2 3))

(deftest adjoin.21
  (adjoin 10 '(1 2 3) :test-not #'>)
  (10 1 2 3))

(deftest adjoin.22
  (adjoin 10 '(1 2 3) :test-not #'<)
  (1 2 3))

;;; Test that :key satisfies the description in 17.2.1
;;; This contradicts other parts of the spec, particularly
;;; PUSHNEW, so the test is commented out.
;;; (deftest adjoin.23
;;;  (adjoin 1 '(1 2 3) :key '1+)
;;;  (1 1 2 3))

(deftest adjoin.24
  (macrolet ((%m (z) z))
	    (values
	     (adjoin (expand-in-current-env (%m 'a)) '(b c))
	     (adjoin 'a (expand-in-current-env (%m '(b c))))
	     (adjoin 'a '(b c) (expand-in-current-env (%m :test)) 'eql)
	     (adjoin 'a '(a a) (expand-in-current-env (%m :test-not)) 'eql)
	     (adjoin 'a '(b c) :test (expand-in-current-env (%m 'eql)))
	     (adjoin 'a '(b c) :test (expand-in-current-env (%m #'eql)))
	     (adjoin 1 '(1 2 3) :key (expand-in-current-env (%m 'identity)))
	     ))
  (a b c)
  (a b c)
  (a b c)
  (a a a)
  (a b c)
  (a b c)
  (1 2 3))

(defharmless adjoin.test-and-test-not.1
  (adjoin 'a '(b c) :test #'eql :test-not #'eql))

(defharmless adjoin.test-and-test-not.2
  (adjoin 'a '(b c) :test-not #'eql :test #'eql))

(deftest adjoin.order.1
  (let ((i 0) w x y z)
    (values
     (adjoin (progn (setf w (incf i)) 'a)
	     (progn (setf x (incf i)) '(b c d a e))
	     :key (progn (setf y (incf i)) #'identity)
	     :test (progn (setf z (incf i)) #'eql))
     i w x y z))
  (b c d a e)
  4 1 2 3 4)

(deftest adjoin.order.2
  (let ((i 0) w x y z p)
    (values
     (adjoin (progn (setf w (incf i)) 'a)
	     (progn (setf x (incf i)) '(b c d e))
	     :test-not (progn (setf y (incf i)) (complement #'eql))
	     :key (progn (setf z (incf i)) #'identity)
	     :key (progn (setf p (incf i)) nil))
     i w x y z p))
  (a b c d e)
  5 1 2 3 4 5)

(def-fold-test adjoin.fold.1 (adjoin 'x '(a b c nil d)))

(deftest adjoin.allow-other-keys.1
  (adjoin 'a '(b c) :bad t :allow-other-keys t)
  (a b c))

(deftest adjoin.allow-other-keys.2
  (adjoin 'a '(b c) :allow-other-keys t :foo t)
  (a b c))

(deftest adjoin.allow-other-keys.3
  (adjoin 'a '(b c) :allow-other-keys t)
  (a b c))

(deftest adjoin.allow-other-keys.4
  (adjoin 'a '(b c) :allow-other-keys nil)
  (a b c))

(deftest adjoin.allow-other-keys.5
  (adjoin 'a '(b c) :allow-other-keys t :allow-other-keys nil 'bad t)
  (a b c))

(deftest adjoin.repeat-key
  (adjoin 'a '(b c) :test #'eq :test (complement #'eq))
  (a b c))

(deftest adjoin.error.1
  (signals-error (adjoin) program-error)
  t)

(deftest adjoin.error.2
  (signals-error (adjoin 'a) program-error)
  t)

(deftest adjoin.error.3
  (signals-error (adjoin 'a '(b c) :bad t) program-error)
  t)

(deftest adjoin.error.4
  (signals-error (adjoin 'a '(b c) :allow-other-keys nil :bad t) program-error)
  t)

(deftest adjoin.error.5
  (signals-error (adjoin 'a '(b c) 1 2) program-error)
  t)

(deftest adjoin.error.6
  (signals-error (adjoin 'a '(b c) :test) program-error)
  t)

(deftest adjoin.error.7
  (signals-error (adjoin 'a '(b c) :test #'identity) program-error)
  t)

(deftest adjoin.error.8
  (signals-error (adjoin 'a '(b c) :test-not #'identity) program-error)
  t)

(deftest adjoin.error.9
  (signals-error (adjoin 'a '(b c) :key #'cons) program-error)
  t)

(deftest adjoin.error.10
  (signals-error (adjoin 'a (list* 'b 'c 'd)) type-error)
  t)
