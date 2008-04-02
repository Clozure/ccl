;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:05:34 2003
;;;; Contains: Tests of PUSH

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

;;; See also places.lsp

(deftest push.1
  (let ((x nil))
    (push 'a x))
  (a))

(deftest push.2
  (let ((x 'b))
    (push 'a x)
    (push 'c x))
  (c a . b))

(deftest push.3
  (let ((x (copy-tree '(a))))
    (push x x)
    (and
     (eqt (car x) (cdr x))
     x))
  ((a) a))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest push.4
  (macrolet
   ((%m (z) z))
   (let ((x nil))
     (values
      (push (expand-in-current-env (%m 1)) x)
      x)))
  (1) (1))

(deftest push.5
  (macrolet
   ((%m (z) z))
   (let ((x nil))
     (values
      (push 1 (expand-in-current-env (%m x)))
      x)))
  (1) (1))

(deftest push.order.1
  (let ((x (list nil)) (i 0) a b)
    (values
     (push (progn (setf a (incf i)) 'z)
	   (car (progn (setf b (incf i)) x)))
     x
     i a b))
  (z) ((z)) 2 1 2)

(deftest push.order.2
  (let ((x (vector nil nil nil nil))
	(y (vector 'a 'b 'c 'd))
	(i 1))
    (push (aref y (incf i)) (aref x (incf i)))
    (values x y i))
  #(nil nil nil (c))
  #(a b c d)
  3)

(deftest push.order.3
  (let ((x '(a b c)))
    (values
     (push (progn (setq x '(d e)) 'z) x)
     x))
  (z d e) (z d e))

(def-macro-test push.error.1 (push x y))

;;; Need to add push vs. various accessors
