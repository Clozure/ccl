;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:27:18 2003
;;;; Contains: Tests of POP

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest pop.1
  (let ((x (copy-tree '(a b c))))
    (let ((y (pop x)))
      (list x y)))
  ((b c) a))

(deftest pop.2
  (let ((x nil))
    (let ((y (pop x)))
      (list x y)))
  (nil nil))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest pop.3
  (macrolet
   ((%m (z) z))
   (let ((x (list 'a 'b 'c)))
     (values
      (pop (expand-in-current-env (%m x)))
      x)))
  a (b c))

;;; Confirm argument is executed just once.
(deftest pop.order.1
  (let ((i 0)
	(a (vector (list 'a 'b 'c))))
    (pop (aref a (progn (incf i) 0)))
    (values a i))
  #((b c)) 1)

(deftest push-and-pop
  (let* ((x (copy-tree '(a b)))
	 (y x))
    (push 'c x)
    (and
     (eqt (cdr x) y)
     (pop x)))
  c)

(def-macro-test pop.error.1 (pop x))

;;; Need to add tests of POP vs. various accessors
