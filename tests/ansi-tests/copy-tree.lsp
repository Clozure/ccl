;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:31:33 2003
;;;; Contains: Tests of COPY-TREE

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

;; Try copy-tree on a tree containing elements of various kinds
(deftest copy-tree.1
  (let* ((x (cons 'a (list
		     (cons 'b 'c)
		     (cons 1 1.2)
		     (list (list "abcde"
				 (make-array '(10) :initial-element
					     (cons 'e 'f)))
				'g))))
	 (y (copy-tree x)))
    (check-cons-copy x y))
  t)

;; Try copy-tree on *universe*
(deftest copy-tree.2
  (let* ((x (copy-list *universe*))
	 (y (copy-tree x)))
    (check-cons-copy x y))
  t)

(deftest copy-tree.order.1
  (let ((i 0))
    (values
     (copy-tree (progn (incf i) '(a b c)))
     i))
  (a b c) 1)

(def-fold-test copy-tree.fold.1 (copy-tree '(a . b)))
(def-fold-test copy-tree.fold.2 (copy-tree '(a)))
(def-fold-test copy-tree.fold.3 (copy-tree '(a b c d e)))

;;; Error tests

(deftest copy-tree.error.1
  (signals-error (copy-tree) program-error)
  t)

(deftest copy-tree.error.2
  (signals-error (copy-tree 'a 'b) program-error)
  t)
