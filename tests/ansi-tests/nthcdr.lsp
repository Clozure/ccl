;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:48:36 2003
;;;; Contains: Tests of NTHCDR

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

;;; Error tests

(deftest nthcdr.error.1
  (check-type-error #'(lambda (x) (nthcdr x (copy-list '(a b c d)))) (typef 'unsigned-byte))
  nil)

(deftest nthcdr.error.6
  (signals-error (nthcdr -10 (copy-tree '(a b c d))) type-error)
  t)

(deftest nthcdr.error.7
  (signals-error (nthcdr) program-error)
  t)

(deftest nthcdr.error.8
  (signals-error (nthcdr 0) program-error)
  t)

(deftest nthcdr.error.9
  (signals-error (nthcdr 0 nil nil) program-error)
  t)

(deftest nthcdr.error.10
  (signals-error (nthcdr 3 (cons 'a 'b)) type-error)
  t)

(deftest nthcdr.error.11
  (signals-error (locally (nthcdr 'a (copy-tree '(a b c d))) t) type-error)
  t)

;;; Non-error tests

(deftest nthcdr.1
  (nthcdr 0 (copy-tree '(a b c d . e)))
  (a b c d . e))

(deftest nthcdr.2
  (nthcdr 1 (copy-tree '(a b c d)))
  (b c d))

(deftest nthcdr.3
  (nthcdr 10 nil)
  nil)

(deftest nthcdr.4
  (nthcdr 4 (list 'a 'b 'c))
  nil)

(deftest nthcdr.5
  (nthcdr 1 (cons 'a 'b))
  b)

(deftest nthcdr.order.1
  (let ((i 0) x y)
    (values
     (nthcdr (setf x (incf i))
	     (progn (setf y (incf i)) '(a b c d)))
     i x y))
  (b c d) 2 1 2)

