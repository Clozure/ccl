;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:27:16 2003
;;;; Contains: Tests of CONSP

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

;; Lists satisfy consp
(deftest consp-list
  (notnot-mv (consp '(a)))
  t)

;; cons satisfies consp
(deftest consp-cons
  (notnot-mv (consp (cons nil nil)))
  t)

;; nil is not a consp
(deftest consp-nil
  (consp nil)
  nil)

;; The empty list is not a cons
(deftest consp-empty-list
  (consp (list))
  nil)

;; A single element list is a cons
(deftest consp-single-element-list
  (notnot-mv (consp (list 'a)))
  t)

;; For everything in *universe*, it is either an atom, or satisfies
;; consp, but not both
(deftest consp-xor-atom-universe
  (check-predicate #'(lambda (x) (or (and (consp x) (not (atom x)))
				     (and (not (consp x)) (atom x)))))
  nil)

;; Everything in type cons satisfies consp, and vice versa
(deftest consp-cons-universe
  (check-type-predicate 'consp 'cons)
  nil)

(deftest consp.order.1
  (let ((i 0))
    (values (consp (incf i)) i))
  nil 1)

;;; Error tests

(deftest consp.error.1
  (signals-error (consp) program-error)
  t)

(deftest consp.error.2
  (signals-error (consp 'a 'b) program-error)
  t)
