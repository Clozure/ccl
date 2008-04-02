;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 06:32:46 2002
;;;; Contains: Tests for NIL

(in-package :cl-test)

(deftest nil.1
  (check-predicate #'(lambda (x) (not (subtypep (type-of x) nil))))
  nil)


(deftest nil.2
  (check-predicate #'(lambda (x) (subtypep nil (type-of x))))
  nil)

(deftest nil.3
  (not-mv (constantp nil))
  nil)

(deftest nil.4
  (car nil)
  nil)

(deftest nil.5
  (cdr nil)
  nil)

(deftest nil.6
  (eval nil)
  nil)

(deftest nil.7
  (symbol-value nil)
  nil)

(deftest nil.8
  (eqt nil 'nil)
  t)

;;; NIL is, of course, present in many other files
