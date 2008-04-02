;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:49:14 2003
;;;; Contains: Tests of REST

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest rest.1
  (rest (list 'a 'b 'c))
  (b c))

(deftest rest.order.1
  (let ((i 0))
    (values (rest (progn (incf i) '(a b))) i))
  (b) 1)

(deftest rest.error.1
  (signals-error (rest) program-error)
  t)

(deftest rest.error.2
  (signals-error (rest nil nil) program-error)
  t)


