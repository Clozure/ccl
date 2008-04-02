;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:28:09 2003
;;;; Contains: Tests of ATOM

(in-package :cl-test)

; (compile-and-load "cons-aux.lsp")

(deftest atom.1
  (loop for x in *universe*
	unless (if (atom x) (not (consp x)) (consp x))
	collect x)
  nil)

(deftest atom.2
  (macrolet ((%m (z) z)) (atom (expand-in-current-env (%m 0))))
  t)

(deftest atom.order.1
  (let ((i 0))
    (values (atom (progn (incf i) '(a b))) i))
  nil 1)

(deftest atom.error.1
  (signals-error (atom) program-error)
  t)

(deftest atom.error.2
  (signals-error (atom 'a 'b) program-error)
  t)
