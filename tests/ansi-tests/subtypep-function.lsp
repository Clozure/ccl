;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Dec 15 21:57:44 2004
;;;; Contains: Tests of SUBTYPEP on FUNCTION types

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

(deftest subtypep-function.1
  (check-all-not-subtypep t '(function (t) t))
  nil)

(deftest subtypep-function.2
  (check-all-subtypep nil '(function (t) t))
  nil)

(deftest subtypep-function.3
  (check-all-subtypep '(function (t) t) 'function)
  nil)

(deftest subtypep-function.4
  (check-all-subtypep '(function (t) integer) '(function (t) real))
  nil)
