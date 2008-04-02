;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jun  3 06:25:52 2004
;;;; Contains: Tests of initial values of printer control variables

(in-package :cl-test)

(deftest print-base.init.1
  *print-base*
  10)

(deftest print-radix.init.1
  *print-radix*
  nil)

(deftest print-case.init.1
  *print-case*
  :upcase)

(deftest print-circle.init.1
  *print-circle*
  nil)

(deftest print-escape.init.1
  (notnot *print-escape*)
  t)

(deftest print-gensym.init.1
  (notnot *print-gensym*)
  t)

(deftest print-level.init.1
  *print-level*
  nil)

(deftest print-length.init.1
  *print-length*
  nil)

(deftest print-lines.init.1
  *print-lines*
  nil)

(deftest print-readably.init.1
  *print-readably*
  nil)

(deftest print-right-margin.init.1
  *print-right-margin*
  nil)


