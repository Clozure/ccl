;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 05:12:13 2004
;;;; Contains: Tests of WITH-STANDARD-IO-SYNTAX

(in-package :cl-test)

(deftest with-standard-io-syntax.1
  (let ((*package* (find-package :cl-test)))
    (with-standard-io-syntax
     (eqlt *package* (find-package "CL-USER"))))
  t)

(deftest with-standard-io-syntax.2
  (let ((*print-array* nil))
    (with-standard-io-syntax *print-array*))
  t)

(deftest with-standard-io-syntax.3
  (let ((*print-base* 8))
    (with-standard-io-syntax *print-base*))
  10)

(deftest with-standard-io-syntax.4
  (let ((*print-case* :downcase))
    (with-standard-io-syntax *print-case*))
  :upcase)

(deftest with-standard-io-syntax.5
  (let ((*print-circle* t))
    (with-standard-io-syntax *print-circle*))
  nil)

(deftest with-standard-io-syntax.6
  (let ((*print-escape* nil))
    (with-standard-io-syntax *print-escape*))
  t)

(deftest with-standard-io-syntax.7
  (let ((*print-gensym* nil))
    (with-standard-io-syntax *print-gensym*))
  t)

(deftest with-standard-io-syntax.8
  (let ((*print-length* 100))
    (with-standard-io-syntax *print-length*))
  nil)

(deftest with-standard-io-syntax.9
  (let ((*print-level* 100))
    (with-standard-io-syntax *print-level*))
  nil)

(deftest with-standard-io-syntax.10
  (let ((*print-lines* 100))
    (with-standard-io-syntax *print-lines*))
  nil)

(deftest with-standard-io-syntax.11
  (let ((*print-miser-width* 100))
    (with-standard-io-syntax *print-miser-width*))
  nil)

(deftest with-standard-io-syntax.12
  (let ((*print-pretty* t))
    (with-standard-io-syntax *print-pretty*))
  nil)

(deftest with-standard-io-syntax.13
  (let ((*print-right-margin* 100))
    (with-standard-io-syntax *print-right-margin*))
  nil)

(deftest with-standard-io-syntax.14
  (let ((*read-base* 8))
    (with-standard-io-syntax *read-base*))
  10)

(deftest with-standard-io-syntax.15
  (let ((*read-default-float-format 'long-float))
    (with-standard-io-syntax *read-default-float-format*))
  single-float)

(deftest with-standard-io-syntax.16
  (let ((*read-eval* nil))
    (with-standard-io-syntax *read-eval*))
  t)

(deftest with-standard-io-syntax.17
  (let ((*read-suppress* t))
    (with-standard-io-syntax *read-suppress*))
  nil)

(deftest with-standard-io-syntax.18
  (with-standard-io-syntax (notnot-mv (readtablep *readtable*)))
  t)

(deftest with-standard-io-syntax.19
  (with-standard-io-syntax)
  nil)

(deftest with-standard-io-syntax.20
  (with-standard-io-syntax (values 'a 'b 'c))
  a b c)

(deftest with-standard-io-syntax.21
  (block done
    (tagbody
     (with-standard-io-syntax (go 10) 10 (return-from done :bad))
     10
     (return-from done :good)))
  :good)

(deftest with-standard-io-syntax.22
  (let ((i 3))
    (with-standard-io-syntax
     (incf i 10)
     (+ i 2)))
  15)











