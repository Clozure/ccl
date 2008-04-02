;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:03:37 2003
;;;; Contains: Tests of LISTP

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest listp-nil
  (notnot-mv (listp nil))
  t)

(deftest listp-symbol
  (listp 'a)
  nil)

(deftest listp-singleton-list
  (notnot-mv (listp '(a)))
  t)

(deftest listp-circular-list
  (let ((x (cons nil nil)))
    (setf (cdr x) x)
    (notnot-mv (listp x)))
  t)

(deftest listp-longer-list
  (notnot-mv (listp '(a b c d e f g h)))
  t)

;;; Check that (listp x) == (typep x 'list)

(deftest listp-universe
  (check-type-predicate 'listp 'list)
  nil)

(deftest listp.order.1
  (let ((i 0))
    (values (listp (incf i)) i))
  nil 1)

(deftest listp.error.1
  (signals-error (listp) program-error)
  t)

(deftest listp.error.2
  (signals-error (listp nil nil) program-error)
  t)
