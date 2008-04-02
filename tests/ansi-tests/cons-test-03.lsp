;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:32:20 1998
;;;; Contains: Testing of CL Features related to "CONS", part 3

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (typep <obj> 'list)

;;; These tests are now somewhat redundant

(deftest typep-nil-list
  (notnot-mv (typep nil 'list))
  t)

(deftest typep-symbol-list
  (typep 'a 'list)
  nil)

(deftest typep-singleton-list-list
  (notnot-mv (typep '(a) 'list))
  t)

(deftest typep-circular-list-list
  (let ((x (cons nil nil)))
    (setf (cdr x) x)
    (notnot-mv (typep x 'list)))
  t)

(deftest typep-longer-list-list
  (notnot-mv (typep '(a b c d e f g h) 'list))
  t)
