;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 14 05:58:01 2003
;;;; Contains: Tests for BOUNDP

(in-package :cl-test)

(deftest boundp.error.1
  (signals-error (boundp) program-error)
  t)

(deftest boundp.error.2
  (signals-error (boundp 'a 'a) program-error)
  t)

(deftest boundp.error.3
  (check-type-error #'boundp #'symbolp)
  nil)

(deftest boundp.error.4
  (signals-type-error x '(setf car) (boundp x))
  t)

(deftest boundp.error.5
  (signals-type-error x "abc" (boundp x))
  t)

(deftest boundp.error.6
  (signals-type-error x "abc" (locally (boundp x) t))
  t)

;;; See other tests in cl-symbols.lsp

(deftest boundp.1
  (notnot-mv (boundp 't))
  t)

(deftest boundp.2
  (notnot-mv (boundp nil))
  t)

(deftest boundp.3
  (notnot-mv (boundp :foo))
  t)

(deftest boundp.4
  (boundp '#:foo)
  nil)

;;; See 11.1.2.1.1
(deftest boundp.5
  (loop for x in *cl-non-variable-constant-symbols*
	when (boundp x)
	collect x)
  nil)

(deftest boundp.6
  (macrolet ((%m (z) z)) (boundp (expand-in-current-env (%m '#:foo))))
  nil)

(deftest boundp.order.1
  (let ((i 0) x)
    (values
     (boundp (progn (setf x (incf i)) '#:foo))
     i x))
  nil 1 1)

