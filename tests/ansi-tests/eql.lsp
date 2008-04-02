;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 19:36:33 2002
;;;; Contains: Tests of EQL

(in-package :cl-test)

;;; EQLT is defined in ansi-aux.lsp
;;; It calls EQL, returning NIL when the result is false and T when it
;;; is true.

(deftest eql.1
  (check-predicate #'(lambda (x) (eql x x)))
  nil)

(deftest eql.2
  (eqlt 2 (1+ 1))
  t)

(deftest eql.3
  (let ((x "abc"))
    (eql x (copy-seq x)))
  nil)

(deftest eql.4
  (eqlt #\a #\a)
  t)

(deftest eql.5
  (eqlt 12345678901234567890 12345678901234567890)
  t)

(deftest eql.7
  (eql 12.0 12)
  nil)

(deftest eql.8
  (eqlt #c(1 -2) #c(1 -2))
  t)

(deftest eql.9
  (let ((x "abc") (y "abc"))
    (if (eq x y) (eqlt x y) (not (eql x y))))
  t)

(deftest eql.10
  (eql (list 'a) (list 'b))
  nil)

(deftest eql.11
  (eqlt #c(1 -2) (- #c(-1 2)))
  t)

(deftest eql.order.1
  (let ((i 0) x y)
    (values
     (eql (setf x (incf i)) (setf y (incf i)))
     i x y))
  nil 2 1 2)

;;; Error tests for EQL

(deftest eql.error.1
  (signals-error (eql) program-error)
  t)

(deftest eql.error.2
  (signals-error (eql nil) program-error)
  t)

(deftest eql.error.3
  (signals-error (eql nil nil nil) program-error)
  t)

;;; Error tests for EQ

(deftest eq.error.1
  (signals-error (eq) program-error)
  t)

(deftest eq.error.2
  (signals-error (eq nil) program-error)
  t)

(deftest eq.error.3
  (signals-error (eq nil nil nil) program-error)
  t)
