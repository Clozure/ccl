;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:30:28 2003
;;;; Contains: Tests of RPLACD

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest rplacd.1
  (let ((x (cons 'a 'b)))
    (let ((y x))
      (and (eqt (rplacd x 'd) y)
	   (eqt x y)
	   (eqt (car x) 'a)
	   (eqt (cdr x) 'd))))
  t)

(deftest rplacd.order.1
  (let ((x (cons 'a 'b))
	(i 0) a b)
    (values
     (rplacd (progn (setf a (incf i)) x)
	     (progn (setf b (incf i)) 'c))
     i a b))
  (a . c) 2 1 2)

;; rplacd on a non-cons is a type error
(deftest rplacd.error.1
  (check-type-error #'(lambda (x) (rplacd x 1)) #'consp)
  nil)

(deftest rplacd.error.2
  (signals-error (rplacd) program-error)
  t)

(deftest rplacd.error.3
  (signals-error (rplacd (cons 'a 'b)) program-error)
  t)

(deftest rplacd.error.4
  (signals-error (rplacd (cons 'a 'b) (cons 'c 'd) 'garbage) program-error)
  t)

(deftest rplacd.error.6
  (signals-error (locally (rplacd 'a 1) t) type-error)
  t)

