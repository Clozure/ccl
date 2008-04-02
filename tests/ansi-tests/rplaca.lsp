;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:29:43 2003
;;;; Contains: Tests of RPLACA

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest rplaca.1
  (let ((x (cons 'a 'b)))
    (let ((y x))
      (and (eqt (rplaca x 'c) y)
	   (eqt x y)
	   (eqt (car x) 'c)
	   (eqt (cdr x) 'b))))
  t)

(deftest rplaca.order.1
  (let ((x (cons 'a 'b))
	(i 0) a b)
    (values
     (rplaca (progn (setf a (incf i)) x)
	     (progn (setf b (incf i)) 'c))
     i a b))
  (c . b) 2 1 2)

;; rplaca on a non-cons is a type error
(deftest rplaca.error.1
  (check-type-error #'(lambda (x) (rplaca x 1)) #'consp)
  nil)

(deftest rplaca.error.2
  (signals-error (rplaca) program-error)
  t)

(deftest rplaca.error.3
  (signals-error (rplaca (cons 'a 'b)) program-error)
  t)

(deftest rplaca.error.4
  (signals-error (rplaca (cons 'a 'b) (cons 'c 'd) 'garbage) program-error)
  t)

(deftest rplaca.error.6
  (signals-error (locally (rplaca 'a 1) t) type-error)
  t)
