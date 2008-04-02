;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:33:23 2003
;;;; Contains: Tests of NTH

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest nth.1
  (nth-1-body (loop for i from 1 to 2000 collect (* 4 i)))
  0)

(deftest nth.2
  (let ((x (loop for i from 1 to 2000 collect i)))
    (loop
     for i from 0 to 1999 do
     (setf (nth i x) (- 1999 i)))
    (equalt x (loop for i from 1999 downto 0 collect i)))
  t)

;;; Test side effects, evaluation order in assignment to NTH
(deftest nth.order.1
  (let ((i 0)
	(x (list 'a 'b 'c 'd))
	y z)
    (and
     (eqlt (setf (nth (setf y (incf i)) x) (progn (setf z (incf i)) 'z))
	   'z)
     (eqlt y 1)
     (eqlt z 2)
     x))
  (a z c d))

(deftest nth.order.2
  (let ((i 0) x y (z '(a b c d e)))
    (values
     (nth (progn (setf x (incf i)) 1)
	  (progn (setf y (incf i)) z))
     i x y))
  b 2 1 2)

(deftest nth.error.1
  (signals-error (nth) program-error)
  t)

(deftest nth.error.2
  (signals-error (nth 0) program-error)
  t)

(deftest nth.error.3
  (signals-error (nth 1 '(a b c) nil) program-error)
  t)

(deftest nth.error.4
  (signals-error (nth 0 '(a b c) nil) program-error)
  t)
