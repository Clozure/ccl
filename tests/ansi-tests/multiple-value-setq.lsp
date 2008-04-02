;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 19 07:00:57 2002
;;;; Contains: Tests of MULTIPLE-VALUE-SETQ

(in-package :cl-test)

(deftest multiple-value-setq.1
  (let ((x 1) (y 2))
    (values
     (multiple-value-list
      (multiple-value-setq (x y) (values 3 4)))
     x y))
  (3) 3 4)

(deftest multiple-value-setq.2
  (let (x)
    (multiple-value-setq (x) (values 1 2))
    x)
  1)

(deftest multiple-value-setq.3
  (let (x)
    (symbol-macrolet ((y x))
      (multiple-value-setq (y) (values 1 2))
    x))
  1)

(deftest multiple-value-setq.4
  (let ((x (list nil)))
    (symbol-macrolet ((y (car x)))
      (multiple-value-setq (y) (values 1 2))
    x))
  (1))

;;; test of order of evaluation
;;; The (INCF I) should be evaluated before the assigned form I.
(deftest multiple-value-setq.5
  (let ((i 0) (x (list nil)))
    (symbol-macrolet ((y (car (progn (incf i) x))))
      (multiple-value-setq (y) i))
    x)
  (1))

(deftest multiple-value-setq.6
  (let ((x (list nil)) z)
    (symbol-macrolet ((y (car x)))
      (multiple-value-setq (y z) (values 1 2)))
    (values x z))
  (1) 2)

(deftest multiple-value-setq.7
  (let ((x (list nil)) (z (list nil)))
    (symbol-macrolet ((y (car x))
		      (w (car z)))
      (multiple-value-setq (y w) (values 1 2)))
    (values x z))
  (1) (2))

;;; Another order of evaluation tests, this time with two
;;; symbol macro arguments
(deftest multiple-value-setq.8
  (let ((x (list nil)) (z (list nil)) (i 0))
    (symbol-macrolet ((y (car (progn (incf i 3) x)))
		      (w (car (progn (incf i i) z))))
      (multiple-value-setq (y w) (values i 10)))
    (values x z))
  (6) (10))

(deftest multiple-value-setq.9
  (let (x)
    (values
     (multiple-value-setq (x x) (values 1 2))
     x))
  1 2)

(deftest multiple-value-setq.10
  (let (x)
    (values
     (multiple-value-setq (x x) (values 1))
     x))
  1 nil)

(deftest multiple-value-setq.11
  (let ((x 1) (y 2) (z 3))
    (multiple-value-setq (x y z) (values))
    (values x y z))
  nil nil nil)


(deftest multiple-value-setq.12
  (let ((n (min 100 multiple-values-limit))
	(vars nil)
	(result nil))
    (loop
     for i from 1 below n
     for form =
     (progn
       (push (gensym) vars)
       (push i result)
       `(let ,vars
	  (and (eql (multiple-value-setq ,vars (values-list (quote ,result)))
		    ,(car result))
	       (equal ,(make-list-expr vars)
		      (quote ,result)))))
     unless (eval form)
     collect (list i form)))
  nil)

(deftest multiple-value-setq.13
  (multiple-value-setq nil :good)
  :good)

(deftest multiple-value-setq.14
  (multiple-value-setq nil (values))
  nil)

(deftest multiple-value-setq.15
  (multiple-value-setq nil (values 'a 'b))
  a)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest multiple-value-setq.16
  (macrolet
   ((%m (z) z))
   (let ((x :bad))
     (symbol-macrolet ((z (expand-in-current-env (%m x))))
		      (multiple-value-setq (z) :good))
     x))
  :good)

(deftest multiple-value-setq.17
  (macrolet
   ((%m (z) z))
   (let ((x :bad))
     (values
      (multiple-value-setq (x) (expand-in-current-env (%m :good)))
      x)))
  :good :good)

;;; Error tests

(deftest multiple-value-setq.error.1
  (signals-error (funcall (macro-function 'multiple-value-setq))
		 program-error)
  t)
  
(deftest multiple-value-setq.error.2
  (signals-error (funcall (macro-function 'multiple-value-setq)
			   '(multiple-value-setq nil nil))
		 program-error)
  t)

(deftest multiple-value-setq.error.3
  (signals-error (funcall (macro-function 'multiple-value-setq)
			   '(multiple-value-setq nil nil)
			   nil nil)
		 program-error)
  t)

  