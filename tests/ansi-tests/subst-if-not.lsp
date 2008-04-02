;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:48:22 2003
;;;; Contains: Tests of SUBST-IF-NOT

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest subst-if-not.1
  (check-subst-if-not '(x) 'consp '(1 (1 2) (1 2 3) (1 2 3 4)))
  ((x)
   ((x) (x) x)
   ((x) (x) (x) x)
   ((x) (x) (x) (x) x)
   x))

(deftest subst-if-not.2
  (check-subst-if-not 'a (complement #'listp)
		      '((100 1) (2 3) (4 3 2 1) (a b c)))
  a)

(deftest subst-if-not.3
  (check-subst-if-not 'c #'identity
		      '((100 1) (2 3) (4 3 2 1) (a b c))
		      :key (complement #'listp))
  c)

(deftest subst-if-not.4
  (check-subst-if-not
   40
   #'(lambda (x) (not (eql x 17)))
   '((17) (17 22) (17 22 31) (17 21 34 54))
   :key #'(lambda (x)
	    (and (consp x)
		 (car x))))
  (40 40 40 40))

(deftest subst-if-not.5
  (check-subst-if-not 'a  #'(lambda (x) (not (eql x 'b)))
		      '((a) (b) (c) (d))
		      :key nil)
  ((a) (a) (c) (d)))

(deftest subst-if-not.7
  (let ((i 0) w x y z)
    (values
     (subst-if-not
      (progn (setf w (incf i)) 'a)
      (progn (setf x (incf i)) #'(lambda (x) (not (eql x 'b))))
      (progn (setf y (incf i)) (copy-list '(1 2 a b c)))
      :key (progn (setf z (incf i)) #'identity))
     i w x y z))
  (1 2 a a c)
  4 1 2 3 4)

(def-fold-test subst-if-not.fold.1 (subst-if-not 'a #'consp '((1 . 2) 3 . 4)))
  
;;; Keywords tests for subst-if-not

(deftest subst-if-not.allow-other-keys.1
  (subst-if-not 'a #'identity nil :bad t :allow-other-keys t)
  a)

(deftest subst-if-not.allow-other-keys.2
  (subst-if-not 'a #'identity nil :allow-other-keys t)
  a)

(deftest subst-if-not.allow-other-keys.3
  (subst-if-not 'a #'identity nil :allow-other-keys nil)
  a)

(deftest subst-if-not.allow-other-keys.4
  (subst-if-not 'a #'identity nil :allow-other-keys t :bad t)
  a)

(deftest subst-if-not.allow-other-keys.5
  (subst-if-not 'a #'identity nil :allow-other-keys t
		:allow-other-keys nil :bad t)
  a)

(deftest subst-if-not.keywords.6
  (subst-if-not 'a #'identity nil :key nil :key (constantly 'b))
  a)

;;; error cases

(deftest subst-if-not.error.1
  (signals-error (subst-if-not) program-error)
  t)

(deftest subst-if-not.error.2
  (signals-error (subst-if-not 'a) program-error)
  t)

(deftest subst-if-not.error.3
  (signals-error (subst-if-not 'a #'null) program-error)
  t)

(deftest subst-if-not.error.4
  (signals-error (subst-if-not 'a #'null nil :foo nil) program-error)
  t)

(deftest subst-if-not.error.5
  (signals-error (subst-if-not 'a #'null nil :test) program-error)
  t)

(deftest subst-if-not.error.6
  (signals-error (subst-if-not 'a #'null nil 1) program-error)
  t)

(deftest subst-if-not.error.7
  (signals-error (subst-if-not 'a #'null nil
				:bad t :allow-other-keys nil) program-error)
  t)

(deftest subst-if-not.error.8
  (signals-error (subst-if-not 'a #'null (list 'a nil 'c) :key #'cons) program-error)
  t)
