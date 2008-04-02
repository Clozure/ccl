;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:39:42 2003
;;;; Contains: Tests of SUBST-IF

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest subst-if.1
  (check-subst-if 'a #'consp '((100 1) (2 3) (4 3 2 1) (a b c)))
  a)

(deftest subst-if.2
  (check-subst-if 17 (complement #'listp) '(a (a b) (a c d) (a nil e f g)))
  (17 (17 17) (17 17 17) (17 nil 17 17 17)))

(deftest subst-if.3
  (check-subst-if '(z)
		  (complement #'consp)
		  '(a (a b) (c d e) (f g h i)))
  ((z)
   ((z) (z) z)
   ((z) (z) (z) z)
   ((z) (z) (z) (z) z)
   z))

(deftest subst-if.4
  (check-subst-if 'b #'identity '((100 1) (2 3) (4 3 2 1) (a b c))
		  :key #'listp)
  b)

(deftest subst-if.5
  (check-subst-if 4 #'(lambda (x) (eql x 1))
		  '((1 3) (1) (1 10 20 30) (1 3 x y))
		  :key #'(lambda (x)
			   (and (consp x)
				(car x))))
  (4 4 4 4))

(deftest subst-if.6
  (check-subst-if 'a  #'(lambda (x) (eql x 'b))
		  '((a) (b) (c) (d))
		  :key nil)
  ((a) (a) (c) (d)))

(deftest subst-if.7
  (let ((i 0) w x y z)
    (values
     (subst-if
      (progn (setf w (incf i)) 'a)
      (progn (setf x (incf i)) #'(lambda (x) (eql x 'b)))
      (progn (setf y (incf i)) (copy-list '(1 2 a b c)))
      :key (progn (setf z (incf i)) #'identity))
     i w x y z))
  (1 2 a a c)
  4 1 2 3 4)

(def-fold-test subst-if.fold.1 (subst-if 'x 'numberp '(a b 3 (4) c d . 12)))

;;; Keyword tests for subst-if

(deftest subst-if.allow-other-keys.1
  (subst-if 'a #'null nil :bad t :allow-other-keys t)
  a)

(deftest subst-if.allow-other-keys.2
  (subst-if 'a #'null nil :allow-other-keys t)
  a)

(deftest subst-if.allow-other-keys.3
  (subst-if 'a #'null nil :allow-other-keys nil)
  a)

(deftest subst-if.allow-other-keys.4
  (subst-if 'a #'null nil :allow-other-keys t :bad t)
  a)

(deftest subst-if.allow-other-keys.5
  (subst-if 'a #'null nil :allow-other-keys t :allow-other-keys nil :bad t)
  a)

(deftest subst-if.keywords.6
  (subst-if 'a #'null nil :key nil :key (constantly 'b))
  a)

;;; Error tests

(deftest subst-if.error.1
  (signals-error (subst-if) program-error)
  t)

(deftest subst-if.error.2
  (signals-error (subst-if 'a) program-error)
  t)

(deftest subst-if.error.3
  (signals-error (subst-if 'a #'null) program-error)
  t)

(deftest subst-if.error.4
  (signals-error (subst-if 'a #'null nil :foo nil) program-error)
  t)

(deftest subst-if.error.5
  (signals-error (subst-if 'a #'null nil :test) program-error)
  t)

(deftest subst-if.error.6
  (signals-error (subst-if 'a #'null nil 1) program-error)
  t)

(deftest subst-if.error.7
  (signals-error (subst-if 'a #'null nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest subst-if.error.8
  (signals-error (subst-if 'a #'null (list 'a nil 'c) :key #'cons) program-error)
  t)
