;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:51:41 2003
;;;; Contains: Tests of NSUBST-IF

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest nsubst-if.1
    (check-nsubst-if 'a #'consp '((100 1) (2 3) (4 3 2 1) (a b c)))
  a)

(deftest nsubst-if.2
  (check-nsubst-if 17 (complement #'listp) '(a (a b) (a c d) (a nil e f g)))
  (17 (17 17) (17 17 17) (17 nil 17 17 17)))

(deftest nsubst-if.3
  (check-nsubst-if '(z)
		   (complement #'consp)
		   '(a (a b) (c d e) (f g h i)))
  ((z)
   ((z) (z) z)
   ((z) (z) (z) z)
   ((z) (z) (z) (z) z)
   z))

(deftest nsubst-if.4
  (check-nsubst-if 'b #'identity '((100 1) (2 3) (4 3 2 1) (a b c))
		   :key #'listp)
  b)

(deftest nsubst-if.5
  (check-nsubst-if 4 #'(lambda (x) (eql x 1))
		   '((1 3) (1) (1 10 20 30) (1 3 x y))
		   :key #'(lambda (x)
			    (and (consp x)
				 (car x))))
  (4 4 4 4))

(deftest nsubst-if.6
  (check-nsubst-if 'a  #'(lambda (x) (eql x 'b))
		   '((a) (b) (c) (d))
		   :key nil)
  ((a) (a) (c) (d)))

(deftest nsubst-if.7
  (nsubst-if 'a #'null nil :bad t :allow-other-keys t)
  a)

(deftest nsubst-if.8
  (let ((i 0) w x y z)
    (values
     (nsubst-if
      (progn (setf w (incf i)) 'a)
      (progn (setf x (incf i)) #'(lambda (x) (eql x 'b)))
      (progn (setf y (incf i)) (copy-list '(1 2 a b c)))
      :key (progn (setf z (incf i)) #'identity))
     i w x y z))
  (1 2 a a c)
  4 1 2 3 4)

;;; Keyword tests for nsubst-if

(deftest nsubst-if.allow-other-keys.1
  (nsubst-if 'a #'null nil :bad t :allow-other-keys t)
  a)

(deftest nsubst-if.allow-other-keys.2
  (nsubst-if 'a #'null nil :allow-other-keys t)
  a)

(deftest nsubst-if.allow-other-keys.3
  (nsubst-if 'a #'null nil :allow-other-keys nil)
  a)

(deftest nsubst-if.allow-other-keys.4
  (nsubst-if 'a #'null nil :allow-other-keys t :bad t)
  a)

(deftest nsubst-if.allow-other-keys.5
  (nsubst-if 'a #'null nil :allow-other-keys t :allow-other-keys nil :bad t)
  a)

(deftest nsubst-if.keywords.6
  (nsubst-if 'a #'null nil :key nil :key (constantly 'b))
  a)

;;; error cases

(deftest nsubst-if.error.1
  (signals-error (nsubst-if) program-error)
  t)

(deftest nsubst-if.error.2
  (signals-error (nsubst-if 'a) program-error)
  t)

(deftest nsubst-if.error.3
  (signals-error (nsubst-if 'a #'null) program-error)
  t)

(deftest nsubst-if.error.4
  (signals-error (nsubst-if 'a #'null nil :foo nil) program-error)
  t)

(deftest nsubst-if.error.5
  (signals-error (nsubst-if 'a #'null nil :test) program-error)
  t)

(deftest nsubst-if.error.6
  (signals-error (nsubst-if 'a #'null nil 1) program-error)
  t)

(deftest nsubst-if.error.7
  (signals-error (nsubst-if 'a #'null nil :bad t :allow-other-keys nil)
		 program-error)
  t)

(deftest nsubst-if.error.8
  (signals-error (nsubst-if 'a #'null (list 'a nil 'c) :key #'cons)
		 program-error)
  t)
