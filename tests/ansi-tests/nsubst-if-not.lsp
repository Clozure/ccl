;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:54:12 2003
;;;; Contains: Tests of NSUBST-IF-NOT

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest nsubst-if-not.1
  (check-nsubst-if-not '(x) 'consp '(1 (1 2) (1 2 3) (1 2 3 4)))
  ((x)
   ((x) (x) x)
   ((x) (x) (x) x)
   ((x) (x) (x) (x) x)
   x))

(deftest nsubst-if-not.2
  (check-nsubst-if-not 'a (complement #'listp)
		       '((100 1) (2 3) (4 3 2 1) (a b c)))
  a)

(deftest nsubst-if-not.3
  (check-nsubst-if-not 'c #'identity
		       '((100 1) (2 3) (4 3 2 1) (a b c))
		       :key (complement #'listp))
  c)

(deftest nsubst-if-not.4
  (check-nsubst-if-not
   40
   #'(lambda (x) (not (eql x 17)))
   '((17) (17 22) (17 22 31) (17 21 34 54))
   :key #'(lambda (x)
	    (and (consp x)
		 (car x))))
  (40 40 40 40))

(deftest nsubst-if-not.5
  (check-nsubst-if-not 'a  #'(lambda (x) (not (eql x 'b)))
		       '((a) (b) (c) (d))
		       :key nil)
  ((a) (a) (c) (d)))

(deftest nsubst-if-not.6
  (nsubst-if-not 'a #'null nil :bad t :allow-other-keys t)
  nil)

(deftest nsubst-if-not.7
  (let ((i 0) w x y z)
    (values
     (nsubst-if-not
      (progn (setf w (incf i)) 'a)
      (progn (setf x (incf i)) #'(lambda (x) (not (eql x 'b))))
      (progn (setf y (incf i)) (copy-list '(1 2 a b c)))
      :key (progn (setf z (incf i)) #'identity))
     i w x y z))
  (1 2 a a c)
  4 1 2 3 4)

;;; Keywords tests for nsubst-if-not

(deftest nsubst-if-not.allow-other-keys.1
  (nsubst-if-not 'a #'identity nil :bad t :allow-other-keys t)
  a)

(deftest nsubst-if-not.allow-other-keys.2
  (nsubst-if-not 'a #'identity nil :allow-other-keys t)
  a)

(deftest nsubst-if-not.allow-other-keys.3
  (nsubst-if-not 'a #'identity nil :allow-other-keys nil)
  a)

(deftest nsubst-if-not.allow-other-keys.4
  (nsubst-if-not 'a #'identity nil :allow-other-keys t :bad t)
  a)

(deftest nsubst-if-not.allow-other-keys.5
  (nsubst-if-not 'a #'identity nil :allow-other-keys t :allow-other-keys nil :bad t)
  a)

(deftest nsubst-if-not.keywords.6
  (nsubst-if-not 'a #'identity nil :key nil :key (constantly 'b))
  a)

;;; error cases

(deftest nsubst-if-not.error.1
  (signals-error (nsubst-if-not) program-error)
  t)

(deftest nsubst-if-not.error.2
  (signals-error (nsubst-if-not 'a) program-error)
  t)

(deftest nsubst-if-not.error.3
  (signals-error (nsubst-if-not 'a #'null) program-error)
  t)

(deftest nsubst-if-not.error.4
  (signals-error (nsubst-if-not 'a #'null nil :foo nil) program-error)
  t)

(deftest nsubst-if-not.error.5
  (signals-error (nsubst-if-not 'a #'null nil :test) program-error)
  t)

(deftest nsubst-if-not.error.6
  (signals-error (nsubst-if-not 'a #'null nil 1) program-error)
  t)

(deftest nsubst-if-not.error.7
  (signals-error (nsubst-if-not 'a #'null nil
				 :bad t :allow-other-keys nil) program-error)
  t)

(deftest nsubst-if-not.error.8
  (signals-error (nsubst-if-not 'a #'null (list 'a nil 'c) :key #'cons) program-error)
  t)

