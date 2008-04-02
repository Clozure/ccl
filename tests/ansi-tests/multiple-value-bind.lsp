;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 23:16:23 2002
;;;; Contains: Tests for MULTIPLE-VALUE-BIND

(in-package :cl-test)

(deftest multiple-value-bind.1
  (multiple-value-bind (x y z) (values 1 2 3)
    (declare (type integer x))
    (declare (type integer y))
    (declare (type integer z))
    (list z y x))
  (3 2 1))

(deftest multiple-value-bind.2
  (multiple-value-bind (x y z) (values 1 2 3)
    (let ((x 4))
      (list x y z)))
  (4 2 3))

(deftest multiple-value-bind.3
  (multiple-value-bind (x y z) (values 1 2 3 4 5 6)
    (list x y z))
  (1 2 3))

(deftest multiple-value-bind.4
  (multiple-value-bind (x y z) (values 1 2)
    (list x y z))
  (1 2 nil))

(deftest multiple-value-bind.5
  (multiple-value-bind () (values 1 2) (values 'a 'b 'c))
  a b c)

(deftest multiple-value-bind.6
  (multiple-value-bind (x y z) (values)
    (list x y z))
  (nil nil nil))

(deftest multiple-value-bind.7
  (let ((z 0) x y)
    (declare (special z))
    (values
     (flet ((%x () (symbol-value 'x))
	    (%y () (symbol-value 'y))
	    (%z () (symbol-value 'z)))
       (multiple-value-bind (x y z) (values 1 2 3)
	 (declare (special x y))
	 (list (%x) (%y) (%z))))
     x y z))
  (1 2 0) nil nil 0)

;;; No implicit tagbody
(deftest multiple-value-bind.8
  (block nil
    (tagbody
     (multiple-value-bind (x) nil
       (go 10)
       10
       (return 'bad))
     10
     (return 'good)))
  good)

;;; Works with single values
(deftest multiple-value-bind.9
  (multiple-value-bind (x y z) :foo (list x y z))
  (:foo nil nil))

(deftest multiple-value-bind.10
  (multiple-value-bind (x) :foo x)
  :foo)

(deftest multiple-value-bind.11
  (multiple-value-bind () :foo)
  nil)

(deftest multiple-value-bind.12
  (multiple-value-bind () (values))
  nil)

(deftest multiple-value-bind.13
  (multiple-value-bind () (values 1 2 3 4 5))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest multiple-value-bind.14
  (macrolet
   ((%m (z) z))
   (multiple-value-bind (x y z)
			(expand-in-current-env (%m (values 1 2 3)))
			(list x y z)))
  (1 2 3))

;;; Error cases

(deftest multiple-value-bind.error.1
  (signals-error (funcall (macro-function 'multiple-value-bind))
		 program-error)
  t)
  
(deftest multiple-value-bind.error.2
  (signals-error (funcall (macro-function 'multiple-value-bind)
			   '(multiple-value-bind nil nil))
		 program-error)
  t)

(deftest multiple-value-bind.error.3
  (signals-error (funcall (macro-function 'multiple-value-bind)
			   '(multiple-value-bind nil nil)
			   nil nil)
		 program-error)
  t)
