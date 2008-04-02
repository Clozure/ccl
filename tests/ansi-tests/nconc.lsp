;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:35:53 2003
;;;; Contains: Tests of NCONC

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest nconc.1
  (nconc)
  nil)

(deftest nconc.2
  (nconc (copy-tree '(a b c d e f)))
  (a b c d e f))

;;; (deftest nconc.3
;;;  (nconc 1)
;;;  1)

(deftest nconc.4
  (let ((x (list 'a 'b 'c))
	(y (list 'd 'e 'f)))
    (let ((ycopy (make-scaffold-copy y)))
      (let ((result (nconc x y)))
	(and
	 (check-scaffold-copy y ycopy)
	 (eqt (cdddr x) y)
	 result))))
  (a b c d e f))

(deftest nconc.5
  (let ((x (list 'a 'b 'c)))
    (nconc x x)
    (and
     (eqt (cdddr x) x)
     (null (list-length x))))
  t)

(deftest nconc.6
  (let ((x (list 'a 'b 'c))
	(y (list 'd 'e 'f 'g 'h))
	(z (list 'i 'j 'k)))
    (let ((result (nconc x y z 'foo)))
      (and
       (eqt (nthcdr 3 x) y)
       (eqt (nthcdr 5 y) z)
       (eqt (nthcdr 3 z) 'foo)
       result)))
  (a b c d e f g h i j k . foo))

(deftest nconc.7
  (nconc (copy-tree '(a . b))
	 (copy-tree '(c . d))
	 (copy-tree '(e . f))
	 'foo)
  (a c e . foo))

(deftest nconc.order.1
  (let ((i 0) x y z)
    (values
     (nconc (progn (setf x (incf i)) (copy-list '(a b c)))
	    (progn (setf y (incf i)) (copy-list '(d e f)))
	    (progn (setf z (incf i)) (copy-list '(g h i))))
     i x y z))
  (a b c d e f g h i) 3 1 2 3)

(deftest nconc.order.2
  (let ((i 0))
    (values
     (nconc (list 'a) (incf i))
     i))
  (a . 1) 1)
