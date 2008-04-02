;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 15:37:20 2003
;;;; Contains: Tests of PSETQ

(in-package :cl-test)

(deftest psetq.1
  (psetq)
  nil)

(deftest psetq.2
  (let ((x 0))
    (values (psetq x 1) x))
  nil 1)

(deftest psetq.3
  (let ((x 0) (y 1))
    (values (psetq x y y x) x y))
  nil 1 0)

(deftest psetq.4
  (let ((x 0))
    (values
     (symbol-macrolet ((x y))
       (let ((y 1))
	 (psetq x 2)
	 y))
     x))
  2 0)

(deftest psetq.5
  (let ((w (list nil)))
    (values
     (symbol-macrolet ((x (car w)))
       (psetq x 2))
     w))
  nil (2))

(deftest psetq.6
  (let ((c 0) x y)
    (psetq x (incf c)
	   y (incf c))
    (values c x y))
  2 1 2)

;;; The next test is a PSETQ that is equivalent to a PSETF
;;; See PSETF.7 for comments related to this test.

(deftest psetq.7
  (symbol-macrolet ((x (aref a (incf i)))
		    (y (aref a (incf i))))
    (let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9)))
	  (i 0))
      (psetq x (aref a (incf i))
	     y (aref a (incf i)))
      (values a i)))
  #(0 2 2 4 4 5 6 7 8 9)
  4)

(deftest psetq.8
  (let ((*x* 0) (*y* 10))
    (declare (special *x* *y*))
    (values
     *x* *y*
     (psetq *x* 6
	    *y* 15)
     *x* *y*))
  0 10 nil 6 15)

(deftest psetq.9
  (let ((*x* 0) (*y* 10))
    (declare (special *x* *y*))
    (values
     *x* *y*
     (psetq *x* *y*
	    *y* *x*)
     *x* *y*))
  0 10 nil 10 0)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest psetq.10
  (macrolet
   ((%m (z) z))
   (let ((x nil) (y nil))
     (values
      (psetq x (expand-in-current-env (%m 1))
	     y (expand-in-current-env (%m 2)))
      x y)))
  nil 1 2)

(deftest psetq.error.1
  (signals-error (funcall (macro-function 'psetq)) program-error)
  t)

(deftest psetq.error.2
  (signals-error (funcall (macro-function 'psetq) '(psetq))
		 program-error)
  t)

(deftest psetq.error.3
  (signals-error (funcall (macro-function 'psetq) '(psetq) nil nil)
		 program-error)
  t)
