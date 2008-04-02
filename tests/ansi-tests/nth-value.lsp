;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 19 08:24:14 2002
;;;; Contains: Tests of NTH-VALUE

(in-package :cl-test)

(deftest nth-value.1
  (nth-value 0 'a)
  a)

(deftest nth-value.2
  (nth-value 1 'a)
  nil)

(deftest nth-value.3
  (nth-value 0 (values))
  nil)

(deftest nth-value.4
  (loop for i from 0 to 19
	collect (nth-value i (values 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k
				     'l 'm 'n 'o 'p 'q 'r 's)))
  (a b c d e f g h i j k l m n o p q r s nil))

(deftest nth-value.5
  (nth-value 100 'a)
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest nth-value.6
  (macrolet
   ((%m (z) z))
   (nth-value (expand-in-current-env (%m 1)) (values 'a 'b 'c)))
  b)

(deftest nth-value.7
  (macrolet
   ((%m (z) z))
   (nth-value 1 (expand-in-current-env (%m (values 'a 'b 'c)))))
  b)

;;; Order of evaluation test

(deftest nth-value.order.1
  (let ((i 0) x y)
    (values
     (nth-value (progn (setf x (incf i)) 3)
		(progn (setf y (incf i)) (values 'a 'b 'c 'd 'e 'f 'g)))
     i x y))
  d 2 1 2)

;;; Error tests

(deftest nth-value.error.1
  (signals-error (funcall (macro-function 'nth-value))
		 program-error)
  t)
  
(deftest nth-value.error.2
  (signals-error (funcall (macro-function 'nth-value)
			   '(nth-value 1 '(a b c)))
		 program-error)
  t)

(deftest nth-value.error.3
  (signals-error (funcall (macro-function 'nth-value)
			   '(nth-value 1 '(a b c))
			   nil nil)
		 program-error)
  t)
