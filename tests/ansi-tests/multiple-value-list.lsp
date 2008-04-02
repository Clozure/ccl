;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 17 06:38:07 2003
;;;; Contains: Tests of MULTIPLE-VALUE-LIST

(in-package :cl-test)

(deftest multiple-value-list.1
  (multiple-value-list 'a)
  (a))

(deftest multiple-value-list.2
  (multiple-value-list (values))
  nil)

(deftest multiple-value-list.3
  (multiple-value-list (values 'a 'b 'c 'd 'e))
  (a b c d e))

(deftest multiple-value-list.4
  (multiple-value-list (values (values 'a 'b 'c 'd 'e)))
  (a))

(deftest multiple-value-list.5
  (multiple-value-list (values 'a))
  (a))

(deftest multiple-value-list.6
  (multiple-value-list (values 'a 'b))
  (a b))

(deftest multiple-value-list.7
  (not
   (loop
    for i from 0 below (min multiple-values-limit 100)
    for x = (make-list i :initial-element 'a)
    always (equal x (multiple-value-list (values-list x)))))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest multiple-value-list.8
  (macrolet
   ((%m (z) z))
   (multiple-value-list (expand-in-current-env (%m 1))))
  (1))

(deftest multiple-value-list.9
  (macrolet
   ((%m (z) z))
   (multiple-value-list (expand-in-current-env (%m (values 1 2 3)))))
  (1 2 3))

;;; Test that the argument is evaluated just once

(deftest multiple-value-list.order.1
  (let ((i 0))
    (values (multiple-value-list (incf i)) i))
  (1) 1)

;;; Error tests

(deftest multiple-value-list.error.1
  (signals-error (funcall (macro-function 'multiple-value-list))
		 program-error)
  t)
  
(deftest multiple-value-list.error.2
  (signals-error (funcall (macro-function 'multiple-value-list)
			   '(multiple-value-list nil))
		 program-error)
  t)

(deftest multiple-value-list.error.3
  (signals-error (funcall (macro-function 'multiple-value-list)
			   '(multiple-value-list nil)
			   nil nil)
		 program-error)
  t)
