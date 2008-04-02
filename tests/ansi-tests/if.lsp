;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 08:21:29 2002
;;;; Contains: Tests for IF

(in-package :cl-test)

(deftest if.1
  (if t 1 2)
  1)

(deftest if.2
  (if nil 1 2)
  2)

(deftest if.3 (if t (values) 'a))

(deftest if.4
  (if nil 'a)
  nil)

(deftest if.5
  (if t (values 'a 'b 'c) 'd)
  a b c)

(deftest if.6
  (if nil 'a (values 'b 'c 'd))
  b c d)

(deftest if.7 (if nil 'a (values)))

;;; Macros are expanded in the appropriate environment

(deftest if.8
  (macrolet ((%m (z) z))
	    (if (expand-in-current-env (%m t)) :good :bad))
  :good)

(deftest if.9
  (macrolet ((%m (z) z))
	    (if (expand-in-current-env (%m nil)) :bad))
  nil)

(deftest if.10
  (macrolet ((%m (z) z))
	    (if (expand-in-current-env (%m t)) :good))
  :good)

(deftest if.11
  (macrolet ((%m (z) z))
	    (if (expand-in-current-env (%m nil)) :bad :good))
  :good)

(deftest if.12
  (macrolet
   ((%m (z) z))
   (flet ((%f (x y) (if x (expand-in-current-env (%m y)))))
	 (declare (notinline %f))
	 (values (%f t :good) (%f nil :bad))))
  :good nil)
	    
(deftest if.13
  (macrolet
   ((%m (z) z))
   (flet ((%f (x y z) (if x y (expand-in-current-env (%m z)))))
	 (declare (notinline %f))
	 (values (%f t :good :bad) (%f nil :bad :good))))
  :good :good)	    

(deftest if.order.1
  (let ((i 0))
    (values (if (= (incf i) 1) 't nil) i))
  t 1)
