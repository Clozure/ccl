;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 19 06:48:02 2002
;;;; Contains: Tests for MULTIPLE-VALUE-PROG1

(in-package :cl-test)

(deftest multiple-value-prog1.1
  (multiple-value-prog1 nil)
  nil)

(deftest multiple-value-prog1.2
  (multiple-value-prog1 '(a b c))
  (a b c))

(deftest multiple-value-prog1.3
  (multiple-value-prog1 (values-list '(a b c)))
  a b c)

(deftest multiple-value-prog1.4
  (multiple-value-prog1 (values)))

(deftest multiple-value-prog1.5
  (let ((x 0) (y 0))
    (multiple-value-prog1 (values x y)
			  (incf x) (incf y 2)))
  0 0)

(deftest multiple-value-prog1.6
  (let ((x 0) (y 0))
    (multiple-value-call
     #'list
     (multiple-value-prog1 (values x y)
			   (incf x) (incf y 2))
     x y))
  (0 0 1 2))

(deftest multiple-value-prog1.7
  (let ((x 0) (y 0))
    (multiple-value-call
     #'list
     (multiple-value-prog1 (values (incf x) y)
			   (incf x x)
			   (incf y 10))
     x y))
  (1 0 2 10))


(deftest multiple-value-prog1.8
  (let* ((n (min 100 multiple-values-limit)))
    (not-mv
     (loop for i from 0 below n
	   for x = (make-int-list i)
	   always
	   (equalt
	    (multiple-value-list
	     (eval `(multiple-value-prog1 (values-list (quote ,(copy-seq x)))
					  nil)))
	    x))))
  nil)


(deftest multiple-value-prog1.9
  (let ((x 0) (y 0))
    (values
     (block foo
       (multiple-value-prog1
	(values (incf x) (incf y 2))
	(return-from foo 'a)))
     x y))
  a 1 2)

;;; No implicit tagbody
(deftest multiple-value-prog1.10
  (block nil
    (tagbody
     (multiple-value-prog1
      (values)
      (go 10)
      10
      (return 'bad))
     10
     (return 'good)))
  good)

;;; Macros are expanded in the appropriate environment

(deftest multiple-value-prog1.11
  (macrolet
   ((%m (z) z))
   (multiple-value-prog1 (expand-in-current-env (%m :good))))
  :good)

(deftest multiple-value-prog1.12
  (macrolet
   ((%m (z) z))
   (multiple-value-prog1 :good (expand-in-current-env (%m :foo))))
  :good)
