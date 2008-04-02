;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 19 09:33:51 2002
;;;; Contains: Tests of PROGN

(in-package :cl-test)

(deftest progn.1
  (progn)
  nil)

(deftest progn.2
  (progn 'a)
  a)

(deftest progn.3
  (progn 'b 'a)
  a)

(deftest progn.4
  (let ((x 0))
    (values (progn (incf x) x) x))
  1 1)

(deftest progn.5 (progn (values)))

(deftest progn.6
  (progn (values 1 2) (values 'a 'b 'c 'd 'e))
  a b c d e)

(deftest progn.7
  (let ((x 0))
    (prog ()
	  (progn (go x) x 'a)
	  (return 'bad)
	  x
	  (return 'good)))
  good)

;;; No implicit tagbody
(deftest progn.8
  (block nil
    (tagbody
     (progn
      (go 10)
      10
      (return 'bad))
     10
     (return 'good)))
  good)

;;; Macros are expanded in the appropriate environment

(deftest progn.9
  (macrolet
   ((%m (z) z))
   (progn (expand-in-current-env (%m :good))))
  :good)

(deftest progn.10
  (macrolet
   ((%m (z) z))
   (progn (expand-in-current-env (%m :bad))
	  (expand-in-current-env (%m :good))))
  :good)


