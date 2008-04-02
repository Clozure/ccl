;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:37:58 2002
;;;; Contains: Tests of COND

(in-package :cl-test)

(deftest cond.1
  (cond)
  nil)

(deftest cond.2
  (cond ('a))
  a)

(deftest cond.3
  (cond (nil))
  nil)

(deftest cond.4
  (cond (nil 'a) (nil 'b))
  nil)

(deftest cond.5
  (cond (nil 'a) ('b))
  b)

(deftest cond.6
  (cond (t 'a) (t 'b))
  a)

(deftest cond.7
  (let ((x 0))
    (values
     (cond ((progn (incf x) nil) 'a) (t 'b) ((incf x) 'c))
     x))
  b 1)

(deftest cond.8
  (let ((x 0))
    (values
     (cond (nil (incf x) 'a)
	   (nil (incf x 10) 'b)
	   (t (incf x 2) 'c)
	   (t (incf x 100) 'd))
     x))
  c 2)

(deftest cond.9
  (cond ((values 'a 'b 'c)))
  a)

(deftest cond.10
  (cond (t (values 'a 'b 'c)))
  a b c)

(deftest cond.11
  (cond
   ((values nil t) 'a)
   (t 'b))
  b)

(deftest cond.12
  (cond ((values)))
  nil)

(deftest cond.13
  (cond ((values)) (t 'a))
  a)

(deftest cond.14 (cond (t (values))))

;;; No implicit tagbody
(deftest cond.15
  (block done
    (tagbody
     (cond (t (go 10)
	      10
	      (return-from done 'bad)))
     10
     (return-from done 'good)))
  good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest cond.16
  (macrolet
   ((%m (z) z))
   (cond ((expand-in-current-env (%m nil)) :bad)
	 (t :good)))
  :good)

(deftest cond.17
  (macrolet
   ((%m (z) z))
   (cond (nil :bad1)
	 ((expand-in-current-env (%m :good)))
	 (t :bad2)))
  :good)

;;; Error tests

(deftest cond.error.1
  (signals-error (funcall (macro-function 'cond))
		 program-error)
  t)

(deftest cond.error.2
  (signals-error (funcall (macro-function 'cond) '(cond))
		 program-error)
  t)

(deftest cond.error.3
  (signals-error (funcall (macro-function 'cond) '(cond) nil nil)
		 program-error)
  t)

