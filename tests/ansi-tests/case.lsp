;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 19:56:44 2002
;;;; Contains: Tests of CASE

(in-package :cl-test)

(deftest case.1
  (case 'a)
  nil)

(deftest case.2
  (case 10 (10 'a))
  a)

(deftest case.3
  (case (copy-seq "abc") ("abc" 'a))
  nil)

(deftest case.4
  (case 'z ((a b c) 1)
	   ((d e) 2)
	   ((f z g) 3)
	   (t 4))
  3)

(deftest case.5
  (case (1+ most-positive-fixnum)
    (#.(1+ most-positive-fixnum) 'a))
  a)

(deftest case.6
  (case nil (nil 'a) (t 'b))
  b)

(deftest case.7
  (case nil ((nil) 'a) (t 'b))
  a)

(deftest case.8
  (case 'a (b 0) (a (values 1 2 3)) (t nil))
  1 2 3)

(deftest case.9
  (case 'c (b 0) (a (values 1 2 3)) (t (values 'x 'y 'z)))
  x y z)

(deftest case.10
  (case 'z (b 1) (a 2) (z (values)) (t nil)))

(deftest case.11
  (case 'z (b 1) (a 2) (t (values))))

(deftest case.12
  (case t (a 10))
  nil)

(deftest case.13
  (case t ((t) 10) (t 20))
  10)

(deftest case.14
  (let ((x (list 'a 'b)))
    (eval `(case (quote ,x) ((,x) 1) (t 2))))
  1)

(deftest case.15
  (case 'otherwise ((t) 10))
  nil)

(deftest case.16
  (case t ((otherwise) 10))
  nil)

(deftest case.17
  (case 'a (b 0) (c 1) (otherwise 2))
  2)

(deftest case.18
  (case 'a (b 0) (c 1) ((otherwise) 2))
  nil)

(deftest case.19
  (case 'a (b 0) (c 1) ((t) 2))
  nil)

(deftest case.20
  (case #\a
    ((#\b #\c) 10)
    ((#\d #\e #\A) 20)
    (() 30)
    ((#\z #\a #\y) 40))
  40)

(deftest case.21 (case 1 (1 (values))))

(deftest case.22 (case 2 (t (values))))

(deftest case.23 (case 1 (1 (values 'a 'b 'c)))
  a b c)

(deftest case.24 (case 2 (t (values 'a 'b 'c)))
  a b c)

;;; Show that the key expression is evaluated only once.
(deftest case.25
  (let ((x 0))
    (values
     (case (progn (incf x) 'c)
       (a 1)
       (b 2)
       (c 3)
       (t 4))
     x))
  3 1)

;;; Repeated keys are allowed (all but the first are ignored)

(deftest case.26
  (case 'b ((a b c) 10) (b 20))
  10)

(deftest case.27
  (case 'b (b 20) ((a b c) 10))
  20)

(deftest case.28
  (case 'b (b 20) (b 10) (t 0))
  20)

;;; There are implicit progns

(deftest case.29
  (let ((x nil))
    (values
     (case 2
       (1 (setq x 'a) 'w)
       (2 (setq x 'b) 'y)
       (t (setq x 'c) 'z))
     x))
  y b)

(deftest case.30
  (let ((x nil))
    (values
     (case 10
       (1 (setq x 'a) 'w)
       (2 (setq x 'b) 'y)
       (t (setq x 'c) 'z))
     x))
  z c)

(deftest case.31
  (case (values 'b 'c) (c 0) ((a b) 10) (t 20))
  10)

(deftest case.32
  (case 'a (a) (t 'b))
  nil)

(deftest case.33
  (case 'a (b 'b) (t))
  nil)

(deftest case.34
  (case 'a (b 'b) (otherwise))
  nil)

;;; No implicit tagbody
(deftest case.35
  (block done
    (tagbody
     (case 'a (a (go 10)
		 10
		 (return-from done 'bad)))
     10
     (return-from done 'good)))
  good)

(deftest case.36
  (block done
    (tagbody
     (case 'b
       (a 'bad)
       (otherwise (go 10)
		  10
		  (return-from done 'bad)))
     10
     (return-from done 'good)))
  good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest case.37
  (macrolet
   ((%m (z) z))
   (case (expand-in-current-env (%m :b))
	 (:a :bad1)
	 (:b :good)
	 (:c :bad2)
	 (t :bad3)))
  :good)

;;; (deftest case.error.1
;;;  (signals-error (case) program-error)
;;;  t)

(deftest case.error.1
  (signals-error (funcall (macro-function 'case))
		 program-error)
  t)

(deftest case.error.2
  (signals-error (funcall (macro-function 'case) '(case t))
		 program-error)
  t)

(deftest case.error.3
  (signals-error (funcall (macro-function 'case) '(case t) nil nil)
		 program-error)
  t)
