;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 20:17:30 2002
;;;; Contains: Tests for ECASE

(in-package :cl-test)

(deftest ecase.1
  (ecase 'b (a 1) (b 2) (c 3))
  2)

(deftest ecase.2
  (signals-type-error x 1 (ecase x))
  t)

(deftest ecase.3
  (signals-type-error x 1 (ecase x (a 1) (b 2) (c 3)))
  t)

;;; It is legal to use T or OTHERWISE as key designators
;;; in ECASE forms.  They have no special meaning here.

(deftest ecase.4
  (signals-type-error x 1 (ecase x (t nil)))
  t)

(deftest ecase.5
  (signals-type-error x 1 (ecase x (otherwise nil)))
  t)

(deftest ecase.6
  (ecase 'b ((a z) 1) ((y b w) 2) ((b c) 3))
  2)

(deftest ecase.7
  (ecase 'z
    ((a b c) 1)
    ((d e) 2)
    ((f z g) 3))
  3)

(deftest ecase.8
  (ecase (1+ most-positive-fixnum)
    (#.(1+ most-positive-fixnum) 'a))
  a)

(deftest ecase.9
  (signals-type-error x nil (ecase x (nil 'a)))
  t)

(deftest ecase.10
  (ecase nil ((nil) 'a))
  a)

(deftest ecase.11
  (ecase 'a (b 0) (a (values 1 2 3)) (c nil))
  1 2 3)

(deftest ecase.12
  (signals-type-error x t (ecase x (a 10)))
  t)

(deftest ecase.13
  (ecase t ((t) 10) (t 20))
  10)

(deftest ecase.14
  (let ((x (list 'a 'b)))
    (eval `(ecase (quote ,x) ((,x) 1) (a 2))))
  1)

(deftest ecase.15
  (signals-type-error x 'otherwise (ecase x ((t) 10)))
  t)

(deftest ecase.16
  (signals-type-error x t (ecase x ((otherwise) 10)))
  t)

(deftest ecase.17
  (signals-type-error x 'a (ecase x (b 0) (c 1) (otherwise 2)))
  t)

(deftest ecase.18
  (signals-type-error x 'a (ecase x (b 0) (c 1) ((otherwise) 2)))
  t)

(deftest ecase.19
  (signals-type-error x 'a (ecase x (b 0) (c 1) ((t) 2)))
  t)

(deftest ecase.20
  (ecase #\a
    ((#\b #\c) 10)
    ((#\d #\e #\A) 20)
    (() 30)
    ((#\z #\a #\y) 40))
  40)

(deftest ecase.21 (ecase 1 (1 (values)) (2 'a)))

(deftest ecase.23 (ecase 1 (1 (values 'a 'b 'c)))
  a b c)

;;; Show that the key expression is evaluated only once.
(deftest ecase.25
  (let ((x 0))
    (values
     (ecase (progn (incf x) 'c)
       (a 1)
       (b 2)
       (c 3)
       (d 4))
     x))
  3 1)

;;; Repeated keys are allowed (all but the first are ignored)

(deftest ecase.26
  (ecase 'b ((a b c) 10) (b 20))
  10)

(deftest ecase.27
  (ecase 'b (b 20) ((a b c) 10))
  20)

(deftest ecase.28
  (ecase 'b (b 20) (b 10) (d 0))
  20)

;;; There are implicit progns

(deftest ecase.29
  (let ((x nil))
    (values
     (ecase 2
       (1 (setq x 'a) 'w)
       (2 (setq x 'b) 'y)
       (3 (setq x 'c) 'z))
     x))
  y b)

(deftest ecase.31
  (ecase (values 'b 'c) (c 0) ((a b) 10) (d 20))
  10)

(deftest ecase.32
  (ecase 'a (a) (b 'b))
  nil)

;;; No implicit tagbody
(deftest ecase.33
  (block done
    (tagbody
     (ecase 'a (a (go 10)
		  10
		  (return-from done 'bad)))
     10
     (return-from done 'good)))
  good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest ecase.34
  (macrolet
   ((%m (z) z))
   (ecase (expand-in-current-env (%m :b))
	  (:a :bad1)
	  (:b :good)
	  (:c :bad2)))
  :good)

(deftest ecase.error.1
  (signals-error (funcall (macro-function 'ecase)) program-error)
  t)

(deftest ecase.error.2
  (signals-error (funcall (macro-function 'ecase) '(ecase t))
		 program-error)
  t)

(deftest ecase.error.3
  (signals-error (funcall (macro-function 'ecase) '(ecase t) nil nil)
		 program-error)
  t)
