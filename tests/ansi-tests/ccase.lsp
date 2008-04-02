;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 21:06:45 2002
;;;; Contains: Tests of CCASE

(in-package :cl-test)

(deftest ccase.1
  (let ((x 'b))
    (ccase x (a 1) (b 2) (c 3)))
  2)

(deftest ccase.2
  (signals-type-error x 1 (ccase x))
  t)

(deftest ccase.3
  (signals-type-error x 1 (ccase x (a 1) (b 2) (c 3)))
  t)

;;; It is legal to use T or OTHERWISE as key designators
;;; in CCASE forms.  They have no special meaning here.

(deftest ccase.4
  (signals-type-error x 1 (ccase x (t nil)))
  t)

(deftest ccase.5
  (signals-type-error x 1 (ccase x (otherwise nil)))
  t)

(deftest ccase.6
  (let ((x 'b))
    (ccase x ((a z) 1) ((y b w) 2) ((b c) 3)))
  2)

(deftest ccase.7
  (let ((x 'z))
    (ccase x
	   ((a b c) 1)
	   ((d e) 2)
	   ((f z g) 3)))
  3)

(deftest ccase.8
  (let ((x (1+ most-positive-fixnum)))
    (ccase x (#.(1+ most-positive-fixnum) 'a)))
  a)

(deftest ccase.9
  (signals-type-error x nil (ccase x (nil 'a)))
  t)

(deftest ccase.10
  (let (x)
    (ccase x ((nil) 'a)))
  a)

(deftest ccase.11
  (let ((x 'a))
    (ccase x (b 0) (a (values 1 2 3)) (c nil)))
  1 2 3)

(deftest ccase.12
  (signals-type-error x t (ccase x (a 10)))
  t)

(deftest ccase.13
  (let ((x t))
    (ccase x ((t) 10) (t 20)))
  10)

(deftest ccase.14
  (let ((x (list 'a 'b)))
    (eval `(let ((y (quote ,x))) (ccase y ((,x) 1) (a 2)))))
  1)

(deftest ccase.15
  (signals-type-error x 'otherwise (ccase x ((t) 10)))
  t)

(deftest ccase.16
  (signals-type-error x t (ccase x ((otherwise) 10)))
  t)

(deftest ccase.17
  (signals-type-error x 'a (ccase x (b 0) (c 1) (otherwise 2)))
  t)

(deftest ccase.19
  (signals-type-error x 'a (ccase x (b 0) (c 1) ((t) 2)))
  t)

(deftest ccase.20
  (let ((x #\a))
    (ccase x
	   ((#\b #\c) 10)
	   ((#\d #\e #\A) 20)
	   (() 30)
	   ((#\z #\a #\y) 40)))
  40)

(deftest ccase.21 (let ((x 1)) (ccase x (1 (values)) (2 'a))))

(deftest ccase.23 (let ((x 1)) (ccase x (1 (values 'a 'b 'c))))
  a b c)

;;; Show that the key expression is evaluated only once.
(deftest ccase.25
  (let ((a (vector 'a 'b 'c 'd 'e))
	(i 1))
    (values
     (ccase (aref a (incf i))
       (a 1)
       (b 2)
       (c 3)
       (d 4))
     i))
  3 2)

;;; Repeated keys are allowed (all but the first are ignored)

(deftest ccase.26
  (let ((x 'b))
    (ccase x ((a b c) 10) (b 20)))
  10)

(deftest ccase.27
  (let ((x 'b))
    (ccase x (b 20) ((a b c) 10)))
  20)

(deftest ccase.28
  (let ((x 'b))
    (ccase x (b 20) (b 10) (d 0)))
  20)

;;; There are implicit progns

(deftest ccase.29
  (let ((x nil) (y 2))
    (values
     (ccase y
       (1 (setq x 'a) 'w)
       (2 (setq x 'b) 'y)
       (3 (setq x 'c) 'z))
     x))
  y b)

(deftest ccase.30
  (let ((x 'a))
    (ccase x (a)))
  nil)

(deftest ccase.31
  (handler-bind
   ((type-error #'(lambda (c) (store-value 7 c))))
   (let ((x 0))
     (ccase x
      (1 :bad)
      (7 :good)
      (2 nil))))
  :good)

;;; No implicit tagbody
(deftest ccase.32
  (block done
    (tagbody
     (let ((x 'a))
       (ccase x (a (go 10)
		   10
		   (return-from done 'bad))))
     10
     (return-from done 'good)))
  good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest ccase.33
  (let ((x :b))
    (macrolet
     ((%m (z) z))
     (ccase (expand-in-current-env (%m x))
	    (:a :bad1)
	    (:b :good)
	    (:c :bad2))))
  :good)



;;; (deftest ccase.error.1
;;;  (signals-error (ccase) program-error)
;;;  t)

(deftest ccase.error.1
  (signals-error (funcall (macro-function 'ccase))
		 program-error)
  t)

(deftest ccase.error.2
  (signals-error (funcall (macro-function 'ccase) '(ccase t))
		 program-error)
  t)

(deftest ccase.error.3
  (signals-error (funcall (macro-function 'ccase) '(ccase t) nil nil)
		 program-error)
  t)
