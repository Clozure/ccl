;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 19:36:57 2002
;;;; Contains: Tests of WHEN

(in-package :cl-test)

(deftest when.1
  (when t)
  nil)

(deftest when.2
  (when nil 'a)
  nil)

(deftest when.3 (when t (values)))

(deftest when.4
  (when t (values 'a 'b 'c 'd))
  a b c d)

(deftest when.5
 (when nil (values))
 nil)

(deftest when.6
  (when nil (values 'a 'b 'c 'd))
  nil)

(deftest when.7
  (let ((x 0))
    (values
     (when t (incf x) 'a)
     x))
  a 1)

;;; No implicit tagbody
(deftest when.8
  (block done
    (tagbody
     (when t
       (go 10)
       10
       (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest when.9
  (macrolet
   ((%m (z) z))
   (when (expand-in-current-env (%m t)) :good))
  :good)

(deftest when.10
  (macrolet
   ((%m (z) z))
   (when (expand-in-current-env (%m nil)) :bad))
  nil)

(deftest when.11
  (macrolet
   ((%m (z) z))
   (let ((x t))
     (values (when x (expand-in-current-env (%m (setf x 'foo)))) x)))
  foo foo)

;;; Error tests

(deftest when.error.1
  (signals-error (funcall (macro-function 'when)) program-error)
  t)

(deftest when.error.2
  (signals-error (funcall (macro-function 'when)
			   '(when t))
		 program-error)
  t)

(deftest when.error.3
  (signals-error (funcall (macro-function 'when)
			   '(when t) nil nil)
		 program-error)
  t)
