;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 19 09:37:14 2002
;;;; Contains: Tests for PROG1

(in-package :cl-test)

(deftest prog1.1
  (prog1 'a)
  a)

(deftest prog1.2
  (prog1 'a 'b)
  a)

(deftest prog1.3
  (prog1 (values 'a 'b) 'c)
  a)

(deftest prog1.4
  (prog1 (values) 'c)
  nil)

(deftest prog1.5
  (let ((x 0))
    (values (prog1 x (incf x)) x))
  0 1)

;;; Test that prog1 doesn't have a tagbody

(deftest prog1.6
  (block nil
    (tagbody
     (return (prog1 'bad (go 10) 10))
     10
     (return 'good)))
  good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest prog1.7
  (macrolet
   ((%m (z) z))
   (prog1 (expand-in-current-env (%m 'good))))
  good)

(def-macro-test prog1.error.1 (prog1 nil))
