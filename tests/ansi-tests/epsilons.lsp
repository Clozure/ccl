;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 20 22:05:20 2003
;;;; Contains: Tests of the EPSILON constants

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest epsilons.1
  (loop for e in (list short-float-epsilon single-float-epsilon
		       double-float-epsilon long-float-epsilon)
	when (= (float 1 e) (+ (float 1 e) e))
	collect e)
  nil)

(deftest epsilons.2
  (loop for e in (list short-float-negative-epsilon
		       single-float-negative-epsilon
		       double-float-negative-epsilon
		       long-float-negative-epsilon)
	when (= (float 1 e) (- (float 1 e) e))
	collect e)
  nil)

(deftest epsilons.3
  (loop for e in (list short-float-epsilon single-float-epsilon
		       double-float-epsilon long-float-epsilon)
	unless (= (float 1 e) (+ (float 1 e) (/ e 2)))
	collect e)
  nil)

(deftest epsilons.4
  (loop for e in (list short-float-negative-epsilon
		       single-float-negative-epsilon
		       double-float-negative-epsilon
		       long-float-negative-epsilon)
	unless (= (float 1 e) (- (float 1 e) (/ e 2)))
	collect e)
  nil)

(deftest epsilons.5
  (loop for (type var) in
	'(
	  (short-float short-float-epsilon)
	  (short-float short-float-negative-epsilon)
	  (single-float single-float-epsilon)
	  (single-float single-float-negative-epsilon)
	  (double-float double-float-epsilon)
	  (double-float double-float-negative-epsilon)
	  (long-float long-float-epsilon)
	  (long-float long-float-negative-epsilon))
	for val = (symbol-value var)
	unless (typep val type)
	collect (list type var val))
  nil)

(deftest epsilons.6
  (flet ((%check (x) (/= 1.0s0 (+ 1.0s0 x))))
    (let ((eps (float-binary-search #'%check 0.0s0 1.0s0)))
      (if (= eps short-float-epsilon)
	  :good
	(list eps short-float-epsilon))))
  :good)

(deftest epsilons.7
  (flet ((%check (x) (/= 1.0f0 (+ 1.0f0 x))))
    (let ((eps (float-binary-search #'%check 0.0f0 1.0f0)))
      (if (= eps single-float-epsilon)
	  :good
	(list eps single-float-epsilon))))
  :good)

(deftest epsilons.8
  (flet ((%check (x) (/= 1.0d0 (+ 1.0d0 x))))
    (let ((eps (float-binary-search #'%check 0.0d0 1.0d0)))
      (if (= eps double-float-epsilon)
	  :good
	(list eps double-float-epsilon))))
  :good)

(deftest epsilons.9
  (flet ((%check (x) (/= 1.0l0 (+ 1.0l0 x))))
    (let ((eps (float-binary-search #'%check 0.0l0 1.0l0)))
      (if (= eps long-float-epsilon)
	  :good
	(list eps long-float-epsilon))))
  :good)

(deftest epsilons.10
  (flet ((%check (x) (/= 1.0s0 (- 1.0s0 x))))
    (let ((eps (float-binary-search #'%check 0.0s0 1.0s0)))
      (if (= eps short-float-negative-epsilon)
	  :good
	(list eps short-float-negative-epsilon))))
  :good)

(deftest epsilons.11
  (flet ((%check (x) (/= 1.0f0 (- 1.0f0 x))))
    (let ((eps (float-binary-search #'%check 0.0f0 1.0f0)))
      (if (= eps single-float-negative-epsilon)
	  :good
	(list eps single-float-negative-epsilon))))
  :good)

(deftest epsilons.12
  (flet ((%check (x) (/= 1.0d0 (- 1.0d0 x))))
    (let ((eps (float-binary-search #'%check 0.0d0 1.0d0)))
      (if (= eps double-float-negative-epsilon)
	  :good
	(list eps double-float-negative-epsilon))))
  :good)

(deftest epsilons.13
  (flet ((%check (x) (/= 1.0l0 (- 1.0l0 x))))
    (let ((eps (float-binary-search #'%check 0.0l0 1.0l0)))
      (if (= eps long-float-negative-epsilon)
	  :good
	(list eps long-float-negative-epsilon))))
  :good)





	