;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 29 08:25:46 2005
;;;; Contains: Tests of TYPE declarations

(in-package :cl-test)

;;; Also of implicit type declarations

(deftest type.1
  (let ((x 1))
    (declare (type (integer 0 1) x))
    (values
     x
     (setq x 0)
     (1+ x)))
  1 0 1)

(deftest type.2
  (let ((x 1))
    (declare (type (integer -1 1) x))
    (locally (declare (type (integer 0 2) x))
	     (values
	      x
	      (setq x 0)
	      (1+ x))))
  1 0 1)

(deftest type.3
  (loop for x in *mini-universe*
	for tp = (type-of x)
	for form = `(let ((y ',x))
		      (declare (type ,tp y))
		      y)
	for val = (eval form)
	unless (eql val x)
	collect (list x tp form val))
  nil)

(deftest type.4
  (loop for x in *mini-universe*
	for tp = (type-of x)
	for form = `(let ((y ',x))
		      (declare (,tp y))
		      y)
	for val = (eval form)
	unless (eql val x)
	collect (list x tp form val))
  nil)

(deftest type.5
  (loop for x in *mini-universe*
	for class = (class-of x)
	for form = `(let ((y ',x))
		      (declare (,class y))
		      y)
	for val = (eval form)
	unless (eql val x)
	collect (list x class form val))
  nil)

;;; Free TYPE declaration
;;; It should not apply to the occurence of X in the form
;;; whose value is being bound to Y.

(deftest type.6
  (let ((x 2))
    (let ((y (+ (decf x) 2)))
      (declare (type (integer 0 1) x))
      (values x y)))
  1 3)
