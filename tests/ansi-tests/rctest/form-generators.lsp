;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 21 10:56:09 2003
;;;; Contains: Generators for forms

(in-package :rctest)

(defclass form-generator (composite-generator) ())

(defparameter *form-generator* (make-instance 'composite-generator))

(defclass implicit-progn-generator (random-iterative-generator)
  ((subgenerator :initform *form-generator*)))

(defgenerator var-form-generator
  :keys (vars)
  :body (random-from-seq vars))

(defgenerator int-form-generator
  :body (random-case
	 0
	 (random-from-seq
	  #.(apply #'vector (loop for i from 0 to 31 collect (ash 1 i))))
	 (random-from-seq
	  #.(apply #'vector (loop for i from 0 to 31 collect (- (ash 1 i)))))
	 (random-from-seq
	  #.(make-array 128 :initial-contents
			(loop for i from 0 to 31
			      for x = (ash 1 i)
			      nconc (list (1- x) (1+ x) (- 1 x) (- -1 x)))))
	 (random 1000)))
