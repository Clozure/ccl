;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  8 20:21:19 2003
;;;; Contains: Tests of BOOLE and associated constants

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(defparameter *boole-val-names*
  '(boole-1 boole-2 boole-and boole-andc1 boole-andc2
    boole-c1 boole-c2 boole-clr boole-eqv boole-ior
    boole-nand boole-nor boole-orc1 boole-orc2 boole-set boole-xor))

(defparameter *boole-vals*
  (list boole-1 boole-2 boole-and boole-andc1 boole-andc2
	boole-c1 boole-c2 boole-clr boole-eqv boole-ior	boole-nand
	boole-nor boole-orc1 boole-orc2 boole-set boole-xor))

(defparameter *boole-fns*
  (list #'(lambda (x y) (declare (ignore y)) x)
	#'(lambda (x y) (declare (ignore x)) y)
	#'logand
	#'logandc1
	#'logandc2
	#'(lambda (x y) (declare (ignore y)) (lognot x))
	#'(lambda (x y) (declare (ignore x)) (lognot y))
	(constantly 0)
	#'logeqv
	#'logior
	#'lognand
	#'lognor
	#'logorc1
	#'logorc2
	(constantly -1)
	#'logxor))

(deftest boole.error.1
  (signals-error (boole) program-error)
  t)

(deftest boole.error.2
  (signals-error (boole boole-1) program-error)
  t)

(deftest boole.error.3
  (signals-error (boole boole-1 1) program-error)
  t)

(deftest boole.error.4
  (signals-error (boole boole-1 1 2 nil) program-error)
  t)

(deftest boole.error.5
  (let ((bad (loop for i from 1 until (not (member i *boole-vals*)))))
    (eval `(signals-type-error x ',bad (boole x 1 1))))
  t)

(deftest boole.error.6
  (loop for n in *boole-val-names*
	unless (eval `(signals-type-error x nil (boole ,n nil 1)))
	collect n)
  nil)

(deftest boole.error.7
  (loop for n in *boole-val-names*
	unless (eval `(signals-type-error x nil (boole ,n 1 nil)))
	collect n)
  nil)

(deftest boole.1
  (loop for v in *boole-vals*
	for fn of-type function in *boole-fns*
	for n in *boole-val-names*
	nconc
	(loop for x = (random-fixnum)
	      for y = (random-fixnum)
	      for result1 = (funcall (the function fn) x y)
	      for vals = (multiple-value-list (boole v x y))
	      for result2 = (car vals)
	      repeat 100
	      unless (and (= (length vals) 1) (eql result1 result2))
	      collect (list n x y result1 result2)))
  nil)

(deftest boole.2
  (loop for v in *boole-vals*
	for fn of-type function in *boole-fns*
	for n in *boole-val-names*
	nconc
	(loop for x = (random-from-interval 1000000000000000)
	      for y = (random-from-interval 1000000000000000)
	      for result1 = (funcall (the function fn) x y)
	      for vals = (multiple-value-list (boole v x y))
	      for result2 = (car vals)
	      repeat 100
	      unless (and (= (length vals) 1) (eql result1 result2))
	      collect (list n x y result1 result2)))
  nil)

(deftest boole.3
  (loop for n in *boole-val-names*
	for fn of-type function in *boole-fns*
	for fn2 = (compile nil `(lambda (x y) (declare (type fixnum x y))
				  (boole ,n x y)))
	nconc
	(loop for x = (random-fixnum)
	      for y = (random-fixnum)
	      for result1 = (funcall (the function fn) x y)
	      for vals = (multiple-value-list (funcall fn2 x y))
	      for result2 = (car vals)
	      repeat 100
	      unless (and (= (length vals) 1) (eql result1 result2))
	      collect (list n x y result1 result2)))
  nil)

(deftest boole.4
  (macrolet ((%m (z) z))
	    (values (boole (expand-in-current-env (%m boole-and)) #b11001100 #b01011010)
		    (boole boole-and (expand-in-current-env (%m #b11001100)) #b01011010)
		    (boole boole-and #b11001100 (expand-in-current-env (%m #b01011010)))))
  #b01001000
  #b01001000
  #b01001000)

;;; Order of evaluation
(deftest boole.order.1
  (let ((i 0) a b c)
    (values
     (boole
      (progn (setf a (incf i)) boole-and)
      (progn (setf b (incf i)) #b1101)
      (progn (setf c (incf i)) #b11001))
     i a b c))
  #b1001 3 1 2 3)

;;; Constants are constants

(deftest boole.constants.1
  (eqlt (length *boole-vals*)
	(length (remove-duplicates *boole-vals*)))
  t)

(deftest boole.constants.2
  (remove-if #'constantp *boole-val-names*)
  nil)

(deftest boole.constants.3
  (remove-if #'boundp *boole-val-names*)
  nil)
