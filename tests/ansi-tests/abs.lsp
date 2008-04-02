;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  1 20:16:42 2003
;;;; Contains: Tests of ABS

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest abs.error.1
  (signals-error (abs) program-error)
  t)

(deftest abs.error.2
  (signals-error (abs 0 0) program-error)
  t)

(deftest abs.error.3
  (signals-error (abs 0 nil nil) program-error)
  t)

(deftest abs.1
  (loop for x in *numbers*
	for a = (abs x)
	always (and (realp a) (not (minusp a))))
  t)

(deftest abs.2
  (loop for x = (random-fixnum)
	for a = (abs x)
	repeat 10000
	unless (if (plusp x) (eql x a) (eql (- x) a))
	collect (list x a))
  nil)

(deftest abs.3
  (let ((bound (ash 1 300)))
    (loop for x = (random-from-interval bound)
	  for a = (abs x)
	  repeat 10000
	  unless (if (plusp x) (eql x a) (eql (- x) a))
	  collect (list x a)))
  nil)

(deftest abs.4
  (loop for num = (random-fixnum)
	for den = (random-fixnum)
	for den2 = (if (zerop den) 1 den)
	for r = (/ num den)
	for a = (abs r)
	repeat 10000
	unless (if (>= r 0) (eql r a) (eql (- r) a))
	collect (list num den2 r a))
  nil)

(deftest abs.5
  (let ((bound (ash 1 210)))
    (loop
     for num = (random-from-interval bound)
     for den = (random-from-interval bound)
     for den2 = (if (zerop den) 1 den)
     for r = (/ num den)
     for a = (abs r)
     repeat 10000
     unless (if (>= r 0) (eql r a) (eql (- r) a))
     collect (list num den2 r a)))
  nil)

(deftest abs.6
  (let ((bound (float (ash 1 11) 1.0s0)))
    (loop
     for x = (random-from-interval bound)
     for a = (abs x)
     repeat 10000
     unless (if (minusp x)
		(eql (- x) a)
	      (eql x a))
     collect (list x a)))
  nil)

(deftest abs.7
  (let ((bound (float (ash 1 22) 1.0f0)))
    (loop
     for x = (random-from-interval bound)
     for a = (abs x)
     repeat 10000
     unless (if (minusp x)
		(eql (- x) a)
	      (eql x a))
     collect (list x a)))
  nil)

(deftest abs.8
  (let ((bound (float (ash 1 48) 1.0d0)))
    (loop
     for x = (random-from-interval bound)
     for a = (abs x)
     repeat 10000
     unless (if (minusp x)
		(eql (- x) a)
	      (eql x a))
     collect (list x a)))
  nil)

(deftest abs.9
  (let ((bound (float (ash 1 48) 1.0l0)))
    (loop
     for x = (random-from-interval bound)
     for a = (abs x)
     repeat 10000
     unless (if (minusp x)
		(eql (- x) a)
	      (eql x a))
     collect (list x a)))
  nil)

;;; The example on the abs page says that (abs -0.0) should be -0,0.
;;; However, FABS on the x86 returns 0.0 for that.  Since the examples
;;; in the hyperspec are not normative, the following four tests
;;; have been commented out.

;;; (deftest abs.10
;;;   (abs -0.0s0)
;;;   -0.0s0)
;;; 
;;; (deftest abs.11
;;;   (abs -0.0f0)
;;;   -0.0f0)
;;; 
;;; (deftest abs.12
;;;   (abs -0.0d0)
;;;   -0.0d0)
;;; 
;;; (deftest abs.13
;;;   (abs -0.0l0)
;;;   -0.0l0)

;;; Complex numbers

(deftest abs.14
  (let ((result (abs #c(3 4))))
    (=t result 5))
  t)

(deftest abs.15
  (let ((result (abs #c(-3 4))))
    (=t result 5))
  t)

(deftest abs.16
  (let ((result (abs #c(3 -4))))
    (=t result 5))
  t)

(deftest abs.17
  (let ((result (abs #c(-3 -4))))
    (=t result 5))
  t)

(deftest abs.18
  (abs #c(3.0s0 4.0s0))
  5.0s0)

(deftest abs.19
  (abs #c(3.0f0 -4.0f0))
  5.0f0)

(deftest abs.20
  (abs #c(-3.0d0 4.0d0))
  5.0d0)

(deftest abs.21
  (abs #c(-3.0l0 4.0l0))
  5.0l0)

(deftest abs.22
  (macrolet ((%m (z) z))
	    (abs (expand-in-current-env (%m -4))))
  4)
