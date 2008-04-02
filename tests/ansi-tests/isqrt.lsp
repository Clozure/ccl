;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 15:40:09 2003
;;;; Contains: Tests of ISQRT

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest isqrt.error.1
  (signals-error (isqrt) program-error)
  t)

(deftest isqrt.error.2
  (signals-error (isqrt 0 0) program-error)
  t)

(deftest isqrt.error.3
  (signals-error (isqrt 0 nil) program-error)
  t)

(deftest isqrt.error.4
  (signals-error (isqrt 0 0 0) program-error)
  t)

(deftest isqrt.error.5
  (loop for x in *mini-universe*
	unless (or (and (integerp x) (>= x 0))
		   (eval `(signals-type-error x ',x (isqrt x))))
	collect x)
  nil)

;;; Non-error tests

(deftest isqrt.1
  (loop for i from 0 to 10000
	for i2 = (* i i)
	for s = (isqrt i2)
	unless (eql s i)
	collect i)
  nil)

(deftest isqrt.2
  (loop for i = (random-from-interval most-positive-fixnum 0)
	for s = (isqrt i)
	repeat 1000
	unless (and (integerp s)
		    (>= s 0)
		    (<= (* s s) i)
		    (> (* (1+ s) (1+ s)) i))
	collect (list i s))
  nil)

(deftest isqrt.3
  (loop for i = (random-from-interval 1000000000000000 0)
	for s = (isqrt i)
	repeat 1000
	unless (and (integerp s)
		    (>= s 0)
		    (<= (* s s) i)
		    (> (* (1+ s) (1+ s)) i))
	collect (list i s))
  nil)
