;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  1 21:24:44 2003
;;;; Contains: Tests of EXP

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "exp-aux.lsp")

;;; Error tests

(deftest exp.error.1
  (signals-error (exp) program-error)
  t)

(deftest exp.error.2
  (signals-error (exp 0 nil) program-error)
  t)

(deftest exp.error.3
  (signals-error (exp 0 0 0) program-error)
  t)

;;; Other tests

(deftest exp.1
  (let ((result (exp 0)))
    (or (eqlt result 1)
	(eqlt result 1.0f0)))
  t)

(deftest exp.2
  (mapcar #'exp '(0.0s0 0.0f0 0.0d0 0.0l0))
  (1.0s0 1.0f0 1.0d0 1.0l0))

(deftest exp.3
  (mapcar #'exp '(-0.0s0 -0.0f0 -0.0d0 -0.0l0))
  (1.0s0 1.0f0 1.0d0 1.0l0))

;;; FIXME
;;; Add more tests here for floating point accuracy

(deftest exp.error.4
  (signals-error (exp (+ (log most-positive-short-float) 100))
		 floating-point-overflow)
  t)

(deftest exp.error.5
  (signals-error (exp (+ (log most-positive-single-float) 100))
		 floating-point-overflow)
  t)

(deftest exp.error.6
  (signals-error (exp (+ (log most-positive-double-float) 100))
		 floating-point-overflow)
  t)

(deftest exp.error.7
  (signals-error (exp (+ (log most-positive-long-float) 100))
		 floating-point-overflow)
  t)

(deftest exp.error.8
  (signals-error (exp (- (log least-positive-short-float) 100))
		 floating-point-underflow)
  t)

(deftest exp.error.9
  (signals-error (exp (- (log least-positive-single-float) 100))
		 floating-point-underflow)
  t)

(deftest exp.error.10
  (signals-error (exp (- (log least-positive-double-float) 100))
		 floating-point-underflow)
  t)

(deftest exp.error.11
  (signals-error (exp (- (log least-positive-double-float) 100))
		 floating-point-underflow)
  t)
