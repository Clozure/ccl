;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May  8 19:53:39 2005
;;;; Contains: Tests of SLEEP

(in-package :cl-test)

(deftest sleep.1
  (sleep 0)
  nil)

(deftest sleep.2
  (sleep 0.0s0)
  nil)

(deftest sleep.3
  (sleep 0.0f0)
  nil)

(deftest sleep.4
  (sleep 0.0d0)
  nil)

(deftest sleep.5
  (sleep 0.0l0)
  nil)

(deftest sleep.6
  (sleep 1.0f-8)
  nil)

(deftest sleep.7
  (sleep 1/100)
  nil)

(deftest sleep.8
  (sleep (/ internal-time-units-per-second))
  nil)

(deftest sleep.9
  (sleep (/ 1000000000000000000000000000000))
  nil)

(deftest sleep.10
  (sleep least-positive-short-float)
  nil)

(deftest sleep.11
  (sleep least-positive-single-float)
  nil)

(deftest sleep.12
  (sleep least-positive-double-float)
  nil)

(deftest sleep.13
  (sleep least-positive-long-float)
  nil)

;;; Error cases

(deftest sleep.error.1
  (signals-error (sleep) program-error)
  t)

(deftest sleep.error.2
  (signals-error (sleep 100 nil) program-error)
  t)

(deftest sleep.error.3
  (check-type-error #'sleep #'(lambda (x) (and (realp x) (>= x 0))))
  nil)



