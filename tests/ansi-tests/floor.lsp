;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug  4 22:16:00 2003
;;;; Contains: Tests of FLOOR

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "floor-aux.lsp")

;;; Error tests

(deftest floor.error.1
  (signals-error (floor) program-error)
  t)

(deftest floor.error.2
  (signals-error (floor 1.0 1 nil) program-error)
  t)

;;; Non-error tests

(deftest floor.1
  (floor.1-fn)
  nil)

(deftest floor.2
  (floor.2-fn)
  nil)

(deftest floor.3
  (floor.3-fn 2.0s4)
  nil)

(deftest floor.4
  (floor.3-fn 2.0f4)
  nil)

(deftest floor.5
  (floor.3-fn 2.0d4)
  nil)

(deftest floor.6
  (floor.3-fn 2.0l4)
  nil)

(deftest floor.7
  (floor.7-fn)
  nil)

(deftest floor.8
  (floor.8-fn)
  nil)

(deftest floor.9
  (floor.9-fn)
  nil)

(deftest floor.10
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (floor x x))
	unless (and (eql q 1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

(deftest floor.11
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (floor (- x) x))
	unless (and (eql q -1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

(deftest floor.12
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (floor x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest floor.13
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (floor x))
	  unless (and (eql q (1- i))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest floor.14
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (floor x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest floor.15
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (floor x))
	  unless (and (eql q (1- i))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest floor.16
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (floor x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest floor.17
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (floor x))
	  unless (and (eql q (1- i))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest floor.18
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (floor x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest floor.19
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (floor x))
	  unless (and (eql q (1- i))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

;;; To add: tests that involve adding/subtracting EPSILON constants
;;; (suitably scaled) to floated integers.

