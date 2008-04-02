;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 20 05:13:26 2003
;;;; Contains: Tests of TRUNCATE

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "truncate-aux.lsp")

(deftest truncate.error.1
  (signals-error (truncate) program-error)
  t)

(deftest truncate.error.2
  (signals-error (truncate 1.0 1 nil) program-error)
  t)

;;;

(deftest truncate.1
  (truncate.1-fn)
  nil)

(deftest truncate.2
  (truncate.2-fn)
  nil)

(deftest truncate.3
  (truncate.3-fn 2.0s4)
  nil)

(deftest truncate.4
  (truncate.3-fn 2.0f4)
  nil)

(deftest truncate.5
  (truncate.3-fn 2.0d4)
  nil)

(deftest truncate.6
  (truncate.3-fn 2.0l4)
  nil)

(deftest truncate.7
  (truncate.7-fn)
  nil)

(deftest truncate.8
  (truncate.8-fn)
  nil)

(deftest truncate.9
  (truncate.9-fn)
  nil)

(deftest truncate.10
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (truncate x x))
	unless (and (eql q 1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

(deftest truncate.11
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (truncate (- x) x))
	unless (and (eql q -1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

(deftest truncate.12
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (truncate x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest truncate.13
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (truncate x))
	  unless (and (eql q (1- i))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest truncate.14
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (truncate x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest truncate.15
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (truncate x))
	  unless (and (eql q (1- i))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest truncate.16
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (truncate x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest truncate.17
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (truncate x))
	  unless (and (eql q (1- i))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest truncate.18
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (truncate x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest truncate.19
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (truncate x))
	  unless (and (eql q (1- i))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)
