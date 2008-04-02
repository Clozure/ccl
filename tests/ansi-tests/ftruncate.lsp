;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 20 06:36:35 2003
;;;; Contains: Tests of FTRUNCATE

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "ftruncate-aux.lsp")

;;; Error tests

(deftest ftruncate.error.1
  (signals-error (ftruncate) program-error)
  t)

(deftest ftruncate.error.2
  (signals-error (ftruncate 1.0 1 nil) program-error)
  t)

;;; Non-error tests

(deftest ftruncate.1
  (ftruncate.1-fn)
  nil)

(deftest ftruncate.10
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (ftruncate x x))
	unless (and (floatp q)
		    (if (floatp x)
			(eql q (float 1 x))
		      (= q 1))
		    (zerop r)
		    (if (floatp x)
			(eql r (float 0 x))
		      (= r 0)))
	collect x)
  nil)

(deftest ftruncate.11
  (loop for x in (remove-if-not #'floatp (remove-if #'zerop *reals*))
	for (q r) = (multiple-value-list (ftruncate  (- x) x))
	unless (and (floatp q)
		    (if (floatp x)
			(eql q (float -1 x))
		      (= q -1))
		    (zerop r)
		    (if (floatp x)
			(eql r (float 0 x))
		      (= r 0)))
	collect x)
  nil)

(deftest ftruncate.12
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ftruncate x))
	  unless (and (eql q (coerce i 'short-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ftruncate.13
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ftruncate x))
	  unless (and (eql q (coerce (1- i) 'short-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ftruncate.14
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ftruncate x))
	  unless (and (eql q (coerce i 'single-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ftruncate.15
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ftruncate x))
	  unless (and (eql q (coerce (1- i) 'single-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ftruncate.16
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ftruncate x))
	  unless (and (eql q (coerce i 'double-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ftruncate.17
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ftruncate x))
	  unless (and (eql q (coerce (1- i) 'double-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ftruncate.18
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ftruncate x))
	  unless (and (eql q (coerce i 'long-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ftruncate.19
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ftruncate x))
	  unless (and (eql q (coerce (1- i) 'long-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

;;; To add: tests that involve adding/subtracting EPSILON constants
;;; (suitably scaled) to floated integers.
