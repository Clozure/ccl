;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 12 06:59:54 2003
;;;; Contains: Tests of FFLOOR

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "ffloor-aux.lsp")

(deftest ffloor.error.1
  (signals-error (ffloor) program-error)
  t)

(deftest ffloor.error.2
  (signals-error (ffloor 1.0 1 nil) program-error)
  t)

;;;

(deftest ffloor.1
  (ffloor.1-fn)
  nil)

(deftest ffloor.10
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (ffloor x x))
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

(deftest ffloor.11
  (loop for x in (remove-if-not #'floatp (remove-if #'zerop *reals*))
	for (q r) = (multiple-value-list (ffloor  (- x) x))
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

(deftest ffloor.12
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ffloor x))
	  unless (and (eql q (coerce i 'short-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ffloor.13
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ffloor x))
	  unless (and (eql q (coerce (1- i) 'short-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ffloor.14
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ffloor x))
	  unless (and (eql q (coerce i 'single-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ffloor.15
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ffloor x))
	  unless (and (eql q (coerce (1- i) 'single-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ffloor.16
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ffloor x))
	  unless (and (eql q (coerce i 'double-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ffloor.17
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ffloor x))
	  unless (and (eql q (coerce (1- i) 'double-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ffloor.18
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ffloor x))
	  unless (and (eql q (coerce i 'long-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest ffloor.19
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ffloor x))
	  unless (and (eql q (coerce (1- i) 'long-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

;;; To add: tests that involve adding/subtracting EPSILON constants
;;; (suitably scaled) to floated integers.
