;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Aug 21 16:07:59 2003
;;;; Contains: Tests of FROUND

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "fround-aux.lsp")

;;; Error tests

(deftest fround.error.1
  (signals-error (fround) program-error)
  t)

(deftest fround.error.2
  (signals-error (fround 1.0 1 nil) program-error)
  t)

;;; Non-error tests

(deftest fround.1
  (fround.1-fn)
  nil)

(deftest fround.10
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (fround x x))
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

(deftest fround.11
  (loop for x in (remove-if-not #'floatp (remove-if #'zerop *reals*))
	for (q r) = (multiple-value-list (fround  (- x) x))
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

(deftest fround.12
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 0.5s0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (fround x))
	  unless (and (eql q (coerce i 'short-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest fround.13
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 0.5s0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (fround x))
	  unless (and (eql q (coerce i 'short-float))
		      (eql r (- rrad)))
	  collect (list i x q r)))
  nil)

(deftest fround.14
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 0.5f0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (fround x))
	  unless (and (eql q (coerce i 'single-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest fround.15
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 0.5f0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (fround x))
	  unless (and (eql q (coerce  i 'single-float))
		      (eql r (- rrad)))
	  collect (list i x q r)))
  nil)

(deftest fround.16
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 0.5d0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (fround x))
	  unless (and (eql q (coerce i 'double-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest fround.17
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 0.5d0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (fround x))
	  unless (and (eql q (coerce i 'double-float))
		      (eql r (- rrad)))
	  collect (list i x q r)))
  nil)

(deftest fround.18
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 0.5l0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (fround x))
	  unless (and (eql q (coerce i 'long-float))
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest fround.19
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 0.5l0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (fround x))
	  unless (and (eql q (coerce i 'long-float))
		      (eql r (- rrad)))
	  collect (list i x q r)))
  nil)
