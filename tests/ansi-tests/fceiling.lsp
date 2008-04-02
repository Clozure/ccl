;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 20 06:22:23 2003
;;;; Contains: Tests of FCEILING

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "fceiling-aux.lsp")

(deftest fceiling.error.1
  (signals-error (fceiling) program-error)
  t)

(deftest fceiling.error.2
  (signals-error (fceiling 1.0 1 nil) program-error)
  t)

;;;

(deftest fceiling.1
  (fceiling.1-fn)
  nil)

(deftest fceiling.10
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (fceiling x x))
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

(deftest fceiling.11
  (loop for x in (remove-if-not #'floatp (remove-if #'zerop *reals*))
	for (q r) = (multiple-value-list (fceiling  (- x) x))
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

(deftest fceiling.12
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (fceiling x))
	  unless (and (eql q (coerce (1+ i) 'short-float))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest fceiling.13
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (fceiling x))
	  unless (and (eql q (coerce i 'short-float))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest fceiling.14
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (fceiling x))
	  unless (and (eql q (coerce (1+ i) 'single-float))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest fceiling.15
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (fceiling x))
	  unless (and (eql q (coerce i 'single-float))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest fceiling.16
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (fceiling x))
	  unless (and (eql q (coerce (1+ i) 'double-float))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest fceiling.17
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (fceiling x))
	  unless (and (eql q (coerce i 'double-float))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest fceiling.18
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (fceiling x))
	  unless (and (eql q (coerce (1+ i) 'long-float))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest fceiling.19
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (fceiling x))
	  unless (and (eql q (coerce i 'long-float))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)
