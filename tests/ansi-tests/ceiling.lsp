;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 19 06:50:44 2003
;;;; Contains: Tests of CEILING

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "ceiling-aux.lsp")

(deftest ceiling.error.1
  (signals-error (ceiling) program-error)
  t)

(deftest ceiling.error.2
  (signals-error (ceiling 1.0 1 nil) program-error)
  t)

;;;

(deftest ceiling.1
  (ceiling.1-fn)
  nil)

(deftest ceiling.2
  (ceiling.2-fn)
  nil)

(deftest ceiling.3
  (ceiling.3-fn 2.0s4)
  nil)

(deftest ceiling.4
  (ceiling.3-fn 2.0f4)
  nil)

(deftest ceiling.5
  (ceiling.3-fn 2.0d4)
  nil)

(deftest ceiling.6
  (ceiling.3-fn 2.0l4)
  nil)

(deftest ceiling.7
  (ceiling.7-fn)
  nil)

(deftest ceiling.8
  (ceiling.8-fn)
  nil)

(deftest ceiling.9
  (ceiling.9-fn)
  nil)

(deftest ceiling.10
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (ceiling x x))
	unless (and (eql q 1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

(deftest ceiling.11
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (ceiling (- x) x))
	unless (and (eql q -1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

(deftest ceiling.12
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ceiling x))
	  unless (and (eql q (1+ i))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest ceiling.13
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 1.0s0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ceiling x))
	  unless (and (eql q i)
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest ceiling.14
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ceiling x))
	  unless (and (eql q (1+ i))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest ceiling.15
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 1.0f0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ceiling x))
	  unless (and (eql q  i)
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest ceiling.16
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ceiling x))
	  unless (and (eql q (1+ i))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest ceiling.17
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 1.0d0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ceiling x))
	  unless (and (eql q i)
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest ceiling.18
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (ceiling x))
	  unless (and (eql q (1+ i))
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

(deftest ceiling.19
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 1.0l0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (ceiling x))
	  unless (and (eql q i)
		      (eql r (- rrad 1)))
	  collect (list i x q r)))
  nil)

;;; To add: tests that involve adding/subtracting EPSILON constants
;;; (suitably scaled) to floated integers.


