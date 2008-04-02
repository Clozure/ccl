;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Aug 21 13:39:56 2003
;;;; Contains: Tests of ROUND

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "round-aux.lsp")

(deftest round.error.1
  (signals-error (round) program-error)
  t)

(deftest round.error.2
  (signals-error (round 1.0 1 nil) program-error)
  t)

;;;

(deftest round.1
  (round.1-fn)
  nil)

(deftest round.2
  (round.2-fn)
  nil)

(deftest round.3
  (round.3-fn 2.0s4)
  nil)

(deftest round.4
  (round.3-fn 2.0f4)
  nil)

(deftest round.5
  (round.3-fn 2.0d4)
  nil)

(deftest round.6
  (round.3-fn 2.0l4)
  nil)

(deftest round.7
  (round.7-fn)
  nil)

(deftest round.8
  (round.8-fn)
  nil)

(deftest round.9
  (round.9-fn)
  nil)

(deftest round.10
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (round x x))
	unless (and (eql q 1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

(deftest round.11
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (round (- x) x))
	unless (and (eql q -1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

(deftest round.12
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 0.5s0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (round x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest round.13
  (let* ((radix (float-radix 1.0s0))
	 (rad (float radix 1.0s0))
	 (rrad (/ 0.5s0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (round x))
	  unless (and (eql q i)
		      (eql r (- rrad)))
	  collect (list i x q r)))
  nil)

(deftest round.14
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 0.5f0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (round x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest round.15
  (let* ((radix (float-radix 1.0f0))
	 (rad (float radix 1.0f0))
	 (rrad (/ 0.5f0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (round x))
	  unless (and (eql q i)
		      (eql r (- rrad)))
	  collect (list i x q r)))
  nil)

(deftest round.16
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 0.5d0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (round x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest round.17
  (let* ((radix (float-radix 1.0d0))
	 (rad (float radix 1.0d0))
	 (rrad (/ 0.5d0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (round x))
	  unless (and (eql q i)
		      (eql r (- rrad)))
	  collect (list i x q r)))
  nil)

(deftest round.18
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 0.5l0 rad)))
    (loop for i from 1 to 1000
	  for x = (+ i rrad)
	  for (q r) = (multiple-value-list (round x))
	  unless (and (eql q i)
		      (eql r rrad))
	  collect (list i x q r)))
  nil)

(deftest round.19
  (let* ((radix (float-radix 1.0l0))
	 (rad (float radix 1.0l0))
	 (rrad (/ 0.5l0 rad)))
    (loop for i from 1 to 1000
	  for x = (- i rrad)
	  for (q r) = (multiple-value-list (round x))
	  unless (and (eql q i)
		      (eql r (- rrad)))
	  collect (list i x q r)))
  nil)

(deftest round.20
  (round 1/2)
  0 1/2)

(deftest round.21
  (round 3/2)
  2 -1/2)
