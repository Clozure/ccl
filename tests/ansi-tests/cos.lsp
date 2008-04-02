;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb  9 20:53:42 2004
;;;; Contains: Tests of COS

(in-package :cl-test)

(deftest cos.1
  (loop for i from -1000 to 1000
	for rlist = (multiple-value-list (cos i))
	for y = (car rlist)
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (or (rationalp y) (typep y 'single-float))))
  t)

(deftest cos.2
  (loop for x = (- (random 2000.0s0) 1000.0s0)
	for rlist = (multiple-value-list (cos x))
	for y = (car rlist)
	repeat 1000
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (typep y 'short-float)))
  t)

(deftest cos.3
  (loop for x = (- (random 2000.0f0) 1000.0f0)
	for rlist = (multiple-value-list (cos x))
	for y = (car rlist)
	repeat 1000
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (typep y 'single-float)))
  t)

(deftest cos.4
  (loop for x = (- (random 2000.0d0) 1000.0d0)
	for rlist = (multiple-value-list (cos x))
	for y = (car rlist)
	repeat 1000
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (typep y 'double-float)))
  t)

(deftest cos.5
  (loop for x = (- (random 2000.0l0) 1000.0l0)
	for rlist = (multiple-value-list (cos x))
	for y = (car rlist)
	repeat 1000
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (typep y 'long-float)))
  t)

(deftest cos.6
  (let ((r (cos 0)))
    (or (eqlt r 1) (eqlt r 1.0)))
  t)

(deftest cos.7
  (cos 0.0s0)
  1.0s0)

(deftest cos.8
  (cos 0.0)
  1.0)

(deftest cos.9
  (cos 0.0d0)
  1.0d0)

(deftest cos.10
  (cos 0.0l0)
  1.0l0)

(deftest cos.11
  (loop for i from 1 to 100
	unless (approx= (cos i) (cos (coerce i 'single-float)))
	collect i)
  nil)

(deftest cos.12
  (approx= (cos (coerce (/ pi 2) 'single-float)) 0.0)
  t)

(deftest cos.13
  (approx= (cos (coerce (/ pi -2) 'single-float)) 0.0)
  t)

(deftest cos.14
  (approx= (cos (coerce (/ pi 2) 'short-float)) 0s0)
  t)

(deftest cos.15
  (approx= (cos (coerce (/ pi -2) 'short-float)) 0s0)
  t)

(deftest cos.16
  (approx= (cos (coerce (/ pi 2) 'double-float)) 0d0)
  t)

(deftest cos.17
  (approx= (cos (coerce (/ pi -2) 'double-float)) 0d0)
  t)

(deftest cos.18
  (approx= (cos (coerce (/ pi 2) 'long-float)) 0l0)
  t)

(deftest cos.19
  (approx= (cos (coerce (/ pi -2) 'long-float)) 0l0)
  t)

(deftest cos.20
  (loop for r = (- (random 2000) 1000)
	for i = (- (random 20) 10)
	for y = (cos (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest cos.21
  (loop for r = (- (random 2000.0s0) 1000.0s0)
	for i = (- (random 20.0s0) 10.0s0)
	for y = (cos (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest cos.22
  (loop for r = (- (random 2000.0f0) 1000.0f0)
	for i = (- (random 20.0f0) 10.0f0)
	for y = (cos (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest cos.23
  (loop for r = (- (random 2000.0d0) 1000.0d0)
	for i = (- (random 20.0d0) 10.0d0)
	for y = (cos (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest cos.24
  (loop for r = (- (random 2000.0l0) 1000.0l0)
	for i = (- (random 20.0l0) 10.0l0)
	for y = (cos (complex r i))
	repeat 1000
	always (numberp y))
  t)

;;; FIXME
;;; More accuracy tests here

;;; Error tests

(deftest cos.error.1
  (signals-error (cos) program-error)
  t)

(deftest cos.error.2
  (signals-error (cos 0.0 0.0) program-error)
  t)

(deftest cos.error.3
  (check-type-error #'cos #'numberp)
  nil)

