;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb  9 20:20:46 2004
;;;; Contains: Tests for SIN

(in-package :cl-test)

(deftest sin.1
  (loop for i from -1000 to 1000
	for rlist = (multiple-value-list (sin i))
	for y = (car rlist)
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (or (rationalp y) (typep y 'single-float))))
  t)

(deftest sin.2
  (loop for x = (- (random 2000.0s0) 1000.0s0)
	for rlist = (multiple-value-list (sin x))
	for y = (car rlist)
	repeat 1000
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (typep y 'short-float)))
  t)

(deftest sin.3
  (loop for x = (- (random 2000.0f0) 1000.0f0)
	for rlist = (multiple-value-list (sin x))
	for y = (car rlist)
	repeat 1000
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (typep y 'single-float)))
  t)

(deftest sin.4
  (loop for x = (- (random 2000.0d0) 1000.0d0)
	for rlist = (multiple-value-list (sin x))
	for y = (car rlist)
	repeat 1000
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (typep y 'double-float)))
  t)

(deftest sin.5
  (loop for x = (- (random 2000.0l0) 1000.0l0)
	for rlist = (multiple-value-list (sin x))
	for y = (car rlist)
	repeat 1000
	always (and (null (cdr rlist))
		    (<= -1 y 1)
		    (typep y 'long-float)))
  t)

(deftest sin.6
  (let ((r (sin 0)))
    (or (eqlt r 0) (eqlt r 0.0)))
  t)

(deftest sin.7
  (sin 0.0s0)
  0.0s0)

(deftest sin.8
  (sin 0.0)
  0.0)

(deftest sin.9
  (sin 0.0d0)
  0.0d0)

(deftest sin.10
  (sin 0.0l0)
  0.0l0)

(deftest sin.11
  (loop for i from 1 to 100
	unless (approx= (sin i) (sin (coerce i 'single-float)))
	collect i)
  nil)

(deftest sin.12
  (approx= (sin (coerce (/ pi 2) 'single-float)) 1.0)
  t)

(deftest sin.13
  (approx= (sin (coerce (/ pi -2) 'single-float)) -1.0)
  t)

(deftest sin.14
  (approx= (sin (coerce (/ pi 2) 'short-float)) 1.0s0)
  t)

(deftest sin.15
  (approx= (sin (coerce (/ pi -2) 'short-float)) -1.0s0)
  t)

(deftest sin.16
  (approx= (sin (coerce (/ pi 2) 'double-float)) 1.0d0)
  t)

(deftest sin.17
  (approx= (sin (coerce (/ pi -2) 'double-float)) -1.0d0)
  t)

(deftest sin.18
  (approx= (sin (coerce (/ pi 2) 'long-float)) 1.0l0)
  t)

(deftest sin.19
  (approx= (sin (coerce (/ pi -2) 'long-float)) -1.0l0)
  t)

(deftest sin.20
  (loop for r = (- (random 2000) 1000)
	for i = (- (random 20) 10)
	for y = (sin (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest sin.21
  (loop for r = (- (random 2000.0s0) 1000.0s0)
	for i = (- (random 20.0s0) 10.0s0)
	for y = (sin (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest sin.22
  (loop for r = (- (random 2000.0f0) 1000.0f0)
	for i = (- (random 20.0f0) 10.0f0)
	for y = (sin (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest sin.23
  (loop for r = (- (random 2000.0d0) 1000.0d0)
	for i = (- (random 20.0d0) 10.0d0)
	for y = (sin (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest sin.24
  (loop for r = (- (random 2000.0l0) 1000.0l0)
	for i = (- (random 20.0l0) 10.0l0)
	for y = (sin (complex r i))
	repeat 1000
	always (numberp y))
  t)

;;; FIXME
;;; More accuracy tests here

;;; Error tests

(deftest sin.error.1
  (signals-error (sin) program-error)
  t)

(deftest sin.error.2
  (signals-error (sin 0.0 0.0) program-error)
  t)

(deftest sin.error.3
  (check-type-error #'sin #'numberp)
  nil)
