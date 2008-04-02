;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb  9 20:55:40 2004
;;;; Contains: Tests of TAN

(in-package :cl-test)

(deftest tan.1
  (loop for i from -1000 to 1000
	for rlist = (multiple-value-list (tan i))
	for y = (car rlist)
	always (and (null (cdr rlist))
		    (or (rationalp y) (typep y 'single-float))))
  t)

(deftest tan.2
  (loop for x = (- (random 2000.0s0) 1000.0s0)
	for y = (safe-tan x 0.0s0)
	repeat 1000
	always (typep y 'short-float))
  t)

(deftest tan.3
  (loop for x = (- (random 2000.0f0) 1000.0f0)
	for y = (safe-tan x 0.0)
	repeat 1000
	always (typep y 'single-float))
  t)

(deftest tan.4
  (loop for x = (- (random 2000.0d0) 1000.0d0)
	for y = (safe-tan x 0.0d0)
	repeat 1000
	always (typep y 'double-float))
  t)

(deftest tan.5
  (loop for x = (- (random 2000.0l0) 1000.0l0)
	for y = (safe-tan 0.0l0)
	repeat 1000
	always (typep y 'long-float))
  t)

(deftest tan.6
  (let ((r (tan 0)))
    (or (eqlt r 0) (eqlt r 0.0)))
  t)

(deftest tan.7
  (tan 0.0s0)
  0.0s0)

(deftest tan.8
  (tan 0.0)
  0.0)

(deftest tan.9
  (tan 0.0d0)
  0.0d0)

(deftest tan.10
  (tan 0.0l0)
  0.0l0)

(deftest tan.11
  (loop for i from 1 to 100
	unless (approx= (tan i) (tan (coerce i 'single-float)))
	collect i)
  nil)

(deftest tan.12
  (approx= (tan (coerce (/ pi 4) 'single-float)) 1.0)
  t)

(deftest tan.13
  (approx= (tan (coerce (/ pi -4) 'single-float)) -1.0)
  t)

(deftest tan.14
  (approx= (tan (coerce (/ pi 4) 'short-float)) 1s0)
  t)

(deftest tan.15
  (approx= (tan (coerce (/ pi -4) 'short-float)) -1s0)
  t)

(deftest tan.16
  (approx= (tan (coerce (/ pi 4) 'double-float)) 1d0)
  t)

(deftest tan.17
  (approx= (tan (coerce (/ pi -4) 'double-float)) -1d0)
  t)

(deftest tan.18
  (approx= (tan (coerce (/ pi 4) 'long-float)) 1l0)
  t)

(deftest tan.19
  (approx= (tan (coerce (/ pi -4) 'long-float)) -1l0)
  t)

(deftest tan.20
  (loop for r = (- (random 2000) 1000)
	for i = (- (random 20) 10)
	for y = (safe-tan (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest tan.21
  (loop for r = (- (random 2000.0s0) 1000.0s0)
	for i = (- (random 20.0s0) 10.0s0)
	for y = (safe-tan (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest tan.22
  (loop for r = (- (random 2000.0f0) 1000.0f0)
	for i = (- (random 20.0f0) 10.0f0)
	for y = (safe-tan (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest tan.23
  (loop for r = (- (random 2000.0d0) 1000.0d0)
	for i = (- (random 20.0d0) 10.0d0)
	for y = (safe-tan (complex r i))
	repeat 1000
	always (numberp y))
  t)

(deftest tan.24
  (loop for r = (- (random 2000.0l0) 1000.0l0)
	for i = (- (random 20.0l0) 10.0l0)
	for y = (safe-tan (complex r i))
	repeat 1000
	always (numberp y))
  t)

;;; FIXME
;;; More accuracy tests here

;;; Error tests

(deftest tan.error.1
  (signals-error (tan) program-error)
  t)

(deftest tan.error.2
  (signals-error (tan 0.0 0.0) program-error)
  t)

(deftest tan.error.3
  (check-type-error #'tan #'numberp)
  nil)
