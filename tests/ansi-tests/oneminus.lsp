;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  1 20:14:34 2003
;;;; Contains: Tests of 1-

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest 1-.error.1
  (signals-error (1-) program-error)
  t)

(deftest 1-.error.2
  (signals-error (1- 0 0) program-error)
  t)

(deftest 1-.error.3
  (signals-error (1- 0 nil nil) program-error)
  t)

;;; Non-error tests

(deftest 1-.1
  (loop for x = (random-fixnum)
	for y = (1- x)
	for z = (- x 1)
	repeat 1000
	unless (eql y z)
	collect (list x y z))
  nil)

(deftest 1-.2
  (loop for x = (random-from-interval (ash 1 1000))
	for y = (1- x)
	for z = (- x 1)
	repeat 1000
	unless (eql y z)
	collect (list x y z))
  nil)

(deftest 1-.3
  (loop for x = (random (1- most-positive-short-float))
	for y = (1- x)
	for z = (- x 1.0s0)
	repeat 1000
	unless (eql y z)
	collect (list x y z))
  nil)

(deftest 1-.4
  (loop for x = (random (1- most-positive-single-float))
	for y = (1- x)
	for z = (- x 1.0f0)
	repeat 1000
	unless (eql y z)
	collect (list x y z))
  nil)

(deftest 1-.5
  (loop for x = (random (1- most-positive-double-float))
	for y = (1- x)
	for z = (- x 1.0d0)
	repeat 1000
	unless (eql y z)
	collect (list x y z))
  nil)

(deftest 1-.6
  (loop for x = (random (1- most-positive-long-float))
	for y = (1- x)
	for z = (- x 1.0l0)
	repeat 1000
	unless (eql y z)
	collect (list x y z))
  nil)

(deftest 1-.7
  (loop for x = (random-fixnum)
	for y = (random-fixnum)
	for y2 = (if (zerop y) 1 y)
	for r = (/ x y2)
	for r1 = (1- r)
	for r2 = (- r 1)
	repeat 1000
	unless (eql r1 r2)
	collect (list x y2 r1 r2))
  nil)

(deftest 1-.8
  (let ((bound (ash 1 200)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound)
	  for y2 = (if (zerop y) 1 y)
	  for r = (/ x y2)
	  for r1 = (1- r)
	  for r2 = (- r 1)
	  repeat 1000
	  unless (eql r1 r2)
	  collect (list x y2 r1 r2)))
  nil)

;;; Complex numbers
(deftest 1-.9
  (loop for xr = (random-fixnum)
	for xi = (random-fixnum)
	for xc = (complex xr xi)
	for xc1 = (1- xc)
	repeat 1000
	unless (eql xc1 (complex (- xr 1) xi))
	collect (list xr xi xc xc1))
  nil)


(deftest 1-.10
  (let ((bound (ash 1 100)))
    (loop for xr = (random-from-interval bound)
	  for xi = (random-from-interval bound)
	  for xc = (complex xr xi)
	  for xc1 = (1- xc)
	  repeat 1000
	  unless (eql xc1 (complex (- xr 1) xi))
	  collect (list xr xi xc xc1)))
  nil)

(deftest 1-.11
  (let ((bound (1- most-positive-short-float)))
    (loop for xr = (random bound)
	  for xi = (random bound)
	  for xc = (complex xr xi)
	  for xc1 = (1- xc)
	  repeat 1000
	  unless (eql xc1 (complex (- xr 1) xi))
	  collect (list xr xi xc xc1)))
  nil)

(deftest 1-.12
  (let ((bound (1- most-positive-single-float)))
    (loop for xr = (random bound)
	  for xi = (random bound)
	  for xc = (complex xr xi)
	  for xc1 = (1- xc)
	  repeat 1000
	  unless (eql xc1 (complex (- xr 1) xi))
	  collect (list xr xi xc xc1)))
  nil)

(deftest 1-.13
  (let ((bound (1- most-positive-double-float)))
    (loop for xr = (random bound)
	  for xi = (random bound)
	  for xc = (complex xr xi)
	  for xc1 = (1- xc)
	  repeat 1000
	  unless (eql xc1 (complex (- xr 1) xi))
	  collect (list xr xi xc xc1)))
  nil)

(deftest 1-.14
  (let ((bound (1- most-positive-long-float)))
    (loop for xr = (random bound)
	  for xi = (random bound)
	  for xc = (complex xr xi)
	  for xc1 = (1- xc)
	  repeat 1000
	  unless (eql xc1 (complex (-  xr 1) xi))
	  collect (list xr xi xc xc1)))
  nil)

(deftest 1-.15
  (macrolet ((%m (z) z)) (1- (expand-in-current-env (%m 2))))
  1)
