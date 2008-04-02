;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 20:23:15 2003
;;;; Contains: Tests of DEPOSIT-FIELD

(in-package :cl-test)

;;; Error tests

(deftest deposit-field.error.1
  (signals-error (deposit-field) program-error)
  t)

(deftest deposit-field.error.2
  (signals-error (deposit-field 1) program-error)
  t)

(deftest deposit-field.error.3
  (signals-error (deposit-field 1 (byte 1 0)) program-error)
  t)

(deftest deposit-field.error.4
  (signals-error (deposit-field 1 (byte 1 0) 0 nil) program-error)
  t)

;;; Non-error tests

(deftest deposit-field.1
  (loop for pos = (random 32)
	for size = (random 32)
	for newbyte = (random (ash 1 (+ pos size)))
	for val = (random (1+ (random (ash 1 (+ pos size)))))
	for result = (deposit-field newbyte (byte size pos) val)
	repeat 100
	unless
	(loop for i from 0 to (+ pos size)
	      always (if (or (< i pos)
			     (>= i (+ pos size)))
			 (if (logbitp i val) (logbitp i result)
			   (not (logbitp i result)))
		       (if (logbitp i newbyte) (logbitp i result)
			   (not (logbitp i result)))))
	collect (list pos size newbyte val result))
  nil)

(deftest deposit-field.2
  (loop for pos = (random 1000)
	for size = (random 1000)
	for newbyte = (random (ash 1 (+ pos size)))
	for val = (random (1+ (random (ash 1 (+ pos size)))))
	for result = (deposit-field newbyte (byte size pos) val)
	repeat 100
	unless
	(loop for i from 0 to (+ pos size)
	      always (if (or (< i pos)
			     (>= i (+ pos size)))
			 (if (logbitp i val) (logbitp i result)
			   (not (logbitp i result)))
		       (if (logbitp i newbyte) (logbitp i result)
			   (not (logbitp i result)))))
	collect (list pos size newbyte val result))
  nil)

(deftest deposit-field.3
  (loop for x = (random-fixnum)
	for y = (random-fixnum)
	for pos = (random 32)
	repeat 100
	always (= (deposit-field x (byte 0 pos) y) y))
  t)

(deftest deposit-field.4
  (let ((bound (ash 1 200)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound)
	  for pos = (random 200)
	  repeat 100
	  always (= (deposit-field x (byte 0 pos) y) y)))
  t)

(deftest deposit-field.5
  (loop for i of-type fixnum from -1000 to 1000
	always (eql (deposit-field -1 (byte 0 0) i) i))
  t)
