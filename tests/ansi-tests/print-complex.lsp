;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Mar  3 06:44:04 2004
;;;; Contains: Tests of printing complex numbers

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest print.complex.1
  (equalt
   (with-standard-io-syntax
     (let ((*print-readably* nil))
       (with-output-to-string (s) (prin1 (complex 1 2) s))))
   "#C(1 2)")
  t)

(deftest print.complex.2
  (equalt
   (with-standard-io-syntax
     (let ((*print-readably* nil))
       (with-output-to-string (s) (prin1 (complex 1.0 2.0) s))))
   "#C(1.0 2.0)")
  t)

(deftest print.complex.random.1
  (loop for numbits = (random 40)
	for bound = (ash 1 numbits)
	for r = (- (random (+ bound bound)) bound)
	for i = (- (random (+ bound bound)) bound)
	repeat 1000
	unless (= i 0)
	nconc (randomly-check-readability (complex r i)))
  nil)

(deftest print.complex.random.2
  (loop for numbits = (random 40)
	for bound = (ash 1 numbits)
	for num1 = (- (random (+ bound bound)) bound)
	for num2 = (- (random (+ bound bound)) bound)
	for denom1 = (1+ (random bound))
	for denom2 = (1+ (random bound))
	for r = (/ num1 denom1)
	for i = (/ num2 denom2)
	repeat 1000
	unless (= i 0)
	nconc (randomly-check-readability (complex r i)))
  nil)

;; General floating point complex printing tests will go here
		





