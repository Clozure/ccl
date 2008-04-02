;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Mar  1 22:03:58 2004
;;;; Contains: Tests for printing ratios

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest print.ratios.random
  (loop for i from 1 to 1000
	for numbits = (1+ (random 40))
	for bound = (ash 1 numbits)
	for num = (- (random (+ bound bound)) bound)
	for denom = (1+ (random bound))
	for r = (/ num denom)
	nconc (randomly-check-readability r))
  nil)

