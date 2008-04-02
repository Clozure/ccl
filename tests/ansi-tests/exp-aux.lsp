;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  1 21:30:38 2003
;;;; Contains: Aux. functions for testing EXP, EXPT

(in-package :cl-test)

(defun my-exp (x n)
  "Compute e^x in the appropriate float result type, summing
   the first n terms of the Taylor series."
  (assert (realp x))
  (let ((result 1)
	(xrat (rational x)))
    (loop
     for i from (1- n) downto 1
     do (setq result (+ 1 (/ (* xrat result) i))))
    (if (floatp x)
      (float result x)
      (float result 1.0f0))))


    


