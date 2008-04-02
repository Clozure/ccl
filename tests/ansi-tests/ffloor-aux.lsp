;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 12 07:02:07 2003
;;;; Contains: Aux. functions used in FFLOOR tests

(in-package :cl-test)

(defun ffloor.1-fn ()
  (loop for n = (- (random 200000)
		   100000)
	for d = (1+ (random 10000))
	for vals = (multiple-value-list (ffloor n d))
	for (q r) = vals
	for n2 = (+ (* q d) r)
	repeat 100
	unless (and (eql (length vals) 2)
		    (floatp q)
		    (= n n2)
		    (integerp r)
		    (< -1 r d))
	collect (list n d q r n2)))
