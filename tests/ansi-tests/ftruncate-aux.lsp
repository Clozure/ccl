;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 20 06:37:01 2003
;;;; Contains: Aux. functions for testing FTRUNCATE

(in-package :cl-test)

(defun ftruncate.1-fn ()
  (loop for n = (- (random 200000)
		   100000)
	for d = (1+ (random 10000))
	for vals = (multiple-value-list (ftruncate n d))
	for (q r) = vals
	for n2 = (+ (* q d) r)
	repeat 100
	unless (and (eql (length vals) 2)
		    (floatp q)
		    (= n n2)
		    (integerp r)
		    (if (>= n 0)
			(< -1 r d)
		      (< -1 (- r) d)))
	collect (list n d q r n2)))
