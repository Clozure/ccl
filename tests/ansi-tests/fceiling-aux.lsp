;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 20 06:24:45 2003
;;;; Contains: Tests of FCEILING

(in-package :cl-test)

(defun fceiling.1-fn ()
  (loop for n = (- (random 200000)
		   100000)
	for d = (1+ (random 10000))
	for vals = (multiple-value-list (fceiling n d))
	for (q r) = vals
	for n2 = (+ (* q d) r)
	repeat 100
	unless (and (eql (length vals) 2)
		    (floatp q)
		    (= n n2)
		    (integerp r)
		    (< (- d) r 1))
	collect (list n d q r n2)))
