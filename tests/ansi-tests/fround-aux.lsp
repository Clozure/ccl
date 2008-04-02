;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Aug 21 16:08:55 2003
;;;; Contains: Aux. functions for testing FROUND

(in-package :cl-test)

(defun fround.1-fn ()
  (loop for n = (- (random 200000)
		   100000)
	for d = (1+ (random 10000))
	for vals = (multiple-value-list (fround n d))
	for (q r) = vals
	for n2 = (+ (* q d) r)
	repeat 100
	unless (and (eql (length vals) 2)
		    (floatp q)
		    (= n n2)
		    (integerp r)
		    (<= (- (/ d 2)) r (/ d 2))
		    (or (/= (abs r) (/ d 2))
			(evenp (floor q))))
	collect (list n d q r n2)))


