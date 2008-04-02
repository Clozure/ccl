;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul 24 19:25:39 2005
;;;; Contains: Aux file for BIT-* tests

(in-package :cl-test)

(defun bit-random-test-fn (bit-fn log-fn &key (reps 5000) (maxlen 100))
  (assert (typep maxlen '(integer 1)))
  (assert (typep reps 'unsigned-byte))
  (loop for len = (random maxlen)
	for twos = (make-list len :initial-element 2)
	for v1 = (map 'bit-vector #'random twos)
	for v2 = (map 'bit-vector #'random twos)
	for result = (funcall bit-fn v1 v2)
	repeat reps
	unless (and (= (length result) len)
		    (every #'(lambda (result-bit v1-bit v2-bit)
			       (= result-bit (logand 1 (funcall log-fn v1-bit v2-bit))))
			   result v1 v2))
	collect (list len v1 v2 result)))

