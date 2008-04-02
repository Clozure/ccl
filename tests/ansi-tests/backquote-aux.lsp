;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jun 11 08:04:23 2004
;;;; Contains: Aux. functions associated with backquote tests

(in-package :cl-test)

;;; Not yet finished


;;; Create random backquoted forms
(defun make-random-backquoted-form (size)
  (my-with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package "CL-TEST")))
     (read-from-string
      (concatenate 'string
		   "`"
		   (make-random-backquoted-sequence-string size))))))

(defun make-random-backquoted-sequence-string (size)
  (case size
    ((0 1) (make-random-backquoted-string size))
    (t
     (let* ((nelements (1+ (min (random (1- size)) (random (1- size)) 9)))
	    (sizes (random-partition (1- size) nelements))
	    (substrings (mapcar #'make-random-backquoted-string sizes)))
       (apply #'concatenate
	      'string
	      "("
	      (car substrings)
	      (if nil ; (and (> nelements 1) (coin))
		  (nconc
		   (loop for s in (cddr substrings) collect " " collect s)
		   (list " . " (cadr substrings) ")"))
		(nconc
		 (loop for s in (cdr substrings) collect " " collect s)
		 (list ")"))))))))

;;; Create a string that is a backquoted form
(defun make-random-backquoted-string (size)
  (if (<= size 1)
      (rcase
       (1 "()")
       (1 (string (random-from-seq #.(coerce *cl-symbol-names* 'vector))))
       (1 (write-to-string (- (random 2001) 1000)))
       (2 (concatenate 'string "," (string (random-from-seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
       )
    ;; size > 1
    (make-random-backquoted-sequence-string size)))
