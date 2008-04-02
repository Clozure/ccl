;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 10 07:14:30 2004
;;;; Contains: Aux. functions for random tests on classes

(in-package :cl-test)

(defun random-class-1-fn (&key (n 10) (rep 1000))
  "Randomly break and recreate a linear chain of class definitions"
  (assert (typep n '(integer 1)) (n) "N is ~A" n)
  (assert (typep rep 'unsigned-byte) (rep) "REP is ~A" rep)
  (let ((class-names (make-array n
				 :initial-contents
				 (loop for i from 1 to n
				       collect (make-symbol
						(format nil "CLASS-NAME-~D" i))))))
    (unwind-protect
	(let ((parents (make-array n :initial-element nil)))
	  ;; Create classes
	  (loop for name across class-names
		do (eval `(defclass ,name () nil)))
	  (loop for i = (1+ (random (1- n)))
		for name = (elt class-names i)
		for parent = (elt parents i)
		repeat rep
		do (if parent
		       (progn
			 (setf (elt parents i) nil)
			 (eval `(defclass ,name () nil)))
		     (eval `(defclass ,name
			      (,(setf (elt parents i) (elt class-names (1- i))))
			      nil
			      )))))
      (loop for name across class-names
	    do (setf (find-class name) nil)))))

