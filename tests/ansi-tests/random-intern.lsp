;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Code to randomly intern and unintern random strings
;;;;           in a package.  Exercises package and hash table routines

(in-package :cl-test)

(defconstant +max-len-random-symbol+ 63)

(defun make-random-symbol (package)
  (declare (optimize (speed 3) (safety 3)))
  (loop
   (let* ((len (random (1+ +max-len-random-symbol+)))
	  (str (make-string len)))
     (declare (type (integer 0 #.+max-len-random-symbol+) len))
     (loop
      for i from 0 to (1- len) do
      (setf (schar str i)
	    (schar +base-chars+
		   (random +num-base-chars+))))
     (multiple-value-bind
      (symbol status)
      (intern (copy-seq str) package)
      (unless (equal str (symbol-name symbol))
	      (error "Intern gave bad symbol: ~A, ~A~%" str symbol))
      (unless status (return symbol))))))

(defun queue-insert (q x)
  (declare (type cons q))
  (push x (cdr q)))

(defun queue-remove (q)
  (declare (type cons q))
  (when (null (car q))
	(when (null (cdr q))
	      (error "Attempty to remove from empty queue.~%"))
	(setf (car q) (nreverse (cdr q)))
	(setf (cdr q) nil))
  (pop (car q)))

(defun queue-empty (q)
  (and (null (car q))
       (null (cdr q))))

(defun random-intern (n)
  (declare (fixnum n))
  (let ((q (list nil))
	(xp (defpackage "X" (:use))))
    (declare (type cons q))
    (loop
     for i from 1 to n do
     (if (and
	  (= (random 2) 0)
	  (not (queue-empty q)))
	 (unintern (queue-remove q) xp)
       (queue-insert q (make-random-symbol xp))))))

(defun fill-intern (n)
  (declare (fixnum n))
  (let ((xp (defpackage "X" (:use))))
    (loop
     for i from 1 to n do
     (make-random-symbol xp))))

	 







