;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 31 10:48:25 2003
;;;; Contains: Tests of ODDP

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest oddp.error.1
  (signals-error (oddp) program-error)
  t)

(deftest oddp.error.2
  (signals-error (oddp 0 nil) program-error)
  t)

(deftest oddp.error.3
  (check-type-error #'oddp #'integerp)
  nil)

;;; Non-error tests

(deftest oddp.1
  (loop for x in *numbers*
	when (integerp x)
	do (oddp x))
  nil)

(deftest oddp.3
  (loop for x = (random-fixnum)
	repeat 10000
	when (or
	      (oddp (+ x x))
	      (not (oddp (+ x x 1)))
	      (if (oddp x)
		  (or (oddp (1+ x))
		      (oddp (1- x))
		      (/= (mod x 2) 1))
		(or (not (oddp (1+ x)))
		    (not (oddp (1- x)))
		    (/= (mod x 2) 0))))
	collect x)
  nil)

(deftest oddp.4
  (let ((upper-bound 1000000000000000)
	(lower-bound -1000000000000000))
    (loop for x = (random-from-interval upper-bound lower-bound)
	  repeat 10000
	  when (or
		(oddp (+ x x))
		(not (oddp (+ x x 1)))
		(if (oddp x)
		    (or (oddp (1+ x))
			(oddp (1- x))
			(/= (mod x 2) 1))
		  (or (not (oddp (1+ x)))
		      (not (oddp (1- x)))
		      (/= (mod x 2) 0))))
	  collect x))
  nil)

(deftest oddp.5
  (notnot-mv (oddp 1))
  t)

(deftest oddp.6
  (oddp 0)
  nil)

(deftest oddp.7
  (notnot-mv (oddp 100000000000000000000000000000001))
  t)

(deftest oddp.8
  (oddp 100000000000000000000000000000000)
  nil)

			