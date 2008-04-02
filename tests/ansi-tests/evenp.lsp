;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 31 10:39:01 2003
;;;; Contains: Tests of EVENP

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest evenp.error.1
  (signals-error (evenp) program-error)
  t)

(deftest evenp.error.2
  (signals-error (evenp 0 nil) program-error)
  t)

(deftest evenp.error.3
  (check-type-error #'evenp #'integerp)
  nil)

(deftest evenp.1
  (loop for x in *numbers*
	when (integerp x)
	do (evenp x))
  nil)

(deftest evenp.3
  (loop for x = (random-fixnum)  
	repeat 10000
	when (or
	      (not (evenp (+ x x)))
	      (evenp (+ x x 1))
	      (if (evenp x)
		  (or (evenp (1+ x))
		      (evenp (1- x))
		      (/= (mod x 2) 0))
		(or (not (evenp (1+ x)))
		    (not (evenp (1- x)))
		    (= (mod x 2) 0))))
	collect x)
  nil)

(deftest evenp.4
  (let ((upper-bound 1000000000000000)
	(lower-bound -1000000000000000))
    (loop for x = (random-from-interval upper-bound lower-bound)
	  repeat 10000
	  when (or
		(not (evenp (+ x x)))
		(evenp (+ x x 1))
		(if (evenp x)
		    (or (evenp (1+ x))
			(evenp (1- x))
			(/= (mod x 2) 0))
		  (or (not (evenp (1+ x)))
		      (not (evenp (1- x)))
		      (= (mod x 2) 0))))
	  collect x))
  nil)

(deftest evenp.5
  (notnot-mv (evenp 0))
  t)

(deftest evenp.6
  (evenp 1)
  nil)

(deftest evenp.7
  (notnot-mv (evenp 100000000000000000000000000000000))
  t)

(deftest evenp.8
  (evenp 100000000000000000000000000000001)
  nil)
