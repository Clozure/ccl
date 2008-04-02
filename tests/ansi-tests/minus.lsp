;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 31 11:15:14 2003
;;;; Contains: Tests of the - function

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest minus.error.1
  (signals-error (-) program-error)
  t)

;;; Unary minus tests
(deftest minus.1
  (loop for x in *numbers*
	unless (eql (- (- x)) x)
	collect x)
  nil)

(deftest minus.2
  (locally
   (declare (notinline -))
   (loop for x in *numbers*
	 unless (eql (- (- x)) x)
	 collect x))
  nil)

(deftest minus.3
  (loop for x in *reals*
	when (and (integerp x)
		  (not (eql (- x) (- 0 x))))
	collect x)
  nil)

(deftest minus.4
  (loop for x in *reals*
	for neg = (- x)
	when (and (floatp x)
		  (not (zerop x))
		  (not (eql neg (- 0.0s0 x)))
		  (eql (float 1.0s0 x)
		       (float 1.0s0 neg)))
	collect x)
  nil)

(deftest minus.5
  (loop for x in *numbers*
	when (and (complexp x)
		  (rationalp (realpart x))
		  (not (eql (- x) (- 0 x))))
	collect x)
  nil)

(deftest minus.6
  (loop for x in *numbers*
	for neg = (- x)
	when (and (complexp x)
		  (floatp (realpart x))
		  (eql (float 1.0s0 (realpart x))
		       (float 1.0s0 (realpart neg)))
		  (or (/= neg (- 0 x))
		      (and (not (zerop (realpart x)))
			   (not (eqlzt neg (- 0 x))))))
	collect x)
  nil)

(deftest minus.7
  (let ((upper-bound most-positive-fixnum)
	(lower-bound most-negative-fixnum))
    (loop
     for x = (+ (random (- upper-bound lower-bound)) lower-bound)
     for neg = (- x)
     repeat 1000
     unless (and (integerp neg)	
		 (eql (abs x) (abs neg))
		 (if (> x 0) (< neg 0) (>= neg 0))
		 (zerop (+ x neg))
		 (eql x (- neg)))
     collect x))
  nil)

(deftest minus.8
  (let ((upper-bound (ash 1 1000))
	(lower-bound (- (ash 1 1000))))
    (loop
     for x = (+ (random (- upper-bound lower-bound)) lower-bound)
     for neg = (- x)
     repeat 1000
     unless (and (integerp neg)
		 (eql (abs x) (abs neg))
		 (if (> x 0) (< neg 0) (>= neg 0))
		 (zerop (+ x neg))
		 (eql x (- neg)))
     collect x))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest minus.9
  (macrolet ((%m (z) z)) (- (expand-in-current-env (%m 1))))
  -1)

;;; Binary minus tests

(deftest subtract.1
  (loop
   for x = (random-fixnum)
   for y = (random-fixnum)
   repeat 1000
   unless (and (eql (+ x (- y)) (- x y))
	       (eql (+ 1 x (- y)) (- x (1- y)))
	       (eql (+ -1 x (- y)) (- x (1+ y))))
   collect (list x y))
  nil)

(deftest subtract.2
  (let ((bound (ash 1 1000)))
    (loop
     for x = (random-from-interval bound (- bound))
     for y = (random-from-interval bound (- bound))
     repeat 1000
     unless  (and (eql (+ x (- y)) (- x y))
	       (eql (+ 1 x (- y)) (- x (1- y)))
	       (eql (+ -1 x (- y)) (- x (1+ y))))
     collect (list x y)))
  nil)

(deftest subtract.3
  (let ((args nil))
    (loop for i from 1 below (min 256 (1- call-arguments-limit))
	  do (push 1 args)
	  always (eql (apply #'- 1000 args) (- 1000 i))))
  t)

;;; Float contagion

(deftest subtract.4
  (loop
   for type1 in '(short-float single-float double-float long-float)
   for bits1 in '(13 24 50 50)
   for bound1 = (ash 1 (- bits1 2))
   for c1 from 1
   nconc
   (loop for type2 in '(short-float single-float double-float long-float)
	 for bits2 in '(13 24 50 50)
	 for bound2 = (ash 1 (- bits2 2))
	 for c2 from 1
	 nconc 
	 (loop
	  for i = (random-from-interval bound1)
	  for x = (coerce i type1)
	  for j = (random-from-interval bound2)
	  for y = (coerce j type2)
	  for idiff1 = (- i j)
	  for idiff2 = (- j i)
	  for diff1 = (- x y)
	  for diff2 = (- y x)
	  repeat 1000
	  unless (or (zerop idiff1)
		     (and (eql idiff1 (- idiff2))
			  (eql diff1 (- diff2))
			  (if (<= c1 c2)
			      (eql (float diff1 y) diff1)
			    (eql (float diff1 x) diff1))
			  (eql (float idiff1 diff1) diff1)))
	  collect (list i x j y idiff1 idiff2 diff1 diff2))))
  nil)

;;; Complex subtraction

(deftest subtract.5
  (loop for i = (random-fixnum)
	for ci = (complex i (+ i 100))
	for j = (random-fixnum)
	for cj = (complex j (- j 200))
	for diff = (- ci cj)
	repeat 1000
	unless (eql diff (complex (- i j) (+ (- i j) 300)))
	collect (list i ci j cj (- ci cj)))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest subtract.6
  (macrolet ((%m (z) z))
	    (values
	     (- (expand-in-current-env (%m 2)) 1)
	     (- 17 (expand-in-current-env (%m 5)))
	     (- 1/2 (expand-in-current-env (%m 1/6))
		(expand-in-current-env (%m 0)))))
  1 12 1/3)
