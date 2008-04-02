;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Aug 28 10:41:34 2003
;;;; Contains: Tests of the multiplication function *

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "times-aux.lsp")

(deftest *.1
  (*)
  1)

(deftest *.2
  (loop for x in *numbers*
	unless (eql x (* x))
	collect x)
  nil)

(deftest *.3
  (loop for x in *numbers*
	for x1 = (* x 1)
	for x2 = (* 1 x)
	unless (and (eql x x1) (eql x x2) (eql x1 x2))
	collect (list x x1 x2))
  nil)

(deftest *.4
  (loop for x in *numbers*
	for x1 = (* x 0)
	for x2 = (* 0 x)
	unless (and (= x1 0) (= x2 0))
	collect (list x x1 x2))
  nil)

(deftest *.5
  (loop for bound in '(1.0s0 1.0f0 1.0d0 1.0l0)
	nconc
	(loop for x = (random bound)
	      for x1 = (* x -1)
	      for x2 = (* -1 x)
	      for x3 = (* x bound)
	      for x4 = (* bound x)
	      repeat 1000
	      unless (and (eql (- x) x1) (eql (- x) x2)
			  (eql x x3) (eql x x4))
	      collect (list x x1 x2 x3 x4)))
  nil)

(deftest *.6
  (let* ((upper-bound (* 1000 1000 1000 1000))
	 (lower-bound (- upper-bound))
	 (spread (1+ (- upper-bound lower-bound))))
    (loop for x = (random-from-interval upper-bound)
	  for y = (random-from-interval upper-bound)
	  for prod = (* x y)
	  for prod2 = (integer-times x y)
	  repeat 1000
	  unless (eql prod prod2)
	  collect (list x y prod prod2)))
  nil)

(deftest *.7
  (let* ((upper-bound (* 1000 1000 1000))
	 (lower-bound (- upper-bound))
	 (spread (1+ (- upper-bound lower-bound))))
    (loop for x = (+ (rational (random (float spread 1.0f0))) lower-bound)
	  for y = (+ (rational (random (float spread 1.0f0))) lower-bound)
	  for prod = (* x y)
	  for prod2 = (rat-times x y)
	  repeat 1000
	  unless (eql prod prod2)
	  collect (list x y prod prod2)))
  nil)

;; Testing of multiplication by integer constants
(deftest *.8
  (let ((bound (isqrt most-positive-fixnum)))
    (loop
     for x = (random bound)
     for y = (random bound)
     for f = (eval `(function (lambda (z)
				(declare (optimize (speed 3) (safety 0)))
				(declare (type (integer 0 (,bound)) z))
				(* ,x z))))
     for prod = (funcall f y)
     repeat 100
     unless (and (eql prod (* x y))
		 (eql prod (integer-times x y)))
     collect (progn (format t "Failed on ~A~%" (list x y prod))
		    (list x y prod (* x y) (integer-times x y)))))
  nil)

(deftest *.9
  (let* ((upper-bound (* 1000 1000 1000 1000)))
    (flet ((%r () (random-from-interval upper-bound)))
      (loop for xr = (%r)
	    for xc = (%r)
	    for x = (complex xr xc)
	    for yr = (%r)
	    for yc = (%r)
	    for y = (complex yr yc)
	    for prod = (* x y)
	    repeat 1000
	    unless (and (eql (realpart prod) (- (integer-times xr yr)
						(integer-times xc yc)))
			(eql (imagpart prod) (+ (integer-times xr yc)
						(integer-times xc yr))))
	    collect (list x y prod))))
  nil)

(deftest *.10
  (let* ((upper-bound (* 1000 1000 1000 1000))
	 (lower-bound (- upper-bound))
	 (spread (1+ (- upper-bound lower-bound))))
    (flet ((%r () (+ (rational (random (float spread 1.0f0))) lower-bound)))
      (loop for xr = (%r)
	    for xc = (%r)
	    for x = (complex xr xc)
	    for yr = (%r)
	    for yc = (%r)
	    for y = (complex yr yc)
	    for prod = (* x y)
	    repeat 1000
	    unless (and (eql (realpart prod) (- (rat-times xr yr)
						(rat-times xc yc)))
			(eql (imagpart prod) (+ (rat-times xr yc)
						(rat-times xc yr))))
	  collect (list x y prod))))
  nil)

(deftest *.11
  (let ((prod 1) (args nil))
    (loop for i from 1 to (min 256 (1- call-arguments-limit))
	  do (push i args)
	  do (setq prod (* prod i))
	  always (eql (apply #'* args) prod)))
  t)

(deftest *.12
  (loop
   for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
   for radix = (float-radix x)
   for (k eps-r eps-f) = (multiple-value-list (find-epsilon x))
   nconc
   (loop for i from 1 to k
	 for y = (+ x (expt radix (- i)))
	 nconc
	 (loop for j from 1 to (- k i)
	       for z = (+ x (expt radix (- j)))
	       unless (eql (* y z)
			   (+ x
			      (expt radix (- i))
			      (expt radix (- j))
			      (expt radix (- (+ i j)))))
	       collect (list x i j))))
  nil)

(deftest *.13
  (loop
   for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
   for radix = (float-radix x)
   for (k eps-r eps-f) = (multiple-value-list (find-epsilon x))
   nconc
   (loop for i from 1 to k
	 for y = (- x (expt radix (- i)))
	 nconc
	 (loop for j from 1 to (- k i)
	       for z = (- x (expt radix (- j)))
	       unless (eql (* y z)
			   (+ x
			      (- (expt radix (- i)))
			      (- (expt radix (- j)))
			      (expt radix (- (+ i j)))))
	       collect (list x i j))))
  nil)

;;; Float contagion

(deftest *.14
  (let ((bound (- (sqrt most-positive-short-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'short-float))
	  collect (list x y p)))
  nil)	

(deftest *.15
  (let ((bound (- (sqrt most-positive-single-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'single-float))
	  collect (list x y p)))
  nil)	

(deftest *.16
  (let ((bound (- (sqrt most-positive-double-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'double-float))
	  collect (list x y p)))
  nil)

(deftest *.17
  (let ((bound (- (sqrt most-positive-long-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'long-float))
	  collect (list x y p)))
  nil)

(deftest *.18
  (let ((bound (- (sqrt most-positive-short-float) 1))
	(bound2 (- (sqrt most-positive-single-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound2)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'single-float))
	  collect (list x y p)))
  nil)

(deftest *.19
  (let ((bound (- (sqrt most-positive-short-float) 1))
	(bound2 (- (sqrt most-positive-double-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound2)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'double-float))
	  collect (list x y p)))
  nil)

(deftest *.20
  (let ((bound (- (sqrt most-positive-short-float) 1))
	(bound2 (- (sqrt most-positive-long-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound2)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'long-float))
	  collect (list x y p)))
  nil)

(deftest *.21
  (let ((bound (- (sqrt most-positive-single-float) 1))
	(bound2 (- (sqrt most-positive-double-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound2)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'double-float))
	  collect (list x y p)))
  nil)

(deftest *.22
  (let ((bound (- (sqrt most-positive-single-float) 1))
	(bound2 (- (sqrt most-positive-long-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound2)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'long-float))
	  collect (list x y p)))
  nil)

(deftest *.23
  (let ((bound (- (sqrt most-positive-double-float) 1))
	(bound2 (- (sqrt most-positive-long-float) 1)))
    (loop for x = (random-from-interval bound)
	  for y = (random-from-interval bound2)
	  for p = (* x y)
	  repeat 1000
	  unless (and (eql p (* y x))
		      (typep p 'long-float))
	  collect (list x y p)))
  nil)

(deftest *.24
  (loop
   for type in '(short-float single-float double-float long-float)
   for bits in '(13 24 50 50)
   for bound = (ash 1 (floor bits 2))
   nconc
   (loop for i = (random bound)
	 for x = (coerce i type)
	 for j = (random bound)
	 for y = (coerce j type)
	 for prod = (* x y)
	 repeat 1000
	 unless (and (eql prod (coerce (* i j) type))
		     (eql prod (* y x)))
	 collect (list i j x y (* x y) (coerce (* i j) type))))
  nil)

(deftest *.25
  (loop
   for type in '(short-float single-float double-float long-float)
   for bits in '(13 24 50 50)
   for bound = (ash 1 (- bits 2))
   when (= (float-radix (coerce 1.0 type)) 2)
   nconc
   (loop for i = (random bound)
	 for x = (coerce i type)
	 for j = (* i 2)
	 for y = (coerce j type)
	 repeat 1000
	 unless (eql (* 2 x) y)
	 collect (list i j x (* 2 x) y)))
  nil)

;;; Shows a compiler bug in sbcl/cmucl
(deftest *.26
  (eqlt (funcall (compile nil
			  '(lambda (x y)
			     (declare (type (single-float -10.0 10.0) x)
				      (type (double-float -1.0d100 1.0d100) y))
			     (* x y)))
		 1.0f0 1.0d0)
	1.0d0)
  t)

(deftest *.27
  (loop
   for type in '(short-float single-float double-float long-float)
   for bits in '(13 24 50 50)
   for bound = (ash 1 (floor bits 2))
   nconc
   (loop for i = (random bound)
	 for x = (coerce i type)
	 for j = (random bound)
	 for y = (coerce j type)
	 for one = (coerce 1.0 type)
	 for cx = (complex one x)
	 for cy = (complex one y)
	 for prod = (* cx cy)
	 repeat 1000
	 unless (and (eql prod (complex (coerce (- 1 (* i j)) type)
					(coerce (+ i j) type)))
		     (eql prod (* cy cx)))
	 collect (list type i j x y (* cx cy))))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest *.28
  (macrolet ((%m (z) z))
	    (values
	     (* (expand-in-current-env (%m 2)))
	     (* (expand-in-current-env (%m 3)) 4)
	     (* 5 (expand-in-current-env (%m 3)))))
  2 12 15)

;;; Order of evaluation tests

(deftest times.order.1
  (let ((i 0) x y)
    (values
     (* (progn (setf x (incf i)) 2)
	(progn (setf y (incf i)) 3))
     i x y))
  6 2 1 2)

(deftest times.order.2
  (let ((i 0) x y z)
    (values
     (* (progn (setf x (incf i)) 2)
	(progn (setf y (incf i)) 3)
	(progn (setf z (incf i)) 5))
     i x y z))
  30 3 1 2 3)
