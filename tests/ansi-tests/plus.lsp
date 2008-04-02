;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 31 04:34:17 2003
;;;; Contains: Tests of the function +

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
;;; (compile-and-load "plus-aux.lsp")

(deftest plus.1
  (+)
  0)

(deftest plus.2
  (loop for x in *numbers*
	unless (eql x (+ x))
	collect x)
  nil)

(deftest plus.3
  (loop for x in *numbers*
	for x1 = (+ x 0)
	for x2 = (+ 0 x)
	unless (and (eql x x1) (eql x x2) (eql x1 x2))
	collect (list x x1 x2))
  nil)

(deftest plus.4
  (loop for x in *numbers*
	for x1 = (- x x)
	unless (= x1 0)
	collect (list x x1))
  nil)

(deftest plus.5
  (let* ((upper-bound most-positive-fixnum)
	 (lower-bound most-negative-fixnum)
	 (spread (- upper-bound lower-bound)))
    (flet ((%r () (+ (random spread) lower-bound)))
      (loop for x = (%r)
	    for y = (%r)
	    for z = (%r)
	    for s1 = (+ x y z)
	    for s2 = (+ z y x)
	    for s3 = (+ y x z)
	    for s4 = (+ x z y)
	    for s5 = (+ z x y)
	    for s6 = (+ y z x)
	    repeat 1000
	    unless (and (eql s1 s2) (eql s1 s3) (eql s1 s4)
			(eql s1 s5) (eql s1 s6))
	    collect (list x y z s1 s2 s3 s4 s5 s6))))
  nil)

(deftest plus.6
  (let* ((upper-bound 1000000000000000)
	 (lower-bound -1000000000000000)
	 (spread (- upper-bound lower-bound)))
    (flet ((%r () (+ (random spread) lower-bound)))
      (loop for x = (%r)
	    for y = (%r)
	    for z = (%r)
	    for s1 = (+ x y z)
	    for s2 = (+ z y x)
	    for s3 = (+ y x z)
	    for s4 = (+ x z y)
	    for s5 = (+ z x y)
	    for s6 = (+ y z x)
	    repeat 1000
	    unless (and (eql s1 s2) (eql s1 s3) (eql s1 s4)
			(eql s1 s5) (eql s1 s6))
	    collect (list x y z s1 s2 s3 s4 s5 s6))))
  nil)

(deftest plus.7
  (let* ((upper-bound most-positive-fixnum)
	 (lower-bound most-negative-fixnum)
	 (spread (- upper-bound lower-bound)))
    (flet ((%r () (+ (random spread) lower-bound)))
      (loop for x = (/ (%r) (max 1 (%r)))
	    for y = (/ (%r) (max 1 (%r)))
	    for z = (/ (%r) (max 1 (%r)))
	    for s1 = (+ x y z)
	    for s2 = (+ z y x)
	    for s3 = (+ y x z)
	    for s4 = (+ x z y)
	    for s5 = (+ z x y)
	    for s6 = (+ y z x)
	    repeat 1000
	    unless (and (eql s1 s2) (eql s1 s3) (eql s1 s4)
			(eql s1 s5) (eql s1 s6))
	    collect (list x y z s1 s2 s3 s4 s5 s6)
	    unless (= (+ x y)
		      (let ((xn (numerator x))
			    (xd (denominator x))
			    (yn (numerator y))
			    (yd (denominator y)))
			(/ (+ (* xn yd) (* xd yn))
			   (* xd yd))))
	    collect (list x y))))
  nil)

(deftest plus.8
  (let (args)
    (loop for i from 0 to (min 256 (1- call-arguments-limit))
	  unless (eql (apply #'+ args) (/ (* i (1+ i)) 2))
	  collect i
	  do (push (1+ i) args)))
  nil)

(deftest plus.9
  (let* ((upper-bound most-positive-fixnum)
	 (lower-bound most-negative-fixnum)
	 (spread (- upper-bound lower-bound)))
    (flet ((%r () (+ (random spread) lower-bound)))
      (loop
       for xr = (%r)
       for xi = (%r)
       for yr = (%r)
       for yi = (%r)
       for x = (complex xr xi)
       for y = (complex yr yi)
       for s = (+ x y)
       repeat 1000
       unless (eql s (complex (+ xr yr) (+ xi yi)))
       collect (list x y s))))
  nil)

(deftest plus.10
  (loop
   for x in '(0.0s0 0.0f0 0.0d0 0.0l0)
   for radix = (float-radix x)
   for (k eps-r eps-f) = (multiple-value-list (find-epsilon x))
   nconc
   (loop for i from 1 to k
	 for e1 = (expt radix (- i))
	 for y = (+ x e1)
	 nconc
	 (loop for j from 1 to (- k i)
	       for e2 = (expt radix (- j))
	       for z = (+ x e2)
	       unless (eql (+ y z) (+ x e1 e2))
	       collect (list x i j))))
  nil)

(deftest plus.11
  (flet ((%r () (- (random most-positive-short-float) (/ most-positive-short-float 2))))
    (loop for x = (%r)
	  for y = (%r)
	  for s = (+ x y)
	  repeat 1000
	  unless (and (eql s (+ y x))
		      (typep s 'short-float))
	  collect (list x y s)))
  nil)

(deftest plus.12
  (flet ((%r () (- (random most-positive-single-float) (/ most-positive-single-float 2))))
    (loop for x = (%r)
	  for y = (%r)
	  for s = (+ x y)
	  repeat 1000
	  unless (and (eql s (+ y x))
		      (typep s 'single-float))
	  collect (list x y s)))
  nil)

(deftest plus.13
  (flet ((%r () (- (random most-positive-double-float) (/ most-positive-double-float 2))))
    (loop for x = (%r)
	  for y = (%r)
	  for s = (+ x y)
	  repeat 1000
	  unless (and (eql s (+ y x))
		      (typep s 'double-float))
	  collect (list x y s)))
  nil)

(deftest plus.14
  (flet ((%r () (- (random most-positive-long-float) (/ most-positive-long-float 2))))
    (loop for x = (%r)
	  for y = (%r)
	  for s = (+ x y)
	  repeat 1000
	  unless (and (eql s (+ y x))
		      (typep s 'long-float))
	  collect (list x y s)))
  nil)

(deftest plus.15
  (let ((bound most-positive-short-float)
	(bound2 most-positive-single-float))
    (loop for x = (- (random bound) (/ bound 2))
	  for y = (- (random bound2)(/ bound2 2))
	  for p = (+ x y)
	  repeat 1000
	  unless (and (eql p (+ y x))
		      (typep p 'single-float))
	  collect (list x y p)))
  nil)

(deftest plus.16
  (let ((bound most-positive-short-float)
	(bound2 most-positive-double-float))
    (loop for x = (- (random bound) (/ bound 2))
	  for y = (- (random bound2)(/ bound2 2))
	  for p = (+ x y)
	  repeat 1000
	  unless (and (eql p (+ y x))
		      (typep p 'double-float))
	  collect (list x y p)))
  nil)

(deftest plus.17
  (let ((bound most-positive-short-float)
	(bound2 most-positive-long-float))
    (loop for x = (- (random bound) (/ bound 2))
	  for y = (- (random bound2)(/ bound2 2))
	  for p = (+ x y)
	  repeat 1000
	  unless (and (eql p (+ y x))
		      (typep p 'long-float))
	  collect (list x y p)))
  nil)

(deftest plus.18
  (let ((bound most-positive-single-float)
	(bound2 most-positive-double-float))
    (loop for x = (- (random bound) (/ bound 2))
	  for y = (- (random bound2)(/ bound2 2))
	  for p = (+ x y)
	  repeat 1000
	  unless (and (eql p (+ y x))
		      (typep p 'double-float))
	  collect (list x y p)))
  nil)

(deftest plus.19
  (let ((bound most-positive-single-float)
	(bound2 most-positive-long-float))
    (loop for x = (- (random bound) (/ bound 2))
	  for y = (- (random bound2)(/ bound2 2))
	  for p = (+ x y)
	  repeat 1000
	  unless (and (eql p (+ y x))
		      (typep p 'long-float))
	  collect (list x y p)))
  nil)

(deftest plus.20
  (let ((bound most-positive-double-float)
	(bound2 most-positive-long-float))
    (loop for x = (- (random bound) (/ bound 2))
	  for y = (- (random bound2)(/ bound2 2))
	  for p = (+ x y)
	  repeat 1000
	  unless (and (eql p (+ y x))
		      (typep p 'long-float))
	  collect (list x y p)))
  nil)

(deftest plus.21
  (loop
   for type in '(short-float single-float double-float long-float)
   for bits in '(13 24 50 50)
   for bound = (ash 1 (1- bits))
   nconc
   (loop for i = (random bound)
	 for x = (coerce i type)
	 for j = (random bound)
	 for y = (coerce j type)
	 for sum = (+ x y)
	 repeat 1000
	 unless (and (eql sum (coerce (+ i j) type))
		     (eql sum (+ y x)))
	 collect (list i j x y sum (coerce (+ i j) type))))
  nil)

(deftest plus.22
  (loop
   for type in '(short-float single-float double-float long-float)
   for bits in '(13 24 50 50)
   for bound = (ash 1 (1- bits))
   nconc
   (loop
    for one = (coerce 1 type)
    for i = (random bound)
    for x = (complex (coerce i type) one)
    for j = (random bound)
    for y = (complex (coerce j type) one)
    for sum = (+ x y)
    repeat 1000
    unless (and (eql sum (complex (coerce (+ i j) type)
				  (coerce 2 type)))
		(eql sum (+ y x)))
    collect (list i j x y sum)))
  nil)

(deftest plus.23
  (loop
   for type in '(short-float single-float double-float long-float)
   for bits in '(13 24 50 50)
   for bound = (ash 1 (1- bits))
   nconc
   (loop
    for one = (coerce 1 type)
    for i = (random bound)
    for x = (complex one (coerce i type))
    for j = (random bound)
    for y = (complex one (coerce j type))
    for sum = (+ x y)
    repeat 1000
    unless (and (eql sum (complex (coerce 2 type)
				  (coerce (+ i j) type)))
		(eql sum (+ y x)))
    collect (list i j x y sum)))
  nil)

;;; Negative zero tests (suggested by R. Toy)

(deftest plus.24
  (funcall
   (compile nil '(lambda (x) (declare (type short-float x) (optimize (speed 3) (safety 0) (debug 0)))
		   (+ 0.0s0 x)))
   -0.0s0)
  0.0s0)

(deftest plus.25
  (funcall
   (compile nil '(lambda (x) (declare (type single-float x) (optimize (speed 3) (safety 0) (debug 0)))
		   (+ 0.0f0 x)))
   -0.0f0)
  0.0f0)

(deftest plus.26
  (funcall
   (compile nil '(lambda (x) (declare (type double-float x) (optimize (speed 3) (safety 0) (debug 0)))
		   (+ 0.0d0 x)))
   -0.0d0)
  0.0d0)

(deftest plus.27
  (funcall
   (compile nil '(lambda (x) (declare (type long-float x) (optimize (speed 3) (safety 0) (debug 0)))
		   (+ 0.0l0 x)))
   -0.0l0)
  0.0l0)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest plus.28
  (macrolet ((%m (z) z))
	    (values
	     (+ (expand-in-current-env (%m 1)))
	     (+ (expand-in-current-env (%m 2)) 3)
	     (+ 4 (expand-in-current-env (%m 5)))
	     (+ 1/2 (expand-in-current-env (%m 6)) 2/3)))
  1 5 9 43/6)

;;; Must test combinations of reals and complex arguments.

;;; Order of evaluation tests

(deftest plus.order.1
  (let ((i 0) x y)
    (values
     (+ (progn (setf x (incf i)) '8)
	(progn (setf y (incf i)) '11))
     i x y))
  19 2 1 2)

(deftest plus.order.2
  (let ((i 0) x y z)
    (values
     (+ (progn (setf x (incf i)) '8)
	(progn (setf y (incf i)) '11)
	(progn (setf z (incf i)) '100))
     i x y z))
  119 3 1 2 3)

;;; Test that compilation does not reassociate float additions

(deftest plus.reassociation.1
  (loop
   for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
   for eps in (list short-float-epsilon single-float-epsilon
		    double-float-epsilon long-float-epsilon)
   for eps2 = (* eps 9/10)
   when (eql
	 (funcall (compile nil `(lambda () (+ ,x (+ ,eps2 ,eps2)))))
	 x)
   collect (list x eps eps2))
  nil)

(deftest plus.reassociation.2
  (loop
   for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
   for eps in (list short-float-epsilon single-float-epsilon
		    double-float-epsilon long-float-epsilon)
   for eps2 = (* eps 9/10)
   unless (equal
	   (funcall (compile nil `(lambda () (list (+ (+ ,x ,eps2) ,eps2)
						   (+ ,eps2 (+ ,eps2 ,x))))))
	   (list x x))
   collect (list x eps eps2))
  nil)

(deftest plus.reassociation.3
  (loop
   for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
   for eps in (list short-float-epsilon single-float-epsilon
		    double-float-epsilon long-float-epsilon)
   for eps2 = (* eps 9/10)
   when (eql
	 (funcall (compile nil `(lambda (y e) (+ y (+ e e)))) x eps2)
	 x)
   collect (list x eps eps2))
  nil)

(deftest plus.reassociation.4
  (loop
   for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
   for eps in (list short-float-epsilon single-float-epsilon
		    double-float-epsilon long-float-epsilon)
   for eps2 = (* eps 9/10)
   unless (equal
	   (funcall (compile nil `(lambda (y e) (list (+ (+ y e) e)
						      (+ e (+ e y)))))
		    x eps2)
	   (list x x))
   collect (list x eps eps2))
  nil)
