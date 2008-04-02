;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 31 20:20:15 2003
;;;; Contains: Tests of the / function

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "division-aux.lsp")

(deftest /.error.1
  (signals-error (/) program-error)
  t)

(deftest /.error.2
  (divide-by-zero-test 0))

(deftest /.error.3
  (divide-by-zero-test 1 0))

(deftest /.error.4
  (divide-by-zero-test 17 10 0 11))

(deftest /.error.5
  (divide-by-zero-test 0.0s0))

(deftest /.error.6
  (divide-by-zero-test 0.0f0))

(deftest /.error.7
  (divide-by-zero-test 0.0d0))

(deftest /.error.8
  (divide-by-zero-test 0.0l0))

;;;;;;;;;;

(deftest /.1
  (/ 1)
  1)

(deftest /.2
  (/ -1)
  -1)

(deftest /.3
  (loop for i = (random-fixnum)
	repeat 1000
	unless (or (zerop i)
		   (let ((q1 (/ i))
			 (q2 (/ 1 i)))
		     (and (rationalp q1)
			  (eql (denominator q1) (abs i))
			  (eql (numerator q1) (signum i))
			  (eql q1 q2)
			  (eql (* q1 i) 1))))
	collect i)
  nil)

(deftest /.4
  (loop for i = (random-from-interval 1000000 1)
	for j = (random-from-interval 1000000 1)
	for g = (gcd i j)
	for q = (/ i j)
	for q2 = (/ j)
	repeat 1000
	unless (and (integerp g)
		    (zerop (mod i g))
		    (zerop (mod j g))
		    (eql (numerator q) (/ i g))
		    (eql (denominator q) (/ j g))
		    (eql (/ q) (/ j i))
		    (eql q (* i q2)))			 
	collect (list i j q))
  nil)

(deftest /.5
  (loop for bound in (list 1.0s5 1.0f10 1.0d20 1.0l20)
	nconc
	(loop for i = (1+ (random bound))
	      for r1 = (/ i)
	      for r2 = (/ 1 i)
	      repeat 1000
	      unless (eql r1 r2)
	      collect (list i r1 r2)))
  nil)

;; Complex division
(deftest /.6
  (loop for i1 = (random-fixnum)
	for i = (if (zerop i1) 1 i1)
	for c = (complex 0 i)
	for r = (/ c)
	repeat 1000
	unless (eql r (complex 0 (- (/ i))))
	collect (list i c r))
  nil)

#|
(deftest /.7
  (loop for bound in (list 1.0s5 1.0f10 1.0d20 1.0l20)
	nconc
	(loop for i = (1+ (random bound))
	      for c = (complex 0 i)
	      for r = (/ c)
	      repeat 1000
	      unless (= r (complex 0 (- (/ i))))
	      collect (list i c r (complex 0 (- (/ i))))))
  nil)
|#

(deftest /.8
  (loop for bound in (list 1.0s5 1.0f10 1.0d20 1.0l20)
	for one = (float 1.0 bound)
	for zero = (float 0.0 bound)
	nconc
	(loop for i = (1+ (random bound))
	      for c = (complex i zero)
	      for q = (/ c c)
	      repeat 100
	      unless (eql q (complex one zero))
	      collect (list i c q (complex one zero))))
  nil)


(deftest /.9
  (loop for a = (random-fixnum)
	for b = (random-fixnum)
	for m = (+ (* a a) (* b b))
	repeat 1000
	unless
	(or (zerop m)
	    (let* ((q (/ (complex a b)))
		   (c (/ a m))
		   (d (/ (- b) m))
		   (expected (complex c d)))
	      (eql q expected)))
	collect (list a b (/ (complex a b))))
  nil)

(deftest /.10
  (let ((bound 1000000000000000000))
    (loop for a = (random-from-interval bound)
	  for b = (random-from-interval bound)
	  for m = (+ (* a a) (* b b))
	  repeat 1000
	  unless
	  (or (zerop m)
	      (let* ((q (/ (complex a b)))
		     (c (/ a m))
		     (d (/ (- b) m))
		     (expected (complex c d)))
		(eql q expected)))
	  collect (list a b (/ (complex a b)))))
  nil)

(deftest /.11
  (loop for a = (random-fixnum)
	for b = (random-fixnum)
	for n = (complex (random-fixnum) (random-fixnum))
	for m = (+ (* a a) (* b b))
	repeat 1000
	unless
	(or (zerop m)
	    (let* ((q (/ n (complex a b)))
		   (c (/ a m))
		   (d (/ (- b) m))
		   (expected (* n (complex c d))))
	      (eql q expected)))
	collect (list a b (/ n (complex a b))))
  nil)

;;; More floating point tests

(deftest /.12
  (loop for type in '(short-float single-float double-float long-float)
	for lower in (mapcar
		     #'rational-safely
		     (list
		      least-positive-short-float least-positive-single-float
		      least-positive-double-float least-positive-long-float))
	for upper in (mapcar
		     #'rational-safely
		     (list
		      most-positive-short-float most-positive-single-float
		      most-positive-double-float most-positive-long-float))
	for one = (coerce 1 type)
	for radix = (float-radix one)
	nconc
	(loop
	 for i from 1
	 for rpos = radix then (* rpos radix)
	 for rneg = (/ radix) then (/ rneg radix)
	 while (<= lower rneg rpos upper)
	 unless
	 (let ((frpos (float rpos one))
	       (frneg (float rneg one)))
	   (and (eql (/ frpos) (/ one frpos))
		(eql (/ frpos) (/ 1.0s0 frpos))
		(eql (/ frpos) (/ 1 frpos))
		(eql (/ frpos) frneg)
		(eql (/ frneg) (/ 1.0s0 frneg))
		(eql (/ frneg) (/ 1 frneg))
		(eql (/ frneg) frpos)))
	 collect (list i rpos rneg (float rpos one) (float rneg one))))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest /.13
  (macrolet ((%m (z) z))
	    (values
	     (/ (expand-in-current-env (%m 1/2)))
	     (/ (expand-in-current-env (%m 2)) 3)
	     (/ 5 (expand-in-current-env (%m 7)))))
  2 2/3 5/7)
