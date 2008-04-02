;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr  7 07:17:42 2003
;;;; Contains: Tests of =, /=, <, <=, >, >=

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Errors tests on comparison functions

(deftest =.error.1
  (signals-error (=) program-error)
  t)

(deftest /=.error.1
  (signals-error (/=) program-error)
  t)

(deftest <.error.1
  (signals-error (<) program-error)
  t)

(deftest <=.error.1
  (signals-error (<=) program-error)
  t)

(deftest >.error.1
  (signals-error (>) program-error)
  t)

(deftest >=.error.1
  (signals-error (>=) program-error)
  t)

;;; Tests of =

(deftest =.1
  (loop for x in *numbers*
	unless (= x)
	collect x)
  nil)

(deftest =.2
  (loop for x in *numbers*
	unless (= x x)
	collect x)
  nil)

(deftest =.3
  (loop for x in *numbers*
	unless (= x x x)
	collect x)
  nil)

(deftest =.4
  (=.4-fn)
  nil)

(deftest =.5
  (loop for i from 1 to 10000
	for i2 = (1+ i)
	never (or (= i i2) (= i2 i)))
  t)

(deftest =.6
  (loop for i from 5 to 10000 by 17
	for j from 2 to i by 19
	for r = (/ i j)
	unless (and (not (= r (1+ r)))
		    (not (= r 0))
		    (not (= r (- r)))
		    (= r r))
	collect r)
  nil)
			 
(deftest =.7
  (let ((args nil))
    (loop for i from 1 to (min 256 (1- call-arguments-limit))
	  do (push 17 args)
	  always (apply #'= args)))
  t)

(deftest =.8
  (loop for i from 2 to (min 256 (1- call-arguments-limit))
	for args = (append (make-list (1- i) :initial-element 7)
			   (list 23))
	when (apply #'= args)
	collect args)
  nil)


(deftest =.9
  (=t 0 0.0)
  t)

(deftest =.10
  (=t 0 #c(0 0))
  t)

(deftest =.11
  (=t 1 #c(1.0 0.0))
  t)

(deftest =.12
  (=t -0.0 0.0)
  t)

(deftest =.13
  (let ((nums '(0 0.0s0 0.0f0 0.0d0 0.0l0
		  #c(0.0s0 0.0s0) #c(0.0f0 0.0f0)
		  #c(0.0d0 0.0d0) #c(0.0l0 0.0l0))))
    (loop for x in nums
	  append
	  (loop for y in nums
		unless (= x y)
		collect (list x y))))
  nil)

(deftest =.14
  (let ((nums '(17 17.0s0 17.0f0 17.0d0 17.0l0
		   #c(17.0s0 0.0s0) #c(17.0f0 0.0f0)
		   #c(17.0d0 0.0d0) #c(17.0l0 0.0l0))))
    (loop for x in nums
	  append
	  (loop for y in nums
		unless (= x y)
		collect (list x y))))
  nil)

(deftest =.15
  (let ((nums '(-17 -17.0s0 -17.0f0 -17.0d0 -17.0l0
		    #c(-17.0s0 0.0s0) #c(-17.0f0 0.0f0)
		    #c(-17.0d0 0.0d0) #c(-17.0l0 0.0l0))))
    (loop for x in nums
	  append
	  (loop for y in nums
		unless (= x y)
		collect (list x y))))
  nil)

(deftest =.16
  (let ((n 60000) (m 30000))
    (loop for x = (- (random n) m)
	  for y = (- (random n) m)
	  for z = (- (random n) m)
	  for w = (- (random n) m)
	  for a = (* x y)
	  for b = (* x w)
	  for c = (* y z)
	  for d = (* w z)
	  repeat 10000
	  when (and (/= b 0)
		    (/= d 0)
		    (or (not (= (/ a b) (/ c d)))
			(/= (/ a b) (/ c d))))
	  collect (list a b c d)))
  nil)

;;; Comparison of a rational with a float

(deftest =.17
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-epsilon single-float-epsilon
			 double-float-epsilon long-float-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat+rat/i = (+ xrat rat/i)
		nconc
		(if (= x xrat+rat/i)
		    (list (list x i  xrat+rat/i))
		  nil))))
  nil)

(deftest =.18
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-negative-epsilon single-float-negative-epsilon
			 double-float-negative-epsilon long-float-negative-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat-rat/i = (- xrat rat/i)
		nconc
		(if (= x xrat-rat/i)
		    (list (list x i xrat-rat/i))
		  nil))))
  nil)

(deftest =.19
  (let ((bound (expt 10 1000)))
    (loop for x in (list most-positive-short-float most-positive-single-float
			 most-positive-double-float most-positive-long-float)
	  for d = (and (<= x bound) (truncate x))
	  when (and d (or (= (* 3/2 d) x)
			  (= x (* 5/4 d))))
	  collect (list x d (* 3/2 d) (* 5/4 d))))
  nil)

(deftest =.order.1
  (let ((i 0) x y)
    (values
     (= (progn (setf x (incf i)) 1)
	(progn (setf y (incf i)) 2))
     i x y))
  nil 2 1 2)

(deftest =.order.2
  (let ((i 0) x y z)
    (values
     (= (progn (setf x (incf i)) 1)
	(progn (setf y (incf i)) 2)
	(progn (setf z (incf i)) 3))
     i x y z))
  nil 3 1 2 3)

(deftest =.order.3
  (let ((i 0) u v w x y z)
    (values
     (=
      (progn (setf u (incf i)) 1)
      (progn (setf v (incf i)) 2)
      (progn (setf w (incf i)) 3)
      (progn (setf x (incf i)) 4)
      (progn (setf y (incf i)) 5)
      (progn (setf z (incf i)) 6))
     i u v w x y z))
  nil 6 1 2 3 4 5 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest /=.1
  (loop for x in *numbers*
	unless (/= x)
	collect x)
  nil)

(deftest /=.2
  (loop for x in *numbers*
	when (/= x x)
	collect x)
  nil)

(deftest /=.3
  (loop for x in *numbers*
	when (/= x x x)
	collect x)
  nil)

(deftest /=.4
  (/=.4-fn)
  nil)

(deftest /=.4a
  (/=.4a-fn)
  nil)

(deftest /=.5
  (loop for i from 1 to 10000
	for i2 = (1+ i)
	always (and (/= i i2) (/= i2 i)))
  t)

(deftest /=.6
  (loop for i from 5 to 10000 by 17
	for j from 2 to i by 19
	for r = (/ i j)
	when (or (not (/= r (1+ r)))
		 (not (/= r 0))
		 (not (/= r (- r)))
		 (/= r r))
	collect r)
  nil)
			 
(deftest /=.7
  (let ((args (list 17))
	(args2 nil))
    (loop for i from 2 to (min 256 (1- call-arguments-limit))
	  do (push 17 args)
	  do (push i args2)
	  always (and (not (apply #'/= args))
		      (apply #'/= args2))))
  t)

(deftest /=.8
  (loop for i from 2 to (min 256 (1- call-arguments-limit))
	for args = (append (make-list (1- i) :initial-element 7)
			   (list 7))
	when (apply #'/= args)
	collect args)
  nil)


(deftest /=.9
  (/= 0 0.0)
  nil)

(deftest /=.10
  (/= 0 #c(0 0))
  nil)

(deftest /=.11
  (/= 1 #c(1.0 0.0))
  nil)

(deftest /=.12
  (/= -0.0 0.0)
  nil)

(deftest /=.13
  (let ((nums '(0 0.0s0 0.0f0 0.0d0 0.0l0
		  #c(0.0s0 0.0s0) #c(0.0f0 0.0f0)
		  #c(0.0d0 0.0d0) #c(0.0l0 0.0l0))))
    (loop for x in nums
	  append
	  (loop for y in nums
		when (/= x y)
		collect (list x y))))
  nil)

(deftest /=.14
  (let ((nums '(17 17.0s0 17.0f0 17.0d0 17.0l0
		   #c(17.0s0 0.0s0) #c(17.0f0 0.0f0)
		   #c(17.0d0 0.0d0) #c(17.0l0 0.0l0))))
    (loop for x in nums
	  append
	  (loop for y in nums
		when (/= x y)
		collect (list x y))))
  nil)

(deftest /=.15
  (let ((nums '(-17 -17.0s0 -17.0f0 -17.0d0 -17.0l0
		    #c(-17.0s0 0.0s0) #c(-17.0f0 0.0f0)
		    #c(-17.0d0 0.0d0) #c(-17.0l0 0.0l0))))
    (loop for x in nums
	  append
	  (loop for y in nums
		when (/= x y)
		collect (list x y))))
  nil)

(deftest /=.17
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-epsilon single-float-epsilon
			 double-float-epsilon long-float-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat+rat/i = (+ xrat rat/i)
		nconc
		(if (/= x xrat+rat/i)
		    nil
		    (list (list x i  xrat+rat/i))))))
  nil)

(deftest /=.18
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-negative-epsilon single-float-negative-epsilon
			 double-float-negative-epsilon long-float-negative-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat-rat/i = (- xrat rat/i)
		nconc
		(if (/= x xrat-rat/i)
		    nil
		    (list (list x i xrat-rat/i))))))
  nil)

(deftest /=.19
  (let ((bound (expt 10 1000)))
    (loop for x in (list most-positive-short-float most-positive-single-float
			 most-positive-double-float most-positive-long-float)
	  for d = (and (<= x bound) (truncate x))
	  unless (or (null d) (and (/= (* 3/2 d) x)
				   (/= x (* 5/4 d))))
	  collect (list x d (* 3/2 d) (* 5/4 d))))
  nil)

(deftest /=.order.1
  (let ((i 0) x y)
    (values
     (notnot (/= (progn (setf x (incf i)) 1)
		 (progn (setf y (incf i)) 2)))
     i x y))
  t 2 1 2)

(deftest /=.order.2
  (let ((i 0) x y z)
    (values
     (notnot (/= (progn (setf x (incf i)) 1)
		 (progn (setf y (incf i)) 2)
		 (progn (setf z (incf i)) 3)))
     i x y z))
  t 3 1 2 3)

(deftest /=.order.3
  (let ((i 0) u v w x y z)
    (values
     (notnot
      (/=
       (progn (setf u (incf i)) 1)
       (progn (setf v (incf i)) 2)
       (progn (setf w (incf i)) 3)
       (progn (setf x (incf i)) 4)
       (progn (setf y (incf i)) 5)
       (progn (setf z (incf i)) 6)))
     i u v w x y z))
  t 6 1 2 3 4 5 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest <.1
  (let ((a 0) (b 1)) (notnot-mv (< a b)))
  t)

(deftest <.2
  (let ((a 0) (b 0)) (notnot-mv (< a b)))
  nil)

(deftest <.3
  (let ((a 1) (b 0)) (notnot-mv (< a b)))
  nil)

(defparameter *number-less-tests*
  (let* ((n (- most-positive-fixnum most-negative-fixnum))
	 (n2 (* 1000 n)))
    (nconc
     (loop for i = (+ (random n) most-negative-fixnum)
	   for i2 = (+ i (random most-positive-fixnum))
	   repeat 1000
	   nconc
	   (list (list i i2 t) (list i2 i nil)))
     (loop for i = (random n2)
	   for i2 = (+ (random n2) i)
	   repeat 1000
	   nconc
	   (list (list i i2 t) (list i2 i nil)))
     (loop for x in *universe*
	   when (integerp x)
	   nconc (list (list x (1+ x) t)
		       (list (1+ x) x nil)))
     (loop for x in *universe*
	   when (realp x)
	   collect (list x x nil))

     (loop for x in *universe*
	   when (and (realp x) (>= x 1))
	   nconc
	   (loop for epsilon in (list short-float-epsilon
				      single-float-epsilon
				      double-float-epsilon
				      long-float-epsilon)
		 for bound in (list most-positive-short-float
				    most-positive-single-float
				    most-positive-double-float
				    most-positive-long-float)
		 for lower-bound in (list most-negative-short-float
				    most-negative-single-float
				    most-negative-double-float
				    most-negative-long-float)
		 for one in '(1.0s0 1.0f0 1.0d0 1.0l0)
		 when (and (<= (abs (float-exponent lower-bound)) 500)
			   (<= (abs (float-exponent x)) 500)
			   (<= (abs (float-exponent bound)) 500))
		 when (<= (rational lower-bound)
			  (rational x)
			  (rational bound))
		 nconc
		 (let* ((y (float x one))
			(z (* y (- one (* 2 epsilon)))))
		   (list (list y z nil)
			 (list z y t)))))
     
     (loop for x in *universe*
	   when (and (realp x) (<= x -1))
	   nconc
	   (loop for epsilon in (list short-float-epsilon
				      single-float-epsilon
				      double-float-epsilon
				      long-float-epsilon)
		 for bound in (list most-negative-short-float
				    most-negative-single-float
				    most-negative-double-float
				    most-negative-long-float)
		 for upper-bound in (list most-positive-short-float
				    most-positive-single-float
				    most-positive-double-float
				    most-positive-long-float)
		 for one in '(1.0s0 1.0f0 1.0d0 1.0l0)
		 when (and (<= (abs (float-exponent bound)) 500)
			   (<= (abs (float-exponent x)) 500)
			   (<= (abs (float-exponent upper-bound)) 500))
		 when (<= (rational bound)
			  (rational x)
			  (rational upper-bound))
		 nconc
		 (let* ((y (float x one)))
		   (let ((z (* y (- one (* 2 epsilon)))))
		     (list (list y z t)
			   (list z y nil))))))
     
     (loop for x in *universe*
	   when (and (realp x) (< -1 x 1))
	   nconc
	   (loop for epsilon in (list short-float-epsilon
				      single-float-epsilon
				      double-float-epsilon
				      long-float-epsilon)
		 for lower-bound in (list most-negative-short-float
				    most-negative-single-float
				    most-negative-double-float
				    most-negative-long-float)
		 for upper-bound in (list most-positive-short-float
				    most-positive-single-float
				    most-positive-double-float
				    most-positive-long-float)
		 for one in '(1.0s0 1.0f0 1.0d0 1.0l0)
		 when (and (<= (abs (float-exponent lower-bound)) 500)
			   (<= (abs (float-exponent x)) 500)
			   (<= (abs (float-exponent upper-bound)) 500))
		 when (<= (rational lower-bound)
			  (rational x)
			  (rational upper-bound))
		 nconc
		 (handler-case
		  (let* ((y (float x one))
			 (z1 (+ y epsilon))
			 (z2 (- y epsilon)))
		    (list (list y z1 t)
			  (list z1 y nil)
			  (list y z2 nil)
			  (list z2 y t)))
		  (arithmetic-error () nil)))
	   ))))

(deftest <.4
  (loop for (x y result . rest) in *number-less-tests*
	unless (if (< x y) result (not result))
	collect (list* x y result rest))
  nil)

(deftest <.5
  (loop for x in *universe*
	when (and (typep x 'real)
		  (not (< x)))
	collect x)
  nil)

(deftest <.6
  (let ((args (list 17))
	(args2 nil))
    (loop for i from 2 to (min 256 (1- call-arguments-limit))
	  do (push 17 args)
	  do (push (- i) args2)
	  unless (and (not (apply #'< args))
		      (apply #'< args2))
	  collect (list args args2)))
  nil)

(deftest <.7
  (let* ((len (min 256 (1- call-arguments-limit)))
	 (args-proto (loop for i from 1 to len collect i)))
    (loop for i from 1 below len
	  for args = (copy-list args-proto)
	  do (setf (elt args i) 0)
	  never (apply #'< args)))
  t)

;;; Check that < is antisymmetric
(deftest <.8
  (<.8-fn)
  nil)

;;;  < is symmetric with >
(deftest <.9
  (<.9-fn)
  nil)

;;;  < is negation of >=
(deftest <.10
  (<.10-fn)
  nil)

(deftest <.11
  (loop for x in '(0.0s0 0.0f0 0.0d0 0.0l0)
	never (or (< (- x) x)
		  (< x (- x))))
  t)

(deftest <.17
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-epsilon single-float-epsilon
			 double-float-epsilon long-float-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat+rat/i = (+ xrat rat/i)
		nconc
		(if (< x xrat+rat/i)
		    nil
		    (list (list x i  xrat+rat/i))))))
  nil)

(deftest <.18
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-negative-epsilon single-float-negative-epsilon
			 double-float-negative-epsilon long-float-negative-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat-rat/i = (- xrat rat/i)
		nconc
		(if (< x xrat-rat/i)
		    (list (list x i xrat-rat/i))
		  nil))))
  nil)

(deftest <.19
  (let ((bound (expt 10 1000)))
    (loop for x in (list most-positive-short-float most-positive-single-float
			 most-positive-double-float most-positive-long-float)
	  for d = (and (<= x bound) (truncate x))
	  unless (or (null d) (and (< x (* 3/2 d))
				   (not (< (* 17/16 d) x))))
	  collect (list x d (* 3/2 d) (* 17/16 d))))
  nil)

(deftest <.order.1
  (let ((i 0) x y)
    (values
     (notnot (< (progn (setf x (incf i)) 1)
		 (progn (setf y (incf i)) 2)))
     i x y))
  t 2 1 2)

(deftest <.order.2
  (let ((i 0) x y z)
    (values
     (notnot (< (progn (setf x (incf i)) 1)
		 (progn (setf y (incf i)) 2)
		 (progn (setf z (incf i)) 3)))
     i x y z))
  t 3 1 2 3)

(deftest <.order.3
  (let ((i 0) u v w x y z)
    (values
     (notnot
      (<
       (progn (setf u (incf i)) 1)
       (progn (setf v (incf i)) 2)
       (progn (setf w (incf i)) 3)
       (progn (setf x (incf i)) 4)
       (progn (setf y (incf i)) 5)
       (progn (setf z (incf i)) 6)))
     i u v w x y z))
  t 6 1 2 3 4 5 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest <=.1
  (let ((a 0) (b 1)) (notnot-mv (<= a b)))
  t)

(deftest <=.2
  (let ((a 0) (b 0)) (notnot-mv (<= a b)))
  t)

(deftest <=.3
  (let ((a 1) (b 0)) (notnot-mv (<= a b)))
  nil)

(defparameter *number-less-or-equal-tests*
  (let* ((n (- most-positive-fixnum most-negative-fixnum))
	 (n2 (* 1000 n)))
    (nconc
     (loop for i = (+ (random n) most-negative-fixnum)
	   for i2 = (+ i (random most-positive-fixnum))
	   repeat 1000
	   nconc
	   (list (list i i2 t) (list i2 i nil)))
     (loop for i = (random n2)
	   for i2 = (+ (random n2) i)
	   repeat 1000
	   nconc
	   (list (list i i2 t) (list i2 i nil)))
     (loop for x in *universe*
	   when (integerp x)
	   nconc (list (list x (1+ x) t)
		       (list (1+ x) x nil)))
     (loop for x in *universe*
	   when (realp x)
	   collect (list x x t))

     (loop for x in *universe*
	   when (and (realp x) (>= x 1))
	   nconc
	   (loop for epsilon in (list short-float-epsilon
				      single-float-epsilon
				      double-float-epsilon
				      long-float-epsilon)
		 for bound in (list most-positive-short-float
				    most-positive-single-float
				    most-positive-double-float
				    most-positive-long-float)
		 for lower-bound in (list most-negative-short-float
				    most-negative-single-float
				    most-negative-double-float
				    most-negative-long-float)
		 for one in '(1.0s0 1.0f0 1.0d0 1.0l0)
		 when (and (<= (abs (float-exponent lower-bound)) 500)
			   (<= (abs (float-exponent x)) 500)
			   (<= (abs (float-exponent bound)) 500))
		 when (<= (rational lower-bound)
			  (rational x)
			  (rational bound))
		 nconc
		 (let* ((y (float x one))
			(z (* y (- one (* 2 epsilon)))))
		   (list (list y z nil)
			 (list z y t)))))
     (loop for x in *universe*
	   when (and (realp x) (<= x -1))
	   nconc
	   (loop for epsilon in (list short-float-epsilon
				      single-float-epsilon
				      double-float-epsilon
				      long-float-epsilon)
		 for bound in (list most-negative-short-float
				    most-negative-single-float
				    most-negative-double-float
				    most-negative-long-float)
		 for upper-bound in (list most-positive-short-float
				    most-positive-single-float
				    most-positive-double-float
				    most-positive-long-float)
		 for one in '(1.0s0 1.0f0 1.0d0 1.0l0)
		 when (and (<= (abs (float-exponent bound)) 500)
			   (<= (abs (float-exponent x)) 500)
			   (<= (abs (float-exponent upper-bound)) 500))
		 when (<= (rational bound)
			  (rational x)
			  (rational upper-bound))
		 nconc
		 (let* ((y (float x one))
			(z (* y (- one (* 2 epsilon)))))
		   (list (list y z t)
			 (list z y nil)))))
     (loop for x in *universe*
	   when (and (realp x) (< -1 x 1))
	   nconc
	   (loop for epsilon in (list short-float-epsilon
				      single-float-epsilon
				      double-float-epsilon
				      long-float-epsilon)
		 for lower-bound in (list most-negative-short-float
				    most-negative-single-float
				    most-negative-double-float
				    most-negative-long-float)
		 for upper-bound in (list most-positive-short-float
				    most-positive-single-float
				    most-positive-double-float
				    most-positive-long-float)
		 for one in '(1.0s0 1.0f0 1.0d0 1.0l0)
		 when (and (<= (abs (float-exponent lower-bound)) 500)
			   (<= (abs (float-exponent x)) 500)
			   (<= (abs (float-exponent upper-bound)) 500))
		 when (<= (rational lower-bound)
			  (rational x)
			  (rational upper-bound))
		 nconc
		 (handler-case
		  (let* ((y (float x one))
			 (z1 (+ y epsilon))
			 (z2 (- y epsilon)))
		    (list (list y z1 t)
			  (list z1 y nil)
			  (list y z2 nil)
			  (list z2 y t)))
		  (floating-point-underflow () nil))))
     )))

(deftest <=.4
  (loop for (x y result . rest) in *number-less-or-equal-tests*
	unless (if (<= x y) result (not result))
	collect (list* x y result rest))
  nil)

(deftest <=.5
  (loop for x in *universe*
	when (and (typep x 'real)
		  (not (<= x)))
	collect x)
  nil)

(deftest <=.6
  (let ((args (list 17))
	(args2 nil)
	(args3 (list 0)))
    (loop for i from 2 to (min 256 (1- call-arguments-limit))
	  do (push 17 args)
	  do (push (- i) args2)
	  do (push i args3)
	  unless (and (apply #'<= args)
		      (apply #'<= args2)
		      (not (apply #'<= args3)))
	  collect (list args args2 args3)))
  nil)

(deftest <=.7
  (let* ((len (min 256 (1- call-arguments-limit)))
	 (args-proto (loop for i from 1 to len collect i)))
    (loop for i from 1 below len
	  for args = (copy-list args-proto)
	  do (setf (elt args i) 0)
	  never (apply #'<= args)))
  t)

;;; Check that <= is symmetric with >=
(deftest <=.8
  (<=.8-fn)
  nil)

;;; Check that <= is equivalent to (or < =)
(deftest <=.9
  (<=.9-fn)
  nil)

(deftest <=.10
  (loop for x in '(0.0s0 0.0f0 0.0d0 0.0l0)
	always (and (<= (- x) x)
		    (<= x (- x))))
  t)

(deftest <=.17
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-epsilon single-float-epsilon
			 double-float-epsilon long-float-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat+rat/i = (+ xrat rat/i)
		nconc
		(if (<= x xrat+rat/i)
		    nil
		    (list (list x i  xrat+rat/i))))))
  nil)

(deftest <=.18
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-negative-epsilon single-float-negative-epsilon
			 double-float-negative-epsilon long-float-negative-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat-rat/i = (- xrat rat/i)
		nconc
		(if (<= x xrat-rat/i)
		    (list (list x i xrat-rat/i))
		  nil))))
  nil)

(deftest <=.19
  (let ((bound (expt 10 1000)))
    (loop for x in (list most-positive-short-float most-positive-single-float
			 most-positive-double-float most-positive-long-float)
	  for d = (and (<= x bound) (truncate x))
	  unless (or (null d) (and (<= x (* 3/2 d))
				   (not (<= (* 5/4 d) x))))
	  collect (list x d (* 3/2 d) (* 5/4 d))))
  nil)

(deftest <=.order.1
  (let ((i 0) x y)
    (values
     (notnot (<= (progn (setf x (incf i)) 1)
		 (progn (setf y (incf i)) 2)))
     i x y))
  t 2 1 2)

(deftest <=.order.2
  (let ((i 0) x y z)
    (values
     (notnot (<= (progn (setf x (incf i)) 1)
		 (progn (setf y (incf i)) 2)
		 (progn (setf z (incf i)) 3)))
     i x y z))
  t 3 1 2 3)

(deftest <=.order.3
  (let ((i 0) u v w x y z)
    (values
     (notnot
      (<=
       (progn (setf u (incf i)) 1)
       (progn (setf v (incf i)) 2)
       (progn (setf w (incf i)) 3)
       (progn (setf x (incf i)) 4)
       (progn (setf y (incf i)) 5)
       (progn (setf z (incf i)) 6)))
     i u v w x y z))
  t 6 1 2 3 4 5 6)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest >.1
  (let ((a 0) (b 1)) (notnot-mv (> a b)))
  nil)

(deftest >.2
  (let ((a 0) (b 0)) (notnot-mv (> a b)))
  nil)

(deftest >.3
  (let ((a 1) (b 0)) (notnot-mv (> a b)))
  t)

(deftest >.4
  (loop for (x y result . rest) in *number-less-tests*
	unless (if (> y x) result (not result))
	collect (list* y x result rest))
  nil)

(deftest >.5
  (loop for x in *universe*
	when (and (typep x 'real)
		  (not (> x)))
	collect x)
  nil)

(deftest >.6
  (let ((args (list 17))
	(args2 nil))
    (loop for i from 2 to (min 256 (1- call-arguments-limit))
	  do (push 17 args)
	  do (push i args2)
	  unless (and (not (apply #'> args))
		      (apply #'> args2))
	  collect (list args args2)))
  nil)

(deftest >.7
  (let* ((len (min 256 (1- call-arguments-limit)))
	 (args-proto (loop for i from 1 to len collect i)))
    (loop for i from 1 below len
	  for args = (copy-list args-proto)
	  do (setf (elt args i) 0)
	  never (apply #'> args)))
  t)

;;; > is negation of <=
(deftest >.8
  (>.8-fn)
  nil)

(deftest >.9
  (loop for x in '(0.0s0 0.0f0 0.0d0 0.0l0)
	never (or (> (- x) x)
		  (> x (- x))))
  t)

(deftest >.17
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-epsilon single-float-epsilon
			 double-float-epsilon long-float-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat+rat/i = (+ xrat rat/i)
		nconc
		(if (> x xrat+rat/i)
		    (list (list x i  xrat+rat/i))
		  nil))))
  nil)

(deftest >.18
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-negative-epsilon single-float-negative-epsilon
			 double-float-negative-epsilon long-float-negative-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat-rat/i = (- xrat rat/i)
		nconc
		(if (> x xrat-rat/i)
		    nil
		    (list (list x i  xrat-rat/i))))))
  nil)

(deftest >.19
  (let ((bound (expt 10 1000)))
    (loop for x in (list most-positive-short-float most-positive-single-float
			 most-positive-double-float most-positive-long-float)
	  for d = (and (<= x bound) (truncate x))
	  unless (or (null d) (and (> (* 3/2 d) x)
				   (not (> x (* 17/16 d)))))
	  collect (list x d (* 3/2 d) (* 17/16 d))))
  nil)

(deftest >.order.1
  (let ((i 0) x y)
    (values
     (notnot (> (progn (setf x (incf i)) 2)
		(progn (setf y (incf i)) 1)))
     i x y))
  t 2 1 2)

(deftest >.order.2
  (let ((i 0) x y z)
    (values
     (notnot (> (progn (setf x (incf i)) 3)
		 (progn (setf y (incf i)) 2)
		 (progn (setf z (incf i)) 1)))
     i x y z))
  t 3 1 2 3)

(deftest >.order.3
  (let ((i 0) u v w x y z)
    (values
     (notnot
      (>
       (progn (setf u (incf i)) 6)
       (progn (setf v (incf i)) 5)
       (progn (setf w (incf i)) 4)
       (progn (setf x (incf i)) 3)
       (progn (setf y (incf i)) 2)
       (progn (setf z (incf i)) 1)))
     i u v w x y z))
  t 6 1 2 3 4 5 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest >=.1
  (let ((a 0) (b 1)) (notnot-mv (>= a b)))
  nil)

(deftest >=.2
  (let ((a 0) (b 0)) (notnot-mv (>= a b)))
  t)

(deftest >=.3
  (let ((a 1) (b 0)) (notnot-mv (>= a b)))
  t)

(deftest >=.4
  (loop for (x y result . rest) in *number-less-or-equal-tests*
	unless (if (>= y x) result (not result))
	collect (list* y x result rest))
  nil)

(deftest >=.5
  (loop for x in *universe*
	when (and (typep x 'real)
		  (not (>= x)))
	collect x)
  nil)

(deftest >=.6
  (let ((args (list 17))
	(args2 (list 0))
	(args3 nil))
    (loop for i from 2 to (min 256 (1- call-arguments-limit))
	  do (push 17 args)
	  do (push (- i) args2)
	  do (push i args3)
	  unless (and (apply #'>= args)
		      (not (apply #'>= args2))
		      (apply #'>= args3))
	  collect (list args args2 args3)))
  nil)

(deftest >=.7
  (let* ((len (min 256 (1- call-arguments-limit)))
	 (args-proto (loop for i from 1 to len collect i)))
    (loop for i from 1 below len
	  for args = (copy-list args-proto)
	  do (setf (elt args i) 0)
	  never (apply #'>= args)))
  t)

;;; Check that >= is equivalent to (or > =)
(deftest >=.8
  (>=.8-fn)
  nil)

(deftest >=.9
  (loop for x in '(0.0s0 0.0f0 0.0d0 0.0l0)
	always (and (>= (- x) x)
		    (>= x (- x))))
  t)


(deftest >=.17
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-epsilon single-float-epsilon
			 double-float-epsilon long-float-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat+rat/i = (+ xrat rat/i)
		nconc
		(if (>= x xrat+rat/i)
		    (list (list x i  xrat+rat/i))
		  nil))))
  nil)

(deftest >=.18
  (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for eps in (list short-float-negative-epsilon single-float-negative-epsilon
			 double-float-negative-epsilon long-float-negative-epsilon)
	for exp = (nth-value 1 (decode-float eps))
	for radix = (float-radix eps)
	when (< (* (log radix 2) exp) 1000)
	nconc
	(let* ((rat (rational eps))
	       (xrat (rational x)))
	  (loop for i from 2 to 100
		for rat/i = (/ rat i)
		for xrat-rat/i = (- xrat rat/i)
		nconc
		(if (>= x xrat-rat/i)
		    nil
		    (list (list x i xrat-rat/i))))))
  nil)

(deftest >=.19
  (let ((bound (expt 10 1000)))
    (loop for x in (list most-positive-short-float most-positive-single-float
			 most-positive-double-float most-positive-long-float)
	  for d = (and (<= x bound) (truncate x))
	  unless (or (null d) (and (>= (* 3/2 d) x)
				   (not (>=  x(* 17/16 d)))))
	  collect (list x d (* 3/2 d) (* 17/16 d))))
  nil)

(deftest >=.order.1
  (let ((i 0) x y)
    (values
     (notnot (>= (progn (setf x (incf i)) 2)
		(progn (setf y (incf i)) 1)))
     i x y))
  t 2 1 2)

(deftest >=.order.2
  (let ((i 0) x y z)
    (values
     (notnot (>= (progn (setf x (incf i)) 3)
		 (progn (setf y (incf i)) 2)
		 (progn (setf z (incf i)) 1)))
     i x y z))
  t 3 1 2 3)

(deftest >=.order.3
  (let ((i 0) u v w x y z)
    (values
     (notnot
      (>=
       (progn (setf u (incf i)) 6)
       (progn (setf v (incf i)) 5)
       (progn (setf w (incf i)) 4)
       (progn (setf x (incf i)) 3)
       (progn (setf y (incf i)) 2)
       (progn (setf z (incf i)) 1)))
     i u v w x y z))
  t 6 1 2 3 4 5 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Comparison of rationals

(deftest compare-rationals.1
  (compare-random-rationals 60000 30000 10000)
  nil)

(deftest compare-rationals.2
  (compare-random-rationals 600000 300000 10000)
  nil)

(deftest compare-rationals.3
  (compare-random-rationals 6000000 3000000 10000)
  nil)

(deftest compare-rationals.4
  (compare-random-rationals 6000000000 3000000000 10000)
  nil)

;;;; Comparison of bignums with floats

(deftest bignum.float.compare.1a
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (+ r (ceiling (rational x)))))
		(unless (< x i)
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.1b
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (- (floor (rational x)) r)))
		(unless (< i x)
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.2a
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (+ r (ceiling (rational x)))))
		(unless (> i x)
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.2b
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (- (floor (rational x)) r)))
		(unless (> x i)
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.3a
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (+ r (ceiling (rational x)))))
		(when (or (= x i) (= i x))
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.3b
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (- (floor (rational x)) r)))
		(when (or (= x i) (= i x))
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.4a
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (+ r (ceiling (rational x)))))
		(unless (and (/= i x) (/= x i))
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.4b
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (- (floor (rational x)) r)))
		(unless (and (/= i x) (/= x i))
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.5a
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (+ r (ceiling (rational x)))))
		(unless (<= x i)
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.5b
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (- (floor (rational x)) r)))
		(unless (<= i x)
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.6a
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (+ r (ceiling (rational x)))))
		(unless (>= i x)
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.6b
  (loop for x in *floats*
	when (or (zerop x)
		 (< (abs (log (abs x))) 10000))
	nconc
	(loop for r = (1+ (random (ash 1 (random 32))))
	      repeat 200
	      nconc
	      (let ((i (- (floor (rational x)) r)))
		(unless (>= x i)
		  (list (list r x i))))))
  nil)

(deftest bignum.float.compare.7
  (let ((toobig (loop for x in *reals*
		      collect (and (> (abs x) 1.0)
				   (> (abs (log (abs x))) 10000)))))
    (loop for x in *reals*
	  for xtoobig in toobig
	  nconc
	  (unless xtoobig
	    (let ((fx (floor x)))
	      (loop for y in *reals*
		    for ytoobig in toobig
		    when (and (not ytoobig)
			      (< x y)
			      (or (not (< fx y))
				  (<= y fx)
				  (not (> y fx))
				  (>= fx y)))
		    collect (list x y))))))
  nil)

(deftest bignum.float.compare.8
  (let ((toobig (loop for x in *reals*
		      collect (and (> (abs x) 1.0)
				   (> (abs (log (abs x))) 10000)))))
    (loop for x in *reals*
	  for xtoobig in toobig
	  nconc
	  (unless xtoobig
	    (let ((fx (floor x)))
	      (loop for y in *reals*
		    for ytoobig in toobig
		    when (and (not ytoobig)
			      (<= x y)
			      (or (not (<= fx y))
				  (> fx y)
				  (not (>= y fx))
				  (< y fx)))
		    collect (list x y))))))
  nil)

;;; More randomized comparisons

(deftest bignum.short-float.random.compare.1
  (let* ((integer-bound (ash 1 1000))
	 (upper-bound (if (< (/ most-positive-short-float 2) integer-bound)
			  (/ most-positive-short-float 2)
			(coerce integer-bound 'short-float))))
    (loop for bound = 1.0s0 then (* bound 2)
	  while (<= bound upper-bound)
	  nconc
	  (loop for r = (random bound)
		for fr = (floor r)
		for cr = (ceiling r)
		repeat 20
		unless (and (<= fr r cr)
			    (if (= r fr)
				(= r cr)
			      (/= r cr))
			    (>= cr r fr))
		collect (list r fr cr))))
  nil)

(deftest bignum.single-float.random.compare.1
  (let* ((integer-bound (ash 1 100))
	 (upper-bound (if (< (/ most-positive-single-float 2) integer-bound)
			  (/ most-positive-single-float 2)
			(coerce integer-bound 'single-float))))
    (loop for bound = 1.0f0 then (* bound 2)
	  while (<= bound upper-bound)
	  nconc
	  (loop for r = (random bound)
		for fr = (floor r)
		for cr = (ceiling r)
		repeat 20
		unless (and (<= fr r cr)
			    (if (= r fr)
				(= r cr)
			      (/= r cr))
			    (>= cr r fr))
		collect (list r fr cr))))
  nil)

(deftest bignum.double-float.random.compare.1
  (let* ((integer-bound (ash 1 100))
	 (upper-bound (if (< (/ most-positive-double-float 2) integer-bound)
			  (/ most-positive-double-float 2)
			(coerce integer-bound 'double-float))))
    (loop for bound = 1.0d0 then (* bound 2)
	  while (<= bound upper-bound)
	  nconc
	  (loop for r = (random bound)
		for fr = (floor r)
		for cr = (ceiling r)
		repeat 20
		unless (and (<= fr r cr)
			    (if (= r fr)
				(= r cr)
			      (/= r cr))
			    (>= cr r fr))
		collect (list r fr cr))))
  nil)

(deftest bignum.long-float.random.compare.1
  (let* ((integer-bound (ash 1 100))
	 (upper-bound (if (< (/ most-positive-long-float 2) integer-bound)
			  (/ most-positive-long-float 2)
			(coerce integer-bound 'long-float))))
    (loop for bound = 1.0l0 then (* bound 2)
	  while (< bound upper-bound)
	  nconc
	  (loop for r = (random bound)
		for fr = (floor r)
		for cr = (ceiling r)
		repeat 20
		unless (and (<= fr r cr)
			    (if (= r fr)
				(= r cr)
			      (/= r cr))
			    (>= cr r fr))
		collect
		(list r fr cr))))
  nil)

;;; Rational/float comparisons

(deftest rational.short-float.random.compare.1
  (let* ((integer-bound (ash 1 1000))
	 (upper-bound (if (< (/ most-positive-short-float 2) integer-bound)
			  (/ most-positive-short-float 2)
			(coerce integer-bound 'short-float))))
    (loop for bound = 1.0s0 then (* bound 2)
	  while (<= bound upper-bound)
	  nconc
	  (loop for r = (+ 1.s0 (random bound))
		for fr = (floor r)
		for cr = (ceiling r)
		for m = (ash 1 (1+ (random 30)))
		for p = (1+ (random m))
		for q = (1+ (random m))
		for x = 0
		repeat 50
		when (<= p q) do (psetf p (1+ q) q p)
		do (setf x (/ p q))
		unless (let ((fr/x (/ fr x))
			     (cr*x (* cr x)))
			 (and (<= fr/x r cr*x)
			      (< fr/x r cr*x)
			      (> cr*x r fr/x)
			      (>= cr*x r fr/x)))
		collect (list r p q x fr cr))))
  nil)

(deftest rational.single-float.random.compare.1
  (let* ((integer-bound (ash 1 1000))
	 (upper-bound (if (< (/ most-positive-single-float 2) integer-bound)
			  (/ most-positive-single-float 2)
			(coerce integer-bound 'single-float))))
    (loop for bound = 1.0f0 then (* bound 2)
	  while (<= bound upper-bound)
	  nconc
	  (loop for r = (+ 1.s0 (random bound))
		for fr = (floor r)
		for cr = (ceiling r)
		for m = (ash 1 (1+ (random 30)))
		for p = (1+ (random m))
		for q = (1+ (random m))
		for x = 0
		repeat 50
		when (<= p q) do (psetf p (1+ q) q p)
		do (setf x (/ p q))
		unless (let ((fr/x (/ fr x))
			     (cr*x (* cr x)))
			 (and (<= fr/x r cr*x)
			      (< fr/x r cr*x)
			      (> cr*x r fr/x)
			      (>= cr*x r fr/x)))
		collect (list r p q x fr cr))))
  nil)

(deftest rational.double-float.random.compare.1
  (let* ((integer-bound (ash 1 1000))
	 (upper-bound (if (< (/ most-positive-double-float 4) integer-bound)
			  (/ most-positive-double-float 4)
			(coerce integer-bound 'double-float))))
    (loop for bound = 1.0d0 then (* bound 4)
	  while (<= bound upper-bound)
	  nconc
	  (loop for r = (+ 1.s0 (random bound))
		for fr = (floor r)
		for cr = (ceiling r)
		for m = (ash 1 (1+ (random 30)))
		for p = (1+ (random m))
		for q = (1+ (random m))
		for x = 0
		repeat 50
		when (<= p q) do (psetf p (1+ q) q p)
		do (setf x (/ p q))
		unless (let ((fr/x (/ fr x))
			     (cr*x (* cr x)))
			 (and (<= fr/x r cr*x)
			      (< fr/x r cr*x)
			      (> cr*x r fr/x)
			      (>= cr*x r fr/x)))
		collect (list r p q x fr cr))))
  nil)

(deftest rational.long-float.random.compare.1
  (let* ((integer-bound (ash 1 1000))
	 (upper-bound (if (< (/ most-positive-long-float 4) integer-bound)
			  (/ most-positive-long-float 4)
			(coerce integer-bound 'long-float))))
    (loop for bound = 1.0d0 then (* bound 4)
	  while (<= bound upper-bound)
	  nconc
	  (loop for r = (+ 1.s0 (random bound))
		for fr = (floor r)
		for cr = (ceiling r)
		for m = (ash 1 (1+ (random 30)))
		for p = (1+ (random m))
		for q = (1+ (random m))
		for x = 0
		repeat 50
		when (<= p q) do (psetf p (1+ q) q p)
		do (setf x (/ p q))
		unless (let ((fr/x (/ fr x))
			     (cr*x (* cr x)))
			 (and (<= fr/x r cr*x)
			      (< fr/x r cr*x)
			      (> cr*x r fr/x)
			      (>= cr*x r fr/x)))
		collect (list r p q x fr cr))))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest =.env.1
  (macrolet ((%m (z) z))
	    (mapcar 'notnot
		    (list (= (expand-in-current-env (%m 0)))
			  (= 1 (expand-in-current-env (%m 1)))
			  (= (expand-in-current-env (%m 2)) 2)
			  (= (expand-in-current-env (%m 3))
			     (expand-in-current-env (%m 3)))
			  (= (expand-in-current-env (%m #c(1 2)))
			     (expand-in-current-env (%m #c(1 2))))

			  (= 1 (expand-in-current-env (%m 2.0)))
			  (= (expand-in-current-env (%m 2)) 2/3)
			  (= (expand-in-current-env (%m 4))
			     (expand-in-current-env (%m 5)))

			  (= (expand-in-current-env (%m 0)) 0 0)
			  (= 0 (expand-in-current-env (%m 0)) 0)
			  (= 0 0 (expand-in-current-env (%m 0)))
			  )))
  (t t t t t nil nil nil t t t))


(deftest /=.env.1
  (macrolet ((%m (z) z))
	    (mapcar 'notnot
		    (list (/= (expand-in-current-env (%m 0)))
			  (/= 1 (expand-in-current-env (%m 1)))
			  (/= (expand-in-current-env (%m 2)) 2)
			  (/= (expand-in-current-env (%m 3))
			      (expand-in-current-env (%m 3)))
			  (/= (expand-in-current-env (%m #c(1 2)))
			      (expand-in-current-env (%m #c(1 2))))

			  (/= 1 (expand-in-current-env (%m 2.0)))
			  (/= (expand-in-current-env (%m 2)) 2/3)
			  (/= (expand-in-current-env (%m 4))
			      (expand-in-current-env (%m 5)))

			  (/= (expand-in-current-env (%m 2)) 0 1)
			  (/= 0 (expand-in-current-env (%m 2)) 1)
			  (/= 0 1 (expand-in-current-env (%m 2)))
			  )))
  (t nil nil nil nil t t t t t t))

(deftest <.env.1
  (macrolet ((%m (z) z))
	    (mapcar 'notnot
		    (list (< (expand-in-current-env (%m 0)))
			  (< 0 (expand-in-current-env (%m 1)))
			  (< (expand-in-current-env (%m 2)) 3)
			  (< (expand-in-current-env (%m 5))
			     (expand-in-current-env (%m 7)))

			  (< 3 (expand-in-current-env (%m 2.0)))
			  (< (expand-in-current-env (%m 2)) 2/3)
			  (< (expand-in-current-env (%m 6))
			     (expand-in-current-env (%m 5)))

			  (< (expand-in-current-env (%m 1)) 2 3)
			  (< 1 (expand-in-current-env (%m 2)) 3)
			  (< 1 2 (expand-in-current-env (%m 3)))
			  )))
  (t t t t nil nil nil t t t))

(deftest <=.env.1
  (macrolet ((%m (z) z))
	    (mapcar 'notnot
		    (list (<= (expand-in-current-env (%m 0)))
			  (<= 0 (expand-in-current-env (%m 1)))
			  (<= (expand-in-current-env (%m 2)) 3)
			  (<= (expand-in-current-env (%m 5))
			     (expand-in-current-env (%m 7)))

			  (<= 3 (expand-in-current-env (%m 2.0)))
			  (<= (expand-in-current-env (%m 2)) 2/3)
			  (<= (expand-in-current-env (%m 6))
			     (expand-in-current-env (%m 5)))

			  (<= (expand-in-current-env (%m 2)) 2 3)
			  (<= 1 (expand-in-current-env (%m 1)) 3)
			  (<= 1 2 (expand-in-current-env (%m 2)))
			  )))
  (t t t t nil nil nil t t t))

(deftest >.env.1
  (macrolet ((%m (z) z))
	    (mapcar 'notnot
		    (list (> (expand-in-current-env (%m 0)))
			  (> 2 (expand-in-current-env (%m 1)))
			  (> (expand-in-current-env (%m 4)) 3)
			  (> (expand-in-current-env (%m 10))
			     (expand-in-current-env (%m 7)))

			  (> 1 (expand-in-current-env (%m 2.0)))
			  (> (expand-in-current-env (%m -1)) 2/3)
			  (> (expand-in-current-env (%m 4))
			     (expand-in-current-env (%m 5)))

			  (> (expand-in-current-env (%m 2)) 1 0)
			  (> 2 (expand-in-current-env (%m 1)) 0)
			  (> 2 1 (expand-in-current-env (%m 0)))
			  )))
  (t t t t nil nil nil t t t))


(deftest >=.env.1
  (macrolet ((%m (z) z))
	    (mapcar 'notnot
		    (list (>= (expand-in-current-env (%m 0)))
			  (>= 2 (expand-in-current-env (%m 1)))
			  (>= (expand-in-current-env (%m 4)) 3)
			  (>= (expand-in-current-env (%m 7))
			      (expand-in-current-env (%m 7)))

			  (>= 1 (expand-in-current-env (%m 2.0)))
			  (>= (expand-in-current-env (%m -1)) 2/3)
			  (>= (expand-in-current-env (%m 4))
			     (expand-in-current-env (%m 5)))

			  (>= (expand-in-current-env (%m 2)) 1 1)
			  (>= 1 (expand-in-current-env (%m 1)) 0)
			  (>= 2 2 (expand-in-current-env (%m 0)))
			  )))
  (t t t t nil nil nil t t t))
