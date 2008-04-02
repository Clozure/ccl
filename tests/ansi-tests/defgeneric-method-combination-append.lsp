;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination APPEND

(in-package :cl-test)

(declaim (special *x*))

(compile-and-load "defgeneric-method-combination-aux.lsp")

(deftest defgeneric-method-combination.append.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.append.1 (x)
		  (:method-combination append)
		  (:method append ((x integer)) (car (push '(d) *x*)))
		  (:method append ((x rational)) (car (push '(c) *x*)))
		  (:method append ((x number)) (car (push '(b) *x*)))
		  (:method append ((x t)) (car (push '(a) *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  ((d c b a) ((a) (b) (c) (d)))
  ((c b a) ((a) (b) (c)))
  ((b a) ((a) (b)))
  ((a) ((a))))

(deftest defgeneric-method-combination.append.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.append.2 (x)
		  (:method-combination append :most-specific-first)
		  (:method append ((x integer)) (car (push '(d) *x*)))
		  (:method append ((x rational)) (car (push '(c) *x*)))
		  (:method append ((x number)) (car (push '(b) *x*)))
		  (:method append ((x t)) (car (push '(a) *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  ((d c b a) ((a) (b) (c) (d)))
  ((c b a) ((a) (b) (c)))
  ((b a) ((a) (b)))
  ((a) ((a))))

(deftest defgeneric-method-combination.append.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.append.3 (x)
		  (:method-combination append :most-specific-last)
		  (:method append ((x integer)) (car (push '(d) *x*)))
		  (:method append ((x rational)) (car (push '(c) *x*)))
		  (:method append ((x number)) (car (push '(b) *x*)))
		  (:method append ((x t)) (car (push '(a) *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  ((a b c d) ((d) (c) (b) (a)))
  ((a b c) ((c) (b) (a)))
  ((a b) ((b) (a)))
  ((a) ((a))))

(deftest defgeneric-method-combination.append.4
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.append.4 (x)
		  (:method-combination append)
		  (:method append ((x integer)) '(a b))
		  (:method :around ((x rational)) 'foo)
		  (:method append ((x number)) '(c d))
		  (:method append ((x symbol)) '(e f))
		  (:method append ((x t)) '(g h))))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo (c d g h) (e f g h) (g h))

(deftest defgeneric-method-combination.append.5
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.append.5 (x)
		  (:method-combination append)
		  (:method append ((x integer)) '(a))
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method append ((x number)) '(b))
		  (:method append ((x symbol)) '(c))
		  (:method append ((x t)) 'd)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (a b . d)) (foo (b . d)) (b . d) (c . d) d)

(deftest defgeneric-method-combination.append.6
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.append.6 (x)
		  (:method-combination append)
		  (:method append ((x integer)) '(a))
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method append ((x number)) '(b))
		  (:method append ((x symbol)) '(c))
		  (:method append ((x t)) '(d))))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar (a b d))) (foo (bar (b d))) (bar (b d)) (b d) (c d) (d))

(deftest defgeneric-method-combination.append.7
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.append.7 (x)
		  (:method-combination append)
		  (:method append ((x dgmc-class-04)) '(a))
		  (:method append ((x dgmc-class-03)) '(b))
		  (:method append ((x dgmc-class-02)) '(c))
		  (:method append ((x dgmc-class-01)) '(d))))))
    (declare (type generic-function fn))
    (values
     (funcall fn (make-instance 'dgmc-class-01))
     (funcall fn (make-instance 'dgmc-class-02))
     (funcall fn (make-instance 'dgmc-class-03))
     (funcall fn (make-instance 'dgmc-class-04))))
  (d)
  (c d)
  (b d)
  (a c b d))

(deftest defgeneric-method-combination.append.8
  (let ((fn
	 (eval '(defgeneric dg-mc.append.8 (x)
		  (:method-combination append)
		  (:method append ((x (eql 1000))) '(a))
		  (:method :around ((x symbol)) (values))
		  (:method :around ((x integer)) (values 'a 'b 'c))
		  (:method :around ((x complex)) (call-next-method))
		  (:method :around ((x number)) (values 1 2 3 4 5 6))
		  (:method append ((x t)) '(b))))))
    (declare (type generic-function fn))
    (values
     (multiple-value-list (funcall fn 'a))
     (multiple-value-list (funcall fn 10))
     (multiple-value-list (funcall fn #c(9 8)))
     (multiple-value-list (funcall fn '(a b c)))))
  () (a b c) (1 2 3 4 5 6) ((b)))

(deftest defgeneric-method-combination.append.9
  (handler-case
   (let ((fn (eval '(defgeneric dg-mc.append.9 (x)
		      (:method-combination append)))))
     (declare (type generic-function fn))
     (funcall fn '(a)))
   (error () :error))
  :error)

(deftest defgeneric-method-combination.append.10
  (progn
    (eval '(defgeneric dg-mc.append.10 (x)
	      (:method-combination append)
	      (:method ((x t)) '(a))))
    (handler-case
     (dg-mc.append.10 'x)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.append.11
  (progn
    (eval '(defgeneric dg-mc.append.11 (x)
	     (:method-combination append)
	     (:method nonsense ((x t)) '(a))))
    (handler-case
     (dg-mc.append.11 0)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.append.12
  (let ((fn (eval '(defgeneric dg-mc.append.12 (x)
		     (:method-combination append)
		     (:method :around ((x t)) '(a))
		     (:method append ((x integer)) x)))))
    (declare (type generic-function fn))
    (handler-case (funcall fn '(b))
		  (error () :error)))
  :error)

(deftest defgeneric-method-combination.append.13
  (progn
    (eval '(defgeneric dg-mc.append.13 (x)
	     (:method-combination append)
	     (:method append ((x dgmc-class-01)) (list 'foo))
	     (:method append ((x dgmc-class-02)) (list 'bar))
	     (:method nonsense ((x dgmc-class-03)) (list 'bad))))
    (values
     (dg-mc.append.13 (make-instance 'dgmc-class-01))
     (dg-mc.append.13 (make-instance 'dgmc-class-02))
     (handler-case
      (dg-mc.append.13 (make-instance 'dgmc-class-03))
      (error () :caught))
     (handler-case
      (dg-mc.append.13 (make-instance 'dgmc-class-04))
      (error () :caught))
          (handler-case
      (dg-mc.append.13 (make-instance 'dgmc-class-07))
      (error () :caught))))
  (foo)
  (bar foo)
  :caught
  :caught
  :caught)
