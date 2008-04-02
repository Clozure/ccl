;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination LIST

(in-package :cl-test)

(declaim (special *x*))

(compile-and-load "defgeneric-method-combination-aux.lsp")

(deftest defgeneric-method-combination.list.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.list.1 (x)
		  (:method-combination list)
		  (:method list ((x integer)) (car (push 'd *x*)))
		  (:method list ((x rational)) (car (push 'c *x*)))
		  (:method list ((x number)) (car (push 'b *x*)))
		  (:method list ((x t)) (car (push 'a *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  ((d c b a) (a b c d))
  ((c b a) (a b c))
  ((b a) (a b))
  ((a) (a)))

(deftest defgeneric-method-combination.list.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.list.2 (x)
		  (:method-combination list :most-specific-first)
		  (:method list ((x integer)) (car (push 'd *x*)))
		  (:method list ((x rational)) (car (push 'c *x*)))
		  (:method list ((x number)) (car (push 'b *x*)))
		  (:method list ((x t)) (car (push 'a *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  ((d c b a) (a b c d))
  ((c b a) (a b c))
  ((b a) (a b))
  ((a) (a)))

(deftest defgeneric-method-combination.list.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.list.3 (x)
		  (:method-combination list :most-specific-last)
		  (:method list ((x integer)) (car (push 'd *x*)))
		  (:method list ((x rational)) (car (push 'c *x*)))
		  (:method list ((x number)) (car (push 'b *x*)))
		  (:method list ((x t)) (car (push 'a *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  ((a b c d) (d c b a))
  ((a b c) (c b a))
  ((a b) (b a))
  ((a) (a)))

(deftest defgeneric-method-combination.list.4
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.list.4 (x)
		  (:method-combination list)
		  (:method list ((x integer)) '(a b))
		  (:method :around ((x rational)) 'foo)
		  (:method list ((x number)) '(c d))
		  (:method list ((x symbol)) '(e f))
		  (:method list ((x t)) '(g h))))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo ((c d) (g h)) ((e f) (g h)) ((g h)))

(deftest defgeneric-method-combination.list.5
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.list.5 (x)
		  (:method-combination list)
		  (:method list ((x integer)) 'a)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method list ((x number)) 'b)
		  (:method list ((x symbol)) 'c)
		  (:method list ((x t)) 'd)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (a b d)) (foo (b d)) (b d) (c d) (d))

(deftest defgeneric-method-combination.list.6
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.list.6 (x)
		  (:method-combination list)
		  (:method list ((x integer)) 'a)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method list ((x number)) 'b)
		  (:method list ((x symbol)) 'c)
		  (:method list ((x t)) 'd)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar (a b d))) (foo (bar (b d))) (bar (b d)) (b d) (c d) (d))

(deftest defgeneric-method-combination.list.7
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.list.7 (x)
		  (:method-combination list)
		  (:method list ((x dgmc-class-04)) 'a)
		  (:method list ((x dgmc-class-03)) 'b)
		  (:method list ((x dgmc-class-02)) 'c)
		  (:method list ((x dgmc-class-01)) 'd)))))
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

(deftest defgeneric-method-combination.list.8
  (let ((fn
	 (eval '(defgeneric dg-mc.list.8 (x)
		  (:method-combination list)
		  (:method list ((x (eql 1000))) 'a)
		  (:method :around ((x symbol)) (values))
		  (:method :around ((x integer)) (values 'a 'b 'c))
		  (:method :around ((x complex)) (call-next-method))
		  (:method :around ((x number)) (values 1 2 3 4 5 6))
		  (:method list ((x t)) 'b)))))
    (declare (type generic-function fn))
    (values
     (multiple-value-list (funcall fn 'a))
     (multiple-value-list (funcall fn 10))
     (multiple-value-list (funcall fn #c(9 8)))
     (multiple-value-list (funcall fn '(a b c)))))
  () (a b c) (1 2 3 4 5 6) ((b)))

(deftest defgeneric-method-combination.list.9
  (handler-case
   (let ((fn (eval '(defgeneric dg-mc.list.9 (x)
		      (:method-combination list)))))
     (declare (type generic-function fn))
     (funcall fn (list 'a)))
   (error () :error))
  :error)

(deftest defgeneric-method-combination.list.10
  (progn
    (eval '(defgeneric dg-mc.list.10 (x)
	     (:method-combination list)
	     (:method ((x t)) (list 'a))))
    (handler-case
     (dg-mc.list.10 'a)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.list.11
  (progn
    (eval '(defgeneric dg-mc.list.11 (x)
	     (:method-combination list)
	     (:method nonsense ((x t)) (list 'a))))
    (handler-case
     (dg-mc.list.11 0)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.list.12
  (let ((fn (eval '(defgeneric dg-mc.list.12 (x)
		     (:method-combination list)
		     (:method :around ((x t)) (list 'a))
		     (:method list ((x integer)) x)))))
    (declare (type generic-function fn))
    (handler-case (funcall fn (list 'b))
		  (error () :error)))
  :error)
