;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination OR

(in-package :cl-test)

(declaim (special *x*))

(compile-and-load "defgeneric-method-combination-aux.lsp")

(deftest defgeneric-method-combination.progn.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.progn.1 (x)
		  (:method-combination progn)
		  (:method progn ((x integer)) (push 4 *x*) nil)
		  (:method progn ((x rational)) (push 3 *x*) nil)
		  (:method progn ((x number)) (push 2 *x*) nil)
		  (:method progn ((x t)) (push 1 *x*) 'a)))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (a (1 2 3 4))
  (a (1 2 3))
  (a (1 2))
  (a (1)))

(deftest defgeneric-method-combination.progn.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.progn.2 (x)
		  (:method-combination progn :most-specific-first)
		  (:method progn ((x integer)) (push 4 *x*) 'a)
		  (:method progn ((x rational)) (push 3 *x*) 'b)
		  (:method progn ((x number)) (push 2 *x*) 'c)
		  (:method progn ((x t)) (push 1 *x*) 'd)))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (d (1 2 3 4))
  (d (1 2 3))
  (d (1 2))
  (d (1)))

(deftest defgeneric-method-combination.progn.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.progn.3 (x)
		  (:method-combination progn :most-specific-last)
		  (:method progn ((x integer)) (push 4 *x*) 'a)
		  (:method progn ((x rational)) (push 3 *x*) 'b)
		  (:method progn ((x number)) (push 2 *x*) 'c)
		  (:method progn ((x t)) (push 1 *x*) 'd)))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (a (4 3 2 1))
  (b (3 2 1))
  (c (2 1))
  (d (1)))

(deftest defgeneric-method-combination.progn.4
  (let ((fn
	 (eval '(defgeneric dg-mc.progn.4 (x)
		  (:method-combination progn)
		  (:method progn ((x integer)) 'd)
		  (:method :around ((x rational)) 'foo)
		  (:method progn ((x number)) 'b)
		  (:method progn ((x symbol)) 'c)
		  (:method progn ((x t)) 'a)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo a a a)

(deftest defgeneric-method-combination.progn.4a
  (let ((fn
	 (eval '(defgeneric dg-mc.progn.4a (x)
		  (:method-combination progn :most-specific-last)
		  (:method progn ((x integer)) 'd)
		  (:method :around ((x rational)) 'foo)
		  (:method progn ((x number)) 'b)
		  (:method progn ((x symbol)) 'c)
		  (:method progn ((x t)) 'a)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo b c a)

(deftest defgeneric-method-combination.progn.5
  (let ((fn
	 (eval '(defgeneric dg-mc.progn.5 (x)
		  (:method-combination progn)
		  (:method progn ((x integer)) 'a)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method progn ((x number)) nil)
		  (:method progn ((x symbol)) 'b)
		  (:method progn ((x t)) 'c)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo c) (foo c) c c c)

(deftest defgeneric-method-combination.progn.5a
  (let ((fn
	 (eval '(defgeneric dg-mc.progn.5a (x)
		  (:method-combination progn :most-specific-last)
		  (:method progn ((x integer)) 'a)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method progn ((x number)) 'e)
		  (:method progn ((x symbol)) 'b)
		  (:method progn ((x t)) 'c)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo a) (foo e) e b c)


(deftest defgeneric-method-combination.progn.6
  (let ((fn
	 (eval '(defgeneric dg-mc.progn.6 (x)
		  (:method-combination progn)
		  (:method progn ((x integer)) 'a)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method progn ((x number)) 'b)
		  (:method progn ((x symbol)) 'c)
		  (:method progn ((x t)) 'd)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar d)) (foo (bar d)) (bar d) d d d)

(deftest defgeneric-method-combination.progn.6a
  (let ((fn
	 (eval '(defgeneric dg-mc.progn.6a (x)
		  (:method-combination progn :most-specific-last)
		  (:method progn ((x integer)) 'a)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method progn ((x number)) 'b)
		  (:method progn ((x symbol)) 'c)
		  (:method progn ((x t)) 'd)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar a)) (foo (bar b)) (bar b) b c d)


(deftest defgeneric-method-combination.progn.7
  (let ((fn
	 (eval '(defgeneric dg-mc.progn.7 (x)
		  (:method-combination progn)
		  (:method progn ((x dgmc-class-04)) 'a)
		  (:method progn ((x dgmc-class-03)) 'b)
		  (:method progn ((x dgmc-class-02)) 'c)
		  (:method progn ((x dgmc-class-01)) 'd)))))
    (declare (type generic-function fn))
    (values
     (funcall fn (make-instance 'dgmc-class-01))
     (funcall fn (make-instance 'dgmc-class-02))
     (funcall fn (make-instance 'dgmc-class-03))
     (funcall fn (make-instance 'dgmc-class-04))))
  d d d d)

(deftest defgeneric-method-combination.progn.7a
  (let ((fn
	 (eval '(defgeneric dg-mc.progn.7a (x)
		  (:method-combination progn :most-specific-last)
		  (:method progn ((x dgmc-class-04)) 'a)
		  (:method progn ((x dgmc-class-03)) 'b)
		  (:method progn ((x dgmc-class-02)) 'c)
		  (:method progn ((x dgmc-class-01)) 'd)))))
    (declare (type generic-function fn))
    (values
     (funcall fn (make-instance 'dgmc-class-01))
     (funcall fn (make-instance 'dgmc-class-02))
     (funcall fn (make-instance 'dgmc-class-03))
     (funcall fn (make-instance 'dgmc-class-04))))
  d c b a)

(deftest defgeneric-method-combination.progn.8
  (let ((fn
	 (eval '(defgeneric dg-mc.progn.8 (x)
		  (:method-combination progn)
		  (:method progn ((x (eql 1000))) 'a)
		  (:method :around ((x symbol)) (values))
		  (:method :around ((x integer)) (values 'a 'b 'c))
		  (:method :around ((x complex)) (call-next-method))
		  (:method :around ((x number)) (values 1 2 3 4 5 6))
		  (:method progn ((x t)) 'b)))))
    (declare (type generic-function fn))
    (values
     (multiple-value-list (funcall fn 'a))
     (multiple-value-list (funcall fn 10))
     (multiple-value-list (funcall fn #c(9 8)))
     (multiple-value-list (funcall fn '(a b c)))))
  () (a b c) (1 2 3 4 5 6) (b))

(deftest defgeneric-method-combination.progn.9
  (handler-case
   (let ((fn (eval '(defgeneric dg-mc.progn.9 (x)
		      (:method-combination progn)))))
     (declare (type generic-function fn))
     (funcall fn (list 'a)))
   (error () :error))
  :error)

(deftest defgeneric-method-combination.progn.10
  (progn
    (eval '(defgeneric dg-mc.progn.10 (x)
	     (:method-combination progn)
	     (:method ((x t)) 0)))
    (handler-case
     (dg-mc.progn.10 'a)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.progn.11
  (progn
    (eval '(defgeneric dg-mc.progn.11 (x)
	     (:method-combination progn)
	     (:method nonsense ((x t)) 0)))
    (handler-case
     (dg-mc.progn.11 0)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.progn.12
  (let ((fn (eval '(defgeneric dg-mc.progn.12 (x)
		     (:method-combination progn)
		     (:method :around ((x t)) 'a)
		     (:method progn ((x integer)) x)))))
    (declare (type generic-function fn))
    (handler-case (funcall fn 'b)
		  (error () :error)))
  :error)
