;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination MAX

(in-package :cl-test)

(declaim (special *x*))

(compile-and-load "defgeneric-method-combination-aux.lsp")

(deftest defgeneric-method-combination.max.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.max.1 (x)
		  (:method-combination max)
		  (:method max ((x integer)) (car (push 8 *x*)))
		  (:method max ((x rational)) (car (push 4 *x*)))
		  (:method max ((x number)) (car (push 2 *x*)))
		  (:method max ((x t)) (car (push 1 *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (8 (1 2 4 8)) (4 (1 2 4)) (2 (1 2)) (1 (1)))

(deftest defgeneric-method-combination.max.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.max.2 (x)
		  (:method-combination max :most-specific-first)
		  (:method max ((x integer)) (car (push 8 *x*)))
		  (:method max ((x rational)) (car (push 4 *x*)))
		  (:method max ((x number)) (car (push 2 *x*)))
		  (:method max ((x t)) (car (push 1 *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (8 (1 2 4 8)) (4 (1 2 4)) (2 (1 2)) (1 (1)))

(deftest defgeneric-method-combination.max.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.max.3 (x)
		  (:method-combination max :most-specific-last)
		  (:method max ((x integer)) (car (push 8 *x*)))
		  (:method max ((x rational)) (car (push 4 *x*)))
		  (:method max ((x number)) (car (push 2 *x*)))
		  (:method max ((x t)) (car (push 1 *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (8 (8 4 2 1)) (4 (4 2 1)) (2 (2 1)) (1 (1)))

(deftest defgeneric-method-combination.max.4
  (let ((fn
	 (eval '(defgeneric dg-mc.max.4 (x)
		  (:method-combination max)
		  (:method max ((x integer)) 4)
		  (:method :around ((x rational)) 'foo)
		  (:method max ((x number)) 3)
		  (:method max ((x symbol)) 5)
		  (:method max ((x t)) 1)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo 3 5 1)

(deftest defgeneric-method-combination.max.5
  (let ((fn
	 (eval '(defgeneric dg-mc.max.5 (x)
		  (:method-combination max)
		  (:method max ((x integer)) 5)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method max ((x number)) 5/2)
		  (:method max ((x symbol)) 4)
		  (:method max ((x t)) 1.0)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo 5) (foo 5/2) 5/2 4 1.0)

(deftest defgeneric-method-combination.max.6
  (let ((fn
	 (eval '(defgeneric dg-mc.max.6 (x)
		  (:method-combination max)
		  (:method max ((x integer)) 9)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method max ((x number)) 4)
		  (:method max ((x symbol)) 6)
		  (:method max ((x t)) 1)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar 9)) (foo (bar 4)) (bar 4) 4 6 1)

(deftest defgeneric-method-combination.max.7
  (let ((fn
	 (eval '(defgeneric dg-mc.max.7 (x)
		  (:method-combination max)
		  (:method max ((x dgmc-class-04)) 4)
		  (:method max ((x dgmc-class-03)) 3)
		  (:method max ((x dgmc-class-02)) 5)
		  (:method max ((x dgmc-class-01)) 1)))))
    (declare (type generic-function fn))
    (values
     (funcall fn (make-instance 'dgmc-class-01))
     (funcall fn (make-instance 'dgmc-class-02))
     (funcall fn (make-instance 'dgmc-class-03))
     (funcall fn (make-instance 'dgmc-class-04))))
  1 5 3 5)

(deftest defgeneric-method-combination.max.8
  (let ((fn
	 (eval '(defgeneric dg-mc.max.8 (x)
		  (:method-combination max)
		  (:method max ((x (eql 1000))) 4)
		  (:method :around ((x symbol)) (values))
		  (:method :around ((x integer)) (values 'a 'b 'c))
		  (:method :around ((x complex)) (call-next-method))
		  (:method :around ((x number)) (values 1 2 3 4 5 6))
		  (:method max ((x t)) 1)))))
    (declare (type generic-function fn))
    (values
     (multiple-value-list (funcall fn 'a))
     (multiple-value-list (funcall fn 10))
     (multiple-value-list (funcall fn #c(9 8)))
     (multiple-value-list (funcall fn '(a b c)))))
  () (a b c) (1 2 3 4 5 6) (1))

(deftest defgeneric-method-combination.max.9
  (handler-case
   (let ((fn (eval '(defgeneric dg-mc.max.9 (x)
		      (:method-combination max)))))
     (declare (type generic-function fn))
     (funcall fn (list 'a)))
   (error () :error))
  :error)

(deftest defgeneric-method-combination.max.10
  (progn
    (eval '(defgeneric dg-mc.max.10 (x)
	     (:method-combination max)
	     (:method ((x t)) 0)))
    (handler-case
     (dg-mc.max.10 'a)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.max.11
  (progn
    (eval '(defgeneric dg-mc.max.11 (x)
	     (:method-combination max)
	     (:method nonsense ((x t)) 0)))
    (handler-case
     (dg-mc.max.11 0)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.max.12
  (let ((fn (eval '(defgeneric dg-mc.max.12 (x)
		     (:method-combination max)
		     (:method :around ((x t)) 1)
		     (:method max ((x integer)) x)))))
    (declare (type generic-function fn))
    (handler-case (funcall fn 'a)
		  (error () :error)))
  :error)
