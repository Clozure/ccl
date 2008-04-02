;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination AND

(in-package :cl-test)

(declaim (special *x*))

(compile-and-load "defgeneric-method-combination-aux.lsp")

(deftest defgeneric-method-combination.and.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.and.1 (x)
		  (:method-combination and)
		  (:method and ((x integer)) (push 4 *x*) t)
		  (:method and ((x rational)) (push 3 *x*) nil)
		  (:method and ((x number)) (push 2 *x*) t)
		  (:method and ((x t)) (push 1 *x*) 'a)))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (nil (3 4))
  (nil (3))
  (a (1 2))
  (a (1)))

(deftest defgeneric-method-combination.and.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.and.2 (x)
		  (:method-combination and :most-specific-first)
		  (:method and ((x integer)) (push 4 *x*) t)
		  (:method and ((x rational)) (push 3 *x*) nil)
		  (:method and ((x number)) (push 2 *x*) t)
		  (:method and ((x t)) (push 1 *x*) 'a)))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (nil (3 4))
  (nil (3))
  (a (1 2))
  (a (1)))

(deftest defgeneric-method-combination.and.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.and.3 (x)
		  (:method-combination and :most-specific-last)
		  (:method and ((x integer)) (push 4 *x*) t)
		  (:method and ((x rational)) (push 3 *x*) nil)
		  (:method and ((x number)) (push 2 *x*) 'a)
		  (:method and ((x t)) (push 1 *x*) t)))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
      (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (nil (3 2 1))
  (nil (3 2 1))
  (a (2 1))
  (t (1)))

(deftest defgeneric-method-combination.and.4
  (let ((fn
	 (eval '(defgeneric dg-mc.and.4 (x)
		  (:method-combination and)
		  (:method and ((x integer)) t)
		  (:method :around ((x rational)) 'foo)
		  (:method and ((x number)) nil)
		  (:method and ((x symbol)) t)
		  (:method and ((x t)) 'a)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo nil a a)

(deftest defgeneric-method-combination.and.5
  (let ((fn
	 (eval '(defgeneric dg-mc.and.5 (x)
		  (:method-combination and)
		  (:method and ((x integer)) nil)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method and ((x number)) 'a)
		  (:method and ((x symbol)) 'b)
		  (:method and ((x t)) 'c)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo nil) (foo c) c c c)

(deftest defgeneric-method-combination.and.6
  (let ((fn
	 (eval '(defgeneric dg-mc.and.6 (x)
		  (:method-combination and)
		  (:method and ((x integer)) 'a)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method and ((x number)) nil)
		  (:method and ((x symbol)) 'c)
		  (:method and ((x t)) 'd)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar nil)) (foo (bar nil)) (bar nil) nil d d)

(deftest defgeneric-method-combination.and.7
  (let ((fn
	 (eval '(defgeneric dg-mc.and.7 (x)
		  (:method-combination and)
		  (:method and ((x dgmc-class-04)) 'c)
		  (:method and ((x dgmc-class-03)) 'b)
		  (:method and ((x dgmc-class-02)) nil)
		  (:method and ((x dgmc-class-01)) 'a)))))
    (declare (type generic-function fn))
    (values
     (funcall fn (make-instance 'dgmc-class-01))
     (funcall fn (make-instance 'dgmc-class-02))
     (funcall fn (make-instance 'dgmc-class-03))
     (funcall fn (make-instance 'dgmc-class-04))))
  a nil a nil)
  
(deftest defgeneric-method-combination.and.8
  (let ((fn
	 (eval '(defgeneric dg-mc.and.8 (x)
		  (:method-combination and)
		  (:method and ((x (eql 1000))) 'a)
		  (:method :around ((x symbol)) (values))
		  (:method :around ((x integer)) (values 'a 'b 'c))
		  (:method :around ((x complex)) (call-next-method))
		  (:method :around ((x number)) (values 1 2 3 4 5 6))
		  (:method and ((x t)) 'b)))))
    (declare (type generic-function fn))
    (values
     (multiple-value-list (funcall fn 'a))
     (multiple-value-list (funcall fn 10))
     (multiple-value-list (funcall fn #c(9 8)))
     (multiple-value-list (funcall fn '(a b c)))))
  () (a b c) (1 2 3 4 5 6) (b))

(deftest defgeneric-method-combination.and.9
  (handler-case
   (let ((fn (eval '(defgeneric dg-mc.and.9 (x)
		      (:method-combination and)))))
     (declare (type generic-function fn))
     (funcall fn 'x))
   (error () :error))
  :error)

(deftest defgeneric-method-combination.and.10
  (progn
    (eval '(defgeneric dg-mc.and.10 (x)
	     (:method-combination and)
	     (:method ((x t)) t)))    
    (handler-case
     (dg-mc.and.10 'a)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.and.11
  (progn
    (eval '(defgeneric dg-mc.and.11 (x)
	     (:method-combination and)
	     (:method nonsense ((x t)) t)))
    (handler-case
     (dg-mc.and.11 0)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.and.12
  (let ((fn (eval '(defgeneric dg-mc.and.12 (x)
		     (:method-combination and)
		     (:method :around ((x t)) t)
		     (:method and ((x integer)) x)))))
    (declare (type generic-function fn))
    (handler-case (funcall fn 'x)
		  (error () :error)))
  :error)
