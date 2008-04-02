;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination OR

(in-package :cl-test)

(declaim (special *x*))

(compile-and-load "defgeneric-method-combination-aux.lsp")

(deftest defgeneric-method-combination.or.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.or.1 (x)
		  (:method-combination or)
		  (:method or ((x integer)) (push 4 *x*) nil)
		  (:method or ((x rational)) (push 3 *x*) nil)
		  (:method or ((x number)) (push 2 *x*) nil)
		  (:method or ((x t)) (push 1 *x*) 'a)))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (a (1 2 3 4))
  (a (1 2 3))
  (a (1 2))
  (a (1)))

(deftest defgeneric-method-combination.or.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.or.2 (x)
		  (:method-combination or :most-specific-first)
		  (:method or ((x integer)) (push 4 *x*) nil)
		  (:method or ((x rational)) (push 3 *x*) 'a)
		  (:method or ((x number)) (push 2 *x*) nil)
		  (:method or ((x t)) (push 1 *x*) 'b)))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (a (3 4)) (a (3)) (b (1 2)) (b (1)))

(deftest defgeneric-method-combination.or.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.or.3 (x)
		  (:method-combination or :most-specific-last)
		  (:method or ((x integer)) (push 4 *x*) 'a)
		  (:method or ((x rational)) (push 3 *x*) nil)
		  (:method or ((x number)) (push 2 *x*) nil)
		  (:method or ((x t)) (push 1 *x*) nil)))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (a (4 3 2 1)) (nil (3 2 1)) (nil (2 1)) (nil (1)))

(deftest defgeneric-method-combination.or.4
  (let ((fn
	 (eval '(defgeneric dg-mc.or.4 (x)
		  (:method-combination or)
		  (:method or ((x integer)) nil)
		  (:method :around ((x rational)) 'foo)
		  (:method or ((x number)) 'b)
		  (:method or ((x symbol)) nil)
		  (:method or ((x t)) 'a)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo b a a)

(deftest defgeneric-method-combination.or.5
  (let ((fn
	 (eval '(defgeneric dg-mc.or.5 (x)
		  (:method-combination or)
		  (:method or ((x integer)) 'a)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method or ((x number)) nil)
		  (:method or ((x symbol)) 'b)
		  (:method or ((x t)) 'c)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo a) (foo c) c b c)

(deftest defgeneric-method-combination.or.6
  (let ((fn
	 (eval '(defgeneric dg-mc.or.6 (x)
		  (:method-combination or)
		  (:method or ((x integer)) 'a)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method or ((x number)) 'b)
		  (:method or ((x symbol)) 'c)
		  (:method or ((x t)) 'd)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar a)) (foo (bar b)) (bar b) b c d)

(deftest defgeneric-method-combination.or.7
  (let ((fn
	 (eval '(defgeneric dg-mc.or.7 (x)
		  (:method-combination or)
		  (:method or ((x dgmc-class-04)) nil)
		  (:method or ((x dgmc-class-03)) nil)
		  (:method or ((x dgmc-class-02)) 'b)
		  (:method or ((x dgmc-class-01)) 'c)))))
    (declare (type generic-function fn))
    (values
     (funcall fn (make-instance 'dgmc-class-01))
     (funcall fn (make-instance 'dgmc-class-02))
     (funcall fn (make-instance 'dgmc-class-03))
     (funcall fn (make-instance 'dgmc-class-04))))
  c b c b)
  
(deftest defgeneric-method-combination.or.8
  (let ((fn
	 (eval '(defgeneric dg-mc.or.8 (x)
		  (:method-combination or)
		  (:method or ((x (eql 1000))) 'a)
		  (:method :around ((x symbol)) (values))
		  (:method :around ((x integer)) (values 'a 'b 'c))
		  (:method :around ((x complex)) (call-next-method))
		  (:method :around ((x number)) (values 1 2 3 4 5 6))
		  (:method or ((x t)) 'b)))))
    (declare (type generic-function fn))
    (values
     (multiple-value-list (funcall fn 'a))
     (multiple-value-list (funcall fn 10))
     (multiple-value-list (funcall fn #c(9 8)))
     (multiple-value-list (funcall fn '(a b c)))))
  () (a b c) (1 2 3 4 5 6) (b))

(deftest defgeneric-method-combination.or.9
  (handler-case
   (let ((fn (eval '(defgeneric dg-mc.or.9 (x)
		      (:method-combination or)))))
     (declare (type generic-function fn))
     (funcall fn (list 'a)))
   (error () :error))
  :error)

(deftest defgeneric-method-combination.or.10
  (progn
    (eval '(defgeneric dg-mc.or.10 (x)
	     (:method-combination or)
	     (:method ((x t)) 0)))
    (handler-case
     (dg-mc.or.10 'a)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.or.11
  (progn
    (eval '(defgeneric dg-mc.or.11 (x)
	     (:method-combination or)
	     (:method nonsense ((x t)) 0)))
    (handler-case
     (dg-mc.or.11 0)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.or.12
  (let ((fn (eval '(defgeneric dg-mc.or.12 (x)
		     (:method-combination or)
		     (:method :around ((x t)) t)
		     (:method or ((x integer)) x)))))
    (declare (type generic-function fn))
    (handler-case (funcall fn 'a)
		  (error () :error)))
  :error)
