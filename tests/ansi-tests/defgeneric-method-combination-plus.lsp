;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination +

(in-package :cl-test)

(declaim (special *x*))

(compile-and-load "defgeneric-method-combination-aux.lsp")

(deftest defgeneric-method-combination.+.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.plus.1 (x)
		  (:method-combination +)
		  (:method + ((x integer)) (car (push 8 *x*)))
		  (:method + ((x rational)) (car (push 4 *x*)))
		  (:method + ((x number)) (car (push 2 *x*)))
		  (:method + ((x t)) (car (push 1 *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (15 (1 2 4 8)) (7 (1 2 4)) (3 (1 2)) (1 (1)))

(deftest defgeneric-method-combination.+.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.plus.2 (x)
		  (:method-combination + :most-specific-first)
		  (:method + ((x integer)) (car (push 8 *x*)))
		  (:method + ((x rational)) (car (push 4 *x*)))
		  (:method + ((x number)) (car (push 2 *x*)))
		  (:method + ((x t)) (car (push 1 *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (15 (1 2 4 8)) (7 (1 2 4)) (3 (1 2)) (1 (1)))

(deftest defgeneric-method-combination.+.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.plus.3 (x)
		  (:method-combination + :most-specific-last)
		  (:method + ((x integer)) (car (push 8 *x*)))
		  (:method + ((x rational)) (car (push 4 *x*)))
		  (:method + ((x number)) (car (push 2 *x*)))
		  (:method + ((x t)) (car (push 1 *x*)))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (15 (8 4 2 1)) (7 (4 2 1)) (3 (2 1)) (1 (1)))

(deftest defgeneric-method-combination.+.4
  (let ((fn
	 (eval '(defgeneric dg-mc.plus.4 (x)
		  (:method-combination +)
		  (:method + ((x integer)) 1)
		  (:method :around ((x rational)) 'foo)
		  (:method + ((x number)) 1)
		  (:method + ((x symbol)) 2)
		  (:method + ((x t)) 4)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo 5 6 4)

(deftest defgeneric-method-combination.+.5
  (let ((fn
	 (eval '(defgeneric dg-mc.plus.5 (x)
		  (:method-combination +)
		  (:method + ((x integer)) 1)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method + ((x number)) 2)
		  (:method + ((x symbol)) 4)
		  (:method + ((x t)) 8)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo 11) (foo 10) 10 12 8)

(deftest defgeneric-method-combination.+.6
  (let ((fn
	 (eval '(defgeneric dg-mc.plus.6 (x)
		  (:method-combination +)
		  (:method + ((x integer)) 1)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method + ((x number)) 2)
		  (:method + ((x symbol)) 4)
		  (:method + ((x t)) 8)))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar 11)) (foo (bar 10)) (bar 10) 10 12 8)

(deftest defgeneric-method-combination.+.7
  (let ((fn
	 (eval '(defgeneric dg-mc.plus.7 (x)
		  (:method-combination +)
		  (:method + ((x dgmc-class-04)) 1)
		  (:method + ((x dgmc-class-03)) 2)
		  (:method + ((x dgmc-class-02)) 4)
		  (:method + ((x dgmc-class-01)) 8)))))
    (declare (type generic-function fn))
    (values
     (funcall fn (make-instance 'dgmc-class-01))
     (funcall fn (make-instance 'dgmc-class-02))
     (funcall fn (make-instance 'dgmc-class-03))
     (funcall fn (make-instance 'dgmc-class-04))))
  8 12 10 15)

(deftest defgeneric-method-combination.+.8
  (let ((fn
	 (eval '(defgeneric dg-mc.plus.8 (x)
		  (:method-combination +)
		  (:method + ((x (eql 1000))) 1)
		  (:method :around ((x symbol)) (values))
		  (:method :around ((x integer)) (values 'a 'b 'c))
		  (:method :around ((x complex)) (call-next-method))
		  (:method :around ((x number)) (values 1 2 3 4 5 6))
		  (:method + ((x t)) 1)))))
    (declare (type generic-function fn))
    (values
     (multiple-value-list (funcall fn 'a))
     (multiple-value-list (funcall fn 10))
     (multiple-value-list (funcall fn #c(9 8)))
     (multiple-value-list (funcall fn '(a b c)))))
  () (a b c) (1 2 3 4 5 6) (1))

(deftest defgeneric-method-combination.+.9
  (handler-case
   (let ((fn (eval '(defgeneric dg-mc.+.9 (x)
		      (:method-combination +)))))
     (declare (type generic-function fn))
     (funcall fn (list 'a)))
   (error () :error))
  :error)

(deftest defgeneric-method-combination.+.10
  (progn
    (eval '(defgeneric dg-mc.+.10 (x)
	     (:method-combination +)
	     (:method ((x t)) 0)))
    (handler-case
     (dg-mc.+.10 'a)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.+.11
  (progn
    (eval '(defgeneric dg-mc.+.11 (x)
	     (:method-combination +)
	     (:method nonsense ((x t)) 0)))
    (handler-case
     (dg-mc.+.11 0)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.+.12
  (let ((fn (eval '(defgeneric dg-mc.+.12 (x)
		     (:method-combination +)
		     (:method :around ((x t)) 1)
		     (:method + ((x integer)) x)))))
    (declare (type generic-function fn))
    (handler-case (funcall fn 'a)
		  (error () :error)))
  :error)

