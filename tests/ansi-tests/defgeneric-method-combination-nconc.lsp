;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination NCONC

(in-package :cl-test)

(declaim (special *x*))

(compile-and-load "defgeneric-method-combination-aux.lsp")

(deftest defgeneric-method-combination.nconc.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.nconc.1 (x)
		  (:method-combination nconc)
		  (:method nconc ((x integer))
			   (copy-list (car (push '(d) *x*))))
		  (:method nconc ((x rational))
			   (copy-list (car (push '(c) *x*))))
		  (:method nconc ((x number))
			   (copy-list (car (push '(b) *x*))))
		  (:method nconc ((x t))
			   (copy-list (car (push '(a) *x*))))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  ((d c b a) ((a) (b) (c) (d)))
  ((c b a) ((a) (b) (c)))
  ((b a) ((a) (b)))
  ((a) ((a))))

(deftest defgeneric-method-combination.nconc.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.nconc.2 (x)
		  (:method-combination nconc :most-specific-first)
		  (:method nconc ((x integer))
			   (copy-list (car (push '(d) *x*))))
		  (:method nconc ((x rational))
			   (copy-list (car (push '(c) *x*))))
		  (:method nconc ((x number))
			   (copy-list (car (push '(b) *x*))))
		  (:method nconc ((x t))
			   (copy-list (car (push '(a) *x*))))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  ((d c b a) ((a) (b) (c) (d)))
  ((c b a) ((a) (b) (c)))
  ((b a) ((a) (b)))
  ((a) ((a))))

(deftest defgeneric-method-combination.nconc.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.nconc.3 (x)
		  (:method-combination nconc :most-specific-last)
		  (:method nconc ((x integer))
			   (copy-list (car (push '(d) *x*))))
		  (:method nconc ((x rational))
			   (copy-list (car (push '(c) *x*))))
		  (:method nconc ((x number))
			   (copy-list (car (push '(b) *x*))))
		  (:method nconc ((x t))
			   (copy-list (car (push '(a) *x*))))))))
    (declare (type generic-function fn))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  ((a b c d) ((d) (c) (b) (a)))
  ((a b c) ((c) (b) (a)))
  ((a b) ((b) (a)))
  ((a) ((a))))

(deftest defgeneric-method-combination.nconc.4
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.nconc.4 (x)
		  (:method-combination nconc)
		  (:method nconc ((x integer)) (list 'a 'b))
		  (:method :around ((x rational)) 'foo)
		  (:method nconc ((x number)) (list 'c 'd))
		  (:method nconc ((x symbol)) (list 'e 'f))
		  (:method nconc ((x t)) (list 'g 'h))))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo (c d g h) (e f g h) (g h))

(deftest defgeneric-method-combination.nconc.5
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.nconc.5 (x)
		  (:method-combination nconc)
		  (:method nconc ((x integer)) (list 'a))
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method nconc ((x number)) (list 'b))
		  (:method nconc ((x symbol)) (list 'c))
		  (:method nconc ((x t)) (cons 'd 'e))))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (a b d . e)) (foo (b d . e)) (b d . e) (c d . e) (d . e))

(deftest defgeneric-method-combination.nconc.6
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.nconc.6 (x)
		  (:method-combination nconc)
		  (:method nconc ((x integer)) (list 'a))
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method nconc ((x number)) (list 'b))
		  (:method nconc ((x symbol)) (list 'c))
		  (:method nconc ((x t)) (list 'd))))))
    (declare (type generic-function fn))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar (a b d))) (foo (bar (b d))) (bar (b d)) (b d) (c d) (d))

(deftest defgeneric-method-combination.nconc.7
  (let ((fn
	 (eval '(defgeneric dg-mc.fun.nconc.7 (x)
		  (:method-combination nconc)
		  (:method nconc ((x dgmc-class-04)) (list 'a))
		  (:method nconc ((x dgmc-class-03)) (list 'b))
		  (:method nconc ((x dgmc-class-02)) (list 'c))
		  (:method nconc ((x dgmc-class-01)) (list 'd))))))
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

(deftest defgeneric-method-combination.nconc.8
  (let ((fn
	 (eval '(defgeneric dg-mc.nconc.8 (x)
		  (:method-combination nconc)
		  (:method nconc ((x (eql 1000))) (list 'a))
		  (:method :around ((x symbol)) (values))
		  (:method :around ((x integer)) (values 'a 'b 'c))
		  (:method :around ((x complex)) (call-next-method))
		  (:method :around ((x number)) (values 1 2 3 4 5 6))
		  (:method nconc ((x t)) (list 'b))))))
    (declare (type generic-function fn))
    (values
     (multiple-value-list (funcall fn 'a))
     (multiple-value-list (funcall fn 10))
     (multiple-value-list (funcall fn #c(9 8)))
     (multiple-value-list (funcall fn '(a b c)))))
  () (a b c) (1 2 3 4 5 6) ((b)))

(deftest defgeneric-method-combination.nconc.9
  (handler-case
   (let ((fn (eval '(defgeneric dg-mc.nconc.9 (x)
		      (:method-combination nconc)))))
     (declare (type generic-function fn))
     (funcall fn (list 'a)))
   (error () :error))
  :error)

(deftest defgeneric-method-combination.nconc.10
  (progn
    (eval '(defgeneric dg-mc.nconc.10 (x)
	     (:method-combination nconc)
	     (:method ((x t)) (list 'a))))
    (handler-case
     (dg-mc.nconc.10 'a)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.nconc.11
  (progn
    (eval '(defgeneric dg-mc.nconc.11 (x)
	    (:method-combination nconc)
	    (:method nonsense ((x t)) (list 'a))))
    (handler-case
     (dg-mc.nconc.11 0)
     (error () :error)))
  :error)

(deftest defgeneric-method-combination.nconc.12
  (let ((fn (eval '(defgeneric dg-mc.nconc.12 (x)
		     (:method-combination nconc)
		     (:method :around ((x t)) (list 'a))
		     (:method nconc ((x integer)) x)))))
    (declare (type generic-function fn))
    (handler-case (funcall fn (list 'b))
		  (error () :error)))
  :error)
