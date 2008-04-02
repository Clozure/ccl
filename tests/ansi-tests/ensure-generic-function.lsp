;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Mar 27 21:29:53 2003
;;;; Contains: Tests for ENSURE-GENERIC-FUNCTION

(in-package :cl-test)

(deftest ensure-generic-function.1
  (if (typep #'car 'generic-function)
      t
    (signals-error (ensure-generic-function 'car) error))
  t)

(deftest ensure-generic-function.2
  (signals-error (ensure-generic-function 'defclass) error)
  t)

(deftest ensure-generic-function.3
  (signals-error (ensure-generic-function 'tagbody) error)
  t)

(deftest ensure-generic-function.4
  (let ((f 'egf-fun-4))
    (when (fboundp f) (fmakunbound f))
    (values
     (fboundp f)
     (notnot-mv (typep (ensure-generic-function f) 'generic-function))
     (notnot-mv (typep (ensure-generic-function f) 'generic-function))
     (notnot-mv (typep (symbol-function f) 'generic-function))))
  nil t t t)

(deftest ensure-generic-function.5
  (let ((f 'egf-fun-5))
    (when (fboundp f) (fmakunbound f))
    (values
     (fboundp f)
     (notnot-mv (typep (ensure-generic-function f :lambda-list '(a b c))
		       'generic-function))
     ;; Test of incongruent generic function lambda list when no
     ;; methods exist
     (notnot-mv (typep (ensure-generic-function f :lambda-list '(x y))
		       'generic-function))
     (notnot-mv (typep (symbol-function f) 'generic-function))))
  nil t t t)

(deftest ensure-generic-function.6
  (let ((f 'egf-fun-6))
    (when (fboundp f) (fmakunbound f))
    (values
     (fboundp f)
     (notnot-mv (typep (ensure-generic-function f :lambda-list '(a b c))
		       'generic-function))
     (notnot-mv (eval `(defmethod ,f ((a t)(b t)(c t)) (list a b c))))
     ;; Test of incongruent generic function lambda list when no
     ;; methods exist
     (eval
      `(signals-error (ensure-generic-function ',f :lambda-list '(x y))
		      error))))
  nil t t t)

(deftest ensure-generic-function.7
  (let ((f 'egf-fun-7))
    (when (fboundp f) (fmakunbound f))
    (let ((fn (eval `(defgeneric ,f (x)
		       (:method ((x symbol)) (list x :a))
		       (:method ((x integer)) (list x :b))
		       (:method ((x t)) (list x :c))))))
      (values
       (mapcar fn '(x 2 3/2))
       (eqlt fn (ensure-generic-function f :lambda-list '(x)))
       (mapcar fn '(x 2 3/2)))))
  ((x :a) (2 :b) (3/2 :c))
  t
  ((x :a) (2 :b) (3/2 :c)))

(deftest ensure-generic-function.8
  (let ((f 'egf-fun-8))
    (when (fboundp f) (fmakunbound f))
    (let ((fn (eval `(defgeneric ,f (x y)
		       (:method ((x t) (y symbol)) 1)
		       (:method ((x symbol) (y t)) 2)))))
      (values
       (mapcar fn '(a a 3) '(b 4 b))
       (eqlt fn (ensure-generic-function f :lambda-list '(x y)
					 :argument-precedence-order '(y x)))
       (mapcar fn '(a a 3) '(b 4 b)))))
  (2 2 1)
  t
  (1 2 1))

(deftest ensure-generic-function.9
  (let ((f 'egf-fun-9))
    (when (fboundp f) (fmakunbound f))
    (let ((fn (eval `(defgeneric ,f (x)
		       (:method-combination +)
		       (:method + ((x t)) 1)
		       (:method + ((x symbol)) 2)
		       (:method + ((x (eql nil))) 4)))))
      (values
       (mapcar fn '(3/2 a nil))
       (eqlt fn (ensure-generic-function f :lambda-list '(x)
					 :method-class 'standard-method))
       (mapcar fn '(3/2 a nil))
       (eqlt fn (ensure-generic-function f :lambda-list '(x)
					 :method-class
					 (find-class 'standard-method)))
       (mapcar fn '(3/2 a nil)))))
       
       
  (1 3 7)
  t
  (1 3 7)
  t
  (1 3 7))

(deftest ensure-generic-function.10
  (let ((f 'egf-fun-10))
    (when (fboundp f) (fmakunbound f))
    (let ((fn (eval `(defgeneric ,f (x)
		       (:method ((x t)) 1)))))
      (values
       (funcall fn 'a)
       (eqlt fn (ensure-generic-function f :lambda-list '(x)
					 :generic-function-class
					 'standard-generic-function))
       (funcall fn 'a)
       (eqlt fn (ensure-generic-function f :lambda-list '(x)
					 :generic-function-class
					 (find-class 'standard-generic-function)))
       (funcall fn 'a))))
  1 t 1 t 1)

(deftest ensure-generic-function.11
  (let ((f 'egf-fun-11))
    (when (fboundp f) (fmakunbound f))
    (let ((fn (eval `(defgeneric ,f (x)
		       (:method ((x t)) 1)))))
      (values
       (funcall fn 'a)
       (eqlt fn (eval `(macrolet ((%m (&environment env)
				      (ensure-generic-function ',f :lambda-list '(x)
							       :environment env)))
			 (%m))))
       (funcall fn 'a))))
  1 t 1)

(deftest ensure-generic-function.12
  (let ((f 'egf-fun-12))
    (when (fboundp f) (fmakunbound f))
    (let ((fn (eval `(defgeneric ,f (x)
		       (:documentation "foo")
		       (:method ((x t)) 1)))))
      (values
       (funcall fn 'a)
       (or (documentation f 'function) "foo")
       (eqlt fn (ensure-generic-function f :lambda-list '(x) :documentation "bar"))
       (or (documentation f 'function) "bar")
       (funcall fn 'a))))
  1 "foo" t "bar" 1)

(deftest ensure-generic-function.13
  (let ((f 'egf-fun-13))
    (when (fboundp f) (fmakunbound f))
    (let ((fn (eval `(defgeneric ,f (x y)
		       (declare (optimize safety (speed 0) (debug 0) (space 0)))
		       (:method ((x t) (y t)) (list x y))))))
      (values
       (funcall fn 'a 'b)
       (eqlt fn (ensure-generic-function f :lambda-list '(x y)
					 :declare '((optimize (safety 0) (debug 2) speed (space 1)))))
       (funcall fn 'a 1))))
  (a b) t (a 1))

(deftest ensure-generic-function.14
  (let ((f '(setf egf-fun-14)))
    (when (fboundp f) (fmakunbound f))
    (let ((fn (eval `(defgeneric ,f (val x)
		       (:method ((val t) (x cons)) (setf (car x) val))))))
      (values
       (let ((z (cons 'a 'b)))
	 (list (setf (egf-fun-14 z) 'c) z))
       (eqlt fn (ensure-generic-function f :lambda-list '(val x)))
       (let ((z (cons 'a 'b)))
	 (list (setf (egf-fun-14 z) 'c) z)))))
  (c (c . b)) t (c (c . b)))       
		       
;;; Many more tests are needed for other combinations of keyword parameters

(deftest ensure-generic-function.error.1
  (signals-error (ensure-generic-function) program-error)
  t)

(deftest ensure-generic-function.error.2
  (signals-error (ensure-generic-function (gensym) :lambda-list) program-error)
  t)
