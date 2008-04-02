;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jun  3 21:12:03 2003
;;;; Contains: Tests for FIND-METHOD

(in-package :cl-test)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defgeneric find-method-gf-01 (x)))
  (report-and-ignore-errors
   (defparameter *find-method-gf-01-method1*
     (defmethod find-method-gf-01 ((x integer)) 'a)))
  (report-and-ignore-errors
   (defparameter *find-method-gf-01-method2*
     (defmethod find-method-gf-01 ((x rational)) 'b)))
  (report-and-ignore-errors
   (defparameter *find-method-gf-01-method3*
     (defmethod find-method-gf-01 ((x real)) 'c)))
  (report-and-ignore-errors
   (defparameter *find-method-gf-01-method4*
     (defmethod find-method-gf-01 ((x t)) 'd)))
  )

(deftest find-method.1
  (eqt (find-method #'find-method-gf-01 nil (list (find-class 'integer)))
       *find-method-gf-01-method1*)
  t)

(deftest find-method.2
  (eqt (find-method #'find-method-gf-01 nil (list (find-class 'rational)))
       *find-method-gf-01-method2*)
  t)

(deftest find-method.3
  (eqt (find-method #'find-method-gf-01 nil (list (find-class 'real)))
       *find-method-gf-01-method3*)
  t)

(deftest find-method.4
  (eqt (find-method #'find-method-gf-01 nil (list (find-class t)))
       *find-method-gf-01-method4*)
  t)

(deftest find-method.5
  (find-method #'find-method-gf-01 (list :around) (list (find-class t))
	       nil)
  nil)

(deftest find-method.6
  (find-method #'find-method-gf-01 (list :after)
	       (list (find-class 'integer)) nil)
  nil)

(deftest find-method.7
  (find-method #'find-method-gf-01 (list :before) (list (find-class 'real))
	       nil)
  nil)

;;; EQL specializers

(defgeneric find-method-gf-02 (x))

(defparameter *find-method-gf-02-method1*
  (defmethod find-method-gf-02 ((x (eql 1234567890))) 'a))

(defparameter *find-method-02-method2-value* (list 'a))

(defparameter *find-method-gf-02-method2*
  (defmethod find-method-gf-02 ((x (eql *find-method-02-method2-value*)))
    'b))

(deftest find-method.8
  (eqt (find-method #'find-method-gf-02 nil (list '(eql 1234567890)))
       *find-method-gf-02-method1*)
  t)

(deftest find-method.9
  (eqt (find-method #'find-method-gf-02 nil
		    (list (list 'eql *find-method-02-method2-value*)))
       *find-method-gf-02-method2*)
  t)

;;; Error tests

(deftest find-method.error.1
  (signals-error (find-method) program-error)
  t)

(deftest find-method.error.2
  (signals-error (find-method #'find-method-gf-01) program-error)
  t)

(deftest find-method.error.3
  (signals-error (find-method #'find-method-gf-01 nil) program-error)
  t)

(deftest find-method.error.4
  (signals-error
   (find-method #'find-method-gf-01 nil (list (find-class 'integer)) nil nil)
   program-error)
  t)

(deftest find-method.error.5
  (handler-case
   (find-method #'find-method-gf-01 nil (list (find-class 'symbol)))
   (error () :error))
  :error)

(deftest find-method.error.6
  (handler-case
   (find-method #'find-method-gf-01 nil (list (find-class 'symbol)) 'x)
   (error () :error))
  :error)

(deftest find-method.error.7
  (handler-case
   (find-method #'find-method-gf-01 nil nil)
   (error () :error))
  :error)

(deftest find-method.error.8
  (handler-case
   (find-method #'find-method-gf-01 nil (list (find-class 'integer)
					      (find-class t)))
   (error () :error))
  :error)

(deftest find-method.error.9
  (handler-case
   (find-method #'find-method-gf-01 nil nil nil)
   (error () :error))
  :error)

(deftest find-method.error.10
  (handler-case
   (find-method #'find-method-gf-01 nil (list (find-class 'integer)
					      (find-class t))
		nil)
   (error () :error))
  :error)









