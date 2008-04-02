;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 11 07:14:12 2003
;;;; Contains: Tests of METHOD-QUALIFIERS

(in-package :cl-test)

(defgeneric mq-generic-function (x))

(defparameter *mq-method-1*
  (defmethod mq-generic-function ((x integer)) (1+ x)))

(deftest method-qualifiers.1
  (method-qualifiers *mq-method-1*)
  nil)

(defclass mq-class-01 ()
  (a b c))

(defparameter *mq-method-2*
  (defmethod mq-generic-function :before ((x mq-class-01))
    'foo))

(deftest method-qualifiers.2
  (method-qualifiers *mq-method-2*)
  (:before))

(defclass mq-class-02 ()
  (e f g))

(defparameter *mq-method-3*
  (defmethod mq-generic-function :after ((x mq-class-02))
    'foo))

(deftest method-qualifiers.3
  (method-qualifiers *mq-method-3*)
  (:after))

(defclass mq-class-03 ()
  (h i j))

(defparameter *mq-method-4*
  (defmethod mq-generic-function :around ((x mq-class-03))
    'foo))

(deftest method-qualifiers.4
  (method-qualifiers *mq-method-4*)
  (:around))

;;; Need tests on user-defined method combinations

(deftest method-qualifiers.error.1
  (signals-error (method-qualifiers) program-error)
  t)

(deftest method-qualifiers.error.2
  (signals-error (method-qualifiers *mq-method-4* nil) program-error)
  t)
