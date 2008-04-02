;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 11 14:41:50 2003
;;;; Contains: Tests of NO-NEXT-METHOD

(in-package :cl-test)

(defgeneric no-next-meth-gf-01 (x))

(defmethod no-next-meth-gf-01 ((x integer))
  (call-next-method))

(defmethod no-next-meth-gf-01 :around ((x character))
  (call-next-method))

(deftest no-next-method.1
  (handler-case (progn (no-next-meth-gf-01 10) :bad)
		(error () :good))
  :good)

(deftest no-next-method.2
  (handler-case (progn (no-next-meth-gf-01 ) :bad)
		(error () :good))
  :good)

;;; (defparameter *no-next-meth-gf-02*
;;;   (defgeneric no-next-meth-gf-02 (x)))
;;; 
;;; (defmethod no-next-meth-gf-02 ((x integer))
;;;   (call-next-method))
;;; 
;;; (defmethod no-next-meth-gf-02 :around ((x character))
;;;   (call-next-method))
;;; 
;;; (defmethod no-next-method ((gf (eql *no-next-meth-gf-02*))
;;; 			   (method standard-method)
;;; 			   &rest args)
;;;   (values (copy-list args) :aborted))
;;; 
;;; (deftest no-next-method.3
;;;   (no-next-meth-gf-02 10)
;;;   (10) :aborted)
;;; 
;;; (deftest no-next-method.4
;;;   (no-next-meth-gf-02 #\a)
;;;   (#\a) :aborted)
