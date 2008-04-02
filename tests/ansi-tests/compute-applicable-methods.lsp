;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jun  2 06:40:41 2003
;;;; Contains: Tests for COMPUTE-APPLICABLE-METHODS

(in-package :cl-test)

(defgeneric cam-gf-01 (x y))

(defparameter *cam-gf-01-method1*
  (defmethod cam-gf-01 ((x integer) (y integer)) 1))

(defparameter *cam-gf-01-method2*
  (defmethod cam-gf-01 ((x integer) (y t)) 2))

(defparameter *cam-gf-01-method3*
  (defmethod cam-gf-01 ((x t) (y integer)) 3))

(defparameter *cam-gf-01-method4*
  (defmethod cam-gf-01 ((x t) (y t)) 4))

(deftest compute-applicable-methods.1
  (let ((methods (compute-applicable-methods #'cam-gf-01 (list 1 2))))
    (equalt methods
	    (list *cam-gf-01-method1* *cam-gf-01-method2*
		  *cam-gf-01-method3* *cam-gf-01-method4*)))
  t)

(deftest compute-applicable-methods.2
  (let ((methods (compute-applicable-methods #'cam-gf-01 (list 1 'x))))
    (equalt methods
	    (list *cam-gf-01-method2* *cam-gf-01-method4*)))
  t)

(deftest compute-applicable-methods.3
  (let ((methods (compute-applicable-methods #'cam-gf-01 (list 'x 10))))
    (equalt methods
	    (list *cam-gf-01-method3* *cam-gf-01-method4*)))
  t)

(deftest compute-applicable-methods.4
  (let ((methods (compute-applicable-methods #'cam-gf-01 (list 'x 'y))))
    (equalt methods (list *cam-gf-01-method4*)))
  t)

(defgeneric cam-gf-02 (x))

(deftest compute-applicable-methods.5
  (compute-applicable-methods #'cam-gf-02 '(1))
  nil)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defgeneric cam-gf-03 (x)
     (:method-combination + :most-specific-first))
   
   (defparameter *cam-gf-03-method1*
     (defmethod cam-gf-03 + ((x integer)) 1))
   
   (defparameter *cam-gf-03-method2*
     (defmethod cam-gf-03 + ((x rational)) 2))
   
   (defparameter *cam-gf-03-method3*
     (defmethod cam-gf-03 + ((x real)) 4))
   
   (defparameter *cam-gf-03-method4*
     (defmethod cam-gf-03 + ((x number)) 8))
   
   (defparameter *cam-gf-03-method5*
     (defmethod cam-gf-03 + ((x t)) 16))))

(deftest compute-applicable-methods.6
  (equalt (compute-applicable-methods #'cam-gf-03 (list 0))
	  (list *cam-gf-03-method1* *cam-gf-03-method2* *cam-gf-03-method3*
		*cam-gf-03-method4* *cam-gf-03-method5*))
  t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defgeneric cam-gf-04 (x)
     (:method-combination + :most-specific-last))
   
   (defparameter *cam-gf-04-method1*
     (defmethod cam-gf-04 + ((x integer)) 1))
   
   (defparameter *cam-gf-04-method2*
     (defmethod cam-gf-04 + ((x rational)) 2))
   
   (defparameter *cam-gf-04-method3*
     (defmethod cam-gf-04 + ((x real)) 4))
   
   (defparameter *cam-gf-04-method4*
     (defmethod cam-gf-04 + ((x number)) 8))
   
   (defparameter *cam-gf-04-method5*
     (defmethod cam-gf-04 + ((x t)) 16))
   ))
      
(deftest compute-applicable-methods.7
  (equalt (compute-applicable-methods #'cam-gf-04 (list 0))
	  (list *cam-gf-04-method1* *cam-gf-04-method2* *cam-gf-04-method3*
		*cam-gf-04-method4* *cam-gf-04-method5*))
  t)

;;; Need tests with :around, :before, :after methods

;;; Error tests

(deftest compute-applicable-methods.error.1
  (signals-error (compute-applicable-methods)
		 program-error)
  t)

(deftest compute-applicable-methods.error.2
  (signals-error (compute-applicable-methods #'cam-gf-01)
		 program-error)
  t)

(deftest compute-applicable-methods.error.3
  (signals-error (compute-applicable-methods #'cam-gf-01 '(1 2) nil)
		 program-error)
  t)
