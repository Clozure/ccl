;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 15 12:05:47 2003
;;;; Contains: Tests of CLASS-NAME

(in-package :cl-test)

;;; This is mostly tested elsewhere.

(deftest class-name.1
  (class-name (find-class 'symbol))
  symbol)

(defclass class-name-class-01 () (a b c))

(report-and-ignore-errors
 (eval '(defmethod class-name ((x class-name-class-01)) 'silly)))

(deftest class-name.2
  (class-name (make-instance 'class-name-class-01))
  silly)

;; Tests of (setf class-name)

(deftest setf-class-name.1
  (typep* #'(setf class-name) 'standard-generic-function)
  t)

(deftest setf-class-name.2
  (let ((sym (gensym))
	(newsym (gensym)))
    (eval `(defclass ,sym () (a b c)))
    (let ((class (find-class sym)))
      (values
       (eqlt (class-name class) sym)
       (equalt
	(multiple-value-list (setf (class-name (find-class sym)) newsym))
	(list newsym))
       (eqlt newsym (class-name class)))))
  t t t)
     

;;; Error tests

(deftest class-name.error.1
  (signals-error (class-name) program-error)
  t)

(deftest class-name.error.2
  (signals-error (class-name (find-class 'symbol) nil)
		 program-error)
  t)
