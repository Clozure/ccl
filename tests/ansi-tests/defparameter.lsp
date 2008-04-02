;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 10 23:13:22 2002
;;;; Contains: Tests of DEFPARAMETER

(in-package :cl-test)

(defparameter *defparameter-test-var-1* 100)

(deftest defparameter.1
  *defparameter-test-var-1*
  100)

(deftest defparameter.2
  (documentation '*defparameter-test-var-1* 'variable)
  nil)

;;; Show that it's declared special.
(deftest defparameter.3
  (flet ((%f () *defparameter-test-var-1*))
    (let ((*defparameter-test-var-1* 29))
      (%f)))
  29)

(deftest defparameter.4
  (values
   (makunbound '*defparameter-test-var-2*)
   (defparameter *defparameter-test-var-2* 200 "Whatever.")
   (documentation '*defparameter-test-var-2* 'variable)
   *defparameter-test-var-2*)
  *defparameter-test-var-2*
  *defparameter-test-var-2*
  "Whatever."
  200)

(deftest defparameter.5
  (values
   (makunbound '*defparameter-test-var-2*)
   (defparameter *defparameter-test-var-2* 200 "Whatever.")
   (documentation '*defparameter-test-var-2* 'variable)
   *defparameter-test-var-2*
   (defparameter *defparameter-test-var-2* 300 "And ever.")
   (documentation '*defparameter-test-var-2* 'variable)
   *defparameter-test-var-2*
   )
  *defparameter-test-var-2*
  *defparameter-test-var-2*
  "Whatever."
  200
  *defparameter-test-var-2*
  "And ever."
  300)

;;; (deftest defparameter.error.1
;;;   (signals-error (defparameter)  program-error)
;;;   t)
;;; 
;;; (deftest defparameter.error.2
;;;   (signals-error (defparameter *ignored-defparameter-name*)
;;;                  program-error)
;;;   t)
;;; 
;;; (deftest defparameter.error.3
;;;   (signals-error (defparameter *ignored-defparameter-name* nil
;;; 		    "documentation"
;;; 		    "illegal extra argument")
;;;                  program-error)
;;;   t)

(deftest defparameter.error.1
  (signals-error (funcall (macro-function 'defparameter))
		 program-error)
  t)

(deftest defparameter.error.2
  (signals-error (funcall (macro-function 'defparameter)
			   '(defparameter *nonexistent-variable* nil))
		 program-error)
  t)

(deftest defparameter.error.3
  (signals-error (funcall (macro-function 'defparameter)
			   '(defparameter *nonexistent-variable* nil)
			   nil nil)
		 program-error)
  t)
