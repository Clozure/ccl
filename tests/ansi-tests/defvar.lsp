;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 10 23:21:50 2002
;;;; Contains: Tests for DEFVAR

(in-package :cl-test)

(defvar *defvar-test-var-1* 100)

(deftest defvar.1
  *defvar-test-var-1*
  100)

(deftest defvar.2
  (documentation '*defvar-test-var-1* 'variable)
  nil)

;;; Show that it's declared special.
(deftest defvar.3
  (flet ((%f () *defvar-test-var-1*))
    (let ((*defvar-test-var-1* 29))
      (%f)))
  29)

(deftest defvar.4
  (values
   (makunbound '*defvar-test-var-2*)
   (defvar *defvar-test-var-2* 200 "Whatever.")
   (documentation '*defvar-test-var-2* 'variable)
   *defvar-test-var-2*)
  *defvar-test-var-2*
  *defvar-test-var-2*
  "Whatever."
  200)

(deftest defvar.5
  (let ((x 0))
    (values
     (makunbound '*defvar-test-var-2*)
     (defvar *defvar-test-var-2* 200 "Whatever.")
     (documentation '*defvar-test-var-2* 'variable)
     *defvar-test-var-2*
     (defvar *defvar-test-var-2* (incf x) "And ever.")
     (documentation '*defvar-test-var-2* 'variable)
     *defvar-test-var-2*
     x
     ))
  *defvar-test-var-2*
  *defvar-test-var-2*
  "Whatever."
  200
  *defvar-test-var-2*
  "And ever."
  200
  0)

;;; (deftest defvar.error.1
;;;   (signals-error (defvar) program-error)
;;;   t)
;;; 
;;; (deftest defvar.error.2
;;;   (signals-error (defvar *ignored-defvar-name* nil "documentation"
;;; 		    "illegal extra argument")
;;;                  program-error)
;;;   t)

(deftest defvar.error.1
  (signals-error (funcall (macro-function 'defvar))
		 program-error)
  t)

(deftest defvar.error.2
  (signals-error (funcall (macro-function 'defvar)
			   '(defvar *nonexistent-variable* nil))
		 program-error)
  t)

(deftest defvar.error.3
  (signals-error (funcall (macro-function 'defvar)
			   '(defvar *nonexistent-variable* nil)
			   nil nil)
		 program-error)
  t)
