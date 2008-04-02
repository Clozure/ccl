;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 10 23:05:39 2002
;;;; Contains: Tests of DEFCONSTANT

(in-package :cl-test)

(defconstant test-constant-1  17)

(deftest defconstant.1
  (symbol-value 'test-constant-1)
  17)

(deftest defconstant.2
  (notnot-mv (constantp 'test-constant-1))
  t)

(deftest defconstant.3
  (documentation 'test-constant-1 'variable)
  nil)

(defconstant test-constant-2 'a
  "This is the documentation.")

(deftest defconstant.4
  (documentation 'test-constant-2 'variable)
  "This is the documentation.")

(deftest defconstant.5
  (defconstant test-constant-3 0)
  test-constant-3)

;;; (deftest defconstant.error.1
;;;   (signals-error (defconstant)  program-error)
;;;   t)
;;; 
;;; (deftest defconstant.error.2
;;;   (signals-error (defconstant +ignorable-constant-name+) program-error)
;;;   t)
;;; 
;;; (deftest defconstant.error.3
;;;   (signals-error (defconstant +ignorable-constant-name2+ nil
;;; 		    "This is a docstring"
;;; 		    "This is an unnecessary extra argument.")
;;;                  program-error)
;;;  t)

(deftest defconstant.error.1
  (signals-error (funcall (macro-function 'defconstant))
		 program-error)
  t)

(deftest defconstant.error.2
  (signals-error (funcall (macro-function 'defconstant)
			   '(defconstant +nonexistent-constant+ 0))
		 program-error)
  t)

(deftest defconstant.error.3
  (signals-error (funcall (macro-function 'defconstant)
			   '(defconstant +nonexistent-constant+ 0)
			   nil nil)
		 program-error)
  t)
