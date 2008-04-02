;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct 21 22:21:48 2002
;;;; Contains: Overall tests for section 5 of spec, "Data and Control Flow"

(in-package :cl-test)

;;; Functions from section 5
(defparameter *dcf-fns*
  '(apply fboundp fmakunbound funcall function-lambda-expression
	  functionp compiled-function-p not eq eql equal equalp identity
	  complement constantly every some notevery notany
	  values-list get-setf-expansion))

;;; Macros from section 5
(defparameter *dcf-macros*
  '(defun defconstant defparameter defvar destructuring-bind
     psetq return and cond or when unless case ccase ecase
     multiple-value-list multiple-value-setq nth-value
     prog prog* prog1 prog2 define-modify-macro defsetf
     define-setf-expander setf psetf shiftf rotatef))

(deftest dcf-funs
  (remove-if #'fboundp *dcf-fns*)
  nil)

(deftest dcf-macros
  (remove-if #'macro-function *dcf-macros*)
  nil)



  
  