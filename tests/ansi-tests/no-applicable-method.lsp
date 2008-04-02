;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 11 13:46:44 2003
;;;; Contains: Tests of NO-APPLICABLE-METHOD

(in-package :cl-test)

(defgeneric no-app-meth-gf-01 (x))

(deftest no-applicable-method.1
  (handler-case
   (progn (no-app-meth-gf-01 'x) :bad)
   (error () :good))
  :good)

;;; I can't conformantly define useful methods for no-applicable-method
;;; without defining new generic function classes, and there's
;;; no standard way to do that.  Grrr.