;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:52:21 2004
;;;; Contains: Tests of the condition PACKAGE-ERROR

(in-package :cl-test)

(deftest package-error.1
  (not
   (typep (make-condition 'package-error :package "CL")
	  'package-error))
  nil)

(deftest package-error.2
  (not
   (typep (make-condition 'package-error
			  :package (find-package "CL"))
	  'package-error))
  nil)

(deftest package-error.3
  (subtypep* 'package-error 'error)
  t t)

(deftest package-error.4
   (not
    (typep (make-condition 'package-error
			   :package (find-package '#:|CL|))
	   'package-error))
  nil)

