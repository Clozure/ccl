;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:52:56 2004
;;;; Contains: Tests of PACKAGE-ERROR-PACKAGE

(in-package :cl-test)

(deftest package-error-package.1
  (eqt (find-package (package-error-package
		      (make-condition 'package-error
				      :package "CL")))
       (find-package "CL"))
  t)

(deftest package-error-package.2
  (eqt (find-package (package-error-package
		      (make-condition 'package-error
				      :package (find-package "CL"))))
       (find-package "CL"))
  t)

(deftest package-error-package.3
  (eqt (find-package (package-error-package
		      (make-condition 'package-error
				      :package '#:|CL|)))
       (find-package "CL"))
  t)

(deftest package-error-package.4
  (eqt (find-package (package-error-package
		      (make-condition 'package-error
				      :package #\A)))
       (find-package "A"))
  t)

(deftest package-error-package.error.1
  (signals-error (package-error-package) program-error)
  t)

(deftest package-error-package.error.2
  (signals-error
   (package-error-package
    (make-condition 'package-error :package #\A)
    nil)
   program-error)
  t)

