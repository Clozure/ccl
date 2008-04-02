;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:51:38 2004
;;;; Contains: Tests of PACKAGEP

(in-package :cl-test)

(deftest packagep.1
  (check-type-predicate #'packagep 'package)
  nil)

;;; *package* is always a package

(deftest packagep.2
  (not-mv (packagep *package*))
  nil)

(deftest packagep.error.1
  (signals-error (packagep) program-error)
  t)

(deftest packagep.error.2
  (signals-error (packagep nil nil) program-error)
  t)
