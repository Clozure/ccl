;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 15 12:54:22 2005
;;;; Contains: Tests of INSPECT

(in-package :cl-test)

;;; INSPECT's normal behavior is entirely implementation-dependent,
;;; so it cannot be tested here.  Only test simple error cases.

(deftest inspect.error.1
  (signals-error (inspect) program-error)
  t)

(deftest inspect.error.2
  (signals-error (inspect nil nil) program-error)
  t)
