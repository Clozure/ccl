;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jun  6 16:46:31 2003
;;;; Contains: Definition of the RCTEST package

(defpackage :rctest
  (:use :cl :cl-test)
  (:import-from "COMMON-LISP-USER" #:compile-and-load)
  (:export
   #:generate
   ))

;; (in-package :rctest)
