;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed May 28 14:02:42 2003
;;;; Contains: Class definitions for defgeneric-method-combination-*.lsp

(in-package :cl-test)

(defclass dgmc-class-01 () ())
(defclass dgmc-class-02 (dgmc-class-01) ())
(defclass dgmc-class-03 (dgmc-class-01) ())
(defclass dgmc-class-04 (dgmc-class-02 dgmc-class-03) ())
(defclass dgmc-class-05 (dgmc-class-04) ())
(defclass dgmc-class-06 (dgmc-class-04) ())
(defclass dgmc-class-07 (dgmc-class-05 dgmc-class-06) ())
