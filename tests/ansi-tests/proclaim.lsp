;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 07:33:53 2005
;;;; Contains: Tests of PROCLAIM

(in-package :cl-test)

(deftest proclaim.1
  (let ((sym (gensym)))
    (proclaim `(special ,sym))
    (eval `(flet ((%f () ,sym))
	     (let ((,sym :good)) (%f)))))
  :good)

(deftest proclaim.2
  (let ((sym (gensym)))
    (proclaim `(declaration ,sym))
    (proclaim `(,sym))
    nil)
  nil)

(deftest proclaim.3
  (let ((i 0))
    (proclaim (progn (incf i) '(optimize)))
    i)
  1)

;;; Error cases

(deftest proclaim.error.1
  (signals-error (proclaim) program-error)
  t)

(deftest proclaim.error.2
  (signals-error (proclaim '(optimize) nil) program-error)
  t)

(deftest proclaim.error.3
  (signals-error (proclaim `(optimize . foo)) error)
  t)

(deftest proclaim.error.4
  (signals-error (proclaim `(inline . foo)) error)
  t)

(deftest proclaim.error.5
  (signals-error (proclaim `(notinline . foo)) error)
  t)

(deftest proclaim.error.6
  (signals-error (proclaim `(type . foo)) error)
  t)

(deftest proclaim.error.7
  (signals-error (proclaim `(ftype . foo)) type-error)
  t)

(deftest proclaim.error.8
  (signals-error (proclaim '(type integer . foo)) error)
  t)

(deftest proclaim.error.9
  (signals-error (proclaim '(integer . foo)) error)
  t)

(deftest proclaim.error.10
  (signals-error (proclaim '(declaration . foo)) error)
  t)

(deftest proclaim.error.11
  (signals-error (proclaim '(ftype (function (t) t) . foo)) error)
  t)
