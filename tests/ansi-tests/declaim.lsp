;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 07:44:07 2005
;;;; Contains: Tests of DECLAIM

(in-package :cl-test)

(deftest declaim.1
  (progn (declaim) nil)
  nil)

(deftest declaim.2
  (progn (eval `(declaim (optimize))) nil)
  nil)

(deftest declaim.3
  (progn (eval `(declaim (inline))) nil)
  nil)

(deftest declaim.4
  (progn (eval `(declaim (notinline))) nil)
  nil)

(deftest declaim.5
  (progn (eval `(declaim (type t))) nil)
  nil)

(deftest declaim.6
  (progn (eval `(declaim (special))) nil)
  nil)

(deftest declaim.7
  (progn (eval `(declaim (integer))) nil)
  nil)

(deftest declaim.8
  (progn (eval `(declaim (declaration))) nil)
  nil)

(deftest declaim.9
  (progn (eval `(declaim (ftype (function (t) t)))) nil)
  nil)

(deftest declaim.10
  (let ((sym (gensym)))
    (eval `(declaim (declaration ,sym)))
    (eval `(declaim (,sym)))
    nil)
  nil)

(deftest declaim.11
  (let ((sym (gensym)))
    (eval `(declaim (optimize) (special ,sym) (inline) (special)))
    (eval `(flet ((%f () ,sym))
	     (let ((,sym :good)) (%f)))))
  :good)
