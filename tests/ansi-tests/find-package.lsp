;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 07:50:39 1998
;;;; Contains: Tests for FIND-PACKAGE

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-package

(deftest find-package.1
  (let ((p (find-package "CL"))
	(p2 (find-package "COMMON-LISP")))
    (and p p2 (eqt p p2)))
  t)

(deftest find-package.2
  (let ((p (find-package "CL-USER"))
	(p2 (find-package "COMMON-LISP-USER")))
    (and p p2 (eqt p p2)))
  t)

(deftest find-package.3
  (let ((p (find-package "KEYWORD")))
    (and p (eqt p (symbol-package :test))))
  t)

(deftest find-package.4
  (progn
    (set-up-packages)
    (let ((p (ignore-errors (find-package "A"))))
      (if (packagep p)
	  t
	p)))
  t)

(deftest find-package.5
  (progn
    (set-up-packages)
    (let ((p (ignore-errors (find-package #\A))))
      (if (packagep p)
	  t
	p)))
  t)

(deftest find-package.6
  (progn
    (set-up-packages)
    (let ((p (ignore-errors (find-package "B"))))
      (if (packagep p)
	  t
	p)))
  t)

(deftest find-package.7
  (progn
    (set-up-packages)
    (let ((p (ignore-errors (find-package #\B))))
      (if (packagep p)
	  t
	p)))
  t)

(deftest find-package.8
  (progn
    (set-up-packages)
    (let ((p (ignore-errors (find-package "Q")))
	  (p2 (ignore-errors (find-package "A"))))
      (and (packagep p)
	   (packagep p2)
	   (eqt p p2))))
  t)

(deftest find-package.9
  (progn
    (set-up-packages)
    (let ((p (ignore-errors (find-package "A")))
	  (p2 (ignore-errors (find-package "B"))))
      (eqt p p2)))
  nil)

(deftest find-package.10
  (progn
    (set-up-packages)
    (let ((p (ignore-errors (find-package #\Q)))
	  (p2 (ignore-errors (find-package "Q"))))
      (and (packagep p)
	   (eqt p p2))))
  t)

(deftest find-package.11
  (let* ((cl (find-package "CL"))
	 (cl2 (find-package cl)))
    (and (packagep cl)
	 (eqt cl cl2)))
  t)

(deftest find-package.12
  (let* ((name (make-array '(7) :initial-contents "KEYWORD"
			   :element-type 'base-char))
	 (p (find-package name)))
    (and p (eqt p (symbol-package :test))))
  t)

(deftest find-package.13
  (let* ((name (make-array '(10) :initial-contents "KEYWORDXYZ"
			   :fill-pointer 7
			   :element-type 'base-char))
	 (p (find-package name)))
    (and p (eqt p (symbol-package :test))))
  t)

(deftest find-package.14
  (let* ((name (make-array '(10) :initial-contents "KEYWORDXYZ"
			   :fill-pointer 7
			   :element-type 'character))
	 (p (find-package name)))
    (and p (eqt p (symbol-package :test))))
  t)

(deftest find-package.15
  (let* ((name0 (make-array '(10) :initial-contents "XYKEYWORDZ"
			    :element-type 'character))
	 (name (make-array '(7) :displaced-to name0 :displaced-index-offset 2
			   :element-type 'character))
	 (p (find-package name)))
    (and p (eqt p (symbol-package :test))))
  t)

(deftest find-package.16
  (let* ((name (make-array '(7) :initial-contents "KEYWORD"
			   :adjustable t
			   :element-type 'base-char))
	 (p (find-package name)))
    (and p (eqt p (symbol-package :test))))
  t)

(deftest find-package.17
  (let* ((name (make-array '(7) :initial-contents "KEYWORD"
			   :adjustable t
			   :element-type 'character))
	 (p (find-package name)))
    (and p (eqt p (symbol-package :test))))
  t)

;;; Error tests

(deftest find-package.error.1
  (signals-error (find-package) program-error)
  t)

(deftest find-package.error.2
  (signals-error (find-package "CL" nil) program-error)
  t)
