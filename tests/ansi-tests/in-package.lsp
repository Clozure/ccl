;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:06:03 1998
;;;; Contains: Tests of IN-PACKAGE

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; in-package

(deftest in-package.1
  (let ((*package* *package*))
    (safely-delete-package "H")
    (make-package "H" :use ())
    (let ((p2 (in-package "H")))
      (and (eqt p2 (find-package "H"))
	   (eqt *package* p2))))
  t)

(deftest in-package.2
  (let ((*package* *package*))
    (safely-delete-package "H")
    (make-package "H" :use ())
    (let ((p2 (in-package |H|)))
      (and (eqt p2 (find-package "H"))
	   (eqt *package* p2))))
  t)

(deftest in-package.3
  (let ((*package* *package*))
    (safely-delete-package "H")
    (make-package "H" :use ())
    (let ((p2 (in-package :|H|)))
      (and (eqt p2 (find-package "H"))
	   (eqt *package* p2))))
  t)

(deftest in-package.4
  (let ((*package* *package*))
    (safely-delete-package "H")
    (make-package "H" :use ())
    (let ((p2 (in-package #\H)))
      (and (eqt p2 (find-package "H"))
	   (eqt *package* p2))))
  t)

(deftest in-package.5
  (let ((*package* *package*))
    (safely-delete-package "H")
    (handler-case
     (eval '(in-package "H"))
     (package-error () 'package-error)
     (error (c) c)))
  package-error)

(def-macro-test in-package.error.1
  (in-package :cl-test))

(defmacro def-in-package-test (test-name name-form)
  `(deftest ,test-name
     (let ((name ,name-form))
       (safely-delete-package name)
       (prog1
	   (let* ((p (make-package name :use nil))
		  (*package* *package*)
		  (p2 (eval `(in-package ,name))))
	     (list (eqt p p2)
		   (eqt p *package*)))
	 (safely-delete-package name)))
     (t t)))

(def-in-package-test in-package.7
  (make-array 5 :initial-contents "TEST1" :element-type 'base-char))

(def-in-package-test in-package.8
  (make-array 10 :initial-contents "TEST1ABCDE"
	      :fill-pointer 5 :element-type 'base-char))

(def-in-package-test in-package.9
  (make-array 10 :initial-contents "TEST1ABCDE"
	      :fill-pointer 5 :element-type 'character))

(def-in-package-test in-package.10
  (make-array 5 :initial-contents "TEST1"
	      :adjustable t :element-type 'base-char))

(def-in-package-test in-package.11
  (make-array 5 :initial-contents "TEST1"
	      :adjustable t :element-type 'character))

(def-in-package-test in-package.12
  (let* ((etype 'base-char)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "xxxxxTEST1")))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 5)))

(def-in-package-test in-package.13
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "xxxxxTEST1")))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 5)))