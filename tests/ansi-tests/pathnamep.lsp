;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  6 10:26:45 2003
;;;; Contains: Tests of PATHNAMEP

(in-package :cl-test)

(deftest pathnamep.1
  (check-type-predicate #'pathnamep 'pathname)
  nil)

(deftest pathnamep.2
  (check-predicate #'(lambda (x) (eql (length (multiple-value-list (pathnamep x))) 1)))
  nil)

(deftest pathnamep.3
  (check-predicate (typef '(not logical-pathname)) #'pathnamep)
  nil)

(deftest pathnamep.error.1
  (signals-error (pathnamep) program-error)
  t)

(deftest pathnamep.error.2
  (signals-error (pathnamep nil nil) program-error)
  t)

(deftest pathnamep.error.3
  (signals-error (pathnamep *default-pathname-defaults* nil)
		 program-error)
  t)
