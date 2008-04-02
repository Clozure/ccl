;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  6 14:45:16 2003
;;;; Contains: Tests for PATHNAME-VERSION

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

(deftest pathname-version.1
  (loop for p in *pathnames*
	for version = (pathname-version p)
	unless (or (integerp version) (symbolp version))
	collect (list p version))
  nil)

;;; section 19.3.2.1
(deftest pathname-version.2
  (loop for p in *logical-pathnames*
	when (eq (pathname-version p) :unspecific)
	collect p)
  nil)

(deftest pathname-version.3
  (do-special-strings (s "" nil) (pathname-version s))
  nil)

(deftest pathname-version.error.1
  (signals-error (pathname-version) program-error)
  t)

(deftest pathname-version.error.2
  (signals-error (pathname-version *default-pathname-defaults* nil)
		 program-error)
  t)

(deftest pathname-version.error.3
  (check-type-error #'pathname-version #'could-be-pathname-designator)
  nil)

