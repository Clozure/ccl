;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  6 14:24:39 2003
;;;; Contains: Tests for PATHNAME-DIRECTORY

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

(deftest pathname-directory.1
  (loop for p in *pathnames*
	for directory = (pathname-directory p)
	unless (or (stringp directory)
		   (member directory '(nil :wild :unspecific))
		   (and (consp directory)
			(member (car directory) '(:absolute :relative))))
	collect (list p directory))
  nil)

(deftest pathname-directory.2
  (loop for p in *pathnames*
	for directory = (pathname-directory p :case :local)
	unless (or (stringp directory)
		   (member directory '(nil :wild :unspecific))
		   (and (consp directory)
			(member (car directory) '(:absolute :relative))))
	collect (list p directory))
  nil)

(deftest pathname-directory.3
  (loop for p in *pathnames*
	for directory = (pathname-directory p :case :common)
	unless (or (stringp directory)
		   (member directory '(nil :wild :unspecific))
		   (and (consp directory)
			(member (car directory) '(:absolute :relative))))
	collect (list p directory))
  nil)

(deftest pathname-directory.4
  (loop for p in *pathnames*
	for directory = (pathname-directory p :allow-other-keys nil)
	unless (or (stringp directory)
		   (member directory '(nil :wild :unspecific))
		   (and (consp directory)
			(member (car directory) '(:absolute :relative))))
	collect (list p directory))
  nil)

(deftest pathname-directory.5
  (loop for p in *pathnames*
	for directory = (pathname-directory p :foo 'bar :allow-other-keys t)
	unless (or (stringp directory)
		   (member directory '(nil :wild :unspecific))
		   (and (consp directory)
			(member (car directory) '(:absolute :relative))))
	collect (list p directory))
  nil)

(deftest pathname-directory.6
  (loop for p in *pathnames*
	for directory = (pathname-directory p :allow-other-keys t
					    :allow-other-keys nil
					    'foo 'bar)
	unless (or (stringp directory)
		   (member directory '(nil :wild :unspecific))
		   (and (consp directory)
			(member (car directory) '(:absolute :relative))))
	collect (list p directory))
  nil)

;;; section 19.3.2.1
(deftest pathname-directory.7
  (loop for p in *logical-pathnames*
	when (eq (pathname-directory p) :unspecific)
	collect p)
  nil)

(deftest pathname-directory.8
  (do-special-strings (s "" nil) (pathname-directory s))
  nil)

(deftest pathname-directory.error.1
  (signals-error (pathname-directory) program-error)
  t)

(deftest pathname-directory.error.2
  (check-type-error #'pathname-directory #'could-be-pathname-designator)
  nil)
