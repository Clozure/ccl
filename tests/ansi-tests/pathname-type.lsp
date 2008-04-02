;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  6 14:45:16 2003
;;;; Contains: Tests for PATHNAME-TYPE

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

(deftest pathname-type.1
  (loop for p in *pathnames*
	for type = (pathname-type p)
	unless (or (stringp type)
		   (member type '(nil :wild :unspecific)))
	collect (list p type))
  nil)

(deftest pathname-type.2
  (loop for p in *pathnames*
	for type = (pathname-type p :case :local)
	unless (or (stringp type)
		   (member type '(nil :wild :unspecific)))
	collect (list p type))
  nil)

(deftest pathname-type.3
  (loop for p in *pathnames*
	for type = (pathname-type p :case :common)
	unless (or (stringp type)
		   (member type '(nil :wild :unspecific)))
	collect (list p type))
  nil)

(deftest pathname-type.4
  (loop for p in *pathnames*
	for type = (pathname-type p :allow-other-keys nil)
	unless (or (stringp type)
		   (member type '(nil :wild :unspecific)))
	collect (list p type))
  nil)

(deftest pathname-type.5
  (loop for p in *pathnames*
	for type = (pathname-type p :foo 'bar :allow-other-keys t)
	unless (or (stringp type)
		   (member type '(nil :wild :unspecific)))
	collect (list p type))
  nil)

(deftest pathname-type.6
  (loop for p in *pathnames*
	for type = (pathname-type p :allow-other-keys t :allow-other-keys nil :foo 'bar)
	unless (or (stringp type)
		   (member type '(nil :wild :unspecific)))
	collect (list p type))
  nil)

;;; section 19.3.2.1
(deftest pathname-type.7
  (loop for p in *logical-pathnames*
	when (eq (pathname-type p) :unspecific)
	collect p)
  nil)

(deftest pathname-type.8
  (do-special-strings (s "" nil) (pathname-type s))
  nil)

(deftest pathname-type.error.1
  (signals-error (pathname-type) program-error)
  t)

(deftest pathname-type.error.2
  (check-type-error #'pathname-type #'could-be-pathname-designator)
  nil)
