;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  6 14:23:22 2003
;;;; Contains: Tests for PATHNAME-HOST

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

(deftest pathname-host.1
  (loop for p in *pathnames*
	always (eql (length (multiple-value-list (pathname-host p))) 1))
  t)

(deftest pathname-host.2
  (loop for p in *pathnames*
	always (eql (length (multiple-value-list (pathname-host p :case :local))) 1))
  t)

(deftest pathname-host.3
  (loop for p in *pathnames*
	always (eql (length (multiple-value-list (pathname-host p :case :common))) 1))
  t)

(deftest pathname-host.4
  (loop for p in *pathnames*
	always (eql (length (multiple-value-list (pathname-host p :allow-other-keys nil))) 1))
  t)

(deftest pathname-host.5
  (loop for p in *pathnames*
	always (eql (length (multiple-value-list
			     (pathname-host p :foo t :allow-other-keys t))) 1))
  t)

(deftest pathname-host.6
  (loop for p in *pathnames*
	always (eql (length (multiple-value-list
			     (pathname-host p :allow-other-keys t
					    :allow-other-keys nil
					    'foo t))) 1))
  t)

;;; section 19.3.2.1
(deftest pathname-host.7
  (loop for p in *logical-pathnames*
	when (eq (pathname-host p) :unspecific)
	collect p)
  nil)

(deftest pathname-host.8
  (do-special-strings (s "" nil) (pathname-host s))
  nil)

#|
(deftest pathname-host.9
  (loop for p in *pathnames*
	for host = (pathname-host p)
	unless (or (stringp host)
		   (and (listp host) (every #'stringp host))
		   (eql host :unspecific))
	collect (list p host))
  nil)
|#

;;; Error cases

(deftest pathname-host.error.1
  (signals-error (pathname-host) program-error)
  t)

(deftest pathname-host.error.2
  (check-type-error #'pathname-host #'could-be-pathname-designator)
  nil)

(deftest pathname-host.error.3
  (signals-error (pathname-host *default-pathname-defaults* '#:bogus t)
		 program-error)
  t)
