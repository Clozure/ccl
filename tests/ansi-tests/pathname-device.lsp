;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  6 14:23:54 2003
;;;; Contains: Tests for PATHNAME-DEVICE

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

(deftest pathname-device.1
  (loop for p in *pathnames*
	for device = (pathname-device p)
	unless (or (stringp device)
		   (member device '(nil :wild :unspecific)))
	collect (list p device))
  nil)

(deftest pathname-device.2
  (loop for p in *pathnames*
	for device = (pathname-device p :case :local)
	unless (or (stringp device)
		   (member device '(nil :wild :unspecific)))
	collect (list p device))
  nil)

(deftest pathname-device.3
  (loop for p in *pathnames*
	for device = (pathname-device p :case :common)
	unless (or (stringp device)
		   (member device '(nil :wild :unspecific)))
	collect (list p device))
  nil)

(deftest pathname-device.4
  (loop for p in *pathnames*
	for device = (pathname-device p :allow-other-keys nil)
	unless (or (stringp device)
		   (member device '(nil :wild :unspecific)))
	collect (list p device))
  nil)

(deftest pathname-device.5
  (loop for p in *pathnames*
	for device = (pathname-device p :foo 'bar :allow-other-keys t)
	unless (or (stringp device)
		   (member device '(nil :wild :unspecific)))
	collect (list p device))
  nil)

(deftest pathname-device.6
  (loop for p in *pathnames*
	for device = (pathname-device p :allow-other-keys t :allow-other-keys nil :foo 'bar)
	unless (or (stringp device)
		   (member device '(nil :wild :unspecific)))
	collect (list p device))
  nil)

;;; section 19.3.2.1
(deftest pathname-device.7
  (loop for p in *logical-pathnames*
	always (eq (pathname-device p) :unspecific))
  t)

(deftest pathname-device.8
  (do-special-strings (s "" nil) (pathname-device s))
  nil)

(deftest pathname-device.error.1
  (signals-error (pathname-device) program-error)
  t)

(deftest pathname-device.error.2
  (check-type-error #'pathname-device #'could-be-pathname-designator)
  nil)