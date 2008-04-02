;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Dec 29 14:45:50 2003
;;;; Contains: Tests for TRANSLATE-LOGICAL-PATHNAME

(in-package :cl-test)

;; On physical pathnames, t-l-p returns the pathname itself

;;; Every physical pathname is converted to itself
(deftest translate-logical-pathname.1
  (loop for p in *pathnames*
	unless (or (typep p 'logical-pathname)
		   (eq p (translate-logical-pathname p)))
	collect p)
  nil)

;;; &key arguments are allowed
(deftest translate-logical-pathname.2
  (loop for p in *pathnames*
	unless (or (typep p 'logical-pathname)
		   (eq p (translate-logical-pathname
			  p :allow-other-keys t)))
	collect p)
  nil)

(deftest translate-logical-pathname.3
  (loop for p in *pathnames*
	unless (or (typep p 'logical-pathname)
		   (eq p (translate-logical-pathname
			  p :allow-other-keys nil)))
	collect p)
  nil)

(deftest translate-logical-pathname.4
  (loop for p in *pathnames*
	unless (or (typep p 'logical-pathname)
		   (eq p (translate-logical-pathname
			  p :foo 1 :allow-other-keys t :bar 2)))
	collect p)
  nil)


;;; errors

(deftest translate-logical-pathname.error.1
  (signals-error (translate-logical-pathname) program-error)
  t)
