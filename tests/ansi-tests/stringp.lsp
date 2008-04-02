;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 29 17:32:20 2004
;;;; Contains: Tests of STRINGP

(in-package :cl-test)

(deftest stringp.1
  (check-type-predicate #'stringp 'string)
  nil)

(deftest stringp.2
  (notnot (stringp "abcd"))
  t)

(deftest stringp.3
  (notnot (stringp (make-array 4 :element-type 'character
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.4
  (notnot (stringp (make-array 4 :element-type 'base-char
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.5
  (notnot (stringp (make-array 4 :element-type 'standard-char
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.6
  (stringp 0)
  nil)

(deftest stringp.7
  (stringp #\a)
  nil)

(deftest stringp.8
  (let* ((s (make-array 10 :element-type 'character
			:initial-element #\a))
	 (s2 (make-array 4 :element-type 'character
			 :displaced-to s
			 :displaced-index-offset 2)))
    (notnot (stringp s2)))
  t)

(deftest stringp.9
  :notes (:nil-vectors-are-strings)
  (notnot-mv (stringp (make-array '(0) :element-type nil)))
  t)

(deftest stringp.10
  :notes (:nil-vectors-are-strings)
  (notnot-mv (stringp (make-array '(37) :element-type nil)))
  t)

(deftest stringp.11
  (notnot (stringp (make-array 4 :element-type 'base-char
			       :fill-pointer 2
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.12
  (notnot (stringp (make-array 4 :element-type 'base-char
			       :adjustable t
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.13
  (notnot (stringp (make-array 4 :element-type 'character
			       :fill-pointer 2
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.14
  (notnot (stringp (make-array 4 :element-type 'character
			       :adjustable t
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.15
  (let ((i 0))
    (values
     (notnot (stringp (progn (incf i) "")))
     i))
  t 1)

;;; Error tests

(deftest stringp.error.1
  (signals-error (stringp) program-error)
  t)

(deftest stringp.error.2
  (signals-error (stringp "" nil) program-error)
  t)
