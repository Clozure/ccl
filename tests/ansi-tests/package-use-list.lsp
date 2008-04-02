;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:55:56 2004
;;;; Contains: Tests of PACKAGE-USE-LIST

(in-package :cl-test)

;;; Most tests of this function are in files for other package-related operators

;;; Specialized sequence tests

(defmacro def-package-use-list-test (test-name name-form)
  `(deftest ,test-name
     (let ((name ,name-form))
       (safely-delete-package name)
       (let ((p (make-package name :use nil)))
	  (package-use-list p)))
     nil))

(def-package-use-list-test package-use-list.1
  (make-array 5 :element-type 'base-char :initial-contents "TEST1"))

(def-package-use-list-test package-use-list.2
  (make-array 10 :element-type 'base-char
	      :fill-pointer 5
	      :initial-contents "TEST1?????"))

(def-package-use-list-test package-use-list.3
  (make-array 10 :element-type 'character
	      :fill-pointer 5
	      :initial-contents "TEST1?????"))

(def-package-use-list-test package-use-list.4
  (make-array 5 :element-type 'base-char :adjustable t
	      :initial-contents "TEST1"))

(def-package-use-list-test package-use-list.5
  (make-array 5 :element-type 'character :adjustable t
	      :initial-contents "TEST1"))

(def-package-use-list-test package-use-list.6
  (let* ((etype 'base-char)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "XXTEST1XXX")))
    (make-array 5 :element-type etype :displaced-to name0
		:displaced-index-offset 2)))

(def-package-use-list-test package-use-list.7
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "XXTEST1XXX")))
    (make-array 5 :element-type etype :displaced-to name0
		:displaced-index-offset 2)))

;;; Error tests

(deftest package-use-list.error.1
  (signals-error (package-use-list) program-error)
  t)

(deftest package-use-list.error.2
  (signals-error (package-use-list "CL" nil) program-error)
  t)
