;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:56:28 2004
;;;; Contains: Tests of PACKAGE-USED-BY-LIST

(in-package :cl-test)

;;; Most tests of this function are in files for other package-related operators

;;; Specialized sequence tests

(defmacro def-package-used-by-list-test (test-name name-form)
  `(deftest ,test-name
     (let ((name ,name-form))
       (safely-delete-package name)
       (let ((p (make-package name :use nil)))
	  (package-used-by-list p)))
     nil))

(def-package-used-by-list-test package-used-by-list.1
  (make-array 5 :element-type 'base-char :initial-contents "TEST1"))

(def-package-used-by-list-test package-used-by-list.2
  (make-array 10 :element-type 'base-char
	      :fill-pointer 5
	      :initial-contents "TEST1?????"))

(def-package-used-by-list-test package-used-by-list.3
  (make-array 10 :element-type 'character
	      :fill-pointer 5
	      :initial-contents "TEST1?????"))

(def-package-used-by-list-test package-used-by-list.4
  (make-array 5 :element-type 'base-char :adjustable t
	      :initial-contents "TEST1"))

(def-package-used-by-list-test package-used-by-list.5
  (make-array 5 :element-type 'character :adjustable t
	      :initial-contents "TEST1"))

(def-package-used-by-list-test package-used-by-list.6
  (let* ((etype 'base-char)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "XXTEST1XXX")))
    (make-array 5 :element-type etype :displaced-to name0
		:displaced-index-offset 2)))

(def-package-used-by-list-test package-used-by-list.7
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "XXTEST1XXX")))
    (make-array 5 :element-type etype :displaced-to name0
		:displaced-index-offset 2)))

;;; Error tests

(deftest package-used-by-list.error.1
  (signals-error (package-used-by-list) program-error)
  t)

(deftest package-used-by-list.error.2
  (signals-error (package-used-by-list "CL" nil) program-error)
  t)

