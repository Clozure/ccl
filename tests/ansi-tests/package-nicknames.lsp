;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 07:51:26 1998
;;;; Contains: Tests of PACKAGE-NICKNAMES

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-nicknames

(deftest package-nicknames.1
  (progn
    (set-up-packages)
    (package-nicknames "A"))
  ("Q"))

(deftest package-nicknames.2
  (progn
    (set-up-packages)
    (package-nicknames #\A))
  ("Q"))

(deftest package-nicknames.3
  (progn
    (set-up-packages)
    (package-nicknames ':|A|))
  ("Q"))

(deftest package-nicknames.4
  (progn
    (set-up-packages)
    (package-nicknames "B"))
  nil)

(deftest package-nicknames.5
  (progn
    (set-up-packages)
    (package-nicknames #\B))
  nil)

(deftest package-nicknames.6
  (progn
    (set-up-packages)
    (package-nicknames '#:|B|))
  nil)

(deftest package-nicknames.7
  (subsetp '(#.(string '#:cl))
	   (package-nicknames "COMMON-LISP")
	   :test #'string=)
  t)

(deftest package-nicknames.8
  (notnot
   (subsetp '(#.(string '#:cl-user))
	    (package-nicknames "COMMON-LISP-USER")
	    :test #'string=))
  t)

(deftest package-nicknames.9
  (signals-error (package-nicknames 10) type-error)
  t)

(deftest package-nicknames.9a
  (signals-error (locally (package-nicknames 10) t) type-error)
  t)

(deftest package-nicknames.10
  (progn
    (set-up-packages)
    (package-nicknames (find-package "A")))
  ("Q"))

(deftest package-nicknames.11
  (handler-case
   (locally (declare (optimize safety))
	    (eval '(package-nicknames "NOT-A-PACKAGE-NAME"))
	    nil)
   (type-error () t)
   (package-error () t))
  t)

;; (find-package n) == p for each n in (package-nicknames p),
;; for any package p
(deftest package-nicknames.12
  (loop
   for p in (list-all-packages) sum
   (loop
    for nk in (package-nicknames p) count
	 (not
	  (and (stringp nk)
	       (eqt p (find-package nk))))))
  0)

;;; Specialized sequence names tests

(defmacro def-package-nicknames-test (test-name name-form)
  `(deftest ,test-name
     (let ((name ,name-form))
       (safely-delete-package name)
       (let ((p (make-package name :use nil)))
	  (package-nicknames p)))
     nil))

(def-package-nicknames-test package-nicknames.16
  (make-array 5 :element-type 'base-char :initial-contents "TEST1"))

(def-package-nicknames-test package-nicknames.17
  (make-array 10 :element-type 'base-char
	      :fill-pointer 5
	      :initial-contents "TEST1?????"))

(def-package-nicknames-test package-nicknames.18
  (make-array 10 :element-type 'character
	      :fill-pointer 5
	      :initial-contents "TEST1?????"))

(def-package-nicknames-test package-nicknames.19
  (make-array 5 :element-type 'base-char :adjustable t
	      :initial-contents "TEST1"))

(def-package-nicknames-test package-nicknames.20
  (make-array 5 :element-type 'character :adjustable t
	      :initial-contents "TEST1"))

(def-package-nicknames-test package-nicknames.21
  (let* ((etype 'base-char)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "XXTEST1XXX")))
    (make-array 5 :element-type etype :displaced-to name0
		:displaced-index-offset 2)))

(def-package-nicknames-test package-nicknames.22
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "XXTEST1XXX")))
    (make-array 5 :element-type etype :displaced-to name0
		:displaced-index-offset 2)))

;;; Error tests

(deftest package-nicknames.error.1
  (signals-error (package-nicknames) program-error)
  t)

(deftest package-nicknames.error.2
  (signals-error (package-nicknames "CL" nil) program-error)
  t)

(deftest package-nicknames.error.3
  (check-type-error #'package-nicknames #'package-designator-p)
  nil)

