;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 21 17:48:05 2004
;;;; Contains: Tests of PACKAGE-NAME

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-name

(deftest package-name.1
  (progn
    (set-up-packages)
    (package-name "A"))
  "A")

(deftest package-name.2
  (progn
    (set-up-packages)
    (package-name #\A))
  "A")

(deftest package-name.3
  (progn
    (set-up-packages)
    (package-name "Q"))
  "A")

(deftest package-name.4
  (progn
    (set-up-packages)
    (package-name #\Q))
  "A")

(deftest package-name.5
  (handler-case
   (locally (declare (optimize safety))
	    (eval '(package-name "NOT-THERE"))
	    nil)
   (type-error () t)
   (package-error () t))
  t)

(deftest package-name.6
  (handler-case
   (locally (declare (optimize safety))
	    (eval '(package-name #\*))
	    nil)
   (type-error () t)
   (package-error () t))
  t)

(deftest package-name.6a
  (handler-case
   (locally (declare (optimize safety))
	    (eval '(locally (package-name #\*) t))
	    nil)
   (type-error () t)
   (package-error () t))
  t)

(deftest package-name.7
  (package-name "CL")
  #.(string '#:common-lisp))

(deftest package-name.8
  (package-name "COMMON-LISP")
  #.(string '#:common-lisp))

(deftest package-name.9
  (package-name "COMMON-LISP-USER")
  #.(string '#:common-lisp-user))

(deftest package-name.10
  (package-name "CL-USER")
  #.(string '#:common-lisp-user))

(deftest package-name.11
  (package-name "KEYWORD")
  #.(string '#:keyword))

(deftest package-name.12
  (package-name (find-package "CL"))
  #.(string '#:common-lisp))

(deftest package-name.13
  (let* ((p (make-package "TEMP1"))
	 (pname1 (package-name p)))
    (rename-package "TEMP1" "TEMP2")
    (let ((pname2 (package-name p)))
      (safely-delete-package p)
      (list pname1 pname2 (package-name p))))
  ("TEMP1" "TEMP2" nil))

;; (find-package (package-name p)) == p for any package p
(deftest package-name.14
  (loop
   for p in (list-all-packages) count
   (not
    (let ((name (package-name p)))
      (and (stringp name)
	   (eqt (find-package name) p)))))
  0)

;; package-name applied to a package's name
;; should return an equal string
(deftest package-name.15
  (loop
   for p in (list-all-packages) count
   (not (equal (package-name p)
	       (package-name (package-name p)))))
  0)

;;; Specialized sequence tests

(defmacro def-package-name-test (test-name name-form expected-name-form)
  `(deftest ,test-name
     (let ((name ,name-form)
	   (expected-name ,expected-name-form))
       (assert (string= name expected-name))
       (safely-delete-package name)
       (let ((p (make-package name :use nil)))
	 (equalt (package-name p) expected-name)))
     t))

(def-package-name-test package-name.16
  (make-array 5 :element-type 'base-char :initial-contents "TEST1")
  "TEST1")

(def-package-name-test package-name.17
  (make-array 10 :element-type 'base-char
	      :fill-pointer 5
	      :initial-contents "TEST1?????")
  "TEST1")

(def-package-name-test package-name.18
  (make-array 10 :element-type 'character
	      :fill-pointer 5
	      :initial-contents "TEST1?????")
  "TEST1")

(def-package-name-test package-name.19
  (make-array 5 :element-type 'base-char :adjustable t
	      :initial-contents "TEST1")
  "TEST1")

(def-package-name-test package-name.20
  (make-array 5 :element-type 'character :adjustable t
	      :initial-contents "TEST1")
  "TEST1")

(def-package-name-test package-name.21
  (let* ((etype 'base-char)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "XXTEST1XXX")))
    (make-array 5 :element-type etype :displaced-to name0
		:displaced-index-offset 2))
  "TEST1")

(def-package-name-test package-name.22
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "XXTEST1XXX")))
    (make-array 5 :element-type etype :displaced-to name0
		:displaced-index-offset 2))
  "TEST1")


(deftest package-name.error.1
  (signals-error (package-name) program-error)
  t)

(deftest package-name.error.2
  (signals-error (package-name "CL" nil) program-error)
  t)

(deftest package-name.error.3
  (check-type-error #'package-name #'package-designator-p)
  nil)
