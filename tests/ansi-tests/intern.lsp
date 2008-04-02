;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 07:59:10 1998
;;;; Contains: Tests of INTERN

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; intern

(deftest intern.1
  (progn
    (safely-delete-package "TEMP1")
    (let ((p (make-package "TEMP1" :use nil))
	  (i 0) x y)
      (multiple-value-bind* (sym1 status1)
	  (find-symbol "FOO" p)
	(intern (progn (setf x (incf i)) "FOO")
		(progn (setf y (incf i)) p))
	(multiple-value-bind* (sym2 status2)
	    (find-symbol "FOO" p)
	  (and (eql i 2)
	       (eql x 1)
	       (eql y 2)
	       (null sym1)
	       (null status1)
	       (string= (symbol-name sym2) "FOO")
	       (eqt (symbol-package sym2) p)
	       (eqt status2 :internal)
	       (progn (delete-package p) t))))))
  t)

(deftest intern.2
  (progn
    (safely-delete-package "TEMP1")
    (let ((p (make-package "TEMP1" :use nil)))
      (multiple-value-bind* (sym1 status1)
	  (find-symbol "FOO" "TEMP1")
	(intern "FOO" "TEMP1")
	(multiple-value-bind* (sym2 status2)
	    (find-symbol "FOO" p)
	  (and (null sym1)
	       (null status1)
	       (string= (symbol-name sym2) "FOO")
	       (eqt (symbol-package sym2) p)
	       (eqt status2 :internal)
	       (progn (delete-package p) t))))))
  t)

(deftest intern.3
  :notes (:nil-vectors-are-strings)
  (let ((cl-user-package (find-package "CL-USER")))
    (eqt (intern "" cl-user-package)
	 (intern (make-array 0 :element-type nil) cl-user-package)))
  t)

(deftest intern.4
  (let ((cl-user-package (find-package "CL-USER")))
    (eqt (intern (make-array 5 :element-type 'character
			     :initial-contents "XYZZY") cl-user-package)
	 (intern (make-array 5 :element-type 'base-char
			     :initial-contents "XYZZY") cl-user-package)))
  t)

;;; String is a specialized sequence type

(defmacro def-intern-test (test-name &key (symbol-name "FOO")
				     (package-name "TEMP1"))
  `(deftest ,test-name
     (let ((sname ,symbol-name)
	   (pname ,package-name))
       (safely-delete-package pname)
       (let ((p (make-package pname :use nil)))
	 (multiple-value-bind*
	  (sym1 status1)
	  (find-symbol sname pname)
	  (intern sname pname)
	  (multiple-value-bind*
	   (sym2 status2)
	   (find-symbol sname p)
	   (and (null sym1)
		(null status1)
		(string= (symbol-name sym2) sname)
		(eqt (symbol-package sym2) p)
		(eqt status2 :internal)
		(progn (delete-package p) t))))))
     t))

(def-intern-test intern.5
  :symbol-name (make-array 3 :element-type 'base-char
			   :initial-contents "BAR"))

(def-intern-test intern.6
  :symbol-name (make-array 13 :element-type 'base-char
			   :fill-pointer 3
			   :initial-contents "BAR1234567890"))

(def-intern-test intern.7
  :symbol-name (make-array 13 :element-type 'character
			   :fill-pointer 3
			   :initial-contents "BAR1234567890"))

(def-intern-test intern.8
  :symbol-name (make-array 3 :element-type 'base-char
			   :adjustable t
			   :initial-contents "BAR"))

(def-intern-test intern.9
  :symbol-name (make-array 3 :element-type 'character
			   :adjustable t
			   :initial-contents "BAR"))

(def-intern-test intern.10
  :symbol-name
  (let* ((etype 'base-char)
	 (name0 (make-array 8 :element-type etype
			    :initial-contents "XBARYYYY")))
    (make-array 3 :element-type etype :displaced-to name0
		:displaced-index-offset 1)))

(def-intern-test intern.11
  :symbol-name
  (let* ((etype 'character)
	 (name0 (make-array 8 :element-type etype
			    :initial-contents "XBARYYYY")))
    (make-array 3 :element-type etype :displaced-to name0
		:displaced-index-offset 1)))

(def-intern-test intern.12
  :package-name (make-array 3 :element-type 'base-char
			   :initial-contents "BAR"))

(def-intern-test intern.13
  :package-name (make-array 13 :element-type 'base-char
			   :fill-pointer 3
			   :initial-contents "BAR1234567890"))

(def-intern-test intern.14
  :package-name (make-array 13 :element-type 'character
			   :fill-pointer 3
			   :initial-contents "BAR1234567890"))

(def-intern-test intern.15
  :package-name (make-array 3 :element-type 'base-char
			   :adjustable t
			   :initial-contents "BAR"))

(def-intern-test intern.16
  :package-name (make-array 3 :element-type 'character
			   :adjustable t
			   :initial-contents "BAR"))

(def-intern-test intern.17
  :package-name
  (let* ((etype 'base-char)
	 (name0 (make-array 8 :element-type etype
			    :initial-contents "XBARYYYY")))
    (make-array 3 :element-type etype :displaced-to name0
		:displaced-index-offset 1)))

(def-intern-test intern.18
  :package-name
  (let* ((etype 'character)
	 (name0 (make-array 8 :element-type etype
			    :initial-contents "XBARYYYY")))
    (make-array 3 :element-type etype :displaced-to name0
		:displaced-index-offset 1)))

;;; Error tests

(deftest intern.error.1
  (signals-error (intern) program-error)
  t)

(deftest intern.error.2
  (signals-error (intern "X" "CL" nil) program-error)
  t)
