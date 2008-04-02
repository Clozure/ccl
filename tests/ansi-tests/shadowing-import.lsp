;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 29 07:42:18 2004
;;;; Contains: Tests for SHADOWING-IMPORT

(in-package :cl-test)

(deftest shadowing-import.1
  (let ((name1 "TEST1")
	(name2 "TEST2"))
    (safely-delete-package name1)
    (safely-delete-package name2)
    (prog1
	(let* ((p1 (make-package name1 :use nil))
	       (p2 (make-package name2))
	       (s1 (intern "X" p1))
	       (s2 (intern "X" p2)))
	  (list
	   (eqt s1 s2)
	   (eqt (find-symbol "X" p2) s2)
	   (shadowing-import s1 p2)
	   (equalt (package-shadowing-symbols p2) (list s1))
	   (eqt (find-symbol "X" p2) s1)))
      (safely-delete-package name1)
      (safely-delete-package name2)))
  (nil t t t t))

(deftest shadowing-import.2
  (let ((name1 "TEST1")
	(name2 "TEST2"))
    (safely-delete-package name1)
    (safely-delete-package name2)
    (prog1
	(let* ((p1 (make-package name1 :use nil))
	       (p2 (make-package name2))
	       (s1 (intern "X" p1)))
	  (list
	   (find-symbol "X" p2)
	   (shadowing-import s1 p2)
	   (equalt (package-shadowing-symbols p2) (list s1))
	   (eqt (find-symbol "X" p2) s1)))
      (safely-delete-package name1)
      (safely-delete-package name2)))
  (nil t t t))

(deftest shadowing-import.3
  (let ((name1 "TEST1")
	(name2 "TEST2"))
    (safely-delete-package name1)
    (safely-delete-package name2)
    (prog1
	(let* ((p1 (make-package name1 :use nil))
	       (p2 (make-package name2 :use nil))
	       (s1 (intern "X" p1))
	       (s2 (intern "X" p2)))
	  (list
	   (eqt s1 s2)
	   (eqt (find-symbol "X" p2) s2)
	   (let ((*package* p2))
	     (shadowing-import s1))
	   (equalt (package-shadowing-symbols p2) (list s1))
	   (eqt (find-symbol "X" p2) s1)))
      (safely-delete-package name1)
      (safely-delete-package name2)))
  (nil t t t t))

(deftest shadowing-import.4
  (let ((name1 "TEST1")
	(name2 "TEST2")
	(name3 "TEST3"))
    (safely-delete-package name1)
    (safely-delete-package name2)
    (safely-delete-package name3)
    (prog1
	(let* ((p1 (make-package name1 :use nil))
	       (p3 (make-package name2 :use nil))
	       (p2 (make-package name3 :use (list p3)))
	       (s1 (intern "X" p1))
	       (s2 (intern "X" p3)))
	  (export s2 p3)
	  (list
	   (eqt s1 s2)
	   (eqt (find-symbol "X" p2) s2)
	   (shadowing-import s1 p2)
	   (equalt (package-shadowing-symbols p2) (list s1))
	   (eqt (find-symbol "X" p2) s1)))
      (safely-delete-package name1)
      (safely-delete-package name3)
      (safely-delete-package name2)))
  (nil t t t t))

;;; Specialized sequence tests

(defmacro def-shadowing-import-test (test-name name-form)
  `(deftest ,test-name
     (let ((name1 ,name-form))
       (safely-delete-package name1)
       (prog1
	   (let* ((p1 (make-package name1 :use nil)))
	     (list
	      (find-symbol "T" p1)
	      (shadowing-import t name1)
	      (package-shadowing-symbols p1)
	      (find-symbol "T" p1)))
	 (safely-delete-package name1)))
     (nil t (t) t)))
  
(def-shadowing-import-test shadowing-import.5
  (make-array '(5) :initial-contents "TEST1"
	      :element-type 'base-char))

(def-shadowing-import-test shadowing-import.6
  (make-array '(7) :initial-contents "TEST1XX"
	      :fill-pointer 7
	      :element-type 'character))

(def-shadowing-import-test shadowing-import.7
  (make-array '(7) :initial-contents "TEST1XX"
	      :fill-pointer 7
	      :element-type 'base-char))

(def-shadowing-import-test shadowing-import.8
  (make-array '(5) :initial-contents "TEST1"
	      :adjustable t
	      :element-type 'base-char))

(def-shadowing-import-test shadowing-import.9
  (make-array '(5) :initial-contents "TEST1"
	      :adjustable t
	      :element-type 'character))

(def-shadowing-import-test shadowing-import.10
  (let* ((etype 'character)
	 (name2 (make-array '(10) :initial-contents "ABTEST1CDE"
			    :element-type etype)))
    (make-array '(5) :element-type etype
		:displaced-to name2
		:displaced-index-offset 2)))

(def-shadowing-import-test shadowing-import.11
  (let* ((etype 'base-char)
	 (name2 (make-array '(10) :initial-contents "ABTEST1CDE"
			    :element-type etype)))
    (make-array '(5) :element-type etype
		:displaced-to name2
		:displaced-index-offset 2)))

;;; Error tests

(deftest shadowing-import.error.1
  (signals-error (shadowing-import) program-error)
  t)

(deftest shadowing-import.error.2
  (signals-error (shadowing-import nil *package* nil)
		 program-error)
  t)
