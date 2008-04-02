;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:06:48 1998
;;;; Contains: Tests of UNUSE-PACKAGE

(in-package :cl-test)

(compile-and-load "package-aux.lsp")

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unuse-package

(deftest unuse-package.1
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use '("G")))
	   (i 0) x y)
      (prog1
	  (and
	   (equal (package-use-list ph) (list pg))
	   (equal (package-used-by-list pg) (list ph))
	   (unuse-package (progn (setf x (incf i)) pg)
			  (progn (setf y (incf i)) ph))
	   (eql i 2) (eql x 1) (eql y 2)
	   (equal (package-use-list ph) nil)
	   (null (package-used-by-list pg)))
	(safely-delete-package "H")
	(safely-delete-package "G"))))
  t)

(deftest unuse-package.2
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use '("G"))))
      (prog1
	  (and
	   (equal (package-use-list ph) (list pg))
	   (equal (package-used-by-list pg) (list ph))
	   (unuse-package "G" ph)
	   (equal (package-use-list ph) nil)
	   (null (package-used-by-list pg)))
	(safely-delete-package "H")
	(safely-delete-package "G"))))
  t)  

(deftest unuse-package.3
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use '("G"))))
      (prog1
	  (and
	   (equal (package-use-list ph) (list pg))
	   (equal (package-used-by-list pg) (list ph))
	   (unuse-package :|G| ph)
	   (equal (package-use-list ph) nil)
	   (null (package-used-by-list pg)))
	(safely-delete-package "H")
	(safely-delete-package "G"))))
  t)

(deftest unuse-package.4
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use '("G"))))
      (prog1
	  (and
	   (equal (package-use-list ph) (list pg))
	   (equal (package-used-by-list pg) (list ph))
	   (ignore-errors (unuse-package #\G ph))
	   (equal (package-use-list ph) nil)
	   (null (package-used-by-list pg)))
	(safely-delete-package "H")
	(safely-delete-package "G"))))
  t)

(deftest unuse-package.5
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use '("G"))))
      (prog1
	  (and
	   (equal (package-use-list ph) (list pg))
	   (equal (package-used-by-list pg) (list ph))
	   (unuse-package (list pg) ph)
	   (equal (package-use-list ph) nil)
	   (null (package-used-by-list pg)))
	(safely-delete-package "H")
	(safely-delete-package "G"))))
  t)  

(deftest unuse-package.6
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use '("G"))))
      (prog1
	  (and
	   (equal (package-use-list ph) (list pg))
	   (equal (package-used-by-list pg) (list ph))
	   (unuse-package (list "G") ph)
	   (equal (package-use-list ph) nil)
	   (null (package-used-by-list pg)))
	(safely-delete-package "H")
	(safely-delete-package "G"))))
  t)

(deftest unuse-package.7
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use '("G"))))
      (prog1
	  (and
	   (equal (package-use-list ph) (list pg))
	   (equal (package-used-by-list pg) (list ph))
	   (unuse-package (list :|G|) ph)
	   (equal (package-use-list ph) nil)
	   (null (package-used-by-list pg)))
	(safely-delete-package "H")
	(safely-delete-package "G"))))
  t)

(deftest unuse-package.8
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use '("G"))))
      (prog1
	  (and
	   (equal (package-use-list ph) (list pg))
	   (equal (package-used-by-list pg) (list ph))
	   (ignore-errors (unuse-package (list #\G) ph))
	   (equal (package-use-list ph) nil)
	   (null (package-used-by-list pg)))
	(safely-delete-package "H")
	(safely-delete-package "G"))))
  t)

;; Now test with multiple packages

(deftest unuse-package.9
  (progn
    (dolist (p '("H1" "H2" "G1" "G2" "G3"))
      (safely-delete-package p))
    (let* ((pg1 (make-package "G1" :use nil))
	   (pg2 (make-package "G2" :use nil))
	   (pg3 (make-package "G3" :use nil))
	   (ph1 (make-package "H1" :use (list pg1 pg2 pg3)))
	   (ph2 (make-package "H2" :use (list pg1 pg2 pg3))))
      (let ((pubg1 (sort-package-list (package-used-by-list pg1)))
	    (pubg2 (sort-package-list (package-used-by-list pg2)))
	    (pubg3 (sort-package-list (package-used-by-list pg3)))
	    (puh1  (sort-package-list (package-use-list ph1)))
	    (puh2  (sort-package-list (package-use-list ph2))))
	(prog1
	    (and
	     (= (length (remove-duplicates (list pg1 pg2 pg3 ph1 ph2)))
		5)
	     (equal (list ph1 ph2) pubg1)
	     (equal (list ph1 ph2) pubg2)
	     (equal (list ph1 ph2) pubg3)
	     (equal (list pg1 pg2 pg3) puh1)
	     (equal (list pg1 pg2 pg3) puh2)
	     (unuse-package (list pg1 pg3) ph1)
	     (equal (package-use-list ph1) (list pg2))
	     (equal (package-used-by-list pg1) (list ph2))
	     (equal (package-used-by-list pg3) (list ph2))
	     (equal (sort-package-list (package-use-list ph2))
		    (list pg1 pg2 pg3))
	     (equal (sort-package-list (package-used-by-list pg2))
		    (list ph1 ph2))
	     t)
	  (dolist (p '("H1" "H2" "G1" "G2" "G3"))
	    (safely-delete-package p))))))
  t)

;;; Specialized sequences

(defmacro def-unuse-package-test (test-name &key
					    (user "H")
					    (used "G"))
  `(deftest ,test-name
     (let ((user-name ,user)
	   (used-name ,used))
       (safely-delete-package user-name)
       (safely-delete-package used-name)
       (let* ((pused (make-package used-name :use nil))
	      (puser (make-package user-name :use (list used-name))))
	 (prog1
	     (and
	      (equal (package-use-list puser) (list pused))
	      (equal (package-used-by-list pused) (list puser))
	      (unuse-package (list used-name) user-name)
	      (equal (package-use-list puser) nil)
	      (null (package-used-by-list pused)))
	   (safely-delete-package user-name)
	   (safely-delete-package used-name))))
     t))

;;; Specialized user package designator

(def-unuse-package-test unuse-package.10
  :user (make-array 5 :initial-contents "TEST1" :element-type 'base-char))

(def-unuse-package-test unuse-package.11
  :user (make-array 10 :initial-contents "TEST1ABCDE"
		    :fill-pointer 5 :element-type 'base-char))

(def-unuse-package-test unuse-package.12
  :user (make-array 10 :initial-contents "TEST1ABCDE"
		    :fill-pointer 5 :element-type 'character))

(def-unuse-package-test unuse-package.13
  :user (make-array 5 :initial-contents "TEST1"
		    :adjustable t :element-type 'base-char))

(def-unuse-package-test unuse-package.14
  :user (make-array 5 :initial-contents "TEST1"
		    :adjustable t :element-type 'character))

(def-unuse-package-test unuse-package.15
  :user (let* ((etype 'base-char)
	       (name0 (make-array 10 :element-type etype
				  :initial-contents "xxxxxTEST1")))
	  (make-array 5 :element-type etype
		      :displaced-to name0
		      :displaced-index-offset 5)))

(def-unuse-package-test unuse-package.16
  :user
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "xxxxxTEST1")))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 5)))

;;; Specialed used package designator

(def-unuse-package-test unuse-package.17
  :used (make-array 5 :initial-contents "TEST1" :element-type 'base-char))

(def-unuse-package-test unuse-package.18
  :used (make-array 10 :initial-contents "TEST1ABCDE"
		    :fill-pointer 5 :element-type 'base-char))

(def-unuse-package-test unuse-package.19
  :used (make-array 10 :initial-contents "TEST1ABCDE"
		    :fill-pointer 5 :element-type 'character))

(def-unuse-package-test unuse-package.20
  :used (make-array 5 :initial-contents "TEST1"
		    :adjustable t :element-type 'base-char))

(def-unuse-package-test unuse-package.21
  :used (make-array 5 :initial-contents "TEST1"
		    :adjustable t :element-type 'character))

(def-unuse-package-test unuse-package.22
  :used (let* ((etype 'base-char)
	       (name0 (make-array 10 :element-type etype
				  :initial-contents "xxxxxTEST1")))
	  (make-array 5 :element-type etype
		      :displaced-to name0
		      :displaced-index-offset 5)))

(def-unuse-package-test unuse-package.23
  :used
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "xxxxxTEST1")))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 5)))

;;; Error tests

(deftest unuse-package.error.1
  (signals-error (unuse-package) program-error)
  t)

(deftest unuse-package.error.2
  (progn
    (safely-delete-package "UPE2A")
    (safely-delete-package "UPE2")
    (make-package "UPE2" :use ())
    (make-package "UPE2A" :use '("UPE2"))
    (signals-error (unuse-package "UPE2" "UPE2A" nil) program-error))
  t)
