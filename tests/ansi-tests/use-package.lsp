;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:08:41 1998
;;;; Contains: Tests of USE-PACKAGE

(in-package :cl-test)

(compile-and-load "package-aux.lsp")

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use-package

(deftest use-package.1
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use nil))
	   (sym1 (intern "FOO" pg))
	   (i 0) x y)
      (and
       (eqt (export sym1 pg) t)
       (null (package-used-by-list pg))
       (null (package-used-by-list ph))
       (null (package-use-list pg))
       (null (package-use-list ph))
       (eqt (use-package (progn (setf x (incf i)) pg)
			 (progn (setf y (incf i)) ph))
	    t)  ;; "H" will use "G"
       (eql i 2) (eql x 1) (eql y 2)
       (multiple-value-bind (sym2 access)
	   (find-symbol "FOO" ph)
	 (and
	  (eqt access :inherited)
	  (eqt sym1 sym2)))
       (equal (package-use-list ph) (list pg))
       (equal (package-used-by-list pg) (list ph))
       (null (package-use-list pg))
       (null (package-used-by-list ph))
       (eqt (unuse-package pg ph) t)
       (null (find-symbol "FOO" ph)))))
  t)

(deftest use-package.2
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use nil))
	   (sym1 (intern "FOO" pg)))
      (and
       (eqt (export sym1 pg) t)
       (null (package-used-by-list pg))
       (null (package-used-by-list ph))
       (null (package-use-list pg))
       (null (package-use-list ph))
       (eqt (use-package "G" "H") t)  ;; "H" will use "G"
       (multiple-value-bind (sym2 access)
	   (find-symbol "FOO" ph)
	 (and
	  (eqt access :inherited)
	  (eqt sym1 sym2)))
       (equal (package-use-list ph) (list pg))
       (equal (package-used-by-list pg) (list ph))
       (null (package-use-list pg))
       (null (package-used-by-list ph))
       (eqt (unuse-package pg ph) t)
       (null (find-symbol "FOO" ph)))))
  t)

(deftest use-package.3
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use nil))
	   (sym1 (intern "FOO" pg)))
      (and
       (eqt (export sym1 pg) t)
       (null (package-used-by-list pg))
       (null (package-used-by-list ph))
       (null (package-use-list pg))
       (null (package-use-list ph))
       (eqt (use-package '#:|G| '#:|H|) t)  ;; "H" will use "G"
       (multiple-value-bind (sym2 access)
	   (find-symbol "FOO" ph)
	 (and
	  (eqt access :inherited)
	  (eqt sym1 sym2)))
       (equal (package-use-list ph) (list pg))
       (equal (package-used-by-list pg) (list ph))
       (null (package-use-list pg))
       (null (package-used-by-list ph))
       (eqt (unuse-package pg ph) t)
       (null (find-symbol "FOO" ph)))))
  t)

(deftest use-package.4
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let* ((pg (make-package "G" :use nil))
	   (ph (make-package "H" :use nil))
	   (sym1 (intern "FOO" pg)))
      (and
       (eqt (export sym1 pg) t)
       (null (package-used-by-list pg))
       (null (package-used-by-list ph))
       (null (package-use-list pg))
       (null (package-use-list ph))
       (eqt (ignore-errors (use-package #\G #\H))
	    t)  ;; "H" will use "G"
       (multiple-value-bind (sym2 access)
	   (find-symbol "FOO" ph)
	 (and
	  (eqt access :inherited)
	  (eqt sym1 sym2)))
       (equal (package-use-list ph) (list pg))
       (equal (package-used-by-list pg) (list ph))
       (null (package-use-list pg))
       (null (package-used-by-list ph))
       (eqt (unuse-package pg ph) t)
       (null (find-symbol "FOO" ph)))))
  t)

;; use lists of packages

(deftest use-package.5
  (let ((pkgs '("H" "G1" "G2" "G3"))
	(vars '("FOO1" "FOO2" "FOO3")))
    (dolist (p pkgs)
      (safely-delete-package p)
      (make-package p :use nil))
    (and
     (every (complement #'package-use-list) pkgs)
     (every (complement #'package-used-by-list) pkgs)
     (every #'(lambda (v p)
		(export (intern v p) p))
	    vars (cdr pkgs))
     (progn
       (dolist (p (cdr pkgs)) (intern "MINE" p))
       (eqt (use-package (cdr pkgs) (car pkgs)) t))
     (every #'(lambda (v p)
		(eqt (find-symbol v p)
		     (find-symbol v (car pkgs))))
	    vars (cdr pkgs))
     (null (find-symbol "MINE" (car pkgs)))
     (every #'(lambda (p)
		(equal (package-used-by-list p)
		       (list (find-package (car pkgs)))))
	    (cdr pkgs))
     (equal (sort-package-list (package-use-list (car pkgs)))
	    (mapcar #'find-package (cdr pkgs)))
     (every (complement #'package-use-list) (cdr pkgs))
     (null (package-used-by-list (car pkgs)))))
  t)

;; Circular package use

(deftest use-package.6
  (progn
    (safely-delete-package "H")
    (safely-delete-package "G")
    (let ((pg (make-package "G"))
	  (ph (make-package "H"))
	  sym1 sym2 sym3 sym4
	  a1 a2 a3 a4)
      (prog1
	  (and
	   (export (intern "X" pg) pg)
	   (export (intern "Y" ph) ph)
	   (use-package pg ph)
	   (use-package ph pg)
	   (progn
	     (multiple-value-setq
		 (sym1 a1) (find-symbol "X" pg))
	     (multiple-value-setq
		 (sym2 a2) (find-symbol "Y" ph))
	     (multiple-value-setq
		 (sym3 a3) (find-symbol "Y" pg))
	     (multiple-value-setq
		 (sym4 a4) (find-symbol "X" ph))
	     (and
	      (eqt a1 :external)
	      (eqt a2 :external)
	      (eqt a3 :inherited)
	      (eqt a4 :inherited)
	      (eqt sym1 sym4)
	      (eqt sym2 sym3)
	      (eqt (symbol-package sym1) pg)
	      (eqt (symbol-package sym2) ph)
	      (unuse-package pg ph)
	      (unuse-package ph pg))))
	(safely-delete-package pg)
	(safely-delete-package ph))))
  t)

;; Check that *PACKAGE* is used as a default

(deftest use-package.7
  (let ((user-name "H")
	(used-name "G"))
    (safely-delete-package user-name)
    (safely-delete-package used-name)
    (let* ((pused (make-package used-name :use nil))
	   (puser (make-package user-name :use nil))
	   (sym1 (intern "FOO" pused)))
      (and
       (eqt (export sym1 pused) t)
       (null (package-used-by-list pused))
       (null (package-used-by-list puser))
       (null (package-use-list pused))
       (null (package-use-list puser))
       (eqt (let ((*package* puser)) (use-package pused)) t)  ;; user will use used
       (multiple-value-bind (sym2 access)
	   (find-symbol "FOO" puser)
	 (and
	  (eqt access :inherited)
	  (eqt sym1 sym2)))
       (equal (package-use-list puser) (list pused))
       (equal (package-used-by-list pused) (list puser))
       (null (package-use-list pused))
       (null (package-used-by-list puser))
       (eqt (unuse-package pused puser) t)
       (null (find-symbol "FOO" puser)))))
  t)

;;; Tests for specialized sequence arguments

(defmacro def-use-package-test (test-name &key (user "H") (used "G"))
  `(deftest ,test-name
     (let ((user-name ,user)
	   (used-name ,used))
       (safely-delete-package user-name)
       (safely-delete-package used-name)
       (let* ((pused (make-package used-name :use nil))
	      (puser (make-package user-name :use nil))
	      (sym1 (intern "FOO" pused)))
	 (and
	  (eqt (export sym1 pused) t)
	  (null (package-used-by-list pused))
	  (null (package-used-by-list puser))
	  (null (package-use-list pused))
	  (null (package-use-list puser))
	  (eqt (let ((*package* puser)) (use-package pused)) t)  ;; user will use used
	  (multiple-value-bind (sym2 access)
	      (find-symbol "FOO" puser)
	    (and
	     (eqt access :inherited)
	     (eqt sym1 sym2)))
	  (equal (package-use-list puser) (list pused))
	  (equal (package-used-by-list pused) (list puser))
	  (null (package-use-list pused))
	  (null (package-used-by-list puser))
	  (eqt (unuse-package pused puser) t)
	  (null (find-symbol "FOO" puser)))))
     t))

;;; Specialized user package designator

(def-use-package-test use-package.10
  :user (make-array 5 :initial-contents "TEST1" :element-type 'base-char))

(def-use-package-test use-package.11
  :user (make-array 10 :initial-contents "TEST1ABCDE"
		    :fill-pointer 5 :element-type 'base-char))

(def-use-package-test use-package.12
  :user (make-array 10 :initial-contents "TEST1ABCDE"
		    :fill-pointer 5 :element-type 'character))

(def-use-package-test use-package.13
  :user (make-array 5 :initial-contents "TEST1"
		    :adjustable t :element-type 'base-char))

(def-use-package-test use-package.14
  :user (make-array 5 :initial-contents "TEST1"
		    :adjustable t :element-type 'character))

(def-use-package-test use-package.15
  :user (let* ((etype 'base-char)
	       (name0 (make-array 10 :element-type etype
				  :initial-contents "xxxxxTEST1")))
	  (make-array 5 :element-type etype
		      :displaced-to name0
		      :displaced-index-offset 5)))

(def-use-package-test use-package.16
  :user
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "xxxxxTEST1")))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 5)))

;;; Specialed used package designator

(def-use-package-test use-package.17
  :used (make-array 5 :initial-contents "TEST1" :element-type 'base-char))

(def-use-package-test use-package.18
  :used (make-array 10 :initial-contents "TEST1ABCDE"
		    :fill-pointer 5 :element-type 'base-char))

(def-use-package-test use-package.19
  :used (make-array 10 :initial-contents "TEST1ABCDE"
		    :fill-pointer 5 :element-type 'character))

(def-use-package-test use-package.20
  :used (make-array 5 :initial-contents "TEST1"
		    :adjustable t :element-type 'base-char))

(def-use-package-test use-package.21
  :used (make-array 5 :initial-contents "TEST1"
		    :adjustable t :element-type 'character))

(def-use-package-test use-package.22
  :used (let* ((etype 'base-char)
	       (name0 (make-array 10 :element-type etype
				  :initial-contents "xxxxxTEST1")))
	  (make-array 5 :element-type etype
		      :displaced-to name0
		      :displaced-index-offset 5)))

(def-use-package-test use-package.23
  :used
  (let* ((etype 'character)
	 (name0 (make-array 10 :element-type etype
			    :initial-contents "xxxxxTEST1")))
    (make-array 5 :element-type etype
		:displaced-to name0
		:displaced-index-offset 5)))

(deftest use-package.error.1
  (signals-error (use-package) program-error)
  t)

(deftest use-package.error.2
  (progn
    (safely-delete-package "UPE2A")
    (safely-delete-package "UPE2")
    (make-package "UPE2" :use ())
    (make-package "UPE2A" :use ())
    (signals-error (use-package "UPE2" "UPE2A" nil) program-error))
  t)
