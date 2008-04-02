;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:00:28 1998
;;;; Contains: Tests of RENAME-PACKAGE

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rename-package

(deftest rename-package.1
  (block nil
    (safely-delete-package "TEST1")
    (safely-delete-package "TEST2")
    (let ((p (make-package "TEST1"))
	  (i 0) x y)
      (unless (packagep p) (return nil))
      (let ((p2 (rename-package (progn (setf x (incf i)) "TEST1")
				(progn (setf y (incf i)) "TEST2"))))
	(unless (packagep p2)
	  (safely-delete-package p)
	  (return p2))
	(unless (and (eqt p p2)
		     (eql i 2)
		     (eql x 1)
		     (eql y 2)
		     (equal (package-name p2) "TEST2"))
	  (safely-delete-package p)
	  (safely-delete-package p2)
	  (return nil))
	(safely-delete-package p2)
	t)))
  t)

(deftest rename-package.2
  (block nil
    (safely-delete-package "TEST1")
    (safely-delete-package "TEST2")
    (safely-delete-package "TEST3")
    (safely-delete-package "TEST4")
    (safely-delete-package "TEST5")
    (let ((p (make-package "TEST1"))
	  (nicknames (copy-list '("TEST3" "TEST4" "TEST5"))))
      (unless (packagep p) (return nil))
      (let ((p2 (rename-package "TEST1" "TEST2" nicknames)))
	(unless (packagep p2)
	  (safely-delete-package p)
	  (return p2))
	(unless (and (eqt p p2)
		     (equal (package-name p2) "TEST2")
		     (null (set-exclusive-or nicknames
					     (package-nicknames p2)
					     :test #'equal)))
	  (safely-delete-package p)
	  (safely-delete-package p2)
	  (return nil))
	(safely-delete-package p2)
	t)))
  t)

(deftest rename-package.3
  (block nil
    (safely-delete-package "TEST1")
    (safely-delete-package "TEST2")
    (let ((p (make-package "TEST1"))
	  (nicknames (copy-list '(#\M #\N))))
      (unless (packagep p) (return nil))
      (let ((p2 (ignore-errors (rename-package "TEST1" "TEST2" nicknames))))
	(unless (packagep p2)
	  (safely-delete-package p)
	  (return p2))
	(unless (and (eqt p p2)
		     (equal (package-name p2) "TEST2")
		     (equal
		      (sort (copy-list (package-nicknames p2))
			    #'string<)
		      (sort (mapcar #'(lambda (c)
					(make-string 1 :initial-element c))
				    nicknames)
			    #'string<)))
	  (safely-delete-package p)
	  (safely-delete-package p2)
	  (return nil))
	(safely-delete-package p2)
	t)))
  t)

(deftest rename-package.4
  (block nil
    (safely-delete-package "G")
    (safely-delete-package "TEST2")
    (let ((p (make-package "G"))
	  (nicknames nil))
      (unless (packagep p) (return nil))
      (let ((p2 (ignore-errors (rename-package #\G "TEST2" nicknames))))
	(unless (packagep p2)
	  (safely-delete-package p)
	  (return p2))
	(unless (and (eqt p p2)
		     (equal (package-name p2) "TEST2")
		     (null (set-exclusive-or nicknames
					     (package-nicknames p2)
					     :test #'equal)))
	  (safely-delete-package p)
	  (safely-delete-package p2)
	  (return nil))
	(ignore-errors (safely-delete-package p2))
	t)))
  t)

(deftest rename-package.5
  (block nil
    (safely-delete-package "TEST1")
    (safely-delete-package "G")
    (let ((p (make-package "TEST1"))
	  (nicknames nil))
      (unless (packagep p) (return nil))
      (let ((p2 (ignore-errors (rename-package "TEST1" #\G nicknames))))
	(unless (packagep p2)
	  (safely-delete-package p)
	  (return p2))
	(unless (and (eqt p p2)
		     (equal (package-name p2) "G")
		     (null (set-exclusive-or nicknames
					     (package-nicknames p2)
					     :test #'equal)))
	  (safely-delete-package p)
	  (safely-delete-package p2)
	  (return nil))
	(safely-delete-package p2)
	t)))
  t)

(deftest rename-package.6
  (block nil
    (safely-delete-package '|TEST1|)
    (safely-delete-package '|TEST2|)
    (safely-delete-package '|M|)
    (safely-delete-package '|N|)
    (let ((p (make-package '|TEST1|))
	  (nicknames (copy-list '(|M| |N|))))
      (unless (packagep p) (return nil))
      (let ((p2 (ignore-errors (rename-package
				'|TEST1| '|TEST2| nicknames))))
	(unless (packagep p2)
	  (safely-delete-package p)
	  (return p2))
	(unless (and (eqt p p2)
		     (equal (package-name p2) "TEST2")
		     (equal
		      (sort (copy-list (package-nicknames p2))
			    #'string<)
		      (sort (mapcar #'symbol-name nicknames)
			    #'string<)))
	  (safely-delete-package p)
	  (safely-delete-package p2)
	  (return nil))
	(safely-delete-package p2)
	t)))
  t)

(deftest rename-package.7
  (block nil
    (let ((name1 (make-array '(5) :element-type 'base-char
			     :initial-contents "TEST1"))
	  (name2 (make-array '(5) :element-type 'base-char
			     :initial-contents "TEST2")))
      (safely-delete-package name1)
      (safely-delete-package name2)
      (let ((p (make-package name1)))
	(unless (packagep p) (return nil))
	(let ((p2 (rename-package name1 name2)))
	  (unless (packagep p2)
	    (safely-delete-package p)
	    (return p2))
	  (unless (and (eqt p p2)
		       (equal (package-name p2) name2))
	    (safely-delete-package p)
	    (safely-delete-package p2)
	    (return nil))
	  (safely-delete-package p2)
	  t))))
  t)

(deftest rename-package.8
  (block nil
    (let ((name1 (make-array '(10) :element-type 'base-char
			     :fill-pointer 5
			     :initial-contents "TEST1     "))
	  (name2 (make-array '(9) :element-type 'character
			     :fill-pointer 5
			     :initial-contents "TEST2XXXX")))
      (safely-delete-package name1)
      (safely-delete-package name2)
      (let ((p (make-package "TEST1")))
	(unless (packagep p) (return nil))
	(let ((p2 (rename-package name1 name2)))
	  (unless (packagep p2)
	    (safely-delete-package p)
	    (return p2))
	  (unless (and (eqt p p2)
		       (string= (package-name p2) "TEST2"))
	    (safely-delete-package p)
	    (safely-delete-package p2)
	    (return nil))
	  (safely-delete-package p2)
	  t))))
  t)

(deftest rename-package.9
  (block nil
    (let ((name1 (make-array '(5) :element-type 'character
			     :adjustable t
			     :initial-contents "TEST1"))
	  (name2 (make-array '(5) :element-type 'base-char
			     :adjustable t
			     :initial-contents "TEST2")))
      (safely-delete-package name1)
      (safely-delete-package name2)
      (let ((p (make-package "TEST1")))
	(unless (packagep p) (return nil))
	(let ((p2 (rename-package name1 name2)))
	  (unless (packagep p2)
	    (safely-delete-package p)
	    (return p2))
	  (unless (and (eqt p p2)
		       (string= (package-name p2) "TEST2"))
	    (safely-delete-package p)
	    (safely-delete-package p2)
	    (return nil))
	  (safely-delete-package p2)
	  t))))
  t)



(deftest rename-package.error.1
  (signals-error (rename-package) program-error)
  t)

(deftest rename-package.error.2
  (signals-error (rename-package "CL") program-error)
  t)

(deftest rename-package.error.3
  (signals-error (rename-package "A" "XXXXX" NIL NIL) program-error)
  t)
