;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 14 06:21:45 2004
;;;; Contains: Tests of APROPOS-LIST

(in-package :cl-test)

(deftest apropos-list.1
  (let ((pkg "CL-TEST-APROPOS-LIST-PACKAGE"))
    (safely-delete-package pkg)
    (unwind-protect
	(progn
	  (eval `(defpackage ,pkg (:use)))
	  (let* ((sym (intern "FOO" pkg)))
	    (loop for p in (list pkg (find-package pkg) (make-symbol pkg))
		  nconc
		  (loop for string-designator in
			'("F" "O" #\F #\O "" "FOO" "FO"
			  "OO" :|F| :|FO| :|FOO| :|O| :|OO|)
			for result = (apropos-list string-designator p)
			unless (equal result (list sym))
			collect (list string-designator result)))))
      (safely-delete-package pkg)))
  nil)

(deftest apropos-list.2
  (let ((pkg #\A))
    (safely-delete-package pkg)
    (unwind-protect
	(progn
	  (eval `(defpackage ,pkg (:use)))
	  (let* ((sym (intern "FOO" pkg)))
	    (loop for string-designator in
		  '("F" "O" #\F #\O "" "FOO" "FO"
		    "OO" :|F| :|FO| :|FOO| :|O| :|OO|)
		  for result = (apropos-list string-designator pkg)
		  unless (equal result (list sym))
		  collect (list string-designator result))))
      (safely-delete-package pkg)))
  nil)

(deftest apropos-list.3
  (let ((pkg "CL-TEST-APROPOS-LIST-PACKAGE"))
    (safely-delete-package pkg)
    (unwind-protect
	(progn
	  (eval `(defpackage ,pkg (:use)))
	  (intern "FOO" pkg)
	  (apropos-list "X" pkg))
      (safely-delete-package pkg)))
  nil)

(deftest apropos-list.4
  (let ((sym :|X|)
	(symbols (apropos-list "X")))
    (notnot (member sym symbols)))
  t)

(deftest apropos-list.5
  (let ((sym :|X|)
	(symbols (apropos-list '#:|X|)))
    (notnot (member sym symbols)))
  t)

(deftest apropos-list.6
  (let ((sym :|X|)
	(symbols (apropos-list #\X)))
    (notnot (member sym symbols)))
  t)

(deftest apropos-list.7
  (let ((sym :|X|)
	(symbols (apropos-list "X" nil)))
    (notnot (member sym symbols)))
  t)

(deftest apropos-list.8
  (let ((*package* (find-package "COMMON-LISP")))
    (macrolet
     ((%m (z) z))
     (intersection '(car)
		   (apropos-list (expand-in-current-env (%m "CAR"))))))
  (car))

(deftest apropos-list.9
  (macrolet
   ((%m (z) z))
   (intersection '(car)
		 (apropos-list "CAR" (expand-in-current-env
				      (%m (find-package "COMMON-LISP"))))))
  (car))

;;; Error tests

(deftest apropos-list.error.1
  (signals-error (apropos-list) program-error)
  t)

(deftest apropos-list.error.2
  (signals-error (apropos-list "X" (find-package "CL-TEST") nil) program-error)
  t)
