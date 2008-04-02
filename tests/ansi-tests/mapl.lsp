;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:23:23 2003
;;;; Contains: Tests of MAPL

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest mapl.1
  (mapl #'list nil)
  nil)

(deftest mapl.2
  (let* ((a nil)
	 (x (copy-list '(a b c)))
	 (xcopy (make-scaffold-copy x))
	 (result
	  (mapl #'(lambda (y) (push y a))
		x)))
    (and
     (check-scaffold-copy x xcopy)
     (eqt result x)
     a))
  ((c) (b c) (a b c)))

(deftest mapl.3
  (let* ((a nil)
	 (x (copy-list '(a b c d)))
	 (y (copy-list '(1 2 3 4)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (mapl #'(lambda (xtail ytail)
		    (setf a
			  (append (mapcar #'list xtail ytail)
				  a)))
		x y)))
    (and
     (eqt result x)
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     a))
  ((d 4) (c 3) (d 4) (b 2) (c 3) (d 4)
   (a 1) (b 2) (c 3) (d 4)))

(deftest mapl.4
  (let* ((a nil)
	 (x (copy-list '(a b c d)))
	 (y (copy-list '(1 2 3 4 5 6 7 8)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (mapl #'(lambda (xtail ytail)
		    (setf a
			  (append (mapcar #'list xtail ytail)
				  a)))
		x y)))
    (and
     (eqt result x)
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     a))
  ((d 4) (c 3) (d 4) (b 2) (c 3) (d 4)
   (a 1) (b 2) (c 3) (d 4)))

(deftest mapl.5
  (let* ((a nil)
	 (x (copy-list '(a b c d e f g)))
	 (y (copy-list '(1 2 3 4)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (mapl #'(lambda (xtail ytail)
		    (setf a
			  (append (mapcar #'list xtail ytail)
				  a)))
		x y)))
    (and
     (eqt result x)
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     a))
  ((d 4) (c 3) (d 4) (b 2) (c 3) (d 4)
   (a 1) (b 2) (c 3) (d 4)))

(deftest mapl.order.1
  (let ((i 0) x y z)
    (values
     (mapl (progn
	     (setf x (incf i))
	     (constantly nil))
	   (progn
	     (setf y (incf i))
	     '(a b c))
	   (progn
	     (setf z (incf i))
	     '(1 2 3)))
     i x y z))
  (a b c) 3 1 2 3)

(deftest mapl.error.1
  (check-type-error #'(lambda (x) (mapl #'identity x)) #'sequencep)
  nil)

(deftest mapl.error.2
  (signals-error (mapl) program-error)
  t)

(deftest mapl.error.3
  (signals-error (mapl #'append) program-error)
  t)

(deftest mapl.error.4
  (signals-error (locally (mapl #'identity 1) t) type-error)
  t)

(deftest mapl.error.5
  (signals-error (mapl #'cons '(a b c)) program-error)
  t)

(deftest mapl.error.6
  (signals-error (mapl #'cons '(a b c) '(1 2 3) '(4 5 6)) program-error)
  t)

(deftest mapl.error.7
  (signals-error (mapl #'caar '(a b c)) type-error)
  t)

(deftest mapl.error.8
  (signals-error (mapl #'identity (list* (list 1) (list 2) 3)) type-error)
  t)

