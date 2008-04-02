;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:24:28 2003
;;;; Contains: Tests of MAPCON

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest mapcon.1
  (mapcon #'(lambda (x) (append '(a) x nil)) nil)
  nil)

(deftest mapcon.2
  (let* ((x (copy-list '(1 2 3 4)))
	 (xcopy (make-scaffold-copy x))
	 (result
	  (mapcon #'(lambda (y) (append '(a) y nil)) x)))
    (and
     (check-scaffold-copy x xcopy)
     result))
  (a 1 2 3 4 a 2 3 4 a 3 4 a 4))

(deftest mapcon.3
  (let* ((x (copy-list '(4 2 3 2 2)))
	 (y (copy-list '(a b c d e f g h i j k l)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (mapcon #'(lambda (xt yt)
		      (subseq yt 0 (car xt)))
		  x y)))
    (and
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     result))
  (a b c d b c c d e d e e f))

(deftest mapcon.4
  (mapcon (constantly 1) (list 'a))
  1)

(deftest mapcon.order.1
  (let ((i 0) x y z)
    (values
     (mapcon (progn (setf x (incf i))
		    #'(lambda (x y) (list (car x) (car y))))
	     (progn (setf y (incf i))
		    '(a b c))
	     (progn (setf z (incf i))
		    '(1 2 3)))
     i x y z))
  (a 1 b 2 c 3)
  3 1 2 3)

(deftest mapcon.error.1
  (check-type-error #'(lambda (x) (mapcon #'identity x)) #'listp)
  nil)

(deftest mapcon.error.2
  (signals-error (mapcon) program-error)
  t)

(deftest mapcon.error.3
  (signals-error (mapcon #'append) program-error)
  t)

(deftest mapcon.error.4
  (signals-error (locally (mapcon #'identity 1) t) type-error)
  t)

(deftest mapcon.error.5
  (signals-error (mapcon #'caar '(a b c)) type-error)
  t)

(deftest mapcon.error.6
  (signals-error (mapcon #'cons '(a b c)) program-error)
  t)

(deftest mapcon.error.7
  (signals-error (mapcon #'cons '(a b c) '(1 2 3) '(4 5 6)) program-error)
  t)

(deftest mapcon.error.8
  (signals-error (mapcon #'copy-tree (cons 1 2)) type-error)
  t)
