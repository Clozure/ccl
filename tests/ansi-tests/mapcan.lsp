;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:22:46 2003
;;;; Contains: Tests of MAPCAN

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest mapcan.1
  (mapcan #'list nil)
  nil)

(deftest mapcan.2
  (mapcan #'list (copy-list '(a b c d e f)))
  (a b c d e f))

(deftest mapcan.3
  (let* ((x (list 'a 'b 'c 'd))
	 (xcopy (make-scaffold-copy x))
	 (result (mapcan #'list x)))
    (and
     (= (length x) (length result))
     (check-scaffold-copy x xcopy)
     (loop
      for e1 on x
      and e2 on result
      count (or (eqt e1 e2) (not (eql (car e1) (car e2)))))))
  0)

(deftest mapcan.4
  (mapcan #'list
	  (copy-list '(1 2 3 4))
	  (copy-list '(a b c d)))
  (1 a 2 b 3 c 4 d))

(deftest mapcan.5
  (mapcan #'(lambda (x y) (make-list y :initial-element x))
	  (copy-list '(a b c d))
	  (copy-list '(1 2 3 4)))
  (a b b c c c d d d d))

(defvar *mapcan.6-var* nil)
(defun mapcan.6-fun (x)
  (push x *mapcan.6-var*)
  (copy-list *mapcan.6-var*))

(deftest mapcan.6
  (progn
    (setf *mapcan.6-var* nil)
    (mapcan 'mapcan.6-fun (copy-list '(a b c d))))
  (a b a c b a d c b a))

(deftest mapcan.order.1
  (let ((i 0) x y z)
    (values
     (mapcan (progn (setf x (incf i))
		    #'list)
	     (progn (setf y (incf i))
		    '(a b c))
	     (progn (setf z (incf i))
		    '(1 2 3)))
     i x y z))
  (a 1 b 2 c 3)
  3 1 2 3)

(deftest mapcan.8
  (mapcan #'(lambda (x y) (make-list y :initial-element x))
	  (copy-list '(a b c d))
	  (copy-list '(1 2 3 4 5 6)))
  (a b b c c c d d d d))

(deftest mapcan.9
  (mapcan #'(lambda (x y) (make-list y :initial-element x))
	  (copy-list '(a b c d e f))
	  (copy-list '(1 2 3 4)))
  (a b b c c c d d d d))

(deftest mapcan.10
  (mapcan #'list
	  (copy-list '(a b c d))
	  (copy-list '(1 2 3 4))
	  nil)
  nil)

(deftest mapcan.11
  (mapcan (constantly 1) (list 'a))
  1)

(deftest mapcan.error.1
  (check-type-error #'(lambda (x) (mapcan #'identity x)) #'listp)
  nil)

(deftest mapcan.error.2
  (signals-error (mapcan) program-error)
  t)

(deftest mapcan.error.3
  (signals-error (mapcan #'append) program-error)
  t)

(deftest mapcan.error.4
  (signals-error (locally (mapcan #'identity 1) t) type-error)
  t)

(deftest mapcan.error.5
  (signals-error (mapcan #'car '(a b c)) type-error)
  t)

(deftest mapcan.error.6
  (signals-error (mapcan #'cons '(a b c)) program-error)
  t)

(deftest mapcan.error.7
  (signals-error (mapcan #'cons '(a b c) '(1 2 3) '(4 5 6))
		 program-error)
  t)

(deftest mapcan.error.8
  (signals-error (mapcan #'identity (list* (list 1) (list 2) 3))
		 type-error)
  t)
