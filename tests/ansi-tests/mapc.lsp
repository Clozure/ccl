;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:21:24 2003
;;;; Contains: Tests of MAPC

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest mapc.1
  (mapc #'list nil)
  nil)

(deftest mapc.2
  (let ((x 0))
    (let ((result
	   (mapc #'(lambda (y) (incf x y))
		 '(1 2 3 4))))
      (list result x)))
  ((1 2 3 4) 10))

(deftest mapc.3
  (let ((x 0))
    (list
     (mapc #'(lambda (y z) (declare (ignore y z)) (incf x))
	   (make-list 5 :initial-element 'a)
	   (make-list 5 ))
     x))
  ((a a a a a) 5))

(deftest mapc.4
  (let ((x 0))
    (list
     (mapc #'(lambda (y z) (declare (ignore y z)) (incf x))
	   (make-list 5 :initial-element 'a)
	   (make-list 10))
     x))
  ((a a a a a) 5))

(deftest mapc.5
  (let ((x 0))
    (list
     (mapc #'(lambda (y z) (declare (ignore y z)) (incf x))
	   (make-list 5 :initial-element 'a)
	   (make-list 3))
     x))
  ((a a a a a) 3))

(deftest mapc.6
  (let* ((x (copy-list '(a b c d e f g h)))
	 (xcopy (make-scaffold-copy x)))
    (setf *mapc.6-var* nil)
    (let ((result (mapc 'mapc.6-fun x)))
      (and (check-scaffold-copy x xcopy)
	   (eqt result x)
	   *mapc.6-var*)))
  (h g f e d c b a))

(deftest mapc.order.1
  (let ((i 0) x y z)
    (values
     (mapc (progn (setf x (incf i))
		  #'list)
	   (progn (setf y (incf i))
		  '(a b c))
	   (progn (setf z (incf i))
		  '(1 2 3)))
     i x y z))
  (a b c) 3 1 2 3)

;;; Error tests

(deftest mapc.error.1
  (check-type-error #'(lambda (x) (mapc #'identity x)) #'listp)
  nil)

(deftest mapc.error.2
  (signals-error (mapc) program-error)
  t)

(deftest mapc.error.3
  (signals-error (mapc #'append) program-error)
  t)

(deftest mapc.error.4
  (signals-error (locally (mapc #'identity 1) t) type-error)
  t)

(deftest mapc.error.5
  (signals-error (mapc #'cons '(a b c)) program-error)
  t)

(deftest mapc.error.6
  (signals-error (mapc #'cons '(a b c) '(1 2 3) '(4 5 6)) program-error)
  t)

(deftest mapc.error.7
  (signals-error (mapc #'car '(a b c)) type-error)
  t)

(deftest mapc.error.8
  (signals-error (mapc #'identity (list* 1 2 3 4)) type-error)
  t)
