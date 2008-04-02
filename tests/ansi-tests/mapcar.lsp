;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:22:16 2003
;;;; Contains: Tests of MAPCAR

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest mapcar.1
  (mapcar #'1+ nil)
  nil)

(deftest mapcar.2
  (let* ((x (copy-list '(1 2 3 4)))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (mapcar #'1+ x)))
      (and (check-scaffold-copy x xcopy)
	   result)))
  (2 3 4 5))

(deftest mapcar.3
  (let* ((n 0)
	 (x (copy-list '(a b c d)))
	 (xcopy (make-scaffold-copy x)))
    (let ((result
	   (mapcar #'(lambda (y) (declare (ignore y)) (incf n))
		   x)))
      (and (check-scaffold-copy x xcopy)
	   result)))
  (1 2 3 4))

(deftest mapcar.4
  (let* ((n 0)
	 (x (copy-list '(a b c d)))
	 (xcopy (make-scaffold-copy x))
	 (x2 (copy-list '(a b c d e f)))
	 (x2copy (make-scaffold-copy x2))
	 (result
	  (mapcar #'(lambda (y z) (declare (ignore y z)) (incf n))
		  x x2)))
    (and (check-scaffold-copy x xcopy)
	 (check-scaffold-copy x2 x2copy)
	 (list result n)))
  ((1 2 3 4) 4))
  
(deftest mapcar.5
  (let* ((n 0)
	 (x (copy-list '(a b c d)))
	 (xcopy (make-scaffold-copy x))
	 (x2 (copy-list '(a b c d e f)))
	 (x2copy (make-scaffold-copy x2))
	 (result
	  (mapcar #'(lambda (y z) (declare (ignore y z)) (incf n))
		  x2 x)))
    (and (check-scaffold-copy x xcopy)
	 (check-scaffold-copy x2 x2copy)
	 (list result n)))
  ((1 2 3 4) 4))

(deftest mapcar.6
 (let* ((x (copy-list '(a b c d e f g h)))
	 (xcopy (make-scaffold-copy x)))
    (setf *mapc.6-var* nil)
    (let ((result (mapcar 'mapc.6-fun x)))
      (and (check-scaffold-copy x xcopy)
	   (list *mapc.6-var* result))))
 ((h g f e d c b a) (a b c d e f g h)))

(deftest mapcar.order.1
  (let ((i 0) x y z)
    (values
     (mapcar (progn (setf x (incf i))
		    #'list)
	     (progn (setf y (incf i))
		    '(a b c))
	     (progn (setf z (incf i))
		    '(1 2 3)))
     i x y z))
  ((a 1) (b 2) (c 3))
  3 1 2 3)

(def-fold-test mapcar.fold.1 (mapcar 'identity '(a b c d)))
(def-fold-test mapcar.fold.2 (mapcar 'not '(t nil nil t t)))

;;; Error tests

(deftest mapcar.error.1
  (check-type-error #'(lambda (x) (mapcar #'identity x)) #'listp)
  nil)

(deftest mapcar.error.2
  (signals-error (mapcar) program-error)
  t)

(deftest mapcar.error.3
  (signals-error (mapcar #'append) program-error)
  t)

(deftest mapcar.error.4
  (signals-error (locally (mapcar #'identity 1) t) type-error)
  t)

(deftest mapcar.error.5
  (signals-error (mapcar #'car '(a b c)) type-error)
  t)

(deftest mapcar.error.6
  (signals-error (mapcar #'cons '(a b c)) program-error)
  t)

(deftest mapcar.error.7
  (signals-error (mapcar #'cons '(a b c) '(1 2 3) '(4 5 6)) program-error)
  t)

(deftest mapcar.error.8
  (signals-error (mapcar #'identity (list* 1 2 3 4)) type-error)
  t)
