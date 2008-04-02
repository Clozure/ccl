;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 09:10:52 2005
;;;; Contains: Tests of DYNAMIC-EXTENT

(in-package :cl-test)

(deftest dynamic-extent.1
  (let () (declare (dynamic-extent)))
  nil)

(deftest dynamic-extent.2
  (let ((x 'a))
    (declare (dynamic-extent x) (optimize speed (safety 0)))
    x)
  a)

(deftest dynamic-extent.3
  (let ((x (list 'a 'b 'c)))
    (declare (dynamic-extent x) (optimize speed (safety 0)))
    (length x))
  3)

(deftest dynamic-extent.4
  (let ((x (vector 'a 'b 'c)))
    (declare (dynamic-extent x) (optimize speed (safety 0)))
    (length x))
  3)

(deftest dynamic-extent.5
  (flet ((%f (x) (list 'a x)))
    (declare (dynamic-extent (function %f))
	     (optimize speed (safety 0)))
    (mapcar #'%f '(1 2 3)))
  ((a 1) (a 2) (a 3)))

(deftest dynamic-extent.6
  (labels ((%f (x) (list 'a x)))
    (declare (dynamic-extent (function %f))
	     (optimize speed (safety 0)))
    (mapcar #'%f '(1 2 3)))
  ((a 1) (a 2) (a 3)))

(deftest dynamic-extent.7
  (labels ((%f (x) (if (consp x)
		       (cons (%f (car x)) (%f (cdr x)))
		     '*)))
    (declare (dynamic-extent (function %f))
	     (optimize speed (safety 0)))
    (mapcar #'%f '((1) 2 (3 4 5))))
  ((* . *) * (* * * . *)))

(deftest dynamic-extent.8
  (let ((x (+ most-positive-fixnum 2)))
    (declare (dynamic-extent x)
	     (optimize speed (safety 0)))
    (1- x))
  #.(1+ most-positive-fixnum))

(deftest dynamic-extent.9
  (flet ((f () (list 'a 'b)))
    (let ((f (list 'c 'd)))
      (declare (dynamic-extent (function f))
	       (optimize speed (safety 0)))
      f))
  (c d))

(deftest dynamic-extent.10
  (let ((x nil))
    (values
     x
     (locally (declare (dynamic-extent x) (notinline length)
		       (optimize speed (safety 0)))
	      (setq x (list 'a 'b 'c 'd 'e))
	      (prog1 (length x) (setq x t)))
     x))
  nil 5 t)

(deftest dynamic-extent.11
  (let* ((x (list 'a 'b))
	 (y (cons 'c x)))
    (declare (dynamic-extent y)
	     (optimize speed (safety 0)))
    (cdr y))
  (a b))

(deftest dynamic-extent.12
  (let* ((contents '(1 0 0 1 1 0 1 1 0 1))
	 (n (length contents)))
    (loop for i from 1 to 32
	  for type = `(unsigned-byte ,i)
	  for form1 = `(make-array '(,n) :initial-contents ',contents
				   :element-type ',type)
	  for form2 = `(let ((a ,form1))
			 (declare (dynamic-extent a))
			 (declare (type (simple-array ,type (,n))))
			 (declare (notinline coerce))
			 (declare (optimize speed (safety 0)))
			 (equal (coerce a 'list) ',contents))
	  unless (funcall (compile nil `(lambda () ,form2)))
	  collect i))
  nil)

(deftest dynamic-extent.13
  (let ((s (make-string 10 :initial-element #\a)))
    (declare (dynamic-extent s) (optimize speed (safety 0)))
    (notnot (every #'(lambda (c) (eql c #\a)) s)))
  t)

(deftest dynamic-extent.14
  (let ((s (make-string 10 :initial-element #\a
			:element-type 'base-char)))
    (declare (dynamic-extent s) (notinline every) (optimize speed (safety 0)))
    (notnot (every #'(lambda (c) (eql c #\a)) s)))
  t)

(deftest dynamic-extent.15
  (flet (((setf %f) (x y) (setf (car y) x)))
	(declare (dynamic-extent #'(setf %f)))
	:good)
  :good)

(deftest dynamic-extent.16
  (labels (((setf %f) (x y) (setf (car y) x)))
	  (declare (dynamic-extent #'(setf %f)))
	  :good)
  :good)


