;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 08:16:27 2005
;;;; Contains: Tests of the IGNORABLE declaration

(in-package :cl-test)

(deftest ignorable.1
  (let ((x 'foo)) (declare (ignorable x)))
  nil)

(deftest ignorable.2
  (let ((x 'foo)) (declare (ignorable x)) x)
  foo)

(deftest ignorable.3
  (flet ((%f () 'foo))
    (declare (ignorable (function %f))))
  nil)
    
(deftest ignorable.4
  (flet ((%f () 'foo))
    (declare (ignorable (function %f)))
    (%f))
  foo)

;;; TODO: add a test for (function (setf foo))

(deftest ignorable.5
  (flet (((setf %f) (x y) nil))
	(declare (ignorable (function (setf %f))))
	:good)
  :good)

(deftest ignorable.6
  (flet (((setf %f) (x y) (setf (car y) x)))
	(declare (ignorable (function (setf %f))))
	(let ((z (cons 'a 'b)))
	  (values (setf (%f z) 'c) z)))
  c (c . b))

(deftest ignorable.7
  (labels (((setf %f) (x y) nil))
	  (declare (ignorable (function (setf %f))))
	  :good)
  :good)

(deftest ignorable.8
  (labels (((setf %f) (x y) (setf (car y) x)))
	  (declare (ignorable (function (setf %f))))
	  (let ((z (cons 'a 'b)))
	    (values (setf (%f z) 'c) z)))
  c (c . b))



