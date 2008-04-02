;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 07:59:24 2005
;;;; Contains: Tests of the IGNORE declarations

(in-package :cl-test)

(deftest ignore.1
  (let ((x 'foo)) (declare (ignore x)))
  nil)

(deftest ignore.2
  (let ((x 'foo)) (declare (ignore x)) x)
  foo)

(deftest ignore.3
  (flet ((%f () 'foo))
    (declare (ignore (function %f))))
  nil)
    
(deftest ignore.4
  (flet ((%f () 'foo))
    (declare (ignore (function %f)))
    (%f))
  foo)

(deftest ignore.5
  (flet (((setf %f) (x y) (setf (car y) x)))
	(declare (ignore (function (setf %f))))
	:good)
  :good)

(deftest ignore.6
  (labels (((setf %f) (x y) (setf (car y) x)))
	  (declare (ignore (function (setf %f))))
	  :good)
  :good)
