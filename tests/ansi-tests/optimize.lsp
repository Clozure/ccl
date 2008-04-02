;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 09:31:34 2005
;;;; Contains: Tests of the OPTIMIZE declaration

(in-package :cl-test)

(deftest optimize.1
  (locally (declare (optimize)) nil)
  nil)

(deftest optimize.2
  (locally (declare (optimize speed)) nil)
  nil)

(deftest optimize.3
  (locally (declare (optimize space)) nil)
  nil)

(deftest optimize.4
  (locally (declare (optimize safety)) nil)
  nil)

(deftest optimize.5
  (locally (declare (optimize debug)) nil)
  nil)

(deftest optimize.6
  (locally (declare (optimize compilation-speed)) nil)
  nil)

(deftest optimize.7
  (loop for d in '(speed space safety debug compilation-speed)
	nconc (loop for n from 0 to 3
		    for form = `(locally (declare (optimize (,d ,n))) t)
		    for val = (eval form)
		    unless (eql val t)
		    collect (list d n val)))
  nil)

(deftest optimize.8
  (loop for d in '(speed space safety debug compilation-speed)
	nconc (loop for n from 0 to 3
		    for form = `(lambda ()
				  (declare (optimize (,d ,n)))
				  t)
		    for val = (funcall (compile nil form))
		    unless (eql val t)
		    collect (list d n val)))
  nil)



		 