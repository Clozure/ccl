;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  1 14:08:07 2003
;;;; Contains: Tests of HANDLER-CASE

(in-package :cl-test)

(deftest handler-case.1
  (handler-case
   (error "an error")
   (error () t))
  t)

(deftest handler-case.2
  (handler-case
   (error "an error")
   (warning () nil)
   (error () t))
  t)

(deftest handler-case.3
  (handler-case
   (error "an error")
   (error (c) (and (typep c 'error) t))
   (error () 'bad)
   (condition () 'bad2))
  t)

(deftest handler-case.4
  (handler-case
   (error "an error")
   (warning (c) c)
   (error (c) (and (typep c 'error) t))
   (error () 'bad)
   (condition () 'bad2))
  t)

(deftest handler-case.5
  (handler-case
   (error "an error")
   (#.(find-class 'error) (c) (and (typep c 'error) t))
   (error () 'bad))
  t)

(deftest handler-case.6
  (handler-case (values)
		(error () nil)))

(deftest handler-case.7
  (handler-case 'foo (condition () 'bar))
  foo)

;;; (deftest handler-case.8
;;;  (handler-case 'foo (t () 'bar))
;;;  foo)

(deftest handler-case.9
  (handler-case (values 1 2 3 4 5 6 7 8) (condition () nil))
  1 2 3 4 5 6 7 8)

;;; (deftest handler-case.10
;;;  (handler-case
;;;   (error "foo")
;;;   (t () 'good))
;;;  good)

(deftest handler-case.11
  (labels ((%f () (declare (special *c*))
	       (and (typep *c* 'condition) t))
	   (%g ()
	       (let ((*c* nil))
		 (declare (special *c*))
		 (%h)))
	   (%h ()
	    (handler-case
	     (error "foo")
	     (error (*c*) (declare (special *c*))
		    (%f)))))
    (%g))
  t)

(deftest handler-case.12
  (handler-case (error "foo")
		(nil () nil)
		(error (c) (notnot-mv (typep c 'simple-error))))
  t)

(deftest handler-case.13
  (handler-case (error "foo")
		(error (c) (values))))

(deftest handler-case.14
  (handler-case (error "foo")
		(error (c)
		       (values 1 2 3 4 5 6 7 8)))
  1 2 3 4 5 6 7 8)

(deftest handler-case.15
  (handler-case
   (handler-case (error "foo")
		 (warning () 'bad))
   (error () 'good))
  good)

(deftest handler-case.16
  (handler-case
   (handler-case (error "foo")
		 (error () 'good))
   (error () 'bad))
  good)

(deftest handler-case.17
  (let ((i 0))
    (values
     (handler-case
      (handler-case (error "foo")
		    (error () (incf i) (error "bar")))
      (error () 'good))
     i))
  good 1)

(deftest handler-case.18
  (let ((i 0))
    (values
     (handler-case
      (handler-case (error "foo")
		    (error (c) (incf i) (error c)))
      (error () 'good))
     i))
  good 1)

(deftest handler-case.19
  (handler-case
   (error "foo")
   (error (c)
	  ;; Test that declarations can go here
	  (declare (optimize (safety 3)))
	  (declare (type condition c))
	  (declare (ignore c))
	  t))
  t)

(deftest handler-case.20
  (handler-case
   10
   (:no-error (x) (+ x 3)))
  13)

(deftest handler-case.21
  (handler-case
   (values)
   (:no-error () 'foo))
  foo)

(deftest handler-case.22
  (handler-case
   (values 1 2 3 4 5)
   (:no-error (a b c d e) (list e d c b a)))
  (5 4 3 2 1))

(deftest handler-case.23
  (signals-error
   (handler-case (values 1 2) (:no-error (x) x))
   program-error)
  t)

(deftest handler-case.24
  (signals-error
   (handler-case (values) (:no-error (x) x))
   program-error)
  t)

(deftest handler-case.25
  (handler-case
   (handler-case
    (values)
    (error () 'bad)
    (:no-error () (error "foo")))
   (error () 'good))
  good)

(deftest handler-case.26
  (handler-case
   (values 1 'a 1.0)
   (error () 'bad)
   (:no-error (a b c)
	      ;; Test that declarations can go here
	      (declare (type integer a))
	      (declare (type symbol b))
	      (declare (type number c))
	      (declare (ignore a c))
	      b))
  a)

(deftest handler-case.27
  (handler-case (error "foo") (error ()))
  nil)

(deftest handler-case.28
  (handler-case (error "foo") (error () (declare (optimize speed))))
  nil)

;;; Free declaration scope

(deftest handler-case.29
  (let ((x :bad))
    (declare (special x))
    (let ((x :good))
      (handler-case nil
		    (:no-error (z &aux (y x))
			       (declare (special x) (ignore z))
			       y))))
  :good)
