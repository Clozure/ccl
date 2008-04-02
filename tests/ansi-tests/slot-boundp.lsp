;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May  6 05:53:32 2003
;;;; Contains: Tests of SLOT-BOUNDP

(in-package :cl-test)

;;; SLOT-BOUNDP is extensively tested in other files as well

(defclass slot-boundp-class-01 ()
  (a (b :initarg :b) (c :initform 'x)))

(deftest slot-boundp.1
  (let ((obj (make-instance 'slot-boundp-class-01)))
    (slot-boundp obj 'a))
  nil)

(deftest slot-boundp.2
  (let ((obj (make-instance 'slot-boundp-class-01)))
    (setf (slot-value obj 'a) nil)
    (notnot-mv (slot-boundp obj 'a)))
  t)

(deftest slot-boundp.3
  (let ((obj (make-instance 'slot-boundp-class-01 :b nil)))
    (notnot-mv (slot-boundp obj 'b)))
  t)

(deftest slot-boundp.4
  (let ((obj (make-instance 'slot-boundp-class-01)))
    (notnot-mv (slot-boundp obj 'c)))
  t)

(deftest slot-boundp.5
  (let ((obj (make-instance 'slot-boundp-class-01)))
    (slot-makunbound obj 'c)
    (slot-boundp obj 'c))
  nil)

;;; Argument order test(s)

(deftest slot-boundp.order.1
  (let ((obj (make-instance 'slot-boundp-class-01))
	(i 0) x y)
    (values
     (slot-boundp (progn (setf x (incf i)) obj)
		  (progn (setf y (incf i)) 'a))
     i x y))
  nil 2 1 2)

;;; Error tests

(deftest slot-boundp.error.1
  (signals-error (slot-boundp) program-error)
  t)

(deftest slot-boundp.error.2
  (signals-error (let ((obj (make-instance 'slot-boundp-class-01)))
		    (slot-boundp obj))
		 program-error)
  t)

(deftest slot-boundp.error.3
  (signals-error (let ((obj (make-instance 'slot-boundp-class-01)))
		    (slot-boundp obj 'a nil))
		 program-error)
  t)

(deftest slot-boundp.error.4
  (signals-error
   (let ((obj (make-instance 'slot-boundp-class-01)))
     (slot-boundp obj 'nonexistent-slot))
   error)
  t)

;;; SLOT-BOUNDP should signal an error on elements of built-in-classes
(deftest slot-boundp.error.5
  (let ((built-in-class (find-class 'built-in-class)))
    (loop for e in *mini-universe*
	  for class = (class-of e)
	  when (and (eq (class-of class) built-in-class)
		    (handler-case (progn (slot-boundp e 'foo) t)
				  (error () nil)))
	  collect e))
  nil)
