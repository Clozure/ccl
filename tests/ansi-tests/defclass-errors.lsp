;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Apr 25 06:59:22 2003
;;;; Contains: Error case tests for DEFCLASS

(in-package :cl-test)

;;; I created some redundant tests by accident.  This list of
;;; tests could be reduced in size.

(deftest defclass.error.1
  (signals-error
   (defclass erroneous-class.1 ()
     (a b c d b e))
   program-error)
  t)

(deftest defclass.error.2
  (signals-error
   (defclass erroneous-class.2 ()
     ((s1 :initarg :foo))
     (:default-initargs :foo 1 :foo 2))
   program-error)
  t)

(deftest defclass.error.3
  (signals-error
   (defclass erroneous-class.3 ()
     ((s1 :initform 0 :initform 2)))
   program-error)
  t)

(deftest defclass.error.4
  (signals-error
   (defclass erroneous-class.4 ()
     ((s1 :initform 0 :initform 0)))
   program-error)
  t)

(deftest defclass.error.5
  (signals-error
   (defclass erroneous-class.5 ()
     ((s1 :type fixnum :type character)))
   program-error)
  t)

(deftest defclass.error.6
  (signals-error
   (defclass erroneous-class.6 ()
     ((s1 :type t :type t)))
   program-error)
  t)

(deftest defclass.error.7
  (signals-error
   (defclass erroneous-class.7 ()
     ((s1 :documentation "foo" :documentation "bar")))
   program-error)
  t)

(deftest defclass.error.8
  (signals-error
   (defclass erroneous-class.8 ()
     ((s1 :documentation #1="foo" :documentation #1#)))
   program-error)
  t)

(deftest defclass.error.9
  (signals-error
   (defclass erroneous-class.9 ()
     ((s1 :allocation :class :allocation :instance)))
   program-error)
  t)

(deftest defclass.error.10
  (signals-error
   (defclass erroneous-class.10 ()
     ((s1 :allocation :class :allocation :class)))
   program-error)
  t)

(deftest defclass.error.11
  (signals-error
   (defclass erroneous-class.11 ()
     ((s1 :allocation :instance :allocation :instance)))
   program-error)
  t)

(deftest defclass.error.12
  (signals-error
   (defclass erroneous-class.12 ()
     ((s1 #.(gensym) nil)))
   program-error)
  t)

(deftest defclass.error.13
  (signals-error
   (defclass erroneous-class.13 ()
     (a b c)
     (#.(gensym)))
   program-error)
  t)

(deftest defclass.error.14
  (signals-error
   (defclass defclass-error-14 nil
     (foo foo))
   program-error)
  t)

(deftest defclass.error.15
  (signals-error
   (defclass defclass-error-15 nil
     (foo (foo)))
   program-error)
  t)

(deftest defclass.error.16
  (signals-error
   (defclass defclass-error-16 nil
     ((foo :initarg f1))
     (:default-initargs :f1 10 :f1 20))
   program-error)
  t)

(deftest defclass.error.17
  (signals-error
   (defclass defclass-error-17 nil
     ((foo :initform 10 :initform 20 :reader defclass-error-4/foo)))
   program-error)
  t)

(deftest defclass.error.18
  (signals-error
   (defclass defclass-error-18 nil
     ((foo :initform 10 :initform 10 :reader defclass-error-5/foo)))
   program-error)
  t)

(deftest defclass.error.19
  (signals-error
   (defclass defclass-error-19 nil
     ((foo :initarg f1 :type t :type t :reader defclass-error-6/foo)))
   program-error)
  t)

(deftest defclass.error.20
  (signals-error
   (defclass defclass-error-20 nil
     ((foo :initarg f1 :documentation "x" :reader defclass-error-7/foo
	   :documentation "x")))
   program-error)
  t)

(deftest defclass.error.21
  (signals-error
   (defclass defclass-error-21 ()
     ((foo #:unknown-slot-option nil)))
   program-error)
  t)

(deftest defclass.error.22
  (let ((option (gentemp "UNKNOWN-OPTION" (symbol-package :foo))))
    (eval
     `(signals-error
       (defclass defclass-error-22 ()
	 (foo bar)
	 (,option nil))
       program-error)))
  t)

(deftest defclass.error.23
  (loop for cl in *built-in-classes*
	for name = (class-name cl)
	unless (or (not name)
		   (handler-case
		    (progn (eval `(defclass ,(gensym) (,name))) nil)
		    (error (c) c)))
	collect (list cl name))
  nil)

(deftest defclass.error.24
  (loop for cl in *built-in-classes*
	for name = (class-name cl)
	unless (or (not name)
		   (handler-case
		    (progn (eval `(defclass ,name ())) nil)
		    (error (c) c)))
	collect (list cl name))
  nil)

