;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 06:38:33 2002
;;;; Contains: Tests of NOT and NULL

(in-package :cl-test)

(deftest null.1
  (null nil)
  t)

(deftest null.2
  (null t)
  nil)

(deftest null.3
  (some #'(lambda (x) (and x (null x))) *universe*)
  nil)

(deftest null.4
    (not (some #'null
	       `(1 a 1.2 "a" #\w (a) ,*terminal-io*
		   #'car (make-array '(10)))))
  t)

(deftest null.error.1
  (signals-error (null) program-error)
  t)

(deftest null.error.2
  (signals-error (null nil nil) program-error)
  t)

(deftest not.1
  (not nil)
  t)

(deftest not.2
  (not t)
  nil)

(deftest not.3
  (some #'(lambda (x) (and x (not x))) *universe*)
  nil)

(deftest not.4
    (not (some #'not
	       `(1 a 1.2 "a" #\w (a) ,*terminal-io*
		   #'car (make-array '(10)))))
  t)


(deftest not.error.1
  (signals-error (not) program-error)
  t)

(deftest not.error.2
  (signals-error (not nil nil) program-error)
  t)
