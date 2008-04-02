;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 23:22:46 2003
;;;; Contains: Tests for LOGTEST

(in-package :cl-test)

;;; Error tests

(deftest logtest.error.1
  (signals-error (logtest) program-error)
  t)

(deftest logtest.error.2
  (signals-error (logtest 0) program-error)
  t)

(deftest logtest.error.3
  (signals-error (logtest 0 0 nil) program-error)
  t)

(deftest logtest.error.4
  (check-type-error #'(lambda (x) (logtest x -1)) #'integerp)
  nil)

(deftest logtest.error.5
  (check-type-error #'(lambda (x) (logtest -1 x)) #'integerp)
  nil)

;;; Non-error tests

(deftest logtest.1
  (loop for x = (logand (random-fixnum) (random-fixnum))
	for y = (logand (random-fixnum) (random-fixnum))
	repeat 10000
	unless (if (logtest x y)
		   (not (zerop (logand x y)))
		 (zerop (logand x y)))
	collect (list x y))
  nil)

(deftest logtest.2
  (logtest 1 2)
  nil)

(deftest logtest.3
  (notnot-mv (logtest 8 (logior 8 4)))
  t)
  

  
