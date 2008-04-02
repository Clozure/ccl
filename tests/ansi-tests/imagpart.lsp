;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 07:47:43 2003
;;;; Contains: Tests of IMAGPART

(in-package :cl-test)

(deftest imagpart.error.1
  (signals-error (imagpart) program-error)
  t)

(deftest imagpart.error.2
  (signals-error (imagpart #c(1.0 2.0) nil) program-error)
  t)

(deftest imagpart.error.3
  (check-type-error #'imagpart #'numberp)
  nil)

(deftest imagpart.1
  (loop for x in *reals*
	for c = (complex 0 x)
	for ip = (imagpart c)
	unless (eql x ip)
	collect (list x c ip))
  nil)

(deftest imagpart.2
  (loop for x in *reals*
	for c = (complex 1 x)
	for ip = (imagpart c)
	unless (eql x ip)
	collect (list x c ip))
  nil)

(deftest imagpart.3
  (loop for x in *reals*
	for c = (complex x x)
	for ip = (imagpart c)
	unless (eql x ip)
	collect (list x c ip))
  nil)

(deftest imagpart.4
  (loop for x in *reals*
	for ip = (imagpart x)
	unless (eql (* 0 x) ip)
	collect (list x ip (* 0 x)))
  nil)


