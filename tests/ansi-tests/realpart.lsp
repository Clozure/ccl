;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 07:41:15 2003
;;;; Contains: Tests of REALPART

(in-package :cl-test)

(deftest realpart.error.1
  (signals-error (realpart) program-error)
  t)

(deftest realpart.error.2
  (signals-error (realpart #c(1.0 2.0) nil) program-error)
  t)

(deftest realpart.error.3
  (check-type-error #'realpart #'numberp)
  nil)

(deftest realpart.1
  (loop for x in *reals*
	for c = (complex x 0)
	for rp = (realpart c)
	unless (eql x rp)
	collect (list x c rp))
  nil)

(deftest realpart.2
  (loop for x in *reals*
	for c = (complex x 1)
	for rp = (realpart c)
	unless (eql x rp)
	collect (list x c rp))
  nil)

(deftest realpart.3
  (loop for x in *reals*
	for c = (complex x x)
	for rp = (realpart c)
	unless (eql x rp)
	collect (list x c rp))
  nil)

;;; Should move this to complex.lsp
(deftest realpart.4
  (loop for c in *complexes*
	for rp = (realpart c)
	for ip = (imagpart c)
	for c2 = (complex rp ip)
	unless (eql c c2)
	collect (list c rp ip c2))
  nil)
