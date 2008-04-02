;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Apr 20 22:10:53 2004
;;;; Contains: Tests for printing of bit vectors

(compile-and-load "printer-aux.lsp")

(in-package :cl-test)

(deftest print.bit-vector.1
  (with-standard-io-syntax
   (write-to-string #* :readably nil :escape nil))
  "#*")

(deftest print.bit-vector.2
  (with-standard-io-syntax
   (subseq (write-to-string #* :readably nil :escape nil :array nil)
	   0 2))
  "#<")

(deftest print.bit-vector.3
  (with-standard-io-syntax
   (write-to-string #*001101010011011 :readably nil :escape nil))
  "#*001101010011011")

(deftest print.bit-vector.4
  (with-standard-io-syntax
   (subseq (write-to-string #*11010011010110101
			    :readably nil :escape nil :array nil)
	   0 2))
  "#<")

(deftest print.bit-vector.5
  (let* ((bv1 #*0001100101)
	 (bv2 (make-array 5 :displaced-to bv1
			  :displaced-index-offset 1
			  :element-type 'bit)))
    (with-standard-io-syntax
     (write-to-string bv2 :readably nil :escape nil)))
  "#*00110")

(deftest print.bit-vector.6
  (let* ((bv (make-array 10
			 :element-type 'bit
			 :initial-contents '(1 0 0 1 0 0 1 1 1 0)
			 :fill-pointer 5)))
    (with-standard-io-syntax
     (write-to-string bv :readably nil :escape nil)))
  "#*10010")


(deftest print.bit-vector.7
  (let* ((bv (make-array 10
			 :element-type 'bit
			 :initial-contents '(1 0 0 1 0 0 1 1 1 0)
			 :adjustable t)))
    (with-standard-io-syntax
     (write-to-string bv :readably nil :escape nil)))
  "#*1001001110")



(deftest print.bit-vector.random
  (loop
   for len = (random 100)
   for bv = (coerce (loop repeat len collect (random 2)) 'bit-vector)
   repeat 1000
   nconc (randomly-check-readability bv))
  nil)
