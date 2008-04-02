;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan 22 03:02:31 2004
;;;; Contains: Tests of FILE-POSITION

(in-package :cl-test)

(deftest file-position.1
  (with-open-file (is "file-position.lsp":direction :input)
		  (file-position is))
  0)

(deftest file-position.2
  (with-open-file (is "file-position.lsp":direction :input)
		  (values
		   (multiple-value-list
		    (notnot-mv (file-position is :start)))
		   (file-position is)))
			      
  (t) 0)

(deftest file-position.3
  (with-open-file (is "file-position.lsp":direction :input)
		  (values
		   (multiple-value-list
		    (notnot-mv (file-position is :end)))
		   (notnot (> (file-position is) 0))))
  (t) t)

(deftest file-position.4
  (with-open-file
   (is "file-position.lsp":direction :input)
   (values
    (file-position is)
    (read-char is)
    (notnot (> (file-position is) 0))))
  0 #\; t)

(deftest file-position.5
  (with-open-file
   (os "tmp.dat":direction :output
       :if-exists :supersede)
   (values
    (file-position os)
    (write-char #\x os)
    (notnot (> (file-position os) 0))))
  0 #\x t)

(deftest file-position.6
  (with-open-file
   (os "tmp.dat":direction :output
       :if-exists :supersede)
   (let ((p1 (file-position os))
	 (delta (file-string-length os #\x)))
     (write-char #\x os)
     (let ((p2 (file-position os)))
       (or (null p1) (null p2) (null delta)
	   (=t (+ p1 delta) p2)))))
  t)

;;; Byte streams

(deftest file-position.7
  (loop for len from 1 to 32
	for n = (ash 1 len)
	do (with-open-file
	    (os "tmp.dat" :direction :output
		:if-exists :supersede
		:element-type `(unsigned-byte ,len))
	    (loop for i from 0 below 100
		  for r = (logand (1- n) i)
		  for pos = (file-position os)
		  do (assert (or (not pos) (eql pos i)))
		  do (write-byte r os)))
	do (with-open-file
	    (is "tmp.dat" :direction :input
		:element-type `(unsigned-byte ,len))
	    (loop for i from 0 below 100
		  for pos = (file-position is)
		  do (assert (or (not pos) (eql pos i)))
		  do (let ((byte (read-byte is)))
		       (assert (eql byte (logand (1- n) i)))))))
  nil)

(deftest file-position.8
  (loop for len from 33 to 100
	for n = (ash 1 len)
	do (with-open-file
	    (os "tmp.dat" :direction :output
		:if-exists :supersede
		:element-type `(unsigned-byte ,len))
	    (loop for i from 0 below 100
		  for r = (logand (1- n) i)
		  for pos = (file-position os)
		  do (assert (or (not pos) (eql pos i)))
		  do (write-byte r os)))
	do (with-open-file
	    (is "tmp.dat" :direction :input
		:element-type `(unsigned-byte ,len))
	    (loop for i from 0 below 100
		  for pos = (file-position is)
		  do (assert (or (not pos) (eql pos i)))
		  do (let ((byte (read-byte is)))
		       (assert (eql byte (logand (1- n) i)))))))
  nil)

(deftest file-position.9
  (with-input-from-string
   (s "abcdefghijklmnopqrstuvwxyz")
   (loop repeat 26
	 for p = (file-position s)
	 unless (or (not p)
		    (progn
		      (file-position s p)
		      (eql (file-position s) p)))
	 collect p
	 do (read-char s)))
  nil)

(deftest file-position.10
  (with-output-to-string
   (s)
   (loop repeat 26
	 for p = (file-position s)
	 unless (or (not p)
		    (progn
		      (file-position s p)
		      (eql (file-position s) p)))
	 collect p
	 do (write-char #\x s)))
  "xxxxxxxxxxxxxxxxxxxxxxxxxx")

;;; Error tests

(deftest file-position.error.1
  (signals-error (file-position) program-error)
  t)

(deftest file-position.error.2
  (signals-error
   (file-position (make-string-input-stream "abc") :start nil)
   program-error)
  t)

;;; It's not clear what 'too large' means -- can we set the
;;; file position to a point where the file may later be extended
;;; by some other writer?
#|
(deftest file-position.error.3
  (signals-error
   (with-open-file
    (is "file-position.lsp" :direction :input)
    (flet ((%fail () (error 'type-error)))
      (unless (file-position is :end) (%fail))
      (let ((fp (file-position is)))
	(unless fp (%fail))
	(file-position is (+ 1000000 fp)))))
   error)
  t)

(deftest file-position.error.4
  (signals-error
   (with-open-file
    (is "file-position.lsp" :direction :input)
    (file-position is 1000000000000000000000))
   error)
  t)
|#

  