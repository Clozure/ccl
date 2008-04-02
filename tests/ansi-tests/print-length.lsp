;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 27 08:27:37 2004
;;;; Contains: Tests involving *PRINT-LENGTH*

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-print-test print-length.1
  '(1)
  "(...)"
  (*print-length* 0))

(def-print-test print-length.2
  '(1)
  "(1)"
  (*print-length* nil))

(def-print-test print-length.3
  '(1)
  "(1)"
  (*print-length* 1))

(def-print-test print-length.4
  '(1 . 2)
  "(1 . 2)"
  (*print-length* 1))

(deftest print-length.5
  (let ((x '(|A| |B| |C| |D| |E| |F|)))
    (with-standard-io-syntax
     (let ((*print-case* :upcase)
	   (*print-escape* nil)
	   (*print-readably* nil)
	   (*print-pretty* nil)
	   (*print-length* nil))
       (apply
	#'values
	(loop for i from 0 to 8
	      collect (let ((*print-length* i))
			(write-to-string x)))))))
  "(...)"
  "(A ...)"
  "(A B ...)"
  "(A B C ...)"
  "(A B C D ...)"
  "(A B C D E ...)"
  "(A B C D E F)"
  "(A B C D E F)"
  "(A B C D E F)")

(deftest print-length.6
  (let ((x '(|A| |B| |C| |D| |E| |F| . |G|)))
    (with-standard-io-syntax
     (let ((*print-case* :upcase)
	   (*print-escape* nil)
	   (*print-readably* nil)
	   (*print-pretty* nil)
	   (*print-length* nil))
       (apply
	#'values
	(loop for i from 0 to 8
	      collect (let ((*print-length* i))
			(write-to-string x)))))))
  "(...)"
  "(A ...)"
  "(A B ...)"
  "(A B C ...)"
  "(A B C D ...)"
  "(A B C D E ...)"
  "(A B C D E F . G)"
  "(A B C D E F . G)"
  "(A B C D E F . G)")

(def-print-test print-length.7
  '(1)
  "(1)"
  (*print-length* (1+ most-positive-fixnum)))

(deftest print-length.8
  (let ((x #(|A| |B| |C| |D| |E| |F|)))
    (with-standard-io-syntax
     (let ((*print-case* :upcase)
	   (*print-escape* nil)
	   (*print-readably* nil)
	   (*print-pretty* nil)
	   (*print-length* nil))
       (apply
	#'values
	(loop for i from 0 to 8
	      collect (let ((*print-length* i))
			(write-to-string x)))))))
  "#(...)"
  "#(A ...)"
  "#(A B ...)"
  "#(A B C ...)"
  "#(A B C D ...)"
  "#(A B C D E ...)"
  "#(A B C D E F)"
  "#(A B C D E F)"
  "#(A B C D E F)")

(def-print-test print-length.9
  "A modest sentence with six words."
  "\"A modest sentence with six words.\""
  (*print-length* 0))

(def-print-test print-length.10
  #*00110101100011
  "#*00110101100011"
  (*print-length* 0))

(defstruct print-length-struct foo)

;;; The next test tacitly assumes issue STRUCTURE-READ-PRINT-SYNTAX

(deftest print-length.11
  (let ((result
	 (with-standard-io-syntax
	  (let ((*print-case* :upcase)
		(*print-escape* nil)
		(*print-readably* nil)
		(*print-pretty* nil)
		(*print-length* nil)
		(*package* (find-package "CL-TEST"))
		(s (make-print-length-struct :foo 17)))
	    (apply
	     #'list
	     (loop for i from 0 to 4
		   collect (let ((*print-length* i))
			     (write-to-string s))))))))
    (if (member result
		'(("#S(...)"
		   "#S(PRINT-LENGTH-STRUCT ...)"
		   "#S(PRINT-LENGTH-STRUCT :FOO ...)"
		   "#S(PRINT-LENGTH-STRUCT :FOO 17)"
		   "#S(PRINT-LENGTH-STRUCT :FOO 17)")
		  ("#S(PRINT-LENGTH-STRUCT ...)"
		   "#S(PRINT-LENGTH-STRUCT :FOO 17)"
		   "#S(PRINT-LENGTH-STRUCT :FOO 17)"
		   "#S(PRINT-LENGTH-STRUCT :FOO 17)"
		   "#S(PRINT-LENGTH-STRUCT :FOO 17)"))
		:test 'equal)
	:good
      result))
  :good)
