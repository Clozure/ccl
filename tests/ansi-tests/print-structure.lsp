;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed May 26 22:19:52 2004
;;;; Contains: Printing tests for structures

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(defstruct print-struct-1
  foo bar)

(deftest print-structure.1
  (let ((s (make-print-struct-1 :foo 1 :bar 2)))
    (with-standard-io-syntax
     (let ((*package* (find-package "CL-TEST")))
       (let ((str (write-to-string s :readably nil :case :upcase :escape nil)))
	 (assert (string= (subseq str 0 3) "#S("))
	 (let ((vals (read-from-string (subseq str 2))))
	   (assert (listp vals))
	   (assert (= (length vals) 5))
	   (assert (eq (car vals) 'print-struct-1))
	   (assert (symbolp (cadr vals)))
	   (assert (symbolp (cadddr vals)))
	   (cond
	    ((string= (symbol-name (cadr vals)) "FOO")
	     (assert (string= (symbol-name (cadddr vals)) "BAR"))
	     (assert (= (caddr vals) 1))
	     (assert (= (car (cddddr vals)) 2)))
	    (t
	     (assert (string= (symbol-name (cadr vals)) "BAR"))
	     (assert (string= (symbol-name (cadddr vals)) "FOO"))
	     (assert (= (caddr vals) 2))
	     (assert (= (car (cddddr vals)) 1))))
	   nil)))))
  nil)
