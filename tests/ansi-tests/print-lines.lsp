;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 27 09:32:46 2004
;;;; Contains: Tests involving PRINT-LINES

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest print-lines.1
  *print-lines*
  nil)

(deftest print-lines.2
  (with-standard-io-syntax
   (let ((*print-lines* 1)
	 (*print-readably* nil)
	 (*print-miser-width* nil)
	 (*print-pprint-dispatch* (copy-pprint-dispatch)))
     (set-pprint-dispatch '(cons (eql 1) t) 'pprint-fill)
     (apply
      #'values
      (loop for i from 1 to 10
	    collect
	    (let ((*print-right-margin* i))
	      (subseq
	       (with-output-to-string
		 (*standard-output*)
		 (terpri)
		 (pprint '(1 2 3 4 5 6 7 8 9)))
	       2))))))
  "(1 ..)"
  "(1 ..)"
  "(1 ..)"
  "(1 ..)"
  "(1 ..)"
  "(1 ..)"
  "(1 ..)"
  "(1 2 ..)"
  "(1 2 ..)"
  "(1 2 3 ..)")

