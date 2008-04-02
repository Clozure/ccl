;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May 25 08:22:03 2004
;;;; Contains: Printer tests for pathnames

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest print.pathname.1
  (loop for p in *universe*
	when (typep p 'pathname)
	nconc
	(loop repeat 10
	      nconc (randomly-check-readability p :test #'is-similar
						:can-fail t)))
  nil)

(deftest print.pathname.2
  (loop for p in *universe*
	when (typep p 'pathname)
	nconc
	(let ((ns (ignore-errors (namestring p))))
	  "Read 22.1.3.11 before commenting on this test"
	  (when ns
	    (let ((expected-result
                   (concatenate 'string "#P"
                                (with-standard-io-syntax
				 (write-to-string ns :readably nil
						  :escape t))))
		  (result (with-standard-io-syntax
			   (write-to-string p :readably nil :escape t))))
	      (unless (string= expected-result result)
		(list (list expected-result result)))))))
  nil)
