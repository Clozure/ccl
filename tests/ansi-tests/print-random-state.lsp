;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May 25 07:15:02 2004
;;;; Contains: Tests of printing random states

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest print.random-state.1
  (loop repeat 100
	do (loop repeat 50 do (random 1000))
	nconc
	(let* ((rs1 (make-random-state *random-state*))
	       (rs2 (with-standard-io-syntax
		     (read-from-string
		      (write-to-string rs1 :readably t))))
	       (result (list (notnot (random-state-p rs2))
			     (is-similar rs1 rs2))))
	  (unless (equal result '(t t)) (list result rs1 rs2))))
  nil)
