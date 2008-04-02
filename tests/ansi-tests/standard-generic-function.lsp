;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May 20 06:47:20 2003
;;;; Contains: Additional tests for class STANDARD-GENERIC-FUNCTION

(in-package :cl-test)

;;; Most tests of this are elsewhere

(unless (typep #'cons 'generic-function)

(deftest standard-generic-function.1
  (progn
    (eval
     '(defgeneric sgf-cpl-gf.1 (x)
	(:method ((x generic-function)) 1)
	(:method ((x function)) 2)
	(:method ((x t)) 3)))
    (values
     (sgf-cpl-gf.1 #'make-instance)
     (sgf-cpl-gf.1 #'cons)
     (sgf-cpl-gf.1 'a)))
  1 2 3)

(deftest standard-generic-function.2
  (progn
    (eval
     '(defgeneric sgf-cpl-gf.2 (x)
	(:method ((x standard-generic-function)) 1)
	(:method ((x function)) 2)
	(:method ((x t)) 3)))
    (values
     (sgf-cpl-gf.2 #'make-instance)
     (sgf-cpl-gf.2 #'cons)
     (sgf-cpl-gf.2 'a)))
  1 2 3)


)




      
  
