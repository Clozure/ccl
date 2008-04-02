;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 16:27:12 2003
;;;; Contains: Tests for FUNCTION-LAMBDA-EXPRESSION

(in-package :cl-test)

(deftest function-lambda-expression.1
  (length
   (multiple-value-list
    (function-lambda-expression #'cons)))
  3)

(deftest function-lambda-expression.2
  (let ((x nil))
    (flet ((%f () x))
      (let ((ret-vals
	     (multiple-value-list
	      (function-lambda-expression #'%f))))
	(values (length ret-vals)
		(notnot (second ret-vals))))))
  3 t)

;;; Verify that it doesn't barf on generic functions
(deftest function-lambda-expression.3
  (length
   (multiple-value-list
    (function-lambda-expression
     #'meaningless-user-generic-function-for-universe)))
  3)

(deftest function-lambda-expression.order.1
  (let ((i 0))
    (function-lambda-expression (progn (incf i) #'cons))
    i)
  1)

(deftest function-lambda-expression.error.1
  (signals-error (function-lambda-expression) program-error)
  t)

(deftest function-lambda-expression.error.2
  (signals-error (function-lambda-expression #'cons nil) program-error)
  t)
