;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 13 07:38:43 2004
;;;; Contains: Tests of SYMBOL-FUNCTION

(in-package :cl-test)

(deftest symbol-function.1
  (let ((sym (gensym))
	(f #'(lambda () (values 1 2 3))))
    (values
     (eqt (setf (symbol-function sym) f) f)
     (multiple-value-list (eval (list sym)))))
  t (1 2 3))

;;; Error cases

(deftest symbol-function.error.1
  (signals-error (symbol-function) program-error)
  t)

(deftest symbol-function.error.2
  (signals-error (symbol-function 'cons nil) program-error)
  t)

(deftest symbol-function.error.3
  (check-type-error #'symbol-function #'symbolp)
  nil)

(deftest symbol-function.error.4
  (check-type-error #'(lambda (x) (setf (symbol-function x) #'identity))
		    #'symbolp)
  nil)

(deftest symbol-function.error.5
  (let ((sym (gensym)))
    (handler-case (progn (symbol-function sym) nil)
		  (undefined-function
		   (c)
		   (assert (eq (cell-error-name c) sym))
		   :good)))
  :good)



