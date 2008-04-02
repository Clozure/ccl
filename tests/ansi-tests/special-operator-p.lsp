;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 14 05:51:41 2003
;;;; Contains: Tests fo SPECIAL-OPERATOR-P

(in-package :cl-test)

;;; See section 3.1.2.1.2.1
(defparameter +special-operators+
  '(block let* return-from catch load-time-value setq eval-when
	  locally symbol-macrolet flet macrolet tagbody function
	  multiple-value-call the go multiple-value-prog1 throw if
	  progn unwind-protect labels progv let quote))


;;; All the symbols in +special-operators+ are special operators
(deftest special-operator-p.1
  (loop for s in +special-operators+
	unless (special-operator-p s)
	collect s)
  nil)

;;; None of the standard symbols except those in +special-operators+
;;; are special operators, unless they have a macro function
;;; (See the page for MACRO-FUNCTION)

(deftest special-operator-p.2
  (let ((p (find-package "CL")))
    (loop for name in *cl-symbol-names*
	  unless (or (member name +special-operators+ :test #'string=)
		     (let ((sym (find-symbol name p)))
		       (or (not (special-operator-p sym))
			   (macro-function sym))))
	  collect name))
  nil)

(deftest special-operator-p.order.1
  (let ((i 0))
    (values (notnot (special-operator-p (progn (incf i) 'catch)))
	    i))
  t 1)

(deftest special-operator-p.error.1
  (check-type-error #'special-operator-p #'symbolp)
  nil)

(deftest special-operator-p.error.2
  (signals-error (special-operator-p) program-error)
  t)

(deftest special-operator-p.error.3
  (signals-error (special-operator-p 'cons 'cons) program-error)
  t)
