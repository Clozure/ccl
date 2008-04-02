;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 06:39:21 2002
;;;; Contains: Tests for FUNCTIONP

(in-package :cl-test)

;;;
;;; Note!  FUNCTIONP and FUNCTION behave differently in ANSI CL than
;;; in CLTL1.  In particular, symbols and various lists are no longer
;;; in the class FUNCTION in ANSI CL.
;;;

(deftest functionp.1
  (functionp nil)
  nil)

;;; In ANSI CL, symbols can no longer be functions
(deftest functionp.2
  (functionp 'identity)
  nil)

(deftest functionp.3
  (not (functionp #'identity))
  nil)

(deftest functionp.4
  (loop for x in *cl-symbol-names*
	for s = (find-symbol x "CL")
	for f = (and (fboundp s)
		     (symbol-function s)
		     (not (special-operator-p s))
		     (not (macro-function s))
		     (symbol-function s))
	unless (or (null f)
		   (functionp f))
	collect x)
  nil)

(deftest functionp.5
  (functionp '(setf car))
  nil)

;;; In ANSI CL, lambda forms are no longer functions
(deftest functionp.6
  (functionp '(lambda (x) x))
  nil)

(report-and-ignore-errors
 (defun (setf functionp-7-accessor) (y x) (setf (car x) y) y))

(deftest functionp.7
  (not-mv (functionp #'(setf functionp-7-accessor)))
  nil)

(deftest functionp.8
  (not-mv (functionp #'(lambda (x) x)))
  nil)

(deftest functionp.9
  (not-mv (functionp (compile nil '(lambda (x) x))))
  nil)

;;; In ANSI CL, symbols and cons can no longer be functions
(deftest functionp.10
  (check-predicate #'(lambda (x)
		       (not (and (or (numberp x) (characterp x)
				     (symbolp x) (consp x)
				     (typep x 'array))
				 (functionp x)))))
  nil)

(deftest functionp.11
  (flet ((%f () nil)) (functionp '%f))
  nil)

(deftest functionp.12
  (flet ((%f () nil)) (not-mv (functionp #'%f)))
  nil)

;;; TODO: Add check-type-predicate test?

(deftest functionp.order.1
  (let ((i 0))
    (values
     (notnot (functionp (progn (incf i) #'cons)))
     i))
  t 1)

(deftest functionp.error.1
  (signals-error (functionp) program-error)
  t)

(deftest functionp.error.2
  (signals-error (functionp #'cons nil) program-error)
  t)
