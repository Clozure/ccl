;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 13 07:55:05 2004
;;;; Contains: Add tests for MAKUNBOUND

(in-package :cl-test)

(deftest makunbound.1
  (let ((sym (gensym)))
    (values
     (boundp sym)
     (equalt (multiple-value-list (makunbound sym)) (list sym))
     (boundp sym)
     (setf (symbol-value sym) nil)
     (notnot (boundp sym))
     (equalt (multiple-value-list (makunbound sym)) (list sym))
     (boundp sym)))
  nil t nil nil t t nil)

(deftest makunbound.2
  (let ((sym (gensym)))
    (values
     (boundp sym)
     (setf (symbol-value sym) :foo)
     (equalt (multiple-value-list (makunbound sym)) (list sym))
     (boundp sym)
     (handler-case (symbol-value sym)
		   (unbound-variable (c)
		     (if (eq (cell-error-name c) sym) :good
		       (list :bad sym (cell-error-name c)))))))
  nil :foo t nil :good)

;;; Error cases

(deftest makunbound.error.1
  (signals-error (makunbound) program-error)
  t)

(deftest makunbound.error.2
  (signals-error (makunbound (gensym) nil) program-error)
  t)

(deftest makunbound.error.3
  (check-type-error #'makunbound #'symbolp)
  nil)
