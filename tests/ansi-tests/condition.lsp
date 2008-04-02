;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 27 22:13:25 2003
;;;; Contains: Tests of class CONDITION

(in-package :cl-test)

(deftest condition.1
  (notnot-mv (find-class 'condition nil))
  t)

(defparameter *allowed-condition-inclusions*
  '(
    (arithmetic-error error serious-condition condition)
    (cell-error error serious-condition condition)
    (condition)
    (control-error error serious-condition condition)
    (division-by-zero arithmetic-error error serious-condition condition)
    (end-of-file stream-error error serious-condition condition)
    (error serious-condition condition)
    (file-error error serious-condition condition)
    (floating-point-inexact arithmetic-error error serious-condition condition)
    (floating-point-invalid-operation arithmetic-error error serious-condition condition)
    (floating-point-overflow arithmetic-error error serious-condition condition)
    (floating-point-underflow arithmetic-error error serious-condition condition)
    (package-error error serious-condition condition)
    (parse-error error serious-condition condition)
    (print-not-readable error serious-condition condition)
    (program-error error serious-condition condition)
    (reader-error parse-error stream-error error serious-condition condition)
    (serious-condition condition)
    (simple-condition condition)
    (simple-error simple-condition error serious-condition condition)
    (simple-type-error simple-condition type-error error serious-condition condition)
    (simple-warning simple-condition warning condition)
    (storage-condition serious-condition condition)
    (stream-error error serious-condition condition)
    (style-warning warning condition)
    (type-error error serious-condition condition)
    (unbound-slot cell-error error serious-condition condition)
    (unbound-variable cell-error error serious-condition condition)
    (undefined-function cell-error error serious-condition condition)
    (warning condition)
    ))

;;; Relationships given in *allowed-condition-inclusions* are the only
;;; subtype relationships allowed on condition types
(deftest condition.2
  (loop for (cnd . supers) in *allowed-condition-inclusions*
	append (loop for super in supers
		     unless (subtypep cnd super)
		     collect (list cnd super)))
  nil)

(deftest condition.3
  ;; Relationships given in *allowed-condition-inclusions* are the only
  ;; subtype relationships allowed on condition types
  (loop for cnds in *allowed-condition-inclusions*
	for cnd = (first cnds)
	append (loop for super in (set-difference *condition-types* cnds)
		     when (subtypep cnd super)
		     collect (list cnd super)))
  nil)
