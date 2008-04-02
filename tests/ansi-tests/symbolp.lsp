;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 22 08:59:12 2003
;;;; Contains: Tests for SYMBOLP

(in-package :cl-test)

(deftest symbolp.1
  (notnot-mv (symbolp nil))
  t)

(deftest symbolp.2
  (check-predicate #'symbolp nil *symbols*)
  nil)

(deftest symbolp.3
  (check-predicate (complement #'symbolp) #'(lambda (x) (member x *symbols*)))
  nil)

;;; Error cases

(deftest symbolp.error.1
  (signals-error (symbolp) program-error)
  t)

(deftest symbolp.error.2
  (signals-error (symbolp nil nil) program-error)
  t)
