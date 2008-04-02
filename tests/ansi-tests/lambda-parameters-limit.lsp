;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 10 22:45:44 2002
;;;; Contains: Tests for LAMBDA-PARAMETERS-LIMIT

(in-package :cl-test)

(deftest lambda-parameters-limit.1
  (not (typep lambda-parameters-limit 'integer))
  nil)

(deftest lambda-parameters-limit.2
  (< lambda-parameters-limit 50)
  nil)

;;; See also tests is flet.lsp, labels.lsp

