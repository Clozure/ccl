;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 15 13:07:39 2005
;;;; Contains: Tests of ED

(in-package :cl-test)

;;; Since the normal behavior of ED is implementation dependent,
;;; test only the error behavior

(deftest ed.error.1
  (signals-error (ed "ed.lsp" nil) program-error)
  t)

;;; Since the editor may not even be included, no other tests
;;; are possible.
