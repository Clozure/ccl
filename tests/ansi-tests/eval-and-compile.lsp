;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct 21 22:52:19 2002
;;;; Contains: Overall tests for section 3, 'Evaluation and Compilation'

(in-package :cl-test)

(defparameter *eval-and-compile-fns*
  '(compile eval macroexpand macroexpand-1 proclaim special-operator-p
	    constantp))

(deftest eval-and-compile-fns
  (remove-if #'fboundp *eval-and-compile-fns*)
  nil)

(defparameter *eval-and-compile-macros*
  '(lambda define-compiler-macro defmacro define-symbol-macro declaim))

(deftest eval-and-compile-macros
  (remove-if #'macro-function *eval-and-compile-macros*)
  nil)

