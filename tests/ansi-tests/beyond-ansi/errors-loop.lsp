;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Tests of non-ANSI exceptions sutation from CLHS for the LOOP macro

(in-package :ba-test)

(compile-and-load "ba-aux.lsp")

;;; LOOP tests

(def-all-error-test loop.1 'listp #'(lambda (x) `(loop . ,x)))

(def-all-error-test loop.named.1 'symbolp #'(lambda (x) `(loop named ,x return nil)))
(def-all-error-test loop.named.2 'listp #'(lambda (x) `(loop named . ,x)))

(def-error-test loop.with.1 (loop with))
(def-all-error-test loop.with.2 #'(lambda (x) (or (symbolp x) (listp x)))
  #'(lambda (x) `(loop with ,x)))
(def-all-error-test loop.with.3 'listp #'(lambda (x) `(loop with . ,x)))
(def-all-error-test loop.with.4 'listp #'(lambda (x) `(loop with x . ,x)))
(def-all-error-test loop.with.5 'listp #'(lambda (x) `(loop with x = . ,x)))
(def-all-error-test loop.with.6 'listp #'(lambda (x) `(loop with x t = . ,x)))

(def-error-test loop.initially.1 (loop initially))
(def-all-error-test loop.initially.2 'listp #'(lambda (x) `(loop initially . ,x)))
(def-all-error-test loop.initially.3 'listp #'(lambda (x) `(loop initially (progn) . ,x)))

(def-error-test loop.finally.1 (loop finally))
(def-all-error-test loop.finally.2 'listp #'(lambda (x) `(loop finally . ,x)))
(def-all-error-test loop.finally.3 'listp #'(lambda (x) `(loop finally (progn) . ,x)))

;;; LOOP FOR clauses

(def-error-test loop.for.1 (loop for))
(def-all-error-test loop.for.2 'listp #'(lambda (x) `(loop for . ,x)))
(def-all-error-test loop.for.3 'symbol-or-list-p #'(lambda (x) `(loop for ,x)))
(def-all-error-test loop.for.4 'symbol-or-list-p #'(lambda (x) `(loop for ,x = nil)))
(def-error-test loop.for.5 (loop for x from))
(def-error-test loop.for.6 (loop for x upfrom))
(def-error-test loop.for.7 (loop for x downfrom))
(def-error-test loop.for.8 (loop for x upto))
(def-error-test loop.for.9 (loop for x to))
(def-error-test loop.for.10 (loop for x below))

(def-all-error-test loop.for.11 (typef '(or symbol list class))
  #'(lambda (x) `(loop for e ,x = nil return e)))

(def-all-error-test loop.for.12 'listp #'(lambda (x) `(loop for x . ,x)))
(def-all-error-test loop.for.13 'listp #'(lambda (x) `(loop for x from . ,x)))
(def-all-error-test loop.for.14 'listp #'(lambda (x) `(loop for x downfrom . ,x)))
(def-all-error-test loop.for.15 'listp #'(lambda (x) `(loop for x upfrom . ,x)))
(def-all-error-test loop.for.16 'listp #'(lambda (x) `(loop for x upto . ,x)))
(def-all-error-test loop.for.17 'listp #'(lambda (x) `(loop for x to . ,x)))
(def-all-error-test loop.for.18 'listp #'(lambda (x) `(loop for x downto . ,x)))


