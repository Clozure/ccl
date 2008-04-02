;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 20 11:50:26 2005
;;;; Contains: Randomized tests of type propagation during compilation

(compile-and-load "random-type-prop.lsp")

(in-package :cl-test)

(load "random-type-prop-tests-01.lsp")
(load "random-type-prop-tests-02.lsp")
(load "random-type-prop-tests-03.lsp")
(load "random-type-prop-tests-04.lsp")
(load "random-type-prop-tests-05.lsp")
(load "random-type-prop-tests-06.lsp")
(load "random-type-prop-tests-07.lsp")
(load "random-type-prop-tests-08.lsp")
(load "random-type-prop-tests-09.lsp")
(load "random-type-prop-tests-10.lsp")

(load "random-type-prop-tests-structs.lsp")

