;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 04:40:33 2004
;;;; Contains: File to load tests of the lisp printer

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(load "copy-pprint-dispatch.lsp")

(load "print-integers.lsp")
(load "print-ratios.lsp")
(load "print-floats.lsp")
(load "print-complex.lsp")
(load "print-characters.lsp")
(load "print-symbols.lsp")
(load "print-strings.lsp")
(load "print-cons.lsp")
(load "print-backquote.lsp")
(load "print-bit-vector.lsp")
(load "print-vector.lsp")
(load "print-array.lsp")
(load "print-random-state.lsp")
(load "print-pathname.lsp")
(load "print-structure.lsp")
(load "printer-control-vars.lsp")
(load "pprint-dispatch.lsp")
(load "pprint-fill.lsp")
(load "pprint-linear.lsp")
(load "pprint-tabular.lsp")
(load "pprint-indent.lsp")
(load "pprint-logical-block.lsp")
(load "pprint-exit-if-list-exhausted.lsp")
(load "pprint-newline.lsp")
(load "pprint-tab.lsp")
(load "print-unreadable-object.lsp")
(load "write.lsp")
(load "print.lsp")
(load "pprint.lsp")
(load "prin1.lsp")
(load "princ.lsp")
(load "write-to-string.lsp")
(load "prin1-to-string.lsp")
(load "princ-to-string.lsp")
(load "print-level.lsp")
(load "print-length.lsp")

(load "load-format.lsp")
