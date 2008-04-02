;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 05:20:41 2004
;;;; Contains: Load tests of the reader

(in-package :cl-test)

(load "reader-test.lsp")
(load "with-standard-io-syntax.lsp")
(load "copy-readtable.lsp")
(load "read.lsp")
(load "read-preserving-whitespace.lsp")
(load "read-delimited-list.lsp")
(load "read-from-string.lsp")
(load "readtable-case.lsp")
(load "readtablep.lsp")
(load "get-macro-character.lsp")
(load "set-macro-character.lsp")
(load "read-suppress.lsp")
(load "set-syntax-from-char.lsp")
(load "dispatch-macro-characters.lsp")

(load "syntax.lsp")
(load "syntax-tokens.lsp")
