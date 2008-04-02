;;; Tests of strings

(load "char-schar.lsp")
(load "string.lsp")
(load "base-string.lsp")
(load "simple-string.lsp")
(load "simple-base-string.lsp")
(load "simple-string-p.lsp")
(load "stringp.lsp")
(load "string-upcase.lsp")
(load "string-downcase.lsp")
(load "string-capitalize.lsp")
(load "nstring-upcase.lsp")
(load "nstring-downcase.lsp")
(load "nstring-capitalize.lsp")
(load "string-trim.lsp")
(load "string-left-trim.lsp")
(load "string-right-trim.lsp")

;;; Tests of string comparison functions
(compile-and-load "string-aux.lsp")
(load "string-comparisons.lsp")
(load "make-string.lsp")