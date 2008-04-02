;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 29 04:33:05 2003
;;;; Contains: Load tests for pathnames and logical pathnames

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

(load "pathnames.lsp")
(load "pathname.lsp")
(load "pathnamep.lsp")
(load "make-pathname.lsp")
(load "pathname-host.lsp")
(load "pathname-device.lsp")
(load "pathname-directory.lsp")
(load "pathname-name.lsp")
(load "pathname-type.lsp")
(load "pathname-version.lsp")

(load "load-logical-pathname-translations.lsp")
(load "logical-pathname.lsp")
(load "logical-pathname-translations.lsp")
(load "translate-logical-pathname.lsp")

(load "namestring.lsp")
(load "file-namestring.lsp")
(load "directory-namestring.lsp")
(load "host-namestring.lsp")
(load "enough-namestring.lsp")

(load "wild-pathname-p.lsp")
(load "merge-pathnames.lsp")
(load "pathname-match-p.lsp")

(load "parse-namestring.lsp")