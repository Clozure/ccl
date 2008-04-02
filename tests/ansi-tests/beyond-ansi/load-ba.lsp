;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jun  8 06:52:59 2005
;;;; Contains: Load beyond-ansi tests

(let ((*default-pathname-defaults* (pathname *load-pathname*)))
  (let ((*default-pathname-defaults*
	 (merge-pathnames (make-pathname :directory '(:relative :up)))))
    (load "gclload1.lsp"))
  (load "ba-test-package.lsp")
  (eval '(compile-and-load "ba-aux.lsp"))
  (load "errors-eval-compile.lsp")
  (load "errors-types-and-class.lsp")
  (load "errors-data-and-control-flow-1.lsp")
  (load "errors-data-and-control-flow-2.lsp")
  (load "errors-data-and-control-flow-3.lsp")
  (in-package :ba-test)
  )

