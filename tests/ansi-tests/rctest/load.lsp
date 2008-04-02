;;; Compile and load the rctest system

(load "../compile-and-load.lsp")
(load "../rt-package.lsp")
(compile-and-load "../rt.lsp")
(load "../cl-test-package.lsp")
(compile-and-load "../random-aux.lsp")
(load "rctest-package.lsp")
(compile-and-load "rctest-util.lsp")
(compile-and-load "generator.lsp")
(compile-and-load "lambda-generator.lsp")
(compile-and-load "form-generators.lsp")
