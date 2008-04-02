#+:ecl (si::package-lock (find-package "COMMON-LISP") nil)
#+:armedbear (require 'pprint)

#+cmu (setq ext:*gc-verbose* nil)

#+gcl (setq compiler:*suppress-compiler-notes* t
            compiler:*suppress-compiler-warnings* t
            compiler:*compile-verbose* nil
            compiler:*compile-print* nil)

#+lispworks (setq compiler::*compiler-warnings* nil)
#+lispworks (make-echo-stream *standard-input* *standard-output*)

#+ecl (compile nil '(lambda () nil))

#+ecl (setq c:*suppress-compiler-warnings* t c:*suppress-compiler-notes* t)

#+clisp (setq custom::*warn-on-floating-point-contagion* nil)

(let (*load-verbose* *load-print* *compile-verbose* *compile-print*)
  (load "compile-and-load.lsp"))

(let (*load-verbose* *load-print* *compile-verbose* *compile-print*)
  (load "rt-package.lsp")
  (compile-and-load "rt.lsp")
  ;; (unless (probe-file "rt.o") (compile-file "rt.lsp"))
  ;; (load "rt.o")
  (load "cl-test-package.lsp")
  (in-package :cl-test)
  (compile-and-load "ansi-aux-macros.lsp")
  (handler-bind
   #-sbcl ()
   #+sbcl ((sb-ext:code-deletion-note #'muffle-warning))
   (load "universe.lsp"))
  (compile-and-load "random-aux.lsp")
  (compile-and-load "ansi-aux.lsp")
  ;; (unless (probe-file "ansi-aux.o") (compile-file "ansi-aux.lsp"))
  ;; (load "ansi-aux.o")
  
  (load "cl-symbol-names.lsp")
  (load "notes.lsp"))

(setq *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil)

