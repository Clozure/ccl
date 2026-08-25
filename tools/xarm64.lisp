(in-package "CCL")

(defpackage "ARM64-DARWIN" (:use))

(defun load-darwinarm64-backend ()
  (in-development-mode
    ;; until updated versions are included in the lisp image
    (load "ccl:lib;systems.lisp")
    (load "ccl:lib;compile-ccl.lisp"))
  (update-modules '(arm64-arch arm64-asm arm64-lap arm64-backend
                    arm64-vinsns arm642)
                  t)
  (setup-arm64-ftd *darwinarm64-backend*)
  (update-modules '(arm64-lapmacros arm64-disassemble ffi-darwinarm64) t)
  (unless (boundp '*arm64-xload-modules*)
    (error "ccl:lib;compile-ccl.lisp did not define *arm64-xload-modules*"))
  (update-modules *arm64-xload-modules* t))

(load-darwinarm64-backend)
