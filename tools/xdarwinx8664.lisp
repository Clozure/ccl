;;; Cross-compile setup for Darwin/x8664 (Intel).
;;; Parallel to tools/xdarwinarm64.lisp
;;;
;;; Loads the darwinx8664 backend into a non-x86 host, so that
;;;
;;;     (cross-compile-ccl :darwinx8664 t)
;;;     (cross-xload-level-0 :darwinx8664 :force)
;;;
;;; Needs ccl:cross-darwin-x86-headers64; which can be a copy of (or link to) darwin-x86-headers64

(in-package "CCL")

(defpackage "X86-DARWIN64" (:use))

(defun load-darwinx8664-backend ()
  (update-modules '(x86-arch x86-asm x8632-arch x8664-arch x86-lap x8632env x8664env
                    x86-backend x8664-backend x8664-vinsns x862)
                  t)
  (fixup-x8664-backend)
  (setup-x8664-ftd *darwinx8664-backend*)
  (update-modules '(x86-lapmacros x86-disassemble ffi-darwinx8664) t)
  (update-modules *x8664-xload-modules* t)
  ;; If you're loading into a linuxx8664 host, you'll need to manually
  ;; evaluate the *darwinx8664-backend* defvar in x8664-backend.lisp and
  ;; do (pushnew *darwinx8664-backend* *known-backends* :key #'backend-name)
  ;; so that (find-backend :darwinx8664) works.
  (format t "~&;; darwinx8664 backend loaded, nil-value => #x~x~%"
          (arch::target-nil-value
	   (backend-target-arch *darwinx8664-backend*))))

(load-darwinx8664-backend)
