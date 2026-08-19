;;;; Smoke: relocatable static space (hardened-VM fallback).
;;;;
;;;; Run with CCL_FORCE_STATIC_RELOC=1 so the kernel skips the canonical
;;;; STATIC_BASE_ADDRESS and relocates the static section at load; verify
;;;; nil moved and that kernel-global access, GC, and static conses work.
;;;;
;;;;   CCL_FORCE_STATIC_RELOC=1 ./darm64cl --no-init --batch \
;;;;     < tools/darwin-static-reloc-smoke.lisp
(in-package :ccl)

(let ((nil-addr (%address-of nil)))
  (format t "~&;; nil at ~x~%" nil-addr)
  (when (getenv "CCL_FORCE_STATIC_RELOC")
    (when (eql nil-addr #x20000100b)
      (error "CCL_FORCE_STATIC_RELOC set but nil is at the canonical base"))))

;; Kernel globals must resolve rnil-relative (argv, all-areas).
(unless (plusp (length *command-line-argument-list*))
  (error "empty *command-line-argument-list* — kernel global argv unread"))
(let ((n 0))
  (do-gc-areas (a) (incf n))
  (unless (> n 3)
    (error "do-gc-areas walked ~d areas — all-areas global broken" n)))

(dotimes (i 10) (gc))
(unless (>= (full-gccount) 10)
  (error "GC did not run"))

(let ((c (static-cons 1 2)))
  (unless (and (eql (car c) 1) (eql (cdr c) 2))
    (error "static-cons broken: ~s" c)))

(format t "~&DARWIN-STATIC-RELOC-SMOKE-OK~%")
(quit 0)
