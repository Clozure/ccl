;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; arm64-io.lisp — ARM64 LAP function drafts for I/O functions
;;;;
;;;; Ported line-by-line from vendor/ccl/level-0/PPC/ppc-io.lisp (PPC64 arm).
;;;; Register map: Matt Emerson's upstream arm64 (arm64-asm.lisp).
;;;; Tags: LOW tags, fixnumshift=3.
;;;;
;;;; STATUS: LEAD-VERIFIED 2026-07-08 (line-by-line vs PPC64; assemble-gate clean
;;;; except DECIDE-blocked sites — see drafts/wave1-verify-report.md)

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM64-LAPMACROS"))

;;; =====================================================================
;;; %get-errno — from vendor/ccl/level-0/PPC/ppc-io.lisp:23
;;; =====================================================================
;;;
;;; PPC64 source:
;;;   (ldr imm1 target::tcr.errno-loc target::rcontext)  ; load ptr to errno
;;;   (lwz imm0 0 imm1)                                   ; load errno (32-bit int)
;;;   (stw rzero 0 imm1)                                  ; clear errno
;;;   (neg imm0 imm0)                                     ; negate (CCL convention)
;;;   (box-fixnum arg_z imm0)                             ; return as fixnum
;;;   (blr)
;;;
;;; Key offsets (arm64-arch.lisp):
;;;   tcr.errno-loc = 128 (byte offset from rcontext)
;;;
;;; On ARM64: errno is a 32-bit signed int at the address stored in
;;; tcr.errno-loc.  Matt's assembler has full W-register (32-bit)
;;; ldr/str templates and parses w0-w30/wzr (arm64-asm.lisp:39,699), so lwz/stw
;;; map directly — no width workaround needed.  Note wN in LAP names the
;;; ARCHITECTURAL register N; imm0 = x0 upstream (arm64-asm.lisp:143
;;; define-register-alias), so its W alias is w0.

(defarm64lapfunction %get-errno ()
  ;; (ldr imm1 target::tcr.errno-loc target::rcontext)
  (ldr imm1 (:@ rcontext (:$ arm64::tcr.errno-loc)))
  ;; (lwz imm0 0 imm1) — 32-bit zero-extending load, exactly ldr wN
  (ldr w0 (:@ imm1 (:$ 0)))              ; w0 = W alias of imm0/x0
  ;; (stw rzero 0 imm1) — clear errno with a true 32-bit store; an 8-byte
  ;; str would overwrite the 4 bytes after errno.
  (str wzr (:@ imm1 (:$ 0)))
  ;; (neg imm0 imm0) — errno was zero-extended by the W load, so a small
  ;; positive negates correctly in 64 bits.
  (sub imm0 xzr imm0)
  ;; (box-fixnum arg_z imm0) — shift left by fixnumshift
  (box-fixnum arg_z imm0)
  (ret))
