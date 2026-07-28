;;; SPDX-License-Identifier: Apache-2.0
;;;
;;; PROPOSED — callback-frame contract constants (FFI lane, msg-28 item).
;;;
;;; ⚠ These describe OUR spentry-E callback trampoline's frame layout,
;;; NOT anything in Matt's tree: he has no callback design yet
;;; (msg-18/19 — "FF-calls not thought about").  They exist so that
;;; lib/ffi-linuxarm64.lisp generate-callback-bindings compiles in the
;;; level-1 gate (l1-lisp-threads defcallback splices
;;; callback-frame.savelr-offset / callback-frame.fp-save-offset at
;;; macroexpansion time).  When Matt specs his callback trampoline,
;;; these constants MUST travel with it — re-derive or ratify then;
;;; do not treat these values as upstream-agreed.
;;;
;;; The contract (CBF = &x0save, 16-aligned; verified against THREE
;;; agreeing sources, 2026-07-16):
;;;   1. lib/ffi-linuxarm64.lisp section (3) header: "x0..x7 saves at
;;;      +0..56, the C caller's stack args CONTIGUOUS at +64, d0..d7
;;;      saves at -64..-8, saved LR at -152".
;;;   2. v2 arch constants (compiler/ARM64/arm64-arch.lisp:1047-1049):
;;;      fp-save-offset -64, savelr-offset -152, stack-args-offset 64.
;;;   3. The actual push sequence in BOTH trampolines:
;;;      v2 kernel lisp-kernel/arm64-spentry.s:4464-4490 (boot-validated
;;;      s89) and our low-tag draft upstream-port/lisp-kernel/
;;;      spentry-E-ffi.s:404-427 — x0..x7 pushed (CBF = sp), then
;;;      d0..d7 (d0 @ CBF-64), then callee-saved GPR pairs
;;;      x19/x20 @ -80, x21/x22 @ -96, x23/x24 @ -112, x25/x26 @ -128,
;;;      x27/x28 @ -144, x29/LR @ -160 ⇒ saved LR at CBF-152.
;;;
;;; Loaded by the gate harness after Matt's arch layer (wired by lead).

(in-package "ARM64")

;;; d0..d7 argument-register save block (8 x 8 bytes at CBF-64..CBF-8).
;;; Referenced: lib/ffi-linuxarm64.lisp:231 (fp-regs-form).
(defconstant callback-frame.fp-save-offset -64)

;;; The trampoline's saved LR = the C caller's return address (the
;;; x29/LR pair at CBF-160, LR in the high word).  Referenced:
;;; lib/ffi-linuxarm64.lisp:247 (generate-callback-bindings' 7th value).
(defconstant callback-frame.savelr-offset -152)

;;; C caller's stack-passed args, contiguous above the x0..x7 block.
;;; Not currently referenced by ffi-linuxarm64.lisp BY NAME (its offset
;;; stream reaches +64 arithmetically), but it is the third member of
;;; the v2 contract triple — defined so the contract artifact is
;;; complete.
(defconstant callback-frame.stack-args-offset 64)
