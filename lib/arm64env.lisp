;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "CCL")

(defconstant $numarm64saveregs 4)
(defconstant $numarm64argregs 3)

(defconstant arm64-nonvolatile-registers-mask
  (logior (ash 1 arm64::save0)
          (ash 1 arm64::save1)
          (ash 1 arm64::save2)
          (ash 1 arm64::save3)))

(defconstant arm64-arg-registers-mask
  (logior (ash 1 arm64::arg_z)
          (ash 1 arm64::arg_y)
          (ash 1 arm64::arg_x)))

(defconstant arm64-temp-registers-mask
  (logior (ash 1 arm64::temp0)
          (ash 1 arm64::temp1)
          (ash 1 arm64::temp2)
          (ash 1 arm64::temp3)
          (ash 1 arm64::temp4)))

(defconstant arm64-tagged-registers-mask
  (logior arm64-temp-registers-mask
          arm64-arg-registers-mask
          arm64-nonvolatile-registers-mask))

(defconstant arm64-temp-node-regs
  (make-mask arm64::temp0
             arm64::temp1
             arm64::temp2
             arm64::temp3
             arm64::temp4
             arm64::arg_x
             arm64::arg_y
             arm64::arg_z))

(defconstant arm64-nonvolatile-node-regs
  (make-mask arm64::save0
             arm64::save1
             arm64::save2
             arm64::save3))

(defconstant arm64-node-regs (logior arm64-temp-node-regs
                                     arm64-nonvolatile-node-regs))

(defconstant arm64-imm-regs
  (make-mask arm64::imm0
             arm64::imm1
             arm64::imm2
             arm64::imm3
             arm64::imm4
             arm64::imm5))

;;; The FP temp pool is the set of ABI-VOLATILE FP registers: PPC64 uses
;;; (1- (ash 1 ppc::fp14)) = f0-f13, exactly its volatile set (ppcenv.lisp:80).
;;; Under AAPCS64 the volatile d-registers are d0-d7 and d16-d31; the LOW 64
;;; bits of d8-d15 are callee-saved.  d0-d7 is the conservative volatile
;;; subset, and is what the linuxarm64 port's measured images were compiled
;;; under.  Widening to d0-d7 + d16-d31 (#xffff00ff) is the strict PPC64
;;; analog; widening to all 32 is only safe if a callback trampoline saves
;;; d8-d15 (ours does, but that contract is not yet upstream-specified).
(defconstant arm64-temp-fp-regs (1- (ash 1 8)))

;;; ARM64 has ONE flags register, NZCV, and the backend models it as CR
;;; field 0: WITH-CRF-TARGET wires (make-wired-lreg 0 :class
;;; hard-reg-class-crf) (compiler/backend.lisp) and the vinsns wire (:crf 0).
;;; So the analog of ppc-cr-fields (all 8 real CR fields, ppcenv.lisp:82) is
;;; the single field 0.  It must not be 0/empty: AVAILABLE-CRF-TEMP and
;;; SELECT-CRF-TEMP scan the mask and signal "Bug: ran out of CR fields" when
;;; it is empty, and ARM642-OR reaches AVAILABLE-CRF-TEMP for any (OR ...) in
;;; statement position (the ppc2.lisp ARM642-OR analog does the same).
(defconstant arm64-cr-fields (make-mask 0))

(defconstant $undo-arm64-c-frame 16)

(provide "ARM64ENV")
