;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "CCL")

(defconstant $numarm64saveregs 0)       ;R1: ARM32-shape; save0-3 later
(defconstant $numarm64argregs 3)

(defconstant arm64-nonvolatile-registers-mask
  0)                                    ;R1

(defconstant arm64-arg-registers-mask
  (logior (ash 1 arm64::arg_z)
          (ash 1 arm64::arg_y)
          (ash 1 arm64::arg_x)))

(defconstant arm64-temp-registers-mask
  (logior (ash 1 arm64::temp0)
          (ash 1 arm64::temp1)
          (ash 1 arm64::temp2)
          (ash 1 arm64::temp3)
          (ash 1 arm64::temp4)
          (ash 1 arm64::temp5)))               ;R2

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
             arm64::temp5
             arm64::arg_x
             arm64::arg_y
             arm64::arg_z))                    ;R2

(defconstant arm64-nonvolatile-node-regs
  0)                                    ;R1

(defconstant arm64-node-regs (logior arm64-temp-node-regs
                                     arm64-nonvolatile-node-regs))

(defconstant arm64-imm-regs
  (make-mask arm64::imm0
             arm64::imm1
             arm64::imm2
             arm64::imm3
             arm64::imm4
             arm64::imm5))

(defconstant arm64-temp-fp-regs (1- (ash 1 8))) ;R4: d0-d7

(defconstant arm64-cr-fields (make-mask 0)) ;single NZCV "field 0"

(defconstant $undo-arm64-c-frame 16)

(provide "ARM64ENV")

;;; ------------------------------------------------------------------
;;; Merged from the linuxarm64 port's compiler overlay. Appended in the
;;; order our build concatenated the fasls, so the definition that wins
;;; is the one that won when the suite was measured.
;;; ------------------------------------------------------------------

(ccl::provide "ARM64ENV")
