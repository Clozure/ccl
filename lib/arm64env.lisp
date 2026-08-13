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

;;; Darwin/arm64 MAP_JIT helpers for the compiler (arm64-lap loads after
;;; arm64env).  Level-0 arm64-utils owns the same defs for the boot image.
;;; Do not redefine live helpers mid-rebuild.
;;;
;;; AREA_CODE: executable code is MAP_JIT from cold-load; purify copies
;;; into AREA_READONLY.  WP only in kernel C.
#+(and darwinarm64-target)
(progn
  (defvar *jit-code-base* nil)
  (defvar *jit-code-limit* nil)
  (defvar *jit-code-free* nil)

  (unless (boundp '*darwinarm64-map-jit-fasls*)
    (defvar *darwinarm64-map-jit-fasls* t))

  (unless (fboundp '%darwinarm64-register-code-heap)
    (defun %darwinarm64-register-code-heap ()
      (when *jit-code-base*
        (ff-call (foreign-symbol-address "darwin_arm64_set_code_heap")
                 :address *jit-code-base*
                 :address *jit-code-free*
                 :void))))

  (unless (fboundp '%ensure-jit-code-heap)
    (defun %ensure-jit-code-heap ()
      "MAP_JIT code heap for this process.  Not part of the saved image."
      (unless (and *jit-code-base*
                   (typep *jit-code-base* 'macptr)
                   (not (%null-ptr-p *jit-code-base*)))
        (let* ((len #.(* 256 1024 1024))
               (p (ff-call (foreign-symbol-address "mmap")
                           :address (%null-ptr)
                           :unsigned-fullword len
                           :int #x7
                           :int (logior #x1002 #x0800)
                           :int -1 :long 0 :address)))
          (when (or (%null-ptr-p p) (eql (%ptr-to-int p) -1))
            (error "mmap(MAP_JIT) code heap failed"))
          (setq *jit-code-base* p
                *jit-code-limit* (%inc-ptr p len)
                *jit-code-free* p)
          (%darwinarm64-register-code-heap)))
      *jit-code-base*))

  (unless (fboundp '%allocate-code-vector)
    (defun %allocate-code-vector (element-count)
      "Allocate a code-vector of ELEMENT-COUNT u32 words in MAP_JIT."
      (declare (fixnum element-count))
      (%ensure-jit-code-heap)
      (let* ((payload (ash element-count 2))
             (total (logandc2 (+ payload 8 15) 15))
             (header (logior (ash element-count arm64::num-subtag-bits)
                             arm64::subtag-code-vector))
             (free *jit-code-free*)
             (next (%inc-ptr free total)))
        (when (>= (%ptr-to-int next) (%ptr-to-int *jit-code-limit*))
          (error "MAP_JIT code heap exhausted"))
        (ff-call (foreign-symbol-address "darwin_arm64_jit_init_code_vector")
                 :address free
                 :unsigned-doubleword header
                 :unsigned-fullword total
                 :void)
        (setq *jit-code-free* next)
        (%darwinarm64-register-code-heap)
        (%tag-as-misc free))))

  (unless (fboundp '%darwinarm64-jit-install-code)
    (defun %darwinarm64-jit-install-code (code-vector src-ivector nbytes)
      "Copy NBYTES from SRC-IVECTOR into CODE-VECTOR.  WP+icache in kernel C."
      (declare (fixnum nbytes))
      (with-macptrs ((d) (s))
        (%vect-data-to-macptr code-vector d)
        (%vect-data-to-macptr src-ivector s)
        (ff-call (foreign-symbol-address "darwin_arm64_jit_install_code")
                 :address d
                 :address s
                 :unsigned-fullword nbytes
                 :void))
      code-vector))

  (defun %enable-darwinarm64-map-jit-fasls ()
    "Ensure MAP_JIT fasl loads (default under AREA_CODE)."
    (setq *darwinarm64-map-jit-fasls* t)
    (%ensure-jit-code-heap)
    t)
  )

(provide "ARM64ENV")
