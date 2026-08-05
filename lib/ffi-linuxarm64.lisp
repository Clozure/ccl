;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 2026 (CCL ARM64 port)
;;; Based on vendor/ccl/lib/ffi-linuxppc64.lisp (Copyright 2007-2009 Clozure Associates)
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; PPC64 LINE-PORT (source: vendor/ccl/lib/ffi-linuxppc64.lisp)
;;; Ported 2026-05-22.  Calling-convention details replaced with AAPCS64
;;; (ARM Procedure Call Standard for the 64-bit Arm Architecture).
;;; Deviations from the PPC64 source are tagged inline with
;;;   ;;; ARM64-DEVIATION: <reason>
;;;
;;; AAPCS64 reference summary (per IHI 0055C, §5-§6):
;;;   - X0..X7   : integer/pointer args (8 GPR slots); return in X0..X1
;;;   - V0..V7   : SIMD&FP args (8 VFP slots); return in V0..V1
;;;   - X8       : indirect-result-area pointer (caller-allocated buffer
;;;                when return is a composite > 16 bytes)
;;;   - X9..X15  : caller-save scratch
;;;   - X16..X17 : ip0/ip1, intra-procedure-call scratch
;;;   - X19..X28 : callee-save
;;;   - X29      : frame pointer
;;;   - X30      : link register
;;;   - SP       : stack pointer (16-byte aligned at public boundaries)
;;;
;;; Composite (struct) argument rules (AAPCS64 §6.8):
;;;   - HFA/HVA (homogeneous aggregate of 1-4 members with the same
;;;     fundamental SIMD&FP type; B.3 exempts these from the >16-byte
;;;     copy): one V register per member when NSRN + members <= 8 (C.2);
;;;     otherwise the WHOLE aggregate goes to the stack (C.3/C.4) —
;;;     never to GPRs, never by reference.  Detected by hfa-type-info
;;;     below and passed by decomposing into scalar member argforms
;;;     (the darwinppc64 model, lib/ffi-darwinppc64.lisp); the C.3
;;;     stack case is refused loudly, exactly as >8 scalar FP args are
;;;     (no stack args until the stack-arg frame layout is ratified).
;;;   - Composite size <= 16 bytes (128 bits), not HFA: passed in GPRs
;;;     (X0..X7 as 1-2 doublewords, left-justified — NOT right-justified
;;;     like PowerOpen).
;;;   - Composite size > 16 bytes, not HFA: passed by reference to a
;;;     caller-allocated copy (single :address slot pointing to the copy).
;;;
;;; Composite return rules (AAPCS64 §6.9):
;;;   - HFA/HVA (1-4 members, any size): returned in V0..V3, one register
;;;     per member.  Captured via :registers/.SPffcall-return-registers
;;;     (regbuf {x0-x7 @ 0..56, d0-d7 @ 64..120}, spentry-E-ffi.s) and
;;;     copied out by struct-from-regbuf-values below — the same protocol
;;;     x86-64 uses (x8664-backend.lisp) and PPC64-Darwin pioneered
;;;     (.SPpoweropen-ffcall-return-registers).
;;;   - Size <= 16 bytes, not HFA: returned in X0/X1; captured via the
;;;     same regbuf, copied from the GPR half.
;;;   - Size > 16 bytes, not HFA: caller allocates buffer, passes pointer
;;;     in X8; function writes through X8 and returns void.  KNOWN GAP:
;;;     the c-frame/.SPffcall protocol has no X8 slot yet, so the buffer
;;;     pointer currently lands in X0 (unchanged pre-existing behavior;
;;;     see comms/HFA-DETECTION-16m71.md §10).

(in-package "CCL")

;;; The ARM64-LINUX package holds the four FTD callback entrypoints
;;; consumed by foreign-types.lisp / nfcomp.lisp's
;;; %with-cross-compilation-target.  Created here (not via defpackage in
;;; level-0) because lib/ffi-*.lisp files are the canonical home for
;;; per-OS FFI interface packages in vendor/ccl/.
;;;
;;; The name of the architecture is arm64.  The 64-bit nature is implied.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "ARM64-LINUX")
    (make-package "ARM64-LINUX" :use '("CL" "CCL"))))


;;;-----------------------------------------------------------------------
;;; (0) HFA/HVA classification — AAPCS64 §4.3.5 "Homogeneous Aggregates"
;;;-----------------------------------------------------------------------
;;; A Homogeneous Floating-point Aggregate is a composite type where all
;;; fundamental data members have the same floating-point type and there
;;; are AT MOST FOUR uniquely addressable members (§4.3.5.1-2).  Members
;;; may be scalars, arrays of members, or nested homogeneous records
;;; (SBCL's arm64 c-call.lisp hfa-member-info implements the same
;;; flattening).  For a union the member count is the MAX over its
;;; alternatives, not the sum (each alternative aliases the same
;;; storage; GCC aarch64_vfp_is_call_or_return_candidate does the same).
;;;
;;; The base types recognized are :single-float and :double-float — the
;;; only fundamental SIMD&FP types CCL's foreign-type system models.
;;; Half- and quad-precision floats and short vector types (the HVA
;;; case) have no CCL foreign-type representation, so aggregates of
;;; them cannot be expressed and need no arm here.
;;;
;;; The top-level size cross-check (bits = count * member-bits) rejects
;;; aggregates whose layout is not densely packed (e.g. over-aligned
;;; types via aligned attributes): GCC applies the same size test, and
;;; such a type is passed as a general composite, not an HFA.

(defun arm64-linux::hfa-element-info (ftype)
  ;; Flatten one potential HFA member.  Returns (values base count)
  ;; with base in {:single-float :double-float}, or NIL if the type
  ;; cannot be part of an HFA.
  (typecase ftype
    (foreign-single-float-type (values :single-float 1))
    (foreign-double-float-type (values :double-float 1))
    (foreign-array-type
     (let* ((dims (foreign-array-type-dimensions ftype)))
       (when (and dims (every #'(lambda (d) (typep d 'unsigned-byte)) dims))
         (let* ((n (reduce #'* dims)))
           (when (> n 0)
             (multiple-value-bind (base count)
                 (arm64-linux::hfa-element-info
                  (foreign-array-type-element-type ftype))
               (when base
                 (values base (* n count)))))))))
    (foreign-record-type
     (let* ((base nil)
            (count 0)
            (union-p (eq (foreign-record-type-kind ftype) :union)))
       (dolist (field (foreign-record-type-fields ftype)
                      (when base (values base count)))
         (multiple-value-bind (fbase fcount)
             (arm64-linux::hfa-element-info (foreign-record-field-type field))
           (when (or (null fbase)
                     (and base (not (eq base fbase))))
             (return nil))
           (setq base fbase)
           (if union-p
             (setq count (max count fcount))
             (incf count fcount))))))
    (t nil)))

(defun arm64-linux::hfa-type-info (ftype)
  ;; If FTYPE is an HFA, return (values base count); else NIL.
  (when (typep ftype 'foreign-record-type)
    (let* ((bits (ensure-foreign-type-bits ftype)))
      (multiple-value-bind (base count)
          (arm64-linux::hfa-element-info ftype)
        (when (and base
                   (<= 1 count 4)
                   (= bits (* count (if (eq base :single-float) 32 64))))
          (values base count))))))


;;;-----------------------------------------------------------------------
;;; (1) record-type-returns-structure-as-first-arg
;;;-----------------------------------------------------------------------
;;; PPC64 source (lines 28-36): always returns T for foreign-record-type
;;; (PowerOpen "all structures - of any size - are returned by passing a
;;;  pointer in the first argument").
;;;
;;; ARM64-DEVIATION: AAPCS64 §6.9 returns composites <= 16 bytes (128
;;; bits) in X0/X1 directly, and HFAs/HVAs of 1-4 members in V0..V3
;;; REGARDLESS of size; only other composites use the X8 indirect
;;; result-area pointer.

(defun arm64-linux::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (when (typep ftype 'foreign-record-type)
        (ensure-foreign-type-bits ftype)
        ;;; ARM64-DEVIATION: > 128 bits (16 bytes) triggers X8 indirect
        ;;; return per AAPCS64 §6.9 — unless the type is an HFA/HVA,
        ;;; which is returned in v0..v3 whatever its size.  PPC64 source
        ;;; returns T unconditionally.
        (and (> (foreign-type-bits ftype) 128)
             (not (arm64-linux::hfa-type-info ftype)))))))


;;;-----------------------------------------------------------------------
;;; (2) expand-ff-call
;;;-----------------------------------------------------------------------
;;; PPC64 source (lines 38-79): for foreign-record-type args < 64 bits,
;;; emits :unsigned-doubleword with the value right-justified via
;;; (ash (%%get-unsigned-longlong arg 0) (- bits 64)).  For >= 64 bits,
;;; emits ceiling(bits, 64) doublewords.  PowerOpen passes small structs
;;; right-justified in the high bits of a register.
;;;
;;; ARM64-DEVIATION: AAPCS64 §6.8 passes composites <= 16 bytes
;;; left-justified in 1-2 GPRs (X0..X7).  We:
;;;   - For bits <= 64: emit :unsigned-doubleword with the raw value
;;;     (NO ash) — load lands in X<n>[bits-1..0], high bits unspecified
;;;     (callee reads only the meaningful bits).
;;;   - For 64 < bits <= 128: emit 2 doublewords (ceiling bits 64).
;;;   - For bits > 128: pass by reference (:address + pointer to copy).
;;;     The caller is responsible for materialising the copy; here we
;;;     pass the original arg-value-form as an address, matching how
;;;     ffi-darwinarm.lisp handles foreign-record-type args (lines 71-75).
;;;
;;; The result-side foreign-record-type handling (lines 48-52 in PPC64
;;; source) — "implicit first arg = result-form, return-type becomes
;;; :void" — applies to AAPCS64 ONLY when the struct is > 128 bits AND
;;; not an HFA (the record-type-returns-structure-as-first-arg answer).
;;; HFAs (returned in v0..v3) and <= 128-bit composites (returned in
;;; x0/x1) are captured with the :registers regbuf protocol instead:
;;; .SPffcall-return-registers saves {x0-x7 @ 0..56, d0-d7 @ 64..120}
;;; into the buffer, and struct-from-regbuf-values copies the right
;;; slots into the caller's result structure.  Model:
;;; x8664::expand-ff-call (x8664-backend.lisp) and PPC64-Darwin's
;;; darwin64::struct-from-regbuf-values (lib/ffi-darwinppc64.lisp).

;;; Generate code to set the fields of a structure R of record type
;;; RTYPE from the register values captured in REGBUF by
;;; .SPffcall-return-registers ({x0..x7 @ 0..56, d0..d7 @ 64..120},
;;; spentry-E-ffi.s).
(defun arm64-linux::struct-from-regbuf-values (r rtype regbuf)
  (let* ((bits (ensure-foreign-type-bits rtype))
         (fp-area 64))                  ;d0 save slot; d<i> at 64 + 8i
    (multiple-value-bind (hfa-base hfa-count)
        (arm64-linux::hfa-type-info rtype)
      (collect ((forms))
        (cond
          ;; HFA: member i came back in v<i> (AAPCS64 §6.9).  Each
          ;; d-save slot holds the full 64-bit register; on
          ;; little-endian the single-float payload sits at the slot
          ;; base (same LE reasoning as the callback generator below).
          ((eq hfa-base :double-float)
           (dotimes (i hfa-count)
             (forms `(setf (%get-double-float ,r ,(* 8 i))
                      (%get-double-float ,regbuf ,(+ fp-area (* 8 i)))))))
          ((eq hfa-base :single-float)
           (dotimes (i hfa-count)
             (forms `(setf (%get-single-float ,r ,(* 4 i))
                      (%get-single-float ,regbuf ,(+ fp-area (* 8 i)))))))
          ;; Non-HFA <= 16 bytes: returned in x0/x1 (§6.9 via C.12);
          ;; copy 32 bits at a time to avoid consing (the darwinppc64/
          ;; x8664 struct-from-regbuf-values idiom).
          (t
           (do* ((b 0 (+ b 32))
                 (w 0 (+ w 4)))
                ((>= b bits))
             (declare (fixnum b w))
             (forms `(setf (%get-unsigned-long ,r ,w)
                      (%get-unsigned-long ,regbuf ,w))))))
        `(progn ,@(forms))))))

(defun arm64-linux::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
         (regbuf nil)
         (result-temp nil)
         (result-form nil)
         (struct-result-type nil)
         (structure-arg-temp nil))
    (multiple-value-bind (result-type error)
        (ignore-errors (parse-foreign-type result-type-spec))
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (eq (car args) :monitor-exception-ports)
          (argforms (pop args)))
        (when (typep result-type 'foreign-record-type)
          (setq result-form (pop args)
                struct-result-type result-type
                result-type *void-foreign-type*
                result-type-spec :void)
          (if (arm64-linux::record-type-returns-structure-as-first-arg
               struct-result-type)
            ;;; > 16 bytes and not an HFA: caller-allocated result
            ;;; buffer, passed by reference.  KNOWN GAP: AAPCS64 §6.9
            ;;; wants this pointer in X8; the c-frame/.SPffcall protocol
            ;;; has no X8 slot yet, so it is passed as the first :address
            ;;; arg and lands in X0 (pre-existing behavior, unchanged;
            ;;; comms/HFA-DETECTION-16m71.md §10).
            (progn
              (argforms :address)
              (argforms result-form))
            ;;; HFA (any size) or non-HFA <= 16 bytes: returned in
            ;;; registers (v0..v3 / x0-x1); capture them via
            ;;; .SPffcall-return-registers into a 128-byte regbuf.
            (progn
              (setq regbuf (gensym)
                    result-temp (gensym))
              (argforms :registers)
              (argforms regbuf))))
        (unless (evenp (length args))
          (error "~s should be an even-length list of alternating foreign types and values" args))
        (flet ((struct-arg-temp ()
                 (or structure-arg-temp
                     (setq structure-arg-temp (gensym)))))
          (do* ((args args (cddr args)))
               ((null args))
            (let* ((arg-type-spec (car args))
                   (arg-value-form (cadr args)))
              (if (or (member arg-type-spec *foreign-representation-type-keywords*
                              :test #'eq)
                      (typep arg-type-spec 'unsigned-byte))
                (progn
                  (argforms arg-type-spec)
                  (argforms arg-value-form))
                (let* ((ftype (parse-foreign-type arg-type-spec)))
                  (if (typep ftype 'foreign-record-type)
                    (let* ((bits (ensure-foreign-type-bits ftype)))
                      (multiple-value-bind (hfa-base hfa-count)
                          (arm64-linux::hfa-type-info ftype)
                        (cond
                          ;;; ARM64-DEVIATION: HFA/HVA passed in one V
                          ;;; register per member (AAPCS64 C.2), by
                          ;;; decomposing into scalar float argforms —
                          ;;; the darwinppc64 "constituent elements as
                          ;;; scalars" model.  The arg-value-form is
                          ;;; evaluated ONCE into a dynamic-extent temp;
                          ;;; member i is read at its natural offset.
                          ;;; If the members would overflow v0-v7, C.3
                          ;;; sends the WHOLE aggregate to the stack —
                          ;;; unratified, and refused loudly by the
                          ;;; backend/%ff-call exactly as >8 scalar FP
                          ;;; args are.
                          (hfa-base
                           (let* ((temp (struct-arg-temp))
                                  (single-p (eq hfa-base :single-float))
                                  (accessor (if single-p
                                              '%get-single-float
                                              '%get-double-float))
                                  (memsize (if single-p 4 8)))
                             (dotimes (i hfa-count)
                               (argforms hfa-base)
                               (argforms
                                `(,accessor
                                  ,(if (eql i 0)
                                     `(%setf-macptr ,temp ,arg-value-form)
                                     temp)
                                  ,(* i memsize))))))
                          ;;; ARM64-DEVIATION: <=64 bit non-HFA struct
                          ;;; passed left-justified (raw value, no ash).
                          ;;; PPC64 source right-justified via
                          ;;; (ash _ (- bits 64)).
                          ((<= bits 64)
                           (argforms :unsigned-doubleword)
                           (argforms `(%%get-unsigned-longlong ,arg-value-form 0)))
                          ;;; ARM64-DEVIATION: 65-128 bit non-HFA struct
                          ;;; passed as 2 doublewords in consecutive GPRs
                          ;;; (AAPCS64 C.12), decomposed here into two
                          ;;; :unsigned-doubleword argforms through the
                          ;;; same evaluate-once temp.  (An integer
                          ;;; word-count argspec would say the same
                          ;;; thing, but the arm642 codegen has no case
                          ;;; for integer argspecs, so the decomposed
                          ;;; form is the one that compiles everywhere.)
                          ((<= bits 128)
                           (let* ((temp (struct-arg-temp)))
                             (argforms :unsigned-doubleword)
                             (argforms `(%%get-unsigned-longlong
                                         (%setf-macptr ,temp ,arg-value-form) 0))
                             (argforms :unsigned-doubleword)
                             (argforms `(%%get-unsigned-longlong ,temp 8))))
                          ;;; ARM64-DEVIATION: > 128 bit non-HFA struct
                          ;;; passed by reference (caller allocates copy,
                          ;;; callee reads through pointer; AAPCS64 B.4).
                          ;;; PPC64 source would emit (ceiling bits 64)
                          ;;; doublewords here — AAPCS64 never does that.
                          (t
                           (argforms :address)
                           (argforms arg-value-form)))))
                    (progn
                      (argforms (foreign-type-to-representation-type ftype))
                      (argforms (funcall arg-coerce arg-type-spec arg-value-form)))))))))
        (argforms (foreign-type-to-representation-type result-type))
        (let* ((call (funcall result-coerce result-type-spec
                              `(,@callform ,@(argforms)))))
          (when structure-arg-temp
            (setq call `(let* ((,structure-arg-temp (%null-ptr)))
                          (declare (dynamic-extent ,structure-arg-temp)
                                   (type macptr ,structure-arg-temp))
                          ,call)))
          (if regbuf
            `(let* ((,result-temp (%null-ptr)))
               (declare (dynamic-extent ,result-temp)
                        (type macptr ,result-temp))
               (%setf-macptr ,result-temp ,result-form)
               (with-ffcall-results (,regbuf)
                 ,call
                 ,(arm64-linux::struct-from-regbuf-values
                   result-temp struct-result-type regbuf)))
            call))))))


;;;-----------------------------------------------------------------------
;;; (3) generate-callback-bindings
;;;-----------------------------------------------------------------------
;;; PPC64 LINE-PORT (vendor/ccl/lib/ffi-linuxppc64.lisp:81-175) against
;;; the A1 callback-frame contract (arm64-arch.lisp callback-frame.*;
;;; built by _spentry(eabi_callback), lisp-kernel/arm64-spentry.s).
;;; stack-ptr = CBF: x0..x7 saves at +0..56, the C caller's stack args
;;; CONTIGUOUS at +64, d0..d7 saves at -64..-8, saved LR at -152.
;;;
;;; ARM64-DEVIATIONs from the PPC64 source:
;;;  - 8 FP arg regs (d0..d7), not PowerOpen's 13 (f1..f13).
;;;  - an FP register arg consumes NO slot in the linear gpr/stack
;;;    offset stream (PowerOpen reserved a param word per FP arg):
;;;    delta = 0 when the arg lands in d0..d7.
;;;  - little-endian: sub-word integers read at bias 0 (PPC64-BE biased
;;;    7/6/4 toward the high end of the doubleword).
;;;  - single-float args in registers were saved by the trampoline as
;;;    the d-register's low 64 bits, so the float bits sit at the slot
;;;    base: plain %get-single-float (PPC read a double and rounded via
;;;    %get-single-float-from-double-ptr — PowerOpen carries singles
;;;    double-extended; AAPCS64 does not).
;;;  - records: <=64 bits arrive in one GPR slot, read low-justified
;;;    (no BE (ash _ (- 64 bits)) re-justify); 65..128 bits inline in
;;;    two slots (%inc-ptr, delta 16); >128 bits arrive BY REFERENCE in
;;;    one GPR (AAPCS64 B.4) — read the pointer.  (PowerOpen passed any
;;;    record inline at full size.)  KNOWN GAP, documented: a 9..16-byte
;;;    record with exactly one GPR left goes wholly to the stack under
;;;    AAPCS64 (no register/stack split); this generator would read it
;;;    one slot early.  No boot-path callback has such a signature.
;;;  - fp-regs-form is frame arithmetic (%inc-ptr CBF -64), not PPC's
;;;    deref of a pointer the trampoline stored into its frame.
(defun arm64-linux::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (collect ((lets)
            (rlets)
            (inits)
            (dynamic-extent-names))
    (let* ((rtype (parse-foreign-type result-spec))
           (fp-regs-form nil))
      (flet ((set-fp-regs-form ()
               (unless fp-regs-form
                 (setq fp-regs-form `(%inc-ptr ,stack-ptr ,arm64::callback-frame.fp-save-offset)))))
        (when (typep rtype 'foreign-record-type)
          (setq argvars (cons struct-result-name argvars)
                argspecs (cons :address argspecs)
                rtype *void-foreign-type*))
        (when (typep rtype 'foreign-float-type)
          (set-fp-regs-form))
        (do* ((argvars argvars (cdr argvars))
              (argspecs argspecs (cdr argspecs))
              (fp-arg-num 0)
              (offset 0 (+ offset delta))
              (delta 8 8)
              (bias 0 0)
              (use-fp-args nil nil))
             ((null argvars)
              (values (rlets) (lets) (dynamic-extent-names) (inits) rtype fp-regs-form
                      arm64::callback-frame.savelr-offset))
          (let* ((name (car argvars))
                 (spec (car argspecs))
                 (argtype (parse-foreign-type spec))
                 (bits (ensure-foreign-type-bits argtype)))
            (if (and (typep argtype 'foreign-record-type)
                     (<= bits 64))
              (progn
                (when name (rlets (list name (foreign-record-type-name argtype))))
                ;; ARM64-DEVIATION (LE): copy the slot verbatim — the value
                ;; is low-justified in its doubleword.
                (when name (inits `(setf (%%get-unsigned-longlong ,name 0)
                                    (%%get-unsigned-longlong ,stack-ptr ,offset)))))
              (let* ((access-form
                      `(,(cond
                          ((typep argtype 'foreign-single-float-type)
                           (when (< (incf fp-arg-num) 9)
                             (setq use-fp-args t
                                   delta 0))
                           '%get-single-float)
                          ((typep argtype 'foreign-double-float-type)
                           (when (< (incf fp-arg-num) 9)
                             (setq use-fp-args t
                                   delta 0))
                           '%get-double-float)
                          ((and (typep argtype 'foreign-integer-type)
                                (= (foreign-integer-type-bits argtype) 64)
                                (foreign-integer-type-signed argtype))
                           '%%get-signed-longlong)
                          ((and (typep argtype 'foreign-integer-type)
                                (= (foreign-integer-type-bits argtype) 64)
                                (not (foreign-integer-type-signed argtype)))
                           '%%get-unsigned-longlong)
                          ((or (typep argtype 'foreign-pointer-type)
                               (typep argtype 'foreign-array-type))
                           '%get-ptr)
                          ((typep argtype 'foreign-record-type)
                           (if (<= bits 128)
                             (progn
                               (setq delta 16)
                               '%inc-ptr)
                             ;; ARM64-DEVIATION: >128-bit records arrive
                             ;; by reference (AAPCS64 B.4).
                             '%get-ptr))
                          (t
                           (cond ((typep argtype 'foreign-integer-type)
                                  (let* ((bits (foreign-integer-type-bits argtype))
                                         (signed (foreign-integer-type-signed argtype)))
                                    ;; ARM64-DEVIATION (LE): bias 0 for all
                                    ;; sub-word widths.
                                    (cond ((<= bits 8)
                                           (if signed
                                             '%get-signed-byte
                                             '%get-unsigned-byte))
                                          ((<= bits 16)
                                           (if signed
                                             '%get-signed-word
                                             '%get-unsigned-word))
                                          ((<= bits 32)
                                           (if signed
                                             '%get-signed-long
                                             '%get-unsigned-long))
                                          (t
                                           (error "Don't know how to access foreign argument of type ~s" (unparse-foreign-type argtype))))))
                                 (t
                                  (error "Don't know how to access foreign argument of type ~s" (unparse-foreign-type argtype))))))
                        ,(if use-fp-args fp-args-ptr stack-ptr)
                        ,(if use-fp-args (* 8 (1- fp-arg-num))
                             `(+ ,offset ,bias)))))
                (when name (lets (list name access-form)))
                (when use-fp-args (set-fp-regs-form))))))))))


;;;-----------------------------------------------------------------------
;;; (4) generate-callback-return-value
;;;-----------------------------------------------------------------------
;;; PPC64 LINE-PORT (vendor/ccl/lib/ffi-linuxppc64.lisp:180-199).
;;; All structures are "returned" via the implicit first argument; the
;;; binding generator already translated the return type to :void then.
;;; The kernel trampoline reloads x0/x1 from CBF+0/+8 and d0 from CBF-64
;;; on exit, so the value is written into the argument save area
;;; (exactly PPC's gp_save reuse).
;;;
;;; ARM64-DEVIATION: a :single-float result is written as FLOAT BITS
;;; (%get-single-float setf, low 32 bits of the d0 reload slot) — the C
;;; caller reads s0.  PPC coerced to double ((float result 0.0d0)) and
;;; wrote a double because PowerOpen returns singles double-extended in
;;; f1; AAPCS64 does not.
(defun arm64-linux::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (declare (ignore struct-return-arg))
  (unless (eq return-type *void-foreign-type*)
    (let* ((return-type-keyword (foreign-type-to-representation-type return-type)))
      (case return-type-keyword
        (:single-float
         `(setf (%get-single-float ,fp-args-ptr 0) ,result))
        (:double-float
         `(setf (%get-double-float ,fp-args-ptr 0) ,result))
        (:address
         `(setf (%get-ptr ,stack-ptr 0) ,result))
        (:signed-doubleword
         `(setf (%%get-signed-longlong ,stack-ptr 0) ,result))
        (:unsigned-doubleword
         `(setf (%%get-unsigned-longlong ,stack-ptr 0) ,result))
        (t
         `(setf (%%get-signed-longlong ,stack-ptr 0) ,result))))))
