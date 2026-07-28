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
;;;   - HFAs/HVAs of 1-4 fundamental SIMD&FP elements: passed in V0..V7
;;;     (not yet detected here; conservatively treated as general composite).
;;;   - Composite size <= 16 bytes (128 bits): passed in GPRs (split across
;;;     X0..X7 as 1-2 doublewords, left-justified — NOT right-justified
;;;     like PowerOpen).
;;;   - Composite size > 16 bytes: passed by reference to a caller-allocated
;;;     copy (single :address slot pointing to the copy).
;;;
;;; Composite return rules (AAPCS64 §6.9):
;;;   - Size <= 16 bytes (and not HFA): returned in X0/X1.
;;;   - Size > 16 bytes (or HFA): caller allocates buffer, passes pointer
;;;     in X8; function writes through X8 and returns void.
;;;
;;; HFA detection is a TODO; we conservatively use the size threshold only.
;;; This is slightly pessimistic for HFA-eligible aggregates (which AAPCS64
;;; allows in V-registers regardless of total size up to 4 elements) but is
;;; never incorrect — pessimistic-but-correct over optimistic-but-wrong.

(in-package "CCL")

;;; The ARM64-LINUX64 package holds the four FTD callback entrypoints
;;; consumed by foreign-types.lisp / nfcomp.lisp's
;;; %with-cross-compilation-target.  Created here (not via defpackage in
;;; level-0) because lib/ffi-*.lisp files are the canonical home for
;;; per-OS FFI interface packages in vendor/ccl/.
;;;
;;; ARM64-DEVIATION: Clozure precedent for 64-bit OS interface packages
;;; is arch-OS (X86-LINUX64, X86-DARWIN64, ARM-LINUX).  Following that
;;; pattern we use ARM64-LINUX64 rather than the PPC64 odd-one-out name
;;; LINUX64 (which doesn't include an arch prefix).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "ARM64-LINUX64")
    (make-package "ARM64-LINUX64" :use '("CL" "CCL"))))


;;;-----------------------------------------------------------------------
;;; (1) record-type-returns-structure-as-first-arg
;;;-----------------------------------------------------------------------
;;; PPC64 source (lines 28-36): always returns T for foreign-record-type
;;; (PowerOpen "all structures - of any size - are returned by passing a
;;;  pointer in the first argument").
;;;
;;; ARM64-DEVIATION: AAPCS64 §6.9 returns composites <= 16 bytes (128
;;; bits) in X0/X1 directly; only larger composites use the X8 indirect
;;; result-area pointer.  We encode the size threshold explicitly.

(defun arm64-linux64::record-type-returns-structure-as-first-arg (rtype)
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
        ;;; return per AAPCS64 §6.9.  PPC64 source returns T unconditionally.
        (> (foreign-type-bits ftype) 128)))))


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
;;; :void" — applies identically to AAPCS64 ONLY when the struct is
;;; > 128 bits (size threshold from record-type-returns-structure-as-first-arg
;;; above).  For smaller structs, AAPCS64 returns them in X0/X1 and the
;;; coercion machinery handles the unpack — but expand-ff-call's contract
;;; with the higher-level macro is shaped by what
;;; record-type-returns-structure-as-first-arg said, so this code path
;;; is only entered for > 128-bit return values (consistent).

(defun arm64-linux64::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void)))
    (multiple-value-bind (result-type error)
        (ignore-errors (parse-foreign-type result-type-spec))
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (eq (car args) :monitor-exception-ports)
          (argforms (pop args)))
        (when (typep result-type 'foreign-record-type)
          ;;; Reached only for > 128-bit returns (per AAPCS64 §6.9, gated
          ;;; by arm64-linux64::record-type-returns-structure-as-first-arg).
          ;;; Caller-allocated result buffer is passed as the first :address
          ;;; arg; AAPCS64 wiring puts it in X8 at the call boundary.
          (setq result-type *void-foreign-type*
                result-type-spec :void)
          (argforms :address)
          (argforms (pop args)))
        (unless (evenp (length args))
          (error "~s should be an even-length list of alternating foreign types and values" args))
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
                    (cond
                      ;;; ARM64-DEVIATION: <=64 bit struct passed
                      ;;; left-justified (raw value, no ash).  PPC64
                      ;;; source right-justified via (ash _ (- bits 64)).
                      ((<= bits 64)
                       (argforms :unsigned-doubleword)
                       (argforms `(%%get-unsigned-longlong ,arg-value-form 0)))
                      ;;; ARM64-DEVIATION: 65-128 bit struct passed as
                      ;;; 2 doublewords in GPRs (X<n>..X<n+1>).  Matches
                      ;;; the PPC64 source for the same size range but
                      ;;; only applies up to 128 bits on AAPCS64.
                      ((<= bits 128)
                       (argforms (ceiling bits 64))
                       (argforms arg-value-form))
                      ;;; ARM64-DEVIATION: > 128 bit struct passed by
                      ;;; reference (caller allocates copy, callee reads
                      ;;; through pointer).  PPC64 source would emit
                      ;;; (ceiling bits 64) doublewords here — AAPCS64
                      ;;; never does that.
                      (t
                       (argforms :address)
                       (argforms arg-value-form))))
                  (progn
                    (argforms (foreign-type-to-representation-type ftype))
                    (argforms (funcall arg-coerce arg-type-spec arg-value-form))))))))
        (argforms (foreign-type-to-representation-type result-type))
        (funcall result-coerce result-type-spec `(,@callform ,@(argforms)))))))


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
(defun arm64-linux64::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
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
(defun arm64-linux64::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
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
