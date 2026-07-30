;;; -*- Mode: Lisp; Package: CCL -*-
;;; PPC64 LINE-PORT (source: vendor/ccl/level-1/ppc-trap-support.lisp,
;;; #+ppc64-target + #+linuxppc-target branches, cited "; ppc:NNN").
;;;
;;; arm64-trap-support.lisp — XCMAIN (the nrs_CMAIN callback) and the
;;; exception-context (xp) accessors for Matt Emerson's upstream ARM64
;;; (low-tag) design, linuxarm64 only.
;;;
;;; Structural deviations from the PPC64 donor (each cited in place):
;;;  - PPC conditional traps under-encode, so PPC's xcmain back-scans the
;;;    code vector for the load that fed the trap (%scan-for-instr,
;;;    match-instr).  ARM64 uuos (udf #imm16) are self-describing —
;;;    format in imm16 bits 1:0, operand registers and info inline
;;;    (arm64-uuo.s:16-65) — so the decode is a dispatch on the trap
;;;    word and the scan machinery has no analog here.
;;;  - The kernel routes to lisp cmain exactly: unary unbound, unary
;;;    slot_unbound (which also forwards its uuo_extra_registers
;;;    companion word as arg0), all binary infos 0-6, wrong_type, misc
;;;    interrupt_now/too_few/too_many/wrong_number, and SIGBUS faults
;;;    (upstream-port/lisp-kernel/arm64-exceptions.c handle_uuo,
;;;    callback_for_trap).  Everything errnum-shaped goes to
;;;    nrs_ERRDISP instead (see arm64-error-signal.lisp), as on PPC.
;;;  - ucontext geometry is linux-aarch64 (arm64-headers64 interface db,
;;;    ucontext.ffi: uc_mcontext at 176; mcontext = {fault_address@0,
;;;    regs[31]@8, sp@256, pc@264, pstate@272}).
;;;
;;; Register-number convention: every register field here (uuo operand
;;; fields, the kernel's fn-reg argument, handle-stack-overflow's rb)
;;; uses the KERNEL/hardware GPR numbering 0-30, plus 31 = SP (the
;;; kernel's Rsp selector, arm64-exceptions.c:112).  The arm64::
;;; register constants x0-x30 and the lisp aliases (arm64::fn = 7,
;;; arm64::arg_z = 11, ...) match that numbering, but arm64::sp is 32
;;; (an index into the assembler's *registers* table, after xzr at 31)
;;; — never pass arm64::sp to these accessors.

(in-package "CCL")

;; regs[0] within uc_mcontext (ucontext.ffi: mcontext regs @8); sp/pc/
;; pstate follow the regs array contiguously, so they are reachable
;; from the same base: sp = 256-8, pc = 264-8, pstate = 272-8.
(defconstant xp-sp-offset-in-regs 248)
(defconstant xp-pc-offset-in-regs 256)
(defconstant xp-pstate-offset-in-regs 264)

(eval-when (:compile-toplevel :execute)
  ;; ppc:223-236.  registers = macptr to mcontext.regs[0].
  (defmacro with-xp-registers-and-gpr-offset ((xp register-number)
                                              (registers offset) &body body)
    `(with-macptrs ((,registers (pref ,xp :ucontext_t.uc_mcontext.regs)))
       (let ((,offset (xp-gpr-offset ,register-number)))
         ,@body)))

  ;; uuo field extractors, arm64-uuo.s:16-65 (mirrored by the C decode
  ;; macros, arm64-exceptions.c:184-197).
  (defmacro uuo-format (the-trap) `(ldb (byte 2 0) ,the-trap))
  (defmacro uuo-unary-gpr (the-trap) `(ldb (byte 5 2) ,the-trap))
  (defmacro uuo-unary-info (the-trap) `(ldb (byte 9 7) ,the-trap))
  (defmacro uuo-binary-ra (the-trap) `(ldb (byte 5 2) ,the-trap))
  (defmacro uuo-binary-rb (the-trap) `(ldb (byte 5 7) ,the-trap))
  (defmacro uuo-binary-info (the-trap) `(ldb (byte 4 12) ,the-trap))
  (defmacro uuo-misc-info (the-trap) `(ldb (byte 14 2) ,the-trap))
  (defmacro uuo-wt-gpr (the-trap) `(ldb (byte 5 2) ,the-trap))
  (defmacro uuo-wt-continuable (the-trap) `(ldb (byte 1 7) ,the-trap))
  (defmacro uuo-wt-xtype (the-trap) `(ldb (byte 8 8) ,the-trap)))

;;; ppc:268-276.  0-30 = x0-x30 in mcontext.regs; 31 = SP (kernel Rsp
;;; selector), stored contiguously after the regs array.
(defun xp-gpr-offset (register-number)
  (unless (and (fixnump register-number)
               (<= 0 (the fixnum register-number))
               (< (the fixnum register-number) 32))
    (setq register-number (require-type register-number '(integer 0 (32)))))
  (the fixnum
    (if (eql register-number 31)
      xp-sp-offset-in-regs
      (* (the fixnum register-number) target::node-size))))

(defun xp-gpr-lisp (xp register-number)          ; ppc:280
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%get-object registers offset))))

(defun (setf xp-gpr-lisp) (value xp register-number) ; ppc:284
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (%set-object registers offset value)))

(defun xp-gpr-signed-long (xp register-number)   ; ppc:288
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%get-signed-long registers offset))))

(defun xp-gpr-signed-doubleword (xp register-number) ; ppc:292
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%%get-signed-longlong registers offset))))

(defun xp-gpr-macptr (xp register-number)        ; ppc:297
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%get-ptr registers offset))))

;;; The saved PC is not a GPR on AArch64 (mcontext.pc, past the regs
;;; array); PPC read/wrote it as regs[PT_NIP] (ppc:391-398).
(defun xp-pc-lisp (xp)
  (with-macptrs ((registers (pref xp :ucontext_t.uc_mcontext.regs)))
    (values (%get-object registers xp-pc-offset-in-regs))))

(defun (setf xp-pc-lisp) (value xp)
  (with-macptrs ((registers (pref xp :ucontext_t.uc_mcontext.regs)))
    (%set-object registers xp-pc-offset-in-regs value)))

;;; NZCV lives in mcontext.pstate bits 31-28 (C = bit 29).  The ARM32
;;; port reads CPSR the same way to sign a failed nargs compare
;;; (arm-error-signal.lisp:216-220).
(defun xp-pstate (xp)
  (with-macptrs ((registers (pref xp :ucontext_t.uc_mcontext.regs)))
    (values (%%get-unsigned-longlong registers xp-pstate-offset-in-regs))))

;;; ppc:301-315, register names remapped (nargs is fixnum-tagged here
;;; too — arm64-asm.lisp:185).
(defun xp-argument-list (xp)
  (let ((nargs (xp-gpr-lisp xp arm64::nargs))
        (arg-x (xp-gpr-lisp xp arm64::arg_x))
        (arg-y (xp-gpr-lisp xp arm64::arg_y))
        (arg-z (xp-gpr-lisp xp arm64::arg_z)))
    (cond ((eql nargs 0) nil)
          ((eql nargs 1) (list arg-z))
          ((eql nargs 2) (list arg-y arg-z))
          (t (let ((args (list arg-x arg-y arg-z)))
               (if (eql nargs 3)
                 args
                 (let ((vsp (xp-gpr-macptr xp arm64::vsp)))
                   (dotimes (i (- nargs 3))
                     (push (%get-object vsp (* i target::node-size)) args))
                   args)))))))

;;; ppc:371-380.  machine-state-offset is relative to mcontext.regs[0].
;;; Codevector is function slot 0, as on PPC64 (kernel ground truth:
;;; spentry-D-call-builtins.s _function.codevector).
(defconstant lr-offset-in-register-context (* 30 target::node-size)) ;x30
(defconstant pc-offset-in-register-context xp-pc-offset-in-regs)

(defun return-address-offset (xp fn machine-state-offset)
  (with-macptrs ((regs (pref xp :ucontext_t.uc_mcontext.regs)))
    (if (functionp fn)
      ;; Since the fulltag-function removal (patch 0055) a function IS
      ;; its misc-tagged uvector (PPC64 shape); the
      ;; %function-to-function-vector below is identity-with-typecheck.
      (or (%code-vector-pc (uvref (%function-to-function-vector fn) 0)
                           (%inc-ptr regs machine-state-offset))
          (%get-ptr regs machine-state-offset))
      (%get-ptr regs machine-state-offset))))

;;; ppc:400-459 — verbatim shape (fake-stack-frame istructs chained
;;; through *fake-stack-frames*, the PPC model; the ARM32 gvector/
;;; %dnode-address-of model needs LAP with no donor here).  See the
;;; case analysis at ppc:400-418.
;;; *fake-stack-frames*'s def-standard-initial-binding lives in
;;; lib/arm64-backtrace.lisp, mirroring lib/ppc-backtrace.lisp:33; that
;;; file loads (l1-boot-2 bin set) before cmain is armed (l1-boot-3).
(defvar *fake-stack-frames*)

(defun funcall-with-xp-stack-frames (xp trap-function thunk)
  (cond ((null trap-function)
         ;; Maybe inside a subprim from a lisp function
         (let* ((fn (xp-gpr-lisp xp arm64::fn))
                (lr (return-address-offset
                     xp fn lr-offset-in-register-context)))
           (if (fixnump lr)
             (let* ((sp (xp-gpr-lisp xp 31)) ;SP, kernel Rsp numbering
                    (vsp (xp-gpr-lisp xp arm64::vsp))
                    (frame (%cons-fake-stack-frame sp sp fn lr vsp xp *fake-stack-frames*))
                    (*fake-stack-frames* frame))
               (declare (dynamic-extent frame))
               (funcall thunk frame))
             (funcall thunk (xp-gpr-lisp xp 31)))))
        ((eq trap-function (xp-gpr-lisp xp arm64::fn))
         (let* ((sp (xp-gpr-lisp xp 31))
                (fn trap-function)
                (lr (return-address-offset
                     xp fn pc-offset-in-register-context))
                (vsp (xp-gpr-lisp xp arm64::vsp))
                (frame (%cons-fake-stack-frame sp sp fn lr vsp xp *fake-stack-frames*))
                (*fake-stack-frames* frame))
           (declare (dynamic-extent frame))
           (funcall thunk frame)))
        ((eq trap-function (xp-gpr-lisp xp arm64::nfn))
         (let* ((sp (xp-gpr-lisp xp 31))
                (fn (xp-gpr-lisp xp arm64::fn))
                (lr (return-address-offset
                     xp fn lr-offset-in-register-context))
                (vsp (xp-gpr-lisp xp arm64::vsp))
                (lr-frame (%cons-fake-stack-frame sp sp fn lr vsp xp))
                (pc-fn trap-function)
                (pc-lr (return-address-offset
                        xp pc-fn pc-offset-in-register-context))
                (pc-frame (%cons-fake-stack-frame sp lr-frame pc-fn pc-lr vsp xp *fake-stack-frames*))
                (*fake-stack-frames* pc-frame))
           (declare (dynamic-extent lr-frame pc-frame))
           (funcall thunk pc-frame)))
        (t (funcall thunk (xp-gpr-lisp xp 31)))))

;;; xtype code -> type specifier, indexed by the wrong_type uuo's 8-bit
;;; expected-type field.  Donor: arm-error-signal.lisp:18-85 (the ARM32
;;; twin of this table — PPC decodes types from trap immediates
;;; instead), 64-bit entries adjusted; arm64-arch.lisp:195-230 declares
;;; these codes "used in *arm64-xtype-specifiers*".
(defparameter *arm64-xtype-specifiers* (make-array 256 :initial-element nil))

(macrolet ((init-arm64-xtype-table (&rest pairs)
             (let* ((table (gensym)))
               (collect ((body))
                 (dolist (pair pairs)
                   (destructuring-bind (code . spec) pair
                     (body `(setf (svref ,table ,code) ',spec))))
                 `(let* ((,table *arm64-xtype-specifiers*))
                    ,@(body))))))
  (init-arm64-xtype-table
   (arm64::tag-fixnum . fixnum)
   (arm64::tag-list . list)
   (arm64::xtype-integer . integer)
   (arm64::xtype-s64 . (signed-byte 64))
   (arm64::xtype-u64 . (unsigned-byte 64))
   (arm64::xtype-s32 . (signed-byte 32))
   (arm64::xtype-u32 . (unsigned-byte 32))
   (arm64::xtype-s16 . (signed-byte 16))
   (arm64::xtype-u16 . (unsigned-byte 16))
   (arm64::xtype-s8  . (signed-byte 8))
   (arm64::xtype-u8  . (unsigned-byte 8))
   (arm64::xtype-bit . bit)
   (arm64::xtype-rational . rational)
   (arm64::xtype-real . real)
   (arm64::xtype-number . number)
   (arm64::xtype-cons . cons)
   (arm64::xtype-char-code . (mod #x110000))
   (arm64::xtype-unsigned-byte-24 . (unsigned-byte 24))
   (arm64::xtype-array2d . (array * (* *)))
   (arm64::xtype-array3d . (array * (* * *)))
   (arm64::xtype-null . null)
   (arm64::subtag-character . character)
   (arm64::subtag-bignum . bignum)
   (arm64::subtag-ratio . ratio)
   (arm64::subtag-single-float . single-float)
   (arm64::subtag-double-float . double-float)
   (arm64::subtag-complex . complex)
   (arm64::subtag-macptr . macptr)
   (arm64::subtag-code-vector . code-vector)
   (arm64::subtag-xcode-vector . xcode-vector)
   (arm64::subtag-catch-frame . catch-frame)
   (arm64::subtag-function . function)
   (arm64::subtag-basic-stream . basic-stream)
   (arm64::subtag-symbol . symbol)
   (arm64::subtag-lock . lock)
   (arm64::subtag-hash-vector . hash-vector)
   (arm64::subtag-pool . pool)
   (arm64::subtag-weak . population)
   (arm64::subtag-package . package)
   (arm64::subtag-slot-vector . slot-vector)
   (arm64::subtag-instance . standard-object)
   (arm64::subtag-struct . structure-object)
   (arm64::subtag-istruct . istruct)
   (arm64::subtag-value-cell . value-cell)
   (arm64::subtag-xfunction . xfunction)
   (arm64::subtag-arrayH . array-header)
   (arm64::subtag-vectorH . vector-header)
   (arm64::subtag-simple-vector . simple-vector)
   (arm64::subtag-single-float-vector . (simple-array single-float (*)))
   (arm64::subtag-u64-vector . (simple-array (unsigned-byte 64) (*)))
   (arm64::subtag-s64-vector . (simple-array (signed-byte 64) (*)))
   (arm64::subtag-u32-vector . (simple-array (unsigned-byte 32) (*)))
   (arm64::subtag-s32-vector . (simple-array (signed-byte 32) (*)))
   (arm64::subtag-fixnum-vector . (simple-array fixnum (*)))
   (arm64::subtag-simple-base-string . simple-base-string)
   (arm64::subtag-u16-vector . (simple-array (unsigned-byte 16) (*)))
   (arm64::subtag-s16-vector . (simple-array (signed-byte 16) (*)))
   (arm64::subtag-u8-vector . (simple-array (unsigned-byte 8) (*)))
   (arm64::subtag-s8-vector . (simple-array (signed-byte 8) (*)))
   (arm64::subtag-double-float-vector . (simple-array double-float (*)))
   (arm64::subtag-bit-vector . simple-bit-vector)
   (arm64::subtag-complex-single-float-vector . (simple-array (complex single-float) (*)))
   (arm64::subtag-complex-double-float-vector . (simple-array (complex double-float) (*)))
   ;; 16m40: the SCALAR complex floats.  The two *-vector entries above were
   ;; here but not these, so trap-unless-complex-single-float /
   ;; -complex-double-float would have reported a raw integer code instead of
   ;; a type -- (or typespec xtype) below falls back to the number.
   (arm64::subtag-complex-single-float . (complex single-float))
   (arm64::subtag-complex-double-float . (complex double-float))
   ;; 16m40: bare FULLTAGS.  arm64-arch.lisp:189-196 states the expected-type
   ;; field holds "either a lisptag, a fulltag, a uvector subtag byte, or an
   ;; xtype code" in ONE 256-entry namespace, and tag-fixnum/tag-list above
   ;; are the lisptag half of that.  These are the fulltag half, needed by the
   ;; trap-unless-fulltag= sites.  No collision: fulltags are 0-15 and every
   ;; xtype is >= #x10 by that file's own compile-time assert.
   ;; (fulltag-function removed, patch 0055: function checks are
   ;; typecode-based and report via the subtag-function row above.)
   (arm64::fulltag-symbol . symbol)
   (arm64::fulltag-nil . null)))

;;; Enter here from the kernel's callback_for_trap
;;; (arm64-exceptions.c:1695-1714; the PPC comment block at ppc:463-475
;;; describes the same contract): xp is the exception context; fn-reg
;;; is arm64::fn, arm64::nfn or 0 depending on whose code vector holds
;;; the PC; pc-or-index is the instruction index into that code vector,
;;; or the raw PC when fn-reg is 0; the-trap is the raw udf instruction
;;; word (equal to its low-16 immediate), or a signal number for memory
;;; faults; arg0/arg1 are per-trap extras (SIGBUS: fault address and
;;; write-flag).
;;;
;;; #$SIGBUS (7) can in principle collide with udf #7 = wrong_type
;;; reg=x1 xtype=0; x1 is imm1, an unboxed scratch register no type
;;; check ever encodes, so the SIGBUS test may safely come first (PPC
;;; had no such overlap because trap words carry opcode bits,
;;; ppc:750).
(defcallback xcmain (:without-interrupts t
                     :address xp
                     :unsigned-fullword fn-reg
                     :address pc-or-index
                     :unsigned-fullword the-trap
                     :signed-doubleword arg0
                     :signed-doubleword arg1)
  (let ((fn (unless (eql fn-reg 0) (xp-gpr-lisp xp fn-reg))))
    (with-xp-stack-frames (xp fn frame-ptr)
      ;; udf #16 = uuo_misc interrupt_now: periodic event polling, the
      ;; common case, tested first (ppc:719-725 tdgti nargs 0).  The
      ;; kernel synthesizes the same word for a deferred process
      ;; interrupt (arm64-exceptions.c:232/1262).
      (if (eql the-trap #x10)
        (cmain)
        (with-error-reentry-detection
          (let ((pc-index (if (eql fn-reg 0) pc-or-index (%ptr-to-int pc-or-index))))
            (cond
              ((eql the-trap #$SIGBUS)  ; ppc:750-755
               (%error (make-condition 'invalid-memory-access
                                       :address arg0
                                       :write-p (not (zerop arg1)))
                       ()
                       frame-ptr))
              ;; unary unbound: gpr holds the SYMBOL (handler contract,
              ;; arm64-exceptions.c uuo_unary_unbound; the emit sites are
              ;; spentry-A specrefcheck and spentry-C's binding path).
              ;; PPC continues from this trap by writing the restart's
              ;; value over the register that held the unbound marker
              ;; (ppc:905-915); the ARM64 uuo encodes only the symbol
              ;; register, so the value cannot be delivered to the
              ;; resume site — signal the standard unbound-variable
              ;; error, but refuse to continue if a restart returns.
              ;; RATIFY: continuing needs a following uuo_extra_registers
              ;; (binary info 7) naming the value register at both emit
              ;; sites, plus a handler-side PC adjustment.
              ((and (eql (uuo-format the-trap) 1)  ;uuo_format_unary
                    (eql (uuo-unary-info the-trap) 2)) ;unary_info_unbound
               (%kernel-restart-internal
                $xvunbnd
                (list (xp-gpr-lisp xp (uuo-unary-gpr the-trap)))
                frame-ptr)
               (%error "Can't continue from an unbound-variable trap on ARM64 (the resume site's value register is not encoded in the trap)."
                       nil frame-ptr))
              ;; unary slot_unbound (patch 0052 assigns the code): a
              ;; THREE-register error, so the primary uuo names only the
              ;; SLOT VECTOR and the kernel hands us its
              ;; uuo_extra_registers companion in arg0 -- ra = index,
              ;; rb = dest (doc/porting/arm64.md "Errors that need three
              ;; registers"; the kernel validated the companion and will
              ;; resume past BOTH words).
              ;;
              ;; Unlike the unbound-variable trap above, this one really
              ;; does continue: CL's SLOT-UNBOUND may RETURN a value,
              ;; which becomes the value of the slot reference, so store
              ;; it into dest.  Same contract as ARM32's
              ;; (uuo-error-slot-unbound dest instance index),
              ;; arm-error-signal.lisp:257-263 -- including its reset of
              ;; the reentry count, because a handler that returns makes
              ;; this a NORMAL occurrence that must not accumulate
              ;; toward the error-reentry guard.
              ((and (eql (uuo-format the-trap) 1)   ;uuo_format_unary
                    (eql (uuo-unary-info the-trap) 6)) ;unary_info_slot_unbound
               (let ((slotv (xp-gpr-lisp xp (uuo-unary-gpr the-trap)))
                     (index (xp-gpr-lisp xp (uuo-binary-ra arg0)))
                     (dest  (uuo-binary-rb arg0)))
                 (setq *error-reentry-count* 0)
                 (setf (xp-gpr-lisp xp dest)
                       (%slot-unbound-trap slotv index frame-ptr))))
              ((eql (uuo-format the-trap) 2)       ;uuo_format_binary
               (let* ((info (uuo-binary-info the-trap))
                      (ra (uuo-binary-ra the-trap))
                      (rb (uuo-binary-rb the-trap)))
                 (case info
                   ((0 1)  ; vector/array bounds: ra = index, rb = the
                           ; vector (convention: uuo_error_vector_bounds
                           ; emit sites; ppc:653-665 reports (index vector))
                    (%error (%rsc-string $xarroob)
                            (list (xp-gpr-lisp xp ra)
                                  (xp-gpr-lisp xp rb))
                            frame-ptr))
                   (2      ; integer divide by zero (no emitter yet —
                           ; level-0 calls divide-by-zero-error directly)
                    (%error (make-condition 'division-by-zero
                                            :operation 'truncate
                                            :operands (list (xp-gpr-lisp xp ra)
                                                            (xp-gpr-lisp xp rb)))
                            nil frame-ptr))
                   (3      ; eep/fv unresolved: ra = destination reg,
                           ; rb = the eep or foreign-variable object
                           ; (arm-error-signal.lisp:274-285 twin;
                           ; ppc:730-749 resolves and stores the address)
                    (let* ((eep-or-fv (xp-gpr-lisp xp rb)))
                      (etypecase eep-or-fv
                        (external-entry-point
                         (resolve-eep eep-or-fv)
                         (setf (xp-gpr-lisp xp ra)
                               (eep.address eep-or-fv)))
                        (foreign-variable
                         (resolve-foreign-variable eep-or-fv)
                         (setf (xp-gpr-lisp xp ra)
                               (fv.addr eep-or-fv))))))
                   (4      ; fpu exception.  No emitter yet; AArch64 FP
                           ; exceptions are untrapped (handle_sigfpe) —
                           ; RATIFY operand decode when an emitter lands.
                    (%error "FPU exception (uuo #x~x, registers ~d/~d: #x~x #x~x)"
                            (list the-trap ra rb
                                  (xp-gpr-signed-doubleword xp ra)
                                  (xp-gpr-signed-doubleword xp rb))
                            frame-ptr))
                   (5      ; array rank: ra = expected rank (fixnum),
                           ; rb = the array (ppc:666-677 reports
                           ; (array rank); convention fixed here — no
                           ; emitter yet)
                    (%err-disp-internal $xndims
                                        (list (xp-gpr-lisp xp rb)
                                              (xp-gpr-lisp xp ra))
                                        frame-ptr))
                   (6      ; array flags: ra = the flags value seen,
                           ; rb = the (purported) array header
                           ; (arm-error-signal.lisp:304-323 twin)
                    (let* ((array (xp-gpr-lisp xp rb))
                           (flags (xp-gpr-lisp xp ra))
                           (subtag (ldb target::arrayH.flags-cell-subtag-byte flags))
                           (element-type
                            (type-specifier
                             (array-ctype-element-type
                              (specifier-type (svref *arm64-xtype-specifiers* subtag))))))
                      (%error (make-condition
                               'type-error
                               :datum array
                               :expected-type `(,(if (logbitp $arh_simple_bit flags) 'simple-array 'array) ,element-type))
                              nil
                              frame-ptr)))
                   (t      ; 7 = two_registers extends a preceding uuo and
                           ; never faults alone (kernel refuses it)
                    (%error "Unknown binary trap: #x~x~%xp: ~s, fn: ~s, pc: #x~x"
                            (list the-trap xp fn pc-index)
                            frame-ptr)))))
              ((eql (uuo-format the-trap) 3)       ;uuo_format_wrong_type
               (let* ((regno (uuo-wt-gpr the-trap))
                      (datum (xp-gpr-lisp xp regno))
                      (xtype (uuo-wt-xtype the-trap))
                      (typespec (or (svref *arm64-xtype-specifiers* xtype)
                                    xtype)))
                 (if (eql (uuo-wt-continuable the-trap) 1)
                   ;; continuable: store the restart's value back into
                   ;; the trapping register and resume past the udf
                   ;; (arm64-uuo.s:57-63; ppc:27/105's err-fn +
                   ;; register-munge idiom)
                   (setf (xp-gpr-lisp xp regno)
                         (%kernel-restart-internal $xwrongtype
                                                   (list datum typespec)
                                                   frame-ptr))
                   (%error (make-condition 'type-error
                                           :datum datum
                                           :expected-type typespec)
                           nil
                           frame-ptr))))
              ((eql (uuo-format the-trap) 0)       ;uuo_format_misc
               (let* ((info (uuo-misc-info the-trap))
                      (nargs (xp-gpr-lisp xp arm64::nargs)))
                 (case info
                   (6                    ;uuo_too_few_args
                    (%error 'too-few-arguments
                            (list :nargs nargs :fn fn)
                            frame-ptr))
                   (7                    ;uuo_too_many_args
                    (%error 'too-many-arguments
                            (list :nargs nargs :fn fn)
                            frame-ptr))
                   (8                    ;uuo_wrong_number_of_args
                    ;; The emit site compares nargs against the exact
                    ;; required count and branches around the udf on
                    ;; equality, so NZCV still holds that compare: C set
                    ;; (and Z clear) = nargs above the requirement.  The
                    ;; ARM32 port signs its nargs trap from CPSR the
                    ;; same way (arm-error-signal.lisp:216-220).
                    (%error (if (logbitp 29 (xp-pstate xp))
                              'too-many-arguments
                              'too-few-arguments)
                            (list :nargs nargs :fn fn)
                            frame-ptr))
                   (t
                    (%error "Unknown misc trap: #x~x~%xp: ~s, fn: ~s, pc: #x~x"
                            (list the-trap xp fn pc-index)
                            frame-ptr)))))
              ;; Unknown trap (ppc:706-709)
              (t (%error "Unknown trap: #x~x~%xp: ~s, fn: ~s, pc: #x~x"
                         (list the-trap xp fn pc-index)
                         frame-ptr)))))))))
