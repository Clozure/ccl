;;; -*- Mode: Lisp; Package: CCL -*-
;;; PPC64 LINE-PORT (source: vendor/ccl/level-1/ppc-error-signal.lisp,
;;; cited "; ppc:NNN").
;;;
;;; arm64-error-signal.lisp — %XERR-DISP (the nrs_ERRDISP callback) for
;;; Matt Emerson's upstream ARM64 (low-tag) design, linuxarm64 only.
;;;
;;; The kernel side is upstream-port/lisp-kernel/arm64-exceptions.c
;;; handle_error → callback_for_trap(errdisp, xp, where, errnum, rb,
;;; continuable): the same PPC-shaped pre-decoded contract as the donor
;;; (errnum-style errors come here; raw trap words go to XCMAIN in
;;; arm64-trap-support.lisp).
;;;
;;; Deviations from the donor (cited in place):
;;;  - PPC's FPU-exception branch (ppc:67-99) back-decodes a PPC FP
;;;    instruction; AArch64 FP exceptions are untrapped on shipping
;;;    cores (arm64-exceptions.c handle_sigfpe) and the fpu uuo route
;;;    goes to XCMAIN, so the branch has no analog here.
;;;  - handle-udf-call's PC redirect requires the kernel NOT to bump
;;;    the resume PC (PPC kernel: ppc-exceptions.c:1424-1429).  The
;;;    arm64 kernel's uuo_unary_udf_call case gains the same bump
;;;    suppression; until the next ARM kernel rebuild picks that up,
;;;    the redirect lands 4 bytes past the code-vector entry (path is
;;;    only reachable by continuing from an undefined-function-call
;;;    restart).

(in-package "CCL")

;;; callback here from C exception handler (ppc:20-24).
;;; pc-or-index widened to :unsigned-doubleword: when fn-reg is 0 it
;;; carries a raw 64-bit PC (the donor's :unsigned-fullword predates
;;; 64-bit naturals).
(defcallback %xerr-disp
    (:address xp
     :unsigned-fullword fn-reg
     :unsigned-doubleword pc-or-index
     :signed-fullword errnum
     :unsigned-fullword rb
     :signed-fullword continuable)
  (declare (ignorable pc-or-index))
  (block %err-disp
    (let ((fn (unless (eql fn-reg 0) (xp-gpr-lisp xp fn-reg)))
          (err-fn (if (eql continuable 0) '%err-disp-internal '%kernel-restart-internal)))
      (if (eql errnum arch::error-stack-overflow)
        (handle-stack-overflow xp fn rb)
        (with-xp-stack-frames (xp fn frame-ptr) ; execute body with dummy stack frame(s)
          (with-error-reentry-detection
              (let* ((rb-value (xp-gpr-lisp xp rb))
                     (res
                      (cond ((< errnum 0)
                             (%err-disp-internal errnum nil frame-ptr))
                            ((logtest errnum arch::error-type-error)
                             (funcall err-fn
                                      #.(car (rassoc 'type-error *kernel-simple-error-classes*))
                                      (list rb-value (logandc2 errnum arch::error-type-error))
                                      frame-ptr))
                            ((eql errnum arch::error-udf)
                             (funcall err-fn $xfunbnd (list rb-value) frame-ptr))
                            ((eql errnum arch::error-throw-tag-missing)
                             (%error (make-condition 'cant-throw-error
                                                     :tag rb-value)
                                     nil frame-ptr))
                            ((eql errnum arch::error-cant-call)
                             (%error (make-condition 'type-error
                                                     :datum rb-value
                                                     :expected-type '(or symbol function)
                                                     :format-control
                                                     "~S is not of type ~S, and can't be FUNCALLed or APPLYed")
                                     nil frame-ptr))
                            ((eql errnum arch::error-udf-call)
                             (return-from %err-disp
                               (handle-udf-call xp frame-ptr)))
                            ;; arm64:: (not arch::): the frozen cross-host
                            ;; image bakes arch.lisp, so a NEW arch::
                            ;; constant reads as an undeclared free variable
                            ;; and the fasl's ARCH-package symbol kills cold
                            ;; load ($XNOPKG "ARCH" -- observed 16m45b).
                            ((eql errnum arm64::error-apply-macro-or-special)
                             ;; Funcalled a symbol naming a macro/special
                             ;; operator (the fcell 2-elt vector's slot 0 =
                             ;; %macro-code% fired uuo_error_apply_macro;
                             ;; rb = fname).  PPC delivers exactly
                             ;; ($XNOTFUN fname args) via ksignalerr
                             ;; (xppcfasload.lisp:37); condition class
                             ;; call-special-operator-or-macro, a subclass
                             ;; of undefined-function.
                             (%err-disp-internal $xnotfun
                                                 (list (maybe-setf-name rb-value)
                                                       (xp-argument-list xp))
                                                 frame-ptr))
                            ((eql errnum arch::error-alloc-failed)
                             (%error (make-condition
                                      'simple-storage-condition
                                      :format-control (%rsc-string $xmemfull))
                                     nil frame-ptr))
                            ((eql errnum arch::error-memory-full)
                             (%error (make-condition
                                      'simple-storage-condition
                                      :format-control (%rsc-string $xnomem))
                                     nil frame-ptr))
                            ((eql errnum arch::error-excised-function-call)
                             (%error "~s: code has been excised." (list (xp-gpr-lisp xp arm64::nfn)) frame-ptr))
                            ((eql errnum arch::error-too-many-values)
                             (%err-disp-internal $xtoomanyvalues (list rb-value) frame-ptr))
                            (t (%error "Unknown error #~d with arg: ~d" (list errnum rb-value) frame-ptr)))))
                (setf (xp-gpr-lisp xp rb) res) ; munge register for continuation
                )))))))

;;; ppc:110-126.  The trampoline function f is entered by writing its
;;; code vector (function slot 0) into the resume PC, exactly the value
;;; _SPjmpnfn branches through (spentry-D-call-builtins.s jmpnfn).
(defun handle-udf-call (xp frame-ptr)
  (let* ((args (xp-argument-list xp))
         (values (multiple-value-list
                  (%kernel-restart-internal
                   $xudfcall
                   (list (maybe-setf-name (xp-gpr-lisp xp arm64::fname)) args)
                   frame-ptr)))
         (stack-argcnt (max 0 (- (length args) 3)))
         (vsp (%i+ (xp-gpr-lisp xp arm64::vsp) stack-argcnt))
         (f #'(lambda (values) (apply #'values values))))
    (setf (xp-gpr-lisp xp arm64::vsp) vsp
          (xp-gpr-lisp xp arm64::nargs) 1
          (xp-gpr-lisp xp arm64::arg_z) values
          (xp-gpr-lisp xp arm64::nfn) f)
    ;; handle_uuo() (in the lisp kernel) must not bump the PC here —
    ;; see the header deviation note.
    ;; Since the fulltag-function removal (patch 0055) a function IS its
    ;; misc-tagged uvector; %function-to-function-vector is
    ;; identity-with-typecheck.
    (setf (xp-pc-lisp xp) (uvref (%function-to-function-vector f) 0))))

;;; ppc:133-153.  rb is the register number of the stack that
;;; overflowed: the kernel's Rsp selector 31 for the control stack
;;; (arm64-exceptions.c:112 — NOT arm64::sp, which is a *registers*
;;; table index), else vsp/tsp GPR numbers.
(defun handle-stack-overflow (xp fn rb)
  (unwind-protect
       (with-xp-stack-frames (xp fn frame-ptr) ; execute body with dummy stack frame(s)
         (%error
          (make-condition
           'stack-overflow-condition
           :format-control "Stack overflow on ~a stack."
           :format-arguments (list
                              (if (eql rb 31)
                                "control"
                                (if (eql rb arm64::vsp)
                                  "value"
                                  (if (eql rb arm64::tsp)
                                    "temp"
                                    "unknown")))))
          nil frame-ptr))
    (ff-call (%kernel-import target::kernel-import-restore-soft-stack-limit)
             :unsigned-fullword rb
             :void)))
