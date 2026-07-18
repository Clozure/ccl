(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "ARM64-BACKEND"))

(defmacro define-arm64-vinsn (vinsn-name (results args &optional temps)
                              &body body)
  (%define-vinsn *arm64-backend* vinsn-name results args temps body))

(define-arm64-vinsn misc-ref-c-node (((dest :lisp))
                                     ((v :lisp)
                                      (idx :u32))
                                     ())
  ;; this range is limited
  (ldur dest (:@ v (:$ (:apply + arm64::misc-data-offset idx)))))

(define-arm64-vinsn check-exact-nargs (()
                                       ((n :u16const)))
  (cmp nargs (:$ (:apply ash n 3)))
  (b.eq :ok)
  (uuo-error-wrong-nargs)
  :ok)
                                                  
(define-arm64-vinsn save-lisp-context-no-stack-args (()
                                                     ()
                                                     ((marker-reg :imm)))
  (mov marker-reg (:$ arm64::lisp-frame-marker))
  (stp marker-reg vsp (:@! sp (:$ -32)))
  (stp fn lr (:@ sp (:$ 16)))
  (mov fn nfn))
  
(define-arm64-vinsn (vpush-register :push :node :vsp) (()
                                                       ((reg :lisp)))
  (str reg (:@! vsp (:$ (- arm64::node-size)))))

#|
;;; Save NFP: allocate an NFP frame on the C stack for unboxed temporaries.
;;; Frame layout: [saved-old-tcr.nfp at SP+0] [data at SP+8 ... SP+8+max_depth]
;;; Sets tcr.nfp = new SP so nested NFP vinsns can find the frame.
(define-arm64-vinsn save-nfp (()
                              ()       
                              ((temp :imm)))
  ((:pred > (:apply arm642-max-nfp-depth) 0)
   (ldr temp (:@ rcontext (:$ arm64::tcr.nfp)))
   (sub sp sp (:$ (:apply arm642-nfp-frame-size)))
   (str temp (:@ sp (:$ 0)))
   (add temp sp (:$ 0))
   (str temp (:@ rcontext (:$ arm64::tcr.nfp)))))

;;; Restore NFP: pop the NFP frame, restoring old tcr.nfp.
(define-arm64-vinsn restore-nfp (()
                                 ()
                                 ((temp :imm)))
  ((:pred > (:apply arm642-max-nfp-depth) 0)
   (ldr temp (:@ sp (:$ 0)))
   (str temp (:@ rcontext (:$ arm64::tcr.nfp)))
   (add sp sp (:$ (:apply arm642-nfp-frame-size)))))
|#

;;; Return from function: restore context and return.
(define-arm64-vinsn (popj :lispcontext :pop :lrRestore :jumpLR)
    (()
     ())
  (ldp fn lr (:@ sp (:$ 16)))
  (ldr vsp (:@ sp (:$ 8)))              ;ignore marker
  (add sp sp (:$ 32))
  (ret))

#|
(define-arm64-vinsn unbox-s64 (((dest :s64))
                               ((src :lisp)))
  (asr dest src (:$ arm64::fixnumshift))
  ;; is it a fixnum?
  (ands xzr src (:$ arm64::fixnummask))
  (b.eq :good)
  ;; maybe a 2-digit bignum?
  (and dest src (:$ arm64::fulltagmask))
  (cmp dest (:$ arm64::fulltag-misc))
  (b.eq :miscobj)
  :bad
  (uuo-error-reg-not-type src (:$ub arch::error-object-not-signed-byte-64))
  :miscobj
  (ldur dest (:@ src (:$ arm64::misc-header-offset)))
  (cmp dest (:$ arm64::two-digit-bignum-header))
  (b.ne :bad)
  (ldur dest (:@ src (:$ arm64::misc-data-offset)))
  :good)
|#

;;; Materialize a Lisp boolean branchlessly: DEST = CC ? t : nil, where CC
;;; is a 4-bit condition code and the NZCV flags were set by a preceding
;;; compare.  t lives at rnil + t-offset; put it in TRUE, then conditionally
;;; select it or nil (in rnil) into DEST.
(define-arm64-vinsn cond->boolean (((dest :lisp))
                                   ((cc :u8const))
                                   ((true :imm)))
  (add true rnil (:$ arm64::t-offset))
  (csel dest true rnil (:? cc)))

;;; Branch to LABEL when the CRBIT condition (a 4-bit condition code,
;;; e.g. arm64::cond-eq) holds; cbranch-false branches when it
;;; doesn't.  The condition is an operand of b.cond, and (:~ crbit)
;;; inverts it (XOR 1), exactly as on ARM32.
(define-arm64-vinsn (cbranch-true :branch) (()
                                           ((label :label)
                                            (crf :crf)
                                            (crbit :u8const)))
  (b.cond (:? crbit) label))

(define-arm64-vinsn (cbranch-false :branch) (()
                                            ((label :label)
                                             (crf :crf)
                                             (crbit :u8const)))
  (b.cond (:~ crbit) label))

(define-arm64-vinsn load-nil (((dest :lisp))
                              ())
  (mov dest rnil))

(define-arm64-vinsn (ref-constant :constant-ref) (((dest :lisp))
                                                  ((src :s16const)))
  (ldur dest (:@ fn (:$ (:apply + arm64::function.constants
                                (:apply ash src 3))))))

(define-arm64-vinsn ref-indexed-constant (((dest :lisp))
                                          ((idxreg :s64)))
  (ldr dest (:@ fn idxreg)))

(define-arm64-vinsn vframe-load (((dest :lisp))
                                  ((frame-offset :u16const)
                                   (cur-vsp :u16const)))
  (ldr dest (:@ arm64::vsp (:$ (:apply - (:apply - cur-vsp
                                                 arm64::word-size-in-bytes)
                                       frame-offset)))))


(define-arm64-vinsn test-fixnums (((dest :crf))
                                  ((x :lisp)
                                   (y :lisp))
                                  ((temp :u64)))
  (orr temp x y)
  (tst temp (:$ arm64::fixnummask)))


(define-arm64-vinsn fixnum-add-set-flags (((dest :imm)
                                           (flags (:crf 0)))
                                          ((x :imm)
                                           (y :imm)))
  (adds dest x y))

(define-arm64-vinsn (call-subprim-1 :call :subprim) (((dest :imm))
                                                     ((spno :u16const)
                                                      (x :imm))
                                                     ((temp (:u64 #.arm64::imm0))))
  (movz temp (:$ spno))
  (ldr temp (:@ rnil temp))
  (blr temp))

(define-arm64-vinsn (call-subprim-2 :call :subprim) (((dest :imm))
                                                     ((spoffset :u16const)
                                                      (x :imm)
                                                      (y :imm))
                                                     ((temp (:u64 #.arm64::imm0))))
  (movz temp (:$ spoffset))
  (ldr temp (:@ rnil temp))
  (blr temp))

(define-arm64-vinsn (jump :jump) (()
                                  ((label :label)))
  (b label))


#|
(define-arm64-vinsn fixnum-ref-c-double-float (((dest :double-float))
                                               ((base :imm)
                                                (idx :u16const)))
  (lfd dest (:apply ash idx 3) base))

(define-arm64-vinsn fixnum-ref-double-float (((dest :double-float))
                                             ((base :imm)
                                              (idx :imm)))
  (lfdx dest base idx))


(define-arm64-vinsn fixnum-set-c-double-float (()
                                               ((base :imm)
                                                (idx :u16const)
                                                (val :double-float)))
  (stfd val (:apply ash idx 3) base))

(define-arm64-vinsn fixnum-set-double-float (()
                                             ((base :imm)
                                              (idx :imm)
                                              (val :double-float)))
  (stfdx val base idx))

(define-arm64-vinsn ivector-typecode-p (((dest :lisp))
                                        ((src :lisp))
                                        ((temp :u64)
                                         (mask :u64)))
  (srdi temp src arm64::fixnumshift)
  
  (clrldi temp temp (- 64 arm64::ntagbits))
  (li mask 1)
  (sld mask mask temp)
  (andi. mask mask (logior (ash 1 arm64::fulltag-immheader-0)
                           (ash 1 arm64::fulltag-immheader-1)
                           (ash 1 arm64::fulltag-immheader-2)
                           (ash 1 arm64::fulltag-immheader-3)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src))
  (bne :done)
  (mr dest arm::rzero)
  :done)

(define-arm64-vinsn gvector-typecode-p (((dest :lisp))
                                        ((src :lisp))
                                        ((temp :u64)
                                         (mask :u64)))
  (srdi temp src arm64::fixnumshift)
  
  (clrldi temp temp (- 64 arm64::ntagbits))
  (li mask 1)
  (sld mask mask temp)
  (andi. mask mask (logior (ash 1 arm64::fulltag-nodeheader-0)
                           (ash 1 arm64::fulltag-nodeheader-1)
                           (ash 1 arm64::fulltag-nodeheader-2)
                           (ash 1 arm64::fulltag-nodeheader-3)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src))
  (bne :done)
  (mr dest arm::rzero)
  :done)
|#

;;; Real part is lane 0, imaginary part lane 1.
(define-arm64-vinsn %complex-single-float-realpart (((dest :single-float))
                                                    ((src :complex-single-float)))
  (dup dest (:s src 0)))

(define-arm64-vinsn %complex-double-float-realpart (((dest :double-float))
                                                    ((src :complex-double-float)))
  (dup dest (:d src 0)))

(define-arm64-vinsn %complex-single-float-imagpart (((dest :single-float))
                                                    ((src :complex-single-float)))
  (dup dest (:s src 1)))

(define-arm64-vinsn %complex-double-float-imagpart (((dest :double-float))
                                                    ((src :complex-double-float)))
  (dup dest (:d src 1)))

(define-arm64-vinsn %make-complex-single-float (((dest :complex-single-float))
                                                ((r :single-float)
                                                 (i :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value r) (:apply %hard-regspec-value dest)))
   (fmov (:s dest) r))
  (ins (:s dest 1) (:s i 0)))

(define-arm64-vinsn %make-complex-double-float (((dest :complex-double-float))
                                                ((r :double-float)
                                                 (i :double-float)))
  ((:not (:pred = (:apply %hard-regspec-value r) (:apply %hard-regspec-value dest)))
   (fmov (:d dest) r))
  (ins (:d dest 1) (:d i 0)))

;;; Population count of an unboxed 64-bit word.  AArch64 has no GPR popcount,
;;; so go through the SIMD side: move the word into a vector register, count
;;; the bits in each of the 8 bytes (CNT), horizontally sum the byte counts
;;; (ADDV) into a byte scalar, and read that back into a GPR.  The count fits
;;; in a byte (<= 64), so the low 32 bits carry it and the :w read zeroes the
;;; rest of DEST.
(define-arm64-vinsn u64-popcount (((dest :u64))
                                  ((src :u64))
                                  ((vtmp :double-float)))
  (fmov vtmp src)
  (cnt (:8b vtmp) (:8b vtmp))
  (addv (:b vtmp) (:8b vtmp))
  (fmov (:w dest) (:s vtmp)))


;;; Reconcile the template ordinals baked into the vinsns just defined
;;; with the assembler's current template table, in case this file was
;;; compiled against a differently-ordered table.
(fixup-arm64-vinsn-templates)
