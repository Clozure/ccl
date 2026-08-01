(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "ARM64-BACKEND"))

(defmacro define-arm64-vinsn (vinsn-name (results args &optional temps)
                              &body body)
  (%define-vinsn *arm64-backend* vinsn-name results args temps body))

(define-arm64-vinsn misc-ref-c-node (((dest :lisp))
                                     ((v :lisp)
                                      (idx :s16const))
                                     ())
  ;; this range is limited
  (ldur dest (:@ v (:$ (:apply + arm64::misc-data-offset (:apply ash idx 3))))))

(define-arm64-vinsn check-exact-nargs (()
                                       ((n :u16const)))
  (cmp nargs (:$ (:apply ash n 3)))
  (b.eq :ok)
  (uuo-error-wrong-number-of-args)
  :ok)
                                                  
(define-arm64-vinsn save-lisp-context-no-stack-args (()
                                                     ()
                                                     ((marker-reg :imm)))
  (mov marker-reg (:$ arm64::lisp-frame-marker))
  (stp marker-reg vsp (:@! sp (:$ -32)))
  ;; If the gc runs here, it would ordinarily get confused because it
  ;; will treat whatever garbage happens to be on the stack as savefn
  ;; and savelr.  We arrange for pc_luser_xp() to recognize this
  ;; situation and ensure that the stack slots in question contain
  ;; gc-safe content.
  (stp fn lr (:@ sp (:$ 16)))
  (mov fn nfn))
  
(define-arm64-vinsn (vpush-register :push :node :vsp) (()
                                                       ((reg :lisp)))
  (str reg (:@! vsp (:$ (- arm64::node-size)))))

;;; Save NFP: build a u64-vector-encapsulated NFP frame on the control stack
;;; for unboxed temporaries, and link it onto tcr.nfp.  Layout (see
;;; ARM642-NFP-FRAME-SIZE): [header][saved tcr.nfp = element 0][data...].
;;;
;;; The frame is constant-size (ARM642-MAX-NFP-DEPTH is known at
;;; compile time), so the pre-indexed STP builds it atomically: a GC
;;; at any instruction boundary sees either the old SP or a complete
;;; self-describing ivector whose header covers the whole (unboxed)
;;; frame, so skip_over_ivector skips it.
;;;
;;; The frame must fit the STP scaled-imm7 reach (<= 512 bytes).
(define-arm64-vinsn save-nfp (()
                              ()
                              ((header :u64)
                               (nfp :imm)))
  ((:pred > (:apply arm642-max-nfp-depth) 0)
   (ldr nfp (:@ rcontext (:$ arm64::tcr.nfp)))    ;nfp = old tcr.nfp (the link)
   (movz header (:$ (:apply logand (:apply arm642-nfp-header) #xffff)))
   (movk header (:$ (:apply logand
                            (:apply ash (:apply arm642-nfp-header) -16)
                            #xffff)
                 :lsl 16))
   ;; create u64-vector in one instruction
   (stp header nfp (:@! sp (:$ (:apply - (:apply arm642-nfp-frame-size)))))
   (add nfp sp (:$ 0))                           ;nfp = new frame base
   (str nfp (:@ rcontext (:$ arm64::tcr.nfp))))) ;tcr.nfp = frame base

;;; Restore NFP: unlink and pop the frame, restoring the saved tcr.nfp.
(define-arm64-vinsn restore-nfp (()
                                 ()
                                 ((nfp :imm)))
  ((:pred > (:apply arm642-max-nfp-depth) 0)
   (ldr nfp (:@ sp (:$ arm64::node-size)))        ;nfp = saved link (element 0)
   (str nfp (:@ rcontext (:$ arm64::tcr.nfp)))    ;restore tcr.nfp
   (add sp sp (:$ (:apply arm642-nfp-frame-size)))))

;;; NFP single-float access.  The datum lives at frame-base + dnode-size +
;;; offset -- past the u64-vector header word and the saved-nfp link (element
;;; 0); see ARM642-NFP-FRAME-SIZE.  A single-float is an S-view FP register, so
;;; a plain STR/LDR with a scaled (by 4) offset does the job.  The direct forms
;;; use SP as the frame base; the -nested forms reload the base from tcr.nfp,
;;; used when an intervening undo point (catch/unwind-protect/dynamic-extent)
;;; has moved SP off the frame.
(define-arm64-vinsn (nfp-store-single-float :nfp :set)
    (()
     ((val :single-float)
      (offset :u16const)))
  (str val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-store-single-float-nested :nfp :set)
    (()
     ((val :single-float)
      (offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (str val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-single-float :nfp :ref)
    (((val :single-float))
     ((offset :u16const)))
  (ldr val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-single-float-nested :nfp :ref)
    (((val :single-float))
     ((offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (ldr val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

;;; NFP double-float access.  Identical to the single-float forms except the
;;; datum is a D-view FP register, so the scaled STR/LDR offset is by 8.  (NFP
;;; slots are 8-byte-granular -- see ARM642-MAX-NFP-DEPTH -- so dnode-size +
;;; offset is always a multiple of 8, satisfying the scaled encoding.)
(define-arm64-vinsn (nfp-store-double-float :nfp :set)
    (()
     ((val :double-float)
      (offset :u16const)))
  (str val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-store-double-float-nested :nfp :set)
    (()
     ((val :double-float)
      (offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (str val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-double-float :nfp :ref)
    (((val :double-float))
     ((offset :u16const)))
  (ldr val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-double-float-nested :nfp :ref)
    (((val :double-float))
     ((offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (ldr val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

;;; NFP unboxed-word (natural) access.  An unboxed natural is a full 64-bit
;;; machine word, so a plain integer STR/LDR (the :x template) does it -- same
;;; frame layout and offset as the float forms.
(define-arm64-vinsn (nfp-store-unboxed-word :nfp :set)
    (()
     ((val :u64)
      (offset :u16const)))
  (str val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-store-unboxed-word-nested :nfp :set)
    (()
     ((val :u64)
      (offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (str val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-unboxed-word :nfp :ref)
    (((val :u64))
     ((offset :u16const)))
  (ldr val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-unboxed-word-nested :nfp :ref)
    (((val :u64))
     ((offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (ldr val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

;;; NFP complex-single-float access.  A complex-single-float is two packed
;;; single-floats = 64 bits, held in the low (D) half of an FP register, so it
;;; spills exactly like a double-float: one 64-bit STR/LDR (the :d template).
;;; It is 8 bytes (NOT :uses-frame-pointer).  The compiler's store path
;;; currently routes these through nfp-store-double-float -- the same 64-bit
;;; store -- while the load path uses these to land the value in a
;;; complex-single-float-classed register.
(define-arm64-vinsn (nfp-store-complex-single-float :nfp :set)
    (()
     ((val :complex-single-float)
      (offset :u16const)))
  (str val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-store-complex-single-float-nested :nfp :set)
    (()
     ((val :complex-single-float)
      (offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (str val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-complex-single-float :nfp :ref)
    (((val :complex-single-float))
     ((offset :u16const)))
  (ldr val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-complex-single-float-nested :nfp :ref)
    (((val :complex-single-float))
     ((offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (ldr val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

;;; NFP complex-double-float access.  A complex-double-float is two
;;; doubles = 128 bits, a full Q register, so it occupies a 16-byte
;;; slot (:uses-frame-pointer) and is spilled with a single 128-bit
;;; store/load (the :q template, added to arm64-asm.lisp).  Unscaled
;;; STUR/LDUR so the offset needn't be 16-aligned -- the nfp offset
;;; accounting is only 8-granular.  The +-256 simm9 reach caps the
;;; complex-double offset; a much larger frame would want scaled
;;; STR/LDR Q with 16-aligned slots instead.
(define-arm64-vinsn (nfp-store-complex-double-float :nfp :set :uses-frame-pointer)
    (()
     ((val :complex-double-float)
      (offset :u16const)))
  (stur val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-store-complex-double-float-nested :nfp :set :uses-frame-pointer)
    (()
     ((val :complex-double-float)
      (offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (stur val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-complex-double-float :nfp :ref :uses-frame-pointer)
    (((val :complex-double-float))
     ((offset :u16const)))
  (ldur val (:@ sp (:$ (:apply + arm64::dnode-size offset)))))

(define-arm64-vinsn (nfp-load-complex-double-float-nested :nfp :ref :uses-frame-pointer)
    (((val :complex-double-float))
     ((offset :u16const)))
  (ldr temp5 (:@ rcontext (:$ arm64::tcr.nfp)))
  (ldur val (:@ temp5 (:$ (:apply + arm64::dnode-size offset)))))

;;; Return from function: restore context and return.
(define-arm64-vinsn (popj :lispcontext :pop :lrRestore :jumpLR)
    (()
     ())
  (ldp fn lr (:@ sp (:$ 16)))
  (ldr vsp (:@ sp (:$ 8)))              ;ignore marker
  (add sp sp (:$ 32))
  (ret))

;;; Allocating C frames for calling foreign functions
;;;
;;; C frames reside on the control stack (the hardware SP) where
;;; foreign code and the AAPCS64 standard require them.
;;;
;;; Because the control stack is scanned by the GC (there are roots in
;;; the lisp-frame, namely savefn and savelr), it has to have some way
;;; to recognize a C frame and skip over it.  We do that by creating a
;;; u64-vector to encapsulate the frame: the GC (and Lisp backtrace)
;;; can easily recognize this and skip over it.
;;;
;;; Right after the header, in element 0 of the vector, we store the
;;; saved previous SP (so that discard-c-frame can pop the frame easily).
;;;
;;; Following the saved previous SP is the outgoing stack-argument
;;; area, and finally 4 words to reserve space for the boundary
;;; lisp-frame.
;;;
;;; The operation of putting the u64-vector on the control stack is
;;; straightforward, but the ff-call sequence is a bit tricky.
;;;
;;; We said that the u64-vector covers the C frame as well as the
;;; reserved lisp frame.  So, ff-call will build the lisp frame at
;;; [prevsp - lisp-frame.size], writing the marker and savevsp, and
;;; writing zeros for savefn and savelr.  This is all gc-safe, because
;;; everything is still hidden inside the u64-vector.
;;;
;;; Now, ff-call will shrink the u64-vector's header element count by
;;; 4 elements, thereby exposing/publishing the lisp-frame.  If the gc
;;; runs between at this point, it's still safe, because savefn and
;;; savelr hold harmless zeros.
;;;
;;; Finally, ff-call can stp the real fn and lr.  It's *not* safe to
;;; pre-store savefn and savelr while still inside the u64-vector,
;;; because the gc won't see them (and therefore won't update them).
;;; With the frame in place, savefn will be kept alive and our caller
;;; won't be gc'd out from under us.

;;; Allocate a C frame whose size is known at compile-time.  A big
;;; frame, beyond the stp scaled-imm7 reach (>63 words), must use
;;; alloc-variable-c-frame.
(define-arm64-vinsn (alloc-c-frame) (()
                                     ((n-c-args :u16const))
                                     ((header :u64)
                                      (prevsp :imm)))
  (mov prevsp sp)
  (movz header (:$ (:apply logand (:apply arm642-c-frame-header n-c-args)
                           #xffff)))
  (movk header (:$ (:apply logand
                           (:apply ash (:apply arm642-c-frame-header n-c-args)
                                   -16)
                           #xffff)
                :lsl 16))
  (stp header prevsp
       (:@! sp (:$ (:apply - (:apply ash (:apply arm642-c-frame-words n-c-args)
                                     arm64::word-shift))))))

;;; Allocate a C frame whose size is specified at run-time.  We can't
;;; write stp imm0, imm1, [sp, sizereg]! because pre-indexing only
;;; works with an immediate offset.
;;;
;;; The header and prevsp registers are pinned to imm0/imm1 so
;;; pc_luser_xp can recognize the stp by its exact encoding and know
;;; which registers to read from the saved context.
(define-arm64-vinsn (alloc-variable-c-frame) (()
                                              ((n-c-args :lisp))
                                              ((header (:u64 #.arm64::imm0))
                                               (size :u64)
                                               (prevsp (:imm #.arm64::imm1))))
  (add size n-c-args (:$ '6))        ;+ header + prevsp + 4-word frame
  (add size size (:$ (:apply 1- arm64::dnode-size))) ;round byte size up...
  (and size size (:$ (:apply - arm64::dnode-size)))  ; ...to a dnode boundary
  (sub header size (:$ '1))          ;element count (omit header word)
  ;; shift already-fixnum-scaled element count into place
  (lsl header header (:$ (:apply - arm64::num-subtag-bits arm64::fixnumshift)))
  (add header header (:$ arm64::subtag-u64-vector))
  (mov prevsp sp)
  (sub sp sp size)
  ;; If the gc runs here, it would ordinarily get confused, but
  ;; pc_luser_xp recognizes this case and will finish the stp.
  (stp header prevsp (:@ sp (:$ 0))))

;;; Pop a C frame by restoring the saved previous SP (element 0, at SP+8).
;;; A load can't target SP directly on AArch64 (Rt=31 is XZR), so go via a
;;; temp.  Rarely needed -- e.g. a non-local exit out of a foreign-call
;;; argument form.
(define-arm64-vinsn (discard-c-frame :pop :discard) (()
                                                     ()
                                                     ((prevsp :imm)))
  (ldr prevsp (:@ sp (:$ arm64::node-size)))
  (mov sp prevsp))

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

(define-arm64-vinsn (load-nil :constant-ref) (((dest :lisp))
                                              ())
  (mov dest rnil))

(define-arm64-vinsn (load-t :constant-ref) (((dest :lisp))
                                            ())
  (add dest rnil (:$ arm64::t-offset)))

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
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (call-subprim-2 :call :subprim) (((dest :imm))
                                                     ((spoffset :u16const)
                                                      (x :imm)
                                                      (y :imm))
                                                     ((temp (:u64 #.arm64::imm0))))
  (movz temp (:$ spoffset))
  (ldr temp (:@ rcontext temp))
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

;;; FPR-to-FPR copies, for arm642-copy-register and arm642-copy-fpr.
;;; PPC64 (ppc64-vinsns.lisp:2047) covers the same ground with three
;;; vinsns, and its dest-eq-src guard is kept verbatim here: the emit
;;; sites do not all promise distinct registers.
;;;
;;; PPC64 needs no single<->double copy because a PPC FPR holds a single
;;; in double format, so its copy-fpr serves both modes; AArch64 keeps
;;; the two in genuinely different formats, hence the fcvt pair below.
(define-arm64-vinsn copy-single-float (((dest :single-float))
                                       ((src :single-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fmov dest src)))

(define-arm64-vinsn copy-double-float (((dest :double-float))
                                       ((src :double-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fmov dest src)))

;;; Not guarded: an in-place precision conversion is not a no-op, so
;;; dest eq src still has to emit the fcvt.
(define-arm64-vinsn copy-double-to-single (((dest :single-float))
                                           ((src :double-float)))
  (fcvt dest src))

(define-arm64-vinsn copy-single-to-double (((dest :double-float))
                                           ((src :single-float)))
  (fcvt dest src))

;;; A complex-single-float is one D register (two S lanes), so a single
;;; D-view fmov copies both parts.  PPC64 and ARM32 both need two moves
;;; here because they hold a complex float in a register PAIR; doing that
;;; on AArch64 would clobber an unrelated register.
(define-arm64-vinsn copy-complex-single-float (((dest :complex-single-float))
                                               ((src :complex-single-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fmov (:d dest) (:d src))))

;;; A complex-double-float is a full 128-bit vector register.  There is
;;; no register-to-register move for one: :q appears only in the
;;; str/ldr/stur/ldur templates, and no template takes a vector
;;; arrangement pair.  Copy it the same way %make-complex-double-float
;;; builds it -- fmov of lane 0 (which zeroes bits 127:64), then ins of
;;; lane 1.
(define-arm64-vinsn copy-complex-double-float (((dest :complex-double-float))
                                               ((src :complex-double-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fmov (:d dest) (:d src))
   (ins (:d dest 1) (:d src 1))))

;;; There's no popcount for a GPR, but there is on SIMD registers.
(define-arm64-vinsn u64-popcount (((dest :u64))
                                  ((src :u64))
                                  ((vtmp :double-float)))
  (fmov vtmp src)
  (cnt (:8b vtmp) (:8b vtmp))           ;count set bits in each element
  (addv (:b vtmp) (:8b vtmp))           ;add elements across
  (fmov (:w dest) (:s vtmp)))           ;move to gpr


(define-arm64-vinsn logior-imm (((dest :imm))
                                ((src :imm)
                                 (mask :s64const)))
  (orr dest src (:$ mask)))

(define-arm64-vinsn %logior2 (((dest :imm))
                              ((r1 :imm)
                               (r2 :imm)))
  (orr dest r1 r2))

(define-arm64-vinsn logand-imm (((dest :imm))
                                ((src :imm)
                                 (mask :s64const)))
  (and dest src (:$ mask)))

(define-arm64-vinsn %logand2 (((dest :imm))
                              ((r1 :imm)
                               (r2 :imm)))
  (and dest r1 r2))

(define-arm64-vinsn logxor-imm (((dest :imm))
                                ((src :imm)
                                 (mask :s64const)))
  (eor dest src (:$ mask)))

(define-arm64-vinsn %logxor2 (((dest :imm))
                              ((r1 :imm)
                               (r2 :imm)))
  (eor dest r1 r2))

(define-arm64-vinsn copy-node-gpr (((dest :lisp))
                                   ((src :lisp)))
  ((:not (:pred = (:apply %hard-regspec-value dest)
                  (:apply %hard-regspec-value src)))
   (mov dest src)))

(define-arm64-vinsn copy-gpr (((dest t))
                              ((src t)))
  ((:not (:pred = (:apply %hard-regspec-value dest)
                  (:apply %hard-regspec-value src)))
   (mov (:x dest) (:x src))))

(define-arm64-vinsn (lri :constant-ref) (((dest :imm))
                                         ((const :u64const)))
  ((:pred arm64::encode-logical-immediate const)
   (orr dest xzr (:$ const)))
  ((:not (:pred arm64::encode-logical-immediate const))
   (movz dest (:$ (:apply logand #xffff const)))
   ((:pred /= (:apply logand #xffff (:apply ash const -16)) 0)
    (movk dest (:$ (:apply logand #xffff (:apply ash const -16)) :lsl 16)))
   ((:pred /= (:apply logand #xffff (:apply ash const -32)) 0)
    (movk dest (:$ (:apply logand #xffff (:apply ash const -32)) :lsl 32)))
   ((:pred /= (:apply logand #xffff (:apply ash const -48)) 0)
    (movk dest (:$ (:apply logand #xffff (:apply ash const -48)) :lsl 48)))))

(define-arm64-vinsn (vpop-register :pop :node :vsp) (((dest :lisp))
                                                     ())
  (ldr dest (:@+ vsp (:$ arm64::node-size))))
;;; ------------------------------------------------------------------
;;; linuxarm64: the port's vinsn layer, promoted out of the compiler overlay
;;; (upstream-port/compiler/arm64-vinsns-additions*.lisp -- 16 files, deleted
;;; in the same commit).  Forms appear in the order our build concatenated the
;;; fasls, so the definition that wins here is the one that won when the suite
;;; was measured.  Each definition keeps its own provenance comment naming the
;;; PPC64 donor it was ported from.
;;; ------------------------------------------------------------------

;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; arm64-vinsns-additions-w13.lisp -- wave 13: the aapcs64-ff-call
;;; codegen unit (vinsns + c-frame layout constants), built to Matt's
;;; msg-29 outline (comms/emerson-thread-archive.md, id
;;; 19f6c4d7d174b28b): C frame on the CONTROL stack, no separate
;;; foreign stack; the .SPffcall subprim (his arm64-arch.lisp:530
;;; defsubprim) saves Lisp state, marks foreign valence, loads the
;;; outgoing register args FROM THE FRAME, calls, and restores.
;;;
;;; Loadable AFTER arm64-vinsns.lisp + all earlier waves (no name here
;;; collides -- checked against w1-w12 2026-07-16).  Donor lineage: v2
;;; compiler/ARM64/arm64-vinsns.lisp:3561-3696 (the battle-tested
;;; AAPCS64 family -- fd-lseek/strace-verified arg marshaling) recut
;;; onto HIS frame layout below; PPC64 ppc64-vinsns.lisp:2270-2348 is
;;; the logic lineage of that donor.
;;;
;;; ============ c-frame layout (HIS, verified 16m30) ============
;;; !! This block used to describe {backlink@0, savelr@8}.  That was WRONG and
;;; cost a boot: his alloc-c-frame vinsn (arm64-vinsns.lisp:287) emits
;;; `mov prevsp sp / movz+movk header / stp header prevsp [sp,-size]!', i.e.
;;; HEADER first and the saved SP second, and he reserves 4 words at the frame
;;; TOP for a boundary lisp_frame (arm642-c-frame-words, arm642.lisp:6323).
;;; The kernel side is upstream-port/patches/0003 (_struct c_frame) and the
;;; canonical note is `spentry ffcall' in upstream-port/lisp-kernel/arm64-spentry.s.
;;; NB these constants are DECLARED here but never referenced by any vinsn body
;;; (the param stores use c-frame.param0, which was already correct at 16).
;;; Grows down from the SP current at alloc-c-frame time:
;;;   [sp+0]   header -- a subtag-u64-vector header whose element count covers
;;;            everything after it, INCLUDING the 4 reserved words, so the GC
;;;            skips them until .SPffcall shrinks the count by 4 to publish
;;;            the boundary lisp_frame built there.
;;;   [sp+8]   savedsp -- the SP value before allocation (his "element 0").
;;;   [sp+16]  param0..param7: the 8 AAPCS64 GPR argument words, to be
;;;            loaded into x0-x7 by .SPffcall.
;;;   [sp+80]  overflow (stack) argument words, contiguous -- .SPffcall
;;;            must leave SP pointing HERE at the moment of the call
;;;            (load x0-x7, then effectively pop header+params).
;;;   above    FP staging words (singles, then 16-byte-slotted doubles);
;;;            compiled code reloads d0-d7 from these BEFORE the
;;;            subprim call, so they are dead at call time and their
;;;            layout is private to the compiler.
;;; Total kept 16-aligned (AAPCS64 SP discipline).  The param/overflow/
;;; staging words hold RAW (untagged) data on the control stack; the
;;; kernel's cstack walk must treat the region below the backlink of a
;;; frame owned by .SPffcall as unboxed -- same obligation his
;;; catch-frame/lisp-frame walk already carries.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'arm64::c-frame.header)
    (defconstant arm64::c-frame.header 0)
    (defconstant arm64::c-frame.savedsp 8)
    (defconstant arm64::c-frame.param0 16)
    (defconstant arm64::c-frame.minsize 16))
  ;; The canonical GPR-only (<=8 argument) ffcall frame, for HAND-WRITTEN
  ;; LAP that must build one itself -- level-0/ARM64/arm64-def.lisp
  ;; %make-code-executable is the only such site on this lane; compiled
  ;; code gets the equivalent from his alloc-c-frame vinsn and there is no
  ;; LAP macro for it here.  2 head words + 8 GPR param words + the 4
  ;; reserved boundary-lisp_frame words = 14 words / 112 bytes, and the
  ;; header's element count (words-1 = 13) deliberately COVERS the
  ;; reserved words, which is exactly what _SPffcall reads to locate the
  ;; boundary frame before shrinking the count by 4 to publish it.
  (unless (boundp 'arm64::c-frame.ffcall-size)
    (defconstant arm64::c-frame.ffcall-size 112)
    (defconstant arm64::c-frame.ffcall-header
      (logior (ash 13 arm64::num-subtag-bits) arm64::subtag-u64-vector))))
;;; ============ set-nargs ============
;;; Donor: v2 vinsn-retrofit-queue.lisp:639 (raw count under fixnumshift=0).
;;; Under Matt's design nargs holds FIXNUM-tagged counts; mirror his own
;;; check-exact-nargs (arm64-vinsns.lisp), which writes (:$ (:apply ash n 3)).
;;; His emit site (arm642.lisp:771 arm642-set-nargs) passes the RAW count.
;;; movz, not mov: a mov with an expand-time immediate hole cannot pick
;;; among its value-multiplexed movz/movn/orr aliases (arm64-asm.lisp
;;; template-matching comment), so name the concrete instruction.
(define-arm64-vinsn set-nargs (()
                               ((n :u16const)))
  (movz nargs (:$ (:apply ash n 3))))

;;; ============ ref-constant -- DROPPED (re-sync 2026-07-14) ============
;;; His b23f340 defines REF-CONSTANT and REF-INDEXED-CONSTANT in his own
;;; arm64-vinsns.lisp, addressed as (+ function.constants (ash index 3))
;;; off fn (constants @ slot 1; no entrypoint slot, no +2 skip).  fn is
;;; fulltag-misc, not a dedicated function tag: since patch 0055
;;; define-fixedsized-object function has a fulltag-misc origin, so
;;; function.constants is misc-relative.  Our former definition here
;;; encoded the deleted entrypoint-slot layout and would collide --
;;; removed.

;;; ============ restore-full-lisp-context ============
;;; Donor: v2 arm64-vinsns.lisp:5678; PPC64 original ppc64-vinsns.lisp:3581.
;;; Authored as the exact inverse of his save-lisp-context-no-stack-args
;;; (frame: marker@sp+0, vsp@sp+8, fn@sp+16, lr@sp+24; 32 bytes) and,
;;; equivalently, as his live popj minus the final (ret) -- his popj already
;;; restores the full context inline, so this is the same teardown for the
;;; paths (e.g. tail calls: restore-full-lisp-context + jump-known-symbol,
;;; his arm642.lisp:716-725) that must not return yet.
;;; Attributes follow his popj (:lispcontext :pop :lrRestore, minus
;;; :jumpLR); the v2/PPC64 :csp attribute is dropped because his popj
;;; doesn't carry it.
(define-arm64-vinsn (restore-full-lisp-context :lispcontext :pop :lrRestore)
    (()
     ())
  (ldp fn lr (:@ sp (:$ 16)))
  (ldr vsp (:@ sp (:$ 8)))              ;ignore marker
  (add sp sp (:$ 32)))

;;; ============ jump-known-symbol ============
;;; Donor: v2 arm64-vinsns.lisp:5877; PPC64 original ppc64-vinsns.lisp:3708:
;;;   (ld nfn symbol.fcell fname) / (ld temp0 misc-data-offset nfn)
;;;   / mtctr / bctr
;;; fname holds the symbol; load its fcell into nfn (callee register per
;;; his mail: callee builds frame and does mov fn,nfn), then dispatch
;;; through the function's code-vector.
;;; RE-SYNC 2026-07-14 (a9ab24b, msg 23): the entrypoint slot is GONE.
;;; Functions are ordinary miscobjs (fulltag-function removed, patch
;;; 0055; define-fixedsized-object function (fulltag-misc):
;;; code-vector@0, constants@1); the code-vector slot holds a
;;; fulltag-misc (#b1100) TAGGED pointer that points at element 1 = the
;;; first real instruction (element 0 is the udf #0 sentinel), so we
;;; branch to the tagged pointer itself -- his msg-23 sketch:
;;; (ldur temp0 (:@ nfn (:$ function.code-vector))) / (blr temp0).
;;; Offsets: symbol.fcell = 17 (origin -fulltag-symbol) and
;;; function.code-vector = -4 (origin -fulltag-misc) -- neither
;;; 8-aligned, so both loads are LDUR (unscaled).
;;; The loaded code-vector is a TAGGED object, so it rides in a NODE temp
;;; (his temp0 sketch) -- an unboxed imm temp would be the makeu64
;;; GC-safety class he flagged in msg 18.  The former raw-entrypoint-in-
;;; imm0 rationale died with the entrypoint slot.
;;; !! cv is WIRED to temp0 (boot-16m5d root): unwired, the allocator
;;; handed it x9 = arg_x, clobbering the 3rd argument of every >=3-arg
;;; call (observed live: %store-node-conditional received its own code
;;; vector as `object`).  Call scratch must NEVER be an allocatable
;;; arg/imm/temp-with-ABI-meaning -- v2 cont-71 class; temp0 is dead at
;;; every call boundary (callee prologue reads only nfn).
(define-arm64-vinsn (jump-known-symbol :jumplr) (()
                                                 ()
                                                 ((cv (:lisp #.arm64::temp0))))
  (ldur nfn (:@ fname (:$ arm64::symbol.fcell)))
  (ldur cv (:@ nfn (:$ arm64::function.code-vector)))
  (br cv))

;;; ============ call-known-symbol ============
;;; Donor: v2 arm64-vinsns.lisp:5871; PPC64 original ppc64-vinsns.lisp:3701.
;;; Non-tail form of jump-known-symbol (same layout evidence -- see above);
;;; result convention: value returns in arg_z, matching the emit site
;;; (! call-known-symbol arm64::arg_z)
;;; (upstream-port/compiler/arm642-additions.lisp:526).
(define-arm64-vinsn (call-known-symbol :call) (((result (:lisp #.arm64::arg_z)))
                                               ()
                                               ((cv (:lisp #.arm64::temp0))))
  (ldur nfn (:@ fname (:$ arm64::symbol.fcell)))
  (ldur cv (:@ nfn (:$ arm64::function.code-vector)))
  (blr cv))

;;; ============ jump-known-function / call-known-function ============
;;; Added at re-sync (same evidence as the known-symbol pair; emit sites
;;; already live at arm642-additions.lisp:686/702/719).  nfn already
;;; holds the misc-tagged callee (no fcell load); dispatch is
;;; the msg-23 sketch verbatim.  PPC64 originals ppc64-vinsns.lisp
;;; jump-known-function/call-known-function (codevector deref + bctr);
;;; 32-bit ARM's one-instruction (ldr pc ...) trick has no arm64 analog --
;;; the tagged-code-vector branch is the arm64 equivalent
;;; (doc/porting/arm64.md "Functions").
(define-arm64-vinsn (jump-known-function :jumplr) (()
                                                   ()
                                                   ((cv (:lisp #.arm64::temp0))))
  (ldur cv (:@ nfn (:$ arm64::function.code-vector)))
  (br cv))

;;; NO result spec: the PPC64 donor (@3715) declares none and every emit
;;; site calls (! call-known-function) with 0 vregs -- the wired-arg_z
;;; result the first draft added made Matt's emitter reject the arity
;;; ("expects 1 spec, received 0").  call-known-symbol differs: ITS donor
;;; (@3701) has the wired result and its sites pass arg_z.
(define-arm64-vinsn (call-known-function :call) (()
                                                 ()
                                                 ((cv (:lisp #.arm64::temp0))))
  (ldur cv (:@ nfn (:$ arm64::function.code-vector)))
  (blr cv))

;;; ============ %unbox-u32 ============
;;; Donor: vinsn-retrofit-queue.lisp:8 (mov under shift-0); PPC64 original
;;; ppc64-vinsns.lisp:1376: (rldicl dest src (- 64 fixnumshift) 32) =
;;; rotate-right by fixnumshift keeping low 32 = extract bits
;;; [fixnumshift, fixnumshift+32) -- ubfx.  sbfx/ubfx are LAPMACROS in his
;;; tree (arm64-lapmacros.lisp:18), NOT instruction templates, and vinsn
;;; bodies don't expand lapmacros -- write the underlying UBFM
;;; (immr = lsb, imms = lsb+width-1).  dest is :u32 (W-width class), but
;;; the extract reads bits above 31 of src: force the X view, (:x dest).
(define-arm64-vinsn %unbox-u32 (((dest :u32))
                                ((src :lisp)))
  (ubfm (:x dest) src (:$ arm64::fixnumshift)
        (:$ (+ arm64::fixnumshift 31))))

;;; ============ unbox-u32 ============
;;; Checking variant of %unbox-u32 (distinct vinsn in every backend):
;;; PPC64 ppc64-vinsns.lisp:1369 traps unless src is a fixnum in
;;; [0, 2^32) then extracts.  Check = w3b require-u32's TST with
;;; ~(#xffffffff<<fixnumshift) (mask encodable: 32-one wraparound run);
;;; extract = %unbox-u32's UBFM.  Trap idiom = the established
;;; brk #xf0xx (w3b:501 uses this exact error code).
(define-arm64-vinsn unbox-u32 (((dest :u32))
                               ((src :lisp)))
  :again
  (tst src (:$ (logand #xffffffffffffffff
                       (lognot (ash #xffffffff arm64::fixnumshift)))))
  (b.eq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm64::xtype-u32))
  (b :again)
  :got-it
  (ubfm (:x dest) src (:$ arm64::fixnumshift)
        (:$ (+ arm64::fixnumshift 31))))

;;; ============ %unbox-u8 ============
;;; Donor: vinsn-retrofit-queue.lisp:14; PPC64 ppc64-vinsns.lisp:1436:
;;; (rldicl dest src (- 64 fixnumshift) 56) = extract 8 bits at
;;; fixnumshift.  Unsafe (caller type-checked), same UBFM story as
;;; %unbox-u32.
(define-arm64-vinsn %unbox-u8 (((dest :u8))
                               ((src :lisp)))
  (ubfm (:x dest) src (:$ arm64::fixnumshift)
        (:$ (+ arm64::fixnumshift 7))))

;;; ============ adjust-sp ============
;;; Donor: vinsn-retrofit-queue.lisp:20; PPC64 ppc64-vinsns.lisp:2797
;;; (la sp amount sp) -- amount is SIGNED (:s16const).  AArch64
;;; add/sub-immediate is unsigned 12-bit; a negative amount must become
;;; SUB (the s61/s72/s89 negative-immediate truncation class) -- split on
;;; the sign with expand-time predicates.  Keeping SP 16-aligned is the
;;; emit site's contract (arm642-unwind-stack passes cstack deltas).
;;; w10 fix: full s16 window via the add-immediate two-lane split
;;; (aimm is u12 + u12<<12; the compare-signed-s16const class).
(define-arm64-vinsn adjust-sp (()
                               ((amount :s16const)))
  ((:pred >= amount 0)
   ((:not (:pred = 0 (:apply ldb (byte 12 0) amount)))
    (add sp sp (:$ (:apply ldb (byte 12 0) amount)))
    ((:not (:pred = 0 (:apply ldb (byte 12 12) amount)))
     (add sp sp (:$ (:apply ldb (byte 12 12) amount) :lsl 12))))
   ((:pred = 0 (:apply ldb (byte 12 0) amount))
    (add sp sp (:$ (:apply ldb (byte 12 12) amount) :lsl 12))))
  ((:pred < amount 0)
   ((:not (:pred = 0 (:apply ldb (byte 12 0) (:apply - amount))))
    (sub sp sp (:$ (:apply ldb (byte 12 0) (:apply - amount))))
    ((:not (:pred = 0 (:apply ldb (byte 12 12) (:apply - amount))))
     (sub sp sp (:$ (:apply ldb (byte 12 12) (:apply - amount)) :lsl 12))))
   ((:pred = 0 (:apply ldb (byte 12 0) (:apply - amount)))
    (sub sp sp (:$ (:apply ldb (byte 12 12) (:apply - amount)) :lsl 12)))))

;;; ============ call-subprim ============
;;; Donor: vinsn-retrofit-queue.lisp:27 (v2 sptab dispatch off rcontext).
;;; Emit sites: his own arm642.lisp:903/:913 (! call-subprim
;;; (subprim-name->offset '.SPmakes64 / '.SPmakeu64)).
;;; BASE = RCONTEXT (boot-16m5 root): the sptab lives in each thread's
;;; TCR (tcr.sptab=496; init_arm_tcr_sptab copies the kernel RELRO table;
;;; his arm64-spentry.s:176-184 documents `ldr xN,[rcontext,#(tcr.sptab+
;;; n*8)]; blr`).  His call-subprim-1/2 vinsns still say rnil -- STALE:
;;; [rnil+off] lands inside the NRS symbol records (observed: gvector
;;; dispatch read a misaligned splice of two NRS slots and jumped to it).
;;; Patched in his tree too (patch 0011); mail-pile ratify item.
;;; DISPATCH SCRATCH IS imm1, NOT imm0 (U2): both of his emit sites write
;;; the subprim's ARGUMENT into imm0 immediately before this vinsn
;;; (arm642-box-s64/-u64 copy the raw value to imm0), so an imm0 scratch
;;; would destroy the argument -- the exact v2 cont-71 bug class
;;; (dispatch scratch colliding with subprim ABI inputs).
(define-arm64-vinsn (call-subprim :call :subprim) (()
                                                   ((spno :u16const))
                                                   ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ spno))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ check-max-nargs ============
;;; Donor: vinsn-retrofit-queue.lisp:33 (raw count + hlt); PPC64
;;; ppc64-vinsns.lisp:3457: (tdlgti nargs (ash max word-shift)) --
;;; unsigned trap-if-greater.  nargs holds FIXNUM-tagged counts under
;;; Matt: compare to (ash max 3), literally mirroring his
;;; check-exact-nargs and his check-nargs lapmacro's max branch
;;; (cmp / b.ls / uuo-error-wrong-nargs).  The too-many-args UUO exists
;;; in his template table and is more precise than wrong-nargs here.
(define-arm64-vinsn check-max-nargs (()
                                     ((max :u16const)))
  (cmp nargs (:$ (:apply ash max 3)))
  (b.ls :ok)
  (uuo-error-too-many-args)
  :ok)

;;; ============ check-min-nargs ============
;;; Donor: vinsn-retrofit-queue.lisp:41; PPC64 ppc64-vinsns.lisp:3453:
;;; (tdllti nargs (ash min word-shift)) -- unsigned trap-if-less.
;;; Same transform as check-max-nargs; uuo-error-too-few-args.
(define-arm64-vinsn check-min-nargs (()
                                     ((min :u16const)))
  (cmp nargs (:$ (:apply ash min 3)))
  (b.hs :ok)
  (uuo-error-too-few-args)
  :ok)

;;; ============ compare-to-nil ============
;;; Donor: vinsn-retrofit-queue.lisp:49; PPC64 ppc64-vinsns.lisp:1849:
;;; (cmpdi crf arg0 (target-nil-value)).  NIL is not a small immediate
;;; (canonical-nil #x1300b, and on Darwin not even a link-time constant --
;;; his rnil design); compare against the pinned rnil register, the same
;;; deviation v2 landed after the s61 truncated-immediate bug.  Result is
;;; a :crf vreg (the emit site passes one: his arm642.lisp:1040-1041);
;;; NZCV is the single flags register, set implicitly by cmp.
;;; arg0's class is t (any GPR-holdable value): class t carries no width,
;;; so force the X view in the body.
(define-arm64-vinsn compare-to-nil (((dest :crf))
                                    ((arg0 t)))
  (cmp (:x arg0) rnil))

;;; ============ copy-complex-{double,single}-float ============
;;; PROMOTED 16m48g (2026-07-28) into upstream patch 0061 together with
;;; w9's four scalar FPR copies; see the note there.  Deleted here because
;;; our additions fasls concatenate AFTER his arm64-vinsns fasl, so a
;;; leftover copy would silently win over the patched one.  The bodies
;;; went upstream unchanged (they already carried PPC64's dest-eq-src
;;; guard and Matt's one-register lane model).


;;; ============ copy-lexpr-argument ============
;;; Donor: vinsn-retrofit-queue.lisp:89; PPC64 ppc64-vinsns.lisp:2462:
;;;   (ldx temp vsp nargs) / (stdu temp -8 vsp)
;;; PPC nargs is BYTE-scaled (fixnum, shift 3 = word-shift) and is used
;;; directly as the byte offset of the top argument.  Matt's nargs is
;;; fixnum-tagged = count<<3 = the same byte offset -- the v2 donor's
;;; extra lsl-by-3 temp (needed at v2's shift 0) DROPS OUT and the body
;;; returns to the exact PPC64 shape: register-offset ldr + vpush.
;;; temp is WIRED to temp0: this vinsn runs between save-lexpr-argregs
;;; and save-lisp-context-lexpr, where temp4 carries the LEXPR-RA as a
;;; hidden cross-vinsn channel the allocator cannot see -- and the
;;; unwired :lisp pool INCLUDES temp4 (lib/arm64env.lisp:33
;;; arm64-temp-node-regs = temp0-4 + arg_x/y/z).  PPC's unwired temp is
;;; safe only because its RA rides loc_pc, which is not in ITS pool
;;; (16m5h unwired-temp class, third instance).
(define-arm64-vinsn copy-lexpr-argument (()
                                         ()
                                         ((temp (:lisp #.arm64::temp0))))
  (ldr temp (:@ vsp nargs))
  (str temp (:@! vsp (:$ (- arm64::node-size)))))

;;; ============ default-1-arg ============
;;; Donor: vinsn-retrofit-queue.lisp:103; PPC64 ppc64-vinsns.lisp:3611.
;;; nargs compare is fixnum-scaled ((ash min 3), his check-exact-nargs
;;; idiom); cmpldi/bne -> cmp/b.ne (one NZCV, no crf temp);
;;; stdu -8 vsp -> str (:@! vsp -8) (his vpush-register idiom);
;;; li arg_z nil -> mov arg_z rnil (his load-nil idiom).
(define-arm64-vinsn default-1-arg (()
                                   ((min :u16const)))
  (cmp nargs (:$ (:apply ash min 3)))
  (b.ne :done)
  ((:pred >= min 3)
   (str arg_x (:@! vsp (:$ (- arm64::node-size)))))
  ((:pred >= min 2)
   (mov arg_x arg_y))
  ((:pred >= min 1)
   (mov arg_y arg_z))
  (mov arg_z rnil)
  :done)

;;; ============ default-2-args ============
;;; Donor: vinsn-retrofit-queue.lisp:124; PPC64 ppc64-vinsns.lisp:3625.
;;; ONE unsigned compare (to min+1, fixnum-scaled) drives two branches
;;; reading the same NZCV (no flag-setting insn between them):
;;; bgt(unsigned cmpldi) -> b.hi, beq -> b.eq.  nargs  in  [min, min+2]
;;; (check-min/max-nargs bounded it), so the fall-through is nargs==min.
(define-arm64-vinsn default-2-args (()
                                    ((min :u16const)))
  (cmp nargs (:$ (:apply ash (:apply 1+ min) 3)))
  (b.hi :done)
  (b.eq :one)
  ;; got "min" args; arg_y & arg_z default to nil
  ((:pred >= min 3)
   (str arg_x (:@! vsp (:$ (- arm64::node-size)))))
  ((:pred >= min 2)
   (str arg_y (:@! vsp (:$ (- arm64::node-size)))))
  ((:pred >= min 1)
   (mov arg_x arg_z))
  (mov arg_y rnil)
  (b :last)
  :one
  ;; got min+1 args: arg_y supplied, arg_z defaults to nil
  ((:pred >= min 2)
   (str arg_x (:@! vsp (:$ (- arm64::node-size)))))
  ((:pred >= min 1)
   (mov arg_x arg_y))
  (mov arg_y arg_z)
  :last
  (mov arg_z rnil)
  :done)

;;; ============ default-3-args ============
;;; Donor: vinsn-retrofit-queue.lisp:156; PPC64 ppc64-vinsns.lisp:3654.
;;; PPC holds TWO CR fields live at once (crfx vs min+2, crfy vs min);
;;; ARM64 has a single NZCV, so compare-then-branch for crfx (its two
;;; branches share that NZCV), then RE-COMPARE for crfy -- the donor's
;;; established deviation, kept.  All compares fixnum-scaled.
(define-arm64-vinsn default-3-args (()
                                    ((min :u16const)))
  (cmp nargs (:$ (:apply ash (:apply + 2 min) 3)))
  (b.hi :done)
  (b.eq :two)
  (cmp nargs (:$ (:apply ash min 3)))
  (b.eq :none)
  ;; nargs==min+1: the first (of three) &optional args was supplied
  ((:pred >= min 2)
   (str arg_x (:@! vsp (:$ (- arm64::node-size)))))
  ((:pred >= min 1)
   (str arg_y (:@! vsp (:$ (- arm64::node-size)))))
  (mov arg_x arg_z)
  (b :last-2)
  :two
  ;; nargs==min+2: the first two &optionals supplied
  ((:pred >= min 1)
   (str arg_x (:@! vsp (:$ (- arm64::node-size)))))
  (mov arg_x arg_y)
  (mov arg_y arg_z)
  (b :last-1)
  :none
  ;; nargs==min: no &optional supplied
  ((:pred >= min 3)
   (str arg_x (:@! vsp (:$ (- arm64::node-size)))))
  ((:pred >= min 2)
   (str arg_y (:@! vsp (:$ (- arm64::node-size)))))
  ((:pred >= min 1)
   (str arg_z (:@! vsp (:$ (- arm64::node-size)))))
  (mov arg_x rnil)
  :last-2
  (mov arg_y rnil)
  :last-1
  (mov arg_z rnil)
  :done)

;;; ============ deref-macptr ============
;;; Donor: vinsn-retrofit-queue.lisp:199; PPC64 ppc64-vinsns.lisp:2748:
;;; (ld addr macptr.address src).  macptr.address = misc-data-offset = -4
;;; (define-fixedsized-object macptr, arm64-arch.lisp) -- not 8-aligned =>
;;; LDUR (his misc-ref-c-node precedent).
(define-arm64-vinsn deref-macptr (((addr :address))
                                  ((src :lisp))
                                  ())
  (ldur addr (:@ src (:$ arm64::macptr.address))))

;;; ============ double->heap ============
;;; Donor: vinsn-retrofit-queue.lisp:205; PPC64 ppc64-vinsns.lisp:2594:
;;;   li header / la allocptr (- fulltag-misc size) allocptr /
;;;   tdlt allocptr allocbase / std header misc-header-offset(allocptr) /
;;;   mr result allocptr / clrrdi allocptr ntagbits / stfd value(result)
;;; Under Matt's LOW tags this is a direct line-port again: the header
;;; (arm64::double-float-header, define-header in his arm64-arch.lisp) is
;;; a SMALL constant (count<<8|subtag -- the v2 Layout-T movz/movk wide-
;;; header ladder drops out), and the sub bakes fulltag-misc into
;;; allocptr so result needs no separate tagging (the v2 top-byte orr
;;; drops out).  tdlt -> cmp/b.hs/uuo-alloc-trap (his uuo canon:
;;; arm64-uuo.s alloc trap; the mnemonic is in his template table).
;;; Header store at -4, value store at +4: both unaligned => STUR.
;;; clrrdi ntagbits -> AND with the UNSIGNED complement of fulltagmask
;;; (logical immediates are unsigned -- his msg-18/19 rule).
(define-arm64-vinsn double->heap (((result :lisp))
                                  ((fpreg :double-float))
                                  ((header-temp :u64)))
  (mov header-temp (:$ arm64::double-float-header))
  (sub allocptr allocptr (:$ (- arm64::double-float.size arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hs :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (stur header-temp (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov result allocptr)
  (and allocptr allocptr (:$ (logand #xffffffffffffffff
                                     (lognot arm64::fulltagmask))))
  (stur fpreg (:@ result (:$ arm64::double-float.value))))

;;; ============ fixnum-ash ============
;;; Donor: vinsn-retrofit-queue.lisp:226; PPC64 ppc64-vinsns.lisp:2978:
;;;   sradi. count amt fixnumshift / blt :right / sld dest num count /
;;;   b :done / :right neg count count / srad count num count /
;;;   clrrdi dest count fixnumshift / :done
;;; fixnumshift=3 reinstates the unbox (asr #3) the v2 donor dropped at
;;; shift 0, and the right-shift result must have its low tag bits
;;; CLEARED to stay a fixnum (clrrdi) -- AND with the unsigned complement
;;; of fixnummask.  sradi./blt -> asr + cmp #0 + b.lt (one NZCV).
;;; count temp is :s64 (X width) -- register-form shifts are X-form with
;;; the 64-bit num.
(define-arm64-vinsn fixnum-ash (((dest :lisp))
                                ((num :lisp)
                                 (amt :lisp))
                                ((count :s64)))
  (asr count amt (:$ arm64::fixnumshift))
  (cmp count (:$ 0))
  (b.lt :right)
  (lsl dest num count)
  (b :done)
  :right
  (neg count count)
  (asr count num count)
  (and dest count (:$ (logand #xffffffffffffffff
                              (lognot arm64::fixnummask))))
  :done)

;;; ============ fixnum-ash-left ============
;;; Donor: vinsn-retrofit-queue.lisp:243; PPC64 ppc64-vinsns.lisp:2971:
;;;   (sradi count amt fixnumshift) / (sld dest num count)
;;; Direct line-port at fixnumshift=3 (the v2 "amt IS the count" shortcut
;;; was the shift-0 exploitation; the unbox comes back).
(define-arm64-vinsn fixnum-ash-left (((dest :lisp))
                                     ((num :lisp)
                                      (amt :lisp))
                                     ((count :s64)))
  (asr count amt (:$ arm64::fixnumshift))
  (lsl dest num count))

;;; ============ get-double ============
;;; Donor: vinsn-retrofit-queue.lisp:253; PPC64 ppc64-vinsns.lisp:2644:
;;; (lfd target double-float.value source).  double-float.value =
;;; misc-data-offset = -4 => LDUR, D form -- exactly his get-double-float
;;; lapmacro (arm64-lapmacros.lisp:46).
(define-arm64-vinsn get-double (((target :double-float))
                                ((source :lisp))
                                ())
  (ldur target (:@ source (:$ arm64::double-float.value))))

;;; ============ get-single ============
;;; Donor: vinsn-retrofit-queue.lisp:259 (v2 Convention-Y payload at bits
;;; 8..39); PPC64 ppc64-vinsns.lisp:2694 bounces through
;;; tcr.single-float-convert scratch (PPC has no GPR->FPR move).
;;; Matt's immediate single-float: PAYLOAD IN THE TOP 32 BITS, tag
;;; #b0001 low (his get-single-float-bits lapmacro: (lsr dest node 32)).
;;; ARMv8 FMOV (general, W->S) replaces the PPC memory bounce (same
;;; deviation both v2 and his u64-popcount rely on for the reverse
;;; direction).  temp stays :u64 (X) for the 64-bit lsr; its W view feeds
;;; the S-register fmov.
(define-arm64-vinsn get-single (((target :single-float))
                                ((source :lisp))
                                ((temp :u64)))
  (lsr temp source (:$ 32))
  (fmov target (:w temp)))

;;; ============ handle-fixnum-overflow-inline ============
;;; Donor: vinsn-retrofit-queue.lisp:276; algorithm reference
;;; x8664-vinsns.lisp:1453 (little-endian, fixnumshift=3 -- Matt's exact
;;; configuration; PPC64's ppc64-vinsns.lisp:3045 rotldi/xoris trick is
;;; big-endian-only, the donor's declared deviation, kept):
;;;   btcq 63 / sarq fixnumshift / btcq 60  -- un-wrap the overflowed
;;; sign bit, unbox, then flip bit 60 (where the true sign landed),
;;; yielding the 64-bit two's-complement value; box as a 2-digit bignum.
;;; btc -> EOR with a single-bit logical immediate.  The v2 donor OMITTED
;;; the sar at shift 0 and used bit 55; at Matt's shift 3 the x8664
;;; constants 63/3/60 apply verbatim.  Alloc + tag: same low-tag pattern
;;; as double->heap (16 bytes = header + one 64-bit word = 2 digits);
;;; two-digit-bignum-header is small again (v2's movz/movk ladder and
;;; runtime subtag derivation drop out).  Emit site passes dest=src=target
;;; (his arm642.lisp:2010: (! handle-fixnum-overflow-inline target
;;; target)), so bigits is computed into a temp before allocptr moves.
(define-arm64-vinsn handle-fixnum-overflow-inline (((dest :lisp))
                                                   ((src :imm))
                                                   ((bigits :u64)
                                                    (header :u64)))
  (eor bigits src (:$ #x8000000000000000))
  (asr bigits bigits (:$ arm64::fixnumshift))
  (eor bigits bigits (:$ #x1000000000000000))
  (mov header (:$ arm64::two-digit-bignum-header))
  (sub allocptr allocptr (:$ (- 16 arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hs :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (stur header (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov dest allocptr)
  (and allocptr allocptr (:$ (logand #xffffffffffffffff
                                     (lognot arm64::fulltagmask))))
  (stur bigits (:@ dest (:$ arm64::misc-data-offset))))

;;; ============ heap-cons-rest-arg ============
;;; Donor: vinsn-retrofit-queue.lisp:329 (v2 subprim-call macro); PPC64
;;; macro ppc64-vinsns.lisp:3927: (bla .SPheap-cons-rest-arg).
;;; Subprim dispatch: his call-subprim-1/2 vinsn shape (movz offset /
;;; ldr entry off rnil / blr) -- see U1 for the ldr-vs-add open question
;;; and U3 for why the offset is resolved at EXPAND time by name (his
;;; 115b7aa *subprims* table doesn't contain this entry yet; the
;;; PROPOSED extension file does -- U4).  Scratch is imm1, NOT imm0:
;;; the emit site (his arm642.lisp:1881) passes nprev IN imm0 (U2).
(define-arm64-vinsn (heap-cons-rest-arg :call :subprim)
    (()
     ()
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPheap-cons-rest-arg")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ heap-rest-arg ============
;;; Donor: vinsn-retrofit-queue.lisp:332; PPC64: (bla .SPheap-rest-arg).
;;; Same shape as heap-cons-rest-arg (emit site arm642.lisp:1878; the
;;; "simple" case sets no imm0, but imm1 scratch keeps the family
;;; uniform).  .SPheap-rest-arg IS in the proposed subprims extension.
(define-arm64-vinsn (heap-rest-arg :call :subprim)
    (()
     ()
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPheap-rest-arg")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ keyword-bind ============
;;; Donor: vinsn-retrofit-queue.lisp:335; PPC64: (bla .SPkeyword-bind).
;;; Emit site (his arm642.lisp:1841-1855) passes imm0 = nprev (fixnum)
;;; and arg_y = flags -- imm0 is LIVE, scratch must be imm1 (U2).
(define-arm64-vinsn (keyword-bind :call :subprim)
    (()
     ()
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPkeyword-bind")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ lcell-load / lcell-store: NOT PORTED ============
;;; Matt's design has NO lcell machinery (his arm642.lisp tracks the
;;; vstack without lcells; established in arm642-additions.lisp port).
;;; Nothing in his p2 can emit these; the v2/PPC64 donors are lineage
;;; artifacts.  Dropped from this wave (queue entries 338/346).

;;; ============ load-character-constant ============
;;; Donor: vinsn-retrofit-queue.lisp:356 (v2 top-byte tag); PPC64
;;; ppc64-vinsns.lisp:3901:
;;;   ori dest rzero (logior (ash (logand #xff code) 8) subtag-character)
;;;   [oris dest dest (ldb (byte 16 8) code)]  when code > #xff
;;; Matt's character: subtag-character (=fulltag-imm-0, #b0010) in the
;;; low byte, charcode at charcode-shift=8 -- the PPC64 layout exactly, so
;;; the value splits into the same two 16-bit lanes: movz low half,
;;; predicated movk (:lsl 16) for code bits 8..23 (Unicode max #x10FFFF
;;; => value < 2^29, two lanes suffice).  ori/oris -> movz/movk (his
;;; ratified wide-constant idiom); shifted lane syntax is his
;;; (:$ v :lsl n) form.
(define-arm64-vinsn load-character-constant (((dest :lisp))
                                             ((code :u32const))
                                             ())
  (movz dest (:$ (:apply logior
                         (:apply ash (:apply logand #xff code) 8)
                         arm64::subtag-character)))
  ((:not (:pred = 0 (:apply ldb (byte 16 8) code)))
   (movk dest (:$ (:apply ldb (byte 16 8) code) :lsl 16))))

;;; ============ load-double-float-constant ============
;;; Donor: vinsn-retrofit-queue.lisp:371; PPC64 ppc64-vinsns.lisp:3425:
;;;   (stw high -8 sp) (stw low -4 sp) (lfd dest -8 sp)
;;; Faithful stack bounce with two LE-ordered 32-bit stores: AArch64 is
;;; little-endian (LOW half at the lower address -- PPC is BE) and has no
;;; red zone, so reserve 16 bytes explicitly (donor's established
;;; deviation, kept).  high/low are :u32 (W-width) vregs, so plain STR
;;; selects the 32-bit template; dest is :double-float so the reload
;;; selects the D-form LDR (offset 0 is aligned => scaled forms encode).
(define-arm64-vinsn load-double-float-constant
    (((dest :double-float))
     ((high :u32)
      (low :u32)))
  (sub sp sp (:$ 16))
  (str low (:@ sp (:$ 0)))
  (str high (:@ sp (:$ 4)))
  (ldr dest (:@ sp (:$ 0)))
  (add sp sp (:$ 16)))

;;; ============ load-single-float-constant ============
;;; Donor: vinsn-retrofit-queue.lisp:394; PPC64 ppc64-vinsns.lisp:3433
;;; bounces through the stack (no GPR->FPR move on PPC).  ARMv8 FMOV
;;; (general, W->S) is the direct move -- donor's established deviation,
;;; kept.  src holds the raw IEEE-754 single bits; class t carries no
;;; width => the W view is forced explicitly, selecting his
;;; (fmov (:rd :s) (:rn :w)) template.
(define-arm64-vinsn load-single-float-constant
    (((dest :single-float))
     ((src t)))
  (fmov dest (:w src)))

;;; ============ load-vframe-address ============
;;; Donor: vinsn-retrofit-queue.lisp:417; PPC64 ppc64-vinsns.lisp:2458:
;;; (la dest offset vsp) -- offset SIGNED.  Same negative-immediate split
;;; as adjust-sp (AArch64 add-imm is unsigned; the v2 s89 load-t bug is
;;; this exact class).
;;; w10 fix: full s16 window, two-lane (first insn reads vsp/writes
;;; dest, second chains off dest -- the add-immediate contract).
(define-arm64-vinsn load-vframe-address (((dest :imm))
                                         ((offset :s16const)))
  ((:pred >= offset 0)
   ((:not (:pred = 0 (:apply ldb (byte 12 0) offset)))
    (add dest vsp (:$ (:apply ldb (byte 12 0) offset)))
    ((:not (:pred = 0 (:apply ldb (byte 12 12) offset)))
     (add dest dest (:$ (:apply ldb (byte 12 12) offset) :lsl 12))))
   ((:pred = 0 (:apply ldb (byte 12 0) offset))
    (add dest vsp (:$ (:apply ldb (byte 12 12) offset) :lsl 12))))
  ((:pred < offset 0)
   ((:not (:pred = 0 (:apply ldb (byte 12 0) (:apply - offset))))
    (sub dest vsp (:$ (:apply ldb (byte 12 0) (:apply - offset))))
    ((:not (:pred = 0 (:apply ldb (byte 12 12) (:apply - offset))))
     (sub dest dest (:$ (:apply ldb (byte 12 12) (:apply - offset)) :lsl 12))))
   ((:pred = 0 (:apply ldb (byte 12 0) (:apply - offset)))
    (sub dest vsp (:$ (:apply ldb (byte 12 12) (:apply - offset)) :lsl 12)))))

(define-arm64-vinsn make-vcell (((dest :lisp))
                                ((closed (:lisp :ne dest)))
                                ((header :u64)))
  (movz header (:$ arm64::value-cell-header))
  (sub allocptr allocptr (:$ (- arm64::value-cell.size arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hi :no-trap)
  (udf (:$ 4))                          ;uuo_alloc (uuo_misc 1)
  :no-trap
  (stur header (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov dest allocptr)
  (and allocptr allocptr (:$ (:apply ldb (byte 64 0)
                                     (:apply lognot arm64::fulltagmask))))
  (stur closed (:@ dest (:$ arm64::misc-data-offset))))

;;; ============ misc-set-c-node ============
;;; Donor: vinsn-retrofit-queue.lisp:469; PPC64 ppc64-vinsns.lisp:440:
;;; (std val (+ misc-data-offset (ash idx 3)) v).  Offset = 8*idx - 4 == 4
;;; (mod 8) -- never 8-aligned => STUR (unscaled simm9; mirror of his
;;; misc-ref-c-node's ldur).  simm9 bounds idx <= 31 (4+8*31 = 252 <= 255);
;;; larger indices fail loudly at expand time via his range check (same
;;; reach story as the additions file's ref-constant flag).
;;; No GC write barrier here, faithfully to PPC64 (emit sites use it for
;;; initializing stores / non-memoized cells).
(define-arm64-vinsn misc-set-c-node (()
                                     ((val :lisp)
                                      (v :lisp)
                                      (idx :s16const))
                                     ())
  (stur val (:@ v (:$ (:apply + arm64::misc-data-offset
                              (:apply ash idx 3))))))

;;; ============ set-closure-forward-reference ============
;;; Donor: x8664-vinsns.lisp:301 (movq val (misc-function-offset + 8*idx)
;;; closure).  The labels forward-ref fixup stores into a closure AFTER
;;; tag-as-function retagged it misc(4)->function(7); misc-set-c-node's
;;; misc-data-offset addressing is then +3 off, producing an UNALIGNED
;;; 8-byte store at slot+3 that merges the closure pointer with the
;;; neighboring cells ((clos<<24)|nil in slot 2, low-bytes leak into
;;; slot 3 -- the 16m13 l1-clos not-callable).  HISTORY: that was the
;;; split-tag era.  Since the fulltag-function removal (patch 0055)
;;; misc-function-offset = misc-data-offset (-4) and this vinsn is
;;; equivalent to misc-set-c-node's addressing; kept as the dedicated
;;; seam.  No GC write barrier: initializing store, faithful to both
;;; donors.
(define-arm64-vinsn set-closure-forward-reference (()
                                                   ((val :lisp)
                                                    (closure :lisp)
                                                    (idx :s16const))
                                                   ())
  (stur val (:@ closure (:$ (:apply + arm64::misc-function-offset
                                    (:apply ash idx 3))))))

;;; ============ nvalret ============
;;; Donor: vinsn-retrofit-queue.lisp:478; PPC64 macro
;;; ppc64-vinsns.lisp:3931: (ba .SPnvalret) -- a JUMP, not a call.
;;; Same dispatch shape as the call vinsns but ending in BR.
;;; .SPnvalret is in the proposed subprims extension (spentry-C has the
;;; body).  imm1 scratch for family uniformity (U2).
(define-arm64-vinsn (nvalret :jumpLR)
    (()
     ()
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPnvalret")))
  (ldr temp (:@ rcontext temp))
  (br temp))

;;; ============ opt-supplied-p ============
;;; Donor: vinsn-retrofit-queue.lisp:481; PPC64: (bla .SPopt-supplied-p).
;;; Emit site (his arm642.lisp:1884-1891) passes imm0 = num-opt (fixnum)
;;; and pre-scales nargs -- imm0 LIVE => imm1 scratch (U2).
(define-arm64-vinsn (opt-supplied-p :call :subprim)
    (()
     ()
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPopt-supplied-p")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ test-fixnum ============
;;; Demand: LIVE GATE FRONTIER (Unknown vinsn: TEST-FIXNUM); his own
;;; emit site arm642-branch-unless-arg-fixnum (arm642.lisp:1991-1995)
;;; passes (! test-fixnum flags reg) with flags = (make-hard-crf-reg 0),
;;; consumed by (! cbranch-false label flags arm64::cond-eq).
;;; ARM32 original arm-vinsns.lisp:4318:
;;;   (tst src (:$ arm::fixnummask))
;;; PPC64 test-fixnum ppc64-vinsns.lisp:4200 (andi. tag x fixnummask --
;;; needs a temp because PPC's and-immediate can't discard the result;
;;; TST is exactly the discard form).  His live test-fixnums (plural,
;;; arm64-vinsns.lisp:128) is the idiom precedent: orr temp x y, then
;;; (tst temp (:$ arm64::fixnummask)).  Under his low tags a fixnum has
;;; the low nlisptagbits (3) zero; fixnummask = #b111 is an encodable
;;; logical immediate; Z=1 iff fixnum -- what cond-eq consumes.  The
;;; singular form needs no temp.  Non-wired (dest :crf) result follows
;;; his test-fixnums (v2/PPC64's wired (:crf 0) form not carried).
(define-arm64-vinsn test-fixnum (((dest :crf))
                                 ((src :lisp)))
  (tst src (:$ arm64::fixnummask)))

;;; ============ jump-subprim ============
;;; Demand: w1-report item 1; emit site arm642-fixed-call-builtin tail
;;; leg (arm642-additions-w1.lisp:89, (! jump-subprim subprim), subprim
;;; = a small positive table offset from subprim-name->offset /
;;; arm642-builtin-index-subprim).  PPC64 (jump-subprim :jumpLR)
;;; ppc64-vinsns.lisp:2131: (ba spno) -- a JUMP, not a call.  ARM32
;;; arm-vinsns.lisp:2090 (spjump spno).  v2's body (arm64-vinsns.lisp:
;;; 3305) dispatched off rcontext's sptab -- v2-only machinery, reverts
;;; to his rnil-relative design: body = his call-subprim-1/2 shape
;;; ending in BR (the w1 nvalret precedent).  Scratch imm1, not imm0,
;;; for family uniformity with w1's subprim vinsns (w1 U2: imm0 is a
;;; live subprim ABI input at several sites; builtin subprims take args
;;; in arg_y/arg_z, but the uniform-scratch rule is cheaper than
;;; per-vinsn proofs).
(define-arm64-vinsn (jump-subprim :jumpLR)
    (()
     ((spno :u16const))
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ spno))
  (ldr temp (:@ rcontext temp))
  (br temp))

;;; ============ compare ============
;;; Demand: w1-report item 2 ((crf) (x :lisp) (y :lisp) -- cmp x,y);
;;; emit sites arm642-compare-registers / arm642-inline-numcmp
;;; (arm642-additions-w1.lisp:220/:223/:336).  PPC64 compare
;;; ppc64-vinsns.lisp:1843: (cmpd crf arg0 arg1).  ARM32
;;; arm-vinsns.lisp:1621: (cmp arg0 arg1).  The crf result is an SSA
;;; pseudo -- cmp writes NZCV implicitly; the slot links the dep DAG to
;;; the downstream cbranch/cond->boolean (his live test-fixnums/
;;; fixnum-add-set-flags convention).  Signed-vs-unsigned lives in the
;;; consumer's branch condition, not here.
(define-arm64-vinsn compare (((dest :crf))
                             ((x :lisp)
                              (y :lisp)))
  (cmp x y))

;;; ============ compare-signed-s16const ============
;;; Demand: w1-report item 3 -- const is the PRE-SHIFTED boxed value,
;;; guaranteed a single CMP immediate (u12 or u12<<12) by every caller
;;; (arm642-add-sub-imm-single-insn-p gate, which also requires >= 0;
;;; sites arm642-additions-w1.lisp:191/:259/:262/:335 -- the %izerop
;;; family passes literal 0).  PPC64 compare-signed-s16const
;;; ppc64-vinsns.lisp:1916: (cmpdi crf arg0 imm).  ARM32
;;; compare-immediate arm-vinsns.lisp:1637.  v2's flat (cmp arg0 (:$
;;; imm)) (arm64-vinsns.lisp:2905) would TRUNCATE a negative immediate
;;; (the class of v2's s61 compare-to-nil bug); the callers never pass
;;; negatives today, but the sign split keeps the vinsn total: CMN
;;; (alias of adds-discard) sets identical flags for reg vs -(-n).
;;; Full s16 window (w10 fix): aimm is u12 (+u12<<12); the original
;;; single-lane body died on const=16384 (l0-io utf-8-octets-in-string,
;;; boxed 2048 = the UTF-8 3-byte boundary, realgate-w10c).  cmp can't
;;; chain lanes like add-immediate, so exact-multiple-of-4096 consts use
;;; the :lsl 12 lane and everything else materializes via movz/movn.
(define-arm64-vinsn compare-signed-s16const (((dest :crf))
                                             ((reg :lisp)
                                              (const :s16const))
                                             ((temp :s64)))
  ((:pred >= const 0)
   ((:pred < const 4096)
    (cmp reg (:$ const)))
   ((:and (:pred >= const 4096) (:pred = 0 (:apply ldb (byte 12 0) const)))
    (cmp reg (:$ (:apply ash const -12) :lsl 12)))
   ((:and (:pred >= const 4096) (:not (:pred = 0 (:apply ldb (byte 12 0) const))))
    (movz temp (:$ const))
    (cmp reg temp)))
  ((:pred < const 0)
   ((:pred > const -4096)
    (cmn reg (:$ (:apply - const))))
   ((:and (:pred <= const -4096) (:pred = 0 (:apply ldb (byte 12 0) (:apply - const))))
    (cmn reg (:$ (:apply ash (:apply - const) -12) :lsl 12)))
   ((:and (:pred <= const -4096) (:not (:pred = 0 (:apply ldb (byte 12 0) (:apply - const)))))
    (movn temp (:$ (:apply lognot const)))
    (cmp reg temp))))

;;; ============ compare-immediate ============
;;; Demand (16m28, pin advance to 33e61e6): HIS arm642-compare,
;;; arm642-natural-compare and arm642-test-reg-%izerop emit it at 5
;;; sites, plus the %set-bit path (arm642.lisp:2326).  We had written the
;;; same operation under the PPC64 name (compare-signed-s16const, above)
;;; for OUR drafts; his copy/adapt calls the ARM32 name
;;; (compare-immediate, arm-vinsns.lisp:1637 -- (arg t) + a const), and
;;; his functions supersede ours, so the name he emits is the one that
;;; has to exist.  Same body as its twin, deliberately: keep them
;;; identical, and retire ours if these functions ever become upstream's
;;; alone.
;;;
;;; ARG is :imm, PPC64's class for the same operand
;;; (compare-signed-s16const, ppc64-vinsns.lisp:1916), because his callers
;;; pass BOTH a node reg (arm642-compare) and an unboxed u64
;;; (arm642-natural-compare, the %set-bit site).  ARM32's `t' -- "any
;;; class" -- does not exist in this dialect: vinsn-gpr-class->family+width
;;; (arm64-asm.lisp:3007) returns NIL for it, and the whole file then fails
;;; to load with "don't understand (CMP ARG (:$ CONST))".  Every 64-bit GPR
;;; class (:lisp :lisp-lreg :imm :wordptr :u64 :s64 :address) encodes
;;; identically here, so the class is documentation plus width.
;;; The constant window is his:
;;; arm642-constant-for-compare-p returns a value only when
;;; (< (abs val) 4096) -- SIGNED, so cmn covers the negative half -- or
;;; unbound-marker / slot-unbound-marker (0x1a / 0x2a).  The >= 4096
;;; lanes therefore never fire today; they are kept so an out-of-window
;;; caller materializes instead of silently truncating the immediate
;;; (the v2 compare-to-nil truncation class).
(define-arm64-vinsn compare-immediate (((dest :crf))
                                       ((arg :imm)
                                        (const :s16const))
                                       ((temp :s64)))
  ((:pred >= const 0)
   ((:pred < const 4096)
    (cmp arg (:$ const)))
   ((:and (:pred >= const 4096) (:pred = 0 (:apply ldb (byte 12 0) const)))
    (cmp arg (:$ (:apply ash const -12) :lsl 12)))
   ((:and (:pred >= const 4096) (:not (:pred = 0 (:apply ldb (byte 12 0) const))))
    (movz temp (:$ const))
    (cmp arg temp)))
  ((:pred < const 0)
   ((:pred > const -4096)
    (cmn arg (:$ (:apply - const))))
   ((:and (:pred <= const -4096) (:pred = 0 (:apply ldb (byte 12 0) (:apply - const))))
    (cmn arg (:$ (:apply ash (:apply - const) -12) :lsl 12)))
   ((:and (:pred <= const -4096) (:not (:pred = 0 (:apply ldb (byte 12 0) (:apply - const)))))
    (movn temp (:$ (:apply lognot const)))
    (cmp arg temp))))

;;; ============ %ilognot ============
;;; Demand: w1-report item 4 (dest = src XOR ~fixnummask: flip every
;;; payload bit, preserve the 000 tag); emit site :360.  PPC64 %ilognot
;;; ppc64-vinsns.lisp:4212: (subfic dest src (ash -1 fixnumshift)) --
;;; (-8) - src = (lognot n)<<3 for src = n<<3.  ARM32
;;; arm-vinsns.lisp:4333: (orr temp src (:$ fixnummask)) / (mvn dest
;;; temp) -- same value, two insns.  One EOR with the UNSIGNED
;;; complement of fixnummask (#xfffffffffffffff8, encodable) computes
;;; it directly: (n<<3) eor ~7 = (n eor -1)<<3 = (lognot n)<<3.
;;; v2's bare mvn (arm64-vinsns.lisp:6483) was the shift-0 shortcut --
;;; at shift 3 it would leave tag bits 111.
(define-arm64-vinsn %ilognot (((dest :imm))
                              ((src :imm)))
  (eor dest src (:$ (logand #xffffffffffffffff
                            (lognot arm64::fixnummask)))))

;;; ============ clear-left ============
;;; Demand: w1-report item 11; emit sites :412/:536 (contiguous
;;; positive-mask logand path, nbits = (- 64 (1+ (+ fixnumshift
;;; fixlen)))).  PPC64 clear-left ppc64-vinsns.lisp:2865:
;;;   (rldicl dest src 0 (:apply 1+ nbits))
;;; -- clears the top (1+ nbits) bits, KEEPING the low (63 - nbits) bits;
;;; the caller's nbits formula RELIES on that vinsn-side 1+ (v2's
;;; session-84 bug was dropping it: every constant-mask logand kept one
;;; extra bit).  v2's corrected body (arm64-vinsns.lisp:4461) is
;;; (ubfx dest src 0 (- 63 nbits)); ubfx is a LAPMACRO in his tree, not
;;; a template (w1 %unbox-u32 lesson) -- write the underlying UBFM:
;;; immr=0, imms = width-1 = (- 62 nbits).  Handler passes nbits in
;;; [0, 59] (fixlen >= 1); nbits > 62 would produce an invalid imms and
;;; fail loudly at expand time.
(define-arm64-vinsn clear-left (((dest :imm))
                                ((src :imm)
                                 (nbits :s8const)))
  (ubfm dest src (:$ 0) (:$ (:apply - 62 nbits))))

;;; ============ clear-right ============
;;; Demand: w1-report item 12 (clear the LOW nbits; the cleared run
;;; always covers the tag bits -- callers pass (+ fixlen fixnumshift)
;;; >= 4, so the result stays a fixnum); emit sites :413/:537.  PPC64
;;; clear-right ppc64-vinsns.lisp:2870: (rldicr dest src 0 (:apply -
;;; 63 nbits)) -- keep the top (64 - nbits) bits.  One AND with the
;;; expand-time mask (ash -1 nbits) (unsigned-clamped): a contiguous
;;; ones-run from bit nbits to 63, an encodable logical immediate for
;;; any nbits in 1..63 (nbits=0 would be all-ones -- unencodable -- and
;;; is never passed).  v2 (arm64-vinsns.lisp:4473) used an lsr/lsl
;;; pair; the single AND is the tighter mirror of PPC's one rldicr.
(define-arm64-vinsn clear-right (((dest :imm))
                                 ((src :imm)
                                  (nbits :s8const)))
  (and dest src (:$ (:apply logand #xffffffffffffffff
                            (:apply ash -1 nbits)))))

;;; ============ %ilogbitp-constant-bit ============
;;; Demand: w1-report item 13 ((crf) (fixnum :lisp) (bit :u8const)) --
;;; TST bit (+ bit fixnumshift) of the BOXED word; handler pre-clamps
;;; bit to 60 = (- 63 fixnumshift) (w1 U4w).  ARM32 original
;;; arm-vinsns.lisp:3990:
;;;   (tst fixnum (:$ (:apply ash 1 (:apply + bitnum arm::fixnumshift))))
;;; -- carried verbatim (single-bit masks are always encodable logical
;;; immediates).  PPC64 reaches logbitp via its extract-constant-bit
;;; family; the tst shape is the w1 handler's structural decision.
;;; Consumer polarity: Z=0 (cond-ne) = bit set.
(define-arm64-vinsn %ilogbitp-constant-bit (((dest :crf))
                                            ((fixnum :lisp)
                                             (bitnum :u8const)))
  (tst fixnum (:$ (:apply ash 1 (:apply + bitnum arm64::fixnumshift)))))

;;; ============ %ilogbitp-variable-bit ============
;;; Demand: w1-report item 14 ((crf) (fixnum :lisp) (bitnum :lisp));
;;; emit sites :615/:618.  ARM32 original arm-vinsns.lisp:3995:
;;;   (mov unboxed (:asr bitnum (:$ fixnumshift)))
;;;   (mov mask (:$ fixnumone))
;;;   (tst fixnum (:lsl mask unboxed))
;;; ARM32's register-shifted TST operand does not exist on AArch64
;;; (shifted-register forms take constant shifts only) -- materialize
;;; the mask with a register-form LSL instead: unbox the bit number,
;;; shift fixnumone (= 1 << fixnumshift, the boxed bit 0) left by it,
;;; TST register-register.  Bit numbers >= 61 shift the mask past bit
;;; 63 to... the AArch64 register LSL amount is taken MOD 64 -- see U5
;;; for the high-bit semantics caveat (present in the ARM32 original
;;; too).  Temps are unboxed :u64 (GC rule); mask value is transient.
(define-arm64-vinsn %ilogbitp-variable-bit (((dest :crf))
                                            ((fixnum :lisp)
                                             (bitnum :lisp))
                                            ((count :u64)
                                             (mask :u64)))
  (asr count bitnum (:$ arm64::fixnumshift))
  (movz mask (:$ arm64::fixnumone))
  (lsl mask mask count)
  (tst fixnum mask))

;;; ============ fixnum-sub-set-flags ============
;;; Demand: w1-report item 15 -- the twin of his LIVE
;;; fixnum-add-set-flags (arm64-vinsns.lisp: (adds dest x y) with
;;; ((dest :imm) (flags (:crf 0)))), mirrored operand-for-operand with
;;; SUBS.  Emit sites (! fixnum-sub-set-flags dest flags x y) at
;;; arm642-additions-w1.lisp:643/:759.  PPC64 fixnum-sub-set-flags
;;; ppc64-vinsns.lisp:3021: (subfo. dest y x) = x - y setting OV;
;;; ARM32 arm-vinsns.lisp:3187 (subs dest x y).  V flag on signed
;;; overflow feeds arm642-check-fixnum-overflow's cond-vs.
(define-arm64-vinsn fixnum-sub-set-flags (((dest :imm)
                                           (flags (:crf 0)))
                                          ((x :imm)
                                           (y :imm)))
  (subs dest x y))

;;; ============ add-immediate ============
;;; Demand: w1-report item 16 -- SHIFTED constant in (signed-byte 24),
;;; body emits 1-2 ADD/SUB immediates, alias-safe for dest = src (the
;;; w1 U5w contract; emit site :712 gates to signed-byte 24).  PPC64
;;; add-immediate ppc64-vinsns.lisp:3033 is the structural donor -- the
;;; SAME nested-predicate two-lane split (addis high lane, addi low
;;; lane, second insn off dest):
;;;   ((:not (:pred = upper 0)) (addis dest src upper)
;;;    ((:not (:pred = lower 0)) (addi dest dest lower)))
;;;   ((:and (:pred = upper 0) (:not (:pred = lower 0))) (addi dest src lower))
;;; AArch64 lanes are u12 and u12<<12 (his :aimm class).  v2's body
;;; (arm64-vinsns.lisp:4641) only covered +/-4095 (its comment defers
;;; the lsl#12 lane); the (:$ v :lsl 12) syntax IS live in his
;;; assembler (vinsn-parse-immediate; :aimm accepts shift 12), so the
;;; full contract is met here.  Negative constants become SUB with the
;;; same lane split (AArch64 add-imm is unsigned -- the adjust-sp /
;;; s61-truncation class).  Lane order low-then-high; the first emitted
;;; insn reads src, any second insn reads dest -- dest=src safe.
;;; const=0 degenerates to (add dest src #0) -- a copy, correct.
(define-arm64-vinsn add-immediate (((dest :imm))
                                   ((src :imm)
                                    (const :s32const)))
  ((:pred >= const 0)
   ((:not (:pred = 0 (:apply ldb (byte 12 0) const)))
    (add dest src (:$ (:apply ldb (byte 12 0) const)))
    ((:not (:pred = 0 (:apply ldb (byte 12 12) const)))
     (add dest dest (:$ (:apply ldb (byte 12 12) const) :lsl 12))))
   ((:pred = 0 (:apply ldb (byte 12 0) const))
    (add dest src (:$ (:apply ldb (byte 12 12) const) :lsl 12))))
  ((:pred < const 0)
   ((:not (:pred = 0 (:apply ldb (byte 12 0) (:apply - const))))
    (sub dest src (:$ (:apply ldb (byte 12 0) (:apply - const))))
    ((:not (:pred = 0 (:apply ldb (byte 12 12) (:apply - const))))
     (sub dest dest (:$ (:apply ldb (byte 12 12) (:apply - const)) :lsl 12))))
   ((:pred = 0 (:apply ldb (byte 12 0) (:apply - const)))
    (sub dest src (:$ (:apply ldb (byte 12 12) (:apply - const)) :lsl 12)))))

;;; ============ fixnum-add ============
;;; Demand: w1-report item 17; emit site :715.  PPC64 fixnum-add
;;; ppc64-vinsns.lisp:3000: ((dest t)) ((x t) (y t)) (add dest x y);
;;; ARM32 arm-vinsns.lisp:3175 identical; v2 arm64-vinsns.lisp:4599.
;;; Boxed + boxed = boxed at any shift.  Class t carries no width in
;;; his backend (w1 mechanics) -- force the X views (his u64-popcount /
;;; w1 copy-gpr precedent).
(define-arm64-vinsn fixnum-add (((dest t))
                                ((x t)
                                 (y t)))
  (add (:x dest) (:x x) (:x y)))

;;; ============ fixnum-sub ============
;;; Demand: w1-report item 18; emit site :769.  PPC64 fixnum-sub
;;; ppc64-vinsns.lisp:3017: (subf dest y x) -- PPC subf rD,rA,rB is
;;; rB - rA, so this computes x - y; AArch64 SUB is directly
;;; dest = x - y (v2's noted deviation, arm64-vinsns.lisp:4613).
;;; ARM32 arm-vinsns.lisp:3220 (sub dest x y).
(define-arm64-vinsn fixnum-sub (((dest t))
                                ((x t)
                                 (y t)))
  (sub (:x dest) (:x x) (:x y)))

;;; ============ fixnum-sub-from-constant ============
;;; Demand: w1-report item 19 ((dest) (x :s16const UNSHIFTED) (y :lisp)
;;; -- body applies (ash x fixnumshift); w1 U6w gates |x| to 12 bits).
;;; PPC64 ppc64-vinsns.lisp:3026:
;;;   (subfic dest y (:apply ash x fixnumshift))   ; dest = (x<<3) - y
;;; AArch64 has no subtract-FROM immediate: materialize the shifted
;;; constant (one movz/movn lane -- the 15-bit shifted value always
;;; fits; movn imm = lognot(value), his ratified negative-constant
;;; idiom, cf. v2 arm64-vinsns.lisp:4627) into an unboxed TEMP, then
;;; SUB.  v2's body used DEST as the scratch -- an alias bug when
;;; dest = y (the emit site targets arg_z and y is an untargeted
;;; arg_z form); the separate temp removes the hazard (the operand-
;;; alias-order class from corrections memory).
(define-arm64-vinsn fixnum-sub-from-constant (((dest :imm))
                                              ((x :s16const)
                                               (y :imm))
                                              ((temp :u64)))
  ((:pred >= x 0)
   (movz temp (:$ (:apply ash x arm64::fixnumshift))))
  ((:pred < x 0)
   (movn temp (:$ (:apply logand
                          (:apply lognot (:apply ash x arm64::fixnumshift))
                          #xffff))))
  (sub dest temp y))

;;; ============ multiply-immediate ============
;;; Demand: w1-report item 20 ((dest :lisp) (src :lisp) (const :s16const
;;; UNSHIFTED)) -- boxed-src x raw-const preserves boxing; emit site
;;; :805.  PPC64 multiply-immediate ppc64-vinsns.lisp:3070:
;;;   (mulli dest boxed const)
;;; AArch64 has no multiply-immediate (v2's noted deviation,
;;; arm64-vinsns.lisp:4728): materialize the raw constant -- one
;;; movz/movn lane, :s16const always fits -- and MUL.  v2 used DEST as
;;; the scratch ((mul dest boxed dest)), an alias bug when dest = boxed
;;; (the emit site passes an untargeted arg_z form and vreg may be
;;; arg_z; w1 U10w flagged the vreg pass-through) -- separate unboxed
;;; temp here.
(define-arm64-vinsn multiply-immediate (((dest :imm))
                                        ((boxed :imm)
                                         (const :s16const))
                                        ((temp :u64)))
  ((:pred >= const 0)
   (movz temp (:$ const)))
  ((:pred < const 0)
   (movn temp (:$ (:apply logand (:apply lognot const) #xffff))))
  (mul dest boxed temp))

;;; ============ multiply-fixnums ============
;;; Demand: w1-report item 21 (dest = a * unboxed(b): ONE operand
;;; unboxed before the multiply -- the v2 shift-0 bare mul
;;; (arm64-vinsns.lisp:4719) re-derived at shift 3); emit site :808.
;;; PPC64 multiply-fixnums ppc64-vinsns.lisp:3063:
;;;   (sradi unboxed b fixnumshift) / (mulld dest a unboxed)
;;; -- line-port.  ARM32 arm-vinsns.lisp:3276 same shape.  Temp is :s64
;;; (X width; the donors' :s32 would select W templates against X
;;; operands -- the w1 fixnum-ash correction).
(define-arm64-vinsn multiply-fixnums (((dest :imm))
                                      ((a :imm)
                                       (b :imm))
                                      ((unboxed :s64)))
  (asr unboxed b (:$ arm64::fixnumshift))
  (mul dest a unboxed))

;;; ============ %ilsl-c ============
;;; Demand: w1-report item 22 (boxed lsl by constant; handler clamps
;;; count to 63 and routes count > 63 to lri-0, emit site :831).  PPC64
;;; %ilsl-c ppc64-vinsns.lisp:2905: (rldicr dest src count ...) -- shift
;;; left by count, low bits zeroed = plain LSL on AArch64 (the 000 tag
;;; shifts up, zeros shift in; result stays boxed).  v2
;;; arm64-vinsns.lisp:4513 identical.  Constant-shift LSL is a live
;;; template (:lsl-imm-x, value 0-63).
(define-arm64-vinsn %ilsl-c (((dest :imm))
                             ((count :u8const)
                              (src :imm)))
  (lsl dest src (:$ count)))

;;; ============ %ilsl ============
;;; Demand: w1-report item 23 (boxed lsl by unboxed count from boxed
;;; reg); emit site :834 -- the variable leg passes an UNBOUNDED count,
;;; so PPC64's out-of-range guard is LOAD-BEARING on AArch64: register
;;; LSL takes the amount MOD 64 (a shift by 64 would be a no-op, not
;;; 0).  PPC64 %ilsl ppc64-vinsns.lisp:2893:
;;;   (cmpldi crx count (ash 63 fixnumshift))   ; UNSIGNED boxed compare
;;;   (srdi temp count fixnumshift)             ; unbox (logical)
;;;   (sld dest src temp)
;;;   (ble+ crx :foo) / (li dest 0) / :foo
;;; Mirrored 1:1 -- cmp against the boxed 63 (504, a definition-time
;;; immediate expression, fits imm12), lsr-unbox, register-form lsl,
;;; b.ls (unsigned <=) skip, else dest = 0.  A negative boxed count is
;;; huge unsigned => takes the 0 path, as on PPC.  v2's cmp against raw
;;; 63 (arm64-vinsns.lisp:4496) was the shift-0 spelling.
(define-arm64-vinsn %ilsl (((dest :imm))
                           ((count :imm)
                            (src :imm))
                           ((raw :s64)))
  (cmp count (:$ (ash 63 arm64::fixnumshift)))
  (lsr raw count (:$ arm64::fixnumshift))
  (lsl dest src raw)
  (b.ls :done)
  (movz dest (:$ 0))
  :done)

;;; ============ %ilsr-c ============
;;; Demand: w1-report item 24 (boxed lsr by const; MUST clear the low
;;; fixnumshift bits after the shift -- the w1 report's re-derive note;
;;; v2's bare lsr (arm64-vinsns.lisp @%ilsr-c) was the shift-0 body).
;;; Emit site :849 (count 0-31 per the U7w gate).  PPC64 %ilsr-c
;;; ppc64-vinsns.lisp:2913:
;;;   (rldicl temp src (- 64 count) count)      ; logical right shift
;;;   (rldicr dest temp 0 (- 63 fixnumshift))   ; clear tag bits
;;; = lsr + AND with the unsigned complement of fixnummask (the w1
;;; fixnum-ash re-box idiom).
(define-arm64-vinsn %ilsr-c (((dest :imm))
                             ((count :u8const)
                              (src :imm)))
  (lsr dest src (:$ count))
  (and dest dest (:$ (logand #xffffffffffffffff
                             (lognot arm64::fixnummask)))))

;;; ============ %ilsr ============
;;; Demand: w1-report item 25 (same, variable count); emit site :852.
;;; PPC64 %ilsr ppc64-vinsns.lisp:2946:
;;;   (cmpdi crx count (ash 63 fixnumshift))    ; SIGNED boxed compare
;;;   (srdi temp count fixnumshift) / (srd temp src temp)
;;;   (rldicr dest temp 0 (- 63 fixnumshift))   ; clear tag bits
;;;   (ble+ crx :foo) / (li dest 0) / :foo
;;; Mirrored 1:1 (tag-clear before the branch, as PPC orders it); b.le
;;; is the signed <= matching cmpdi/ble.  Guard load-bearing: AArch64
;;; register LSR takes the amount MOD 64.
(define-arm64-vinsn %ilsr (((dest :imm))
                           ((count :imm)
                            (src :imm))
                           ((raw :s64)))
  (cmp count (:$ (ash 63 arm64::fixnumshift)))
  (lsr raw count (:$ arm64::fixnumshift))
  (lsr raw src raw)
  (and dest raw (:$ (logand #xffffffffffffffff
                            (lognot arm64::fixnummask))))
  (b.le :done)
  (movz dest (:$ 0))
  :done)

;;; ============ %iasr-c ============
;;; Demand: w1-report item 26 (boxed asr by const; clear low tag bits);
;;; emit site :867 (count pre-clamped to 63 by the handler).  PPC64
;;; %iasr-c ppc64-vinsns.lisp:2939:
;;;   (sradi temp src count) / (rldicr dest temp 0 (- 63 fixnumshift))
;;; = asr + tag re-clear.  v2's bare asr (arm64-vinsns.lisp:4534) was
;;; the shift-0 body.
(define-arm64-vinsn %iasr-c (((dest :imm))
                             ((count :u8const)
                              (src :imm)))
  (asr dest src (:$ count))
  (and dest dest (:$ (logand #xffffffffffffffff
                             (lognot arm64::fixnummask)))))

;;; ============ %iasr ============
;;; Demand: w1-report item 27 (same, variable count); emit site :870.
;;; PPC64 %iasr ppc64-vinsns.lisp:2926:
;;;   (cmpdi crx count (ash 63 fixnumshift))
;;;   (sradi temp count fixnumshift) / (srad temp src temp)
;;;   (ble+ crx :foo) / (sradi temp src 63) / :foo
;;;   (rldicr dest temp 0 (- 63 fixnumshift))
;;; Out-of-range counts SIGN-FILL (asr by 63), not zero -- mirrored 1:1
;;; including the shared tag-clear tail.  Guard load-bearing (register
;;; ASR takes the amount MOD 64); unbox is ARITHMETIC (sradi) here,
;;; faithful to PPC.
(define-arm64-vinsn %iasr (((dest :imm))
                           ((count :imm)
                            (src :imm))
                           ((raw :s64)))
  (cmp count (:$ (ash 63 arm64::fixnumshift)))
  (asr raw count (:$ arm64::fixnumshift))
  (asr raw src raw)
  (b.le :ok)
  (asr raw src (:$ 63))
  :ok
  (and dest raw (:$ (logand #xffffffffffffffff
                            (lognot arm64::fixnummask)))))

;;; ============ misc-ref (generic, subprim-backed) ============
;;; PPC64 ppc64-vinsns.lisp:4086:
;;;   (define-ppc64-subprim-call-vinsn (misc-ref) .SPmisc-ref)
;;; (macro @3927 expands to ((name :call :subprim) (() ()) (bla spno))).
;;; ABI (PPC64 spentry lineage, carried): arg_y = uvector, arg_z = index
;;; (fixnum); result in arg_z.  ALL typechecking, bounds checking and
;;; subtag dispatch happen in the subprim.  Emit site: arm642-uvref
;;; (arm642-additions-w3.lisp:1218, (! misc-ref) after
;;; two-targeted-reg-forms into arg_y/arg_z).
;;; Dispatch shape = his call-subprim-1/2 (movz offset / ldr entry off
;;; rnil / blr); offset resolved BY NAME at expand time (w1
;;; heap-cons-rest-arg precedent) because .SPmisc-ref is not yet in his
;;; *subprims* table NOR in the proposals file -- U2a: loud expand-time
;;; failure until the level-0 lane lands it (w3 U4m/U10m carry-through).
;;; Scratch is imm1, not imm0 (w1 U2 family-uniformity rule).
(define-arm64-vinsn (misc-ref :call :subprim)
    (()
     ()
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmisc-ref")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ misc-set (generic, subprim-backed) ============
;;; PPC64 ppc64-vinsns.lisp:4088:
;;;   (define-ppc64-subprim-call-vinsn (misc-set) .SPmisc-set)
;;; ABI: arg_x = uvector, arg_y = index, arg_z = value; result arg_z.
;;; The subprim performs the GC WRITE BARRIER (refbits memoization) for
;;; node stores -- the barrier lives kernel-side in every lineage, never
;;; in handler-emitted inline code (w3 U4m).  Emit site: arm642-uvset
;;; (arm642-additions-w3.lisp:1227).  Same by-name offset + imm1 story
;;; as misc-ref (U2a).
(define-arm64-vinsn (misc-set :call :subprim)
    (()
     ()
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmisc-set")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ misc-ref-node ============
;;; PPC64 ppc64-vinsns.lisp:418: (ldx dest v scaled-idx).
;;; scaled-idx = byte offset INCLUDING misc-data-offset, from
;;; scale-node-misc-index (at fixnumshift=3 the fixnum index IS the
;;; 8-byte-element byte offset -- the PPC64 pun; scale = add data-offset
;;; only).  Register-offset X-form ldr, amount 0.
;;; Class deviation (U3a): PPC64 declares scaled-idx :s64 here (and
;;; :u64/:u32 elsewhere, sloppily); his assembler derives the register
;;; WIDTH from the operand class at template-match time, so the class is
;;; load-bearing -- normalized to :u64 (X) across this whole file, per
;;; the w3 report's contract line "(byte-offset-reg :u64)".
(define-arm64-vinsn misc-ref-node (((dest :lisp))
                                   ((v :lisp)
                                    (scaled-idx :u64))
                                   ())
  (ldr dest (:@ v scaled-idx)))

;;; ============ misc-set-node ============
;;; PPC64 ppc64-vinsns.lisp:424: (stdx val v scaled-idx).
;;; NO WRITE BARRIER, faithfully to PPC64: the w3 handler
;;; (arm642-vset1's node leg) emits this ONLY for values proven
;;; immediate (fixnum/nil/t/character); any store that might need
;;; memoization goes through .SPgvset (call-subprim-3) instead.
(define-arm64-vinsn misc-set-node (()
                                   ((val :lisp)
                                    (v :lisp)
                                    (scaled-idx :u64))
                                   ())
  (str val (:@ v scaled-idx)))

;;; ============================================================
;;; Constant-index misc-ref (12) -- raw slot index; vinsn applies
;;; element scale + data-start (see ADDRESSING KEY).
;;; ============================================================

;;; ============ misc-ref-c-u8 ============
;;; PPC64 ppc64-vinsns.lisp:355: (lbz dest (+ misc-data-offset idx) v).
;;; Element shift 0: disp = k - 4 (misc-data-offset = -4 since a9ab24b);
;;; NEGATIVE for k < 4 => unscaled LDURB there, scaled-u12 LDRB for k >= 4
;;; up to his max-8-bit-constant-index -- the split his doc/porting/
;;; arm64.md "Addressing modes" prescribes.  A W-form byte load
;;; zero-extends through the full X register architecturally.
(define-arm64-vinsn misc-ref-c-u8 (((dest :u8))
                                   ((v :lisp)
                                    (idx :u32const))
                                   ())
  ((:pred < idx 4)
   (ldurb dest (:@ v (:$ (:apply + arm64::misc-data-offset idx)))))
  ((:not (:pred < idx 4))
   (ldrb dest (:@ v (:$ (:apply + arm64::misc-data-offset idx))))))

;;; ============ misc-ref-c-s8 ============
;;; PPC64 ppc64-vinsns.lisp:380: (lbz ...) (extsb dest dest) -- byte
;;; load + sign-extend TO 64 BITS.  AArch64 folds both into LDRSB
;;; X-form (v2's noted deviation, kept).  (:x dest) is LOAD-BEARING
;;; (U7a): the :s8 class parses W, which would select the W-form
;;; ldrsb (32-bit extension only).
(define-arm64-vinsn misc-ref-c-s8 (((dest :s8))
                                   ((v :lisp)
                                    (idx :u32const))
                                   ())
  ((:pred < idx 4)                      ;disp < 0 (see -c-u8): unscaled
   (ldursb (:x dest) (:@ v (:$ (:apply + arm64::misc-data-offset idx)))))
  ((:not (:pred < idx 4))
   (ldrsb (:x dest) (:@ v (:$ (:apply + arm64::misc-data-offset idx))))))

;;; ============ misc-ref-c-u16 ============
;;; PPC64 ppc64-vinsns.lisp:309: (lhz dest (+ misc-data-offset
;;; (ash idx 1)) v).  Element shift 1: disp = 2k - 4, a multiple of 2
;;; but NEGATIVE for k < 2 => unscaled LDURH there, scaled LDRH (uoff1)
;;; for k >= 2; max index gated by the handler.
(define-arm64-vinsn misc-ref-c-u16 (((dest :u16))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  ((:pred < idx 2)
   (ldurh dest (:@ v (:$ (:apply + arm64::misc-data-offset
                                 (:apply ash idx 1))))))
  ((:not (:pred < idx 2))
   (ldrh dest (:@ v (:$ (:apply + arm64::misc-data-offset
                                (:apply ash idx 1)))))))

;;; ============ misc-ref-c-s16 ============
;;; PPC64 ppc64-vinsns.lisp:332: (lha ...) -- halfword load, sign-extend
;;; to 64.  LDRSH X-form; (:x dest) load-bearing (U7a).
(define-arm64-vinsn misc-ref-c-s16 (((dest :s16))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  ((:pred < idx 2)                      ;disp < 0 (see -c-u16): unscaled
   (ldursh (:x dest) (:@ v (:$ (:apply + arm64::misc-data-offset
                                       (:apply ash idx 1))))))
  ((:not (:pred < idx 2))
   (ldrsh (:x dest) (:@ v (:$ (:apply + arm64::misc-data-offset
                                      (:apply ash idx 1)))))))

;;; ============ misc-ref-c-u32 ============
;;; PPC64 ppc64-vinsns.lisp:144: (lwz dest (+ misc-data-offset
;;; (ash idx 2)) v).  Element shift 2: disp = 4k - 4, multiple of 4 but
;;; NEGATIVE for k = 0 => unscaled LDUR there, scaled W-form LDR (uoff2)
;;; for k >= 1.  The :u32 class gives the W view natively -- the 32-bit
;;; load that v2 had to respell as `ldrw` (its find-package root cause)
;;; falls out of his width-by-class design.  W write zero-extends.
(define-arm64-vinsn misc-ref-c-u32 (((dest :u32))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  ((:pred < idx 1)
   (ldur dest (:@ v (:$ arm64::misc-data-offset))))
  ((:not (:pred < idx 1))
   (ldr dest (:@ v (:$ (:apply + arm64::misc-data-offset
                               (:apply ash idx 2)))))))

;;; ============ misc-ref-c-s32 ============
;;; PPC64 ppc64-vinsns.lisp:156: (lwa ...) -- word load, sign-extend to
;;; 64 = LDRSW (X-only in the ISA and in his template table); (:x dest)
;;; load-bearing (U7a).
(define-arm64-vinsn misc-ref-c-s32 (((dest :s32))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  ((:pred < idx 1)                      ;disp < 0 (see -c-u32): unscaled
   (ldursw (:x dest) (:@ v (:$ arm64::misc-data-offset))))
  ((:not (:pred < idx 1))
   (ldrsw (:x dest) (:@ v (:$ (:apply + arm64::misc-data-offset
                                      (:apply ash idx 2)))))))

;;; ============ misc-ref-c-u64 ============
;;; PPC64 ppc64-vinsns.lisp:100: (ld dest (+ misc-data-offset
;;; (ash idx word-shift)) v).  Element shift 3: disp = 8k - 4 -- never
;;; 8-aligned (his arch comment @236-244) => LDUR (simm9); k <= 31 = his
;;; max-64-bit-constant-index, handler-gated.  His misc-ref-c-node
;;; precedent exactly.
(define-arm64-vinsn misc-ref-c-u64 (((dest :u64))
                                    ((v :lisp)
                                     (idx :u32const)) ; sic (PPC64 comment)
                                    ())
  (ldur dest (:@ v (:$ (:apply + arm64::misc-data-offset
                               (:apply ash idx arm64::word-shift))))))

;;; ============ misc-ref-c-s64 ============
;;; PPC64 ppc64-vinsns.lisp:106 -- same body as -c-u64 (a 64-bit load
;;; needs no extension; the s64/u64 split is allocator bookkeeping).
(define-arm64-vinsn misc-ref-c-s64 (((dest :s64))
                                    ((v :lisp)
                                     (idx :u32const)) ; sic
                                    ())
  (ldur dest (:@ v (:$ (:apply + arm64::misc-data-offset
                               (:apply ash idx arm64::word-shift))))))

;;; ============ misc-ref-c-single-float ============
;;; PPC64 ppc64-vinsns.lisp:193: (lfs dest (+ misc-data-offset
;;; (ash idx 2)) v).  Same disp arithmetic and k=0 split as -c-u32; the
;;; :single-float class selects the S-form LDR/LDUR (his FP templates
;;; reuse the uoff2/simm9 classes).
(define-arm64-vinsn misc-ref-c-single-float (((dest :single-float))
                                             ((v :lisp)
                                              (idx :u32const))
                                             ())
  ((:pred < idx 1)
   (ldur dest (:@ v (:$ arm64::misc-data-offset))))
  ((:not (:pred < idx 1))
   (ldr dest (:@ v (:$ (:apply + arm64::misc-data-offset
                               (:apply ash idx 2)))))))

;;; ============ misc-ref-c-double-float ============
;;; PPC64 ppc64-vinsns.lisp:206: (lfd dest (+ misc-dfloat-offset
;;; (ash idx 3)) v).  His misc-dfloat-offset = misc-data-offset = -4
;;; (arch @195 -- 64-bit design needs no dfloat pad, unlike PPC32):
;;; disp = 8k - 4, unaligned => D-form LDUR (his get-double-float
;;; lapmacro / w1 get-double precedent); k <= 31 handler-gated.
(define-arm64-vinsn misc-ref-c-double-float (((dest :double-float))
                                             ((v :lisp)
                                              (idx :u32const))
                                             ())
  (ldur dest (:@ v (:$ (:apply + arm64::misc-dfloat-offset
                               (:apply ash idx arm64::word-shift))))))

;;; ============ misc-ref-c-complex-single-float ============
;;; PPC64 ppc64-vinsns.lisp:232: TWO lfs loads (realpart @
;;; complex-single-float.realpart + 8k, imagpart +4) into an FPR PAIR.
;;; Matt keeps a complex-single-float in ONE 64-bit FPR (S lanes 0/1 of
;;; the D view -- class table, w1 copy-complex-single-float precedent),
;;; and the element's two singles are ADJACENT in memory with realpart
;;; at the lower address (his csf object layout @447-448) -- on
;;; little-endian AArch64 a SINGLE D-form load moves both lanes at once
;;; (realpart -> bits 0-31 = lane 0).  Established deviation: PPC's pair
;;; becomes one ldur.  Data start .realpart = -4 = misc-data-offset
;;; (U5a); disp = 8k - 4 unaligned => LDUR; k <= 31 (handler gates csf
;;; with max-64-bit-constant-index).  The :complex-single-float class
;;; is (:fpr 64) => bare dest selects the D-form template.
(define-arm64-vinsn misc-ref-c-complex-single-float
    (((dest :complex-single-float))
     ((v :lisp)
      (idx :u32const))
     ())
  (ldur dest (:@ v (:$ (:apply + arm64::complex-single-float.realpart
                               (:apply ash idx 3))))))

;;; ============ misc-ref-c-complex-double-float ============
;;; PPC64 ppc64-vinsns.lisp:271: two lfd loads (realpart @
;;; complex-double-float.realpart + 16k, imagpart +8) into an FPR pair.
;;; Matt: ONE 128-bit vector register, D lanes 0/1 -- but his template
;;; table has NO Q-form load/store, so the transfer is composed exactly
;;; like his %make-complex-double-float: load lane 0 via the D view
;;; (zeroing the upper lane -- hence low lane FIRST), load the imag word
;;; into a D temp, INS it into lane 1.  (:d dest) is load-bearing: the
;;; :complex-double-float class is (:fpr 128) and matches no load
;;; template bare.  Data start .realpart = +4 = misc-complex-dfloat-
;;; offset (pad word => absolute 16-alignment, arch @199-202, U6a);
;;; disps 16k+4 / 16k+12, k <= 11 handler-gated (conservative) => both in simm9.
(define-arm64-vinsn misc-ref-c-complex-double-float
    (((dest :complex-double-float))
     ((v :lisp)
      (idx :u32const))
     ((dtemp :double-float)))
  (ldur (:d dest) (:@ v (:$ (:apply + arm64::complex-double-float.realpart
                                    (:apply ash idx 4)))))
  (ldur dtemp (:@ v (:$ (:apply + (+ arm64::complex-double-float.realpart 8)
                                (:apply ash idx 4)))))
  (ins (:d dest 1) (:d dtemp 0)))

;;; ============================================================
;;; Variable-index misc-ref (12) -- scaled-idx = byte offset INCLUDING
;;; data-start, in an X register (scale-*-misc-index contract, wave 3b).
;;; PPC64's reg+reg indexed loads (ldx/lwzx/lhzx/lbzx/lfsx/lfdx...) map
;;; to his register-offset form (:@ base Xm), amount 0.
;;; All scaled-idx classes normalized to :u64 (U3a).
;;; ============================================================

;;; ============ misc-ref-u8 ============
;;; PPC64 ppc64-vinsns.lisp:349: (lbzx dest v scaled-idx).
(define-arm64-vinsn misc-ref-u8 (((dest :u8))
                                 ((v :lisp)
                                  (scaled-idx :u64))
                                 ())
  (ldrb dest (:@ v scaled-idx)))

;;; ============ misc-ref-s8 ============
;;; PPC64 ppc64-vinsns.lisp:373: (lbzx ...) (extsb dest dest) ->
;;; LDRSB X-form regoff, (:x dest) load-bearing (U7a).
(define-arm64-vinsn misc-ref-s8 (((dest :s8))
                                 ((v :lisp)
                                  (scaled-idx :u64))
                                 ())
  (ldrsb (:x dest) (:@ v scaled-idx)))

;;; ============ misc-ref-u16 ============
;;; PPC64 ppc64-vinsns.lisp:303: (lhzx dest v scaled-idx).
(define-arm64-vinsn misc-ref-u16 (((dest :u16))
                                  ((v :lisp)
                                   (scaled-idx :u64))
                                  ())
  (ldrh dest (:@ v scaled-idx)))

;;; ============ misc-ref-s16 ============
;;; PPC64 ppc64-vinsns.lisp:326: (lhax dest v scaled-idx) -> LDRSH
;;; X-form (U7a).  PPC64's :s64 index class -> :u64 (U3a).
(define-arm64-vinsn misc-ref-s16 (((dest :s16))
                                  ((v :lisp)
                                   (scaled-idx :u64))
                                  ())
  (ldrsh (:x dest) (:@ v scaled-idx)))

;;; ============ misc-ref-u32 ============
;;; PPC64 ppc64-vinsns.lisp:138: (lwzx dest v scaled-idx).  W-form LDR
;;; regoff (:u32 class => W).  Also the 1-bit handler legs' word-load
;;; workhorse (arm642-additions-w3.lisp:896/:905).
(define-arm64-vinsn misc-ref-u32 (((dest :u32))
                                  ((v :lisp)
                                   (scaled-idx :u64))
                                  ())
  (ldr dest (:@ v scaled-idx)))

;;; ============ misc-ref-s32 ============
;;; PPC64 ppc64-vinsns.lisp:150: (lwax dest v scaled-idx) -> LDRSW
;;; regoff (U7a).
(define-arm64-vinsn misc-ref-s32 (((dest :s32))
                                  ((v :lisp)
                                   (scaled-idx :u64))
                                  ())
  (ldrsw (:x dest) (:@ v scaled-idx)))

;;; ============ misc-ref-u64 ============
;;; PPC64 ppc64-vinsns.lisp:88: (ldx dest v scaled-idx).
(define-arm64-vinsn misc-ref-u64 (((dest :u64))
                                  ((v :lisp)
                                   (scaled-idx :u64))
                                  ())
  (ldr dest (:@ v scaled-idx)))

;;; ============ misc-ref-s64 ============
;;; PPC64 ppc64-vinsns.lisp:94 -- same body as -u64.
(define-arm64-vinsn misc-ref-s64 (((dest :s64))
                                  ((v :lisp)
                                   (scaled-idx :u64))
                                  ())
  (ldr dest (:@ v scaled-idx)))

;;; ============ misc-ref-single-float ============
;;; PPC64 ppc64-vinsns.lisp:187: (lfsx dest v scaled-idx).  S-form LDR
;;; regoff (regoff2 accepts an X index at amount 0).
(define-arm64-vinsn misc-ref-single-float (((dest :single-float))
                                           ((v :lisp)
                                            (scaled-idx :u64))
                                           ())
  (ldr dest (:@ v scaled-idx)))

;;; ============ misc-ref-double-float ============
;;; PPC64 ppc64-vinsns.lisp:199: (lfdx dest v scaled-idx).  D-form LDR
;;; regoff.  PPC64's :u32 index class -> :u64 (U3a -- a W index matches
;;; no regoff template in his assembler).
(define-arm64-vinsn misc-ref-double-float (((dest :double-float))
                                           ((v :lisp)
                                            (scaled-idx :u64))
                                           ())
  (ldr dest (:@ v scaled-idx)))

;;; ============ misc-ref-complex-single-float ============
;;; PPC64 ppc64-vinsns.lisp:223: idx2 = scaled-idx + 4, two lfsx into
;;; an FPR pair.  One D-form regoff LDR moves both S lanes (the
;;; -c-complex-single-float deviation, U5a) -- PPC's idx2 temp and
;;; second load DROP OUT.
(define-arm64-vinsn misc-ref-complex-single-float
    (((dest :complex-single-float))
     ((v :lisp)
      (scaled-idx :u64))
     ())
  (ldr dest (:@ v scaled-idx)))

;;; ============ misc-ref-complex-double-float ============
;;; PPC64 ppc64-vinsns.lisp:262: (addi idx2 scaled-idx 8) then two lfdx
;;; into an FPR pair.  Composed 128-bit load (U6a): PPC's idx2-temp
;;; structure carried for the second lane's address; low lane loaded
;;; FIRST via the D view (zeroing lane 1), then ins.  scaled-idx
;;; already includes misc-complex-dfloat-offset (scale-128bit contract).
(define-arm64-vinsn misc-ref-complex-double-float
    (((dest :complex-double-float))
     ((v :lisp)
      (scaled-idx :u64))
     ((idx2 :u64)
      (dtemp :double-float)))
  (add idx2 scaled-idx (:$ 8))
  (ldr (:d dest) (:@ v scaled-idx))
  (ldr dtemp (:@ v idx2))
  (ins (:d dest 1) (:d dtemp 0)))

;;; ============================================================
;;; Constant-index misc-set (12).  Same displacement arithmetic as the
;;; misc-ref-c- family.  NO write barrier anywhere in this family,
;;; faithfully to PPC64 -- these are IMMEDIATE-element (ivector) stores;
;;; node stores that might need memoization go through .SPgvset /
;;; .SPmisc-set (w3 U4m).  PPC64 declares VAL in the RESULTS slot for
;;; most of these -- an upstream quirk carried verbatim, see U4a.
;;; ============================================================

;;; ============ misc-set-c-u8 ============
;;; PPC64 ppc64-vinsns.lisp:361: (stb val (+ misc-data-offset idx) v).
;;; disp = k - 4, NEGATIVE for k < 4 => STURB there, scaled STRB (uoff0)
;;; for k >= 4 (same split as misc-ref-c-u8).
(define-arm64-vinsn misc-set-c-u8 (((val :u8))
                                   ((v :lisp)
                                    (idx :u32const))
                                   ())
  ((:pred < idx 4)
   (sturb val (:@ v (:$ (:apply + arm64::misc-data-offset idx)))))
  ((:not (:pred < idx 4))
   (strb val (:@ v (:$ (:apply + arm64::misc-data-offset idx))))))

;;; ============ misc-set-c-s8 ============
;;; PPC64 ppc64-vinsns.lisp:387: (stb ...) -- sign is consumed at load
;;; time, the store is the same byte store.
(define-arm64-vinsn misc-set-c-s8 (((val :s8))
                                   ((v :lisp)
                                    (idx :u32const))
                                   ())
  ((:pred < idx 4)                      ;disp < 0 (see -c-u8): unscaled
   (sturb val (:@ v (:$ (:apply + arm64::misc-data-offset idx)))))
  ((:not (:pred < idx 4))
   (strb val (:@ v (:$ (:apply + arm64::misc-data-offset idx))))))

;;; ============ misc-set-c-u16 ============
;;; PPC64 ppc64-vinsns.lisp:315: (sth val (+ misc-data-offset
;;; (ash idx 1)) v).  disp = 2k - 4, NEGATIVE for k < 2 => STURH there,
;;; scaled STRH for k >= 2 (same split as misc-ref-c-u16).
(define-arm64-vinsn misc-set-c-u16 (((val :u16))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  ((:pred < idx 2)
   (sturh val (:@ v (:$ (:apply + arm64::misc-data-offset
                                (:apply ash idx 1))))))
  ((:not (:pred < idx 2))
   (strh val (:@ v (:$ (:apply + arm64::misc-data-offset
                               (:apply ash idx 1)))))))

;;; ============ misc-set-c-s16 ============
;;; PPC64 ppc64-vinsns.lisp:338: (sth ...) -- same store as -c-u16.
(define-arm64-vinsn misc-set-c-s16 (((val :s16))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  ((:pred < idx 2)                      ;disp < 0 (see -c-u16): unscaled
   (sturh val (:@ v (:$ (:apply + arm64::misc-data-offset
                                (:apply ash idx 1))))))
  ((:not (:pred < idx 2))
   (strh val (:@ v (:$ (:apply + arm64::misc-data-offset
                               (:apply ash idx 1)))))))

;;; ============ misc-set-c-u32 ============
;;; PPC64 ppc64-vinsns.lisp:163: (stw val (+ misc-data-offset
;;; (ash idx 2)) v).  disp = 4k - 4, NEGATIVE for k = 0 => STUR there,
;;; scaled W-form STR for k >= 1 (:u32 class gives the W view -- v2's
;;; strw width-bug class impossible here).
;;; NOTE: PPC64 declares this val in the ARGS slot (unlike its u16/u8
;;; twins) -- that asymmetry carried too (U4a).
(define-arm64-vinsn misc-set-c-u32 (()
                                    ((val :u32)
                                     (v :lisp)
                                     (idx :u32const)))
  ((:pred < idx 1)
   (stur val (:@ v (:$ arm64::misc-data-offset))))
  ((:not (:pred < idx 1))
   (str val (:@ v (:$ (:apply + arm64::misc-data-offset
                              (:apply ash idx 2)))))))

;;; ============ misc-set-c-s32 ============
;;; PPC64 ppc64-vinsns.lisp:175: (stw ...) -- same store, args slot.
(define-arm64-vinsn misc-set-c-s32 (()
                                    ((val :s32)
                                     (v :lisp)
                                     (idx :u32const)))
  ((:pred < idx 1)                      ;disp < 0 (see -c-u32): unscaled
   (stur val (:@ v (:$ arm64::misc-data-offset))))
  ((:not (:pred < idx 1))
   (str val (:@ v (:$ (:apply + arm64::misc-data-offset
                              (:apply ash idx 2)))))))

;;; ============ misc-set-c-u64 ============
;;; PPC64 ppc64-vinsns.lisp:119: (std val (+ misc-data-offset
;;; (ash idx 3)) v).  disp = 8k - 4 unaligned => STUR (simm9), k <= 31 --
;;; the drafted misc-set-c-node's exact story (additions.lisp:570).
(define-arm64-vinsn misc-set-c-u64 (()
                                    ((val :u64)
                                     (v :lisp)
                                     (idx :u32const)))
  (stur val (:@ v (:$ (:apply + arm64::misc-data-offset
                              (:apply ash idx 3))))))

;;; ============ misc-set-c-s64 ============
;;; PPC64 ppc64-vinsns.lisp:132 -- same body.
(define-arm64-vinsn misc-set-c-s64 (()
                                    ((val :s64)
                                     (v :lisp)
                                     (idx :u32const)))
  (stur val (:@ v (:$ (:apply + arm64::misc-data-offset
                              (:apply ash idx 3))))))

;;; ============ misc-set-c-single-float ============
;;; PPC64 ppc64-vinsns.lisp:257: (stfs val (+ misc-data-offset
;;; (ash idx 2)) v).  S-form STR/STUR, k=0 split as -c-u32.
(define-arm64-vinsn misc-set-c-single-float (((val :single-float))
                                             ((v :lisp)
                                              (idx :u32const)))
  ((:pred < idx 1)
   (stur val (:@ v (:$ arm64::misc-data-offset))))
  ((:not (:pred < idx 1))
   (str val (:@ v (:$ (:apply + arm64::misc-data-offset
                              (:apply ash idx 2)))))))

;;; ============ misc-set-c-double-float ============
;;; PPC64 ppc64-vinsns.lisp:212: (stfd val (+ misc-dfloat-offset
;;; (ash idx 3)) v).  disp = 8k - 4 unaligned => D-form STUR, k <= 31.
(define-arm64-vinsn misc-set-c-double-float (((val :double-float))
                                             ((v :lisp)
                                              (idx :u32const)))
  (stur val (:@ v (:$ (:apply + arm64::misc-dfloat-offset
                              (:apply ash idx arm64::word-shift))))))

;;; ============ misc-set-c-complex-single-float ============
;;; PPC64 ppc64-vinsns.lisp:240: two stfs (realpart @ .realpart + 8k,
;;; imagpart +4) from an FPR pair.  One D-form STUR stores both S lanes
;;; (single-FPR csf, the misc-ref-c-complex-single-float deviation,
;;; U5a); disp = 8k - 4, k <= 31.
(define-arm64-vinsn misc-set-c-complex-single-float
    (((val :complex-single-float))
     ((v :lisp)
      (idx :u32const)))
  (stur val (:@ v (:$ (:apply + arm64::complex-single-float.realpart
                              (:apply ash idx 3))))))

;;; ============ misc-set-c-complex-double-float ============
;;; PPC64 ppc64-vinsns.lisp:279: two stfd (realpart @ .realpart + 16k,
;;; imagpart +8) from an FPR pair.  Composed 128-bit store (U6a): STUR
;;; the D view (lane 0), DUP lane 1 into a D temp (his
;;; %complex-double-float-imagpart idiom), STUR the temp.  (:d val)
;;; load-bearing ((:fpr 128) matches no store template bare).
;;; disps 16k+4 / 16k+12, k <= 11 handler-gated (conservative).
(define-arm64-vinsn misc-set-c-complex-double-float
    (((val :complex-double-float))
     ((v :lisp)
      (idx :u32const))
     ((dtemp :double-float)))
  (stur (:d val) (:@ v (:$ (:apply + arm64::complex-double-float.realpart
                                   (:apply ash idx 4)))))
  (dup dtemp (:d val 1))
  (stur dtemp (:@ v (:$ (:apply + (+ arm64::complex-double-float.realpart 8)
                                (:apply ash idx 4))))))

;;; ============================================================
;;; Variable-index misc-set (12) -- scaled-idx contract as misc-ref-*.
;;; PPC64 reg+reg stores (stdx/stwx/sthx/stbx/stfsx/stfdx) -> regoff STR
;;; forms.  Same no-barrier contract as the constant family.
;;; ============================================================

;;; ============ misc-set-u8 ============
;;; PPC64 ppc64-vinsns.lisp:367: (stbx val v scaled-idx); val in the
;;; results slot (U4a).
(define-arm64-vinsn misc-set-u8 (((val :u8))
                                 ((v :lisp)
                                  (scaled-idx :u64))
                                 ())
  (strb val (:@ v scaled-idx)))

;;; ============ misc-set-s8 ============
;;; PPC64 ppc64-vinsns.lisp:393: (stbx ...).
(define-arm64-vinsn misc-set-s8 (((val :s8))
                                 ((v :lisp)
                                  (scaled-idx :u64))
                                 ())
  (strb val (:@ v scaled-idx)))

;;; ============ misc-set-u16 ============
;;; PPC64 ppc64-vinsns.lisp:321: (sthx val v scaled-idx); :s64 idx ->
;;; :u64 (U3a).
(define-arm64-vinsn misc-set-u16 (((val :u16))
                                  ((v :lisp)
                                   (scaled-idx :u64)))
  (strh val (:@ v scaled-idx)))

;;; ============ misc-set-s16 ============
;;; PPC64 ppc64-vinsns.lisp:344: (sthx ...).
(define-arm64-vinsn misc-set-s16 (((val :s16))
                                  ((v :lisp)
                                   (scaled-idx :u64)))
  (strh val (:@ v scaled-idx)))

;;; ============ misc-set-u32 ============
;;; PPC64 ppc64-vinsns.lisp:169: (stwx val v scaled-idx); args slot.
;;; W-form STR regoff.  Also the 1-bit handler leg's word store
;;; (arm642-additions-w3.lisp:908).
(define-arm64-vinsn misc-set-u32 (()
                                  ((val :u32)
                                   (v :lisp)
                                   (scaled-idx :u64)))
  (str val (:@ v scaled-idx)))

;;; ============ misc-set-s32 ============
;;; PPC64 ppc64-vinsns.lisp:181: (stwx ...).
(define-arm64-vinsn misc-set-s32 (()
                                  ((val :s32)
                                   (v :lisp)
                                   (scaled-idx :u64)))
  (str val (:@ v scaled-idx)))

;;; ============ misc-set-u64 ============
;;; PPC64 ppc64-vinsns.lisp:113: (stdx val v scaled-idx).
(define-arm64-vinsn misc-set-u64 (()
                                  ((val :u64)
                                   (v :lisp)
                                   (scaled-idx :u64)))
  (str val (:@ v scaled-idx)))

;;; ============ misc-set-s64 ============
;;; PPC64 ppc64-vinsns.lisp:125: (stdx ...).
(define-arm64-vinsn misc-set-s64 (()
                                  ((val :s64)
                                   (v :lisp)
                                   (scaled-idx :u64)))
  (str val (:@ v scaled-idx)))

;;; ============ misc-set-single-float ============
;;; PPC64 ppc64-vinsns.lisp:296: (stfsx val v scaled-idx); args slot;
;;; :u32 idx -> :u64 (U3a).  S-form STR regoff.
(define-arm64-vinsn misc-set-single-float (()
                                           ((val :single-float)
                                            (v :lisp)
                                            (scaled-idx :u64)))
  (str val (:@ v scaled-idx)))

;;; ============ misc-set-double-float ============
;;; PPC64 ppc64-vinsns.lisp:217: (stfdx val v scaled-idx).  D-form STR
;;; regoff.
(define-arm64-vinsn misc-set-double-float (()
                                           ((val :double-float)
                                            (v :lisp)
                                            (scaled-idx :u64)))
  (str val (:@ v scaled-idx)))

;;; ============ misc-set-complex-single-float ============
;;; PPC64 ppc64-vinsns.lisp:248: idx2 = scaled-idx + 4, two stfsx.
;;; NOTE (latent PPC64 bug, recorded not inherited): PPC64's second
;;; store is `(stfsx val v idx2)` -- it stores the REALPART register
;;; twice instead of the pair's second FPR.  Matt's single-FPR csf
;;; makes the question moot: ONE D-form regoff STR stores both lanes.
(define-arm64-vinsn misc-set-complex-single-float
    (()
     ((val :complex-single-float)
      (v :lisp)
      (scaled-idx :u64)))
  (str val (:@ v scaled-idx)))

;;; ============ misc-set-complex-double-float ============
;;; PPC64 ppc64-vinsns.lisp:287: (addi idx2 scaled-idx 8), two stfdx.
;;; Composed store (U6a): str (:d val) lane 0 at scaled-idx, dup lane 1
;;; into dtemp, str dtemp at idx2.
(define-arm64-vinsn misc-set-complex-double-float
    (()
     ((val :complex-double-float)
      (v :lisp)
      (scaled-idx :u64))
     ((idx2 :u64)
      (dtemp :double-float)))
  (add idx2 scaled-idx (:$ 8))
  (str (:d val) (:@ v scaled-idx))
  (dup dtemp (:d val 1))
  (str dtemp (:@ v idx2)))

;;; ============ misc-ref-c-bit-fixnum ============
;;; The one member of the MISC-REF family this file deferred (see the header:
;;; "DEFERRED with the rest of the 1-bit cluster until the bit order is pinned
;;; against HIS kernel").  16m48 pins it, and the wait was justified: this is
;;; the STRUCT-TEST-49/5,/7,/9 failure.  `(defstruct-with-tests
;;; (struct-test-49 (:type (vector bit))) ...)` gives its accessors a CONSTANT
;;; bit index, arm642.lisp:1661 emits this template, and NEED-VINSN-TEMPLATE
;;; signalled "Unknown vinsn: CCL::MISC-REF-C-BIT-FIXNUM".  The CONSTRUCTOR
;;; works, and struct-test-50 (vector character) / -51 (vector (integer 0 255))
;;; are the passing controls.
;;;
;;; BIT ORDER -- LSB0 within a 32-bit LITTLE-ENDIAN word.  Three independent
;;; sources, one of them a measurement of the running image, not a reading:
;;;   (a) the kernel, which is what every passing bit-vector test in the suite
;;;       goes through today: spentry-B misc_ref_bit_vector is
;;;         lsr imm2,imm0,#5 / lsl imm2,imm2,#2 / ldr w3,[..#misc_data_offset]
;;;         / and imm1,imm0,#31 / lsr w3,w3,w1 / and w3,w3,#1
;;;       under a literal "ARM64 LSB0 bit order" comment, and misc_set_bit_vector
;;;       is its exact inverse;
;;;   (b) the VARIABLE-index path of this very handler (arm642.lisp:1663-1670):
;;;       scale-1bit-misc-index (w6) produces the same (idx>>5)<<2 word byte
;;;       offset and an in-word bit number, and extract-variable-bit-fixnum
;;;       (w3b) right-shifts by it -- so a constant index MUST agree with it
;;;       bit for bit or the two arms of one handler disagree;
;;;   (c) MEASURED 16m48 on image 44cb19ff63a57dadb4f529e39aca2b95: the literal
;;;       #*1100000000000001 read back through that variable-index path as
;;;       (1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1).
;;;
;;; Donor: PPC64 ppc64-vinsns.lisp:406, which also loads the containing 32-BIT
;;; WORD (lwz at misc-data-offset + ((idx>>5)<<2)) rather than the byte -- kept
;;; deliberately, because it is the shape the variable-index sibling uses.  PPC
;;; then does one rlwinm to rotate the MSB0-numbered bit into place; on LSB0
;;; AArch64 that is a UBFM extracting bit (idx & 31), then the fixnum shift.
;;;
;;; Addressing: disp = 4*(idx>>5) - 4, so idx < 32 is the sole negative case
;;; and takes the unscaled LDUR; idx >= 32 gives a non-negative multiple of 4
;;; for the scaled W-form LDR (uoff2).  Same split, same reason, as
;;; misc-ref-c-u32 above.  The handler gates idx at his
;;; max-1-bit-constant-index = 32792, whose word disp is 4092 = uoff2 1023.
;;;
;;; (ubfm temp temp b b) is ubfx width 1 at lsb b: imms >= immr, so it is the
;;; extract form, and b = 0 is an ordinary encoding needing no special case --
;;; which is why this does NOT use extract-variable-bit-fixnum's
;;; (lsr temp src bitnum) shape, where a constant 0 shift would have to be
;;; guarded.  (:x temp) on the last line is load-bearing for the same reason
;;; w3b flags it: the :u32 class parses W, and the fixnum result is 64-bit.
;;; The W-form load and W-form UBFM both zero-extend through the full X
;;; register architecturally, so the X view reads a clean word.
;;; dest is :imm, matching both the PPC64 donor and extract-variable-bit-fixnum
;;; -- the sibling called with the identical `target' from the identical
;;; ensuring-node-target at the same emit site.
(define-arm64-vinsn misc-ref-c-bit-fixnum (((dest :imm))
                                           ((v :lisp)
                                            (idx :u32const))
                                           ((temp :u32)))
  ((:pred < idx 32)
   (ldur temp (:@ v (:$ (:apply + arm64::misc-data-offset
                                (:apply ash (:apply ash idx -5) 2))))))
  ((:not (:pred < idx 32))
   (ldr temp (:@ v (:$ (:apply + arm64::misc-data-offset
                               (:apply ash (:apply ash idx -5) 2))))))
  (ubfm temp temp
        (:$ (:apply logand idx 31))
        (:$ (:apply logand idx 31)))
  (lsl dest (:x temp) (:$ arm64::fixnumshift)))

;;; ============================================================
;;; (a) tag-extract family
;;; ============================================================

;;; ============ extract-tag-fixnum ============
;;; LIVE GATE FRONTIER.  Emit site: the w3 lisptag handler
;;; (arm642-additions-w3.lisp:1100, (! extract-tag-fixnum target src)).
;;; PPC64 original ppc64-vinsns.lisp:932:
;;;   (clrlsldi tag object (- 64 nlisptagbits) fixnum-shift)
;;; = (object & #b111) << 3: the 3-bit lisptag, boxed as a fixnum.
;;; One UBFM in insert-zeros form (UBFIZ): immr = (- 64 fixnumshift) = 61,
;;; imms = (1- nlisptagbits) = 2 copies src<2:0> to dest<5:3>, zeroing
;;; everything else.  Result is a boxed fixnum -- :imm class is safe.
(define-arm64-vinsn extract-tag-fixnum (((tag :imm))
                                        ((object :lisp)))
  (ubfm tag object (:$ (- arm64::nbits-in-word arm64::fixnumshift))
        (:$ (1- arm64::nlisptagbits))))

;;; ============ extract-fulltag ============
;;; Emit site: the w3 consp handler (arm642-additions-w3.lisp:1050,
;;; unboxed tag reg compared to arm64::fulltag-cons -- the U1m contract:
;;; dest := (and src fulltagmask), raw).  PPC64 original
;;; ppc64-vinsns.lisp:936: (clrldi tag object (- 64 ntagbits)).
;;; fulltagmask = 15 is an encodable logical immediate.  tag is :u8
;;; (W class); the AND only touches bits 0-3 but the template matcher
;;; needs matching widths against the X-class :lisp src -- force the X
;;; view (w1 %unbox-u32 precedent).
(define-arm64-vinsn extract-fulltag (((tag :u8))
                                     ((object :lisp)))
  (and (:x tag) object (:$ arm64::fulltagmask)))

;;; ============ extract-fulltag-fixnum ============
;;; Emit site: w3 fulltag handler (arm642-additions-w3.lisp:1110).
;;; PPC64 original ppc64-vinsns.lisp:942:
;;;   (clrlsldi tag object (- 64 ntagbits) fixnum-shift)
;;; = (object & #b1111) << 3.  UBFIZ: immr = 61, imms = (1- ntagbits) = 3.
(define-arm64-vinsn extract-fulltag-fixnum (((tag :imm))
                                            ((object :lisp)))
  (ubfm tag object (:$ (- arm64::nbits-in-word arm64::fixnumshift))
        (:$ (1- arm64::ntagbits))))

;;; ============ extract-typecode-fixnum ============
;;; Emit site: w3 typecode handler (arm642-additions-w3.lisp:1122).
;;; PPC64 original ppc64-vinsns.lisp:957: fulltag -> compare fulltag-misc ->
;;; default typecode = LISPTAG (low 3 bits) -> misc objects read the
;;; header's subtag byte -> box.  Same hierarchy on his x8664-style scheme
;;; (x8664-vinsns.lisp:1002 extract-typecode-fixnum is the scheme
;;; reference; it compares the 3-bit lisptag to tag-misc, but his arm64
;;; scheme has no unambiguous lisptag for misc -- fulltag-misc #b0100 and
;;; fulltag-immheader-1 #b1100 share lisptag #b100 -- so the 4-bit fulltag
;;; compare, PPC64's own shape, is used).  PPC64's overwrite-before-branch
;;; trick carried: the lisptag AND lands between the cmp and the b.ne and
;;; doesn't touch NZCV.  The subtag byte is at misc-subtag-offset = -4
;;; (his arch: the header's low byte) -- LDURB, zero-extending.
;;; PPC64's (object (:lisp (:ne code))) constraint carried; the w3
;;; handler's arg_z-dodging quirk keeps it satisfiable.
(define-arm64-vinsn extract-typecode-fixnum (((code :imm))
                                             ((object (:lisp (:ne code))))
                                             ((temp :u64)))
  (and temp object (:$ arm64::fulltagmask))
  (cmp temp (:$ arm64::fulltag-misc))
  (and temp object (:$ arm64::tagmask))
  (b.ne :not-misc)
  (ldurb (:w temp) (:@ object (:$ arm64::misc-subtag-offset)))
  :not-misc
  (lsl code temp (:$ arm64::fixnumshift)))

;;; ============ extract-variable-bit-fixnum ============
;;; Emit site: w3 1-bit vref leg (arm642-additions-w3.lisp:720,
;;; (! extract-variable-bit-fixnum target u32-word bitnum)).
;;; PPC64 original ppc64-vinsns.lisp:1642 (rotlw + rlwinm) works in PPC's
;;; MSB0 bit numbering; the w3 contract (U6m, kernel-verified in v2 s92
;;; cont-63) is LSB0: dest = fixnum((src >> bitnum) & 1).  Body:
;;; register-form LSR (W: src/bitnum/temp are all W classes), then one
;;; UBFIZ placing bit 0 at the fixnum position (immr = 61, imms = 0) and
;;; clearing the rest.  (:x temp) view: the W lsr zero-extended, so the
;;; X-form UBFM reads a clean word.  MSB0-vs-LSB0 is flagged Uw3b-6: the
;;; bit order must match HIS kernel's sbit/%set-sbit when those land.
(define-arm64-vinsn extract-variable-bit-fixnum (((dest :imm))
                                                 ((src :u32)
                                                  (bitnum :u8))
                                                 ((temp :u32)))
  (lsr temp src bitnum)
  (ubfm dest (:x temp) (:$ (- arm64::nbits-in-word arm64::fixnumshift))
        (:$ 0)))

;;; ============ mask-base-char ============
;;; Emit site: w3 arm642-char-p (arm642-additions-w3.lisp:149,
;;; (! mask-base-char arm64::imm0 src) then cmp vs subtag-character).
;;; PPC64 original ppc64-vinsns.lisp:3077:
;;;   (clrldi dest src (- 64 num-subtag-bits))
;;; = low subtag byte, which for a character equals subtag-character
;;; exactly.  U2m RESOLVED for his scheme: character = (code <<
;;; charcode-shift=8) | subtag-character, subtag in the LOW byte
;;; (x8664-vinsns.lisp:3637 mask-base-char = movzbl low byte -- same
;;; extract).  subtag-mask = #xff, an encodable logical immediate.
(define-arm64-vinsn mask-base-char (((dest :u32))
                                    ((src :lisp)))
  (and (:x dest) src (:$ arm64::subtag-mask)))

;;; ============================================================
;;; (b) require family -- trap unless VAL is of type; value untouched.
;;; Emit sites: the w3 def-arm642-require macrolet
;;; (arm642-additions-w3.lisp, (! require-X val-reg)).  PPC64 originals
;;; use uuo_intcerr (CONTINUABLE error) and loop back to retest -- the
;;; (b :again) after each trap carries that contract verbatim for when
;;; his type-error uuo lands (see Uw3b-1 header note; brk placeholders
;;; per the level-0 drafts' convention).
;;; ============================================================

;;; ============ require-fixnum ============
;;; PPC64 ppc64-vinsns.lisp:969 (clrldi. low 3 bits, beq).  Low tags:
;;; fixnum <=> (object & fixnummask) == 0 -- one TST (w2 test-fixnum
;;; idiom; x8664-vinsns.lisp:3350 testb is the scheme reference).
(define-arm64-vinsn require-fixnum (()
                                    ((object :lisp)))
  :again
  (tst object (:$ arm64::fixnummask))
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::tag-fixnum))
  (b :again)
  :got-it)

;;; ============ require-integer ============
;;; PPC64 ppc64-vinsns.lisp:980: fixnum, or fulltag-misc whose header
;;; subtag byte is subtag-bignum (x8664:3359 same shape).  His
;;; subtag-bignum = #b1100|1<<4 = 28; subtag byte read at
;;; misc-subtag-offset = -4 (LDURB, zero-extend to the X temp's W view).
(define-arm64-vinsn require-integer (()
                                     ((object :lisp))
                                     ((tag :u64)))
  :again
  (tst object (:$ arm64::fixnummask))
  (b.eq :got-it)
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-bignum))
  (b.eq :got-it)
  :bad
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-integer))
  (b :again)
  :got-it)

;;; ============ require-simple-vector ============
;;; PPC64 ppc64-vinsns.lisp:998; x8664:3376.  fulltag-misc + subtag
;;; compare (his subtag-simple-vector = #b1110|11<<4 = 190, fits cmp
;;; imm12).
(define-arm64-vinsn require-simple-vector (()
                                           ((object :lisp))
                                           ((tag :u64)))
  :again
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-simple-vector))
  (b.eq :got-it)
  :bad
  (uuo-cerror-reg-not-xtype object (:$ arm64::subtag-simple-vector))
  (b :again)
  :got-it)

;;; ============ require-simple-string ============
;;; PPC64 ppc64-vinsns.lisp:1014; x8664:3391.  Same shape, subtag =
;;; his subtag-simple-base-string (#b1100|12<<4 = 204).
(define-arm64-vinsn require-simple-string (()
                                           ((object :lisp))
                                           ((tag :u64)))
  :again
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-simple-base-string))
  (b.eq :got-it)
  :bad
  (uuo-cerror-reg-not-xtype object (:$ arm64::subtag-simple-base-string))
  (b :again)
  :got-it)

;;; ============ require-real ============
;;; PPC64 ppc64-vinsns.lisp:1031 / x8664:3406 (the scheme reference --
;;; identical typecode-mask trick under identical tag numbering rules):
;;; typecode  in  {tag-fixnum=0, tag-single-float=1, subtag-bignum=28,
;;; subtag-ratio=30, subtag-double-float=44}, all < 64, tested as bits
;;; of a 64-bit mask shifted right by the typecode.  Typecode = lisptag,
;;; or the header subtag byte for fulltag-misc (extract-typecode-fixnum's
;;; hierarchy, including the overwrite-before-branch trick).  The mask
;;; constant has zero bits above 47 (max member 44), so three movz/movk
;;; lanes materialize it exactly (his ratified wide-constant idiom; the
;;; lane values are definition-time constants).  Typecodes >= 64 fail the
;;; unsigned bound check first (x8664's rcmpl/jae).  Bit-0 test: TST #1
;;; + b.ne = PPC's clrldi./bne+ (bit set => member).
(define-arm64-vinsn require-real (()
                                  ((object :lisp))
                                  ((tag :u64)
                                   (mask :u64)))
  :again
  (movz mask (:$ (ldb (byte 16 0)
                      (logior (ash 1 arm64::tag-fixnum)
                              (ash 1 arm64::tag-single-float)
                              (ash 1 arm64::subtag-bignum)
                              (ash 1 arm64::subtag-ratio)
                              (ash 1 arm64::subtag-double-float)))))
  (movk mask (:$ (ldb (byte 16 16)
                      (logior (ash 1 arm64::tag-fixnum)
                              (ash 1 arm64::tag-single-float)
                              (ash 1 arm64::subtag-bignum)
                              (ash 1 arm64::subtag-ratio)
                              (ash 1 arm64::subtag-double-float)))
              :lsl 16))
  (movk mask (:$ (ldb (byte 16 32)
                      (logior (ash 1 arm64::tag-fixnum)
                              (ash 1 arm64::tag-single-float)
                              (ash 1 arm64::subtag-bignum)
                              (ash 1 arm64::subtag-ratio)
                              (ash 1 arm64::subtag-double-float)))
              :lsl 32))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (and tag object (:$ arm64::tagmask))
  (b.ne :have-typecode)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  :have-typecode
  (cmp tag (:$ arm64::nbits-in-word))
  (b.hs :bad)
  (lsr mask mask tag)
  (tst mask (:$ 1))
  (b.ne :got-it)
  :bad
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-real))
  (b :again)
  :got-it)

;;; ============ require-number ============
;;; PPC64 ppc64-vinsns.lisp:1060 / x8664:3432.  require-real's mask plus
;;; subtag-complex (46, still < 64) plus two direct compares for the
;;; typecodes that DON'T fit the mask: subtag-complex-single-float (76)
;;; and subtag-complex-double-float (92) -- exactly x8664's pre-compares.
(define-arm64-vinsn require-number (()
                                    ((object :lisp))
                                    ((tag :u64)
                                     (mask :u64)))
  :again
  (movz mask (:$ (ldb (byte 16 0)
                      (logior (ash 1 arm64::tag-fixnum)
                              (ash 1 arm64::tag-single-float)
                              (ash 1 arm64::subtag-bignum)
                              (ash 1 arm64::subtag-ratio)
                              (ash 1 arm64::subtag-double-float)
                              (ash 1 arm64::subtag-complex)))))
  (movk mask (:$ (ldb (byte 16 16)
                      (logior (ash 1 arm64::tag-fixnum)
                              (ash 1 arm64::tag-single-float)
                              (ash 1 arm64::subtag-bignum)
                              (ash 1 arm64::subtag-ratio)
                              (ash 1 arm64::subtag-double-float)
                              (ash 1 arm64::subtag-complex)))
              :lsl 16))
  (movk mask (:$ (ldb (byte 16 32)
                      (logior (ash 1 arm64::tag-fixnum)
                              (ash 1 arm64::tag-single-float)
                              (ash 1 arm64::subtag-bignum)
                              (ash 1 arm64::subtag-ratio)
                              (ash 1 arm64::subtag-double-float)
                              (ash 1 arm64::subtag-complex)))
              :lsl 32))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (and tag object (:$ arm64::tagmask))
  (b.ne :have-typecode)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  :have-typecode
  (cmp tag (:$ arm64::subtag-complex-single-float))
  (b.eq :got-it)
  (cmp tag (:$ arm64::subtag-complex-double-float))
  (b.eq :got-it)
  (cmp tag (:$ arm64::nbits-in-word))
  (b.hs :bad)
  (lsr mask mask tag)
  (tst mask (:$ 1))
  (b.ne :got-it)
  :bad
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-number))
  (b :again)
  :got-it)

;;; ============ require-list ============
;;; PPC64 ppc64-vinsns.lisp:1095 compares NIL and fulltag-cons
;;; separately; on his scheme ONE lisptag compare covers both -- tag-list
;;; #b011 is fulltag-cons (#b0011) and fulltag-nil (#b1011) and nothing
;;; else (x8664:3463, the scheme reference; his arch comment "cons cell
;;; or nil").
(define-arm64-vinsn require-list (()
                                  ((object :lisp))
                                  ((tag :u64)))
  :again
  (and tag object (:$ arm64::tagmask))
  (cmp tag (:$ arm64::tag-list))
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::tag-list))
  (b :again)
  :got-it)

;;; ============ require-symbol ============
;;; PPC64 ppc64-vinsns.lisp:1110 (fulltag-misc + subtag-symbol) does NOT
;;; transfer: on his scheme symbols are POINTER-tagged fulltag-symbol
;;; (#b0111, "non-null symbol") and NIL is fulltag-nil (#b1011, "nil and
;;; nothing but") -- x8664:3476 is the structural reference (nil check +
;;; symbol tag check), with one deviation flagged Uw3b-2: x8664 has a
;;; 3-bit tag-symbol; his scheme's lisptag #b111 covers fulltag-symbol
;;; (#b0111) and fulltag-15 (#b1111) alike, so a 3-bit compare is not
;;; exact and BOTH compares here use the 4-bit fulltag.  (#b1111 was
;;; fulltag-function until patch 0055, which made the distinction
;;; load-bearing; 15 is unallocated now, so this is exactness rather
;;; than necessity.)
(define-arm64-vinsn require-symbol (()
                                    ((object :lisp))
                                    ((tag :u64)))
  :again
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-nil))
  (b.eq :got-it)
  (cmp tag (:$ arm64::fulltag-symbol))
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::subtag-symbol))
  (b :again)
  :got-it)

;;; ============ require-character ============
;;; PPC64 ppc64-vinsns.lisp:1126 (low subtag byte == subtag-character);
;;; identical on his scheme (character = code<<8 | subtag-character,
;;; subtag-character = fulltag-imm-0 = 2; x8664:3492 cmpb equivalent).
(define-arm64-vinsn require-character (()
                                       ((object :lisp))
                                       ((tag :u64)))
  :again
  (and tag object (:$ arm64::subtag-mask))
  (cmp tag (:$ arm64::subtag-character))
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::subtag-character))
  (b :again)
  :got-it)

;;; ============ require-s8 ============
;;; PPC64 ppc64-vinsns.lisp:1139: shift the boxed value so only the low
;;; 8 payload bits survive, arithmetic-shift back, re-box, compare with
;;; the original -- equal iff object is a fixnum in (signed-byte 8).
;;; (Non-fixnums can't match: the reconstruction always has tag 000.)
;;; Line-port at shift 3 (x8664:3501 identical constants).
(define-arm64-vinsn require-s8 (()
                                ((object :lisp))
                                ((tag :s64)))
  :again
  (lsl tag object (:$ (- arm64::nbits-in-word (+ 8 arm64::fixnumshift))))
  (asr tag tag (:$ (- arm64::nbits-in-word 8)))
  (lsl tag tag (:$ arm64::fixnumshift))
  (cmp tag object)
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-s8))
  (b :again)
  :got-it)

;;; ============ require-u8 ============
;;; PPC64 ppc64-vinsns.lisp:1154 (rldicr. -- the low fixnumshift bits AND
;;; everything above bit 8+fixnumshift must be zero).  One TST with the
;;; complement of the boxed-u8 field: ~(#xff<<3) is the complement of a
;;; contiguous run = an encodable logical immediate (unsigned-clamped,
;;; his msg-18/19 rule; x8664:3516 same single-test shape).
(define-arm64-vinsn require-u8 (()
                                ((object :lisp)))
  :again
  (tst object (:$ (logand #xffffffffffffffff
                          (lognot (ash #xff arm64::fixnumshift)))))
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-u8))
  (b :again)
  :got-it)

;;; ============ require-s16 ============
;;; PPC64 ppc64-vinsns.lisp:1167; same reconstruction with width 16.
(define-arm64-vinsn require-s16 (()
                                 ((object :lisp))
                                 ((tag :s64)))
  :again
  (lsl tag object (:$ (- arm64::nbits-in-word (+ 16 arm64::fixnumshift))))
  (asr tag tag (:$ (- arm64::nbits-in-word 16)))
  (lsl tag tag (:$ arm64::fixnumshift))
  (cmp tag object)
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-s16))
  (b :again)
  :got-it)

;;; ============ require-u16 ============
;;; PPC64 ppc64-vinsns.lisp:1182; TST with ~(#xffff<<3) (encodable).
(define-arm64-vinsn require-u16 (()
                                 ((object :lisp)))
  :again
  (tst object (:$ (logand #xffffffffffffffff
                          (lognot (ash #xffff arm64::fixnumshift)))))
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-u16))
  (b :again)
  :got-it)

;;; ============ require-s32 ============
;;; PPC64 ppc64-vinsns.lisp:1195; same reconstruction with width 32.
;;; (x8664:3552 adds a redundant belt-and-braces fixnummask test; PPC64
;;; doesn't -- the reconstruction already catches every non-fixnum tag --
;;; and the PPC64 line is carried.)
(define-arm64-vinsn require-s32 (()
                                 ((object :lisp))
                                 ((tag :s64)))
  :again
  (lsl tag object (:$ (- arm64::nbits-in-word (+ 32 arm64::fixnumshift))))
  (asr tag tag (:$ (- arm64::nbits-in-word 32)))
  (lsl tag tag (:$ arm64::fixnumshift))
  (cmp tag object)
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-s32))
  (b :again)
  :got-it)

;;; ============ require-u32 ============
;;; PPC64 ppc64-vinsns.lisp:1210; TST with ~(#xffffffff<<3) (encodable).
(define-arm64-vinsn require-u32 (()
                                 ((object :lisp)))
  :again
  (tst object (:$ (logand #xffffffffffffffff
                          (lognot (ash #xffffffff arm64::fixnumshift)))))
  (b.eq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-u32))
  (b :again)
  :got-it)

;;; ============ require-s64 ============
;;; PPC64 ppc64-vinsns.lisp:1223: fixnum, or fulltag-misc whose whole
;;; header word equals two-digit-bignum-header ((2<<8)|subtag-bignum =
;;; 540, fits cmp imm12 -- the low-tag small-header win, cf. w1
;;; double->heap).  Header word at misc-header-offset = -4 => LDUR.
(define-arm64-vinsn require-s64 (()
                                 ((object :lisp))
                                 ((tag :u64)))
  :again
  (tst object (:$ arm64::fixnummask))
  (b.eq :got-it)
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldur tag (:@ object (:$ arm64::misc-header-offset)))
  (cmp tag (:$ arm64::two-digit-bignum-header))
  (b.eq :got-it)
  :bad
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-s64))
  (b :again)
  :got-it)

;;; ============ require-u64 ============
;;; PPC64 ppc64-vinsns.lisp:1242, little-endian shape from x8664:3596
;;; (PPC64's rotldi digit-swizzle is big-endian-only): non-negative
;;; fixnum, or a two-digit bignum (32-bit digits at +4/+8 read as ONE
;;; little-endian 64-bit word at misc-data-offset) whose value is
;;; non-negative, or a three-digit bignum whose top digit (the u32 at
;;; misc-data-offset+8 = +4) is zero.  mov doesn't touch NZCV, so the
;;; TST's flags survive into b.eq.  W-view compare for the top digit.
(define-arm64-vinsn require-u64 (()
                                 ((object :lisp))
                                 ((tag :s64)))
  :again
  (tst object (:$ arm64::fixnummask))
  (mov tag object)
  (b.eq :ok-if-non-negative)
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldur tag (:@ object (:$ arm64::misc-header-offset)))
  (cmp tag (:$ arm64::two-digit-bignum-header))
  (b.eq :two-digit)
  (cmp tag (:$ arm64::three-digit-bignum-header))
  (b.ne :bad)
  (ldur (:w tag) (:@ object (:$ (+ arm64::misc-data-offset 8))))
  (cmp (:w tag) (:$ 0))
  (b.eq :got-it)
  (b :bad)
  :two-digit
  (ldur tag (:@ object (:$ arm64::misc-data-offset)))
  :ok-if-non-negative
  (cmp tag (:$ 0))
  (b.ge :got-it)
  :bad
  (uuo-cerror-reg-not-xtype object (:$ arm64::xtype-u64))
  (b :again)
  :got-it)

;;; ============================================================
;;; (c) trap-unless family -- one-shot (uuo_interr, not continuable):
;;; PPC64's td* trap instructions have no retest loop, and none is
;;; carried.  Same brk placeholder convention (Uw3b-1).
;;; ============================================================

;;; ============ trap-unless-fixnum ============
;;; Emit sites: w3 vref/vset guard blocks (arm642-additions-w3.lisp:741,
;;; :944) and the fixnum->signed-natural checked legs (:411, :432).
;;; PPC64 ppc64-vinsns.lisp:1504 (clrldi + tdnei tag-fixnum); low tags:
;;; one TST of the low 3 bits (x8664:782 testb; w2 test-fixnum idiom).
(define-arm64-vinsn trap-unless-fixnum (()
                                        ((object :lisp)))
  (tst object (:$ arm64::fixnummask))
  (b.eq :ok)
  (uuo-error-reg-not-xtype object (:$ arm64::tag-fixnum))
  :ok)

;;; ============ trap-unless-list ============
;;; Emit sites: w3 endp / reference-list safe legs
;;; (arm642-additions-w3.lisp:164, :1067).  PPC64 ppc64-vinsns.lisp:1523
;;; compares NIL and fulltag-cons separately; his scheme folds both into
;;; the lisptag: tag-list #b011 = fulltag-cons | fulltag-nil and nothing
;;; else (x8664:714 trap-unless-list, the scheme reference).
;;; 16m40: the trap was `brk #xf000|arch::error-object-not-list', the w3b
;;; placeholder (Uw3b-1).  It is now the canon wrong_type uuo (patch 0047
;;; adds the register-bearing templates).  brk is NOT a uuo on this
;;; architecture -- arm64-uuo.s:11 says so outright -- so IS_UUO
;;; ((insn & 0xffff0000) == 0) rejected it, PMCL_exception_handler fell
;;; through to handle_unimplemented_instruction, and the image DIED
;;; ("Unhandled exception 5") instead of signalling.  It also carried no
;;; register field, so the offending object was unrecoverable.
;;; The code operand is the lisptag tag-list, which
;;; level-1/arm64-trap-support.lisp's *arm64-xtype-specifiers* maps to
;;; LIST -- the table already carries that entry.
(define-arm64-vinsn trap-unless-list (()
                                      ((object :lisp))
                                      ((tag :u64)))
  (and tag object (:$ arm64::tagmask))
  (cmp tag (:$ arm64::tag-list))
  (b.eq :ok)
  (uuo-error-reg-not-xtype object (:$ arm64::tag-list))
  :ok)

;;; ============ trap-unless-cons ============
;;; Emit site: w3 arm642-modify-cons safe leg
;;; (arm642-additions-w3.lisp:186).  PPC64 ppc64-vinsns.lisp:1517
;;; (fulltag == fulltag-cons -- NIL is NOT a cons; x8664:729 identical).
(define-arm64-vinsn trap-unless-cons (()
                                      ((object :lisp))
                                      ((tag :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-cons))
  (b.eq :ok)
  (uuo-error-reg-not-xtype object (:$ arm64::xtype-cons))
  :ok)

;;; ============ trap-unless-typecode= ============
;;; Emit sites: w3 vref/vset guard blocks (arm642-additions-w3.lisp:739,
;;; :942 -- tagval is a known uvector subtag) and the typechecked-form
;;; builtin leg.  PPC64 ppc64-vinsns.lisp:1601: extract-typecode
;;; hierarchy (fulltag; misc -> header subtag byte; else lisptag -- the
;;; same overwrite-before-branch trick as extract-typecode-fixnum), then
;;; trap unless == tagval.  Subtags are <= 255, so (:$ tagval) always
;;; fits cmp imm12.  Trap is the drafts' PARAMETERIZED-type placeholder
;;; #xf0ff (the type is a template hole, not an arch error code -- same
;;; code the drafts' trap-unless-typecode= lapmacro uses).
(define-arm64-vinsn trap-unless-typecode= (()
                                           ((object :lisp)
                                            (tagval :u16const))
                                           ((tag :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (and tag object (:$ arm64::tagmask))
  (b.ne :have-typecode)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  :have-typecode
  (cmp tag (:$ tagval))
  (b.eq :ok)
  (uuo-error-reg-not-xtype object (:$ tagval))
  :ok)

;;; ============================================================
;;; (d) box/convert family
;;; ============================================================

;;; ============ box-fixnum ============
;;; Emit sites: w3 box legs (arm642-additions-w3.lisp:571/:614/:618/:637).
;;; PPC64 ppc64-vinsns.lisp:1292: (sldi dest src fixnumshift).
(define-arm64-vinsn box-fixnum (((dest :imm))
                                ((src :s64)))
  (lsl dest src (:$ arm64::fixnumshift)))

;;; ============ fixnum->signed-natural ============
;;; Emit sites: w3 unbox legs (arm642-additions-w3.lisp:412/:433).
;;; PPC64 ppc64-vinsns.lisp:1296: (sradi dest src fixnumshift).
(define-arm64-vinsn fixnum->signed-natural (((dest :s64))
                                            ((src :imm)))
  (asr dest src (:$ arm64::fixnumshift)))

;;; ============ unbox-base-char ============
;;; Emit site: w3 aset char leg (arm642-additions-w3.lisp:440,
;;; (! unbox-base-char unboxed-reg src)).  PPC64 ppc64-vinsns.lisp:1455:
;;; low subtag byte == subtag-character else uuo_interr (one-shot), code
;;; = src >> charcode-shift.  PPC64's order carried exactly: dest gets
;;; the code BEFORE the branch (lsr doesn't touch the cmp's NZCV).
;;; Character subtag is the LOW byte on his scheme (mask-base-char
;;; note); dest :u32 forced to the X view against the :lisp src.
(define-arm64-vinsn unbox-base-char (((dest :u32))
                                     ((src :lisp)))
  (and (:x dest) src (:$ arm64::subtag-mask))
  (cmp (:x dest) (:$ arm64::subtag-character))
  (lsr (:x dest) src (:$ arm64::charcode-shift))
  (b.eq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm64::subtag-character))
  :got-it)

;;; ============ unbox-bit ============
;;; Emit site: w3 aset bit leg (arm642-additions-w3.lisp:483).  PPC64
;;; ppc64-vinsns.lisp:1465: unsigned-compare boxed value against boxed 1
;;; (fixnumone), shift to unbox, trap unless <=.  LSB0 semantics per the
;;; w3 U6m carry (Uw3b-6): the bit value lands at bit 0 of dest, agreeing
;;; with extract-variable-bit-fixnum and the 1-bit store family's input
;;; position.  PPC64 quirk carried verbatim: a raw 1..7 (non-fixnum low
;;; bits, unsigned < 8) would pass the compare and unbox to 0 -- callers
;;; guarantee a fixnum, as on PPC64.
(define-arm64-vinsn unbox-bit (((dest :u32))
                               ((src :lisp)))
  (cmp src (:$ arm64::fixnumone))
  (lsr (:x dest) src (:$ arm64::fixnumshift))
  (b.ls :got-it)
  (uuo-error-reg-not-xtype src (:$ arm64::xtype-bit))
  :got-it)

;;; ============ u32->char ============
;;; Emit sites: w3 char vref legs (arm642-additions-w3.lisp:574/:621).
;;; PPC64 ppc64-vinsns.lisp:2740: (sldi dest src charcode-shift) /
;;; (ori dest dest subtag-character) -- line-port; subtag-character = 2
;;; is an encodable logical immediate.  GC-safe though dest is :lisp:
;;; after the lsl dest holds code<<8, whose low 3 bits are 0 -- a valid
;;; fixnum -- and the orr completes the character; no trap or branch
;;; intervenes.
(define-arm64-vinsn u32->char (((dest :lisp))
                               ((src :u32)))
  (lsl (:x dest) (:x src) (:$ arm64::charcode-shift))
  (orr dest dest (:$ arm64::subtag-character)))

;;; ============ single-float-bits ============
;;; Emit site: w3 single-float aset leg (arm642-additions-w3.lisp:447).
;;; PPC64 ppc64-vinsns.lisp:2123: (srdi dest src 32).  Matt's immediate
;;; single-float carries the IEEE payload in the TOP 32 bits (his
;;; get-single-float-bits lapmacro `(lsr dest node 32)`; w1 get-single
;;; precedent) -- the PPC64 body transfers verbatim.
(define-arm64-vinsn single-float-bits (((dest :u32))
                                       ((src :lisp)))
  (lsr (:x dest) src (:$ 32)))

;;; ============================================================
;;; (e) fp unbox-to-fpr family
;;; ============================================================

;;; ============ get-double? ============
;;; Emit site: w3 double-float aset leg (arm642-additions-w3.lisp:397,
;;; (! get-double? fpreg src)).  PPC64 ppc64-vinsns.lisp:2670: typecode
;;; check (fulltag-misc + subtag-double-float), trap otherwise, then the
;;; value load -- get-double (w1) plus the check.  PPC64's shared-trap
;;; shape carried exactly: the not-misc path branches straight to the
;;; trap decision with the fulltag compare's NE flags, so b.eq falls
;;; into the brk; the misc path arrives with the subtag compare's flags.
;;; double-float.value = misc-data-offset = -4 => D-form LDUR (w1
;;; get-double precedent).
(define-arm64-vinsn get-double? (((target :double-float))
                                 ((source :lisp))
                                 ((tag :u64)))
  (and tag source (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :trap)
  (ldurb (:w tag) (:@ source (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-double-float))
  :trap
  (b.eq :ok)
  (uuo-error-reg-not-xtype source (:$ arm64::subtag-double-float))
  :ok
  (ldur target (:@ source (:$ arm64::double-float.value))))

;;; ============ get-complex-single ============
;;; Emit site: w3 complex-single aset leg (arm642-additions-w3.lisp:404).
;;; PPC64 ppc64-vinsns.lisp:2658 does two lfs loads into an FPR PAIR;
;;; Matt keeps a complex-single-float in ONE 64-bit FPR (realpart lane 0,
;;; imagpart lane 1 of the D view -- his %complex-single-float-realpart/
;;; imagpart, w1 copy-complex-single-float).  Little-endian: realpart at
;;; the lower address (+4 = complex-single-float.realpart, imagpart +8)
;;; lands in lane 0 of a single D-form LDUR -- one load replaces both,
;;; the established w1 deviation.  target's class is
;;; :complex-single-float (:fpr 64); the explicit (:d target) view keeps
;;; the template selection unambiguous (w1 copy-complex precedent).
(define-arm64-vinsn get-complex-single (((target :complex-single-float))
                                        ((source :lisp)))
  (ldur (:d target) (:@ source (:$ arm64::complex-single-float.realpart))))

;;; ============ get-complex-double ============
;;; Emit site: w3 complex-double aset leg (arm642-additions-w3.lisp:422).
;;; PPC64 ppc64-vinsns.lisp:2651 (lfd realpart / lfd imagpart into a
;;; pair); Matt: ONE 128-bit vector register, lanes 0/1 of the D view
;;; (his %make-complex-double-float).  His assembler has NO Q-form
;;; load/store template (verified @115b7aa), so the load is two D-form
;;; LDURs + INS -- exactly his %make-complex-double-float lane idiom.
;;; Offsets symbolic: complex-double-float has a PAD slot (his arch
;;; @449-452: pad @+4, realpart @+12, imagpart @+20; both simm9,
;;; unaligned => LDUR).
(define-arm64-vinsn get-complex-double (((target :complex-double-float))
                                        ((source :lisp))
                                        ((temp :double-float)))
  (ldur (:d target) (:@ source (:$ arm64::complex-double-float.realpart)))
  (ldur temp (:@ source (:$ arm64::complex-double-float.imagpart)))
  (ins (:d target 1) (:d temp 0)))

;;; ============================================================
;;; (f) singletons
;;; ============================================================

;;; ============ %car ============
;;; Emit site: w3 arm642-reference-list (arm642-additions-w3.lisp:169).
;;; PPC64 ppc64-vinsns.lisp:1985: (ld dest cons.car src).  His layout
;;; (define-lisp-object cons fulltag-cons cdr car): car @ +5, cdr @ -3
;;; relative to the fulltag-cons-tagged pointer -- neither 8-aligned =>
;;; LDUR (offsets SYMBOLIC per the w3 handler contract).
(define-arm64-vinsn %car (((dest :lisp))
                          ((src :lisp)))
  (ldur dest (:@ src (:$ arm64::cons.car))))

;;; ============ %cdr ============
;;; Emit site: arm642-additions-w3.lisp:168.  PPC64
;;; ppc64-vinsns.lisp:1981: (ld dest cons.cdr src).
(define-arm64-vinsn %cdr (((dest :lisp))
                          ((src :lisp)))
  (ldur dest (:@ src (:$ arm64::cons.cdr))))

;;; ============ %slot-ref ============
;;; Emit site: w3 %slot-ref handler (arm642-additions-w3.lisp:1192,
;;; (! %slot-ref dest v i)).  PPC64 ppc64-vinsns.lisp:618:
;;;   (la scaled misc-data-offset index) / (ldx dest instance scaled) /
;;;   (tdeqi dest slot-unbound-marker)
;;; At shift 3 the fixnum index IS the byte offset (the PPC64 pun, U5m),
;;; so scaled = index + misc-data-offset -- but misc-data-offset is
;;; NEGATIVE (-4) since a9ab24b and ADD's imm12 is unsigned (no
;;; template), so the bias is applied as one SUB of the negated
;;; constant; register-offset LDR (w1 copy-lexpr-argument idiom).
;;; slot-unbound-marker = subtag-slot-unbound = 42 (fits cmp imm12).
;;; PPC64's (:ne dest) constraint on instance carried.
;;;
;;; The signal is a THREE-register error (dest, instance, index) and
;;; 16 bits of udf immediate cannot hold three 5-bit fields plus a
;;; format, so per doc/porting/arm64.md "Errors that need three
;;; registers" it is a primary unary UUO naming the slot vector,
;;; FOLLOWED by a uuo_extra_registers companion carrying (index, dest).
;;; The companion is data: the handler reads it and resumes past both
;;; words, so execution never reaches it.  Emitting the two adjacently
;;; is load-bearing -- the handler reads the second at pc+4.
;;;
;;; Was `brk #xf0fb', the last placeholder 16m40's UUO swap left because
;;; arm64-uuo.s assigned no primary code (patch 0052 assigns
;;; unary_info_slot_unbound = 6).  brk is NOT a uuo on arm64, so reading
;;; an unbound slot KILLED THE IMAGE instead of signalling -- it walled
;;; stage 11 at SLOT-UNBOUND.1 and again at WITH-SLOTS (16m42).
(define-arm64-vinsn %slot-ref (((dest :lisp))
                               ((instance (:lisp (:ne dest)))
                                (index :lisp))
                               ((scaled :s64)))
  (sub scaled index (:$ (:apply - arm64::misc-data-offset)))
  (ldr dest (:@ instance scaled))
  (cmp dest (:$ arm64::slot-unbound-marker))
  (b.ne :ok)
  (uuo-error-slot-unbound instance)
  (uuo-extra-registers index dest)
  :ok)

;;; ============ check-misc-bound ============
;;; Emit sites: w3 vref/vset guards and %slot-ref/check leg
;;; (arm642-additions-w3.lisp:742/:945/:1190, (! check-misc-bound idx v)).
;;; PPC64 ppc64-vinsns.lisp:455:
;;;   (ld temp misc-header-offset v) / (srdi temp num-subtag-bits) /
;;;   (sldi temp fixnumshift) / (tdlge idx temp)
;;; Header's element count (bits 8+) re-boxed as a fixnum, then an
;;; UNSIGNED bound check: trap unless idx <u count.  His define-header
;;; = (count << num-subtag-bits) | subtag -- same field.  Header at -4 =>
;;; LDUR.  Trap placeholder #xf0fc: his canon has the BINARY
;;; uuo_error_vector_bounds ra=idx rb=v for exactly this (no mnemonic at
;;; 115b7aa yet -- Uw3b-1).
(define-arm64-vinsn check-misc-bound (()
                                      ((idx :imm)
                                       (v :lisp))
                                      ((temp :u64)))
  (ldur temp (:@ v (:$ arm64::misc-header-offset)))
  (lsr temp temp (:$ arm64::num-subtag-bits))
  (lsl temp temp (:$ arm64::fixnumshift))
  (cmp idx temp)
  (b.lo :ok)
  (uuo-error-vector-bounds idx v)
  :ok)

;;; ============ istruct-typep ============
;;; Emit site: w3 istruct-typep handler (arm642-additions-w3.lisp:1078,
;;; (! istruct-typep dest val type) -> signed-natural 0 iff VAL is an
;;; istruct whose istruct-cell (slot 0) is EQ to TYPE).  PPC64
;;; ppc64-vinsns.lisp:3082 line-by-line:
;;;   fulltag -> dest; cmp fulltag-misc; dest = -1 (movn #0, flag-safe
;;;   after the cmp, exactly PPC's li -1 slot); not misc => done (-1);
;;;   dest = subtag byte; != subtag-istruct => done (dest = subtag, always
;;;   nonzero -- subtags are >= 1); temp = slot 0 (a NODE -- :lisp temp,
;;;   GC-safe); dest = temp - type (0 iff EQ).
;;; dest is a :s64 result -- the allocator draws it from the unboxed
;;; pool, disjoint from the :lisp operands (PPC64 relies on the same).
(define-arm64-vinsn istruct-typep (((dest :s64))
                                   ((val :lisp)
                                    (type :lisp))
                                   ((temp :lisp)))
  (and dest val (:$ arm64::fulltagmask))
  (cmp dest (:$ arm64::fulltag-misc))
  (movn dest (:$ 0))
  (b.ne :done)
  (ldurb (:w dest) (:@ val (:$ arm64::misc-subtag-offset)))
  (cmp dest (:$ arm64::subtag-istruct))
  (b.ne :done)
  (ldur temp (:@ val (:$ arm64::misc-data-offset)))
  (sub dest temp type)
  :done)

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp ksignalerr): nargs pre-set by
;;; the %err-disp handler; .SPksignalerr already registered (extension
;;; line 51, spentry body verified in the draft lane).
(define-arm64-vinsn (ksignalerr :call :subprim) (()
                                                 ()
                                                 ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPksignalerr")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ---------------------------------------------------------------
;;; LEAD ADDENDUM (post-wave, gate-19 frontier)
;;; ---------------------------------------------------------------

;;; vstack-discard -- drop NWORDS nodes from the value stack.  Demanded by
;;; the call cluster (arm642-call-fn mvpass/cleanup paths).  PPC64
;;; original (ppc64-vinsns.lisp): (la vsp (ash nwords 3) vsp); his vsp
;;; grows down (str reg [vsp,#-8]! pushes), so discard = add.
(define-arm64-vinsn vstack-discard (()
                                    ((nwords :u16const)))
  (add vsp vsp (:$ (:apply ash nwords 3))))

;;; vframe-store -- store twin of HIS live vframe-load (arm64-vinsns.lisp
;;; :120-125, offset arithmetic copied verbatim); scaled STR encodes the
;;; same node-aligned offsets the load does.
(define-arm64-vinsn vframe-store (()
                                  ((src :lisp)
                                   (frame-offset :u16const)
                                   (cur-vsp :u16const)))
  (str src (:@ vsp (:$ (:apply - (:apply - cur-vsp
                                         arm64::word-size-in-bytes)
                                 frame-offset)))))

;;; make-stack-cons -- cons a cell on the TEMP STACK (tsp=x24 in his map).
;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:2106-2116 make-tsp-cons): 32-byte
;;; tstack frame; stdu -> str-preindex-writeback is the exact ARM64
;;; equivalent; payload zeroed before the tagged stores (GC-safe walk
;;; order); car/cdr offsets symbolic off HIS cons layout so the
;;; expression is correct for any tagged-relative cons.car/cons.cdr.
;;; RATIFY (proceed-unless-objection): the tstack FRAME SHAPE
;;; ([backlink][0][payload x2], dest = tsp+16+fulltag-cons) adopts PPC64
;;; verbatim -- his kernel's tstack walker doesn't exist yet and must
;;; match this layout when it lands.
(define-arm64-vinsn make-stack-cons (((dest :lisp))
                                     ((car :lisp) (cdr :lisp)))
  (str tsp (:@! tsp (:$ -32)))
  (str xzr (:@ tsp (:$ 8)))
  (str xzr (:@ tsp (:$ 16)))
  (str xzr (:@ tsp (:$ 24)))
  (str car (:@ tsp (:$ (+ 16 arm64::fulltag-cons arm64::cons.car))))
  (str cdr (:@ tsp (:$ (+ 16 arm64::fulltag-cons arm64::cons.cdr))))
  (add dest tsp (:$ (+ 16 arm64::fulltag-cons))))

;;; ============ symbol-function ============
;;; Emit site: w4 %function handler (arm642-additions-w4.lisp:48).
;;; PPC64 ppc64-vinsns.lisp:3150 is the logic lineage: val := sym.fcell,
;;; trap error-udf unless val is a function.  Since the fulltag-function
;;; removal (patch 0055) the test is PPC64's own two-step shape again:
;;; fulltag-misc, then the header's subtag-function byte
;;; (misc-subtag-offset = -12, LDURB range).  symbol.fcell = 17
;;; (tip-verified: define-fixedsized-object symbol (fulltag-symbol);
;;; odd because relative to the #b0111-tagged pointer -- LDUR takes it
;;; and the effective address is 8-aligned).  Fixnums (tag 0), NIL, and
;;; any unbound-marker non-function all fail the first compare -- logic
;;; transfers from PPC64 unchanged.
(define-arm64-vinsn symbol-function (((val :lisp))
                                     ((sym (:lisp (:ne val))))
                                     ((tag :u64)))
  (ldur val (:@ sym (:$ arm64::symbol.fcell)))
  (and tag val (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldur tag (:@ val (:$ arm64::misc-header-offset)))
  (and tag tag (:$ #xff))
  (cmp tag (:$ arm64::subtag-function))
  (b.eq :good)
  :bad
  (uuo-error-udf sym)
  :good)

;;; ============ cons ============
;;; Emit site: arm642-cons (arm642-additions-w4.lisp) -- the gate-25
;;; frontier (`unhandled cons acode`).  PPC64 ppc64-vinsns.lisp:2376 is
;;; the logic lineage (la/tdlt/std/std/mr/rldicr); the CANONICAL arm64
;;; sequence is HIS OWN `Cons` macro (lisp-kernel/arm64-macros.s:43-52
;;; @8b1ed24), copied instruction-for-instruction -- the order (tagged
;;; allocptr decrement, trap, cdr store FIRST, car, dest copy, tag
;;; clear LAST) is what the kernel's pc_luser_xp recovery expects; do
;;; not reorder.  uuo_alloc = uuo_misc 1 = udf #(1<<2 | format 0) =
;;; udf #4 (his arm64-uuo.s @8b1ed24; udf #0 is the code-vector
;;; sentinel, misc info must be non-zero).  allocptr/allocbase are his
;;; x26/x27 aliases (arm64-asm.lisp:217-218).  cons.cdr = -3 /
;;; cons.car = +5 off the fulltag-cons-tagged pointer -- neither
;;; str-scalable => STUR.  cons.size - fulltag-cons = 13 (sub imm12).
;;; Tag clear: BIC-immediate does not exist in A64 (his assembler has
;;; only the shifted-register bic -- gate-26 "no template matched");
;;; per his msg-18 rule (logical immediates are unsigned; bic-with-imm
;;; is lapmacro territory) it is AND with the complemented mask,
;;; written as the UNSIGNED 64-bit value (ldb wrap -- CL lognot 15 is
;;; the banned -16).  dest is written only after both inputs are
;;; consumed (operand-alias order rule).
(define-arm64-vinsn cons (((dest :lisp))
                          ((newcar :lisp)
                           (newcdr :lisp)))
  (sub allocptr allocptr (:$ (- arm64::cons.size arm64::fulltag-cons)))
  (cmp allocptr allocbase)
  (b.hi :no-trap)
  (udf (:$ 4))                          ;uuo_alloc (uuo_misc 1)
  :no-trap
  (stur newcdr (:@ allocptr (:$ arm64::cons.cdr)))
  (stur newcar (:@ allocptr (:$ arm64::cons.car)))
  (mov dest allocptr)
  (and allocptr allocptr (:$ (:apply ldb (byte 64 0)
                                     (:apply lognot arm64::fulltagmask)))))

;;; ============ funcall / tail-funcall-gen / tail-funcall-slide ============
;;; Gate-26 demand (funcall, 1 hit; emit sites arm642-additions.lisp
;;; :729/:731/:733 -- the general call-fn case where the callee is not
;;; known at compile time).  PPC64 ppc64-vinsns.lisp:3963-3967:
;;;   (define-ppc64-subprim-call-vinsn (funcall) .SPfuncall)
;;;   (define-ppc64-subprim-jump-vinsn (tail-funcall-gen) .SPtfuncallgen)
;;;   (define-ppc64-subprim-jump-vinsn (tail-funcall-slide) .SPtfuncallslide)
;;; Dispatch shape = his call-subprim-1/2 (movz offset / ldr off rnil /
;;; blr, or br for the tail jumps); offsets resolved BY NAME at expand
;;; time (w1 heap-cons-rest-arg precedent, U3/U4) -- all three are in the
;;; PROPOSED subprims extension with spentry-D bodies (funcall@145,
;;; tfuncallgen@662, tfuncallslide@689).  Scratch imm1 (w1 U2 family
;;; uniformity: the callee designator rides in arg_z/nfn-adjacent
;;; registers, and imm0 carries nargs-adjacent state at some emit
;;; sites).
;;; !! CALLEE ABI (boot-16m5e root): codegen produces the callee in NFN
;;; (destreg, arm642-additions.lisp:624) but the kernel funcall family
;;; (his _SPfuncall + our spentry-D funcall/tfuncall*) reads TEMP0
;;; (x86-64-style, `and imm0,temp0,#fulltagmask...`).  Each dispatch
;;; adapts with `mov temp0, nfn` -- ratify the register with Matt.
(define-arm64-vinsn (funcall :call) (()
                                     ()
                                     ((temp (:u64 #.arm64::imm1))
                                      (callee (:lisp #.arm64::temp0))))
  (mov callee nfn)
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPfuncall")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (tail-funcall-gen :jumplr) (()
  ()
  ((temp (:u64 #.arm64::imm1))
   (callee (:lisp #.arm64::temp0))))
  (mov callee nfn)
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPtfuncallgen")))
  (ldr temp (:@ rcontext temp))
  (br temp))

(define-arm64-vinsn (tail-funcall-slide :jumplr) (()
  ()
  ((temp (:u64 #.arm64::imm1))
   (callee (:lisp #.arm64::temp0))))
  (mov callee nfn)
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPtfuncallslide")))
  (ldr temp (:@ rcontext temp))
  (br temp))

;;; tail-funcall-vsp -- 4th family member (PPC64 3969), missed at the
;;; gate-26 draft, demanded at gate-32; spentry-D body ported
;;; (ppc-spentry.s:2299-2306, = tfuncallgen's no-stack-args leg).
(define-arm64-vinsn (tail-funcall-vsp :jumplr) (()
  ()
  ((temp (:u64 #.arm64::imm1))
   (callee (:lisp #.arm64::temp0))))
  (mov callee nfn)
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPtfuncallvsp")))
  (ldr temp (:@ rcontext temp))
  (br temp))

;;; ============ MV-pass + known-tail-call family ============
;;; Gate-34 demand (pass-multiple-values-symbol) + sibling sweep of the
;;; whole PPC64 dispatch block ppc64-vinsns.lisp:3947-3957.  All six
;;; are pure subprim dispatch (call for the mvpass pair, jump for the
;;; tail calls); kernel bodies live in spentry-D (mvpass@279,
;;; mvpasssym@1581, tcallsymgen@720, tcallsymslide@751, tcallnfngen@772,
;;; tcallnfnslide@780); registered in the PROPOSED subprims extension.
;;; 16m5t fix: p2 stages the callee in NFN, but kernel _SPmvpass tails
;;; into _SPfuncall which dispatches on TEMP0 (PPC do_funcall contract) --
;;; same adapter as tail-funcall-vsp above.  Without it, funcall trapped
;;; on stale temp0 (= the CALLER's own code-vector left by its entry
;;; sequence; observed at compute-hash-code's keytransF mv-call).
;;; mvpasssym needs no adapter (kernel reads fname from temp3).
(define-arm64-vinsn (pass-multiple-values :call)
    (() () ((temp (:u64 #.arm64::imm1))
            (callee (:lisp #.arm64::temp0))))
  (mov callee nfn)
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmvpass")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (pass-multiple-values-symbol :call)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmvpasssym")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (tail-call-sym-gen :jumplr)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPtcallsymgen")))
  (ldr temp (:@ rcontext temp))
  (br temp))

(define-arm64-vinsn (tail-call-sym-slide :jumplr)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPtcallsymslide")))
  (ldr temp (:@ rcontext temp))
  (br temp))

(define-arm64-vinsn (tail-call-fn-gen :jumplr)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPtcallnfngen")))
  (ldr temp (:@ rcontext temp))
  (br temp))

(define-arm64-vinsn (tail-call-fn-slide :jumplr)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPtcallnfnslide")))
  (ldr temp (:@ rcontext temp))
  (br temp))

;;; ============ misc-alloc / misc-alloc-init ============
;;; Gate-36 %make-uvector cluster.  PPC64 ppc64-vinsns.lisp:4051/4053
;;; subprim calls (.SPmisc-alloc: arg_y=element-count, arg_z=subtag ->
;;; arg_z; -init additionally arg_x=count, arg_y=subtag, arg_z=initval).
;;; Kernel bodies: spentry-A-alloc-numbers.s misc_alloc@450 /
;;; misc_alloc_init@679; registered in the extension.
(define-arm64-vinsn (misc-alloc :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmisc-alloc")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (misc-alloc-init :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmisc-alloc-init")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ stack/list allocation family ============
;;; Gate-39 demand (make-stack-block) + sibling sweep of the PPC64
;;; subprim block ppc64-vinsns.lisp:3983-3999 (make-stack-gvector
;;; already above).  All standard by-name dispatch; kernel bodies:
;;; spentry-A makestackblock@579/makestackblock0@736/makestacklist@619/
;;; stack_misc_alloc@504/stack_misc_alloc_init@705, spentry-B
;;; conslist@371/conslist_star@386/mkstackv@449.
(define-arm64-vinsn (list :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPconslist")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (list* :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPconslist-star")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (make-stack-block :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmakestackblock")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (make-stack-block0 :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmakestackblock0")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (make-stack-list :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmakestacklist")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (make-stack-vector :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmkstackv")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (stack-misc-alloc :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPstack-misc-alloc")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (stack-misc-alloc-init :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPstack-misc-alloc-init")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ vcell triple (post-wall scan; PPC64 2072-2096) ============
;;; Closed-over-variable value cells: slot-0 ref/set (LDUR/STUR @ -4;
;;; vcell-set carries NO write barrier on PPC64 -- the barrier'd path is
;;; .SPgvset via the handler, same split as misc-set-node) + make-vcell
;;; on the cons/%alloc-misc-fixed alloc canon (value-cell = header +
;;; 1 cell = 16 bytes; his value-cell-header @708).  Operand-alias
;;; order: closed is stored via dest only AFTER dest is formed --
;;; PPC64's (:ne dest) constraint carried.
(define-arm64-vinsn vcell-ref (((dest :lisp))
                               ((vcell :lisp)))
  (ldur dest (:@ vcell (:$ arm64::misc-data-offset))))

(define-arm64-vinsn vcell-set (()
                               ((vcell :lisp)
                                (value :lisp)))
  (stur value (:@ vcell (:$ arm64::misc-data-offset))))

;;; ===== interrupt-level bind/unbind quad (PPC64 4116-4168) =====
;;; All four subprim-dispatch (kernel bodies spentry-C 804/830/847/866;
;;; registered).  The -inline variants stay PPC64-only for now (they
;;; need the tlb/db-link vpush protocol; the subprim does the same work).
(define-arm64-vinsn (bind-interrupt-level-0 :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPbind-interrupt-level-0")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (bind-interrupt-level-m1 :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPbind-interrupt-level-m1")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (bind-interrupt-level :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPbind-interrupt-level")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (unbind-interrupt-level :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPunbind-interrupt-level")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; trap-unless-double-float -- doubles are BOXED miscobjs (unlike the
;;; immediate singles); subtag two-step like trap-unless-macptr, named
;;; arch error code.
(define-arm64-vinsn trap-unless-double-float (()
                                              ((object :lisp))
                                              ((tag :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-double-float))
  (b.eq :ok)
  :bad
  (uuo-error-reg-not-xtype object (:$ arm64::subtag-double-float))
  :ok)

;;; ============ lambda-bind wave: stack-cons-list pair ============
;;; PPC64 ppc64-vinsns.lisp:3979/3983 (.SPstkconslist{,-star}) --
;;; dynamic-extent &rest lists consed on the temp stack.  Kernel
;;; bodies: spentry-B stkconslist@400 / stkconslist_star@428;
;;; registered.  Standard by-name dispatch.
(define-arm64-vinsn (stack-cons-list :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPstkconslist")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (stack-cons-list* :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPstkconslist-star")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ demand-scan CUT-9: heap-box csf ============
;;; complex-single-float->heap -- box a csf (one 64-bit payload word)
;;; into a fresh 16-byte miscobj: the cons/%alloc-misc-fixed alloc
;;; canon + a D-form STUR of both lanes to realpart (-4).  His arch
;;; defines no csf header constant (define-header list ends at macptr)
;;; -- computed inline as make-vheader's expansion
;;; (logior subtag (ash element-count 8)); make-vheader is a MACRO in
;;; his arch.lisp (not a defun), so it cannot be (:apply)'d -- use the
;;; underlying logior/ash functions, which the assembler CAN apply.
(define-arm64-vinsn complex-single-float->heap (((dest :lisp))
                                                ((val :complex-single-float))
                                                ((header :u64)))
  ;; element-count 2: csf sits in ivector-class-32-bit (2 x 4 = 8 data
  ;; bytes) -- x8664 setup-complex-single-float-allocation canon
  ;; (make-vheader 2 subtag-complex-single-float), x8664-vinsns:2527.
  (movz header (:$ (:apply logior arm64::subtag-complex-single-float
                           (:apply ash 2 8))))
  (sub allocptr allocptr (:$ (- 16 arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hi :no-trap)
  (udf (:$ 4))                          ;uuo_alloc (uuo_misc 1)
  :no-trap
  (stur header (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov dest allocptr)
  (and allocptr allocptr (:$ (:apply ldb (byte 64 0)
                                     (:apply lognot arm64::fulltagmask))))
  (stur val (:@ dest (:$ arm64::complex-single-float.realpart))))

;;; complex-double-float->heap -- cut-9's sibling: box a cdf (both
;;; double lanes live in one 128-bit Q reg, :fpr 128 per his
;;; arm64-asm.lisp:2934) into a fresh 32-byte miscobj.  Layout is his
;;; define-fixedsized-object complex-double-float (pad realpart
;;; imagpart) -- the pad word 16-aligns realpart (arch comment @261).
;;; element-count 6: cdf is ivector-class-32-bit (6 x 4 = 24 data
;;; bytes) -- x8664 setup-complex-double-float-allocation canon
;;; (make-vheader 6 subtag-complex-double-float), x8664-vinsns:2522.
;;; Store composed as two D STURs + DUP (the w3a
;;; misc-set-c-complex-double-float idiom): his template table has no
;;; Q-form stur, and a bare 128-bit val matches nothing -- the original
;;; single (stur val ...) here was an arm642-expand-vinsn "unhandled
;;; form" (instruction silently DROPPED; caught in realgate-w10a).
(define-arm64-vinsn complex-double-float->heap (((dest :lisp))
                                                ((val :complex-double-float))
                                                ((header :u64)
                                                 (dtemp :double-float)))
  (movz header (:$ (:apply logior arm64::subtag-complex-double-float
                           (:apply ash 6 8))))
  (sub allocptr allocptr (:$ (- arm64::complex-double-float.size
                                arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hi :no-trap)
  (udf (:$ 4))                          ;uuo_alloc (uuo_misc 1)
  :no-trap
  (stur header (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov dest allocptr)
  (and allocptr allocptr (:$ (:apply ldb (byte 64 0)
                                     (:apply lognot arm64::fulltagmask))))
  (stur (:d val) (:@ dest (:$ arm64::complex-double-float.realpart)))
  (dup dtemp (:d val 1))
  (stur dtemp (:@ dest (:$ (+ arm64::complex-double-float.realpart 8)))))

;;; ============ demand-scan CUT-8: the last vinsn ============
;;; trap-unless-single-float -- singles are IMMEDIATES in his scheme
;;; (fulltag-single-float #b0001, "and nothing but"; the #b1001 slot is
;;; reserved-not-live), so this is a pure fulltag check -- no subtag
;;; step.  arch enum has short-float (the CL name for his 32-bit
;;; single) -- named brk.
(define-arm64-vinsn trap-unless-single-float (()
                                              ((object :lisp))
                                              ((tag :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-single-float))
  (b.eq :ok)
  (uuo-error-reg-not-xtype object (:$ arm64::subtag-single-float))
  :ok)

;;; ============ demand-scan CUT-7 wave: 2 stragglers ============
;;; spread-list (PPC64 3973, .SPspreadargz -- already registered) and
;;; req-heap-rest-arg (PPC64 4037, .SPreq-heap-rest-arg -- spentry-D
;;; body @434, registered below).  Standard by-name dispatch.
(define-arm64-vinsn (spread-list :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPspreadargz")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (req-heap-rest-arg :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPreq-heap-rest-arg")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ demand-scan CUT-6 wave: the last 2 ============

;;; save-lisp-context-lexpr -- PPC64 ppc64-vinsns.lisp:3502 (like the
;;; vsp variant but savefn = 0: lexpr frames carry no fn).  HIS frame
;;; canon (marker/vsp @0/8, fn/lr @16/24; -32 = stack-down, msg-26 +
;;; his fixed canon); xzr in the fn slot; (mov fn nfn) per the PPC64
;;; donor.
(define-arm64-vinsn save-lisp-context-lexpr (()
                                             ()
                                             ((marker-reg :imm)))
  (mov marker-reg (:$ arm64::lisp-frame-marker))
  (stp marker-reg vsp (:@! sp (:$ -32)))
  ;; savelr = temp4 = the lexpr cleanup continuation .SPlexpr-entry
  ;; handed back (ret1val_addr on the mv path, lexpr_return1v on the
  ;; 1v path) -- PPC64's `std loc_pc savelr(sp)', NOT lr: lr here is
  ;; the return-to-prologue from the subprim blr, and storing it made
  ;; the function's return jump back into its own prologue (16m10
  ;; infinite vector-allocation loop, disasm-observed).  CONSTRAINT:
  ;; temp4 must survive from save-lexpr-argregs to here --
  ;; copy-lexpr-argument sits between them for num-fixed > 0 lexprs;
  ;; RESOLVED 16m11: the unwired :lisp pool DOES include temp4
  ;; (lib/arm64env.lisp:33), so copy-lexpr-argument's temp is WIRED to
  ;; temp0 (w1).
  (stp xzr temp4 (:@ sp (:$ 16)))
  (mov fn nfn))

;;; get-complex-double-float -- x8664's 128-bit load; his template
;;; table has no Q-form load, so composed exactly like w3a's
;;; misc-ref-c-complex-double-float: D-lane 0 (zeroing upper), imag
;;; word into a D temp, INS to lane 1.  realpart = +4 (see w3a KEY).
(define-arm64-vinsn get-complex-double-float (((result :complex-double-float))
                                              ((source :lisp))
                                              ((dtemp :double-float)))
  (ldur (:d result) (:@ source (:$ arm64::complex-double-float.realpart)))
  (ldur dtemp (:@ source (:$ (+ arm64::complex-double-float.realpart 8))))
  (ins (:d result 1) (:d dtemp 0)))

;;; ============ demand-scan CUT-5 wave: 3 vinsns ============

;;; trap-unless-complex-double-float -- sibling of the csf trap above
;;; (x8664 lineage; same two-step shape; expected-type =
;;; subtag-complex-double-float for the udf wrong-type sweep; #xf0ff
;;; parameterized placeholder, no arch error code).
(define-arm64-vinsn trap-unless-complex-double-float (()
                                                      ((object :lisp))
                                                      ((tag :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-complex-double-float))
  (b.eq :ok)
  :bad
  (uuo-error-reg-not-xtype object (:$ arm64::subtag-complex-double-float))
  :ok)

;;; slide-values -- PPC64 ppc64-vinsns.lisp:4015 subprim call .SPmvslide
;;; (kernel body spentry-D mvslide@1532 ends in ret; registered).
;;; MUST link (blr): donor is define-ppc64-subprim-call-vinsn (bla); a br
;;; here made mvslide's ret warp to the caller's stale lr (16m5u fault).
(define-arm64-vinsn (slide-values :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmvslide")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; save-lexpr-argregs -- PPC64 ppc64-vinsns.lisp:3525 LINE-PORT.
;;; vpush the received register args (arg_x/y/z) under min-fixed
;;; predicates, push the extra-args count, entry-vsp (WIRED imm0 -- the
;;; .SPlexpr-entry payload, so dispatch scratch is imm1), call
;;; .SPlexpr-entry (kernel body spentry-E lexpr_entry@604; registered).
;;; PPC's dual-crf compares (crfx/crfy live simultaneously) serialize
;;; onto NZCV: compare/branch pairs in an order that preserves the
;;; exactly-0/exactly-2/one/3+ dispatch; unsigned branches (b.lo)
;;; mirror cmpldi.  nargs fixnums => (ash n 3) byte constants.
;;; 3 = $numarm64argregs, INLINED (arm64env loads later -- cut-3 lesson).
(define-arm64-vinsn (save-lexpr-argregs :call :subprim)
    (()
     ((min-fixed :u16const))
     ((entry-vsp (:u64 #.arm64::imm0))
      (arg-temp :u64)
      (temp (:u64 #.arm64::imm1))))
  ((:pred >= min-fixed 3)               ;all argregs already fixed
   (str arg_x (:@! vsp (:$ -8)))
   (str arg_y (:@! vsp (:$ -8)))
   (str arg_z (:@! vsp (:$ -8))))
  ((:pred = min-fixed 2)                ;at least 2 args
   (cmp nargs (:$ (ash 2 arm64::fixnumshift)))
   (b.eq :yz2)                          ;skip arg_x if exactly 2
   (str arg_x (:@! vsp (:$ -8)))
   :yz2
   (str arg_y (:@! vsp (:$ -8)))
   (str arg_z (:@! vsp (:$ -8))))
  ((:pred = min-fixed 1)                ;at least one arg
   (cmp nargs (:$ (ash 2 arm64::fixnumshift)))
   (b.lo :z1)                           ;exactly one
   (b.eq :yz1)                          ;exactly two
   (str arg_x (:@! vsp (:$ -8)))
   :yz1
   (str arg_y (:@! vsp (:$ -8)))
   :z1
   (str arg_z (:@! vsp (:$ -8))))
  ((:pred = min-fixed 0)
   (cmp nargs (:$ 0))
   (b.eq :none)                         ;exactly zero
   (cmp nargs (:$ (ash 2 arm64::fixnumshift)))
   (b.eq :yz0)                          ;exactly two
   (b.lo :z0)                           ;one
                                        ;three or more ...
   (str arg_x (:@! vsp (:$ -8)))
   :yz0
   (str arg_y (:@! vsp (:$ -8)))
   :z0
   (str arg_z (:@! vsp (:$ -8)))
   :none
   )
  ((:pred = min-fixed 0)
   (str nargs (:@! vsp (:$ -8))))
  ((:not (:pred = min-fixed 0))
   (sub arg-temp nargs (:$ (:apply ash min-fixed arm64::fixnumshift)))
   (str arg-temp (:@! vsp (:$ -8))))
  (add entry-vsp vsp nargs)
  (add entry-vsp entry-vsp (:$ 8))
  ;; LEXPR-RA (spentry-E lexpr_entry contract): the caller's return pc
  ;; travels in temp4 (PPC's loc_pc channel); the subprim hands back the
  ;; lexpr cleanup continuation in temp4 for save-lisp-context-lexpr's
  ;; savelr.  Last before the blr so the unwired arg-temp above can't
  ;; alias it.  (16m10 third spin: without this + the savelr store
  ;; below, a compiled lexpr's frame savelr was the return-to-prologue
  ;; address -- returning re-entered the function forever.)
  (mov temp4 lr)
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPlexpr-entry")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ demand-scan CUT-4 wave: 3 micro-layer vinsns ============

;;; scale-nargs -- PPC64 ppc64-vinsns.lisp:2008: for &lexpr/&rest entry,
;;; drop the NFIXED required args from the count (nargs holds fixnums =
;;; bytes at shift 3); no-op when nfixed = 0 (same predicate).
(define-arm64-vinsn scale-nargs (()
                                 ((nfixed :s16const)))
  ((:pred > nfixed 0)
   (sub nargs nargs (:$ (:apply ash nfixed arm64::fixnumshift)))))

;;; single->node -- donor x8664-vinsns.lisp:2007 (bits<<32 | tag): his
;;; single-floats are IMMEDIATES, fulltag-single-float #b0001 with the
;;; float bits in the TOP 32 (same layout as x8664's tag-single-float
;;; scheme).  fmov W-view zero-extends the 32 float bits; orr imm 1 is
;;; a valid logical immediate.
(define-arm64-vinsn single->node (((result :lisp))
                                  ((source :single-float)))
  (fmov (:w result) source)
  (lsl result result (:$ 32))
  (orr result result (:$ arm64::fulltag-single-float)))

;;; ============ demand-scan CUT-3 wave: the 4 tail vinsns ============

;;; scale-node-misc-index -- PPC64 ppc64-vinsns.lisp:34
;;; (addi dest idx misc-data-offset): idx is a boxed fixnum = byte
;;; offset (shift-3 pun); just add the data-start bias.  The bias is
;;; NEGATIVE (-4) and ADD imm12 is unsigned => SUB of the negation
;;; (gate-24 %slot-ref class).
(define-arm64-vinsn scale-node-misc-index (((dest :u64))
                                           ((idx :lisp))
                                           ())
  (sub dest idx (:$ (:apply - arm64::misc-data-offset))))

;;; macptr->stack -- PPC64 ppc64-vinsns.lisp:2773: build a stack-consed
;;; MACPTR on the temp stack (48-byte frame: backlink + zero word +
;;; header/address/domain/type; domain/type MUST be zeroed -- tstack
;;; memory isn't 0-filled).  Frame protocol = our V3b make-stack-cons
;;; canon (str-preindex backlink, RATIFY tstack-frame-shape item).
;;; Displacements land 8-aligned (16 + fulltag-misc + slot offsets =>
;;; 16/24/32/40) so scaled STRs encode; dest = tsp + 16 + fulltag-misc.
;;; macptr-header = his define-header (arch:709), fits movz.
(define-arm64-vinsn macptr->stack (((dest :lisp))
                                   ((address :u64))
                                   ((header :u64)))
  (movz header (:$ arm64::macptr-header))
  (str tsp (:@! tsp (:$ -48)))
  (str xzr (:@ tsp (:$ 8)))
  (str header (:@ tsp (:$ (+ 16 arm64::fulltag-misc arm64::macptr.header))))
  (str address (:@ tsp (:$ (+ 16 arm64::fulltag-misc arm64::macptr.address))))
  (str xzr (:@ tsp (:$ (+ 16 arm64::fulltag-misc arm64::macptr.domain))))
  (str xzr (:@ tsp (:$ (+ 16 arm64::fulltag-misc arm64::macptr.type))))
  (add dest tsp (:$ (+ 16 arm64::fulltag-misc))))

;;; default-optionals -- PPC64 ppc64-vinsns.lisp:3695: imm0 = the boxed
;;; total-arg-count, then .SPdefault-optional-args (kernel body
;;; spentry-D default_optional_args@357; registered).  imm1 scratch.
(define-arm64-vinsn (default-optionals :call :subprim)
    (()
     ((n :u16const))
     ((temp (:u64 #.arm64::imm1))))
  (movz imm0 (:$ (:apply ash n arm64::fixnumshift)))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPdefault-optional-args")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; get-complex-single-float -- donor x8664-vinsns.lisp:1980 (one 64-bit
;;; load of both S lanes from complex-single-float.realpart); emit site
;;; is HIS arm642.lisp:1200.  realpart = misc-data-offset = -4 => D-form
;;; LDUR (w3a misc-ref-c-complex-single-float precedent -- lanes land
;;; realpart->S[0], imagpart->S[1] on little-endian).
(define-arm64-vinsn get-complex-single-float (((result :complex-single-float))
                                              ((source :lisp)))
  (ldur result (:@ source (:$ arm64::complex-single-float.realpart))))

;;; ============ demand-scan CUT-2 wave: 6 second-layer blockers ============
;;; (demand-scan cut 2: call-subprim-3 x3 files, save-lisp-context-
;;; variable x2, call-label, setq-special, trap-unless-{complex-single-
;;; float,macptr}.)

;;; call-subprim-3 -- 3-node-arg member of his call-subprim-1/2 family.
;;; Emit contract (ours, w2:387 + w3:772): (! call-subprim-3 dest
;;; spoffset x y z) with x/y/z TARGETED to arg_x/arg_y/arg_z (the
;;; .SPgvset ABI; emit site asserts the targeting).  Scratch is imm1,
;;; NOT imm0 -- his -1/-2 wire imm0 while their emit sites pass an
;;; argument in imm0 (the cont-71 collision class, already mailed);
;;; this one stays out of that hole.
(define-arm64-vinsn (call-subprim-3 :call :subprim) (((dest :lisp))
                                                     ((spoffset :u16const)
                                                      (x :lisp)
                                                      (y :lisp)
                                                      (z :lisp))
                                                     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ spoffset))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; save-lisp-context-variable -- donor: ARM32 arm-vinsns.lisp:3630 (no
;;; PPC64 analog by this name; PPC64's save-lisp-context-vsp is the
;;; fixed-vsp cousin).  savevsp = vsp + max(0, nargs - argregs.8): when
;;; the caller passed more than $numarm64argregs args the extras were
;;; vpushed and the saved vsp must point above them.  nargs holds
;;; FIXNUM-tagged counts (shift 3) so the subtraction is directly in
;;; bytes.  ARM32's movmi (mov-if-minus) becomes csel-on-pl with xzr.
;;; Frame push mirrors HIS no-stack-args canon (-32 stack-down, the
;;; msg-26-confirmed direction his canon now carries).  The trailing
;;; (mov fn nfn) is the
;;; ARM32 donor's; NOTE (mail): none of HIS three save-lisp-context
;;; vinsns sets fn from nfn, yet his ref-constant addresses constants
;;; off fn -- prologue gap in his tree.
(define-arm64-vinsn save-lisp-context-variable (()
                                                ()
                                                ((marker-reg :imm)
                                                 (vsp-reg :imm)))
  ;; 3 node arg regs (arg_x/y/z; $numarm64argregs) << fixnumshift = 24.
  ;; Inlined: $numarm64argregs lives in arm64env, which loads AFTER
  ;; this file -- referencing it here aborted the whole file's load
  ;; (demand-scan cut-3 regression).  His arch defines no nargregs
  ;; constant Lisp-side (his kernel asm's `nargregs` is .s-only) --
  ;; MAIL nit.
  (subs vsp-reg nargs (:$ (ash 3 arm64::fixnumshift)))
  (csel vsp-reg vsp-reg xzr (:? pl))
  (add vsp-reg vsp-reg vsp)
  (mov marker-reg (:$ arm64::lisp-frame-marker))
  (stp marker-reg vsp-reg (:@! sp (:$ -32)))
  (stp fn lr (:@ sp (:$ 16)))
  (mov fn nfn))

;;; call-label -- PPC64 ppc64-vinsns.lisp:2184 verbatim (bl label).
(define-arm64-vinsn (call-label :call) (()
                                        ((label :label)))
  (bl label))

;;; setq-special -- PPC64 ppc64-vinsns.lisp:3143 (bla .SPspecset);
;;; sym/val in arg_y/arg_z per the spentry ABI.  .SPspecset already
;;; registered (spentry-A body).
(define-arm64-vinsn (setq-special :call :subprim)
    (()
     ((sym :lisp)
      (val :lisp))
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPspecset")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; trap-unless-macptr -- PPC64 ppc64-vinsns.lisp:1589; two-step
;;; typecode extraction exactly like w3b's trap-unless-typecode=
;;; (fulltag; misc -> header subtag byte), trap unless subtag-macptr.
;;; arch::error-object-not-macptr exists (arch.lisp enum) -- named brk.
(define-arm64-vinsn trap-unless-macptr (()
                                        ((object :lisp))
                                        ((tag :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-macptr))
  (b.eq :ok)
  :bad
  (uuo-error-reg-not-xtype object (:$ arm64::subtag-macptr))
  :ok)

;;; trap-unless-complex-single-float -- donor x8664-vinsns.lisp:849
;;; (same two-step shape; its uuo carries subtag-complex-single-float
;;; as the expected-type byte -- exactly his wrong_type udf namespace).
;;; No arch::error-object-not-complex-single-float exists, so the trap
;;; is the PARAMETERIZED placeholder #xf0ff (w3b trap-unless-typecode=
;;; precedent); the udf sweep encodes expected-type =
;;; subtag-complex-single-float.
(define-arm64-vinsn trap-unless-complex-single-float (()
                                                      ((object :lisp))
                                                      ((tag :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-complex-single-float))
  (b.eq :ok)
  :bad
  (uuo-error-reg-not-xtype object (:$ arm64::subtag-complex-single-float))
  :ok)

;;; ref-symbol-value -- PPC64 ppc64-vinsns.lisp:3099 (bla .SPspecrefcheck):
;;; special-variable read WITH unbound check in the subprim.  Emit site:
;;; our w2 special-ref handler leg (arm642-additions-w2.lisp:319).
;;; .SPspecrefcheck registered (kernel body spentry-A:372).
(define-arm64-vinsn (ref-symbol-value :call :subprim)
    (((val :lisp))
     ((sym (:lisp (:ne val))))
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPspecrefcheck")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; save-lisp-context-offset -- PPC64 ppc64-vinsns.lisp:3482 (frame push
;;; with saved-vsp = vsp + nbytes-vpushed, for functions that vpushed
;;; register args before building the frame).  Body mirrors HIS
;;; save-lisp-context-no-stack-args canon (arm64-vinsns.lisp:25-30)
;;; INSTRUCTION-FOR-INSTRUCTION, with the vsp+nbytes computed into an
;;; imm temp for the savevsp slot.  Deviations from PPC64 follow his
;;; canon: no cs-limit stack probe (his canon has none yet).
;;; (mov fn nfn) per PPC64:3490 -- the "his lambda path does it
;;; separately" claim (msg-18) was refuted for the canon variant
;;; (patch 0010, e4440cb) and holds nowhere for -offset either:
;;; arm642-argregs-entry emits this vinsn bare for >3-arg functions,
;;; so without it fn stays the CALLER's fn (or 0 via _SPmvpass) and
;;; the first fn-relative constant ref reads the wrong function --
;;; 16m5r frontier fault (fn=0 at lr=ret1val_addr).
;;; Sign RESOLVED (msg-26 + his canon now -32): stack-down pre-index.
;;; The old +32 mirror survived here past the canon sweep and cost the
;;; 16m5i toplevel-vsp wall (unwind-protect cleanup frame built UP,
;;; clobbering caller frames) -- 2026-07-17 boot-validated.
(define-arm64-vinsn save-lisp-context-offset (()
                                              ((nbytes-vpushed :u16const))
                                              ((marker-reg :imm)
                                               (vsp-reg :imm)))
  ;; w10 fix: full u16 window for the vsp-reg add (two-lane; nbytes >= 0,
  ;; first insn reads vsp/writes vsp-reg, second chains off vsp-reg).
  ((:not (:pred = 0 (:apply ldb (byte 12 0) nbytes-vpushed)))
   (add vsp-reg vsp (:$ (:apply ldb (byte 12 0) nbytes-vpushed)))
   ((:not (:pred = 0 (:apply ldb (byte 12 12) nbytes-vpushed)))
    (add vsp-reg vsp-reg (:$ (:apply ldb (byte 12 12) nbytes-vpushed) :lsl 12))))
  ((:pred = 0 (:apply ldb (byte 12 0) nbytes-vpushed))
   (add vsp-reg vsp (:$ (:apply ldb (byte 12 12) nbytes-vpushed) :lsl 12)))
  (mov marker-reg (:$ arm64::lisp-frame-marker))
  (stp marker-reg vsp-reg (:@! sp (:$ -32)))
  (stp fn lr (:@ sp (:$ 16)))
  (mov fn nfn))

;;; ============ fitvals ============
;;; Gate-37 multiple-value-bind cluster.  PPC64 ppc64-vinsns.lisp:4049
;;; (.SPfitvals: imm0 = requested byte count -- cont-71 ABI note -- pads/
;;; truncates the returned values on the vstack to exactly that many).
;;; Kernel body: spentry-D fitvals@300; registered in the extension.
(define-arm64-vinsn (fitvals :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPfitvals")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ adjust-vsp ============
;;; Gate-29 demand.  PPC64 ppc64-vinsns.lisp:2793 (la vsp amount vsp) --
;;; amount SIGNED; sign-split exactly like its w1 sibling adjust-sp
;;; (AArch64 add/sub-imm is unsigned 12-bit; the s61/s72/s89
;;; negative-immediate truncation class).
;;; w10 fix: full s16 window, two-lane (the adjust-sp/add-immediate
;;; pattern -- vsp chains, order-safe).
(define-arm64-vinsn adjust-vsp (()
                                ((amount :s16const)))
  ((:pred >= amount 0)
   ((:not (:pred = 0 (:apply ldb (byte 12 0) amount)))
    (add vsp vsp (:$ (:apply ldb (byte 12 0) amount)))
    ((:not (:pred = 0 (:apply ldb (byte 12 12) amount)))
     (add vsp vsp (:$ (:apply ldb (byte 12 12) amount) :lsl 12))))
   ((:pred = 0 (:apply ldb (byte 12 0) amount))
    (add vsp vsp (:$ (:apply ldb (byte 12 12) amount) :lsl 12))))
  ((:pred < amount 0)
   ((:not (:pred = 0 (:apply ldb (byte 12 0) (:apply - amount))))
    (sub vsp vsp (:$ (:apply ldb (byte 12 0) (:apply - amount))))
    ((:not (:pred = 0 (:apply ldb (byte 12 12) (:apply - amount))))
     (sub vsp vsp (:$ (:apply ldb (byte 12 12) (:apply - amount)) :lsl 12))))
   ((:pred = 0 (:apply ldb (byte 12 0) (:apply - amount)))
    (sub vsp vsp (:$ (:apply ldb (byte 12 12) (:apply - amount)) :lsl 12)))))

;;; ============ discard-temp-frame ============
;;; Gate-30 demand.  PPC64 ppc64-vinsns.lisp:2258:
;;;   (define-ppc64-vinsn (discard-temp-frame :tsp :pop :discard)
;;;     (() ()) (ld tsp 0 tsp))
;;; -- pop the temp-stack frame by loading the BACKLINK, which every
;;; tstack frame stores at [tsp, #0] (our V3b make-stack-cons writes it
;;; with (str tsp (:@! tsp (:$ -32))); same PPC64 stdu protocol --
;;; RATIFY item "tstack frame shape" already queued with Matt).
;;; tsp = his x24 (arm64-asm.lisp:215); frames are 16-aligned so the
;;; offset-0 scaled LDR encodes.
(define-arm64-vinsn (discard-temp-frame :tsp :pop :discard) (()
                                                             ())
  (ldr tsp (:@ tsp (:$ 0))))

;;; ============ lisp-word-ref / lisp-word-ref-c ============
;;; Gate-31 demand (%lisp-word-ref handler, arm642-additions-w4.lisp).
;;; PPC64 ppc64-vinsns.lisp:2214/2219:
;;;   (ldx dest base offset)  /  (ld dest offset base)
;;; base holds a fixnum-locative: under fixnumshift=3 the raw bits of
;;; an 8-aligned address ARE the address (the PPC64 pun -- v2's
;;; scale-by-node-shift ARM64-DEVIATION was fixnumshift=0 logic and is
;;; DEAD in his design).  Register case: offset is the BOXED fixnum =
;;; byte offset, passed straight through (PPC64-verbatim).
;;; Constant case: %lisp-word-ref feeds idx<<3 (8-aligned), but the
;;; raw-byte-offset callers (%fixnum-ref / %fixnum-ref-natural) feed
;;; fixedsized-object slot constants which under HIS
;;; misc-data-offset=-4 are 8k-4 -- POSITIVE and unaligned (empirical:
;;; catch-frame.db-link=20, l1-lisp-threads UOFF3 wall).  Scaled LDR
;;; (uoff3, reach 32760) requires offset >= 0 AND 8-aligned; everything
;;; else takes LDUR (simm9, reach -256..255) -- the handlers gate their
;;; constants to exactly those two windows (documented ARM64 deviation
;;; from PPC's single sb16 ld).
(define-arm64-vinsn lisp-word-ref (((dest :lisp))
                                   ((base :lisp)
                                    (offset :lisp)))
  (ldr dest (:@ base offset)))

(define-arm64-vinsn lisp-word-ref-c (((dest :lisp))
                                     ((base :lisp)
                                      (offset :s16const)))
  ((:pred < offset 0)
   (ldur dest (:@ base (:$ offset))))
  ((:and (:pred >= offset 0) (:pred = 0 (:apply logand offset 7)))
   (ldr dest (:@ base (:$ offset))))
  ((:and (:pred >= offset 0) (:not (:pred = 0 (:apply logand offset 7))))
   (ldur dest (:@ base (:$ offset)))))

;;; ============ %closure-code% ============
;;; Gate-33 make-closure cluster.  PPC64 ppc64-vinsns.lisp:2119: load
;;; the VCELL of the nil-relative symbol %closure-code% (holds the
;;; shared closure-trampoline code-vector).  It is in HIS nrs list
;;; (arm64-arch.lisp:377) -- far beyond simm9 and not 8-aligned, so the
;;; movz+regoffset idiom (his call-subprim shape).  The offset formula
;;; tracks his list via read-eval so a reorder can't silently skew it.
;;; 16m5x FIX (mechanism gdb-observed): his nrs-offset values are
;;; NILSYM-relative (t=-64, nil=0); the rnil-relative anchor is
;;; t-offset+symbol.size (= +92; arm64-lisp-globals.s nrs record;
;;; VERIFIED live: tsym=rnil+28, nilsym=rnil+92, [rnil+1573]=the
;;; trampoline cv while the old anchor-less read [rnil+1481]=0x3020
;;; garbage that broke every runtime closure's slot 0).
(define-arm64-vinsn %closure-code% (((dest :lisp))
                                    ()
                                    ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ #.(+ arm64::t-offset arm64::symbol.size
                      arm64::symbol.vcell
                      (* (1- (position 'ccl::%closure-code%
                                       arm64::*nilreg-relative-symbols*))
                         arm64::symbol.size))))
  (ldr dest (:@ rnil temp)))

;;; ============ %alloc-misc-fixed ============
;;; PPC64 ppc64-vinsns.lisp:2414 logic (round nbytes+header to a dnode,
;;; decrement, trap, plant header, tag) composed on HIS Misc_Alloc_Fixed
;;; canon (lisp-kernel/arm64-macros.s:68-76): the same alloc protocol as
;;; the `cons' vinsn above (udf #4 = uuo_alloc; order load-bearing).
;;; misc-header-offset = -12 => STUR.  Emit sites gate the size within
;;; sub's imm12 (PPC64 has the equivalent constraint via la simm16).
(define-arm64-vinsn %alloc-misc-fixed (((dest :lisp))
                                       ((Rheader :u64)
                                        (nbytes :u32const)))
  ;; ARM64-DEVIATION: `sub Xd,Xn,#imm' takes a 12-bit unsigned immediate,
  ;; optionally shifted left by 12, so a request over 4095 bytes cannot be
  ;; spelled in ONE sub and the assembler refuses the vinsn:
  ;;   vinsn immediate 4324 (shift 0) out of range for operand class :AIMM
  ;; (compiling arm64-asm.lisp: a 539-element gvector literal).  PPC64's
  ;; donor never hits this -- its `la' displacement is simm16.  Split into
  ;; the u12<<12 lane plus the u12 lane; the `(:$ v :lsl 12)' spelling is
  ;; live in the assembler (vinsn-parse-immediate, :aimm accepts shift 12),
  ;; and 0081's own note at the lri site documents both lanes.
  ((:pred <= (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc) 4095)
   (sub allocptr allocptr (:$ (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc))))
  ((:not (:pred <= (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc) 4095))
   (sub allocptr allocptr (:$ (:apply ash (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc) -12) :lsl 12))
   (sub allocptr allocptr (:$ (:apply logand 4095 (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc)))))
  (cmp allocptr allocbase)
  (b.hi :no-trap)
  (udf (:$ 4))                          ;uuo_alloc (uuo_misc 1)
  :no-trap
  (stur Rheader (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov dest allocptr)
  (and allocptr allocptr (:$ (:apply ldb (byte 64 0)
                                     (:apply lognot arm64::fulltagmask)))))

;;; ============ %arm64-gvector ============
;;; Donor: PPC64 ppc64-vinsns.lisp:2389 %ppc-gvector (LINE-PORT).  The
;;; donor is literally %alloc-misc-fixed (ppc64-vinsns.lisp:2414)
;;; followed by a store loop, and the handler that emits it --
;;; arm642-allocate-initialized-gvector's inline leg (arm642.lisp:4975,
;;; from ppc2.lisp:4484 ppc2-allocate-initialized-gvector) -- is a
;;; faithful port already; only this vinsn was missing (defined NOWHERE
;;; at our pin 9c61574 OR at Matt's tip 9fb47830: an upstream defect).
;;;
;;; Semantics preserved exactly from the donor: bump allocptr down by the
;;; 16-aligned (header + data) size less fulltag-misc, trap against
;;; allocbase, store the header at misc-header-offset, set dest, clear
;;; allocptr's tag bits, then -- unless nbytes is 0 -- pop n node words
;;; off vsp and store them into dest from the LAST index down to the
;;; first.
;;;
;;; Alloc/trap protocol taken verbatim from OUR double->heap
;;; (arm64-vinsns.lisp:1125-1138) and macptr->heap (:5088-5100), which
;;; are the sites whose b.hs matches the donor's tdlt (see below).
;;; STORE-LOOP KEY.  misc-data-offset is -4 on this LOW-TAG target
;;; (arm64-arch.lisp:262-263: misc-header-offset = -fulltag-misc = -12,
;;; misc-data-offset = -12 + 8).  Offsets are relative to the TAGGED
;;; dest, so element i lands at dest + (-4 + 8i) = base + 8 + 8i, i.e.
;;; correctly 8-aligned; the register-offset STR form adds a full 64-bit
;;; two's-complement Xm, so the final (negative) -4 offset is fine.
(define-arm64-vinsn %arm64-gvector (((dest :lisp))
                                    ((Rheader :u64)
                                     (nbytes :u32const))
                                    ((immtemp0 :u64)
                                     (nodetemp :lisp)))
  ;; ARM64-DEVIATION: `sub Xd,Xn,#imm' takes a 12-bit unsigned immediate,
  ;; optionally shifted left by 12, so a request over 4095 bytes cannot be
  ;; spelled in ONE sub and the assembler refuses the vinsn:
  ;;   vinsn immediate 4324 (shift 0) out of range for operand class :AIMM
  ;; (compiling arm64-asm.lisp: a 539-element gvector literal).  PPC64's
  ;; donor never hits this -- its `la' displacement is simm16.  Split into
  ;; the u12<<12 lane plus the u12 lane; the `(:$ v :lsl 12)' spelling is
  ;; live in the assembler (vinsn-parse-immediate, :aimm accepts shift 12),
  ;; and 0081's own note at the lri site documents both lanes.
  ((:pred <= (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc) 4095)
   (sub allocptr allocptr (:$ (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc))))
  ((:not (:pred <= (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc) 4095))
   (sub allocptr allocptr (:$ (:apply ash (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc) -12) :lsl 12))
   (sub allocptr allocptr (:$ (:apply logand 4095 (:apply - (:apply logand (lognot 15)
                                               (:apply + (+ 15 8) nbytes))
                                     arm64::fulltag-misc)))))
  (cmp allocptr allocbase)
  ;;; ARM64-DEVIATION: PPC's single `tdlt allocptr allocbase' has no
  ;;; ARM64 analog (no trap-on-condition instruction), so it becomes
  ;;; cmp + skip-branch + trap.  tdlt traps on STRICTLY less-than, so
  ;;; equality must NOT trap => the skip is b.hs, not b.hi.
  (b.hs :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (stur Rheader (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov dest allocptr)
  ;;; ARM64-DEVIATION: PPC's `rldicr allocptr allocptr 0 (- 63 ntagbits)'
  ;;; (clear the low ntagbits) becomes AND with the UNSIGNED complement
  ;;; of fulltagmask -- ARM64 logical immediates are unsigned.
  (and allocptr allocptr (:$ (:apply ldb (byte 64 0)
                                     (:apply lognot arm64::fulltagmask))))
  ((:not (:pred = nbytes 0))
   ;;; ARM64-DEVIATION: PPC's `li immtemp0 <imm>' has no single-instruction
   ;;; ARM64 spelling.  `mov Xd,#imm' is an ALIAS (movz/movn/orr), so the
   ;;; assembler refuses it -- "Ambiguous immediate or condition ...: 3
   ;;; templates match; use a specific (non-alias) instruction", which fails
   ;;; the LOAD of this whole file.  movz+movk is the idiom, spelled exactly as
   ;;; the pin's own constant loader spells it -- `lri :constant-ref'
   ;;; (arm64-vinsns.lisp:596-607).  NB the shift lives INSIDE the (:$ ...)
   ;;; form: `(movk d (:$ <expr> :lsl 16))', never `(movk d (:$ <expr>) :lsl
   ;;; 16)' -- the latter loads as "don't understand (MOVK ...)".
   ;;; Both halves unconditionally, rather than movz alone: nbytes is
   ;;; 8*(length initforms), so a literal of more than 8192 elements
   ;;; overflows movz's 16-bit field, and this must not depend on the
   ;;; assembler happening to reject rather than truncate it.
   (movz immtemp0 (:$ (:apply logand #xffff
                              (:apply + arm64::misc-data-offset nbytes))))
   (movk immtemp0 (:$ (:apply logand #xffff
                              (:apply ash (:apply + arm64::misc-data-offset nbytes) -16))
                      :lsl 16))
   :loop
   (sub immtemp0 immtemp0 (:$ arm64::node-size))
   ;;; ARM64-DEVIATION: the donor's `cmpdi crf immtemp0 misc-data-offset'
   ;;; compares against -4.  ARM64 cmp takes an UNSIGNED 12-bit
   ;;; immediate, so the negative compare is expressed as CMN against
   ;;; +4 (cmn Xn,#4 sets Z iff Xn = -4).  No :crf temp is declared --
   ;;; ARM64 has one NZCV, so the donor's crf operand drops out.
   ;;; Neither LDR nor STR writes flags, so keeping the donor's
   ;;; compare-before-load order is still correct here.
   (cmn immtemp0 (:$ (:apply - arm64::misc-data-offset)))
   ;;; ARM64-DEVIATION: the donor's `ld nodetemp 0 vsp' + `la vsp 8 vsp'
   ;;; fuse into one post-indexed load -- this repo's established vpop
   ;;; idiom (vpop-register, arm64-vinsns.lisp:658).
   (ldr nodetemp (:@+ vsp (:$ arm64::node-size)))
   ;;; register-offset store, as misc-set-node (arm64-vinsns.lisp:2000)
   ;;; does for PPC64's stdx.
   (str nodetemp (:@ dest immtemp0))
   (b.ne :loop)))

;;; ============ make-stack-gvector ============
;;; PPC64 ppc64-vinsns.lisp:3995: subprim call .SPstkgvector (kernel
;;; body: spentry-B-vectors-misc.s:769; registered in the PROPOSED
;;; subprims extension).  Standard by-name dispatch, imm1 scratch.
(define-arm64-vinsn (make-stack-gvector :call :subprim)
    (()
     ()
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPstkgvector")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ tag-as-function ============
;;; Since the fulltag-function removal (patch 0055) a finished closure
;;; already carries its final tag (fulltag-misc) straight out of the
;;; misc allocator, exactly as on PPC64 -- this vinsn is now a plain
;;; move.  It was "the single seam to change" if Matt landed a different
;;; convention, and he did: this IS that change.  Kept (as a move)
;;; because patch 0042's arm642-make-closure emit site calls it with
;;; dest = src, where it degenerates to a no-op mov.
(define-arm64-vinsn tag-as-function (((dest :lisp))
                                     ((src :lisp)))
  (mov dest src))

;;; ============ make-stack-vcell ============
;;; Demand: l0-hash (gate 2026-07-15); emit site = w2 arm642-bind-var
;;; (arm642-additions-w2.lisp:464).  PPC64 LINE-PORT (ppc64-vinsns.lisp
;;; make-stack-vcell, the 2072-2096 vcell cluster): a value-cell
;;; uvector stack-consed on the TEMP STACK.  Frame protocol = our V3b
;;; make-stack-cons canon (32-byte frame: backlink str-preindex, zero
;;; word, then header/value at the tagged-relative offsets; RATIFY
;;; tstack-frame-shape item already queued).  dest = tsp+16+fulltag-misc;
;;; header lands at dest+misc-header-offset (=tsp+16), value at
;;; dest+misc-data-offset (=tsp+24) under the 8b1ed24 -12/-4 layout.
(define-arm64-vinsn make-stack-vcell (((dest :lisp))
                                      ((closed :lisp))
                                      ((header :u64)))
  (movz header (:$ arm64::value-cell-header))
  (str tsp (:@! tsp (:$ -32)))
  (str xzr (:@ tsp (:$ 8)))
  (str header (:@ tsp (:$ (+ 16 arm64::fulltag-misc arm64::misc-header-offset))))
  (str closed (:@ tsp (:$ (+ 16 arm64::fulltag-misc arm64::misc-data-offset))))
  (add dest tsp (:$ (+ 16 arm64::fulltag-misc))))

;;; ============ uvsize support (demand 82, gates 5+ files) ============

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:1533 trap-unless-uvector):
;;; uvector reference = fulltag-misc; one-step fulltag check, no
;;; subtag read.  (Since the fulltag-function removal, patch 0055,
;;; functions are misc-tagged and pass this check too -- full PPC64
;;; semantics.)
;;; Interim NOT-TAG brk encoding (see file header).
(define-arm64-vinsn trap-unless-uvector (()
                                         ((object :lisp))
                                         ((tag :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.eq :ok)
  (uuo-error-reg-not-xtype object (:$ arm64::fulltag-misc))
  :ok)

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp misc-element-count-fixnum):
;;; element count = header >> num-subtag-bits, boxed by fixnumshift.
;;; Header at misc-header-offset (-12) is simm9-unaligned => LDUR.
(define-arm64-vinsn misc-element-count-fixnum (((dest :imm))
                                               ((v :lisp))
                                               ((temp :u64)))
  (ldur temp (:@ v (:$ arm64::misc-header-offset)))
  (lsr temp temp (:$ arm64::num-subtag-bits))
  (lsl dest temp (:$ arm64::fixnumshift)))

;;; ============ %gvector / %err-disp subprim calls ============

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp gvector): nargs pre-set by the
;;; handler, initial values + boxed subtag vpushed; .SPgvector builds
;;; the gvector (spentry-B-vectors-misc.s:564-584, verified body;
;;; registered in the extension this wave).
(define-arm64-vinsn (gvector :call :subprim) (()
                                              ()
                                              ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPgvector")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:76 scale-1bit-misc-index).
;;; Decompose a boxed bit-index (in an unboxed reg) into a byte
;;; word-offset from the misc pointer + the bit position within that
;;; 32-bit word.  Matt's arm64 constants are PPC64-identical here
;;; (fixnumshift=3, misc-data-offset=-4 -- his arm64-arch.lisp:269 says
;;; so verbatim), so this is a pure ISA translation of PPC64's body:
;;;   PPC (srdi word-index idx (+ 5 fixnum-shift))  -> (lsr ... (:$ 8))
;;;   PPC (sldi word-index word-index 2)            -> (lsl ... (:$ 2))
;;;   PPC (addi word-index word-index misc-data-offset) [mdo=-4]
;;;                                                 -> (sub ... (:$ 4))
;;;   PPC (extrwi bitnum idx 5 (- 32 (+ fixnum-shift 5))) i.e.
;;;       bitnum = (idx >> fixnumshift) & 31
;;; extrwi is a fixed-position bitfield extract -> raw UBFM (= ubfx
;;; bitnum,idx,#fixnumshift,#5; immr=fixnumshift, imms=fixnumshift+4),
;;; the w3b idiom.  (A plain `(lsr bitnum idx ...)` into the :u8 bitnum reg
;;; is an "unhandled form" in his arm642-expand-vinsn -- it only templates
;;; lsr into the wider :s64/:u64 class; UBFM with (:x) casts is the
;;; expander-accepted form, cf. w3b.)  word-index feeds misc-ref-u32
;;; (byte-indexed ldr), matching the W3 constant path (arm642-vref1 @716
;;; = ppc2 @1462, byte-identical).
(define-arm64-vinsn scale-1bit-misc-index (((word-index :s64)
                                            (bitnum :u8)) ; (unsigned-byte 5)
                                           ((idx :imm)) ; a fixnum
                                           ())
  (lsr word-index idx (:$ (:apply + 5 arm64::fixnumshift)))
  (lsl word-index word-index (:$ 2))
  (sub word-index word-index (:$ (:apply - arm64::misc-data-offset)))
  (ubfm (:x bitnum) (:x idx)
        (:$ arm64::fixnumshift) (:$ (:apply + arm64::fixnumshift 4))))

;;; ARM64-DEVIATION: no PPC64 analog -- PPC builds its variable single-bit
;;; mask with rlwnm/rotate; ARM64 (like ARM32) has a native variable
;;; left-shift (LSLV).  Donor: ARM32 arm-vinsns.lisp:1378
;;; shift-left-variable-word = (mov dest (:lsl src sh)); on AArch64 the
;;; register-operand LSL is LSLV, matching the lane's existing variable
;;; shifts (w1:331, w2:262).  Used by the W3 bit-vector set path
;;; (arm642-vset1 @893: mask = 1 << bit-number) -- the sole remaining
;;; wall for l0-array.
(define-arm64-vinsn shift-left-variable-word (((dest :u32))
                                              ((src :u32)
                                               (sh :u32)))
  (lsl dest src sh))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:1493 u32logandc2 = (andc dest x y),
;;; :1498 u32logior = (or dest x y)).  Pure logical ops, no offset/shift
;;; constants, so design-independent.  ARM64-DEVIATION: PPC andc -> ARM64
;;; bic (x AND NOT y); PPC or -> ARM64 orr.  Both used by the W3
;;; bit-vector set path (arm642-vset1 @906/908).
;;; !! RUNTIME LANDMINE (DECISION-LOG cont-62 2026-06-30): in v2 these
;;; assembled to a LOGICAL-IMMEDIATE form (e.g. `orr x3,x3,#0x2`) instead
;;; of the register form when the y-operand's regnum was itself a valid
;;; bitmask-immediate -- form-qualifier-tuple mis-classified the vreg as an
;;; immediate -- silently corrupting (setf (sbit) ...).  Partially
;;; de-risked (DECISION-LOG 2026-07-15 RULED): form-qualifier-tuple, the
;;; exact v2 defect site, does NOT exist in Matt's rewritten
;;; arm64-asm.lisp.  Compile-gate is unaffected; still spot-check
;;; bit-vector setf sbit the first time the image boots.
(define-arm64-vinsn u32logandc2 (((dest :u32))
                                 ((x :u32)
                                  (y :u32)))
  (bic dest x y))

(define-arm64-vinsn u32logior (((dest :u32))
                               ((x :u32)
                                (y :u32)))
  (orr dest x y))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:40 scale-32bit-misc-index).
;;; Boxed fixnum idx = N<<3; 32-bit slots are N*4 bytes past
;;; misc-data-offset:  (srdi dest idx 1) (addi dest dest -4)
;;;   -> scaled byte offset = (idx >> 1) + misc-data-offset = 4N - 4.
;;; Referenced by the landed W3 helpers arm642-vref1/arm642-vset1
;;; (variable-index 32-bit vector paths) -- called but never defined
;;; until now.
(define-arm64-vinsn scale-32bit-misc-index (((dest :u64))
                                            ((idx :imm)) ; a fixnum
                                            ())
  (lsr dest idx (:$ 1))
  (sub dest dest (:$ (:apply - arm64::misc-data-offset))))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:498 2d-dim1).
;;; dest <- unboxed dim1 of a 2d arrayH (dim1 = cell after dim0).
;;; PPC (ld dest (+ misc-data-offset (* 8 (1+ arrayH.dim0-cell))) header)
;;;     (sradi dest dest fixnumshift)
;;; Byte offset = -4 + 8*6 = 44 (simm9) -> ldur; sradi -> asr.
(define-arm64-vinsn 2d-dim1 (((dest :u64))
                             ((header :lisp)))
  (ldur dest (:@ header (:$ (+ arm64::misc-data-offset
                               (* 8 (1+ arm64::arrayH.dim0-cell))))))
  (asr dest dest (:$ arm64::fixnumshift)))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:503 3d-dims).
;;; dim1/dim2 <- unboxed dims 1 and 2 of a 3d arrayH.
(define-arm64-vinsn 3d-dims (((dim1 :u64)
                              (dim2 :u64))
                             ((header :lisp)))
  (ldur dim1 (:@ header (:$ (+ arm64::misc-data-offset
                               (* 8 (1+ arm64::arrayH.dim0-cell))))))
  (ldur dim2 (:@ header (:$ (+ arm64::misc-data-offset
                               (* 8 (+ 2 arm64::arrayH.dim0-cell))))))
  (asr dim1 dim1 (:$ arm64::fixnumshift))
  (asr dim2 dim2 (:$ arm64::fixnumshift)))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:512 check-2d-bound).
;;; Bounds-check boxed i/j against the boxed dim0/dim1 cells (both sides
;;; boxed, so raw unsigned compare is valid); return dim1 UNBOXED.
;;; PPC tdlge (trap if unsigned >=) -> cmp + b.lo over brk #xf0fc
;;; (w3b check-misc-bound bound-trap code).
(define-arm64-vinsn check-2d-bound (((dim :u64))
                                    ((i :imm)
                                     (j :imm)
                                     (header :lisp)))
  (ldur dim (:@ header (:$ (+ arm64::misc-data-offset
                              (* 8 arm64::arrayH.dim0-cell)))))
  (cmp i dim)
  (b.lo :ok-i)
  (uuo-error-array-bounds i header)
  :ok-i
  (ldur dim (:@ header (:$ (+ arm64::misc-data-offset
                              (* 8 (1+ arm64::arrayH.dim0-cell))))))
  (cmp j dim)
  (b.lo :ok-j)
  (uuo-error-array-bounds j header)
  :ok-j
  (asr dim dim (:$ arm64::fixnumshift)))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:522 check-3d-bound).
;;; 3d sibling: returns dim1 AND dim2 unboxed.
(define-arm64-vinsn check-3d-bound (((dim1 :u64)
                                     (dim2 :u64))
                                    ((i :imm)
                                     (j :imm)
                                     (k :imm)
                                     (header :lisp)))
  (ldur dim1 (:@ header (:$ (+ arm64::misc-data-offset
                               (* 8 arm64::arrayH.dim0-cell)))))
  (cmp i dim1)
  (b.lo :ok-i)
  (uuo-error-array-bounds i header)
  :ok-i
  (ldur dim1 (:@ header (:$ (+ arm64::misc-data-offset
                               (* 8 (1+ arm64::arrayH.dim0-cell))))))
  (cmp j dim1)
  (b.lo :ok-j)
  (uuo-error-array-bounds j header)
  :ok-j
  (ldur dim2 (:@ header (:$ (+ arm64::misc-data-offset
                               (* 8 (+ 2 arm64::arrayH.dim0-cell))))))
  (cmp k dim2)
  (b.lo :ok-k)
  (uuo-error-array-bounds k header)
  :ok-k
  (asr dim1 dim1 (:$ arm64::fixnumshift))
  (asr dim2 dim2 (:$ arm64::fixnumshift)))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:464 2d-unscaled-index).
;;; dest <- boxed (i*dim1 + j): i/j are boxed fixnums, dim1 is raw, and
;;; boxed*raw = boxed product (fixnumshift is a pure scale factor), so
;;; the result is a proper unscaled (boxed) index for vref1/vset1.
;;; PPC mulld -> mul, add -> add.  dim1 is in/out scratch as in PPC.
;;; NOTE (2026-07-15): v2's body is bare (mul dim1 i dim1)/(add dest dim1 j)
;;; and compiled under v2's looser assembler, but Matt's assembler rejects
;;; the mixed :u32(W)/:imm(X) widths ("no template matched").  All values
;;; are fixnum-range (boxed i/j/dest, small raw dim1), so force the X-form
;;; with (:x) casts -- the arithmetic is identical.
(define-arm64-vinsn 2d-unscaled-index (((dest :imm)
                                        (dim1 :u32))
                                       ((dim1 :u32)
                                        (i :imm)
                                        (j :imm)))
  (mul (:x dim1) (:x i) (:x dim1))
  (add (:x dest) (:x dim1) (:x j)))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:474 3d-unscaled-index).
;;; dest <- boxed (+ (* i dim1 dim2) (* j dim2) k); dim1/dim2 raw in/out.
(define-arm64-vinsn 3d-unscaled-index (((dest :imm)
                                        (dim1 :u64)
                                        (dim2 :u64))
                                       ((dim1 :u64)
                                        (dim2 :u64)
                                        (i :imm)
                                        (j :imm)
                                        (k :imm)))
  ;; (:x) casts as in 2d-unscaled-index: Matt's assembler needs uniform
  ;; X-width mul/add operands (dim1/dim2 are :u64 already, but i/j/k/dest
  ;; are :imm -- cast all so no W/X mismatch).
  (mul (:x dim1) (:x dim1) (:x dim2))
  (mul (:x dim2) (:x j) (:x dim2))
  (mul (:x dim1) (:x i) (:x dim1))
  (add (:x dim2) (:x dim1) (:x dim2))
  (add (:x dest) (:x dim2) (:x k)))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:537 array-data-vector-ref).
;;; dest <- arrayH.data-vector slot.  arm64::arrayH.data-vector is his
;;; define-lisp-object byte offset (= misc-data-offset + 8*data-vector-cell
;;; = 12); not 8-aligned -> ldur.
(define-arm64-vinsn array-data-vector-ref (((dest :lisp))
                                           ((header :lisp)))
  (ldur dest (:@ header (:$ arm64::arrayH.data-vector))))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:560 trap-unless-simple-array-2).
;;; Trap (brk #xf0nn, nn = type-error code, w3b trap-unless-fixnum idiom)
;;; unless OBJECT is an arrayH of rank 2 whose boxed flags word equals
;;; boxed EXPECTED-FLAGS (subtag-of-underlying-vector dpb'd over
;;; $arh_simple_bit -- see the arm642-aref2/aset2 callers).
;;; PPC64 body mapping (straightened branch schedule, same semantics):
;;;   clrldi tag obj 60 / cmpdi fulltag-misc  -> and fulltagmask + cmp
;;;   lbz tag misc-subtag-offset              -> ldurb (:w tag) @ -12
;;;   ld rank / cmpdi (ash 2 fixnumshift)     -> ldur @ arrayH.rank + cmp 16
;;;   lis/ori boxed expected-flags halves     -> movz/movk (lri idiom;
;;;     boxed flags = 16-bit subtag/simple field << 3 = max 19 bits)
;;;   ld flags arrayH.flags / cmpd            -> ldur + cmp
;;;   uuo_interr type-error object            -> brk #xf000|type-error
(define-arm64-vinsn trap-unless-simple-array-2 (()
                                                ((object :lisp)
                                                 (expected-flags :u64const)
                                                 (type-error :u8const))
                                                ((tag :u64)
                                                 (flags :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-arrayH))
  (b.ne :bad)
  (ldur tag (:@ object (:$ arm64::arrayH.rank)))
  (cmp tag (:$ (:apply ash 2 arm64::fixnumshift)))
  (b.ne :bad)
  (movz tag (:$ (:apply ldb (byte 16 0)
                        (:apply ash expected-flags arm64::fixnumshift))))
  (movk tag (:$ (:apply ldb (byte 16 16)
                        (:apply ash expected-flags arm64::fixnumshift)) :lsl 16))
  (ldur flags (:@ object (:$ arm64::arrayH.flags)))
  (cmp tag flags)
  (b.eq :good)
  :bad
  (uuo-error-array-flags tag object)
  :good)

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:585 trap-unless-simple-array-3).
;;; Rank-3 sibling; only the rank constant differs.
(define-arm64-vinsn trap-unless-simple-array-3 (()
                                                ((object :lisp)
                                                 (expected-flags :u64const)
                                                 (type-error :u8const))
                                                ((tag :u64)
                                                 (flags :u64)))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-arrayH))
  (b.ne :bad)
  (ldur tag (:@ object (:$ arm64::arrayH.rank)))
  (cmp tag (:$ (:apply ash 3 arm64::fixnumshift)))
  (b.ne :bad)
  (movz tag (:$ (:apply ldb (byte 16 0)
                        (:apply ash expected-flags arm64::fixnumshift))))
  (movk tag (:$ (:apply ldb (byte 16 16)
                        (:apply ash expected-flags arm64::fixnumshift)) :lsl 16))
  (ldur flags (:@ object (:$ arm64::arrayH.flags)))
  (cmp tag flags)
  (b.eq :good)
  :bad
  (uuo-error-array-flags tag object)
  :good)

;;; ARM32 LINE-PORT (arm-vinsns.lisp:3090 trap-unless-typed-array-2); PPC64
;;; has no analog (ppc2 emits NO check when the declared type is not a known
;;; simple subtag -- his arm642 aset2/aset3/aref3 sites follow arm2's richer
;;; shape, whose else-arm emits this).  OBJECT must be an arrayH of rank 2
;;; whose flags-cell subtag byte equals SUBTAG; the simple/displaced bits are
;;; deliberately NOT tested (any complexity is acceptable here).
;;; A64 deviations from the ARM32 body, both immediate-width forced:
;;;   - boxed expected subtag = subtag << (8+fixnumshift) needs up to 19 bits
;;;     (not movz/cmp-imm12 material), so build it once up front with the
;;;     file's movz/movk idiom and compare REGISTER-register -- which also
;;;     leaves the uuo's expected-datum reg valid on every :bad path;
;;;   - the flags word is masked to its subtag byte with a logical immediate
;;;     (#xff << 11: one contiguous run, bitmask-encodable).
(define-arm64-vinsn trap-unless-typed-array-2 (()
                                               ((object :lisp)
                                                (subtag :u8const))
                                               ((tag :u64)
                                                (flags :u64)))
  (movz flags (:$ (:apply ldb (byte 16 0)
                          (:apply ash subtag (+ 8 arm64::fixnumshift)))))
  (movk flags (:$ (:apply ldb (byte 16 16)
                          (:apply ash subtag (+ 8 arm64::fixnumshift))) :lsl 16))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-arrayH))
  (b.ne :bad)
  (ldur tag (:@ object (:$ arm64::arrayH.rank)))
  (cmp tag (:$ (:apply ash 2 arm64::fixnumshift)))
  (b.eq :rank-ok)
  (movz flags (:$ (:apply ash 2 arm64::fixnumshift)))
  (uuo-error-array-rank flags object)
  :rank-ok
  (ldur tag (:@ object (:$ arm64::arrayH.flags)))
  (and tag tag (:$ (:apply ash #xff (+ 8 arm64::fixnumshift))))
  (cmp tag flags)
  (b.eq :good)
  :bad
  (uuo-error-array-flags flags object)
  :good)

;;; ARM32 LINE-PORT (arm-vinsns.lisp:3138 trap-unless-typed-array-3).
;;; Rank-3 sibling; only the rank constant differs.
(define-arm64-vinsn trap-unless-typed-array-3 (()
                                               ((object :lisp)
                                                (subtag :u8const))
                                               ((tag :u64)
                                                (flags :u64)))
  (movz flags (:$ (:apply ldb (byte 16 0)
                          (:apply ash subtag (+ 8 arm64::fixnumshift)))))
  (movk flags (:$ (:apply ldb (byte 16 16)
                          (:apply ash subtag (+ 8 arm64::fixnumshift))) :lsl 16))
  (and tag object (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldurb (:w tag) (:@ object (:$ arm64::misc-subtag-offset)))
  (cmp tag (:$ arm64::subtag-arrayH))
  (b.ne :bad)
  (ldur tag (:@ object (:$ arm64::arrayH.rank)))
  (cmp tag (:$ (:apply ash 3 arm64::fixnumshift)))
  (b.eq :rank-ok)
  (movz flags (:$ (:apply ash 3 arm64::fixnumshift)))
  (uuo-error-array-rank flags object)
  :rank-ok
  (ldur tag (:@ object (:$ arm64::arrayH.flags)))
  (and tag tag (:$ (:apply ash #xff (+ 8 arm64::fixnumshift))))
  (cmp tag flags)
  (b.eq :good)
  :bad
  (uuo-error-array-flags flags object)
  :good)

;;; PPC64 LINE-PORT of the u8/u16->fixnum pattern extended to 32 bits
;;; (ppc64-vinsns.lisp:2472/2488 use
;;;   (clrlsldi result val (- nbits-in-word W) fixnumshift)
;;; = zero-extend the low W bits, then shift left by fixnumshift).  For
;;; W=32 that is exactly AArch64 UBFIZ result, val, #fixnumshift, #32,
;;; which is the UBFM insert form (imms<immr): raw UBFM per lane
;;; convention (ubfiz/ubfx are his lapmacros, avoided in-lane).
;;;   UBFM Xd,Xn,#immr,#imms with immr=(- nbits-in-word fixnumshift)=61,
;;;   imms=(1- 32)=31  =>  Xd = ZeroExtend(Xn[0:31]) << (64-61)
;;;                        = (val & #xffffffff) << fixnumshift.
;;; A boxed u32 (max 2^32-1) << 3 = 2^35 max, well within a 61-bit
;;; fixnum, so no overflow.  result :imm holds the boxed fixnum.
(define-arm64-vinsn u32->fixnum (((result :imm))
                                 ((val :u32)))
  (ubfm (:x result) (:x val)
        (:$ (- arm64::nbits-in-word arm64::fixnumshift))
        (:$ (1- 32))))

;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:611 node-slot-ref).  Constant-index
;;; boxed slot read: dest <- [node + misc-data-offset + cellno*8].
;;; PPC (ld dest (+ misc-data-offset (ash cellno 3)) node).  Emitted by
;;; arm642-global-ref (w8) and other constant node-slot readers; missing
;;; from his tree / additions / v2-carryover.
;;; ARM64-DEVIATION vs v2: v2 used scaled LDR (valid only at its
;;; misc-data-offset=0); under Matt's -4 the offset is -4+8k (==4 mod 8),
;;; not 8-aligned for scaled LDR -> LDUR (simm9; small cellno fits), per
;;; the misc-ref-c-node / W7 array-cell idiom.
(define-arm64-vinsn node-slot-ref (((dest :lisp))
                                   ((node :lisp)
                                    (cellno :u32const)))
  (ldur dest (:@ node (:$ (:apply + arm64::misc-data-offset
                                  (:apply ash cellno arm64::word-shift))))))

;;; X8664 LINE-PORT (x8664-vinsns.lisp symbol-ref).  Constant-index slot
;;; read off a SYMBOL-TAGGED pointer: dest <- [sym + (node-size -
;;; fulltag-symbol) + cellno*8] (vcell-cell=1 -> +9).  node-slot-ref
;;; above is the misc-tagged sibling (misc-data-offset = -4 under his
;;; tags); feeding it a symptr reads 5 bytes into the wrong slot -- the
;;; 16m8 SPECIFIER-TYPE wall (gdb-observed: ldur [sym,#4] returned
;;; 0x1300b0000302000 for a vcell holding NIL).  x8664 keeps the two
;;; vinsns distinct for exactly this reason; PPC64 has no symbol-ref
;;; because its symbols ARE misc-tagged.
(define-arm64-vinsn symbol-ref (((dest :lisp))
                                ((src :lisp)
                                 (cellno :u32const)))
  (ldur dest (:@ src (:$ (:apply + (:apply - arm64::node-size arm64::fulltag-symbol)
                                 (:apply ash cellno arm64::word-shift))))))

;;; ============ double-float-compare / single-float-compare ============
;;; PPC64 ppc64-vinsns.lisp:1859 (fcmpo crf a b) -> fcmp writes NZCV;
;;; crf is the flag pseudo.  His fcmp templates: arm64-asm.lisp:1079 (S)
;;; / 1080 (D), matched by operand width.
(define-arm64-vinsn double-float-compare (((crf :crf))
                                          ((arg0 :double-float)
                                           (arg1 :double-float)))
  (fcmp arg0 arg1))

(define-arm64-vinsn single-float-compare (((crf :crf))
                                          ((arg0 :single-float)
                                           (arg1 :single-float)))
  (fcmp arg0 arg1))

;;; ============ fp arith (8) ============
;;; PPC64 ppc64-vinsns.lisp fadd/fsub/fmul/fdiv (+ fadds etc. singles).
(define-arm64-vinsn double-float+-2 (((result :double-float))
                                     ((x :double-float)
                                      (y :double-float)))
  (fadd result x y))

(define-arm64-vinsn double-float--2 (((result :double-float))
                                     ((x :double-float)
                                      (y :double-float)))
  (fsub result x y))

(define-arm64-vinsn double-float*-2 (((result :double-float))
                                     ((x :double-float)
                                      (y :double-float)))
  (fmul result x y))

(define-arm64-vinsn double-float/-2 (((result :double-float))
                                     ((x :double-float)
                                      (y :double-float)))
  (fdiv result x y))

(define-arm64-vinsn single-float+-2 (((result :single-float))
                                     ((x :single-float)
                                      (y :single-float)))
  (fadd result x y))

(define-arm64-vinsn single-float--2 (((result :single-float))
                                     ((x :single-float)
                                      (y :single-float)))
  (fsub result x y))

(define-arm64-vinsn single-float*-2 (((result :single-float))
                                     ((x :single-float)
                                      (y :single-float)))
  (fmul result x y))

(define-arm64-vinsn single-float/-2 (((result :single-float))
                                     ((x :single-float)
                                      (y :single-float)))
  (fdiv result x y))

;;; ============ negate-fixnum-set-flags / negate-fixnum-no-ovf ============
;;; PPC64 nego./neg -> negs/neg (aliases of subs/sub from xzr,
;;; arm64-asm.lisp:900-903).  flags is the SSA pseudo; negs sets NZCV
;;; implicitly -- V flags exactly the most-negative-fixnum case (boxed
;;; min-fixnum = #x8000000000000000), consumed by his
;;; arm642-check-fixnum-overflow (arm642.lisp:2000, cond-vs).
(define-arm64-vinsn negate-fixnum-set-flags (((dest :lisp)
                                              (flags (:crf 0)))
                                             ((src :imm)))
  (negs dest src))

(define-arm64-vinsn negate-fixnum-no-ovf (((dest :lisp))
                                          ((src :imm)))
  (neg dest src))

;;; ============ fixnum->fpr ============
;;; PPC64 (ppc2 idiom): unbox then integer->double convert.  v2 @6436.
;;; asr undoes the fixnum shift; scvtf X->D (arm64-asm.lisp:1103).
(define-arm64-vinsn fixnum->fpr (((f :double-float))
                                 ((fixnum :lisp))
                                 ((temp :s64)))
  (asr temp fixnum (:$ arm64::fixnumshift))
  (scvtf f temp))

;;; ============ store-double ============
;;; PPC64 stfd -> D-form STUR at double-float.value (mirror of landed w1
;;; get-double's ldur; the offset is unscaled-range).  Overwrites the
;;; value cell of an EXISTING boxed double (emit site %setf-double-float
;;; type-checks the node first).
(define-arm64-vinsn store-double (()
                                  ((dest :lisp)
                                   (source :double-float)))
  (stur source (:@ dest (:$ arm64::double-float.value))))

;;; ============ fpr<->fpr copies (4) ============
;;; PROMOTED 16m48g (2026-07-28) into upstream patch 0061, which defines
;;; these in HIS arm64-vinsns.lisp next to %make-complex-double-float and
;;; also repoints arm642-copy-fpr at them (it emitted ARM32's
;;; single-to-single / double-to-double names, defined nowhere).  Deleted
;;; here because our additions fasls concatenate AFTER his arm64-vinsns
;;; fasl, so a leftover copy would silently win over the patched one.
;;; The promoted bodies add PPC64's dest-eq-src guard (ppc64-vinsns.lisp:
;;; 2047), which these four lacked; the fcvt pair is deliberately still
;;; unguarded.

;;; ============ vpush-argregs ============
;;; Emit site: HIS arm642-lambda keyword-bind path (arm642.lisp:1840,
;;; (! vpush-argregs num-fixed)).  Donor: ARM32 arm-vinsns.lisp:1927
;;; (same 3-arg-reg protocol; PPC's version is the vpush_argregs
;;; lapmacro, ppc-macros.s:329).  The arm64 instruction idiom is the
;;; gate-verified inline in our spentry drafts
;;; (spentry-D-call-builtins.s:360 default_optional_args): dispatch on
;;; boxed nargs, push arg_x/y/z deepest-first with pre-indexed STR.
;;; num-fixed-args known => the compile-time :pred clauses skip checks
;;; for registers known present (donor's exact split).
(define-arm64-vinsn (vpush-argregs :push :node :vsp) (()
                                                      ((num-fixed-args :u16const)))
  ((:pred = num-fixed-args 0)
   (cbz nargs :done))
  ((:pred < num-fixed-args 2)
   (cmp nargs (:$ (:apply ash 2 arm64::fixnumshift)))
   (b.lo :push-z)
   (b.eq :push-yz))
  ((:pred = num-fixed-args 2)
   (cmp nargs (:$ (:apply ash 2 arm64::fixnumshift)))
   (b.eq :push-yz))
  (str arg_x (:@! vsp (:$ (- arm64::node-size))))
  :push-yz
  (str arg_y (:@! vsp (:$ (- arm64::node-size))))
  :push-z
  (str arg_z (:@! vsp (:$ (- arm64::node-size))))
  :done)

;;; ============ integer-sign ============
;;; PPC64 define-ppc64-subprim-call-vinsn (integer-sign) .SPinteger-sign
;;; (ppc64-vinsns.lisp:4055); w4 subprim-caller canon.  Result in imm0
;;; (the emit site reads imm0 -- ppc2-int>0-p).  Spentry:
;;; spentry-A-alloc-numbers.s:399 (registered).
(define-arm64-vinsn (integer-sign :call :subprim) (()
                                                   ()
                                                   ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPinteger-sign")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ set-macptr-address ============
;;; PPC64 :2753 (std addr macptr.address src).  macptr.address is
;;; misc-data-offset-relative (-4+8k) => STUR.
(define-arm64-vinsn set-macptr-address (()
                                        ((addr :address)
                                         (src :lisp)))
  (stur addr (:@ src (:$ arm64::macptr.address))))

;;; ============ scale-8bit/16bit-misc-index ============
;;; The missing middle siblings of the landed scale-1bit (w6) and
;;; scale-32bit (w7): unbox the fixnum index to an element index, fold
;;; misc-data-offset=-4.  PPC64 :360/:352 shape.
(define-arm64-vinsn scale-8bit-misc-index (((dest :u64))
                                           ((idx :imm)) ; a fixnum
                                           ())
  (lsr dest idx (:$ arm64::fixnumshift))
  (sub dest dest (:$ (:apply - arm64::misc-data-offset))))

(define-arm64-vinsn scale-16bit-misc-index (((dest :u64))
                                            ((idx :imm)) ; a fixnum
                                            ())
  (lsr dest idx (:$ (- arm64::fixnumshift 1)))
  (sub dest dest (:$ (:apply - arm64::misc-data-offset))))

;;; ============ macptr->heap ============
;;; PPC64 ppc64-vinsns.lisp:2760: box an :address into a fresh macptr
;;; miscobj (4 words: header + address/domain/type; domain/type stay 0 --
;;; newly heap-allocated memory is 0-filled, donor comment carried).
;;; Alloc canon = w1 double->heap.  macptr-header = his define-header.
(define-arm64-vinsn macptr->heap (((dest :lisp))
                                  ((address :address))
                                  ((header :u64)))
  (mov header (:$ arm64::macptr-header))
  (sub allocptr allocptr (:$ (- arm64::macptr.size arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hs :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (stur header (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov dest allocptr)
  (and allocptr allocptr (:$ (logand #xffffffffffffffff
                                     (lognot arm64::fulltagmask))))
  (stur address (:@ dest (:$ arm64::macptr.address))))

;;; ============ character cluster ============
;;; His character rep = PPC64's exactly: (code << charcode-shift=8) |
;;; subtag-character (=fulltag-imm-0 #b0010, LOW byte; bits 4-7 zero)
;;; -- the w1 load-character-constant / w3b u32->char ground truth.
;;; fixnumshift=3 => char->fixnum is a plain LSR by 8-3=5 (the subtag's
;;; #b0010 shifts out; bit1 lands below the fixnum tag bits => 0).

;;; PPC64 ppc64-vinsns.lisp:2705 (srdi (- charcode-shift fixnumshift)).
(define-arm64-vinsn character->fixnum (((dest :lisp))
                                       ((src :lisp)))
  (lsr dest src (:$ (- arm64::charcode-shift arm64::fixnumshift))))

;;; PPC64 :1510 (clrldi num-subtag-bits / tdnei subtag-character) --
;;; low-byte extract + compare; brk #xf0xx trap canon (w3b).
(define-arm64-vinsn trap-unless-character (()
                                           ((object :lisp))
                                           ((tag :u64)))
  (and tag object (:$ #xff))
  (cmp tag (:$ arm64::subtag-character))
  (b.eq :ok)
  (uuo-error-reg-not-xtype object (:$ arm64::subtag-character))
  :ok)

;;; PPC64 :1275: fixnum AND unsigned-< (ash #x110000 fixnumshift).
;;; Reformulated for imm12: code < #x110000 <=> (code >> 16) < #x11,
;;; and boxed code>>16 = object >> (fixnumshift+16).
(define-arm64-vinsn require-char-code (()
                                       ((object :lisp))
                                       ((tag :u64)))
  :again
  (tst object (:$ arm64::fixnummask))
  (b.ne :bad)
  (lsr tag object (:$ (+ arm64::fixnumshift 16)))
  (cmp tag (:$ #x11))
  (b.lo :got-it)
  :bad
  (uuo-error-reg-not-xtype object (:$ arm64::xtype-char-code))
  (b :again)
  :got-it)

;;; PPC64 :2733 -- src is a code CODE-CHAR is known non-nil for.
(define-arm64-vinsn code-char->char (((dest :lisp))
                                     ((src :imm)))
  (lsl dest src (:$ (- arm64::charcode-shift arm64::fixnumshift)))
  (orr dest dest (:$ arm64::subtag-character)))

;;; x8664 canon (x8664-vinsns.lisp:4155; PPC64's #xfffe/#xffff leg is
;;; a UTF-16-era extra his x8664 dropped): NIL for the surrogate range
;;; [#xd800,#xdfff] (code>>11 = #x1b), else box.  temp holds the tag
;;; comparison; dest written only after src is fully consumed
;;; (operand-alias rule).
(define-arm64-vinsn fixnum->char (((dest :lisp))
                                  ((src :imm))
                                  ((temp :u64)))
  (asr temp src (:$ (+ arm64::fixnumshift 11)))
  (cmp temp (:$ (ash #xd800 -11)))
  (b.ne :ok)
  (mov dest rnil)
  (b :done)
  :ok
  (lsl dest src (:$ (- arm64::charcode-shift arm64::fixnumshift)))
  (orr dest dest (:$ arm64::subtag-character))
  :done)

;;; %scharcode8/32 + set siblings -- PPC64 :3775-3820.  Address math =
;;; the landed scale-Nbit-misc-index idiom (unbox index, fold
;;; misc-data-offset=-4 via (sub (:apply - misc-data-offset))); access =
;;; the landed misc-ref/set regoff idiom (w3a).  code result re-boxed
;;; by LSL fixnumshift.
(define-arm64-vinsn %scharcode8 (((code :imm))
                                 ((str :lisp)
                                  (idx :imm))
                                 ((imm :u64)))
  (lsr imm idx (:$ arm64::fixnumshift))
  (sub imm imm (:$ (:apply - arm64::misc-data-offset)))
  (ldrb (:w imm) (:@ str imm))
  (lsl code imm (:$ arm64::fixnumshift)))

(define-arm64-vinsn %scharcode32 (((code :imm))
                                  ((str :lisp)
                                   (idx :imm))
                                  ((imm :u64)))
  (lsr imm idx (:$ 1))
  (sub imm imm (:$ (:apply - arm64::misc-data-offset)))
  (ldr (:w imm) (:@ str imm))
  (lsl code imm (:$ arm64::fixnumshift)))

(define-arm64-vinsn %set-scharcode8 (()
                                     ((str :lisp)
                                      (idx :imm)
                                      (code :imm))
                                     ((imm :u64)
                                      (imm1 :u64)))
  (lsr imm idx (:$ arm64::fixnumshift))
  (sub imm imm (:$ (:apply - arm64::misc-data-offset)))
  (lsr imm1 code (:$ arm64::fixnumshift))
  (strb (:w imm1) (:@ str imm)))

(define-arm64-vinsn %set-scharcode32 (()
                                      ((str :lisp)
                                       (idx :imm)
                                       (code :imm))
                                      ((imm :u64)
                                       (imm1 :u64)))
  (lsr imm idx (:$ 1))
  (sub imm imm (:$ (:apply - arm64::misc-data-offset)))
  (lsr imm1 code (:$ arm64::fixnumshift))
  (str (:w imm1) (:@ str imm)))

;;; ============ symbol cluster: symptr/symvector retags ============
;;; Under his LOW tags fulltag-symbol=#b0111(7) != fulltag-misc=#b1100(12),
;;; so symptr<->symvector are RETAGS -- x8664 canon (x8664-vinsns.lisp:4483
;;; subb (- fulltag-symbol fulltag-misc), i.e. +5 toward the misc view;
;;; :4487 the reverse).  v2/PPC64's identity handlers are a
;;; symbols-are-misc-tagged artifact -- NOT carried.  The retag is blind
;;; (no NIL special case) per the x8664 canon; NIL's NRS placement makes
;;; the arithmetic land on symbol-shaped data (his arm64-constants.s
;;; nrs_symbol_fulltag/nilsym block).
(define-arm64-vinsn %symptr->symvector (((dest :lisp))
                                        ((src :lisp)))
  (add dest src (:$ (- arm64::fulltag-misc arm64::fulltag-symbol))))

(define-arm64-vinsn %symvector->symptr (((dest :lisp))
                                        ((src :lisp)))
  (sub dest src (:$ (- arm64::fulltag-misc arm64::fulltag-symbol))))

;;; %symbol->symptr -- x8664 canon (x8664-vinsns.lisp:4055): NIL maps to
;;; NILSYM, the real NRS symbol struct at rnil+t-offset+symbol.size
;;; (= rnil+92 = 0x13067; VERIFIED live in w4 -- pname there reads "NIL"),
;;; else trap unless symbol-tagged.  Matches the lap %symbol->symptr
;;; (level-0/ARM64/arm64-symbol.lisp:71).  Trap = the established
;;; brk #xf0xx type-error canon (w3b).
;;; 16m14 CORRECTION: the original w9 body passed NIL through raw (a
;;; "nilsym = nil itself" misread of his def_nrs nilsym slot).  NIL has
;;; fulltag-nil(0xb), so downstream %symptr->symvector's blind +5 made a
;;; bogus pseudo-misc pointer (0x13010) whose slot accesses land 4 bytes
;;; askew inside T's symbol struct -- boot-observed: set-type-predicate /
;;; %symbol-bits on NIL shredded T.pname, killing the first hash probe
;;; that walked over T (16m13 sysutils $XWRONGTYPE fatal).
(define-arm64-vinsn %symbol->symptr (((dest :lisp))
                                     ((src :lisp))
                                     ((tag :u64)))
  (add tag rnil (:$ (+ arm64::t-offset arm64::symbol.size)))
  (cmp src rnil)
  (csel dest tag src (:? eq))
  (b.eq :ok)
  (and tag src (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-symbol))
  (b.eq :ok)
  (uuo-error-reg-not-xtype src (:$ arm64::subtag-symbol))
  :ok)

;;; ============ %debug-trap ============
;;; PPC64 ppc64-vinsns.lisp (twlle) / v2:5961 (brk #x802): native trap,
;;; NO .SPbreakpoint.  Immediate #x802 carried from v2; RATIFY/mail --
;;; Matt's new uuo canon (arm64-uuo.s 2e10ffb) may want a udf encoding
;;; for this instead (handoff msg-27 list).
(define-arm64-vinsn %debug-trap (()
                                 ())
  (uuo-debug-trap))

;;; ============ zero-double-float-register ============
;;; PPC `fmr Dd, fp-zero` -> FMOV Dd, XZR (v2 @5251's deviation, carried;
;;; his fmov D<-X template arm64-asm.lisp:1131).  ARMv8 has no
;;; fmov-imm-zero; XZR->FPR is the canonical idiom.
(define-arm64-vinsn zero-double-float-register (((dest :double-float))
                                                ())
  (fmov dest xzr))

;;; Sibling (parity): S variant via WZR (his S<-W template
;;; arm64-asm.lisp:1129).
(define-arm64-vinsn zero-single-float-register (((dest :single-float))
                                                ())
  (fmov dest wzr))

;;; ============ nth-value ============
;;; PPC64 ppc64-vinsns.lisp (nth-value :call :subprim-call) bla
;;; .SPnthvalue; w4 subprim-caller canon (movz offset / ldr through
;;; rnil / blr).  dest = arg_z at the emit site (arm642-nth-value).
;;; Spentry draft: upstream-port/lisp-kernel/spentry-D-call-builtins.s:317.
(define-arm64-vinsn (nth-value :call :subprim) (((dest :lisp))
                                                ()
                                                ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPnthvalue")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ single-float-negate / double-float-negate ============
;;; PPC64 ppc64-vinsns.lisp:4220 (single) and :4218 (double), both `fneg
;;; dest src'.  ARM32 arm-vinsns.lisp:1794 is the same instruction.
;;;
;;; 16m45: these were the vinsns behind acode-handler-coverage.py's
;;; `*** %single-float-negate  ppc64,arm32,x8664' -- an ALL-THREE gap, i.e.
;;; every reference backend handles the operator and arm642 did not, so a
;;; source form that reached it was a hard "Compiler bug or inconsistency"
;;; rather than a slow path.  It cost ANSI ROUND/FROUND.13/15/17/19: those
;;; tests assert (eql r (- rrad)), and `(- rrad)' on a known single-float
;;; lowers straight to %single-float-negate, so the deftest never COMPILED.
;;; (The even-numbered siblings pass because they compare against rrad
;;; itself, with no negation -- which is the odd/even split in the failure
;;; list, and it is about the assertion, not about negative arithmetic.)
;;;
;;; ARM64-DEVIATION from the PPC body: PPC's %single-float-negate handler
;;; emits the DOUBLE vinsn for both precisions, because a PPC FPR is always
;;; 64-bit and `fneg' just flips the sign bit.  AArch64 encodes fneg
;;; separately per precision -- (:rd :s)(:rn :s) #x1e214000 vs
;;; (:rd :d)(:rn :d) #x1e614000, his arm64-asm.lisp:1038-1039 -- and the
;;; vinsn operand classes differ with it, so each precision needs its own.
(define-arm64-vinsn single-float-negate (((dest :single-float))
                                        ((src :single-float)))
  (fneg dest src))

(define-arm64-vinsn double-float-negate (((dest :double-float))
                                        ((src :double-float)))
  (fneg dest src))

;;; ============ mem-set-c-* (constant displacement) ============
;;; PPC64: std/stw/sth/stb val (:$ index) src.  Operand classes carried
;;; from PPC64/v2 verbatim, including the mem-set-c-byte val :u16
;;; oddity (stb stores the low 8 bits; sturb likewise).
(define-arm64-vinsn mem-set-c-doubleword (()
                                          ((val :u64)
                                           (src :address)
                                           (index :s16const)))
  (stur (:x val) (:@ src (:$ index))))

(define-arm64-vinsn mem-set-c-address (()
                                       ((val :address)
                                        (src :address)
                                        (index :s16const)))
  (stur (:x val) (:@ src (:$ index))))

(define-arm64-vinsn mem-set-c-fullword (()
                                        ((val :u32)
                                         (src :address)
                                         (index :s16const)))
  (stur (:w val) (:@ src (:$ index))))

(define-arm64-vinsn mem-set-c-halfword (()
                                        ((val :u16)
                                         (src :address)
                                         (index :s16const)))
  (sturh (:w val) (:@ src (:$ index))))

(define-arm64-vinsn mem-set-c-byte (()
                                    ((val :u16)
                                     (src :address)
                                     (index :s16const)))
  ;; val :u16 preserved from PPC64 (stb oddity); sturb stores low 8.
  (sturb (:w val) (:@ src (:$ index))))

;;; ============ mem-set-* (register byte offset) ============
;;; PPC64: stdx/stwx/sthx/stbx val src index.  A64 register-offset
;;; forms, no extend/shift (index is an unscaled byte offset) -- the w9
;;; (ldr dest (:@ fn idxreg)) addressing precedent.
(define-arm64-vinsn mem-set-doubleword (()
                                        ((val :u64)
                                         (src :address)
                                         (index :s64)))
  (str (:x val) (:@ src (:x index))))

(define-arm64-vinsn mem-set-address (()
                                     ((val :address)
                                      (src :address)
                                      (index :s64)))
  (str (:x val) (:@ src (:x index))))

(define-arm64-vinsn mem-set-fullword (()
                                      ((val :u32)
                                       (src :address)
                                       (index :s32)))
  (str (:w val) (:@ src (:x index))))

(define-arm64-vinsn mem-set-halfword (()
                                      ((val :u16)
                                       (src :address)
                                       (index :s32)))
  (strh (:w val) (:@ src (:x index))))

(define-arm64-vinsn mem-set-byte (()
                                  ((val :u8)
                                   (src :address)
                                   (index :s32)))
  (strb (:w val) (:@ src (:x index))))

;;; ============ gets64 / getu64 ============
;;; PPC64 define-ppc64-subprim-call-vinsn (gets64/getu64) .SPgets64/
;;; .SPgetu64 (ppc64-vinsns.lisp subprim block).  Contract: arg_z
;;; (an integer) -> unboxed 64-bit in imm0, type-error UUO on range
;;; failure (spentry-A-alloc-numbers.s:230/:265, registered in
;;; arm64-subprims-additions.lisp).  Body = w4/w9 subprim-caller canon
;;; (integer-sign shape): nil-relative table load, blr.  temp wired to
;;; imm1 because the result lands in imm0.
(define-arm64-vinsn (getu64 :call :subprim) (()
                                             ()
                                             ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPgetu64")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (gets64 :call :subprim) (()
                                             ()
                                             ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPgets64")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ mem-ref-c-* / mem-ref-* (batch 3) ============
;;; The load twins of the mem-set family above, demanded by
;;; arm642-immediate-get-xxx (realgate-w10a l0-utils wall).  PPC64
;;; ld/lwz/lhz/lbz + lwa/lha/(lbz+extsb) and the x-form register-offset
;;; variants.  Same STUR-window story: c-forms are LDUR-class (unscaled
;;; simm9, alignment-free), register forms take an unscaled byte-offset
;;; X reg.  Sign-extending loads land in X regs (ldursb/ldursh/ldursw
;;; X-form templates); unsigned land in W regs (zero-extend).
(define-arm64-vinsn mem-ref-c-doubleword (((dest :u64))
                                          ((src :address)
                                           (index :s16const)))
  (ldur (:x dest) (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-doubleword (((dest :u64))
                                        ((src :address)
                                         (index :s64)))
  (ldr (:x dest) (:@ src (:x index))))

(define-arm64-vinsn mem-ref-c-signed-doubleword (((dest :s64))
                                                 ((src :address)
                                                  (index :s16const)))
  (ldur (:x dest) (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-signed-doubleword (((dest :s64))
                                               ((src :address)
                                                (index :s64)))
  (ldr (:x dest) (:@ src (:x index))))

(define-arm64-vinsn mem-ref-c-fullword (((dest :u32))
                                        ((src :address)
                                         (index :s16const)))
  (ldur (:w dest) (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-fullword (((dest :u32))
                                      ((src :address)
                                       (index :s64)))
  (ldr (:w dest) (:@ src (:x index))))

(define-arm64-vinsn mem-ref-c-signed-fullword (((dest :s64))
                                               ((src :address)
                                                (index :s16const)))
  (ldursw (:x dest) (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-signed-fullword (((dest :s64))
                                             ((src :address)
                                              (index :s64)))
  (ldrsw (:x dest) (:@ src (:x index))))

(define-arm64-vinsn mem-ref-c-u16 (((dest :u16))
                                   ((src :address)
                                    (index :s16const)))
  (ldurh (:w dest) (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-u16 (((dest :u16))
                                 ((src :address)
                                  (index :s64)))
  (ldrh (:w dest) (:@ src (:x index))))

(define-arm64-vinsn mem-ref-c-s16 (((dest :s16))
                                   ((src :address)
                                    (index :s16const)))
  (ldursh (:x dest) (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-s16 (((dest :s16))
                                 ((src :address)
                                  (index :s64)))
  (ldrsh (:x dest) (:@ src (:x index))))

(define-arm64-vinsn mem-ref-c-u8 (((dest :u8))
                                  ((src :address)
                                   (index :s16const)))
  (ldurb (:w dest) (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-u8 (((dest :u8))
                                ((src :address)
                                 (index :s64)))
  (ldrb (:w dest) (:@ src (:x index))))

(define-arm64-vinsn mem-ref-c-s8 (((dest :s8))
                                  ((src :address)
                                   (index :s16const)))
  (ldursb (:x dest) (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-s8 (((dest :s8))
                                ((src :address)
                                 (index :s64)))
  (ldrsb (:x dest) (:@ src (:x index))))

;;; ============ %current-tcr ============
;;; PPC64: (mr dest rcontext); v2 arm64-vinsns.lisp:5222 identical.
;;; l0-def wall (realgate-w10a).
(define-arm64-vinsn %current-tcr (((dest :imm))
                                  ())
  (mov dest rcontext))

;;; ============ ref-symbol-value-inline / %ref-symbol-value-inline ====
;;; PPC64 ppc64-vinsns.lisp:3104/:3126.  Emit site = the w2
;;; arm642-ref-symbol-value open-code path; the l0-int wall
;;; (realgate-w10a, %-variant demanded; checked twin ported as its
;;; sibling).  fixnumshift=3 on his tree => the PPC64 pun holds: the
;;; fixnum binding-index IS the TLB byte offset -- no scaling (v2's
;;; word-shift scaling was its fixnumshift=0 deviation, NOT carried).
;;; PPC bge -> b.hs (indices unsigned; ARM32 :3318 uses hs).  The
;;; TLB-slot marker compare is against the small immediate
;;; subtag-no-thread-local-binding (his arch @185), per ARM32 :3322
;;; (low-tag marker = the subtag byte, not a shifted top-byte marker).
;;; Checked variant traps unbound via the w4 brk #xf0ff placeholder
;;; (uuo canon RATIFY pending -- same sweep as trap-unless-*).
(define-arm64-vinsn ref-symbol-value-inline (((dest :lisp))
                                             ((src (:lisp (:ne dest))))
                                             ((table :imm)
                                              (idx :imm)))
  (ldur idx (:@ src (:$ arm64::symbol.binding-index)))
  (ldr table (:@ rcontext (:$ arm64::tcr.tlb-limit)))
  (cmp idx table)
  (ldr table (:@ rcontext (:$ arm64::tcr.tlb-pointer)))
  (b.hs :symbol)
  (ldr dest (:@ table idx))
  (cmp dest (:$ arm64::subtag-no-thread-local-binding))
  (b.ne :done)
  :symbol
  (ldur dest (:@ src (:$ arm64::symbol.vcell)))
  :done
  (cmp dest (:$ arm64::unbound-marker))
  (b.ne :bound)
  (uuo-error-unbound src)
  :bound)

(define-arm64-vinsn %ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((table :imm)
                                               (idx :imm)))
  (ldur idx (:@ src (:$ arm64::symbol.binding-index)))
  (ldr table (:@ rcontext (:$ arm64::tcr.tlb-limit)))
  (cmp idx table)
  (ldr table (:@ rcontext (:$ arm64::tcr.tlb-pointer)))
  (b.hs :symbol)
  (ldr dest (:@ table idx))
  (cmp dest (:$ arm64::subtag-no-thread-local-binding))
  (b.ne :done)
  :symbol
  (ldur dest (:@ src (:$ arm64::symbol.vcell)))
  :done)

;;; ============ %ref-symbol-value ============
;;; PPC64 :3121 (bla .SPspecref) -- the ool twin of ref-symbol-value
;;; (.SPspecrefcheck, w4:758); same w4 subprim-caller canon.
(define-arm64-vinsn (%ref-symbol-value :call :subprim)
    (((val :lisp))
     ((sym (:lisp (:ne val))))
     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPspecref")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ unbox-u8 / unbox-u16 ============
;;; PPC64 ppc64-vinsns.lisp:1425/:1447 -- the CHECKED unboxers (w1's
;;; %unbox-u8 is the unchecked twin; unbox-u32 @w1:51 is the exact
;;; trap+extract canon: valid iff every bit outside (mask<<fixnumshift)
;;; is clear).  l0-io wall (realgate-w10f demanded unbox-u8).  Both
;;; lognot masks are wraparound one-runs -- encodable logical imms.
(define-arm64-vinsn unbox-u8 (((dest :u8))
                              ((src :lisp)))
  :again
  (tst src (:$ (logand #xffffffffffffffff
                       (lognot (ash #xff arm64::fixnumshift)))))
  (b.eq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm64::xtype-u8))
  (b :again)
  :got-it
  (ubfm (:x dest) src (:$ arm64::fixnumshift)
        (:$ (+ arm64::fixnumshift 7))))

(define-arm64-vinsn unbox-u16 (((dest :u16))
                               ((src :lisp)))
  :again
  (tst src (:$ (logand #xffffffffffffffff
                       (lognot (ash #xffff arm64::fixnumshift)))))
  (b.eq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm64::xtype-u16))
  (b :again)
  :got-it
  (ubfm (:x dest) src (:$ arm64::fixnumshift)
        (:$ (+ arm64::fixnumshift 15))))

;;; ============ u8->fixnum / u16->fixnum ============
;;; PPC64 ppc64-vinsns.lisp u8->fixnum/u16->fixnum (clrlsldi = the w8
;;; u32->fixnum UBFM idiom, narrower widths): Xd = ZeroExtend(val[0:N-1])
;;; << fixnumshift.  l0-io wall (realgate-w10d demanded u8->fixnum;
;;; u16 sibling ported with it -- w8's u32 note already named the pair).
(define-arm64-vinsn u8->fixnum (((result :imm))
                                ((val :u8)))
  (ubfm (:x result) (:x val)
        (:$ (- arm64::nbits-in-word arm64::fixnumshift))
        (:$ (1- 8))))

(define-arm64-vinsn u16->fixnum (((result :imm))
                                 ((val :u16)))
  (ubfm (:x result) (:x val)
        (:$ (- arm64::nbits-in-word arm64::fixnumshift))
        (:$ (1- 16))))

;;; ============ check-max-nargs-large ============
;;; ARM32 arm-vinsns.lisp:3594 (materialize the boxed max in a temp,
;;; compare) -- emitted-but-undefined at HIS arm642.lisp:1831 and our
;;; w4:586 whenever arm642-aimm-p rejects the boxed max (max > 511).
;;; boxed max = max<<3 <= 8191<<3 fits movz's imm16.
(define-arm64-vinsn check-max-nargs-large (()
                                           ((max :u16const))
                                           ((temp :u64)))
  (movz temp (:$ (:apply ash max 3)))
  (cmp nargs temp)
  (b.ls :ok)
  (uuo-error-too-many-args)
  :ok)

;;; ============ check-min-nargs-large ============
;;; ARM32 arm-vinsns.lisp:3573 shape via the check-max-nargs-large idiom
;;; above -- emitted at HIS arm642.lisp:5583 and our w4 lambda-bind
;;; chooser whenever the BOXED min (min<<3) exceeds imm12 (min > 511;
;;; 16m41 FLET.20 probe: threshold exactly 512).  Same movz constraint:
;;; boxed min <= 8191<<3 fits imm16, and lambda-parameters-limit (4096)
;;; gates the rest.
(define-arm64-vinsn check-min-nargs-large (()
                                           ((min :u16const))
                                           ((temp :u64)))
  (movz temp (:$ (:apply ash min 3)))
  (cmp nargs temp)
  (b.hs :ok)
  (uuo-error-too-few-args)
  :ok)

;;; ============ check-exact-nargs-large ============
;;; ARM32 arm-vinsns.lisp:3557; the -large twin of HIS check-exact-nargs
;;; (arm64-vinsns.lisp:18, "this range is limited").  16m41 FLET.20:
;;; arm642-req-nargs-entry emitted the small form unguarded, so a
;;; 512-required-arg function died at compile time with "vinsn immediate
;;; 4096 (shift 0) out of range for operand class :AIMM"; patch 0051
;;; restores arm2's small/large split (arm2.lisp:1149-1151).
(define-arm64-vinsn check-exact-nargs-large (()
                                             ((n :u16const))
                                             ((temp :u64)))
  (movz temp (:$ (:apply ash n 3)))
  (cmp nargs temp)
  (b.eq :ok)
  ;; 16m48h: uuo-error-wrong-number-of-args, NOT uuo-error-wrong-nargs.
  ;; Both names exist at pin 9c61574 and patch 0012 maps them to the same
  ;; encoding, but 690dc53a DELETES uuo-error-wrong-nargs upstream, so this
  ;; emit site would fail to assemble the moment the pin advances.  The
  ;; surviving name is correct on its own terms: this is the exact-nargs
  ;; check, and a mismatch is neither too-few nor too-many specifically.
  (uuo-error-wrong-number-of-args)
  :ok)

;;; ============ interrupt-level inline binders (batch 4) ============
;;; PPC64 ppc64-vinsns.lisp:4118 (bind-0) /:4144 (bind-m1) /:4167
;;; (unbind); the *arm642-open-code-inline* twins of the w4 subprim
;;; callers (bind-interrupt-level-0/-m1/unbind, w4:283-306).  l0-hash
;;; wall (realgate-w10c demanded bind-m1-inline; siblings ported with
;;; it).  fixnumshift=3: boxed -1 = -8 = movn #7 (movn Rd,#v = ~v);
;;; interrupt-level-binding-index (his arch @672 = (ash 1 fixnumshift)
;;; = 8) doubles as the raw TLB byte offset -- the PPC64 pun, same as
;;; ref-symbol-value-inline above.  Binding frame pushed on vsp:
;;; value@16/idx@8/link@0 (PPC stdu order).  PPC's conditional trap
;;; tdgti nargs,0 (pending-interrupt poll) -> cmp + b.le + brk #xf0ff
;;; (w4 placeholder canon; uuo RATIFY sweep pending).  nargs is
;;; save/restored around the poll exactly as PPC (mr save-nargs).
(define-arm64-vinsn bind-interrupt-level-0-inline (()
                                                   ()
                                                   ((tlb :imm)
                                                    (value :imm)
                                                    (link :imm)
                                                    (temp :imm)
                                                    (save-nargs :u64)))
  (ldr tlb (:@ rcontext (:$ arm64::tcr.tlb-pointer)))
  (ldr value (:@ tlb (:$ arm64::interrupt-level-binding-index)))
  (ldr link (:@ rcontext (:$ arm64::tcr.db-link)))
  (cmp value (:$ 0))
  (mov temp (:$ arm64::interrupt-level-binding-index))
  (str value (:@! vsp (:$ (- arm64::node-size))))
  (str temp (:@! vsp (:$ (- arm64::node-size))))
  (str link (:@! vsp (:$ (- arm64::node-size))))
  (str xzr (:@ tlb (:$ arm64::interrupt-level-binding-index)))
  (str vsp (:@ rcontext (:$ arm64::tcr.db-link)))
  (b.eq :done)
  (mov save-nargs nargs)
  (mov nargs value)
  (b.gt :do-trap)
  (ldr nargs (:@ rcontext (:$ arm64::tcr.interrupt-pending)))
  :do-trap
  (cmp nargs (:$ 0))
  (b.le :restore)
  (uuo-interrupt-now)
  :restore
  (mov nargs save-nargs)
  :done)

(define-arm64-vinsn bind-interrupt-level-m1-inline (()
                                                    ()
                                                    ((tlb :imm)
                                                     (oldvalue :imm)
                                                     (link :imm)
                                                     (newvalue :imm)
                                                     (idx :imm)))
  ;; Bind *interrupt-level* to -1 (disable); no pending-interrupt check
  ;; (disabling can't make one runnable) -- donor comment carried.
  (movn newvalue (:$ 7))
  (mov idx (:$ arm64::interrupt-level-binding-index))
  (ldr tlb (:@ rcontext (:$ arm64::tcr.tlb-pointer)))
  (ldr oldvalue (:@ tlb (:$ arm64::interrupt-level-binding-index)))
  (ldr link (:@ rcontext (:$ arm64::tcr.db-link)))
  (str oldvalue (:@! vsp (:$ (- arm64::node-size))))
  (str idx (:@! vsp (:$ (- arm64::node-size))))
  (str link (:@! vsp (:$ (- arm64::node-size))))
  (str newvalue (:@ tlb (:$ arm64::interrupt-level-binding-index)))
  (str vsp (:@ rcontext (:$ arm64::tcr.db-link))))

;;; ============ catch/throw/unwind-protect cluster (batch 5) ============
;;; The Class-G codegen family -- vinsn side.  PPC64
;;; define-ppc64-subprim-call-vinsn (mkcatch1v/mkcatchmv/mkunwind/
;;; nmkunwind/throw/nthrowvalues/nthrow1value/progvsave) + the
;;; progvrestore subprim-JUMP + save/restore-cleanup-context +
;;; jump-return-pc + non-barrier-jump (ppc64-vinsns.lisp:2130-2200 and
;;; :3580-3610 region).  Kernel bodies: our spentry-C-bind-catch-throw.s.
;;;
;;; CONTRACT (spentry-C mkcatch macro @289): mkcatch1v/mkcatchmv/
;;; mkunwind/nmkunwind decode the single forward B at [lr] as the
;;; cleanup/exit PC and return to lr+4.  So each caller MUST be
;;; IMMEDIATELY followed by (! non-barrier-jump L) -- one B instruction
;;; -- exactly as the ppc2/arm642 handlers emit them.
;;; ARM64-DEVIATION (host): PPC64's :xref attribute doesn't exist in
;;; the 1.12.2 host's *known-vinsn-attributes* (encode errors on
;;; unknowns -- this killed the w10-batch-5 load).  Attribute-FREE is
;;; the correct stock-host spelling: label liveness is tracked through
;;; the :label OPERAND (vinsn-label-refs), and the point of the donor
;;; comment is only that it must NOT be :jump (dead-code analysis).
(define-arm64-vinsn non-barrier-jump (()
                                      ((label :label)))
  (b label))

(define-arm64-vinsn (mkcatch1v :call :subprim) (()
                                                ()
                                                ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmkcatch1v")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (mkcatchmv :call :subprim) (()
                                                ()
                                                ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmkcatchmv")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (mkunwind :call :subprim) (()
                                               ()
                                               ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPmkunwind")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (nmkunwind :call :subprim) (()
                                                ()
                                                ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPnmkunwind")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (throw :call :subprim) (()
                                            ()
                                            ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPthrow")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (nthrowvalues :call :subprim) (()
                                                   ()
                                                   ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPnthrowvalues")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (nthrow1value :call :subprim) (()
                                                   ()
                                                   ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPnthrow1value")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (progvsave :call :subprim) (()
                                                ()
                                                ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPprogvsave")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; progvrestore is a subprim JUMP (PPC define-ppc64-subprim-jump-vinsn):
;;; the progv cleanup tail-jumps into the subprim, which returns to the
;;; unwinder -- br, not blr.
;;; !! ATTRIBUTE MUST BE :jumpLR, NOT :jump (s92 w11 LAP-label ROOT):
;;; the PPC macro (ppc64-vinsns.lisp:3931) expands subprim-jump-vinsns
;;; to :jumpLR.  :jump tells the host optimizer "unconditional branch
;;; with a LABEL at vp[0]" -- maximize-jumps then took vp[0] (the imm1
;;; TEMP, register number 1) as a branch TARGET, forwarded the preceding
;;; label's refs onto the fixnum 1, and rewrote non-barrier-jump's vp to
;;; the fixnum => "LAP label 1 was referenced but not defined" in every
;;; progv-using level-1 file (5 files).  :jumpLR = jumps via register,
;;; no label operand -- exempt from that transformation.
(define-arm64-vinsn (progvrestore :jumpLR) (()
                                            ()
                                            ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPprogvrestore")))
  (ldr temp (:@ rcontext temp))
  (br temp))

;;; ============ mv save/recover + stack rest-arg trio ============
;;; PPC64 define-ppc64-subprim-call-vinsn (save-values/recover-values/
;;; add-values) and (stack-rest-arg/req-stack-rest-arg/
;;; stack-cons-rest-arg); w4 caller canon.  Emit sites: the w2 mv
;;; handlers (multiple-value-prog1/mv-combination) and HIS lambda
;;; rest-arg path.  Kernel bodies spentry-C:1554/1595/1629/1167/1187/
;;; 1205.  Demanded by l0-cfm-support (save-values) and l0-hash
;;; (req-stack-rest-arg) at realgate-w10i.
(define-arm64-vinsn (save-values :call :subprim) (()
                                                  ()
                                                  ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPsave-values")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (recover-values :call :subprim) (()
                                                     ()
                                                     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPrecover-values")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (add-values :call :subprim) (()
                                                 ()
                                                 ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPadd-values")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (stack-rest-arg :call :subprim) (()
                                                     ()
                                                     ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPstack-rest-arg")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (req-stack-rest-arg :call :subprim) (()
                                                         ()
                                                         ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPreq-stack-rest-arg")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (stack-cons-rest-arg :call :subprim) (()
                                                          ()
                                                          ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPstack-cons-rest-arg")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; save-cleanup-context -- PPC64 ppc64-vinsns.lisp:3585: mflr loc-pc;
;;; stdu sp; std rzero savefn; std loc-pc savelr; std vsp savevsp (no
;;; stack probe -- the cleanup site is reached from return-like flow
;;; with frame headroom, donor comment).  This lane's frame = Matt's
;;; MARKER frame {marker@0, savevsp@8, savefn@16, savelr@24}, built the
;;; way his save-lisp-context canon builds it (w4:778 idiom).
;;; savefn = 0 (xzr), as PPC stores rzero -- a cleanup frame owns no fn.
;;; Sign RESOLVED (msg-26; his canon carries -32): stack-down.  The old
;;; +32 mirror here was THE 16m5i toplevel-vsp wall: nthrow1value calls
;;; the cleanup with sp = the UP lisp_frame holding its stashed fn/lr;
;;; a +32 build FREED that frame, the cleanup's callee rebuilt its own
;;; frame over it, and nthrow resumed with the toplevel catch frame's
;;; savelr/savefn (gdb stepi-trace 2026-07-17, boot-validated).
(define-arm64-vinsn save-cleanup-context (()
                                          ()
                                          ((marker-reg :imm)))
  (mov marker-reg (:$ arm64::lisp-frame-marker))
  (stp marker-reg vsp (:@! sp (:$ -32)))
  (stp xzr lr (:@ sp (:$ 16))))

;;; restore-cleanup-context -- PPC64 :3603: ld loc-pc savelr; mtlr;
;;; la sp size(sp).  vsp is NOT restored (cleanup body preserved it).
(define-arm64-vinsn restore-cleanup-context (()
                                             ())
  (ldr lr (:@ sp (:$ 24)))
  (add sp sp (:$ 32)))

;;; jump-return-pc -- PPC blr; the cleanup body returns to the unwinder
;;; (or to the normal-exit resume) through LR.
(define-arm64-vinsn (jump-return-pc :jumpLR) (()
                                              ())
  (ret))

(define-arm64-vinsn unbind-interrupt-level-inline (()
                                                   ()
                                                   ((tlb :imm)
                                                    (link :imm)
                                                    (curval :imm)
                                                    (oldval :imm)
                                                    (save-nargs :u64)))
  ;; Pop the *interrupt-level* binding; if we just RE-ENABLED interrupts
  ;; (curval<0 restored to oldval>=0) and one is pending, trap.
  ;; ARM64-DEVIATION (v2's, carried): PPC keeps two compare results in
  ;; crf0/crf1 across the restore; ARM64 has one NZCV, so curval/oldval
  ;; live in registers and each is compared immediately before its
  ;; branch.
  (ldr tlb (:@ rcontext (:$ arm64::tcr.tlb-pointer)))
  (ldr curval (:@ tlb (:$ arm64::interrupt-level-binding-index)))
  (ldr link (:@ rcontext (:$ arm64::tcr.db-link)))
  (ldr oldval (:@ link (:$ 16)))
  (ldr link (:@ link (:$ 0)))
  (str oldval (:@ tlb (:$ arm64::interrupt-level-binding-index)))
  (str link (:@ rcontext (:$ arm64::tcr.db-link)))
  (cmp curval (:$ 0))
  (b.ge :done)
  (cmp oldval (:$ 0))
  (b.lt :done)
  (mov save-nargs nargs)
  (ldr nargs (:@ rcontext (:$ arm64::tcr.interrupt-pending)))
  (cmp nargs (:$ 0))
  (b.le :restore)
  (uuo-interrupt-now)
  :restore
  (mov nargs save-nargs)
  :done)

;;; Requires .SPbind registered in arm64-subprims-additions.lisp (w11) --
;;; subprimitive-offset returns NIL for unregistered names.
(define-arm64-vinsn (bind :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPbind")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; dpayback -- unwind N special-variable bindings on exit from a binding
;;; contour.  PPC64 LINE-PORT (ppc64-vinsns.lisp:3406):
;;;   (define-ppc64-vinsn (dpayback :call :subprim)
;;;       (() ((n :s16const)) ((temp (:u32 #.ppc::imm0))))
;;;     ((:pred > n 1) (li temp n) (bla .SPunbind-n))
;;;     ((:pred = n 1) (bla .SPunbind)))
;;; n>1: RAW (unboxed, unshifted) count in imm0 for .SPunbind-n, exactly
;;; PPC's `li temp n` -- positive-count movz per the ratified :s16const
;;; materialization idiom (w2 multiply-immediate:400; n is a binding
;;; count, always >0, so no movn arm and no aimm-window concern: movz's
;;; 16-bit field = li's window).  n=1: .SPunbind takes no count.  The
;;; call scratch is the lane-standard imm1 (w4 bind-interrupt trio);
;;; the count lives in imm0 per PPC, declared as a separate wired temp.
(define-arm64-vinsn (dpayback :call :subprim)
    (()
     ((n :s16const))
     ((countreg (:u64 #.arm64::imm0))
      (temp (:u64 #.arm64::imm1))))
  ((:pred > n 1)
   (movz countreg (:$ n))
   (movz temp (:$ (:apply arm64::subprimitive-offset ".SPunbind-n")))
   (ldr temp (:@ rcontext temp))
   (blr temp))
  ((:pred = n 1)
   (movz temp (:$ (:apply arm64::subprimitive-offset ".SPunbind")))
   (ldr temp (:@ rcontext temp))
   (blr temp)))

;;; ============ unbox-s32 ============
;;; Checking unbox: trap unless src is a fixnum in (signed-byte 32),
;;; else extract the raw value.  PPC64 LINE-PORT (ppc64-vinsns.lisp:1385):
;;;   (sldi dest src (- nbits-in-word (+ 32 fixnumshift)))  ; isolate
;;;   (sradi dest dest (- nbits-in-word 32))                ; sign-ext 32
;;;   (sldi dest dest fixnumshift)                          ; re-shift
;;;   (cmpd crf dest src)                                   ; == original?
;;;   (sradi dest src fixnumshift)
;;;   (beq crf :got-it) :bad (uuo_interr error-object-not-signed-byte-32
;;;   src) :got-it
;;; The check is the "reconstruct and compare" reconstruction -- VERBATIM
;;; the check body of our gate-proven require-s32 (w3b:480, same donor
;;; family ppc64:1195), done in a :s64 temp so src stays live; the brk
;;; #xf0xx trap idiom + b :again resume loop is the same as require-s32/
;;; unbox-u32 (w1:51).  Extract = SBFM (immr=lsb, imms=lsb+width-1 =
;;; sbfx fixnumshift,32 with sign-extension into the X view) -- the
;;; SIGNED twin of %unbox-u32's UBFM (w1:39; sbfx/ubfx are LAPMACROS in
;;; his tree, not templates, so the underlying SBFM is written out;
;;; "sbfm" confirmed present in the assembler's :bitfield table beside
;;; "ubfm").  dest is :s32 (W class) but the extract reads src bits
;;; above 31: force the X view, exactly like %unbox-u32 / misc-ref-s32's
;;; (ldrsw (:x dest) ...) -- the X reg ends up holding the 64-bit
;;; sign-extension of the s32 value, as PPC's sradi leaves it.
;;; NOTE fixnumshift = 3 in this lane (Matt low-tag): v2's high-tag
;;; unbox-s32 (compiler/ARM64/arm64-vinsns.lisp:2324) was written for
;;; fixnumshift = 0 and skips the unshift -- NOT the donor here.
(define-arm64-vinsn unbox-s32 (((dest :s32))
                               ((src :lisp))
                               ((tag :s64)))
  :again
  (lsl tag src (:$ (- arm64::nbits-in-word (+ 32 arm64::fixnumshift))))
  (asr tag tag (:$ (- arm64::nbits-in-word 32)))
  (lsl tag tag (:$ arm64::fixnumshift))
  (cmp tag src)
  (b.eq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm64::xtype-s32))
  (b :again)
  :got-it
  (sbfm (:x dest) src (:$ arm64::fixnumshift)
        (:$ (+ arm64::fixnumshift 31))))

;;; ============ w11 handler-wave vinsns (level-1 broad map) ============

;;; sign-extend-halfword -- demanded by the %word-to-int handler
;;; (arm642-additions-w11.lisp).  PPC64 LINE-PORT (ppc64-vinsns.lisp:2993):
;;;   (sldi dest src (- 48 fixnumshift))
;;;   (sradi dest dest (- 48 fixnumshift))
;;; Sign-extend the low 16 bits of a BOXED halfword value, preserving
;;; the fixnum boxing: shift the boxed sign bit (15 + fixnumshift) to
;;; bit 63, arithmetic-shift back.  dest==src alias-safe (src read only
;;; by the first instruction, before any write).
(define-arm64-vinsn sign-extend-halfword (((dest :imm))
                                          ((src :imm)))
  (lsl dest src (:$ (- 48 arm64::fixnumshift)))
  (asr dest dest (:$ (- 48 arm64::fixnumshift))))

;;; %current-frame-ptr -- demanded by the %current-frame-ptr handler.
;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:3398): (mr dest ppc::sp) -- the
;;; control-stack pointer AS a node value (frames are 16-aligned, so
;;; under fixnumshift=3 the raw SP is a valid boxed fixnum, same pun as
;;; PPC).  A64: reg 31 in an add-immediate Rn slot IS SP (the alias of
;;; `mov Xd, SP`); bare `sp` is this lane's gate-proven operand spelling
;;; for it (w4:443 / w10:558 build-lisp-frame bases).  May DUPLICATE a
;;; definition in Matt's arm64-vinsns.lisp (unverifiable locally) --
;;; by-name redefinition is benign; drop this one if his tree has it.
(define-arm64-vinsn %current-frame-ptr (((dest :imm))
                                        ())
  (add dest sp (:$ 0)))

;;; mem-ref-c-address / mem-ref-address -- demanded by the
;;; immediate-get-ptr handler; the load twins of w10's mem-set-c-address
;;; and the :address members of the w10 mem-ref-c-*/mem-ref-* family
;;; (PPC64 mem-ref-c-natural/mem-ref-natural: ld (:$ index) src / ldx).
;;; Same STUR-window story as w10: c-form = LDUR-class unscaled simm9;
;;; register form takes an unscaled byte-offset X reg.
(define-arm64-vinsn mem-ref-c-address (((dest :address))
                                       ((src :address)
                                        (index :s16const)))
  (ldur (:x dest) (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-address (((dest :address))
                                     ((src :address)
                                      (index :s64)))
  (ldr (:x dest) (:@ src (:x index))))

;;; ============ w11 vinsn tail (level-1 broad map, final 4+1) ============

;;; unbox-u64 -- PPC64 LINE-PORT (ppc64-vinsns.lisp:1306): value = a
;;; non-negative fixnum, a non-negative two-digit bignum, or a
;;; three-digit bignum whose top digit is zero; else
;;; error-object-not-unsigned-byte-64.  The CHECK skeleton is verbatim
;;; our gate-proven require-u64 (w3b:535, same donor family ppc64:1242)
;;; -- same fixnummask TST, fulltag-misc test, header LDUR at
;;; misc-header-offset (-12 post-8b1ed24), two/three-digit-bignum-header
;;; compares (all constants resolve in his arch via w3b) -- with the
;;; value produced in dest along the way.  ARM64-DEVIATION (carried
;;; from require-u64 / x8664:3596): PPC's (rotldi dest 32) digit-swizzle
;;; is BIG-ENDIAN-only; on LE the two 32-bit digits at misc-data-offset
;;; (-4) read as ONE little-endian 64-bit word ARE the u64 -- plain
;;; LDUR, no swizzle.  asr between TST and b.eq doesn't touch NZCV
;;; (require-u64's mov-slot note).  (:x dest) views per the w10
;;; mem-ref-c-doubleword / w1 ubfm precedents.  NOT v2's high-tag
;;; unbox-u64 (arm64-vinsns.lisp:2227) -- wrong layout for this tree.
(define-arm64-vinsn unbox-u64 (((dest :u64))
                               ((src :lisp))
                               ((tag :s64)))
  :again
  (tst src (:$ arm64::fixnummask))
  (asr (:x dest) src (:$ arm64::fixnumshift))
  (b.eq :ok-if-non-negative)
  (and tag src (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldur tag (:@ src (:$ arm64::misc-header-offset)))
  (cmp tag (:$ arm64::two-digit-bignum-header))
  (b.eq :two-digit)
  (cmp tag (:$ arm64::three-digit-bignum-header))
  (b.ne :bad)
  (ldur (:w tag) (:@ src (:$ (+ arm64::misc-data-offset 8))))
  (cmp (:w tag) (:$ 0))
  (b.ne :bad)
  (ldur (:x dest) (:@ src (:$ arm64::misc-data-offset)))
  (b :got-it)
  :two-digit
  (ldur (:x dest) (:@ src (:$ arm64::misc-data-offset)))
  :ok-if-non-negative
  (cmp (:x dest) (:$ 0))
  (b.ge :got-it)
  :bad
  (uuo-error-reg-not-xtype src (:$ arm64::xtype-u64))
  (b :again)
  :got-it)

;;; unbox-s64 -- PPC64 LINE-PORT (ppc64-vinsns.lisp:1339).  Demanded by
;;; level-1/l1-streams.lisp, which failed to cross-compile with "Unknown
;;; vinsn: CCL::UNBOX-S64" (16m33): his arm64-vinsns.lisp:339 has a draft
;;; but it sits inside a #| ... |# block, so nothing defines it, and the
;;; draft's trap is the aspirational (uuo-error-reg-not-type ...) whose
;;; template his assembler does not have yet (Uw3b-1).
;;;
;;; An object is (SIGNED-BYTE 64) iff it is a fixnum or a two-digit
;;; bignum.  CHECK skeleton = verbatim our gate-proven require-s64
;;; (w3b:510, same donor family ppc64:1223), with the value produced in
;;; dest along the way:
;;;   fixnum      -> dest = src >> fixnumshift        (ASR, signed)
;;;   two-digit   -> dest = the data word at misc-data-offset
;;; ARM64-DEVIATION (identical to unbox-u64 above, carried from
;;; x8664:3596): PPC's (rotldi dest dest 32) digit-swizzle is
;;; BIG-ENDIAN-only -- on LE the two 32-bit digits read as ONE
;;; little-endian 64-bit word ARE the value, so plain LDUR, no swizzle.
;;; ASR between the TST and the b.eq does not touch NZCV (the require-u64
;;; note).  dest is :s64 so (:x dest) views per the w1 UBFM precedent.
;;; Trap = this lane's ratified placeholder brk #xf0NN (w3b header) since
;;; his tree still has no type-error uuo mnemonic; retry via :again so a
;;; handler that fixes the value can resume, as require-s64 does.
(define-arm64-vinsn unbox-s64 (((dest :s64))
                               ((src :lisp))
                               ((tag :u64)))
  :again
  (tst src (:$ arm64::fixnummask))
  (asr (:x dest) src (:$ arm64::fixnumshift))
  (b.eq :got-it)
  (and tag src (:$ arm64::fulltagmask))
  (cmp tag (:$ arm64::fulltag-misc))
  (b.ne :bad)
  (ldur tag (:@ src (:$ arm64::misc-header-offset)))
  (cmp tag (:$ arm64::two-digit-bignum-header))
  (b.ne :bad)
  (ldur (:x dest) (:@ src (:$ arm64::misc-data-offset)))
  (b :got-it)
  :bad
  (uuo-error-reg-not-xtype src (:$ arm64::xtype-s64))
  (b :again)
  :got-it)

;;; unbox-s16 -- PPC64 LINE-PORT (ppc64-vinsns.lisp:1410): the width-16
;;; member of the unbox-s32 family above -- check body verbatim
;;; require-s16 (w3b:450, donor ppc64:1167): reconstruct-and-compare at
;;; width 16, extract = SBFM at fixnumshift width 16 (imms =
;;; fixnumshift+15; the signed twin of the w1 UBFM idiom, cf. unbox-s32
;;; above).  Error code arch::error-object-not-signed-byte-16 (verified:
;;; w3b:459 brk's it).
(define-arm64-vinsn unbox-s16 (((dest :s16))
                               ((src :lisp))
                               ((tag :s64)))
  :again
  (lsl tag src (:$ (- arm64::nbits-in-word (+ 16 arm64::fixnumshift))))
  (asr tag tag (:$ (- arm64::nbits-in-word 16)))
  (lsl tag tag (:$ arm64::fixnumshift))
  (cmp tag src)
  (b.eq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm64::xtype-s16))
  (b :again)
  :got-it
  (sbfm (:x dest) src (:$ arm64::fixnumshift)
        (:$ (+ arm64::fixnumshift 15))))

;;; scale-64bit-misc-index -- PPC64 LINE-PORT (ppc64-vinsns.lisp:62):
;;;   (addi dest idx misc-data-offset)
;;; NO shift: 64-bit elements scale by word-shift (3) from the fixnum
;;; index, and fixnumshift = word-shift = 3, so the BOXED fixnum IS the
;;; byte offset (the fixnumshift-vs-node-shift class,
;;; corrections_fixnumshift_vs_node_shift -- here the null shift is the
;;; CORRECT rendering, same reasoning as scale-node-misc-index w4:588,
;;; whose body this matches exactly).  misc-data-offset is negative
;;; (-4) => addi renders as SUB of its negation, the family idiom
;;; (w7:32 scale-32bit / w9:189 scale-8bit / w9:195 scale-16bit).
(define-arm64-vinsn scale-64bit-misc-index (((dest :u64))
                                            ((idx :imm)) ; a fixnum
                                            ())
  (sub dest idx (:$ (:apply - arm64::misc-data-offset))))

;;; scale-128bit-misc-index -- PPC64 LINE-PORT (ppc64-vinsns.lisp:68):
;;;   (add dest idx idx) / (addi dest dest complex-double-float.realpart)
;;; complex-double-float is the ONLY 16-byte element type, so unlike every
;;; other member of this family the null shift is NOT correct here: the
;;; boxed fixnum is index*8 (fixnumshift 3), and a 16-byte element needs
;;; index*16, hence PPC64's doubling, carried verbatim as (add dest idx
;;; idx).
;;;
;;; The displacement is PPC64's constant, unchanged, and it is NOT
;;; misc-data-offset: arm64 lays a PAD word after the uvector header so
;;; the elements are 16-byte aligned (arm64-arch.lisp:270-272), exactly as
;;; PPC64 does, and both spell the resulting +4 as
;;; complex-double-float.realpart -- slot 1 of (define-fixedsized-object
;;; complex-double-float () pad realpart imagpart), arm64-arch.lisp:615,
;;; ppc64-arch.lisp:365.  arm64 also defines it as
;;; misc-complex-dfloat-offset (= misc-data-offset + node-size, :272); the
;;; two are the same number and the -c- vinsns in w3a:321/608 already use
;;; the PPC64 name, so this matches them.  A positive displacement means
;;; addi renders as a plain ADD here, not the SUB-of-the-negation the rest
;;; of the family needs for the negative misc-data-offset.
;;;
;;; 16m48g: this is the vinsn arm64-vinsns-additions-w3a.lisp:445 already
;;; documented as the "scale-128bit contract" that misc-{ref,set}-complex-
;;; double-float's scaled-idx operand requires.  It was never written, and
;;; both is-128-bit arms of arm642-{vref1,vset1} passed the raw boxed
;;; fixnum instead, so every open-coded complex-double-float element access
;;; addressed index*8 with no displacement.  MEASURED before the fix
;;; (tools/probes/complex-double-float-lanes.lisp): compiled (aref a i) on
;;; #(#C(1d0 2d0) #C(11d0 12d0) #C(21d0 22d0) #C(31d0 32d0)) returned
;;; #C(0d0 5.299808824d-315), #C(5.299808824d-315 5.304989477d-315), ...
;;; -- denormals whose bit patterns are the HIGH 32 bits of the correct
;;; doubles (0x3ff00000 is 1.0d0's), i.e. reads straddling two elements.
;;; All eight observed values matched address = v + index*8 - 8 exactly.
(define-arm64-vinsn scale-128bit-misc-index (((dest :u64))
                                             ((idx :imm)) ; a fixnum
                                             ())
  (add dest idx idx)
  (add dest dest (:$ arm64::complex-double-float.realpart)))

;;; set-constant-msb0-bit-to-1 -- donor PPC64 ppc64-vinsns.lisp:1945
;;; set-constant-ppc-bit-to-1: (oris/ori #x8000>>...) sets PHYSICAL bit
;;; (31 - bitnum) -- IBM MSB0 numbering, correct on BIG-endian where
;;; bit-vector bit i lives at MSB0 position (i mod 32) of its u32 word.
;;; ARM64-DEVIATION (LSB0; v2 s92 cont-63, boot-validated): on
;;; little-endian, bit i lives at LSB0 position (i mod 32), so the
;;; physical bit IS bitnum: orr with (ash 1 bitnum), NOT the donor's
;;; (ash #x8000... (- bitnum)) MSB0 mirror.  The caller (his
;;; arm642-vset1 constant-index leg; ppc2:2124 shape) passes bitnum =
;;; (logand index 31) unchanged, and this lane's VARIABLE-index set
;;; path is already LSB0 (w6 shift-left-variable-word: mask =
;;; 1 << bitnum) -- the read and set sides agree.  Name keeps the
;;; historical "msb0" tag (his arm642 emits it by this name).  Only the
;;; -to-1 member is demanded (l1-clos-boot); -to-0/-to-variable-value
;;; stay undrafted per demand discipline.
(define-arm64-vinsn set-constant-msb0-bit-to-1 (((dest :u32))
                                                ((src :u32)
                                                 (bitnum :u8const)))
  (orr dest src (:$ (:apply ash 1 bitnum))))

;;; ============ u64->integer / s64->integer (boxing twins) ============

;;; u64->integer -- the boxing INVERSE of unbox-u64 above.  PPC64 donor
;;; ppc64-vinsns.lisp:2543 -- an INLINE fixnum-or-bignum path (NOT a
;;; .SPmakeu64 subprim call): fits-positive-fixnum fastpath (top
;;; fixnumshift+1 bits zero), else heap-cons a 2-digit (16-byte) bignum,
;;; or 3-digit (32-byte, aligned_bignum_size(3)) when bit 63 is set.
;;; !! DONOR BUG, not reproduced (comment-gold class): the donor's
;;; :three leg stores `header` STILL HOLDING two-digit-bignum-header
;;; (its second `li` is missing) -- a u64 >= 2^63 would box as a NEGATIVE
;;; 2-digit bignum inside a 32-byte allocation.  The kernel's
;;; _SPmakeu64 (vendor ppc-spentry.s:6551, PPC64 branch) is the
;;; authoritative sibling: it loads three_digit_bignum_header before
;;; the 3-digit alloc.  Ported per the kernel.
;;; ARM64-DEVIATION (LE, as unbox-u64): PPC's rotldi digit-swizzle
;;; dropped -- the raw u64 stored as one LE 64-bit word at
;;; misc-data-offset IS the digit pair (third digit of the 32-byte case
;;; = 0 from the zeroed allocation area, as on PPC).
;;; Alloc idiom = w4 make-vcell verbatim (sub allocptr / cmp allocbase /
;;; b.hi / udf#4 / stur header @mho / tagged result / untag allocptr);
;;; udf#4 kept to match the current additions' trap numbering (the
;;; c9e7ffb uuo-canon reconcile is a tracked separate follow-up, resync
;;; doc item 3).  Fixnum window spelled from fixnumshift (=
;;; nfixnumtagbits on this arch): top (fixnumshift+1) bits must be 0.
;;; Raw u64 lives ONLY in :u64 imm-class regs (src/header) -- never a
;;; node reg (Matt's msg-18/19 GC-safety catch).
(define-arm64-vinsn u64->integer (((result :lisp))
                                  ((src :u64))
                                  ((header :u64)))
  (lsl result src (:$ arm64::fixnumshift))
  (tst src (:$ (:apply logand #xffffffffffffffff
                       (:apply lognot
                               (:apply 1- (:apply ash 1 (- 63 arm64::fixnumshift)))))))
  (b.eq :done)
  (cmp src (:$ 0))
  (b.lt :three)
  (movz header (:$ arm64::two-digit-bignum-header))
  (sub allocptr allocptr (:$ (- 16 arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hi :no-trap2)
  (udf (:$ 4))                          ;uuo_alloc
  :no-trap2
  (stur header (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov result allocptr)
  (and allocptr allocptr (:$ (:apply ldb (byte 64 0)
                                     (:apply lognot arm64::fulltagmask))))
  (b :store)
  :three
  (movz header (:$ arm64::three-digit-bignum-header)) ;kernel ppc-spentry.s:6551, NOT the donor's stale two-digit
  (sub allocptr allocptr (:$ (- 32 arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hi :no-trap3)
  (udf (:$ 4))                          ;uuo_alloc
  :no-trap3
  (stur header (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov result allocptr)
  (and allocptr allocptr (:$ (:apply ldb (byte 64 0)
                                     (:apply lognot arm64::fulltagmask))))
  :store
  (stur src (:@ result (:$ arm64::misc-data-offset)))
  :done)

;;; s64->integer -- PPC64 donor ppc64-vinsns.lisp:2515 (trivial signed
;;; twin, co-ported): fixnum iff the value survives the box/unbox
;;; round-trip (the donor's addo-overflow trick has no NZCV analog
;;; worth carrying -- lsl/asr/cmp is the lane's established
;;; reconstruction idiom, cf. unbox-s32/require-s32); else a 2-digit
;;; bignum ALWAYS suffices (64-bit signed = the two digits, LE store,
;;; same rotldi-drop).  (No u64->fixnum donor exists in PPC64 --
;;; u32->integer:2537 is the only other member and is undemanded.)
(define-arm64-vinsn s64->integer (((result :lisp))
                                  ((src :s64))
                                  ((header :u64)))
  (lsl result src (:$ arm64::fixnumshift))
  (asr header result (:$ arm64::fixnumshift))
  (cmp header src)
  (b.eq :done)
  (movz header (:$ arm64::two-digit-bignum-header))
  (sub allocptr allocptr (:$ (- 16 arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hi :no-trap)
  (udf (:$ 4))                          ;uuo_alloc
  :no-trap
  (stur header (:@ allocptr (:$ arm64::misc-header-offset)))
  (mov result allocptr)
  (and allocptr allocptr (:$ (:apply ldb (byte 64 0)
                                     (:apply lognot arm64::fulltagmask))))
  (stur src (:@ result (:$ arm64::misc-data-offset)))
  :done)

;;; ref-interrupt-level -- read the current interrupt level (a fixnum)
;;; from the thread's TLB slot.  PPC64 LINE-PORT (ppc64-vinsns.lisp:2172):
;;; ld temp tcr.tlb-pointer(rcontext); ld dest INTERRUPT-LEVEL-BINDING-
;;; INDEX(temp).  Spellings per the gate-proven w10 sibling
;;; bind-interrupt-level-0-inline (same two loads at :360-361).
(define-arm64-vinsn ref-interrupt-level (((dest :imm))
                                         ()
                                         ((temp :imm)))
  (ldr temp (:@ rcontext (:$ arm64::tcr.tlb-pointer)))
  (ldr dest (:@ temp (:$ arm64::interrupt-level-binding-index))))

;;; unbox-s8 -- PPC64 LINE-PORT (ppc64-vinsns.lisp:1442): the width-8
;;; member of the unbox-s16/s32 family above, same reconstruct-and-
;;; compare check at width 8, extract = SBFM (imms = fixnumshift+7).
;;; Error code arch::error-object-not-signed-byte-8 (x8664-vinsns:1214).
(define-arm64-vinsn unbox-s8 (((dest :s8))
                              ((src :lisp))
                              ((tag :s64)))
  :again
  (lsl tag src (:$ (- arm64::nbits-in-word (+ 8 arm64::fixnumshift))))
  (asr tag tag (:$ (- arm64::nbits-in-word 8)))
  (lsl tag tag (:$ arm64::fixnumshift))
  (cmp tag src)
  (b.eq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm64::xtype-s8))
  (b :again)
  :got-it
  (sbfm (:x dest) src (:$ arm64::fixnumshift)
        (:$ (+ arm64::fixnumshift 7))))

;;; %unbox-u16 -- width-16 sibling of w1's %unbox-u8 (:68): unchecked
;;; UBFM extract (caller type-checked).  PPC64 rldicl family.
(define-arm64-vinsn %unbox-u16 (((dest :u16))
                                ((src :lisp)))
  (ubfm (:x dest) src (:$ arm64::fixnumshift)
        (:$ (+ arm64::fixnumshift 15))))

(define-arm64-vinsn (bind-nil :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPbind-nil")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (bind-self :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPbind-self")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (bind-self-boundp-check :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPbind-self-boundp-check")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn (spread-lexpr :call :subprim)
    (() () ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPspread-lexprz")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

(define-arm64-vinsn s32->fixnum (((result :imm))
                                 ((val :s32)))
  (sbfm (:x result) (:x val)
        (:$ (- arm64::nbits-in-word arm64::fixnumshift))
        (:$ (1- 32))))

;;; ============ set-c-arg / set-single-c-arg / set-double-c-arg ============
;;; argnum is a WORD index into the param area (param0 = word 0); the
;;; handler assigns words 0-7 to GPR args, 8.. to overflow, then FP
;;; staging.  Byte offsets are 16+8k: 8-aligned, scaled-STR encodable
;;; through the whole u16const range.  Little-endian: singles store/
;;; load at offset+0 (PPC's +4 was big-endian) -- v2 donor deviation
;;; kept.  Register width (X/S/D form) follows the operand class, as
;;; in the landed w9 float bodies (store-double stur idiom).
(define-arm64-vinsn set-c-arg (()
                               ((argval :u64)
                                (argnum :u16const)))
  (str argval (:@ sp (:$ (:apply + arm64::c-frame.param0
                                 (:apply ash argnum 3))))))

(define-arm64-vinsn set-single-c-arg (()
                                      ((argval :single-float)
                                       (argnum :u16const)))
  (str argval (:@ sp (:$ (:apply + arm64::c-frame.param0
                                 (:apply ash argnum 3))))))

(define-arm64-vinsn set-double-c-arg (()
                                      ((argval :double-float)
                                       (argnum :u16const)))
  (str argval (:@ sp (:$ (:apply + arm64::c-frame.param0
                                 (:apply ash argnum 3))))))

(define-arm64-vinsn reload-single-c-arg (((argval :single-float))
                                         ((argnum :u16const)))
  (ldr argval (:@ sp (:$ (:apply + arm64::c-frame.param0
                                 (:apply ash argnum 3))))))

(define-arm64-vinsn reload-double-c-arg (((argval :double-float))
                                         ((argnum :u16const)))
  (ldr argval (:@ sp (:$ (:apply + arm64::c-frame.param0
                                 (:apply ash argnum 3))))))

;;; ============ ff-call ============
;;; Call .SPffcall (his defsubprim, arm64-arch.lisp:530 -- already in
;;; the registered table, no subprims-additions entry needed).
;;; Contract (msg-29): C-function address in arg_z (node: macptr or
;;; fixnum-locative -- subprim extracts the raw pointer); frame as
;;; above at SP.  Returns with the GPR result in imm0 (x0), FP result
;;; in d0, frame popped, Lisp state restored.  Body = w4/w9/w10
;;; subprim-caller canon (getu64 shape): nil-relative table load, blr.
;;; !! KERNEL GAP: _SPffcall body not yet written for his kernel --
;;; queued with the bind-family bodies (same class); compile/xload
;;; need only the table entry.
(define-arm64-vinsn (ff-call :call :subprim) (()
                                              ()
                                              ((temp (:u64 #.arm64::imm1))))
  (movz temp (:$ (:apply arm64::subprimitive-offset ".SPffcall")))
  (ldr temp (:@ rcontext temp))
  (blr temp))

;;; ============ eep.address ============
;;; PPC64 ppc64-vinsns.lisp:3829: load slot 1 (the address) of an
;;; external-entry-point gvector, trap if NIL (unresolved eep -- PPC's
;;; tdeqi rendered as cmp/brk per the lane trap registry; #xf0fd =
;;; unresolved-eep, first use).  Slot offset -4+8 under HIS
;;; misc-data-offset=-4 => LDUR (w8 node-slot-ref idiom).
;;; 16m8: dest was the donor's class `t` referenced BARE -- his emitter
;;; silently dropped the ldur AND the cmp (964e3 silent-drop class;
;;; gdb-observed: only b.ne/brk emitted, stale flags, arg_z kept
;;; malloc's return value, _SPffcall branched to arg0 = the l1-init
;;; #_memset spin).  Class-t operands need the (:x ...) view (w1 load-t
;;; rule); a node dest is :lisp anyway -- bare :lisp in ldur/cmp is the
;;; exercised ref-symbol-value-inline shape.
(define-arm64-vinsn eep.address (((dest :lisp))
                                 ((src (:lisp (:ne dest)))))
  (ldur dest (:@ src (:$ (:apply + arm64::misc-data-offset
                                 (:apply ash 1 arm64::word-shift)))))
  (cmp dest rnil)
  (b.ne :ok)
  (uuo-error-eep-unresolved dest src)
  :ok)

;;; ============ pop-argument-registers ============
;;; Emit site: arm642-call-fn (arm642-additions.lisp:664, from
;;; arm2.lisp:2837) when nargs is unknown (multiple-value call).  PPC64
;;; routes this through .SPvpopargregs (ppc64-vinsns.lisp:3975); our
;;; kernel port has the spentry BODY (spentry-D-call-builtins.s:1560)
;;; but his 123-slot sptab (arm64-spentry.s:105-229) has no slot for it
;;; and extending the table is a kernel rebuild -- so inline it, the way
;;; ARM32 does (arm-vinsns.lisp:1952, same 3-arg-register profile).
;;; Semantics = the PPC64 spentry verbatim: nargs is a fixnum count;
;;; pop arg_z (1), arg_z+arg_y (2), or arg_z+arg_y+arg_x (3+) from the
;;; vstack, youngest at [vsp].
(define-arm64-vinsn (pop-argument-registers :pop :node :vsp) (()
                                                              ())
  (cmp nargs (:$ 0))
  (b.eq :done)
  (cmp nargs (:$ (ash 2 arm64::fixnumshift)))
  (b.lt :one)
  (b.eq :two)
  ;; 3 or more args: three pops
  (ldr arg_z (:@ vsp (:$ 0)))
  (ldr arg_y (:@ vsp (:$ arm64::node-size)))
  (ldr arg_x (:@ vsp (:$ (* 2 arm64::node-size))))
  (add vsp vsp (:$ (* 3 arm64::node-size)))
  (b :done)
  :two
  (ldr arg_z (:@ vsp (:$ 0)))
  (ldr arg_y (:@ vsp (:$ arm64::node-size)))
  (add vsp vsp (:$ (* 2 arm64::node-size)))
  (b :done)
  :one
  (ldr arg_z (:@ vsp (:$ 0)))
  (add vsp vsp (:$ arm64::node-size))
  :done)

;;; ============ macptr float readers ============
;;; PPC64 ppc64-vinsns.lisp mem-ref-c-double-float (lfd) /
;;; mem-ref-double-float (lfdx) and the single-float pair.  The C
;;; (constant-offset) forms use the scaled ldr immediate; the handler
;;; (arm642-get-float, w14) guards alignment + range so the scaled form
;;; always encodes.  The reg-offset forms compute the address with a
;;; plain add -- no shifted-reg or reg-offset addressing modes (encode
;;; landmine class, corrections memory).
(define-arm64-vinsn mem-ref-c-double-float (((dest :double-float))
                                            ((src :address)
                                             (index :s16const)))
  (ldr dest (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-double-float (((dest :double-float))
                                          ((src :address)
                                           (index :s64))
                                          ((addr :u64)))
  (add addr src index)
  (ldr dest (:@ addr (:$ 0))))

(define-arm64-vinsn mem-ref-c-single-float (((dest :single-float))
                                            ((src :address)
                                             (index :s16const)))
  (ldr dest (:@ src (:$ index))))

(define-arm64-vinsn mem-ref-single-float (((dest :single-float))
                                          ((src :address)
                                           (index :s64))
                                          ((addr :u64)))
  (add addr src index)
  (ldr dest (:@ addr (:$ 0))))

;;; ============ macptr bit readers ============
;;; PPC64 mem-ref-c-bit* pack the byte index and a rotate count into
;;; rlwinm shapes; the arm64 contract is plainer: the handler passes the
;;; BYTE index (ldrb-scaled, guarded < 4096) and the bit number within
;;; the byte, and we shift+mask.  The reg-offset forms take the fixnum
;;; BIT offset (as arg_z), unbox it, and split it 3/3.
;;; lsr with a register count = lsrv alias (w2:493 precedent).
(define-arm64-vinsn mem-ref-c-bit-fixnum (((dest :lisp))
                                          ((src :address)
                                           (byte-index :u16const)
                                           (bit :u8const))
                                          ((temp :u64)))
  (ldrb (:w temp) (:@ src (:$ byte-index)))
  (lsr temp temp (:$ bit))
  (and temp temp (:$ 1))
  (lsl dest temp (:$ arm64::fixnumshift)))

(define-arm64-vinsn mem-ref-c-bit (((dest :u8))
                                   ((src :address)
                                    (byte-index :u16const)
                                    (bit :u8const)))
  (ldrb (:w dest) (:@ src (:$ byte-index)))
  (lsr dest dest (:$ bit))
  (and dest dest (:$ 1)))

(define-arm64-vinsn mem-ref-bit-fixnum (((dest :lisp))
                                        ((src :address)
                                         (offset :lisp))
                                        ((bitnum :u64)
                                         (addr :u64)))
  (lsr bitnum offset (:$ arm64::fixnumshift))
  (lsr addr bitnum (:$ 3))
  (add addr src addr)
  (ldrb (:w addr) (:@ addr (:$ 0)))
  (and bitnum bitnum (:$ 7))
  (lsr addr addr bitnum)
  (and addr addr (:$ 1))
  (lsl dest addr (:$ arm64::fixnumshift)))

;;; 16m48: this was the ONLY member of the macptr bit family that reused its
;;; own `dest' as a 64-bit address accumulator and shift operand.  `dest' is
;;; declared :u8, which parses W, while `src' (:address) and `bitnum' (:u64)
;;; parse X -- and AArch64 has no mixed-width shift, add or address base, so
;;; the assembler had no template for four of the seven lines and
;;; VINSN-SIMPLIFY-INSTRUCTION warned on each:
;;;   (LSR DEST BITNUM (:$ 3))          W,X,imm  vs arm64-asm.lisp:824/825 W,W/X,X
;;;   (ADD DEST SRC DEST)               W,X,W    vs :462/463 all-W or all-X
;;;   (LDRB (:W DEST) (:@ DEST (:$ 0))) base W   vs :733 (:base :x/sp)
;;;   (LSR DEST DEST BITNUM)            W,W,X    vs :822/823
;;; NOT missing templates -- every one of those instructions has both a W and
;;; an X template and neither can be widened, because the ISA has no such
;;; encoding.  It is an operand-CLASS error in this vinsn alone.
;;;
;;; Every sibling here already gets this right by carrying a dedicated
;;; (addr :u64): mem-ref-bit-fixnum, mem-set-bit-0/-1 and
;;; mem-set-bit-variable-value are all warning-free, and mem-ref-c-bit is
;;; clean the other way -- wholly W, because its bit number is a constant.
;;; This now matches mem-ref-bit-fixnum line for line except for the last
;;; instruction, which narrows to the :u8 result instead of boxing a fixnum.
;;;
;;; !! REVIEW POINT: the final (:w addr) is the one construct in this patch
;;; with no exact precedent -- the corpus uses (:x reg) to WIDEN a :u32
;;; operand (w3b's extract-variable-bit-fixnum, which is live and measured
;;; working) but never (:w reg) to narrow a :u64 one.  Same operand parser,
;;; mirror direction.  If a "no template matched (AND DEST (:W ADDR) (:$ 1))"
;;; warning survives this, the parser does not narrow, and the fallback is to
;;; keep the byte in `dest' from the ldrb and give the shift amount its own
;;; :u32 temp.
(define-arm64-vinsn mem-ref-bit (((dest :u8))
                                 ((src :address)
                                  (offset :lisp))
                                 ((bitnum :u64)
                                  (addr :u64)))
  (lsr bitnum offset (:$ arm64::fixnumshift))   ;bit index          X,X,imm
  (lsr addr bitnum (:$ 3))                      ;byte index         X,X,imm
  (add addr src addr)                           ;byte address       X,X,X
  (ldrb (:w addr) (:@ addr (:$ 0)))             ;the byte, zero-ext W,[X]
  (and bitnum bitnum (:$ 7))                    ;bit within byte    X,X,limm-x
  (lsr addr addr bitnum)                        ;                   X,X,X
  (and dest (:w addr) (:$ 1)))                  ;narrow to :u8      W,W,limm-w

;;; ============ macptr float/bit writers (set side) ============
;;; Demanded one cycle after the readers: %set-bit (lib/level-2.lisp) and
;;; %set-single-float (lib/db-io.lisp).  Same contracts as the reader
;;; family above; PPC64 mem-set-* lineage (stfd/stfdx etc.), x8664
;;; handler structure.  Plain 3-register logical forms only (the
;;; shifted-register operand class is not expressible in a vinsn body --
;;; corrections memory).
(define-arm64-vinsn mem-set-c-double-float (()
                                            ((val :double-float)
                                             (src :address)
                                             (index :s16const)))
  (str val (:@ src (:$ index))))

(define-arm64-vinsn mem-set-double-float (()
                                          ((val :double-float)
                                           (src :address)
                                           (index :s64))
                                          ((addr :u64)))
  (add addr src index)
  (str val (:@ addr (:$ 0))))

(define-arm64-vinsn mem-set-c-single-float (()
                                            ((val :single-float)
                                             (src :address)
                                             (index :s16const)))
  (str val (:@ src (:$ index))))

(define-arm64-vinsn mem-set-single-float (()
                                          ((val :single-float)
                                           (src :address)
                                           (index :s64))
                                          ((addr :u64)))
  (add addr src index)
  (str val (:@ addr (:$ 0))))

(define-arm64-vinsn mem-set-c-bit-0 (()
                                     ((src :address)
                                      (byte-index :u16const)
                                      (bit :u8const))
                                     ((temp :u64)
                                      (mask :u64)))
  (ldrb (:w temp) (:@ src (:$ byte-index)))
  (movz mask (:$ (:apply ash 1 bit)))
  (bic temp temp mask)
  (strb (:w temp) (:@ src (:$ byte-index))))

(define-arm64-vinsn mem-set-c-bit-1 (()
                                     ((src :address)
                                      (byte-index :u16const)
                                      (bit :u8const))
                                     ((temp :u64)
                                      (mask :u64)))
  (ldrb (:w temp) (:@ src (:$ byte-index)))
  (movz mask (:$ (:apply ash 1 bit)))
  (orr temp temp mask)
  (strb (:w temp) (:@ src (:$ byte-index))))

;;; val = fixnum 0 or 1 (node reg); merge it at the constant bit position.
(define-arm64-vinsn mem-set-c-bit (()
                                   ((src :address)
                                    (byte-index :u16const)
                                    (bit :u8const)
                                    (val :lisp))
                                   ((temp :u64)
                                    (valbit :u64)))
  (ldrb (:w temp) (:@ src (:$ byte-index)))
  (movz valbit (:$ (:apply ash 1 bit)))
  (bic temp temp valbit)
  (lsr valbit val (:$ arm64::fixnumshift))
  (lsl valbit valbit (:$ bit))
  (orr temp temp valbit)
  (strb (:w temp) (:@ src (:$ byte-index))))

;;; offset = fixnum BIT index (node reg).
(define-arm64-vinsn mem-set-bit-0 (()
                                   ((src :address)
                                    (offset :lisp))
                                   ((bitnum :u64)
                                    (addr :u64)
                                    (temp :u64)
                                    (mask :u64)))
  (lsr bitnum offset (:$ arm64::fixnumshift))
  (lsr addr bitnum (:$ 3))
  (add addr src addr)
  (and bitnum bitnum (:$ 7))
  (movz mask (:$ 1))
  (lsl mask mask bitnum)
  (ldrb (:w temp) (:@ addr (:$ 0)))
  (bic temp temp mask)
  (strb (:w temp) (:@ addr (:$ 0))))

(define-arm64-vinsn mem-set-bit-1 (()
                                   ((src :address)
                                    (offset :lisp))
                                   ((bitnum :u64)
                                    (addr :u64)
                                    (temp :u64)
                                    (mask :u64)))
  (lsr bitnum offset (:$ arm64::fixnumshift))
  (lsr addr bitnum (:$ 3))
  (add addr src addr)
  (and bitnum bitnum (:$ 7))
  (movz mask (:$ 1))
  (lsl mask mask bitnum)
  (ldrb (:w temp) (:@ addr (:$ 0)))
  (orr temp temp mask)
  (strb (:w temp) (:@ addr (:$ 0))))

(define-arm64-vinsn mem-set-bit-variable-value (()
                                                ((src :address)
                                                 (offset :lisp)
                                                 (val :lisp))
                                                ((bitnum :u64)
                                                 (addr :u64)
                                                 (temp :u64)
                                                 (mask :u64)))
  (lsr bitnum offset (:$ arm64::fixnumshift))
  (lsr addr bitnum (:$ 3))
  (add addr src addr)
  (and bitnum bitnum (:$ 7))
  (movz mask (:$ 1))
  (lsl mask mask bitnum)
  (ldrb (:w temp) (:@ addr (:$ 0)))
  (bic temp temp mask)
  (lsr mask val (:$ arm64::fixnumshift))
  (lsl mask mask bitnum)
  (orr temp temp mask)
  (strb (:w temp) (:@ addr (:$ 0))))

;;; ============ natural (unboxed u64) logical ops ============
;;; Demand: %natural-logand in lib/db-io.lisp (cdb record math);
;;; logior/logxor are the same shape (PPC64 ppc64-vinsns
;;; %natural-logand/-logior/-logxor: 3-register and/or/xor).
;;; Handlers in arm642-additions-w14.
(define-arm64-vinsn %natural-logand (((dest :u64))
                                     ((x :u64)
                                      (y :u64)))
  (and dest x y))

(define-arm64-vinsn %natural-logior (((dest :u64))
                                     ((x :u64)
                                      (y :u64)))
  (orr dest x y))

(define-arm64-vinsn %natural-logxor (((dest :u64))
                                     ((x :u64)
                                      (y :u64)))
  (eor dest x y))

;;; ============ ivector-typecode-p / gvector-typecode-p ============
;;; Demand (16m28, pin advance to 33e61e6): his arm642-ivector-typecode-p
;;; and arm642-gvector-typecode-p (arm642.lisp:6188/6195) emit them, and
;;; level-0 calls both in l0-array (the array-header subtype predicates,
;;; e.g. l0-array.lisp:233 -- which is where the cross-compile stopped).
;;;
;;; PPC64 (ppc64-vinsns.lisp:4246/4267) is the reference and the semantics
;;; are NOT a boolean: SRC is the BOXED typecode, and the result is SRC
;;; itself when its fulltag is in the class, else fixnum 0 -- the callers
;;; read it as (>= (the (unsigned-byte 8) (ivector-typecode-p tc)) min-...),
;;; so the "no" answer has to be a number below every real subtag.
;;;
;;; PPC's method carries over unchanged: 1 << fulltag, tested against a
;;; bitmask of the class's fulltags.  Only the membership differs, and
;;; that is the 16m19 lesson restated -- Matt's subtags are not
;;; class-monotonic: THREE immheader fulltags (4, 5, 13) and TWO
;;; nodeheader fulltags (6, 14), against PPC64's four each.  Neither
;;; bitmask (#x2030 / #x4040) is a valid AArch64 logical immediate, so it
;;; is materialized with movz and the test is register-form TST.
;;; CSEL needs no dest==src guard (PPC's `(:not (:pred = dest src)) (mr
;;; dest src)`): it reads both sources before writing.
(define-arm64-vinsn ivector-typecode-p (((dest :lisp))
                                        ((src :lisp))
                                        ((temp :u64)
                                         (mask :u64)))
  (and temp src (:$ (ash arm64::fulltagmask arm64::fixnumshift)))
  (lsr temp temp (:$ arm64::fixnumshift))
  (movz mask (:$ 1))
  (lsl mask mask temp)
  (movz temp (:$ (logior (ash 1 arm64::fulltag-immheader-0)
                         (ash 1 arm64::fulltag-immheader-1)
                         (ash 1 arm64::fulltag-immheader-2))))
  (tst mask temp)
  (csel dest src xzr (:? ne)))

(define-arm64-vinsn gvector-typecode-p (((dest :lisp))
                                        ((src :lisp))
                                        ((temp :u64)
                                         (mask :u64)))
  (and temp src (:$ (ash arm64::fulltagmask arm64::fixnumshift)))
  (lsr temp temp (:$ arm64::fixnumshift))
  (movz mask (:$ 1))
  (lsl mask mask temp)
  (movz temp (:$ (logior (ash 1 arm64::fulltag-nodeheader-0)
                         (ash 1 arm64::fulltag-nodeheader-1))))
  (tst mask temp)
  (csel dest src xzr (:? ne)))

;;; ============ bit-vector element store cluster ============
;;; Demand (16m28): his bit-vector %aset1/%set-bit path emits all four
;;; (arm642.lisp:2313-2337) and level-0's (setf (sbit ...)) in l0-array is
;;; where the cross-compile now stands.  ARM32 (arm-vinsns.lisp:1396) is
;;; the only reference port that names set-or-clear-bit; the
;;; set-constant-bit-* trio has no reference body anywhere -- PPC64 does
;;; the same work inline with rlwinm -- so those bodies are AArch64's own
;;; bitfield forms.  His path reads and writes the word with
;;; misc-ref-c-u32 / misc-set-c-u32, so every operand is a W register and
;;; BIT is always < 32; the bfi lsb>=32 landmine cannot apply here.
;;; NB every constant that mentions the BIT parameter has to go through
;;; (:apply ...) -- a bare (ash 1 bit) would be evaluated when the vinsn
;;; is DEFINED, not when it is expanded.
(define-arm64-vinsn set-constant-bit-to-0 (((dest :u32))
                                           ((src :u32)
                                            (bit :u8const)))
  ;; AArch64 has no BIC-immediate; AND with the complement, which is a run
  ;; of 31 ones and so a legal 32-bit logical immediate (his
  ;; encode-logical-immediate-32).
  (and dest src (:$ (:apply logand #xffffffff
                            (:apply lognot (:apply ash 1 bit))))))

(define-arm64-vinsn set-constant-bit-to-1 (((dest :u32))
                                           ((src :u32)
                                            (bit :u8const)))
  (orr dest src (:$ (:apply ash 1 bit))))

(define-arm64-vinsn set-constant-bit-to-variable-value (((dest :u32))
                                                        ((src :u32)
                                                         (val :u32)
                                                         (bit :u8const)))
  ;; VAL holds the unboxed bit value (0 or 1); BFI drops its low bit into
  ;; position BIT, which makes the caller's dest==src case a single insn.
  ((:not (:pred = (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mov dest src))
  (bfi dest val (:$ bit) (:$ 1)))

(define-arm64-vinsn set-or-clear-bit (((dest :u32))
                                      ((src :u32)
                                       (mask :u32)
                                       (crf :crf))
                                      ((setw :u32)
                                       (clrw :u32)))
  ;; ARM32 predicates the two forms -- biceq / orrne: clear the bit when
  ;; the value compared EQUAL to 0, set it otherwise.  AArch64 has no
  ;; predicated data processing, so compute both and select.  MASK is
  ;; already 1<<bit (his caller builds it with lri + shift-left-variable-
  ;; word), and CRF carries the flags from his compare-immediate of the
  ;; value against 0.
  (orr setw src mask)
  (bic clrw src mask)
  (csel dest clrw setw (:? eq)))

;;; ============ fixnum-as-address double-float family ============
;;; PPC64 LINE-PORT (ppc64-vinsns.lisp:4222-4243, all four).  His own
;;; drafts exist at arm64-vinsns.lisp:447-469 but sit inside a #| ... |#
;;; block AND are still raw PPC mnemonics (lfd/lfdx/stfd/stfdx), so
;;; nothing defines them -- exactly the unbox-s64 shape (w11:211).
;;; The acode handlers are HIS and are already live: arm642.lisp:6110
;;; arm642-fixnum-ref-double-float / :6141 arm642-fixnum-set-double-float.
;;;
;;; BASE is a fixnum standing for a raw machine address (the %fixnum-ref-*
;;; family's contract), so NO misc-data bias applies here -- unlike the
;;; misc-ref-double-float twins (w3a:418), whose displacement carries
;;; misc-dfloat-offset.  The byte displacement is exactly 8*idx.
;;;
;;; Index scaling, both forms, is a NULL transform on this lane and that
;;; is why PPC64 indexes raw:
;;;   -c- forms: IDX is a raw element index, which his handler gates to
;;;     (unsigned-byte 12) (arm642.lisp:6117, :6143), so 8*idx is
;;;     8-aligned and <= 32760 -- precisely the D-form scaled LDR/STR
;;;     immediate range (0..32760 step 8).  Scaled LDR/STR encodes for
;;;     the whole gated range; no LDUR/STUR needed.
;;;   register forms: IDX arrives as a BOXED FIXNUM in a node register,
;;;     and fixnumshift = 3 = word-shift (arm64-arch.lisp:23,31), so its
;;;     machine value ALREADY IS the byte offset 8*n.  No lsl.
;;; ARM64-DEVIATION: none -- the addressing is a direct PPC64 analog.
;;;
;;; NOTE for the v2 tree only: comms/ARM64-NARGS-RAW-SIBLING-AUDIT.md:104
;;; flags the raw index here as needing an `lsl #3` first.  That is a
;;; HIGH-TAG finding (fixnumshift = 0 there, so 8*n needed materializing);
;;; on this low-tag lane fixnumshift = 3 and the shift would be WRONG.
;;;
;;; Templates -- VERIFIED IN HIS ASSEMBLER, not inferred, because the
;;; "macptr float readers" note above says the reg-offset addressing modes
;;; are an encode landmine and routes its own reg forms through an explicit
;;; `add`.  That note is over-cautious: it holds for a SHIFTED-register
;;; operand, not for a plain register index.  arm64-asm.lisp @ the pin:
;;;   :782 (def str ((:rt :d) (:mem-regoff (:base :x/sp) (:index :regoff3))))
;;;   :783 (def ldr ((:rt :d) (:mem-regoff (:base :x/sp) (:index :regoff3))))
;;; so the D-form regoff pair EXISTS.  :regoff3 bakes in a natural scale of
;;; 3, but the scale is OPT-IN via the S bit: match-index-operand (:1788)
;;; accepts an amount of either 0 or the scale, and encode-index-operand
;;; (:2221) emits `S @ 12 = (if (zerop amount) 0 1)`.  A bare register name
;;; parses to a register-operand whose AMOUNT DEFAULTS TO 0 (:1305-1308),
;;; and index-option (:1778) maps a nil modifier at width 64 to UXTX -- so
;;; `(:@ base idx)` encodes LDR/STR Dt,[Xbase,Xidx,LSL #0]: the index is
;;; used as a raw byte offset, which is exactly lfdx/stfdx.  Had it scaled
;;; by 8 the already-scaled fixnum would have been multiplied twice.
;;; Same-shape precedent, and the build-proven one: w10's
;;; ref-symbol-value-inline `(ldr dest (:@ table idx))` with idx :imm
;;; indexes the TLB by a raw fixnum byte offset and is exercised on every
;;; special-variable reference in the live boot.
;;;
;;; dest/val are FPRs and every input is a GPR, so the input-before-output
;;; aliasing rule (corrections memory) cannot bind.
(define-arm64-vinsn fixnum-ref-c-double-float (((dest :double-float))
                                               ((base :imm)
                                                (idx :u16const)))
  (ldr dest (:@ base (:$ (:apply ash idx arm64::word-shift)))))

(define-arm64-vinsn fixnum-ref-double-float (((dest :double-float))
                                             ((base :imm)
                                              (idx :imm)))
  (ldr dest (:@ base idx)))

(define-arm64-vinsn fixnum-set-c-double-float (()
                                               ((base :imm)
                                                (idx :u16const)
                                                (val :double-float)))
  (str val (:@ base (:$ (:apply ash idx arm64::word-shift)))))

(define-arm64-vinsn fixnum-set-double-float (()
                                             ((base :imm)
                                              (idx :imm)
                                              (val :double-float)))
  (str val (:@ base idx)))


;;; Reconcile the template ordinals baked into the vinsns just defined
;;; with the assembler's current template table, in case this file was
;;; compiled against a differently-ordered table.
(fixup-arm64-vinsn-templates)
