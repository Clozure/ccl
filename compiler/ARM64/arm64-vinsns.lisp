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
  (uuo-error-wrong-nargs)
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

;;; Reconcile the template ordinals baked into the vinsns just defined
;;; with the assembler's current template table, in case this file was
;;; compiled against a differently-ordered table.
(fixup-arm64-vinsn-templates)
