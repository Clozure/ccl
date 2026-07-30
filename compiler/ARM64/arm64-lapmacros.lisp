(in-package "CCL")

(defarm64lapmacro load-constant (dest constant)
  (let ((offset (arm64::constant-offset constant)))
    (if (typep offset '(signed-byte 9))
      `(ldur ,dest (:@ fn (:$ ,offset)))
      (error "constant ~s is too far away: use load-indexed-constant"
             constant))))

(defarm64lapmacro load-indexed-constant (dest constant idxreg)
  `(progn
     (movz ,idxreg (:$ ,(arm64::constant-offset constant)))
     (ldr ,dest (:@ fn ,idxreg))))

;;; This needs pc_luser_xp support so that building the frame looks
;;; atomic to the gc
(defarm64lapmacro build-lisp-frame (&optional (marker-reg 'imm0))
  `(progn
     (mov ,marker-reg (:$ arm64::lisp-frame-marker))
     (stp ,marker-reg vsp (:@! sp (:$ -32)))
     (stp fn lr (:@ sp (:$ 16)))))

(defarm64lapmacro restore-lisp-frame ()
  `(progn
     (ldp fn lr (:@ sp (:$ 16)))
     (ldr vsp (:@ sp (:$ 8)))            ;ignore marker
     (add sp sp (:$ 32))))

(defarm64lapmacro unbox-fixnum (dest src)
  `(asr ,dest ,src (:$ arm64::fixnumshift)))

(defarm64lapmacro box-fixnum (dest src)
  `(lsl ,dest ,src (:$ arm64::fixnumshift)))

(defarm64lapmacro get-double-float (dest node)
  `(ldur ,dest (:@ ,node (:$ arm64::double-float.value))))

;; a single-float is stored in top 32 bits
(defarm64lapmacro get-single-float-bits (dest node)
  `(lsr ,dest ,node (:$ 32)))

;;; call-subprim: the offset `arm64::subprimitive-offset' returns is a
;;; TCR offset (`*subprims-base*' = `tcr.sptab' = 496, step 8 --
;;; arm64-arch.lisp), and the arch file's own comment states the design:
;;; "the subprim address table will be referenced relative to rcontext.
;;; The lisp kernel will make sure that every thread's TCR will contain
;;; the table."  So the slot holds the subprim's ADDRESS and must be
;;; LOADED; `(add imm0 rnil offset)' computed rnil+496+8n, which is
;;; neither the table nor the subprim.  Observed under gdb at 16m5:
;;; [rnil+0x3b0] read a misaligned NRS splice (0xfc0000302000) and blr'd
;;; into it.  The sequence below is the one the call-subprim-1/2 vinsns
;;; in arm64-vinsns.lisp already use, so the lapmacro was the outlier.
;;; PPC64 has no analog to diff against -- ppc-lapmacros.lisp has no
;;; call-subprim at all (PPC LAP branches absolutely, `bla .SPfoo'); the
;;; structural donor for a table-indirect port is x86-lapmacros.lisp:518,
;;; `(lisp-call (@ (x86-subprim-offset name)))' -- also load-then-call.
;;;
;;; Scratch is imm1, NOT imm0 as the vinsns use: a vinsn declares its
;;; temp and the compiler keeps inputs clear of it, but LAP callers pass
;;; unboxed subprim arguments in imm0 by ABI, so an imm0 scratch here
;;; clobbers the argument.  Reported upstream 2026-07-10 (ratify mail v2).
(defarm64lapmacro call-subprim (spname)
  (let ((offset (arm64::subprimitive-offset spname)))
    (if offset
      `(progn
         (movz imm1 (:$ ,offset))
         (ldr imm1 (:@ rcontext imm1))
         (blr imm1))
      (error "unknown subprimitive name ~s" spname))))

(defarm64lapmacro set-nargs (n)
  (check-type n (unsigned-byte 13))
  `(movz nargs (:$ ',n)))

(defarm64lapmacro check-nargs (min &optional (max min))
  (let ((ok1 (gensym "@"))
        (ok2 (gensym "@")))
    (if (eq max min)
      `(progn
         (cmp nargs (:$ (ash ,min arm64::fixnumshift)))
         (b.eq ,ok1)
         (uuo-error-wrong-number-of-args)
         ,ok1)
      (if (null max)
        (unless (= min 0)
          `(progn
             (cmp nargs (:$ (ash ,min arm64::fixnumshift)))
             (b.hs ,ok1)
             (uuo-error-too-few-args)
             ,ok1))
        (if (= min 0)
          `(progn
             (cmp nargs (:$ (ash ,max arm64::fixnumshift)))
             (b.ls ,ok1)
             (uuo-error-too-many-args)
             ,ok1)
          `(progn
             (cmp nargs (:$ (ash ,min arm64::fixnumshift)))
             (b.hs ,ok1)
             (uuo-error-too-few-args)
             ,ok1
             (cmp nargs (:$ (ash ,max arm64::fixnumshift)))
             (b.ls ,ok2)
             (uuo-error-too-many-args)
             ,ok2))))))

;;; ------------------------------------------------------------------
;;; From the linuxarm64 port.  Ported line-by-line from
;;; compiler/PPC/ppc-lapmacros.lisp (PPC64 arms only); each macro keeps
;;; its donor line number in the comment above it.  Appended in the order
;;; our build concatenated the fasls, so the definition that wins is the
;;; one that won when the suite was measured (ANSI 21679/0).
;;; ------------------------------------------------------------------

;;; ===================================================================
;;; SECTION 1: Tag extraction
;;; ===================================================================

;;; from ppc-lapmacros.lisp:355 (extract 3-bit lisptag from node)
(defarm64lapmacro extract-lisptag (dest node)
  `(and ,dest ,node (:$ arm64::tagmask)))


;;; from ppc-lapmacros.lisp:360 (extract 4-bit fulltag from node)
(defarm64lapmacro extract-fulltag (dest node)
  `(and ,dest ,node (:$ arm64::fulltagmask)))


;;; from ppc-lapmacros.lisp:365 (extract lowtag — on arm64 this is synonym for lisptag)
;;; NOTE: PPC64 has nlowtagbits; arm64 does not define a separate "lowtag".
;;; On Matt's design nlisptagbits=3 and ntagbits=4; "lowtag" = lisptag.
(defarm64lapmacro extract-lowtag (dest node)
  `(and ,dest ,node (:$ arm64::tagmask)))


;;; from ppc-lapmacros.lisp:373 (extract subtag byte from uvector header)
;;; On arm64: subtag is the low 8 bits of the header word.
;;; Header is at misc-header-offset = -fulltag-misc = -4 from tagged pointer.
;;; Load the full header, then extract the low byte.
;;; (Cannot use ldurb directly because LAP register aliases are 64-bit and
;;; ldurb requires a W-register operand for its destination.)
(defarm64lapmacro extract-subtag (dest node)
  `(progn
     (ldur ,dest (:@ ,node (:$ arm64::misc-header-offset)))
     (and ,dest ,dest (:$ arm64::subtag-mask))))


;;; from ppc-lapmacros.lisp:380 (get typecode: if miscobj load subtag, else LISPTAG)
;;; NB the PPC64 original narrows the non-misc result to the 3-bit lisptag
;;; (extract-fulltag, cmp, THEN extract-lisptag, then subtag if misc).  This
;;; matters: fixnums have fulltag 0 or 8 (even/odd), so returning the fulltag
;;; would make (cmp typecode tag-fixnum) fail for odd fixnums.  Mirror PPC64.
(defarm64lapmacro extract-typecode (dest node)
  (let ((done (gensym "@")))
    `(progn
       (extract-fulltag ,dest ,node)
       (cmp ,dest (:$ arm64::fulltag-misc))
       (and ,dest ,dest (:$ arm64::tagmask))
       (b.ne ,done)
       (extract-subtag ,dest ,node)
       ,done)))


;;; from ppc-lapmacros.lisp:513 (extract low 8 bits of a value)
(defarm64lapmacro extract-lowbyte (dest src)
  `(and ,dest ,src (:$ #xff)))


;;; ===================================================================
;;; SECTION 2: Object access — cons, uvector, symbol
;;; ===================================================================

;;; from ppc-lapmacros.lisp:346 (%cdr — load cdr field from cons)
;;; cons layout: cdr @ -cons_bias, car @ -cons_bias + 8
;;; cons_bias = fulltag-cons = 3.  So cdr @ [p - 3], car @ [p + 5].
;;; Using arm64::cons.cdr = -3, arm64::cons.car = 5.
;;; NOTE: These offsets are defined by (define-lisp-object cons fulltag-cons cdr car)
;;; in arm64-arch.lisp: cons.cdr = -fulltag-cons = -3, cons.car = -3 + 8 = 5.
;;; The 9-bit signed immediate form is needed (offsets not multiples of 8).
(defarm64lapmacro %cdr (dest node)
  ;; DECIDE(ledger-ref): cons.cdr should be arm64::cons.cdr = -3 per
  ;; (define-lisp-object cons fulltag-cons cdr car).  Verify this offset
  ;; against the struct in arm64-constants.h.
  `(ldur ,dest (:@ ,node (:$ arm64::cons.cdr))))


;;; from ppc-lapmacros.lisp:350 (%car — load car field from cons)
(defarm64lapmacro %car (dest node)
  `(ldur ,dest (:@ ,node (:$ arm64::cons.car))))


;;; from ppc-lapmacros.lisp:439 (load uvector header word)
(defarm64lapmacro getvheader (dest src)
  `(ldur ,dest (:@ ,src (:$ arm64::misc-header-offset))))


;;; from ppc-lapmacros.lisp:447 (header-size: unboxed element count from header)
;;; On PPC64: srdi dest, header, num-subtag-bits.  Same on arm64.
(defarm64lapmacro header-size (dest vheader)
  `(lsr ,dest ,vheader (:$ arm64::num-subtag-bits)))


;;; from ppc-lapmacros.lisp:456 (header-length: fixnum element count from header)
;;; Shift right by (num-subtag-bits - fixnumshift) to produce a fixnum.
;;; On arm64: num-subtag-bits=8, fixnumshift=3, so shift right by 5 then
;;; clear the tag bits.  Actually: (header >> 8) << 3 = header >> 5 with
;;; low 3 bits cleared.  Use ubfm to extract bits [63:8] shifted left by 3.
;;; Equivalent: lsr by (num-subtag-bits - fixnumshift) then clear low fixnumshift bits.
(defarm64lapmacro header-length (dest vheader)
  ;; (ash (ash header (- num-subtag-bits)) fixnumshift) with high bits intact
  ;; = ubfx dest, vheader, #(num-subtag-bits - fixnumshift), #(64 - num-subtag-bits)
  ;; then lsl dest, dest, #0 — no, simpler:
  ;; dest = (vheader >> num-subtag-bits) << fixnumshift
  ;; = (vheader & ~subtag-mask) >> (num-subtag-bits - fixnumshift)
  ;; Since fixnumshift=3 and num-subtag-bits=8, net shift is 5 right,
  ;; with the bottom 3 bits guaranteed zero (it's a fixnum).
  ;; Use: and + lsr, or ubfm.
  ;; Cleanest: lsr by 5, then bic the low 3 bits (always zero from header).
  ;; Actually the canonical form:
  `(progn
     (lsr ,dest ,vheader (:$ (- arm64::num-subtag-bits arm64::fixnumshift)))
     (and ,dest ,dest (:$ (ldb (byte 64 0) (lognot arm64::fixnummask))))))  ; encoder rejects negative logimms (wave-1 convention)


;;; from ppc-lapmacros.lisp:487 (vector-size: unboxed element count from uvector)
(defarm64lapmacro vector-size (dest v vheader)
  `(progn
     (getvheader ,vheader ,v)
     (header-size ,dest ,vheader)))


;;; from ppc-lapmacros.lisp:492 (vector-length: fixnum element count from uvector)
(defarm64lapmacro vector-length (dest v vheader)
  `(progn
     (getvheader ,vheader ,v)
     (header-length ,dest ,vheader)))


;;; from ppc-lapmacros.lisp:720 (svref — load slot N from simple-vector)
;;; On PPC64: ld dest, (* 8 index) + misc-data-offset, vector
;;; On arm64: misc-data-offset = 4.  Offset = 8*index + 4; not 8-aligned
;;; so we must use ldur (9-bit signed immediate).
(defarm64lapmacro svref (dest index vector)
  `(ldur ,dest (:@ ,vector (:$ (+ (* ,index arm64::node-size) arm64::misc-data-offset)))))


;;; from ppc-lapmacros.lisp:729 (svset — store slot N into simple-vector)
(defarm64lapmacro svset (new-value index vector)
  `(stur ,new-value (:@ ,vector (:$ (+ (* ,index arm64::node-size) arm64::misc-data-offset)))))


;;; from ppc-lapmacros.lisp:713 (macptr-ptr — load raw address from macptr)
(defarm64lapmacro macptr-ptr (dest macptr)
  `(ldur ,dest (:@ ,macptr (:$ arm64::macptr.address))))


;;; ===================================================================
;;; SECTION 3: Stack operations
;;; ===================================================================

;;; from ppc-lapmacros.lisp:329 (push — decrement stack and store)
(defarm64lapmacro push (src stack)
  `(str ,src (:@! ,stack (:$ (- arm64::node-size)))))


;;; from ppc-lapmacros.lisp:331 (vpush — push onto value stack)
(defarm64lapmacro vpush (src)
  `(str ,src (:@! vsp (:$ (- arm64::node-size)))))


;;; from ppc-lapmacros.lisp:339 (vpop — pop from value stack)
;;; Post-indexed load: ldr dest, [vsp], #8
(defarm64lapmacro vpop (dest)
  `(ldr ,dest (:@+ vsp (:$ arm64::node-size))))


;;; from ppc-lapmacros.lisp:736 (vpush-argregs — conditionally push arg_x/y/z)
;;; nargs is SCALED (fixnum-tagged count of args * fixnumone).
;;; nargregs=3, so up to 3 args in registers: arg_z (always if nargs>=1),
;;; arg_y (if nargs>=2), arg_x (if nargs>=3).
(defarm64lapmacro vpush-argregs ()
  (let ((none (gensym "@"))
        (two (gensym "@"))
        (one (gensym "@")))
    `(progn
       (cmp nargs (:$ (ash 2 arm64::fixnumshift)))
       (b.eq ,two)
       (cbz nargs ,none)
       (cmp nargs (:$ (ash 1 arm64::fixnumshift)))
       (b.eq ,one)
       ;; nargs >= 3
       (vpush arg_x)
       ,two
       (vpush arg_y)
       ,one
       (vpush arg_z)
       ,none)))


;;; ===================================================================
;;; SECTION 4: Lisp-frame save / restore
;;; ===================================================================

;;; from ppc-lapmacros.lisp:280 (save-lisp-context)
;;; PPC64 builds a frame: decrement sp by frame-size, store fn/lr/vsp.
;;; Matt's build-lisp-frame already does this!  It stores: marker, vsp, fn, lr
;;; in a 32-byte frame.  The PPC macro also does (mr fn nfn) to establish
;;; the new function register.
;;;
;;; We define save-lisp-context as build-lisp-frame + (mov fn nfn).
;;; The &optional save-pc parameter from PPC is unnecessary on arm64
;;; (lr is stored by stp directly).
(defarm64lapmacro save-lisp-context (&optional (vsp-source 'vsp))
  ;; DECIDE(ledger-ref): Matt's build-lisp-frame always saves vsp.
  ;; If the caller wants a DIFFERENT vsp (common on PPC when non-register
  ;; args were vpushed before the call), we need a variant.  For now,
  ;; handle the common case (vsp-source = vsp) via his macro, and the
  ;; non-default case inline.
  (if (eq vsp-source 'vsp)
    `(progn
       (build-lisp-frame)
       (mov fn nfn))
    ;; Inline variant that saves a computed vsp.  All four level-0 call
    ;; sites pass IMM0 as vsp-source, so the marker scratch register must
    ;; NOT be imm0 (else the marker overwrites the value being saved) —
    ;; use imm1.
    `(progn
       (mov imm1 (:$ arm64::lisp-frame-marker))
       (stp imm1 ,vsp-source (:@! sp (:$ -32)))
       (stp fn lr (:@ sp (:$ 16)))
       (mov fn nfn))))


;;; from ppc-lapmacros.lisp:308 (restore-full-lisp-context)
;;; PPC64 restores: lr, vsp, fn, then adjusts sp.
;;; Matt's restore-lisp-frame does: ldp fn lr @ sp+16; ldr vsp @ sp+8;
;;; add sp sp 32.  It does NOT mtlr (arm64 uses blr/ret lr directly).
;;; The only difference is that PPC puts savelr in loc-pc then does mtlr,
;;; whereas arm64 restores directly into lr.  So restore-full-lisp-context
;;; is exactly Matt's restore-lisp-frame.
(defarm64lapmacro restore-full-lisp-context (&optional (vsp-dest 'vsp))
  (if (eq vsp-dest 'vsp)
    `(restore-lisp-frame)
    ;; Non-default destination: inline the loads
    `(progn
       (ldp fn lr (:@ sp (:$ 16)))
       (ldr ,vsp-dest (:@ sp (:$ 8)))
       (add sp sp (:$ 32)))))


;;; jump-subprim: TAIL transfer (no link) — PPC's `ba .SPfoo`.  For lap
;;; functions whose whole body is one subprim (e.g. %store-node-conditional,
;;; ppc-misc.lisp:483): the subprim's ret returns to the ORIGINAL caller.
(defarm64lapmacro jump-subprim (spname)
  (let ((offset (arm64::subprimitive-offset spname)))
    (if offset
      `(progn
         (movz imm1 (:$ ,offset))
         (ldr imm1 (:@ rcontext imm1))
         (br imm1))
      (error "unknown subprimitive name ~s" spname))))


;;; ===================================================================
;;; SECTION 6: Globals and symbols
;;; ===================================================================

;;; from ppc-lapmacros.lisp:561 (ref-global: load a kernel global)
;;; On PPC64: (ld reg, (offset + nil-value), 0) — absolute address.
;;; On arm64: globals are at negative offsets from rnil (tagged nil).
;;; arm64::%kernel-global returns -(fulltag-nil + (1+pos)*node-size).
;;; ENCODING CONSTRAINT: with 79 globals the offset ranges to -651, but
;;; ldur's signed-9-bit immediate only reaches -256 (and ldr's scaled
;;; unsigned form takes no negative offset at all).  Globals past index
;;; ~30 need a two-instruction form (sub then ldr).  Emit that form
;;; unconditionally when out of ldur range.
(defarm64lapmacro ref-global (reg sym)
  (let ((offset (arm64::%kernel-global sym)))
    (if (>= offset -256)
      `(ldur ,reg (:@ rnil (:$ ,offset)))
      `(progn
         (sub ,reg rnil (:$ ,(- offset)))
         (ldr ,reg (:@ ,reg (:$ 0)))))))


;;; from ppc-lapmacros.lisp:570 (set-global: store a kernel global)
;;; Same encoding constraint; the out-of-range store form needs a scratch
;;; register for the address — use imm1 (never a store payload at the
;;; level-0 set-global sites).
(defarm64lapmacro set-global (reg sym)
  (let ((offset (arm64::%kernel-global sym)))
    (if (>= offset -256)
      `(stur ,reg (:@ rnil (:$ ,offset)))
      `(progn
         (sub imm1 rnil (:$ ,(- offset)))
         (str ,reg (:@ imm1 (:$ 0)))))))


;;; load-nfn-constant — NFN-relative twin of his load-constant (v2
;;; s92 cont-13 class, re-observed 16m8 in EQUAL): a lap function runs
;;; with SELF in nfn and the CALLER's fn in fn until save-lisp-context's
;;; (mov fn nfn); after restore-full-lisp-context fn is the CALLER again,
;;; so any post-restore constant load off fn reads the CALLER's pool
;;; (16m8: EQUAL's hairy-equal load fetched %HASH-PROBE's KEY symbol).
;;; PPC64's donor form is exactly this: `(ld fname 'hairy-equal nfn)`.
(defarm64lapmacro load-nfn-constant (dest constant)
  (let ((offset (arm64::constant-offset constant)))
    (if (typep offset '(signed-byte 9))
      `(ldur ,dest (:@ nfn (:$ ,offset)))
      (error "constant ~s is too far away: use load-indexed-constant"
             constant))))


;;; from ppc-lapmacros.lisp:417 (call-symbol: call through symbol's fcell)
;;; On PPC64: load-constant fname; ld nfn fcell(fname); ld loc-pc data(nfn); mtctr+bctrl
;;; On arm64: load-constant fname; load nfn from symbol.fcell; load entry from
;;; misc-data-offset(nfn); blr.
(defarm64lapmacro call-symbol (function-name)
  ;; Offsets VERIFIED against arm64-arch.lisp: symbol.fcell = 17
  ;; (define-fixedsized-object symbol, origin -fulltag-symbol = -7: header
  ;; @-7, pname @1, vcell @9, fcell @17); misc-function-offset = -4
  ;; (= misc-data-offset since the fulltag-function removal, patch 0055).
  ;; DECIDE(ledger-ref): the general function-CALLING convention itself —
  ;; what a function object's slot 0 holds and where the entrypoint is on
  ;; Matt's design (his udf#0-prefixed code-vectors; no entrypoint slot,
  ;; PPC64-shaped).  Sequence below mirrors PPC64 (load fcell, load slot 0,
  ;; indirect call) and must be revisited when he ratifies the convention;
  ;; he may prefer a .SPjmpsym-style subprim here.
  `(progn
     (load-constant fname ,function-name)
     (ldur nfn (:@ fname (:$ arm64::symbol.fcell)))
     (ldur imm0 (:@ nfn (:$ arm64::misc-function-offset)))
     (blr imm0)))


;;; ===================================================================
;;; SECTION 7: Traps and type checks
;;; ===================================================================

;;; from ppc-lapmacros.lisp:396 (trap-unless-lisptag=)
;;; On PPC: extract-lisptag + trnei.  On arm64: extract + cmp + UUO.
;;; DECIDE(ledger-ref): need a type-error UUO that encodes the offending
;;; object register.  Matt's unary UUOs encode Rt.  Using brk placeholder.
(defarm64lapmacro trap-unless-lisptag= (node tag &optional (immreg 'imm0))
  (let ((ok (gensym "@")))
    `(progn
       (and ,immreg ,node (:$ arm64::tagmask))
       (cmp ,immreg (:$ ,tag))
       (b.eq ,ok)
       ;; DECIDE(ledger-ref): need a UUO for type errors that encodes the
       ;; offending object.  Matt's unary UUOs encode an Rt register.
       ;; Use uuo-error-reg-not-tag or brk #type-error pending his design.
       ;; #xf0fe: type-trap placeholder namespace moved OFF #xf000/#xf001 so
       ;; #xf0NN can uniformly mean NN = arch error code (uuo_interr family;
       ;; wave-3 convention).  #xfe/#xff exceed every arch error code.
       (uuo-error-reg-not-xtype ,node (:$ ,tag))
       ,ok)))


;;; trap-unless-fulltag= — x8664 lineage (x86-lapmacros trap-unless-fulltag=;
;;; Matt's tag model has no PPC analog for the 4-bit fulltag check).  Same
;;; brk-placeholder convention as trap-unless-lisptag= above.
(defarm64lapmacro trap-unless-fulltag= (node tag &optional (immreg 'imm0))
  (let ((ok (gensym "@")))
    `(progn
       (and ,immreg ,node (:$ arm64::fulltagmask))
       (cmp ,immreg (:$ ,tag))
       (b.eq ,ok)
       (uuo-error-reg-not-xtype ,node (:$ ,tag))
       ,ok)))


;;; from ppc-lapmacros.lisp:407 (trap-unless-typecode=)
(defarm64lapmacro trap-unless-typecode= (node tag &optional (immreg 'imm0))
  (let ((ok (gensym "@")))
    `(progn
       (extract-typecode ,immreg ,node)
       (cmp ,immreg (:$ ,tag))
       (b.eq ,ok)
       ;; DECIDE(ledger-ref): same UUO question as trap-unless-lisptag=
       ;; (#xf0ff: see trap-unless-lisptag= note — placeholder namespace)
       (uuo-error-reg-not-xtype ,node (:$ ,tag))
       ,ok)))


;;; ===================================================================
;;; SECTION 8: Floating-point operations
;;; ===================================================================

;;; from ppc-lapmacros.lisp:649 (get-single-float)
;;; RESOLVED (was DECIDE): Matt's commit 8e6a295 adds get-single-float-bits
;;; `(lsr dest node 32)` with the comment "a single-float is stored in top
;;; 32 bits" — singles ARE immediate, IEEE bits in [63:32], exactly the
;;; presumed packing.  His arm64-numbers.lisp also uses the (:w reg)
;;; operand syntax for W-width fmov ((fmov s0 (:w imm0)),
;;; %truncate-single-float->fixnum) — the former W/X-alias gap is closed.
(defarm64lapmacro get-single-float (dest src)
  ;; dest: FPR (s-register), src: GPR holding tagged single-float.
  ;; Bits extraction = upstream get-single-float-bits; then move to FPR.
  `(progn
     (get-single-float-bits imm0 ,src)
     (fmov ,dest (:w imm0))))


;;; from ppc-lapmacros.lisp:666 (put-single-float)
;;; Reverse of get-single-float: pack FPR bits back into a tagged immediate.
(defarm64lapmacro put-single-float (src dest)
  ;; src: FPR (s-register), dest: GPR to receive tagged single-float.
  ;; Inverse of get-single-float; packing RESOLVED by upstream
  ;; get-single-float-bits (bits in [63:32]); (:w) fmov per his
  ;; arm64-numbers.lisp usage ((fmov (:w imm0) (:s d0)), %ilogcount).
  `(progn
     (fmov (:w imm0) ,src)
     (lsl ,dest imm0 (:$ 32))
     (orr ,dest ,dest (:$ arm64::tag-single-float))))


;;; from ppc-lapmacros.lisp:675 (put-double-float)
;;; Store FPR into a double-float uvector.
(defarm64lapmacro put-double-float (src node)
  ;; src: FPR (d-register), node: tagged pointer to double-float uvector
  `(stur ,src (:@ ,node (:$ arm64::double-float.value))))


;;; from ppc-lapmacros.lisp: (int-to-freg — convert fixnum GPR to FPR)
;;; PPC: unbox-fixnum then use kernel routine or FP convert instruction.
;;; On arm64: asr to unbox, then scvtf.
(defarm64lapmacro int-to-freg (fpr gpr)
  ;; Converts a FIXNUM in gpr to a double in fpr
  `(progn
     (asr imm0 ,gpr (:$ arm64::fixnumshift))
     (scvtf ,fpr imm0)))


;;; from ppc-lapmacros.lisp:682 (clear-fpu-exceptions)
;;; PPC: mtfsf.  On arm64: write 0 to FPSR (FP status register) to clear
;;; exception flags.
(defarm64lapmacro clear-fpu-exceptions ()
  ;; DECIDE(ledger-ref): verify system register name for FPSR in Matt's
  ;; assembler.  ARM64 FPSR is system register S3_3_C4_C4_1.
  ;; His mrs/msr templates take :sysreg operands.
  `(msr :fpsr xzr))


;;; ===================================================================
;;; SECTION 9: Boolean results
;;; ===================================================================

;;; from ppc-lapmacros.lisp:623 (ne0->boolean: dest = T if src != 0, else NIL)
;;; On arm64: compare src to 0, then csel between nil and t.
(defarm64lapmacro ne0->boolean (dest src temp)
  (declare (ignore temp))
  (let ((is-t (gensym "@"))
        (done (gensym "@")))
    `(progn
       ;; t-offset = canonical-t-value - canonical-nil-value = 28 (#x1c)
       ;; (already defined as arm64::t-offset in arm64-arch.lisp)
       (cbnz ,src ,is-t)
       (mov ,dest rnil)
       (b ,done)
       ,is-t
       (add ,dest rnil (:$ arm64::t-offset))
       ,done)))


;;; from ppc-lapmacros.lisp:610 (eq0->boolean: dest = T if src == 0, else NIL)
(defarm64lapmacro eq0->boolean (dest src temp)
  (declare (ignore temp))
  (let ((is-t (gensym "@"))
        (done (gensym "@")))
    `(progn
       (cbz ,src ,is-t)
       (mov ,dest rnil)
       (b ,done)
       ,is-t
       (add ,dest rnil (:$ arm64::t-offset))
       ,done)))


;;; from ppc-lapmacros.lisp:639 (eq->boolean: dest = T if rx == ry, else NIL)
(defarm64lapmacro eq->boolean (dest rx ry temp)
  (declare (ignore temp))
  (let ((is-eq (gensym "@"))
        (done (gensym "@")))
    `(progn
       (cmp ,rx ,ry)
       (b.eq ,is-eq)
       (mov ,dest rnil)
       (b ,done)
       ,is-eq
       (add ,dest rnil (:$ arm64::t-offset))
       ,done)))


;;; ===================================================================
;;; SECTION 10: Unsigned-byte checking
;;; ===================================================================

;;; from ppc-lapmacros.lisp:584 (extract-unsigned-byte-bits.)
;;; PPC64: (rldicr. dest src (- 64 fixnumshift) (- 63 width)) — rotate the
;;; fixnum RIGHT by fixnumshift (tag bits land in the top 3 bits, the
;;; unboxed value in the low bits), clear the low `width` bits, set CR0
;;; from the result.  EQ afterwards iff src is a fixnum in [0, 2^width):
;;; any tag bit, sign bit, or excess-magnitude bit survives the mask.
;;; ARM64 faithful equivalent: ror (extr alias) + ands with the
;;; complement-of-low-width-bits logical immediate (a single ones-run,
;;; always encodable).  Callers test :eq exactly as on PPC.
(defarm64lapmacro extract-unsigned-byte-bits. (dest src width)
  `(progn
     (ror ,dest ,src (:$ arm64::fixnumshift))
     ;; logical immediates are UNSIGNED (Matt, 2026-07-11 mail); wrap
     ;; the mask into (byte 64 0) like the array.lisp sites.
     (ands ,dest ,dest (:$ (ldb (byte 64 0) (lognot (1- (ash 1 ,width))))))))


;;; ===================================================================
;;; SECTION 11: Character unboxing
;;; ===================================================================

;;; from ppc-lapmacros.lisp:535 (unbox-base-char)
;;; On PPC64: srdi dest, src, charcode-shift.  Optionally type-check.
;;; On arm64: subtag-character = (logior fulltag-imm-0 0) = 2.
;;; Character value = charcode in bits [15:8] (charcode-shift=8).
;;; Full character = (charcode << 8) | subtag-character.
(defarm64lapmacro unbox-base-char (dest src &optional crf)
  (if (null crf)
    `(lsr ,dest ,src (:$ arm64::charcode-shift))
    (let ((ok (gensym "@")))
      `(progn
         (and ,dest ,src (:$ arm64::subtag-mask))
         (cmp ,dest (:$ arm64::subtag-character))
         (lsr ,dest ,src (:$ arm64::charcode-shift))
         (b.eq ,ok)
         ;; DECIDE(ledger-ref): need type-error UUO for characters
         (uuo-error-reg-not-xtype ,src (:$ arm64::subtag-character))
         ,ok))))


;;; ===================================================================
;;; SECTION 12: u32-ref / u32-set
;;; ===================================================================

;;; from ppc-lapmacros.lisp:1074 (u32-ref — load 32-bit element at CONSTANT index)
;;; PPC: (lwz dest (+ (* 4 index) misc-data-offset) vector) — index is a
;;; compile-time constant, NOT a register.  Offset = 4*index + 4, always
;;; 4-aligned, so the scaled-12-bit ldr form encodes it.
;;; DECIDE(ledger-ref): 32-bit load needs a W-width destination; LAP register
;;; aliases are X-width (same W/X constraint as extract-subtag).  Needs either
;;; W-aliases (w0/imm0.w) in Matt's assembler or a 64-bit load + mask idiom.
;;; Written with the intended W-form pending that decision.
(defarm64lapmacro u32-ref (dest index vector)
  `(ldr ,dest (:@ ,vector (:$ (+ (* 4 ,index) arm64::misc-data-offset)))))


(defarm64lapmacro u32-set (new-value index vector)
  `(str ,new-value (:@ ,vector (:$ (+ (* 4 ,index) arm64::misc-data-offset)))))


;;; ===================================================================
;;; SECTION 13: Conditional (if)
;;; ===================================================================

;;; from ppc-lapmacros.lisp:252 (if — conditional execution based on CC)
;;; PPC uses CR-bit specifications.  On arm64 we use condition codes.
;;; The arm64 form: (if (:cc cond) then else) where cond is eq/ne/cs/cc/etc.
;;; DECIDE(ledger-ref): the PPC lap 'if' macro parses CR bit forms like
;;; (:cr0 :eq), (:cr1 :gt), etc.  ARM64 has no condition register file;
;;; conditions are checked directly after the setting instruction.
;;; The arm64 version takes a condition keyword: :eq, :ne, :lt, :gt, :le,
;;; :ge, :hi, :ls, :cs, :cc, :mi, :pl, :vs, :vc.
(defarm64lapmacro if (cond then &optional (else nil else-p))
  ;; cond should be a condition keyword like :eq, :ne, :lt, etc.
  ;; We negate it for the branch-over.
  (let ((false-label (gensym "@"))
        (cont-label (gensym "@")))
    (flet ((negate-cond (c)
             (ecase c
               (:eq :ne) (:ne :eq)
               (:cs :cc) (:cc :cs) (:hs :lo) (:lo :hs)
               (:mi :pl) (:pl :mi)
               (:vs :vc) (:vc :vs)
               (:hi :ls) (:ls :hi)
               (:ge :lt) (:lt :ge)
               (:gt :le) (:le :gt))))
      (let ((inv (negate-cond cond)))
        (if (not else-p)
          `(progn
             (,(intern (format nil "B.~a" inv) "CCL") ,false-label)
             ,then
             ,false-label)
          `(progn
             (,(intern (format nil "B.~a" inv) "CCL") ,false-label)
             ,then
             (b ,cont-label)
             ,false-label
             ,else
             ,cont-label))))))


;;; ===================================================================
;;; SECTION 14: Miscellaneous
;;; ===================================================================

;;; from ppc-lapmacros.lisp:473 (header-subtag[fixnum])
;;; Extract subtag from header as a fixnum.
(defarm64lapmacro header-subtag[fixnum] (dest vheader)
  `(progn
     (and ,dest ,vheader (:$ arm64::subtag-mask))
     (lsl ,dest ,dest (:$ arm64::fixnumshift))))


;;; from ppc-lapmacros.lisp:644 (repeat — emit N copies of an instruction)
(defarm64lapmacro repeat (n inst)
  (let ((insts ()))
    (dotimes (i n `(progn ,@(nreverse insts)))
      (push inst insts))))


;;; from ppc-lapmacros.lisp:269 (save-pc — on arm64, a no-op; lr is saved
;;; directly by stp in build-lisp-frame)
(defarm64lapmacro save-pc ()
  `(progn))


;;; from ppc-lapmacros.lisp:325 (restore-pc — on arm64, also a no-op;
;;; ret uses lr directly)
(defarm64lapmacro restore-pc ()
  `(progn))


;;; from ppc-lapmacros.lisp:235 (event-poll — check interrupt level)
(defarm64lapmacro event-poll ()
  ;; DECIDE(ledger-ref): the TCR field offsets for tlb-pointer.
  ;; From arm64-arch.lisp define-storage-layout tcr: tlb-pointer field.
  ;; Also need interrupt-level-binding-index = (ash 1 fixnumshift) = 8.
  `(progn
     (ldr imm0 (:@ rcontext (:$ arm64::tcr.tlb-pointer)))
     (ldr imm0 (:@ imm0 (:$ arm64::interrupt-level-binding-index)))
     (cmp imm0 (:$ 0))
     ;; DECIDE(ledger-ref): what trap/UUO to use for interrupt polling?
     ;; PPC uses tdgti nargs 0.  On arm64, a conditional uuo or subprim call.
     ))


;;; ===================================================================
;;; uuo-interr — emit side of the PROPOSED uuo_interr misc-format
;;; extension (kernel decode: arm64-exceptions.c UUO_MISC_IS_INTERR,
;;; implemented incl. the PPC:1387-1419 kernel services; ratify item).
;;; udf #imm16, imm16 = ((1<<13) | (errnum<<5) | reg) << 2 | format-misc(0).
;;; errnum = arch::error-* constant; reg = GPR NUMBER (default 0 = PPC's
;;; rzero; the tcr-service cases take their target from arg_z, not reg).
;;; ===================================================================
(defarm64lapmacro uuo-interr (errnum &optional (reg 0))
  `(udf (:$ (ash (logior (ash 1 13) (ash ,errnum 5) ,reg) 2))))


(provide "ARM64-LAPMACROS")
