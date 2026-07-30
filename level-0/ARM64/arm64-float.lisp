;;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; arm64-float.lisp — ACTIVE seed (promoted from drafts wave-6 at the
;;; 16m5p %INT-TO-SFLOAT demand; pin-gate sweep applied — see the
;;; PIN-GATED CLUSTER banner).
;;;;
;;;; arm64-float.lisp — wave-6 draft: level-0 float LAP functions for
;;;; Matt Emerson's upstream arm64 (low-tag) design, pin d71a5ad.
;;;;
;;;; Source: vendor/ccl/level-0/PPC/ppc-float.lisp (PPC64/unguarded forms
;;;; only; every form's feature guard re-verified against the source —
;;;; see wave6-float-report.md ledger).
;;;; Macro vocabulary: Matt's arm64-lapmacros.lisp (get-double-float,
;;;; get-single-float-bits, unbox-fixnum, box-fixnum, check-nargs) plus
;;;; upstream-port/level-0/drafts/arm64-lapmacros-additions.lisp
;;;; (get/put-single-float, put-double-float, int-to-freg,
;;;; clear-fpu-exceptions, macptr-ptr, vpush/vpop).
;;;;
;;;; Representation ground truth (his arm64-arch.lisp @ d71a5ad):
;;;;   * single-floats are IMMEDIATE: tag-single-float = #b001 (:47),
;;;;     fulltag-single-float = #b0001 (:57), subtag-single-float =
;;;;     fulltag-single-float = 1 (:78), :single-float-tag-is-subtag nil
;;;;     (:951).  IEEE bits live in the TOP 32 bits of the tagged word —
;;;;     his own get-single-float-bits is `(lsr dest node (:$ 32))`
;;;;     (arm64-lapmacros.lisp:49-51) — SAME packing as PPC64, so the
;;;;     PPC64 single-float shift arithmetic ports line-for-line.
;;;;     (MISSING-CONSTANTS-RATIFY.md §10.1 tracks ratification.)
;;;;   * double-floats are uvectors: subtag-double-float (:144),
;;;;     double-float.value = misc-data-offset = 4 (:432) — misc-biased,
;;;;     NOT an 8-multiple, so all value accesses are ldur/stur.
;;;;     LE cell map: value-cell=0 ALIASES val-low-cell=0; val-high-cell=1
;;;;     (:433-437).
;;;;   * FP register convention (this file): PPC fpN → dN (double) / sN
;;;;     (single), SAME number: fp0→d0/s0, fp1→d1/s1, fp2→d2/s2.
;;;;     get-single-float / put-single-float / int-to-freg stage through
;;;;     imm0 (additions macros) — noted where imm0 is otherwise live.
;;;;   * FP-EXCEPTION MODEL = ARM32/VFP lineage, NOT PPC (W6-D40):
;;;;     ARMv8 FPCR trap-enable bits (IOE..IDE, bits 8-15) are OPTIONAL
;;;;     and RAZ/WI on mainstream cores — untrapped FP is the baseline,
;;;;     exactly the NEON situation vendor arm-float.lisp:384-390 solved.
;;;;     Adopted model: the LOGICAL exception-enable mask lives in
;;;;     SOFTWARE (bits 8-15 of *fp-logical-enables*, a special variable
;;;;     and therefore per-thread); the hardware FPCR carries only the
;;;;     rounding mode (RMode, bits 22-23); cumulative status comes from
;;;;     tcr.foreign-fpsr, the FPSR the ffcall spentry captures and clears
;;;;     around each foreign call (arm64-spentry.s:264-266).
;;;;     mrs/msr fpsr/fpcr templates + names verified at tip
;;;;     (arm64-asm.lisp:577-578, :1440-1443; resolves
;;;;     MISSING-CONSTANTS-RATIFY.md §10.6's open question).
;;;;     ⚠ UPDATED 16m44.  This block used to say the mask lives in
;;;;     tcr.lisp-fpscr at offset 16 with tcr.ffi-exception at 136, and
;;;;     flagged a W4-D20 disagreement with arm64-constants.h.  Both slots
;;;;     were REMOVED upstream at f067047; only tcr.foreign-fpsr survives,
;;;;     and lisp/kernel now agree on it.  The disagreement is closed and
;;;;     the two offsets above are gone — do not reinstate them.
;;;;
;;;; STATUS: DRAFT — not compiled; brk placeholders: none (this file needs
;;;; no subprims, no lisp frames, no constant-pool refs, no type-trap UUOs).

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")
  (require :number-case-macro)
  (require "ARM64-LAPMACROS"))

;;; from ppc-float.lisp:48 (#+ppc64-target %make-float-from-fixnums)
;;; make a float from hi - high 24 bits mantissa (ignore implied higher bit)
;;;                   lo -  low 28 bits mantissa
;;;                   exp  - take low 11 bits
;;;                   sign - sign(sign) => result
;;; no error checks, no tweaks, no nuthin.
;;; PPC64 assembles the two 32-bit halves and stw's them separately (BE);
;;; here the whole 64-bit IEEE word is built in imm1 and stored once (LE).
;;; PPC64's sign extraction is `rlwinm imm0 sign 0 0 0` (keep bit 31 of the
;;; low word of the BOXED sign — works for small ±fixnums); the intent per
;;; the header comment is sign(sign), expressed here as asr #63 of the boxed
;;; fixnum (negative fixnums have bit 63 set), same trick for any magnitude.
;;; NB: like PPC, does NOT move the float into arg_z — callers
;;; (l0-float.lisp) keep their own reference and ignore the return value.
(defarm64lapfunction %make-float-from-fixnums ((float 8) (hi 0) (lo arg_x) (exp arg_y) (sign arg_z))
  (ldr imm2 (:@ vsp (:$ hi)))           ; boxed hi (vstack)
  (ldr temp0 (:@ vsp (:$ float)))       ; the double-float node (vstack)
  (add vsp vsp (:$ 16))                 ; ppc:59 (la vsp '2 vsp)
  (unbox-fixnum imm2 imm2)
  (and imm2 imm2 (:$ #xffffff))         ; hi: 24 mantissa bits
  (lsl imm2 imm2 (:$ 28))               ; → IEEE bits 28..51
  (unbox-fixnum imm1 lo)
  (and imm1 imm1 (:$ #xfffffff))        ; lo: 28 mantissa bits → bits 0..27
  (orr imm1 imm1 imm2)
  (unbox-fixnum imm0 exp)
  (and imm0 imm0 (:$ #x7ff))            ; exp: low 11 bits (ppc:50 keeps 11)
  (lsl imm0 imm0 (:$ 52))               ; = IEEE-double-float-exponent-offset
  (orr imm1 imm1 imm0)
  (asr imm0 sign (:$ 63))               ; 0 (sign>=0) or -1 (sign<0)
  (lsl imm0 imm0 (:$ 63))               ; → IEEE sign bit
  (orr imm1 imm1 imm0)
  (stur imm1 (:@ temp0 (:$ arm64::double-float.value)))  ; offset 4 → stur
  (ret))

;;; from ppc-float.lisp:72 (unguarded)
(defarm64lapfunction %%double-float-abs! ((n arg_y) (val arg_z))
  (get-double-float d1 n)               ; upstream macro: ldur d1, [n, #4]
  (fabs d1 d1)
  (put-double-float d1 val)
  (ret))

;;; from ppc-float.lisp:86 (#+ppc64-target %short-float-abs; the
;;; destructive #+ppc32 %%short-float-abs! at :79 is SKIPped)
(defarm64lapfunction %short-float-abs ((n arg_z))
  (get-single-float s1 n)               ; stages via imm0
  (fabs s0 s1)
  (put-single-float s0 arg_z)           ; stages via imm0
  (ret))

;;; from ppc-float.lisp:92 (unguarded)
(defarm64lapfunction %double-float-negate! ((src arg_y) (res arg_z))
  (get-double-float d0 src)
  (fneg d1 d0)
  (put-double-float d1 res)
  (ret))

;;; from ppc-float.lisp:107 (#+ppc64-target, non-destructive)
(defarm64lapfunction %short-float-negate ((src arg_z))
  (get-single-float s0 src)
  (fneg s1 s0)
  (put-single-float s1 arg_z)
  (ret))

;;; from ppc-float.lisp:160 (unguarded; body is 32-bit-halves code)
;;; PPC counts leading zeros of the top-justified 20-bit significand-high
;;; word, falling through to 20+clz32(low word).  On a 64-bit LE load this
;;; collapses to clz64(value << 12): for ANY nonzero significand the two
;;; agree exactly; for an all-zero significand PPC yields 20+32 = 52 while
;;; clz64 yields 64 — clamp to 52 to stay value-identical (3 extra insns;
;;; only ±0.0/±inf hit it).
(defarm64lapfunction dfloat-significand-zeros ((dfloat arg_z))
  (ldur imm1 (:@ dfloat (:$ arm64::double-float.value)))  ; 64-bit IEEE word
  (lsl imm1 imm1 (:$ 12))               ; drop sign+exp: (1+ IEEE-double-float-exponent-width)
  (clz imm1 imm1)
  (mov imm0 (:$ 52))
  (cmp imm1 (:$ 52))
  (csel imm1 imm1 imm0 (:? ls))         ; 64 (zero significand) → 52, as PPC
  (box-fixnum arg_z imm1)
  (ret))

;;; from ppc-float.lisp:174 (unguarded; #+ppc64-target body arm
;;; `srdi imm1 sfloat 32` = his get-single-float-bits)
;;; PPC then counts within a 32-bit word (rlwinm 9 0 22 + cntlzw); done
;;; here with W-register forms so the mantissa==0 case gives 32 exactly
;;; as cntlzw does — bit-identical, no clamp needed.
(defarm64lapfunction sfloat-significand-zeros ((sfloat arg_z))
  (get-single-float-bits imm1 sfloat)   ; upstream macro: lsr #32
  (lsl (:w imm1) (:w imm1) (:$ 9))      ; drop sign+exp: (- 32 IEEE-single-float-mantissa-width)
  (clz (:w imm1) (:w imm1))             ; W clz = cntlzw semantics
  (box-fixnum arg_z imm1)
  (ret))

;;; from ppc-float.lisp:210 (#+ppc64-target %%scale-dfloat!)
;;; PPC builds the scale factor 2^(int-bias) by storing (unboxed int)<<20
;;; as the high word of a double in tsp scratch memory and reloading it;
;;; here the identical bit pattern is (unboxed int)<<52 moved to an FPR
;;; with fmov — no scratch memory, no tsp.  As on PPC, the exponent is
;;; NOT biased here (callers pass the biased value; ppc:223 comment).
(defarm64lapfunction %%scale-dfloat! ((float arg_x) (int arg_y) (result arg_z))
  (clear-fpu-exceptions)                ; additions macro: msr fpsr, xzr
  (get-double-float d0 float)
  (unbox-fixnum imm0 int)
  (lsl imm0 imm0 (:$ 52))               ; = IEEE-double-float-exponent-offset
  (fmov d1 imm0)                        ; raw bits → double 2^int
  (fmul d2 d0 d1)
  (put-double-float d2 result)
  (ret))

;;; from ppc-float.lisp:252 (#+ppc64-target %%scale-sfloat!, 2-arg form)
(defarm64lapfunction %%scale-sfloat! ((float arg_y) (int arg_z))
  (clear-fpu-exceptions)
  (get-single-float s0 float)           ; stages via imm0 — before int unbox
  (unbox-fixnum imm0 int)
  (lsl imm0 imm0 (:$ IEEE-single-float-exponent-offset))  ; <<23
  (fmov s1 (:w imm0))                   ; raw bits → single 2^int
  (fmul s2 s0 s1)
  (put-single-float s2 arg_z)
  (ret))

;;; from ppc-float.lisp:267 (unguarded)
(defarm64lapfunction %copy-double-float ((f1 arg_y) (f2 arg_z))
  (get-double-float d0 f1)
  (put-double-float d0 f2)
  (ret))

;;; from ppc-float.lisp:312 (unguarded)
;;; PPC's put-double-float of an lfs-loaded single relies on lfs's
;;; load-time widening; ARM64 needs the explicit fcvt (cf. vendor
;;; arm-float.lisp:234-238 %short-float->double-float, same shape).
(defarm64lapfunction %short-float->double-float ((src arg_y) (result arg_z))
  (get-single-float s0 src)
  (fcvt d1 s0)                          ; single → double
  (put-double-float d1 result)
  (ret))

;;; from ppc-float.lisp:326 (#+ppc64-target, 1-arg form)
(defarm64lapfunction %double-float->short-float ((src arg_z))
  ;;(clear-fpu-exceptions)              ; commented out in PPC (ppc:327) — kept as-is
  (get-double-float d0 src)
  (fcvt s1 d0)                          ; double → single (rounds; PPC frsp)
  (put-single-float s1 arg_z)
  (ret))

;;; from ppc-float.lisp:343 (#+ppc64-target %int-to-sfloat)
;;; PPC64 rounds int→double (int-to-freg) then double→single (frsp), and
;;; materializes the tagged immediate via a pre-seeded 8-byte scratch slot
;;; tcr.single-float-convert (stfs into it + ld the whole word).  The fmov
;;; path (put-single-float) makes the scratch slot unnecessary — no tcr
;;; field needed.  The two-step int→double→single rounding is kept
;;; PPC-faithful (a direct scvtf-to-single would round once, differing for
;;; magnitudes ≥ 2^53).
(defarm64lapfunction %int-to-sfloat ((int arg_z))
  (int-to-freg d0 int)                  ; additions macro: asr imm0 + scvtf d0
  (fcvt s1 d0)                          ; PPC frsp fp1 fp0
  (put-single-float s1 arg_z)
  (ret))

;;; from ppc-float.lisp:351 (unguarded)
(defarm64lapfunction %int-to-dfloat ((int arg_y) (dfloat arg_z))
  (int-to-freg d0 int)
  (put-double-float d0 dfloat)
  (ret))

;;; ===================================================================
;;; FPSCR family — W6-D40: ARM32/VFP model, NOT a PPC line-port.
;;; PPC's FPSCR is one trapping control+status register (mffs/mtfsf via an
;;; FPR + memory).  ARM64 splits it into FPCR (control) + FPSR (status),
;;; readable directly into GPRs with mrs/msr — no FPR/memory staging, no
;;; scratch TCR slots.  Word layouts:
;;;   status (FPSR):  IOC=0 DZC=1 OFC=2 UFC=3 IXC=4 IDC=7
;;;   control (FPCR): IOE=8 DZE=9 OFE=10 UFE=11 IXE=12 IDE=15
;;;                   + RMode=(byte 2 22)
;;;   RMode: 0=nearest 1=+inf 2=-inf 3=zero (≠ PPC's 0=nearest 1=zero …)
;;; The enable bits sit uniformly 8 above their status twins, which is what
;;; makes enabled-and-occurred a single shift-and-AND everywhere below.
;;;
;;; ⚠ SUPERSEDED 16m44: this block used to say the LOGICAL enable mask
;;; lives in tcr.lisp-fpscr bits 8-15 (the W6-D40 model, following vendor
;;; arm-float.lisp:384-390).  Matt removed tcr.lisp-fpscr and
;;; tcr.ffi-exception at f067047, so no such slot exists on this lane and
;;; the whole cluster that depended on it had been compiled out.  Enables
;;; now live in hardware FPCR and the captured post-ff-call status in
;;; tcr.foreign-fpsr — see the cluster header below for the full rationale
;;; and the RAZ/WI caveat.
;;; ===================================================================


;;; =====================================================================
;;; FORMERLY PIN-GATED CLUSTER — UN-GATED 16m44.
;;;
;;; History: Matt removed tcr.lisp-fpscr and tcr.ffi-exception at f067047
;;; (only tcr.foreign-fpsr survives; lisp runs with the process-default
;;; FPCR per the w13 ffcall design).  These five forms had been written
;;; against the REMOVED slots and were gated on #+arm64-lisp-fpscr-slot —
;;; a feature defined NOWHERE in this repo or Matt's tree — so the reader
;;; discarded them and the file still compiled clean.
;;;
;;; That cost ~190 ANSI failures.  l1-numbers.lisp calls
;;; (%ffi-exception-status) on EVERY sin/cos/tan/asin/acos/atan/sinh/cosh/
;;; tanh/asinh/acosh/atanh/exp/log/expt, so the whole transcendental family
;;; died with "Undefined function CCL::%FFI-EXCEPTION-STATUS" — measured
;;; 16m44, and it is why SIN.1-24/COS.1-24/TAN.1-24 were red.  Nothing in
;;; boot or cold-load calls sin, which is why it hid until the suite tail
;;; first executed.
;;;
;;; THE RE-PORT.  Same W6-D40 formula, sourced from what the lane HAS:
;;;   * enables  — hardware FPCR bits 8-15, not a software TCR word.
;;;   * status   — tcr.foreign-fpsr, which our spentry publishes after
;;;                every ff-call (arm64-spentry.s:265, spentry-E-ffi.s:337).
;;; AArch64 keeps the trap-enable bits 8 above their FPSR flag twins
;;; (IOE8/IOC0, DZE9/DZC1, OFE10/OFC2, UFE11/UFC3, IXE12/IXC4, IDE15/IDC7),
;;; so enabled-and-occurred = flags AND (enables>>8) — one shift and one
;;; AND, exactly the ARM32 idiom (arm-float.lisp:270-280), with FPCR
;;; standing in for ARM32's tcr.lisp-fpscr.
;;;
;;; ⚠ WHERE THE ENABLES LIVE — and why NOT in hardware FPCR.
;;; AArch64's FPCR trap-enable bits are OPTIONAL and are RAZ/WI on every
;;; implementation without trapped-FP support (Graviton included): you write
;;; them, they read back 0.  Sourcing the enables from FPCR therefore makes
;;; %ffi-exception-status unconditionally NIL and NO floating-point condition
;;; can ever be signalled.  Measured cost when this file did that: the eight
;;; EXP.ERROR.4-7 / EXPT.ERROR.4-7 tests, which assert that
;;;   (exp (+ (log most-positive-single-float) 100))
;;; signals FLOATING-POINT-OVERFLOW.
;;;
;;; Hardware traps are also the wrong mechanism here regardless of support:
;;; the exception happens inside libm, during a foreign call.  The ffcall
;;; spentry already captures the callee's cumulative FPSR into
;;; tcr.foreign_fpsr and then zeroes FPSR (arm64-spentry.s:264-266), so the
;;; check is a pure software AND against the LOGICAL enables afterwards --
;;; which is exactly the ARM32 model (arm-float.lisp:384-390) and the reason
;;; ARM32 keeps its enables in tcr.lisp-fpscr rather than in the FPSCR.
;;;
;;; arm64 has no such TCR slot (removed at f067047) and adding one would
;;; shift every following slot and the sptab in Matt's layout.  A special
;;; variable gives the same PER-THREAD granularity the TCR slot gave ARM32,
;;; costs no layout change, and keeps the whole thing upstream-friendly.
;;; Only the rounding mode -- which FPCR really does hold -- stays hardware.
;;; MAIL ITEM.
;;; =====================================================================

;;; ⚠ The defvar for *fp-logical-enables* and the three defuns that use it
;;; live at the END of this file, beside *rounding-mode-alist*, NOT here.
;;; Placing the defvar at this point in the file made cold load die with
;;; "FATAL (cold load, no lisp error system): undefined function %DEFVAR"
;;; (measured 16m44, image e53da35a).  Only the LAP helpers are here.
;;; from ppc-float.lisp:360 (%get-fpscr-control — PPC: low 8 FPSCR bits)
;;; Hardware half only: the rounding mode, FPCR (byte 2 22).
(defarm64lapfunction %get-fpcr-rmode ()
  (mrs imm0 fpcr)
  (and imm0 imm0 (:$ (ash 3 22)))
  (box-fixnum arg_z imm0)
  (ret))

;;; ldb-wrap on the lognot: the encoder rejects negative logimms.
(defarm64lapfunction %set-fpcr-rmode ((new arg_z))
  (unbox-fixnum imm0 new)
  (and imm0 imm0 (:$ (ash 3 22)))                             ; new RMode
  (mrs imm1 fpcr)
  (and imm1 imm1 (:$ (ldb (byte 64 0) (lognot (ash 3 22)))))  ; clear old
  (orr imm0 imm0 imm1)
  (msr fpcr imm0)
  (ret))

;;; The FPSR flag byte captured by the ffcall spentry, consumed and cleared
;;; on read — x86-64's %get-post-ffi-mxcsr shape (x86-float.lisp:201).
;;;
;;; ⚠ CORRECTED 16m48b.  This comment used to say: "the spentry does
;;; `msr fpsr, xzr' immediately AFTER capturing, so the slot carries THIS
;;; call's flags only and cannot accumulate across calls."  That does not
;;; follow, and it was the bug.  FPSR is CUMULATIVE: zeroing it after the
;;; capture makes the slot carry everything raised since the PREVIOUS
;;; ff-call, which includes all the inline lisp float arithmetic in between.
;;; The parenthetical the old comment added — "a sticky source here would
;;; make every transcendental after the first overflow report an overflow" —
;;; was an accurate description of the behaviour the code actually had.
;;; Measured: clear FPSR, do `(* most-positive-single-float
;;; most-positive-single-float)' inline (FPSR := #x14), then `(log 2.0d0)'
;;; signalled FLOATING-POINT-OVERFLOW on (2.0D0); the next identical call
;;; returned 0.693....  log/exp/sin/atan all reproduced it.
;;;
;;; The spentry now clears FPSR immediately BEFORE the `blr' as well, so the
;;; window really is the callee's (arm64-spentry.s `spentry ffcall',
;;; spentry-E-ffi.s `spentry ffcall_return_registers').  That is the arm64
;;; equivalent of ARM32's per-call-site `#+arm-target (%set-fpscr-status 0)'
;;; (33 sites in level-1/l1-numbers.lisp) — ARM32 needs it at every site
;;; because it has no tcr.foreign_fpsr and reads the live FPSCR after the
;;; call; we own the slot, so we clear at the one seam that owns it.
(defarm64lapfunction %get-post-ffi-fpsr ()
  (ldr imm0 (:@ rcontext (:$ arm64::tcr.foreign-fpsr)))
  (mov imm1 (:$ 0))
  (str imm1 (:@ rcontext (:$ arm64::tcr.foreign-fpsr)))
  (and imm0 imm0 (:$ #xff))
  (box-fixnum arg_z imm0)
  (ret))

;;; from ppc-float.lisp:368 (%get-fpscr-status — cumulative exception flags)
;;; ARM32 shape (arm-float.lisp:402-406).  NB the PPC original loads from
;;; `tsp` (ppc:371) — a latent bug in the vendor source (should be
;;; rcontext); moot here, no memory staging at all.
(defarm64lapfunction %get-fpscr-status ()
  (mrs imm0 fpsr)
  (and imm0 imm0 (:$ #xff))
  (box-fixnum arg_z imm0)
  (ret))

;;; from ppc-float.lisp:377 (%set-fpscr-status — set/clear cumulative flags)
;;; ARM32 shape (arm-float.lisp:409-416): read-modify-write the flag byte,
;;; preserving FPSR's non-flag bits (QC bit 27, AArch32-compat NZCV 28-31).
(defarm64lapfunction %set-fpscr-status ((new arg_z))
  (mrs imm1 fpsr)
  (unbox-fixnum imm0 new)
  (and imm0 imm0 (:$ #xff))
  (and imm1 imm1 (:$ (ldb (byte 64 0) (lognot #xff))))  ; ldb-wrap (encoder rejects negative logimms)
  (orr imm0 imm0 imm1)
  (msr fpsr imm0)
  (ret))

;;; from ppc-float.lisp:394 (%ffi-exception-status)
;;; PPC replays the FPSCR captured at ff-call return (tcr.ffi-exception)
;;; through CR logic to compute the FEX summary of enabled-and-occurred
;;; exceptions.  ARM64: FPCR enable bits sit uniformly 8 above their FPSR
;;; flags (IOE 8/IOC 0 … IDE 15/IDC 7), so enabled-and-occurred =
;;; flags AND (enables>>8) — the ARM32 idiom (arm-float.lisp:270-280),
;;; masked against the SOFTWARE enables in *fp-logical-enables*.
;;;
;;; ⚠ NOT the hardware FPCR, and this is not an optimisation.  CCL's ARM
;;; port deliberately never puts enable bits in the FPU at all — see the
;;; comment above %get-fpscr-control in arm-float.lisp ("the NEON doesn't
;;; support traps on FP exceptions ... we keep the (logical) enabled
;;; exception mask in tcr.lisp-fpscr, and just store the rounding mode in
;;; the hardware FPSCR").  On Graviton the FPCR enable bits are RAZ/WI on
;;; top of that, so a mask taken from hardware would read 0 and this
;;; function would silently return NIL forever — i.e. every FP condition
;;; would vanish and the only symptom would be missing errors.
;;; See comms/EXPT-ERROR-FP-OVERFLOW-FINDING.md.
;;; Returns NIL when nothing fired, else the offending flag bits (ARM32
;;; contract; consumed by the %*-check-exception-* defuns below, which test
;;; it with WHEN).  Reads the CAPTURED copy per the ffcall design
;;; (FPSR → tcr.foreign_fpsr, arm64-spentry.s:265 / spentry-E-ffi.s:337).
;;;
;;; enabled-and-occurred = flags AND (enables >> 8): AArch64 keeps each
;;; enable exactly 8 bits above its status twin (IOE8/IOC0, DZE9/DZC1,
;;; OFE10/OFC2, UFE11/UFC3, IXE12/IXC4), so the shift lines them up.
;;; DEFINED AT THE END OF THIS FILE, with the defvar it reads.

;;; from ppc-float.lisp:418-464 (%df/%sf-check-exception-2/-1)
;;; ARM32 shape (arm-float.lisp:282-313): fp-status is NIL or the boxed
;;; enabled-and-occurred FPSR flag bits (see %ffi-exception-status /
;;; %get-fpscr-status + logical mask).  Double operands heap-consed as on
;;; PPC; single operands passed directly (immediates on this 64-bit
;;; design — the vendor #+ppc64-target arms do the same, ppc:439/442/464).
(defun %df-check-exception-2 (operation op0 op1 fp-status)
  (when fp-status
    (let* ((condition-name (fp-condition-name-from-fpscr-status fp-status)))
      (error (make-instance (or condition-name 'arithmetic-error)
                            :operation operation
                            :operands (list (%copy-double-float op0 (%make-dfloat))
                                            (%copy-double-float op1 (%make-dfloat))))))))

(defun %sf-check-exception-2 (operation op0 op1 fp-status)
  (when fp-status
    (let* ((condition-name (fp-condition-name-from-fpscr-status fp-status)))
      (error (make-instance (or condition-name 'arithmetic-error)
                            :operation operation
                            :operands (list op0 op1))))))

(defun %df-check-exception-1 (operation op0 fp-status)
  (when fp-status
    (let* ((condition-name (fp-condition-name-from-fpscr-status fp-status)))
      (error (make-instance (or condition-name 'arithmetic-error)
                            :operation operation
                            :operands (list (%copy-double-float op0 (%make-dfloat))))))))

(defun %sf-check-exception-1 (operation op0 fp-status)
  (when fp-status
    (let* ((condition-name (fp-condition-name-from-fpscr-status fp-status)))
      (error (make-instance (or condition-name 'arithmetic-error)
                            :operation operation
                            :operands (list op0))))))

;;; Replaces ppc-float.lisp:467 (fp-condition-from-fpscr) + :487
;;; (%fp-error-from-status): with untrapped FP there is no separate
;;; control-bit check at condition-mapping time (the mask was applied in
;;; %ffi-exception-status / the inline status check).  ARM32 shape
;;; (arm-float.lisp:441-447); also what an arm64-error-signal.lisp will
;;; call (cf. vendor level-1/arm-error-signal.lisp:290).  FPSR bits,
;;; W6-D41 proposes arm64::fpsr-* names.
;;; ppc:497's fp-minor-opcode-operation NOT carried — PPC-instruction-
;;; opcode keyed; only consumer is level-1/ppc-error-signal.lisp.
(defun fp-condition-name-from-fpscr-status (status)
  (cond
    ((logbitp 0 status) 'floating-point-invalid-operation) ; FPSR.IOC
    ((logbitp 1 status) 'division-by-zero)                 ; FPSR.DZC
    ((logbitp 2 status) 'floating-point-overflow)          ; FPSR.OFC
    ((logbitp 3 status) 'floating-point-underflow)         ; FPSR.UFC
    ((logbitp 4 status) 'floating-point-inexact)))         ; FPSR.IXC

;;; from ppc-float.lisp:507 (unguarded; "Don't we already have about 20
;;; versions of this ?")
(defarm64lapfunction %double-float-from-macptr! ((ptr arg_x) (byte-offset arg_y) (dest arg_z))
  (macptr-ptr imm0 ptr)
  (unbox-fixnum imm1 byte-offset)
  (ldr d1 (:@ imm0 imm1))               ; lfdx → register-offset FP load
  (put-double-float d1 dest)
  (ret))

;;; from ppc-float.lisp:515 (*rounding-mode-alist*)
;;; ARM64 FPCR.RMode encoding: 0=RN(nearest) 1=RP(+inf) 2=RM(-inf) 3=RZ(zero)
;;; — NOT PPC's (0=nearest 1=zero 2=+inf 3=-inf).  Same alist as ARM32
;;; (arm-float.lisp:315-316, VFP = same encoding).
(defvar *rounding-mode-alist*
  '((:nearest . 0) (:positive . 1) (:negative . 2) (:zero . 3)))

;;; ===================================================================
;;; THE SOFTWARE HALF OF THE FPSCR MODEL.  See the cluster header above
;;; for WHY the logical enables cannot live in hardware FPCR.
;;;
;;; ⚠ POSITION IS LOAD-ORDER-SENSITIVE, not stylistic.  These four forms
;;; sat next to their LAP helpers ~150 lines up, and cold load died with
;;;   FATAL (cold load, no lisp error system): undefined function %DEFVAR
;;; (measured 16m44, image e53da35a).  %defvar is a level-1 function
;;; (l1-utils.lisp) that nfasload's defvar/defparameter fasl ops call, so
;;; a defvar is only safe at a point in cold load where it already exists.
;;; *rounding-mode-alist* directly above is the proven-good position in
;;; this file.  Keep this block here, and put new defvars beside it —
;;; NOT beside the code that uses them.
;;; ===================================================================

;;; Default matches every other CCL port's initial fpu mode:
;;; invalid + division-by-zero + overflow enabled, underflow/inexact not.
(defvar *fp-logical-enables* #x0700)    ; IOE(8) | DZE(9) | OFE(10)

(defun %get-fpscr-control ()
  (logior (logand #xff00 *fp-logical-enables*) (%get-fpcr-rmode)))

(defun %set-fpscr-control (new)
  (setq *fp-logical-enables* (logand #xff00 new))
  (%set-fpcr-rmode new)
  new)

;;; from ppc-float.lisp:394 (%ffi-exception-status)
;;; enabled-and-occurred = captured flags AND (enables >> 8).  Returns NIL
;;; when nothing fired, else the offending flag bits — the ARM32 contract
;;; (arm-float.lisp:270-280); the %*-check-exception-* defuns test it with
;;; WHEN.
(defun %ffi-exception-status ()
  (let* ((hit (logand (%get-post-ffi-fpsr)
                      (ash (logand #xff00 *fp-logical-enables*) -8))))
    (unless (eql 0 hit) hit)))

;;; ---------------------------------------------------------------------
;;; INLINE-arithmetic exception checkpoints.
;;;
;;; The FFI path above is not enough.  PPC64 and x86-64 catch an overflow in
;;; an INLINE fp multiply with a hardware trap (PPC's %set-fpscr-control does
;;; `mtfsf #xff', putting the enable bits in the real FPSCR, and
;;; ppc-exceptions.c:1297 fields the SIGFPE).  AArch64 defines the same
;;; enables — FPCR.IOE/DZE/OFE/UFE/IXE — but implementing them is OPTIONAL,
;;; and they are RAZ/WI on this part.  MEASURED on Neoverse N1 (CPU part
;;; 0xd0c): wrote 0x1f00 to FPCR, read back 0x0.  So there is no trap to take
;;; and the only way to see the exception is to POLL the cumulative flags.
;;;
;;; This is the fallback arm of the intended design, NOT a statement that
;;; "arm64 has no fp traps" — other AArch64 implementations may implement
;;; them, in which case the right answer is a startup capability probe plus
;;; a line-port of ppc-exceptions.c's SIGFPE path, and these checkpoints
;;; become dead weight on that hardware.  See
;;; comms/EXPT-ERROR-FP-OVERFLOW-FINDING.md.
;;;
;;; MEASURED, so that the next reader does not re-derive it: the cumulative
;;; FPSR flags DO survive to a Lisp-level read — after a compiled multiply,
;;; after the result is boxed, and under the interpreter (all three read
;;; #x14 = OFC|IXC, against #x0 for a control with no arithmetic).  A poll
;;; does NOT have to be adjacent to the operation.  One earlier observation
;;; read #x0, and the only difference was a signalled error immediately
;;; before it, so the unwind path is the suspect; poll BEFORE signalling
;;; anything, which the callers below do by construction.
;;;
;;; The flags are STICKY, so a checkpoint must clear before the operation or
;;; it will report an exception raised by unrelated earlier arithmetic.  That
;;; clear is the same one ARM32 does before each of its FFI ops
;;; (`#+arm-target (%set-fpscr-status 0)', l1-numbers.lisp).

(defun %fp-begin-inline-check ()
  "Clear the cumulative FPSR flags so a following %FP-INLINE-EXCEPTION-STATUS
reports only what the operation in between raised."
  (%set-fpscr-status 0)
  nil)

(defun %fp-inline-exception-status ()
  "Enabled-and-occurred flags from the LIVE FPSR, or NIL.  The inline-
arithmetic twin of %FFI-EXCEPTION-STATUS, which reads the ffcall spentry's
captured copy and would return 0 here."
  (let* ((hit (logand (%get-fpscr-status)
                      (ash (logand #xff00 *fp-logical-enables*) -8))))
    (unless (eql 0 hit) hit)))

(defun %fp-check-inline-exception (operation operands status)
  "Signal the fp condition named by STATUS, if any.  Unlike the
%*-check-exception-N quartet this takes OPERANDS as a ready-made list and
copies nothing: an integer-exponent EXPT has operands (float integer), so
the double-float coercion those do would be wrong here."
  (when status
    (error (make-instance (or (fp-condition-name-from-fpscr-status status)
                              'arithmetic-error)
                          :operation operation
                          :operands operands))))

;;; from ppc-float.lisp:518 (get-fpu-mode) — ARM32 shape
;;; (arm-float.lisp:318-341); control word per W6-D40/41: RMode (byte 2 22),
;;; logical enables IOE=8 DZE=9 OFE=10 UFE=11 IXE=12.
(defun get-fpu-mode (&optional (mode nil mode-p))
  (let* ((flags (%get-fpscr-control)))
    (declare (fixnum flags))
    (let* ((rounding-mode
            (car (nth (ldb (byte 2 22) flags) *rounding-mode-alist*)))
           (overflow (logbitp 10 flags))         ; OFE
           (underflow (logbitp 11 flags))        ; UFE
           (division-by-zero (logbitp 9 flags))  ; DZE
           (invalid (logbitp 8 flags))           ; IOE
           (inexact (logbitp 12 flags)))         ; IXE
      (if mode-p
        (ecase mode
          (:rounding-mode rounding-mode)
          (:overflow overflow)
          (:underflow underflow)
          (:division-by-zero division-by-zero)
          (:invalid invalid)
          (:inexact inexact))
        `(:rounding-mode ,rounding-mode
          :overflow ,overflow
          :underflow ,underflow
          :division-by-zero ,division-by-zero
          :invalid ,invalid
          :inexact ,inexact)))))

;;; from ppc-float.lisp:537 (set-fpu-mode) — ARM32 shape
;;; (arm-float.lisp:344-381) with logior/logandc2 in place of ARM32's
;;; bitsetf/bitclrf; returns the new control word (ARM32 returns its
;;; %get-fpscr, which ppc-float has no analog of).
(defun set-fpu-mode (&key (rounding-mode :nearest rounding-p)
                          (overflow t overflow-p)
                          (underflow t underflow-p)
                          (division-by-zero t zero-p)
                          (invalid t invalid-p)
                          (inexact t inexact-p))
  (let* ((current (%get-fpscr-control))
         (new current))
    (declare (fixnum current new))
    (when rounding-p
      (let* ((rc-bits (or
                       (cdr (assoc rounding-mode *rounding-mode-alist*))
                       (error "Unknown rounding mode: ~s" rounding-mode))))
        (declare (fixnum rc-bits))
        (setq new (dpb rc-bits (byte 2 22) new))))
    (macrolet ((set-enable (flag-p flag bit)
                 `(when ,flag-p
                    (setq new (if ,flag
                                (logior new (ash 1 ,bit))
                                (logandc2 new (ash 1 ,bit)))))))
      (set-enable invalid-p invalid 8)            ; IOE
      (set-enable zero-p division-by-zero 9)      ; DZE
      (set-enable overflow-p overflow 10)         ; OFE
      (set-enable underflow-p underflow 11)       ; UFE
      (set-enable inexact-p inexact 12))          ; IXE
    (unless (= current new)
      (%set-fpscr-control new))
    new))

;;; from ppc-float.lisp:574 (unguarded)
;;; Copy a single float pointed at by the macptr in single
;;; to a double float pointed at by the macptr in double.
;;; PPC lfs widens on load; explicit fcvt here (arm-float.lisp:462-469 same).
(defarm64lapfunction %single-float-ptr->double-float-ptr ((single arg_y) (double arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 single)
  (ldr s0 (:@ imm0 (:$ 0)))
  (fcvt d1 s0)
  (macptr-ptr imm0 double)
  (str d1 (:@ imm0 (:$ 0)))
  (ret))

;;; from ppc-float.lisp:584 (unguarded)
;;; Copy a double float pointed at by the macptr in double
;;; to a single float pointed at by the macptr in single.
(defarm64lapfunction %double-float-ptr->single-float-ptr ((double arg_y) (single arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 double)
  (ldr d0 (:@ imm0 (:$ 0)))
  (fcvt s2 d0)
  (macptr-ptr imm0 single)
  (str s2 (:@ imm0 (:$ 0)))
  (ret))

;;; from ppc-float.lisp:593 (unguarded)
;;; PPC stfs converts double→single on store; explicit fcvt here.
(defarm64lapfunction %set-ieee-single-float-from-double ((src arg_y) (macptr arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 macptr)
  (get-double-float d1 src)
  (fcvt s0 d1)
  (str s0 (:@ imm0 (:$ 0)))
  (ret))

;;; from ppc-float.lisp:607 (#+ppc64-target; the :601 defun is ppc32)
;;; Identical packing to PPC64 (bits in [63:32], subtag in the low byte):
;;; the PPC64 body ports line-for-line.  fixnumshift=3 both.
(defarm64lapfunction host-single-float-from-unsigned-byte-32 ((u32 arg_z))
  (lsl arg_z u32 (:$ (- 32 arm64::fixnumshift)))
  (orr arg_z arg_z (:$ arm64::subtag-single-float))
  (ret))

;;; from ppc-float.lisp:618 (#+ppc64-target; the :614 defun is ppc32)
;;; Payload [63:32] → boxed fixnum; the low-nibble tag (1) falls off the
;;; bottom of the 29-bit shift.  Line-for-line PPC64.
(defarm64lapfunction single-float-bits ((f arg_z))
  (lsr arg_z f (:$ (- 32 arm64::fixnumshift)))
  (ret))

;;; from ppc-float.lisp:622/626 (double-float-bits / double-float-from-bits)
;;; LE cell adaptation: his value-cell(0) ALIASES val-low-cell(0) with the
;;; high 32 bits in val-high-cell(1) (arm64-arch.lisp:433-437) — PPC64 (BE)
;;; has value-cell = the HIGH half, so a verbatim port would read/write
;;; cell 0 twice.  high/low semantics preserved via val-high/val-low cells.
(defun double-float-bits (f)
  (values (uvref f arm64::double-float.val-high-cell)
          (uvref f arm64::double-float.val-low-cell)))

(defun double-float-from-bits (high low)
  (let* ((f (%make-dfloat)))
    (setf (uvref f arm64::double-float.val-high-cell) high
          (uvref f arm64::double-float.val-low-cell) low)
    f))

;;; from ppc-float.lisp:632 (unguarded)
;;; PPC signed-compares the 32-bit high word; the sign is bit 63 of the
;;; 64-bit value — tbz on the single ldur (catches -0.0, as PPC does).
(defarm64lapfunction %double-float-sign ((n arg_z))
  (ldur imm0 (:@ n (:$ arm64::double-float.value)))
  (mov arg_z rnil)
  (tbz imm0 (:$ 63) @done)
  (add arg_z rnil (:$ arm64::t-offset))
  @done
  (ret))

;;; from ppc-float.lisp:640 (unguarded; ppc64 body arm srdi-32 =
;;; his get-single-float-bits); sign = bit 31 of the payload.
(defarm64lapfunction %short-float-sign ((n arg_z))
  (get-single-float-bits imm0 n)
  (mov arg_z rnil)
  (tbz imm0 (:$ 31) @done)
  (add arg_z rnil (:$ arm64::t-offset))
  @done
  (ret))

;;; from ppc-float.lisp:657 (#+64-bit-target %single-float-sqrt; the
;;; destructive #+32-bit-target %single-float-sqrt! at :650 is SKIPped)
(defarm64lapfunction %single-float-sqrt ((arg arg_z))
  (get-single-float s1 arg)
  (fsqrt s2 s1)
  (put-single-float s2 arg_z)
  (ret))

;;; from ppc-float.lisp:663 (unguarded)
(defarm64lapfunction %double-float-sqrt! ((src arg_y) (dest arg_z))
  (get-double-float d1 src)
  (fsqrt d2 d1)
  (put-double-float d2 dest)
  (ret))

;;; from ppc-float.lisp:670 (#+poweropen-target %get-fp-arg-regs —
;;; linuxppc64 is PowerOpen-ABI-family, so PPC64-reachable; FFI-CRITICAL
;;; per W4-D13).  AAPCS64 re-shape (W6-D42): PPC dumps its 13 FP arg/result
;;; registers f1-f13 into a 104-byte block; AAPCS64's FP arg/result
;;; registers are d0-d7 — 8 doubles, d_i at byte offset 8*i, 64 bytes.
;;; NB: this buffer is FPRs-only at offset 0; it is NOT the
;;; ffcall-return-registers regbuf ({x0-x7@0..56, d0-d7@64..120},
;;; spentry-E-ffi.s:286-289).
(defarm64lapfunction %get-fp-arg-regs ((ptr arg_z))
  (macptr-ptr imm0 ptr)
  (str d0 (:@ imm0 (:$ 0)))
  (str d1 (:@ imm0 (:$ 8)))
  (str d2 (:@ imm0 (:$ 16)))
  (str d3 (:@ imm0 (:$ 24)))
  (str d4 (:@ imm0 (:$ 32)))
  (str d5 (:@ imm0 (:$ 40)))
  (str d6 (:@ imm0 (:$ 48)))
  (str d7 (:@ imm0 (:$ 56)))
  (ret))

;;; from ppc-float.lisp:688 (#+poweropen-target %load-fp-arg-regs)
;;; The d0-d7 loader named by W4-D13(b): n = BOXED count of FP args, ptr =
;;; macptr to the fp-args block (layout above, W6-D42).  Loads d0..d(n-1)
;;; then returns; spentry-E's ffcall expects "FP args already staged in
;;; d0-d7" on entry (spentry-E-ffi.s:183, :235-237), so the future
;;; AAPCS64 %ff-call defun calls this immediately before %do-ff-call.
;;; PPC's 8-CR-field compare pipeline (cr0-cr7) serialized to one NZCV:
;;; cmp/b.eq per step, with only flag-safe FP loads between (wave-1
;;; convention).  Arity 13 → 8 (AAPCS64).
(defarm64lapfunction %load-fp-arg-regs ((n arg_y) (ptr arg_z))
  (cbz n @done)                         ; ppc:697 beqlr cr0
  (macptr-ptr imm0 ptr)
  (ldr d0 (:@ imm0 (:$ 0)))
  (cmp n (:$ (ash 1 arm64::fixnumshift)))
  (b.eq @done)
  (ldr d1 (:@ imm0 (:$ 8)))
  (cmp n (:$ (ash 2 arm64::fixnumshift)))
  (b.eq @done)
  (ldr d2 (:@ imm0 (:$ 16)))
  (cmp n (:$ (ash 3 arm64::fixnumshift)))
  (b.eq @done)
  (ldr d3 (:@ imm0 (:$ 24)))
  (cmp n (:$ (ash 4 arm64::fixnumshift)))
  (b.eq @done)
  (ldr d4 (:@ imm0 (:$ 32)))
  (cmp n (:$ (ash 5 arm64::fixnumshift)))
  (b.eq @done)
  (ldr d5 (:@ imm0 (:$ 40)))
  (cmp n (:$ (ash 6 arm64::fixnumshift)))
  (b.eq @done)
  (ldr d6 (:@ imm0 (:$ 48)))
  (cmp n (:$ (ash 7 arm64::fixnumshift)))
  (b.eq @done)
  (ldr d7 (:@ imm0 (:$ 56)))
  @done
  (ret))
