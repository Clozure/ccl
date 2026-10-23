;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; arm64-array.lisp — Wave-8 DRAFT port of vendor/ccl/level-0/PPC/ppc-array.lisp
;;; PPC64 LINE-PORT (source: vendor/ccl/level-0/PPC/ppc-array.lisp)
;;; Target: Matt Emerson upstream arm64 (low-tag) design, pin d71a5ad.
;;; Per-line citations: "; ppc:NNN" = line NNN of vendor/ccl/level-0/PPC/ppc-array.lisp.
;;;
;;; CLASS-DISPATCH RESTRUCTURE (W8-D70): Matt's design has NO 8-bit ivector
;;; class.  His classes (arm64-arch.lisp:89-91) are 64-bit (#b1101),
;;; 32-bit (#b1100) and OTHER (#b0101); s8/u8/s16/u16/bit/complex-double-
;;; float-vector all live in the OTHER class.  PPC64's four-way class
;;; dispatch (64/32/8/residue) is re-keyed here: fulltag → 64/32, then the
;;; OTHER residue dispatches on the header SUBTAG.  Per-subtag arms are
;;; line-ported unchanged.
;;;
;;; NODEHEADER TEST (W8-D71): PPC64 tests the 2-bit lowtag against
;;; lowtag-nodeheader.  Matt has no lowtags, but both nodeheader fulltags
;;; (#b0110/#b1110, arm64-arch.lisp:62,70) share the 3-bit lisptag
;;; tag-nodeheader (#b110, arm64-arch.lisp:52), so the test becomes
;;; (and x tagmask) == tag-nodeheader.
;;;
;;; STATUS: DRAFT — not assembled; ledger in wave8-array-clos-report.md.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM64-LAPMACROS"))

;;; =====================================================================
;;; %init-misc — ppc:231 (#+ppc64-target arm; the ppc:41 arm is ppc32-only)
;;; =====================================================================
;;; Fill every element of a freshly-allocated uvector with val, after
;;; type-checking val against the vector's element type.
;;; Register plan (one-NZCV serialization; PPC uses cr0..cr7):
;;;   imm1 = header fulltag (ivector class)   imm2 = header subtag
;;;   imm3 = unboxed element count            imm4 = byte-offset cursor
;;;   imm5 = val's typecode, held LIVE for the whole body; PPC's persistent
;;;          cr7 (fixnum) / cr5 (bignum) compares are RE-ISSUED against
;;;          imm5 at each use point.  (imm5=x5 is DISTINCT from nargs=x6
;;;          on Matt's map — plain scratch use; @bad does its own set-nargs.)
;;;   imm0 = scratch / the value eventually stored.
(defarm64lapfunction %init-misc ((val arg_y)
                                 (miscobj arg_z))
  (getvheader imm0 miscobj)             ; ppc:233
  (header-size imm3 imm0)               ; ppc:236 unboxed element count
  (extract-fulltag imm1 imm0)           ; ppc:238 ivector class = header fulltag
  (extract-lowbyte imm2 imm0)           ; ppc:240 header subtag
  (cbz imm3 @return)                    ; ppc:237/241 silly 0-length case (beqlr cr3)
  (mov imm4 (:$ arm64::misc-data-offset)) ; ppc:242
  ;; node-vs-ivector: W8-D71 lisptag test replaces ppc:235/239 lowtag test
  (and imm0 imm0 (:$ arm64::tagmask))   ; ppc:235 (clrldi → and tagmask)
  (cmp imm0 (:$ arm64::tag-nodeheader)) ; ppc:239
  (b.ne @imm)                           ; ppc:243
  ;; Node vector.  Don't need to memoize, since initial value is
  ;; older than vector.                 ; ppc:244-245
  @node-loop                            ; ppc:246
  (subs imm3 imm3 (:$ 1))               ; ppc:247-248 (cmpdi/subi → subs at loop top, wave-5)
  (str val (:@ miscobj imm4))           ; ppc:249 stdx (regoff)
  (add imm4 imm4 (:$ arm64::node-size)) ; ppc:250
  (b.ne @node-loop)                     ; ppc:251
  @return
  (ret)                                 ; ppc:252
  @imm                                  ; ppc:253
  (extract-typecode imm5 val)           ; ppc:254 — persistent (see header note)
  ;; W8-D70 class dispatch: 64/32 on fulltag, then OTHER residue on subtag
  (cmp imm1 (:$ arm64::ivector-class-64-bit)) ; ppc:255
  (b.eq @64)                            ; ppc:261
  (cmp imm1 (:$ arm64::ivector-class-32-bit)) ; ppc:256
  (b.eq @32)                            ; ppc:262
  ;; OTHER class residue (ppc:257/263 ivector-class-8-bit has no analog)
  (cmp imm2 (:$ arm64::subtag-s8-vector)) ; ppc:402 (@8 arm's s8 test, hoisted)
  (b.eq @s8)
  (cmp imm2 (:$ arm64::subtag-u8-vector)) ; ppc:257/263 (@8 arm's default = u8)
  (b.eq @u8)
  (cmp imm2 (:$ arm64::subtag-complex-double-float-vector)) ; ppc:260
  (b.eq @complex-double-float)          ; ppc:264
  ;; u16, s16, or bit-vector.  Val must be a fixnum.  ; ppc:265
  (cmp imm5 (:$ arm64::tag-fixnum))     ; ppc:258 cr7 (re-issued)
  (b.ne @bad)                           ; ppc:268
  (cmp imm2 (:$ arm64::subtag-u16-vector)) ; ppc:266
  (b.eq @u16)                           ; ppc:269
  (cmp imm2 (:$ arm64::subtag-s16-vector)) ; ppc:267
  (b.eq @s16)                           ; ppc:270
  ;; Bit vector.                        ; ppc:271
  (cmp val (:$ (ash 1 arm64::fixnumshift))) ; ppc:272 cmpldi val '1 (UNSIGNED)
  (add imm3 imm3 (:$ 63))               ; ppc:273 (flag-safe)
  (lsr imm3 imm3 (:$ 6))                ; ppc:274 bit count → word count
  (unbox-fixnum imm0 val)               ; ppc:275
  (neg imm0 imm0)                       ; ppc:276 0 → 0, 1 → all-ones (non-flag-setting)
  (b.ls @set-64)                        ; ppc:277 ble+ cr0 (unsigned ≤)
  ;; fall through to @bad
  @bad                                  ; ppc:278
  (mov arg_x (:$ (ash $xnotelt arm64::fixnumshift))) ; ppc:279 li arg_x '#.$xnotelt
  (save-lisp-context)                   ; ppc:280
  (set-nargs 3)                         ; ppc:281
  (call-symbol %err-disp)               ; ppc:282 — does not return (DECIDE-14/W3 call-symbol)
  @complex-double-float                 ; ppc:283
  (cmp imm5 (:$ arm64::subtag-complex-double-float)) ; ppc:284
  (b.ne @bad)                           ; ppc:285
  ;; scalar complex-double-float: pad word after header, realpart @ +12,
  ;; imagpart @ +20 (arm64-arch.lisp:450-453) — not 8-multiples → ldur
  (ldur d0 (:@ val (:$ arm64::complex-double-float.realpart))) ; ppc:286 lfd fp0
  (ldur d1 (:@ val (:$ arm64::complex-double-float.imagpart))) ; ppc:287 lfd fp1
  ;; W8-D72: destination vector's element 0 assumed at the same
  ;; pad-aligned offset (misc-complex-dfloat-offset = 12) — 16-byte
  ;; element alignment demands it, but the VECTOR layout is not spelled
  ;; out in Matt's arch.  See ledger.
  (mov imm4 (:$ arm64::complex-double-float.realpart)) ; ppc:288
  @complex-double-float-loop            ; ppc:289
  (subs imm3 imm3 (:$ 1))               ; ppc:290-291
  (str d0 (:@ miscobj imm4))            ; ppc:292 stfdx (FP regoff)
  (add imm4 imm4 (:$ 8))                ; ppc:293
  (str d1 (:@ miscobj imm4))            ; ppc:294
  (add imm4 imm4 (:$ 8))                ; ppc:295
  (b.ne @complex-double-float-loop)     ; ppc:296
  (ret)                                 ; ppc:297
  @64                                   ; ppc:298
  (cmp imm2 (:$ arm64::subtag-complex-single-float-vector)) ; ppc:299
  (b.eq @complex-single-float)          ; ppc:303
  (cmp imm2 (:$ arm64::subtag-fixnum-vector)) ; ppc:300
  (b.eq @fixnum)                        ; ppc:304
  (cmp imm2 (:$ arm64::subtag-double-float-vector)) ; ppc:301
  (b.eq @dfloat)                        ; ppc:305
  (cmp imm2 (:$ arm64::subtag-s64-vector)) ; ppc:302
  (b.ne @u64)                           ; ppc:306
  ;; s64                                ; ppc:307
  (unbox-fixnum imm0 val)               ; ppc:308
  (cmp imm5 (:$ arm64::tag-fixnum))     ; cr7 re-issue
  (b.eq @set-64)                        ; ppc:309 all fixnums are (SIGNED-BYTE 64)
  (cmp imm5 (:$ arm64::subtag-bignum))  ; cr5 re-issue
  (b.ne @bad)                           ; ppc:310
  (getvheader imm1 val)                 ; ppc:311
  ;; W8-D73: ppc:314 (rotldi imm0 imm0 32) DROPPED — on the little-endian
  ;; target a 64-bit load of two LE 32-bit digits already has the low
  ;; digit in the low half (x86-64 does no swap either).
  (ldur imm0 (:@ val (:$ arm64::misc-data-offset))) ; ppc:312
  (cmp imm1 (:$ arm64::two-digit-bignum-header)) ; ppc:313 (arch:662)
  (b.eq @set-64)                        ; ppc:315
  (b @bad)                              ; ppc:316
  @complex-single-float                 ; ppc:317
  (cmp imm5 (:$ arm64::subtag-complex-single-float)) ; ppc:318
  (b.ne @bad)                           ; ppc:319
  ;; 64-bit load grabs the {realpart,imagpart} pair as one element image
  ;; (LE: real in low half; scalar and vector element layouts agree)
  (ldur imm0 (:@ val (:$ arm64::complex-single-float.realpart))) ; ppc:320
  (b @set-64)                           ; ppc:321
  @fixnum                               ; ppc:322
  (unbox-fixnum imm0 val)               ; ppc:323
  (cmp imm5 (:$ arm64::tag-fixnum))     ; cr7 re-issue
  (b.eq @set-64)                        ; ppc:324
  (b @bad)                              ; ppc:325
  ;; u64 if fixnum and positive, 2-digit bignum and positive, or
  ;; 3-digit bignum with most-significant digit 0.  ; ppc:326-327
  @u64                                  ; ppc:328
  (unbox-fixnum imm0 val)               ; ppc:330
  (cmp imm5 (:$ arm64::tag-fixnum))     ; cr7 re-issue
  (b.ne @u64-maybe-bignum)              ; ppc:331
  (cmp val (:$ 0))                      ; ppc:329 cr2 (re-issued at use point, signed)
  (b.ge @set-64)                        ; ppc:332
  (b @bad)                              ; ppc:333
  @u64-maybe-bignum                     ; ppc:334
  (cmp imm5 (:$ arm64::subtag-bignum))  ; cr5 re-issue
  (b.ne @bad)                           ; ppc:335
  (ldur imm0 (:@ val (:$ arm64::misc-data-offset))) ; ppc:336 (W8-D73: no rotldi)
  (getvheader imm1 val)                 ; ppc:337
  (cmp imm1 (:$ arm64::two-digit-bignum-header)) ; ppc:339
  (b.eq @u64-two-digit)                 ; ppc:342
  (cmp imm1 (:$ arm64::three-digit-bignum-header)) ; ppc:340 (arch:663)
  (b.ne @bad)                           ; ppc:343
  ;; third (most significant) digit must be 0
  (ldr (:w imm1) (:@ val (:$ (+ 8 arm64::misc-data-offset)))) ; ppc:344 lwz @12 (4-aligned, scaled ok)
  (cbz imm1 @set-64)                    ; ppc:345-346 (cmpwi/beq → cbz)
  (b @bad)                              ; ppc:347
  @u64-two-digit                        ; ppc:348
  (cmp imm0 (:$ 0))                     ; ppc:341 cr0 (re-issued, signed)
  (b.gt @set-64)                        ; ppc:349
  (b @bad)                              ; ppc:350
  @dfloat                               ; ppc:351
  (cmp imm5 (:$ arm64::subtag-double-float)) ; ppc:352
  (b.ne @bad)                           ; ppc:353
  (ldur imm0 (:@ val (:$ arm64::double-float.value))) ; ppc:354 (offset 4 → ldur)
  (b @set-64)                           ; ppc:355
  @32                                   ; ppc:356
  (cmp imm2 (:$ arm64::subtag-simple-base-string)) ; ppc:357
  (b.eq @char32)                        ; ppc:360
  (cmp imm2 (:$ arm64::subtag-s32-vector)) ; ppc:358
  (b.eq @s32)                           ; ppc:361
  (cmp imm2 (:$ arm64::subtag-single-float-vector)) ; ppc:359
  (b.ne @u32)                           ; ppc:362
  ;; @sfloat — single-floats are IMMEDIATE (bits in [63:32]) on BOTH
  ;; PPC64 and Matt's design (his get-single-float-bits = lsr 32);
  ;; ppc:365 ports verbatim.
  (cmp imm5 (:$ arm64::subtag-single-float)) ; ppc:364
  (lsr imm0 val (:$ 32))                ; ppc:365 (flag-safe)
  (b.ne @bad)                           ; ppc:366
  (b @set-32)                           ; ppc:367
  @s32                                  ; ppc:368
  ;; Must be a fixnum (and a (SIGNED-BYTE 32)).  ; ppc:369
  (cmp imm5 (:$ arm64::tag-fixnum))     ; cr7 re-issue
  (b.ne @bad)                           ; ppc:370
  (unbox-fixnum imm0 val)               ; ppc:371
  (lsl imm1 imm0 (:$ 32))               ; ppc:372
  (asr imm1 imm1 (:$ 32))               ; ppc:373
  (cmp imm1 imm0)                       ; ppc:374
  (b.ne @bad)                           ; ppc:375
  (b @set-32)                           ; ppc:376
  @char32                               ; ppc:377
  (unbox-base-char imm0 val t)          ; ppc:378 — checked variant (brk #xf002, DECIDE-8)
  (b @set-32)                           ; ppc:379
  @u32                                  ; ppc:380
  ;; Also has to be a fixnum (and an (UNSIGNED-BYTE 32)).  ; ppc:381
  (cmp imm5 (:$ arm64::tag-fixnum))     ; cr7 re-issue (ppc:384, hoisted before ands)
  (b.ne @bad)                           ; ppc:384
  (unbox-fixnum imm0 val)               ; ppc:382
  (ands imm1 imm0 (:$ (ldb (byte 64 0) (lognot #xffffffff)))) ; ppc:383 clrrdi. 32 (ldb-wrap)
  (b.ne @bad)                           ; ppc:385
  (b @set-32)                           ; ppc:386
  @u16                                  ; ppc:387
  ;; fixnum-ness already checked at the residue dispatch (ppc:390 cr7)
  (unbox-fixnum imm0 val)               ; ppc:388
  (ands imm1 imm0 (:$ (ldb (byte 64 0) (lognot #xffff)))) ; ppc:389 clrrdi. 16 (ldb-wrap)
  (b.ne @bad)                           ; ppc:391
  (b @set-16)                           ; ppc:392
  @s16                                  ; ppc:393
  ;; fixnum-ness already checked at the residue dispatch (ppc:398 cr7)
  ;; W8-D74: ppc:396 is cmpw (32-bit compare); full-width cmp used here is
  ;; strictly stricter — see ledger.
  (lsl imm0 val (:$ (- 64 (+ 16 arm64::fixnumshift))))  ; ppc:394
  (asr imm0 imm0 (:$ (- 64 (+ 16 arm64::fixnumshift)))) ; ppc:395
  (cmp imm0 val)                        ; ppc:396
  (unbox-fixnum imm0 val)               ; ppc:397 (flag-safe)
  (b.ne @bad)                           ; ppc:398-400
  (b @set-16)                           ; ppc:399
  @u8                                   ; ppc:401/404 (@8 arm's default = u8)
  (extract-unsigned-byte-bits. imm0 val 8) ; ppc:404 (ror+ands; :eq iff u8 fixnum)
  (unbox-fixnum imm0 val)               ; ppc:405 (flag-safe)
  (b.eq @set-8)                         ; ppc:406
  (b @bad)                              ; ppc:407
  @s8                                   ; ppc:408
  (cmp imm5 (:$ arm64::tag-fixnum))     ; ppc:413 cr7 (hoisted; one-NZCV)
  (b.ne @bad)
  (lsl imm0 val (:$ (- 64 (+ 8 arm64::fixnumshift))))  ; ppc:409
  (asr imm0 imm0 (:$ (- 64 (+ 8 arm64::fixnumshift)))) ; ppc:410
  (cmp imm0 val)                        ; ppc:411 (W8-D74 applies: cmpd here on PPC)
  (unbox-fixnum imm0 val)               ; ppc:412 (flag-safe)
  (b.ne @bad)                           ; ppc:414
  (b @set-8)                            ; ppc:414
  @char8                                ; ppc:416 — unreachable in the PPC64 source
                                        ; too (base-strings are 32-bit); kept for fidelity
  (unbox-base-char imm0 val t)          ; ppc:417
  ;; Replicate the element across a 64-bit word.
  @set-8
  (add imm3 imm3 (:$ 1))
  (bfi imm0 imm0 (:$ 8) (:$ 8))         ;(dpb imm0 (byte 8 8) imm0)
  (lsr imm3 imm3 (:$ 1))
  @set-16
  (add imm3 imm3 (:$ 1))
  (bfi imm0 imm0 (:$ 16) (:$ 16))       ;(dpb imm0 (byte 16 16) imm0)
  (lsr imm3 imm3 (:$ 1))
  @set-32
  (add imm3 imm3 (:$ 1))
  (bfi imm0 imm0 (:$ 32) (:$ 32))       ;(dpb imm0 (byte 32 32) imm0)
  (lsr imm3 imm3 (:$ 1))
  @set-64
  (subs imm3 imm3 (:$ 1))
  (str imm0 (:@ miscobj imm4))          ;store full 64-bit word
  (add imm4 imm4 (:$ 8))
  (b.ne @set-64)
  (ret))

;;; =====================================================================
;;; %array-header-data-and-offset — ppc:706
;;; =====================================================================
;;; argument is a vector header or an array header.  Or else.  ; ppc:705
;;; PPC's cr0/cr1 subtag pair serialized: both compares re-issued
;;; back-to-back ahead of their branches (loads/adds between are flag-safe).
(defarm64lapfunction %array-header-data-and-offset ((a arg_z))
  (let ((offset arg_y)
        (disp arg_x)
        (temp temp0))
    (mov offset (:$ 0))                 ; ppc:710
    (mov temp a)                        ; ppc:711
    @loop                               ; ppc:712
    (ldur a (:@ temp (:$ arm64::arrayH.data-vector))) ; ppc:713 (+20 → ldur)
    (ldurb (:w imm0) (:@ a (:$ arm64::misc-subtag-offset))) ; ppc:714 lbz (LE low byte @ -4)
    (ldur disp (:@ temp (:$ arm64::arrayH.displacement))) ; ppc:717 (+28 → ldur)
    (mov temp a)                        ; ppc:718
    (add offset offset disp)            ; ppc:719
    (cmp imm0 (:$ arm64::subtag-vectorH)) ; ppc:715
    (b.eq @loop)                        ; ppc:720
    (cmp imm0 (:$ arm64::subtag-arrayH)) ; ppc:716 (re-issued — one NZCV)
    (b.eq @loop)                        ; ppc:721
    (vpush a)                           ; ppc:722
    (vpush offset)                      ; ppc:723
    (set-nargs 2)                       ; ppc:724
    (add temp0 vsp (:$ (* 2 arm64::node-size))) ; ppc:725 entry-vsp for .SPvalues
    ;; ppc:726 (ba .SPvalues) — TAIL, no-link (DECIDE-10; temp0 = entry-vsp)
    (jump-subprim .SPvalues)))

;;; =====================================================================
;;; %simple-bit-boole — ppc:812 (#+ppc64-target arm; ppc:732 arm is ppc32-only)
;;; =====================================================================
;;; If the bit-arrays are all simple-bit-vector-p, do the boole op 64 bits
;;; at a time.  ; ppc:729-730
;;; PPC materializes the dispatch-table pc via bl/blrl/mflr; arm64 uses
;;; adr (W8-D77).  PPC entries are 2 insns (8 bytes) so the BOXED op is
;;; the byte offset; arm64 entries are FOUR insns (16 bytes) because
;;; nand/nor need two ALU ops — offset = boxed-op * 2, formed by two adds
;;; (shifted-register add avoided, encode-landmine family).
;;; Dispatch entries are flag-free (mov/mvn/and/orr/eor/eon/bic/orn) and
;;; end in ret; loop flags survive the blr call (W8-D78).
(defarm64lapfunction %simple-bit-boole ((op 0) (bv1 arg_x) (bv2 arg_y) (result arg_z))
  (add imm0 vsp (:$ 8))                 ; ppc:813 caller's vsp (one stack arg)
  (save-lisp-context imm0)              ; ppc:814 (non-default variant; imm1 = marker scratch)
  (vector-size imm4 result imm4)        ; ppc:815 unboxed bit count
  (lsr imm3 imm4 (:$ 6))                ; ppc:816 srdi. — whole 64-bit words
  (and imm4 imm4 (:$ 63))               ; ppc:817 residual bits (at most low 6 bits)
  ;; ppc:818/820 bl @get-dispatch / mflr — the draft's `adr @label`
  ;; doesn't exist in his lap (16m5q wall 3: adr wants a numeric
  ;; immediate).  Use the donor's own double-link trick: bl to a
  ;; `blr lr`, which branches straight back while re-linking lr to the
  ;; instruction after itself = the dispatch table base (A64 BLR sets
  ;; x30 := pc+4, exactly PPC's blrl).
  (bl @get-dispatch)                    ; ppc:818
  (mov imm5 lr)                         ; ppc:820 mflr loc-pc
  (ldr temp0 (:@ vsp (:$ 0)))           ; ppc:821 boxed op
  (add imm5 imm5 temp0)                 ; ppc:822 dispatch entry address...
  (add imm5 imm5 temp0)                 ;   ...second add: 16-byte entries (see header)
  (mov imm0 (:$ arm64::misc-data-offset)) ; ppc:824
  (cbz imm3 @residual)                  ; ppc:816/825/834-835 loop gate (srdi./b @testd → cbz)
  @nextd                                ; ppc:826
  (ldr imm1 (:@ bv1 imm0))               ; ppc:829 ldx (regoff)
  (ldr imm2 (:@ bv2 imm0))               ; ppc:830
  (blr imm5)                            ; ppc:831 bctrl — entry computes imm1 = op(imm1,imm2)
  (str imm1 (:@ result imm0))           ; ppc:832 stdx
  (add imm0 imm0 (:$ 8))                ; ppc:833
  (subs imm3 imm3 (:$ 1))               ; ppc:827-828 (cmpdi/subi → subs; adjacent to branch)
  (b.ne @nextd)                         ; ppc:835
  @residual
  (cbz imm4 @done)                      ; ppc:819/836 (cmpdi cr1/beq → cbz)
  ;; Not sure if we need to make this much fuss about the partial word
  ;; in this simple case, but what the hell.  ; ppc:837-838
  ;; W8-D79: bit-vectors are LSB-first on the LE targets (x86-64 family)
  ;; vs MSB-first on PPC — the partial-word merge sense INVERTS: the new
  ;; result contributes the LOW imm4 bits, the old contents keep the
  ;; HIGH (64-imm4) bits.
  (ldr imm1 (:@ bv1 imm0))               ; ppc:839
  (ldr imm2 (:@ bv2 imm0))               ; ppc:840
  (blr imm5)                            ; ppc:841
  (ldr imm2 (:@ result imm0))           ; ppc:842 old contents
  (lsrv imm2 imm2 imm4)                 ; ppc:843-844 clear LOW imm4 bits (sense inverted)
  (lslv imm2 imm2 imm4)
  (mov imm3 (:$ 64))                    ; ppc:845 subfic imm4,imm4,64 (imm3 free here)
  (sub imm3 imm3 imm4)
  (lslv imm1 imm1 imm3)                 ; ppc:846-847 keep LOW imm4 bits of new
  (lsrv imm1 imm1 imm3)
  (orr imm1 imm1 imm2)                  ; ppc:848
  (str imm1 (:@ result imm0))           ; ppc:849
  @done                                 ; ppc:850
  (restore-full-lisp-context)           ; ppc:851 (restores caller vsp — pops op)
  (ret)                                 ; ppc:852
  @get-dispatch                         ; ppc:854
  (blr lr)                              ; ppc:855 blrl — lr := @dispatch, branch back
  ;; Dispatch table — ppc:856-888.  Order = boole-clr..boole-orc2 (0..15),
  ;; exactly as PPC.  Each entry EXACTLY 4 instructions (16 bytes).
  @dispatch
  (mov imm1 (:$ 0))                     ; boole-clr        ; ppc:857
  (ret) (ret) (ret)
  (mov imm1 (:$ -1))                    ; boole-set        ; ppc:859 (movn alias)
  (ret) (ret) (ret)
  (ret)                                 ; boole-1          ; ppc:861 (imm1 already = b1 word)
  (ret) (ret) (ret)
  (mov imm1 imm2)                       ; boole-2          ; ppc:863
  (ret) (ret) (ret)
  (mvn imm1 imm1)                       ; boole-c1         ; ppc:865
  (ret) (ret) (ret)
  (mvn imm1 imm2)                       ; boole-c2         ; ppc:867
  (ret) (ret) (ret)
  (and imm1 imm1 imm2)                  ; boole-and        ; ppc:869
  (ret) (ret) (ret)
  (orr imm1 imm1 imm2)                  ; boole-ior        ; ppc:871
  (ret) (ret) (ret)
  (eor imm1 imm1 imm2)                  ; boole-xor        ; ppc:873
  (ret) (ret) (ret)
  (eon imm1 imm1 imm2)                  ; boole-eqv        ; ppc:875 (eqv = eon)
  (ret) (ret) (ret)
  (and imm1 imm1 imm2)                  ; boole-nand       ; ppc:877 (no arm64 nand:
  (mvn imm1 imm1)                       ;   and+mvn — the reason entries are 16 bytes)
  (ret) (ret)
  (orr imm1 imm1 imm2)                  ; boole-nor        ; ppc:879 (orr+mvn)
  (mvn imm1 imm1)
  (ret) (ret)
  (bic imm1 imm2 imm1)                  ; boole-andc1      ; ppc:881 (andc rS&~rB → bic)
  (ret) (ret) (ret)
  (bic imm1 imm1 imm2)                  ; boole-andc2      ; ppc:883
  (ret) (ret) (ret)
  (orn imm1 imm2 imm1)                  ; boole-orc1       ; ppc:885
  (ret) (ret) (ret)
  (orn imm1 imm1 imm2)                  ; boole-orc2       ; ppc:887
  (ret) (ret) (ret))

;;; =====================================================================
;;; %aref2 / %aref3 / %aset2 / %aset3 — ppc:891-910
;;; =====================================================================
;;; Thin tails to the array subprims.  Register contract mirrors PPC64:
;;; the vstack-passed array (and i, for aset3) are popped into temp0
;;; (/temp1) before the tail jump.  None of these subprims are in Matt's
;;; table — DECIDE-10 (no-link tail) on each.

;;; from ppc-array.lisp:891
(defarm64lapfunction %aref2 ((array arg_x) (i arg_y) (j arg_z))
  (check-nargs 3)                       ; ppc:892
  (jump-subprim .SParef2))              ; ppc:893 ba — TAIL no-link (DECIDE-10)

;;; from ppc-array.lisp:895
(defarm64lapfunction %aref3 ((array 0) (i arg_x) (j arg_y) (k arg_z))
  (check-nargs 4)                       ; ppc:896
  (vpop temp0)                          ; ppc:897 array → temp0 (subprim ABI input)
  (jump-subprim .SParef3))              ; ppc:898 — TAIL no-link (DECIDE-10)

;;; from ppc-array.lisp:901
(defarm64lapfunction %aset2 ((array 0) (i arg_x) (j arg_y) (newval arg_z))
  (check-nargs 4)                       ; ppc:902
  (vpop temp0)                          ; ppc:903 array → temp0
  (jump-subprim .SPaset2))              ; ppc:904 — TAIL no-link (DECIDE-10)

;;; from ppc-array.lisp:906
;;; PPC lambda list: ((array #.target::node-size) (i 0) ...) — array @ vsp+8,
;;; i @ vsp+0; vpop order gives temp0 = i, temp1 = array (matches the
;;; v2-tree subprim-ABI note: aset3 temp1=array temp0=i).
(defarm64lapfunction %aset3 ((array 8) (i 0) (j arg_x) (k arg_y) (newval arg_z))
  (check-nargs 5)                       ; ppc:907
  (vpop temp0)                          ; ppc:908 i
  (vpop temp1)                          ; ppc:909 array
  (jump-subprim .SPaset3))              ; ppc:910 — TAIL no-link (DECIDE-10)
