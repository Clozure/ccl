;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; arm64-bignum.lisp — Wave-7 DRAFT port of vendor/ccl/level-0/PPC/PPC64/ppc64-bignum.lisp
;;; PPC64 LINE-PORT (source: vendor/ccl/level-0/PPC/PPC64/ppc64-bignum.lisp)
;;; Target: Matt Emerson upstream arm64 (low-tag) design, pin d71a5ad.
;;; Per-line citations: "; ppc:NNN" = line NNN of ppc64-bignum.lisp.
;;; Cross-ref for ENDIANNESS (arm64 is little-endian like x86-64, NOT
;;; big-endian like PPC64): vendor/ccl/level-0/X86/X8664/x8664-bignum.lisp
;;; — cited "; x86:NNN".
;;;
;;; THE BIGNUM DIGIT-ORDER RULE (W7-D50):
;;; CCL bignum digits are 32 bits, digit n living at byte offset
;;; misc-data-offset + 4n.  All 32-bit accesses (lwzx/lwax/stw ↔
;;; ldr/ldrsw/str W-forms) are therefore ENDIAN-NEUTRAL and port
;;; line-by-line.  64-bit accesses to a digit PAIR are NOT: on
;;; big-endian PPC64 the ld places digit n in the HIGH half, so the
;;; PPC64 code rotldi-swaps by 32 after every ld and before every std
;;; to get/put the LE-style value (digit n low, digit n+1 high).  On
;;; little-endian arm64 a plain 64-bit load/store already has digit n
;;; in the low half: EVERY ld+rotldi / rotldi+std pair collapses to a
;;; plain ldr/str/ldur/stur.  Precedent: kernel draft
;;; upstream-port/lisp-kernel/spentry-B-vectors-misc.s:951-953,966,
;;; 979-980,992 ("LE: no rotldi") and wave-4 W4-D19 (%fixnum-set-natural
;;; took the x86 shape for the same reason).  64-bit BITWISE ops on a
;;; digit pair (%bignum-logior/logand) are order-neutral (both digits
;;; transformed independently, stored back in place) — PPC64 itself
;;; does no rotate there, port line-by-line.
;;;
;;; Alignment note: tagged misc pointer = object base + fulltag-misc(4),
;;; so EA of a 64-bit digit-pair access [ptr + misc-data-offset(4) + 8k]
;;; is 8-ALIGNED in memory (the +4 bias cancels the tag); 32-bit digit
;;; accesses land 4-aligned.  misc-data-offset/misc-header-offset are
;;; not 8-multiples, so 64-bit constant-offset forms use ldur/stur
;;; (wave-1 convention); 32-bit forms at +4 use the scaled str/ldr
;;; (multiple-of-4, u32-ref precedent in arm64-lapmacros-additions).
;;;
;;; CARRY CHAINS: PPC addc/adde/addze → adds/adcs/adc.  Every region
;;; where the C flag is live between the setting adds and the consuming
;;; adc is marked "C LIVE" inline; only flag-safe instructions (mul,
;;; umulh, ldr, str, mov — none write NZCV) may intervene, mirroring
;;; PPC where mulhdu doesn't touch CA (ppc:61-65).
;;;
;;; STATUS: DRAFT — not assembled; DECIDE rows in wave7-bignum-report.md.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM64-LAPMACROS"))

;;; =====================================================================
;;; %fixnum-to-bignum-set — from ppc64-bignum.lisp:24
;;; =====================================================================
;;; The caller has allocated a two-digit bignum (quite likely on the
;;; stack).  If we can fit in a single digit (the high word is just a
;;; sign extension of the low word), truncate the bignum in place.
;;; Digit stores are 32-bit (endian-neutral, W7-D50).  The fits-in-one-
;;; digit predicate takes the x86-64 whole-register shape (x86:26-29:
;;; sign-extend low 32, compare with full value) because PPC's split
;;; srawi/cmpw pair is a 32-bit-compare idiom with no 1:1 arm64 form.
(defarm64lapfunction %fixnum-to-bignum-set ((bignum arg_y) (fixnum arg_z))
  (unbox-fixnum imm0 fixnum)                      ; ppc:25
  (lsr imm1 imm0 (:$ 32))                         ; ppc:26 srdi — high digit
  (sbfx imm2 imm0 (:$ 0) (:$ 32))                           ; ppc:27 srawi 31 → sign-ext of low digit (x86:26-27 shape)
  (cmp imm2 imm0)                                 ; ppc:28 cmpw — eq iff high = sign-ext(low)
  (stur (:w imm0) (:@ bignum (:$ arm64::misc-data-offset)))  ; ppc:29 stw digit 0 (32-bit; -4 => STUR, 16m5q wall 4; flag-safe)
  (mov imm2 (:$ arm64::one-digit-bignum-header))  ; ppc:30 (arch.lisp:661; flag-safe)
  (b.eq @chop)                                    ; ppc:31
  (stur (:w imm1) (:@ bignum (:$ (+ arm64::misc-data-offset 4)))) ; ppc:32 stw digit 1 = high word (0 unscaled => STUR pair w/ digit 0)
  (ret)                                           ; ppc:33
  @chop
  (stur imm2 (:@ bignum (:$ arm64::misc-header-offset)))  ; ppc:35 std (-4 → stur)
  (ret))                                          ; ppc:36

;;; =====================================================================
;;; %multiply-and-add-loop64 — from ppc64-bignum.lisp:39
;;; =====================================================================
;;; 64-bit-chunk inner multiply loop: r[i..] += x[i] * y[0..ylen-1] with
;;; carry propagation.  All digit-pair loads/stores were ld/std+rotldi
;;; on PPC64 (BE); on LE they are plain ldr/str (W7-D50; x86:40-67 uses
;;; plain movq throughout).  idx is a boxed word index (= byte offset,
;;; fixnumshift 3 = word-shift).  Register map per the PPC let: i=imm0,
;;; j=imm1, xx=imm2, yy=imm3, rr=imm4, dd=imm5, cc=nargs (imm5=x5 and
;;; nargs=x6 are DISTINCT in Matt's map, arm64-asm.lisp:183-185).
(defarm64lapfunction %multiply-and-add-loop64
    ((x (* 1 arm64::node-size)) (y 0) (r arg_x) (idx arg_y) (ylen arg_z))
  (ldr temp0 (:@ vsp (:$ arm64::node-size)))      ; ppc:48 x bignum
  (sub imm0 idx (:$ (- arm64::misc-data-offset)))     ; ppc:49 la i
  (ldr imm2 (:@ temp0 imm0))                      ; ppc:50-51 ldx+rotldi x[i] → LE plain (x86:46)
  (ldr temp0 (:@ vsp (:$ 0)))                     ; ppc:52 y bignum
  (mov nargs (:$ 0))                              ; ppc:53 li cc 0
  (mov imm1 (:$ arm64::misc-data-offset))         ; ppc:54 li j
  @loop
  (ldr imm3 (:@ temp0 imm1))                      ; ppc:56-57 ldx+rotldi y[j] → LE plain
  (mul imm5 imm2 imm3)                            ; ppc:58 mulld — low 64 of product
  (ldr imm4 (:@ r imm0))                          ; ppc:59-60 ldx+rotldi r[i] → LE plain
  (adds imm4 imm4 imm5)                           ; ppc:61 addc — r[i] += low; C LIVE →
  (umulh imm5 imm2 imm3)                          ; ppc:62 mulhdu — high 64 (flag-safe, as PPC mulhdu leaves CA)
  (adc imm5 imm5 xzr)                             ; ppc:63 addze — dd = high + C; C DEAD
  (adds imm4 imm4 nargs)                          ; ppc:64 addc — add carry digit; C LIVE →
  (adc nargs imm5 xzr)                            ; ppc:65 addze — cc = dd + C; C DEAD
  (str imm4 (:@ r imm0))                          ; ppc:66-67 rotldi+stdx → LE plain str
  (cmp ylen (:$ (ash 1 arm64::fixnumshift)))      ; ppc:68 cmpdi '1
  (add imm0 imm0 (:$ 8))                          ; ppc:69 (flag-safe)
  (add imm1 imm1 (:$ 8))                          ; ppc:70 (flag-safe)
  (sub ylen ylen (:$ (ash 1 arm64::fixnumshift))) ; ppc:71 subi '1 (flag-safe)
  (b.ne @loop)                                    ; ppc:72
  (str nargs (:@ r imm0))                         ; ppc:73-74 rotldi+stdx carry → LE plain (x86:66)
  (set-nargs 0)                                   ; ppc:75
  (add vsp vsp (:$ (* 2 arm64::node-size)))       ; ppc:76 la vsp 16
  (ret))                                          ; ppc:77

;;; =====================================================================
;;; %multiply-and-add4 — from ppc64-bignum.lisp:82
;;; =====================================================================
;;; Multiply the (32-bit) digits X and Y, producing a 64-bit result.
;;; Add the 32-bit "prev" and "carry-in" digits; return (VALUES high low).
;;; Pure boxed arithmetic — no digit memory access, order-neutral (W7-D50);
;;; port PPC64 line-by-line.  clrlsldi (box of low32) → ubfx + box-fixnum.
(defarm64lapfunction %multiply-and-add4 ((x 0) (y arg_x) (prev arg_y) (carry-in arg_z))
  (ldr temp0 (:@ vsp (:$ 0)))                     ; ppc:90 x @ vsp+0
  (unbox-fixnum imm0 temp0)                       ; ppc:91 unboxed-x
  (unbox-fixnum imm1 y)                           ; ppc:92 unboxed-y
  (unbox-fixnum imm2 prev)                        ; ppc:93 unboxed-prev (arg_y consumed before high write)
  (unbox-fixnum imm3 carry-in)                    ; ppc:94 unboxed-carry-in (arg_z consumed before low write)
  (mul imm4 imm0 imm1)                            ; ppc:95 mulld — ≤64-bit product of two u32
  (add imm4 imm4 imm2)                            ; ppc:96
  (add imm4 imm4 imm3)                            ; ppc:97
  (ubfx arg_z imm4 (:$ 0) (:$ 32))                          ; ppc:98 clrlsldi low — extract low digit
  (box-fixnum arg_z arg_z)                        ;   ...and box
  (lsr arg_y imm4 (:$ 32))                        ; ppc:99-100 clrrdi+srdi high — high digit
  (box-fixnum arg_y arg_y)                        ;   ...boxed
  (str arg_y (:@ vsp (:$ 0)))                     ; ppc:101 std high 0 vsp (overwrites x slot)
  (set-nargs 2)                                   ; ppc:102
  (vpush arg_z)                                   ; ppc:103 vpush low
  (add temp0 vsp (:$ (ash 2 arm64::fixnumshift))) ; ppc:104 la temp0 '2 vsp (= entry vsp)
  ;; ppc:105 (ba .SPvalues): TAIL no-link jump (DECIDE-10 — .SPvalues not
  ;; in Matt's *subprims* table; wave-5 `values` precedent).
  (jump-subprim .SPvalues))

;;; =====================================================================
;;; %multiply-and-add3 — from ppc64-bignum.lisp:107
;;; =====================================================================
;;; As %multiply-and-add4 without the "prev" digit.  Order-neutral.
(defarm64lapfunction %multiply-and-add3 ((x arg_x) (y arg_y) (carry-in arg_z))
  (unbox-fixnum imm0 x)                           ; ppc:114 unboxed-x
  (unbox-fixnum imm1 y)                           ; ppc:115 unboxed-y
  (unbox-fixnum imm2 carry-in)                    ; ppc:116 unboxed-carry-in
  (mul imm3 imm0 imm1)                            ; ppc:117 mulld
  (add imm3 imm3 imm2)                            ; ppc:118
  (ubfx arg_z imm3 (:$ 0) (:$ 32))                          ; ppc:119 clrlsldi low (arg_z=carry-in consumed ppc:116)
  (box-fixnum arg_z arg_z)
  (lsr arg_y imm3 (:$ 32))                        ; ppc:120-121 clrrdi+srdi high (arg_y=y consumed ppc:115)
  (box-fixnum arg_y arg_y)
  (vpush arg_y)                                   ; ppc:122 vpush high
  (set-nargs 2)                                   ; ppc:123
  (vpush arg_z)                                   ; ppc:124 vpush low
  (add temp0 vsp (:$ (ash 2 arm64::fixnumshift))) ; ppc:125 la temp0 '2 vsp (= entry vsp)
  ;; ppc:126 (ba .SPvalues): TAIL no-link (DECIDE-10)
  (jump-subprim .SPvalues))

;;; =====================================================================
;;; %multiply-and-add-fixnum-loop — from ppc64-bignum.lisp:128
;;; =====================================================================
;;; result[0..rlen-1] = x[0..rlen-1] * unboxed-y + carry, over 64-bit
;;; words.  ld+rotldi / rotldi+stdx → plain ldr/str (W7-D50; x86:128-148
;;; twin uses plain movq).  rlen and i are boxed word counts.
;;; PPC register let: carry=imm4, iidx=imm3, unboxed-y=imm0, i=temp0,
;;; hi=imm2, rlen=temp1 — kept 1:1.
(defarm64lapfunction %multiply-and-add-fixnum-loop ((len64 0) (x arg_x) (y arg_y) (result arg_z))
  (vpop temp1)                                    ; ppc:135 rlen (vpop adjusts vsp — no exit la)
  (mov imm4 (:$ 0))                               ; ppc:136 li carry 0
  (mov imm3 (:$ arm64::misc-data-offset))         ; ppc:137 li iidx
  (mov temp0 (:$ 0))                              ; ppc:138 li i 0 (boxed 0)
  (b @test)                                       ; ppc:139
  @loop
  (unbox-fixnum imm0 y)                           ; ppc:141
  (ldr imm1 (:@ x imm3))                          ; ppc:142-143 ldx+rotldi x[i] → LE plain
  (umulh imm2 imm1 imm0)                          ; ppc:144 mulhdu hi
  (mul imm0 imm1 imm0)                            ; ppc:145 mulld low (dest = unboxed-y reg, as PPC)
  (adds imm0 imm0 imm4)                           ; ppc:146 addc — low += carry; C LIVE →
  (adc imm4 imm2 xzr)                             ; ppc:147 addze — carry = hi + C; C DEAD
  (str imm0 (:@ result imm3))                     ; ppc:148-149 rotldi+stdx → LE plain
  (add imm3 imm3 (:$ 8))                          ; ppc:150
  (add temp0 temp0 (:$ (ash 1 arm64::fixnumshift))) ; ppc:151 la i '1
  @test
  (cmp temp0 temp1)                               ; ppc:153 cmpd (signed)
  (b.lt @loop)                                    ; ppc:154
  (str imm4 (:@ result imm3))                     ; ppc:155-156 rotldi+stdx carry → LE plain
  (ret))                                          ; ppc:157

;;; =====================================================================
;;; %floor — from ppc64-bignum.lisp:163
;;; =====================================================================
;;; Return the (possibly truncated) 32-bit quotient and remainder from
;;; dividing hi:low by divisor.  Pure register arithmetic, order-neutral.
;;; divdu → udiv (64÷64 unsigned); remainder via mul+sub (PPC-faithful;
;;; msub exists but keep 1:1).
(defarm64lapfunction %floor ((num-high arg_x) (num-low arg_y) (divisor arg_z))
  (lsl imm0 num-high (:$ (- 32 arm64::fixnumshift))) ; ppc:169 sldi — boxed high → val<<32
  (unbox-fixnum imm1 num-low)                     ; ppc:170
  (unbox-fixnum imm2 divisor)                     ; ppc:171
  (orr imm0 imm0 imm1)                            ; ppc:172 combined 64-bit numerator
  (udiv imm3 imm0 imm2)                           ; ppc:173 divdu
  (mul imm4 imm3 imm2)                            ; ppc:174 mulld
  (sub imm4 imm0 imm4)                            ; ppc:175 rem = num - quo*divisor
  (ubfx arg_y imm3 (:$ 0) (:$ 32))                          ; ppc:176 clrlsldi — truncate quo to 32, box
  (box-fixnum arg_y arg_y)
  (ubfx arg_z imm4 (:$ 0) (:$ 32))                          ; ppc:177 clrlsldi — rem
  (box-fixnum arg_z arg_z)
  (mov temp0 vsp)                                 ; ppc:178 (entry vsp for .SPvalues)
  (vpush arg_y)                                   ; ppc:179 quotient
  (vpush arg_z)                                   ; ppc:180 remainder
  (set-nargs 2)                                   ; ppc:181
  ;; ppc:182 (ba .SPvalues): TAIL no-link (DECIDE-10)
  (jump-subprim .SPvalues))

;;; =====================================================================
;;; %multiply — from ppc64-bignum.lisp:186
;;; =====================================================================
;;; Multiply two (UNSIGNED-BYTE 32) arguments, return (VALUES high low)
;;; of the 64-bit result.  Order-neutral.
(defarm64lapfunction %multiply ((x arg_y) (y arg_z))
  (unbox-fixnum imm0 x)                           ; ppc:191
  (unbox-fixnum imm1 y)                           ; ppc:192
  (mul imm2 imm0 imm1)                            ; ppc:193 mulld
  (ubfx arg_y imm2 (:$ 0) (:$ 32))                          ; ppc:194 clrlsldi — arg_y = boxed low32
  (box-fixnum arg_y arg_y)
  (lsr imm2 imm2 (:$ 32))                         ; ppc:195 srdi
  (box-fixnum arg_z imm2)                         ; ppc:196 arg_z = boxed high32
  (mov temp0 vsp)                                 ; ppc:197
  (vpush arg_z)                                   ; ppc:198 high
  (set-nargs 2)                                   ; ppc:199
  (vpush arg_y)                                   ; ppc:200 low
  ;; ppc:201 (ba .SPvalues): TAIL no-link (DECIDE-10)
  (jump-subprim .SPvalues))

;;; =====================================================================
;;; %set-bignum-length — from ppc64-bignum.lisp:205
;;; =====================================================================
;;; Any words in the "tail" of the bignum should have been zeroed by the
;;; caller.  Boxed newlen<<(num-subtag-bits - fixnumshift) = count<<8,
;;; matching define-header (arch.lisp:653).  subtag-bignum (arch.lisp:143)
;;; is not a guaranteed logical-immediate pattern → PPC's ori becomes
;;; mov+orr(register) (W7-D51).
(defarm64lapfunction %set-bignum-length ((newlen arg_y) (bignum arg_z))
  (lsl imm0 newlen (:$ (- arm64::num-subtag-bits arm64::fixnumshift))) ; ppc:206 sldi
  (mov imm1 (:$ arm64::subtag-bignum))            ; ppc:207 ori → mov+orr (W7-D51)
  (orr imm0 imm0 imm1)
  (stur imm0 (:@ bignum (:$ arm64::misc-header-offset))) ; ppc:208 std (-4 → stur)
  (ret))                                          ; ppc:209

;;; =====================================================================
;;; %bignum-sign-bits — from ppc64-bignum.lisp:213
;;; =====================================================================
;;; Count the sign bits in the most significant digit; return fixnum.
;;; The MS digit is at byte offset misc-data-offset + 4*(count-1) — a
;;; 32-bit indexed access, endian-neutral (W7-D50; x86:213 same address
;;; arithmetic).  cntlzw → clz (W-form); not → mvn (W-form).
(defarm64lapfunction %bignum-sign-bits ((bignum arg_z))
  (vector-size imm0 bignum imm0)                  ; ppc:214 — digit count
  (lsl imm0 imm0 (:$ 2))                          ; ppc:215 sldi 2 — byte size
  (sub imm0 imm0 (:$ (- 4 arm64::misc-data-offset))) ; ppc:216 la — offset of MS digit (= +0 on this layout; kept symbolic)
  (ldr (:w imm0) (:@ bignum imm0))                ; ppc:217 lwzx (32-bit regoff, endian-neutral)
  (cmp (:w imm0) (:$ 0))                          ; ppc:218 cmpwi (signed 32-bit)
  (mvn (:w imm0) (:w imm0))                       ; ppc:219 not (flag-safe)
  (b.lt @wasneg)                                  ; ppc:220 — negative: count leading ones (via complement)
  (mvn (:w imm0) (:w imm0))                       ; ppc:221 not back — non-negative: count leading zeros
  @wasneg
  (clz (:w imm0) (:w imm0))                       ; ppc:223 cntlzw (W-form: counts within 32 bits)
  (box-fixnum arg_z imm0)                         ; ppc:224
  (ret))                                          ; ppc:225

;;; =====================================================================
;;; %signed-bignum-ref — from ppc64-bignum.lisp:227
;;; =====================================================================
;;; Sign-extended 32-bit digit read.  Boxed index n (= n<<3) >> 1 = 4n
;;; byte offset (fixnumshift 3, same as PPC64).  lwax → ldrsw (regoff,
;;; arm64-asm.lisp:721).  32-bit indexed access — endian-neutral (W7-D50).
(defarm64lapfunction %signed-bignum-ref ((bignum arg_y) (index arg_z))
  (lsr imm0 index (:$ 1))                         ; ppc:228 srdi 1 — boxed n → byte offset 4n
  (sub imm0 imm0 (:$ (- arm64::misc-data-offset)))    ; ppc:229 la
  (ldrsw imm0 (:@ bignum imm0))                   ; ppc:230 lwax — sign-extending 32-bit load
  (box-fixnum arg_z imm0)                         ; ppc:231
  (ret))                                          ; ppc:232

;;; =====================================================================
;;; %maybe-fixnum-from-one-or-two-digit-bignum — from ppc64-bignum.lisp:239
;;; =====================================================================
;;; One-digit → fixnum of the (signed) digit.  Two-digit → the 64-bit
;;; digit-pair value if it fits a fixnum, else NIL.  The two-digit read
;;; was ld+rotldi (BE digit swap) → plain ldur on LE (W7-D50; x86:245
;;; plain movq).  The one-digit read is lwa → ldrsw (32-bit, endian-
;;; neutral; scaled form, offset 4 = multiple of 4, arm64-asm.lisp:736).
;;; PPC uses cr1/cr2 for the two header compares — serialized here into
;;; sequential cmp/branch (one-NZCV rule); beqlr → b.eq to a ret label.
;;; Header immediates (1<<8|subtag, 2<<8|subtag ≤ #x2ff) fit cmp's
;;; 12-bit aimm.
(defarm64lapfunction %maybe-fixnum-from-one-or-two-digit-bignum ((bignum arg_z))
  (getvheader imm1 bignum)                        ; ppc:240 ld misc-header-offset
  (cmp imm1 (:$ arm64::one-digit-bignum-header))  ; ppc:241 cmpdi cr1 (serialized)
  (b.eq @one)                                     ; ppc:243 beq cr1
  (cmp imm1 (:$ arm64::two-digit-bignum-header))  ; ppc:242 cmpdi cr2 (serialized)
  (b.ne @no)                                      ; ppc:244 bne cr2
  (ldur imm0 (:@ bignum (:$ arm64::misc-data-offset))) ; ppc:245-246 ld+rotldi → LE plain (x86:245)
  (box-fixnum arg_z imm0)                         ; ppc:247 (truncates to 61 bits + tag)
  (unbox-fixnum imm1 arg_z)                       ; ppc:248 round-trip
  (cmp imm0 imm1)                                 ; ppc:249 — fits iff round-trip preserved
  (b.eq @done)                                    ; ppc:250 beqlr → branch to ret
  @no
  (mov arg_z rnil)                                ; ppc:252 li nil
  (ret)                                           ; ppc:253
  @one
  (ldursw imm0 (:@ bignum (:$ arm64::misc-data-offset))) ; ppc:255 lwa (signed 32-bit; -4 => LDURSW)
  (box-fixnum arg_z imm0)                         ; ppc:256
  @done
  (ret))                                          ; ppc:257

;;; =====================================================================
;;; %digit-logical-shift-right — from ppc64-bignum.lisp:260
;;; =====================================================================
;;; PPC srw is a 32-bit shift with count taken mod 64 (counts 32..63 →
;;; result 0).  A W-form lsr would take the count mod 32 (WRONG for
;;; count=32).  The 64-bit lsr on the zero-extended digit is exact:
;;; digit has only low-32 significance, count mod 64 identical → this is
;;; the x86-64 shape (x86:262-267 shrq).  W7-D52.
(defarm64lapfunction %digit-logical-shift-right ((digit arg_y) (count arg_z))
  (unbox-fixnum imm0 digit)                       ; ppc:261 (u32 digit, zero-extended by unbox)
  (unbox-fixnum imm1 count)                       ; ppc:262
  (lsr imm0 imm0 imm1)                            ; ppc:263 srw → 64-bit lsrv (W7-D52; x86:265)
  (box-fixnum arg_z imm0)                         ; ppc:264
  (ret))                                          ; ppc:265

;;; =====================================================================
;;; %ashr — from ppc64-bignum.lisp:267
;;; =====================================================================
;;; PPC sraw: arithmetic 32-bit shift, count mod 64 (counts 32..63 →
;;; all sign bits).  Exact 64-bit equivalent: sign-extend the digit to
;;; 64 bits (x86:274 movslq shape), then 64-bit asr — identical for all
;;; counts 0..63.  W7-D52.
(defarm64lapfunction %ashr ((digit arg_y) (count arg_z))
  (unbox-fixnum imm0 digit)                       ; ppc:268
  (unbox-fixnum imm1 count)                       ; ppc:269
  (sbfx imm0 imm0 (:$ 0) (:$ 32))                           ; x86:274 movslq — sign-extend digit
  (asr imm0 imm0 imm1)                            ; ppc:270 sraw → 64-bit asrv (x86:275)
  (box-fixnum arg_z imm0)                         ; ppc:271
  (ret))                                          ; ppc:272

;;; =====================================================================
;;; %ashl — from ppc64-bignum.lisp:274
;;; =====================================================================
;;; PPC slw (32-bit shift left, count mod 64) followed by clrlsldi
;;; (keep low 32, box): 64-bit lsl then extract low 32 is identical for
;;; all counts 0..63 (x86:279-285 shlq+movl shape).  W7-D52.
(defarm64lapfunction %ashl ((digit arg_y) (count arg_z))
  (unbox-fixnum imm0 digit)                       ; ppc:275
  (unbox-fixnum imm1 count)                       ; ppc:276
  (lsl imm0 imm0 imm1)                            ; ppc:277 slw → 64-bit lslv (x86:282)
  (ubfx imm0 imm0 (:$ 0) (:$ 32))                           ; ppc:278 clrlsldi — keep low digit (x86:283 movl)
  (box-fixnum arg_z imm0)                         ; ppc:278 (shift-in tag)
  (ret))                                          ; ppc:279

;;; =====================================================================
;;; macptr->fixnum — from ppc64-bignum.lisp:281
;;; =====================================================================
;;; If the macptr's address is 8-aligned it IS a fixnum on this tag
;;; scheme (tag-fixnum=0, tagmask=7 — same trick as PPC64); else NIL.
;;; (The x86:287 twin skips the alignment check; PPC's checked shape is
;;; the safe one — kept.)
(defarm64lapfunction macptr->fixnum ((ptr arg_z))
  (macptr-ptr imm0 ptr)                           ; ppc:282
  (ands imm1 imm0 (:$ arm64::tagmask))            ; ppc:283 andi. 7
  (mov arg_z rnil)                                ; ppc:284 li nil (flag-safe)
  (b.ne @done)                                    ; ppc:285
  (mov arg_z imm0)                                ; ppc:286 aligned address = fixnum
  @done
  (ret))                                          ; ppc:288

;;; =====================================================================
;;; fix-digit-logand — from ppc64-bignum.lisp:290 (index 0)
;;; =====================================================================
;;; AND of a fixnum with the first digit PAIR of big (64-bit access):
;;; ld+rotldi / rotldi+std → plain ldur/stur on LE (W7-D50; x86:294/302
;;; plain movq; kernel precedent spentry-B-vectors-misc.s:966).  If dest
;;; is NIL, return boxed result (PPC boxes with possible top-3-bit loss
;;; for negative fix — faithful, not "fixed"); else store into dest's
;;; digit pair.  cmp-to-nil then flag-safe and → b.ne, as PPC interleaves.
(defarm64lapfunction fix-digit-logand ((fix arg_x) (big arg_y) (dest arg_z))
  (ldur imm1 (:@ big (:$ arm64::misc-data-offset))) ; ppc:293+295 ld+rotldi → LE plain (w2)
  (unbox-fixnum imm0 fix)                         ; ppc:294 (w1)
  (cmp dest rnil)                                 ; ppc:296 cmpdi nil
  (and imm0 imm0 imm1)                            ; ppc:297 (flag-safe)
  (b.ne @store)                                   ; ppc:298
  (box-fixnum arg_z imm0)                         ; ppc:299
  (ret)                                           ; ppc:300
  @store
  (stur imm0 (:@ dest (:$ arm64::misc-data-offset))) ; ppc:302-303 rotldi+std → LE plain
  (ret))                                          ; ppc:304

;;; =====================================================================
;;; fix-digit-logandc2 — from ppc64-bignum.lisp:308
;;; =====================================================================
;;; fix AND NOT big-digit-pair.  andc w1,w2 → bic Rd,Rn,Rm (Rn & ~Rm):
;;; (andc imm1 imm0 imm1) = fix & ~big → (bic imm1 imm0 imm1).
;;; 64-bit pair access → plain ldur/stur (W7-D50; x86:308-317).
(defarm64lapfunction fix-digit-logandc2 ((fix arg_x) (big arg_y) (dest arg_z))
  (cmp dest rnil)                                 ; ppc:309
  (ldur imm1 (:@ big (:$ arm64::misc-data-offset))) ; ppc:310+312 ld+rotldi → LE plain (flag-safe)
  (unbox-fixnum imm0 fix)                         ; ppc:311 (flag-safe)
  (bic imm1 imm0 imm1)                            ; ppc:313 andc — fix & ~big (flag-safe)
  (b.ne @store)                                   ; ppc:314
  (box-fixnum arg_z imm1)                         ; ppc:315
  (ret)                                           ; ppc:316
  @store
  (stur imm1 (:@ dest (:$ arm64::misc-data-offset))) ; ppc:318-319 rotldi+std → LE plain
  (ret))                                          ; ppc:320

;;; =====================================================================
;;; fix-digit-logandc1 — from ppc64-bignum.lisp:322
;;; =====================================================================
;;; big-digit-pair AND NOT fix: (andc imm1 imm1 imm0) → (bic imm1 imm1 imm0).
(defarm64lapfunction fix-digit-logandc1 ((fix arg_x) (big arg_y) (dest arg_z))
  (cmp dest rnil)                                 ; ppc:323
  (ldur imm1 (:@ big (:$ arm64::misc-data-offset))) ; ppc:324+326 ld+rotldi → LE plain (flag-safe)
  (unbox-fixnum imm0 fix)                         ; ppc:325 (flag-safe)
  (bic imm1 imm1 imm0)                            ; ppc:327 andc — big & ~fix (flag-safe)
  (b.ne @store)                                   ; ppc:328
  (box-fixnum arg_z imm1)                         ; ppc:329
  (ret)                                           ; ppc:330
  @store
  (stur imm1 (:@ dest (:$ arm64::misc-data-offset))) ; ppc:332-333 rotldi+std → LE plain
  (ret))                                          ; ppc:334

;;; =====================================================================
;;; %bignum-logior — from ppc64-bignum.lisp:340
;;; =====================================================================
;;; Do LOGIOR on the N 32-bit digits of A and B into C, 64 bits at a
;;; time where possible.  Odd digit (if any) handled first with 32-bit
;;; ops (endian-neutral); the 64-bit loop is a BITWISE op on digit pairs
;;; — order-neutral, PPC itself does no rotldi here (W7-D50); port
;;; line-by-line.  vpopped boxed N (n<<3) >>1 = total byte count 4n.
;;; NZCV serialization is natural: andi.→b.eq, then cmpdi 4 survives
;;; flag-safe sub/ldr/orr/str to the beqlr; in-loop cmpdi 0 survives to
;;; the bne.  beqlr → b.eq @done (no conditional ret on arm64).
(defarm64lapfunction %bignum-logior ((n 0) (a arg_x) (b arg_y) (c arg_z))
  (vpop imm0)                                     ; ppc:341
  (lsr imm0 imm0 (:$ 1))                          ; ppc:342 srdi 1 — boxed n → byte count
  (ands imm1 imm0 (:$ 4))                         ; ppc:343 andi. — odd digit count?
  (sub imm3 imm0 (:$ (- arm64::misc-data-offset)))    ; ppc:344 la (flag-safe)
  (b.eq @loop)                                    ; ppc:345
  (cmp imm0 (:$ 4))                               ; ppc:346 — was that the ONLY digit?
  (sub imm0 imm0 (:$ 4))                          ; ppc:347 (flag-safe)
  (sub imm3 imm3 (:$ 4))                          ; ppc:348 (flag-safe)
  (ldr (:w imm1) (:@ a imm3))                     ; ppc:349 lwzx (32-bit, endian-neutral)
  (ldr (:w imm2) (:@ b imm3))                     ; ppc:350 lwzx
  (orr (:w imm1) (:w imm1) (:w imm2))             ; ppc:351 (flag-safe)
  (str (:w imm1) (:@ c imm3))                     ; ppc:352 stwx
  (b.eq @done)                                    ; ppc:353 beqlr
  @loop
  (sub imm0 imm0 (:$ 8))                          ; ppc:355
  (sub imm3 imm3 (:$ 8))                          ; ppc:356
  (cmp imm0 (:$ 0))                               ; ppc:357 (can't be equal on 1st iteration)
  (ldr imm1 (:@ a imm3))                          ; ppc:358 ldx — digit pair (bitwise: order-neutral)
  (ldr imm2 (:@ b imm3))                          ; ppc:359 ldx
  (orr imm1 imm1 imm2)                            ; ppc:360 (flag-safe)
  (str imm1 (:@ c imm3))                          ; ppc:361 stdx
  (b.ne @loop)                                    ; ppc:362
  @done
  (ret))                                          ; ppc:363

;;; =====================================================================
;;; %bignum-logand — from ppc64-bignum.lisp:370
;;; =====================================================================
;;; Identical shape to %bignum-logior with AND.
(defarm64lapfunction %bignum-logand ((n 0) (a arg_x) (b arg_y) (c arg_z))
  (vpop imm0)                                     ; ppc:371
  (lsr imm0 imm0 (:$ 1))                          ; ppc:372
  (ands imm1 imm0 (:$ 4))                         ; ppc:373
  (sub imm3 imm0 (:$ (- arm64::misc-data-offset)))    ; ppc:374 (flag-safe)
  (b.eq @loop)                                    ; ppc:375
  (cmp imm0 (:$ 4))                               ; ppc:376
  (sub imm0 imm0 (:$ 4))                          ; ppc:377 (flag-safe)
  (sub imm3 imm3 (:$ 4))                          ; ppc:378 (flag-safe)
  (ldr (:w imm1) (:@ a imm3))                     ; ppc:379 lwzx (32-bit, endian-neutral)
  (ldr (:w imm2) (:@ b imm3))                     ; ppc:380 lwzx
  (and (:w imm1) (:w imm1) (:w imm2))             ; ppc:381 (flag-safe)
  (str (:w imm1) (:@ c imm3))                     ; ppc:382 stwx
  (b.eq @done)                                    ; ppc:383 beqlr
  @loop
  (sub imm0 imm0 (:$ 8))                          ; ppc:385
  (sub imm3 imm3 (:$ 8))                          ; ppc:386
  (cmp imm0 (:$ 0))                               ; ppc:387
  (ldr imm1 (:@ a imm3))                          ; ppc:388 ldx (bitwise pair: order-neutral)
  (ldr imm2 (:@ b imm3))                          ; ppc:389 ldx
  (and imm1 imm1 imm2)                            ; ppc:390 (flag-safe)
  (str imm1 (:@ c imm3))                          ; ppc:391 stdx
  (b.ne @loop)                                    ; ppc:392
  @done
  (ret))                                          ; ppc:393
