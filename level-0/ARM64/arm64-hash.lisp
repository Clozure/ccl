;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; arm64-hash.lisp — ARM64 LAP function drafts for hash functions
;;;;
;;;; Ported line-by-line from vendor/ccl/level-0/PPC/ppc-hash.lisp (PPC64 arms).
;;;; Register map: Matt Emerson's upstream arm64 (arm64-asm.lisp).
;;;; Tags: LOW tags, fixnumshift=3, misc-data-offset=4, misc-header-offset=-4.
;;;;
;;;; STATUS: LEAD-VERIFIED 2026-07-08 (line-by-line vs PPC64; assemble-gate clean
;;;; except DECIDE-blocked sites — see drafts/wave1-verify-report.md)

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "HASHENV" "ccl:xdump;hashenv")
  (require "ARM64-LAPMACROS"))

;;; =====================================================================
;;; fast-mod — from vendor/ccl/level-0/PPC/ppc-hash.lisp:30
;;; =====================================================================
;;;
;;; Equivalent to cl:mod when both args are positive fixnums.
;;; PPC64: (divdu imm0 number divisor) (mulld arg_z imm0 divisor)
;;;        (subf arg_z arg_z number)
;;; ARM64: udiv + msub (or udiv + mul + sub).
;;; Since both args are fixnums (tag bits = low 3 zeros), the division
;;; and multiply work on the raw fixnum representation and the tag
;;; bits cancel out correctly: number mod divisor = number - (number/divisor)*divisor
;;; (all as fixnums, since fixnum arithmetic is closed under these ops
;;; when both are positive and result < divisor).

(defarm64lapfunction fast-mod ((number arg_y) (divisor arg_z))
  ;; udiv imm0, number, divisor (unsigned integer divide)
  (udiv imm0 number divisor)
  ;; msub arg_z, imm0, divisor, number  =  number - imm0*divisor
  (msub arg_z imm0 divisor number)
  (ret))

;;; =====================================================================
;;; fast-mod-3 — from vendor/ccl/level-0/PPC/ppc-hash.lisp:43
;;; =====================================================================
;;;
;;; Fast modulo using a reciprocal approximation.
;;; PPC64: srdi, mulhd, mulld, sub, sub, srari, and, add.
;;; The algorithm: unbox number, multiply high by recip, multiply low by
;;; divisor to get quotient*divisor, subtract twice, then fix up with
;;; conditional add.
;;;
;;; On ARM64: smulh replaces mulhd (signed multiply high), umulh for unsigned.
;;; PPC64 uses mulhd which is SIGNED multiply-high.

(defarm64lapfunction fast-mod-3 ((number arg_x) (divisor arg_y) (recip arg_z))
  ;; (srdi imm0 number ppc64::fixnumshift) — unbox number
  (lsr imm0 number (:$ arm64::fixnumshift))
  ;; (mulhd imm1 imm0 recip) — signed multiply high
  (smulh imm1 imm0 recip)
  ;; (mulld imm0 imm1 divisor) — quotient * divisor
  (mul imm0 imm1 divisor)
  ;; (sub number number imm0)
  (sub number number imm0)
  ;; (sub number number divisor)
  (sub number number divisor)
  ;; (srari imm0 number (1- target::nbits-in-word)) — arithmetic shift right 63
  ;; This produces 0 if number >= 0, -1 if number < 0 (sign extension)
  (asr imm0 number (:$ (1- arm64::nbits-in-word)))
  ;; (and divisor divisor imm0) — divisor if number < 0, else 0
  (and divisor divisor imm0)
  ;; (add arg_z number divisor)
  (add arg_z number divisor)
  (ret))

;;; =====================================================================
;;; %dfloat-hash — from vendor/ccl/level-0/PPC/ppc-hash.lisp:70 (ppc64 arm)
;;; =====================================================================
;;;
;;; PPC64: load 64-bit double value, box as fixnum.
;;; On arm64, double-float.value = misc-data-offset = 4.

(defarm64lapfunction %dfloat-hash ((key arg_z))
  ;; (ld imm0 ppc64::double-float.value key)
  (ldur imm0 (:@ key (:$ arm64::double-float.value)))
  ;; (box-fixnum arg_z imm0)
  (box-fixnum arg_z imm0)
  (ret))

;;; =====================================================================
;;; %sfloat-hash — from vendor/ccl/level-0/PPC/ppc-hash.lisp:82 (ppc64 arm)
;;; =====================================================================
;;;
;;; PPC64 version: checks for negative zero (0x80000000 in upper 32 bits),
;;; returns 0 for -0.0, otherwise shifts the float bits to a fixnum.
;;; On arm64: single-floats are IMMEDIATE (IEEE bits in upper 32 of tagged word).
;;; The hash is derived from those bits.
;;;
;;; PPC64 code:
;;;   (lis imm0 #x8000)          ; imm0 = 0x80000000
;;;   (srdi imm1 key 32)         ; imm1 = upper 32 bits of tagged sf
;;;   (cmpw imm0 imm1)           ; is it negative zero?
;;;   (srdi arg_z key (- 32 ppc64::fixnumshift))  ; shift to fixnum
;;;   (bnelr)                     ; not neg-zero, return
;;;   (li arg_z 0)               ; neg-zero hashes to 0
;;;   (blr)
;;;
;;; On arm64 with immediate single-floats: same logic applies.
;;; The tagged single-float has IEEE bits in [63:32] and tag in [2:0].
;;; Extract upper 32 bits, check for -0 (0x80000000), produce fixnum.

(defarm64lapfunction %sfloat-hash ((key arg_z))
  ;; (lis imm0 #x8000) — load 0x80000000
  (mov imm0 (:$ #x80000000))
  ;; (srdi imm1 key 32) — extract upper 32 bits (IEEE float value)
  (lsr imm1 key (:$ 32))
  ;; (cmpw imm0 imm1) — is it negative zero?
  (cmp imm0 imm1)
  ;; (srdi arg_z key (- 32 ppc64::fixnumshift)) — shift to fixnum position
  ;; On arm64: fixnumshift=3, so shift right by 29 (= 32-3)
  (lsr arg_z key (:$ (- 32 arm64::fixnumshift)))
  ;; Clear the tag bits that leaked into the fixnum.
  ;; ldb wrap: Matt's encode-logical-immediate rejects negative lisp
  ;; integers, so the lognot mask must be expressed as unsigned 64-bit.
  (and arg_z arg_z (:$ (ldb (byte 64 0) (lognot arm64::fixnummask))))
  (b.ne @done)
  ;; Negative zero — hash to 0
  (mov arg_z (:$ 0))
  @done
  (ret))

;;; =====================================================================
;;; %macptr-hash — from vendor/ccl/level-0/PPC/ppc-hash.lisp:91
;;; =====================================================================
;;;
;;; PPC64: load macptr address, add high-shifted version, clear low bits
;;; for fixnum.
;;; (ldr imm0 target::macptr.address key)
;;; (slri imm1 imm0 24)              — shift right by 24 (PPC slri=shift LEFT??)
;;; Wait: in the INVENTORY.md, the macros used are: clrrri, ldr, slri.
;;; PPC source: (slri imm1 imm0 24) (add imm0 imm0 imm1) (clrrri arg_z imm0 fixnumshift)
;;; From the transform table: slri = shift LEFT immediate.
;;; So: imm1 = imm0 << 24?? That doesn't make sense for hashing (would lose bits).
;;; Let me re-read the PPC source carefully.

;;; PPC source (ppc-hash.lisp:91-96):
;;;   (ldr imm0 target::macptr.address key)
;;;   (slri imm1 imm0 24)
;;;   (add imm0 imm0 imm1)
;;;   (clrrri arg_z imm0 target::fixnumshift)
;;;
;;; Wait — looking at the lapmacros-report.md transform table:
;;;   slri (dest, src, n) -> lsl dest, src, #n  (Shift left immediate)
;;;   srri (dest, src, n) -> lsr dest, src, #n  (Logical shift right immediate)
;;;
;;; So (slri imm1 imm0 24) = imm1 = imm0 << 24.  That means we add the
;;; address shifted LEFT by 24 to itself.  This mixes high bits into the
;;; hash... hmm, this IS a hash function so spreading bits is the point.
;;; Actually wait — re-reading ppc-lapmacros.lisp more carefully:
;;; PPC sldi = shift left doubleword immediate = "slri" macro in the codebase?
;;; Let me check: the INVENTORY uses "slri" as "shift left register immediate".
;;; From ppc-lapmacros.lisp, slri is defined as rldicr (rotate left doubleword
;;; immediate then clear right) which IS shift-left.
;;; BUT looking at the hash context: adding (addr << 24) to addr makes the hash
;;; LARGER, mixing low bits with high bits.  That's reasonable for a pointer hash.
;;; Actually I think this is srri (shift RIGHT) not slri.  Let me re-check.
;;;
;;; Re-reading ppc-hash.lisp:91-96 raw:
;;;   (defppclapfunction %macptr-hash ((key arg_z))
;;;     (ldr imm0 target::macptr.address key)
;;;     (slri imm1 imm0 24)
;;;     (add imm0 imm0 imm1)
;;;     (clrrri arg_z imm0 target::fixnumshift)
;;;     (blr))
;;;
;;; With slri = shift LEFT by 24: imm1 = addr << 24, result = addr + (addr<<24)
;;; with low fixnumshift bits cleared.  This is the hash.  It works because
;;; addresses are typically in a limited range and the left-shift spreads
;;; the interesting bits (byte offsets 0-15) into the upper portion.
;;; On ARM64: lsl for shift left.

(defarm64lapfunction %macptr-hash ((key arg_z))
  ;; (ldr imm0 target::macptr.address key)
  (macptr-ptr imm0 key)
  ;; (slri imm1 imm0 24) — shift left by 24
  (lsl imm1 imm0 (:$ 24))
  ;; (add imm0 imm0 imm1)
  (add imm0 imm0 imm1)
  ;; (clrrri arg_z imm0 target::fixnumshift) — clear low fixnumshift bits
  ;; ARM64: and with ~((1<<fixnumshift)-1) = ~7; ldb wrap because Matt's
  ;; encode-logical-immediate rejects negative lisp integers.
  (and arg_z imm0 (:$ (ldb (byte 64 0) (lognot (1- (ash 1 arm64::fixnumshift))))))
  (ret))

;;; =====================================================================
;;; %bignum-hash — from vendor/ccl/level-0/PPC/ppc-hash.lisp:121 (ppc64 arm)
;;; =====================================================================
;;;
;;; Hash a bignum by XOR-accumulating 32-bit digits with rotate.
;;; PPC64: rotldi (64-bit rotate left by 13), add accumulation, lwzx loads.
;;; Loop over 32-bit digits (header-size gives count of 32-bit elements).

(defarm64lapfunction %bignum-hash ((key arg_z))
  (let ((header imm3)
        (offset imm2)
        (ndigits imm1)
        (immhash imm0))
    (mov immhash (:$ 0))
    (mov offset (:$ arm64::misc-data-offset))
    (getvheader header key)
    (header-size ndigits header)
    (let ((next header))           ; reuse header reg as temp for loaded word
      @loop
      ;; PPC64 uses cmpdi+bne with pre-decrement compare; ARM64: decrement
      ;; then cbnz (equivalent: loop while count-after-decrement > 0).
      (sub ndigits ndigits (:$ 1))
      ;; (lwzx next key offset) — 32-bit digit load; w3 = W alias of
      ;; next/header/imm3/x3 (Matt's arm64-asm.lisp:146); avoids
      ;; over-reading past the last digit.
      (add imm4 key offset)
      (ldr w3 (:@ imm4 (:$ 0)))
      ;; (rotldi immhash immhash 13) — 64-bit rotate left by 13 = ror #51
      (ror immhash immhash (:$ 51))
      ;; (addi offset offset 4)
      (add offset offset (:$ 4))
      ;; (add immhash immhash next)
      (add immhash immhash next)
      (cbnz ndigits @loop))
    ;; (clrrdi arg_z immhash ppc64::fixnumshift) — clear low 3 bits for
    ;; fixnum; ldb wrap for Matt's negative-immediate encoder restriction.
    (and arg_z immhash (:$ (ldb (byte 64 0) (lognot (1- (ash 1 arm64::fixnumshift))))))
    (ret)))

;;; =====================================================================
;;; %get-fwdnum — from vendor/ccl/level-0/PPC/ppc-hash.lisp:143
;;; =====================================================================
;;;
;;; Return the GC forwarding generation number (a fixnum kernel global).

(defarm64lapfunction %get-fwdnum ()
  (ref-global arg_z fwdnum)
  (ret))

;;; =====================================================================
;;; %get-gc-count — from vendor/ccl/level-0/PPC/ppc-hash.lisp:148
;;; =====================================================================
;;;
;;; Return the GC call count (a fixnum kernel global).

(defarm64lapfunction %get-gc-count ()
  (ref-global arg_z gc-count)
  (ret))

;;; =====================================================================
;;; %set-hash-table-vector-key — from vendor/ccl/level-0/PPC/ppc-hash.lisp:155
;;; =====================================================================
;;;
;;; Setting a key needs to ensure the vector header gets memoized (GC barrier).
;;; Tail-calls .SPset-hash-key subprim.

(defarm64lapfunction %set-hash-table-vector-key ((vector arg_x) (index arg_y) (value arg_z))
  (jump-subprim .SPset-hash-key))

;;; =====================================================================
;;; %set-hash-table-vector-key-conditional — from vendor/ccl/level-0/PPC/ppc-hash.lisp:158
;;; =====================================================================
;;;
;;; Conditional key set — tail-calls .SPset-hash-key-conditional.
;;; Note: has a stack arg (offset at vsp+0).

(defarm64lapfunction %set-hash-table-vector-key-conditional ((offset 0) (vector arg_x) (old arg_y) (new arg_z))
  (jump-subprim .SPset-hash-key-conditional))

;;; =====================================================================
;;; strip-tag-to-fixnum — from vendor/ccl/level-0/PPC/ppc-hash.lisp:162
;;; =====================================================================
;;;
;;; Strip tag bits from x, producing a fixnum.
;;; PPC64: (clrlri. imm0 arg_z (- nbits-in-word fixnumshift))
;;;        (beq @done)                    ; already a fixnum (low bits zero)
;;;        (clrrri arg_z x ntagbits)      ; clear low ntagbits
;;;        (srri arg_z arg_z (- ntagbits fixnumshift))  ; shift to fixnum
;;;        @done (blr)
;;;
;;; On arm64: ntagbits=4, fixnumshift=3, nbits-in-word=64.
;;; clrlri. with n=(64-3)=61 clears the top 61 bits and sets flags:
;;;   ands imm0, arg_z, #0x7 (tagmask)
;;; If the low 3 bits are zero, it's already a fixnum — return as-is.
;;; Otherwise: clear low 4 bits (fulltag), shift right by (4-3)=1.
;;; = (arg_z & ~0xF) >> 1

(defarm64lapfunction strip-tag-to-fixnum ((x arg_z))
  ;; (clrlri. imm0 arg_z (- target::nbits-in-word target::fixnumshift))
  ;; = ands imm0, arg_z, #((1<<fixnumshift)-1) = ands imm0, arg_z, #7
  (ands imm0 arg_z (:$ arm64::fixnummask))
  (b.eq @done)
  ;; (clrrri arg_z x target::ntagbits) — clear low ntagbits bits
  ;; = and arg_z, x, ~((1<<ntagbits)-1) = and arg_z, x, ~0xF;
  ;; ldb wrap for Matt's negative-immediate encoder restriction.
  (and arg_z x (:$ (ldb (byte 64 0) (lognot (1- (ash 1 arm64::ntagbits))))))
  ;; (srri arg_z arg_z (- target::ntagbits target::fixnumshift))
  ;; = lsr arg_z, arg_z, #(4-3) = lsr arg_z, arg_z, #1
  (lsr arg_z arg_z (:$ (- arm64::ntagbits arm64::fixnumshift)))
  @done
  (ret))
