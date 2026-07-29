;;;;-*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "CCL")

(defarm64lapfunction %fixnum-signum ((number arg_z))
  (cmp number (:$ 0))
  (cset imm0 (:? gt))                  ;1 if number > 0, else 0
  (csinv imm0 imm0 xzr (:? ge))        ;same (0 or 1) if number >= 0, else -1
  (lsl arg_z imm0 (:$ arm64::fixnumshift))
  (ret))

(defarm64lapfunction %ilogcount ((number arg_z))
  (fmov d0 number)
  (cnt (:8b d0) (:8b d0))
  (addv b0 (:8b d0))
  (fmov (:w imm0) (:s d0))
  (lsl arg_z imm0 (:$ arm64::fixnumshift))
  (ret))

;; positive count: shift left, negative count: shift right
(defarm64lapfunction %iash ((number arg_y) (count arg_z))
  (unbox-fixnum imm1 count)
  (negs imm2 imm1)
  (b.lt @left)
  (unbox-fixnum imm0 number)
  (asr imm0 imm0 imm2)
  (box-fixnum arg_z imm0)
  (ret)
  @left
  (lsl arg_z number imm1)
  (ret))

;; (integer-length n) = (- 63 (cls n))
(defarm64lapfunction %fixnum-intlen ((number arg_z))
  (unbox-fixnum imm0 number)
  (cls imm0 imm0)                       ;result in [0, 63]
  (eor imm0 imm0 (:$ 63))               ;trick: computes (- 63 cls)
  (box-fixnum arg_z imm0)
  (ret))

(defarm64lapfunction %truncate-double-float->fixnum ((arg arg_z))
  (get-double-float d0 arg)
  (fcvtzs imm0 d0)
  (box-fixnum arg_z imm0)
  (ret))

(defarm64lapfunction %truncate-single-float->fixnum ((arg arg_z))
  (get-single-float-bits imm0 arg)
  (fmov s0 (:w imm0))
  (fcvtzs imm0 s0)
  (box-fixnum arg_z imm0)
  (ret))

(defarm64lapfunction %round-nearest-double-float->fixnum ((arg arg_z))
  (get-double-float d0 arg)
  (fcvtns imm0 d0)
  (box-fixnum arg_z imm0)
  (ret))

(defarm64lapfunction %round-nearest-single-float->fixnum ((arg arg_z))
  (get-single-float-bits imm0 arg)
  (fmov s0 (:w imm0))
  (fcvtns imm0 s0)
  (box-fixnum arg_z imm0)
  (ret))

;;; The out-of-line complex-float constructors.  COMPLEX calls these by name
;;; (l0-numbers.lisp:1643-1654); the compiler open-codes the call when both
;;; component types are known, so what is missing here is only the full-call
;;; fallback -- which is nevertheless what every other port defines
;;; (ppc-numbers.lisp:503/516, x86-numbers.lisp:306/324, arm-numbers.lisp:
;;; 291/313).  PPC64 is the donor.
;;;
;;; The vinsns of the same name (arm64-vinsns.lisp:530/537) are NOT donors and
;;; are not alternatives: they build the value in a register (an fmov plus an
;;; `ins' into lane 1), which is the compiler's unboxed representation.  These
;;; build the boxed heap object.
;;;
;;; Allocation follows the kernel's own Misc_Alloc_Fixed macro
;;; (lisp-kernel/arm64-macros.s:64-70) instruction for instruction, which is
;;; what replaces PPC's always-true conditional trap (twllt allocptr
;;; allocbase):
;;;
;;;     sub allocptr, allocptr, #(size - fulltag_misc)
;;;     cmp allocptr, allocbase
;;;     b.hi 1f
;;;     uuo_alloc
;;;  1: str <header>, [allocptr, #misc_header_offset]
;;;     mov <dest>, allocptr
;;;     clear_allocptr_tag
;;;
;;; Note b.hi, not b.hs: the kernel traps when allocptr <= allocbase, so the
;;; skip is taken only on a strict unsigned greater-than.  clear_allocptr_tag
;;; is `bic allocptr, allocptr, #fulltagmask'; there is no bic-immediate
;;; template in arm64-asm.lisp, so it is spelled as the equivalent `and' with
;;; the complement (ldb-wrapped, because the encoder takes no negative
;;; immediates).
;;;
;;; Object sizes are 32 and 16 bytes, both multiples of the 16-byte dnode, so
;;; the new base inherits allocptr's dnode alignment -- which is what keeps
;;; complex-double-float.realpart 16-aligned.
;;;
;;; Layouts, from (define-fixedsized-object complex-double-float () pad
;;; realpart imagpart) and (define-fixedsized-object complex-single-float ()
;;; value) at arm64-arch.lisp:604-608 and :599-601:
;;;   complex-double-float: header @-12, pad @-4, realpart @+4, imagpart @+12
;;;     (the pad word is there for natural alignment, exactly as on PPC64;
;;;      realpart @+4 is the same displacement patch 0062 pinned down)
;;;   complex-single-float: header @-12, value @-4, and
;;;     complex-single-float.realpart = value = -4,
;;;     complex-single-float.imagpart = value + 4 = 0.
;;; Header element counts are in 32-bit units: 6 for the double (pad + two
;;; doubles = 24 bytes) and 2 for the single, matching PPC64's own
;;; #+ppc64-target arms at ppc:506 and ppc:519.

(defarm64lapfunction %make-complex-double-float ((r arg_y) (i arg_z))
  (get-double-float d0 r)                  ; ppc:504
  (get-double-float d1 i)                  ; ppc:505 -- before arg_z is clobbered
  ;; ppc:506 (li imm0 (logior (ash 6 8) subtag-complex-double-float))
  (mov imm0 (:$ (logior (ash 6 arm64::num-subtag-bits)
                        arm64::subtag-complex-double-float)))
  ;; ppc:507-508 (subi/twllt) -> Misc_Alloc_Fixed, arm64-macros.s:64-70
  (sub allocptr allocptr (:$ (- 32 arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hi @no-trap)
  (uuo-alloc-trap)
  @no-trap
  (stur imm0 (:@ allocptr (:$ arm64::misc-header-offset))) ; ppc:509
  (mov arg_z allocptr)                     ; ppc:510
  ;; ppc:511 (clrrri allocptr allocptr ntagbits) = clear_allocptr_tag
  (and allocptr allocptr (:$ (ldb (byte 64 0) (lognot arm64::fulltagmask))))
  ;; ppc:512-513 (stfd).  Both displacements are positive but not 8-scaled
  ;; multiples of 8 from the TAGGED pointer, so unscaled stur.
  (stur d0 (:@ arg_z (:$ arm64::complex-double-float.realpart)))
  (stur d1 (:@ arg_z (:$ arm64::complex-double-float.imagpart)))
  (ret))                                   ; ppc:514

;;; ARM64-DEVIATION: PPC round-trips the two singles through FPRs (get-single-
;;; float + stfs) because a PPC single-float store has to come from an FPR.
;;; Here a single-float is immediate -- its IEEE bits sit in the tagged word --
;;; so get-single-float-bits lands them in a GPR and a 32-bit W store writes
;;; them out directly; the FPR hop would be two redundant fmovs.  Same bits.
(defarm64lapfunction %make-complex-single-float ((r arg_y) (i arg_z))
  (get-single-float-bits imm1 r)           ; ppc:517 (get-single-float fp0 r)
  (get-single-float-bits imm2 i)           ; ppc:518 -- before arg_z is clobbered
  ;; ppc:519 (li imm0 (logior (ash 2 8) subtag-complex-single-float))
  (mov imm0 (:$ (logior (ash 2 arm64::num-subtag-bits)
                        arm64::subtag-complex-single-float)))
  ;; ppc:520-521 (subi/twllt) -> Misc_Alloc_Fixed
  (sub allocptr allocptr (:$ (- 16 arm64::fulltag-misc)))
  (cmp allocptr allocbase)
  (b.hi @no-trap)
  (uuo-alloc-trap)
  @no-trap
  (stur imm0 (:@ allocptr (:$ arm64::misc-header-offset))) ; ppc:522
  (mov arg_z allocptr)                     ; ppc:523
  (and allocptr allocptr (:$ (ldb (byte 64 0) (lognot arm64::fulltagmask)))) ; ppc:524
  ;; ppc:525 (stfs fp0 complex-single-float.realpart arg_z).  realpart is -4,
  ;; and str's 32-bit scaled form takes an UNSIGNED offset (:uoff2,
  ;; arm64-asm.lisp:740), so this must be the unscaled stur (:680) -- the draft
  ;; had `str' here and would not have assembled.
  (stur (:w imm1) (:@ arg_z (:$ arm64::complex-single-float.realpart)))
  ;; ppc:526.  imagpart is 0, so either form encodes; keep stur for symmetry.
  (stur (:w imm2) (:@ arg_z (:$ arm64::complex-single-float.imagpart)))
  (ret))                                   ; ppc:527
