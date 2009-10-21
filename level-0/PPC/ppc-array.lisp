;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  #+ppc32-target
  (require "PPC32-ARCH")
  #+ppc64-target
  (require "PPC64-ARCH")
  (require "PPC-LAPMACROS"))


;;; Users of this shouldn't make assumptions about return value.


#+ppc32-target
(eval-when (:compile-toplevel :execute)
;;; Assumptions made by %init-misc
  (assert (and (< ppc32::max-32-bit-ivector-subtag
                  ppc32::max-8-bit-ivector-subtag
                  ppc32::max-16-bit-ivector-subtag)
               (eql ppc32::max-32-bit-ivector-subtag ppc32::subtag-simple-base-string)
               (eql ppc32::max-16-bit-ivector-subtag ppc32::subtag-s16-vector)
               (eql ppc32::max-8-bit-ivector-subtag 223))))

#+ppc32-target
(defppclapfunction %init-misc ((val arg_y)
                               (miscobj arg_z))
  (getvheader imm0 miscobj)
  (header-size imm3 imm0)
  (cmpwi cr3 imm3 0)
  (extract-fulltag imm1 imm0)
  (cmpwi cr0 imm1 ppc32::fulltag-nodeheader)
  (extract-lowbyte imm2 imm0)
  (beqlr cr3)                           ; Silly 0-length case
  (li imm4 ppc32::misc-data-offset)
  (bne cr0 @imm)
  ; Node vector.  Don't need to memoize, since initial value is
  ; older than vector.
  @node-loop
  (cmpwi cr0 imm3 1)
  (subi imm3 imm3 1)
  (stwx val miscobj imm4)
  (la imm4 4 imm4)
  (bne cr0 @node-loop)
  (blr)
  @imm
  (cmpwi cr0 imm2 ppc32::subtag-double-float-vector)
  (cmpwi cr1 imm2 ppc32::max-32-bit-ivector-subtag)
  (cmpwi cr2 imm2 ppc32::max-8-bit-ivector-subtag)
  (cmpwi cr3 imm2 ppc32::max-16-bit-ivector-subtag)
  (extract-typecode imm0 val :CR6)		; don't clobber CR0
  (cmpwi cr7 imm0 ppc32::tag-fixnum)
  (beq cr0 @dfloat)
  (ble cr1 @32)
  (ble cr2 @8)
  (ble cr3 @16)
  ; Bit vector.
  (cmplwi cr0 val '1)
  (la imm3 31 imm3)
  (srwi imm3 imm3 5)
  (unbox-fixnum imm0 val)
  (neg imm0 imm0)
  (ble+ cr0 @set-32)
  @bad
  (li arg_x '#.$xnotelt)
  (save-lisp-context)
  (set-nargs 3)
  (call-symbol %err-disp)
  @dfloat
  (cmpwi cr0 imm0 ppc32::subtag-double-float)
  (li imm4 ppc32::misc-dfloat-offset)
  (bne- cr0 @bad)
  (lfd fp0 ppc32::double-float.value val)
  @dfloat-loop
  (cmpwi cr0 imm3 1)
  (subi imm3 imm3 1)
  (stfdx fp0 miscobj imm4)
  (la imm4 8 imm4)
  (bne cr0 @dfloat-loop)
  (blr)
  @32
  (cmpwi cr4 imm2 ppc32::subtag-s32-vector)
  (cmpwi cr0 imm2 ppc32::subtag-single-float-vector)
  (cmpwi cr2 imm0 ppc32::subtag-bignum)
  (cmpwi cr3 imm2 ppc32::subtag-fixnum-vector)
  (beq cr1 @char32)                      ; ppc32::max-32-bit-ivector-subtag
  (beq cr4 @s32)
  (beq cr3 @fixnum)
  (bne cr0 @u32)
  ;@sfloat
  (cmpwi cr0 imm0 ppc32::subtag-single-float)
  (bne- cr0 @bad)
  (lwz imm0 ppc32::single-float.value val)
  (b @set-32)
  @fixnum
  (unbox-fixnum imm0 val)
  (beq+ cr7 @set-32)
  (b @bad)
  @char32
  (unbox-base-char imm0 val cr0)
  (b @set-32)
  @s32
  (unbox-fixnum imm0 val)
  (beq+ cr7 @set-32)
  (bne- cr2 @bad)
  (getvheader imm0 val)
  (cmpwi cr0 imm0 (logior (ash 1 ppc32::num-subtag-bits) ppc32::subtag-bignum))
  (lwz imm0 ppc32::misc-data-offset val)
  (beq+ cr0 @set-32)
  (b @bad)
  @u32
  (extract-unsigned-byte-bits. imm0 val 30)
  (unbox-fixnum imm0 val)
  (beq cr0 @set-32)
  (bne- cr2 @bad)
  ; a one-digit bignum is ok if that digit is positive.
  ; a two-digit bignum is ok if the sign-digit is 0.
  (getvheader imm0 val)
  (cmpwi cr2 imm0 (logior (ash 2 ppc32::num-subtag-bits) ppc32::subtag-bignum))
  (lwz imm0 ppc32::misc-data-offset val)
  (cmpwi cr3 imm0 0)
  (bgt- cr2 @bad)                       ; more than two digits.
  (beq cr2 @two-digits)
  (bgt+ cr3 @set-32)
  (b @bad)
  @two-digits
  (lwz imm1 (+ 4 ppc32::misc-data-offset) val)
  (cmpwi cr0 imm1 0)
  (bne- cr0 @bad)
  (b @set-32)
  @16
  (cmpwi cr0 imm2 ppc32::subtag-u16-vector)
  (la imm3 1 imm3)
  (srwi imm3 imm3 1)
  (beq cr3 @s16)                        ; ppc32::max-16-bit-ivector-subtag
  (extract-unsigned-byte-bits. imm0 val 16)
  (unbox-fixnum imm0 val)
  (beq+ cr0 @set-16)
  (b @bad)
  @s16
  (slwi imm0 val (- 32 (+ 16 ppc32::fixnumshift)))
  (srawi imm0 imm0 (- 32 (+ 16 ppc32::fixnumshift)))
  (cmpw cr0 imm0 val)
  (unbox-fixnum imm0 val)
  (bne- cr7 @bad)
  (beq+ cr0 @set-16)
  (b @bad)
  @8
  (cmpwi cr0 imm0 ppc32::subtag-s8-vector)
  (la imm3 3 imm3)
  (srwi imm3 imm3 2)
  (beq cr2 @char8)                      ; ppc32::max-8-bit-ivector-subtag
  (beq cr0 @s8)
  (extract-unsigned-byte-bits. imm0 val 8)
  (unbox-fixnum imm0 val)
  (beq+ cr0 @set-8)
  (b @bad)
  @s8
  (slwi imm0 val (- 32 (+ 8 ppc32::fixnumshift)))
  (srawi imm0 imm0 (- 32 (+ 8 ppc32::fixnumshift)))
  (cmpw cr0 imm0 val)
  (unbox-fixnum imm0 val)
  (bne- cr7 @bad)
  (beq+ cr0 @set-8)
  (b @bad)
  @char8
  (unbox-base-char imm0 val cr0)   ; this type checks val
  @set-8                                ; propagate low 8 bits into low 16
  (rlwimi imm0 imm0 8 (- 32 16) (- 31 8))
  @set-16                               ; propagate low 16 bits into high 16
  (rlwimi imm0 imm0 16 0 (- 31 16))
  @set-32
  (cmpwi cr0 imm3 1)
  (subi imm3 imm3 1)
  (stwx imm0 miscobj imm4)
  (la imm4 4 imm4)
  (bne cr0 @set-32)
  (blr))

#+ppc64-target
(defppclapfunction %init-misc ((val arg_y)
                               (miscobj arg_z))
  (getvheader imm0 miscobj)
  ;(extract-lowtag imm2 imm0)
  (clrldi imm2 imm0 (- 64 ppc64::nlowtagbits))
  (header-size imm3 imm0)
  (cmpdi cr3 imm3 0)
  (extract-fulltag imm1 imm0)
  (cmpdi cr0 imm2 ppc64::lowtag-nodeheader)
  (extract-lowbyte imm2 imm0)
  (beqlr cr3)                           ; Silly 0-length case
  (li imm4 ppc64::misc-data-offset)
  (bne cr0 @imm)
  ;; Node vector.  Don't need to memoize, since initial value is
  ;; older than vector.
  @node-loop
  (cmpdi cr0 imm3 1)
  (subi imm3 imm3 1)
  (stdx val miscobj imm4)
  (la imm4 ppc64::node-size imm4)
  (bne cr0 @node-loop)
  (blr)
  @imm
  (extract-typecode imm0 val)		
  (cmpdi cr0 imm1 ppc64::ivector-class-64-bit)
  (cmpdi cr1 imm1 ppc64::ivector-class-32-bit)
  (cmpdi cr2 imm1 ppc64::ivector-class-8-bit)
  (cmpwi cr7 imm0 ppc64::tag-fixnum)
  (beq cr0 @64)
  (beq cr1 @32)
  (beq cr2 @8)
  ;; u16, s16, or bit-vector.  Val must be a fixnum.
  (cmpdi cr0 imm2 ppc64::subtag-u16-vector)
  (cmpdi cr1 imm2 ppc64::subtag-s16-vector)
  (bne cr7 @bad)                        ; not a fixnum
  (beq cr0 @u16)
  (beq cr1 @s16)
  ; Bit vector.
  (cmpldi cr0 val '1)
  (la imm3 31 imm3)
  (srdi imm3 imm3 5)
  (unbox-fixnum imm0 val)
  (neg imm0 imm0)
  (ble+ cr0 @set-32)
  @bad
  (li arg_x '#.$xnotelt)
  (save-lisp-context)
  (set-nargs 3)
  (call-symbol %err-disp)
  @64
  (cmpdi cr3 imm2 ppc64::subtag-fixnum-vector)
  (cmpdi cr1 imm2 ppc64::subtag-double-float-vector)
  (cmpdi cr2 imm2 ppc64::subtag-s64-vector)
  (beq cr3 @fixnum)
  (beq cr1 @dfloat)
  (beq cr2 @u64)
  ;; s64
  (unbox-fixnum imm0 val)
  (beq cr7 @set-64)                     ; all fixnums are (SIGNED-BYTE 64)
  (bne cr3 @bad)                        ; as are 2-digit bignums
  (getvheader imm1 val)
  (ld imm0 ppc64::misc-data-offset val)
  (cmpdi imm1 ppc64::two-digit-bignum-header)
  (rotldi imm0 imm0 32)
  (beq @set-64)
  (b @bad)
@fixnum
  (unbox-fixnum imm0 val)
  (beq cr7 @set-64)                     ; all fixnums are (SIGNED-BYTE 64)
  (b  @bad)                        ; as are 2-digit bignums
   ;; u64 if fixnum and positive, 2-digit bignum and positive, or
  ;; 3-digit bignum with most-significant digit 0.
  @u64
  (cmpdi cr2 val 0)
  (bne cr7 @u64-maybe-bignum)
  (bge cr2 @set-64)
  (b @bad)
  @u64-maybe-bignum
  (bne cr3 @bad)
  (ld imm0 ppc64::misc-data-offset val)
  (getvheader imm1 val)
  (rotldi imm0 imm0 32)
  (cmpdi cr2 imm1 ppc64::two-digit-bignum-header)
  (cmpdi cr3 imm1 ppc64::three-digit-bignum-header)
  (cmpdi cr0 imm0 0)
  (beq cr2 @u32-two-digit)
  (bne cr3 @bad)
  (lwz imm1 (+ 8 ppc64::misc-data-offset) val)
  (cmpwi imm0 0)
  (beq @set-64)
  (b @bad)
  @u32-two-digit
  (bgt cr0 @set-64)
  (b @bad)
  @dfloat
  (cmpdi cr0 imm0 ppc64::subtag-double-float)
  (bne- cr0 @bad)
  (ld imm0 ppc64::double-float.value val)
  (b @set-64)
  @32
  (cmpdi cr3 imm2 ppc64::subtag-simple-base-string)
  (cmpdi cr2 imm2 ppc64::subtag-s32-vector)
  (cmpdi cr0 imm2 ppc64::subtag-single-float-vector)
  (beq cr3 @char32)
  (beq cr2 @s32)
  (bne cr0 @u32)
  ;@sfloat
  (cmpdi cr0 imm0 ppc64::subtag-single-float)
  (srdi imm0 val 32)
  (bne- cr0 @bad)
  (b @set-32)
  @s32
  ;; Must be a fixnum (and a (SIGNED-BYTE 32)).
  (bne cr7 @bad)
  (unbox-fixnum imm0 val)
  (sldi imm1 imm0 32)
  (sradi imm1 imm1 32)
  (cmpd imm1 imm0)
  (bne @bad)
  (b @set-32)
  @char32
  (unbox-base-char imm0 val cr0)   ; this type checks val
  (b @set-32)
  @u32
  ;; Also has to be a fixnum (and an (UNSIGNED-BYTE 32)).
  (unbox-fixnum imm0 val)
  (clrrdi. imm1 imm0 32)                ; ~Z if any high bits set
  (bne cr7 @bad)
  (bne cr0 @bad)
  (b @set-32)
  @u16
  (unbox-fixnum imm0 val)
  (clrrdi. imm1 imm0 16)
  (bne cr7 @bad)
  (bne cr0 @bad)
  (b @set-16)
  @s16
  (sldi imm0 val (- 64 (+ 16 ppc64::fixnumshift)))
  (sradi imm0 imm0 (- 64 (+ 16 ppc64::fixnumshift)))
  (cmpw cr0 imm0 val)
  (unbox-fixnum imm0 val)
  (bne- cr7 @bad)
  (beq+ cr0 @set-16)
  (b @bad)
  @8
  (cmpdi cr0 imm2 ppc64::subtag-s8-vector)
  (beq cr0 @s8)
  (extract-unsigned-byte-bits. imm0 val 8)
  (unbox-fixnum imm0 val)
  (beq+ cr0 @set-8)
  (b @bad)
  @s8
  (sldi imm0 val (- 64 (+ 8 ppc64::fixnumshift)))
  (sradi imm0 imm0 (- 64 (+ 8 ppc64::fixnumshift)))
  (cmpd cr0 imm0 val)
  (unbox-fixnum imm0 val)
  (bne- cr7 @bad)
  (beq+ cr0 @set-8)
  (b @bad)
  @char8
  (unbox-base-char imm0 val cr0)   ; this type checks val
  @set-8                                ; propagate low 8 bits into low 16
  (la imm3 1 imm3)
  (rlwimi imm0 imm0 8 (- 32 16) (- 31 8))
  (srdi imm3 imm3 1)
  @set-16                               ; propagate low 16 bits into high 16
  (la imm3 1 imm3)
  (rlwimi imm0 imm0 16 0 (- 31 16))
  (srdi imm3 imm3 1) 
  @set-32                               ; propagate low 32 bits into high 32
  (la imm3 1 imm3)
  (rldimi imm0 imm0 32 0)
  (srdi imm3 imm3 1)
  @set-64
  (cmpdi cr0 imm3 1)
  (subi imm3 imm3 1)
  (stdx imm0 miscobj imm4)
  (la imm4 8 imm4)
  (bne cr0 @set-64)
  (blr))

;;; Make a new vector of size newsize whose subtag matches that of oldv-arg.
;;; Blast the contents of the old vector into the new one as quickly as
;;; possible; leave remaining elements of new vector undefined (0).
;;; Return new-vector.
#+ppc32-target
(defppclapfunction %extend-vector ((start-arg arg_x) (oldv-arg arg_y) (newsize arg_z))
  (let ((oldv save0)
        (oldsize save1)
        (oldsubtag save2)
        (start-offset save3))
    (save-lisp-context)
    (:regsave save3 0)
    (vpush save0)
    (vpush save1)
    (vpush save2)
    (vpush save3)
    (mr oldv oldv-arg)
    (mr start-offset start-arg)
    (getvheader imm0 oldv)
    (header-length oldsize imm0)
    (header-subtag[fixnum] oldsubtag imm0)
    (mr arg_y newsize)
    (mr arg_z oldsubtag)
    (bla .SPmisc-alloc)
    (extrwi imm0 oldsubtag ppc32::ntagbits (- 32 (+  ppc32::fixnumshift ppc32::ntagbits)))
    (cmpwi cr0 oldsize 0)
    (cmpwi cr1 imm0 ppc32::fulltag-nodeheader)
    (cmpwi cr2 oldsubtag '#.ppc32::max-32-bit-ivector-subtag)
    (la imm1 ppc32::misc-data-offset start-offset)
    (li imm3 ppc32::misc-data-offset)
    (beq cr0 @done)
    (bne cr1 @imm)
    ;; copy nodes.  New vector is "new", so no memoization required.
    @node-loop
    (cmpwi cr0 oldsize '1)
    (lwzx temp0 oldv imm1)
    (addi imm1 imm1 4)
    (subi oldsize oldsize '1)
    (stwx temp0 arg_z imm3)
    (addi imm3 imm3 4)
    (bne cr0 @node-loop)
    ;;Restore registers.  New vector's been in arg_z all this time.
    @done
    (lwz save3 0 vsp)
    (lwz save2 4 vsp)
    (lwz save1 8 vsp)
    (lwz save0 12 vsp)
    (restore-full-lisp-context)
    (blr)
    @imm
    (unbox-fixnum imm2 oldsize)
    (unbox-fixnum imm3 start-offset)
    (li imm1 ppc32::misc-data-offset)
    (la imm4 ppc32::misc-data-offset start-offset)
    (cmpwi cr1 oldsubtag '#.ppc32::max-8-bit-ivector-subtag)
    (cmpwi cr0 oldsubtag '#.ppc32::max-16-bit-ivector-subtag)
    (ble cr2 @fullword-loop)
    (cmpwi cr2 oldsubtag '#.ppc32::subtag-bit-vector)
    (ble cr1 @8-bit)
    (ble cr0 @16-bit)
    (beq cr2 @1-bit)
    ;; 64-bit (double-float) vectors.  There's a different
    ;; initial offset, but we're always word-aligned, so that
    ;; part's easy.
    (li imm1 ppc32::misc-dfloat-offset)   ; scaled destination pointer
    (slwi imm2 imm2 1)                  ; twice as many fullwords
    (slwi imm3 imm3 3)                  ; convert dword count to byte offset
    (la imm4 ppc32::misc-dfloat-offset imm3)      ; scaled source pointer
    (b @fullword-loop)
    ;; The bitvector case is hard if START-OFFSET isn't on an 8-bit boundary,
    ;;  and can be turned into the 8-bit case otherwise.
    ;; The 8-bit case is hard if START-OFFSET isn't on a 16-bit boundary, 
    ;;  and can be turned into the 16-bit case otherwise.
    ;; The 16-bit case is hard if START-OFFSET isn't on a 32-bit boundary, 
    ;;  and can be turned into the 32-bit case otherwise.
    ;; Hmm.
    @1-bit
    (clrlwi. imm0 imm3 (- 32 3))
    (bne- cr0 @hard-1-bit)
    (srwi imm3 imm3 3)                  ; bit offset to byte offset
    (addi imm2 imm2 7)
    (srwi imm2 imm2 3)                  ; bit count to byte count
    @8-bit
    ; If the byte offset's even, copy half as many halfwords
    (clrlwi. imm0 imm3 (- 32 1))
    (bne- cr0 @hard-8-bit)
    (addi imm2 imm2 1)
    (srwi imm2 imm2 1)                  ; byte count to halfword count
    (srwi imm3 imm3 1)                  ; byte offset to halfword offset
    @16-bit
    ; If the halfword offset's even, copy half as many fullwords
    (clrlwi. imm0 imm3 (- 32 1))
    (bne- cr0 @hard-16-bit)
    (addi imm2 imm2 1)
    (srwi imm2 imm2 1)                  ; halfword count to fullword count
    (li imm1 ppc32::misc-data-offset)   
    @fullword-loop
    (cmpwi cr0 imm2 1)
    (lwzx imm0 oldv imm4)
    (addi imm4 imm4 4)
    (subi imm2 imm2 1)
    (stwx imm0 arg_z imm1)
    (addi imm1 imm1 4)
    (bne cr0 @fullword-loop)
    (b @done)
    ;;; This can just do a uvref/uvset loop.  Cases that can
    ;;; cons (x32, double-float) have already been dealt with.
    @hard-1-bit
    @hard-8-bit
    @hard-16-bit
    (let ((newv save4)
          (outi save5)
          (oldlen save6))
      (vpush save4)
      (vpush save5)
      (vpush save6)
      (mr newv arg_z)
      (sub oldlen oldsize start-offset)
      (li outi 0)
      @hard-loop
      (mr arg_y oldv)
      (mr arg_z start-offset)
      (bla .SPmisc-ref)
      (mr arg_x newv)
      (mr arg_y outi)
      (bla .SPmisc-set)
      (la outi '1 outi)
      (cmpw cr0 outi oldlen)
      (la start-offset '1 start-offset)
      (bne @hard-loop)
      (mr arg_z newv)
      (vpop save6)
      (vpop save5)
      (vpop save4)
      (b @done))))

#+ppc64-target
(defppclapfunction %extend-vector ((start-arg arg_x) (oldv-arg arg_y) (newsize arg_z))
  (let ((oldv save0)
        (oldsize save1)
        (oldsubtag save2)
        (start-offset save3))
    (save-lisp-context)
    (:regsave save3 0)
    (vpush save0)
    (vpush save1)
    (vpush save2)
    (vpush save3)
    (mr oldv oldv-arg)
    (mr start-offset start-arg)
    (getvheader imm0 oldv)
    (header-length oldsize imm0)
    (header-subtag[fixnum] oldsubtag imm0)
    (mr arg_y newsize)
    (mr arg_z oldsubtag)
    (bla .SPmisc-alloc)
    (unbox-fixnum imm0 oldsubtag)
    (extract-lowtag imm1 imm0)
    (extract-fulltag imm2 imm0)
    (cmpdi cr0 oldsize 0)
    (cmpdi cr1 imm1 ppc64::lowtag-nodeheader)
    (cmpdi cr2 imm2 ppc64::ivector-class-8-bit)
    (cmpdi cr3 imm2 ppc64::ivector-class-32-bit)
    (cmpdi cr4 imm2 ppc64::ivector-class-64-bit)
    (cmpdi cr5 imm0 ppc64::subtag-bit-vector)
    (li imm3 ppc64::misc-data-offset)
    (beq cr0 @done)
    (bne cr1 @imm)
    (la imm1 ppc64::misc-data-offset start-offset)
    ;; copy nodes.  New vector is "new", so no memoization required.
    @node-loop
    (cmpdi cr0 oldsize '1)
    (ldx temp0 oldv imm1)
    (addi imm1 imm1 8)
    (subi oldsize oldsize '1)
    (stdx temp0 arg_z imm3)
    (addi imm3 imm3 8)
    (bne cr0 @node-loop)
    ;;Restore registers.  New vector's been in arg_z all this time.
    @done
    (ld save3 0 vsp)
    (ld save2 8 vsp)
    (ld save1 16 vsp)
    (ld save0 24 vsp)
    (restore-full-lisp-context)
    (blr)
    @imm
    (beq cr2 @8-bit)
    (beq cr3 @32-bit)
    (beq cr4 @64-bit)
    (beq cr5 @1-bit)
    (srdi imm1 start-offset 2)
    (la imm1 ppc64::misc-data-offset imm1)
    @16-loop
    (cmpdi cr0 oldsize '1)
    (lhzx imm4 oldv imm1)
    (addi imm1 imm1 2)
    (subi oldsize oldsize '1)
    (sthx imm4 arg_z imm3)
    (addi imm3 imm3 2)
    (bne cr0 @16-loop)
    (b @done)
    @8-bit
    (srdi imm1 start-offset 3)
    (la imm1 ppc64::misc-data-offset imm1)
    @8-loop
    (cmpdi cr0 oldsize '1)
    (lbzx imm4 oldv imm1)
    (addi imm1 imm1 1)
    (subi oldsize oldsize '1)
    (stbx imm4 arg_z imm3)
    (addi imm3 imm3 1)
    (bne cr0 @8-loop)
    (b @done)
    @32-bit
    (srdi imm1 start-offset 1)
    (la imm1 ppc64::misc-data-offset imm1)
    @32-loop
    (cmpdi cr0 oldsize '1)
    (lwzx imm4 oldv imm1)
    (addi imm1 imm1 4)
    (subi oldsize oldsize '1)
    (stwx imm4 arg_z imm3)
    (addi imm3 imm3 4)
    (bne cr0 @32-loop)
    (b @done)
    @64-bit
    (la imm1 ppc64::misc-data-offset start-offset)
    @64-loop
    (cmpdi cr0 oldsize '1)
    (ldx imm4 oldv imm1)
    (addi imm1 imm1 8)
    (subi oldsize oldsize '1)
    (stdx imm4 arg_z imm3)
    (addi imm3 imm3 8)
    (bne cr0 @64-loop)
    (b @done)
    @1-bit
    (let ((newv save4)
          (outi save5)
          (oldlen save6))
      (vpush save4)
      (vpush save5)
      (vpush save6)
      (mr newv arg_z)
      (sub oldlen oldsize start-offset)
      (li outi 0)
      @hard-loop
      (mr arg_y oldv)
      (mr arg_z start-offset)
      (bla .SPmisc-ref)
      (mr arg_x newv)
      (mr arg_y outi)
      (bla .SPmisc-set)
      (la outi '1 outi)
      (cmpd cr0 outi oldlen)
      (la start-offset '1 start-offset)
      (bne @hard-loop)
      (mr arg_z newv)
      (vpop save6)
      (vpop save5)
      (vpop save4)
      (b @done))))


;;; argument is a vector header or an array header.  Or else.
(defppclapfunction %array-header-data-and-offset ((a arg_z))
  (let ((offset arg_y)
        (disp arg_x)
        (temp temp0))
    (li offset 0)
    (mr temp a)
    @loop
    (ldr a target::arrayH.data-vector temp)
    (lbz imm0 target::misc-subtag-offset a)
    (cmpri cr0 imm0 target::subtag-vectorH)
    (ldr disp target::arrayH.displacement temp)
    (mr temp a)
    (add offset offset disp)
    (ble cr0 @loop)
    (vpush a)
    (vpush offset)
    (set-nargs 2)
    (la temp0 (* 2 (ash 1 target::word-shift)) vsp)
    (ba .SPvalues)))


;;; If the bit-arrays are all simple-bit-vectorp, we can do the operations
;;; 32 bits at a time.  (other case have to worry about alignment/displacement.)
#+ppc32-target
(defppclapfunction %simple-bit-boole ((op 0) (b1 arg_x) (b2 arg_y) (result arg_z))
  (la imm0 4 vsp)
  (save-lisp-context imm0)
  (vector-size imm4 result imm4)
  (srwi. imm3 imm4 5)
  (clrlwi imm4 imm4 27)
  (bl @get-dispatch)
  (cmpwi cr1 imm4 0)
  (mflr loc-pc)
  (lwz temp0 op vsp)
  (add loc-pc loc-pc temp0)
  (add loc-pc loc-pc temp0)
  (mtctr loc-pc)
  (li imm0 ppc32::misc-data-offset)
  (b @testw)
  @nextw
  (cmpwi cr0 imm3 1)
  (subi imm3 imm3 1)
  (lwzx imm1 b1 imm0)
  (lwzx imm2 b2 imm0)
  (bctrl)
  (stwx imm1 result imm0)
  (addi imm0 imm0 4)
  @testw
  (bne cr0 @nextw)
  (beq cr1 @done)
  ;; Not sure if we need to make this much fuss about the partial word
  ;; in this simple case, but what the hell.
  (lwzx imm1 b1 imm0)
  (lwzx imm2 b2 imm0)
  (bctrl)
  (lwzx imm2 result imm0)
  (slw imm2 imm2 imm4)
  (srw imm2 imm2 imm4)
  (subfic imm4 imm4 32)
  (srw imm1 imm1 imm4)
  (slw imm1 imm1 imm4)
  (or imm1 imm1 imm2)
  (stwx imm1 result imm0)
  @done
  (restore-full-lisp-context)
  (blr)

  @get-dispatch 
  (blrl)
  @disptach
  (li imm1 0)                           ; boole-clr
  (blr)
  (li imm1 -1)                          ; boole-set
  (blr)
  (blr)                                 ; boole-1
  (blr)                             
  (mr imm1 imm2)                        ; boole-2
  (blr)
  (not imm1 imm1)                       ; boole-c1
  (blr)
  (not imm1 imm2)                       ; boole-c2
  (blr)
  (and imm1 imm1 imm2)                  ; boole-and
  (blr)
  (or imm1 imm1 imm2)                   ; boole-ior
  (blr)
  (xor imm1 imm1 imm2)                  ; boole-xor
  (blr)
  (eqv imm1 imm1 imm2)                  ; boole-eqv
  (blr)
  (nand imm1 imm1 imm2)                 ; boole-nand
  (blr)
  (nor imm1 imm1 imm2)                  ; boole-nor
  (blr)
  (andc imm1 imm2 imm1)                 ; boole-andc1
  (blr)
  (andc imm1 imm1 imm2)                 ; boole-andc2
  (blr)
  (orc imm1 imm2 imm1)                  ; boole-orc1
  (blr)
  (orc imm1 imm1 imm2)                  ; boole-orc2
  (blr))

#+ppc64-target
(defppclapfunction %simple-bit-boole ((op 0) (b1 arg_x) (b2 arg_y) (result arg_z))
  (la imm0 8 vsp)
  (save-lisp-context imm0)
  (vector-size imm4 result imm4)
  (srdi. imm3 imm4 6)
  (clrldi imm4 imm4 (- 64 6))
  (bl @get-dispatch)
  (cmpdi cr1 imm4 0)                    ; at most low 6 bits set in imm4
  (mflr loc-pc)
  (ld temp0 op vsp)
  (add loc-pc loc-pc temp0)
  (mtctr loc-pc)
  (li imm0 ppc64::misc-data-offset)
  (b @testd)
  @nextd
  (cmpdi cr0 imm3 1)
  (subi imm3 imm3 1)
  (ldx imm1 b1 imm0)
  (ldx imm2 b2 imm0)
  (bctrl)
  (stdx imm1 result imm0)
  (addi imm0 imm0 8)
  @testd
  (bne cr0 @nextd)
  (beq cr1 @done)
  ;; Not sure if we need to make this much fuss about the partial word
  ;; in this simple case, but what the hell.
  (ldx imm1 b1 imm0)
  (ldx imm2 b2 imm0)
  (bctrl)
  (ldx imm2 result imm0)
  (sld imm2 imm2 imm4)
  (srd imm2 imm2 imm4)
  (subfic imm4 imm4 64)
  (srd imm1 imm1 imm4)
  (sld imm1 imm1 imm4)
  (or imm1 imm1 imm2)
  (stdx imm1 result imm0)
  @done
  (restore-full-lisp-context)
  (blr)

  @get-dispatch 
  (blrl)
  @disptach
  (li imm1 0)                           ; boole-clr
  (blr)
  (li imm1 -1)                          ; boole-set
  (blr)
  (blr)                                 ; boole-1
  (blr)                             
  (mr imm1 imm2)                        ; boole-2
  (blr)
  (not imm1 imm1)                       ; boole-c1
  (blr)
  (not imm1 imm2)                       ; boole-c2
  (blr)
  (and imm1 imm1 imm2)                  ; boole-and
  (blr)
  (or imm1 imm1 imm2)                   ; boole-ior
  (blr)
  (xor imm1 imm1 imm2)                  ; boole-xor
  (blr)
  (eqv imm1 imm1 imm2)                  ; boole-eqv
  (blr)
  (nand imm1 imm1 imm2)                 ; boole-nand
  (blr)
  (nor imm1 imm1 imm2)                  ; boole-nor
  (blr)
  (andc imm1 imm2 imm1)                 ; boole-andc1
  (blr)
  (andc imm1 imm1 imm2)                 ; boole-andc2
  (blr)
  (orc imm1 imm2 imm1)                  ; boole-orc1
  (blr)
  (orc imm1 imm1 imm2)                  ; boole-orc2
  (blr))


(defppclapfunction %aref2 ((array arg_x) (i arg_y) (j arg_z))
  (check-nargs 3)
  (ba .SParef2))

(defppclapfunction %aref3 ((array 0) (i arg_x) (j arg_y) (k arg_z))
  (check-nargs 4)
  (vpop temp0)
  (ba .SParef3))


(defppclapfunction %aset2 ((array 0) (i arg_x) (j arg_y) (newval arg_z))
  (check-nargs 4)
  (vpop temp0)
  (ba .SPaset2))

(defppclapfunction %aset3 ((array #.target::node-size) (i 0) (j arg_x) (k arg_y)  (newval arg_z))
  (check-nargs 5)
  (vpop temp0)
  (vpop temp1)
  (ba .SPaset3))
  

