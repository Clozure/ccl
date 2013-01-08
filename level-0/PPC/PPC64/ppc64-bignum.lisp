;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
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

;;; The caller has allocated a two-digit bignum (quite likely on the stack).
;;; If we can fit in a single digit (if the high word is just a sign
;;; extension of the low word), truncate the bignum in place (the
;;; trailing words should already be zeroed.
(defppclapfunction %fixnum-to-bignum-set ((bignum arg_y) (fixnum arg_z))
  (unbox-fixnum imm0 fixnum)
  (srdi imm1 imm0 32)
  (srawi imm2 imm0 31)
  (cmpw imm2 imm1)
  (stw imm0 ppc64::misc-data-offset bignum)
  (li imm2 ppc64::one-digit-bignum-header)
  (beq @chop)
  (stw imm1 (+ ppc64::misc-data-offset 4) bignum)
  (blr)
  @chop
  (std imm2 ppc64::misc-header-offset bignum)
  (blr))
  

(defppclapfunction %multiply-and-add-loop64
    ((x 8) (y 0) (r arg_x) (idx arg_y) (ylen arg_z))
  (let ((i imm0)
	(j imm1)
	(xx imm2)
	(yy imm3)
	(rr imm4)
	(dd imm5)
	(cc nargs))
    (ld temp0 x vsp)
    (la i ppc64::misc-data-offset idx)
    (ldx xx temp0 i)			;x[i]
    (rotldi xx xx 32)
    (ld temp0 y vsp)
    (li cc 0)
    (li j ppc64::misc-data-offset)
    @loop
    (ldx yy temp0 j)			;y[j]
    (rotldi yy yy 32)
    (mulld dd xx yy)  ;low
    (ldx rr r i)			;r[i]
    (rotldi rr rr 32)
    (addc rr rr dd)			;r[i] = r[i] + low
    (mulhdu dd xx yy)			;high
    (addze dd dd)			;carry from addding in low
    (addc rr rr cc)			;add in carry digit
    (addze cc dd)
    (rotldi rr rr 32)
    (stdx rr r i)			;update r[i]
    (cmpdi ylen '1)
    (la i 8 i)
    (la j 8 j)
    (subi ylen ylen '1)
    (bne @loop)
    (rotldi cc cc 32)
    (stdx cc r i)
    (set-nargs 0)
    (la vsp 16 vsp)
    (blr)))

;;; Multiply the (32-bit) digits X and Y, producing a 64-bit result.
;;; Add the 32-bit "prev" digit and the 32-bit carry-in digit to that 64-bit
;;; result; return the halves as (VALUES high low).
(defppclapfunction %multiply-and-add4 ((x 0) (y arg_x) (prev arg_y) (carry-in arg_z))
  (let ((unboxed-x imm0)
        (unboxed-y imm1)
        (unboxed-prev imm2)
        (unboxed-carry-in imm3)
        (result64 imm4)
        (high arg_y)
        (low arg_z))
    (ld temp0 x vsp)
    (unbox-fixnum unboxed-x temp0)
    (unbox-fixnum unboxed-y y)
    (unbox-fixnum unboxed-prev prev)
    (unbox-fixnum unboxed-carry-in carry-in)
    (mulld result64 unboxed-x unboxed-y)
    (add result64 result64 unboxed-prev)
    (add result64 result64 unboxed-carry-in)
    (clrlsldi low result64 32 ppc64::fixnumshift)
    (clrrdi high result64 32)
    (srdi high high (- 32 ppc64::fixnumshift))
    (std high 0 vsp)
    (set-nargs 2)
    (vpush low)
    (la temp0 '2 vsp)
    (ba .SPvalues)))

(defppclapfunction %multiply-and-add3 ((x arg_x) (y arg_y) (carry-in arg_z))
  (let ((unboxed-x imm0)
        (unboxed-y imm1)
        (unboxed-carry-in imm2)
        (result64 imm3)
        (high arg_y)
        (low arg_z))
    (unbox-fixnum unboxed-x arg_x)
    (unbox-fixnum unboxed-y y)
    (unbox-fixnum unboxed-carry-in carry-in)
    (mulld result64 unboxed-x unboxed-y)
    (add result64 result64 unboxed-carry-in)
    (clrlsldi low result64 32 ppc64::fixnumshift)
    (clrrdi high result64 32)
    (srdi high high (- 32 ppc64::fixnumshift))
    (vpush high)
    (set-nargs 2)
    (vpush low)
    (la temp0 '2 vsp)
    (ba .SPvalues)))

(defppclapfunction %multiply-and-add-fixnum-loop ((len64 0) (x arg_x) (y arg_y) (result arg_z))
  (let ((carry imm4)
        (iidx imm3)
        (unboxed-y imm0)
        (i temp0)
        (hi imm2)
        (rlen temp1))
    (vpop rlen)
    (li carry 0)
    (li iidx ppc64::misc-data-offset)
    (li i 0)
    (b @test)
    @loop
    (unbox-fixnum unboxed-y y)
    (ldx imm1 x iidx)
    (rotldi imm1 imm1 32)
    (mulhdu hi imm1 unboxed-y)
    (mulld imm0 imm1 unboxed-y)
    (addc imm0 imm0 carry)
    (addze carry hi)
    (rotldi imm0 imm0 32)
    (stdx imm0 result iidx)
    (la iidx 8 iidx)
    (la i '1 i)
    @test
    (cmpd i rlen)
    (blt @loop)
    (rotldi carry carry 32)
    (stdx carry result iidx)
    (blr)))
    
    

;;; Return the (possibly truncated) 32-bit quotient and remainder
;;; resulting from dividing hi:low by divisor.
(defppclapfunction %floor ((num-high arg_x) (num-low arg_y) (divisor arg_z))
  (let ((unboxed-num imm0)
        (unboxed-low imm1)
        (unboxed-divisor imm2)
        (unboxed-quo imm3)
        (unboxed-rem imm4))
    (sldi unboxed-num num-high (- 32 ppc64::fixnumshift))
    (unbox-fixnum unboxed-low num-low)
    (unbox-fixnum unboxed-divisor divisor)
    (or unboxed-num unboxed-low unboxed-num)
    (divdu unboxed-quo unboxed-num unboxed-divisor)
    (mulld unboxed-rem unboxed-quo unboxed-divisor)
    (sub unboxed-rem unboxed-num unboxed-rem)
    (clrlsldi arg_y unboxed-quo 32 ppc64::fixnumshift)
    (clrlsldi arg_z unboxed-rem 32 ppc64::fixnumshift)
    (mr temp0 vsp)
    (vpush arg_y)
    (vpush arg_z)
    (set-nargs 2)
    (ba .SPvalues)))

;;; Multiply two (UNSIGNED-BYTE 32) arguments, return the high and
;;; low halves of the 64-bit result
(defppclapfunction %multiply ((x arg_y) (y arg_z))
  (let ((unboxed-x imm0)
        (unboxed-y imm1)
        (unboxed-high imm2)
        (unboxed-low imm3))
    (unbox-fixnum unboxed-x x)
    (unbox-fixnum unboxed-y y)
    (mulld imm2 unboxed-x unboxed-y)
    (clrlsldi arg_y imm2 32 ppc64::fixnumshift) ; arg_y = low32
    (srdi imm2 imm2 32)
    (box-fixnum arg_z imm2)             ; arg_z = high32
    (mr temp0 vsp)
    (vpush arg_z)
    (set-nargs 2)
    (vpush arg_y)
    (ba .SPvalues)))

;;; Any words in the "tail" of the bignum should have been
;;; zeroed by the caller.
(defppclapfunction %set-bignum-length ((newlen arg_y) (bignum arg_z))
  (sldi imm0 newlen (- ppc64::num-subtag-bits ppc64::fixnumshift))
  (ori imm0 imm0 ppc64::subtag-bignum)
  (std imm0 ppc64::misc-header-offset bignum)
  (blr))

;;; Count the sign bits in the most significant digit of bignum;
;;; return fixnum count.
(defppclapfunction %bignum-sign-bits ((bignum arg_z))
  (vector-size imm0 bignum imm0)
  (sldi imm0 imm0 2)
  (la imm0 (- ppc64::misc-data-offset 4) imm0) ; Reference last (most significant) digit
  (lwzx imm0 bignum imm0)
  (cmpwi imm0 0)
  (not imm0 imm0)
  (blt @wasneg)
  (not imm0 imm0)
  @wasneg
  (cntlzw imm0 imm0)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %signed-bignum-ref ((bignum arg_y) (index arg_z))
  (srdi imm0 index 1)
  (la imm0 ppc64::misc-data-offset imm0)
  (lwax imm0 bignum imm0)
  (box-fixnum arg_z imm0)
  (blr))


;;; If the bignum is a one-digit bignum, return the value of the
;;; single digit as a fixnum.  Otherwise, if it's a two-digit-bignum
;;; and the two words of the bignum can be represented in a fixnum,
;;; return that fixnum; else return nil.
(defppclapfunction %maybe-fixnum-from-one-or-two-digit-bignum ((bignum arg_z))
  (ld imm1 ppc64::misc-header-offset bignum)
  (cmpdi cr1 imm1 ppc64::one-digit-bignum-header)
  (cmpdi cr2 imm1 ppc64::two-digit-bignum-header)
  (beq cr1 @one)
  (bne cr2 @no)
  (ld imm0 ppc64::misc-data-offset bignum)
  (rotldi imm0 imm0 32)
  (box-fixnum arg_z imm0)
  (unbox-fixnum imm1 arg_z)
  (cmpd imm0 imm1)
  (beqlr)
  @no
  (li arg_z nil)
  (blr)
  @one
  (lwa imm0 ppc64::misc-data-offset bignum)
  (box-fixnum arg_z imm0)
  (blr))


(defppclapfunction %digit-logical-shift-right ((digit arg_y) (count arg_z))
  (unbox-fixnum imm0 digit)
  (unbox-fixnum imm1 count)
  (srw imm0 imm0 imm1)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %ashr ((digit arg_y) (count arg_z))
  (unbox-fixnum imm0 digit)
  (unbox-fixnum imm1 count)
  (sraw imm0 imm0 imm1)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %ashl ((digit arg_y) (count arg_z))
  (unbox-fixnum imm0 digit)
  (unbox-fixnum imm1 count)
  (slw imm0 imm0 imm1)
  (clrlsldi arg_z imm0 32 ppc64::fixnumshift)
  (blr))

(defppclapfunction macptr->fixnum ((ptr arg_z))
  (macptr-ptr imm0 ptr)
  (andi. imm1 imm0 7)
  (li arg_z nil)
  (bne @done)
  (mr arg_z imm0)
  @done
  (blr))

(defppclapfunction fix-digit-logand ((fix arg_x) (big arg_y) (dest arg_z)) ; index 0
  (let ((w1 imm0)
        (w2 imm1))
    (ld w2 ppc64::misc-data-offset big)
    (unbox-fixnum  w1 fix)
    (rotldi w2 w2 32)
    (cmpdi dest nil)
    (and w1 w1 w2)
    (bne @store)
    (box-fixnum arg_z w1)
    (blr)
    @store
    (rotldi w1 w1 32)
    (std w1 ppc64::misc-data-offset dest)
    (blr)))



(defppclapfunction fix-digit-logandc2 ((fix arg_x) (big arg_y) (dest arg_z))
  (cmpdi dest nil)
  (ld imm1 ppc64::misc-data-offset big)
  (unbox-fixnum imm0 fix)
  (rotldi imm1 imm1 32)
  (andc imm1 imm0 imm1)
  (bne @store)
  (box-fixnum arg_z imm1)
  (blr)
  @store
  (rotldi imm1 imm1 32)
  (std imm1 ppc64::misc-data-offset dest)
  (blr))

(defppclapfunction fix-digit-logandc1 ((fix arg_x) (big arg_y) (dest arg_z))
  (cmpdi dest nil)
  (ld imm1 ppc64::misc-data-offset big)
  (unbox-fixnum imm0 fix)
  (rotldi imm1 imm1 32)
  (andc imm1 imm1 imm0)
  (bne @store)
  (box-fixnum arg_z imm1)
  (blr)
  @store
  (rotldi imm1 imm1 32)
  (std imm1 ppc64::misc-data-offset dest)
  (blr))


;;; Do LOGIOR on the N 32-bit words in A and B, storing the result in
;;; C.  (It's legal and desirable to do this more than 32 bits at a time.)

(defppclapfunction %bignum-logior ((n 0) (a arg_x) (b arg_y) (c arg_z))
  (vpop imm0)
  (srdi imm0 imm0 1)
  (andi. imm1 imm0 4)
  (la imm3 ppc64::misc-data-offset imm0)
  (beq @loop)
  (cmpdi imm0 4)
  (subi imm0 imm0 4)
  (subi imm3 imm3 4)
  (lwzx imm1 a imm3)
  (lwzx imm2 b imm3)
  (or imm1 imm1 imm2)
  (stwx imm1 c imm3)
  (beqlr)
  @loop
  (subi imm0 imm0 8)
  (subi imm3 imm3 8)
  (cmpdi imm0 0)                        ;can't happen on 1st iteration
  (ldx imm1 a imm3)
  (ldx imm2 b imm3)
  (or imm1 imm1 imm2)
  (stdx imm1 c imm3)
  (bne @loop)
  (blr))



;;; Do LOGAND on the N 32-bit words in A and B, storing the result in
;;; C.  (It's legal and desirable to do this more than 32 bits at a time.)

(defppclapfunction %bignum-logand ((n 0) (a arg_x) (b arg_y) (c arg_z))
  (vpop imm0)
  (srdi imm0 imm0 1)
  (andi. imm1 imm0 4)
  (la imm3 ppc64::misc-data-offset imm0)
  (beq @loop)
  (cmpdi imm0 4)
  (subi imm0 imm0 4)
  (subi imm3 imm3 4)
  (lwzx imm1 a imm3)
  (lwzx imm2 b imm3)
  (and imm1 imm1 imm2)
  (stwx imm1 c imm3)
  (beqlr)
  @loop
  (subi imm0 imm0 8)
  (subi imm3 imm3 8)
  (cmpdi imm0 0)                        ;can't happen on 1st iteration
  (ldx imm1 a imm3)
  (ldx imm2 b imm3)
  (and imm1 imm1 imm2)
  (stdx imm1 c imm3)
  (bne @loop)
  (blr))