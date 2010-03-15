;-*- Mode: Lisp; Package: CCL -*-
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

(defppclapfunction %fixnum-signum ((number arg_z))
  (cmpri :cr0 number '0)
  (li arg_z '0)
  (beqlr :cr0)
  (li arg_z '1)               ; assume positive
  (bgtlr :cr0)
  (li arg_z '-1)
  (blr))

; see %logcount (ppc-bignum.lisp)
(defppclapfunction %ilogcount ((number arg_z))
  (let ((arg imm0)
        (shift imm1)
        (temp imm2))
    (unbox-fixnum arg number)
    (mr. shift arg)
    (li arg_z 0)
    (b @test)
    @next
    (la temp -1 shift)
    (and. shift shift temp)
    (la arg_z '1 arg_z)
    @test
    (bne @next)
    (blr)))

(defppclapfunction %iash ((number arg_y) (count arg_z))
  (unbox-fixnum imm1 count)
  (unbox-fixnum imm0 number)
  (neg. imm2 imm1)
  (blt @left)
  (srar imm0 imm0 imm2)
  (box-fixnum arg_z imm0)
  (blr)
  @left
  (slr arg_z number imm1)
  (blr))

(defparameter *double-float-zero* 0.0d0)
(defparameter *short-float-zero* 0.0s0)


#+ppc32-target
(defppclapfunction %sfloat-hwords ((sfloat arg_z))
  (lwz imm0 ppc32::single-float.value sfloat)
  (digit-h temp0 imm0)
  (digit-l temp1 imm0)
  (vpush temp0)
  (vpush temp1)
  (la temp0 8 vsp)
  (set-nargs 2)
  (ba .SPvalues))


; (integer-length arg) = (- 32 (cntlz (if (>= arg 0) arg (lognot arg))))
#+ppc32-target
(defppclapfunction %fixnum-intlen ((number arg_z))  
  (unbox-fixnum imm0 arg_z)
  (cntlzw. imm1 imm0)			; testing result of cntlzw? - ah no zeros if neg
  (bne @nonneg)
  (not imm1 imm0)
  (cntlzw imm1 imm1)
  @nonneg
  (subfic imm1 imm1 32)
  (box-fixnum arg_z imm1)
  (blr))

#+ppc64-target
(defppclapfunction %fixnum-intlen ((number arg_z))  
  (unbox-fixnum imm0 arg_z)
  (cntlzd. imm1 imm0)
  (bne @nonneg)
  (not imm1 imm0)
  (cntlzd imm1 imm1)
  @nonneg
  (subfic imm1 imm1 64)
  (box-fixnum arg_z imm1)
  (blr))




;;; Caller guarantees that result fits in a fixnum.
#+ppc32-target
(defppclapfunction %truncate-double-float->fixnum ((arg arg_z))
  (get-double-float fp0 arg)
  (fctiwz fp0 fp0)
  (stwu tsp -16 tsp)
  (stw tsp 4 tsp)
  (stfd fp0 8 tsp)
  (lwz imm0 (+ 8 4) tsp)
  (lwz tsp 0 tsp)
  (box-fixnum arg_z imm0)  
  (blr))

#+ppc64-target
(defppclapfunction %truncate-double-float->fixnum ((arg arg_z))
  (get-double-float fp0 arg)
  (fctidz fp0 fp0)
  (stdu tsp -32 tsp)
  (std tsp 8 tsp)
  (stfd fp0 16 tsp)
  (ld imm0 16 tsp)
  (la tsp 32 tsp)
  (box-fixnum arg_z imm0)  
  (blr))

#+ppc32-target
(defppclapfunction %truncate-short-float->fixnum ((arg arg_z))
  (get-single-float fp0 arg)
  (fctiwz fp0 fp0)
  (stwu tsp -16 tsp)
  (stw tsp 4 tsp)
  (stfd fp0 8 tsp)
  (lwz imm0 (+ 8 4) tsp)
  (lwz tsp 0 tsp)
  (box-fixnum arg_z imm0)  
  (blr))

#+ppc64-target
(defppclapfunction %truncate-short-float->fixnum ((arg arg_z))
  (get-single-float fp0 arg)
  (fctidz fp0 fp0)
  (stdu tsp -32 tsp)
  (std tsp 8 tsp)
  (stfd fp0 16 tsp)
  (ld imm0 16 tsp)
  (la tsp 32 tsp)
  (box-fixnum arg_z imm0)  
  (blr))

;;; DOES round to even
#+ppc32-target
(defppclapfunction %round-nearest-double-float->fixnum ((arg arg_z))
  (get-double-float fp0 arg)
  (fctiw fp0 fp0)
  (stwu tsp -16 tsp)
  (stw tsp 4 tsp)
  (stfd fp0 8 tsp)
  (lwz imm0 (+ 8 4) tsp)
  (lwz tsp 0 tsp)
  (box-fixnum arg_z imm0)  
  (blr))

#+ppc64-target
(defppclapfunction %round-nearest-double-float->fixnum ((arg arg_z))
  (get-double-float fp0 arg)
  (fctid fp0 fp0)
  (stdu tsp -32 tsp)
  (std tsp 8 tsp)
  (stfd fp0 16 tsp)
  (ld imm0 16 tsp)
  (la tsp 32 tsp)
  (box-fixnum arg_z imm0)  
  (blr))

#+ppc32-target
(defppclapfunction %round-nearest-short-float->fixnum ((arg arg_z))
  (get-single-float fp0 arg)
  (fctiw fp0 fp0)
  (stwu tsp -16 tsp)
  (stw tsp 4 tsp)
  (stfd fp0 8 tsp)
  (lwz imm0 (+ 8 4) tsp)
  (lwz tsp 0 tsp)
  (box-fixnum arg_z imm0)  
  (blr))

#+ppc64-target
(defppclapfunction %round-nearest-short-float->fixnum ((arg arg_z))
  (get-single-float fp0 arg)
  (fctid fp0 fp0)
  (stdu tsp -32 tsp)
  (std tsp 8 tsp)
  (stfd fp0 16 tsp)
  (ld imm0 16 tsp)
  (la tsp 32 tsp)
  (box-fixnum arg_z imm0)  
  (blr))




;;; maybe this could be smarter but frankly scarlett I dont give a damn
;;; ticket:666 describes one reason to give a damn.
#+ppc32-target
(defppclapfunction %fixnum-truncate ((dividend arg_y) (divisor arg_z))
  (let ((unboxed-quotient imm0)
        (unboxed-dividend imm1)
        (unboxed-divisor imm2)
        (unboxed-product imm3)
        (product temp0)
        (boxed-quotient temp1)
        (remainder temp2))
    (cmpwi divisor '-1)    
    (unbox-fixnum unboxed-dividend dividend)
    (unbox-fixnum unboxed-divisor divisor)
    (beq @neg)
    (divwo. unboxed-quotient unboxed-dividend unboxed-divisor)          ; set OV if divisor = 0
    (box-fixnum boxed-quotient unboxed-quotient)
    (mullw unboxed-product unboxed-quotient unboxed-divisor)
    (bns+ @ok)
    (mtxer rzero)
    (save-lisp-context)
    (set-nargs 3)
    (load-constant arg_x truncate)
    (call-symbol divide-by-zero-error)
    @not-0
    @ok
    (subf imm0 unboxed-product unboxed-dividend)
    (vpush boxed-quotient)
    (box-fixnum remainder imm0)
    (vpush remainder)
    (set-nargs 2)
    (la temp0 8 vsp)
    (ba .SPvalues)
    @neg
    (nego. dividend dividend)
    (lwz arg_z '*least-positive-bignum* nfn)
    (bns @ret)
    (mtxer rzero)
    (lwz dividend ppc32::symbol.vcell arg_z)
    @ret
    (mr temp0 vsp)
    (vpush dividend)
    (vpush rzero)
    (set-nargs 2)
    (ba .SPvalues)))

#+ppc64-target
(defppclapfunction %fixnum-truncate ((dividend arg_y) (divisor arg_z))
  (let ((unboxed-quotient imm0)
        (unboxed-dividend imm1)
        (unboxed-divisor imm2)
        (unboxed-product imm3)
        (product temp0)
        (boxed-quotient temp1)
        (remainder temp2))
    (cmpdi divisor '-1)
    (unbox-fixnum unboxed-dividend dividend)
    (unbox-fixnum unboxed-divisor divisor)
    (beq @neg)
    (divdo. unboxed-quotient unboxed-dividend unboxed-divisor)          ; set OV if divisor = 0
    (box-fixnum boxed-quotient unboxed-quotient)
    (mulld unboxed-product unboxed-quotient unboxed-divisor)
    (bns+ @ok)
    (mtxer rzero)
    (save-lisp-context)
    (set-nargs 3)
    (load-constant arg_x truncate)
    (call-symbol divide-by-zero-error)
    @not-0
    @ok
    (subf imm0 unboxed-product unboxed-dividend)
    (vpush boxed-quotient)
    (box-fixnum remainder imm0)
    (vpush remainder)
    (set-nargs 2)
    (la temp0 '2 vsp)
    (ba .SPvalues)
    @neg
    (nego. dividend dividend)
    (ld arg_z '*least-positive-bignum* nfn)
    (bns @ret)
    (mtxer rzero)
    (ld dividend ppc64::symbol.vcell arg_z)
    @ret
    (mr temp0 vsp)
    (vpush dividend)
    (vpush rzero)
    (set-nargs 2)
    (ba .SPvalues)    
    ))


(defppclapfunction called-for-mv-p ()
  (ref-global imm0 ret1valaddr)
  (ldr imm1 target::lisp-frame.savelr sp)
  (eq->boolean arg_z imm0 imm1 imm0)
  (blr))

;;; n1 and n2 must be positive (esp non zero)
#+ppc32-target
(defppclapfunction %fixnum-gcd ((n1 arg_y)(n2 arg_z))
  (let ((temp imm0)
	(u imm1)
	(v imm2)
	(ut0 imm3)
	(vt0 imm4))
    (unbox-fixnum u n1)
    (unbox-fixnum v n2)
    (neg temp u)
    (and temp temp u)
    (cntlzw ut0 temp)
    (subfic ut0 ut0 31)
    (neg temp v)
    (and temp temp v)
    (cntlzw vt0 temp)
    (subfic vt0 vt0 31)
    (cmpw cr2 ut0 vt0)
    (srw u u ut0)
    (srw v v vt0)
    (addi ut0 ut0 ppc32::fixnum-shift)
    (addi vt0 vt0 ppc32::fixnum-shift)
    @loop
    (cmpw cr0 u v)
    (slw arg_z u ut0)
    (bgt cr0 @u>v)
    (blt cr0 @u<v)
    (blelr cr2)
    (slw arg_z u vt0)
    (blr)
    @u>v
    (sub u u v)
    @shiftu
    (andi. temp u (ash 1 1))
    (srwi u u 1)
    (beq cr0 @shiftu)
    (b @loop)
    @u<v
    (sub v v u)
    @shiftv
    (andi. temp v (ash 1 1))
    (srwi v v 1)
    (beq cr0 @shiftv)
    (b @loop)))

#+ppc64-target
(defppclapfunction %fixnum-gcd ((n1 arg_y)(n2 arg_z))
  (let ((temp imm0)
	(u imm1)
	(v imm2)
	(ut0 imm3)
	(vt0 imm4))
    (unbox-fixnum u n1)
    (unbox-fixnum v n2)
    (neg temp u)
    (and temp temp u)
    (cntlzd ut0 temp)
    (subfic ut0 ut0 63)
    (neg temp v)
    (and temp temp v)
    (cntlzd vt0 temp)
    (subfic vt0 vt0 63)
    (cmpw cr2 ut0 vt0)
    (srd u u ut0)
    (srd v v vt0)
    (addi ut0 ut0 ppc64::fixnum-shift)
    (addi vt0 vt0 ppc64::fixnum-shift)
    @loop
    (cmpd cr0 u v)
    (sld arg_z u ut0)
    (bgt cr0 @u>v)
    (blt cr0 @u<v)
    (blelr cr2)
    (sld arg_z u vt0)
    (blr)
    @u>v
    (sub u u v)
    @shiftu
    (andi. temp u (ash 1 1))
    (srdi u u 1)
    (beq cr0 @shiftu)
    (b @loop)
    @u<v
    (sub v v u)
    @shiftv
    (andi. temp v (ash 1 1))
    (srdi v v 1)
    (beq cr0 @shiftv)
    (b @loop)))

(defppclapfunction %mrg31k3p ((state arg_z))
  (let ((seed temp0))
    (svref seed 1 state)
    (u32-ref imm0 1 seed)
    (u32-ref imm3 2 seed)
    (rlwinm imm1 imm0 22 1 9)
    (srwi imm2 imm0 9)
    (add imm0 imm1 imm2)
    
    ;; construct m1 (1- (expt 2 31))
    (lis imm1 #x7fff)
    (ori imm1 imm1 #xffff)

    (rlwinm imm4 imm3 7 1 24)
    (srwi imm5 imm3 24)
    (add imm0 imm0 imm4)
    (add imm0 imm0 imm5)

    ;; reduce mod m1
    (cmplw cr7 imm0 imm1)
    (blt cr7 @ok1)
    (sub imm0 imm0 imm1)
    @ok1

    (add imm0 imm0 imm3)

    ;; reduce mod m1
    (cmplw cr7 imm0 imm1)
    (blt cr7 @ok2)
    (sub imm0 imm0 imm1)
    @ok2

    ;; update state
    (u32-ref imm1 1 seed)
    (u32-set imm1 2 seed)
    (u32-ref imm1 0 seed)
    (u32-set imm1 1 seed)
    (u32-set imm0 0 seed)

    ;; construct m2 (- (expt 2 31) 21069))
    (lis imm5 #x7fff)
    (ori imm5 imm5 44467)

    ;; second component
    (u32-ref imm0 3 seed)
    (rlwinm imm1 imm0 15 1 16)
    (srwi imm2 imm0 16)
    (mulli imm2 imm2 21069)
    (add imm0 imm1 imm2)

    ;; reduce mod m2
    (cmplw cr7 imm0 imm5)
    (blt cr7 @ok3)
    (sub imm0 imm0 imm5)
    @ok3

    (u32-ref imm1 5 seed)
    (rlwinm imm2 imm1 15 1 16)
    (srwi imm3 imm1 16)
    (mulli imm3 imm3 21069)
    (add imm2 imm2 imm3)

    ;; reduce mod m2
    (cmplw cr7 imm2 imm5)
    (blt cr7 @ok4)
    (sub imm2 imm2 imm5)
    @ok4

    (add imm2 imm1 imm2)
    (cmplw cr7 imm2 imm5)
    (blt cr7 @ok5)
    (sub imm2 imm2 imm5)
    @ok5

    (add imm2 imm2 imm0)
    (cmplw cr7 imm2 imm5)
    (blt cr7 @ok6)
    (sub imm2 imm2 imm5)
    @ok6

    ;; update state
    (u32-ref imm0 4 seed)
    (u32-set imm0 5 seed)
    (u32-ref imm0 3 seed)
    (u32-set imm0 4 seed)
    (u32-set imm2 3 seed)

    ;; construct m1 (1- (expt 2 31))
    (lis imm5 #x7fff)
    (ori imm5 imm5 #xffff)

    ;; combination
    (u32-ref imm0 0 seed)
    (cmplw cr7 imm0 imm2)
    (sub imm0 imm0 imm2)
    (bgt cr7 @finish)
    (add imm0 imm0 imm5)
    @finish
    #+ppc32-target
    (clrlwi imm0 imm0 3)		;don't want negative fixnums
    (box-fixnum arg_z imm0)
    (blr)))    

; End of ppc-numbers.lisp
