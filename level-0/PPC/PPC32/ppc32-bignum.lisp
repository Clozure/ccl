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

(eval-when (:compile-toplevel :execute)
  (require "PPC32-ARCH")
  (require "PPC-LAPMACROS")

    ;; Set RES to 1 if (u< x y), to 0 otherwise.
  (defppclapmacro sltu (res x y)
    `(progn
      (subfc ,res ,x ,y)
      (subfe ,res ,res ,res)
      (neg ,res ,res)))

    (defppclapmacro 48x32-divide (x-hi16 x-lo y freg temp-freg freg2 immx)
    `(let ((temp 16)
           (temp.h 16)
           (temp.l 20)
           (zero 8)
           (zero.h 8)
           (zero.l 12))
      (stwu tsp -24 tsp)
      (stw tsp 4 tsp)
      (lwi ,immx #x43300000)  ; 1075 = 1022+53 
      (stw ,immx zero.h tsp)
      (stw rzero zero.l tsp)
      (lfd ,temp-freg zero tsp)
      (rlwimi ,immx ,x-hi16 0 16 31)           
      (stw ,immx temp.h tsp)
      (stw ,x-lo temp.l tsp)
      (lfd ,freg temp tsp)
      
      (fsub ,freg ,freg ,temp-freg)
      (lwi ,immx #x43300000)
      (stw ,immx temp.h tsp)
      (stw ,y temp.l tsp)
      (lfd ,freg2 temp tsp)
      (lwz tsp 0 tsp)
      (fsub ,freg2 ,freg2 ,temp-freg)
      (fdiv ,freg ,freg ,freg2)
      ))
    
  )

;;; %BIGNUM-REF needs to access bignums as obviously as possible, and it needs
;;; to be able to return 32 bits somewhere no one looks for real objects.
;;;
;;; The easiest thing to do is to store the 32 raw bits in two fixnums
;;; and return multiple values.
(defppclapfunction %bignum-ref ((bignum arg_y) (i arg_z))
  (vref32 imm0 bignum i imm1)
  (digit-h temp0 imm0)
  (digit-l temp1 imm0)
  (vpush temp0)
  (vpush temp1)
  (la temp0 8 vsp)                      ; ?? why not (mr temp0 vsp) before vpushing?
  (set-nargs 2)                         ; that doesn't make any difference.  And, in this case,
                                        ; we can get away without setting nargs (since the caller
                                        ; called us with 2 args, but that's horrible style.)
  (ba .SPvalues))


;;; Set the 0th element of DEST (a bignum or some other 32-bit ivector)
;;; to the Ith element of the bignum SRC.
(defppclapfunction %ref-digit ((bignum arg_x) (i arg_y) (dest arg_z))
  (la imm1 ppc32::misc-data-offset i)
  (lwzx imm0 bignum imm1)
  (stw imm0 ppc32::misc-data-offset dest)
  (blr))

;;; BIGNUM[I] := DIGIT[0]
(defppclapfunction %set-digit ((bignum arg_x) (i arg_y) (digit arg_z))
  (la imm1 ppc32::misc-data-offset i)
  (lwz imm0 ppc32::misc-data-offset digit)
  (stwx imm0 bignum imm1)
  (blr))

;;; Return 0 if the 0th digit in X is 0.
(defppclapfunction %digit-zerop ((x arg_z))
  (lwz imm0 ppc32::misc-data-offset x)
  (cntlzw imm0 imm0)
  (srwi imm0 imm0 5)
  (rlwimi imm0 imm0 4 27 27)
  (addi arg_z imm0 (target-nil-value))
  (blr))

;;; store the sign of bignum (0 or -1) in the one-word bignum "digit".
(defppclapfunction %bignum-sign-digit ((bignum arg_y) (digit arg_z))
  (vector-length imm0 bignum imm0)
  (la imm0 (- ppc32::misc-data-offset 4) imm0) ; Reference last (most significant) digit
  (lwzx imm0 bignum imm0)
  (srawi imm0 imm0 31)			;propagate sign bit
  (stw imm0 ppc32::misc-data-offset digit)
  (blr))

;;; Return the sign of bignum (0 or -1) as a fixnum
(defppclapfunction %bignum-sign ((bignum arg_z))
  (vector-length imm0 bignum imm0)
  (la imm0 (- ppc32::misc-data-offset 4) imm0) ; Reference last (most significant) digit
  (lwzx imm0 bignum imm0)
  (srawi imm0 imm0 31)			;propagate sign bit
  (box-fixnum arg_z imm0)
  (blr))

;;; Count the sign bits in the most significant digit of bignum;
;;; return fixnum count.
(defppclapfunction %bignum-sign-bits ((bignum arg_z))
  (vector-length imm0 bignum imm0)
  (la imm0 (- ppc32::misc-data-offset 4) imm0) ; Reference last (most significant) digit
  (lwzx imm0 bignum imm0)
  (cmpwi imm0 0)
  (not imm0 imm0)
  (blt @wasneg)
  (not imm0 imm0)
  @wasneg
  (cntlzw imm0 imm0)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %digit-0-or-plusp ((bignum arg_y) (idx arg_z))
  (la imm0 ppc32::misc-data-offset idx)
  (lwzx imm0 bignum imm0)
  (xoris imm0 imm0 #x8000)		; invert sign bit
  (srwi imm0 imm0 31)
  (bit0->boolean arg_z imm0 imm0)	; return T if sign bit was clear before inversion
  (blr))

;;; For oddp, evenp
(defppclapfunction %bignum-oddp ((bignum arg_z))
  (lwz imm0 ppc32::misc-data-offset bignum)
  (clrlwi imm0 imm0 31)
  (bit0->boolean arg_z imm0 imm0)
  (blr))
  
(defppclapfunction bignum-plusp ((bignum arg_z))
  (vector-length imm0 bignum imm0)
  (la imm0 (- ppc32::misc-data-offset 4) imm0) ; Reference last (most significant) digit
  (lwzx imm0 bignum imm0)
  (xoris imm0 imm0 #x8000)		; invert sign bit
  (srwi imm0 imm0 31)
  (bit0->boolean arg_z imm0 imm0)	; return T if sign bit was clear before inversion
  (blr))

(defppclapfunction %fixnum-to-bignum-set ((bignum arg_y) (fixnum arg_z))
  (unbox-fixnum imm0 fixnum)
  (stw imm0 ppc32::misc-data-offset bignum)
  (blr))

(defppclapfunction bignum-minusp ((bignum arg_z))
  (vector-length imm0 bignum imm0)
  (la imm0 (- ppc32::misc-data-offset 4) imm0) ; Reference last (most significant) digit
  (lwzx imm0 bignum imm0)
  (srwi imm0 imm0 31)
  (rlwimi imm0 imm0 4 27 27)
  (addi arg_z imm0 (target-nil-value))	; return T if sign bit was clear before inversion
  (blr))


;;; Add the digits A[I] and B[J], and the incoming carry C (a fixnum).
;;; Store the result in R[K], and return the outgoing carry.
;;; If I is NIL, A is a fixnum.  If J is NIL, B is a fixnum.

(defppclapfunction %add-with-carry ((r 12) (k 8) (c 4) (a 0) (i arg_x) (b arg_y) (j arg_z))
  (cmpwi cr1 j (target-nil-value))
  (cmpwi cr0 i (target-nil-value))
  (lwz temp0 a vsp)
  (unbox-fixnum imm1 temp0)
  (unbox-fixnum imm2 b)
  (beq cr0 @got-a)
  (la imm1 ppc32::misc-data-offset i)
  (lwzx imm1 temp0 imm1)
  @got-a
  (beq cr1 @got-b)
  (la imm2 ppc32::misc-data-offset j)
  (lwzx imm2 b imm2)
  @got-b
  (lwz temp0 c vsp)
  (unbox-fixnum imm0 temp0)
  (addic imm0 imm0 -1)
  (lwz temp1 r vsp)
  (lwz temp0 k vsp)
  (la vsp 16 vsp)  
  (adde imm0 imm1 imm2)
  (la imm2 ppc32::misc-data-offset temp0)
  (stwx imm0 temp1 imm2)
  (addze imm0 rzero)
  (box-fixnum arg_z imm0)
  (blr))




    
;;; Store the result of A[I] - B[J] - borrow into R[K], returning the borrow.
;;; If I is NIL, A is a fixnum; likewise for J and B.
(defppclapfunction %subtract-with-borrow ((r 12) (k 8) (borrow 4) (a 0) (i arg_x) (b arg_y) (j arg_z))
  (cmpwi cr0 i (target-nil-value))
  (cmpwi cr1 j (target-nil-value))
  (lwz temp0 a vsp)
  (unbox-fixnum imm2 b)
  (unbox-fixnum imm1 temp0)
  (beq cr1 @got-b)
  (la imm2 ppc32::misc-data-offset j)
  (lwzx imm2 b imm2)
  @got-b
  (beq cr0 @got-a)
  (la imm1 ppc32::misc-data-offset i)
  (lwzx imm1 temp0 imm1)
  @got-a
  (lwz temp0 borrow vsp)
  (unbox-fixnum imm0 temp0)
  (addic imm0 imm0 -1)
  (lwz temp0 r vsp)
  (lwz temp1 k vsp)
  (la vsp 16 vsp)  
  (subfe imm0 imm2 imm1)
  (la imm1 ppc32::misc-data-offset temp1)
  (stwx imm0 temp0 imm1)
  (addze imm0 rzero)
  (box-fixnum arg_z imm0)
  (blr))

;; multiply i'th digit of x by y and add to result starting at digit i
(defppclapfunction %multiply-and-add-harder-loop-2
    ((x-ptr 4) (y-ptr 0) (resptr arg_x)(residx arg_y) (count arg_z))  
  (let ((tem imm0)
        (y imm1)
        (prod-h imm2)
        (prod-l imm3)
        (x imm4)
        (xptr temp2)
        (yidx temp1)
        (yptr temp0))
    (lwz xptr x-ptr vsp)
    (la tem ppc32::misc-data-offset residx)
    (lwzx x xptr tem)
    (lwz yptr y-ptr vsp)
    (li yidx 0) ; init yidx 0 
    (addc prod-h rzero rzero) ; init carry 0, mumble 0
    @loop
    (subi count count '1)
    (cmpwi count 0)
    (la tem ppc32::misc-data-offset yidx)   ; get yidx
    (lwzx y yptr tem) 
    (mullw prod-l x y)
    (addc prod-l prod-l prod-h)
    (mulhwu prod-h x y)
    (addze prod-h prod-h)
    (la tem ppc32::misc-data-offset residx)
    (lwzx y resptr tem)    
    (addc prod-l prod-l y)
    (addze prod-h prod-h)
    (stwx prod-l resptr tem)    
    (addi residx residx '1)
    (addi yidx yidx '1)
    (bgt @loop)
    (la tem ppc32::misc-data-offset residx)
    (stwx prod-h resptr tem)
    (la vsp 8 vsp)      
    (blr)))



;;; Multiply X[I] by the unboxed value of the (non-negative) fixnum Y;
;;; add the incoming carry from CARRY[0] to the 64-bit product.  Store
;;; the low word of the 64-bit sum in R[0] and the high word in
;;; CARRY[0].

(defppclapfunction %multiply-and-add ((r 4) (carry 0) (x arg_y) (i arg_x) (y arg_z))
  (unbox-fixnum imm0 arg_z)
  (la imm1 ppc32::misc-data-offset i)
  (lwzx imm1 x imm1)
  (mulhwu imm2 imm0 imm1)
  (mullw imm1 imm0 imm1)
  (lwz temp0 carry vsp)
  (lwz imm0 ppc32::misc-data-offset temp0)
  (addc imm1 imm1 imm0)
  (addze imm2 imm2)
  (stw imm2 ppc32::misc-data-offset temp0)
  (lwz arg_z r vsp)
  (la vsp 8 vsp)    
  (stw imm1 ppc32::misc-data-offset arg_z)
  (blr))
  
(defppclapfunction %floor ((q 4) (r 0) (num-high arg_x) (num-low arg_y) (denom-arg arg_z))
  (let ((rem imm0)
	(rem-low imm1)
	(quo imm2)
	(temp imm3)
	(denom imm4))
    (lwz denom ppc32::misc-data-offset denom)
    (lwz rem ppc32::misc-data-offset num-high)
    (lwz rem-low ppc32::misc-data-offset num-low)
    (mr temp denom)
    (sltu quo rem denom)
    (subi temp temp quo)
    (and temp temp denom)
    (sub rem temp rem)
    (li temp0 '32)
    @loop
    (subi temp0 temp0 '1)
    (cmpwi temp0 0)
    (slwi rem rem 1)
    (srwi temp rem-low 31)
    (or rem rem temp)
    (slwi rem-low rem-low 1)
    (sltu rem rem denom)
    (slwi quo quo 1)
    (or quo quo temp)
    (subi temp temp 1)
    (and temp temp denom)
    (sub rem rem temp)
    (bne @loop)
    (not quo quo)
    (lwz temp0 q vsp)
    (stw quo ppc32::misc-data-offset temp0)
    (lwz arg_z r vsp)
    (la vsp 8 vsp)  
    (stw rem ppc32::misc-data-offset arg_z)
    (blr)))

(defppclapfunction %bignum-ref-hi ((bignum arg_y) (i arg_z))
  (la imm1 ppc32::misc-data-offset i)
  (lhzx imm0 bignum imm1)
  (box-fixnum arg_z imm0)
  (blr))


(defppclapfunction %bignum-set ((bignum 0) (i arg_x) (high arg_y) (low arg_z))
  (compose-digit imm0 high low)
  (lwz arg_z bignum vsp)
  (vset32 imm0 arg_z i imm1)
  (la vsp 4 vsp)
  (blr))




; this is silly 
(defppclapfunction %add-the-carry ((b-h arg_x) (b-l arg_y) (carry-in arg_z))
  (let ((a imm0)
        (b imm1)
        (temp imm2)
        (c imm3))    
    (compose-digit b b-h b-l)
    (unbox-fixnum c carry-in)
    (add b c b)
    (digit-h temp0 b)
    (digit-l temp1 b)
    (vpush temp0)
    (vpush temp1)
    (la temp0 8 vsp)
    (set-nargs 2)
    (ba .SPvalues)))




;;; %SUBTRACT-WITH-BORROW -- Internal.
;;;
;;; This should be in assembler, and should not cons intermediate results.  It
;;; returns a 32bit digit and a borrow resulting from subtracting b from a, and
;;; subtracting a possible incoming borrow.
;;;
;;; We really do:  a - b - 1 + borrow, where borrow is either 0 or 1.
;;; 

(defppclapfunction %subtract-with-borrow-1 ((a-h 4) (a-l 0) (b-h arg_x) (b-l
arg_y) (borrow-in arg_z))
  (let ((a imm0)
        (b imm1)
        (temp imm2)
        (c imm3))
    (lwz temp0 a-h vsp)
    (lwz temp1 a-l vsp)
    (compose-digit a temp0 temp1)
    (compose-digit b b-h b-l)
    (unbox-fixnum c borrow-in)
    (li temp -1)
    (addc temp c temp)
    (subfe a b a)
    (addze c rzero)
    (box-fixnum c c)
    (digit-h temp0 a)
    (digit-l temp1 a)
    (vpush temp0)
    (vpush temp1)
    (vpush c)
    (la temp0 20 vsp)
    (set-nargs 3)
    (ba .SPvalues)))



(defppclapfunction %subtract-one ((a-h arg_y)(a-l arg_z))
  (let ((a imm0))
    (compose-digit a a-h a-l)
    (subi a a 1)
    (digit-h temp0 a)
    (vpush temp0)
    (digit-l temp0 a)
    (vpush temp0)
    (la temp0 8 vsp)
    (set-nargs 2)
    (ba .spvalues)))




;;; %MULTIPLY-AND-ADD  --  Internal.
;;;
;;; This multiplies x-digit and y-digit, producing high and low digits
;;; manifesting the result.  Then it adds the low digit, res-digit, and
;;; carry-in-digit.  Any carries (note, you still have to add two digits at a
;;; time possibly producing two carries) from adding these three digits get
;;; added to the high digit from the multiply, producing the next carry digit.
;;; Res-digit is optional since two uses of this primitive multiplies a single
;;; digit bignum by a multiple digit bignum, and in this situation there is no
;;; need for a result buffer accumulating partial results which is where the
;;; res-digit comes from.
;;; [slh] I assume that the returned carry "digit" can only be 0, 1 or 2


(defppclapfunction %multiply-and-add-1 ((x-high 8)
					(x-low 4)
					(y-high 0)
					(y-low arg_x)
					(carry-in-high arg_y)
					(carry-in-low arg_z))
  (let ((x imm0)
	(y imm1)
	(carry-in imm2)
	(lo imm3)
	(hi imm4))
    (compose-digit carry-in carry-in-high carry-in-low)
    (vpop temp0)
    (compose-digit y temp0 y-low)
    (vpop temp0)
    (vpop temp1)
    (compose-digit x temp1 temp0)
    (mullw lo x y)
    (mulhwu hi x y)
    (addc lo lo carry-in)
    (addze hi hi)
    (digit-h temp0 hi)
    (digit-l temp1 hi)
    (digit-h temp2 lo)
    (digit-l temp3 lo)
    (vpush temp0)
    (vpush temp1)
    (vpush temp2)
    (vpush temp3)
    (set-nargs 4)
    (la temp0 16 vsp)
    (ba .SPvalues)))


(defppclapfunction %logcount-complement ((bignum arg_y) (idx arg_z))
  (let ((arg imm0)
        (shift imm1)
        (temp imm2))
    (la arg ppc32::misc-data-offset idx)
    (lwzx arg bignum arg)
    (not. shift arg)
    (li arg_z 0)
    (if ne
      (progn
        @loop
        (la temp -1 shift)
        (and. shift shift temp)
        (la arg_z '1 arg_z)
        (bne @loop)))
    (blr)))

(defppclapfunction %logcount ((bignum arg_y) (idx arg_z))
  (let ((arg imm0)
        (shift imm1)
        (temp imm2))
    (la arg ppc32::misc-data-offset idx)
    (lwzx arg bignum arg)
    (mr. shift arg)
    (li arg_z 0)
    (if ne
      (progn
        @loop
        (la temp -1 shift)
        (and. shift shift temp)
        (la arg_z '1 arg_z)
        (bne @loop)))
    (blr)))

; return res
(defppclapfunction bignum-add-loop-2 ((aptr arg_x)(bptr arg_y) (result arg_z))
  (let ((idx imm0)
        (count imm1)
        (x imm2)
        (y imm3)        
        (len-a temp0)
        (len-b temp1)
        (tem temp2))
    (li idx ppc32::misc-data-offset)    
    (lwz imm4 ppc32::misc-header-offset aptr)
    (header-length len-a imm4)
    (lwz imm4 ppc32::misc-header-offset bptr)
    (header-length len-b imm4)
    ; make a be shorter one
    (cmpw len-a len-b)
    (li count 0)
    ; initialize carry 0
    (addc x rzero rzero)
    (ble @loop)
    ; b shorter - swap em
    (mr tem len-a)
    (mr len-a len-b)
    (mr len-b tem)
    (mr tem aptr)
    (mr aptr bptr)
    (mr bptr tem)    
    @loop
    (lwzx y aptr idx)
    (lwzx x bptr idx)    
    (addi count count '1)
    (cmpw count len-a)
    (adde x x y)
    (stwx x result idx)
    (addi idx idx '1)
    (blt @loop)
    ; now propagate carry thru longer (b) using sign of shorter    
    ;(SUBI imm4 idx '1) ; y has hi order word of a
    ;(lwzx y aptr imm4)
    (cmpw len-a len-b)
    (adde imm4 rzero rzero) ; get carry
    (srawi y y 31)  ; p.o.s clobbers carry 
    (addic imm4 imm4 -1)  ; restore carry
    (beq @l3)  ; unless equal
    @loop2
    (lwzx x bptr idx)
    (adde x x y)
    (stwx x result idx)
    (addi count count '1)
    (cmpw count len-b)
    (addi idx idx '1)
    (blt @loop2)
    ; y has sign of shorter - get sign of longer to x
    @l3
    (subi imm4 idx '1)
    (lwzx x bptr imm4)
    (adde imm4 rzero rzero) ; get carry
    (srawi x x 31)  ; clobbers carry 
    (addic imm4 imm4 -1)
    (adde x x y)
    (stwx x result idx)
    (blr)))

;; same as above but with initial a index and finishes
(defppclapfunction bignum-add-loop-+ ((init-a 0)(aptr arg_x)(bptr arg_y)(length arg_z))
  (let ((idx imm0)        
        (count imm1)
        (x imm2)
        (y imm3)
        (aidx imm4))
    (li idx ppc32::misc-data-offset)
    (lwz aidx init-a vsp)
    (addi aidx aidx ppc32::misc-data-offset)
    (li count 0)
    ; initialize carry 0
    (addc x rzero rzero)
    @loop
    (lwzx x aptr aidx)
    (lwzx y bptr idx)
    (adde x x y)
    (stwx x aptr aidx)
    (addi count count '1)
    (cmpw count length)
    (addi idx idx '1)
    (addi aidx aidx '1)
    (blt @loop)
    (lwzx x aptr aidx)  ; add carry into next one
    (adde x x  rzero)
    (stwx x aptr aidx)
    (la vsp 4 vsp)
    (blr)))



(defppclapfunction bignum-negate-loop-really ((big arg_x) (len arg_y) (result arg_z))
  (let ((idx imm0)
        (one imm1)
        (x imm2))
    (li idx ppc32::misc-data-offset)
    (li one '1)
    ; initialize carry 1
    (li x -1)
    (addic x x 1)
    @loop        
    ;(addi count count '1)    
    ;(cmpw count len)
    (subf. len one len)
    (lwzx x big idx)
    (not x x)
    (adde x x rzero)
    (stwx x result idx)    
    (addi idx idx '1)
    (bgt @loop)
    ; return carry
    (li x 0)
    (adde x x  rzero)
    (box-fixnum arg_z x)
    (blr)))

(defppclapfunction bignum-negate-to-pointer ((big arg_x) (len arg_y) (result arg_z))
  (let ((idx imm0)
        (one imm1)
        (x imm2)
        (oidx imm3)
        (ptr imm4))
    (li idx ppc32::misc-data-offset)
    (li oidx 0)
    (macptr-ptr ptr result)
    (li one '1)
    ; initialize carry 1
    (li x -1)
    (addic x x 1)
    @loop        
    ;(addi count count '1)    
    ;(cmpw count len)
    (subf. len one len)
    (lwzx x big idx)
    (not x x)
    (adde x x rzero)
    (stwx x ptr oidx)    
    (addi idx idx '1)
    (addi oidx oidx 4)
    (bgt @loop)
    ; return carry
    (li x 0)
    (adde x x  rzero)
    (box-fixnum arg_z x)
    (blr)))

;; she do tolerate len = jidx
(defppclapfunction bignum-shift-left-loop ((nbits 4)(result 0) (bignum arg_x) (len arg_y) (jidx arg_z))
  (let ((y imm0)
        (idx imm1)
        (bits imm2)
        (rbits imm3)
        (x imm4)
        (iidx temp0)
        (resptr temp1))
    (li iidx 0)
    (lwz bits nbits vsp)
    (lwz resptr result vsp)
    (unbox-fixnum bits bits)
    (subfic rbits bits 32)    
    ;(dbg)
    (lwz imm4 ppc32::misc-data-offset bignum)
    (slw imm4 imm4 bits)
    (la y (+ ppc32::misc-data-offset -4) jidx)  
    (stwx imm4 y resptr) 
     
    (cmpw len jidx)
    (beq @done)
    @loop
    (addi idx iidx ppc32::misc-data-offset)
    (lwzx x bignum idx)
    (srw x x rbits)
    (addi idx idx '1)
    (lwzx y bignum idx)
    (slw y y bits)
    (or x x y)
    (addi idx jidx ppc32::misc-data-offset)
    (stwx x resptr idx)
    (addi jidx jidx '1)    
    (cmpw jidx len)
    (addi iidx iidx '1)
    (blt @loop)    
    @done
    ; do first - lo order
       
    ; do last - hi order    
    (addi idx iidx ppc32::misc-data-offset)
    ;(dbg t)
    (lwzx y bignum idx)
    (sraw y y rbits)
    (addi idx len ppc32::misc-data-offset)
    (stwx y resptr idx)
    (la vsp 8 vsp)
    (blr)))



(defppclapfunction bignum-shift-right-loop-1 ((nbits 4)(result 0) (bignum arg_x) (len arg_y) (iidx arg_z))
  (let ((y imm0)
        (idx imm1)
        (bits imm2)
        (rbits imm3)
        (x imm4)
        (jidx temp0)
        (resptr temp1))
    (li jidx 0)
    (lwz bits nbits vsp)
    (lwz resptr result vsp)
    (unbox-fixnum bits bits)
    (cmpw jidx len)
    (subfic rbits bits 32)    
    (bge @done)
    @loop
    (addi idx iidx ppc32::misc-data-offset)
    (lwzx x bignum idx)
    (srw x x bits)
    (addi idx idx '1)
    (lwzx y bignum idx)
    (slw y y rbits)
    (or x x y)
    (addi idx jidx ppc32::misc-data-offset)
    (stwx x resptr idx)
    (addi jidx jidx '1)    
    (cmpw jidx len)
    (addi iidx iidx '1)
    (blt @loop)
    @done
    (addi idx iidx ppc32::misc-data-offset)
    (lwzx x bignum idx)
    (sraw x x bits)
    (addi idx jidx ppc32::misc-data-offset)
    (stwx x resptr idx)
    (la vsp 8 vsp)
    (blr)))


(defppclapfunction %compare-digits ((a arg_x) (b arg_y) (idx arg_z))
  (la imm0 ppc32::misc-data-offset idx)
  (lwzx imm1 a imm0)
  (lwzx imm0 b imm0)
  (cmplw imm1 imm0)
  (li arg_z '0)
  (beqlr)
  (li arg_z '1)
  (bgtlr)
  (li arg_z '-1)
  (blr))


  
;; returns number of bits in digit-hi,digit-lo that are sign bits
;; 32 - digits-sign-bits is integer-length

(defppclapfunction %digits-sign-bits ((hi arg_y) (lo arg_z))
  (rlwinm. imm1 hi (- 16 ppc32::fixnumshift) 0 15)
  (rlwimi imm1 lo (- 32 ppc32::fixnumshift) 16 31)
  (not imm1 imm1)
  (blt @wasneg)
  (not imm1 imm1)
  @wasneg
  (cntlzw imm1 imm1)
  (box-fixnum arg_z imm1)
  (blr))

(defppclapfunction bignum-logtest-loop ((count arg_x) (s1 arg_y) (s2 arg_z))  
  (addi imm1 rzero ppc32::misc-data-offset)
  @loop
  (lwzx imm2 s1 imm1)
  (lwzx imm3 s2 imm1)
  (and. imm2 imm3 imm2)  
  (addi imm1 imm1 4)
  (bne @true)
  (subic. count count 4)
  (bgt  @loop)
  (li arg_z (target-nil-value))
  (blr)
  @true
  (li arg_z (+ (target-nil-value)  ppc32::t-offset))
  (blr))

;;; dest[idx] <- (lognot src[idx])
(defppclapfunction %bignum-lognot ((idx arg_x) (src arg_y) (dest arg_z))
  (la imm1 ppc32::misc-data-offset idx)
  (lwzx imm0 src imm1)
  (not imm0 imm0)
  (stwx imm0 dest imm1)
  (blr))

;;; dest[idx] <- (logand x[idx] y[idx])
(defppclapfunction %bignum-logand ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop temp0)
  (la imm1 ppc32::misc-data-offset temp0)
  (lwzx imm0 x imm1)
  (lwzx imm2 y imm1)
  (and imm0 imm0 imm2)
  (stwx imm0 dest imm1)
  (blr))

;;; dest[idx] <- (logandc2 x[idx] y[idx])
(defppclapfunction %bignum-logandc2 ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop temp0)
  (la imm1 ppc32::misc-data-offset temp0)
  (lwzx imm0 x imm1)
  (lwzx imm2 y imm1)
  (andc imm0 imm0 imm2)
  (stwx imm0 dest imm1)
  (blr))

;;; dest[idx] <- (logandc1 x[idx] y[idx])
(defppclapfunction %bignum-logandc1 ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop temp0)
  (la imm1 ppc32::misc-data-offset temp0)
  (lwzx imm0 x imm1)
  (lwzx imm2 y imm1)
  (andc imm0 imm2 imm0)
  (stwx imm0 dest imm1)
  (blr))



(defppclapfunction digit-lognot-move ((index arg_x) (source arg_y) (dest arg_z))
  (let ((scaled-index imm1))
    (vref32 imm0 source index scaled-index) ; imm1 has c(index) + data-offset
    (not imm0 imm0)
    (stwx imm0 dest scaled-index)
    (blr)))

; if dest not nil store unboxed result in dest(0), else return boxed result
(defppclapfunction fix-digit-logandc2 ((fix arg_x) (big arg_y) (dest arg_z)) ; index 0
  (let ((w1 imm0)
        (w2 imm1))
    (unbox-fixnum  w1 fix)
    (lwz w2 ppc32::misc-data-offset big)
    (cmpwi dest (target-nil-value))
    (not w2 w2)
    (and w1 w1 w2)
    (bne @store)
    (box-fixnum arg_z w1)
    (blr)
    @store
    (stw w1 ppc32::misc-data-offset dest)
    (blr)))



(defppclapfunction fix-digit-logand ((fix arg_x) (big arg_y) (dest arg_z)) ; index 0
  (let ((w1 imm0)
        (w2 imm1))
    (unbox-fixnum  w1 fix)
    (lwz w2 ppc32::misc-data-offset big)
    (cmpwi dest (target-nil-value))
    (and w1 w1 w2)
    (bne @store)
    (box-fixnum arg_z w1)
    (blr)
    @store
    (stw w1 ppc32::misc-data-offset dest)
    (blr)))



(defppclapfunction fix-digit-logandc1 ((fix arg_x) (big arg_y) (dest arg_z)) ; index 0
  (let ((w1 imm0)
        (w2 imm1))
    (unbox-fixnum  w1 fix)
    (lwz w2 ppc32::misc-data-offset big)
    (cmpwi dest (target-nil-value))
    (not w1 w1)
    (and w1 w1 w2)
    (bne @store)
    (box-fixnum arg_z w1)
    (blr)
    @store
    (stw w1 ppc32::misc-data-offset dest)
    (blr)))

;;; dest[idx] <- (logior x[idx] y[idx])
(defppclapfunction %bignum-logior ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop temp0)
  (la imm1 ppc32::misc-data-offset temp0)
  (lwzx imm0 x imm1)
  (lwzx imm2 y imm1)
  (or imm0 imm0 imm2)
  (stwx imm0 dest imm1)
  (blr))

;;; dest[idx] <- (logxor x[idx] y[idx])
(defppclapfunction %bignum-logxor ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop temp0)
  (la imm1 ppc32::misc-data-offset temp0)
  (lwzx imm0 x imm1)
  (lwzx imm2 y imm1)
  (xor imm0 imm0 imm2)
  (stwx imm0 dest imm1)
  (blr))



(defppclapfunction bignum-xor-loop ((count 0) (s1 arg_x) (s2 arg_y) (dest arg_z))
  (lwz imm0 count vsp)
  (addi imm1 rzero ppc32::misc-data-offset)
  @loop
  (lwzx imm2 s1 imm1)
  (lwzx imm3 s2 imm1)
  (xor imm2 imm3 imm2)
  (subic. imm0 imm0 4)
  (stwx imm2 dest imm1)
  (addi imm1 imm1 4)
  (bgt @loop)
  @out
  (la vsp 4 vsp)
  (blr))

#+nomore
(defppclapfunction try-guess-loop-1 ((guess-h 8)(guess-l 4)(len-y 0)
                                     (xidx arg_x) (xptr arg_y) (yptr arg_z))
  (let ((guess imm0)
        (carry imm1)
        (y imm2)
        (x imm2)
        (prod-l imm3)
        (prod-h imm4)
        (tem imm4)
        (yidx temp0)
        (end-y temp1)
        (carry-bit temp2))
    (lwz x guess-h vsp)
    (lwz tem guess-l vsp)
    (compose-digit guess x tem)
    (lwz end-y len-y vsp)
    (li yidx 0)
    (li carry 0) 
    (li carry-bit '1)
    @loop
    ; multiply guess by ydigit, add carry to lo, hi is new carry
    ; then get an xdigit subtract prod-lo from it and store result in x (remember carry)
    (addi tem yidx ppc32::misc-data-offset)   ; get yidx
    (lwzx y yptr tem)
    (mullw prod-l guess y)
    (mulhwu prod-h guess y)    
    (addc prod-l prod-l carry) 
    (adde carry prod-h rzero)
    ; get back saved carry
    (li tem '-1)
    (addc tem carry-bit tem)
    (addi tem xidx ppc32::misc-data-offset)
    (lwzx x xptr tem)    
    (subfe x prod-l x)        
    (stwx x xptr tem)
    ; save carry
    (adde prod-l rzero rzero)
    (box-fixnum carry-bit prod-l)
    (addi yidx yidx '1)
    (cmpw yidx end-y)
    (addi xidx xidx '1)
    (blt @loop)
    ; finally subtract carry from last x digit
    @done
    (li prod-l '-1)  ; get back saved carry again - box clobbered it?
    (addc prod-l carry-bit prod-l)
    (addi tem xidx ppc32::misc-data-offset) ; maybe still there - nope
    (lwzx x xptr tem)
    (subfe x carry x)
    (stwx x xptr tem)
    (la vsp 12 vsp)
    (blr)))

;; x0 is at index, x1 at index-1, x2 at index-2
;; y1 is at index, y2 at index-1
;; this doesnt help much
(defppclapfunction truncate-guess-loop ((guess-h 8)(guess-l 4)(x 0)
                                        (xidx arg_x)(yptr arg_y) (yidx arg_z))
  (let ((guess imm0)
        (y1 imm1)
        (y2 imm1)
        (gy1-lo imm2) ; look out below
        (gy1-hi imm2)
        (gy2-lo imm2)
        (gy2-hi imm2)
        (xptr temp0)
        (m imm3)
        (tem imm4)
        (y1-idx 28)
        (y2-idx 24)
        (x0-idx 20)
        (x1-idx 16)
        (x2-idx 12))
    (stwu tsp -32 tsp)
    (stw tsp 4 tsp)
    (lwz y1 guess-h vsp)
    (lwz tem guess-l vsp)
    (compose-digit guess y1 tem)
    (addi tem yidx ppc32::misc-data-offset)
    (lwzx y1 yptr tem)
    (stw y1 y1-idx tsp)
    (subi tem tem 4)
    (lwzx y2 yptr tem)
    (stw y2 y2-idx tsp)
    (lwz xptr x vsp)
    (addi tem xidx ppc32::misc-data-offset)
    (lwzx y1 xptr tem) ; its x0
    (stw y1 x0-idx tsp)
    (subi tem tem 4)
    (lwzx y1 xptr tem)
    (stw y1 x1-idx tsp)
    (subi tem tem 4)
    (lwzx y1 xptr tem)
    (stw y1 x2-idx tsp)
    @loop
    (lwz y1 y1-idx tsp)     ; get y1
    (mullw gy1-lo guess y1)
    (lwz m x1-idx tsp)      ; get x1
    (subc m m gy1-lo)      ; x1 - gy1-lo => m
    (mulhwu gy1-hi guess y1)
    (lwz tem x0-idx tsp)    ; get x0
    (subfe. tem gy1-hi tem)      ; - val not used just cr
    (lwz y2 y2-idx tsp)     ; get y2
    (mulhwu gy2-hi guess y2)   ; does it pay to do this now even tho may not need?
    (bne @done)
    (cmpl :cr0 gy2-hi m)       ; if > or = and foo then more - L means logical means unsigned
    (blt @done)           ; if < done
    (bne @more)           ; if = test lo
    (mullw gy2-lo guess y2)
    (lwz tem x2-idx tsp) ; get x2
    (cmpl :cr0 gy2-lo tem)
    (ble @done)
    @more
    (subi guess guess 1)
    (b @loop)
    @done
    (digit-h temp0 guess)
    (vpush temp0)
    (digit-l temp0 guess)
    (vpush temp0)
    (la temp0 20 vsp)
    (lwz tsp 0 tsp)
    (set-nargs 2)
    (ba .spvalues)))

(defppclapfunction normalize-bignum-loop ((sign arg_x)(res arg_y)(len arg_z))
  (let ((idx imm0)
        (usign imm1)
        (val imm2))      
    (unbox-fixnum usign sign)
    (cmpwi len 0)
    (addi idx len (- ppc32::misc-data-offset 4))  
    (beqlr) ; huh - can this ever happen?
    @loop
    (lwzx val res idx)
    (cmpw  val usign)    
    (subi idx idx '1)
    (bne @neq)    
    (subic. len len '1)
    (bgt @loop)
    ; fall through - its all sign - return 1
    (li arg_z '1)
    (blr)
    @neq
    (rlwinm usign usign 0 0 0) ; hi bit
    (rlwinm val val 0 0 0)
    (cmpw usign val)  ; is hi bit = sign, if so then done   
    (beqlr)
    (addi len len '1) ; if not, need 1 more
    (blr)))

(defppclapfunction %normalize-bignum-2 ((fixp arg_y)(res arg_z))
  (let ((idx imm0)
        (usign imm1)
        (val imm2)
        (len arg_x)
        (oldlen temp0))
    (lwz imm4 (- ppc32::fulltag-misc) res)
    (header-length len imm4)
    (cmpwi len 0)
    (mr oldlen len)
    (addi idx len (- ppc32::misc-data-offset 4))  
    (beqlr) ; huh - can this ever happen?
    (lwzx val res idx) ; high order word
    (srawi usign val 31) ; get sign
    @loop
    (lwzx val res idx)
    (cmpw  val usign)    
    (subi idx idx '1)
    (bne @neq)    
    (subic. len len '1)
    (bgt @loop)
    ; fall through - its all sign - return 1
    (li len '1)
    (rlwinm usign usign 0 0 0) ; hi bit
    (b @more)
    @neq
    (rlwinm usign usign 0 0 0) ; hi bit
    (rlwinm val val 0 0 0)
    (cmpw usign val)  ; is hi bit = sign, if so then done   
    (beq @more)
    (addi len len '1) ; if not, need 1 more
    (b @big)
    @more
    (cmpwi :cr1 fixp (target-nil-value))
    (cmpwi len '1)
    (beq :cr1 @big)  ; dont return fixnum
    (bgt @big)
    ;; stuff for maybe fixnum
    ;(dbg t)
    (lwz val ppc32::misc-data-offset res)
    (rlwinm imm4 val 0 0 2) ; hi 3 bits same? - we assume fixnumshift is 2
    (srawi usign usign 2)
    (cmpw usign imm4)
    (bne @big)    
    (box-fixnum arg_z val)
    (blr)
    @big
    (cmpw oldlen len)
    (beqlr) ; same length - done
    (li imm4 ppc32::subtag-bignum) ; set new length
    (rlwimi imm4 len (- ppc32::num-subtag-bits ppc32::fixnumshift) 0 (- 31 ppc32::num-subtag-bits))
    (stw imm4 ppc32::misc-header-offset res)
    ; 0 to tail if negative
    (cmpwi usign 0)
    (beqlr) 
     ; zero from len inclusive to oldlen exclusive
    ;(dbg t)
    (addi idx len ppc32::misc-data-offset)
    @loop2
    (stwx rzero idx res)
    (addi len len '1)
    (cmpw len oldlen)
    (addi idx idx '1)
    (blt @loop2)
    (blr)))

(defppclapfunction %count-digit-leading-zeros ((high arg_y) (low arg_z))
  (compose-digit imm0 high low)
  (cntlzw imm0 imm0)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %count-digit-trailing-zeros ((high arg_y) (low arg_z))
  (compose-digit imm0 high low)
  (neg imm1 imm0)
  (and imm0 imm0 imm1)
  (cntlzw imm0 imm0)
  (subfic imm0 imm0 31)
  (box-fixnum arg_z imm0)
  (blr))


(defppclapfunction %bignum-count-trailing-zero-bits ((bignum arg_z))
  (let ((ndigits arg_x)
	(nbits arg_y)
	(digit imm0)
	(ptr imm1))
    (li ptr ppc32::misc-data-offset)
    (li ndigits '-32)
    @next
    (lwzx digit bignum ptr)
    (cmpwi digit 0)
    (la ptr 4 ptr)
    (addi ndigits ndigits '32)
    (beq @next)
    (neg ptr digit)
    (and digit digit ptr)
    (cntlzw digit digit)
    (subfic digit digit 31)
    (box-fixnum nbits digit)
    (add arg_z nbits ndigits)
    (blr)))


(defppclapfunction %bignum-trim-leading-zeros ((bignum arg_x)
					       (start arg_y)
					       (len arg_z))
  (add imm1 start len)
  (la imm1 (- ppc32::misc-data-offset 4) imm1)
  @loop
  (cmpwi cr0 len '1)
  (lwzx imm0 bignum imm1)
  (cmpwi cr1 imm0 0)
  (la imm1 -4 imm1)
  (bnelr cr1)
  (la len '-1 len)
  (bne @loop)
  (blr))
  
;;; Set length of bignum to new-len (zeroing out any trailing words between
;;; the old length and the new.
(defppclapfunction %shrink-bignum ((new-len arg_y) (bignum arg_z))
  (let ((old-len temp0)
	(old-idx imm0)
	(new-idx imm2)
	(header imm1))
    (getvheader header bignum)
    (header-length old-len header)
    (cmpw old-len new-len)
    (la old-idx ppc32::misc-data-offset old-len)
    (la new-idx ppc32::misc-data-offset new-len)
    (beqlr)
    @loop
    (subi old-idx old-idx 4)
    (cmpw old-idx new-idx)
    (stwx ppc32::rzero bignum old-idx)
    (bne @loop)
    (slwi header new-len (- ppc32::num-subtag-bits ppc32::fixnumshift))
    (ori header header ppc32::subtag-bignum)
    (stw header ppc32::misc-header-offset bignum)
    (blr)))
    
;;; Especially when large operands are involved, the GNU Multiple Precision
;;; library's algorithm's are often faster than Clozure CL's.  GMP's MPN
;;; library defines operations on "limb vectors", which are basically
;;; just sequences of 32-bit digits (least-significant digit first), which
;;; is just about exactly the same way that Clozure CL stores bignums.
;;; We might want to (eventually) link some or all of GMP into Clozure CL;
;;; in the meantime, it seems that we get some performance benefit from
;;; using GMP representation and algorithms in some mixture of LAP and Lisp.
;;; To approximate the "limb vector" representation, we copy operands to
;;; (and results from) stack-allocated macptrs.  Since those macptrs are
;;; word-aligned, we can use fixnums to represent word-aligned pointers.
;;; Obviously, it costs a little to copy back and forth like this; we
;;; only win when operands are fairly large, and when we can replace an
;;; N^2 algorithm with something cheaper.

;;; Macptr MUST be word-aligned (low 2 bits must be 0).  Extract
;;; such an address, return it as a fixnum.
(defppclapfunction macptr->fixnum ((ptr arg_z))
  (macptr-ptr arg_z ptr)
  (blr))

;;; Copy the limb SRC points to to where DEST points.
(defppclapfunction copy-limb ((src arg_y) (dest arg_z))
  (lwz imm0 0 src)
  (stw imm0 0 dest)
  (blr))

;;; Return T iff LIMB contains 0.
(defppclapfunction limb-zerop ((limb arg_z))
  (lwz imm0 0 limb)
  (cntlzw imm0 imm0)
  (srwi imm0 imm0 5)
  (bit0->boolean arg_z imm0 imm0)
  (blr))

;;; Return -1,0,1 according to whether the contents of Y are
;;; <,=,> the contents of Z.
(defppclapfunction compare-limbs ((y arg_y) (z arg_z))
  (lwz imm1 0 z)
  (lwz imm0 0 y)
  (cmplw imm0 imm1)
  (li arg_z 0)
  (beqlr)
  (li arg_z '1)
  (bgtlr)
  (li arg_z '-1)
  (blr))

;;; Add a fixnum to the limb LIMB points to.  Ignore overflow.
(defppclapfunction add-fixnum-to-limb ((fixnum arg_y) (limb arg_z))
  (unbox-fixnum imm0 fixnum)
  (lwz imm1 0 limb)
  (add imm1 imm1 imm0)
  (stw imm1 0 limb)
  (blr))

;;; Store a fixnum value where LIMB points.
(defppclapfunction copy-fixnum-to-limb ((fixnum arg_y) (limb arg_z))
  (unbox-fixnum imm0 fixnum)
  (stwu imm0 0 limb)
  (blr))

;;; Increment a "LIMB VECTOR" (bignum) by a small amount.  The caller
;;; knows that carries will only propagate for a word or two.
(defppclapfunction mpn-incr-u ((limb arg_y) (fixby arg_z))
  (let ((by imm0)
	(sum imm1))
    (unbox-fixnum by fixby)
    @loop
    (lwz sum 0 limb)
    (add sum sum by)
    (cmplw sum by)
    (stw sum 0 limb)
    (li by 1)
    (la limb 4 limb)
    (blt @loop)
    (blr)))

;;; Store XP-YP at WP; return carry (0 or 1).
;;; wp, xp, yp: word-aligned, unboxed ptrs (fixnums)
;;; size: boxed fixnum
;;; returns boxed carry 
(defppclapfunction mpn-sub-n ((wp 0) (xp arg_x) (yp arg_y) (size arg_z))
  (vpop imm0)
  (subi size size '1)
  (cmpwi size 0)
  (lwz imm3 0 xp)
  (lwz imm4 0 yp)
  (sub imm1 xp imm0)			; imm1 = xp-wp
  (sub imm2 yp imm0)			; imm2 = yp-wp
  (addi imm1 imm1 4)			; imm1 = xp-wp+4
  (addi imm2 imm2 4)			; imm2 = yp-wp+4
  (subfc imm3 imm4 imm3)
  (stw imm3 0 imm0)			; wp[0]
  (beq @done)
  @top
  (subi size size '1)
  (cmpwi size 0)
  (lwzx imm3 imm1 imm0)			; imm3 = xp[i]
  (lwzx imm4 imm2 imm0)			; imm4 = xp[i]
  (subfe imm3 imm4 imm3)
  (stwu imm3 4 imm0)
  (bne @top)
  @done
  (subfe imm0 rzero rzero)
  (subfic imm0 imm0 0)
  (box-fixnum arg_z imm0)
  (blr))

;;; Store XP+YP at WP; return carry (0 or 1).
;;; wp, xp, yp = word-aligned, unboxed macptrs (fixnums).
;;; size = boxed fixnum
;;; result = boxed carry
(defppclapfunction mpn-add-n ((wp 0) (xp arg_x) (yp arg_y) (size arg_z))
  (vpop imm0)
  (subi size size '1)
  (cmpwi size 0)
  (lwz imm3 0 xp)
  (lwz imm4 0 yp)
  (sub imm1 xp imm0)			; imm1 = xp-wp
  (sub imm2 yp imm0)			; imm2 = yp-wp
  (addi imm1 imm1 4)			; imm1 = xp-wp+4
  (addi imm2 imm2 4)			; imm2 = yp-wp+4
  (addc imm3 imm3 imm4)
  (stw imm3 0 imm0)			; wp[0]
  (beq @done)
  @top
  (subi size size '1)
  (cmpwi size 0)
  (lwzx imm3 imm1 imm0)			; imm3 = xp[i]
  (lwzx imm4 imm2 imm0)			; imm4 = xp[i]
  (adde imm3 imm4 imm3)
  (stwu imm3 4 imm0)
  (bne @top)
  @done
  (addze imm0 rzero)
  (box-fixnum arg_z imm0)
  (blr))

;;; Add the single limb LIMB to S1P (propagating carry.)  Store the
;;; result at RP.  RP and S1P may be the same place, so check for
;;; that and do nothing after carry stops propagating.  Return carry.
(defppclapfunction mpn-add-1 ((rp-offset 0) (s1p arg_x) (size arg_y) (limb arg_z))
  (let ((rp temp0))
    (vpop rp)
    (subi size size '1)
    (cmpwi cr2 size 0)
    (cmpw cr1 rp s1p)			;a common case
    (subi rp rp 4)
    (subi s1p s1p 4)
    (lwz imm0 0 limb)
    (lwzu imm1 4 s1p)
    (addc imm1 imm1 imm0)
    (addze. imm0 rzero)
    (stwu imm1 4 rp)
    (beq cr2 @done)
    @top
    (beq cr0 @finish)			; branch if  no more carry
    (subi size size '1)
    (cmpwi cr2 size 0)
    (lwzu imm1 4 s1p)
    (addc imm1 imm1 imm0)
    (addze. imm0 rzero)
    (stwu imm1 4 rp)
    (bne cr2 @top)
    (box-fixnum arg_z imm0)
    (blr)
    @finish
    (beq cr1 @done)
    @loop
    (subi size size '1)
    (cmpwi cr2 size 0)
    (lwzu imm1 4 s1p)
    (stwu imm1 4 rp)
    (bne cr2 @loop)
    @done
    (box-fixnum arg_z imm0)
    (blr)))
;;; Multiply the limb vector S1 by the single limb at LIMBPTR, storing
;;; the result at RES.  Store the "carry out" (high word of last 64-bit
;;; partial product) at the limb RESULT.
;;; res, s1, limbptr, result:
;;;   unboxed, word-aligned ptrs (fixnums).  size: boxed fixnum
;;; It'd be hard to transliterate the GMP code here; the GMP version
;;; uses lots more immediate registers than we can easily use in LAP
;;; (and is much more aggressively pipelined).
(defppclapfunction mpn-mul-1 ((res-offset 4)
			      (s1-offset 0)
			      (size arg_x)
			      (limbptr arg_y)
			      (result arg_z))
  (let ((limb imm0)
	(resptr temp0)
	(s1 temp1)
	(src imm1)
	(prod-low imm2)
	(prod-high imm3)
	(carry imm4))
    (lwz resptr res-offset vsp)
    (lwz s1 s1-offset vsp)
    (la vsp 8 vsp)
    (la resptr -4 resptr)		; pre-decrement
    (la s1 -4 s1)
    (addic carry carry 0)
    (li carry 0)
    (lwz limb 0 limbptr)
    @loop
    (subi size size '1)
    (cmpwi size 0)
    (lwzu src 4 s1)
    (mulhwu prod-high src limb)
    (mullw prod-low src limb)
    (addc prod-low prod-low carry)
    (addze carry prod-high)
    (stwu prod-low 4 resptr)
    (bne @loop)
    (stw carry 0 result)
    (blr)))

;;; multiply s1*limb and add result to res
;;; res, s1, limbptr, result:
;;;   unboxed, word-aligned ptrs (fixnums).
;;; size: boxed fixnum
;;; limbptr: source "limb".
;;; result: carry out (high word of product).
(defppclapfunction mpn-addmul-1 ((res-offset 4)
				 (s1-offset 0)
				 (size arg_x)
				 (limbptr arg_y)
				 (result arg_z))
  (let ((limb imm0)
	(resptr temp0)
	(s1 temp1)
	(src imm1)
	(prod-low imm2)
	(prod-high imm3)
	(carry imm4)
	(prev imm4))
    (lwz resptr res-offset vsp)
    (lwz s1 s1-offset vsp)
    (la vsp 8 vsp)
    (la resptr -4 resptr)		; pre-decrement
    (la s1 -4 s1)
    (addic carry carry 0)
    (li carry 0)
    (lwz limb 0 limbptr)
    @loop
    (subi size size '1)
    (cmpwi size 0)
    (lwzu src 4 s1)
    (mulhwu prod-high src limb)
    (mullw prod-low src limb)
    (addc prod-low prod-low carry)
    (addze prod-high prod-high)
    (lwz prev 4 resptr)
    (addc prev prev prod-low)
    (stwu prev 4 resptr)
    (addze carry prod-high)
    (bne @loop)
    (stw carry 0 result)
    (blr)))  

;;; Multiply the UN-word limb vector at UP and the VN-word limb vector
;;; at VP, store the result at RP.
(defppclapfunction mpn-mul-basecase ((rp-offset 4)
				     (up-offset 0)
				     (un arg_x)
				     (vp arg_y)
				     (vn arg_z))
  (let ((resptr temp0)
	(s1 temp1)
	(up temp2)
	(rp temp3)
	(size nargs)
	(limb imm0)
	(src imm1)
	(prod-low imm2)
	(prod-high imm3)
	(prev imm4)
	(carry imm4))
    (lwz resptr rp-offset vsp)
    (la rp -4 resptr)
    (lwz up up-offset vsp)
    (la s1 -4 up)
    (la vsp 8 vsp)
    (mr size un)
    (lwz limb 0 vp)
    (subi vn vn '1)
    (cmpwi cr2 vn 0)
    (li carry 0)
    @mul-1-loop
    (subi size size '1)
    (cmpwi size 0)
    (lwzu src 4 s1)
    (mulhwu prod-high src limb)
    (mullw prod-low src limb)
    (addc prod-low prod-low carry)
    (addze carry prod-high)
    (stwu prod-low 4 rp)
    (bne @mul-1-loop)
    (stw carry 4 rp)
    @again
    (beq cr2 @done)
    (subi vn vn '1)
    (cmpwi cr2 vn 0)
    (mr rp resptr)
    (la resptr 4 resptr)
    (la s1 -4 up)
    (lwzu limb 4 vp)
    (mr size un)
    (addic carry carry 0)
    (li carry 0)
    @addmul-1-loop
    (subi size size '1)
    (cmpwi size 0)
    (lwzu src 4 s1)
    (mulhwu prod-high src limb)
    (mullw prod-low src limb)
    (addc prod-low prod-low carry)
    (addze prod-high prod-high)
    (lwz prev 4 rp)
    (addc prev prev prod-low)
    (stwu prev 4 rp)
    (addze carry prod-high)
    (bne @addmul-1-loop)
    (stw carry 4 rp)
    (b @again)
    @done
    (li arg_z (target-nil-value))
    (blr)))

;;; left-shift src by 1 bit, storing result at res.  Return
;;; the bit that was shifted out.
(defppclapfunction mpn-lshift-1 ((resptr arg_x) (s1ptr arg_y) (size-arg arg_z))
  (let ((size temp0)
	(last-bit imm0)
	(prev imm1)
	(curr imm2)
	(sleft imm3)
	(sright imm4))
    (subi size size-arg '1)
    (cmpwi size 0)
    (add resptr resptr size-arg)
    (add s1ptr s1ptr size-arg)
    (lwzu prev -4 s1ptr)
    (srwi last-bit prev 31)
    (box-fixnum arg_z last-bit)
    (beq @end1)
    @loop
    (subi size size '1)
    (cmpwi size 0)
    (lwzu curr -4 s1ptr)
    (slwi sleft prev 1)
    (srwi sright curr 31)
    (or sright sright sleft)
    (stwu sright -4 resptr)
    (beq @end2)
    (subi size size '1)
    (cmpwi size 0)
    (lwzu prev -4 s1ptr)
    (slwi sleft curr 1)
    (srwi sright prev 31)
    (or sright sright sleft)
    (stwu sright -4 resptr)
    (bne @loop)
    @end1
    (slwi sleft prev 1)
    (stwu sleft -4 resptr)
    (blr)
    @end2
    (slwi sleft curr 1)
    (stwu sleft -4 resptr)
    (blr)))

;;; Do a 32x32=64 unsigned multiply of the words at X and Y.  Store
;;; result (low word first) at RESULT.
(defppclapfunction umulppm ((x arg_x) (y arg_y) (result arg_z))
  (lwz imm0 0 x)
  (lwz imm1 0 y)
  (mullw imm2 imm0 imm1)
  (mulhwu imm3 imm0 imm1)
  (stw imm2 0 result)
  (stw imm3 4 result)
  (blr))


;;; for truncate-by-fixnum etal
;;; doesnt store quotient - just returns rem in 2 halves
(defppclapfunction %floor-loop-no-quo ((q arg_x)(yhi arg_y)(ylo arg_z))
  (let ((a imm1)
        (b imm2)
        (y imm3)
        (quo imm0)
        (qidx temp0)
        (qlen temp1))
    (lwz imm4 (- ppc32::fulltag-misc) q)
    (header-length qlen imm4)
    (subi qidx qlen 4)
    (mr b rzero)
    (compose-digit y yhi ylo)
    @loop
    (rlwinm a b -16 16 31)
    (rlwinm b b 16 0 15)
    (la imm4 ppc32::misc-data-offset q)
    (lwzx imm4 qidx imm4) ; q contents
    (rlwimi b imm4 16 16 31) ; hi 16 to lo b
    ;(dbg)         
    (48x32-divide a b y fp0 fp1 fp2 imm4)
    (fctiwz fp0 fp0)
    (stwu tsp -32 tsp)
    (stw tsp 4 tsp)
    (stfd fp0 24 tsp)
    (lwz quo (+ 24 4) tsp) ; 16 quo bits above stuff used by 48x32
    ; now mul quo by y
    (mullw imm4 y quo)
    ; and subtract from a,b
    (subfc b imm4 b)
    ; new a and b are low 2 digits of this (b) and last digit in array
    ; and do it again on low 3 digits
    ;(dbg)
    (rlwinm a b -16 16 31)
    (rlwinm b b 16 0 15)
    (la imm4 ppc32::misc-data-offset q)
    (lwzx imm4 qidx imm4)
    (rlwimi b imm4 0 16 31)
    (48x32-divide a b y fp0 fp1 fp2 imm4)
    (fctiwz fp0 fp0)
    (stfd fp0 16 tsp)  ; quo lo
    (subi qidx qidx 4)
    (cmpwi :cr1 qidx 0)
    (lwz quo (+ 16 4) tsp)
    (lwz tsp 0 tsp)
    (mullw imm4 y quo)
    (subfc b imm4 b)  ; b is remainder
    (bge :cr1 @loop)
    (digit-h temp0 b)
    (vpush temp0)
    (digit-l temp0 b)
    (vpush temp0)
    (la temp0 8 vsp)
    (set-nargs 2)
    (ba .SPvalues)))
    

; store result in dest, return rem in 2 halves
(defppclapfunction %floor-loop-quo ((q-stk 0)(dest arg_x)(yhi arg_y)(ylo arg_z))
  (let ((a imm1)
        (b imm2)
        (y imm3)
        (quo imm0)
        (qidx temp0)
        (qlen temp1)
        (q temp2))
    (vpop q)
    (lwz imm4 (- ppc32::fulltag-misc) q)
    (header-length qlen imm4)
    (subi qidx qlen 4)
    (mr b rzero)
    (compose-digit y yhi ylo)
    @loop
    (rlwinm a b -16 16 31)
    (rlwinm b b 16 0 15)
    (la imm4 ppc32::misc-data-offset q)
    (lwzx imm4 qidx imm4) ; q contents
    (rlwimi b imm4 16 16 31) ; hi 16 to lo b        
    (48x32-divide a b y fp0 fp1 fp2 imm4)
    (fctiwz fp0 fp0)
    (stwu tsp -32 tsp)
    (stw tsp 4 tsp)
    (stfd fp0 24 tsp)
    (lwz quo (+ 24 4) tsp) ; 16 quo bits above stuff used by 48x32
    ; now mul quo by y
    (mullw imm4 y quo)
    ; and subtract from a,b
    (subfc b imm4 b)
    ; new a and b are low 2 digits of this (b) and last digit in array
    ; and do it again on low 3 digits
    ;(dbg)
    (rlwinm a b -16 16 31)
    (rlwinm b b 16 0 15)
    (la imm4 ppc32::misc-data-offset q)
    (lwzx imm4 qidx imm4)
    (rlwimi b imm4 0 16 31)
    (48x32-divide a b y fp0 fp1 fp2 imm4)
    (fctiwz fp0 fp0)
    (stfd fp0 16 tsp)  ; quo lo
    (lwz quo (+ 16 4) tsp)
    (mullw imm4 y quo)
    (subfc b imm4 b)  ; b is remainder    
    (lwz quo (+ 24 4) tsp) ; quo-hi
    (rlwinm quo quo 16 0 15)
    (lwz imm4 (+ 16 4) tsp) ; quo lo
    (lwz tsp 0 tsp)
    (rlwimi quo imm4 0 16 31)    
    (la imm4 ppc32::misc-data-offset dest)
    (stwx quo qidx imm4)
    (subic. qidx qidx 4)
    (bge @loop)
    (digit-h temp0 b)
    (vpush temp0)
    (digit-l temp0 b)
    (vpush temp0)
    (la temp0 8 vsp)
    (set-nargs 2)
    (ba .SPvalues)))

;;; get xidx thing from x, yidx thing from y if same return #xffff
;;; #xffff otherwise get another thing from x and 1- xidx and do as
;;; %floor of xthing otherx ything
;;; Huh?
(defppclapfunction %floor-99 ((x-stk 0)(xidx arg_x)(yptr arg_y)(yidx arg_z))
  (let ((xptr temp0)
        (a imm1)
        (b imm2)
        (y imm3)
        (quo imm0)) 
    (vpop xptr)
    (la imm4 ppc32::misc-data-offset XIDX)
    (lwzx a xptr imm4)
    (la imm4 ppc32::misc-data-offset YIDX)
    (lwzx y yptr imm4)
    (cmpw a y)
    (bne @more)
    (li imm4 #xffff)
    (rlwinm imm4 imm4 ppc32::fixnumshift (- 16 ppc32::fixnumshift) (- 31 ppc32::fixnum-shift))
    (vpush imm4)
    (vpush imm4)
    (la temp0 8 vsp)
    (set-nargs 2)
    (ba .spvalues)
    @MORE
    ;  a has 16 bits from ahi, bhi gets alo blo gets bhi
    (la imm4 (- ppc32::misc-data-offset 4) xidx)
    (lwzx b xptr imm4)
    (rlwinm b b 16 16 31)  ; bhi to blo 
    (rlwimi b a 16 0 15)   ; alo to bhi
    (rlwinm a a 16 16 31)  ; a gets alo 
    (48x32-divide a b y fp0 fp1 fp2 imm4)
    (fctiwz fp0 fp0)
    (stwu tsp -32 tsp)
    (stw tsp 4 tsp)
    (stfd fp0 24 tsp)
    (lwz quo (+ 24 4) tsp) ; 16 quo bits above stuff used by 48x32
    ; now mul quo by y
    (mullw imm4 y quo)
    ; and subtract from a,b
    (subfc b imm4 b)
    ; AND AGAIN
    (rlwinm a b -16 16 31) ; a gets b hi
    (rlwinm b b 16 0 15)   ; b lo to b hi
    (la imm4 (- ppc32::misc-data-offset 4) xidx) 
    (lwzx imm4 imm4 xptr)
    (rlwimi b imm4 0 16 31)
    (48x32-divide a b y fp0 fp1 fp2 imm4)
    (fctiwz fp0 fp0)
    (stfd fp0 16 tsp)  ; quo lo
    (lwz quo (+ 24 4) tsp) ; quo-hi
    (box-fixnum temp0 quo)
    (vpush temp0)
    (lwz quo (+ 16 4) tsp) ; quo lo
    (lwz tsp 0 tsp)
    (box-fixnum temp0 quo)
    (vpush temp0)    
    (la temp0 8 vsp)
    (set-nargs 2)
    (ba .SPvalues)))

; End of ppc32-bignum.lisp
