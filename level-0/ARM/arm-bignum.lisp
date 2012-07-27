;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates

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
  (require "ARM-ARCH")
  (require "ARM-LAPMACROS"))

;;; %BIGNUM-REF needs to access bignums as obviously as possible, and it needs
;;; to be able to return 32 bits somewhere no one looks for real objects.
;;;
;;; The easiest thing to do is to store the 32 raw bits in two fixnums
;;; and return multiple values.
(defarmlapfunction %bignum-ref ((bignum arg_y) (i arg_z))
  (vref32 imm0 bignum i imm1)
  (digit-h temp0 imm0)
  (digit-l temp1 imm0)
  (vpush1 temp0)
  (vpush1 temp1)
  (add temp0 vsp (:$ 8))                  
  (set-nargs 2)                         
  (spjump .SPvalues))


;;; Set the 0th element of DEST (a bignum or some other 32-bit ivector)
;;; to the Ith element of the bignum SRC.
(defarmlapfunction %ref-digit ((bignum arg_x) (i arg_y) (dest arg_z))
  (add imm1 i (:$ arm::misc-data-offset))
  (ldr imm0 (:@ bignum imm1))
  (str imm0 (:@ dest (:$ arm::misc-data-offset)))
  (bx lr))

;;; BIGNUM[I] := DIGIT[0]
(defarmlapfunction %set-digit ((bignum arg_x) (i arg_y) (digit arg_z))
  (add imm1 i (:$ arm::misc-data-offset))
  (ldr imm0 (:@ digit (:$ arm::misc-data-offset)))
  (str imm0 (:@ bignum imm1))
  (bx lr))





;;; Return the sign of bignum (0 or -1) as a fixnum
(defarmlapfunction %bignum-sign ((bignum arg_z))
  (vector-length imm0 bignum imm0)
  (add imm0 imm0 (:$ (- arm::misc-data-offset 4))) ; Reference last (most significant) digit
  (ldr imm0 (:@ bignum imm0))
  (mov imm0 (:asr imm0 (:$ 31)))        ;propagate sign bit
  (box-fixnum arg_z imm0)
  (bx lr))

;;; Count the sign bits in the most significant digit of bignum;
;;; return fixnum count.
(defarmlapfunction %bignum-sign-bits ((bignum arg_z))
  (vector-length imm0 bignum imm0)
  (add imm0 imm0 (:$ (- arm::misc-data-offset 4))) ; Reference last (most significant) digit
  (ldr imm0 (:@ bignum imm0))
  (cmp imm0 (:$ 0))
  (mvnlt imm0 imm0)
  (clz imm0 imm0)
  (box-fixnum arg_z imm0)
  (bx lr))

(defarmlapfunction %digit-0-or-plusp ((bignum arg_y) (idx arg_z))
  (add imm0 idx (:$ arm::misc-data-offset))
  (ldr imm0 (:@ bignum imm0))
  (mov arg_z 'nil)
  (cmp imm0 (:$ 0))
  (addge arg_z arg_z (:$ arm::t-offset))
  (bx lr))

;;; For oddp, evenp
(defarmlapfunction %bignum-oddp ((bignum arg_z))
  (ldr imm0 (:@ bignum (:$ arm::misc-data-offset)))
  (mov arg_z 'nil)
  (tst imm0 (:$ 1))
  (addne arg_z arg_z (:$ arm::t-offset))
  (bx lr))
  
(defarmlapfunction bignum-plusp ((bignum arg_z))
  (vector-length imm0 bignum imm0)
  (add imm0 imm0 (:$ (- arm::misc-data-offset 4))) ; Reference last (most significant) digit
  (ldr imm0 (:@ bignum imm0))
  (mov arg_z 'nil)
  (cmp imm0 (:$ 0))
  (addge arg_z arg_z (:$ arm::t-offset))
  (bx lr))

(defarmlapfunction %fixnum-to-bignum-set ((bignum arg_y) (fixnum arg_z))
  (unbox-fixnum imm0 fixnum)
  (str imm0 (:@ bignum  (:$ arm::misc-data-offset)))
  (bx lr))

(defarmlapfunction bignum-minusp ((bignum arg_z))
  (vector-length imm0 bignum imm0)
  (add imm0 imm0 (:$ (- arm::misc-data-offset 4))) ; Reference last (most significant) digit
  (ldr imm0 (:@ bignum imm0))
  (mov arg_z 'nil)
  (cmp imm0 (:$ 0))
  (addlt arg_z arg_z (:$ arm::t-offset))
  (bx lr))


;;; Add the digits A[I] and B[J], and the incoming carry C (a fixnum).
;;; Store the result in R[K], and return the outgoing carry.
;;; If I is NIL, A is a fixnum.  If J is NIL, B is a fixnum.

(defarmlapfunction %add-with-carry ((r 12) (k 8) (c 4) (a 0) (i arg_x) (b arg_y) (j arg_z))
  (cmp i 'nil)
  (ldr temp0 (:@ vsp (:$ a)))
  (moveq imm1 (:asr temp0 (:$ arm::fixnumshift)))
  (addne imm1 i (:$ arm::misc-data-offset))
  (ldrne imm1 (:@ temp0 imm1))
  (cmp j 'nil)
  (moveq imm2 (:asr b (:$ arm::fixnumshift)))
  (addne imm2 j (:$ arm::misc-data-offset))
  (ldrne imm2 (:@ b imm2))
  (ldr temp0 (:@ vsp (:$ c)))
  (unbox-fixnum imm0 temp0)
  (subs imm0 imm0 (:$ 1))
  (ldr temp1 (:@ vsp (:$ r)))
  (ldr temp0 (:@ vsp (:$ k)))
  (add vsp vsp (:$ 16))  
  (adcs imm0 imm1 imm2)
  (add imm2 temp0 (:$ arm::misc-data-offset))
  (str imm0 (:@ temp1 imm2))
  (mov imm0 (:$ 0))
  (adc imm0 imm0 (:$ 0))
  (box-fixnum arg_z imm0)
  (bx lr))

; this is silly 
(defarmlapfunction %add-the-carry ((b-h arg_x) (b-l arg_y) (carry-in arg_z))
  (let ((a imm0)
        (b imm1)
        (c imm2))    
    (compose-digit b b-h b-l)
    (unbox-fixnum c carry-in)
    (add b c b)
    (digit-h temp0 b)
    (digit-l temp1 b)
    (vpush1 temp0)
    (vpush1 temp1)
    (add temp0 vsp '2)
    (set-nargs 2)
    (spjump .SPvalues)))




    
;;; Store the result of A[I] - B[J] - borrow into R[K], returning the borrow.
;;; If I is NIL, A is a fixnum; likewise for J and B.
(defarmlapfunction %subtract-with-borrow ((r 12) (k 8) (borrow 4) (a 0) (i arg_x) (b arg_y) (j arg_z))
  (cmp i 'nil)
  (ldr temp0 (:@ vsp (:$ a)))
  (moveq imm1 (:asr temp0 (:$ arm::fixnumshift)))
  (addne imm1 i (:$ arm::misc-data-offset))
  (ldrne imm1 (:@ temp0 imm1))
  (cmp j 'nil)
  (moveq imm2 (:asr b (:$ arm::fixnumshift)))
  (addne imm2 j (:$ arm::misc-data-offset))
  (ldrne imm2 (:@ b imm2))
  (ldr temp0 (:@ vsp (:$ borrow)))
  
  (unbox-fixnum imm0 temp0)
  (subs imm0 imm0 (:$ 1))
  (ldr temp0 (:@ vsp (:$ r)))
  (ldr temp1 (:@ vsp (:$ k)))
  (add vsp vsp (:$ 16))  
  (sbcs imm0 imm1 imm2)
  (add imm1 temp1 (:$ arm::misc-data-offset))
  (str imm0 (:@ temp0 imm1))
  (mov imm0 (:$ 0))
  (adc imm0 imm0 (:$ 0))
  (box-fixnum arg_z imm0)
  (bx lr))

;; multiply i'th digit of x by y and add to result starting at digit i

(defarmlapfunction %multiply-and-add-harder-loop-2
    ((x-ptr 4) (y-ptr 0) (resptr arg_x)(residx arg_y) (count arg_z))  
  (let ((x imm0)
        (y imm1)
        (prod-h imm2)
        (prod-l rcontext)
        (xptr temp2)
        (yidx temp1)
        (yptr temp0)
        (xsave 4))
    (mov imm1 (:$ 0))
    (mov imm0 (:$ (ash 1 arm::num-subtag-bits)))
    (orr imm0 imm0 (:$ arm::subtag-u32-vector))
    (stmdb (:! sp) (imm0 imm1))
    (ldr xptr (:@ vsp (:$ x-ptr)))
    (mov residx (:lsl residx (:$ 2)))
    (add residx residx (:$ (ash arm::misc-data-offset 2)))
    (ldr x (:@ xptr (:asr residx (:$ 2))))    
    (ldr yptr (:@ vsp (:$ y-ptr)))
    (vpush1 rcontext)
    (str x (:@ sp (:$ xsave)))
    (mov yidx (:$ (ash arm::misc-data-offset 2))) ; init yidx 0 
    (movs prod-h (:$ 0)) ; init carry 0, mumble 0
    @loop
    (ldr y (:@ yptr (:asr yidx (:$ 2))))
    (mul prod-l x y)
    (adds prod-l prod-l prod-h)
    (umull x prod-h x y)
    (adc prod-h prod-h (:$ 0))
    (ldr x (:@ sp (:$ xsave)))
    (ldr y (:@ resptr (:asr residx (:$ 2))))
    (adds prod-l prod-l y)
    (adc prod-h prod-h (:$ 0))
    (subs count count '1)
    (str prod-l (:@ resptr (:asr residx (:$ 2))))    
    (add residx residx '4)              ;sic
    (add yidx yidx '4)                  ;even sicer
    (bgt @loop)
    (str prod-h (:@ resptr (:asr residx (:$ 2))))
    (vpop1 rcontext)
    (add vsp vsp (:$ 8))
    (add sp sp (:$ 8))
    (bx lr)))



;;; Multiply X[I] by the unboxed value of the (non-negative) fixnum Y;
;;; add the incoming carry from CARRY[0] to the 64-bit product.  Store
;;; the low word of the 64-bit sum in R[0] and the high word in
;;; CARRY[0].

(defarmlapfunction %multiply-and-add ((r 4) (carry 0) (x arg_y) (i arg_x) (y arg_z))
  (unbox-fixnum imm0 arg_z)
  (add imm1 i (:$ arm::misc-data-offset))
  (ldr imm1 (:@ x imm1))
  (umull imm1 imm2 imm0 imm1)
  (ldr temp0 (:@ vsp (:$ carry)))
  (ldr imm0 (:@ temp0 (:$ arm::misc-data-offset)))
  (adds imm1 imm1 imm0)
  (adc imm2 imm2 (:$ 0))
  (str imm2 (:@ temp0  (:$ arm::misc-data-offset)))
  (ldr arg_z (:@ vsp (:$ r)))
  (add vsp vsp (:$ 8))    
  (str imm1 (:@ arg_z  (:$ arm::misc-data-offset)))
  (bx lr))
  


(defarmlapfunction %bignum-ref-hi ((bignum arg_y) (i arg_z))
  (add imm1 i (:$ (+ 2 arm::misc-data-offset)))
  (ldrh imm0 (:@ bignum imm1))
  (box-fixnum arg_z imm0)
  (bx lr))


(defarmlapfunction %bignum-set ((bignum 0) (i arg_x) (high arg_y) (low arg_z))
  (compose-digit imm0 high low)
  (ldr arg_z (:@ vsp (:$ bignum)))
  (vset32 imm0 arg_z i imm1)
  (add vsp vsp (:$ 4))
  (bx lr))




; this is silly
#+notyet
(defarmlapfunction %add-the-carry ((b-h arg_x) (b-l arg_y) (carry-in arg_z))
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
    (add temp0 vsp (:$ 8))
    (set-nargs 2)
    (spjump .SPvalues)))




;;; %SUBTRACT-WITH-BORROW -- Internal.
;;;
;;; This should be in assembler, and should not cons intermediate results.  It
;;; returns a 32bit digit and a borrow resulting from subtracting b from a, and
;;; subtracting a possible incoming borrow.
;;;
;;; We really do:  a - b - 1 + borrow, where borrow is either 0 or 1.
;;; 

(defarmlapfunction %subtract-with-borrow-1 ((a-h 4) (a-l 0) (b-h arg_x) (b-l
arg_y) (borrow-in arg_z))
  (let ((a imm0)
        (b imm1)
        (temp imm0)
        (c imm2))
    (ldr temp0 (:@ vsp (:$ a-h)))
    (ldr temp1 (:@ vsp (:$ a-l)))
    (compose-digit b b-h b-l)
    (unbox-fixnum c borrow-in)
    (adds temp c (:$ -1))
    (compose-digit a temp0 temp1)
    (sbcs a a b)
    (mov c (:$ 0))
    (adc c c c)
    (box-fixnum c c)
    (digit-h temp0 a)
    (digit-l temp1 a)
    (vpush1 temp0)
    (vpush1 temp1)
    (vpush1 c)
    (add temp0 vsp (:$ 20))
    (set-nargs 3)
    (spjump .SPvalues)))


(defarmlapfunction %subtract-one ((a-h arg_y)(a-l arg_z))
  (let ((a imm0))
    (compose-digit a a-h a-l)
    (sub a a (:$ 1))
    (digit-h temp0 a)
    (vpush1 temp0)
    (digit-l temp0 a)
    (vpush1 temp0)
    (add temp0 vsp (:$ 8))
    (set-nargs 2)
    (spjump .SPvalues)))




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



(defarmlapfunction %multiply-and-add-1 ((x-high 8)
					(x-low 4)
					(y-high 0)
					(y-low arg_x)
					(carry-in-high arg_y)
					(carry-in-low arg_z))
  (let ((x imm0)
	(y imm1)
	(carry-in imm2)
	(lo x)
	(hi y))
    (compose-digit carry-in carry-in-high carry-in-low)
    (vpop1 temp0)
    (compose-digit y temp0 y-low)
    (vpop1 temp0)
    (vpop1 temp1)
    (compose-digit x temp1 temp0)
    (umull lo hi x y)
    (adds lo lo carry-in)
    (adc hi hi (:$ 0))
    (digit-h temp0 hi)
    (digit-l temp1 hi)
    (digit-h temp2 lo)
    (digit-l arg_z lo)
    (vpush1 temp0)
    (vpush1 temp1)
    (vpush1 temp2)
    (vpush1 arg_z)
    (set-nargs 4)
    (add temp0 vsp (:$ 16))
    (spjump .SPvalues)))


(defarmlapfunction %logcount-complement ((bignum arg_y) (idx arg_z))
  (let ((arg imm0)
        (shift imm1)
        (temp imm2))
    (add arg idx (:$ arm::misc-data-offset))
    (ldr arg (:@ bignum arg))
    (mvns shift arg)
    (mov arg_z '0)
    (bxeq lr)
    @loop
    (add temp shift (:$ -1))
    (ands shift shift temp)
    (add arg_z arg_z '1)
    (bne @loop)
    (bx lr)))

(defarmlapfunction %logcount ((bignum arg_y) (idx arg_z))
  (let ((arg imm0)
        (shift imm1)
        (temp imm2))
    (add arg idx (:$ arm::misc-data-offset))
    (ldr arg (:@ bignum arg))
    (movs shift arg)
    (mov arg_z '0)
    (bxeq lr)
    @loop
    (add temp shift (:$ -1))
    (ands shift shift temp)
    (add arg_z arg_z '1)
    (bne @loop)
    (bx lr)))

; return res
#+notyet
(defarmlapfunction bignum-add-loop-2 ((aptr arg_x)(bptr arg_y) (result arg_z))
  (let ((idx imm0)
        (count imm1)
        (x imm2)
        (y imm3)        
        (len-a temp0)
        (len-b temp1)
        (tem temp2))
    (li idx arm::misc-data-offset)    
    (ldr imm4 aptr (:$ arm::misc-header-offset))
    (header-length len-a imm4)
    (ldr imm4 bptr (:$ arm::misc-header-offset))
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
    (ldr y (:@ aptr idx))
    (ldr x (:@ bptr idx))    
    (addi count count '1)
    (cmpw count len-a)
    (adde x x y)
    (str x (:@ result idx))
    (addi idx idx '1)
    (blt @loop)
    ; now propagate carry thru longer (b) using sign of shorter    
    ;(SUBI imm4 idx '1) ; y has hi order word of a
    ;(ldr y (:@ aptr imm4))
    (cmpw len-a len-b)
    (adde imm4 rzero rzero) ; get carry
    (srawi y y 31)  ; p.o.s clobbers carry 
    (addic imm4 imm4 -1)  ; restore carry
    (beq @l3)  ; unless equal
    @loop2
    (ldr x (:@ bptr idx))
    (adde x x y)
    (str x (:@ result idx))
    (addi count count '1)
    (cmpw count len-b)
    (addi idx idx '1)
    (blt @loop2)
    ; y has sign of shorter - get sign of longer to x
    @l3
    (subi imm4 idx '1)
    (ldr x (:@ bptr imm4))
    (adde imm4 rzero rzero) ; get carry
    (srawi x x 31)  ; clobbers carry 
    (addic imm4 imm4 -1)
    (adde x x y)
    (str x (:@ result idx))
    (bx lr)))

;; same as above but with initial a index and finishes, and is actually called.
(defarmlapfunction bignum-add-loop-+ ((init-a 0)(aptr arg_x)(bptr arg_y)(length arg_z))
  (let ((count temp0)
        (carry temp1)
        (x imm0)
        (y imm1)
        (aidx imm2)
        (idx y))
    (ldr aidx (:@ vsp (:$ init-a)))
    (add aidx aidx (:$ arm::misc-data-offset))
    (mov count (:$ 0))
    ; initialize carry 0
    (mov carry (:$ 0))
    @loop
    (ldr x (:@ aptr aidx))
    (add idx count (:$ arm::misc-data-offset))
    (ldr y (:@ bptr idx))
    (add x x y)
    (adds x x (:asr carry (:$ arm::fixnumshift)))
    (movcc carry (:$ 0))
    (movcs carry (:$ arm::fixnumone))
    (str x (:@ aptr aidx))
    (add count count '1)
    (cmp count length)
    (add aidx aidx '1)
    (blt @loop)
    (ldr x (:@ aptr aidx))  ; add carry into next one
    (add x x (:asr carry (:$ arm::fixnumshift)))
    (str x (:@ aptr aidx))
    (add vsp vsp (:$ 4))
    (bx lr)))


#+notyet
(defarmlapfunction bignum-negate-loop-really ((big arg_x) (len arg_y) (result arg_z))
  (let ((idx imm0)
        (one imm1)
        (x imm2))
    (li idx arm::misc-data-offset)
    (li one '1)
    ; initialize carry 1
    (li x -1)
    (addic x x 1)
    @loop        
    ;(addi count count '1)    
    ;(cmpw count len)
    (subf. len one len)
    (ldr x (:@ big idx))
    (not x x)
    (adde x x rzero)
    (str x (:@ result idx))    
    (addi idx idx '1)
    (bgt @loop)
    ; return carry
    (li x 0)
    (adde x x  rzero)
    (box-fixnum arg_z x)
    (bx lr)))

#+notyet
(defarmlapfunction bignum-negate-to-pointer ((big arg_x) (len arg_y) (result arg_z))
  (let ((idx imm0)
        (one imm1)
        (x imm2)
        (oidx imm3)
        (ptr imm4))
    (li idx arm::misc-data-offset)
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
    (ldr x (:@ big idx))
    (not x x)
    (adde x x rzero)
    (str x (:@ ptr oidx))    
    (addi idx idx '1)
    (addi oidx oidx 4)
    (bgt @loop)
    ; return carry
    (li x 0)
    (adde x x  rzero)
    (box-fixnum arg_z x)
    (bx lr)))




#+notyet
(defarmlapfunction bignum-shift-right-loop-1 ((nbits 4)(result 0) (bignum arg_x) (len arg_y) (iidx arg_z))
  (let ((y imm0)
        (idx imm1)
        (bits imm2)
        (rbits imm3)
        (x imm4)
        (jidx temp0)
        (resptr temp1))
    (li jidx 0)
    (ldr bits vsp (:$ nbits))
    (ldr resptr vsp (:$ result))
    (unbox-fixnum bits bits)
    (cmpw jidx len)
    (subfic rbits bits 32)    
    (bge @done)
    @loop
    (addi idx iidx arm::misc-data-offset)
    (ldr x (:@ bignum idx))
    (srw x x bits)
    (addi idx idx '1)
    (ldr y (:@ bignum idx))
    (slw y y rbits)
    (or x x y)
    (addi idx jidx arm::misc-data-offset)
    (str x (:@ resptr idx))
    (addi jidx jidx '1)    
    (cmpw jidx len)
    (addi iidx iidx '1)
    (blt @loop)
    @done
    (addi idx iidx arm::misc-data-offset)
    (ldr x (:@ bignum idx))
    (sraw x x bits)
    (addi idx jidx arm::misc-data-offset)
    (str x (:@ resptr idx))
    (add vsp vsp (:$ 8))
    (bx lr)))


(defarmlapfunction %compare-digits ((a arg_x) (b arg_y) (idx arg_z))
  (add imm0 idx (:$ arm::misc-data-offset))
  (ldr imm1 (:@ a imm0))
  (ldr imm0 (:@ b imm0))
  (cmp imm1 imm0)
  (moveq arg_z '0)
  (movhi arg_z '1)
  (movlo arg_z '-1)
  (bx lr))


  
;; returns number of bits in digit-hi,digit-lo that are sign bits
;; 32 - digits-sign-bits is integer-length

(defarmlapfunction %digits-sign-bits ((hi arg_y) (lo arg_z))
  (compose-digit imm1 hi lo)
  (cmp imm1 (:$ 0))
  (mvnlt imm1 imm1)
  (clz imm1 imm1)
  (box-fixnum arg_z imm1)
  (bx lr))

(defarmlapfunction bignum-logtest-loop ((count arg_x) (b1 arg_y) (b2 arg_z))  
  (mov imm1 (:$ arm::misc-data-offset))
  @loop
  (ldr imm2 (:@ b1 imm1))
  (ldr imm0 (:@ b2 imm1))
  (ands imm2 imm0 imm2)  
  (add imm1 imm1 (:$ 4))
  (bne @true)
  (subs count count (:$ 4))
  (bgt  @loop)
  (mov arg_z (:$ arm::nil-value))
  (bx lr)
  @true
  (mov arg_z (:$ arm::nil-value))
  (add arg_z arg_z (:$ arm::t-offset))
  (bx lr))

;;; dest[idx] <- (lognot src[idx])
(defarmlapfunction %bignum-lognot ((idx arg_x) (src arg_y) (dest arg_z))
  (add imm1 idx (:$ arm::misc-data-offset))
  (ldr imm0 (:@ src imm1))
  (mvn imm0 imm0)
  (str imm0 (:@ dest imm1))
  (bx lr))

;;; dest[idx] <- (logand x[idx] y[idx])
(defarmlapfunction %bignum-logand ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop1 temp0)
  (add imm1 temp0 (:$ arm::misc-data-offset))
  (ldr imm0 (:@ x imm1))
  (ldr imm2 (:@ y imm1))
  (and imm0 imm0 imm2)
  (str imm0 (:@ dest imm1))
  (bx lr))

;;; dest[idx] <- (logandc2 x[idx] y[idx])
(defarmlapfunction %bignum-logandc2 ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop1 temp0)
  (add imm1 temp0 (:$ arm::misc-data-offset))
  (ldr imm0 (:@ x imm1))
  (ldr imm2 (:@ y imm1))
  (bic imm0 imm0 imm2)
  (str imm0 (:@ dest imm1))
  (bx lr))

;;; dest[idx] <- (logandc1 x[idx] y[idx])
(defarmlapfunction %bignum-logandc1 ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop1 temp0)
  (add imm1 temp0 (:$ arm::misc-data-offset))
  (ldr imm0 (:@ x imm1))
  (ldr imm2 (:@ y imm1))
  (bic imm0 imm2 imm0)
  (str imm0 (:@ dest imm1))
  (bx lr))



(defarmlapfunction digit-lognot-move ((index arg_x) (source arg_y) (dest arg_z))
  (let ((scaled-index imm1))
    (vref32 imm0 source index scaled-index) ; imm1 has c(index) + data-offset
    (mvn imm0 imm0)
    (str imm0 (:@ dest scaled-index))
    (bx lr)))

(defarmlapfunction macptr->fixnum ((ptr arg_z))
  (macptr-ptr arg_z ptr)
  (bx lr))

; if dest not nil store unboxed result in dest(0), else return boxed result
(defarmlapfunction fix-digit-logandc2 ((fix arg_x) (big arg_y) (dest arg_z)) ; index 0
  (let ((w1 imm0)
        (w2 imm1))
    (unbox-fixnum  w1 fix)
    (ldr w2 (:@ big (:$ arm::misc-data-offset)))
    (cmp dest 'nil)
    (bic w1 w1 w2)
    (bne @store)
    (box-fixnum arg_z w1)
    (bx lr)
    @store
    (str w1 (:@ dest  (:$ arm::misc-data-offset)))
    (bx lr)))



(defarmlapfunction fix-digit-logand ((fix arg_x) (big arg_y) (dest arg_z)) ; index 0
  (let ((w1 imm0)
        (w2 imm1))
    (unbox-fixnum  w1 fix)
    (ldr w2 (:@ big (:$ arm::misc-data-offset)))
    (cmp dest 'nil)
    (and w1 w1 w2)
    (bne @store)
    (box-fixnum arg_z w1)
    (bx lr)
    @store
    (str w1 (:@ dest  (:$ arm::misc-data-offset)))
    (bx lr)))



(defarmlapfunction fix-digit-logandc1 ((fix arg_x) (big arg_y) (dest arg_z)) ; index 0
  (let ((w1 imm0)
        (w2 imm1))
    (unbox-fixnum  w1 fix)
    (ldr w2 (:@ big (:$ arm::misc-data-offset)))
    (cmp dest 'nil)
    (bic w1 w2 w1)
    (bne @store)
    (box-fixnum arg_z w1)
    (bx lr)
    @store
    (str w1 (:@ dest  (:$ arm::misc-data-offset)))
    (bx lr)))

;;; dest[idx] <- (logior x[idx] y[idx])
(defarmlapfunction %bignum-logior ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop1 temp0)
  (add imm1 temp0 (:$ arm::misc-data-offset))
  (ldr imm0 (:@ x imm1))
  (ldr imm2 (:@ y imm1))
  (orr imm0 imm0 imm2)
  (str imm0 (:@ dest imm1))
  (bx lr))

;;; dest[idx] <- (logxor x[idx] y[idx])
(defarmlapfunction %bignum-logxor ((idx 0) (x arg_x) (y arg_y) (dest arg_z))
  (vpop1 temp0)
  (add imm1 temp0 (:$ arm::misc-data-offset))
  (ldr imm0 (:@ x imm1))
  (ldr imm2 (:@ y imm1))
  (eor imm0 imm0 imm2)
  (str imm0 (:@ dest imm1))
  (bx lr))



(defarmlapfunction bignum-xor-loop ((count 0) (b1 arg_x) (b2 arg_y) (dest arg_z))
  (ldr temp0 (:@ vsp (:$ count)))
  (mov imm1 (:$ arm::misc-data-offset))
  @loop
  (ldr imm2 (:@ b1 imm1))
  (ldr imm0 (:@ b2 imm1))
  (eor imm2 imm0 imm2)
  (subs temp0 temp0 (:$ 4))
  (str imm2 (:@ dest imm1))
  (add imm1 imm1 (:$ 4))
  (bgt @loop)
  @out
  (add vsp vsp (:$ 4))
  (bx lr))

#+nomore
(defarmlapfunction try-guess-loop-1 ((guess-h 8)(guess-l 4)(len-y 0)
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
    (ldr x vsp (:$ guess-h))
    (ldr tem vsp (:$ guess-l))
    (compose-digit guess x tem)
    (ldr end-y vsp (:$ len-y))
    (li yidx 0)
    (li carry 0) 
    (li carry-bit '1)
    @loop
    ; multiply guess by ydigit, add carry to lo, hi is new carry
    ; then get an xdigit subtract prod-lo from it and store result in x (remember carry)
    (addi tem yidx arm::misc-data-offset)   ; get yidx
    (ldr y (:@ yptr tem))
    (mullw prod-l guess y)
    (mulhwu prod-h guess y)    
    (addc prod-l prod-l carry) 
    (adde carry prod-h rzero)
    ; get back saved carry
    (li tem '-1)
    (addc tem carry-bit tem)
    (addi tem xidx arm::misc-data-offset)
    (ldr x (:@ xptr tem))    
    (subfe x prod-l x)        
    (str x (:@ xptr tem))
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
    (addi tem xidx arm::misc-data-offset) ; maybe still there - nope
    (ldr x (:@ xptr tem))
    (subfe x carry x)
    (str x (:@ xptr tem))
    (add vsp vsp (:$ 12))
    (bx lr)))

;; x0 is at index, x1 at index-1, x2 at index-2
;; y1 is at index, y2 at index-1
;; this doesnt help much
(defarmlapfunction truncate-guess-loop ((guess-h 8)(guess-l 4)(x 0)
                                        (xidx arg_x)(yptr arg_y) (yidx arg_z))
  (let ((guess imm0)
        (y1 imm1)
        (y2 imm1)
        (gy1-lo imm2) ; look out below
        (gy1-hi imm2)
        (gy2-lo imm2)
        (gy2-hi imm2)
        (xptr temp0)
        (m r3)
        (tem temp1)
        (save-rcontext temp2)
        (m-save 24)
        (y1-idx 20)
        (y2-idx 16)
        (x0-idx 12)
        (x1-idx 8)
        (x2-idx 4))
    (mov imm0 (:$ (ash 6 arm::num-subtag-bits)))
    (orr imm0 imm0 (:$ arm::subtag-u32-vector))
    (str imm0 (:@! sp (:$ -32)))
    (mov save-rcontext rcontext)
    (ldr y1 (:@ vsp (:$ guess-h)))
    (ldr m (:@ vsp (:$ guess-l)))
    (compose-digit guess y1 m)
    (mov tem (:lsl yidx (:$ arm::fixnumshift)))
    (add tem tem (:$ (ash arm::misc-data-offset arm::fixnumshift)))
    (ldr y1 (:@ yptr (:asr tem (:$ arm::fixnumshift))))
    (str y1 (:@ sp  (:$ y1-idx)))
    (sub tem tem '4)
    (ldr y2 (:@ yptr (:asr tem (:$ arm::fixnumshift))))
    (str y2 (:@ sp  (:$ y2-idx)))
    (ldr xptr (:@ vsp (:$ x)))
    (mov tem (:lsl xidx (:$ arm::fixnumshift)))
    (add tem tem (:$ (ash arm::misc-data-offset arm::fixnumshift)))
    (ldr y1 (:@ xptr (:asr tem (:$ arm::fixnumshift)))) ; its x0
    (str y1 (:@ sp  (:$ x0-idx)))
    (sub tem tem '4)
    (ldr y1 (:@ xptr (:asr tem (:$ arm::fixnumshift))))
    (str y1 (:@ sp  (:$ x1-idx)))
    (sub tem tem '4)
    (ldr y1 (:@ xptr (:asr tem (:$ arm::fixnumshift))))
    (str y1 (:@ sp  (:$ x2-idx)))
    @loop
    (ldr y1 (:@ sp (:$ y1-idx)))     ; get y1
    (mul gy1-lo guess y1)
    (ldr m (:@ sp (:$ x1-idx)))      ; get x1
    (subs m m gy1-lo)      ; x1 - gy1-lo => m
    (umull y2 gy1-hi guess y1)
    (ldr y2 (:@ sp (:$ x0-idx)))    ; get x0
    (rscs y2 gy1-hi y2)      ; - val not used just cr
    (ldr y2 (:@ sp (:$ y2-idx)))     ; get y2
    (str m (:@ sp (:$ m-save)))
    (umull m gy2-hi guess y2)   ; does it pay to do this now even tho may not need?
    (ldr m (:@ sp (:$ m-save)))
    (bne @done)
    (cmp gy2-hi m)       ; if > or = and foo then more - L means logical means unsigned
    (blo @done)           ; if < done
    (bne @more)           ; if = test lo
    (mul gy2-lo guess y2)
    (ldr y1 (:@ sp (:$ x2-idx))) ; get x2
    (cmp gy2-lo y1)
    (bls @done)
    @more
    (sub guess guess (:$ 1))
    (b @loop)
    @done
    (mov rcontext save-rcontext)
    (digit-h temp0 guess)
    (vpush1 temp0)
    (digit-l temp0 guess)
    (vpush1 temp0)
    (add temp0 vsp (:$ 20))
    (add sp sp (:$ 32))
    (set-nargs 2)
    (spjump .SPvalues)))

(defarmlapfunction normalize-bignum-loop ((sign arg_x)(res arg_y)(len arg_z))
  (let ((idx imm0)
        (usign imm1)
        (val imm2))      
    (unbox-fixnum usign sign)
    (cmp len '0)
    (add idx len (:$ (- arm::misc-data-offset 4))  )
    (bxeq lr) ; huh - can this ever happen?
    @loop
    (ldr val (:@ res idx))
    (cmp  val usign)    
    (sub idx idx '1)
    (bne @neq)    
    (subs len len '1)
    (bgt @loop)
    ; fall through - its all sign - return 1
    (mov arg_z '1)
    (bx lr)
    @neq
    (and usign usign (:$ #x80000000))
    (and val val (:$ #x80000000))
    (cmp usign val)  ; is hi bit = sign, if so then done   
    (addne len len '1) ; if not, need 1 more
    (bx lr)))

(defarmlapfunction %normalize-bignum-2 ((fixp arg_y)(res arg_z))
  (let ((idx imm0)
        (usign imm1)
        (val imm2)
        (len arg_x)
        (oldlen temp0))
    (vector-length len res imm0)
    (cmp len (:$ 0))
    (mov oldlen len)
    (add idx len (:$ (- arm::misc-data-offset 4))  )
    (bxeq lr) ; huh - can this ever happen?
    (ldr val (:@ res idx)) ; high order word
    (mov usign (:asr val (:$ 31))) ; get sign
    @loop
    (ldr val (:@ res idx))
    (cmp  val usign)    
    (sub idx idx '1)
    (bne @neq)    
    (subs len len '1)
    (bgt @loop)
    ; fall through - its all sign - return 1
    (mov len '1)
    (and usign usign (:$ #x80000000))
    (b @more)
    @neq
    (and usign usign (:$ #x80000000))
    (and val val (:$ #x80000000))
    (cmp usign val)  ; is hi bit = sign, if so then done   
    (beq @more)
    (add len len '1) ; if not, need 1 more
    (b @big)
    @more
    (cmp  fixp 'nil)
    (beq @big)                          ; dont return fixnum
    (cmp len '1)
    (bgt @big)
    ;; stuff for maybe fixnum
    ;(dbg t)
    (ldr val (:@ res (:$ arm::misc-data-offset)))
    (box-fixnum temp1 val)
    (cmp val (:asr temp1 (:$ arm::fixnumshift)))
    (moveq arg_z temp1)
    (bxeq lr)
    @big
    (cmp oldlen len)
    (bxeq lr) ; same length - done
    (mov imm2 (:$ arm::subtag-bignum))
    (cmp usign (:$ 0))
    (orr imm2 imm2 (:lsl len (:$ (- arm::num-subtag-bits arm::fixnumshift))))
    ;; 0 to tail if negative
    (beq @set-header) 
    ;; zero from len inclusive to oldlen exclusive
    (add idx len (:$ arm::misc-data-offset))
    @loop2
    (tst len (:$ arm::fixnumone))
    (add len len '1)
    (movne imm1 (:$ (logand #xff00 arm::one-digit-bignum-header)))
    (orrne imm1 imm1 (:$ (logand #xff arm::one-digit-bignum-header)))
    (moveq imm1 (:$ #x80000000))
    (cmp len oldlen)
    (str imm1 (:@ idx res))
    (add idx idx '1)
    (blt @loop2)
    @set-header
    (str imm2 (:@ res (:$ arm::misc-header-offset)))
    (bx lr)))

(defarmlapfunction %count-digit-leading-zeros ((high arg_y) (low arg_z))
  (compose-digit imm0 high low)
  (clz imm0 imm0)
  (box-fixnum arg_z imm0)
  (bx lr))

(defarmlapfunction %count-digit-trailing-zeros ((high arg_y) (low arg_z))
  (compose-digit imm0 high low)
  (rsb  imm1 imm0 (:$ 0))
  (and imm0 imm0 imm1)
  (clz imm0 imm0)
  (rsb imm0 imm0 (:$ 31))
  (box-fixnum arg_z imm0)
  (bx lr))


(defarmlapfunction %bignum-count-trailing-zero-bits ((bignum arg_z))
  (let ((ndigits arg_x)
	(nbits arg_y)
	(digit imm0)
	(ptr imm1))
    (mov ptr (:$ arm::misc-data-offset))
    (mov ndigits '-32)
    @next
    (ldr digit (:@ bignum ptr))
    (cmp digit (:$ 0))
    (add ptr ptr (:$ 4))
    (add ndigits ndigits '32)
    (beq @next)
    (rsb ptr digit (:$ 0))
    (and digit digit ptr)
    (clz digit digit)
    (rsb digit digit (:$ 31))
    (box-fixnum nbits digit)
    (add arg_z nbits ndigits)
    (bx lr)))


(defarmlapfunction %bignum-trim-leading-zeros ((bignum arg_x)
					       (start arg_y)
					       (len arg_z))
  (add imm1 start len)
  (add imm1 imm1 (:$ (- arm::misc-data-offset 4)))
  @loop
  (ldr imm0 (:@ bignum imm1))
  (cmp imm0 (:$ 0))
  (add imm1 imm1 (:$ -4))
  (bxne lr)
  (subs len len '-1)
  (bne @loop)
  (bx lr))
  
;;; Set length of bignum to new-len (zeroing out any trailing words between
;;; the old length and the new.
(defarmlapfunction %shrink-bignum ((new-len arg_y) (bignum arg_z))
  (let ((old-len temp0)
        (rzero temp1)
	(old-idx imm0)
	(new-idx imm2)
	(header imm1))
    (getvheader header bignum)
    (header-length old-len header)
    (mov rzero (:$ 0))
    (cmp old-len new-len)
    (add old-idx old-len (:$ arm::misc-data-offset))
    (add new-idx new-len (:$ arm::misc-data-offset))
    (bxeq lr)
    @loop
    (sub old-idx old-idx (:$ 4))
    (cmp old-idx new-idx)
    (str rzero (:@ bignum old-idx))
    (bne @loop)
    (mov header (:lsl new-len (:$ (- arm::num-subtag-bits arm::fixnumshift))))
    (orr header header (:$ arm::subtag-bignum))
    (str header (:@ bignum  (:$ arm::misc-header-offset)))
    (bx lr)))

;;; Divide bignum x by single digit y (passed as two halves).
;;; The quotient in stored in q, and the remainder is returned
;;; in two halves.  (cf. Knuth, 4.3.1, exercise 16)
(defarmlapfunction %floor-loop-quo ((x 0) (res arg_x) (yhi arg_y) (ylo arg_z))
  (let ((bignum temp0)
        (len temp2))                    ;not nfn here.
    (ldr bignum (:@ vsp (:$ x)))
    (add imm1 vsp (:$ arm::node-size))
    (build-lisp-frame imm0 imm1)
    (vector-length len bignum imm0)
    (mov imm2 (:$ 0))
    (b @next)
    @loop
    (add imm0 len (:$ arm::misc-data-offset))
    (ldr imm0 (:@ bignum imm0))
    (mov imm1 imm2)
    (compose-digit imm2 yhi ylo)
    (sploadlr .SPudiv64by32)
    (blx lr)
    (add imm1 len (:$ arm::misc-data-offset))
    (str imm0 (:@ res imm1))
    @next
    (subs len len '1)
    (bge @loop)
    (digit-h yhi imm2)
    (digit-l ylo imm2)
    (vpush1 yhi)
    (vpush1 ylo)
    (set-nargs 2)
    (spjump .SPnvalret)))

;;; For TRUNCATE-BY-FIXNUM et al.
;;; Doesn't store quotient: just returns rem in 2 halves.
(defarmlapfunction %floor-loop-no-quo ((x arg_x) (yhi arg_y) (ylo arg_z))
  (let ((len temp1))
    (build-lisp-frame)
    (vector-length len x imm0)
    (mov imm2 (:$ 0))
    (b @next)
    @loop
    (add imm0 len (:$ arm::misc-data-offset))
    (ldr imm0 (:@ x imm0))
    (mov imm1 imm2)
    (compose-digit imm2 yhi ylo)
    (sploadlr .SPudiv64by32)
    (blx lr)
    @next
    (subs len len '1)
    (bge @loop)
    (digit-h yhi imm2)
    (digit-l ylo imm2)
    (vpush1 yhi)
    (vpush1 ylo)
    (set-nargs 2)
    (spjump .SPnvalret)))
    
    

    
    
(defarmlapfunction bignum-negate-loop-really ((big arg_x) (len arg_y) (result arg_z))
  (let ((idx imm0)
        (x imm1)
        (carry imm2))
    (mov idx (:$ arm::misc-data-offset))
    ;; initialize carry 1
    (mov carry (:$ 1))
    @loop        
    (ldr x (:@ big idx))
    (mvn x x)
    (adds x x carry)
    (str x (:@ result idx))
    (movcc carry (:$ 0))
    (movcs carry (:$ 1))
    (subs len len '1)
    (add idx idx '1)
    (bgt @loop)
    ; return carry
    (box-fixnum arg_z carry)
    (bx lr)))

(defarmlapfunction bignum-shift-left-loop ((nbits 4)(result 0) (bignum arg_x) (len arg_y) (j arg_z))
  (let ((y imm0)
        (x imm1)
        (shift imm2)
        (rcontext-save temp1)
        (rshift r3)
        (i temp0)
        (resptr temp2))
    (vpop1 resptr)
    (mov i (:$ (ash arm::misc-data-offset 2)))
    (vpop1 shift)
    (mov rcontext-save rcontext)
    (ldr x (:@ bignum (:$ arm::misc-data-offset)))
    (unbox-fixnum shift shift)
    (rsb rshift shift (:$ 32))
    (mov x (:lsl x shift))
    (add y j (:$ (+ arm::misc-data-offset -4)))
    (str x (:@ resptr y))
    (cmp len j)
    (beq @done)
    @loop
    (ldr x (:@ bignum (:asr i (:$ 2))))
    (mov x (:lsr x rshift))
    (add i i '4)                        ;sic
    (ldr y (:@ bignum (:asr i (:$ 2))))
    (orr y x (:lsl y shift))
    (add x j (:$ arm::misc-data-offset))
    (str y (:@ resptr x))
    (add j j '1)    
    (cmp j len)
    (blt @loop)    
    @done
    (ldr y (:@ bignum (:asr i (:$ 2))))
    (mov y (:asr y rshift))
    (add x len (:$ arm::misc-data-offset))
    (str y (:@ resptr x))
    (mov rcontext rcontext-save)
    (bx lr)))

(defarmlapfunction bignum-shift-right-loop-1 ((nbits 4)(result 0) (bignum arg_x) (len arg_y) (iidx arg_z))
  (let ((y imm0)
        (x imm1)
        (shift imm2)
        (idx imm2)
        (jidx temp0)
        (resptr temp1)
        (boxed-shift temp2))
    (vpop1 resptr)
    (vpop1 boxed-shift)
    (mov jidx '0)
    (cmp jidx len)
    (bge @done)
    @loop
    (add idx iidx (:$ arm::misc-data-offset))
    (ldr x (:@ bignum idx))
    (unbox-fixnum shift boxed-shift)
    (mov x (:lsr x shift))
    (add idx iidx (:$ (+ arm::misc-data-offset 4)))
    (ldr y (:@ bignum idx))
    (unbox-fixnum shift boxed-shift)
    (rsb shift shift (:$ 32))
    (mov y (:lsl y shift))
    (orr x x y)
    (add idx jidx (:$ arm::misc-data-offset))
    (str x (:@ resptr idx))
    (add jidx jidx '1)
    (cmp jidx len)
    (add iidx iidx '1)
    (blt @loop)
    @done
    (add idx iidx (:$ arm::misc-data-offset))
    (ldr x (:@ bignum idx))
    (unbox-fixnum shift boxed-shift)
    (mov x (:asr x shift))
    (add idx jidx (:$ arm::misc-data-offset))
    (str x (:@ resptr idx))
    (bx lr)))

;;; If x[i] = y[j], return the all ones digit (as two halves).
;;; Otherwise, compute floor x[i]x[i-1] / y[j].
(defarmlapfunction %floor-99 ((x-stk 0) (xidx arg_x) (yptr arg_y) (yidx arg_z))
  (add imm1 vsp (:$ 4))
  (build-lisp-frame imm0 imm1)
  (mov fn nfn)
  (ldr temp0 (:@ vsp (:$ x-stk)))
  (add imm0 xidx (:$ arm::misc-data-offset))
  (add imm1 yidx (:$ arm::misc-data-offset))
  (ldr imm0 (:@ temp0 imm0))
  (ldr imm2 (:@ yptr imm1))
  (cmp imm0 imm2)
  (bne @more)
  (mov imm0 (:$ (ash #xff arm::fixnumshift)))
  (orr imm0 imm0 (:$ (ash #xff00 arm::fixnumshift)))
  (vpush1 imm0)
  (vpush1 imm0)
  (set-nargs 2)
  (spjump .SPnvalret)
  @more
  (add imm1 xidx (:$ (- arm::misc-data-offset arm::node-size)))
  (ldr imm0 (:@ temp0 imm1))
  (add imm1 imm1 (:$ arm::node-size))
  (ldr imm1 (:@ temp0 imm1))
  (sploadlr .SPudiv64by32)
  (blx lr)
  (mov arg_y '-1)
  (and arg_y arg_y (:lsr imm0 (:$ (- 16 arm::fixnumshift))))
  (mov imm0 (:lsl imm0 (:$ 16)))
  (mov arg_z '-1)
  (and arg_z arg_z (:lsr imm0 (:$ (- 16 arm::fixnumshift))))
  (stmdb (:! vsp) (arg_z arg_y))
  (set-nargs 2)
  (spjump .SPnvalret))

;;; Karatsuba multiplication stuff. NYI.
;;; Copy the limb SRC points to to where DEST points.
(defarmlapfunction copy-limb ((src arg_y) (dest arg_z))
  (uuo-debug-trap (:? al)))

;;; Return T iff LIMB contains 0.
(defarmlapfunction limb-zerop ((limb arg_z))
  (uuo-debug-trap (:? al)))

;;; Return -1,0,1 according to whether the contents of Y are
;;; <,=,> the contents of Z.
(defarmlapfunction compare-limbs ((y arg_y) (z arg_z))
  (uuo-debug-trap (:? al)))

;;; Add a fixnum to the limb LIMB points to.  Ignore overflow.
(defarmlapfunction add-fixnum-to-limb ((fixnum arg_y) (limb arg_z))
  (uuo-debug-trap (:? al)))

;;; Store a fixnum value where LIMB points.
(defarmlapfunction copy-fixnum-to-limb ((fixnum arg_y) (limb arg_z))
  (uuo-debug-trap (:? al)))

;;; Increment a "LIMB VECTOR" (bignum) by a small amount.  The caller
;;; knows that carries will only propagate for a word or two.
(defarmlapfunction mpn-incr-u ((limb arg_y) (fixby arg_z))
  (uuo-debug-trap (:? al)))

;;; Store XP-YP at WP; return carry (0 or 1).
;;; wp, xp, yp: word-aligned, unboxed ptrs (fixnums)
;;; size: boxed fixnum
;;; returns boxed carry
(defarmlapfunction mpn-sub-n ((wp 8) (xp arg_x) (yp arg_y) (size arg_z))
  (uuo-debug-trap (:? al)))

;;; Store XP+YP at WP; return carry (0 or 1).
;;; wp, xp, yp = word-aligned, unboxed macptrs (fixnums).
;;; size = boxed fixnum
;;; result = boxed carry
(defarmlapfunction mpn-add-n ((wp 0) (xp arg_x)
				(yp arg_y) (size arg_z))
  (uuo-debug-trap (:? al)))

;;; Add the single limb LIMB to S1P (propagating carry.)  Store the
;;; result at RP.  RP and S1P may be the same place, so check for
;;; that and do nothing after carry stops propagating.  Return carry.
(defarmlapfunction mpn-add-1 ((rp-offset 0) (s1p arg_x) 
				(size arg_y) (limb arg_z))
  (uuo-debug-trap (:? al)))

;;; Multiply the limb vector S1 by the single limb at LIMBPTR, storing
;;; the result at RES.  Store the "carry out" (high word of last 64-bit
;;; partial product) at the limb RESULT.
;;; res, s1, limbptr, result:
;;;   unboxed, word-aligned ptrs (fixnums).  size: boxed fixnum
;;; It'd be hard to transliterate the GMP code here; the GMP version
;;; uses lots more immediate registers than we can easily use in LAP
;;; (and is much more aggressively pipelined).
(defarmlapfunction mpn-mul-1 ((res-offset 4)
				(s1-offset 0)
				(size arg_x)
				(limbptr arg_y)
				(result arg_z))
  (uuo-debug-trap (:? al)))

;;; multiply s1*limb and add result to res
;;; res, s1, limbptr, result:
;;;   unboxed, word-aligned ptrs (fixnums).
;;; size: boxed fixnum
;;; limbptr: source "limb".
;;; result: carry out (high word of product).
(defarmlapfunction mpn-addmul-1 ((res-offset 4)
				   (s1-offset 0)
				   (size arg_x)
				   (limbptr arg_y)
				   (result arg_z))
  (uuo-debug-trap (:? al)))

;;; Multiply the UN-word limb vector at UP and the VN-word limb vector
;;; at VP, store the result at RP.
(defarmlapfunction mpn-mul-basecase ((rp-offset 4)
				       (up-offset 0)
				       (un arg_x)
				       (vp arg_y)
				       (vn arg_z))
  (uuo-debug-trap (:? al)))

;;; left-shift src by 1 bit, storing result at res.  Return
;;; the bit that was shifted out.
(defarmlapfunction mpn-lshift-1 ((resptr arg_x) (s1ptr arg_y) (size-arg arg_z))
  (uuo-debug-trap (:? al)))

;;; Do a 32x32=64 unsigned multiply of the words at X and Y.  Store
;;; result (low word first) at RESULT.
(defarmlapfunction umulppm ((x arg_x) (y arg_y) (result arg_z))
  (uuo-debug-trap (:? al)))


; End of arm-bignum.lisp
