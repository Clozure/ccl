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

(defarmlapfunction %fixnum-signum ((number arg_z))
  (cmp number (:$ 0))
  (movlt arg_z '-1)
  (movgt arg_z '1)
  (bx lr))

(defarmlapfunction %ilogcount ((number arg_z))
  (let ((arg imm0)
        (shift imm1)
        (temp imm2))
    (unbox-fixnum arg number)
    (movs shift arg)
    (mov arg_z '0)
    (b @test)
    @next
    (sub  temp shift (:$ 1))
    (ands shift shift temp)
    (add arg_z arg_z '1)
    @test
    (bne @next)
    (bx lr)))

(defarmlapfunction %iash ((number arg_y) (count arg_z))
  (unbox-fixnum imm1 count)
  (unbox-fixnum imm0 number)
  (rsbs imm2 imm1 (:$ 0))
  (blt @left)
  (mov imm0 (:asr imm0 imm2))
  (box-fixnum arg_z imm0)
  (bx lr)
  @left
  (mov arg_z (:lsl number imm1))
  (bx lr))

(defparameter *double-float-zero* 0.0d0)
(defparameter *short-float-zero* 0.0s0)


(defarmlapfunction %sfloat-hwords ((sfloat arg_z))
  (ldr imm0 (:@ sfloat (:$ arm::single-float.value)))
  (digit-h temp0 imm0)
  (digit-l temp1 imm0)
  (vpush1 temp0)
  (vpush1 temp1)
  (add temp0 vsp '2)
  (set-nargs 2)
  (ba .SPvalues))


; (integer-length arg) = (- 32 (clz (if (>= arg 0) arg (lognot arg))))
(defarmlapfunction %fixnum-intlen ((number arg_z))  
  (unbox-fixnum imm0 arg_z)
  (clz imm1 imm0)
  (cmp imm1 (:$ 0))
  (bne @nonneg)
  (mvn imm1 imm0)
  (clz imm1 imm1)
  @nonneg
  (rsb imm1 imm1 (:$ 32))
  (box-fixnum arg_z imm1)
  (bx lr))






;;; Caller guarantees that result fits in a fixnum.
(defarmlapfunction %truncate-double-float->fixnum ((arg arg_z))
  (get-double-float d0 arg)
  (ftosizd s2 d0)
  (fmrs imm0 s2)
  (box-fixnum arg_z imm0)
  (bx lr))



(defarmlapfunction %truncate-short-float->fixnum ((arg arg_z))
  (get-single-float s0 arg imm0)
  (ftosizs s2 s0)
  (fmrs imm0 s2)
  (box-fixnum arg_z imm0)
  (bx lr))



;;; DOES round to even

(defarmlapfunction %round-nearest-double-float->fixnum ((arg arg_z))
  (get-double-float d0 arg)
  (ftosid s2 d0)
  (fmrs imm0 s2)
  (box-fixnum arg_z imm0)
  (bx lr))



(defarmlapfunction %round-nearest-short-float->fixnum ((arg arg_z))
  (get-single-float s0 arg imm0)
  (ftosis s2 s0)
  (fmrs imm0 s2)
  (box-fixnum arg_z imm0)
  (bx lr))






;;; maybe this could be smarter but frankly scarlett I dont give a damn
;;; ticket:666 describes one reason to give a damn.
(defarmlapfunction %fixnum-truncate ((dividend arg_y) (divisor arg_z))
  (let ((unboxed-quotient imm0)
        (unboxed-dividend imm0)
        (unboxed-divisor imm1)
        (unboxed-remainder imm1)
        (quotient arg_y)
        (remainder arg_z))
    (build-lisp-frame)
    (mov fn nfn)
    (cmp divisor '-1)    
    (unbox-fixnum unboxed-dividend dividend)
    (unbox-fixnum unboxed-divisor divisor)
    (beq @neg)
    (bla .SPsdiv32)
    (box-fixnum quotient unboxed-quotient)
    (box-fixnum remainder unboxed-remainder)
    (stmdb (:! vsp) (quotient remainder))
    (set-nargs 2)
    (ba .SPnvalret)
    @neg
    (ldr arg_z (:@ fn '*least-positive-bignum*))
    (rsbs dividend dividend (:$ 0))
    (ldrvs dividend (:@ arg_z (:$ arm::symbol.vcell)))
    @ret
    (mov temp0 (:$ 0))
    (vpush1 dividend)
    (vpush1 temp0)
    (set-nargs 2)
    (ba .SPnvalret)))




(defarmlapfunction called-for-mv-p ()
  (ref-global imm0 ret1valaddr)
  (ldr imm1 (:@ sp (:$ arm::lisp-frame.savelr)))
  (cmp imm1 imm0)
  (mov arg_z 'nil)
  (add arg_z arg_z (:$ arm::t-offset))
  (bx lr))

;;; n1 and n2 must be positive (esp non zero)
;;; See <http://en.wikipedia.org/wiki/Binary_GCD_algorithm>
(defarmlapfunction %fixnum-gcd ((n1 arg_y)(n2 arg_z))
  (mov arg_x rcontext)                  ;need an extra imm reg
  (unbox-fixnum imm0 n1)
  (unbox-fixnum imm1 n2)
  (subs r3 imm0 imm0)                   ; zero power-of-2 counter, set c flag
  (orrs imm2 imm0 imm1)                 ; preserves carry, set other flags
  @remove-twos-loop
  (movsne imm2 (:lsr imm2 (:$ 1)))      ; carry = lsbit
  (addcc r3 r3 (:$ 1))                  ; increment counter if lsbit 0
  (bcc @remove-twos-loop)
  (movs imm0 (:lsr imm0 r3))
  (movsne imm1 (:lsr imm1 r3))
  (beq @finish)
  @check-two-r0
  (movs imm0 (:lsr imm0 (:$ 1)))
  (bcc @check-two-r0)
  @check-two-r1
  (movs imm1 (:lsr imm1 (:$ 1)))
  (bcc @check-two-r1)
  (subs imm1 imm1 imm0)
  (addcc imm0 imm0 imm1)
  (rsbcc imm1 imm1 (:$ 0))
  (bne @check-two-r1)
  (adc imm0 imm0 imm0)
  @finish
  (orr imm0 imm1 (:lsl imm0 r3))
  (mov rcontext arg_x)
  (box-fixnum arg_z imm0)
  (bx lr))



(defarmlapfunction %mrg31k3p ((state arg_z))
  (let ((seed temp0)
	(m1 #x7fffffff))
    (svref seed 1 state)
    (u32-ref imm0 1 seed)

    (mov imm1 (:lsr imm0 (:$ 9)))
    (mov imm2 (:lsl imm0 (:$ 23)))	;get low 9 bits
    (mov imm2 (:lsr imm2 (:$ 23)))
    (add imm1 imm1 (:lsl imm2 (:$ 22)))

    (u32-ref imm0 2 seed)
    (add imm1 imm1 (:lsr imm0 (:$ 24)))
    (bic imm2 imm0 (:$ #xff000000))
    (add imm1 imm1 (:lsl imm2 (:$ 7)))

    (cmp imm1 (:$ m1))
    (subhi imm1 imm1 (:$ m1))

    (add imm1 imm1 imm0)
    (cmp imm1 (:$ m1))
    (subhi imm1 imm1 (:$ m1))

    (u32-ref imm0 1 seed)
    (u32-set imm0 2 seed)
    (u32-ref imm0 0 seed)
    (u32-set imm0 1 seed)
    (u32-set imm1 0 seed)

    ;; second component
    (u32-ref imm0 3 seed)
    (mov imm1 (:$ 20992))
    (add imm1 imm1 (:$ 77))
    (mov imm2 (:lsr imm0 (:$ 16)))
    (mul imm2 imm1 imm2)
    (mov imm0 (:lsl imm0 (:$ 16)))
    (add imm0 imm2 (:lsr imm0 (:$ 1)))

    (lri imm2 2147462579)
    (cmp imm0 imm2)
    (subhi imm0 imm0 imm2)

    (vpush1 rcontext)
    (mov rcontext imm0)			;save t1

    (u32-ref imm0 5 seed)
    (mov imm2 (:lsr imm0 (:$ 16)))
    (mul imm2 imm1 imm2)		;21069 still in imm1
    (mov imm1 (:lsl imm0 (:$ 16)))
    (add imm1 imm2 (:lsr imm1 (:$ 1)))

    (lri imm2 2147462579)
    (cmp imm1 imm2)
    (subhi imm1 imm1 imm2)

    (add imm1 imm1 imm0)
    (cmp imm1 imm2)
    (subhi imm1 imm1 imm2)

    (add imm1 imm1 rcontext)		;add in t1 from back when
    (vpop1 rcontext)
    (cmp imm1 imm2)
    (subhi imm1 imm1 imm2)

    (u32-ref imm0 4 seed)
    (u32-set imm0 5 seed)
    (u32-ref imm0 3 seed)
    (u32-set imm0 4 seed)
    (u32-set imm1 3 seed)

    ;; combination
    (u32-ref imm0 0 seed)
    (sub imm2 imm0 imm1)
    (cmp imm0 imm1)
    (addls imm2 imm2 (:$ m1))
    (bic imm2 imm2 (:$ #xe0000000))	;avoid negative fixnums
    (box-fixnum arg_z imm2)
    (bx lr)))

; End of arm-numbers.lisp
