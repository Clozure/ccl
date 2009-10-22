;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2006-2009 Clozure Associates
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
(defx86lapfunction %fixnum-to-bignum-set ((bignum arg_y) (fixnum arg_z))
  (movq (% fixnum) (% arg_x))
  (shl ($ (- 32 x8664::fixnumshift)) (% arg_x))
  (sar ($ (- 32 x8664::fixnumshift)) (% arg_x))
  (unbox-fixnum fixnum imm0)
  (cmp (% arg_x) (% fixnum))
  (je @chop)
  (movq (% imm0)  (@ x8664::misc-data-offset (% bignum)))
  (single-value-return)
  @chop
  (movq ($ x8664::one-digit-bignum-header) (@ x8664::misc-header-offset (% bignum)))
  (movl (% imm0.l) (@ x8664::misc-data-offset (% bignum)))
  (single-value-return))
  
;; multiply x[i] by y and add to result starting at digit idx
(defx86lapfunction %multiply-and-add-loop
    ((x 16) (y 8) #|(ra 0)|# (r arg_x) (idx arg_y) (ylen arg_z))
  (let ((cc mm2)
	(xx mm3)
	(yy mm4)
	(rr mm5)
	(i imm0)
	(j imm1))
    (unbox-fixnum idx i)
    (movq (@ x (% rsp)) (% temp0))
    (movd (@ x8664::misc-data-offset (% temp0) (% i) 4) (% xx)) ;x[i]
    (movq (@ y (% rsp)) (% temp0))
    (movq (% r) (% temp1))
    (pxor (% cc) (% cc))
    (xorq (% j) (% j))
    @loop
    (movd (@ x8664::misc-data-offset (% temp0) (% j) 4) (% yy)) ;y[j]
    (pmuludq (% xx) (% yy))
    ;; 64-bit product now in %yy
    (movd (@ x8664::misc-data-offset (% temp1) (% i) 4) (% rr))
    ;; add in digit from r[i]
    (paddq (% yy) (% rr))
    ;; add in carry
    (paddq (% cc) (% rr))
    (movd (% rr) (@ x8664::misc-data-offset (% temp1) (% i) 4)) ;update r[i]
    (movq (% rr) (% cc))
    (psrlq ($ 32) (% cc))		;get carry digit into low word
    (addq ($ 1) (% i))
    (addq ($ 1) (% j))
    (subq ($ '1) (% ylen))
    (jg @loop)
    (movd (% cc) (@ x8664::misc-data-offset (% temp1) (% i) 4))
    (single-value-return 4)))

(defx86lapfunction %multiply-and-add-loop64
    ((xs 16) (ys 8) #|(ra 0)|# (r arg_x) (i arg_y) (ylen arg_z))
  (let ((y temp2)
	(j temp0)
	(c imm2))
    (movq (@ xs (% rsp)) (% temp0))
    (movq (@ x8664::misc-data-offset (% temp0) (% i)) (% mm0)) ;x[i]
    (movq (@ ys (% rsp)) (% y))
    (xorl (%l j) (%l j))
    (xorl (%l c) (%l c))
    @loop
    ;; It's a pity to have to reload this every time, but there's no
    ;; imm3.  (Give him 16 registers, and he still complains...)
    (movd (% mm0) (% rax))
    (mulq (@ x8664::misc-data-offset (% y) (% j))) ;128-bit x * y[j] in rdx:rax
    (addq (@ x8664::misc-data-offset (% r) (% i)) (% rax)) ;add in r[i]
    (adcq ($ 0) (% rdx))
    ;; add in carry digit
    (addq (% c) (% rax))
    (movl ($ 0) (%l c))
    (adcq (% rdx) (% c))				   ;new carry digit
    (movq (% rax) (@ x8664::misc-data-offset (% r) (% i))) ;update r[i]
    (addq ($ '1) (% i))
    (addq ($ '1) (% j))
    (subq ($ '1) (% ylen))
    (ja @loop)
    (movq (% c) (@ x8664::misc-data-offset (% r) (% i)))
    (single-value-return 4)))

;;; Multiply the (32-bit) digits X and Y, producing a 64-bit result.
;;; Add the 32-bit "prev" digit and the 32-bit carry-in digit to that 64-bit
;;; result; return the halves as (VALUES high low).
(defx86lapfunction %multiply-and-add4 ((x 8) #|(ra 0)|# (y arg_x) (prev arg_y) (carry-in arg_z))
  (let ((unboxed-x imm0)
        (unboxed-y imm1)
        (unboxed-prev imm0)
        (unboxed-carry-in imm0)
        (unboxed-low imm0)
        (high arg_y)
        (low arg_z))
    (pop (% ra0))
    (popq (% temp0))
    (discard-reserved-frame)
    (push (% ra0))
    (unbox-fixnum temp0 unboxed-x)
    (unbox-fixnum y unboxed-y)
    (mull (%l unboxed-y))
    (shlq ($ 32) (% unboxed-y))
    (orq (% unboxed-x) (% unboxed-y))   ; I got yer 64-bit product right here
    (unbox-fixnum prev unboxed-prev)
    (addq (% unboxed-prev) (% unboxed-y))
    (unbox-fixnum carry-in unboxed-carry-in)
    (addq (% unboxed-carry-in) (% unboxed-y))
    (movl (%l unboxed-y) (%l unboxed-low))
    (box-fixnum unboxed-low low)
    (shr ($ 32) (% unboxed-y))
    (box-fixnum unboxed-y high)
    (movq (% rsp) (% temp0))
    (pushq (% high))
    (pushq (% low))
    (set-nargs 2)
    (jmp-subprim .SPvalues)))

(defx86lapfunction %multiply-and-add3 ((x arg_x) (y arg_y) (carry-in arg_z))
  (let ((unboxed-x imm0)
        (unboxed-y imm1)
        (unboxed-carry-in imm0)
        (unboxed-low imm0)
        (high arg_y)
        (low arg_z))
    (unbox-fixnum arg_x unboxed-x)
    (unbox-fixnum y unboxed-y)
    (mull (%l unboxed-y))
    (shlq ($ 32) (% unboxed-y))
    (orq (% unboxed-x) (% unboxed-y))
    (unbox-fixnum carry-in unboxed-carry-in)
    (addq (% unboxed-carry-in) (% unboxed-y))
    (movl (%l unboxed-y) (%l unboxed-low))
    (box-fixnum unboxed-low low)
    (shr ($ 32) (% unboxed-y))
    (box-fixnum unboxed-y high)
    (movq (% rsp) (% temp0))
    (pushq (% high))
    (pushq (% low))
    (set-nargs 2)
    (jmp-subprim .SPvalues)))

;;; Return the (possibly truncated) 32-bit quotient and remainder
;;; resulting from dividing hi:low by divisor.
(defx86lapfunction %floor ((num-high arg_x) (num-low arg_y) (divisor arg_z))
  (let ((unboxed-high imm1)
        (unboxed-low imm0)
        (unboxed-quo imm0)
        (unboxed-rem imm1)
        (unboxed-divisor imm2))
    (unbox-fixnum divisor unboxed-divisor)
    (unbox-fixnum num-high unboxed-high)
    (unbox-fixnum num-low unboxed-low)
    (divl (%l unboxed-divisor))
    (box-fixnum unboxed-quo arg_y)
    (box-fixnum unboxed-rem arg_z)
    (movq (% rsp) (% temp0))
    (pushq (% arg_y))
    (pushq (% arg_z))
    (set-nargs 2)
    (jmp-subprim .SPvalues)))

;;; Multiply two (UNSIGNED-BYTE 32) arguments, return the high and
;;; low halves of the 64-bit result
(defx86lapfunction %multiply ((x arg_y) (y arg_z))
  (let ((unboxed-x imm0)
        (unboxed-y imm1)
        (unboxed-high imm1)
        (unboxed-low imm0))
    (unbox-fixnum x unboxed-x)
    (unbox-fixnum y unboxed-y)
    (mull (%l unboxed-y))
    (box-fixnum unboxed-high arg_y)
    (box-fixnum unboxed-low arg_z)
    (movq (% rsp) (% temp0))
    (pushq (% arg_y))
    (pushq (% arg_z))
    (set-nargs 2)
    (jmp-subprim .SPvalues)))

;;; Any words in the "tail" of the bignum should have been
;;; zeroed by the caller.
(defx86lapfunction %set-bignum-length ((newlen arg_y) (bignum arg_z))
  (movq (% newlen) (% imm0))
  (shl ($ (- x8664::num-subtag-bits x8664::fixnumshift)) (% imm0))
  (movb ($ x8664::subtag-bignum) (%b imm0))
  (movq (% imm0) (@ x8664::misc-header-offset (% bignum)))
  (single-value-return))

;;; Count the sign bits in the most significant digit of bignum;
;;; return fixnum count.
(defx86lapfunction %bignum-sign-bits ((bignum arg_z))
  (vector-size bignum imm0 imm0)
  (movl (@ (- x8664::misc-data-offset 4) (% bignum) (% imm0) 4) (%l imm0))
  (movl (% imm0.l) (% imm1.l))
  (notl (% imm0.l))
  (testl (% imm1.l) (% imm1.l))
  (js @wasneg)
  (notl (% imm0.l))  
  @wasneg
  (bsrl (% imm0.l) (% imm0.l))
  (sete (% imm1.b))
  (xorl ($ 31) (% imm0))
  (addb (% imm1.b) (% imm0.b))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %signed-bignum-ref ((bignum arg_y) (index arg_z))
  (uuo-error-debug-trap)
  (unbox-fixnum index imm0)
  (movslq (@ x8664::misc-data-offset (% bignum) (% imm0) 4) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))


;;; If the bignum is a one-digit bignum, return the value of the
;;; single digit as a fixnum.  Otherwise, if it's a two-digit-bignum
;;; and the two words of the bignum can be represented in a fixnum,
;;; return that fixnum; else return nil.
(defx86lapfunction %maybe-fixnum-from-one-or-two-digit-bignum ((bignum arg_z))
  (getvheader bignum imm1)
  (cmpq ($ x8664::one-digit-bignum-header) (% imm1))
  (je @one)
  (cmpq ($ x8664::two-digit-bignum-header) (% imm1))
  (jne @no)
  (movq (@ x8664::misc-data-offset (% bignum)) (% imm0))
  (box-fixnum imm0 arg_z)
  (unbox-fixnum arg_z imm1)
  (cmpq (% imm0) (% imm1))
  (je @done)
  @no
  (movq ($ nil) (% arg_z))
  (single-value-return)
  @one
  (movslq (@ x8664::misc-data-offset (% bignum)) (% imm0))
  (box-fixnum imm0 arg_z)
  @done
  (single-value-return))

;;; Again, we're out of imm regs: a variable shift count has to go in %cl.
;;; Make sure that the rest of %rcx is 0, to keep the GC happy.
;;; %rcx == temp2
(defx86lapfunction %digit-logical-shift-right ((digit arg_y) (count arg_z))
  (unbox-fixnum digit imm0)
  (unbox-fixnum count imm2)
  (shrq (% imm2.b) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))



(defx86lapfunction %ashr ((digit arg_y) (count arg_z))
  (unbox-fixnum digit imm0)
  (unbox-fixnum count imm2)
  (movslq (%l imm0) (% imm0))
  (sarq (% imm2.b) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %ashl ((digit arg_y) (count arg_z))
  (unbox-fixnum digit imm0)
  (unbox-fixnum count imm2)
  (shlq (% imm2.b) (% imm0))
  (movl (%l imm0) (%l imm0))            ;zero-extend
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction macptr->fixnum ((ptr arg_z))
  (macptr-ptr arg_z ptr)
  (single-value-return))

(defx86lapfunction fix-digit-logand ((fix arg_x) (big arg_y) (dest arg_z)) ; index 0
  (let ((w1 imm0)
        (w2 imm1))
    (movq (@ x8664::misc-data-offset (% big)) (% w2))
    (unbox-fixnum  fix w1)
    (andq (% w2) (% w1))
    (cmp-reg-to-nil dest)
    (jne @store)
    (box-fixnum w1 arg_z)
    (single-value-return)
    @store
    (movq (% w1) (@ x8664::misc-data-offset (% dest)))
    (single-value-return)))

(defx86lapfunction fix-digit-logandc2 ((fix arg_x) (big arg_y) (dest arg_z))
  (let ((w1 imm0)
        (w2 imm1))
    (movq (@ x8664::misc-data-offset (% big)) (% w2))
    (unbox-fixnum  fix w1)
    (notq (% w2))
    (andq (% w2) (% w1))
    (cmp-reg-to-nil dest)
    (jne @store)
    (box-fixnum w1 arg_z)
    (single-value-return)
    @store
    (movq (% w1) (@ x8664::misc-data-offset (% dest)))
    (single-value-return)))


(defx86lapfunction fix-digit-logandc1 ((fix arg_x) (big arg_y) (dest arg_z))
  (let ((w1 imm0)
        (w2 imm1))
    (movq (@ x8664::misc-data-offset (% big)) (% w2))
    (unbox-fixnum  fix w1)
    (notq (% w1))
    (andq (% w2) (% w1))
    (cmp-reg-to-nil dest)
    (jne @store)
    (box-fixnum w1 arg_z)
    (single-value-return)
    @store
    (movq (% w1) (@ x8664::misc-data-offset (% dest)))
    (single-value-return)))



