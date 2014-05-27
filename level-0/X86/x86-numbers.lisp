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
#+x8664-target
(progn



(defx86lapfunction %fixnum-signum ((number arg_z))
  (movq ($ '-1) (% arg_x))
  (movq ($ '1) (% arg_y))
  (testq (% number) (% number))
  (cmovsq (% arg_x) (% arg_z))
  (cmovnsq (% arg_y) (% arg_z))
  (single-value-return))

;;; see %logcount.
(defx86lapfunction %ilogcount ((number arg_z))
  (let ((rshift imm0)
        (temp imm1))
    (unbox-fixnum number rshift)
    (xorq (% arg_z) (% arg_z))
    (testq (% rshift) (% rshift))
    (jmp @test)
    @next
    (lea (@ -1 (% rshift)) (% temp))
    (and (% temp) (% rshift))            ; sets flags
    (lea (@ '1 (% arg_z)) (% arg_z))    ; doesn't set flags
    @test
    (jne @next)
    (single-value-return)))

(defx86lapfunction %iash ((number arg_y) (count arg_z))
  (unbox-fixnum count imm1)
  (unbox-fixnum number imm0)
  (xorq (% rcx) (% rcx))                ;rcx = imm2
  (testq (% count) (% count))
  (jge @left)
  (subb (% imm1.b) (% cl))
  (sar (% cl) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return)
  @left
  (movb (% imm1.b) (% cl))
  (shl (% cl) (% number))
  (movq (% number) (% arg_z))
  (single-value-return))

(defparameter *double-float-zero* 0.0d0)
(defparameter *short-float-zero* 0.0s0)


(defx86lapfunction %fixnum-intlen ((number arg_z))
  (unbox-fixnum arg_z imm0)
  (movq (% imm0) (% imm1))
  (notq (% imm1))
  (testq (% imm0) (% imm0))
  (cmovsq (% imm1) (% imm0))
  (bsrq (% imm0) (% imm0))
  (setne (% imm1.b))
  (addb (% imm1.b) (% imm0.b))
  (box-fixnum imm0 arg_z)
  (single-value-return))


;;; Caller guarantees that result fits in a fixnum.

(defx86lapfunction %truncate-double-float->fixnum ((arg arg_z))
  (get-double-float arg fp1)
  (cvttsd2si (% fp1) (% imm0))
  (box-fixnum imm0 arg_z)  
  (single-value-return))


(defx86lapfunction %truncate-short-float->fixnum ((arg arg_z))
  (get-single-float arg fp1)
  (cvttss2si (% fp1) (% imm0))
  (box-fixnum imm0 arg_z)  
  (single-value-return))

;;; DOES round to even

(defx86lapfunction %round-nearest-double-float->fixnum ((arg arg_z))
  (get-double-float arg fp1)
  (cvtsd2si (% fp1) (% imm0))
  (box-fixnum imm0 arg_z)  
  (single-value-return))


(defx86lapfunction %round-nearest-short-float->fixnum ((arg arg_z))
  (get-single-float arg fp1)
  (cvtss2si (% fp1) (% imm0))
  (box-fixnum imm0 arg_z)  
  (single-value-return))



;;; We'll get a SIGFPE if divisor is 0.
;;; Don't use %rbp.  Trust callback_for_interrupt() to preserve
;;; the word below the stack pointer
(defx86lapfunction %fixnum-truncate ((dividend arg_y) (divisor arg_z))
  (save-simple-frame)
  (cmpq ($ '-1) (% divisor))
  (je @neg)
  (unbox-fixnum divisor imm0)
  (movq (% imm0) (% imm2))
  (unbox-fixnum dividend imm0)
  (cqto)                                ; imm1 := sign_extend(imm0)
  (idivq (% imm2))
  (pop (% rbp))
  (movq (% rsp) (% temp0))
  (box-fixnum imm1 arg_y)
  (box-fixnum imm0 arg_z)
  (pushq (% arg_z))
  (pushq (% arg_y))
  (set-nargs 2)
  (jmp-subprim .SPvalues)
  @neg
  (negq (% dividend))
  (load-constant *least-positive-bignum* arg_z)
  (cmovoq (@ x8664::symbol.vcell (% arg_z)) (% dividend))
  (pop (% rbp))
  (movq (% rsp) (% temp0))
  (pushq (% dividend))
  (pushq ($ 0))
  (set-nargs 2)
  (jmp-subprim .SPvalues))
  

(defx86lapfunction called-for-mv-p ()
  (ref-global ret1valaddr imm0)
  (movq (@ x8664::lisp-frame.return-address (% rbp)) (% imm1))
  (cmpq (% imm0) (% imm1))
  (movq ($ t) (% imm0))
  (movq ($ nil) (% arg_z))
  (cmoveq (% imm0) (% arg_z))
  (single-value-return))


;;; n1 and n2 must be positive (esp non zero)
(defx86lapfunction %fixnum-gcd ((boxed-u arg_y) (boxed-v arg_z))
  (let ((u imm0)
        (v imm1)
        (k imm2))
    (xorl (% imm2.l) (% imm2.l))
    (bsfq (% boxed-u) (% u))
    (bsfq (% boxed-v) (% v))
    (rcmp (% u) (% v))
    (cmovlel (%l u) (%l k))
    (cmovgl (%l v) (%l k))
    (unbox-fixnum boxed-u u)
    (unbox-fixnum boxed-v v)
    (subb ($ x8664::fixnumshift) (%b k))
    (jz @start)
    (shrq (% cl) (% u))
    (shrq (% cl) (% v))
    @start
    ;; At least one of u or v is odd at this point
    @loop
    ;; if u is even, shift it right one bit
    (testb ($ 1) (%b u))
    (jne @u-odd)
    (shrq ($ 1) (% u))
    (jmp @test)
    @u-odd
    ;; if v is even, shift it right one bit
    (testb ($ 1) (%b v))
    (jne @both-odd)
    (shrq ($ 1) (% v))
    (jmp @test-u)
    @both-odd
    (cmpq (% v) (% u))
    (jb @v>u)
    (subq (% v) (% u))
    (shrq ($ 1) (% u))
    (jmp @test)
    @v>u
    (subq (% u) (% v))
    (shrq ($ 1) (% v))
    @test-u
    (testq (% u) (% u))
    @test
    (ja @loop)
    (shlq (% cl) (% v))
    (movb ($ 0) (% cl))
    (box-fixnum v arg_z)
    (single-value-return)))

(defx86lapfunction %mrg31k3p ((state arg_z))
  (let ((seed temp0)
	(m1 #x7fffffff)
	(m2 #x7fffadb3)
	(negative-m1 #x80000001)
	(negative-m2 #x8000524d))
    (svref state 1 seed)
    (movl (@ (+ x8664::misc-data-offset (* 4 1)) (% seed)) (% imm0.l))
    (andl ($ #x1ff) (% imm0.l))
    (shll ($ 22) (% imm0.l))
    (movl (@ (+ x8664::misc-data-offset (* 4 1)) (% seed)) (% imm1.l))
    (shrl ($ 9) (% imm1.l))
    (addl (% imm1.l) (% imm0.l))

    (movl (@ (+ x8664::misc-data-offset (* 4 2)) (% seed)) (% imm1.l))
    (andl ($ #xffffff) (% imm1.l))
    (shll ($ 7) (% imm1.l))
    (addl (% imm1.l) (% imm0.l))
    (movl (@ (+ x8664::misc-data-offset (* 4 2)) (% seed)) (% imm1.l))
    (shrl ($ 24) (% imm1.l))

    (addl (% imm1.l) (% imm0.l))
    (leal (@ negative-m1 (% imm0.l)) (% imm1.l))
    (cmpl ($ m1) (% imm0.l))
    (cmovael (% imm1.l) (% imm0.l))

    (addl (@ (+ x8664::misc-data-offset (* 4 2)) (% seed)) (% imm0.l))
    (leal (@ negative-m1 (% imm0.l)) (% imm1.l))
    (cmpl ($ m1) (% imm0.l))
    (cmovael (% imm1.l) (% imm0.l))

    ;; update state
    (movl (@ (+ x8664::misc-data-offset (* 4 1)) (% seed)) (% imm1.l))
    (movl (% imm1.l) (@ (+ x8664::misc-data-offset (* 4 2)) (% seed)))
    (movl (@ (+ x8664::misc-data-offset (* 4 0)) (% seed)) (% imm1.l))
    (movl (% imm1.l) (@ (+ x8664::misc-data-offset (* 4 1)) (% seed)))
    (movl (% imm0.l) (@ (+ x8664::misc-data-offset (* 4 0)) (% seed)))

    ;; second component
    (movl (@ (+ x8664::misc-data-offset (* 4 3)) (% seed)) (% imm0.l))
    (andl ($ #xffff) (% imm0.l))
    (shll ($ 15) (% imm0.l))
    (movl (@ (+ x8664::misc-data-offset (* 4 3)) (% seed)) (% imm1.l))
    (shrl ($ 16) (% imm1.l))
    (imull ($ 21069) (% imm1.l) (% imm1.l))

    (addl (% imm1.l) (% imm0.l))
    (leal (@ negative-m2 (% imm0.l)) (% imm1.l))
    (cmpl ($ m2) (% imm0.l))
    (cmovael (% imm1.l) (% imm0.l))

    (movl (% imm0.l) (% imm2.l))	;stash t1

    (movl (@ (+ x8664::misc-data-offset (* 4 5)) (% seed)) (% imm0.l))
    (andl ($ #xffff) (% imm0.l))
    (shll ($ 15) (% imm0.l))
    (movl (@ (+ x8664::misc-data-offset (* 4 5)) (% seed)) (% imm1.l))
    (shrl ($ 16) (% imm1.l))
    (imull ($ 21069) (% imm1.l) (% imm1.l))

    (addl (% imm1.l) (% imm0.l))
    (leal (@ negative-m2 (% imm0.l)) (% imm1.l))
    (cmpl ($ m2) (% imm0.l))
    (cmovael (% imm1.l) (% imm0.l))

    (addl (@ (+ x8664::misc-data-offset (* 4 5)) (% seed)) (% imm0.l))
    (leal (@ negative-m2 (% imm0.l)) (% imm1.l))
    (cmpl ($ m2) (% imm0.l))
    (cmovael (% imm1.l) (% imm0.l))

    (addl (% imm2.l) (% imm0.l))	;add in t1
    (leal (@ negative-m2 (% imm0.l)) (% imm1.l))
    (cmpl ($ m2) (% imm0.l))
    (cmovael (% imm1.l) (% imm0.l))

    ;; update state
    (movl (@ (+ x8664::misc-data-offset (* 4 4)) (% seed)) (% imm1.l))
    (movl (% imm1.l) (@ (+ x8664::misc-data-offset (* 4 5)) (% seed)))
    (movl (@ (+ x8664::misc-data-offset (* 4 3)) (% seed)) (% imm1.l))
    (movl (% imm1.l) (@ (+ x8664::misc-data-offset (* 4 4)) (% seed)))
    (movl (% imm0.l) (@ (+ x8664::misc-data-offset (* 4 3)) (% seed)))

    ;; combination
    (movl (@ (+ x8664::misc-data-offset (* 4 0)) (% seed)) (% imm1.l))
    (xchgl (% imm1.l) (% imm0.l))		;for sanity
    (rcmpl (% imm0.l) (% imm1.l))
    (ja @ok)
    (subl (% imm1.l) (% imm0.l))
    (addl ($ m1) (% imm0.l))
    (box-fixnum imm0 arg_z)
    (single-value-return)
    @ok
    (subl (% imm1.l) (% imm0.l))
    (box-fixnum imm0 arg_z)
    (single-value-return)))


;;; These things (or something like them) should get inlined soon.
;;; Recall that (COMPLEX SINGLE-FLOAT) and (COMPLEX DOUBLE-FLOAT)
;;; objects are viewed as having 32-bit elements and are viewed
;;  as having some extra words for alignment.
(defx86lapfunction %make-complex-double-float ((r arg_y) (i arg_z))
  (movsd (@ target::misc-dfloat-offset (% r)) (% xmm0))
  (movsd (@ target::misc-dfloat-offset (% i)) (% xmm1))
  (unpcklpd (% xmm1) (% xmm0))
  (movl ($ (logior (ash 6 x8664::num-subtag-bits) x8664::subtag-complex-double-float)) (%l imm0))
  (movl ($ (- (* 3 16) x8664::fulltag-misc)) (%l imm1))
  (subq (% imm1) (:rcontext x8664::tcr.save-allocptr))
  (movq (:rcontext x8664::tcr.save-allocptr) (% allocptr))
  (cmpq (:rcontext x8664::tcr.save-allocbase) (% allocptr))
  (ja @no-trap)
  (uuo-alloc)
  @no-trap
  (movq (% imm0) (@ x8664::misc-header-offset (% temp0)))
  (andb ($ (lognot x8664::fulltagmask)) (:rcontext x8664::tcr.save-allocptr))
  (movq (% allocptr) (% arg_z))
  (movdqa (% xmm0) (@ x8664::complex-double-float.realpart (% arg_z)))
  (single-value-return))

(defx86lapfunction %make-complex-single-float ((r arg_y) (i arg_z))
  (movd (% r) (% xmm0))
  (psrlq ($ 32) (% xmm0))
  (movd (% i) (% xmm1))
  (psrlq ($ 32) (% xmm1))
  (unpcklps (% xmm1) (% xmm0))
  (movl ($ (logior (ash 2 x8664::num-subtag-bits) x8664::subtag-complex-single-float)) (%l imm0))
  (movl ($ (- (* 1 16) x8664::fulltag-misc)) (%l imm1))
  (subq (% imm1) (:rcontext x8664::tcr.save-allocptr))
  (movq (:rcontext x8664::tcr.save-allocptr) (% allocptr))
  (cmpq (:rcontext x8664::tcr.save-allocbase) (% allocptr))
  (ja @no-trap)
  (uuo-alloc)
  @no-trap
  (movq (% imm0) (@ x8664::misc-header-offset (% temp0)))
  (andb ($ (lognot x8664::fulltagmask)) (:rcontext x8664::tcr.save-allocptr))
  (movq (% allocptr) (% arg_z))
  (movq (% xmm0) (@ x8664::complex-single-float.realpart (% arg_z)))
  (single-value-return))
                                               


  
;;; End of x86-numbers.lisp
) ; #+x8664-target
