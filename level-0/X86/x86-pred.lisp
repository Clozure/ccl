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
#+x8664-target
(progn

(eval-when (:compile-toplevel :execute)
  (require "X86-LAPMACROS"))


(defx86lapfunction eql ((x arg_y) (y arg_z))
  "Return T if OBJ1 and OBJ2 represent either the same object or
numbers with the same type and value."
  (check-nargs 2)
  @top
  @tail
  (cmpq (% x) (% y))
  (je @win)
  (extract-fulltag x imm0)
  (extract-fulltag y imm1)
  (cmpb (% imm0.b) (% imm1.b))
  (jnz @lose)
  (cmpb ($ x8664::fulltag-misc) (% imm0.b))
  (jnz @lose)
  (getvheader x imm0)
  (getvheader y imm1)
  (cmpb ($ x8664::subtag-macptr) (% imm0.b))
  (je @macptr)                          ; will need to check %imm1.b
  (cmpq (% imm0) (% imm1))
  (jne @lose)
  (cmpb ($ x8664::subtag-bignum) (% imm0.b))
  (je @bignum)
  (cmpb ($ x8664::subtag-double-float) (% imm0.b))
  (je @double-float)
  (cmpb ($ x8664::subtag-complex) (% imm0.b))
  (je @complex)
  (cmpb ($ x8664::subtag-ratio) (% imm0.b))
  (je @ratio)
  @lose
  (movq ($ nil) (% arg_z))
  (single-value-return)
  @macptr
  (cmpb ($ x8664::subtag-macptr) (% imm1.b))
  (jne @lose)
  @double-float
  (movq  (@ x8664::misc-data-offset (% x)) (% imm0))
  (movq  (@ x8664::misc-data-offset (% y)) (% imm1))
  @test
  (cmpq (% imm0) (% imm1))
  (movl ($ (target-t-value)) (%l imm0))
  (lea (@ (- x8664::t-offset) (% imm0)) (% arg_z))
  (cmovel (%l imm0) (%l arg_z))
  (single-value-return)
  @win
  (movq ($ t) (% arg_z))
  (single-value-return)
  @ratio
  @complex
  (save-simple-frame)
  (pushq (@ x8664::ratio.denom (% x)))  ; aka complex.imagpart
  (pushq (@ x8664::ratio.denom (% y)))
  (movq (@ x8664::ratio.numer (% x)) (% x))       ; aka complex.realpart
  (movq (@ x8664::ratio.numer (% y)) (% y))       ; aka complex.realpart
  (:talign 3)
  (call @top)
  (recover-fn-from-rip)
  (cmp-reg-to-nil arg_z)
  (pop (% y))
  (pop (% x))
  (restore-simple-frame)
  (jnz @tail)
  ;; lose, again
  (movq ($ nil) (% arg_z))
  (single-value-return)
  @bignum
  ;; Way back when, we got x's header into imm0.  We know that y's
  ;; header is identical.  Use the element-count from imm0 to control
  ;; the loop.  There's no such thing as a 0-element bignum, so the
  ;; loop must always execute at least once.
  (header-length imm0 temp0)
  (xorq (% imm1) (% imm1))
  @bignum-next
  (movl (@ x8664::misc-data-offset (% x) (% imm1)) (% imm0.l))
  (cmpl (@ x8664::misc-data-offset (% y) (% imm1)) (% imm0.l))
  (jne @lose)
  (addq ($ 4) (% imm1))
  (sub ($ '1) (% temp0))
  (jnz @bignum-next)
  (movq ($ t) (% arg_z))
  (single-value-return))
  


(defx86lapfunction equal ((x arg_y) (y arg_z))
  "Return T if X and Y are EQL or if they are structured components
  whose elements are EQUAL. Strings and bit-vectors are EQUAL if they
  are the same length and have identical components. Other arrays must be
  EQ to be EQUAL.  Pathnames are EQUAL if their components are."
  (check-nargs 2)
  @top
  @tail
  (cmpq (% x) (% y))
  (je @win)
  (extract-fulltag x imm0)
  (extract-fulltag y imm1)
  (cmpb (% imm0.b) (% imm1.b))
  (jne @lose)
  (cmpb ($ x8664::fulltag-cons) (% imm0.b))
  (je @cons)
  (cmpb ($ x8664::fulltag-misc) (% imm0.b))
  (je @misc)
  @lose
  (movq ($ nil) (% arg_z))
  (single-value-return)
  @win
  (movq ($ t) (% arg_z))
  (single-value-return)
  @cons
  ;; Check to see if the CARs are EQ.  If so, we can avoid saving
  ;; context, and can just tail call ourselves on the CDRs.
  (%car x temp0)
  (%car y temp1)
  (cmpq (% temp0) (% temp1))
  (jne @recurse)
  (%cdr x x)
  (%cdr y y)
  (jmp @tail)
  @recurse
  (save-simple-frame)
  (pushq (@ x8664::cons.cdr (% x)))
  (pushq (@ x8664::cons.cdr (% y)))
  (movq (% temp0) (% x))
  (movq (% temp1) (% y))
  (:talign 4)
  (call @top)
  (recover-fn-from-rip)
  (cmp-reg-to-nil arg_z)
  (pop (% y))
  (pop (% x))
  (restore-simple-frame)         
  (jnz @top)
  (movl ($ nil) (% arg_z.l))
  (single-value-return)
  @misc
  ;; Both objects are uvectors of some sort.  Try EQL; if that fails,
  ;; call HAIRY-EQUAL.
  (save-simple-frame)
  (pushq (% x))
  (pushq (% y))
  (call-symbol eql 2)
  (cmp-reg-to-nil arg_z)
  (jne @won-with-eql)
  (popq (% y))
  (popq (% x))
  (restore-simple-frame)
  (jump-symbol hairy-equal 2)
  @won-with-eql
  (restore-simple-frame)                ; discards pushed args
  (movl ($ t) (% arg_z.l))
  (single-value-return))

(defx86lapfunction %lisp-lowbyte-ref ((thing arg_z))
  (box-fixnum thing arg_z)
  (andl ($ '#xff) (%l arg_z))
  (single-value-return))


      







) ; #+x8664-target
