;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2006-2009 Clozure Associates and contributors
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

;;; level-0;ppc;ppc-hash.lisp


(in-package "CCL")
#+x8664-target
(progn

(eval-when (:compile-toplevel :execute)
  (require "HASHENV" "ccl:xdump;hashenv"))




;;; This should stay in LAP so that it's fast
;;; Equivalent to cl:mod when both args are positive fixnums


(defx86lapfunction fast-mod ((number arg_y) (divisor arg_z))
  (xorq (% imm1) (% imm1))
  (mov (% number) (% imm0))
  (div (% divisor))
  (mov (% imm1) (% arg_z))
  (single-value-return))


;; Faster mod based on Bruce Hoult's Dylan version, modified to use a branch-free max.
(defx86lapfunction fast-mod-3 ((number arg_x) (divisor arg_y) (recip arg_z))
  (mov (% number) (% imm0))
  (shrq ($ target::fixnumshift) (% imm0))
  (mov (% recip) (% imm1))
  (mul (% imm1)) ;; -> hi word in imm1 (unboxed)
  (mov (% divisor) (% imm0))
  (mul (% imm1)) ;; -> lo word in imm0 (boxed)
  (subq (% imm0) (% number))
  (subq (% divisor) (% number))
  (mov (% number) (% arg_z))
  (mov (% number) (% imm0))
  (sar ($ (1- target::nbits-in-word)) (% imm0))
  (andq (% imm0) (% divisor))
  (addq (% divisor) (% arg_z))
  (single-value-return))

(defx86lapfunction %dfloat-hash ((key arg_z))
  (movq (@ x8664::double-float.value (% key)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %sfloat-hash ((key arg_z))
  (mov (% key) (% imm1))
  (movl ($ #x-80000000) (%l imm0))
  (shr ($ 32) (% imm1))
  (xorq (% arg_y) (% arg_y))
  (shr ($ (- 32 x8664::fixnumshift)) (% key))
  (rcmp (%l imm0) (%l imm1))
  (cmoveq (% arg_y) (% arg_z))
  (single-value-return))

(defx86lapfunction %macptr-hash ((key arg_z))
  (movq (@ target::macptr.address (% key)) (% imm0))
  (movq (% imm0) (% imm1))
  (shlq ($ 24) (% imm1))
  (addq (% imm1) (% imm0))
  (movq ($ (lognot target::fixnummask)) (% arg_z))
  (andq (% imm0) (% arg_z))
  (single-value-return))


(defx86lapfunction %bignum-hash ((key arg_z))
  (let ((header imm0)
        (offset imm1)
        (ndigits temp0))
    (getvheader key header)
    (header-length header ndigits)
    (xorq (% offset) (% offset))
    (let ((immhash header))
      @loop
      (rolq ($ 13) (% immhash))
      (addl (@ x8664::misc-data-offset (% key) (% offset)) (%l immhash))
      (addq ($ 4) (% offset))
      (subq ($ '1) (% ndigits))
      (jne  @loop)
      (box-fixnum immhash arg_z))
    (single-value-return)))


(defx86lapfunction %get-fwdnum ()
  (ref-global target::fwdnum arg_z)
  (single-value-return))


(defx86lapfunction %get-gc-count ()
  (ref-global target::gc-count arg_z)
  (single-value-return))


;;; Setting a key in a hash-table vector needs to 
;;; ensure that the vector header gets memoized as well
(defx86lapfunction %set-hash-table-vector-key ((vector arg_x) (index arg_y) (value arg_z))
  (jmp-subprim .SPset-hash-key))

;;; This needs to be done out-of-line, to handle EGC memoization.
(defx86lapfunction %set-hash-table-vector-key-conditional ((offset 8) #|(ra 0)|# (vector arg_x) (old arg_y) (new arg_z))
  (movq (@ offset (% rsp)) (% temp0))
  (save-simple-frame)
  (call-subprim .SPset-hash-key-conditional)
  (restore-simple-frame)
  (single-value-return 3))

;;; Strip the tag bits to turn x into a fixnum
(defx86lapfunction strip-tag-to-fixnum ((x arg_z))
  (andq ($ (lognot target::fulltagmask)) (% x))
  (shrq ($ (- target::ntagbits target::fixnumshift)) (% arg_z))
  (single-value-return))

;;; end of x86-hash.lisp
) ; #+x8664-target
