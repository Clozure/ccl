;;; Copyright 2009 Clozure Associates
;;; This file is part of Clozure CL.  
;;;
;;; Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;; Public License , known as the LLGPL and distributed with Clozure
;;; CL as the file "LICENSE".  The LLGPL consists of a preamble and
;;; the LGPL, which is distributed with Clozure CL as the file "LGPL".
;;; Where these conflict, the preamble takes precedence.
;;;
;;; Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "HASHENV" "ccl:xdump;hashenv"))

;;; This should stay in LAP so that it's fast
;;; Equivalent to cl:mod when both args are positive fixnums
(defx8632lapfunction fast-mod ((number arg_y) (divisor arg_z))
  (xorl (% edx) (% edx))		;aka temp1
  (mov (% number) (% imm0))
  (div (% divisor))			;boxed remainder goes into edx/temp1
  (mov (% edx) (% arg_z))
  (single-value-return))

;; Faster mod based on Bruce Hoult's Dylan version, modified to use a
;; branch-free max.
(defx8632lapfunction fast-mod-3 ((number 4) #|(ra 0)|# (divisor arg_y) (recip arg_z))
  (mark-as-imm temp1)
  (let ((imm1 temp1)
	(n temp0))
    (movl (@ number (% esp)) (% n))
    (movl (% n) (% imm0))
    (shrl ($ target::fixnumshift) (% imm0)) ;logical shift is intentional
    (mov (% recip) (% imm1))
    (mul (% imm1)) ;; -> hi word in imm1 (unboxed)
    (mov (% divisor) (% imm0))
    (mul (% imm1)) ;; -> lo word in imm0 (boxed)
    (subl (% imm0) (% n))
    (subl (% divisor) (% n))
    (mov (% n) (% arg_z))
    (mov (% n) (% imm0))
    (sar ($ (1- target::nbits-in-word)) (% imm0))
    (andl (% imm0) (% divisor))
    (addl (% divisor) (% arg_z)))
  (mark-as-node temp1)
  (single-value-return 3))

(defx8632lapfunction %dfloat-hash ((key arg_z))
  (movl (@ x8632::double-float.value (% key)) (% imm0))
  (addl (@ x8632::double-float.val-high (% key)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx8632lapfunction %sfloat-hash ((key arg_z))
  (movl (@ x8632::single-float.value (% key)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx8632lapfunction %macptr-hash ((key arg_z))
  (movl (@ x8632::macptr.address (% key)) (% imm0))
  (box-fixnum imm0 temp0)
  (shll ($ (- 24 x8632::fixnumshift)) (% temp0))
  (addl (% temp0) (% imm0))
  (movl ($ (lognot x8632::fixnummask)) (% arg_z))
  (andl (% imm0) (% arg_z))
  (single-value-return))

(defx8632lapfunction %bignum-hash ((key arg_z))
  (mark-as-imm temp1)
  (let ((header imm0)
	(offset temp1)
	(ndigits temp0))
    (getvheader key header)
    (header-length header ndigits)
    (xorl (% offset) (% offset))
    (let ((immhash header))
      @loop
      (roll ($ 13) (% immhash))
      (addl (@ x8632::misc-data-offset (% key) (% offset)) (% immhash))
      (addl ($ 4) (% offset))
      (subl ($ '1) (% ndigits))
      (jne @loop)
      (box-fixnum immhash arg_z)))
  (mark-as-node temp1)
  (single-value-return))

(defx8632lapfunction %get-fwdnum ()
  (ref-global target::fwdnum arg_z)
  (single-value-return))

(defx8632lapfunction %get-gc-count ()
  (ref-global target::gc-count arg_z)
  (single-value-return))

;;; Setting a key in a hash-table vector needs to 
;;; ensure that the vector header gets memoized as well
(defx8632lapfunction %set-hash-table-vector-key ((vector 4) #|(ra 0)|# (index arg_y) (value arg_z))
  (pop (% temp1))			;return address
  (pop (% temp0))			;.SPset-hash-key wants arg in temp0
  (discard-reserved-frame)
  (push (% temp1))
  (jmp-subprim .SPset-hash-key))

;;; This needs to be done out-of-line, to handle EGC memoization.
(defx8632lapfunction %set-hash-table-vector-key-conditional ((offset 8)
                                                             (vector 4)
                                                             #|(ra 0)|#
                                                             (old arg_y)
                                                             (new arg_z))
  (movl (@ offset (% esp)) (% temp0))
  (movl (@ vector (% esp)) (% temp1))
  (save-simple-frame)
  (call-subprim .SPset-hash-key-conditional)
  (restore-simple-frame)
  (single-value-return 4))


;;; Strip the tag bits to turn x into a fixnum
(defx8632lapfunction strip-tag-to-fixnum ((x arg_z))
  (andl ($ (lognot target::fulltagmask)) (% x))
  (shrl ($ (- target::ntagbits target::fixnumshift)) (% arg_z))
  (single-value-return))

