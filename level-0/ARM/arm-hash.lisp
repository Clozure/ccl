;;; -*- Mode: Lisp; Package: CCL -*-
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

;;; level-0;ARM;arm-hash.lisp


(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "HASHENV" "ccl:xdump;hashenv"))




;;; This should stay in LAP so that it's fast
;;; Equivalent to cl:mod when both args are positive fixnums
(defarmlapfunction fast-mod ((number arg_y) (divisor arg_z))
  (build-lisp-frame imm0)
  (mov imm0 (:lsr number (:$ arm::fixnumshift)))
  (mov imm1 (:lsr divisor (:$ arm::fixnumshift)))
  (sploadlr .SPudiv32)
  (blx lr)
  (box-fixnum arg_z imm1)
  (return-lisp-frame imm0))


(defarmlapfunction fast-mod-3 ((number arg_x) (divisor arg_y) (recip arg_z))
  (mov imm0 (:lsr number (:$ arm::fixnumshift)))
  (smull imm2 imm1 imm0 recip)
  (mul imm0 imm1 divisor)
  (sub number number imm0)
  (sub number number divisor)
  (mov imm0 (:asr number (:$ (1- arm::nbits-in-word))))
  (and divisor divisor imm0)
  (add arg_z number divisor)
  (bx lr))

(defarmlapfunction %dfloat-hash ((key arg_z))
  (ldr imm0 (:@ key (:$ arm::double-float.value)))
  (ldr imm1 (:@ key (:$ arm::double-float.val-low)))
  (add imm0 imm0 imm1)
  (box-fixnum arg_z imm0)
  (bx lr))



(defarmlapfunction %sfloat-hash ((key arg_z))
  (ldr imm0 (:@ key (:$ arm::single-float.value)))
  (box-fixnum arg_z imm0)
  (bx lr))



(defarmlapfunction %macptr-hash ((key arg_z))
  (ldr imm0 (:@ key (:$ arm::macptr.address)))
  (add imm0 imm0 (:lsr imm0 (:$ 24)))
  (bic arg_z imm0 (:$ arm::fixnummask))
  (bx lr))

(defarmlapfunction %bignum-hash ((key arg_z))
  (let ((header imm1)
        (offset imm2)
        (ndigits temp1)
        (immhash imm0))
    (mov immhash (:$ 0))
    (mov offset (:$ arm::misc-data-offset))
    (getvheader header key)
    (header-length ndigits header)
    (let ((next header))
      @loop
      (subs ndigits ndigits '1)
      (ldr next (:@ key offset))
      (add offset offset (:$ 4))
      (add immhash next (:ror immhash (:$ 19)))
      (bne @loop))
    (bic arg_z immhash (:$ arm::fixnummask))
    (bx lr)))




(defarmlapfunction %get-fwdnum ()
  (ref-global arg_z arm::fwdnum)
  (bx lr))


(defarmlapfunction %get-gc-count ()
  (ref-global arg_z arm::gc-count)
  (bx lr))


;;; Setting a key in a hash-table vector needs to 
;;; ensure that the vector header gets memoized as well
(defarmlapfunction %set-hash-table-vector-key ((vector arg_x) (index arg_y) (value arg_z))
  (spjump .SPset-hash-key))

(defarmlapfunction %set-hash-table-vector-key-conditional ((offset 0) (vector arg_x) (old arg_y) (new arg_z))
  (spjump .SPset-hash-key-conditional))

;;; Strip the tag bits to turn x into a fixnum
(defarmlapfunction strip-tag-to-fixnum ((x arg_z))
  (bic arg_z x (:$ arm::fulltagmask))
  (mov arg_z (:lsr arg_z (:$ (- arm::ntagbits arm::nfixnumtagbits))))
  (bx lr))

;;; end of arm-hash.lisp
