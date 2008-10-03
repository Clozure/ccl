;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  #+ppc32-target
  (require "PPC32-ARCH")
  #+ppc64-target
  (require "PPC64-ARCH")
  (require "PPC-LAPMACROS"))

;;; This assumes that macros & special-operators
;;; have something that's not FUNCTIONP in their
;;; function-cells.
#+ppc32-target
(defppclapfunction %function ((sym arg_z))
  (check-nargs 1)
  (cmpwi cr1 sym (target-nil-value))
  (let ((symptr temp0)
        (symbol temp1)
        (def arg_z))
    (li symptr (+ ppc32::nilsym-offset (target-nil-value)))
    (mr symbol sym)
    (if (:cr1 :ne)
      (progn
        (trap-unless-typecode= sym ppc32::subtag-symbol)
        (mr symptr sym)))
    (lwz def ppc32::symbol.fcell symptr)
    (extract-typecode imm0 def)
    (cmpwi cr0 imm0 ppc32::subtag-function)
    (beqlr+)
    (uuo_interr arch::error-udf symbol)))

#+ppc64-target
(defppclapfunction %function ((sym arg_z))
  (check-nargs 1)
  (let ((symbol temp1)
        (def arg_z))
    (mr symbol sym)
    (trap-unless-typecode= sym ppc64::subtag-symbol)
    (mr symbol sym)
    (ld def ppc64::symbol.fcell symbol)
    (extract-typecode imm0 def)
    (cmpdi cr0 imm0 ppc64::subtag-function)
    (beqlr+)
    (uuo_interr arch::error-udf symbol)))

;;; Traps unless sym is NIL or some other symbol.
;;; On PPC32, NIL isn't really a symbol; this function maps from NIL
;;; to an internal proxy symbol ("nilsym").
;;; On PPC64, NIL is a real symbol, so this function just does a
;;; little bit of type checking.
(defppclapfunction %symbol->symptr ((sym arg_z))
  #+ppc32-target
  (progn
    (cmpwi cr0 arg_z (target-nil-value))
    (if (:cr0 :eq)
      (progn
        (li arg_z (+ ppc32::nilsym-offset (target-nil-value)))
        (blr))))
  (trap-unless-typecode= arg_z target::subtag-symbol)
  (blr))

;;; Traps unless symptr is a symbol; on PPC32, returns NIL if symptr
;;; is NILSYM.
(defppclapfunction %symptr->symbol ((symptr arg_z))
  #+ppc32-target
  (progn
    (li imm1 (+ ppc32::nilsym-offset (target-nil-value)))
    (cmpw cr0 imm1 symptr)
    (if (:cr0 :eq)
      (progn 
        (li arg_z nil)
        (blr))))
  (trap-unless-typecode= symptr target::subtag-symbol imm0)
  (blr))

(defppclapfunction %symptr-value ((symptr arg_z))
  (ba .SPspecref))

(defppclapfunction %set-symptr-value ((symptr arg_y) (val arg_z))
  (ba .SPspecset))

(defppclapfunction %symptr-binding-address ((symptr arg_z))
  (ldr imm3 target::symbol.binding-index symptr)
  (ldr imm2 target::tcr.tlb-limit target::rcontext)
  (ldr imm4 target::tcr.tlb-pointer target::rcontext)
  (cmplr imm3 imm2)
  (bge @sym)
  (ldrx temp0 imm4 imm3)
  (cmpdi temp0 target::subtag-no-thread-local-binding)
  (slri imm3 imm3 target::fixnumshift)
  (beq @sym)
  (vpush imm4)
  (vpush imm3)
  (set-nargs 2)
  (la temp0 '2 vsp)
  (ba .SPvalues)
  @sym
  (li arg_y '#.target::symbol.vcell)
  (vpush arg_z)
  (vpush arg_y)
  (set-nargs 2)
  (la temp0 '2 vsp)
  (ba .SPvalues))

(defppclapfunction %tcr-binding-location ((tcr arg_y) (sym arg_z))
  (ldr imm3 target::symbol.binding-index sym)
  (ldr imm2 target::tcr.tlb-limit tcr)
  (ldr imm4 target::tcr.tlb-pointer tcr)
  (li arg_z nil)
  (cmplr imm3 imm2)
  (bgelr)
  (ldrx temp0 imm4 imm3)
  (cmpri temp0 target::subtag-no-thread-local-binding)
  (beqlr)
  (add arg_z imm4 imm3)
  (blr))

  
(defppclapfunction %pname-hash ((str arg_y) (len arg_z))
  (let ((nextw imm1)
        (accum imm0)
        (offset imm2))
    (cmpwi cr0 len 0)
    (li offset target::misc-data-offset)
    (li accum 0)
    (beqlr- cr0)    
    @loop
    (cmpri cr1 len '1)
    (subi len len '1)
    (lwzx nextw str offset)
    (addi offset offset 4)
    (rotlwi accum accum 5)
    (xor accum accum nextw)
    (bne cr1 @loop)
    (slri accum accum 5)
    (srri arg_z accum (- 5 target::fixnumshift))
    (blr)))

(defppclapfunction %string-hash ((start arg_x) (str arg_y) (len arg_z))
  (let ((nextw imm1)
        (accum imm0)
        (offset imm2))
    (cmpwi cr0 len 0)
    #+32-bit-target
    (la offset target::misc-data-offset start)
    #+64-bit-target
    (progn
      (srwi offset start 1)
      (la offset target::misc-data-offset offset))
    (li accum 0)
    (beqlr- cr0)    
    @loop
    (cmpri cr1 len '1)
    (subi len len '1)
    (lwzx nextw str offset)
    (addi offset offset 4)
    (rotlwi accum accum 5)
    (xor accum accum nextw)
    (bne cr1 @loop)
    (slri accum accum 5)
    (srri arg_z accum (- 5 target::fixnumshift))
    (blr)))
