;;;-*- Mode: Lisp; Package: CCL -*-
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
  (require "ARM-ARCH")
  (require "ARM-LAPMACROS"))

;;; This assumes that macros & special-operators
;;; have something that's not FUNCTIONP in their
;;; function-cells.
(defarmlapfunction %function ((sym arg_z))
  (check-nargs 1)
  (let ((symptr temp0)
        (symbol temp1)
        (def arg_z))
    (cmp sym 'nil)
    (mov symbol sym)
    (mov symptr (:$ arm::nil-value))
    (add symptr symptr (:$ arm::nilsym-offset))
    (beq @ref)
    (trap-unless-xtype= sym arm::subtag-symbol)
    (mov symptr sym)
    @ref
    (ldr def (:@ symptr (:$ arm::symbol.fcell)))
    (extract-typecode imm0 def)
    (cmp imm0 (:$ arm::subtag-function))
    (bxeq lr)
    (uuo-error-udf symbol)
    (bx lr)))



;;; Traps unless sym is NIL or some other symbol.
;;; On ARM, NIL isn't really a symbol; this function maps from NIL
;;; to an internal proxy symbol ("nilsym").
(defarmlapfunction %symbol->symptr ((sym arg_z))
  (cmp sym 'nil)
  (addeq sym sym (:$ arm::nilsym-offset))
  (bxeq lr)
  (trap-unless-xtype= sym arm::subtag-symbol)
  (bx lr))

;;; Traps unless symptr is a symbol; returns NIL if symptr
;;; is NILSYM.
(defarmlapfunction %symptr->symbol ((symptr arg_z))
  (mov imm1 (:$ arm::nil-value))
  (add imm1 imm1 (:$ arm::nilsym-offset))
  (cmp imm1 symptr)
  (moveq arg_z 'nil)
  (bxeq lr)
  (trap-unless-xtype= symptr arm::subtag-symbol)
  (bx lr))

(defarmlapfunction %symptr-value ((symptr arg_z))
  (ba .SPspecref))

(defarmlapfunction %set-symptr-value ((symptr arg_y) (val arg_z))
  (ba .SPspecset))

(defarmlapfunction %symptr-binding-address ((symptr arg_z))
  (ldr imm0 (:@ symptr (:$ arm::symbol.binding-index)))
  (ldr imm2 (:@ arm::rcontext (:$ arm::tcr.tlb-limit)))
  (ldr imm1 (:@ arm::rcontext (:$ arm::tcr.tlb-pointer)))
  (cmp imm0 imm2)
  (bhs @sym)
  (ldr temp0 (:@ imm1 imm0))
  (cmp temp0 (:$ arm::subtag-no-thread-local-binding))
  (unbox-fixnum imm0 imm0)
  (beq @sym)
  (vpush1 imm1)
  (vpush1 imm0)
  (set-nargs 2)
  (add temp0 vsp '2)
  (ba .SPvalues)
  @sym
  (mov arg_y '#.arm::symbol.vcell)
  (vpush1 arg_z)
  (vpush1 arg_y)
  (set-nargs 2)
  (add temp0 vsp '2)
  (ba .SPvalues))

(defarmlapfunction %tcr-binding-location ((tcr arg_y) (sym arg_z))
  (ldr imm1 (:@ sym (:$ arm::symbol.binding-index)))
  (ldr imm2 (:@ tcr (:$ arm::tcr.tlb-limit)))
  (ldr imm0 (:@ tcr (:$ arm::tcr.tlb-pointer)))
  (mov arg_z 'nil)
  (cmp imm1 imm2)
  (bxhs lr)
  (ldr  temp0 (:@ imm0 imm1))
  (cmp  temp0 (:$ arm::subtag-no-thread-local-binding))
  (bxeq lr)
  (add arg_z imm0 imm1)
  (bx lr))

  
(defarmlapfunction %pname-hash ((str arg_y) (len arg_z))
  (let ((nextw imm1)
        (accum imm0)
        (offset imm2))
    (cmp len (:$ 0))
    (mov offset (:$ arm::misc-data-offset))
    (mov accum (:$ 0))
    (bxeq lr)
    @loop
    (subs len len '1)
    (ldr nextw (:@ str offset))
    (add offset offset (:$ 4))
    (mov accum (:ror accum (:$ 27)))
    (eor accum accum nextw)
    (bne @loop)
    (mov accum (:lsl accum (:$ 5)))
    (mov arg_z (:lsr accum (:$ (- 5 arm::fixnumshift))))
    (bx lr)))

(defarmlapfunction %string-hash ((start arg_x) (str arg_y) (len arg_z))
  (let ((nextw imm1)
        (accum imm0)
        (offset imm2))
    (cmp len (:$ 0))
    (add offset start (:$ arm::misc-data-offset))
    (mov accum (:$ 0))
    (bxeq lr)
    @loop
    (subs len len '1)
    (ldr nextw (:@ str offset))
    (add offset offset (:$ 4))
    (mov accum (:ror accum (:$ 27)))
    (eor accum accum nextw)
    (bne @loop)
    (mov accum (:lsl accum (:$ 5)))
    (mov arg_z (:lsr accum (:$ (- 5 arm::fixnumshift))))
    (bx lr)))

;;; Ensure that the current thread's thread-local-binding vector
;;; contains room for an entry with index INDEX.
;;; Return the fixnum-tagged tlb vector.
(defarmlapfunction %ensure-tlb-index ((idx arg_z))
  (ldr arg_y (:@ rcontext (:$ arm::tcr.tlb-limit)))
  (cmp arg_y idx)
  (bhi @ok)
  (uuo-tlb-too-small idx)
  @ok
  (ldr arg_z (:@ rcontext (:$ arm::tcr.tlb-pointer)))
  (bx lr))
