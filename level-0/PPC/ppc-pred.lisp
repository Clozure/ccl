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
  (require "PPC-LAPMACROS"))

#+ppc32-target
(defppclapfunction eql ((x arg_y) (y arg_z))
  "Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL."
  (check-nargs 2)
  @tail
  (cmpw cr0 x y)
  (extract-lisptag imm0 x)
  (extract-lisptag imm1 y)
  (cmpwi cr1 imm0 ppc32::tag-misc)
  (cmpwi cr2 imm1 ppc32::tag-misc)
  (beq cr0 @win)
  (bne cr1 @lose)
  (bne cr2 @lose)
  ;; Objects are both of tag-misc.  Headers must match exactly;
  ;; dispatch on subtag.
  (getvheader imm0 x)
  (getvheader imm1 y)
  (cmpw cr0 imm0 imm1)
  (extract-lowbyte imm1 imm1)
  (cmpwi cr1 imm1 ppc32::subtag-macptr)
  (cmpwi cr2 imm1 ppc32::max-numeric-subtag)
  (beq cr1 @macptr)
  (bne cr0 @lose)
  (bgt cr2 @lose)
  (cmpwi cr0 imm1 ppc32::subtag-ratio)
  (cmpwi cr1 imm1 ppc32::subtag-complex)
  (beq cr0 @node)
  (beq cr1 @node)
  ; A single-float looks a lot like a macptr to me.
  ; A double-float is simple, a bignum involves a loop.
  (cmpwi cr0 imm1 ppc32::subtag-bignum)
  (cmpwi cr1 imm1 ppc32::subtag-double-float)
  (beq cr0 @bignum)
  (bne cr1 @one-unboxed-word)                     ; single-float case
  ; This is the double-float case.
  (lwz imm0 ppc32::double-float.value x)
  (lwz imm1 ppc32::double-float.value y)
  (cmpw cr0 imm0 imm1)
  (lwz imm0 ppc32::double-float.val-low x)
  (lwz imm1 ppc32::double-float.val-low y)
  (cmpw cr1 imm0 imm1)
  (bne cr0 @lose)
  (bne cr1 @lose)
  @win
  (li arg_z (+ ppc32::t-offset ppc32::nil-value))
  (blr)
  @macptr
  (extract-lowbyte imm0 imm0)
  (cmpw cr0 imm1 imm0)
  (bne- cr0 @lose)
  @one-unboxed-word
  (lwz imm0 ppc32::misc-data-offset x)
  (lwz imm1 ppc32::misc-data-offset y)
  (cmpw cr0 imm0 imm1)
  (beq cr0 @win)
  @lose
  (li arg_z ppc32::nil-value)
  (blr)
  @bignum
  ;; Way back when, we got x's header into imm0.  We know that y's
  ;; header is identical.  Use the element-count from imm0 to control
  ;; the loop.  There's no such thing as a 0-element bignum, so the
  ;; loop must always execute at least once.
  (header-size imm0 imm0)
  (li imm1 ppc32::misc-data-offset)
  @bignum-next
  (cmpwi cr1 imm0 1)                    ; last time through ?
  (lwzx imm2 x imm1)
  (lwzx imm3 y imm1)
  (cmpw cr0 imm2 imm3)
  (subi imm0 imm0 1)
  (la imm1 4 imm1)
  (bne cr0 @lose)
  (bne cr1 @bignum-next)
  (li arg_z (+ ppc32::t-offset ppc32::nil-value))
  (blr)
  @node
  ;; Have either a ratio or a complex.  In either case, corresponding
  ;; elements of both objects must be EQL.  Recurse on the first
  ;; elements.  If true, tail-call on the second, else fail.
  (vpush x)
  (vpush y)
  (save-lisp-context)
  (lwz x ppc32::misc-data-offset x)
  (lwz y ppc32::misc-data-offset y)
  (bl @tail)
  (cmpwi cr0 arg_z ppc32::nil-value)
  (restore-full-lisp-context)
  (vpop y)
  (vpop x)
  (beq cr0 @lose)
  (lwz x (+ 4 ppc32::misc-data-offset) x)
  (lwz y (+ 4 ppc32::misc-data-offset) y)
  (b @tail))

#+ppc64-target
(defppclapfunction eql ((x arg_y) (y arg_z))
  "Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL."
  (check-nargs 2)
  @tail
  (cmpd cr0 x y)
  (extract-fulltag imm0 x)
  (extract-fulltag imm1 y)
  (cmpri cr1 imm0 ppc64::fulltag-misc)
  (cmpri cr2 imm1 ppc64::fulltag-misc)
  (beq cr0 @win)
  (bne cr1 @lose)
  (bne cr2 @lose)
  ;; Objects are both of tag-misc.  Headers must match exactly;
  ;; dispatch on subtag.
  (getvheader imm0 x)
  (getvheader imm1 y)
  (cmpd cr0 imm0 imm1)
  (extract-lowbyte imm1 imm1)
  (cmpdi cr1 imm1 ppc64::subtag-macptr)
  (cmpdi cr2 imm1 ppc64::subtag-bignum)
  (cmpdi cr3 imm1 ppc64::subtag-double-float)
  (beq cr1 @macptr)
  (cmpdi cr4 imm1 ppc64::subtag-complex)
  (cmpdi cr5 imm1 ppc64::subtag-ratio)
  (bne cr0 @lose)
  (beq cr2 @bignum)
  (beq cr3 @double-float)
  (beq cr4 @complex)
  (beq cr5 @ratio)
  @lose
  (li arg_z nil)
  (blr)
  @double-float
  (ld imm0 ppc64::double-float.value x)
  (ld imm1 ppc64::double-float.value y)
  @test  
  (cmpd imm0 imm1)
  (bne @lose)
  @win
  (li arg_z (+ ppc64::nil-value ppc64::t-offset))
  (blr)
  ;; Macptr objects can have different lengths, but their subtags must
  ;; match
  @macptr
  (extract-lowbyte imm0 imm0)
  (cmpd imm0 imm1)
  (bne @lose)
  (ld imm0 ppc64::macptr.address x)
  (ld imm1 ppc64::macptr.address y)
  (b @test)
  @ratio
  @complex
  (vpush x)
  (vpush y)
  (save-lisp-context)
  (ld x ppc64::ratio.numer x)       ; aka complex.realpart
  (ld y ppc64::ratio.numer y)       ; aka complex.imagpart
  (bl @tail)
  (cmpdi cr0 arg_z nil)
  (restore-full-lisp-context)
  (vpop y)
  (vpop x)
  (beq cr0 @lose)
  (ld x ppc64::ratio.denom x)
  (ld y ppc64::ratio.denom y)
  (b @tail)
  @bignum
  ;; Way back when, we got x's header into imm0.  We know that y's
  ;; header is identical.  Use the element-count from imm0 to control
  ;; the loop.  There's no such thing as a 0-element bignum, so the
  ;; loop must always execute at least once.
  (header-size imm0 imm0)
  (li imm1 ppc64::misc-data-offset)
  @bignum-next
  (cmpwi cr1 imm0 1)                    ; last time through ?
  (lwzx imm2 x imm1)
  (lwzx imm3 y imm1)
  (cmpw cr0 imm2 imm3)
  (subi imm0 imm0 1)
  (la imm1 4 imm1)
  (bne cr0 @lose)
  (bne cr1 @bignum-next)
  (li arg_z t)
  (blr))
  

#+ppc32-target
(defppclapfunction equal ((x arg_y) (y arg_z))
  "Return T if X and Y are EQL or if they are structured components
  whose elements are EQUAL. Strings and bit-vectors are EQUAL if they
  are the same length and have identical components. Other arrays must be
  EQ to be EQUAL.  Pathnames are EQUAL if their components are."
  (check-nargs 2)
  @top
  (cmpw cr0 x y)
  (extract-fulltag imm0 x)
  (extract-fulltag imm1 y)
  (cmpw cr1 imm0 imm1)
  (cmpwi cr2 imm0 ppc32::fulltag-cons)
  (cmpwi cr3 imm0 ppc32::fulltag-misc)
  (beq cr0 @win)
  (bne cr1 @lose)
  (beq cr2 @cons)
  (bne cr3 @lose)
  (extract-subtag imm0 x)
  (extract-subtag imm1 y)
  (cmpwi cr0 imm0 ppc32::subtag-macptr)
  (cmpwi cr2 imm0 ppc32::subtag-istruct)
  (cmpwi cr1 imm0 ppc32::subtag-vectorH)
  (cmpw cr3 imm0 imm1)
  (ble cr0 @eql)
  (cmplwi cr0 imm1 ppc32::subtag-vectorH)
  (beq cr2 @same)
  (blt cr1 @lose)
  (bge cr0 @go)
  @lose
  (li arg_z ppc32::nil-value)
  (blr)
  @same
  (bne cr3 @lose)
  @go
  (set-nargs 2)
  (lwz fname 'hairy-equal nfn)
  (ba .SPjmpsym)
  @eql
  (set-nargs 2)
  (lwz fname 'eql nfn)
  (ba .SPjmpsym)
  @cons
  (%car temp0 x)
  (%car temp1 y)
  (cmpw temp0 temp1)
  (bne @recurse)
  (%cdr x x)
  (%cdr y y)
  (b @top)
  @recurse
  (vpush x)
  (vpush y)
  (save-lisp-context)
  (lwz imm0 ppc32::tcr.cs-limit ppc32::rcontext) ; stack probe
  (twllt ppc32::sp imm0)
  (mr x temp0)
  (mr y temp1)
  (bl @top)
  (cmpwi :cr0 arg_z ppc32::nil-value)  
  (mr nfn fn)
  (restore-full-lisp-context)           ; gets old fn to fn  
  (vpop y)
  (vpop x)
  (beq cr0 @lose)
  (%cdr x x)
  (%cdr y y)
  (b @top)
  @win
  (li arg_z (+ ppc32::t-offset ppc32::nil-value))
  (blr))

#+ppc64-target
(defppclapfunction equal ((x arg_y) (y arg_z))
  "Return T if X and Y are EQL or if they are structured components
  whose elements are EQUAL. Strings and bit-vectors are EQUAL if they
  are the same length and have identical components. Other arrays must be
  EQ to be EQUAL.  Pathnames are EQUAL if their components are."
  (check-nargs 2)
  @top
  (cmpd cr0 x y)
  (extract-fulltag imm0 x)
  (extract-fulltag imm1 y)
  (cmpd cr1 imm0 imm1)
  (cmpdi cr2 imm0 ppc64::fulltag-cons)
  (cmpdi cr3 imm0 ppc64::fulltag-misc)
  (beq cr0 @win)
  (bne cr1 @lose)
  (beq cr2 @cons)
  (beq cr3 @misc)
  @lose
  (li arg_z nil)
  (blr)
  @win
  (li arg_z (+ ppc64::nil-value ppc64::t-offset))
  (blr)
  @cons
  ;; Check to see if the CARs are EQ.  If so, we can avoid saving
  ;; context, and can just tail call ourselves on the CDRs.
  (%car temp0 x)
  (%car temp1 y)
  (cmpd temp0 temp1)
  (bne @recurse)
  (%cdr x x)
  (%cdr y y)
  (b @top)
  @recurse
  (vpush x)
  (vpush y)
  (save-lisp-context)
  (ld imm0 ppc64::tcr.cs-limit ppc64::rcontext) ; stack probe
  (tdllt ppc32::sp imm0)
  (mr x temp0)
  (mr y temp1)
  (bl @top)
  (cmpdi :cr0 arg_z nil)  
  (mr nfn fn)
  (restore-full-lisp-context)           ; gets old fn to fn  
  (vpop y)
  (vpop x)
  (beq cr0 @lose)
  (%cdr x x)
  (%cdr y y)
  (b @top)
  @misc
  ;; Both objects are uvectors of some sort.  Try EQL; if that fails,
  ;; call HAIRY-EQUAL.
  (vpush x)
  (vpush y)
  (save-lisp-context)
  (set-nargs 2)
  (ld fname 'eql nfn)
  (set-nargs 2)
  (bla .SPjmpsym)
  (cmpdi arg_z nil)
  (mr nfn fn)
  (restore-full-lisp-context)
  (vpop y)
  (vpop x)
  (bne @win)
  (set-nargs 2)
  (ld fname 'hairy-equal nfn)
  (ba .SPjmpsym))



      







