;;;-*- Mode: Lisp; Package: CCL -*-
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

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARM-LAPMACROS"))

(defarmlapfunction eql ((x arg_y) (y arg_z))
  "Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL."
  (check-nargs 2)
  @tail
  (cmp x y)
  (extract-lisptag imm0 x)
  (extract-lisptag imm1 y)
  (beq @win)
  (cmp imm0 (:$ arm::tag-misc))
  (cmpeq imm1 (:$ arm::tag-misc))
  (bne @lose)
  ;; Objects are both of tag-misc.  Headers must match exactly;
  ;; dispatch on subtag.
  (getvheader imm1 y)
  (extract-lowbyte imm2 imm1)
  (getvheader imm0 x)
  (cmp imm2 (:$ arm::subtag-macptr))
  (beq @macptr)
  (cmp imm0 imm1)
  (bne @lose)
  (lri imm1 (logior (ash 1 target::tag-fixnum)
                    (ash 1 target::subtag-bignum)
                    (ash 1 target::subtag-single-float)
                    (ash 1 target::subtag-double-float)
                    (ash 1 target::subtag-ratio)
                    (ash 1 target::subtag-complex)))
  (mov imm0 (:$ 1))
  (tst imm1 (:lsl imm0 imm2))
  (getvheader imm0 x)
  (getvheader imm1 y)
  (beq @lose)
  (cmp imm2 (:$ arm::subtag-ratio))
  (cmpne imm2 (:$ arm::subtag-complex))
  (beq @node)
  (cmp imm2 (:$ arm::subtag-bignum))
  (beq @bignum)
  (cmp imm2 (:$ arm::subtag-double-float))
  (bne @one-unboxed-word)
  ;; This is the double-float case.
  (ldr imm0 (:@ x (:$ arm::double-float.val-low)))
  (ldr imm1 (:@ y (:$ arm::double-float.val-low)))
  (cmp imm0 imm1)
  (ldreq imm0 (:@ x (:$ arm::double-float.val-high)))
  (ldreq imm1 (:@ y (:$ arm::double-float.val-high)))
  (cmpeq imm0 imm1)
  (mov arg_z 'nil)
  (addeq arg_z arg_z (:$ arm::t-offset))
  (bx lr)
  @win
  (mov arg_z 'nil)
  (add arg_z arg_z (:$ arm::t-offset))
  (bx lr)
  @macptr
  (extract-lowbyte imm0 imm0)
  (cmp imm2 imm0)
  (bne @lose)
  @one-unboxed-word
  (ldr imm0 (:@ x (:$ arm::misc-data-offset)))
  (ldr imm1 (:@ y (:$ arm::misc-data-offset)))
  (cmp imm0 imm1)
  (beq  @win)
  @lose
  (mov arg_z 'nil)
  (bx lr)
  @bignum
  ;; Way back when, we got x's header into imm0.  We know that y's
  ;; header is identical.  Use the element-count from imm0 to control
  ;; the loop.  There's no such thing as a 0-element bignum, so the
  ;; loop must always execute at least once.
  (header-length temp0 imm0)
  (mov imm2 (:$ arm::misc-data-offset))
  @bignum-next
  (ldr imm0 (:@ x imm2))
  (ldr imm1 (:@ y imm2))
  (cmp imm0 imm1)
  (add imm2 imm2 (:$ arm::node-size))
  (bne @lose)
  (subs temp0 temp0 '1)
  (bne @bignum-next)
  (mov arg_z 'nil)
  (add arg_z arg_z (:$ arm::t-offset))
  (bx lr)
  @node
  ;; Have either a ratio or a complex.  In either case, corresponding
  ;; elements of both objects must be EQL.  Recurse on the first
  ;; elements.  If true, tail-call on the second, else fail.
  (vpush1 x)
  (vpush1 y)
  (build-lisp-frame imm0)
  (ldr x (:@ x (:$ arm::misc-data-offset)))
  (ldr y (:@ y (:$ arm::misc-data-offset)))
  (bl @tail)
  (cmp arg_z 'nil)
  (restore-lisp-frame imm0)
  (vpop1 y)
  (vpop1 x)
  (beq @lose)
  (ldr x (:@ x (:$ (+ 4 arm::misc-data-offset))))
  (ldr y (:@ y (:$ (+ 4 arm::misc-data-offset))))
  (b @tail))


  

(defarmlapfunction equal ((x arg_y) (y arg_z))
  "Return T if X and Y are EQL or if they are structured components
  whose elements are EQUAL. Strings and bit-vectors are EQUAL if they
  are the same length and have identical components. Other arrays must be
  EQ to be EQUAL.  Pathnames are EQUAL if their components are."
  (check-nargs 2)
  @top
  (cmp x y)
  (extract-fulltag imm0 x)
  (extract-fulltag imm1 y)
  (beq @win)
  (cmp imm0 imm1)
  (bne @lose)
  (cmp imm0 (:$ arm::fulltag-cons))
  (beq @cons)
  (cmp imm0 (:$ arm::fulltag-misc))
  (beq @misc)
  @lose
  (mov arg_z 'nil)
  (bx lr)
  @win
  (mov arg_z 'nil)
  (add arg_z arg_z (:$ arm::t-offset))
  (bx lr)
  @cons
  (%car temp0 x)
  (%car temp1 y)
  (cmp temp0 temp1)
  (bne @recurse)
  (%cdr x x)
  (%cdr y y)
  (b @top)
  @recurse
  (vpush1 x)
  (vpush1 y)
  (build-lisp-frame imm0)
  (mov fn nfn)
  (mov x temp0)
  (mov y temp1)
  (bl @top)
  (cmp arg_z 'nil)  
  (mov nfn fn)
  (restore-lisp-frame imm0)           ; gets old fn to fn  
  (vpop1 y)
  (vpop1 x)
  (beq  @lose)
  (%cdr x x)
  (%cdr y y)
  (b @top)
  @misc
  (extract-subtag imm0 x)
  (extract-subtag imm1 y)
  (cmp imm0 (:$ arm::subtag-vectorH))
  (cmpne imm1 (:$ arm::subtag-vectorH))
  (beq @hairy)
  (cmp imm0 (:$ arm::subtag-macptr))
  (bgt @same)
  (set-nargs 2)
  (ldr fname (:@ nfn 'eql))
  (ldr nfn (:@ fname (:$ arm::symbol.fcell)))
  (ldr pc (:@ nfn (:$ arm::function.entrypoint)))
  @same
  (cmp imm1 imm0)
  (bne @lose)
  @hairy
  (set-nargs 2)
  (ldr fname (:@ nfn 'hairy-equal))
  (ldr nfn (:@ fname (:$ arm::symbol.fcell)))
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))
