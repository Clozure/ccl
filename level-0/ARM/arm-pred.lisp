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
  (spjump .SPbuiltin-eql))


  

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
