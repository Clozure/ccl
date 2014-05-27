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
  (beq @win)
  (extract-fulltag imm0 x)
  (extract-fulltag imm1 y)
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
  ;; If either a vector header, let HAIRY-EQUAL deal with them
  (beq @hairy)
  ;; If both are istructs (and potentially pathnames),
  ;; try HAIRY-EQUAL
  (cmp imm0 (:$ arm::subtag-istruct))
  (cmpeq imm1 (:$ arm::subtag-istruct))
  (beq @hairy)
  (getvheader imm0 x)
  (getvheader imm1 y)
  (cmp imm0 imm1)
  (spjumpne .SPbuiltin-eql)
  (and imm0 imm0 (:$ #xff))
  (cmp imm0 (:$ arm::subtag-bit-vector))
  (beq @bit-vector)
  (cmp imm0 (:$ arm::subtag-simple-base-string))
  (spjumpne .SPbuiltin-eql)
  (header-length temp0 imm1)
  @compare-words
  (mov imm2 (:$ arm::misc-data-offset))
  (b @string-next)
  @string-loop
  (ldr imm0 (:@ x imm2))
  (ldr imm1 (:@ y imm2))
  (cmp imm0 imm1)
  (bne @lose)
  (add imm2 imm2 (:$ arm::node-size))
  @string-next
  (subs temp0 temp0 (:$ arm::fixnumone))
  (bge @string-loop) 
  (mov arg_z 'nil)
  (add arg_z arg_z (:$ arm::t-offset))
  (bx lr)
  @hairy
  (set-nargs 2)
  (ldr fname (:@ nfn 'hairy-equal))
  (ldr nfn (:@ fname (:$ arm::symbol.fcell)))
  (ldr pc (:@ nfn (:$ arm::function.entrypoint)))
  @bit-vector
  ;; Go backwards through the bitvectors, comparing
  ;; each 32-bit word.  If the number of bits isn't
  ;; a multiple of 32, we have to mask the last words.
  (header-size imm2 imm1)
  (mov imm1 (:lsr imm2 (:$ 5)))
  (box-fixnum temp0 imm1)
  (ands imm2 imm2 (:$ 31))
  (beq @compare-words)
  (mov imm0 (:$ 1))
  (mov imm2 (:lsl imm0 imm2))
  (sub imm2 imm2 (:$ 1))
  (add imm1 temp0 (:$ arm::misc-data-offset))
  (ldr imm0 (:@ x imm1))
  (ldr imm1 (:@ y imm1))
  (and imm0 imm0 imm2)
  (and imm1 imm1 imm2)
  (cmp imm1 imm0)
  (beq @compare-words)
  (b @lose))




  
  
  
