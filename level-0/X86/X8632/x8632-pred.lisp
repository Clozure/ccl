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
  (require "X86-LAPMACROS"))

(defx8632lapfunction eql ((x arg_y) (y arg_z))
  "Return T if OBJ1 and OBJ2 represent either the same object or
numbers with the same type and value."
  (check-nargs 2)
  (jmp-subprim .SPbuiltin-eql))

(defx8632lapfunction equal ((x arg_y) (y arg_z))
  "Return T if X and Y are EQL or if they are structured components
  whose elements are EQUAL. Strings and bit-vectors are EQUAL if they
  are the same length and have identical components. Other arrays must be
  EQ to be EQUAL.  Pathnames are EQUAL if their components are."
  (check-nargs 2)
  @top
  @tail
  (cmp (% x) (% y))
  (je @win)
  (movl (% x) (% imm0))
  (andb ($ x8632::fulltagmask) (% al))
  (movb (% arg_z.b) (% ah))
  (andb ($ x8632::fulltagmask) (% ah))
  (cmpb (% al) (% ah))
  (jnz @lose)
  (cmpb ($ x8632::fulltag-cons) (% imm0.b))
  (je @cons)
  (cmpb ($ x8632::fulltag-misc) (% imm0.b))
  (je @misc)
  @lose
  (movl ($ (target-nil-value)) (% arg_z))
  (single-value-return)
  @win
  (movl ($ (target-t-value)) (% arg_z))
  (single-value-return)
  @cons
  ;; If either X or Y is NIL, lose.
  (cmp-reg-to-nil x)
  (je @lose)
  (cmp-reg-to-nil y)
  (je @lose)
  ;; Check to see if the CARs are EQ.  If so, we can avoid saving
  ;; context, and can just tail call ourselves on the CDRs.
  (%car x temp0)
  (%car y temp1)
  (cmpl (% temp0) (% temp1))
  (jne @recurse)
  (%cdr x x)
  (%cdr y y)
  (jmp @tail)
  @recurse
  (save-simple-frame)
  (pushl (@ x8632::cons.cdr (% x)))
  (pushl (@ x8632::cons.cdr (% y)))
  (movl (% temp0) (% x))
  (movl (% temp1) (% y))
  (:talign 5)
  (call @top)
  (recover-fn)
  (cmp-reg-to-nil arg_z)
  (pop (% y))
  (pop (% x))
  (restore-simple-frame)         
  (jnz @top)
  (movl ($ (target-nil-value)) (% arg_z))
  (single-value-return)
  @misc
  ;; Both objects are uvectors of some sort.  Try EQL; if that fails,
  ;; call HAIRY-EQUAL.
  (save-simple-frame)
  (pushl (% x))
  (pushl (% y))
  (call-symbol eql 2)
  (cmp-reg-to-nil arg_z)
  (jne @won-with-eql)
  (popl (% y))
  (popl (% x))
  (restore-simple-frame)
  (jump-symbol hairy-equal 2)
  @won-with-eql
  (restore-simple-frame)                ; discards pushed args
  (movl ($ (target-t-value)) (% arg_z))
  (single-value-return))

(defx8632lapfunction %lisp-lowbyte-ref ((thing arg_z))
  (box-fixnum thing arg_z)
  (andl ($ '#xff) (%l arg_z))
  (single-value-return))
