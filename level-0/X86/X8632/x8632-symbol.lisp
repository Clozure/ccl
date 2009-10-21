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
  (require "X8632-ARCH")
  (require "X86-LAPMACROS"))

;;; This assumes that macros & special-operators
;;; have something that's not FUNCTIONP in their
;;; function-cells.  It also assumes that NIL
;;; isn't a true symbol, but that NILSYM is.
(defx8632lapfunction %function ((sym arg_z))
  (check-nargs 1)
  (let ((symaddr temp0))
    (movl ($ (+ (target-nil-value) x8632::nilsym-offset)) (% symaddr))
    (cmp-reg-to-nil sym)
    (cmovne (% sym) (% symaddr))
    (trap-unless-typecode= symaddr x8632::subtag-symbol)
    (movl (% sym) (% arg_y))
    (movl (@ x8632::symbol.fcell (% symaddr)) (% arg_z))
    (extract-typecode arg_z imm0)
    (cmpb ($ x8632::subtag-function) (%b imm0))
    (je.pt @ok)
    (uuo-error-udf (% arg_y))
    @ok
    (single-value-return)))

;;; Traps unless sym is NIL or some other symbol.  If NIL, return
;;; nilsym
(defx8632lapfunction %symbol->symptr ((sym arg_z))
  (let ((tag imm0))
    (movl ($ (+ (target-nil-value) x8632::nilsym-offset)) (% tag))
    (cmp-reg-to-nil sym)
    (cmove (% tag) (% sym))
    (je :done)
    (trap-unless-typecode= sym x8632::subtag-symbol)
    :done
    (single-value-return)))

;;; If symptr is NILSYM, return NIL; else typecheck and return symptr
(defx8632lapfunction %symptr->symbol ((symptr arg_z))
  (cmpl ($ (+ (target-nil-value) x8632::nilsym-offset)) (% symptr))
  (jne @typecheck)
  (movl ($ (target-nil-value)) (% arg_z))
  (single-value-return)
  @typecheck
  (trap-unless-typecode= symptr x8632::subtag-symbol)
  (single-value-return))

(defx8632lapfunction %symptr-value ((symptr arg_z))
  (jmp-subprim .SPspecref))

(defx8632lapfunction %set-symptr-value ((symptr arg_y) (val arg_z))
  (jmp-subprim .SPspecset))

;;; This gets a tagged symbol as an argument.
;;; If there's no thread-local binding, it should return
;;; the underlying symbol vector as a first return value.
(defx8632lapfunction %symptr-binding-address ((symptr arg_z))
  (movl (@ x8632::symbol.binding-index (% symptr)) (% arg_y))
  (rcmp (% arg_y) (:rcontext x8632::tcr.tlb-limit))
  (movl (:rcontext x8632::tcr.tlb-pointer) (% temp0))
  (jae @sym)
  (cmpb ($ x8632::subtag-no-thread-local-binding) (@ (% temp0) (% arg_y)))
  (je @sym)
  (shl ($ x8632::word-shift) (% arg_y))
  (push (% temp0))
  (push (% arg_y))
  (set-nargs 2)
  (lea (@ '2 (% esp)) (% temp0))
  (jmp-subprim .SPvalues)
  @sym
  (push (% arg_z))
  (pushl ($ '#.x8632::symbol.vcell))
  (set-nargs 2)
  (lea (@ '2 (% esp)) (% temp0))
  (jmp-subprim .SPvalues))

(defx8632lapfunction %tcr-binding-location ((tcr arg_y) (sym arg_z))
  (movl (@ x8632::symbol.binding-index (% sym)) (% temp0))
  (movl ($ (target-nil-value)) (% arg_z))
  (rcmp (% temp0) (@ x8632::tcr.tlb-limit (% tcr)))
  (movl (@ x8632::tcr.tlb-pointer (% tcr)) (% arg_y))
  (jae @done)
  (lea (@ (% arg_y) (% temp0)) (% arg_y))
  ;; We're little-endian, so the tag is at the EA with no
  ;; displacement
  (cmpb ($ x8632::subtag-no-thread-local-binding) (@ (% arg_y)))
  (cmovnel (% arg_y) (% arg_z))
  @done
  (single-value-return))

(defx86lapfunction %pname-hash ((str arg_y) (len arg_z))
  (let ((accum imm0)
        (offset temp0))
    (xor (% offset) (% offset))
    (xor (% accum) (% accum))
    (testl (% len) (% len))
    (jz.pn @done)
    @loop8
    (roll ($ 5) (%l accum))
    (xorl (@ x8632::misc-data-offset (% str) (% offset)) (%l accum))
    (addl ($ '1) (% offset))
    (subl ($ '1) (% len))
    (jnz @loop8)
    (shll ($ 5) (% accum))
    (shrl ($ (- 5 x8632::fixnumshift)) (% accum))
    (movl (% accum) (% arg_z))
    @done
    (single-value-return)))

(defx8632lapfunction %string-hash ((start 4) #|(ra 0)|# (str arg_y) (len arg_z))
  (let ((accum imm0)
        (offset temp0))
    (movl (@ start (% esp)) (% offset))
    (xorl (% accum) (% accum))
    (testl (% len) (% len))
    (jz @done)
    @loop8
    (roll ($ 5) (%l accum))
    (xorl (@ x8632::misc-data-offset (% str) (% offset)) (%l accum))
    (addl ($ '1) (% offset))
    (subl ($ '1) (% len))
    (jnz @loop8)
    (shll ($ 5) (% accum))
    (shrl ($ (- 5 x8632::fixnumshift)) (% accum))
    (movl (% accum) (% arg_z))
    @done
    (single-value-return 3)))
