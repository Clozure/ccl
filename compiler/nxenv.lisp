;;; -*- Mode: Lisp; Package: CCL -*-
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


;;; Compile-time environment for the compiler.


(in-package "CCL")

(eval-when (:execute :compile-toplevel)
  (require'backquote)
  (require 'lispequ)
)

#+ppc-target (require "PPCENV")
#+x8632-target (require "X8632ENV")
#+x8664-target (require "X8664ENV")

;

(defconstant $vbittemporary 16)    ; a compiler temporary
(defconstant $vbitreg 17)          ; really wants to live in a register.
(defconstant $vbitnoreg 18)        ; something inhibits register allocation
(defconstant $vbitdynamicextent 19)
(defconstant $vbitparameter 20)    ; iff special
(defconstant $vbitpunted 20)       ; iff lexical
(defconstant $vbitignoreunused 21)
(defconstant $vbitignorable 21)
(defconstant $vbitcloseddownward 22)  
(defconstant $vbitsetq 23)
(defconstant $vbitpuntable 24)
(defconstant $vbitclosed 25)
(defconstant $vbitignore 26)
(defconstant $vbitreffed 27)
(defconstant $vbitspecial 28)
(defconstant $vsetqmask #xff00)
(defconstant $vrefmask #xff)

(defconstant $decl_optimize (%ilsl 16 0))  ; today's chuckle
(defconstant $decl_tailcalls (ash 1 16))
(defconstant $decl_opencodeinline (ash 4 16))
(defconstant $decl_eventchk (ash 8 16))
(defconstant $decl_unsafe (ash 16 16))
(defconstant $decl_trustdecls (ash 32 16))
(defconstant $decl_full_safety (ash 64 16))

(defconstant $regnote-ea 1)

(defmacro nx-null (x)
 `(eq ,x *nx-nil*))

(defmacro nx-t (x)
 `(eq ,x *nx-t*))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defconstant operator-id-mask (1- (%ilsl 10 1)))
  (defconstant operator-acode-subforms-bit 10)
  (defconstant operator-acode-subforms-mask (%ilsl operator-acode-subforms-bit 1))
  (defconstant operator-acode-list-bit 11)
  (defconstant operator-acode-list-mask (%ilsl operator-acode-list-bit 1))
  (defconstant operator-side-effect-free-bit 12) ; operator is side-effect free; subforms may not be ...
  (defconstant operator-side-effect-free-mask 
    (%ilsl operator-side-effect-free-bit 1))
  (defconstant operator-single-valued-bit 13)
  (defconstant operator-single-valued-mask
    (%ilsl operator-single-valued-bit 1))
  (defconstant operator-assignment-free-bit 14)
  (defconstant operator-assignment-free-mask
    (%ilsl operator-assignment-free-bit 1))
  (defconstant operator-cc-invertable-bit 15)
  (defconstant operator-cc-invertable-mask (ash 1 operator-cc-invertable-bit))
  (defconstant operator-boolean-bit 16)
  (defconstant operator-boolean-mask (ash 1 operator-boolean-bit))
  (defconstant operator-returns-address-bit 17)
  (defconstant operator-returns-address-mask (ash 1 operator-returns-address-bit))

  )

(defparameter *next-nx-operators*
  (reverse
   '((%primitive . 0)
     (progn . #.(logior operator-acode-list-mask operator-assignment-free-mask operator-side-effect-free-mask))
     (not . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (%i+ . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%i- . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (cxxr . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%dfp-combine . 0)
     (%ilsl . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%ilogand2 . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%ilogior2 . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%ilogbitp . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (eq . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (neq . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (list . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-list-mask operator-side-effect-free-mask))
     (values . #.(logior operator-acode-list-mask operator-assignment-free-mask operator-side-effect-free-mask))
     (if . #.(logior operator-acode-subforms-mask operator-side-effect-free-mask))
     (or . 0)
     (without-interrupts . 0)
     (%fixnum-ref . #.operator-single-valued-mask)
     (%fixnum-ref-natural . #.operator-single-valued-mask)
     (%current-tcr . #.operator-single-valued-mask)
     (%stack-trap . #.operator-single-valued-mask)
     (multiple-value-prog1 . 0)
     (multiple-value-bind . 0)
     (multiple-value-call . 0)
     (put-xxx . #.operator-single-valued-mask)
     (get-xxx . #.operator-single-valued-mask)
     (typed-form . 0)
     (let . 0)
     (let* . 0)
     (tag-label . 0)
     (local-tagbody . #.operator-single-valued-mask)
     (%fixnum-set-natural . #.operator-single-valued-mask)
     (spushl . #.operator-single-valued-mask)
     (spushp . #.operator-single-valued-mask)
     (simple-function . #.operator-single-valued-mask)
     (closed-function . #.operator-single-valued-mask)
     (setq-lexical . #.operator-single-valued-mask)
     (lexical-reference . #.(logior operator-assignment-free-mask operator-single-valued-mask))
     (free-reference . #.(logior operator-assignment-free-mask operator-single-valued-mask))
     (immediate . #.(logior operator-assignment-free-mask operator-single-valued-mask))
     (fixnum . #.(logior operator-assignment-free-mask operator-single-valued-mask ))
     (call . 0)
     (local-go . 0)
     (local-block . 0)
     (local-return-from . 0)
     (%car . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%cdr . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%rplaca . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (%rplacd . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (cons . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask))
     (simple-typed-aref2 . #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask))
     (setq-free . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (prog1 . 0)
     (catch . 0)
     (throw . 0)
     (unwind-protect . 0)
     (characterp . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (multiple-value-list . 0)
     (%izerop . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (%immediate-ptr-to-int . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%immediate-int-to-ptr . #.(logior operator-returns-address-mask operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (immediate-get-xxx . 0)
     (immediate-put-xxx . 0)
     (setq-special . 0)
     (special-ref . 0)
     (1+ . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (1- . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (add2 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (sub2 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (numeric-comparison . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-cc-invertable-mask))
     (numcmp . #.(logior operator-assignment-free-mask operator-acode-subforms-mask operator-single-valued-mask operator-cc-invertable-mask))
     (struct-ref . 0)
     (struct-set . 0)
     (%aref1 . #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask))
     (embedded-nlexit . 0)
     (embedded-conditional . 0) 
     (%word-to-int . #.(logior operator-assignment-free-mask operator-single-valued-mask))
     (%svref . #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask))
     (%svset . #.(logior operator-acode-subforms-mask operator-single-valued-mask))
     (%consmacptr% . 0)
     (%macptrptr% . 0)
     (%ptr-eql . #.operator-cc-invertable-mask)
     (%setf-macptr . 0)
     (bound-special-ref . 0)
     (%char-code . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%code-char . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (lap . 0)
     (lap-inline . 0)
     (%function . #.operator-single-valued-mask)
     (%valid-code-char . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%ttag . #.operator-single-valued-mask)  
     (uvsize . #.operator-single-valued-mask)
     (endp . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (sequence-type . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (fixnum-overflow . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (vector . #.(logior operator-assignment-free-mask operator-single-valued-mask))
     (%immediate-inc-ptr . #.(logior operator-returns-address-mask operator-single-valued-mask))
     (general-aref3 . #.(logior operator-acode-subforms-mask operator-single-valued-mask))
     (general-aset2 . #.(logior operator-acode-subforms-mask operator-single-valued-mask))
     (%new-ptr . 0)
     (%schar . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%set-schar . #.(logior operator-single-valued-mask operator-acode-subforms-mask))	;??
     (debind . 0)
     (lambda-bind . 0)
     (general-aset3 . #.(logior operator-acode-subforms-mask operator-single-valued-mask))
     (simple-typed-aref3 . #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask))
     (simple-typed-aset3 . #.(logior operator-acode-subforms-mask  operator-single-valued-mask))
     (nth-value . 0)
     (progv . 0)
     (svref . #.(logior operator-assignment-free-mask operator-single-valued-mask))
     (svset . #.operator-single-valued-mask)
     (make-list . #.(logior operator-assignment-free-mask operator-single-valued-mask))	; exists only so we can stack-cons
     (%badarg1 . 0)
     (%badarg2 . 0)
     (newblocktag . 0)
     (newgotag . 0)
     (flet . 0)				; may not be necessary - for dynamic-extent, mostly
					; for dynamic-extent, forward refs, etc.
     (labels . 0)			; removes 75% of LABELS bogosity
     (lexical-function-call . 0)	; most of other 25%
     (with-downward-closures . 0)
     (self-call . 0)
     (inherited-arg . #.operator-single-valued-mask)
     (ff-call . 0)
     (commutative-subprim-binop . 0)
     (%immediate-set-xxx . #.(logior operator-acode-subforms-mask))
     (symbol-name . #.(logior operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (memq . #.(logior operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (assq . #.(logior operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (simple-typed-aset2 . #.(logior operator-acode-subforms-mask operator-single-valued-mask))
     (consp . #.(logior operator-cc-invertable-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-boolean-mask))
     (aset1 . #.(logior operator-acode-subforms-mask))
     (syscall . 0)
     (car . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (cdr . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (length . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (list-length . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (ensure-simple-string . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%ilsr . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (set . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (eql . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-boolean-mask))
     (%iasr . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (logand2 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (logior2 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (logxor2 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%i<> . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (set-car . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (set-cdr . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (rplaca . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (rplacd . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (with-variable-c-frame . #.(logior operator-acode-list-mask operator-assignment-free-mask))
     (uvref . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (uvset . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (%temp-cons . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%temp-List . #.(logior operator-single-valued-mask operator-side-effect-free-mask))
     (%make-uvector . #.(logior operator-assignment-free-mask operator-single-valued-mask  operator-side-effect-free-mask))
     (%decls-body . 0)
     (%old-gvector . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%typed-uvref . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%typed-uvset . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (schar . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (set-schar . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (code-char . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (char-code . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (list* . #.(logior operator-assignment-free-mask operator-single-valued-mask  operator-side-effect-free-mask))
     (append . #.(logior operator-assignment-free-mask operator-single-valued-mask  operator-side-effect-free-mask))
     (symbolp . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-boolean-mask))
     (integer-point-h . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (integer-point-v . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (int>0-p . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (immediate-constant .  #.(logior operator-assignment-free-mask operator-single-valued-mask ))
     (with-stack-double-floats . 0)
     (short-float . #.operator-single-valued-mask)
     (istruct-typep . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (%ilogxor2 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%err-disp . 0)
     (%quo2 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (minus1 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%ineg . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%i* . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (logbitp . #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-boolean-mask))
     (%sbchar . 0)
     (%sechar . 0)
     (%set-sbchar . 0)
     (%scharcode . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%set-scharcode . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (lambda-list . 0)
     (ppc-lap-function . 0)
     (lisptag . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (fulltag . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (typecode . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-simple-vector . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-simple-string . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-integer . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-fixnum . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-real . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-list . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-character . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-number . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-symbol . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (base-char-p . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (%vect-subtype . #.operator-single-valued-mask)
     (%unbound-marker . #.operator-single-valued-mask)
     (%slot-unbound-marker . #.operator-single-valued-mask)
     (%gvector . #.(logior operator-assignment-free-mask operator-single-valued-mask))
     (immediate-get-ptr . #.operator-returns-address-mask)
     (%lisp-word-ref . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (%lisp-lowbyte-ref . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (poweropen-ff-call . 0)
     (double-float-compare . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (builtin-call . 0)
     (%setf-double-float . 0)
     (%double-float+-2 . 0)
     (%double-float--2 . 0)
     (%double-float*-2 . 0)
     (%double-float/-2 . 0)
     (%double-float+-2! . 0)
     (%double-float--2! . 0)
     (%double-float*-2! . 0)
     (%double-float/-2! . 0)
     (poweropen-syscall . 0)
     (%debug-trap . 0)
     (%%ineg . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%setf-short-float . 0)
     (%short-float+-2 . 0)
     (%short-float--2 . 0)
     (%short-float*-2 . 0)
     (%short-float/-2 . 0)
     (short-float-compare . 0)
     (eabi-ff-call . 0)
     (%reference-external-entry-point . 0)
     (eabi-syscall . 0)
     (%get-bit . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%set-bit   . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (%natural+ . 0)
     (%natural- . 0)
     (%natural-logand . 0)
     (%natural-logior . 0)
     (%natural-logxor . 0)
     (%natural<> . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask))
     (%get-double-float . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%get-single-float . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%set-double-float . #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
      (%set-single-float . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (natural-shift-right  . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (natural-shift-left  . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (global-ref . 0)
     (global-setq . 0)
     (disable-interrupts . #.(logior operator-assignment-free-mask operator-single-valued-mask))

     (%interrupt-poll  . #.(logior operator-assignment-free-mask operator-single-valued-mask))
     (with-c-frame . #.(logior operator-acode-list-mask operator-assignment-free-mask operator-side-effect-free-mask))    
     (%current-frame-ptr . 0)
     (%slot-ref . 0)
     (%illegal-marker . #.operator-single-valued-mask)
     (%symbol->symptr . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%single-to-double  . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%double-to-single . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%symptr->symvector  . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%symvector->symptr  . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%foreign-stack-pointer . 0)
     (mul2 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (div2 . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%fixnum-to-single  . #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (%fixnum-to-double .  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask))
     (require-s8 . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-u8 . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-s16 . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-u16 . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-s32 . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-u32 . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-s64 . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (require-u64 . #.(logior operator-single-valued-mask operator-acode-subforms-mask))
     (general-aref2 .  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask))
     (%single-float .  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask))
     (%double-float . #. #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask))
     (i386-ff-call . 0)
     (i386-syscall . 0))))

(defmacro %nx1-operator (sym)
  (let ((op (assq sym *next-nx-operators*)))
    (if op (logior (%cdr op) (length (%cdr (memq op *next-nx-operators*))))
        (error "Bug - operator not found for ~S" sym))))

(declaim (special *nx1-alphatizers* *nx1-operators*))

(defmacro %nx1-default-operator ()
 #-bccl
 `(nx1-default-operator)
 #+bccl
 `(gethash *nx-sfname* *nx1-operators*))

(defmacro defnx1 (name sym arglist &body forms)
  (let ((fn `(nfunction ,name ,(parse-macro name arglist forms)))
        (theprogn ())
        (ysym (gensym)))
    `(let ((,ysym ,fn))
       ,(if (symbolp sym)
          `(progn
             (setf (gethash ',sym *nx1-alphatizers*) ,ysym)
             ;(proclaim '(inline ,sym))
             (pushnew ',sym *nx1-compiler-special-forms*))
          (dolist (x sym `(progn ,@(nreverse theprogn)))
            (if (consp x)
              (setq x (%car x))
              (push `(pushnew ',x *nx1-compiler-special-forms*) theprogn))
            ;(push `(proclaim '(inline ,x)) theprogn)
            (push `(setf (gethash ',x *nx1-alphatizers*) ,ysym) theprogn)))
       (record-source-file ',name 'function)
       ,ysym)))

(defmacro next-nx-num-ops ()
  (length *next-nx-operators*))

(defmacro next-nx-defops (&aux (ops (gensym)) 
                                (num (gensym)) 
                                (flags (gensym)) 
                                (op (gensym)))
  `(let ((,num ,(length *next-nx-operators*)) 
         (,ops ',*next-nx-operators*) 
         (,flags nil)
         (,op nil))
     (while ,ops
       (setq ,op (%car ,ops)  ,flags (cdr ,op))
       (setf (gethash (car ,op) *nx1-operators*) 
             (logior ,flags (setq ,num (%i- ,num 1))))
       (setq ,ops (cdr ,ops)))))

(defconstant $fbitnextmethargsp 0)
(defconstant $fbitmethodp 1)
(defconstant $fbitnextmethp 2)
(defconstant $fbitnoregs 3)
(defconstant $fbitdownward 4)
(defconstant $fbitresident 5)
(defconstant $fbitbounddownward 6)
(defconstant $fbitembeddedlap 7)
(defconstant $fbitruntimedef 8)
(defconstant $fbitnonnullenv 9)
(defconstant $fbitccoverage 10)

(defconstant $eaclosedbit 24)

#+what?
(progn
;;; condition codes :
;;; These are 68K condition code values, but the frontend uses them and
;;; both backends need to understand them.
;;; They're really backend-specific; it wouldn't hurt to have the frontend
;;; use a more "neutral" representation.
(defconstant $ccT 0)
(defconstant $ccEQ 7)
(defconstant $ccNE 6)
(defconstant $ccVC 8)
(defconstant $ccMI 11)
(defconstant $ccPL 10)
(defconstant $ccGE 12)
(defconstant $ccLT 13)
(defconstant $ccGT 14)
(defconstant $ccLE 15)
)


(defmacro %temp-push (value place &environment env)
  (if (not (consp place))
    `(setq ,place (%temp-cons ,value ,place))
    (multiple-value-bind (dummies vals store-var setter getter)
                         (get-setf-expansion place env)
      (let ((valvar (gensym)))
        `(let* ((,valvar ,value)
                ,@(mapcar #'list dummies vals)
                (,(car store-var) (%temp-cons ,valvar ,getter)))
           ,@dummies
           ,(car store-var)
           ,setter)))))

; undo tokens :

(defconstant $undocatch 0)  ; do some nthrowing
(defconstant $undovalues 1) ; flush pending multiple values
(defconstant $undostkblk 2) ; discard "variable stack block"
(defconstant $undospecial 3) ; restore dynamic binding
(defconstant $undointerruptlevel 4) ; restore dynamic binding of *interrupt-level*
(defconstant $undomvexpect 5) ; stop expecting values
(defconstant $undoregs 6)   ; allocated regs when dynamic extent var bound.

; Stuff having to do with lisp:

(defmacro make-acode (operator &rest args)
  `(%temp-list ,operator ,@args))

(defmacro make-acode* (operator &rest args)
  `(%temp-cons ,operator (mapcar #'nx1-form ,@args)))

; More Bootstrapping Shit.
(defmacro acode-operator (form)
  ; Gak.
  `(%car ,form))

(defmacro acode-operand (n form)
  ; Gak. Gak.
  `(nth ,n (the list ,form)))

(defmacro acode-p (x)
  " A big help this is ..."
  `(consp ,x))


(defmacro defnxdecl (sym lambda-list &body forms)
  (multiple-value-bind (body decls) (parse-body forms nil t)
    `(setf (getf *nx-standard-declaration-handlers* ',sym )
           (function (lambda ,lambda-list
                       ,@decls
                       ,@body)))))

(defmacro with-declarations ((pending new-env-var &optional old-env) &body body)
  `(let* ((,pending (make-pending-declarations))
          (,new-env-var (new-lexical-environment ,old-env)))
     ,@body))

(defmacro with-nx-declarations ((pending) &body body)
  `(let* ((*nx-new-p2decls* nil)
	  (*nx-inlined-self* *nx-inlined-self*))
    (with-declarations (,pending *nx-lexical-environment* *nx-lexical-environment*)
      ,@body)))


(eval-when (:compile-toplevel :load-toplevel :execute)

(declaim (inline 
          nx-decl-set-fbit
          nx-adjust-setq-count
          nx-init-var
          nx1-sysnode
          ))

(defun nx-init-var (state node)
  (let* ((sym (var-name node))
         (env *nx-lexical-environment*)
         (bits (%i+
                (if (nx-proclaimed-special-p sym)
                 (if (nx-proclaimed-parameter-p sym)
                   (%ilogior (ash -1 $vbitspecial) (%ilsl $vbitparameter 1))
                   (ash -1 $vbitspecial))
                 0)
                (if (proclaimed-ignore-p sym) (%ilsl $vbitignore 1) 0))))
    (push node (lexenv.variables env))
    (%temp-push node *nx-all-vars*)
    (setf (var-binding-info node) *nx-bound-vars*)
    (%temp-push node *nx-bound-vars*)
    (dolist (decl (nx-effect-vdecls state sym env) (setf (var-bits node) bits))
      (case (car decl)
        (special (setq bits (%ilogior bits (ash -1 $vbitspecial) (%ilsl $vbitparameter 1))))
        (ignore (setq bits (%ilogior bits (%ilsl $vbitignore 1))))
        (ignore-if-unused (setq bits (%ilogior bits (%ilsl $vbitignoreunused 1))))
        (dynamic-extent (setq bits (%ilogior bits (%ilsl $vbitdynamicextent 1))))))
    node))

(defun nx-decl-set-fbit (bit)
  (when *nx-parsing-lambda-decls*
    (let* ((afunc *nx-current-function*))
      (setf (afunc-bits afunc)
            (%ilogior (%ilsl bit 1)
                      (afunc-bits afunc))))))

(defun nx-adjust-setq-count (var &optional (by 1) catchp)
  (let* ((bits (nx-var-bits var))
         (scaled-by (if (%ilogbitp $vbittemporary bits)
                      by
                      (expt 4 *nx-loop-nesting-level*)))
         (new (%i+ (%ilsr 8 (%ilogand2 $vsetqmask bits)) scaled-by)))
    (if (%i> new 255) (setq new 255))
    (setq bits (nx-set-var-bits var (%ilogior (%ilogand (%ilognot $vsetqmask) bits) (%ilsl 8 new))))
; If a variable is setq'ed from a catch nested within the construct that
; bound it, it can't be allocated to a register. *
; * unless it can be proved that the variable isn't referenced
;   after that catch construct has been exited. **
; ** or unless the saved value of the register in the catch frame 
;    is also updated.
    (when catchp
      (nx-set-var-bits var (%ilogior2 bits (%ilsl $vbitnoreg 1))))
    new))


(defun nx1-sysnode (form)
  (if form
    (if (eq form t)
      *nx-t*)
    *nx-nil*))
)

(provide "NXENV")

