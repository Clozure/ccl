;;; -*- Mode: Lisp; Package: CCL -*-
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


;;; Compile-time environment for the compiler.


(in-package "CCL")

(eval-when (:execute :compile-toplevel)
  (require'backquote)
  (require 'lispequ)
)

(def-accessors (acode) %svref
  nil                                   ; 'acode
  acode.operator                        ; fixnum
  acode.operands                        ; list, elements often acode
  acode.asserted-type                   ; NIL or type specifier.
  acode.info                            ; plist: notes, etc
  )
  
(def-accessors (var) %svref
  nil                                   ; 'var
  var-name                              ; symbol
  (var-bits var-parent)                 ; fixnum or ptr to parent
  (var-ea  var-expansion)               ; p2 address (or symbol-macro expansion)
  var-ref-forms                         ; in intermediate-code
  var-type
  var-binding-info
  var-refs
  var-nvr
  var-declared-type                     ; 
  var-root-nrefs                        ; reference count of "root" var
  var-root-nsetqs                       ; setq count of root var
  var-initform                          ; initial value acode or NIL.
  var-local-bits
)

(defconstant $vlocalbitiveacrosscall 0) ;
(defconstant $vlocalbitargument 1)
(defconstant $vlocalbitregisterarg 2)   ;
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

(defconstant $decl_optimize (%ilsl 16 0))  ; today's chuckle
(defconstant $decl_tailcalls (ash 1 16))
(defconstant $decl_opencodeinline (ash 4 16))
(defconstant $decl_eventchk (ash 8 16))
(defconstant $decl_unsafe (ash 16 16))
(defconstant $decl_trustdecls (ash 32 16))
(defconstant $decl_full_safety (ash 64 16))
(defconstant $decl_float_safety (ash 128 16))

(defconstant $regnote-ea 1)

(defmacro nx-null (x)
 `(%nx-null ,x))

(defmacro nx-t (x)
 `(%nx-t ,x))

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
   '(()
     (progn  #.(logior operator-acode-list-mask operator-assignment-free-mask operator-side-effect-free-mask) :infer)
     (not  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (%i+  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (%i-  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (fixnum-add-no-overflow  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask)fixnum)
     (ash  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (%ilsl  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (%ilogand2  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (%ilogior2  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (%ilogbitp  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (eq  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (neq  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (list  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-list-mask operator-side-effect-free-mask) list)
     (values  #.(logior operator-acode-list-mask operator-assignment-free-mask operator-side-effect-free-mask) t)
     (if  #.(logior operator-acode-subforms-mask operator-side-effect-free-mask) :infer)
     (or  0 :infer)
     (fixnum-add-overflow  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (%fixnum-ref  #.(logior operator-single-valued-mask operator-acode-subforms-mask) t)
     (%fixnum-ref-natural  #.(logior operator-single-valued-mask operator-acode-subforms-mask) natural)
     (%current-tcr  #.operator-single-valued-mask fixnum)
     (%ilognot  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask ) fixnum)
     (multiple-value-prog1  0 :infer)
     (multiple-value-bind  0 :infer)
     (multiple-value-call  0 :infer) 
     (%complex-single-float-realpart  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) single-float)
     (%complex-single-float-imagpart  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) single-float)
     (typed-form  0 :infer)
     (let  0 :infer)
     (let*  0 :infer)
     (tag-label  0 nil)
     (local-tagbody  #.operator-single-valued-mask null)
     (%complex-double-float-realpart  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) double-float)
     (type-asserted-form  0 :infer)
     (fixnum-ash   #.(logior operator-single-valued-mask operator-assignment-free-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (simple-function  #.operator-single-valued-mask function)
     (closed-function  #.operator-single-valued-mask function)
     (setq-lexical  #.operator-single-valued-mask :infer)
     (lexical-reference  #.(logior operator-assignment-free-mask operator-single-valued-mask) :infer)
     (free-reference  #.(logior operator-assignment-free-mask operator-single-valued-mask) :infer)
     (immediate  #.(logior operator-assignment-free-mask operator-single-valued-mask) :infer)
     (fixnum  #.(logior operator-assignment-free-mask operator-single-valued-mask ) :infer)
     (call  0 :infer)
     (local-go  0 nil)
     (local-block  0 :infer)
     (local-return-from  0 :infer)
     (%car  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) :infer)
     (%cdr  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) :infer)
     (%rplaca  #.(logior operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (%rplacd  #.(logior operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (cons  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask) cons)
     (simple-typed-aref2  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask) :infer)
     (setq-free  #.(logior operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (prog1  0 :infer)
     (catch  #.operator-acode-subforms-mask :infer)
     (throw  #.operator-acode-subforms-mask nil)
     (unwind-protect  0 t)
     (characterp  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (multiple-value-list  #.operator-acode-subforms-mask list)
     (%izerop  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (%immediate-ptr-to-int  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) natural)
     (%immediate-int-to-ptr  #.(logior operator-returns-address-mask operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) macptr)
     (immediate-get-xxx  0 :infer)
     (%complex-double-float-imagpart  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) double-float)
     (setq-special  0 :infer)
     (special-ref  #.operator-single-valued-mask :infer)
     (realpart #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) real)
     (imagpart #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) real)
     (add2  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) number)
     (sub2  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) number)
     (%make-complex-single-float #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) (complex single-float))
     (numcmp  #.(logior operator-assignment-free-mask operator-acode-subforms-mask operator-single-valued-mask operator-cc-invertable-mask) boolean)
     (struct-ref  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask operator-side-effect-free-mask) :infer)
     (struct-set  #.(logior operator-acode-subforms-mask operator-single-valued-mask) :infer)
     (%aref1  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask operator-side-effect-free-mask) :infer)
     (nil  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-side-effect-free-mask) null)
     (t  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-side-effect-free-mask) boolean)
     (%word-to-int  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask) fixnum)
     (%svref  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask) :infer)
     (%svset  #.(logior operator-acode-subforms-mask operator-single-valued-mask) :infer)
     (%consmacptr%  #.operator-acode-subforms-mask macptr)
     (%macptrptr%  #.operator-acode-subforms-mask macptr)
     (%ptr-eql  #.(logior operator-cc-invertable-mask operator-acode-subforms-mask) boolean)
     (%setf-macptr  #.operator-acode-subforms-mask macptr)
     (bound-special-ref  #.operator-single-valued-mask :infer)
     (%char-code  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) (unsigned-byte 8))
     (%code-char  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) character)
     (%make-complex-double-float #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) (complex double-float))
     (complex #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) :infer)
     (%function  #.operator-single-valued-mask function)
     (%valid-code-char  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) character)
     ()
     (uvsize  #.(logior operator-single-valued-mask operator-acode-subforms-mask) index)
     (endp  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (sequence-type  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (fixnum-overflow  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (vector  #.(logior operator-assignment-free-mask operator-single-valued-mask) simple-vector)
     (%immediate-inc-ptr  #.(logior operator-returns-address-mask operator-single-valued-mask operator-acode-subforms-mask) macptr)
     (general-aref3  #.(logior operator-acode-subforms-mask operator-single-valued-mask) :infer)
     (general-aset2  #.(logior operator-acode-subforms-mask operator-single-valued-mask) :infer)
     (%new-ptr  #.operator-acode-subforms-mask macptr)
     (%schar  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) character)
     (%set-schar  #.(logior operator-single-valued-mask operator-acode-subforms-mask) character)	;??
     ()
     (lambda-bind  0 :infer)
     (general-aset3  #.(logior operator-acode-subforms-mask operator-single-valued-mask) :infer)
     (simple-typed-aref3  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask) :infer)
     (simple-typed-aset3  #.(logior operator-acode-subforms-mask  operator-single-valued-mask) :infer)
     (nth-value  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (progv  #.operator-acode-subforms-mask :infer)
     (svref  #.(logior operator-assignment-free-mask operator-single-valued-mask) :infer)
     (svset  #.(logior operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (make-list  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask) list)	; exists only so we can stack-cons
     (%badarg1  #.operator-acode-subforms-mask nil)
     (%badarg2  #.operator-acode-subforms-mask nil)
     (%fixnum-ref-double-float  #.(logior operator-acode-subforms-mask  operator-single-valued-mask) double-float)
     (%fixnum-set-double-float  #.(logior operator-acode-subforms-mask  operator-single-valued-mask) double-float)
     (flet  0 :infer)				; may not be necessary - for dynamic-extent, mostly
					; for dynamic-extent, forward refs, etc.
     (labels  0 :infer)			; removes 75% of LABELS bogosity
     (lexical-function-call  0 :infer)	; most of other 25%
     ()
     (self-call  0 :infer)
     (inherited-arg  #.operator-single-valued-mask :infer)     
     (ff-call  0 :infer)
     ()
     (%immediate-set-xxx  #.(logior operator-acode-subforms-mask) :infer)
     (symbol-name  #.(logior operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) simple-base-string)
     (memq  #.(logior operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) list)
     (assq  #.(logior operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask) list)
     (simple-typed-aset2  #.(logior operator-acode-subforms-mask operator-single-valued-mask) :infer)
     (consp  #.(logior operator-cc-invertable-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-boolean-mask) boolean)
     (aset1  #.(logior operator-acode-subforms-mask) :infer)
     ()
     (car  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) :infer)
     (cdr  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) :infer)
     (length  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (list-length  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) t)
     (ensure-simple-string  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) simple-base-string)
     (%ilsr  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (set  #.(logior operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (eql  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-boolean-mask) boolean)
     (%iasr  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (logand2  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (logior2  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (logxor2  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (%i<>  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (set-car  #.(logior operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (set-cdr  #.(logior operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (rplaca  #.(logior operator-single-valued-mask operator-acode-subforms-mask) cons)
     (rplacd  #.(logior operator-single-valued-mask operator-acode-subforms-mask) cons)
     (with-variable-c-frame  #.(logior operator-acode-list-mask operator-assignment-free-mask) :infer)
     (uvref  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) :infer)
     (uvset  #.(logior operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (%temp-cons  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) cons)
     (%temp-List  #.(logior operator-single-valued-mask operator-side-effect-free-mask) list)
     (%make-uvector  #.(logior operator-assignment-free-mask operator-single-valued-mask  operator-side-effect-free-mask operator-acode-subforms-mask) :infer)
     (%decls-body  0 :infer)
     (%old-gvector  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) :infer)
     (%typed-uvref  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) :infer)
     (%typed-uvset  #.(logior operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (schar  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) character)
     (set-schar  #.(logior operator-single-valued-mask operator-acode-subforms-mask) character)
     (code-char  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) character)
     (char-code  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) (mod #.char-code-limit))
     (list*  #.(logior operator-assignment-free-mask operator-single-valued-mask  operator-side-effect-free-mask) :infer)
     (ivector-typecode-p  #.(logior operator-assignment-free-mask operator-single-valued-mask  operator-side-effect-free-mask operator-acode-subforms-mask) fixnum)
     (symbolp  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-boolean-mask) boolean)
     (fixnum-sub-no-overflow  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (fixnum-sub-overflow  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (int>0-p  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (gvector-typecode-p  #.(logior operator-assignment-free-mask operator-single-valued-mask  operator-side-effect-free-mask operator-acode-subforms-mask) fixnum)
     ()
     ()
     (istruct-typep  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (%ilogxor2  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (%err-disp  0 nil)
     (%quo2  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) number)
     (minus1  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) number)
     (%ineg  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) integer)
     (%i*  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (logbitp  #.(logior operator-single-valued-mask operator-assignment-free-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-boolean-mask) boolean)
     (%sbchar  0 character)
     ()
     (%set-sbchar  #.(logior operator-single-valued-mask operator-acode-subforms-mask) character)
     (%scharcode  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) (mod #.char-code-limit))
     (%set-scharcode  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (mod #.char-code-limit))
     (lambda-list  0 :infer)
     ()
     (lisptag  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (unsigned-byte 8))
     (fulltag  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (unsigned-byte 8))
     (typecode  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (unsigned-byte 8))
     (require-simple-vector  #.(logior operator-single-valued-mask operator-acode-subforms-mask) simple-vector)
     (require-simple-string  #.(logior operator-single-valued-mask operator-acode-subforms-mask) simple-base-string)
     (require-integer  #.(logior operator-single-valued-mask operator-acode-subforms-mask) integer)
     (require-fixnum  #.(logior operator-single-valued-mask operator-acode-subforms-mask) fixnum)
     (require-real  #.(logior operator-single-valued-mask operator-acode-subforms-mask) real)
     (require-list  #.(logior operator-single-valued-mask operator-acode-subforms-mask) list)
     (require-character  #.(logior operator-single-valued-mask operator-acode-subforms-mask) character)
     (require-number  #.(logior operator-single-valued-mask operator-acode-subforms-mask) number)
     (require-symbol  #.(logior operator-single-valued-mask operator-acode-subforms-mask) symbol)
     (base-char-p  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     ()
     (%unbound-marker  #.operator-single-valued-mask t)
     (%slot-unbound-marker  #.operator-single-valued-mask t)
     (%gvector  #.(logior operator-assignment-free-mask operator-single-valued-mask) :infer)
     (immediate-get-ptr  #.(logior operator-returns-address-mask operator-acode-subforms-mask) macptr)
     (%lisp-word-ref  #.(logior operator-single-valued-mask operator-acode-subforms-mask) t)
     (%lisp-lowbyte-ref  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (unsigned-byte 8))
     (poweropen-ff-call  0 :infer)
     (double-float-compare  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (builtin-call  0 :infer)
     (%setf-double-float  #.(logior operator-single-valued-mask operator-acode-subforms-mask) double-float)
     (%double-float+-2  #.(logior operator-single-valued-mask operator-side-effect-free-mask operator-acode-subforms-mask) double-float)
     (%double-float--2   #.(logior operator-single-valued-mask operator-side-effect-free-mask operator-acode-subforms-mask) double-float)
     (%double-float*-2   #.(logior operator-single-valued-mask operator-side-effect-free-mask operator-acode-subforms-mask) double-float)
     (%double-float/-2   #.(logior operator-single-valued-mask operator-side-effect-free-mask operator-acode-subforms-mask) double-float)
     ()
     ()
     ()
     ()
     ()
     (%debug-trap  #.operator-acode-subforms-mask t)
     (%%ineg  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) fixnum)
     (%setf-short-float  #.(logior operator-single-valued-mask operator-acode-subforms-mask) single-float)
     (%short-float+-2   #.(logior operator-single-valued-mask operator-side-effect-free-mask operator-acode-subforms-mask) single-float)
     (%short-float--2   #.(logior operator-single-valued-mask operator-side-effect-free-mask operator-acode-subforms-mask) single-float)
     (%short-float*-2   #.(logior operator-single-valued-mask operator-side-effect-free-mask operator-acode-subforms-mask) single-float)
     (%short-float/-2   #.(logior operator-single-valued-mask operator-side-effect-free-mask operator-acode-subforms-mask) single-float)
     (short-float-compare  #.operator-acode-subforms-mask boolean)
     (eabi-ff-call  0 :infer)
     (%reference-external-entry-point  #.operator-acode-subforms-mask t)
     ()
     (%get-bit  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) bit)
     (%set-bit    #.(logior operator-single-valued-mask operator-acode-subforms-mask) bit)
     (%natural+   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) natural)
     (%natural-   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) natural)
     (%natural-logand   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) natural)
     (%natural-logior  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) natural)
     (%natural-logxor   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) natural)
     (%natural<>  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask operator-cc-invertable-mask) boolean)
     (%get-double-float  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) double-float)
     (%get-single-float  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) single-float)
     (%set-double-float  #.(logior operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) double-float)
      (%set-single-float  #.(logior operator-single-valued-mask operator-acode-subforms-mask) single-float)
     (natural-shift-right   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) natural)
     (natural-shift-left   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) natural)
     (global-ref  0 :infer)
     (global-setq  0 :infer)
     ()
     (%interrupt-poll   #.(logior operator-assignment-free-mask operator-single-valued-mask) nil)
     (with-c-frame  #.(logior operator-acode-list-mask operator-assignment-free-mask operator-side-effect-free-mask):infer)    
     (%current-frame-ptr  0 fixnum)
     (%slot-ref  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask) :infer)
     (%illegal-marker  #.operator-single-valued-mask t)
     (%symbol->symptr  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) t)
     (%single-to-double   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) double-float)
     (%double-to-single  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) single-float)
     (%symptr->symvector   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) t)
     (%symvector->symptr   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) t)
     (%foreign-stack-pointer  0 fixnum)
     (mul2  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) number)
     (div2  #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) number)
     (%fixnum-to-single   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) single-float)
     (%fixnum-to-double   #.(logior operator-assignment-free-mask operator-single-valued-mask operator-acode-subforms-mask operator-side-effect-free-mask) double-float)
     (require-s8  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (signed-byte 8))
     (require-u8  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (unsigned-byte 8))
     (require-s16  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (signed-byte 16))
     (require-u16  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (unsigned-byte 16))
     (require-s32  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (signed-byte 32))
     (require-u32  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (unsigned-byte 32))
     (require-s64  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (signed-byte 64))
     (require-u64  #.(logior operator-single-valued-mask operator-acode-subforms-mask) (unsigned-byte 64))
     (general-aref2   #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask) :infer)
     (%single-float   #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask) single-float)
     (%double-float  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask) double-float)
     (i386-ff-call  0 :infer)
     ()
     (%double-float-negate  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask) double-float)
     (%single-float-negate  #.(logior operator-acode-subforms-mask operator-assignment-free-mask operator-single-valued-mask) single-float) )))

(defmacro %nx1-operator (sym)
  (let ((op (assq sym *next-nx-operators*)))
    (if op (logior (cadr op) (length (%cdr (memq op *next-nx-operators*))))
        (error "Bug - operator not found for ~S" sym))))

;;; For debugging ...
(defun acode-operator-name (op)
  (car (nth (- (1- (length *next-nx-operators*))
               (logand op operator-id-mask))
            *next-nx-operators*)))

(declaim (special *nx1-alphatizers* *nx1-operators* *acode-operator-types*))

(defmacro %nx1-default-operator ()
  `(nx1-default-operator))



(defmacro next-nx-num-ops ()
  (length *next-nx-operators*))

(defmacro next-nx-defops (&aux (ops (gensym)) 
                                (num (gensym))
                                (name (gensym))
                                (flags (gensym))
                                (type (gensym))
                                (op (gensym)))
  (dolist (def *next-nx-operators*)
    (when def
      (destructuring-bind (name flags &optional (type t type-p)) def
        (declare (ignore name flags))
        (unless (and type-p
                     (or (eq type :infer)
                         (specifier-type-if-known type)))
          (warn "Suspect operator type definition in ~s" def)))))
  `(let ((,num ,(length *next-nx-operators*)) 
         (,ops ',*next-nx-operators*) 
         (,op nil))
     (while ,ops
       (setq ,op (%car ,ops) ,num (%i- ,num 1))
       (when ,op
         (destructuring-bind (,name ,flags ,type) ,op
         (setf (gethash ,name *nx1-operators*) 
               (logior ,flags ,num)
               (svref *acode-operator-types* ,num)
               ,type)))
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
(defconstant $fbittailcallsself 11)

(defconstant $eaclosedbit 24)

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

(defmacro make-acode* (operator operands)
  `(%istruct 'acode ,operator ,operands nil nil))

(defmacro make-acode (operator &rest args)
  `(make-acode* ,operator (list ,@args)))




(defmacro acode-operator (form)
  `(the fixnum (acode.operator ,form)))


(defmacro acode-operands (form)
  `(the list (acode.operands ,form)))

(defmacro acode-p (x)
  `(istruct-typep ,x 'acode))




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
        ((ignorable ignore-if-unused) (setq bits (%ilogior bits (%ilsl $vbitignoreunused 1))))
        (dynamic-extent (setq bits (%ilogior bits (%ilsl $vbitdynamicextent 1))))
        (type (let* ((type (cdr decl))
                     (ctype (specifier-type-if-known type env)))
                (when ctype (setf (var-declared-type node)
                                  (type-specifier ctype)))))))
    node))

(defun nx-decl-set-fbit (bit)
  (when *nx-parsing-lambda-decls*
    (let* ((afunc *nx-current-function*))
      (setf (afunc-bits afunc)
            (%ilogior (%ilsl bit 1)
                      (afunc-bits afunc))))))

(defun nx-adjust-setq-count (var &optional (by 1) catchp)
  (let* ((bits (nx-var-bits var))
         (nsetqs (nx-var-root-nsetqs var))
         (scaled-by (if (%ilogbitp $vbittemporary bits)
                      by
                      (expt 4 *nx-loop-nesting-level*)))
         (new (+ (var-refs var) scaled-by)))
    (nx-set-var-root-nsetqs var (1+ nsetqs))
    ;; If a variable is setq'ed from a catch nested within the construct that
    ;; bound it, it can't be allocated to a register. *
    ;; * unless it can be proved that the variable isn't referenced
    ;;   after that catch construct has been exited. **
    ;; ** or unless the saved value of the register in the catch frame 
    ;;    is also updated.
    (when catchp
      (nx-set-var-bits var (%ilogior2 bits (%ilsl $vbitnoreg 1))))
    (setf (var-refs var) new)    
    new))



)

(defmacro make-mask (&rest weights)
  `(logior ,@(mapcar #'(lambda (w) `(ash 1 ,w)) weights)))



(provide "NXENV")

