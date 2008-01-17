;;;-*- Mode: Lisp; Package: (PPC :use CL) -*-
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

(defpackage "PPC"
  (:use "CL"))

(require "ARCH")

(in-package "PPC")
;;; Lisp registers.
(eval-when (:compile-toplevel :execute)
  (defmacro defregs (&body regs)
    `(progn
       (ccl::defenum () ,@regs)
       (defparameter *gpr-register-names* ,(coerce (mapcar #'string regs) 'vector))))
  (defmacro deffpregs (&body regs)
    `(progn
       (ccl::defenum () ,@regs)
       (defparameter *fpr-register-names* ,(coerce (mapcar #'string regs) 'vector))))
  (defmacro defvregs (&body regs)
    `(progn
      (ccl::defenum () ,@regs)
      (defparameter *vector-register-names* ,(coerce (mapcar #'string regs) 'vector))
      )))

(defregs
  rzero                                 ; Always contains 0; not as handy as it sounds.
  sp					; The control stack.  Aligned on 16-byte boundary.
  target-1                              ; volatile reg on Darwin, tp or TOC on Linux.
  imm0                                  ; Unboxed, volatile registers.
  imm1 
  imm2 
  imm3 
  imm4
  imm5
  allocptr
  allocbase
  nargs                                 ; Volatile.  SHOULDN'T be used for tag extraction. (TWI handler confusion.)
  tsp                                   ; Temp-stack pointer.
  target-2
  loc-pc                                ; for return PC only.
  vsp                                   ; Value stack pointer; grows towards 0.
  fn                                    ; Current function (constants vector).
  temp3                                 ; Boxed, volatile registers.  Some
					; may be defined on function entry.
  temp2 
  temp1 
  temp0 
  arg_x                                 ; Next-to-next-to-last function arg.
  arg_y                                 ; Next-to-last function argument.
  arg_z                                 ; Last function argument.
  save7                                 ; Boxed, nonvolatile registers.
  save6
  save5
  save4 
  save3 
  save2 
  save1 
  save0
  )

(deffpregs 
  fp0
  fp1
  fp2
  fp3
  fp4
  fp5
  fp6
  fp7
  fp8
  fp9
  fp10
  fp11
  fp12
  fp13
  fp14
  fp15
  fp16
  fp17
  fp18
  fp19
  fp20
  fp21
  fp22
  fp23
  fp24
  fp25
  fp26
  fp27
  fp28
  fp29
  fp30
  fp31)

(defvregs
  vr0					; General temp vector register
  vr1					; Most-significant quadword when word-aligning
  vr2					; Least-significant quadword when word-aligning
  vr3					; Operand A resulting from word-aligning
  vr4					; Operand B resulting from word-aligning
  vr5					; Result from operations on A and B
  vr6
  vr7
  vr8
  vr9
  vr10
  vr11
  vr12
  vr13
  vr14
  vr15
  vr16
  vr17
  vr18
  vr19
  ;;By convention, registers after this point are considered non-volatile. Callee should save.
  vr20
  vr21
  vr22
  vr23
  vr24
  vr25
  vr26
  vr27					; Permutation control register A for loads
  vr28					; Permutation control register B for stores
  vr29					; mask register
  vr30					; All zeros
  vr31					; All ones
  )



;;; Calling sequence may pass additional arguments in temp registers.
;;; "nfn" (new function) is always passed; it's the new value of "fn".
(defconstant nfn temp2)

;;; CLOS may pass the context for, e.g.., CALL-NEXT-METHOD in 
;;;; the "next-method-context" register.
(defconstant next-method-context temp1)


;;; It's handy to have 0.0 in an fpr.
(defconstant fp-zero fp31)

; Also handy to have #x4330000080000000 in an fpr, for s32->float conversion.
(defconstant fp-s32conv fp30)

(defconstant fname temp3)

;;; Calling sequence may pass additional arguments in temp registers.
;;; "nfn" (new function) is always passed; it's the new value of "fn".
(defconstant nfn temp2)

;;; CLOS may pass the context for, e.g.., CALL-NEXT-METHOD in 
;;;; the "next-method-context" register.
(defconstant next-method-context temp1)


;;; It's handy to have 0.0 in an fpr.
(defconstant fp-zero fp31)

; Also handy to have #x4330000080000000 in an fpr, for s32->float conversion.
(defconstant fp-s32conv fp30)

(ccl::defenum (:prefix "FPSCR-" :suffix "-BIT")
  fx
  fex
  vx
  ox
  ux
  zx
  xx
  vxsnan
  vxisi
  vxidi
  vxzdz
  vximz
  vxvc
  fr
  fi
  fprfc
  fl
  fg
  fe
  fu
  nil
  vxsoft
  vxsqrt
  vxcvi
  ve
  oe
  ue
  ze
  xe
  ni
  rn0
  rn1
)

(ccl::defenum (:prefix "PPC-" :suffix "-BIT")
  lt
  gt
  eq
  so
)

;;; Kernel globals are allocated "below" nil.  This list (used to map
;;; symbolic names to rnil-relative offsets) must (of course) exactly
;;; match the kernel's notion of where things are.
;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" & the
;;; lisp_globals record in "ccl:lisp-kernel;*constants*.s"
(defparameter *ppc-kernel-globals*
  '(get-tcr				; callback to obtain (real) tcr
    tcr-count
    interrupt-signal			; used by PROCESS-INTERRUPT
    kernel-imports                      ; some things we need to have imported for us.
    tcr-lock
    emulator-registers                  ; Where the 68K registers are kept.
    appmain                             ; application's (c-runtime) main() function
    subprims-base                       ; start of dynamic subprims jump table
    ret1valaddr                         ; magic multiple-values return address.
    tcr-key                             ; tsd key for thread's tcr
    area-lock                           ; serialize access to gc
    exception-lock			; serialize exception handling
    static-conses                       ; when FREEZE is in effect
    default-allocation-quantum          ; log2_heap_segment_size, as a fixnum.
    intflag				; interrupt-pending flag
    gc-inhibit-count                    ; for gc locking
    refbits                             ; oldspace refbits
    oldspace-dnode-count                ; number of dnodes in dynamic space that are older than
                                        ; youngest generation
    altivec-present                     ; non-zero if cpu supports AltiVec 
    fwdnum                              ; fixnum: GC "forwarder" call count.
    gc-count                            ; fixnum: GC call count.
    gcable-pointers                     ; linked-list of weak macptrs.
    heap-start                          ; start of lisp heap
    heap-end                            ; end of lisp heap
    statically-linked                   ; true if the lisp kernel is statically linked
    stack-size                          ; value of --stack-size arg
    bad-current-ts                      ; current temp-stack area
    bad-cs-overflow-limit               ; limit for control-stack overflow check
    all-areas                           ; doubly-linked area list
    lexpr-return                        ; multiple-value lexpr return address
    lexpr-return1v                      ; single-value lexpr return address
    in-gc                               ; non-zero when GC-ish thing active
    metering-info                       ; kernel metering structure
    doh-head                            ; creole
    short-float-zero                    ; low half of 1.0d0
    double-float-one                    ; high half of 1.0d0
    ffi-exception                       ; ffi fpscr[fex] bit
    exception-saved-registers           ; saved registers from exception frame
    oldest-ephemeral                    ; doublenode address of oldest ephemeral object or 0
    tenured-area                        ; the tenured_area.
    errno                               ; address of C lib errno
    argv                                ; address of C lib argv
    host-platform                       ; 0 on MacOS, 1 on PPC Linux, 2 on VxWorks ...
    batch-flag				; non-zero if --batch specified
    BAD-fpscr-save			; lisp's fpscr when in FFI-land
    BAD-fpscr-save-high  		; high word of FP reg used to save FPSCR
    image-name				; current image name
    initial-tcr                         ; initial thread's context record
    ))

;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" and the nrs record
;;; in "ccl:lisp-kernel;constants.s".
(defparameter *ppc-nil-relative-symbols*
  '(t
    nil
    ccl::%err-disp
    ccl::cmain
    eval
    ccl::apply-evaluated-function
    error    
    ccl::%defun
    ccl::%defvar
    ccl::%defconstant
    ccl::%macro
    ccl::%kernel-restart
    *package*
    ccl::*total-bytes-freed*
    :allow-other-keys    
    ccl::%toplevel-catch%
    ccl::%toplevel-function%
    ccl::%pascal-functions%    
    ccl::*all-metered-functions*
    ccl::*total-gc-microseconds*
    ccl::%builtin-functions%
    ccl::%unbound-function%
    ccl::%init-misc
    ccl::%macro-code%
    ccl::%closure-code%
    ccl::%new-gcable-ptr
    ccl::*gc-event-status-bits*
    ccl::*post-gc-hook*
    ccl::%handlers%
    ccl::%all-packages%
    ccl::*keyword-package* 
    ccl::%finalization-alist%
    ccl::%foreign-thread-control
    ))

;;; Old (and slightly confusing) name; NIL used to be in a register.
(defparameter *ppc-nilreg-relative-symbols* *ppc-nil-relative-symbols*)





(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *ppc-subprims-shift* 2)
(defparameter *ppc-subprims-base* (ash 5 12) )
)

;;; For now, nothing's nailed down and we don't say anything about
;;; registers clobbered.
;;; These are shared between ppc32 and ppc64.
(let* ((origin *ppc-subprims-base*)
       (step (ash 1 *ppc-subprims-shift*)))
  (flet ((define-ppc-subprim (name)
             (ccl::make-subprimitive-info :name (string name)
                                          :offset (prog1 origin
                                                    (incf origin step)))))
    (macrolet ((defppcsubprim (name)
                   `(define-ppc-subprim ',name)))
      (defparameter *ppc-subprims*
        (vector
         (defppcsubprim .SPjmpsym)
         (defppcsubprim .SPjmpnfn)
         (defppcsubprim .SPfuncall)
         (defppcsubprim .SPmkcatch1v)
         (defppcsubprim .SPmkunwind)
         (defppcsubprim .SPmkcatchmv)
         (defppcsubprim .SPthrow)
         (defppcsubprim .SPnthrowvalues)
         (defppcsubprim .SPnthrow1value)
         (defppcsubprim .SPbind)
         (defppcsubprim .SPbind-self)
         (defppcsubprim .SPbind-nil)
         (defppcsubprim .SPbind-self-boundp-check)
         (defppcsubprim .SPrplaca)
         (defppcsubprim .SPrplacd)
         (defppcsubprim .SPconslist)
         (defppcsubprim .SPconslist-star)
         (defppcsubprim .SPstkconslist)
         (defppcsubprim .SPstkconslist-star)
         (defppcsubprim .SPmkstackv)
         (defppcsubprim .SPsubtag-misc-ref)
         (defppcsubprim .SPsetqsym)
         (defppcsubprim .SPprogvsave)
         (defppcsubprim .SPstack-misc-alloc)
         (defppcsubprim .SPgvector)
         (defppcsubprim .SPnvalret)
         (defppcsubprim .SPmvpass)
         (defppcsubprim .SPfitvals)
         (defppcsubprim .SPnthvalue)
         (defppcsubprim .SPvalues)
         (defppcsubprim .SPdefault-optional-args)
         (defppcsubprim .SPopt-supplied-p)
         (defppcsubprim .SPheap-rest-arg)
         (defppcsubprim .SPreq-heap-rest-arg)
         (defppcsubprim .SPheap-cons-rest-arg)
         (defppcsubprim .SPsimple-keywords)
         (defppcsubprim .SPkeyword-args)
         (defppcsubprim .SPkeyword-bind)
         (defppcsubprim .SPpoweropen-ffcall)
         (defppcsubprim .SParef2)
         (defppcsubprim .SPksignalerr)
         (defppcsubprim .SPstack-rest-arg)
         (defppcsubprim .SPreq-stack-rest-arg)
         (defppcsubprim .SPstack-cons-rest-arg)
         (defppcsubprim .SPpoweropen-callbackX)
         (defppcsubprim .SPcall-closure)
         (defppcsubprim .SPgetXlong)
         (defppcsubprim .SPspreadargz)
         (defppcsubprim .SPtfuncallgen)
         (defppcsubprim .SPtfuncallslide)
         (defppcsubprim .SPtfuncallvsp)
         (defppcsubprim .SPtcallsymgen)
         (defppcsubprim .SPtcallsymslide)
         (defppcsubprim .SPtcallsymvsp)
         (defppcsubprim .SPtcallnfngen)
         (defppcsubprim .SPtcallnfnslide)
         (defppcsubprim .SPtcallnfnvsp)
         (defppcsubprim .SPmisc-ref)
         (defppcsubprim .SPmisc-set)
         (defppcsubprim .SPstkconsyz)
         (defppcsubprim .SPstkvcell0)
         (defppcsubprim .SPstkvcellvsp)
         (defppcsubprim .SPmakestackblock)
         (defppcsubprim .SPmakestackblock0)
         (defppcsubprim .SPmakestacklist)
         (defppcsubprim .SPstkgvector)
         (defppcsubprim .SPmisc-alloc)
         (defppcsubprim .SPpoweropen-ffcallX)
         (defppcsubprim .SPgvset)
         (defppcsubprim .SPmacro-bind)
         (defppcsubprim .SPdestructuring-bind)
         (defppcsubprim .SPdestructuring-bind-inner)
         (defppcsubprim .SPrecover-values)
         (defppcsubprim .SPvpopargregs)
         (defppcsubprim .SPinteger-sign)
         (defppcsubprim .SPsubtag-misc-set)
         (defppcsubprim .SPspread-lexpr-z)
         (defppcsubprim .SPstore-node-conditional)
         (defppcsubprim .SPreset)
         (defppcsubprim .SPmvslide)
         (defppcsubprim .SPsave-values)
         (defppcsubprim .SPadd-values)
         (defppcsubprim .SPpoweropen-callback)
         (defppcsubprim .SPmisc-alloc-init)
         (defppcsubprim .SPstack-misc-alloc-init)
         (defppcsubprim .SPset-hash-key)
         (defppcsubprim .SPaset2)
         (defppcsubprim .SPcallbuiltin)
         (defppcsubprim .SPcallbuiltin0)
         (defppcsubprim .SPcallbuiltin1)
         (defppcsubprim .SPcallbuiltin2)
         (defppcsubprim .SPcallbuiltin3)
         (defppcsubprim .SPpopj)
         (defppcsubprim .SPrestorefullcontext)
         (defppcsubprim .SPsavecontextvsp)
         (defppcsubprim .SPsavecontext0)
         (defppcsubprim .SPrestorecontext)
         (defppcsubprim .SPlexpr-entry)
         (defppcsubprim .SPpoweropen-syscall)
         (defppcsubprim .SPbuiltin-plus)
         (defppcsubprim .SPbuiltin-minus)
         (defppcsubprim .SPbuiltin-times)
         (defppcsubprim .SPbuiltin-div)
         (defppcsubprim .SPbuiltin-eq)
         (defppcsubprim .SPbuiltin-ne)
         (defppcsubprim .SPbuiltin-gt)
         (defppcsubprim .SPbuiltin-ge)
         (defppcsubprim .SPbuiltin-lt)
         (defppcsubprim .SPbuiltin-le)
         (defppcsubprim .SPbuiltin-eql)
         (defppcsubprim .SPbuiltin-length)
         (defppcsubprim .SPbuiltin-seqtype)
         (defppcsubprim .SPbuiltin-assq)
         (defppcsubprim .SPbuiltin-memq)
         (defppcsubprim .SPbuiltin-logbitp)
         (defppcsubprim .SPbuiltin-logior)
         (defppcsubprim .SPbuiltin-logand)
         (defppcsubprim .SPbuiltin-ash)
         (defppcsubprim .SPbuiltin-negate)
         (defppcsubprim .SPbuiltin-logxor)
         (defppcsubprim .SPbuiltin-aref1)
         (defppcsubprim .SPbuiltin-aset1)
         (defppcsubprim .SPbreakpoint)
         (defppcsubprim .SPeabi-ff-call)
         (defppcsubprim .SPeabi-callback)
         (defppcsubprim .SPeabi-syscall)
         (defppcsubprim .SPgetu64)
         (defppcsubprim .SPgets64)
         (defppcsubprim .SPmakeu64)
         (defppcsubprim .SPmakes64)
         (defppcsubprim .SPspecref)
         (defppcsubprim .SPspecset)
         (defppcsubprim .SPspecrefcheck)
         (defppcsubprim .SPrestoreintlevel)
         (defppcsubprim .SPmakes32)
         (defppcsubprim .SPmakeu32)
         (defppcsubprim .SPgets32)
         (defppcsubprim .SPgetu32)
         (defppcsubprim .SPfix-overflow)
         (defppcsubprim .SPmvpasssym)
         (defppcsubprim .SParef3)
         (defppcsubprim .SPaset3)
         (defppcsubprim .SPpoweropen-ffcall-return-registers)
         (defppcsubprim .SPnmkunwind)
         (defppcsubprim .SPunused-6)
         (defppcsubprim .SPunbind-interrupt-level)
         (defppcsubprim .SPunbind)
         (defppcsubprim .SPunbind-n)
         (defppcsubprim .SPunbind-to)
         (defppcsubprim .SPbind-interrupt-level-m1)
         (defppcsubprim .SPbind-interrupt-level)
         (defppcsubprim .SPbind-interrupt-level-0)
         (defppcsubprim .SPprogvrestore)
         )))))


  
(provide "PPC-ARCH")
