;;;-*- Mode: Lisp; Package: (ARM :use CL) -*-
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

(defpackage "ARM"
  (:use "CL")
  #+arm-target
  (:nicknames "TARGET"))


(require "ARCH")

(in-package "ARM")


;;; Lisp registers.


(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *arm-register-names* ())


(defun get-arm-register (name)
  (let* ((pair (assoc (string name) *arm-register-names* :test #'string-equal)))
    (if pair
      (cdr pair))))

(defun get-arm-gpr (name)
  (let* ((value (get-arm-register name)))
    (and value (< value 16) value)))

(defun get-arm-sfpr (name)
  (let* ((value (get-arm-register name)))
    (and value (logbitp 5 value) (logand #x1f value))))

(defun get-arm-dfpr (name)
  (let* ((value (get-arm-register name)))
    (and value (logbitp 6 value) (logand #x0f value))))
  

;;; This allows redefinition, which might be helpful while
;;; boostrapping.  ARM-LAP-EQUATE-FORM checks for redefinition
;;; before calling this.
(defun define-arm-register (name val)
  (let* ((value (if (typep val 'fixnum) val (get-arm-register val)))
         (string (string name)))
    (unless value
      (error "invalid ARM register value ~d for ~s." val name))
    (let* ((pair (assoc string *arm-register-names* :test #'string-equal)))
      (if pair
        (progn
          (unless (eql (cdr pair) value)
            (when ccl::*cerror-on-constant-redefinition*
              (cerror "Redefine ARM register ~s to have value ~*~d."
                      "ARM register ~s currently has value ~d."
                      name (cdr pair) value)
              (setf (cdr pair) value))))
        (push (cons string value) *arm-register-names*))
        value)))

(defmacro defarmgpr (name val)
  `(defconstant ,name (define-arm-register ',name ',val)))

(defarmgpr r0 0)
(defarmgpr r1 1)
(defarmgpr r2 2)
(defarmgpr r3 3)
(defarmgpr r4 4)
(defarmgpr r5 5)
(defarmgpr r6 6)
(defarmgpr r7 7)
(defarmgpr r8 8)
(defarmgpr r9 9)
(defarmgpr r10 10)
(defarmgpr r11 11)
(defarmgpr r12 12)
(defarmgpr r13 13)
(defarmgpr r14 14)
(defarmgpr r15 15)

(defarmgpr imm0 r0)
(defarmgpr imm1 r1)
(defarmgpr imm2 r2)
(defarmgpr rcontext r3)
(defarmgpr arg_z r4)
(defarmgpr arg_y r5)
(defarmgpr arg_x r6)
(defarmgpr temp0 r7)
(defarmgpr temp1 r8)
(defarmgpr temp2 r9)
(defarmgpr vsp r10)
(defarmgpr fn r11)
(defarmgpr allocptr r12)
(defarmgpr sp r13)
(defarmgpr lr r14)
(defarmgpr pc r15)



;;; Calling sequence may pass additional arguments in temp registers.
;;; "nfn" (new function) is always passed; it's the new value of "fn".
(defarmgpr nfn temp2)
;;; CLOS may pass the context for, e.g.., CALL-NEXT-METHOD in 
;;;; the "next-method-context" register.
(defarmgpr next-method-context temp1)

(defarmgpr fname temp1)

(defarmgpr nargs imm2)

(defmacro defarmsfpr (name val)
  `(defconstant ,name (define-arm-register ',name ',val)))

(defarmsfpr s0 32)
(defarmsfpr s1 33)
(defarmsfpr s2 34)
(defarmsfpr s3 35)
(defarmsfpr s4 36)
(defarmsfpr s5 37)
(defarmsfpr s6 38)
(defarmsfpr s7 39)
(defarmsfpr s8 40)
(defarmsfpr s9 41)
(defarmsfpr s10 42)
(defarmsfpr s11 43)
(defarmsfpr s12 44)
(defarmsfpr s13 45)
(defarmsfpr s14 46)
(defarmsfpr s15 47)
(defarmsfpr s16 48)
(defarmsfpr s17 49)
(defarmsfpr s18 50)
(defarmsfpr s19 51)
(defarmsfpr s20 52)
(defarmsfpr s21 53)
(defarmsfpr s22 54)
(defarmsfpr s23 55)
(defarmsfpr s24 56)
(defarmsfpr s25 57)
(defarmsfpr s26 58)
(defarmsfpr s27 59)
(defarmsfpr s28 60)
(defarmsfpr s29 61)
(defarmsfpr s30 62)
(defarmsfpr s31 63)
(defarmsfpr single-float-zero s14)

;;; The first 16 double-float registers overlap pairs of single-float
;;; registers (d0 overlaps s0-s1, d15 overlaps s30-s31, etc.)

(defmacro defarmdfpr (name val)
  `(defconstant ,name (define-arm-register ',name ',val)))

(defarmdfpr d0 64)
(defarmdfpr d1 65)
(defarmdfpr d2 66)
(defarmdfpr d3 67)
(defarmdfpr d4 68)
(defarmdfpr d5 69)
(defarmdfpr d6 70)
(defarmdfpr d7 71)
(defarmdfpr d8 72)
(defarmdfpr d9 73)
(defarmdfpr d10 74)
(defarmdfpr d11 75)
(defarmdfpr d12 76)
(defarmdfpr d13 77)
(defarmdfpr d14 78)
(defarmdfpr d15 79)

(defarmdfpr double-float-zero d7)
)


(defparameter *standard-arm-register-names* *arm-register-names*)


;;; Kernel globals are allocated "below" nil.  This list (used to map
;;; symbolic names to rnil-relative offsets) must (of course) exactly
;;; match the kernel's notion of where things are.
;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" & the
;;; lisp_globals record in "ccl:lisp-kernel;*constants*.s"
(defparameter *arm-kernel-globals*
  '(get-tcr				; callback to obtain (real) tcr
    tcr-count
    interrupt-signal			; used by PROCESS-INTERRUPT
    kernel-imports                      ; some things we need to have imported for us.
    objc-2-personality
    savetoc                  ; used to save TOC on some platforms
    saver13                             ; used to save r13 on some platforms
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
    float-abi                           ; non-zero if using hard float abi
    fwdnum                              ; fixnum: GC "forwarder" call count.
    gc-count                            ; fixnum: GC call count.
    gcable-pointers                     ; linked-list of weak macptrs.
    heap-start                          ; start of lisp heap
    heap-end                            ; end of lisp heap
    statically-linked                   ; true if the lisp kernel is statically linked
    stack-size                          ; value of --stack-size arg
    objc-2-begin-catch                  ; objc_begin_catch
    kernel-path
    all-areas                           ; doubly-linked area list
    lexpr-return                        ; multiple-value lexpr return address
    lexpr-return1v                      ; single-value lexpr return address
    in-gc                               ; non-zero when GC-ish thing active
    free-static-conses                  ; fixnum
    objc-2-end-catch                    ; _objc_end_catch
    short-float-zero                    ; low half of 1.0d0
    double-float-one                    ; high half of 1.0d0
    static-cons-area                    ; 
    exception-saved-registers           ; saved registers from exception frame
    oldest-ephemeral                    ; doublenode address of oldest ephemeral object or 0
    tenured-area                        ; the tenured_area.
    errno                               ; address of C lib errno
    argv                                ; address of C lib argv
    host-platform                       ; 0 on MacOS, 1 on ARM Linux, 2 on VxWorks ...
    batch-flag				; non-zero if --batch specified
    unwind-resume			; _Unwind_Resume
    weak-gc-method                      ; weak gc algorithm.
    image-name				; current image name
    initial-tcr                         ; initial thread's context record
    weakvll                             ; all populations as of last GC
    ))

;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" and the nrs record
;;; in "ccl:lisp-kernel;constants.s".
(defparameter *arm-nil-relative-symbols*
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
    ccl::restore-lisp-pointers
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
    ccl::%os-init-function%
    ccl::%foreign-thread-control
    ))

;;; Old (and slightly confusing) name; NIL used to be in a register.
(defparameter *arm-nilreg-relative-symbols* *arm-nil-relative-symbols*)





(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *arm-subprims-shift* 2)
(defconstant tcr.sptab 256)
(defparameter *arm-subprims-base* tcr.sptab )
)
(defvar *arm-subprims*)


(let* ((origin *arm-subprims-base*)
       (step (ash 1 *arm-subprims-shift*)))
  (flet ((define-arm-subprim (name)
             (ccl::make-subprimitive-info :name (string name)
                                          :offset
                                          (prog1 origin
                                            (incf origin step)))))
    (macrolet ((defarmsubprim (name)
                   `(define-arm-subprim ',name)))
      (setq *arm-subprims*
            (vector
             (defarmsubprim .SPfix-nfn-entrypoint) ;must be first
             (defarmsubprim .SPbuiltin-plus)
             (defarmsubprim .SPbuiltin-minus)
             (defarmsubprim .SPbuiltin-times)
             (defarmsubprim .SPbuiltin-div)
             (defarmsubprim .SPbuiltin-eq)
             (defarmsubprim .SPbuiltin-ne)
             (defarmsubprim .SPbuiltin-gt)
             (defarmsubprim .SPbuiltin-ge)
             (defarmsubprim .SPbuiltin-lt)
             (defarmsubprim .SPbuiltin-le)
             (defarmsubprim .SPbuiltin-eql)
             (defarmsubprim .SPbuiltin-length)
             (defarmsubprim .SPbuiltin-seqtype)
             (defarmsubprim .SPbuiltin-assq)
             (defarmsubprim .SPbuiltin-memq)
             (defarmsubprim .SPbuiltin-logbitp)
             (defarmsubprim .SPbuiltin-logior)
             (defarmsubprim .SPbuiltin-logand)
             (defarmsubprim .SPbuiltin-ash)
             (defarmsubprim .SPbuiltin-negate)
             (defarmsubprim .SPbuiltin-logxor)
             (defarmsubprim .SPbuiltin-aref1)
             (defarmsubprim .SPbuiltin-aset1)
             (defarmsubprim .SPfuncall)
             (defarmsubprim .SPmkcatch1v)
             (defarmsubprim .SPmkcatchmv)
             (defarmsubprim .SPmkunwind)
             (defarmsubprim .SPbind)
             (defarmsubprim .SPconslist)
             (defarmsubprim .SPconslist-star)
             (defarmsubprim .SPmakes32)
             (defarmsubprim .SPmakeu32)
             (defarmsubprim .SPfix-overflow)
             (defarmsubprim .SPmakeu64)
             (defarmsubprim .SPmakes64)
             (defarmsubprim .SPmvpass)
             (defarmsubprim .SPvalues)
             (defarmsubprim .SPnvalret)
             (defarmsubprim .SPthrow)
             (defarmsubprim .SPnthrowvalues)
             (defarmsubprim .SPnthrow1value)
             (defarmsubprim .SPbind-self)
             (defarmsubprim .SPbind-nil)
             (defarmsubprim .SPbind-self-boundp-check)
             (defarmsubprim .SPrplaca)
             (defarmsubprim .SPrplacd)
             (defarmsubprim .SPgvset)
             (defarmsubprim .SPset-hash-key)
             (defarmsubprim .SPstore-node-conditional)
             (defarmsubprim .SPset-hash-key-conditional)
             (defarmsubprim .SPstkconslist)
             (defarmsubprim .SPstkconslist-star)
             (defarmsubprim .SPmkstackv)
             (defarmsubprim .SPsetqsym)
             (defarmsubprim .SPprogvsave)
             (defarmsubprim .SPstack-misc-alloc)
             (defarmsubprim .SPgvector)
             (defarmsubprim .SPfitvals)
             (defarmsubprim .SPnthvalue)
             (defarmsubprim .SPdefault-optional-args)
             (defarmsubprim .SPopt-supplied-p)
             (defarmsubprim .SPheap-rest-arg)
             (defarmsubprim .SPreq-heap-rest-arg)
             (defarmsubprim .SPheap-cons-rest-arg)
             (defarmsubprim .SPcheck-fpu-exception)
             (defarmsubprim .SPdiscard_stack_object)
             (defarmsubprim .SPksignalerr)
             (defarmsubprim .SPstack-rest-arg)
             (defarmsubprim .SPreq-stack-rest-arg)
             (defarmsubprim .SPstack-cons-rest-arg)
             (defarmsubprim .SPcall-closure)        
             (defarmsubprim .SPspreadargz)
             (defarmsubprim .SPtfuncallgen)
             (defarmsubprim .SPtfuncallslide)
             (defarmsubprim .SPjmpsym)
             (defarmsubprim .SPtcallsymgen)
             (defarmsubprim .SPtcallsymslide)
             (defarmsubprim .SPtcallnfngen)
             (defarmsubprim .SPtcallnfnslide)
             (defarmsubprim .SPmisc-ref)
             (defarmsubprim .SPsubtag-misc-ref)
             (defarmsubprim .SPmakestackblock)
             (defarmsubprim .SPmakestackblock0)
             (defarmsubprim .SPmakestacklist)
             (defarmsubprim .SPstkgvector)
             (defarmsubprim .SPmisc-alloc)
             (defarmsubprim .SPatomic-incf-node)
             (defarmsubprim .SPunused1)
             (defarmsubprim .SPunused2)
             (defarmsubprim .SPrecover-values)
             (defarmsubprim .SPinteger-sign)
             (defarmsubprim .SPsubtag-misc-set)
             (defarmsubprim .SPmisc-set)
             (defarmsubprim .SPspread-lexprz)
             (defarmsubprim .SPreset)
             (defarmsubprim .SPmvslide)
             (defarmsubprim .SPsave-values)
             (defarmsubprim .SPadd-values)
             (defarmsubprim .SPmisc-alloc-init)
             (defarmsubprim .SPstack-misc-alloc-init)
             (defarmsubprim .SPpopj)
             (defarmsubprim .SPudiv64by32)
             (defarmsubprim .SPgetu64)
             (defarmsubprim .SPgets64)
             (defarmsubprim .SPspecref)
             (defarmsubprim .SPspecrefcheck)
             (defarmsubprim .SPspecset)
             (defarmsubprim .SPgets32)
             (defarmsubprim .SPgetu32)
             (defarmsubprim .SPmvpasssym)
             (defarmsubprim .SPunbind)
             (defarmsubprim .SPunbind-n)
             (defarmsubprim .SPunbind-to)
             (defarmsubprim .SPprogvrestore)
             (defarmsubprim .SPbind-interrupt-level-0)
             (defarmsubprim .SPbind-interrupt-level-m1)
             (defarmsubprim .SPbind-interrupt-level)
             (defarmsubprim .SPunbind-interrupt-level)
             (defarmsubprim .SParef2)
             (defarmsubprim .SParef3)
             (defarmsubprim .SPaset2)
             (defarmsubprim .SPaset3)
             (defarmsubprim .SPkeyword-bind)
             (defarmsubprim .SPudiv32)
             (defarmsubprim .SPsdiv32)
             (defarmsubprim .SPeabi-ff-call)
             (defarmsubprim .SPdebind)
             (defarmsubprim .SPeabi-callback)
             (defarmsubprim .SPeabi-ff-callhf)
             )))))



  
(defmacro define-storage-layout (name origin &rest cells)
  `(progn
     (ccl::defenum (:start ,origin :step 4)
       ,@(mapcar #'(lambda (cell) (ccl::form-symbol name "." cell)) cells))
     (defconstant ,(ccl::form-symbol name ".SIZE") ,(* (length cells) 4))))
 
(defmacro define-lisp-object (name tagname &rest cells)
  `(define-storage-layout ,name ,(- (symbol-value tagname)) ,@cells))

(defmacro define-subtag (name tag subtag)
  `(defconstant ,(ccl::form-symbol "SUBTAG-" name) (logior ,tag (ash ,subtag ntagbits))))


(defmacro define-imm-subtag (name subtag)
  `(define-subtag ,name fulltag-immheader ,subtag))

(defmacro define-node-subtag (name subtag)
  `(define-subtag ,name fulltag-nodeheader ,subtag))

(defmacro define-fixedsized-object (name &rest non-header-cells)
  `(progn
     (define-lisp-object ,name fulltag-misc header ,@non-header-cells)
     (ccl::defenum ()
       ,@(mapcar #'(lambda (cell) (ccl::form-symbol name "." cell "-CELL")) non-header-cells))
     (defconstant ,(ccl::form-symbol name ".ELEMENT-COUNT") ,(length non-header-cells))))

  


(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant nbits-in-word 32)
(defconstant least-significant-bit 31)
(defconstant nbits-in-byte 8)
(defconstant ntagbits 3)                ; But non-header objects only use 2
(defconstant nlisptagbits 2)
(defconstant nfixnumtagbits 2)          ; See ?
(defconstant num-subtag-bits 8)         ; tag part of header is 8 bits wide
(defconstant fixnumshift nfixnumtagbits)
(defconstant fixnum-shift fixnumshift)          ; A pet name for it.
(defconstant fulltagmask (1- (ash 1 ntagbits)))         ; Only needed by GC/very low-level code
(defconstant full-tag-mask fulltagmask)
(defconstant tagmask (1- (ash 1 nlisptagbits)))
(defconstant tag-mask tagmask)
(defconstant fixnummask (1- (ash 1 nfixnumtagbits)))
(defconstant fixnum-mask fixnummask)
(defconstant subtag-mask (1- (ash 1 num-subtag-bits)))
(defconstant ncharcodebits 24)          ; only the low 8 bits are used, currently
(defconstant charcode-shift (- nbits-in-word ncharcodebits))
(defconstant word-shift 2)
(defconstant word-size-in-bytes 4)
(defconstant node-size 4)
(defconstant dnode-size 8)
(defconstant dnode-align-bits 3)
(defconstant dnode-shift dnode-align-bits)
(defconstant bitmap-shift 5)

(defconstant target-most-negative-fixnum (ash -1 (1- (- nbits-in-word nfixnumtagbits))))
(defconstant target-most-positive-fixnum (1- (ash 1 (1- (- nbits-in-word nfixnumtagbits)))))
(defconstant fixnumone (ash 1 fixnumshift))



;; Tags.
;; There are two-bit tags and three-bit tags.
;; A FULLTAG is the value of the low three bits of a tagged object.
;; A TAG is the value of the low two bits of a tagged object.
;; A TYPECODE is either a TAG or the value of a "tag-misc" object's header-byte.

;; There are 4 primary TAG values.  Any object which lisp can "see"
;; can be classified by its TAG.  (Some headers have FULLTAGS that are
;; congruent modulo 4 with the TAGS of other objects, but lisp can't
;; "see" headers.)
(ccl::defenum ()
  tag-fixnum                            ; All fixnums, whether odd or even
  tag-list                              ; Conses and NIL
  tag-misc                              ; Heap-consed objects other than lists: vectors, symbols, functions, floats ...
  tag-imm                               ; Immediate-objects: characters, UNBOUND, other markers.
)

;;; And there are 8 FULLTAG values.  Note that NIL has its own FULLTAG
;;; (congruent mod 4 to tag-list) and that both FULLTAG-MISC and
;;; FULLTAG-IMM have header fulltags that share the same TAG.  Things
;;; that walk memory (and the stack) have to be careful to look at the
;;; FULLTAG of each object that they see.
(ccl::defenum ()
  fulltag-even-fixnum                   ; I suppose EVENP/ODDP might care; nothing else does.
  fulltag-nil                           ; NIL and nothing but.  (Note that there's still a hidden NILSYM.)
  fulltag-nodeheader                    ; Header of heap-allocated object that contains lisp-object pointers
  fulltag-imm                           ; a "real" immediate object.  Shares TAG with fulltag-immheader.
  fulltag-odd-fixnum                    ; 
  fulltag-cons                          ; a real (non-null) cons.  Shares TAG with fulltag-nil.
  fulltag-misc                          ; Pointer "real" tag-misc object.  Shares TAG with fulltag-nodeheader.
  fulltag-immheader                     ; Header of heap-allocated object that contains unboxed data.
)

(defconstant misc-header-offset (- fulltag-misc))
(defconstant misc-subtag-offset misc-header-offset)
(defconstant misc-data-offset (+ misc-header-offset 4))
(defconstant misc-dfloat-offset (+ misc-header-offset 8))


(defconstant canonical-nil-value (+ #x04000000 fulltag-nil))
(defconstant nil-value canonical-nil-value)

;;; T is almost adjacent to NIL: since NIL is a misaligned CONS, it spans
;;; two doublewords.  The arithmetic difference between T and NIL is
;;; not inherently interesting; it should be possible to express that
;;; difference as an ARM constant, but that's the only real constraint.

(defconstant t-offset (+ (- dnode-size fulltag-nil) fulltag-misc))


;;; The order in which various header values are defined is significant in several ways:
;;; 1) Numeric subtags precede non-numeric ones; there are further
;;; orderings among numeric subtags.
;;; 2) All subtags which denote CL arrays are preceded by those that
;;; don't, with a further ordering which requires that (<
;;; header-arrayH header-vectorH ,@all-other-CL-vector-types)
;;; 3) The element-size of ivectors is determined by the ordering of
;;; ivector subtags.
;;; 4) All subtags are >= fulltag-immheader .


;;; Numeric subtags.
(define-imm-subtag bignum 0)
(define-node-subtag ratio 1)
(define-imm-subtag single-float 1)          ; "SINGLE" float, aka short-float in the new order.
(define-imm-subtag double-float 2)
(define-node-subtag complex 3)

;;; CL array types.  There are more immediate types than node types; all CL array subtags must be > than
;;; all non-CL-array subtags.  So we start by defining the immediate subtags in decreasing order, starting
;;; with that subtag whose element size isn't an integral number of bits and ending with those whose
;;; element size - like all non-CL-array fulltag-immheader types - is 32 bits.
(define-imm-subtag bit-vector 31)
(define-imm-subtag double-float-vector 30)
(define-imm-subtag s16-vector 29)
(define-imm-subtag u16-vector 28)
(defconstant min-16-bit-ivector-subtag subtag-u16-vector)
(defconstant max-16-bit-ivector-subtag subtag-s16-vector)


;;(define-imm-subtag simple-base-string 27)
(define-imm-subtag s8-vector 26)
(define-imm-subtag u8-vector 25)
(defconstant min-8-bit-ivector-subtag subtag-u8-vector)
(defconstant max-8-bit-ivector-subtag (logior fulltag-immheader (ash 27 ntagbits)))

(define-imm-subtag simple-base-string 24)
(define-imm-subtag fixnum-vector 23)
(define-imm-subtag s32-vector 22)
(define-imm-subtag u32-vector 21)
(define-imm-subtag single-float-vector 20)
(defconstant max-32-bit-ivector-subtag (logior fulltag-immheader (ash 24 ntagbits)))
(defconstant min-cl-ivector-subtag subtag-single-float-vector)

(define-node-subtag vectorH 20)
(define-node-subtag arrayH 19)
(assert (< subtag-arrayH subtag-vectorH min-cl-ivector-subtag))
(define-node-subtag simple-vector 21)   ; Only one such subtag
(assert (< subtag-arrayH subtag-vectorH subtag-simple-vector))
(defconstant min-vector-subtag subtag-vectorH)
(defconstant min-array-subtag subtag-arrayH)

;;; So, we get the remaining subtags (n: (n < min-array-subtag))
;;; for various immediate/node object types.

(define-node-subtag pseudofunction 0)
(define-imm-subtag macptr 3)
(define-imm-subtag dead-macptr 4)
(define-imm-subtag code-vector 5)
(define-imm-subtag creole-object 6)
(define-imm-subtag xcode-vector 7)  ; code-vector for cross-development

(defconstant max-non-array-imm-subtag (logior (ash 19 ntagbits) fulltag-immheader))

(define-node-subtag catch-frame 4)
(defconstant min-non-numeric-node-subtag subtag-catch-frame)
(define-node-subtag function 5)
(define-node-subtag basic-stream 6)
(define-node-subtag symbol 7)
(define-node-subtag lock 8)
(define-node-subtag hash-vector 9)
(define-node-subtag pool 10)
(define-node-subtag weak 11)
(define-node-subtag package 12)
(define-node-subtag slot-vector 13)
(define-node-subtag instance 14)
(define-node-subtag struct 15)
(define-node-subtag istruct 16)
(define-node-subtag value-cell 17)
(define-node-subtag xfunction 18)       ; Function for cross-development
(defconstant max-non-array-node-subtag (logior (ash 18 ntagbits) fulltag-nodeheader))

(define-subtag stack-alloc-marker fulltag-imm 1)
(define-subtag lisp-frame-marker fulltag-imm 2)
(define-subtag character fulltag-imm 9)
(define-subtag slot-unbound fulltag-imm 10)
(defconstant slot-unbound-marker subtag-slot-unbound)
(define-subtag illegal fulltag-imm 11)
(defconstant illegal-marker subtag-illegal)
(define-subtag go-tag fulltag-imm 12)
(define-subtag block-tag fulltag-imm 24)
(define-subtag no-thread-local-binding fulltag-imm 30)
(define-subtag unbound fulltag-imm 6)
(defconstant unbound-marker subtag-unbound)
(defconstant undefined unbound-marker)
(defconstant lisp-frame-marker subtag-lisp-frame-marker)
(defconstant stack-alloc-marker subtag-stack-alloc-marker)

(defconstant max-64-bit-constant-index 127)
(defconstant max-32-bit-constant-index (ash (+ #xfff arm::misc-data-offset) -2))
(defconstant max-16-bit-constant-index (ash (+ #xfff arm::misc-data-offset) -1))
(defconstant max-8-bit-constant-index (+ #xfff arm::misc-data-offset))
(defconstant max-1-bit-constant-index (ash (+ #xfff arm::misc-data-offset) 5))


;;; The objects themselves look something like this:

;;; Order of CAR and CDR doesn't seem to matter much - there aren't
;;; too many tricks to be played with predecrement/preincrement addressing.
;;; Keep them in the confusing MCL 3.0 order, to avoid confusion.
(define-lisp-object cons fulltag-cons 
  cdr 
  car)


(define-fixedsized-object ratio
  numer
  denom)

(define-fixedsized-object single-float
  value)

(define-fixedsized-object double-float
  pad
  val-low
  val-high)

(defconstant double-float.value double-float.val-low)
(defconstant double-float.value-cell double-float.val-low-cell)


(define-fixedsized-object complex
  realpart
  imagpart
)


;;; There are two kinds of macptr; use the length field of the header if you
;;; need to distinguish between them
(define-fixedsized-object macptr
  address
  domain
  type
)

(define-fixedsized-object xmacptr
  address
  domain
  type
  flags
  link
)

;;; Catch frames go on the cstack, below a lisp frame whose savelr
;;; field references the catch exit point/unwind-protect cleanup code.
(define-fixedsized-object catch-frame
  link                                  ; tagged pointer to next older catch frame
  mvflag                                ; 0 if single-value, 1 if uwp or multiple-value
  catch-tag                             ; #<unbound> -> unwind-protect, else catch
  db-link                               ; value of dynamic-binding link on thread entry.
  xframe                                ; exception-frame link
  last-lisp-frame
  code-vector                           ; may not be useful

)

(define-fixedsized-object lock
  _value                                ;finalizable pointer to kernel object
  kind                                  ; '0 = recursive-lock, '1 = rwlock
  writer				;tcr of owning thread or 0
  name
  whostate
  whostate-2
  )



(define-fixedsized-object symbol
  pname
  vcell
  fcell
  package-predicate
  flags
  plist
  binding-index
)

(define-fixedsized-object function
  entrypoint
  codevector
  )




(defconstant nilsym-offset (+ t-offset symbol.size))


(define-fixedsized-object vectorH
  logsize                               ; fillpointer if it has one, physsize otherwise
  physsize                              ; total size of (possibly displaced) data vector
  data-vector                           ; object this header describes
  displacement                          ; true displacement or 0
  flags                                 ; has-fill-pointer,displaced-to,adjustable bits; subtype of underlying simple vector.
)

(define-lisp-object arrayH fulltag-misc
  header                                ; subtag = subtag-arrayH
  rank                                  ; NEVER 1
  physsize                              ; total size of (possibly displaced) data vector
  data-vector                           ; object this header describes
  displacement                          ; true displacement or 0  
  flags                                 ; has-fill-pointer,displaced-to,adjustable bits; subtype of underlying simple vector.
 ;; Dimensions follow
)

(defconstant arrayH.rank-cell 0)
(defconstant arrayH.physsize-cell 1)
(defconstant arrayH.data-vector-cell 2)
(defconstant arrayH.displacement-cell 3)
(defconstant arrayH.flags-cell 4)
(defconstant arrayH.dim0-cell 5)

(defconstant arrayH.flags-cell-bits-byte (byte 8 0))
(defconstant arrayH.flags-cell-subtag-byte (byte 8 8))


(define-fixedsized-object value-cell
  value)

;;; The kernel uses these (rather generically named) structures
;;; to keep track of various memory regions it (or the lisp) is
;;; interested in.
;;; The gc-area record definition in "ccl:interfaces;mcl-records.lisp"
;;; matches this.

(define-storage-layout area 0
  pred                                  ; pointer to preceding area in DLL
  succ                                  ; pointer to next area in DLL
  low                                   ; low bound on area addresses
  high                                  ; high bound on area addresses.
  active                                ; low limit on stacks, high limit on heaps
  softlimit                             ; overflow bound
  hardlimit                             ; another one
  code                                  ; an area-code; see below
  markbits                              ; bit vector for GC
  ndnodes                               ; "active" size of dynamic area or stack
  older                                 ; in EGC sense
  younger                               ; also for EGC
  h                                     ; Handle or null pointer
  softprot                              ; protected_area structure pointer
  hardprot                              ; another one.
  owner                                 ; fragment (library) which "owns" the area
  refbits                               ; bitvector for intergenerational refernces
  threshold                             ; for egc
  gc-count                              ; generational gc count.
  static-dnodes                         ; for honsing, etc.
  static-used                           ; bitvector
)


(define-storage-layout protected-area 0
  next
  start                                 ; first byte (page-aligned) that might be protected
  end                                   ; last byte (page-aligned) that could be protected
  nprot                                 ; Might be 0
  protsize                              ; number of bytes to protect
  why)

(defconstant tcr-bias 0)

(define-storage-layout tcr (- tcr-bias)
  prev					; in doubly-linked list 
  next					; in doubly-linked list 
  lisp-fpscr
  pad
  db-link				; special binding chain head 
  catch-top				; top catch frame 
  save-vsp				; VSP when in foreign code 
  save-tsp				; TSP when in foreign code 
  cs-area				; cstack area pointer 
  vs-area				; vstack area pointer 
  last-lisp-frame
  cs-limit				; cstack overflow limit
  total-bytes-allocated-low
  total-bytes-allocated-high
  log2-allocation-quantum		; unboxed
  interrupt-pending			; fixnum
  xframe				; exception frame linked list
  errno-loc				; thread-private, maybe
  ffi-exception				; fpscr bits from ff-call.
  osid					; OS thread id 
  valence				; odd when in foreign code 
  foreign-exception-status
  native-thread-info
  native-thread-id
  last-allocptr
  save-allocptr
  save-allocbase
  reset-completion
  activate
  suspend-count
  suspend-context
  pending-exception-context
  suspend				; semaphore for suspension notify 
  resume				; sempahore for resumption notify
  flags					; foreign, being reset, ...
  gc-context
  termination-semaphore
  unwinding
  tlb-limit
  tlb-pointer
  shutdown-count
  safe-ref-address
)


(defconstant interrupt-level-binding-index (ash 1 fixnumshift))

(define-storage-layout lockptr 0
  avail
  owner
  count
  signal
  waiting
  malloced-ptr
  spinlock)

(define-storage-layout rwlock 0
  spin
  state
  blocked-writers
  blocked-readers
  writer
  reader-signal
  writer-signal
  malloced-ptr
  )



(arm::define-storage-layout lisp-frame 0
  marker
  savevsp
  savefn
  savelr
)




(defmacro define-header (name element-count subtag)
  `(defconstant ,name (logior (ash ,element-count num-subtag-bits) ,subtag)))

(define-header single-float-header single-float.element-count subtag-single-float)
(define-header double-float-header double-float.element-count subtag-double-float)
(define-header one-digit-bignum-header 1 subtag-bignum)
(define-header two-digit-bignum-header 2 subtag-bignum)
(define-header three-digit-bignum-header 3 subtag-bignum)
(define-header symbol-header symbol.element-count subtag-symbol)
(define-header value-cell-header value-cell.element-count subtag-value-cell)
(define-header macptr-header macptr.element-count subtag-macptr)


)




(defun %kernel-global (sym)
  ;; Returns index relative to (- nil-value fulltag-nil)
  (let* ((pos (position sym arm::*arm-kernel-globals* :test #'string=)))
    (if pos
      (- (* (+ 3 pos) 4))
      (error "Unknown kernel global : ~s ." sym))))

(defmacro kernel-global (sym)
  (let* ((pos (position sym arm::*arm-kernel-globals* :test #'string=)))
    (if pos
      (- (* (+ 3 pos) 4))
      (error "Unknown kernel global : ~s ." sym))))

;;; The kernel imports things that are defined in various other
;;; libraries for us.  The objects in question are generally
;;; fixnum-tagged; the entries in the "kernel-imports" vector are 4
;;; bytes apart.
(ccl::defenum (:prefix "KERNEL-IMPORT-" :start 0 :step 4)
  fd-setsize-bytes
  do-fd-set
  do-fd-clr
  do-fd-is-set
  do-fd-zero
  MakeDataExecutable
  GetSharedLibrary
  FindSymbol
  malloc
  free
  wait-for-signal
  tcr-frame-ptr
  register-xmacptr-dispose-function
  open-debug-output
  get-r-debug
  restore-soft-stack-limit
  egc-control
  lisp-bug
  NewThread
  YieldToThread
  DisposeThread
  ThreadCurrentStackSpace
  usage-exit
  save-fp-context
  restore-fp-context
  put-altivec-registers
  get-altivec-registers
  new-semaphore
  wait-on-semaphore
  signal-semaphore
  destroy-semaphore
  new-recursive-lock
  lock-recursive-lock
  unlock-recursive-lock
  destroy-recursive-lock
  suspend-other-threads
  resume-other-threads
  suspend-tcr
  resume-tcr
  rwlock-new
  rwlock-destroy
  rwlock-rlock
  rwlock-wlock
  rwlock-unlock
  recursive-lock-trylock
  foreign-name-and-offset
  lisp-read
  lisp-write
  lisp-open
  lisp-fchmod
  lisp-lseek
  lisp-close
  lisp-ftruncate
  lisp-stat
  lisp-fstat
  lisp-futex
  lisp-opendir
  lisp-readdir
  lisp-closedir
  lisp-pipe
  lisp-gettimeofday
  lisp-sigexit
  jvm-init
)

(defmacro nrs-offset (name)
  (let* ((pos (position name arm::*arm-nilreg-relative-symbols* :test #'eq)))
    (if pos (+ t-offset (* pos symbol.size)))))





(defmacro with-stack-short-floats (specs &body body)
  (ccl::collect ((binds)
		 (inits)
		 (names))
		(dolist (spec specs)
		  (let ((name (first spec)))
		    (binds `(,name (ccl::%make-sfloat)))
		    (names name)
		    (let ((init (second spec)))
		      (when init
			(inits `(ccl::%short-float ,init ,name))))))
		`(let* ,(binds)
		  (declare (dynamic-extent ,@(names))
			   (short-float ,@(names)))
		  ,@(inits)
		  ,@body)))

(defparameter *arm-target-uvector-subtags*
  `((:bignum . ,subtag-bignum)
    (:ratio . ,subtag-ratio)
    (:single-float . ,subtag-single-float)
    (:double-float . ,subtag-double-float)
    (:complex . ,subtag-complex  )
    (:symbol . ,subtag-symbol)
    (:function . ,subtag-function )
    (:code-vector . ,subtag-code-vector)
    (:xcode-vector . ,subtag-xcode-vector)
    (:macptr . ,subtag-macptr )
    (:catch-frame . ,subtag-catch-frame)
    (:struct . ,subtag-struct )    
    (:istruct . ,subtag-istruct )
    (:pool . ,subtag-pool )
    (:population . ,subtag-weak )
    (:hash-vector . ,subtag-hash-vector )
    (:package . ,subtag-package )
    (:value-cell . ,subtag-value-cell)
    (:instance . ,subtag-instance )
    (:lock . ,subtag-lock )
    (:slot-vector . ,subtag-slot-vector)
    (:basic-stream . ,subtag-basic-stream)
    (:simple-string . ,subtag-simple-base-string )
    (:bit-vector . ,subtag-bit-vector )
    (:signed-8-bit-vector . ,subtag-s8-vector )
    (:unsigned-8-bit-vector . ,subtag-u8-vector )
    (:signed-16-bit-vector . ,subtag-s16-vector )
    (:unsigned-16-bit-vector . ,subtag-u16-vector )
    (:signed-32-bit-vector . ,subtag-s32-vector )
    (:fixnum-vector . ,subtag-fixnum-vector)
    (:unsigned-32-bit-vector . ,subtag-u32-vector )
    (:single-float-vector . ,subtag-single-float-vector)
    (:double-float-vector . ,subtag-double-float-vector )
    (:simple-vector . ,subtag-simple-vector )
    (:vector-header . ,subtag-vectorH)
    (:array-header . ,subtag-arrayH)
    (:xfunction . ,subtag-xfunction)
    (:pseudofunction . ,subtag-pseudofunction)))


;;; This should return NIL unless it's sure of how the indicated
;;; type would be represented (in particular, it should return
;;; NIL if the element type is unknown or unspecified at compile-time.
(defun arm-array-type-name-from-ctype (ctype)
  (when (typep ctype 'ccl::array-ctype)
    (let* ((element-type (ccl::array-ctype-element-type ctype)))
      (typecase element-type
        (ccl::class-ctype
         (let* ((class (ccl::class-ctype-class element-type)))
           (if (or (eq class ccl::*character-class*)
                   (eq class ccl::*base-char-class*)
                   (eq class ccl::*standard-char-class*))
             :simple-string
             :simple-vector)))
        (ccl::numeric-ctype
         (if (eq (ccl::numeric-ctype-complexp element-type) :complex)
           :simple-vector
           (case (ccl::numeric-ctype-class element-type)
             (integer
              (let* ((low (ccl::numeric-ctype-low element-type))
                     (high (ccl::numeric-ctype-high element-type)))
                (cond ((or (null low) (null high)) :simple-vector)
                      ((and (>= low 0) (<= high 1) :bit-vector))
                      ((and (>= low 0) (<= high 255)) :unsigned-8-bit-vector)
                      ((and (>= low 0) (<= high 65535)) :unsigned-16-bit-vector)
                      ((and (>= low 0) (<= high #xffffffff) :unsigned-32-bit-vector))
                      ((and (>= low -128) (<= high 127)) :signed-8-bit-vector)
                      ((and (>= low -32768) (<= high 32767) :signed-16-bit-vector))
                      ((and (>= low target-most-negative-fixnum)
                            (<= high target-most-positive-fixnum))
                       :fixnum-vector)
                      ((and (>= low (ash -1 31)) (<= high (1- (ash 1 31))))
                       :signed-32-bit-vector)
                      (t :simple-vector))))
             (float
              (case (ccl::numeric-ctype-format element-type)
                ((double-float long-float) :double-float-vector)
                ((single-float short-float) :single-float-vector)
                (t :simple-vector)))
             (t :simple-vector))))
        (ccl::unknown-ctype)
        (ccl::named-ctype
         (if (eq element-type ccl::*universal-type*)
           :simple-vector))
        (t nil)))))
        
(defun arm-misc-byte-count (subtag element-count)
  (declare (fixnum subtag))
  (if (or (= fulltag-nodeheader (logand subtag fulltagmask))
          (<= subtag max-32-bit-ivector-subtag))
    (ash element-count 2)
    (if (<= subtag max-8-bit-ivector-subtag)
      element-count
      (if (<= subtag max-16-bit-ivector-subtag)
        (ash element-count 1)
        (if (= subtag subtag-bit-vector)
          (ash (+ element-count 7) -3)
          (+ 4 (ash element-count 3)))))))

(defparameter *arm-target-arch*
  (progn
    (arch::make-target-arch :name :arm
                            :lisp-node-size 4
                            :nil-value canonical-nil-value
                            :fixnum-shift fixnumshift
                            :most-positive-fixnum (1- (ash 1 (1- (- 32 fixnumshift))))
                            :most-negative-fixnum (- (ash 1 (1- (- 32 fixnumshift))))
                            :misc-data-offset misc-data-offset
                            :misc-dfloat-offset misc-dfloat-offset
                            :nbits-in-word 32
                            :ntagbits 3
                            :nlisptagbits 2
                            :uvector-subtags *arm-target-uvector-subtags*
                            :max-64-bit-constant-index max-64-bit-constant-index
                            :max-32-bit-constant-index max-32-bit-constant-index
                            :max-16-bit-constant-index max-16-bit-constant-index
                            :max-8-bit-constant-index max-8-bit-constant-index
                            :max-1-bit-constant-index max-1-bit-constant-index
                            :word-shift 2
                            :code-vector-prefix ()
                            :gvector-types '(:ratio :complex :symbol :function
                                             :catch-frame :struct :istruct
                                             :pool :population :hash-vector
                                             :package :value-cell :instance
                                             :lock :slot-vector
                                             :simple-vector :xfunction
                                             :pseudofunction)
                            :1-bit-ivector-types '(:bit-vector)
                            :8-bit-ivector-types '(:signed-8-bit-vector
                                                   :unsigned-8-bit-vector)
                            :16-bit-ivector-types '(:signed-16-bit-vector
                                                    :unsigned-16-bit-vector)
                            :32-bit-ivector-types '(:signed-32-bit-vector
                                                    :unsigned-32-bit-vector
                                                    :single-float-vector
                                                    :fixnum-vector
                                                    :single-float
                                                    :double-float
                                                    :bignum
                                                    :simple-string)
                            :64-bit-ivector-types '(:double-float-vector)
                            :array-type-name-from-ctype-function
                            #'arm-array-type-name-from-ctype
                            :package-name "ARM"
                            :t-offset t-offset
                            :array-data-size-function #'arm-misc-byte-count
                            :numeric-type-name-to-typecode-function
                            #'(lambda (type-name)
                                (ecase type-name
                                  (fixnum tag-fixnum)
                                  (bignum subtag-bignum)
                                  ((short-float single-float) subtag-single-float)
                                  ((long-float double-float) subtag-double-float)
                                  (ratio subtag-ratio)
                                  (complex subtag-complex)))
                            :subprims-base arm::*arm-subprims-base*
                            :subprims-shift arm::*arm-subprims-shift*
                            :subprims-table arm::*arm-subprims*
                            :primitive->subprims `(((0 . 23) . ,(ccl::%subprim-name->offset '.SPbuiltin-plus arm::*arm-subprims*)))
                            :unbound-marker-value unbound-marker
                            :slot-unbound-marker-value slot-unbound-marker
                            :fixnum-tag tag-fixnum
                            :single-float-tag subtag-single-float
                            :single-float-tag-is-subtag t
                            :double-float-tag subtag-double-float
                            :cons-tag fulltag-cons
                            :null-tag fulltag-nil
                            :symbol-tag subtag-symbol
                            :symbol-tag-is-subtag t
                            :function-tag subtag-function
                            :function-tag-is-subtag t
                            :big-endian nil
                            :misc-subtag-offset misc-subtag-offset
                            :car-offset cons.car
                            :cdr-offset cons.cdr
                            :subtag-char subtag-character
                            :charcode-shift charcode-shift
                            :fulltagmask fulltagmask
                            :fulltag-misc fulltag-misc
                            :char-code-limit #x110000
                            )))

;;; arch macros
(defmacro defarmarchmacro (name lambda-list &body body)
  `(arch::defarchmacro :arm ,name ,lambda-list ,@body))

(defarmarchmacro ccl::%make-sfloat ()
  `(ccl::%alloc-misc arm::single-float.element-count arm::subtag-single-float))

(defarmarchmacro ccl::%make-dfloat ()
  `(ccl::%alloc-misc arm::double-float.element-count arm::subtag-double-float))

(defarmarchmacro ccl::%numerator (x)
  `(ccl::%svref ,x arm::ratio.numer-cell))

(defarmarchmacro ccl::%denominator (x)
  `(ccl::%svref ,x arm::ratio.denom-cell))

(defarmarchmacro ccl::%realpart (x)
  `(ccl::%svref ,x arm::complex.realpart-cell))
                    
(defarmarchmacro ccl::%imagpart (x)
  `(ccl::%svref ,x arm::complex.imagpart-cell))

;;;
(defarmarchmacro ccl::%get-single-float-from-double-ptr (ptr offset)
 `(ccl::%double-float->short-float (ccl::%get-double-float ,ptr ,offset)
   (ccl::%alloc-misc 1 arm::subtag-single-float)))

(defarmarchmacro ccl::codevec-header-p (word)
  `(eql arm::subtag-code-vector
    (logand ,word arm::subtag-mask)))

(defarmarchmacro ccl::immediate-p-macro (thing)
  (let* ((tag (gensym)))
    `(let* ((,tag (ccl::lisptag ,thing)))
      (declare (fixnum ,tag))
      (or (= ,tag arm::tag-fixnum)
       (= ,tag arm::tag-imm)))))

(defarmarchmacro ccl::hashed-by-identity (thing)
  (let* ((typecode (gensym)))
    `(let* ((,typecode (ccl::typecode ,thing)))
      (declare (fixnum ,typecode))
      (or
       (= ,typecode arm::tag-fixnum)
       (= ,typecode arm::tag-imm)
       (= ,typecode arm::subtag-symbol)
       (= ,typecode arm::subtag-instance)))))

;;;
(defarmarchmacro ccl::%get-kernel-global (name)
  `(ccl::%fixnum-ref (ash (+ (- nil-value fulltag-nil)
                           ,(%kernel-global
                             (if (ccl::quoted-form-p name)
                               (cadr name)
                               name)))
                      (- fixnumshift))))
    

(defarmarchmacro ccl::%get-kernel-global-ptr (name dest)
  `(ccl::%setf-macptr
    ,dest
    (ccl::%fixnum-ref-macptr (ash (+ (- nil-value fulltag-nil)
                                     ,(%kernel-global
                                       (if (ccl::quoted-form-p name)
                                         (cadr name)
                                         name)))
                              (- fixnumshift)))))

(defarmarchmacro ccl::%target-kernel-global (name)
  `(arm::%kernel-global ,name))

(defarmarchmacro ccl::lfun-vector (fun)
  fun)

(defarmarchmacro ccl::lfun-vector-lfun (lfv)
  lfv)

(defarmarchmacro ccl::area-code ()
  area.code)

(defarmarchmacro ccl::area-succ ()
  area.succ)

;;; We generally don't want much code to see the function's entrypoint.
(defarmarchmacro ccl::nth-immediate (f i)
  `(ccl::%svref ,f (the fixnum (+ (the fixnum ,i) 1))))

(defarmarchmacro ccl::set-nth-immediate (f i new)
  `(setf (ccl::%svref ,f (the fixnum (+ (the fixnum ,i) 1))) ,new))

(defarmarchmacro ccl::symptr->symvector (s)
  s)

(defarmarchmacro ccl::symvector->symptr (s)
  s)

(defarmarchmacro ccl::function-to-function-vector (f)
  f)

(defarmarchmacro ccl::function-vector-to-function (v)
  v)

(defarmarchmacro ccl::with-ffcall-results ((buf) &body body)
  (let* ((size (+ (* 8 4) (* 31 8))))
    `(%stack-block ((,buf ,size))
      ,@body)))

(defconstant arg-check-trap-pc-limit 8)

;;; UUO encoding
(defconstant uuo-format-nullary 0)      ; 12 bits of code 
(defconstant uuo-format-unary 1)        ; 8 bits of info - NOT type info - 4-bit reg 
(defconstant uuo-format-error-lisptag 2) ; 2 bits of lisptag info, 4-bit reg 
(defconstant uuo-format-error-fulltag 3) ; 3 bits of fulltag info, 4 bit reg 

(defconstant uuo-format-error-xtype 4)  ; 8 bits of extended type/subtag info, 4 bit reg 
(defconstant uuo-format-cerror-lisptag 10) ; continuable, lisptag, reg 
(defconstant uuo-format-cerror-fulltag 11) ; continuable, fulltag, reg 
(defconstant uuo-format-cerror-xtype 12) ; continuable, xtype, reg         
(defconstant uuo-format-binary 15)      ;  4 bits of code, r1, r0 

;;; xtypes: 8-bit integers used to report type errors for types that can't
;;; be represented via tags.

(defconstant xtype-unsigned-byte-24  252)
(defconstant xtype-array2d  248)
(defconstant xtype-array3d  244)
(defconstant xtype-integer  4)
(defconstant xtype-s64  8)
(defconstant xtype-u64  12)
(defconstant xtype-s32  16)
(defconstant xtype-u32  20)
(defconstant xtype-s16  24)
(defconstant xtype-u16  28)
(defconstant xtype-s8  32)
(defconstant xtype-u8  36)
(defconstant xtype-bit  40)
(defconstant xtype-rational 44)
(defconstant xtype-real 48)
(defconstant xtype-number 52)
(defconstant xtype-char-code 56)

;;; Condition field values.
(ccl::defenum (:prefix "ARM-COND-")
  eq
  ne
  hs
  lo
  mi
  pl
  vs
  vc
  hi
  ls
  ge
  lt
  gt
  le
  al)

;;; FPSCR exception bits
(defconstant ioc 0)                     ;invalid operation
(defconstant dzc 1)                     ;division by 0
(defconstant ofc 2)                     ;overflow
(defconstant ufc 3)                     ;underflow
(defconstant ixc 4)                     ;inexact

(defconstant ioe 8)                     ;invalid operation enable
(defconstant dze 9)                     ;division by 0 enable
(defconstant ofe 10)                    ;overflow enable
(defconstant ufe 11)                    ;underflow enable
(defconstant ixe 12)                    ;inexact enable



;;; These are always stack-allocated, "near" where the missing lisp frame
;;; that they represent would be.

(define-storage-layout fake-stack-frame 0
  header
  type                                  ; 'arm::fake-stack-frame
  sp
  next-sp
  fn
  lr
  vsp
  xp)

#+arm-target
(ccl::make-istruct-class 'fake-stack-frame ccl::*istruct-class*)

(defconstant real-tags-mask (logior (ash 1 tag-fixnum)
                                    (ash 1 subtag-bignum)
                                    (ash 1 subtag-single-float)
                                    (ash 1 subtag-double-float)
                                    (ash 1 subtag-ratio)))
(defconstant numeric-tags-mask (logior real-tags-mask (ash 1 subtag-complex)))

  
(defconstant fasl-version #x61)
(defconstant fasl-max-version #x61)
(defconstant fasl-min-version #x61)
(defparameter *image-abi-version* 1039)

(provide "ARM-ARCH")
