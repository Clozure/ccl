;;;-*- Mode: Lisp; Package: (X8632 :use CL) -*-

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

;;; This stuff has to match lisp-kernel/x86-constants32.[hs]

(defpackage "X8632"
  (:use "CL")
  #+x8632-target
  (:nicknames "TARGET"))

(in-package "X8632")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "X86-ARCH")
  (require "X86-LAP")

(defparameter *x8632-symbolic-register-names*
  (make-hash-table :test #'equal)
  "For the disassembler, mostly.")

;;; Define integer constants which map to indices in the
;;; X86::*X8632-REGISTER-ENTRIES* array.
(ccl::defenum ()
  ;; 32-bit registers
  eax
  ecx
  edx
  ebx
  esp
  ebp
  esi
  edi
  ;; 16-bit-registers
  ax
  cx
  dx
  bx
  sp
  bp
  si
  di
  ;; 8-bit registers
  al
  cl
  dl
  bl
  ah
  ch
  dh
  bh
  ;; xmm registers
  xmm0
  xmm1
  xmm2
  xmm3
  xmm4
  xmm5
  xmm6
  xmm7
  ;; MMX registers
  mm0
  mm1
  mm2
  mm3
  mm4
  mm5
  mm6
  mm7
  ;; x87 FP regs
  st[0]
  st[1]
  st[2]
  st[3]
  st[4]
  st[5]
  st[6]
  st[7]
  ;; Segment registers
  cs
  ds
  ss
  es
  fs
  gs
  )

(defmacro defx86reg (alias known)
  (let* ((known-entry (gensym)))
    `(let* ((,known-entry (gethash ,(string known) x86::*x8632-registers*)))
       (unless ,known-entry
	 (error "register ~a not defined" ',known))
       (setf (gethash ,(string alias) x86::*x8632-registers*) ,known-entry)
       (unless (gethash ,(string-downcase (string known)) *x8632-symbolic-register-names*)
	 (setf (gethash ,(string-downcase (string known)) *x8632-symbolic-register-names*)
	       (string-downcase ,(string alias))))
       (defconstant ,alias ,known))))

;;; The limited number of registers that we have may make it
;;; impossible to statically partition the register file into
;;; immediate and tagged sets.
;;;
;;; As a baseline, we will use the scheme defined below.  This
;;; partitioning will be in effect any time a function is entered
;;; (and therefore at the time of a function call).
;;;
;;; This partitioning can be altered by setting or clearing bits in
;;; thread-private memory which indicate whether a register is an
;;; immmediate or a node.  The GC will look at these flag bits to
;;; decide how to treat the registers.
;;;
;;; "Lispy" register names might be therefore be confusing at times.
;;; 

(defx86reg imm0 eax)
(defx86reg imm0.w ax)
(defx86reg imm0.b al)
(defx86reg imm0.bh ah)

(defx86reg temp0 ecx)
(defx86reg temp0.w cx)
(defx86reg temp0.b cl)
(defx86reg temp0.bh ch)
(defx86reg shift cl)

(defx86reg temp1 edx)
(defx86reg temp1.w dx)
(defx86reg temp1.b dl)
(defx86reg temp1.bh dh)
(defx86reg nargs edx)

(defx86reg arg_z ebx)
(defx86reg arg_z.w bx)
(defx86reg arg_z.b bl)
(defx86reg arg_z.bh bh)

(defx86reg arg_y esi)
(defx86reg arg_y.w si)

(defx86reg fn edi)

;; Callee-saved non-volatile registers are probably a non-starter on
;; IA-32.

;;; Use xmm regs for floating-point.  (They can also hold integer values.)
(defx86reg fp0 xmm0)
(defx86reg fp1 xmm1)
(defx86reg fp2 xmm2)
(defx86reg fp3 xmm3)
(defx86reg fp4 xmm4)
(defx86reg fp5 xmm5)
(defx86reg fp6 xmm6)
(defx86reg fp7 xmm7)

(defx86reg fpzero fp7)

;;; The 8 MMX registers overlap the x87 FPU.
;;; (so when/if we use the x87 FPU, we need to be careful with this)
(defx86reg stack-temp mm7)

(defx86reg fname temp0)

(defx86reg allocptr temp0)

(defx86reg ra0 temp0)

;;; We rely one at least one of %ra0/%fn pointing to the current function
;;; (or to a TRA that references the function) at all times.  When we
;;; tail call something, we want %RA0 to point to our caller's TRA and
;;; %FN to point to the new function.  Unless we go out of line to
;;; do tail calls, we need some register not involved in the calling
;;; sequence to hold the current function, since it might get GCed otherwise.
;;; (The odds of this happening are low, but non-zero.)
;;; xxx
(defx86reg xfn temp1)

(defx86reg next-method-context temp0)

;;; This follows the ppc32 scheme pretty closely.

(defconstant nbits-in-word 32)
(defconstant nbits-in-byte 8)
(defconstant ntagbits 3)
(defconstant nlisptagbits 2)
(defconstant nfixnumtagbits 2)
(defconstant num-subtag-bits 8)
(defconstant subtagmask 255)
(defconstant fixnumshift 2)
(defconstant fixnum-shift 2)
(defconstant fulltagmask 7)
(defconstant tagmask 3)
(defconstant fixnummask 3)
(defconstant ncharcodebits 8)
(defconstant charcode-shift 8)
(defconstant word-shift 2)
(defconstant word-size-in-bytes 4)
(defconstant node-size word-size-in-bytes)
(defconstant dnode-size 8)
(defconstant dnode-align-bits 3)
(defconstant dnode-shift dnode-align-bits)
(defconstant bitmap-shift 5)

(defconstant fixnumone (ash 1 fixnumshift))
(defconstant fixnum-one fixnumone)
(defconstant fixnum1 fixnumone)

(defconstant target-most-negative-fixnum (ash -1 (1- (- nbits-in-word nfixnumtagbits))))
(defconstant target-most-positive-fixnum (1- (ash 1 (1- (- nbits-in-word nfixnumtagbits)))))

;;; bits correspond to reg encoding used in instructions
;;;  7   6   5   4   3   2   1   0
;;; edi esi ebp esp ebx edx ecx eax

(defconstant default-node-regs-mask #b11001110)

;;; 2-bit "lisptag" values
(defconstant tag-fixnum 0)
(defconstant tag-list 1)		;a misnomer now
(defconstant tag-misc 2)
(defconstant tag-imm 3)

;;; 3-bit "fulltag" values
(defconstant fulltag-even-fixnum 0)
(defconstant fulltag-cons 1)
(defconstant fulltag-nodeheader 2)
(defconstant fulltag-imm 3)
(defconstant fulltag-odd-fixnum 4)
(defconstant fulltag-tra 5)		;was for nil on PPC32
(defconstant fulltag-misc 6)
(defconstant fulltag-immheader 7)

(defmacro define-subtag (name tag subtag)
  `(defconstant ,(ccl::form-symbol "SUBTAG-" name) (logior ,tag (ash ,subtag ntagbits))))

(defmacro define-imm-subtag (name subtag)
  `(define-subtag ,name fulltag-immheader ,subtag))

(defmacro define-node-subtag (name subtag)
  `(define-subtag ,name fulltag-nodeheader ,subtag))

;;; The order in which various header values are defined is
;;; significant in several ways:
;;; 1) Numeric subtags precede non-numeric ones; there are further
;;;    orderings among numeric subtags.
;;; 2) All subtags which denote CL arrays are preceded by those that
;;;    don't, with a further ordering which requires that
;;;    (< header-arrayH header-vectorH ,@all-other-CL-vector-types)
;;; 3) The element-size of ivectors is determined by the ordering of
;;;    ivector subtags.
;;; 4) All subtags are >= fulltag-immheader.

;;; Numeric subtags
(define-imm-subtag bignum 0)
(defconstant min-numeric-subtag subtag-bignum)
(define-node-subtag ratio 1)
(defconstant max-rational-subtag subtag-ratio)

(define-imm-subtag single-float 1)
(define-imm-subtag double-float 2)
(defconstant min-float-subtag subtag-single-float)
(defconstant max-float-subtag subtag-double-float)
(defconstant max-real-subtag subtag-double-float)

(define-node-subtag complex 3)
(defconstant max-numeric-subtag subtag-complex)

;;; CL array types.  There are more immediate types than node types;
;;; all CL array subtags must be > than all non-CL-array subtags.  So
;;; we start by defining the immediate subtags in decreasing order,
;;; starting with that subtag whose element size isn't an integral
;;; number of bits and ending with those whose element size - like all
;;; non-CL-array fulltag-immheader types - is 32 bits.

(define-imm-subtag bit-vector 31)
(define-imm-subtag double-float-vector 30)
(define-imm-subtag s16-vector 29)
(define-imm-subtag u16-vector 28)
(defconstant min-16-bit-ivector-subtag subtag-u16-vector)
(defconstant max-16-bit-ivector-subtag subtag-s16-vector)

;imm-subtag 27 unused

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

(define-node-subtag arrayH 19)
(define-node-subtag vectorH 20)
(assert (< subtag-arrayH subtag-vectorH min-cl-ivector-subtag))
(define-node-subtag simple-vector 21)   ; Only one such subtag
(assert (< subtag-arrayH subtag-vectorH subtag-simple-vector))
(defconstant min-vector-subtag subtag-vectorH)
(defconstant min-array-subtag subtag-arrayH)

(define-imm-subtag macptr 3)
(defconstant min-non-numeric-imm-subtag subtag-macptr)
(assert (> min-non-numeric-imm-subtag max-numeric-subtag))
(define-imm-subtag dead-macptr 4)
;;(define-imm-subtag unused 5)		;was creole-object
;;(define-imm-subtag unused 6)		;was code-vector
(define-imm-subtag xcode-vector 7)

;;; immediate subtags
(define-subtag unbound fulltag-imm 6)
(defconstant unbound-marker subtag-unbound)
(defconstant undefined unbound-marker)
(define-subtag character fulltag-imm 9)
(define-subtag slot-unbound fulltag-imm 10)
(defconstant slot-unbound-marker subtag-slot-unbound)
(define-subtag illegal fulltag-imm 11)
(defconstant illegal-marker subtag-illegal)
(define-subtag forward-marker fulltag-imm 28)
(define-subtag reserved-frame fulltag-imm 29)
(defconstant reserved-frame-marker subtag-reserved-frame)
(define-subtag no-thread-local-binding fulltag-imm 30)

;;; This has two functions: it tells the link-inverting marker where
;;; the code ends and the self-reference table and constants start, and it
;;; ensures that the 0th constant will never be in the same memozized
;;; dnode as some (unboxed) word of machine code.  I'm not sure if
;;; there's a better way to do either of those things.
;;;
;;; Depending on how you look at it, we either lose 8 bytes per
;;; function, or gain 7 bytes of otherwise unused space for debugging
;;; info.
;;; xxx -- comments above not right for x8632
(define-subtag function-boundary-marker fulltag-imm 31)
(defconstant function-boundary-marker subtag-function-boundary-marker)
(defconstant max-non-array-imm-subtag (logior (ash 19 ntagbits) fulltag-immheader))

(define-node-subtag catch-frame 4)
(defconstant min-non-numeric-node-subtag subtag-catch-frame)
(assert (> min-non-numeric-node-subtag max-numeric-subtag))
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

(defconstant misc-header-offset (- fulltag-misc))
(defconstant misc-subtag-offset misc-header-offset)
(defconstant misc-data-offset (+ misc-header-offset node-size))
(defconstant misc-dfloat-offset ( + misc-header-offset 8))

(defconstant max-64-bit-constant-index (ash 1 24))
(defconstant max-32-bit-constant-index (ash 1 24))
(defconstant max-16-bit-constant-index (ash 1 24))
(defconstant max-8-bit-constant-index (ash 1 24))
(defconstant max-1-bit-constant-index (ash 1 24))

)  ;eval-when

;;; On IA-32, the tag which was used for nil on ppc32 is now used for
;;; tagged return addresses.  We therefore make nil a distinguished
;;; CONS.  This way, CAR and CDR can just check the tag, and
;;; CONSP/RPLACA/RPLACD can check the tag and complain if the argument
;;; is NIL.
(defconstant canonical-nil-value (+ #x13000 fulltag-cons))
(defconstant canonical-t-value (+ #x13008 fulltag-misc))
(defconstant t-offset (- canonical-t-value canonical-nil-value))

(defconstant misc-bias fulltag-misc)
(defconstant cons-bias fulltag-cons)


(defmacro define-storage-layout (name origin &rest cells)
  `(progn
     (ccl::defenum (:start ,origin :step 4)
	 ,@(mapcar #'(lambda (cell) (ccl::form-symbol name "." cell)) cells))
     (defconstant ,(ccl::form-symbol name ".SIZE") ,(* (length cells) 4))))

(defmacro define-lisp-object (name tagname &rest cells)
  `(define-storage-layout ,name ,(- (symbol-value tagname)) ,@cells))

(defmacro define-fixedsized-object (name &rest non-header-cells)
  `(progn
     (define-lisp-object ,name fulltag-misc header ,@non-header-cells)
     (ccl::defenum ()
	 ,@(mapcar #'(lambda (cell) (ccl::form-symbol name "." cell "-CELL")) non-header-cells))
     (defconstant ,(ccl::form-symbol name ".ELEMENT-COUNT") ,(length non-header-cells))))

(define-lisp-object cons tag-list 
  cdr 
  car)

(define-fixedsized-object ratio
  numer
  denom)

(define-fixedsized-object single-float
  value)

(define-fixedsized-object double-float
  pad
  value
  val-high)

(define-fixedsized-object complex
  realpart
  imagpart)

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

;;; Need to think about catch frames on x8632, too.
(define-fixedsized-object catch-frame
  catch-tag                             ; #<unbound> -> unwind-protect, else catch
  link                                  ; tagged pointer to next older catch frame
  mvflag                                ; 0 if single-value, 1 if uwp or multiple-value
  esp                                   ;
  ebp
  foreign-sp
  db-link                               ; value of dynamic-binding link on thread entry.
  xframe                                ; exception-frame link
  pc                                    ; tra of catch exit/unwind cleanup
)

(define-fixedsized-object lock
  _value                                ;finalizable pointer to kernel object
  kind                                  ; '0 = recursive-lock, '1 = rwlock
  writer                                ;tcr of owning thread or 0
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

(define-storage-layout lisp-frame 0
  backptr
  return-address
  xtra)

(define-storage-layout tsp-frame 0
  backptr
  ebp)

(define-storage-layout csp-frame 0
  backptr
  ebp)

(define-storage-layout xcf 0            ;"exception callback frame"
  backptr
  return-address                        ; always 0
  nominal-function
  relative-pc
  containing-object
  xp
  ra0
  foreign-sp				;value of tcr.foreign_sp
  prev-xframe				;tcr.xframe before exception
  )					;(last 2 needed by apply-in-frame)

;;; The kernel uses these (rather generically named) structures
;;; to keep track of various memory regions it (or the lisp) is
;;; interested in.

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

#+windows-target
(progn
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant tcr-bias #xe88))

(define-storage-layout tcr tcr-bias
  linear
  aux
  valence
  node-regs-mask       ; bit set means corresponding reg contains node
  save-allocbase
  save-allocptr
  last-allocptr
  catch-top
  db-link
  tlb-limit
  tlb-pointer
  ffi-exception
  foreign-sp
  interrupt-pending
  next-method-context
  next-tsp
  safe-ref-address
  save-tsp
  save-vsp
  save-ebp
  ts-area
  vs-area
  xframe
  unwinding
  flags
  foreign-mxcsr
  lisp-mxcsr
  pending-exception-context
  unboxed0
  unboxed1
  save0
  save1
  save2
  save3)

(define-storage-layout tcr-aux 0
  total-bytes-allocated-low
  total-bytes-allocated-high
  cs-area
  cs-limit
  log2-allocation-quantum
  errno-loc
  osid
  foreign-exception-status
  native-thread-info
  native-thread-id
  reset-completion
  activate
  gc-context
  termination-semaphore
  shutdown-count
  suspend-count
  suspend-context
  suspend
  resume
  allocated
  pending-io-info
  io-datum
  next
  prev)

)

#-windows-target
(progn

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant tcr-bias 0))

(define-storage-layout tcr (- tcr-bias)
  next					; in doubly-linked list
  prev					; in doubly-linked list 
  node-regs-mask			; bit set means corresponding reg contains node
  linear
  ;; save0 *must* be aligned on a 16-byte boundary!
  save0					;spill area for node registers
  save1					; (caller saved)
  save2					; probably saved/restored in
  save3					; callout/trap handlers
  save-ebp                              ; lisp frame ptr for foreign code
  lisp-mxcsr
  foreign-mxcsr
  db-link				; special binding chain head 
  catch-top				; top catch frame 
  save-vsp				; SP when in foreign code 
  save-tsp				; TSP, at all times
  foreign-sp                            ; SP when in lisp code
  cs-area				; cstack area pointer 
  vs-area				; vstack area pointer 
  ts-area				; tstack area pointer 
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
  next-tsp
  safe-ref-address
  ldt-selector
  scratch-mxcsr				;used for reading/writing mxcsr
  unboxed0				;unboxed scratch locations
  unboxed1
  next-method-context			;used in lieu of register
  save-eflags
  allocated                             ;maybe unaligned TCR pointer
  pending-io-info
  io-datum                              ;for windows overlapped I/O
)
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

(defmacro define-header (name element-count subtag)
  `(defconstant ,name (logior (ash ,element-count num-subtag-bits) ,subtag)))

(define-header single-float-header single-float.element-count subtag-single-float)
(define-header double-float-header double-float.element-count subtag-double-float)

;;; We could possibly have a one-digit bignum header when dealing
;;; with "small bignums" in some bignum code.  Like other cases of
;;; non-normalized bignums, they should never escape from the lab.
(define-header one-digit-bignum-header 1 subtag-bignum)
(define-header two-digit-bignum-header 2 subtag-bignum)
(define-header three-digit-bignum-header 3 subtag-bignum)
(define-header symbol-header symbol.element-count subtag-symbol)
(define-header value-cell-header value-cell.element-count subtag-value-cell)
(define-header macptr-header macptr.element-count subtag-macptr)

;;; see x86-clos.lisp
(defconstant gf-code-size 30)

(defun %kernel-global (sym)
  (let* ((pos (position sym x86::*x86-kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-cons (* (1+ pos) node-size)))
      (error "Unknown kernel global : ~s ." sym))))

(defmacro kernel-global (sym)
  (let* ((pos (position sym x86::*x86-kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-cons (* (1+ pos) node-size)))
      (error "Unknown kernel global : ~s ." sym))))

(ccl::defenum (:prefix "KERNEL-IMPORT-" :start 0 :step node-size)
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
  cooperative-thread-startup
  DisposeThread
  ThreadCurrentStackSpace
  usage-exit
  save-fp-context
  restore-fp-context
  put-altivec-registers			;is there any
  get-altivec-registers			;point to these on x86?
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
)

(defmacro nrs-offset (name)
  (let* ((pos (position name x86::*x86-nilreg-relative-symbols* :test #'eq)))
    (if pos (* (1- pos) symbol.size))))

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

(defparameter *x8632-target-uvector-subtags*
  `((:bignum . ,subtag-bignum)
    (:ratio . ,subtag-ratio)
    (:single-float . ,subtag-single-float)
    (:double-float . ,subtag-double-float)
    (:complex . ,subtag-complex  )
    (:symbol . ,subtag-symbol)
    (:function . ,subtag-function )
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
    (:array-header . ,subtag-arrayH)))

;;; This should return NIL unless it's sure of how the indicated
;;; type would be represented (in particular, it should return
;;; NIL if the element type is unknown or unspecified at compile-time.
(defun x8632-array-type-name-from-ctype (ctype)
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

(defun x8632-misc-byte-count (subtag element-count)
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

(defparameter *x8632-subprims-shift* 2)
(defconstant x8632-subprims-base #x15000)

(declaim (special *x8632-subprims*))

(let* ((origin x8632-subprims-base)
       (step (ash 1 *x8632-subprims-shift*)))
  (flet ((define-x8632-subprim (name)
	   (ccl::make-subprimitive-info :name (string name)
					:offset (prog1 origin
						  (incf origin step)))))
    (macrolet ((defx8632subprim (name)
		 `(define-x8632-subprim ',name)))
      (defparameter *x8632-subprims*
	(vector
         (defx8632subprim .SPjmpsym)
         (defx8632subprim .SPjmpnfn)
         (defx8632subprim .SPfuncall)
         (defx8632subprim .SPmkcatch1v)
         (defx8632subprim .SPmkunwind)
         (defx8632subprim .SPmkcatchmv)
         (defx8632subprim .SPthrow)
         (defx8632subprim .SPnthrowvalues)
         (defx8632subprim .SPnthrow1value)
         (defx8632subprim .SPbind)
         (defx8632subprim .SPbind-self)
         (defx8632subprim .SPbind-nil)
         (defx8632subprim .SPbind-self-boundp-check)
         (defx8632subprim .SPrplaca)
         (defx8632subprim .SPrplacd)
         (defx8632subprim .SPconslist)
         (defx8632subprim .SPconslist-star)
         (defx8632subprim .SPstkconslist)
         (defx8632subprim .SPstkconslist-star)
         (defx8632subprim .SPmkstackv)
         (defx8632subprim .SPsubtag-misc-ref)
         (defx8632subprim .SPsetqsym)
         (defx8632subprim .SPprogvsave)
         (defx8632subprim .SPstack-misc-alloc)
         (defx8632subprim .SPgvector)
         (defx8632subprim .SPnvalret)
         (defx8632subprim .SPmvpass)
         (defx8632subprim .SPrecover-values-for-mvcall)
         (defx8632subprim .SPnthvalue)
         (defx8632subprim .SPvalues)
         (defx8632subprim .SPdefault-optional-args)
         (defx8632subprim .SPopt-supplied-p)
         (defx8632subprim .SPheap-rest-arg)
         (defx8632subprim .SPreq-heap-rest-arg)
         (defx8632subprim .SPheap-cons-rest-arg)
         (defx8632subprim .SPsimple-keywords)
         (defx8632subprim .SPkeyword-args)
         (defx8632subprim .SPkeyword-bind)
         (defx8632subprim .SPffcall)
         (defx8632subprim .SParef2)
         (defx8632subprim .SPksignalerr)
         (defx8632subprim .SPstack-rest-arg)
         (defx8632subprim .SPreq-stack-rest-arg)
         (defx8632subprim .SPstack-cons-rest-arg)
         (defx8632subprim .SPpoweropen-callbackX) ;needed on x86?
         (defx8632subprim .SPcall-closure)
         (defx8632subprim .SPgetXlong)
         (defx8632subprim .SPspreadargz)
         (defx8632subprim .SPtfuncallgen)
         (defx8632subprim .SPtfuncallslide)
         (defx8632subprim .SPtfuncallvsp)
         (defx8632subprim .SPtcallsymgen)
         (defx8632subprim .SPtcallsymslide)
         (defx8632subprim .SPtcallsymvsp)
         (defx8632subprim .SPtcallnfngen)
         (defx8632subprim .SPtcallnfnslide)
         (defx8632subprim .SPtcallnfnvsp)
         (defx8632subprim .SPmisc-ref)
         (defx8632subprim .SPmisc-set)
         (defx8632subprim .SPstkconsyz)
         (defx8632subprim .SPstkvcell0)
         (defx8632subprim .SPstkvcellvsp)
         (defx8632subprim .SPmakestackblock)
         (defx8632subprim .SPmakestackblock0)
         (defx8632subprim .SPmakestacklist)
         (defx8632subprim .SPstkgvector)
         (defx8632subprim .SPmisc-alloc)
         (defx8632subprim .SPpoweropen-ffcallX)	;needed on x86?
         (defx8632subprim .SPgvset)
         (defx8632subprim .SPmacro-bind)
         (defx8632subprim .SPdestructuring-bind)
         (defx8632subprim .SPdestructuring-bind-inner)
         (defx8632subprim .SPrecover-values)
         (defx8632subprim .SPvpopargregs)
         (defx8632subprim .SPinteger-sign)
         (defx8632subprim .SPsubtag-misc-set)
         (defx8632subprim .SPspread-lexpr-z)
         (defx8632subprim .SPstore-node-conditional)
         (defx8632subprim .SPreset)
         (defx8632subprim .SPmvslide)
         (defx8632subprim .SPsave-values)
         (defx8632subprim .SPadd-values)
         (defx8632subprim .SPcallback)
         (defx8632subprim .SPmisc-alloc-init)
         (defx8632subprim .SPstack-misc-alloc-init)
         (defx8632subprim .SPset-hash-key)
         (defx8632subprim .SPaset2)
         (defx8632subprim .SPcallbuiltin)
         (defx8632subprim .SPcallbuiltin0)
         (defx8632subprim .SPcallbuiltin1)
         (defx8632subprim .SPcallbuiltin2)
         (defx8632subprim .SPcallbuiltin3)
         (defx8632subprim .SPpopj)
         (defx8632subprim .SPrestorefullcontext)
         (defx8632subprim .SPsavecontextvsp)
         (defx8632subprim .SPsavecontext0)
         (defx8632subprim .SPrestorecontext)
         (defx8632subprim .SPlexpr-entry)
         (defx8632subprim .SPsyscall2)
         (defx8632subprim .SPbuiltin-plus)
         (defx8632subprim .SPbuiltin-minus)
         (defx8632subprim .SPbuiltin-times)
         (defx8632subprim .SPbuiltin-div)
         (defx8632subprim .SPbuiltin-eq)
         (defx8632subprim .SPbuiltin-ne)
         (defx8632subprim .SPbuiltin-gt)
         (defx8632subprim .SPbuiltin-ge)
         (defx8632subprim .SPbuiltin-lt)
         (defx8632subprim .SPbuiltin-le)
         (defx8632subprim .SPbuiltin-eql)
         (defx8632subprim .SPbuiltin-length)
         (defx8632subprim .SPbuiltin-seqtype)
         (defx8632subprim .SPbuiltin-assq)
         (defx8632subprim .SPbuiltin-memq)
         (defx8632subprim .SPbuiltin-logbitp)
         (defx8632subprim .SPbuiltin-logior)
         (defx8632subprim .SPbuiltin-logand)
         (defx8632subprim .SPbuiltin-ash)
         (defx8632subprim .SPbuiltin-negate)
         (defx8632subprim .SPbuiltin-logxor)
         (defx8632subprim .SPbuiltin-aref1)
         (defx8632subprim .SPbuiltin-aset1)
         (defx8632subprim .SPbreakpoint)
         (defx8632subprim .SPeabi-ff-call)
         (defx8632subprim .SPeabi-callback)
         (defx8632subprim .SPsyscall)
         (defx8632subprim .SPgetu64)
         (defx8632subprim .SPgets64)
         (defx8632subprim .SPmakeu64)
         (defx8632subprim .SPmakes64)
         (defx8632subprim .SPspecref)
         (defx8632subprim .SPspecset)
         (defx8632subprim .SPspecrefcheck)
         (defx8632subprim .SPrestoreintlevel)
         (defx8632subprim .SPmakes32)
         (defx8632subprim .SPmakeu32)
         (defx8632subprim .SPgets32)
         (defx8632subprim .SPgetu32)
         (defx8632subprim .SPfix-overflow)
         (defx8632subprim .SPmvpasssym)
         (defx8632subprim .SParef3)
         (defx8632subprim .SPaset3)
         (defx8632subprim .SPffcall-return-registers)
         (defx8632subprim .SPaset1)
         (defx8632subprim .SPset-hash-key-conditional)
         (defx8632subprim .SPunbind-interrupt-level)
         (defx8632subprim .SPunbind)
         (defx8632subprim .SPunbind-n)
         (defx8632subprim .SPunbind-to)
         (defx8632subprim .SPbind-interrupt-level-m1)
         (defx8632subprim .SPbind-interrupt-level)
         (defx8632subprim .SPbind-interrupt-level-0)
         (defx8632subprim .SPprogvrestore)
	 (defx8632subprim .SPnmkunwind)
         )))))



(defparameter *x8632-target-arch*
  (arch::make-target-arch :name :x8632
                          :lisp-node-size node-size
                          :nil-value canonical-nil-value
                          :fixnum-shift fixnumshift
                          :most-positive-fixnum target-most-positive-fixnum
                          :most-negative-fixnum target-most-negative-fixnum
                          :misc-data-offset misc-data-offset
                          :misc-dfloat-offset misc-dfloat-offset
                          :nbits-in-word nbits-in-word
                          :ntagbits ntagbits
                          :nlisptagbits nlisptagbits
                          :uvector-subtags *x8632-target-uvector-subtags*
                          :max-64-bit-constant-index max-64-bit-constant-index
                          :max-32-bit-constant-index max-32-bit-constant-index
                          :max-16-bit-constant-index max-16-bit-constant-index
                          :max-8-bit-constant-index max-8-bit-constant-index
                          :max-1-bit-constant-index max-1-bit-constant-index
                          :word-shift word-shift
                          :code-vector-prefix ()
                          :gvector-types '(:ratio :complex :symbol :function
                                           :catch-frame :struct :istruct
                                           :pool :population :hash-vector
                                           :package :value-cell :instance
                                           :lock :slot-vector
                                           :simple-vector)
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
                          #'x8632-array-type-name-from-ctype
                          :package-name "X8632"
                          :t-offset t-offset
                          :array-data-size-function #'x8632-misc-byte-count
                          :numeric-type-name-to-typecode-function
                          #'(lambda (type-name)
                              (ecase type-name
                                (fixnum tag-fixnum)
                                (bignum subtag-bignum)
                                ((short-float single-float) subtag-single-float)
                                ((long-float double-float) subtag-double-float)
                                (ratio subtag-ratio)
                                (complex subtag-complex)))
                          :subprims-base x8632-subprims-base
                          :subprims-shift x8632::*x8632-subprims-shift*
                          :subprims-table x8632::*x8632-subprims*
                          :primitive->subprims `(((0 . 23) . ,(ccl::%subprim-name->offset '.SPbuiltin-plus x8632::*x8632-subprims*)))
                          :unbound-marker-value unbound-marker
                          :slot-unbound-marker-value slot-unbound-marker
                          :fixnum-tag tag-fixnum
                          :single-float-tag subtag-single-float
                          :single-float-tag-is-subtag t
                          :double-float-tag subtag-double-float
                          :cons-tag fulltag-cons
                          :null-tag fulltag-cons
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
                          ))

;; arch macros

(defmacro defx8632archmacro (name lambda-list &body body)
  `(arch::defarchmacro :x8632 ,name ,lambda-list ,@body))

(defx8632archmacro ccl::%make-sfloat ()
  `(ccl::%alloc-misc x8632::single-float.element-count x8632::subtag-single-float))

(defx8632archmacro ccl::%make-dfloat ()
  `(ccl::%alloc-misc x8632::double-float.element-count x8632::subtag-double-float))

(defx8632archmacro ccl::%numerator (x)
  `(ccl::%svref ,x x8632::ratio.numer-cell))

(defx8632archmacro ccl::%denominator (x)
  `(ccl::%svref ,x x8632::ratio.denom-cell))

(defx8632archmacro ccl::%realpart (x)
  `(ccl::%svref ,x x8632::complex.realpart-cell))
                    
(defx8632archmacro ccl::%imagpart (x)
  `(ccl::%svref ,x x8632::complex.imagpart-cell))

;;;
(defx8632archmacro ccl::%get-single-float-from-double-ptr (ptr offset)
 `(ccl::%double-float->short-float (ccl::%get-double-float ,ptr ,offset)
   (ccl::%alloc-misc 1 x8632::subtag-single-float)))

(defx8632archmacro ccl::codevec-header-p (word)
  (declare (ignore word))
  (error "~s makes no sense on :X8632" 'ccl::codevec-header-p))

(defx8632archmacro ccl::immediate-p-macro (thing)
  (let* ((tag (gensym)))
    `(let* ((,tag (ccl::lisptag ,thing)))
       (declare (fixnum ,tag))
       (or (= ,tag x8632::tag-fixnum)
	   (= ,tag x8632::tag-imm)))))

(defx8632archmacro ccl::hashed-by-identity (thing)
  (let* ((typecode (gensym)))
    `(let* ((,typecode (ccl::typecode ,thing)))
       (declare (fixnum ,typecode))
       (or
	(= ,typecode x8632::tag-fixnum)
	(= ,typecode x8632::tag-imm)
	(= ,typecode x8632::subtag-symbol)
	(= ,typecode x8632::subtag-instance)))))

;;;
(defx8632archmacro ccl::%get-kernel-global (name)
  `(ccl::%fixnum-ref 0 (+ ,(ccl::target-nil-value)
                        ,(%kernel-global
                          (if (ccl::quoted-form-p name)
                            (cadr name)
                            name)))))

(defx8632archmacro ccl::%get-kernel-global-ptr (name dest)
  `(ccl::%setf-macptr
    ,dest
    (ccl::%fixnum-ref-macptr 0 (+ ,(ccl::target-nil-value)
				  ,(%kernel-global
				    (if (ccl::quoted-form-p name)
				      (cadr name)
				      name))))))

(defx8632archmacro ccl::%target-kernel-global (name)
  `(x8632::%kernel-global ,name))

(defx8632archmacro ccl::lfun-vector (fun)
  fun)

(defx8632archmacro ccl::lfun-vector-lfun (lfv)
  lfv)

(defx8632archmacro ccl::area-code ()
  area.code)

(defx8632archmacro ccl::area-succ ()
  area.succ)

(defx8632archmacro ccl::nth-immediate (f i)
  `(ccl::%nth-immediate ,f (the fixnum (- (the fixnum ,i) 1))))

(defx8632archmacro ccl::set-nth-immediate (f i new)
  `(ccl::%set-nth-immediate ,f (the fixnum (- (the fixnum ,i) 1)) ,new))

(defx8632archmacro ccl::symptr->symvector (s)
  s)

(defx8632archmacro ccl::symvector->symptr (s)
  s)

(defx8632archmacro ccl::function-to-function-vector (f)
  f)

(defx8632archmacro ccl::function-vector-to-function (v)
  v)

(defx8632archmacro ccl::with-ffcall-results ((buf) &body body)
  ;; Reserve space for eax,edx,st0 only.
  (let* ((size (+ (* 2 4) (* 1 8))))
    `(ccl::%stack-block ((,buf ,size :clear t))
      ,@body)))

;;; When found at a tagged return address, the instruction
;;; (movl ($ imm32) (% fn))
;;; lets the runtime easily map a return address to the containing
;;; function.
;;;
;;; The notation ($ :self) is used in the assembler to mean "a 32-bit
;;; immediate whose offset will be remembered in a table at the end of
;;; the function object."
;;;
;;; Before the function is made executable (or when the GC moves the
;;; function), these :self immediates are filled in with the actual
;;; address of the function.

(defconstant recover-fn-opcode-byte #b10111111) ;when %fn is %edi
(defconstant recover-fn-address-offset 1)

;;; For backtrace: the relative PC of an argument-check trap
;;; must be less than or equal to this value.  (Because of
;;; the way that we do "anchored" UUOs, it should always be =.)
;;; (maybe not = on x8632)
(defconstant arg-check-trap-pc-limit 7)

(provide "X8632-ARCH")
