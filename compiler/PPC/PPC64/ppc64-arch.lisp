;;;-*- Mode: Lisp; Package: (PPC64 :use CL) -*-
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

;;; This file matches "ccl:lisp-kernel;constants64.h" &
;;; "ccl:lisp-kernel;constants64.s"

(defpackage "PPC64"
  (:use "CL")
  #+ppc64-target
  (:nicknames "TARGET"))


(in-package "PPC64")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant rcontext 2)                ;sigh.  Could use r13+bias on Linux,
                                        ; but Apple hasn't invented tls yet.
(defconstant nbits-in-word 64)
(defconstant least-significant-bit 63)
(defconstant nbits-in-byte 8)
(defconstant ntagbits 4)
(defconstant nlisptagbits 3)
(defconstant nfixnumtagbits 3)          ; See ?
(defconstant nlowtagbits 2)
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
(defconstant ncharcodebits 8)           ;24
(defconstant charcode-shift 8)
(defconstant word-shift 3)
(defconstant word-size-in-bytes 8)
(defconstant node-size word-size-in-bytes)
(defconstant dnode-size 16)
(defconstant dnode-align-bits 4)
(defconstant dnode-shift dnode-align-bits)
(defconstant bitmap-shift 6)

(defconstant target-most-negative-fixnum (ash -1 (1- (- nbits-in-word nfixnumtagbits))))
(defconstant target-most-positive-fixnum (1- (ash 1 (1- (- nbits-in-word nfixnumtagbits)))))
(defmacro define-subtag (name tag value)
  `(defconstant ,(ccl::form-symbol "SUBTAG-" name) (logior ,tag (ash ,value ntagbits))))

;;; PPC64 stuff and tags.

;;; There are several ways to look at the 4 tag bits of any object or
;;; header.  Looking at the low 2 bits, we can classify things as
;;; follows (I'm not sure if we'd ever want to do this) :
;;;
;;;  #b00   a "primary" object: fixnum, cons, uvector
;;;  #b01   an immediate
;;;  #b10   the header on an immediate uvector
;;;  #b11   the header on a node (pointer-containing) uvector
;;
;;;  Note that the ppc64's LD and STD instructions require that the low
;;;  two bits of the constant displacement be #b00.  If we want to use constant
;;;  offsets to access CONS and UVECTOR fields, we're pretty much obligated
;;;  to ensure that CONS and UVECTOR have tags that also end in #b00, and
;;;  fixnum addition and subtraction work better when fixnum tags are all 0.
;;;  We generally have to look at all 4 tag bits before we really know what
;;;  class of "potentially primary" object we're looking at.
;;;  If we look at 3 tag bits, we can see:
;;;
;;;  #b000  fixnum
;;;  #b001  immediate
;;;  #b010  immedate-header
;;;  #b011  node-header
;;;  #b100  CONS or UVECTOR
;;;  #b101  immediate
;;;  #b110  immediate-header
;;;  #b111  node-header
;;;

(defconstant tag-fixnum 0)
(defconstant tag-imm-0 1)
(defconstant tag-immheader-0 2)
(defconstant tag-nodeheader-0 3)
(defconstant tag-memory 4)
(defconstant tag-imm-2 5)
(defconstant tag-immheader2 6)
(defconstant tag-nodeheader2 7)


;;;  Note how we're already winding up with lots of header and immediate
;;;  "classes".  That might actually be useful.
;;
;;;  When we move to 4 bits, we wind up (obviously) with 4 tags of the form
;;;  #bxx00.  There are two partitionings that make (some) sense: we can either
;;;  use 2 of these for (even and odd) fixnums, or we can give NIL a tag
;;;  that's congruent (mod 16) with CONS.  There seem to be a lot of tradeoffs
;;;  involved, but it ultimately seems best to be able to treat 64-bit
;;;  aligned addresses as fixnums: we don't want the VSP to look like a
;;;  vector.   That basically requires that NIL really be a symbol (good
;;;  bye, nilsym) and that we ensure that there are NILs where its CAR and
;;;  CDR would be (-4, 4 bytes from the tagged pointer.)  That means that
;;;  CONS is 4 and UVECTOR is 12, and we have even more immediate/header types.

(defconstant fulltag-even-fixnum    #b0000)
(defconstant fulltag-imm-0          #b0001)
(defconstant fulltag-immheader-0    #b0010)
(defconstant fulltag-nodeheader-0   #b0011)
(defconstant fulltag-cons           #b0100)
(defconstant fulltag-imm-1          #b0101)
(defconstant fulltag-immheader-1    #b0110)
(defconstant fulltag-nodeheader-1   #b0111)
(defconstant fulltag-odd-fixnum     #b1000)
(defconstant fulltag-imm-2          #b1001)
(defconstant fulltag-immheader-2    #b1010)
(defconstant fulltag-nodeheader-2   #b1011)
(defconstant fulltag-misc           #b1100)
(defconstant fulltag-imm-3          #b1101)
(defconstant fulltag-immheader-3    #b1110)
(defconstant fulltag-nodeheader-3   #b1111)

(defconstant lowtagmask (1- (ash 1 nlowtagbits)))
(defconstant lowtag-mask lowtagmask)
(defconstant lowtag-primary 0)
(defconstant lowtag-imm 1)
(defconstant lowtag-immheader 2)
(defconstant lowtag-nodeheader 3)

;;; The general algorithm for determining the (primary) type of an
;;; object is something like:
;;; (clrldi tag node 60)
;;; (cmpwi tag fulltag-misc)
;;; (clrldi tag tag 61)
;;; (bne @done)
;;; (lbz tag misc-subtag-offset node)
;;; @done
;;
;;; That's good enough to identify FIXNUM, "generally immediate", cons,
;;; or a header tag from a UVECTOR.  In some cases, we may need to hold
;;; on to the full 4-bit tag.
;;; In no specific order:
;;; - it's important to be able to quickly recognize fixnums; that's
;;;    simple
;;; - it's important to be able to quickly recognize lists (for CAR/CDR)
;;;   and somewhat important to be able to quickly recognize conses.
;;;   Also simple, though we have to special-case NIL.
;;; - it's desirable to be able to do VECTORP, ARRAYP, and specific-array-type-
;;;   p.  We need at least 12 immediate CL vector types (SIGNED/UNSIGNED-BYTE
;;;   8/16/32/64, SINGLE-FLOAT, DOUBLE-FLOAT, BIT, and at least one CHARACTER;
;;;   we need SIMPLE-ARRAY, VECTOR-HEADER, and ARRAY-HEADER as node
;;;   array types.  That's suspciciously close to 16
;;; - it's desirable to be able (in FUNCALL) to quickly recognize
;;;   functions/symbols/other, and probably desirable to trap on other.
;;;   Pretty much have to do a memory reference and at least one comparison
;;;   here.
;;; - it's sometimes desirable to recognize numbers and distinct numeric
;;;   types (other than FIXNUM) quickly.
;;; - The GC (especially) needs to be able to determine the size of
;;;   ivectors (ivector elements) fairly cheaply.  Most ivectors are CL
;;;   arrays, but code-vectors are fairly common (and have 32-bit elements,
;;;   naturally.)
;;; - We have a fairly large number of non-array gvector types, and it's
;;;   always desirable to have room for expansion.
;;; - we basically have 8 classes of header subtags, each of which has
;;;   16 possible values.  If we stole the high bit of the subtag to
;;;   indicate CL-array-ness, we'd still have 6 bits to encode non-CL
;;;   array types.  

(defconstant cl-array-subtag-bit 7)
(defconstant cl-array-subtag-mask (ash 1 cl-array-subtag-bit))
(defmacro define-cl-array-subtag (name tag value)
  `(defconstant ,(ccl::form-symbol "SUBTAG-" name)
    (logior cl-array-subtag-mask (logior ,tag (ash ,value ntagbits)))))

(define-cl-array-subtag arrayH  fulltag-nodeheader-1 0)
(define-cl-array-subtag vectorH fulltag-nodeheader-2 0)
(define-cl-array-subtag simple-vector fulltag-nodeheader-3 0)
(defconstant min-array-subtag subtag-arrayH)
(defconstant min-vector-subtag subtag-vectorH)

;;;  bits:                         64             32       16    8     1
;;;  CL-array ivector types    DOUBLE-FLOAT     SINGLE    s16   CHAR  BIT
;;;                               s64             s32     u16    s8
;;;                               u64             u32            u8
;;;  Other ivector types       MACPTR           CODE-VECTOR
;;;                            DEAD-MACPTR     XCODE-VECTOR
;;;                                            BIGNUM
;;;                                            DOUBLE-FLOAT
;;; There might possibly be ivectors with 128-bit (VMX/AltiVec) elements
;;; someday, and there might be multiple character sizes (16/32 bits).
;;; That sort of suggests that we use the four immheader classes to
;;; encode the ivector size (64, 32, 8, other) and make BIT an easily-
;;; detected case of OTHER.

(defconstant ivector-class-64-bit fulltag-immheader-3)
(defconstant ivector-class-32-bit fulltag-immheader-2)
(defconstant ivector-class-other-bit fulltag-immheader-1)
(defconstant ivector-class-8-bit fulltag-immheader-0)

(define-cl-array-subtag s64-vector ivector-class-64-bit 1)
(define-cl-array-subtag u64-vector ivector-class-64-bit 2)
(define-cl-array-subtag fixnum-vector ivector-class-64-bit 3)
(define-cl-array-subtag double-float-vector ivector-class-64-bit 4)
(define-cl-array-subtag s32-vector ivector-class-32-bit 1)
(define-cl-array-subtag u32-vector ivector-class-32-bit 2)
(define-cl-array-subtag single-float-vector ivector-class-32-bit 3)
(define-cl-array-subtag simple-base-string ivector-class-32-bit 5)
(define-cl-array-subtag s16-vector ivector-class-other-bit 1)
(define-cl-array-subtag u16-vector ivector-class-other-bit 2)
(define-cl-array-subtag bit-vector ivector-class-other-bit 7)
(define-cl-array-subtag s8-vector ivector-class-8-bit 1)
(define-cl-array-subtag u8-vector ivector-class-8-bit 2)

;;; There's some room for expansion in non-array ivector space.
(define-subtag macptr ivector-class-64-bit 1)
(define-subtag dead-macptr ivector-class-64-bit 2)

(define-subtag code-vector ivector-class-32-bit 0)
(define-subtag xcode-vector ivector-class-32-bit 1)
(define-subtag bignum ivector-class-32-bit 2)
(define-subtag double-float ivector-class-32-bit 3)

;;; Size doesn't matter for non-CL-array gvectors; I can't think of a good
;;; reason to classify them in any particular way.  Let's put funcallable
;;; things in the first slice by themselves, though it's not clear that
;;; that helps FUNCALL much.
(defconstant gvector-funcallable fulltag-nodeheader-0)
(define-subtag function gvector-funcallable 0)
(define-subtag symbol gvector-funcallable 1)

(define-subtag catch-frame fulltag-nodeheader-1 0)
(define-subtag basic-stream fulltag-nodeheader-1 1)
(define-subtag lock fulltag-nodeheader-1 2)
(define-subtag hash-vector fulltag-nodeheader-1 3)
(define-subtag pool fulltag-nodeheader-1 4)
(define-subtag weak fulltag-nodeheader-1 5)
(define-subtag package fulltag-nodeheader-1 6)
(define-subtag slot-vector fulltag-nodeheader-2 0)
(define-subtag instance fulltag-nodeheader-2 1)
(define-subtag struct fulltag-nodeheader-2  2)
(define-subtag istruct fulltag-nodeheader-2  3)
(define-subtag value-cell fulltag-nodeheader-2  4)
(define-subtag xfunction fulltag-nodeheader-2 5)

(define-subtag ratio fulltag-nodeheader-3 0)
(define-subtag complex fulltag-nodeheader-3 1)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "PPC-ARCH")
  (defmacro define-storage-layout (name origin &rest cells)
  `(progn
     (ccl::defenum (:start ,origin :step 8)
       ,@(mapcar #'(lambda (cell) (ccl::form-symbol name "." cell)) cells))
     (defconstant ,(ccl::form-symbol name ".SIZE") ,(* (length cells)
						       8))))
 
(defmacro define-lisp-object (name tagname &rest cells)
  `(define-storage-layout ,name ,(- (symbol-value tagname)) ,@cells))



(defmacro define-fixedsized-object (name &rest non-header-cells)
  `(progn
     (define-lisp-object ,name fulltag-misc header ,@non-header-cells)
     (ccl::defenum ()
       ,@(mapcar #'(lambda (cell) (ccl::form-symbol name "." cell "-CELL")) non-header-cells))
     (defconstant ,(ccl::form-symbol name ".ELEMENT-COUNT") ,(length non-header-cells))))







(defconstant misc-header-offset (- fulltag-misc))
(defconstant misc-subtag-offset (+ misc-header-offset 7 ))
(defconstant misc-data-offset (+ misc-header-offset 8))
(defconstant misc-dfloat-offset (+ misc-header-offset 8))



(define-subtag single-float fulltag-imm-0 0)

(define-subtag character fulltag-imm-1 0)

;;; FULLTAG-IMM-2 is unused, so the only type with lisptag (3-bit tag)
;;; TAG-IMM-0 should be SINGLE-FLOAT.

(define-subtag unbound fulltag-imm-3 0)
(defconstant unbound-marker subtag-unbound)
(defconstant undefined unbound-marker)
(define-subtag slot-unbound fulltag-imm-3 1)
(defconstant slot-unbound-marker subtag-slot-unbound)
(define-subtag illegal fulltag-imm-3 2)
(defconstant illegal-marker subtag-illegal)

(define-subtag no-thread-local-binding fulltag-imm-3 3)
(define-subtag forward-marker fulltag-imm-3 7)


(defconstant max-64-bit-constant-index (ash (+ #x7fff ppc64::misc-dfloat-offset) -3))
(defconstant max-32-bit-constant-index (ash (+ #x7fff ppc64::misc-data-offset) -2))
(defconstant max-16-bit-constant-index (ash (+ #x7fff ppc64::misc-data-offset) -1))
(defconstant max-8-bit-constant-index (+ #x7fff ppc64::misc-data-offset))
(defconstant max-1-bit-constant-index (ash (+ #x7fff ppc64::misc-data-offset) 5))


; The objects themselves look something like this:

; Order of CAR and CDR doesn't seem to matter much - there aren't
; too many tricks to be played with predecrement/preincrement addressing.
; Keep them in the confusing MCL 3.0 order, to avoid confusion.
(define-lisp-object cons fulltag-cons 
  cdr 
  car)


(define-fixedsized-object ratio
  numer
  denom)

;;; It's slightly easier (for bootstrapping reasons)
;;; to view a DOUBLE-FLOAT as being UVECTOR with 2 32-bit elements
;;; (rather than 1 64-bit element).

(defconstant double-float.value misc-data-offset)
(defconstant double-float.value-cell 0)
(defconstant double-float.val-high double-float.value)
(defconstant double-float.val-high-cell double-float.value-cell)
(defconstant double-float.val-low (+ double-float.value 4))
(defconstant double-float.val-low-cell 1)
(defconstant double-float.element-count 2)
(defconstant double-float.size 16)

(define-fixedsized-object complex
  realpart
  imagpart
)


; There are two kinds of macptr; use the length field of the header if you
; need to distinguish between them
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

; Catch frames go on the tstack; they point to a minimal lisp-frame
; on the cstack.  (The catch/unwind-protect PC is on the cstack, where
; the GC expects to find it.)
(define-fixedsized-object catch-frame
  catch-tag                             ; #<unbound> -> unwind-protect, else catch
  link                                  ; tagged pointer to next older catch frame
  mvflag                                ; 0 if single-value, 1 if uwp or multiple-value
  csp                                   ; pointer to control stack
  db-link                               ; value of dynamic-binding link on thread entry.
  save-save7                            ; saved registers
  save-save6
  save-save5
  save-save4
  save-save3
  save-save2
  save-save1
  save-save0
  xframe                                ; exception-frame link
  tsp-segment                           ; mostly padding, for now.
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


(defconstant t-offset (- symbol.size))




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
  static-dnodes                         ; for honsing. etc
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
  single-float-convert			; per-thread scratch space.
  lisp-fpscr-high
  db-link				; special binding chain head 
  catch-top				; top catch frame 
  save-vsp				; VSP when in foreign code 
  save-tsp				; TSP when in foreign code 
  cs-area				; cstack area pointer 
  vs-area				; vstack area pointer 
  ts-area				; tstack area pointer 
  cs-limit				; cstack overflow limit
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

(defconstant tcr.lisp-fpscr-low (+ tcr.lisp-fpscr-high 4))
(defconstant tcr.total-bytes-allocated-low (+ tcr.total-bytes-allocated-high 4))

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

;;; For the eabi port: mark this stack frame as Lisp's (since EABI
;;; foreign frames can be the same size as a lisp frame.)


(ppc64::define-storage-layout lisp-frame 0
  backlink
  savefn
  savelr
  savevsp
)

(ppc64::define-storage-layout c-frame 0
  backlink
  crsave
  savelr
  unused-1
  unused-2
  savetoc
  param0
  param1
  param2
  param3
  param4
  param5
  param6
  param7
)

(defconstant c-frame.minsize c-frame.size)

(defmacro define-header (name element-count subtag)
  `(defconstant ,name (logior (ash ,element-count num-subtag-bits) ,subtag)))

(define-header double-float-header double-float.element-count subtag-double-float)
;;; We could possibly have a one-digit bignum header when dealing
;;; with "small bignums" in some bignum code.  Like other cases of
;;; non-normalized bignums, they should never escape from the lab.
(define-header one-digit-bignum-header 1 subtag-bignum)
(define-header two-digit-bignum-header 2 subtag-bignum)
(define-header three-digit-bignum-header 3 subtag-bignum)
(define-header four-digit-bignum-header 4 subtag-bignum)
(define-header five-digit-bignum-header 5 subtag-bignum)
(define-header symbol-header symbol.element-count subtag-symbol)
(define-header value-cell-header value-cell.element-count subtag-value-cell)
(define-header macptr-header macptr.element-count subtag-macptr)


(defconstant yield-syscall
  #+darwinppc-target -60
  #+linuxppc-target #$__NR_sched_yield)
)
)






(defun %kernel-global (sym)
  (let* ((pos (position sym ppc::*ppc-kernel-globals* :test #'string=)))
    (if pos
      (- (+ symbol.size fulltag-misc (* (1+ pos) word-size-in-bytes)))
      (error "Unknown kernel global : ~s ." sym))))

(defmacro kernel-global (sym)
  (let* ((pos (position sym ppc::*ppc-kernel-globals* :test #'string=)))
    (if pos
      (- (+ symbol.size fulltag-misc (* (1+ pos) word-size-in-bytes)))
      (error "Unknown kernel global : ~s ." sym))))

;;; The kernel imports things that are defined in various other
;;; libraries for us.  The objects in question are generally
;;; fixnum-tagged; the entries in the "kernel-imports" vector are 8
;;; bytes apart.
(ccl::defenum (:prefix "KERNEL-IMPORT-" :start 0 :step word-size-in-bytes)
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
  allocate_tstack
  allocate_vstack
  register_cstack
  raise-thread-interrupt
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
)

(defmacro nrs-offset (name)
  (let* ((pos (position name ppc::*ppc-nilreg-relative-symbols* :test #'eq)))
    (if pos (* (1- pos) symbol.size))))

(defconstant nil-value (+ #x3000 symbol.size fulltag-misc))


(defconstant reservation-discharge #x2008)

(defparameter *ppc64-target-uvector-subtags*
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
    (:basic-stream . ,subtag-basic-stream)
    (:slot-vector . ,subtag-slot-vector)
    (:simple-string . ,subtag-simple-base-string )
    (:bit-vector . ,subtag-bit-vector )
    (:signed-8-bit-vector . ,subtag-s8-vector )
    (:unsigned-8-bit-vector . ,subtag-u8-vector )
    (:signed-16-bit-vector . ,subtag-s16-vector )
    (:unsigned-16-bit-vector . ,subtag-u16-vector )
    (:signed-32-bit-vector . ,subtag-s32-vector )
    (:unsigned-32-bit-vector . ,subtag-u32-vector )
    (:fixnum-vector . ,subtag-fixnum-vector)
    (:signed-64-bit-vector . ,subtag-s64-vector)
    (:unsigned-64-bit-vector . ,subtag-u64-vector)    
    (:single-float-vector . ,subtag-single-float-vector)
    (:double-float-vector . ,subtag-double-float-vector )
    (:simple-vector . ,subtag-simple-vector )
    (:vector-header . ,subtag-vectorH)
    (:array-header . ,subtag-arrayH)))

;;; This should return NIL unless it's sure of how the indicated
;;; type would be represented (in particular, it should return
;;; NIL if the element type is unknown or unspecified at compile-time.
(defun ppc64-array-type-name-from-ctype (ctype)
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
                (cond ((or (null low) (null high))
                       :simple-vector)
                      ((and (>= low 0) (<= high 1))
                       :bit-vector)
                      ((and (>= low 0) (<= high 255))
                       :unsigned-8-bit-vector)
                      ((and (>= low 0) (<= high 65535))
                       :unsigned-16-bit-vector)
                      ((and (>= low 0) (<= high #xffffffff))
                       :unsigned-32-bit-vector)
                      ((and (>= low 0) (<= high #xffffffffffffffff))
                       :unsigned-64-bit-vector)
                      ((and (>= low -128) (<= high 127))
                       :signed-8-bit-vector)
                      ((and (>= low -32768) (<= high 32767))
                       :signed-16-bit-vector)
                      ((and (>= low (ash -1 31)) (<= high (1- (ash 1 31))))
                       :signed-32-bit-vector)
                      ((and (>= low target-most-negative-fixnum)
                            (<= high target-most-positive-fixnum))
                       :fixnum-vector)
                      ((and (>= low (ash -1 63)) (<= high (1- (ash 1 63))))
                       :signed-64-bit-vector)
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
        (t)))))

(defun ppc64-misc-byte-count (subtag element-count)
  (declare (fixnum subtag))
  (if (= lowtag-nodeheader (logand subtag lowtagmask))
    (ash element-count 3)
    (case (logand subtag fulltagmask)
      (#.ivector-class-64-bit (ash element-count 3))
      (#.ivector-class-32-bit (ash element-count 2))
      (#.ivector-class-8-bit element-count)
      (t
       (if (= subtag subtag-bit-vector)
         (ash (+ 7 element-count) -3)
         (ash element-count 1))))))

(defparameter *ppc64-target-arch*
  (arch::make-target-arch :name :ppc64
                          :lisp-node-size 8
                          :nil-value nil-value
                          :fixnum-shift fixnumshift
                          :most-positive-fixnum (1- (ash 1 (1- (- 64 fixnumshift))))
                          :most-negative-fixnum (- (ash 1 (1- (- 64 fixnumshift))))
                          :misc-data-offset misc-data-offset
                          :misc-dfloat-offset misc-dfloat-offset
                          :nbits-in-word 64
                          :ntagbits 4
                          :nlisptagbits 3
                          :uvector-subtags *ppc64-target-uvector-subtags*
                          :max-64-bit-constant-index max-64-bit-constant-index
                          :max-32-bit-constant-index max-32-bit-constant-index
                          :max-16-bit-constant-index max-16-bit-constant-index
                          :max-8-bit-constant-index max-8-bit-constant-index
                          :max-1-bit-constant-index max-1-bit-constant-index
                          :word-shift 3
                          :code-vector-prefix '(#$"CODE")
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
                                                  :double-float
                                                  :bignum
                                                  :simple-string)
                          :64-bit-ivector-types '(:double-float-vector
                                                  :unsigned-64-bit-vector
                                                  :signed-64-bit-vector
                                                  :fixnum-vector)
                          :array-type-name-from-ctype-function
                          #'ppc64-array-type-name-from-ctype
                          :package-name "PPC64"
                          :t-offset t-offset
                          :array-data-size-function #'ppc64-misc-byte-count
                          :numeric-type-name-to-typecode-function
                          #'(lambda (type-name)
                              (ecase type-name
                                (fixnum tag-fixnum)
                                (bignum subtag-bignum)
                                ((short-float single-float) subtag-single-float)
                                ((long-float double-float) subtag-double-float)
                                (ratio subtag-ratio)
                                (complex subtag-complex)))
                                                    :subprims-base ppc::*ppc-subprims-base*
                          :subprims-shift ppc::*ppc-subprims-shift*
                          :subprims-table ppc::*ppc-subprims*
                          :primitive->subprims `(((0 . 23) . ,(ccl::%subprim-name->offset '.SPbuiltin-plus ppc::*ppc-subprims*)))
                          :unbound-marker-value unbound-marker
                          :slot-unbound-marker-value slot-unbound-marker
                          :fixnum-tag tag-fixnum
                          :single-float-tag subtag-single-float
                          :single-float-tag-is-subtag nil
                          :double-float-tag subtag-double-float
                          :cons-tag fulltag-cons
                          :null-tag subtag-symbol
                          :symbol-tag subtag-symbol
                          :symbol-tag-is-subtag t
                          :function-tag subtag-function
                          :function-tag-is-subtag t
                          :big-endian t
                          :misc-subtag-offset misc-subtag-offset
                          :car-offset cons.car
                          :cdr-offset cons.cdr
                          :subtag-char subtag-character
                          :charcode-shift charcode-shift
                          :fulltagmask fulltagmask
                          :fulltag-misc fulltag-misc
                          :char-code-limit #x110000
                          ))

;;; arch macros
(defmacro defppc64archmacro (name lambda-list &body body)
  `(arch::defarchmacro :ppc64 ,name ,lambda-list ,@body))

(defppc64archmacro ccl::%make-sfloat ()
  (error "~s shouldn't be used in code targeting :PPC64" 'ccl::%make-sfloat))

(defppc64archmacro ccl::%make-dfloat ()
  `(ccl::%alloc-misc ppc64::double-float.element-count ppc64::subtag-double-float))

(defppc64archmacro ccl::%numerator (x)
  `(ccl::%svref ,x ppc64::ratio.numer-cell))

(defppc64archmacro ccl::%denominator (x)
  `(ccl::%svref ,x ppc64::ratio.denom-cell))

(defppc64archmacro ccl::%realpart (x)
  `(ccl::%svref ,x ppc64::complex.realpart-cell))
                    
(defppc64archmacro ccl::%imagpart (x)
  `(ccl::%svref ,x ppc64::complex.imagpart-cell))

;;;
(defppc64archmacro ccl::%get-single-float-from-double-ptr (ptr offset)
 `(ccl::%double-float->short-float (ccl::%get-double-float ,ptr ,offset)))

(defppc64archmacro ccl::codevec-header-p (word)
  `(eql ,word #$"CODE"))

;;;

(defppc64archmacro ccl::immediate-p-macro (thing)
  (let* ((tag (gensym)))
    `(let* ((,tag (ccl::lisptag ,thing)))
      (declare (fixnum ,tag))
      (or (= ,tag ppc64::tag-fixnum)
       (= (logand ,tag ppc64::lowtagmask) ppc64::lowtag-imm)))))

(defppc64archmacro ccl::hashed-by-identity (thing)
  (let* ((typecode (gensym)))
    `(let* ((,typecode (ccl::typecode ,thing)))
      (declare (fixnum ,typecode))
      (or
       (= ,typecode ppc64::tag-fixnum)
       (= (logand ,typecode ppc64::lowtagmask) ppc64::lowtag-imm)
       (= ,typecode ppc64::subtag-symbol)
       (= ,typecode ppc64::subtag-instance)))))

;;;
(defppc64archmacro ccl::%get-kernel-global (name)
  `(ccl::%fixnum-ref 0 (+ ppc64::nil-value
                                 ,(%kernel-global
                                   (if (ccl::quoted-form-p name)
                                     (cadr name)
                                     name)))))

(defppc64archmacro ccl::%get-kernel-global-ptr (name dest)
  `(ccl::%setf-macptr
    ,dest
    (ccl::%fixnum-ref-macptr 0 (+ ppc64::nil-value
                                 ,(%kernel-global
                                   (if (ccl::quoted-form-p name)
                                     (cadr name)
                                     name))))))

(defppc64archmacro ccl::%target-kernel-global (name)
  `(ppc64::%kernel-global ,name))

(defppc64archmacro ccl::lfun-vector (fn)
  fn)

(defppc64archmacro ccl::lfun-vector-lfun (lfv)
  lfv)

(defppc64archmacro ccl::area-code ()
  area.code)

(defppc64archmacro ccl::area-succ ()
  area.succ)


(defppc64archmacro ccl::nth-immediate (f i)
  `(ccl::%svref ,f ,i))

(defppc64archmacro ccl::set-nth-immediate (f i new)
  `(setf (ccl::%svref ,f ,i) ,new))


(defppc64archmacro ccl::symptr->symvector (s)
  s)

(defppc64archmacro ccl::symvector->symptr (s)
  s)

(defppc64archmacro ccl::function-to-function-vector (f)
  f)

(defppc64archmacro ccl::function-vector-to-function (v)
  v)

(defppc64archmacro ccl::with-ffcall-results ((buf) &body body)
  (let* ((size (+ (* 8 8) (* 13 8))))
    `(ccl::%stack-block ((,buf ,size))
      ,@body)))

(defconstant arg-check-trap-pc-limit 8)
                              
(provide "PPC64-ARCH")
