;;;-*- Mode: Lisp; Package: (PPC32 :use CL) -*-
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


;; This file matches "ccl:pmcl;constants.h" & "ccl:pmcl;constants.s"

(defpackage "PPC32"
  (:use "CL")
  #+ppc32-target
  (:nicknames "TARGET"))

(in-package "PPC32")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "PPC-ARCH")

  
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

  
)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant rcontext 13)  
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

;; PPC-32 stuff and tags.

;; Tags.
;; There are two-bit tags and three-bit tags.
;; A FULLTAG is the value of the low three bits of a tagged object.
;; A TAG is the value of the low two bits of a tagged object.
;; A TYPECODE is either a TAG or the value of a "tag-misc" object's header-byte.

;; There are 4 primary TAG values.  Any object which lisp can "see" can be classified 
;; by its TAG.  (Some headers have FULLTAGS that are congruent modulo 4 with the
;; TAGS of other objects, but lisp can't "see" headers.)
(ccl::defenum ()
  tag-fixnum                            ; All fixnums, whether odd or even
  tag-list                              ; Conses and NIL
  tag-misc                              ; Heap-consed objects other than lists: vectors, symbols, functions, floats ...
  tag-imm                               ; Immediate-objects: characters, UNBOUND, other markers.
)

;;; And there are 8 FULLTAG values.  Note that NIL has its own FULLTAG (congruent mod 4 to tag-list),
;;; that FULLTAG-MISC is > 4 (so that code-vector entry-points can be branched to, since the low
;;; two bits of the PC are ignored) and that both FULLTAG-MISC and FULLTAG-IMM have header fulltags
;;; that share the same TAG.
;;; Things that walk memory (and the stack) have to be careful to look at the FULLTAG of each
;;; object that they see.
(ccl::defenum ()
  fulltag-even-fixnum                   ; I suppose EVENP/ODDP might care; nothing else does.
  fulltag-cons                          ; a real (non-null) cons.  Shares TAG with fulltag-nil.
  fulltag-nodeheader                    ; Header of heap-allocated object that contains lisp-object pointers
  fulltag-imm                           ; a "real" immediate object.  Shares TAG with fulltag-immheader.
  fulltag-odd-fixnum                    ; 
  fulltag-nil                           ; NIL and nothing but.  (Note that there's still a hidden NILSYM.)
  fulltag-misc                          ; Pointer "real" tag-misc object.  Shares TAG with fulltag-nodeheader.
  fulltag-immheader                     ; Header of heap-allocated object that contains unboxed data.
)

(defconstant misc-header-offset (- fulltag-misc))
(defconstant misc-subtag-offset (+ misc-header-offset 3))
(defconstant misc-data-offset (+ misc-header-offset 4))
(defconstant misc-dfloat-offset (+ misc-header-offset 8))






(defconstant canonical-nil-value #x00003015)
;;; T is almost adjacent to NIL: since NIL is a misaligned CONS, it spans
;;; two doublewords.  The arithmetic difference between T and NIL is
;;; such that the least-significant bit and exactly one other bit is
;;; set in the result.

(defconstant t-offset (+ 8 (- 8 fulltag-nil) fulltag-misc))
(assert (and (logbitp 0 t-offset) (= (logcount t-offset) 2)))

;;; The order in which various header values are defined is significant in several ways:
;;; 1) Numeric subtags precede non-numeric ones; there are further orderings among numeric subtags.
;;; 2) All subtags which denote CL arrays are preceded by those that don't,
;;;    with a further ordering which requires that (< header-arrayH header-vectorH ,@all-other-CL-vector-types)
;;; 3) The element-size of ivectors is determined by the ordering of ivector subtags.
;;; 4) All subtags are >= fulltag-immheader .


;;; Numeric subtags.
(define-imm-subtag bignum 0)
(defconstant min-numeric-subtag subtag-bignum)
(define-node-subtag ratio 1)
(defconstant max-rational-subtag subtag-ratio)

(define-imm-subtag single-float 1)          ; "SINGLE" float, aka short-float in the new order.
(define-imm-subtag double-float 2)
(defconstant min-float-subtag subtag-single-float)
(defconstant max-float-subtag subtag-double-float)
(defconstant max-real-subtag subtag-double-float)

(define-node-subtag complex 3)
(defconstant max-numeric-subtag subtag-complex)

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

;;; So, we get the remaining subtags (n: (n > max-numeric-subtag) & (n < min-array-subtag))
;;; for various immediate/node object types.

(define-imm-subtag macptr 3)
(defconstant min-non-numeric-imm-subtag subtag-macptr)
(assert (> min-non-numeric-imm-subtag max-numeric-subtag))
(define-imm-subtag dead-macptr 4)
(define-imm-subtag code-vector 5)
(define-imm-subtag creole-object 6)
(define-imm-subtag xcode-vector 7)  ; code-vector for cross-development

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

(define-subtag character fulltag-imm 9)
(define-subtag vsp-protect fulltag-imm 7)
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


(defconstant max-64-bit-constant-index (ash (+ #x7fff ppc32::misc-dfloat-offset) -3))
(defconstant max-32-bit-constant-index (ash (+ #x7fff ppc32::misc-data-offset) -2))
(defconstant max-16-bit-constant-index (ash (+ #x7fff ppc32::misc-data-offset) -1))
(defconstant max-8-bit-constant-index (+ #x7fff ppc32::misc-data-offset))
(defconstant max-1-bit-constant-index (ash (+ #x7fff ppc32::misc-data-offset) 5))


;;; The objects themselves look something like this:

;;; Order of CAR and CDR doesn't seem to matter much - there aren't
;;; too many tricks to be played with predecrement/preincrement addressing.
;;; Keep them in the confusing MCL 3.0 order, to avoid confusion.
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
  val-low)

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

;;; Catch frames go on the tstack; they point to a minimal lisp-frame
;;; on the cstack.  (The catch/unwind-protect PC is on the cstack, where
;;; the GC expects to find it.)
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
  lisp-fpscr-high
  lisp-fpscr-low
  db-link				; special binding chain head 
  catch-top				; top catch frame 
  save-vsp				; VSP when in foreign code 
  save-tsp				; TSP when in foreign code 
  cs-area				; cstack area pointer 
  vs-area				; vstack area pointer 
  ts-area				; tstack area pointer 
  cs-limit				; cstack overflow limit
  total-bytes-allocated-high
  total-bytes-allocated-low
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

;;; For the eabi port: mark this stack frame as Lisp's (since EABI
;;; foreign frames can be the same size as a lisp frame.)


(ppc32::define-storage-layout lisp-frame 0
  backlink
  savefn
  savelr
  savevsp
)

(ppc32::define-storage-layout c-frame 0
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

;;; .SPeabi-ff-call "shrinks" this frame after loading the GPRs.
(ppc32::define-storage-layout eabi-c-frame 0
  backlink
  savelr
  param0
  param1
  param2
  param3
  param4
  param5
  param6
  param7
)

(defconstant eabi-c-frame.minsize eabi-c-frame.size)

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

(defconstant yield-syscall
  #+darwinppc-target -60
  #+linuxppc-target #$__NR_sched_yield)
)




(defun %kernel-global (sym)
  (let* ((pos (position sym ppc::*ppc-kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-nil (* (1+ pos) 4)))
      (error "Unknown kernel global : ~s ." sym))))

(defmacro kernel-global (sym)
  (let* ((pos (position sym ppc::*ppc-kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-nil (* (1+ pos) 4)))
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
  (let* ((pos (position name ppc::*ppc-nilreg-relative-symbols* :test #'eq)))
    (if pos (+ t-offset (* pos symbol.size)))))


(defconstant reservation-discharge #x2004)



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

(defparameter *ppc32-target-uvector-subtags*
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
    (:array-header . ,subtag-arrayH)))


;;; This should return NIL unless it's sure of how the indicated
;;; type would be represented (in particular, it should return
;;; NIL if the element type is unknown or unspecified at compile-time.
(defun ppc32-array-type-name-from-ctype (ctype)
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
        
(defun ppc32-misc-byte-count (subtag element-count)
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

(defparameter *ppc32-target-arch*
  (arch::make-target-arch :name :ppc32
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
                          :uvector-subtags *ppc32-target-uvector-subtags*
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
                          #'ppc32-array-type-name-from-ctype
                          :package-name "PPC32"
                          :t-offset t-offset
                          :array-data-size-function #'ppc32-misc-byte-count
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
                          :single-float-tag-is-subtag t
                          :double-float-tag subtag-double-float
                          :cons-tag fulltag-cons
                          :null-tag fulltag-nil
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
(defmacro defppc32archmacro (name lambda-list &body body)
  `(arch::defarchmacro :ppc32 ,name ,lambda-list ,@body))

(defppc32archmacro ccl::%make-sfloat ()
  `(ccl::%alloc-misc ppc32::single-float.element-count ppc32::subtag-single-float))

(defppc32archmacro ccl::%make-dfloat ()
  `(ccl::%alloc-misc ppc32::double-float.element-count ppc32::subtag-double-float))

(defppc32archmacro ccl::%numerator (x)
  `(ccl::%svref ,x ppc32::ratio.numer-cell))

(defppc32archmacro ccl::%denominator (x)
  `(ccl::%svref ,x ppc32::ratio.denom-cell))

(defppc32archmacro ccl::%realpart (x)
  `(ccl::%svref ,x ppc32::complex.realpart-cell))
                    
(defppc32archmacro ccl::%imagpart (x)
  `(ccl::%svref ,x ppc32::complex.imagpart-cell))

;;;
(defppc32archmacro ccl::%get-single-float-from-double-ptr (ptr offset)
 `(ccl::%double-float->short-float (ccl::%get-double-float ,ptr ,offset)
   (ccl::%alloc-misc 1 ppc32::subtag-single-float)))

(defppc32archmacro ccl::codevec-header-p (word)
  `(eql ppc32::subtag-code-vector
    (logand ,word ppc32::subtag-mask)))

(defppc32archmacro ccl::immediate-p-macro (thing)
  (let* ((tag (gensym)))
    `(let* ((,tag (ccl::lisptag ,thing)))
      (declare (fixnum ,tag))
      (or (= ,tag ppc32::tag-fixnum)
       (= ,tag ppc32::tag-imm)))))

(defppc32archmacro ccl::hashed-by-identity (thing)
  (let* ((typecode (gensym)))
    `(let* ((,typecode (ccl::typecode ,thing)))
      (declare (fixnum ,typecode))
      (or
       (= ,typecode ppc32::tag-fixnum)
       (= ,typecode ppc32::tag-imm)
       (= ,typecode ppc32::subtag-symbol)
       (= ,typecode ppc32::subtag-instance)))))

;;;
(defppc32archmacro ccl::%get-kernel-global (name)
  `(ccl::%fixnum-ref 0 (+ ,(ccl::target-nil-value)
                        ,(%kernel-global
                          (if (ccl::quoted-form-p name)
                            (cadr name)
                            name)))))

(defppc32archmacro ccl::%get-kernel-global-ptr (name dest)
  `(ccl::%setf-macptr
    ,dest
    (ccl::%fixnum-ref-macptr 0 (+ ,(ccl::target-nil-value)
                                ,(%kernel-global
                                  (if (ccl::quoted-form-p name)
                                    (cadr name)
                                    name))))))

(defppc32archmacro ccl::%target-kernel-global (name)
  `(ppc32::%kernel-global ,name))

(defppc32archmacro ccl::lfun-vector (fn)
  fn)

(defppc32archmacro ccl::lfun-vector-lfun (lfv)
  lfv)

(defppc32archmacro ccl::area-code ()
  area.code)

(defppc32archmacro ccl::area-succ ()
  area.succ)

(defppc32archmacro ccl::nth-immediate (f i)
  `(ccl::%svref ,f ,i))

(defppc32archmacro ccl::set-nth-immediate (f i new)
  `(setf (ccl::%svref ,f ,i) ,new))

(defppc32archmacro ccl::symptr->symvector (s)
  s)

(defppc32archmacro ccl::symvector->symptr (s)
  s)

(defppc32archmacro ccl::function-to-function-vector (f)
  f)

(defppc32archmacro ccl::function-vector-to-function (v)
  v)

(defppc32archmacro ccl::with-ffcall-results ((buf) &body body)
  (let* ((size (+ (* 8 4) (* 31 8))))
    `(%stack-block ((,buf ,size))
      ,@body)))

(defconstant arg-check-trap-pc-limit 8)

(defconstant fasl-version #x5f)
(defconstant fasl-max-version #x5f)
(defconstant fasl-min-version #x5e)
(defparameter *image-abi-version* 1038)

(provide "PPC32-ARCH")
