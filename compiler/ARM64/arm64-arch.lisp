(defpackage "ARM64"
  (:use "CL")
  #+arm64-target
  (:nicknames "TARGET"))

(require "ARCH")

(in-package "ARM64")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *register-names* ())
(defvar *registers* (make-hash-table :test #'equalp))  

(defun lookup-register (name)
  (let* ((pair (assoc (string name) *register-names* :test #'string-equal)))
    (when pair
      (cdr pair))))

;;; This allows redefinition, which might be helpful while
;;; boostrapping.  ARM64-LAP-EQUATE-FORM checks for redefinition
;;; before calling this.
(defun define-register (name val)
  (let* ((value (if (typep val 'fixnum) val (lookup-register val)))
         (string (string name)))
    (unless value
      (error "invalid ARM64 register value ~d for ~s." val name))
    (let* ((pair (assoc string *register-names* :test #'string-equal)))
      (if pair
        (progn
          (unless (eql (cdr pair) value)
            (when ccl::*cerror-on-constant-redefinition*
              (cerror "Redefine ARM64 register ~s to have value ~*~d."
                      "ARM64 register ~s currently has value ~d."
                      name (cdr pair) value)
              (setf (cdr pair) value))))
        (push (cons string value) *register-names*))
      value)))

(defmacro defgpr (name val)
  `(defconstant ,name (define-register ',name ',val)))

;;; 64-bit general-purpose registers
(defgpr x0 0)
(defgpr x1 1)
(defgpr x2 2)
(defgpr x3 3)
(defgpr x4 4)
(defgpr x5 5)
(defgpr x6 6)
(defgpr x7 7)
(defgpr x8 8)
(defgpr x9 9)
(defgpr x10 10)
(defgpr x11 11)
(defgpr x12 12)
(defgpr x13 13)
(defgpr x14 14)
(defgpr x15 15)
(defgpr x16 16)
(defgpr x17 17)
(defgpr x18 18)                         ;reserved platform register
(defgpr x19 19)
(defgpr x20 20)
(defgpr x21 21)
(defgpr x22 22)
(defgpr x23 23)
(defgpr x24 24)
(defgpr x25 25)
(defgpr x26 26)
(defgpr x27 27)
(defgpr x28 28)
(defgpr x29 29)                         ;frame pointer
(defgpr x30 30)                         ;link register

;;; Lisp names
(defgpr imm0 x0)                        ;unboxed, volatile registers
(defgpr imm1 x1)
(defgpr imm2 x2)
(defgpr imm3 x3)
(defgpr imm4 x4)
(defgpr imm5 x5)
;; x6
(defgpr fn x7)
(defgpr nfn x8)

(defgpr arg_x x10)                      ;next-to-next-to-last argument
(defgpr arg_y x11)                      ;next-to-late argument
(defgpr arg_z x12)                      ;last argument

(defgpr temp0 x13)                      ;boxed, volatile registers
(defgpr temp1 x14)                      ; Some may be defined on function
(defgpr temp2 x15)                      ; entry.
(defgpr temp3 x16)
;; x17
;; x18 permanantly reserved (at least on Darwin)
(defgpr save0 x19)                      ;boxed, non-volatile registers
(defgpr save1 x20)
(defgpr save2 x21)
(defgpr save3 x22)

(defgpr rnil x23)
(defgpr tsp x24)                        ;temp stack pointer
(defgpr vsp x25)                        ;value stack pointer
(defgpr allocptr x26)
(defgpr allocbase x27)
(defgpr rcontext x28)                   ;per-thread data

;;; A 32-bit W register is the lower half of the corresponding 64-bit
;;; X register.  Reads from W registers ignore the upper 32 bits of
;;; the corresponding X register and leave them unchanged.  Writes to
;;; W registers clear the upper 32 bits of the corresponding X register.
(defgpr w0 32)
(defgpr w1 33)
(defgpr w2 34)
(defgpr w3 35)
(defgpr w4 36)
(defgpr w5 37)
(defgpr w6 38)
(defgpr w7 39)
(defgpr w8 40)
(defgpr w9 41)
(defgpr w10 42)
(defgpr w11 43)
(defgpr w12 44)
(defgpr w13 45)
(defgpr w14 46)
(defgpr w15 47)
(defgpr w16 48)
(defgpr w17 49)
(defgpr w18 50)
(defgpr w19 51)
(defgpr w20 52)
(defgpr w21 53)
(defgpr w22 54)
(defgpr w23 55)
(defgpr w24 56)
(defgpr w25 57)
(defgpr w26 58)
(defgpr w27 59)
(defgpr w28 60)
(defgpr w29 61)
(defgpr w30 62)

(defmacro deffpr (name val)
  `(defconstant ,name (define-arm-register ',name ',val)))

;;; There are 32 SIMD&FP registers (v0 to v31), each of which is 128 bits long.
;;; Each register can be accessed as:
;;;  a 128-bit register named q0 to q31
;;;  a 64-bit register named d0 to d31
;;;  a 32-bit register named s0 to s31
;;;  a 16-bit register named h0 to h31
;;;  an 8-bit register named b0 to b31
;;;  a 128-bit vector of elements
;;;  a 64-bit vector of elements
;;;
;;; When the number of bits described by a register name does not occupy
;;; an entire SIMD&FP register, it refers to the least significant bits.

;;; single-float register names
(deffpr s0 64)
(deffpr s1 65)
(deffpr s2 66)
(deffpr s3 67)
(deffpr s4 68)
(deffpr s5 69)
(deffpr s6 70)
(deffpr s7 71)
(deffpr s8 72)
(deffpr s9 73)
(deffpr s10 74)
(deffpr s11 75)
(deffpr s12 76)
(deffpr s13 77)
(deffpr s14 78)
(deffpr s15 79)
(deffpr s16 80)
(deffpr s17 81)
(deffpr s18 82)
(deffpr s19 83)
(deffpr s20 84)
(deffpr s21 85)
(deffpr s22 86)
(deffpr s23 87)
(deffpr s24 88)
(deffpr s25 89)
(deffpr s26 90)
(deffpr s27 91)
(deffpr s28 92)
(deffpr s29 93)
(deffpr s30 94)
(deffpr s31 95)

;;; double-float register names
(deffpr d0 96)
(deffpr d1 97)
(deffpr d2 98)
(deffpr d3 99)
(deffpr d4 100)
(deffpr d5 101)
(deffpr d6 102)
(deffpr d7 103)
(deffpr d8 104)
(deffpr d9 105)
(deffpr d10 106)
(deffpr d11 107)
(deffpr d12 108)
(deffpr d13 109)
(deffpr d14 110)
(deffpr d15 111)
(deffpr d16 112)
(deffpr d17 113)
(deffpr d18 114)
(deffpr d19 115)
(deffpr d20 116)
(deffpr d21 117)
(deffpr d22 118)
(deffpr d23 119)
(deffpr d24 120)
(deffpr d25 121)
(deffpr d26 122)
(deffpr d27 123)
(deffpr d28 124)
(deffpr d29 125)
(deffpr d30 126)
(deffpr d31 127)

(defparameter *standard-register-names* *register-names*)

;;; Kernel globals are allocated below nil.  This list must, of course,
;;; exactly match the lisp kernel's notion of where things are.
;;; See "ccl:lisp-kernel;lisp_globals.h" and "ccl:lisp-kernel;constants.s"
(defparameter *kernel-globals*
  '(get-tcr                             ;callback to obtain (real) tcr
    tcr-count
    interrupt-signal                    ; used by PROCESS-INTERRUPT
    kernel-imports                      ; some things we need to have imported for us.
    objc-2-personality                  ;
    savetoc                             ; used to save TOC on some platforms
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
    kernel-path                         ;
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
(defparameter *nil-relative-symbols*
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

(defconstant nbits-in-word 64)
(defconstant nbits-in-byte 8)
(defconstant ntagbits 4)
(defconstant nlisptagbits 3)
(defconstant nfixnumtagbits 3)
(defconstant num-subtag-bits 8)
(defconstant fixnumshift 3)
(defconstant fixnum-shift 3)
(defconstant fulltagmask 15)
(defconstant tagmask 7)
(defconstant fixnummask 7)
(defconstant ncharcodebits 8)
(defconstant charcode-shift 8)
(defconstant word-shift 3)
(defconstant word-size-in-bytes 8)
(defconstant node-size word-size-in-bytes)
(defconstant dnode-size 16)
(defconstant dnode-align-bits 4)
(defconstant dnode-shift dnode-align-bits)
(defconstant bitmap-shift 6)

(defconstant fixnumone (ash 1 fixnumshift))
(defconstant fixnum-one fixnumone)
(defconstant fixnum1 fixnumone)

(defconstant target-most-negative-fixnum
  (ash -1 (1- (- nbits-in-word nfixnumtagbits))))
(defconstant target-most-positive-fixnum
  (1- (ash 1 (1- (- nbits-in-word nfixnumtagbits)))))

;;; 3-bit "tag" or "lisptag" values
(defconstant tag-fixnum 0)
(defconstant tag-imm-0 1)               ;subtag-single-float ONLY
(defconstant tag-imm-1 2)               ;subtag-character, internal markers
(defconstant tag-list 3)                ;fulltag-cons or NIL
(defconstant tag-function 4)            ;function entry point
(defconstant tag-misc 5)                ;random uvector
(defconstant tag-symbol 6)              ;non-null symbol
(defconstant tag-tbd 7)                 ;

;;; 4-bit "fulltag" values
(defconstant fulltag-even-fixnum  #b0000)
(defconstant fulltag-imm-0        #b0001) ;single-float
(defconstant fulltag-imm-1        #b0010) ;characters, markers
(defconstant fulltag-cons         #b0011) ;a real (non-null) cons
(defconstant fulltag-tbd-0        #b0100)
(defconstant fulltag-nodeheader-0 #b0101)
(defconstant fulltag-nodeheader-1 #b0110)
(defconstant fulltag-immheader-0  #b0111) ;other-bit ivector
(defconstant fulltag-odd-fixnum   #b1000)
(defconstant fulltag-immheader-1  #b1001) ;32-bit ivector
(defconstant fulltag-immheader-2  #b1010) ;64-bit ivector
(defconstant fulltag-nil          #b1011) ;NIL and nothing but
(defconstant fulltag-function     #b1100) ;something we can branch to directly
(defconstant fulltag-misc         #b1101)
(defconstant fulltag-symbol       #b1110)
(defconstant fulltag-tbd          #b1111)

;;; The ARM64 has fixed-length instructions, so it should be possible
;;; to recover the containing lisp object given a pc-locative into that
;;; object (such as the PC or LR).  There is thus no need to keep the
;;; current function in a GPR because the GC can recover the function
;;; object from a pc-locative.
;;;
;;; An instruction with the top 16 bits clear is architecturally
;;; undefined.  If we limit the element count of a function vector to
;;; 40 bits (leaving the top 16 bits clear), then we can scan
;;; backwards from a given pc-locative until we find a word with the
;;; upper 16 bits clear (which will be the most-significant half of
;;; the uvector header.
;;;
;;;        6                   5                     1    
;;;  3 2 1 0 9 8 7 6 5 4 3 2 1 0       7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+ ... +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; |                                   element count     |    subtag     |
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+ ... +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;
;;; The bad thing about this is that we won't be able to use the UDF
;;; instruction (top 16 bits clear, 16-bit immediate in lower-half)
;;; for UUOs.  Maybe we use the BRK instruction for UUOs (but would
;;; have to be careful not to step on gdb/lldb)? Or maybe we use the
;;; tag of #b1100 for functions (see below), and use the word between
;;; the uvector header and the first instruction as a sentinel word to
;;; look for when scanning backwards.
;;;
;;; ARM64 also supports PC-relative addressing with a displacement of
;;; +/- 1MB, so there's no strong reason to have separate code-vector
;;; and function objects.  There's no need to have a dedicated fn register.
;;; If we use a concatenated function object that contains both code
;;; and constants, we should be able to use that pc-relative addressing
;;; mode to access the function's constants.

;;; We want to be able to call the function by loading it into a node
;;; register and doing a br or blr to branch to it.  Unlike PowerPC,
;;; the ARM64 will fault if the low two bits of the PC are not clear.
;;; Thus, a function has to have a tag with the low two bits clear as
;;; well.  #b0100 or #b1100 are thus the candidates (recall that
;;; #b0000 and #b1000 are even fixnum and odd fixnum, respectively).
;;; If we use #b0100, then the first instruction word comes right
;;; after the uvector header.  If we use #b1100, then that leaves 4
;;; bytes between the header and the first instruction word, and we
;;; might profitably put something there (perhaps the instruction word
;;; count), with the proviso that the 4 bytes in question must not
;;; look like an undefined instruction (i.e., the upper 16 bits must
;;; not all be zero).  Maybe we store the instruction word count in
;;; negated form.  Or if we decide we can't reliably recognize the
;;; uvector header, maybe we make those 4 bytes be an appropriate UDF
;;; instruction, and put the instruction word count, if needed, somewhere
;;; else (perhaps in the last word of the uvector so that you'd get it, e.g.
;;; via (uvref (1- (uvsize f)))
;;;

;;; https://forums.developer.apple.com/forums/thread/655950

;;; "Modifying pagezero_size isn't a supportable option in the arm64
;;; environment. arm64 code must be in an ASLR binary, which using a
;;; custom pagezero_size is incompatible with. An ASLR binary encodes
;;; signed pointers using a large random size along with the expected
;;; page zero size, and this combination is going to extend beyond the
;;; range of values covered in the lower 32-bits. Further, even if
;;; that did work, 32-bit pointers are completely incompatible with
;;; the arm64e architecture, which is available as a preview
;;; technology."


(defconstant fulltag-single-float fulltag-imm-0)

;; could maybe use fulltag-tbd for ivector-class-8-bit?
(defconstant ivector-class-other-bit fulltag-immheader-0)
(defconstant ivector-class-32-bit fulltag-immheader-1)
(defconstant ivector-class-64-bit fulltag-immheader-2)

;;; ...

(defmacro define-subtag (name tag value)
  `(defconstant ,(ccl::form-symbol "SUBTAG-" name)
     (logior ,tag (ash ,value ntagbits))))

(define-subtag double-float ivector-class
) ;eval-when

(defmacro define-storage-layout (name origin &rest cells)
  `(progn
     (ccl::defenum (:start ,origin :step 8)
                   ,@(mapcar #'(lambda (cell)
                                 (ccl::form-symbol name "." cell))
                             cells))
     (defconstant ,(ccl::form-symbol name ".SIZE") ,(* (length cells) 8))))

(defmacro define-lisp-object (name tagname &rest cells)
  `(define-storage-layout ,name ,(- (symbol-value tagname)) ,@cells))

(defmacro define-fixedsized-object (name (&optional (fulltag 'fulltag-misc))
                                         &rest non-header-cells)
  `(progn
     (define-lisp-object ,name ,fulltag header ,@non-header-cells)
     (ccl::defenum ()
       ,@(mapcar #'(lambda (cell)
                     (ccl::form-symbol name "." cell "-CELL"))
                 non-header-cells))
     (defconstant ,(ccl::form-symbol name ".ELEMENT-COUNT")
       ,(length non-header-cells))))

;;; Order of CAR and CDR doesn't seem to matter much - there aren't
;;; too many tricks to be played with predecrement/preincrement addressing.
;;; Keep them in the confusing MCL 3.0 order, to avoid confusion.
(define-lisp-object cons fulltag-cons 
  cdr 
  car)

(define-fixedsized-object ratio ()
  numer
  denom)

(define-fixedsize-object double-float ()
  value)

(define-fixedsized-object complex ()
  realpart
  imagpart)




;;; Kernel globals are allocated "below" nil.  This list (used to map
;;; symbolic names to rnil-relative offsets) must (of course) exactly
;;; match the kernel's notion of where things are.
;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" & the
;;; lisp_globals record in "ccl:lisp-kernel;*constants*.s"
(defparameter *arm64-kernel-globals*
  '(get-tcr                            ; callback to obtain (real) tcr
    tcr-count
    interrupt-signal    ; used by PROCESS-INTERRUPT
    kernel-imports      ; some things we need to have imported for us.
    objc-2-personality
    savetoc                  ; used to save TOC on some platforms
    saver13                             ; used to save r13 on some platforms
    subprims-base                       ; start of dynamic subprims jump table
    ret1valaddr                         ; magic multiple-values return address.
    tcr-key                             ; tsd key for thread's tcr
    area-lock                           ; serialize access to gc
    exception-lock			; serialize exception handling
    static-conses                       ; when FREEZE is in effect
    default-allocation-quantum   ;log2_heap_segment_size, as a fixnum.
    intflag                      ;interrupt-pending flag
    gc-inhibit-count             ;for gc locking
    refbits                      ;oldspace refbits
    oldspace-dnode-count         ;number of dnodes in dynamic space that are older than
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
(defparameter *arm64-nilreg-relative-symbols*
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


(defparameter *arm64-target-arch*
  (arch::make-target-arch :name :arm
                          :lisp-node-size 8
                          :nil-value canonical-nil-value
                          :fixnum-shift fixnumshift
                          :most-positive-fixnum (1- (ash 1 (1- (- 64 fixnumshift))))
                          :most-negative-fixnum (- (ash 1 (1- (- 64 fixnumshift))))
                          :misc-data-offset misc-data-offset
                          :misc-dfloat-offset misc-dfloat-offset
                          :nbits-in-word 64
                          :ntagbits 4
                          :nlisptagbits 3
                          :uvector-subtags *arm64-target-uvector-subtags*
                          :max-64-bit-constant-index max-64-bit-constant-index
                          :max-32-bit-constant-index max-32-bit-constant-index
                          :max-16-bit-constant-index max-16-bit-constant-index
                          :max-8-bit-constant-index max-8-bit-constant-index
                          :max-1-bit-constant-index max-1-bit-constant-index
                          :word-shift 3
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
                                                  :double-float
                                                  :bignum
                                                  :simple-string)
                          :64-bit-ivector-types '(:double-float-vector
                                                  :complex-single-float-vector
                                                  :unsigned-64-bit-vector
                                                  :signed-64-bit-vector
                                                  :fixnum-vector)
                          :array-type-name-from-ctype-function
                          #'arm64-array-type-name-from-ctype
                          :package-name "ARM64"
                          :t-offset t-offset
                          :array-data-size-function #'arm64-misc-byte-count
                          :fpr-mask-function 'arm64-fpr-mask
                          :subprims-base arm64::subprims-base*
                          :subprims-shift arm64::*subprims-shift*
                          :subprims-table arm64::*subprims*
                          :primitive->subprims `(((0 . 23) . ,(ccl::%subprim-name->offset '.SPbuiltin-plus arm64::*subprims*)))
                          :unbound-marker-value unbound-marker
                          :slot-unbound-marker-value slot-unbound-marker
                          :fixnum-tag tag-fixnum
                          :single-float-tag subtag-single-float
                          :single-float-tag-is-subtag nil
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
                          ))

(provide "ARM64-ARCH")
