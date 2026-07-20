;;;; -*- Mode: Lisp; Package: (ARM64 :use CL) -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(defpackage "ARM64"
  (:use "CL" "CCL")
  #+arm64-target
  (:nicknames "TARGET"))

(require "ARCH")
(in-package "ARM64")

;;; For register definitions, see arm64-asm.lisp.

;;; Tagging scheme
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant nbits-in-word 64)
(defconstant nbits-in-byte 8)
(defconstant ntagbits 4)                ;"fulltag" bits
(defconstant nlisptagbits 3)            ;"lisptag" bits
(defconstant nfixnumtagbits 3)
(defconstant num-subtag-bits 8)         ;in uvector headers
(defconstant fixnumshift 3)
(defconstant fixnum-shift 3)
(defconstant fulltagmask 15)
(defconstant tagmask 7)
(defconstant fixnummask 7)
(defconstant subtag-mask (1- (ash 1 num-subtag-bits)))
(defconstant ncharcodebits 8)           ;not exactly
(defconstant charcode-shift 8)
(defconstant word-shift 3)
(defconstant word-size-in-bytes 8)
(defconstant node-size word-size-in-bytes)
(defconstant dnode-size 16)
(defconstant dnode-align-bits 4)
(defconstant dnode-shift dnode-align-bits)
(defconstant bitmap-shift 6)

(defconstant fixnumone (ash 1 fixnumshift))
(defconstant target-most-negative-fixnum
  (ash -1 (1- (- nbits-in-word nfixnumtagbits)))) ;-2^{n-1}
(defconstant target-most-positive-fixnum
  (1- (ash 1 (1- (- nbits-in-word nfixnumtagbits))))) ;2^{n-1] - 1

;;; 3-bit "tag" or "lisptag" values
(defconstant tag-fixnum       #b000) ;all fixnums, whether odd or even
(defconstant tag-single-float #b001) ;single-float (and nothing but)
(defconstant tag-imm          #b010) ;characters, markers, etc.
(defconstant tag-list         #b011) ;cons cell or nil
(defconstant tag-4            #b100) ;miscobj and immheader-0
(defconstant tag-5            #b101) ;immheader-1 and immheader-2
(defconstant tag-nodeheader   #b110) ;nodeheader-0 and nodeheader-1
(defconstant tag-7            #b111) ;fulltag-{symbol,function}

;;; 4-bit "fulltag" values
(defconstant fulltag-even-fixnum  #b0000)
(defconstant fulltag-single-float #b0001)
(defconstant fulltag-imm-0        #b0010) ;characters
(defconstant fulltag-cons         #b0011) ;a cons cell
(defconstant fulltag-immheader-0  #b0100)
(defconstant fulltag-immheader-1  #b0101)
(defconstant fulltag-nodeheader-0 #b0110)
(defconstant fulltag-symbol       #b0111)
(defconstant fulltag-odd-fixnum   #b1000)
(defconstant fulltag-reserved     #b1001) ;reserved (for single-float)
(defconstant fulltag-imm-1        #b1010) ;markers
(defconstant fulltag-nil          #b1011) ;nil and nothing but
(defconstant fulltag-misc         #b1100) ;uvector/miscobj (see note below)
(defconstant fulltag-immheader-2  #b1101)
(defconstant fulltag-nodeheader-1 #b1110)
(defconstant fulltag-function     #b1111)

;;; Note on fulltag-misc: the value (12) was selected deliberately.
;;; This allows us to branch directly to a tagged code-vector pointer:
;;; we land right on the first real instruction in the code-vector
;;; (word 0 is the udf #0 sentinel prefix).

;;; The numeric order of subtags matters.
;;; * A gvector array subtag must be >= subtag-array-header
;;; * An ivector array subtag must >= min-cl-ivector-subtag
;;; * A gvector vector subtag must be >= subtag-vector-header

(defconstant subtag-single-float fulltag-single-float)

(defmacro define-subtag (name tag value)
  `(defconstant ,(ccl::form-symbol "SUBTAG-" name)
     (logior ,tag (ash ,value ntagbits))))

;;; gvector array subtags
(define-subtag arrayH fulltag-nodeheader-0 10)
(define-subtag vectorH fulltag-nodeheader-1 10)
(define-subtag simple-vector fulltag-nodeheader-1 11)

(defconstant ivector-class-64-bit fulltag-immheader-2)
(defconstant ivector-class-32-bit fulltag-immheader-1)
(defconstant ivector-class-other-bit fulltag-immheader-0)

(define-subtag complex-single-float-vector ivector-class-64-bit 11)
(define-subtag fixnum-vector ivector-class-64-bit 12)
(define-subtag s64-vector ivector-class-64-bit 13)
(define-subtag u64-vector ivector-class-64-bit 14)
(define-subtag double-float-vector ivector-class-64-bit 15)

(define-subtag simple-base-string ivector-class-32-bit 12)
(define-subtag s32-vector ivector-class-32-bit 13)
(define-subtag u32-vector ivector-class-32-bit 14)
(define-subtag single-float-vector ivector-class-32-bit 15)

(define-subtag complex-double-float-vector ivector-class-other-bit 9)
(defconstant min-cl-ivector-subtag subtag-complex-double-float-vector)
(define-subtag s16-vector ivector-class-other-bit 10)
(define-subtag u16-vector ivector-class-other-bit 11)
(define-subtag s8-vector ivector-class-other-bit 13)
(defconstant min-8-bit-ivector-subtag subtag-s8-vector)
(define-subtag u8-vector ivector-class-other-bit 14)
(defconstant max-8-bit-ivector-subtag subtag-u8-vector)
(define-subtag bit-vector ivector-class-other-bit 15)

;;; A few sanity tests
(eval-when (:compile-toplevel)
  ;; ivector header fulltags must be ordered according to element size
  (assert (< ivector-class-other-bit ivector-class-32-bit
             ivector-class-64-bit))
  ;; CL ivector subtags must be >= min-cl-ivector-subtag
  (assert (every #'(lambda (s) (>= s min-cl-ivector-subtag))
               (list subtag-complex-single-float-vector
                     subtag-fixnum-vector
                     subtag-s64-vector
                     subtag-u64-vector
                     subtag-double-float-vector
                     subtag-simple-base-string
                     subtag-s32-vector
                     subtag-u32-vector
                     subtag-single-float-vector
                     subtag-complex-double-float-vector
                     subtag-s16-vector
                     subtag-u16-vector
                     subtag-s8-vector
                     subtag-u8-vector
                     subtag-bit-vector)))
  ;; required ordering for CL gvector types
  (assert (< subtag-arrayH subtag-vectorH subtag-simple-vector)))

(define-subtag macptr ivector-class-64-bit 1)
(define-subtag dead-macptr ivector-class-64-bit 2)
;; additional non-array subtags up to 8 are available for expansion

(define-subtag bignum ivector-class-32-bit 1)
(define-subtag double-float ivector-class-32-bit 2)
(define-subtag xcode-vector ivector-class-32-bit 3)
(define-subtag complex-single-float ivector-class-32-bit 4)
(define-subtag complex-double-float ivector-class-32-bit 5)
(define-subtag code-vector ivector-class-32-bit 6)
;; additional non-array subtags up to 8 are available for expansion

(define-subtag symbol fulltag-nodeheader-0 1)
(define-subtag catch-frame fulltag-nodeheader-0 2)
(define-subtag hash-vector fulltag-nodeheader-0 3)
(define-subtag pool fulltag-nodeheader-0 4)
(define-subtag weak fulltag-nodeheader-0 5)
(define-subtag package fulltag-nodeheader-0 6)
(define-subtag slot-vector fulltag-nodeheader-0 7)
(define-subtag basic-stream fulltag-nodeheader-0 8)
(define-subtag function fulltag-nodeheader-0 9)

(define-subtag ratio fulltag-nodeheader-1 1)
(define-subtag complex fulltag-nodeheader-1 2)
(define-subtag struct fulltag-nodeheader-1 3)
(define-subtag istruct fulltag-nodeheader-1 4)
(define-subtag value-cell fulltag-nodeheader-1 5)
(define-subtag xfunction fulltag-nodeheader-1 6)
(define-subtag lock fulltag-nodeheader-1 7)
(define-subtag instance fulltag-nodeheader-1 8)

(define-subtag character fulltag-imm-0 0)

(define-subtag unbound fulltag-imm-1 1)
(defconstant unbound-marker subtag-unbound)
(defconstant undefined unbound-marker)
(define-subtag slot-unbound fulltag-imm-1 2)
(defconstant slot-unbound-marker subtag-slot-unbound)
(define-subtag illegal fulltag-imm-1 3)
(defconstant illegal-marker subtag-illegal)
(define-subtag no-thread-local-binding fulltag-imm-1 4)
(defconstant no-thread-local-binding-marker subtag-no-thread-local-binding)
(define-subtag lisp-frame-marker fulltag-imm-1 5)
(defconstant lisp-frame-marker subtag-lisp-frame-marker)

;;; Extended type codes ("xtypes") for wrong-type UUOs.  The 8-bit
;;; expected-type field of a wrong-type UUO holds either a lisptag, a
;;; fulltag, a uvector subtag byte, or an xtype code.  These type
;;; codes all go into a single 256-entry namespace, so they must not
;;; conflict.
;;;
;;; These values are used in *arm64-xtype-specifiers* and duplicated
;;; in lisp-kernel/arm64-uuo.s (and they must, of course, match what
;;; we list here).
;;;
;;; Two kinds of code already occupy that namespace:
;;;
;;;  * The bare lisptag/fulltag bytes #x00-#x0f.
;;;  * Real uvector subtags, of the form (fulltag | (index << ntagbits)).
;;;
;;; The xtype codes below avoid conflict with the above codes by using
;;; a high nibble of at least 1 (avoiding potential conflict with the
;;; entries for lisptag/fulltag values), and a low nibble of
;;; fulltag-odd-fixnum (8) or fulltag-even-fixnum (0).  A subtag can
;;; never be fixnum-tagged, so these xtype codes cannot conflict with
;;; any defined subtags.

(defconstant xtype-integer #x18)
(defconstant xtype-s64 #x28)
(defconstant xtype-u64 #x38)
(defconstant xtype-s32 #x48)
(defconstant xtype-u32 #x58)
(defconstant xtype-s16 #x68)
(defconstant xtype-u16 #x78)
(defconstant xtype-s8 #x88)
(defconstant xtype-u8 #x98)
(defconstant xtype-bit #xa8)
(defconstant xtype-rational #xb8)
(defconstant xtype-real #xc8)
(defconstant xtype-number #xd8)
(defconstant xtype-cons #xe8)   ;a real cons
                                ;#xf8 free
(defconstant xtype-char-code #x10)
(defconstant xtype-unsigned-byte-24 #x20)
(defconstant xtype-array2d #x30)
(defconstant xtype-array3d #x40)
(defconstant xtype-null #x50)

;;; A sanity check: no synthetic xtype may collide with a real subtag
;;; byte or with a bare tag code (#x00-#x0f).
(eval-when (:compile-toplevel)
  (let ((non-fixnum-low-nibbles '(#| 0 |# 1 2 3 4 5 6 7
                                  #| 8 |# 9 10 11 12 13 14 15)))
    (dolist (xt (list xtype-integer xtype-s64 xtype-u64 xtype-s32 xtype-u32
                      xtype-s16 xtype-u16 xtype-s8 xtype-u8 xtype-bit
                      xtype-rational xtype-real xtype-number xtype-cons
                      xtype-char-code xtype-unsigned-byte-24
                      xtype-array2d xtype-array3d xtype-null))
      (assert (>= xt #x10))
      (assert (not (member (logand xt fulltagmask) non-fixnum-low-nibbles))))))

(defconstant canonical-nil-value (+ #x13000 fulltag-nil)) ;xxx nil can't be a constant
(defconstant canonical-t-value (+ #x13020 fulltag-symbol)) ;xxx see above
(defconstant misc-bias fulltag-misc)
(defconstant cons-bias fulltag-cons)
(defconstant t-offset (- canonical-t-value canonical-nil-value))

(defconstant misc-header-offset (- fulltag-misc))
(defconstant misc-data-offset (+ misc-header-offset node-size))
(defconstant misc-subtag-offset misc-header-offset)
(defconstant misc-dfloat-offset misc-data-offset)

(defconstant misc-symbol-offset (- node-size fulltag-symbol))
(defconstant misc-function-offset (- node-size fulltag-function))

;;; There is a pad word after the uvector header so that the
;;; complex-double-float elements are 16-byte aligned.
(defconstant misc-complex-dfloat-offset (+ misc-data-offset node-size))

;;; There are two variants of base + immediate addressing: a 9-bit
;;; signed byte offset (-256 to 255, used with ldur), and a 12-bit
;;; unsigned offset scaled by the access size (so the byte offset must
;;; be non-negative and a multiple of that size).
;;;
;;; fulltag-misc is 12, which makes misc-data-offset -4.  Therefore,
;;; although we have to use the unscaled form for index 0, index 1 and
;;; up sit at a non-negative multiple of the access size for 32-bit
;;; and lower access sizes, so we can use the scaled form for them.
;;;
;;; To use the scaled offset for 64-bit elements, misc-data-offset
;;; would have to be a multiple of 8: fulltag-misc would need to be
;;; one of #bx000 and that's not possible because those tags are used
;;; for fixnums.
;;;
;;; This is probably not that important: we could likely just use the
;;; unscaled offset in all cases and not be greatly inconvenienced.

(defconstant max-64-bit-constant-index (ash (- #xff misc-data-offset) -3))
(defconstant max-32-bit-constant-index (ash (- (ash #xfff 2) misc-data-offset)
                                            -2))
(defconstant max-16-bit-constant-index (ash (- (ash #xfff 1) misc-data-offset)
                                            -1))
(defconstant max-8-bit-constant-index (- #xfff misc-data-offset))
;; Assuming we index bit-vector  memory by bytes
(defconstant max-1-bit-constant-index (ash (- #xfff misc-data-offset) 3))
) ; eval-when


;;; Kernel globals are allocated "below" nil.  This list (used to map
;;; symbolic names to rnil-relative offsets) must (of course) exactly
;;; match the kernel's notion of where things are.
;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" & the
;;; lisp_globals record in "ccl:lisp-kernel;*constants*.s"
(defparameter *kernel-globals*
  '(get-tcr                      ;callback to obtain (real) tcr
    tcr-count
    interrupt-signal             ;used by PROCESS-INTERRUPT
    kernel-imports               ;some things we need to have imported for us
    objc-2-personality
    savetoc                      ;used to save TOC on some platforms
    saver13                      ;used to save r13 on some platforms
    subprims-base                ;start of dynamic subprims jump table
    ret1valaddr                  ;magic multiple-values return address.
    tcr-key                      ;tsd key for thread's tcr
    area-lock                    ;serialize access to gc
    exception-lock               ;serialize exception handling
    static-conses                ;when FREEZE is in effect
    default-allocation-quantum   ;log2_heap_segment_size, as a fixnum.
    intflag                      ;interrupt-pending flag
    gc-inhibit-count             ;for gc locking
    refbits                      ;oldspace refbits
    oldspace-dnode-count         ;number of dnodes in dynamic space that are
                                 ; older than youngest generation
    float-abi                    ;non-zero if using hard float abi
    fwdnum                       ;fixnum: GC "forwarder" call count.
    gc-count                     ;fixnum: GC call count.
    gcable-pointers              ;linked-list of weak macptrs.
    heap-start                   ;start of lisp heap
    heap-end                     ;end of lisp heap
    statically-linked            ;true if the lisp kernel is statically linked
    stack-size                   ;value of --stack-size arg
    objc-2-begin-catch           ;objc_begin_catch
    kernel-path
    all-areas                    ;doubly-linked area list
    lexpr-return                 ;multiple-value lexpr return address
    lexpr-return1v               ;single-value lexpr return address
    in-gc                        ;non-zero when GC-ish thing active
    free-static-conses           ;fixnum
    objc-2-end-catch             ;_objc_end_catch
    short-float-zero             ;low half of 1.0d0
    double-float-one             ;high half of 1.0d0
    static-cons-area
    exception-saved-registers    ;saved registers from exception frame
    oldest-ephemeral             ;dnode address of oldest ephemeral object or 0
    tenured-area                 ;the tenured_area
    errno                        ;address of C lib errno
    argv                         ;address of C lib argv
    host-platform                ;0 on MacOS, 1 on Linux, 2 on VxWorks ...
    batch-flag                   ;non-zero if --batch specified
    unwind-resume                ;_Unwind_Resume
    weak-gc-method               ;weak gc algorithm.
    image-name                   ;current image name
    initial-tcr                  ;initial thread's context record
    weakvll                      ;all populations as of last GC
    ))

;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" and the nrs record
;;; in "ccl:lisp-kernel;*constants*.s".
(defparameter *nilreg-relative-symbols*
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

;; The idea here is that the subprim address table will be referenced
;; relative to rcontext.  The lisp kernel will make sure that every
;; thread's TCR will contain the table.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *subprims-shift* 3)
  (defconstant tcr.spare 336)
  (defconstant tcr.sptab 496)
  (defparameter *subprims-base* tcr.sptab))

(defvar *subprims*)

(let ((offset *subprims-base*)
      (step (ash 1 *subprims-shift*)))
  (flet ((define-subprim (name)
           (ccl::make-subprimitive-info :name (string name)
                                        :offset (prog1
                                                    offset
                                                  (incf offset step)))))
    (macrolet ((defsubprim (name)
                 `(define-subprim ',name)))
      (setq *subprims*
            (vector
             (defsubprim .SPbuiltin-plus)
             (defsubprim .SPbuiltin-minus)
             (defsubprim .SPbuiltin-times)
             (defsubprim .SPbuiltin-div)
             (defsubprim .SPbuiltin-eq)
             (defsubprim .SPbuiltin-ne)
             (defsubprim .SPbuiltin-gt)
             (defsubprim .SPbuiltin-ge)
             (defsubprim .SPbuiltin-lt)
             (defsubprim .SPbuiltin-le)
             (defsubprim .SPbuiltin-eql)
             (defsubprim .SPbuiltin-length)
             (defsubprim .SPbuiltin-seqtype)
             (defsubprim .SPbuiltin-assq)
             (defsubprim .SPbuiltin-memq)
             (defsubprim .SPbuiltin-logbitp)
             (defsubprim .SPbuiltin-logior)
             (defsubprim .SPbuiltin-logand)
             (defsubprim .SPbuiltin-ash)
             (defsubprim .SPbuiltin-negate)
             (defsubprim .SPbuiltin-logxor)
             (defsubprim .SPbuiltin-aref1)
             (defsubprim .SPbuiltin-aset1)
             (defsubprim .SPfuncall)
             (defsubprim .SPmkcatch1v)
             (defsubprim .SPmkcatchmv)
             (defsubprim .SPmkunwind)
             (defsubprim .SPbind)
             (defsubprim .SPconslist)
             (defsubprim .SPconslist-star)
             (defsubprim .SPmakes32)
             (defsubprim .SPmakeu32)
             (defsubprim .SPfix-overflow)
             (defsubprim .SPmakeu64)
             (defsubprim .SPmakes64)
             (defsubprim .SPmvpass)
             (defsubprim .SPvalues)
             (defsubprim .SPnvalret)
             (defsubprim .SPthrow)
             (defsubprim .SPnthrowvalues)
             (defsubprim .SPnthrow1value)
             (defsubprim .SPbind-self)
             (defsubprim .SPbind-nil)
             (defsubprim .SPbind-self-boundp-check)
             (defsubprim .SPrplaca)
             (defsubprim .SPrplacd)
             (defsubprim .SPgvset)
             (defsubprim .SPset-hash-key)
             (defsubprim .SPstore-node-conditional)
             (defsubprim .SPset-hash-key-conditional)
             (defsubprim .SPstkconslist)
             (defsubprim .SPstkconslist-star)
             (defsubprim .SPmkstackv)
             (defsubprim .SPsetqsym)
             (defsubprim .SPprogvsave)
             (defsubprim .SPstack-misc-alloc)
             (defsubprim .SPgvector)
             (defsubprim .SPfitvals)
             (defsubprim .SPnthvalue)
             (defsubprim .SPdefault-optional-args)
             (defsubprim .SPopt-supplied-p)
             (defsubprim .SPheap-rest-arg)
             (defsubprim .SPreq-heap-rest-arg)
             (defsubprim .SPheap-cons-rest-arg)
             (defsubprim .SPcheck-fpu-exception)
             (defsubprim .SPdiscard-stack-object)
             (defsubprim .SPksignalerr)
             (defsubprim .SPstack-rest-arg)
             (defsubprim .SPreq-stack-rest-arg)
             (defsubprim .SPstack-cons-rest-arg)
             (defsubprim .SPcall-closure)
             (defsubprim .SPspreadargz)
             (defsubprim .SPtfuncallgen)
             (defsubprim .SPtfuncallslide)
             (defsubprim .SPjmpsym)
             (defsubprim .SPtcallsymgen)
             (defsubprim .SPtcallsymslide)
             (defsubprim .SPtcallnfngen)
             (defsubprim .SPtcallnfnslide)
             (defsubprim .SPmisc-ref)
             (defsubprim .SPsubtag-misc-ref)
             (defsubprim .SPmakestackblock)
             (defsubprim .SPmakestackblock0)
             (defsubprim .SPmakestacklist)
             (defsubprim .SPstkgvector)
             (defsubprim .SPmisc-alloc)
             (defsubprim .SPatomic-incf-node)
             (defsubprim .SPrecover-values)
             (defsubprim .SPinteger-sign)
             (defsubprim .SPsubtag-misc-set)
             (defsubprim .SPmisc-set)
             (defsubprim .SPspread-lexprz)
             (defsubprim .SPreset)
             (defsubprim .SPmvslide)
             (defsubprim .SPsave-values)
             (defsubprim .SPadd-values)
             (defsubprim .SPmisc-alloc-init)
             (defsubprim .SPstack-misc-alloc-init)
             (defsubprim .SPpopj)
             (defsubprim .SPgetu64)
             (defsubprim .SPgets64)
             (defsubprim .SPspecref)
             (defsubprim .SPspecrefcheck)
             (defsubprim .SPspecset)
             (defsubprim .SPgets32)
             (defsubprim .SPgetu32)
             (defsubprim .SPmvpasssym)
             (defsubprim .SPunbind)
             (defsubprim .SPunbind-n)
             (defsubprim .SPunbind-to)
             (defsubprim .SPprogvrestore)
             (defsubprim .SPbind-interrupt-level-0)
             (defsubprim .SPbind-interrupt-level-m1)
             (defsubprim .SPbind-interrupt-level)
             (defsubprim .SPunbind-interrupt-level)
             (defsubprim .SParef2)
             (defsubprim .SParef3)
             (defsubprim .SPaset2)
             (defsubprim .SPaset3)
             (defsubprim .SPkeyword-bind)
             (defsubprim .SPffcall)
             (defsubprim .SPdebind)
             (defsubprim .SPcallback)
             )))))

(defun subprimitive-offset (name)
  (when (and name (or (symbolp name) (stringp name)))
    (let ((info (find name *subprims* :test #'string-equal
                                      :key  #'ccl::subprimitive-info-name)))
      (when info
        (ccl::subprimitive-info-offset info)))))

;;; Memory layout of Lisp objects

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

;;; It seems like by now we ought to be able to say
;;; (define-fixedsized-object double-float ()
;;;   value)
;;; But, cargo-cult this forward anyway...
;;;
;;; It's slightly easier (for bootstrapping reasons)
;;; to view a DOUBLE-FLOAT as being UVECTOR with 2 32-bit elements
;;; (rather than 1 64-bit element).

(defconstant double-float.value misc-data-offset)
(defconstant double-float.value-cell 0)
(defconstant double-float.val-low double-float.value)
(defconstant double-float.val-low-cell 0)
(defconstant double-float.val-high (+ double-float.value 4))
(defconstant double-float.val-high-cell 1)
(defconstant double-float.element-count 2)
(defconstant double-float.size 16)

(define-fixedsized-object complex ()
  realpart
  imagpart)

(define-fixedsized-object complex-single-float ()
  value)
(defconstant complex-single-float.realpart complex-single-float.value)
(defconstant complex-single-float.imagpart (+ complex-single-float.value 4))

(define-fixedsized-object complex-double-float ()
  pad                                   ;for natural alignment
  realpart
  imagpart)

;;; There are two kinds of macptr; use the length field of the header if you
;;; need to distinguish between them
(define-fixedsized-object macptr ()
  address
  domain
  type)

(define-fixedsized-object xmacptr ()
  address
  domain
  type
  flags
  link)

;;; XXX no idea about this for ARM64 right now
;;; Catch frames go on the cstack, below a lisp frame whose savelr
;;; field references the catch exit point/unwind-protect cleanup code.
(define-fixedsized-object catch-frame ()
  link                 ;tagged pointer to next older catch frame
  mvflag               ;0 if single-value, 1 if uwp or multiple-value
  catch-tag            ;#<unbound> -> unwind-protect, else catch
  db-link              ;value of dynamic-binding link on thread entry.
  xframe               ;exception-frame link
  last-lisp-frame
  nfp)

(define-fixedsized-object lock ()
  _value                         ;finalizable pointer to kernel object
  kind                           ; '0 = recursive-lock, '1 = rwlock
  writer                         ;tcr of owning thread or 0
  name
  whostate
  whostate-2)


;;; Symbols have their own fulltag, but they're otherwise just like a
;;; miscobj.  We can convert between the differently- tagged
;;; references with %symptr->symvector and %symvector->symptr.

;;; If we're referencing a miscobj, we can use this:
(define-fixedsized-object symptr ()
  pname
  vcell
  fcell
  package-predicate
  flags
  plist
  binding-index)

;;; And for the symbol-tagged case, we use this:
(define-fixedsized-object symbol (fulltag-symbol)
  pname
  vcell
  fcell
  package-predicate
  flags
  plist
  binding-index)

(define-fixedsized-object function (fulltag-function)
  code-vector
  constants
  ;; constants and metadata follow
  )

(define-fixedsized-object vectorH ()
  logsize             ;fill pointer if there is one, physsize otherwise
  physsize            ;total size of (possibly displaced) data vector
  data-vector         ;object this header describes
  displacement        ;true displacement or 0
  flags               ;has-fill-pointer, displaced-to, adjustable bits;
                      ; subtype of underlying simple vector.
)

(define-lisp-object arrayH fulltag-misc
  header              ;subtag = subtag-arrayH
  rank                ;NEVER 1
  physsize            ;total size of (possibly displaced) data vector
  data-vector         ;object this header describes
  displacement        ;true displacement or 0
  flags               ;has-fill-pointer, displaced-to, adjustable bits;
                      ;  subtype of underlying simple vector.
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

(define-fixedsized-object value-cell ()
  value)

;;; The lisp kernel uses these (rather generically named) structures
;;; to keep track of various memory regions it (or the lisp) is
;;; interested in.  This definition must match lisp-kernel/area.h.
(define-storage-layout area 0
  pred                     ;pointer to preceding area in DLL
  succ                     ;pointer to next area in DLL
  low                      ;inclusive lower limit on area addresses
  high                     ;exclusive upper limit on area addresses
  active                   ;low limit on stacks, high limit on heaps
  softlimit                ;overflow bound
  hardlimit                ;another one
  code                     ;an area-code; see below
  markbits                 ;bit vector for GC
  ndnodes                  ;"active" size of dynamic area or stack
  older                    ;in EGC sense
  younger                  ;also for EGC
  h                        ;Handle or null pointer
  softprot                 ;protected_area structure pointer
  hardprot                 ;another one.
  owner                    ;fragment (library) which "owns" the area
  refbits                  ;bitvector for intergenerational refernces
  threshold                ;for egc
  gc-count                 ;generational gc count.
  static-dnodes            ;for honsing, etc
  static-used              ;bitvector
  refidx                   ;compressed refbits
)

(define-storage-layout protected-area 0
  next
  start            ;first byte (page-aligned) that might be protected
  end              ;last byte (page-aligned) that could be protected
  nprot            ;could be 0
  protsize         ;number of bytes to protect
  why)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant tcr-bias 0))

(define-storage-layout tcr (- tcr-bias)
  ;; this next/prev order is correct: other ports are wrong
  next                            ;in doubly-linked list
  prev                            ;in doubly-linked list
  db-link                         ;special binding chain head
  catch-top                       ;top catch frame
  last-lisp-frame
  save-vsp                        ;vsp when in foreign code
  save-tsp                        ;tsp when in foreign code
  cs-area                         ;cstack area pointer
  vs-area                         ;vstack area pointer
  ts-area                         ;tstack area pointer
  cs-limit                        ;cstack overflow limit
  total-bytes-allocated           ;
  log2-allocation-quantum         ;unboxed
  interrupt-pending               ;fixnum
  xframe                          ;exception frame linked list
  errno-loc                       ;thread-private, maybe
  foreign-fpsr                    ;fpscr bits from ff-call.
  osid                            ;OS thread id
  valence                         ;odd when in foreign code
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
  suspend                         ;semaphore for suspension notify
  resume                          ;sempahore for resumption notify
  flags                           ;foreign, being reset, ...
  gc-context
  termination-semaphore
  unwinding
  tlb-limit
  tlb-pointer
  shutdown-count
  safe-ref-address
  io-datum                        ;Darwin: Mach thread exception port
  nfp
  ;; spare slots plus sptab follow
  )

(assert (= tcr.spare tcr.size))
(assert (= tcr.sptab (+ tcr.spare (* 20 arm64::node-size))))

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
  malloced-ptr)

(defmacro define-header (name element-count subtag)
  `(defconstant ,name (logior (ash ,element-count num-subtag-bits) ,subtag)))

(define-header double-float-header
  double-float.element-count subtag-double-float)

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


;;; On the Mac, we can't have a static area at a fixed address.
;;;
;;; On other ports, nil is basically a really popular constant, and it
;;; happens to be a pointer to a fixed address in low-ish memory.
;;;
;;; So, it looks like we're going to have to bring back nilreg.
;;;
;;;
;;; https://forums.developer.apple.com/forums/thread/655950
;;;
;;; "Modifying pagezero_size isn't a supportable option in the arm64
;;; environment. arm64 code must be in an ASLR binary, which using a
;;; custom pagezero_size is incompatible with. An ASLR binary encodes
;;; signed pointers using a large random size along with the expected
;;; page zero size, and this combination is going to extend beyond the
;;; range of values covered in the lower 32-bits. Further, even if
;;; that did work, 32-bit pointers are completely incompatible with
;;; the arm64e architecture, which is available as a preview
;;; technology."

(defun %kernel-global (sym)
  ;; Returns byte offset relative to rnil
  (let* ((pos (position sym *kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-nil (* (1+ pos) node-size)))
      (error "Unknown kernel global: ~s" sym))))

(defmacro kernel-global (sym)
  (let* ((pos (position sym *kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-nil (* (1+ pos) node-size)))
      (error "Unknown kernel global: ~s" sym))))

;;; These definitions must match lisp-kernel/imports.s.
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
  lisp-lstat
  lisp-realpath)

;;; "nrs" means nil-relative symbol
(defmacro nrs-offset (name)
  (let* ((pos (position name *nilreg-relative-symbols* :test #'eq)))
    (if pos (* (1- pos) symbol.size))))

(defparameter *uvector-subtags*
  `((:bignum . ,subtag-bignum)
    (:ratio . ,subtag-ratio)
    (:single-float . ,subtag-single-float)
    (:double-float . ,subtag-double-float)
    (:complex . ,subtag-complex )
    (:complex-single-float . ,subtag-complex-single-float)
    (:complex-double-float . ,subtag-complex-double-float)
    (:symbol . ,subtag-symbol)
    (:function . ,subtag-function)
    (:code-vector . ,subtag-code-vector)
    (:xcode-vector . ,subtag-xcode-vector)
    (:macptr . ,subtag-macptr)
    (:catch-frame . ,subtag-catch-frame)
    (:struct . ,subtag-struct)
    (:istruct . ,subtag-istruct)
    (:pool . ,subtag-pool)
    (:population . ,subtag-weak)
    (:hash-vector . ,subtag-hash-vector)
    (:package . ,subtag-package)
    (:value-cell . ,subtag-value-cell)
    (:instance . ,subtag-instance)
    (:lock . ,subtag-lock)
    (:basic-stream . ,subtag-basic-stream)
    (:slot-vector . ,subtag-slot-vector)
    (:simple-string . ,subtag-simple-base-string)
    (:bit-vector . ,subtag-bit-vector)
    (:signed-8-bit-vector . ,subtag-s8-vector)
    (:unsigned-8-bit-vector . ,subtag-u8-vector)
    (:signed-16-bit-vector . ,subtag-s16-vector)
    (:unsigned-16-bit-vector . ,subtag-u16-vector)
    (:signed-32-bit-vector . ,subtag-s32-vector)
    (:unsigned-32-bit-vector . ,subtag-u32-vector)
    (:signed-64-bit-vector . ,subtag-s64-vector)
    (:fixnum-vector . ,subtag-fixnum-vector)
    (:unsigned-64-bit-vector . ,subtag-u64-vector)
    (:single-float-vector . ,subtag-single-float-vector)
    (:double-float-vector . ,subtag-double-float-vector)
    (:simple-vector . ,subtag-simple-vector)
    (:complex-single-float-vector . ,subtag-complex-single-float-vector)
    (:complex-double-float-vector . ,subtag-complex-double-float-vector)
    (:vector-header . ,subtag-vectorH)
    (:array-header . ,subtag-arrayH)
    (:xfunction . ,subtag-xfunction)
    (:min-cl-ivector-subtag . ,min-cl-ivector-subtag)))

(export '*uvector-subtags*)

;;; This should return NIL unless it's sure of how the indicated
;;; type would be represented (in particular, it should return
;;; NIL if the element type is unknown or unspecified at compile-time.
(defun array-type-name-from-ctype (ctype)
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
           (case (ccl::numeric-ctype-format element-type)
             (single-float :complex-single-float-vector)
             (double-float :complex-double-float-vector)
             (t :simple-vector))
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
                      ((and (>= low -128) (<= high 127))
                       :signed-8-bit-vector)
                      ((and (>= low -32768) (<= high 32767))
                       :signed-16-bit-vector)
                      ((and (>= low (ash -1 31)) (<= high (1- (ash 1 31))))
                       :signed-32-bit-vector)
                      ((and (>= low target-most-negative-fixnum)
                            (<= high target-most-positive-fixnum))
                       :fixnum-vector)
                      ((and (>= low 0) (<= high #xffffffffffffffff))
                       :unsigned-64-bit-vector)
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

(defun misc-byte-count (subtag element-count)
  (declare (fixnum subtag))
  (if (= (logand subtag tagmask) tag-nodeheader)
    (ash element-count 3)
    (case (logand subtag fulltagmask)
      (#.ivector-class-64-bit (ash element-count 3))
      (#.ivector-class-32-bit (ash element-count 2))
      (t
       (if (= subtag subtag-bit-vector)
         (ash (+ 7 element-count) -3)
         (if (= subtag subtag-complex-double-float-vector)
           (ash element-count 4)
           (if (>= subtag min-8-bit-ivector-subtag)
             element-count
             (ash element-count 1))))))))

(defparameter *arm64-target-arch*
  (arch::make-target-arch
   :name :arm64
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
   :uvector-subtags *uvector-subtags*
   :max-64-bit-constant-index max-64-bit-constant-index
   :max-32-bit-constant-index max-32-bit-constant-index
   :max-16-bit-constant-index max-16-bit-constant-index
   :max-8-bit-constant-index max-8-bit-constant-index
   :max-1-bit-constant-index max-1-bit-constant-index
   :word-shift 3
   :code-vector-prefix '(#x00000000)    ;udf #0
   :gvector-types '(:ratio :complex :symbol :function :catch-frame
                    :struct :istruct :pool :population :hash-vector
                    :package :value-cell :instance :lock :slot-vector
                    :simple-vector :xfunction)
   :1-bit-ivector-types '(:bit-vector)
   :8-bit-ivector-types '(:signed-8-bit-vector :unsigned-8-bit-vector)
   :16-bit-ivector-types '(:signed-16-bit-vector :unsigned-16-bit-vector)
   :32-bit-ivector-types '(:signed-32-bit-vector :unsigned-32-bit-vector
                           :single-float-vector
                           :double-float
                           :bignum
                           :simple-string)
   :64-bit-ivector-types '(:double-float-vector
                           :complex-single-float-vector
                           :unsigned-64-bit-vector
                           :signed-64-bit-vector
                           :fixnum-vector)
   :array-type-name-from-ctype-function #'array-type-name-from-ctype
   :package-name "ARM64"
   :t-offset t-offset
   :array-data-size-function #'misc-byte-count
   :fpr-mask-function 'arm64-fpr-mask
   :subprims-base arm64::*subprims-base*
   :subprims-shift arm64::*subprims-shift*
   :subprims-table arm64::*subprims*
   :primitive->subprims `(((0 . 23) . ,(ccl::%subprim-name->offset
                                        '.SPbuiltin-plus arm64::*subprims*)))
   :unbound-marker-value unbound-marker
   :slot-unbound-marker-value slot-unbound-marker
   :fixnum-tag tag-fixnum
   :single-float-tag subtag-single-float
   :single-float-tag-is-subtag nil
   :double-float-tag subtag-double-float
   :cons-tag fulltag-cons
   :null-tag fulltag-nil
   :symbol-tag fulltag-symbol
   :symbol-tag-is-subtag nil
   :function-tag fulltag-function
   :function-tag-is-subtag nil
   :big-endian nil
   :misc-subtag-offset misc-subtag-offset
   :car-offset cons.car
   :cdr-offset cons.cdr
   :subtag-char subtag-character
   :charcode-shift charcode-shift
   :fulltagmask fulltagmask
   :fulltag-misc fulltag-misc
   :char-code-limit #x110000))


;;; arch macros

(defmacro defarm64archmacro (name lambda-list &body body)
  `(arch::defarchmacro :arm64 ,name ,lambda-list ,@body))

(defarm64archmacro ccl::%make-sfloat ()
  (error "~s shouldn't be used in code targeting arm64" 'ccl::%make-sfloat))

(defarm64archmacro ccl::%make-dfloat ()
  `(ccl::%alloc-misc arm64::double-float.element-count
                     arm64::subtag-double-float))

(defarm64archmacro ccl::%numerator (x)
  `(ccl::%svref ,x arm64::ratio.numer-cell))

(defarm64archmacro ccl::%denominator (x)
  `(ccl::%svref ,x arm64::ratio.denom-cell))

(defarm64archmacro ccl::%realpart (x)
  (let* ((thing (gensym)))
    `(let* ((,thing ,x))
      (case (ccl::typecode ,thing)
        (#.arm64::subtag-complex-single-float
         (ccl::%complex-single-float-realpart ,thing))
        (#.arm64::subtag-complex-double-float
         (ccl::%complex-double-float-realpart ,thing))
        (t (ccl::%svref ,thing arm64::complex.realpart-cell))))))

(defarm64archmacro ccl::%imagpart (x)
  (let* ((thing (gensym)))
    `(let* ((,thing ,x))
      (case (ccl::typecode ,thing)
        (#.arm64::subtag-complex-single-float
         (ccl::%complex-single-float-imagpart ,thing))
        (#.arm64::subtag-complex-double-float
         (ccl::%complex-double-float-imagpart ,thing))
        (t (ccl::%svref ,thing arm64::complex.imagpart-cell))))))

(defarm64archmacro ccl::%get-single-float-from-double-ptr (ptr offset)
 `(ccl::%double-float->short-float (ccl::%get-double-float ,ptr ,offset)))

(defarm64archmacro ccl::codevec-header-p (word)
  `(eql arm64::subtag-code-vector (logand ,word arm64::subtag-mask)))

(defarm64archmacro ccl::immediate-p-macro (thing)
  (let* ((tag (gensym)))
    `(let* ((,tag (ccl::lisptag ,thing)))
       (declare (type (unsigned-byte 3) ,tag))
       (logbitp ,tag (logior (ash 1 arm64::tag-fixnum)
                             (ash 1 arm64::tag-single-float)
                             (ash 1 arm64::tag-imm))))))

(defarm64archmacro ccl::hashed-by-identity (thing)
  (let* ((typecode (gensym))
         (fulltag (gensym)))
    `(let* ((,typecode (ccl::typecode ,thing))
            (,fulltag (ccl::fulltag ,thing)))
       (declare (fixnum ,typecode))
       ;; There must be an opportunity to be cleverer here.
       (or (= ,typecode arm64::tag-fixnum)
           (= ,typecode arm64::tag-imm)
           (= ,typecode arm64::tag-single-float)
           (= ,fulltag arm64::fulltag-symbol)
           (= ,typecode arm64::subtag-instance)))))

;;; xxx --- these references will need to be relative to rnil
(defarm64archmacro ccl::%get-kernel-global (name)
  `(ccl::%fixnum-ref 0 (+ ,(ccl::target-nil-value)
                        ,(%kernel-global
                         (if (ccl::quoted-form-p name)
                           (cadr name)
                           name)))))
;;; xxx
(defarm64archmacro ccl::%get-kernel-global-ptr (name dest)
  `(ccl::%setf-macptr
    ,dest
    (ccl::%int-to-ptr (ccl::%fixnum-ref-natural 0 (+ ,(ccl::target-nil-value)
                                 ,(%kernel-global
                                   (if (ccl::quoted-form-p name)
                                     (cadr name)
                                     name)))))))

(defarm64archmacro ccl::%target-kernel-global (name)
  `(arm64::%kernel-global ,name))

(defarm64archmacro ccl::lfun-vector (fun)
  `(ccl::%function-to-function-vector ,fun))

(defarm64archmacro ccl::lfun-vector-lfun (lfv)
  `(ccl::%function-vector-to-function ,lfv))

(defarm64archmacro ccl::area-code ()
  area.code)

(defarm64archmacro ccl::area-succ ()
  area.succ)

(defarm64archmacro ccl::nth-immediate (f i)
  `(ccl::%nth-immediate ,f (the fixnum (- (the fixnum ,i) 1))))

(defarm64archmacro ccl::set-nth-immediate (f i new)
  `(ccl::%set-nth-immediate ,f (the fixnum (- (the fixnum ,i) 1)) ,new))

(defarm64archmacro ccl::symptr->symvector (s)
  `(ccl::%symptr->symvector ,s))

(defarm64archmacro ccl::symvector->symptr (s)
  `(ccl::%symvector->symptr ,s))

(defarm64archmacro ccl::function-to-function-vector (f)
  `(ccl::%function-to-function-vector ,f))

(defarm64archmacro ccl::function-vector-to-function (v)
  `(ccl::%function-vector-to-function ,v))

(defarm64archmacro ccl::with-ffcall-results ((buf) &body body)
  ;; Reserve space for x0--x7, d0--d7
  (let* ((size (+ (* 8 8) (* 8 8))))
    `(ccl::%stack-block ((,buf ,size :clear t))
       ,@body)))

;;; For backtrace: the relative PC of an argument-check trap
;;; must be less than or equal to this value.
(defconstant arg-check-trap-pc-limit 8)

;; These values are per-(architecture + word size)
(defconstant fasl-version #x1)
(defconstant fasl-max-version #x1)
(defconstant fasl-min-version #x1)
(defparameter *image-abi-version* #x1)

(provide "ARM64-ARCH")
