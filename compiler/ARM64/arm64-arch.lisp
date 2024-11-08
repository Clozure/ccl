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
  `(defconstant ,name (define-register ',name ',val)))

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

(defun misc-byte-count (subtag element-count)
  (declare (fixnum subtag))
  (if (logbitp (logand subtag fulltagmask)
               (logior (ash 1 fulltag-nodeheader-0)
                       (ash 1 fulltag-nodeheader-1)))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *subprims-shift* 3)
  (defparameter *subprims-base* 0))

(defvar *subprims*)

(let* ((origin *subprims-base*)
       (step (ash 1 *subprims-shift*)))
  (flet ((define-subprim (name)
           (ccl::make-subprimitive-info :name (string name)
                                        :offset (prog1 origin
                                                  (incf origin step)))))
    (macrolet ((defarm64subprim (name)
                 `(define-subprim ',name)))
      (setq *subprims*
            (vector
             (defarm64subprim .SPjmpsym)
             (defarm64subprim .SPjmpnfn)
             (defarm64subprim .SPfuncall)
             (defarm64subprim .SPmkcatch1v)
             (defarm64subprim .SPmkunwind)
             (defarm64subprim .SPmkcatchmv)
             (defarm64subprim .SPthrow)
             (defarm64subprim .SPnthrowvalues)
             (defarm64subprim .SPnthrow1value)
             (defarm64subprim .SPbind)
             (defarm64subprim .SPbind-self)
             (defarm64subprim .SPbind-nil)
             (defarm64subprim .SPbind-self-boundp-check)
             (defarm64subprim .SPrplaca)
             (defarm64subprim .SPrplacd)
             (defarm64subprim .SPconslist)
             (defarm64subprim .SPconslist-star)
             (defarm64subprim .SPstkconslist)
             (defarm64subprim .SPstkconslist-star)
             (defarm64subprim .SPmkstackv)
             (defarm64subprim .SPsubtag-misc-ref)
             (defarm64subprim .SPsetqsym)
             (defarm64subprim .SPprogvsave)
             (defarm64subprim .SPstack-misc-alloc)
             (defarm64subprim .SPgvector)
             (defarm64subprim .SPnvalret)
             (defarm64subprim .SPmvpass)
             (defarm64subprim .SPrecover-values-for-mvcall)
             (defarm64subprim .SPnthvalue)
             (defarm64subprim .SPvalues)
             (defarm64subprim .SPdefault-optional-args)
             (defarm64subprim .SPopt-supplied-p)
             (defarm64subprim .SPheap-rest-arg)
             (defarm64subprim .SPreq-heap-rest-arg)
             (defarm64subprim .SPheap-cons-rest-arg)
             (defarm64subprim .SPsimple-keywords)
             (defarm64subprim .SPkeyword-args)
             (defarm64subprim .SPkeyword-bind)
             (defarm64subprim .SPffcall)
             (defarm64subprim .SParef2)
             (defarm64subprim .SPksignalerr)
             (defarm64subprim .SPstack-rest-arg)
             (defarm64subprim .SPreq-stack-rest-arg)
             (defarm64subprim .SPstack-cons-rest-arg)
             (defarm64subprim .SPpoweropen-callbackX)
             (defarm64subprim .SPcall-closure)
             (defarm64subprim .SPgetXlong)
             (defarm64subprim .SPspreadargz)
             (defarm64subprim .SPtfuncallgen)
             (defarm64subprim .SPtfuncallslide)
             (defarm64subprim .SPtfuncallvsp)
             (defarm64subprim .SPtcallsymgen)
             (defarm64subprim .SPtcallsymslide)
             (defarm64subprim .SPtcallsymvsp)
             (defarm64subprim .SPtcallnfngen)
             (defarm64subprim .SPtcallnfnslide)
             (defarm64subprim .SPtcallnfnvsp)
             (defarm64subprim .SPmisc-ref)
             (defarm64subprim .SPmisc-set)
             (defarm64subprim .SPstkconsyz)
             (defarm64subprim .SPstkvcell0)
             (defarm64subprim .SPstkvcellvsp)
             (defarm64subprim .SPmakestackblock)
             (defarm64subprim .SPmakestackblock0)
             (defarm64subprim .SPmakestacklist)
             (defarm64subprim .SPstkgvector)
             (defarm64subprim .SPmisc-alloc)
             (defarm64subprim .SPpoweropen-ffcallX)
             (defarm64subprim .SPgvset)
             (defarm64subprim .SPmacro-bind)
             (defarm64subprim .SPdestructuring-bind)
             (defarm64subprim .SPdestructuring-bind-inner)
             (defarm64subprim .SPrecover-values)
             (defarm64subprim .SPvpopargregs)
             (defarm64subprim .SPinteger-sign)
             (defarm64subprim .SPsubtag-misc-set)
             (defarm64subprim .SPspread-lexpr-z)
             (defarm64subprim .SPstore-node-conditional)
             (defarm64subprim .SPreset)
             (defarm64subprim .SPmvslide)
             (defarm64subprim .SPsave-values)
             (defarm64subprim .SPadd-values)
             (defarm64subprim .SPcallback)
             (defarm64subprim .SPmisc-alloc-init)
             (defarm64subprim .SPstack-misc-alloc-init)
             (defarm64subprim .SPset-hash-key)
             (defarm64subprim .SPaset2)
             (defarm64subprim .SPcallbuiltin)
             (defarm64subprim .SPcallbuiltin0)
             (defarm64subprim .SPcallbuiltin1)
             (defarm64subprim .SPcallbuiltin2)
             (defarm64subprim .SPcallbuiltin3)
             (defarm64subprim .SPpopj)
             (defarm64subprim .SPrestorefullcontext)
             (defarm64subprim .SPsavecontextvsp)
             (defarm64subprim .SPsavecontext0)
             (defarm64subprim .SPrestorecontext)
             (defarm64subprim .SPlexpr-entry)
             (defarm64subprim .SPpoweropen-syscall)
             (defarm64subprim .SPbuiltin-plus)
             (defarm64subprim .SPbuiltin-minus)
             (defarm64subprim .SPbuiltin-times)
             (defarm64subprim .SPbuiltin-div)
             (defarm64subprim .SPbuiltin-eq)
             (defarm64subprim .SPbuiltin-ne)
             (defarm64subprim .SPbuiltin-gt)
             (defarm64subprim .SPbuiltin-ge)
             (defarm64subprim .SPbuiltin-lt)
             (defarm64subprim .SPbuiltin-le)
             (defarm64subprim .SPbuiltin-eql)
             (defarm64subprim .SPbuiltin-length)
             (defarm64subprim .SPbuiltin-seqtype)
             (defarm64subprim .SPbuiltin-assq)
             (defarm64subprim .SPbuiltin-memq)
             (defarm64subprim .SPbuiltin-logbitp)
             (defarm64subprim .SPbuiltin-logior)
             (defarm64subprim .SPbuiltin-logand)
             (defarm64subprim .SPbuiltin-ash)
             (defarm64subprim .SPbuiltin-negate)
             (defarm64subprim .SPbuiltin-logxor)
             (defarm64subprim .SPbuiltin-aref1)
             (defarm64subprim .SPbuiltin-aset1)
             (defarm64subprim .SPbreakpoint)
             (defarm64subprim .SPeabi-ff-call)
             (defarm64subprim .SPeabi-callback)
             (defarm64subprim .SPsyscall)
             (defarm64subprim .SPgetu64)
             (defarm64subprim .SPgets64)
             (defarm64subprim .SPmakeu64)
             (defarm64subprim .SPmakes64)
             (defarm64subprim .SPspecref)
             (defarm64subprim .SPspecset)
             (defarm64subprim .SPspecrefcheck)
             (defarm64subprim .SPrestoreintlevel)
             (defarm64subprim .SPmakes32)
             (defarm64subprim .SPmakeu32)
             (defarm64subprim .SPgets32)
             (defarm64subprim .SPgetu32)
             (defarm64subprim .SPfix-overflow)
             (defarm64subprim .SPmvpasssym)
             (defarm64subprim .SParef3)
             (defarm64subprim .SPaset3)
             (defarm64subprim .SPffcall-return-registers)
             (defarm64subprim .SPunused-5)
             (defarm64subprim .SPset-hash-key-conditional)
             (defarm64subprim .SPunbind-interrupt-level)
             (defarm64subprim .SPunbind)
             (defarm64subprim .SPunbind-n)
             (defarm64subprim .SPunbind-to)
             (defarm64subprim .SPbind-interrupt-level-m1)
             (defarm64subprim .SPbind-interrupt-level)
             (defarm64subprim .SPbind-interrupt-level-0)
             (defarm64subprim .SPprogvrestore)
             (defarm64subprim .SPnmkunwind))))))

;;; Notes on functions
;;;
;;; Because ARM64 supports PC-relative addressing with a displacement
;;; of +/- 1MB, so there's no strong reason to have separate
;;; code-vector and function objects.  If we use a concatenated
;;; function object that contains both code and constants, we should
;;; be able to use pc-relative addressing to access the function's
;;; constants. (But we might run into size limitations if the function
;;; is big.)
;;;
;;; We want to be able to call a function by loading it into a node
;;; register and doing a br or blr to branch to it.  Unlike PowerPC,
;;; the ARM64 will fault if the low two bits of the PC are not clear.
;;; Therefore #b0100 or #b1100 are the candidates for
;;; fulltag-function.  (Recall that #b0000 and #b1000 are even fixnum
;;; and odd fixnum, respectively).
;;;
;;; The ARM64 has fixed-length instructions, so it is in principle
;;; possible to recover the containing lisp object given a pc-locative
;;; into that object (such as the PC or LR).  This is desirable
;;; because it prevents the need to keep the current function in a GPR
;;; if backtrace and friends (and the GC) can recover the function
;;; object from a pc-locative.
;;;
;;; The idea is that we can scan backwards over a function's
;;; instructions until we find the start of the function.  This means
;;; that there must be some recognizable sentinel value that will mark
;;; the start of a function.
;;;
;;; A uvector header looks like this:
;;;
;;;        6                   5                     1
;;;  3 2 1 0 9 8 7 6 5 4 3 2 1 0       7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+ ... +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; |                                   element count     |    subtag     |
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+ ... +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;
;;; In the case of a function, the subtag will be subtag-function.
;;;
;;; If we use #b0100 for fulltag-function, the first instruction word
;;; comes right after the uvector header.  If the uvector header never
;;; looks like any valid instruction, then this could be workable.  It
;;; it not clear to me that we can guarantee that, however.
;;;
;;; If we use #b1100 for fulltag-function, then that leaves 4 bytes
;;; between the header and the first instruction word.  We could store
;;; an appropriate UDF instruction there, and use that as the sentinel
;;; value.
;;;
;;; An instruction with the top 16 bits clear is architecturally
;;; undefined.  We could thus leave a 0 word (or anything in the range
;;; #x00000000--#x0000ffff) between the uvector header and the first
;;; instruction word, and that would be easy to recognize.
;;;
;;; We *could* think about limiting the element count of a function
;;; vector such that the top 16 bits (at least) are always clear.  That
;;; would make the uvector header for a function look like an undefined
;;; instruction.
;;;
;;; If we said that the element count was limited to 40 bits (because
;;; we're keeping the top 16 bits clear), then that wouldn't be any
;;; practical burden on function size.  It would use up all the UDF
;;; instructions, and we want to be able to at least a few of those
;;; for UUOs.
;;;
;;; If we shorten the element count of a function vector even further,
;;; say by an additional 8 bits, that would leave 32 bits to encode
;;; the element count (still quite reasonable).  UUOs could then use
;;; any non-zero value (byte 8 32) as an immediate value.
;;;
;;; This would require treating function uvectors specially. This is not
;;; uprecedented: the x86 ports have funny function objects.
;;;
;;; But, it seems to me that the approach of using a tag of #b1100
;;; and an distinguished UDF to mark the start of the function is perhaps
;;; the better choice here.

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
(defconstant tag-fixnum   #b000)
(defconstant tag-imm-0    #b001)   ;subtag-single-float 
(defconstant tag-imm-1    #b010)   ;subtag-character, internal markers
(defconstant tag-list     #b011)   ;cons cell or nil
(defconstant tag-function #b100)   ;function entry point
(defconstant tag-misc     #b101)   ;uvector of some sort
(defconstant tag-symbol   #b110)   ;non-null symbol
(defconstant tag-tbd      #b111)   ;

;;; 4-bit "fulltag" values
(defconstant fulltag-even-fixnum  #b0000)
(defconstant fulltag-imm-0        #b0001) ;single-float
(defconstant fulltag-imm-1        #b0010) ;characters, markers
(defconstant fulltag-cons         #b0011) ;a cons cell
(defconstant fulltag-tbd-0        #b0100)
(defconstant fulltag-nodeheader-0 #b0101)
(defconstant fulltag-nodeheader-1 #b0110)
(defconstant fulltag-immheader-0  #b0111) ;other-bit ivector
(defconstant fulltag-odd-fixnum   #b1000)
(defconstant fulltag-immheader-1  #b1001) ;32-bit ivector
(defconstant fulltag-immheader-2  #b1010) ;64-bit ivector
(defconstant fulltag-nil          #b1011) ;nil and nothing but
(defconstant fulltag-function     #b1100) ;something we can branch to directly
(defconstant fulltag-misc         #b1101) ;a uvector of some sort
(defconstant fulltag-symbol       #b1110)
(defconstant fulltag-tbd-1        #b1111)

(defconstant fulltag-single-float fulltag-imm-0)

;; could maybe use fulltag-tbd for ivector-class-8-bit?
(defconstant ivector-class-other-bit fulltag-immheader-0)
(defconstant ivector-class-32-bit fulltag-immheader-1)
(defconstant ivector-class-64-bit fulltag-immheader-2)

;;; ...

(defmacro define-subtag (name tag value)
  `(defconstant ,(ccl::form-symbol "SUBTAG-" name)
     (logior ,tag (ash ,value ntagbits))))

(define-subtag arrayH fulltag-nodeheader-0 10)
(define-subtag vectorH fulltag-nodeheader-1 10)
(define-subtag simple-vector fulltag-nodeheader-1 11)

;; 64-bit ivectors
(define-subtag complex-single-float-vector ivector-class-64-bit 11)
(define-subtag fixnum-vector ivector-class-64-bit 12)
(define-subtag s64-vector ivector-class-64-bit 13)
(define-subtag u64-vector ivector-class-64-bit 14)
(define-subtag double-float-vector ivector-class-64-bit 15)

;; 32-bit ivectors
(define-subtag simple-base-string ivector-class-32-bit 12)
(define-subtag s32-vector ivector-class-32-bit 13)
(define-subtag u32-vector ivector-class-32-bit 14)
(define-subtag single-float-vector ivector-class-32-bit 15)

;; other-bit ivectors
(define-subtag complex-double-float-vector ivector-class-other-bit 9)
(define-subtag s16-vector ivector-class-other-bit 10)
(define-subtag u16-vector ivector-class-other-bit 11)

;; XXX
(defconstant min-cl-ivector-subtag subtag-complex-double-float-vector)

(define-subtag s8-vector ivector-class-other-bit 13)
(define-subtag u8-vector ivector-class-other-bit 14)
(defconstant min-8-bit-ivector-subtag subtag-s8-vector)
(defconstant max-8-bit-ivector-subtag subtag-u8-vector)

(define-subtag bit-vector ivector-class-other-bit 15)

;; non-array ivectors
(define-subtag macptr ivector-class-64-bit 1)
(define-subtag dead-macptr ivector-class-64-bit 2)
(define-subtag bignum ivector-class-32-bit 1)
(define-subtag double-float ivector-class-32-bit 2)
(define-subtag xcode-vector ivector-class-32-bit 3)
(define-subtag complex-single-float ivector-class-32-bit 4)
(define-subtag complex-double-float ivector-class-32-bit 5)

;;; don't use nodheader/0, since that would conflict with tag-misc
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

;;; XXX
(defconstant canonical-nil-value 0)
(defconstant t-offset 0)

(defconstant misc-header-offset (- fulltag-misc))
(defconstant misc-data-offset (+ misc-header-offset node-size))
(defconstant misc-subtag-offset misc-header-offset)
(defconstant misc-dfloat-offset misc-data-offset)
(defconstant misc-symbol-offset (- node-size fulltag-symbol))
(defconstant misc-function-offset (- node-size fulltag-function))
;;; There is a pad word after the uvector header so that the
;;; complex-double-float elements are 16-byte aligned.
(defconstant misc-complex-dfloat-offset (+ misc-data-offset node-size))

(define-subtag single-float fulltag-imm-0 0)

(define-subtag character fulltag-imm-1 0)
(define-subtag unbound fulltag-imm-1 1)
(defconstant unbound-marker subtag-unbound)
(define-subtag slot-unbound fulltag-imm-1 2)
(defconstant slot-unbound-marker subtag-slot-unbound)
(define-subtag illegal fulltag-imm-1 3)
(defconstant illegal-marker subtag-illegal)
(define-subtag no-thread-local-binding fulltag-imm-1 4)
(defconstant no-thread-local-binding-marker subtag-no-thread-local-binding)
(define-subtag reserved-frame fulltag-imm-1 5)
(defconstant reserved-frame-marker subtag-reserved-frame)

(define-subtag function-boundary-marker fulltag-imm-1 15)
(defconstant function-boundary-marker subtag-function-boundary-marker)

(defconstant max-64-bit-constant-index (ash (+ #x7fffffff misc-dfloat-offset)
                                            -3))
(defconstant max-32-bit-constant-index (ash (+ #x7fffffff misc-data-offset)
                                            -2))
(defconstant max-16-bit-constant-index (ash (+ #x7fffffff misc-data-offset)
                                            -1))
(defconstant max-8-bit-constant-index (+ #x7fffffff misc-data-offset))
(defconstant max-1-bit-constant-index (ash (+ #x7fffffff misc-data-offset) 3))

) ; eval-when

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

(define-fixedsized-object double-float ()
  value)

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


;;; If we're pointing at the "symbol-vector", we can use these
(define-fixedsized-object symptr ()
  pname
  vcell
  fcell
  package-predicate
  flags
  plist
  binding-index)

(define-fixedsized-object symbol (fulltag-symbol)
  pname
  vcell
  fcell
  package-predicate
  flags
  plist
  binding-index)

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
;;; interested in.

;;; Should match lisp-kernel/area.h
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
(defconstant tcr-bias 0)
)

(define-storage-layout tcr (- tcr-bias)
  prev                            ;in doubly-linked list
  next                            ;in doubly-linked list
  lisp-fpscr
  db-link                         ;special binding chain head 
  catch-top                       ;top catch frame 
  save-vsp                        ;SP when in foreign code 
  save-tsp                        ;TSP, at all times
  foreign-sp                      ;SP when in lisp code
  cs-area                         ;cstack area pointer 
  vs-area                         ;vstack area pointer 
  ts-area                         ;tstack area pointer 
  cs-limit                        ;cstack overflow limit
  total-bytes-allocated           ;
  log2-allocation-quantum         ;unboxed
  interrupt-pending               ;fixnum
  xframe                          ;exception frame linked list
  errno-loc                       ;thread-private, maybe
  ffi-exception                   ;fpscr bits from ff-call.
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
  next-tsp
  safe-ref-address
  pending-io-info
  io-datum
  nfp
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
;;; On other ports, NIL is basically a really popular constant, and it
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
  ;; Returns index relative to (- nilreg fulltag-nil)
  (let* ((pos (position sym *arm64-kernel-globals* :test #'string=)))
    (if pos
      (- (* (+ 3 pos) 4))
      (error "Unknown kernel global : ~s ." sym))))

(defmacro kernel-global (sym)
  (let* ((pos (position sym *arm64-kernel-globals* :test #'string=)))
    (if pos
      (- (* (+ 3 pos) 4))
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
  lisp-realpath
  )

(defmacro nrs-offset (name)
  (let* ((pos (position name *arm64-nilreg-relative-symbols* :test #'eq)))
    (if pos (* (1- pos) symbol.size))))

(defparameter *arm64-target-uvector-subtags*
  `((:bignum . ,subtag-bignum)
    (:ratio . ,subtag-ratio)
    (:single-float . ,subtag-single-float)
    (:double-float . ,subtag-double-float)
    (:complex . ,subtag-complex  )
    (:complex-single-float . ,subtag-complex-single-float)
    (:complex-double-float . ,subtag-complex-double-float)
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
    (:signed-64-bit-vector . ,subtag-s64-vector)
    (:fixnum-vector . ,subtag-fixnum-vector)
    (:unsigned-64-bit-vector . ,subtag-u64-vector)    
    (:single-float-vector . ,subtag-single-float-vector)
    (:double-float-vector . ,subtag-double-float-vector )
    (:simple-vector . ,subtag-simple-vector )
    (:complex-single-float-vector . ,subtag-complex-single-float-vector)
    (:complex-double-float-vector . ,subtag-complex-double-float-vector)
    (:vector-header . ,subtag-vectorH)
    (:array-header . ,subtag-arrayH)
    (:min-cl-ivector-subtag . ,min-cl-ivector-subtag)))

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


;;; Kernel globals are allocated "below" nil.  This list (used to map
;;; symbolic names to rnil-relative offsets) must (of course) exactly
;;; match the kernel's notion of where things are.
;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" & the
;;; lisp_globals record in "ccl:lisp-kernel;*constants*.s"
(defparameter *arm64-kernel-globals*
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
   :uvector-subtags *arm64-target-uvector-subtags*
   :max-64-bit-constant-index max-64-bit-constant-index
   :max-32-bit-constant-index max-32-bit-constant-index
   :max-16-bit-constant-index max-16-bit-constant-index
   :max-8-bit-constant-index max-8-bit-constant-index
   :max-1-bit-constant-index max-1-bit-constant-index
   :word-shift 3
   :code-vector-prefix ()
   :gvector-types '(:ratio :complex :symbol :function :catch-frame
                    :struct :istruct :pool :population :hash-vector
                    :package :value-cell :instance :lock :slot-vector
                    :simple-vector :xfunction :pseudofunction)
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
   :char-code-limit #x110000))


;;; arch macros

(defmacro defarm64archmacro (name lambda-list &body body)
  `(arch::defarchmacro :arm64 ,name ,lambda-list ,@body))

(defarm64archmacro ccl::%make-sfloat ()
  (error "~s shouldn't be used in code targeting ARM64" 'ccl::%make-sfloat))

(defarm64archmacro ccl::%make-dfloat ()
  `(ccl::%alloc-misc double-float.element-count subtag-double-float))

(provide "ARM64-ARCH")
