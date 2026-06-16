;;;;-*- Mode: Lisp; Package: (ARM64 :use CL) -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ARM64-ARCH"))

(in-package "ARM64")

(defstruct register
  name         ;canonical name
  number       ;as encoded in instruction
  width        ;in bits
  (family :gpr :type (member :gpr :fpr))  ;register file: GPR vs FP/SIMD
  flags)

(defmethod make-load-form ((r register) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots r))

;;; Flags in the register defstruct
(defconstant $sp #b001)                 ;r31 role is stack pointer

;;; Given a designator for a register name, figure out everything the
;;; name implies.  Returns (values name number width family flags).
(defun %parse-register-name (designator)
  (let ((name (string-downcase designator)))
    (cond
      ((string= name "sp") (values name 31 64 :gpr $sp))
      ((string= name "wsp") (values name 31 32 :gpr $sp))
      ((string= name "xzr") (values name 31 64 :gpr 0))
      ((string= name "wzr") (values name 31 32 :gpr 0))
      (t (multiple-value-bind (width family)
             (ecase (char name 0)
               (#\x (values 64 :gpr))
               (#\w (values 32 :gpr))
               (#\s (values 32 :fpr))
               (#\d (values 64 :fpr)))
           (let ((number (parse-integer name :start 1)))
             (when (and (eq family :gpr) (= number 31))
               (error "Write xzr/wzr/sp/wsp, not ~a." name))
             (unless (<= 0 number (if (eq family :gpr) 30 31))
               (error "Register number out of range: ~a" name))
             (values name number width family 0)))))))

;;; An abbreviation to be short in writing.
(defmacro reg (namespec)
  (multiple-value-bind (name number width family flags)
      (%parse-register-name namespec)
    `(make-register :name ,name :number ,number :width ,width
                    :family ,family :flags ,flags)))

(defparameter *registers*
  (vector
   ;; 64 bit
   (reg x0) (reg x1) (reg x2) (reg x3) (reg x4) (reg x5) (reg x6)
   (reg x7) (reg x8) (reg x9) (reg x10) (reg x11) (reg x12) (reg x13)
   (reg x14) (reg x15) (reg x16) (reg x17) (reg x18) (reg x19) (reg x20)
   (reg x21) (reg x22) (reg x23) (reg x24) (reg x25) (reg x26) (reg x27)
   (reg x28) (reg x29) (reg x30) (reg xzr) (reg sp)
   ;; 32 bit
   (reg w0) (reg w1) (reg w2) (reg w3) (reg w4) (reg w5) (reg w6)
   (reg w7) (reg w8) (reg w9) (reg w10) (reg w11) (reg w12) (reg w13)
   (reg w14) (reg w15) (reg w16) (reg w17) (reg w18) (reg w19) (reg w20)
   (reg w21) (reg w22) (reg w23) (reg w24) (reg w25) (reg w26) (reg w27)
   (reg w28) (reg w29) (reg w30) (reg wzr) (reg wsp)
   ;; scalar single-float
   (reg s0) (reg s1) (reg s2) (reg s3) (reg s4) (reg s5) (reg s6)
   (reg s7) (reg s8) (reg s9) (reg s10) (reg s11) (reg s12) (reg s13)
   (reg s14) (reg s15) (reg s16) (reg s17) (reg s18) (reg s19) (reg s20)
   (reg s21) (reg s22) (reg s23) (reg s24) (reg s25) (reg s26) (reg s27)
   (reg s28) (reg s29) (reg s30) (reg s31)
   ;; scalar double-float
   (reg d0) (reg d1) (reg d2) (reg d3) (reg d4) (reg d5) (reg d6)
   (reg d7) (reg d8) (reg d9) (reg d10) (reg d11) (reg d12) (reg d13)
   (reg d14) (reg d15) (reg d16) (reg d17) (reg d18) (reg d19) (reg d20)
   (reg d21) (reg d22) (reg d23) (reg d24) (reg d25) (reg d26) (reg d27)
   (reg d28) (reg d29) (reg d30) (reg d31)))

;;; Constants for indexes into *registers* (not sure if useful).
(ccl::defenum ()
   x0  x1  x2  x3  x4  x5  x6  x7  x8  x9  x10 x11 x12 x13 x14 x15
   x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 xzr sp
   ;; 32-bit names
   w0  w1  w2  w3  w4  w5  w6  w7  w8  w9  w10 w11 w12 w13 w14 w15
   w16 w17 w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30 wzr wsp
   ;; single-float names
   s0  s1  s2  s3  s4  s5  s6  s7  s8  s9  s10 s11 s12 s13 s14 s15
   s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31
   ;; double-float names
   d0  d1  d2  d3  d4  d5  d6  d7  d8  d9  d10 d11 d12 d13 d14 d15
   d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31)

(defvar *registers-by-name* (make-hash-table :test #'equalp))

(defun hash-registers ()
  (clrhash *registers-by-name*)
  (dotimes (i (length *registers*))
    (let ((r (svref *registers* i)))
      (setf (gethash (register-name r) *registers-by-name*) r))))

(hash-registers)

(defmacro define-register-alias (alias known)
  (let ((known-entry (gensym)))
    `(let ((,known-entry (gethash ,(string known) *registers-by-name*)))
       (unless ,known-entry
         (error "register ~a not defined" ',known))
       (setf (gethash ,(string alias) *registers-by-name*) ,known-entry)
       (defconstant ,alias ,known))))

(define-register-alias fp x29)          ;frame pointer
(define-register-alias lr x30)          ;link register

;;; Lisp register names
(define-register-alias imm0 x0)         ;unboxed, volatile registers
(define-register-alias imm1 x1)
(define-register-alias imm2 x2)
(define-register-alias imm3 x3)
(define-register-alias imm4 x4)
(define-register-alias imm5 x5)
;; nargs probably doesn't need to be a dedicated register
(define-register-alias nargs x6)        ;unboxed, but nargs fixnum tagged

(define-register-alias fn x7)
;;(define-register-alias ... x8)        ;tbd

(define-register-alias arg_w x9)        ;for future use
(define-register-alias arg_x x10)       ;next-to-next-to-last argument
(define-register-alias arg_y x11)       ;next-to-last argument
(define-register-alias arg_z x12)       ;last argument

(define-register-alias temp0 x13)       ;boxed, volatile registers
(define-register-alias temp1 x14)       ; Some may be defined on function
(define-register-alias temp2 x15)       ; entry as part of the calling
(define-register-alias temp3 x16)       ; convention
(define-register-alias temp4 x17)

;;; The calling sequence may pass some additional arguments in
;;; temp registers.
(define-register-alias next-method-context temp1) ;for call-next-method, etc.
(define-register-alias nfn temp2)       ;new fn
(define-register-alias fname temp3)     ;symbol being called

;; x18 is reserved as a "platform register" (at least on Darwin)

(define-register-alias save0 x19)       ;boxed, non-volatile registers
(define-register-alias save1 x20)
(define-register-alias save2 x21)
(define-register-alias save3 x22)

(define-register-alias rnil x23)        ;nil (and to reference nilreg area)
(define-register-alias tsp x24)         ;temp stack pointer
(define-register-alias vsp x25)         ;value stack pointer
(define-register-alias allocptr x26)
(define-register-alias allocbase x27)
(define-register-alias rcontext x28)    ;per-thread data

(defparameter *instruction-flags*
  '((:alias . 0)))                      ;disassembler ignores aliases

(defun %encode-instruction-flags (flags)
  (flet ((encode-one-flag (name)
           (ash 1 (or (cdr (assoc name *instruction-flags* :test #'eq))
                      (error "Unknown instruction flag ~s" name)))))
    (if flags
      (if (atom flags)
        (encode-one-flag flags)
        (let ((mask 0))
          (dolist (f flags mask)
            (setq mask (logior mask (encode-one-flag f))))))
      0)))

(defmacro encode-instruction-flags (flags)
  (%encode-instruction-flags flags))

(defstruct instruction-template
  name
  operand-specs
  base-opcode
  mask            ;for disassembly: masks out variable parts of instruction
  alias-printer   ;optional function to rewrite disassembled insn as
                  ; as preferred alias
  (flags 0))

(defmacro define-instruction-template (name operand-specs base-opcode mask
                                       &key alias-printer flags)
  `(make-instruction-template :name ,(string-downcase name)
                              :operand-specs ',operand-specs
                              :base-opcode ,base-opcode
                              :mask ,mask
                              :alias-printer ,alias-printer
                              :flags (encode-instruction-flags ,flags)))

(defparameter *instruction-groups*
  '(:addsub-carry :addsub-ext :addsub-imm :addsub-shift :condbranch
    :dp-2src :dp-3src :exception :extract :float2fix :float2int
    :floatccmp :floatcmp :floatdp1 :floatdp2 :floatdp3 :floatimm
    :floatsel :ic-system :ldst-imm9 :ldst-pos :ldst-regoff :ldst-unpriv
    :ldst-unscaled :ldstexcl :ldstnapair-offs :ldstpair-indexed
    :ldstpair-off :loadlit :log-imm :log-shift :movewide :pcreladdr
    :testbranch))
;; data processing -- immediate
;; branch / exception / system
;; loads and stores
;; data processing -- register
;; data processing -- scalar FP & Advanced SIMD

(defconstant $rm/rn/rd-mask #xffe0fc00)
(defconstant $addsub-imm-mask #xff000000)
(defconstant $addsub-shift-mask #xff200000)
(defconstant $addsub-ext-mask #xffe00000)
(defconstant $log-shift-mask #xff200000)
(defconstant $ldst-pos-mask #xffc00000)  ;load/store register, unsigned immediate
(defconstant $ldst-unscaled-mask #xffe00c00) ;load/store register, unscaled immediate
(defconstant $ldst-regoff-mask #xffe00c00) ;load/store register, register offset
(defconstant $ldstpair-mask #xffc00000) ;load/store register pair (all index modes)
(defconstant $movewide-mask #xff800000) ;move wide (immediate)
(defconstant $bitfield-mask #xffc00000) ;bitfield
(defconstant $extract-mask #xffe00000) ;extract
(defconstant $pcrel-mask #x9f000000) ;PC-relative addressing
(defconstant $uncond-branch-imm-mask #xfc000000) ;b, bl
(defconstant $cmp-branch-mask #xff000000) ;cbz, cbnz
(defconstant $test-branch-mask #x7f000000) ;tbz, tbnz (bit 31 is b5, not fixed)
(defconstant $condsel-mask #xffe00c00) ;conditional select (csel/csinc/csinv/csneg)
(defconstant $condcmp-mask #xffe00c10) ;conditional compare (ccmp/ccmn), reg & imm
(defconstant $dp-3src-mask #xffe08000) ;data-processing 3-source (madd/msub/...)
(defconstant $dp-2src-mask #xffe0fc00) ;data-processing 2-source (sdiv/udiv); also smulh/umulh
(defconstant $dp-1src-mask #xfffffc00) ;data-processing 1-source (rbit/rev/clz/cls)

;;; The operands in the operand list in these templates may be considered
;;; as operand specs.
;;;
;;; Here's the vocabulary:
;;;
;;; Bare keywords, such as :aimm or :limm represent a immediate
;;; value of various sorts.  They have particular valid ranges, and
;;; go into differeing bit-fields within the instruction word.
;;;
;;; (:mem-* ...) represent a memory operand.  Generally speaking,
;;; all memory operands include a base register and an offset.
;;; :mem-scaled represents an immediate offset that is scaled by the
;;; memory access size.
;;; :mem-unscaled is an immediate offset in the range -256--255.
;;; There also need to be :mem-shifted  and :mem-extended, where
;;; the offset is a register that is shifted or extended in some way.
;;;
;;; Otherwise, something like (:rd :x) represents a register.  In this
;;; example, the first item, :rd, represents a role.  In this case,
;;; the role is the Rd field in an instruction word.  The second item,
;;; :x in this case, represents a class: the set of concrete registers
;;; that are eligible to fill the specified role.


;;; A64 instruction table
;;;
;;; Each entry in the table is called an instruction template (or
;;; template for short).
;;;
;;; A template includes the instruction name, a list of operand specs,
;;; and a fully-resolved base opcode (SF/size bits already included).
;;; (Other slots in the template are for the disassembler.)
;;;
;;; There are three kinds of operands: register, immediate, and memory.
;;;
;;; An operand class (e.g., :x, :w, :aimm, &c.) may be thought of as a
;;; set of acceptable operands.  If we have an operand, we can ask
;;; "does the operand belong to this class?"
;;;
;;; A role (:rd/:rn/:base) represents a field in the instruction word
;;; where the encoded operand will go.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; To be shorter in writing
  (defmacro def (&rest args)
    `(define-instruction-template ,@args)))

;;; Section references are to the Arm Architecture Reference Manual
;;; for A-profile architecture ARM DDI 0487H.a

(defparameter *instruction-templates*
  (vector
   ;;; C4.1.1  Reserved
   
   ;; 16-bit immediate in (byte 16 0)
   (def udf (:udf16) #x00000000 #xffff0000)
   
   ;;; C4.1.64  Data Processing -- Immediate

   ;; PC-rel. addressing
   ;; 21-bit immediate split into immlo in (byte 2 29) and immhi in
   ;; (byte 19 5)
   (def adr ((:rd :x) :pcrel) #x10000000 $pcrel-mask)
   (def adrp ((:rd :x) :pcrel) #x90000000 $pcrel-mask)

   ;; Add/subtract (immediate)
   (def add ((:rd :w/sp) (:rn :w/sp) :aimm) #x11000000 #xff000000)
   (def add ((:rd :x/sp) (:rn :x/sp) :aimm) #x91000000 #xff000000)
   (def mov ((:rd :wsp) (:rn :w/sp)) #x11000000 0 :flags :alias)
   (def mov ((:rd :w/sp) (:rn :wsp)) #x11000000 0 :flags :alias)
   (def mov ((:rd :sp) (:rn :x/sp)) #x91000000 0 :flags :alias)
   (def mov ((:rd :x/sp) (:rn :sp)) #x91000000 0 :flags :alias)
   (def adds ((:rd :w) (:rn :w/sp) :aimm) #x31000000 #xff000000)
   (def adds ((:rd :x) (:rn :x/sp) :aimm) #xb1000000 #xff000000)
   (def cmn ((:rn :w/sp) :aimm) #x3100001f 0 :flags :alias)
   (def cmn ((:rn :x/sp) :aimm) #xb100001f 0 :flags :alias)
   (def sub ((:rd :w/sp) (:rn :w/sp) :aimm) #x51000000 #xff000000)
   (def sub ((:rd :x/sp) (:rn :x/sp) :aimm) #xd1000000 #xff000000)
   (def subs ((:rd :w) (:rn :w/sp) :aimm) #x71000000 #xff000000)
   (def subs ((:rd :x) (:rn :x/sp) :aimm) #xf1000000 #xff000000)
   (def cmp ((:rn :w/sp) :aimm) #x7100001f 0 :flags :alias)
   (def cmp ((:rn :x/sp) :aimm) #xf100001f 0 :flags :alias)

   ;; Logical (immediate)
   (def and ((:rd :w/sp) (:rn :w) :limm) #x12000000 #xff800000)
   (def and ((:rd :x/sp) (:rn :x) :limm) #x92000000 #xff800000)
   (def orr ((:rd :w/sp) (:rn :w) :limm) #x32000000 #xff800000)
   (def orr ((:rd :x/sp) (:rn :x) :limm) #xb2000000 #xff800000)
   (def mov ((:rd :w/sp) :limm) #x320003e0 0 :flags :alias)
   (def mov ((:rd :x/sp) :limm) #xb20003e0 0 :flags :alias)
   (def eor ((:rd :w/sp) (:rn :w) :limm) #x52000000 #xff800000)
   (def eor ((:rd :x/sp) (:rn :x) :limm) #xd2000000 #xff800000)
   (def ands ((:rd :w) (:rn :w) :limm) #x72000000 #xff800000)
   (def ands ((:rd :x) (:rn :x) :limm) #xf2000000 #xff800000)
   (def tst ((:rn :w) :limm) #x7200001f 0 :flags :alias)
   (def tst ((:rn :x) :limm) #xf200001f 0 :flags :alias)

   ;; Move wide (immediate)
   (def movn ((:rd :w) :movw-w) #x12800000 $movewide-mask)
   (def movn ((:rd :x) :movw-x) #x92800000 $movewide-mask)
   (def movz ((:rd :w) :movw-w) #x52800000 $movewide-mask)
   (def movz ((:rd :x) :movw-x) #xd2800000 $movewide-mask)
   (def movk ((:rd :w) :movw-w) #x72800000 $movewide-mask)
   (def movk ((:rd :x) :movw-x) #xf2800000 $movewide-mask)

   ;; Bitfield
   (def sbfm ((:rd :w) (:rn :w) :immr-w :imms-w) #x13000000 $bitfield-mask)
   (def sbfm ((:rd :x) (:rn :x) :immr-x :imms-x) #x93400000 $bitfield-mask)
   (def bfm ((:rd :w) (:rn :w) :immr-w :imms-w) #x33000000 $bitfield-mask)
   (def bfm ((:rd :x) (:rn :x) :immr-x :imms-x) #xb3400000 $bitfield-mask)
   (def ubfm ((:rd :w) (:rn :w) :immr-w :imms-w) #x53000000 $bitfield-mask)
   (def ubfm ((:rd :x) (:rn :x) :immr-x :imms-x) #xd3400000 $bitfield-mask)

   ;; Extract
   (def extr ((:rd :w) (:rn :w) (:rm :w) :imms-w) #x13800000 $extract-mask)
   (def extr ((:rd :x) (:rn :x) (:rm :x) :imms-x) #x93c00000 $extract-mask)

   ;;; C4.1.65  Branches, Exception Generating, and System instructions

   ;; Conditional branch (immediate)
   ;; The 4-bit condition is pre-inserted into (byte 4 0) of the base opcode.
   ;; A 19-bit word offset is inserted into (byte 19 5).
   (def b.eq ((:label :b19)) #x54000000 #xff00001f)
   (def b.ne ((:label :b19)) #x54000001 #xff00001f)
   (def b.cs ((:label :b19)) #x54000002 #xff00001f)
   (def b.cc ((:label :b19)) #x54000003 #xff00001f)
   (def b.mi ((:label :b19)) #x54000004 #xff00001f)
   (def b.pl ((:label :b19)) #x54000005 #xff00001f)
   (def b.vs ((:label :b19)) #x54000006 #xff00001f)
   (def b.vc ((:label :b19)) #x54000007 #xff00001f)
   (def b.hi ((:label :b19)) #x54000008 #xff00001f)
   (def b.ls ((:label :b19)) #x54000009 #xff00001f)
   (def b.ge ((:label :b19)) #x5400000a #xff00001f)
   (def b.lt ((:label :b19)) #x5400000b #xff00001f)
   (def b.gt ((:label :b19)) #x5400000c #xff00001f)
   (def b.le ((:label :b19)) #x5400000d #xff00001f)
   (def b.al ((:label :b19)) #x5400000e #xff00001f)   ;pointless
   (def b.nv ((:label :b19)) #x5400000f #xff00001f)   ;also pointless

   ;; Exception generation
   ;; 16-bit immediate in (byte 16 5)
   (def svc (:exc16) #xd4000001 #xffe0001f)
   (def brk (:exc16) #xd4200000 #xffe0001f)
   (def hlt (:exc16) #xd4400000 #xffe0001f)

   ;; Hints
   (def nop () #xd503201f #xffffffff)
   (def yield () #xd503203f #xffffffff)
   (def wfe () #xd503205f #xffffffff)
   (def sev () #xd503209f #xffffffff)

   ;; Barriers
   ;; The bare form defaults to SY; an explicit (:$ option) may be
   ;; used to select a domain (e.g. 11 = ish, 10 = ishst).  Support
   ;; for named options is not implemented.
   ;; clrex ignores its CRm operand, so there's just one form (CRm=15).
   (def clrex () #xd5033f5f #xffffffff)
   (def dmb () #xd5033fbf #xffffffff)
   (def dmb (:baropt) #xd50330bf #xfffff0ff)
   (def dsb () #xd5033f9f #xffffffff)
   (def dsb (:baropt) #xd503309f #xfffff0ff)
   (def isb () #xd5033fdf #xffffffff)

   ;; Unconditional branch (register)
   (def br ((:rn :x)) #xd61f0000 #xfffffc1f)
   (def blr ((:rn :x)) #xd63f0000 #xfffffc1f)
   ;; Plain ret defaults to x30 (lr) in (byte 5 5)
   (def ret () #xd65f03c0 #xffffffff)
   (def ret ((:rn :x)) #xd65f0000 #xfffffc1f)

   ;; Unconditional branch (immediate)
   ;; 26-bit word displacement in (byte 26 0)
   (def b ((:label :b26)) #x14000000 $uncond-branch-imm-mask)
   (def bl ((:label :b26)) #x94000000 $uncond-branch-imm-mask)

   ;; Compare and branch (immediate)
   ;; Rt in (byte 4 0), 19-bit word displacement in (byte 19 5)
   (def cbz ((:rt :w) (:label :b19)) #x34000000 $cmp-branch-mask)
   (def cbz ((:rt :x) (:label :b19)) #xb4000000 $cmp-branch-mask)
   (def cbnz ((:rt :w) (:label :b19)) #x35000000 $cmp-branch-mask)
   (def cbnz ((:rt :x) (:label :b19)) #xb5000000 $cmp-branch-mask)

   ;; Test and branch (immediate)
   ;; The bit number is in two parts: (byte 1 31) and (byte 4 19).
   ;; 14-bit word displacement in (byte 14 5)
   (def tbz ((:rt :w) :tbit-w (:label :b14)) #x36000000 $test-branch-mask)
   (def tbz ((:rt :x) :tbit-x (:label :b14)) #x36000000 $test-branch-mask)
   (def tbnz ((:rt :w) :tbit-w (:label :b14)) #x37000000 $test-branch-mask)
   (def tbnz ((:rt :x) :tbit-x (:label :b14)) #x37000000 $test-branch-mask)

   ;;; C4.1.66  Loads and Stores

      ;; Load/store register pair (post-indexed)
   (def stp ((:rt :w) (:rt2 :w) (:mem-post (:base :x/sp) (:imm :poff2))) #x28800000 $ldstpair-mask)
   (def stp ((:rt :x) (:rt2 :x) (:mem-post (:base :x/sp) (:imm :poff3))) #xa8800000 $ldstpair-mask)
   (def ldp ((:rt :w) (:rt2 :w) (:mem-post (:base :x/sp) (:imm :poff2))) #x28c00000 $ldstpair-mask)     
   (def ldp ((:rt :x) (:rt2 :x) (:mem-post (:base :x/sp) (:imm :poff3))) #xa8c00000 $ldstpair-mask)

   ;; Load/store register pair (offset)
   (def stp ((:rt :w) (:rt2 :w) (:mem-scaled (:base :x/sp) (:imm :poff2))) #x29000000 $ldstpair-mask)     
   (def ldp ((:rt :w) (:rt2 :w) (:mem-scaled (:base :x/sp) (:imm :poff2))) #x29400000 $ldstpair-mask)
   (def stp ((:rt :x) (:rt2 :x) (:mem-scaled (:base :x/sp) (:imm :poff3))) #xa9000000 $ldstpair-mask)
   (def ldp ((:rt :x) (:rt2 :x) (:mem-scaled (:base :x/sp) (:imm :poff3))) #xa9400000 $ldstpair-mask)

   ;; Load/store register pair (pre-indexed)
   (def stp ((:rt :w) (:rt2 :w) (:mem-pre (:base :x/sp) (:imm :poff2))) #x29800000 $ldstpair-mask)
   (def ldp ((:rt :w) (:rt2 :w) (:mem-pre (:base :x/sp) (:imm :poff2))) #x29c00000 $ldstpair-mask)
   (def stp ((:rt :x) (:rt2 :x) (:mem-pre (:base :x/sp) (:imm :poff3))) #xa9800000 $ldstpair-mask)
   (def ldp ((:rt :x) (:rt2 :x) (:mem-pre (:base :x/sp) (:imm :poff3))) #xa9c00000 $ldstpair-mask)

   ;; Load/store register (unscaled immediate)
   (def sturb ((:rt :w) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #x38000000 $ldst-unscaled-mask)
   (def ldurb ((:rt :w) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #x38400000 $ldst-unscaled-mask)
   (def ldursb ((:rt :x) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #x38800000 $ldst-unscaled-mask)
   (def ldursb ((:rt :w) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #x38c00000 $ldst-unscaled-mask)
   (def sturh ((:rt :w) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #x78000000 $ldst-unscaled-mask)
   (def ldurh ((:rt :w) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #x78400000 $ldst-unscaled-mask)
   (def ldursh ((:rt :x) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #x78800000 $ldst-unscaled-mask)
   (def ldursh ((:rt :w) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #x78c00000 $ldst-unscaled-mask)
   (def stur ((:rt :w) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #xb8000000 $ldst-unscaled-mask)
   (def ldur ((:rt :w) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #xb8400000 $ldst-unscaled-mask)
   (def ldursw ((:rt :x) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #xb8800000 $ldst-unscaled-mask)
   (def stur ((:rt :x) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #xf8000000 $ldst-unscaled-mask)
   (def ldur ((:rt :x) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #xf8400000 $ldst-unscaled-mask)

   ;; Load/store register (immediate post-indexed)
   (def strb ((:rt :w) (:mem-post (:base :x/sp) (:imm :simm9))) #x38000400 $ldst-unscaled-mask)
   (def ldrb ((:rt :w) (:mem-post (:base :x/sp) (:imm :simm9))) #x38400400 $ldst-unscaled-mask)
   (def ldrsb ((:rt :x) (:mem-post (:base :x/sp) (:imm :simm9))) #x38800400 $ldst-unscaled-mask)
   (def ldrsb ((:rt :w) (:mem-post (:base :x/sp) (:imm :simm9))) #x38c00400 $ldst-unscaled-mask)   
   (def strh ((:rt :w) (:mem-post (:base :x/sp) (:imm :simm9))) #x78000400 $ldst-unscaled-mask)
   (def ldrh ((:rt :w) (:mem-post (:base :x/sp) (:imm :simm9))) #x78400400 $ldst-unscaled-mask)
   (def ldrsh ((:rt :x) (:mem-post (:base :x/sp) (:imm :simm9))) #x78800400 $ldst-unscaled-mask)
   (def ldrsh ((:rt :w) (:mem-post (:base :x/sp) (:imm :simm9))) #x78c00400 $ldst-unscaled-mask)
   (def str ((:rt :w) (:mem-post (:base :x/sp) (:imm :simm9))) #xb8000400 $ldst-unscaled-mask)
   (def ldr ((:rt :w) (:mem-post (:base :x/sp) (:imm :simm9))) #xb8400400 $ldst-unscaled-mask)
   (def ldrsw ((:rt :x) (:mem-post (:base :x/sp) (:imm :simm9))) #xb8800400 $ldst-unscaled-mask)
   (def str ((:rt :x) (:mem-post (:base :x/sp) (:imm :simm9))) #xf8000400 $ldst-unscaled-mask)   
   (def ldr ((:rt :x) (:mem-post (:base :x/sp) (:imm :simm9))) #xf8400400 $ldst-unscaled-mask)

   ;; Load/store register (immediate pre-indexed)
   (def strb ((:rt :w) (:mem-pre (:base :x/sp) (:imm :simm9))) #x38000c00 $ldst-unscaled-mask)
   (def ldrb ((:rt :w) (:mem-pre (:base :x/sp) (:imm :simm9))) #x38400c00 $ldst-unscaled-mask)
   (def ldrsb ((:rt :x) (:mem-pre (:base :x/sp) (:imm :simm9))) #x38800c00 $ldst-unscaled-mask)
   (def ldrsb ((:rt :w) (:mem-pre (:base :x/sp) (:imm :simm9))) #x38c00c00 $ldst-unscaled-mask)
   (def strh ((:rt :w) (:mem-pre (:base :x/sp) (:imm :simm9))) #x78000c00 $ldst-unscaled-mask)
   (def ldrh ((:rt :w) (:mem-pre (:base :x/sp) (:imm :simm9))) #x78400c00 $ldst-unscaled-mask)
   (def ldrsh ((:rt :x) (:mem-pre (:base :x/sp) (:imm :simm9))) #x78800c00 $ldst-unscaled-mask)
   (def ldrsh ((:rt :w) (:mem-pre (:base :x/sp) (:imm :simm9))) #x78c00c00 $ldst-unscaled-mask)
   (def str ((:rt :w) (:mem-pre (:base :x/sp) (:imm :simm9))) #xb8000c00 $ldst-unscaled-mask)
   (def ldr ((:rt :w) (:mem-pre (:base :x/sp) (:imm :simm9))) #xb8400c00 $ldst-unscaled-mask)
   (def ldrsw ((:rt :x) (:mem-pre (:base :x/sp) (:imm :simm9))) #xb8800c00 $ldst-unscaled-mask)
   (def str ((:rt :x) (:mem-pre (:base :x/sp) (:imm :simm9))) #xf8000c00 $ldst-unscaled-mask)
   (def ldr ((:rt :x) (:mem-pre (:base :x/sp) (:imm :simm9))) #xf8400c00 $ldst-unscaled-mask)

   ;; Load/store register (register offset)
   (def strb ((:rt :w) (:mem-regoff (:base :x/sp) (:index :regoff0))) #x38200800 $ldst-regoff-mask)
   (def ldrb ((:rt :w) (:mem-regoff (:base :x/sp) (:index :regoff0))) #x38600800 $ldst-regoff-mask)
   (def ldrsb ((:rt :x) (:mem-regoff (:base :x/sp) (:index :regoff0))) #x38a00800 $ldst-regoff-mask)
   (def ldrsb ((:rt :w) (:mem-regoff (:base :x/sp) (:index :regoff0))) #x38e00800 $ldst-regoff-mask)
   (def strh ((:rt :w) (:mem-regoff (:base :x/sp) (:index :regoff1))) #x78200800 $ldst-regoff-mask)
   (def ldrh ((:rt :w) (:mem-regoff (:base :x/sp) (:index :regoff1))) #x78600800 $ldst-regoff-mask)
   (def ldrsh ((:rt :x) (:mem-regoff (:base :x/sp) (:index :regoff1))) #x78a00800 $ldst-regoff-mask)
   (def ldrsh ((:rt :w) (:mem-regoff (:base :x/sp) (:index :regoff1))) #x78e00800 $ldst-regoff-mask)
   (def str ((:rt :w) (:mem-regoff (:base :x/sp) (:index :regoff2))) #xb8200800 $ldst-regoff-mask)
   (def ldr ((:rt :w) (:mem-regoff (:base :x/sp) (:index :regoff2))) #xb8600800 $ldst-regoff-mask)
   (def ldrsw ((:rt :x) (:mem-regoff (:base :x/sp) (:index :regoff2))) #xb8a00800 $ldst-regoff-mask)
   (def str ((:rt :x) (:mem-regoff (:base :x/sp) (:index :regoff3))) #xf8200800 $ldst-regoff-mask)
   (def ldr ((:rt :x) (:mem-regoff (:base :x/sp) (:index :regoff3))) #xf8600800 $ldst-regoff-mask)

   ;; Load/store register (unsigned immediate)
   (def strb ((:rt :w) (:mem-scaled (:base :x/sp) (:imm :uoff0))) #x39000000 $ldst-pos-mask)
   (def ldrb ((:rt :w) (:mem-scaled (:base :x/sp) (:imm :uoff0))) #x39400000 $ldst-pos-mask)
   (def ldrsb ((:rt :x) (:mem-scaled (:base :x/sp) (:imm :uoff0))) #x39800000 $ldst-pos-mask)
   (def ldrsb ((:rt :w) (:mem-scaled (:base :x/sp) (:imm :uoff0))) #x39c00000 $ldst-pos-mask)
   (def strh ((:rt :w) (:mem-scaled (:base :x/sp) (:imm :uoff1))) #x79000000 $ldst-pos-mask)
   (def ldrh ((:rt :w) (:mem-scaled (:base :x/sp) (:imm :uoff1))) #x79400000 $ldst-pos-mask)
   (def ldrsh ((:rt :x) (:mem-scaled (:base :x/sp) (:imm :uoff1))) #x79800000 $ldst-pos-mask)
   (def ldrsh ((:rt :w) (:mem-scaled (:base :x/sp) (:imm :uoff1))) #x79c00000 $ldst-pos-mask)
   (def str ((:rt :w) (:mem-scaled (:base :x/sp) (:imm :uoff2))) #xb9000000 $ldst-pos-mask)
   (def ldr ((:rt :w) (:mem-scaled (:base :x/sp) (:imm :uoff2))) #xb9400000 $ldst-pos-mask)
   (def ldrsw ((:rt :x) (:mem-scaled (:base :x/sp) (:imm :uoff2))) #xb9800000 $ldst-pos-mask)
   (def str ((:rt :x) (:mem-scaled (:base :x/sp) (:imm :uoff3))) #xf9000000 $ldst-pos-mask)
   (def ldr ((:rt :x) (:mem-scaled (:base :x/sp) (:imm :uoff3))) #xf9400000 $ldst-pos-mask)

   ;;; Data Processing -- Register

   ;; Data-processing (2 source)
   (def udiv ((:rd :w) (:rn :w) (:rm :w)) #x1ac00800 $dp-2src-mask)
   (def udiv ((:rd :x) (:rn :x) (:rm :x)) #x9ac00800 $dp-2src-mask)
   (def sdiv ((:rd :w) (:rn :w) (:rm :w)) #x1ac00c00 $dp-2src-mask)
   (def sdiv ((:rd :x) (:rn :x) (:rm :x)) #x9ac00c00 $dp-2src-mask)
   (def lslv ((:rd :w) (:rn :w) (:rm :w)) #x1ac02000 $dp-2src-mask)
   (def lslv ((:rd :x) (:rn :x) (:rm :x)) #x9ac02000 $dp-2src-mask)
   (def lsrv ((:rd :w) (:rn :w) (:rm :w)) #x1ac02400 $dp-2src-mask)
   (def lsrv ((:rd :x) (:rn :x) (:rm :x)) #x9ac02400 $dp-2src-mask)
   (def asrv ((:rd :w) (:rn :w) (:rm :w)) #x1ac02800 $dp-2src-mask)
   (def asrv ((:rd :x) (:rn :x) (:rm :x)) #x9ac02800 $dp-2src-mask)
   (def rorv ((:rd :w) (:rn :w) (:rm :w)) #x1ac02c00 $dp-2src-mask)
   (def rorv ((:rd :x) (:rn :x) (:rm :x)) #x9ac02c00 $dp-2src-mask)

   ;; shift aliases.  The register forms alias lslv/lsrv/asrv/rorv; the
   ;; immediate forms alias ubfm/sbfm (lsl/lsr/asr) and extr (ror), with
   ;; immr/imms computed from the shift amount.  Register vs immediate
   ;; third operand selects the form.
   (def lsl ((:rd :w) (:rn :w) (:rm :w)) #x1ac02000 0 :flags :alias)
   (def lsl ((:rd :x) (:rn :x) (:rm :x)) #x9ac02000 0 :flags :alias)
   (def lsl ((:rd :w) (:rn :w) :lsl-imm-w) #x53000000 0 :flags :alias)
   (def lsl ((:rd :x) (:rn :x) :lsl-imm-x) #xd3400000 0 :flags :alias)
   (def lsr ((:rd :w) (:rn :w) (:rm :w)) #x1ac02400 0 :flags :alias)
   (def lsr ((:rd :x) (:rn :x) (:rm :x)) #x9ac02400 0 :flags :alias)
   (def lsr ((:rd :w) (:rn :w) :lsr-imm-w) #x53000000 0 :flags :alias)
   (def lsr ((:rd :x) (:rn :x) :lsr-imm-x) #xd3400000 0 :flags :alias)
   (def asr ((:rd :w) (:rn :w) (:rm :w)) #x1ac02800 0 :flags :alias)
   (def asr ((:rd :x) (:rn :x) (:rm :x)) #x9ac02800 0 :flags :alias)
   (def asr ((:rd :w) (:rn :w) :asr-imm-w) #x13000000 0 :flags :alias)
   (def asr ((:rd :x) (:rn :x) :asr-imm-x) #x93400000 0 :flags :alias)
   (def ror ((:rd :w) (:rn :w) (:rm :w)) #x1ac02c00 0 :flags :alias)
   (def ror ((:rd :x) (:rn :x) (:rm :x)) #x9ac02c00 0 :flags :alias)
   (def ror ((:rd :w) (:rn+rm :w) :imms-w) #x13800000 0 :flags :alias)
   (def ror ((:rd :x) (:rn+rm :x) :imms-x) #x93c00000 0 :flags :alias)

   ;; Data-processing (1 source)
   (def rbit ((:rd :w) (:rn :w)) #x5ac00000 $dp-1src-mask)
   (def rbit ((:rd :x) (:rn :x)) #xdac00000 $dp-1src-mask)
   (def rev16 ((:rd :w) (:rn :w)) #x5ac00400 $dp-1src-mask)
   (def rev16 ((:rd :x) (:rn :x)) #xdac00400 $dp-1src-mask)
   (def rev ((:rd :w) (:rn :w)) #x5ac00800 $dp-1src-mask)
   (def rev ((:rd :x) (:rn :x)) #xdac00c00 $dp-1src-mask)
   (def rev32 ((:rd :x) (:rn :x)) #xdac00800 $dp-1src-mask)
   (def clz ((:rd :w) (:rn :w)) #x5ac01000 $dp-1src-mask)
   (def clz ((:rd :x) (:rn :x)) #xdac01000 $dp-1src-mask)
   (def cls ((:rd :w) (:rn :w)) #x5ac01400 $dp-1src-mask)
   (def cls ((:rd :x) (:rn :x)) #xdac01400 $dp-1src-mask)

   ;; Logical (shifted register)
   (def and ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x0a000000 $log-shift-mask)
   (def and ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #x8a000000 $log-shift-mask)
   (def bic ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x0a200000 $log-shift-mask)
   (def bic ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #x8a200000 $log-shift-mask)
   (def orr ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x2a000000 $log-shift-mask)
   (def orr ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xaa000000 $log-shift-mask)
   (def mov ((:rd :w) (:rm :w)) #x2a0003e0 0 :flags :alias)
   (def mov ((:rd :x) (:rm :x)) #xaa0003e0 0 :flags :alias)
   (def orn ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x2a200000 $log-shift-mask)
   (def orn ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xaa200000 $log-shift-mask)
   (def mvn ((:rd :w) (:rm :w-shift-ror)) #x2a2003e0 0 :flags :alias)
   (def mvn ((:rd :x) (:rm :x-shift-ror)) #xaa2003e0 0 :flags :alias)
   (def eor ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x4a000000 $log-shift-mask)
   (def eor ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xca000000 $log-shift-mask)
   (def eon ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x4a200000 $log-shift-mask)
   (def eon ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xca200000 $log-shift-mask)
   (def ands ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x6a000000 $log-shift-mask)
   (def ands ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xea000000 $log-shift-mask)
   (def tst ((:rn :w) (:rm :w-shift-ror)) #x6a00001f 0 :flags :alias)
   (def tst ((:rn :x) (:rm :x-shift-ror)) #xea00001f 0 :flags :alias)
   (def bics ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x6a200000 $log-shift-mask)
   (def bics ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xea200000 $log-shift-mask)

   ;; Add/subtract (shifted register)
   (def add ((:rd :w) (:rn :w) (:rm :w-shift)) #x0b000000 $addsub-shift-mask)
   (def add ((:rd :x) (:rn :x) (:rm :x-shift)) #x8b000000 $addsub-shift-mask)
   (def adds ((:rd :w) (:rn :w) (:rm :w-shift)) #x2b000000 $addsub-shift-mask)
   (def adds ((:rd :x) (:rn :x) (:rm :x-shift)) #xab000000 $addsub-shift-mask)
   (def cmn ((:rn :w) (:rm :w-shift)) #x2b00001f 0 :flags :alias)
   (def cmn ((:rn :x) (:rm :x-shift)) #xab00001f 0 :flags :alias)
   (def sub ((:rd :w) (:rn :w) (:rm :w-shift)) #x4b000000 $addsub-shift-mask)
   (def sub ((:rd :x) (:rn :x) (:rm :x-shift)) #xcb000000 $addsub-shift-mask)
   (def subs ((:rd :w) (:rn :w) (:rm :w-shift)) #x6b000000 $addsub-shift-mask)
   (def subs ((:rd :x) (:rn :x) (:rm :x-shift)) #xeb000000 $addsub-shift-mask)
   (def cmp ((:rn :w) (:rm :w-shift)) #x6b00001f 0 :flags :alias)
   (def cmp ((:rn :x) (:rm :x-shift)) #xeb00001f 0 :flags :alias)
   (def neg ((:rd :w) (:rm :w-shift)) #x4b0003e0 0 :flags :alias)
   (def neg ((:rd :x) (:rm :x-shift)) #xcb0003e0 0 :flags :alias)
   (def negs ((:rd :w) (:rm :w-shift)) #x6b0003e0 0 :flags :alias)
   (def negs ((:rd :x) (:rm :x-shift)) #xeb0003e0 0 :flags :alias)

   ;; Add/subtract (extended register)
   (def add ((:rd :w/sp) (:rn :w/sp) (:rm :w-ext)) #x0b200000 $addsub-ext-mask)
   (def add ((:rd :x/sp) (:rn :x/sp) (:rm :x-ext)) #x8b200000 $addsub-ext-mask)
   (def adds ((:rd :w) (:rn :w/sp) (:rm :w-ext)) #x2b200000 $addsub-ext-mask)
   (def adds ((:rd :x) (:rn :x/sp) (:rm :x-ext)) #xab200000 $addsub-ext-mask)
   (def cmn ((:rn :w/sp) (:rm :w-ext)) #x2b20001f 0 :flags :alias)
   (def cmn ((:rn :x/sp) (:rm :x-ext)) #xab20001f 0 :flags :alias)
   (def sub ((:rd :w/sp) (:rn :w/sp) (:rm :w-ext)) #x4b200000 $addsub-ext-mask)
   (def sub ((:rd :x/sp) (:rn :x/sp) (:rm :x-ext)) #xcb200000 $addsub-ext-mask)
   (def subs ((:rd :w) (:rn :w/sp) (:rm :w-ext)) #x6b200000 $addsub-ext-mask)
   (def subs ((:rd :x) (:rn :x/sp) (:rm :x-ext)) #xeb200000 $addsub-ext-mask)
   (def cmp ((:rn :w/sp) (:rm :w-ext)) #x6b20001f 0 :flags :alias)
   (def cmp ((:rn :x/sp) (:rm :x-ext)) #xeb20001f 0 :flags :alias)

   ;; Add/subtract (with carry)
   (def adc ((:rd :w) (:rn :w) (:rm :w)) #x1a000000 $rm/rn/rd-mask)
   (def adc ((:rd :x) (:rn :x) (:rm :x)) #x9a000000 $rm/rn/rd-mask)
   (def adcs ((:rd :w) (:rn :w) (:rm :w)) #x3a000000 $rm/rn/rd-mask)
   (def adcs ((:rd :x) (:rn :x) (:rm :x)) #xba000000 $rm/rn/rd-mask)
   (def sbc ((:rd :w) (:rn :w) (:rm :w)) #x5a000000 $rm/rn/rd-mask)
   (def sbc ((:rd :x) (:rn :x) (:rm :x)) #xda000000 $rm/rn/rd-mask)
   (def ngc ((:rd :w) (:rm :w)) #x5a0003e0 0 :flags :alias)
   (def ngc ((:rd :x) (:rm :x)) #xda0003e0 0 :flags :alias)
   (def sbcs ((:rd :w) (:rn :w) (:rm :w)) #x7a000000 $rm/rn/rd-mask)
   (def sbcs ((:rd :x) (:rn :x) (:rm :x)) #xfa000000 $rm/rn/rd-mask)
   (def ngcs ((:rd :w) (:rm :w)) #x7a0003e0 0 :flags :alias)
   (def ngcs ((:rd :x) (:rm :x)) #xfa0003e0 0 :flags :alias)

   ;; Conditional compare (register)
   (def ccmp ((:rn :w) (:rm :w) :nzcv :cond) #x7a400000 $condcmp-mask)
   (def ccmp ((:rn :x) (:rm :x) :nzcv :cond) #xfa400000 $condcmp-mask)
   (def ccmn ((:rn :w) (:rm :w) :nzcv :cond) #x3a400000 $condcmp-mask)
   (def ccmn ((:rn :x) (:rm :x) :nzcv :cond) #xba400000 $condcmp-mask)

   ;; Conditional compare (immediate)
   (def ccmp ((:rn :w) :imm5 :nzcv :cond) #x7a400800 $condcmp-mask)
   (def ccmp ((:rn :x) :imm5 :nzcv :cond) #xfa400800 $condcmp-mask)
   (def ccmn ((:rn :w) :imm5 :nzcv :cond) #x3a400800 $condcmp-mask)
   (def ccmn ((:rn :x) :imm5 :nzcv :cond) #xba400800 $condcmp-mask)

   ;; Conditional select
   (def csinc ((:rd :w) (:rn :w) (:rm :w) :cond) #x1a800400 $condsel-mask)
   (def csinc ((:rd :x) (:rn :x) (:rm :x) :cond) #x9a800400 $condsel-mask)
   (def csinv ((:rd :w) (:rn :w) (:rm :w) :cond) #x5a800000 $condsel-mask)
   (def csinv ((:rd :x) (:rn :x) (:rm :x) :cond) #xda800000 $condsel-mask)
   (def csneg ((:rd :w) (:rn :w) (:rm :w) :cond) #x5a800400 $condsel-mask)
   (def csneg ((:rd :x) (:rn :x) (:rm :x) :cond) #xda800400 $condsel-mask)
   (def csel ((:rd :w) (:rn :w) (:rm :w) :cond) #x1a800000 $condsel-mask)
   (def csel ((:rd :x) (:rn :x) (:rm :x) :cond) #x9a800000 $condsel-mask)

   ;; Conditional-select aliases.  Each encodes the inverse condition
   ;; (:cond-inv).  cset/csetm set Rn=Rm=zr (31); cinc/cinv/cneg write
   ;; their one source register into both Rn and Rm (the :rn+rm role).
   (def cset ((:rd :w) :cond-inv) #x1a9f07e0 0 :flags :alias)
   (def cset ((:rd :x) :cond-inv) #x9a9f07e0 0 :flags :alias)
   (def csetm ((:rd :w) :cond-inv) #x5a9f03e0 0 :flags :alias)
   (def csetm ((:rd :x) :cond-inv) #xda9f03e0 0 :flags :alias)
   (def cinc ((:rd :w) (:rn+rm :w) :cond-inv) #x1a800400 0 :flags :alias)
   (def cinc ((:rd :x) (:rn+rm :x) :cond-inv) #x9a800400 0 :flags :alias)
   (def cinv ((:rd :w) (:rn+rm :w) :cond-inv) #x5a800000 0 :flags :alias)
   (def cinv ((:rd :x) (:rn+rm :x) :cond-inv) #xda800000 0 :flags :alias)
   (def cneg ((:rd :w) (:rn+rm :w) :cond-inv) #x5a800400 0 :flags :alias)
   (def cneg ((:rd :x) (:rn+rm :x) :cond-inv) #xda800400 0 :flags :alias)

   ;; Data-processing (3-source)
   (def madd ((:rd :w) (:rn :w) (:rm :w) (:ra :w)) #x1b000000 $dp-3src-mask)
   (def madd ((:rd :x) (:rn :x) (:rm :x) (:ra :x)) #x9b000000 $dp-3src-mask)
   (def msub ((:rd :w) (:rn :w) (:rm :w) (:ra :w)) #x1b008000 $dp-3src-mask)
   (def msub ((:rd :x) (:rn :x) (:rm :x) (:ra :x)) #x9b008000 $dp-3src-mask)
   (def smaddl ((:rd :x) (:rn :w) (:rm :w) (:ra :x)) #x9b200000 $dp-3src-mask)
   (def smsubl ((:rd :x) (:rn :w) (:rm :w) (:ra :x)) #x9b208000 $dp-3src-mask)
   (def umaddl ((:rd :x) (:rn :w) (:rm :w) (:ra :x)) #x9ba00000 $dp-3src-mask)
   (def umsubl ((:rd :x) (:rn :w) (:rm :w) (:ra :x)) #x9ba08000 $dp-3src-mask)
   ;; the high-multiplies have no Ra operand (it's fixed at 31)
   (def smulh ((:rd :x) (:rn :x) (:rm :x)) #x9b407c00 $dp-2src-mask)
   (def umulh ((:rd :x) (:rn :x) (:rm :x)) #x9bc07c00 $dp-2src-mask)
   ;; multiply aliases: madd/msub/... with Ra = zr (31)
   (def mul ((:rd :w) (:rn :w) (:rm :w)) #x1b007c00 0 :flags :alias)
   (def mul ((:rd :x) (:rn :x) (:rm :x)) #x9b007c00 0 :flags :alias)
   (def mneg ((:rd :w) (:rn :w) (:rm :w)) #x1b00fc00 0 :flags :alias)
   (def mneg ((:rd :x) (:rn :x) (:rm :x)) #x9b00fc00 0 :flags :alias)
   (def smull ((:rd :x) (:rn :w) (:rm :w)) #x9b207c00 0 :flags :alias)
   (def smnegl ((:rd :x) (:rn :w) (:rm :w)) #x9b20fc00 0 :flags :alias)
   (def umull ((:rd :x) (:rn :w) (:rm :w)) #x9ba07c00 0 :flags :alias)
   (def umnegl ((:rd :x) (:rn :w) (:rm :w)) #x9ba0fc00 0 :flags :alias)
   ))

(defvar *instruction-template-lists* (make-hash-table :test #'equalp))

(defun initialize-templates ()
  (clrhash *instruction-template-lists*)
  (dotimes (i (length *instruction-templates*))
    (let* ((template (svref *instruction-templates* i))
           (name (instruction-template-name template)))
      (push template (gethash name *instruction-template-lists*)))))

(initialize-templates)


;;; This is the entry point to the assembler.

;;; lap-form is a list and its car isn't a pseudo-op or lapmacro
(defun assemble (seg lap-form)
  (declare (ignore seg))
  (let ((insn (%make-instruction lap-form)))
    (destructuring-bind (name . lap-operands) lap-form
      (let ((templates (gethash (string-downcase name)
                                *instruction-template-lists*)))
        (unless templates
          (error "Unknown instruction ~s" lap-form))
        ;; 1. Parse LAP-format operands into operand structs
        (let ((operands (mapcar #'parse-operand lap-operands)))
          (setf (instruction-parsed-operands insn) operands)
          ;; 2. find a template whose operands match ours
          (dolist (template templates
                            (explain-no-match name lap-operands operands
                                              templates))
            (when (match-template template operands)
              ;; 3. encode the operands into the instruction word
              (setf (instruction-template insn) template)
              (encode-operands insn)
              (return insn))))))))

;;; Parsing operands in LAP notation

;;; The operand classes: each denotes the set of concrete operands
;;; acceptable in a register role or as an immediate.
(defparameter *operand-classes*
  '(:x             ;Xn or XZR
    :x/sp          ;Xn or SP
    :w             ;Wn or WZR
    :w/sp          ;Wn or WSP
    :sp            ;SP, specifically
    :wsp           ;WSP, specifically
    :aimm          ;uimm12, maybe shifted left 12 bits
    :limm          ;fancy logical immediate
    :simm9
    :movw-x        ;16-bit immediate, LSL 0/16/32/48 (move wide, X)
    :movw-w        ;16-bit immediate, LSL 0/16 (move wide, W)
    :immr-x        ;immr field, 0..63 (bitfield, X)
    :immr-w        ;immr field, 0..31 (bitfield, W)
    :imms-x        ;imms field, 0..63 (bitfield imms / extr lsb, X)
    :imms-w        ;imms field, 0..31 (bitfield imms / extr lsb, W)
    :tbit-x        ;tbz/tbnz bit number 0..63, split b5 @ 31 + b40 @ 23:19 (X)
    :tbit-w        ;tbz/tbnz bit number 0..31 (W); b5 is always 0
    :exc16         ;16-bit exception immediate @ 20:5 (brk/hlt/svc)
    :udf16         ;16-bit undefined immediate @ 15:0 (udf)
    :baropt        ;4-bit barrier option (CRm) @ 11:8 (dmb/dsb); 15 = full system
    :imm5          ;5-bit unsigned immediate @ 20:16 (ccmp/ccmn immediate form)
    :nzcv          ;4-bit flags immediate @ 3:0 (ccmp/ccmn)
    :lsl-imm-x     ;lsl #n alias of ubfm: immr=(-n)&63, imms=63-n (X)
    :lsl-imm-w     ; ... and the W form (immr=(-n)&31, imms=31-n)
    :lsr-imm-x     ;lsr/asr #n alias of u/sbfm: immr=n, imms=63 (X)
    :lsr-imm-w     ; ... and the W form (immr=n, imms=31)
    :asr-imm-x     ;asr #n: same field encoding as :lsr-imm-x, sbfm base
    :asr-imm-w
    :cond          ;4-bit condition @ 15:12 (csel/csinc/ccmp ...), written (:? cc)
    :cond-inv      ;like :cond but encodes the inverse (cset/cinc ... aliases)
    :pcrel         ;signed 21-bit value, split into immlo/immhi (adr/adrp)
    :b26           ;branch target, imm26 @ 25:0 (b, bl)
    :b19           ;branch target, imm19 @ 23:5 (b.cond, cbz/cbnz)
    :b14           ;branch target, imm14 @ 18:5 (tbz/tbnz)
    :uoff0         ;scaled unsigned offset; N = log2(access size in bytes),
    :uoff1         ; so the access-size scale is baked into the class and the
    :uoff2         ; offset predicate needs no external scale-shift
    :uoff3
    :uoff4
    :regoff0       ;register index with natural scale N = log2(access size):
    :regoff1       ; an Xm (lsl/sxtx) or Wm (uxtw/sxtw), amount 0 or N.  The
    :regoff2       ; option @ 15:13 comes from the extend, S @ 12 from the amount.
    :regoff3
    :poff2         ;load/store-pair signed scaled offset, imm7 @ 21:15 (W pair)
    :poff3         ; ... and the X pair (scale 3); the access size is baked in
    :x-shift       ;Xn lsl/lsr/asr by 0...63 (add/sub shifted register)
    :w-shift       ;Wn lsl/lsr/asr by 0...31
    :x-shift-ror   ;Xn lsl/lsr/asr/ror by 0...63 (logical shifted register)
    :w-shift-ror   ;Wn lsl/lsr/asr/ror by 0...31
    :x-ext         ;Xn, maybe shifted
    :w-ext         ;Wn, maybe extended
    ))

;;; The logical instructions permit :ror but register shifts don't.
(defparameter *shift-operators*  '(:lsl :lsr :asr :ror))
(defparameter *extend-operators* '(:uxtb :uxth :uxtw :uxtx
                                   :sxtb :sxth :sxtw :sxtx))

(defstruct immediate-operand
  value       ;an integer
  shift       ;how many bits to shift by (:lsl only), if applicable
  )

(defstruct register-operand
  register
  modifier                     ;nil or a shift/extend operator keyword
  (amount 0))                  ;shift/extend amount

(defstruct memory-operand
  base                              ;a register operand
  offset                            ;nil or the offset, specified as
                                    ; an immediate or register operand
  pre-indexed-p                     ;one or the other;
  post-indexed-p)                   ; having both set makes no sense

(defstruct label-operand
  name)                               ;the label's name (a symbol)

(defstruct condition-operand
  name                                ;the condition name (a symbol)
  value)                              ;its 4-bit encoding

(defun need-register (name)
  (or (gethash (string name) *registers-by-name*)
      (error "No register named ~a" name)))

(defun register-name-p (name)
  (gethash (string name) *registers-by-name*))

(defun parse-register-operand (form)
  ;; Recognize a plain register name like x0 or a shifted or extended
  ;; register of the form (x0 modifier {amount})
  (flet ((parse-shift/extend (form)
           (destructuring-bind (name modifier &optional (amount 0)) form
             (unless (or (member modifier *shift-operators* :test #'eq)
                         (member modifier *extend-operators* :test #'eq))
               (error "~s is not a shift or extend operator" modifier))
             (make-register-operand :register (need-register name)
                                    :modifier modifier :amount amount))))
    (if (consp form)
      (if (<= 2 (length form) 3)
        (error "Invalid register form ~s" form)
        (parse-shift/extend form))
      (make-register-operand :register (need-register form)))))

(defun parse-immediate-operand (form)
  ;; Regcognize (:$ value) or (:$ value :lsl amount).
  ;; Legal values and shift amounts are not checked here.
  (unless (and (consp form)
               (eq (car form) :$)
               (let ((l (length form)))
                 (or (= l 2) (= l 4))))
    (error "Invalid immediate operand ~s" form))
  (destructuring-bind (marker value &optional op (shift 0)) form
    (declare (ignore marker))
    (when op
      (unless (eq op :lsl)
        (error "Only :lsl is valid for an immediate: ~s" form)))
    (make-immediate-operand :value value :shift shift)))

(defun parse-memory-operand (form)
  ;; Recognize (:@ base), (:@ base offset), (:@! ...), (:@+ ...)
  (destructuring-bind (marker base &optional offset) form
    (make-memory-operand
     :base (parse-register-operand base)
     :offset (and offset (parse-operand offset))
     :pre-indexed-p (eq marker :@!)
     :post-indexed-p (eq marker :@+))))

(defun parse-label-operand (form)
  ;; A branch target written as a bare symbol naming a label.
  (make-label-operand :name form))

(defparameter *arm64-condition-names*
  '(("eq" . 0)                          ;equal
    ("ne" . 1)                          ;not equal
    ("cs" . 2) ("hs" . 2)               ;carry set, unsigned higher or same
    ("cc" . 3) ("lo" . 3)               ;carry clear, unsigned lower
    ("mi" . 4)                          ;minus, negative
    ("pl" . 5)                          ;plus, positive or zero
    ("vs" . 6)                          ;overflow
    ("vc" . 7)                          ;no overflow
    ("hi" . 8)                          ;unsigned higher
    ("ls" . 9)                          ;unsigned lower or same
    ("ge" . 10)                         ;signed >=
    ("lt" . 11)                         ;signed <
    ("gt" . 12)                         ;signed >
    ("le" . 13)                         ;signed <=
    ("al" . 14)                         ;always
    ("nv" . 15)))                       ;identical to always

(defun lookup-arm64-condition-name (name)
  (cdr (assoc name *arm64-condition-names* :test #'string-equal)))

(defun lookup-arm64-condition-value (val)
  (car (rassoc val *arm64-condition-names* :test #'eq)))

(defun need-arm64-condition-name (name)
  (or (lookup-arm64-condition-name name)
      (error "Unknown ARM64 condition name ~s." name)))

(defun parse-condition-operand (form)
  ;; A condition written (:? cc), e.g. (:? eq).  The name is validated
  ;; here so a bogus condition is caught at parse time.
  (destructuring-bind (marker name) form
    (declare (ignore marker))
    (make-condition-operand :name name
                            :value (need-arm64-condition-name name))))

(defun parse-operand (form)
  ;; Recognize an operand written in LAP notation.
  (cond
    ((and form (symbolp form))
     ;; A bare symbol naming a register is a register; any other bare
     ;; symbol is a label reference (a branch target).
     (if (register-name-p form)
       (parse-register-operand form)
       (parse-label-operand form)))
    ((consp form)
     (case (car form)
       (:$ (parse-immediate-operand form))
       (:? (parse-condition-operand form))
       ((:@ :@! :@+) (parse-memory-operand form))
       (t (if (register-name-p (car form))
            ;; maybe a scaled/extended register like (x0 modifier {amt})
            (parse-register-operand form)
            (error "Unrecognized operand ~s" form)))))
    (t (error "Unrecognized operand ~s" form))))


;;; Matching parsed operands against a template

;;; Matching parsed operands against a template.  A template matches iff its
;;; operand specs and the parsed operands agree in count and, pairwise, the
;;; operand satisfies the spec.  Matching is a cheap boolean predicate run
;;; against every candidate; encoding is a separate later pass.

;;; Return true if the register in the register-operand is a member of
;;; the given operand class.
(defun match-register-operand (r-op class)
  (let* ((r (register-operand-register r-op))
         (family (register-family r))
         (width (register-width r))
         (amount (register-operand-amount r-op))
         (r31-role (if (= (register-number r) 31)
                     (if (logtest (register-flags r) $sp)
                       :stack-pointer
                       :zero-register)))
         (modifier (register-operand-modifier r-op)))
    (flet ((ordinary-gpr-p (required-width required-r31-role)
             (and (eq family :gpr)
                  (= required-width width)
                  (null modifier)
                  (or (null r31-role)
                      (eq required-r31-role r31-role))))
           (sp-p (required-width)
             ;; the stack pointer specifically: register 31 in its SP role
             (and (eq family :gpr)
                  (= required-width width)
                  (null modifier)
                  (eq r31-role :stack-pointer)))
           (shifted-p (required-width allow-ror)
             ;; Rm shifted by lsl/lsr/asr (+ror for logical), or a bare
             ;; register (lsl #0); never SP.
             (and (eq family :gpr)
                  (= required-width width)
                  (not (eq r31-role :stack-pointer))
                  (let ((ops (if allow-ror '(:lsl :lsr :asr :ror) '(:lsl :lsr :asr))))
                    (or (null modifier) (member modifier ops :test #'eq)))
                  (<= 0 amount (1- required-width))))
           (extended-p (inst-width)
             ;; Rm extended (uxtb..sxtx), the lsl alias, or a bare register.
             ;; The Rm width follows the extend option (W for the b/h/w extends,
             ;; X for the x extends), and the x extends need a 64-bit insn.
             ;; Amount is 0..4; never SP.
             (and (eq family :gpr)
                  (not (eq r31-role :stack-pointer))
                  (cond
                    ((or (null modifier) (eq modifier :lsl))
                     (and (= width inst-width)
                          (or (null modifier) (<= 0 amount 4))))
                    ((member modifier *extend-operators* :test #'eq)
                     (let ((needs-x (member modifier '(:uxtx :sxtx) :test #'eq)))
                       (and (= width (if needs-x 64 32))
                            (or (= inst-width 64) (not needs-x))
                            (<= 0 amount 4))))
                    (t nil)))))
      (ecase class
        (:x (ordinary-gpr-p 64 :zero-register))
        (:x/sp (ordinary-gpr-p 64 :stack-pointer))
        (:w (ordinary-gpr-p 32 :zero-register))
        (:w/sp (ordinary-gpr-p 32 :stack-pointer))
        (:sp (sp-p 64))
        (:wsp (sp-p 32))
        (:x-shift (shifted-p 64 nil))
        (:w-shift (shifted-p 32 nil))
        (:x-shift-ror (shifted-p 64 t))
        (:w-shift-ror (shifted-p 32 t))
        (:x-ext (extended-p 64))
        (:w-ext (extended-p 32))))))

;;; The access-size scale for a scaled offset is baked into the class (:uoffN,
;;; N = log2 of the access size in bytes), so the class is self-describing and
;;; this predicate needs only the operand and the class — no template.  Values
;;; are assumed already resolved to integers.
(defun match-immediate-operand (imm-op class)
  (let ((value (immediate-operand-value imm-op))
        (shift (immediate-operand-shift imm-op)))
    (flet ((uoff-p (scale)
             (and (eql shift 0)
                  (zerop (logand value (1- (ash 1 scale))))   ;multiple of access size
                  (typep (ash value (- scale)) '(unsigned-byte 12))))
           (poff-p (scale)                  ;signed scaled 7-bit (load/store pair)
             (and (eql shift 0)
                  (zerop (logand value (1- (ash 1 scale))))
                  (typep (ash value (- scale)) '(signed-byte 7)))))
      (ecase class
        (:aimm  (and (member shift '(0 12)) (typep value '(unsigned-byte 12))))
        (:simm9 (and (eql shift 0) (typep value '(signed-byte 9))))
        (:limm  (and (eql shift 0) (encode-logical-immediate value)))
        (:uoff0 (uoff-p 0))
        (:uoff1 (uoff-p 1))
        (:uoff2 (uoff-p 2))
        (:uoff3 (uoff-p 3))
        (:uoff4 (uoff-p 4))
        (:poff2 (poff-p 2))
        (:poff3 (poff-p 3))
        (:movw-x (and (typep value '(unsigned-byte 16)) (member shift '(0 16 32 48))))
        (:movw-w (and (typep value '(unsigned-byte 16)) (member shift '(0 16))))
        ((:immr-x :imms-x :tbit-x) (and (eql shift 0) (typep value '(integer 0 63))))
        ((:immr-w :imms-w :tbit-w) (and (eql shift 0) (typep value '(integer 0 31))))
        ((:exc16 :udf16) (and (eql shift 0) (typep value '(unsigned-byte 16))))
        (:baropt (and (eql shift 0) (typep value '(unsigned-byte 4))))
        (:imm5 (and (eql shift 0) (typep value '(unsigned-byte 5))))
        (:nzcv (and (eql shift 0) (typep value '(unsigned-byte 4))))
        ((:lsl-imm-x :lsr-imm-x :asr-imm-x)
         (and (eql shift 0) (typep value '(integer 0 63))))
        ((:lsl-imm-w :lsr-imm-w :asr-imm-w)
         (and (eql shift 0) (typep value '(integer 0 31))))
        (:pcrel (and (eql shift 0) (typep value '(signed-byte 21))))
        ))))

(defun regoff-scale (class)
  ;; The natural scale (log2 access size) baked into a :regoffN class.
  (ecase class (:regoff0 0) (:regoff1 1) (:regoff2 2) (:regoff3 3)))

(defun index-option (width modifier)
  ;; The 3-bit option field for a register-offset index, or NIL if the
  ;; width/modifier pairing is illegal.  A bare register or lsl means a
  ;; 64-bit index (UXTX); the w-extends take a 32-bit index.
  (case modifier
    ((nil :lsl) (and (eql width 64) 3))
    (:uxtw (and (eql width 32) 2))
    (:sxtw (and (eql width 32) 6))
    (:sxtx (and (eql width 64) 7))))

(defun match-index-operand (r-op scale)
  ;; A register-offset index: a non-SP GPR with a legal extend for its
  ;; width and an amount of either 0 (S=0) or the natural scale (S=1).
  (and (register-operand-p r-op)
       (let* ((reg (register-operand-register r-op))
              (modifier (register-operand-modifier r-op))
              (amount (register-operand-amount r-op)))
         (and (eq (register-family reg) :gpr)
              (not (logtest (register-flags reg) $sp))
              (index-option (register-width reg) modifier)
              (member amount (list 0 scale))))))

(defun match-memory-operand (mem-operand spec)
  ;; SPEC is (:mem-FORM (:base base-class) ...) and the base must satisfy
  ;; base-class.  The index mode is part of the form: :mem-scaled /
  ;; :mem-unscaled / :mem-regoff are plain (no writeback); :mem-pre /
  ;; :mem-post are the writeback forms.  The immediate forms carry (:imm
  ;; imm-class); :mem-regoff carries (:index index-class).
  (let ((base   (memory-operand-base mem-operand))
        (offset (memory-operand-offset mem-operand))
        (pre    (memory-operand-pre-indexed-p mem-operand))
        (post   (memory-operand-post-indexed-p mem-operand)))
    (flet ((imm-offset-p ()
             ;; an immediate offset of the spec's :imm class, required here
             (and offset (immediate-operand-p offset)
                  (match-immediate-operand offset (cadr (assoc :imm (cdr spec)))))))
      (and (register-operand-p base)
           (match-register-operand base (cadr (assoc :base (cdr spec))))
           (ecase (car spec)
             ((:mem-scaled :mem-unscaled)
              (and (not pre) (not post)
                   (cond
                     ((null offset) t)  ;[Xn] ≡ [Xn, #0]
                     ((immediate-operand-p offset)
                      (match-immediate-operand offset (cadr (assoc :imm (cdr spec)))))
                     (t nil))))         ;a register offset ⇒ the regoff form
             (:mem-regoff
              (and (not pre) (not post) offset
                   (match-index-operand
                    offset (regoff-scale (cadr (assoc :index (cdr spec)))))))
             (:mem-pre  (and pre (imm-offset-p)))
             (:mem-post (and post (imm-offset-p))))))))

(defun match-operand (operand spec)
  (cond
    ((label-spec-p spec)                ;(:label class) ⇒ branch target
     (label-operand-p operand))         ;reach is checked at finalize
    ((member spec '(:cond :cond-inv))   ;a (:? cc) condition (maybe inverted)
     (condition-operand-p operand))
    ((keywordp spec)                    ;bare keyword ⇒ immediate class
     (and (immediate-operand-p operand)
          (match-immediate-operand operand spec)))
    ((mem-spec-p spec)                  ;(:mem-FORM …) ⇒ memory group
     (and (memory-operand-p operand)
          (match-memory-operand operand spec)))
    ((consp spec)                       ;(role class) ⇒ register
     (and (register-operand-p operand)
          (match-register-operand operand (cadr spec))))
    (t nil)))

(defun match-template (template operands)
  (let ((specs (instruction-template-operand-specs template)))
    (and (= (length specs) (length operands))
         (every #'match-operand operands specs))))


;;; Encoding operands into the instruction word

;;; Encoding works similarly to the way matching does: walk the same
;;; (spec . operand) pairs and fold each operand's bits into the word
;;; at the field according to the spec names.  A matched template means every
;;; operand's range was already checked, so the encoders insert
;;; operand bits with no further ado.  Register field positions live in
;;; insert-rd/rn/rm; the field for an immediate is determined by its
;;; class.

;;; A64's analog of ARM's set-field-value: one 32-bit word, so it's just
;;; dpb-in-place into the insn's word slot.
(defun set-field-value (insn bytespec value)
  (setf (ldb bytespec (instruction-word insn)) value))

(defun insert-rd (insn operand)
  (set-field-value insn (byte 5 0)
                   (register-number (register-operand-register operand))))

(defun insert-rn (insn operand)
  (set-field-value insn (byte 5 5)
                   (register-number (register-operand-register operand))))

(defun insert-rm (insn operand)
  (set-field-value insn (byte 5 16)
                   (register-number (register-operand-register operand))))

(defun insert-ra (insn operand)
  (set-field-value insn (byte 5 10)
                   (register-number (register-operand-register operand))))

;;; Rt2 (the second transfer register of a load/store pair) shares the
;;; 14:10 field with Ra.
(defun insert-rt2 (insn operand)
  (set-field-value insn (byte 5 10)
                   (register-number (register-operand-register operand))))

(defparameter *shift-types*
  #(:lsl :lsr :asr :ror))

(defun encode-shift-type (name)
  (or (position name *shift-types* :test #'eq)
      (error "Unknown shift type name ~s" name)))

(defparameter *extend-options*
  #(:uxtb :uxth :uxtw :uxtx :sxtb :sxth :sxtw :sxtx))

(defun encode-extend-option (name)
  (or (position name *extend-options* :test #'eq)
      (error "Unknown extend option ~s" name)))

;;; A shifted register encodes its shift type in bits 23:22; an extended
;;; register encodes its extend option in bits 15:13.
(defun shift-type-code (op)
  (ecase op (:lsl 0) (:lsr 1) (:asr 2) (:ror 3)))

(defun extend-option-code (op)
  (ecase op (:uxtb 0) (:uxth 1) (:uxtw 2) (:uxtx 3)
            (:sxtb 4) (:sxth 5) (:sxtw 6) (:sxtx 7)))

;;; ROLE places the register number; CLASS says whether to also place shift or
;;; extend fields (the role of a shifted/extended Rm is just :rm, so the class
;;; is what distinguishes the two forms).
(defun encode-register-operand (insn operand role class)
  (ecase role
    ((:rd :rt) (insert-rd insn operand))
    ((:rn :base) (insert-rn insn operand))
    (:rm (insert-rm insn operand))
    (:ra (insert-ra insn operand))
    (:rt2 (insert-rt2 insn operand))
    ;; one source register written into both Rn and Rm (cinc/cinv/cneg)
    (:rn+rm (insert-rn insn operand) (insert-rm insn operand)))
  (let ((modifier (register-operand-modifier operand))
        (amount (register-operand-amount operand)))
    (case class
      ((:x-shift :w-shift :x-shift-ror :w-shift-ror)
       ;; shift type @ 23:22, imm6 amount @ 15:10; a bare register is lsl #0
       (set-field-value insn (byte 2 22) (shift-type-code (or modifier :lsl)))
       (set-field-value insn (byte 6 10) amount))
      ((:x-ext :w-ext)
       ;; option @ 15:13, imm3 amount @ 12:10; a bare register or lsl encodes
       ;; as uxtx (64-bit) / uxtw (32-bit)
       (let ((option (if (or (null modifier) (eq modifier :lsl))
                       (if (eq class :x-ext) 3 2)
                       (extend-option-code modifier))))
         (set-field-value insn (byte 3 13) option)
         (set-field-value insn (byte 3 10) amount))))))

(defun encode-immediate-operand (insn operand class)
  (let ((value (immediate-operand-value operand))
        (shift (immediate-operand-shift operand)))
    (flet ((uoff (scale)                ;scale byte offset by memory access size
             (set-field-value insn (byte 12 10) (ash value (- scale)))))
      (case class
        (:aimm (set-field-value insn (byte 12 10) value)
               (set-field-value insn (byte 1 22) (if (= shift 12) 1 0)))
        (:simm9 (set-field-value insn (byte 9 12) value))
        (:limm (set-field-value insn (byte 13 10) (encode-logical-immediate value)))
        (:uoff0 (uoff 0))
        (:uoff1 (uoff 1))
        (:uoff2 (uoff 2))
        (:uoff3 (uoff 3))
        (:uoff4 (uoff 4))
        (:poff2 (set-field-value insn (byte 7 15) (ash value -2)))
        (:poff3 (set-field-value insn (byte 7 15) (ash value -3)))
        ((:movw-x :movw-w)                ;imm16 @ 20:5, hw (= shift/16) @ 22:21
         (set-field-value insn (byte 16 5) value)
         (set-field-value insn (byte 2 21) (ash shift -4)))
        ((:immr-x :immr-w) (set-field-value insn (byte 6 16) value))
        ((:imms-x :imms-w) (set-field-value insn (byte 6 10) value))
        ((:tbit-x :tbit-w)                ;bit number: b40 @ 23:19, b5 @ 31
         (set-field-value insn (byte 5 19) (ldb (byte 5 0) value))
         (set-field-value insn (byte 1 31) (ldb (byte 1 5) value)))
        (:exc16 (set-field-value insn (byte 16 5) value))
        (:udf16 (set-field-value insn (byte 16 0) value))
        (:baropt (set-field-value insn (byte 4 8) value))
        (:imm5 (set-field-value insn (byte 5 16) value))
        (:nzcv (set-field-value insn (byte 4 0) value))
        ;; immediate shifts encode as bitfield moves: lsl #n has
        ;; immr=(-n) mod width, imms=msb-n; lsr/asr #n have immr=n, imms=msb.
        (:lsl-imm-x (set-field-value insn (byte 6 16) (logand (- value) 63))
                    (set-field-value insn (byte 6 10) (- 63 value)))
        (:lsl-imm-w (set-field-value insn (byte 6 16) (logand (- value) 31))
                    (set-field-value insn (byte 6 10) (- 31 value)))
        ((:lsr-imm-x :asr-imm-x) (set-field-value insn (byte 6 16) value)
                                 (set-field-value insn (byte 6 10) 63))
        ((:lsr-imm-w :asr-imm-w) (set-field-value insn (byte 6 16) value)
                                 (set-field-value insn (byte 6 10) 31))
        (:pcrel                           ;immlo (low 2 bits) @ 30:29, immhi @ 23:5
         (set-field-value insn (byte 2 29) value)
         (set-field-value insn (byte 19 5) (ash value -2)))
        (t (error "encoding of immediate class ~s not implemented" class))))))

(defun encode-index-operand (insn r-op)
  ;; A register-offset index: Rm @ 20:16, the extend option @ 15:13, and
  ;; S @ 12 (set iff the index is scaled, i.e. the amount is nonzero).
  (let* ((reg (register-operand-register r-op))
         (modifier (register-operand-modifier r-op))
         (amount (register-operand-amount r-op)))
    (set-field-value insn (byte 5 16) (register-number reg))
    (set-field-value insn (byte 3 13) (index-option (register-width reg) modifier))
    (set-field-value insn (byte 1 12) (if (zerop amount) 0 1))))

(defun encode-memory-operand (insn operand spec)
  ;; Base → Rn; the offset is encoded per the addressing form: an
  ;; immediate (reusing encode-immediate-operand; a missing offset is #0,
  ;; already in the base-opcode) or a register index.
  (insert-rn insn (memory-operand-base operand))
  (let ((offset (memory-operand-offset operand)))
    (ecase (car spec)
      ;; the pre/post writeback bits @ 11:10 are baked into the base
      ;; opcode, so these encode just like the plain immediate forms.
      ((:mem-scaled :mem-unscaled :mem-pre :mem-post)
       (when offset
         (encode-immediate-operand insn offset (cadr (assoc :imm (cdr spec))))))
      (:mem-regoff
       (encode-index-operand insn offset)))))

(defun encode-label-operand (insn operand class)
  ;; Record a reference from INSN to the named label; finalize patches
  ;; the displacement field once all addresses are known.  CLASS
  ;; (:b26/:b19/:b14) is the reftype that selects the field.  The field
  ;; is left zero (its base-opcode value) until then.
  (note-label-reference (label-operand-name operand) insn class))

(defun encode-condition-operand (insn operand &optional invert)
  ;; The condition is a 4-bit field @ 15:12 in the conditional-select and
  ;; conditional-compare instructions (not the 3:0 spot b.cond uses).  The
  ;; cset/cinc/... aliases encode the inverse condition; al/nv have no
  ;; inverse (their low bit isn't a negation), so inverting them is an error.
  (let ((value (condition-operand-value operand)))
    (when invert
      (if (< value 14)
        (setq value (logxor value 1))
        (error "condition ~s has no inverse" (condition-operand-name operand))))
    (set-field-value insn (byte 4 12) value)))

(defun encode-operand (insn operand spec)   ;like match-operand
  (cond
    ((label-spec-p spec) (encode-label-operand insn operand (cadr spec)))
    ((eq spec :cond) (encode-condition-operand insn operand))
    ((eq spec :cond-inv) (encode-condition-operand insn operand t))
    ((keywordp spec) (encode-immediate-operand insn operand spec))
    ((mem-spec-p spec) (encode-memory-operand insn operand spec))
    ((consp spec) (encode-register-operand insn operand (car spec) (cadr spec)))))

(defun encode-operands (insn)
  (let ((template (instruction-template insn)))
    (setf (instruction-word insn) (instruction-template-base-opcode template))
    (loop for spec in (instruction-template-operand-specs template)
          for operand in (instruction-parsed-operands insn)
          do (encode-operand insn operand spec))))

;;; Rendering operand specs in human-readable, GAS-ish form.  Used now for
;;; "no match" diagnostics; reusable later for disassembly/documentation.

(defparameter *memory-specs*
  '(:mem-scaled :mem-unscaled :mem-regoff :mem-pre :mem-post))

(defun mem-spec-p (spec)
  (and (consp spec)
       (member (car spec) *memory-specs* :test #'eq)))

(defun label-spec-p (spec)
  ;; (:label class), where class is one of :b26/:b19/:b14.
  (and (consp spec)
       (eq (car spec) :label)))

(defun render-gpr-token (class suffix)
  (case class
    (:x       (format nil "X~a" suffix))
    (:w       (format nil "W~a" suffix))
    (:x/sp    (format nil "X~a|SP" suffix))
    (:w/sp    (format nil "W~a|WSP" suffix))
    (:wsp     "WSP")
    (:sp      "SP")
    ((:x-shift :x-shift-ror) (format nil "X~a{, shift #amt}" suffix))
    ((:w-shift :w-shift-ror) (format nil "W~a{, shift #amt}" suffix))
    (:x-ext   (format nil "X~a{, extend #amt}" suffix))
    (:w-ext   (format nil "W~a{, extend #amt}" suffix))
    (t (format nil "~(~a~)" class))))

(defun render-register-spec (spec)
  ;; SPEC is (role class), e.g. (:rd :x).  The role's trailing letter
  ;; (d/n/m/t) becomes the operand suffix.
  (render-gpr-token (cadr spec)
                    (subseq (string-downcase (string (car spec))) 1)))

(defun render-immediate-spec (class)
  (case class
    (:aimm   "#imm{, LSL #12}")
    (:limm   "#bitmask")
    (:simm9  "#simm9")
    ((:movw-x :movw-w) "#imm16{, LSL #shift}")
    ((:immr-x :immr-w) "#immr")
    ((:imms-x :imms-w) "#imms")
    ((:tbit-x :tbit-w) "#bit")
    ((:exc16 :udf16) "#imm16")
    (:baropt "#option")
    (:imm5 "#imm5")
    (:nzcv "#nzcv")
    ((:lsl-imm-x :lsl-imm-w :lsr-imm-x :lsr-imm-w :asr-imm-x :asr-imm-w) "#shift")
    (:pcrel "label")
    ((:uoff0 :uoff1 :uoff2 :uoff3 :uoff4 :poff2 :poff3) "#off")
    (t (format nil "#~(~a~)" class))))

(defun render-mem-spec (spec)
  ;; SPEC is (:mem-FORM (:base qual) (:imm qual) ...).
  (let (base off)
    (dolist (component (cdr spec))
      (case (car component)
        (:base (setq base (render-gpr-token (cadr component) "n")))
        (:imm  (setq off (render-immediate-spec (cadr component))))
        (:index (setq off "Xm|Wm{, extend #amt}"))))
    (setq base (or base "Xn|SP"))
    (case (car spec)
      (:mem-pre  (format nil "[~a, ~a]!" base off))
      (:mem-post (format nil "[~a], ~a" base off))
      (t (format nil "[~a~@[{, ~a}~]]" base off)))))

(defun render-operand-spec (spec)
  (cond
    ((label-spec-p spec) "label")
    ((member spec '(:cond :cond-inv)) "(:? cc)")
    ((keywordp spec)   (render-immediate-spec spec))
    ((mem-spec-p spec) (render-mem-spec spec))
    ((consp spec)      (render-register-spec spec))
    (t (princ-to-string spec))))

(defun render-template-operand-specs (template)
  (let ((specs (instruction-template-operand-specs template)))
    (if specs
      (format nil "~{~a~^, ~}" (mapcar #'render-operand-spec specs))
      "")))

(defun template-arity (template)
  (length (instruction-template-operand-specs template)))

(defun no-match-forms (lname templates)
  ;; The mnemonic's accepted forms, rendered and deduplicated, one per line.
  (mapcar (lambda (form)
            (if (string= form "") lname (format nil "~a ~a" lname form)))
          (remove-duplicates (mapcar #'render-template-operand-specs templates)
                             :test #'string=)))

(defun spec-expected-kind (spec)
  (cond ((label-spec-p spec) :label)
        ((member spec '(:cond :cond-inv)) :condition)
        ((keywordp spec)   :immediate)
        ((mem-spec-p spec) :memory)
        ((consp spec)      :register)))

(defun operand-kind (operand)
  (cond ((register-operand-p operand)  :register)
        ((immediate-operand-p operand) :immediate)
        ((memory-operand-p operand)    :memory)
        ((label-operand-p operand)     :label)
        ((condition-operand-p operand) :condition)))

;;; Invoked only when no template matched.  Always signals an error, sharpest
;;; diagnosis first: an arity mismatch, then an operand whose KIND fits no
;;; candidate at its position (this stays correct even while the detail
;;; predicates are stubs, since it only compares register/immediate/memory),
;;; otherwise a listing of the forms the mnemonic accepts.
(defun explain-no-match (name opvals operands templates)
  (let* ((lname (string-downcase (string name)))
         (arity (length opvals))
         (arities (sort (remove-duplicates (mapcar #'template-arity templates)) #'<)))
    (unless (member arity arities)
      (error "~a takes ~{~a~^ or ~} operand(s), but got ~d: ~s"
             lname arities arity (cons name opvals)))
    (let ((right-arity (remove arity templates :key #'template-arity :test-not #'=)))
      (loop for i from 0
            for opval in opvals
            for operand in operands
            for kind = (operand-kind operand)
            unless (some (lambda (template)
                           (eq kind (spec-expected-kind
                                     (nth i (instruction-template-operand-specs template)))))
                         right-arity)
              do (error "~a: operand ~d (~s) should be ~{~(~a~)~^ or ~}.~%~a accepts:~%~{  ~a~%~}"
                        lname (1+ i) opval
                        (remove-duplicates
                         (mapcar (lambda (template)
                                   (spec-expected-kind
                                    (nth i (instruction-template-operand-specs template))))
                                 right-arity))
                        lname (no-match-forms lname right-arity)))
      (error "~a: ~s matches no form.~%~a accepts:~%~{  ~a~%~}"
             lname (cons name opvals) lname (no-match-forms lname right-arity)))))



(defstruct (instruction-element (:include ccl::dll-node))
  address
  (size 0))

;;; An instruction in the process of being assembled
(defstruct (instruction (:include instruction-element (size 4))
                        (:constructor %make-instruction (source)))
  source                             ;the lap form
  template                           ;the matched instruction-template
  (word 0 :type (unsigned-byte 32))  ;encoded instruction word
  parsed-operands)

(defstruct (label (:include instruction-element)
                  (:constructor %%make-label (name)))
  name
  refs)

;;; Labels and branch fixups.
;;;
;;; Labels are zero-size elements spliced into the same doubly-linked
;;; section as instructions; a label therefore inherits the address of
;;; whatever instruction follows it.  Assembly is two-pass: pass one
;;; emits elements and, for each branch, records a (insn . reftype)
;;; reference on the target label without resolving it; FINALIZE is pass
;;; two, computing label addresses and patching branch displacements.
;;;
;;; Unlike the ARM32 port there is no constant-pool drain: arm64 reaches
;;; lisp constants through the fn register, not PC-relative, so the only
;;; thing FINALIZE resolves is branch reach.  And unlike ARM there is no
;;; +8 PC bias: on A64 the PC reads as the branch instruction's own
;;; address.

(defvar *lap-labels* ()
  "The labels of the function currently being assembled: an alist keyed
by name that auto-promotes to a hash-table past 255 entries.")

(defun emit-element (element seg)
  (ccl::append-dll-node element seg)
  element)

(defun section-size (seg)
  (let ((last (ccl::dll-header-last seg)))
    (if (eq last seg)                   ;empty
      0
      (+ (instruction-element-address last)
         (instruction-element-size last)))))

(defun set-element-addresses (seg)
  ;; One non-iterative pass: lay out elements at successive addresses.
  ;; Labels have size 0, so each takes the address of the next real
  ;; instruction.
  (let ((address 0))
    (ccl::do-dll-nodes (element seg)
      (setf (instruction-element-address element) address)
      (incf address (instruction-element-size element)))))

;;; A label can only be emitted once.  Until it is, its pred slot is nil.
(defun label-emitted-p (lab)
  (not (null (label-pred lab))))

(defun make-label (name)
  (let ((lab (%%make-label name)))
    (if (typep *lap-labels* 'hash-table)
      (setf (gethash name *lap-labels*) lab)
      (progn
        (push lab *lap-labels*)
        (when (> (length *lap-labels*) 255)
          (let ((hash (make-hash-table :size 512 :test #'eq)))
            (dolist (l *lap-labels*)
              (setf (gethash (label-name l) hash) l))
            (setq *lap-labels* hash)))))
    lab))

(defun find-label (name)
  (if (typep *lap-labels* 'hash-table)
    (gethash name *lap-labels*)
    (car (member name *lap-labels* :test #'eq :key #'label-name))))

(defun note-label-reference (name insn reftype)
  (let ((lab (or (find-label name)
                 (make-label name))))
    (push (cons insn reftype) (label-refs lab))
    lab))

(defun emit-label (seg name)
  (let ((lab (find-label name)))
    (if lab
      (when (label-emitted-p lab)
        (error "Label ~s: multiply defined." name))
      (setq lab (make-label name)))
    (emit-element lab seg)))

(defmacro do-lap-labels ((lab &optional result) &body body)
  (let ((thunk (gensym))
        (k (gensym))
        (xlab (gensym)))
    `(flet ((,thunk (,lab) ,@body))
       (if (typep *lap-labels* 'hash-table)
         (maphash (lambda (,k ,xlab)
                    (declare (ignore ,k))
                    (,thunk ,xlab))
                  *lap-labels*)
         (dolist (,xlab *lap-labels*)
           (,thunk ,xlab)))
       ,result)))

;;; The branch field for each reftype: (bytespec . signed-width).
(defparameter *branch-fields*
  '((:b26 . #.(cons (byte 26 0) 26))
    (:b19 . #.(cons (byte 19 5) 19))
    (:b14 . #.(cons (byte 14 5) 14))))

(defun set-branch-displacement (insn reftype words)
  ;; WORDS is the target displacement in instructions (bytes / 4).
  (destructuring-bind (bytespec . width)
      (or (cdr (assoc reftype *branch-fields*))
          (error "Unknown branch reftype ~s." reftype))
    (unless (typep words (list 'signed-byte width))
      (error "Branch target out of range: ~d words won't fit in ~
              a signed ~d-bit field (~s)." words width reftype))
    (set-field-value insn bytespec words)))

(defun finalize (seg)
  ;; Assign addresses, then patch every branch's displacement field.
  ;; One shot: no outliers, no re-addressing, no iteration.
  (set-element-addresses seg)
  (do-lap-labels (lab)
    (if (label-emitted-p lab)
      (let ((labaddr (instruction-element-address lab)))
        (dolist (ref (label-refs lab))
          (destructuring-bind (insn . reftype) ref
            (let ((words (ash (- labaddr (instruction-element-address insn))
                              -2)))
              (set-branch-displacement insn reftype words)))))
      (when (label-refs lab)
        (error "LAP label ~s was referenced but not defined."
               (label-name lab)))))
  (ash (section-size seg) -2))

;;; Bring-up driver: assemble a list of LAP forms (instructions, and
;;; bare symbols standing for label definitions) into a fresh section,
;;; resolve branch labels, and return the section.
(defun assemble-section (forms)
  (let ((seg (ccl::make-dll-header))
        (*lap-labels* ()))
    (dolist (form forms)
      (if (symbolp form)
        (emit-label seg form)
        (emit-element (assemble seg form) seg)))
    (finalize seg)
    seg))

(defun section-words (seg)
  ;; The encoded 32-bit words of SEG's instructions, in order.
  (let ((words '()))
    (ccl::do-dll-nodes (element seg)
      (when (instruction-p element)
        (push (instruction-word element) words)))
    (nreverse words)))


(progn
  ;; Sanity-check the hand-entered template table.  These checks need
  ;; only base-opcode + mask + the operand classes — NOT operand bit
  ;; positions — so they cost nothing to maintain as the table grows.
  ;; Round-trip encode/decode tests (once the disassembler exists)
  ;; cover the rest.

  (defun template-alias-p (template)
    (logtest (instruction-template-flags template)
             (%encode-instruction-flags :alias)))

  (defun operand-spec-classes (spec)
    ;; The class keyword(s) a spec refers to: a bare immediate is
    ;; itself a class; a register spec's class is its cadr; a memory
    ;; group's components each carry one.
    (cond ((keywordp spec) (list spec))
          ((mem-spec-p spec)
           (loop for component in (cdr spec)
                 when (consp component) collect (cadr component)))
          ((consp spec) (list (cadr spec)))
          (t nil)))

  (defun validate-template (template)
    "Return a list of human-readable problem strings for TEMPLATE."
    (let ((name (instruction-template-name template))
          (base (instruction-template-base-opcode template))
          (mask (instruction-template-mask template))
          (specs (instruction-template-operand-specs template))
          (problems '()))
      (flet ((problem (fmt &rest args)
               (push (format nil "~a: ~?" name fmt args) problems)))
        (unless (typep base '(unsigned-byte 32))
          (problem "base-opcode ~x is not a 32-bit value" base))
        (unless (typep mask '(unsigned-byte 32))
          (problem "mask ~x is not a 32-bit value" mask))
        ;; Mask checks apply only to real instructions; aliases are
        ;; encoder-only and excluded from disassembly, so their
        ;; (conventionally 0) mask is of no interest.
        (when (and (typep base '(unsigned-byte 32))
                   (typep mask '(unsigned-byte 32))
                   (not (template-alias-p template)))
          (if (zerop mask)
            (problem "non-alias template has a zero mask")
            (let ((stray (logandc2 base mask)))
              (unless (zerop stray)
                (problem "base-opcode ~x sets bits ~x outside its mask ~x ~
                        (i.e. inside an operand field)" base stray mask)))))
        ;; Every operand class must be one we know how to encode.
        (dolist (spec specs)
          (dolist (class (operand-spec-classes spec))
            (unless (member class *operand-classes*)
              (problem "unknown operand class ~s in spec ~s" class spec))))
        (nreverse problems))))

  (defun validate-templates (&optional (errorp t))
    "Check every template.  With ERRORP (the default) signal an error
  listing all problems; otherwise warn.  Returns T when the table is clean."
    (let ((problems '()))
      (dotimes (i (length *instruction-templates*))
        (setf problems
              (nconc problems (validate-template
                               (svref *instruction-templates* i)))))
      (cond
        (problems
         (funcall (if errorp #'error #'warn)
                  "~d instruction-template problem~:p:~%~{  ~a~%~}"
                  (length problems) problems)
         nil)
        (t
         (format t "~&~d instruction templates validated.~%"
                 (length *instruction-templates*))
         t))))

  (validate-templates)
  ) ;progn

(defun count-trailing-zeros-64 (u64)
  (do* ((i 0 (1+ i)))
       ((or (= i 64) (logbitp i u64))
        i)
    (declare (fixnum i))))

(defun count-leading-zeros-64 (u64)
  (do* ((count 0 (1+ count))
        (i 63 (1- i)))
       ((or (= count 64) (logbitp i u64))
        count)
    (declare (fixnum count i))))

(defun count-leading-zeros-32 (u32)
  (do* ((count 0 (1+ count))
        (i 31 (1- i)))
       ((or (= count 32) (logbitp i u32))
        count)
    (declare (fixnum count i))))

(defun clear-trailing-ones-64 (u64)
  (ldb (byte 64 0) (logand u64 (1+ u64))))

(defun rotate-right-64 (u64 n)
  (let* ((right (logand n 63))
         (left (logand (- n) 63)))
    (logior (ldb (byte 64 0) (ash u64 (- right)))
            (ldb (byte 64 0) (ash u64 left)))))

;;; Adapted from https://dougallj.wordpress.com/2021/10/30/bit-twiddling-optimising-aarch64-logical-immediate-encoding-and-decoding/

(defun %encode-logical-immediate (u64)
  ;; Consider an ARM64 logical immediate as a pattern of "o" ones preceded
  ;; by "z" more-significant zeroes, repeated to fill a 64-bit integer.
  ;; o > 0, z > 0, and the size (o + z) is a power of two in [2,64]. This
  ;; part of the pattern is encoded in the fields "imms" and "N".
  ;;
  ;; "immr" encodes a further right rotate of the repeated pattern, allowing
  ;; a wide range of useful bitwise constants to be represented.
  ;;
  ;; (The spec describes the "immr" rotate as rotating the "o + z" bit
  ;; pattern before repeating it to fill 64-bits, but, as it's a repeating
  ;; pattern, rotating afterwards is equivalent.)
  ;;
  ;; This encoding is not allowed to represent all-zero or all-one values,
  ;; which must have been excluded prior to calling this function,
  ;;
  ;; To detect an immediate that may be encoded in this scheme, we first
  ;; remove the right-rotate, by rotating such that the least significant
  ;; bit is a one and the most significant bit is a zero.
  ;;
  ;; We do this by clearing any trailing one bits, then counting the
  ;; trailing zeroes. This finds an "edge", where zero goes to one.
  ;; We then rotate the original value right by that amount, moving
  ;; the first one to the least significant bit.
  (let* ((rotation (count-trailing-zeros-64 (clear-trailing-ones-64 u64)))
         (normalized (rotate-right-64 u64 (logand rotation 63)))
         ;; Now we have normalized the value, and determined the
         ;; rotation, we can determine "z" by counting the leading
         ;; zeroes, and "o" by counting the trailing ones. (These will
         ;; both be positive, as we already rejected 0 and ~0, and
         ;; rotated the value to start with a zero and end with a
         ;; one.)
         (zeros (count-leading-zeros-64 normalized))
         (ones (count-trailing-zeros-64 (ldb (byte 64 0) (lognot normalized))))
         (size (+ zeros ones)))
    ;; Detect the repeating pattern (by comparing every repetition to the
    ;; one next to it, using rotate).
    (if (/= (rotate-right-64 u64 (logand size 63)) u64)
      nil
      ;; We do not need to further validate size to ensure it is a
      ;; power of two between 2 and 64. The only "minimal" patterns
      ;; that can repeat to fill a 64-bit value must have a length
      ;; that is a factor of 64 (i.e. it is a power of two in the
      ;; range [1,64]). And our pattern cannot be of length one (as we
      ;; already rejected 0 and ~0).
      ;;
      ;; By "minimal" patterns I refer to patterns which do not
      ;; themselves contain repetitions. For example, '010101' is a
      ;; non-minimal pattern of a non-power-of-two length that can
      ;; pass the above rotational test. It consists of the minimal
      ;; pattern '01'. All our patterns are minimal, as they contain
      ;; only one contiguous run of ones separated by at least one
      ;; zero.
      ;;
      ;; Finally, we encode the values. "rotation" is the amount we
      ;; rotated right by to "undo" the right-rotate encoded in immr,
      ;; so must be negated.
      ;;
      ;; size 2:  N=0 immr=00000r imms=11110s
      ;; size 4:  N=0 immr=0000rr imms=1110ss
      ;; size 8:  N=0 immr=000rrr imms=110sss
      ;; size 16: N=0 immr=00rrrr imms=10ssss
      ;; size 32: N=0 immr=0rrrrr imms=0sssss
      ;; size 64: N=1 immr=rrrrrr imms=ssssss
      (let* ((immr (logand (- rotation) (1- size)))
             (imms (logior (- (ash size 1))
                           (1- ones)))
             (n (ash size (- 6))))
        (logior (ash n 12) (ash immr 6) (ldb (byte 6 0) imms))))))

(defun encode-logical-immediate (n)
  "Return a 13 bit encoding of n, or NIL if it can't be encoded."
  (let* ((u64 (ldb (byte 64 0) n))
         (u64-inverted (ldb (byte 64 0) (lognot u64))))
    (if (or (/= n u64)                  ;n too big
            (zerop u64)                 ;can't encode all zeros...
            (zerop u64-inverted))       ;...or all ones
      nil
      (%encode-logical-immediate u64))))

;;; Form of an encoded logical immediate:
;;;
;;;      1
;;;  2 1 0 9 8 7 6 5 4 3 2 1 0
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; |N|   immr    |    imms   |
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+


(defconstant mask-lookup
  #(#xffffffffffffffff                  ;size = 64
    #x00000000ffffffff                  ;size = 32
    #x0000ffff0000ffff                  ;size = 16
    #x00ff00ff00ff00ff                  ;size = 8
    #x0f0f0f0f0f0f0f0f                  ;size = 4
    #x3333333333333333))                ;size = 2

(defun decode-logical-immediate (imm)
  (let* ((n (ldb (byte 1 12) imm))
         (immr (ldb (byte 6 6) imm))
         (imms (ldb (byte 6 0) imm))
         (pattern (logior (ash n 6) (logand (lognot imms) #x3f))))
    (if (zerop (logand pattern (1- pattern)))
      nil
      (let* ((leading-zeros (count-leading-zeros-32 pattern))
             (imms-mask (ash #x7fffffff (- leading-zeros)))
             (mask (aref mask-lookup (- leading-zeros 25)))
             (s (logand (1+ imms) imms-mask)))
        (rotate-right-64 (logxor mask (ash mask s)) immr)))))




(defparameter *junk*
'(
  ("adc" #x1a000000 #xffe0fc00 :addsub-carry 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) 0)
  ("adc" #x9a000000 #xffe0fc00 :addsub-carry 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) 0)
  ("adcs" #x3a000000 #xffe0fc00 :addsub-carry 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w)0)
  ("adcs" #xba000000 #xffe0fc00 :addsub-carry 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x)0)
  ("sbc" #x5a000000 #xffe0fc00 :addsub-carry 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-HAS-ALIAS)
  ("sbc" #xda000000 #xffe0fc00 :addsub-carry 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-HAS-ALIAS)
  ("ngc" #x5a0003e0 #xffe0ffe0 :addsub-carry 0 :CORE '(:Rd :Rm) '(:w :w) F-ALIAS)
  ("ngc" #xda0003e0 #xffe0ffe0 :addsub-carry 0 :CORE '(:Rd :Rm) '(:x :x) F-ALIAS)
  ("sbcs" #x7a000000 #xffe0fc00 :addsub-carry 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-HAS-ALIAS)
  ("sbcs" #xfa000000 #xffe0fc00 :addsub-carry 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-HAS-ALIAS)

  ("ngcs" #x7a0003e0 #xffe0ffe0 :addsub-carry 0 :CORE '(:Rd :Rm) '(:w :w) F-ALIAS)
  ("ngcs" #xfa0003e0 #xffe0ffe0 :addsub-carry 0 :CORE '(:Rd :Rm) '(:x :x) F-ALIAS)
  ("add" #x0b200000 #x7fe00000 :addsub-ext 0 :CORE '(:Rd-SP :Rn-SP :Rm-EXT) QL-I3-EXT F-SF)
  ("adds" #x2b200000 #x7fe00000 :addsub-ext 0 :CORE '(:Rd :Rn-SP :Rm-EXT) QL-I3-EXT (F-HAS-ALIAS  F-SF))
  ("cmn" #x2b20001f #x7fe0001f :addsub-ext 0 :CORE '(:Rn-SP :Rm-EXT) QL-I2-EXT (F-ALIAS  F-SF))
  ("sub" #x4b200000 #x7fe00000 :addsub-ext 0 :CORE '(:Rd-SP :Rn-SP :Rm-EXT) QL-I3-EXT F-SF)
  ("subs" #x6b200000 #x7fe00000 :addsub-ext 0 :CORE '(:Rd :Rn-SP :Rm-EXT) QL-I3-EXT (F-HAS-ALIAS  F-SF))
  ("cmp" #x6b20001f #x7fe0001f :addsub-ext 0 :CORE '(:Rn-SP :Rm-EXT) QL-I2-EXT (F-ALIAS  F-SF))
  ("add" #x11000000 #xff000000 :addsub-imm OP-ADD :CORE '(:Rd-SP :Rn-SP :AIMM) '(:w :w :aimm) F-HAS-ALIAS)
  ("add" #x91000000 #xff000000 :addsub-imm OP-ADD :CORE '(:Rd-SP :Rn-SP :AIMM) '(:x :x :aimm) F-HAS-ALIAS)
  ("mov" #x11000000 #x7ffffc00 :addsub-imm 0 :CORE '(:Rd-SP :Rn-SP) QL-I2SP (F-ALIAS  F-SF))
  ("adds" #x31000000 #xff000000 :addsub-imm 0 :CORE '(:Rd :Rn-SP :AIMM) '(:w :w :aimm) F-HAS-ALIAS)
  ("adds" #xb1000000 #xff000000 :addsub-imm 0 :CORE '(:Rd :Rn-SP :AIMM) '(:x :x :aimm) F-HAS-ALIAS)
  ("cmn" #x3100001f #x7f00001f :addsub-imm 0 :CORE '(:Rn-SP :AIMM) QL-R1NIL (F-ALIAS  F-SF))
  ("sub" #x51000000 #xff000000 :addsub-imm 0 :CORE '(:Rd-SP :Rn-SP :AIMM) '(:w :w :aimm) 0)
  ("sub" #xd1000000 #xff000000 :addsub-imm 0 :CORE '(:Rd-SP :Rn-SP :AIMM) '(:x :x :aimm) 0)
  ("subs" #x71000000 #xff000000 :addsub-imm 0 :CORE '(:Rd :Rn-SP :AIMM) '(:w :w :aimm) F-HAS-ALIAS)
  ("subs" #xf1000000 #xff000000 :addsub-imm 0 :CORE '(:Rd :Rn-SP :AIMM) '(:x :x :aimm) -HAS-ALIAS)
  ("cmp" #x7100001f #x7f00001f :addsub-imm 0 :CORE '(:Rn-SP :AIMM) QL-R1NIL (F-ALIAS  F-SF))
  ("add" #xb000000 #xff200000 :addsub-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) 0)
  ("add" #x8b000000 #xff200000 :addsub-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) 0)
  ("adds" #x2b000000 #xff200000 :addsub-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) F-HAS-ALIAS)
  ("adds" #xab000000 #xff200000 :addsub-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) F-HAS-ALIAS)
  ("cmn" #x2b00001f #x7f20001f :addsub-shift 0 :CORE '(:Rn :Rm-SFT) QL-I2SAME (F-ALIAS  F-SF))
  ("sub" #x4b000000 #xff200000 :addsub-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) F-HAS-ALIAS)
  ("sub" #xcb000000 #xff200000 :addsub-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) F-HAS-ALIAS)

  ("neg" #x4b0003e0 #x7f2003e0 :addsub-shift 0 :CORE '(:Rd :Rm-SFT) QL-I2SAME (F-ALIAS  F-SF))
  ("subs" #x6b000000 #xff200000 :addsub-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) F-HAS-ALIAS)
  ("subs" #xeb000000 #xff200000 :addsub-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift F-HAS-ALIAS)
   ("cmp" #x6b00001f #x7f20001f :addsub-shift 0 :CORE '(:Rn :Rm-SFT) QL-I2SAME (F-ALIAS  F-SF))
   ("negs" #x6b0003e0 #x7f2003e0 :addsub-shift 0 :CORE '(:Rd :Rm-SFT) QL-I2SAME (F-ALIAS  F-SF))
   ("saddlv" #xe303800 #xbf3ffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES-L F-SIZEQ)
   ("smaxv" #xe30a800 #xbf3ffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES F-SIZEQ)
   ("sminv" #xe31a800 #xbf3ffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES F-SIZEQ)
   ("addv" #xe31b800 #xbf3ffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES F-SIZEQ)
   ("uaddlv" #x2e303800 #xbf3ffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES-L F-SIZEQ)
   ("umaxv" #x2e30a800 #xbf3ffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES F-SIZEQ)
   ("uminv" #x2e31a800 #xbf3ffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES F-SIZEQ)
   ("fmaxnmv" #x2e30c800 #xbfbffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES-FP F-SIZEQ)
   ("fmaxv" #x2e30f800 #xbfbffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES-FP F-SIZEQ)
   ("fminnmv" #x2eb0c800 #xbfbffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES-FP F-SIZEQ)
   ("fminv" #x2eb0f800 #xbfbffc00 :asimdall 0 SIMD '(:Fd :Vn) QL-XLANES-FP F-SIZEQ)
   ("saddl" #x0e200000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("saddl2" #x4e200000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("saddw" #x0e201000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3WIDEBHS F-SIZEQ)
   ("saddw2" #x4e201000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3WIDEBHS2 F-SIZEQ)
   ("ssubl" #x0e202000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("ssubl2" #x4e202000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("ssubw" #x0e203000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3WIDEBHS F-SIZEQ)
   ("ssubw2" #x4e203000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3WIDEBHS2 F-SIZEQ)
   ("addhn" #x0e204000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3NARRBHS F-SIZEQ)
   ("addhn2" #x4e204000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3NARRBHS2 F-SIZEQ)
   ("sabal" #x0e205000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("sabal2" #x4e205000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("subhn" #x0e206000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3NARRBHS F-SIZEQ)
   ("subhn2" #x4e206000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3NARRBHS2 F-SIZEQ)
   ("sabdl" #x0e207000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("sabdl2" #x4e207000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("smlal" #x0e208000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("smlal2" #x4e208000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("sqdmlal" #x0e209000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGHS F-SIZEQ)
   ("sqdmlal2" #x4e209000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGHS2 F-SIZEQ)
   ("smlsl" #x0e20a000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("smlsl2" #x4e20a000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("sqdmlsl" #x0e20b000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGHS F-SIZEQ)
   ("sqdmlsl2" #x4e20b000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGHS2 F-SIZEQ)
   ("smull" #x0e20c000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("smull2" #x4e20c000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("sqdmull" #x0e20d000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGHS F-SIZEQ)
   ("sqdmull2" #x4e20d000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGHS2 F-SIZEQ)
   ("pmull" #x0e20e000 #xffe0fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGB 0)
   ("pmull" #x0ee0e000 #xffe0fc00 :asimddiff 0 :CRYPTO '(:Vd :Vn :Vm) QL-V3LONGD 0)
   ("pmull2" #x4e20e000 #xffe0fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGB2 0)
   ("pmull2" #x4ee0e000 #xffe0fc00 :asimddiff 0 :CRYPTO '(:Vd :Vn :Vm) QL-V3LONGD2 0)
   ("uaddl" #x2e200000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("uaddl2" #x6e200000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("uaddw" #x2e201000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3WIDEBHS F-SIZEQ)
   ("uaddw2" #x6e201000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3WIDEBHS2 F-SIZEQ)
   ("usubl" #x2e202000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("usubl2" #x6e202000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("usubw" #x2e203000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3WIDEBHS F-SIZEQ)
   ("usubw2" #x6e203000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3WIDEBHS2 F-SIZEQ)
   ("raddhn" #x2e204000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3NARRBHS F-SIZEQ)
   ("raddhn2" #x6e204000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3NARRBHS2 F-SIZEQ)
   ("uabal" #x2e205000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("uabal2" #x6e205000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("rsubhn" #x2e206000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3NARRBHS F-SIZEQ)
   ("rsubhn2" #x6e206000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3NARRBHS2 F-SIZEQ)
   ("uabdl" #x2e207000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("uabdl2" #x6e207000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("umlal" #x2e208000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("umlal2" #x6e208000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("umlsl" #x2e20a000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("umlsl2" #x6e20a000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("umull" #x2e20c000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS F-SIZEQ)
   ("umull2" #x6e20c000 #xff20fc00 :asimddiff 0 SIMD '(:Vd :Vn :Vm) QL-V3LONGBHS2 F-SIZEQ)
   ("smlal" #x0f002000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L F-SIZEQ)
   ("smlal2" #x4f002000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L2 F-SIZEQ)
   ("sqdmlal" #x0f003000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L F-SIZEQ)
   ("sqdmlal2" #x4f003000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L2 F-SIZEQ)
   ("smlsl" #x0f006000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L F-SIZEQ)
   ("smlsl2" #x4f006000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L2 F-SIZEQ)
   ("sqdmlsl" #x0f007000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L F-SIZEQ)
   ("sqdmlsl2" #x4f007000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L2 F-SIZEQ)
   ("mul" #xf008000 #xbf00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT F-SIZEQ)
   ("smull" #x0f00a000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L F-SIZEQ)
   ("smull2" #x4f00a000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L2 F-SIZEQ)
   ("sqdmull" #x0f00b000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L F-SIZEQ)
   ("sqdmull2" #x4f00b000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L2 F-SIZEQ)
   ("sqdmulh" #xf00c000 #xbf00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT F-SIZEQ)
   ("sqrdmulh" #xf00d000 #xbf00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT F-SIZEQ)
   ("fmla" #xf801000 #xbf80f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-FP F-SIZEQ)
   ("fmls" #xf805000 #xbf80f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-FP F-SIZEQ)
   ("fmul" #xf809000 #xbf80f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-FP F-SIZEQ)
   ("mla" #x2f000000 #xbf00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT F-SIZEQ)
   ("umlal" #x2f002000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L F-SIZEQ)
   ("umlal2" #x6f002000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L2 F-SIZEQ)
   ("mls" #x2f004000 #xbf00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT F-SIZEQ)
   ("umlsl" #x2f006000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L F-SIZEQ)
   ("umlsl2" #x6f006000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L2 F-SIZEQ)
   ("umull" #x2f00a000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L F-SIZEQ)
   ("umull2" #x6f00a000 #xff00f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-L2 F-SIZEQ)
   ("fmulx" #x2f809000 #xbf80f400 :asimdelem 0 SIMD '(:Vd :Vn :Em) QL-ELEMENT-FP F-SIZEQ)
   ("ext" #x2e000000 #xbfe0c400 :asimdext 0 SIMD '(:Vd :Vn :Vm :IDX) QL-VEXT F-SIZEQ)
   ("movi" #xf000400 #xbff89c00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S0W F-SIZEQ)
   ("orr" #xf001400 #xbff89c00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S0W F-SIZEQ)
   ("movi" #xf008400 #xbff8dc00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S0H F-SIZEQ)
   ("orr" #xf009400 #xbff8dc00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S0H F-SIZEQ)
   ("movi" #xf00c400 #xbff8ec00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S1W F-SIZEQ)
   ("movi" #xf00e400 #xbff8fc00 :asimdimm OP-V-MOVI-B SIMD '(:Vd :SIMD-IMM) QL-SIMD-IMM-B F-SIZEQ)
   ("fmov" #xf00f400 #xbff8fc00 :asimdimm 0 SIMD '(:Vd :SIMD-FPIMM) QL-SIMD-IMM-S F-SIZEQ)
   ("mvni" #x2f000400 #xbff89c00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S0W F-SIZEQ)
   ("bic" #x2f001400 #xbff89c00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S0W F-SIZEQ)
   ("mvni" #x2f008400 #xbff8dc00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S0H F-SIZEQ)
   ("bic" #x2f009400 #xbff8dc00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S0H F-SIZEQ)
   ("mvni" #x2f00c400 #xbff8ec00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM-SFT) QL-SIMD-IMM-S1W F-SIZEQ)
   ("movi" #x2f00e400 #xfff8fc00 :asimdimm 0 SIMD '(:Sd :SIMD-IMM) QL-SIMD-IMM-D F-SIZEQ)
   ("movi" #x6f00e400 #xfff8fc00 :asimdimm 0 SIMD '(:Vd :SIMD-IMM) QL-SIMD-IMM-V2D F-SIZEQ)
   ("fmov" #x6f00f400 #xfff8fc00 :asimdimm 0 SIMD '(:Vd :SIMD-FPIMM) QL-SIMD-IMM-V2D F-SIZEQ)
   ("dup" #xe000400 #xbfe0fc00 :asimdins 0 SIMD '(:Vd :En) QL-DUP-VX F-T)
   ("dup" #xe000c00 #xbfe0fc00 :asimdins 0 SIMD '(:Vd :Rn) QL-DUP-VR F-T)
   ("smov" #xe002c00 #xbfe0fc00 :asimdins 0 SIMD '(:Rd :En) QL-SMOV F-GPRSIZE-IN-Q)
   ("umov" #xe003c00 #xbfe0fc00 :asimdins 0 SIMD '(:Rd :En) QL-UMOV (F-HAS-ALIAS  F-GPRSIZE-IN-Q))
   ("mov" #xe003c00 #xbfe0fc00 :asimdins 0 SIMD '(:Rd :En) QL-MOV (F-ALIAS  F-GPRSIZE-IN-Q))
   ("ins" #x4e001c00 #xffe0fc00 :asimdins 0 SIMD '(:Ed :Rn) QL-INS-XR F-HAS-ALIAS)
   ("mov" #x4e001c00 #xffe0fc00 :asimdins 0 SIMD '(:Ed :Rn) QL-INS-XR F-ALIAS)
   ("ins" #x6e000400 #xffe08400 :asimdins 0 SIMD '(:Ed :En) QL-S-2SAME F-HAS-ALIAS)
   ("mov" #x6e000400 #xffe08400 :asimdins 0 SIMD '(:Ed :En) QL-S-2SAME F-ALIAS)
   ("rev64" #xe200800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMEBHS F-SIZEQ)
   ("rev16" #xe201800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMEB F-SIZEQ)
   ("saddlp" #xe202800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2PAIRWISELONGBHS F-SIZEQ)
   ("suqadd" #xe203800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAME F-SIZEQ)
   ("cls" #xe204800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMEBHS F-SIZEQ)
   ("cnt" #xe205800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMEB F-SIZEQ)
   ("sadalp" #xe206800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2PAIRWISELONGBHS F-SIZEQ)
   ("sqabs" #xe207800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAME F-SIZEQ)
   ("cmgt" #xe208800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAME F-SIZEQ)
   ("cmeq" #xe209800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAME F-SIZEQ)
   ("cmlt" #xe20a800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAME F-SIZEQ)
   ("abs" #xe20b800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAME F-SIZEQ)
   ("xtn" #xe212800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRBHS F-SIZEQ)
   ("xtn2" #x4e212800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRBHS2 F-SIZEQ)
   ("sqxtn" #xe214800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRBHS F-SIZEQ)
   ("sqxtn2" #x4e214800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRBHS2 F-SIZEQ)
   ("fcvtn" #xe216800 #xffbffc00 :asimdmisc OP-FCVTN SIMD '(:Vd :Vn) QL-V2NARRHS F-MISC)
   ("fcvtn2" #x4e216800 #xffbffc00 :asimdmisc OP-FCVTN2 SIMD '(:Vd :Vn) QL-V2NARRHS2 F-MISC)
   ("fcvtl" #xe217800 #xffbffc00 :asimdmisc OP-FCVTL SIMD '(:Vd :Vn) QL-V2LONGHS F-MISC)
   ("fcvtl2" #x4e217800 #xffbffc00 :asimdmisc OP-FCVTL2 SIMD '(:Vd :Vn) QL-V2LONGHS2 F-MISC)
   ("frintn" #xe218800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("frintm" #xe219800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtns" #xe21a800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtms" #xe21b800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtas" #xe21c800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("scvtf" #xe21d800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcmgt" #xea0c800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAMESD F-SIZEQ)
   ("fcmeq" #xea0d800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAMESD F-SIZEQ)
   ("fcmlt" #xea0e800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAMESD F-SIZEQ)
   ("fabs" #xea0f800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("frintp" #xea18800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("frintz" #xea19800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtps" #xea1a800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtzs" #xea1b800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("urecpe" #xea1c800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMES F-SIZEQ)
   ("frecpe" #xea1d800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("rev32" #x2e200800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMEBH F-SIZEQ)
   ("uaddlp" #x2e202800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2PAIRWISELONGBHS F-SIZEQ)
   ("usqadd" #x2e203800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAME F-SIZEQ)
   ("clz" #x2e204800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMEBHS F-SIZEQ)
   ("uadalp" #x2e206800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2PAIRWISELONGBHS F-SIZEQ)
   ("sqneg" #x2e207800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAME F-SIZEQ)
   ("cmge" #x2e208800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAME F-SIZEQ)
   ("cmle" #x2e209800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAME F-SIZEQ)
   ("neg" #x2e20b800 #xbf3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAME F-SIZEQ)
   ("sqxtun" #x2e212800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRBHS F-SIZEQ)
   ("sqxtun2" #x6e212800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRBHS2 F-SIZEQ)
   ("shll" #x2e213800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn :SHLL-IMM) QL-V2LONGBHS F-SIZEQ)
   ("shll2" #x6e213800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn :SHLL-IMM) QL-V2LONGBHS2 F-SIZEQ)
   ("uqxtn" #x2e214800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRBHS F-SIZEQ)
   ("uqxtn2" #x6e214800 #xff3ffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRBHS2 F-SIZEQ)
   ("fcvtxn" #x2e616800 #xfffffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRS 0)
   ("fcvtxn2" #x6e616800 #xfffffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2NARRS2 0)
   ("frinta" #x2e218800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("frintx" #x2e219800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtnu" #x2e21a800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtmu" #x2e21b800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtau" #x2e21c800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("ucvtf" #x2e21d800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("not" #x2e205800 #xbffffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMEB (F-SIZEQ  F-HAS-ALIAS))
   ("mvn" #x2e205800 #xbffffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMEB (F-SIZEQ  F-ALIAS))
   ("rbit" #x2e605800 #xbffffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMEB F-SIZEQ)
   ("fcmge" #x2ea0c800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAMESD F-SIZEQ)
   ("fcmle" #x2ea0d800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn :IMM0) QL-V2SAMESD F-SIZEQ)
   ("fneg" #x2ea0f800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("frinti" #x2ea19800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtpu" #x2ea1a800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fcvtzu" #x2ea1b800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("ursqrte" #x2ea1c800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMES F-SIZEQ)
   ("frsqrte" #x2ea1d800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("fsqrt" #x2ea1f800 #xbfbffc00 :asimdmisc 0 SIMD '(:Vd :Vn) QL-V2SAMESD F-SIZEQ)
   ("uzp1" #xe001800 #xbf20fc00 :asimdperm 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("trn1" #xe002800 #xbf20fc00 :asimdperm 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("zip1" #xe003800 #xbf20fc00 :asimdperm 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("uzp2" #xe005800 #xbf20fc00 :asimdperm 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("trn2" #xe006800 #xbf20fc00 :asimdperm 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("zip2" #xe007800 #xbf20fc00 :asimdperm 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("shadd" #xe200400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("sqadd" #xe200c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("srhadd" #xe201400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("shsub" #xe202400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("sqsub" #xe202c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("cmgt" #xe203400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("cmge" #xe203c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("sshl" #xe204400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("sqshl" #xe204c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("srshl" #xe205400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("sqrshl" #xe205c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("smax" #xe206400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("smin" #xe206c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("sabd" #xe207400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("saba" #xe207c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("add" #xe208400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("cmtst" #xe208c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("mla" #xe209400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("mul" #xe209c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("smaxp" #xe20a400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("sminp" #xe20ac00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("sqdmulh" #xe20b400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEHS F-SIZEQ)
   ("addp" #xe20bc00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("fmaxnm" #xe20c400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fmla" #xe20cc00 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fadd" #xe20d400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fmulx" #xe20dc00 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fcmeq" #xe20e400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fmax" #xe20f400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("frecps" #xe20fc00 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("and" #xe201c00 #xbfe0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEB F-SIZEQ)
   ("bic" #xe601c00 #xbfe0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEB F-SIZEQ)
   ("fminnm" #xea0c400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fmls" #xea0cc00 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fsub" #xea0d400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fmin" #xea0f400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("frsqrts" #xea0fc00 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("orr" #xea01c00 #xbfe0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEB (F-HAS-ALIAS  F-SIZEQ))
   ("mov" #xea01c00 #xbfe0fc00 :asimdsame OP-MOV-V SIMD '(:Vd :Vn) QL-V2SAMEB (F-ALIAS  F-CONV))
   ("orn" #xee01c00 #xbfe0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEB F-SIZEQ)
   ("uhadd" #x2e200400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("uqadd" #x2e200c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("urhadd" #x2e201400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("uhsub" #x2e202400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("uqsub" #x2e202c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("cmhi" #x2e203400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("cmhs" #x2e203c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("ushl" #x2e204400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("uqshl" #x2e204c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("urshl" #x2e205400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("uqrshl" #x2e205c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("umax" #x2e206400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("umin" #x2e206c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("uabd" #x2e207400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("uaba" #x2e207c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("sub" #x2e208400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("cmeq" #x2e208c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAME F-SIZEQ)
   ("mls" #x2e209400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("pmul" #x2e209c00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEB F-SIZEQ)
   ("umaxp" #x2e20a400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("uminp" #x2e20ac00 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEBHS F-SIZEQ)
   ("sqrdmulh" #x2e20b400 #xbf20fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEHS F-SIZEQ)
   ("fmaxnmp" #x2e20c400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("faddp" #x2e20d400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fmul" #x2e20dc00 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fcmge" #x2e20e400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("facge" #x2e20ec00 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fmaxp" #x2e20f400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fdiv" #x2e20fc00 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("eor" #x2e201c00 #xbfe0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEB F-SIZEQ)
   ("bsl" #x2e601c00 #xbfe0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEB F-SIZEQ)
   ("fminnmp" #x2ea0c400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fabd" #x2ea0d400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fcmgt" #x2ea0e400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("facgt" #x2ea0ec00 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("fminp" #x2ea0f400 #xbfa0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMESD F-SIZEQ)
   ("bit" #x2ea01c00 #xbfe0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEB F-SIZEQ)
   ("bif" #x2ee01c00 #xbfe0fc00 :asimdsame 0 SIMD '(:Vd :Vn :Vm) QL-V3SAMEB F-SIZEQ)
   ("sshr" #xf000400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT 0)
   ("ssra" #xf001400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT 0)
   ("srshr" #xf002400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT 0)
   ("srsra" #xf003400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT 0)
   ("shl" #xf005400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSL) QL-VSHIFT 0)
   ("sqshl" #xf007400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSL) QL-VSHIFT 0)
   ("shrn" #xf008400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN 0)
   ("shrn2" #x4f008400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN2 0)
   ("rshrn" #xf008c00 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN 0)
   ("rshrn2" #x4f008c00 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN2 0)
   ("sqshrn" #xf009400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN 0)
   ("sqshrn2" #x4f009400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN2 0)
   ("sqrshrn" #xf009c00 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN 0)
   ("sqrshrn2" #x4f009c00 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN2 0)
   ("sshll" #xf00a400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSL) QL-VSHIFTL F-HAS-ALIAS)
   ("sxtl" #xf00a400 #xff87fc00 :asimdshf OP-SXTL SIMD '(:Vd :Vn) QL-V2LONGBHS (F-ALIAS  F-CONV))
   ("sshll2" #x4f00a400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSL) QL-VSHIFTL2 F-HAS-ALIAS)
   ("sxtl2" #x4f00a400 #xff87fc00 :asimdshf OP-SXTL2 SIMD '(:Vd :Vn) QL-V2LONGBHS2 (F-ALIAS  F-CONV))
   ("scvtf" #xf00e400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT-SD 0)
   ("fcvtzs" #xf00fc00 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT-SD 0)
   ("ushr" #x2f000400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT 0)
   ("usra" #x2f001400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT 0)
   ("urshr" #x2f002400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT 0)
   ("ursra" #x2f003400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT 0)
   ("sri" #x2f004400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT 0)
   ("sli" #x2f005400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSL) QL-VSHIFT 0)
   ("sqshlu" #x2f006400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSL) QL-VSHIFT 0)
   ("uqshl" #x2f007400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSL) QL-VSHIFT 0)
   ("sqshrun" #x2f008400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN 0)
   ("sqshrun2" #x6f008400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN2 0)
   ("sqrshrun" #x2f008c00 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN 0)
   ("sqrshrun2" #x6f008c00 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN2 0)
   ("uqshrn" #x2f009400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN 0)
   ("uqshrn2" #x6f009400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN2 0)
   ("uqrshrn" #x2f009c00 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN 0)
   ("uqrshrn2" #x6f009c00 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFTN2 0)
   ("ushll" #x2f00a400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSL) QL-VSHIFTL F-HAS-ALIAS)
   ("uxtl" #x2f00a400 #xff87fc00 :asimdshf OP-UXTL SIMD '(:Vd :Vn) QL-V2LONGBHS (F-ALIAS  F-CONV))
   ("ushll2" #x6f00a400 #xff80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSL) QL-VSHIFTL2 F-HAS-ALIAS)
   ("uxtl2" #x6f00a400 #xff87fc00 :asimdshf OP-UXTL2 SIMD '(:Vd :Vn) QL-V2LONGBHS2 (F-ALIAS  F-CONV))
   ("ucvtf" #x2f00e400 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT-SD 0)
   ("fcvtzu" #x2f00fc00 #xbf80fc00 :asimdshf 0 SIMD '(:Vd :Vn :IMM-VLSR) QL-VSHIFT-SD 0)
   ("tbl" #xe000000 #xbfe09c00 :asimdtbl 0 SIMD '(:Vd :LVn :Vm) QL-TABLE F-SIZEQ)
   ("tbx" #xe001000 #xbfe09c00 :asimdtbl 0 SIMD '(:Vd :LVn :Vm) QL-TABLE F-SIZEQ)
   ("sqdmlal" #x5e209000 #xff20fc00 :asisddiff 0 SIMD '(:Sd :Sn :Sm) QL-SISDL-HS F-SSIZE)
   ("sqdmlsl" #x5e20b000 #xff20fc00 :asisddiff 0 SIMD '(:Sd :Sn :Sm) QL-SISDL-HS F-SSIZE)
   ("sqdmull" #x5e20d000 #xff20fc00 :asisddiff 0 SIMD '(:Sd :Sn :Sm) QL-SISDL-HS F-SSIZE)
   ("sqdmlal" #x5f003000 #xff00f400 :asisdelem 0 SIMD '(:Sd :Sn :Em) QL-SISDL-HS F-SSIZE)
   ("sqdmlsl" #x5f007000 #xff00f400 :asisdelem 0 SIMD '(:Sd :Sn :Em) QL-SISDL-HS F-SSIZE)
   ("sqdmull" #x5f00b000 #xff00f400 :asisdelem 0 SIMD '(:Sd :Sn :Em) QL-SISDL-HS F-SSIZE)
   ("sqdmulh" #x5f00c000 #xff00f400 :asisdelem 0 SIMD '(:Sd :Sn :Em) QL-SISD-HS F-SSIZE)
   ("sqrdmulh" #x5f00d000 #xff00f400 :asisdelem 0 SIMD '(:Sd :Sn :Em) QL-SISD-HS F-SSIZE)
   ("fmla" #x5f801000 #xff80f400 :asisdelem 0 SIMD '(:Sd :Sn :Em) QL-FP3 F-SSIZE)
   ("fmls" #x5f805000 #xff80f400 :asisdelem 0 SIMD '(:Sd :Sn :Em) QL-FP3 F-SSIZE)
   ("fmul" #x5f809000 #xff80f400 :asisdelem 0 SIMD '(:Sd :Sn :Em) QL-FP3 F-SSIZE)
   ("fmulx" #x7f809000 #xff80f400 :asisdelem 0 SIMD '(:Sd :Sn :Em) QL-FP3 F-SSIZE)
   ("st4" #xc000000 #xbfff0000 :asisdlse 0 SIMD '(:LVt :SIMD-ADDR-SIMPLE) QL-SIMD-LDST (F-SIZEQ  F-OD)(4))
   ("st1" #xc000000 #xbfff0000 :asisdlse 0 SIMD '(:LVt :SIMD-ADDR-SIMPLE) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(1))
   ("st2" #xc000000 #xbfff0000 :asisdlse 0 SIMD '(:LVt :SIMD-ADDR-SIMPLE) QL-SIMD-LDST (F-SIZEQ  F-OD)(2))
   ("st3" #xc000000 #xbfff0000 :asisdlse 0 SIMD '(:LVt :SIMD-ADDR-SIMPLE) QL-SIMD-LDST (F-SIZEQ  F-OD)(3))
   ("ld4" #xc400000 #xbfff0000 :asisdlse 0 SIMD '(:LVt :SIMD-ADDR-SIMPLE) QL-SIMD-LDST (F-SIZEQ  F-OD)(4))
   ("ld1" #xc400000 #xbfff0000 :asisdlse 0 SIMD '(:LVt :SIMD-ADDR-SIMPLE) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(1))
   ("ld2" #xc400000 #xbfff0000 :asisdlse 0 SIMD '(:LVt :SIMD-ADDR-SIMPLE) QL-SIMD-LDST (F-SIZEQ  F-OD)(2))
   ("ld3" #xc400000 #xbfff0000 :asisdlse 0 SIMD '(:LVt :SIMD-ADDR-SIMPLE) QL-SIMD-LDST (F-SIZEQ  F-OD)(3))
   ("st4" #xc800000 #xbfe00000 :asisdlsep 0 SIMD '(:LVt :SIMD-ADDR-POST) QL-SIMD-LDST (F-SIZEQ  F-OD)(4))
   ("st1" #xc800000 #xbfe00000 :asisdlsep 0 SIMD '(:LVt :SIMD-ADDR-POST) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(1))
   ("st2" #xc800000 #xbfe00000 :asisdlsep 0 SIMD '(:LVt :SIMD-ADDR-POST) QL-SIMD-LDST (F-SIZEQ  F-OD)(2))
   ("st3" #xc800000 #xbfe00000 :asisdlsep 0 SIMD '(:LVt :SIMD-ADDR-POST) QL-SIMD-LDST (F-SIZEQ  F-OD)(3))
   ("ld4" #xcc00000 #xbfe00000 :asisdlsep 0 SIMD '(:LVt :SIMD-ADDR-POST) QL-SIMD-LDST (F-SIZEQ  F-OD)(4))
   ("ld1" #xcc00000 #xbfe00000 :asisdlsep 0 SIMD '(:LVt :SIMD-ADDR-POST) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(1))
   ("ld2" #xcc00000 #xbfe00000 :asisdlsep 0 SIMD '(:LVt :SIMD-ADDR-POST) QL-SIMD-LDST (F-SIZEQ  F-OD)(2))
   ("ld3" #xcc00000 #xbfe00000 :asisdlsep 0 SIMD '(:LVt :SIMD-ADDR-POST) QL-SIMD-LDST (F-SIZEQ  F-OD)(3))
   ("st1" #xd000000 #xbfff2000 :asisdlso 0 SIMD '(:LEt :SIMD-ADDR-SIMPLE) QL-SIMD-LDSTONE F-OD(1))
   ("st3" #xd002000 #xbfff2000 :asisdlso 0 SIMD '(:LEt :SIMD-ADDR-SIMPLE) QL-SIMD-LDSTONE F-OD(3))
   ("st2" #xd200000 #xbfff2000 :asisdlso 0 SIMD '(:LEt :SIMD-ADDR-SIMPLE) QL-SIMD-LDSTONE F-OD(2))
   ("st4" #xd202000 #xbfff2000 :asisdlso 0 SIMD '(:LEt :SIMD-ADDR-SIMPLE) QL-SIMD-LDSTONE F-OD(4))
   ("ld1" #xd400000 #xbfff2000 :asisdlso 0 SIMD '(:LEt :SIMD-ADDR-SIMPLE) QL-SIMD-LDSTONE F-OD(1))
   ("ld3" #xd402000 #xbfff2000 :asisdlso 0 SIMD '(:LEt :SIMD-ADDR-SIMPLE) QL-SIMD-LDSTONE F-OD(3))
   ("ld1r" #xd40c000 #xbfffe000 :asisdlso 0 SIMD '(:LVt-AL :SIMD-ADDR-SIMPLE) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(1))
   ("ld3r" #xd40e000 #xbfffe000 :asisdlso 0 SIMD '(:LVt-AL :SIMD-ADDR-SIMPLE) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(3))
   ("ld2" #xd600000 #xbfff2000 :asisdlso 0 SIMD '(:LEt :SIMD-ADDR-SIMPLE) QL-SIMD-LDSTONE F-OD(2))
   ("ld4" #xd602000 #xbfff2000 :asisdlso 0 SIMD '(:LEt :SIMD-ADDR-SIMPLE) QL-SIMD-LDSTONE F-OD(4))
   ("ld2r" #xd60c000 #xbfffe000 :asisdlso 0 SIMD '(:LVt-AL :SIMD-ADDR-SIMPLE) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(2))
   ("ld4r" #xd60e000 #xbfffe000 :asisdlso 0 SIMD '(:LVt-AL :SIMD-ADDR-SIMPLE) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(4))
   ("st1" #xd800000 #xbfe02000 :asisdlsop 0 SIMD '(:LEt :SIMD-ADDR-POST) QL-SIMD-LDSTONE F-OD(1))
   ("st3" #xd802000 #xbfe02000 :asisdlsop 0 SIMD '(:LEt :SIMD-ADDR-POST) QL-SIMD-LDSTONE F-OD(3))
   ("st2" #xda00000 #xbfe02000 :asisdlsop 0 SIMD '(:LEt :SIMD-ADDR-POST) QL-SIMD-LDSTONE F-OD(2))
   ("st4" #xda02000 #xbfe02000 :asisdlsop 0 SIMD '(:LEt :SIMD-ADDR-POST) QL-SIMD-LDSTONE F-OD(4))
   ("ld1" #xdc00000 #xbfe02000 :asisdlsop 0 SIMD '(:LEt :SIMD-ADDR-POST) QL-SIMD-LDSTONE F-OD(1))
   ("ld3" #xdc02000 #xbfe02000 :asisdlsop 0 SIMD '(:LEt :SIMD-ADDR-POST) QL-SIMD-LDSTONE F-OD(3))
   ("ld1r" #xdc0c000 #xbfe0e000 :asisdlsop 0 SIMD '(:LVt-AL :SIMD-ADDR-POST) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(1))
   ("ld3r" #xdc0e000 #xbfe0e000 :asisdlsop 0 SIMD '(:LVt-AL :SIMD-ADDR-POST) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(3))
   ("ld2" #xde00000 #xbfe02000 :asisdlsop 0 SIMD '(:LEt :SIMD-ADDR-POST) QL-SIMD-LDSTONE F-OD(2))
   ("ld4" #xde02000 #xbfe02000 :asisdlsop 0 SIMD '(:LEt :SIMD-ADDR-POST) QL-SIMD-LDSTONE F-OD(4))
   ("ld2r" #xde0c000 #xbfe0e000 :asisdlsop 0 SIMD '(:LVt-AL :SIMD-ADDR-POST) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(2))
   ("ld4r" #xde0e000 #xbfe0e000 :asisdlsop 0 SIMD '(:LVt-AL :SIMD-ADDR-POST) QL-SIMD-LDST-ANY (F-SIZEQ  F-OD)(4))
   ("suqadd" #x5e203800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAME F-SSIZE)
   ("sqabs" #x5e207800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAME F-SSIZE)
   ("cmgt" #x5e208800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-CMP-0 F-SSIZE)
   ("cmeq" #x5e209800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-CMP-0 F-SSIZE)
   ("cmlt" #x5e20a800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-CMP-0 F-SSIZE)
   ("abs" #x5e20b800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-2SAMED F-SSIZE)
   ("sqxtn" #x5e214800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-SISD-NARROW F-SSIZE)
   ("fcvtns" #x5e21a800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("fcvtms" #x5e21b800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("fcvtas" #x5e21c800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("scvtf" #x5e21d800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("fcmgt" #x5ea0c800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-FCMP-0 F-SSIZE)
   ("fcmeq" #x5ea0d800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-FCMP-0 F-SSIZE)
   ("fcmlt" #x5ea0e800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-FCMP-0 F-SSIZE)
   ("fcvtps" #x5ea1a800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("fcvtzs" #x5ea1b800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("frecpe" #x5ea1d800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("frecpx" #x5ea1f800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("usqadd" #x7e203800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAME F-SSIZE)
   ("sqneg" #x7e207800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAME F-SSIZE)
   ("cmge" #x7e208800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-CMP-0 F-SSIZE)
   ("cmle" #x7e209800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-CMP-0 F-SSIZE)
   ("neg" #x7e20b800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-2SAMED F-SSIZE)
   ("sqxtun" #x7e212800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-SISD-NARROW F-SSIZE)
   ("uqxtn" #x7e214800 #xff3ffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-SISD-NARROW F-SSIZE)
   ("fcvtxn" #x7e216800 #xffbffc00 :asisdmisc OP-FCVTXN-S SIMD '(:Sd :Sn) QL-SISD-NARROW-S F-MISC)
   ("fcvtnu" #x7e21a800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("fcvtmu" #x7e21b800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("fcvtau" #x7e21c800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("ucvtf" #x7e21d800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("fcmge" #x7ea0c800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-FCMP-0 F-SSIZE)
   ("fcmle" #x7ea0d800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn :IMM0) QL-SISD-FCMP-0 F-SSIZE)
   ("fcvtpu" #x7ea1a800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("fcvtzu" #x7ea1b800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("frsqrte" #x7ea1d800 #xffbffc00 :asisdmisc 0 SIMD '(:Sd :Sn) QL-S-2SAMESD F-SSIZE)
   ("dup" #x5e000400 #xffe0fc00 :asisdone 0 SIMD '(:Sd :En) QL-S-2SAME F-HAS-ALIAS)
   ("mov" #x5e000400 #xffe0fc00 :asisdone 0 SIMD '(:Sd :En) QL-S-2SAME F-ALIAS)
   ("addp" #x5e31b800 #xff3ffc00 :asisdpair 0 SIMD '(:Sd :Vn) QL-SISD-PAIR-D F-SIZEQ)
   ("fmaxnmp" #x7e30c800 #xffbffc00 :asisdpair 0 SIMD '(:Sd :Vn) QL-SISD-PAIR F-SIZEQ)
   ("faddp" #x7e30d800 #xffbffc00 :asisdpair 0 SIMD '(:Sd :Vn) QL-SISD-PAIR F-SIZEQ)
   ("fmaxp" #x7e30f800 #xffbffc00 :asisdpair 0 SIMD '(:Sd :Vn) QL-SISD-PAIR F-SIZEQ)
   ("fminnmp" #x7eb0c800 #xffbffc00 :asisdpair 0 SIMD '(:Sd :Vn) QL-SISD-PAIR F-SIZEQ)
   ("fminp" #x7eb0f800 #xffbffc00 :asisdpair 0 SIMD '(:Sd :Vn) QL-SISD-PAIR F-SIZEQ)
   ("sqadd" #x5e200c00 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAME F-SSIZE)
   ("sqsub" #x5e202c00 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAME F-SSIZE)
   ("sqshl" #x5e204c00 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAME F-SSIZE)
   ("sqrshl" #x5e205c00 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAME F-SSIZE)
   ("sqdmulh" #x5e20b400 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-SISD-HS F-SSIZE)
   ("fmulx" #x5e20dc00 #xffa0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-FP3 F-SSIZE)
   ("fcmeq" #x5e20e400 #xffa0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-FP3 F-SSIZE)
   ("frecps" #x5e20fc00 #xffa0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-FP3 F-SSIZE)
   ("frsqrts" #x5ea0fc00 #xffa0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-FP3 F-SSIZE)
   ("cmgt" #x5ee03400 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("cmge" #x5ee03c00 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("sshl" #x5ee04400 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("srshl" #x5ee05400 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("add" #x5ee08400 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("cmtst" #x5ee08c00 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("uqadd" #x7e200c00 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAME F-SSIZE)
   ("uqsub" #x7e202c00 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAME F-SSIZE)
   ("uqshl" #x7e204c00 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAME F-SSIZE)
   ("uqrshl" #x7e205c00 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAME F-SSIZE)
   ("sqrdmulh" #x7e20b400 #xff20fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-SISD-HS F-SSIZE)
   ("fcmge" #x7e20e400 #xffa0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-FP3 F-SSIZE)
   ("facge" #x7e20ec00 #xffa0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-FP3 F-SSIZE)
   ("fabd" #x7ea0d400 #xffa0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-FP3 F-SSIZE)
   ("fcmgt" #x7ea0e400 #xffa0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-FP3 F-SSIZE)
   ("facgt" #x7ea0ec00 #xffa0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-FP3 F-SSIZE)
   ("cmhi" #x7ee03400 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("cmhs" #x7ee03c00 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("ushl" #x7ee04400 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("urshl" #x7ee05400 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("sub" #x7ee08400 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("cmeq" #x7ee08c00 #xffe0fc00 :asisdsame 0 SIMD '(:Sd :Sn :Sm) QL-S-3SAMED F-SSIZE)
   ("sshr" #x5f000400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-D 0)
   ("ssra" #x5f001400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-D 0)
   ("srshr" #x5f002400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-D 0)
   ("srsra" #x5f003400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-D 0)
   ("shl" #x5f005400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSL) QL-SSHIFT-D 0)
   ("sqshl" #x5f007400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSL) QL-SSHIFT 0)
   ("sqshrn" #x5f009400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFTN 0)
   ("sqrshrn" #x5f009c00 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFTN 0)
   ("scvtf" #x5f00e400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-SD 0)
   ("fcvtzs" #x5f00fc00 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-SD 0)
   ("ushr" #x7f000400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-D 0)
   ("usra" #x7f001400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-D 0)
   ("urshr" #x7f002400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-D 0)
   ("ursra" #x7f003400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-D 0)
   ("sri" #x7f004400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-D 0)
   ("sli" #x7f005400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSL) QL-SSHIFT-D 0)
   ("sqshlu" #x7f006400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSL) QL-SSHIFT 0)
   ("uqshl" #x7f007400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSL) QL-SSHIFT 0)
   ("sqshrun" #x7f008400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFTN 0)
   ("sqrshrun" #x7f008c00 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFTN 0)
   ("uqshrn" #x7f009400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFTN 0)
   ("uqrshrn" #x7f009c00 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFTN 0)
   ("ucvtf" #x7f00e400 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-SD 0)
   ("fcvtzu" #x7f00fc00 #xff80fc00 :asisdshf 0 SIMD '(:Sd :Sn :IMM-VLSR) QL-SSHIFT-SD 0)
   ("sbfm" #x13000000 #x7f800000 :bitfield 0 :CORE '(:Rd :Rn :IMMR :IMMS) QL-BF ((F-HAS-ALIAS  F-SF)  F-N))
   ("sbfiz" #x13000000 #x7f800000 :bitfield OP-SBFIZ :CORE '(:Rd :Rn :IMM :WIDTH) QL-BF2 ((F-ALIAS  F-P1)  F-CONV))
   ("sbfx" #x13000000 #x7f800000 :bitfield OP-SBFX :CORE '(:Rd :Rn :IMM :WIDTH) QL-BF2 ((F-ALIAS  F-P1)  F-CONV))
   ("sxtb" #x13001c00 #x7fbffc00 :bitfield 0 :CORE '(:Rd :Rn) QL-EXT (((F-ALIAS  F-P3)  F-SF)  F-N))
   ("sxth" #x13003c00 #x7fbffc00 :bitfield 0 :CORE '(:Rd :Rn) QL-EXT (((F-ALIAS  F-P3)  F-SF)  F-N))
   ("sxtw" #x93407c00 #xfffffc00 :bitfield 0 :CORE '(:Rd :Rn) QL-EXT-W (F-ALIAS  F-P3))
   ("asr" #x13000000 #x7f800000 :bitfield OP-ASR-IMM :CORE '(:Rd :Rn :IMM) QL-SHIFT ((F-ALIAS  F-P2)  F-CONV))
   ("bfm" #x33000000 #x7f800000 :bitfield 0 :CORE '(:Rd :Rn :IMMR :IMMS) QL-BF ((F-HAS-ALIAS  F-SF)  F-N))
   ("bfi" #x33000000 #x7f800000 :bitfield OP-BFI :CORE '(:Rd :Rn :IMM :WIDTH) QL-BF2 ((F-ALIAS  F-P1)  F-CONV))
   ("bfxil" #x33000000 #x7f800000 :bitfield OP-BFXIL :CORE '(:Rd :Rn :IMM :WIDTH) QL-BF2 ((F-ALIAS  F-P1)  F-CONV))
   ("ubfm" #x53000000 #x7f800000 :bitfield 0 :CORE '(:Rd :Rn :IMMR :IMMS) QL-BF ((F-HAS-ALIAS  F-SF)  F-N))
   ("ubfiz" #x53000000 #x7f800000 :bitfield OP-UBFIZ :CORE '(:Rd :Rn :IMM :WIDTH) QL-BF2 ((F-ALIAS  F-P1)  F-CONV))
   ("ubfx" #x53000000 #x7f800000 :bitfield OP-UBFX :CORE '(:Rd :Rn :IMM :WIDTH) QL-BF2 ((F-ALIAS  F-P1)  F-CONV))
   ("uxtb" #x53001c00 #xfffffc00 :bitfield OP-UXTB :CORE '(:Rd :Rn) QL-I2SAMEW (F-ALIAS  F-P3))
   ("uxth" #x53003c00 #xfffffc00 :bitfield OP-UXTH :CORE '(:Rd :Rn) QL-I2SAMEW (F-ALIAS  F-P3))
   ("lsl" #x53000000 #x7f800000 :bitfield OP-LSL-IMM :CORE '(:Rd :Rn :IMM) QL-SHIFT ((F-ALIAS  F-P2)  F-CONV))
   ("lsr" #x53000000 #x7f800000 :bitfield OP-LSR-IMM :CORE '(:Rd :Rn :IMM) QL-SHIFT ((F-ALIAS  F-P2)  F-CONV))
   ("b" #x14000000 #xfc000000 :branch-imm OP-B :CORE '(:ADDR-PCREL26) QL-PCREL-26 0)
   ("bl" #x94000000 #xfc000000 :branch-imm OP-BL :CORE '(:ADDR-PCREL26) QL-PCREL-26 0)
   ("br" #xd61f0000 #xfffffc1f :branch-reg 0 :CORE '(:Rn) QL-I1X 0)
   ("blr" #xd63f0000 #xfffffc1f :branch-reg 0 :CORE '(:Rn) QL-I1X 0)
   ("ret" #xd65f0000 #xfffffc1f :branch-reg 0 :CORE '(:Rn) QL-I1X (F-OPD0-OPT  F-DEFAULT) (30))
   ("eret" #xd69f03e0 #xffffffff :branch-reg 0 :CORE '() () 0)
   ("drps" #xd6bf03e0 #xffffffff :branch-reg 0 :CORE '() () 0)
   ("cbz" #x34000000 #x7f000000 :compbranch 0 :CORE '(:Rt :ADDR-PCREL19) QL-R-PCREL F-SF)
   ("cbnz" #x35000000 #x7f000000 :compbranch 0 :CORE '(:Rt :ADDR-PCREL19) QL-R-PCREL F-SF)
   ("b.c" #x54000000 #xff000010 :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL F-COND)
   ("ccmn" #x3a400800 #x7fe00c10 :condcmp-imm 0 :CORE '(:Rn :CCMP-IMM :NZCV :COND) QL-CCMP-IMM F-SF)
   ("ccmp" #x7a400800 #x7fe00c10 :condcmp-imm 0 :CORE '(:Rn :CCMP-IMM :NZCV :COND) QL-CCMP-IMM F-SF)
   ("ccmn" #x3a400000 #x7fe00c10 :condcmp-reg 0 :CORE '(:Rn :Rm :NZCV :COND) QL-CCMP F-SF)
   ("ccmp" #x7a400000 #x7fe00c10 :condcmp-reg 0 :CORE '(:Rn :Rm :NZCV :COND) QL-CCMP F-SF)
   ("csel" #x1a800000 #x7fe00c00 :condsel 0 :CORE '(:Rd :Rn :Rm :COND) QL-CSEL F-SF)
   ("csinc" #x1a800400 #x7fe00c00 :condsel 0 :CORE '(:Rd :Rn :Rm :COND) QL-CSEL (F-HAS-ALIAS  F-SF))
   ("cinc" #x1a800400 #x7fe00c00 :condsel OP-CINC :CORE '(:Rd :Rn :COND) QL-CSEL ((F-ALIAS  F-SF)  F-CONV))
   ("cset" #x1a9f07e0 #x7fff0fe0 :condsel OP-CSET :CORE '(:Rd :COND) QL-DST-R (((F-ALIAS  F-P1)  F-SF)  F-CONV))
   ("csinv" #x5a800000 #x7fe00c00 :condsel 0 :CORE '(:Rd :Rn :Rm :COND) QL-CSEL (F-HAS-ALIAS  F-SF))
   ("cinv" #x5a800000 #x7fe00c00 :condsel OP-CINV :CORE '(:Rd :Rn :COND) QL-CSEL ((F-ALIAS  F-SF)  F-CONV))
   ("csetm" #x5a9f03e0 #x7fff0fe0 :condsel OP-CSETM :CORE '(:Rd :COND) QL-DST-R (((F-ALIAS  F-P1)  F-SF)  F-CONV))
   ("csneg" #x5a800400 #x7fe00c00 :condsel 0 :CORE '(:Rd :Rn :Rm :COND) QL-CSEL (F-HAS-ALIAS  F-SF))
   ("cneg" #x5a800400 #x7fe00c00 :condsel OP-CNEG :CORE '(:Rd :Rn :COND) QL-CSEL ((F-ALIAS  F-SF)  F-CONV))
   ("aese" #x4e284800 #xfffffc00 :cryptoaes 0 :CRYPTO '(:Vd :Vn) QL-V2SAME16B 0)
   ("aesd" #x4e285800 #xfffffc00 :cryptoaes 0 :CRYPTO '(:Vd :Vn) QL-V2SAME16B 0)
   ("aesmc" #x4e286800 #xfffffc00 :cryptoaes 0 :CRYPTO '(:Vd :Vn) QL-V2SAME16B 0)
   ("aesimc" #x4e287800 #xfffffc00 :cryptoaes 0 :CRYPTO '(:Vd :Vn) QL-V2SAME16B 0)
   ("sha1h" #x5e280800 #xfffffc00 :cryptosha2 0 :CRYPTO '(:Fd :Fn) QL-2SAMES 0)
   ("sha1su1" #x5e281800 #xfffffc00 :cryptosha2 0 :CRYPTO '(:Vd :Vn) QL-V2SAME4S 0)
   ("sha256su0" #x5e282800 #xfffffc00 :cryptosha2 0 :CRYPTO '(:Vd :Vn) QL-V2SAME4S 0)
   ("sha1c" #x5e000000 #xffe0fc00 :cryptosha3 0 :CRYPTO '(:Fd :Fn :Vm) QL-SHAUPT 0)
   ("sha1p" #x5e001000 #xffe0fc00 :cryptosha3 0 :CRYPTO '(:Fd :Fn :Vm) QL-SHAUPT 0)

   ("sha1m" #x5e002000 #xffe0fc00 :cryptosha3 0 :CRYPTO '(:Fd :Fn :Vm) QL-SHAUPT 0)
   ("sha1su0" #x5e003000 #xffe0fc00 :cryptosha3 0 :CRYPTO '(:Vd :Vn :Vm) QL-V3SAME4S 0)
   ("sha256h" #x5e004000 #xffe0fc00 :cryptosha3 0 :CRYPTO '(:Fd :Fn :Vm) QL-SHA256UPT 0)
   ("sha256h2" #x5e005000 #xffe0fc00 :cryptosha3 0 :CRYPTO '(:Fd :Fn :Vm) QL-SHA256UPT 0)
   ("sha256su1" #x5e006000 #xffe0fc00 :cryptosha3 0 :CRYPTO '(:Vd :Vn :Vm) QL-V3SAME4S 0)
   ("rbit" #x5ac00000 #x7ffffc00 :dp-1src 0 :CORE '(:Rd :Rn) QL-I2SAME F-SF)
   ("rev16" #x5ac00400 #x7ffffc00 :dp-1src 0 :CORE '(:Rd :Rn) QL-I2SAME F-SF)
   ("rev" #x5ac00800 #xfffffc00 :dp-1src 0 :CORE '(:Rd :Rn) QL-I2SAMEW 0)
   ("rev" #xdac00c00 #x7ffffc00 :dp-1src 0 :CORE '(:Rd :Rn) QL-I2SAMEX 0)
   ("clz" #x5ac01000 #x7ffffc00 :dp-1src 0 :CORE '(:Rd :Rn) QL-I2SAME F-SF)
   ("cls" #x5ac01400 #x7ffffc00 :dp-1src 0 :CORE '(:Rd :Rn) QL-I2SAME F-SF)
   ("rev32" #xdac00800 #xfffffc00 :dp-1src 0 :CORE '(:Rd :Rn) QL-I2SAMEX 0)

   ("udiv" #x1ac00800 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) 0)
   ("udiv" #x9ac00800 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) 0)
   ("sdiv" #x1ac00c00 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) 0)
   ("sdiv" #x9ac00c00 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) 0)

   ("lslv" #x1ac02000 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-HAS-ALIAS)
   ("lslv" #x9ac02000 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-HAS-ALIAS)
   ("lsl" #x1ac02000 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-ALIAS)
   ("lsl" #x9ac02000 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-ALIAS)
   ("lsrv" #x1ac02400 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-HAS-ALIAS)
   ("lsrv" #x9ac02400 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-HAS-ALIAS))
  ("lsr" #x1ac02400 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-ALIAS)
  ("lsr" #x9ac02400 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-ALIAS)
  ("asrv" #x1ac02800 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-HAS-ALIAS)
  ("asrv" #x9ac02800 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-HAS-ALIAS)
  ("asr" #x1ac02800 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-ALIAS)
  ("asr" #x9ac02800 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-ALIAS)
  ("rorv" #x1ac02c00 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-HAS-ALIAS)
  ("rorv" #x9ac02c00 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-HAS-ALIAS)
  ("ror" #x1ac02c00 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-ALIAS)
  ("ror" #x9ac02c00 #xffe0fc00 :dp-2src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-ALIAS)

  ("madd" #x1b000000 #x7fe08000 :dp-3src 0 :CORE '(:Rd :Rn :Rm :Ra) QL-I4SAMER (F-HAS-ALIAS  F-SF))
  ("mul" #x1b007c00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-ALIAS)
  ("mul" #x9b007c00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-ALIAS)
  ("msub" #x1b008000 #x7fe08000 :dp-3src 0 :CORE '(:Rd :Rn :Rm :Ra) QL-I4SAMER (F-HAS-ALIAS  F-SF))
  ("mneg" #x1b00fc00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:w :w :w) F-ALIAS)
  ("mneg" #x9b00fc00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:x :x :x) F-ALIAS)
  ("smaddl" #x9b200000 #xffe08000 :dp-3src 0 :CORE '(:Rd :Rn :Rm :Ra) '(:x :w :w :x) F-HAS-ALIAS)
  ("smull" #x9b207c00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:X :W :W) F-ALIAS)
  ("smsubl" #x9b208000 #xffe08000 :dp-3src 0 :CORE '(:Rd :Rn :Rm :Ra) '(:X :W :W :X) F-HAS-ALIAS)
  ("smnegl" #x9b20fc00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:X :W :W) F-ALIAS)
  ("smulh" #x9b407c00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:X :X :X) 0)
  ("umaddl" #x9ba00000 #xffe08000 :dp-3src 0 :CORE '(:Rd :Rn :Rm :Ra) '(:X :W :W :X) F-HAS-ALIAS)
  ("umull" #x9ba07c00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:X :W :W) F-ALIAS)
  ("umsubl" #x9ba08000 #xffe08000 :dp-3src 0 :CORE '(:Rd :Rn :Rm :Ra) '(:X :W :W :X) F-HAS-ALIAS)
  ("umnegl" #x9ba0fc00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:X :W :W) F-ALIAS)
  ("umulh" #x9bc07c00 #xffe0fc00 :dp-3src 0 :CORE '(:Rd :Rn :Rm) '(:X :X :X) 0)
  ("svc" #xd4000001 #xffe0001f :exception 0 :CORE '(:EXCEPTION) () 0)
  ("hvc" #xd4000002 #xffe0001f :exception 0 :CORE '(:EXCEPTION) () 0)
  ("smc" #xd4000003 #xffe0001f :exception 0 :CORE '(:EXCEPTION) () 0)
  ("brk" #xd4200000 #xffe0001f :exception 0 :CORE '(:EXCEPTION) () 0)
  ("hlt" #xd4400000 #xffe0001f :exception 0 :CORE '(:EXCEPTION) () 0)
  ("dcps1" #xd4a00001 #xffe0001f :exception 0 :CORE '(:EXCEPTION) () (F-OPD0-OPT  F-DEFAULT) (0))
  ("dcps2" #xd4a00002 #xffe0001f :exception 0 :CORE '(:EXCEPTION) () (F-OPD0-OPT  F-DEFAULT) (0))
  ("dcps3" #xd4a00003 #xffe0001f :exception 0 :CORE '(:EXCEPTION) () (F-OPD0-OPT  F-DEFAULT) (0))
  ("extr" #x13800000 #x7fa00000 :extract 0 :CORE '(:Rd :Rn :Rm :IMMS) QL-EXTR ((F-HAS-ALIAS  F-SF)  F-N))
  ("ror" #x13800000 #x7fa00000 :extract OP-ROR-IMM :CORE '(:Rd :Rm :IMMS) QL-SHIFT (F-ALIAS  F-CONV))
  ("scvtf" #x1e020000 #x7f3f0000 :float2fix 0 FP '(:Fd :Rn :FBITS) QL-FIX2FP (F-FPTYPE  F-SF))
  ("ucvtf" #x1e030000 #x7f3f0000 :float2fix 0 FP '(:Fd :Rn :FBITS) QL-FIX2FP (F-FPTYPE  F-SF))
  ("fcvtzs" #x1e180000 #x7f3f0000 :float2fix 0 FP '(:Rd :Fn :FBITS) QL-FP2FIX (F-FPTYPE  F-SF))
  ("fcvtzu" #x1e190000 #x7f3f0000 :float2fix 0 FP '(:Rd :Fn :FBITS) QL-FP2FIX (F-FPTYPE  F-SF))
  ("fcvtns" #x1e200000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fcvtnu" #x1e210000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("scvtf" #x1e220000 #x7f3ffc00 :float2int 0 FP '(:Fd :Rn) QL-INT2FP (F-FPTYPE  F-SF))
  ("ucvtf" #x1e230000 #x7f3ffc00 :float2int 0 FP '(:Fd :Rn) QL-INT2FP (F-FPTYPE  F-SF))
  ("fcvtas" #x1e240000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fcvtau" #x1e250000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fmov" #x1e260000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fmov" #x1e270000 #x7f3ffc00 :float2int 0 FP '(:Fd :Rn) QL-INT2FP (F-FPTYPE  F-SF))
  ("fcvtps" #x1e280000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fcvtpu" #x1e290000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fcvtms" #x1e300000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fcvtmu" #x1e310000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fcvtzs" #x1e380000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fcvtzu" #x1e390000 #x7f3ffc00 :float2int 0 FP '(:Rd :Fn) QL-FP2INT (F-FPTYPE  F-SF))
  ("fmov" #x9eae0000 #xfffffc00 :float2int 0 FP '(:Rd :VnD1) QL-XVD1 0)
  ("fmov" #x9eaf0000 #xfffffc00 :float2int 0 FP '(:VdD1 :Rn) QL-VD1X 0)
  ("fccmp" #x1e200400 #xff200c10 :floatccmp 0 FP '(:Fn :Fm :NZCV :COND) QL-FCCMP F-FPTYPE)
  ("fccmpe" #x1e200410 #xff200c10 :floatccmp 0 FP '(:Fn :Fm :NZCV :COND) QL-FCCMP F-FPTYPE)
  ("fcmp" #x1e202000 #xff20fc1f :floatcmp 0 FP '(:Fn :Fm) QL-FP2 F-FPTYPE)
  ("fcmpe" #x1e202010 #xff20fc1f :floatcmp 0 FP '(:Fn :Fm) QL-FP2 F-FPTYPE)
  ("fcmp" #x1e202008 #xff20fc1f :floatcmp 0 FP '(:Fn :FPIMM0) QL-DST-SD F-FPTYPE)
  ("fcmpe" #x1e202018 #xff20fc1f :floatcmp 0 FP '(:Fn :FPIMM0) QL-DST-SD F-FPTYPE)
  ("fmov" #x1e204000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("fabs" #x1e20c000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("fneg" #x1e214000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("fsqrt" #x1e21c000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("fcvt" #x1e224000 #xff3e7c00 :floatdp1 OP-FCVT FP '(:Fd :Fn) QL-FCVT (F-FPTYPE  F-MISC))
  ("frintn" #x1e244000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("frintp" #x1e24c000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("frintm" #x1e254000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("frintz" #x1e25c000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("frinta" #x1e264000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("frintx" #x1e274000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("frinti" #x1e27c000 #xff3ffc00 :floatdp1 0 FP '(:Fd :Fn) QL-FP2 F-FPTYPE)
  ("fmul" #x1e200800 #xff20fc00 :floatdp2 0 FP '(:Fd :Fn :Fm) QL-FP3 F-FPTYPE)
  ("fdiv" #x1e201800 #xff20fc00 :floatdp2 0 FP '(:Fd :Fn :Fm) QL-FP3 F-FPTYPE)
  ("fadd" #x1e202800 #xff20fc00 :floatdp2 0 FP '(:Fd :Fn :Fm) QL-FP3 F-FPTYPE)
  ("fsub" #x1e203800 #xff20fc00 :floatdp2 0 FP '(:Fd :Fn :Fm) QL-FP3 F-FPTYPE)
  ("fmax" #x1e204800 #xff20fc00 :floatdp2 0 FP '(:Fd :Fn :Fm) QL-FP3 F-FPTYPE)
  ("fmin" #x1e205800 #xff20fc00 :floatdp2 0 FP '(:Fd :Fn :Fm) QL-FP3 F-FPTYPE)
  ("fmaxnm" #x1e206800 #xff20fc00 :floatdp2 0 FP '(:Fd :Fn :Fm) QL-FP3 F-FPTYPE)
  ("fminnm" #x1e207800 #xff20fc00 :floatdp2 0 FP '(:Fd :Fn :Fm) QL-FP3 F-FPTYPE)
  ("fnmul" #x1e208800 #xff20fc00 :floatdp2 0 FP '(:Fd :Fn :Fm) QL-FP3 F-FPTYPE)
  ("fmadd" #x1f000000 #xff208000 :floatdp3 0 FP '(:Fd :Fn :Fm :Fa) QL-FP4 F-FPTYPE)
  ("fmsub" #x1f008000 #xff208000 :floatdp3 0 FP '(:Fd :Fn :Fm :Fa) QL-FP4 F-FPTYPE)
  ("fnmadd" #x1f200000 #xff208000 :floatdp3 0 FP '(:Fd :Fn :Fm :Fa) QL-FP4 F-FPTYPE)
  ("fnmsub" #x1f208000 #xff208000 :floatdp3 0 FP '(:Fd :Fn :Fm :Fa) QL-FP4 F-FPTYPE)
  ("fmov" #x1e201000 #xff201fe0 :floatimm 0 FP '(:Fd :FPIMM) QL-DST-SD F-FPTYPE)
  ("fcsel" #x1e200c00 #xff200c00 :floatsel 0 FP '(:Fd :Fn :Fm :COND) QL-FP-COND F-FPTYPE)
  ("strb" #x38000400 #xffe00400 :ldst-imm9 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W8 0)
  ("ldrb" #x38400400 #xffe00400 :ldst-imm9 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W8 0)
  ("ldrsb" #x38800400 #xffa00400 :ldst-imm9 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R8 F-LDS-SIZE)
  ("str" #x3c000400 #x3f600400 :ldst-imm9 0 :CORE '(:Ft :ADDR-SIMM9) QL-LDST-FP 0)
  ("ldr" #x3c400400 #x3f600400 :ldst-imm9 0 :CORE '(:Ft :ADDR-SIMM9) QL-LDST-FP 0)
  ("strh" #x78000400 #xffe00400 :ldst-imm9 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W16 0)
  ("ldrh" #x78400400 #xffe00400 :ldst-imm9 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W16 0)
  ("ldrsh" #x78800400 #xffa00400 :ldst-imm9 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R16 F-LDS-SIZE)
  ("str" #xb8000400 #xbfe00400 :ldst-imm9 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R F-GPRSIZE-IN-Q)
  ("ldr" #xb8400400 #xbfe00400 :ldst-imm9 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R F-GPRSIZE-IN-Q)
  ("ldrsw" #xb8800400 #xffe00400 :ldst-imm9 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-X32 0)
  ("strb" #x39000000 #xffc00000 :ldst-pos OP-STRB-POS :CORE '(:Rt :ADDR-UIMM12) QL-LDST-W8 0)
  ("ldrb" #x39400000 #xffc00000 :ldst-pos OP-LDRB-POS :CORE '(:Rt :ADDR-UIMM12) QL-LDST-W8 0)
  ("ldrsb" #x39800000 #xff800000 :ldst-pos OP-LDRSB-POS :CORE '(:Rt :ADDR-UIMM12) QL-LDST-R8 F-LDS-SIZE)
  ("str" #x3d000000 #x3f400000 :ldst-pos OP-STRF-POS :CORE '(:Ft :ADDR-UIMM12) QL-LDST-FP 0)
  ("ldr" #x3d400000 #x3f400000 :ldst-pos OP-LDRF-POS :CORE '(:Ft :ADDR-UIMM12) QL-LDST-FP 0)
  ("strh" #x79000000 #xffc00000 :ldst-pos OP-STRH-POS :CORE '(:Rt :ADDR-UIMM12) QL-LDST-W16 0)
  ("ldrh" #x79400000 #xffc00000 :ldst-pos OP-LDRH-POS :CORE '(:Rt :ADDR-UIMM12) QL-LDST-W16 0)
  ("ldrsh" #x79800000 #xff800000 :ldst-pos OP-LDRSH-POS :CORE '(:Rt :ADDR-UIMM12) QL-LDST-R16 F-LDS-SIZE)
  ("str" #xb9000000 #xbfc00000 :ldst-pos OP-STR-POS :CORE '(:Rt :ADDR-UIMM12) QL-LDST-R F-GPRSIZE-IN-Q)
  ("ldr" #xb9400000 #xbfc00000 :ldst-pos OP-LDR-POS :CORE '(:Rt :ADDR-UIMM12) QL-LDST-R F-GPRSIZE-IN-Q)
  ("ldrsw" #xb9800000 #xffc00000 :ldst-pos OP-LDRSW-POS :CORE '(:Rt :ADDR-UIMM12) QL-LDST-X32 0)
  ("prfm" #xf9800000 #xffc00000 :ldst-pos OP-PRFM-POS :CORE '(:PRFOP :ADDR-UIMM12) QL-LDST-PRFM 0)
  ("strb" #x38200800 #xffe00c00 :ldst-regoff 0 :CORE '(:Rt :ADDR-REGOFF) QL-LDST-W8 0)
  ("ldrb" #x38600800 #xffe00c00 :ldst-regoff 0 :CORE '(:Rt :ADDR-REGOFF) QL-LDST-W8 0)
  ("ldrsb" #x38a00800 #xffa00c00 :ldst-regoff 0 :CORE '(:Rt :ADDR-REGOFF) QL-LDST-R8 F-LDS-SIZE)
  ("str" #x3c200800 #x3f600c00 :ldst-regoff 0 :CORE '(:Ft :ADDR-REGOFF) QL-LDST-FP 0)
  ("ldr" #x3c600800 #x3f600c00 :ldst-regoff 0 :CORE '(:Ft :ADDR-REGOFF) QL-LDST-FP 0)
  ("strh" #x78200800 #xffe00c00 :ldst-regoff 0 :CORE '(:Rt :ADDR-REGOFF) QL-LDST-W16 0)
  ("ldrh" #x78600800 #xffe00c00 :ldst-regoff 0 :CORE '(:Rt :ADDR-REGOFF) QL-LDST-W16 0)
  ("ldrsh" #x78a00800 #xffa00c00 :ldst-regoff 0 :CORE '(:Rt :ADDR-REGOFF) QL-LDST-R16 F-LDS-SIZE)
  ("str" #xb8200800 #xbfe00c00 :ldst-regoff 0 :CORE '(:Rt :ADDR-REGOFF) QL-LDST-R F-GPRSIZE-IN-Q)
  ("ldr" #xb8600800 #xbfe00c00 :ldst-regoff 0 :CORE '(:Rt :ADDR-REGOFF) QL-LDST-R F-GPRSIZE-IN-Q)
  ("ldrsw" #xb8a00800 #xffe00c00 :ldst-regoff 0 :CORE '(:Rt :ADDR-REGOFF) QL-LDST-X32 0)
  ("prfm" #xf8a00800 #xffe00c00 :ldst-regoff 0 :CORE '(:PRFOP :ADDR-REGOFF) QL-LDST-PRFM 0)
  ("sttrb" #x38000800 #xffe00c00 :ldst-unpriv 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W8 0)
  ("ldtrb" #x38400800 #xffe00c00 :ldst-unpriv 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W8 0)
  ("ldtrsb" #x38800800 #xffa00c00 :ldst-unpriv 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R8 F-LDS-SIZE)
  ("sttrh" #x78000800 #xffe00c00 :ldst-unpriv 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W16 0)
  ("ldtrh" #x78400800 #xffe00c00 :ldst-unpriv 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W16 0)
  ("ldtrsh" #x78800800 #xffa00c00 :ldst-unpriv 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R16 F-LDS-SIZE)
  ("sttr" #xb8000800 #xbfe00c00 :ldst-unpriv 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R F-GPRSIZE-IN-Q)
  ("ldtr" #xb8400800 #xbfe00c00 :ldst-unpriv 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R F-GPRSIZE-IN-Q)
  ("ldtrsw" #xb8800800 #xffe00c00 :ldst-unpriv 0 :CORE '(:Rt :ADDR-SIMM9) QL-LDST-X32 0)
  ("sturb" #x38000000 #xffe00c00 :ldst-unscaled OP-STURB :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W8 F-HAS-ALIAS)
  ("ldurb" #x38400000 #xffe00c00 :ldst-unscaled OP-LDURB :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W8 F-HAS-ALIAS)
  ("strb" #x38000000 #xffe00c00 :ldst-unscaled 0 :CORE '(:Rt :ADDR-SIMM9-2) QL-LDST-W8 F-ALIAS)
  ("ldrb" #x38400000 #xffe00c00 :ldst-unscaled 0 :CORE '(:Rt :ADDR-SIMM9-2) QL-LDST-W8 F-ALIAS)
  ("ldursb" #x38800000 #xffa00c00 :ldst-unscaled OP-LDURSB :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R8 (F-HAS-ALIAS  F-LDS-SIZE))
  ("ldrsb" #x38800000 #xffa00c00 :ldst-unscaled 0 :CORE '(:Rt :ADDR-SIMM9-2) QL-LDST-R8 (F-ALIAS  F-LDS-SIZE))
  ("stur" #x3c000000 #x3f600c00 :ldst-unscaled OP-STURV :CORE '(:Ft :ADDR-SIMM9) QL-LDST-FP F-HAS-ALIAS)
  ("ldur" #x3c400000 #x3f600c00 :ldst-unscaled OP-LDURV :CORE '(:Ft :ADDR-SIMM9) QL-LDST-FP F-HAS-ALIAS)
  ("str" #x3c000000 #x3f600c00 :ldst-unscaled 0 :CORE '(:Ft :ADDR-SIMM9-2) QL-LDST-FP F-ALIAS)
  ("ldr" #x3c400000 #x3f600c00 :ldst-unscaled 0 :CORE '(:Ft :ADDR-SIMM9-2) QL-LDST-FP F-ALIAS)
  ("sturh" #x78000000 #xffe00c00 :ldst-unscaled OP-STURH :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W16 F-HAS-ALIAS)
  ("ldurh" #x78400000 #xffe00c00 :ldst-unscaled OP-LDURH :CORE '(:Rt :ADDR-SIMM9) QL-LDST-W16 F-HAS-ALIAS)
  ("strh" #x78000000 #xffe00c00 :ldst-unscaled 0 :CORE '(:Rt :ADDR-SIMM9-2) QL-LDST-W16 F-ALIAS)
  ("ldrh" #x78400000 #xffe00c00 :ldst-unscaled 0 :CORE '(:Rt :ADDR-SIMM9-2) QL-LDST-W16 F-ALIAS)
  ("ldursh" #x78800000 #xffa00c00 :ldst-unscaled OP-LDURSH :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R16 (F-HAS-ALIAS  F-LDS-SIZE))
  ("ldrsh" #x78800000 #xffa00c00 :ldst-unscaled 0 :CORE '(:Rt :ADDR-SIMM9-2) QL-LDST-R16 (F-ALIAS  F-LDS-SIZE))
  ("stur" #xb8000000 #xbfe00c00 :ldst-unscaled OP-STUR :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R (F-HAS-ALIAS  F-GPRSIZE-IN-Q))
  ("ldur" #xb8400000 #xbfe00c00 :ldst-unscaled OP-LDUR :CORE '(:Rt :ADDR-SIMM9) QL-LDST-R (F-HAS-ALIAS  F-GPRSIZE-IN-Q))
  ("str" #xb8000000 #xbfe00c00 :ldst-unscaled 0 :CORE '(:Rt :ADDR-SIMM9-2) QL-LDST-R (F-ALIAS  F-GPRSIZE-IN-Q))
  ("ldr" #xb8400000 #xbfe00c00 :ldst-unscaled 0 :CORE '(:Rt :ADDR-SIMM9-2) QL-LDST-R (F-ALIAS  F-GPRSIZE-IN-Q))
  ("ldursw" #xb8800000 #xffe00c00 :ldst-unscaled OP-LDURSW :CORE '(:Rt :ADDR-SIMM9) QL-LDST-X32 F-HAS-ALIAS)
  ("ldrsw" #xb8800000 #xffe00c00 :ldst-unscaled 0 :CORE '(:Rt :ADDR-SIMM9-2) QL-LDST-X32 F-ALIAS)
  ("prfum" #xf8800000 #xffe00c00 :ldst-unscaled OP-PRFUM :CORE '(:PRFOP :ADDR-SIMM9) QL-LDST-PRFM F-HAS-ALIAS)
  ("prfm" #xf8800000 #xffe00c00 :ldst-unscaled 0 :CORE '(:PRFOP :ADDR-SIMM9-2) QL-LDST-PRFM F-ALIAS)
  ("stxrb" #x8007c00 #xffe0fc00 :ldstexcl 0 :CORE '(:Rs :Rt :ADDR-SIMPLE) QL-W2-LDST-EXC 0)
  ("stlxrb" #x800fc00 #xffe0fc00 :ldstexcl 0 :CORE '(:Rs :Rt :ADDR-SIMPLE) QL-W2-LDST-EXC 0)
  ("ldxrb" #x85f7c00 #xfffffc00 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-W1-LDST-EXC 0)
  ("ldaxrb" #x85ffc00 #xfffffc00 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-W1-LDST-EXC 0)
  ("stlrb" #x89ffc00 #xfffffc00 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-W1-LDST-EXC 0)
  ("ldarb" #x8dffc00 #xfffffc00 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-W1-LDST-EXC 0)
  ("stxrh" #x48007c00 #xfffffc00 :ldstexcl 0 :CORE '(:Rs :Rt :ADDR-SIMPLE) QL-W2-LDST-EXC 0)
  ("stlxrh" #x4800fc00 #xfffffc00 :ldstexcl 0 :CORE '(:Rs :Rt :ADDR-SIMPLE) QL-W2-LDST-EXC 0)
  ("ldxrh" #x485f7c00 #xfffffc00 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-W1-LDST-EXC 0)
  ("ldaxrh" #x485ffc00 #xfffffc00 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-W1-LDST-EXC 0)
  ("stlrh" #x489ffc00 #xfffffc00 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-W1-LDST-EXC 0)
  ("ldarh" #x48dffc00 #xfffffc00 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-W1-LDST-EXC 0)
  ("stxr" #x88007c00 #xbfe0fc00 :ldstexcl 0 :CORE '(:Rs :Rt :ADDR-SIMPLE) QL-R2-LDST-EXC F-GPRSIZE-IN-Q)
  ("stlxr" #x8800fc00 #xbfe0fc00 :ldstexcl 0 :CORE '(:Rs :Rt :ADDR-SIMPLE) QL-R2-LDST-EXC F-GPRSIZE-IN-Q)
  ("stxp" #x88200000 #xbfe0fc00 :ldstexcl 0 :CORE '(:Rs :Rt :Rt2 :ADDR-SIMPLE) QL-R3-LDST-EXC F-GPRSIZE-IN-Q)
  ("stlxp" #x88208000 #xbfe08000 :ldstexcl 0 :CORE '(:Rs :Rt :Rt2 :ADDR-SIMPLE) QL-R3-LDST-EXC F-GPRSIZE-IN-Q)
  ("ldxr" #x885f7c00 #xbfe08000 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-R1NIL F-GPRSIZE-IN-Q)
  ("ldaxr" #x885ffc00 #xbfe0fc00 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-R1NIL F-GPRSIZE-IN-Q)
  ("ldxp" #x887f0000 #xbfe08000 :ldstexcl 0 :CORE '(:Rt :Rt2 :ADDR-SIMPLE) QL-R2NIL F-GPRSIZE-IN-Q)
  ("ldaxp" #x887f8000 #xbfe08000 :ldstexcl 0 :CORE '(:Rt :Rt2 :ADDR-SIMPLE) QL-R2NIL F-GPRSIZE-IN-Q)
  ("stlr" #x889ffc00 #xbfe08000 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-R1NIL F-GPRSIZE-IN-Q)
  ("ldar" #x88dffc00 #xbfe08000 :ldstexcl 0 :CORE '(:Rt :ADDR-SIMPLE) QL-R1NIL F-GPRSIZE-IN-Q)
  ("stnp" #x28000000 #x7fc00000 :ldstnapair-offs 0 :CORE '(:Rt :Rt2 :ADDR-SIMM7) QL-LDST-PAIR-R F-SF)
  ("ldnp" #x28400000 #x7fc00000 :ldstnapair-offs 0 :CORE '(:Rt :Rt2 :ADDR-SIMM7) QL-LDST-PAIR-R F-SF)
  ("stnp" #x2c000000 #x3fc00000 :ldstnapair-offs 0 :CORE '(:Ft :Ft2 :ADDR-SIMM7) QL-LDST-PAIR-FP 0)
  ("ldnp" #x2c400000 #x3fc00000 :ldstnapair-offs 0 :CORE '(:Ft :Ft2 :ADDR-SIMM7) QL-LDST-PAIR-FP 0)
  ("stp" #x29000000 #x7ec00000 :ldstpair-off 0 :CORE '(:Rt :Rt2 :ADDR-SIMM7) QL-LDST-PAIR-R F-SF)
  ("ldp" #x29400000 #x7ec00000 :ldstpair-off 0 :CORE '(:Rt :Rt2 :ADDR-SIMM7) QL-LDST-PAIR-R F-SF)
  ("stp" #x2d000000 #x3fc00000 :ldstpair-off 0 :CORE '(:Ft :Ft2 :ADDR-SIMM7) QL-LDST-PAIR-FP 0)
  ("ldp" #x2d400000 #x3fc00000 :ldstpair-off 0 :CORE '(:Ft :Ft2 :ADDR-SIMM7) QL-LDST-PAIR-FP 0)
  ("ldpsw" #x69400000 #xffc00000 :ldstpair-off 0 :CORE '(:Rt :Rt2 :ADDR-SIMM7) QL-LDST-PAIR-X32 0)
  ("stp" #x28800000 #x7ec00000 :ldstpair-indexed 0 :CORE '(:Rt :Rt2 :ADDR-SIMM7) QL-LDST-PAIR-R F-SF)
  ("ldp" #x28c00000 #x7ec00000 :ldstpair-indexed 0 :CORE '(:Rt :Rt2 :ADDR-SIMM7) QL-LDST-PAIR-R F-SF)
  ("stp" #x2c800000 #x3ec00000 :ldstpair-indexed 0 :CORE '(:Ft :Ft2 :ADDR-SIMM7) QL-LDST-PAIR-FP 0)
  ("ldp" #x2cc00000 #x3ec00000 :ldstpair-indexed 0 :CORE '(:Ft :Ft2 :ADDR-SIMM7) QL-LDST-PAIR-FP 0)
  ("ldpsw" #x68c00000 #xfec00000 :ldstpair-indexed 0 :CORE '(:Rt :Rt2 :ADDR-SIMM7) QL-LDST-PAIR-X32 0)
  ("ldr" #x18000000 #xbf000000 :loadlit OP-LDR-LIT :CORE '(:Rt :ADDR-PCREL19) QL-R-PCREL F-GPRSIZE-IN-Q)
  ("ldr" #x1c000000 #x3f000000 :loadlit OP-LDRV-LIT :CORE '(:Ft :ADDR-PCREL19) QL-FP-PCREL 0)
  ("ldrsw" #x98000000 #xff000000 :loadlit OP-LDRSW-LIT :CORE '(:Rt :ADDR-PCREL19) QL-X-PCREL 0)
  ("prfm" #xd8000000 #xff000000 :loadlit OP-PRFM-LIT :CORE '(:PRFOP :ADDR-PCREL19) QL-PRFM-PCREL 0)
  ("and" #x12000000 #x7f800000 :log-imm 0 :CORE '(:Rd-SP :Rn :LIMM) QL-R2NIL (F-HAS-ALIAS  F-SF))
  ("bic" #x12000000 #x7f800000 :log-imm OP-BIC :CORE '(:Rd-SP :Rn :LIMM) QL-R2NIL ((F-ALIAS  F-PSEUDO)  F-SF))
  ("orr" #x32000000 #x7f800000 :log-imm 0 :CORE '(:Rd-SP :Rn :LIMM) QL-R2NIL (F-HAS-ALIAS  F-SF))
  ("mov" #x320003e0 #x7f8003e0 :log-imm OP-MOV-IMM-LOG :CORE '(:Rd-SP :IMM-MOV) QL-R1NIL (((F-ALIAS  F-P1)  F-SF)  F-CONV))
  ("eor" #x52000000 #x7f800000 :log-imm 0 :CORE '(:Rd-SP :Rn :LIMM) QL-R2NIL F-SF)
  ("ands" #x72000000 #x7f800000 :log-imm 0 :CORE '(:Rd :Rn :LIMM) QL-R2NIL (F-HAS-ALIAS  F-SF))
  ("tst" #x7200001f #x7f80001f :log-imm 0 :CORE '(:Rn :LIMM) QL-R1NIL (F-ALIAS  F-SF))
  ("and" #xa000000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) 0)
  ("and" #x8a000000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) 0)
  ("bic" #xa200000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) 0)
  ("bic" #x8a200000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) 0)
  ("orr" #x2a000000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) F-HAS-ALIAS)
  ("orr" #xaa000000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) F-HAS-ALIAS)

  ("mov" #x2a0003e0 #xff2003e0 :log-shift 0 :CORE '(:Rd :Rm) '(:w :w) F-ALIAS)
  ("mov" #xaa0003e0 #xff2003e0 :log-shift 0 :CORE '(:Rd :Rm) '(:x :x) F-ALIAS)
  ("uxtw" #x2a0003e0 #x7f2003e0 :log-shift OP-UXTW :CORE '(:Rd :Rm) QL-I2SAMEW (F-ALIAS  F-PSEUDO))
  ("orn" #x2a200000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) F-HAS-ALIAS)
  ("orn" #xaa200000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) F-HAS-ALIAS)

  ("mvn" #x2a2003e0 #x7f2003e0 :log-shift 0 :CORE '(:Rd :Rm-SFT) QL-I2SAMER (F-ALIAS  F-SF))
  ("eor" #x4a000000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) 0)
  ("eor" #xca000000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) 0)
  ("eon" #x4a200000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) 0)
  ("eon" #xca200000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT)
   (:x :x :x-shift) 0)
  ("ands" #x6a000000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) F-HAS-ALIAS)
  ("ands" #xea000000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) F-HAS-ALIAS)

  ("tst" #x6a00001f #x7f20001f :log-shift 0 :CORE '(:Rn :Rm-SFT) QL-I2SAMER (F-ALIAS  F-SF))
  ("bics" #x6a200000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:w :w :w-shift) 0)
  ("bics" #xea200000 #xff200000 :log-shift 0 :CORE '(:Rd :Rn :Rm-SFT) '(:x :x :x-shift) 0)
  ("movn" #x12800000 #x7f800000 :movewide OP-MOVN :CORE '(:Rd :HALF) QL-DST-R (F-SF  F-HAS-ALIAS))
  ("mov" #x12800000 #x7f800000 :movewide OP-MOV-IMM-WIDEN :CORE '(:Rd :IMM-MOV) QL-DST-R ((F-SF  F-ALIAS)  F-CONV))
  ("movz" #x52800000 #x7f800000 :movewide OP-MOVZ :CORE '(:Rd :HALF) QL-DST-R (F-SF  F-HAS-ALIAS))
  ("mov" #x52800000 #x7f800000 :movewide OP-MOV-IMM-WIDE :CORE '(:Rd :IMM-MOV) QL-DST-R ((F-SF  F-ALIAS)  F-CONV))
  ("movk" #x72800000 #x7f800000 :movewide OP-MOVK :CORE '(:Rd :HALF) QL-DST-R F-SF)
  ("adr" #x10000000 #x9f000000 :pcreladdr 0 :CORE '(:Rd :ADDR-PCREL21) QL-ADRP 0)
  ("adrp" #x90000000 #x9f000000 :pcreladdr 0 :CORE '(:Rd :ADDR-ADRP) QL-ADRP 0)
  ("msr" #xd500401f #xfff8f01f :ic-system 0 :CORE '(:PSTATEFIELD :UIMM4) () 0)
  ("hint" #xd503201f #xfffff01f :ic-system 0 :CORE '(:UIMM7) () F-HAS-ALIAS)
  ("nop" #xd503201f #xffffffff :ic-system 0 :CORE '() () F-ALIAS)
  ("yield" #xd503203f #xffffffff :ic-system 0 :CORE '() () F-ALIAS)
  ("wfe" #xd503205f #xffffffff :ic-system 0 :CORE '() () F-ALIAS)
  ("wfi" #xd503207f #xffffffff :ic-system 0 :CORE '() () F-ALIAS)
  ("sev" #xd503209f #xffffffff :ic-system 0 :CORE '() () F-ALIAS)
  ("sevl" #xd50320bf #xffffffff :ic-system 0 :CORE '() () F-ALIAS)
  ("clrex" #xd503305f #xfffff0ff :ic-system 0 :CORE '(:UIMM4) () (F-OPD0-OPT  F-DEFAULT) (#xF))
  ("dsb" #xd503309f #xfffff0ff :ic-system 0 :CORE '(:BARRIER) () 0)
  ("dmb" #xd50330bf #xfffff0ff :ic-system 0 :CORE '(:BARRIER) () 0)
  ("isb" #xd50330df #xfffff0ff :ic-system 0 :CORE '(:BARRIER-ISB) () (F-OPD0-OPT  F-DEFAULT) (#xF))
  ("sys" #xd5080000 #xfff80000 :ic-system 0 :CORE OP5 (UIMM3-OP1 :Cn Cm UIMM3-OP2 Rt) QL-SYS ((F-HAS-ALIAS  F-OPD4-OPT)  F-DEFAULT) (#x1F))
  ("at" #xd5080000 #xfff80000 :ic-system 0 :CORE '(:SYSREG-AT :Rt) QL-SRC-X F-ALIAS)
  ("dc" #xd5080000 #xfff80000 :ic-system 0 :CORE '(:SYSREG-DC :Rt) QL-SRC-X F-ALIAS)
  ("ic" #xd5080000 #xfff80000 :ic-system 0 :CORE '(:SYSREG-IC :Rt-SYS) QL-SRC-X ((F-ALIAS  F-OPD1-OPT)  F-DEFAULT) (#x1F))
  ("tlbi" #xd5080000 #xfff80000 :ic-system 0 :CORE '(:SYSREG-TLBI :Rt-SYS) QL-SRC-X ((F-ALIAS  F-OPD1-OPT)  F-DEFAULT) (#x1F))
  ("msr" #xd5100000 #xfff00000 :ic-system 0 :CORE '(:SYSREG :Rt) QL-SRC-X 0)
  ("sysl" #xd5280000 #xfff80000 :ic-system 0 :CORE '(:Rt :UIMM3-OP1 :Cn :Cm :UIMM3-OP2) QL-SYSL 0)
  ("mrs" #xd5300000 #xfff00000 :ic-system 0 :CORE '(:Rt :SYSREG) QL-DST-X 0)
  ("tbz" #x36000000 #x7f000000 :testbranch 0 :CORE '(:Rt :BIT-NUM :ADDR-PCREL14) QL-PCREL-14 0)
  ("tbnz" #x37000000 #x7f000000 :testbranch 0 :CORE '(:Rt :BIT-NUM :ADDR-PCREL14) QL-PCREL-14 0)
  ("beq" #x54000000 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bne" #x54000001 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bcs" #x54000002 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bhs" #x54000002 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bcc" #x54000003 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("blo" #x54000003 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bmi" #x54000004 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bpl" #x54000005 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bvs" #x54000006 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bvc" #x54000007 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bhi" #x54000008 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bls" #x54000009 #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bge" #x5400000a #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("blt" #x5400000b #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("bgt" #x5400000c #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
  ("ble" #x5400000d #xff00001f :condbranch 0 :CORE '(:ADDR-PCREL19) QL-PCREL-NIL (F-ALIAS  F-PSEUDO))
))

(provide "ARM64-ASM")
