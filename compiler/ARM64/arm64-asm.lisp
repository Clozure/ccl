;;;;-*- Mode: Lisp; Package: (ARM64 :use CL) -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ARM64-ARCH"))

(in-package "ARM64")

;;; Various data pertinent to the function being assembled
(defvar *labels* ())
(defvar *constants* ())
(defvar *instructions* ())

(eval-when (:compile-toplevel :load-toplevel :execute)

(defstruct register
  name         ;canonical name
  number       ;as encoded in instruction
  width        ;in bits
  (family :gpr :type (member :gpr :fpr))  ;register file: GPR vs FP/SIMD
  flags)

(defmethod make-load-form ((r register) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots r))

;;; Flags in the register defstruct (there's only one)
(defconstant $rflag-sp 1)               ;r31 role is stack pointer

;;; Given a designator for a register name, figure out everything the
;;; name implies.  Returns (values name number width family flags).
(defun %parse-register-name (designator)
  (let ((name (string-downcase designator)))
    (cond
      ((string= name "sp") (values name 31 64 :gpr $rflag-sp))
      ((string= name "wsp") (values name 31 32 :gpr $rflag-sp))
      ((string= name "xzr") (values name 31 64 :gpr 0))
      ((string= name "wzr") (values name 31 32 :gpr 0))
      (t (multiple-value-bind (width family)
             (ecase (char name 0)
               (#\x (values 64 :gpr))
               (#\w (values 32 :gpr))
               (#\s (values 32 :fpr))
               (#\d (values 64 :fpr))
               (#\q (values 128 :fpr))
               (#\b (values 8 :fpr))
               (#\h (values 16 :fpr)))
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
   (reg d28) (reg d29) (reg d30) (reg d31)
   ;; 128-bit fprs
   (reg q0) (reg q1) (reg q2) (reg q3) (reg q4) (reg q5) (reg q6)
   (reg q7) (reg q8) (reg q9) (reg q10) (reg q11) (reg q12) (reg q13)
   (reg q14) (reg q15) (reg q16) (reg q17) (reg q18) (reg q19) (reg q20)
   (reg q21) (reg q22) (reg q23) (reg q24) (reg q25) (reg q26) (reg q27)
   (reg q28) (reg q29) (reg q30) (reg q31)
   ;; byte view
   (reg b0) (reg b1) (reg b2) (reg b3) (reg b4) (reg b5) (reg b6)
   (reg b7) (reg b8) (reg b9) (reg b10) (reg b11) (reg b12) (reg b13)
   (reg b14) (reg b15) (reg b16) (reg b17) (reg b18) (reg b19) (reg b20)
   (reg b21) (reg b22) (reg b23) (reg b24) (reg b25) (reg b26) (reg b27)
   (reg b28) (reg b29) (reg b30) (reg b31)
   ;; halfword view
   (reg h0) (reg h1) (reg h2) (reg h3) (reg h4) (reg h5) (reg h6)
   (reg h7) (reg h8) (reg h9) (reg h10) (reg h11) (reg h12) (reg h13)
   (reg h14) (reg h15) (reg h16) (reg h17) (reg h18) (reg h19) (reg h20)
   (reg h21) (reg h22) (reg h23) (reg h24) (reg h25) (reg h26) (reg h27)
   (reg h28) (reg h29) (reg h30) (reg h31)))

;;; Constants for indexes into *registers*
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
   d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31
   ;; 128-bit names
   q0  q1  q2  q3  q4  q5  q6  q7  q8  q9  q10 q11 q12 q13 q14 q15
   q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31
   ;; byte names
   b0  b1  b2  b3  b4  b5  b6  b7  b8  b9  b10 b11 b12 b13 b14 b15
   b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31
   ;; halfword names
   h0  h1  h2  h3  h4  h5  h6  h7  h8  h9  h10 h11 h12 h13 h14 h15
   h16 h17 h18 h19 h20 h21 h22 h23 h24 h25 h26 h27 h28 h29 h30 h31)

(defun gpr-ref (number width &optional r31-is-sp)
  (multiple-value-bind (base stack-pointer zero-register)
      (ecase width
        (64 (values x0 sp xzr))
        (32 (values w0 wsp wzr)))
    (svref *registers*
           (cond ((< number 31) (+ base number))
                 (r31-is-sp stack-pointer)
                 (t zero-register)))))

(defun fpr-ref (number width)
  (let ((base (ecase width
                (32 s0)
                (64 d0)
                (128 q0)
                (8 b0)
                (16 h0))))
    (svref *registers* (+ base number))))

(defvar *registers-by-name* (make-hash-table :test #'equalp))

(defun hash-registers ()
  (clrhash *registers-by-name*)
  (dotimes (i (length *registers*))
    (let ((r (svref *registers* i)))
      (setf (gethash (register-name r) *registers-by-name*) r))))

(hash-registers)

;;; For the disassembler: an alist of the form (("x0" . "imm0") ...)
(defparameter *register-alias-names* ())

;;; Add a permanent alias for a register.
(defmacro define-register-alias (alias known)
  (let ((known-entry (gensym)))
    `(let ((,known-entry (gethash ,(string known) *registers-by-name*)))
       (unless ,known-entry
         (error "Register ~a not defined" ',known))
       (setf (gethash ,(string alias) *registers-by-name*) ,known-entry)
       (push (cons ,(string-downcase known) ,(string-downcase alias))
             *register-alias-names*)
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

) ;eval-when

;; Temporary register aliases established via LAP notation
;; (see arm64-lap-equate-form).  This is an alist mapping
;; symbols to register structs.
(defvar *lap-register-equates* ())

;;; Look up a register by name.
(defun lookup-register (name)
  (or (gethash (string name) *registers-by-name*)
      (cdr (assoc name *lap-register-equates*))))

(defun need-register (name)
  (or (lookup-register name)
      (error "No register named ~a" name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
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
)

(defmacro encode-instruction-flags (flags)
  (%encode-instruction-flags flags))

(defstruct instruction-template
  name
  operand-specs
  base-opcode
  mask            ;for disassembly: masks out variable parts of instruction
  (flags 0)
  ordinal)        ;this template's own index in *instruction-templates*

(defmacro define-instruction-template (name operand-specs base-opcode mask
                                       &rest flags)
  `(make-instruction-template :name ,(string-downcase name)
                              :operand-specs ',operand-specs
                              :base-opcode ,base-opcode
                              :mask ,mask
                              :flags (encode-instruction-flags ,flags)))

;;; masks for various instruction groups

(defconstant $rm/rn/rd-mask #xffe0fc00)
(defconstant $addsub-imm-mask #xff000000)
(defconstant $addsub-shift-mask #xff200000)
(defconstant $addsub-ext-mask #xffe00000)
(defconstant $log-shift-mask #xff200000)
(defconstant $ldst-pos-mask #xffc00000)  ;load/store register, unsigned immediate
(defconstant $ldst-unscaled-mask #xffe00c00) ;load/store register, unscaled immediate
(defconstant $ldst-regoff-mask #xffe00c00) ;load/store register, register offset
(defconstant $ldstpair-mask #xffc00000) ;load/store register pair (all index modes)
(defconstant $ldst-excl-st-mask #xffe0fc00) ;store exclusive (free Rs/Rn/Rt)
(defconstant $ldst-excl-ld-mask #xfffffc00) ;load exclusive, stlr/ldar (free Rn/Rt)
(defconstant $ldst-excl-stp-mask #xffe08000) ;store-exclusive pair (free Rs/Rt2/Rn/Rt)
(defconstant $ldst-excl-ldp-mask #xffff8000) ;load-exclusive pair (free Rt2/Rn/Rt)
(defconstant $lse-atomic-mask #xffe0fc00) ;LSE atomics/cas/swp (free Rs/Rn/Rt)
(defconstant $fp-dp2src-mask #xffe0fc00) ;FP data-processing 2-source (fadd/fmul/...)
(defconstant $fp-dp1src-mask #xfffffc00) ;FP data-processing 1-source (fabs/fneg/fcvt/...)
(defconstant $fp-dp3src-mask #xffe08000) ;FP data-processing 3-source (fmadd/fmsub/...)
(defconstant $fp-cmp-mask #xffe0fc1f) ;FP compare, register form (free Rm/Rn)
(defconstant $fp-cmp-zero-mask #xfffffc1f) ;FP compare against #0.0 (free Rn only)
(defconstant $fp-cvt-mask #xfffffc00) ;FP<->int convert, fmov gpr<->fpr (free Rn/Rd)
(defconstant $fp-imm-mask #xffe01fe0) ;FP move immediate (free imm8 @ 20:13, Rd)
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

(progn
;;; LSE atomics (FEAT_LSE), C6.2.  These families are large but perfectly
;;; regular -- size @ 31:30, the ordering bits, and an operation selector --
;;; so we generate them rather than spell out ~200 near-identical templates.
;;; They all share the (Rs, Rt, [Xn]) shape the :rs role and :mem-base form
;;; already provide; ST<op> is the Rt=31 alias of LD<op>.  CASP (register-pair
;;; compare-and-swap) is omitted: it needs consecutive even/odd register-pair
;;; validation we don't have yet.
(defun lse-atomic-templates ()
  (let ((templates '())
        ;; size code @ 31:30, mnemonic size suffix, operand width class
        (sizes '((0 "b" :w) (1 "h" :w) (2 "" :w) (3 "" :x)))
        ;; ordering suffix and (A R) for the load/swap variants
        (ld-orders '(("" 0 0) ("a" 1 0) ("l" 0 1) ("al" 1 1)))
        ;; o3=0 read-modify-write ops: mnemonic stem and opc @ 14:12
        (ld-ops '(("add" 0) ("clr" 1) ("eor" 2) ("set" 3)
                  ("smax" 4) ("smin" 5) ("umax" 6) ("umin" 7))))
    (flet ((emit (name specs base mask &optional alias)
             (push (make-instruction-template
                    :name (string-downcase name)
                    :operand-specs specs
                    :base-opcode base
                    :mask mask
                    :flags (%encode-instruction-flags (and alias :alias)))
                   templates))
           ;; atomic memory op: size 111000 A R 1 Rs o3 opc 00 Rn Rt
           (atomic (size a r o3 opc)
             (logior (ash size 30) (ash #x38 24) (ash a 23) (ash r 22)
                     (ash 1 21) (ash o3 15) (ash opc 12)))
           ;; compare-and-swap: size 001000 1 L 1 Rs o0 11111 Rn Rt
           (cas-base (size l o0)
             (logior (ash size 30) (ash #x08 24) (ash 1 23) (ash l 22)
                     (ash 1 21) (ash o0 15) (ash #x1f 10))))
      (dolist (sz sizes)
        (destructuring-bind (size suff wclass) sz
          (let ((rs (list :rs wclass))
                (rt (list :rt wclass))
                (mem '(:mem-base (:base :x/sp))))
            (dolist (op ld-ops)
              (destructuring-bind (stem opc) op
                ;; LD<op>: read-modify-write, prior value loaded into Rt
                (dolist (ord ld-orders)
                  (destructuring-bind (osuff a r) ord
                    (emit (format nil "ld~a~a~a" stem osuff suff)
                          (list rs rt mem)
                          (atomic size a r 0 opc) $lse-atomic-mask)))
                ;; ST<op>/ST<op>L: alias of LD<op>{L} with Rt=31 (no result)
                (dolist (ord '(("" 0) ("l" 1)))
                  (destructuring-bind (osuff r) ord
                    (emit (format nil "st~a~a~a" stem osuff suff)
                          (list rs mem)
                          (logior (atomic size 0 r 0 opc) #x1f) 0 t)))))
            ;; SWP: o3=1, opc=0; value in Rs swapped with memory into Rt
            (dolist (ord ld-orders)
              (destructuring-bind (osuff a r) ord
                (emit (format nil "swp~a~a" osuff suff)
                      (list rs rt mem)
                      (atomic size a r 1 0) $lse-atomic-mask)))
            ;; CAS: compare value in Rs (read-write), new value in Rt
            (dolist (ord '(("" 0 0) ("a" 1 0) ("l" 0 1) ("al" 1 1)))
              (destructuring-bind (osuff l o0) ord
                (emit (format nil "cas~a~a" osuff suff)
                      (list rs rt mem)
                      (cas-base size l o0) $lse-atomic-mask)))))))
    (nreverse templates)))

#+nil
(defparameter *augmented-templates*
  (concatenate 'vector *instruction-templates* (lse-atomic-templates))))

;;; Note that order matters here: when searching for a  template,
;;; we pick the first one that matches.  Therefore, keep the preferred
;;; forms first when needed.

(defparameter *instruction-templates*
  (vector
   ;; nullary UUOs (uuo format #b111)
   (def uuo-alloc-trap () (logior (ash 0 3) #x7) #xffffffff)
   (def uuo-error-wrong-nargs () (logior (ash 1 3) #x7)  #xffffffff)
   (def uuo-gc-trap () (logior (ash 2 3) #x7) #xffffffff)
   (def uuo-debug-trap () (logior (ash 3 3) #x7) #xffffffff)

   ;; unary UUOs (uuo format #b001)
   (def uuo-error-too-few-args () (logior (ash 0 3) #x1)  #xffffffff)
   (def uuo-error-too-many-args () (logior (ash 1 3) #x1)  #xffffffff)
   (def uuo-error-wrong-number-of-args () (logior (ash 2 3) #x1) #xffffffff)

   ;; binary UUOs (uuo format #b010)

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
   (def mov ((:rd :wsp) (:rn :w/sp)) #x11000000 0 :alias)
   (def mov ((:rd :w/sp) (:rn :wsp)) #x11000000 0 :alias)
   (def mov ((:rd :sp) (:rn :x/sp)) #x91000000 0 :alias)
   (def mov ((:rd :x/sp) (:rn :sp)) #x91000000 0 :alias)
   (def adds ((:rd :w) (:rn :w/sp) :aimm) #x31000000 #xff000000)
   (def adds ((:rd :x) (:rn :x/sp) :aimm) #xb1000000 #xff000000)
   (def cmn ((:rn :w/sp) :aimm) #x3100001f 0 :alias)
   (def cmn ((:rn :x/sp) :aimm) #xb100001f 0 :alias)
   (def sub ((:rd :w/sp) (:rn :w/sp) :aimm) #x51000000 #xff000000)
   (def sub ((:rd :x/sp) (:rn :x/sp) :aimm) #xd1000000 #xff000000)
   (def subs ((:rd :w) (:rn :w/sp) :aimm) #x71000000 #xff000000)
   (def subs ((:rd :x) (:rn :x/sp) :aimm) #xf1000000 #xff000000)
   (def cmp ((:rn :w/sp) :aimm) #x7100001f 0 :alias)
   (def cmp ((:rn :x/sp) :aimm) #xf100001f 0 :alias)

   ;; Move (immediate) aliases.  Listed in order of preference.
   (def mov ((:rd :w) :movw-mov-w)  #x52800000 0 :alias)
   (def mov ((:rd :x) :movw-mov-x)  #xd2800000 0 :alias)
   (def mov ((:rd :w) :movw-movn-w) #x12800000 0 :alias)
   (def mov ((:rd :x) :movw-movn-x) #x92800000 0 :alias)
   (def mov ((:rd :w/sp) :limm) #x320003e0 0 :alias)
   (def mov ((:rd :x/sp) :limm) #xb20003e0 0 :alias)

   ;; Move wide (immediate)
   (def movn ((:rd :w) :movw-w) #x12800000 $movewide-mask)
   (def movn ((:rd :x) :movw-x) #x92800000 $movewide-mask)
   (def movz ((:rd :w) :movw-w) #x52800000 $movewide-mask)
   (def movz ((:rd :x) :movw-x) #xd2800000 $movewide-mask)
   (def movk ((:rd :w) :movw-w) #x72800000 $movewide-mask)
   (def movk ((:rd :x) :movw-x) #xf2800000 $movewide-mask)

   ;; Logical (immediate)
   (def and ((:rd :w/sp) (:rn :w) :limm) #x12000000 #xff800000)
   (def and ((:rd :x/sp) (:rn :x) :limm) #x92000000 #xff800000)
   (def orr ((:rd :w/sp) (:rn :w) :limm) #x32000000 #xff800000)
   (def orr ((:rd :x/sp) (:rn :x) :limm) #xb2000000 #xff800000)
   (def eor ((:rd :w/sp) (:rn :w) :limm) #x52000000 #xff800000)
   (def eor ((:rd :x/sp) (:rn :x) :limm) #xd2000000 #xff800000)
   (def ands ((:rd :w) (:rn :w) :limm) #x72000000 #xff800000)
   (def ands ((:rd :x) (:rn :x) :limm) #xf2000000 #xff800000)
   (def tst ((:rn :w) :limm) #x7200001f 0 :alias)
   (def tst ((:rn :x) :limm) #xf200001f 0 :alias)

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
   (def b.hs ((:label :b19)) #x54000002 0 :alias)
   (def b.cc ((:label :b19)) #x54000003 #xff00001f)
   (def b.lo ((:label :b19)) #x54000003 0 :alias)
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

   ;; This template provides a way to write a conditional branch whose
   ;; condition is an operand, (:? cc) or (:~ cc), rather than baked
   ;; into the mnemonic.  The condition fills (byte 4 0); the label
   ;; fills (byte 19 5).
   (def b.cond (:cond-b (:label :b19)) #x54000000 0 :alias)

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

   ;; Barriers and system register access
   ;; The bare form defaults to SY; an explicit (:$ option) selects a
   ;; domain (e.g. 11 = ish, 10 = ishst).  Named options aren't implemented.
   ;; The bare forms bake in CRm=15; the (:baropt) forms accept any CRm.
   (def clrex () #xd5033f5f #xffffffff)
   (def clrex (:baropt) #xd503305f #xfffff0ff)
   (def dmb () #xd5033fbf #xffffffff)
   (def dmb (:baropt) #xd50330bf #xfffff0ff)
   (def dsb () #xd5033f9f #xffffffff)
   (def dsb (:baropt) #xd503309f #xfffff0ff)
   (def isb () #xd5033fdf #xffffffff)
   (def isb (:baropt) #xd50330df #xfffff0ff)

   ;; Move to/from a system register (e.g. fpsr/fpcr for FP-exception
   ;; polling).  The 15-bit op0:op1:CRn:CRm:op2 field @ 19:5 names the
   ;; register; see *system-registers*.
   (def mrs ((:rt :x) :sysreg) #xd5300000 #xfff00000)
   (def msr (:sysreg (:rt :x)) #xd5100000 #xfff00000)

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

   ;; Load/store exclusive pair
   (def stxp ((:rs :w) (:rt :w) (:rt2 :w) (:mem-base (:base :x/sp))) #x88200000 $ldst-excl-stp-mask)
   (def stxp ((:rs :w) (:rt :x) (:rt2 :x) (:mem-base (:base :x/sp))) #xc8200000 $ldst-excl-stp-mask)
   (def stlxp ((:rs :w) (:rt :w) (:rt2 :w) (:mem-base (:base :x/sp))) #x88208000 $ldst-excl-stp-mask)
   (def stlxp ((:rs :w) (:rt :x) (:rt2 :x) (:mem-base (:base :x/sp))) #xc8208000 $ldst-excl-stp-mask)
   (def ldxp ((:rt :w) (:rt2 :w) (:mem-base (:base :x/sp))) #x887f0000 $ldst-excl-ldp-mask)
   (def ldxp ((:rt :x) (:rt2 :x) (:mem-base (:base :x/sp))) #xc87f0000 $ldst-excl-ldp-mask)
   (def ldaxp ((:rt :w) (:rt2 :w) (:mem-base (:base :x/sp))) #x887f8000 $ldst-excl-ldp-mask)
   (def ldaxp ((:rt :x) (:rt2 :x) (:mem-base (:base :x/sp))) #xc87f8000 $ldst-excl-ldp-mask)

   ;; Load/store exclusive register
   (def stxrb ((:rs :w) (:rt :w) (:mem-base (:base :x/sp))) #x08007c00 $ldst-excl-st-mask)
   (def stxrh ((:rs :w) (:rt :w) (:mem-base (:base :x/sp))) #x48007c00 $ldst-excl-st-mask)
   (def stxr ((:rs :w) (:rt :w) (:mem-base (:base :x/sp))) #x88007c00 $ldst-excl-st-mask)
   (def stxr ((:rs :w) (:rt :x) (:mem-base (:base :x/sp))) #xc8007c00 $ldst-excl-st-mask)
   (def stlxrb ((:rs :w) (:rt :w) (:mem-base (:base :x/sp))) #x0800fc00 $ldst-excl-st-mask)
   (def stlxrh ((:rs :w) (:rt :w) (:mem-base (:base :x/sp))) #x4800fc00 $ldst-excl-st-mask)
   (def stlxr ((:rs :w) (:rt :w) (:mem-base (:base :x/sp))) #x8800fc00 $ldst-excl-st-mask)
   (def stlxr ((:rs :w) (:rt :x) (:mem-base (:base :x/sp))) #xc800fc00 $ldst-excl-st-mask)
   (def ldxrb ((:rt :w) (:mem-base (:base :x/sp))) #x085f7c00 $ldst-excl-ld-mask)
   (def ldxrh ((:rt :w) (:mem-base (:base :x/sp))) #x485f7c00 $ldst-excl-ld-mask)
   (def ldxr ((:rt :w) (:mem-base (:base :x/sp))) #x885f7c00 $ldst-excl-ld-mask)
   (def ldxr ((:rt :x) (:mem-base (:base :x/sp))) #xc85f7c00 $ldst-excl-ld-mask)
   (def ldaxrb ((:rt :w) (:mem-base (:base :x/sp))) #x085ffc00 $ldst-excl-ld-mask)
   (def ldaxrh ((:rt :w) (:mem-base (:base :x/sp))) #x485ffc00 $ldst-excl-ld-mask)
   (def ldaxr ((:rt :w) (:mem-base (:base :x/sp))) #x885ffc00 $ldst-excl-ld-mask)
   (def ldaxr ((:rt :x) (:mem-base (:base :x/sp))) #xc85ffc00 $ldst-excl-ld-mask)

   ;; Load/store ordered
   (def stlrb ((:rt :w) (:mem-base (:base :x/sp))) #x089ffc00 $ldst-excl-ld-mask)
   (def stlrh ((:rt :w) (:mem-base (:base :x/sp))) #x489ffc00 $ldst-excl-ld-mask)
   (def stlr ((:rt :w) (:mem-base (:base :x/sp))) #x889ffc00 $ldst-excl-ld-mask)
   (def stlr ((:rt :x) (:mem-base (:base :x/sp))) #xc89ffc00 $ldst-excl-ld-mask)
   (def ldarb ((:rt :w) (:mem-base (:base :x/sp))) #x08dffc00 $ldst-excl-ld-mask)
   (def ldarh ((:rt :w) (:mem-base (:base :x/sp))) #x48dffc00 $ldst-excl-ld-mask)
   (def ldar ((:rt :w) (:mem-base (:base :x/sp))) #x88dffc00 $ldst-excl-ld-mask)
   (def ldar ((:rt :x) (:mem-base (:base :x/sp))) #xc8dffc00 $ldst-excl-ld-mask)

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

   ;;; Scalar FP load/store.  These are the integer load/store encodings
   ;;; with the SIMD&FP V bit (#x04000000) set and an S/D transfer register;
   ;;; S uses the size=10 (scale 2) base, D the size=11 (scale 3) base.  All
   ;;; the addressing forms, masks, and offset classes are reused unchanged.

   ;; FP load/store register (unsigned immediate)
   (def str ((:rt :s) (:mem-scaled (:base :x/sp) (:imm :uoff2))) #xbd000000 $ldst-pos-mask)
   (def ldr ((:rt :s) (:mem-scaled (:base :x/sp) (:imm :uoff2))) #xbd400000 $ldst-pos-mask)
   (def str ((:rt :d) (:mem-scaled (:base :x/sp) (:imm :uoff3))) #xfd000000 $ldst-pos-mask)
   (def ldr ((:rt :d) (:mem-scaled (:base :x/sp) (:imm :uoff3))) #xfd400000 $ldst-pos-mask)

   ;; FP load/store register (unscaled immediate)
   (def stur ((:rt :s) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #xbc000000 $ldst-unscaled-mask)
   (def ldur ((:rt :s) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #xbc400000 $ldst-unscaled-mask)
   (def stur ((:rt :d) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #xfc000000 $ldst-unscaled-mask)
   (def ldur ((:rt :d) (:mem-unscaled (:base :x/sp) (:imm :simm9))) #xfc400000 $ldst-unscaled-mask)

   ;; FP load/store register (immediate post-indexed)
   (def str ((:rt :s) (:mem-post (:base :x/sp) (:imm :simm9))) #xbc000400 $ldst-unscaled-mask)
   (def ldr ((:rt :s) (:mem-post (:base :x/sp) (:imm :simm9))) #xbc400400 $ldst-unscaled-mask)
   (def str ((:rt :d) (:mem-post (:base :x/sp) (:imm :simm9))) #xfc000400 $ldst-unscaled-mask)
   (def ldr ((:rt :d) (:mem-post (:base :x/sp) (:imm :simm9))) #xfc400400 $ldst-unscaled-mask)

   ;; FP load/store register (immediate pre-indexed)
   (def str ((:rt :s) (:mem-pre (:base :x/sp) (:imm :simm9))) #xbc000c00 $ldst-unscaled-mask)
   (def ldr ((:rt :s) (:mem-pre (:base :x/sp) (:imm :simm9))) #xbc400c00 $ldst-unscaled-mask)
   (def str ((:rt :d) (:mem-pre (:base :x/sp) (:imm :simm9))) #xfc000c00 $ldst-unscaled-mask)
   (def ldr ((:rt :d) (:mem-pre (:base :x/sp) (:imm :simm9))) #xfc400c00 $ldst-unscaled-mask)

   ;; FP load/store register (register offset)
   (def str ((:rt :s) (:mem-regoff (:base :x/sp) (:index :regoff2))) #xbc200800 $ldst-regoff-mask)
   (def ldr ((:rt :s) (:mem-regoff (:base :x/sp) (:index :regoff2))) #xbc600800 $ldst-regoff-mask)
   (def str ((:rt :d) (:mem-regoff (:base :x/sp) (:index :regoff3))) #xfc200800 $ldst-regoff-mask)
   (def ldr ((:rt :d) (:mem-regoff (:base :x/sp) (:index :regoff3))) #xfc600800 $ldst-regoff-mask)

   ;; FP load/store pair (offset, pre-indexed, post-indexed)
   (def stp ((:rt :s) (:rt2 :s) (:mem-scaled (:base :x/sp) (:imm :poff2))) #x2d000000 $ldstpair-mask)
   (def ldp ((:rt :s) (:rt2 :s) (:mem-scaled (:base :x/sp) (:imm :poff2))) #x2d400000 $ldstpair-mask)
   (def stp ((:rt :s) (:rt2 :s) (:mem-pre (:base :x/sp) (:imm :poff2))) #x2d800000 $ldstpair-mask)
   (def ldp ((:rt :s) (:rt2 :s) (:mem-pre (:base :x/sp) (:imm :poff2))) #x2dc00000 $ldstpair-mask)
   (def stp ((:rt :s) (:rt2 :s) (:mem-post (:base :x/sp) (:imm :poff2))) #x2c800000 $ldstpair-mask)
   (def ldp ((:rt :s) (:rt2 :s) (:mem-post (:base :x/sp) (:imm :poff2))) #x2cc00000 $ldstpair-mask)
   (def stp ((:rt :d) (:rt2 :d) (:mem-scaled (:base :x/sp) (:imm :poff3))) #x6d000000 $ldstpair-mask)
   (def ldp ((:rt :d) (:rt2 :d) (:mem-scaled (:base :x/sp) (:imm :poff3))) #x6d400000 $ldstpair-mask)
   (def stp ((:rt :d) (:rt2 :d) (:mem-pre (:base :x/sp) (:imm :poff3))) #x6d800000 $ldstpair-mask)
   (def ldp ((:rt :d) (:rt2 :d) (:mem-pre (:base :x/sp) (:imm :poff3))) #x6dc00000 $ldstpair-mask)
   (def stp ((:rt :d) (:rt2 :d) (:mem-post (:base :x/sp) (:imm :poff3))) #x6c800000 $ldstpair-mask)
   (def ldp ((:rt :d) (:rt2 :d) (:mem-post (:base :x/sp) (:imm :poff3))) #x6cc00000 $ldstpair-mask)

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

   ;; Shift aliases.  The register forms alias lslv/lsrv/asrv/rorv;
   ;; the immediate forms alias ubfm/sbfm (lsl/lsr/asr) and extr (ror),
   ;; with immr/imms computed from the shift amount.
   (def lsl ((:rd :w) (:rn :w) (:rm :w)) #x1ac02000 0 :alias)
   (def lsl ((:rd :x) (:rn :x) (:rm :x)) #x9ac02000 0 :alias)
   (def lsl ((:rd :w) (:rn :w) :lsl-imm-w) #x53000000 0 :alias)
   (def lsl ((:rd :x) (:rn :x) :lsl-imm-x) #xd3400000 0 :alias)
   (def lsr ((:rd :w) (:rn :w) (:rm :w)) #x1ac02400 0 :alias)
   (def lsr ((:rd :x) (:rn :x) (:rm :x)) #x9ac02400 0 :alias)
   (def lsr ((:rd :w) (:rn :w) :lsr-imm-w) #x53000000 0 :alias)
   (def lsr ((:rd :x) (:rn :x) :lsr-imm-x) #xd3400000 0 :alias)
   (def asr ((:rd :w) (:rn :w) (:rm :w)) #x1ac02800 0 :alias)
   (def asr ((:rd :x) (:rn :x) (:rm :x)) #x9ac02800 0 :alias)
   (def asr ((:rd :w) (:rn :w) :asr-imm-w) #x13000000 0 :alias)
   (def asr ((:rd :x) (:rn :x) :asr-imm-x) #x93400000 0 :alias)
   (def ror ((:rd :w) (:rn :w) (:rm :w)) #x1ac02c00 0 :alias)
   (def ror ((:rd :x) (:rn :x) (:rm :x)) #x9ac02c00 0 :alias)
   (def ror ((:rd :w) (:rn+rm :w) :imms-w) #x13800000 0 :alias)
   (def ror ((:rd :x) (:rn+rm :x) :imms-x) #x93c00000 0 :alias)

   ;; Sign/zero-extend aliases of sbfm/ubfm: fixed immr=0,
   ;; imms=7/15/31 baked into the base, so they take only Rd, Rn
   ;; (source is always Wn).
   (def sxtb ((:rd :w) (:rn :w)) #x13001c00 0 :alias)
   (def sxtb ((:rd :x) (:rn :w)) #x93401c00 0 :alias)
   (def sxth ((:rd :w) (:rn :w)) #x13003c00 0 :alias)
   (def sxth ((:rd :x) (:rn :w)) #x93403c00 0 :alias)
   (def sxtw ((:rd :x) (:rn :w)) #x93407c00 0 :alias)
   (def uxtb ((:rd :w) (:rn :w)) #x53001c00 0 :alias)
   (def uxth ((:rd :w) (:rn :w)) #x53003c00 0 :alias)

   ;; Bitfield insert aliases: immr=(-lsb) mod width, imms=width-1
   ;; (separable, so #lsb and #width each drive one field).  Extract
   ;; forms (sbfx/ubfx/ bfxil) are lapmacros.
   (def sbfiz ((:rd :w) (:rn :w) :bf-lsb-w :bf-width-w) #x13000000 0 :alias)
   (def sbfiz ((:rd :x) (:rn :x) :bf-lsb-x :bf-width-x) #x93400000 0 :alias)
   (def ubfiz ((:rd :w) (:rn :w) :bf-lsb-w :bf-width-w) #x53000000 0 :alias)
   (def ubfiz ((:rd :x) (:rn :x) :bf-lsb-x :bf-width-x) #xd3400000 0 :alias)
   (def bfi ((:rd :w) (:rn :w) :bf-lsb-w :bf-width-w) #x33000000 0 :alias)
   (def bfi ((:rd :x) (:rn :x) :bf-lsb-x :bf-width-x) #xb3400000 0 :alias)

   ;; bfc is bfi with Rn=zr
   (def bfc ((:rd :w) :bf-lsb-w :bf-width-w) #x330003e0 0 :alias)
   (def bfc ((:rd :x) :bf-lsb-x :bf-width-x) #xb34003e0 0 :alias)

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
   (def mov ((:rd :w) (:rm :w)) #x2a0003e0 0 :alias)
   (def mov ((:rd :x) (:rm :x)) #xaa0003e0 0 :alias)
   (def orn ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x2a200000 $log-shift-mask)
   (def orn ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xaa200000 $log-shift-mask)
   (def mvn ((:rd :w) (:rm :w-shift-ror)) #x2a2003e0 0 :alias)
   (def mvn ((:rd :x) (:rm :x-shift-ror)) #xaa2003e0 0 :alias)
   (def eor ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x4a000000 $log-shift-mask)
   (def eor ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xca000000 $log-shift-mask)
   (def eon ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x4a200000 $log-shift-mask)
   (def eon ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xca200000 $log-shift-mask)
   (def ands ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x6a000000 $log-shift-mask)
   (def ands ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xea000000 $log-shift-mask)
   (def tst ((:rn :w) (:rm :w-shift-ror)) #x6a00001f 0 :alias)
   (def tst ((:rn :x) (:rm :x-shift-ror)) #xea00001f 0 :alias)
   (def bics ((:rd :w) (:rn :w) (:rm :w-shift-ror)) #x6a200000 $log-shift-mask)
   (def bics ((:rd :x) (:rn :x) (:rm :x-shift-ror)) #xea200000 $log-shift-mask)

   ;; Add/subtract (shifted register)
   (def add ((:rd :w) (:rn :w) (:rm :w-shift)) #x0b000000 $addsub-shift-mask)
   (def add ((:rd :x) (:rn :x) (:rm :x-shift)) #x8b000000 $addsub-shift-mask)
   (def adds ((:rd :w) (:rn :w) (:rm :w-shift)) #x2b000000 $addsub-shift-mask)
   (def adds ((:rd :x) (:rn :x) (:rm :x-shift)) #xab000000 $addsub-shift-mask)
   (def cmn ((:rn :w) (:rm :w-shift)) #x2b00001f 0 :alias)
   (def cmn ((:rn :x) (:rm :x-shift)) #xab00001f 0 :alias)
   (def sub ((:rd :w) (:rn :w) (:rm :w-shift)) #x4b000000 $addsub-shift-mask)
   (def sub ((:rd :x) (:rn :x) (:rm :x-shift)) #xcb000000 $addsub-shift-mask)
   (def subs ((:rd :w) (:rn :w) (:rm :w-shift)) #x6b000000 $addsub-shift-mask)
   (def subs ((:rd :x) (:rn :x) (:rm :x-shift)) #xeb000000 $addsub-shift-mask)
   (def cmp ((:rn :w) (:rm :w-shift)) #x6b00001f 0 :alias)
   (def cmp ((:rn :x) (:rm :x-shift)) #xeb00001f 0 :alias)
   (def neg ((:rd :w) (:rm :w-shift)) #x4b0003e0 0 :alias)
   (def neg ((:rd :x) (:rm :x-shift)) #xcb0003e0 0 :alias)
   (def negs ((:rd :w) (:rm :w-shift)) #x6b0003e0 0 :alias)
   (def negs ((:rd :x) (:rm :x-shift)) #xeb0003e0 0 :alias)

   ;; Add/subtract (extended register)
   (def add ((:rd :w/sp) (:rn :w/sp) (:rm :w-ext)) #x0b200000 $addsub-ext-mask)
   (def add ((:rd :x/sp) (:rn :x/sp) (:rm :x-ext)) #x8b200000 $addsub-ext-mask)
   (def adds ((:rd :w) (:rn :w/sp) (:rm :w-ext)) #x2b200000 $addsub-ext-mask)
   (def adds ((:rd :x) (:rn :x/sp) (:rm :x-ext)) #xab200000 $addsub-ext-mask)
   (def cmn ((:rn :w/sp) (:rm :w-ext)) #x2b20001f 0 :alias)
   (def cmn ((:rn :x/sp) (:rm :x-ext)) #xab20001f 0 :alias)
   (def sub ((:rd :w/sp) (:rn :w/sp) (:rm :w-ext)) #x4b200000 $addsub-ext-mask)
   (def sub ((:rd :x/sp) (:rn :x/sp) (:rm :x-ext)) #xcb200000 $addsub-ext-mask)
   (def subs ((:rd :w) (:rn :w/sp) (:rm :w-ext)) #x6b200000 $addsub-ext-mask)
   (def subs ((:rd :x) (:rn :x/sp) (:rm :x-ext)) #xeb200000 $addsub-ext-mask)
   (def cmp ((:rn :w/sp) (:rm :w-ext)) #x6b20001f 0 :alias)
   (def cmp ((:rn :x/sp) (:rm :x-ext)) #xeb20001f 0 :alias)

   ;; Add/subtract (with carry)
   (def adc ((:rd :w) (:rn :w) (:rm :w)) #x1a000000 $rm/rn/rd-mask)
   (def adc ((:rd :x) (:rn :x) (:rm :x)) #x9a000000 $rm/rn/rd-mask)
   (def adcs ((:rd :w) (:rn :w) (:rm :w)) #x3a000000 $rm/rn/rd-mask)
   (def adcs ((:rd :x) (:rn :x) (:rm :x)) #xba000000 $rm/rn/rd-mask)
   (def sbc ((:rd :w) (:rn :w) (:rm :w)) #x5a000000 $rm/rn/rd-mask)
   (def sbc ((:rd :x) (:rn :x) (:rm :x)) #xda000000 $rm/rn/rd-mask)
   (def ngc ((:rd :w) (:rm :w)) #x5a0003e0 0 :alias)
   (def ngc ((:rd :x) (:rm :x)) #xda0003e0 0 :alias)
   (def sbcs ((:rd :w) (:rn :w) (:rm :w)) #x7a000000 $rm/rn/rd-mask)
   (def sbcs ((:rd :x) (:rn :x) (:rm :x)) #xfa000000 $rm/rn/rd-mask)
   (def ngcs ((:rd :w) (:rm :w)) #x7a0003e0 0 :alias)
   (def ngcs ((:rd :x) (:rm :x)) #xfa0003e0 0 :alias)

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
   (def cset ((:rd :w) :cond-inv) #x1a9f07e0 0 :alias)
   (def cset ((:rd :x) :cond-inv) #x9a9f07e0 0 :alias)
   (def csetm ((:rd :w) :cond-inv) #x5a9f03e0 0 :alias)
   (def csetm ((:rd :x) :cond-inv) #xda9f03e0 0 :alias)
   (def cinc ((:rd :w) (:rn+rm :w) :cond-inv) #x1a800400 0 :alias)
   (def cinc ((:rd :x) (:rn+rm :x) :cond-inv) #x9a800400 0 :alias)
   (def cinv ((:rd :w) (:rn+rm :w) :cond-inv) #x5a800000 0 :alias)
   (def cinv ((:rd :x) (:rn+rm :x) :cond-inv) #xda800000 0 :alias)
   (def cneg ((:rd :w) (:rn+rm :w) :cond-inv) #x5a800400 0 :alias)
   (def cneg ((:rd :x) (:rn+rm :x) :cond-inv) #xda800400 0 :alias)

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
   (def mul ((:rd :w) (:rn :w) (:rm :w)) #x1b007c00 0 :alias)
   (def mul ((:rd :x) (:rn :x) (:rm :x)) #x9b007c00 0 :alias)
   (def mneg ((:rd :w) (:rn :w) (:rm :w)) #x1b00fc00 0 :alias)
   (def mneg ((:rd :x) (:rn :x) (:rm :x)) #x9b00fc00 0 :alias)
   (def smull ((:rd :x) (:rn :w) (:rm :w)) #x9b207c00 0 :alias)
   (def smnegl ((:rd :x) (:rn :w) (:rm :w)) #x9b20fc00 0 :alias)
   (def umull ((:rd :x) (:rn :w) (:rm :w)) #x9ba07c00 0 :alias)
   (def umnegl ((:rd :x) (:rn :w) (:rm :w)) #x9ba0fc00 0 :alias)

   ;;; Data Processing -- Scalar Floating-Point

   ;; FP data-processing (2 source)
   (def fmul ((:rd :s) (:rn :s) (:rm :s)) #x1e200800 $fp-dp2src-mask)
   (def fmul ((:rd :d) (:rn :d) (:rm :d)) #x1e600800 $fp-dp2src-mask)
   (def fdiv ((:rd :s) (:rn :s) (:rm :s)) #x1e201800 $fp-dp2src-mask)
   (def fdiv ((:rd :d) (:rn :d) (:rm :d)) #x1e601800 $fp-dp2src-mask)
   (def fadd ((:rd :s) (:rn :s) (:rm :s)) #x1e202800 $fp-dp2src-mask)
   (def fadd ((:rd :d) (:rn :d) (:rm :d)) #x1e602800 $fp-dp2src-mask)
   (def fsub ((:rd :s) (:rn :s) (:rm :s)) #x1e203800 $fp-dp2src-mask)
   (def fsub ((:rd :d) (:rn :d) (:rm :d)) #x1e603800 $fp-dp2src-mask)
   (def fmax ((:rd :s) (:rn :s) (:rm :s)) #x1e204800 $fp-dp2src-mask)
   (def fmax ((:rd :d) (:rn :d) (:rm :d)) #x1e604800 $fp-dp2src-mask)
   (def fmin ((:rd :s) (:rn :s) (:rm :s)) #x1e205800 $fp-dp2src-mask)
   (def fmin ((:rd :d) (:rn :d) (:rm :d)) #x1e605800 $fp-dp2src-mask)
   (def fmaxnm ((:rd :s) (:rn :s) (:rm :s)) #x1e206800 $fp-dp2src-mask)
   (def fmaxnm ((:rd :d) (:rn :d) (:rm :d)) #x1e606800 $fp-dp2src-mask)
   (def fminnm ((:rd :s) (:rn :s) (:rm :s)) #x1e207800 $fp-dp2src-mask)
   (def fminnm ((:rd :d) (:rn :d) (:rm :d)) #x1e607800 $fp-dp2src-mask)
   (def fnmul ((:rd :s) (:rn :s) (:rm :s)) #x1e208800 $fp-dp2src-mask)
   (def fnmul ((:rd :d) (:rn :d) (:rm :d)) #x1e608800 $fp-dp2src-mask)

   ;; FP data-processing (1 source)
   (def fmov ((:rd :s) (:rn :s)) #x1e204000 $fp-dp1src-mask)
   (def fmov ((:rd :d) (:rn :d)) #x1e604000 $fp-dp1src-mask)
   (def fabs ((:rd :s) (:rn :s)) #x1e20c000 $fp-dp1src-mask)
   (def fabs ((:rd :d) (:rn :d)) #x1e60c000 $fp-dp1src-mask)
   (def fneg ((:rd :s) (:rn :s)) #x1e214000 $fp-dp1src-mask)
   (def fneg ((:rd :d) (:rn :d)) #x1e614000 $fp-dp1src-mask)
   (def fsqrt ((:rd :s) (:rn :s)) #x1e21c000 $fp-dp1src-mask)
   (def fsqrt ((:rd :d) (:rn :d)) #x1e61c000 $fp-dp1src-mask)
   ;; fcvt between single and double
   (def fcvt ((:rd :d) (:rn :s)) #x1e22c000 $fp-dp1src-mask)
   (def fcvt ((:rd :s) (:rn :d)) #x1e624000 $fp-dp1src-mask)

   ;; Advanced SIMD lane insert/extract (element copy).
   (def ins ((:rd :elt5) (:rn :elt4)) #x6e000400 #xffe08400)
   (def mov ((:rd :elt5) (:rn :elt4)) #x6e000400 0 :alias)
   ;; dup (scalar destination)
   (def dup ((:rd :s) (:rn :elt5)) #x5e040400 #xffe7fc00)
   (def dup ((:rd :d) (:rn :elt5)) #x5e080400 #xffeffc00)

   ;; Advanced SIMD whole-vector ops (arrangement operands).  C7 CNT/ADDV.
   ;; CNT: per-byte population count, Vd.<T> <- popcount(Vn.<T>), byte
   ;; arrangements only (size fixed 00, Q free for 8b vs 16b).
   (def cnt ((:rd :varr) (:rn :varr)) #x0e205800 #xbffffc00)
   ;; ADDV: horizontal add across the vector into a scalar Bd/Hd/Sd (the
   ;; "Advanced SIMD across lanes" group).  Only the byte form is provided
   ;; here -- size pinned to 00 in the mask, dest a byte scalar :b -- which
   ;; is all %ilogcount/popcount needs.
   ;;
   ;; To generalize (e.g. addv over 4h/8h/4s, or the siblings saddlv/uaddlv/
   ;; smaxv/sminv/umaxv/uminv, all same shape at #x?e30?800): widen the mask
   ;; to #xbf3ffc00 so size (bits 23:22) is operand-determined, and size the
   ;; scalar dest from the arrangement -- Bd/Hd/Sd for the same-size
   ;; reductions, one step wider (Hd/Sd/Dd) for the *LV widening variants.
   ;; That needs a dest class that reads its width from the source
   ;; arrangement's size field (the :b dest here is a fixed byte); today
   ;; each size would otherwise be a separate template.
   (def addv ((:rd :b) (:rn :varr)) #x0e31b800 #xbffffc00)

   ;; round to integral value (frintN/P/M/Z/A/X/I)
   (def frintn ((:rd :s) (:rn :s)) #x1e244000 $fp-dp1src-mask)
   (def frintn ((:rd :d) (:rn :d)) #x1e644000 $fp-dp1src-mask)
   (def frintp ((:rd :s) (:rn :s)) #x1e24c000 $fp-dp1src-mask)
   (def frintp ((:rd :d) (:rn :d)) #x1e64c000 $fp-dp1src-mask)
   (def frintm ((:rd :s) (:rn :s)) #x1e254000 $fp-dp1src-mask)
   (def frintm ((:rd :d) (:rn :d)) #x1e654000 $fp-dp1src-mask)
   (def frintz ((:rd :s) (:rn :s)) #x1e25c000 $fp-dp1src-mask)
   (def frintz ((:rd :d) (:rn :d)) #x1e65c000 $fp-dp1src-mask)
   (def frinta ((:rd :s) (:rn :s)) #x1e264000 $fp-dp1src-mask)
   (def frinta ((:rd :d) (:rn :d)) #x1e664000 $fp-dp1src-mask)
   (def frintx ((:rd :s) (:rn :s)) #x1e274000 $fp-dp1src-mask)
   (def frintx ((:rd :d) (:rn :d)) #x1e674000 $fp-dp1src-mask)
   (def frinti ((:rd :s) (:rn :s)) #x1e27c000 $fp-dp1src-mask)
   (def frinti ((:rd :d) (:rn :d)) #x1e67c000 $fp-dp1src-mask)

   ;; FP data-processing (3 source)
   (def fmadd ((:rd :s) (:rn :s) (:rm :s) (:ra :s)) #x1f000000 $fp-dp3src-mask)
   (def fmadd ((:rd :d) (:rn :d) (:rm :d) (:ra :d)) #x1f400000 $fp-dp3src-mask)
   (def fmsub ((:rd :s) (:rn :s) (:rm :s) (:ra :s)) #x1f008000 $fp-dp3src-mask)
   (def fmsub ((:rd :d) (:rn :d) (:rm :d) (:ra :d)) #x1f408000 $fp-dp3src-mask)
   (def fnmadd ((:rd :s) (:rn :s) (:rm :s) (:ra :s)) #x1f200000 $fp-dp3src-mask)
   (def fnmadd ((:rd :d) (:rn :d) (:rm :d) (:ra :d)) #x1f600000 $fp-dp3src-mask)
   (def fnmsub ((:rd :s) (:rn :s) (:rm :s) (:ra :s)) #x1f208000 $fp-dp3src-mask)
   (def fnmsub ((:rd :d) (:rn :d) (:rm :d) (:ra :d)) #x1f608000 $fp-dp3src-mask)

   ;; FP compare
   (def fcmp ((:rn :s) (:rm :s)) #x1e202000 $fp-cmp-mask)
   (def fcmp ((:rn :d) (:rm :d)) #x1e602000 $fp-cmp-mask)
   (def fcmp ((:rn :s) :fpzero) #x1e202008 $fp-cmp-zero-mask)
   (def fcmp ((:rn :d) :fpzero) #x1e602008 $fp-cmp-zero-mask)
   (def fcmpe ((:rn :s) (:rm :s)) #x1e202010 $fp-cmp-mask)
   (def fcmpe ((:rn :d) (:rm :d)) #x1e602010 $fp-cmp-mask)
   (def fcmpe ((:rn :s) :fpzero) #x1e202018 $fp-cmp-zero-mask)
   (def fcmpe ((:rn :d) :fpzero) #x1e602018 $fp-cmp-zero-mask)

   ;; FP conditional compare
    (def fccmp ((:rn :s) (:rm :s) :nzcv :cond) #x1e200400 $condcmp-mask)
   (def fccmp ((:rn :d) (:rm :d) :nzcv :cond) #x1e600400 $condcmp-mask)
   (def fccmpe ((:rn :s) (:rm :s) :nzcv :cond) #x1e200410 $condcmp-mask)
   (def fccmpe ((:rn :d) (:rm :d) :nzcv :cond) #x1e600410 $condcmp-mask)

   ;; FP conditional select
   (def fcsel ((:rd :s) (:rn :s) (:rm :s) :cond) #x1e200c00 $condsel-mask)
   (def fcsel ((:rd :d) (:rn :d) (:rm :d) :cond) #x1e600c00 $condsel-mask)

   ;; Conversion between FP and integer
   ;; Every GPR-width by FP-type combination gets its own template.
   (def scvtf ((:rd :s) (:rn :w)) #x1e220000 $fp-cvt-mask)
   (def scvtf ((:rd :s) (:rn :x)) #x9e220000 $fp-cvt-mask)
   (def scvtf ((:rd :d) (:rn :w)) #x1e620000 $fp-cvt-mask)
   (def scvtf ((:rd :d) (:rn :x)) #x9e620000 $fp-cvt-mask)
   (def ucvtf ((:rd :s) (:rn :w)) #x1e230000 $fp-cvt-mask)
   (def ucvtf ((:rd :s) (:rn :x)) #x9e230000 $fp-cvt-mask)
   (def ucvtf ((:rd :d) (:rn :w)) #x1e630000 $fp-cvt-mask)
   (def ucvtf ((:rd :d) (:rn :x)) #x9e630000 $fp-cvt-mask)
   ;; truncate (round toward zero)
   (def fcvtzs ((:rd :w) (:rn :s)) #x1e380000 $fp-cvt-mask)
   (def fcvtzs ((:rd :x) (:rn :s)) #x9e380000 $fp-cvt-mask)
   (def fcvtzs ((:rd :w) (:rn :d)) #x1e780000 $fp-cvt-mask)
   (def fcvtzs ((:rd :x) (:rn :d)) #x9e780000 $fp-cvt-mask)
   (def fcvtzu ((:rd :w) (:rn :s)) #x1e390000 $fp-cvt-mask)
   (def fcvtzu ((:rd :x) (:rn :s)) #x9e390000 $fp-cvt-mask)
   (def fcvtzu ((:rd :w) (:rn :d)) #x1e790000 $fp-cvt-mask)
   (def fcvtzu ((:rd :x) (:rn :d)) #x9e790000 $fp-cvt-mask)
   ;; round to nearest, ties to even
   (def fcvtns ((:rd :w) (:rn :s)) #x1e200000 $fp-cvt-mask)
   (def fcvtns ((:rd :x) (:rn :s)) #x9e200000 $fp-cvt-mask)
   (def fcvtns ((:rd :w) (:rn :d)) #x1e600000 $fp-cvt-mask)
   (def fcvtns ((:rd :x) (:rn :d)) #x9e600000 $fp-cvt-mask)
   (def fcvtnu ((:rd :w) (:rn :s)) #x1e210000 $fp-cvt-mask)
   (def fcvtnu ((:rd :x) (:rn :s)) #x9e210000 $fp-cvt-mask)
   (def fcvtnu ((:rd :w) (:rn :d)) #x1e610000 $fp-cvt-mask)
   (def fcvtnu ((:rd :x) (:rn :d)) #x9e610000 $fp-cvt-mask)

   ;; fmov between a GPR and an FP register (without conversion)
   (def fmov ((:rd :w) (:rn :s)) #x1e260000 $fp-cvt-mask)
   (def fmov ((:rd :s) (:rn :w)) #x1e270000 $fp-cvt-mask)
   (def fmov ((:rd :x) (:rn :d)) #x9e660000 $fp-cvt-mask)
   (def fmov ((:rd :d) (:rn :x)) #x9e670000 $fp-cvt-mask)

   ;; fmov scalar immediate: see encode-fp-imm8, decode-fp-imm8.
   (def fmov ((:rd :s) :fpimm8) #x1e201000 $fp-imm-mask)
   (def fmov ((:rd :d) :fpimm8) #x1e601000 $fp-imm-mask)
   ))

(defvar *instruction-template-lists* (make-hash-table :test #'equalp))

(defun initialize-templates ()
  (clrhash *instruction-template-lists*)
  (dotimes (i (length *instruction-templates*))
    (let* ((template (svref *instruction-templates* i))
           (name (instruction-template-name template)))
      (setf (instruction-template-ordinal template) i)
      (push template (gethash name *instruction-template-lists*))))
  ;; template order can be significant, so put them in original order
  (maphash #'(lambda (k v)
               (setf (gethash k *instruction-template-lists*)
                     (nreverse v)))
           *instruction-template-lists*)
  ;; If vinsn templates have already been loaded, their baked-in template
  ;; ordinals may now be stale (the vector may have been reordered since
  ;; they were compiled): re-resolve them against the current table.  On
  ;; the first load neither the function nor the templates exist yet.
  (when (and (fboundp 'ccl::fixup-arm64-vinsn-templates)
             (boundp 'ccl::*arm64-vinsn-templates*))
    (funcall 'ccl::fixup-arm64-vinsn-templates
             (symbol-value 'ccl::*arm64-vinsn-templates*))))

(initialize-templates)


;;; This is the entry point to the assembler.

;;; lap-form is a list and its car isn't a pseudo-op or lapmacro
(defun assemble-instruction (seg lap-form)
  (let ((insn (%make-instruction lap-form)))
    (destructuring-bind (name . lap-operands) lap-form
      (let ((templates (gethash (string name) *instruction-template-lists*)))
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
              (when seg
                (emit-element seg insn))
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
    :s             ;Sn, scalar single-float (FP/SIMD reg, 32-bit view)
    :d             ;Dn, scalar double-float (FP/SIMD reg, 64-bit view)
    :b             ;8-bit view, SIMD&FP reg
    :h             ;16-bit view, SIMD&FP reg
    :elt5          ;a vector lane Vn.Ts[i] whose size+index encode into imm5
    :elt4          ;a vector lane Vn.Ts[i] whose index encodes into imm4
    :varr          ;a whole vector Vn.<T> (arrangement -> Q @ 30, size @ 23:22)
    :aimm          ;uimm12, maybe shifted left 12 bits
    :limm          ;fancy logical immediate
    :simm9         ;signed 9-bit immediate for unscaled register offset
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
    :sysreg        ;named system register, 15-bit op0:op1:CRn:CRm:op2 @ 19:5 (mrs/msr)
    :imm5          ;5-bit unsigned immediate @ 20:16 (ccmp/ccmn immediate form)
    :nzcv          ;4-bit flags immediate @ 3:0 (ccmp/ccmn)
    :fpzero        ;the literal #0.0 (fcmp/fcmpe zero form); encodes nothing
    :fpimm8        ;8-bit FP move immediate @ 20:13 (fmov scalar immediate)
    :lsl-imm-x     ;lsl #n alias of ubfm: immr=(-n)&63, imms=63-n (X)
    :lsl-imm-w     ; ... and the W form (immr=(-n)&31, imms=31-n)
    :lsr-imm-x     ;lsr/asr #n alias of u/sbfm: immr=n, imms=63 (X)
    :lsr-imm-w     ; ... and the W form (immr=n, imms=31)
    :asr-imm-x     ;asr #n: same field encoding as :lsr-imm-x, sbfm base
    :asr-imm-w
    :bf-lsb-x    ;sbfiz/ubfiz/bfi #lsb (X): immr = (-lsb) & 63
    :bf-lsb-w    ; ... and the W form (immr = (-lsb) & 31)
    :bf-width-x    ;sbfiz/ubfiz/bfi #width (X): imms = width-1
    :bf-width-w    ; ... and the W form
    :cond          ;4-bit condition @ 15:12 (csel/csinc/ccmp ...), written (:? cc)
    :cond-inv      ;like :cond but encodes the inverse (cset/cinc ... aliases)
    :cond-b        ;4-bit condition @ 3:0 (b.cond), written (:? cc) or (:~ cc)
    :pcrel         ;signed 21-bit value, split into immlo/immhi (adr/adrp)
    :b26           ;branch target, imm26 @ 25:0 (b, bl)
    :b19           ;branch target, imm19 @ 23:5 (b.cond, cbz/cbnz)
    :b14           ;branch target, imm14 @ 18:5 (tbz/tbnz)
    :uoff0         ;scaled unsigned offset; N = log2(access size in bytes),
    :uoff1         ; so the access-size scale is baked into the class and the
    :uoff2         ; offset predicate needs no external scale-shift
    :uoff3
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
    :movw-mov-w    ;wide immediate for W
    :movw-mov-x    ;wide immediate for X
    :movw-movn-w   ;negated wide immedate for W
    :movw-movn-x   ;negated wide immediate for X
    ))

;;; The logical instructions permit :ror but register shifts don't.
(defparameter *shift-operators*  '(:lsl :lsr :asr :ror))
(defparameter *extend-operators* '(:uxtb :uxth :uxtw :uxtx
                                   :sxtb :sxth :sxtw :sxtx))

(defstruct immediate-operand
  value          ;an integer, or a float for :fpimm8
  shift)         ;how many bits to shift by (:lsl only), if applicable

(defstruct register-operand
  register                     ;a register struct
  modifier                     ;nil or a shift/extend operator keyword
  (amount 0))                  ;shift/extend amount

;;; A vector element reference: one lane of a SIMD register, e.g. Vn.D[1].
(defstruct vector-element-operand
  register                              ;register number
  esize                                 ;(member (:b :h :s :d))
  index)                                ;lane number

;;; A whole-vector operand with an arrangement specifier, e.g. Vn.8B -- the
;;; register viewed as N lanes of a given element size.  ARRANGEMENT is one
;;; of :8b :16b :4h :8h :2s :4s :1d :2d.  Used by the SIMD data-processing
;;; instructions (cnt, addv, ...) that operate on a whole vector at once.
(defstruct vector-arrangement-operand
  register
  arrangement)

(defstruct memory-operand
  base                              ;a register operand
  offset                            ;nil or the offset, specified as
                                    ; an immediate or register operand
  pre-indexed                       ;one or the other;
  post-indexed)                     ; having both set makes no sense

;;; A reference to a label, i.e., the operand of a branch instruction.
(defstruct label-operand
  name
  offset                  ;byte offset (read by disassembler)
  target)                 ;target instruction index (set by disassembler)

(defstruct condition-operand
  name                                ;the condition name (a symbol)
  value)                              ;its 4-bit encoding

(defun parse-register-operand (form)
  ;; Recognize a plain register name like x0 or a shifted or extended
  ;; register of the form (x0 modifier [amount]).
  (flet ((parse-shift/extend (form)
           (destructuring-bind (name modifier &optional (amount 0)) form
             (unless (or (member modifier *shift-operators* :test #'eq)
                         (member modifier *extend-operators* :test #'eq))
               (error "~s is not a shift or extend operator" modifier))
             (setq amount (eval-immediate-expression amount))
             (make-register-operand :register (need-register name)
                                    :modifier modifier :amount amount))))
    (if (consp form)
      (if (<= 2 (length form) 3)
        (parse-shift/extend form)
        (error "Invalid register form ~s" form))
      (make-register-operand :register (need-register form)))))

(defun parse-immediate-operand (form)
  ;; Regcognize (:$ value) or (:$ value :lsl amount).  Legal values
  ;; and shift amounts are not checked here.
  (unless (and (consp form)
               (eq (car form) :$)
               (let ((l (length form)))
                 (or (= l 2) (= l 4))))
    (error "Invalid immediate operand ~s" form))
  (destructuring-bind (marker value &optional op (shift 0)) form
    (declare (ignore marker))
    (when op
      ;; There are a few Advanced SIMD modified-immediate instructions
      ;; that use the shift operator MSL.  If we ever support those,
      ;; this will need to change.
      (unless (eq op :lsl)
        (error "Only :lsl is valid for an immediate: ~s" form)))
    (setf value (eval-immediate-expression value)
          shift (eval-immediate-expression shift))
    (make-immediate-operand :value value :shift shift)))

(defun eval-immediate-expression (form)
  (cond
    ((realp form) form)               ;might be a float
    ((and (consp form) (eq (car form) 'quote))
     (let ((n (cadr form)))
       (unless (integerp n)
         (error "Quoted immediate must be an integer: ~s" form))
       (ash n fixnumshift)))
    (t (multiple-value-bind (value condition)
           (ignore-errors (eval form))
         (if condition
           (error "Evaluation of ~s signaled assembly-time error ~s" form
                  condition)
           value)))))

(defun parse-memory-operand (form)
  ;; Recognize (:@ base), (:@ base offset), (:@! ...), (:@+ ...)
  (destructuring-bind (marker base &optional offset) form
    (make-memory-operand
     :base (parse-register-operand base)
     :offset (and offset (parse-operand offset))
     :pre-indexed (eq marker :@!)
     :post-indexed (eq marker :@+))))

(defun parse-label-operand (form)
  ;; A branch target written as a bare symbol naming a label.
  (make-label-operand :name form))

(ccl::defenum (:prefix "COND-")
  eq
  ne
  cs
  cc
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
  al
  nv)
(defconstant cond-hs cond-cs)
(defconstant cond-lo cond-cc)

(defparameter *arm64-condition-names*
  `(("eq" . ,cond-eq)                   ;equal
    ("ne" . ,cond-ne)                   ;not equal
    ("cs" . ,cond-cs) ("hs" . ,cond-hs) ;carry set, unsigned higher or same
    ("cc" . ,cond-cc) ("lo" . ,cond-lo) ;carry clear, unsigned lower
    ("mi" . ,cond-mi)                   ;minus, negative
    ("pl" . ,cond-pl)                   ;plus, positive or zero
    ("vs" . ,cond-vs)                   ;overflow
    ("vc" . ,cond-vc)                   ;no overflow
    ("hi" . ,cond-hi)                   ;unsigned higher
    ("ls" . ,cond-ls)                   ;unsigned lower or same
    ("ge" . ,cond-ge)                   ;signed >=
    ("lt" . ,cond-lt)                   ;signed <
    ("gt" . ,cond-gt)                   ;signed >
    ("le" . ,cond-le)                   ;signed <=
    ("al" . ,cond-al)                   ;always
    ("nv" . ,cond-nv)))                 ;identical to always (despite name)

(defun lookup-arm64-condition-name (name)
  (cdr (assoc name *arm64-condition-names* :test #'string-equal)))

(defun lookup-arm64-condition-value (val)
  (car (rassoc val *arm64-condition-names* :test #'eql)))

(defun need-arm64-condition-name (name)
  (or (lookup-arm64-condition-name name)
      (error "Unknown arm64 condition name ~s." name)))

(defun parse-condition-operand (form &optional invert)
  ;; A condition written (:? cc), e.g. (:? eq), or its inverse (:~ cc).
  ;; The name is validated here so a bogus condition is caught at parse
  ;; time.  Inversion is XOR 1 on the 4-bit code; al/nv have no inverse.
  (destructuring-bind (marker name) form
    (declare (ignore marker))
    (let ((value (need-arm64-condition-name name)))
      (if invert
        (progn
          (unless (< value 14)
            (error "condition ~s has no inverse" name))
          (setq value (logxor value 1))
          (make-condition-operand :name (lookup-arm64-condition-value value)
                                  :value value))
        (make-condition-operand :name name :value value)))))

(defparameter *system-registers*
  ;; name -> the 15-bit op0:op1:CRn:CRm:op2 encoding for msr/mrs
  '(("fpsr" . #x5a21)                 ;FP status (sticky exception bits)
    ("fpcr" . #x5a20)                 ;FP control
    ("nzcv" . #x5a10)                 ;condition flags
    ("tpidr_el0" . #x5e82)))          ;EL0 thread pointer

(defun lookup-system-register (name)
  (cdr (assoc (string name) *system-registers* :test #'string-equal)))

(defun system-register-name (value)
  (car (rassoc value *system-registers* :test #'eql)))

(defun parse-vector-element-operand (form)
  ;; (:s|:d|:b|:h reg index) names one lane of a SIMD register, e.g.
  ;; (:d q1 0) for V1.D[0].  REG is any FP/SIMD register name -- only its
  ;; number matters, the element size comes from the leading keyword.
  (destructuring-bind (esize reg index) form
    (let ((r (need-register reg)))
      (unless (eq (register-family r) :fpr)
        (error "~s: ~s is not an FP/SIMD register" form reg))
      (make-vector-element-operand :register r :esize esize :index index))))

(defun parse-vector-arrangement-operand (form)
  ;; (:8b|:16b|:4h|... reg) names a whole SIMD register with an arrangement,
  ;; e.g. (:8b q0) for V0.8B.  REG is any FP/SIMD register name.
  (destructuring-bind (arrangement reg) form
    (let ((r (need-register reg)))
      (unless (eq (register-family r) :fpr)
        (error "~s: ~s is not an FP/SIMD register" form reg))
      (make-vector-arrangement-operand :register r :arrangement arrangement))))

(defun parse-register-view-operand (form)
  ;; (:s|:d|:w|:x reg) names a non-default view of REG's register number:
  ;; the S/D scalar-FP view or the W/X GPR-width view.  Mirrors the
  ;; register-view override used in vinsn bodies, so LAP can write e.g.
  ;; (:s d0) for the S view (= s0) or (:w imm0) for the W view.  The view
  ;; keyword picks the family, so only the register number is taken from REG.
  (destructuring-bind (view reg) form
    (let ((number (register-number (need-register reg))))
      (make-register-operand
       :register (ecase view
                   (:s (fpr-ref number 32))
                   (:d (fpr-ref number 64))
                   (:b (fpr-ref number 8))
                   (:h (fpr-ref number 16))
                   (:w (gpr-ref number 32))
                   (:x (gpr-ref number 64)))))))

(defun parse-operand (form)
  ;; Recognize an operand written in LAP notation.
  (cond
    ((and form (symbolp form))
     ;; A bare symbol is a register, a known system register, or a label.
     (cond
       ((lookup-register form) (parse-register-operand form))
       ;; A named system register is just an immediate: its value is the
       ;; 15-bit op0:op1:CRn:CRm:op2 encoding the :sysreg field carries.
       ((lookup-system-register form)
        (make-immediate-operand :value (lookup-system-register form) :shift 0))
       (t (parse-label-operand form))))
    ((consp form)
     (case (car form)
       (:$ (parse-immediate-operand form))
       (:? (parse-condition-operand form))
       (:~ (parse-condition-operand form t))
       ((:@ :@! :@+) (parse-memory-operand form))
       ;; (:s x)/(:d x)/(:w x)/(:x x) are register-view overrides; the S/D
       ;; forms grow a third element to become a vector lane (:d x i).
       ((:b :h :s :d) (if (cddr form)
                        (parse-vector-element-operand form)
                        (parse-register-view-operand form)))
       ((:w :x) (parse-register-view-operand form))
       ((:8b :16b :4h :8h :2s :4s :1d :2d) (parse-vector-arrangement-operand form))
       (t (if (symbolp (car form))
            ;; a scaled/extended register like (x0 :lsl 3) or (count :lsl 3)
            (parse-register-operand form)
            (error "Unrecognized operand ~s" form)))))
    (t (error "Unrecognized operand ~s" form))))


;;; Matching parsed operands against a template

;;; A template matches when its operand specs and the parsed operands
;;; agree in number and, pairwise, the operand satisfies the spec.
;;; Encoding is a separate later pass.

;;; Return true if the register in the register-operand is a member of
;;; the given operand class.
(defun match-register-operand (r-op class)
  (let* ((r (register-operand-register r-op))
         (family (register-family r))
         (width (register-width r))
         (amount (register-operand-amount r-op))
         (r31-role (if (= (register-number r) 31)
                     (if (logtest (register-flags r) $rflag-sp)
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
           (fpr-p (required-width)
             ;; a scalar FP/SIMD register at the given access width (Sn/Dn);
             ;; no r31 special case -- v31 is an ordinary register
             (and (eq family :fpr)
                  (= required-width width)
                  (null modifier)))
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
        (:s (fpr-p 32))
        (:d (fpr-p 64))
        (:b (fpr-p 8))
        (:h (fpr-p 16))
        (:x-shift (shifted-p 64 nil))
        (:w-shift (shifted-p 32 nil))
        (:x-shift-ror (shifted-p 64 t))
        (:w-shift-ror (shifted-p 32 t))
        (:x-ext (extended-p 64))
        (:w-ext (extended-p 32))))))

;;; See if the floating-point value can be encoded as a special
;;; floating-point immediate.  Section C2.2.3 in the manual discusses
;;; the format.
;;;
;;; The format is a little obscure, but the key is to note that every
;;; encodable value is +/-(1 + m/16) * 2^e with m in [0, 15] and e in
;;; the range -3 to 4.  Thus, an encodable significand uses only its
;;; top 4 fraction bits and the exponent is in a narrow range around
;;; zero.
;;;
;;; This is a job for integer-decode-float.
(defun encode-fp-imm8 (value)
  (when (and (floatp value) (not (zerop value)))
    (multiple-value-bind (significand exponent sign)
        (integer-decode-float value)
      (let ((nbits (float-digits value)))
        ;; The significand we get from integer-decode-float is scaled
        ;; such that the high bit is the now-explicit hidden bit, and
        ;; the remaining bits are the fraction.  The imm8 float
        ;; immediate can encode a fraction of 4 bits, so if the lower
        ;; part of the significand is clear, then maybe the provided
        ;; value is encodable.
        (when (zerop (ldb (byte (- nbits 5) 0) significand))
          ;; Now check exponent range.
          (let ((m (ldb (byte 4 (- nbits 5)) significand))
                (e (+ exponent (1- nbits))));unbiased exponent of the MSB
            (when (<= -3 e 4)               ;exponent in range
              (let ((k (+ e 3)))            ;0 to 7; e = k-3
                (logior (if (minusp sign) #x80 0)
                        (ash (logxor 1 (ldb (byte 1 2) k)) 6)
                        (ash (ldb (byte 2 0) k) 4)
                        m)))))))))

;;; Create a single-float representation of the imm8 float immediate.
;;; See C2.2.3 in the manual.
(defun decode-fp-imm8 (imm8)
  (check-type imm8 (unsigned-byte 8))
  (let* ((sign (ldb (byte 1 7) imm8))   ;negative if set
         (b    (ldb (byte 1 6) imm8))   ;exponent selector
         (cd   (ldb (byte 2 4) imm8))   ; in two parts
         (m    (ldb (byte 4 0) imm8))   ;fraction
         (k    (logior (ash (logxor 1 b) 2) cd))   ;k = e+3
         (e    (- k 3)))                           ;unbiased exponent
    (ccl::make-short-float-from-fixnums
     ;; insert into top 4 bits of mantissa
     (dpb m (byte 4 19) 0)
     ;; CCL misdefines ieee-single-float-bias: it's 126, not 127 as expected
     (+ (1+ ccl::ieee-single-float-bias) e)
     (if (= sign 1) -1 1))))

;;; If the integer n can be encoded as a wide immmediate, return
;;; (values imm16 hw)
(defun encode-wide-immediate (n &optional (width 64))
  (unless (or (= width 64) (= width 32))
    (error "Width must be either 32 or 64, not ~s" width))
  (when (typep n `(unsigned-byte ,width))
    (do* ((pos 0 (+ pos 16))
          (hw 0 (1+ hw))          ;hw field in movz/movn/movk encoding
          (imm16 (ldb (byte 16 0) n) (ldb (byte 16 pos) n)))
         ((= pos width))
      (when (= n (ash imm16 pos))
        (return (values imm16 hw))))))

;;; Some immediate operands are written already scaled by the memory
;;; access size.  We encode the scale in the operand class (e.g.,
;;; :uoff2 means that the written value is shifted left 2 bits).
(defun match-immediate-operand (imm-op class)
  (let ((value (immediate-operand-value imm-op))
        (shift (immediate-operand-shift imm-op)))
    (flet ((uoff-p (scale)
             (and (eql shift 0)
                  ;; multiple of access size?
                  (zerop (logand value (1- (ash 1 scale))))
                  (typep (ash value (- scale)) '(unsigned-byte 12))))
           (poff-p (scale)      ;signed scaled 7-bit (load/store pair)
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
        (:poff2 (poff-p 2))
        (:poff3 (poff-p 3))
        (:movw-x (and (typep value '(unsigned-byte 16))
                      (member shift '(0 16 32 48))))
        (:movw-w (and (typep value '(unsigned-byte 16))
                      (member shift '(0 16))))
        ((:immr-x :imms-x :tbit-x) (and (eql shift 0)
                                        (typep value '(integer 0 63))))
        ((:immr-w :imms-w :tbit-w) (and (eql shift 0)
                                        (typep value '(integer 0 31))))
        ((:exc16 :udf16) (and (eql shift 0) (typep value '(unsigned-byte 16))))
        (:baropt (and (eql shift 0) (typep value '(unsigned-byte 4))))
        (:imm5 (and (eql shift 0) (typep value '(unsigned-byte 5))))
        (:nzcv (and (eql shift 0) (typep value '(unsigned-byte 4))))
        (:sysreg (and (eql shift 0) (typep value '(unsigned-byte 15))))
        (:fpzero (and (eql shift 0) (numberp value) (zerop value)))
        (:fpimm8 (and (eql shift 0) (encode-fp-imm8 value)))
        ((:lsl-imm-x :lsr-imm-x :asr-imm-x)
         (and (eql shift 0) (typep value '(integer 0 63))))
        ((:lsl-imm-w :lsr-imm-w :asr-imm-w)
         (and (eql shift 0) (typep value '(integer 0 31))))
        (:bf-lsb-x (and (eql shift 0) (typep value '(integer 0 63))))
        (:bf-lsb-w (and (eql shift 0) (typep value '(integer 0 31))))
        (:bf-width-x (and (eql shift 0) (typep value '(integer 1 64))))
        (:bf-width-w (and (eql shift 0) (typep value '(integer 1 32))))
        (:pcrel (and (eql shift 0) (typep value '(signed-byte 21))))
        (:movw-mov-w  (and (eql shift 0)
                           (encode-wide-immediate value 32)))
        (:movw-mov-x  (and (eql shift 0)
                           (encode-wide-immediate value 64)))
        (:movw-movn-w (and (eql shift 0)
                           (encode-wide-immediate
                            (ldb (byte 32 0) (lognot value)) 32)))
        (:movw-movn-x (and (eql shift 0)
                           (encode-wide-immediate
                            (ldb (byte 64 0) (lognot value)) 64)))))))

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
              (not (logtest (register-flags reg) $rflag-sp))
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
        (pre    (memory-operand-pre-indexed mem-operand))
        (post   (memory-operand-post-indexed mem-operand)))
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
                     ((null offset) t)  ;(:@ xn) means (:@ xn (:$ 0))
                     ((immediate-operand-p offset)
                      (match-immediate-operand offset
                                               (cadr (assoc :imm (cdr spec)))))
                     (t nil))))         ;a register offset ⇒ the regoff form
             (:mem-regoff
              (and (not pre) (not post) offset
                   (match-index-operand
                    offset (regoff-scale (cadr (assoc :index (cdr spec)))))))
             (:mem-pre  (and pre (imm-offset-p)))
             (:mem-post (and post (imm-offset-p)))
             ;; bare [Xn]: base only, no offset, no writeback (load/store
             ;; exclusive).  A stray offset must not match -- there is no
             ;; immediate field to put it in.
             (:mem-base (and (not pre) (not post) (null offset))))))))

(defun match-operand (operand spec)
  (cond
    ((label-spec-p spec)                ;(:label class) ⇒ branch target
     (label-operand-p operand))         ;reach is checked at finalize
    ((member spec '(:cond :cond-inv :cond-b)) ;a (:? cc) condition (maybe inverted)
     (condition-operand-p operand))
    ((keywordp spec)                    ;bare keyword ⇒ immediate class
     (and (immediate-operand-p operand)
          (match-immediate-operand operand spec)))
    ((mem-spec-p spec)                  ;(:mem-FORM …) ⇒ memory group
     (and (memory-operand-p operand)
          (match-memory-operand operand spec)))
    ((and (consp spec)                  ;(role :elt5|:elt4) ⇒ vector lane
          (vector-element-class-p (cadr spec)))
     (vector-element-operand-p operand))
    ((and (consp spec)                  ;(role :varr) ⇒ whole-vector arrangement
          (vector-arrangement-class-p (cadr spec)))
     (vector-arrangement-operand-p operand))
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
;;; *register-fields*; the field for an immediate is determined by its
;;; class.

(defun set-field-value (insn bytespec value)
  (setf (ldb bytespec (instruction-word insn)) value))

(defparameter *register-fields*
 `((:rd . ,(byte 5 0)) (:rt . ,(byte 5 0))
   (:rn . ,(byte 5 5)) (:base . ,(byte 5 5))
   (:ra . ,(byte 5 10)) (:rt2 . ,(byte 5 10))
   (:rm . ,(byte 5 16)) (:rs . ,(byte 5 16))))

(defun register-field (role)
  (or (cdr (assoc role *register-fields*))
      (error "No register field for role ~s" role)))

(defun insert-register (insn role operand)
  (set-field-value insn (register-field role)
                   (register-number (register-operand-register operand))))

(defun extract-register (word role)
  (ldb (register-field role) word))

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

;;; ROLE places the register number; CLASS says whether to also place shift or
;;; extend fields (the role of a shifted/extended Rm is just :rm, so the class
;;; is what distinguishes the two forms).
(defun encode-register-operand (insn operand role class)
  (ecase role
    ((:rd :rt :rn :base :rm :rs :ra :rt2)
     (insert-register insn role operand))
    (:rn+rm
     (insert-register insn :rn operand)
     (insert-register insn :rm operand)))
  (let ((modifier (register-operand-modifier operand))
        (amount (register-operand-amount operand)))
    (case class
      ((:x-shift :w-shift :x-shift-ror :w-shift-ror)
       ;; shift type @ 23:22, imm6 amount @ 15:10; a bare register is lsl #0
       (set-field-value insn (byte 2 22) (encode-shift-type (or modifier
                                                                :lsl)))
       (set-field-value insn (byte 6 10) amount))
      ((:x-ext :w-ext)
       ;; option @ 15:13, imm3 amount @ 12:10; a bare register or lsl encodes
       ;; as uxtx (64-bit) / uxtw (32-bit)
       (let ((option (if (or (null modifier) (eq modifier :lsl))
                       (if (eq class :x-ext)
                         (encode-extend-option :uxtx)
                         (encode-extend-option :uxtw))
                       (encode-extend-option modifier))))
         (set-field-value insn (byte 3 13) option)
         (set-field-value insn (byte 3 10) amount))))))

(defun decode-register-operand (word role class)
  (let ((number (extract-register word role)))
    (ecase class
      (:x    (make-register-operand :register (gpr-ref number 64)))
      (:x/sp (make-register-operand :register (gpr-ref number 64 t)))
      (:w    (make-register-operand :register (gpr-ref number 32)))
      (:w/sp (make-register-operand :register (gpr-ref number 32 t)))
      (:sp   (make-register-operand :register (gpr-ref 31 64 t)))
      (:wsp  (make-register-operand :register (gpr-ref 31 32 t)))
      (:s    (make-register-operand :register (fpr-ref number 32)))
      (:d    (make-register-operand :register (fpr-ref number 64)))
      (:b    (make-register-operand :register (fpr-ref number 8)))
      (:h    (make-register-operand :register (fpr-ref number 16)))
      ;; shifted
      ((:x-shift :w-shift :x-shift-ror :w-shift-ror)
       (let ((type (svref *shift-types* (ldb (byte 2 22) word)))
             (amount (ldb (byte 6 10) word))
             (width (if (member class '(:x-shift :x-shift-ror)) 64 32)))
         (if (and (eq type :lsl) (zerop amount))
           (make-register-operand :register (gpr-ref number width))
           (make-register-operand :register (gpr-ref number width)
                                  :modifier type :amount amount))))
      ;; extended
      ((:x-ext :w-ext)
       (let* ((option (svref *extend-options* (ldb (byte 3 13) word)))
              (amount (ldb (byte 3 10) word))
              (width (if (member option '(:uxtx :sxtx)) 64 32)))
         (make-register-operand :register (gpr-ref number width)
                                :modifier option :amount amount))))))

;;; Advanced SIMD lane operands (INS/DUP/MOV element).  The imm5 field
;;; (bits 20:16) fuses element size and one lane index: the least-significant
;;; set bit marks the size (B/H/S/D at bit 0/1/2/3), and the bits above it
;;; hold the index.  The imm4 field (bits 14:11) holds a second lane index
;;; (the INS source), positioned by the same size shift.

(defparameter *element-size-shifts* '((:b . 0) (:h . 1) (:s . 2) (:d . 3)))

(defun element-size-shift (esize)
  (or (cdr (assoc esize *element-size-shifts* :test #'eq))
      (error "Unknown vector element size ~s" esize)))

(defun encode-vector-imm5 (esize index)
  (let ((sh (element-size-shift esize)))
    (logior (ash 1 sh) (ash index (1+ sh)))))

(defun encode-vector-imm4 (esize index)
  (ash index (element-size-shift esize)))

(defun decode-vector-imm5 (bits)
  ;; The bits argument is the raw imm5 field.  Returns (values esize index).
  (cond ((logbitp 0 bits) (values :b (ash bits -1)))
        ((logbitp 1 bits) (values :h (ash bits -2)))
        ((logbitp 2 bits) (values :s (ash bits -3)))
        ((logbitp 3 bits) (values :d (ash bits -4)))
        (t (error "Invalid vector element imm5 #b~5,'0b" bits))))

;;; The element operand classes, in the (role class) spec form: :elt5 puts
;;; its lane (and the size) in imm5; :elt4 puts its lane in imm4.  Both name
;;; a vector-element-operand; the role says which register field (Rd/Rn)
;;; takes the vector number.
(defun vector-element-class-p (class)
  (or (eq class :elt5) (eq class :elt4)))

(defun operand-register (operand)
  ;; The register struct of a plain register or a vector element operand.
  (etypecase operand
    (register-operand (register-operand-register operand))
    (vector-element-operand (vector-element-operand-register operand))))

(defun encode-vector-element-operand (insn operand role class)
  (set-field-value insn (register-field role)
                   (register-number (operand-register operand)))
  (let ((esize (vector-element-operand-esize operand))
        (index (vector-element-operand-index operand)))
    (ecase class
      (:elt5 (set-field-value insn (byte 5 16) (encode-vector-imm5 esize
                                                                   index)))
      (:elt4 (set-field-value insn (byte 4 11) (encode-vector-imm4 esize
                                                                   index))))))

(defun decode-vector-element-operand (word role class)
  (let ((number (extract-register word role)))
    (multiple-value-bind (esize imm5-index)
        (decode-vector-imm5 (ldb (byte 5 16) word))
      (make-vector-element-operand
       :register (fpr-ref number 128)   ;the width doesn't really matter
       :esize esize
       ;; :elt5 carries its own index in imm5; :elt4 takes the size from
       ;; imm5 (the paired :elt5 operand) but its index from imm4.
       :index (ecase class
                (:elt5 imm5-index)
                (:elt4 (ash (ldb (byte 4 11) word)
                            (- (element-size-shift esize)))))))))

;;; A vector arrangement (Vn.<T>) encodes as a Q bit (bit 30, the "wide"
;;; half of each pair) and a 2-bit element size (bits 23:22): B/H/S/D =
;;; 0/1/2/3.  These positions are shared across the SIMD data-processing
;;; formats (two-register misc, across-lanes, ...), so one class serves all.
(defparameter *arrangements*
  ;; arrangement -> (Q . size)
  '((:8b 0 . 0) (:16b 1 . 0)
    (:4h 0 . 1) (:8h 1 . 1)
    (:2s 0 . 2) (:4s 1 . 2)
    (:1d 0 . 3) (:2d 1 . 3)))

(defun arrangement-q+size (arrangement)
  (let ((entry (cdr (assoc arrangement *arrangements* :test #'eq))))
    (or entry (error "Unknown vector arrangement ~s" arrangement))))

(defun q+size-arrangement (q size)
  (or (car (rassoc (cons q size) *arrangements* :test #'equal))
      (error "No vector arrangement for Q=~d size=~d" q size)))

;;; The :varr class, in the (role class) spec form, names a
;;; vector-arrangement-operand: the role's register field takes the number,
;;; and the arrangement drives the Q and size bits.
(defun vector-arrangement-class-p (class)
  (eq class :varr))

(defun encode-vector-arrangement-operand (insn operand role)
  (set-field-value insn (register-field role)
                   (register-number (vector-arrangement-operand-register operand)))
  (destructuring-bind (q . size)
      (arrangement-q+size (vector-arrangement-operand-arrangement operand))
    (set-field-value insn (byte 1 30) q)
    (set-field-value insn (byte 2 22) size)))

(defun decode-vector-arrangement-operand (word role)
  (make-vector-arrangement-operand
   :register (fpr-ref (extract-register word role) 128)
   :arrangement (q+size-arrangement (ldb (byte 1 30) word) (ldb (byte 2 22) word))))

(defparameter *immediate-field-specs*
  `((:simm9 ,(byte 9 12) :signed t)
    (:uoff0 ,(byte 12 10) :scale 0)
    (:uoff1 ,(byte 12 10) :scale 1)
    (:uoff2 ,(byte 12 10) :scale 2)
    (:uoff3 ,(byte 12 10) :scale 3)
    (:poff2 ,(byte 7 15) :scale 2 :signed t)
    (:poff3 ,(byte 7 15) :scale 3 :signed t)
    (:immr-x ,(byte 6 16))
    (:immr-w ,(byte 6 16))
    (:imms-x ,(byte 6 10))
    (:imms-w ,(byte 6 10))
    (:exc16 ,(byte 16 5))
    (:udf16 ,(byte 16 0))
    (:baropt ,(byte 4 8))
    (:imm5 ,(byte 5 16))
    (:nzcv ,(byte 4 0))
    (:sysreg ,(byte 15 5))))

(defun immediate-field-spec (class)
  (or (cdr (assoc class *immediate-field-specs*))
      (error "No immediate field spec for ~s" class)))

(defun insert-immediate-field (insn class value)
  (destructuring-bind (bytespec &key (scale 0) signed)
      (immediate-field-spec class)
    (declare (ignore signed))
    (set-field-value insn bytespec (ash value (- scale)))))

(defun sign-extend (integer width)
  (if (logbitp (1- width) integer)
    (- integer (ash 1 width))
    integer))

(defun extract-immediate-field (word class)
  (destructuring-bind (bytespec &key (scale 0) signed)
      (immediate-field-spec class)
    (let* ((raw (ldb bytespec word))
           (unscaled (if signed
                       (sign-extend raw (byte-size bytespec))
                       raw)))
      (ash unscaled scale))))

(defun encode-immediate-operand (insn operand class)
  (let ((value (immediate-operand-value operand))
        (shift (immediate-operand-shift operand)))
    (case class
      ;; Some classes are encoded in special ways (via custom encode
      ;; functions, or into multiple fields in the instruction).
      (:aimm
       (set-field-value insn (byte 12 10) value)
       (set-field-value insn (byte 1 22) (if (= shift 12) 1 0)))
      (:limm (set-field-value insn (byte 13 10)
                              (encode-logical-immediate value)))
      ((:movw-x :movw-w)
       (set-field-value insn (byte 16 5) value)
       (set-field-value insn (byte 2 21) (ash shift -4)))
      ((:tbit-x :tbit-w)
       (set-field-value insn (byte 5 19) (ldb (byte 5 0) value))
       (set-field-value insn (byte 1 31) (ldb (byte 1 5) value)))
      (:fpimm8 (set-field-value insn (byte 8 13) (encode-fp-imm8 value)))
      (:pcrel
       (set-field-value insn (byte 2 29) (ldb (byte 2 0) value))
       (set-field-value insn (byte 19 5) (ash value -2)))
      (:fpzero)  ;encode nothing: 0.0 literal included in base opcode
      ;; These classes only occur in alias templates.  The disassembler
      ;; never sees them.
      (:lsl-imm-x
       (set-field-value insn (byte 6 16) (logand (- value) 63))
       (set-field-value insn (byte 6 10) (- 63 value)))
      (:lsl-imm-w
       (set-field-value insn (byte 6 16) (logand (- value) 31))
       (set-field-value insn (byte 6 10) (- 31 value)))
      ((:lsr-imm-x :asr-imm-x)
       (set-field-value insn (byte 6 16) value)
       (set-field-value insn (byte 6 10) 63))
      ((:lsr-imm-w :asr-imm-w)
       (set-field-value insn (byte 6 16) value)
       (set-field-value insn (byte 6 10) 31))
      (:bf-lsb-x (set-field-value insn (byte 6 16) (logand (- value) 63)))
      (:bf-lsb-w (set-field-value insn (byte 6 16) (logand (- value) 31)))
      ((:bf-width-x :bf-width-w) (set-field-value insn (byte 6 10) (1- value)))
      ((:movw-mov-w :movw-mov-x)
       (multiple-value-bind (imm16 hw)
           (encode-wide-immediate value (if (eq class :movw-mov-x) 64 32))
         (set-field-value insn (byte 16 5) imm16)
         (set-field-value insn (byte 2 21) hw)))
      ((:movw-movn-w :movw-movn-x)
       (let ((width (if (eq class :movw-movn-x) 64 32)))
         (multiple-value-bind (imm16 hw)
             (encode-wide-immediate (ldb (byte width 0) (lognot value))
                                    width)
           (set-field-value insn (byte 16 5) imm16)
           (set-field-value insn (byte 2 21) hw))))
      ;; Remaining classes are regular.
      (t (insert-immediate-field insn class value)))))

;; Return an immediate-operand struct with value and shift filled in.
(defun decode-immediate-operand (word class)
  (flet ((imm (value &optional (shift 0))
           (make-immediate-operand :value value :shift shift)))
    (case class
      (:aimm (imm (ldb (byte 12 10) word) (if (logbitp 22 word) 12 0)))
      (:limm (imm (decode-logical-immediate (ldb (byte 13 10) word))))
      ((:movw-x :movw-w)
       (imm (ldb (byte 16 5) word) (* 16 (ldb (byte 2 21) word))))
      ((:tbit-x :tbit-w)
       (imm (dpb (ldb (byte 1 31) word) (byte 1 5) (ldb (byte 5 19) word))))
      (:fpimm8 (imm (decode-fp-imm8 (ldb (byte 8 13) word))))
      (t (imm (extract-immediate-field word class))))))

(defun encode-index-operand (insn register-operand)
  ;; A register-offset index: Rm @ 20:16, the extend option @ 15:13, and
  ;; S @ 12 (set iff the index is scaled, i.e. the amount is nonzero).
  (let* ((r (register-operand-register register-operand))
         (modifier (register-operand-modifier register-operand))
         (amount (register-operand-amount register-operand)))
    (set-field-value insn (register-field :rm) (register-number r))
    (set-field-value insn (byte 3 13) (index-option (register-width r)
                                                    modifier))
    (set-field-value insn (byte 1 12) (if (zerop amount) 0 1))))

(defun decode-index-operand (word scale)
  ;; Inverse of encode-index-operand: rebuild the register-offset index as
  ;; a register-operand.  The extend option @ 15:13 gives both the modifier
  ;; and the index width; S @ 12 says whether the index is scaled, in which
  ;; case the amount is the addressing SCALE.  The index is never SP.
  (let ((number (extract-register word :rm))
        (scaled (logbitp 12 word)))
    (multiple-value-bind (width modifier)
        (ecase (ldb (byte 3 13) word)
          (#b010 (values 32 :uxtw))
          (#b011 (values 64 :lsl))
          (#b110 (values 32 :sxtw))
          (#b111 (values 64 :sxtx)))
      (let ((r (gpr-ref number width)))
        (cond
          (scaled
           (make-register-operand :register r :modifier modifier
                                  :amount scale))
          ((eq modifier :lsl)           ;bare [Xn, Xm], no shift
           (make-register-operand :register r))
          (t                            ;extend, no shift: [Xn, Wm, uxtw] &c.
           (make-register-operand :register r :modifier modifier)))))))


(defun encode-memory-operand (insn operand spec)
  ;; Base → Rn; the offset is encoded per the addressing form: an
  ;; immediate (reusing encode-immediate-operand; a missing offset is #0,
  ;; already in the base-opcode) or a register index.
  (insert-register insn :base (memory-operand-base operand))
  (let ((offset (memory-operand-offset operand)))
    (ecase (car spec)
      ;; the pre/post writeback bits @ 11:10 are baked into the base
      ;; opcode, so these encode just like the plain immediate forms.
      ((:mem-scaled :mem-unscaled :mem-pre :mem-post)
       (when offset
         (encode-immediate-operand insn offset
                                   (cadr (assoc :imm (cdr spec))))))
      (:mem-regoff
       (encode-index-operand insn offset))
      ;; bare [Xn]: Rn is already inserted above; nothing more to encode.
      (:mem-base))))

(defun decode-memory-operand (word spec)
  ;; Inverse of encode-memory-operand.  SPEC is (:mem-FORM (:base class)
  ;; ...).  The base is Rn; the offset's form follows the addressing mode:
  ;; an immediate (scaled/unscaled/pre/post) or a register index (regoff);
  ;; the bare [Xn] form has no offset.
  (let* ((mem-spec (pop spec))
         (base (decode-register-operand word :base (cadr (assoc :base spec)))))
    (flet ((imm-offset ()
             (decode-immediate-operand word (cadr (assoc :imm spec)))))
      (ecase mem-spec
        ((:mem-scaled :mem-unscaled)
         (make-memory-operand :base base :offset (imm-offset)))
        (:mem-pre
         (make-memory-operand :base base :offset (imm-offset) :pre-indexed t))
        (:mem-post
         (make-memory-operand :base base :offset (imm-offset) :post-indexed t))
        (:mem-regoff
         (make-memory-operand
          :base base
          :offset (decode-index-operand
                   word (regoff-scale (cadr (assoc :index spec))))))
        (:mem-base
         (make-memory-operand :base base))))))

(defun encode-label-operand (insn operand class)
  ;; Record a reference from INSN to the named label; finalize patches
  ;; the displacement field once all addresses are known.  CLASS
  ;; (:b26/:b19/:b14) is the reftype that selects the field.  The field
  ;; is left zero (its base-opcode value) until then.
  (note-label-reference (label-operand-name operand) insn class))

(defun decode-label-operand (word class)
  ;; The branch field holds a signed instruction-count displacement from
  ;; this instruction.  Lacking the instruction's address, we record it as a
  ;; signed byte offset; resolve-labels turns it into a target di-vector
  ;; index in the whole-code-vector pass (see the LABELED slot).  CLASS is
  ;; the reftype (:b26/:b19/:b14) that selects the field, as in
  ;; *branch-fields*.
  (destructuring-bind (bytespec . width) (cdr (assoc class *branch-fields*))
    (make-label-operand :offset (* 4 (sign-extend (ldb bytespec word)
                                                  width)))))

(defun encode-condition-operand (insn operand &optional invert)
  ;; The condition is a 4-bit field @ 15:12 in the conditional-select and
  ;; conditional-compare instructions (not the 3:0 spot b.cond uses).  The
  ;; cset/cinc/... aliases encode the inverse condition; al/nv have no
  ;; inverse (their low bit isn't a negation), so inverting them is an error.
  (let ((value (condition-operand-value operand)))
    (when invert
      (if (< value 14)
        (setq value (logxor value 1))
        (error "condition ~s has no inverse" (condition-operand-name
                                              operand))))
    (set-field-value insn (byte 4 12) value)))

(defun decode-condition-operand (word &optional invert)
  ;; The 4-bit condition @ 15:12 (csel/ccmp family), the inverse of
  ;; encode-condition-operand.  An :cond-inv operand was encoded as the
  ;; inverse, so re-invert to recover the source name -- though :cond-inv
  ;; occurs only in alias templates, which the disassembler skips, so in
  ;; practice only the plain path runs.
  (let ((value (ldb (byte 4 12) word)))
    (when invert (setq value (logxor value 1)))
    (make-condition-operand :name (lookup-arm64-condition-value value)
                            :value value)))

(defun encode-condition-branch-operand (insn operand)
  ;; b.cond: the 4-bit condition lives at 3:0 (not the 15:12 spot csel/ccmp
  ;; use).  Any inversion (the (:~ cc) form) was already folded into the
  ;; operand's value at parse or expand time, so we just write it.
  (set-field-value insn (byte 4 0) (condition-operand-value operand)))

(defun decode-condition-branch-operand (word)
  ;; The 4-bit condition @ 3:0 (b.cond).  Only reached if a :cond-b template
  ;; ever participates in disassembly; b.c is an alias, so in practice the
  ;; specific b.<cond> forms decode these instead.
  (let ((value (ldb (byte 4 0) word)))
    (make-condition-operand :name (lookup-arm64-condition-value value)
                            :value value)))

(defun encode-operand (insn operand spec)
  (cond
    ((label-spec-p spec) (encode-label-operand insn operand (cadr spec)))
    ((eq spec :cond) (encode-condition-operand insn operand))
    ((eq spec :cond-inv) (encode-condition-operand insn operand t))
    ((eq spec :cond-b) (encode-condition-branch-operand insn operand))
    ((keywordp spec) (encode-immediate-operand insn operand spec))
    ((mem-spec-p spec) (encode-memory-operand insn operand spec))
    ((and (consp spec) (vector-element-class-p (second spec)))
     (encode-vector-element-operand insn operand (first spec) (second spec)))
    ((and (consp spec) (vector-arrangement-class-p (second spec)))
     (encode-vector-arrangement-operand insn operand (first spec)))
    ((consp spec) (encode-register-operand insn operand (first spec)
                                           (second spec)))))

(defun decode-operand (word spec)
  (cond
    ((label-spec-p spec) (decode-label-operand word (cadr spec)))
    ((eq spec :cond) (decode-condition-operand word))
    ((eq spec :cond-inv) (decode-condition-operand word t))
    ((eq spec :cond-b) (decode-condition-branch-operand word))
    ((keywordp spec) (decode-immediate-operand word spec))
    ((mem-spec-p spec) (decode-memory-operand word spec))
    ((and (consp spec) (vector-element-class-p (second spec)))
     (decode-vector-element-operand word (first spec) (second spec)))
    ((and (consp spec) (vector-arrangement-class-p (second spec)))
     (decode-vector-arrangement-operand word (first spec)))
    ((consp spec) (decode-register-operand word (first spec) (second spec)))))

(defun encode-operands (insn)
  (let ((template (instruction-template insn)))
    (setf (instruction-word insn) (instruction-template-base-opcode template))
    (loop for spec in (instruction-template-operand-specs template)
          for operand in (instruction-parsed-operands insn)
          do (encode-operand insn operand spec))))

;;; Rendering operand specs in human-readable, GAS-ish form.  Used now for
;;; "no match" diagnostics; reusable later for disassembly/documentation.

(defparameter *memory-specs*
  '(:mem-scaled :mem-unscaled :mem-regoff :mem-pre :mem-post :mem-base))

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
    (:s       (format nil "S~a" suffix))
    (:d       (format nil "D~a" suffix))
    (:b       (format nil "B~a" suffix))
    (:h       (format nil "H~a" suffix))
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
    (:sysreg "sysreg")
    (:imm5 "#imm5")
    (:nzcv "#nzcv")
    (:fpzero "#0.0")
    (:fpimm8 "#fpimm")
    ((:lsl-imm-x :lsl-imm-w :lsr-imm-x :lsr-imm-w :asr-imm-x :asr-imm-w) "#shift")
    ((:bf-lsb-x :bf-lsb-w) "#lsb")
    ((:bf-width-x :bf-width-w) "#width")
    (:pcrel "label")
    ((:uoff0 :uoff1 :uoff2 :uoff3 :poff2 :poff3) "#off")
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
    ((member spec '(:cond :cond-inv :cond-b)) "(:? cc)")
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
        ((member spec '(:cond :cond-inv :cond-b)) :condition)
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

(ccl::def-standard-initial-binding *instruction-freelist*
                                   (ccl::make-dll-node-freelist))

;;; An instruction in the process of being assembled
(defstruct (instruction (:include instruction-element (size 4))
                        (:constructor %make-instruction (source)))
  source                             ;the lap form
  template                           ;the matched instruction-template
  (word 0 :type (unsigned-byte 32))  ;encoded instruction word
  parsed-operands)

(defun make-instruction (form)
  (let ((insn (ccl::alloc-dll-node *instruction-freelist*)))
    (if (typep insn 'instruction)
      (progn
        (setf (instruction-source insn) form
              (instruction-template insn) nil
              (instruction-word insn) 0
              (instruction-parsed-operands insn) nil
              (instruction-address insn) nil
              ;; Every A64 instruction is one 4-byte word.  (%make-instruction
              ;; and the struct default agree; a recycled node must too, or
              ;; addressing/branch displacements come out wrong.)
              (instruction-size insn) 4)
        insn)
      (%make-instruction form))))

;;; Labels and branch fixups.
;;;
;;; Labels are zero-size elements spliced into the same doubly-linked
;;; section as instructions; a label therefore inherits the address of
;;; whatever instruction follows it.  Assembly is two-pass: pass one
;;; emits elements and, for each branch, records a (insn . reftype)
;;; reference on the target label without resolving it; FINALIZE is pass
;;; two, computing label addresses and patching branch displacements.

(ccl::def-standard-initial-binding *label-freelist*
                                   (ccl::make-dll-node-freelist))

;; A label definition
(defstruct (label (:include instruction-element)
                  (:constructor %%make-label (name)))
  name                                  ;a symbol
  refs)

(defun %make-label (name)
  (let ((lab (ccl::alloc-dll-node *label-freelist*)))
    (if lab
      (progn
        (setf (label-address lab) nil
              (label-refs lab) nil
              (label-name lab) name)
        name)
      (%%make-label name))))

(defun emit-element (seg element)
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
  (let ((lab (%make-label name)))
    (if (typep *labels* 'hash-table)
      (setf (gethash name *labels*) lab)
      (progn
        (push lab *labels*)
        (when (> (length *labels*) 255)
          (let ((hash (make-hash-table :size 512 :test #'eq)))
            (dolist (l *labels*)
              (setf (gethash (label-name l) hash) l))
            (setq *labels* hash)))))
    lab))

(defun find-label (name)
  (if (typep *labels* 'hash-table)
    (gethash name *labels*)
    (car (member name *labels* :test #'eq :key #'label-name))))

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
    (emit-element seg lab)))

(defmacro do-lap-labels ((lab &optional result) &body body)
  (let ((thunk (gensym))
        (k (gensym))
        (xlab (gensym)))
    `(flet ((,thunk (,lab) ,@body))
       (if (typep *labels* 'hash-table)
         (maphash (lambda (,k ,xlab)
                    (declare (ignore ,k))
                    (,thunk ,xlab))
                  *labels*)
         (dolist (,xlab *labels*)
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
        (*labels* ()))
    (dolist (form forms)
      (if (symbolp form)
        (emit-label seg form)
        ;; ASSEMBLE-INSTRUCTION already emits into SEG when given one.
        (assemble-instruction seg form)))
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

;;; Vinsn instruction "simplification" (done at definition time).
;;;
;;; The instructions in a vinsn body are written using a LAP-like
;;; syntax, but within vinsns, instruction operands may be
;;; parameterized: that is, the operands may include "holes" that
;;; will be filled in by vinsn parameters (results/args/temps)
;;; at vinsn expand time.
;;;
;;; The "simplification" involves sort of partially assembling the
;;; instructions: we can at least parse the operands and pre-select
;;; the instruction template so that we don't have to repeatedly
;;; do that at vinsn expansion time.

;;; A "simplified" vinsn instruction looks like
;;;
;;;    (ordinal . operand-descriptors)
;;;
;;; where ordinal is the index of the matched instruction template in
;;; *instruction-templates*, and each operand-descriptor says how to
;;; build one operand at expand time by filling in the holes with data
;;; provided in the variable-parts (vp) vector.  At vinsn expand time
;;; we have only to fill the holes and encode, reusing the assembler's
;;; encode-operands function.
;;;
;;; Operand descriptors look like this:
;;;
;;;   register    (:opnd index)   parameter register, or a label parameter
;;;               (:reg number)   literal register (vsp, sp, fn, ...)
;;;   label       (:local-label kw)   template-local branch target (:ok)
;;;   immediate   (:imm value shift) | (:imm-opnd index shift)
;;;               (:imm-apply shift fn arg...)   arg is (:opnd i), a constant,
;;;                                              or a nested (:apply fn arg...)
;;;   memory      (:mem marker base-desc off-desc)   off-desc nil / imm / reg
;;;   condition   (:cond value) | (:cond-opnd index [:invert])
;;;   shifted/ext (:shifted-reg reg-desc modifier amount)
;;;
;;; Template selection is first-match, exactly as the assembler does.  A
;;; hole whose value isn't known until expand time is treated as a wild
;;; immediate or wild condition that matches any operand spec of the right
;;; class; because such a hole can't disambiguate value-multiplexed aliases
;;; (mov's movz/movn/orr), a wild match must be unique or we signal an error
;;; asking the vinsn to name a concrete instruction.  Value-dependent checks
;;; (an immediate's range) are deferred to expand time.
;;;
;;; VINSN-SIMPLIFY-INSTRUCTION returns a second value as well: an
;;; opcode-alist entry recording enough to re-resolve the baked ordinal at
;;; load time, in case this file was compiled against a differently-ordered
;;; template table (see FIXUP-ARM64-VINSN-TEMPLATES).
;;;
;;; A form we can't simplify -- an unknown mnemonic, operands matching no
;;; template, or an operand shape not yet parseable (e.g. a parameterized
;;; shift amount) -- is returned unchanged.
;;;
;;; We derive an operand's width from its vreg's storage class: node and
;;; 64-bit unboxed values (:lisp/:u64/:s64/:address/:imm) use X; the
;;; 32-bit unboxed modes (:u32/:s32) use W.  arm64 GPRs have no B/H view,
;;; so 8/16-bit values are handled with W-form ops plus explicit
;;; extend/mask instructions, not a narrower register -- hence the width
;;; axis is strictly binary (W or X).
;;;
;;; This default keeps vinsn bodies clean (bare register names, no width
;;; tags) and is correct for essentially everything during bring-up,
;;; where all operands are node or 64-bit.  When a vinsn eventually wants
;;; a non-default view of a register (e.g. the W view of a node register,
;;; or forcing X on a :u32), add a per-operand override to the body
;;; syntax -- e.g. (:w reg) / (:x reg) resolved here -- rather than
;;; changing register identity.  Deferred until a concrete 32-bit vinsn
;;; needs it.
;;;
;;; Returns (values family width) or NIL.
(defun vinsn-gpr-class->family+width (class)
  (case class
    ((:lisp :lisp-lreg :imm :wordptr :u64 :s64 :address) (values :gpr 64))
    ((:u32 :s32 :u16 :s16 :u8 :s8) (values :gpr 32))
    (:single-float (values :fpr 32))
    (:double-float (values :fpr 64))
    (:complex-single-float (values :fpr 64))
    (:complex-double-float (values :fpr 128))
    (t nil)))

;;; Match-time stand-in for an immediate whose value isn't known until
;;; expand time (a const-parameter hole or an (:apply ...) expression).
;;; VINSN-MATCH-TEMPLATE lets it match any immediate-class operand spec;
;;; the actual range is checked at expand time.
(defparameter *wild-immediate* '#:wild-immediate)

;;; Match-time stand-in for a condition (:? cc) whose value comes from a
;;; parameter.  Matches a :cond / :cond-inv operand spec.
(defparameter *wild-condition* '#:wild-condition)

;;; Resolve one argument of an immediate (:apply fn ...) form to a
;;; dumpable descriptor: (:opnd index) for a parameter, a nested
;;; (:apply fn arg...) for a nested apply, else a constant.
(defun vinsn-imm-apply-arg (arg name-list)
  (cond
    ((and (consp arg) (eq (car arg) :apply))
     (list* :apply (cadr arg)
            (mapcar #'(lambda (a) (vinsn-imm-apply-arg a name-list))
                    (cddr arg))))
    ((and (symbolp arg) (position arg name-list :test #'eq))
     (list :opnd (position arg name-list :test #'eq)))
    (t (eval-immediate-expression arg))))

;;; Parse (:$ value) or (:$ value :lsl amount).  Returns two values: a
;;; dumpable descriptor and the literal value, or :UNKNOWN if the value
;;; isn't known until expand time (a const-parameter hole or (:apply ...)).
(defun vinsn-parse-immediate (op name-list)
  (destructuring-bind (value-form &optional lsl (shift-form 0)) (cdr op)
    (when (and lsl (not (eq lsl :lsl)))
      (error "Only :lsl is valid in a vinsn immediate: ~s" op))
    (let ((shift (eval-immediate-expression shift-form)))
      (cond
        ((and (consp value-form) (eq (car value-form) :apply))
         (values (list* :imm-apply shift (cadr value-form)
                        (mapcar #'(lambda (a)
                                    (vinsn-imm-apply-arg a name-list))
                                (cddr value-form)))
                 :unknown))
        ((and (symbolp value-form) (position value-form name-list :test #'eq))
         (values (list :imm-opnd (position value-form name-list :test #'eq)
                       shift)
                 :unknown))
        (t (let ((value (eval-immediate-expression value-form)))
             (values (list :imm value shift) value)))))))

;;; Parse a vinsn memory operand (:@ base [offset]) / (:@! ...) / (:@+ ...).
;;; Returns (values memory-operand descriptor), or nil if we lose.
(defun vinsn-parse-memory-operand (op name-list param-types)
  (destructuring-bind (marker base &optional offset) op
    (multiple-value-bind (base-mop base-desc)
        (vinsn-parse-operand base name-list param-types)
      (when (and base-mop (register-operand-p base-mop))
        (cond
          ((null offset)
           ;; plain register
           (values (make-memory-operand :base base-mop :offset nil
                                        :pre-indexed (eq marker :@!)
                                        :post-indexed (eq marker :@+))
                   (list :mem marker base-desc nil)))
          ((and (consp offset) (eq (car offset) :$))
           ;; immediate offset
           (multiple-value-bind (off-desc value)
               (vinsn-parse-immediate offset name-list)
             (values (make-memory-operand
                      :base base-mop
                      :offset (make-immediate-operand
                               :value (if (eq value :unknown) 0 value)
                               :shift 0)
                      :pre-indexed (eq marker :@!)
                      :post-indexed (eq marker :@+))
                     (list :mem marker base-desc off-desc))))
          (t
           ;; register offset
           (multiple-value-bind (off-mop off-desc)
               (vinsn-parse-operand offset name-list param-types)
             (when (and off-mop (register-operand-p off-mop))
               (values (make-memory-operand
                        :base base-mop :offset off-mop
                        :pre-indexed (eq marker :@!)
                        :post-indexed (eq marker :@+))
                       (list :mem marker base-desc off-desc))))))))))

;;; Try to parse a single vinsn instruction operand.  If we win,
;;; return two values: an operand struct (or wildcard marker) to be
;;; used in template matching, and an operand descriptor to
;;; save. Otherwise return nil.
(defun vinsn-parse-operand (op name-list param-types)
  (cond
    ;; A bare keyword names a template-local label (e.g. :ok): defined
    ;; elsewhere in this body and referenced here by a branch.
    ((keywordp op)
     (values (make-label-operand :name op) ;name is a placeholder for matching
             (list :local-label op)))
    ;; An immediate: (:$ value) or (:$ value :lsl amount).  A literal value
    ;; is matched by value (so e.g. (mov reg (:$ const)) picks the right
    ;; movz/movn/orr); an unknown value uses the wild marker.
    ((and (consp op) (eq (car op) :$))
     (multiple-value-bind (desc value) (vinsn-parse-immediate op name-list)
       (if (eq value :unknown)
         (values *wild-immediate* desc)
         (values (make-immediate-operand :value value :shift (caddr desc))
                 desc))))
    ;; A memory operand: (:@ base [offset]) and the writeback variants.
    ((and (consp op) (member (car op) '(:@ :@! :@+) :test #'eq))
     (vinsn-parse-memory-operand op name-list param-types))
    ;; A condition: (:? cc) or its inverse (:~ cc).  A parameter cc is
    ;; treated as a wild condition (value supplied at expand time; the
    ;; inversion, if any, is applied then); a literal condition name is
    ;; resolved and inverted now.
    ((and (consp op) (member (car op) '(:? :~) :test #'eq))
     (let* ((invert (eq (car op) :~))
            (cc (cadr op))
            (index (and (symbolp cc) (position cc name-list :test #'eq))))
       (if index
         (values *wild-condition*
                 (if invert
                   (list :cond-opnd index :invert)
                   (list :cond-opnd index)))
         (let ((value (need-arm64-condition-name cc)))
           (if invert
             (progn
               (unless (< value 14)
                 (error "condition ~s has no inverse" cc))
               (setq value (logxor value 1))
               (values (make-condition-operand
                        :name (lookup-arm64-condition-value value) :value value)
                       (list :cond value)))
             (values (make-condition-operand :name cc :value value)
                     (list :cond value)))))))
    ;; A vector arrangement operand: (:8b x) / (:16b x) / ... names Vx.<T>,
    ;; a whole SIMD register with an element arrangement (for cnt/addv/...).
    ;; x is a parameter hole or a literal FP register name.
    ((and (consp op) (consp (cdr op)) (null (cddr op))
          (member (car op) '(:8b :16b :4h :8h :2s :4s :1d :2d) :test #'eq)
          (symbolp (cadr op)))
     (let* ((arrangement (car op))
            (inner (cadr op))
            (hole (position inner name-list :test #'eq)))
       (if hole
         (values (make-vector-arrangement-operand
                  :register (fpr-ref 0 128) :arrangement arrangement)
                 (list :varr-opnd hole arrangement))
         (let ((reg (lookup-register inner)))
           (when reg
             (values (make-vector-arrangement-operand
                      :register (fpr-ref (register-number reg) 128)
                      :arrangement arrangement)
                     (list :varr-reg (register-number reg) arrangement)))))))
    ;; A vector lane operand: (:s x i) / (:d x i) names Vx.<S|D>[i], one
    ;; lane of a SIMD register (for ins/dup/mov element).  x is a parameter
    ;; hole or a literal FP register name; i is a constant lane index.  It's
    ;; the extra index element that tells this apart from the scalar view
    ;; below.  Only :s/:d are needed so far.
    ((and (consp op) (consp (cdr op)) (consp (cddr op)) (null (cdddr op))
          (member (car op) '(:s :d) :test #'eq)
          (symbolp (cadr op)))
     (let* ((esize (car op))
            (inner (cadr op))
            (index (eval-immediate-expression (caddr op)))
            (hole (position inner name-list :test #'eq)))
       (if hole
         (values (make-vector-element-operand
                  :register (fpr-ref 0 128) :esize esize :index index)
                 (list :velt-opnd hole esize index))
         (let ((reg (lookup-register inner)))
           (when reg
             (values (make-vector-element-operand
                      :register (fpr-ref (register-number reg) 128)
                      :esize esize :index index)
                     (list :velt-reg (register-number reg) esize index)))))))
    ;; A register-view override: (:b r) / (:h r) / (:s r) / (:d r) / (:w r)
    ;; / (:x r) forces the Bn/Hn/Sn/Dn scalar-FP view or the Wn/Xn GPR-width
    ;; view of the register r, ignoring its declared operand class.
    ((and (consp op) (consp (cdr op)) (null (cddr op))
          (member (car op) '(:b :h :s :d :w :x) :test #'eq)
          (symbolp (cadr op)))
     (multiple-value-bind (family width)
         (ecase (car op)
           (:b (values :fpr 8))
           (:h (values :fpr 16))
           (:s (values :fpr 32))
           (:d (values :fpr 64))
           (:w (values :gpr 32))
           (:x (values :gpr 64)))
       (flet ((ref (number)
                (if (eq family :gpr)
                  (gpr-ref number width)
                  (fpr-ref number width))))
         (let* ((inner (cadr op))
                (index (position inner name-list :test #'eq)))
           (if index
             ;; a parameter
             (values (make-register-operand :register (ref 0))
                     (list :opnd index))
             ;; a literal register name (e.g. (:x fn))
             (let ((reg (lookup-register inner)))
               (when reg
                 (values (make-register-operand :register
                                                (ref (register-number reg)))
                         (list :reg (register-number reg))))))))))
    ;; A shifted or extended register: (reg :lsl 3), (reg :uxtw), etc.  The
    ;; base register may be a hole or literal; the amount must be constant.
    ((and (consp op) (symbolp (car op))
          (or (member (cadr op) *shift-operators* :test #'eq)
              (member (cadr op) *extend-operators* :test #'eq)))
     (destructuring-bind (reg modifier &optional (amount-form 0)) op
       (multiple-value-bind (reg-mop reg-desc)
           (vinsn-parse-operand reg name-list param-types)
         (when (and reg-mop (register-operand-p reg-mop)
                    ;; a hole shift amount isn't supported yet -> fall back
                    (not (and (symbolp amount-form)
                              (position amount-form name-list :test #'eq))))
           (let ((amount (eval-immediate-expression amount-form)))
             (values (make-register-operand
                      :register (register-operand-register reg-mop)
                      :modifier modifier :amount amount)
                     (list :shifted-reg reg-desc modifier amount)))))))
    ((symbolp op)
     (let ((index (position op name-list :test #'eq)))
       (if index
         ;; a vinsn parameter
         (let ((class (cdr (assoc op param-types :test #'eq))))
           (if (eq class :label)
             ;; a branch target passed in as a backend (vinsn) label
             (values (make-label-operand :name op)
                     (list :opnd index))
             ;; a register parameter
             (multiple-value-bind (family width)
                 (vinsn-gpr-class->family+width class)
               (when family
                 (values (make-register-operand
                          :register (if (eq family :gpr)
                                      (gpr-ref 0 width)
                                      (fpr-ref 0 width)))
                         (list :opnd index))))))
         ;; a literal register name (e.g. vsp, sp, fn)
         (let ((reg (lookup-register op)))
           (when reg
             (values (make-register-operand :register reg)
                     (list :reg (register-number reg))))))))))

;;; Like match-template, but support *wild-immediate* and *wild-condition*
;;; markers.
(defun vinsn-match-template (template operands)
  (let ((specs (instruction-template-operand-specs template)))
    (and (= (length specs) (length operands))
         (every #'(lambda (operand spec)
                    (cond
                      ((eq operand *wild-immediate*)
                       (and (keywordp spec)
                            (not (member spec '(:cond :cond-inv :cond-b)))))
                      ((eq operand *wild-condition*)
                       (member spec '(:cond :cond-inv :cond-b)))
                      (t (match-operand operand spec))))
                operands specs))))

;;; Returns two values: the simplified body form, and (unless we fell
;;; back) an opcode-alist entry (ordinal name . operand-specs) recording
;;; enough to re-resolve the template's ordinal at load time.  See
;;; FIXUP-ARM64-VINSN-TEMPLATES.
(defun vinsn-simplify-instruction (form name-list &optional param-types)
  ;; This is like assemble-instruction.
  (destructuring-bind (name . opvals) form
    (let ((candidates (gethash (string name) *instruction-template-lists*)))
      (when candidates
        (let ((match-operands ())
              (descriptors ()))
          (dolist (op opvals)
            (multiple-value-bind (mop desc)
                (vinsn-parse-operand op name-list param-types)
              ;; If we don't understand an operand (like a
              ;; parameterized shift amount), return the instruction
              ;; form unsimplified, and the expander will choke on it
              ;; later.
              (unless mop
                (return-from vinsn-simplify-instruction form))
              (push mop match-operands)
              (push desc descriptors)))
          (setq match-operands (nreverse match-operands)
                descriptors (nreverse descriptors))
          ;; Select the template.  With all-concrete operands the first
          ;; matching template wins, exactly as the assembler does, so we
          ;; stop at the first match.  A wild immediate/condition hole,
          ;; though, can't disambiguate value-multiplexed aliases (e.g.
          ;; mov's movz/movn/orr forms): there we must prove the match is
          ;; unique -- scanning all candidates -- and otherwise complain
          ;; that the vinsn should use a specific (non-alias) instruction.
          (flet ((simplified (template)
                   (let ((ordinal (instruction-template-ordinal template)))
                     (return-from vinsn-simplify-instruction
                       (values (cons ordinal descriptors)
                               (list* ordinal
                                      (instruction-template-name template)
                                      (instruction-template-operand-specs
                                       template)))))))
            (if (or (member *wild-immediate* match-operands :test #'eq)
                    (member *wild-condition* match-operands :test #'eq))
              (let ((matches (remove-if-not
                              #'(lambda (tp)
                                  (vinsn-match-template tp match-operands))
                              candidates)))
                (when matches
                  (when (cdr matches)
                    (error "Ambiguous immediate or condition in vinsn ~
                            instruction ~s: ~d templates match; use a ~
                            specific (non-alias) instruction."
                           form (length matches)))
                  (simplified (car matches))))
              (dolist (tp candidates)
                (when (vinsn-match-template tp match-operands)
                  (simplified tp)))))))))
  ;; Unknown instruction, or no template matched: leave form as-is so the
  ;; rest of the file still compiles.  During bring-up this is expected for
  ;; not-yet-implemented vinsns; warn (don't halt) so the unmatched forms
  ;; stay visible, and let the expander choke only if one is actually used.
  (warn "arm64 vinsn: no template matched ~s" form)
  form)

(provide "ARM64-ASM")
