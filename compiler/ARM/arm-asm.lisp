;;;-*- Mode: Lisp; Package: (ARM :use CL) -*-
;;;
;;;   Copyright (C) 2005-2009 Clozure Associates and contributors.
;;;   This file is part of Clozure CL.
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License   known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict  the preamble takes precedence.
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(eval-when (:compile-toplevel :load-toplevel :execute)
(require "ARM-ARCH")
)

(in-package "ARM")

(defparameter *arm-condition-names* '(("eq" . 0) ("ne" . 1)
                                      ("hs" . 2) ("cs" . 2) ("lo" . 3) ("cc" . 3)
                                      ("mi" . 4) ("pl" . 5)
                                      ("vs" . 6) ("vc" . 7)
                                      ("hi" . 8) ("ls" . 9)
                                      ("ge" . 10) ("lt" . 11)
                                      ("gt" . 12) ("le" . 13)
                                      ("al" . 14)))



(defun lookup-arm-condition-name (name)
  (cdr (assoc name *arm-condition-names* :test #'string-equal)))

(defun lookup-arm-condition-value (val)
  (car (rassoc val *arm-condition-names* :test #'eq)))

(defun need-arm-condition-name (name)
  (or (lookup-arm-condition-name name)
      (error "Unknown ARM condition name ~s." name)))

(defvar *arm-constants* ())
(defvar *lap-labels* ())
(defvar *called-subprim-jmp-labels* ())




(defun arm-subprimitive-address (x)
  (if (and x (or (symbolp x) (stringp x)))
    (let* ((info (find x arm::*arm-subprims* :test #'string-equal :key #'ccl::subprimitive-info-name)))
      (when info
        (ccl::subprimitive-info-offset info)))))

(defun arm-subprimitive-name (addr)
  (let* ((info (find addr arm::*arm-subprims* :key #'ccl::subprimitive-info-offset)))
    (when info
      (string (ccl::subprimitive-info-name info)))))


(defun arm-constant-index (form)
  (let* ((idx (or (assoc form *arm-constants* :test 'equal)
                  (let* ((n (length *arm-constants*)))
                    (push (cons form n) *arm-constants*)
                    n))))
    (+ (ash (+ idx 2) arm::word-shift)  ; skip entrypoint, codevector
       arm::misc-data-offset)))

           

(defun need-constant (form)
  (if (ccl::quoted-form-p form)
    (let* ((quoted (ccl::nx-unquote form)))
      (if (null quoted)
        arm::canonical-nil-value
        (if (typep quoted '(signed-byte 30))
          (ash quoted arm::fixnumshift)
          (arm-constant-index quoted))))
    (progn
      (unless (and (consp form) (eq (keywordize (car form)) :$))
        (error "Invalid constant syntax in ~s" form))
      (destructuring-bind (val) (cdr form)
        (eval val)))))
                
                
(defstruct arm-instruction-template
  name
  ordinal                               ;if we need this
  val
  (flags 0)
  operand-types
  mask-list)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *arm-operand-types*
  #(:rd                                 ; destination register in bits 12:15
    :rn                                 ; unshifted source/base reg in 16:19
    :shifter                            ; composite operand for ALU ops
    :mem12                              ; 12-bit address for LDR/STR/LDRB/STB
    :reglist
    :rnw                                ; rn, with optional writeback.
    :uuoA                               ; GPR in UUO bits 8:11
    :uuo-unary                          ; constant in UUO bits 12:15
    :uuoB                               ; GPR in UUO bits 12:15
    :rm
    :b
    :subprim
    :mem8
    :dd
    :dm
    :sd
    :sm
    :dn
    :sn
    :rde
    :rs
    :fpaddr
    :@rn
    :uuoC
    :fpux
    :imm16
    ))

(defun %encode-arm-operand-type (name)
  (or (position name *arm-operand-types* :test #'eq)
      (error "Unknown ARM operand type name ~s." name)))

(defmacro encode-arm-operand-type (name)
  (%encode-arm-operand-type name))

(ccl::defenum (:prefix "ARM-INSTRUCTION-FLAG-")
  non-conditional                       ;doesn't use standard condition field
  prefer-separate-cond
  )

(defparameter *arm-instruction-flag-names*
  `((:non-conditional . ,arm-instruction-flag-non-conditional)
    (:prefer-separate-cond . ,arm-instruction-flag-prefer-separate-cond)
    ))

(defun %encode-arm-instruction-flag (name)
  (flet ((encode-one-instruction-type (name)
           (ash 1 (or (cdr (assoc name *arm-instruction-flag-names* :test #'eq))
                      (error "Unknown ARM instruction type: ~s" name)))))
    (if name
      (if (atom name)
        (encode-one-instruction-type name)
        (let* ((mask 0))
          (dolist (n name mask)
            (setq mask (logior mask (encode-one-instruction-type n))))))
      0)))
)

(defmacro encode-arm-instruction-flag (name)
  (%encode-arm-instruction-flag name))

(defvar *arm-instruction-ordinals* (make-hash-table :test #'equalp))



(defun %define-arm-instruction (name value mask-list flags operand-types)
  (make-arm-instruction-template :name name
                                    :val value
                                    :ordinal nil
                                    :mask-list mask-list
                                    :flags (or flags 0)
                                    :operand-types operand-types))

(defmacro define-arm-instruction (name operand-type-names value mask-list flag-names)
  `(%define-arm-instruction ,(string-downcase name) ,value ',mask-list ,(%encode-arm-instruction-flag flag-names) ',(mapcar #'%encode-arm-operand-type operand-type-names) ))



(defparameter *arm-instruction-table*
  (vector

   (define-arm-instruction clrex ()
     #xf57ff01f
     #xffffffff
     (:non-conditional))
   
;;; UUOs.

;;; Nullary UUOs
   (define-arm-instruction uuo-alloc-trap ()
     #x07f000f0
     #x0fffffff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-wrong-nargs ()
     #x07f001f8
     #x0fffffff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-gc-trap ()
     #x07f002f0
     #x0fffffff 
     (:prefer-separate-cond))
   (define-arm-instruction uuo-debug-trap ()
     #x07f003f0
     #x0fffffff 
     (:prefer-separate-cond))
   (define-arm-instruction uuo-interrupt-now ()
     #x07f004f0
     #x0fffffff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-suspend-now ()
     #x07f005f0
     #x0fffffff
     (:prefer-separate-cond))
;;; Misc format
   (define-arm-instruction uuo-error-reg-not-lisptag (:uuoA :uuo-unary)
     #x07f000f2
     #x0ff000ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-reg-not-fulltag (:uuoA :uuo-unary)
     #x07f000f3
     #x0ff000ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-reg-not-xtype (:uuoA :uuo-unary)
     #x07f000f4
     #x0ff000ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-cerror-reg-not-lisptag (:uuoA :uuo-unary)
     #x07f000fa
     #x0ff000ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-cerror-reg-not-fulltag (:uuoA :uuo-unary)
     #x07f000fb
     #x0ff000ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-cerror-reg-not-xtype (:uuoA :uuo-unary)
     #x07f000fc
     #x0ff000ff
     (:prefer-separate-cond))

;;; Unary UUOs
   (define-arm-instruction uuo-error-unbound (:uuoA)
     #x07f000f9
     #x0ffff0ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-cerror-unbound (:uuoA)
     #x07f010f9
     #x0ffff0ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-not-callable (:uuoA)
     #x07f020f9
     #x0ffff0ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-tlb-too-small (:uuoA)
     #x07f030f1
     #x0ffff0ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-no-throw-tag (:uuoA)
     #x07f040f9
     #x0ffff0ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-udf-call (:uuoA)
     #x07f050f9
     #x0ffff0ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-udf (:uuoA)
     #x07f060f9
     #x0ffff0ff
     (:prefer-separate-cond))
   
;;; Binary UUOs
   (define-arm-instruction uuo-error-vector-bounds (:uuoA :uuoB)
     #x07f000ff
     #x0fff00ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-array-bounds (:uuoA :uuoB)
     #x07f100ff
     #x0fff00ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-integer-divide-by-zero (:uuoA :uuoB)
     #x07f200ff
     #x0fff00ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-slot-unbound (:uuoA :uuoB :uuoC)
     #x07f000fe
     #x0ff000ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-eep-unresolved (:uuoA :uuoB)
     #x07f400ff
     #x0fff00ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-fpu-exception (:uuoA :uuoB)
     #x07f500ff
     #x0fff00ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-array-rank (:uuoA :uuoB)
     #x07f600ff
     #x0fff00ff
     (:prefer-separate-cond))
   (define-arm-instruction uuo-error-array-flags (:uuoA :uuoB)
     #x07f700ff
     #x0fff00ff
     (:prefer-separate-cond))     
   ;; Kernel services.
   (define-arm-instruction uuo-kernel-service (:uuo-unary)
     #x07f000fd
     #x0fff00ff
     (:prefer-separate-cond))

   ;; movw, movt require ARMv6T2 or later
   (define-arm-instruction movw (:rd :imm16)
     #x03000000
     #x0ff00000
     ())
   (define-arm-instruction movt (:rd :imm16)
     #x03400000
     #x0ff00000
     ())

   (define-arm-instruction and (:rd :rn :shifter)
     #x00000000
     ((#x02000000 . #x0ff00000)
      (#x00000000 . #x0ff00010)
      (#x00000010 . #x0ff00090))
     ())
   (define-arm-instruction ands (:rd :rn :shifter)
     #x00100000
     ((#x02100000 . #x0ff00000)
      (#x00100000 . #x0ff00010)
      (#x00100010 . #x0ff00090))
     ())
   (define-arm-instruction eor (:rd :rn :shifter)
     #x00200000
     ((#x02200000 . #x0ff00000)
      (#x00200000 . #x0ff00010)
      (#x00200010 . #x0ff00090))
     ())
   (define-arm-instruction eors (:rd :rn :shifter)
     #x00300000
     ((#x02300000 . #x0ff00000)
      (#x00300000 . #x0ff00010)
      (#x00300010 . #x0ff00090))
     ())
   (define-arm-instruction sub (:rd :rn :shifter)
     #x00400000
     ((#x02400000 . #x0ff00000)
      (#x00400000 . #x0ff00010)
      (#x00400010 . #x0ff00090))
     ())
   (define-arm-instruction subs (:rd :rn :shifter)
     #x00500000
     ((#x02500000 . #x0ff00000)
      (#x00500000 . #x0ff00010)
      (#x00500010 . #x0ff00090))
     ())
   (define-arm-instruction rsb (:rd :rn :shifter)
     #x00600000
     ((#x02600000 . #x0ff00000)
      (#x00600000 . #x0ff00010)
      (#x00600010 . #x0ff00090))
     ())
   (define-arm-instruction rsbs (:rd :rn :shifter)
     #x00700000
     ((#x02700000 . #x0ff00000)
      (#x00700000 . #x0ff00010)
      (#x00700010 . #x0ff00090))
     ())   
   (define-arm-instruction add (:rd :rn :shifter)
     #x00800000
     ((#x02800000 . #x0ff00000)
      (#x00800000 . #x0ff00010)
      (#x00800010 . #x0ff00090))
     ())
   (define-arm-instruction adds (:rd :rn :shifter)
     #x00900000
     ((#x02900000 . #x0ff00000)
      (#x00900000 . #x0ff00010)
      (#x00900010 . #x0ff00090))
     ())

   (define-arm-instruction adc (:rd :rn :shifter)
     #x00a00000
     ((#x02a00000 . #x0ff00000)
      (#x00a00000 . #x0ff00010)
      (#x00a00010 . #x0ff00090))
     ())
   (define-arm-instruction adcs (:rd :rn :shifter)
     #x00b00000
     ((#x02b00000 . #x0ff00000)
      (#x00b00000 . #x0ff00010)
      (#x00b00010 . #x0ff00090))
     ())
   (define-arm-instruction sbc (:rd :rn :shifter)
     #x00c00000
     ((#x02c00000 . #x0ff00000)
      (#x00c00000 . #x0ff00010)
      (#x00c00010 . #x0ff00090))
     ())
   (define-arm-instruction sbcs (:rd :rn :shifter)
     #x00d00000
     ((#x02d00000 . #x0ff00000)
      (#x00d00000 . #x0ff00010)
      (#x00d00010 . #x0ff00090))
     ())
   (define-arm-instruction rsc (:rd :rn :shifter)
     #x00e00000
     ((#x02e00000 . #x0ff00000)
      (#x00e00000 . #x0ff00010)
      (#x00e00010 . #x0ff00090))
     ())
   (define-arm-instruction rscs (:rd :rn :shifter)
     #x00e00000
     ((#x02e00000 . #x0ff00000)
      (#x00e00000 . #x0ff00010)
      (#x00e00010 . #x0ff00090))
     ())
   (define-arm-instruction tst (:rn :shifter)
     #x01100000
     ((#x03100000 . #x0ff00000)
      (#x01100000 . #x0ff00010)
      (#x01100010 . #x0ff00090))
     ())
   (define-arm-instruction tsts (:rn :shifter)
     #x01100000
     ((#x03100000 . #x0ff00000)
      (#x01100000 . #x0ff00010)
      (#x01100010 . #x0ff00090))
     ())
   (define-arm-instruction orr (:rd :rn :shifter)
     #x01800000
     ((#x03800000 . #x0ff00000)
      (#x01800000 . #x0ff00010)
      (#x01800010 . #x0ff00090))
     ())
   (define-arm-instruction orrs (:rd :rn :shifter)
     #x01900000
     ((#x03900000 . #x0ff00000)
      (#x01900000 . #x0ff00010)
      (#x01900010 . #x0ff00090))
     ())
   (define-arm-instruction bic (:rd :rn :shifter)
     #x01c00000
     ((#x03c00000 . #x0ff00000)
      (#x01c00000 . #x0ff00010)
      (#x01c00010 . #x0ff00090))
     ())
   (define-arm-instruction bics (:rd :rn :shifter)
     #x01d00000
     ((#x03d00000 . #x0ff00000)
      (#x01d00000 . #x0ff00010)
      (#x01d00010 . #x0ff00090))
     ())
   (define-arm-instruction cmp (:rn :shifter)
     #x01500000
     ((#x03500000 . #x0ff00000)
      (#x01500000 . #x0ff00010)
      (#x01500010 . #x0ff00090))
     ())
   (define-arm-instruction cmps (:rn :shifter)
     #x01500000
     ((#x03500000 . #x0ff00000)
      (#x01500000 . #x0ff00010)
      (#x01500010 . #x0ff00090))
     ())
   (define-arm-instruction cmn (:rd :shifter)
     #x01700000
     ((#x03700000 . #x0ff00000)
      (#x01700000 . #x0ff00010)
      (#x01700010 . #x0ff00090))
     ())

   (define-arm-instruction cmns (:rd :shifter)
     #x01700000
     ((#x03700000 . #x0ff00000)
      (#x01700000 . #x0ff00010)
      (#x01700010 . #x0ff00090))
     ())
   

  
   (define-arm-instruction mov (:rd :shifter)
     #x01a00000
     ((#x03a00000 . #x0ff00000)
      (#x01a00000 . #x0ff00010)
      (#x01a00010 . #x0ff00090))
     ())
   (define-arm-instruction movs (:rd :shifter)
     #x01b00000
     ((#x03b00000 . #x0ff00000)
      (#x01b00000 . #x0ff00010)
      (#x01b00010 . #x0ff00090))
     ())
   (define-arm-instruction mvn (:rd :shifter)
     #x01e00000
     ((#x03e00000 . #x0ff00000)
      (#x01e00000 . #x0ff00010)
      (#x01e00010 . #x0ff00090))
     ())
   (define-arm-instruction mvns (:rd :shifter)
     #x01f00000
     ((#x03f00000 . #x0ff00000)
      (#x01f00000 . #x0ff00010)
      (#x01f00010 . #x0ff00090))
     ())

   (define-arm-instruction ldr (:rd :mem12)
     #x04100000
     #x0c500000
     ())
   (define-arm-instruction ldrb (:rd :mem12)
     #x04500000
     #x0c500000
     ())
   (define-arm-instruction str (:rd :mem12)
     #x04000000
     #x0c500000
     ())
   (define-arm-instruction strb (:rd :mem12)
     #x04400000
     #x0c500000
     ())
   (define-arm-instruction ldrh (:rd :mem8)
     #x001000b0
     #x0e3000f0
     ())
   (define-arm-instruction strh (:rd :mem8)
     #x000000b0
     #x0e3000f0
     ())
   (define-arm-instruction ldrsh (:rd :mem8)
     #x001000f0
     #x0e3000f0
     ())
   (define-arm-instruction ldrsb (:rd :mem8)
     #x001000d0
     #x0e3000f0
     ())
   (define-arm-instruction ldrd  (:rde :mem8)
     #x000000d0
     #x0e1000f0
     ())
   (define-arm-instruction strd  (:rde :mem8)
     #x000000f0
     #x0e1000f0
     ())

   (define-arm-instruction mul (:rn :rm :rs)
     #x00000090
     #x0ff000f0
     ())
   (define-arm-instruction muls (:rn :rm :rs)
     #x00100090
     #x0ff000f0
     ())
   
   (define-arm-instruction stm (:rnw :reglist)
     #x08800000
     #x0fd00000
     ())
   (define-arm-instruction stmia (:rnw :reglist)
     #x08800000
     #x0fd00000
     ())
   (define-arm-instruction stmea (:rnw :reglist)
     #x08800000
     #x0fd00000
     ())
   (define-arm-instruction ldmia (:rnw :reglist)
     #x08900000
     #x0fd00000
     ())
   (define-arm-instruction ldm (:rnw :reglist)
     #x08900000
     #x0fd00000
     ())
   (define-arm-instruction ldmfd (:rnw :reglist)
     #x08900000
     #x0fd00000
     ())
   (define-arm-instruction stmdb (:rnw :reglist)
     #x09000000
     #x0fd00000
     ())
   (define-arm-instruction stmfb (:rnw :reglist)
     #x09000000
     #x0fd00000
     ())
   (define-arm-instruction stmfd (:rnw :reglist)
     #x09000000
     #x0ff00000
     ())
   (define-arm-instruction ldmdb (:rnw :reglist)
     #x09100000
     #x0fd00000
     ())
   (define-arm-instruction ldmea (:rnw :reglist)
     #x09100000
     #x0fd00000
     ())

   (define-arm-instruction b (:b)
     #x0a000000
     #x0f000000
     ())
   (define-arm-instruction bl (:b)
     #x0b000000
     #x0f000000
     ())
   ;; BA and BLA are indistinguishable from B/BL in their
   ;; generated code; they branch to/call subprim glue.
   (define-arm-instruction ba (:subprim)
     #x0a000000
     #x0f000000
     ())     
      (define-arm-instruction bla (:subprim)
     #x0b000000
     #x0f000000
     ())   
   (define-arm-instruction bx (:rm)
     #x012fff10
     #x0ffffff0
     ())
   (define-arm-instruction blx (:rm)
     #x012fff30
     #x0ffffff0
     ())

;;; VFP instructions
   (define-arm-instruction fabsd (:dd :dm)
     #x0eb00bc0
     #x0fff0ff0
     ())
   (define-arm-instruction fabss (:sd :sm)
     #x0eb00ac0
     #x0fbf0fd0
     ())
   (define-arm-instruction fnegd (:dd :dm)
     #x0eb10b40
     #x0fff0ff0
     ())
   (define-arm-instruction fnegs (:sd :sm)
     #x0eb10a40
     #x0fbf0fd0
     ())
   (define-arm-instruction fsqrtd (:dd :dm)
     #x0eb10bc0
     #x0fff0ff0
     ())
   (define-arm-instruction fsqrts (:sd :sm)
     #x0eb10ac0
     #x0bff0fb0
     ())   
   (define-arm-instruction faddd (:dd :dn :dm)
     #x0e300b00
     #x0ff00ff0
     ())
   (define-arm-instruction fadds (:sd :sn :sm)
     #x0e300a00
     #x0f300f50
     ())
   (define-arm-instruction fmsr (:sn :rd)
     #x0e000a10
     #x0ff00f90
     ())
   (define-arm-instruction fmrs (:rd :sn)
     #x0e100a10
     #x0ff00f90
     ())
   (define-arm-instruction fmrrd (:rd :rn :dm)
     #x0c500b10
     #x0ff00ff0
     ())
   (define-arm-instruction fmdrr (:dm :rd :rn)
     #x0c400b10
     #x0ff00ff0
     ())
   (define-arm-instruction fsitod (:dd :sm)
     #x0eb80bc0
     #x0fff0fc0
     ())
   (define-arm-instruction fsitos (:sd :sm)
     #x0eb80ac0
     #x0fff0fc0
     ())
   (define-arm-instruction fcmped (:dd :dm)
     #x0eb40bc0
     #x0fff0fc0
     ())
   (define-arm-instruction fcmpes (:sd :sm)
     #x0eb40ac0
     #x0fff0fc0
     ())
   (define-arm-instruction fmstat ()
     #x0ef1fa10
     #x0fffffff
     ())
   (define-arm-instruction fsubd (:dd :dn :dm)
     #x0e300b40
     #x0ff00fc0
     ())
   (define-arm-instruction fsubs (:sd :sn :sm)
     #x0e300a40
     #x0ff00fc0
     ())
   (define-arm-instruction fmuld (:dd :dn :dm)
     #x0e200b00
     #x0ff00ff0
     ())
   (define-arm-instruction fmuls (:sd :sn :sm)
     #x0e200a00
     #x0ff00f50
     ())
   (define-arm-instruction fdivd (:dd :dn :dm)
     #x0e800b00
     #x0ff00ff0
     ())
   (define-arm-instruction fdivs (:sd :sn :sm)
     #x0e800a00
     #x0ff00f50
     ())
   (define-arm-instruction fcpyd (:dd :dm)
     #x0eb00b40
     #x0fb00ff0
     ())
   (define-arm-instruction fcpys (:sd :sm)
     #x0eb00a40
     #x0fb00fc0
     ())
   (define-arm-instruction fcvtsd (:sd :dm)
     #x0eb70bc0
     #x0fbf0ff0
     ())
   (define-arm-instruction fcvtds (:dd :sm)
     #x0eb70ac0
     #x0ff70ac0
     ())
   (define-arm-instruction fmxr (:fpux :rd)
     #x0ee00a10
     #x0ff00fff
     ())
   (define-arm-instruction fmrx (:rd :fpux)
     #x0ef00a10
     #x0ff00fff
     ())
   (define-arm-instruction smull (:rd :rn :rm :rs)
     #x00c00090
     #x0ff000f0
     ())
   (define-arm-instruction smulls (:rd :rn :rm :rs)
     #x00d00090
     #x0ff000f0
     ())
   (define-arm-instruction umull (:rd :rn :rm :rs)
     #x00800090
     #x0ff000f0
     ())
   (define-arm-instruction umulls (:rd :rn :rm :rs)
     #x00900090
     #x0ff000f0
     ())

   (define-arm-instruction fstd (:dd :fpaddr)
     #x0d000b00
     #x0f700f00
     ())
   (define-arm-instruction fsts (:sd :fpaddr)
     #x0d000a00
     #x0f300f00
     ())
   (define-arm-instruction fldd (:dd :fpaddr)
     #x0d100b00
     #x0f700f00
     ())     
   (define-arm-instruction flds (:sd :fpaddr)
     #x0d100a00
     #x0f300f00
     ())
   (define-arm-instruction ftosid (:sd :dm)
     #x0ebd0b40
     #x0fbf0fc0
     ())
   (define-arm-instruction ftosizd (:sd :dm)
     #x0ebd0bc0
     #x0fbf0fc0
     ())
   (define-arm-instruction ftosis (:sd :sm)
     #x0ebd0a40
     #x0fbf0fc0
     ())
   (define-arm-instruction ftosizs (:sd :sm)
     #x0ebd0ac0
     #x0fbf0fc0
     ())   
   (define-arm-instruction ldrex (:rd :@rn)
     #x01900f9f
     #x0ff00fff
     ())
   (define-arm-instruction strex (:rd :rm :@rn)
     #x01800f90
     #x0ff00ff0
     ())
   (define-arm-instruction clz (:rd :rm)
     #x016f0f10
     #x0fff0ff0
     ())
 ))

(dotimes (i (length *arm-instruction-table*))
  (let* ((template (svref *arm-instruction-table* i))
         (name (arm-instruction-template-name template)))
    (setf (arm-instruction-template-ordinal template) i
          (gethash name *arm-instruction-ordinals*) i)))

    



(defun lookup-arm-instruction (name)
  ;; return (values instruction template & condition value), or (NIL NIL)
  (let* ((cond-value #xe)              ;always
         (string (string name))
         (len (length string))
         (ordinal (gethash string *arm-instruction-ordinals*))
         (template (if ordinal (aref *arm-instruction-table* ordinal))))
    (if template
      (if (logtest (encode-arm-instruction-flag :non-conditional) (arm-instruction-template-flags template))
        (let* ((cond (ldb (byte 4 28) (arm-instruction-template-val template))))
          (values template cond cond))
        (values template cond-value nil))
      (if (> len 2)
        (let* ((cond-name (make-string 2)))
          (declare (dynamic-extent cond-name))
          (setf (schar cond-name 0)
                (schar string (- len 2))
                (schar cond-name 1)
                (schar string (- len 1)))
          (if (setq cond-value (lookup-arm-condition-name cond-name))
            (let* ((prefix-len (- len 2))
                   (prefix (make-string prefix-len)))
              (declare (dynamic-extent prefix)
                       (fixnum prefix-len))
              (dotimes (i prefix-len)
                (setf (schar prefix i) (schar string i)))
              (if (setq template
                        (progn
                          (setq ordinal (gethash prefix *arm-instruction-ordinals*))
                          (when ordinal
                            (svref *arm-instruction-table* ordinal))))
                (if (logbitp (encode-arm-instruction-flag :non-conditional) (arm-instruction-template-flags template))
                  (values nil nil nil)
                  (values template cond-value t))
                (values nil nil nil)))
            (values nil nil nil)))
        (values nil nil nil)))))

(defun keywordize (name)
  (if (typep name 'keyword)
    name
    (intern (string-upcase (string name)) "KEYWORD")))

(defun arm-rotate-left (u32 nbits)
  (assert (and (evenp nbits)
               (>= nbits 0)
               (< nbits 32)))
  (let* ((r (- 32 nbits))
         (mask (1- (ash 1 nbits))))
    (logand #xffffffff
            (logior (ash u32 nbits)
                    (logand mask
                            (ash  u32 (- r)))))))

;;; Return a 12-bit value encoding u32 as an 8-bit constant rotated
;;; by an even number of bits if u32 can be encoded that way, nil
;;; otherwise.
#-arm-target
(defun encode-arm-immediate (u32)
  (do* ((u32 (logand #xffffffff u32))
        (rot 0 (+ rot 2)))
       ((= rot 32) (values nil nil))
    (let* ((a (arm-rotate-left u32 rot)))
      (when (<= a #xff)
        (return (logior (ash rot 7) a))))))

#+arm-target
(ccl::defarmlapfunction encode-arm-immediate ((u32 arg_z))
  (check-nargs 1)
  (extract-typecode imm0 u32)
  (cmp imm0 (:$ arm::tag-fixnum))
  (moveq imm0 (:asr u32 (:$ arm::fixnumshift)))
  (beq @got)
  (cmp imm0 (:$ arm::subtag-bignum))
  (uuo-error-reg-not-xtype (:? ne) u32 (:$ arm::xtype-integer))
  (ldr imm0 (:@ u32 (:$ arm::misc-data-offset)))
  @got
  (mov imm1 (:$ 32))
  (mov imm2 imm0)
  @loop
  (cmp imm2 (:$ 256))
  (blo @win)
  (subs imm1 imm1 (:$ 2))
  (moveq arg_z 'nil)
  (bxeq lr)
  (mov imm2 (:ror imm0 imm1))
  (b @loop)
  @win
  (rsb imm1 imm1 (:$ 32))
  (orr imm0 imm2 (:lsl imm1 (:$ 7)))
  (box-fixnum arg_z imm0)
  (bx lr))

(eval-when (:execute :load-toplevel)
  (defstruct (instruction-element (:include ccl::dll-node))
    address
    (size 0))

;;; A LAP-INSTRUCTION's field-values list contains (byte-spec . value)
;;; pairs, where the byte-spec is encoded as a fixnum.  If the BYTE-SIZE
;;; of the byte-spec is non-zero, the value is to be inserted in the
;;; instruction by DPB; if the BYTE-SIZE is zero, the BYTE-POSITION of

;;; the byte-spec is used to select a function which affects arbitrary
;;; bitfields in the instruction.  (E.g., a negative constant in an ADD
;;; instruction might turn the instruction into a SUB.)
;;; The relationship between logical operands and field-values isn't
;;; necessarily 1:1.
;;; For vinsn expansion, the field values with constant values can
;;; be applied at vinsn-definition time.
  
  (defstruct (lap-instruction (:include instruction-element (size 4))
                              (:constructor %make-lap-instruction (source)))
    source                              ; for LAP, maybe vinsn-template
    (opcode-high 0)
    (opcode-low 0)
    )

    
  (defstruct (lap-label (:include instruction-element)
                            (:constructor %%make-lap-label (name)))
    name
    refs))

(ccl::def-standard-initial-binding *lap-label-freelist* (ccl::make-dll-node-freelist))
(ccl::def-standard-initial-binding *lap-instruction-freelist* (ccl::make-dll-node-freelist))


(defun set-opcode-values (high low bytespec value)
  (declare (type (unsigned-byte 16) low high))
  (let* ((width (byte-size bytespec))
         (pos (byte-position bytespec)))
    (declare (type (unsigned-byte 5) width pos))
    (cond ((<= (the fixnum (+ width pos)) 16)
           (values high (dpb value bytespec low)))
          ((>= pos 16)
           (values (dpb value (byte width (- pos 16)) high) low))
          ;; Branch displacements are about the only things
          ;; that span the two halves of an instruction.
          (t
           (let* ((low-width (- 16 pos))
                  (high-width (- width low-width)))
             (declare (fixnum low-width high-width))
             (values (dpb (ldb (byte high-width low-width) value)
                          (byte high-width 0)
                          high)
                     (dpb (ldb (byte low-width 0) value)
                        (byte low-width pos)
                        low)))))))

(defun set-field-value (instruction bytespec value)
  (let* ((low (lap-instruction-opcode-low instruction))
         (high (lap-instruction-opcode-high instruction)))
    (declare (type (unsigned-byte 16) low high))
    (multiple-value-bind (new-high new-low)
        (set-opcode-values high low bytespec value)
      (declare (type (unsigned-byte 16) new-low new-high))
      (unless (eql low new-low)
        (setf (lap-instruction-opcode-low instruction) new-low))
      (unless (eql high new-high)
        (setf (lap-instruction-opcode-high instruction) new-high)))))


(defun get-opcode-field (high low bytespec)
  (declare (fixnum high low))
  (let* ((width (byte-size bytespec))
         (pos (byte-position bytespec)))
    (declare (fixnum width pos))
    (cond ((<= (the fixnum (+ width pos)) 16)
           (ldb bytespec low))
          ((>= pos 16)
           (ldb (byte width (- pos 16)) high))
          ;; Branch displacements are about the only things
          ;; that span the two halves of an instruction.
          (t
           (let* ((low-width (- 16 pos))
                  (high-width (- width low-width)))
             (declare (fixnum low-width high-width))
             (dpb (ldb (byte high-width 0) high)
                  (byte high-width low-width)
                  (ldb (byte low-width pos) low)))))))
  
(defun get-field-value (instruction bytespec)
  (get-opcode-field (lap-instruction-opcode-high instruction)
                    (lap-instruction-opcode-low instruction)
                    bytespec))


(defun need-arm-gpr (form)
  (or (get-arm-gpr form)
      (error "Expected an ARM general-purpose register, got ~s" form)))

(defun need-arm-sfpr (form)
  (or (get-arm-sfpr form)
      (error "Expected an ARM single FP register, got ~s" form)))

(defun need-arm-dfpr (form)
  (or (get-arm-dfpr form)
      (error "Expected an ARM double FP register, got ~s" form)))

(defun encode-arm-shift-type (op)
  (case op
    (:lsl 0)
    (:lsr 1)
    (:asr 2)
    (:ror 3)))


(defconstant opcode-and 0)
(defconstant opcode-eor 1)
(defconstant opcode-sub 2)
(defconstant opcode-rsb 3)
(defconstant opcode-add 4)
(defconstant opcode-adc 5)
(defconstant opcode-sbc 6)
(defconstant opcode-rsc 7)
(defconstant opcode-tst 8)
(defconstant opcode-teq 9)
(defconstant opcode-cmp 10)
(defconstant opcode-cmn 11)
(defconstant opcode-orr 12)
(defconstant opcode-mov 13)
(defconstant opcode-bic 14)
(defconstant opcode-mvn 15)

(defparameter *equivalent-complemented-opcodes*
  (vector opcode-bic                    ;and->bic
          nil                           ;eor->
          nil                           ;sub->
          nil                           ;rsb->
          nil                           ;add->
          opcode-sbc                    ;adc->sbc
          opcode-adc                    ;sbc->adc
          nil                           ;rsc->
          nil                           ;tst->
          nil                           ;teq->
          nil                           ;cmp->
          nil                           ;cmn->
          nil                           ;orr->
          opcode-mvn                    ;mov->mvn
          opcode-and                    ;bic->and
          opcode-mov                    ;mvn->mov
          ))

(defparameter *equivalent-negated-opcodes*
  (vector nil                           ;and->
          nil                           ;eor->
          opcode-add                    ;sub->add
          nil                           ;rsb->
          opcode-sub                    ;add->sub
          nil                           ;adc->
          nil                           ;sbc->
          nil                           ;rsc->
          nil                           ;tst->
          nil                           ;teq->
          opcode-cmn                    ;cmp->cmn
          opcode-cmp                    ;cmn->cmp
          nil                           ;orr->
          nil                           ;mov->
          nil                           ;bic->
          nil                           ;mvn->
          ))


   
(defun parse-rd-operand (form instruction)
  (set-field-value instruction (byte 4 12) (need-arm-gpr form)))

(defun parse-rn-operand (form instruction)
  (set-field-value instruction (byte 4 16) (need-arm-gpr form)))

(defun parse-shifter-operand (form instruction)
  (if (atom form)
    ;; rm is shorthand for (:lsl rm (:$ 0)); the :lsl is encoded as 0.
    (set-field-value instruction (byte 12 0) (need-arm-gpr form))
    (if (ccl::quoted-form-p form)
      (insert-shifter-constant (need-constant form) instruction)
      (let* ((op (keywordize (car form))))
        (ecase op
          (:$ (destructuring-bind (value) (cdr form)
                (insert-shifter-constant (eval value) instruction)))
          (:rrx (destructuring-bind (reg) (cdr form)
                  (set-field-value instruction (byte 12 0)
                                   (logior (need-arm-gpr reg)
                                           (ash (encode-arm-shift-type :ror) 5)))))
          ((:lsl :lsr :asr :ror)
           (destructuring-bind (reg count) (cdr form)
             (if (atom count)
               (set-field-value instruction (byte 12 0)
                                (logior (need-arm-gpr reg)
                                        (ash 1 4)
                                        (ash (encode-arm-shift-type op) 5)
                                        (ash (need-arm-gpr count) 8)))
               (ecase (keywordize (car count))
                 (:$ (destructuring-bind (countval) (cdr count)
                       (set-field-value instruction (byte 12 0)
                                        (logior (need-arm-gpr reg)
                                                (ash (encode-arm-shift-type op) 5)
                                                (ash (logand 31 (eval countval)) 7))))))))))))))
      
(defun insert-shifter-constant (value instruction)
  (let* ((constant (encode-arm-immediate value)))
    (cond (constant
            (set-field-value instruction (byte 12 0) constant)
            (set-field-value instruction (byte 1 25) 1))
          (t
           ;; If value couldn't be encoded but its complement can be
           ;; and there's an instruction that can operate on complemented
           ;; values, change the instruction and encode the complemented
           ;; value.  If that doesn't work, try negating the value and
           ;; seeing if there's an equivalent instruction that could use
           ;; that.  If none of this works, complain that the value can't
           ;; be encoded.
           (let* ((op (get-field-value instruction (byte 4 21)))
                  (newop nil))
             (if (or (and (setq constant (encode-arm-immediate (lognot value)))
                          (setq newop (svref *equivalent-complemented-opcodes* op)))
                     (and (setq constant (encode-arm-immediate (- value)))
                          (setq newop (svref *equivalent-negated-opcodes* op))))
               (progn
                 (set-field-value instruction (byte 1 25) 1)
                 (set-field-value instruction (byte 12 0) constant)
                 (set-field-value instruction (byte 4 21) newop))
               (error "Can't encode ARM constant ~s." value)))))))

(defun set-opcode-value-from-addressing-mode (high mode constant-index)
  ;; Look at mode and set P/W/U bits.  If CONSTANT-INDEX is
  ;; true, the U bit depends on the sign of the constant.
  (ecase mode            
    ((:@ :+@ :+@! :@!)
     ;; Preindexed, no writeback unless :[+]@! , add register operands.
     (unless constant-index
       (setq high (logior high (ash 1 (- 23 16)))))
     (when (or (eq mode :+@!)
               (eq mode :@!))
       (setq high (logior high (ash 1 (- 21 16)))))
     (setq high (logior high (ash 1 (- 24 16)))))
    ((:-@ :-@!)
     ;; Preindexed. Leave the U bit clear, maybe set W if writeback.
     (when (eq mode :-@!)
       (setq high (logior high (ash 1 (- 21 16)))))
     (setq high (logior high (ash 1 (- 24 16)))))
    ((:@+ :@-)
     ;; Postindex; writeback is implicit (and setting P and W would
     ;; change the instruction.)
     (unless (or (eq mode :@-) constant-index)
       (setq high (logior high (ash 1 (- 23 16)))))))
  high)


(defun set-addressing-mode (instruction mode constant-index)
  (setf (lap-instruction-opcode-high instruction)
        (set-opcode-value-from-addressing-mode
         (lap-instruction-opcode-high instruction)
         mode
         constant-index)))


;;; "general" address operand, as used in LDR/LDRB/STR/STRB
(defun parse-m12-operand (form instruction)
  (if (atom form)
    (error "Invalid memory operand ~s" form)    
    (let* ((mode (keywordize (car form))))
      (if (eq mode :=)
        (destructuring-bind (label) (cdr form)
          (when (arm::arm-subprimitive-address label)
            (error "Invalid label in ~s." form))
          (set-field-value instruction (byte 4 16) arm::pc)
          (set-field-value instruction (byte 1 24) 1) ;P bit
          ;; Insert function will have to set U bit appropriately.
          (lap-note-label-reference label instruction :mem12))
        (destructuring-bind (rn &optional (index '(:$ 0) index-p)) (cdr form)
          (unless (or index-p (eq mode :@))
            (error "missing index in memory operand ~s." form))
          (set-field-value instruction (byte 4 16) (need-arm-gpr rn))
          (let* ((quoted (ccl::quoted-form-p index))
                 (index-op (if quoted :quote (and (consp index) (keywordize (car index)))))
                 (constant-index (or quoted (eq index-op :$))))
            (cond (constant-index
                   (destructuring-bind (val) (cdr index)
                     (let* ((constval (if quoted
                                        (need-constant index)
                                        (eval val))))
                       (if (< constval 0)
                         (setq constval (- constval))
                         ;; das u bit
                         (set-field-value instruction (byte 1 23) 1))
                       (unless (typep constval '(unsigned-byte 12))
                         (warn "constant offset too large : ~s" constval))
                       (set-field-value instruction (byte 12 0) constval))))
                  (t
                   (set-field-value instruction (byte 1 25) 1)
                   (if (atom index)
                     (set-field-value instruction (byte 12 0) (need-arm-gpr index))
                     ;; Shifts here are always by a constant (not another reg)
                     (if (eq index-op :rrx)
                       (destructuring-bind (rm) (cdr index)
                         (set-field-value instruction (byte 12 0)
                                          (logior (need-arm-gpr rm)
                                                  (ash (encode-arm-shift-type :ror) 5))))
                     
                       (destructuring-bind (rm shift-expr) (cdr index)
                         (unless (and (consp shift-expr)
                                      (eq (keywordize (car shift-expr)) :$))
                           (error "Shift count must be immediate : ~s" shift-expr))
                         (destructuring-bind (count-expr) (cdr shift-expr)
                           (set-field-value instruction (byte 12 0)
                                            (logior (need-arm-gpr rm)
                                                    (ash (encode-arm-shift-type
                                                          index-op) 5)
                                                    (ash (logand 31 (eval count-expr))
                                                         7)))))))))
            (set-addressing-mode instruction mode constant-index)))))))

(defun parse-reglist-operand (form instruction)
  (let* ((mask 0))
    (dolist (r form)
      (let* ((regno (need-arm-gpr r)))
        (when (logbitp regno mask)
          (warn "Duplicate register ~s in ~s." r form))
        (setq mask (logior mask (ash 1 regno)))))
    (if (zerop mask)
      (error "Empty register list ~s." form)
      (set-field-value instruction (byte 16 0) mask))))

(defun parse-rnw-operand (form instruction)
  (if (atom form)
    (set-field-value instruction (byte 4 16) (need-arm-gpr form))
    (if (eq (keywordize (car form)) :!)
      (destructuring-bind (rn) (cdr form)
        (set-field-value instruction (byte 1 21) 1)
        (set-field-value instruction (byte 4 16) (need-arm-gpr rn)))
      (error "Unrecognized writeback indicator in ~s." form))))

(defun parse-uuoA-operand (form instruction)
  (set-field-value instruction (byte 4 8) (need-arm-gpr form)))

(defun parse-uuo-unary-operand (form instruction)
  (set-field-value instruction (byte 8 12) (need-constant form)))

(defun parse-uuoB-operand (form instruction)
  (set-field-value instruction (byte 4 12) (need-arm-gpr form)))

(defun parse-uuoC-operand (form instruction)
  (set-field-value instruction (byte 4 16) (need-arm-gpr form)))

(defun parse-fpux-operand (form instruction)
  (let* ((regno (if (typep form '(unsigned-byte 4))
                  form
                  (ecase (keywordize form)
                    (:fpsid 0)
                    (:fpscr 1)
                    (:fpexc 8)))))
    (set-field-value instruction (byte 4 16) regno)))

(defun parse-imm16-operand (form instruction)
  (unless (and (consp form)
               (eq (keywordize (car form)) :$)
               (consp (cdr form))
               (null (cddr form)))
    (error "Bad 16-bit immediate operand: ~s" form))
  (let* ((val (eval (cadr form))))
    (set-field-value instruction (byte 12 0) (ldb (byte 12 0) val))
    (set-field-value instruction (byte 4 16) (ldb (byte 4 12) val))))
    

(defun parse-rm-operand (form instruction)
  (set-field-value instruction (byte 4 0) (need-arm-gpr form)))

(defun parse-b-operand (form instruction)
  (lap-note-label-reference form instruction :b))

(defun parse-subprim-operand (form instruction)
  (multiple-value-bind (addr name)
      (if (typep form 'fixnum)
        (values form
                (arm-subprimitive-name form))
        (values (arm-subprimitive-address form)
                form))
    (unless (and name addr)
      (error "~s is not the name or address of an ARM subprimitive." form))
    (let* ((lab (or (find-lap-label name)
                    (make-lap-label name))))
      (pushnew lab *called-subprim-jmp-labels*)
      (push (cons instruction :b) (lap-label-refs lab)))))


    
(defun parse-m8-operand (form instruction)
  (if (atom form)
    (error "Invalid memory operand ~s." form)
    (let* ((mode (keywordize (car form)))
           (constant-index nil))
      (destructuring-bind (rn index) (cdr form)
        (set-field-value instruction (byte 4 16) (need-arm-gpr rn))
        (cond ((atom index)
               (set-field-value instruction (byte 4 0) (need-arm-gpr index)))
              (t (unless (eq (keywordize (car index)) :$)
                   (error "Invalid index: ~s." index))
                 (destructuring-bind (val) (cdr index)
                   (let* ((value (eval val)))
                     (setq constant-index t)
                     (if (< value 0)
                       (setq value (- value))
                       (set-field-value instruction (byte 1 23) 1))
                     (set-field-value instruction (byte 1 22) 1)
                     (set-field-value instruction (byte 4 0) (ldb (byte 4 0) value))
                     (set-field-value instruction (byte 4 8) (ldb (byte 4 4) value)))))))
      (set-addressing-mode instruction mode constant-index))))

(defun parse-dd-operand (form instruction)
  (set-field-value instruction (byte 4 12) (need-arm-dfpr form)))

(defun parse-dm-operand (form instruction)
  (set-field-value instruction (byte 4 0) (need-arm-dfpr form)))

(defun parse-sd-operand (form instruction)
  (let* ((val (need-arm-sfpr form)))
    (set-field-value instruction (byte 4 12) (ash val -1))
    (set-field-value instruction (byte 1 22) (logand val 1))))

(defun parse-sm-operand (form instruction)
  (let* ((val (need-arm-sfpr form)))
    (set-field-value instruction (byte 4 0) (ash val -1))
    (set-field-value instruction (byte 1 5) (logand val 1))))

(defun parse-dn-operand (form instruction)
  (set-field-value instruction (byte 4 16) (need-arm-dfpr form)))        
                             
(defun parse-sn-operand (form instruction)
  (let* ((val (need-arm-sfpr form)))
    (set-field-value instruction (byte 4 16) (ash val -1))
    (set-field-value instruction (byte 1 7) (logand val 1))))

(defun parse-rde-operand (form instruction)
  (let* ((val (need-arm-gpr form)))
    (when (oddp val)
      (error "Register must be even-numbered: ~s." form))
    (set-field-value instruction (byte 4 12) val)))

(defun parse-rs-operand (form instruction)
  (set-field-value instruction (byte 4 8) (need-arm-gpr form)))

(defun parse-fpaddr-operand (form instruction)
  (if (atom form)
    (error "Invalid FP address: ~s" form)
    (destructuring-bind (op rn offset) form
      (unless (eq (keywordize op) :@)
        (error "Invalid FP addressing mode ~s in ~s." op form))
      (set-field-value instruction (byte 4 16) (need-arm-gpr rn))
      (unless (and (consp offset) (eq (keywordize (car offset)) :$))
        (error "Invalid FP address offset ~s in ~s." offset form))
      (destructuring-bind (offset-form) (cdr offset)
        (let* ((offset-val (eval offset-form)))
          (when (logtest offset-val 3)
            (error "FP address offset ~s must be a multiple of 4 in ~s." offset form))
          (if (< offset-val 0)
            (setq offset-val (- offset-val))
            (set-field-value instruction (byte 1 23) 1))
          (set-field-value instruction (byte 8 0) (ash offset-val -2)))))))

(defun parse-@rn-operand (form instruction)
  (when (or (atom form)
          (not (eq (keywordize (car form)) :@)))
    (error "Invalid register indirect operand: ~s" form))
  (destructuring-bind (rn) (cdr form)
    (set-field-value instruction (byte 4 16) (need-arm-gpr rn))))
  
(defparameter *arm-operand-parsers*
    #(parse-rd-operand
      parse-rn-operand
      parse-shifter-operand
      parse-m12-operand
      parse-reglist-operand
      parse-rnw-operand
      parse-uuoa-operand
      parse-uuo-unary-operand
      parse-uuob-operand
      parse-rm-operand
      parse-b-operand
      parse-subprim-operand
      parse-m8-operand
      parse-dd-operand
      parse-dm-operand
      parse-sd-operand
      parse-sm-operand
      parse-dn-operand
      parse-sn-operand
      parse-rde-operand
      parse-rs-operand
      parse-fpaddr-operand
      parse-@rn-operand
      parse-uuoc-operand
      parse-fpux-operand
      parse-imm16-operand
      ))



(defun make-lap-instruction (form)
  (let* ((insn (ccl::alloc-dll-node *lap-instruction-freelist*)))
    (if (typep insn 'lap-instruction)
      (progn
        (setf (lap-instruction-source insn) form
              (lap-instruction-address insn) nil
              (lap-instruction-opcode-low insn) 0
              (lap-instruction-opcode-high insn) 0)
        insn)
      (%make-lap-instruction form))))

(defun emit-lap-instruction-element (insn seg)
  (ccl::append-dll-node insn seg)
  (let* ((addr (let* ((prev (ccl::dll-node-pred insn)))
                 (if (eq prev seg)
                   0
                   (the fixnum (+ (the fixnum (instruction-element-address prev))
                                  (the fixnum (instruction-element-size prev))))))))
    (setf (instruction-element-address insn) addr))
  insn)
  
;;; FORM is a list and its car isn't a pseudo-op or lapmacro; try to
;;; generate an instruction.
(defun assemble-instruction (seg form)
  (let* ((insn (make-lap-instruction form)))
    (destructuring-bind (name . opvals) form
      (multiple-value-bind (template cond explicit-cond) (lookup-arm-instruction name)
        (unless template
          (error "Unknown ARM instruction - ~s" form))
        (let* ((cond-indicator (and (consp (car opvals))
                                    (keywordize (caar opvals)))))
          (when (or (eq cond-indicator :?)
                    (eq cond-indicator :~))
            (let* ((condform (pop opvals)))
              (destructuring-bind (q cond-name) condform
                (declare (ignore q))
                (let* ((c (need-arm-condition-name cond-name)))
                  (when (eq cond-indicator :~)
                    (if (< c 14)
                      (setq c (logxor c 1))
                      (error "Invalid explicit condition ~s." condform)))
                  (if (and explicit-cond (not (eql c cond)))
                    (error "Can't use explicit condition and :? : ~s" condform)
                    (setq cond c)))))))
        (let* ((optypes (arm-instruction-template-operand-types template))
               (n (length optypes)))
          (unless (= n (length opvals))
            (error "ARM ~a instructions require ~d operands, but ~d were provided in ~s." (arm-instruction-template-name template) n (length opvals) form))
          (set-field-value insn (byte 32 0) (arm-instruction-template-val template))
          (dotimes (i n)
            (let* ((optype (pop optypes))
                   (val (pop opvals)))
              (funcall (svref *arm-operand-parsers* optype) val insn)))
          (when cond
            (set-field-value insn (byte 4 28) cond))
          (emit-lap-instruction-element insn seg))))))

;;; A label can only be emitted once.  Once it's been emitted, its pred/succ
;;; slots will be non-nil.

(defun lap-label-emitted-p (lab)
  (not (null (lap-label-pred lab))))

(defun %make-lap-label (name)
  (let* ((lab (ccl::alloc-dll-node *lap-label-freelist*)))
    (if lab
      (progn
        (setf (lap-label-address lab) nil
              (lap-label-refs lab) nil
              (lap-label-name lab) name)
        lab)
      (%%make-lap-label name))))

(defun make-lap-label (name)
  (let* ((lab (%make-lap-label name)))
    (if (typep *lap-labels* 'hash-table)
      (setf (gethash name *lap-labels*) lab)
      (progn
        (push lab *lap-labels*)
        (if (> (length *lap-labels*) 255)
          (let* ((hash (make-hash-table :size 512 :test #'eq)))
            (dolist (l *lap-labels* (setq *lap-labels* hash))
              (setf (gethash (lap-label-name l) hash) l))))))
    lab))

(defun find-lap-label (name)
  (if (typep *lap-labels* 'hash-table)
    (gethash name *lap-labels*)
    (car (member name *lap-labels* :test #'eq :key #'lap-label-name))))

(defun lap-note-label-reference (labx insn type)
  (let* ((lab (or (find-lap-label labx)
                  (make-lap-label labx))))
    (push (cons insn type) (lap-label-refs lab))
    lab))

(defun emit-lap-label (seg name)
  (let* ((lab (find-lap-label name)))
    (if  lab 
      (when (lap-label-emitted-p lab)
        (error "Label ~s: multiply defined." name))
      (setq lab (make-lap-label name)))
    (emit-lap-instruction-element lab seg)))

(defmacro do-lap-labels ((lab &optional result) &body body)
  (let* ((thunk-name (gensym))
         (k (gensym))
         (xlab (gensym)))
    `(flet ((,thunk-name (,lab) ,@body))
      (if (listp *lap-labels*)
        (dolist (,xlab *lap-labels*)
          (,thunk-name ,xlab))
        (maphash #'(lambda (,k ,xlab)
                     (declare (ignore ,k))
                     (,thunk-name ,xlab))
                 *lap-labels*))
      ,result)))

(defun section-size (seg)
  (let* ((last (ccl::dll-node-pred seg)))
    (if (eq last seg)                   ;empty
      0
      (the fixnum
        (+ (the fixnum (instruction-element-address last))
           (the fixnum (instruction-element-size last)))))))
                 
(defun set-element-addresses (start seg)
  (ccl::do-dll-nodes (element seg start)
    (setf (instruction-element-address element) start)
    (incf start (instruction-element-size element))))


            
    
  
(defun arm-finalize (seg)
  (let* ((data-labels ())
         (removed nil))
    (do-lap-labels (lab)
      (loop
        (when (dolist (ref (lap-label-refs lab) t)              
                (when (and (eq :b (cdr ref))
                           (eq lab (lap-instruction-succ (car ref))))
                  (ccl::remove-dll-node (car ref))
                  (setq removed t)
                  (setf (lap-label-refs lab)
                        (delete ref (lap-label-refs lab)))
                  (return)))
          (return))))
    (when removed
      (set-element-addresses 0 seg))
    (dolist (jmp-label *called-subprim-jmp-labels*)
      (let* ((spname (lap-label-name jmp-label))
             (data-label-name (cons spname (arm-subprimitive-address spname)))
             (data-label (make-lap-label data-label-name)))
        (push data-label data-labels)
        (emit-lap-label seg spname)
        (assemble-instruction seg `(ldr pc (:= ,data-label-name)))))
    
    (let* ((marker (make-lap-instruction nil))
           (code-count (make-lap-instruction nil)))
      (emit-lap-instruction-element marker seg)
      (emit-lap-instruction-element code-count seg)
      (set-field-value code-count (byte 32 0) (ash (section-size seg) -2)))
    
    (dolist (data-label (nreverse data-labels))
      (let* ((name (lap-label-name data-label))
             (addr (cdr name)))
        (emit-lap-label seg name)
        (let* ((insn (make-lap-instruction nil)))
          (set-field-value insn (byte 32 0) addr)
          (emit-lap-instruction-element insn seg))))
          
    
    ;; Now fix up label references.  Recall that the PC value at some
    ;; point in program execution is 8 bytes beyond that point.
    (do-lap-labels (lab)
      (if (lap-label-emitted-p lab)
        (let* ((labaddr (lap-label-address lab)))
          (dolist (ref (lap-label-refs lab))
            (destructuring-bind (insn . reftype) ref
              (let* ((diff-in-bytes (- labaddr (+ 8 (lap-instruction-address insn)))))
                (case reftype
                  (:b (set-field-value insn (byte 24 0) (ash diff-in-bytes -2)))
                  (:mem12
                   (if (>= diff-in-bytes 0)
                     (set-field-value insn (byte 1 23) 1)
                     (setq diff-in-bytes (- diff-in-bytes)))
                   (when (> (integer-length diff-in-bytes) 12)
                     (error "PC-relative displacement can't be encoded."))
                   (set-field-value insn (byte 12 0) diff-in-bytes))
                  (:offset
                   (set-field-value insn (byte 32 0)(1+ (ash (lap-instruction-address insn) (- arm::word-shift)))))
                  (t
                   (error "Label type ~s invalid or not yet supported."
                          reftype)))))))
        (if (lap-label-refs lab)
          (error "LAP label ~s was referenced but not defined." (lap-label-name lab)))))
    (ash (section-size seg) -2)))

;;; We want to be able to write vinsn templates using a (mostly) LAP-like
;;; syntax, but ideally don't want to have to repeatedly expand those
;;; vinsn-definition-time-invariant elements of that syntax.
;;;
;;; For example, if DEST is a vinsn parameter and the vinsn body
;;; contains:
;;;
;;;   (ldr DEST (:@ rcontext (:$ arm::tcr.db-link)))
;;;
;;; then we know at definition time:
;;;  1) the opcode of the LDR instruction (obviously)
;;;  2) the fact that the LDR's :mem12 operand uses indexed
;;;     addressing with an immediate operand and no writeback
;;;  3) in this example, we also know the value of the RB field
;;;     and the value of the immediate operand, which happens
;;;     to be positive (setting the U bit).
;;;
;;;  We can apply this knowledge at definition time, and set
;;;  the appropriate bits (U, RN, IMM12) in the opcode.
;;;
;;;  We don't, of course, know the value of DEST at vinsn-definition
;;;  time, but we do know that it's the Nth vinsn parameter, so we
;;;  can turn this example into something like:
;;;
;;;  `(,(augmented-opcode-for-LDR) #(rd-field) #(index-of-DEST)
;;;
;;; This is defined here (rather than in the compiler backend) since
;;; it needs to know a lot about ARM instruction encoding.

(defstruct (arm-vinsn-instruction (:constructor %make-arm-vinsn-instruction)
                                  (:conc-name avi-))
  head
  tail)

(defun make-arm-vinsn-instruction (opcode)
  (let* ((head (list (cons (ldb (byte 16 16) opcode)
                           (ldb (byte 16 0) opcode)))))
    (%make-arm-vinsn-instruction :head head :tail head)))

(defun add-avi-operand (instruction field-type value)
  (let* ((tail (avi-tail instruction)))
    (setf (avi-tail instruction)
          (cdr (rplacd tail (cons (cons field-type value) nil))))))

(defun avi-opcode (avi)
  (car (avi-head avi)))


(defun set-avi-opcode-field (avi bytespec value)
  (let* ((opcode (avi-opcode avi)))
    (multiple-value-bind (high low)
        (set-opcode-values (car opcode) (cdr opcode) bytespec value)
      (declare (type (unsigned-byte 16) high low))
      (setf (car opcode) high
            (cdr opcode) low))
    value))

(defun get-avi-opcode-field (avi bytespec)
  (let* ((opcode (avi-opcode avi)))
    (get-opcode-field (car opcode) (cdr opcode) bytespec)))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *vinsn-field-types*
  #(:cond
    :negated-cond
    :rn
    :rd
    :rm
    :rs
    :alu-constant
    :shift-count                        ;shift type is always explicit
    :mem12-offset
    :mem8-offset
    :reglist-bit
    :uuoA
    :uuo-unary
    :uuoB
    :label
    :subprim
    :data-label
    :dd
    :dm
    :sd
    :sm
    :dn
    :sn
    :fpaddr-offset
    :uuoC
    :imm16
    )))

(defmacro encode-vinsn-field-type (name)
  (or (position name *vinsn-field-types*)
      (error "Unknown vinsn-field-type name ~s." name)))

(defparameter *arm-vinsn-operand-parsers*
    #(vinsn-parse-rd-operand
      vinsn-parse-rn-operand
      vinsn-parse-shifter-operand
      vinsn-parse-m12-operand
      vinsn-parse-reglist-operand
      vinsn-parse-rnw-operand
      vinsn-parse-uuoa-operand
      vinsn-parse-uuo-unary-operand
      vinsn-parse-uuob-operand
      vinsn-parse-rm-operand
      vinsn-parse-b-operand
      vinsn-parse-subprim-operand
      vinsn-parse-m8-operand
      vinsn-parse-dd-operand
      vinsn-parse-dm-operand
      vinsn-parse-sd-operand
      vinsn-parse-sm-operand
      vinsn-parse-dn-operand
      vinsn-parse-sn-operand
      vinsn-parse-rde-operand
      vinsn-parse-rs-operand
      vinsn-parse-fpaddr-operand
      vinsn-parse-@rn-operand
      vinsn-parse-uuoc-operand
      vinsn-parse-fpux-operand
      vinsn-parse-imm16-operand
      ))

(defun vinsn-arg-or-gpr (avi form vinsn-params encoded-type bytespec)
  (let* ((p (position form vinsn-params)))
    (cond (p
           (add-avi-operand avi encoded-type (list p))
           nil)
          (t           
           (set-avi-opcode-field avi bytespec (need-arm-gpr form))))))

(defun vinsn-arg-or-dfpr (avi form vinsn-params encoded-type bytespec)
  (let* ((p (position form vinsn-params)))
    (cond (p
           (add-avi-operand avi encoded-type (list p))
           nil)
          (t           
           (set-avi-opcode-field avi bytespec (need-arm-dfpr form))))))

(defun vinsn-arg-or-sfpr (avi form vinsn-params encoded-type top4 low1)
  (let* ((p (position form vinsn-params)))
    (cond (p
           (add-avi-operand avi encoded-type (list p))
           nil)
          (t
           (let* ((val (need-arm-sfpr form)))
             (set-avi-opcode-field avi top4 (ash val -1))
             (set-avi-opcode-field avi low1 (logand val 1)))))))

(defun simplify-arm-vinsn-application (form params)
  (labels ((simplify-operand (op)
             (if (atom op)
               (if (typep form 'fixnum)
                 op
                 (if (constantp op)
                   (eval op)
                   (let* ((p (position op params)))
                     (if p
                       (list p)
                       (error "Unknown operand: ~s" op)))))
               (if (eq (car op) :apply)
                 `(,(cadr op) ,@(mapcar #'simplify-operand (cddr op)))
                 (eval op)))))
    `(,(cadr form) ,@(mapcar #'simplify-operand (cddr form)))))

(defun vinsn-arg-or-constant (avi form vinsn-params encoded-type bytespec)
  (let* ((p (position form vinsn-params)))
    (cond (p
           (add-avi-operand avi encoded-type (list p))
           nil)
          ((and (typep form 'keyword)
                (eql encoded-type (encode-vinsn-field-type :mem12-offset)))
           (add-avi-operand avi (encode-vinsn-field-type :data-label) form)
           nil)
          ((and (consp form) (eq (car form) :apply))
           (add-avi-operand avi encoded-type (simplify-arm-vinsn-application form vinsn-params))
           nil)
          (t
           (let* ((val (eval form)))
             (when bytespec
               (set-avi-opcode-field avi bytespec val))
             val)))))



(defun vinsn-parse-rd-operand (avi value vinsn-params)
  (vinsn-arg-or-gpr avi value vinsn-params (encode-vinsn-field-type :rd) (byte 4 12)))

(defun vinsn-parse-rn-operand (avi value vinsn-params)
  (vinsn-arg-or-gpr avi value vinsn-params (encode-vinsn-field-type :rn) (byte 4 16)))

(defun vinsn-parse-shifter-operand (avi value vinsn-params)
  (if (atom value)
    (vinsn-arg-or-gpr avi value vinsn-params (encode-vinsn-field-type :rm) (byte 4 0))
    (ecase (car value)
      (:$
       (destructuring-bind (v) (cdr value)
         (let* ((val (vinsn-arg-or-constant avi v vinsn-params (encode-vinsn-field-type :alu-constant) nil)))
           (when val
             (let* ((constant (encode-arm-immediate val)))
               (if constant
                 (progn
                   (set-avi-opcode-field avi (byte 1 25) 1)
                   (set-avi-opcode-field avi (byte 12 0) constant))
                 (let* ((op (get-avi-opcode-field avi (byte 4 21)))
                        (newop nil))
                   (if (or (and (setq constant (encode-arm-immediate (lognot val)))
                                (setq newop (svref *equivalent-complemented-opcodes* op)))
                           (and (setq constant (encode-arm-immediate (- val)))
                                (setq newop (svref *equivalent-negated-opcodes* op))))
                     (progn
                       (set-avi-opcode-field avi (byte 1 25) 1)
                       (set-avi-opcode-field avi (byte 4 21) newop)
                       (set-avi-opcode-field avi (byte 12 0) constant))
                     
                     (error "Can't encode ARM constant ~s." value)))))))))
      (:rrx
       (destructuring-bind (rm) (cdr value)
         (vinsn-arg-or-gpr avi rm vinsn-params (encode-vinsn-field-type :rm) (byte 4 0))
         (set-avi-opcode-field avi (byte 2 5) 3)))
      ((:lsl :lsr :asr :ror)
       (destructuring-bind (rm count) (cdr value)
         (set-avi-opcode-field avi (byte 2 5) (encode-arm-shift-type (car value)))
         (vinsn-arg-or-gpr avi rm vinsn-params (encode-vinsn-field-type :rm) (byte 4 0))
         (cond
           ((atom count)
            (set-avi-opcode-field avi (byte 1 4) 1)
            (vinsn-arg-or-gpr avi count vinsn-params (encode-vinsn-field-type :rs) (byte 4 8)))
           (t
            (unless (eq (car count) :$)
              (error "Invalid shift count: ~s" count))
            (destructuring-bind (countval) (cdr count)
              (vinsn-arg-or-constant avi countval vinsn-params (encode-vinsn-field-type :shift-count) (byte 5 7))))))))))

(defun vinsn-parse-m12-operand (avi value vinsn-params)
  (when (typep value 'keyword)
    (setq value `(:@ arm::pc (:$ ,value))))
  (destructuring-bind (op rn index) value     ; no (:@ reg) sugar
    (vinsn-arg-or-gpr avi rn vinsn-params (encode-vinsn-field-type :rn) (byte 4 16))
    (let* ((constant-index (and (consp index) (eq (car index) :$))))
      (unless constant-index
        (set-avi-opcode-field avi (byte 1 25) 1))
      (cond
        ((atom index)
         (vinsn-arg-or-gpr avi index vinsn-params (encode-vinsn-field-type :rm) (byte 4 0)))
        (constant-index
         (destructuring-bind (constform) (cdr index)
           (let* ((constval
                   (vinsn-arg-or-constant avi constform vinsn-params (encode-vinsn-field-type :mem12-offset) nil)))
             (when constval
               (if (< constval 0)
                 (setq constval (- constval))
                 (set-avi-opcode-field avi (byte 1 23) 1))
               (unless (typep constval '(unsigned-byte 12))
                 (warn "constant offset too large : ~s" constval))
               (set-avi-opcode-field avi (byte 12 0) constval)))))
        ((eq (car index) :rrx)
         (destructuring-bind (rm) (cdr index)
           (vinsn-arg-or-gpr avi rm vinsn-params (encode-vinsn-field-type :rm) (byte 4 0))
           (set-avi-opcode-field avi (byte 2 5) 3)))
        (t
         (destructuring-bind (shift-op rm shift-count) index
           (vinsn-arg-or-gpr avi rm vinsn-params (encode-vinsn-field-type :rm) (byte 4 0))
           (set-avi-opcode-field avi (byte 2 5) (encode-arm-shift-type shift-op))

           (unless (and (consp shift-count)
                        (eq (car shift-count) :$))
             (error "Invalid shift-count: ~s" shift-count))
           (destructuring-bind (shift-count-form) (cdr shift-count)
             (vinsn-arg-or-constant avi shift-count-form vinsn-params (encode-vinsn-field-type :shift-count) (byte 5 7))))))
      (let* ((opcode (avi-opcode avi)))
        (setf (car opcode)
              (the (unsigned-byte 16)
                (set-opcode-value-from-addressing-mode
                 (car opcode)
                 op
                 constant-index)))))))

(defun vinsn-parse-reglist-operand (avi value vinsn-params)
  (dolist (r value)
    (let* ((p (position r vinsn-params)))
      (if p
        (add-avi-operand avi (encode-vinsn-field-type :reglist-bit) (list p))
        (let* ((bit (need-arm-gpr r)))
          (set-avi-opcode-field avi (byte 1 bit) 1))))))

(defun vinsn-parse-rnw-operand (avi value vinsn-params)
  (let* ((rn (if (atom value)
               value
               (destructuring-bind (marker reg) value
                 (if (eq marker :!)
                   (set-avi-opcode-field avi (byte 1 21) 1)
                   (error "Unrecognized writeback indicator in ~s." value))
                 reg))))
    (vinsn-arg-or-gpr avi rn vinsn-params  (encode-vinsn-field-type :rn) (byte 4 16))))

(defun vinsn-parse-uuoA-operand (avi value vinsn-params)
  (vinsn-arg-or-gpr avi value vinsn-params (encode-vinsn-field-type :uuoA) (byte 4 8)))

(defun vinsn-parse-uuo-unary-operand (avi value vinsn-params)
  (when (or (atom value)
          (not (eq (car value) :$)))
    (error "Invalid constant syntax in ~s." value))
  (destructuring-bind (valform) (cdr value)
    (vinsn-arg-or-constant avi valform vinsn-params (encode-vinsn-field-type :uuo-unary) (byte 8 12))))

(defun vinsn-parse-uuoB-operand (avi value vinsn-params)
  (vinsn-arg-or-gpr avi value vinsn-params (encode-vinsn-field-type :uuoB) (byte 4 12)))

(defun vinsn-parse-uuoC-operand (avi value vinsn-params)
  (vinsn-arg-or-gpr avi value vinsn-params (encode-vinsn-field-type :uuoC) (byte 4 16)))

(defun vinsn-parse-fpux-operand (avi value vinsn-params)
  (declare (ignore vinsn-params))
  (let* ((regno (if (typep value '(unsigned-byte 4))
                  value
                  (ecase (keywordize value)
                    (:fpsid 0)
                    (:fpscr 1)
                    (:fpexc 8)))))
    (set-avi-opcode-field avi (byte 4 16) regno)))

(defun vinsn-parse-rm-operand (avi value vinsn-params)
  (vinsn-arg-or-gpr avi value vinsn-params (encode-vinsn-field-type :rm) (byte 4 0)))

(defun vinsn-parse-b-operand (avi value vinsn-params)
  ;; Pretty much has to be a param or a local label what else would we b to ?
  (let* ((p (position value vinsn-params)))
    (cond (p
           (add-avi-operand avi (encode-vinsn-field-type :label) (list p)))
          ((typep value 'keyword)
           (add-avi-operand avi (encode-vinsn-field-type :label) value))
          (t
           (error "Unknown branch target: ~s." value)))))

(defun vinsn-parse-subprim-operand (avi value vinsn-params)
  (let* ((p (position value vinsn-params))
         (addr nil))
    (cond (p
           (add-avi-operand avi (encode-vinsn-field-type :subprim) (list p)))
          ((setq addr (arm-subprimitive-address value))
           (add-avi-operand avi (encode-vinsn-field-type :subprim) addr))
          ((arm-subprimitive-name value)
           (add-avi-operand avi (encode-vinsn-field-type :subprim) value))  
          (t
           (error "Unknown subprimitive name or address: ~s." value)))))

(defun vinsn-parse-m8-operand (avi value vinsn-params)
  (if (atom value)
    (error "Invalid memory operand ~s." value)
    (destructuring-bind (mode rn index) value
      (vinsn-arg-or-gpr avi rn vinsn-params (encode-vinsn-field-type :rn) (byte 4 16))
      (let* ((constant-index (and (consp index) (eq (car index) :$))))
        (when constant-index
          (set-avi-opcode-field avi (byte 1 22) 1))
        (cond ((atom index)
               (vinsn-arg-or-gpr avi index vinsn-params (encode-vinsn-field-type :rm) (byte 4 0)))
              (constant-index
               (destructuring-bind (constform) (cdr index)
                 (let* ((constval
                         (vinsn-arg-or-constant avi constform vinsn-params (encode-vinsn-field-type :mem8-offset) nil)))
                   (when constval
                     (if (< constval 0)
                       (setq constval (- constval))
                       (set-avi-opcode-field avi (byte 1 23) 1))
                     (unless (typep constval '(unsigned-byte 8))
                       (warn "constant offset too large : ~s" constval))
                     (set-avi-opcode-field avi (byte 4 0) (ldb (byte 4 0) constval))
                     (set-avi-opcode-field avi (byte 4 8) (ldb (byte 4 4) constval))))))
              ((eq (car index) :rrx)
               (destructuring-bind (rm) (cdr index)
                 (vinsn-arg-or-gpr avi rm vinsn-params (encode-vinsn-field-type :rm) (byte 4 0))
                 (set-avi-opcode-field avi (byte 2 5) 3)))
              (t
               (destructuring-bind (shift-op rm shift-count) index
                 (vinsn-arg-or-gpr avi rm vinsn-params (encode-vinsn-field-type :rm) (byte 4 0))
                 (set-avi-opcode-field avi (byte 2 5) (encode-arm-shift-type shift-op))
                 (unless (and (consp shift-count)
                              (eq (car shift-count) :$))
                   (error "Invalid shift-count: ~s" shift-count))
                 (destructuring-bind (shift-count-form) (cdr shift-count)
                   (vinsn-arg-or-constant avi shift-count-form vinsn-params (encode-vinsn-field-type :shift-count) (byte 5 7))))))
        (setf (car (avi-opcode avi))
              (the (unsigned-byte 16)
              (set-opcode-value-from-addressing-mode (car (avi-opcode avi)) mode constant-index)))))))



(defun vinsn-parse-dd-operand (avi value vinsn-params)
  (vinsn-arg-or-dfpr avi value vinsn-params (encode-vinsn-field-type :dd) (byte 4 12)))

(defun vinsn-parse-dm-operand (avi value vinsn-params)
  (vinsn-arg-or-dfpr avi value vinsn-params (encode-vinsn-field-type :dm) (byte 4 0)))

(defun vinsn-parse-sd-operand (avi value vinsn-params)
  (vinsn-arg-or-sfpr avi value vinsn-params (encode-vinsn-field-type :sd) (byte 4 12) (byte 1 22)))

(defun vinsn-parse-sm-operand (avi value vinsn-params)
  (vinsn-arg-or-sfpr avi value vinsn-params (encode-vinsn-field-type :sm) (byte 4 0) (byte 1 5)))

(defun vinsn-parse-dn-operand (avi value vinsn-params)
  (vinsn-arg-or-dfpr avi value vinsn-params (encode-vinsn-field-type :dn) (byte 4 16)))

(defun vinsn-parse-sn-operand (avi value vinsn-params)
  (vinsn-arg-or-sfpr avi value vinsn-params (encode-vinsn-field-type :sn) (byte 4 16) (byte 1 7)))

(defun vinsn-parse-rde-operand (avi value vinsn-params)
  (let* ((val (get-arm-gpr value)))
    (when (and val (oddp val))
      (error "Register ~s must be even-numbered." value)))
  (vinsn-arg-or-gpr avi value vinsn-params (encode-vinsn-field-type :rd) (byte 4 12)))

(defun vinsn-parse-rs-operand (avi value vinsn-params)
  (vinsn-arg-or-gpr avi value vinsn-params (encode-vinsn-field-type :rs) (byte 4 8)))

(defun vinsn-parse-fpaddr-operand (avi value vinsn-params)
  (destructuring-bind (op rn offset) value
    (unless (eq op :@) (error "Bad FP address operand: ~s." value))
    (vinsn-arg-or-gpr avi rn vinsn-params (encode-vinsn-field-type :rn) (byte 4 16))
    (destructuring-bind (marker offform) offset
      (unless (eq marker :$) (error "Bad FP offset: ~s" offset))
      (let* ((offval (vinsn-arg-or-constant avi offform vinsn-params (encode-vinsn-field-type :fpaddr-offset) nil)))
        (when offval
          (if (< offval 0)
            (setq offval (- offval))
            (set-avi-opcode-field avi (byte 1 23) 1))
          (when (logtest 3 offval)
            (error "Memory offset ~s must be a multiple of 4." offval))
          (set-avi-opcode-field avi (byte 8 0) (ash offval -2)))))))

(defun vinsn-parse-imm16-operand (avi value vinsn-params)
  (unless (and (consp value)
               (eq (car value) :$)
               (consp (cdr value))
               (null (cddr value)))
    (error "Bad imm16 constant operand syntax: ~s." value))
  (let* ((val (vinsn-arg-or-constant avi (cadr value) vinsn-params (encode-vinsn-field-type :imm16) nil)))
    (when val
      (set-avi-opcode-field avi (byte 12 0) (ldb (byte 12 0) val))
      (set-avi-opcode-field avi (byte 4 16) (ldb (byte 4 12) val)))))


(defun vinsn-simplify-instruction (form vinsn-params)
  (destructuring-bind (name . opvals) form
    (case name
      ((:code :data) form)
      (:word (destructuring-bind (val) opvals
               (let* ((p (position val vinsn-params)))
                 (list name (if p (list p) (eval val))))))
      (t
       (multiple-value-bind (template cond explicit-cond) (lookup-arm-instruction name)
         (unless template
           (error "Unknown ARM instruction - ~s" form))
         (let* ((cond-indicator (and (consp (car opvals))
                                     (keywordize (caar opvals))))
                (avi (make-arm-vinsn-instruction (arm-instruction-template-val template))))
           (when (or (eq cond-indicator :?)
                     (eq cond-indicator :~))
             (let* ((condform (pop opvals)))
               (destructuring-bind (cond-name) (cdr condform)
                 (let* ((p (position cond-name vinsn-params)))
                   (if p
                     (if explicit-cond
                       (error "Can't use ~s with explicit condition name." condform)
                       (progn
                         (add-avi-operand avi (if (eq cond-indicator :?)
                                                (encode-vinsn-field-type :cond)
                                                (encode-vinsn-field-type :negated-cond))
                                          (list p))
                         (setq cond nil)))
                     (let* ((c (need-arm-condition-name cond-name)))
                       (when (eq cond-indicator :~)
                         (if (< c 14)
                           (setq c (logxor c 1))
                           (error "Invalid explicit condition ~s." condform)))
                       (if (and explicit-cond (not (eql c cond)))
                         (error "Can't use explicit condition and :? : ~s" condform)
                         (setq cond c))))))))
           (let* ((optypes (arm-instruction-template-operand-types template))
                  (n (length optypes)))
             (unless (= n (length opvals))
               (error "ARM ~a instructions require ~d operands, but ~d were provided in ~s." (arm-instruction-template-name template) n (length opvals) form))
             (dotimes (i n)
               (let* ((optype (pop optypes))
                      (opval (pop opvals)))
                 (funcall (svref *arm-vinsn-operand-parsers* optype)
                          avi opval vinsn-params)))
             (when cond
               (set-avi-opcode-field avi (byte 4 28) cond))
             (avi-head avi))))))))
          

(defparameter *arm-vinsn-insert-functions*
  #(vinsn-insert-cond-operand
    vinsn-insert-negated-cond-operand
    vinsn-insert-rn-operand
    vinsn-insert-rd-operand
    vinsn-insert-rm-operand
    vinsn-insert-rs-operand
    vinsn-insert-alu-constant-operand
    vinsn-insert-shift-count-operand                        ;shift type is always explicit
    vinsn-insert-mem12-offset-operand
    vinsn-insert-mem8-offset-operand
    vinsn-insert-reglist-bit-operand
    vinsn-insert-uuoA-operand
    vinsn-insert-uuo-unary-operand
    vinsn-insert-uuoB-operand
    vinsn-insert-label-operand
    vinsn-insert-subprim-operand
    vinsn-insert-data-label-operand
    vinsn-insert-dd-operand
    vinsn-insert-dm-operand
    vinsn-insert-sd-operand
    vinsn-insert-sm-operand
    vinsn-insert-dn-operand
    vinsn-insert-sn-operand
    vinsn-insert-fpaddr-offset-operand
    vinsn-insert-uuoc-operand
    vinsn-insert-imm16-operand
    ))

(defun vinsn-insert-cond-operand (instruction value)
  (set-field-value instruction (byte 4 28) value))

(defun vinsn-insert-negated-cond-operand (instruction value)
  (set-field-value instruction (byte 4 28) (logxor value 1)))

(defun vinsn-insert-rn-operand (instruction value)
  (set-field-value instruction (byte 4 16) value))

(defun vinsn-insert-rd-operand (instruction value)
  (set-field-value instruction (byte 4 12) value))

(defun vinsn-insert-rm-operand (instruction value)
  (set-field-value instruction (byte 4 0) value))

(defun vinsn-insert-rs-operand (instruction value)
  (set-field-value instruction (byte 4 8) value))

(defun vinsn-insert-alu-constant-operand (instruction value)
  (insert-shifter-constant value instruction))

(defun vinsn-insert-shift-count-operand (instruction value)
  (set-field-value instruction (byte 5 7) value))

(defun vinsn-insert-mem12-offset-operand (instruction value)
  (if (typep value 'lap-label)
    (lap-note-label-reference value instruction :mem12)
    (progn
      (if (< value 0)
        (setq value (- value))
        (set-field-value instruction (byte 1 23) 1))
      (set-field-value instruction (byte 12 0) value))))

(defun vinsn-insert-mem8-offset-operand (instruction value) 
  (if (< value 0)
    (setq value (- value))
    (set-field-value instruction (byte 1 23) 1))
  (set-field-value instruction (byte 4 8) (ldb (byte 4 4) value))
  (set-field-value instruction (byte 4 0) (ldb (byte 4 0) value)))

(defun vinsn-insert-reglist-bit-operand (instruction value)
  (set-field-value instruction (byte 1 value) 1))

(defun vinsn-insert-uuoA-operand (instruction value)
  (set-field-value instruction (byte 4 8) value))

(defun vinsn-insert-uuo-unary-operand (instruction value)
  (set-field-value instruction (byte 8 12) value))

(defun vinsn-insert-uuoB-operand (instruction value)
  (set-field-value instruction (byte 4 12) value))

(defun vinsn-insert-uuoC-operand (instruction value)
  (set-field-value instruction (byte 4 16) value))

(defun vinsn-insert-label-operand (instruction value)
  (let* ((label (etypecase value
                  (cons (or (find-lap-label value)
                            (error "No LAP label for ~s." (car value))))
                  (lap-label value)
                  (ccl::vinsn-label
                   (or (find-lap-label value)
                       (make-lap-label value))))))
    (push (cons instruction :b) (lap-label-refs label))))

(defun vinsn-insert-subprim-operand (instruction value)
  (let* ((name (arm-subprimitive-name value))
         (label (or (find-lap-label name)
                    (make-lap-label name))))
    (pushnew label *called-subprim-jmp-labels*)
    (push (cons instruction :b) (lap-label-refs label))))



(defun vinsn-insert-data-label-operand (instruction value)
  (let* ((label (if (typep value 'lap-label) value (find-lap-label value))))
    (unless label
      (error "Mystery data label: ~s" value))
    (push (cons instruction :mem12) (lap-label-refs label))))

(defun vinsn-insert-dd-operand (instruction value)
  (set-field-value instruction (byte 4 12) value) )

(defun vinsn-insert-dm-operand (instruction value)
  (set-field-value instruction (byte 4 0) value))

(defun vinsn-insert-sd-operand (instruction value)
  (set-field-value instruction (byte 4 12) (ash value -1))
  (set-field-value instruction (byte 1 22) (logand value 1)))

(defun vinsn-insert-sm-operand (instruction value)
  (set-field-value instruction (byte 4 0) (ash value -1))
  (set-field-value instruction (byte 1 5) (logand value 1)))

(defun vinsn-insert-dn-operand (instruction value)
  (set-field-value instruction (byte 4 16) value))

(defun vinsn-insert-sn-operand (instruction value)
  (set-field-value instruction (byte 4 16) (ash value -1))
  (set-field-value instruction (byte 1 7) (logand value 1)))

(defun vinsn-insert-fpaddr-offset-operand (instruction value)
  (if (< value 0)
    (setq value (- value))
    (set-field-value instruction (byte 1 23) 1))
  (set-field-value instruction (byte 8 0) (ash value -2)))

(defun vinsn-insert-imm16-operand (instruction value)
  (set-field-value instruction (byte 12 0) (ldb (byte 12 0) value))
  (set-field-value instruction (byte 4 16) (ldb (byte 4 12) value)))



(provide "ARM-ASM")
