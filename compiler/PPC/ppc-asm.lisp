;;;-*- Mode: Lisp; Package: (PPC :use CL) -*-
;;;
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


(cl:eval-when (:compile-toplevel :execute)
  (require "PPC-ARCH"))

(in-package "PPC")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "RISC-LAP")
(ccl::defenum ()
  $ppc-operand-signed                    ; This operand takes signed values.
  $ppc-operand-signopt                   ; This operand takes signed or positive values.
  $ppc-operand-cr                        ; This operand uses symbolic names for CR fields
  $ppc-operand-gpr                       ; This operand is a GPR.
  $ppc-operand-fpr                       ; This operand is an FPR.
  $ppc-operand-relative                  ; This operand is a relative branch offset.
  $ppc-operand-absolute                  ; This operand is an absolute address.
  $ppc-operand-optional                  ; This operand is optional, defaulting to 0.
  $ppc-operand-next                      ; A brutal hack to make some rotate instructions work.
  $ppc-operand-negative                  ; This operand should be treated as negative wrt overflow checking.
  $ppc-operand-fake                      ; Used to signify operands whose value is that of another operand.
  $ppc-operand-parens                    ; Operand should be enclosed in parens in traditional as syntax.
  $ppc-operand-source                    ; Operand value is read by the instruction
  $ppc-operand-dest                      ; Operand value is written by the instruction
  $ppc-operand-vr			; Operand is an Altivec vector register
  )

(ccl::defenum ()
  $ppc                                  ; Opcode is defined for the PowerPC architecture.
  $b32                                  ; Opcode is only defined on 32 bit architectures.
  $b64                                  ; Opcode is only defined on 64 bit architectures.
)

                                        ;;; A macro to extract the major opcode from an instruction.
(defmacro major-opcode (i) `(ldb (byte 6 26) ,i))

;; Operand class indices.
(ccl::defenum ()
  $unused
  $ba                                   ; the BA field in an XL form instruction.
  $bat                                  ; The BA field in an XL form instruction when it 
                                        ;  must be the same as the BT field in the same instruction.
  $bb                                   ; The BB field in an XL form instruction.
  $bba                                  ; The BB field in an XL form instruction when it must be 
                                        ;  the same as the BA field in the same instruction. 
  $bd                                   ; The BD field in a B form instruction.  The lower two
                                        ; bits are forced to zero.
  $bda                                  ; The BD field in a B form instruction when absolute 
                                        ;  addressing is used.
  $bdm                                  ; The BD field in a B form instruction when the - modifier 
                                        ;  is used. This sets the y bit of the BO field appropriately.
  $bdma                                 ; The BD field in a B form instruction when the - modifier is used       
                                        ;  and absolute addressing is used.   
  $bdp                                  ; The BD field in a B form instruction when the + modifier 
                                        ;  is used. This sets the y bit of the BO field appropriately.
  $bdpa                                 ; The BD field in a B form instruction when the + modifier is used       
                                        ;  and absolute addressing is used.   
  $bf                                   ; The BF field in an X or XL form instruction.
  $obf                                  ; An optional BF field.  This is used for comparison instructions,
                                        ;  in which an omitted BF field is taken as zero.
  $bfa                                  ; The BFA field in an X or XL form instruction.
  $bi                                   ; The BI field in a B form or XL form instruction.
  $bo                                   ; The BO field in a B form instruction.  Certain values are illegal.
  $boe                                  ; The BO field in a B form instruction when the + or - modifier is         
                                        ; used.  This is like the BO field, but it must be even.
  $bt                                   ; The BT field in an X or XL form instruction.  
  $cr                                   ; The condition register number portion of the BI field in a B form
                                        ;  or XL form instruction.  This is used for the extended
                                        ;  conditional branch mnemonics, which set the lower two bits of the
                                        ;  BI field.  This field is optional.         
  $d                                    ; The D field in a D form instruction.  This is a displacement off
                                        ;  a register, and implies that the next operand is a register in     
                                        ;  parentheses. 
  $ds                                   ; The DS field in a DS form instruction.  This is like D, but the
                                        ;  lower two bits are forced to zero.  
  $flm                                  ; The FLM field in an XFL form instruction.  
  $fra                                  ; The FRA field in an X or A form instruction.  
  $frb                                  ; The FRB field in an X or A form instruction.  
  $frc                                  ; The FRC field in an A form instruction.  
  $frs                                  ; The FRS field in an X form instruction
  $frt                                  ; The FRT field in a D, X or A form instruction.  
  $fxm                                  ; The FXM field in an XFX instruction.  
  $l                                    ; The L field in a D or X form instruction.  
  $li                                   ; The LI field in an I form instruction.  The lower two bits are
                                        ;  forced to zero.  
  $lia                                  ; The LI field in an I form instruction when used as an absolute
                                        ;  address.
  $mb                                   ; The MB field in an M form instruction.  
  $me                                   ; The ME field in an M form instruction.  
  $mbe                                  ; The MB and ME fields in an M form instruction expressed a single
                                        ;  operand which is a bitmask indicating which bits to select.  This
                                        ;  is a two operand form using $PPC-OPERAND-NEXT.  See the
                                        ;  description of $PPC-OPERAND-NEXT. for what this means.
  $mbe-aux                              ; A placeholder for the second MBE operand.
  $mb6                                  ; The MB or ME field in an MD or MDS form instruction.  The high
                                        ;  bit is wrapped to the low end.  
  $nb                                   ; The NB field in an X form instruction.  The value 32 is stored as 0.  
  $nsi                                  ; The NSI field in a D form instruction.  This is the same as the
                                        ;  SI field, only negated.  
  $ra                                   ; The RA field in an D, DS, X, XO, M, or MDS form instruction.    
  $ral                                  ; The RA field in a D or X form instruction which is an updating
                                        ;  load, which means that the RA field may not be zero and may not
                                        ;  equal the RT field.  
  $ram                                  ; The RA field in an lmw instruction, which has special value
                                        ;  restrictions.  
  $ras                                  ; The RA field in a D or X form instruction which is an updating
                                        ;  store or an updating floating point load, which means that the RA
                                        ;  field may not be zero.  
  $rTa                                  ; The RA field in an D, DS, X, XO, M, or MDS form instruction, when
                                        ;  used as a destination.
  $rb                                   ; The RB field in an X, XO, M, or MDS form instruction.    
  $rbs                                  ; The RB field in an X form instruction when it must be the same as
                                        ;  the RS field in the instruction.  This is used for extended
                                        ;  mnemonics like mr.  
  $rs                                   ; The RS field in a D, DS, X, XFX, XS, M, MD or MDS form   
                                        ;  instruction. 
  $rt                                   ; The RT field in a D, DS, X, XFX or XO form instruction.  
  $sh                                   ; The SH field in an X or M form instruction.  
  $sh6                                  ; The SH field in an MD form instruction.  This is split.  
  $si                                   ; The SI field in a D form instruction.  
  $sisignopt                            ; The SI field in a D form instruction when we accept a wide range
                                        ;  of positive values.  
  $spr                                  ; The SPR or TBR field in an XFX form instruction.  This is
                                        ;  flipped--the lower 5 bits are stored in the upper 5 and vice-
                                        ;  versa.  
  $sr                                   ; The SR field in an X form instruction.  
  $to                                   ; The TO field in a D or X form instruction.  
  $u                                    ; The U field in an X form instruction.  
  $ui                                   ; The UI field in a D form instruction.
  $uuo-code                             ; UUO extended-operation code.
  $uuo-errnum
  $uuo-small-errnum
  $va               ; The vA field in a vector instruction
  $vb               ; The vB field in a vector instruction
  $vc               ; the vC field in a vector VA form instruction
  $vd               ; the vD field in a vector instruction
  $vs               ; the vS field in a vector instruction
  $vsh              ; the SH field in a vector instruction
  $all/transient    ; the all/transient bit in a vector data stream instruction
  $strm             ; the strm field in a vector data stream instruction
  $vsimm            ; a 5-bit signed immediate that goes in the vA field
  $vuimm            ; a 5-bit unsigned immediate that goes in the vA field
  $ls               ; The LS field in an X (sync) form instruction

  )

(defconstant $me6 $mb6)
(defconstant $tbr $spr)

(defmacro defopmask (name width offset)
  `(defconstant ,name (mask-field (byte ,width ,offset) -1)))

(defopmask $ba-mask 5 16)
(defopmask $bb-mask 5 11)
(defopmask $bi-mask 5 16)
(defopmask $bo-mask 5 21)
(defopmask $fra-mask 5 16)
(defopmask $frb-mask 5 11)
(defopmask $frc-mask 5 6)
(defopmask $mb-mask 5 6)
(defopmask $me-mask 5 1)
(defopmask $mb6-mask 6 5)
(defopmask $ra-mask 5 16)
(defopmask $rb-mask 5 11)
(defopmask $rt-mask 5 21)
(defopmask $sh-mask 5 11)
(defconstant $sh6-mask (logior (mask-field (byte 1 1) -1) (mask-field (byte 5 11) -1)))
(defopmask $spr-mask 10 11)
(defopmask $to-mask 5 21)
(defopmask $uuo-code-mask 7 4)
(defopmask $uuo-interr-mask 10 16)
(defopmask $uuo-small-interr-mask 5 21)
(defopmask $vsimm-mask 5 16)
(defopmask $vuimm-mask 5 16)

)




(eval-when (:compile-toplevel :execute)
  (defmacro ppc-op (index width offset &optional insert-function extract-function &rest flags)
    `(ccl::make-operand :index ,index
      :width ,width 
      :offset ,offset 
      :insert-function ',insert-function
      :extract-function ',extract-function
      :flags (logior ,@(mapcar #'(lambda (f) `(ash 1 ,f)) flags)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *ppc-operands*
  (vector
   (ppc-op $unused 0 0)
   (ppc-op $ba 5 16 nil nil $ppc-operand-cr)
   (ppc-op $bat 5 16 insert-bat extract-bat ccl::operand-fake)
   (ppc-op $bb 5 11 nil nil $ppc-operand-cr)
   (ppc-op $bba 5 11 insert-bba extract-bba ccl::operand-fake)
   (ppc-op $bd 16 0 insert-bd extract-bd $ppc-operand-relative $ppc-operand-signed)
   (ppc-op $bda 16 0 insert-bd extract-bd $ppc-operand-absolute $ppc-operand-signed)
   (ppc-op $bdm 16 0 insert-bdm extract-bdm $ppc-operand-relative $ppc-operand-signed)
   (ppc-op $bdma 16 0 insert-bdm extract-bdm $ppc-operand-absolute $ppc-operand-signed)
   (ppc-op $bdp 16 0 insert-bdp extract-bdp $ppc-operand-relative $ppc-operand-signed)
   (ppc-op $bdpa 16 0 insert-bdp extract-bdp $ppc-operand-absolute $ppc-operand-signed)
   (ppc-op $bf 3 23 insert-bf extract-bf $ppc-operand-cr)
   (ppc-op $obf 3 23 insert-bf extract-bf $ppc-operand-cr ccl::operand-optional)
   (ppc-op $bfa 3 18 insert-cr extract-cr $ppc-operand-cr)
   (ppc-op $bi 5 16 nil nil $ppc-operand-cr)
   (ppc-op $bo 5 21 insert-bo extract-bo)
   (ppc-op $boe 5 21 insert-boe extract-boe)
   (ppc-op $bt 5 21 nil nil $ppc-operand-cr)
   (ppc-op $cr 5 16 insert-cr extract-cr $ppc-operand-cr ccl::operand-optional)
   (ppc-op $d 16 0 nil nil $ppc-operand-parens $ppc-operand-signed)
   (ppc-op $ds 16 0 insert-ds extract-ds $ppc-operand-parens $ppc-operand-signed)
   (ppc-op $flm 8 17)
   (ppc-op $fra 5 16 nil nil $ppc-operand-fpr $ppc-operand-source)
   (ppc-op $frb 5 11 nil nil $ppc-operand-fpr $ppc-operand-source)
   (ppc-op $frc 5 6 nil nil $ppc-operand-fpr $ppc-operand-source)
   (ppc-op $frs 5 21 nil nil $ppc-operand-fpr $ppc-operand-source)
   (ppc-op $frt 5 21 nil nil $ppc-operand-fpr $ppc-operand-dest)
   (ppc-op $fxm 8 12)
   (ppc-op $l 1 21 nil nil ccl::operand-optional)
   (ppc-op $li 26 0 insert-li extract-li $ppc-operand-relative $ppc-operand-signed)
   (ppc-op $lia 26 0 insert-li extract-li $ppc-operand-absolute $ppc-operand-signed)
   (ppc-op $mb 5 6)
   (ppc-op $me 5 1 )
   (ppc-op $mbe 5 6 nil nil ccl::operand-optional $ppc-operand-next)
   (ppc-op $mbe-aux 10 1 insert-mbe extract-mbe)
   (ppc-op $mb6 6 5 insert-mb6 extract-mb6)
   (ppc-op $nb 6 11 insert-nb extract-nb)
   (ppc-op $nsi 16 0 insert-nsi extract-nsi $ppc-operand-negative $ppc-operand-signed)
   (ppc-op $ra 5 16 nil nil $ppc-operand-gpr $ppc-operand-source)
   (ppc-op $ral 5 16 insert-ral nil $ppc-operand-gpr $ppc-operand-source)
   (ppc-op $ram 5 16 insert-ram nil $ppc-operand-gpr $ppc-operand-source)
   (ppc-op $ras 5 16 insert-ras nil $ppc-operand-gpr $ppc-operand-source)
   (ppc-op $rTa 5 16 nil nil $ppc-operand-gpr $ppc-operand-dest)
   (ppc-op $rb 5 11 nil nil $ppc-operand-gpr $ppc-operand-source)
   (ppc-op $rbs 5 11 insert-rbs extract-rbs ccl::operand-fake)
   (ppc-op $rs 5 21 nil nil $ppc-operand-gpr $ppc-operand-source)
   (ppc-op $rt 5 21 nil nil $ppc-operand-gpr $ppc-operand-dest)
   (ppc-op $sh 5 11)
   (ppc-op $sh6 6 1 insert-sh6 extract-sh6)
   (ppc-op $si 16 0 nil nil $ppc-operand-signed)
   (ppc-op $sisignopt 16 0 nil nil $ppc-operand-signed $ppc-operand-signopt)
   (ppc-op $spr 10 11 insert-spr extract-spr)
   (ppc-op $sr 4 16)
   (ppc-op $to 5 21)
   (ppc-op $u 4 12)
   (ppc-op $ui 16 0)
   (ppc-op $uuo-code 7 4)
   (ppc-op $uuo-errnum 10 16)
   (ppc-op $uuo-small-errnum 5 21)
   (ppc-op $va 5 16 nil nil $ppc-operand-vr $ppc-operand-source)
   (ppc-op $vb 5 11 nil nil $ppc-operand-vr $ppc-operand-source)
   (ppc-op $vc 5 6  nil nil $ppc-operand-vr $ppc-operand-source)
   (ppc-op $vd 5 21 nil nil $ppc-operand-vr $ppc-operand-dest)
   (ppc-op $vs 5 21 nil nil $ppc-operand-vr $ppc-operand-source)
   (ppc-op $vsh 4 6 nil nil)
   (ppc-op $all/transient 1 25 nil nil)
   (ppc-op $strm 2 21 nil nil)
   (ppc-op $vsimm 5 16 nil nil $ppc-operand-signed)
   (ppc-op $vuimm 5 16 nil nil)
   (ppc-op $ls 21 2 nil nil ccl::operand-optional)

   ))


(eval-when (:load-toplevel :execute)
  (dotimes (i (length *ppc-operands*))
    (unless (= i (ccl::operand-index (svref *ppc-operands* i)))
      (break "Operand table out-of-synch at ~d : ~s. " i (svref *ppc-operands* i)))))

)

(eval-when (:compile-toplevel :execute)
;; The main opcode of an instruction.
(defmacro op (x &optional (base 0)) `(dpb ,x (byte 6 26) ,base))
(defconstant $op-mask (mask-field (byte 6 26) -1))

;; The main opcode combined with a trap code in the TO field
;; of a D form instruction.  Used for extended mnemonics for
;; the trap instructions.
(defmacro opto (x to) `(op ,x (dpb ,to (byte 5 21) 0)))
(defconstant $opto-mask (opto -1 -1))

;; The main opcode combined with a comparison size bit in the L field
;; of a D form or X form instruction.  Used for extended mnemonics for
;; the comparison instructions.
(defmacro opl (x l) `(op ,x (dpb ,l (byte 1 21) 0)))
(defconstant $opl-mask (opl -1 -1))

;; An A form instruction.
(defmacro a (op xop rc) `(op ,op (dpb ,xop (byte 5 1) (logand ,rc 1))))
(defconstant $a-mask (a -1 -1 -1))

;; An A-MASK with the FRB field fixed.  
(defconstant $afrb-mask (logior $a-mask $frb-mask))

;; An A-MASK with the FRC field fixed.  
(defconstant $afrc-mask (logior $a-mask $frc-mask))

;; An A-MASK with the FRA and FRC fields fixed.  
(defconstant $afrafrc-mask (logior $a-mask $fra-mask $frc-mask))

;; A B form instruction.  
(defmacro b (op aa lk) `(op ,op (dpb ,aa (byte 1 1) (logand ,lk 1))))
(defconstant $b-mask (b -1 -1 -1))

;; A B form instruction setting the BO field.  
(defmacro bbo (op bo aa lk) 
  `(op ,op (dpb ,bo (byte 5 21) (dpb ,aa (byte 1 1) (logand ,lk 1)))))
(defconstant $bbo-mask (bbo -1 -1 -1 -1))

;; A BBO-MASK with the y bit of the BO field removed.  This permits
;; matching a conditional branch regardless of the setting of the y
;; bit.  
(defconstant $y-mask (dpb 1 (byte 1 21) 0))
(defconstant $bboy-mask (logandc2 $bbo-mask $y-mask))

;; A B form instruction setting the BO field and the condition bits of
;; the BI field.  
(defmacro bbocb (op bo cb  aa lk)
  `(op ,op (dpb ,bo (byte 5 21) (dpb ,cb (byte 2 16) (dpb ,aa (byte 1 1) (logand ,lk 1))))))
(defconstant $bbocb-mask (bbocb -1 -1 -1 -1 -1))

;; A BBOCB-MASK with the y bit of the BO field removed.  
(defconstant $bboycb-mask (logandc2 $bbocb-mask $y-mask))

;; A BBOYCB-MASK in which the BI field is fixed.  
(defconstant $bboybi-mask (logior $bboycb-mask $bi-mask))

;; The main opcode mask with the RA field clear.  
(defconstant $DRA-MASK (logior $op-mask $ra-mask))

;; A DS form instruction.  
(defmacro dso (op xop) `(op ,op  (logand ,xop #x3)))
(defconstant $ds-mask (dso -1 -1))

;; An M form instruction.  
(defmacro m (op &optional (rc 0)) `(op ,op (logand ,rc 1)))
(defconstant $m-mask (m -1 -1))

;; An M form instruction with the ME field specified.  
(defmacro mme (op me &optional (rc 0)) `(op ,op (dpb ,me (byte 5 1) (logand ,rc 1))))

;; An M-MASK with the MB and ME fields fixed.  
(defconstant $mmbme-mask (logior $m-mask $mb-mask $me-mask))

;; An M-MASK with the SH and ME fields fixed.  
(defconstant $mshme-mask (logior $m-mask $sh-mask $me-mask))

;; An MD form instruction.  
(defmacro md (op xop &optional (rc 0)) `(op ,op (dpb ,xop (byte 3 2) (logand ,rc 1))))
(defconstant $md-mask (md -1 -1 -1))

;; An MD-MASK with the MB field fixed.  
(defconstant $mdmb-mask (logior $md-mask $mb6-mask))

;; An MD-MASK with the SH field fixed.  
(defconstant $mdsh-mask (logior $md-mask $sh6-mask))

;; An MDS form instruction. 
(defmacro mds (op xop &optional (rc 0)) `(op ,op (dpb ,xop (byte 4 1) (logand ,rc 1))))
(defconstant $mds-mask (mds -1 -1 -1))

;; An MDS-MASK with the MB field fixed.  
(defconstant $mdsmb-mask (logior $mds-mask $mb6-mask))

;; An SC form instruction. 
(defmacro sc (op sa lk) `(op ,op (dpb ,sa (byte 1 1) (logand ,lk 1))))
(defconstant $sc-mask (sc -1 -1 -1))

;; A UUO is an unimplemented instruction that the exception handler
;; decodes and emulates. The major opcode and low three bits are clear;
;; bit 3 is set.

(defmacro uuo (xop) `(op 0 (dpb ,xop (byte 7 4) (logior (ash 1 3) ppc32::fulltag-imm))))
(defconstant $uuo-mask (logior $op-mask (uuo -1)))
(defconstant $uuorb-mask (logior $uuo-mask $rb-mask))

;; An X form instruction.  
(defmacro x (op xop &optional (base 0)) `(op ,op (dpb ,xop (byte 10 1) ,base)))

;; An X form instruction with the RC bit specified.
(defmacro xrc (op xop &optional (rc 0)) `(op ,op (dpb ,xop (byte 10 1) (logand ,rc 1))))

;; The mask for an X form instruction. 
(defconstant $x-mask (xrc -1 -1 -1))

;; An X-MASK with the RA field fixed.  
(defconstant $xra-mask (logior $x-mask $ra-mask))

;; An X-MASK with the RB field fixed.  
(defconstant $xrb-mask (logior $x-mask $rb-mask))

;; An X-MASK with the RT field fixed.  
(defconstant $xrt-mask (logior $x-mask $rt-mask))

;; An X-MASK with the RA and RB fields fixed.  
(defconstant $xrarb-mask (logior $x-mask $ra-mask $rb-mask))

;; An X-MASK with the RT and RA fields fixed.  
(defconstant $xrtra-mask (logior $x-mask $rt-mask $ra-mask))

;; An X form comparison instruction.  
(defmacro xcmpl (op xop l)
  `(x ,op ,xop (dpb ,l (byte 1 21) 0)))

;; The mask for an X form comparison instruction.  
(defconstant $xcmp-mask (logior $x-mask (ash 1  22)))

;; The mask for an X form comparison instruction with the L field
;; fixed.  
(defconstant $xcmpl-mask (logior $xcmp-mask (ash 1 21)))

(defmacro xsync (op xop l) `(x ,op ,xop (dpb ,l (byte 3 21) 0)))
(defconstant $xsync-mask #xff9fffff)

;; An X form trap instruction with the TO field specified.  
(defmacro xto (op xop to) `(x ,op ,xop (dpb ,to (byte 5 21) 0)))
(defconstant $xto-mask (xto -1 -1 -1))

;; An XFL form instruction.  
(defmacro xfl (op xop &optional (rc 0)) `(op ,op (dpb ,xop (byte 10 1) (logand ,rc 1))))
(defconstant $xfl-mask (logior (xfl -1 -1 -1) (ash 1 25) (ash 1 16)))

;; An XL form instruction with the LK field set to 0. 
(defmacro xl (op xop &optional (base 0)) `(op ,op (dpb ,xop (byte 10 1) ,base)))

;; An XL form instruction which uses the LK field.  
(defmacro xllk (op xop &optional (lk 0)) `(xl ,op ,xop (logand ,lk 1)))

;; The mask for an XL form instruction.  
(defconstant $xl-mask (xllk -1 -1 -1))

;; An XL form instruction which explicitly sets the BO field. 
(defmacro xlo (op bo xop &optional (lk 0))
  `(xl ,op ,xop (dpb ,bo (byte 5 21) (logand ,lk 1))))
(defconstant $xlo-mask (logior $xl-mask $bo-mask))

;; An XL form instruction which explicitly sets the y bit of the BO
;; field.  
(defmacro xlylk (op xop y &optional (lk 0)) `(xl ,op ,xop (dpb ,y (byte 1 21) (logand ,lk 1))))
(defconstant $xlylk-mask (logior $xl-mask $y-mask))

;; An XL form instruction which sets the BO field and the condition
;; bits of the BI field.  
(defmacro xlocb (op bo cb xop &optional (lk 0))
  `(x ,op ,xop (dpb ,bo (byte 5 21) (dpb ,cb (byte 2 16) (logand ,lk 1)))))
(defconstant $xlocb-mask (xlocb -1 -1 -1 -1 -1))

;; An XL-MASK or XLYLK-MASK or XLOCB-MASK with the BB field fixed.  
(defconstant $xlbb-mask (logior $xl-mask $bb-mask))
(defconstant $xlybb-mask (logior $xlylk-mask $bb-mask))
(defconstant $xlbocbbb-mask (logior $xlocb-mask $bb-mask))

;; An XL-MASK with the BO and BB fields fixed.  
(defconstant $xlbobb-mask (logior $xl-mask $bo-mask $bb-mask))

;; An XL-MASK with the BO, BI and BB fields fixed.  
(defconstant $xlbobibb-mask (logior $xl-mask $bo-mask $bi-mask $bb-mask))

;; An XO form instruction. 
(defmacro xo (op xop oe rc)
  `(op ,op (dpb ,xop (byte 9 1) (dpb ,oe (byte 1 10) (logand ,rc 1)))))
(defconstant $xo-mask (xo -1 -1 -1 -1))

;; An XO-MASK with the RB field fixed.  
(defconstant $xorb-mask (logior $xo-mask $rb-mask))

;; An XS form instruction.  
(defmacro xs (op xop &optional (rc 0)) 
  `(op ,op (dpb ,xop (byte 9 2) (logand ,rc 1))))
(defconstant $xs-mask (xs -1 -1 -1))

;; An XFX form instruction with the SPR field filled in.  
(defmacro xspr (op xop spr) `(x ,op ,xop (dpb ,spr (byte 5 16) (ash (logand ,spr #x3e0) 6))))
(defconstant $xspr-mask (logior $x-mask $spr-mask))

;; A VX form instruction.
(defmacro vx (op xop) `(op ,op (dpb ,xop (byte 11 0) 0)))
(defconstant $vx-mask (vx -1 -1))

;; A VXR form instruction.
(defmacro vxr (op xop rc) `(op ,op (dpb ,xop (byte 10 0) (ash (logand ,rc 1) 10))))
(defconstant $vxr-mask (vxr -1 -1 1))
  
;; A VXA form instruction.
(defmacro vxa (op xop &optional (base 0)) `(op ,op (dpb ,xop (byte 6 0) ,base)))
(defconstant $vxa-mask (vxa -1 -1))
(defconstant $vash-mask (logior $vxa-mask (ash 1 10)))




;; The BO encodings used in extended conditional branch mnemonics.  
(defconstant $bodnzf #x0)
(defconstant $bodnzfp #x1)
(defconstant $bodzf #x2)
(defconstant $bodzfp #x3)
(defconstant $bof #x4)
(defconstant $bofp #x5)
(defconstant $bodnzt #x8)
(defconstant $bodnztp #x9)
(defconstant $bodzt #xa)
(defconstant $bodztp #xb)
(defconstant $bot #xc)
(defconstant $botp #xd)
(defconstant $bodnz #x10)
(defconstant $bodnzp #x11)
(defconstant $bodz #x12)
(defconstant $bodzp #x13)
(defconstant $bou #x14)
 
;; The BI condition bit encodings used in extended conditional branch
;;   mnemonics. 
(defconstant $cblt 0)
(defconstant $cbgt 1)
(defconstant $cbeq 2)
(defconstant $cbso 3)

;; The TO encodings used in extended trap mnemonics.
(defconstant $tolgt #x1)
(defconstant $tollt #x2)
(defconstant $toeq #x4)
(defconstant $tolge #x5)
(defconstant $tolnl #x5)
(defconstant $tolle #x6)
(defconstant $tolng #x6)
(defconstant $togt #x8)
(defconstant $toge #xc)
(defconstant $tonl #xc)
(defconstant $tolt #x10)
(defconstant $tole #x14)
(defconstant $tong #x14)
(defconstant $tone #x18)
(defconstant $tou #x1f)


)



(eval-when (:compile-toplevel :execute)
(defun max-operand-count (opnums)
  (let* ((max 0))
    (declare (fixnum max))
    (dolist (i opnums max)
      (unless 
        (logbitp ccl::operand-fake (ccl::operand-flags (svref *ppc-operands* i)))
        (incf max)))))

(defun min-operand-count (opnums)
  (let* ((min 0))
    (declare (fixnum min))
    (dolist (i opnums min)
      (let* ((flags (ccl::operand-flags (svref *ppc-operands* i))))
        (declare (fixnum flags))
        (unless (or (logbitp ccl::operand-fake flags)
                    (logbitp ccl::operand-optional flags))
          (incf min))))))

(defmacro ppc-opcode (name opcode mask (&rest flags) &rest operands)
  `(ccl::make-opcode
    :name (string ',name)
    :opcode ,opcode
    :majorop (major-opcode ,opcode)
    :mask ,mask
    :flags (logior ,@(mapcar #'(lambda (f) `(ash 1 ,f)) flags))
    :min-args (min-operand-count (list ,@operands))
    :max-args (max-operand-count (list ,@operands))
    :operands (mapcar #'(lambda (i) (svref *ppc-operands* i)) (list ,@operands))))
)


; The #.s are a necesary evil here (to keep the function vector size < 32K) in MCL 3.0.

; If you change this, you need to evaluate (initialize-ppc-opcode-numbers)
(defparameter *ppc-opcodes*
  (vector
   #.(ppc-opcode uuo_interr (uuo 11) $uuo-mask ($ppc) $uuo-errnum $rb)
   #.(ppc-opcode uuo_intcerr (uuo 12) $uuo-mask ($ppc) $uuo-errnum $rb)
   #.(ppc-opcode uuo_interr2 (uuo 13) $uuo-mask ($ppc) $uuo-small-errnum $ra $rb)
   #.(ppc-opcode uuo_intcerr2 (uuo 14) $uuo-mask ($ppc) $uuo-small-errnum $ra $rb)
   ;; We'll clearly need more; add a few "anonymous" ones for now so that
   ;; other opcode's opcode numbers stay constant.
   #.(ppc-opcode uuo_fpuXbinop (uuo 22) $uuo-mask ($ppc) $frt $fra $frb)

   #.(ppc-opcode tdlgti (opto 2 $tolgt) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdllti (opto 2 $tollt) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdeqi (opto 2 $toeq) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdlgei (opto 2 $tolge) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdlnli (opto 2 $tolnl) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdllei (opto 2 $tolle) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdlngi (opto 2 $tolng) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdgti (opto 2 $togt) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdgei (opto 2 $toge) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdnli (opto 2 $tonl) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdlti (opto 2 $tolt) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdlei (opto 2 $tole) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdngi (opto 2 $tong) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdnei (opto 2 $tone) $opto-mask ($ppc $b64) $ra $si)
   #.(ppc-opcode tdi (op 2) $op-mask ($ppc $b64) $to $ra $si)

   #.(ppc-opcode twlgti (opto 3 $tolgt) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twllti (opto 3 $tollt) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode tweqi (opto 3 $toeq) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twlgei (opto 3 $tolge) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twlnli (opto 3 $tolnl) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twllei (opto 3 $tolle) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twlngi (opto 3 $tolng) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twgti (opto 3 $togt) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twgei (opto 3 $toge) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twnli (opto 3 $tonl) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twlti (opto 3 $tolt) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twlei (opto 3 $tole) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twngi (opto 3 $tong) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twnei (opto 3 $tone) $opto-mask ($ppc) $ra $si)
   #.(ppc-opcode twi (op 3) $op-mask ($ppc) $to $ra $si)

   #.(ppc-opcode mfvscr (vx 4 1540) $vx-mask ($ppc) $vd )
   #.(ppc-opcode mtvscr (vx 4 1604) $vx-mask ($ppc) $vd )
   #.(ppc-opcode vaddcuw (vx 4 384) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vaddfp (vx 4 10) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vaddsbs (vx 4 768) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vaddshs (vx 4 832) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vaddsws (vx 4 896) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vaddubm (vx 4 0) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vaddubs (vx 4 512) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vadduhm (vx 4 64) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vadduhs (vx 4 576) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vadduwm (vx 4 128) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vadduws (vx 4 640) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vand (vx 4 1028) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vandc (vx 4 1092) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vavgsb (vx 4 1282) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vavgsh (vx 4 1346) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vavgsw (vx 4 1410) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vavgub (vx 4 1026) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vavguh (vx 4 1090) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vavguw (vx 4 1154) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcfsx (vx 4 842) $vx-mask ($ppc) $vd $vb $vuimm )
   #.(ppc-opcode vcfux (vx 4 778) $vx-mask ($ppc) $vd $vb $vuimm )
   #.(ppc-opcode vcmpbfp (vxr 4 966 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpbfp. (vxr 4 966 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpeqfp (vxr 4 198 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpeqfp. (vxr 4 198 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpequb (vxr 4 6 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpequb. (vxr 4 6 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpequh (vxr 4 70 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpequh. (vxr 4 70 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpequw (vxr 4 134 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpequw. (vxr 4 134 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgefp (vxr 4 454 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgefp. (vxr 4 454 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtfp (vxr 4 710 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtfp. (vxr 4 710 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtsb (vxr 4 774 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtsb. (vxr 4 774 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtsh (vxr 4 838 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtsh. (vxr 4 838 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtsw (vxr 4 902 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtsw. (vxr 4 902 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtub (vxr 4 518 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtub. (vxr 4 518 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtuh (vxr 4 582 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtuh. (vxr 4 582 1) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtuw (vxr 4 646 0) $vxr-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vcmpgtuw. (vxr 4 646 1) $vxr-mask  ($ppc) $vd $va $vb )
   #.(ppc-opcode vctsxs (vx 4 970) $vx-mask ($ppc) $vd $vb $vuimm )
   #.(ppc-opcode vctuxs (vx 4 906) $vx-mask ($ppc) $vd $vb $vuimm )
   #.(ppc-opcode vexptefp (vx 4 394) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vlogefp (vx 4 458) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vmaddfp (vxa 4 46) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vmaxfp (vx 4 1034) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmaxsb (vx 4 258) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmaxsh (vx 4 322) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmaxsw (vx 4 386) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmaxub (vx 4 2) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmaxuh (vx 4 66) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmaxuw (vx 4 130) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmhaddshs (vxa 4 32) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vmhraddshs (vxa 4 33) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vminfp (vx 4 1098) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vminsb (vx 4 770) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vminsh (vx 4 834) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vminsw (vx 4 898) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vminub (vx 4 514) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vminuh (vx 4 578) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vminuw (vx 4 642) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmladduhm (vxa 4 34) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vmrghb (vx 4 12) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmrghh (vx 4 76) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmrghw (vx 4 140) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmrglb (vx 4 268) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmrglh (vx 4 332) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmrglw (vx 4 396) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmsummbm (vxa 4 37) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vmsumshm (vxa 4 40) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vmsumshs (vxa 4 41) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vmsumubm (vxa 4 36) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vmsumuhm (vxa 4 38) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vmsumuhs (vxa 4 39) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vmulesb (vx 4 776) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmulesh (vx 4 840) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmuleub (vx 4 520) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmuleuh (vx 4 584) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmulosb (vx 4 264) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmulosh (vx 4 328) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmuloub (vx 4 8) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vmulouh (vx 4 72) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vnmsubfp (vxa 4 47) $vxa-mask ($ppc) $vd $va $vc $vb )
   #.(ppc-opcode vnor (vx 4 1284) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vor (vx 4 1156) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vperm (vxa 4 43) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vpkpx (vx 4 782) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vpkshss (vx 4 398) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vpkshus (vx 4 270) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vpkswss (vx 4 462) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vpkswus (vx 4 334) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vpkuhum (vx 4 14) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vpkuhus (vx 4 142) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vpkuwum (vx 4 78) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vpkuwus (vx 4 206) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vrefp (vx 4 266) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vrfim (vx 4 714) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vrfin (vx 4 522) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vrfip (vx 4 650) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vrfiz (vx 4 586) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vrlb (vx 4 4) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vrlh (vx 4 68) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vrlw (vx 4 132) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vrsqrtefp (vx 4 330) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vsel (vxa 4 42) $vxa-mask ($ppc) $vd $va $vb $vc )
   #.(ppc-opcode vsl (vx 4 452) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vslb (vx 4 260) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsldoi (vxa 4 44) $vxa-mask ($ppc) $vd $va $vb $vsh)
   #.(ppc-opcode vslh (vx 4 324) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vslo (vx 4 1036) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vslw (vx 4 388) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vspltb (vx 4 524) $vx-mask ($ppc) $vd $vb $vuimm )
   #.(ppc-opcode vsplth (vx 4 588) $vx-mask ($ppc) $vd $vb $vuimm )
   #.(ppc-opcode vspltisb (vx 4 780) $vx-mask ($ppc) $vd $vsimm )
   #.(ppc-opcode vspltish (vx 4 844) $vx-mask ($ppc) $vd $vsimm )
   #.(ppc-opcode vspltisw (vx 4 908) $vx-mask ($ppc) $vd $vsimm )
   #.(ppc-opcode vspltw (vx 4 652) $vx-mask ($ppc) $vd $vb $vuimm )
   #.(ppc-opcode vsr (vx 4 708) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsrab (vx 4 772) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsrah (vx 4 836) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsraw (vx 4 900) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsrb (vx 4 516) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsrh (vx 4 580) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsro (vx 4 1100) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsrw (vx 4 644) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsubcuw (vx 4 1408) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsubfp (vx 4 74) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsubsbs (vx 4 1792) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsubshs (vx 4 1856) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsubsws (vx 4 1920) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsububm (vx 4 1024) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsububs (vx 4 1536) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsubuhm (vx 4 1088) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsubuhs (vx 4 1600) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsubuwm (vx 4 1152) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsubuws (vx 4 1664) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsumsws (vx 4 1928) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsum2sws (vx 4 1672) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsum4sbs (vx 4 1800) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsum4shs (vx 4 1608) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vsum4ubs (vx 4 1544) $vx-mask ($ppc) $vd $va $vb )
   #.(ppc-opcode vupkhpx (vx 4 846) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vupkhsb (vx 4 526) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vupkhsh (vx 4 590) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vupklpx (vx 4 974) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vupklsb (vx 4 654) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vupklsh (vx 4 718) $vx-mask ($ppc) $vd $vb )
   #.(ppc-opcode vxor (vx 4 1220) $vx-mask ($ppc) $vd $va $vb  )

   #.(ppc-opcode mulli (op 7) $op-mask ($ppc) $rt $ra $si)
   
   #.(ppc-opcode subfic (op 8) $op-mask ($ppc) $rt $ra $si)
   
   #.(ppc-opcode cmplwi (opl 10 0) $opl-mask ($ppc) $obf $ra $ui)

   #.(ppc-opcode cmpldi (opl 10 1) $opl-mask ($ppc $b64) $obf $ra $ui)

   #.(ppc-opcode cmpli (op 10) $op-mask ($ppc) $bf $l $ra $ui)

   #.(ppc-opcode cmpwi (opl 11 0) $opl-mask ($ppc) $obf $ra $si)

   #.(ppc-opcode cmpdi (opl 11 1) $opl-mask ($ppc $b64) $obf $ra $si)

   #.(ppc-opcode cmpi (op 11) $op-mask ($ppc) $bf $l $ra $si)

   #.(ppc-opcode addic (op 12) $op-mask ($ppc) $rt $ra $si)
   #.(ppc-opcode subic (op 12) $op-mask ($ppc) $rt $ra $nsi)

   #.(ppc-opcode addic. (op 13) $op-mask ($ppc) $rt $ra $si)
   #.(ppc-opcode subic. (op 13) $op-mask ($ppc) $rt $ra $nsi)

   #.(ppc-opcode li (op 14) $dra-mask ($ppc) $rt $si)
   #.(ppc-opcode addi (op 14) $op-mask ($ppc) $rt $ra $si)
   #.(ppc-opcode subi (op 14) $op-mask ($ppc) $rt $ra $nsi)
   #.(ppc-opcode la (op 14) $op-mask ($ppc) $rt $d $ra)

   #.(ppc-opcode lis (op 15) $dra-mask ($ppc) $rt $sisignopt)
   #.(ppc-opcode addis (op 15) $op-mask ($ppc) $rt $ra $sisignopt)
   #.(ppc-opcode subis (op 15) $op-mask ($ppc) $rt $ra $nsi)

   #.(ppc-opcode bdnz- (bbo 16 $bodnz 0 0) $bboybi-mask ($ppc) $bdm)
   #.(ppc-opcode bdnz+ (bbo 16 $bodnz 0 0) $bboybi-mask ($ppc) $bdp)
   #.(ppc-opcode bdnz (bbo 16 $bodnz 0 0) $bboybi-mask ($ppc) $bd)
   #.(ppc-opcode bdnzl- (bbo 16 $bodnz 0 1) $bboybi-mask ($ppc) $bdm)
   #.(ppc-opcode bdnzl+ (bbo 16 $bodnz 0 1) $bboybi-mask ($ppc) $bdp)
   #.(ppc-opcode bdnzl (bbo 16 $bodnz 0 1) $bboybi-mask ($ppc) $bd)
   #.(ppc-opcode bdnza- (bbo 16 $bodnz 1 0) $bboybi-mask ($ppc) $bdma)
   #.(ppc-opcode bdnza+ (bbo 16 $bodnz 1 0) $bboybi-mask ($ppc) $bdpa)
   #.(ppc-opcode bdnza (bbo 16 $bodnz 1 0) $bboybi-mask ($ppc) $bda)
   #.(ppc-opcode bdnzla- (bbo 16 $bodnz 1 1) $bboybi-mask ($ppc) $bdma)
   #.(ppc-opcode bdnzla+ (bbo 16 $bodnz 1 1) $bboybi-mask ($ppc) $bdpa)
   #.(ppc-opcode bdnzla (bbo 16 $bodnz 1 1) $bboybi-mask ($ppc) $bda)
   #.(ppc-opcode bdz- (bbo 16 $bodz 0 0) $bboybi-mask ($ppc) $bdm)
   #.(ppc-opcode bdz+ (bbo 16 $bodz 0 0) $bboybi-mask ($ppc) $bdp)
   #.(ppc-opcode bdz (bbo 16 $bodz 0 0) $bboybi-mask ($ppc) $bd)
   #.(ppc-opcode bdzl- (bbo 16 $bodz 0 1) $bboybi-mask ($ppc) $bdm)
   #.(ppc-opcode bdzl+ (bbo 16 $bodz 0 1) $bboybi-mask ($ppc) $bdp)
   #.(ppc-opcode bdzl (bbo 16 $bodz 0 1) $bboybi-mask ($ppc) $bd)
   #.(ppc-opcode bdza- (bbo 16 $bodz 1 0) $bboybi-mask ($ppc) $bdma)
   #.(ppc-opcode bdza+ (bbo 16 $bodz 1 0) $bboybi-mask ($ppc) $bdpa)
   #.(ppc-opcode bdza (bbo 16 $bodz 1 0) $bboybi-mask ($ppc) $bda)
   #.(ppc-opcode bdzla- (bbo 16 $bodz 1 1) $bboybi-mask ($ppc) $bdma)
   #.(ppc-opcode bdzla+ (bbo 16 $bodz 1 1) $bboybi-mask ($ppc) $bdpa)
   #.(ppc-opcode bdzla (bbo 16 $bodz 1 1) $bboybi-mask ($ppc) $bda)
   #.(ppc-opcode blt- (bbocb 16 $bot $cblt 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode blt+ (bbocb 16 $bot $cblt 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode blt (bbocb 16 $bot $cblt 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bltl- (bbocb 16 $bot $cblt 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bltl+ (bbocb 16 $bot $cblt 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bltl (bbocb 16 $bot $cblt 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode blta- (bbocb 16 $bot $cblt 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode blta+ (bbocb 16 $bot $cblt 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode blta (bbocb 16 $bot $cblt 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bltla- (bbocb 16 $bot $cblt 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bltla+ (bbocb 16 $bot $cblt 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bltla (bbocb 16 $bot $cblt 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bgt- (bbocb 16 $bot $cbgt 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bgt+ (bbocb 16 $bot $cbgt 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bgt (bbocb 16 $bot $cbgt 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bgtl- (bbocb 16 $bot $cbgt 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bgtl+ (bbocb 16 $bot $cbgt 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bgtl (bbocb 16 $bot $cbgt 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bgta- (bbocb 16 $bot $cbgt 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bgta+ (bbocb 16 $bot $cbgt 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bgta (bbocb 16 $bot $cbgt 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bgtla- (bbocb 16 $bot $cbgt 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bgtla+ (bbocb 16 $bot $cbgt 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bgtla (bbocb 16 $bot $cbgt 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode beq- (bbocb 16 $bot $cbeq 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode beq+ (bbocb 16 $bot $cbeq 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode beq (bbocb 16 $bot $cbeq 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode beql- (bbocb 16 $bot $cbeq 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode beql+ (bbocb 16 $bot $cbeq 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode beql (bbocb 16 $bot $cbeq 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode beqa- (bbocb 16 $bot $cbeq 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode beqa+ (bbocb 16 $bot $cbeq 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode beqa (bbocb 16 $bot $cbeq 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode beqla- (bbocb 16 $bot $cbeq 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode beqla+ (bbocb 16 $bot $cbeq 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode beqla (bbocb 16 $bot $cbeq 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bso- (bbocb 16 $bot $cbso 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bso+ (bbocb 16 $bot $cbso 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bso (bbocb 16 $bot $cbso 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bsol- (bbocb 16 $bot $cbso 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bsol+ (bbocb 16 $bot $cbso 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bsol (bbocb 16 $bot $cbso 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bsoa- (bbocb 16 $bot $cbso 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bsoa+ (bbocb 16 $bot $cbso 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bsoa (bbocb 16 $bot $cbso 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bsola- (bbocb 16 $bot $cbso 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bsola+ (bbocb 16 $bot $cbso 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bsola (bbocb 16 $bot $cbso 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bun- (bbocb 16 $bot $cbso 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bun+ (bbocb 16 $bot $cbso 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bun (bbocb 16 $bot $cbso 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bunl- (bbocb 16 $bot $cbso 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bunl+ (bbocb 16 $bot $cbso 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bunl (bbocb 16 $bot $cbso 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode buna- (bbocb 16 $bot $cbso 0 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode buna+ (bbocb 16 $bot $cbso 0 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode buna (bbocb 16 $bot $cbso 0 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bunla- (bbocb 16 $bot $cbso 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bunla+ (bbocb 16 $bot $cbso 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bunla (bbocb 16 $bot $cbso 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bge- (bbocb 16 $bof $cblt 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bge+ (bbocb 16 $bof $cblt 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bge (bbocb 16 $bof $cblt 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bgel- (bbocb 16 $bof $cblt 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bgel+ (bbocb 16 $bof $cblt 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bgel (bbocb 16 $bof $cblt 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bgea- (bbocb 16 $bof $cblt 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bgea+ (bbocb 16 $bof $cblt 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bgea (bbocb 16 $bof $cblt 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bgela- (bbocb 16 $bof $cblt 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bgela+ (bbocb 16 $bof $cblt 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bgela (bbocb 16 $bof $cblt 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bnl- (bbocb 16 $bof $cblt  0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bnl+ (bbocb 16 $bof $cblt  0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bnl (bbocb 16 $bof $cblt 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bnll- (bbocb 16 $bof $cblt 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bnll+ (bbocb 16 $bof $cblt 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bnll (bbocb 16 $bof $cblt 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bnla- (bbocb 16 $bof $cblt 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bnla+ (bbocb 16 $bof $cblt 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bnla (bbocb 16 $bof $cblt 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bnlla- (bbocb 16 $bof $cblt 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bnlla+ (bbocb 16 $bof $cblt 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bnlla (bbocb 16 $bof $cblt 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode ble- (bbocb 16 $bof $cbgt 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode ble+ (bbocb 16 $bof $cbgt 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode ble (bbocb 16 $bof $cbgt 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode blel- (bbocb 16 $bof $cbgt 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode blel+ (bbocb 16 $bof $cbgt 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode blel (bbocb 16 $bof $cbgt 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode blea- (bbocb 16 $bof $cbgt 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode blea+ (bbocb 16 $bof $cbgt 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode blea (bbocb 16 $bof $cbgt 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode blela- (bbocb 16 $bof $cbgt 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode blela+ (bbocb 16 $bof $cbgt 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode blela (bbocb 16 $bof $cbgt 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bng- (bbocb 16 $bof $cbgt 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bng+ (bbocb 16 $bof $cbgt 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bng (bbocb 16 $bof $cbgt 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bngl- (bbocb 16 $bof $cbgt 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bngl+ (bbocb 16 $bof $cbgt 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bngl (bbocb 16 $bof $cbgt 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bnga- (bbocb 16 $bof $cbgt 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bnga+ (bbocb 16 $bof $cbgt 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bnga (bbocb 16 $bof $cbgt 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bngla- (bbocb 16 $bof $cbgt 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bngla+ (bbocb 16 $bof $cbgt 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bngla (bbocb 16 $bof $cbgt 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bne- (bbocb 16 $bof $cbeq 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bne+ (bbocb 16 $bof $cbeq 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bne (bbocb 16 $bof $cbeq 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bnel- (bbocb 16 $bof $cbeq 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bnel+ (bbocb 16 $bof $cbeq 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bnel (bbocb 16 $bof $cbeq 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bnea- (bbocb 16 $bof $cbeq 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bnea+ (bbocb 16 $bof $cbeq 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bnea (bbocb 16 $bof $cbeq 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bnela- (bbocb 16 $bof $cbeq 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bnela+ (bbocb 16 $bof $cbeq 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bnela (bbocb 16 $bof $cbeq 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bns- (bbocb 16 $bof $cbso 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bns+ (bbocb 16 $bof $cbso 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bns (bbocb 16 $bof $cbso 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bnsl- (bbocb 16 $bof $cbso 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bnsl+ (bbocb 16 $bof $cbso 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bnsl (bbocb 16 $bof $cbso 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bnsa- (bbocb 16 $bof $cbso 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bnsa+ (bbocb 16 $bof $cbso 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bnsa (bbocb 16 $bof $cbso 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bnsla- (bbocb 16 $bof $cbso 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bnsla+ (bbocb 16 $bof $cbso 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bnsla (bbocb 16 $bof $cbso 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bnu- (bbocb 16 $bof $cbso 0 0) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bnu+ (bbocb 16 $bof $cbso 0 0) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bnu (bbocb 16 $bof $cbso 0 0) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bnul- (bbocb 16 $bof $cbso 0 1) $bboycb-mask ($ppc) $cr $bdm)
   #.(ppc-opcode bnul+ (bbocb 16 $bof $cbso 0 1) $bboycb-mask ($ppc) $cr $bdp)
   #.(ppc-opcode bnul (bbocb 16 $bof $cbso 0 1) $bboycb-mask ($ppc) $cr $bd)
   #.(ppc-opcode bnua- (bbocb 16 $bof $cbso 1 0) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bnua+ (bbocb 16 $bof $cbso 1 0) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bnua (bbocb 16 $bof $cbso 1 0) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bnula- (bbocb 16 $bof $cbso 1 1) $bboycb-mask ($ppc) $cr $bdma)
   #.(ppc-opcode bnula+ (bbocb 16 $bof $cbso 1 1) $bboycb-mask ($ppc) $cr $bdpa)
   #.(ppc-opcode bnula (bbocb 16 $bof $cbso 1 1) $bboycb-mask ($ppc) $cr $bda)
   #.(ppc-opcode bdnzt- (bbo 16 $bodnzt 0 0) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bdnzt+ (bbo 16 $bodnzt 0 0) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bdnzt (bbo 16 $bodnzt 0 0) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bdnztl- (bbo 16 $bodnzt 0 1) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bdnztl+ (bbo 16 $bodnzt 0 1) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bdnztl (bbo 16 $bodnzt 0 1) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bdnzta- (bbo 16 $bodnzt 1 0) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bdnzta+ (bbo 16 $bodnzt 1 0) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bdnzta (bbo 16 $bodnzt 1 0) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bdnztla- (bbo 16 $bodnzt 1 1) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bdnztla+ (bbo 16 $bodnzt 1 1) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bdnztla (bbo 16 $bodnzt 1 1) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bdnzf- (bbo 16 $bodnzf 0 0) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bdnzf+ (bbo 16 $bodnzf 0 0) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bdnzf (bbo 16 $bodnzf 0 0) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bdnzfl- (bbo 16 $bodnzf 0 1) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bdnzfl+ (bbo 16 $bodnzf 0 1) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bdnzfl (bbo 16 $bodnzf 0 1) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bdnzfa- (bbo 16 $bodnzf 1 0) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bdnzfa+ (bbo 16 $bodnzf 1 0) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bdnzfa (bbo 16 $bodnzf 1 0) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bdnzfla- (bbo 16 $bodnzf 1 1) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bdnzfla+ (bbo 16 $bodnzf 1 1) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bdnzfla (bbo 16 $bodnzf 1 1) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bt- (bbo 16 $bot 0 0) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bt+ (bbo 16 $bot 0 0) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bt (bbo 16 $bot 0 0) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode btl- (bbo 16 $bot 0 1) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode btl+ (bbo 16 $bot 0 1) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode btl (bbo 16 $bot 0 1) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bta- (bbo 16 $bot 1 0) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bta+ (bbo 16 $bot 1 0) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bta (bbo 16 $bot 1 0) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode btla- (bbo 16 $bot 1 1) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode btla+ (bbo 16 $bot 1 1) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode btla (bbo 16 $bot 1 1) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bf- (bbo 16 $bof 0 0) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bf+ (bbo 16 $bof 0 0) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bf (bbo 16 $bof 0 0) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bfl- (bbo 16 $bof 0 1) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bfl+ (bbo 16 $bof 0 1) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bfl (bbo 16 $bof 0 1) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bfa- (bbo 16 $bof 1 0) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bfa+ (bbo 16 $bof 1 0) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bfa (bbo 16 $bof 1 0) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bfla- (bbo 16 $bof 1 1) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bfla+ (bbo 16 $bof 1 1) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bfla (bbo 16 $bof 1 1) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bdzt- (bbo 16 $bodzt 0 0) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bdzt+ (bbo 16 $bodzt 0 0) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bdzt (bbo 16 $bodzt 0 0) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bdztl- (bbo 16 $bodzt 0 1) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bdztl+ (bbo 16 $bodzt 0 1) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bdztl (bbo 16 $bodzt 0 1) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bdzta- (bbo 16 $bodzt 1 0) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bdzta+ (bbo 16 $bodzt 1 0) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bdzta (bbo 16 $bodzt 1 0) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bdztla- (bbo 16 $bodzt 1 1) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bdztla+ (bbo 16 $bodzt 1 1) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bdztla (bbo 16 $bodzt 1 1) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bdzf- (bbo 16 $bodzf 0 0) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bdzf+ (bbo 16 $bodzf 0 0) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bdzf (bbo 16 $bodzf 0 0) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bdzfl- (bbo 16 $bodzf 0 1) $bboy-mask ($ppc) $bi $bdm)
   #.(ppc-opcode bdzfl+ (bbo 16 $bodzf 0 1) $bboy-mask ($ppc) $bi $bdp)
   #.(ppc-opcode bdzfl (bbo 16 $bodzf 0 1) $bboy-mask ($ppc) $bi $bd)
   #.(ppc-opcode bdzfa- (bbo 16 $bodzf 1 0) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bdzfa+ (bbo 16 $bodzf 1 0) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bdzfa (bbo 16 $bodzf 1 0) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bdzfla- (bbo 16 $bodzf 1 1) $bboy-mask ($ppc) $bi $bdma)
   #.(ppc-opcode bdzfla+ (bbo 16 $bodzf 1 1) $bboy-mask ($ppc) $bi $bdpa)
   #.(ppc-opcode bdzfla (bbo 16 $bodzf 1 1) $bboy-mask ($ppc) $bi $bda)
   #.(ppc-opcode bc- (b 16 0 0) $b-mask ($ppc) $boe $bi $bdm)
   #.(ppc-opcode bc+ (b 16 0 0) $b-mask ($ppc) $boe $bi $bdp)
   #.(ppc-opcode bc (b 16 0 0) $b-mask ($ppc) $bo $bi $bd)
   #.(ppc-opcode bcl- (b 16 0 1) $b-mask ($ppc) $boe $bi $bdm)
   #.(ppc-opcode bcl+ (b 16 0 1) $b-mask ($ppc) $boe $bi $bdp)
   #.(ppc-opcode bcl (b 16 0 1) $b-mask ($ppc) $bo $bi $bd)
   #.(ppc-opcode bca- (b 16 1 0) $b-mask ($ppc) $boe $bi $bdma)
   #.(ppc-opcode bca+ (b 16 1 0) $b-mask ($ppc) $boe $bi $bdpa)
   #.(ppc-opcode bca (b 16 1 0) $b-mask ($ppc) $bo $bi $bda)
   #.(ppc-opcode bcla- (b 16 1 1) $b-mask ($ppc) $boe $bi $bdma)
   #.(ppc-opcode bcla+ (b 16 1 1) $b-mask ($ppc) $boe $bi $bdpa)
   #.(ppc-opcode bcla (b 16 1 1) $b-mask ($ppc) $bo $bi $bda)

   #.(ppc-opcode sc (sc 17 1 0) #xffffffff ($ppc))

   #.(ppc-opcode b (b 18 0 0) $b-mask ($ppc) $li)
   #.(ppc-opcode bl (b 18 0 1) $b-mask ($ppc) $li)
   #.(ppc-opcode ba (b 18 1 0) $b-mask ($ppc) $lia)
   #.(ppc-opcode bla (b 18 1 1) $b-mask ($ppc) $lia)

   #.(ppc-opcode mcrf (xl 19 0) (logior $xlbb-mask (ash 3 21) (ash 3 16)) ($ppc) $bf $bfa)

   #.(ppc-opcode blr (xlo 19 $bou 16 0) $xlbobibb-mask ($ppc))
   #.(ppc-opcode blrl (xlo 19 $bou 16 1) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdnzlr (xlo 19 $bodnz 16 0) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdnzlr- (xlo 19 $bodnz 16 0) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdnzlr+ (xlo 19 $bodnzp 16 0) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdnzlrl (xlo 19 $bodnz 16 1) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdnzlrl- (xlo 19 $bodnz 16 1) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdnzlrl+ (xlo 19 $bodnzp 16 1) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdzlr (xlo 19 $bodz 16 0) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdzlr- (xlo 19 $bodz 16 0) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdzlr+ (xlo 19 $bodzp 16 0) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdzlrl (xlo 19 $bodz 16 1) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdzlrl- (xlo 19 $bodz 16 1) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bdzlrl+ (xlo 19 $bodzp 16 1) $xlbobibb-mask ($ppc))
   #.(ppc-opcode bltlr (xlocb 19 $bot $cblt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltlr- (xlocb 19 $bot $cblt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltlr+ (xlocb 19 $botp $cblt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltlrl (xlocb 19 $bot $cblt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltlrl- (xlocb 19 $bot $cblt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltlrl+ (xlocb 19 $botp $cblt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtlr (xlocb 19 $bot $cbgt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtlr- (xlocb 19 $bot $cbgt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtlr+ (xlocb 19 $botp $cbgt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtlrl (xlocb 19 $bot $cbgt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtlrl- (xlocb 19 $bot $cbgt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtlrl+ (xlocb 19 $botp $cbgt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqlr (xlocb 19 $bot $cbeq 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqlr- (xlocb 19 $bot $cbeq 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqlr+ (xlocb 19 $botp $cbeq 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqlrl (xlocb 19 $bot $cbeq 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqlrl- (xlocb 19 $bot $cbeq 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqlrl+ (xlocb 19 $botp $cbeq 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsolr (xlocb 19 $bot $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsolr- (xlocb 19 $bot $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsolr+ (xlocb 19 $botp $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsolrl (xlocb 19 $bot $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsolrl- (xlocb 19 $bot $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsolrl+ (xlocb 19 $botp $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunlr (xlocb 19 $bot $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunlr- (xlocb 19 $bot $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunlr+ (xlocb 19 $botp $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunlrl (xlocb 19 $bot $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunlrl- (xlocb 19 $bot $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunlrl+ (xlocb 19 $botp $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgelr (xlocb 19 $bof $cblt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgelr- (xlocb 19 $bof $cblt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgelr+ (xlocb 19 $bofp $cblt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgelrl (xlocb 19 $bof $cblt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgelrl- (xlocb 19 $bof $cblt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgelrl+ (xlocb 19 $bofp $cblt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnllr (xlocb 19 $bof $cblt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnllr- (xlocb 19 $bof $cblt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnllr+ (xlocb 19 $bofp $cblt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnllrl (xlocb 19 $bof $cblt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnllrl- (xlocb 19 $bof $cblt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnllrl+ (xlocb 19 $bofp $cblt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blelr (xlocb 19 $bof $cbgt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blelr- (xlocb 19 $bof $cbgt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blelr+ (xlocb 19 $bofp $cbgt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blelrl (xlocb 19 $bof $cbgt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blelrl- (xlocb 19 $bof $cbgt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blelrl+ (xlocb 19 $bofp $cbgt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnglr (xlocb 19 $bof $cbgt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnglr- (xlocb 19 $bof $cbgt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnglr+ (xlocb 19 $bofp $cbgt 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnglrl (xlocb 19 $bof $cbgt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnglrl- (xlocb 19 $bof $cbgt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnglrl+ (xlocb 19 $bofp $cbgt 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnelr (xlocb 19 $bof $cbeq 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnelr- (xlocb 19 $bof $cbeq 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnelr+ (xlocb 19 $bofp $cbeq 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnelrl (xlocb 19 $bof $cbeq 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnelrl- (xlocb 19 $bof $cbeq 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnelrl+ (xlocb 19 $bofp $cbeq 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnslr (xlocb 19 $bof $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnslr- (xlocb 19 $bof $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnslr+ (xlocb 19 $bofp $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnslrl (xlocb 19 $bof $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnslrl- (xlocb 19 $bof $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnslrl+ (xlocb 19 $bofp $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnulr (xlocb 19 $bof $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnulr- (xlocb 19 $bof $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnulr+ (xlocb 19 $bofp $cbso 16) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnulrl (xlocb 19 $bof $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnulrl- (xlocb 19 $bof $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnulrl+ (xlocb 19 $bofp $cbso 16 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode btlr (xlo 19 $bot 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btlr- (xlo 19 $bot 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btlr+ (xlo 19 $botp 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btlrl (xlo 19 $bot 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btlrl- (xlo 19 $bot 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btlrl+ (xlo 19 $botp 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bflr (xlo 19 $bof 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bflr- (xlo 19 $bof 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bflr+ (xlo 19 $bofp 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bflrl (xlo 19 $bof 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bflrl- (xlo 19 $bof 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bflrl+ (xlo 19 $bofp 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnztlr (xlo 19 $bodnzt 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnztlr- (xlo 19 $bodnzt 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnztlr+ (xlo 19 $bodnztp 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnztlrl (xlo 19 $bodnzt 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnztlrl- (xlo 19 $bodnzt 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnztlrl+ (xlo 19 $bodnztp 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnzflr (xlo 19 $bodnzf 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnzflr- (xlo 19 $bodnzf 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnzflr+ (xlo 19 $bodnzfp 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnzflrl (xlo 19 $bodnzf 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnzflrl- (xlo 19 $bodnzf 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdnzflrl+ (xlo 19 $bodnzfp 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdztlr (xlo 19 $bodzt 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdztlr- (xlo 19 $bodzt 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdztlr+ (xlo 19 $bodztp 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdztlrl (xlo 19 $bodzt 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdztlrl- (xlo 19 $bodzt 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdztlrl+ (xlo 19 $bodztp 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdzflr (xlo 19 $bodzf 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdzflr- (xlo 19 $bodzf 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdzflr+ (xlo 19 $bodzfp 16) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdzflrl (xlo 19 $bodzf 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdzflrl- (xlo 19 $bodzf 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bdzflrl+ (xlo 19 $bodzfp 16 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bclr (xllk 19 16) $xlybb-mask ($ppc) $bo $bi)
   #.(ppc-opcode bclrl (xllk 19 16 1) $xlybb-mask ($ppc) $bo $bi)
   #.(ppc-opcode bclr+ (xlylk 19 16 1) $xlybb-mask ($ppc) $boe $bi)
   #.(ppc-opcode bclrl+ (xlylk 19 16 1 1) $xlybb-mask ($ppc) $boe $bi)
   #.(ppc-opcode bclr- (xlylk 19 16 0) $xlybb-mask ($ppc) $boe $bi)
   #.(ppc-opcode bclrl- (xlylk 19 16 1) $xlybb-mask ($ppc) $boe $bi)

   #.(ppc-opcode crnot (xl 19 33) $xl-mask ($ppc) $bt $ba $bba)
   #.(ppc-opcode crnor (xl 19 33) $xl-mask ($ppc) $bt $ba $bb)

   #.(ppc-opcode rfi (xl 19 50) #xffffffff ($ppc) )


   #.(ppc-opcode crandc (xl 19 129) $xl-mask ($ppc) $bt $ba $bb)

   #.(ppc-opcode isync (xl 19 150) #xffffffff ($ppc))

   #.(ppc-opcode crclr (xl 19 193) $xl-mask ($ppc) $bt $bat $bba)
   #.(ppc-opcode crxor (xl 19 193) $xl-mask ($ppc) $bt $ba $bb)

   #.(ppc-opcode crnand (xl 19 225) $xl-mask ($ppc) $bt $ba $bb)

   #.(ppc-opcode crand (xl 19 257) $xl-mask ($ppc) $bt $ba $bb)

   #.(ppc-opcode crset (xl 19 289) $xl-mask ($ppc) $bt $bat $bba)
   #.(ppc-opcode creqv (xl 19 289) $xl-mask ($ppc) $bt $ba $bb)

   #.(ppc-opcode crorc (xl 19 417) $xl-mask ($ppc) $bt $ba $bb)

   #.(ppc-opcode crmove (xl 19 449) $xl-mask ($ppc) $bt $ba $bba)
   #.(ppc-opcode cror (xl 19 449) $xl-mask ($ppc) $bt $ba $bb)

   #.(ppc-opcode bctr (xlo 19 $bou 528) $xlbobibb-mask ($ppc) )
   #.(ppc-opcode bctrl (xlo 19 $bou 528 1) $xlbobibb-mask ($ppc) )
   #.(ppc-opcode bltctr (xlocb 19 $bot $cblt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltctr- (xlocb 19 $bot $cblt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltctr+ (xlocb 19 $botp $cblt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltctrl (xlocb 19 $bot $cblt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltctrl- (xlocb 19 $bot $cblt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bltctrl+ (xlocb 19 $botp $cblt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtctr (xlocb 19 $bot $cbgt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtctr- (xlocb 19 $bot $cbgt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtctr+ (xlocb 19 $botp $cbgt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtctrl (xlocb 19 $bot $cbgt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtctrl- (xlocb 19 $bot $cbgt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgtctrl+ (xlocb 19 $botp $cbgt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqctr (xlocb 19 $bot $cbeq 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqctr- (xlocb 19 $bot $cbeq 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqctr+ (xlocb 19 $botp $cbeq 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqctrl (xlocb 19 $bot $cbeq 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqctrl- (xlocb 19 $bot $cbeq 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode beqctrl+ (xlocb 19 $botp $cbeq 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsoctr (xlocb 19 $bot $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsoctr- (xlocb 19 $bot $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsoctr+ (xlocb 19 $botp $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsoctrl (xlocb 19 $bot $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsoctrl- (xlocb 19 $bot $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bsoctrl+ (xlocb 19 $botp $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunctr (xlocb 19 $bot $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunctr- (xlocb 19 $bot $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunctr+ (xlocb 19 $botp $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunctrl (xlocb 19 $bot $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunctrl- (xlocb 19 $bot $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bunctrl+ (xlocb 19 $botp $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgectr (xlocb 19 $bof $cblt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgectr- (xlocb 19 $bof $cblt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgectr+ (xlocb 19 $bofp $cblt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgectrl (xlocb 19 $bof $cblt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgectrl- (xlocb 19 $bof $cblt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bgectrl+ (xlocb 19 $bofp $cblt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnlctr (xlocb 19 $bof $cblt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnlctr- (xlocb 19 $bof $cblt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnlctr+ (xlocb 19 $bofp $cblt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnlctrl (xlocb 19 $bof $cblt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnlctrl- (xlocb 19 $bof $cblt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnlctrl+ (xlocb 19 $bofp $cblt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blectr (xlocb 19 $bof $cbgt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blectr- (xlocb 19 $bof $cbgt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blectr+ (xlocb 19 $bofp $cbgt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blectrl (xlocb 19 $bof $cbgt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blectrl- (xlocb 19 $bof $cbgt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode blectrl+ (xlocb 19 $bofp $cbgt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bngctr (xlocb 19 $bof $cbgt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bngctr- (xlocb 19 $bof $cbgt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bngctr+ (xlocb 19 $bofp $cbgt 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bngctrl (xlocb 19 $bof $cbgt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bngctrl- (xlocb 19 $bof $cbgt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bngctrl+ (xlocb 19 $bofp $cbgt 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnectr (xlocb 19 $bof $cbeq 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnectr- (xlocb 19 $bof $cbeq 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnectr+ (xlocb 19 $bofp $cbeq 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnectrl (xlocb 19 $bof $cbeq 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnectrl- (xlocb 19 $bof $cbeq 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnectrl+ (xlocb 19 $bofp $cbeq 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnsctr (xlocb 19 $bof $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnsctr- (xlocb 19 $bof $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnsctr+ (xlocb 19 $bofp $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnsctrl (xlocb 19 $bof $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnsctrl- (xlocb 19 $bof $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnsctrl+ (xlocb 19 $bofp $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnuctr (xlocb 19 $bof $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnuctr- (xlocb 19 $bof $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnuctr+ (xlocb 19 $bofp $cbso 528) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnuctrl (xlocb 19 $bof $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnuctrl- (xlocb 19 $bof $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode bnuctrl+ (xlocb 19 $bofp $cbso 528 1) $xlbocbbb-mask ($ppc) $cr)
   #.(ppc-opcode btctr (xlo 19 $bot 528) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btctr- (xlo 19 $bot 528) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btctr+ (xlo 19 $botp 528) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btctrl (xlo 19 $bot 528 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btctrl- (xlo 19 $bot 528 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode btctrl+ (xlo 19 $botp 528 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bfctr (xlo 19 $bof 528) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bfctr- (xlo 19 $bof 528) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bfctr+ (xlo 19 $bofp 528) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bfctrl (xlo 19 $bof 528 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bfctrl- (xlo 19 $bof 528 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bfctrl+ (xlo 19 $bofp 528 1) $xlbobb-mask ($ppc) $bi)
   #.(ppc-opcode bcctr (xllk 19 528) $xlybb-mask ($ppc) $bo $bi)
   #.(ppc-opcode bcctr- (xlylk 19 528 0) $xlybb-mask ($ppc) $boe $bi)
   #.(ppc-opcode bcctr+ (xlylk 19 528 1) $xlybb-mask ($ppc) $boe $bi)
   #.(ppc-opcode bcctrl (xllk 19 528 1) $xlybb-mask ($ppc) $bo $bi)
   #.(ppc-opcode bcctrl- (xlylk 19 528 1) $xlybb-mask ($ppc) $boe $bi)
   #.(ppc-opcode bcctrl+ (xlylk 19 528 1 1) $xlybb-mask ($ppc) $boe $bi)

   #.(ppc-opcode rlwimi (m 20) $m-mask ($ppc) $rta $rs $sh $mb $me)

   #.(ppc-opcode rlwimi. (m 20 1) $m-mask ($ppc) $rta $rs $sh $mb $me)

   #.(ppc-opcode rotlwi (mme 21 31) $mmbme-mask ($ppc) $rta $rs $sh)
   #.(ppc-opcode clrlwi (mme 21 31) $mshme-mask ($ppc) $rta $rs $mb)
   #.(ppc-opcode rlwinm (m 21) $m-mask ($ppc) $rta $rs $sh $mb $me)
   #.(ppc-opcode rotlwi. (mme 21 31 1) $mmbme-mask ($ppc) $rta $rs $sh)
   #.(ppc-opcode clrlwi. (mme 21 31 1) $mshme-mask ($ppc) $rta $rs $mb)
   #.(ppc-opcode rlwinm. (m 21 1) $m-mask ($ppc) $rta $rs $sh $mb $me)

   #.(ppc-opcode rotlw (mme 23 31) $mmbme-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode rlwnm (m 23) $m-mask ($ppc) $rta $rs $rb $mb $me)
   #.(ppc-opcode rotlw. (mme 23 31 1) $mmbme-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode rlwnm. (m 23 1) $m-mask ($ppc) $rta $rs $rb $mb $me)

   #.(ppc-opcode nop (op 24) #xffffffff ($ppc))
   #.(ppc-opcode ori (op 24) $op-mask ($ppc) $rta $rs $ui)

   #.(ppc-opcode oris (op 25) $op-mask ($ppc) $rta $rs $ui)

   #.(ppc-opcode xori (op 26) $op-mask ($ppc) $rta $rs $ui)

   #.(ppc-opcode xoris (op 27) $op-mask ($ppc) $rta $rs $ui)

   #.(ppc-opcode andi. (op 28) $op-mask ($ppc) $rta $rs $ui)

   #.(ppc-opcode andis. (op 29) $op-mask ($ppc) $rta $rs $ui)

   #.(ppc-opcode rotldi (md 30 0 0) $mdmb-mask ($ppc $b64) $rta $rs $sh6)
   #.(ppc-opcode clrldi (md 30 0 0) $mdsh-mask ($ppc $b64) $rta $rs $mb6)
   #.(ppc-opcode rldicl (md 30 0 0) $md-mask ($ppc $b64) $rta $rs $sh6 $mb6)
   #.(ppc-opcode rotldi. (md 30 0 1) $mdmb-mask ($ppc $b64) $rta $rs $sh6)
   #.(ppc-opcode clrldi. (md 30 0 1) $mdsh-mask ($ppc $b64) $rta $rs $mb6)
   #.(ppc-opcode rldicl. (md 30 0 1) $md-mask ($ppc $b64) $rta $rs $sh6 $mb6)

   #.(ppc-opcode rldicr (md 30 1 0) $md-mask ($ppc $b64) $rta $rs $sh6 $me6)
   #.(ppc-opcode rldicr. (md 30 1 1) $md-mask ($ppc $b64) $rta $rs $sh6 $me6)

   #.(ppc-opcode rldic (md 30 2 0) $md-mask ($ppc $b64) $rta $rs $sh6 $mb6)
   #.(ppc-opcode rldic. (md 30 2 1) $md-mask ($ppc $b64) $rta $rs $sh6 $mb6)

   #.(ppc-opcode rldimi (md 30 3 0) $md-mask ($ppc $b64) $rta $rs $sh6 $mb6)
   #.(ppc-opcode rldimi. (md 30 3 1) $md-mask ($ppc $b64) $rta $rs $sh6 $mb6)

   #.(ppc-opcode rotld (mds 30 8 0) $mdsmb-mask ($ppc $b64) $rta $rs $rb)
   #.(ppc-opcode rldcl (mds 30 8 0) $mds-mask ($ppc $b64) $rta $rs $rb $mb6)
   #.(ppc-opcode rotld. (mds 30 8 1) $mdsmb-mask ($ppc $b64) $rta $rs $rb)
   #.(ppc-opcode rldcl. (mds 30 8 1) $mds-mask ($ppc $b64) $rta $rs $rb $mb6)

   #.(ppc-opcode rldcr (mds 30 9 0) $mds-mask ($ppc $b64) $rta $rs $rb $me6)
   #.(ppc-opcode rldcr. (mds 30 9 1) $mds-mask ($ppc $b64) $rta $rs $rb $me6)

   #.(ppc-opcode cmpw (xcmpl 31 0 0) $xcmpl-mask ($ppc) $obf $ra $rb)

   #.(ppc-opcode cmpd (xcmpl 31 0 1) $xcmpl-mask ($ppc $b64) $obf $ra $rb)


   #.(ppc-opcode cmp (x 31 0) $xcmp-mask ($ppc) $bf $l $ra $rb)

   #.(ppc-opcode twlgt (xto 31 4 $tolgt) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twllt (xto 31 4 $tollt) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode tweq (xto 31 4 $toeq) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twlge (xto 31 4 $tolge) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twlnl (xto 31 4 $tolnl) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twlle (xto 31 4 $tolle) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twlng (xto 31 4 $tolng) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twgt (xto 31 4 $togt) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twge (xto 31 4 $toge) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twnl (xto 31 4 $tonl) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twlt (xto 31 4 $tolt) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twle (xto 31 4 $tole) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twng (xto 31 4 $tong) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode twne (xto 31 4 $tone) $xto-mask ($ppc) $ra $rb)
   #.(ppc-opcode trap (xto 31 4 $tou) #xffffffff ($ppc))
   #.(ppc-opcode tw (x 31 4) $x-mask ($ppc) $to $ra $rb)

   #.(ppc-opcode subfc (xo 31 8 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode subc (xo 31 8 0 0) $xo-mask ($ppc) $rt $rb $ra)
   #.(ppc-opcode subfc. (xo 31 8 0 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode subc. (xo 31 8 0 1) $xo-mask ($ppc) $rt $rb $ra)
   #.(ppc-opcode subfco (xo 31 8 1 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode subco (xo 31 8 1 0) $xo-mask ($ppc) $rt $rb $ra)
   #.(ppc-opcode subfco. (xo 31 8 1 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode subco. (xo 31 8 1 1) $xo-mask ($ppc) $rt $rb $ra)


   #.(ppc-opcode mulhdu (xo 31 9 0 0) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode mulhdu. (xo 31 9 0 1) $xo-mask ($ppc $b64) $rt $ra $rb)


   #.(ppc-opcode addc (xo 31 10 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode addc. (xo 31 10 0 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode addco (xo 31 10 1 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode addco. (xo 31 10 1 1) $xo-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode mulhwu (xo 31 11 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode mulhwu. (xo 31 11 0 1) $xo-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode mfcr (x 31 19) $xrarb-mask ($ppc) $rt)

   #.(ppc-opcode lwarx (x 31 20) $x-mask ($ppc) $rt $ra $rb)


   #.(ppc-opcode ldx (x 31 21) $x-mask ($ppc $b64) $rt $ra $rb)


   #.(ppc-opcode lwzx (x 31 23) $x-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode slw (xrc 31 24) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode slw. (xrc 31 24 1) $x-mask ($ppc) $rta $rs $rb)

   #.(ppc-opcode cntlzw (xrc 31 26) $xrb-mask ($ppc) $rta $rs)
   #.(ppc-opcode cntlzw. (xrc 31 26 1) $xrb-mask ($ppc) $rta $rs)


   #.(ppc-opcode sld (xrc 31 27) $x-mask ($ppc $b64) $rta $rs $rb)
   #.(ppc-opcode sld. (xrc 31 27 1) $x-mask ($ppc $b64) $rta $rs $rb)


   #.(ppc-opcode and (xrc 31 28) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode and. (xrc 31 28 1) $x-mask ($ppc) $rta $rs $rb)

   #.(ppc-opcode cmplw (xcmpl 31 32 0) $xcmpl-mask ($ppc) $obf $ra $rb)

   #.(ppc-opcode cmpld (xcmpl 31 32 1) $xcmpl-mask ($ppc $b64) $obf $ra $rb)

   #.(ppc-opcode cmpl (x 31 32) $xcmp-mask ($ppc) $bf $l $ra $rb)

   #.(ppc-opcode subf (xo 31 40 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode sub (xo 31 40 0 0) $xo-mask ($ppc) $rt $rb $ra)
   #.(ppc-opcode subf. (xo 31 40 0 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode sub. (xo 31 40 0 1) $xo-mask ($ppc) $rt $rb $ra)
   #.(ppc-opcode subfo (xo 31 40 1 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode subo (xo 31 40 1 0) $xo-mask ($ppc) $rt $rb $ra)
   #.(ppc-opcode subfo. (xo 31 40 1 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode subo. (xo 31 40 1 1) $xo-mask ($ppc) $rt $rb $ra)


   #.(ppc-opcode ldux (x 31 53) $x-mask ($ppc $b64) $rt $ral $rb)


   #.(ppc-opcode dcbst (x 31 54) $xrt-mask ($ppc) $ra $rb)

   #.(ppc-opcode lwzux (x 31 55) $x-mask ($ppc) $rt $ral $rb)


   #.(ppc-opcode cntlzd (xrc 31 58) $xrb-mask ($ppc $b64) $rta $rs)
   #.(ppc-opcode cntlzd. (xrc 31 58 1) $xrb-mask ($ppc $b64) $rta $rs)


   #.(ppc-opcode andc (xrc 31 60) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode andc. (xrc 31 60 1) $x-mask ($ppc) $rta $rs $rb)


   #.(ppc-opcode tdlgt (xto 31 68 $tolgt) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdllt (xto 31 68 $tollt) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdeq (xto 31 68 $toeq) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdlge (xto 31 68 $tolge) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdlnl (xto 31 68 $tolnl) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdlle (xto 31 68 $tolle) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdlng (xto 31 68 $tolng) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdgt (xto 31 68 $togt) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdge (xto 31 68 $toge) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdnl (xto 31 68 $tonl) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdlt (xto 31 68 $tolt) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdle (xto 31 68 $tole) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdng (xto 31 68 $tong) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode tdne (xto 31 68 $tone) $xto-mask ($ppc $b64) $ra $rb)
   #.(ppc-opcode td (x 31 68) $x-mask ($ppc $b64) $to $ra $rb)

   #.(ppc-opcode mulhd (xo 31 73 0 0) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode mulhd. (xo 31 73 0 1) $xo-mask ($ppc $b64) $rt $ra $rb)


   #.(ppc-opcode mulhw (xo 31 75 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode mulhw. (xo 31 75 0 1) $xo-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode mfmsr (x 31 83) $xrarb-mask ($ppc) $rt)


   #.(ppc-opcode ldarx (x 31 84) $x-mask ($ppc $b64) $rt $ra $rb)


   #.(ppc-opcode dcbf (x 31 86) $xrt-mask ($ppc) $ra $rb)

   #.(ppc-opcode lbzx (x 31 87) $x-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode neg (xo 31 104 0 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode neg. (xo 31 104 0 1) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode nego (xo 31 104 1 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode nego. (xo 31 104 1 1) $xorb-mask ($ppc) $rt $ra)

   #.(ppc-opcode lbzux (x 31 119) $x-mask ($ppc) $rt $ral $rb)

   #.(ppc-opcode not (xrc 31 124) $x-mask ($ppc) $rta $rs $rbs)
   #.(ppc-opcode nor (xrc 31 124) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode not. (xrc 31 124 1) $x-mask ($ppc) $rta $rs $rbs)
   #.(ppc-opcode nor. (xrc 31 124 1) $x-mask ($ppc) $rta $rs $rb)

   #.(ppc-opcode subfe (xo 31 136 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode subfe. (xo 31 136 0 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode subfeo (xo 31 136 1 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode subfeo. (xo 31 136 1 1) $xo-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode adde (xo 31 138 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode adde. (xo 31 138 0 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode addeo (xo 31 138 1 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode addeo. (xo 31 138 1 1) $xo-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode mtcrf (x 31 144) (logior $x-mask (ash 1 20) (ash 1 11)) ($ppc) $fxm $rs)

   #.(ppc-opcode mtmsr (x 31 146) $xrarb-mask ($ppc) $rs)


   #.(ppc-opcode stdx (x 31 149) $x-mask ($ppc $b64) $rs $ra $rb)


   #.(ppc-opcode stwcx. (xrc 31 150 1) $x-mask ($ppc) $rs $ra $rb)

   #.(ppc-opcode stwx (x 31 151) $x-mask ($ppc) $rs $ra $rb)

   #.(ppc-opcode stdux (x 31 181) $x-mask ($ppc $b64) $rs $ras $rb)

   #.(ppc-opcode stwux (x 31 183) $x-mask ($ppc) $rs $ras $rb)

   #.(ppc-opcode subfze (xo 31 200 0 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode subfze. (xo 31 200 0 1) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode subfzeo (xo 31 200 1 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode subfzeo. (xo 31 200 1 1) $xorb-mask ($ppc) $rt $ra)

   #.(ppc-opcode addze (xo 31 202 0 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode addze. (xo 31 202 0 1) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode addzeo (xo 31 202 1 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode addzeo. (xo 31 202 1 1) $xorb-mask ($ppc) $rt $ra)

   #.(ppc-opcode mtsr (x 31 210) (logior $xrb-mask (ash 1 20)) ($ppc $b32) $sr $rs)

   #.(ppc-opcode stdcx. (xrc 31 214 1) $x-mask ($ppc $b64) $rs $ra $rb)

   #.(ppc-opcode stbx (x 31 215) $x-mask ($ppc) $rs $ra $rb)

   #.(ppc-opcode subfme (xo 31 232 0 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode subfme. (xo 31 232 0 1) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode subfmeo (xo 31 232 1 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode subfmeo. (xo 31 232 1 1) $xorb-mask ($ppc) $rt $ra)


   #.(ppc-opcode mulld (xo 31 233 0 0) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode mulld. (xo 31 233 0 1) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode mulldo (xo 31 233 1 0) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode mulldo. (xo 31 233 1 1) $xo-mask ($ppc $b64) $rt $ra $rb)


   #.(ppc-opcode addme (xo 31 234 0 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode addme. (xo 31 234 0 1) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode addmeo (xo 31 234 1 0) $xorb-mask ($ppc) $rt $ra)
   #.(ppc-opcode addmeo. (xo 31 234 1 1) $xorb-mask ($ppc) $rt $ra)

   #.(ppc-opcode mullw (xo 31 235 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode mullw. (xo 31 235 0 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode mullwo (xo 31 235 1 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode mullwo. (xo 31 235 1 1) $xo-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode mtsrin (x 31 242) $xra-mask ($ppc $b32) $rs $rb)

   #.(ppc-opcode dcbtst (x 31 246) $xrt-mask ($ppc) $ra $rb)

   #.(ppc-opcode stbux (x 31 247) $x-mask ($ppc) $rs $ras $rb)

   #.(ppc-opcode add (xo 31 266 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode add. (xo 31 266 0 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode addo (xo 31 266 1 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode addo. (xo 31 266 1 1) $xo-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode dcbt (x 31 278) $xrt-mask ($ppc) $ra $rb)

   #.(ppc-opcode lhzx (x 31 279) $x-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode eqv (xrc 31 284) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode eqv. (xrc 31 284 1) $x-mask ($ppc) $rta $rs $rb)

   #.(ppc-opcode tlbie (x 31 306) $xrtra-mask ($ppc) $rb)

   #.(ppc-opcode eciwx (x 31 310) $x-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode lhzux (x 31 311) $x-mask ($ppc) $rt $ral $rb)

   #.(ppc-opcode xor (xrc 31 316) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode xor. (xrc 31 316 1) $x-mask ($ppc) $rta $rs $rb)

   #.(ppc-opcode mfxer (xspr 31 339 1) $xspr-mask ($ppc) $rt)
   #.(ppc-opcode mflr (xspr 31 339 8) $xspr-mask ($ppc) $rt)
   #.(ppc-opcode mfctr (xspr 31 339 9) $xspr-mask ($ppc) $rt)
   #.(ppc-opcode mfspr (x 31 339) $x-mask ($ppc) $rt $spr)


   #.(ppc-opcode lwax (x 31 341) $x-mask ($ppc $b64) $rt $ra $rb)

   #.(ppc-opcode lhax (x 31 343) $x-mask ($ppc) $rt $ra $rb)


   #.(ppc-opcode tlbia (x 31 370) #xffffffff ($ppc))

   #.(ppc-opcode mftb (x 31 371) $x-mask ($ppc) $rt $tbr)


   #.(ppc-opcode lwaux (x 31 373) $x-mask ($ppc $b64) $rt $ral $rb)

   #.(ppc-opcode lhaux (x 31 375) $x-mask ($ppc) $rt $ral $rb)

   #.(ppc-opcode sthx (x 31 407) $x-mask ($ppc) $rs $ra $rb)
   #.(ppc-opcode orc (xrc 31 412) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode orc. (xrc 31 412 1) $x-mask ($ppc) $rta $rs $rb)

   #.(ppc-opcode sradi (xs 31 413) $xs-mask ($ppc $b64) $rta $rs $sh6)
   #.(ppc-opcode sradi. (xs 31 413 1) $xs-mask ($ppc $b64) $rta $rs $sh6)

   #.(ppc-opcode slbie (x 31 434) $xrtra-mask ($ppc $b64) $rb)


   #.(ppc-opcode ecowx (x 31 438) $x-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode sthux (x 31 439) $x-mask ($ppc) $rs $ras $rb)

   #.(ppc-opcode mr (xrc 31 444) $x-mask ($ppc) $rta $rs $rbs)
   #.(ppc-opcode or (xrc 31 444) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode mr. (xrc 31 444 1) $x-mask ($ppc) $rta $rs $rbs)
   #.(ppc-opcode or. (xrc 31 444 1) $x-mask ($ppc) $rta $rs $rb)


   #.(ppc-opcode divdu (xo 31 457 0 0) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode divdu. (xo 31 457 0 1) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode divduo (xo 31 457 1 0) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode divduo. (xo 31 457 1 1) $xo-mask ($ppc $b64) $rt $ra $rb)


   #.(ppc-opcode divwu (xo 31 459 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode divwu. (xo 31 459 0 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode divwuo (xo 31 459 1 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode divwuo. (xo 31 459 1 1) $xo-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode mtxer (xspr 31 467 1) $xspr-mask ($ppc) $rs)
   #.(ppc-opcode mtlr (xspr 31 467 8) $xspr-mask ($ppc) $rs)
   #.(ppc-opcode mtctr (xspr 31 467 9) $xspr-mask ($ppc) $rs)
   #.(ppc-opcode mtspr (x 31 467) $x-mask ($ppc) $spr $rs)

   #.(ppc-opcode dcbi (x 31 470) $xrt-mask ($ppc) $ra $rb)

   #.(ppc-opcode nand (xrc 31 476) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode nand. (xrc 31 476 1) $x-mask ($ppc) $rta $rs $rb)


   #.(ppc-opcode divd (xo 31 489 0 0) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode divd. (xo 31 489 0 1) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode divdo (xo 31 489 1 0) $xo-mask ($ppc $b64) $rt $ra $rb)
   #.(ppc-opcode divdo. (xo 31 489 1 1) $xo-mask ($ppc $b64) $rt $ra $rb)

   #.(ppc-opcode divw (xo 31 491 0 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode divw. (xo 31 491 0 1) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode divwo (xo 31 491 1 0) $xo-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode divwo. (xo 31 491 1 1) $xo-mask ($ppc) $rt $ra $rb)


   #.(ppc-opcode slbia (x 31 498) #xffffffff ($ppc $b64))



   #.(ppc-opcode mcrxr (x 31 512) (logior $xrarb-mask (ash 3 21)) ($ppc) $bf)

   #.(ppc-opcode lswx (x 31 533) $x-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode lwbrx (x 31 534) $x-mask ($ppc) $rt $ra $rb)

   #.(ppc-opcode lfsx (x 31 535) $x-mask ($ppc) $frt $ra $rb)

   #.(ppc-opcode srw (xrc 31 536) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode srw. (xrc 31 536 1) $x-mask ($ppc) $rta $rs $rb)



   #.(ppc-opcode srd (xrc 31 539) $x-mask ($ppc $b64) $rta $rs $rb)
   #.(ppc-opcode srd. (xrc 31 539 1) $x-mask ($ppc $b64) $rta $rs $rb)


   #.(ppc-opcode tlbsync (x 31 566) #xffffffff ($ppc))

   #.(ppc-opcode lfsux (x 31 567) $x-mask ($ppc) $frt $ras $rb)

   #.(ppc-opcode mfsr (x 31 595) (logior $xrb-mask (ash 1 20)) ($ppc $b32) $rt $sr)

   #.(ppc-opcode lswi (x 31 597) $x-mask ($ppc) $rt $ra $nb)

   #.(ppc-opcode lwsync (xsync 31 598 1) #xffffffff ($ppc))
   #.(ppc-opcode sync (x 31 598) $xsync-mask ($ppc))

   #.(ppc-opcode lfdx (x 31 599) $x-mask ($ppc) $frt $ra $rb)
   #.(ppc-opcode lfdux (x 31 631) $x-mask ($ppc) $frt $ras $rb)

   #.(ppc-opcode mfsrin (x 31 659) $xra-mask ($ppc $b32) $rt $rb)

   #.(ppc-opcode stswx (x 31 661) $x-mask ($ppc) $rs $ra $rb)

   #.(ppc-opcode stwbrx (x 31 662) $x-mask ($ppc) $rs $ra $rb)

   #.(ppc-opcode stfsx (x 31 663) $x-mask ($ppc) $frs $ra $rb)
   #.(ppc-opcode stfsux (x 31 695) $x-mask ($ppc) $frs $ras $rb)
   #.(ppc-opcode stswi (x 31 725) $x-mask ($ppc) $rs $ra $nb)
   #.(ppc-opcode stfdx (x 31 727) $x-mask ($ppc) $frs $ra $rb)
   #.(ppc-opcode stfdux (x 31 759) $x-mask ($ppc) $frs $ras $rb)
   #.(ppc-opcode lhbrx (x 31 790) $x-mask ($ppc) $rt $ra $rb)
   #.(ppc-opcode sraw (xrc 31 792) $x-mask ($ppc) $rta $rs $rb)
   #.(ppc-opcode sraw. (xrc 31 792 1) $x-mask ($ppc) $rta $rs $rb)


   #.(ppc-opcode srad (xrc 31 794) $x-mask ($ppc $b64) $rta $rs $rb)
   #.(ppc-opcode srad. (xrc 31 794 1) $x-mask ($ppc $b64) $rta $rs $rb)


   #.(ppc-opcode srawi (xrc 31 824) $x-mask ($ppc) $rta $rs $sh)
   #.(ppc-opcode srawi. (xrc 31 824 1) $x-mask ($ppc) $rta $rs $sh)

   #.(ppc-opcode eieio (x 31 854) #xffffffff ($ppc))

   #.(ppc-opcode sthbrx (x 31 918) $x-mask ($ppc) $rs $ra $rb)

   #.(ppc-opcode extsh (xrc 31 922) $xrb-mask ($ppc) $rta $rs)
   #.(ppc-opcode extsh. (xrc 31 922 1) $xrb-mask ($ppc) $rta $rs)

   #.(ppc-opcode extsb (xrc 31 954) $xrb-mask ($ppc) $rta $rs)
   #.(ppc-opcode extsb. (xrc 31 954 1) $xrb-mask ($ppc) $rta $rs)

   #.(ppc-opcode icbi (x 31 982) $xrt-mask ($ppc) $ra $rb)

   #.(ppc-opcode stfiwx (x 31 983) $x-mask ($ppc) $frs $ra $rb)

   #.(ppc-opcode extsw (xrc 31 986) $xrb-mask ($ppc) $rta $rs)
   #.(ppc-opcode extsw. (xrc 31 986 1) $xrb-mask ($ppc) $rta $rs)

   #.(ppc-opcode dcbz (x 31 1014) $xrt-mask ($ppc) $ra $rb)
   #.(ppc-opcode dclz (x 31 1014) $xrt-mask ($ppc) $ra $rb)

   #.(ppc-opcode lvebx (x 31 7) $x-mask ($ppc) $vd $ra $rb)
   #.(ppc-opcode lvehx (x 31 39) $x-mask ($ppc) $vd $ra $rb)
   #.(ppc-opcode lvewx (x 31 71) $x-mask ($ppc) $vd $ra $rb)
   #.(ppc-opcode lvsl (x 31 6) $x-mask ($ppc) $vd $ra $rb)
   #.(ppc-opcode lvsr (x 31 38) $x-mask ($ppc) $vd $ra $rb)
   #.(ppc-opcode lvx (x 31 103) $x-mask ($ppc) $vd $ra $rb)
   #.(ppc-opcode lvxl (x 31 359) $x-mask ($ppc) $vd $ra $rb)
   #.(ppc-opcode stvebx (x 31 135) $x-mask ($ppc) $vs $ra $rb)
   #.(ppc-opcode stvehx (x 31 167) $x-mask ($ppc) $vs $ra $rb)
   #.(ppc-opcode stvewx (x 31 199) $x-mask ($ppc) $vs $ra $rb)
   #.(ppc-opcode stvx (x 31 231) $x-mask ($ppc) $vs $ra $rb)
   #.(ppc-opcode stvxl (x 31 487) $x-mask ($ppc) $vs $ra $rb)

   #.(ppc-opcode dss (x 31 822) $x-mask ($ppc) $strm $all/transient)
   #.(ppc-opcode dst (x 31 342) $x-mask ($ppc) $ra $rb $strm $all/transient)
   #.(ppc-opcode dstst (x 31 374) $x-mask ($ppc) $ra $rb $strm $all/transient)
	 
   #.(ppc-opcode lwz (op 32) $op-mask ($ppc) $rt $d $ra)

   #.(ppc-opcode lwzu (op 33) $op-mask ($ppc) $rt $d $ral)

   #.(ppc-opcode lbz (op 34) $op-mask ($ppc) $rt $d $ra)

   #.(ppc-opcode lbzu (op 35) $op-mask ($ppc) $rt $d $ral)

   #.(ppc-opcode stw (op 36) $op-mask ($ppc) $rs $d $ra)

   #.(ppc-opcode stwu (op 37) $op-mask ($ppc) $rs $d $ras)

   #.(ppc-opcode stb (op 38) $op-mask ($ppc) $rs $d $ra)

   #.(ppc-opcode stbu (op 39) $op-mask ($ppc) $rs $d $ras)

   #.(ppc-opcode lhz (op 40) $op-mask ($ppc) $rt $d $ra)

   #.(ppc-opcode lhzu (op 41) $op-mask ($ppc) $rt $d $ral)

   #.(ppc-opcode lha (op 42) $op-mask ($ppc) $rt $d $ra)

   #.(ppc-opcode lhau (op 43) $op-mask ($ppc) $rt $d $ral)

   #.(ppc-opcode sth (op 44) $op-mask ($ppc) $rs $d $ra)

   #.(ppc-opcode sthu (op 45) $op-mask ($ppc) $rs $d $ras)

   #.(ppc-opcode lmw (op 46) $op-mask ($ppc) $rt $d $ram)

   #.(ppc-opcode stmw (op 47) $op-mask ($ppc) $rs $d $ra)

   #.(ppc-opcode lfs (op 48) $op-mask ($ppc) $frt $d $ra)

   #.(ppc-opcode lfsu (op 49) $op-mask ($ppc) $frt $d $ras)

   #.(ppc-opcode lfd (op 50) $op-mask ($ppc) $frt $d $ra)

   #.(ppc-opcode lfdu (op 51) $op-mask ($ppc) $frt $d $ras)

   #.(ppc-opcode stfs (op 52) $op-mask ($ppc) $frs $d $ra)

   #.(ppc-opcode stfsu (op 53) $op-mask ($ppc) $frs $d $ras)

   #.(ppc-opcode stfd (op 54) $op-mask ($ppc) $frs $d $ra)

   #.(ppc-opcode stfdu (op 55) $op-mask ($ppc) $frs $d $ras)




   #.(ppc-opcode ld (dso 58 0) $ds-mask ($ppc $b64) $rt $ds $ra)

   #.(ppc-opcode ldu (dso 58 1) $ds-mask ($ppc $b64) $rt $ds $ral)

   #.(ppc-opcode lwa (dso 58 2) $ds-mask ($ppc $b64) $rt $ds $ra)


   #.(ppc-opcode fdivs (a 59 18 0) $afrc-mask ($ppc) $frt $fra $frb)
   #.(ppc-opcode fdivs. (a 59 18 1) $afrc-mask ($ppc) $frt $fra $frb)

   #.(ppc-opcode fsubs (a 59 20 0) $afrc-mask ($ppc) $frt $fra $frb)
   #.(ppc-opcode fsubs. (a 59 20 1) $afrc-mask ($ppc) $frt $fra $frb)

   #.(ppc-opcode fadds (a 59 21 0) $afrc-mask ($ppc) $frt $fra $frb)
   #.(ppc-opcode fadds. (a 59 21 1) $afrc-mask ($ppc) $frt $fra $frb)

   #.(ppc-opcode fsqrts (a 59 22 0) $afrafrc-mask ($ppc) $frt $frb)
   #.(ppc-opcode fsqrts. (a 59 22 1) $afrafrc-mask ($ppc) $frt $frb)

   #.(ppc-opcode fres (a 59 24 0) $afrafrc-mask ($ppc) $frt $frb)
   #.(ppc-opcode fres. (a 59 24 1) $afrafrc-mask ($ppc) $frt $frb)

   #.(ppc-opcode fmuls (a 59 25 0) $afrb-mask ($ppc) $frt $fra $frc)
   #.(ppc-opcode fmuls. (a 59 25 1) $afrb-mask ($ppc) $frt $fra $frc)

   #.(ppc-opcode fmsubs (a 59 28 0) $a-mask ($ppc) $frt $fra $frc $frb)
   #.(ppc-opcode fmsubs. (a 59 28 1) $a-mask ($ppc) $frt $fra $frc $frb)

   #.(ppc-opcode fmadds (a 59 29 0) $a-mask ($ppc) $frt $fra $frc $frb)
   #.(ppc-opcode fmadds. (a 59 29 1) $a-mask ($ppc) $frt $fra $frc $frb)

   #.(ppc-opcode fnmsubs (a 59 30 0) $a-mask ($ppc) $frt $fra $frc $frb)
   #.(ppc-opcode fnmsubs. (a 59 30 1) $a-mask ($ppc) $frt $fra $frc $frb)

   #.(ppc-opcode fnmadds (a 59 31 0) $a-mask ($ppc) $frt $fra $frc $frb)
   #.(ppc-opcode fnmadds. (a 59 31 1) $a-mask ($ppc) $frt $fra $frc $frb)




   #.(ppc-opcode std (dso 62 0) $ds-mask ($ppc $b64) $rs $ds $ra)

   #.(ppc-opcode stdu (dso 62 1) $ds-mask ($ppc $b64) $rs $ds $ras)


   #.(ppc-opcode fcmpu (x 63 0) (logior $x-mask (ash 3 21)) ($ppc) $bf $fra $frb)

   #.(ppc-opcode frsp (xrc 63 12) $xra-mask ($ppc) $frt $frb)
   #.(ppc-opcode frsp. (xrc 63 12 1) $xra-mask ($ppc) $frt $frb)

   #.(ppc-opcode fctiw (xrc 63 14) $xra-mask ($ppc) $frt $frb)
   #.(ppc-opcode fctiw. (xrc 63 14 1) $xra-mask ($ppc) $frt $frb)
   
   #.(ppc-opcode fctiwz (xrc 63 15) $xra-mask ($ppc) $frt $frb)
   #.(ppc-opcode fctiwz. (xrc 63 15 1) $xra-mask ($ppc) $frt $frb)
   
   #.(ppc-opcode fdiv (a 63 18 0) $afrc-mask ($ppc) $frt $fra $frb)
   #.(ppc-opcode fdiv. (a 63 18 1) $afrc-mask ($ppc) $frt $fra $frb)
   
   #.(ppc-opcode fsub (a 63 20 0) $afrc-mask ($ppc) $frt $fra $frb)
   #.(ppc-opcode fsub. (a 63 20 1) $afrc-mask ($ppc) $frt $fra $frb)
   
   #.(ppc-opcode fadd (a 63 21 0) $afrc-mask ($ppc) $frt $fra $frb)
   #.(ppc-opcode fadd. (a 63 21 1) $afrc-mask ($ppc) $frt $fra $frb)

   #.(ppc-opcode fsqrt (a 63 22 0) $afrafrc-mask ($ppc) $frt $frb)
   #.(ppc-opcode fsqrt. (a 63 22 1) $afrafrc-mask ($ppc) $frt $frb)
   
   #.(ppc-opcode fsel (a 63 23 0) $a-mask ($ppc) $frt $fra $frc $frb)
   #.(ppc-opcode fsel. (a 63 23 1) $a-mask ($ppc) $frt $fra $frc $frb)

   #.(ppc-opcode fmul (a 63 25 0) $afrb-mask ($ppc) $frt $fra $frc)
   #.(ppc-opcode fmul. (a 63 25 1) $afrb-mask ($ppc) $frt $fra $frc)
      
   #.(ppc-opcode fmsub (a 63 28 0) $a-mask ($ppc) $frt $fra $frc $frb)
   #.(ppc-opcode fmsub. (a 63 28 1) $a-mask ($ppc) $frt $fra $frc $frb)

   #.(ppc-opcode fmadd (a 63 29 0) $a-mask ($ppc) $frt $fra $frc $frb)
   #.(ppc-opcode fmadd. (a 63 29 1) $a-mask ($ppc) $frt $fra $frc $frb)

   #.(ppc-opcode fnmsub (a 63 30 0) $a-mask ($ppc) $frt $fra $frc $frb)
   #.(ppc-opcode fnmsub. (a 63 30 1) $a-mask ($ppc) $frt $fra $frc $frb)
   
   #.(ppc-opcode fnmadd (a 63 31 0) $a-mask ($ppc) $frt $fra $frc $frb)
   #.(ppc-opcode fnmadd. (a 63 31 1) $a-mask ($ppc) $frt $fra $frc $frb)
   
   #.(ppc-opcode fcmpo (x 63 32) (logior $x-mask (ash 3 21)) ($ppc) $bf $fra $frb)

   #.(ppc-opcode mtfsb1 (xrc 63 38) $xrarb-mask ($ppc) $bt)
   #.(ppc-opcode mtfsb1. (xrc 63 38 1) $xrarb-mask ($ppc) $bt)

   #.(ppc-opcode fneg (xrc 63 40) $xra-mask ($ppc) $frt $frb)
   #.(ppc-opcode fneg. (xrc 63 40 1) $xra-mask ($ppc) $frt $frb)

   #.(ppc-opcode mcrfs (x 63 64) (logior $xrb-mask (ash 3 21) (ash 3 16)) ($ppc) $bf $bfa)

   #.(ppc-opcode mtfsb0 (xrc 63 70) $xrarb-mask ($ppc) $bt)
   #.(ppc-opcode mtfsb0. (xrc 63 70 1) $xrarb-mask ($ppc) $bt)

   #.(ppc-opcode fmr (xrc 63 72) $xra-mask ($ppc) $frt $frb)
   #.(ppc-opcode fmr. (xrc 63 72 1) $xra-mask ($ppc) $frt $frb)

   #.(ppc-opcode mtfsfi (xrc 63 134) (logior $xra-mask (ash 3 21) (ash 1 11)) ($ppc) $bf $u)
   #.(ppc-opcode mtfsfi. (xrc 63 134 1) (logior $xra-mask (ash 3 21) (ash 1 11)) ($ppc) $bf $u)

   #.(ppc-opcode fnabs (xrc 63 136) $xra-mask ($ppc) $frt $frb)
   #.(ppc-opcode fnabs. (xrc 63 136 1) $xra-mask ($ppc) $frt $frb)

   #.(ppc-opcode fabs (xrc 63 264) $xra-mask ($ppc) $frt $frb)
   #.(ppc-opcode fabs. (xrc 63 264 1) $xra-mask ($ppc) $frt $frb)

   #.(ppc-opcode mffs (xrc 63 583) $xrarb-mask ($ppc) $frt)
   #.(ppc-opcode mffs. (xrc 63 583 1) $xrarb-mask ($ppc) $frt)

   #.(ppc-opcode mtfsf (xfl 63 711) $xfl-mask ($ppc) $flm $frb)
   #.(ppc-opcode mtfsf. (xfl 63 711 1) $xfl-mask ($ppc) $flm $frb)

   #.(ppc-opcode fctid (xrc 63 814) $xra-mask ($ppc $b64) $frt $frb)
   #.(ppc-opcode fctid. (xrc 63 814 1) $xra-mask ($ppc $b64) $frt $frb)

   #.(ppc-opcode fctidz (xrc 63 815) $xra-mask ($ppc $b64) $frt $frb)
   #.(ppc-opcode fctidz. (xrc 63 815 1) $xra-mask ($ppc $b64) $frt $frb)

   #.(ppc-opcode fcfid (xrc 63 846) $xra-mask ($ppc $b64) $frt $frb)
   #.(ppc-opcode fcfid. (xrc 63 846 1) $xra-mask ($ppc $b64) $frt $frb)

))

(defvar *ppc-opcode-indices* (make-array 64 :initial-element -1))
(defvar *ppc-opcode-counts* (make-array 64 :initial-element 0))
(defvar *ppc-opcode-numbers* (make-hash-table :test #'equalp))
(defvar *ppc-instruction-macros* (make-hash-table :test #'equalp))

(defun initialize-ppc-opcode-numbers ()
  (clrhash *ppc-opcode-numbers*)
  (dotimes (i 64) 
    (setf (svref *ppc-opcode-indices* i) -1
          (svref *ppc-opcode-counts* i) 0))
  (dotimes (i (length *ppc-opcodes*))
    (let* ((code (svref *ppc-opcodes* i))
    (opcode (ccl::opcode-opcode code))
    (mask (ccl::opcode-mask code)))
      (setf (gethash (string (ccl::opcode-name code))  *ppc-opcode-numbers*) i)
      (setf (ccl::opcode-op-high code) (ldb (byte 16 16) opcode)
     (ccl::opcode-op-low code) (ldb (byte 16 0) opcode)
     (ccl::opcode-mask-high code) (ldb (byte 16 16) mask)
     (ccl::opcode-mask-low code) (ldb (byte 16 0) mask))
      (setf (ccl::opcode-vinsn-operands code) (ccl::opcode-operands code)
     (ccl::opcode-min-vinsn-args code) (ccl::opcode-min-args code)
     (ccl::opcode-max-vinsn-args code) (ccl::opcode-max-args code))
      (let* ((op (ccl::opcode-majorop code)))
          (if (= -1 (svref *ppc-opcode-indices* op))
            (setf (svref *ppc-opcode-indices* op) i
                  (svref *ppc-opcode-counts* op) 1)
            (incf (svref *ppc-opcode-counts* op))))))
  (when (fboundp 'ccl::fixup-vinsn-templates)   ; not defined yet at bootstrap time
    (ccl::fixup-vinsn-templates (ccl::backend-p2-vinsn-templates ccl::*target-backend*) *ppc-opcode-numbers* ))
  (when (fboundp 'ccl::fixup-ppc-backend)
    (ccl::fixup-ppc-backend)))

(initialize-ppc-opcode-numbers)


(defmacro defppcmacro (name arglist &body body)
  `(setf (ppc-macro-function ',(string name))
         #',(ccl:parse-macro name arglist body)))

(defun ppc-macro-function (name)
  (gethash (string name) *ppc-instruction-macros*))

(defun (setf ppc-macro-function) (new-function name)
  (if (gethash name *ppc-opcode-numbers*)
    (error "~s is already defined as an assembler instruction" name))
  (setf (gethash name *ppc-instruction-macros*) new-function))

(defppcmacro extlwi (ra rs n b)
  `(rlwinm ,ra ,rs ,b 0 (1- ,n)))

(defppcmacro extlwi. (ra rs n b)
  `(rlwinm. ,ra ,rs ,b 0 (1- ,n)))

(defppcmacro extrwi (ra rs n b)
  `(rlwinm ,ra ,rs (+ ,b ,n) (- 32 ,n) 31))

(defppcmacro extrwi. (ra rs n b)
  `(rlwinm. ,ra ,rs (+ ,b ,n) (- 32 ,n) 31))

(defppcmacro inslwi (ra rs n b)
  `(rlwimi ,ra ,rs (- 32 ,b) ,b (1- (+ ,b ,n))))

(defppcmacro inslwi. (ra rs n b)
  `(rlwimi. ,ra ,rs (- 32 ,b) ,b (1- (+ ,b ,n))))

(defppcmacro insrwi (ra rs n b)
  `(rlwimi ,ra ,rs (- 32 (+ ,b ,n)) ,b (1- (+ ,b ,n))))

(defppcmacro insrwi. (ra rs n b)
  `(rlwimi. ,ra ,rs (- 32 (+ ,b ,n)) ,b (1- (+ ,b ,n))))

(defppcmacro rotrwi (ra rs n)
  `(rlwinm ,ra ,rs (- 32 ,n) 0 31))

(defppcmacro rotrwi. (ra rs n)
  `(rlwinm. ,ra ,rs (- 32 ,n) 0 31))

(defppcmacro slwi (ra rs n)
  `(rlwinm ,ra ,rs ,n 0 (- 31 ,n)))

(defppcmacro slwi. (ra rs n)
  `(rlwinm. ,ra ,rs ,n 0 (- 31 ,n)))

(defppcmacro srwi (ra rs n)
  `(rlwinm ,ra ,rs (- 32 ,n) ,n 31))

(defppcmacro srwi. (ra rs n)
  `(rlwinm. ,ra ,rs (- 32 ,n) ,n 31))

(defppcmacro clrrwi (ra rs n)
  `(rlwinm ,ra ,rs 0 0 (- 31 ,n)))

(defppcmacro clrrwi. (ra rs n)
  `(rlwinm. ,ra ,rs 0 0 (- 31 ,n)))

(defppcmacro clrlslwi (ra rs b n)
  `(rlwinm ,ra ,rs ,n (- ,b ,n) (- 31 ,n)))

(defppcmacro clrlslwi. (ra rs b n)
  `(rlwinm. ,ra ,rs ,n (- ,b ,n) (- 31 ,n)))

(defppcmacro extldi (ra rs n b)
  `(rldicr ,ra ,rs ,b ,n))

(defppcmacro extldi. (ra rs n b)
  `(rldicr. ,ra ,rs ,b ,n))

(defppcmacro extrdi (ra rs n b)
  `(rldicl ,ra ,rs (+ ,b ,n) (- 64 ,n)))

(defppcmacro extrdi. (ra rs n b)
  `(rldicl. ,ra ,rs (+ ,b ,n) (- 64 ,n)))

(defppcmacro insrdi (ra rs n b)
  `(rldimi ,ra ,rs (- 64 (+ ,b ,n)) ,b))

(defppcmacro insrdi. (ra rs n b)
  `(rldimi. ,ra ,rs (- 64 (+ ,b ,n)) ,b))

(defppcmacro rotrdi (ra rs n)
  `(rldicl ,ra ,rs (- 64 ,n) 0))

(defppcmacro rotrdi. (ra rs n)
  `(rldicl. ,ra ,rs (- 64 ,n) 0))

(defppcmacro sldi (ra rs n)
  `(rldicr ,ra ,rs ,n (- 63 ,n)))

(defppcmacro sldi. (ra rs n)
  `(rldicr. ,ra ,rs ,n (- 63 ,n)))

(defppcmacro srdi (ra rs n)
  `(rldicl ,ra ,rs (- 64 ,n) ,n))

(defppcmacro srdi. (ra rs n)
  `(rldicl. ,ra ,rs (- 64 ,n) ,n))

(defppcmacro clrrdi (ra rs n)
  `(rldicr ,ra ,rs 0 (- 63 ,n)))

(defppcmacro clrrdi. (ra rs n)
  `(rldicr. ,ra ,rs 0 (- 63 ,n)))

(defppcmacro clrlsldi (ra rs b sh)
  `(rldic ,ra ,rs ,sh (- ,b ,sh)))

(defppcmacro clrlsldi. (ra rs b sh)
  `(rldic. ,ra ,rs ,sh (- ,b ,sh)))


;; Vector unit macros
(defppcmacro dssall ()
  ;;Data stream stop all
  `(dss 0 1))

(defppcmacro dstt (a b strm)
  `(dst ,a ,b ,strm 1))

(defppcmacro dststt (a b strm)
  `(dstst ,a ,b ,strm 1))

(defppcmacro vmr (vd vs)
  ;;Analogous to mr for GP registers. Moves contents of vs to vd
  `(vor ,vd ,vs ,vs))



;; The BA field in an XL form instruction when it must be the same as
;; the BT field in the same instruction.  This operand is marked FAKE.
;; The insertion function just copies the BT field into the BA field,
;; and the extraction function just checks that the fields are the
;; same. 

(defun insert-bat (high low val)
  (declare (ignore val))
  (values  (dpb (ldb (byte 5 (- 21 16)) high) (byte 5 (- 16 16)) high) low))

(defun extract-bat (instr)
  (if (= (ldb (byte 5 21) instr) (ldb (byte 5 16) instr))
    0))

;; The BB field in an XL form instruction when it must be the same as
;; the BA field in the same instruction.  This operand is marked FAKE.
;; The insertion function just copies the BA field into the BB field,
;; and the extraction function just checks that the fields are the
;; same. 

(defun insert-bba (high low val)
  (declare (ignore val))
  (values high (dpb (ldb (byte 5 (- 21 16)) high) (byte 5 11) low)))

(defun extract-bba (instr)
  (if (= (ldb (byte 5 16) instr) (ldb (byte 5 11) instr))
    0))

;; The BD field in a B form instruction.  The lower two bits are
;; forced to zero.

(defun insert-bd (high low val)
  (values high (logior (logand val #xfffc) (logand low 3))))

(defun extract-bd (instr)
  (- (logand instr #xfffc)
     (if (logbitp 15 instr)                ; negative branch displacement
       #x10000
       0)))

;; The BD field in a B form instruction when the - modifier is used.
;; This modifier means that the branch is not expected to be taken.
;; We must set the y bit of the BO field to 1 if the offset is
;; negative.  When extracting, we require that the y bit be 1 and that
;; the offset be positive, since if the y bit is 0 we just want to
;; print the normal form of the instruction. 

(defun insert-bdm (high low val)
  (values
   (if (logbitp 15 val) (logior high (ash 1 (- 21 16))) high)
   (logior (logand val #xfffc) (logand low 3))))

(defun extract-bdm (instr)
  ;; Recognize this if both the "y" (branch predict false) bit
  ;;  is set and the displacement is negative.
  (if (and (logbitp 15 instr)           ; branch disp is negative
           (logbitp 21 instr))          ; prediction inverted
    (extract-bd instr)))                ; return the displacement

;; The BD field in a B form instruction when the + modifier is used.
;; This is like BDM, above, except that the branch is expected to be
;; taken.

(defun insert-bdp (high low val)
  (values
   (if (logbitp 15 val) high (logior high (ash 1 (- 21 16))))
   (logior (logand val #xfffc) (logand low 3))))

(defun extract-bdp (instr)
  ;; Recognize this if both the "y" (branch predict false) bit
  ;;  is set and the displacement is non-negative.
  (if (and (not (logbitp 15 instr))     ; branch disp is non-negative
           (logbitp 21 instr))          ; prediction inverted
    (extract-bd instr)))                ; return the displacement

;; return nil if val isn't a valid bo field i.e. if it has any reserved bits set.
(defun valid-bo (val)
  (and (= val (ldb (byte 5 0) val))
       (case (logand val #x14)
             (4 (not (logbitp 1 val)))
             (#x10 (not (logbitp 3 val)))
             (#x14 (= val #x14))
             (t t))))
 
;; The BO field in a B form instruction.  Fail on attempts to set
;; the field to an illegal value.
(defun insert-bo (high low val)
  (if (valid-bo val)
    (values (dpb val (byte 5 (- 21 16)) high) low)))

(defun extract-bo (instr)
  (let* ((val (ldb (byte 5 21) instr)))
    (and (valid-bo val) val)))

;; The BO field in a B form instruction when the + or - modifier is
;; used.  This is like the BO field, but it must be even.  When
;; extracting it, we force it to be even.

(defun insert-boe (high low val)
  (unless (logbitp 0 val) (insert-bo high low val)))

(defun extract-boe (instr)
  (let* ((val (extract-bo instr)))
    (if val (logandc2 val 1))))

;; The condition register number portion of the BI field in a B form
;; or XL form instruction.  This is used for the extended conditional
;; branch mnemonics, which set the lower two bits of the BI field.  It
;; is the BI field with the lower two bits ignored.

(defun insert-cr (high low val)
  (values (dpb (ash val -2) (byte 3 (- 18 16)) high) low))

(defun extract-cr (instr)
  (logandc2 (ldb (byte 5 16) instr) 3))

(defun insert-bf (high low val)
  (values (dpb (ash val -2) (byte 3 (- 23 16)) high) low))

(defun extract-bf (instr)
  (logandc2 (ldb (byte 5 21) instr) 3))


;; The DS field in a DS form instruction.  This is like D, but the
;; lower two bits are forced to zero.
(defun insert-ds (high low val)
  (when (logtest #b11 val)
    (warn "low two bits of operand #x~8,'0x must be zero - clearing."
	  val))
  (values high (logior low (logand val #xfffc))))

(defun extract-ds (instr)
  (- (logand instr #xfffc) (if (logbitp 15 instr) #x10000 0)))

;; The LI field in an I form instruction.  The lower two bits are
;; forced to zero.

(defun insert-li (high low val)
  (values (dpb (ash val -16) (byte 10 (- 16 16)) high) (logior (logand val #xfffc) (logand low 3))))

(defun extract-li (instr)
  (- (logand instr #x3fffffc) (if (logbitp 25 instr) #x4000000 0)))

;; The MB and ME fields in an M form instruction expressed as a single
;; operand which is itself a bitmask.  The extraction function always
;; marks it as invalid, since we never want to recognize an
;; instruction which uses a field of this type.

#|
(defun insert-mbe (instr val)
  (let* ((uval val)
         (me 31))
    (declare (integer uval)
             (fixnum me))
    (when (/= uval 0)
      (do ()
          ((logbitp 0 uval))
        (setq uval (ash uval -1))
        (decf me))
      (let* ((nbits (logcount uval))
             (mb (- (1+ me) nbits)))
        (declare (fixnum nbits mb))
        (when (= nbits (integer-length uval))
          (dpb me (byte 5 1) (dpb mb (byte 5 6) instr)))))))


(defun extract-mbe (instr)
  (declare (ignore instr)))

;; The MB or ME field in an MD or MDS form instruction.  The high bit
;; is wrapped to the low end.


|#

;; The NB field in an X form instruction.  The value 32 is stored as
;; 0.

(defun insert-nb (high low val)
  (if (<= 0 val 32)
    (values high (dpb val (byte 5 11) low))))

(defun extract-nb (instr)
  (let* ((val (ldb (byte 5 11) instr)))
    (declare (fixnum val))
    (if (= val 0) 32 val)))

;; The NSI field in a D form instruction.  This is the same as the SI
;; field, only negated.  The extraction function always marks it as
;; invalid, since we never want to recognize an instruction which uses
;; a field of this type.
(defun insert-nsi (high low val)
  (declare (ignore low))
  (values high (logand (- val) #xffff)))

(defun extract-nsi (instr)
  (declare (ignore instr)))

;; The RA field in a D or X form instruction which is an updating
;; load, which means that the RA field may not be zero and may not
;; equal the RT field.

(defun insert-ral (high low val)
  (and (/= val 0)
       (/= val (ldb (byte 5 (- 21 16)) high))
       (values (dpb val (byte 5 (- 16 16)) high) low)))

;; The RA field in an lmw instruction, which has special value
;; restrictions.
(defun insert-ram (high low val)
  (if (< val (ldb (byte 5 (- 21 16)) high))
    (values (dpb val (byte 5 (- 16 16)) high) low)))

;; The RA field in a D or X form instruction which is an updating
;; store or an updating floating point load, which means that the RA
;; field may not be zero. 

(defun insert-ras (high low val)
  (unless (= val 0)
    (values (dpb val (byte 5 (- 16 16)) high) low)))
 
;; The RB field in an X form instruction when it must be the same as
;; the RS field in the instruction.  This is used for extended
;; mnemonics like mr.  This operand is marked FAKE.  The insertion
;; function just copies the BT field into the BA field, and the
;; extraction function just checks that the fields are the same.

(defun insert-rbs (high low val)
  (declare (ignore val))
  (values high (dpb (ldb (byte 5 (- 21 16)) high) (byte 5 11) low)))

(defun extract-rbs (instr)
  (if (= (ldb (byte 5 21) instr) (ldb (byte 5 11) instr))
    0))

;; The SH field in an MD form instruction.  This is split.
(defun insert-sh6 (high low val)
  (values high
          (dpb (ldb (byte 5 0) val) (byte 5 11)
               (dpb (ldb (byte 1 5) val) (byte 1 1) low))))

(defun extract-sh6 (instr)
  (logior (ldb (byte 5 11) instr) (ash (ldb (byte 1 1) instr) 5)))


(defun insert-mb6 (high low val)
  (values high
          (dpb (ldb (byte 1 5) val)
               (byte 1 5)
               (dpb val (byte 5 6) low))))

(defun extract-mb6 (instr)
  (dpb (ldb (byte 1 5) instr)
       (byte 1 5)
       (ldb (byte 5 6) instr)))


;; The SPR or TBR field in an XFX form instruction.  This is
;; flipped--the lower 5 bits are stored in the upper 5 and vice-
;; versa.
(defun insert-spr (high low val)
  (values (dpb val (byte 5 (- 16 16)) high)
          (logior low (ash (logand val #x3e0) 6))))


(defun extract-spr (instr)
  (logior (ldb (byte 5 16) instr) (logand #x3e0 (ash instr -6))))

(defun insert-default (operand high low val)
  (let* ((width (ccl::operand-width operand))
         (offset (ccl::operand-offset operand))
         (msbit (1- (+ width offset))))
    (declare (fixnum width offset msbit))
    (if (>= offset 16)
      (values (dpb val (byte width (- offset 16)) high) low)
      (if (< msbit 16)
        (values high (dpb val (byte width offset) low))
        (let* ((lowbits (- 16 offset)))
          (values
           (dpb (the fixnum (ash val (the fixnum (- lowbits))))
                (byte  (the fixnum (- width lowbits)) 0) 
                high)
           (dpb val (byte lowbits offset) low)))))))


(defun extract-default (operand instr)
  (let* ((width (ccl::operand-width operand))
           (op (ldb (byte width (ccl::operand-offset operand)) instr)))
    (if (and (logbitp $ppc-operand-signed (ccl::operand-flags operand))
                (logbitp (1- width) op))
         (- op (ash 1 width))
       op)))




(defun ccl::lookup-ppc-opcode (name)
  (gethash (string name) ppc::*ppc-opcode-numbers*))

(provide "PPC-ASM")
