;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005-2009 Clozure Associates and contributors.
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

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "NXENV")
  (require "DLL-NODE")
  (require "X86-ASM")
  (require "X86-LAP"))

(defstruct (x86-disassembled-instruction (:include dll-node)
                                         (:conc-name x86-di-))
  address
  labeled
  prefixes                              ;explicit prefixes
  mnemonic
  op0
  op1
  op2
  start					;start of instruction in code-vector
  end					;and its end
  )

(defmethod print-object ((xdi x86-disassembled-instruction) stream)
  (print-unreadable-object (xdi stream :type t :identity t)
    (dolist (p (x86-di-prefixes xdi))
      (format stream "(~a) " p))
    (format stream "(~a" (x86-di-mnemonic xdi))
    (let* ((op0 (x86-di-op0 xdi))
	   (op1 (x86-di-op1 xdi))
	   (op2 (x86-di-op2 xdi))
	   (ds (make-x86-disassembly-state :mode-64 #+x8664-target t
					            #+x8632-target nil
					   :code-vector nil
					   :code-pointer 0)))
      (when op0
	(write-x86-lap-operand stream op0 ds)
	(when op1
	  (write-x86-lap-operand stream op1 ds)
	  (when op2
	    (write-x86-lap-operand stream op2 ds)))))
    (format stream ")")))

(defstruct (x86-disassembly-state (:conc-name x86-ds-))
  (mode-64 t)
  (prefixes 0)
  (used-prefixes 0)
  (rex 0)
  (rex-used 0)
  (need-modrm nil)
  (mod 0)
  (reg 0)
  (rm 0)
  (blocks (make-dll-header))
  (insn-start 0)                        ; offset of first prefix byte
  (opcode-start 0)                      ; offset of first opcode byte
  code-vector
  code-pointer
  code-limit
  constants-vector
  pending-labels
  (entry-point 0)
  current-instruction
  (string-buffer (make-array 16 :element-type 'character
                             :fill-pointer 0
                             :adjustable t))
  (symbolic-names ())
)

(defun badop (ds)
  (setf (x86-ds-code-pointer ds) (1+ (x86-ds-opcode-start ds)))
  ;;; Do more here.
  )

(defun x86-ds-peek-u8 (ds)
  (aref (x86-ds-code-vector ds) (x86-ds-code-pointer ds)))

(defun x86-ds-skip (ds &optional (n 1))
  (incf (x86-ds-code-pointer ds) n))

(defun x86-ds-next-u8 (ds)
  (let* ((idx (x86-ds-code-pointer ds)))
    (incf (x86-ds-code-pointer ds))
    (aref (x86-ds-code-vector ds) idx)))

(defun x86-ds-next-s8 (ds)
  (let* ((u8 (x86-ds-next-u8 ds)))
    (if (logbitp 7 u8)
      (- u8 #x100)
      u8)))

(defun x86-ds-next-u16 (ds)
  (let* ((low (x86-ds-next-u8 ds))
         (high (x86-ds-next-u8 ds)))
    (declare (type (unsigned-byte 8) low high))
    (logior (the fixnum (ash high 8)) low)))

(defun x86-ds-next-s16 (ds)
  (let* ((low (x86-ds-next-u8 ds))
         (high (x86-ds-next-s8 ds)))
    (declare (type (unsigned-byte 8) low)
             (type (signed-byte 8) high))
    (logior (the fixnum (ash high 8)) low)))

(defun x86-ds-next-u32 (ds)
  (let* ((low (x86-ds-next-u16 ds))
         (high (x86-ds-next-u16 ds)))
    (declare (type (unsigned-byte 16) low high))
    (logior (ash high 16) low)))

(defun x86-ds-next-s32 (ds)
  (let* ((low (x86-ds-next-u16 ds))
         (high (x86-ds-next-s16 ds)))
    (declare (type (unsigned-byte 16) low)
             (type (signed-byte 16) high))
    (logior (ash high 16) low)))

(defun x86-ds-next-u64 (ds)
  (let* ((low (x86-ds-next-u32 ds))
         (high (x86-ds-next-u32 ds)))
    (logior (ash high 32) low)))

(defun x86-ds-next-s64 (ds)
  (let* ((low (x86-ds-next-u32 ds))
         (high (x86-ds-next-s32 ds)))
    (logior (ash high 32) low)))

(defun x86-ds-u8-ref (ds idx)
  (aref (x86-ds-code-vector ds) (+ idx (x86-ds-entry-point ds))))

(defun x86-ds-u16-ref (ds idx)
  (logior (x86-ds-u8-ref ds idx)
          (ash (x86-ds-u8-ref ds (1+ idx)) 8)))

(defun x86-ds-u32-ref (ds idx)
  (logior (x86-ds-u16-ref ds idx)
          (ash (x86-ds-u16-ref ds (+ idx 2)) 16)))



(defun used-rex (ds value)
  (if (not (zerop value))
    (setf (x86-ds-rex-used ds)
          (logior (x86-ds-rex-used ds)
                  (if (logtest (x86-ds-rex ds) value)
                    #x40
                    0)))
    (setf (x86-ds-rex-used ds)
          (logior (x86-ds-rex-used ds) #x40))))

(defun used-prefix (ds mask)
  (setf (x86-ds-used-prefixes ds)
        (logior (x86-ds-used-prefixes ds)
                (logand (x86-ds-prefixes ds) mask))))



;;; An x86-disassembly-block is -something- like a basic block in a
;;; compiler flow graph; it ends with an unconditional jump and it's
;;; either the entry node in that graph or it's reachable via a jump
;;; or branch from some other reachable block.  There may, however, be
;;; internal labels that are referenced from within the block's
;;; instructions, from some other block, or both.  Each disassembled
;;; instruction within a block keeps track of its address and whether
;;; or not it's a label (a branch or jump target or a tagged return
;;; address.)  The first instruction in each block is a label; others
;;; (initally) aren't.  Whenever we encounter a branch or jmp
;;; instruction (or a manipulation of a tagged return address, which
;;; is a kind of jmp) and determine the address of the label, we add
;;; that address to the disassembly-state's PENDING-LABELS set.  When
;;; we're through processing the block (having encountered an
;;; unconditional jmp), we remove a pending label from that set.  If
;;; it's within a block that's already been processed, we ensure that
;;; the instruction at that address is marked as a label; otherwise,
;;; we process the new block which starts at that address.
;;; Eventually, this'll terminate with all reachable code having been
;;; processed.  There's embedded data and alignment padding in Clozure CL
;;; x86 functions and this approach means that we won't try to
;;; disassemble any of that; if the compiler generates any unreachable
;;; code, we won't see that, either.

;;; There might be a large number of blocks, in which case
;;; keeping them in a search tree might be a better idea.
(defstruct (x86-dis-block (:include dll-node))
  start-address
  end-address
  (instructions (make-dll-header))
)

;;; Insert the block before the first existing block whose
;;; start address is greater than or equal to this block's
;;; end address.  (Yes, they can be equal; no, there should
;;; never be any overlap.)
(defun insert-x86-block (block blocks)
  (let* ((this-end (x86-dis-block-end-address block)))
    (declare (fixnum this-end))
    (do-dll-nodes (other blocks (append-dll-node block blocks))
      (when (>= (the fixnum (x86-dis-block-start-address other))
                this-end)
        (return (insert-dll-node-before block other))))))

(defun x86-dis-find-label (address blocks)
  (declare (fixnum address))
  (do-dll-nodes (block blocks)
    (when (and (>= address (the fixnum (x86-dis-block-start-address block)))
               (< address (the fixnum (x86-dis-block-end-address block))))
      (let* ((instruction
              (do-dll-nodes (i (x86-dis-block-instructions block))
                (when (= (x86-di-address i) address)
                  (return i)))))
        (unless instruction
          (error "Bug: no instruction at address #x~x" address))
        (return (or (x86-di-labeled instruction)
                    (setf (x86-di-labeled instruction) t)))))))


;;; Flags stored in PREFIXES
(defconstant +PREFIX-REPZ+ 1)
(defconstant +PREFIX-REPNZ+ 2)
(defconstant +PREFIX-LOCK+ 4)
(defconstant +PREFIX-CS+ 8)
(defconstant +PREFIX-SS+ #x10)
(defconstant +PREFIX-DS+ #x20)
(defconstant +PREFIX-ES+ #x40)
(defconstant +PREFIX-FS+ #x80)
(defconstant +PREFIX-GS+ #x100)
(defconstant +PREFIX-DATA+ #x200)
(defconstant +PREFIX-ADDR+ #x400)
(defconstant +PREFIX-FWAIT+ #x800)



                              
(defstruct (x86-dis (:constructor %make-x86-dis))
  mnemonic                              ; may be nil
  flags                                 ; extra info
  op1                                   ; function to obtain 1st operand
  bytemode1                             ; flags associated with operand1
  op2                                   ; function for second operand
  bytemode2                             ; flags for operand2
  op3                                   ; function,
  bytemode3                             ; flags for operand3
  )

(defconstant +SUFFIX-ALWAYS+ 4)
(defconstant +AFLAG+ 2)
(defconstant +DFLAG+ 1)

(defconstant +b-mode+ 1)                ; byte operand
(defconstant +v-mode+ 2)                ; operand size depends on prefixes
(defconstant +w-mode+ 3)                ; word operand
(defconstant +d-mode+ 4)                ; double word operand
(defconstant +q-mode+ 5)                ; quad word operand
(defconstant +t-mode+ 6)                ; ten-byte operand
(defconstant +x-mode+ 7)                ; 16-byte XMM operand
(defconstant +m-mode+ 8)                ; d-mode in 32bit, q-mode in 64bit mode.
(defconstant +cond-jump-mode+ 9)
(defconstant +loop-jcxz-mode+ 10)
(defconstant +dq-mode+ 11)              ; operand size depends on REX prefixes.
(defconstant +dqw-mode+ 12)             ; registers like dq-mode, memory like w-mode.
(defconstant +f-mode+ 13)               ; 4- or 6-byte pointer operand
(defconstant +const-1-mode+ 14)

(defconstant +es-reg+ 100)
(defconstant +cs-reg+ 101)
(defconstant +ss-reg+ 102)
(defconstant +ds-reg+ 103)
(defconstant +fs-reg+ 104)
(defconstant +gs-reg+ 105)

(defconstant +eAX-reg+ 108)
(defconstant +eCX-reg+ 109)
(defconstant +eDX-reg+ 110)
(defconstant +eBX-reg+ 111)
(defconstant +eSP-reg+ 112)
(defconstant +eBP-reg+ 113)
(defconstant +eSI-reg+ 114)
(defconstant +eDI-reg+ 115)

(defconstant +al-reg+ 116)
(defconstant +cl-reg+ 117)
(defconstant +dl-reg+ 118)
(defconstant +bl-reg+ 119)
(defconstant +ah-reg+ 120)
(defconstant +ch-reg+ 121)
(defconstant +dh-reg+ 122)
(defconstant +bh-reg+ 123)

(defconstant +ax-reg+ 124)
(defconstant +cx-reg+ 125)
(defconstant +dx-reg+ 126)
(defconstant +bx-reg+ 127)
(defconstant +sp-reg+ 128)
(defconstant +bp-reg+ 129)
(defconstant +si-reg+ 130)
(defconstant +di-reg+ 131)

(defconstant +rAX-reg+ 132)
(defconstant +rCX-reg+ 133)
(defconstant +rDX-reg+ 134)
(defconstant +rBX-reg+ 135)
(defconstant +rSP-reg+ 136)
(defconstant +rBP-reg+ 137)
(defconstant +rSI-reg+ 138)
(defconstant +rDI-reg+ 139)

(defconstant +indir-dx-reg+ 150)

(defconstant +FLOATCODE+ 1)
(defconstant +USE-GROUPS+ 2)
(defconstant +USE-PREFIX-USER-TABLE+ 3)
(defconstant +X86-64-SPECIAL+ 4)
(defconstant +UUOCODE+ 5)

(defconstant +REX-MODE64+ 8)
(defconstant +REX-EXTX+ 4)
(defconstant +REX-EXTY+ 2)
(defconstant +REX-EXTZ+ 1)

(defparameter *x86-segment-prefix-alist*
  `((,+prefix-cs+ . "cs")
    (,+prefix-ds+ . "ds")
    (,+prefix-ss+ . "ss")
    (,+prefix-es+ . "es")
    (,+prefix-fs+ . "fs")
    (,+prefix-gs+ . "gs")))


(defun segment-register-from-prefixes (ds)
  (let* ((prefixes (x86-ds-prefixes ds)))
    (dolist (pair *x86-segment-prefix-alist*)
      (when (logtest (car pair) prefixes)
        (setf (x86-ds-used-prefixes ds)
              (logior (x86-ds-used-prefixes ds)
                      (car pair)))
        (return (parse-x86-register-operand (cdr pair) :%))))))

(defun x86-dis-make-reg-operand (r)
  (x86::make-x86-register-operand
   :type (logandc2 (x86::reg-entry-reg-type r)
                   (x86::encode-operand-type :baseIndex))
   :entry r))

(defun op-st (ds bytemode sizeflag)
  (declare (ignore ds bytemode sizeflag))
  (parse-x86-register-operand "st" :%))

(defun op-sti (ds bytemode sizeflag)
  (declare (ignore bytemode sizeflag))
  (x86-dis-make-reg-operand (svref x86::*x86-float-regs* (x86-ds-rm ds))))

(defun op-indire (ds bytemode sizeflag)
  (when (and (x86-ds-mode-64 ds)
	     (zerop (x86-ds-prefixes ds)))
    (setf (x86-ds-rex ds) (logior #x48 (x86-ds-rex ds))))
  (op-e ds bytemode sizeflag))


(defun op-e (ds bytemode sizeflag)
  (let* ((add 0)
         (riprel nil))
    (used-rex ds +rex-extz+)
    (if (logtest (x86-ds-rex ds) +rex-extz+)
      (setq add 8))
    (x86-ds-skip ds)                    ;skip MODRM byte
    (cond ((eql (x86-ds-mod ds) 3)      ; EA is just a register
           (cond ((eql bytemode +b-mode+)
                  (used-rex ds 0)
                  ;; This is wrong: if we don't have an REX prefix,
                  ;; we should use the old byte register names
                  ;; (dh, ah, ...) instead of the new ones (bpl, sil ...)
                  ;; That'll matter if Lisp code ever needs to
                  ;; access the #xff00 byte, but that seems unlikely
                  (x86-dis-make-reg-operand (x86::x86-reg8 (+ (x86-ds-rm ds)
                                                              add))))
                 ((eql bytemode +w-mode+)
                  (x86-dis-make-reg-operand (x86::x86-reg16 (+ (x86-ds-rm ds)
                                                              add))))
                 ((eql bytemode +d-mode+)
                  (x86-dis-make-reg-operand (x86::x86-reg32 (+ (x86-ds-rm ds)
                                                              add))))
                 ((eql bytemode +q-mode+)
                  (x86-dis-make-reg-operand (x86::x86-reg64 (+ (x86-ds-rm ds)
                                                              add))))
                 ((eql bytemode +m-mode+)
                  (if (x86-ds-mode-64 ds)
                    (x86-dis-make-reg-operand (x86::x86-reg64 (+ (x86-ds-rm ds)
                                                              add)))
                    (x86-dis-make-reg-operand (x86::x86-reg32 (+ (x86-ds-rm ds)
                                                              add)))))
                 ((or (eql bytemode +v-mode+)
                      (eql bytemode +dq-mode+)
                      (eql bytemode +dqw-mode+))
                  (used-rex ds +rex-mode64+)
                  (used-prefix ds +prefix-data+)
                  (cond ((logtest (x86-ds-rex ds) +rex-mode64+)
                         (x86-dis-make-reg-operand (x86::x86-reg64 (+ (x86-ds-rm ds)
                                                              add))))
                        ((or (logtest sizeflag +dflag+)
                             (not (eql bytemode +v-mode+)))
                         (x86-dis-make-reg-operand (x86::x86-reg32 (+ (x86-ds-rm ds)
                                                              add))))
                        (t
                         (x86-dis-make-reg-operand (x86::x86-reg16 (+ (x86-ds-rm ds)
                                                              add))))))
                 ((eql bytemode 0) nil)
                 (t (error "Disassembly error"))))
          (t                            ; memory operand
           (let* ((disp nil)
                  (base (x86-ds-rm ds))
                  (index nil)
                  (scale nil)
                  (have-base t)
                  (have-sib nil)
                  (memop (x86::make-x86-memory-operand)))
             (setf (x86::x86-memory-operand-seg memop)
                   (segment-register-from-prefixes ds))
             (when (= base 4)
               (setq have-sib t)
               (let* ((sib (x86-ds-next-u8 ds)))
                 (setq index (ldb (byte 3 3) sib))
                 (if (or (x86-ds-mode-64 ds)
                         (not (eql index 4)))
                   (setq scale (ldb (byte 2 6) sib)))
                 (setq base (ldb (byte 3 0) sib))
                 (used-rex ds +rex-exty+)
                 (used-rex ds +rex-extz+)
                 (when (logtest (x86-ds-rex ds) +rex-exty+)
                   (incf index 8))
                 (when (logtest  (x86-ds-rex ds) +rex-extz+)
                   (incf base 8))))
             (case (x86-ds-mod ds)
               (0
                (when (= 5 (logand base 7))
                  (setq have-base nil)
                  (if (and (x86-ds-mode-64 ds) (not have-sib))
                    (setq riprel t))
                  (setq disp (x86-ds-next-s32 ds))))
               (1
                (setq disp (x86-ds-next-s8 ds)))
               (2
                (setq disp (x86-ds-next-s32 ds))))
             (when (or (not (eql (x86-ds-mod ds) 0))
                       (eql 5 (logand base 7)))
               (setf (x86::x86-memory-operand-disp memop)
                     (parse-x86-lap-expression disp))
               (when riprel
                 (setf (x86::x86-memory-operand-base memop)
                       (parse-x86-register-operand "rip" :%))))
             (when (or have-base
                       (and have-sib
                            (or (not (eql index 4))
                                (not (eql scale 0)))))
               (used-rex ds +rex-extz+)
               (if (and (not have-sib)
                        (logtest (x86-ds-rex ds) +rex-extz+))
                 (incf base 8))
               (if have-base
                 (setf (x86::x86-memory-operand-base memop)
                       (if (and (x86-ds-mode-64 ds)
                                (logtest sizeflag +aflag+))
                         (x86-dis-make-reg-operand (x86::x86-reg64 base))
                         (x86-dis-make-reg-operand (x86::x86-reg32 base)))))
               (when have-sib
                 (unless (= index 4)
                   (setf (x86::x86-memory-operand-index memop)
                    (if (and (x86-ds-mode-64 ds)
                             (logtest sizeflag +aflag+))
                      (x86-dis-make-reg-operand (x86::x86-reg64 index))
                      (x86-dis-make-reg-operand (x86::x86-reg32 index)))))
                 (unless scale
                   (setq scale 0))
                 (when (or (not (eql scale 0))
                           (not (eql index 4)))
                   (setf (x86::x86-memory-operand-scale memop) scale))))
             memop)))))


(defun op-g (ds bytemode sizeflag)
  (let* ((add 0)
         (reg (x86-ds-reg ds)))
    (used-rex ds +rex-extx+)
    (if (logtest (x86-ds-rex ds) +rex-extx+)
      (setq add 8))
    (cond ((eql bytemode +b-mode+)
           (used-rex ds 0)
           ;; This is wrong: if we don't have an REX prefix,
           ;; we should use the old byte register names
           ;; (dh, ah, ...) instead of the new ones (bpl, sil ...)
           ;; That'll matter if Lisp code ever needs to
           ;; access the #xff00 byte, but that seems unlikely
           (x86-dis-make-reg-operand (x86::x86-reg8 (+ reg add))))
          ((eql bytemode +w-mode+)
           (x86-dis-make-reg-operand (x86::x86-reg16 (+ reg add))))
          ((eql bytemode +d-mode+)
           (x86-dis-make-reg-operand (x86::x86-reg32 (+ reg add))))
          ((eql bytemode +q-mode+)
           (x86-dis-make-reg-operand (x86::x86-reg64 (+ reg add))))
          ((eql bytemode +m-mode+)
           (if (x86-ds-mode-64 ds)
             (x86-dis-make-reg-operand (x86::x86-reg64 (+ reg add)))
             (x86-dis-make-reg-operand (x86::x86-reg32 (+ reg add)))))
          ((or (eql bytemode +v-mode+)
               (eql bytemode +dq-mode+)
               (eql bytemode +dqw-mode+))
           (used-rex ds +rex-mode64+)
           (used-prefix ds +prefix-data+)
           (cond ((logtest (x86-ds-rex ds) +rex-mode64+)
                  (x86-dis-make-reg-operand (x86::x86-reg64 (+ reg add))))
                 ((or (logtest sizeflag +dflag+)
                      (not (eql bytemode +v-mode+)))
                  (x86-dis-make-reg-operand (x86::x86-reg32 (+ reg add))))
                 (t
                  (x86-dis-make-reg-operand (x86::x86-reg16 (+ reg add))))))
          ((eql bytemode 0) nil)
          (t (error "Disassembly error")))))

(defun op-reg (ds code sizeflag)
  (declare (fixnum code))
  (let* ((add 0))
    (used-rex ds +rex-extz+)
    (if (logtest (x86-ds-rex ds) +rex-extz+)
      (setq add 8))
    (cond ((= code +indir-dx-reg+)
           (x86::make-x86-memory-operand
            :base (parse-x86-register-operand "dx" :%)))
          (t
           (let* ((r (cond ((and (>= code +ax-reg+)
                                 (<= code +di-reg+))
                            (x86::x86-reg16 (+ (- code +ax-reg+) add)))
                           ((= code +es-reg+) (lookup-x86-register "es" :%))
                           ((= code +cs-reg+) (lookup-x86-register "cs" :%))
                           ((= code +ds-reg+) (lookup-x86-register "ds" :%))
                           ((= code +ss-reg+) (lookup-x86-register "ss" :%))
                           ((= code +fs-reg+) (lookup-x86-register "fs" :%))
                           ((= code +gs-reg+) (lookup-x86-register "gs" :%))
                           ((and (>= code +al-reg+)
                                 (<= code +dh-reg+))
                            ;; Again, this is wrong if there's no REX
                            ;; prefix.
                            (used-rex ds 0)
                            (x86::x86-reg8 (+ add (- code +al-reg+))))
                           ((and (>= code +rax-reg+)
                                 (<= code +rdi-reg+)
                                 (or (x86-ds-mode-64 ds)
                                     (progn
                                       (setq code (+ code (- +eax-reg+ +rax-reg+)))
                                       nil)))
                            (x86::x86-reg64 (+ add (- code +rax-reg+))))
                           ((and (>= code +eax-reg+)
                                 (<= code +edi-reg+))
                            (used-rex ds +rex-mode64+)
                            (used-prefix ds +prefix-data+)
                            (if (logtest (x86-ds-rex ds) +rex-mode64+)
                              (x86::x86-reg64 (+ add (- code +eax-reg+)))
                              (if (logtest sizeflag +dflag+)
                                (x86::x86-reg32 (+ add (- code +eax-reg+)))
                                (x86::x86-reg16 (+ add (- code +eax-reg+))))))
                           ((and (>= code +al-reg+)
                                 (<= code +bh-reg+))
                            (x86::x86-reg8 (+ add (- code +al-reg+))))
                           (t (error "Disassembly error: code = ~s" code)))))
             (x86-dis-make-reg-operand r))))))

;;; Like OP-REG, but doesn't deal with extended 64-bit registers.
(defun op-imreg (ds code sizeflag)
  (declare (fixnum code))
  (cond ((= code +indir-dx-reg+)
         (x86::make-x86-memory-operand
          :base (parse-x86-register-operand "dx" :%)))
        (t
         (let* ((r (cond ((and (>= code +ax-reg+)
                               (<= code +di-reg+))
                          (x86::x86-reg16 (- code +ax-reg+)))
                         ((= code +es-reg+) (lookup-x86-register "es" :%))
                         ((= code +cs-reg+) (lookup-x86-register "cs" :%))
                         ((= code +ds-reg+) (lookup-x86-register "ds" :%))
                         ((= code +ss-reg+) (lookup-x86-register "ss" :%))
                         ((= code +fs-reg+) (lookup-x86-register "fs" :%))
                         ((= code +gs-reg+) (lookup-x86-register "gs" :%))
                         ((and (>= code +al-reg+)
                               (<= code +dh-reg+))
                          ;; Again, this is wrong if there's no REX
                          ;; prefix.
                          (used-rex ds 0)
                          (x86::x86-reg8 (- code +al-reg+)))

                         ((and (>= code +eax-reg+)
                                 (<= code +edi-reg+))
                          (used-rex ds +rex-mode64+)
                          (used-prefix ds +prefix-data+)
                          (if (logtest (x86-ds-rex ds) +rex-mode64+)
                            (x86::x86-reg64 (- code +eax-reg+))
                            (if (logtest sizeflag +dflag+)
                              (x86::x86-reg32 (- code +eax-reg+))
                              (x86::x86-reg16 (- code +eax-reg+)))))
                         (t (error "Disassembly error")))))
           (x86-dis-make-reg-operand r)))))

;;; A (possibly unsigned) immediate.
(defun op-i (ds bytemode sizeflag)
  (let* ((mask -1)
         (op (cond ((= bytemode +b-mode+)
                    (setq mask #xff)
                    (x86-ds-next-u8 ds))
                   ((and (= bytemode +q-mode+)
                         (x86-ds-mode-64 ds))
                    (x86-ds-next-s32 ds))
                   ((or (= bytemode +q-mode+)
                        (= bytemode +v-mode+))
                    (used-rex ds +rex-mode64+)
                    (used-prefix ds +prefix-data+)
                    (if (logtest (x86-ds-rex ds) +rex-mode64+)
                      (x86-ds-next-s32 ds)
                      (if (logtest sizeflag +dflag+)
                        (progn
                          (setq mask #xffffffff)
                          (x86-ds-next-u32 ds))
                        (progn
                          (setq mask #xfffff)
                          (x86-ds-next-u16 ds)))))
                   ((= bytemode +w-mode+)
                    (setq mask #xfffff)
                    (x86-ds-next-u16 ds))
                   ((= bytemode +const-1-mode+)
                    nil))))
    (when op
      (setq op (logand op mask))
      (x86::make-x86-immediate-operand :value (parse-x86-lap-expression op)))))

(defun op-i64 (ds bytemode sizeflag)
  (if (not (x86-ds-mode-64 ds))
    (op-i ds bytemode sizeflag)
    (let* ((op (cond ((= bytemode +b-mode+)
                      (x86-ds-next-u8 ds))
                     ((= bytemode +v-mode+)
                      (used-rex ds +rex-mode64+)
                      (used-prefix ds +prefix-data+)
                      (if (logtest (x86-ds-rex ds) +rex-mode64+)
                        (x86-ds-next-u64 ds)
                        (if (logtest sizeflag +dflag+)
                          (x86-ds-next-u32 ds)
                          (x86-ds-next-u16 ds))))
                     ((= bytemode +w-mode+)
                      (x86-ds-next-u16 ds))
                     (t (error "Disassembly error")))))
      (when op
        (x86::make-x86-immediate-operand :value (parse-x86-lap-expression op))))))

(defun op-si (ds bytemode sizeflag)
  (let* ((op
          (cond ((= bytemode +b-mode+)
                 (x86-ds-next-s8 ds))
                ((= bytemode +v-mode+)
                 (used-rex ds +rex-mode64+)
                 (used-prefix ds +prefix-data+)
                 (if (logtest (x86-ds-rex ds) +rex-mode64+)
                   (x86-ds-next-s32 ds)
                   (if (logtest sizeflag +dflag+)
                     (x86-ds-next-s32 ds)
                     (x86-ds-next-s16 ds))))
                ((= bytemode +w-mode+)
                 (x86-ds-next-s16 ds))
                (t (error "Disassembly error")))))
    (x86::make-x86-immediate-operand :value (parse-x86-lap-expression op))))

(defun op-j (ds bytemode sizeflag)
  (let* ((mask -1)
         (disp (cond ((= bytemode +b-mode+)
                      (x86-ds-next-s8 ds))
                     ((= bytemode +v-mode+)
                      (if (logtest sizeflag +dflag+)
                        (x86-ds-next-s32 ds)
                        (progn
                          (setq mask #xffff)
                          (x86-ds-next-u16 ds))))
                     (t (error "Disassembly error"))))
         (label-address (logand (+ (x86-ds-code-pointer ds) disp)
                                mask)))
    (push label-address (x86-ds-pending-labels ds))
    (x86::make-x86-label-operand :label label-address)))

(defun op-seg (ds x y)
  (declare (ignore x y))
  (x86-dis-make-reg-operand (x86::x86-segment-register (x86-ds-reg ds))))

(defun op-dir (ds x sizeflag)
  (declare (ignore x))
  (let* ((offset (if (logtest sizeflag +dflag+)
                   (x86-ds-next-u32 ds)
                   (x86-ds-next-u16 ds)))
         (seg (x86-ds-next-u16 ds)))
    (list (x86::make-x86-immediate-operand :value (parse-x86-lap-expression seg))
          (x86::make-x86-memory-operand :disp (parse-x86-lap-expression offset)))))

(defun op-off (ds x sizeflag)
  (declare (ignore x))
  (x86::make-x86-memory-operand
   :seg (segment-register-from-prefixes ds)
   :disp (parse-x86-lap-expression (cond ((or (x86-ds-mode-64 ds)
                                              (logtest sizeflag +aflag+))
                                          (x86-ds-next-u32 ds))
                                         (t (x86-ds-next-u16 ds))))))


(defun op-off64 (ds bytemode sizeflag)
  (if (not (x86-ds-mode-64 ds))
    (op-off ds bytemode sizeflag)
    (x86::make-x86-memory-operand
     :seg (segment-register-from-prefixes ds)
     :disp (parse-x86-lap-expression (x86-ds-next-u64 ds)))))
       

(defun %ptr-reg (ds code sizeflag)
  (used-prefix ds +prefix-addr+)
  (let* ((idx (- code +eax-reg+))
         (r (if (x86-ds-mode-64 ds)
              (if (not (logtest sizeflag +aflag+))
                (x86::x86-reg32 idx)
                (x86::x86-reg64 idx))
              (if (logtest sizeflag +aflag+)
                (x86::x86-reg32 idx)
                (x86::x86-reg16 idx)))))
    (x86-dis-make-reg-operand r)))

(defun op-esreg (ds code sizeflag)
  (x86::make-x86-memory-operand
   :seg (parse-x86-register-operand "es" :%)
   :base (%ptr-reg ds code sizeflag)))
     
(defun op-dsreg (ds code sizeflag)
  (unless (logtest (x86-ds-prefixes ds)
                   (logior +prefix-cs+
                           +prefix-ds+
                           +prefix-ss+
                           +prefix-es+
                           +prefix-fs+
                           +prefix-gs+))
    (setf (x86-ds-prefixes ds)
          (logior (x86-ds-prefixes ds) +prefix-ds+)))
  (x86::make-x86-memory-operand
   :seg (segment-register-from-prefixes ds)
   :base (%ptr-reg ds code sizeflag)))

;;; Control-register reference.
(defun op-c (ds x sizeflag)
  (declare (ignore x sizeflag))
  (let* ((add (cond ((logtest (x86-ds-rex ds) +rex-extx+)
                     (used-rex ds +rex-extx+)
                     8)
                    ((and (not (x86-ds-mode-64 ds))
                          (logtest (x86-ds-prefixes ds) +prefix-lock+))
                     (setf (x86-ds-used-prefixes ds)
                           (logior (x86-ds-used-prefixes ds) +prefix-lock+))
                     8)
                    (t 0)))
         (regname (format nil "cr~d" (+ (x86-ds-reg ds) add))))
    (parse-x86-register-operand regname :%)))
  
;;; Debug-register reference.
(defun op-d (ds x sizeflag)
  (declare (ignore x sizeflag))
  (used-rex ds +rex-extx+)
  (let* ((add (if (logtest (x86-ds-rex ds) +rex-extx+)
                8
                0))
         (regname (format nil "db~d" (+ (x86-ds-reg ds) add))))
    (parse-x86-register-operand regname :%)))

;;; Test-register.  There are only 8 of them, even on x86-64.
(defun op-t (ds x y)
  (declare (ignore x y))
  (parse-x86-register-operand (format nil "tr~d" (x86-ds-reg ds)) :%))

(defun op-rd (ds bytemode sizeflag)
  (if (= (x86-ds-mod ds) 3)
    (op-e ds bytemode sizeflag)
    (badop ds)))


;;; A data prefix causes a reference to an xmm register instead of
;;; the (default) case of referencing an mmx register.
(defun op-mmx (ds x sizeflag)
  (declare (ignore x sizeflag))
  (let* ((prefixes (x86-ds-prefixes ds)))
    (used-prefix ds +prefix-data+)
    (if (logtest prefixes +prefix-data+)
      (let* ((add (progn (used-rex ds +rex-extx+)
                         (if (logtest (x86-ds-rex ds) +rex-extx+)
                           8
                           0))))
        (x86-dis-make-reg-operand (x86::x86-xmm-register (+ (x86-ds-reg ds) add))))
      (x86-dis-make-reg-operand (x86::x86-mmx-register (x86-ds-reg ds))))))


(defun op-xmm (ds bytemode sizeflag)
  (declare (ignore bytemode sizeflag))
  (used-rex ds +rex-extx+)
  (let* ((add (if (logtest (x86-ds-rex ds) +rex-extx+) 8 0)))
    (x86-dis-make-reg-operand (x86::x86-xmm-register (+ (x86-ds-reg ds) add)))))

(defun op-em (ds bytemode sizeflag)
  (if (not (eql (x86-ds-mod ds) 3))
    (op-e ds bytemode sizeflag)
    (let* ((prefixes (x86-ds-prefixes ds)))
      (x86-ds-skip ds)                  ; skip modrm
      (used-prefix ds +prefix-data+)
      (cond ((logtest prefixes +prefix-data+)
             (used-rex ds +rex-extz+)
             (let* ((add (if (logtest (x86-ds-rex ds) +rex-extz+)
                           8
                           0)))
               (x86-dis-make-reg-operand
                (x86::x86-xmm-register (+ (x86-ds-rm ds) add)))))
            (t
             (x86-dis-make-reg-operand
              (x86::x86-mmx-register (x86-ds-rm ds))))))))

(defun op-ex (ds bytemode sizeflag)
  (if (not (eql (x86-ds-mod ds) 3))
    (op-e ds bytemode sizeflag)
    (let* ((add (if (logtest (x86-ds-rex ds) +rex-extz+) 8 0)))
      (used-rex ds +rex-extz+)
      (x86-ds-skip ds)                  ; skip modrm
      (x86-dis-make-reg-operand (x86::x86-xmm-register (+ (x86-ds-rm ds) add))))))
           
(defun op-ms (ds bytemode sizeflag)
  (if (eql (x86-ds-mod ds) 3)
    (op-em ds bytemode sizeflag)
    (badop ds)))

(defun op-xs (ds bytemode sizeflag)
  (if (eql (x86-ds-mod ds) 3)
    (op-ex ds bytemode sizeflag)
    (badop ds)))

(defun op-m (ds bytemode sizeflag)
  (if (eql (x86-ds-mod ds) 3)
    (badop ds)
    (op-e ds bytemode sizeflag)))

(defun op-0f07 (ds bytemode sizeflag)
  (if (or (not (eql (x86-ds-mod ds) 3))
          (not (eql (x86-ds-rm ds) 0)))
    (badop ds)
    (op-e ds bytemode sizeflag)))

(defun nop-fixup (ds bytemode sizeflag)
  (declare (ignore bytemode sizeflag)
           (ignorable ds))
  #+nothing
  (if (logtest (x86-ds-prefixes ds) +prefix-repz+)
    (break "should be PAUSE")))

;;;             

(defun make-x86-dis (opstring &optional
                             op1-fun
                             (op1-byte 0)
                             op2-fun
                             (op2-byte 0)
                             op3-fun
                             (op3-byte 0))
  (let* ((flags nil))
    (if (consp opstring)
      (setq flags (cdr opstring) opstring (car opstring)))
    (%make-x86-dis :mnemonic opstring
                   :flags flags
                   :op1 op1-fun
                   :bytemode1 op1-byte
                   :op2 op2-fun
                   :bytemode2 op2-byte
                   :op3 op3-fun
                   :bytemode3 op3-byte)))
                         

;;; The root of all evil, unless the first byte of the opcode
;;; is #xf
(defparameter *disx86*
  (vector
   ;; #x00
   (make-x86-dis "addB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "addS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "addB" 'op-g +b-mode+ 'op-e +b-mode+)
   (make-x86-dis "addS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "addB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "addS" 'op-imreg +eax-reg+ 'op-i +v-mode+)
   (make-x86-dis '(("pushT" . "(bad)")) 'op-reg +es-reg+)
   (make-x86-dis '(("popT" . "(bad)")) 'op-reg +es-reg+)
   ;; #x08
   (make-x86-dis "orB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "orS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "orB" 'op-g +b-mode+ 'op-e +b-mode+)
   (make-x86-dis "orS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "orB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "orS" 'op-imreg +eax-reg+ 'op-i +v-mode+)
   (make-x86-dis '(("pushT" . "(bad)")) 'op-reg +cs-reg+)
   (make-x86-dis "(bad)")               ; #x0f extended opcode escape
   ;; #x10
   (make-x86-dis "adcB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "adcS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "adcB" 'op-g +b-mode+ 'op-e +b-mode+)
   (make-x86-dis "adcS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "adcB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "adcS" 'op-imreg +eax-reg+ 'op-i +v-mode+)
   (make-x86-dis '(("pushT" . "(bad)")) 'op-reg +ss-reg+)
   (make-x86-dis '(("popT" . "(bad)")) 'op-reg +ss-reg+)
   ;; #x18
   (make-x86-dis "sbbB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "sbbS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "sbbB" 'op-g +b-mode+ 'op-e +b-mode+)
   (make-x86-dis "sbbS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "sbbB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "sbbS" 'op-imreg +eax-reg+ 'op-i +v-mode+)
   (make-x86-dis '(("pushT" . "(bad)")) 'op-reg +ds-reg+)
   (make-x86-dis '(("popT" . "(bad)")) 'op-reg +ds-reg+)
   ;; #x20
   (make-x86-dis "andB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "andS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "andB" 'op-g +b-mode+ 'op-e +b-mode+)
   (make-x86-dis "andS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "andB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "andS" 'op-imreg +eax-reg+ 'op-i +v-mode+)
   (make-x86-dis "(bad)")               ; SEG ES prefix
   (make-x86-dis '(("daa" . "(bad)")))
   ;; #x28
   (make-x86-dis "subB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "subS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "subB" 'op-g +b-mode+ 'op-e +b-mode+)
   (make-x86-dis "subS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "subB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "subS" 'op-imreg +eax-reg+ 'op-i +v-mode+)
   (make-x86-dis "(bad)")               ; SEG CS prefix
   (make-x86-dis '(("das" . "(bad)")))
   ;; #x30
   (make-x86-dis "xorB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "xorS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "xorB" 'op-g +b-mode+ 'op-e +b-mode+)
   (make-x86-dis "xorS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "xorB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "xorS" 'op-imreg +eax-reg+ 'op-i +v-mode+)
   (make-x86-dis "(bad)")               ; SEG SS prefix
   (make-x86-dis '(("aaa" . "(bad)")))
   ;; #x38
   (make-x86-dis "cmpB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "cmpS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "cmpB" 'op-g +b-mode+ 'op-e +b-mode+)
   (make-x86-dis "cmpS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmpB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "cmpS" 'op-imreg +eax-reg+ 'op-i +v-mode+)
   (make-x86-dis "(bad)")               ; SEG DS prefix
   (make-x86-dis '(("aas" . "(bad)")))
   ;; #x40
   (make-x86-dis '(("incS" . "(bad)")) 'op-reg +eax-reg+)
   (make-x86-dis '(("incS" . "(bad)")) 'op-reg +ecx-reg+)
   (make-x86-dis '(("incS" . "(bad)")) 'op-reg +edx-reg+)
   (make-x86-dis '(("incS" . "(bad)")) 'op-reg +ebx-reg+)
   (make-x86-dis '(("incS" . "(bad)")) 'op-reg +esp-reg+)
   (make-x86-dis '(("incS" . "(bad)")) 'op-reg +ebp-reg+)
   (make-x86-dis '(("incS" . "(bad)")) 'op-reg +esi-reg+)
   (make-x86-dis '(("incS" . "(bad)")) 'op-reg +edi-reg+)
   ;; #x48
   (make-x86-dis '(("decS" . "(bad)")) 'op-reg +eax-reg+)
   (make-x86-dis '(("decS" . "(bad)")) 'op-reg +ecx-reg+)
   (make-x86-dis '(("decS" . "(bad)")) 'op-reg +edx-reg+)
   (make-x86-dis '(("decS" . "(bad)")) 'op-reg +ebx-reg+)
   (make-x86-dis '(("decS" . "(bad)")) 'op-reg +esp-reg+)
   (make-x86-dis '(("decS" . "(bad)")) 'op-reg +ebp-reg+)
   (make-x86-dis '(("decS" . "(bad)")) 'op-reg +esi-reg+)
   (make-x86-dis '(("decS" . "(bad)")) 'op-reg +edi-reg+)
   ;; #x50
   (make-x86-dis "pushT" 'op-reg +rax-reg+)
   (make-x86-dis "pushT" 'op-reg +rcx-reg+)
   (make-x86-dis "pushT" 'op-reg +rdx-reg+)
   (make-x86-dis "pushT" 'op-reg +rbx-reg+)
   (make-x86-dis "pushT" 'op-reg +rsp-reg+)
   (make-x86-dis "pushT" 'op-reg +rbp-reg+)
   (make-x86-dis "pushT" 'op-reg +rsi-reg+)
   (make-x86-dis "pushT" 'op-reg +rdi-reg+)
   ;; #x58
   (make-x86-dis "popT" 'op-reg +rax-reg+)
   (make-x86-dis "popT" 'op-reg +rcx-reg+)
   (make-x86-dis "popT" 'op-reg +rdx-reg+)
   (make-x86-dis "popT" 'op-reg +rbx-reg+)
   (make-x86-dis "popT" 'op-reg +rsp-reg+)
   (make-x86-dis "popT" 'op-reg +rbp-reg+)
   (make-x86-dis "popT" 'op-reg +rsi-reg+)
   (make-x86-dis "popT" 'op-reg +rdi-reg+)
   ;; #x60
   (make-x86-dis '(("pushaP" . "(bad)")))
   (make-x86-dis '(("popaP" . "(bad)")))
   (make-x86-dis '(("boundS" . "(bad)")) 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis nil nil +x86-64-special+)
   (make-x86-dis "(bad)")               ; seg fs
   (make-x86-dis "(bad)")               ; seg gs
   (make-x86-dis "(bad)")               ; op size prefix
   (make-x86-dis "(bad)")               ; adr size prefix
   ;; #x68
   (make-x86-dis "pushT" 'op-i +q-mode+)
   (make-x86-dis "imulS" 'op-g +v-mode+ 'op-e +v-mode+ 'op-i +v-mode+ )
   (make-x86-dis "pushT" 'op-si +b-mode+)
   (make-x86-dis "imulS" 'op-g +v-mode+ 'op-e +v-mode+ 'op-si +b-mode+ )
   (make-x86-dis "insb" 'op-dsreg +esi-reg+ 'op-imreg +indir-dx-reg+)
   (make-x86-dis "insR" 'op-esreg +edi-reg+ 'op-imreg +indir-dx-reg+)
   (make-x86-dis "outsb" 'op-imreg +indir-dx-reg+ 'op-dsreg +esi-reg+)
   (make-x86-dis "outsR" 'op-imreg +indir-dx-reg+ 'op-dsreg +esi-reg+)
   ;; #x70
   (make-x86-dis "joH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jnoH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jbH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jaeH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jeH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jneH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jbeH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jaH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   ;; #x78
   (make-x86-dis "jsH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jnsH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jpH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jnpH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jlH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jgeH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jleH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   (make-x86-dis "jgH" 'op-j +b-mode+ nil +cond-jump-mode+ )
   ;; #x80
   (make-x86-dis nil nil +use-groups+ nil 0)
   (make-x86-dis nil nil +use-groups+ nil 1)
   (make-x86-dis "(bad)")
   (make-x86-dis nil nil +use-groups+ nil 2 )
   (make-x86-dis "testB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "testS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "xchgB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "xchgS" 'op-e +v-mode+ 'op-g +v-mode+)
   ;; #x88
   (make-x86-dis "movB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "movS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "movB" 'op-g +b-mode+ 'op-e +b-mode+)
   (make-x86-dis "movS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "movQ" 'op-e +v-mode+ 'op-seg +w-mode+)
   (make-x86-dis '("leaS" . :lea) 'op-g +v-mode+ 'op-m 0)
   (make-x86-dis "movQ" 'op-seg +w-mode+ 'op-e +v-mode+)
   (make-x86-dis "popU" 'op-e +v-mode+)
   ;; #x90
   (make-x86-dis "nop" 'nop-fixup 0)
   (make-x86-dis "xchgS" 'op-reg +ecx-reg+ 'op-imreg +eax-reg+)
   (make-x86-dis "xchgS" 'op-reg +edx-reg+ 'op-imreg +eax-reg+)
   (make-x86-dis "xchgS" 'op-reg +ebx-reg+ 'op-imreg +eax-reg+)
   (make-x86-dis "xchgS" 'op-reg +esp-reg+ 'op-imreg +eax-reg+)
   (make-x86-dis "xchgS" 'op-reg +ebp-reg+ 'op-imreg +eax-reg+)
   (make-x86-dis "xchgS" 'op-reg +esi-reg+ 'op-imreg +eax-reg+)
   (make-x86-dis "xchgS" 'op-reg +edi-reg+ 'op-imreg +eax-reg+)
   ;; #x98
   (make-x86-dis "cWtR")
   (make-x86-dis "cRtO")
   (make-x86-dis '(("JcallT" . "(bad)")) 'op-dir 0)
   (make-x86-dis "(bad)")               ; fwait
   (make-x86-dis "pushfT")
   (make-x86-dis "popfT")
   ;; "sahf" and "lahf" are unimplemented on some Intel EM64T
   ;; steppings, allegedly because an early AMD64 manual
   ;; accidentally omitted them.  It makes sense to disassemble
   ;; them in 64-bit mode, but it may require some thought
   ;; before using them in compiled code.
   (make-x86-dis "sahf")
   (make-x86-dis "lahf")
   ;; #xa0
   (make-x86-dis "movB" 'op-imreg +al-reg+ 'op-off64 +b-mode+)
   (make-x86-dis "movS" 'op-imreg +eax-reg+ 'op-off64 +v-mode+)
   (make-x86-dis "movB" 'op-off64 +b-mode+  'op-imreg +al-reg+)
   (make-x86-dis "movS" 'op-off64 +v-mode+ 'op-imreg +eax-reg+)
   (make-x86-dis "movsb" 'op-dsreg +esi-reg+ 'op-dsreg +esi-reg+)
   (make-x86-dis "movsR" 'op-esreg +edi-reg+ 'op-dsreg +esi-reg+)
   (make-x86-dis "cmpsb" 'op-dsreg +esi-reg+ 'op-dsreg +esi-reg+)
   (make-x86-dis "cmpsR" 'op-dsreg +esi-reg+ 'op-esreg +edi-reg+)
   ;; #xa8
   (make-x86-dis "testB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "testS" 'op-imreg +eax-reg+ 'op-i +v-mode+)
   (make-x86-dis "stosB" 'op-dsreg +esi-reg+ 'op-imreg +al-reg+)
   (make-x86-dis "stosS" 'op-esreg +edi-reg+ 'op-imreg +eax-reg+)
   (make-x86-dis "lodsB" 'op-imreg +al-reg+ 'op-dsreg +esi-reg+)
   (make-x86-dis "lodsS" 'op-imreg +eax-reg+ 'op-dsreg +esi-reg+)
   (make-x86-dis "scasB" 'op-imreg +al-reg+ 'op-dsreg +esi-reg+)
   (make-x86-dis "scasS" 'op-imreg +eax-reg+ 'op-esreg +edi-reg+)
   ;; #xb0
   (make-x86-dis "movB" 'op-reg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "movB" 'op-reg +cl-reg+ 'op-i +b-mode+)
   (make-x86-dis "movB" 'op-reg +dl-reg+ 'op-i +b-mode+)
   (make-x86-dis "movB" 'op-reg +bl-reg+ 'op-i +b-mode+)
   (make-x86-dis "movB" 'op-reg +ah-reg+ 'op-i +b-mode+)
   (make-x86-dis "movB" 'op-reg +ch-reg+ 'op-i +b-mode+)
   (make-x86-dis "movB" 'op-reg +dh-reg+ 'op-i +b-mode+)
   (make-x86-dis "movB" 'op-reg +bh-reg+ 'op-i +b-mode+)
   ;; #xb8
   (make-x86-dis "movS" 'op-reg +eax-reg+ 'op-i64 +v-mode+)
   (make-x86-dis "movS" 'op-reg +ecx-reg+ 'op-i64 +v-mode+)
   (make-x86-dis "movS" 'op-reg +edx-reg+ 'op-i64 +v-mode+)
   (make-x86-dis "movS" 'op-reg +ebx-reg+ 'op-i64 +v-mode+)
   (make-x86-dis "movS" 'op-reg +esp-reg+ 'op-i64 +v-mode+)
   (make-x86-dis "movS" 'op-reg +ebp-reg+ 'op-i64 +v-mode+)
   (make-x86-dis "movS" 'op-reg +esi-reg+ 'op-i64 +v-mode+)
   (make-x86-dis "movS" 'op-reg +edi-reg+ 'op-i64 +v-mode+)
   ;; #xc0
   (make-x86-dis nil nil +use-groups+ nil 3)
   (make-x86-dis nil nil +use-groups+ nil 4)
   (make-x86-dis '("retT" . :jump) 'op-i +w-mode+)
   (make-x86-dis '("retT" . :jump))
   (make-x86-dis '(("lesS" . "(bad)")) 'op-g +v-mode+ 'op-m +f-mode+)
   (make-x86-dis "ldsS" 'op-g +v-mode+ 'op-m +f-mode+)
   (make-x86-dis "movA" 'op-e +b-mode+ 'op-i +b-mode+)
   (make-x86-dis "movQ" 'op-e +v-mode+ 'op-i +v-mode+)
   ;; #xc8
   (make-x86-dis "enterT" 'op-i +w-mode+ 'op-i +b-mode+)
   (make-x86-dis "leaveT")
   (make-x86-dis "lretP" 'op-i +w-mode+)
   (make-x86-dis "lretP")
   (make-x86-dis "int3")
   (make-x86-dis nil nil +uuocode+)
   (make-x86-dis '(("into" . "(bad)")))
   (make-x86-dis "iretP")
   ;; #xd0
   (make-x86-dis nil nil +use-groups+ nil 5)
   (make-x86-dis nil nil +use-groups+ nil 6)
   (make-x86-dis nil nil +use-groups+ nil 7)
   (make-x86-dis nil nil +use-groups+ nil 8)
   (make-x86-dis '(("aam" . "(bad)")) 'op-si +b-mode+)
   (make-x86-dis '(("aad" . "(bad)")) 'op-si +b-mode+)
   (make-x86-dis "(bad)")
   (make-x86-dis "xlat" 'op-dsreg +ebx-reg+)
   ;; #xd8
   (make-x86-dis nil nil +floatcode+)
   (make-x86-dis nil nil +floatcode+)
   (make-x86-dis nil nil +floatcode+)
   (make-x86-dis nil nil +floatcode+)
   (make-x86-dis nil nil +floatcode+)
   (make-x86-dis nil nil +floatcode+)
   (make-x86-dis nil nil +floatcode+)
   (make-x86-dis nil nil +floatcode+)
   ;; #xe0
   (make-x86-dis "loopneFH" 'op-j +b-mode+ nil +loop-jcxz-mode+ )
   (make-x86-dis "loopeFH" 'op-j +b-mode+ nil +loop-jcxz-mode+ )
   (make-x86-dis "loopFH" 'op-j +b-mode+ nil +loop-jcxz-mode+ )
   (make-x86-dis "jEcxzH" 'op-j +b-mode+ nil +loop-jcxz-mode+ )
   (make-x86-dis "inB" 'op-imreg +al-reg+ 'op-i +b-mode+)
   (make-x86-dis "inS" 'op-imreg +eax-reg+ 'op-i +b-mode+)
   (make-x86-dis "outB" 'op-i +b-mode+ 'op-imreg +al-reg+)
   (make-x86-dis "outS" 'op-i +b-mode+ 'op-imreg +eax-reg+)
   ;; #xe8
   (make-x86-dis '("callT" . :call) 'op-j +v-mode+)
   (make-x86-dis '("jmpT" . :jump) 'op-j +v-mode+)
   (make-x86-dis '(("JjmpT" . "(bad)") . :jump) 'op-dir 0)
   (make-x86-dis '("jmp" . :jump)  'op-j +b-mode+)
   (make-x86-dis "inB" 'op-imreg +al-reg+ 'op-imreg +indir-dx-reg+)
   (make-x86-dis "inS" 'op-imreg +eax-reg+ 'op-imreg +indir-dx-reg+)
   (make-x86-dis "outB" 'op-imreg +indir-dx-reg+ 'op-imreg +al-reg+)
   (make-x86-dis "outS" 'op-imreg +indir-dx-reg+ 'op-imreg +eax-reg+)
   ;; #xf0
   (make-x86-dis "(bad)")               ; lock prefix
   (make-x86-dis "icebp")
   (make-x86-dis "(bad)")               ; repne
   (make-x86-dis "(bad)")               ; repz
   (make-x86-dis "hlt")
   (make-x86-dis "cmc")
   (make-x86-dis nil nil +use-groups+ nil 9)
   (make-x86-dis nil nil +use-groups+ nil 10)
   ;; #xf8
   (make-x86-dis "clc")
   (make-x86-dis "stc")
   (make-x86-dis "cli")
   (make-x86-dis "sti")
   (make-x86-dis "cld")
   (make-x86-dis "std")
   (make-x86-dis nil nil +use-groups+ nil 11)
   (make-x86-dis nil nil +use-groups+ nil 12)
   ))

(defparameter *disx86-twobyte*
  (vector
   ;; #x00
   (make-x86-dis nil nil +use-groups+ nil 13)
   (make-x86-dis nil nil +use-groups+ nil 14)
   (make-x86-dis "larS" 'op-g +v-mode+ 'op-e +w-mode+)
   (make-x86-dis "lslS" 'op-g +v-mode+ 'op-e +w-mode+)
   (make-x86-dis "(bad)")
   (make-x86-dis "syscall")
   (make-x86-dis "clts")
   (make-x86-dis "sysretP")
   ;; #x08
   (make-x86-dis "invd")
   (make-x86-dis "wbinvd")
   (make-x86-dis "(bad)")
   (make-x86-dis "ud2a" 'op-i +b-mode+)
   (make-x86-dis "(bad)")
   (make-x86-dis nil nil +use-groups+ nil 22)
   (make-x86-dis "femms")
   (make-x86-dis "" 'op-mmx 0 'op-em +v-mode+ 'op-3dnowsuffix 0) ; See OP-3DNowSuffix.
   ;; #x10
   (make-x86-dis nil nil +use-prefix-user-table+ nil 8)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 9)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 30)
   (make-x86-dis "movlpX" 'op-ex +v-mode+ 'op-xmm 0 'SIMD-Fixup #\h)
   (make-x86-dis "unpcklpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "unpckhpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 31)
   (make-x86-dis "movhpX" 'op-ex +v-mode+ 'op-xmm 0 'SIMD-Fixup #\l)
   ;; #x18
   (make-x86-dis nil nil +use-groups+ nil 21)
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   ;; #x20
   (make-x86-dis "movL" 'op-rd +m-mode+ 'op-c +m-mode+)
   (make-x86-dis "movL" 'op-rd +m-mode+ 'op-d +m-mode+)
   (make-x86-dis "movL" 'op-c +m-mode+ 'op-rd +m-mode+)
   (make-x86-dis "movL" 'op-d +m-mode+ 'op-rd +m-mode+)
   (make-x86-dis "movL" 'op-rd +d-mode+ 'op-t +d-mode+)
   (make-x86-dis "(bad)")
   (make-x86-dis "movL" 'op-t +d-mode+ 'op-rd +d-mode+)
   (make-x86-dis "(bad)")
   ;; #x28
   (make-x86-dis "movapX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "movapX" 'op-ex +v-mode+ 'op-xmm 0)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 2)
   (make-x86-dis "movntpX" 'op-e +v-mode+ 'op-xmm 0)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 4)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 3)
   (make-x86-dis "ucomisX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "comisX" 'op-xmm 0 'op-ex +v-mode+)
   ;; #x30
   (make-x86-dis "wrmsr")
   (make-x86-dis "rdtsc")
   (make-x86-dis "rdmsr")
   (make-x86-dis "rdpmc")
   (make-x86-dis "sysenter")
   (make-x86-dis "sysexit")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   ;; #x38
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   ;; #x40
   (make-x86-dis "cmovoS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovnoS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovbS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovaeS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmoveS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovneS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovbeS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovaS" 'op-g +v-mode+ 'op-e +v-mode+)
   ;; #x48
   (make-x86-dis "cmovsS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovnsS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovpS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovnpS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovlS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovgeS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovleS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovgS" 'op-g +v-mode+ 'op-e +v-mode+)
   ;; #x50
   (make-x86-dis "movmskpX" 'op-g +dq-mode+ 'op-xs +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 13)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 12)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 11)
   (make-x86-dis "andpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "andnpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "orpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "xorpX" 'op-xmm 0 'op-ex +v-mode+)
   ;; #x58
   (make-x86-dis nil nil +use-prefix-user-table+ nil 0)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 10)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 17)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 16)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 14)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 7)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 5)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 6)
   ;; #x60
   (make-x86-dis "punpcklbw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "punpcklwd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "punpckldq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "packsswb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpgtb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpgtw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpgtd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "packuswb" 'op-mmx 0 'op-em +v-mode+)
   ;; #x68
   (make-x86-dis "punpckhbw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "punpckhwd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "punpckhdq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "packssdw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 26)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 24)
   (make-x86-dis "movd" 'op-mmx 0 'op-e +dq-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 19)
   ;; #x70
   (make-x86-dis nil nil +use-prefix-user-table+ nil 22)
   (make-x86-dis nil nil +use-groups+ nil 17)
   (make-x86-dis nil nil +use-groups+ nil 18)
   (make-x86-dis nil nil +use-groups+ nil 19)
   (make-x86-dis "pcmpeqb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpeqw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpeqd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "emms")
   ;; #x78
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis nil nil +use-prefix-user-table+ nil 28)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 29)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 23)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 20)
   ;; #x80
   (make-x86-dis "joH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jnoH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jbH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jaeH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jeH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jneH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jbeH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jaH" 'op-j +v-mode+ nil +cond-jump-mode+)
   ;; #x88
   (make-x86-dis "jsH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jnsH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jpH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jnpH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jlH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jgeH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jleH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jgH" 'op-j +v-mode+ nil +cond-jump-mode+)
   ;; #x90
   (make-x86-dis "seto" 'op-e +b-mode+)
   (make-x86-dis "setno" 'op-e +b-mode+)
   (make-x86-dis "setb" 'op-e +b-mode+)
   (make-x86-dis "setae" 'op-e +b-mode+)
   (make-x86-dis "sete" 'op-e +b-mode+)
   (make-x86-dis "setne" 'op-e +b-mode+)
   (make-x86-dis "setbe" 'op-e +b-mode+)
   (make-x86-dis "seta" 'op-e +b-mode+)
   ;; #x98
   (make-x86-dis "sets" 'op-e +b-mode+)
   (make-x86-dis "setns" 'op-e +b-mode+)
   (make-x86-dis "setp" 'op-e +b-mode+)
   (make-x86-dis "setnp" 'op-e +b-mode+)
   (make-x86-dis "setl" 'op-e +b-mode+)
   (make-x86-dis "setge" 'op-e +b-mode+)
   (make-x86-dis "setle" 'op-e +b-mode+)
   (make-x86-dis "setg" 'op-e +b-mode+)
   ;; #xa0
   (make-x86-dis "pushT" 'op-reg +fs-reg+)
   (make-x86-dis "popT" 'op-reg +fs-reg+)
   (make-x86-dis "cpuid")
   (make-x86-dis "btS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "shldS" 'op-e +v-mode+ 'op-g +v-mode+ 'op-i +b-mode+)
   (make-x86-dis "shldS" 'op-e +v-mode+ 'op-g +v-mode+ 'op-imreg +cl-reg+)
   (make-x86-dis nil nil +use-groups+ nil 24)
   (make-x86-dis nil nil +use-groups+ nil 23)
   ;; #xa8
   (make-x86-dis "pushT" 'op-reg +gs-reg+)
   (make-x86-dis "popT" 'op-reg +gs-reg+)
   (make-x86-dis "rsm")
   (make-x86-dis "btsS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "shrdS" 'op-e +v-mode+ 'op-g +v-mode+ 'op-i +b-mode+)
   (make-x86-dis "shrdS" 'op-e +v-mode+ 'op-g +v-mode+ 'op-imreg +cl-reg+)
   (make-x86-dis nil nil +use-groups+ nil 20)
   (make-x86-dis "imulS" 'op-g +v-mode+ 'op-e +v-mode+)
   ;; #xb0
   (make-x86-dis "cmpxchgB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "cmpxchgS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "lssS" 'op-g +v-mode+ 'op-m +f-mode+)
   (make-x86-dis "btrS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "lfsS" 'op-g +v-mode+ 'op-m +f-mode+)
   (make-x86-dis "lgsS" 'op-g +v-mode+ 'op-m +f-mode+)
   (make-x86-dis "movzbR" 'op-g +v-mode+ 'op-e +b-mode+)
   (make-x86-dis "movzwR" 'op-g +v-mode+ 'op-e +w-mode+) ; yes there really is movzww !
   ;; #xb8
   (make-x86-dis "(bad)")
   (make-x86-dis "ud2b")
   (make-x86-dis nil nil +use-groups+ nil 15)
   (make-x86-dis "btcS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "bsfS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "bsrS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "movsbR" 'op-g +v-mode+ 'op-e +b-mode+)
   (make-x86-dis "movswR" 'op-g +v-mode+ 'op-e +w-mode+) ; yes there really is movsww !
   ;; #xc0
   (make-x86-dis "xaddB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "xaddS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 1)
   (make-x86-dis "movntiS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "pinsrw" 'op-mmx 0 'op-e +dqw-mode+ 'op-i +b-mode+)
   (make-x86-dis "pextrw" 'op-g +dq-mode+ 'op-ms +v-mode+ 'op-i +b-mode+)
   (make-x86-dis "shufpX" 'op-xmm 0 'op-ex +v-mode+ 'op-i +b-mode+)
   (make-x86-dis nil nil +use-groups+ nil 16)
   ;; #xc8
   (make-x86-dis "bswap" 'op-reg +eax-reg+)
   (make-x86-dis "bswap" 'op-reg +ecx-reg+)
   (make-x86-dis "bswap" 'op-reg +edx-reg+)
   (make-x86-dis "bswap" 'op-reg +ebx-reg+)
   (make-x86-dis "bswap" 'op-reg +esp-reg+)
   (make-x86-dis "bswap" 'op-reg +ebp-reg+)
   (make-x86-dis "bswap" 'op-reg +esi-reg+)
   (make-x86-dis "bswap" 'op-reg +edi-reg+)
   ;; #xd0
   (make-x86-dis nil nil +use-prefix-user-table+ nil 27)
   (make-x86-dis "psrlw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psrld" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psrlq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmullw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 21)
   (make-x86-dis "pmovmskb" 'op-g +dq-mode+ 'op-ms +v-mode+)
   ;; #xd8
   (make-x86-dis "psubusb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubusw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pminub" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pand" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddusb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddusw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmaxub" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pandn" 'op-mmx 0 'op-em +v-mode+)
   ;; #xe0
   (make-x86-dis "pavgb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psraw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psrad" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pavgw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmulhuw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmulhw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 15)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 25)
   ;; #xe8
   (make-x86-dis "psubsb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubsw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pminsw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "por" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddsb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddsw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmaxsw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pxor" 'op-mmx 0 'op-em +v-mode+)
   ;; #xf0
   (make-x86-dis nil nil +use-prefix-user-table+ nil 32)
   (make-x86-dis "psllw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pslld" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psllq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmuludq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmaddwd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psadbw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 18)
   ;; #xf8
   (make-x86-dis "psubb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "(bad)")
   ))

(defparameter *onebyte-has-modrm*
  (make-array 256 :element-type 'bit
              :initial-contents '(
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
  #|       -------------------------------        |#
  #| 00 |# 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0  #| 00 |#
  #| 10 |# 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0  #| 10 |#
  #| 20 |# 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0  #| 20 |#
  #| 30 |# 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0  #| 30 |#
  #| 40 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 40 |#
  #| 50 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 50 |#
  #| 60 |# 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0 0  #| 60 |#
  #| 70 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 70 |#
  #| 80 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 80 |#
  #| 90 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 90 |#
  #| a0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| a0 |#
  #| b0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| b0 |#
  #| c0 |# 1 1 0 0 1 1 1 1 0 0 0 0 0 0 0 0  #| c0 |#
  #| d0 |# 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1  #| d0 |#
  #| e0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| e0 |#
  #| f0 |# 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 1  #| f0 |#
  #|       -------------------------------        |#
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
)))


(defparameter *twobyte-has-modrm*
  (make-array 256 :element-type 'bit
              :initial-contents '(
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
  #|       -------------------------------        |#
  #| 00 |# 1 1 1 1 0 0 0 0 0 0 0 0 0 1 0 1  #| 0f |#
  #| 10 |# 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0  #| 1f |#
  #| 20 |# 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1  #| 2f |#
  #| 30 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 3f |#
  #| 40 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 4f |#
  #| 50 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 5f |#
  #| 60 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 6f |#
  #| 70 |# 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1  #| 7f |#
  #| 80 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 8f |#
  #| 90 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 9f |#
  #| a0 |# 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1  #| af |#
  #| b0 |# 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1  #| bf |#
  #| c0 |# 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0  #| cf |#
  #| d0 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| df |#
  #| e0 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| ef |#
  #| f0 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0  #| ff |#
  #|       -------------------------------        |#
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
)))

(defparameter *twobyte-uses-sse-prefix*
  (make-array 256 :element-type 'bit
              :initial-contents '(
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
  #|       -------------------------------        |#
  #| 00 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 0f |#
  #| 10 |# 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0  #| 1f |#
  #| 20 |# 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0  #| 2f |#
  #| 30 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 3f |#
  #| 40 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 4f |#
  #| 50 |# 0 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1  #| 5f |#
  #| 60 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1  #| 6f |#
  #| 70 |# 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1  #| 7f |#
  #| 80 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 8f |#
  #| 90 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 9f |#
  #| a0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| af |#
  #| b0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| bf |#
  #| c0 |# 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0  #| cf |#
  #| d0 |# 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0  #| df |#
  #| e0 |# 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0  #| ef |#
  #| f0 |# 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0  #| ff |#
  #|       -------------------------------        |#
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
)))



(defparameter *grps*
  (vector
   ;; GRP1b
   (vector
    (make-x86-dis "addA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "orA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "adcA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "sbbA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "andA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "subA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "xorA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "cmpA" 'op-e +b-mode+ 'op-i +b-mode+))
   ;; GRP1S
   (vector
    (make-x86-dis '("addQ" . :addi32) 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "orQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "adcQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "sbbQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "andQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis '("subQ" . :subi32) 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "xorQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "cmpQ" 'op-e +v-mode+ 'op-i +v-mode+))
   ;; GRP1Ss
   (vector
    (make-x86-dis '("addQ" . :addi64) 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "orQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "adcQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "sbbQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "andQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis '("subQ" . :subi64) 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "xorQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "cmpQ" 'op-e +v-mode+ 'op-si +b-mode+))
   ;; GRP2b
   (vector
    (make-x86-dis "rolA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "rorA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "rclA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "rcrA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "shlA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "shrA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarA" 'op-e +b-mode+ 'op-i +b-mode+))
   ;; GRP2S
   (vector
    (make-x86-dis "rolQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "rorQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "rclQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "rcrQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "shlQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "shrQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarQ" 'op-e +v-mode+ 'op-i +b-mode+))
   ;; GRP2b-one
   (vector
    (make-x86-dis "rolA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rorA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rclA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rcrA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "shlA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "shrA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarA" 'op-e +b-mode+ 'op-i +const-1-mode+))
   ;; GRP2S-one
   (vector
    (make-x86-dis "rolQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rorQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rclQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rcrQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "shlQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "shrQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarQ" 'op-e +v-mode+ 'op-i +const-1-mode+))
   ;; GRP2b-cl
   (vector
    (make-x86-dis "rolA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rorA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rclA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rcrA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "shlA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "shrA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarA" 'op-e +b-mode+ 'op-imreg +cl-reg+))
   ;; GRP2S-cl
   (vector
    (make-x86-dis "rolQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rorQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rclQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rcrQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "shlQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "shrQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarQ" 'op-e +v-mode+ 'op-imreg +cl-reg+))
   ;; GRP3b
   (vector
    (make-x86-dis "testA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)" 'op-e +b-mode+)
    (make-x86-dis "notA" 'op-e +b-mode+)
    (make-x86-dis "negA" 'op-e +b-mode+)
    (make-x86-dis "mulA" 'op-e +b-mode+)            ; Don't print the implicit %al register
    (make-x86-dis "imulA" 'op-e +b-mode+)           ; to distinguish these opcodes from other
    (make-x86-dis "divA" 'op-e +b-mode+)            ; mul/imul opcodes. Do the same for div
    (make-x86-dis "idivA" 'op-e +b-mode+)           ; and idiv for consistency.
    )
   ;; GRP3S
   (vector
    (make-x86-dis "testQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "notQ" 'op-e +v-mode+)
    (make-x86-dis "negQ" 'op-e +v-mode+)
    (make-x86-dis "mulQ" 'op-e +v-mode+)            ; Don't print the implicit register.
    (make-x86-dis "imulQ" 'op-e +v-mode+)
    (make-x86-dis "divQ" 'op-e +v-mode+)
    (make-x86-dis "idivQ" 'op-e +v-mode+))
   ;; GRP4
   (vector
    (make-x86-dis "incA" 'op-e +b-mode+)
    (make-x86-dis "decA" 'op-e +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRP5
   (vector
    (make-x86-dis "incQ" 'op-e +v-mode+)
    (make-x86-dis "decQ" 'op-e +v-mode+)
    (make-x86-dis '("callT" . :call) 'op-indire +v-mode+)
    (make-x86-dis '("JcallT" . :call) 'op-indire +f-mode+)
    (make-x86-dis '("jmpT" . :jump) 'op-indire +v-mode+)
    (make-x86-dis '("JjmpT" . :jump) 'op-indire +f-mode+)
    (make-x86-dis "pushU" 'op-e +v-mode+)
    (make-x86-dis "(bad)"))
   ;; GRP6
   (vector
    (make-x86-dis "sldtQ" 'op-e +v-mode+)
    (make-x86-dis "strQ" 'op-e +v-mode+)
    (make-x86-dis "lldt" 'op-e +w-mode+)
    (make-x86-dis "ltr" 'op-e +w-mode+)
    (make-x86-dis "verr" 'op-e +w-mode+)
    (make-x86-dis "verw" 'op-e +w-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRP7
   (vector
    (make-x86-dis "sgdtQ" 'op-m 0)
    (make-x86-dis "sidtQ" 'pni-fixup 0)
    (make-x86-dis '(("lgdtQ" . "lgdt")) 'op-m 0)
    (make-x86-dis '(("lidtQ" . "lidt")) 'op-m 0)
    (make-x86-dis "smswQ" 'op-e +v-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "lmsw" 'op-e +w-mode+)
    (make-x86-dis "invlpg" 'INVLPG-Fixup +w-mode+))
   ;; GRP8
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "btQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "btsQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "btrQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "btcQ" 'op-e +v-mode+ 'op-i +b-mode+))
   ;; GRP9
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "cmpxchg8b" 'op-e +q-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRP10
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "psrlw" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "psraw" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "psllw" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)"))
   ;; GRP11
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "psrld" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "psrad" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "pslld" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)"))
   ;; GRP12
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "psrlq" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "psrldq" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "psllq" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "pslldq" 'op-ms +v-mode+ 'op-i +b-mode+))
   ;; GRP13
   (vector
    (make-x86-dis "fxsave" 'op-e +v-mode+)
    (make-x86-dis "fxrstor" 'op-e +v-mode+)
    (make-x86-dis "ldmxcsr" 'op-e +v-mode+)
    (make-x86-dis "stmxcsr" 'op-e +v-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "lfence" 'OP-0fae 0)
    (make-x86-dis "mfence" 'OP-0fae 0)
    (make-x86-dis "clflush" 'OP-0fae 0))
   ;; GRP14
   (vector
    (make-x86-dis "prefetchnta" 'op-e +v-mode+)
    (make-x86-dis "prefetcht0" 'op-e +v-mode+)
    (make-x86-dis "prefetcht1" 'op-e +v-mode+)
    (make-x86-dis "prefetcht2" 'op-e +v-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRPAMD
   (vector
    (make-x86-dis "prefetch" 'op-e +b-mode+)
    (make-x86-dis "prefetchw" 'op-e +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRPPADLCK1
   (vector
    (make-x86-dis "xstorerng" 'op-0f07 0)
    (make-x86-dis "xcryptecb" 'op-0f07 0)
    (make-x86-dis "xcryptcbc" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "xcryptcfb" 'op-0f07 0)
    (make-x86-dis "xcryptofb" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0))
   ;; GRPPADLCK2
   (vector
    (make-x86-dis "montmul" 'op-0f07 0)
    (make-x86-dis "xsha1" 'op-0f07 0)
    (make-x86-dis "xsha256" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0))))

(defparameter *prefix-user-table*
  (vector
   ;; PREGRP0
   (vector
    (make-x86-dis "addps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP1
   (vector
    (make-x86-dis "" 'op-xmm 0 'op-ex +v-mode+ 'op-simd-suffix 0);; See OP-SIMD-SUFFIX.
    (make-x86-dis "" 'op-xmm 0 'op-ex +v-mode+ 'op-simd-suffix 0)
    (make-x86-dis "" 'op-xmm 0 'op-ex +v-mode+ 'op-simd-suffix 0)
    (make-x86-dis "" 'op-xmm 0 'op-ex +v-mode+ 'op-simd-suffix 0))
   ;; PREGRP2
   (vector
    (make-x86-dis "cvtpi2ps" 'op-xmm 0 'op-em +v-mode+)
    (make-x86-dis "cvtsi2ssY" 'op-xmm 0 'op-e +v-mode+)
    (make-x86-dis "cvtpi2pd" 'op-xmm 0 'op-em +v-mode+)
    (make-x86-dis "cvtsi2sdY" 'op-xmm 0 'op-e +v-mode+))
   ;; PREGRP3
   (vector
    (make-x86-dis "cvtps2pi" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "cvtss2siY" 'op-g +v-mode+ 'op-ex +v-mode+)
    (make-x86-dis "cvtpd2pi" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "cvtsd2siY" 'op-g +v-mode+ 'op-ex +v-mode+))
   ;; PREGRP4
   (vector
    (make-x86-dis "cvttps2pi" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "cvttss2siY" 'op-g +v-mode+ 'op-ex +v-mode+)
    (make-x86-dis "cvttpd2pi" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "cvttsd2siY" 'op-g +v-mode+ 'op-ex +v-mode+))
   ;; PREGRP5
   (vector
    (make-x86-dis "divps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "divss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "divpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "divsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP6
   (vector
    (make-x86-dis "maxps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "maxss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "maxpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "maxsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP7
   (vector
    (make-x86-dis "minps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "minss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "minpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "minsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP8
   (vector
    (make-x86-dis "movups" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movupd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP9
   (vector
    (make-x86-dis "movups" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movss" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movupd" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movsd" 'op-ex +v-mode+ 'op-xmm 0))
   ;; PREGRP10
   (vector
    (make-x86-dis "mulps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "mulss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "mulpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "mulsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP11
   (vector
    (make-x86-dis "rcpps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "rcpss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP12
   (vector
    (make-x86-dis "rsqrtps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "rsqrtss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP13
   (vector
    (make-x86-dis "sqrtps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "sqrtss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "sqrtpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "sqrtsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP14
   (vector
    (make-x86-dis "subps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "subss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "subpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "subsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP15
   (vector
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtdq2pd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvttpd2dq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtpd2dq" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP16
   (vector
    (make-x86-dis "cvtdq2ps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvttps2dq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtps2dq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP17
   (vector
    (make-x86-dis "cvtps2pd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtss2sd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtpd2ps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtsd2ss" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP18
   (vector
    (make-x86-dis "maskmovq" 'op-mmx 0 'op-s +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "maskmovdqu" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP19
   (vector
    (make-x86-dis "movq" 'op-mmx 0 'op-em +v-mode+)
    (make-x86-dis "movdqu" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movdqa" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP20
   (vector
    (make-x86-dis "movq" 'op-em +v-mode+ 'op-mmx 0)
    (make-x86-dis "movdqu" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movdqa" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "(bad)" 'op-ex +v-mode+ 'op-xmm 0))
   ;; PREGRP21
   (vector
    (make-x86-dis "(bad)" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movq2dq" 'op-xmm 0 'op-s +v-mode+)
    (make-x86-dis "movq" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movdq2q" 'op-mmx 0 'op-xs +v-mode+))
   ;; PREGRP22
   (vector
    (make-x86-dis "pshufw" 'op-mmx 0 'op-em +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "pshufhw" 'op-xmm 0 'op-ex +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "pshufd" 'op-xmm 0 'op-ex +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "pshuflw" 'op-xmm 0 'op-ex +v-mode+ 'op-i +b-mode+))
   ;; PREGRP23
   (vector
    (make-x86-dis "movd" 'op-e +dq-mode+ 'op-mmx 0)
    (make-x86-dis "movq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movd" 'op-e +dq-mode+ 'op-xmm 0)
    (make-x86-dis "(bad)" 'op-e +d-mode+ 'op-xmm 0))
   ;; PREGRP24
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "punpckhqdq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP25
   (vector
    (make-x86-dis "movntq" 'op-em +v-mode+ 'op-mmx 0)
    (make-x86-dis "(bad)" 'op-em +v-mode+ 'op-xmm 0)
    (make-x86-dis "movntdq" 'op-em +v-mode+ 'op-xmm 0)
    (make-x86-dis "(bad)" 'op-em +v-mode+ 'op-xmm 0))
   ;; PREGRP26
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "punpcklqdq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP27
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addsubpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addsubps" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP28
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "haddpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "haddps" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP29
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "hsubpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "hsubps" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP30
   (vector
    (make-x86-dis "movlpX" 'op-xmm 0 'op-ex +v-mode+ 'SIMD-Fixup #\h);; really only 2 operands
    (make-x86-dis "movsldup" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movlpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movddup" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP31
   (vector
    (make-x86-dis "movhpX" 'op-xmm 0 'op-ex +v-mode+ 'SIMD-Fixup #\l)
    (make-x86-dis "movshdup" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movhpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP32
   (vector
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "lddqu" 'op-xmm 0 'op-m 0))))

(defparameter *x86-64-table*
    (vector
     (vector
      (make-x86-dis "arpl" 'op-e +w-mode+ 'op-g +w-mode+)
      (make-x86-dis "movslq" 'op-g +v-mode+ 'op-e +d-mode+))))


(defun prefix-name (ds b sizeflag)
  (case b
    (#x40 "rex")
    (#x41 "rexZ")
    (#x42 "rexY")
    (#x43 "rexYZ")
    (#x44 "rexX")
    (#x45 "rexXZ")
    (#x46 "rexYZ")
    (#x47 "rexXYZ")
    (#x48 "rex64")
    (#x49 "rex64Z")
    (#x4a "rex64Y")
    (#x4b "rex64YZ")
    (#x4c "rex64X")
    (#x4d "rex64XZ")
    (#x4e "rex64XY")
    (#x4f "rex64XYZ")
    (#xf3 "repz")
    (#xf2 "repnz")
    (#xf0 "lock")
    (#x2e "cs")
    (#x36 "ss")
    (#x3e "ds")
    (#x26 "es")
    (#x64 "fs")
    (#x65 "gs")
    (#x66 (if (logtest sizeflag +dflag+) "data16" "data32"))
    (#x67 (if (x86-ds-mode-64 ds)
            (if (logtest sizeflag +aflag+) "addr32" "addr64")
            (if (logtest sizeflag +aflag+) "addr16" "addr32")))

    (#x9b "fwait")))

(defun scan-prefixes (ds instruction)
  (setf (x86-ds-prefixes ds) 0
        (x86-ds-used-prefixes ds) 0
        (x86-ds-rex ds) 0
        (x86-ds-rex-used ds) 0)
  (let* ((newrex 0)
         (prefixes 0))
    (declare (fixnum prefixes))
    (do* ((b (x86-ds-peek-u8 ds)
             (progn (x86-ds-skip ds)
                    (x86-ds-peek-u8 ds))))
         ()
      (declare (type (unsigned-byte 8) b))
      (setq newrex 0)
      (cond ((and (>= b #x40)
                  (<= b #x4f))
             (if (x86-ds-mode-64 ds)
               (setq newrex b)
               (return)))
            ((= b #xf3)
             (setq prefixes (logior prefixes +prefix-repz+)))
            ((= b #xf2)
             (setq prefixes (logior prefixes +prefix-repnz+)))
            ((= b #xf0)
             (setq prefixes (logior prefixes +prefix-lock+)))
            ((= b #x2e)
             (setq prefixes (logior prefixes +prefix-cs+)))
            ((= b #x36)
             (setq prefixes (logior prefixes +prefix-ss+)))
            ((= b #x3e)
             (setq prefixes (logior prefixes +prefix-ds+)))
            ((= b #x26)
             (setq prefixes (logior prefixes +prefix-es+)))
            ((= b #x64)
             (setq prefixes (logior prefixes +prefix-fs+)))
            ((= b #x65)
             (setq prefixes (logior prefixes +prefix-gs+)))
            ((= b #x66)
             (setq prefixes (logior prefixes +prefix-data+)))
            ((= b #x67)
             (setq prefixes (logior prefixes +prefix-addr+)))
            ((= b #x9b)
             ;; FWAIT. If there are already some prefixes,
             ;; we've found the opcode.
             (if (= prefixes 0)
               (progn
                 (setq prefixes +prefix-fwait+)
                 (return))
               (setq prefixes (logior prefixes +prefix-fwait+))))
            (t (return)))
      (unless (zerop (x86-ds-rex ds))
        (let* ((prefix-name (prefix-name ds (x86-ds-rex ds) 0)))
          (when prefix-name
            (push prefix-name
                  (x86-di-prefixes instruction)))))
      (setf (x86-ds-rex ds) newrex))
    (setf (x86-ds-prefixes ds) prefixes)))


(defun x86-putop (ds template sizeflag instruction)
  (let* ((ok t))
    (when (consp template)
      (if (x86-ds-mode-64 ds)
      (setq template (cdr template))
      (setq template (car template))))
  (if (dotimes (i (length template) t)
          (unless (lower-case-p (schar template i))
            (return nil)))
      (setf (x86-di-mnemonic instruction) template)
      (let* ((string-buffer (x86-ds-string-buffer ds))
             (mod (x86-ds-mod ds))
             (rex (x86-ds-rex ds))
             (prefixes (x86-ds-prefixes ds))
             (mode64 (x86-ds-mode-64 ds)))
        (declare (fixnum rex prefixes))
        (setf (fill-pointer string-buffer) 0)
        (dotimes (i (length template))
          (let* ((c (schar template i))
                 (b 
                  (case c
                    (#\) (setq ok nil))
                    (#\A (if (or (not (eql mod 3))
                                 (logtest sizeflag +suffix-always+))
                           #\b))
                    (#\B (if (logtest sizeflag +suffix-always+)
                           #\b))
                    (#\C (when (or (logtest prefixes +prefix-data+)
                                   (logtest sizeflag +suffix-always+))
                           (used-prefix ds +prefix-data+)
                           (if (logtest sizeflag +dflag+)
                             #\l
                             #\s)))
                    (#\E (used-prefix ds +prefix-addr+)
                         (if mode64
                           (if (logtest sizeflag +aflag+)
                             #\r
                             #\e)
                           (if (logtest sizeflag +aflag+)
                             #\e)))
                    (#\F (when (or (logtest prefixes +prefix-addr+)
                                   (logtest sizeflag +suffix-always+))
                           (used-prefix ds +prefix-addr+)
                           (if (logtest sizeflag +aflag+)
                             (if mode64 #\q #\l)
                             (if mode64 #\l #\w))))
                    (#\H (let* ((ds-or-cs
                                 (logand prefixes
                                         (logior +prefix-ds+ +prefix-ds+)))
                                (ds-only (= ds-or-cs +prefix-ds+))
                                (cs-only (= ds-or-cs +prefix-cs+)))
                           (when (or ds-only cs-only)
                             (setf (x86-ds-used-prefixes ds)
                                   (logior (x86-ds-used-prefixes ds)
                                           ds-or-cs))
                             (if ds-only ".pt" ".pn"))))
                    (#\J #\l)
                    (#\L (if (logtest sizeflag +suffix-always+) #\l))
                    (#\N (if (logtest prefixes +prefix-fwait+)
                           (setf (x86-ds-used-prefixes ds)
                                 (logior (x86-ds-used-prefixes ds)
                                         +prefix-fwait+))
                           #\n))
                    (#\O (used-rex ds +rex-mode64+)
                         (if (logtest rex +rex-mode64+)
                           #\o
                           #\d))
                    ((#\T #\P)
                     (if (and (eql c #\T) mode64)
                       #\q
                       (when (or (logtest prefixes +prefix-data+)
                                 (logtest rex +rex-mode64+)
                                 (logtest sizeflag +suffix-always+))
                         (used-rex ds +rex-mode64+)
                         (if (logtest rex +rex-mode64+)
                           #\q
                           (progn
                             (used-prefix ds +prefix-data+)
                             (if (logtest sizeflag +dflag+)
                               #\l
                               #\w))))))
                    ((#\U #\Q)
                     (if (and (eql c #\U) mode64)
                       #\q
                       (progn
                         (used-rex ds +rex-mode64+)
                         (when (or (not (eql mod 3))
                                   (logtest sizeflag +suffix-always+))
                           (if (logtest rex +rex-mode64+)
                             #\q
                             (progn
                               (used-prefix ds +prefix-data+)
                               (if (logtest sizeflag +dflag+)
                                 #\l
                                 #\w)))))))
                    (#\R
                     (used-rex ds +rex-mode64+)
                     (if (logtest rex +rex-mode64+)
                       #\q
                       (if (logtest sizeflag +dflag+)
                         #\l
                         #\w)))
                    (#\S
                     (when (logtest sizeflag +suffix-always+)
                       (if (logtest rex +rex-mode64+)
                         #\q
                         (progn
                           (used-prefix ds +prefix-data+)
                           (if (logtest sizeflag +dflag+)
                             #\l
                             #\w)))))
                    (#\X
                     (used-prefix ds +prefix-data+)
                     (if (logtest prefixes +prefix-data+)
                       #\d
                       #\s))
                    (#\Y
                     (when (logtest rex +rex-mode64+)
                       (used-rex ds +rex-mode64+)
                       #\q))
                    (#\W
                     (used-rex ds 0)
                     (if (not (eql rex 0))
                       #\l
                       (progn
                         (used-prefix ds +prefix-data+)
                         (if (logtest sizeflag +dflag+)
                           #\w
                           #\b))))
                    (t c))))
            (if b
              (if (typep b 'character)
                (vector-push-extend b string-buffer)
                (dotimes (i (length b))
                  (vector-push-extend (schar b i) string-buffer))))))
        (setf (x86-di-mnemonic instruction) (subseq string-buffer 0))))
  ok))

(defparameter *x86-dissassemble-always-print-suffix* t)

(defun x86-dis-do-float (ds instruction floatop sizeflag)
  (declare (ignore floatop sizeflag))
  ;; Later; we want to make minimal use of the x87 fpu.
  (setf (x86-di-mnemonic instruction) "x87-fpu-op")
  (x86-ds-skip ds))

(defun x86-dis-do-uuo (ds instruction intop)
  (declare (type (unsigned-byte 8) intop))
  (let* ((stop t)
         (regmask (if (x86-ds-mode-64 ds) #xf #x7)))
    (cond ((and (>= intop #x70) (< intop #x80))
           (let* ((pseudo-modrm-byte (x86-ds-next-u8 ds)))
             (setf (x86-di-mnemonic instruction)
                   "uuo-error-slot-unbound"
                   (x86-di-op0 instruction)
                   (x86-dis-make-reg-operand (lookup-x86-register (logand intop regmask) :%))                     
                   (x86-di-op1 instruction)
                   (x86-dis-make-reg-operand (lookup-x86-register (ldb (byte 4 4)
                                                                       pseudo-modrm-byte) :%))
                   (x86-di-op2 instruction)
                   (x86-dis-make-reg-operand (lookup-x86-register (ldb (byte 4 0)
                                                                       pseudo-modrm-byte) :%)))))
          ((< intop #x90)
           (setf (x86-di-mnemonic instruction) "int"
                 (x86-di-op0 instruction)
                 (x86::make-x86-immediate-operand :value (parse-x86-lap-expression intop))))
          ((< intop #xa0)
           (setf (x86-di-mnemonic instruction)
                 "uuo-error-unbound"
                 (x86-di-op0 instruction)
                 (x86-dis-make-reg-operand (lookup-x86-register (logand intop regmask) :%))))
          ((< intop #xb0)
           (setf (x86-di-mnemonic instruction)
                 "uuo-error-udf"
                 (x86-di-op0 instruction)
                 (x86-dis-make-reg-operand (lookup-x86-register (logand intop regmask) :%))))
         
          ((< intop #xc0)
           (setf (x86-di-mnemonic instruction)
                 "uuo-error-reg-not-type"
                 (x86-di-op0 instruction)
                 (x86-dis-make-reg-operand (lookup-x86-register (logand intop regmask) :%))
                 (x86-di-op1 instruction)
                 (x86::make-x86-immediate-operand :value (parse-x86-lap-expression (x86-ds-next-u8 ds)))))
          ((< intop #xc8)
           (if (= intop #xc3)
             (let* ((pseudo-modrm-byte (x86-ds-next-u8 ds)))
               (setf (x86-di-mnemonic instruction)
                     "uuo-error-array-rank"
                     (x86-di-op0 instruction)
                     (x86-dis-make-reg-operand (lookup-x86-register (ldb (byte 4 4)
                                                                         pseudo-modrm-byte) :%))
                     (x86-di-op1 instruction)
                     (x86-dis-make-reg-operand (lookup-x86-register (ldb (byte 4 0)
                                                                         pseudo-modrm-byte) :%))))
                   
           (setf (x86-di-mnemonic instruction)
                 (case intop
                   (#xc0 "uuo-error-too-few-args")
                   (#xc1 "uuo-error-too-many-args")
                   (#xc2 "uuo-error-wrong-number-of-args")
                   (#xc4 (progn (setq stop nil) "uuo-gc-trap"))
                   (#xc5 "uuo-alloc")
                   (#xc6 "uuo-error-not-callable")
                   (#xc7 "uuo-udf-call")
                   (t "unknown-UUO")))))
          ((= intop #xc8)
           (let* ((pseudo-modrm-byte (x86-ds-next-u8 ds)))
             (declare (type (unsigned-byte 8) pseudo-modrm-byte))
             (setf (x86-di-op0 instruction)
                 (x86-dis-make-reg-operand
                  (lookup-x86-register (ldb (byte 4 4) pseudo-modrm-byte) :%))
                 (x86-di-op1 instruction)
                 (x86-dis-make-reg-operand
                  (lookup-x86-register (ldb (byte 4 0) pseudo-modrm-byte) :%))
                 (x86-di-mnemonic instruction) "uuo-error-vector-bounds")))
          ((< intop #xd0)
           (cond ((= intop #xcb)
                  (let* ((pseudo-modrm-byte (x86-ds-next-u8 ds)))
                    (setf (x86-di-mnemonic instruction)
                          "uuo-error-array-bounds"
                          (x86-di-op0 instruction)
                          (x86-dis-make-reg-operand
                           (lookup-x86-register (ldb (byte 4 4)
                                                     pseudo-modrm-byte) :%))
                          (x86-di-op1 instruction)
                          (x86-dis-make-reg-operand
                           (lookup-x86-register (ldb (byte 4 0)
                                                     pseudo-modrm-byte) :%)))))
                 ((= intop #xcc)
                  (let* ((pseudo-modrm-byte (x86-ds-next-u8 ds)))
                    (setf (x86-di-mnemonic instruction)
                          "uuo-error-eep-unresolved"
                          (x86-di-op0 instruction)
                          (x86-dis-make-reg-operand
                           (lookup-x86-register (ldb (byte 4 4)
                                                     pseudo-modrm-byte) :%))
                          (x86-di-op1 instruction)
                          (x86-dis-make-reg-operand
                           (lookup-x86-register (ldb (byte 4 0)
                                                     pseudo-modrm-byte) :%)))))
                 (t (setf (x86-di-mnemonic instruction)
                          (case intop
                            (#xc9 "uuo-error-call-macro-or-special-operator")
                            (#xca (setq stop nil) "uuo-error-debug-trap")
                            (#xcd (setq stop nil) "uuo-error-debug-trap-with-string")
                            (t "unknown-UUO"))))))
          ((< intop #xe0)
           (setf (x86-di-mnemonic instruction)
                 "uuo-error-reg-not-tag"
                 (x86-di-op0 instruction)
                 (x86-dis-make-reg-operand (lookup-x86-register (logand intop regmask) :%))
                 (x86-di-op1 instruction)
                 (x86::make-x86-immediate-operand :value (parse-x86-lap-expression (x86-ds-next-u8 ds)))))
          ((< intop #xf0)
           (setf (x86-di-mnemonic instruction)
                 "uuo-error-reg-not-list"
                 (x86-di-op0 instruction)
                 (x86-dis-make-reg-operand (lookup-x86-register (logand intop regmask) :%))))
          (t
           (setf (x86-di-mnemonic instruction)
                 "uuo-error-reg-not-fixnum"
                 (x86-di-op0 instruction)
                 (x86-dis-make-reg-operand (lookup-x86-register (logand intop regmask) :%)))))
    stop))



(defun x86-dis-analyze-operands (ds instruction flag)
  ;; If instruction is adding a positive displacement to the FN
  ;; register, note the effective address as a label reference
  ;; and modify the operand(s).
  ;; If the instruction is a MOV or PUSH whose source operand
  ;; is relative to the FN register, generate a constant reference.
  ;; If the instruction is adding a displacement to RIP, note
  ;; the effective address as a label reference.
  ;; On ia32, if op0 is a 32-bit immediate and op1 is (% fn),
  ;; treat the immediate as :self.
  (let* ((op0 (x86-di-op0 instruction))
         (op1 (x86-di-op1 instruction))
         (entry-ea (x86-ds-entry-point ds)))
    (flet ((is-fn (thing)
             (if (typep thing 'x86::x86-register-operand)
               (let* ((entry (x86::x86-register-operand-entry thing)))
                 (eq entry (if (x86-ds-mode-64 ds)
                             (x86::x86-reg64 13)
                             (x86::x86-reg32 x8632::fn))))))
           (is-rip (thing)
             (if (and (typep thing 'x86::x86-register-operand)
                      (x86-ds-mode-64 ds))
               (let* ((entry (x86::x86-register-operand-entry thing)))
                 (eq entry (svref x86::*x8664-register-entries* 102)))))
           (is-ra0 (thing)
             (if (typep thing 'x86::x86-register-operand)
               (let* ((entry (x86::x86-register-operand-entry thing)))
                 (eq entry (if (x86-ds-mode-64 ds)
                             (x86::x86-reg64 10)
                             (x86::x86-reg32 7))))))
           (is-disp-only (thing)
             (and (typep thing 'x86::x86-memory-operand)
                  (null (x86::x86-memory-operand-base thing))
                  (null (x86::x86-memory-operand-index thing))
                  (x86::x86-memory-operand-disp thing))))
      (flet ((is-fn-ea (thing)
               (and (typep thing 'x86::x86-memory-operand)
                    (is-fn (x86::x86-memory-operand-base thing))
                    (null (x86::x86-memory-operand-index thing))
                    (let* ((scale (x86::x86-memory-operand-scale thing)))
                      (or (null scale) (eql 0 scale)))
                    (let* ((disp (x86::x86-memory-operand-disp thing)))
                      (and disp (early-x86-lap-expression-value disp)))))
             (is-jump-table-ref (thing)
               (and (typep thing 'x86::x86-memory-operand)
                    (is-fn (x86::x86-memory-operand-base thing))
                    (x86::x86-memory-operand-index thing)
                    (let* ((scale (x86::x86-memory-operand-scale thing)))
                      (or (null scale) (eql 0 scale)))
                    (let* ((disp (x86::x86-memory-operand-disp thing)))
                      (and disp (early-x86-lap-expression-value disp)))))
             (is-ra0-ea (thing)
               (and (typep thing 'x86::x86-memory-operand)
                    (is-ra0 (x86::x86-memory-operand-base thing))
                    (null (x86::x86-memory-operand-index thing))
                    (let* ((scale (x86::x86-memory-operand-scale thing)))
                      (or (null scale) (eql 0 scale)))
                    (let* ((disp (x86::x86-memory-operand-disp thing)))
                      (and disp (early-x86-lap-expression-value disp)))))
             (is-rip-ea (thing)
               (and (typep thing 'x86::x86-memory-operand)
                    (is-rip (x86::x86-memory-operand-base thing))
                    (null (x86::x86-memory-operand-index thing))
                    (let* ((scale (x86::x86-memory-operand-scale thing)))
                      (or (null scale) (eql 0 scale)))
                    (let* ((disp (x86::x86-memory-operand-disp thing)))
                      (and disp (early-x86-lap-expression-value disp))))))
        (case flag
          ;; Should also check alignment here, and check
          
          (:lea
           (let* ((disp ))
             (if (or (and (setq disp (is-fn-ea op0)) (> disp 0))
                     (and (setq disp (is-ra0-ea op0)) (< disp 0) (is-fn op1)))
               (let* ((label-ea (+ entry-ea (abs disp))))
                 (when (< label-ea (x86-ds-code-limit ds))
                   (setf (x86::x86-memory-operand-disp op0)
                         (parse-x86-lap-expression
                          (if (< disp 0)
                            `(- (:^ ,label-ea))
                            `(:^ ,label-ea))))
                   (push label-ea (x86-ds-pending-labels ds))))
               (if (and (setq disp (is-rip-ea op0)) (< disp 0) (is-fn op1))
                 (progn
                   (setf (x86::x86-memory-operand-disp op0)
                         (parse-x86-lap-expression `(:^ ,entry-ea)))
                   (push entry-ea (x86-ds-pending-labels ds)))))))
          ((:jump :call)
           (let* ((disp (is-disp-only op0)))
             (when disp
               (let* ((info (find (early-x86-lap-expression-value disp)
				  (if (x86-ds-mode-64 ds)
				    x8664::*x8664-subprims*
				    x8632::*x8632-subprims*)
                                  :key #'subprimitive-info-offset)))
                 (when info (setf (x86::x86-memory-operand-disp op0)
                                  (subprimitive-info-name info)))))))
          (t
           (let* ((jtab (is-jump-table-ref op0)))
             (if (and jtab (> jtab 0))
               (let* ((count (x86-ds-u32-ref ds (- jtab 4)))
                      (block (make-x86-dis-block :start-address jtab
                                                 :end-address (+ jtab (* 4 count))))
                      (instructions (x86-dis-block-instructions block))
                      (labeled t))
                 (setf (x86::x86-memory-operand-disp op0)
                       (parse-x86-lap-expression `(:^ ,jtab)))
                 (dotimes (i count)
                   (let* ((target (+ (x86-ds-u32-ref ds jtab)
                                     (x86-ds-entry-point ds)))
                          (start (+ jtab (x86-ds-entry-point ds)))
                          (instruction (make-x86-disassembled-instruction
                                        :address jtab
                                        :labeled labeled
                                        :mnemonic ":long"
                                        :op0 (parse-x86-lap-expression `(:^ ,target))
                                        :start start
                                        :end (+ start 4))))
                     (append-dll-node instruction instructions)
                     (setq labeled nil)
                     (push target (x86-ds-pending-labels ds))
                     (incf jtab 4)))
                 (insert-x86-block block (x86-ds-blocks ds)))
               (unless (x86-ds-mode-64 ds)
                 (when (and (is-fn op1)
                            (typep op0 'x86::x86-immediate-operand)
                            ;; Not sure what else would have an
                            ;; immediate source and %fn as destination,
                            ;; but check for this.
                            (equal (x86-di-mnemonic instruction) "movl"))
                   (setf (x86-di-mnemonic instruction) "recover-fn"
                         (x86-di-op0 instruction) nil
                         (x86-di-op0 instruction) nil))))))

          )))
    instruction))

(defun x86-disassemble-instruction (ds labeled)
  (let* ((addr (x86-ds-code-pointer ds))
         (sizeflag (logior +aflag+ +dflag+
                           (if *x86-dissassemble-always-print-suffix*
                             +suffix-always+
                             0)))
         (instruction (make-x86-disassembled-instruction :address addr
                                                         :labeled labeled))
         (stop nil))
    (setf (x86-ds-insn-start ds) addr
          (x86-ds-current-instruction ds) instruction)
    (scan-prefixes ds instruction)
    (setf (x86-ds-opcode-start ds) (x86-ds-code-pointer ds))
    (let* ((primary-opcode (x86-ds-next-u8 ds))
           (two-source-ops (or (= primary-opcode #x62)
                               (= primary-opcode #xc8)))
           (prefixes (x86-ds-prefixes ds))
           (need-modrm nil)
           (uses-sse-prefix nil)
           (uses-lock-prefix nil)
           (dp nil))
      (declare (type (unsigned-byte 8) primary-opcode)
               (fixnum prefixes))
      (if (= primary-opcode #x0f)       ;two-byte opcode
        (setq primary-opcode (x86-ds-next-u8 ds)
              dp (svref *disx86-twobyte* primary-opcode)
              need-modrm (eql 1 (sbit *twobyte-has-modrm* primary-opcode))
              uses-sse-prefix (eql 1 (sbit *twobyte-uses-sse-prefix* primary-opcode))
              uses-lock-prefix (= #x20 (logandc2 primary-opcode 2)))
        (setq dp (svref *disx86* primary-opcode)
              need-modrm (eql 1 (sbit *onebyte-has-modrm* primary-opcode))))
      (when (and (not uses-sse-prefix) 
                 (logtest prefixes +prefix-repz+))
        (push "repz" (x86-di-prefixes instruction))
        (setf (x86-ds-used-prefixes ds)
              (logior (x86-ds-used-prefixes ds) +prefix-repz+)))
      (when (and (not uses-sse-prefix) 
                 (logtest prefixes +prefix-repnz+))
        (push "repnz" (x86-di-prefixes instruction))
        (setf (x86-ds-used-prefixes ds)
              (logior (x86-ds-used-prefixes ds) +prefix-repnz+)))
      (when (and (not uses-lock-prefix)
                 (logtest prefixes +prefix-lock+))
        (push "lock" (x86-di-prefixes instruction))
        (setf (x86-ds-used-prefixes ds)
              (logior (x86-ds-used-prefixes ds) +prefix-lock+)))
      (when (logtest prefixes +prefix-addr+)
        (setq sizeflag (logxor sizeflag +aflag+))
        (unless (= (x86-dis-bytemode3 dp) +loop-jcxz-mode+)
          (if (or (x86-ds-mode-64 ds)
                  (logtest sizeflag +aflag+))
            (push "addr32" (x86-di-prefixes instruction))
            (push "addr16" (x86-di-prefixes instruction)))
          (setf (x86-ds-used-prefixes ds)
                (logior (x86-ds-used-prefixes ds) +prefix-addr+))))
      (when (and (not uses-sse-prefix)
                 (logtest prefixes +prefix-data+))
        (setq sizeflag (logxor sizeflag +dflag+))
        (when (and (= (x86-dis-bytemode3 dp) +cond-jump-mode+)
                   (= (x86-dis-bytemode1 dp) +v-mode+))
          (if (logtest sizeflag +dflag+)
            (push "data32" (x86-di-prefixes instruction))
            (push "data16" (x86-di-prefixes instruction)))
          (setf (x86-ds-used-prefixes ds)
                (logior (x86-ds-used-prefixes ds) +prefix-data+))))
      (when need-modrm
        (let* ((modrm-byte (x86-ds-peek-u8 ds)))
          (declare (type (unsigned-byte 8) modrm-byte))
          (setf (x86-ds-mod ds) (ldb (byte 2 6) modrm-byte)
                (x86-ds-reg ds) (ldb (byte 3 3) modrm-byte)
                (x86-ds-rm ds) (ldb (byte 3 0) modrm-byte))))
      (if (and (null (x86-dis-mnemonic dp))
               (eql (x86-dis-bytemode1 dp) +floatcode+))
        (x86-dis-do-float ds instruction primary-opcode sizeflag)
        (if (and (null (x86-dis-mnemonic dp))
                 (eql (x86-dis-bytemode1 dp) +uuocode+))
          (progn
            (setq stop
                  (x86-dis-do-uuo ds instruction (x86-ds-next-u8 ds))))
          (progn
            (when (null (x86-dis-mnemonic dp))
              (let* ((bytemode1 (x86-dis-bytemode1 dp)))
                (declare (fixnum bytemode1))
                (cond ((= bytemode1 +use-groups+)
                       (setq dp (svref (svref *grps* (x86-dis-bytemode2 dp))
                                       (x86-ds-reg ds))))
                      ((= bytemode1 +use-prefix-user-table+)
                       (let* ((index 0))
                         (used-prefix ds +prefix-repz+)
                         (if (logtest prefixes +prefix-repz+)
                           (setq index 1)
                           (progn
                             (used-prefix ds +prefix-data+)
                             (if (logtest prefixes +prefix-data+)
                               (setq index 2)
                               (progn
                                 (used-prefix ds +prefix-repnz+)
                                 (if (logtest prefixes +prefix-repnz+)
                                   (setq index 3))))))
                         (setq dp (svref (svref *prefix-user-table*
                                                (x86-dis-bytemode2 dp))
                                         index))))
                      ((= bytemode1 +x86-64-special+)
                       (setq dp (svref (svref *x86-64-table*
                                              (x86-dis-bytemode2 dp))
                                       (if (x86-ds-mode-64 ds) 1 0))))
                    (t (error "Disassembly error")))))
          (when (x86-putop ds (x86-dis-mnemonic dp) sizeflag instruction)
            (let* ((operands ())
                   (op1 (x86-dis-op1 dp))
                   (op2 (x86-dis-op2 dp))
                   (op3 (x86-dis-op3 dp))
                   (operand nil))
              (when op1
                ;(format t "~& op1 = ~s" op1)
                (setq operand (funcall op1 ds (x86-dis-bytemode1 dp) sizeflag))
                (if operand
                  (push operand operands)))
              (when op2
                ;(format t "~& op2 = ~s" op2)
                (setq operand (funcall op2 ds (x86-dis-bytemode2 dp) sizeflag))
                (if operand
                  (push operand operands)))
              (when op3
                ;(format t "~& op3 = ~s" op3)
                (setq operand (funcall op3 ds (x86-dis-bytemode3 dp) sizeflag))
                (if operand
                  (push operand operands)))
              (if two-source-ops
                (setf (x86-di-op1 instruction) (pop operands)
                      (x86-di-op0 instruction) (pop operands))
                (setf (x86-di-op0 instruction) (pop operands)
                      (x86-di-op1 instruction) (pop operands)
                      (x86-di-op2 instruction) (pop operands))))))))
      (setf (x86-di-start instruction) (x86-ds-insn-start ds)
	    (x86-di-end instruction) (x86-ds-code-pointer ds))
      (values (x86-dis-analyze-operands ds instruction (x86-dis-flags dp))
              (or stop (eq (x86-dis-flags dp) :jump))))))

(defun x86-disassemble-new-block (ds addr)
  (setf (x86-ds-code-pointer ds) addr)
  (let* ((limit (do-dll-nodes (b (x86-ds-blocks ds) (x86-ds-code-limit ds))
                  (when (> (x86-dis-block-start-address b) addr)
                    (return (x86-dis-block-start-address b)))))
         (block (make-x86-dis-block :start-address addr))
         (instructions (x86-dis-block-instructions block))
         (labeled (not (eql addr (x86-ds-entry-point ds)))))
    (loop
      (multiple-value-bind (instruction stop)
          (x86-disassemble-instruction ds labeled)
        (setq labeled nil)
        (append-dll-node instruction instructions)
        (if stop (return))
        (if (>= (x86-ds-code-pointer ds) limit)
          (if (= (x86-ds-code-pointer ds) limit)
            (return)
            (error "Internal disassembly error")))))
    (setf (x86-dis-block-end-address block) (x86-ds-code-pointer ds))
    (insert-x86-block block (x86-ds-blocks ds))))

(defmethod unparse-x86-lap-expression ((exp t)
                                       ds)
  (declare (ignore ds))
  exp)

(defmethod unparse-x86-lap-expression ((exp constant-x86-lap-expression)
                                       ds)
  (declare (ignore ds))
  (constant-x86-lap-expression-value exp))

(defmethod unparse-x86-lap-expression ((exp label-x86-lap-expression)
                                       ds)
  (let* ((label (label-x86-lap-expression-label exp))
         (name (x86-lap-label-name label))
         (entry (x86-ds-entry-point ds)))
    `(":^" , (if (typep name 'fixnum)
            (format nil "L~d" (- name entry))
            name))))

(defmethod unparse-x86-lap-expression ((exp unary-x86-lap-expression)
                                       ds)
  `(,(unary-x86-lap-expression-operator exp)
    ,(unparse-x86-lap-expression (unary-x86-lap-expression-operand exp) ds)))

(defmethod unparse-x86-lap-expression ((exp binary-x86-lap-expression)
                                       ds)
  `(,(binary-x86-lap-expression-operator exp)
    ,(unparse-x86-lap-expression (binary-x86-lap-expression-operand0 exp) ds)
    ,(unparse-x86-lap-expression (binary-x86-lap-expression-operand1 exp) ds)))

(defmethod unparse-x86-lap-expression ((exp n-ary-x86-lap-expression)
                                       ds)
  `(,(n-ary-x86-lap-expression-operator exp)
    ,@(mapcar #'(lambda (x)
                  (unparse-x86-lap-expression x ds))
              (n-ary-x86-lap-expression-operands exp))))

(defmethod unparse-x86-lap-operand ((op x86::x86-register-operand)
                                    ds)
  (let* ((r (x86::x86-register-operand-entry op))
         (symbolic-names (x86-ds-symbolic-names ds))
         (reg-name (x86::reg-entry-reg-name r))
         (name (or (if symbolic-names
                     (gethash reg-name symbolic-names))
                     reg-name)))
    `(% ,name)))

(defmethod unparse-x86-lap-operand ((op x86::x86-immediate-operand)
                                    ds)
  `($ ,(unparse-x86-lap-expression (x86::x86-immediate-operand-value op)
                                   ds)))

(defmethod unparse-x86-lap-operand ((op x86::x86-label-operand)
                                    ds)
  (let* ((addr (x86::x86-label-operand-label op))
         (entrypoint (x86-ds-entry-point ds)))
    (format nil "L~d" (- addr entrypoint))))

(defmethod unparse-x86-lap-operand ((op label-x86-lap-expression)
                                    ds)
  (unparse-x86-lap-expression op ds))


(defmethod x86-lap-operand-constant-offset (op ds)
  (declare (ignore op ds))
  nil)

(defmethod x86-lap-operand-constant-offset ((op x86::x86-memory-operand) ds)
  (let* ((disp (x86::x86-memory-operand-disp op)) 
         (base (x86::x86-memory-operand-base op))
         (index (x86::x86-memory-operand-index op))
         (scale (x86::x86-memory-operand-scale op))
         (code-limit (x86-ds-code-limit ds))
         (val (and base
                   (eq (x86::x86-register-operand-entry base)
                       (if (x86-ds-mode-64 ds)
                         (x86::x86-reg64 13)
                         (x86::x86-reg32 x8632::fn)))
                   (null index)
                   (or (eql scale 0) (null scale))
                   (typecase disp
                     (constant-x86-lap-expression
                      (+ (x86-ds-entry-point ds)
                         (constant-x86-lap-expression-value disp)))
                     (integer
                      (+ (x86-ds-entry-point ds) disp))
                     (t nil)))))
    (when (and val (>= val code-limit))
      (- val code-limit))))

(defun x86-lap-operand-constant (op ds)
  (let ((diff (x86-lap-operand-constant-offset op ds)))
    (when diff
      (values (uvref (x86-ds-constants-vector ds)
                     (1+ (ash diff (if (x86-ds-mode-64 ds)
                                     (- x8664::word-shift)
                                     (- x8632::word-shift)))))
              t))))


(defmethod unparse-x86-lap-operand ((x x86::x86-memory-operand) ds)
  (multiple-value-bind (constant foundp) (x86-lap-operand-constant x ds)
    (if foundp
      `(@ ',constant ,(unparse-x86-lap-operand (x86::x86-memory-operand-base x) ds))
      (let* ((seg (x86::x86-memory-operand-seg x))
             (disp (x86::x86-memory-operand-disp x)) 
             (base (x86::x86-memory-operand-base x))
             (index (x86::x86-memory-operand-index x))
             (scale (x86::x86-memory-operand-scale x)))
        (collect ((subforms))
          (subforms '@)
          (if seg
            (subforms (unparse-x86-lap-operand seg ds)))
          (if disp
            (subforms (unparse-x86-lap-expression disp ds)))
          (if base
            (subforms (unparse-x86-lap-operand base ds)))
          (if index
            (subforms (unparse-x86-lap-operand index ds)))
          (if (and scale (not (eql scale 0)))
            (subforms (ash 1 scale)))
          (subforms))))))
    
(defmethod unparse-x86-lap-operand :around ((op x86::x86-operand)
                                            ds)
  (declare (ignore ds))
  (let* ((usual (call-next-method))
         (type (or (x86::x86-operand-type op) 0)))
    (if (logtest (x86::encode-operand-type :jumpabsolute) type)
      `(* ,usual)
      usual)))

(defun write-x86-lap-operand (stream op ds)
  ;; Basically, have to princ because some parts are already stringified,
  ;; plus don't want package prefixes on assembler syntax.  But want to
  ;; prin1 immediates. 
  (let ((expr (unparse-x86-lap-operand op ds)))
    (format stream " ")
    (labels ((out (stream expr)
               (cond ((atom expr)
		      (if (and (typep expr 'integer)
			       (> (abs expr) 100))
			(format stream "#x~x" expr)
			(format stream "~a" expr)))
                     ((quoted-form-p expr)
                      (format stream "'~s" (cadr expr)))
                     (t
                      (loop for item in expr as pre = "(" then " "
                        do (format stream pre)
                        do (out stream item))
                      (format stream ")")))))
      (out stream expr))))

(defvar *previous-source-note*)

(defun x86-print-di-lap (ds instruction tab-stop pc)
  (dolist (p (x86-di-prefixes instruction))
    (when tab-stop
      (format t "~vt" tab-stop))
    (format t "(~a)~%" p))
  (when tab-stop
    (format t "~vt" tab-stop))
  (format t "(~a" (x86-di-mnemonic instruction))
  (let* ((op0 (x86-di-op0 instruction))
	 (op1 (x86-di-op1 instruction))
	 (op2 (x86-di-op2 instruction)))
    (when op0
      (write-x86-lap-operand t op0 ds)
      (when op1
	(write-x86-lap-operand t op1 ds)
	(when op2
	  (write-x86-lap-operand t op2 ds)))))
  (format t ")~vt;~8<[~D]~>" (+ 40 tab-stop) pc)  
  (format t "~%"))

(defun x86-print-disassembled-instruction (ds instruction seq function)
  (let* ((addr (x86-di-address instruction))
         (entry (x86-ds-entry-point ds))
         (pc (- addr entry)))
    (let ((source-note (find-source-note-at-pc function pc)))
      (unless (eql (source-note-file-range source-note)
                   (source-note-file-range *previous-source-note*))
        (setf *previous-source-note* source-note)
        (let* ((source-text (source-note-text source-note))
               (text (if source-text
                       (string-sans-most-whitespace source-text 100)
                       "#<no source text>")))
          (format t "~&~%;;; ~A" text))))
    (when (x86-di-labeled instruction)
      (format t "~&L~d~%" pc)
      (setq seq 0))
    (format t "~&")
    (let* ((istart (x86-di-start instruction))
	   (iend (x86-di-end instruction))
	   (nbytes (- iend istart))
	   (code-vector (x86-ds-code-vector ds))
	   (byteidx istart)
	   (tab-stop (if *disassemble-verbose* 22 9)))
      (when *disassemble-verbose*
	(dotimes (i (min nbytes 4))
	  (format t " ~(~2,'0x~)" (aref code-vector byteidx))
	  (incf byteidx))
	(decf nbytes 4))
      (x86-print-di-lap ds instruction tab-stop pc)
      (when *disassemble-verbose*
	(while (plusp nbytes)
	  (format t "~8t")
	  (dotimes (i (min nbytes 4))
	    (format t " ~(~2,'0x~)" (aref code-vector byteidx))
	    (incf byteidx))
	  (format t "~%")
	  (decf nbytes 4))))
    (1+ seq)))

(defun x86-print-disassembled-function-header (function xfunction)
  (declare (ignore xfunction))
  (let ((source-note (function-source-note function)))
    (when source-note
      (ensure-source-note-text source-note)
      (if (source-note-filename source-note)
	(format t ";; ~S:~D-~D"
		(source-note-filename source-note)
		(source-note-start-pos source-note)
		(source-note-end-pos source-note))
	  (let* ((source-text (source-note-text source-note)))
	    (when source-text
	      (format t ";;; ~A" (string-sans-most-whitespace source-text 100))))))))

(defun x86-disassemble-xfunction (function xfunction
                                  &key (symbolic-names #+x8664-target target::*x8664-symbolic-register-names*
                                                       #+x8632-target target::*x8632-symbolic-register-names*)
                                       (collect-function #'x86-print-disassembled-instruction)
                                       (header-function #'x86-print-disassembled-function-header))
  (check-type xfunction xfunction)
  (check-type (uvref xfunction 0) (simple-array (unsigned-byte 8) (*)))
  (let* ((entry-point  #+x8664-target 7  #+x8632-target 2)
         (ds (make-x86-disassembly-state
              :mode-64 #+x8664-target t #+x8632-target nil
              :code-vector (uvref xfunction 0)
              :constants-vector xfunction
              :entry-point entry-point
              :code-pointer 0           ; for next-u32/next-u16 below
              :symbolic-names symbolic-names
              :pending-labels (list entry-point)))
         (blocks (x86-ds-blocks ds)))
    (setf (x86-ds-code-limit ds)
          #+x8664-target (ash (x86-ds-next-u32 ds) 3)
          #+x8632-target (ash (x86-ds-next-u16 ds) 2))
    (do* ()
         ((null (x86-ds-pending-labels ds)))
      (let* ((lab (pop (x86-ds-pending-labels ds))))
        (or (x86-dis-find-label lab blocks)
            (x86-disassemble-new-block ds lab))))
    (when (and header-function
               blocks
               (let ((something-to-disassemble nil))
                 (do-dll-nodes (block blocks)
                   (do-dll-nodes (instruction (x86-dis-block-instructions block))
                     (setf something-to-disassemble t)))
                 something-to-disassemble))
      (funcall header-function function xfunction))
    (let* ((seq 0)
           (*previous-source-note* nil))
      (declare (special *previous-source-note*))
      (do-dll-nodes (block blocks)
        (do-dll-nodes (instruction (x86-dis-block-instructions block))
          (setq seq (funcall collect-function ds instruction seq function)))))))

(defun x86-xdisassemble (function
                         &optional (collect-function #'x86-print-disassembled-instruction)
                                   (header-function #'x86-print-disassembled-function-header))
  (let* ((fv (function-to-function-vector function))
         (function-size-in-words (uvsize fv))
         (code-words (%function-code-words function))
         (ncode-bytes (ash function-size-in-words target::word-shift))
         (code-bytes (make-array ncode-bytes
                                 :element-type '(unsigned-byte 8)))
         (numimms (- function-size-in-words code-words))
         (xfunction (%alloc-misc (the fixnum (1+ numimms)) target::subtag-xfunction)))
    (declare (fixnum code-words ncode-bytes numimms))
    (%copy-ivector-to-ivector fv 0 code-bytes 0 ncode-bytes)
    (setf (uvref xfunction 0) code-bytes)
    (do* ((k code-words (1+ k))
          (j 1 (1+ j)))
         ((= k function-size-in-words)
          (x86-disassemble-xfunction function xfunction
                                     :collect-function collect-function
                                     :header-function header-function))
      (declare (fixnum j k))
      (setf (uvref xfunction j) (uvref fv k)))))

(defun disassemble-list (function)
  (collect ((instructions))
    (x86-xdisassemble
     function
     #'(lambda (ds instruction seq function)
         (declare (ignore function))
         (collect ((insn))
           (let* ((addr (x86-di-address instruction))
                  (entry (x86-ds-entry-point ds))
                  (rpc (- addr entry)))
             (if (x86-di-labeled instruction)
               (progn
                 (insn `(label ,rpc))
                 (setq seq 0))
               (insn rpc))
             (dolist (p (x86-di-prefixes instruction))
               (insn p))
             (insn (x86-di-mnemonic instruction))
             (let* ((op0 (x86-di-op0 instruction))
                    (op1 (x86-di-op1 instruction))
                    (op2 (x86-di-op2 instruction)))
               (when op0
                 (insn (unparse-x86-lap-operand op0 ds))
                 (when op1
                   (insn (unparse-x86-lap-operand op1 ds))
                   (when op2
                     (insn (unparse-x86-lap-operand op2 ds))  ))))
             (instructions (insn))
             (1+ seq))))
     nil)
    (instructions)))

(defun x86-disassembled-instruction-line (ds instruction function &optional string-stream)
  (if (null string-stream)
    (with-output-to-string (stream)
      (return-from x86-disassembled-instruction-line
                   (x86-disassembled-instruction-line ds instruction function stream)))
    (let* ((addr (x86-di-address instruction))
           (entry (x86-ds-entry-point ds))
           (pc (- addr entry))
           (op0 (x86-di-op0 instruction))
           (op1 (x86-di-op1 instruction))
           (op2 (x86-di-op2 instruction))
           (label (if (x86-di-labeled instruction) (list :label pc) pc))
           (instr (progn
                    (dolist (p (x86-di-prefixes instruction))
                      (format string-stream "(~a) " p))
                    (format string-stream "(~a" (x86-di-mnemonic instruction))
                    (when op0 (write-x86-lap-operand string-stream op0 ds))
                    (when op1 (write-x86-lap-operand string-stream op1 ds))
                    (when op2 (write-x86-lap-operand string-stream op2 ds))
                    (format string-stream ")")
                    (get-output-stream-string string-stream)))
           (comment (let ((source-note (find-source-note-at-pc function pc)))
                      (unless (eql (source-note-file-range source-note)
                                   (source-note-file-range *previous-source-note*))
                        (setf *previous-source-note* source-note)
                        (let* ((source-text (source-note-text source-note))
                               (text (if source-text
                                       (string-sans-most-whitespace source-text 100)
                                       "#<no source text>")))
                          (format string-stream ";;; ~A" text)
                          (get-output-stream-string string-stream)))))
           (imms (let ((imms nil))
                   (multiple-value-bind (imm foundp) (x86-lap-operand-constant op2 ds)
                     (when foundp (push imm imms)))
                   (multiple-value-bind (imm foundp) (x86-lap-operand-constant op1 ds)
                     (when foundp (push imm imms)))
                   (multiple-value-bind (imm foundp) (x86-lap-operand-constant op0 ds)
                     (when foundp (push imm imms)))
                   imms)))
      ;; Subtle difference between no imms and a single NIL imm, so if anybody ever
      ;; cares for some reason, they could distinguish the two cases.
      (if imms
        (values comment label instr (if (cdr imms) (coerce imms 'vector) (car imms)))
        (values comment label instr)))))

(defun disassemble-lines (function)
  (let ((source-note (function-source-note function)))
    (when source-note
      ;; Fetch source from file if don't already have it.
      (ensure-source-note-text source-note)))
  (let ((lines (make-array 20 :adjustable t :fill-pointer 0)))
    (with-output-to-string (stream)
      (x86-xdisassemble
       function
       #'(lambda (ds instruction seq function)
           (declare (ignore seq))
           (multiple-value-bind (comment label instr object)
                                (x86-disassembled-instruction-line ds instruction function stream)
             (when comment
               (vector-push-extend comment lines))
             (vector-push-extend (list object label instr) lines)))
       nil))
    (coerce lines 'simple-vector)))
