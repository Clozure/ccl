;;;-*- Mode: Lisp; Package: (X86 :use CL) -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates and contributors.
;;;   This file is part of OpenMCL.
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License   known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict  the preamble takes precedence.
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(eval-when (:compile-toplevel :load-toplevel :execute)
(require "X86-ARCH")
)

(in-package "X86")

(defconstant +MAX-OPERANDS+ 3) ; max operands per insn
(defconstant +MAX-IMMEDIATE-OPERANDS+ 2) ; max immediates per insn (lcall  ljmp)
(defconstant +MAX-MEMORY-OPERANDS+ 2) ; max memory refs per insn (string ops)

;;; Prefixes will be emitted in the order defined below.
;;; WAIT-PREFIX must be the first prefix since FWAIT is really is an
;;; instruction  and so must come before any prefixes.

(defconstant +WAIT-PREFIX+ 0)
(defconstant +LOCKREP-PREFIX+ 1)
(defconstant +ADDR-PREFIX+ 2)
(defconstant +DATA-PREFIX+ 3)
(defconstant +SEG-PREFIX+ 4)
(defconstant +REX-PREFIX+ 5) ; must come last.
(defconstant +MAX-PREFIXES+ 6) ; max prefixes per opcode

;;; we define the syntax here (modulo base index scale syntax)
(defconstant +REGISTER-PREFIX+ #\%)
(defconstant +IMMEDIATE-PREFIX+ #\$)
(defconstant +ABSOLUTE-PREFIX+ #\*)

(defconstant +TWO-BYTE-OPCODE-ESCAPE+ #x0f)
(defconstant +NOP-OPCODE+ #x90)

;;; register numbers
(defconstant +EBP-REG-NUM+ 5)
(defconstant +ESP-REG-NUM+ 4)

;;; modrm-byte.regmem for twobyte escape
(defconstant +ESCAPE-TO-TWO-BYTE-ADDRESSING+ +ESP-REG-NUM+)
;;; index-base-byte.index for no index register addressing
(defconstant +NO-INDEX-REGISTER+ +ESP-REG-NUM+)
;;; index-base-byte.base for no base register addressing
(defconstant +NO-BASE-REGISTER+ +EBP-REG-NUM+)
(defconstant +NO-BASE-REGISTER-16+ 6)

;;; these are the instruction mnemonic suffixes.
(defconstant +WORD-MNEM-SUFFIX+ #\w)
(defconstant +BYTE-MNEM-SUFFIX+ #\b)
(defconstant +SHORT-MNEM-SUFFIX+ #\s)
(defconstant +LONG-MNEM-SUFFIX+ #\l)
(defconstant +QWORD-MNEM-SUFFIX+ #\q)
(defconstant +LONG-DOUBLE-MNEM-SUFFIX+ #\x)

;;; modrm.mode = REGMEM-FIELD-HAS-REG when a register is in there
(defconstant +REGMEM-FIELD-HAS-REG+ #x3) ; always = #x3
(defconstant +REGMEM-FIELD-HAS-MEM+ (lognot +REGMEM-FIELD-HAS-REG+))


;;; cpu feature flags
(defconstant +Cpu086+ #x1)                ; Any old cpu will do  0 does the same
(defconstant +Cpu186+ #x2)                ; i186 or better required
(defconstant +Cpu286+ #x4)                ; i286 or better required
(defconstant +Cpu386+ #x8)                ; i386 or better required
(defconstant +Cpu486+ #x10)               ; i486 or better required
(defconstant +Cpu586+ #x20)               ; i585 or better required
(defconstant +Cpu686+ #x40)               ; i686 or better required
(defconstant +CpuP4+ #x80)                ; Pentium4 or better required
(defconstant +CpuK6+ #x100)               ; AMD K6 or better required
(defconstant +CpuAthlon+ #x200)           ; AMD Athlon or better required
(defconstant +CpuSledgehammer+ #x400)     ; Sledgehammer or better required
(defconstant +CpuMMX+ #x800)              ; MMX support required
(defconstant +CpuMMX2+ #x1000)            ; extended MMX support (with SSE or 3DNow!Ext) required
(defconstant +CpuSSE+ #x2000)             ; Streaming SIMD extensions required
(defconstant +CpuSSE2+ #x4000)            ; Streaming SIMD extensions 2 required
(defconstant +Cpu3dnow+ #x8000)           ; 3dnow! support required
(defconstant +Cpu3dnowA+ #x10000)         ; 3dnow!Extensions support required
(defconstant +CpuPNI+ #x20000)            ; Prescott New Instructions required
(defconstant +CpuPadLock+ #x40000)        ; VIA PadLock required
;;; These flags are set by gas depending on the flag-code.
(defconstant +Cpu64+ #x4000000)           ; 64bit support required
(defconstant +CpuNo64+ #x8000000)         ; Not supported in the 64bit mode
;;; The default value for unknown CPUs - enable all features to avoid problems.
(defconstant +CpuUnknownFlags+ (logior +Cpu086+ +Cpu186+ +Cpu286+ +Cpu386+ +Cpu486+ +Cpu586+ +Cpu686+ +CpuP4+ +CpuSledgehammer+ +CpuMMX+ +CpuMMX2+ +CpuSSE+ +CpuSSE2+ +CpuPNI+ +Cpu3dnow+ +Cpu3dnowA+ +CpuK6+ +CpuAthlon+ +CpuPadLock+))

(defparameter *cpu-feature-names*
  `((:Cpu086 . #x1) ; Any old cpu will do  0 does the same
    (:Cpu186 . #x2) ; i186 or better required
    (:Cpu286 . #x4) ; i286 or better required
    (:Cpu386 . #x8) ; i386 or better required
    (:Cpu486 . #x10) ; i486 or better required
    (:Cpu586 . #x20) ; i585 or better required
    (:Cpu686 . #x40) ; i686 or better required
    (:CpuP4 . #x80) ; Pentium4 or better required
    (:CpuK6 . #x100) ; AMD K6 or better required
    (:CpuAthlon . #x200) ; AMD Athlon or better required
    (:CpuSledgehammer . #x400) ; Sledgehammer or better required
    (:CpuMMX . #x800) ; MMX support required
    (:CpuMMX2 . #x1000) ; extended MMX support (with SSE or 3DNow!Ext) required
    (:CpuSSE . #x2000) ; Streaming SIMD extensions required
    (:CpuSSE2 . #x4000) ; Streaming SIMD extensions 2 required
    (:Cpu3dnow . #x8000) ; 3dnow! support required
    (:Cpu3dnowA . #x10000) ; 3dnow!Extensions support required
    (:CpuPNI . #x20000) ; Prescott New Instructions required
    (:CpuPadLock . #x40000) ; VIA PadLock required
    ;; These flags are set depending on the flag-code.
    (:Cpu64 . #x4000000) ; 64bit support required
    (:CpuNo64 . #x8000000))) ; Not supported in the 64bit mode

(defun %encode-cpu-flags (flags)
  (flet ((encode-atomic-cpu-flag (f)
           (cdr (assoc f *cpu-feature-names* :test #'eq))))
    (if flags
      (if (atom flags)
        (encode-atomic-cpu-flag flags)
        (let* ((k 0))
          (dolist (flag flags k)
            (let* ((k0 (encode-atomic-cpu-flag flag)))
              (if k0
                (setq k (logior k k0))
                (return))))))
      1)))
         

;;; opcode-modifier bits:
(defconstant +opcode-modifier-W+ #x1) ; set if operands can be words or dwords  encoded the canonical way
(defconstant +opcode-modifier-D+ #x2) ; D = 0 if Reg --> Regmem  D = 1 if Regmem --> Reg:    MUST BE #x2
(defconstant +opcode-modifier-Modrm+ #x4)
(defconstant +opcode-modifier-FloatR+ #x8) ; src/dest swap for floats:   MUST BE #x8
(defconstant +opcode-modifier-ShortForm+ #x10) ; register is in low 3 bits of opcode
(defconstant +opcode-modifier-FloatMF+ #x20) ; FP insn memory format bit  sized by #x4
(defconstant +opcode-modifier-Jump+ #x40) ; special case for jump insns.
(defconstant +opcode-modifier-JumpDword+ #x80) ; call and jump
(defconstant +opcode-modifier-JumpByte+ #x100) ; loop and jecxz
(defconstant +opcode-modifier-JumpInterSegment+ #x200) ; special case for intersegment leaps/calls
(defconstant +opcode-modifier-FloatD+ #x400) ; direction for float insns:  MUST BE #x400
(defconstant +opcode-modifier-Seg2ShortForm+ #x800) ; encoding of load segment reg insns
(defconstant +opcode-modifier-Seg3ShortForm+ #x1000) ; fs/gs segment register insns.
(defconstant +opcode-modifier-Size16+ #x2000) ; needs size prefix if in 32-bit mode
(defconstant +opcode-modifier-Size32+ #x4000) ; needs size prefix if in 16-bit mode
(defconstant +opcode-modifier-Size64+ #x8000) ; needs size prefix if in 16-bit mode
(defconstant +opcode-modifier-IgnoreSize+ #x10000) ; instruction ignores operand size prefix
(defconstant +opcode-modifier-DefaultSize+ #x20000) ; default insn size depends on mode
(defconstant +opcode-modifier-No-bSuf+ #x40000) ; b suffix on instruction illegal
(defconstant +opcode-modifier-No-wSuf+ #x80000) ; w suffix on instruction illegal
(defconstant +opcode-modifier-No-lSuf+ #x100000) ; l suffix on instruction illegal
(defconstant +opcode-modifier-No-sSuf+ #x200000) ; s suffix on instruction illegal
(defconstant +opcode-modifier-No-qSuf+ #x400000) ; q suffix on instruction illegal
(defconstant +opcode-modifier-No-xSuf+ #x800000) ; x suffix on instruction illegal
(defconstant +opcode-modifier-FWait+ #x1000000) ; instruction needs FWAIT
(defconstant +opcode-modifier-IsString+ #x2000000) ; quick test for string instructions
(defconstant +opcode-modifier-regKludge+ #x4000000) ; fake an extra reg operand for clr  imul
(defconstant +opcode-modifier-IsPrefix+ #x8000000) ; opcode is a prefix
(defconstant +opcode-modifier-ImmExt+ #x10000000) ; instruction has extension in 8 bit imm
(defconstant +opcode-modifier-NoRex64+ #x20000000) ; instruction don't need Rex64 prefix.
(defconstant +opcode-modifier-Rex64+ #x40000000) ; instruction require Rex64 prefix.
(defconstant +opcode-modifier-Ugh+ #x80000000) ; deprecated fp insn  gets a warning


(defconstant +opcode-modifier-NoSuf+ (logior +opcode-modifier-No-bSuf+
                                             +opcode-modifier-No-wSuf+
                                             +opcode-modifier-No-lSuf+
                                             +opcode-modifier-No-sSuf+
                                             +opcode-modifier-No-xSuf+
                                             +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-b-Suf+ (logior +opcode-modifier-No-wSuf+ +opcode-modifier-No-lSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+ +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-w-Suf+ (logior +opcode-modifier-No-bSuf+ +opcode-modifier-No-lSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+ +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-l-Suf+ (logior +opcode-modifier-No-bSuf+ +opcode-modifier-No-wSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+ +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-q-Suf+ (logior +opcode-modifier-No-bSuf+ +opcode-modifier-No-wSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-lSuf+ +opcode-modifier-No-xSuf+))
(defconstant +opcode-modifier-x-Suf+ (logior +opcode-modifier-No-bSuf+ +opcode-modifier-No-wSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-lSuf+ +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-bw-Suf+ (logior +opcode-modifier-No-lSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+ +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-bl-Suf+ (logior +opcode-modifier-No-wSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+ +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-wl-Suf+ (logior +opcode-modifier-No-bSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+ +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-wlq-Suf+ (logior +opcode-modifier-No-bSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+))
(defconstant +opcode-modifier-lq-Suf+ (logior +opcode-modifier-No-bSuf+ +opcode-modifier-No-wSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+))
(defconstant +opcode-modifier-wq-Suf+ (logior +opcode-modifier-No-bSuf+ +opcode-modifier-No-lSuf+ +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+))
(defconstant +opcode-modifier-sl-Suf+ (logior +opcode-modifier-No-bSuf+ +opcode-modifier-No-wSuf+ +opcode-modifier-No-xSuf+ +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-bwl-Suf+ (logior +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+ +opcode-modifier-No-qSuf+))
(defconstant +opcode-modifier-bwlq-Suf+ (logior +opcode-modifier-No-sSuf+ +opcode-modifier-No-xSuf+))
(defconstant +opcode-modifier-FP+ +opcode-modifier-NoSuf+)
(defconstant +opcode-modifier-l-FP+ +opcode-modifier-l-Suf+)
(defconstant +opcode-modifier-q-FP+ (logior +opcode-modifier-q-Suf+ +opcode-modifier-NoRex64+))
(defconstant +opcode-modifier-x-FP+ (logior +opcode-modifier-x-Suf+ +opcode-modifier-FloatMF+))
(defconstant +opcode-modifier-sl-FP+ (logior +opcode-modifier-sl-Suf+ +opcode-modifier-FloatMF+))
;;; Someone forgot that the FloatR bit reverses the operation when not
;;; equal to the FloatD bit.  ie. Changing only FloatD results in the
;;; destination being swapped *and* the direction being reversed.
(defconstant +opcode-modifier-FloatDR+ +opcode-modifier-FloatD+)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *opcode-modifier-names*
  `((:w . ,+opcode-modifier-W+)
    (:d . ,+opcode-modifier-D+)
    (:modrm . ,+opcode-modifier-Modrm+)
    (:shortform . ,+opcode-modifier-ShortForm+)
    (:floatr . ,+opcode-modifier-FloatR+)
    (:floatmf . ,+opcode-modifier-FloatMF+)
    (:jump . ,+opcode-modifier-Jump+)
    (:jumpdword . ,+opcode-modifier-JumpDword+)
    (:jumpbyte . ,+opcode-modifier-JumpByte+)
    (:jumpintersegment . ,+opcode-modifier-JumpInterSegment+)
    (:floatd . ,+opcode-modifier-FloatD+)
    (:seg2shortform . ,+opcode-modifier-Seg2ShortForm+)
    (:seg3shortform . ,+opcode-modifier-Seg3ShortForm+)
    (:size16 . ,+opcode-modifier-Size16+)
    (:size32 . ,+opcode-modifier-Size32+)
    (:size64 . ,+opcode-modifier-Size64+)
    (:ignoresize . ,+opcode-modifier-IgnoreSize+)
    (:no-bsuf . ,+opcode-modifier-No-bsuf+)
    (:no-wsuf . ,+opcode-modifier-No-wsuf+)
    (:no-lsuf . ,+opcode-modifier-No-lsuf+)
    (:no-ssuf . ,+opcode-modifier-No-ssuf+)
    (:no-qsuf . ,+opcode-modifier-No-qsuf+)
    (:no-xsuf . ,+opcode-modifier-No-xsuf+)
    (:defaultsize . ,+opcode-modifier-DefaultSize+)
    (:fwait . ,+opcode-modifier-FWait+)
    (:isstring . ,+opcode-modifier-IsString+)
    (:regkludge . ,+opcode-modifier-regKludge+)
    (:isprefix . ,+opcode-modifier-IsPrefix+)
    (:immext . ,+opcode-modifier-ImmExt+)
    (:norex64 . ,+opcode-modifier-NoRex64+)
    (:rex64 . ,+opcode-modifier-Rex64+)
    (:ugh . ,+opcode-modifier-Ugh+)
    (:nosuf . ,+opcode-modifier-NoSuf+)
    (:b-suf . ,+opcode-modifier-b-Suf+)
    (:w-suf . ,+opcode-modifier-w-Suf+)
    (:l-suf . ,+opcode-modifier-l-Suf+)
    (:q-suf . ,+opcode-modifier-q-Suf+)
    (:x-suf . ,+opcode-modifier-x-suf+)
    (:wl-suf . ,+opcode-modifier-wl-Suf+)
    (:wlq-suf . ,+opcode-modifier-wlq-Suf+)
    (:lq-suf . ,+opcode-modifier-lq-Suf+)
    (:wq-suf . ,+opcode-modifier-wq-Suf+)
    (:sl-suf . ,+opcode-modifier-sl-Suf+)
    (:bwl-suf . ,+opcode-modifier-bwl-Suf+)
    (:bwlq-suf . ,+opcode-modifier-bwlq-Suf+)
    (:fp . ,+opcode-modifier-FP+)
    (:l-fp . ,+opcode-modifier-l-FP+)
    (:q-fp . ,+opcode-modifier-q-FP+)
    (:x-fp . ,+opcode-modifier-x-FP+)
    (:sl-fp . ,+opcode-modifier-sl-FP+)
    (:floatd . ,+opcode-modifier-FloatD+)
    (:floatdr . ,+opcode-modifier-FloatDR+)))


;;; By default, this returns NIL if the modifier can't be encoded.
;;; That's an error, but the caller can provide better error context.
(defun %encode-opcode-modifier (mod &optional errorp)
  (flet ((encode-atomic-opcode-modifier (m)
           (if m
             (cdr (assoc m *opcode-modifier-names*))
             0)))
    (or
     (if (atom mod)
       (encode-atomic-opcode-modifier mod)
       (let* ((k 0))
         (dolist (m mod k)
           (let* ((k0 (encode-atomic-opcode-modifier m)))
             (if k0
               (setq k (logior k0 k))
               (return))))))
     (if errorp (error "Unknown x86 opcode modifier: ~s" mod)))))

)
(defmacro encode-opcode-modifier (&rest mod)
  (%encode-opcode-modifier mod t))


;;; operand-types[i] bits
;;; register
(defconstant +operand-type-Reg8+ #x1) ; 8 bit reg
(defconstant +operand-type-Reg16+ #x2) ; 16 bit reg
(defconstant +operand-type-Reg32+ #x4) ; 32 bit reg
(defconstant +operand-type-Reg64+ #x8) ; 64 bit reg
;;; immediate
(defconstant +operand-type-Imm8+ #x10) ; 8 bit immediate
(defconstant +operand-type-Imm8S+ #x20) ; 8 bit immediate sign extended
(defconstant +operand-type-Imm16+ #x40) ; 16 bit immediate
(defconstant +operand-type-Imm32+ #x80) ; 32 bit immediate
(defconstant +operand-type-Imm32S+ #x100) ; 32 bit immediate sign extended
(defconstant +operand-type-Imm64+ #x200) ; 64 bit immediate
(defconstant +operand-type-Imm1+ #x400) ; 1 bit immediate
;;; memory
(defconstant +operand-type-BaseIndex+ #x800)
;;; Disp8 16 32 are used in different ways  depending on the
;;; instruction.  For jumps  they specify the size of the PC relative
;;; displacement  for baseindex type instructions  they specify the
;;; size of the offset relative to the base register  and for memory
;;; offset instructions such as `mov 1234 %al' they specify the size of
;;; the offset relative to the segment base.
(defconstant +operand-type-Disp8+ #x1000) ; 8 bit displacement
(defconstant +operand-type-Disp16+ #x2000) ; 16 bit displacement
(defconstant +operand-type-Disp32+ #x4000) ; 32 bit displacement
(defconstant +operand-type-Disp32S+ #x8000) ; 32 bit signed displacement
(defconstant +operand-type-Disp64+ #x10000) ; 64 bit displacement
;;; specials
(defconstant +operand-type-InOutPortReg+ #x20000) ; register to hold in/out port addr = dx
(defconstant +operand-type-ShiftCount+ #x40000) ; register to hold shift cound = cl
(defconstant +operand-type-Control+ #x80000) ; Control register
(defconstant +operand-type-Debug+ #x100000) ; Debug register
(defconstant +operand-type-Test+ #x200000) ; Test register
(defconstant +operand-type-FloatReg+ #x400000) ; Float register
(defconstant +operand-type-FloatAcc+ #x800000) ; Float stack top %st(0)
(defconstant +operand-type-SReg2+ #x1000000) ; 2 bit segment register
(defconstant +operand-type-SReg3+ #x2000000) ; 3 bit segment register
(defconstant +operand-type-Acc+ #x4000000) ; Accumulator %al or %ax or %eax
(defconstant +operand-type-JumpAbsolute+ #x8000000)
(defconstant +operand-type-RegMMX+ #x10000000) ; MMX register
(defconstant +operand-type-RegXMM+ #x20000000) ; XMM registers in PIII
(defconstant +operand-type-EsSeg+ #x40000000) ; String insn operand with fixed es segment

;;; InvMem is for instructions with a modrm byte that only allow a
;;; general register encoding in the i.tm.mode and i.tm.regmem fields
;;; eg. control reg moves.  They really ought to support a memory form
;;; but don't  so we add an InvMem flag to the register operand to
;;; indicate that it should be encoded in the i.tm.regmem field.
(defconstant +operand-type-InvMem+ #x80000000)
(defconstant +operand-type-Label+ #x100000000)

(defconstant +operand-type-Reg+ (logior +operand-type-Reg8+ +operand-type-Reg16+ +operand-type-Reg32+ +operand-type-Reg64+)) ; gen'l register
(defconstant +operand-type-WordReg+ (logior +operand-type-Reg16+ +operand-type-Reg32+ +operand-type-Reg64+))
(defconstant +operand-type-ImplicitRegister+ (logior +operand-type-InOutPortReg+ +operand-type-ShiftCount+ +operand-type-Acc+ +operand-type-FloatAcc+))
(defconstant +operand-type-Imm+ (logior +operand-type-Imm8+ +operand-type-Imm8S+ +operand-type-Imm16+ +operand-type-Imm32S+ +operand-type-Imm32+ +operand-type-Imm64+)) ; gen'l immediate
(defconstant +operand-type-EncImm+ (logior +operand-type-Imm8+ +operand-type-Imm16+ +operand-type-Imm32+ +operand-type-Imm32S+)) ; Encodable gen'l immediate
(defconstant +operand-type-Disp+ (logior +operand-type-Disp8+ +operand-type-Disp16+ +operand-type-Disp32+ +operand-type-Disp32S+ +operand-type-Disp64+)) ; General displacement
(defconstant +operand-type-AnyMem+ (logior +operand-type-Disp8+ +operand-type-Disp16+ +operand-type-Disp32+ +operand-type-Disp32S+ +operand-type-BaseIndex+ +operand-type-InvMem+)) ; General memory
;;; The following aliases are defined because the opcode table
;;; carefully specifies the allowed memory types for each instruction.
;;; At the moment we can only tell a memory reference size by the
;;; instruction suffix  so there's not much point in defining Mem8
;;; Mem16  Mem32 and Mem64 opcode modifiers - We might as well just use
;;; the suffix directly to check memory operands.
(defconstant +operand-type-LLongMem+ +operand-type-AnyMem+); 64 bits (or more)
(defconstant +operand-type-LongMem+  +operand-type-AnyMem+) ; 32 bit memory ref
(defconstant +operand-type-ShortMem+ +operand-type-AnyMem+) ; 16 bit memory ref
(defconstant +operand-type-WordMem+ +operand-type-AnyMem+) ; 16 or 32 bit memory ref
(defconstant +operand-type-ByteMem+ +operand-type-AnyMem+) ; 8 bit memory ref

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *x86-operand-type-names*
  `((:Reg8 . ,+operand-type-Reg8+)
    (:Reg16 . ,+operand-type-Reg16+)
    (:Reg32 . ,+operand-type-Reg32+)
    (:Reg64 . ,+operand-type-Reg64+)
    (:Imm8 . ,+operand-type-Imm8+)
    (:Imm8S . ,+operand-type-Imm8S+)
    (:Imm16 . ,+operand-type-Imm16+)
    (:Imm32 . ,+operand-type-Imm32+)
    (:Imm32S . ,+operand-type-Imm32S+)
    (:Imm64 . ,+operand-type-Imm64+)
    (:Imm1 . ,+operand-type-Imm1+)
    (:BaseIndex . ,+operand-type-BaseIndex+)
    (:Disp8 . ,+operand-type-Disp8+)
    (:Disp16 . ,+operand-type-Disp16+)
    (:Disp32 . ,+operand-type-Disp32+)
    (:Disp32S . ,+operand-type-Disp32S+)
    (:Disp64 . ,+operand-type-Disp64+)
    (:InOutPortReg . ,+operand-type-InOutPortReg+)
    (:ShiftCount . ,+operand-type-ShiftCount+)
    (:Control . ,+operand-type-Control+)
    (:Debug . ,+operand-type-Debug+)
    (:Test . ,+operand-type-Test+)
    (:FloatReg . ,+operand-type-FloatReg+)
    (:FloatAcc . ,+operand-type-FloatAcc+)
    (:SReg2 . ,+operand-type-SReg2+)
    (:SReg3 . ,+operand-type-SReg3+)
    (:Acc . ,+operand-type-Acc+)
    (:JumpAbsolute . ,+operand-type-JumpAbsolute+)
    (:RegMMX . ,+operand-type-RegMMX+)
    (:RegXMM . ,+operand-type-RegXMM+)
    (:EsSeg . ,+operand-type-EsSeg+)
    (:InvMem . ,+operand-type-InvMem+)
    (:Reg . ,+operand-type-Reg+)
    (:WordReg . ,+operand-type-WordReg+)
    (:ImplicitRegister . ,+operand-type-ImplicitRegister+)
    (:Imm . ,+operand-type-Imm+)
    (:EncImm . ,+operand-type-EncImm+)
    (:Disp . ,+operand-type-Disp+)
    (:AnyMem . ,+operand-type-AnyMem+)
    (:LLongMem . ,+operand-type-LLongMem+)
    (:LongMem . ,+operand-type-LongMem+)
    (:ShortMem . ,+operand-type-ShortMem+)
    (:WordMem . ,+operand-type-WordMem+)
    (:ByteMem . ,+operand-type-ByteMem+)
    (:Label . ,+operand-type-Label+)
  ))

(defun %encode-operand-type (optype &optional errorp)
  (flet ((encode-atomic-operand-type (op)
           (if op
             (cdr (assoc op *x86-operand-type-names* :test #'eq))
             0)))
    (or
     (if (atom optype)
       (encode-atomic-operand-type optype)
       (let* ((k 0))
         (dolist (op optype k)
           (let* ((k0 (encode-atomic-operand-type op)))
             (if k0
               (setq k (logior k k0))
               (return))))))
     (if errorp (error "Unknown x86 operand type ~s" optype)))))
)

(defmacro encode-operand-type (&rest op)
  (%encode-operand-type op t))





(defconstant +RegRex+ #x1) ; Extended register.
(defconstant +RegRex64+ #x2) ; Extended 8 bit register.

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; these are for register name --> number & type hash lookup
(defstruct reg-entry
  reg-name
  reg-type
  reg-flags
  reg-num                               ; for encoding in instruction fields
  ordinal64                             ; canonical, ordinal register number
  ordinal32
)

(defmethod make-load-form ((r reg-entry) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots r))

(defstruct seg-entry
  seg-name
  seg-prefix
)

)


(defstruct modrm-byte
  regmem ; codes register or memory operand
  reg ; codes register operand (or extended opcode)
  mode ; how to interpret regmem & reg
)

;;; x86-64 extension prefix.
;; typedef int rex-byte
(defconstant +REX-OPCODE+ #x40)

;;; Indicates 64 bit operand size.
(defconstant +REX-MODE64+ 8)
;;; High extension to reg field of modrm byte.
(defconstant +REX-EXTX+ 4)
;;; High extension to SIB index field.
(defconstant +REX-EXTY+ 2)
;;; High extension to base field of modrm or SIB  or reg field of opcode.
(defconstant +REX-EXTZ+ 1)

;;; 386 opcode byte to code indirect addressing.
(defstruct sib-byte
  base
  index
  scale
)


;;; x86 arch names and features
(defstruct arch-entry
  name  ; arch name
  flags ; cpu feature flags
)


;;; The SystemV/386 SVR3.2 assembler  and probably all AT&T derived
;;; ix86 Unix assemblers  generate floating point instructions with
;;; reversed source and destination registers in certain cases.
;;; Unfortunately  gcc and possibly many other programs use this
;;; reversed syntax  so we're stuck with it.
;;;
;;; eg. `fsub %st(3) %st' results in st = st - st(3) as expected  but
;;;`fsub %st %st(3)' results in st(3) = st - st(3)  rather than
;;; the expected st(3) = st(3) - st
;;;
;;; This happens with all the non-commutative arithmetic floating point
;;; operations with two register operands  where the source register is
;;; %st  and destination register is %st(i).  See FloatDR below.
;;;
;;; The affected opcode map is dceX  dcfX  deeX  defX.

(defconstant +MOV-AX-DISP32+ #xa0)
(defconstant +POP-SEG-SHORT+ #x07)
(defconstant +JUMP-PC-RELATIVE+ #xe9)
(defconstant +INT-OPCODE+  #xcd)
(defconstant +INT3-OPCODE+ #xcc)
(defconstant +FWAIT-OPCODE+ #x9b)
(defconstant +ADDR-PREFIX-OPCODE+ #x67)
(defconstant +DATA-PREFIX-OPCODE+ #x66)
(defconstant +LOCK-PREFIX-OPCODE+ #xf0)
(defconstant +CS-PREFIX-OPCODE+ #x2e)
(defconstant +DS-PREFIX-OPCODE+ #x3e)
(defconstant +ES-PREFIX-OPCODE+ #x26)
(defconstant +FS-PREFIX-OPCODE+ #x64)
(defconstant +GS-PREFIX-OPCODE+ #x65)
(defconstant +SS-PREFIX-OPCODE+ #x36)
(defconstant +REPNE-PREFIX-OPCODE+ #xf2)
(defconstant +REPE-PREFIX-OPCODE+  #xf3)


(defstruct (x86-opcode-template (:constructor %make-x86-opcode-template))
  mnemonic               ; fully qualified, includes suffix if applicable
  flags                  ; cpuflags
  ordinal                ; unique id
  operand-types          ; as specific as possible
  operand-classes        ; describe how to insert operands in base op, modrm
  prefixes               ; list of 0 or more explicit prefixes
  base-opcode            ; 1-3 bytes
  rex-prefix             ; initial REX value
  modrm-byte             ; initial modrm vale, may be nil if no modrm byte
  )


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-x86-opcode-operand-types (types&classes)
  (ccl::collect ((types))
    (dolist (t&c types&classes (apply #'vector (types)))
      (destructuring-bind (type class) t&c
        (declare (ignore class))
        (types (%encode-operand-type type t))))))



(defparameter *x86-operand-insert-function-keywords*
  #(:insert-nothing
    :insert-modrm-reg
    :insert-modrm-rm
    :insert-memory
    :insert-opcode-reg
    :insert-opcode-reg4
    :insert-cc
    :insert-label
    :insert-imm8-for-int
    :insert-extra
    :insert-imm8
    :insert-imm8s
    :insert-imm16
    :insert-imm32s
    :insert-imm32
    :insert-imm64
    :insert-mmx-reg
    :insert-mmx-rm
    :insert-xmm-reg
    :insert-xmm-rm
    :insert-reg4-pseudo-rm-high
    :insert-reg4-pseudo-rm-low
    ))

(defun parse-x86-opcode-operand-classes (types&classes)
  (ccl::collect ((classes))
    (dolist (t&c types&classes (apply #'vector (classes)))
      (destructuring-bind (type class) t&c
        (declare (ignore type))
        (classes (or (position class *x86-operand-insert-function-keywords*)
                     (error "Unknown operand class: ~s" class)))))))

(defun parse-x86-opcode-name (name&flags)
  (string-downcase (if (atom name&flags) name&flags (car name&flags))))


(defun parse-x86-opcode-flags (name&flags)
  (if (atom name&flags)
    0
    (%encode-opcode-modifier (cdr name&flags))))

)

;;; Any instruction with no operands.
(defstruct x86-instruction
  opcode-template
  rex-prefix                            ; ignored in 32-bit assembly
  base-opcode
  modrm-byte
  sib-byte
  seg-prefix
  disp
  imm
  extra
  )

(defun need-modrm-byte (instruction)
  (or (x86-instruction-modrm-byte instruction)
      (error "Bug: no modrm byte in ~s" instruction)))

(defun need-rex-prefix (instruction)
  (or (x86-instruction-rex-prefix instruction)
      (error "Bug: no REX prefix in ~s" instruction)))




(defconstant modrm-mod-byte (byte 2 6))
(defconstant modrm-reg-byte (byte 3 3))
(defconstant modrm-rm-byte (byte 3 0))

(defconstant sib-scale-byte (byte 2 6))
(defconstant sib-index-byte (byte 3 3))
(defconstant sib-base-byte (byte 3 0))

(defun mode-from-disp-size (type)
  (cond ((logtest type (x86::encode-operand-type :disp8)) 1)
        ((logtest type (x86::encode-operand-type :disp16 :disp32 :disp32S)) 2)
        (t 0)))


(defun insert-memory-operand-values (instruction
                                     explicit-seg
                                     disp
                                     base
                                     index
                                     scale
                                     memtype)
  (declare (special *ds-segment-register* *ss-segment-register*)) ;fwd refs
  (let* ((rm-byte (x86-instruction-modrm-byte instruction))
         (sib 0)
         (default-seg *ds-segment-register*))
    (cond ((null base)
           (setf (ldb modrm-mod-byte rm-byte) 0
                 (ldb modrm-rm-byte rm-byte) +escape-to-two-byte-addressing+
                 (ldb sib-base-byte sib) +no-base-register+
                 memtype (encode-operand-type :disp32s))
           (cond ((null index)
                  ;; Just a displacement.
                  (setf (ldb sib-index-byte sib) +no-index-register+))
                 (t
                  ;; No base, but index
                  (let* ((index-reg (reg-entry-reg-num index)))
                    (setf (ldb sib-index-byte sib) index-reg
                          (ldb sib-scale-byte sib) (or scale 0))
                    (when (logtest (reg-entry-reg-flags index) +RegRex+)
                      (setf (x86-instruction-rex-prefix instruction)
                            (logior +rex-exty+ (need-rex-prefix instruction))))))))
          ((= (reg-entry-reg-type base) (encode-operand-type :baseIndex))
           ;; RIP-relative.  Need a displacement if we don't already
           ;; have one.
           (setf (ldb modrm-rm-byte rm-byte) +no-base-register+)
           (setq memtype
                 (logior (encode-operand-type :disp32s)
                         (encode-operand-type :label)
                         (logandc2 memtype (encode-operand-type :disp)))))
          (t
           ;; have a real base register (not just %rip).  Maybe an
           ;; index register, too.
           (let* ((baseregnum (reg-entry-reg-num base)))
             (setf (ldb modrm-rm-byte rm-byte) baseregnum)
             (when (logtest (reg-entry-reg-flags base) +RegRex+)
               (setf (x86-instruction-rex-prefix instruction)
                     (logior 1 (need-rex-prefix instruction))))
             (setf (ldb sib-base-byte sib) baseregnum)
             (cond ((= (logand baseregnum 7) +ebp-reg-num+)
                    (setq default-seg *ss-segment-register*)
                    (unless disp
                      (setf memtype (logior memtype (encode-operand-type :disp8)))))
                   ((= baseregnum x86::+esp-reg-num+)
                    (setq default-seg x86::*ss-segment-register*)))
             (setf (ldb sib-scale-byte sib) (or scale 0))
             (if (null index)
               (setf (ldb sib-index-byte sib) +no-index-register+)
               (progn
                 (setf (ldb sib-index-byte sib)
                       (reg-entry-reg-num index)
                       (ldb modrm-rm-byte rm-byte) +escape-to-two-byte-addressing+)
                 (when (logtest (reg-entry-reg-flags index) +RegRex+)
                   (setf (x86-instruction-rex-prefix instruction)
                         (logior +rex-exty+
                                 (need-rex-prefix instruction)))))))
               (setf (ldb modrm-mod-byte rm-byte) (mode-from-disp-size memtype))))
    (setf (x86-instruction-modrm-byte instruction) rm-byte)
    (when (= (ldb modrm-rm-byte rm-byte) +escape-to-two-byte-addressing+)
      (setf (x86-instruction-sib-byte instruction) sib))
    (when (logtest memtype (encode-operand-type :disp))
      (unless disp (setq disp 0))
      (setf (x86-instruction-disp instruction) disp
            (x86-instruction-extra instruction) memtype))
    (when (and explicit-seg
               (not (eq explicit-seg default-seg)))
      (setf (x86-instruction-seg-prefix instruction)
            (seg-entry-seg-prefix explicit-seg)))))

(defun insert-memory (instruction operand)
  (insert-memory-operand-values instruction
                                (x86-memory-operand-seg operand)
                                (x86-memory-operand-disp operand)
                                (x86-memory-operand-base operand)
                                (x86-memory-operand-index operand)
                                (x86-memory-operand-scale operand)
                                (x86-memory-operand-type operand)))

         
(defmacro def-x8664-opcode (name&flags types-and-classes base-opcode
                                          modrm-byte
                                          rex-prefix
                                          &rest prefixes)
  `(%make-x86-opcode-template
    :mnemonic ,(parse-x86-opcode-name name&flags)
    :flags ,(parse-x86-opcode-flags name&flags)
    :operand-types ,(parse-x86-opcode-operand-types types-and-classes)
    :operand-classes ,(parse-x86-opcode-operand-classes types-and-classes)
    :base-opcode ,base-opcode
    :prefixes ',prefixes
    :rex-prefix ,rex-prefix
    :modrm-byte ,modrm-byte))


(defparameter *x8664-opcode-templates*
  (vector
   ;; adc
   (def-x8664-opcode adcq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x11 #o300 #x48)
   (def-x8664-opcode adcq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x13 #o000 #x48)
   (def-x8664-opcode adcq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x11 #x00 #x48)
   (def-x8664-opcode adcq ((:imm8s :insert-imm8s) (:reg64 :insert-modrm-rm))
     #x83 #o320 #x48)
   (def-x8664-opcode adcq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x15 nil #x48)
   (def-x8664-opcode adcq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #x81 #o320 #x48)
   (def-x8664-opcode adcq ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o020 #x48)
   (def-x8664-opcode adcq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o020 #x48)

   (def-x8664-opcode adcl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x11 #o300 #x00)
   (def-x8664-opcode adcl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x13 #o000 #x00)
   (def-x8664-opcode adcl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x11 #x00 #x00)
   (def-x8664-opcode adcl ((:imm8s :insert-imm8s) (:reg32 :insert-modrm-rm))
     #x83 #o320 #x00)
   (def-x8664-opcode adcl ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x15 nil nil)
   (def-x8664-opcode adcl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #x81 #o320 #x00)
   (def-x8664-opcode adcl ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o020 #x00)
   (def-x8664-opcode adcl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o020 #x00)

   (def-x8664-opcode adcw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x11 #o300 #x00 #x66)
   (def-x8664-opcode adcw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x13 #o000 #x00 #x66)
   (def-x8664-opcode adcw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x11 #x00 #x00 #x66)
   (def-x8664-opcode adcw ((:imm8s :insert-imm8s) (:reg16 :insert-modrm-rm))
     #x83 #o320 #x00 #x66)
   (def-x8664-opcode adcw ((:imm16 :insert-imm16) (:acc :insert-nothing))
     #x15 nil nil #x66)
   (def-x8664-opcode adcw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #x81 #o320 #x00 #x66)
   (def-x8664-opcode adcw ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o020 #x00 #x66)
   (def-x8664-opcode adcw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #x81 #o020 #x00 #x66)

   (def-x8664-opcode adcb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x10 #o300 #x00)
   (def-x8664-opcode adcb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x12 #o000 #x00)
   (def-x8664-opcode adcb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x10 #x00 #x00)
   (def-x8664-opcode adcb ((:imm8 :insert-imm8) (:acc :insert-nothing))
     #x14 nil nil)
   (def-x8664-opcode adcb ((:imm8 :insert-imm8) (:reg8 :insert-modrm-rm))
     #x80 #o320 #x00)
   (def-x8664-opcode adcb ((:imm8 :insert-imm8) (:reg8 :insert-modrm-rm))
     #x80 #o320 #x00)
   (def-x8664-opcode adcb ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x80 #o020 #x00)

   ;; add
   (def-x8664-opcode addq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x01 #o300 #x48)
   (def-x8664-opcode addq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x03 #o000 #x48)
   (def-x8664-opcode addq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x01 #x00 #x48)
   (def-x8664-opcode addq ((:imm8s :insert-imm8s) (:reg64 :insert-modrm-rm))
     #x83 #o300 #x48)
   (def-x8664-opcode addq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x05 nil #x48)
   (def-x8664-opcode addq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #x81 #o300 #x48)
   (def-x8664-opcode addq ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o000 #x48)
   (def-x8664-opcode addq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o000 #x48)

   (def-x8664-opcode addl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x01 #o300 #x00)
   (def-x8664-opcode addl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x03 #o000 #x00)
   (def-x8664-opcode addl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x01 #x00 #x00)
   (def-x8664-opcode addl ((:imm8s :insert-imm8s) (:reg32 :insert-modrm-rm))
     #x83 #o300 #x00)
   (def-x8664-opcode addl ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x05 nil nil)
   (def-x8664-opcode addl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #x81 #o300 #x00)
   (def-x8664-opcode addl ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o000 #x00)
   (def-x8664-opcode addl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o000 #x00)

   (def-x8664-opcode addw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x01 #o300 #x00 #x66)
   (def-x8664-opcode addw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x03 #o000 #x00 #x66)
   (def-x8664-opcode addw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x01 #x00 #x00 #x66)
   (def-x8664-opcode addw ((:imm8s :insert-imm8s) (:reg16 :insert-modrm-rm))
     #x83 #o300 #x00 #x66)
   (def-x8664-opcode addw ((:imm16 :insert-imm16) (:acc :insert-nothing))
     #x05 nil nil #x66)
   (def-x8664-opcode addw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #x81 #o300 #x00 #x66)
   (def-x8664-opcode addw ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o000 #x00 #x66)
   (def-x8664-opcode addw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #x81 #o000 #x00 #x66)

   (def-x8664-opcode addb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x00 #o300 #x00)
   (def-x8664-opcode addb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x02 #o000 #x00)
   (def-x8664-opcode addb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x00 #x00 #x00)
   (def-x8664-opcode addb ((:imm8s :insert-imm8s) (:acc :insert-nothing))
     #x04 nil nil)
   (def-x8664-opcode addb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o300 #x00)
   (def-x8664-opcode addb ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x80 #o000 #x00)

   ;; and
   (def-x8664-opcode andq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x21 #o300 #x48)
   (def-x8664-opcode andq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x23 #o000 #x48)
   (def-x8664-opcode andq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x21 #x00 #x48)
   (def-x8664-opcode andq ((:imm8s :insert-imm8s) (:reg64 :insert-modrm-rm))
     #x83 #o340 #x48)
   (def-x8664-opcode andq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x25 nil #x48)
   (def-x8664-opcode andq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #x81 #o340 #x48)
   (def-x8664-opcode andq ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o040 #x48)
   (def-x8664-opcode andq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o040 #x48)

   (def-x8664-opcode andl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x21 #o300 #x00)
   (def-x8664-opcode andl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x23 #o000 #x00)
   (def-x8664-opcode andl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x21 #x00 #x00)
   (def-x8664-opcode andl ((:imm8s :insert-imm8s) (:reg32 :insert-modrm-rm))
     #x83 #o340 #x00)
   (def-x8664-opcode andl ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x25 nil nil)
   (def-x8664-opcode andl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #x81 #o340 #x00)
   (def-x8664-opcode andl ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o040 #x00)
   (def-x8664-opcode andl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o040 #x00)

   (def-x8664-opcode andw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x21 #o300 #x00 #x66)
   (def-x8664-opcode andw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x23 #o000 #x00 #x66)
   (def-x8664-opcode andw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x21 #x00 #x00 #x66)
   (def-x8664-opcode andw ((:imm8s :insert-imm8s) (:reg16 :insert-modrm-rm))
     #x83 #o340 #x00 #x66)
   (def-x8664-opcode andw ((:imm16 :insert-imm16) (:acc :insert-nothing))
     #x25 nil nil #x66)
   (def-x8664-opcode andw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #x81 #o340 #x00 #x66)
   (def-x8664-opcode andw ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o040 #x00 #x66)
   (def-x8664-opcode andw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #x81 #o040 #x00 #x66)

   (def-x8664-opcode andb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x20 #o300 #x00)
   (def-x8664-opcode andb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x22 #o000 #x00)
   (def-x8664-opcode andb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x20 #o000 #x00)
   (def-x8664-opcode andb ((:imm8s :insert-imm8s) (:acc :insert-nothing))
     #x24 nil nil)
   (def-x8664-opcode andb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o340 #x00)
   (def-x8664-opcode andb ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x80 #o040 #x00)

   ;; bsf
   (def-x8664-opcode bsfq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0fbc #o300 #x48)
   (def-x8664-opcode bsfq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0fbc #o000 #x48)

   (def-x8664-opcode bsfl ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0fbc #o300 #x00)
   (def-x8664-opcode bsfl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0fbc #o000 #x00)

   (def-x8664-opcode bsfw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0fbc #o300 #x00 #x66)
   (def-x8664-opcode bsfw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0fbc #o000 #x00 #x66)

   ;; bsr
   (def-x8664-opcode bsrq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0fbd #o300 #x48)
   (def-x8664-opcode bsrq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0fbd #o000 #x48)

   (def-x8664-opcode bsrl ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0fbd #o300 #x00)
   (def-x8664-opcode bsrl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0fbd #o000 #x00)

   (def-x8664-opcode bsrw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0fbd #o300 #x00 #x66)
   (def-x8664-opcode bsrw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0fbd #o000 #x00 #x66)

   ;; bswap
   (def-x8664-opcode bswapq ((:reg64 :insert-opcode-reg))
     #x0fc8 nil #x48)

   (def-x8664-opcode bswapl ((:reg64 :insert-opcode-reg))
     #x0fc8 nil #x00)

   ;; bt
   (def-x8664-opcode btq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #x0fba #o340 #x48)
   (def-x8664-opcode btq ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o040 #x48)
   (def-x8664-opcode btq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa3 #o300 #x48)
   (def-x8664-opcode btq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa3 #o000 #x48)

   (def-x8664-opcode btl ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #x0fba #o340 #x00)
   (def-x8664-opcode btl ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o040 #x00)
   (def-x8664-opcode btl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x0fa3 #o300 #x00)
   (def-x8664-opcode btl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa3 #o000 #x00)

   (def-x8664-opcode btw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #x0fba #o340 #x00 #x66)
   (def-x8664-opcode btw ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o040 #x00 #x66)
   (def-x8664-opcode btw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x0fa3 #o300 #x00 #x66)
   (def-x8664-opcode btw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa3 #o000 #x00 #x66)

   ;; btc
   (def-x8664-opcode btcq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #x0fba #o370 #x48)
   (def-x8664-opcode btcq ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o070 #x48)
   (def-x8664-opcode btcq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fbb #o300 #x48)
   (def-x8664-opcode btcq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fbb #o000 #x48)

   (def-x8664-opcode btcl ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #x0fba #o370 #x00)
   (def-x8664-opcode btcl ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o070 #x00)
   (def-x8664-opcode btcl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x0fbb #o300 #x00)
   (def-x8664-opcode btcl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fbb #o000 #x00)

   (def-x8664-opcode btcw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #x0fba #o370 #x00 #x66)
   (def-x8664-opcode btcw ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o070 #x00 #x66)
   (def-x8664-opcode btcw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x0fbb #o300 #x00 #x66)
   (def-x8664-opcode btcw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fbb #o000 #x00 #x66)

   ;; btr
   (def-x8664-opcode btrq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #x0fba #o360 #x48)
   (def-x8664-opcode btrq ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o060 #x48)
   (def-x8664-opcode btrq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fb3 #o300 #x48)
   (def-x8664-opcode btrq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fb3 #o000 #x48)

   (def-x8664-opcode btrl ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #x0fba #o360 #x00)
   (def-x8664-opcode btrl ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o060 #x00)
   (def-x8664-opcode btrl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x0fb3 #o300 #x00)
   (def-x8664-opcode btrl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fb3 #o000 #x00)

   (def-x8664-opcode btrw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #x0fba #o360  #x00 #x66)
   (def-x8664-opcode btrw ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o060 #x00 #x66)
   (def-x8664-opcode btrw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x0fb3 #o300 #x00 #x66)
   (def-x8664-opcode btrw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fb3 #o000 #x00 #x66)

   ;; bts
   (def-x8664-opcode btsq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #x0fba #o350 #x48)
   (def-x8664-opcode btsq ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o050 #x48)
   (def-x8664-opcode btsq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fab #o300 #x48)
   (def-x8664-opcode btsq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fab #o000 #x48)

   (def-x8664-opcode btsl ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #x0fba #o350 #x00)
   (def-x8664-opcode btsl ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o050 #x00)
   (def-x8664-opcode btsl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x0fab #o300 #x00)
   (def-x8664-opcode btsl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fab #o000 #x00)

   (def-x8664-opcode btsw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #x0fba #o350  #x00 #x66)
   (def-x8664-opcode btsw ((:imm8 :insert-imm8) (:anymem :insert-memory))
     #x0fba #o050 #x00 #x66)
   (def-x8664-opcode btsw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x0fab #o300 #x00 #x66)
   (def-x8664-opcode btsw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fab #o000 #x00 #x66)

   ;; call
   ;; Probably need to align CALL instructions within the containing function,
   ;; so that return addresses are tagged appropriately.
   (def-x8664-opcode call ((:label :insert-label))
     #xe8 nil nil)

   (def-x8664-opcode call ((:reg64 :insert-modrm-rm))
     #xff #o320 #x0)

   (def-x8664-opcode call ((:anymem :insert-memory))
     #xff #o020 #x0)

   ;; cbtw
   (def-x8664-opcode cbtw ()
     #x98 nil nil #x66)

   ;; clc
   (def-x8664-opcode clc ()
     #xf8 nil nil)

   ;; cld
   (def-x8664-opcode cld ()
     #xfc nil nil)

   ;; cltd
   (def-x8664-opcode cltd ()
     #x99 nil nil)

  
   ;; cltq
   (def-x8664-opcode cltq ()
     #x98 nil #x48)

   ;; cmc
   (def-x8664-opcode cmc ()
     #xf5 nil nil)

   ;; cmovCC
   (def-x8664-opcode cmovccq
       ((:imm8 :insert-cc) (:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f40 #o300 #x48)
   (def-x8664-opcode cmovccq
       ((:imm8 :insert-cc) (:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f40 #o000 #x48)
   (def-x8664-opcode cmovccl
       ((:imm8 :insert-cc) (:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f40 #o300 #x00)
   (def-x8664-opcode cmovccl
       ((:imm8 :insert-cc) (:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f40 #o000 #x00)
   (def-x8664-opcode cmovccw
       ((:imm8 :insert-cc) (:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f40 #o300 #x00 #x66)
   (def-x8664-opcode cmovccw
       ((:imm8 :insert-cc) (:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f40 #o000 #x00 #x66)

   (def-x8664-opcode cmovoq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f40 #o300 #x48)
   (def-x8664-opcode cmovoq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f40 #o000 #x48)
   (def-x8664-opcode cmovol ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f40 #o300 #x00)
   (def-x8664-opcode cmovol ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f40 #o000 #x00)
   (def-x8664-opcode cmovow ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f40 #o300 #x00 #x66)
   (def-x8664-opcode cmovow ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f40 #o000 #x00 #x66)

   (def-x8664-opcode cmovnoq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f41 #o300 #x48)
   (def-x8664-opcode cmovnoq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f41 #o000 #x48)
   (def-x8664-opcode cmovnol ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f41 #o300 #x00)
   (def-x8664-opcode cmovnol ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f41 #o000 #x00)
   (def-x8664-opcode cmovnow ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f41 #o300 #x00 #x66)
   (def-x8664-opcode cmovnow ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f41 #o000 #x00 #x66)

   (def-x8664-opcode cmovbq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f42 #o300 #x48)
   (def-x8664-opcode cmovbq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f42 #o000 #x48)
   (def-x8664-opcode cmovbl ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f42 #o300 #x00)
   (def-x8664-opcode cmovbl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f42 #o000 #x00)
   (def-x8664-opcode cmovbw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f42 #o300 #x00 #x66)
   (def-x8664-opcode cmovbw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f42 #o000 #x00 #x66)

   (def-x8664-opcode cmovaeq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f43 #o300 #x48)
   (def-x8664-opcode cmovaeq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f43 #o000 #x48)
   (def-x8664-opcode cmovael ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f43 #o300 #x00)
   (def-x8664-opcode cmovael ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f43 #o000 #x00)
   (def-x8664-opcode cmovaew ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f43 #o300 #x00 #x66)
   (def-x8664-opcode cmovaew ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f43 #o000 #x00 #x66)

   (def-x8664-opcode cmoveq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f44 #o300 #x48)
   (def-x8664-opcode cmoveq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f44 #o000 #x48)
   (def-x8664-opcode cmovel ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f44 #o300 #x00)
   (def-x8664-opcode cmovel ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f44 #o000 #x00)
   (def-x8664-opcode cmovew ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f44 #o300 #x00 #x66)
   (def-x8664-opcode cmovew ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f44 #o000 #x00 #x66)

   (def-x8664-opcode cmovneq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f45 #o300 #x48)
   (def-x8664-opcode cmovneq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f45 #o000 #x48)
   (def-x8664-opcode cmovnel ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f45 #o300 #x00)
   (def-x8664-opcode cmovnel ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f45 #o000 #x00)
   (def-x8664-opcode cmovnew ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f45 #o300 #x00 #x66)
   (def-x8664-opcode cmovnew ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f45 #o000 #x00 #x66)

   (def-x8664-opcode cmovbeq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f46 #o300 #x48)
   (def-x8664-opcode cmovbeq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f46 #o000 #x48)
   (def-x8664-opcode cmovbel ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-reg))
     #x0f46 #o300 #x00)
   (def-x8664-opcode cmovbel ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f46 #o000 #x00)
   (def-x8664-opcode cmovbew ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f46 #o300 #x00 #x66)
   (def-x8664-opcode cmovbew ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f46 #o000 #x00 #x66)

   (def-x8664-opcode cmovaq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f47 #o300 #x48)
   (def-x8664-opcode cmovaq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f47 #o000 #x48)
   (def-x8664-opcode cmoval ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f47 #o300 #x00)
   (def-x8664-opcode cmoval ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f47 #o000 #x00)
   (def-x8664-opcode cmovaw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f47 #o300 #x00 #x66)
   (def-x8664-opcode cmovaw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f47 #o000 #x00 #x66)

   (def-x8664-opcode cmovsq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f48 #o300 #x48)
   (def-x8664-opcode cmovsq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f48 #o000 #x48)
   (def-x8664-opcode cmovsl ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f48 #o300 #x00)
   (def-x8664-opcode cmovsl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f48 #o000 #x00)
   (def-x8664-opcode cmovsw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f48 #o300 #x00 #x66)
   (def-x8664-opcode cmovsw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f48 #o000 #x00 #x66)

   (def-x8664-opcode cmovnsq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f49 #o300 #x48)
   (def-x8664-opcode cmovnsq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f49 #o000 #x48)
   (def-x8664-opcode cmovnsl ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f49 #o300 #x00)
   (def-x8664-opcode cmovnsl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f49 #o000 #x00)
   (def-x8664-opcode cmovnsw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f49 #o300 #x00 #x66)
   (def-x8664-opcode cmovnsw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f49 #o000 #x00 #x66)

   (def-x8664-opcode cmovpeq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f4a #o300 #x48)
   (def-x8664-opcode cmovpeq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f4a #o000 #x48)
   (def-x8664-opcode cmovpel ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f4a #o300 #x00)
   (def-x8664-opcode cmovpel ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f4a #o000 #x00)
   (def-x8664-opcode cmovpew ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f4a #o300 #x00 #x66)
   (def-x8664-opcode cmovpew ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f4a #o000 #x00 #x66)

   (def-x8664-opcode cmovpoq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f4b #o300 #x48)
   (def-x8664-opcode cmovpoq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f4b #o000 #x48)
   (def-x8664-opcode cmovpol ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f4b #o300 #x00)
   (def-x8664-opcode cmovpol ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f4b #o000 #x00)
   (def-x8664-opcode cmovpow ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f4b #o300 #x00 #x66)
   (def-x8664-opcode cmovpow ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f4b #o000 #x00 #x66)

   (def-x8664-opcode cmovlq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f4c #o300 #x48)
   (def-x8664-opcode cmovlq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f4c #o000 #x48)
   (def-x8664-opcode cmovll ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f4c #o300 #x00)
   (def-x8664-opcode cmovll ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f4c #o000 #x00)
   (def-x8664-opcode cmovlw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f4c #o300 #x00 #x66)
   (def-x8664-opcode cmovlw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f4c #o000 #x00 #x66)

   (def-x8664-opcode cmovgeq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f4d #o300 #x48)
   (def-x8664-opcode cmovgeq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f4d #o000 #x48)
   (def-x8664-opcode cmovgel ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f4d #o300 #x00)
   (def-x8664-opcode cmovgel ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f4d #o000 #x00)
   (def-x8664-opcode cmovgew ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f4d #o300 #x00 #x66)
   (def-x8664-opcode cmovgew ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f4d #o000 #x00 #x66)

   (def-x8664-opcode cmovleq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f4e #o300 #x48)
   (def-x8664-opcode cmovleq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f4e #o000 #x48)
   (def-x8664-opcode cmovlel ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f4e #o300 #x00)
   (def-x8664-opcode cmovlel ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f4e #o000 #x00)
   (def-x8664-opcode cmovlew ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f4e #o300 #x00 #x66)
   (def-x8664-opcode cmovlew ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f4e #o000 #x00 #x66)

   (def-x8664-opcode cmovgq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0f4f #o300 #x48)
   (def-x8664-opcode cmovgq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f4f #o000 #x48)
   (def-x8664-opcode cmovgl ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0f4f #o300 #x00)
   (def-x8664-opcode cmovgl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f4f #o000 #x00)
   (def-x8664-opcode cmovgw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0f4f #o300 #x00 #x66)
   (def-x8664-opcode cmovgw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0f4f #o000 #x00 #x66)


   ;; cmp

   (def-x8664-opcode cmpq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x39 #o300 #x48)
   (def-x8664-opcode rcmpq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x39 #o300 #x48)
   (def-x8664-opcode cmpq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x3b #o000 #x48)
   (def-x8664-opcode rcmpq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x3b #o000 #x48)   
   (def-x8664-opcode cmpq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x39 #x00 #x48)
   (def-x8664-opcode rcmpq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x39 #x00 #x48)
   (def-x8664-opcode cmpq ((:imm8s :insert-imm8s) (:reg64 :insert-modrm-rm))
     #x83 #o370 #x48)
   (def-x8664-opcode rcmpq ((:reg64 :insert-modrm-rm) (:imm8s :insert-imm8s))
     #x83 #o370 #x48)
   (def-x8664-opcode cmpq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x3d nil #x48)
   (def-x8664-opcode rcmpq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x3d nil #x48)
   (def-x8664-opcode cmpq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #x81 #o370 #x48)
   (def-x8664-opcode rcmpq ((:reg64 :insert-modrm-rm) (:imm32s :insert-imm32s))
     #x81 #o370 #x48)   
   (def-x8664-opcode cmpq ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o070 #x48)
   (def-x8664-opcode rcmpq ((:anymem :insert-memory) (:imm8s :insert-imm8s))
     #x83 #o070 #x48)
   (def-x8664-opcode cmpq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o070 #x48)
   (def-x8664-opcode rcmpq ((:anymem :insert-memory) (:imm32s :insert-imm32s))
     #x81 #o070 #x48)

   (def-x8664-opcode cmpl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x39 #o300 #x00)
   (def-x8664-opcode rcmpl ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x39 #o300 #x00)   
   (def-x8664-opcode cmpl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x3b #o000 #x00)
   (def-x8664-opcode rcmpl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x3b #o000 #x00)   
   (def-x8664-opcode cmpl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x39 #x00 #x00)
   (def-x8664-opcode rcmpl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x39 #x00 #x00)   
   (def-x8664-opcode cmpl ((:imm8s :insert-imm8s) (:reg32 :insert-modrm-rm))
     #x83 #o370 #x00)
   (def-x8664-opcode rcmpl ((:reg32 :insert-modrm-rm) (:imm8s :insert-imm8s))
     #x83 #o370 #x00)   
   (def-x8664-opcode cmpl ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x3d nil nil)
   (def-x8664-opcode rcmpl ((:acc :insert-nothing) (:imm32s :insert-imm32s))
     #x3d nil nil)   
   (def-x8664-opcode cmpl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #x81 #o370 #x00)
   (def-x8664-opcode rcmpl ((:reg32 :insert-modrm-rm) (:imm32s :insert-imm32s))
     #x81 #o370 #x00)   
   (def-x8664-opcode cmpl ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o070 #x00)
   (def-x8664-opcode rcmpl ((:anymem :insert-memory) (:imm8s :insert-imm8s))
     #x83 #o070 #x00)   
   (def-x8664-opcode cmpl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o070 #x00)
   (def-x8664-opcode rcmpl ((:anymem :insert-memory) (:imm32s :insert-imm32s))
     #x81 #o070 #x00)   

   (def-x8664-opcode cmpw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x39 #o300 #x00 #x66)
   (def-x8664-opcode rcmpw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x39 #o300 #x00 #x66)   
   (def-x8664-opcode cmpw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x3b #o000 #x00 #x66)
   (def-x8664-opcode rcmpw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x3b #o000 #x00 #x66)   
   (def-x8664-opcode cmpw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x39 #x00 #x00 #x66)
   (def-x8664-opcode rcmpw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x39 #x00 #x00 #x66)   
   (def-x8664-opcode cmpw ((:imm8s :insert-imm8s) (:reg16 :insert-modrm-rm))
     #x83 #o370 #x00 #x66)
   (def-x8664-opcode rcmpw ((:reg16 :insert-modrm-rm) (:imm8s :insert-imm8s))
     #x83 #o370 #x00 #x66)   
   (def-x8664-opcode cmpw ((:imm16 :insert-imm16) (:acc :insert-nothing))
     #x3d nil nil #x66)
   (def-x8664-opcode rcmpw ((:acc :insert-nothing) (:imm16 :insert-imm16))
     #x3d nil nil #x66)   
   (def-x8664-opcode cmpw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #x81 #o370 #x00 #x66)
   (def-x8664-opcode rcmpw ((:reg16 :insert-modrm-rm) (:imm16 :insert-imm16))
     #x81 #o370 #x00 #x66)   
   (def-x8664-opcode cmpw ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o070 #x00 #x66)
   (def-x8664-opcode rcmpw ((:anymem :insert-memory) (:imm8s :insert-imm8s))
     #x83 #o070 #x00 #x66)   
   (def-x8664-opcode cmpw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #x81 #o070 #x00 #x66)
   (def-x8664-opcode rcmpw ((:anymem :insert-memory) (:imm16 :insert-imm16))
     #x81 #o070 #x00 #x66)   

   (def-x8664-opcode cmpb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x38 #o300 #x00)
   (def-x8664-opcode rcmpb ((:reg8 :insert-modrm-rm) (:reg8 :insert-modrm-reg))
     #x38 #o300 #x00)
   (def-x8664-opcode cmpb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x3a #o000 #x00)
   (def-x8664-opcode rcmpb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x3a #o000 #x00)
   (def-x8664-opcode cmpb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x38 #x00 #x00)
   (def-x8664-opcode rcmpb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x38 #x00 #x00)   
   (def-x8664-opcode cmpb (((:imm8s :imm8) :insert-imm8s) (:acc :insert-nothing))
     #x3c nil nil)
   (def-x8664-opcode rcmpb ((:acc :insert-nothing) ((:imm8s :imm8) :insert-imm8s))
     #x3c nil nil)
   (def-x8664-opcode cmpb (((:imm8s :imm8) :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o370 #x00)
   (def-x8664-opcode rcmpb ((:reg8 :insert-modrm-rm) ((:imm8s :imm8) :insert-imm8s))
     #x80 #o370 #x00)
   (def-x8664-opcode cmpb (((:imm8s :imm8) :insert-imm8s) (:anymem :insert-memory))
     #x80 #o070 #x00)
   (def-x8664-opcode rcmpb ((:anymem :insert-memory) ((:imm8s :imm8) :insert-imm8s))
     #x80 #o070 #x00)

   ;; cmps
   (def-x8664-opcode cmpsq ()
     #xa7 nil #x48)

   (def-x8664-opcode cmpsl ()
     #xa7 nil nil)

   (def-x8664-opcode cmpsw ()
     #xa7 nil nil #x66)

   (def-x8664-opcode cmpsb ()
     #xa6 nil nil)

   ;; cmpxchg
   (def-x8664-opcode cmpxchgq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fb1 #o300 #x48)
   (def-x8664-opcode cmpxchgq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fb1 #o000 #x48)

   (def-x8664-opcode cmpxchgl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x0fb1 #o300 #x00)
   (def-x8664-opcode cmpxchgl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fb1 #o000 #x00)

   (def-x8664-opcode cmpxchgw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x0fb1 #o300 #x00 #x66)
   (def-x8664-opcode cmpxchgw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fb1 #o000 #x00 #x66)

   (def-x8664-opcode cmpxchgb ((:reg8 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x0fb0 #o300 #x00)
   (def-x8664-opcode cmpxchgb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fb0 #o000 #x00)

   ;; cpuid
   (def-x8664-opcode cpuid ()
     #x0fa2 nil nil)

   ;; cqto
   (def-x8664-opcode cqto ()
     #x99 nil #x48)

   ;; cwtd
   (def-x8664-opcode cwtd ()
     #x99 nil nil #x66)

   ;; cwtl
   (def-x8664-opcode cwtl ()
     #x98 nil nil)

   ;; dec (not the 1-byte form).  This exists on x8664, but gas doesn't
   ;; know that.
   (def-x8664-opcode decq ((:reg64 :insert-modrm-rm))
     #xff #o310 #x48)
   (def-x8664-opcode decq ((:anymem :insert-memory))
     #xff #o010 #x48)

   (def-x8664-opcode decl ((:reg32 :insert-modrm-rm))
     #xff #o310 #x00)
   (def-x8664-opcode decl ((:anymem :insert-memory))
     #xff #o010 #x00)

   (def-x8664-opcode decw ((:reg16 :insert-modrm-rm))
     #xff #o310 #x00 #x66)
   (def-x8664-opcode decw ((:anymem :insert-memory))
     #xff #o010 #x00 #x66)

   (def-x8664-opcode decb ((:reg8 :insert-modrm-rm))
     #xfe #o310 #x00)
   (def-x8664-opcode decb ((:anymem :insert-memory))
     #xfe #o010 #x00)

   ;; div
   (def-x8664-opcode divq ((:reg64 :insert-modrm-rm))
     #xf7 #o360 #x48)
   (def-x8664-opcode divq ((:anymem :insert-memory))
     #xf7 #o060 #x48)

   (def-x8664-opcode divl ((:reg32 :insert-modrm-rm))
     #xf7 #o360 #x00)
   (def-x8664-opcode divl ((:anymem :insert-memory))
     #xf7 #o060 #x00)

   (def-x8664-opcode divw ((:reg16 :insert-modrm-rm))
     #xf7 #o360 #x00 #x66)
   (def-x8664-opcode divw ((:anymem :insert-memory))
     #xf7 #o060 #x00 #x66)

   (def-x8664-opcode divb ((:reg8 :insert-modrm-rm))
     #xf6 #o360 #x00)
   (def-x8664-opcode divl ((:anymem :insert-memory))
     #xf6 #o060 #x00)

   ;; enter.

   (def-x8664-opcode enter ((:imm16 :insert-imm16) (:imm8 :insert-extra))
     #xc8 nil nil)

   ;; hlt
   (def-x8664-opcode hlt ()
     #xf4 nil nil)

   ;; idiv.  Note that GAS doesn't know about newer(?) idiv forms
   (def-x8664-opcode idivq ((:reg64 :insert-modrm-rm))
     #xf7 #o370 #x48)
   (def-x8664-opcode idivq ((:anymem :insert-memory))
     #xf7 #o070 #x48)

   (def-x8664-opcode idivl ((:reg32 :insert-modrm-rm))
     #xf7 #o370 #x00)
   (def-x8664-opcode idivl ((:anymem :insert-memory))
     #xf7 #o070 #x00)

   (def-x8664-opcode idivw ((:reg16 :insert-modrm-rm))
     #xf7 #o370 #x00 #x66)
   (def-x8664-opcode idivw ((:anymem :insert-memory))
     #xf7 #o070 #x00 #x66)

   (def-x8664-opcode idivb ((:reg8 :insert-modrm-rm))
     #xf6 #o370 #x00)
   (def-x8664-opcode idivl ((:anymem :insert-memory))
     #xf6 #o070 #x00)

   ;; imul
   (def-x8664-opcode imulq ((:reg64 :insert-modrm-rm))
     #xf7 #o350 #x48)
   (def-x8664-opcode imulq ((:anymem :insert-memory))
     #xf7 #o050 #x48)

   (def-x8664-opcode imulq ((:imm8s :insert-imm8s) (:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x6b #o300 #x48)
   (def-x8664-opcode imulq ((:imm8s :insert-imm8s) (:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x6b #o000 #x48)
   (def-x8664-opcode imulq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x69 #o300 #x48)
   (def-x8664-opcode imulq ((:imm32s :insert-imm32s) (:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x69 #o000 #x48)
   (def-x8664-opcode imulq ((:reg64 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0faf #o300 #x48)
   (def-x8664-opcode imulq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0faf #o000 #x48)   

   
   (def-x8664-opcode imull ((:reg32 :insert-modrm-rm))
     #xf7 #o350 #x00)
   (def-x8664-opcode imull ((:anymem :insert-memory))
     #xf7 #o050 #x00)

   (def-x8664-opcode imull ((:imm8s :insert-imm8s) (:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x6b #o300 #x00)
   (def-x8664-opcode imull ((:imm8s :insert-imm8s) (:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x6b #o000 #x00)
   (def-x8664-opcode imull ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x69 #o300 #x00)
   (def-x8664-opcode imull ((:imm32s :insert-imm32s) (:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x69 #o000 #x00)
   (def-x8664-opcode imull ((:reg32 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0faf #o300 #x00)
   (def-x8664-opcode imull ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0faf #o000 #x00)   
   
   (def-x8664-opcode imulw ((:reg16 :insert-modrm-rm))
     #xf7 #o350 #x00 #x66)
   (def-x8664-opcode imulw ((:anymem :insert-memory))
     #xf7 #o050 #x00 #x66)

   (def-x8664-opcode imulw ((:imm8s :insert-imm8s) (:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x6b #o300 #x00 #x66)
   (def-x8664-opcode imulw ((:imm8s :insert-imm8s) (:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x6b #o000 #x00 #x66)
   (def-x8664-opcode imulw ((:imm32s :insert-imm32s) (:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x69 #o300 #x00 #x66)
   (def-x8664-opcode imulw ((:imm32s :insert-imm32s) (:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x69 #o000 #x00 #x66)
   (def-x8664-opcode imulw ((:reg16 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0faf #o300 #x00 #x66)
   (def-x8664-opcode imulw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0faf #o000 #x00 #x66)   

   (def-x8664-opcode imulb ((:reg8 :insert-modrm-rm))
     #xf6 #o350 #x00)
   (def-x8664-opcode imulb ((:anymem :insert-memory))
     #xf6 #o050 #x00)

   ;; inc (but not the one-byte form) is available on x86-64.
   (def-x8664-opcode incq ((:reg64 :insert-modrm-rm))
     #xff #o300 #x48)
   (def-x8664-opcode incq ((:anymem :insert-memory))
     #xff #o000 #x48)

   (def-x8664-opcode incl ((:reg32 :insert-modrm-rm))
     #xff #o300 #x00)
   (def-x8664-opcode incl ((:anymem :insert-memory))
     #xff #o000 #x00)

   (def-x8664-opcode incw ((:reg16 :insert-modrm-rm))
     #xff #o300 #x00 #x66)
   (def-x8664-opcode incw ((:anymem :insert-memory))
     #xff #o000 #x00 #x66)

   (def-x8664-opcode incb ((:reg8 :insert-modrm-rm))
     #xfe #o300 #x00)
   (def-x8664-opcode incb ((:anymem :insert-memory))
     #xfe #o000 #x00)

   ;; int.  See also UUOs.
   (def-x8664-opcode int ((:imm8 :insert-imm8-for-int))
     #xcd nil nil)

   ;; Jcc.  Generate the short form here; maybe relax later.
   (def-x8664-opcode (jcc :jump) ((:imm8 :insert-cc) (:label :insert-label))
     #x70 nil nil)
   (def-x8664-opcode (jcc.pt :jump) ((:imm8 :insert-cc) (:label :insert-label))
     #x70 nil nil #x3e)
   (def-x8664-opcode (jcc.pn :jump) ((:imm8 :insert-cc) (:label :insert-label))
     #x70 nil nil #x2e)

   (def-x8664-opcode (jo :jump) ((:label :insert-label))
     #x70 nil nil)
   (def-x8664-opcode (jo.pt :jump) ((:label :insert-label))
     #x70 nil nil #x3e)
   (def-x8664-opcode (jo.pn :jump) ((:label :insert-label))
     #x70 nil nil #x2e)
   (def-x8664-opcode (jno :jump) ((:label :insert-label))
     #x71 nil nil)
   (def-x8664-opcode (jno.pt :jump) ((:label :insert-label))
     #x71 nil nil #x3e)
   (def-x8664-opcode (jno.pn :jump) ((:label :insert-label))
     #x71 nil nil #x2e)
   (def-x8664-opcode (jb :jump) ((:label :insert-label))
     #x72 nil nil)
   (def-x8664-opcode (jb.pt :jump) ((:label :insert-label))
     #x72 nil nil #x3e)
   (def-x8664-opcode (jb.pn :jump) ((:label :insert-label))
     #x72 nil nil #x2e)
   (def-x8664-opcode (jae :jump) ((:label :insert-label))
     #x73 nil nil)
   (def-x8664-opcode (jae.pt :jump) ((:label :insert-label))
     #x73 nil nil #x3e)
   (def-x8664-opcode (jae.pn :jump) ((:label :insert-label))
     #x73 nil nil #x2e)
   (def-x8664-opcode (je :jump) ((:label :insert-label))
     #x74 nil nil)
   (def-x8664-opcode (je.pt :jump) ((:label :insert-label))
     #x74 nil nil #x3e)
   (def-x8664-opcode (je.pn :jump) ((:label :insert-label))
     #x74 nil nil #x2e)
   (def-x8664-opcode (jz :jump) ((:label :insert-label))
     #x74 nil nil)
   (def-x8664-opcode (jz.pt :jump) ((:label :insert-label))
     #x74 nil nil #x3e)
   (def-x8664-opcode (jz.pn :jump) ((:label :insert-label))
     #x74 nil nil #x2e)
   (def-x8664-opcode (jne :jump) ((:label :insert-label))
     #x75 nil nil)
   (def-x8664-opcode (jne.pt :jump) ((:label :insert-label))
     #x75 nil nil #x3e)
   (def-x8664-opcode (jne.pn :jump) ((:label :insert-label))
     #x75 nil nil #x2e)
   (def-x8664-opcode (jnz :jump) ((:label :insert-label))
     #x75 nil nil)
   (def-x8664-opcode (jnz.pt :jump) ((:label :insert-label))
     #x75 nil nil #x3e)
   (def-x8664-opcode (jnz.pn :jump) ((:label :insert-label))
     #x75 nil nil #x2e)
   (def-x8664-opcode (jbe :jump) ((:label :insert-label))
     #x76 nil nil)
   (def-x8664-opcode (jbe.pt :jump) ((:label :insert-label))
     #x76 nil nil #x3e)
   (def-x8664-opcode (jbe.pn :jump) ((:label :insert-label))
     #x76 nil nil #x2e)
   (def-x8664-opcode (ja :jump) ((:label :insert-label))
     #x77 nil nil)
   (def-x8664-opcode (ja.pt :jump) ((:label :insert-label))
     #x77 nil nil #x3e)
   (def-x8664-opcode (ja.pn :jump) ((:label :insert-label))
     #x77 nil nil #x2e)
   (def-x8664-opcode (js :jump) ((:label :insert-label))
     #x78 nil nil)
   (def-x8664-opcode (js.pt :jump) ((:label :insert-label))
     #x78 nil nil #x3e)
   (def-x8664-opcode (js.pn :jump) ((:label :insert-label))
     #x78 nil nil #x2e)
   (def-x8664-opcode (jns :jump) ((:label :insert-label))
     #x79 nil nil)
   (def-x8664-opcode (jns.pt :jump) ((:label :insert-label))
     #x79 nil nil #x3e)
   (def-x8664-opcode (jns.pn :jump) ((:label :insert-label))
     #x79 nil nil #x2e)
   (def-x8664-opcode (jpe :jump) ((:label :insert-label))
     #x7a nil nil)
   (def-x8664-opcode (jpe.pt :jump) ((:label :insert-label))
     #x7a nil nil #x3e)
   (def-x8664-opcode (jpe.pn :jump) ((:label :insert-label))
     #x7a nil nil #x2e)
   (def-x8664-opcode (jpo :jump) ((:label :insert-label))
     #x7b nil nil)
   (def-x8664-opcode (jpo.pt :jump) ((:label :insert-label))
     #x7b nil nil #x3e)
   (def-x8664-opcode (jpo.pn :jump) ((:label :insert-label))
     #x7b nil nil #x2e)
   (def-x8664-opcode (jl :jump) ((:label :insert-label))
     #x7c nil nil)
   (def-x8664-opcode (jl.pt :jump) ((:label :insert-label))
     #x7c nil nil #x3e)
   (def-x8664-opcode (jl.pn :jump) ((:label :insert-label))
     #x7c nil nil #x2e)
   (def-x8664-opcode (jge :jump) ((:label :insert-label))
     #x7d nil nil)
   (def-x8664-opcode (jge.pt :jump) ((:label :insert-label))
     #x7d nil nil #x3e)
   (def-x8664-opcode (jge.pn :jump) ((:label :insert-label))
     #x7d nil nil #x2e)
   (def-x8664-opcode (jle :jump) ((:label :insert-label))
     #x7e nil nil)
   (def-x8664-opcode (jle.pt :jump) ((:label :insert-label))
     #x7e nil nil #x3e)
   (def-x8664-opcode (jle.pn :jump) ((:label :insert-label))
     #x7e nil nil #x2e)
   (def-x8664-opcode (jg :jump) ((:label :insert-label))
     #x7f nil nil)
   (def-x8664-opcode (jg.pt :jump) ((:label :insert-label))
     #x7f nil nil #x3e)
   (def-x8664-opcode (jg.pn :jump) ((:label :insert-label))
     #x7f nil nil #x2e)

   ;; jmp .  Translating the 8-bit pc-relative version to the 32-bit
   ;;        pc-relative version happens during relaxation.
   (def-x8664-opcode (jmp :jump) ((:label :insert-label))
     #xeb nil nil)

   (def-x8664-opcode jmp ((:reg64 :insert-modrm-rm))
     #xff #o340 #x0)

   (def-x8664-opcode jmp ((:anymem :insert-memory))
     #xff #o040 #x0)

   ;; lea
   (def-x8664-opcode leaq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x8d 0 #x48)

   (def-x8664-opcode leal ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x8d 0 #x00)

   (def-x8664-opcode leaw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x8d 0 #x00 #x66)

   ;; leave
   (def-x8664-opcode leave ()
     #xc9 nil nil)

   ;; lock
   (def-x8664-opcode lock ()
     #xf0 nil nil)

   ;; lods
   (def-x8664-opcode lodsq ()
     #xac nil #x48)

   (def-x8664-opcode lodsl ()
     #xac nil nil)

   ;; loop
   (def-x8664-opcode loopq ((:label :insert-label))
     #xe2 nil #x48)

   (def-x8664-opcode loopl ((:label :insert-label))
     #xe2 nil nil)

   (def-x8664-opcode loopzq ((:label :insert-label))
     #xe1 nil #x48)

   (def-x8664-opcode loopzl ((:label :insert-label))
     #xe1 nil nil)

   (def-x8664-opcode loopnzq ((:label :insert-label))
     #xe0 nil #x48)

   (def-x8664-opcode loopnzl ((:label :insert-label))
     #xe0 nil nil)

   ;; mov, including the MMX/XMM variants.
   (def-x8664-opcode movq ((:regmmx :insert-mmx-rm) (:regmmx :insert-mmx-reg))
     #x0f6f #o300 0)
   (def-x8664-opcode movq ((:regmmx :insert-mmx-reg) (:anymem :insert-memory))
     #x0f7f #o0 0)
   (def-x8664-opcode movq ((:anymem :insert-memory) (:regmmx :insert-mmx-reg))
     #x0f6f #o0 0)
   (def-x8664-opcode movq ((:regxmm :insert-xmm-reg) (:regxmm :insert-xmm-rm))
     #x0f7e #o300 0 #xf3)
   (def-x8664-opcode movq ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f7e #o000 0 #xf3)
   (def-x8664-opcode movq ((:regxmm :insert-xmm-reg) (:anymem :insert-memory))
     #x0fd6 #o000 0 #x66)

   (def-x8664-opcode movq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x89 #o300 #x48)
   (def-x8664-opcode movq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x8b #o0 #x48)
   (def-x8664-opcode movq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x89 #o0 #x48)
   (def-x8664-opcode movq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #xc7 #o300 #x48)
   (def-x8664-opcode movq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #xc7 #o000 #x48)
   (def-x8664-opcode movq ((:imm64 :insert-imm64) (:reg64 :insert-opcode-reg))
     #xb8 nil #x48)

   (def-x8664-opcode movl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x89 #o300 #x00)
   (def-x8664-opcode movl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x8b #o0 #x00)
   (def-x8664-opcode movl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x89 #o0 #x00)
   (def-x8664-opcode movl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #xc7 #o300 #x00)
   (def-x8664-opcode movl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #xc7 #o000 #x00)


   (def-x8664-opcode movw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x89 #o300 #x00 #x66)
   (def-x8664-opcode movw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x8b #o0 #x00  #x66)
   (def-x8664-opcode movw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x89 #o0 #x00 #x66)
   (def-x8664-opcode movw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #xc7 #o300 #x00 #x66)
   (def-x8664-opcode movw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #xc7 #o000 #x00 #x66)

   (def-x8664-opcode movb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x88 #o300 0)
   (def-x8664-opcode movb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x8a #o0 0)
   (def-x8664-opcode movb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x88 #o0 0)
   (def-x8664-opcode movb ((:imm8s :insert-imm8s) (:reg8 :insert-opcode-reg))
     #xb0 nil 0)
   (def-x8664-opcode movb ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #xc6 #o000 0)
  
   ;; movd
   (def-x8664-opcode movd ((:reg64 :insert-modrm-rm) (:regmmx :insert-mmx-reg))
     #x0f6e #o300 #x48)
   (def-x8664-opcode movd ((:reg32 :insert-modrm-rm) (:regmmx :insert-mmx-reg))
     #x0f6e #o300 0)
   (def-x8664-opcode movd ((:anymem :insert-memory) (:regmmx :insert-mmx-reg))
     #x0f6e #o000 0)
   (def-x8664-opcode movd ((:regmmx :insert-mmx-reg) (:reg64 :insert-modrm-rm))
     #x0f7e #o300 #x48)
   (def-x8664-opcode movd ((:regmmx :insert-mmx-reg) (:reg32 :insert-modrm-rm))
     #x0f7e #o300 #x0)
   (def-x8664-opcode movd ((:regmmx :insert-mmx-reg) (:anymem :insert-memory))
     #x0f7e #o000 #x0)

   (def-x8664-opcode movd ((:reg64 :insert-modrm-rm) (:regxmm :insert-xmm-reg))
     #x0f6e #o300 #x48 #x66)
   (def-x8664-opcode movd ((:reg32 :insert-modrm-rm) (:regxmm :insert-xmm-reg))
     #x0f6e #o300 0 #x66)
   (def-x8664-opcode movd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f6e #o000 0 #x66)
   (def-x8664-opcode movd ((:regxmm :insert-xmm-reg) (:reg64 :insert-modrm-rm))
     #x0f7e #o300 #x48 #x66)
   (def-x8664-opcode movd ((:regxmm :insert-xmm-reg) (:reg32 :insert-modrm-rm))
     #x0f7e #o300 #x0 #x66)
   (def-x8664-opcode movd ((:regxmm :insert-xmm-reg) (:anymem :insert-memory))
     #x0f7e #o000 #x0 #x66)

   ;; sign-extending mov
   (def-x8664-opcode movsbl ((:reg8 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0fbe #o300 0)
   (def-x8664-opcode movsbl ((:anymem :insert-memory)  (:reg32 :insert-modrm-reg))
     #x0fbe #o000 0)
   (def-x8664-opcode movsbw ((:reg8 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x0fbe #o300 0 #x66)
   (def-x8664-opcode movsbw ((:anymem :insert-memory) (:reg16 :insert-modrm-rm))
     #x0fbe #o300 0 #x66)
   (def-x8664-opcode movsbq ((:reg8 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0fbe #o300 #x48)
   (def-x8664-opcode movsbq ((:anymem :insert-memory)  (:reg64 :insert-modrm-reg))
     #x0fbe #o000 #x48)
   (def-x8664-opcode movswl ((:reg16 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0fbf #o300 0)
   (def-x8664-opcode movswl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0fbf #o000 0)
   (def-x8664-opcode movswq ((:reg16 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0fbf #o300 #x48)
   (def-x8664-opcode movswq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0fbf #o000 #x48)
   (def-x8664-opcode movslq ((:reg32 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x63 #o300 #x48)
   (def-x8664-opcode movslq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x63 #o000 #x48)

   ;; zero-extending MOVs
   (def-x8664-opcode movzbl ((:reg8 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0fb6 #o300 0)
   (def-x8664-opcode movzbl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0fb6 #o000 0)
   (def-x8664-opcode movzbw ((:reg8 :insert-modrm-rm) (:reg16 :insert-modrm-reg))
     #x0fb6 #o300 0 #x66)
   (def-x8664-opcode movzbw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0fb6 #o300 0 #x66)
   (def-x8664-opcode movzwl ((:reg16 :insert-modrm-rm) (:reg32 :insert-modrm-reg))
     #x0fb7 #o300 0)
   (def-x8664-opcode movzwl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0fb7 #o000 0)
   (def-x8664-opcode movzbq ((:reg8 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0fb6 #o300 #x48)
   (def-x8664-opcode movzbq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0fb6 #o000 #x48)
   (def-x8664-opcode movzwq ((:reg16 :insert-modrm-rm) (:reg64 :insert-modrm-reg))
     #x0fb7 #o300 #x48)
   (def-x8664-opcode movzwq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0fb7 #o000 #x48)

   ;; mul
   (def-x8664-opcode mulq ((:reg64 :insert-modrm-rm))
     #xf7 #o340 #x48)
   (def-x8664-opcode mulq ((:anymem :insert-memory))
     #xf7 #o040 #x48)

   (def-x8664-opcode mull ((:reg32 :insert-modrm-rm))
     #xf7 #o340 #x00)
   (def-x8664-opcode mull ((:anymem :insert-memory))
     #xf7 #o040 #x00)

   (def-x8664-opcode mulw ((:reg16 :insert-modrm-rm))
     #xf7 #o340 #x00 #x66)
   (def-x8664-opcode mulw ((:anymem :insert-memory))
     #xf7 #o040 #x00 #x66)

   (def-x8664-opcode mulb ((:reg8 :insert-modrm-rm))
     #xf6 #o340 #x00)
   (def-x8664-opcode mull ((:anymem :insert-memory))
     #xf6 #o040 #x00)

   ;; neg
   (def-x8664-opcode negq ((:reg64 :insert-modrm-rm))
     #xf7 #o330 #x48)
   (def-x8664-opcode negq ((:anymem :insert-memory))
     #xf7 #o030 #x48)

   (def-x8664-opcode negl ((:reg32 :insert-modrm-rm))
     #xf7 #o330 #x00)
   (def-x8664-opcode negl ((:anymem :insert-memory))
     #xf7 #o030 #x00)

   (def-x8664-opcode negw ((:reg16 :insert-modrm-rm))
     #xf7 #o330 #x00 #x66)
   (def-x8664-opcode negw ((:anymem :insert-memory))
     #xf7 #o030 #x00 #x66)

   (def-x8664-opcode negb ((:reg8 :insert-modrm-rm))
     #xf6 #o330 #x00)
   (def-x8664-opcode negb ((:anymem :insert-memory))
     #xf6 #o030 #x00)

   ;; nop
   (def-x8664-opcode nop ()
     #x90 nil nil)

   ;; not
   (def-x8664-opcode notq ((:reg64 :insert-modrm-rm))
     #xf7 #o320 #x48)
   (def-x8664-opcode notq ((:anymem :insert-memory))
     #xf7 #o020 #x48)
   (def-x8664-opcode notl ((:reg32 :insert-modrm-rm))
     #xf7 #o320 #x0)
   (def-x8664-opcode notl ((:anymem :insert-memory))
     #xf7 #o020 #x0)
   (def-x8664-opcode notw ((:reg16 :insert-modrm-rm))
     #xf7 #o320 #x0 #x66)
   (def-x8664-opcode notw ((:anymem :insert-memory))
     #xf7 #o020 #x0 #x66)
   (def-x8664-opcode notb ((:reg8 :insert-modrm-rm))
     #xf6 #o320 #x0)
   (def-x8664-opcode notb ((:anymem :insert-memory))
     #xf6 #o020 #x0)

   ;; or
   (def-x8664-opcode orq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x09 #o300 #x48)
   (def-x8664-opcode orq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0b #o000 #x48)
   (def-x8664-opcode orq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x09 #x00 #x48)
   (def-x8664-opcode orq ((:imm8s :insert-imm8s) (:reg64 :insert-modrm-rm))
     #x83 #o310 #x48)
   (def-x8664-opcode orq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x0d nil #x48)
   (def-x8664-opcode orq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #x81 #o310 #x48)
   (def-x8664-opcode orq ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o010 #x48)
   (def-x8664-opcode orq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o010 #x48)

   (def-x8664-opcode orl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x09 #o300 #x00)
   (def-x8664-opcode orl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0b #o000 #x00)
   (def-x8664-opcode orl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x09 #x00 #x00)
   (def-x8664-opcode orl ((:imm8s :insert-imm8s) (:reg32 :insert-modrm-rm))
     #x83 #o310 #x00)
   (def-x8664-opcode orl ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x0d nil nil)
   (def-x8664-opcode orl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #x81 #o310 #x00)
   (def-x8664-opcode orl ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o010 #x00)
   (def-x8664-opcode orl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o010 #x00)

   (def-x8664-opcode orw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x09 #o300 #x00 #x66)
   (def-x8664-opcode orw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x0b #o000 #x00 #x66)
   (def-x8664-opcode orw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x09 #x00 #x00 #x66)
   (def-x8664-opcode orw ((:imm8s :insert-imm8s) (:reg16 :insert-modrm-rm))
     #x83 #o310 #x00 #x66)
   (def-x8664-opcode orw ((:imm16 :insert-imm16) (:acc :insert-nothing))
     #x0d nil nil #x66)
   (def-x8664-opcode orw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #x81 #o310 #x00 #x66)
   (def-x8664-opcode orw ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o010 #x00 #x66)
   (def-x8664-opcode orw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #x81 #o010 #x00 #x66)

   (def-x8664-opcode orb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x08 #o300 #x00)
   (def-x8664-opcode orb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x0a #o000 #x00)
   (def-x8664-opcode orb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x08 #x00 #x00)
   (def-x8664-opcode orb ((:imm8s :insert-imm8s) (:acc :insert-nothing))
     #x0c nil nil)
   (def-x8664-opcode orb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o310 #x00)
   (def-x8664-opcode orb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o310 #x00)
   (def-x8664-opcode orb ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x80 #o010 #x00)

   ;; pop
   (def-x8664-opcode popq ((:reg64 :insert-opcode-reg))
     #x58 nil #x0)
   (def-x8664-opcode popq ((:anymem :insert-memory))
     #x8f #o000 #x0)
   (def-x8664-opcode popw ((:reg16 :insert-opcode-reg))
     #x58 nil #x0 #x66)
   (def-x8664-opcode popw ((:anymem :insert-memory))
     #x8f #o000 #x0 #x66)

   ;; popf
   (def-x8664-opcode popfq ()
     #x9d nil #x48)
   (def-x8664-opcode popfl ()
     #x9d nil nil)

   ;; push .  It's not clear how "pushw $imm16" is encoded.
   (def-x8664-opcode pushq ((:reg64 :insert-opcode-reg))
     #x50 nil #x0)
   (def-x8664-opcode pushq ((:anymem :insert-memory))
     #xff #o060 #x0)
   (def-x8664-opcode pushq ((:imm8s :insert-imm8s))
     #x6a nil nil)
   (def-x8664-opcode pushq ((:imm32s :insert-imm32s))
     #x68 nil nil)

   (def-x8664-opcode pushw ((:reg16 :insert-opcode-reg))
     #x50 nil 0 #x66)
   (def-x8664-opcode pushw ((:anymem :insert-memory))
     #xff #o060 #x0 #x66)

   ;; pushf
   (def-x8664-opcode pushfq ()
     #x9c nil nil)
   (def-x8664-opcode pushfw ()
     #x9c nil nil #x66)

   ;; rcl.  Note that the :ShiftCount operand type only matches %cl.
   (def-x8664-opcode rclq ((:imm1 :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd1 #o320 #x48)
   (def-x8664-opcode rclq ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o020 #x48)
   (def-x8664-opcode rclq ((:reg64 :insert-modrm-rm))
     #xd1 #o320 #x48)
   (def-x8664-opcode rclq ((:anymem :insert-memory))
     #xd1 #o020 #x48)
   (def-x8664-opcode rclq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #xc1 #o320 #x48)
   (def-x8664-opcode rclq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd3 #o320 #x48)
  
   (def-x8664-opcode rcll ((:imm1 :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd1 #o320 #x0)
   (def-x8664-opcode rcll ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o020 #x0)
   (def-x8664-opcode rcll ((:reg32 :insert-modrm-rm))
     #xd1 #o320 #x0)
   (def-x8664-opcode rcll ((:anymem :insert-memory))
     #xd1 #o020 #x0)
   (def-x8664-opcode rcll ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #xc1 #o320 #x0)
   (def-x8664-opcode rcll ((:shiftcount :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd3 #o320 #x0)

   (def-x8664-opcode rclw ((:imm1 :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd1 #o320 #x0 #x66)
   (def-x8664-opcode rclw ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o020 #x0 #x66)
   (def-x8664-opcode rclw ((:reg16 :insert-modrm-rm))
     #xd1 #o320 #x0 #x66)
   (def-x8664-opcode rclw ((:anymem :insert-memory))
     #xd1 #o020 #x0 #x66)
   (def-x8664-opcode rclw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #xc1 #o320 #x0 #x66)
   (def-x8664-opcode rclw ((:shiftcount :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd3 #o320 #x0 #x66)

   (def-x8664-opcode rclb ((:imm1 :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd0 #o320 #x0)
   (def-x8664-opcode rclb ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd0 #o020 #x0)
   (def-x8664-opcode rclb ((:reg8 :insert-modrm-rm))
     #xd0 #o320 #x0)
   (def-x8664-opcode rclb ((:anymem :insert-memory))
     #xd0 #o020 #x0)
   (def-x8664-opcode rclb ((:imm8 :insert-imm8) (:reg8 :insert-modrm-rm))
     #xc0 #o320 #x0)
   (def-x8664-opcode rclb ((:shiftcount :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd2 #o320 #x0)

   ;; rcr
   (def-x8664-opcode rcrq ((:imm1 :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd1 #o330 #x48)
   (def-x8664-opcode rcrq ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o030 #x48)
   (def-x8664-opcode rcrq ((:reg64 :insert-modrm-rm))
     #xd1 #o330 #x48)
   (def-x8664-opcode rcrq ((:anymem :insert-memory))
     #xd1 #o030 #x48)
   (def-x8664-opcode rcrq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #xc1 #o330 #x48)
   (def-x8664-opcode rcrq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd3 #o330 #x48)
  
   (def-x8664-opcode rcrl ((:imm1 :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd1 #o330 #x0)
   (def-x8664-opcode rcrl ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o030 #x0)
   (def-x8664-opcode rcrl ((:reg32 :insert-modrm-rm))
     #xd1 #o330 #x0)
   (def-x8664-opcode rcrl ((:anymem :insert-memory))
     #xd1 #o030 #x0)
   (def-x8664-opcode rcrl ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #xc1 #o330 #x0)
   (def-x8664-opcode rcrl ((:shiftcount :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd3 #o330 #x0)

   (def-x8664-opcode rcrw ((:imm1 :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd1 #o330 #x0 #x66)
   (def-x8664-opcode rcrw ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o030 #x0 #x66)
   (def-x8664-opcode rcrw ((:reg16 :insert-modrm-rm))
     #xd1 #o330 #x0 #x66)
   (def-x8664-opcode rcrw ((:anymem :insert-memory))
     #xd1 #o030 #x0 #x66)
   (def-x8664-opcode rcrw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #xc1 #o330 #x0 #x66)
   (def-x8664-opcode rcrw ((:shiftcount :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd3 #o330 #x0 #x66)

   (def-x8664-opcode rcrb ((:imm1 :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd0 #o330 #x0)
   (def-x8664-opcode rcrb ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd0 #o030 #x0)
   (def-x8664-opcode rcrb ((:reg8 :insert-modrm-rm))
     #xd0 #o330 #x0)
   (def-x8664-opcode rcrb ((:anymem :insert-memory))
     #xd0 #o030 #x0)
   (def-x8664-opcode rcrb ((:imm8 :insert-imm8) (:reg8 :insert-modrm-rm))
     #xc0 #o330 #x0)
   (def-x8664-opcode rcrb ((:shiftcount :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd2 #o330 #x0)

   ;; repe, repne.  These are really prefixes, that should
   ;; only be used before string instructions.
   (def-x8664-opcode repe ()
     #xf3 nil nil)

   (def-x8664-opcode repne ()
     #xf2 nil nil)

   ;; ret
   (def-x8664-opcode ret ()
     #xc3 nil nil)

   (def-x8664-opcode ret ((:imm16 :insert-imm16))
     #xc2 nil nil)

   ;; rol
   (def-x8664-opcode rolq ((:imm1 :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd1 #o300 #x48)
   (def-x8664-opcode rolq ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o000 #x48)
   (def-x8664-opcode rolq ((:reg64 :insert-modrm-rm))
     #xd1 #o300 #x48)
   (def-x8664-opcode rolq ((:anymem :insert-memory))
     #xd1 #o000 #x48)
   (def-x8664-opcode rolq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #xc1 #o300 #x48)
   (def-x8664-opcode rolq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd3 #o300 #x48)
  
   (def-x8664-opcode roll ((:imm1 :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd1 #o300 #x0)
   (def-x8664-opcode roll ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o000 #x0)
   (def-x8664-opcode roll ((:reg32 :insert-modrm-rm))
     #xd1 #o300 #x0)
   (def-x8664-opcode roll ((:anymem :insert-memory))
     #xd1 #o000 #x0)
   (def-x8664-opcode roll ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #xc1 #o300 #x0)
   (def-x8664-opcode roll ((:shiftcount :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd3 #o300 #x0)

   (def-x8664-opcode rolw ((:imm1 :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd1 #o300 #x0 #x66)
   (def-x8664-opcode rolw ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o000 #x0 #x66)
   (def-x8664-opcode rolw ((:reg16 :insert-modrm-rm))
     #xd1 #o300 #x0 #x66)
   (def-x8664-opcode rolw ((:anymem :insert-memory))
     #xd1 #o000 #x0 #x66)
   (def-x8664-opcode rolw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #xc1 #o300 #x0 #x66)
   (def-x8664-opcode rolw ((:shiftcount :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd3 #o300 #x0 #x66)

   (def-x8664-opcode rolb ((:imm1 :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd0 #o300 #x0)
   (def-x8664-opcode rolb ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd0 #o000 #x0)
   (def-x8664-opcode rolb ((:reg8 :insert-modrm-rm))
     #xd0 #o300 #x0)
   (def-x8664-opcode rolb ((:anymem :insert-memory))
     #xd0 #o000 #x0)
   (def-x8664-opcode rolb ((:imm8 :insert-imm8) (:reg8 :insert-modrm-rm))
     #xc0 #o300 #x0)
   (def-x8664-opcode rolb ((:shiftcount :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd2 #o300 #x0)

   ;; ror
   (def-x8664-opcode rorq ((:imm1 :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd1 #o310 #x48)
   (def-x8664-opcode rorq ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o010 #x48)
   (def-x8664-opcode rorq ((:reg64 :insert-modrm-rm))
     #xd1 #o310 #x48)
   (def-x8664-opcode rorq ((:anymem :insert-memory))
     #xd1 #o010 #x48)
   (def-x8664-opcode rorq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #xc1 #o310 #x48)
   (def-x8664-opcode rorq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd3 #o310 #x48)
  
   (def-x8664-opcode rorl ((:imm1 :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd1 #o310 #x0)
   (def-x8664-opcode rorl ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o010 #x0)
   (def-x8664-opcode rorl ((:reg32 :insert-modrm-rm))
     #xd1 #o310 #x0)
   (def-x8664-opcode rorl ((:anymem :insert-memory))
     #xd1 #o010 #x0)
   (def-x8664-opcode rorl ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #xc1 #o310 #x0)
   (def-x8664-opcode rorl ((:shiftcount :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd3 #o310 #x0)

   (def-x8664-opcode rorw ((:imm1 :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd1 #o310 #x0 #x66)
   (def-x8664-opcode rorw ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o010 #x0 #x66)
   (def-x8664-opcode rorw ((:reg16 :insert-modrm-rm))
     #xd1 #o310 #x0 #x66)
   (def-x8664-opcode rorw ((:anymem :insert-memory))
     #xd1 #o010 #x0 #x66)
   (def-x8664-opcode rorw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #xc1 #o310 #x0 #x66)
   (def-x8664-opcode rorw ((:shiftcount :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd3 #o310 #x0 #x66)

   (def-x8664-opcode rorb ((:imm1 :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd0 #o310 #x0)
   (def-x8664-opcode rorb ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd0 #o010 #x0)
   (def-x8664-opcode rorb ((:reg8 :insert-modrm-rm))
     #xd0 #o310 #x0)
   (def-x8664-opcode rorb ((:anymem :insert-memory))
     #xd0 #o010 #x0)
   (def-x8664-opcode rorb ((:imm8 :insert-imm8) (:reg8 :insert-modrm-rm))
     #xc0 #o310 #x0)
   (def-x8664-opcode rorb ((:shiftcount :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd2 #o310 #x0)

   ;; sar
   (def-x8664-opcode sarq ((:imm1 :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd1 #o370 #x48)
   (def-x8664-opcode sarq ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o070 #x48)
   (def-x8664-opcode sarq ((:reg64 :insert-modrm-rm))
     #xd1 #o370 #x48)
   (def-x8664-opcode sarq ((:anymem :insert-memory))
     #xd1 #o070 #x48)
   (def-x8664-opcode sarq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #xc1 #o370 #x48)
   (def-x8664-opcode sarq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd3 #o370 #x48)
  
   (def-x8664-opcode sarl ((:imm1 :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd1 #o370 #x0)
   (def-x8664-opcode sarl ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o070 #x0)
   (def-x8664-opcode sarl ((:reg32 :insert-modrm-rm))
     #xd1 #o370 #x0)
   (def-x8664-opcode sarl ((:anymem :insert-memory))
     #xd1 #o070 #x0)
   (def-x8664-opcode sarl ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #xc1 #o370 #x0)
   (def-x8664-opcode sarl ((:shiftcount :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd3 #o370 #x0)

   (def-x8664-opcode sarw ((:imm1 :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd1 #o370 #x0 #x66)
   (def-x8664-opcode sarw ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o070 #x0 #x66)
   (def-x8664-opcode sarw ((:reg16 :insert-modrm-rm))
     #xd1 #o370 #x0 #x66)
   (def-x8664-opcode sarw ((:anymem :insert-memory))
     #xd1 #o070 #x0 #x66)
   (def-x8664-opcode sarw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #xc1 #o370 #x0 #x66)
   (def-x8664-opcode sarw ((:shiftcount :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd3 #o370 #x0 #x66)

   (def-x8664-opcode sarb ((:imm1 :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd0 #o370 #x0)
   (def-x8664-opcode sarb ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd0 #o070 #x0)
   (def-x8664-opcode sarb ((:reg8 :insert-modrm-rm))
     #xd0 #o370 #x0)
   (def-x8664-opcode sarb ((:anymem :insert-memory))
     #xd0 #o070 #x0)
   (def-x8664-opcode sarb ((:imm8 :insert-imm8) (:reg8 :insert-modrm-rm))
     #xc0 #o370 #x0)
   (def-x8664-opcode sarb ((:shiftcount :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd2 #o370 #x0)

   ;; sbb
   (def-x8664-opcode sbbq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x19 #o300 #x48)
   (def-x8664-opcode sbbq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x1b #o000 #x48)
   (def-x8664-opcode sbbq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x19 #x00 #x48)
   (def-x8664-opcode sbbq ((:imm8s :insert-imm8s) (:reg64 :insert-modrm-rm))
     #x83 #o330 #x48)
   (def-x8664-opcode sbbq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x1d nil #x48)
   (def-x8664-opcode sbbq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #x81 #o330 #x48)
   (def-x8664-opcode sbbq ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o030 #x48)
   (def-x8664-opcode sbbq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o030 #x48)

   (def-x8664-opcode sbbl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x19 #o300 #x00)
   (def-x8664-opcode sbbl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x1b #o000 #x00)
   (def-x8664-opcode sbbl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x19 #x00 #x00)
   (def-x8664-opcode sbbl ((:imm8s :insert-imm8s) (:reg32 :insert-modrm-rm))
     #x83 #o330 #x00)
   (def-x8664-opcode sbbl ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x1d nil nil)
   (def-x8664-opcode sbbl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #x81 #o330 #x00)
   (def-x8664-opcode sbbl ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o030 #x00)
   (def-x8664-opcode sbbl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o030 #x00)

   (def-x8664-opcode sbbw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x19 #o300 #x00 #x66)
   (def-x8664-opcode sbbw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x1b #o000 #x00 #x66)
   (def-x8664-opcode sbbw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x19 #x00 #x00 #x66)
   (def-x8664-opcode sbbw ((:imm8s :insert-imm8s) (:reg16 :insert-modrm-rm))
     #x83 #o330 #x00 #x66)
   (def-x8664-opcode sbbw ((:imm16 :insert-imm16) (:acc :insert-nothing))
     #x1d nil nil #x66)
   (def-x8664-opcode sbbw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #x81 #o330 #x00 #x66)
   (def-x8664-opcode sbbw ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o030 #x00 #x66)
   (def-x8664-opcode sbbw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #x81 #o030 #x00 #x66)

   (def-x8664-opcode sbbb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x18 #o300 #x00)
   (def-x8664-opcode sbbb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x1a #o000 #x00)
   (def-x8664-opcode sbbb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x18 #x00 #x00)
   (def-x8664-opcode sbbb ((:imm8 :insert-imm8) (:acc :insert-nothing))
     #x1c nil nil)
   (def-x8664-opcode sbbb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o330 #x00)
   (def-x8664-opcode sbbb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o330 #x00)
   (def-x8664-opcode sbbb ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x80 #o030 #x00)

   ;; scas
   (def-x8664-opcode scasq ()
     #xaf nil #x48)
   (def-x8664-opcode scasl ()
     #xaf nil nil)
   (def-x8664-opcode scasw ()
     #xaf nil nil #x66)
   (def-x8664-opcode scasb ()
     #xae nil nil)


   ;; setcc
   (def-x8664-opcode setcc ((:imm8 :insert-cc) (:reg8 :insert-modrm-rm))
     #x0f90 #o300 0)     
   (def-x8664-opcode seto ((:reg8 :insert-modrm-rm))
     #x0f90 #o300 0)
   (def-x8664-opcode seto ((:anymem :insert-memory))
     #x0f90 #o000 0)
   (def-x8664-opcode setno ((:reg8 :insert-modrm-rm))
     #x0f91 #o300 0)
   (def-x8664-opcode setno ((:anymem :insert-memory))
     #x0f91 #o000 0)
   (def-x8664-opcode setb ((:reg8 :insert-modrm-rm))
     #x0f92 #o300 0)
   (def-x8664-opcode setb ((:anymem :insert-memory))
     #x0f92 #o000 0)
   (def-x8664-opcode setc ((:reg8 :insert-modrm-rm))
     #x0f92 #o300 0)
   (def-x8664-opcode setc ((:anymem :insert-memory))
     #x0f92 #o000 0)
   (def-x8664-opcode setae ((:reg8 :insert-modrm-rm))
     #x0f93 #o300 0)
   (def-x8664-opcode setae ((:anymem :insert-memory))
     #x0f93 #o000 0)
   (def-x8664-opcode sete ((:reg8 :insert-modrm-rm))
     #x0f94 #o300 0)
   (def-x8664-opcode sete ((:anymem :insert-memory))
     #x0f94 #o000 0)
   (def-x8664-opcode setne ((:reg8 :insert-modrm-rm))
     #x0f95 #o300 0)
   (def-x8664-opcode setne ((:anymem :insert-memory))
     #x0f95 #o000 0)
   (def-x8664-opcode setbe ((:reg8 :insert-modrm-rm))
     #x0f96 #o300 0)
   (def-x8664-opcode setbe ((:anymem :insert-memory))
     #x0f96 #o000 0)
   (def-x8664-opcode seta ((:reg8 :insert-modrm-rm))
     #x0f97 #o300 0)
   (def-x8664-opcode seta ((:anymem :insert-memory))
     #x0f97 #o000 0)
   (def-x8664-opcode sets ((:reg8 :insert-modrm-rm))
     #x0f98 #o300 0)
   (def-x8664-opcode sets ((:anymem :insert-memory))
     #x0f98 #o000 0)
   (def-x8664-opcode setns ((:reg8 :insert-modrm-rm))
     #x0f99 #o300 0)
   (def-x8664-opcode setns ((:anymem :insert-memory))
     #x0f99 #o000 0)
   (def-x8664-opcode setpe ((:reg8 :insert-modrm-rm))
     #x0f9a #o300 0)
   (def-x8664-opcode setpe ((:anymem :insert-memory))
     #x0f9a #o000 0)
   (def-x8664-opcode setpo ((:reg8 :insert-modrm-rm))
     #x0f9b #o300 0)
   (def-x8664-opcode setpo ((:anymem :insert-memory))
     #x0f9b #o000 0)
   (def-x8664-opcode setl ((:reg8 :insert-modrm-rm))
     #x0f9c #o300 0)
   (def-x8664-opcode setl ((:anymem :insert-memory))
     #x0f9c #o000 0)
   (def-x8664-opcode setge ((:reg8 :insert-modrm-rm))
     #x0f9d #o300 0)
   (def-x8664-opcode setge ((:anymem :insert-memory))
     #x0f9d #o000 0)
   (def-x8664-opcode setle ((:reg8 :insert-modrm-rm))
     #x0f9e #o300 0)
   (def-x8664-opcode setle ((:anymem :insert-memory))
     #x0f9e #o000 0)
   (def-x8664-opcode setg ((:reg8 :insert-modrm-rm))
     #x0f9f #o300 0)
   (def-x8664-opcode setg ((:anymem :insert-memory))
     #x0f9f #o000 0)

   ;; shl
   (def-x8664-opcode shlq ((:imm1 :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd1 #o340 #x48)
   (def-x8664-opcode shlq ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o040 #x48)
   (def-x8664-opcode shlq ((:reg64 :insert-modrm-rm))
     #xd1 #o340 #x48)
   (def-x8664-opcode shlq ((:anymem :insert-memory))
     #xd1 #o040 #x48)
   (def-x8664-opcode shlq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #xc1 #o340 #x48)
   (def-x8664-opcode shlq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd3 #o340 #x48)
  
   (def-x8664-opcode shll ((:imm1 :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd1 #o340 #x0)
   (def-x8664-opcode shll ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o040 #x0)
   (def-x8664-opcode shll ((:reg32 :insert-modrm-rm))
     #xd1 #o340 #x0)
   (def-x8664-opcode shll ((:anymem :insert-memory))
     #xd1 #o040 #x0)
   (def-x8664-opcode shll ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #xc1 #o340 #x0)
   (def-x8664-opcode shll ((:shiftcount :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd3 #o340 #x0)

   (def-x8664-opcode shlw ((:imm1 :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd1 #o340 #x0 #x66)
   (def-x8664-opcode shlw ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o040 #x0 #x66)
   (def-x8664-opcode shlw ((:reg16 :insert-modrm-rm))
     #xd1 #o340 #x0 #x66)
   (def-x8664-opcode shlw ((:anymem :insert-memory))
     #xd1 #o040 #x0 #x66)
   (def-x8664-opcode shlw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #xc1 #o340 #x0 #x66)
   (def-x8664-opcode shlw ((:shiftcount :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd3 #o340 #x0 #x66)

   (def-x8664-opcode shlb ((:imm1 :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd0 #o340 #x0)
   (def-x8664-opcode shlb ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd0 #o040 #x0)
   (def-x8664-opcode shlb ((:reg8 :insert-modrm-rm))
     #xd0 #o340 #x0)
   (def-x8664-opcode shlb ((:anymem :insert-memory))
     #xd0 #o040 #x0)
   (def-x8664-opcode shlb ((:imm8 :insert-imm8) (:reg8 :insert-modrm-rm))
     #xc0 #o340 #x0)
   (def-x8664-opcode shlb ((:shiftcount :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd2 #o340 #x0)

   ;; shld
   (def-x8664-opcode shldq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa4 #o300 #x48)
   (def-x8664-opcode shldq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa4 #o000 #x48)
   (def-x8664-opcode shldq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa5 #o300 #x48)
   (def-x8664-opcode shldq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa5 #o000 #x48)
   (def-x8664-opcode shldq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa5 #o300 #x48)
   (def-x8664-opcode shldq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa5 #o000 #x48)

   (def-x8664-opcode shldl ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa4 #o300 #x0)
   (def-x8664-opcode shldl ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa4 #o000 #x0)
   (def-x8664-opcode shldl ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa5 #o300 #x0)
   (def-x8664-opcode shldl ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa5 #o000 #x0)
   (def-x8664-opcode shldl ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa5 #o300 #x0)
   (def-x8664-opcode shldl ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa5 #o000 #x0)

   (def-x8664-opcode shldw ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa4 #o300 #x0 #x66)
   (def-x8664-opcode shldw ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa4 #o000 #x0 #x66)
   (def-x8664-opcode shldw ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa5 #o300 #x0 #x66)
   (def-x8664-opcode shldw ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa5 #o000 #x0 #x66)
   (def-x8664-opcode shldw ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fa5 #o300 #x0 #x66)
   (def-x8664-opcode shldw ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fa5 #o000 #x0 #x66)

   ;; shr
   (def-x8664-opcode shrq ((:imm1 :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd1 #o350 #x48)
   (def-x8664-opcode shrq ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o050 #x48)
   (def-x8664-opcode shrq ((:reg64 :insert-modrm-rm))
     #xd1 #o350 #x48)
   (def-x8664-opcode shrq ((:anymem :insert-memory))
     #xd1 #o050 #x48)
   (def-x8664-opcode shrq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-rm))
     #xc1 #o350 #x48)
   (def-x8664-opcode shrq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-rm))
     #xd3 #o350 #x48)
  
   (def-x8664-opcode shrl ((:imm1 :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd1 #o350 #x0)
   (def-x8664-opcode shrl ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o050 #x0)
   (def-x8664-opcode shrl ((:reg32 :insert-modrm-rm))
     #xd1 #o350 #x0)
   (def-x8664-opcode shrl ((:anymem :insert-memory))
     #xd1 #o050 #x0)
   (def-x8664-opcode shrl ((:imm8 :insert-imm8) (:reg32 :insert-modrm-rm))
     #xc1 #o350 #x0)
   (def-x8664-opcode shrl ((:shiftcount :insert-nothing) (:reg32 :insert-modrm-rm))
     #xd3 #o350 #x0)

   (def-x8664-opcode shrw ((:imm1 :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd1 #o350 #x0 #x66)
   (def-x8664-opcode shrw ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd1 #o050 #x0 #x66)
   (def-x8664-opcode shrw ((:reg16 :insert-modrm-rm))
     #xd1 #o350 #x0 #x66)
   (def-x8664-opcode shrw ((:anymem :insert-memory))
     #xd1 #o050 #x0 #x66)
   (def-x8664-opcode shrw ((:imm8 :insert-imm8) (:reg16 :insert-modrm-rm))
     #xc1 #o350 #x0 #x66)
   (def-x8664-opcode shrw ((:shiftcount :insert-nothing) (:reg16 :insert-modrm-rm))
     #xd3 #o350 #x0 #x66)

   (def-x8664-opcode shrb ((:imm1 :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd0 #o350 #x0)
   (def-x8664-opcode shrb ((:imm1 :insert-nothing) (:anymem :insert-memory))
     #xd0 #o050 #x0)
   (def-x8664-opcode shrb ((:reg8 :insert-modrm-rm))
     #xd0 #o350 #x0)
   (def-x8664-opcode shrb ((:anymem :insert-memory))
     #xd0 #o050 #x0)
   (def-x8664-opcode shrb ((:imm8 :insert-imm8) (:reg8 :insert-modrm-rm))
     #xc0 #o350 #x0)
   (def-x8664-opcode shrb ((:shiftcount :insert-nothing) (:reg8 :insert-modrm-rm))
     #xd2 #o350 #x0)

   ;; shrd
   (def-x8664-opcode shrdq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fac #o300 #x48)
   (def-x8664-opcode shrdq ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fac #o000 #x48)
   (def-x8664-opcode shrdq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fad #o300 #x48)
   (def-x8664-opcode shrdq ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fad #o000 #x48)
   (def-x8664-opcode shrdq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fad #o300 #x48)
   (def-x8664-opcode shrdq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fad #o000 #x48)

   (def-x8664-opcode shrdl ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fac #o300 #x0)
   (def-x8664-opcode shrdl ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fac #o000 #x0)
   (def-x8664-opcode shrdl ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fad #o300 #x0)
   (def-x8664-opcode shrdl ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fad #o000 #x0)
   (def-x8664-opcode shrdl ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fad #o300 #x0)
   (def-x8664-opcode shrdl ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fad #o000 #x0)

   (def-x8664-opcode shrdw ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fac #o300 #x0 #x66)
   (def-x8664-opcode shrdw ((:imm8 :insert-imm8) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fac #o000 #x0 #x66)
   (def-x8664-opcode shrdw ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fad #o300 #x0 #x66)
   (def-x8664-opcode shrdw ((:shiftcount :insert-nothing) (:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fad #o000 #x0 #x66)
   (def-x8664-opcode shrdw ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fad #o300 #x0 #x66)
   (def-x8664-opcode shrdw ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fad #o000 #x0 #x66)

   ;; stc
   (def-x8664-opcode stc ()
     #xf9 nil nil)

   ;; std
   (def-x8664-opcode std ()
     #xfd nil nil)

   ;; sub
   (def-x8664-opcode subq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x29 #o300 #x48)
   (def-x8664-opcode subq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x2b #o000 #x48)
   (def-x8664-opcode subq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x29 #x00 #x48)
   (def-x8664-opcode subq ((:imm8s :insert-imm8s) (:reg64 :insert-modrm-rm))
     #x83 #o350 #x48)
   (def-x8664-opcode subq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x2d nil #x48)
   (def-x8664-opcode subq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #x81 #o350 #x48)
   (def-x8664-opcode subq ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o050 #x48)
   (def-x8664-opcode subq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o050 #x48)

   (def-x8664-opcode subl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x29 #o300 #x00)
   (def-x8664-opcode subl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x2b #o000 #x00)
   (def-x8664-opcode subl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x29 #x00 #x00)
   (def-x8664-opcode subl ((:imm8s :insert-imm8s) (:reg32 :insert-modrm-rm))
     #x83 #o350 #x00)
   (def-x8664-opcode subl ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x2d nil nil)
   (def-x8664-opcode subl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #x81 #o350 #x00)
   (def-x8664-opcode subl ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o050 #x00)
   (def-x8664-opcode subl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o050 #x00)

   (def-x8664-opcode subw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x29 #o300 #x00 #x66)
   (def-x8664-opcode subw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x2b #o000 #x00 #x66)
   (def-x8664-opcode subw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x29 #x00 #x00 #x66)
   (def-x8664-opcode subw ((:imm8s :insert-imm8s) (:reg16 :insert-modrm-rm))
     #x83 #o350 #x00 #x66)
   (def-x8664-opcode subw ((:imm16 :insert-imm16) (:acc :insert-nothing))
     #x2d nil nil #x66)
   (def-x8664-opcode subw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #x81 #o350 #x00 #x66)
   (def-x8664-opcode subw ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o050 #x00 #x66)
   (def-x8664-opcode subw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #x81 #o050 #x00 #x66)

   (def-x8664-opcode subb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x28 #o300 #x00)
   (def-x8664-opcode subb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x2a #o000 #x00)
   (def-x8664-opcode subb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x2a #x00 #x00)
   (def-x8664-opcode subb ((:imm8s :insert-imm8s) (:acc :insert-nothing))
     #x2c nil nil)
   (def-x8664-opcode subb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o350 #x00)
   (def-x8664-opcode subb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o350 #x00)
   (def-x8664-opcode subb ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x80 #o050 #x00)

   ;; syscall
   (def-x8664-opcode syscall ()
     #x0f0f nil nil)

   ;; test
   (def-x8664-opcode testq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x85 #o300 #x48)
   (def-x8664-opcode testq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x85 #o000 #x48)
   (def-x8664-opcode testq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x87 #o000 #x48)
   (def-x8664-opcode testq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #xa9 nil #x48)
   (def-x8664-opcode testq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #xf7 #o300 #x48)
   (def-x8664-opcode testq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #xf7 #o000 #x48)

   (def-x8664-opcode testl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x85 #o300 #x00)
   (def-x8664-opcode testl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x85 #o000 #x00)
   (def-x8664-opcode testl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x87 #o000 #x00)
   (def-x8664-opcode testl ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #xa9 nil #x00)
   (def-x8664-opcode testl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #xf7 #o300 #x00)
   (def-x8664-opcode testl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #xf7 #o000 #x00)


   (def-x8664-opcode testw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x85 #o300 #x00 #x66)
   (def-x8664-opcode testw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x85 #o000 #x00 #x66)
   (def-x8664-opcode testw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x87 #o000 #x00 #x66)
   (def-x8664-opcode testw ((:imm16 :insert-imm16) (:acc :insert-nothing))
     #xa9 nil #x00 #x66)
   (def-x8664-opcode testw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #xf7 #o300 #x00 #x66)
   (def-x8664-opcode testw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #xf7 #o000 #x00 #x66)


   (def-x8664-opcode testb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x84 #o300 #x00)
   (def-x8664-opcode testb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x84 #o000 #x00)
   (def-x8664-opcode testb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x86 #o000 #x00)
   (def-x8664-opcode testb ((:imm8s :insert-imm8s) (:acc :insert-nothing))
     #xa8 nil #x00)
   (def-x8664-opcode testb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #xf6 #o300 #x00)
   (def-x8664-opcode testb ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #xf6 #o000 #x00)

   ;; ud2a (not to be confused with all of the other undefined/accidental
   ;; instructions) is "officially undefined".
   (def-x8664-opcode ud2a ()
     #x0f0b nil nil)

   (def-x8664-opcode ud2b ()
     #x0fb9 nil nil)

   ;; xadd
   (def-x8664-opcode xaddq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x0fc1 #o300 #x48)
   (def-x8664-opcode xaddq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fc1 #o000 #x48)

   (def-x8664-opcode xaddl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x0fc1 #o300 #x00)
   (def-x8664-opcode xaddl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fc1 #o000 #x00)

   (def-x8664-opcode xaddw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x0fc1 #o300 #x00 #x66)
   (def-x8664-opcode xaddw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fc1 #o000 #x00 #x66)

   (def-x8664-opcode xaddb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x0fc0 #o300 #x00)
   (def-x8664-opcode xaddb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x0fc0 #o000 #x00)

   ;; xchg
   ;; Allegedly, using the opcode #x9x to implement "(xchg (% eax) (% eax))"
   ;; doesn't zero-extend eax to rax on x86-64.  (So don't special-case
   ;; :acc as source or destination, and use #x86 and a modrm byte in all cases.)
   (def-x8664-opcode xchgq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x87 #o300 #x48)
   (def-x8664-opcode xchgq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x87 #o000 #x48)
   (def-x8664-opcode xchgq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x89 #o000 #x48)

   (def-x8664-opcode xchgl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x87 #o300 #x00)
   (def-x8664-opcode xchgl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x87 #o000 #x00)
   (def-x8664-opcode xchgl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x89 #o000 #x00)

   (def-x8664-opcode xchgw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x87 #o300 #x00 #x66)
   (def-x8664-opcode xchgw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x87 #o000 #x00 #x66)
   (def-x8664-opcode xchgw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x89 #o000 #x00 #x66)

   (def-x8664-opcode xchgl ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x86 #o300 #x00)
   (def-x8664-opcode xchgl ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x86 #o000 #x00)
   (def-x8664-opcode xchgl ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x88 #o000 #x00)

   ;; xlat

   (def-x8664-opcode xlatb ()
     #xd7 nil nil)

   ;; xor
   (def-x8664-opcode xorq ((:reg64 :insert-modrm-reg) (:reg64 :insert-modrm-rm))
     #x31 #o300 #x48)
   (def-x8664-opcode xorq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x33 #o000 #x48)
   (def-x8664-opcode xorq ((:reg64 :insert-modrm-reg) (:anymem :insert-memory))
     #x31 #x00 #x48)
   (def-x8664-opcode xorq ((:imm8s :insert-imm8s) (:reg64 :insert-modrm-rm))
     #x83 #o360 #x48)
   (def-x8664-opcode xorq ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x35 nil #x48)
   (def-x8664-opcode xorq ((:imm32s :insert-imm32s) (:reg64 :insert-modrm-rm))
     #x81 #o360 #x48)
   (def-x8664-opcode xorq ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o060 #x48)
   (def-x8664-opcode xorq ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o060 #x48)

   (def-x8664-opcode xorl ((:reg32 :insert-modrm-reg) (:reg32 :insert-modrm-rm))
     #x31 #o300 #x00)
   (def-x8664-opcode xorl ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x33 #o000 #x00)
   (def-x8664-opcode xorl ((:reg32 :insert-modrm-reg) (:anymem :insert-memory))
     #x31 #x00 #x00)
   (def-x8664-opcode xorl ((:imm8s :insert-imm8s) (:reg32 :insert-modrm-rm))
     #x83 #o360 #x00)
   (def-x8664-opcode xorl ((:imm32s :insert-imm32s) (:acc :insert-nothing))
     #x35 nil nil)
   (def-x8664-opcode xorl ((:imm32s :insert-imm32s) (:reg32 :insert-modrm-rm))
     #x81 #o360 #x00)
   (def-x8664-opcode xorl ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o060 #x00)
   (def-x8664-opcode xorl ((:imm32s :insert-imm32s) (:anymem :insert-memory))
     #x81 #o060 #x00)

   (def-x8664-opcode xorw ((:reg16 :insert-modrm-reg) (:reg16 :insert-modrm-rm))
     #x31 #o300 #x00 #x66)
   (def-x8664-opcode xorw ((:anymem :insert-memory) (:reg16 :insert-modrm-reg))
     #x33 #o000 #x00 #x66)
   (def-x8664-opcode xorw ((:reg16 :insert-modrm-reg) (:anymem :insert-memory))
     #x31 #x00 #x00 #x66)
   (def-x8664-opcode xorw ((:imm8s :insert-imm8s) (:reg16 :insert-modrm-rm))
     #x83 #o360 #x00 #x66)
   (def-x8664-opcode xorw ((:imm16 :insert-imm16) (:acc :insert-nothing))
     #x35 nil nil #x66)
   (def-x8664-opcode xorw ((:imm16 :insert-imm16) (:reg16 :insert-modrm-rm))
     #x81 #o360 #x00 #x66)
   (def-x8664-opcode xorw ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x83 #o060 #x00 #x66)
   (def-x8664-opcode xorw ((:imm16 :insert-imm16) (:anymem :insert-memory))
     #x81 #o060 #x00 #x66)

   (def-x8664-opcode xorb ((:reg8 :insert-modrm-reg) (:reg8 :insert-modrm-rm))
     #x30 #o300 #x00)
   (def-x8664-opcode xorb ((:anymem :insert-memory) (:reg8 :insert-modrm-reg))
     #x32 #o000 #x00)
   (def-x8664-opcode xorb ((:reg8 :insert-modrm-reg) (:anymem :insert-memory))
     #x30 #x00 #x00)
   (def-x8664-opcode xorb ((:imm8s :insert-imm8s) (:acc :insert-nothing))
     #x34 nil nil)
   (def-x8664-opcode xorb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o360 #x00)
   (def-x8664-opcode xorb ((:imm8s :insert-imm8s) (:reg8 :insert-modrm-rm))
     #x80 #o360 #x00)
   (def-x8664-opcode xorb ((:imm8s :insert-imm8s) (:anymem :insert-memory))
     #x80 #o060 #x00)

   ;; fxsave
   (def-x8664-opcode fxsaveq ((:anymem :insert-memory))
     #x0fae #o000 0)

   ;; fxrstor
   (def-x8664-opcode fxrstor ((:anymem :insert-memory))
     #x0fae #o010 0)

   ;; clflush
   (def-x8664-opcode clflush ((:anymem :insert-memory))
     #x0fae #o070 0)

   ;; lfence
   (def-x8664-opcode lfence ()
     #x0fae #xe8 nil)

   ;; mfence
   (def-x8664-opcode mfence ()
     #x0fae #xf0 nil)
   
   ;; pause
   (def-x8664-opcode pause ()
     #xf390 nil nil)

   ;; I don't want to have to define all mmx/sse/sse2 instructions at the
   ;; moment, but it wouldn't hurt to define those that the lisp is
   ;; likely to use.

   ;; Useful mmx/sse2 instructions, other than movd/movq:

   ;; emms
   (def-x8664-opcode emms ()
     #x0f77 nil nil)

   ;; addsd
   (def-x8664-opcode addsd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f58 #o000 #x0 #xf2)
   (def-x8664-opcode addsd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f58 #o300 #x0 #xf2)
   
   ;; addss
   (def-x8664-opcode addss ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f58 #o000 #x0 #xf3)
   (def-x8664-opcode addss ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f58 #o300 #x0 #xf3)

   ;; subsd
   (def-x8664-opcode subsd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f5c #o000 #x0 #xf2)
   (def-x8664-opcode subsd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f5c #o300 #x0 #xf2)

   ;; subss
   (def-x8664-opcode subss ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f5c #o000 #x0 #xf3)
   (def-x8664-opcode subss ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f5c #o300 #x0 #xf3)

   ;; movapd
   (def-x8664-opcode movapd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f28 #o300 #x0 #x66)
   (def-x8664-opcode movapd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f28 #o000 #x0 #x66)
   (def-x8664-opcode movapd ((:regxmm :insert-xmm-reg) (:anymem :insert-memory))
     #x0f29 #o000 #x0 #x66)
   
   ;; mulsd
   (def-x8664-opcode mulsd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f59 #o000 #x0 #xf2)
   (def-x8664-opcode mulsd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f59 #o300 #x0 #xf2)

   ;; mulss
   (def-x8664-opcode mulss ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f59 #o000 #x0 #xf3)
   (def-x8664-opcode mulss ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f59 #o300 #x0 #xf3)

   ;; divsd
   (def-x8664-opcode divsd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f5e #o000 #x0 #xf2)
   (def-x8664-opcode divsd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f5e #o300 #x0 #xf2)

   ;; divss
   (def-x8664-opcode divss ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f5e #o000 #x0 #xf3)
   (def-x8664-opcode divss ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f5e #o300 #x0 #xf3)


   ;; sqrtsd
   (def-x8664-opcode sqrtsd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f51 #o000 #x0 #xf2)
   (def-x8664-opcode sqrtsd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f51 #o300 #x0 #xf2)

   ;; sqrtss
   (def-x8664-opcode sqrtss ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f51 #o000 #x0 #xf3)
   (def-x8664-opcode sqrtss ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f51 #o300 #x0 #xf3)
   
   ;; comisd
   (def-x8664-opcode comisd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2f #o000 #x0 #x66)
   (def-x8664-opcode comisd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f2f #o300 #x0 #x66)

   ;; ucomisd
   (def-x8664-opcode ucomisd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2e #o000 #x0 #x66)
   (def-x8664-opcode comisd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f2e #o300 #x0 u#x66)

   
      ;; comiss
   (def-x8664-opcode comiss ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2f #o000 #x0)
   (def-x8664-opcode comiss ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f2f #o300 #x0)

   ;; ucomiss
   (def-x8664-opcode ucomiss ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2e #o000 #x0)
   (def-x8664-opcode ucomiss ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f2e #o300 #x0)

   ;; movsd
   (def-x8664-opcode movsd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f10 #o300 #x0 #xf2)
   (def-x8664-opcode movsd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f10 #o300 #x0 #xf2)
   (def-x8664-opcode movsd ((:regxmm :insert-xmm-reg) (:anymem :insert-memory))
     #x0f11 #o000 #x0 #xf2)

   

   ;; movss
   (def-x8664-opcode movss ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f10 #o300 #x0 #xf3)
   (def-x8664-opcode movss ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f10 #o300 #x0 #xf3)
   (def-x8664-opcode movss ((:regxmm :insert-xmm-reg) (:anymem :insert-memory))
     #x0f11 #o000 #x0 #xf3)

   
   ;;; cvtsd2si.  This does rounding (as opposed to truncation).
   (def-x8664-opcode cvtsd2siq ((:regxmm :insert-xmm-rm) (:reg64 :insert-modrm-reg))
     #x0f2d #o300 #x48 #xf2)
   (def-x8664-opcode cvtsd2siq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f2d #o000 #x48 #xf2)
   (def-x8664-opcode cvtsd2sil ((:regxmm :insert-xmm-rm) (:reg32 :insert-modrm-reg))
     #x0f2d #o300 #x00 #xf2)
   (def-x8664-opcode cvtsd2sil ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f2d #o000 #x00 #xf2)

   ;;; cvtss2si.  This does rounding (as opposed to truncation).
   (def-x8664-opcode cvtss2siq ((:regxmm :insert-xmm-rm) (:reg64 :insert-modrm-reg))
     #x0f2d #o300 #x48 #xf3)
   (def-x8664-opcode cvtss2siq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f2d #o000 #x48 #xf3)
   (def-x8664-opcode cvtss2sil ((:regxmm :insert-xmm-rm) (:reg32 :insert-modrm-reg))
     #x0f2d #o300 #x00 #xf3)
   (def-x8664-opcode cvtss2sil ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f2d #o000 #x00 #xf3)
   
   ;;; cvttsd2si.  This does truncation (as opposed to rounding).
   (def-x8664-opcode cvttsd2siq ((:regxmm :insert-xmm-rm) (:reg64 :insert-modrm-reg))
     #x0f2c #o300 #x48 #xf2)
   (def-x8664-opcode cvttsd2siq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f2c #o000 #x48 #xf2)
   (def-x8664-opcode cvttsd2sil ((:regxmm :insert-xmm-rm) (:reg32 :insert-modrm-reg))
     #x0f2c #o300 #x00 #xf2)
   (def-x8664-opcode cvtsd2sil ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f2c #o000 #x00 #xf2)

   ;;; cvttss2si.  This does truncation (as opposed to rounding).
   (def-x8664-opcode cvttss2siq ((:regxmm :insert-xmm-rm) (:reg64 :insert-modrm-reg))
     #x0f2c #o300 #x48 #xf3)
   (def-x8664-opcode cvttss2siq ((:anymem :insert-memory) (:reg64 :insert-modrm-reg))
     #x0f2c #o000 #x48 #xf3)
   (def-x8664-opcode cvttss2sil ((:regxmm :insert-xmm-rm) (:reg32 :insert-modrm-reg))
     #x0f2c #o300 #x00 #xf3)
   (def-x8664-opcode cvttss2sil ((:anymem :insert-memory) (:reg32 :insert-modrm-reg))
     #x0f2c #o000 #x00 #xf3)

   ;; cvtsi2sd
   (def-x8664-opcode cvtsi2sdq ((:reg64 :insert-modrm-rm) (:regxmm :insert-xmm-reg))
     #x0f2a #o300 #x48 #xf2)
   (def-x8664-opcode cvtsi2sdq ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2a #o000 #x48 #xf2)
   (def-x8664-opcode cvtsi2sdl ((:reg32 :insert-modrm-rm) (:regxmm :insert-xmm-reg))
     #x0f2a #o300 #x00 #xf2)
   (def-x8664-opcode cvtsi2sdl ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2a #o000 #x00 #xf2)
   
   ;; cvtsd2ss
   (def-x8664-opcode cvtsd2ss ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f5a #o300 #x0 #xf2)
   (def-x8664-opcode cvtsd2ss ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f5a #o000 #x0 #xf2)

   ;; cvtsi2sd
   (def-x8664-opcode cvtsi2sdq ((:reg64 :insert-modrm-rm) (:regxmm :insert-xmm-reg))
     #x0f2a #o300 #x48 #xf2)
   (def-x8664-opcode cvtsi2sdq ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2a #o000 #x48 #xf2)
   (def-x8664-opcode cvtsi2sdl ((:reg32 :insert-modrm-rm) (:regxmm :insert-xmm-reg))
     #x0f2a #o300 #x00 #xf2)
   (def-x8664-opcode cvtsi2sdl ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2a #o000 #x00 #xf2)

   ;; cvtsi2ss
   (def-x8664-opcode cvtsi2ssq ((:reg64 :insert-modrm-rm) (:regxmm :insert-xmm-reg))
     #x0f2a #o300 #x48 #xf3)
   (def-x8664-opcode cvtsi2ssq ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2a #o000 #x48 #xf3)
   (def-x8664-opcode cvtsi2ssl ((:reg32 :insert-modrm-rm) (:regxmm :insert-xmm-reg))
     #x0f2a #o300 #x00 #xf3)
   (def-x8664-opcode cvtsi2ssl ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f2a #o000 #x00 #xf3)

   ;;; cvtss2sd
   (def-x8664-opcode cvtss2sd ((:regxmm :insert-xmm-rm) (:regxmm :insert-xmm-reg))
     #x0f5a #o300 #x0 #xf3)
   (def-x8664-opcode cvtss2sd ((:anymem :insert-memory) (:regxmm :insert-xmm-reg))
     #x0f5a #o000 #x0 #xf3)
   
   ;; pand
   (def-x8664-opcode pand ((:regmmx :insert-mmx-rm) (:regmmx :insert-mmx-reg))
     #x0fdb #o300 #x0)
   (def-x8664-opcode pand ((:anymem :insert-memory) (:regmmx :insert-mmx-reg))
     #x0fdb #o000 #x0)
   (def-x8664-opcode pand ((:regxmm :insert-modrm-rm) (:regxmm :insert-modrm-reg))
     #x0fef #o300 #x0 #x66)
   (def-x8664-opcode pand ((:anymem :insert-memory) (:regxmm :insert-modrm-reg))
     #x0fdb #o000 #x0 #x66)
   
   ;; pandn
   (def-x8664-opcode pandn ((:regmmx :insert-mmx-rm) (:regmmx :insert-mmx-reg))
     #x0fdf #o300 #x0)
   (def-x8664-opcode pandn ((:anymem :insert-memory) (:regmmx :insert-mmx-reg))
     #x0fdf #o000 #x0)
   (def-x8664-opcode pandn ((:regxmm :insert-modrm-rm) (:regxmm :insert-modrm-reg))
     #x0fdf #o300 #x0 #x66)
   (def-x8664-opcode pandn ((:anymem :insert-memory) (:regxmm :insert-modrm-reg))
     #x0fdf #o000 #x0 #x66)

   ;; por
   (def-x8664-opcode por ((:regmmx :insert-mmx-rm) (:regmmx :insert-mmx-reg))
     #x0feb #o300 #x0)
   (def-x8664-opcode por ((:anymem :insert-memory) (:regmmx :insert-mmx-reg))
     #x0feb #o000 #x0)
   (def-x8664-opcode por ((:regxmm :insert-modrm-rm) (:regxmm :insert-modrm-reg))
     #x0feb #o300 #x0 #x66)
   (def-x8664-opcode por ((:anymem :insert-memory) (:regxmm :insert-modrm-reg))
     #x0feb #o000 #x0 #x66)

   ;; pxor
   (def-x8664-opcode pxor ((:regmmx :insert-mmx-rm) (:regmmx :insert-mmx-reg))
     #x0fef #o300 #x0)
   (def-x8664-opcode pxor ((:anymem :insert-memory) (:regmmx :insert-mmx-reg))
     #x0fef #o000 #x0)
   (def-x8664-opcode pxor ((:regxmm :insert-modrm-rm) (:regxmm :insert-modrm-reg))
     #x0fef #o300 #x0 #x66)
   (def-x8664-opcode pxor ((:anymem :insert-memory) (:regxmm :insert-modrm-reg))
     #x0fef #o000 #x0 #x66)

   ;; psllq 
   (def-x8664-opcode psllq ((:regmmx :insert-mmx-rm) (:regmmx :insert-mmx-reg))
     #x0ff3 #o300 #x0)
   (def-x8664-opcode psllq ((:anymem :insert-memory) (:regmmx :insert-mmx-reg))
     #x0ff3 #o000 #x0)
   (def-x8664-opcode psllq ((:regxmm :insert-modrm-rm) (:regxmm :insert-modrm-reg))
     #x0ff3 #o300 #x0 #x66)
   (def-x8664-opcode psllq ((:anymem :insert-memory) (:regxmm :insert-modrm-reg))
     #x0ff3 #o000 #x0 #x66)
   (def-x8664-opcode psllq ((:imm8 :insert-imm8) (:regxmm :insert-xmm-rm))
     #x0f73 #o360 #o0 #x66)

   ;; psllw
   
   ;; pslld

   ;; pslldq
   (def-x8664-opcode pslldq ((:imm8 :insert-imm8) (:regxmm :insert-xmm-rm))
     #x0f73 #o370 #x0 #x66)
   
   ;; psrlq 
   (def-x8664-opcode psrlq ((:regmmx :insert-mmx-rm) (:regmmx :insert-mmx-reg))
     #x0fd3 #o300 #x0)
   (def-x8664-opcode psrlq ((:anymem :insert-memory) (:regmmx :insert-mmx-reg))
     #x0fd3 #o000 #x0)
   (def-x8664-opcode psrlq ((:regxmm :insert-modrm-rm) (:regxmm :insert-modrm-reg))
     #x0fd3 #o300 #x0 #x66)
   (def-x8664-opcode psrlq ((:anymem :insert-memory) (:regxmm :insert-modrm-reg))
     #x0fd3 #o000 #x0 #x66)
   (def-x8664-opcode psrlq ((:imm8 :insert-imm8) (:regxmm :insert-xmm-rm))
     #x0f73 #o320 #o0 #x66)

   ;; psrld

   ;; psrldq
   (def-x8664-opcode psrldq ((:imm8 :insert-imm8) (:regxmm :insert-xmm-rm))
     #x0f73 #o330 #x0 #x66)
   
   ;; psrlw
   
   ;;; End of list of useful mmx instructions
   (def-x8664-opcode ldmxcsr ((:anymem :insert-memory))
     #x0fae #o020 nil)

   (def-x8664-opcode stmxcsr ((:anymem :insert-memory))
     #x0fae #o030 nil)

   ;; UUOs.  Expect lots more, some of which may take pseudo-operands.
   (def-x8664-opcode uuo-error-slot-unbound ((:reg64 :insert-opcode-reg4)
                                             (:reg64 :insert-reg4-pseudo-rm-high)
                                             (:reg64 :insert-reg4-pseudo-rm-low))
     #xcd70 0 nil)

   ;;; DON'T use #xcd8x: doing so will make Mach angry and confused.
   
   (def-x8664-opcode uuo-error-unbound ((:reg64 :insert-opcode-reg4))
     #xcd90 nil 0)

   (def-x8664-opcode uuo-error-udf ((:reg64 :insert-opcode-reg4))
     #xcda0 nil 0)
   
   (def-x8664-opcode uuo-error-reg-not-type ((:reg64 :insert-opcode-reg4) (:imm8 :insert-imm8))
     #xcdb0 nil 0)
   
   (def-x8664-opcode uuo-error-too-few-args ()
     #xcdc0 nil nil)
   (def-x8664-opcode uuo-error-too-many-args ()
     #xcdc1 nil nil)
   (def-x8664-opcode uuo-error-wrong-number-of-args ()
     #xcdc2 nil nil)
   (def-x8664-opcode uuo-error-array-rank ((:reg64 :insert-reg4-pseudo-rm-high)
                                           (:reg64 :insert-reg4-pseudo-rm-low))
     #xcdc3 0 nil)

   (def-x8664-opcode uuo-gc-trap ()
     #xcdc4 nil nil)
   (def-x8664-opcode uuo-alloc ()
     #xcdc5 nil nil)
   (def-x8664-opcode uuo-error-not-callable ()
     #xcdc6 nil nil)
   (def-x8664-opcode uuo-error-udf-call ()
     #xcdc7 nil nil)

   (def-x8664-opcode uuo-error-vector-bounds ((:reg64 :insert-reg4-pseudo-rm-high) (:reg64 :insert-reg4-pseudo-rm-low))
     #xcdc8 0 nil)

   (def-x8664-opcode uuo-error-call-macro-or-special-operator ()
     #xcdc9 nil nil)

   (def-x8664-opcode uuo-error-debug-trap ()
     #xcdca nil nil)

   (def-x8664-opcode uuo-error-array-bounds ((:reg64 :insert-reg4-pseudo-rm-high) (:reg64 :insert-reg4-pseudo-rm-low))
     #xcdcb 0 nil)

   (def-x8664-opcode uuo-error-eep-unresolved ((:reg64 :insert-reg4-pseudo-rm-high)
                                               (:reg64 :insert-reg4-pseudo-rm-low))
     #xcdcc 0 nil)

   (def-x8664-opcode uuo-error-debug-trap-with-string ()
     #xcdcd nil nil)
   
   (def-x8664-opcode uuo-error-reg-not-tag ((:reg64 :insert-opcode-reg4) (:imm8 :insert-imm8))
     #xcdd0 nil 0)
   (def-x8664-opcode uuo-error-reg-not-list ((:reg64 :insert-opcode-reg4))
     #xcde0 nil 0)
   (def-x8664-opcode uuo-error-reg-not-fixnum ((:reg64 :insert-opcode-reg4))
     #xcdf0 nil 0)

   ))

(dotimes (i (length *x8664-opcode-templates*))
  (setf (x86-opcode-template-ordinal (svref *x8664-opcode-templates* i)) i))
  
    
      
(defparameter *x86-32-opcode-template-lists*
  (make-hash-table :test #'equalp))


(defparameter *x86-64-opcode-template-lists*
  (make-hash-table :test #'equalp))


(defun initialize-x86-opcode-templates ()
  (flet ((setup-templates-hash (hash templates)
           (clrhash hash)
           (do* ((i (1- (length templates)) (1- i)))
                ((< i 0) hash)
             (declare (fixnum i))
             (let* ((template (svref templates i))
                    (name (x86-opcode-template-mnemonic template)))
               (push template (gethash name hash))))))
    #+notyet
    (setup-templates-hash
     *x86-32-opcode-template-lists*
     *x8632-opcode-templates*)
    (setup-templates-hash
     *x86-64-opcode-template-lists*
     *x8664-opcode-templates*)
    #+x8664-target
    (when (fboundp 'ccl::fixup-x86-vinsn-templates)
      (ccl::fixup-x86-vinsn-templates
       (ccl::backend-p2-vinsn-templates ccl::*target-backend*)
       *x86-64-opcode-template-lists*))
    t))

(defparameter *x86-opcode-template-lists* ())

(defvar *x8632-registers* (make-hash-table :test #'equalp))
(defvar *x8664-registers* (make-hash-table :test #'equalp))
(defvar *x86-registers* nil)

(defparameter *x86-32-operand-insert-functions*
  #(tbd))

(defparameter *x86-64-operand-insert-functions*
  #(insert-nothing
    insert-modrm-reg
    insert-modrm-rm
    insert-memory
    insert-opcode-reg
    insert-opcode-reg4
    insert-cc
    insert-label
    insert-imm8-for-int
    insert-extra
    insert-imm8
    insert-imm8s
    insert-imm16
    insert-imm32s
    insert-imm32
    insert-imm64
    insert-mmx-reg
    insert-mmx-rm
    insert-xmm-reg
    insert-xmm-rm
    insert-reg4-pseudo-rm-high
    insert-reg4-pseudo-rm-low))

(defvar *x86-operand-insert-functions* ())

(defun setup-x86-assembler (&optional (cpu :x86-64))
  (initialize-x86-opcode-templates)
  (ecase cpu
    (:x86-32 (setq *x86-opcode-template-lists*
                   *x86-32-opcode-template-lists*
                   *x86-registers* *x8632-registers*
                   *x86-operand-insert-functions*
                   *x86-32-operand-insert-functions*
                   ))
    (:x86-64 (setq *x86-opcode-template-lists*
                   *x86-64-opcode-template-lists*
                   *x86-registers* *x8664-registers*
                   *x86-operand-insert-functions*
                   *x86-64-operand-insert-functions*)))
  t)

(setup-x86-assembler :x86-64)



;;; 386 register table.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant +REGNAM-AL+ 1) ; Entry in i386-regtab.
(defconstant +REGNAM-AX+ 25)
(defconstant +REGNAM-EAX+ 41)

(defvar *x86-regtab*
  (vector
   ;; Make %st first as we test for it.
   (make-reg-entry :reg-name "st"
                   :reg-type (encode-operand-type :FloatReg :floatacc)
                   :reg-flags 0
                   :reg-num 0 )
   ;; 8 bit regs
   (make-reg-entry :reg-name "al"
                   :reg-type (encode-operand-type :Reg8 :Acc)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "cl"
                   :reg-type (encode-operand-type :Reg8 :ShiftCount)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "dl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "bl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "ah"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "ch"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "dh"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "bh"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "axl"
                   :reg-type (encode-operand-type :Reg8 :Acc)
                   :reg-flags +RegRex64+
                   :reg-num 0 ) ; Must be in the "al + 8" slot.
   (make-reg-entry :reg-name "cxl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags +RegRex64+
                   :reg-num 1)
   (make-reg-entry :reg-name "dxl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags +RegRex64+
                   :reg-num 2)
   (make-reg-entry :reg-name "bxl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags +RegRex64+
                   :reg-num 3)
   (make-reg-entry :reg-name "spl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags +RegRex64+
                   :reg-num 4)
   (make-reg-entry :reg-name "bpl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags +RegRex64+
                   :reg-num 5)
   (make-reg-entry :reg-name "sil"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags +RegRex64+
                   :reg-num 6)
   (make-reg-entry :reg-name "dil"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags +RegRex64+
                   :reg-num 7)
   (make-reg-entry :reg-name "r8b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior +RegRex64+ +RegRex+)
                   :reg-num 0 )
   (make-reg-entry :reg-name "r9b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior +RegRex64+ +RegRex+)
                   :reg-num 1)
   (make-reg-entry :reg-name "r10b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior +RegRex64+ +RegRex+)
                   :reg-num 2)
   (make-reg-entry :reg-name "r11b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior +RegRex64+ +RegRex+)
                   :reg-num 3)
   (make-reg-entry :reg-name "r12b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior +RegRex64+ +RegRex+)
                   :reg-num 4)
   (make-reg-entry :reg-name "r13b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior +RegRex64+ +RegRex+)
                   :reg-num 5)
   (make-reg-entry :reg-name "r14b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior +RegRex64+ +RegRex+)
                   :reg-num 6)
   (make-reg-entry :reg-name "r15b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior +RegRex64+ +RegRex+)
                   :reg-num 7)
   ;; 16 bit regs
   (make-reg-entry :reg-name "ax"
                   :reg-type (encode-operand-type :Reg16 :Acc)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "cx"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "dx"
                   :reg-type (encode-operand-type :Reg16 :InOutPortReg)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "bx"
                   :reg-type (encode-operand-type :Reg16 :BaseIndex)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "sp"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "bp"
                   :reg-type (encode-operand-type :Reg16 :BaseIndex)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "si"
                   :reg-type (encode-operand-type :Reg16 :BaseIndex)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "di"
                   :reg-type (encode-operand-type :Reg16 :BaseIndex)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "r8w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags +RegRex+
                   :reg-num 0 )
   (make-reg-entry :reg-name "r9w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags +RegRex+
                   :reg-num 1)
   (make-reg-entry :reg-name "r10w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags +RegRex+
                   :reg-num 2)
   (make-reg-entry :reg-name "r11w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags +RegRex+
                   :reg-num 3)
   (make-reg-entry :reg-name "r12w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags +RegRex+
                   :reg-num 4)
   (make-reg-entry :reg-name "r13w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags +RegRex+
                   :reg-num 5)
   (make-reg-entry :reg-name "r14w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags +RegRex+
                   :reg-num 6)
   (make-reg-entry :reg-name "r15w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags +RegRex+
                   :reg-num 7)
        ; 32 bit regs
   (make-reg-entry :reg-name "eax"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex :Acc)
                   :reg-flags 0
                   :reg-num 0 ) ; Must be in ax + 16 slot.
   (make-reg-entry :reg-name "ecx"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "edx"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "ebx"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "esp"
                   :reg-type (encode-operand-type :Reg32)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "ebp"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "esi"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "edi"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "r8d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 0 )
   (make-reg-entry :reg-name "r9d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 1)
   (make-reg-entry :reg-name "r10d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 2)
   (make-reg-entry :reg-name "r11d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 3)
   (make-reg-entry :reg-name "r12d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 4)
   (make-reg-entry :reg-name "r13d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 5)
   (make-reg-entry :reg-name "r14d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 6)
   (make-reg-entry :reg-name "r15d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 7)
   (make-reg-entry :reg-name "rax"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex :Acc)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "rcx"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "rdx"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "rbx"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "rsp"
                   :reg-type (encode-operand-type :Reg64)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "rbp"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "rsi"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "rdi"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "r8"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 0 )
   (make-reg-entry :reg-name "r9"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 1)
   (make-reg-entry :reg-name "r10"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 2)
   (make-reg-entry :reg-name "r11"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 3)
   (make-reg-entry :reg-name "r12"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 4)
   (make-reg-entry :reg-name "r13"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 5)
   (make-reg-entry :reg-name "r14"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 6)
   (make-reg-entry :reg-name "r15"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags +RegRex+
                   :reg-num 7)
        ; Segment registers.
   (make-reg-entry :reg-name "es"
                   :reg-type (encode-operand-type :SReg2)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "cs"
                   :reg-type (encode-operand-type :SReg2)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "ss"
                   :reg-type (encode-operand-type :SReg2)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "ds"
                   :reg-type (encode-operand-type :SReg2)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "fs"
                   :reg-type (encode-operand-type :SReg3)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "gs"
                   :reg-type (encode-operand-type :SReg3)
                   :reg-flags 0
                   :reg-num 5)
   ;; Control registers.
   (make-reg-entry :reg-name "cr0"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "cr1"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "cr2"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "cr3"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "cr4"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "cr5"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "cr6"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "cr7"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "cr8"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags +RegRex+
                   :reg-num 0 )
   (make-reg-entry :reg-name "cr9"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags +RegRex+
                   :reg-num 1)
   (make-reg-entry :reg-name "cr10"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags +RegRex+
                   :reg-num 2)
   (make-reg-entry :reg-name "cr11"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags +RegRex+
                   :reg-num 3)
   (make-reg-entry :reg-name "cr12"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags +RegRex+
                   :reg-num 4)
   (make-reg-entry :reg-name "cr13"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags +RegRex+
                   :reg-num 5)
   (make-reg-entry :reg-name "cr14"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags +RegRex+
                   :reg-num 6)
   (make-reg-entry :reg-name "cr15"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags +RegRex+
                   :reg-num 7)
   ;; Debug registers.
   (make-reg-entry :reg-name "db0"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "db1"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "db2"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "db3"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "db4"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "db5"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "db6"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "db7"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "db8"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 0 )
   (make-reg-entry :reg-name "db9"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 1)
   (make-reg-entry :reg-name "db10"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 2)
   (make-reg-entry :reg-name "db11"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 3)
   (make-reg-entry :reg-name "db12"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 4)
   (make-reg-entry :reg-name "db13"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 5)
   (make-reg-entry :reg-name "db14"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 6)
   (make-reg-entry :reg-name "db15"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 7)
   (make-reg-entry :reg-name "dr0"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "dr1"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "dr2"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "dr3"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "dr4"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "dr5"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "dr6"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "dr7"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "dr8"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 0 )
   (make-reg-entry :reg-name "dr9"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 1)
   (make-reg-entry :reg-name "dr10"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 2)
   (make-reg-entry :reg-name "dr11"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 3)
   (make-reg-entry :reg-name "dr12"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 4)
   (make-reg-entry :reg-name "dr13"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 5)
   (make-reg-entry :reg-name "dr14"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 6)
   (make-reg-entry :reg-name "dr15"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags +RegRex+
                   :reg-num 7)
   ;; Test registers.
   (make-reg-entry :reg-name "tr0"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "tr1"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "tr2"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "tr3"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "tr4"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "tr5"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "tr6"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "tr7"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 7)
   ;; MMX and simd registers.
   (make-reg-entry :reg-name "mm0"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "mm1"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "mm2"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "mm3"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "mm4"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "mm5"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "mm6"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "mm7"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "xmm0"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "xmm1"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "xmm2"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "xmm3"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "xmm4"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "xmm5"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "xmm6"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "xmm7"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "xmm8"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags +RegRex+
                   :reg-num 0 )
   (make-reg-entry :reg-name "xmm9"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags +RegRex+
                   :reg-num 1)
   (make-reg-entry :reg-name "xmm10"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags +RegRex+
                   :reg-num 2)
   (make-reg-entry :reg-name "xmm11"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags +RegRex+
                   :reg-num 3)
   (make-reg-entry :reg-name "xmm12"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags +RegRex+
                   :reg-num 4)
   (make-reg-entry :reg-name "xmm13"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags +RegRex+
                   :reg-num 5)
   (make-reg-entry :reg-name "xmm14"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags +RegRex+
                   :reg-num 6)
   (make-reg-entry :reg-name "xmm15"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags +RegRex+
                   :reg-num 7)
   ;; No type will make this register rejected for all purposes except
   ;; for addressing. This saves creating one extra type for RIP.
   (make-reg-entry :reg-name "rip"
                   :reg-type (encode-operand-type :BaseIndex)
                   :reg-flags 0
                   :reg-num 0 )
   ))

(defvar *x86-float-regs*
  (vector
   (make-reg-entry :reg-name "st[0]"
                   :reg-type (encode-operand-type :FloatReg :FloatAcc)
                   :reg-flags 0
                   :reg-num 0)
   (make-reg-entry :reg-name "st[1]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "st[2]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "st[3]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "st[4]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "st[5]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "st[6]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "st[7]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 7)))


;;; Segment stuff.
(defvar *cs-segment-register* (make-seg-entry :seg-name "cs" :seg-prefix #x23))
(defvar *ds-segment-register* (make-seg-entry :seg-name "ds" :seg-prefix #x3e))
(defvar *ss-segment-register* (make-seg-entry :seg-name "ss" :seg-prefix #x36))
(defvar *es-segment-register* (make-seg-entry :seg-name "es" :seg-prefix #x26))
(defvar *fs-segment-register* (make-seg-entry :seg-name "fs" :seg-prefix #x64))
(defvar *gs-segment-register* (make-seg-entry :seg-name "gs" :seg-prefix #x65))

(defvar *x86-seg-entries*
  (vector *es-segment-register*
          *cs-segment-register*
          *ss-segment-register*
          *ds-segment-register*
          *fs-segment-register*
          *gs-segment-register*))





(defun init-x86-registers ()
  (flet ((hash-registers (vector hash 64p)
           (dotimes (i (length vector))
             (let* ((entry (svref vector i)))
               (if (or 64p (not (logtest (reg-entry-reg-flags entry) +regrex+)))
                 (setf (gethash (reg-entry-reg-name entry) hash)
                       entry))))))
    (hash-registers *x86-regtab* *x8632-registers* nil)
    (hash-registers *x86-float-regs* *x8632-registers* nil)
    (hash-registers *x86-regtab* *x8664-registers* t)
    (hash-registers *x86-float-regs* *x8664-registers* t)))

)

(init-x86-registers)



(defstruct x86-operand
  (type ))

(defstruct (x86-immediate-operand (:include x86-operand))
  ;; The "value" of an immediate operand may be an expression (that we
  ;; have to do some sort of delayed evaluation on.)  It could just be
  ;; a lisp form (that we call EVAL on), but there might be scoping or
  ;; similar issues in that case.
  value)

(defstruct (x86-register-operand (:include x86-operand))
  entry                                 ;the reg-entry
)

(defstruct (x86-label-operand (:include x86-operand))
  label)


(defstruct (x86-memory-operand (:include x86-operand))
  ;; Any of these fields can be null.  Some combinations of fields -
  ;; like a segment register or scale factor by itself - make no
  ;; sense.
  seg                                   ; a segment register
  disp                                  ; a signed displacement added to base
  base                                  ; a GPR
  index                                 ; another GPR
  scale                                 ; scale factor, multiplied with index
  )


(defun insert-nothing (instruction operand)
  (declare (ignore instruction operand)))

;;; Insert a 3-bit register value derived from OPERAND in INSN's modrm.reg
;;; field.  If the register requires REX addressing, set the REX.R bit
;;; in the instruction's rex-prefix.  If either the modrm or rex-prefix
;;; fields of the instruction are NIL, we're very confused; check for
;;; that explicitly until this code matures a bit.

(defun insert-modrm-reg-entry (instruction entry)
  (let* ((reg-num (reg-entry-reg-num entry))
         (flags (reg-entry-reg-flags entry))
         (need-rex.r (logtest +regrex+ flags)))
    (setf (x86-instruction-modrm-byte instruction)
          (dpb reg-num (byte 3 3)
               (need-modrm-byte instruction)))
    (when need-rex.r
      (setf (x86-instruction-rex-prefix instruction)
            (logior +rex-extx+ (need-rex-prefix instruction))))
    (when (logtest +regrex64+ flags)
      (setf (x86-instruction-rex-prefix instruction)
            (logior #x80 (need-rex-prefix instruction))))))



(defun insert-modrm-reg (instruction operand)
  (insert-modrm-reg-entry instruction (x86-register-operand-entry operand)))

(defun insert-mmx-reg-entry (instruction entry)
  (let* ((reg-num (reg-entry-reg-num entry)))
    (setf (x86-instruction-modrm-byte instruction)
          (dpb reg-num (byte 3 3)
               (need-modrm-byte instruction)))))

(defun insert-mmx-reg (instruction operand)
  (insert-mmx-reg-entry instruction (x86-register-operand-entry operand)))

(defun insert-xmm-reg (instruction operand)
  (insert-modrm-reg instruction operand))

(defun insert-xmm-rm (instruction operand)
  (insert-modrm-rm instruction operand))

(defun insert-opcode-reg-entry (instruction entry)
  (let* ((reg-num (reg-entry-reg-num entry))
         (flags (reg-entry-reg-flags entry))
         (need-rex.b (logtest +regrex+ flags)))
    (setf (x86-instruction-base-opcode instruction)
          (dpb reg-num (byte 3 0)
               (x86-instruction-base-opcode instruction)))
    (when need-rex.b
      (setf (x86-instruction-rex-prefix instruction)
            (logior +rex-extz+ (need-rex-prefix instruction))))
    (when (logtest +regrex64+ flags)
      (setf (x86-instruction-rex-prefix instruction)
            (logior #x80 (need-rex-prefix instruction))))))

(defun insert-opcode-reg (instruction operand)
  (insert-opcode-reg-entry instruction (x86-register-operand-entry operand)))

;;; Insert a 4-bit register number in the low 4 bits of the opcode.
;;; (This is only used in synthetic instructions, like some UUOs.)

(defun insert-opcode-reg4-entry (instruction entry)
  (let* ((reg-num (reg-entry-reg-num entry))
         (xreg-num (logior reg-num
                           (if (logtest +regrex+ (reg-entry-reg-flags entry))
                             #x08
                             #x00))))
    (setf (x86-instruction-base-opcode instruction)
          (dpb xreg-num (byte 4 0)
               (x86-instruction-base-opcode instruction)))))

(defun insert-opcode-reg4 (instruction operand)
  (insert-opcode-reg4-entry instruction (x86-register-operand-entry operand)))


(defun insert-reg4-pseudo-rm-high-entry (instruction entry)
  (let* ((reg-num (reg-entry-reg-num entry))
         (xreg-num (logior reg-num
                           (if (logtest +regrex+ (reg-entry-reg-flags entry))
                             #x08
                             #x00))))
    (setf (x86-instruction-modrm-byte instruction)
          (dpb xreg-num (byte 4 4)
               (x86-instruction-modrm-byte instruction)))))

(defun insert-reg4-pseudo-rm-high (instruction operand)
  (insert-reg4-pseudo-rm-high-entry instruction (x86-register-operand-entry operand)))


(defun insert-reg4-pseudo-rm-low-entry (instruction entry)
  (let* ((reg-num (reg-entry-reg-num entry))
         (xreg-num (logior reg-num
                           (if (logtest +regrex+ (reg-entry-reg-flags entry))
                             #x08
                             #x00))))
    (setf (x86-instruction-modrm-byte instruction)
          (dpb xreg-num (byte 4 0)
               (x86-instruction-modrm-byte instruction)))))

(defun insert-reg4-pseudo-rm-low (instruction operand)
  (insert-reg4-pseudo-rm-low-entry instruction (x86-register-operand-entry operand)))

;;; Insert a 3-bit register value derived from OPERAND in INSN's modrm.rm
;;; field.  If the register requires REX addressing, set the REX.B bit
;;; in the instruction's rex-prefix.  If either the modrm or rex-prefix
;;; fields of the instruction are NIL, we're very confused; check for
;;; that explicitly until this code matures a bit.

(defun insert-modrm-rm-entry (instruction entry)
  (let* ((reg-num (reg-entry-reg-num entry))
         (flags (reg-entry-reg-flags entry))
         (need-rex.b (logtest +regrex+ flags)))
    (setf (x86-instruction-modrm-byte instruction)
          (dpb reg-num (byte 3 0) (need-modrm-byte instruction)))
    (when need-rex.b
      (setf (x86-instruction-rex-prefix instruction)
            (logior +rex-extz+ (need-rex-prefix instruction))))
    (when (logtest +regrex64+ flags)
      (setf (x86-instruction-rex-prefix instruction)
            (logior #x80 (need-rex-prefix instruction))))))

(defun insert-modrm-rm (instruction operand)
  (insert-modrm-rm-entry instruction (x86-register-operand-entry operand)))

(defun insert-mmx-rm-entry (instruction entry)
  (let* ((reg-num (reg-entry-reg-num entry)))
    (setf (x86-instruction-modrm-byte instruction)
          (dpb reg-num (byte 3 0) (need-modrm-byte instruction)))))

(defun insert-mmx-rm (instruction operand)
  (insert-mmx-rm-entry instruction (x86-register-operand-entry operand)))

(defun insert-imm64 (instruction operand)
  (setf (x86-immediate-operand-type operand)
        (encode-operand-type :imm64))
  (setf (x86-instruction-imm instruction) operand))

(defun insert-imm32s (instruction operand)
  (setf (x86-immediate-operand-type operand)
        (encode-operand-type :imm32s))
  (setf (x86-instruction-imm instruction) operand))

(defun insert-imm32 (instruction operand)
  (setf (x86-immediate-operand-type operand)
        (encode-operand-type :imm32))
  (setf (x86-instruction-imm instruction) operand))

(defun insert-imm16 (instruction operand)
  (setf (x86-immediate-operand-type operand)
        (encode-operand-type :imm16))
  (setf (x86-instruction-imm instruction) operand))

(defun insert-imm8 (instruction operand)
  (setf (x86-immediate-operand-type operand)
        (encode-operand-type :imm8))
  (setf (x86-instruction-imm instruction) operand))

(defun insert-imm8s (instruction operand)
  (setf (x86-immediate-operand-type operand)
        (encode-operand-type :imm8s))
  (setf (x86-instruction-imm instruction) operand))

(defun insert-imm8-for-int (instruction operand)
  (declare (ftype (function (t) t) ccl::early-x86-lap-expression-value))
  (let* ((expr (x86-immediate-operand-value operand))
         (value (ccl::early-x86-lap-expression-value expr)))
    (if (eql value 3)
      (setf (x86-instruction-base-opcode instruction)
            +int3-opcode+)
      (insert-imm8 instruction operand))))

(defun insert-label (instruction operand)
  (setf (x86-instruction-extra instruction)
        (x86::x86-label-operand-label operand)))

(defparameter *x8664-register-entries*
  (flet ((register-entry (name)
           (let* ((r (gethash name *x86-registers*)))
             (unless r (error "unknown register ~s" name))
             r)))
    (vector
     ;; 64-bit general-purpose registers
     (register-entry "rax")
     (register-entry "rcx")
     (register-entry "rdx")
     (register-entry "rbx")
     (register-entry "rsp")
     (register-entry "rbp")
     (register-entry "rsi")
     (register-entry "rdi")
     (register-entry "r8")
     (register-entry "r9")
     (register-entry "r10")
     (register-entry "r11")
     (register-entry "r12")
     (register-entry "r13")
     (register-entry "r14")
     (register-entry "r15")
     ;; 32-bit registers
     (register-entry "eax")
     (register-entry "ecx")
     (register-entry "edx")
     (register-entry "ebx")
     (register-entry "esp")
     (register-entry "ebp")
     (register-entry "esi")
     (register-entry "edi")
     (register-entry "r8d")
     (register-entry "r9d")
     (register-entry "r10d")
     (register-entry "r11d")
     (register-entry "r12d")
     (register-entry "r13d")
     (register-entry "r14d")
     (register-entry "r15d")
     ;; 16-bit-registers
     (register-entry "ax")
     (register-entry "cx")
     (register-entry "dx")
     (register-entry "bx")
     (register-entry "sp")
     (register-entry "bp")
     (register-entry "si")
     (register-entry "di")
     (register-entry "r8w")
     (register-entry "r9w")
     (register-entry "r10w")
     (register-entry "r11w")
     (register-entry "r12w")
     (register-entry "r13w")
     (register-entry "r14w")
     (register-entry "r15w")
     ;; 8-bit registers
     (register-entry "al")
     (register-entry "cl")
     (register-entry "dl")
     (register-entry "bl")
     (register-entry "spl")
     (register-entry "bpl")
     (register-entry "sil")
     (register-entry "dil")
     (register-entry "r8b")
     (register-entry "r9b")
     (register-entry "r10b")
     (register-entry "r11b")
     (register-entry "r12b")
     (register-entry "r13b")
     (register-entry "r14b")
     (register-entry "r15b")
       ;;; xmm registers
     (register-entry "xmm0")
     (register-entry "xmm1")
     (register-entry "xmm2")
     (register-entry "xmm3")
     (register-entry "xmm4")
     (register-entry "xmm5")
     (register-entry "xmm6")
     (register-entry "xmm7")
     (register-entry "xmm8")
     (register-entry "xmm9")
     (register-entry "xmm10")
     (register-entry "xmm11")
     (register-entry "xmm12")
     (register-entry "xmm13")
     (register-entry "xmm14")
     (register-entry "xmm15")
     ;; MMX registers
     (register-entry "mm0")
     (register-entry "mm1")
     (register-entry "mm2")
     (register-entry "mm3")
     (register-entry "mm4")
     (register-entry "mm5")
     (register-entry "mm6")
     (register-entry "mm7")
     ;; x87 FP regs.  May or may not be useful.
     (register-entry "st[0]")
     (register-entry "st[1]")
     (register-entry "st[2]")
     (register-entry "st[3]")
     (register-entry "st[4]")
     (register-entry "st[5]")
     (register-entry "st[6]")
     (register-entry "st[7]")
     ;; Our friends, the segment registers
     (register-entry "cs")
     (register-entry "ds")
     (register-entry "ss")
     (register-entry "es")
     (register-entry "fs")
     (register-entry "gs")
     (register-entry "rip")
     )))

(dotimes (i (length *x8664-register-entries*))
  (let* ((entry (svref *x8664-register-entries* i)))
    (when entry
      (setf (reg-entry-ordinal64 entry) i))))


(defconstant +x86-64-bit-register+ #x00)
(defconstant +x86-32-bit-register+ #x10)
(defconstant +x86-16-bit-register+ #x20)
(defconstant +x86-8-bit-register+ #x30)
(defconstant +x86-xmm-register-offset+ #x40)
(defconstant +x86-mmx-register-offset+ #x50)
(defconstant +x86-fpu-register-offset+ #x58)
(defconstant +x86-segment-register-offset+ #x60)

(defun x86-segment-register (i)
  (if (and (typep i 'unsigned-byte)
           (< i 6))
    (svref *x8664-register-entries* (+ +x86-segment-register-offset+ i))))

(defun x86-xmm-register (i)
  (if (typep i '(mod 16))
    (svref *x8664-register-entries* (+ +x86-xmm-register-offset+ i))))

(defun x86-mmx-register (i)
  (if (typep i '(mod 8))
    (svref *x8664-register-entries* (+ +x86-mmx-register-offset+ i))))
    

(defun gpr-ordinal (r)
  (or
   (etypecase r
     ((mod 64) r)
     ((or string symbol)
      (let* ((entry (gethash r *x86-registers*)))
        (if entry
          (reg-entry-ordinal64 entry))))
     (reg-entry (reg-entry-ordinal64 r))
     (x86-register-operand
      (reg-entry-ordinal64 (x86-register-operand-entry r))))
   (error "Can't determine register ordinal of ~s" r)))


(defun x86-reg8 (r)
  (svref *x8664-register-entries* (dpb (gpr-ordinal r)
                                       (byte 4 0)
                                       +x86-8-bit-register+)))

(defun x86-reg16 (r)
  (svref *x8664-register-entries* (dpb (gpr-ordinal r)
                                        (byte 4 0)
                                        +x86-16-bit-register+)))

(defun x86-reg32 (r)
  (svref *x8664-register-entries* (dpb (gpr-ordinal r)
                                        (byte 4 0)
                                        +x86-32-bit-register+)))       

(defun x86-reg64 (r)
  (svref *x8664-register-entries* (dpb (gpr-ordinal r)
                                        (byte 4 0)
                                        +x86-64-bit-register+)))


;;; This returns true if the template's operand types "match" the
;;; types of the actual operands.
(defun match-template-types (template type0 type1 type2)
  (flet ((match (overlap given)
           (and
            (not (zerop (logandc2 overlap (encode-operand-type :jumpabsolute))))
            (= (logand given (encode-operand-type :baseindex :jumpabsolute))
               (logand overlap (encode-operand-type :baseindex :jumpabsolute)))))
         (consistent-register-match (m0 g0 t0 m1 g1 t1)
           (let* ((g0&reg (logand g0 (encode-operand-type :reg)))
                  (g1&reg (logand g1 (encode-operand-type :reg))))
             (or (zerop g0&reg)
                 (zerop g1&reg)
                 (= g0&reg g1&reg)
                 (not
                  (logtest
                   (if (logtest m0 (encode-operand-type :acc))
                     (encode-operand-type :reg)
                     t0)
                   (if (logtest m1 (encode-operand-type :acc))
                     (encode-operand-type :reg)
                     t1)))))))
    (let* ((nops (if type2 3 (if type1 2 (if type0 1 0)))))
      (declare (fixnum nops))
      (let* ((template-types
              (x86-opcode-template-operand-types template)))
        (when (= nops (the fixnum (length template-types)))
          (or (zerop nops)
              (let* ((template-type0 (svref template-types 0))
                     (overlap0
                      (logand type0 template-type0))
                     (match0 (match overlap0 type0)))
                (if match0
                  (or (= nops 1)
                      ;; 2 or 3 operands.
                      (let* ((template-type1 (svref template-types 1))
                             (overlap1 (logand type1 template-type1))
                             (match1 (match overlap1 type1)))
                        (if (and
                             match1
                             (consistent-register-match
                              overlap0
                              type0
                              template-type0
                              overlap1
                              type1
                              template-type1))
                          (or (= nops 2)
                              ;; 3 operands
                              (let* ((template-type2 (svref template-types 2))
                                     (overlap2 (logand type2 template-type2)))
                                (and (match overlap2 type2)
                                     (consistent-register-match
                                      overlap1
                                      type1
                                      template-type1
                                      overlap2
                                      type2
                                      template-type2)))))))))))))))
  
(defun match-template (template parsed-operands)
  (apply #'match-template-types template (mapcar #'x86-operand-type parsed-operands)))



(provide "X86-ASM")
