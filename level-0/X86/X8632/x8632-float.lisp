;;; Copyright 2009 Clozure Associates
;;; This file is part of Clozure CL.  
;;;
;;; Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;; Public License , known as the LLGPL and distributed with Clozure
;;; CL as the file "LICENSE".  The LLGPL consists of a preamble and
;;; the LGPL, which is distributed with Clozure CL as the file "LGPL".
;;; Where these conflict, the preamble takes precedence.
;;;
;;; Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO"))

;;; make a float from hi - high 24 bits mantissa (ignore implied higher bit)
;;;                   lo -  low 28 bits mantissa
;;;                   exp  - take low 11 bits
;;;                   sign - sign(sign) => result
;;; hi result - 1 bit sign: 11 bits exp: 20 hi bits of hi arg
;;; lo result - 4 lo bits of hi arg: 28 lo bits of lo arg
;;; no error checks, no tweaks, no nuthin 

;;; sign is -1, 1, maybe zero

(defx8632lapfunction %make-float-from-fixnums ((dfloat 12) (hi 8) (lo 4) #|(ra 0)|# (exp arg_y) (sign arg_z))
  (mov (% sign) (% imm0))
  (movl (@ dfloat (% esp)) (% arg_z))
  (sar ($ 31) (% imm0))
  (shl ($ 31) (% imm0))			;insert sign
  (shl ($ (- 20 x8632::fixnumshift)) (% exp))
  (orl (% exp) (% imm0))		;insert biased exponent
  (movl (% imm0) (@ x8632::double-float.val-high (% arg_z)))
  (movl (@ hi (% esp)) (% arg_y))
  (andl ($ (ash (1- (ash 1 24)) x8632::fixnumshift)) (% arg_y))
  (movl (% arg_y) (% imm0))
  (shrl ($ (+ 4 x8632::fixnumshift)) (% imm0))              ;top 20 bits of hi
  (orl (% imm0) (@ x8632::double-float.val-high (% arg_z))) ; into high word
  ;; do low half
  (movl (@ lo (% esp)) (% imm0))
  (sar ($ x8632::fixnumshift) (% imm0))
  (andl ($ (1- (ash 1 28))) (% imm0))
  (shl ($ (- 28 x8632::fixnumshift)) (% arg_y)) ;position low 4 bits of hi
  (orl (% arg_y) (% imm0))
  (movl (% imm0) (@ x8632::double-float.value (% arg_z)))
  (single-value-return 5))

(defx8632lapfunction %make-short-float-from-fixnums ((sfloat 8) (significand 4) #|(ra 0)|# (biased-exp arg_y) (sign arg_z))
  (movl (% sign) (% imm0))
  (movl (@ sfloat (% esp)) (% arg_z))
  (sarl ($ 31) (% imm0))
  (shll ($ 31) (% imm0))		;insert sign
  (shll ($ (- ieee-single-float-exponent-offset x8632::fixnumshift)) (% biased-exp))
  (or (% biased-exp) (% imm0))		;insert biased exponent
  (movl (% imm0) (@ x8632::single-float.value (% arg_z)))
  (movl (@ significand (% esp)) (% imm0))
  (sar ($ x8632::fixnumshift) (% imm0))
  (andl ($ (1- (ash 1 ieee-single-float-hidden-bit))) (% imm0))
  (or (% imm0) (@ x8632::single-float.value (% arg_z)))
  (single-value-return 4))

;;; Maybe we should trap - or something - on NaNs.
(defx8632lapfunction %%double-float-abs! ((n arg_y) (val arg_z))
  (get-double-float n fp1)
  (put-double-float fp1 val)
  (btrl ($ 31) (@ x8632::double-float.val-high (% val)))
  (single-value-return))

(defx8632lapfunction %%short-float-abs! ((n arg_y) (val arg_z))
  (movl (@ x8632::single-float.value (% n)) (% imm0))
  (btr ($ 31) (% imm0))
  (movl (% imm0) (@ x8632::single-float.value (% val)))
  (single-value-return))

(defx8632lapfunction %double-float-negate! ((src arg_y) (res arg_z))
  (get-double-float src fp1)
  (put-double-float fp1 res)
  (btcl ($ 31) (@ x8632::double-float.val-high (% res)))
  (single-value-return))

(defx8632lapfunction %short-float-negate! ((src arg_y) (res arg_z))
  (movl (@ x8632::single-float.value (% src)) (% imm0))
  (btcl ($ 31) (% imm0))
  (movl (% imm0) (@ x8632::single-float.value (% res)))
  (single-value-return))

;;; return hi (25 bits) lo (28 bits) exp sign
(defx8632lapfunction %integer-decode-double-float ((n arg_z))
  (mark-as-imm temp0)
  (let ((imm1 temp0)
	(sign 0)
	(exp 4)
	(lo 8)
	(hi 12))
    (pushl ($ 0))			;hi
    (pushl ($ 0))			;lo
    (pushl ($ 0))			;exp
    (pushl ($ 0))			;sign

    (movl (@ x8632::double-float.val-high (% n)) (% imm1))
    (movl ($ '1) (% arg_y))
    (movl ($ '-1) (% imm0))
    (btl ($ 31) (% imm1))
    (cmovcl (% imm0) (% arg_y))
    (movl (% arg_y) (@ sign (% esp)))

    (movl (% imm1) (% imm0))
    (andl ($ #x7ff00000) (% imm0))	;exponent
    (shrl ($ (- 20 x8632::fixnumshift)) (% imm0))
    (movl (% imm0) (@ exp (% esp)))

    (movl (@ x8632::double-float.value (% n)) (% imm0))
    (andl ($ #x000fffff) (% imm1))	;high 20 bits of fraction
    (shldl ($ 4) (% imm0) (% imm1))	;shift in 4 bits from low word
    (cmpl ($ 0) (@ exp (% esp)))
    (je @denorm)
    (or ($ (ash 1 (- ieee-double-float-hidden-bit 28))) (% imm1))
    @denorm
    (box-fixnum imm1 arg_y)
    (movl (% arg_y) (@ hi (% esp)))

    (shll ($ 4) (% imm0))		;shift out bits included in hi
    (shrl ($ x8632::fixnumshift) (% imm0)) ;and box 28 low bits
    (movl (% imm0) (@ lo (% esp))))
  (mark-as-node temp0)
  (set-nargs 4)
  (leal (@ '4 (% esp)) (% temp0))
  (jmp-subprim .SPvalues))

;;; hi is 25 bits lo is 28 bits
;;; big is 32 lo, 21 hi right justified
(defx8632lapfunction make-big-53 ((hi 4) #|(ra 0)|# (lo arg_y) (big arg_z))
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (movl (@ hi (% esp)) (% temp1))
    (movl (% temp1) (% imm0))
    (shll ($ (- 28 x8632::fixnumshift)) (% imm0))
    (unbox-fixnum lo imm1)
    (orl (% imm0) (% imm1))
    (movl (% imm1) (@ x8632::misc-data-offset (% big))) ;low 32 bits
    (movl (% temp1) (% imm0))
    (sarl ($ (+ 4 x8632::fixnumshift)) (% imm0))
    (movl (% imm0) (@ (+ 4 x8632::misc-data-offset) (% big)))) ;high 21 bits
  (mark-as-node temp0)
  (single-value-return 3))

;;; dfloat must be non-zero
(defx8632lapfunction dfloat-significand-zeros ((dfloat arg_z))
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (movl (@ x8632::double-float.value (% dfloat)) (% imm0))
    (movl (@ x8632::double-float.val-high (% dfloat)) (% imm1))
    ;; shift imm1 left by count, shifting bits from imm0 in from the right
    (shldl ($ (1+ ieee-double-float-exponent-width)) (% imm0) (% imm1))
    (testl (% imm1) (% imm1))
    (jz @low)
    (bsrl (% imm1) (% imm0))
    (xorl ($ (1- x8632::nbits-in-word)) (% imm0))
    (jmp @done)
    @low
    (bsrl (% imm0) (% imm0))
    (xorl ($ (1- x8632::nbits-in-word)) (% imm0))
    ;; if we're here, the upper part of the fraction was all zeros,
    ;; so add the count for those in.
    (add ($ (- ieee-double-float-mantissa-width 32)) (% imm0))
    @done
    (box-fixnum imm0 arg_z))
  (mark-as-node temp0)
  (single-value-return))

;;; sfloat must be non-zero
(defx8632lapfunction sfloat-significand-zeros ((sfloat arg_z))
  (movl (@ x8632::single-float.value (% sfloat)) (% imm0))
  (shl ($ (1+ IEEE-single-float-exponent-width)) (% imm0))
  (bsrl (% imm0) (% imm0))
  (xorl ($ (1- x8632::nbits-in-word)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx8632lapfunction %%scale-dfloat! ((dfloat 4) #|(ra 0)|# (int arg_y) (result arg_z))
  (unbox-fixnum int imm0)
  (movl (@ dfloat (% esp)) (% temp0))
  (get-double-float temp0 fp1)
  (shl ($ (- ieee-double-float-exponent-offset 32)) (% imm0))
  (movl ($ 0) (@ x8632::double-float.value (% result)))
  (movl (% imm0) (@ x8632::double-float.val-high (% result)))
  (get-double-float result fp2)
  (mulsd (% fp2) (% fp1))
  (put-double-float fp1 result)
  (single-value-return 3))

(defx8632lapfunction %%scale-sfloat! ((sfloat 4) #|(ra 0)|# (int arg_y) (result arg_z))
  (unbox-fixnum int imm0)
  (movl (@ sfloat (% esp)) (% temp0))
  (get-single-float temp0 fp1)
  (shl ($ ieee-single-float-exponent-offset) (% imm0))
  (movd (% imm0) (% fp2))
  (mulss (% fp2) (% fp1))
  (put-single-float fp1 arg_z)
  (single-value-return 3))

(defx8632lapfunction %copy-double-float ((f1 arg_y) (f2 arg_z))
  (get-double-float f1 fp1)
  (put-double-float fp1 f2)
  (single-value-return))

(defx8632lapfunction %copy-short-float ((f1 arg_y) (f2 arg_z))
  (get-single-float f1 fp1)
  (put-single-float fp1 f2)
  (single-value-return))

(defx8632lapfunction %double-float-exp ((n arg_z))
  (movl (@ x8632::double-float.val-high (% n)) (% imm0))
  (shll ($ 1) (% imm0))
  (shrl ($ (1+ (- ieee-double-float-exponent-offset 32))) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx8632lapfunction set-%double-float-exp ((dfloat arg_y) (exp arg_z))
  (movl (% exp) (% temp0))
  (shll ($ (1+ (- 20 x8632::fixnumshift))) (% temp0))
  (shrl ($ 1) (% temp0))
  (movl (@ x8632::double-float.val-high (% dfloat)) (% imm0))
  (andl ($ #x800fffff) (% imm0))
  (orl (% temp0) (% imm0))
  (movl (% imm0) (@ x8632::double-float.val-high (% dfloat)))
  (single-value-return))

(defx8632lapfunction %short-float-exp ((n arg_z))
  (movl (@ x8632::single-float.value (% n)) (% imm0))
  (shll ($ 1) (% imm0))
  (shrl ($ (1+ ieee-single-float-exponent-offset)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx8632lapfunction set-%short-float-exp ((sfloat arg_y) (exp arg_z))
  (movl (% exp) (% temp0))
  (shll ($ (1+ (- ieee-single-float-exponent-offset x8632::fixnumshift))) (% temp0))
  (shrl ($ 1) (% temp0))
  (movl (@ x8632::single-float.value (% sfloat)) (% imm0))
  (andl ($ #x807fffff) (% imm0))
  (orl (% temp0) (% imm0))
  (movl (% imm0) (@ x8632::single-float.value (% sfloat)))
  (single-value-return))

(defx8632lapfunction %short-float->double-float ((src arg_y) (result arg_z))
  (get-single-float src fp1)
  (cvtss2sd (% fp1) (% fp1))
  (put-double-float fp1 result)
  (single-value-return))

(defx8632lapfunction %double-float->short-float ((src arg_y) (result arg_z))
  (get-double-float src fp1)
  (cvtsd2ss (% fp1) (% fp1))
  (put-single-float fp1 result)
  (single-value-return))

(defx8632lapfunction %int-to-sfloat! ((int arg_y) (sfloat arg_z))
  (int-to-single int imm0 fp1)
  (put-single-float fp1 arg_z)
  (single-value-return))

(defx8632lapfunction %int-to-dfloat ((int arg_y) (dfloat arg_z))
  (int-to-double int imm0 fp1)
  (put-double-float fp1 arg_z)
  (single-value-return))



;;; Manipulate the MXCSR.  It'll fit in a fixnum, but we have to
;;; load and store it through memory.  On x8664, we can hide the
;;; 32-bit MXCSR value in a fixnum on the stack; on a 32-bit x86,
;;; we might need to use a scratch location in the TCR or something.

;;; Return the MXCSR as a fixnum
(defx8632lapfunction %get-mxcsr ()
  (stmxcsr (:rcontext x8632::tcr.scratch-mxcsr))
  (movl (:rcontext x8632::tcr.scratch-mxcsr) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

;;; Store the fixnum in arg_z in the MXCSR.  Just to be
;;; on the safe side, mask the arg with X86::MXCSR-WRITE-MASK,
;;; so that only known control and status bits are written to.
(defx8632lapfunction %set-mxcsr ((val arg_z))
  (unbox-fixnum val imm0)
  (andl ($ x86::mxcsr-write-mask) (% imm0))
  (movl (% imm0) (:rcontext x8632::tcr.scratch-mxcsr))
  (ldmxcsr (:rcontext x8632::tcr.scratch-mxcsr))
  (single-value-return))


;;; Get the bits that contain exception masks and rounding mode.

(defun %get-mxcsr-control ()
  (logand x86::mxcsr-control-and-rounding-mask (the fixnum (%get-mxcsr))))

;;; Get the bits that describe current exceptions.
(defun %get-mxcsr-status ()
  (logand x86::mxcsr-status-mask (the fixnum (%get-mxcsr))))

;;; Set the bits that describe current exceptions, presumably to clear them.
(defun %set-mxcsr-status (arg)
  (%set-mxcsr
   (logior (logand x86::mxcsr-status-mask arg)
           (logandc2 (%get-mxcsr) x86::mxcsr-status-mask)))
  arg)

;;; Set the bits that mask/unmask exceptions and control rounding.
;;; Clear the bits which describe current exceptions.
(defun %set-mxcsr-control (arg)
  (%set-mxcsr (logand x86::mxcsr-control-and-rounding-mask arg)))

;;; Return the MXCSR value in effect after the last ff-call.
(defx8632lapfunction %get-post-ffi-mxcsr ()
  (xor (% arg_z) (% arg_z))
  (movl (:rcontext x8632::tcr.ffi-exception) (%l imm0))
  (movl (%l arg_z) (:rcontext x8632::tcr.ffi-exception))
  (box-fixnum imm0 arg_z)
  (single-value-return))

;;; The next several defuns are copied verbatim from x8664-float.lisp.
;;; It will probably be desirable to factor this code out into a new
;;; x86-float.lisp, perhaps conditionalized via #+sse2 or something.
;;; (Some day we may want to support x87 fp and we'll need
;;; x87-specific versions of these functions.)

;;; start duplicated code

;;; Return the status bits from the last ff-call that represent
;;; unmasked exceptions
(defun %ffi-exception-status ()
  (logior (%get-mxcsr-control)
          (logand x86::mxcsr-status-mask (the fixnum (%get-post-ffi-mxcsr)))))

;;; See if the binary double-float operation OP set any enabled
;;; exception bits in the mxcsr
(defun %df-check-exception-2 (operation op0 op1 fp-status)
  (declare (type (unsigned-byte 6) fp-status))
  (unless (zerop fp-status)
    (%set-mxcsr-status 0)
    ;; Ensure that operands are heap-consed
    (%fp-error-from-status fp-status
			   operation 
			   (%copy-double-float op0 (%make-dfloat)) 
			   (%copy-double-float op1 (%make-dfloat)))))

(defun %sf-check-exception-2 (operation op0 op1 fp-status)
  (declare (type (unsigned-byte 6) fp-status))
  (unless (zerop fp-status)
    (%set-mxcsr-status 0)
    ;; Ensure that operands are heap-consed
    (%fp-error-from-status fp-status 
			   operation
			   #+32-bit-target
			   (%copy-short-float op0 (%make-sfloat))
			   #+64-bit-target op0
			   #+32-bit-target
			   (%copy-short-float op1 (%make-sfloat))
			   #+64-bit-target op1)))

(defun %df-check-exception-1 (operation op0 fp-status)
  (declare (fixnum fp-status))
  (unless (zerop fp-status)
    (%set-mxcsr-status 0)
    ;; Ensure that operands are heap-consed
    (%fp-error-from-status fp-status 
                           operation 
                           (%copy-double-float op0 (%make-dfloat)))))

(defun %sf-check-exception-1 (operation op0 fp-status)
  (declare (type (unsigned-byte 6) fp-status))
  (unless (zerop fp-status)
    (%set-mxcsr-status 0)
    ;; Ensure that operands are heap-consed
    (%fp-error-from-status fp-status 
			   operation
			   #+32-bit-target
			   (%copy-short-float op0 (%make-sfloat))
			   #+64-bit-target op0)))


(defun fp-condition-from-mxcsr (status-bits control-bits)
  (declare (fixnum status-bits control-bits))
  (cond 
   ((and (logbitp x86::mxcsr-ie-bit status-bits)
         (not (logbitp x86::mxcsr-im-bit control-bits)))
    'floating-point-invalid-operation)
   ((and (logbitp x86::mxcsr-oe-bit status-bits)
         (not (logbitp x86::mxcsr-om-bit control-bits)))
    'floating-point-overflow)
   ((and (logbitp x86::mxcsr-ue-bit status-bits)
         (not (logbitp x86::mxcsr-um-bit control-bits)))
    'floating-point-underflow)
   ((and (logbitp x86::mxcsr-ze-bit status-bits)
         (not (logbitp x86::mxcsr-zm-bit control-bits)))
    'division-by-zero)
   ((and (logbitp x86::mxcsr-pe-bit status-bits)
         (not (logbitp x86::mxcsr-pm-bit control-bits)))
    'floating-point-inexact)))

(defun %fp-error-from-status (status-bits  operation op0 &optional op1)
  (declare (type (unsigned-byte 6) status-bits))
  (let* ((condition-class (fp-condition-from-mxcsr status-bits (%get-mxcsr-control))))
    (if condition-class
      (let* ((operands (if op1 (list op0 op1) (list op0))))
        (error (make-instance condition-class
                              :operation operation
                              :operands operands))))))

(defvar *rounding-mode-alist*
  '((:nearest . 0) (:zero . 1) (:positive . 2) (:negative . 3)))

(defun get-fpu-mode (&optional (mode nil mode-p))
  (let* ((flags (%get-mxcsr-control)))
    (declare (fixnum flags))
    (let* ((rounding-mode
            (car (nth (+ (if (logbitp x86::mxcsr-rc0-bit flags) 1 0)
                         (if (logbitp x86::mxcsr-rc1-bit flags) 2 0))
                      *rounding-mode-alist*)))
           (overflow (not (logbitp x86::mxcsr-om-bit flags)))
           (underflow (not (logbitp x86::mxcsr-um-bit flags)))
           (division-by-zero (not (logbitp x86::mxcsr-zm-bit flags)))
           (invalid (not (logbitp x86::mxcsr-im-bit flags)))
           (inexact (not (logbitp x86::mxcsr-pm-bit flags))))
    (if mode-p
      (ecase mode
        (:rounding-mode rounding-mode)
        (:overflow overflow)
        (:underflow underflow)
        (:division-by-zero division-by-zero)
        (:invalid invalid)
        (:inexact inexact))
      `(:rounding-mode ,rounding-mode
        :overflow ,overflow
        :underflow ,underflow
        :division-by-zero ,division-by-zero
        :invalid ,invalid
        :inexact ,inexact)))))

;;; did we document this?
(defun set-fpu-mode (&key (rounding-mode :nearest rounding-p)
                          (overflow t overflow-p)
                          (underflow t underflow-p)
                          (division-by-zero t zero-p)
                          (invalid t invalid-p)
                          (inexact t inexact-p))
  (let* ((current (%get-mxcsr-control))
         (new current))
    (declare (fixnum current new))
    (when rounding-p
      (let* ((rc-bits (or
                       (cdr (assoc rounding-mode *rounding-mode-alist*))
                       (error "Unknown rounding mode: ~s" rounding-mode))))
        (declare (fixnum rc-bits))
        (if (logbitp 0 rc-bits)
          (bitsetf x86::mxcsr-rc0-bit new)
          (bitclrf x86::mxcsr-rc0-bit new))
        (if (logbitp 1 rc-bits)
          (bitsetf x86::mxcsr-rc1-bit new)
          (bitclrf x86::mxcsr-rc1-bit new))))
    (when invalid-p
      (if invalid
        (bitclrf x86::mxcsr-im-bit new)
        (bitsetf x86::mxcsr-im-bit new)))
    (when overflow-p
      (if overflow
        (bitclrf x86::mxcsr-om-bit new)
        (bitsetf x86::mxcsr-om-bit new)))
    (when underflow-p
      (if underflow
        (bitclrf x86::mxcsr-um-bit new)
        (bitsetf x86::mxcsr-um-bit new)))
    (when zero-p
      (if division-by-zero
        (bitclrf x86::mxcsr-zm-bit new)
        (bitsetf x86::mxcsr-zm-bit new)))
    (when inexact-p
      (if inexact
        (bitclrf x86::mxcsr-pm-bit new)
        (bitsetf x86::mxcsr-pm-bit new)))
    (unless (= current new)
      (%set-mxcsr-control new))
    (%get-mxcsr)))

;;; end duplicated code

;;; Don't we already have about 20 versions of this ?
(defx8632lapfunction %double-float-from-macptr! ((ptr 4) #|(ra 0)|# (byte-offset arg_y) (dest arg_z))
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (movl (@ ptr (% esp)) (% temp1))
    (macptr-ptr temp1 imm0)
    (unbox-fixnum byte-offset imm1)
    (movsd (@ (% imm0) (% imm1)) (% fp1))
    (put-double-float fp1 dest))
  (mark-as-node temp0)
  (single-value-return 3))

;;; Copy a single float pointed at by the macptr in single
;;; to a double float pointed at by the macptr in double
(defx8632lapfunction %single-float-ptr->double-float-ptr ((single arg_y) (double arg_z))
  (check-nargs 2)
  (macptr-ptr single imm0)
  (movss (@ (% imm0)) (% fp1))
  (cvtss2sd (% fp1) (% fp1))
  (macptr-ptr double imm0)
  (movsd (% fp1) (@ (% imm0)))
  (single-value-return))

;;; Copy a double float pointed at by the macptr in double
;;; to a single float pointed at by the macptr in single.
(defx8632lapfunction %double-float-ptr->single-float-ptr ((double arg_y) (single arg_z))
  (check-nargs 2)
  (macptr-ptr double imm0)
  (movsd (@ (% imm0)) (% fp1))
  (cvtsd2ss (% fp1) (% fp1))
  (macptr-ptr single imm0)
  (movss (% fp1) (@ (% imm0)))
  (single-value-return))

(defx8632lapfunction %set-ieee-single-float-from-double ((src arg_y) (macptr arg_z))
  (check-nargs 2)
  (macptr-ptr macptr imm0)
  (get-double-float src fp1)
  (cvtsd2ss (% fp1) (% fp1))
  (movss (% fp1) (@ (% imm0)))
  (single-value-return))

(defun host-single-float-from-unsigned-byte-32 (u32)
  (let* ((f (%make-sfloat)))
    (setf (uvref f x8632::single-float.value-cell) u32)
    f))

(defun single-float-bits (f)
  (uvref f x8632::single-float.value-cell))

(defun double-float-bits (f)
  (values (uvref f target::double-float.val-high-cell)
          (uvref f target::double-float.value-cell)))

(defun double-float-from-bits (high low)
  (let* ((f (%make-dfloat)))
    (setf (uvref f target::double-float.val-high-cell) high
          (uvref f target::double-float.value-cell) low)
    f))

;;; Return T if n is negative, else NIL.
(defx8632lapfunction %double-float-sign ((n arg_z))
  (movl (@ x8632::double-float.val-high (% n)) (% imm0))
  (testl (% imm0) (% imm0))
  (movl ($ (target-t-value)) (% imm0))
  (movl ($ (target-nil-value)) (% arg_z))
  (cmovll (% imm0) (% arg_z))
  (single-value-return))

(defx8632lapfunction %short-float-sign ((n arg_z))
  (movl (@ x8632::single-float.value (% n)) (% imm0))
  (testl (% imm0) (% imm0))
  (movl ($ (target-t-value)) (% imm0))
  (movl ($ (target-nil-value)) (% arg_z))
  (cmovll (% imm0) (% arg_z))
  (single-value-return))

(defx8632lapfunction %double-float-sqrt! ((n arg_y) (result arg_z))
  (get-double-float n fp0)
  (sqrtsd (% fp0) (% fp0))
  (put-double-float fp0 result)
  (single-value-return))

(defx8632lapfunction %single-float-sqrt! ((n arg_y) (result arg_z))
  (get-single-float n fp0)
  (sqrtss (% fp0) (% fp0))
  (put-single-float fp0 arg_z)
  (single-value-return))



