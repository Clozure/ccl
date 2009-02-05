;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")
#+x8664-target
(progn

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")
  (require :number-case-macro))


;;; make a float from hi - high 24 bits mantissa (ignore implied higher bit)
;;;                   lo -  low 28 bits mantissa
;;;                   exp  - take low 11 bits
;;;                   sign - sign(sign) => result
;;; hi result - 1 bit sign: 11 bits exp: 20 hi bits of hi arg
;;; lo result - 4 lo bits of hi arg: 28 lo bits of lo arg
;;; no error checks, no tweaks, no nuthin 

;;; sign is -1, 1, maybe zero



(defx86lapfunction %make-float-from-fixnums ((float 16 )(hi 8) #|(ra 0)|#(lo arg_x) (exp arg_y) (sign arg_z))
  (mov (% sign) (% imm1))
  (sar ($ 63) (% imm1))
  (shl ($ 63) (% imm1))
  (movq (@ hi (% rsp)) (% imm0))                        ;hi
  (andl ($ (ash (1- (ash 1 24)) x8664::fixnumshift)) (%l imm0))
  (shl ($ (- 28 x8664::fixnumshift)) (% imm0))
  (or (% imm0) (% imm1))
  (unbox-fixnum lo imm0)
  (andl ($ (1- (ash 1 28))) (%l imm0))
  (or (% imm0) (% imm1))
  (mov (% exp) (% imm0))
  (shl ($ (- ieee-double-float-exponent-offset x8664::fixnumshift)) (% imm0))
  (or (% imm0) (% imm1))
  (movq (@ float (% rsp)) (% arg_z))
  (mov (% imm1) (@ x8664::double-float.value (% arg_z)))
  (single-value-return 4))


;;; Maybe we should trap - or something - on NaNs.
(defx86lapfunction %%double-float-abs! ((n arg_y)(val arg_z))
  (mov (@ x8664::double-float.value (% n)) (% imm0))
  (btr ($ 63) (% imm0))
  (mov (% imm0) (@ x8664::double-float.value (% val)))
  (single-value-return))


(defx86lapfunction %short-float-abs ((n arg_z))
  (btr ($ 63) (% n))
  (single-value-return))


(defx86lapfunction %double-float-negate! ((src arg_y) (res arg_z))
  (movq (@ x8664::double-float.value (% src)) (% imm0))
  (btcq ($ 63) (% imm0))
  (movq (% imm0) (@ x8664::double-float.value (% res)))
  (single-value-return))


(defx86lapfunction %short-float-negate ((src arg_z))
  (btcq ($ 63) (% arg_z))
  (single-value-return))



(defx86lapfunction dfloat-significand-zeros ((dfloat arg_z))
  (movq (@ target::double-float.value (% dfloat)) (% imm1))
  (shl ($ (1+ IEEE-double-float-exponent-width)) (% imm1))
  (bsrq (% imm1) (% imm0))
  (xorq ($ (1- target::nbits-in-word)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

;;; This exploits the fact that the single float is already
;;; shifted left 32 bits.  We don't want to count the tag
;;; bit as significant, so bash the argument into a fixnum
;;; first.
(defx86lapfunction sfloat-significand-zeros ((sfloat arg_z))
  (xorb (%b sfloat) (%b sfloat))
  (shl ($ (1+ IEEE-single-float-exponent-width)) (% sfloat))
  (bsrq (% sfloat) (% imm0))
  (xorq ($ (1- target::nbits-in-word)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %%scale-dfloat! ((float arg_x)(int arg_y)(result arg_z))
  (unbox-fixnum int imm0)
  (get-double-float float fp1)
  (shl ($ IEEE-double-float-exponent-offset) (% imm0))
  (movd (% imm0) (% fp2))
  (mulsd (% fp2) (% fp1))
  (put-double-float fp1 result)
  (single-value-return))

(defx86lapfunction %%scale-sfloat! ((float arg_y)(int arg_z))
  (unbox-fixnum int imm0)
  (shl ($ IEEE-double-float-exponent-offset) (% imm0))
  (movd (% imm0) (% fp2))
  (get-single-float float fp1)
  (mulss (% fp2) (% fp1))
  (put-single-float fp1 arg_z)
  (single-value-return))

(defx86lapfunction %copy-double-float ((f1 arg_y) (f2 arg_z))
  (get-double-float f1 fp1)
  (put-double-float fp1 f2)
  (single-value-return))

(defx86lapfunction %short-float->double-float ((src arg_y) (result arg_z))
  (get-single-float src fp1)
  (cvtss2sd (% fp1) (% fp1))
  (put-double-float fp1 result)
  (single-value-return))

(defx86lapfunction %double-float->short-float ((src arg_z))
  (get-double-float src fp1)
  (cvtsd2ss (% fp1) (% fp1))
  (put-single-float fp1 arg_z)
  (single-value-return))

(defx86lapfunction %int-to-sfloat ((int arg_z))
  (int-to-single int imm0 fp1)
  (put-single-float fp1 arg_z)
  (single-value-return))
  

(defx86lapfunction %int-to-dfloat ((int arg_y) (dfloat arg_z))
  (int-to-double int imm0 fp1)
  (put-double-float fp1 arg_z)
  (single-value-return))



;;; Manipulate the MXCSR.  It'll fit in a fixnum, but we have to
;;; load and store it through memory.  On x8664, we can hide the
;;; 32-bit MXCSR value in a fixnum on the stack; on a 32-bit x86,
;;; we might need to use a scratch location in the TCR or something.

;;; Return the MXCSR as a fixnum
(defx86lapfunction %get-mxcsr ()
  (pushq ($ '0))
  (stmxcsr (@ 4 (% rsp)))
  (pop (% arg_z))
  (shr ($ (- 32 x8664::fixnumshift)) (% arg_z))
  (single-value-return))

;;; Store the fixnum in arg_z in the MXCSR.  Just to be
;;; on the safe side, mask the arg with X86::MXCSR-WRITE-MASK,
;;; so that only known control and status bits are written to.
(defx86lapfunction %set-mxcsr ((val arg_z))
  (mov (% val) (% temp0))
  (andl ($ '#.x86::mxcsr-write-mask) (%l temp0))
  (shl ($ (- 32 x8664::fixnumshift)) (% temp0))
  (push (% temp0))
  (ldmxcsr (@ 4 (% rsp)))
  (add ($ '1) (% rsp))
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
(defx86lapfunction %get-post-ffi-mxcsr ()
  (xor (% arg_z) (% arg_z))
  (movl (:rcontext x8664::tcr.ffi-exception) (%l imm0))
  (movl (%l arg_z) (:rcontext x8664::tcr.ffi-exception))
  (box-fixnum imm0 arg_z)
  (single-value-return))

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



;;; Don't we already have about 20 versions of this ?
(defx86lapfunction %double-float-from-macptr! ((ptr arg_x) (byte-offset arg_y) (dest arg_z))
  (macptr-ptr ptr imm0)
  (unbox-fixnum byte-offset imm1)
  (movsd (@ (% imm0) (% imm1)) (% fp1))
  (put-double-float fp1 dest)
  (single-value-return))


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



;;; Copy a single float pointed at by the macptr in single
;;; to a double float pointed at by the macptr in double

(defx86lapfunction %single-float-ptr->double-float-ptr ((single arg_y) (double arg_z))
  (check-nargs 2)
  (macptr-ptr single imm0)
  (movss (@ (% imm0)) (% fp1))
  (cvtss2sd (% fp1) (% fp1))
  (macptr-ptr double imm0)
  (movsd (% fp1) (@ (% imm0)))
  (single-value-return))

;;; Copy a double float pointed at by the macptr in double
;;; to a single float pointed at by the macptr in single.
(defx86lapfunction %double-float-ptr->single-float-ptr ((double arg_y) (single arg_z))
  (check-nargs 2)
  (macptr-ptr double imm0)
  (movsd (@ (% imm0)) (% fp1))
  (cvtsd2ss (% fp1) (% fp1))
  (macptr-ptr single imm0)
  (movss (% fp1) (@ (% imm0)))
  (single-value-return))


(defx86lapfunction %set-ieee-single-float-from-double ((src arg_y) (macptr arg_z))
  (check-nargs 2)
  (macptr-ptr macptr imm0)
  (get-double-float src fp1)
  (cvtsd2ss (% fp1) (% fp1))
  (movss (% fp1) (@ (% imm0)))
  (single-value-return))

(defx86lapfunction host-single-float-from-unsigned-byte-32 ((u32 arg_z))
  (shl ($ (- 32 x8664::fixnumshift)) (% arg_z))
  (movb ($ x8664::subtag-single-float) (% arg_z.b))
  (single-value-return))

(defx86lapfunction single-float-bits ((f arg_z))
  (shr ($ (- 32 x8664::fixnumshift)) (% f))
  (single-value-return))

(defun double-float-bits (f)
  (values (uvref f target::double-float.val-high-cell)
          (uvref f target::double-float.val-low-cell)))

(defun double-float-from-bits (high low)
  (let* ((f (%make-dfloat)))
    (setf (uvref f target::double-float.val-high-cell) high
          (uvref f target::double-float.val-low-cell) low)
    f))

;;; Return T if n is negative, else NIL.
(defx86lapfunction %double-float-sign ((n arg_z))
  (movl (@ x8664::double-float.val-high (% n)) (% imm0.l))
  (testl (% imm0.l) (% imm0.l))
  (movl ($ (target-t-value)) (% imm0.l))
  (movl ($ (target-nil-value)) (% arg_z.l))
  (cmovlq (% imm0) (% arg_z))
  (single-value-return))


(defx86lapfunction %short-float-sign ((n arg_z))
  (testq (% n) (% n))
  (movl ($ (target-t-value)) (% imm0.l))
  (movl ($ (target-nil-value)) (% arg_z.l))
  (cmovlq (% imm0) (% arg_z))
  (single-value-return))

(defx86lapfunction %double-float-sqrt! ((n arg_y) (result arg_z))
  (get-double-float n fp0)
  (sqrtsd (% fp0) (% fp0))
  (put-double-float fp0 result)
  (single-value-return))

(defx86lapfunction %single-float-sqrt ((n arg_z))
  (get-single-float n fp0)
  (sqrtss (% fp0) (% fp0))
  (put-single-float fp0 arg_z)
  (single-value-return))

;;; end of x86-float.lisp
) ; #+x8664-target
