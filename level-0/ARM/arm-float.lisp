;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
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


(defarmlapfunction %make-float-from-fixnums ((float 4)(hi 0) (lo arg_x) (exp arg_y) (sign arg_z))
  (ldr imm0 (:@ vsp (:$ hi)))
  (unbox-fixnum imm0 imm0)
  (unbox-fixnum imm1 lo)
  (cmp sign (:$ 0))
  (orr imm1 imm1 (:lsl imm0 (:$ 28)))
  (mov imm0 (:lsr imm0 (:$ 4)))
  (bic imm0 imm0 (:$ #xff000000))
  (bic imm0 imm0 (:$ #x00f00000))
  (orr imm0 imm0 (:lsl exp (:$ (- 20 arm::fixnumshift)))) ;  exp left 20 right 2 keep 11 bits
  (ldr arg_z (:@ vsp (:$ float)))
  (orrmi imm0 imm0 (:$ #x80000000))
  (str imm0 (:@ arg_z (:$ arm::double-float.val-high)))
  (str imm1 (:@ arg_z (:$ arm::double-float.val-low)))
  (add vsp vsp '2)
  (bx lr))


(defarmlapfunction %make-short-float-from-fixnums ((float 0) (sig arg_x) (exp arg_y) (sign arg_z))
  (mov imm0 (:lsl sig (:$ (- 32 (+ ieee-single-float-hidden-bit arm::fixnumshift)))))
  (mov imm0 (:lsr imm0 (:$ (- 32 ieee-single-float-hidden-bit))))
  (and imm1 sign (:$ #x80000000))
  (orr imm0 imm0 (:lsl exp (:$ (- 23 arm::fixnumshift))))
  (orr imm0 imm0 imm1)
  (vpop1 arg_z)
  (str imm0 (:@ arg_z (:$ arm::single-float.value)))
  (bx lr))


(defarmlapfunction %%double-float-abs! ((n arg_y)(val arg_z))
  (get-double-float d0 n)
  (fabsd d1 d0)
  (put-double-float d1 val)
  (bx lr))

(defarmlapfunction %%short-float-abs! ((n arg_y) (val arg_z))
  (get-single-float s1 n imm0)
  (fabss s0 s1)
  (put-single-float s0 val imm0)
  (bx lr))



(defarmlapfunction %double-float-negate! ((src arg_y) (res arg_z))
  (get-double-float d0 src)
  (fnegd d1 d0)
  (put-double-float d1 res)
  (bx lr))

(defarmlapfunction %short-float-negate! ((src arg_y) (res arg_z))
  (get-single-float s0 src imm0)
  (fnegs s1 s0)
  (put-single-float s1 res imm0)
  (bx lr))




;;; rets hi (25 bits) lo (28 bits) exp sign
(defarmlapfunction %integer-decode-double-float ((n arg_z))
  (ldr imm0  (:@ n (:$ arm::double-float.val-high)))
  (mov temp0 '1)
  (tst imm0 (:$ #x80000000))
  (movne temp0 '-1)
  (bic imm1 imm0 (:$ #x80000000))
  (mov temp1 '-1)
  (ands temp1 temp1 (:lsr imm1 (:$ (- (- IEEE-double-float-exponent-offset 32)
                                      arm::fixnumshift))))
  (mov imm0 (:lsl imm0 (:$ 12)))
  (mov imm0 (:lsr imm0 (:$ 12)))
  (ldr imm1 (:@ n (:$ arm::double-float.val-low)))
  (orrne imm0 imm0 (:$ (ash 1 (- IEEE-double-float-hidden-bit 32))))
  (mov imm0 (:lsl imm0 (:$ 4)))
  (orr imm0 imm0 (:lsr imm1 (:$ (- 32 4))))
  (box-fixnum imm0 imm0)
  (mov imm1 (:lsl imm1 (:$ 4)))
  (mov imm1 (:lsr imm1 (:$ (- 4 arm::fixnumshift))))
  (vpush1 imm0)   ; hi 25 bits of mantissa (includes implied 1)
  (vpush1 imm1)   ; lo 28 bits of mantissa
  (vpush1 temp1)  ; exp
  (vpush1 temp0)  ; sign
  (set-nargs 4)
  (add temp0 vsp '4)
  (spjump .SPvalues))


;;; hi is 25 bits lo is 28 bits
;;; big is 32 lo, 21 hi right justified
(defarmlapfunction make-big-53 ((hi arg_x)(lo arg_y)(big arg_z))
  (unbox-fixnum imm0 hi)
  (unbox-fixnum imm1 lo)
  (orr imm1 imm1 (:lsl imm0 (:$ 28)))
  (mov imm0 (:lsr imm0 (:$ 4)))
  (str imm0 (:@ big (:$ (+ arm::misc-data-offset 4))))
  (str imm1 (:@ big (:$ arm::misc-data-offset)))
  (bx lr))


(defarmlapfunction dfloat-significand-zeros ((dfloat arg_z))
  (ldr imm1 (:@ dfloat (:$ arm::double-float.value)))
  (movs imm1 (:lsl imm1 (:$ 12)))
  (clz imm1 imm1)
  (movne arg_z (:lsl imm1 (:$ arm::fixnumshift)))
  (bxne lr)
  @golo
  (ldr imm1 (:@ dfloat (:$ arm::double-float.val-low)))
  (clz imm1 imm1)
  (add imm1 imm1 (:$ 20))
  (box-fixnum arg_z imm1)
  (bx lr))

(defarmlapfunction sfloat-significand-zeros ((sfloat arg_z))
  (ldr imm1 (:@ sfloat (:$ arm::single-float.value)))
  (mov imm1 (:lsl imm1 (:$ 9)))
  (clz imm1 imm1)
  (box-fixnum arg_z imm1)
  (bx lr))



(defarmlapfunction %%scale-dfloat! ((float arg_x)(int arg_y)(result arg_z))
  (unbox-fixnum imm2 int)               ;imm0/imm1 needed for ldrd, etc.
  (get-double-float d0 float)
  (mov temp0 (:$ 0))
  (mov imm2 (:lsl imm2 (:$ (- ieee-double-float-exponent-offset 32))))
  (fmdrr d1 temp0 imm2)
  (fmuld d0 d1 d0)
  (put-double-float d0 result)
  (bx lr))



(defarmlapfunction %%scale-sfloat! ((float arg_x)(int arg_y)(result arg_z))
  (ldr imm1 (:@ float (:$ arm::single-float.value)))
  (mov imm0 (:lsl int (:$ (- IEEE-single-float-exponent-offset arm::fixnumshift))))
  (fmsr s0 imm1)
  (fmsr s2 imm0)
  (fmuls s0 s0 s2)
  (fmrs imm0 s0)
  (str imm0 (:@ result (:$ arm::single-float.value)))
  (bx lr))
                   



(defarmlapfunction %copy-double-float ((f1 arg_y) (f2 arg_z))
  (ldrd imm0 (:@ f1 (:$ arm::double-float.value)))
  (strd imm0 (:@ f2 (:$ arm::double-float.value)))
  (bx lr))
                   


(defarmlapfunction %copy-short-float ((f1 arg_y) (f2 arg_z))
  (ldr imm0 (:@ f1 (:$ arm::single-float.value)))
  (str imm0 (:@ f2 (:$ arm::single-float.value)))
  (bx lr))


(defarmlapfunction %double-float-exp ((n arg_z))
  (ldr imm1 (:@ n (:$ arm::double-float.val-high)))
  (mov imm1 (:lsl imm1 (:$ 1)))
  (mov imm1 (:lsr imm1 (:$ (1+ (- ieee-double-float-exponent-offset 32)))))
  (box-fixnum arg_z imm1)
  (bx lr))




(defarmlapfunction set-%double-float-exp ((float arg_y) (exp arg_z))
  (ldr imm1 (:@ float (:$ arm::double-float.val-high)))
  (mov imm0 (:$ #xff000000))
  (orr imm0 imm0 (:$ #x00e00000))
  (bic imm1 imm1 (:lsr imm0 (:$ 1)))
  (and imm0 imm0 (:lsl exp (:$ (- 21 arm::fixnumshift))))
  (orr imm1 imm1 (:lsr imm0 (:$ 1)))
  (str imm1 (:@ float (:$ arm::double-float.val-high))) ; hdr - tag = 8 - 2
  (bx lr))




(defarmlapfunction %short-float-exp ((n arg_z))
  (ldr imm1 (:@ n (:$ arm::single-float.value)))
  (mov imm1 (:lsl imm1 (:$ 1)))
  (mov imm1 (:lsr imm1 (:$ (1+ ieee-single-float-exponent-offset))))
  (box-fixnum arg_z imm1)
  (bx lr))




(defarmlapfunction set-%short-float-exp ((float arg_y) (exp arg_z))
  (ldr imm1 (:@ float (:$ arm::single-float.value)))
  (mov imm0 (:$ #xff000000))
  (mvn imm0 (:lsr imm0 (:$ 1)))
  (and imm1 imm1 imm0)
  (orr imm1 imm1 (:lsl exp (:$ (- ieee-single-float-exponent-offset arm::fixnumshift))))
  (str imm1 (:@ float (:$ arm::single-float.value)))
  (bx lr))

  
(defarmlapfunction %short-float->double-float ((src arg_y) (result arg_z))
  (get-single-float s0 src imm0)
  (fcvtds d1 s0)
  (put-double-float d1 result)
  (bx lr))


(defarmlapfunction %double-float->short-float ((src arg_y) (result arg_z))
  ;(clear-fpu-exceptions)
  (get-double-float d0 src)
  (fcvtsd s1 d0)
  (put-single-float s1 result imm0)
  (bx lr))


  



(defarmlapfunction %int-to-sfloat! ((int arg_y) (sfloat arg_z))
  (unbox-fixnum imm0 int)
  (fmsr s0 imm0)
  (fsitos s1 s0)
  (put-single-float s1 sfloat imm0)
  (bx lr))


  

(defarmlapfunction %int-to-dfloat ((int arg_y) (dfloat arg_z))
  (unbox-fixnum imm0 int)
  (fmsr s0 imm0)
  (fsitod d1 s0)
  (put-double-float d1 dfloat)
  (bx lr))

(defarmlapfunction %ffi-exception-status ()
  (ldr imm1 (:@ rcontext (:$ arm::tcr.lisp-fpscr)))
  (fmrx imm2 :fpscr)
  (and imm0 imm2 (:$ #xff))
  (ands imm0 imm0 (:lsr imm1 (:$ 8)))
  (moveq arg_z 'nil)
  (bxeq lr)
  (mov arg_z (:lsl imm0 (:$ arm::fixnumshift)))
  (bic imm0 imm2 (:$ #xff))
  (fmxr :fpscr imm0)
  (bx lr))

(defun %sf-check-exception-1 (operation op0 fp-status)
  (when fp-status
    (let* ((condition-name (fp-condition-name-from-fpscr-status fp-status)))
      (error (make-instance (or condition-name 'arithmetic-error)
                            :operation operation
                            :operands (list (%copy-short-float op0 (%make-sfloat))))))))

(defun %sf-check-exception-2 (operation op0 op1 fp-status)
  (when fp-status
    (let* ((condition-name (fp-condition-name-from-fpscr-status fp-status)))
      (error (make-instance (or condition-name 'arithmetic-error)
                            :operation operation
                            :operands (list (%copy-short-float op0 (%make-sfloat))
                                            (%copy-short-float op1 (%make-sfloat))))))))


(defun %df-check-exception-1 (operation op0 fp-status)
  (when fp-status
    (let* ((condition-name (fp-condition-name-from-fpscr-status fp-status)))
      (error (make-instance (or condition-name 'arithmetic-error)
                            :operation operation
                            :operands (list (%copy-double-float op0 (%make-dfloat))))))))

; See if the binary double-float operation OP set any enabled
; exception bits in the fpscr
(defun %df-check-exception-2 (operation op0 op1 fp-status)
  (when fp-status
    (let* ((condition-name (fp-condition-name-from-fpscr-status fp-status)))
      (error (make-instance (or condition-name 'arithmetic-error)
                            :operation operation
                            :operands (list (%copy-double-float op0 (%make-dfloat))
                                            (%copy-double-float op1 (%make-dfloat))))))))

(defvar *rounding-mode-alist*
  '((:nearest . 0) (:positive . 1) (:negative . 2) (:zero . 3)))

(defun get-fpu-mode (&optional (mode nil mode-p))
  (let* ((flags (%get-fpscr-control)))
    (declare (fixnum flags))
    (let* ((rounding-mode
            (car (nth (ldb (byte 2 22) flags) *rounding-mode-alist*)))
           (overflow (logbitp arm::ofe flags))
           (underflow (logbitp arm::ufe flags))
           (division-by-zero (logbitp arm::dze flags))
           (invalid (logbitp arm::ioe flags))
           (inexact (logbitp arm::ixe flags)))
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
  (let* ((current (%get-fpscr-control))
         (new current))
    (declare (fixnum current new))
    (when rounding-p
      (let* ((rc-bits (or
                       (cdr (assoc rounding-mode *rounding-mode-alist*))
                       (error "Unknown rounding mode: ~s" rounding-mode))))
        (declare (fixnum rc-bits))
        (setq new (dpb rc-bits (byte 2 22) new))))
    (when invalid-p
      (if invalid
        (bitsetf arm::ioe new)
        (bitclrf arm::ioe new)))
    (when overflow-p
      (if overflow
        (bitsetf arm::ofe new)
        (bitclrf arm::ofe new)))
    (when underflow-p
      (if underflow
        (bitsetf arm::ufe new)
        (bitclrf arm::ufe new)))
    (when zero-p
      (if division-by-zero
        (bitsetf arm::dze new)
        (bitclrf arm::dze new)))
    (when inexact-p
      (if inexact
        (bitsetf arm::ixe new)
        (bitclrf arm::ixe new)))
    (unless (= current new)
      (%set-fpscr-control new))
    (%get-fpscr)))


;;; Manipulating the FPSCR.  Keeping FP exception enable bits in
;;; the FPSCR doesn't do us a whole lot of good; the NEON doesn't
;;; support traps on FP exceptions, and some OSes (the World's
;;; Most Advanced, in particular) reboot when a process gets an
;;; enabled trapping FP exception on older hardware.
;;; So: we keep the (logical) enabled exception mask in tcr.lisp-fpscr,
;;; and just store the rounding mode in the hardware FPSCR.

(defarmlapfunction %get-fpscr-control ()
  (fmrx imm0 :fpscr)
  (ldr imm1 (:@ rcontext (:$ arm::tcr.lisp-fpscr)))
  (and imm0 imm0 (:$ (ash 3 22)))       ;rounding mode
  (and imm1 imm1 (:$ #xff00))
  (orr imm0 imm0 imm1)
  (box-fixnum arg_z imm0)
  (bx lr))

;;; Get the cumulative exception status bits out of the FPSCR.
(defarmlapfunction %get-fpscr-status ()
  (fmrx imm0 :fpscr)
  (and imm0 imm0 (:$ #xff))
  (box-fixnum arg_z imm0)
  (bx lr))

;;; Set (clear, usually) the cumulative exception status bits in the FPSCR.
(defarmlapfunction %set-fpscr-status ((new arg_z))
  (fmrx imm1 :fpscr)
  (unbox-fixnum imm0 new)
  (and imm0 imm0 (:$ #xff))
  (bic imm1 imm1 (:$ #xff))
  (orr imm0 imm0 imm1)
  (fmxr :fpscr imm0)
  (bx lr))

;;; Set the rounding mode directly in the FPSCR, and the exception enable
;;; bits in tcr.lisp-fpscr.
(defarmlapfunction %set-fpscr-control ((new arg_z))
  (unbox-fixnum imm0 new)
  (and imm1 imm0 (:$ #xff00))
  (str imm1 (:@ rcontext (:$ arm::tcr.lisp-fpscr)))
  (fmrx imm1 :fpscr)
  (bic imm1 imm1 (:$ (ash 3 22)))
  (and imm0 imm0 (:$ (ash 3 22)))
  (orr imm0 imm1 imm0)
  (fmxr :fpscr imm0)
  (bx lr))

(defarmlapfunction %get-fpscr ()
  (fmrx imm0 :fpscr)
  (bic imm0 imm0 (:$ #xff00))
  (ldr imm1 (:@ rcontext (:$ arm::tcr.lisp-fpscr)))
  (and imm1 imm1 (:$ #xff00))
  (orr imm0 imm1 imm0)
  (mov imm0 (:lsl imm0 (:$ 4)))
  (mov arg_z (:lsr imm0 (:$ (- 4 arm::fixnumshift))))
  (bx lr))

(defun fp-condition-name-from-fpscr-status (status)
  (cond
    ((logbitp arm::ioc status) 'floating-point-invalid-operation)
    ((logbitp arm::dzc status) 'division-by-zero)
    ((logbitp arm::ofc status) 'floating-point-overflow)
    ((logbitp arm::ufc status) 'floating-point-underflow)
    ((logbitp arm::ixc status) 'floating-point-inexact)))
     
 
;;; Don't we already have about 20 versions of this ?
(defarmlapfunction %double-float-from-macptr! ((ptr arg_x) (byte-offset arg_y) (dest arg_z))
  (ldr imm0 (:@ ptr (:$ arm::macptr.address)))
  (unbox-fixnum imm1 byte-offset)
  (ldrd imm0  (:@ imm0 imm1))
  (strd imm0 (:@ dest (:$ arm::double-float.value)))
  (bx lr))


;;; Copy a single float pointed at by the macptr in single
;;; to a double float pointed at by the macptr in double

(defarmlapfunction %single-float-ptr->double-float-ptr ((single arg_y) (double arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 single)
  (flds s0 (:@ imm0 (:$ 0)))
  (fcvtds d1 s0)
  (macptr-ptr imm0 double)
  (fstd d1 (:@ imm0 (:$ 0)))
  (bx lr))

;;; Copy a double float pointed at by the macptr in double
;;; to a single float pointed at by the macptr in single.
(defarmlapfunction %double-float-ptr->single-float-ptr ((double arg_y) (single arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 double)
  (fldd d0 (:@ imm0 (:$ 0)))
  (macptr-ptr imm0 single)
  (fcvtsd s2 d0)
  (fsts s2 (:@  imm0 (:$ 0)))
  (bx lr))


(defarmlapfunction %set-ieee-single-float-from-double ((src arg_y) (macptr arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 macptr)
  (get-double-float d1 src)
  (fcvtsd s0 d1)
  (fsts s0 (:@ imm0 (:$ 0)))
  (bx lr))


(defun host-single-float-from-unsigned-byte-32 (u32)
  (let* ((f (%make-sfloat)))
    (setf (uvref f arm::single-float.value-cell) u32)
    f))





(defun single-float-bits (f)
  (uvref f arm::single-float.value-cell))



(defun double-float-bits (f)
  (values (uvref f arm::double-float.val-high-cell)
          (uvref f arm::double-float.val-low-cell)))

(defun double-float-from-bits (high low)
  (let* ((f (%make-dfloat)))
    (setf (uvref f arm::double-float.val-high-cell) high
          (uvref f arm::double-float.val-low-cell) low)
    f))

(defarmlapfunction %double-float-sign ((n arg_z))
  (ldr imm0 (:@ n (:$ arm::double-float.val-high)))
  (cmp imm0 (:$ 0))
  (mov arg_z 'nil)
  (addlt arg_z arg_z (:$ arm::t-offset))
  (bx lr))

(defarmlapfunction %short-float-sign ((n arg_z))
  (ldr imm0 (:@ n (:$ arm::single-float.value)))
  (cmp imm0 (:$ 0))
  (mov arg_z 'nil)
  (addlt arg_z arg_z (:$ arm::t-offset))
  (bx lr))

(defarmlapfunction %single-float-sqrt! ((src arg_y) (dest arg_z))
  (build-lisp-frame)
  (get-single-float s0 src imm0)
  (fmrx imm0 :fpscr)
  (bic imm0 imm0 (:$ #xff))
  (fmxr :fpscr imm0)
  (fsqrts s1 s0)
  (sploadlr .SPcheck-fpu-exception)
  (blx lr)
  (put-single-float s1 dest imm0)
  (return-lisp-frame))



(defarmlapfunction %double-float-sqrt! ((src arg_y) (dest arg_z))
  (build-lisp-frame)
  (get-double-float d0 src)
  (fmrx imm0 :fpscr)
  (bic imm0 imm0 (:$ #xff))
  (fmxr :fpscr imm0)
  (fsqrtd d1 d0)
  (sploadlr .SPcheck-fpu-exception)
  (blx lr)
  (put-double-float d1 dest)
  (return-lisp-frame))


