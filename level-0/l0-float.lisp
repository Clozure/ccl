;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
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
;;;
;;; level-0;l0-float.lisp

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")
  (require :number-case-macro) 
)

;;; used by float reader
(defun make-float-from-fixnums (hi lo exp sign &optional result)
  ;;(require-null-or-double-float-sym result)
  ;; maybe nuke all these require-types?
  ;;(setq hi (require-type hi 'fixnum))
  ;;(setq lo (require-type lo 'fixnum))
  ;;(setq exp (require-type exp 'fixnum))
  ;;(setq sign (require-type sign 'fixnum))
  (let ((the-float (or result (%make-dfloat))))
    (%make-float-from-fixnums the-float hi lo exp sign)
    the-float))


#+32-bit-target
(defun make-short-float-from-fixnums (significand biased-exp sign &optional result)
  (%make-short-float-from-fixnums (or result (%make-sfloat)) significand biased-exp sign))

#+64-bit-target
(defun make-short-float-from-fixnums (significand biased-exp sign)
  (declare (fixnum significand biased-exp sign))
  (host-single-float-from-unsigned-byte-32
   (logior
    (the fixnum (if (< sign 0) (ash 1 31) 0))
    (the fixnum (ash biased-exp IEEE-single-float-exponent-offset))
    (the fixnum (logand significand
                        (1- (ash 1 IEEE-single-float-hidden-bit)))))))


(defun float-sign (n1 &optional n2) ; second arg silly
  "Return a floating-point number that has the same sign as
   FLOAT1 and, if FLOAT2 is given, has the same absolute value
   as FLOAT2."
  (if (and n2 (not (typep n2 'float)))
    (setq n2 (require-type n2 'float)))
  (number-case n1
    (double-float                       
     (if (%double-float-sign n1) 
       (if n2
         (if (if (typep n2 'double-float) (%double-float-minusp n2) (%short-float-minusp n2)) n2 (- n2))
         -1.0d0)
       (if n2
         (if (if (typep n2 'double-float) (%double-float-minusp n2) (%short-float-minusp n2)) (- n2) n2)
         1.0d0)))
    (short-float
     (if (%short-float-sign n1)
       (if n2
         (if (if (typep n2 'double-float) (%double-float-minusp n2) (%short-float-minusp n2)) n2 (- n2))
         -1.0s0)
       (if n2
         (if (if (typep n2 'double-float) (%double-float-minusp n2) (%short-float-minusp n2)) (- n2) n2)
         1.0s0)))))



(defun %double-float-minusp (n)
  (and (%double-float-sign n)(not (%double-float-zerop n))))

(defun %short-float-minusp (n)
  (and (%short-float-sign n) (not (%short-float-zerop n))))

(defun %double-float-abs (n)
  (if (not (%double-float-sign n))
    n 
    (%%double-float-abs! n (%make-dfloat))))

#+32-bit-target
(defun %short-float-abs (n)
  (if (not (%short-float-sign n))
    n 
    (%%short-float-abs! n (%make-sfloat))))

(defun fixnum-decode-float (n)
  (etypecase n
    (double-float (%integer-decode-double-float n))))

(defun nan-or-infinity-p (n)
  (etypecase n
    (double-float (eq 2047 (%double-float-exp n)))
    (short-float (eq 255 (%short-float-exp n)))))

; not sure this is right
(defun infinity-p (n)
  (etypecase n
    (double-float (multiple-value-bind (hi lo exp)(fixnum-decode-float n)
                    (and (eq 2047 exp)
                         (eq #x1000000 hi)
                         (eq 0 lo))))
    (short-float
     #+32-bit-target
     (multiple-value-bind (high low)(%sfloat-hwords n)
                  (let*  ((mantissa (%ilogior2 low (%ilsl 16 (%ilogand2 high #x007F))))
                          (exp (%ilsr 7 (%ilogand2 high #x7F80))))
                    (and (eq exp 255)
                         (eq 0 mantissa))))
     #+64-bit-target
     (let* ((bits (single-float-bits n))
            (exp (ldb (byte IEEE-single-float-exponent-width
                            IEEE-single-float-exponent-offset)
                      bits))
            (mantissa (ldb (byte IEEE-single-float-mantissa-width
                            IEEE-single-float-mantissa-offset)
                           bits)))
       (declare (fixnum bits exp mantissa))
       (and (= exp 255)
            (zerop mantissa))))))

#+32-bit-target
(defun fixnum-decode-short-float (float)
  (multiple-value-bind (high low)(%sfloat-hwords float)
    (let*  ((mantissa (%ilogior2 low (%ilsl 16 (%ilogand2 high #x007F))))
            (exp (%ilsr 7 (%ilogand2 high #x7F80))))
      (if (and (neq exp 0)(neq exp 255))(setq mantissa (%ilogior mantissa #x800000)))
      (values mantissa exp (%ilsr 15 high)))))

#+64-bit-target
(defun fixnum-decode-short-float (float)
  (let* ((bits (single-float-bits float)))
    (declare (fixnum bits))
    (let* ((mantissa (ldb (byte IEEE-single-float-mantissa-width
                                IEEE-single-float-mantissa-offset)
                          bits))
           (exp (ldb (byte IEEE-single-float-exponent-width
                           IEEE-single-float-exponent-offset)
                     bits))
           (sign (lsh bits -31)))
      (declare (fixnum mantissa exp sign))
      (unless (or (= exp 0) (= exp 255))
        (setq mantissa (logior mantissa (ash 1 IEEE-single-float-hidden-bit))))
      (values mantissa exp sign))))
                  
                   

#+32-bit-target
(defun integer-decode-double-float (n)
  (multiple-value-bind (hi lo exp sign)(%integer-decode-double-float n)
    ; is only 53 bits and positive so should be easy
    ;(values (logior (ash hi 28) lo) exp sign)))
    ; if denormalized, may fit in a fixnum
    (setq exp (- exp (if (< hi #x1000000) 
                       (+ IEEE-double-float-mantissa-width IEEE-double-float-bias)
                       (+ IEEE-double-float-mantissa-width (1+ IEEE-double-float-bias)))))
    (if (< hi (ash 1 (1- target::fixnumshift))) ; aka 2
      (values (logior (ash hi 28) lo) exp sign)
      ; might fit in 1 word?
      (let ((big (%alloc-misc 2 target::subtag-bignum)))
        (make-big-53 hi lo big)
        (if (< hi #x1000000) (%normalize-bignum big))
        (values big exp sign)))))

#+64-bit-target
(defun integer-decode-double-float (n)
  (multiple-value-bind (hi lo exp sign)(%integer-decode-double-float n)
    (setq exp (- exp (if (< hi #x1000000) 
                       (+ IEEE-double-float-mantissa-width IEEE-double-float-bias)
                       (+ IEEE-double-float-mantissa-width (1+ IEEE-double-float-bias)))))
    (values (logior (ash hi 28) lo) exp sign)))
    

;;; actually only called when magnitude bigger than a fixnum
#+32-bit-target
(defun %truncate-double-float (n)
  (multiple-value-bind (hi lo exp sign)(%integer-decode-double-float n)
    (if (< exp (1+ IEEE-double-float-bias)) ; this is false in practice
      0
      (progn
        (setq exp (- exp (+ IEEE-double-float-mantissa-width (1+ IEEE-double-float-bias))))
        (if (eq sign 1)  ; positive
          (logior (ash hi (+ 28 exp))(ash lo exp))
          (if (<= exp 0) ; exp positive - negate before shift - else after
            (let ((poo (logior (ash hi (+ 28 exp))(ash lo exp))))
              (- poo))
            (let ((poo (logior (ash hi 28) lo)))
              (ash (- poo) exp))))))))

#+64-bit-target
(defun %truncate-double-float (n)
  (multiple-value-bind (mantissa exp sign) (integer-decode-float n)
    (* sign (ash mantissa exp))))



; actually only called when bigger than a fixnum
(defun %truncate-short-float (n)
  (multiple-value-bind (mantissa exp sign)(fixnum-decode-short-float n)
    (if (< exp (1+ IEEE-single-float-bias)) ; is magnitude less than 1 - false in practice
      0
      (progn
        (setq exp (- exp (+ IEEE-single-float-mantissa-width (1+ IEEE-single-float-bias))))
        (ash (if (eq sign 0) mantissa (- mantissa)) exp)))))

(defun decode-float (n)
  "Return three values:
   1) a floating-point number representing the significand. This is always
      between 0.5 (inclusive) and 1.0 (exclusive).
   2) an integer representing the exponent.
   3) -1.0 or 1.0 (i.e. the sign of the argument.)"
  (number-case n
    (double-float
     (let* ((old-exp (%double-float-exp n))
            (sign (if (%double-float-sign n) -1.0d0 1.0d0)))    
       (if (eq 0 old-exp)
         (if (%double-float-zerop n)
           (values 0.0d0 0 sign)
           (let* ((val (%make-dfloat))
                  (zeros (dfloat-significand-zeros n)))
	     (%%double-float-abs! n val)
             (%%scale-dfloat! val (+ 2 IEEE-double-float-bias zeros) val) ; get it normalized
             (set-%double-float-exp val IEEE-double-float-bias)      ; then bash exponent
             (values val (- old-exp zeros IEEE-double-float-bias) sign)))
         (if (> old-exp IEEE-double-float-normal-exponent-max)
           (error "Can't decode NAN or infinity ~s" n)
           (let ((val (%make-dfloat)))
             (%%double-float-abs! n val)
             (set-%double-float-exp val IEEE-double-float-bias)
             (values val (- old-exp IEEE-double-float-bias) sign))))))
    (short-float
     (let* ((old-exp (%short-float-exp n))
            (sign (if (%short-float-sign n) -1.0s0 1.0s0)))
       (if (eq 0 old-exp)
         (if (%short-float-zerop n)
           (values 0.0s0 0 sign)
           #+32-bit-target
           (let* ((val (%make-sfloat))
                  (zeros (sfloat-significand-zeros n)))
	     (%%short-float-abs! n val)
             (%%scale-sfloat! val (+ 2 IEEE-single-float-bias zeros) val) ; get it normalized
             (set-%short-float-exp val IEEE-single-float-bias)      ; then bash exponent
             (values val (- old-exp zeros IEEE-single-float-bias) sign))
           #+64-bit-target
           (let* ((zeros (sfloat-significand-zeros n))
                  (val (%%scale-sfloat (%short-float-abs n)
				       (+ 2 IEEE-single-float-bias zeros))))
             (values (set-%short-float-exp val IEEE-single-float-bias)
                     (- old-exp zeros IEEE-single-float-bias) sign)))
         (if (> old-exp IEEE-single-float-normal-exponent-max)
           (error "Can't decode NAN or infinity ~s" n)
           #+32-bit-target
           (let ((val (%make-sfloat)))
             (%%short-float-abs! n val)
             (set-%short-float-exp val IEEE-single-float-bias)
             (values val (- old-exp IEEE-single-float-bias) sign))
           #+64-bit-target
	   (values (set-%short-float-exp (%short-float-abs n)
					 IEEE-single-float-bias)
		   (- old-exp IEEE-single-float-bias) sign)))))))

; (* float (expt 2 int))
(defun scale-float (float int)
  "Return the value (* f (expt (float 2 f) ex)), but with no unnecessary loss
  of precision or overflow."
  (unless (fixnump int)(setq int (require-type int 'fixnum)))
  (number-case float
    (double-float
     (let* ((float-exp (%double-float-exp float))
            (new-exp (+ float-exp int)))
       (if (eq 0 float-exp) ; already denormalized?
         (if (%double-float-zerop float)
           float 
           (let ((result (%make-dfloat)))
             (%%scale-dfloat! float (+ (1+ IEEE-double-float-bias) int) result)))
         (if (<= new-exp 0)  ; maybe going denormalized        
           (if (<= new-exp (- IEEE-double-float-digits))
             0.0d0 ; should this be underflow? - should just be normal and result is fn of current fpu-mode
             ;(error "Can't scale ~s by ~s." float int) ; should signal something                      
             (let ((result (%make-dfloat)))
               (%copy-double-float float result)
               (set-%double-float-exp result 1) ; scale by float-exp -1
               (%%scale-dfloat! result (+ IEEE-double-float-bias (+ float-exp int)) result)              
               result))
           (if (> new-exp IEEE-double-float-normal-exponent-max) 
             (error (make-condition 'floating-point-overflow
                                    :operation 'scale-float
                                    :operands (list float int)))
             (let ((new-float (%make-dfloat)))
               (%copy-double-float float new-float)
               (set-%double-float-exp new-float new-exp)
               new-float))))))
    (short-float
     (let* ((float-exp (%short-float-exp float))
            (new-exp (+ float-exp int)))
       (if (eq 0 float-exp) ; already denormalized?
         (if (%short-float-zerop float)
           float
           #+32-bit-target
           (let ((result (%make-sfloat)))
             (%%scale-sfloat! float (+ (1+ IEEE-single-float-bias) int) result))
           #+64-bit-target
           (%%scale-sfloat float (+ (1+ IEEE-single-float-bias) int)))
         (if (<= new-exp 0)  ; maybe going denormalized        
           (if (<= new-exp (- IEEE-single-float-digits))
             ;; should this be underflow? - should just be normal and
             ;; result is fn of current fpu-mode (error "Can't scale
             ;; ~s by ~s." float int) ; should signal something
             0.0s0
             #+32-bit-target
             (let ((result (%make-sfloat)))
               (%copy-short-float float result)
               (set-%short-float-exp result 1) ; scale by float-exp -1
               (%%scale-sfloat! result (+ IEEE-single-float-bias (+ float-exp int)) result)              
               result)
             #+64-bit-target
             (%%scale-sfloat (set-%short-float-exp float 1)
                             (+ IEEE-single-float-bias (+ float-exp int))))
           (if (> new-exp IEEE-single-float-normal-exponent-max) 
             (error (make-condition 'floating-point-overflow
                                    :operation 'scale-float
                                    :operands (list float int)))
             #+32-bit-target
             (let ((new-float (%make-sfloat)))
               (%copy-short-float float new-float)
               (set-%short-float-exp new-float new-exp)
               new-float)
             #+64-bit-target
             (set-%short-float-exp float new-exp))))))))

(defun %copy-float (f)
  ;Returns a freshly consed float.  float can also be a macptr.
  (cond ((double-float-p f) (%copy-double-float f (%make-dfloat)))
        ((macptrp f)
         (let ((float (%make-dfloat)))
           (%copy-ptr-to-ivector f 0 float (* 4 target::double-float.value-cell) 8)
           float))
        (t (error "Illegal arg ~s to %copy-float" f))))

(defun float-precision (float)     ; not used - not in cltl2 index ?
  "Return a non-negative number of significant digits in its float argument.
  Will be less than FLOAT-DIGITS if denormalized or zero."
  (number-case float
     (double-float
      (if (eq 0 (%double-float-exp float))
        (if (not (%double-float-zerop float))
        ; denormalized
          (- IEEE-double-float-mantissa-width (dfloat-significand-zeros float))
          0)
        IEEE-double-float-digits))
     (short-float 
      (if (eq 0 (%short-float-exp float))
        (if (not (%short-float-zerop float))
        ; denormalized
          (- IEEE-single-float-mantissa-width (sfloat-significand-zeros float))
          0)
        IEEE-single-float-digits))))


(defun %double-float (number &optional result)
  ;(require-null-or-double-float-sym result)
  ; use number-case when macro is common
  (number-case number
    (double-float
     (if result 
       (%copy-double-float number result)
         number))
    (short-float
     (%short-float->double-float number (or result (%make-dfloat))))
    (fixnum
     (%fixnum-dfloat number (or result (%make-dfloat))))
    (bignum (%bignum-dfloat number result))
    (ratio 
     (if (not result)(setq result (%make-dfloat)))
     (let* ((num (%numerator number))
            (den (%denominator number)))
       ; dont error if result is floatable when either top or bottom is not.
       ; maybe do usual first, catching error
       (if (not (or (bignump num)(bignump den)))
         (with-stack-double-floats ((fnum num)
                                        (fden den))       
             (%double-float/-2! fnum fden result))
         (let* ((numlen (integer-length num))
                (denlen (integer-length den))
                (exp (- numlen denlen))
                (minusp (minusp num)))
           (if (and (<= numlen IEEE-double-float-bias)
                    (<= denlen IEEE-double-float-bias)
                    #|(not (minusp exp))|# 
                    (<= (abs exp) IEEE-double-float-mantissa-width))
             (with-stack-double-floats ((fnum num)
                                            (fden den))
       
               (%double-float/-2! fnum fden result))
             (if (> exp IEEE-double-float-mantissa-width)
               (progn  (%double-float (round num den) result))               
               (if (>= exp 0)
                 ; exp between 0 and 53 and nums big
                 (let* ((shift (- IEEE-double-float-digits exp))
                        (num (if minusp (- num) num))
                        (int (round (ash num shift) den)) ; gaak
                        (intlen (integer-length int))
                        (new-exp (+ intlen (- IEEE-double-float-bias shift))))
                   
                   (when (> intlen IEEE-double-float-digits)
                     (setq shift (1- shift))
                     (setq int (round (ash num shift) den))
                     (setq intlen (integer-length int))
                     (setq new-exp (+ intlen (- IEEE-double-float-bias shift))))
                   (when (> new-exp 2046)
                     (error (make-condition 'floating-point-overflow
                                            :operation 'double-float
                                            :operands (list number))))
		   (make-float-from-fixnums (ldb (byte 25 (- intlen 25)) int)
					    (ldb (byte 28 (max (- intlen 53) 0)) int)
					    new-exp ;(+ intlen (- IEEE-double-float-bias 53))
					    (if minusp -1 1)
					    result))
                 ; den > num - exp negative
                 (progn  
                   (float-rat-neg-exp num den (if minusp -1 1) result)))))))))))


#+32-bit-target
(defun %short-float-ratio (number &optional result)
  (if (not result)(setq result (%make-sfloat)))
  (let* ((num (%numerator number))
         (den (%denominator number)))
    ;; dont error if result is floatable when either top or bottom is
    ;; not.  maybe do usual first, catching error
    (if (not (or (bignump num)(bignump den)))
      (target::with-stack-short-floats ((fnum num)
				       (fden den))       
        (%short-float/-2! fnum fden result))
      (let* ((numlen (integer-length num))
             (denlen (integer-length den))
             (exp (- numlen denlen))
             (minusp (minusp num)))
        (if (and (<= numlen IEEE-single-float-bias)
                 (<= denlen IEEE-single-float-bias)
                 #|(not (minusp exp))|# 
                 (<= (abs exp) IEEE-single-float-mantissa-width))
          (target::with-stack-short-floats ((fnum num)
					   (fden den))
            (%short-float/-2! fnum fden result))
          (if (> exp IEEE-single-float-mantissa-width)
            (progn  (%short-float (round num den) result))               
            (if (>= exp 0)
              ; exp between 0 and 23 and nums big
              (let* ((shift (- IEEE-single-float-digits exp))
                     (num (if minusp (- num) num))
                     (int (round (ash num shift) den)) ; gaak
                     (intlen (integer-length int))
                     (new-exp (+ intlen (- IEEE-single-float-bias shift))))
		(when (> intlen IEEE-single-float-digits)
                  (setq shift (1- shift))
                  (setq int (round (ash num shift) den))
                  (setq intlen (integer-length int))
                  (setq new-exp (+ intlen (- IEEE-single-float-bias shift))))
                (when (> new-exp IEEE-single-float-normal-exponent-max)
                  (error (make-condition 'floating-point-overflow
                                         :operation 'short-float
                                         :operands (list number))))
                (make-short-float-from-fixnums 
                   (ldb (byte IEEE-single-float-digits  (- intlen  IEEE-single-float-digits)) int)
                   new-exp
                   (if minusp -1 1)
                   result))
              ; den > num - exp negative
              (progn  
                (float-rat-neg-exp num den (if minusp -1 1) result t)))))))))

#+64-bit-target
(defun %short-float-ratio (number)
  (let* ((num (%numerator number))
         (den (%denominator number)))
    ;; dont error if result is floatable when either top or bottom is
    ;; not.  maybe do usual first, catching error
    (if (not (or (bignump num)(bignump den)))
      (/ (the short-float (%short-float num))
         (the short-float (%short-float den)))
      (let* ((numlen (integer-length num))
             (denlen (integer-length den))
             (exp (- numlen denlen))
             (minusp (minusp num)))
        (if (and (<= numlen IEEE-single-float-bias)
                 (<= denlen IEEE-single-float-bias)
                 #|(not (minusp exp))|# 
                 (<= (abs exp) IEEE-single-float-mantissa-width))
          (/ (the short-float (%short-float num))
             (the short-float (%short-float den)))
          (if (> exp IEEE-single-float-mantissa-width)
            (progn  (%short-float (round num den)))
            (if (>= exp 0)
              ; exp between 0 and 23 and nums big
              (let* ((shift (- IEEE-single-float-digits exp))
                     (num (if minusp (- num) num))
                     (int (round (ash num shift) den)) ; gaak
                     (intlen (integer-length int))
                     (new-exp (+ intlen (- IEEE-single-float-bias shift))))
		(when (> intlen IEEE-single-float-digits)
                  (setq shift (1- shift))
                  (setq int (round (ash num shift) den))
                  (setq intlen (integer-length int))
                  (setq new-exp (+ intlen (- IEEE-single-float-bias shift))))
                (when (> new-exp IEEE-single-float-normal-exponent-max)
                  (error (make-condition 'floating-point-overflow
                                         :operation 'short-float
                                         :operands (list number))))
                (make-short-float-from-fixnums 
                   (ldb (byte IEEE-single-float-digits  (- intlen  IEEE-single-float-digits)) int)
                   new-exp
                   (if minusp 1 0)))
              ; den > num - exp negative
              (progn  
                (float-rat-neg-exp num den (if minusp -1 1) nil t)))))))))


#+32-bit-target
(defun %short-float (number &optional result)
  (number-case number
    (short-float
     (if result (%copy-short-float number result) number))
    (double-float
     (%double-float->short-float number (or result (%make-sfloat))))
    (fixnum
     (%fixnum-sfloat number (or result (%make-sfloat))))
    (bignum
     (%bignum-sfloat number (or result (%make-sfloat))))
    (ratio
     (%short-float-ratio number result))))

#+64-bit-target
(defun %short-float (number)
  (number-case number
    (short-float number)
    (double-float (%double-float->short-float number))
    (fixnum (%fixnum-sfloat number))
    (bignum (%bignum-sfloat number))
    (ratio (%short-float-ratio number))))


(defun float-rat-neg-exp (integer divisor sign &optional result short)
  (if (minusp sign)(setq integer (- integer)))       
  (let* ((integer-length (integer-length integer))
         ;; make sure we will have enough bits in the quotient
         ;; (and a couple extra for rounding)
         (shift-factor (+ (- (integer-length divisor) integer-length) (if short 28 60))) ; fix
         (scaled-integer integer))
    (if (plusp shift-factor)
      (setq scaled-integer (ash integer shift-factor))
      (setq divisor (ash divisor (- shift-factor)))  ; assume div > num
      )
    ;(pprint (list shift-factor scaled-integer divisor))
    (multiple-value-bind (quotient remainder)(floor scaled-integer divisor)
      (unless (zerop remainder) ; whats this - tells us there's junk below
        (setq quotient (logior quotient 1)))
      ;; why do it return 2 values?
      (values (float-and-scale-and-round sign quotient (- shift-factor)  short result)))))



;;; when is (negate-bignum (bignum-ashift-right big)) ; can't negate
;;; in place cause may get bigger cheaper than (negate-bignum big) - 6
;;; 0r 8 digits ; 8 longs so win if digits > 7 or negate it on the
;;; stack

(defun %bignum-dfloat (big &optional result)  
  (let* ((minusp (bignum-minusp big)))
    (flet 
      ((doit (new-big)
         (let* ((int-len (bignum-integer-length new-big)))
           (when (>= int-len (- 2047 IEEE-double-float-bias)) ; args?
             (error (make-condition 'floating-point-overflow 
                                    :operation 'float :operands (list big))))
           (if (> int-len 53)
             (let* ((hi (ldb (byte 25  (- int-len  25)) new-big))
                    (lo (ldb (byte 28 (- int-len 53)) new-big)))
               ;(print (list new-big hi lo))
               (when (and (logbitp (- int-len 54) new-big)  ; round bit
                          (or (%ilogbitp 0 lo)    ; oddp
                              ;; or more bits below round
                              (%i< (one-bignum-factor-of-two new-big) (- int-len 54))))
                 (if (eq lo #xfffffff)
                   (setq hi (1+ hi) lo 0)
                   (setq lo (1+ lo)))
                 (when (%ilogbitp 25 hi) ; got bigger
                   (setq int-len (1+ int-len))
                   (let ((bit (%ilogbitp 0 hi)))
                     (setq hi (%ilsr 1 hi))
                     (setq lo (%ilsr 1 lo))
                     (if bit (setq lo (%ilogior #x8000000 lo))))))
               (make-float-from-fixnums hi lo (+ IEEE-double-float-bias int-len)(if minusp -1 1) result))
             (let* ((hi (ldb (byte 25  (- int-len  25)) new-big))
                    (lobits (min (- int-len 25) 28))
                    (lo (ldb (byte lobits (- int-len (+ lobits 25))) new-big)))
               (if (< lobits 28) (setq lo (ash lo (- 28 lobits))))
               (make-float-from-fixnums hi lo (+ IEEE-double-float-bias int-len) (if minusp -1 1) result))))))
      (declare (dynamic-extent #'doit))
      (with-one-negated-bignum-buffer big doit))))

#+32-bit-target
(defun %bignum-sfloat (big &optional result)  
  (let* ((minusp (bignum-minusp big)))
    (flet 
      ((doit (new-big)
         (let* ((int-len (bignum-integer-length new-big)))
           (when (>= int-len (- 255 IEEE-single-float-bias)) ; args?
             (error (make-condition 'floating-point-overflow 
                                    :operation 'float :operands (list big 1.0s0))))
           (if t ;(> int-len IEEE-single-float-digits) ; always true
             (let* ((lo (ldb (byte IEEE-single-float-digits  (- int-len  IEEE-single-float-digits)) new-big)))
               (when (and (logbitp (- int-len 25) new-big)  ; round bit
                          (or (%ilogbitp 0 lo)    ; oddp
                              ; or more bits below round
                              (%i< (one-bignum-factor-of-two new-big) (- int-len 25))))
                 (setq lo (1+ lo))
                 (when (%ilogbitp 24 lo) ; got bigger
                   (setq int-len (1+ int-len))
                   (setq lo (%ilsr 1 lo))))
               (make-short-float-from-fixnums  lo (+ IEEE-single-float-bias int-len)(if minusp -1 1) result))
             ))))
      (declare (dynamic-extent #'doit))
      (with-one-negated-bignum-buffer big doit))))


#+64-bit-target
(defun %bignum-sfloat (big)  
  (let* ((minusp (bignum-minusp big)))
    (flet 
      ((doit (new-big)
         (let* ((int-len (bignum-integer-length new-big)))
           (when (>= int-len (- 255 IEEE-single-float-bias)) ; args?
             (error (make-condition 'floating-point-overflow 
                                    :operation 'float :operands (list big 1.0s0))))
           (if t ;(> int-len IEEE-single-float-digits) ; always true
             (let* ((lo (ldb (byte IEEE-single-float-digits  (- int-len  IEEE-single-float-digits)) new-big)))
               (when (and (logbitp (- int-len 25) new-big)  ; round bit
                          (or (%ilogbitp 0 lo)    ; oddp
                              ; or more bits below round
                              (%i< (one-bignum-factor-of-two new-big) (- int-len 25))))
                 (setq lo (1+ lo))
                 (when (%ilogbitp 24 lo) ; got bigger
                   (setq int-len (1+ int-len))
                   (setq lo (%ilsr 1 lo))))
               (make-short-float-from-fixnums  lo (+ IEEE-single-float-bias int-len)(if minusp -1 1)))
             ))))
      (declare (dynamic-extent #'doit))
      (with-one-negated-bignum-buffer big doit))))




(defun %fixnum-dfloat (fix &optional result)  
  (if (eq 0 fix) 
    (if result (%copy-double-float 0.0d0 result) 0.0d0)
    (progn
      (when (not result)(setq result (%make-dfloat)))
      ; it better return result
      (%int-to-dfloat fix result))))


#+32-bit-target
(defun %fixnum-sfloat (fix &optional result)
  (if (eq 0 fix)
    (if result (%copy-short-float 0.0s0 result) 0.0s0)
    (%int-to-sfloat! fix (or result (%make-sfloat)))))

#+64-bit-target
(defun %fixnum-sfloat (fix)
  (if (eq 0 fix)
    0.0s0
    (%int-to-sfloat fix)))

;;; Transcendental functions.
(defun sin (x)
  "Return the sine of NUMBER."
  (if (complexp x)
    (let* ((r (realpart x))
           (i (imagpart x)))
      (complex (* (sin r) (cosh i))
               (* (cos r) (sinh i))))
    (if (typep x 'double-float)
      (%double-float-sin! x (%make-dfloat))
      #+32-bit-target
      (target::with-stack-short-floats ((sx x))
        (%single-float-sin! sx (%make-sfloat)))
      #+64-bit-target
      (%single-float-sin (%short-float x)))))

(defun cos (x)
  "Return the cosine of NUMBER."
  (if (complexp x)
    (let* ((r (realpart x))
           (i (imagpart x)))
      (complex (* (cos r) (cosh i))
               (- (* (sin r) (sinh i)))))
    (if (typep x 'double-float)
      (%double-float-cos! x (%make-dfloat))
      #+32-bit-target
      (target::with-stack-short-floats ((sx x))
        (%single-float-cos! sx (%make-sfloat)))
      #+64-bit-target
      (%single-float-cos (%short-float x)))))

(defun tan (x)
  "Return the tangent of NUMBER."
  (if (complexp x)
    (/ (sin x) (cos x))
    (if (typep x 'double-float)
      (%double-float-tan! x (%make-dfloat))
      #+32-bit-target
      (target::with-stack-short-floats ((sx x))
        (%single-float-tan! sx (%make-sfloat)))
      #+64-bit-target
      (%single-float-tan (%short-float x))
      )))




(defun atan (y &optional (x nil x-p))
  "Return the arc tangent of Y if X is omitted or Y/X if X is supplied."
  (if x-p
    (if (or (typep x 'double-float)
            (typep y 'double-float))
      (with-stack-double-floats ((dy y)
                                 (dx x))
        (%df-atan2 dy dx))
      #+32-bit-target
      (target::with-stack-short-floats ((sy y)
                                (sx x))
        (%sf-atan2! sy sx))
      #+64-bit-target
      (%sf-atan2 (%short-float y) (%short-float x)))
    (if (typep y 'complex)
      (let* ((iy (* (sqrt -1) y)))
             (/ (- (log (+ 1 iy)) (log (- 1 iy)))
                #c(0 2)))
      (if (typep y 'double-float)
        (%double-float-atan! y (%make-dfloat))
        #+32-bit-target
        (target::with-stack-short-floats ((sy y))
          (%single-float-atan! sy (%make-sfloat)))
        #+64-bit-target
        (%single-float-atan (%short-float y))
        ))))



(defun log (x &optional (b nil b-p))
  "Return the logarithm of NUMBER in the base BASE, which defaults to e."
  (if b-p
    (if (zerop b)
      (if (zerop x)
        (report-bad-arg x '(not (satisfies zerop) ))
        (if (floatp x) (float 0.0d0 x) 0))
      (/ (log-e x) (log-e b)))
    (log-e x)))

(defun log-e (x)
  (cond 
    ((bignump x)
     (if (minusp x)
       (complex (log-e (- x)) pi)
       (let* ((base1 3)
              (guess (floor (1- (integer-length x))
                            (log base1 2)))
              (guess1 (* guess (log-e base1))))
         (+ guess1 (log-e (/ x (expt base1 guess)))))))
    ((and (ratiop x)  
          (or (> x most-positive-short-float)
              (< x most-negative-short-float)))
     (- (log-e (%numerator x)) (log-e (%denominator x))))
    ((typep x 'complex)
     (complex (log-e (abs x)) (phase x)))
    ((typep x 'double-float)
     (with-stack-double-floats ((dx x))
       (if (minusp x)
         (complex (%double-float-log! (%%double-float-abs! dx dx) (%make-dfloat)) pi)
         (%double-float-log! dx (%make-dfloat)))))
    (t
     #+32-bit-target
     (target::with-stack-short-floats ((sx x))
       (if (minusp x)
         (complex (%single-float-log! (%%short-float-abs! sx sx) (%make-sfloat))
                  #.(coerce pi 'short-float))
         (%single-float-log! sx (%make-sfloat))))
     #+64-bit-target
     (if (minusp x)
       (complex (%single-float-log (%short-float-abs (%short-float x))) #.(coerce pi 'single-float))
       (%single-float-log (%short-float x)))
     )))



(defun exp (x)
  "Return e raised to the power NUMBER."
  (typecase x
    (complex (* (exp (realpart x)) (cis (imagpart x))))
    (double-float (%double-float-exp! x (%make-dfloat)))
    (t
     #+32-bit-target
     (target::with-stack-short-floats ((sx x))
       (%single-float-exp! sx (%make-sfloat)))
     #+64-bit-target
     (%single-float-exp (%short-float x)))))



(defun expt (b e)
  "Return BASE raised to the POWER."
  (cond ((zerop e) (1+ (* b e)))
	((integerp e)
         (if (minusp e) (/ 1 (%integer-power b (- e))) (%integer-power b e)))
        ((zerop b)
         (if (plusp (realpart e)) b (report-bad-arg e '(number (0) *))))
        ((and (realp b) (plusp b) (realp e))
         (if (or (typep b 'double-float)
                 (typep e 'double-float))
           (with-stack-double-floats ((b1 b)
                                      (e1 e))
             (%double-float-expt! b1 e1 (%make-dfloat)))
           #+32-bit-target
           (target::with-stack-short-floats ((b1 b)
                                     (e1 e))
             (%single-float-expt! b1 e1 (%make-sfloat)))
           #+64-bit-target
           (%single-float-expt (%short-float b) (%short-float e))
           ))
        (t (exp (* e (log b))))))



(defun sqrt (x &aux a b)
  "Return the square root of NUMBER."
  (cond ((zerop x) x)
        ((complexp x) (* (sqrt (abs x)) (cis (/ (phase x) 2))))          
        ((minusp x) (complex 0 (sqrt (- x))))
        ((floatp x)
         (fsqrt x))
        ((and (integerp x) (eql x (* (setq a (isqrt x)) a))) a)
        ((and (ratiop x)
              (let ((n (numerator x))
                    d)
                (and (eql n (* (setq a (isqrt n)) a))
                     (eql (setq d (denominator x))
                          (* (setq b (isqrt d)) b)))))
         (/ a b))          
        (t
         #+32-bit-target
         (target::with-stack-short-floats ((f1))
           (fsqrt (%short-float x f1)))
         #+64-bit-target
         (fsqrt (%short-float x)))))



(defun asin (x)
  "Return the arc sine of NUMBER."
  (number-case x
    (complex
      (let ((sqrt-1-x (sqrt (- 1 x)))
            (sqrt-1+x (sqrt (+ 1 x))))
        (complex (atan (/ (realpart x)
                          (realpart (* sqrt-1-x sqrt-1+x))))
                 (asinh (imagpart (* (conjugate sqrt-1-x)
                                     sqrt-1+x))))))
    (double-float
     (locally (declare (type double-float x))
       (if (and (<= -1.0d0 x)
		(<= x 1.0d0))
	 (%double-float-asin! x (%make-dfloat))
	 (let* ((temp (+ (complex -0.0d0 x)
			 (sqrt (- 1.0d0 (the double-float (* x x)))))))
	   (complex (phase temp) (- (log (abs temp))))))))
    ((short-float rational)
     #+32-bit-target
     (let* ((x1 (%make-sfloat)))
       (declare (dynamic-extent x1))
       (if (and (realp x) 
		(<= -1.0s0 (setq x (%short-float x x1)))
		(<= x 1.0s0))
	 (%single-float-asin! x1 (%make-sfloat))
	 (progn
	   (setq x (+ (complex (- (imagpart x)) (realpart x))
		      (sqrt (- 1 (* x x)))))
	   (complex (phase x) (- (log (abs x)))))))
     #+64-bit-target
     (if (and (realp x) 
              (<= -1.0s0 (setq x (%short-float x)))
              (<= x 1.0s0))
	 (%single-float-asin x)
	 (progn
	   (setq x (+ (complex (- (imagpart x)) (realpart x))
		      (sqrt (- 1 (* x x)))))
	   (complex (phase x) (- (log (abs x))))))
     )))


(eval-when (:execute :compile-toplevel)
  (defconstant double-float-half-pi (asin 1.0d0))
  (defconstant single-float-half-pi (asin 1.0f0))
)



(defun acos (x)
  "Return the arc cosine of NUMBER."
  (number-case x
    (complex
     (let ((sqrt-1+x (sqrt (+ 1 x)))
	   (sqrt-1-x (sqrt (- 1 x))))
       (complex (* 2 (atan (/ (realpart sqrt-1-x)
			      (realpart sqrt-1+x))))
		(asinh (imagpart (* (conjugate sqrt-1+x)
				    sqrt-1-x))))))
    
    (double-float
     (locally (declare (type double-float x))
       (if (and (<= -1.0d0 x)
		(<= x 1.0d0))
	 (%double-float-acos! x (%make-dfloat))
	 (- double-float-half-pi (asin x)))))
    ((short-float rational)
     #+32-bit-target
     (target::with-stack-short-floats ((sx x))
	(locally
	    (declare (type short-float sx))
	  (if (and (<= -1.0s0 sx)
		   (<= sx 1.0s0))
	    (%single-float-acos! sx (%make-sfloat))
	    (- single-float-half-pi (asin sx)))))
     #+64-bit-target
     (let* ((sx (%short-float x)))
       (declare (type short-float sx))
       (if (and (<= -1.0s0 sx)
                (<= sx 1.0s0))
         (%single-float-acos sx)
         (- single-float-half-pi (asin sx))))
     )))


(defun fsqrt (x)
  (etypecase x
    (double-float (%double-float-sqrt! x (%make-dfloat)))
    (single-float
     #+32-bit-target
     (%single-float-sqrt! x (%make-sfloat))
     #+64-bit-target
     (%single-float-sqrt x))))



(defun %df-atan2 (y x &optional result)
  (if (zerop x)
    (if (zerop y)
      (if (plusp (float-sign x))
        y
        (float-sign y pi))
      (float-sign y double-float-half-pi))
    (%double-float-atan2! y x (or result (%make-dfloat)))))

#+32-bit-target
(defun %sf-atan2! (y x &optional result)
  (if (zerop x)
    (if (zerop y)
      (if (plusp (float-sign x))
        y
        (float-sign y pi))
      (float-sign y single-float-half-pi))
    (%single-float-atan2! y x (or result (%make-sfloat)))))

#+64-bit-target
(defun %sf-atan2 (y x)
  (if (zerop x)
    (if (zerop y)
      (if (plusp (float-sign x))
        y
        (float-sign y pi))
      (float-sign y single-float-half-pi))
    (%single-float-atan2 y x)))

#+64-bit-target
(defun %short-float-exp (n)
  (let* ((bits (single-float-bits n)))
    (declare (type (unsigned-byte 32) bits))
    (ldb (byte IEEE-single-float-exponent-width IEEE-single-float-exponent-offset) bits)))


#+64-bit-target
(defun set-%short-float-exp (float exp)
  (host-single-float-from-unsigned-byte-32
   (dpb exp
        (byte IEEE-single-float-exponent-width
              IEEE-single-float-exponent-offset)
        (the (unsigned-byte 32) (single-float-bits float)))))

#+64-bit-target
(defun %%scale-sfloat (float int)
  (* (the single-float float)
     (the single-float (host-single-float-from-unsigned-byte-32
                        (dpb int
                             (byte IEEE-single-float-exponent-width
                                   IEEE-single-float-exponent-offset)
                             0)))))

#+64-bit-target
(defun %double-float-exp (n)
  (let* ((highword (double-float-bits n)))
    (declare (fixnum highword))
    (logand (1- (ash 1 IEEE-double-float-exponent-width))
            (ash highword (- (- IEEE-double-float-exponent-offset 32))))))

#+64-bit-target
(defun set-%double-float-exp (float exp)
  (let* ((highword (double-float-bits float)))
    (declare (fixnum highword))
    (setf (uvref float target::double-float.val-high-cell)
          (dpb exp
               (byte IEEE-double-float-exponent-width
                     (- IEEE-double-float-exponent-offset 32))
               highword))
    exp))

#+64-bit-target
(defun %integer-decode-double-float (f)
  (multiple-value-bind (hiword loword) (double-float-bits f)
    (declare (type (unsigned-byte 32) hiword loword))
    (let* ((exp (ldb (byte IEEE-double-float-exponent-width
                           (- IEEE-double-float-exponent-offset 32))
                     hiword))
           (mantissa (logior
                      (the fixnum
                        (dpb (ldb (byte (- IEEE-double-float-mantissa-width 32)
                                        IEEE-double-float-mantissa-offset)
                                  hiword)
                             (byte (- IEEE-double-float-mantissa-width 32)
                                   32)
                             loword))
                      (if (zerop exp)
                        0
                        (ash 1 IEEE-double-float-hidden-bit))))
           (sign (if (logbitp 31 hiword) -1 1)))
      (declare (fixnum exp mantissa sign))
      (values (ldb (byte 25 28) mantissa)
              (ldb (byte 28 0) mantissa)
              exp
              sign))))

;;; end of l0-float.lisp
