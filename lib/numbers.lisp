;;;-*-Mode: LISP; Package: CCL -*-
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

;; Lib;numbers.lisp - Lisp arithmetic code.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
 (require :number-macros)
 (require :number-case-macro)
 #+(and cross-compiling 64-bit-target)
 (declaim (ftype function %single-float-atanh %single-float-acosh
                 %single-float-asinh %single-float-tanh
                 %single-float-cosh %single-float-sinh)))



(defconstant double-float-positive-infinity
  #.(let* ((division-by-zero (get-fpu-mode  :division-by-zero)))
      (declare (notinline /))
      (unwind-protect
           (progn
             (ccl:set-fpu-mode :division-by-zero nil)
             (/ 0d0))
	(ccl:set-fpu-mode :division-by-zero division-by-zero))))

(defconstant double-float-negative-infinity
  #.(let* ((division-by-zero (get-fpu-mode  :division-by-zero)))
      (declare (notinline /))
      (unwind-protect
           (progn
             (ccl:set-fpu-mode :division-by-zero nil)
             (/ -0d0))
	(ccl:set-fpu-mode :division-by-zero division-by-zero))))

(defconstant double-float-nan
  #.(make-float-from-fixnums #x8000 0 #x3ff 1))

(defun parse-float (str len off)  
  ; we cant assume this really is a float but dont call with eg s1 or e1
  (let ((integer 0)(expt 0)(sign 0)(done 0)(digits 0) point-pos type) 
    (setq integer
          (do ((n off (1+ n))
               (first t nil)
               (maxn  (+ off len)))
              ((>= n maxn) integer)
            (declare (fixnum n maxn))
            (let ((c (%schar str n)))
              (cond ((eq c #\.)
                     (setq point-pos digits))
                    ((and first (eq c #\+)))
                    ((and first (eq c #\-))
                     (setq sign -1))
                    ((memq c '(#\s #\f #\S #\F))
                     (setq type 'short-float)
                     (return integer))
                    ((memq c '(#\d #\l  #\D  #\L))
                     (setq type 'double-float)
                     (return integer))
                    ((memq c '(#\e #\E))
                     (return integer))
                    ((setq c (digit-char-p c))
                     (setq digits (1+ digits))
                     (setq integer (+ c (* 10 integer))))                  
                    (t (return-from parse-float nil)))
              (setq done (1+ done)))))
    (when point-pos
      (setq expt  (%i- point-pos digits)))
    (when (null type)
      (setq type *read-default-float-format*))
    (when (> len done)
      (let ((eexp nil) (inf nil) (nan nil) (esign 1) c (xsign-n -1))
        (do ((n (%i+ off done 1) (1+ n))
             (first t nil))
            ((>= n (+ off len)))
          (declare (fixnum n))
          (setq c (%schar str n))
          (cond ((and first (or (eq c #\+)(eq c #\-)))
                 (when (eq c #\-)(setq esign -1))
		 (setq xsign-n (1+ n)))
		((and (= n xsign-n)
		      (or (eq c #\+)(eq c #\-)))
                 (if (eq c #\-)
		     (setq nan t)
		     (setq inf t)))
                ((setq c (digit-char-p c))
                 (setq eexp (+ c (* (or eexp 0) 10))))
                (t (return-from parse-float nil))))
        (when (not eexp)(return-from parse-float nil))
        (cond 
	 (inf 
	  (return-from parse-float
	    (coerce (if (minusp sign)
			double-float-negative-infinity
			double-float-positive-infinity)
		    type)))
	 (nan 
	  (return-from parse-float
	    (coerce double-float-nan type)))
	 (expt (setq expt (%i+ expt (* esign eexp))))
	 (t (return-from parse-float nil)))))
    (fide sign integer expt (subtypep type 'short-float))))


;; an interesting test case: 1.448997445238699
;; The correct result is 6525704354437805 x 2^-52
;; Incorrect is          6525704354437806 x 2^-52
;; (from Will Clinger, "How to Read Floating Point Numbers Accurately",
;;  ACM SIGPLAN'90 Conference on Programming Language Design and Implementation")
;; Doug Curries numbers 214748.3646, 1073741823/5000


;; Sane read losers
;; 15871904747836473438871.0e-8
;; 3123927307537977993905.0-13
;; 17209940865514936528.0e-6
;; "13.60447536e132" => adds some gratuitous drech
;; "94824331561426550.889e182"
;; "1166694.64175277e-150" => 1.1666946417527701E-144
;; "3109973217844.55680988601e-173"
;; "817332.e-184" => 8.173320000000001E-179
;; "2695.13e-180" => 2.6951300000000002E-177
;; "89.85345789e-183" => 8.985345789000001E-182
;; "0864813880.29e140" => 8.648138802899999E+148
;; "5221.e-193" => 5.2209999999999995E-190
;; "7.15628e-175" => 7.156280000000001E-175

(defparameter float-powers-of-5  nil)
(defparameter integer-powers-of-5 nil)

(defun 5-to-e (e)
  (declare (fixnum e)(optimize (speed 3)(safety 0)))
  (if (> e 335)
    (* (5-to-e 335)(5-to-e (- e 335))) ; for the dude who types 200 digits and e-500
    (if (< e 12)
      (svref integer-powers-of-5 e)
      (multiple-value-bind (q r) (truncate e 12) ; was floor
        (declare (fixnum q r))        
        (if (eql r 0)
          (svref integer-powers-of-5 (%i+ q 11))
          (* (svref integer-powers-of-5 r)
             (svref integer-powers-of-5 (%i+ q 11))))))))

(defun float-5-to-e (e)
  (if (> e 22)  ; shouldnt happen
    (expt 5.0d0 e)
    (svref float-powers-of-5 e)))

(defparameter a-short-float nil)

(eval-when (:compile-toplevel :execute)
  ; number of bits for mantissa before rounding
  (defconstant *short-float-extended-precision* 28)
  (defconstant *double-float-extended-precision* 60)
  ; number of mantissa bits including hidden bit
  (defconstant *double-float-precision* (1+ IEEE-double-float-mantissa-width))
  (defconstant *short-float-precision* (1+ IEEE-single-float-mantissa-width))
  (defconstant *double-float-bias* IEEE-double-float-bias)
  (defconstant *double-float-max-exponent* (1+ IEEE-double-float-normal-exponent-max))
  (defconstant *double-float-max-exact-power-of-5* 23)
  ;(defconstant *short-float-max-exact-integer-length* 24)
  (defconstant *double-float-max-exact-integer-length* 53)
)




(eval-when (:compile-toplevel :execute)
  (defconstant *short-float-max-exact-power-of-5* 10)
  (defconstant *short-float-bias* IEEE-single-float-bias)
  (defconstant *short-float-max-exact-integer-length* 24)
  (defconstant *short-float-max-exponent* (1+ IEEE-single-float-normal-exponent-max))
)

  
;; this stuff  could be in a shared file

(defun fide #|float-integer-with-decimal-exponent|# (sign integer power-of-10 &optional short)
  ;; take care of the zero case
  (when (zerop integer)
    (return-from fide ;float-integer-with-decimal-exponent
       (if short
         (if (minusp sign) -0.0s0 0.0s0)
         (if (minusp sign) -0.0d0 0.0d0))))
  (let ((abs-power (abs power-of-10))
        (integer-length (integer-length integer)))
    ;; this doesn't work for the above example, so arithmetic must be done wrong
    ;; This does work if set FPCR precision to double
    ;; now see if the conversion can be done simply:
    ;; if both the integer and the power of 10 can be floated exactly, then
    ;; correct rounding can be done by the multiply or divide
    (when (or;short
           (and (<= integer-length  
                    ;; was (if short 17 53) why 17? see above
                    (if short *short-float-max-exact-integer-length* *double-float-max-exact-integer-length*)) 
                ;; (integer-length (expt 5 23)) => 54
                ;; was (if short 5 23)
                (< abs-power  (if short 
                                *short-float-max-exact-power-of-5*
                                *double-float-max-exact-power-of-5*)))) ; we mean < 23 not <=
      ;; if you care about consing, this could be done in assembly language or whatever,
      ;; since all integers fit in 53 bits
      (return-from fide ;float-integer-with-decimal-exponent
        (let* ((signed-integer (prog1 (if (minusp sign) (- integer) integer)))
               (float (float signed-integer (if short 0.0s0 0.0d0)))
               (10-to-power (scale-float (float-5-to-e abs-power) abs-power)))
          ;; coerce to short-float does not whine about undeflow, but does re overflow
          (when short (setq 10-to-power (coerce 10-to-power 'short-float)))
          (if (zerop abs-power)
            float
            (if (minusp power-of-10)
              (/ float  10-to-power)
              (* float  10-to-power))))))
    (try-harder sign integer power-of-10 short)))


(defun try-harder (sign integer power-of-10 short)
  (flet ((ovf (&optional under)
           (if under
             (if (get-fpu-mode :underflow)
               (error 'floating-point-underflow
                      :operation 'scale
                      :operands (list sign integer power-of-10)))
             (if (get-fpu-mode :overflow)
               (error 'floating-point-overflow
                      :operation 'scale
                      :operands (list sign integer power-of-10))))
           (return-from try-harder
             (if under
               (if short
                 (if (minusp sign) -0.0s0 0.0s0)                 
                 (if (minusp sign) 0.0d0 0.0d0))
               (if short
                 (if (minusp sign) most-negative-short-float most-positive-short-float)              
                 (if (minusp sign) most-negative-double-float most-positive-double-float))))))
  (let* ((integer-length (integer-length integer)) new-int power-of-2)
    (if (minusp power-of-10)
      (progn 
        ;; avoid creating enormous integers with 5-to-e only to error later
        (when (< power-of-10 -335)
          (let ((poo (+ (round integer-length 3.2) power-of-10)))
            ;; overestimate digits in integer
            (when (< poo -335) (ovf t))
            ;; this case occurs if 600+ digits 
            (when (> poo 335) (ovf))))
        (let* ((divisor (5-to-e (- power-of-10)))
               ;; make sure we will have enough bits in the quotient
               ;; (and a couple extra for rounding)
               (shift-factor (+ (- (integer-length divisor) integer-length)
                                (if short *short-float-extended-precision* *double-float-extended-precision*)))
               (scaled-integer integer))
          (if (plusp shift-factor)
            (setq scaled-integer (ash integer shift-factor))
            (setq divisor (ash divisor (- shift-factor))))
          (multiple-value-bind (quotient remainder)(floor scaled-integer divisor)
            (unless (zerop remainder) ; whats this - tells us there's junk below
              (setq quotient (logior quotient 1)))
            (setq new-int quotient)
            (setq power-of-2  (- power-of-10 shift-factor)))))
      (progn
        (when (> power-of-10 335)(ovf))
        (setq new-int (* integer (5-to-e power-of-10)))
        (setq power-of-2 power-of-10)))
    (float-and-scale-and-round sign new-int power-of-2 short))))


(defun float-and-scale-and-round (sign integer power-of-2 short &optional result)
  (let* ((length (integer-length integer))
         (lowbits 0)
         (prec (if short *short-float-precision* *double-float-precision*))
         (ep (if short *short-float-extended-precision* *double-float-extended-precision*)))
    (when (<= length prec)
      ;; float can be done exactly, so do it the easy way
      (return-from float-and-scale-and-round
        (scale-float (float (if (minusp sign) (- integer) integer) (if short a-short-float))
                     power-of-2)))    
    (let* ((exponent (+ length power-of-2))
           (biased-exponent (+ exponent (if short *short-float-bias* *double-float-bias*)))
           (sticky-residue nil))
      (cond
       ((<= biased-exponent 0)
        ;; denormalize the number
        (setf sticky-residue (not (zerop (ldb integer (byte (- 1 biased-exponent) 0)))))
        (setf integer (ash integer (- biased-exponent 1)))
        (setf biased-exponent 0)))
      (let ((lowest (min ep length)))
        (when (and (> length ep)(not (zerop (ldb (byte (- length ep) 0) integer))))
          (setq integer (logior integer (ash 1 (- length ep)))))
        ; somewhere between 1 and (- ep prec) bits
        (setq lowbits (ash (ldb (byte (- lowest prec) (- length lowest)) integer) (- ep lowest))))
      (let* ((significand (ldb (byte (1- prec) (- length prec)) integer)))
        (when (and (not (zerop (ldb (byte 1 (- length (1+ prec))) integer)))   ; round bit
                   (or sticky-residue (oddp significand)
                       (not (zerop (ldb (byte (- ep prec 1) 0) lowbits)))))
          ;; round up
          (setf significand (ldb (byte (1- prec) 0) (+ significand 1)))
          (when (zerop significand)
            (incf biased-exponent)))
        (cond ((and (zerop biased-exponent)
                    (zerop significand)
                    (get-fpu-mode :underflow))
               (error 'floating-point-underflow
                      :operation 'scale
                      :operands (list sign integer power-of-2)))
              ((>= biased-exponent (if short *short-float-max-exponent* *double-float-max-exponent*))
               (cond 
                     (t
                      (if (get-fpu-mode :overflow)
                        (error 'floating-point-overflow
                               :operation 'scale
                               :operands (list sign integer power-of-2)))
                      (setf significand 0)                      
                      (setq biased-exponent (if short *short-float-max-exponent* *double-float-max-exponent*))))))
        (values
         (if short 
           (make-short-float-from-fixnums (ldb (byte 23 0) significand)
                                          biased-exponent
                                          sign #-64-bit-target result)
           (make-float-from-fixnums (ldb (byte 24 28) significand)
                                    (ldb (byte 28 0) significand)
                                    biased-exponent
                                    sign result))
         lowbits)))))




(defparameter a-short-float 1.0s0)

(defmethod print-object ((rs random-state) stream)
  (let* ((s1 (random.mrg31k3p-state rs)))
    (format stream "#.(~s~{ ~s~})"       ;>> #.GAG!!!
            'ccl::initialize-mrg31k3p-state
	    (coerce s1 'list))))

(defun float-radix (float)
  "Return (as an integer) the radix b of its floating-point argument."
  (require-type float 'float)
  2)

(defun float-digits (float)
  (if (typep (require-type float 'float) 'short-float)
    IEEE-single-float-digits
    IEEE-double-float-digits))

(defun number-arg (arg)
  (if (numberp arg) arg (%badarg arg 'number)))





;==> Needs a transform...
(defun logandc2 (integer1 integer2)
  "Bitwise AND INTEGER1 with (LOGNOT INTEGER2)."
  (logandc1 integer2 integer1))

(defun logorc2 (integer1 integer2)
  "Bitwise OR INTEGER1 with (LOGNOT INTEGER2)."
  (logorc1 integer2 integer1))



; Figure that the common (2-arg) case is caught by a compiler transform anyway.
(defun gcd (&lexpr numbers)
  "Return the greatest common divisor of the arguments, which must be
  integers. Gcd with no arguments is defined to be 0."
  (let* ((count (%lexpr-count numbers)))
    (declare (fixnum count))   
    (if (zerop count)
      0
      (let* ((n0 (%lexpr-ref numbers count 0)))
        (if (= count 1)
          (%integer-abs n0)
          (do* ((i 1 (1+ i)))
               ((= i count) n0)
            (declare (fixnum i))
            (setq n0 (gcd-2 n0 (%lexpr-ref numbers count i)))))))))

(defun lcm-2 (n0 n1)
  (or (typep n0 'integer) (report-bad-arg n0 'integer))
  (or (typep n1 'integer) (report-bad-arg n1 'integer))
  (locally (declare (integer n0 n1))
    (if (zerop n0)
      0
      (if (zerop n1)
	0
	(let* ((small (if (< n0 n1) n0 n1))
	       (large (if (eq small n0) n1 n0)))
	  (values (truncate (abs (* n0 n1)) (gcd large small))))))))

(defun lcm (&lexpr numbers)
  "Return the least common multiple of one or more integers. LCM of no
  arguments is defined to be 1."
  (let* ((count (%lexpr-count numbers)))
    (declare (fixnum count))    
    (if (zerop count)
      1
      (let* ((n0 (%lexpr-ref numbers count 0)))
        (if (= count 1)
          (%integer-abs n0)
	  (if (= count 2)
	    (lcm-2 n0 (%lexpr-ref numbers count 1))
	    (do* ((i 1 (1+ i)))
		 ((= i count) n0)
	      (declare (fixnum i))
	      (setq n0 (lcm-2 n0 (%lexpr-ref numbers count i))))))))))


#|
(defun rationalize (x)
  (etypecase x
    (rational x)
    (real
     (cond ((minusp x) (- (rationalize (- x))))
	   ((zerop x) 0)
	   (t
	    (let ((eps (etypecase x
			 (single-float single-float-epsilon)
			 (double-float double-float-epsilon)))
		  (y ())
		  (a ()))
	      (do ((xx x (setq y (/ (float 1.0 x) (- xx (float a x)))))
		   (num (setq a (truncate x))
			(+ (* (setq a (truncate y)) num) onum))
		   (den 1 (+ (* a den) oden))
		   (onum 1 num)
		   (oden 0 den))
		  ((and (not (zerop den))
			(not (> (abs (/ (- x (/ (float num x)
						(float den x)))
					x))
				eps)))
		   (integer-/-integer num den)))))))))
|#

(defun rationalize (number)
  "Converts any REAL to a RATIONAL.  Floats are converted to a simple rational
  representation exploiting the assumption that floats are only accurate to
  their precision.  RATIONALIZE (and also RATIONAL) preserve the invariant:
      (= x (float (rationalize x) x))"
  (if (floatp number)
    (labels ((simpler-rational (less-predicate lonum loden hinum hiden
                                               &aux (trunc (if (eql less-predicate #'<=)
                                                             #'ceiling
                                                             #'(lambda (n d) (1+ (floor n d)))))
                                               (term (funcall trunc lonum loden)))
               ;(pprint (list lonum loden hinum hiden))
               (if (funcall less-predicate (* term hiden) hinum)
                 (values term 1)
                 (multiple-value-bind 
                   (num den)
                   (simpler-rational less-predicate hiden (- hinum (* (1- term) hiden))
                                     loden (- lonum (* (1- term) loden)))
                   (values (+ den (* (1- term) num)) num)))))                           
      (multiple-value-bind (fraction exponent sign) (integer-decode-float number)
        ;; the first 2 tests may be unnecessary - I think the check
        ;; for denormalized is compensating for a bug in 3.0 re
        ;; floating a rational (in order to pass tests in
        ;; ppc-test-arith).
        (if (or (and (typep number 'double-float)  ; is it denormalized
                     (eq exponent #.(nth-value 1 (integer-decode-float least-positive-double-float)))) ; aka -1074))
                (eq exponent #.(nth-value 1 (integer-decode-float least-positive-short-float))) ; aka -149))
                (zerop (logand fraction (1- fraction)))) ; or a power of two
          (rational number)
          (if (minusp exponent)
	    ;;less than 1
            (let ((num (ash fraction 2))
	          (den (ash 1 (- 2 exponent))))
	      (multiple-value-bind 
                (n d)
                (simpler-rational (if (evenp fraction) #'<= #'<)
                                  (- num 2) ;(if (zerop (logand fraction (1- fraction))) 1 2))
                                  den  (+ num 2) den)
	        (when (minusp sign)
	          (setq n (- n)))
	        (/ n d)))
            ;;greater than 1
            (ash (if (minusp number) (- fraction) fraction) exponent)))))
    (rational number)))
#|
(defun testrat (&optional (n 1000))
  (dotimes (i n)
    (let* (( numerator (random (ash 1 63)))
          (denominator (random (ash 1 63)))
          (sign  (if (zerop (random 2)) 1 -1))
          (trial (float (/ (* sign numerator) denominator)))
          (rat (rationalize trial)))
      (when (not (= (float rat) trial))
        (error "Rationalize failed. Input ~s Rational ~s Float ~s" trial rat (float rat))))))

; smallest fails in 3.0 - powers of 2 - works here but we cheat a bit
(defun testrat2 ()
  (let ((f least-positive-double-float))
    (dotimes (i 100)
      (when (not (= (float (rationalize f)) f))
        (cerror "a" "rat failed ~s ~s" f i))
      (setq f (* f 2)))))

; fails a lot in 3.0 - not powers of 2 - works here
(defun testrat3 ()
  (let ((f least-positive-double-float))
    (dotimes (i 1000)
      (let ((f2 (* (+ i i 1) f)))
        (when (not (= (float (rationalize f2)) f2))
          (cerror "a" "rat failed ~s ~s" f2 i)))))
  (let ((f least-negative-double-float))
    (dotimes (i 1000)
      (let ((f2 (* (+ i i 1) f)))
        (when (not (= (float (rationalize f2)) f2))
          (cerror "a" "rat failed ~s ~s" f2 i))))))

(defun testrat31 ()
  (let ((f least-positive-short-float))
    (dotimes (i 1000)
      (let ((f2 (* (+ i i 1) f)))
        (when (not (= (float (rationalize f2)) f2))
          (cerror "a" "rat failed ~s ~s" f2 i)))))
  (let ((f least-negative-short-float))
    (dotimes (i 1000)
      (let ((f2 (* (+ i i 1) f)))
        (when (not (= (float (rationalize f2)) f2))
          (cerror "a" "rat failed ~s ~s" f2 i))))))

; works in 3.0 - and here
(defun testrat4 ()
  (let ((f least-positive-normalized-double-float))
    (dotimes (i 1000)
      (let ((f2 (* (+ i i 1) f)))
        (when (not (= (float (rationalize f2)) f2))
          (cerror "a" "rat failed ~s ~s" f2 i)))))
  (let ((f least-negative-normalized-double-float))
    (dotimes (i 100)
      (let ((f2 (* (+ i i 1) f)))
        (when (not (= (float (rationalize f2)) f2))
          (cerror "a" "rat failed ~s ~s" f2 i))))))
        
    
|#

#| now in l1-numbers.lisp
(defun logeqv (&lexpr numbers)
  (let* ((count (%lexpr-count numbers)))
    (declare (fixnum count))
    (if (zerop count)
      -1
      (let* ((n0 (%lisp-word-ref numbers count)))
        (if (= count 1)
          (require-type n0 'integer)
          (do* ((i 1 (1+ i)))
               ((= i count) n0)
            (declare (fixnum i))
            (declare (optimize (speed 3) (safety 0)))
            (setq n0 (logeqv-2 (%lexpr-ref numbers count i) n0))))))))
|#


(defparameter *boole-ops* 
  (vector
   #'(lambda (i1 i2) (declare (ignore i1 i2)) 0)
   #'(lambda (i1 i2) (declare (ignore i1 i2)) -1)
   #'(lambda (i1 i2) (declare (ignore i2)) i1)
   #'(lambda (i1 i2) (declare (ignore i1)) i2)
   #'(lambda (i1 i2) (declare (ignore i2)) (lognot i1))
   #'(lambda (i1 i2) (declare (ignore i1)) (lognot i2))
   #'(lambda (i1 i2) (logand i1 i2))
   #'(lambda (i1 i2) (logior i1 i2))
   #'(lambda (i1 i2) (logxor i1 i2))
   #'(lambda (i1 i2) (logeqv i1 i2))
   #'(lambda (i1 i2) (lognand i1 i2))
   #'(lambda (i1 i2) (lognor i1 i2))
   #'(lambda (i1 i2) (logandc1 i1 i2))
   #'(lambda (i1 i2) (logandc2 i1 i2))
   #'(lambda (i1 i2) (logorc1 i1 i2))
   #'(lambda (i1 i2) (logorc2 i1 i2))))
 


;===> Change these constants to match maclisp!!
(defun boole (op integer1 integer2)
  "Bit-wise boolean function on two integers. Function chosen by OP:
        0       BOOLE-CLR
        1       BOOLE-SET
        2       BOOLE-1
        3       BOOLE-2
        4       BOOLE-C1
        5       BOOLE-C2
        6       BOOLE-AND
        7       BOOLE-IOR
        8       BOOLE-XOR
        9       BOOLE-EQV
        10      BOOLE-NAND
        11      BOOLE-NOR
        12      BOOLE-ANDC1
        13      BOOLE-ANDC2
        14      BOOLE-ORC1
        15      BOOLE-ORC2"
  (unless (and (typep op 'fixnum)
               (locally (declare (fixnum op))
                 (and (>= op 0)
                      (<= op 15))))
    (report-bad-arg op '(integer 0 15)))
  (funcall (%svref *boole-ops* op)
	   (require-type integer1 'integer)
	   (require-type integer2 'integer)))


(defun %integer-power (b e)
  (declare (type unsigned-byte e))
  (if (zerop e)
    (+ 1 (* b 0))
    (if (eql b 2)
      (ash 1 e)
      (do* ((next (ash e -1) (ash e -1))
            (oddexp (oddp e) (oddp e))
            (total (if oddexp b 1) (if oddexp (* b total) total)))
           ((zerop next) total)
        (declare (type unsigned-byte next))
        (setq b (* b b) e next)))))

(defun signum (x)
  "If NUMBER is zero, return NUMBER, else return (/ NUMBER (ABS NUMBER))."
  (cond ((complexp x) (if (zerop x) x (/ x (abs x))))
        ((rationalp x) (if (plusp x) 1 (if (zerop x) 0 -1)))
        ((zerop x) (float 0.0 x))
        (t (float-sign x))))



; Thanks to d34676@tansei.cc.u-tokyo.ac.jp (Akira KURIHARA)
(defun isqrt (n &aux n-len-quarter n-half n-half-isqrt
                init-value iterated-value)
  "Return the root of the nearest integer less than n which is a perfect
   square.  Argument n must be a non-negative integer"
  (cond
   ((eql n 0) 0)
   ; this fails sometimes - do we care? 70851992595801818865024053174 or #x80000000
   ; maybe we do - its used by dotimes
   ;((not (int>0-p n)) (report-bad-arg n '(integer 0))) ;'unsigned-byte)) ; Huh?
   ((or (not (integerp n))(minusp n))(report-bad-arg n '(integer 0)))
   ((> n 24)		; theoretically (> n 7) ,i.e., n-len-quarter > 0
    (setq n-len-quarter (ash (integer-length n) -2))
    (setq n-half (ash n (- (ash n-len-quarter 1))))
    (setq n-half-isqrt (isqrt n-half))
    (setq init-value (ash (1+ n-half-isqrt) n-len-quarter))
    (loop
      (setq iterated-value (ash (+ init-value (floor n init-value)) -1))
      (if (not (< iterated-value init-value))
        (return init-value)
        (setq init-value iterated-value))))
   ((> n 15) 4)
   ((> n  8) 3)
   ((> n  3) 2)
   (t 1)))


(defun sinh (x)
  "Return the hyperbolic sine of NUMBER."
  (if (complexp x) 
    (/ (- (exp x) (exp (- x))) 2)
    (if (typep x 'double-float)
      (%double-float-sinh! x (%make-dfloat))
      #+32-bit-target
      (target::with-stack-short-floats ((sx x))
	(%single-float-sinh! sx (%make-sfloat)))
      #+64-bit-target
        (%single-float-sinh (%short-float x)))))


(defun cosh (x)
  "Return the hyperbolic cosine of NUMBER."
  (if (complexp x) 
    (/ (+ (exp x) (exp (- x))) 2)
    (if (typep x 'double-float)
      (%double-float-cosh! x (%make-dfloat))
      #+32-bit-target
      (target::with-stack-short-floats ((sx x))
	(%single-float-cosh! sx (%make-sfloat)))
      #+64-bit-target
      (%single-float-cosh (%short-float x)))))

(defun tanh (x)
  "Return the hyperbolic tangent of NUMBER."
  (if (complexp x) 
    (/ (sinh x) (cosh x))
    (if (typep x 'double-float)
      (%double-float-tanh! x (%make-dfloat))
      #+32-bit-target
      (target::with-stack-short-floats ((sx x))
	(%single-float-tanh! sx (%make-sfloat)))
      #+64-bit-target
      (%single-float-tanh (%short-float x)))))

(defun asinh (x)
  "Return the hyperbolic arc sine of NUMBER."
  (if (complexp x) 
    (log (+ x (sqrt (+ 1 (* x x)))))
    (if (typep x 'double-float)
      (%double-float-asinh! x (%make-dfloat))
      #+32-bit-target
      (target::with-stack-short-floats ((sx x))
	(%single-float-asinh! sx (%make-sfloat)))
      #+64-bit-target
      (%single-float-asinh (%short-float x)))))

(defun acosh (x)
  "Return the hyperbolic arc cosine of NUMBER."
  (if (and (realp x) (<= 1.0 x))
    (if (typep x 'double-float)
      (%double-float-acosh! x (%make-dfloat))
      #+32-bit-target
      (target::with-stack-short-floats ((sx x))
	(%single-float-acosh! sx (%make-sfloat)))
      #+64-bit-target
      (%single-float-acosh (%short-float x)))
    (* 2 (log (+ (sqrt (/ (1+ x) 2)) (sqrt (/ (1- x) 2)))))))

(defun atanh (x)
  "Return the hyperbolic arc tangent of NUMBER."
  (if (and (realp x) (<= -1.0 (setq x (float x)) 1.0))
    (if (typep x 'double-float)
      (%double-float-atanh! x (%make-dfloat))
      #+32-bit-target
      (%single-float-atanh! x (%make-sfloat))
      #+64-bit-target
      (%single-float-atanh x))
    (/ (log (/ (+ 1 x) (- 1 x))) 2)))


(defun ffloor (number &optional divisor)
  "Same as FLOOR, but returns first value as a float."
  (multiple-value-bind (q r) (floor number divisor)
    (values (float q (if (floatp r) r 0.0)) r)))

(defun fceiling (number &optional divisor)
  "Same as CEILING, but returns first value as a float."
  (multiple-value-bind (q r) (ceiling number divisor)
    (values (float q (if (floatp r) r 0.0)) r)))

(defun ftruncate (number &optional divisor)
  "Same as TRUNCATE, but returns first value as a float."
  (multiple-value-bind (q r) (truncate number divisor)
    (values (float q (if (floatp r) r 0.0)) r)))

(defun fround (number &optional divisor)
  "Same as ROUND, but returns first value as a float."
  (multiple-value-bind (q r) (round number divisor)
    (values (float q (if (floatp r) r 0.0)) r)))

(defun rational (number)
  "RATIONAL produces a rational number for any real numeric argument. This is
  more efficient than RATIONALIZE, but it assumes that floating-point is
  completely accurate, giving a result that isn't as pretty."
  (if (floatp number)
    (multiple-value-bind (s e sign)
        (number-case number
          (short-float
           (integer-decode-short-float number))
          (double-float
           (integer-decode-double-float number)))
      (if (eq sign -1) (setq s (- s)))
      (if (%iminusp e)
        (/ s (ash 1 (%i- 0 e)))
        (ash s e)))
    (if (rationalp number)
      number
      (report-bad-arg number 'real))))

; make power tables for floating point reader
(progn
  (setq float-powers-of-5 (make-array 23))
  (let ((array float-powers-of-5))
    (dotimes (i 23)
      (setf (svref array i)  (float (expt 5 i) 0.0d0))))
  (setq integer-powers-of-5 (make-array (+ 12 (floor 324 12))))
  (let ((array integer-powers-of-5))
    (dotimes (i 12)
      (setf (svref array i)  (expt 5 i)))
    (dotimes (i (floor 324 12))
      (setf (svref array (+ i 12)) (expt 5 (* 12 (1+ i)))))))


(provide 'numbers)

