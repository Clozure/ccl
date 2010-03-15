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
;;; level-0;l0-numbers.lisp

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARCH")
  (require "LISPEQU")
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO")



  (defvar *dfloat-dops* '((* . %double-float*-2!)(/ . %double-float/-2!)
			  (+ . %double-float+-2!)(- . %double-float--2!)))
  
  (defvar *sfloat-dops* '((* . %short-float*-2!)(/ . %short-float/-2!)
			  (+ . %short-float+-2!)(- . %short-float--2!)))

  (defmacro dfloat-rat (op x y &optional (destructive-op (cdr (assq op *dfloat-dops*))))
    (if destructive-op
	(let ((f2 (gensym)))
	  `(let ((,f2 (%double-float ,y (%make-dfloat))))
	    (,destructive-op ,x ,f2 ,f2)))          
	`(,op (the double-float ,x) (the double-float (%double-float ,y)))))

  (defmacro rat-dfloat (op x y &optional (destructive-op (cdr (assq op *dfloat-dops*))))
    (if destructive-op
	(let ((f1 (gensym)))
	  `(let ((,f1 (%double-float ,x (%make-dfloat)))) 
	    (,destructive-op ,f1 ,y ,f1)))
	`(,op (the double-float (%double-float ,x)) (the double-float ,y))))

  (defmacro sfloat-rat (op x y &optional (destructive-op (cdr (assq op *sfloat-dops*))))
    (let* ((use-destructive-op
            (target-word-size-case
             (32 destructive-op)
             (64 nil))))
      (if use-destructive-op
	(let ((f2 (gensym)))
	  `(let ((,f2 (%short-float ,y (%make-sfloat)))) 
	    (,destructive-op ,x ,f2 ,f2)))
	`(,op (the short-float ,x) (the short-float (%short-float ,y))))))

  (defmacro rat-sfloat (op x y &optional (destructive-op (cdr (assq op *sfloat-dops*))))
    (let* ((use-destructive-op
            (target-word-size-case
             (32 destructive-op)
             (64 nil))))
      (if use-destructive-op
        (let ((f1 (gensym)))
          `(let ((,f1 (%short-float ,x (%make-sfloat)))) 
            (,destructive-op ,f1 ,y ,f1)))
        `(,op (the short-float (%short-float ,x)) (the short-float ,y)))))


  


  (declaim (inline  %make-complex %make-ratio))
  (declaim (inline canonical-complex))
  (declaim (inline build-ratio))
  (declaim (inline maybe-truncate)))



(defun %make-complex (realpart imagpart)
  (gvector :complex realpart imagpart))

(defun %make-ratio (numerator denominator)
  (gvector :ratio numerator denominator))
 


; this is no longer used
(defun %integer-signum (num)
  (if (fixnump num)
    (%fixnum-signum num)
    ; there is no such thing as bignum zero we hope
    (if (bignum-minusp num) -1 1)))


; Destructive primitives.
(macrolet ((defdestructive-df-op (non-destructive-name destructive-name op)
             `(progn
                (defun ,non-destructive-name (x y)
                  (,destructive-name x y (%make-dfloat)))
                (defun ,destructive-name (x y result)
                  (declare (double-float x y result))
                  (%setf-double-float result (the double-float (,op x y)))))))
  (defdestructive-df-op %double-float+-2 %double-float+-2! +)
  (defdestructive-df-op %double-float--2 %double-float--2! -)
  (defdestructive-df-op %double-float*-2 %double-float*-2! *)
  (defdestructive-df-op %double-float/-2 %double-float/-2! /))

#-64-bit-target
(macrolet ((defdestructive-sf-op (non-destructive-name destructive-name op)
             `(progn
                (defun ,non-destructive-name (x y)
                  (,destructive-name x y (%make-sfloat)))
                (defun ,destructive-name (x y result)
                  (declare (short-float x y result))
                  (%setf-short-float result (the short-float (,op x y)))))))
  (defdestructive-sf-op %short-float+-2 %short-float+-2! +)
  (defdestructive-sf-op %short-float--2 %short-float--2! -)
  (defdestructive-sf-op %short-float*-2 %short-float*-2! *)
  (defdestructive-sf-op %short-float/-2 %short-float/-2! /))


(defun %negate (x)
  (number-case x
    (fixnum  (- (the fixnum x)))
    (double-float  (%double-float-negate! x (%make-dfloat)))
    (short-float
     #+32-bit-target (%short-float-negate! x (%make-sfloat))
     #+64-bit-target (%short-float-negate x))
    (bignum (negate-bignum x))
    (ratio (%make-ratio (%negate (%numerator x)) (%denominator x)))
    (complex (%make-complex (%negate (%realpart X))(%negate (%imagpart X))) )))

(defun %double-float-zerop (n)
  (zerop (the double-float n)))

(defun %short-float-zerop (n)
  (zerop (the single-float n)))

(defun zerop (number)
  "Is this number zero?"
  (number-case number
    (integer (eq number 0))
    (short-float (%short-float-zerop number))
    (double-float (%double-float-zerop number))
    (ratio nil)
    (complex
     (number-case (%realpart number)
       (short-float (and (%short-float-zerop (%realpart number))
                         (%short-float-zerop (%imagpart number))))
       (double-float (and (%double-float-zerop (%realpart number))
                          (%double-float-zerop (%imagpart number))))
       (t (and (eql 0 (%realpart number))(eql 0 (%imagpart number))))))))

(defun %short-float-plusp (x)
  (> (the single-float x) 0.0f0))

(defun %double-float-plusp (x)
  (> (the double-float x) 0.0d0))

(defun plusp (number)
  "Is this real number strictly positive?"
  (number-case number
    (fixnum (%i> number 0))
    (bignum (bignum-plusp number))
    (short-float (%short-float-plusp number))
    (double-float (%double-float-plusp number))
    (ratio (plusp (%numerator number)))))


(defun minusp (number)
  "Is this real number strictly negative?"
  (number-case number
    (fixnum (%i< number 0))
    (bignum (bignum-minusp number))
    (short-float (%short-float-minusp number))
    (double-float (%double-float-minusp number))
    (ratio (minusp (%numerator number)))))


(defun oddp (n)
  "Is this integer odd?"
  (case (typecode n)
    (#.target::tag-fixnum (logbitp 0 (the fixnum n)))
    (#.target::subtag-bignum (%bignum-oddp n))
    (t (report-bad-arg n 'integer))))

(defun evenp (n)
  "Is this integer even?"
  (case (typecode n)
    (#.target::tag-fixnum (not (logbitp 0 (the fixnum n))))
    (#.target::subtag-bignum (not (%bignum-oddp n)))
    (t (report-bad-arg n 'integer))))

;; expansion slightly changed
(defun =-2 (x y)
  (number-case x
    (fixnum (number-case y
              (fixnum (eq x y))
              (double-float (eq 0 (fixnum-dfloat-compare x y)))
              (short-float (eq 0 (fixnum-sfloat-compare x y)))
              ((bignum ratio) nil)
              (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (double-float (number-case y ; x
                    (double-float (= (the double-float x)(the double-float y))) ;x 
                    (short-float (with-stack-double-floats ((dy y))
                                   (= (the double-float x) (the double-float dy))))
                    (fixnum (eq 0 (fixnum-dfloat-compare  y x)))
                    (bignum (eq 0 (bignum-dfloat-compare y x)))
                    (ratio (= (rational x) y))
                    (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (short-float (number-case y
                   (short-float (= (the short-float x) (the short-float y)))
                   (double-float (with-stack-double-floats ((dx x))
                                   (= (the double-float dx) (the double-float y))))
                   (fixnum (eq 0 (fixnum-sfloat-compare y x)))
                   (bignum (eq 0 (bignum-sfloat-compare y x)))
                   (ratio (= (rational x) y))
                   (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (bignum (number-case y 
              (bignum (eq 0 (bignum-compare x y)))
              ((fixnum ratio) nil)
              (double-float (eq 0 (bignum-dfloat-compare x y)))
              (short-float (eq 0 (bignum-sfloat-compare x y)))
              (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (ratio (number-case y
             (integer nil)
             (ratio
              (and (eql (%numerator x) (%numerator y))
                   (eql (%denominator x) (%denominator y))))
             (float (= x (rational y)))
             (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (complex (number-case y
               (complex (and (= (%realpart x) (%realpart y))
                             (= (%imagpart x) (%imagpart y))))
               ((float rational)
                (and (zerop (%imagpart x)) (= (%realpart x) y)))))))

(defun /=-2 (x y)
  (declare (notinline =-2))
  (not (= x y)))


; true iff (< x y) is false.
(defun >=-2 (x y)
  (declare (notinline <-2))
  (not (< x y)))



(defun <=-2 (x y)
  (declare (notinline >-2))
  (not (> x y)))

(defun <-2 (x y)
  (number-case x
    (fixnum (number-case y
              (fixnum (< (the fixnum x) (the fixnum y)))
              (double-float (eq -1 (fixnum-dfloat-compare x y)))
              (short-float (eq -1 (fixnum-sfloat-compare x y)))
              (bignum (bignum-plusp y))
              (ratio (< x (ceiling (%numerator y)(%denominator y))))))
    (double-float (number-case y ; x
                    (double-float (< (the double-float x)(the double-float y))) ;x
                    (short-float (with-stack-double-floats ((dy y))
                                   (< (the double-float x) (the double-float dy))))
                    (fixnum (eq 1 (fixnum-dfloat-compare  y x)))
                    (bignum (eq 1 (bignum-dfloat-compare y x)))
                    (ratio (< (rational x) y))))
    (short-float (number-case y
                    (short-float (< (the short-float x) (the short-float y)))
                    (double-float (with-stack-double-floats ((dx x))
                                    (< (the double-float dx) (the double-float y))))
                    (fixnum (eq 1 (fixnum-sfloat-compare y x)))
                    (bignum (eq 1 (bignum-sfloat-compare y x)))
                    (ratio (< (rational x) y))))
    (bignum (number-case y 
              (bignum (EQ -1 (bignum-compare x y)))
              (fixnum (not (bignum-plusp x)))
              (ratio (< x (ceiling (%numerator y)(%denominator y))))
              (double-float (eq -1 (bignum-dfloat-compare x y)))
              (short-float (eq -1 (bignum-sfloat-compare x y)))))
    (ratio (number-case y
             (integer (< (floor (%numerator x)(%denominator x)) y))
             (ratio
              (< (* (%numerator (the ratio x))
                    (%denominator (the ratio y)))
                 (* (%numerator (the ratio y))
                    (%denominator (the ratio x)))))
             (float (< x (rational y)))))))



(defun >-2 (x y)
  ;(declare (optimize (speed 3)(safety 0)))
  (number-case x
    (fixnum (number-case y
              (fixnum (> (the fixnum x) (the fixnum y)))
              (bignum (not (bignum-plusp y)))
              (double-float (eq 1 (fixnum-dfloat-compare x y)))
              (short-float (eq 1 (fixnum-sfloat-compare x y)))
              ;; or (> (* x denom) num) ?
              (ratio (> x (floor (%numerator y) (%denominator y))))))
    (double-float (number-case y
                    (double-float (> (the double-float x) (the double-float y)))
                    (short-float (with-stack-double-floats ((dy y))
                                   (> (the double-float x) (the double-float dy))))
                    (fixnum (eq -1 (fixnum-dfloat-compare  y x)))
                    (bignum (eq -1 (bignum-dfloat-compare y x)))
                    (ratio (> (rational x) y))))
    (short-float (number-case y
                    (short-float (> (the short-float x) (the short-float y)))
                    (double-float (with-stack-double-floats ((dx x))
                                   (> (the double-float dx) (the double-float y))))
                    (fixnum (eq -1 (fixnum-sfloat-compare  y x)))
                    (bignum (eq -1 (bignum-sfloat-compare y x)))
                    (ratio (> (rational x) y))))
    (bignum (number-case y
              (fixnum (bignum-plusp x))
              (bignum (eq 1 (bignum-compare x y)))
              ;; or (> (* x demon) num)
              (ratio (> x (floor (%numerator y) (%denominator y))))
              (double-float (eq 1 (bignum-dfloat-compare x y)))
              (short-float (eq 1 (bignum-sfloat-compare x y)))))
    (ratio (number-case y
             ;; or (> num (* y denom))
             (integer (> (ceiling (%numerator x) (%denominator x)) y))
             (ratio
              (> (* (%numerator (the ratio x))
                    (%denominator (the ratio y)))
                 (* (%numerator (the ratio y))
                    (%denominator (the ratio x)))))
             (float (> x (rational y)))))))


; t if any bits set after exp (unbiased)
(defun hi-lo-fraction-p (hi lo exp)
  (declare (fixnum hi lo exp))
  (if (> exp 24)
    (not (eql 0 (%ilogand lo (%ilsr (- exp 25) #xfffffff))))
    (or (not (zerop lo))(not (eql 0 (%ilogand hi (%ilsr exp #x1ffffff)))))))



(defun negate-hi-lo (hi lo)
  (setq hi (%ilogxor hi #x3ffffff))
  (if (eq 0 lo)    
    (setq hi (+ hi 1))
    (setq lo (+ (%ilogxor lo #xfffffff) 1)))
  (values hi lo))



(defun fixnum-dfloat-compare (int dfloat)
  (declare (double-float dfloat) (fixnum int))
  (if (and (eq int 0)(= dfloat 0.0d0))
    0
    (with-stack-double-floats ((d1 int))
      (locally (declare (double-float d1))
        (if (eq int (%truncate-double-float->fixnum d1))
          (cond ((< d1 dfloat) -1)
                ((= d1 dfloat) 0)
                (t 1))
          ;; Whatever we do here should have the effect
          ;; of comparing the integer to the result of calling
          ;; RATIONAL on the float.  We could probably
          ;; skip the call to RATIONAL in more cases,
          ;; but at least check the obvious ones here
          ;; (e.g. different signs)
          (multiple-value-bind (mantissa exponent sign)
              (integer-decode-double-float dfloat)
            (declare (type (integer -1 1) sign)
                     (fixnum exponent))
            (cond ((zerop int)
                   (- sign))
                  ((and (< int 0) (eql sign 1)) -1)
                  ((and (> int 0) (eql sign -1)) 1)
                  (t
                   ;; See RATIONAL.  Can probably avoid this if
                   ;; magnitudes are clearly dissimilar.
                   (if (= sign -1) (setq mantissa (- mantissa)))
                   (let* ((rat (if (< exponent 0)
                                 (/ mantissa (ash 1 (the fixnum (- exponent))))
                                 (ash mantissa exponent))))
                     (if (< int rat)
                       -1
                       (if (eq int rat)
                         0
                         1)))))))))))



(defun fixnum-sfloat-compare (int sfloat)
  (declare (short-float sfloat) (fixnum int))
  (if (and (eq int 0)(= sfloat 0.0s0))
    0
    (#+32-bit-target target::with-stack-short-floats #+32-bit-target ((s1 int))
     #-32-bit-target let* #-32-bit-target ((s1 (%int-to-sfloat int)))
                     (locally
                         (declare (short-float s1))
                       (if (eq (%truncate-short-float->fixnum s1) int)
                         (cond ((< s1 sfloat) -1)
                               ((= s1 sfloat) 0)
                               (t 1))
                         ;; Whatever we do here should have the effect
                         ;; of comparing the integer to the result of calling
                         ;; RATIONAL on the float.  We could probably
                         ;; skip the call to RATIONAL in more cases,
                         ;; but at least check the obvious ones here
                         ;; (e.g. different signs)
                         (multiple-value-bind (mantissa exponent sign)
                             (integer-decode-short-float sfloat)
                           (declare (type (integer -1 1) sign)
                                    (fixnum exponent))
                           (cond ((zerop int)
                                  (- sign))
                                 ((and (< int 0) (eql sign 1)) -1)
                                 ((and (> int 0) (eql sign -1)) 1)
                                 (t
                                  ;; See RATIONAL.  Can probably avoid this if
                                  ;; magnitudes are clearly dissimilar.
                                  (if (= sign -1) (setq mantissa (- mantissa)))
                                  (let* ((rat (if (< exponent 0)
                                                (/ mantissa (ash 1 (the fixnum (- exponent))))
                                                (ash mantissa exponent))))
                                    (if (< int rat)
                                      -1
                                      (if (eq int rat)
                                        0
                                        1)))))))))))


        
;;; lotta stuff to avoid making a rational from a float
;;; returns -1 less, 0 equal, 1 greater
(defun bignum-dfloat-compare (int float)
  (cond 
   ((and (eq int 0)(= float 0.0d0)) 0)
   (t
    (let* ((fminus  (%double-float-minusp float))
           (iminus (minusp int))
           (gt (if iminus -1 1)))
      (declare (fixnum gt))
      (if (neq fminus iminus)
        gt  ; if different signs, done
        (let ((intlen (integer-length int)) 
              (exp (- (the fixnum (%double-float-exp float)) 1022)))
          (declare (fixnum intlen exp))
          (cond 
           ((and (not fminus) (< intlen exp)) -1)
           ((> intlen exp)  gt)   ; if different exp, done
           ((and fminus (or (< (1+ intlen) exp)
                            (and (= (1+ intlen) exp)
                                 (neq (one-bignum-factor-of-two int) intlen))))
            ;(print 'zow)
            (the fixnum (- gt)))  ; ; integer-length is strange for neg powers of 2            
           (t (multiple-value-bind (hi lo)(fixnum-decode-float float)
                (declare (fixnum hi lo)) 
                (when fminus (multiple-value-setq (hi lo)(negate-hi-lo hi lo)))
                (let* ((sz 26)  ; exp > 28 always
                       (pos (- exp 25))
                       (big-bits (%ldb-fixnum-from-bignum int sz pos)))
                  (declare (fixnum pos big-bits sz))
                  ;(print (list big-bits hi sz pos))
                  (cond 
                   ((< big-bits hi) -1)
                   ((> big-bits hi) 1)
                   (t (let* ((sz (min (- exp 25) 28))
                             (pos (- exp 25 sz)) ; ?
                             (ilo (if (< exp 53) (ash lo (- exp 53)) lo))                                    
                             (big-bits (%ldb-fixnum-from-bignum int sz pos)))
                        (declare (fixnum pos sz ilo big-bits))
                        ;(PRINT (list big-bits ilo))
                        (cond
                         ((< big-bits ilo) -1)
                         ((> big-bits ilo) 1)
                         ((eq exp 53) 0)
                         ((< exp 53)
                          (if (not (hi-lo-fraction-p hi lo exp)) 0 -1)) ; -1 if pos 
                         (t (if (%i< (one-bignum-factor-of-two int) (- exp 53)) 1 0)))))))
                )))))))))



;;; I don't know if it's worth doing a more "real" version of this.
(defun bignum-sfloat-compare (int float)
  (with-stack-double-floats ((df float))
    (bignum-dfloat-compare int df)))

;;;; Canonicalization utilities:

;;; CANONICAL-COMPLEX  --  Internal
;;;
;;;    If imagpart is 0, return realpart, otherwise make a complex.  This is
;;; used when we know that realpart and imagpart are the same type, but
;;; rational canonicalization might still need to be done.
;;;

(defun canonical-complex (realpart imagpart)
  (if (eql imagpart 0)
    realpart
    (%make-complex realpart imagpart)))




(defun +-2 (x y)     
  (number-case x
    (fixnum (number-case y
              (fixnum (+ (the fixnum x) (the fixnum y)))
              (double-float (rat-dfloat + x y))
              (short-float (rat-sfloat + x y))
              (bignum (add-bignum-and-fixnum y x))
              (complex (complex (+ x (%realpart y))
                                (%imagpart y)))
              (ratio (let* ((dy (%denominator y)) 
                            (n (+ (* x dy) (%numerator y))))
                       (%make-ratio n dy)))))
    (double-float (number-case y
                    (double-float (+ (the double-float x) (the double-float y)))
                    (short-float (with-stack-double-floats ((dy y))
                                   (+ (the double-float x) (the double-float dy))))
                    (rational (dfloat-rat + x y))
                    (complex (complex (+ x (%realpart y)) 
                                      (%imagpart y)))))
    (short-float (number-case y                                
                   (short-float (+ (the short-float x) (the short-float y)))
                   (double-float (with-stack-double-floats ((dx x))
                                   (+ (the double-float dx) (the double-float y))))
                   (rational (sfloat-rat + x y))
                   (complex (complex (+ x (%realpart y))
                                     (%imagpart y)))))
    (bignum (number-case y
              (bignum (add-bignums x y))
              (fixnum (add-bignum-and-fixnum x y))
              (double-float (rat-dfloat + x y))
              (short-float (rat-sfloat + x y))
              (complex (complex (+ x (realpart y)) 
                                (%imagpart y)))
              (ratio
               (let* ((dy (%denominator y))
                      (n (+ (* x dy) (%numerator y))))
                 (%make-ratio n dy)))))
    (complex (number-case y
               (complex (canonical-complex (+ (%realpart x) (%realpart y))
                                           (+ (%imagpart x) (%imagpart y))))
               ((rational float) (complex (+ (%realpart x) y) (%imagpart x)))))
    (ratio (number-case y
             (ratio
              (let* ((nx (%numerator x))
                     (dx (%denominator x))
                     (ny (%numerator y))
                     (dy (%denominator y))
                     (g1 (gcd dx dy)))
                (if (eql g1 1)
                  (%make-ratio (+ (* nx dy) (* dx ny)) (* dx dy))
                  (let* ((t1 (+ (* nx (truncate dy g1)) (* (truncate dx g1) ny)))
                         (g2 (gcd t1 g1))
                         (t2 (truncate dx g1)))
                    (cond ((eql t1 0) 0)
                          ((eql g2 1) (%make-ratio t1 (* t2 dy)))
                          (t
                           (let* ((nn (truncate t1 g2))
                                  (t3 (truncate dy g2))
                                  (nd (if (eql t2 1) t3 (* t2 t3))))
                             (if (eql nd 1) nn (%make-ratio nn nd)))))))))
             (integer
              (let* ((dx (%denominator x)) (n (+ (%numerator x) (* y dx))))
                (%make-ratio n dx)))
             (double-float (rat-dfloat + x y))
             (short-float (rat-sfloat + x y))
             (complex (complex (+ x (%realpart y)) 
                               (%imagpart y)))))))

(defun --2 (x y)     
  (number-case x
    (fixnum (number-case y
              (fixnum (- (the fixnum x) (the fixnum y)))
              (double-float (rat-dfloat - x y))
              (short-float (rat-sfloat - x y))
              (bignum 
               (with-small-bignum-buffers ((bx x))
                        (subtract-bignum bx y)))
              (complex (complex (- x (%realpart y))
                                (- (%imagpart y))))
              (ratio (let* ((dy (%denominator y)) 
                            (n (- (* x dy) (%numerator y))))
                       (%make-ratio n dy)))))
    (double-float (number-case y
                    (double-float (- (the double-float x) (the double-float y)))
                    (short-float (with-stack-double-floats ((dy y))
                                   (- (the double-float x) (the double-float dy))))
                    (rational (dfloat-rat - x y))
                    (complex (complex (- x (%realpart y)) 
                                      (- (%imagpart y))))))
    (short-float (number-case y                                
                   (short-float (- (the short-float x) (the short-float y)))
                   (double-float (with-stack-double-floats ((dx x))
                                   (- (the double-float dx) (the double-float y))))
                   (rational (sfloat-rat - x y))
                   (complex (complex (- x (%realpart y))
                                     (- (%imagpart y))))))
    (bignum (number-case y
              (bignum (subtract-bignum x y))
              (fixnum (if (eql y target::target-most-negative-fixnum)
                        (with-small-bignum-buffers ((by y))
                          (subtract-bignum x by))
                        (add-bignum-and-fixnum x (- y))))
              (double-float (rat-dfloat - x y))
              (short-float (rat-sfloat - x y))
              (complex (complex (- x (realpart y)) 
                                (- (%imagpart y))))
              (ratio
               (let* ((dy (%denominator y))
                      (n (- (* x dy) (%numerator y))))
                 (%make-ratio n dy)))))
    (complex (number-case y
               (complex (canonical-complex (- (%realpart x) (%realpart y))
                                           (- (%imagpart x) (%imagpart y))))
               ((rational float) (complex (- (%realpart x) y) (%imagpart x)))))
    (ratio (number-case y
             (ratio
              (let* ((nx (%numerator x))
                     (dx (%denominator x))
                     (ny (%numerator y))
                     (dy (%denominator y))
                     (g1 (gcd dx dy)))
                (if (eql g1 1)
                  (%make-ratio (- (* nx dy) (* dx ny)) (* dx dy))
                  (let* ((t1 (- (* nx (truncate dy g1)) (* (truncate dx g1) ny)))
                         (g2 (gcd t1 g1))
                         (t2 (truncate dx g1)))
                    (cond ((eql t1 0) 0)
                          ((eql g2 1) (%make-ratio t1 (* t2 dy)))
                          (t
                           (let* ((nn (truncate t1 g2))
                                  (t3 (truncate dy g2))
                                  (nd (if (eql t2 1) t3 (* t2 t3))))
                             (if (eql nd 1) nn (%make-ratio nn nd)))))))))
             (integer
              (let* ((dx (%denominator x)) (n (- (%numerator x) (* y dx))))
                (%make-ratio n dx)))
             (double-float (rat-dfloat - x y))
             (short-float (rat-sfloat - x y))
             (complex (complex (- x (%realpart y)) 
                               (- (%imagpart y))))))))


;;; BUILD-RATIO  --  Internal
;;;
;;;    Given a numerator and denominator with the GCD already divided out, make
;;; a canonical rational.  We make the denominator positive, and check whether
;;; it is 1.
;;;

(defun build-ratio (num den)
  (if (minusp den) (setq num (- num) den (- den)))
  (case den
    (0 (divide-by-zero-error 'build-ratio num den))
    (1 num)
    (t (%make-ratio num den))))




;;; MAYBE-TRUNCATE  --  Internal
;;;
;;;    Truncate X and Y, but bum the case where Y is 1.
;;;


(defun maybe-truncate (x y)
  (if (eql y 1)
    x
    (truncate x y)))

(defun *-2 (x y)
  ;(declare (optimize (speed 3)(safety 0)))
  (flet ((integer*ratio (x y)
	   (if (eql x 0) 0
	       (let* ((ny (%numerator y))
		      (dy (%denominator y))
		      (gcd (gcd x dy)))
		 (if (eql gcd 1)
		     (%make-ratio (* x ny) dy)
		     (let ((nn (* (truncate x gcd) ny))
			   (nd (truncate dy gcd)))
		       (if (eql nd 1)
			   nn
			   (%make-ratio nn nd)))))))
	 (complex*real (x y)
	   (canonical-complex (* (%realpart x) y) (* (%imagpart x) y))))
    (number-case x
      (double-float (number-case y
                      (double-float (* (the double-float x)(the double-float y)))
                      (short-float (with-stack-double-floats ((dy y))
                                     (* (the double-float x) (the double-float dy))))
                      (rational (dfloat-rat * x y))
                      (complex (complex*real y x))))
      (short-float (number-case y
                      (double-float (with-stack-double-floats ((dx x))
                                     (* (the double-float dx) (the double-float y))))
                      (short-float (* (the short-float x) (the short-float y)))
                      (rational (sfloat-rat * x y))
                      (complex (complex*real y x))))
      (bignum (number-case y
                (fixnum (multiply-bignum-and-fixnum x y))
                (bignum (multiply-bignums x y))
                (double-float (dfloat-rat * y x))
                (short-float (sfloat-rat * y x))
                (ratio (integer*ratio x y))
                (complex (complex*real y x))))
      (fixnum (number-case y
                (bignum (multiply-bignum-and-fixnum y x))
                (fixnum (multiply-fixnums (the fixnum x) (the fixnum y)))
                (short-float (sfloat-rat * y x))
                (double-float (dfloat-rat * y x))
                (ratio (integer*ratio x y))
                (complex (complex*real y x))))
      (complex (number-case y
                 (complex (let* ((rx (%realpart x))
	                         (ix (%imagpart x))
	                         (ry (%realpart y))
	                         (iy (%imagpart y)))
	                    (canonical-complex (- (* rx ry) (* ix iy)) (+ (* rx iy) (* ix ry)))))
                 (real (complex*real x y))))
      (ratio (number-case y
               (ratio (let* ((nx (%numerator x))
	                     (dx (%denominator x))
	                     (ny (%numerator y))
	                     (dy (%denominator y))
	                     (g1 (gcd nx dy))
	                     (g2 (gcd dx ny)))
	                (build-ratio (* (maybe-truncate nx g1)
			                (maybe-truncate ny g2))
		                     (* (maybe-truncate dx g2)
			                (maybe-truncate dy g1)))))
               (integer (integer*ratio y x))
               (double-float (rat-dfloat * x y))
               (short-float (rat-sfloat * x y))
               (complex (complex*real y x)))))))



(defun integer*integer (x y &optional res)
  (declare (ignore res))
  (number-case x      
      (fixnum (number-case y
                (fixnum (* (the fixnum x) (the fixnum y)))
                (t (multiply-bignum-and-fixnum y x))))
      (bignum (number-case y
                (fixnum (multiply-bignum-and-fixnum x y))
                (t (multiply-bignums x y))))))



  

;;; INTEGER-/-INTEGER  --  Internal
;;;
;;;    Divide two integers, producing a canonical rational.  If a fixnum, we
;;; see if they divide evenly before trying the GCD.  In the bignum case, we
;;; don't bother, since bignum division is expensive, and the test is not very
;;; likely to suceed.
;;;
(defun integer-/-integer (x y)
  (if (and (typep x 'fixnum) (typep y 'fixnum))
    (multiple-value-bind (quo rem) (%fixnum-truncate x y)
      (if (eql 0 rem)
        quo
        (let ((gcd (gcd x y)))
          (declare (fixnum gcd))
          (if (eql gcd 1)
            (build-ratio x y)
            (build-ratio (%fixnum-truncate x gcd) (%fixnum-truncate y gcd))))))
      (let ((gcd (gcd x y)))
        (if (eql gcd 1)
          (build-ratio x y)
          (build-ratio (truncate x gcd) (truncate y gcd))))))



(defun /-2 (x y)
  (number-case x
    (double-float (number-case y
                    (double-float (/ (the double-float x) (the double-float y)))
                    (short-float (with-stack-double-floats ((dy y))
                                   (/ (the double-float x) (the double-float dy))))
                    (rational (dfloat-rat / x y))
                    (complex (let* ((ry (%realpart y))
                                    (iy (%imagpart y))
                                    (dn (+ (* ry ry) (* iy iy))))
                               (canonical-complex (/ (* x ry) dn) (/ (- (* x iy)) dn))))))
    (short-float (number-case y
                   (short-float (/ (the short-float x) (the short-float y)))
                   (double-float (with-stack-double-floats ((dx x))
                                   (/ (the double-float dx) (the double-float y))))
                   (rational (sfloat-rat / x y))
                   (complex (let* ((ry (%realpart y))
                                    (iy (%imagpart y))
                                    (dn (+ (* ry ry) (* iy iy))))
                               (canonical-complex (/ (* x ry) dn) (/ (- (* x iy)) dn))))))                   
    (integer (number-case y
               (double-float (rat-dfloat / x y))
               (short-float (rat-sfloat / x y))
               (integer (integer-/-integer x y))
               (complex (let* ((ry (%realpart y))
                               (iy (%imagpart y))
                               (dn (+ (* ry ry) (* iy iy))))
                          (canonical-complex (/ (* x ry) dn) (/ (- (* x iy)) dn))))
               (ratio
                (if (eql 0 x)
                  0
                  (let* ((ny (%numerator y)) 
                         (dy (%denominator y)) 
                         (gcd (gcd x ny)))
                    (build-ratio (* (maybe-truncate x gcd) dy)
                                 (maybe-truncate ny gcd)))))))
    (complex (number-case y
               (complex (let* ((rx (%realpart x))
                               (ix (%imagpart x))
                               (ry (%realpart y))
                               (iy (%imagpart y))
                               (dn (+ (* ry ry) (* iy iy))))
                          (canonical-complex (/ (+ (* rx ry) (* ix iy)) dn)
                                             (/ (- (* ix ry) (* rx iy)) dn))))
               ((rational float)
                (canonical-complex (/ (%realpart x) y) (/ (%imagpart x) y)))))
    (ratio (number-case y
             (double-float (rat-dfloat / x y))
             (short-float (rat-sfloat / x y))
             (integer
              (when (eql y 0)
                (divide-by-zero-error '/ x y))
              (let* ((nx (%numerator x)) (gcd (gcd nx y)))
                (build-ratio (maybe-truncate nx gcd)
                             (* (maybe-truncate y gcd) (%denominator x)))))
             (complex (let* ((ry (%realpart y))
                             (iy (%imagpart y))
                             (dn (+ (* ry ry) (* iy iy))))
                        (canonical-complex (/ (* x ry) dn) (/ (- (* x iy)) dn))))
             (ratio
              (let* ((nx (%numerator x))
                     (dx (%denominator x))
                     (ny (%numerator y))
                     (dy (%denominator y))
                     (g1 (gcd nx ny))
                     (g2 (gcd dx dy)))
                (build-ratio (* (maybe-truncate nx g1) (maybe-truncate dy g2))
                             (* (maybe-truncate dx g2) (maybe-truncate ny g1)))))))))



(defun divide-by-zero-error (operation &rest operands)
  (error (make-condition 'division-by-zero
           :operation operation
           :operands operands)))


(defun 1+ (number)
  "Returns NUMBER + 1."
  (+-2 number 1))

(defun 1- (number)
  "Returns NUMBER - 1."
  (--2 number 1))




(defun conjugate (number)
  "Return the complex conjugate of NUMBER. For non-complex numbers, this is
  an identity."
  (number-case number
    (complex (complex (%realpart number) (- (%imagpart number))))
    (number number)))

(defun numerator (rational)
  "Return the numerator of NUMBER, which must be rational."
  (number-case rational
    (ratio (%numerator rational))
    (integer rational)))

(defun denominator (rational)
  "Return the denominator of NUMBER, which must be rational."
  (number-case rational
    (ratio (%denominator rational))
    (integer 1)))



(defun abs (number)
  "Return the absolute value of the number."
  (number-case number
   (fixnum
    (locally (declare (fixnum number))
      (if (minusp number) (- number) number)))
   (double-float
    (%double-float-abs number))
   (short-float
    (%short-float-abs number))
   (bignum
    (if (bignum-minusp number)(negate-bignum number) number))
   (ratio
    (if (minusp number) (- number) number))    
   (complex
    (let ((rx (%realpart number))
          (ix (%imagpart number)))
      (number-case rx
        (rational
         (sqrt (+ (* rx rx) (* ix ix))))
        (short-float
         (%short-float (%hypot (%double-float rx)
                               (%double-float ix))))
        (double-float
         (%hypot rx ix)))))))



(defun phase (number)
  "Return the angle part of the polar representation of a complex number.
  For complex numbers, this is (atan (imagpart number) (realpart number)).
  For non-complex positive numbers, this is 0. For non-complex negative
  numbers this is PI."
  (number-case number
    (rational
     (if (minusp number)
       (%short-float pi)
       0.0f0))
    (double-float
     (if (minusp number)
       (%double-float pi)
       0.0d0))
    (complex
     (atan (%imagpart number) (%realpart number)))
    (short-float
     (if (minusp number)
       (%short-float pi)
       0.0s0))))



; from Lib;numbers.lisp, sort of
(defun float (number &optional other)
  "Converts any REAL to a float. If OTHER is not provided, it returns a
  SINGLE-FLOAT if NUMBER is not already a FLOAT. If OTHER is provided, the
  result is the same float format as OTHER."
  (if (null other)
    (if (typep number 'float)
      number
      (%short-float number))
    (if (typep other 'double-float)
      (%double-float number)
      (if (typep other 'short-float)
        (%short-float number)
        (float number (require-type other 'float))))))





;;; If the numbers do not divide exactly and the result of (/ number divisor)
;;; would be negative then decrement the quotient and augment the remainder by
;;; the divisor.
;;;
(defun floor (number &optional divisor)
  "Return the greatest integer not greater than number, or number/divisor.
  The second returned value is (mod number divisor)."
  (if (null divisor)(setq divisor 1))
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
               (plusp number)
               (minusp number)))
      (if (called-for-mv-p)
        (values (1- tru) (+ rem divisor))
        (1- tru))
      (values tru rem))))



(defun %fixnum-floor (number divisor)
  (declare (fixnum number divisor))
  (if (eq divisor 1)
    (values number 0)
    (multiple-value-bind (tru rem) (truncate number divisor)
      (if (eq rem 0)
        (values tru 0)
        (locally (declare (fixnum tru rem))
          (if (and ;(not (zerop rem))
	           (if (minusp divisor)
                     (plusp number)
                     (minusp number)))
            (values (the fixnum (1- tru)) (the fixnum (+ rem divisor)))
            (values tru rem)))))))



;;; If the numbers do not divide exactly and the result of (/ number divisor)
;;; would be positive then increment the quotient and decrement the remainder by
;;; the divisor.
;;;
(defun ceiling (number &optional divisor)
  "Return the smallest integer not less than number, or number/divisor.
  The second returned value is the remainder."
  (if (null divisor)(setq divisor 1))
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
               (minusp number)
               (plusp number)))
      (if (called-for-mv-p)
        (values (+ tru 1) (- rem divisor))
        (+ tru 1))
      (values tru rem))))



(defun %fixnum-ceiling (number  divisor)
  "Returns the smallest integer not less than number, or number/divisor.
  The second returned value is the remainder."
  (declare (fixnum number divisor))
  (multiple-value-bind (tru rem) (%fixnum-truncate number divisor)
    (if (eq 0 rem)
      (values tru 0)
      (locally (declare (fixnum tru rem))
        (if (and ;(not (zerop rem))
	     (if (minusp divisor)
               (minusp number)
               (plusp number)))
          (values (the fixnum (+ tru 1))(the fixnum  (- rem divisor)))
          (values tru rem))))))



(defun integer-decode-denorm-short-float (mantissa sign)
  (declare (fixnum mantissa sign))
  (do* ((bias 0 (1+ bias))
	(sig mantissa (ash sig 1)))
       ((logbitp 23 sig)
	(values sig
		(- (- IEEE-single-float-bias)
		   IEEE-single-float-digits
		   bias)
		sign))))


(defun integer-decode-short-float (sfloat)
  (multiple-value-bind (mantissa exp sign)(fixnum-decode-short-float sfloat)
    (let* ((biased (- exp IEEE-single-float-bias IEEE-single-float-digits)))
      (setq sign (if (eql 0 sign) 1 -1))
      (if (eq exp 255)
	(error "Can't decode NAN/Inf: ~s" sfloat))
      (if (eql 0 exp)
	(if (eql 0 mantissa)
	  (values 0 biased sign)
	  (integer-decode-denorm-short-float (ash mantissa 1) sign))
	(values (logior #x800000 mantissa) biased sign)))))




;;; INTEGER-DECODE-FLOAT  --  Public
;;;
;;;    Dispatch to the correct type-specific i-d-f function.
;;;
(defun integer-decode-float (x)
  "Returns three values:
   1) an integer representation of the significand.
   2) the exponent for the power of 2 that the significand must be multiplied
      by to get the actual value.  This differs from the DECODE-FLOAT exponent
      by FLOAT-DIGITS, since the significand has been scaled to have all its
      digits before the radix point.
   3) -1 or 1 (i.e. the sign of the argument.)"
  (number-case x
    (short-float
     (integer-decode-short-float x))
    (double-float
     (integer-decode-double-float x))))


;;; %UNARY-TRUNCATE  --  Interface
;;;
;;;    This function is called when we are doing a truncate without any funky
;;; divisor, i.e. converting a float or ratio to an integer.  Note that we do
;;; *not* return the second value of truncate, so it must be computed by the
;;; caller if needed.
;;;
;;;    In the float case, we pick off small arguments so that compiler can use
;;; special-case operations.  We use an exclusive test, since (due to round-off
;;; error), (float most-positive-fixnum) may be greater than
;;; most-positive-fixnum.
;;;
(defun %unary-truncate (number)
  (number-case number
    (integer number)
    (ratio (truncate-no-rem (%numerator number) (%denominator number)))
    (double-float
     (if (and (< (the double-float number) 
                 (float (1- (ash 1 (- (1- target::nbits-in-word) target::fixnumshift))) 0.0d0))
              (< (float (ash -1 (- (1- target::nbits-in-word) target::fixnumshift)) 0.0d0)
	         (the double-float number)))
       (%truncate-double-float->fixnum number)
       (%truncate-double-float number)))
    (short-float
     (if (and (< (the short-float number) 
                 (float (1- (ash 1 (- (1- target::nbits-in-word) target::fixnumshift))) 0.0s0))
              (< (float (ash -1 (- (1- target::nbits-in-word) target::fixnumshift)) 0.0s0)
	         (the short-float number)))
       (%truncate-short-float->fixnum number)
       (%truncate-short-float number)))))



; cmucl:compiler:float-tran.lisp
(defun xform-truncate (x)
  (let ((res (%unary-truncate x)))
    (values res (- x res))))



(defun truncate (number &optional divisor)
  "Returns number (or number/divisor) as an integer, rounded toward 0.
  The second returned value is the remainder."
  (if (null divisor)(setq divisor 1))
  (when (not (called-for-mv-p))
    (return-from truncate (truncate-no-rem number divisor)))
  (macrolet 
      ((truncate-rat-dfloat (number divisor)
         `(with-stack-double-floats ((fnum ,number)
                                     (f2))
           (let ((res (%unary-truncate (%double-float/-2! fnum ,divisor f2))))
             (values res 
                     (%double-float--2 fnum (%double-float*-2! (%double-float res f2) ,divisor f2))))))
       (truncate-rat-sfloat (number divisor)
         #+32-bit-target
         `(target::with-stack-short-floats ((fnum ,number)
                                            (f2))
           (let ((res (%unary-truncate (%short-float/-2! fnum ,divisor f2))))
             (values res 
                     (%short-float--2 fnum (%short-float*-2! (%short-float res f2) ,divisor f2)))))
         #+64-bit-target
         `(let* ((temp (%short-float ,number))
                 (res (%unary-truncate (/ (the short-float temp)
                                          (the short-float ,divisor)))))
           (values res
            (- (the short-float temp)
             (the short-float (* (the short-float (%short-float res))
                                 (the short-float ,divisor)))))))
       )
    (number-case number
      (fixnum
       (number-case divisor
         (fixnum (if (eq divisor 1) (values number 0) (%fixnum-truncate number divisor)))
         (bignum (values 0 number))
         (double-float (truncate-rat-dfloat number divisor))
         (short-float (truncate-rat-sfloat number divisor))
         (ratio (let ((q (truncate (* number (%denominator divisor)) ; this was wrong
                                   (%numerator divisor))))
                  (values q (- number (* q divisor)))))))
      (bignum (number-case divisor
                (fixnum (if (eq divisor 1) (values number 0)
                          (if (eq divisor target::target-most-negative-fixnum);; << aargh
                            (with-small-bignum-buffers ((bd divisor))
                              (bignum-truncate number bd))
                            (bignum-truncate-by-fixnum number divisor))))
                (bignum (bignum-truncate number divisor))
                (double-float  (truncate-rat-dfloat number divisor))
                (short-float (truncate-rat-sfloat number divisor))
                (ratio (let ((q (truncate (* number (%denominator divisor)) ; so was this
                                          (%numerator divisor))))
                         (values q (- number (* q divisor)))))))
      (short-float (if (eql divisor 1)
                     (let* ((res (%unary-truncate number)))
                       (values res (- number res)))
                     (number-case divisor
                       (short-float
                        #+32-bit-target
                        (target::with-stack-short-floats ((f2))
                          (let ((res (%unary-truncate (%short-float/-2! number divisor f2))))
                            (values res 
                                    (%short-float--2
                                     number 
                                     (%short-float*-2! (%short-float res f2) divisor f2)))))
                        #+64-bit-target
                        (let ((res (%unary-truncate
                                    (/ (the short-float number)
                                       (the short-float divisor)))))
                          (values res
                                  (- (the short-float number)
                                     (* (the short-float (%short-float res))
                                        (the short-float divisor))))))
                       ((fixnum bignum ratio)
                        #+32-bit-target
                        (target::with-stack-short-floats ((fdiv divisor)
                                                          (f2))
                          (let ((res (%unary-truncate (%short-float/-2! number fdiv f2))))
                            (values res 
                                    (%short-float--2 
                                     number 
                                     (%short-float*-2! (%short-float res f2) fdiv f2)))))
                        #+64-bit-target
                        (let* ((fdiv (%short-float divisor))
                               (res (%unary-truncate
                                     (/ (the short-float number)
                                        (the short-float fdiv)))))
                          (values res (- number (* res fdiv))))
                                     
                        )
                       (double-float
                        (with-stack-double-floats ((fnum number)
                                                   (f2))
                          (let* ((res (%unary-truncate (%double-float/-2! fnum divisor f2))))
                            (values res
                                    (%double-float--2
                                     fnum
                                     (%double-float*-2! (%double-float res f2) divisor f2)))))))))
      (double-float (if (eql divisor 1)
                      (let ((res (%unary-truncate number)))
                        (values res (- number res)))
                      (number-case divisor
                        ((fixnum bignum ratio short-float)
                         (with-stack-double-floats ((fdiv divisor)
                                                    (f2))
                           (let ((res (%unary-truncate (%double-float/-2! number fdiv f2))))
                             (values res 
                                     (%double-float--2 
                                      number 
                                      (%double-float*-2! (%double-float res f2) fdiv f2))))))                        
                        (double-float
                         (with-stack-double-floats ((f2))
                           (let ((res (%unary-truncate (%double-float/-2! number divisor f2))))
                             (values res 
                                     (%double-float--2
                                      number 
                                      (%double-float*-2! (%double-float res f2) divisor f2)))))))))
      (ratio (number-case divisor
               (double-float (truncate-rat-dfloat number divisor))
               (short-float (truncate-rat-sfloat number divisor))
               (rational
                (let ((q (truncate (%numerator number)
                                   (* (%denominator number) divisor))))
                  (values q (- number (* q divisor))))))))))

(defun truncate-no-rem (number  divisor)
  "Returns number (or number/divisor) as an integer, rounded toward 0."
  (macrolet 
    ((truncate-rat-dfloat (number divisor)
       `(with-stack-double-floats ((fnum ,number)
                                      (f2))
         (%unary-truncate (%double-float/-2! fnum ,divisor f2))))
     (truncate-rat-sfloat (number divisor)
       #+32-bit-target
       `(target::with-stack-short-floats ((fnum ,number)
                                      (f2))
         (%unary-truncate (%short-float/-2! fnum ,divisor f2)))
       #+64-bit-target
       `(let ((fnum (%short-float ,number)))
         (%unary-truncate (/ (the short-float fnum)
                           (the short-float ,divisor))))))
    (number-case number
    (fixnum
     (if (eql number target::target-most-negative-fixnum)
       (if (zerop divisor)
         (error 'division-by-zero :operation 'truncate :operands (list number divisor))
         (with-small-bignum-buffers ((bn number))
           (let* ((result (truncate-no-rem bn divisor)))
             (if (eq result bn)
               number
               result))))
       (number-case divisor
         (fixnum (if (eq divisor 1) number (values (%fixnum-truncate number divisor))))
         (bignum 0)
         (double-float (truncate-rat-dfloat number divisor))
         (short-float (truncate-rat-sfloat number divisor))
         (ratio (let ((q (truncate (* number (%denominator divisor))
                                   (%numerator divisor))))
                  q)))))
     (bignum (number-case divisor
               (fixnum (if (eq divisor 1) number
                         (if (eq divisor target::target-most-negative-fixnum)
                           (with-small-bignum-buffers ((bd divisor))
                             (bignum-truncate number bd :no-rem))
                           (bignum-truncate-by-fixnum number divisor))))
               (bignum (bignum-truncate number divisor :no-rem))
               (double-float  (truncate-rat-dfloat number divisor))
               (short-float (truncate-rat-sfloat number divisor))
               (ratio (let ((q (truncate (* number (%denominator divisor))
                                         (%numerator divisor))))
                        Q))))
     (double-float (if (eql divisor 1)
                     (let ((res (%unary-truncate number)))
                       RES)
                     (number-case divisor
                       ((fixnum bignum ratio)
                        (with-stack-double-floats ((fdiv divisor)
                                                   (f2))
                          (let ((res (%unary-truncate (%double-float/-2! number fdiv f2))))
                            RES)))
                       (short-float
                        (with-stack-double-floats ((ddiv divisor)
                                                   (f2))
                          (%unary-truncate (%double-float/-2! number ddiv f2))))
                       (double-float
                        (with-stack-double-floats ((f2))
                          (%unary-truncate (%double-float/-2! number divisor f2)))))))
     (short-float (if (eql divisor 1)
                    (let ((res (%unary-truncate number)))
                      RES)
                    (number-case divisor
                      ((fixnum bignum ratio)
                       #+32-bit-target
                       (target::with-stack-short-floats ((fdiv divisor)
                                                 (f2))
                         (let ((res (%unary-truncate (%short-float/-2! number fdiv f2))))
                           RES))
                       #+64-bit-target
                       (%unary-truncate (/ (the short-float number)
                                           (the short-float (%short-float divisor)))))
                      (short-float
                       #+32-bit-target
                       (target::with-stack-short-floats ((ddiv divisor)
                                                      (f2))
                         (%unary-truncate (%short-float/-2! number ddiv f2)))
                       #+64-bit-target
                       (%unary-truncate (/ (the short-float number)
                                           (the short-float (%short-float divisor)))))
                      (double-float
                       (with-stack-double-floats ((n2 number)
						      (f2))
                         (%unary-truncate (%double-float/-2! n2 divisor f2)))))))
    (ratio (number-case divisor
                  (double-float (truncate-rat-dfloat number divisor))
                  (short-float (truncate-rat-sfloat number divisor))
                  (rational
                   (let ((q (truncate (%numerator number)
                                      (* (%denominator number) divisor))))
                     Q)))))))


;;; %UNARY-ROUND  --  Interface
;;;
;;;    Similar to %UNARY-TRUNCATE, but rounds to the nearest integer.  If we
;;; can't use the round primitive, then we do our own round-to-nearest on the
;;; result of i-d-f.  [Note that this rounding will really only happen with
;;; double floats, since the whole single-float fraction will fit in a fixnum,
;;; so all single-floats larger than most-positive-fixnum can be precisely
;;; represented by an integer.]
;;;
;;; returns both values today

(defun %unary-round (number)
  (number-case number
    (integer (values number 0))
    (ratio (let ((q (round (%numerator number) (%denominator number))))             
             (values q (- number q))))
    (double-float
     (if (and (< (the double-float number) 
                 (float (1- (ash 1 (- (1- target::nbits-in-word) target::fixnumshift))) 1.0d0))
              (< (float (ash -1 (- (1- target::nbits-in-word) target::fixnumshift)) 1.0d0)
                 (the double-float number)))
       (let ((round (%unary-round-to-fixnum number)))
         (values round (- number round)))
       (multiple-value-bind (trunc rem) (truncate number)         
         (if (not (%double-float-minusp number))
           (if (or (> rem 0.5d0)(and (= rem 0.5d0) (oddp trunc)))
             (values (+ trunc 1) (- rem 1.0d0))
             (values trunc rem))
           (if (or (> rem -0.5d0)(and (evenp trunc)(= rem -0.5d0)))
             (values trunc rem)
             (values (1- trunc) (+ 1.0d0 rem)))))))
    (short-float
     (if (and (< (the short-float number) 
                 (float (1- (ash 1 (- (1- target::nbits-in-word) target::fixnumshift))) 1.0s0))
              (< (float (ash -1 (- (1- target::nbits-in-word) target::fixnumshift)) 1.0s0)
                 (the double-float number)))
       (let ((round (%unary-round-to-fixnum number)))
         (values round (- number round)))
       (multiple-value-bind (trunc rem) (truncate number)         
         (if (not (%short-float-minusp number))
           (if (or (> rem 0.5s0)(and (= rem 0.5s0) (oddp trunc)))
             (values (+ trunc 1) (- rem 1.0s0))
             (values trunc rem))
           (if (or (> rem -0.5s0)(and (evenp trunc)(= rem -0.5s0)))
             (values trunc rem)
             (values (1- trunc) (+ 1.0s0 rem)))))))))

(defun %unary-round-to-fixnum (number)
  (number-case number
    (double-float
     (%round-nearest-double-float->fixnum number))
    (short-float
     (%round-nearest-short-float->fixnum number))))

                         
                                
         
; cmucl:compiler:float-tran.lisp
#|
(defun xform-round (x)
  (let ((res (%unary-round x)))
    (values res (- x res))))
|#

#|
(defun round (number &optional divisor)
  "Rounds number (or number/divisor) to nearest integer.
  The second returned value is the remainder."
  (if (null divisor)(setq divisor 1))
  (if (eql divisor 1)
    (xform-round number)
    (multiple-value-bind (tru rem) (truncate number divisor)
      (let ((thresh (if (integerp divisor) (ash (abs divisor) -1)(/ (abs divisor) 2)))) ; does this need to be a ratio?
        (cond ((or (> rem thresh)
                   (and (= rem thresh) (oddp tru)))
               (if (minusp divisor)
                 (values (- tru 1) (+ rem divisor))
                 (values (+ tru 1) (- rem divisor))))
              ((let ((-thresh (- thresh)))
                 (or (< rem -thresh)
                     (and (= rem -thresh) (oddp tru))))
               (if (minusp divisor)
                 (values (+ tru 1) (- rem divisor))
                 (values (- tru 1) (+ rem divisor))))
              (t (values tru rem)))))))
|#


(defun %fixnum-round (number divisor)
  (declare (fixnum number divisor))
  (multiple-value-bind (quo rem)(truncate number divisor) ; should => %fixnum-truncate
    (if (= 0 rem)
      (values quo rem)
      (locally (declare (fixnum quo rem))
        (let* ((minusp-num (minusp number))
               (minusp-div (minusp divisor))
               (2rem (* rem (if (neq minusp-num minusp-div) -2 2))))
          ;(declare (fixnum 2rem)) ; no way jose  
          ;(truncate (1- most-positive-fixnum) most-positive-fixnum)
          ; 2rem has same sign as divisor
          (cond (minusp-div              
                 (if (or (< 2rem divisor)
                         (and (= 2rem divisor)(logbitp 0 quo)))
                   (if minusp-num
                     (values (the fixnum (+ quo 1))(the fixnum (- rem divisor)))
                     (values (the fixnum (- quo 1))(the fixnum (+ rem divisor))))
                   (values quo rem)))
                (t (if (or (> 2rem divisor)
                           (and (= 2rem divisor)(logbitp 0 quo)))
                     (if minusp-num
                       (values (the fixnum (- quo 1))(the fixnum (+ rem divisor)))
                       (values (the fixnum (+ quo 1))(the fixnum (- rem divisor))))
                     (values quo rem)))))))))
#|
; + + => + +
; + - => - +
; - + => - -
; - - => + -
(defun %fixnum-round (number divisor)
  (declare (fixnum number divisor))
  "Rounds number (or number/divisor) to nearest integer.
  The second returned value is the remainder."
  (if (eq divisor 1)
    (values number 0)
    (multiple-value-bind (tru rem) (truncate number divisor)
      (if (= 0 rem)
        (values tru rem)
        (locally (declare (fixnum tru rem))
          (let* ((minusp-num (minusp number))
                 (minusp-div (minusp divisor))
                 (half-div (ash (if minusp-div (- divisor) divisor) -1))
                 (abs-rem (if minusp-num (- rem) rem)))           
            (declare (fixnum half-div abs-rem)) ; true of abs-rem?
            (if (or (> abs-rem half-div)
                    (and 
                     (not (logbitp 0 divisor))
                     (logbitp 0 tru) ; oddp
                     (= abs-rem half-div)))
              (if (eq minusp-num minusp-div)
                (values (the fixnum (+ tru 1))(the fixnum (- rem divisor)))
                (values (the fixnum (- tru 1))(the fixnum (+ rem divisor))))
              (values tru rem))))))))
|#



;; makes 1 piece of garbage instead of average of 2
(defun round (number &optional divisor)
  "Rounds number (or number/divisor) to nearest integer.
  The second returned value is the remainder."
  (if (null divisor)(setq divisor 1))
  (if (eql divisor 1)
    (%unary-round number)
    (multiple-value-bind (tru rem) (truncate number divisor)
      (if (= 0 rem)
        (values tru rem)
        (let* ((mv-p (called-for-mv-p))
               (minusp-num (minusp number))
               (minusp-div (minusp divisor))
               (2rem (* rem (if (neq minusp-num minusp-div) -2 2))))
          ; 2rem has same sign as divisor
          (cond (minusp-div              
                 (if (or (< 2rem divisor)
                         (and (= 2rem divisor)(oddp tru)))
                   (if mv-p
                     (if minusp-num
                       (values (+ tru 1)(- rem divisor))
                       (values (- tru 1)(+ rem divisor)))
                     (if minusp-num (+ tru 1)(- tru 1)))
                   (values tru rem)))
                (t (if (or (> 2rem divisor)
                           (and (= 2rem divisor)(oddp tru)))
                     (if mv-p
                       (if minusp-num
                         (values (- tru 1)(+ rem divisor))
                         (values (+ tru 1)(- rem divisor)))
                       (if minusp-num (- tru 1)(+ tru 1)))
                     (values tru rem)))))))))


;; #-PPC IN L1-NUMBERS.LISP (or implement %%numdiv)
;; Anyone caught implementing %%numdiv will be summarily executed.
(defun rem (number divisor)
  "Returns second result of TRUNCATE."
  (number-case number
    (fixnum
     (number-case divisor
       (fixnum (nth-value 1 (%fixnum-truncate number divisor)))
       (bignum number)
       (t (nth-value 1 (truncate number divisor)))))
    (bignum
     (number-case divisor
       (fixnum
        (if (eq divisor target::target-most-negative-fixnum)
          (nth-value 1 (truncate number divisor))
          (bignum-truncate-by-fixnum-no-quo number divisor)))
       (bignum
        (bignum-rem number divisor))
       (t (nth-value 1 (truncate number divisor)))))
    (t (nth-value 1 (truncate number divisor)))))

;; #-PPC IN L1-NUMBERS.LISP (or implement %%numdiv)
;; See above.
(defun mod (number divisor)
  "Returns second result of FLOOR."
  (let ((rem (rem number divisor)))
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(+ rem divisor)
	rem)))

(defun cis (theta)
  "Return cos(Theta) + i sin(Theta), i.e. exp(i Theta)."
  (if (complexp theta)
    (error "Argument to CIS is complex: ~S" theta)
    (complex (cos theta) (sin theta))))


(defun complex (realpart &optional (imagpart 0))
  "Return a complex number with the specified real and imaginary components."
  (number-case realpart
    (short-float
      (number-case imagpart
         (short-float (canonical-complex realpart imagpart))
         (double-float (canonical-complex (%double-float realpart) imagpart))
         (rational (canonical-complex realpart (%short-float imagpart)))))
    (double-float 
     (number-case imagpart
       (double-float (canonical-complex
                      (the double-float realpart)
                      (the double-float imagpart)))
       (short-float (canonical-complex realpart (%double-float imagpart)))
       (rational (canonical-complex
                              (the double-float realpart)
                              (the double-float (%double-float imagpart))))))
    (rational (number-case imagpart
                (double-float (canonical-complex
                               (the double-float (%double-float realpart))
                               (the double-float imagpart)))
                (short-float (canonical-complex (%short-float realpart) imagpart))
                (rational (canonical-complex realpart imagpart))))))  

;; #-PPC IN L1-NUMBERS.LISP
(defun realpart (number)
  "Extract the real part of a number."
  (number-case number
    (complex (%realpart number))
    (number number)))

;; #-PPC IN L1-NUMBERS.LISP
(defun imagpart (number)
  "Extract the imaginary part of a number."
  (number-case number
    (complex (%imagpart number))
    (float (* 0 number))
    (rational 0)))

(defun logand-2 (x y)  
  (number-case x
    (fixnum (number-case y
              (fixnum
               (%ilogand (the fixnum x)(the fixnum y)))
              (bignum (fix-big-logand x y))))
    (bignum (number-case y
              (fixnum (fix-big-logand y x))
              (bignum (bignum-logical-and x y))))))

(defun logior-2 (x y)
  (number-case x
    (fixnum (number-case y
              (fixnum (%ilogior2 x y))
              (bignum
               (if (zerop x)
                 y
                 (with-small-bignum-buffers ((bx x))
                   (bignum-logical-ior bx y))))))
    (bignum (number-case y
              (fixnum (if (zerop y)
                        x
                        (with-small-bignum-buffers ((by y))
                          (bignum-logical-ior x by))))
              (bignum (bignum-logical-ior x y))))))

(defun logxor-2 (x y)
  (number-case x
    (fixnum (number-case y
              (fixnum (%ilogxor2 x y))
              (bignum
               (with-small-bignum-buffers ((bx x))
                 (bignum-logical-xor bx y)))))
    (bignum (number-case y
              (fixnum (with-small-bignum-buffers ((by y))
                        (bignum-logical-xor x by)))
              (bignum (bignum-logical-xor x y))))))

               

; see cmucl:compiler:srctran.lisp for transforms

(defun lognand (integer1 integer2)
  "Complement the logical AND of INTEGER1 and INTEGER2."
  (lognot (logand integer1 integer2)))

(defun lognor (integer1 integer2)
  "Complement the logical AND of INTEGER1 and INTEGER2."
  (lognot (logior integer1 integer2)))

(defun logandc1 (x y)
  "Return the logical AND of (LOGNOT integer1) and integer2."
  (number-case x
    (fixnum (number-case y               
              (fixnum (%ilogand (%ilognot x) y))
              (bignum  (fix-big-logandc1 x y))))    ; (%ilogand-fix-big (%ilognot x) y))))
    (bignum (number-case y
              (fixnum  (fix-big-logandc2 y x))      ; (%ilogandc2-fix-big y x))
              (bignum (bignum-logandc2 y x))))))    ;(bignum-logical-and (bignum-logical-not x)  y))))))


#| ; its in numbers
(defun logandc2 (integer1 integer2)
  "Returns the logical AND of integer1 and (LOGNOT integer2)."
  (logand integer1 (lognot integer2)))
|#

(defun logorc1 (integer1 integer2)
  "Return the logical OR of (LOGNOT integer1) and integer2."
  (logior (lognot integer1) integer2))

#|
(defun logorc2 (integer1 integer2)
  "Returns the logical OR of integer1 and (LOGNOT integer2)."
  (logior integer1 (lognot integer2)))
|#

(defun logtest (integer1 integer2)
  "Predicate which returns T if logand of integer1 and integer2 is not zero."
 ; (not (zerop (logand integer1 integer2)))
  (number-case integer1
    (fixnum (number-case integer2
              (fixnum (not (= 0 (%ilogand integer1 integer2))))
              (bignum (logtest-fix-big integer1 integer2))))
    (bignum (number-case integer2
              (fixnum (logtest-fix-big integer2 integer1))
              (bignum (bignum-logtest integer1 integer2)))))) 



(defun lognot (number)
  "Return the bit-wise logical not of integer."
  (number-case number
    (fixnum (%ilognot number))
    (bignum (bignum-logical-not number))))

(defun logcount (integer)
  "Count the number of 1 bits if INTEGER is positive, and the number of 0 bits
  if INTEGER is negative."
  (number-case integer
    (fixnum
     (%ilogcount (if (minusp (the fixnum integer))
                   (%ilognot integer)
                   integer)))
    (bignum
     (bignum-logcount integer))))



(defun ash (integer count)
  "Shifts integer left by count places preserving sign. - count shifts right."
  (etypecase integer
    (fixnum
     (etypecase count
       (fixnum
	(if (eql integer 0)
	  0
	  (if (eql count 0)
	    integer
	    (let ((length (integer-length (the fixnum integer))))
	      (declare (fixnum length count))
	      (cond ((and (plusp count)
			  (> (+ length count)
			     (- (1- target::nbits-in-word) target::fixnumshift)))
		     (with-small-bignum-buffers ((bi integer))
		       (bignum-ashift-left bi count)))
		    ((and (minusp count) (< count (- (1- target::nbits-in-word))))
		     (if (minusp integer) -1 0))
		    (t (%iash (the fixnum integer) count)))))))
       (bignum
	(if (minusp count)
	  (if (minusp integer) -1 0)          
	  (error "Count ~s too large for ASH" count)))))
    (bignum
     (etypecase count
       (fixnum
        (if (eql count 0) 
          integer
          (if (plusp count)
            (bignum-ashift-left integer count)
            (bignum-ashift-right integer (- count)))))
       (bignum
        (if (minusp count)
          (if (minusp integer) -1 0)
          (error "Count ~s too large for ASH" count)))))))

(defun integer-length (integer)
  "Return the number of significant bits in the absolute value of integer."
  (number-case integer
    (fixnum
     (%fixnum-intlen (the fixnum integer)))
    (bignum
     (bignum-integer-length integer))))


; not CL, used below
(defun byte-mask (size)
  (1- (ash 1 (the fixnum size))))

(defun byte-position (bytespec)
  "Return the position part of the byte specifier bytespec."
  (if (> bytespec 0)
    (- (integer-length bytespec) (logcount bytespec))
    (- bytespec)))


; CMU CL returns T.
(defun upgraded-complex-part-type (type)
  "Return the element type of the most specialized COMPLEX number type that
   can hold parts of type SPEC."
  (declare (ignore type))
  'real)

;;; This is the MRG31k3p random number generator described in
;;; P. L'Ecuyer and R. Touzin, "Fast Combined Multiple Recursive
;;; Generators with Multipliers of the form a = +/- 2^d +/- 2^e",
;;; Proceedings of the 2000 Winter Simulation Conference, Dec. 2000,
;;; 683--689.
;;;
;;; A link to the paper is available on L'Ecuyer's web site:
;;; http://www.iro.umontreal.ca/~lecuyer/papers.html.
;;;
;;; This generator has a period of about 2^185.  It produces values in
;;; in the half-open interval [0, 2^31 - 1).
;;;
;;; It uses 6 words of state.

(defconstant mrg31k3p-m1 #.(- (expt 2 31) 1))
(defconstant mrg31k3p-m2 #.(- (expt 2 31) 21069))
(defconstant mrg31k3p-limit #.(1- (expt 2 31))
	     "Exclusive upper bound on values returned by %mrg31k3p.")


;;; This is a portable version of the MRG31k3p generator.  It's not
;;; too bad in a 64-bit CCL, but the generator pretty much has to be
;;; in LAP for 32-bit ports.
#-(or x8632-target ppc32-target x8664-target ppc64-target)
(defun %mrg31k3p (state)
  (let* ((v (random.mrg31k3p-state state)))
    (declare (type (simple-array (unsigned-byte 32) (*)) v)
	     (optimize speed))
    (let ((y1 (+ (+ (ash (logand (aref v 1) #x1ff) 22)
		    (ash (aref v 1) -9))
		 (+ (ash (logand (aref v 2) #xffffff) 7)
		    (ash (aref v 2) -24)))))
      (declare (type (unsigned-byte 32) y1))
      (if (>= y1 mrg31k3p-m1) (decf y1 mrg31k3p-m1))
      (incf y1 (aref v 2))
      (if (>= y1 mrg31k3p-m1) (decf y1 mrg31k3p-m1))
      (setf (aref v 2) (aref v 1)
	    (aref v 1) (aref v 0)
	    (aref v 0) y1))
    (let ((y1 (+ (ash (logand (aref v 3) #xffff) 15)
		 (* 21069 (ash (aref v 3) -16))))
	  (y2 (+ (ash (logand (aref v 5) #xffff) 15)
		 (* 21069 (ash (aref v 5) -16)))))
      (declare (type (unsigned-byte 32) y1 y2))
      (if (>= y1 mrg31k3p-m2) (decf y1 mrg31k3p-m2))
      (if (>= y2 mrg31k3p-m2) (decf y2 mrg31k3p-m2))
      (incf y2 (aref v 5))
      (if (>= y2 mrg31k3p-m2) (decf y2 mrg31k3p-m2))
      (incf y2 y1)
      (if (>= y2 mrg31k3p-m2) (decf y2 mrg31k3p-m2))
      (setf (aref v 5) (aref v 4)
	    (aref v 4) (aref v 3)
	    (aref v 3) y2))
    (let* ((x10 (aref v 0))
	   (x20 (aref v 3)))
      (if (<= x10 x20)
	(+ (- x10 x20) mrg31k3p-m1)
	(- x10 x20)))))

(eval-when (:compile-toplevel :execute)
  (declaim (inline %16-random-bits)))

(defun %16-random-bits (state)
  (logand #xffff (the fixnum (%mrg31k3p state))))

#+64-bit-target
(defun %big-fixnum-random (number state)
  (declare (fixnum number)
	   (ftype (function (random-state) fixnum) %mrg31k3p))
  (let ((low (ldb (byte 30 0) (%mrg31k3p state)))
	(high (ldb (byte 30 0) (%mrg31k3p state))))
    (declare (fixnum low high))
    (fast-mod (logior low (the fixnum (ash high 30)))
	      number)))

;;; When using a dead simple random number generator, it's reasonable
;;; to take 16 bits of the output and discard the rest.  With a more
;;; expensive generator, however, it may be worthwhile to do more bit
;;; fiddling here here so that we can use all of the random bits
;;; produced by %mrg31k2p.
#+32-bit-target
(defun %bignum-random (number state)
  (let* ((bits (+ (integer-length number) 8))
         (half-words (ash (the fixnum (+ bits 15)) -4))
         (long-words (ash (+ half-words 1) -1))
         (dividend (%alloc-misc long-words target::subtag-bignum))
         (16-bit-dividend dividend)
         (index 1))
    (declare (fixnum long-words index bits)
             (dynamic-extent dividend)
             (type (simple-array (unsigned-byte 16) (*)) 16-bit-dividend) ;lie
             (optimize (speed 3) (safety 0)))
    (loop
       ;; This had better inline due to the lie above, or it will error
       #+big-endian-target
       (setf (aref 16-bit-dividend index) (%16-random-bits state))
       #+little-endian-target
       (setf (aref 16-bit-dividend (the fixnum (1- index)))
	     (%16-random-bits state))
       (decf half-words)
       (when (<= half-words 0) (return))
       #+big-endian-target
       (setf (aref 16-bit-dividend (the fixnum (1- index)))
	     (%16-random-bits state))
       #+little-endian-target
       (setf (aref 16-bit-dividend index) (%16-random-bits state))
       (decf half-words)
       (when (<= half-words 0) (return))
       (incf index 2))
    ;; The bignum code expects normalized bignums
    (let* ((result (mod dividend number)))
      (if (eq dividend result)
	(copy-uvector result)
	result))))

(defun %float-random (number state)
  (let ((ratio (gvector :ratio (random target::target-most-positive-fixnum state) target::target-most-positive-fixnum)))
    (declare (dynamic-extent ratio))
    (* number ratio)))

(defun random (number &optional (state *random-state*))
  (if (not (typep state 'random-state)) (report-bad-arg state 'random-state))
  (cond
    ((and (fixnump number) (> (the fixnum number) 0))
     #+32-bit-target
     (fast-mod (%mrg31k3p state) number)
     #+64-bit-target
     (if (< number mrg31k3p-limit)
       (fast-mod (%mrg31k3p state) number)
       (%big-fixnum-random number state)))
    ((and (typep number 'double-float) (> (the double-float number) 0.0))
     (%float-random number state))
    ((and (typep number 'short-float) (> (the short-float number) 0.0s0))
     (%float-random number state))
    ((and (bignump number) (> number 0))
     (%bignum-random number state))
    (t (report-bad-arg number '(or (integer (0)) (float (0.0)))))))

(eval-when (:compile-toplevel :execute)
  (defmacro bignum-abs (nexp)
    (let ((n (gensym)))
      `(let ((,n ,nexp))
         (if  (bignum-minusp ,n) (negate-bignum ,n) ,n))))
  
  (defmacro fixnum-abs (nexp)
    (let ((n (gensym)))
      `(let ((,n ,nexp))
         (if (minusp (the fixnum ,n))
           (if (eq ,n target::target-most-negative-fixnum)
             (- ,n)
             (the fixnum (- (the fixnum ,n))))
           ,n))))
  )
  

;;; TWO-ARG-GCD  --  Internal
;;;
;;;    Do the GCD of two integer arguments.  With fixnum arguments, we use the
;;; binary GCD algorithm from Knuth's seminumerical algorithms (slightly
;;; structurified), otherwise we call BIGNUM-GCD.  We pick off the special case
;;; of 0 before the dispatch so that the bignum code doesn't have to worry
;;; about "small bignum" zeros.
;;;
(defun gcd-2 (n1 n2)
  ;(declare (optimize (speed 3)(safety 0)))
  (cond 
   ((eql n1 0) (%integer-abs n2))
   ((eql n2 0) (%integer-abs n1))
   (t (number-case n1
        (fixnum 
         (number-case n2
          (fixnum
	   (if (eql n1 target::target-most-negative-fixnum)
	     (if (eql n2 target::target-most-negative-fixnum)
	       (- target::target-most-negative-fixnum)
	       (bignum-fixnum-gcd (- target::target-most-negative-fixnum) (abs n2)))
	     (if (eql n2 target::target-most-negative-fixnum)
	       (bignum-fixnum-gcd (- target::target-most-negative-fixnum) (abs n1))
	       (locally
		   (declare (optimize (speed 3) (safety 0))
			    (fixnum n1 n2))
		 (if (minusp n1)(setq n1 (the fixnum (- n1))))
		 (if (minusp n2)(setq n2 (the fixnum (- n2))))
               (%fixnum-gcd n1 n2)))))
           (bignum (if (eql n1 target::target-most-negative-fixnum)
		     (%bignum-bignum-gcd n2 (- target::target-most-negative-fixnum))
		     (bignum-fixnum-gcd (bignum-abs n2)(fixnum-abs n1))))))
	(bignum
	 (number-case n2
	   (fixnum
            (if (eql n2 target::target-most-negative-fixnum)
              (%bignum-bignum-gcd (bignum-abs n1)(fixnum-abs n2))
              (bignum-fixnum-gcd (bignum-abs n1)(fixnum-abs n2))))
	   (bignum (%bignum-bignum-gcd n1 n2))))))))

#|
(defun fixnum-gcd (n1 n2)
  (declare (optimize (speed 3) (safety 0))
           (fixnum n1 n2))                    
  (do* ((k 0 (%i+ 1 k))
        (n1 n1 (%iasr 1 n1))
        (n2 n2 (%iasr 1 n2)))
       ((oddp (logior n1 n2))
        (do ((temp (if (oddp n1) (the fixnum (- n2)) (%iasr 1 n1))
                   (%iasr 1 temp)))
            (nil)
          (declare (fixnum temp))
          (when (oddp temp)
            (if (plusp temp)
              (setq n1 temp)
              (setq n2 (- temp)))
            (setq temp (the fixnum (- n1 n2)))
            (when (zerop temp)
              (let ((res (%ilsl k n1)))
                (return res))))))
    (declare (fixnum n1 n2 k))))
|#



(defun %quo-1 (n)
  (/ 1 n))

; x & y must both be double floats
(defun %hypot (x y)
  (with-stack-double-floats ((x**2) (y**2))
    (let ((res**2 x**2))
      (%double-float*-2! x x x**2)
      (%double-float*-2! y y y**2)
      (%double-float+-2! x**2 y**2 res**2)
      (fsqrt res**2))))


