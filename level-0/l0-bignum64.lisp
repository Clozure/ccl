;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

#+64-bit-target
(eval-when (:compile-toplevel :execute)
  (require "ARCH")
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO")

  (defsetf bignum-ref bignum-set)
  
  (defconstant digit-size 32)
  (defconstant half-digit-size (/ digit-size 2))
  
  (defconstant maximum-bignum-length (1- (ash 1 56)))
  (defconstant all-ones-digit #xffffffff)
  (deftype bignum-index () `(integer 0 (,maximum-bignum-length)))
  (deftype bignum-element-type () `(unsigned-byte ,digit-size))
  (deftype bignum-half-element-type () `(unsigned-byte ,half-digit-size))
  (deftype bignum-type () 'bignum)
  (defmacro %normalize-bignum-macro (big)
    `(%normalize-bignum-2 t ,big))

  (defmacro %mostly-normalize-bignum-macro (big)
    `(%normalize-bignum-2 nil ,big))
  (defmacro %lognot (x)
    `(logand #xffffffff (lognot (the fixnum ,x))))
  (defmacro %logior (x y)
    `(logior (the fixnum ,x) (the fixnum ,y)))
  (defmacro %logxor (x y)
    `(logand #xffffffff (logxor (the fixnum ,x) (the fixnum ,y))))
  
  ;;; BIGNUM-REPLACE -- Internal.
  ;;;
  (defmacro bignum-replace (dest src &key (start1 '0) end1 (start2 '0) end2
                                 from-end)
    (once-only ((n-dest dest)
                (n-src src))
               (if (and (eq start1 0)(eq start2 0)(null end1)(null end2)(null from-end))
                 ;; this is all true for some uses today <<
                 `(%copy-ivector-to-ivector ,n-src 0 ,n-dest 0 (%ilsl 2 (min (the fixnum (%bignum-length ,n-src))
                                                                         (the fixnum (%bignum-length ,n-dest)))))
                 (let* ((n-start1 (gensym))
                        (n-end1 (gensym))
                        (n-start2 (gensym))
                        (n-end2 (gensym)))
                   `(let ((,n-start1 ,start1)
                          (,n-start2 ,start2)
                          (,n-end1 ,(or end1 `(%bignum-length ,n-dest)))
                          (,n-end2 ,(or end2 `(%bignum-length ,n-src))))
                     ,(if (null from-end)            
                          `(%copy-ivector-to-ivector
                            ,n-src (%i* 4 ,n-start2) 
                            ,n-dest (%i* 4 ,n-start1)
                            (%i* 4 (min (%i- ,n-end2 ,n-start2) 
                                    (%i- ,n-end1 ,n-start1))))
                          `(let ((nwds (min (%i- ,n-end2 ,n-start2)
                                            (%i- ,n-end1 ,n-start1))))
                            (%copy-ivector-to-ivector
                             ,n-src (%ilsl 2 (%i- ,n-end2 nwds))
                             ,n-dest (%ilsl 2 (%i- ,n-end1 nwds))
                             (%i* 4 nwds))))))))) 
  

  ;;;; Shifting.
  
  (defconstant all-ones-half-digit #xFFFF)  
  

;;; %ALLOCATE-BIGNUM must zero all elements.
;;;
  (defmacro %allocate-bignum (ndigits)
    `(%alloc-misc ,ndigits target::subtag-bignum))

  (declaim (inline  %bignum-length))

;;; This macro is used by BIGNUM-ASHIFT-RIGHT,
;;; BIGNUM-BUFFER-ASHIFT-RIGHT, and BIGNUM-LDB-BIGNUM-RES. They supply
;;; a termination form that references locals established by this
;;; form. Source is the source bignum. Start-digit is the first digit
;;; in source from which we pull bits. Start-pos is the first bit we
;;; want. Res-len-form is the form that computes the length of the
;;; resulting bignum. Termination is a DO termination form with a test
;;; and body. When result is supplied, it is the variable to which
;;; this binds a newly allocated bignum.
;;;
;;; Given start-pos, 1-31 inclusively, of shift, we form the j'th resulting
;;; digit from high bits of the i'th source digit and the start-pos number of
;;; bits from the i+1'th source digit.
  (defmacro shift-right-unaligned (source
                                   start-digit
                                   start-pos
                                   res-len-form
                                   termination
                                   &optional result)
    `(let* ((high-bits-in-first-digit (- digit-size ,start-pos))
            (res-len ,res-len-form)
            (res-len-1 (1- res-len))
            ,@(if result `((,result (%allocate-bignum res-len)))))
      (declare (type bignum-index res-len res-len-1))
      (do ((i ,start-digit i+1)
           (i+1 (1+ ,start-digit) (1+ i+1))
           (j 0 (1+ j)))
          ,termination
        (declare (type bignum-index i i+1 j))
        (setf (bignum-ref ,(if result result source) j)
              (%logior (%digit-logical-shift-right (bignum-ref ,source i)
                                                   ,start-pos)
                       (%ashl (bignum-ref ,source i+1)
                              high-bits-in-first-digit))))))


  )


#+64-bit-target
(progn

;;; Extract the length of the bignum.
;;; 
(defun %bignum-length (bignum)
  (uvsize bignum)) 



;;; We can probably do better than UVREF here, but
;;; a) it's not -that- bad
;;; b) it does some bounds/sanity checking, which isn't a bad idea.

(defmacro bignum-ref (b i)
  `(%typed-miscref :bignum ,b ,i))

(defmacro bignum-set (b i val)
  `(%typed-miscset :bignum ,b ,i ,val))


(defun bignum-plusp (b)
  (not (logbitp (1- digit-size) (the bignum-element-type (bignum-ref b (1- (%bignum-length b)))))))

;;; Return T if digit is positive, or NIL if negative.
(defun %digit-0-or-plusp (digit)
  (declare (type bignum-element-type digit))
  (not (logbitp (1- digit-size) digit)))

(defun %bignum-0-or-plusp (bignum len)
  (declare (type bignum-type bignum)
	   (type bignum-index len))
  (%digit-0-or-plusp (bignum-ref bignum (1- len))))

(defun bignum-minusp (b)
  (logbitp 31 (the fixnum (bignum-ref b (1- (%bignum-length b))))))

(defun %sign-digit (b i)
  (%ashr (bignum-ref b (1- i)) (1- digit-size)))

;;; Return the sign of bignum (0 or -1) as a fixnum
(defun %bignum-sign (b)
  (if (logbitp 31 (the fixnum (bignum-ref b (1- (%bignum-length b)))))
    -1
    0))

         
(defun %add-with-carry (a-digit b-digit carry-in)
  (declare (fixnum a-digit b-digit carry-in))
  (setq a-digit (logand all-ones-digit a-digit)
        b-digit (logand all-ones-digit b-digit))
  (let* ((sum (+ carry-in (the fixnum (+ a-digit b-digit)))))
    (declare (fixnum sum))
    (values (logand all-ones-digit sum) (logand 1 (the fixnum (ash sum -32))))))

(defun %subtract-with-borrow (a-digit b-digit borrow-in)
  (declare (fixnum a-digit b-digit borrow-in))
  (setq a-digit (logand all-ones-digit a-digit)
        b-digit (logand all-ones-digit b-digit))
  (let* ((diff (- (the fixnum (- a-digit b-digit))
                  (the fixnum (- 1 borrow-in)))))
    (declare (fixnum diff))
    (values (logand all-ones-digit diff)
            (- 1 (logand (the fixnum (ash diff -32)) 1)))))



(defun %compare-digits (bignum-a bignum-b idx)
  (let* ((a (bignum-ref bignum-a idx))
         (b (bignum-ref bignum-b idx)))
    (declare (fixnum a b))
    (if (= a b)
      0
      (if (> a b)
        1
        -1))))


;;;; Addition.
(defun add-bignums (a b)
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b)))
    (declare (bignum-index len-a len-b)
             (optimize (speed 3) (safety 0)))
    (when (> len-b len-a)
      (rotatef a b)
      (rotatef len-a len-b))
    (let* ((len-res (1+ len-a))
	   (res (%allocate-bignum len-res))
	   (carry 0)
	   (sign-b (%bignum-sign b)))
	(dotimes (i len-b)
          (let* ((sum (+
                       (the fixnum (+ (the bignum-element-type (bignum-ref a i))
                                      (the bignum-element-type (bignum-ref b i))))
                       carry)))
            (declare (fixnum sum))
            (setf (bignum-ref res i) sum)
            (setq carry (logand 1 (the fixnum (ash sum -32))))))
	(if (/= len-a len-b)
	  (finish-bignum-add  res carry a sign-b len-b len-a)
          (setf (bignum-ref res len-a)
                (+ (the fixnum carry)
                   (the fixnum (+ (the bignum-element-type (%bignum-sign a))
                                  sign-b)))))
	(%normalize-bignum-macro res))))

(defun add-bignum-and-fixnum (bignum fixnum)
  (declare (bignum-type bignum)
           (fixnum fixnum)
           (optimize (speed 3) (safety 0)))
  (let* ((len-bignum (%bignum-length bignum))
         (len-res (1+ len-bignum))
         (res (%allocate-bignum len-res))
         (low (logand all-ones-digit fixnum))
         (high (logand all-ones-digit (the fixnum (ash fixnum -32)))))
    (declare (bignum-index len-bignum)
             (bignum-type res)
             (bignum-element-type low high))
    (let* ((sum0 (+ (the bignum-element-type (bignum-ref bignum 0)) low))
           (sum1 (+ (the fixnum (+ (the bignum-element-type (bignum-ref bignum 1))
                                   high))
                    (the fixnum (logand 1 (ash sum0 -32)))))
           (carry (logand 1 (ash sum1 -32))))
      (declare (fixnum sum0 sum1) (bignum-element-type carry))
      (setf (bignum-ref res 0) sum0
            (bignum-ref res 1) sum1)
      (if (> len-bignum 2)
        (finish-bignum-add  res carry bignum (ash fixnum (- (- target::nbits-in-word target::fixnumshift))) 2 len-bignum)
        (setf (bignum-ref res 2)
              (+ (the fixnum carry)
                 (the fixnum (+ (the bignum-element-type (%bignum-sign bignum))
                                (the fixnum (ash fixnum (- (- target::nbits-in-word target::fixnumshift)))))))))
      (%normalize-bignum-macro res))))





;;; B was shorter than A; keep adding B's sign digit to each remaining
;;; digit of A, propagating the carry.
(defun finish-bignum-add (result carry a sign-b start end)
  (declare (type bignum-index start end)
           (bignum-element-type sign-b carry)
           (optimize (speed 3) (safety 0)))
  (do* ((i start (1+ i))
        (sign-b (logand all-ones-digit sign-b)))
       ((= i end)
        (setf (bignum-ref result end)
              (the fixnum (+
                           (the fixnum (+ (the fixnum
                                            (logand all-ones-digit
                                                    (the fixnum
                                                      (%sign-digit a end))))
                                          sign-b))
                           carry))))
    (declare (fixnum i) (bignum-element-type sign-b))
    (let* ((sum (the fixnum (+ (the fixnum (+ (bignum-ref a i)
                                              sign-b))
                               carry))))
      (declare (fixnum sum))
      (setf (bignum-ref result i) sum)
      (setq carry (logand 1 (the fixnum (ash sum -32)))))))




;;;; Subtraction.
(defun subtract-bignum (a b)
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (len-res (1+ (max len-a len-b)))
	 (res (%allocate-bignum len-res)))
    (declare (bignum-index len-a len-b len-res))
    (bignum-subtract-loop a len-a b len-b res)
    (%normalize-bignum-macro res)))

(defun bignum-subtract-loop (a len-a b len-b res)
  (declare (bignum-index len-a len-b )
           (optimize (speed 3) (safety 0)))
  (let* ((len-res (%bignum-length res)))
    (declare (bignum-index len-res))
    (let* ((borrow 1)
	   (sign-a (%bignum-sign a))
	   (sign-b (%bignum-sign b)))
      (declare (bignum-element-type borrow sign-a sign-b))
      (dotimes (i (the bignum-index len-res))
        (multiple-value-bind (result-digit borrow-out)
            (%subtract-with-borrow
             (if (< i len-a)
               (bignum-ref a i)
               sign-a)
             (if (< i len-b)
               (bignum-ref b i)
               sign-b)
             borrow)
          (setf (bignum-ref res i) result-digit
                borrow borrow-out))))))


;;;; Multiplication.

#||
;;; These parameters match GMP's.
(defvar *sqr-basecase-threshold* 5)
(defvar *sqr-karatsuba-threshold* 22)
(defvar *mul-karatsuba-threshold* 10)

;;; Squaring is often simpler than multiplication.  This should never
;;; be called with (>= N *sqr-karatsuba-threshold*).
(defun mpn-sqr-basecase (prodp up n)
  (declare (fixnum prodp up n))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (umulppm up up prodp)
  (when (> n 1)
    (%stack-block ((tarr (* 4 (* 2 *sqr-karatsuba-threshold*))))
      (let* ((tp (macptr->fixnum tarr)))
	(mpn-mul-1 tp
		   (the fixnum (1+ up))
		   (the fixnum (1- n))
		   up
		   (the fixnum (+ tp (the fixnum (1- n)))))
	(do* ((i 2 (1+ i)))
	     ((= i n))
	  (declare (fixnum i))
	  (mpn-addmul-1 (the fixnum (- (the fixnum (+ tp (the fixnum (+ i i))))
				       2))
			(the fixnum (+ up i))
			(the fixnum (- n i))
			(the fixnum (+ up (the fixnum (1- i))))
			(the fixnum (+ tp (the fixnum (+ n (the fixnum (- i 2))))))))
	(do* ((i 1 (1+ i))
	      (ul (1+ up) (1+ ul)))
	     ((= i n))
	  (declare (fixnum i ul))
	  (umulppm ul ul (the fixnum (+ prodp (the fixnum (+ i i))))))
	(let* ((2n-2 (- (the fixnum (+ n n)) 2))
	       (carry (mpn-lshift-1 tp tp 2n-2)))
	  (declare (fixnum 2n-2 carry))
	  (incf carry (the fixnum (mpn-add-n (the fixnum (1+ prodp))
					     (the fixnum (1+ prodp))
					     tp
					     2n-2)))
	  (add-fixnum-to-limb carry (the fixnum (+ prodp
						   (the fixnum (1-
								(the fixnum
								  (+ n n))))))))))))

;;; For large enough values of N, squaring via Karatsuba-style
;;; divide&conquer is faster than in the base case.
(defun mpn-kara-sqr-n (p a n ws)
  (declare (fixnum p a n ws))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (%stack-block ((limbs 16))
    (let* ((w (macptr->fixnum limbs))
	   (w0 (1+ w))
	   (w1 (1+ w0))
	   (xx (1+ w1))
	   (n2 (ash n -1))
	   (x 0)
	   (y 0)
	   (i 0))
      (declare (fixnum w w0 w1 xx n2 x y i))
      (cond ((logbitp 0 n)
	     ;; Odd length
	     (let* ((n3 (- n n2))
		    (n1 0)
		    (nm1 0))
	       (declare (fixnum n3 n1 nm1))
	       (copy-limb (the fixnum (+ a n2)) w)
	       (if (not (limb-zerop w))
		 (add-fixnum-to-limb
		  (the fixnum
		    (- (the fixnum (mpn-sub-n p a (the fixnum (+ a n3)) n2))))
		  w)
		 (progn
		   (setq i n2)
		   (loop
		     (decf i)
		     (copy-limb (the fixnum (+ a i)) w0)
		     (copy-limb (the fixnum (+ a (the fixnum (+ n3 i)))) w1)
		     (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
			     (= i 0))
		       (return)))
		   (if (< (the fixnum (compare-limbs w0 w1)) 0)
		     (setq x (+ a n3)
			   y a)
		     (setq y (+ a n3)
			   x a))
		   (mpn-sub-n p x y n2)))
	       (copy-limb w (the fixnum (+ p n2)))
	       (setq n1 (1+ n))
	       (cond ((< n3 *sqr-basecase-threshold*)
		      (mpn-mul-basecase ws p n3 p n3)
		      (mpn-mul-basecase p a n3 a n3))
		     ((< n3 *sqr-karatsuba-threshold*)
		      (mpn-sqr-basecase ws p n3)
		      (mpn-sqr-basecase p a n3))
		     (t
		      (mpn-kara-sqr-n ws p n3 (the fixnum (+ ws n1)))
		      (mpn-kara-sqr-n p  a n3 (the fixnum (+ ws n1)))))
	       (cond ((< n2 *sqr-basecase-threshold*)
		      (mpn-mul-basecase (the fixnum (+ p n1))
					(the fixnum (+ a n3))
					n2
					(the fixnum (+ a n3))
					n2))
		     ((< n2 *sqr-karatsuba-threshold*)
		      (mpn-sqr-basecase (the fixnum (+ p n1))
					(the fixnum (+ a n3))
					n2))
		     (t
		      (mpn-kara-sqr-n (the fixnum (+ p n1))
				      (the fixnum (+ a n3))
				      n2
				      (the fixnum (+ ws n1)))))
	       (mpn-sub-n ws p ws n1)
	       (setq nm1 (1- n))
	       (unless (zerop (the fixnum
				(mpn-add-n ws
					   (the fixnum (+ p n1))
					   ws
					   nm1)))
		 (copy-limb (the fixnum (+ ws nm1)) xx)
		 (add-fixnum-to-limb 1 xx)
		 (copy-limb xx (the fixnum (+ ws nm1)))
		 (if (limb-zerop xx)
		   (add-fixnum-to-limb 1 (the fixnum (+ ws n)))))
	       (unless (zerop
			(the fixnum
			  (mpn-add-n (the fixnum (+ p n3))
				     (the fixnum (+ p n3))
				     ws
				     n1)))
		 (mpn-incr-u (the fixnum (+ p (the fixnum (+ n1 n3))))
			     1))))
	    (t ; N is even
	     (setq i n2)
	     (loop
	       (decf i)
	       (copy-limb (the fixnum (+ a i)) w0)
	       (copy-limb (the fixnum (+ a (the fixnum (+ n2 i)))) w1)
	       (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
		       (= i 0))
		 (return)))
	     (if (< (the fixnum (compare-limbs w0 w1)) 0)
	       (setq x (+ a n2)
		     y a)
	       (setq y (+ a n2)
		     x a))
	     (mpn-sub-n p x y n2)
	     (cond ((< n2 *sqr-basecase-threshold*)
		    (mpn-mul-basecase ws p n2 p n2)
		    (mpn-mul-basecase p a n2 a n2)
		    (mpn-mul-basecase (the fixnum (+ p n))
				      (the fixnum (+ a n2))
				      n2
				      (the fixnum (+ a n2))
				      n2))
		   ((< n2 *sqr-karatsuba-threshold*)
		    (mpn-sqr-basecase ws p n2)
		    (mpn-sqr-basecase p a n2)
		    (mpn-sqr-basecase (the fixnum (+ p n))
				      (the fixnum (+ a n2))
				      n2))
		   (t
		    (mpn-kara-sqr-n ws p n2 (the fixnum (+ ws n)))
		    (mpn-kara-sqr-n p  a n2 (the fixnum (+ ws n)))
		    (mpn-kara-sqr-n (the fixnum (+ p n))
				    (the fixnum (+ a n2))
				    n2
				    (the fixnum (+ ws n)))))
	     (let* ((ww (- (the fixnum (mpn-sub-n ws p ws n)))))
	       (declare (fixnum ww))
	       (setq ww (+ ww (mpn-add-n ws (the fixnum (+ p n)) ws n)))
	       (setq ww (+ ww (mpn-add-n (the fixnum (+ p n2))
                                         (the fixnum (+ p n2))
                                         ws
                                         n)))
	       (mpn-incr-u (the fixnum (+ p (the fixnum (+ n2 n)))) ww)))))))

;;; Karatsuba subroutine: multiply A and B, store result at P, use WS
;;; as scrach space.  Treats A and B as if they were both of size N;
;;; if that's not true, caller must fuss around the edges.
(defun mpn-kara-mul-n (p a b n ws)
  (declare (fixnum p a b n ws))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (%stack-block ((limbs 16))
    (let* ((w (macptr->fixnum limbs))
	   (w0 (1+ w))
	   (w1 (1+ w0))
	   (xx (1+ w1))
	   (x 0)
	   (y 0)
	   (i 0)
	   (n2 (ash n -1))
	   (sign 0))
      (declare (fixnum w w0 w1 xx x y i n2 sign))
      (cond ((logbitp 0 n)
	     (let* ((n1 0)
		    (n3 (- n n2))
		    (nm1 0))
	       (declare (fixnum n1 n3 nm1))
	       (copy-limb (the fixnum (+ a n2)) w)
	       (if (not (limb-zerop w))
		 (add-fixnum-to-limb
		  (the fixnum (- (mpn-sub-n p a (the fixnum (+ a n3)) n2))) w)
		 (progn
		   (setq i n2)
		   (loop
		     (decf i)
		     (copy-limb (the fixnum (+ a i)) w0)
		     (copy-limb (the fixnum (+ a (the fixnum (+ n3 i)))) w1)
		     (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
			     (zerop i))
		       (return)))
		   (if (< (the fixnum (compare-limbs w0 w1)) 0)
		     (setq x (+ a n3)
			   y a
			   sign -1)
		     (setq x a
			   y (+ a n3)))
		   (mpn-sub-n p x y n2)))
	       (copy-limb w (the fixnum (+ p n2)))
	       (copy-limb (the fixnum (+ b n2)) w)
	       (if (not (limb-zerop w))
		 (add-fixnum-to-limb
		  (the fixnum (- (the fixnum (mpn-sub-n (the fixnum (+ p n3))
							b
							(the fixnum (+ b n3))
							n2))))
		  w)
		 (progn
		   (setq i n2)
		   (loop
		     (decf i)
		     (copy-limb (the fixnum (+ b i)) w0)
		     (copy-limb (the fixnum (+ b (the fixnum (+ n3 i)))) w1)
		     (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
			     (zerop i))
		       (return)))
		   (if (< (the fixnum (compare-limbs w0 w1)) 0)
		     (setq x (+ b n3)
			   y b
			   sign (lognot sign))
		     (setq x b
			   y (+ b n3)))
		   (mpn-sub-n (the fixnum (+ p n3)) x y n2)))
	       (copy-limb w (the fixnum (+ p n)))
	       (setq n1 (1+ n))
	       (cond
		 ((< n2 *mul-karatsuba-threshold*)
		  (cond
		    ((< n3 *mul-karatsuba-threshold*)
		     (mpn-mul-basecase ws p n3 (the fixnum (+ p n3)) n3)
		     (mpn-mul-basecase p a n3 b n3))
		    (t
		     (mpn-kara-mul-n ws p (the fixnum (+ p n3)) n3 (the fixnum (+ ws n1)))
		     (mpn-kara-mul-n p a b n3 (the fixnum (+ ws n1)))))
		  (mpn-mul-basecase (the fixnum (+ p n1))
				    (the fixnum (+ a n3))
				    n2
				    (the fixnum (+ b n3))
				    n2))
		 (t
		  (mpn-kara-mul-n ws p (the fixnum (+ p n3)) n3 (the fixnum (+ ws n1)))
		  (mpn-kara-mul-n p a b n3 (the fixnum (+ ws n1)))
		  (mpn-kara-mul-n (the fixnum (+ p n1))
				  (the fixnum (+ a n3))
				  (the fixnum (+ b n3))
				  n2
				  (the fixnum (+ ws n1)))))
	       (if (not (zerop sign))
		 (mpn-add-n ws p ws n1)
		 (mpn-sub-n ws p ws n1))
	       (setq nm1 (1- n))
	       (unless (zerop (the fixnum (mpn-add-n ws
						     (the fixnum (+ p n1))
						     ws
						     nm1)))
		 (copy-limb (the fixnum (+ ws nm1)) xx)
		 (add-fixnum-to-limb 1 xx)
		 (copy-limb xx (the fixnum (+ ws nm1)))
		 (if (limb-zerop xx)
		   (add-fixnum-to-limb 1 (the fixnum (+ ws n)))))
	       (unless (zerop (the fixnum
				(mpn-add-n (the fixnum (+ p n3))
					   (the fixnum (+ p n3))
					   ws
					   n1)))
		 (mpn-incr-u (the fixnum
			       (+ p (the fixnum (+ n1 n3)))) 1))))
	    (t				; even length
	     (setq i n2)
	     (loop
	       (decf i)
	       (copy-limb (the fixnum (+ a i)) w0)
	       (copy-limb (the fixnum (+ a (the fixnum (+ n2 i)))) w1)
	       (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
		       (zerop i))
		 (return)))
	     (setq sign 0)
	     (if (< (the fixnum (compare-limbs w0 w1)) 0)
	       (setq x (+ a n2)
		     y a
		     sign -1)
	       (setq x a
		     y (+ a n2)))
	     (mpn-sub-n p x y n2)
	     (setq i n2)
	     (loop
	       (decf i)
	       (copy-limb (the fixnum (+ b i)) w0)
	       (copy-limb (the fixnum (+ b (the fixnum (+ n2 i)))) w1)
	       (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
		       (zerop i))
		 (return)))	      
	     (if (< (the fixnum (compare-limbs w0 w1)) 0)
	       (setq x (+ b n2)
		     y b
		     sign (lognot sign))
	       (setq x b
		     y (+ b n2)))
	     (mpn-sub-n (the fixnum (+ p n2)) x y n2)
	     (cond
	       ((< n2 *mul-karatsuba-threshold*)
		(mpn-mul-basecase ws p n2 (the fixnum (+ p n2)) n2)
		(mpn-mul-basecase p a n2 b n2)
		(mpn-mul-basecase (the fixnum (+ p n))
				  (the fixnum (+ a n2))
				  n2
				  (the fixnum (+ b n2))
				  n2))
	       (t
		(mpn-kara-mul-n ws p (the fixnum (+ p n2)) n2
				(the fixnum (+ ws n)))
		(mpn-kara-mul-n p a b n2 (the fixnum (+ ws n)))
		(mpn-kara-mul-n (the fixnum (+ p n))
				(the fixnum (+ a n2))
				(the fixnum (+ b n2))
				n2
				(the fixnum (+ ws n)))))
	     (let* ((ww (if (not (zerop sign))
			  (mpn-add-n ws p ws n)
			  (- (the fixnum (mpn-sub-n ws p ws n))))))
	       (declare (fixnum ww))
	       (setq ww (+ ww (mpn-add-n ws (the fixnum (+ p n)) ws n)))
	       (setq ww (+ ww (mpn-add-n (the fixnum (+ p n2))
                                         (the fixnum (+ p n2))
                                         ws
                                         n)))
	       (mpn-incr-u (the fixnum (+ p (the fixnum (+ n2 n)))) ww)))))))

;;; Square UP, of length UN.  I wonder if a Karatsuba multiply might be
;;; faster than a basecase square.
(defun mpn-sqr-n (prodp up un)
  (declare (fixnum prodp up un))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (if (< un *sqr-basecase-threshold*)
    (mpn-mul-basecase prodp up un up un)
    (if (< un *sqr-karatsuba-threshold*)
      (mpn-sqr-basecase prodp up un)
      (%stack-block ((wsptr (mpn-kara-sqr-n-tsize un)))
	(mpn-kara-sqr-n prodp up un (macptr->fixnum wsptr))))))

;;; Subroutine: store AxB at P.  Assumes A & B to be of length N
(defun mpn-mul-n (p a b n)
  (declare (fixnum p a b n))
  (declare (optimize (speed 3) (safety 0) (space 0)))  
  (if (< n *mul-karatsuba-threshold*)
    (mpn-mul-basecase p a n b n)
    (%stack-block ((wsptr (mpn-kara-mul-n-tsize n)))
      (mpn-kara-mul-n p a b n (macptr->fixnum wsptr)))))


;;; Multiply [UP,UN] by [VP,VN].  UN must not be less than VN.
;;; This does Karatsuba if operands are big enough; if they are
;;; and they differ in size, this computes the product of the
;;; smaller-size slices, then fixes up the resut.
(defun mpn-mul (prodp up un vp vn)
  (declare (fixnum prodp up un vp vn))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  ;(assert (>= un vn 1))
  (if (and (= up vp) (= un vn))
    (mpn-sqr-n prodp up un)
    (if (< vn *mul-karatsuba-threshold*)
      (mpn-mul-basecase prodp up un vp vn)
      (let* ((l vn))
	(declare (fixnum l))
	(mpn-mul-n prodp up vp vn)
	(unless (= un vn)
	  (incf prodp vn)
	  (incf up vn)
	  (decf un vn)
	  (if (< un vn)
	    (psetq un vn vn un up vp vp up))
	  (%stack-block ((wsptr
			  (the fixnum
			    (+ 8
			       (the fixnum
				 (* 4
				    (the fixnum
				      (+ vn
					 (if (>= vn *mul-karatsuba-threshold*)
					   vn
					   un)))))))))
	    (setf (%get-unsigned-long wsptr 0) 0
		  (%get-unsigned-long wsptr 4) 0)
	    (let* ((tt (macptr->fixnum wsptr))
		   (c (1+ tt))
		   (ws (1+ c)))
	      (declare (fixnum tt c ws ))
	      (do* ()
		   ((< vn *mul-karatsuba-threshold*))
		(mpn-mul-n ws up vp vn)
		(cond ((<= l (the fixnum (+ vn vn)))
		       (add-fixnum-to-limb (mpn-add-n prodp prodp ws l) tt)
		       (unless (= l (the fixnum (+ vn vn)))
			 (copy-fixnum-to-limb
			  (mpn-add-1 (the fixnum (+ prodp l))
				     (the fixnum (+ ws l))
				     (the fixnum (- (the fixnum (+ vn vn)) l))
				     tt)
			  tt)
			 (setq l (the fixnum (+ vn vn)))))
		      (t
		       (copy-fixnum-to-limb
			(mpn-add-n prodp prodp ws (the fixnum (+ vn vn))) c)
		       (add-fixnum-to-limb
			(mpn-add-1 (the fixnum (+ prodp (the fixnum (+ vn vn))))
				   (the fixnum (+ prodp (the fixnum (+ vn vn))))
				   (the fixnum (- l (the fixnum (+ vn vn))))
				   c)
			tt)))
		(incf prodp vn)
		(decf l vn)
		(incf up vn)
		(decf un vn)
		(if (< un vn)
		  (psetq up vp vp up un vn vn un)))
	      (unless (zerop vn)
		(mpn-mul-basecase ws up un vp vn)
		(cond ((<= l (the fixnum (+ un vn)))
		       (add-fixnum-to-limb
			(mpn-add-n prodp prodp ws l)
			tt)
		       (unless (= l (the fixnum (+ un vn)))
			 (copy-fixnum-to-limb
			  (mpn-add-1 (the fixnum (+ prodp l))
				     (the fixnum (+ ws l))
				     (the fixnum (- (the fixnum (+ un vn)) l))
				     tt)
			  tt)))
		      (t
		       (copy-fixnum-to-limb
			(mpn-add-n prodp prodp ws (the fixnum (+ un vn)))
			c)
		       (add-fixnum-to-limb
			(mpn-add-1
			 (the fixnum (+ prodp (the fixnum (+ un vn))))
			 (the fixnum (+ prodp (the fixnum (+ un vn))))
			 (the fixnum (- (the fixnum (- l un)) vn))
			 c)
			tt)))))))))))
||#

(defun multiply-bignums (a b)
  (let* ((signs-differ (not (eq (bignum-minusp a) (bignum-minusp b)))))
    (flet ((multiply-unsigned-bignums (a b)
	     (let* ((len-a (%bignum-length a))
		    (len-b (%bignum-length b))
		    (len-res (+ len-a len-b))
		    (res (%allocate-bignum len-res)))
	       (declare (bignum-index len-a len-b len-res))
	       (dotimes (i len-a)
		 (declare (type bignum-index i))
		 (%multiply-and-add-loop a b res i len-b))
	       res))
	   (multiply-unsigned-bignums64 (a b)
	     (let* ((len-a (ceiling (%bignum-length a) 2))
		    (len-b (ceiling (%bignum-length b) 2))
		    (len-res (+ len-a len-b))
		    (res (%allocate-bignum (+ len-res len-res))))
	       (declare (bignum-index len-a len-b len-res))
	       (dotimes (i len-a)
		 (declare (type bignum-index i))
		 (%multiply-and-add-loop64 a b res i len-b))
	       res)))
      (let* ((res (with-negated-bignum-buffers a b
					       multiply-unsigned-bignums64)))
	(if signs-differ (negate-bignum-in-place res))
	(%normalize-bignum-macro res)))))

(defun multiply-bignum-and-fixnum (bignum fixnum)
  (declare (type bignum-type bignum) (fixnum fixnum))
  (if (eql fixnum 1)
    bignum
    (with-small-bignum-buffers ((big-fix fixnum))
      (multiply-bignums bignum big-fix))))

#+slower
(defun multiply-bignum-and-fixnum (bignum fixnum)
  (declare (type bignum-type bignum) (fixnum fixnum))
  (if (eql fixnum 1)
    bignum
    (if (eql fixnum target::target-most-negative-fixnum)
      (with-small-bignum-buffers ((big-fix fixnum))
        (multiply-bignums bignum big-fix))
      (let* ((big-len (%bignum-length bignum))
             (big-neg (bignum-minusp bignum))
             (signs-differ (not (eq big-neg (minusp fixnum)))))
        (flet ((multiply-unsigned-bignum-and-2-digit-fixnum (a len-a high low)
                 (declare (bignum-type a)
                          (bignum-element-type high low)
                          (bignum-index len-a)
                          (optimize (speed 3) (safety 0)))
                 (let* ((len-res (+ len-a 2))
                        (res (%allocate-bignum len-res)) )
                   (declare (bignum-index len-a  len-res))
                   (dotimes (i len-a)
                     (declare (type bignum-index i))
                     (let* ((carry-digit 0)
                            (x (bignum-ref a i))
                            (k i))
                       (declare (fixnum k))
                       (multiple-value-bind (big-carry res-digit)
                           (%multiply-and-add4 x
                                               low
                                               (bignum-ref res k)
                                               carry-digit)
                         (setf (bignum-ref res k) res-digit
                               carry-digit big-carry
                               k (1+ k)))
                       (multiple-value-bind (big-carry res-digit)
                           (%multiply-and-add4 x
                                               high
                                               (bignum-ref res k)
                                               carry-digit)
                         (setf (bignum-ref res k) res-digit
                               carry-digit big-carry
                               k (1+ k)))
                       (setf (bignum-ref res k) carry-digit)))
                   res))
               (multiply-unsigned-bignum-and-1-digit-fixnum (a len-a fix)
                 (declare (bignum-type a)
                          (bignum-element-type fix)
                          (bignum-index len-a)
                          (optimize (speed 3) (safety 0)))
                 (let* ((len-res (+ len-a 1))
                        (res (%allocate-bignum len-res)) )
                   (declare (bignum-index len-a  len-res))
                   (dotimes (i len-a)
                     (declare (type bignum-index i))
                     (let* ((carry-digit 0)
                            (x (bignum-ref a i))
                            (k i))
                       (declare (fixnum k))
                       (multiple-value-bind (big-carry res-digit)
                           (%multiply-and-add4 x
                                               fix
                                               (bignum-ref res k)
                                               carry-digit)
                         (setf (bignum-ref res k) res-digit
                               carry-digit big-carry
                               k (1+ k)))
                       (setf (bignum-ref res k) carry-digit)))
                   res)))
          (let* ((fixnum (if (< fixnum 0) (- fixnum) fixnum))
                 (low (logand (1- (ash 1 32)) fixnum))
                 (high (unless (<= (%fixnum-intlen fixnum) 32)
                         (ldb (byte 32 32) fixnum)))
                 (res (if big-neg
                        (let* ((neg-len (1+ big-len)))
                          (declare (type bignum-index neg-len))
                          (with-bignum-buffers ((neg neg-len))
                            (negate-bignum bignum nil neg)
                            (setq neg-len (%bignum-length bignum))
                            (if high
                              (multiply-unsigned-bignum-and-2-digit-fixnum
                               neg
                               neg-len
                               high
                               low)
                              (multiply-unsigned-bignum-and-1-digit-fixnum
                               neg
                               neg-len
                               low))))
                        (if high
                          (multiply-unsigned-bignum-and-2-digit-fixnum
                           bignum
                           big-len
                           high
                           low)
                          (multiply-unsigned-bignum-and-1-digit-fixnum
                           bignum
                           big-len
                           low)))))
            (if signs-differ (negate-bignum-in-place res))
            (%normalize-bignum-macro res)))))))


;; assume we already know result won't fit in a fixnum
;; only caller is fixnum-*-2
;;

(defun multiply-fixnums (a b)
  (declare (fixnum a b))
  (* a b))


;;;; GCD.


;;; Both args are > 0.
(defun bignum-fixnum-gcd (bignum fixnum)
  (let* ((rem (bignum-truncate-by-fixnum-no-quo bignum fixnum)))
    (declare (fixnum rem))
    (if (zerop rem)
      fixnum
      (%fixnum-gcd rem fixnum))))



;;; NEGATE-BIGNUM -- Public.
;;;
;;; Fully-normalize is an internal optional.  It cause this to always return
;;; a bignum, without any extraneous digits, and it never returns a fixnum.
;;;
(defun negate-bignum (x &optional (fully-normalize t) res)
  (declare (type bignum-type x))
  (let* ((len-x (%bignum-length x))
	 (len-res (1+ len-x))
         (minusp (bignum-minusp x))
	 (res (or res (%allocate-bignum len-res))))
    (declare (type bignum-index len-x len-res)) ;Test len-res for range?
    (let ((carry (bignum-negate-loop-really x len-x res)))
      (declare (fixnum carry))
      (if (zerop carry)
        (setf (bignum-ref res len-x) (if minusp 0 all-ones-digit))
        (setf (bignum-ref res len-x) (if minusp 1 0))))
    (if fully-normalize
      (%normalize-bignum-macro res)
      (%mostly-normalize-bignum-macro res))))

;;; NEGATE-BIGNUM-IN-PLACE -- Internal.
;;;
;;; This assumes bignum is positive; that is, the result of negating it will
;;; stay in the provided allocated bignum.
;;;
(defun negate-bignum-in-place (bignum)
  (bignum-negate-loop-really bignum (%bignum-length bignum) bignum)
  bignum)


  

(defun copy-bignum (bignum)
  (let ((res (%allocate-bignum (%bignum-length bignum))))
    (bignum-replace res bignum)
    res))



;;; BIGNUM-ASHIFT-RIGHT -- Public.
;;;
;;; First compute the number of whole digits to shift, shifting them by
;;; skipping them when we start to pick up bits, and the number of bits to
;;; shift the remaining digits into place.  If the number of digits is greater
;;; than the length of the bignum, then the result is either 0 or -1.  If we
;;; shift on a digit boundary (that is, n-bits is zero), then we just copy
;;; digits.  The last branch handles the general case which uses a macro that a
;;; couple other routines use.  The fifth argument to the macro references
;;; locals established by the macro.
;;;


(defun bignum-ashift-right (bignum x)
  (declare (type bignum-type bignum)
           (fixnum x)
           (optimize (speed 3) (safety 0)))
  (let ((bignum-len (%bignum-length bignum)))
    (declare (type bignum-index bignum-len))
    (multiple-value-bind (digits n-bits) (truncate x digit-size)
      (declare (type bignum-index digits)(fixnum n-bits))
      (cond
       ((>= digits bignum-len)
        (if (bignum-plusp bignum) 0 -1))
       ((eql 0 n-bits)
        (bignum-ashift-right-digits bignum digits))
       (t
        (shift-right-unaligned bignum digits n-bits (- bignum-len digits)
				      ((= j res-len-1)
				       (setf (bignum-ref res j)
					     (%ashr (bignum-ref bignum i) n-bits))
				       (%normalize-bignum-macro res))
				      res))))))

			       



;;; BIGNUM-ASHIFT-RIGHT-DIGITS -- Internal.
;;;
(defun bignum-ashift-right-digits (bignum digits)
  (declare (type bignum-type bignum)
	   (type bignum-index digits))
  (let* ((res-len (- (%bignum-length bignum) digits))
	 (res (%allocate-bignum res-len)))
    (declare (type bignum-index res-len)
	     (type bignum-type res))
    (bignum-replace res bignum :start2 digits)
    (%normalize-bignum-macro res)))


;;; BIGNUM-BUFFER-ASHIFT-RIGHT -- Internal.
;;;
;;; GCD uses this for an in-place shifting operation.  This is different enough
;;; from BIGNUM-ASHIFT-RIGHT that it isn't worth folding the bodies into a
;;; macro, but they share the basic algorithm.  This routine foregoes a first
;;; test for digits being greater than or equal to bignum-len since that will
;;; never happen for its uses in GCD.  We did fold the last branch into a macro
;;; since it was duplicated a few times, and the fifth argument to it
;;; references locals established by the macro.
;;;
 

;;; BIGNUM-ASHIFT-LEFT -- Public.
;;;
;;; This handles shifting a bignum buffer to provide fresh bignum data for some
;;; internal routines.  We know bignum is safe when called with bignum-len.
;;; First we compute the number of whole digits to shift, shifting them
;;; starting to store farther along the result bignum.  If we shift on a digit
;;; boundary (that is, n-bits is zero), then we just copy digits.  The last
;;; branch handles the general case.
;;;
(defun bignum-ashift-left (bignum x &optional bignum-len)
  (declare (type bignum-type bignum)
	   (fixnum x)
	   (type (or null bignum-index) bignum-len))
  (multiple-value-bind (digits n-bits)
		       (truncate x digit-size)
    (declare (fixnum digits n-bits))
    (let* ((bignum-len (or bignum-len (%bignum-length bignum)))
	   (res-len (+ digits bignum-len 1)))
      (declare (fixnum bignum-len res-len))
      (when (> res-len maximum-bignum-length)
	(error "Can't represent result of left shift."))
      (if (zerop n-bits)
        (bignum-ashift-left-digits bignum bignum-len digits)
        (bignum-ashift-left-unaligned bignum digits n-bits res-len)))))

;;; BIGNUM-ASHIFT-LEFT-DIGITS -- Internal.
;;;
(defun bignum-ashift-left-digits (bignum bignum-len digits)
  (declare (type bignum-index bignum-len digits))
  (let* ((res-len (+ bignum-len digits))
	 (res (%allocate-bignum res-len)))
    (declare (type bignum-index res-len))
    (bignum-replace res bignum :start1 digits :end1 res-len :end2 bignum-len
		    :from-end t)
    res))



;;; BIGNUM-ASHIFT-LEFT-UNALIGNED -- Internal.
;;;
;;; BIGNUM-TRUNCATE uses this to store into a bignum buffer by supplying res.
;;; When res comes in non-nil, then this foregoes allocating a result, and it
;;; normalizes the buffer instead of the would-be allocated result.
;;;
;;; We start storing into one digit higher than digits, storing a whole result
;;; digit from parts of two contiguous digits from bignum.  When the loop
;;; finishes, we store the remaining bits from bignum's first digit in the
;;; first non-zero result digit, digits.  We also grab some left over high
;;; bits from the last digit of bignum.
;;;

(defun bignum-ashift-left-unaligned (bignum digits n-bits res-len
                                            &optional (res nil resp))
  (declare (type bignum-index digits res-len)
	   (type (mod #.digit-size) n-bits))
  (let* ((remaining-bits (- digit-size n-bits))
	 (res-len-1 (1- res-len))
	 (res (or res (%allocate-bignum res-len))))
    (declare (type bignum-index res-len res-len-1)
             (optimize (speed 3) (safety 0)))
    (do ((i 0 i+1)
	 (i+1 1 (1+ i+1))
	 (j (1+ digits) (1+ j)))
	((= j res-len-1)
	 (setf (bignum-ref res digits)
	       (%ashl (bignum-ref bignum 0) n-bits))
	 (setf (bignum-ref res j)
	       (%ashr (bignum-ref bignum i) remaining-bits))
	 (if resp
           (%zero-trailing-sign-digits res res-len)
           (%mostly-normalize-bignum-macro res)))
      (declare (type bignum-index i i+1 j))
      (setf (bignum-ref res j)
	    (%logior (%digit-logical-shift-right (bignum-ref bignum i)
						 remaining-bits)
		     (%ashl (bignum-ref bignum i+1) n-bits))))))







;;;; Relational operators.



;;; BIGNUM-COMPARE -- Public.
;;;
;;; This compares two bignums returning -1, 0, or 1, depending on whether a
;;; is less than, equal to, or greater than b.
;;;
;(proclaim '(function bignum-compare (bignum bignum) (integer -1 1)))
(defun bignum-compare (a b)
  (declare (type bignum-type a b))
  (let* ((a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (if (eq a-plusp b-plusp)
      (let* ((len-a (%bignum-length a))
	     (len-b (%bignum-length b)))
	(declare (type bignum-index len-a len-b))
	(cond ((= len-a len-b)
	       (do* ((i (1- len-a) (1- i)))
		    ((zerop i) (%compare-digits a b 0))
		 (declare (fixnum i))
		 (let* ((signum (%compare-digits a b i)))
		   (declare (fixnum signum))
		   (unless (zerop signum)
		     (return signum)))))
	      ((> len-a len-b)
	       (if a-plusp 1 -1))
	      (t (if a-plusp -1 1))))
      (if a-plusp 1 -1))))






;;;; Integer length and logcount


(defun bignum-integer-length (big)
  (the fixnum (- (the fixnum (ash (the fixnum (%bignum-length big)) 5))
		 (the fixnum (%bignum-sign-bits big)))))

; (not (zerop (logand integer1 integer2)

(defun bignum-logtest (num1 num2)
  (let* ((length1 (%bignum-length num1))
         (length2 (%bignum-length num2))
         (n1-minusp (bignum-minusp num1))
         (n2-minusp (bignum-minusp num2)))
    (declare (fixnum length1 length2))
    (if (and n1-minusp n2-minusp) ; both neg, get out quick
      T        
      (or (dotimes (i (min length1 length2))
            (unless (zerop (the fixnum
                             (logand (the fixnum (bignum-ref num1 i))
                                     (the fixnum (bignum-ref num2 i)))))
              (return t)))
          (if (< length1 length2)
            n1-minusp
            (if (< length1 length2)
              n2-minusp))))))

(defun logtest-fix-big (fix big)
  (declare (fixnum fix))
  (unless (zerop fix)
    (if (plusp fix)
      (or
       (not (eql 0 (the fixnum (logand (the fixnum (bignum-ref big 0)) fix))))
       (and (> (%bignum-length big) 1)
            (not (eql 0 (the fixnum (logand (the fixnum (bignum-ref big 1))
                                            (the fixnum (ash fix -32))))))))
      t)))


(defun bignum-logcount (bignum)
  (declare (type bignum-type bignum))
  (let* ((length (%bignum-length bignum))
	 (plusp (bignum-plusp bignum))
	 (result 0))
    (declare (type bignum-index length)
	     (fixnum result))
    (if plusp
      (dotimes (index length result)
	(incf result (the fixnum (%logcount bignum index))))
      (dotimes (index length result)
	(incf result (the fixnum (%logcount-complement bignum index)))))))


;;;; Logical operations.

;;; NOT.
;;;

;;; BIGNUM-LOGICAL-NOT -- Public.
;;;
(defun bignum-logical-not (a)
  (declare (type bignum-type a))
  (let* ((len (%bignum-length a))
	 (res (%allocate-bignum len)))
    (declare (type bignum-index len))
    (dotimes (i len res)
      (bignum-set res i (%lognot (the fixnum (bignum-ref a i)))))))




;;; AND.
;;;

;;; BIGNUM-LOGICAL-AND -- Public.
;;;
(defun bignum-logical-and (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (declare (type bignum-index len-a len-b))
    (cond
      ((< len-a len-b)
       (if a-plusp
	 (logand-shorter-positive a len-a b (%allocate-bignum len-a))
	 (logand-shorter-negative a len-a b len-b (%allocate-bignum len-b))))
      ((< len-b len-a)
       (if b-plusp
	 (logand-shorter-positive b len-b a (%allocate-bignum len-b))
	 (logand-shorter-negative b len-b a len-a (%allocate-bignum len-a))))
      (t (logand-shorter-positive a len-a b (%allocate-bignum len-a))))))

;;; LOGAND-SHORTER-POSITIVE -- Internal.
;;;
;;; This takes a shorter bignum, a and len-a, that is positive.  Because this
;;; is AND, we don't care about any bits longer than a's since its infinite 0
;;; sign bits will mask the other bits out of b.  The result is len-a big.
;;;
(defun logand-shorter-positive (a len-a b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (logand (the fixnum (bignum-ref a i))
                  (the fixnum (bignum-ref b i)))))
  (%normalize-bignum-macro res))

;;; LOGAND-SHORTER-NEGATIVE -- Internal.
;;;
;;; This takes a shorter bignum, a and len-a, that is negative.  Because this
;;; is AND, we just copy any bits longer than a's since its infinite 1 sign
;;; bits will include any bits from b.  The result is len-b big.
;;;
(defun logand-shorter-negative (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (logand (the fixnum (bignum-ref a i))
                              (the fixnum (bignum-ref b i)))))
  (bignum-replace res b :start1 len-a :start2 len-a :end1 len-b :end2 len-b)
  (%normalize-bignum-macro res))



;;;
;;;
;;; bignum-logandc2

(defun bignum-logandc2 (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (declare (type bignum-index len-a len-b))
    (cond
     ((< len-a len-b)
      (logandc2-shorter-any a len-a b len-b (if a-plusp (%allocate-bignum len-a) (%allocate-bignum len-b))))
     ((< len-b len-a) ; b shorter 
      (logandc1-shorter-any b len-b a len-a (if b-plusp (%allocate-bignum len-a)(%allocate-bignum len-b))))
     (t (logandc2-shorter-any a len-a b len-b (%allocate-bignum len-a))))))

(defun logandc2-shorter-any (a len-a b len-b res)
  (declare (type bignum-type a b res)
           (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (logand (the fixnum (bignum-ref a i))
                  (the fixnum (%lognot (the fixnum (bignum-ref b i)))))))
  (if (bignum-minusp a)
    (do ((i len-a (1+ i)))
          ((= i len-b))
        (declare (type bignum-index i))
      (setf (bignum-ref res i)
            (%lognot (the fixnum (bignum-ref b i))))))
  (%normalize-bignum-macro res))



(defun logandc1-shorter-any (a len-a b len-b res)
  (declare (type bignum-type a b res)
           (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (logand
           (the fixnum (%lognot (the fixnum (bignum-ref a i))))
           (the fixnum (bignum-ref b i)))))
  (when (bignum-plusp a)
    (unless (= len-a len-b)
      (bignum-replace res b :start1 len-a :start2 len-a :end1 len-b :end2 len-b)))
  (%normalize-bignum-macro res))



(defun fix-big-logand (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (< fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logand fix big res)))
      (if res
        (progn
          (bignum-replace res big :start1 2 :start2 2 :end1 len-b :end2 len-b)
          (%normalize-bignum-macro res))
        val))))


(defun fix-big-logandc2 (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (< fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logandc2 fix big res)))
      (if res
        (progn
          (do ((i 2 (1+ i)))
              ((= i len-b))
            (declare (type bignum-index i))
            (setf (bignum-ref res i)
                  (%lognot (bignum-ref big i))))
          (%normalize-bignum-macro res))
        val))))

(defun fix-big-logandc1 (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (>= fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logandc1 fix big res)))
      (if res
        (progn  
          (bignum-replace res big :start1 2 :start2 2 :end1 len-b :end2 len-b)
          (%normalize-bignum-macro res))
        val))))


;;; IOR.
;;;

;;; BIGNUM-LOGICAL-IOR -- Public.
;;;
(defun bignum-logical-ior (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (declare (type bignum-index len-a len-b))
    (cond
     ((< len-a len-b)
      (if a-plusp
	  (logior-shorter-positive a len-a b len-b (%allocate-bignum len-b))
	  (logior-shorter-negative a len-a b len-b (%allocate-bignum len-b))))
     ((< len-b len-a)
      (if b-plusp
	  (logior-shorter-positive b len-b a len-a (%allocate-bignum len-a))
	  (logior-shorter-negative b len-b a len-a (%allocate-bignum len-a))))
     (t (logior-shorter-positive a len-a b len-b (%allocate-bignum len-a))))))

;;; LOGIOR-SHORTER-POSITIVE -- Internal.
;;;
;;; This takes a shorter bignum, a and len-a, that is positive.  Because this
;;; is IOR, we don't care about any bits longer than a's since its infinite
;;; 0 sign bits will mask the other bits out of b out to len-b.  The result
;;; is len-b long.
;;;
(defun logior-shorter-positive (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (logior (the fixnum (bignum-ref a i))
                  (the fixnum (bignum-ref b i)))))
  (if (not (eql len-a len-b))
    (bignum-replace res b :start1 len-a :start2 len-a :end1 len-b :end2 len-b))
  (%normalize-bignum-macro res))

;;; LOGIOR-SHORTER-NEGATIVE -- Internal.
;;;
;;; This takes a shorter bignum, a and len-a, that is negative.  Because this
;;; is IOR, we just copy any bits longer than a's since its infinite 1 sign
;;; bits will include any bits from b.  The result is len-b long.
;;;
(defun logior-shorter-negative (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (logior (the fixnum (bignum-ref a i))
                  (the fixnum (bignum-ref b i)))))
  (do ((i len-a (1+ i)))
      ((= i len-b))
    (declare (type bignum-index i))
    (setf (bignum-ref res i) #xffffffff))
  (%normalize-bignum-macro res))




;;; XOR.
;;;

;;; BIGNUM-LOGICAL-XOR -- Public.
;;;
(defun bignum-logical-xor (a b)
  (declare (type bignum-type a b))
  (let ((len-a (%bignum-length a))
	(len-b (%bignum-length b)))
    (declare (type bignum-index len-a len-b))
    (if (< len-a len-b)
	(bignum-logical-xor-aux a len-a b len-b (%allocate-bignum len-b))
	(bignum-logical-xor-aux b len-b a len-a (%allocate-bignum len-a)))))

;;; BIGNUM-LOGICAL-XOR-AUX -- Internal.
;;;
;;; This takes the the shorter of two bignums in a and len-a.  Res is len-b
;;; long.  Do the XOR.
;;;
(defun bignum-logical-xor-aux (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (%logxor (the fixnum (bignum-ref a i))
                  (the fixnum (bignum-ref b i)))))
  (unless (= len-a len-b)
    (let ((sign (if (bignum-minusp a) all-ones-digit 0)))
      (declare (fixnum sign))
      (do ((i len-a (1+ i)))
          ((= i len-b))
        (declare (type bignum-index i))
        (setf (bignum-ref res i)
              (%logxor (bignum-ref b i) sign)))))
  (%normalize-bignum-macro res))


;;;; TRUNCATE

;;; Divide X by Y when Y is a single bignum digit. BIGNUM-TRUNCATE
;;; fixes up the quotient and remainder with respect to sign and
;;; normalization.
;;;
;;; We don't have to worry about shifting Y to make its most
;;; significant digit sufficiently large for %FLOOR to return
;;; digit-size quantities for the q-digit and r-digit. If Y is
;;; a single digit bignum, it is already large enough for
;;; %FLOOR. That is, it has some bits on pretty high in the
;;; digit.

(defun bignum-truncate-single-digit (x len-x y)
  (declare (type bignum-index len-x))
  (let ((q (%allocate-bignum len-x))
        (r 0)
        (y (bignum-ref y 0)))
    (declare (type bignum-element-type r y))
    (do ((i (1- len-x) (1- i)))
        ((minusp i))
      (multiple-value-bind (q-digit r-digit)
          (%floor r (bignum-ref x i) y)
        (declare (type bignum-element-type q-digit r-digit))
        (setf (bignum-ref q i) q-digit)
        (setf r r-digit)))
    (let ((rem (%allocate-bignum 1)))
      (setf (bignum-ref rem 0) r)
      (values q rem))))

;;; This returns a guess for the next division step. Y1 is the
;;; highest y digit, and y2 is the second to highest y
;;; digit. The x... variables are the three highest x digits
;;; for the next division step.
;;;
;;; From Knuth, our guess is either all ones or x-i and x-i-1
;;; divided by y1, depending on whether x-i and y1 are the
;;; same. We test this guess by determining whether guess*y2
;;; is greater than the three high digits of x minus guess*y1
;;; shifted left one digit:
;;;    ------------------------------
;;;   |    x-i    |   x-i-1  | x-i-2 |
;;;    ------------------------------
;;;    ------------------------------
;;; - | g*y1 high | g*y1 low |   0   |
;;;    ------------------------------
;;;		...		  <   guess*y2     ???	 
;;; If guess*y2 is greater, then we decrement our guess by one
;;; and try again.  This returns a guess that is either
;;; correct or one too large.
(defun bignum-truncate-guess (y1 y2 x-i x-i-1 x-i-2)
  (declare (type bignum-element-type y1 y2 x-i x-i-1 x-i-2))
  (let ((guess (if (= x-i y1)
                 all-ones-digit
                 (%floor x-i x-i-1 y1))))
    (declare (type bignum-element-type guess))
    (loop
      (multiple-value-bind (high-guess*y1 low-guess*y1)
          (%multiply guess y1)
        (declare (type bignum-element-type low-guess*y1
                       high-guess*y1))
        (multiple-value-bind (high-guess*y2 low-guess*y2)
            (%multiply guess y2)
          (declare (type bignum-element-type high-guess*y2
                         low-guess*y2))
          (multiple-value-bind (middle-digit borrow)
              (%subtract-with-borrow x-i-1 low-guess*y1 1)
            (declare (type bignum-element-type middle-digit)
                     (fixnum borrow))
            ;; Supplying borrow of 1 means there was no
            ;; borrow, and we know x-i-2 minus 0 requires
            ;; no borrow.
            (let ((high-digit (%subtract-with-borrow x-i
                                                     high-guess*y1
                                                     borrow)))
              (declare (type bignum-element-type high-digit))
              (if (and (= high-digit 0)
                       (or (> high-guess*y2
                              middle-digit)
                           (and (= middle-digit
                                   high-guess*y2)
                                (> low-guess*y2
                                   x-i-2))))
                (setf guess (%subtract-with-borrow guess 1 1))
                (return guess)))))))))


;;; This returns the amount to shift y to place a one in the
;;; second highest bit. Y must be positive. If the last digit
;;; of y is zero, then y has a one in the previous digit's
;;; sign bit, so we know it will take one less than digit-size
;;; to get a one where we want. Otherwise, we count how many
;;; right shifts it takes to get zero; subtracting this value
;;; from digit-size tells us how many high zeros there are
;;; which is one more than the shift amount sought.
;;;
;;; Note: This is exactly the same as one less than the
;;; integer-length of the last digit subtracted from the
;;; digit-size.
;;;
;;; We shift y to make it sufficiently large that doing the
;;; 2*digit-size by digit-size %FLOOR calls ensures the quotient and
;;; remainder fit in digit-size.
(defun shift-y-for-truncate (y)
  (the fixnum (1- (the fixnum (%bignum-sign-bits y)))))

;;; Stores two bignums into the truncation bignum buffers,
;;; shifting them on the way in. This assumes x and y are
;;; positive and at least two in length, and it assumes
;;; truncate-x and truncate-y are one digit longer than x and
;;; y.
(defun shift-and-store-truncate-buffers (truncate-x truncate-y x len-x y len-y shift)
  (declare (type bignum-index len-x len-y)
           (type (integer 0 (#.digit-size)) shift))
  (cond ((zerop shift)
         (bignum-replace truncate-x x :end1 len-x)
         (bignum-replace truncate-y y :end1 len-y))
        (t
         (bignum-ashift-left-unaligned x 0 shift (1+ len-x)
                                       truncate-x)
         (bignum-ashift-left-unaligned y 0 shift (1+ len-y)
                                       truncate-y))))

;;; Divide TRUNCATE-X by TRUNCATE-Y, returning the quotient
;;; and destructively modifying TRUNCATE-X so that it holds
;;; the remainder.
;;;
;;; LEN-X and LEN-Y tell us how much of the buffers we care about.
;;;
;;; TRUNCATE-X definitely has at least three digits, and it has one
;;; more than TRUNCATE-Y. This keeps i, i-1, i-2, and low-x-digit
;;; happy. Thanks to SHIFT-AND-STORE-TRUNCATE-BUFFERS.

(defun do-truncate (truncate-x truncate-y len-x len-y)
  (declare (type bignum-index len-x len-y))
  (let* ((len-q (- len-x len-y))
         ;; Add one for extra sign digit in case high bit is on.
         (q (%allocate-bignum (1+ len-q)))
         (k (1- len-q))
         (y1 (bignum-ref truncate-y (1- len-y)))
         (y2 (bignum-ref truncate-y (- len-y 2)))
         (i (1- len-x))
         (i-1 (1- i))
         (i-2 (1- i-1))
         (low-x-digit (- i len-y)))
    (declare (type bignum-index len-q k i i-1 i-2 low-x-digit)
             (type bignum-element-type y1 y2))
    (loop
      (setf (bignum-ref q k)
            (try-bignum-truncate-guess
             truncate-x truncate-y
             ;; This modifies TRUNCATE-X. Must access
             ;; elements each pass.
             (bignum-truncate-guess y1 y2
                                    (bignum-ref truncate-x i)
                                    (bignum-ref truncate-x i-1)
                                    (bignum-ref truncate-x i-2))
             len-y low-x-digit))
      (cond ((zerop k) (return))
            (t (decf k)
               (decf low-x-digit)
               (shiftf i i-1 i-2 (1- i-2)))))
    q))

#+notyet
(defun do-truncate-no-quo (truncate-x truncate-y len-x len-y)
  (declare (type bignum-index len-x len-y))
  (let* ((len-q (- len-x len-y))
	 (k (1- len-q))
	 (i (1- len-x))
	 (low-x-digit (- i len-y)))
    (declare (type bignum-index len-q k i  low-x-digit))
    (loop
      (let* ((guess (bignum-truncate-guess truncate-x i truncate-y (the fixnum (1- len-y)))                                 
        (try-bignum-truncate-guess guess len-y low-x-digit)
        (cond ((zerop k) (return))
              (t (decf k)
                 (decf low-x-digit)
                 (setq i (1- i))))))
    nil))))

;;; This takes a digit guess, multiplies it by TRUNCATE-Y for a
;;; result one greater in length than LEN-Y, and subtracts this result
;;; from TRUNCATE-X. LOW-X-DIGIT is the first digit of X to start
;;; the subtraction, and we know X is long enough to subtract a LEN-Y
;;; plus one length bignum from it. Next we check the result of the
;;; subtraction, and if the high digit in X became negative, then our
;;; guess was one too big. In this case, return one less than GUESS
;;; passed in, and add one value of Y back into X to account for
;;; subtracting one too many. Knuth shows that the guess is wrong on
;;; the order of 3/b, where b is the base (2 to the digit-size power)
;;; -- pretty rarely.

(defun try-bignum-truncate-guess (truncate-x truncate-y guess len-y low-x-digit)
  (declare (type bignum-index low-x-digit len-y)
           (type bignum-element-type guess))
  (let ((carry-digit 0)
        (borrow 1)
        (i low-x-digit))
    (declare (type bignum-element-type carry-digit)
             (type bignum-index i)
             (fixnum borrow))
    ;; Multiply guess and divisor, subtracting from dividend
    ;; simultaneously.
    (dotimes (j len-y)
      (multiple-value-bind (high-digit low-digit)
          (%multiply-and-add3 guess
                              (bignum-ref truncate-y j)
                              carry-digit)
        (declare (type bignum-element-type high-digit low-digit))
        (setf carry-digit high-digit)
        (multiple-value-bind (x temp-borrow)
            (%subtract-with-borrow (bignum-ref truncate-x i)
                                   low-digit
                                   borrow)
          (declare (type bignum-element-type x)
                   (fixnum temp-borrow))
          (setf (bignum-ref truncate-x i) x)
          (setf borrow temp-borrow)))
      (incf i))
    (setf (bignum-ref truncate-x i)
          (%subtract-with-borrow (bignum-ref truncate-x i)
                                 carry-digit borrow))
    ;; See whether guess is off by one, adding one
    ;; Y back in if necessary.
    (cond ((%digit-0-or-plusp (bignum-ref truncate-x i))
           guess)
          (t
           ;; If subtraction has negative result, add one
           ;; divisor value back in. The guess was one too
           ;; large in magnitude.
           (let ((i low-x-digit)
                 (carry 0))
             (dotimes (j len-y)
               (multiple-value-bind (v k)
                   (%add-with-carry (bignum-ref truncate-y j)
                                    (bignum-ref truncate-x i)
                                    carry)
                 (declare (type bignum-element-type v))
                 (setf (bignum-ref truncate-x i) v)
                 (setf carry k))
               (incf i))
             (setf (bignum-ref truncate-x i)
                   (%add-with-carry (bignum-ref truncate-x i)
                                    0 carry)))
           (%subtract-with-borrow guess 1 1)))))

;;; Someone (from the original CMUCL or SPICE Lisp project, perhaps)
;;; is the "I" who implemented the original version of this.

;;; This is the original sketch of the algorithm from which I implemented this
;;; TRUNCATE, assuming both operands are bignums. I should modify this to work
;;; with the documentation on my functions, as a general introduction. I've
;;; left this here just in case someone needs it in the future. Don't look at
;;; this unless reading the functions' comments leaves you at a loss. Remember
;;; this comes from Knuth, so the book might give you the right general
;;; overview.
;;;
;;; (truncate x y):
;;;
;;; If X's magnitude is less than Y's, then result is 0 with remainder X.
;;;
;;; Make x and y positive, copying x if it is already positive.
;;;
;;; Shift y left until there's a 1 in the 30'th bit (most significant, non-sign
;;;       digit)
;;;    Just do most sig digit to determine how much to shift whole number.
;;; Shift x this much too.
;;; Remember this initial shift count.
;;;
;;; Allocate q to be len-x minus len-y quantity plus 1.
;;;
;;; i = last digit of x.
;;; k = last digit of q.
;;;
;;; LOOP
;;;
;;; j = last digit of y.
;;;
;;; compute guess.
;;; if x[i] = y[j] then g = (1- (ash 1 digit-size))
;;; else g = x[i]x[i-1]/y[j].
;;;
;;; check guess.
;;; %UNSIGNED-MULTIPLY returns b and c defined below.
;;;    a = x[i-1] - (logand (* g y[j]) #xFFFFFFFF).
;;;       Use %UNSIGNED-MULTIPLY taking low-order result.
;;;    b = (logand (ash (* g y[j-1]) (- digit-size)) (1- (ash 1 digit-size))).
;;;    c = (logand (* g y[j-1]) (1- (ash 1 digit-size))).
;;; if a < b, okay.
;;; if a > b, guess is too high
;;;    g = g - 1; go back to "check guess".
;;; if a = b and c > x[i-2], guess is too high
;;;    g = g - 1; go back to "check guess".
;;; GUESS IS 32-BIT NUMBER, SO USE THING TO KEEP IN SPECIAL REGISTER
;;; SAME FOR A, B, AND C.
;;;
;;; Subtract g * y from x[i - len-y+1]..x[i]. See paper for doing this in step.
;;; If x[i] < 0, guess is screwed up.
;;;    negative g, then add 1
;;;    zero or positive g, then subtract 1
;;; AND add y back into x[len-y+1..i].
;;;
;;; q[k] = g.
;;; i = i - 1.
;;; k = k - 1.
;;;
;;; If k>=0, goto LOOP.
;;;
;;; Now quotient is good, but remainder is not.
;;; Shift x right by saved initial left shifting count.
;;;
;;; Check quotient and remainder signs.
;;; x pos y pos --> q pos r pos
;;; x pos y neg --> q neg r pos
;;; x neg y pos --> q neg r neg
;;; x neg y neg --> q pos r neg
;;;
;;; Normalize quotient and remainder. Cons result if necessary.


(defun bignum-truncate (x y &optional no-rem)
  (declare (type bignum-type x y))
  (DECLARE (IGNORE NO-REM))
  ;; Divide X by Y returning the quotient and remainder. In the
  ;; general case, we shift Y to set up for the algorithm, and we
  ;; use two buffers to save consing intermediate values. X gets
  ;; destructively modified to become the remainder, and we have
  ;; to shift it to account for the initial Y shift. After we
  ;; multiple bind q and r, we first fix up the signs and then
  ;; return the normalized results.
  (let* ((x-plusp (%bignum-0-or-plusp x (%bignum-length x)))
         (y-plusp (%bignum-0-or-plusp y (%bignum-length y)))
         (x (if x-plusp x (negate-bignum x nil)))
         (y (if y-plusp y (negate-bignum y nil)))
         (len-x (%bignum-length x))
         (len-y (%bignum-length y)))
    (multiple-value-bind (q r)
        (cond ((< len-y 2)
               (bignum-truncate-single-digit x len-x y))
              ((plusp (bignum-compare y x))
               (let ((res (%allocate-bignum len-x)))
                 (dotimes (i len-x)
                   (setf (bignum-ref res i) (bignum-ref x i)))
                 (values 0 res)))
              (t
               (let ((len-x+1 (1+ len-x)))
                 (with-bignum-buffers ((truncate-x len-x+1)
                                       (truncate-y (1+ len-y)))
                   (let ((y-shift (shift-y-for-truncate y)))
                     (shift-and-store-truncate-buffers truncate-x
                                                       truncate-y
                                                       x len-x
                                                       y len-y
                                                       y-shift)
                     (values
                      (do-truncate truncate-x
                        truncate-y
                        len-x+1
                        len-y)
                      ;; Now DO-TRUNCATE has executed, we just
                      ;; tidy up the remainder (in TRUNCATE-X)
                      ;; and return it.
                      (cond
                        ((zerop y-shift)
                         (let ((res (%allocate-bignum len-y)))
                           (declare (type bignum-type res))
                           (bignum-replace res truncate-x :end2 len-y)
                           (%normalize-bignum-macro res)))
                        (t
                         (shift-right-unaligned
                          truncate-x 0 y-shift len-y
                          ((= j res-len-1)
                           (setf (bignum-ref res j)
                                 (%ashr (bignum-ref truncate-x i)
                                        y-shift))
                           (%normalize-bignum-macro res))
                          res)))))))))
      (let ((quotient (cond ((eq x-plusp y-plusp) q)
                            ((typep q 'fixnum) (the fixnum (- q)))
                            (t (negate-bignum-in-place q))))
            (rem (cond (x-plusp r)
                       ((typep r 'fixnum) (the fixnum (- r)))
                       (t (negate-bignum-in-place r)))))
        (values (if (typep quotient 'fixnum)
                  quotient
                  (%normalize-bignum-macro quotient))
                (if (typep rem 'fixnum)
                  rem
                  (%normalize-bignum-macro rem)))))))

(defun bignum-truncate-by-fixnum (bignum fixnum)
  (with-small-bignum-buffers ((y fixnum))
    (bignum-truncate bignum y)))

(defun bignum-truncate-by-fixnum-no-quo (bignum fixnum)
  (nth-value 1 (bignum-truncate-by-fixnum bignum fixnum)))

;;; This may do unnecessary computation in some cases.
(defun bignum-rem (x y)
  (nth-value 1 (bignum-truncate x y)))



;;;; General utilities.

(defun %zero-trailing-sign-digits (bignum len)
  (declare (fixnum len))
  (unless (<= len 1)
    (do ((next (bignum-ref bignum (the fixnum (- len 2)))
               (bignum-ref bignum (the fixnum (- len 2))))
         (sign (bignum-ref bignum (the fixnum (- len 1)))
               next))
        ((not (zerop (the fixnum (%logxor sign (%ashr next 31))))))
      (decf len)
      (setf (bignum-ref bignum len) 0)
      ;; Return, unless we've already done so (having found significant
      ;; digits earlier.)
      (when (= len 1)
        (return))))
  len)


(defun %normalize-bignum-2 (return-fixnum-p bignum)
  (let* ((len (%bignum-length bignum))
         (newlen (%zero-trailing-sign-digits bignum len)))
    (declare (fixnum len newlen))
    (unless (= len newlen)
      (%set-bignum-length newlen bignum))
    (or (and return-fixnum-p
             (%maybe-fixnum-from-one-or-two-digit-bignum bignum))
        bignum)))
           
    
;;; %MOSTLY-NORMALIZE-BIGNUM -- Internal.
;;;
;;; This drops the last digit if it is unnecessary sign information.  It
;;; repeats this as needed, possibly ending with a fixnum magnitude but never
;;; returning a fixnum.
;;;

(defun %mostly-normalize-bignum (res &optional len)
  (declare (ignore len))
  (%normalize-bignum-2 nil res))





(defun load-byte (size position integer)
  (if (and (bignump integer)
           (<= size (- 63 target::fixnumshift))
           (fixnump position))
    (%ldb-fixnum-from-bignum integer size position)
    (let ((mask (byte-mask size)))
      (if (and (fixnump mask) (fixnump integer)(fixnump position))
        (%ilogand mask (%iasr position integer))
        (logand mask (ash integer (- position)))))))


#+safe-but-slow
;;; This is basically the same algorithm as the "destructive"
;;; version below; while it may be more readable, it's often
;;; slower and conses too much to be at all viable.
(defun %bignum-bignum-gcd (u v)
  (setq u (abs u) v (abs v))
  (do* ((g 1 (ash g 1)))
       ((or (oddp u) (oddp v))
	(do* ()
	     ((zerop u) (* g v))
	  (cond ((evenp u) (setq u (ash u -1)))
		((evenp v) (setq v (ash v -1)))
		(t (let* ((temp (ash (abs (- u v)) -1)))
		     (if (< u v)
		       (setq v temp)
		       (setq u temp)))))))
    (setq u (ash u -1) v (ash v -1))))




#-safe-but-slow
(progn
(defun %positive-bignum-bignum-gcd (u0 v0)
  (let* ((u-len (%bignum-length u0))
	 (v-len (%bignum-length v0)))
    (declare (fixnum u-len v-len))
    (if (or (< u-len v-len)
	    (and (= u-len v-len)
		 (< (bignum-compare u0 v0) 0)))
      (psetq u0 v0 v0 u0 u-len v-len v-len u-len))
    (with-bignum-buffers ((u u-len)
			  (u2 u-len)
			  (v v-len)
			  (v2 v-len))
      (bignum-replace u u0)
      (bignum-replace v v0)
      (let* ((u-trailing-0-bits (%bignum-count-trailing-zero-bits u))
	     (u-trailing-0-digits (ash u-trailing-0-bits -5))
	     (v-trailing-0-bits (%bignum-count-trailing-zero-bits v))
	     (v-trailing-0-digits (ash v-trailing-0-bits -5)))
	(declare (fixnum u-trailing-0-bits v-trailing-0-bits))
	(unless (zerop u-trailing-0-bits)
	  (bignum-shift-right-loop-1
	   (logand u-trailing-0-bits 31)
	   u2
	   u
	   (the fixnum (1- (the fixnum (- u-len u-trailing-0-digits ))))
	   u-trailing-0-digits)
	  (rotatef u u2)
	  (%mostly-normalize-bignum-macro u)
	  (setq u-len (%bignum-length u)))
	(unless (zerop v-trailing-0-bits)
	  (bignum-shift-right-loop-1
	   (logand v-trailing-0-bits 31)
	   v2
	   v
	   (the fixnum (1- (the fixnum (- v-len v-trailing-0-digits))))
	   v-trailing-0-digits)
	  (rotatef v v2)
	  (%mostly-normalize-bignum-macro v)
	  (setq v-len (%bignum-length v)))
	(let* ((shift (min u-trailing-0-bits
			   v-trailing-0-bits)))
	  (loop
	      (let* ((fix-u (and (<= u-len 2)
                                 (%maybe-fixnum-from-one-or-two-digit-bignum u)))
		     (fix-v (and (<= v-len 2)
                                 (%maybe-fixnum-from-one-or-two-digit-bignum v))))
		(if fix-v
		  (if fix-u
		    (return (ash (%fixnum-gcd fix-u fix-v) shift))
		    (return (ash (bignum-fixnum-gcd u fix-v) shift)))
		  (if fix-u
		    (return (ash (bignum-fixnum-gcd v fix-u) shift)))))
	      (let* ((signum (if (> u-len v-len)
			       1
			       (if (< u-len v-len)
				 -1
				 (bignum-compare u v)))))
		(declare (fixnum signum))
		(case signum
		  (0			; (= u v)
		   (if (zerop shift)
		     (let* ((copy (%allocate-bignum u-len)))
		       (bignum-replace copy u)
		       (return copy))
		     (return (ash u shift))))
		  (1			; (> u v)
		   (bignum-subtract-loop u u-len v v-len u)
		   (%mostly-normalize-bignum-macro u)
		   (setq u-len (%bignum-length u))
		   (setq u-trailing-0-bits
			 (%bignum-count-trailing-zero-bits u)
			 u-trailing-0-digits
			 (ash u-trailing-0-bits -5))
                   (unless (zerop u-trailing-0-bits)
                     (%init-misc 0 u2)
                     (bignum-shift-right-loop-1
                      (logand u-trailing-0-bits 31)
                      u2
                      u
                      (the fixnum (1- (the fixnum (- u-len
                                                     u-trailing-0-digits))))
                      u-trailing-0-digits)
                     (rotatef u u2)
                     (%mostly-normalize-bignum-macro u)
                     (setq u-len (%bignum-length u))))
		  (t			; (> v u)
		   (bignum-subtract-loop v v-len u u-len v)
		   (%mostly-normalize-bignum-macro v)
		   (setq v-len (%bignum-length v))
		   (setq v-trailing-0-bits
			 (%bignum-count-trailing-zero-bits v)
			 v-trailing-0-digits
			 (ash v-trailing-0-bits -5))
                   (unless (zerop v-trailing-0-bits)
                     (%init-misc 0 v2)
                     (bignum-shift-right-loop-1
                      (logand v-trailing-0-bits 31)
                      v2
                      v
                      (the fixnum (1- (the fixnum (- v-len v-trailing-0-digits))))
                      v-trailing-0-digits)
                     (rotatef v v2)
                     (%mostly-normalize-bignum-macro v)
                     (setq v-len (%bignum-length v))))))))))))

(defun %bignum-bignum-gcd (u v)
  (with-negated-bignum-buffers u v %positive-bignum-bignum-gcd))
)


(defun bignum-shift-right-loop-1 (nbits result source len idx)
  (declare (type bignum-type result source)
           (type (mod 32) nbits)
           (type bignum-index idx len))
  (let* ((rbits (- 32 nbits)))
    (declare (type (mod 33) rbits))
    (dotimes (j len)
      (let* ((x (bignum-ref source idx)))
        (declare (type bignum-element-type x))
        (setq x (%ilsr nbits x))
        (incf idx)
        (let* ((y (bignum-ref source idx)))
          (declare (type bignum-element-type y))
          (setq y (%ashl y rbits))
          (setf (bignum-ref result j)
                (%logior x y)))))
    (setf (bignum-ref result len)
          (%ilsr nbits (bignum-ref source idx)))
    idx))
    

(defun %logcount (bignum idx)
  (%ilogcount (bignum-ref bignum idx)))

(defun %logcount-complement (bignum idx)
  (- 32 (the fixnum (%ilogcount (bignum-ref bignum idx)))))

(defun %bignum-evenp (bignum)
  (not (logbitp 0 (the fixnum (bignum-ref bignum 0)))))

(defun %bignum-oddp (bignum)
  (logbitp 0 (the fixnum (bignum-ref bignum 0))))

(defun %ldb-fixnum-from-bignum (bignum size position)
  (declare (fixnum size position))
  (let* ((low-idx (ash position -5))
         (low-bit (logand position 31))
         (hi-bit (+ low-bit size))
         (len (%bignum-length bignum))
         (minusp (bignum-minusp bignum)))
    (declare (fixnum size position low-bit hi-bit low-idx len))
    (if (>= low-idx len)
      (if minusp (1- (ash 1 size)) 0)
      (flet ((ldb32 (digit size pos)
               (declare (fixnum digit size pos))
               (logand (the fixnum (1- (ash 1 size)))
                       (the fixnum (ash digit (the fixnum (- pos)))))))
        (let* ((low-digit (bignum-ref bignum low-idx))
               (chunk-lo (ldb32 low-digit (min size (%i- 32 low-bit)) low-bit)))
          (if (< hi-bit 32) 
            chunk-lo
            (let* ((have (- 32 low-bit))
                   (remain (- size have)))
              (declare (fixnum have remain))
              (setq low-idx (1+ low-idx))
              (when (> remain 32)
                (setq chunk-lo
                      (logior (ash (if (< low-idx len)
                                     (bignum-ref bignum low-idx)
                                     (if minusp all-ones-digit 0))
                                   have)
                              chunk-lo))
                (incf have 32)
                (decf remain 32)
                (incf low-idx))
              (let* ((high-digit
                      (if (>= low-idx len)
                        (if minusp all-ones-digit 0)
                        (bignum-ref bignum low-idx)))
                     (chunk-hi (ldb32 high-digit remain 0)))
                (%ilogior (ash chunk-hi have) chunk-lo)))))))))



(defun bignum-negate-loop-really (big len res)
  (declare (fixnum len))
  (let* ((carry 1))
    (dotimes (i len carry)
      (multiple-value-bind (result-digit carry-out)
          (%add-with-carry (%lognot (bignum-ref big i)) 0 carry)
        (setf (bignum-ref res i) result-digit
              carry carry-out)))))

(defun bignum-negate-to-pointer (big len res)
  (declare (fixnum len))
  (let* ((carry 1))
    (do* ((i 0 (1+ i))
          (j 0 (+ j 4)))
         ((= i len) carry)
      (declare (fixnum i))
      (multiple-value-bind (result-digit carry-out)
          (%add-with-carry (%lognot (bignum-ref big i)) 0 carry)
        (setf (%get-unsigned-long res j) result-digit
              carry carry-out)))))
  

(defun %bignum-count-trailing-zero-bits (bignum)
  (let* ((count 0))
    (dotimes (i (%bignum-length bignum))
      (let* ((digit (bignum-ref bignum i)))
        (declare (type bignum-element-type digit))
        (if (zerop digit)
          (incf count 32)
          (progn
            (dotimes (bit 32)
              (declare (type (mod 32) bit))
              (if (logbitp bit digit)
                (return)
                (incf count)))
            (return)))))
    count))
                  

(defun one-bignum-factor-of-two (a)  
  (declare (type bignum-type a))
  (let ((len (%bignum-length a)))
    (declare (fixnum len))
    (dotimes (i len)
      (let* ((x (bignum-ref a i)))
        (declare (fixnum x))
        (unless (zerop x)
          (return (+ (ash i 5)
                     (dotimes (j 32)
                       (if (logbitp j x)
                         (return j))))))))))


(defun %bignum-random (number state)
  (let* ((ndigits (%bignum-length number))
         (sign-index (1- ndigits)))
    (declare (fixnum ndigits sign-index))
    (with-bignum-buffers ((bignum ndigits))
      (dotimes (i sign-index)
        (setf (bignum-ref bignum i) (%next-random-seed state)))
      (setf (bignum-ref bignum sign-index)
            (logand #x7fffffff (the (unsigned-byte 32)
                                 (%next-random-seed state))))
      (let* ((result (mod bignum number)))
        (if (eq result bignum)
          (copy-uvector bignum)
          result)))))



(defun logbitp (index integer)
  "Predicate returns T if bit index of integer is a 1."
  (number-case index
    (fixnum
     (if (minusp (the fixnum index))(report-bad-arg index '(integer 0))))
    (bignum
     ;; assuming bignum cant have more than most-positive-fixnum bits
     ;; (2 expt 24 longs)
     (if (bignum-minusp index)(report-bad-arg index '(integer 0)))
     ;; should error if integer isn't
     (return-from logbitp (minusp (require-type integer 'integer)))))
  (number-case integer
    (fixnum
     (if (%i< index (- target::nbits-in-word target::fixnumshift))
       (%ilogbitp index integer)
       (minusp (the fixnum integer))))
    (bignum
     (let ((bidx (%iasr 5 index))
           (bbit (%ilogand index 31)))
       (declare (fixnum bidx bbit))
       (if (>= bidx (%bignum-length integer))
         (bignum-minusp integer)
         (logbitp bbit (bignum-ref integer bidx)))))))

) ; #+64-bit-target
