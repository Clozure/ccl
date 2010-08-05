;;-*- Mode: Lisp; Package: CCL -*-
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

(in-package "CCL")


#+32-bit-target                         ; the whole shebang
(eval-when (:compile-toplevel :execute)
  (require "ARCH")
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO")
  
  (defconstant digit-size 32)
  (defconstant half-digit-size (/ digit-size 2))
  
  (defconstant maximum-bignum-length (1- (ash 1 24)))

  (deftype bignum-index () `(integer 0 (,maximum-bignum-length)))
  (deftype bignum-element-type () `(unsigned-byte ,digit-size))
  (deftype bignum-half-element-type () `(unsigned-byte ,half-digit-size))
  (deftype bignum-type () 'bignum)
  (defmacro %bignum-digits (bignum)`(uvsize ,bignum))

  (defmacro digit-bind ((&rest digits) form &body body)
    `(multiple-value-bind ,digits
                          ,form
       (declare (type bignum-half-element-type ,@digits))
       ,@body))

  (defmacro digit-set ((&rest digits) form)
    `(multiple-value-setq ,digits
                          ,form))

  (defmacro digit-zerop (h l)
    `(and (zerop ,h) (zerop ,l)))
 


  ;;;; BIGNUM-REPLACE and WITH-BIGNUM-BUFFERS.

  ;;; BIGNUM-REPLACE -- Internal.
  ;;;
  (defmacro bignum-replace (dest src &key (start1 '0) end1 (start2 '0) end2
                                 from-end)
    (once-only ((n-dest dest)
		 (n-src src))
      (if (and (eq start1 0)(eq start2 0)(null end1)(null end2)(null from-end))
        ; this is all true for some uses today <<
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
  

  

  
  (defmacro %logxor (h1 l1 h2 l2)
    (once-only ((h1v h1)(l1v l1)(h2v h2)(l2v l2))
      `(values (%ilogxor ,h1v ,h2v)(%ilogxor ,l1v ,l2v))))
  
  
  (defmacro %lognot (h l)
    (once-only ((h1v h)(l1v l))
      `(values (%ilognot ,h1v)(%ilognot ,l1v))))

  (defmacro %allocate-bignum (ndigits)
    `(%alloc-misc ,ndigits target::subtag-bignum))

  (defmacro %normalize-bignum-macro (big)
    `(%normalize-bignum-2 t ,big))

  (defmacro %mostly-normalize-bignum-macro (big)
    `(%normalize-bignum-2 nil ,big))


;;; %ALLOCATE-BIGNUM must zero all elements.
;;;
  (declaim (inline  %bignum-length))

;;; Temp space needed to (Karatsuba)-square N-digit argument
  (defmacro mpn-kara-mul-n-tsize (n)
    `(* 8 (+ ,n 32)))
;;; Need the same amount of space to do Karatsuba multiply.
  (defmacro mpn-kara-sqr-n-tsize (n)
    `(mpn-kara-mul-n-tsize ,n))
  
)




#+32-bit-target
(progn
;;; Extract the length of the bignum.
;;; 
(defun %bignum-length (bignum)
  (uvsize bignum)) 





;;;; Addition.
(defun add-bignums (a b)
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b)))
    (declare (bignum-index len-a len-b))
    (when (> len-b len-a)
      (rotatef a b)
      (rotatef len-a len-b))
    (let* ((len-res (1+ len-a))
	   (res (%allocate-bignum len-res))
	   (carry 0)
	   (sign-b (%bignum-sign b)))
	(dotimes (i len-b)
	  (setq carry (%add-with-carry res i carry a i b i)))
	(if (/= len-a len-b)
	  (finish-bignum-add  res carry a sign-b len-b len-a)
	  (%add-with-carry res len-a carry (%bignum-sign a) nil sign-b nil))
	(%normalize-bignum-macro res))))

;;; Could do better than this, surely.
(defun add-bignum-and-fixnum (bignum fixnum)
  (with-small-bignum-buffers ((bigfix fixnum))
    (add-bignums bignum bigfix)))



;;; B was shorter than A; keep adding B's sign digit to each remaining
;;; digit of A, propagating the carry.
(defun finish-bignum-add (result carry a sign-b start end)
  (declare (type bignum-index start end))
  (do* ((i start (1+ i)))
       ((= i end)
	(%add-with-carry result end carry (%bignum-sign a) nil sign-b nil))
    (setq carry (%add-with-carry result i carry a i sign-b nil))))






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
  (declare (bignum-index len-a len-b ))
  (let* ((len-res (%bignum-length res)))
    (declare (bignum-index len-res))
    (let* ((borrow 1)
	   (sign-a (%bignum-sign a))
	   (sign-b (%bignum-sign b)))
      (dotimes (i (the bignum-index (min len-a len-b)))
	(setq borrow (%subtract-with-borrow res i borrow a i b i)))
      (if (< len-a len-b)
	(do* ((i len-a (1+ i)))
	     ((= i len-b)
	      (if (< i len-res)
		(%subtract-with-borrow res i borrow sign-a nil sign-b nil)))
	  (setq borrow (%subtract-with-borrow res i borrow sign-a nil b i)))
	(do* ((i len-b (1+ i)))
	     ((= i len-a)
	      (if (< i len-res)
		(%subtract-with-borrow res i borrow sign-a nil sign-b nil)))
	  (setq borrow (%subtract-with-borrow res i borrow a i sign-b nil)))))))


;;;; Multiplication.

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
	  (setq carry
                (+ carry
                   (the fixnum (mpn-add-n (the fixnum (1+ prodp))
                                          (the fixnum (1+ prodp))
                                          tp
                                          2n-2))))
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

(defun multiply-bignums (a b)
  (let* ((signs-differ (not (eq (bignum-minusp a) (bignum-minusp b)))))
    (flet ((multiply-unsigned-bignums (a b)
	     (let* ((len-a (%bignum-length a))
		    (len-b (%bignum-length b))
		    (len-res (+ len-a len-b))
		    (res (%allocate-bignum len-res)) )
	       (declare (bignum-index len-a len-b len-res))
	       (if (and (>= len-a 16)
			(>= len-b 16)
			#+(or x8632-target arm-target)
			nil)
		 (let* ((ubytes (* len-a 4))
			(vbytes (* len-b 4))
			(rbytes (* len-res 4)))
		   (declare (fixnum ubytes vbytes rbytes))
		   (%stack-block ((uptr ubytes)
				  (vptr vbytes)
				  (rptr rbytes))
		     (let* ((up (macptr->fixnum uptr))
			    (vp (macptr->fixnum vptr))
			    (rp (macptr->fixnum rptr)))
		       (declare (fixnum up vp rp))
		       (%copy-ivector-to-ptr a 0 uptr 0 ubytes)
		       (if (eq a b)	; maybe try eql ..
			 (mpn-mul rp up len-a up len-a)
			 (progn
			   (%copy-ivector-to-ptr b 0 vptr 0 vbytes)
			   (if (< len-a len-b)
			     (mpn-mul rp vp len-b up len-a)
			     (mpn-mul rp up len-a vp len-b)))))
		     (%copy-ptr-to-ivector rptr 0 res 0 rbytes)))
		 (dotimes (i len-a)
		   (declare (type bignum-index i))
		   (%multiply-and-add-harder-loop-2 a b res i len-b)))
		 res)))
      (let* ((res (with-negated-bignum-buffers a b multiply-unsigned-bignums)))
	(if signs-differ (negate-bignum-in-place res))
	(%normalize-bignum-macro res)))))


(defun multiply-bignum-and-fixnum (bignum fixnum)
  (declare (type bignum-type bignum) (fixnum fixnum))
  (let* ((bignum-len (%bignum-length bignum))
         (bignum-plus-p (bignum-plusp bignum))
	 (fixnum-plus-p (not (minusp fixnum)))
         (negate-res (neq bignum-plus-p fixnum-plus-p)))
    (declare (type bignum-type bignum)
	     (type bignum-index bignum-len))
    (flet ((do-it (bignum fixnum  negate-res)
             (let* ((bignum-len (%bignum-length bignum))
                    (result (%allocate-bignum (the fixnum (1+ bignum-len)))))
               (declare (type bignum-type bignum)
	                (type bignum-index bignum-len))
	       (with-small-bignum-buffers ((carry-digit)
					   (result-digit))
		 (dotimes (i bignum-len (%set-digit result bignum-len carry-digit))
		   (%set-digit result i
			       (%multiply-and-add result-digit carry-digit bignum i fixnum))))
               (when negate-res
                 (negate-bignum-in-place result))
               (%normalize-bignum-macro result ))))
      (declare (dynamic-extent #'do-it))
      (if bignum-plus-p
        (do-it bignum (if fixnum-plus-p fixnum (- fixnum))  negate-res)
        (with-bignum-buffers ((b1 (the fixnum (1+ bignum-len))))
          (negate-bignum bignum nil b1)
          (do-it b1 (if fixnum-plus-p fixnum (- fixnum))  negate-res))))))

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
         (minusp (bignum-minusp x)))
    (declare (type bignum-index len-x len-res))
    (if (not res) (setq res (%allocate-bignum len-res))) ;Test len-res for range?
    (let ((carry (bignum-negate-loop-really x len-x res)))  ; i think carry is always 0
      (if (eq carry 0)
        (if minusp (%bignum-set res len-x 0 0)(%bignum-set res len-x #xffff #xffff))
        (digit-bind (h l)
                    (if minusp 
                      (%add-the-carry 0 0 carry)
                      (%add-the-carry #xffff #xffff carry))
                    
          (%bignum-set res len-x h l))))
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
           (fixnum x))
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
        (let* ((res-len (- bignum-len digits))
               (res (%allocate-bignum res-len))
               (len-1 (1- res-len)))
          (declare (fixnum res-len len-1))
          (bignum-shift-right-loop-1 n-bits res bignum len-1 digits)          
          (%normalize-bignum-macro res )))))))

			       



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
#|
(defun bignum-buffer-ashift-right (bignum bignum-len x)
  (declare (type bignum-index bignum-len) (fixnum x))
  (multiple-value-bind (digits n-bits)
		       (truncate x digit-size)
    (declare (type bignum-index digits))
    (cond
     ((zerop n-bits)
      (let ((new-end (- bignum-len digits)))
	(bignum-replace bignum bignum :end1 new-end :start2 digits
			:end2 bignum-len)
	(%normalize-bignum-buffer bignum new-end)))
     (t
      (shift-right-unaligned bignum digits n-bits (- bignum-len digits)
			     ((= j res-len-1)
                              (digit-bind (h l) (%bignum-ref bignum i)
                                (digit-set (h l) (%ashr h l n-bits))
			        (%bignum-set bignum j h l))
			      (%normalize-bignum-buffer bignum res-len)))))))
|#
#|
(defun bignum-buffer-ashift-right (bignum bignum-len x)
  (declare (type bignum-index bignum-len) (fixnum x))
  (multiple-value-bind (digits n-bits) (truncate x digit-size)
    (declare (type bignum-index digits)(fixnum n-bits))
    (macrolet ((clear-high-digits ()
                 `(do* ((i (1- (the fixnum (%bignum-length bignum))) (1- i))
                        (j digits (1- j)))
                       ((= 0 j))
                    (declare (fixnum i j))
                    (%bignum-set bignum i 0 0))))
      (cond
       ((zerop n-bits)
        (let* ((new-end (- bignum-len digits)))
          (declare (fixnum new-end))
          (bignum-replace bignum bignum :end1 new-end :start2 digits
                          :end2 bignum-len)
          (clear-high-digits)
          (%normalize-bignum-buffer bignum new-end)))
       (t
        (let* ((res-len (- bignum-len digits))
               (len-1 (1- res-len)))
          (declare (fixnum res-len len-1))
          (bignum-shift-right-loop-1 n-bits bignum bignum len-1 digits)
          ; clear the old high order digits - assume always positive
          ; (when (neq 0 digits)(push digits poof))
          (clear-high-digits)
          (%normalize-bignum-buffer bignum res-len)))))))
|#

 

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
  (let* (;(remaining-bits (- digit-size n-bits))
         (res-len-1 (1- res-len))
         (res (or res (%allocate-bignum res-len))))
    (declare (type bignum-index res-len res-len-1))
    (bignum-shift-left-loop n-bits res bignum res-len-1 (the fixnum (1+ digits)))
    ; if resp provided we don't care about returned value
    (if (not resp) (%normalize-bignum-macro res))))




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
      (let ((val (bignum-logtest-loop (min length1 length2) num1 num2)))
                 #|(do* ((index 0 (1+ index)))
	              ((= index (min length1 length2)) nil)
                   ; maybe better to start from high end of shorter?
                   (multiple-value-bind (hi1 lo1)(%bignum-ref num1 index)
                     (multiple-value-bind (hi2 lo2)(%bignum-ref num2 index)
                       (when (or (not (zerop (%ilogand hi1 hi2)))
                                 (not (zerop (%ilogand lo1 lo2))))
                         (return t)))))))|#
        (or val
            (when (not (eql length1 length2)) ; lengths same => value nil
              (if (< length1 length2)
                n1-minusp
                n2-minusp)))))))



(defun logtest-fix-big (fix big)
  (declare (fixnum fix))
  (if (eql 0 (the fixnum fix))
    nil
    (if (> (the fixnum fix) 0) 
      (let ()
        (multiple-value-bind (hi lo)(%bignum-ref big 0)
          (declare (fixnum hi lo))
          (or (not (zerop (logand fix lo)))
              (not (zerop (logand (ash fix (- 16)) hi))))))
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
      (%bignum-lognot i a res))))




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
    (%bignum-logand i a b res))
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
    (%bignum-logand i a b res))
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
    (%bignum-logandc2 i a b res))
  (if (bignum-minusp a)
    (do ((i len-a (1+ i)))
          ((= i len-b))
        (declare (type bignum-index i))
        (digit-bind (h l) (%bignum-ref b i)
          (%bignum-set res i (%ilognot h) (%ilognot l)))))
  (%normalize-bignum-macro res))



(defun logandc1-shorter-any (a len-a b len-b res)
  (declare (type bignum-type a b res)
           (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (%bignum-logandc1 i a b res))
  (if (bignum-plusp a)
    (if (neq len-a len-b)
      (bignum-replace res b :start1 len-a :start2 len-a :end1 len-b :end2 len-b)))
  (%normalize-bignum-macro res))



(defun fix-big-logand (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (< fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logand fix big res)))
      (if res
        (progn
          (bignum-replace res big :start1 1 :start2 1 :end1 len-b :end2 len-b)
          (%normalize-bignum-macro res))
        val))))
  

(defun fix-big-logandc2 (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (< fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logandc2 fix big res)))
      (if res
        (progn
          (do ((i 1 (1+ i)))
              ((= i len-b))
            (declare (type bignum-index i))
            (digit-lognot-move i big res))
          (%normalize-bignum-macro res))
        val))))

(defun fix-big-logandc1 (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (>= fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logandc1 fix big res)))
      (if res
        (progn  
          (bignum-replace res big :start1 1 :start2 1 :end1 len-b :end2 len-b)
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
    (%bignum-logior i a b res))
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
    (%bignum-logior i a b res))
  ; silly to propagate sign and then normalize it away
  ; but may need to do at least once - but we are only normalizing from len-a?
  ; ah but the sign needs to be correct
  (do ((i len-a (1+ i)))
      ((= i len-b))
    (declare (type bignum-index i))
    (%bignum-set res i #xffff #xffff))
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
    (%bignum-logxor i a b res))
  (unless (= len-a len-b)
    (let ((sign (if (bignum-minusp a) #xffff 0)))
      (do ((i len-a (1+ i)))
          ((= i len-b))
        (declare (type bignum-index i))
        (digit-bind (h l) (%bignum-ref b i)
          (%bignum-set res i (%ilogxor sign h)(%ilogxor sign l))))))
  (%normalize-bignum-macro res))





;;;; LDB (load byte)

; [slh] 'twas all commented out - thank gawd


;;;; TRUNCATE.

;;; This is the original sketch of the algorithm from which I implemented this
;;; TRUNCATE, assuming both operands are bignums.  I should modify this to work
;;; with the documentation on my functions, as a general introduction.  I've
;;; left this here just in case someone needs it in the future.  Don't look
;;; at this unless reading the functions' comments leaves you at a loss.
;;; Remember this comes from Knuth, so the book might give you the right general
;;; overview.
;;; 
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
;;; if x[i] = y[j] then g = #xFFFFFFFF
;;; else g = x[i]x[i-1]/y[j].
;;;
;;; check guess.
;;; %UNSIGNED-MULTIPLY returns b and c defined below.
;;;    a = x[i-1] - (logand (* g y[j]) #xFFFFFFFF).
;;;       Use %UNSIGNED-MULTIPLY taking low-order result.
;;;    b = (logand (ash (* g y[j-1]) -32) #xFFFFFFFF).
;;;    c = (logand (* g y[j-1]) #xFFFFFFFF).
;;; if a < b, okay.
;;; if a > b, guess is too high
;;;    g = g - 1; go back to "check guess".
;;; if a = b and c > x[i-2], guess is too high
;;;    g = g - 1; go back to "check guess".
;;; GUESS IS 32-BIT NUMBER, SO USE THING TO KEEP IN SPECIAL REGISTER
;;; SAME FOR A, B, AND C.
;;;
;;; Subtract g * y from x[i - len-y+1]..x[i].  See paper for doing this in step.
;;; If x[i] < 0, guess is fucked.
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
;;; Normalize quotient and remainder.  Cons result if necessary.
;;;




;;; BIGNUM-TRUNCATE -- Public.
;;;
;;; This divides x by y returning the quotient and remainder.  In the general
;;; case, we shift y to setup for the algorithm, and we use two buffers to save
;;; consing intermediate values.  X gets destructively modified to become the
;;; remainder, and we have to shift it to account for the initial Y shift.
;;; After we multiple bind q and r, we first fix up the signs and then return
;;; the normalized results.
;;;


(defun bignum-truncate (x1 y1 &optional no-rem)
  (declare (type bignum-type x1 y1))
  (let* ((x-plusp (bignum-plusp x1))
	 (y-plusp (bignum-plusp y1)))
    (flet 
      ((do-it (x y) 
         (let* ((len-x (%bignum-length x))
                (len-y (%bignum-length y)))
           (declare (fixnum len-x len-y))
           
           (let ((c (bignum-compare y x)))
             (cond 
              ((eql c 1)  ; >
               (return-from bignum-truncate (values 0 x1)))
              ((eql c 0)(values 1 0))  ; =  might as well since did compare anyway
              ((< len-y 2)
               (multiple-value-bind (q r)
                                    (bignum-truncate-single-digit x len-x y no-rem)
                 (values q
                         (unless no-rem
                           (cond (x-plusp r)
                                 ((typep r 'fixnum) (the fixnum (- (the fixnum r))))
                                 (t (negate-bignum-in-place r)
                                    (%normalize-bignum-macro r )))))))
              (t
               (let* ((len-x+1 (1+ len-x)))
                 (declare (fixnum len-x+1))
                 (with-bignum-buffers ((truncate-x len-x+1)
                                       (truncate-y (the fixnum (1+ len-y))))
                   (let ((y-shift (shift-y-for-truncate y)))
                     (shift-and-store-truncate-buffers truncate-x truncate-y x len-x y len-y y-shift)
                     (values (do-truncate truncate-x truncate-y len-x+1 len-y)
                             ;; DO-TRUNCATE must execute first.
                             (when (not no-rem)                               
                               (when (not (eql 0 y-shift))                                  
                                 (let* ((res-len-1 (1- len-y)))
                                   (declare (fixnum res-len-1))
                                   (bignum-shift-right-loop-1 y-shift truncate-x truncate-x res-len-1 0)))                                
                               (let ((the-res (%normalize-bignum-macro truncate-x )))
                                 (if (not (fixnump the-res))
                                   (if x-plusp (copy-bignum the-res) (negate-bignum the-res))
                                   (if x-plusp the-res (the fixnum (- (the fixnum the-res)))))
                                     ))))))))))))
      (multiple-value-bind (q r)(with-negated-bignum-buffers x1 y1 do-it)
        (let ((quotient (cond ((eq x-plusp y-plusp) q)
                              ((typep q 'fixnum) (the fixnum (- (the fixnum q))))
                              (t (negate-bignum-in-place q)
                                 (%normalize-bignum-macro q )))))
          (if no-rem
            quotient            
            (values quotient r)))))))

(defun bignum-rem (x1 y1)
  (declare (type bignum-type x1 y1))  
  (let* ((x-plusp (bignum-plusp x1)))
    (flet 
      ((do-it (x y) 
         (let* ((len-x (%bignum-length x))
                (len-y (%bignum-length y)))
           (declare (fixnum len-x len-y))           
           (let ((c (bignum-compare y x)))
             (cond 
              ((eql c 1) (return-from bignum-rem x1))
              ((eql c 0) 0)  ; =  might as well since did compare anyway
              ((< len-y 2)
               (let ((r (bignum-truncate-single-digit-no-quo x len-x y)))  ; phooey 
                 (cond (x-plusp r)
                       ((typep r 'fixnum) (the fixnum (- (the fixnum r))))
                       (t (negate-bignum-in-place r)
                          (%normalize-bignum-macro r )))))
              (t
               (let* ((len-x+1 (1+ len-x)))
                 (declare (fixnum len-x+1))
                 (with-bignum-buffers ((truncate-x len-x+1)
                                       (truncate-y (the fixnum (1+ len-y))))
                   (let ((y-shift (shift-y-for-truncate y)))
                     (shift-and-store-truncate-buffers truncate-x truncate-y x len-x y len-y y-shift)
                     (do-truncate-no-quo truncate-x truncate-y len-x+1 len-y)
                     (when (not (eql 0 y-shift))                                 
                       (let* ((res-len-1 (1- len-y)))
                         (declare (fixnum res-len-1))
                         (bignum-shift-right-loop-1 y-shift truncate-x truncate-x res-len-1 0)))
                     (let ((the-res (%normalize-bignum-macro truncate-x)))
                       (if (not (fixnump the-res))
                         (if x-plusp (copy-bignum the-res) (negate-bignum the-res))
                         (if x-plusp the-res (the fixnum (- (the fixnum the-res)))))))))))))))
      (declare (dynamic-extent #'do-it))
      (with-negated-bignum-buffers x1 y1 do-it))))



;;; BIGNUM-TRUNCATE-SINGLE-DIGIT -- Internal.
;;;
;;; This divides x by y when y is a single bignum digit.  BIGNUM-TRUNCATE fixes
;;; up the quotient and remainder with respect to sign and normalization.
;;;
;;; We don't have to worry about shifting y to make its most significant digit
;;; sufficiently large for %FLOOR to return 32-bit quantities for the q-digit
;;; and r-digit.  If y is a single digit bignum, it is already large enough
;;; for %FLOOR.  That is, it has some bits on pretty high in the digit.
;;;
;;; x is positive
(defun bignum-truncate-single-digit (x len-x y &optional no-rem)
  (declare (type bignum-index len-x))
  (let* ((maybe-q (%allocate-bignum 2))
         (q (if (<= len-x 2) maybe-q (%allocate-bignum len-x)))
	 (r-h 0)
         (r-l 0))
    (declare (dynamic-extent maybe-q))
    (digit-bind (y-h y-l) (%bignum-ref y 0)
      (multiple-value-setq (r-h r-l)(%floor-loop-quo x q y-h y-l))      
      (if (eq q maybe-q)
        (progn 
          (setq q (%normalize-bignum-macro q))
          (if (not (fixnump q)) (setq q (copy-bignum q))))
        (setq q (%normalize-bignum-macro q )))
      ;; might as well make a fixnum if possible
      (if no-rem
        q
        (if (> (%digits-sign-bits r-h r-l)  target::fixnumshift)
          (values q (%ilogior (%ilsl 16 r-h) r-l))
          (let ((rem (%allocate-bignum 1)))
            (%bignum-set rem 0 r-h r-l)
            (values q rem)))))))

;;; aka rem
(defun bignum-truncate-single-digit-no-quo (x len-x y)
  (declare (type bignum-index len-x))
  (declare (ignore len-x))
  (let (;(q (%allocate-bignum len-x))
	(r-h 0)
        (r-l 0))
    (progn
      (digit-bind (y-h y-l) (%bignum-ref y 0)
        (multiple-value-setq (r-h r-l)(%floor-loop-no-quo x y-h y-l))
        ; might as well make a fixnum if possible
        (if (> (%digits-sign-bits r-h r-l)  target::fixnumshift)
          (%ilogior (%ilsl 16 r-h) r-l)
          (let ((rem (%allocate-bignum 1)))
            (%bignum-set rem 0 r-h r-l)
            rem))))))

;; so big deal - we save a one digit bignum for y 
;; and bigger deal if x is negative - we copy or negate x, computing result destructively
;;  - thus avoiding making a negated x in addition to result
;; 
(defun bignum-truncate-by-fixnum (x y)
  (declare (fixnum y))
  (when (eql y 0)(error (make-condition 'division-by-zero :operation 'truncate :operands (list x y))))
  (let* ((len-x (%bignum-length x))
         (x-minus (bignum-minusp x))
         (maybe-q (%allocate-bignum 3))
         (q (if x-minus
              (if (<= len-x 2)
                (dotimes (i 3 (negate-bignum-in-place maybe-q))
                  (if (< i len-x)
                    (multiple-value-bind (hi lo) (%bignum-ref x i)
                      (%bignum-set maybe-q i hi lo))
                    (%bignum-set maybe-q i 65535 65535)))
                (negate-bignum x))
              (if (<= len-x 2) ; this was broken if negative because bignum-replace just copies min len-a len-b digits
                (progn
                  (bignum-replace maybe-q x)                
                  maybe-q)
                (%allocate-bignum len-x))))      ;  q is new big or -x
         ;(len-q (%bignum-length q))
         (y-minus (minusp y))         
         (y (if y-minus (- y) y)))
    (declare (fixnum y))
    (declare (type bignum-index len-x))
    (declare (dynamic-extent maybe-q))
    (let* ((r-h 0)
           (r-l 0)
           (y-h (%ilogand #xffff (%iasr 16 y)))
           (y-l (%ilogand #xffff y)))
      (multiple-value-setq (r-h r-l)(%floor-loop-quo (if x-minus q x) q y-h y-l))      
      (let* ((r (%ilogior (%ilsl 16 r-h) r-l)))
        (declare (fixnum r))
        (when (neq x-minus y-minus)(negate-bignum-in-place q))
        (setq q (%normalize-bignum-macro q ))
        (values (if (eq q maybe-q) (copy-bignum q) q)
                (if x-minus (the fixnum (- r)) r))))))

(defun bignum-truncate-by-fixnum-no-quo (x y)
  (declare (fixnum y))
  (when (eql y 0)(error (make-condition 'division-by-zero :operation 'truncate :operands (list x Y))))
  (let* ((len-x (%bignum-length x))
         (x-minus (bignum-minusp x))
         (y-minus (minusp y))         
         (y (if y-minus (- y) y)))
    (declare (fixnum y))
    (declare (type bignum-index len-x))
      (let* (;(LEN-Q (%BIGNUM-LENGTH Q))
             (r-h 0)
             (r-l 0)
             (y-h (%ilogand #xffff (%iasr 16 y)))
             (y-l (%ilogand #xffff y)))
        (if x-minus
          (with-bignum-buffers ((q (the fixnum (1+ len-x))))
            (negate-bignum x nil q)
            (multiple-value-setq (r-h r-l)(%floor-loop-no-quo q y-h y-l)))
          (multiple-value-setq (r-h r-l)(%floor-loop-no-quo x y-h y-l)))        
        (let* ((r (%ilogior (%ilsl 16 r-h) r-l)))
          (declare (fixnum r))
          (if x-minus (the fixnum (- r)) r)))))


;;; DO-TRUNCATE -- Internal.
;;;
;;; This divides *truncate-x* by *truncate-y*, and len-x and len-y tell us how
;;; much of the buffers we care about.  TRY-BIGNUM-TRUNCATE-GUESS modifies
;;; *truncate-x* on each interation, and this buffer becomes our remainder.
;;;
;;; *truncate-x* definitely has at least three digits, and it has one more than
;;; *truncate-y*.  This keeps i, i-1, i-2, and low-x-digit happy.  Thanks to
;;; SHIFT-AND-STORE-TRUNCATE-BUFFERS.
;;;


(defun do-truncate (truncate-x truncate-y len-x len-y)
  (declare (type bignum-index len-x len-y))
  (let* ((len-q (- len-x len-y))
	 ;; Add one for extra sign digit in case high bit is on.
         (len-res (1+ len-q))
         (maybe-q (%allocate-bignum 2))         
	 (q (if (<= len-res 2) maybe-q (%allocate-bignum len-res)))
	 (k (1- len-q))
	 (i (1- len-x))
	 (low-x-digit (- i len-y)))
    (declare (type bignum-index len-q len-res k i  low-x-digit))
    (declare (dynamic-extent maybe-q))
    (loop
      (digit-bind (h l)
                  (digit-bind (guess-h guess-l)
                              (bignum-truncate-guess-2 truncate-x i truncate-y (the fixnum (1- len-y)))                                  
                    (try-bignum-truncate-guess truncate-x truncate-y guess-h guess-l len-y low-x-digit))
        (%bignum-set q k h l))
      (cond ((zerop k) (return))
            (t (decf k)
               (decf low-x-digit)
               (setq i (1- i)))))
    (if (eq q maybe-q)
      (progn 
        (setq q (%normalize-bignum-macro q))
        (if (fixnump q) q (copy-bignum q)))
      (%normalize-bignum-macro q))))

(defun do-truncate-no-quo (truncate-x truncate-y len-x len-y)
  (declare (type bignum-index len-x len-y))
  (let* ((len-q (- len-x len-y))
	 (k (1- len-q))
	 (i (1- len-x))
	 (low-x-digit (- i len-y)))
    (declare (type bignum-index len-q k i  low-x-digit))
    (loop
      (digit-bind (guess-h guess-l) (bignum-truncate-guess-2 truncate-x i truncate-y (the fixnum (1- len-y)))                                 
        (try-bignum-truncate-guess truncate-x truncate-y guess-h guess-l len-y low-x-digit)
        (cond ((zerop k) (return))
              (t (decf k)
                 (decf low-x-digit)
                 (setq i (1- i))))))
    nil))

;;; TRY-BIGNUM-TRUNCATE-GUESS -- Internal.
;;;
;;; This takes a digit guess, multiplies it by *truncate-y* for a result one
;;; greater in length than len-y, and subtracts this result from *truncate-x*.
;;; Low-x-digit is the first digit of x to start the subtraction, and we know x
;;; is long enough to subtract a len-y plus one length bignum from it.  Next we
;;; check the result of the subtraction, and if the high digit in x became
;;; negative, then our guess was one too big.  In this case, return one less
;;; than guess passed in, and add one value of y back into x to account for
;;; subtracting one too many.  Knuth shows that the guess is wrong on the order
;;; of 3/b, where b is the base (2 to the digit-size power) -- pretty rarely.
;;;

(defun try-bignum-truncate-guess (truncate-x truncate-y guess-h guess-l len-y low-x-digit)
  (declare (type bignum-index low-x-digit len-y))

  (let ((carry-digit-h 0)
        (carry-digit-l 0)
	(borrow 1)
	(i low-x-digit))
    (declare (type bignum-index i)
	     (fixnum borrow carry-digit-h carry-digit-l))
    ;; Multiply guess and divisor, subtracting from dividend simultaneously.
    (dotimes (j len-y)
      (multiple-value-bind (y-h y-l) (%bignum-ref truncate-y j)
	(multiple-value-bind (high-h high-l low-h low-l)
	    (%multiply-and-add-1 guess-h
			       guess-l
			       y-h
			       y-l
			       carry-digit-h
			       carry-digit-l)
	  (setq carry-digit-h high-h
		carry-digit-l high-l)
	  (multiple-value-bind (tx-h tx-l) (%bignum-ref truncate-x i)
	    (multiple-value-bind (x-h x-l temp-borrow)
		(%subtract-with-borrow-1 tx-h tx-l low-h low-l borrow)
	      (%bignum-set truncate-x i x-h x-l)
	      (setq borrow temp-borrow)))))
      (incf i))
    (multiple-value-bind (tx-h tx-l) (%bignum-ref truncate-x i)
      (multiple-value-bind (x-h x-l)
	  (%subtract-with-borrow-1 tx-h tx-l carry-digit-h carry-digit-l borrow)
	(%bignum-set truncate-x i x-h x-l)))
    ;; See if guess is off by one, adding one Y back in if necessary.


    (cond ((%digit-0-or-plusp truncate-x i)
	   (values guess-h guess-l))
	  (t
	   ;; If subtraction has negative result, add one divisor value back
	   ;; in.  The guess was one too large in magnitude.
           ;; hmm - happens about 1.6% of the time
           (bignum-add-loop-+ low-x-digit truncate-x truncate-y len-y)
           (%subtract-one guess-h guess-l)
	   ;(%subtract-with-borrow guess-h guess-l 0 1 1)
           ))))



;;; BIGNUM-TRUNCATE-GUESS -- Internal.
;;;
;;; This returns a guess for the next division step.  Y1 is the highest y
;;; digit, and y2 is the second to highest y digit.  The x... variables are
;;; the three highest x digits for the next division step.
;;;
;;; From Knuth, our guess is either all ones or x-i and x-i-1 divided by y1,
;;; depending on whether x-i and y1 are the same.  We test this guess by
;;; determining whether guess*y2 is greater than the three high digits of x
;;; minus guess*y1 shifted left one digit:
;;;    ------------------------------
;;;   |    x-i    |   x-i-1  | x-i-2 |
;;;    ------------------------------
;;;    ------------------------------
;;; - | g*y1 high | g*y1 low |   0   |
;;;    ------------------------------
;;;                ...                   <   guess*y2     ???
;;; If guess*y2 is greater, then we decrement our guess by one and try again.
;;; This returns a guess that is either correct or one too large.
;;;
;;; the y's come from *truncate-y*, x's from *truncate-x*
;;; doing this in lap is not screamingly difficult - x's at i, i-1, i-2





(defun bignum-truncate-guess-2 (x xidx y yidx)
  (digit-bind (guess-h guess-l)
              (%floor-99 x xidx y yidx)
    (truncate-guess-loop guess-h guess-l x xidx y yidx)))



    

;;; SHIFT-Y-FOR-TRUNCATE -- Internal.
;;;
;;; This returns the amount to shift y to place a one in the second highest
;;; bit.  Y must be positive.  If the last digit of y is zero, then y has a
;;; one in the previous digit's sign bit, so we know it will take one less
;;; than digit-size to get a one where we want.  Otherwise, we count how many
;;; right shifts it takes to get zero; subtracting this value from digit-size
;;; tells us how many high zeros there are which is one more than the shift
;;; amount sought.
;;;
;;; Note: This is exactly the same as one less than the integer-length of the
;;; last digit subtracted from the digit-size.
;;; 
;;; We shift y to make it sufficiently large that doing the 64-bit by 32-bit
;;; %FLOOR calls ensures the quotient and remainder fit in 32-bits.
;;;
(defun shift-y-for-truncate (y)
  (the fixnum (1- (the fixnum (%bignum-sign-bits y)))))

;;; SHIFT-AND-STORE-TRUNCATE-BUFFERS -- Internal.
;;;
;;; Stores two bignums into the truncation bignum buffers, shifting them on the
;;; way in.  This assumes x and y are positive and at least two in length, and
;;; it assumes *truncate-x* and *truncate-y* are one digit longer than x and y.
;;;
(defun shift-and-store-truncate-buffers (truncate-x truncate-y x len-x y len-y shift)
  (declare (type bignum-index len-x len-y)
	   (type (integer 0 (#.digit-size)) shift))
  (cond ((eql 0 shift)
	 (bignum-replace truncate-x x :end1 len-x)
	 (bignum-replace truncate-y y :end1 len-y))
	(t
	 (bignum-ashift-left-unaligned x 0 shift (the fixnum (1+ len-x)) truncate-x)
	 (bignum-ashift-left-unaligned y 0 shift (the fixnum (1+ len-y)) truncate-y))))




;;;; General utilities.


;;; %NORMALIZE-BIGNUM-BUFFER -- Internal.
;;;
;;; Internal in-place operations use this to fixup remaining digits in the
;;; incoming data, such as in-place shifting.  This is basically the same as
;;; the first form in %NORMALIZE-BIGNUM, but we return the length of the buffer
;;; instead of shrinking the bignum.
;;;



    




;;; %NORMALIZE-BIGNUM -- Internal.
;;;
;;; This drops the last digit if it is unnecessary sign information.  It
;;; repeats this as needed, possibly ending with a fixnum.  If the resulting
;;; length from shrinking is one, see if our one word is a fixnum.  Shift the
;;; possible fixnum bits completely out of the word, and compare this with
;;; shifting the sign bit all the way through.  If the bits are all 1's or 0's
;;; in both words, then there are just sign bits between the fixnum bits and
;;; the sign bit.  If we do have a fixnum, shift it over for the two low-tag
;;; bits.
;;;

(defun %normalize-bignum (res)
  ;(declare (optimize (speed 3)(safety 0)))
  (%normalize-bignum-2 t res))

;;; %MOSTLY-NORMALIZE-BIGNUM -- Internal.
;;;
;;; This drops the last digit if it is unnecessary sign information.  It
;;; repeats this as needed, possibly ending with a fixnum magnitude but never
;;; returning a fixnum.
;;;

(defun %mostly-normalize-bignum (res &optional len)
  (declare (ignore len))
  (%normalize-bignum-2 nil res))




; think its ok
(defun ldb32 (hi-data lo-data size pos)
  (declare (fixnum hi-data lo-data size pos))
  (let* ((hi-bit (+ pos size))
         (mask (%i- (%ilsl size 1) 1)))
    (declare (fixnum hi-bit mask))    
    (%ilogand mask (if (< hi-bit 16)
                     (%iasr pos lo-data)
                     (if (>= pos 16)
                       (%ilsr (the fixnum (- pos 16)) hi-data)
                       (%ilogior 
                         (%iasr pos lo-data)
                         (%ilsl (the fixnum (- 16 pos)) hi-data)))))))





; this was wrong for negative bigs when byte includes or exceeds sign
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
      (multiple-value-bind (hi lo)(%bignum-ref bignum low-idx)
        (let ((chunk-lo (ldb32 hi lo (min size (%i- 32 low-bit)) low-bit)))
          (let ((val
                 (if (< hi-bit 32) 
                   chunk-lo
                   (progn
                     (setq low-idx (1+ low-idx))
                     (multiple-value-setq (hi lo)
                       (if (>= low-idx len)
                         (if minusp (values #xffff #xffff)(values 0 0))
                         (%bignum-ref bignum low-idx)))
                     (let ((chunk-hi (ldb32 hi lo (%i- size (%i- 32 low-bit)) 0)))
                       (%ilogior (ash chunk-hi (%i- 32 low-bit)) chunk-lo))))))
            val))))))

(defun load-byte (size position integer)
  (if (and (bignump integer)
           (<= size (- 31 target::fixnumshift))
           (fixnump position))
    (%ldb-fixnum-from-bignum integer size position)
    (let ((mask (byte-mask size)))
      (if (and (fixnump mask) (fixnump integer)(fixnump position))
        (%ilogand mask (%iasr position integer))
        (logand mask (ash integer (- position)))))))    


#+safe-but-slow
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
            (let* ((fix-u (and (= u-len 1)
                               (let* ((hi-u (%bignum-ref-hi u 0)))
                                 (declare (fixnum hi-u))
                                 (= hi-u (the fixnum
                                           (logand hi-u (ash target::target-most-positive-fixnum -16)))))
                               (uvref u 0)))
                   (fix-v (and (= v-len 1)
                               (let* ((hi-v (%bignum-ref-hi v 0)))
                                 (declare (fixnum hi-v))
                                 (= hi-v (the fixnum
                                           (logand hi-v (ash target::target-most-positive-fixnum -16)))))
                               (uvref v 0))))
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

(defun unsignedwide->integer (uwidep)
  (with-bignum-buffers ((b 3))
    (setf (uvref b 0) (%get-unsigned-long uwidep 4)
	  (uvref b 1) (%get-unsigned-long uwidep 0))
    (let* ((n (%normalize-bignum b)))
      (if (typep n 'bignum)
        (copy-bignum n)
        n))))

(defun one-bignum-factor-of-two (a)  
  (declare (type bignum-type a))
  (let ((len (%bignum-length a)))
    (declare (fixnum len))
    (dotimes (i len)
      (multiple-value-bind (a-h a-l) (%bignum-ref a i)
        (declare (fixnum a-h a-l))
        (unless (and (= a-h 0)(= a-l 0))
          (return (+ (%ilsl 5 i)
                     (let* ((j 0)
                            (a a-l))
                       (declare (fixnum a j))
                       (if (= a-l 0) (setq j 16 a a-h))
                       (dotimes (i 16)            
                         (if (oddp a)
                           (return (%i+ j i))
                           (setq a (%iasr 1 a))))))))))))

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
         (multiple-value-bind (hi lo) (%bignum-ref integer bidx)
           (declare (fixnum hi lo))
           (if (> bbit 15)
             (%ilogbitp (%i- bbit 16) hi)
             (%ilogbitp bbit lo))))))))

) ; #+32-bit-target
