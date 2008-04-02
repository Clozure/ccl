;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Mar  4 06:21:51 2005
;;;; Contains: CMUCL type prop failures (moved from misc.lsp)

(in-package :cl-test)

;;; All these are 'strange template failures'
;;; The comment before each is the NAME of the template in the backtrace
;;; These tests seem to all have (space 2) (speed 3)

; X86::FAST-LOGAND-C/FIXNUM=>FIXNUM
(deftest cmucl-type-prop.1
  (funcall
   (compile
    nil
    '(lambda (p1)
       (declare (optimize (speed 2) (safety 2) (debug 2) (space 3))
		(type (member 2 -4 -211907662 -27215198) p1))
       (logand (the (integer * 161212781) p1) 10600829)))
   -27215198)
  2129952)

; X86::FAST-LOGAND/SIGNED-UNSIGNED=>UNSIGNED
(deftest cmucl-type-prop.2
  (funcall
   (compile
    nil
    '(lambda (p1 p2)
       (declare (optimize (speed 2) (safety 1) (debug 3) (space 3))
		(type (integer 1619851121 1619868587) p1) (type (integer * 303689) p2))
       (logandc2 (the (integer -5359291650 1619851136) p1) (the unsigned-byte p2))))
   1619851124 300065)
  1619551060)

; X86::FAST-LOGIOR-C/FIXNUM=>FIXNUM
(deftest cmucl-type-prop.3
  (funcall
   (compile
    nil
    '(lambda (p1)
       (declare (optimize (speed 2) (safety 3) (debug 0) (space 3))
		(type (integer 59087 63964) p1))
       (logior p1 -65887623)))
   59967)
  -65869185)

; X86::FAST-LOGIOR/FIXNUM=>FIXNUM
(deftest cmucl-type-prop.4
  (funcall
   (compile
    nil
    '(lambda (p1 p2)
       (declare (optimize (speed 2) (safety 2) (debug 0) (space 3))
		(type (integer 3585942 72924743) p1) (type (integer -70689 *) p2))
       (logorc2 (the (integer * 8514860) p1) (the (integer 1 411) p2))))
   3586455 4)
  -1)

; X86::FAST-LOGAND-C/SIGNED=>SIGNED
(deftest cmucl-type-prop.5
  (funcall
   (compile
    nil
    '(lambda (p2)
       (declare (optimize (speed 2) (safety 1) (debug 2) (space 3))
		(type (integer -257 *) p2))
       (lognand 1020158769 (the (integer -5275217 2381998) p2))))
   2)
  -1)

; X86::FAST-LOGAND-C/SIGNED-UNSIGNED=>UNSIGNED
(deftest cmucl-type-prop.6
  (funcall
   (compile
    nil
    '(lambda (p1)
       (declare (optimize (speed 2) (safety 1) (debug 0) (space 3))
		(type (integer -96413017 -96297711) p1))
       (lognand p1 3472289945)))
   -96413016)
  -3393245321)

; X86::FAST-LOGAND/UNSIGNED-SIGNED=>UNSIGNED
(deftest cmucl-type-prop.7
  (funcall
   (compile
    nil
    '(lambda (p1 p2)
       (declare (optimize (speed 2) (safety 3) (debug 2) (space 3))
		(type (integer 438294 891242) p1)
		(type (member 16317 -15 -541332155 33554427) p2))
       (logand (the (integer -33116139 1759877902) p1) p2)))
   438295 16317)
  12309)

; X86::FAST-LOGIOR-C/SIGNED=>SIGNED
(deftest cmucl-type-prop.8
  (funcall
   (compile
    nil
    '(lambda (p1)
       (declare (optimize (speed 2) (safety 1) (debug 1) (space 3))
		(type (integer -728025757 -727856169) p1))
       (logorc1 (the (integer -734005577 -727855553) p1) -3311)))
   -727856176)
  -2241)

; X86::FAST-LOGXOR/FIXNUM=>FIXNUM
(deftest cmucl-type-prop.9
  (funcall
   (compile
    nil
    '(lambda (p1 p2)
       (declare (optimize (speed 2) (safety 3) (debug 3) (space 3))
		(type (integer * 1489068) p1) (type (integer -7455 *) p2))
       (logeqv (the (member 9543 -15 32766 -264472) p1)
	       (the (integer -524303 11182721) p2))))
   9543 -8)
  9536)

; X86::FAST-LOGXOR/SIGNED=>SIGNED
(deftest cmucl-type-prop.10
  (funcall
   (compile
    nil
    '(lambda (p1 p2)
       (declare (optimize (speed 2) (safety 1) (debug 3) (space 3))
		(type (integer -616605365 -616598658) p1) (type (eql 499113) p2))
       (logeqv (the real p1) p2)))
   -616604953 499113)
  617035953)

; X86::FAST-LOGXOR-C/FIXNUM=>FIXNUM
(deftest cmucl-type-prop.11
  (funcall
   (compile
    nil
    '(lambda (p1)
       (declare (optimize (speed 2) (safety 1) (debug 0) (space 3))
		(type (integer -112225610 *) p1))
       (logeqv (the (integer -2822315666 3) p1) 1679389)))
   1)
  -1679389)

; X86::FAST-LOGXOR-C/SIGNED=>SIGNED
(deftest cmucl-type-prop.12
  (funcall
   (compile
    nil
    '(lambda (p2)
       (declare (optimize (speed 2) (safety 3) (debug 0) (space 3))
		(type (integer -67 268435455) p2))
       (logeqv 1038360149 (the (integer -3605943309) p2))))
   -1)
  1038360149)

; X86::-/SINGLE-FLOAT
(deftest cmucl-type-prop.13
  (notnot
   (typep
    (funcall
     (compile
      nil
      '(lambda (p1)
	 (declare (optimize (speed 2) (safety 2) (debug 1) (space 3))
		  (type (eql 64848.973) p1))
	 (- (the (eql 64848.973f0) p1) -2808/1031)))
     64848.973f0)
    'single-float))
  t)

; X86::-/DOUBLE-FLOAT
(deftest cmucl-type-prop.14
  (notnot
   (typep
    (funcall
     (compile
      nil
      '(lambda (p2)
	 (declare (optimize (speed 2) (safety 1) (debug 1) (space 3))
		  (type (integer 9297 *) p2))
	 (- 54090.82691488265d0 (the (integer * 1263530808) p2))))
     9590)
    'double-float))
  t)

; X86::-/SINGLE-FLOAT
(deftest cmucl-type-prop.15
  (notnot
   (typep
    (funcall
     (compile
      nil
      '(lambda (p1)
	 (declare (optimize (speed 2) (safety 3) (debug 3) (space 3))
		  (type (eql 328536/53893) p1))
	 (- p1 59218.633f0)))
     328536/53893)
    'single-float))
  t)

; X86::FAST--/FIXNUM=>FIXNUM 
(deftest cmucl-type-prop.16
  (funcall
   (compile nil '(lambda (p2)
		   (declare (optimize (speed 2) (safety 2) (debug 3) (space 3))
			    (type (integer -605782 -28141) p2))
		   (- -61118 p2)))
   -28225)
  -32893)

; X86::FAST---C/FIXNUM=>FIXNUM
(deftest cmucl-type-prop.17
  (funcall
   (compile nil '(lambda (p1)
		   (declare (optimize (speed 2) (safety 1) (debug 1) (space 3))
			    (type (integer 5535202) p1))
		   (- (the (integer * 27858177) p1) 405)))
   5535436)
  5535031)

; X86::FAST--/SIGNED=>SIGNED
(deftest cmucl-type-prop.18
  (funcall
   (compile nil '(lambda (p2)
		   (declare (optimize (speed 2) (safety 2) (debug 2) (space 3))
			    (type (integer -1175231414 -3471291) p2))
		   (- -440 p2)))
   -3536832)
  3536392)

; X86::FAST-+-C/FIXNUM=>FIXNUM
(deftest cmucl-type-prop.19
  (funcall
   (compile nil '(lambda (p2)
		   (declare (optimize (speed 2) (safety 3) (debug 2) (space 3))
			    (type (integer -1015240116 5) p2))
		   (+ 491841 (the unsigned-byte p2))))
   0)
  491841)

; X86::+/DOUBLE-FLOAT
(deftest cmucl-type-prop.20
  (notnot (typep (funcall (compile nil '(lambda (p1)
					  (declare (optimize (speed 2) (safety 3) (debug 3) (space 3))
						   (type (rational -1255531/68466 4) p1))
					  (+ p1 41888.98682005542d0)))
			  -1255531/68466)
		 'double-float))
  t)

;  X86::+/SINGLE-FLOAT
(deftest cmucl-type-prop.21
  (notnot (typep (funcall (compile nil '(lambda (p1)
					  (declare (optimize (speed 2) (safety 2) (debug 1) (space 3))
						   (type (integer -284887911 *) p1))
					  (+ (the (integer -50006902 19512639861) p1) 68648.28f0)))
			  -16452463)
		 'single-float))
  t)

; X86::=0/DOUBLE-FLOAT
(deftest cmucl-type-prop.22
  (funcall (compile nil '(lambda (p1)
			   (declare (optimize (speed 2) (safety 3) (debug 1) (space 3))
				    (type (complex double-float) p1))
			   (= p1 -1590311/896933)))
	   #c(1.0d0 1.0d0))
  nil)

; X86::=/SINGLE-FLOAT
(deftest cmucl-type-prop.23
  (funcall (compile nil '(lambda (p2)
			   (declare (optimize (speed 2) (safety 2) (debug 1) (space 3))
				    (type (complex single-float) p2))
			   (= -976855 (the (eql #c(-57420.04 806984.0)) p2))))
	   #c(-57420.04f0 806984.0f0))
  nil)

; X86::FAST-EQL/FIXNUM
(deftest cmucl-type-prop.24
  (notnot
   (funcall (compile nil '(lambda (p1 p2)
			    (declare (optimize (speed 2) (safety 1) (debug 3) (space 3))
				     (type (integer -3705845 488458) p1) (type (integer * 869076010) p2))
			    (/= p1 (the (integer -69832764 470) p2))))
	    488456 465))
  t)

; X86::FAST-EQL-C/FIXNUM
(deftest cmucl-type-prop.25
  (notnot
   (funcall (compile nil '(lambda (p1)
			    (declare (optimize (speed 2) (safety 3) (debug 2) (space 3))
				     (type (integer -69741922) p1))
			    (/= (the (integer * 216) p1) 182)))
	    103))
  t)

; X86::FAST-IF->-C/FIXNUM
(deftest cmucl-type-prop.26
  (funcall (compile nil '(lambda (p2)
			   (declare (optimize (speed 2) (safety 2) (debug 3) (space 3))
				    (type (integer -451 204073899) p2))
			   (< 134799 (the (integer -56 8589934581) p2))))
	   -2)
  nil)

; X86::FAST-IF-<-C/FIXNUM
(deftest cmucl-type-prop.27
  (funcall (compile nil '(lambda (p2)
			   (declare (optimize (speed 2) (safety 2) (debug 2) (space 3))
				    (type (integer -93662 *) p2))
			   (<= -1 (the (integer -2975848 16770677) p2))))
	    -6548)
  nil)

; X86::FAST-+-C/FIXNUM=>FIXNUM
; (simple example)
(deftest cmucl-type-prop.28
  (funcall (compile nil '(lambda (p1)
			   (declare (optimize (speed 2) (safety 1) (debug 0) (space 3))
				    (type (integer -65545 80818) p1))
			   (1+ p1)))
	   -1)
  0)

; X86::FAST-NEGATE/FIXNUM
(deftest cmucl-type-prop.29
  (funcall (compile nil '(lambda (p1)
			   (declare (optimize (speed 2) (safety 1) (debug 0) (space 3))
				    (type (integer -4194320 11531) p1))
			   (- (the (integer -6253866924 34530147) p1))))
	   -20)
  20)

;;; Bug in COPY-SEQ

(deftest cmucl-type-prop.30
  (let ((a (funcall
	    (compile nil `(lambda ()
			    (declare (optimize (speed 2) (safety 2) (debug 0) (space 2)))
			    (copy-seq
			     ,(make-array '(0) :adjustable t)))))))
    (and (not (adjustable-array-p a))
	 (= (length a) 0)
	 t))
  t)

; Bug for PACKAGEP

(deftest cmucl-type-prop.31
  (funcall (compile nil '(lambda (x)
			   (declare (optimize (speed 2) (space 3)))
			   (packagep x)))
	   t)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There were many failures in string comparison functions
;;; Some are that C::WIN strange template problem, but others
;;; are not.

;;;  0 is not of type (INTEGER 0 (0))
(deftest cmucl-type-prop.32
  (funcall
   (compile
    nil
    '(lambda (p4)
       (declare (optimize (speed 1) (safety 1) (debug 1) (space 0))
		(type (integer -2040 9) p4))
       (string< "bbaa" "" :start1 p4)))
   2)
  nil)

;;;  2 is not of type (INTEGER 0 (2))
(deftest cmucl-type-prop.33
  (funcall
   (compile
    nil
    '(lambda (p4)
       (declare (optimize (speed 0) (safety 0) (debug 2) (space 0))
		(type (integer -52340 *) p4))
       (string< "baabbb" "bb" :start2 p4)))
   1)
  nil)

;;; Incorrect return value
(deftest cmucl-type-prop.34
  (funcall
   (compile
    nil
    '(lambda (p1 p4)
       (declare (optimize (speed 2) (safety 0) (debug 3) (space 0))
		(type (simple-string) p1) (type real p4))
       (string< (the array p1)
		"bbbba"
		:start1 (the (integer -16382 *) p4)
		:end1 7)))
   "J4sPI71C3Xn" 5)
  5)
