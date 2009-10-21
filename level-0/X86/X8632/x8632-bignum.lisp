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

;;; %BIGNUM-REF needs to access bignums as obviously as possible, and it needs
;;; to be able to return 32 bits somewhere no one looks for real objects.
;;;
;;; The easiest thing to do is to store the 32 raw bits in two fixnums
;;; and return multiple values.
;;;
(defx8632lapfunction %bignum-ref ((bignum arg_y) (i arg_z))
  (movl (% esp) (% temp0))		;ptr to return addr on stack in temp0
  (movzwl (@ (+ 2 x8632::misc-data-offset) (% bignum) (% i)) (% imm0))
  (box-fixnum imm0 temp1)
  (push (% temp1))			;high
  (movzwl (@ x8632::misc-data-offset (% bignum) (% i)) (% imm0))
  (box-fixnum imm0 temp1)
  (push (% temp1))			;low
  (set-nargs 2)
  (jmp-subprim .SPvalues))

(defx8632lapfunction %bignum-ref-hi ((bignum arg_y) (i arg_z))
  (movzwl (@ (+ 2 x8632::misc-data-offset) (% bignum) (% i)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

;;; BIGNUM[I] := DIGIT[0]
(defx8632lapfunction %set-digit ((bignum 4) #|(ra 0)|# (i arg_y) (digit arg_z))
  (movl (@ bignum (% esp)) (% temp0))
  (svref digit 0 imm0)
  (movl (% imm0) (@ x8632::misc-data-offset (% temp0) (% i)))
  (single-value-return 3))

;;; Return the sign of bignum (0 or -1) as a fixnum
(defx8632lapfunction %bignum-sign ((bignum arg_z))
  (vector-length bignum imm0)
  (movl (@ (- x8632::misc-data-offset 4) (% bignum) (% imm0)) (% imm0))
  (sarl ($ 31) (% imm0))		;propagate sign bit
  (box-fixnum imm0 arg_z)
  (single-value-return))

;;; Count the sign bits in the most significant digit of bignum;
;;; return fixnum count.
(defx8632lapfunction %bignum-sign-bits ((bignum arg_z))
  (vector-length bignum imm0)
  (movl (@ (- x8632::misc-data-offset 4) (% bignum) (% imm0)) (% imm0))
  (mark-as-imm temp0)
  (movl (% imm0) (% temp0))
  (notl (% imm0))
  (testl (% temp0) (% temp0))
  (js @wasneg)
  (notl (% imm0))
  @wasneg
  (bsrl (% imm0) (% imm0))
  (sete (% temp0.b))
  (xorl ($ 31) (% imm0))
  (addb (% temp0.b) (% imm0.b))
  (box-fixnum imm0 arg_z)
  (mark-as-node temp0)
  (single-value-return))

(defx8632lapfunction %digit-0-or-plusp ((bignum arg_y) (idx arg_z))
  (movl (@ x8632::misc-data-offset (% bignum) (% idx)) (% imm0))
  (movl ($ (target-nil-value)) (% temp0))
  (leal (@ x8632::t-offset (% temp0)) (% arg_z))
  (testl (% imm0) (% imm0))
  (cmovll (% temp0) (% arg_z))
  (single-value-return))

;;; For oddp, evenp
(defx8632lapfunction %bignum-oddp ((bignum arg_z))
  (movl (@ x8632::misc-data-offset (% bignum)) (% imm0))
  (movl ($ (target-nil-value)) (% temp0))
  (leal (@ x8632::t-offset (% temp0)) (% arg_z))
  (testb ($ 1) (% imm0.b))
  (cmovzl (% temp0) (% arg_z))
  (single-value-return))

(defx8632lapfunction bignum-plusp ((bignum arg_z))
  (vector-length bignum imm0)
  (movl (@ (- x8632::misc-data-offset 4) (% bignum) (% imm0)) (% imm0))
  (movl ($ (target-nil-value)) (% arg_z))
  (lea (@ x8632::t-offset (% arg_z)) (% temp0))
  (testl (% imm0) (% imm0))
  (cmovnsl (% temp0) (% arg_z))
  (single-value-return))

(defx8632lapfunction bignum-minusp ((bignum arg_z))
  (vector-length bignum imm0)
  (movl (@ (- x8632::misc-data-offset 4) (% bignum) (% imm0)) (% imm0))
  (movl ($ (target-nil-value)) (% arg_z))
  (lea (@ x8632::t-offset (% arg_z)) (% temp0))
  (testl (% imm0) (% imm0))
  (cmovsl (% temp0) (% arg_z))
  (single-value-return))

;;; Add the digits A[I] and B[J], and the incoming carry C (a fixnum,
;;; either 0 or 1).  Store the result in R[K], and return the outgoing
;;; carry.  If I is NIL, A is a fixnum.  If J is NIL, B is a fixnum.
(defx8632lapfunction %add-with-carry ((r 20) (k 16) (c 12) (a 8) (i 4) #|(ra 0)|# (b arg_y) (j arg_z))
  (mark-as-imm temp0)
  (unbox-fixnum b imm0)
  (cmpl ($ (target-nil-value)) (% j))
  ;; if j not nil, get b[j]
  (cmovnel (@ x8632::misc-data-offset (% b) (% j)) (% imm0))
  (movl (@ a (% esp)) (% arg_y))
  (unbox-fixnum arg_y temp0)
  (movl (@ i (% esp)) (% arg_z))
  (cmpl ($ (target-nil-value)) (% arg_z))
  ;; if i not nil, get a[i]
  (cmovnel (@ x8632::misc-data-offset (% arg_y) (% arg_z)) (% temp0))
  (xorl (% arg_z) (% arg_z))
  ;; I can't think of a better way to set CF at the moment.
  ;; NEG would be ideal, but we don't have a free imm reg.
  (btl ($ x8632::fixnumshift) (@ c (% esp))) ;CF = lsb of carry fixnum 
  (adc (% temp0) (% imm0))
  (setc (% arg_z.bh))
  (sarl ($ (- 8 x8632::fixnumshift)) (% arg_z)) ;outgoing carry
  (mark-as-node temp0)
  (movl (@ r (% esp)) (% temp0))
  (movl (@ k (% esp)) (% temp1))
  (movl (% imm0) (@ x8632::misc-data-offset (% temp0) (% temp1)))
  (single-value-return 7))

;;; Add the digits A[I] and B[J], and the incoming carry C (a fixnum).
;;; Store the result in R[K], and return the outgoing carry.  If I is
;;; NIL, A is a fixnum.  If J is NIL, B is a fixnum.
#+sse2
(defx8632lapfunction %add-with-carry ((r 20) (k 16) (c 12) (a 8) (i 4) #|(ra 0)|# (b arg_y) (j arg_z))
  (let ((aa mm2)
	(bb mm3)
	(cc mm4))
    (unbox-fixnum b imm0)		;assume j will be nil
    (cmpl ($ (target-nil-value)) (% j))
    ;; if j not nil, get b[j]
    (cmovnel (@ x8632::misc-data-offset (% b) (% j)) (% imm0))
    (movd (% imm0) (% bb))
    (movl (@ a (% esp)) (% arg_y))
    (movl (@ i (% esp)) (% arg_z))
    (movl (@ c (% esp)) (% temp0))
    (unbox-fixnum arg_y imm0)		;assume i will be nil
    (cmpl ($ (target-nil-value)) (% arg_z))
    ;; if i not nil, get a[i]
    (cmovnel (@ x8632::misc-data-offset (% arg_y) (% arg_z)) (% imm0))
    (movd (% imm0) (% aa))
    (unbox-fixnum temp0 imm0)
    (movd (% imm0) (% cc))
    (paddq (% xx) (% yy))
    (paddq (% cc) (% yy))
    (movl (@ r (% esp)) (% temp0))
    (movl (@ k (% esp)) (% temp1))
    (movd (% yy) (@ x8632::misc-data-offset (% temp0) (% temp1)))
    (psrlq ($ 32) (% yy))		;carry bit
    (movd (% yy) (% imm0))
    (box-fixnum imm0 arg_z)
    (single-value-return 7)))

;;; Store the result of A[I] - B[J] - borrow into R[K], returning the borrow.
;;; If I is NIL, A is a fixnum; likewise for J and B.
;;;
;;; (a - b) - (1 - borrow), or equivalently, (a - b) + borrow - 1
;;; 
;;; Note: borrow is 1 for no borrow and 0 for a borrow.
(defx8632lapfunction %subtract-with-borrow ((r 20) (k 16) (borrow 12) (a 8) (i 4) #|(ra 0)|# (b arg_y) (j arg_z))
  (mark-as-imm temp0)
  (unbox-fixnum b imm0)
  (cmpl ($ (target-nil-value)) (% j))
  (cmovnel (@ x8632::misc-data-offset (% b) (% j)) (% imm0))
  (movl (@ a (% esp)) (% arg_y))
  (unbox-fixnum arg_y temp0)
  (movl (@ i (% esp)) (% arg_z))
  (cmpl ($ (target-nil-value)) (% arg_z))
  (cmovnel (@ x8632::misc-data-offset (% arg_y) (% arg_z)) (% temp0))
  ;; unboxed a or a[i] in temp0, unboxed b or b[j] in imm0
  (cmpl ($ '1) (@ borrow (% esp)))	;CF = 1 if borrow is 0 else CF = 0
  (sbb (% imm0) (% temp0))
  (movl ($ 1) (% imm0))
  (sbb ($ 0) (% imm0))
  (box-fixnum imm0 arg_z)
  (movl (% temp0) (% imm0))
  (mark-as-node temp0)
  (movl (@ r (% esp)) (% temp0))
  (movl (@ k (% esp)) (% temp1))
  (movl (% imm0) (@ x8632::misc-data-offset (% temp0) (% temp1)))
  (single-value-return 7))

#+sse2
(defx8632lapfunction %subtract-with-borrow ((r 20) (k 16) (borrow 12) (a 8) (i 4) #|(ra 0)|# (b arg_y) (j arg_z))
  (let ((aa mm2)
	(bb mm3)
	(ww mm4))
    (unbox-fixnum b imm0)
    (cmpl ($ (target-nil-value)) (% j))
    ;; if j not nil, get b[j]
    (cmovnel (@ x8632::misc-data-offset (% b) (% j)) (% imm0))
    (movd (% imm0) (% bb))
    (movl (@ a (% esp)) (% arg_y))
    (movl (@ i (% esp)) (% arg_z))
    (movl (@ borrow (% esp)) (% temp0))
    (unbox-fixnum arg_y imm0)
    (cmpl ($ (target-nil-value)) (% arg_z))
    ;; if i not nil, get a[i]
    (cmovnel (@ x8632::misc-data-offset (% arg_y) (% arg_z)) (% imm0))
    (movd (% imm0) (% aa))
    (unbox-fixnum temp0 imm0)
    (subl ($ 1) (% imm0))
    (movd (% imm0) (% ww))
    (psubq (% bb) (% aa))
    (paddq (% ww) (% aa))
    (movl (@ r (% esp)) (% temp0))
    (movl (@ k (% esp)) (% temp1))
    (movd (% aa) (@ x8632::misc-data-offset (% temp0) (% temp1)))
    (psrlq ($ 32) (% aa))		;carry digit
    (movd (% aa) (% imm0))
    (xorl (% arg_z) (% arg_z))
    (test ($ 1) (% imm0))
    (cmovzl ($ '1) (% arg_z))
    (single-value-return 7)))

(defx8632lapfunction %subtract-one ((high arg_y) (low arg_z))
  (shll ($ (- 16 x8632::fixnumshift)) (% arg_y))
  (unbox-fixnum low imm0)
  ;; high half should always be clear...
  ;;(movzwl (% imm0.w) (% imm0))
  (orl (% arg_y) (% imm0))
  (decl (% imm0))
  (movl (% esp) (% temp0))
  ;; extract and push high half
  (movl ($ (- #x10000)) (% arg_y))
  (andl (% imm0) (% arg_y))
  (shrl ($ (- 16 x8632::fixnumshift)) (% arg_y))
  (push (% arg_y))
  ;; low half
  (andl ($ #xffff) (% imm0))
  (shll ($ x8632::fixnumshift) (% imm0))
  (push (% imm0))
  (set-nargs 2)
  (jmp-subprim .SPvalues))

;;; %SUBTRACT-WITH-BORROW -- Internal.
;;;
;;; This should be in assembler, and should not cons intermediate results.  It
;;; returns a 32bit digit and a borrow resulting from subtracting b from a, and
;;; subtracting a possible incoming borrow.
;;;
;;; We really do:  a - b - 1 + borrow, where borrow is either 0 or 1.
;;; 

(defx8632lapfunction %subtract-with-borrow-1 ((a-h 12) (a-l 8) (b-h 4) #|(ra 0)|# (b-l arg_y) (borrow arg_z))
  (mark-as-imm temp0)
  (mark-as-imm temp1)
  (unbox-fixnum b-l temp0)
  (movl (@ b-h (% esp)) (% imm0))
  (sarl ($ x8632::fixnumshift) (% imm0))
  (shll ($ 16) (% imm0))
  (orl (% imm0) (% temp0))		;b in temp0
  (movl (@ a-l (% esp)) (% temp1))
  (sarl ($ x8632::fixnumshift) (% temp1))
  (movl (@ a-h (% esp)) (% imm0))
  (sarl ($ x8632::fixnumshift) (% imm0))
  (shll ($ 16) (% imm0))
  (orl (% imm0) (% temp1))	    ;a in temp1

  (unbox-fixnum borrow imm0)
  (subl ($ 1) (% imm0))			;sets carry appropriately
  (sbbl (% temp0) (% temp1))
  (setae (%b imm0))			;resulting borrow (1 for no, 0 for yes)
  (movzbl (%b imm0) (% imm0))
  (box-fixnum imm0 arg_z)
  (movl (% temp1) (% imm0))
  (andl ($ (- #x10000)) (% imm0))
  (shrl ($ (- 16 x8632::fixnumshift)) (% imm0))
  (popl (% arg_y))			;return address
  (addl ($ '5) (% esp))			;discard reserved frame & stack args
  (pushl (% arg_y))
  (push (% imm0))			;high
  (andl ($ #xffff) (% temp1))
  (box-fixnum temp1 imm0)
  (mark-as-node temp0)
  (mark-as-node temp1)
  (push (% imm0))			;low
  (push (% arg_z))			;borrow
  (set-nargs 3)
  (leal (@ '3 (% esp)) (% temp0))
  (jmp-subprim .SPvalues))
  

;;; To normalize a bignum is to drop "trailing" digits which are
;;; redundant sign information.  When return-fixnum-p is non-nil, make
;;; the resultant bignum into a fixnum if it fits.
(defx8632lapfunction %normalize-bignum-2 ((return-fixnum-p arg_y) (bignum arg_z))
  (push (% return-fixnum-p))
  (mark-as-imm temp0)
  (mark-as-imm temp1)
  (let ((len arg_y)
	(sign temp0)
	(next temp1))
    (vector-length bignum len)
    (cmpl ($ '1) (% len))
    (jle @maybe-return-fixnum)
    ;; Zero trailing sign digits.
    (push (% len))
    ;; next-to-last digit
    (movl (@ (- x8632::misc-data-offset 8) (% bignum) (% len)) (% next))
    ;; last digit
    (movl (@ (- x8632::misc-data-offset 4) (% bignum) (% len)) (% sign))
    (jmp @test)
    @loop
    (subl ($ '1) (% len))
    (movl ($ 0) (@ x8632::misc-data-offset (% bignum) (% len)))
    (cmpl ($ '1) (% len))		;any more digits?
    (je @adjust-length)
    (movl (% next) (% sign))
    ;; (bignum-ref bignum (- len 2))
    (movl (@ (- x8632::misc-data-offset 8) (% bignum) (% len)) (% next))
    @test
    (movl (% next) (% imm0))
    (sarl ($ 31) (% imm0))		;propagate sign bit
    (xorl (% sign) (% imm0))		;whole digit only sign?
    (jz @loop)
    ;; New length now in len.
    @adjust-length
    (pop (% imm0))			;original length
    (cmpl (% len) (% imm0))
    ;; If the new length is the same as the original length, we know
    ;; that the bignum is at least two digits long (because if it was
    ;; shorter, we would have branched directly to
    ;; @maybe-return-fixnum), and thus won't fit in a fixnum.
    ;; Therefore, there's no need to do either of the tests at
    ;; @maybe-return-fixnum.
    (je @done)
    (movl (% len) (% imm0))
    (shll ($ (- x8632::num-subtag-bits x8632::fixnumshift)) (% imm0))
    (movb ($ x8632::subtag-bignum) (% imm0.b))
    (movl (% imm0) (@ x8632::misc-header-offset (% bignum)))
    @maybe-return-fixnum
    ;; could use SETcc here to avoid one branch
    (cmpl ($ (target-nil-value)) (@ 0 (% esp))) ;return-fixnum-p
    (je @done)
    (cmpl ($ x8632::one-digit-bignum-header)
	  (@ x8632::misc-header-offset (% bignum)))
    (jne @done)
    ;; Bignum has one digit.  If it fits in a fixnum, return a fixnum.
    (movl (@ x8632::misc-data-offset (% bignum)) (% imm0))
    (box-fixnum imm0 arg_y)
    (unbox-fixnum arg_y temp0)
    (cmpl (% temp0) (% imm0))
    (cmovel (% arg_y) (% arg_z))
    @done
    (pop (% imm0))			;discard saved return-fixnum-p
    (mark-as-node temp0)
    (mark-as-node temp1)
    (single-value-return)))

;;; Multiply X[I] by the unboxed value of the (non-negative) fixnum Y;
;;; add the incoming carry from CARRY[0] to the 64-bit product.  Store
;;; the low word of the 64-bit sum in R[0] and the high word in
;;; CARRY[0].
(defx8632lapfunction %multiply-and-add ((r 12) (carry 8) (x 4) #|(ra 0)|# (i arg_y) (y arg_z))
  (let ((xx mm2)
	(yy mm3)
	(cc mm4))
    (movl (@ x (% esp)) (% imm0))
    (movd (@ x8632::misc-data-offset (% imm0) (% i)) (% xx))
    (unbox-fixnum y imm0)
    (movd (% imm0) (% yy))
    (pmuludq (% xx) (% yy))		;64 bit product
    (movl (@ carry (% esp)) (% arg_y))
    (movd (@ x8632::misc-data-offset (% arg_y)) (% cc))
    (paddq (% cc) (% yy))		;add in 32 bit carry digit
    (movl (@ r (% esp)) (% arg_z))
    (movd (% yy) (@ x8632::misc-data-offset (% arg_z)))
    (psrlq ($ 32) (% yy))
    (movd (% yy) (@ x8632::misc-data-offset (% arg_y)))
    (single-value-return 5)))

;; multiply x[i] by y and add to result starting at digit i
(defx8632lapfunction %multiply-and-add-harder-loop-2
    ((x 12) (y 8) (r 4) #|(ra 0)|# (i arg_y) (ylen arg_z))
  (let ((cc mm2)
	(xx mm3)
	(yy mm4)
	(rr mm5)
	(j imm0))
    (movl (@ x (% esp)) (% temp0))
    (movd (@ x8632::misc-data-offset (% temp0) (% i)) (% xx)) ;x[i]
    (movl (@ y (% esp)) (% temp0))
    (movl (@ r (% esp)) (% temp1))
    (pxor (% cc) (% cc))
    (xorl (% j) (% j))
    @loop
    (movd (@ x8632::misc-data-offset (% temp0) (% j)) (% yy)) ;y[j]
    (pmuludq (% xx) (% yy))
    ;; 64-bit product now in %yy
    (movd (@ x8632::misc-data-offset (% temp1) (% i)) (% rr))
    ;; add in digit from r[i]
    (paddq (% yy) (% rr))
    ;; add in carry
    (paddq (% cc) (% rr))
    (movd (% rr) (@ x8632::misc-data-offset (% temp1) (% i))) ;update r[i]
    (movq (% rr) (% cc))
    (psrlq ($ 32) (% cc))		;get carry digit into low word
    (addl ($ '1) (% i))
    (addl ($ '1) (% j))
    (subl ($ '1) (% ylen))
    (jg @loop)
    (movd (% cc) (@ x8632::misc-data-offset (% temp1) (% i)))
    (single-value-return 5)))

;; this is silly  
(defx8632lapfunction %add-the-carry ((high 4) #|(ra 0)|# (low arg_y) (c arg_z))
  (mark-as-imm temp0)
  (let ((imm1 temp0)
	(imm1.w temp0.w))
    (pop (% temp1))
    (popl (% imm1))			;high
    (discard-reserved-frame)
    (push (% temp1))
    (shll ($ (- 16 x8632::fixnumshift)) (% temp0))
    (unbox-fixnum low imm0)
    (orl (% imm0) (% imm1))
    (unbox-fixnum c imm0)
    (addl (% imm0) (% imm1))
    (movzwl (% imm1.w) (% imm0))
    (box-fixnum imm0 temp1)
    (sarl ($ 16) (% imm1))
    (shll ($ x8632::fixnumshift) (% imm1))
    (push (% imm1))			;high
    (push (% temp1)))			;low
  (mark-as-node temp0)
  (set-nargs 2)
  (leal (@ '2 (% esp)) (% temp0))
  (jmp-subprim .SPvalues))

(defx8632lapfunction %bignum-count-trailing-zero-bits ((bignum arg_z))
  (let ((i arg_y)
	(len temp0)
	(zeros temp1))
    (vector-length bignum temp0)
    (xorl (% i) (% i))
    (xorl (% zeros) (% zeros))
    @loop
    (movl (@ x8632::misc-data-offset (% bignum) (% i)) (% imm0))
    (testl (% imm0) (% imm0))
    (jnz @last)
    (addl ($ '32) (% zeros))
    (addl ($ '1) (% i))
    (cmpl (% len) (% i))
    (jb @loop)
    @last
    ;; now count zero bits in digit
    (bsfl (% imm0) (% imm0))
    (shll ($ x8632::fixnumshift) (% imm0))
    (addl (% imm0) (% zeros))
    (movl (% zeros) (% arg_z))
    (single-value-return)))

;;; dest[i] = (logand x[i] y[i])
(defx8632lapfunction %bignum-logand ((idx 8) (x 4) #|(ra 0)|# (y arg_y) (dest arg_z))
  (let ((i temp0)
	(xx temp1)
	(yy arg_y))
    (movl (@ idx (% esp)) (% i))
    (movl (@ x (% esp)) (% xx))
    (movl (@ x8632::misc-data-offset (% xx) (% i)) (% imm0))
    (andl (@ x8632::misc-data-offset (% yy) (% i)) (% imm0))
    (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% i)))
    (single-value-return 4)))

;;; dest[i] = (logandc1 x[i] y[i])
(defx8632lapfunction %bignum-logandc1 ((idx 8) (x 4) #|(ra 0)|# (y arg_y) (dest arg_z))
  (let ((i temp0)
	(xx temp1)
	(yy arg_y))
    (movl (@ idx (% esp)) (% i))
    (movl (@ x (% esp)) (% xx))
    (movl (@ x8632::misc-data-offset (% xx) (% i)) (% imm0))
    (not (% imm0))
    (andl (@ x8632::misc-data-offset (% yy) (% i)) (% imm0))
    (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% i)))
    (single-value-return 4)))

;;; dest[i] = (logandc2 x[i] y[i])
(defx8632lapfunction %bignum-logandc2 ((idx 8) (x 4) #|(ra 0)|# (y arg_y) (dest arg_z))
  (let ((i temp0)
	(xx temp1)
	(yy arg_y))
    (movl (@ idx (% esp)) (% i))
    (movl (@ x (% esp)) (% xx))
    (movl (@ x8632::misc-data-offset (% yy) (% i)) (% imm0))
    (not (% imm0))
    (andl (@ x8632::misc-data-offset (% xx) (% i)) (% imm0))
    (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% i)))
    (single-value-return 4)))

;;; dest[i] = (logior x[i] y[i])
(defx8632lapfunction %bignum-logior ((idx 8) (x 4) #|(ra 0)|# (y arg_y) (dest arg_z))
  (let ((i temp0)
	(xx temp1)
	(yy arg_y))
    (movl (@ idx (% esp)) (% i))
    (movl (@ x (% esp)) (% xx))
    (movl (@ x8632::misc-data-offset (% xx) (% i)) (% imm0))
    (orl (@ x8632::misc-data-offset (% yy) (% i)) (% imm0))
    (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% i)))
    (single-value-return 4)))

;;; dest[i] = (lognot x[i])
(defx8632lapfunction %bignum-lognot ((idx 4) #|(ra 0)|# (x arg_y) (dest arg_z))
  (let ((i temp0))
    (movl (@ idx (% esp)) (% i))
    (movl (@ x8632::misc-data-offset (% x) (% i)) (% imm0))
    (not (% imm0))
    (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% i)))
    (single-value-return 3)))

;;; dest[i] = (logxor x[i] y[i])
(defx8632lapfunction %bignum-logxor ((idx 8) (x 4) #|(ra 0)|# (y arg_y) (dest arg_z))
  (let ((i temp0)
	(xx temp1)
	(yy arg_y))
    (movl (@ idx (% esp)) (% i))
    (movl (@ x (% esp)) (% xx))
    (movl (@ x8632::misc-data-offset (% xx) (% i)) (% imm0))
    (xorl (@ x8632::misc-data-offset (% yy) (% i)) (% imm0))
    (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% i)))
    (single-value-return 4)))

;;; 0 if a[i] = b[i]; 1 if a[i] > b[i]; -1 if a[i] < b[i]
(defx8632lapfunction %compare-digits ((a 4) #|(ra 0)|# (b arg_y) (i arg_z))
  (movl (@ a (% esp)) (% temp0))
  (movl (@ x8632::misc-data-offset (% temp0) (% i)) (% imm0))
  (movl ($ '1) (% temp0))
  (movl ($ '-1) (% temp1))
  (subl (@ x8632::misc-data-offset (% b) (% i)) (% imm0))
  (cmoval (% temp0) (% imm0))
  (cmovbl (% temp1) (% imm0))
  (movl (% imm0) (% arg_z))
  (single-value-return 3))

;; returns number of bits in digit-hi,digit-lo that are sign bits
;; 32 - digits-sign-bits is integer-length
(defx8632lapfunction %digits-sign-bits ((hi arg_y) (lo arg_z))
  (mark-as-imm temp0)
  (shll ($ (- 16 x8632::fixnumshift)) (% hi))
  (unbox-fixnum lo imm0)
  (orl (% hi) (% imm0))
  (movl (% imm0) (% temp0))
  (not (% imm0))
  (testl (% temp0) (% temp0))
  (js @wasneg)
  (not (% imm0))
  @wasneg
  (bsrl (% imm0) (% imm0))
  (sete (% temp0.b))
  (xorl ($ 31) (% imm0))
  (addb (% temp0.b) (% imm0.b))
  (box-fixnum imm0 arg_z)
  (mark-as-node temp0)
  (single-value-return))

(defx8632lapfunction macptr->fixnum ((ptr arg_z))
  (macptr-ptr arg_z ptr)
  (single-value-return))

; if dest not nil store unboxed result in dest(0), else return a fixnum
(defx8632lapfunction fix-digit-logandc2 ((fix 4) #|(ra 0)|# (big arg_y) (dest arg_z)) ; index 0
  (mark-as-imm temp0)
  (movl (@ fix (% esp)) (% temp0))
  (unbox-fixnum temp0 temp0)
  (movl (@ x8632::misc-data-offset (% big)) (% imm0))
  (not (% imm0))
  (andl (% temp0) (% imm0))
  (mark-as-node temp0)
  (cmpl ($ (target-nil-value)) (% dest))
  (jne @store)
  (box-fixnum imm0 arg_z)
  (single-value-return 3)
  @store
  (movl (% imm0) (@ x8632::misc-data-offset (% dest)))
  (single-value-return 3))

(defx8632lapfunction fix-digit-logandc1 ((fix 4) #|(ra 0)|# (big arg_y) (dest arg_z)) ; index 0
  (mark-as-imm temp0)
  (movl (@ fix (% esp)) (% temp0))
  (unbox-fixnum temp0 temp0)
  (movl (@ x8632::misc-data-offset (% big)) (% imm0))
  (not (% temp0))
  (andl (% temp0) (% imm0))
  (mark-as-node temp0)
  (cmpl ($ (target-nil-value)) (% dest))
  (jne @store)
  (box-fixnum imm0 arg_z)
  (single-value-return 3)
  @store
  (movl (% imm0) (@ x8632::misc-data-offset (% dest)))
  (single-value-return 3))

(defx8632lapfunction fix-digit-logand ((fix 4) #|(ra 0)|# (big arg_y) (dest arg_z)) ; index 0
  (mark-as-imm temp0)
  (movl (@ fix (% esp)) (% temp0))
  (sarl ($ x8632::fixnumshift) (% temp0))
  (movl (@ x8632::misc-data-offset (% big)) (% imm0))
  (andl (% temp0) (% imm0))
  (mark-as-node temp0)
  (cmpl ($ (target-nil-value)) (% dest))
  (jne @store)
  (box-fixnum imm0 arg_z)
  (single-value-return 3)
  @store
  (movl (% imm0) (@ x8632::misc-data-offset (% dest)))
  (single-value-return 3))


(defx8632lapfunction digit-lognot-move ((index 4) #|(ra 0)|# (source arg_y) (dest arg_z))
  (movl (@ index (% esp)) (% temp0))
  (movl (@ x8632::misc-data-offset (% source) (% temp0)) (% imm0))
  (not (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

;; Add b to a starting at a[i]
;; might want not to use SSE2 for this.  use lea to update loop counter
;; variables so that the flags don't get set.
(defx8632lapfunction bignum-add-loop-+ ((i 8) (a 4) #|(ra 0)|# (b arg_y) (blen arg_z))
  (let ((aa mm2)
	(bb mm3)
	(cc mm4))
    (movl (@ a (% esp)) (% temp0))
    (movl (@ i (% esp)) (% temp1))
    (xorl (% imm0) (% imm0))
    (pxor (% cc) (% cc))
    @loop
    (movd (@ x8632::misc-data-offset (% temp0) (% temp1)) (% aa))
    (movd (@ x8632::misc-data-offset (% b) (% imm0)) (% bb))
    (paddq (% bb) (% aa))
    (paddq (% cc) (% aa))
    (movd (% aa) (@ x8632::misc-data-offset (% temp0) (% temp1)))
    (psrlq ($ 32) (% aa))
    (movq (% aa) (% cc))
    (addl ($ '1) (% temp1))
    (addl ($ '1) (% imm0))
    (subl ($ '1) (% blen))
    (jg @loop)
    ;; add in final carry
    (movd (% cc) (% imm0))
    (addl (% imm0) (@ x8632::misc-data-offset (% temp0) (% temp1)))
    (single-value-return 4)))

(defx8632lapfunction bignum-logtest-loop ((count 4) #|(ra 0)|# (s1 arg_y) (s2 arg_z))
  (let ((i temp1)
	(c temp0))
    (movl (@ count (% esp)) (% c))
    (xorl (% i) (% i))
    @loop
    (movl (@ x8632::misc-data-offset (% s1) (% i)) (% imm0))
    (test (@ x8632::misc-data-offset (% s2) (% i)) (% imm0))
    (jnz @true)
    (addl ($ '1) (% i))
    (cmpl (% i) (% c))
    (jg @loop)
    (movl ($ (target-nil-value)) (% arg_z))
    (single-value-return 3)
    @true
    (movl ($ (target-t-value)) (% arg_z))
    (single-value-return 3)))

;;; shift bignum left by nbits bits (1 <= nbits < 32)
;;; start storing into result at digit j
(defx8632lapfunction bignum-shift-left-loop ((nbits 12) (result 8)
					     (bignum 4) #|(ra 0)|#
					     (res-len-1 arg_y) (j arg_z))
  (movl (% ebp) (@ 16 (% esp)))
  (leal (@ 16 (% esp)) (% ebp))
  (popl (@ 4 (% ebp)))
  (push (% arg_y))			;ebp - 16
  (push (% arg_z))			;ebp - 20

  (movl (@ -4 (% ebp)) (% imm0))
  (sarl ($ x8632::fixnumshift) (% imm0))
  (movd (% imm0) (% mm7))		;shift count
  (negl (% imm0))
  (addl ($ 32) (% imm0))
  (movd (% imm0) (% mm6))		;remaining bits

  (let ((rl-1 -16)
	(r temp0)
	(b temp1)
	(i arg_y)
	(i+1 imm0))
    (movl (@ -8 (% ebp)) (% r))
    (movl (@ -12 (% ebp)) (% b))
    (xorl (% i) (% i))
    (movl ($ '1) (% i+1))
    ;; j (in arg_z) is already (1+ digits)
    (jmp @test)
    @loop
    (movd (@ x8632::misc-data-offset (% b) (% i)) (% mm0))
    (psrlq (% mm6) (% mm0))
    (movd (@ x8632::misc-data-offset (% b) (% i+1)) (% mm1))
    (psllq (% mm7) (% mm1))
    (por (% mm1) (% mm0))
    (movd (% mm0) (@ x8632::misc-data-offset (% r) (% j)))
    (movl (% i+1) (% i))
    (addl ($ '1) (% i+1))
    (addl ($ '1) (% j))
    @test
    (cmpl (@ rl-1 (% ebp)) (% j))
    (jne @loop)
    (movd (@ x8632::misc-data-offset (% b)) (% mm0))
    (psllq (% mm7) (% mm0))
    (movl (@ -20 (% ebp)) (% imm0))	;digits + 1 (that is, the original j)
    (subl ($ '1) (% imm0))		;digits
    (movd (% mm0) (@ x8632::misc-data-offset (% r) (% imm0)))
    (movd (@ x8632::misc-data-offset (% b) (% i)) (% mm0))
    (psrad (% mm6) (% mm0))
    (movd (% mm0) (@ x8632::misc-data-offset (% r) (% j))))
  (leave)
  (ret))

;;; shift bignum right by i words plus nbits bits.
(defx8632lapfunction bignum-shift-right-loop-1 ((nbits 12) (result 8)
						(bignum 4) #|(ra 0)|#
						(res-len-1 arg_y)
						(i arg_z))
  (movl (@ nbits (% esp)) (% imm0))
  (sarl ($ x8632::fixnumshift) (% imm0))
  (movd (% imm0) (% mm7))		;shift count

  (movl (@ result (% esp)) (% temp0))
  (movl (@ bignum (% esp)) (% temp1))
  (push (% res-len-1))
  (xorl (% arg_y) (% arg_y))		;index into result
  (jmp @test)
  @loop
  (movq (@ x8632::misc-data-offset (% temp1) (% i)) (% mm0)) ;b[i+1] || b[i]
  (psrlq (% mm7) (% mm0))
  (movd (% mm0) (@ x8632::misc-data-offset (% temp0) (% arg_y)))
  (addl ($ '1) (% i))
  (addl ($ '1) (% arg_y))
  @test
  (cmpl (@ (% esp)) (% arg_y))		;compare to res-len-1
  (jne @loop)
  (addl ($ x8632::node-size) (% esp))
  @finish
  (movd (@ x8632::misc-data-offset (% temp1) (% i)) (% mm0)) ;last digit of b
  (psrad (% mm7) (% mm0))
  (movd (% mm0) (@ x8632::misc-data-offset (% temp0) (% arg_y)))
  (single-value-return 5))

(defx8632lapfunction %logcount-complement ((bignum arg_y) (i arg_z))
  (mark-as-imm temp0)
  (let ((rshift imm0)
	(temp temp0))
    (movl (@ x8632::misc-data-offset (% bignum) (% i)) (% rshift))
    (notl (% rshift))
    (xorl (% arg_z) (% arg_z))
    (testl (% rshift) (% rshift))
    (jmp @test)
    @next
    (lea (@ -1 (% rshift)) (% temp))
    (and (% temp) (% rshift))		;sets flags
    (lea (@ '1 (% arg_z)) (% arg_z))	;doesn't set flags
    @test
    (jne @next)
    (mark-as-node temp0)
    (single-value-return)))

(defx8632lapfunction %logcount ((bignum arg_y) (i arg_z))
  (mark-as-imm temp0)
  (let ((rshift imm0)
	(temp temp0))
    (movl (@ x8632::misc-data-offset (% bignum) (% i)) (% rshift))
    (xorl (% arg_z) (% arg_z))
    (testl (% rshift) (% rshift))
    (jmp @test)
    @next
    (lea (@ -1 (% rshift)) (% temp))
    (and (% temp) (% rshift))		;sets flags
    (lea (@ '1 (% arg_z)) (% arg_z))	;doesn't set flags
    @test
    (jne @next)
    (mark-as-node temp0)
    (single-value-return)))


;;; Divide bignum x by single digit y (passed as two halves).
;;; The quotient in stored in q, and the remainder is returned
;;; in two halves.  (cf. Knuth, 4.3.1, exercise 16)
(defx8632lapfunction %floor-loop-quo ((x 8) (res 4) #|(ra 0)|# (yhi arg_y) (ylo arg_z))
  (compose-digit yhi ylo imm0)
  (movl (% imm0) (:rcontext x8632::tcr.unboxed0))
  (pop (% temp0))
  (pop (% arg_z))			;res
  (pop (% arg_y))			;x
  (discard-reserved-frame)
  (push (% temp0))
  (mark-as-imm edx)			;aka temp1
  (let ((bignum arg_y)			;bignum dividend
	(result arg_z))			;bignum result (quotient)
    (xorl (% edx) (% edx))
    (vector-length bignum temp0)
    (jmp @next)
    @loop
    (movl (@ x8632::misc-data-offset (% bignum) (% temp0)) (% eax))
    (divl (:rcontext x8632::tcr.unboxed0))
    (movl (% eax) (@ x8632::misc-data-offset (% result) (% temp0)))
    @next
    (subl ($ '1) (% temp0))
    (jge @loop))
  (movl (% esp) (% temp0))
  ;; extract and push high half of remainder
  (movl ($ (- #x10000)) (% arg_y))
  (andl (% edx) (% arg_y))
  (shrl ($ (- 16 x8632::fixnumshift)) (% arg_y))
  (push (% arg_y))
  ;; extract and push low half
  (andl ($ #xffff) (% edx))
  (shll ($ x8632::fixnumshift) (% edx))
  (push (% edx))
  (mark-as-node edx)
  (set-nargs 2)
  (jmp-subprim .SPvalues))

;;; For TRUNCATE-BY-FIXNUM et al.
;;; Doesn't store quotient: just returns rem in 2 halves.
;;; Could avoid using tcr.unboxed0 if it matters...
(defx8632lapfunction %floor-loop-no-quo ((x 4) #|(ra 0)|# (yhi arg_y) (ylo arg_z))
  (compose-digit yhi ylo imm0)
  (movl (% imm0) (:rcontext x8632::tcr.unboxed0))
  (pop (% temp0))
  (pop (% arg_y))
  (discard-reserved-frame)
  (push (% temp0))
  (mark-as-imm edx)			;aka temp1
  (let ((bignum arg_y)			;bignum dividend
	(result arg_z))			;bignum result (quotient)
    (xorl (% edx) (% edx))
    (vector-length bignum temp0)
    (jmp @next)
    @loop
    (movl (@ x8632::misc-data-offset (% bignum) (% temp0)) (% eax))
    (divl (:rcontext x8632::tcr.unboxed0))
    ;;(movl (% eax) (@ x8632::misc-data-offset (% result) (% temp0)))
    @next
    (subl ($ '1) (% temp0))
    (jge @loop))
  (movl (% esp) (% temp0))
  ;; extract and push high half of remainder
  (movl ($ (- #x10000)) (% arg_y))
  (andl (% edx) (% arg_y))
  (shrl ($ (- 16 x8632::fixnumshift)) (% arg_y))
  (push (% arg_y))
  ;; extract and push low half
  (andl ($ #xffff) (% edx))
  (shll ($ x8632::fixnumshift) (% edx))
  (push (% edx))
  (mark-as-node edx)
  (set-nargs 2)
  (jmp-subprim .SPvalues))

;;; transliterated from bignum-truncate-guess in l0-bignum64.lisp
;;; this is not beautiful...
(defx8632lapfunction truncate-guess-loop ((guess-h 16) (guess-l 12) (x 8)
					  (xidx 4) #|(ra 0)|#
					  (yptr arg_y) (yidx arg_z))
  (save-stackargs-frame 4)
  (push (% arg_y))
  (push (% arg_z))

  (movl (@ -4 (% ebp)) (% temp0))	;guess-h
  (movl (@ -8 (% ebp)) (% temp1))	;guess-l
  (compose-digit temp0 temp1 imm0)
  (movd (% imm0) (% mm0))		;save guess

  @loop
  (movl (@ (% esp)) (% yidx))
  (movl (@ 4 (% esp)) (% yptr))
  (movd (@ (- x8632::misc-data-offset 0) (% yptr) (% yidx)) (% mm1)) ;y1 (high)
  ;; (%multiply guess y1)
  (pmuludq (% mm0) (% mm1))
  ;; (%multiply guess y2)
  (movd (@ (- x8632::misc-data-offset 4) (% yptr) (% yidx)) (% mm2)) ;y2 (low)
  (pmuludq (% mm0) (% mm2))

  (movl (@ -12 (% ebp)) (% temp0))	 ;x
  (movl (@ -16 (% ebp)) (% arg_y))	 ;xidx
  (mark-as-imm temp1)			 ;edx now unboxed

  ;; (%subtract-with-borrow x-i-1 low-guess*y1 1)
  (movl (@ (- x8632::misc-data-offset 4) (% temp0) (% arg_y)) (% edx)) ;x-i-1
  (movd (% mm1) (% eax))		;low part of y1*guess
  (subl (% eax) (% edx))
  (movd (% edx) (% mm6))		;save middle digit
  ;; (%subtract-with-borrow x-i high-guess*y1 borrow)
  (movl (@ (- x8632::misc-data-offset 0) (% temp0) (% arg_y)) (% edx)) ;x-i
  (movq (% mm1) (% mm3))
  (psrlq ($ 32) (% mm3))		;get high part into low half
  (movd (% mm3) (% eax))		;high part of y1*guess
  (sbbl (% eax) (% edx))
  (movd (% edx) (% mm7))		;save high digit
  ;; see if guess is suitable
  ;; if (and (= high-digit 0)
  (test (% edx) (% edx))
  (jne @return)
  ;;         (or (> high-guess*y2 middle-digit)
  (movq (% mm2) (% mm3))
  (psrlq ($ 32) (% mm3))
  (movd (% mm3) (% eax))		;high part of y2*guess
  (movd (% mm6) (% edx))		;middle-digit
  (cmpl (% edx) (% eax))
  (ja @decrement)
  ;;             (and (= middle-digit high-guess*y2)
  (jne @return)
  ;;                  (> low-guess*y2 x-i-2)
  (movd (% mm2) (% eax))		;low part of y2*guess
  (movl (@ (- x8632::misc-data-offset 8) (% temp0) (% arg_y)) (% edx)) ;x-i-2
  (cmpl (% edx) (% eax))
  (ja @decrement)
  @return
  (mark-as-node edx)
  (leave)
  (movl (% esp) (% temp0))
  (movd (% mm0) (% imm0))
  (shrl ($ 16) (% imm0))
  (shll ($ x8632::fixnumshift) (% imm0)) ;high half
  (push (% imm0))
  (movd (% mm0) (% imm0))
  (andl ($ #xffff) (% imm0))
  (shll ($ x8632::fixnumshift) (% imm0))
  (push (% imm0))			;low half
  (set-nargs 2)
  (jmp-subprim .SPvalues)
  @decrement
  (movd (% mm0) (% imm0))		;guess
  (subl ($ 1) (% imm0))
  (movd (% imm0) (% mm0))
  (jmp @loop))

;;; If x[i] = y[j], return the all ones digit (as two halves).
;;; Otherwise, compute floor x[i]x[i-1] / y[j].
(defx8632lapfunction %floor-99 ((x-stk 8) (xidx 4) #|(ra 0)|#
				(yptr arg_y) (yidx arg_z))
  (pop (% temp1))
  (pop (% imm0))
  (pop (% temp0))
  (discard-reserved-frame)
  (push (% temp1))
  (movl (% imm0) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp0) (% temp1)) (% imm0)) ;x[i]
  (cmpl (% imm0) (@ x8632::misc-data-offset (% yptr) (% yidx)))	  ;y[j]
  (jne @more)
  (pushl ($ '#xffff))
  (pushl ($ '#xffff))
  (lea (@ '2 (% esp)) (% temp0))
  (set-nargs 2)
  (jmp-subprim .SPvalues)
  @more
  (mark-as-imm edx)			;aka temp1 (contains a fixnum)
  (movl (@ (- x8632::misc-data-offset 4) (% temp0) (% temp1)) (% eax)) ;low
  (movl (@ x8632::misc-data-offset (% temp0) (% temp1)) (% edx))    ;high digit
  (divl (@ x8632::misc-data-offset (% yptr) (% yidx)))
  (mark-as-node edx)
  ;; extract and push high half of quotient
  (movl ($ (- #x10000)) (% arg_y))
  (andl (% eax) (% arg_y))
  (shrl ($ (- 16 x8632::fixnumshift)) (% arg_y))
  (push (% arg_y))
  ;; extract and push low half
  (andl ($ #xffff) (% eax))
  (shll ($ x8632::fixnumshift) (% eax))
  (push (% eax))
  (set-nargs 2)
  (lea (@ '2 (% esp)) (% temp0))
  (jmp-subprim .SPvalues))

;;; x * y + carry
(defx8632lapfunction %multiply-and-add-1 ((x-high 16)
					  (x-low 12)
					  (y-high 8)
					  (y-low 4)
					  #|(ra 0)|#
					  (carry-in-high arg_y)
					  (carry-in-low arg_z))
  (movl (@ x-high (% esp)) (% temp0))
  (movl (@ x-low (% esp)) (% temp1))
  (compose-digit temp0 temp1 imm0)
  (movd (% imm0) (% mm0))
  (movl (@ y-high (% esp)) (% temp0))
  (movl (@ y-low (% esp)) (% temp1))
  (compose-digit temp0 temp1 imm0)
  (movd (% imm0) (% mm1))
  (pmuludq (% mm1) (% mm0))		;x * y
  (compose-digit arg_y arg_z imm0)
  (movd (% imm0) (% mm1))
  (paddq (% mm1) (% mm0))		;add in carry digit
  (movq (% mm0) (% mm1))
  (psrlq ($ 32) (% mm1))		;resultant carry digit
  ;; clean up stack
  (pop (% temp0))
  (addl ($ '6) (% esp))
  (push (% temp0))
  ;; return (values carry-h carry-l result-h result-l) 
  (movl (% esp) (% temp0))
  (movd (% mm1) (% imm0))
  (shrl ($ 16) (% imm0))
  (shll ($ x8632::fixnumshift) (% imm0)) ;carry-h
  (push (% imm0))
  (movd (% mm1) (% imm0))
  (shll ($ 16) (% imm0))
  (shrl ($ (- 16 x8632::fixnumshift)) (% imm0)) ;carry-l
  (push (% imm0))
  (movd (% mm0) (% imm0))
  (shrl ($ 16) (% imm0))
  (shll ($ x8632::fixnumshift) (% imm0)) ;result-h
  (push (% imm0))
  (movd (% mm0) (% imm0))
  (shll ($ 16) (% imm0))
  (shrl ($ (- 16 x8632::fixnumshift)) (% imm0)) ;result-l
  (push (% imm0))
  (set-nargs 4)
  (jmp-subprim .SPvalues))

;;; Copy the limb SRC points to to where DEST points.
(defx8632lapfunction copy-limb ((src arg_y) (dest arg_z))
  (int ($ 3)))

;;; Return T iff LIMB contains 0.
(defx8632lapfunction limb-zerop ((limb arg_z))
  (int ($ 3)))

;;; Return -1,0,1 according to whether the contents of Y are
;;; <,=,> the contents of Z.
(defx8632lapfunction compare-limbs ((y arg_y) (z arg_z))
  (int ($ 3)))

;;; Add a fixnum to the limb LIMB points to.  Ignore overflow.
(defx8632lapfunction add-fixnum-to-limb ((fixnum arg_y) (limb arg_z))
  (int ($ 3)))

;;; Store a fixnum value where LIMB points.
(defx8632lapfunction copy-fixnum-to-limb ((fixnum arg_y) (limb arg_z))
  (int ($ 3)))

;;; Increment a "LIMB VECTOR" (bignum) by a small amount.  The caller
;;; knows that carries will only propagate for a word or two.
(defx8632lapfunction mpn-incr-u ((limb arg_y) (fixby arg_z))
  (int ($ 3)))

;;; Store XP-YP at WP; return carry (0 or 1).
;;; wp, xp, yp: word-aligned, unboxed ptrs (fixnums)
;;; size: boxed fixnum
;;; returns boxed carry
(defx8632lapfunction mpn-sub-n ((wp 8) (xp 4) #|(ra 0)|#
				(yp arg_y) (size arg_z))
  (int ($ 3)))

;;; Store XP+YP at WP; return carry (0 or 1).
;;; wp, xp, yp = word-aligned, unboxed macptrs (fixnums).
;;; size = boxed fixnum
;;; result = boxed carry
(defx8632lapfunction mpn-add-n ((wp 8) (xp 4) #|(ra 0)|#
				(yp arg_y) (size arg_z))
  (int ($ 3)))

;;; Add the single limb LIMB to S1P (propagating carry.)  Store the
;;; result at RP.  RP and S1P may be the same place, so check for
;;; that and do nothing after carry stops propagating.  Return carry.
(defx8632lapfunction mpn-add-1 ((rp-offset 8) (s1p 4) #|(ra 0)|#
				(size arg_y) (limb arg_z))
  (int ($ 3)))

;;; Multiply the limb vector S1 by the single limb at LIMBPTR, storing
;;; the result at RES.  Store the "carry out" (high word of last 64-bit
;;; partial product) at the limb RESULT.
;;; res, s1, limbptr, result:
;;;   unboxed, word-aligned ptrs (fixnums).  size: boxed fixnum
;;; It'd be hard to transliterate the GMP code here; the GMP version
;;; uses lots more immediate registers than we can easily use in LAP
;;; (and is much more aggressively pipelined).
(defx8632lapfunction mpn-mul-1 ((res-offset 12)
				(s1-offset 8)
				(size 4)
				#|(ra 0)|#
				(limbptr arg_y)
				(result arg_z))
  (int ($ 3)))

;;; multiply s1*limb and add result to res
;;; res, s1, limbptr, result:
;;;   unboxed, word-aligned ptrs (fixnums).
;;; size: boxed fixnum
;;; limbptr: source "limb".
;;; result: carry out (high word of product).
(defx8632lapfunction mpn-addmul-1 ((res-offset 12)
				   (s1-offset 8)
				   (size 4)
				   #|(ra 0)|#
				   (limbptr arg_y)
				   (result arg_z))
  (int ($ 3)))

;;; Multiply the UN-word limb vector at UP and the VN-word limb vector
;;; at VP, store the result at RP.
(defx8632lapfunction mpn-mul-basecase ((rp-offset 12)
				       (up-offset 8)
				       (un 4)
				       #|(ra 0)|#
				       (vp arg_y)
				       (vn arg_z))
  (int ($ 3)))

;;; left-shift src by 1 bit, storing result at res.  Return
;;; the bit that was shifted out.
(defx8632lapfunction mpn-lshift-1 ((resptr 4) #|(ra 0)|#
				   (s1ptr arg_y) (size-arg arg_z))
  (int ($ 3)))

;;; Do a 32x32=64 unsigned multiply of the words at X and Y.  Store
;;; result (low word first) at RESULT.
(defx8632lapfunction umulppm ((x 4) #|(ra 0)|# (y arg_y) (result arg_z))
  (int ($ 3)))

(defx8632lapfunction %fixnum-to-bignum-set ((bignum arg_y) (fixnum arg_z))
  (unbox-fixnum fixnum imm0)
  (movl (% imm0) (@ x8632::misc-data-offset (% bignum)))
  (single-value-return))

(defx8632lapfunction bignum-negate-loop-really ((bignum 4) #|(ra 0)|# 
						(len arg_y) (result arg_z))
  (mark-as-imm edx)			;aka %temp1
  (unbox-fixnum arg_y edx)
  (movl (@ bignum (% esp)) (% arg_y))
  (xorl (% temp0) (% temp0))
  (stc)
  @loop
  (movl (@ x8632::misc-data-offset (% arg_y) (% temp0)) (% imm0))
  (not (% imm0))
  (adc ($ 0) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% arg_z) (% temp0)))
  (lea (@ x8632::node-size (% temp0)) (% temp0))
  (decl (% edx))			;preserves carry flag
  (jg @loop)
  ;; return carry
  (setc (% imm0.b))
  (movzbl (% imm0.b) (% imm0))
  (box-fixnum imm0 arg_z)
  (mark-as-node edx)
  (single-value-return 3))

(defx8632lapfunction %bignum-set ((bignum 8) (i 4) #|(ra 0)|#
				  (high arg_y) (low arg_z))
  (compose-digit high low imm0)
  (movl (@ bignum (% esp)) (% arg_z))
  (movl (@ i (% esp)) (% arg_y))
  (movl (% imm0) (@ x8632::misc-data-offset (% arg_z) (% arg_y)))
  (single-value-return 4))

