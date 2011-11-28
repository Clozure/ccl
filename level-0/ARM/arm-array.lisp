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
  (require "ARM-ARCH")
  (require "ARM-LAPMACROS"))


;;; Users of this shouldn't make assumptions about return value.


(eval-when (:compile-toplevel :execute)
;;; Assumptions made by %init-misc
  (assert (and (< arm::max-32-bit-ivector-subtag
                  arm::max-8-bit-ivector-subtag
                  arm::max-16-bit-ivector-subtag)
               (eql arm::max-32-bit-ivector-subtag arm::subtag-simple-base-string)
               (eql arm::max-16-bit-ivector-subtag arm::subtag-s16-vector)
               (eql arm::max-8-bit-ivector-subtag 223))))


(defarmlapfunction %init-misc ((val arg_y)
                               (miscobj arg_z))
  (getvheader imm0 miscobj)
  (bic temp1 imm0 (:$ arm::subtag-mask))
  (movs temp1 (:lsr temp1 (:$ (- arm::num-subtag-bits arm::fixnumshift))))
  (extract-lowbyte imm2 imm0)
  (extract-fulltag imm1 imm0)
  (bxeq lr)
  (cmp imm1 (:$ arm::fulltag-nodeheader))
  (bne @ivector)
  (mov imm1 (:$ arm::misc-data-offset))
  @node-loop
  (subs temp1 temp1 '1)
  (str val (:@ miscobj imm1))
  (add imm1 imm1 '1)
  (bne @node-loop)
  (bx lr)
  @ivector
  (build-lisp-frame imm0)
  (mov imm1 (:$ arm::misc-data-offset))
  (mov imm2 (:lsr imm2 (:$ 3)))
  (bl @dispatch)
  (b @u32)                              ;bignum,0
  (b @u32)                              ;single-float
  (b @u32)                              ;double-float
  (b @u32)                              ;macptr
  (b @u32)                              ;dead-macptr
  (b @u32)                              ;code-vector, 5
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @bad)
  (b @single-float-vector)              ;20
  (b @u32)
  (b @s32)
  (b @fixnum)
  (b @string)
  (b @u8)
  (b @s8)
  (b @string8)
  (b @u16)
  (b @s16)
  (b @double-float-vector)
  (b @bit-vector)
  @dispatch
  (add pc lr (:lsl imm2 (:$ arm::word-shift)))
  @u32
  ;; Non-negative fixnum, positive one-digit bignum, two-digit bignum with
  ;; high word 0.
  (tst val (:$ #x80000003))
  (moveq imm0 (:lsr val (:$ arm::fixnumshift)))
  (beq @word-set-loop)
  (extract-typecode imm0 val)
  (cmp imm0 (:$ arm::subtag-bignum))
  (bne @bad)
  (getvheader imm0 val)
  (header-size imm0 imm0)
  (cmp imm0 (:$ 1))
  (bne @u32-two-digit)
  (ldr imm0 (:@ val (:$ arm::misc-data-offset)))
  (cmp imm0 (:$ 0))
  (bmi @bad)
  (b @word-set-loop)
  @u32-two-digit
  (cmp imm0 (:$ 2))
  (ldr imm0 (:@ val (:$ (+ arm::misc-data-offset 4))))
  (bne @bad)
  (cmp imm0 (:$ 0))
  (ldr imm0 (:@ val (:$ arm::misc-data-offset)))  
  (bne @bad)
  (b @word-set-loop)
  @s32
  ;; A fixnum or a 1-digit bignum.
  (ands imm0 val (:$ arm::tag-mask))
  (moveq imm0 (:asr val (:$ arm::fixnumshift)))
  (beq @word-set-loop)
  (cmp imm0 (:$ arm::tag-misc))
  (ldrbeq imm0 (:@ val (:$ arm::misc-subtag-offset)))
  (cmp imm0 (:$ arm::subtag-bignum))
  (bne @bad)
  (getvheader imm0 val)
  (header-size imm0 imm0)
  (cmp imm0 (:$ 1))
  (ldr imm0 (:@ val (:$ arm::misc-data-offset)))
  (bne @bad)
  @word-set-loop
  (subs temp1 temp1 '1)
  (str imm0 (:@ miscobj imm1))
  (add imm1 imm1 '1)
  (bne @word-set-loop)
  (return-lisp-frame)
  @string
  (extract-lowbyte imm0 val)
  (cmp imm0 (:$ arm::subtag-character))
  (mov imm0 (:lsr val (:$ arm::charcode-shift)))
  (beq @word-set-loop)
  @bad
  (mov arg_x  '#.$xnotelt)
  (set-nargs 3)
  (sploadlr .SPksignalerr)
  (blx lr)
  @fixnum
  (tst val (:$ arm::fixnum-mask))
  (unbox-fixnum imm0 val)
  (beq @word-set-loop)
  (b @bad)
  @single-float-vector
  (extract-subtag imm0 val)
  (cmp imm0 (:$ arm::subtag-single-float))
  (bne @bad)
  (ldr imm0 (:@ val (:$ arm::misc-data-offset)))
  (b @word-set-loop)
  @u16
  (mov imm0 (:lsl val (:$ (- 16 arm::fixnumshift))))
  (mov imm0 (:lsr imm0 (:$ 16)))
  (cmp val (:lsl imm0 (:$ arm::fixnumshift)))
  (bne @bad)
  @set16
  (orr imm0 imm0 (:lsl imm0 (:$ 16)))
  (add imm2 temp1 '1)
  (mov temp1 (:$ (- arm::fixnumone)))
  (and temp1 temp1 (:lsr imm2 (:$ 1)))
  (b @word-set-loop)
  @s16
  (mov imm0 (:lsl val (:$ (- 16 arm::fixnumshift))))
  (mov imm0 (:asr imm0 (:$ 16)))
  (cmp val (:lsl imm0 (:$ arm::fixnumshift)))
  (bne @bad)
  (b @set16)
  @u8
  (mov imm0 (:lsl val (:$ (- 24 arm::fixnumshift))))
  (mov imm0 (:lsr imm0 (:$ 24)))
  (cmp val (:lsl imm0 (:$ arm::fixnumshift)))  
  (bne @bad)
  @set8
  (orr imm0 imm0 (:lsl imm0 (:$ 8)))
  (orr imm0 imm0 (:lsl imm0 (:$ 16)))
  (unbox-fixnum imm2 temp1)
  (add imm2 imm2 (:$ 3))
  (mov imm2 (:lsr imm2 (:$ 2)))
  (box-fixnum temp1 imm2)
  (b @word-set-loop)
  @s8
  (mov imm0 (:lsl val (:$ (- 24 arm::fixnumshift))))
  (mov imm0 (:asr imm0 (:$ 24)))
  (cmp val (:lsl imm0 (:$ arm::fixnumshift)))  
  (beq @set8)
  (b @bad)
  @string8
  (extract-lowbyte imm0 val)
  (cmp imm0 (:$ arm::subtag-character))
  (mov imm0 (:lsr imm0 (:$ arm::charcode-shift)))
  (bne @bad)
  (cmp imm0 (:$ #xff))
  (bls @set8)
  (b @bad)
  @bit-vector
  (cmp val '1)
  (moveq imm0 (:$ -1))
  (movne imm0 (:$ 0))
  (bhi @bad)
  (unbox-fixnum imm2 temp1)
  (add imm2 imm2 (:$ 31))
  (mov imm2 (:lsr imm2 (:$ 5)))
  (box-fixnum temp1 imm2)
  (b @word-set-loop)
  @double-float-vector
  (extract-typecode imm0 val)
  (cmp imm0 (:$ arm::subtag-double-float))
  (bne @bad)
  (ldrd imm0 (:@ val (:$ arm::double-float.value)))
  (mov imm2 (:$ arm::misc-dfloat-offset))
  @double-float-loop
  (subs temp1 temp1 '1)
  (strd imm0 (:@ miscobj imm2))
  (add imm2 imm2 (:$ 8))
  (bne @double-float-loop)
  (return-lisp-frame imm0))



;;; Make a new vector of size newsize whose subtag matches that of oldv-arg.
;;; Blast the contents of the old vector into the new one as quickly as
;;; possible; leave remaining elements of new vector undefined (0).
;;; Return new-vector.
(defun %extend-vector (start oldv newsize)
  (declare (fixnum start))
  (let* ((new (%alloc-misc newsize (typecode oldv)))
         (oldsize (uvsize oldv)))
    (declare (fixnum oldsize))
    (do* ((i 0 (1+ i))
          (j start (1+ j)))
         ((= i oldsize) new)
      (declare (fixnum i j))
      (setf (uvref new j) (uvref oldv i)))))




;;; argument is a vector header or an array header.  Or else.
(defarmlapfunction %array-header-data-and-offset ((a arg_z))
  (let ((offset arg_y)
        (disp arg_x)
        (temp temp0))
    (mov offset (:$ 0))
    (mov temp a)
    @loop
    (ldr a (:@ temp (:$ target::arrayH.data-vector)))
    (ldrb imm0 (:@ a (:$ target::misc-subtag-offset)))
    (cmp imm0 (:$ target::subtag-vectorH))
    (ldr disp (:@ temp (:$ target::arrayH.displacement)))
    (mov temp a)
    (add offset offset disp)
    (ble  @loop)
    (mov temp0 vsp)
    (vpush1 a)
    (vpush1 offset)
    (set-nargs 2)
    (spjump .SPvalues)))

(defarmlapfunction %boole-clr ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (mov imm0 (:$ 0))
  (b @test)
  @loop
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-set ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (mov imm0 (:$ -1))
  (b @test)
  @loop
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-1 ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-2 ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b1 imm2))
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-c1 ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (mvn imm0 imm0)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-c2 ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b1 imm2))
  (mvn imm0 imm0)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-and ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (and imm0 imm0 imm1)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-ior ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (orr imm0 imm0 imm1)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-xor ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (eor imm0 imm0 imm1)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-eqv ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (eor imm0 imm0 imm1)
  (mvn imm0 imm0)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-nand ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (and imm0 imm0 imm1)
  (mvn imm0 imm0)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-nor ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (orr imm0 imm0 imm1)
  (mvn imm0 imm0)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-andc1 ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (bic imm0 imm1 imm0)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-andc2 ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (bic imm0 imm0 imm1)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-orc1 ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (mvn imm0 imm0)
  (orr imm0 imm0 imm1)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defarmlapfunction %boole-orc2 ((len 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (vpop1 temp0)
  (mov imm2 (:$ arm::misc-data-offset))
  (b @test)
  @loop
  (ldr imm0 (:@ b0 imm2))
  (ldr imm1 (:@ b1 imm2))
  (mvn imm1 imm1)
  (orr imm0 imm0 imm1)
  (str imm0 (:@ dest imm2))
  (add imm2 imm2 (:$ 4))
  @test
  (subs temp0 temp0 '1)
  (bpl @loop)
  (bx lr))

(defparameter *simple-bit-boole-functions* ())

(setq *simple-bit-boole-functions*
      (vector
       #'%boole-clr
       #'%boole-set
       #'%boole-1
       #'%boole-2
       #'%boole-c1
       #'%boole-c2
       #'%boole-and
       #'%boole-ior
       #'%boole-xor
       #'%boole-eqv
       #'%boole-nand
       #'%boole-nor
       #'%boole-andc1
       #'%boole-andc2
       #'%boole-orc1
       #'%boole-orc2))

(defun %simple-bit-boole (op b1 b2 result)
  (funcall (svref *simple-bit-boole-functions* op)
           (ash (the fixnum (+ (length result) 31)) -5)
           b1
           b2
           result))


(defarmlapfunction %aref2 ((array arg_x) (i arg_y) (j arg_z))
  (check-nargs 3)
  (spjump .SParef2))

(defarmlapfunction %aref3 ((array 0) (i arg_x) (j arg_y) (k arg_z))
  (check-nargs 4)
  (vpop1 temp0)
  (spjump .SParef3))


(defarmlapfunction %aset2 ((array 0) (i arg_x) (j arg_y) (newval arg_z))
  (check-nargs 4)
  (vpop1 temp0)
  (spjump .SPaset2))

(defarmlapfunction %aset3 ((array #.target::node-size) (i 0) (j arg_x) (k arg_y)  (newval arg_z))
  (check-nargs 5)
  (vpop1 temp0)
  (vpop1 temp1)
  (spjump .SPaset3))
  

