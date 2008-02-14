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

(eval-when (:compile-toplevel :execute)
  #+x8632-target
  (require "X8632-ARCH")
  #+x8664-target
  (require "X8664-ARCH")
  (require "X86-LAPMACROS"))



#+x8664-target
(progn
;;; None of the stores in here can be intergenerational; the vector
;;; is known to be younger than the initial value
(defx86lapfunction %init-gvector ((len arg_x) (value arg_y) (vector arg_z))
  (jmp @test)
  @loop
  (movq (% value) (@ x8664::misc-data-offset (% vector) (% len)))
  @test
  (subq ($ x8664::fixnumone) (% len))
  (jns @loop)
  (single-value-return))

;;; "val" is either a fixnum or a uvector with 64-bits of data
;;; (small bignum, DOUBLE-FLOAT).
(defx86lapfunction %%init-ivector64 ((len arg_x) (value arg_y) (vector arg_z))
  (unbox-fixnum value imm0)
  (testb ($ x8664::fixnummask) (%b value))
  (je @test)
  (movq (@ x8664::misc-data-offset (% value)) (% imm0))
  (jmp @test)
  @loop
  (movq (% imm0) (@ x8664::misc-data-offset (% vector) (% len)))
  @test
  (subq ($ x8664::fixnumone) (% len))
  (jns @loop)
  (single-value-return))

(defun %init-ivector64 (typecode len val uvector)
  (declare (type (mod 256) typecode))
  (%%init-ivector64 len
                    (case typecode
                      (#.x8664::subtag-fixnum-vector
                       (require-type val 'fixnum))
                      (#.x8664::subtag-double-float-vector
                       (if (typep val 'double-float)
                         val
                         (require-type val 'double-float)))
                      (#.x8664::subtag-s64-vector
                       (require-type val '(signed-byte 64)))
                      (#.x8664::subtag-u64-vector
                       (require-type val '(unsigned-byte 64)))
                      (t (report-bad-arg uvector
                                         '(or (simple-array fixnum (*))
                                           (simple-array double-float (*))
                                           (simple-array (signed-byte 64) (*))
                                           (simple-array (unsigned-byte 64) (*))))))
                    uvector))
  

(eval-when (:compile-toplevel :execute)
  (declaim (inline %init-ivector-u32)))

(defun %init-ivector-u32 (len u32val uvector)
  (declare (type index len)
           (type (unsigned-byte 32) u32val)
           (type (simple-array (unsigned-byte 32) (*)) uvector)
           (optimize (speed 3) (safety 0)))
  (dotimes (i len uvector)
    (setf (aref uvector i) u32val)))

(eval-when (:compile-toplevel :execute)
  (declaim (inline %init-ivector-u16)))

(defun %init-ivector-u16 (len val uvector)
  (declare (type index len)
           (type (unsigned-byte 16) val)
           (type (simple-array (unsigned-byte 16) (*)) uvector)
           (optimize (speed 3) (safety 0)))
  (dotimes (i len uvector)
    (setf (aref uvector i) val)))

                              

(defun %init-ivector32 (typecode len val uvector)
  (declare (type (unsigned-byte 32) typecode)
           (type index len))
  (let* ((u32val (case typecode
                   (#.x8664::subtag-s32-vector
                    (logand (the (signed-byte 32)
                              (require-type val '(signed-byte 32)))
                            #xffffffff))
                   (#.x8664::subtag-single-float-vector
                    (single-float-bits (require-type val 'single-float)))
                   (#.x8664::subtag-simple-base-string
                    (char-code val))
                   (t
                    (require-type val '(unsigned-byte 32))))))
    (declare (type (unsigned-byte 32) u32val))
    (%init-ivector-u32 len u32val uvector)))

(defun %init-misc (val uvector)
  (let* ((len (uvsize uvector))
         (typecode (typecode uvector))
         (fulltag (logand x8664::fulltagmask typecode)))
    (declare (type index len)
             (type (unsigned-byte 8) typecode)
             (type (mod 16) fulltag))
    (if (or (= fulltag x8664::fulltag-nodeheader-0)
            (= fulltag x8664::fulltag-nodeheader-1))
      (%init-gvector len val uvector)
      (if (= fulltag x8664::ivector-class-64-bit)
        (%init-ivector64 typecode len val uvector)
        (if (= fulltag x8664::ivector-class-32-bit)
          (%init-ivector32 typecode len val uvector)
          ;; Value must be a fixnum, 1, 8, 16 bits
          (case typecode
            (#.x8664::subtag-u16-vector
             (%init-ivector-u16 len
                                (require-type val '(unsigned-byte 16))
                                uvector))
            (#.x8664::subtag-s16-vector
             (%init-ivector-u16 len
                                (logand (the (signed-byte 16)
                                          (require-type val '(unsigned-byte 16)))
                                        #xffff)
                                uvector))
            (#.x8664::subtag-u8-vector
             (let* ((v0 (require-type val '(unsigned-byte 8)))
                    (l0 (ash (the fixnum (1+ len)) -1)))
               (declare (type (unsigned-byte 8) v0)
                        (type index l0))
               (%init-ivector-u16 l0
                                  (logior (the (unsigned-byte 16) (ash v0 8))
                                          v0)
                                  uvector)))
            (#.x8664::subtag-s8-vector
             (let* ((v0 (logand #xff
                                (the (signed-byte 8)
                                  (require-type val '(signed-byte 8)))))
                    (l0 (ash (the fixnum (1+ len)) -1)))
               (declare (type (unsigned-byte 8) v0)
                        (type index l0))
               (%init-ivector-u16 l0
                                  (logior (the (unsigned-byte 16) (ash v0 8))
                                          v0)
                                  uvector)))
            (#.x8664::subtag-bit-vector
               (let* ((v0 (case val
                            (1 -1)
                            (0 0)
                            (t (report-bad-arg val 'bit))))
                      (l0 (ash (the fixnum (+ len 63)) -6)))
                 (declare (type (unsigned-byte 8) v0)
                          (type index l0))
                 (%%init-ivector64  l0 v0 uvector)))
            (t (report-bad-arg uvector
                               '(or simple-bit-vector
                                   (simple-array (signed-byte 8) (*))
                                   (simple-array (unsigned-byte 8) (*))
                                   (simple-array (signed-byte 16) (*))
                                   (simple-array (unsigned-byte 16) (*)))))))))))
             

)

#-x8664-target
(defun %init-misc (val uvector)
  (dotimes (i (uvsize uvector) uvector)
    (setf (uvref uvector i) val)))
          

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
(defx86lapfunction %array-header-data-and-offset ((a arg_z))
  (let ((offset arg_y)
        (temp temp1))
    (movq (% rsp) (% temp0))
    (movl ($ '0) (%l offset))
    (movq (% a) (% temp))
    @loop
    (movq (@ target::arrayH.data-vector (% temp)) (% a))
    (extract-subtag a imm0)
    (addq (@ target::arrayH.displacement (% temp)) (% offset))
    (rcmp (% imm0) ($ target::subtag-vectorH))
    (movq (% a) (% temp))
    (jle @loop)
    (push (% a))
    (push (% offset))
    (set-nargs 2)
    (jmp-subprim  .SPvalues)))



(defx86lapfunction %boole-clr ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq ($ 0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-set ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq ($ -1) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-1 ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-2 ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-c1 ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-c2 ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-and ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (andq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-ior ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (orq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-xor ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
n  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (xorq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-eqv ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (xorq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-nand ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (andq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-nor ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (orq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-andc1 ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (notq (% imm0))
  (andq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-andc2 ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (andq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-orc1 ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (notq (% imm0))
  (orq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

(defx86lapfunction %boole-orc2 ((idx 8) #|(ra 0)|# (b0 arg_x) (b1 arg_y) (dest arg_z))
  (movq (@ idx (% rsp)) (% temp0))
  (movq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (orq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return 3))

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
  (let* ((f (svref *simple-bit-boole-functions* op)))
    (dotimes (i (ash (the fixnum (+ (length result) 63)) -6) result)
      (funcall f i b1 b2 result))))

(defx86lapfunction %aref2 ((array arg_x) (i arg_y) (j arg_z))
  (check-nargs 3)
  (jmp-subprim .SParef2))

(defx86lapfunction %aref3 ((array 8) #|(ra 0)|# (i arg_x) (j arg_y) (k arg_z))
  (check-nargs 4)
  (pop (% ra0))
  (pop (% temp0))
  (discard-reserved-frame)
  (push (% ra0))
  (jmp-subprim .SParef3))

(defx86lapfunction %aset2 ((array 8) #|(ra 0)|# (i arg_x) (j arg_y) (newval arg_z))
  (check-nargs 4)
  (pop (% ra0))
  (pop (% temp0))
  (discard-reserved-frame)
  (push (% ra0))
  (jmp-subprim .SPaset2))

(defx86lapfunction %aset3 ((array 16) (i 8) #|(ra 0)|# (j arg_x) (k arg_y) (newval arg_z))
  (check-nargs 5)
  (pop (% ra0))
  (pop (% temp0))
  (pop (% temp1))
  (discard-reserved-frame)
  (push (% ra0))
  (jmp-subprim .SPaset3))

