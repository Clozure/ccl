;;; -*- Mode: Lisp; Package: CCL -*-
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

;;; level-0;ARM;arm-misc.lisp


(in-package "CCL")

  
;;; Copy N bytes from pointer src, starting at byte offset src-offset,
;;; to ivector dest, starting at offset dest-offset.
;;; It's fine to leave this in lap.
;;; Depending on alignment, it might make sense to move more than
;;; a byte at a time.
;;; Does no arg checking of any kind.  Really.


(defun %copy-ptr-to-ivector (src src-byte-offset dest dest-byte-offset nbytes)
  (declare (fixnum src-byte-offset dest-byte-offset nbytes)
           (optimize (speed 3) (safety 0)))
  (let* ((ptr-align (logand 7 (%ptr-to-int src))))
    (declare (type (mod 8) ptr-align))
    (if (and (>= nbytes 32)
             (= 0 (logand nbytes 3))
             (= 0 (logand dest-byte-offset 3))
             (= 0 (logand (the fixnum (+ ptr-align src-byte-offset)) 3)))
      (%copy-ptr-to-ivector-32bit src src-byte-offset dest dest-byte-offset nbytes)
      (%copy-ptr-to-ivector-8bit src src-byte-offset dest dest-byte-offset nbytes))
    dest))
            
(defarmlapfunction %copy-ptr-to-ivector-8bit ((src (* 1 arm::node-size) )
                                               (src-byte-offset 0) 
                                               (dest arg_x)
                                               (dest-byte-offset arg_y)
                                               (nbytes arg_z))
  (let ((src-reg imm0)
        (src-byteptr temp2)
        (src-node-reg temp0)
        (dest-byteptr imm2)
        (val imm1)
        (node-temp temp1))
    (cmp nbytes (:$ 0))
    (ldr src-node-reg (:@ vsp (:$ src)))
    (macptr-ptr src-reg src-node-reg)
    (ldr src-byteptr (:@ vsp (:$ src-byte-offset)))
    (add src-reg src-reg (:asr src-byteptr (:$ arm::fixnumshift)))
    (unbox-fixnum dest-byteptr dest-byte-offset)
    (add dest-byteptr dest-byteptr (:$ arm::misc-data-offset))
    (b @test)
    @loop
    (subs nbytes nbytes '1)
    (ldrb val (:@+ src-reg (:$ 1)))
    (strb val (:@ dest dest-byteptr))
    (add dest-byteptr dest-byteptr (:$ 1))
    @test
    (bne  @loop)
    (mov arg_z dest)
    (add vsp vsp '2)
    (bx lr)))

;;; Everything's aligned OK and NBYTES is a multiple of 4.
(defarmlapfunction %copy-ptr-to-ivector-32bit ((src (* 1 arm::node-size) )
                                               (src-byte-offset 0) 
                                               (dest arg_x)
                                               (dest-byte-offset arg_y)
                                               (nbytes arg_z))
  (add imm1 vsp (:$ (* 2 arm::node-size)))
  (build-lisp-frame imm0 imm1)
  (add lr dest (:$ arm::misc-data-offset))
  (add lr lr (:asr dest-byte-offset (:$ arm::fixnumshift)))
  (ldr temp0 (:@ vsp (:$ src)))
  (ldr imm1 (:@ vsp (:$ src-byte-offset)))
  (macptr-ptr imm0 temp0)
  (add imm0 imm0 (:asr imm1 (:$ arm::fixnumshift)))
  (b @test32)
  @loop32
  (fldmias s0 (:! imm0) 8)
  (fstmias s0 (:! lr) 8)
  (sub nbytes nbytes '32)
  @test32
  (cmp nbytes '32)
  (bge @loop32)
  (add pc pc (:asr nbytes (:$ arm::fixnumshift)))
  (nop)
  (b @0)
  (b @4)
  (b @8)
  (b @12)
  (b @16)
  (b @20)
  (b @24)
  (b @28)
  (nop)
  @0
  (mov arg_z dest)
  (restore-lisp-frame imm0)
  (bx lr)
  @4
  (flds s0 (:@ imm0 (:$ 0)))
  (fsts s0 (:@ lr (:$ 0)))
  (b @0)
  @8
  (fldmias s0 imm0 2)
  (fstmias s0 lr 2)
  (b @0)
  @12
  (fldmias s0 imm0 3)
  (fstmias s0 lr 3)
  (b @0)
  @16
  (fldmias s0 imm0 4)
  (fstmias s0 lr 4)
  (b @0)
  @20
  (fldmias s0 imm0 5)
  (fstmias s0 lr 5)
  (b @0)
  @24
  (fldmias s0 imm0 6)
  (fstmias s0 lr 6)
  (b @0)
  @28
  (fldmias s0 imm0 7)
  (fstmias s0 lr 7)
  (b @0))
  

(defun %copy-ivector-to-ptr (src src-byte-offset dest dest-byte-offset nbytes)
  (declare (fixnum src-byte-offset dest-byte-offset nbytes)
           (optimize (speed 3) (safety 0)))
  (let* ((ptr-align (logand (the (unsigned-byte 32)(%ptr-to-int dest)) 7)))
    (declare (type (mod 8) ptr-align))
    (if (or (< nbytes 32)
            (not (= 0 (logand nbytes 3)))
            (not (= 0 (logand src-byte-offset 3)))
            (not (= 0 (logand (the fixnum (+ ptr-align dest-byte-offset)) 3))))
      (%copy-ivector-to-ptr-8bit src src-byte-offset dest dest-byte-offset nbytes)
      (%copy-ivector-to-ptr-32bit src src-byte-offset dest dest-byte-offset nbytes))
    dest))

(defarmlapfunction %copy-ivector-to-ptr-8bit ((src (* 1 arm::node-size))
                                              (src-byte-offset 0)
                                              (dest arg_x)
                                              (dest-byte-offset arg_y)
                                              (nbytes arg_z))
  (ldr temp0 (:@ vsp (:$ src)))
  (cmp nbytes (:$ 0))
  (ldr imm0 (:@ vsp (:$ src-byte-offset)))
  (unbox-fixnum imm0 imm0)
  (add imm0 imm0 (:$ arm::misc-data-offset))
  (macptr-ptr imm1 dest)
  (add imm1 imm1 (:asr dest-byte-offset (:$ arm::fixnumshift)))
  (b @test)
  @loop
  (subs nbytes nbytes '1)
  (ldrb imm2 (:@ temp0 imm0))
  (add imm0 imm0 (:$ 1))
  (strb imm2 (:@+ imm1 (:$ 1)))
  @test
  (bne @loop)
  (mov arg_z dest)
  (add vsp vsp '2)
  (bx lr))

;;; Everything's aligned OK and NBYTES is a multiple of 4.
(defarmlapfunction %copy-ivector-to-ptr-32bit ((src (* 1 arm::node-size) )
                                               (src-byte-offset 0) 
                                               (dest arg_x)
                                               (dest-byte-offset arg_y)
                                               (nbytes arg_z))
  (add imm1 vsp (:$ (* 2 arm::node-size)))
  (build-lisp-frame imm0 imm1)
  (ldr temp0 (:@ vsp (:$ src)))
  (ldr imm1 (:@ vsp (:$ src-byte-offset)))
  (add lr temp0 (:$ arm::misc-data-offset))
  (add lr lr (:asr imm1 (:$ arm::fixnumshift)))
  (macptr-ptr imm0 dest)
  (add imm0 imm0 (:asr dest-byte-offset (:$ arm::fixnumshift)))
  (b @test32)
  @loop32
  (fldmias s0 (:! lr) 8)
  (fstmias s0 (:! imm0) 8)
  (sub nbytes nbytes '32)
  @test32
  (cmp nbytes '32)
  (bge @loop32)
  (add pc pc (:asr nbytes (:$ arm::fixnumshift)))
  (nop)
  (b @0)
  (b @4)
  (b @8)
  (b @12)
  (b @16)
  (b @20)
  (b @24)
  (b @28)
  (nop)
  @0
  (mov arg_z dest)
  (restore-lisp-frame imm0)
  (bx lr)
  @4
  (flds s0 (:@ lr (:$ 0)))
  (fsts s0 (:@ imm0 (:$ 0)))
  (b @0)
  @8
  (fldmias s0 lr 2)
  (fstmias s0 imm0 2)
  (b @0)
  @12
  (fldmias s0 lr 3)
  (fstmias s0 imm0 3)
  (b @0)
  @16
  (fldmias s0 lr 4)
  (fstmias s0 imm0 4)
  (b @0)
  @20
  (fldmias s0 lr 5)
  (fstmias s0 imm0 5)
  (b @0)
  @24
  (fldmias s0 lr 6)
  (fstmias s0 imm0 6)
  (b @0)
  @28
  (fldmias s0 lr 7)
  (fstmias s0 imm0 7)
  (b @0))


(defun %copy-ivector-to-ivector (src src-byte-offset dest dest-byte-offset nbytes)
  (declare (fixnum src-byte-offset dest-byte-offset nbytes))
  (if (or (not (eq src dest))
          (< dest-byte-offset src-byte-offset)
          (>= dest-byte-offset (the fixnum (+ src-byte-offset nbytes))))
    (%copy-ivector-to-ivector-postincrement src src-byte-offset dest dest-byte-offset nbytes)
    (if (and (eq src dest)
             (eql src-byte-offset dest-byte-offset))
      dest
      (%copy-ivector-to-ivector-predecrement src
                                             (the fixnum (+ src-byte-offset nbytes))
                                             dest
                                             (the fixnum (+ dest-byte-offset nbytes))
                                             nbytes)))
  dest)

(defun %copy-ivector-to-ivector-postincrement (src src-byte-offset dest dest-byte-offset nbytes)
  (declare (fixnum src-byte-offset dest-byte-offset nbytes))
  
  (cond ((or (< nbytes 8)
             (not (= (logand src-byte-offset 3)
                     (logand dest-byte-offset 3))))
         (%copy-ivector-to-ivector-postincrement-8bit src src-byte-offset dest dest-byte-offset nbytes))
        (t
         (let* ((prefix-size (- 4 (logand src-byte-offset 3))))
           (declare (fixnum prefix-size))
           (unless (= 4 prefix-size)
             (%copy-ivector-to-ivector-postincrement-8bit src src-byte-offset dest dest-byte-offset prefix-size)
             (incf src-byte-offset prefix-size)
             (incf dest-byte-offset prefix-size)
             (decf nbytes prefix-size)))
         (let* ((tail-size (logand nbytes 3))
                (fullword-size (- nbytes tail-size)))
           (declare (fixnum tail-size fullword-size))
           (unless (zerop fullword-size)
             (%copy-ivector-to-ivector-postincrement-32bit src src-byte-offset dest dest-byte-offset fullword-size))
           (unless (zerop tail-size)
             (%copy-ivector-to-ivector-postincrement-8bit src (the fixnum (+ src-byte-offset fullword-size)) dest (the fixnum (+ dest-byte-offset fullword-size)) tail-size))))))

(defun %copy-ivector-to-ivector-predecrement (src src-byte-offset dest dest-byte-offset nbytes)
  (declare (fixnum src-byte-offset dest-byte-offset nbytes))
  (cond ((or (< nbytes 8)
             (not (= (logand src-byte-offset 3)
                     (logand dest-byte-offset 3))))
         (%copy-ivector-to-ivector-predecrement-8bit src src-byte-offset dest dest-byte-offset nbytes))
    (t
      (let* ((suffix-size (logand src-byte-offset 3)))
        (declare (fixnum suffix-size))
        (unless (zerop suffix-size)
          (%copy-ivector-to-ivector-predecrement-8bit src src-byte-offset dest dest-byte-offset suffix-size)
          (decf src-byte-offset suffix-size)
          (decf dest-byte-offset suffix-size)
          (decf nbytes suffix-size)))
      (let* ((head-size (logand nbytes 3))
             (fullword-size (- nbytes head-size)))
        (declare (fixnum head-size fullword-size))
        (unless (zerop fullword-size)
          (%copy-ivector-to-ivector-predecrement-32bit src src-byte-offset dest dest-byte-offset fullword-size))
        (unless (zerop head-size)
          (%copy-ivector-to-ivector-predecrement-8bit src (the fixnum (- src-byte-offset fullword-size)) dest (the fixnum (- dest-byte-offset fullword-size)) head-size))))
))

(defarmlapfunction %copy-ivector-to-ivector-postincrement-8bit ((src 4) 
                                                                (src-byte-offset 0) 
                                                                (dest arg_x)
                                                                (dest-byte-offset arg_y)
                                                                (nbytes arg_z))
  (let ((rsrc temp0)
        (scaled-src-idx imm1)
        (scaled-dest-idx imm2)
        (val imm0))
    (cmp nbytes (:$ 0))
    (vpop1 scaled-src-idx)
    (mov scaled-src-idx (:lsr scaled-src-idx (:$ arm::fixnumshift)))
    (add scaled-src-idx scaled-src-idx (:$ arm::misc-data-offset))
    (mov scaled-dest-idx (:lsr dest-byte-offset (:$ arm::fixnumshift)))
    (add scaled-dest-idx scaled-dest-idx (:$ arm::misc-data-offset))    
    (vpop1 rsrc)
    (b @test)
    @loop
    (subs nbytes nbytes '1)
    (ldrb val (:@ rsrc scaled-src-idx))
    (add scaled-src-idx scaled-src-idx (:$ 1))
    (strb val (:@ dest scaled-dest-idx))
    (add scaled-dest-idx scaled-dest-idx (:$ 1))
    @test
    (bne @loop)
    (mov arg_z dest)
    (bx lr)))

(defarmlapfunction %copy-ivector-to-ivector-postincrement-32bit ((src 4) 
                                                                 (src-byte-offset 0) 
                                                                 (dest arg_x)
                                                                 (dest-byte-offset arg_y)
                                                                 (nbytes arg_z))
  (let ((rsrc temp0)
        (scaled-src-idx imm1)
        (scaled-dest-idx imm2)
        (val imm0))
    (cmp nbytes '32)
    (vpop1 scaled-src-idx)
    (mov scaled-src-idx (:lsr scaled-src-idx (:$ arm::fixnumshift)))
    (add scaled-src-idx scaled-src-idx (:$ arm::misc-data-offset))
    (mov scaled-dest-idx (:lsr dest-byte-offset (:$ arm::fixnumshift)))
    (add scaled-dest-idx scaled-dest-idx (:$ arm::misc-data-offset))    
    (vpop1 rsrc)
    (build-lisp-frame imm0)             
    (b @test)
    @loop
    (sub nbytes nbytes '32)
    (cmp nbytes '32)
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 8)
    (add scaled-src-idx scaled-src-idx (:$ 32))
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 8)
    (add scaled-dest-idx scaled-dest-idx (:$ 32))
    @test
    (bge @loop)
    (add pc pc (:asr nbytes (:$ arm::fixnumshift)))
    (nop)
    (b @0)
    (b @4)
    (b @8)
    (b @12)
    (b @16)
    (b @20)
    (b @24)
    (b @28)
    (nop)
    @4
    (ldr val (:@ rsrc scaled-src-idx))
    (str val (:@ dest scaled-dest-idx))
    (b @0)
    @8
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 2)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 2)
    (b @0)
    @12
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 3)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 3)
    (b @0)
    @16
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 4)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 4)
    (b @0)
    @20
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 5)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 5)
    (b @0)
    @24
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 6)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 6)
    (b @0)
    @28
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 7)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 7)
    @0
    (mov arg_z dest)
    (restore-lisp-frame imm0)
    (bx lr)))

(defarmlapfunction %copy-ivector-to-ivector-predecrement-8bit ((src 4) 
                                                               (src-byte-offset 0) 
                                                               (dest arg_x)
                                                               (dest-byte-offset arg_y)
                                                               (nbytes arg_z))
  (let ((rsrc temp0)
        (scaled-src-idx imm1)
        (scaled-dest-idx imm2)
        (val imm0))
    (cmp nbytes (:$ 0))
    (vpop1 scaled-src-idx)
    (mov scaled-src-idx (:lsr scaled-src-idx (:$ arm::fixnumshift)))
    (add scaled-src-idx scaled-src-idx (:$ arm::misc-data-offset))
    (mov scaled-dest-idx (:lsr dest-byte-offset (:$ arm::fixnumshift)))
    (add scaled-dest-idx scaled-dest-idx (:$ arm::misc-data-offset))    
    (vpop1 rsrc)
    (b @test)
    @loop
    (sub scaled-src-idx scaled-src-idx (:$ 1))
    (sub scaled-dest-idx scaled-dest-idx (:$ 1))
    (subs nbytes nbytes '1)
    (ldrb val (:@ rsrc scaled-src-idx))
    (strb val (:@ dest scaled-dest-idx))
    @test
    (bne @loop)
    (mov arg_z dest)
    (bx lr)))

(defarmlapfunction %copy-ivector-to-ivector-predecrement-32bit ((src 4) 
                                                                (src-byte-offset 0) 
                                                                (dest arg_x)
                                                                (dest-byte-offset arg_y)
                                                                (nbytes arg_z))
  (let ((rsrc temp0)
        (scaled-src-idx imm1)
        (scaled-dest-idx imm2)
        (val imm0))
    (cmp nbytes (:$ 32))
    (vpop1 scaled-src-idx)
    (mov scaled-src-idx (:lsr scaled-src-idx (:$ arm::fixnumshift)))
    (add scaled-src-idx scaled-src-idx (:$ arm::misc-data-offset))
    (mov scaled-dest-idx (:lsr dest-byte-offset (:$ arm::fixnumshift)))
    (add scaled-dest-idx scaled-dest-idx (:$ arm::misc-data-offset))    
    (vpop1 rsrc)
    (build-lisp-frame imm0)             
    (b @test)
    @loop
    (sub scaled-src-idx scaled-src-idx (:$ 32))
    (sub scaled-dest-idx scaled-dest-idx (:$ 32))
    (sub nbytes nbytes '32)
    (cmp nbytes '32)
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 8)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 8)
    @test
    (bge @loop)
    (sub scaled-src-idx scaled-src-idx (:asr nbytes (:$ arm::fixnumshift)))
    (sub scaled-dest-idx scaled-dest-idx (:asr nbytes (:$ arm::fixnumshift)))
    (add pc pc (:asr nbytes (:$ arm::fixnumshift)))
    (nop)
    (b @0)
    (b @4)
    (b @8)
    (b @12)
    (b @16)
    (b @20)
    (b @24)
    (b @28)
    (nop)
    @4
    (ldr val (:@ rsrc scaled-src-idx))
    (str val (:@ dest scaled-dest-idx))
    (b @0)
    @8
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 2)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 2)
    (b @0)
    @12
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 3)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 3)
    (b @0)
    @16
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 4)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 4)
    (b @0)
    @20
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 5)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 5)
    (b @0)
    @24
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 6)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 6)
    (b @0)
    @28
    (add lr rsrc scaled-src-idx)
    (fldmias s0 lr 7)
    (add lr dest scaled-dest-idx)
    (fstmias s0 lr 7)
    @0
    (mov arg_z dest)
    (restore-lisp-frame imm0)
    (bx lr)))

;;; Unless we're sure that DEST is newly-created, we have to do this
;;; in a way that honors the write barrier.
(defun %copy-gvector-to-gvector (src src-element dest dest-element nelements)
  (declare (fixnum src-element dest-element nelements)
           (optimize (speed 3) (safety 0)))
  (if (or (not (eq src dest))
          (< dest-element src-element)
          (>= dest-element (the fixnum (+ src-element nelements))))
    (do* ()
         ((<= nelements 0) dest)
      (setf (%svref dest dest-element)
            (%svref src src-element))
      (incf dest-element)
      (incf src-element)
      (decf nelements))
    (do* ((src-element (+ src-element nelements))
          (dest-element (+ dest-element nelements)))
         ((<= nelements 0) dest)
      (declare (fixnum src-element dest-element))
      (decf src-element)
      (decf dest-element)
      (setf (%svref dest dest-element)
            (%svref src src-element))
      (decf nelements))))
  
  

(defarmlapfunction %heap-bytes-allocated ()
  (ldr imm2 (:@ rcontext (:$ arm::tcr.last-allocptr)))
  (ldr imm1 (:@ rcontext (:$ arm::tcr.total-bytes-allocated-high)))
  (ldr imm0 (:@ rcontext (:$ arm::tcr.total-bytes-allocated-low)))
  (cmp imm2 (:$ 0))
  (sub imm2 imm2 allocptr)
  (beq @go)
  (cmp allocptr (:$ -8))
  (beq @go)
  (adds imm0 imm0 imm2)
  (adc imm1 imm1 (:$ 0))
  @go
  (spjump .SPmakeu64))




(defarmlapfunction values ()
  (:arglist (&rest values))
  (vpush-argregs)
  (add temp0 nargs vsp)
  (spjump .SPvalues))

;; It would be nice if (%setf-macptr macptr (ash (the fixnum value)
;; ash::fixnumshift)) would do this inline.
(defarmlapfunction %setf-macptr-to-object ((macptr arg_y) (object arg_z))
  (check-nargs 2)
  (trap-unless-xtype= arg_y arm::subtag-macptr)
  (str arg_z (:@ arg_y (:$ arm::macptr.address)))
  (bx lr))

(defarmlapfunction %fixnum-from-macptr ((macptr arg_z))
  (check-nargs 1)
  (trap-unless-xtype= arg_z arm::subtag-macptr)
  (ldr imm0 (:@ arg_z (:$ arm::macptr.address)))
  (trap-unless-fixnum imm0)
  (mov arg_z imm0)
  (bx lr))

(defarmlapfunction %%get-unsigned-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-xtype= ptr arm::subtag-macptr)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 offset)
  (add imm2 imm2 imm1)
  (ldr imm0 (:@ imm2 (:$ 0)))
  (ldr imm1 (:@ imm2 (:$ 4)))
  (spjump .SPmakeu64))



(defarmlapfunction %%get-signed-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-xtype= ptr arm::subtag-macptr)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 offset)
  (add imm2 imm2 imm1)
  (ldr imm0 (:@ imm2 (:$ 0)))           ;low
  (ldr imm1 (:@ imm2 (:$ 4)))           ;high
  (spjump .SPmakes64))



(defarmlapfunction %%set-unsigned-longlong ((ptr arg_x)
                                            (offset arg_y)
                                            (val arg_z))
  (build-lisp-frame imm0)
  (mov fn nfn)
  (trap-unless-xtype= ptr arm::subtag-macptr) 
  (sploadlr .SPgetu64)
  (blx lr)
  (macptr-ptr imm2 ptr)
  (add imm2 imm2 (:asr offset (:$ arm::fixnumshift)))
  (str imm0 (:@ imm2 (:$ 0)))
  (str imm1 (:@ imm2 (:$ 4)))
  (return-lisp-frame imm0))



(defarmlapfunction %%set-signed-longlong ((ptr arg_x)
                                          (offset arg_y)
                                          (val arg_z))
  (build-lisp-frame imm0)
  (mov fn nfn)
  (trap-unless-xtype= ptr arm::subtag-macptr)
  (sploadlr .SPgets64)
  (blx lr)
  (macptr-ptr imm2 ptr)
  (add imm2 imm2 (:asr offset (:$ arm::fixnumshift)))
  (str imm0 (:@ imm2 (:$ 0)))
  (str imm1 (:@ imm2 (:$ 4)))
  (return-lisp-frame imm0))



(defarmlapfunction interrupt-level ()
  (ldr arg_z (:@ arm::rcontext (:$ arm::tcr.tlb-pointer)))
  (ldr arg_z (:@ arg_z (:$ arm::interrupt-level-binding-index)))
  (bx lr))




(defarmlapfunction set-interrupt-level ((new arg_z))
  (ldr imm1 (:@ arm::rcontext (:$ arm::tcr.tlb-pointer)))
  (trap-unless-fixnum new)
  (str new (:@ imm1 (:$ arm::interrupt-level-binding-index)))
  (bx lr))



(defarmlapfunction %current-tcr ()
  (mov arg_z rcontext)
  (bx lr))

(defarmlapfunction %tcr-toplevel-function ((tcr arg_z))
  (check-nargs 1)
  (cmp tcr arm::rcontext)
  (mov imm0 vsp)
  (ldr temp0 (:@ tcr (:$ arm::tcr.vs-area)))
  (ldr imm1 (:@ temp0 (:$ arm::area.high)))
  (ldrne imm0 (:@ temp0 (:$ arm::area.active)))
  (cmp imm1 imm0)
  (moveq arg_z 'nil)
  (ldrne arg_z (:@ imm1 (:$ (- arm::node-size))))
  (bx lr))

(defarmlapfunction %set-tcr-toplevel-function ((tcr arg_y) (fun arg_z))
  (check-nargs 2)
  (cmp tcr arm::rcontext)
  (mov imm0 vsp)
  (ldr temp0 (:@ tcr (:$ arm::tcr.vs-area)))
  (ldr imm1 (:@ temp0 (:$ arm::area.high)))
  (ldrne  imm0 (:@ temp0 (:$ arm::area.active)))
  (cmp imm1 imm0)
  (mov imm0 (:$ 0))
  (push1 imm0 imm1)
  (streq imm1 (:@ temp0 (:$ arm::area.active)))
  (streq imm1 (:@ tcr (:$ arm::tcr.save-vsp)))
  (str fun (:@ imm1 (:$ 0)))
  (bx lr))

;;; This needs to be done out-of-line, to handle EGC memoization.
(defarmlapfunction %store-node-conditional ((offset 0) (object arg_x) (old arg_y) (new arg_z))
  (spjump .SPstore-node-conditional))

#+notyet                                ; needs a subprim on ARM
(defarmlapfunction %store-immediate-conditional ((offset 0) (object arg_x) (old arg_y) (new arg_z))
  (vpop temp0)
  (unbox-fixnum imm0 temp0)
  (let ((current temp1))
    @again
    (lrarx current object imm0)
    (cmpr current old)
    (bne @lose)
    (strcx. new object imm0)
    (bne @again)
    (isync)
    (li arg_z (+ arm::t-offset (target-nil-value)))
    (bx lr)
    @lose
    (li imm0 arm::reservation-discharge)
    (strcx. rzero rzero imm0)
    (li arg_z nil)
    (bx lr)))

(defarmlapfunction set-%gcable-macptrs% ((ptr arm::arg_z))
  (mov imm0 (:$ (- arm::nil-value arm::fulltag-nil)))
  (add imm1 imm0 (:$ (arm::kernel-global gcable-pointers)))
  @again
  (ldrex arg_y (:@ imm1))
  (str arg_y (:@ ptr (:$ arm::xmacptr.link)))
  (strex imm0 ptr (:@ imm1))
  (cmp imm0 (:$ 0))
  (bne @again)
  (bx lr))

;;; Atomically increment or decrement the gc-inhibit-count kernel-global
;;; (It's decremented if it's currently negative, incremented otherwise.)
(defarmlapfunction %lock-gc-lock ()
  (mov imm0 (:$ (- arm::nil-value arm::fulltag-nil)))
  (add imm1 imm0 (:$ (arm::kernel-global gc-inhibit-count)))
  @again
  (ldrex arg_y (:@ imm1))
  (cmp arg_y (:$ 0))
  (add arg_z arg_y '1)
  (sublt arg_z arg_y '1)
  @store
  (strex imm0 arg_z (:@ imm1))
  (cmp imm0 (:$ 0))
  (bne @again)
  (bx lr))

;;; Atomically decrement or increment the gc-inhibit-count kernel-global
;;; (It's incremented if it's currently negative, incremented otherwise.)
;;; If it's incremented from -1 to 0, try to GC (maybe just a little.)
(defarmlapfunction %unlock-gc-lock ()
  (mov imm0 (:$ (- arm::nil-value arm::fulltag-nil)))
  (add imm1 imm0 (:$ (arm::kernel-global gc-inhibit-count)))
  @again
  (mov arg_x (:$ 0))
  (ldrex arg_y (:@ imm1))
  (cmp arg_y '-1)
  (moveq arg_x arg_y)
  (subgt arg_z arg_y '1)
  (addle arg_z arg_y '1)
  (strex imm0 arg_z (:@ imm1))
  (cmp imm0 (:$ 0))
  (bne @again)
  (cmp arg_x '0)
  (bxeq lr)
  (mov imm0 (:$ arch::gc-trap-function-immediate-gc))
  (uuo-gc-trap (:? al))
  (bx lr))



(defarmlapfunction %atomic-incf-node ((by arg_x) (node arg_y) (disp arg_z))
  (spjump .SPatomic-incf-node))

(defarmlapfunction %atomic-incf-ptr ((ptr arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (ldrex imm0 (:@ imm1))
  (add imm0 imm0 (:$ 1))
  (strex imm2 imm0 (:@ imm1))
  (cmp imm2 (:$ 0))
  (bne @again)
  (box-fixnum arg_z imm0)
  (bx lr))


(defarmlapfunction %atomic-incf-ptr-by ((ptr arg_y) (by arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (ldrex imm0 (:@ imm1))
  (add imm0 imm0 (:asr by (:$ arm::fixnumshift)))
  (strex imm2 imm0 (:@ imm1))
  (bne @again)
  (box-fixnum arg_z imm0)
  (bx lr))

(defarmlapfunction %atomic-decf-ptr ((ptr arg_z))
  (macptr-ptr imm1 ptr)
  (dmb)
  @again
  (ldrex imm0 (:@ imm1))
  (sub imm0 imm0 (:$ 1))
  (strex imm2 imm0 (:@ imm1))
  (cmp imm2 (:$ 0))
  (bne @again)
  (box-fixnum arg_z imm0)
  (bx lr))

(defarmlapfunction %atomic-decf-ptr-if-positive ((ptr arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (ldrex imm0 (:@ imm1))
  (cmp imm0 (:$ 0))
  (sub imm0 imm0 (:$ 1))
  (beq @done)
  (strex imm2 imm0 (:@ imm1))
  (cmp imm2 (:$ 0))
  (bne @again)
  (box-fixnum arg_z imm0)
  (bx lr)
  @done
  (clrex)
  (box-fixnum arg_z imm0)
  (bx lr))


(defarmlapfunction %atomic-swap-ptr ((ptr arg_y) (newval arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (unbox-fixnum imm2 arg_z)
  (ldrex imm0 (:@ imm1))
  (strex imm2 imm2 (:@ imm1))
  (cmp imm2 (:$ 0))
  (bne @again)
  (box-fixnum arg_z imm0)
  (bx lr))

;;; Try to store the fixnum NEWVAL at PTR, if and only if the old value
;;; was equal to OLDVAL.  Return the old value
(defarmlapfunction %ptr-store-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (macptr-ptr imm0 ptr)
  @again
  (ldrex imm1 (:@ imm0))
  (cmp imm1 (:asr expected-oldval (:$ arm::fixnumshift)))
  (unbox-fixnum imm2 newval)
  (bne @done)
  (strex imm2 imm2 (:@ imm0))
  (cmp imm2 (:$ 0))
  (bne @again)
  (dmb)
  (box-fixnum arg_z imm1)
  (bx lr)
  @done
  (clrex)
  (box-fixnum arg_z imm1)
  (bx lr))

(defarmlapfunction %ptr-store-fixnum-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (let ((address imm2)
        (actual-oldval imm1))
    (macptr-ptr address ptr)
    @again
    (ldrex actual-oldval (:@ address))
    (cmp actual-oldval expected-oldval)
    (bne @done)
    (strex imm0 newval (:@ address))
    (cmp imm0 (:$ 0))
    (bne @again)
    (mov arg_z actual-oldval)
    (bx lr)
    @done
    (clrex)
    (mov arg_z actual-oldval)
    (bx lr)))




(defarmlapfunction %macptr->dead-macptr ((macptr arg_z))
  (check-nargs 1)
  (mov imm0 (:$ arm::subtag-dead-macptr))
  (strb imm0 (:@ macptr (:$ arm::misc-subtag-offset)))
  (bx lr))

#+notyet                                ;for different reasons
(defarmlapfunction %%apply-in-frame ((catch-count imm0) (srv temp0) (tsp-count imm0) (db-link imm0)
                                     (parent arg_x) (function arg_y) (arglist arg_z))
  (check-nargs 7)

  ; Throw through catch-count catch frames
  (lwz imm0 12 vsp)                      ; catch-count
  (vpush parent)
  (vpush function)
  (vpush arglist)
  (sploadlr .SPnthrowvalues)
  (blx lr)

  ; Pop tsp-count TSP frames
  (lwz tsp-count 16 vsp)
  (cmpi cr0 tsp-count 0)
  (b @test)
@loop
  (subi tsp-count tsp-count '1)
  (cmpi cr0 tsp-count 0)
  (lwz tsp 0 tsp)
@test
  (bne cr0 @loop)

  ; Pop dynamic bindings until we get to db-link
  (lwz imm0 12 vsp)                     ; db-link
  (lwz imm1 arm::tcr.db-link arm::rcontext)
  (cmp cr0 imm0 imm1)
  (beq cr0 @restore-regs)               ; .SPunbind-to expects there to be something to do
  (sploadlr .SPunbind-to)
  (blx lr)

@restore-regs
  ; restore the saved registers from srv
  (lwz srv 20 vsp)
@get0
  (svref imm0 1 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get1)
  (lwz save0 0 imm0)
@get1
  (svref imm0 2 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get2)
  (lwz save1 0 imm0)
@get2
  (svref imm0 3 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get3)
  (lwz save2 0 imm0)
@get3
  (svref imm0 4 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get4)
  (lwz save3 0 imm0)
@get4
  (svref imm0 5 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get5)
  (lwz save4 0 imm0)
@get5
  (svref imm0 6 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get6)
  (lwz save5 0 imm0)
@get6
  (svref imm0 7 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get7)
  (lwz save6 0 imm0)
@get7
  (svref imm0 8 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @got)
  (lwz save7 0 imm0)
@got

  (vpop arg_z)                          ; arglist
  (vpop temp0)                          ; function
  (vpop parent)                         ; parent
  (extract-lisptag imm0 parent)
  (cmpi cr0 imm0 arm::tag-fixnum)
  (if (:cr0 :ne)
    ; Parent is a fake-stack-frame. Make it real
    (progn
      (svref sp %fake-stack-frame.sp parent)
      (stwu sp (- arm::lisp-frame.size) sp)
      (svref fn %fake-stack-frame.fn parent)
      (stw fn arm::lisp-frame.savefn sp)
      (svref temp1 %fake-stack-frame.vsp parent)
      (stw temp1 arm::lisp-frame.savevsp sp)
      (svref temp1 %fake-stack-frame.lr parent)
      (extract-lisptag imm0 temp1)
      (cmpi cr0 imm0 arm::tag-fixnum)
      (if (:cr0 :ne)
        ;; must be a macptr encoding the actual link register
        (macptr-ptr loc-pc temp1)
        ;; Fixnum is offset from start of function vector
        (progn
          (svref temp2 0 fn)        ; function vector
          (unbox-fixnum temp1 temp1)
          (add loc-pc temp2 temp1)))
      (stw loc-pc arm::lisp-frame.savelr sp))
    ;; Parent is a real stack frame
    (mov sp parent))
  (set-nargs 0)
  (sploadlr .SPspreadargz)
  (blx lr)
  (spjump .SPtfuncallgen))



        


(defarmlapfunction %%save-application ((flags arg_y) (fd arg_z))
  (unbox-fixnum imm0 flags)
  (orr imm0 imm0 (:$ arch::gc-trap-function-save-application))
  (unbox-fixnum imm1 fd)
  (uuo-gc-trap (:? al))
  (bx lr))



(defarmlapfunction %misc-address-fixnum ((misc-object arg_z))
  (check-nargs 1)
  (add arg_z misc-object (:$ arm::misc-data-offset))
  (bx lr))


(defarmlapfunction fudge-heap-pointer ((ptr arg_x) (subtype arg_y) (len arg_z))
  (check-nargs 3)
  (macptr-ptr imm1 ptr) ; address in macptr
  (add imm0 imm1 (:$ 9))     ; 2 for delta + 7 for alignment
  (bic imm0 imm0 (:$ 7))   ; Clear low three bits to align
  (rsb imm1 imm1 imm0)  ; imm1 = delta
  (strh imm1 (:@  imm0 (:$ -2)))     ; save delta halfword
  (unbox-fixnum imm1 subtype)  ; subtype at low end of imm1
  (orr imm1 imm1 (:lsl len (:$ (- arm::num-subtag-bits arm::fixnum-shift))))
  (str imm1 (:@ imm0 (:$ 0)))       ; store subtype & length
  (add arg_z imm0 (:$ arm::fulltag-misc)) ; tag it, return it
  (bx lr))



(defarmlapfunction %%make-disposable ((ptr arg_y) (vector arg_z))
  (check-nargs 2)
  (sub imm0 vector (:$ arm::fulltag-misc)) ; imm0 is addr = vect less tag
  (ldrh imm1 (:@ imm0 (:$ -2)))   ; get delta
  (sub imm0 imm0 imm1)  ; vector addr (less tag)  - delta is orig addr
  (str imm0 (:@ ptr (:$ arm::macptr.address)))
  (bx lr))

(defarmlapfunction %vect-data-to-macptr ((vect arg_y) (ptr arg_z))
  ;; put address of vect data in macptr.  For all vector types
  ;; other than DOUBLE-FLOAT (or vectors thereof), the first byte
  ;; of data is at ARM::MISC-DATA-OFFSET; for the double-float
  ;; types, it's at ARM::MISC-DFLOAT-OFFSET.
  (extract-subtag imm0 vect)
  (cmp imm0 (:$ arm::subtag-double-float-vector))
  (cmpne imm0 (:$ arm::subtag-double-float))
  (addne temp0 vect (:$ arm::misc-data-offset))
  (addeq temp0 vect (:$ arm::misc-dfloat-offset))
  (str temp0 (:@ arg_z (:$ arm::macptr.address)))
  (bx lr))

(defarmlapfunction %ivector-from-macptr ((ptr arg_z))
  ;; Assuming that PTR points to the first byte of vector data
  ;; (in an ivector allocated on a stack or in foreign memory),
  ;; return the (tagged) ivector.  Crash and burn if the assumption
  ;; is incorrect.
  (macptr-ptr imm0 arg_z)
  (and imm1 imm0 (:$ arm::node-size))
  (eor imm1 imm1 (:$ arm::node-size))
  (add imm0 imm0 (:$ (- arm::fulltag-misc arm::node-size)))
  (sub arg_z imm0 imm1)
  (bx lr))

(defun get-saved-register-values ()
  (values))

(defarmlapfunction %current-db-link ()
  (ldr arg_z (:@ arm::rcontext (:$ arm::tcr.db-link)))
  (bx lr))

(defarmlapfunction %no-thread-local-binding-marker ()
  (mov arg_z (:$ arm::subtag-no-thread-local-binding))
  (bx lr))



;;; Should be called with interrupts disabled.
(defarmlapfunction %safe-get-ptr ((src arg_y) (dest arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 src)
  (str imm0 (:@ arm::rcontext (:$ arm::tcr.safe-ref-address)))
  (ldr imm0 (:@ imm0 (:$ 0)))                     ; may fault
  (str imm0 (:@ dest (:$ arm::macptr.address)))
  (bx lr))



;;; r13 contains thread context on Linux/Darwin PPC64.
;;; That's maintained in r2 on LinuxPPC32, and not maintained
;;; in a GPR on DarwinPPC32
#+huh
(defarmlapfunction %get-os-context ()
  #+ppc64-target (mov arg_z 13)
  #+linuxppc32-target (mov arg_z 2)
  #+darinppc32-target (mov arg_z 0)
  (bx lr))

#+bad-idea
(defarmlapfunction %check-deferred-gc ()
  (ldr imm0 arm::tcr.flags arm::rcontext)
  (slri. imm0 imm0 (- (1- arm::nbits-in-word) (+ arch::tcr-flag-bit-pending-suspend arm::fixnumshift)))
  (li arg_z nil)
  (bgelr)
  (uuo_interr arch::error-propagate-suspend rzero)
  (li arg_z t)
  (bx lr))



(defarmlapfunction %%tcr-interrupt ((target arg_z))
  (check-nargs 1)
  (uuo-kernel-service (:$  arch::error-interrupt))
  (box-fixnum arg_z imm0)
  (bx lr))

(defarmlapfunction %suspend-tcr ((target arg_z))
  (check-nargs 1)
  (uuo-kernel-service (:$ arch::error-suspend))
  (mov arg_z 'nil)
  (cmp imm0 (:$ 0))
  (addne arg_z arg_z (:$ arm::t-offset))
  (bx lr))

(defarmlapfunction %suspend-other-threads ()
  (check-nargs 0)
  (uuo-kernel-service (:$ arch::error-suspend-all))
  (mov arg_z 'nil)
  (cmp imm0 (:$ 0))
  (addne arg_z arg_z (:$ arm::t-offset))
  (bx lr))

(defarmlapfunction %resume-tcr ((target arg_z))
  (check-nargs 1)
  (uuo-kernel-service (:$ arch::error-resume))
  (mov arg_z 'nil)
  (cmp imm0 (:$ 0))
  (addne arg_z arg_z (:$ arm::t-offset))
  (bx lr))

(defarmlapfunction %resume-other-threads ()
  (check-nargs 0)
  (uuo-kernel-service (:$ arch::error-resume-all))
  (mov arg_z 'nil)
  (bx lr))

(defarmlapfunction %kill-tcr ((target arg_z))
  (check-nargs 1)
  (uuo-kernel-service (:$ arch::error-kill))
  (mov arg_z 'nil)
  (cmp imm0 (:$ 0))
  (addne arg_z arg_z (:$ arm::t-offset))
  (bx lr))

(defarmlapfunction pending-user-interrupt ()
  (mov temp0 (:$ 0))
  (ref-global arg_z arm::intflag)
  (set-global temp0 arm::intflag imm0)
  (bx lr))

#+later
(progn
(defarmlapfunction %atomic-pop-static-cons ()
  (li imm0 (+ (target-nil-value) (arm::kernel-global static-conses)))
  @again
  (lrarx arg_z rzero imm0)
  (cmpri arg_z (target-nil-value))
  (beq @lose)
  (%cdr arg_y arg_z)
  (strcx. arg_y rzero imm0)
  (bne @again)
  (li imm0 (+ (target-nil-value) (arm::kernel-global free-static-conses)))
  @decf
  (lrarx imm1 rzero imm0)
  (subi imm1 imm1 '1)
  (strcx. imm1 rzero imm0)
  (bne @decf)
  (isync)
  (bx lr)
  @lose
  (li imm0 arm::reservation-discharge)
  (strcx. rzero rzero imm0)
  (bx lr))



(defarmlapfunction %staticp ((x arg_z))
  (check-nargs 1)
  (ref-global temp0 static-cons-area)
  (ldr imm1 arm::area.low temp0)
  (sub imm0 x imm1)
  (ldr imm1 arm::area.ndnodes temp0)
  (srri imm0 imm0 arm::dnode-shift)
  (li arg_z nil)
  (sub imm1 imm1 imm0)
  (cmplri imm1 0)
  (la imm1 128 imm1)
  (blelr)
  (box-fixnum arg_z imm1)
  (bx lr))

(defarmlapfunction %static-inverse-cons ((n arg_z))
  (check-nargs 1)
  (extract-lisptag imm0 arg_z)
  (cmpri imm0 0)
  (ref-global temp0 static-cons-area)
  (bne @fail)
  (la n '-128 n)
  (ldr imm0 arm::area.ndnodes temp0)
  (ldr imm1 arm::area.high temp0)
  (box-fixnum arg_y imm0)
  (sub imm1 imm1 n)
  (cmplr arg_z arg_y)
  (sub imm1 imm1 n)
  (bgt @fail)
  (la arg_z arm::fulltag-cons imm1)
  (ldr arg_y arm::cons.car arg_z)
  (cmpri arg_y arm::unbound-marker)
  (bnelr)
  @fail
  (li arg_z nil)
  (bx lr))
);#+later

(defarmlapfunction xchgl ((newval arg_y) (ptr arg_z))
  (unbox-fixnum imm0 newval)
  @again
  (macptr-ptr imm2 ptr)
  (ldrex imm1 (:@ imm2))
  (strex imm2 imm0 (:@ imm2))
  (cmp imm2 (:$ 0))
  (bne @again)
  (dmb)
  (box-fixnum arg_z imm1)
  (bx lr))

(defarmlapfunction %atomic-pop-static-cons ()
  (load-global-address imm0 arm::static-conses)
  (load-global-address imm2 arm::free-static-conses)
  @again
  (ldrex arg_z (:@ imm0))
  (cmp arg_z 'nil)
  (bxeq lr)
  (%cdr temp0 arg_z)
  (strex imm1 temp0 (:@ imm0))
  (cmp imm1 (:$ 0))
  (bne @again)
  @dec
  (ldrex imm0 (:@ imm2))
  (sub imm0 imm0 '1)
  (strex imm1 imm0 (:@ imm2))
  (cmp imm1 (:$ 0))
  (bne @dec)
  (dmb)
  (bx lr))
  

; end of arm-misc.lisp
