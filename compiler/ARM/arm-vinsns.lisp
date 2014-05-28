;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "ARM-BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "ARMENV"))

(defmacro define-arm-vinsn (vinsn-name (results args &optional temps) &body body)
  (%define-vinsn *arm-backend* vinsn-name results args temps body))


;;; Non-volatile FPRs.
(define-arm-vinsn (push-nvfprs :push :multiple :doubleword :csp :predicatable)
    (()
     ((n :u16const)
      (header :u16const))
     ((d7 (:double-float #.arm::d7))))
  (flds s14 (:= :header))
  (fstmdbd d7 (:! arm::sp) (:apply + n 1))
  (fcpys single-float-zero s15)
  (:data)
  :header
  (:word header)
  (:code))

(define-arm-vinsn (pop-nvfprs :pop :multiple :doubleword :csp :predicatable)
    (()
     ((n :u16const))
     ((d7 (:double-float #.arm::d7))))
  (fldmiad d7 (:! arm::sp) (:apply + n 1))
  (fcpys single-float-zero s15))

(define-arm-vinsn data-section (()
                                ())
  (:data))

(define-arm-vinsn code-section (()
                                ())
  (:code))


;;; Index "scaling" and constant-offset misc-ref vinsns.

(define-arm-vinsn (scale-node-misc-index :predicatable)
    (((dest :u32))
     ((idx :imm) ; A fixnum
      )
     ())
  (add dest idx (:$ arm::misc-data-offset)))

(define-arm-vinsn (scale-32bit-misc-index :predicatable)
    (((dest :u32))
     ((idx :imm)                        ; A fixnum
      )
     ())
  (add dest idx (:$ arm::misc-data-offset)))

(define-arm-vinsn (scale-16bit-misc-index :predicatable)
    (((dest :u32))
     ((idx :imm)                        ; A fixnum
      )
     ())
  (mov  dest (:lsr idx (:$ 1)))
  (add dest dest (:$ arm::misc-data-offset)))

(define-arm-vinsn (scale-8bit-misc-index :predicatable)
    (((dest :u32))
     ((idx :imm)                        ; A fixnum
      )
     ())
  (mov dest (:lsr idx (:$ 2)))
  (add dest dest (:$ arm::misc-data-offset)))

(define-arm-vinsn (scale-64bit-misc-index :predicatable)
    (((dest :u32))
     ((idx :imm)                        ; A fixnum
      )
     ())
  (add dest idx idx)
  (add dest dest (:$ arm::misc-dfloat-offset)))


(define-arm-vinsn (scale-1bit-misc-index :predicatable)
    (((word-index :u32)
      (bitnum :u8))                     ; (unsigned-byte 5)
     ((idx :imm)                        ; A fixnum
      )
     )
  (mov word-index (:lsr idx (:$ arm::fixnumshift)))
  (and bitnum word-index (:$ 31))
  (mov word-index (:lsr word-index (:$ 5)))
  (mov word-index (:lsl word-index (:$ arm::word-shift)))
  (add word-index word-index (:$ arm::misc-data-offset)))



(define-arm-vinsn (misc-ref-u32 :predicatable)
    (((dest :u32))
     ((v :lisp)
      (scaled-idx :u32))
     ())
  (ldr dest (:+@ v scaled-idx)))


(define-arm-vinsn (misc-ref-c-u32 :predicatable)
    (((dest :u32))
     ((v :lisp)
      (idx :u32const))
     ())
  (ldr dest (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 2))))))

(define-arm-vinsn (misc-ref-s32 :predicatable)
    (((dest :s32))
     ((v :lisp)
      (scaled-idx :u32))
     ())
  (ldr dest (:+@ v  scaled-idx)))

(define-arm-vinsn (misc-ref-c-s32 :predicatable)
    (((dest :s32))
     ((v :lisp)
      (idx :u32const))
     ())
  (ldr dest (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 2))))))


(define-arm-vinsn (misc-set-c-u32 :predicatable)
    (()
     ((val :u32)
      (v :lisp)
      (idx :u32const)))
  (str val (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 2))))))

(define-arm-vinsn (misc-set-c-s32 :predicatable)
    (()
     ((val :s32)
      (v :lisp)
      (idx :u32const)))
  (str val (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 2))))))

(define-arm-vinsn (misc-set-u32 :predicatable)
    (()
     ((val :u32)
      (v :lisp)
      (scaled-idx :u32)))
  (str val (:+@ v scaled-idx)))

(define-arm-vinsn (misc-set-s32 :predicatable)
    (()
     ((val :s32)
      (v :lisp)
      (scaled-idx :u32)))
  (str val (:+@ v scaled-idx)))

                              
(define-arm-vinsn (misc-ref-single-float :predicatable :sets-lr)
    (((dest :single-float))
     ((v :lisp)
      (scaled-idx :u32)))
  (add lr v scaled-idx)
  (flds dest (:@ lr (:$ 0)))
  (mov lr (:$ 0)))

(define-arm-vinsn (misc-ref-c-single-float :predicatable :sets-lr)
    (((dest :single-float))
     ((v :lisp)
      (idx :u32const))
     ())
  (add lr v (:$ arm::misc-data-offset))
  (flds dest (:@ lr (:$ (:apply ash idx 2))))
  (mov lr (:$ 0)))

(define-arm-vinsn (misc-ref-double-float :predicatable :sets-lr)
    (((dest :double-float))
     ((v :lisp)
      (unscaled-idx :imm)))
  (add arm::lr v (:$ arm::misc-dfloat-offset))
  (add arm::lr arm::lr (:lsl unscaled-idx (:$ 1)))
  (fldd dest (:@ arm::lr (:$ 0)))
  (mov lr (:$ 0)))


(define-arm-vinsn (misc-ref-complex-double-float :predicatable :sets-lr)
    (((dest :complex-double-float))
     ((v :lisp)
      (unscaled-idx :imm)))
  (add arm::lr v (:$ arm::misc-dfloat-offset))
  (add arm::lr arm::lr (:lsl unscaled-idx (:$ 2)))
  (fldmiad dest arm::lr   2)
  (mov lr (:$ 0)))

(define-arm-vinsn (misc-ref-c-double-float :predicatable :sets-lr)
    (((dest :double-float))
     ((v :lisp)
      (idx :u32const)))
  (add lr v (:$ arm::double-float.value))
  (fldd dest (:@ lr (:$ (:apply ash idx 3))))
  (mov lr (:$ 0)))

(define-arm-vinsn (misc-set-c-double-float :predicatable :sets-lr)
    (((val :double-float))
     ((v :lisp)
      (idx :u32const)))
  (add lr v (:$ arm::double-float.value))
  (fstd val (:@ lr (:$ (:apply ash idx 3))))
  (mov lr (:$ 0)))

(define-arm-vinsn (misc-set-double-float :predicatable :sets-lr)
    (()
     ((val :double-float)
      (v :lisp)
      (unscaled-idx :imm)))             ; a fixnum
  (add lr v (:$ arm::misc-dfloat-offset))
  (add lr lr (:lsl unscaled-idx (:$ 1)))
  (fstd val (:@ lr (:$ 0)))
  (mov lr (:$ 0)))

(define-arm-vinsn (misc-set-c-single-float :predicatable)
    (()
     ((val :single-float)
      (v :lisp)
      (idx :u32const)))
  (add lr v (:$ arm::misc-data-offset))
  (fsts val (:@ lr (:$ (:apply ash idx 2))))
  (mov lr (:$ 0)))




(define-arm-vinsn (misc-set-single-float :predicatable :sets-lr)
    (()
     ((val :single-float)
      (v :lisp)
      (scaled-idx :u32)))
  (add arm::lr v scaled-idx)
  (fsts val (:@ arm::lr (:$ 0)))
  (mov lr (:$ 0)))


(define-arm-vinsn (misc-ref-u16 :predicatable)
    (((dest :u16))
     ((v :lisp)
      (scaled-idx :u32))
     ())
  (ldrh dest (:+@ v scaled-idx)))

(define-arm-vinsn (misc-ref-c-u16 :predicatable)
    (((dest :u16))
     ((v :lisp)
      (idx :u32const))
     ())
  (ldrh dest (:+@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 1))))))

(define-arm-vinsn (misc-set-c-u16 :predicatable)
    (((val :u16))
     ((v :lisp)
      (idx :u32const))
     ())
  (strh val (:+@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 1))))))

(define-arm-vinsn (misc-set-u16 :predicatable)
    (((val :u16))
     ((v :lisp)
      (scaled-idx :s32)))
  (strh val (:+@ v scaled-idx)))

(define-arm-vinsn misc-ref-s16  (((dest :s16))
                                 ((v :lisp)
                                  (scaled-idx :u32))
                                 ())
  (ldrsh dest (:@ v scaled-idx)))

(define-arm-vinsn (misc-ref-c-s16 :predicatable)
    (((dest :s16))
     ((v :lisp)
      (idx :u32const))
     ())
  (ldrsh dest (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 1))))))


(define-arm-vinsn (misc-set-c-s16 :predicatable)
    (((val :s16))
     ((v :lisp)
      (idx :u32const))
     ())
  (strh val (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 1))))))

(define-arm-vinsn (misc-set-s16 :predicatable)
    (((val :s16))
     ((v :lisp)
      (scaled-idx :s32)))
  (strh val (:@ v scaled-idx)))

(define-arm-vinsn (misc-ref-u8 :predicatable)
    (((dest :u8))
     ((v :lisp)
      (scaled-idx :u32))
     ())
  (ldrb dest (:@ v scaled-idx)))

(define-arm-vinsn (misc-ref-c-u8 :predicatable)
    (((dest :u8))
     ((v :lisp)
      (idx :u32const))
     ())
  (ldrb dest (:@ v (:$ (:apply + arm::misc-data-offset idx)))))

(define-arm-vinsn (misc-set-c-u8 :predicatable)
    (((val :u8))
     ((v :lisp)
      (idx :u32const))
     ())
  (strb val (:@ v (:$ (:apply + arm::misc-data-offset idx)))))

(define-arm-vinsn (misc-set-u8 :predicatable)
    (((val :u8))
     ((v :lisp)
      (scaled-idx :u32))
     ())
  (strb val (:@ v scaled-idx)))

(define-arm-vinsn (misc-ref-s8 :predicatable)
    (((dest :s8))
     ((v :lisp)
      (scaled-idx :u32))
     ())
  (ldrsb dest (:@ v scaled-idx)))

(define-arm-vinsn (misc-ref-c-s8 :predicatable)
    (((dest :s8))
     ((v :lisp)
      (idx :u32const))
     ())
  (ldrsb dest (:@ v (:$ (:apply + arm::misc-data-offset idx)))))

(define-arm-vinsn (misc-set-c-s8 :predicatable)
    (((val :s8))
     ((v :lisp)
      (idx :u32const))
     ())
  (strb val (:@ v (:$ (:apply + arm::misc-data-offset idx)))))

(define-arm-vinsn (misc-set-s8 :predicatable)
    (((val :s8))
     ((v :lisp)
      (scaled-idx :u32))
     ())
  (strb val (:@ v scaled-idx)))


(define-arm-vinsn (misc-ref-c-bit :predicatable)
    (((dest :u8))
     ((v :lisp)
      (idx :u32const))
     ())
  (ldr dest (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash (:apply ash idx -5) arm::word-shift)))))
  (mov dest (:lsr dest (:$ (:apply logand idx #x1f))))
  (and dest dest (:$ 1)))

(define-arm-vinsn (misc-ref-c-bit-fixnum :predicatable)
    (((dest :imm))
     ((v :lisp)
      (idx :u32const))
     ((temp :u32)))
  (ldr temp (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash (:apply ash idx -5) arm::word-shift)))))
  ((:not (:pred = 0 (:apply logand #x1f (:apply - (:apply logand idx #x1f) arm::fixnumshift))))
   (mov temp (:ror temp (:$ (:apply logand #x1f (:apply - (:apply logand idx #x1f) arm::fixnumshift))))))
  (and dest temp (:$ arm::fixnumone)))


(define-arm-vinsn (misc-ref-node :predicatable)
    (((dest :lisp))
     ((v :lisp)
      (scaled-idx :s32))
     ())
  (ldr dest (:@ v scaled-idx)))


(define-arm-vinsn (misc-ref-c-node :predicatable)
    (((dest :lisp))
     ((v :lisp)
      (idx :s16const))
     ())
  (ldr dest (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 2))))))

(define-arm-vinsn (misc-set-node :predicatable)
    (()
     ((val :lisp)
      (v :lisp)
      (scaled-idx :u32)))
  (str val (:@ v scaled-idx)))

;;; This should only be used for initialization (when the value being
;;; stored is known to be older than the vector V.)
(define-arm-vinsn (misc-set-c-node :predicatable)
    (()
     ((val :lisp)
      (v :lisp)
      (idx :s16const))
     ())
  (str val (:@ v (:$ (:apply + arm::misc-data-offset (:apply ash idx 2))))))


(define-arm-vinsn (misc-element-count-fixnum :predicatable)
    (((dest :imm))
     ((v :lisp))
     ((temp :u32)))
  (ldr temp (:@ v (:$ arm::misc-header-offset)))
  (bic temp temp (:$ arm::subtag-mask))
  (mov dest (:lsr temp (:$ (- arm::num-subtag-bits arm::fixnum-shift)))))

(define-arm-vinsn check-misc-bound (()
                                    ((idx :imm)
                                     (v :lisp))
                                    ((temp :u32)))
  (ldr temp (:@ v (:$ arm::misc-header-offset)))
  (bic temp temp (:$ arm::subtag-mask))
  (cmp idx (:lsr temp (:$ (- arm::num-subtag-bits arm::fixnum-shift))))
  (blo :ok)
  (uuo-error-vector-bounds idx v)
  :ok)

(define-arm-vinsn (2d-unscaled-index :predicatable)
    (((dest :imm)
      (dim1 :u32))
     ((dim1 :u32)
      (i :imm)
      (j :imm)))
  (mla dest i dim1 j))

;; dest <- (+ (* i dim1 dim2) (* j dim2) k)

(define-arm-vinsn (3d-unscaled-index :predicatable)
    (((dest :imm)
      (dim1 :u32)
      (dim2 :u32))
     ((dim1 :u32)
      (dim2 :u32)
      (i :imm)
      (j :imm)
      (k :imm)))
  (mul dim1 dim1 dim2)
  (mla dim2 j dim2 k)
  (mla dest dim1 i dim2))

(define-arm-vinsn (2d-dim1 :predicatable)
    (((dest :u32))
     ((header :lisp)))
  (ldr dest (:@ header (:$ (+ arm::misc-data-offset (* 4 (1+ arm::arrayH.dim0-cell))))))
  (mov dest (:asr dest (:$ arm::fixnumshift))))



(define-arm-vinsn (3d-dims :predicatable)
    (((dim1 :u32)
      (dim2 :u32))
     ((header :lisp)))
  (ldr dim1 (:@ header (:$ (+ arm::misc-data-offset (* 4 (1+ arm::arrayH.dim0-cell))))))
  (ldr dim2 (:@ header (:$ (+ arm::misc-data-offset (* 4 (+ 2 arm::arrayH.dim0-cell))))))
  (mov dim1 (:asr dim1 (:$ arm::fixnumshift)))
  (mov dim2 (:asr dim2 (:$ arm::fixnumshift))))

;; Return dim1 (unboxed)
(define-arm-vinsn check-2d-bound (((dim :u32))
                                  ((i :imm)
                                   (j :imm)
                                   (header :lisp)))
  (ldr dim (:@ header (:$ (+ arm::misc-data-offset (* 4 arm::arrayH.dim0-cell)))))
  (cmp i dim)
  (blo :ok1)
  (mov dim (:$ 0))
  (uuo-error-array-axis-bounds  i dim header)
  :ok1
  (ldr dim (:@ header (:$ (+ arm::misc-data-offset (* 4 (1+ arm::arrayH.dim0-cell))))))
  (cmp j dim)
  (blo :ok2)
  (mov dim (:$ arm::fixnumone))
  (uuo-error-array-axis-bounds j dim header)
  :ok2
  (mov dim (:asr dim (:$ arm::fixnumshift))))

(define-arm-vinsn check-3d-bound (((dim1 :u32)
                                   (dim2 :u32))
                                  ((i :imm)
                                   (j :imm)
                                   (k :imm)
                                   (header :lisp)))
  (ldr dim1 (:@ header (:$ (+ arm::misc-data-offset (* 4 arm::arrayH.dim0-cell)))))
  (cmp i dim1)
  (blo :ok1)
  (mov dim1 (:$ 0))
  (uuo-error-array-axis-bounds  i dim1  header)
  :ok1
  (ldr dim1 (:@ header (:$ (+ arm::misc-data-offset (* 4 (1+ arm::arrayH.dim0-cell))))))
  (cmp j dim1)
  (blo :ok2)
  (mov dim1 (:$ arm::fixnumone))
  (uuo-error-array-axis-bounds  i dim1 header)
  :ok2
  (ldr dim2 (:@ header (:$ (+ arm::misc-data-offset (* 4 (+ 2 arm::arrayH.dim0-cell))))))
  (cmp k dim2)
  (blo :ok3)
  (mov dim2 (:$ (ash 2 arm::fixnumshift)))
  (uuo-error-array-axis-bounds  i dim2 header)
  :ok3
  (mov dim1 (:asr dim1 (:$ arm::fixnumshift)))
  (mov dim2 (:asr dim2 (:$ arm::fixnumshift))))

(define-arm-vinsn (array-data-vector-ref :predicatable)
    (((dest :lisp))
     ((header :lisp)))
  (ldr dest (:@ header (:$ arm::arrayH.data-vector))))

  
(define-arm-vinsn (node-slot-ref :predicatable)
    (((dest :lisp))
     ((node :lisp)
      (cellno :u32const)))
  (ldr dest (:@ node (:$ (:apply + arm::misc-data-offset (:apply ash cellno 2))))))



(define-arm-vinsn  %slot-ref (((dest :lisp))
                              ((instance (:lisp (:ne dest)))
                               (index :lisp))
                              ((scaled :u32)))
  (add scaled index (:$ arm::misc-data-offset))
  (ldr dest (:@ instance scaled))
  (cmp dest (:$ arm::slot-unbound-marker))
  (bne :ok)
  (uuo-error-slot-unbound  dest instance index)
  :ok)


;;; Untagged memory reference & assignment.

(define-arm-vinsn (mem-ref-c-fullword :predicatable)
    (((dest :u32))
     ((src :address)
      (index :s16const)))
  (ldr dest (:@ src (:$ index))))


(define-arm-vinsn (mem-ref-c-signed-fullword :predicatable)
    (((dest :s32))
     ((src :address)
      (index :s16const)))
  (ldr dest (:@ src (:$ index))))


(define-arm-vinsn (mem-ref-c-natural :predicatable)
    (((dest :u32))
     ((src :address)
      (index :s16const)))
  (ldr dest (:@ src (:$ index))))
  

(define-arm-vinsn (mem-ref-fullword :predicatable)
    (((dest :u32))
     ((src :address)
      (index :s32)))
  (ldr dest (:@ src index)))

(define-arm-vinsn (mem-ref-signed-fullword :predicatable)
    (((dest :u32))
     ((src :address)
      (index :s32)))
  (ldr dest (:@ src index)))

(define-arm-vinsn (mem-ref-natural :predicatable)
    (((dest :u32))
     ((src :address)
      (index :s32)))
  (ldr dest (:@ src index)))


(define-arm-vinsn (mem-ref-c-u16 :predicatable)
    (((dest :u16))
     ((src :address)
      (index :s16const)))
  (ldrh dest (:@ src (:$ index))))


(define-arm-vinsn (mem-ref-u16 :predicatable)
    (((dest :u16))
     ((src :address)
      (index :s32)))
  (ldrh dest (:@ src index)))



(define-arm-vinsn (mem-ref-c-s16 :predicatable)
    (((dest :s16))
     ((src :address)
      (index :s16const)))
  (ldrsh dest (:@ src (:$ index))))

(define-arm-vinsn (mem-ref-s16 :predicatable)
    (((dest :s16))
     ((src :address)
      (index :s32)))
  (ldrsh dest (:@ src index)))

(define-arm-vinsn (mem-ref-c-u8 :predicatable)
    (((dest :u8))
     ((src :address)
      (index :s16const)))
  (ldrb dest (:@ src (:$ index))))

(define-arm-vinsn (mem-ref-u8 :predicatable)
    (((dest :u8))
     ((src :address)
      (index :s32)))
  (ldrb dest (:@ src index)))

(define-arm-vinsn (mem-ref-c-s8 :predicatable)
    (((dest :s8))
     ((src :address)
      (index :s16const)))
  (ldrsb dest (:@ src (:$ index))))

(define-arm-vinsn (mem-ref-s8 :predicatable)
    (((dest :s8))
     ((src :address)
      (index :s32)))
  (ldrsb dest (:@ src index)))

(define-arm-vinsn (mem-ref-c-bit :predicatable)
    (((dest :u8))
     ((src :address)
      (byte-index :s16const)
      (bit-shift :u8const)))
  (ldrb dest (:@ src (:$ byte-index)))
  (mov dest (:lsr dest (:$ bit-shift)))
  (and dest dest (:$ 1)))


(define-arm-vinsn (mem-ref-c-bit-fixnum :predicatable)
    (((dest :lisp))
     ((src :address)
      (byte-index :s16const)
      (bit-shift :u8const))
     ((byteval :u8)))
  (ldrb byteval (:@ src (:$ byte-index)))
  (mov byteval (:lsr byteval (:$ bit-shift)))
  (and byteval byteval (:$ 1))
  (mov dest (:lsr byteval (:$ arm::fixnumshift))))

(define-arm-vinsn (mem-ref-bit :predicatable)
    (((dest :u8))
     ((src :address)
      (bit-index :lisp))
     ((byte-index :s16)
      (bit-shift :u8)))

  (mov byte-index (:lsr bit-index (:$ arm::fixnumshift)))
  (and bit-shift byte-index (:$ 7))
  (ldrb byte-index (:@ src (:lsr byte-index (:$ 5))))
  (mov dest (:lsr byte-index bit-shift))
  (and dest dest (:$ 1)))


(define-arm-vinsn (mem-ref-bit-fixnum :predicatable)
    (((dest :lisp))
     ((src :address)
      (bit-index :lisp))
     ((byte-index :s16)
      (bit-shift :u8)))
  (mov byte-index (:lsr bit-index (:$ arm::fixnumshift)))
  (and bit-shift byte-index (:$ 7))
  (ldrb byte-index (:@ src (:lsr byte-index (:$ 3))))
  (mov byte-index (:lsr byte-index bit-shift))
  (mov dest (:$ arm::fixnumone))
  (and dest dest (:lsl byte-index (:$ arm::fixnumshift))))

(define-arm-vinsn (mem-ref-c-double-float :predicatable)
    (((dest :double-float))
     ((src :address)
      (index :s16const)))
  (fldd dest (:@ src (:$ index)))
)

(define-arm-vinsn (mem-ref-double-float :predicatable)
    (((dest :double-float)
      (src :address))
     ((src :address)
      (index :lisp)))
  (add src src (:asr index (:$ arm::fixnumshift)))
  (fldd dest (:@ src (:$ 0))))

(define-arm-vinsn (mem-set-c-double-float :predicatable)
    (()
     ((val :double-float)
      (src :address)
      (index :s16const))
     ((addr :u32)))
  (add addr src (:$ index))
  (fstd src (:@ addr (:$ 0))))

(define-arm-vinsn (mem-set-double-float :predicatable)
    (()
     ((val :double-float)
      (src :address)
      (index :s32))                     
     ((addr :u32)))
  (add addr src index)
  (fstd src (:@ addr (:$ 0))))

(define-arm-vinsn (mem-ref-c-single-float :predicatable)
    (((dest :single-float))
     ((src :address)
      (index :s16const)))
  (flds dest (:@ src (:$ index))))

(define-arm-vinsn (mem-ref-single-float :predicatable)
    (((dest :single-float)
      (src :address))
     ((src :address)
      (index :lisp)))
  (add src  src (:asr index (:$ arm::fixnumshift)))
  (flds dest (:@ src (:$ 0))))

;;; The caller should check that the index is kosher
(define-arm-vinsn (mem-set-c-single-float :predicatable)
    (()
     ((val :single-float)
      (src :address)
      (index :s16const)))
  (fsts val (:@ src (:$ index))))

(define-arm-vinsn (mem-set-single-float :predicatable)
    (()
     ((val :single-float)
      (src :address)
      (index :s32))
     ((temp :address)))
  (add temp src index)
  (fsts val (:@ temp (:$ 0))))


(define-arm-vinsn (mem-set-c-address :predicatable)
    (()
     ((val :address)
      (src :address)
      (index :s16const)))
  (str val (:@ src (:$ index))))

(define-arm-vinsn (mem-set-address :predicatable)
    (()
     ((val :address)
      (src :address)
      (index :s32)))
  (str val (:@ src index)))

(define-arm-vinsn (mem-set-c-fullword :predicatable)
    (()
     ((val :u32)
      (src :address)
      (index :s16const)))
  (str val (:@ src (:$ index))))

(define-arm-vinsn (mem-set-fullword :predicatable)
    (()
     ((val :u32)
      (src :address)
      (index :s32)))
  (str val (:@ src index)))

(define-arm-vinsn (mem-set-c-halfword :predicatable)
    (()
     ((val :u16)
      (src :address)
      (index :s16const)))
  (strh val (:@ src (:$ index))))

(define-arm-vinsn (mem-set-halfword :predicatable)
    (()
     ((val :u16)
      (src :address)
      (index :s32)))
  (strh val (:@ src index)))

(define-arm-vinsn (mem-set-c-byte :predicatable)
    (()
     ((val :u16)
      (src :address)
      (index :s16const)))
  (strb val (:@ src (:$ index))))

(define-arm-vinsn (mem-set-byte :predicatable)
    (()
     ((val :u8)
      (src :address)
      (index :s32)))
  (strb val (:@ src index)))

(define-arm-vinsn (mem-set-c-bit-0 :predicatable)
    (()
     ((src :address)
      (byte-index :s16const)
      (mask :u8const))
     ((val :u8)))
  (ldrb val (:@ src (:$ byte-index)))
  (bic val val (:$ mask))
  (strb val (:@ src (:$ byte-index))))


(define-arm-vinsn (mem-set-c-bit-1 :predicatable)
    (()
     ((src :address)
      (byte-index :s16const)
      (mask :u8const))
     ((val :u8)))
  (ldrb val (:@ src (:$ byte-index)))
  (orr val val (:$ mask))
  (strb val (:@ src (:$ byte-index))))


(define-arm-vinsn mem-set-c-bit (()
                                 ((src :address)
                                  (byte-index :s16const)
                                  (bit-index :u8const)
                                  (val :imm))
                                 ((byteval :u8)
                                  (mask :u8)))
  (mov mask (:$ 1))
  (mov mask (:lsl mask bit-index))
  (cmp val (:$ 0))
  (ldrb byteval (:@ src (:$ byte-index)))
  (orrne byteval byteval mask)
  (biceq byteval byteval mask)
  (strb byteval (:@ src (:$ byte-index)))
)

;;; Hey, they should be happy that it even works.  Who cares how big it is or how
;;; long it takes ...
(define-arm-vinsn mem-set-bit (()
                               ((src :address)
                                (bit-index :lisp)
                                (val :lisp))
                               ((bit-shift :u32)
                                (mask :u32)))
  (cmp val (:$ (ash 1 arm::fixnumshift)))
  (mov bit-shift (:$ 7))
  (mov mask (:$ 1))
  (and bit-shift bit-shift (:lsr bit-index (:$ arm::fixnumshift)))
  (mov mask (:lsl mask bit-shift))
  (ldrb bit-shift (:@ src (:lsr bit-index (:$ (+ 3 arm::fixnumshift)))))
  (bls :ok)
  (uuo-error-reg-not-xtype  val (:$ arm::xtype-bit))
  :ok
  (orrne bit-shift bit-shift mask)
  (biceq bit-shift bit-shift mask)
  (strb bit-shift (:@ src (:lsr bit-index (:$ (+ 3 arm::fixnumshift))))))
     
;;; Tag and subtag extraction, comparison, checking, trapping ...

(define-arm-vinsn (extract-tag :predicatable)
    (((tag :u8)) 
     ((object :lisp)) 
     ())
  (and tag object (:$ arm::tagmask)))

(define-arm-vinsn (extract-tag-fixnum :predicatable)
    (((tag :imm))
     ((object :lisp)))
  (mov tag (:lsl object (:$ arm::fixnumshift)))
  (and tag tag (:$ (ash arm::tagmask arm::fixnumshift))))

(define-arm-vinsn (extract-fulltag :predicatable)
    (((tag :u8))
     ((object :lisp))
     ())
  (and tag object (:$ arm::fulltagmask)))


(define-arm-vinsn (extract-fulltag-fixnum :predicatable)
    (((tag :imm))
     ((object :lisp)))
  (mov tag (:lsl object (:$ arm::fixnumshift)))
  (and tag tag (:$ (ash arm::fulltagmask arm::fixnumshift))))

(define-arm-vinsn extract-typecode (((code :u8))
                                    ((object :lisp))
                                    ())
  (and code object (:$ arm::tagmask))
  (cmp code (:$ arm::tag-misc))
  (ldrbeq code (:@ object (:$ arm::misc-subtag-offset))))

(define-arm-vinsn extract-typecode-fixnum (((code :imm))
                                           ((object (:lisp (:ne code))))
                                           ((subtag :u8)))
  (and subtag object (:$ arm::tagmask))
  (cmp subtag (:$ arm::tag-misc))
  (ldrbeq subtag (:@ object (:$ arm::misc-subtag-offset)))
  (mov code (:lsl subtag (:$ arm::fixnumshift))))


;;; Can we assume that an error handler can retry this without our
;;; emitting a branch ?  I'd like to think so.
(define-arm-vinsn require-fixnum (()
                                  ((object :lisp))
                                  ())
  (tst object (:$ arm::tagmask))
  (beq :ok)
  (uuo-cerror-reg-not-lisptag object (:$ arm::tag-fixnum))
  :ok)

(define-arm-vinsn require-integer (()
                                   ((object :lisp))
                                   ((tag :u8)))
  (ands tag object (:$ arm::tagmask))
  (beq :got-it)
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-bignum))
  (beq :got-it)
  (uuo-cerror-reg-not-xtype object (:$ arm::xtype-integer))
  :got-it)

(define-arm-vinsn require-simple-vector (()
                                         ((object :lisp))
                                         ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-simple-vector))
  (beq :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::subtag-simple-vector))
  :ok)

(define-arm-vinsn require-simple-string (()
                                         ((object :lisp))
                                         ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-simple-base-string))
  (beq :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::subtag-simple-base-string))
  :ok)

  
(define-arm-vinsn require-real (()
                                ((object :lisp))
                                ((tag :u8)
                                 (mask :u32)
                                 (realtags :u32)))
  (mov mask (:$ 1))
  (and tag object (:$ arm::tagmask))
  (mov realtags (:$ (logand #xff arm::real-tags-mask)))
  (orr realtags realtags (:$ (logand #xff00 arm::real-tags-mask)))
  (cmp tag (:$ arm::tag-misc))
  (orr realtags realtags (:$ (logand #xffff0000 arm::real-tags-mask)))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (tst realtags (:lsl mask tag))
  (bne :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::xtype-real))
  :ok)

(define-arm-vinsn require-number (()
                                  ((object :lisp))
                                  ((tag :u8)
                                   (mask :u32)
                                   (numtags :u32)))
  (mov mask (:$ 1))
  (and tag object (:$ arm::tagmask))
  (mov numtags (:$ (logand #xff arm::numeric-tags-mask)))
  (orr numtags numtags (:$ (logand #xff00 arm::numeric-tags-mask)))
  (cmp tag (:$ arm::tag-misc))
  (orr numtags numtags (:$ (logand #xffff0000 arm::numeric-tags-mask)))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ 31))
  (movhi mask (:$ 0))     ;not a number
  (tst numtags (:lsl mask tag))
  (bne :ok)
  (cmp tag (:$ arm::subtag-complex-double-float))
  (cmpne  tag (:$ arm::subtag-complex-single-float))
  (beq :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::xtype-number))
  :ok)


(define-arm-vinsn require-list (()
                                ((object :lisp))
                                ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-list))
  (beq :ok)
  (uuo-cerror-reg-not-lisptag object (:$ arm::tag-list))
  :ok)

(define-arm-vinsn require-symbol (()
                                  ((object :lisp))
                                  ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmpeq tag (:$ arm::subtag-symbol))
  (cmpne object (:$ arm::nil-value))
  (beq :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::subtag-symbol))
  :ok)

(define-arm-vinsn require-character (()
                                     ((object :lisp))
                                     ((tag :u8)))
  (and tag object (:$ arm::subtag-mask))
  (cmp tag (:$ arm::subtag-character))
  (beq :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::subtag-character))
  :ok)


(define-arm-vinsn require-s8 (()
                              ((object :lisp))
                              ((tag :u32)))
  (mov tag (:lsl object (:$ (- arm::nbits-in-word (+ 8 arm::fixnumshift)))))
  (mov tag (:asr tag (:$ (- arm::nbits-in-word 8))))
  (cmp object (:lsl tag (:$ arm::fixnumshift)))
  (beq :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::xtype-s8))
  :ok)


(define-arm-vinsn require-u8 (()
                              ((object :lisp))
                              ((temp :u32)))
  (mov temp (:$ (lognot (ash #xff arm::fixnumshift))))
  (tst object temp)
  (beq :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::xtype-u8))
  :ok)

(define-arm-vinsn require-s16 (()
                               ((object :lisp))
                               ( (tag :u32)))
  (mov tag (:lsl object (:$ (- arm::nbits-in-word (+ 16 arm::fixnumshift)))))
  (mov tag (:asr tag (:$ (- arm::nbits-in-word 16))))
  (cmp object (:lsl tag (:$ arm::fixnumshift)))
  (beq :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::xtype-s16))
  :ok)

(define-arm-vinsn require-u16 (()
                               ((object :lisp))
                               ((tag :u32)))
  (mov tag (:$ (lognot (ash #xff arm::fixnumshift))))
  (bic tag tag (:$ (ash #xff (+ 8 arm::fixnumshift))))
  (tst object tag)
  (beq :ok)
  (uuo-cerror-reg-not-xtype object (:$ arm::xtype-u16))
  :ok)

(define-arm-vinsn require-s32 (()
                               ((src :lisp))
                               ((tag :u32)
                                (header :u32)))
  (ands tag src (:$ arm::tagmask))
  (beq :got-it)
  (cmp tag (:$ arm::tag-misc))
  (mov tag (:$ (logand #xff arm::one-digit-bignum-header)))
  (orr tag tag (:$ (logand #xff00 arm::one-digit-bignum-header)))
  (ldreq header (:@ src (:$ arm::misc-header-offset)))
  (cmpeq tag header)
  (beq :got-it)
  (uuo-cerror-reg-not-xtype src (:$ arm::xtype-s32))
  :got-it)


(define-arm-vinsn require-u32 (()
                               ((src :lisp))
                               ((temp :u32)))
  :again
  (tst src (:$ (logior (ash 1 (1- arm::nbits-in-word)) arm::tagmask)))
  (beq :got-it)
  (and temp src (:$ arm::tagmask))
  (cmp temp (:$ arm::tag-misc))
  (ldrbeq temp (:@ src (:$ arm::misc-subtag-offset)))
  (cmp temp (:$ arm::subtag-bignum))
  (bne :bad-if-ne)
  (ldr temp (:@ src (:$ arm::misc-header-offset)))
  (mov temp (:lsr temp (:$ arm::num-subtag-bits)))
  (cmp temp (:$ 2))
  (beq :two)
  (cmp temp (:$ 1))
  (bne :bad-if-ne)
  (ldr temp (:@ src (:$ arm::misc-data-offset)))
  (tst temp (:$ (ash 1 31)))
  (b :bad-if-ne)
  :two
  (ldr temp (:@ src (:$ (+ 4 arm::misc-data-offset))))
  (cmp temp (:$ 0))
  :bad-if-ne
  (beq :got-it)
  (uuo-cerror-reg-not-xtype src (:$ arm::xtype-u32))
  :got-it)

(define-arm-vinsn require-s64 (()
                               ((src :lisp))
                               ((tag :u32)
                                (header :u32)))
  (ands tag src (:$ arm::tag-mask))
  (beq :got-it)
  (cmp tag (:$ arm::tag-misc))
  (ldreq header (:@ src (:$ arm::misc-header-offset)))
  (andeq tag header (:$ arm::subtag-mask))
  (cmp tag (:$ arm::subtag-bignum))
  (mov header (:lsr header (:$ arm::num-subtag-bits)))
  (bne :bad-if-ne)
  (cmp header (:$ 1))
  (beq :got-it)
  (cmp header (:$ 2))
  :bad-if-ne
  (beq :got-it)
  (uuo-cerror-reg-not-xtype src (:$ arm::xtype-s64))
  :got-it)

(define-arm-vinsn require-u64 (()
                               ((src :lisp))
                               ((temp :u32)
                                (header :u32)))
  :again
  (tst src (:$ (logior (ash 1 31) arm::fixnum-mask)))
  (and temp src (:$ arm::fixnum-mask))
  (beq :got-it)
  (cmp temp (:$ arm::tag-misc))
  (ldreq header (:@ src (:$ arm::misc-header-offset)))
  (andeq temp header (:$ arm::subtag-mask))
  (moveq header (:lsr header (:$ arm::num-subtag-bits)))
  (cmpeq temp (:$ arm::subtag-bignum))
  (bne :bad-if-ne)
  (cmp header (:$ 3))
  (ldreq temp (:@ src (:$ (+ arm::misc-data-offset 8))))
  (beq :three)
  (cmp header (:$ 2))
  (ldreq temp (:@ src (:$ (+ arm::misc-data-offset 4))))
  (beq :sign-of-highword)
  (cmp header (:$ 1))
  (ldr temp (:@ src (:$ arm::misc-data-offset)))
  (bne :bad-if-ne)
  :sign-of-highword
  (tst temp (:$ (ash 1 31)))
  (b :bad-if-ne)
  :three
  (cmp temp (:$ 0))
  :bad-if-ne
  (beq :got-it)
  (uuo-cerror-reg-not-xtype src (:$ arm::xtype-s64))
  :got-it)




(define-arm-vinsn require-char-code (()
                                     ((object :lisp)))
  (tst object (:$ arm::fixnum-mask))
  (bne :bad)
  (cmp object (:$ (ash char-code-limit arm::fixnumshift)))
  (bls :got-it)
  :bad
  (uuo-error-reg-not-xtype object (:$ arm::xtype-char-code))
  :got-it)


(define-arm-vinsn (box-fixnum :predicatable)
    (((dest :imm))
     ((src :s32)))
  (mov dest (:lsl src (:$ arm::fixnumshift))))

(define-arm-vinsn (fixnum->signed-natural :predicatable)
    (((dest :s32))
     ((src :imm)))
  (mov dest (:asr src (:$ arm::fixnumshift))))

(define-arm-vinsn (fixnum->unsigned-natural :predicatable)
    (((dest :u32))
     ((src :imm)))
  (mov dest (:lsr src (:$ arm::fixnumshift))))

;;; An object is of type (UNSIGNED-BYTE 32) iff
;;;  a) it's of type (UNSIGNED-BYTE 30) (e.g., an unsigned fixnum)
;;;  b) it's a bignum of length 1 and the 0'th digit is positive
;;;  c) it's a bignum of length 2 and the sign-digit is 0.

(define-arm-vinsn unbox-u32 (((dest :u32))
                             ((src :lisp))
                             ((temp :u32)))
                             
  (tst src (:$ #x80000003))
  (mov dest (:lsr src (:$ arm::fixnumshift)))
  (beq :got-it)
  (and temp src (:$ arm::tagmask))
  (cmp temp (:$ arm::tag-misc))
  (beq :is-uvector)
  (uuo-error-reg-not-xtype src (:$ arm::xtype-u32))
  :is-uvector
  (ldr dest (:@ src (:$ arm::misc-header-offset)))
  (mov temp (:$ (logand #xff arm::one-digit-bignum-header)))
  (orr temp temp (:$ (logand #xff00 arm::one-digit-bignum-header)))
  (cmp dest temp)
  (bne :maybe-two-digit)
  (ldr dest (:@ src (:$ arm::misc-data-offset)))
  (tst dest (:$ (ash 1 31)))
  (beq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm::xtype-u32))
  :maybe-two-digit
  (add temp temp (:$ (ash 1 arm::num-subtag-bits)))
  (cmp dest temp)
  (ldreq temp (:@ src (:$ (+ arm::misc-data-offset 4))))
  (cmpeq temp (:$ 0))
  (ldreq dest (:@ src (:$ arm::misc-data-offset)))
  (beq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm::xtype-u32))
  :got-it)

;;; an object is of type (SIGNED-BYTE 32) iff
;;; a) it's a fixnum
;;; b) it's a bignum with exactly one digit.

(define-arm-vinsn unbox-s32 (((dest :s32))
                             ((src :lisp))
                             ((tag :u32)))
  (ands tag src (:$ arm::tagmask))
  (mov dest (:asr src (:$ arm::fixnumshift)))
  (beq :got-it)
  (mov dest (:$ (logand #xff arm::one-digit-bignum-header)))
  (orr dest dest (:$ (logand #xff00 arm::one-digit-bignum-header)))
  (cmp tag (:$ arm::tag-misc))
  (ldreq tag (:@ src (:$ arm::misc-header-offset)))
  (cmpeq dest tag)
  (ldreq dest (:@ src (:$ arm::misc-data-offset)))
  (beq :got-it)
  (uuo-error-reg-not-xtype src (:$ arm::xtype-s32))
  :got-it)



(define-arm-vinsn unbox-u16 (((dest :u16))
                             ((src :lisp)))
  (mov dest (:lsl src (:$ (- 16 arm::fixnumshift))))
  (mov dest (:lsr dest (:$ 16)))
  (cmp src (:lsl dest (:$ arm::fixnumshift)))
  (beq :ok)
  (uuo-error-reg-not-xtype src (:$ arm::xtype-u16))
  :ok)

(define-arm-vinsn unbox-s16 (((dest :s16))
                             ((src :lisp)))
  (mov dest (:lsl src (:$ (- arm::nbits-in-word (+ 16 arm::fixnumshift)))))
  (mov dest (:asr dest (:$ 16)))
  (cmp src (:lsl dest (:$ arm::fixnumshift)))
  (beq :ok)
  (uuo-error-reg-not-xtype src (:$ arm::xtype-s16))
  :ok)

  
  
(define-arm-vinsn unbox-u8 (((dest :u8))
                            ((src :lisp)))
  (mov dest (:lsl src (:$ (- 24 arm::fixnumshift))))
  (mov dest (:lsr dest (:$ 24)))
  (cmp src (:lsl dest (:$ arm::fixnumshift)))
  (beq :ok)
  (uuo-error-reg-not-xtype src (:$ arm::xtype-u8))
  :ok)

(define-arm-vinsn (%unbox-u8 :predicatable)
    (((dest :u8))
     ((src :lisp)))
  (mov dest (:$ #xff))
  (and dest dest (:lsr src (:$ arm::fixnumshift))))

(define-arm-vinsn unbox-s8 (((dest :s8))
                            ((src :lisp)))
  (mov dest (:lsl src (:$ (- 24 arm::fixnumshift))))
  (mov dest (:asr dest (:$ 24)))
  (cmp src (:lsl dest (:$ arm::fixnumshift)))
  (beq :ok)
  (uuo-error-reg-not-xtype src (:$ arm::xtype-s8))
  :ok)

(define-arm-vinsn (%unbox-s8 :predicatable)
    (((dest :s8))
     ((src :lisp)))
  (mov dest (:$ #xff))
  (and dest dest (:asr src (:$ arm::fixnumshift))))

(define-arm-vinsn unbox-base-char (((dest :u32))
                                   ((src :lisp)))
  (and dest src (:$ arm::subtag-mask))
  (cmp dest (:$ arm::subtag-character))
  (mov dest (:lsr src (:$ arm::charcode-shift)))
  (beq :ok)
  (uuo-error-reg-not-xtype src (:$ arm::subtag-character))
  :ok)


(define-arm-vinsn unbox-bit (((dest :u32))
                             ((src :lisp)))
  (cmp src (:$ arm::fixnumone))
  (mov dest (:lsr src (:$ arm::fixnumshift)))
  (bls :ok)
  (uuo-error-reg-not-xtype src (:$ arm::xtype-bit))
  :ok)


(define-arm-vinsn clear-pending-fpu-exceptions (()
                                                ()
                                                ((imm :u32)))
  (fmrx imm :fpscr)
  (bic imm imm (:$ #xff))
  (fmxr :fpscr imm))

(define-arm-vinsn trap-if-fpu-exception (()
                                         ()
                                         ((signaled :u32)
                                          (enabled :u32)))
  (fmrx signaled :fpscr)
  (ldr enabled (:@ rcontext (:$ arm::tcr.lisp-fpscr)))
  (ands signaled signaled (:lsr enabled (:$ 8)))
  (beq :no-trap)
  (uuo-fpu-exception signaled enabled)
  :no-trap)
                                        

(define-arm-vinsn (fixnum->double :predicatable)
    (((dest :double-float))
     ((src :lisp)
      (temp :single-float))
     ((imm :s32)))
  (mov imm (:asr src (:$ arm::fixnumshift)))
  (fmsr temp imm)
  (fsitod dest temp))


(define-arm-vinsn (fixnum->single :predicatable)
    (((dest :single-float))
     ((src :lisp))
     ((imm :s32)))
  (mov imm (:asr src (:$ arm::fixnumshift)))
  (fmsr dest imm)
  (fsitos dest dest))



(define-arm-vinsn (shift-left-variable-word :predicatable)
    (((dest :u32))
     ((src :u32)
      (sh :u32)))
  (mov dest (:lsl src sh)))

(define-arm-vinsn (u32logandc2 :predicatable)
    (((dest :u32))
     ((x :u32)
      (y :u32)))
  (bic dest x y))

(define-arm-vinsn (u32logior :predicatable)
    (((dest :u32))
     ((x :u32)
      (y :u32)))
  (orr dest x y))

(define-arm-vinsn set-or-clear-bit (((dest :u32))
                                    ((src :u32)
                                     (mask :u32)
                                     (crf :crf)))
  (biceq dest src mask)
  (orrne dest src mask))

(define-arm-vinsn (complement-shift-count :predicatable)
    (((dest :u32))
     ((src :u32)))
  (rsb dest src (:$ 32)))

(define-arm-vinsn (extract-lowbyte :predicatable)
    (((dest :u32))
     ((src :lisp)))
  (and dest src (:$ arm::subtag-mask)))




(define-arm-vinsn trap-unless-fixnum (()
                                      ((object :lisp)))
  (tst object (:$ arm::fixnummask))
  (beq :ok)
  (uuo-error-reg-not-lisptag object (:$ arm::tag-fixnum))
  :ok)

(define-arm-vinsn trap-unless-list (()
                                    ((object :lisp))
                                    ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-list))
  (beq :ok)
  (uuo-error-reg-not-lisptag object (:$ arm::tag-list))
  :ok)

(define-arm-vinsn trap-unless-single-float (()
                                            ((object :lisp))
                                            ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-single-float))
  (beq :ok)
  (uuo-error-reg-not-xtype object (:$ arm::subtag-single-float))
  :ok)

(define-arm-vinsn trap-unless-double-float (()
                                            ((object :lisp))
                                            ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-double-float))
  (beq :ok)
  (uuo-error-reg-not-xtype object (:$ arm::subtag-double-float))
  :ok)


(define-arm-vinsn trap-unless-array-header (()
                                            ((object :lisp))
                                            ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-arrayH))
  (beq :ok)
  (uuo-error-reg-not-xtype object (:$ arm::subtag-arrayH))
  :ok)

(define-arm-vinsn trap-unless-macptr (()
                                      ((object :lisp))
                                      ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-macptr))
  (beq :ok)
  (uuo-error-reg-not-xtype object (:$ arm::subtag-macptr))
  :ok)



(define-arm-vinsn trap-unless-uvector (()
                                       ((object :lisp))
                                       ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (beq :ok)
  (uuo-error-reg-not-lisptag object (:$ arm::tag-misc))
  :ok)



(define-arm-vinsn trap-unless-character (()
                                         ((object :lisp))
                                         ((tag :u8)))
  (and tag object (:$ arm::subtag-mask))
  (cmp tag (:$ arm::subtag-character))
  (beq :ok)
  (uuo-error-reg-not-xtype object (:$ arm::subtag-character))
  :ok)

(define-arm-vinsn trap-unless-cons (()
                                    ((object :lisp))
                                    ((tag :u8)))
  (and tag object (:$ arm::fulltagmask))
  (cmp tag (:$ arm::fulltag-cons))
  (beq :ok)
  (uuo-error-reg-not-fulltag  object (:$ arm::fulltag-cons))
  :ok)

(define-arm-vinsn trap-unless-typecode= (()
                                         ((object :lisp)
                                          (tagval :u16const))
                                         ((tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ tagval))
  (beq :ok)
  (uuo-error-reg-not-xtype object (:$ tagval))
  :ok)

(define-arm-vinsn set-z-if-vector-header (((crf :crf))
                                          ((src :lisp))
                                          ((tag :u8)))
  (and tag src (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ src (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-vectorH)))

(define-arm-vinsn check-vector-header-bound (()
                                             ((header :lisp)
                                              (index :imm))
                                             ((limit :u32)))
  (ldr limit (:@ header (:$ arm::vectorH.physsize)))
  (cmp index limit)
  (blo :ok)
  (uuo-error-vector-bounds index header)
  :ok)

(define-arm-vinsn deref-vector-header (((vector :lisp)
                                        (index :lisp))
                                       ((vector :lisp)
                                        (index :lisp))
                                       ((flags :u32)))
  :again
  (ldr flags (:@ vector (:$ arm::vectorH.flags)))
  (tst flags (:$ (ash 1 $arh_disp_bit)))
  (ldr flags (:@ vector (:$ arm::vectorH.displacement)))
  (add index index flags)
  (ldr vector (:@ vector (:$ arm::vectorH.data-vector)))
  (bne :again))

(define-arm-vinsn trap-unless-vector-type (()
                                           ((header :lisp)
                                            (expected-type :u8const))
                                           ((flags :u32)
                                            (expected-flags :u32)))
  (ldr flags (:@ header (:$ arm::vectorH.flags)))
  (and flags flags (:$ (ash #xff (+ 8 arm::fixnumshift))))
  (cmp flags (:$ (:apply ash expected-type (+ 8 arm::fixnumshift))))
  (beq :ok)
  (mov expected-flags (:$ (:apply ash expected-type (+ 8 arm::fixnumshift))))
  (uuo-error-array-flags expected-flags header)
  :ok)

(define-arm-vinsn trap-unless-simple-1d-array (()
                                               ((vector :lisp)
                                                (expected-type :u8const))
                                               ((flags :u32)))
  (and flags vector (:$ arm::tagmask))
  (cmp flags (:$ arm::tag-misc))
  (ldrbeq flags (:@ vector (:$ arm::misc-subtag-offset)))
  (cmp flags (:$ expected-type))
  (beq :ok)
  (mov flags (:$ (:apply ash expected-type (+ 8 arm::fixnumshift))))
  (uuo-error-array-flags flags vector)
  :ok)
  
(define-arm-vinsn (subtract-constant :predicatable)
    (((dest :imm))
     ((src :imm)
      (const :s16const)))
  (sub dest src (:$ const)))



;; Bit-extraction & boolean operations


;; For some mind-numbing reason, IBM decided to call the most significant
;; bit in a 32-bit word "bit 0" and the least significant bit "bit 31"
;; (this despite the fact that it's essentially a big-endian architecture
;; (it was exclusively big-endian when this decision was made.))
;; We'll probably be least confused if we consistently use this backwards
;; bit ordering (letting things that have a "sane" bit-number worry about
;; it at compile-time or run-time (subtracting the "sane" bit number from
;; 31.))

#+later
(define-arm-vinsn extract-variable-bit (((dest :u8))
                                        ((src :u32)
                                         (bitnum :u8))
                                        ())
  (rotlw dest src bitnum)
  (extrwi dest dest 1 0))


(define-arm-vinsn (extract-variable-bit-fixnum :predicatable)
    (((dest :lisp))
     ((src :u32)
      (bitnum :u8))
     ((temp :u32)))
  (mov temp (:lsr src bitnum))
  (mov dest (:$ arm::fixnumone))
  (and dest dest (:lsl temp (:$ arm::fixnumshift))))



                           



(define-arm-vinsn compare (((crf :crf))
                           ((arg0 t)
                            (arg1 t))
                           ())
  (cmp arg0 arg1))

(define-arm-vinsn compare-to-nil (((crf :crf))
                                  ((arg0 t)))
  (cmp arg0 (:$ arm::nil-value)))

(define-arm-vinsn compare-logical (((crf :crf))
                                   ((arg0 t)
                                    (arg1 t))
                                   ())
  (cmp  arg0 arg1))

(define-arm-vinsn compare-immediate (((crf :crf))
                                     ((arg t)
                                      (imm :u32const)))
  (cmp arg (:$ imm)))

(define-arm-vinsn double-float-compare (((crf :crf))
                                        ((arg0 :double-float)
                                         (arg1 :double-float))
                                        ())
  (fcmped arg0 arg1)
  (fmstat))
              

(define-arm-vinsn (double-float+-2 :predicatable)
    (((result :double-float))
     ((x :double-float)
      (y :double-float)))
  (faddd result x y))



(define-arm-vinsn (double-float--2 :predicatable)
    (((result :double-float))
     ((x :double-float)
      (y :double-float)))
  (fsubd result x y))



(define-arm-vinsn (double-float*-2 :predicatable)
    (((result :double-float))
     ((x :double-float)
      (y :double-float)))
  (fmuld result x y))



(define-arm-vinsn (double-float/-2 :predicatable)
    (((result :double-float))
     ((x :double-float)
      (y :double-float)))
  (fdivd result x y))



(define-arm-vinsn (double-float-negate :predicatable) (((dest :double-float))
                                                       ((src :double-float)))
  (fnegd dest src))



(define-arm-vinsn single-float-compare (((crf :crf))
                                        ((arg0 :single-float)
                                         (arg1 :single-float))
                                        ())
  (fcmpes arg0 arg1)
  (fmstat))

(define-arm-vinsn (single-float+-2 :predicatable)
    (((result :single-float))
     ((x :single-float)
      (y :single-float))
     ())
  (fadds result x y))



(define-arm-vinsn (single-float--2 :predicatable)
    (((result :single-float))
     ((x :single-float)
      (y :single-float)))
  (fsubs result x y))



(define-arm-vinsn (single-float*-2 :predicatable)
    (((result :single-float))
     ((x :single-float)
      (y :single-float)))
  (fmuls result x y))



(define-arm-vinsn (single-float/-2 :predicatable)
    (((result :single-float))
     ((x :single-float)
      (y :single-float)))
  (fdivs result x y))



(define-arm-vinsn (single-float-negate :predicatable) (((dest :single-float))
                                                       ((src :single-float)))
  (fnegs dest src))


(define-arm-vinsn compare-unsigned (()
                                    ((arg0 :imm)
                                     (arg1 :imm))
                                    ())
  (cmp arg0 arg1))





;; Extract a constant bit (0-31) from src; make it be bit 31 of dest.
;; Bitnum is treated mod 32.
#+later
(define-arm-vinsn extract-constant-arm-bit (((dest :u32))
                                            ((src :imm)
                                             (bitnum :u16const))
                                            ())
  (rlwinm dest src (:apply + 1 bitnum) 31 31))



(define-arm-vinsn set-constant-arm-bit-to-variable-value (((dest :u32))
                                                          ((src :u32)
                                                           (bitval :u32) ; 0 or 1
                                                           (bitnum :u8const)))
  (cmp bitval (:$ 0))
  (biceq dest src (:$ (:apply ash 1 bitnum)))
  (orrne dest src (:$ (:apply ash 1 bitnum))))

(define-arm-vinsn (set-constant-arm-bit-to-1 :predicatable)
    (((dest :u32))
     ((src :u32)
      (bitnum :u8const)))
  (orr dest src (:$ (:apply ash 1 bitnum))))


(define-arm-vinsn (set-constant-arm-bit-to-0 :predicatable)
    (((dest :u32))
     ((src :u32)
      (bitnum :u8const)))
  (bic dest src (:$ (:apply ash 1 bitnum))))



                                               
;;; Operations on lists and cons cells

(define-arm-vinsn (%cdr :predicatable)
    (((dest :lisp))
     ((src :lisp)))
  (ldr dest (:@ src (:$ arm::cons.cdr))))

(define-arm-vinsn (%car :predicatable)
    (((dest :lisp))
     ((src :lisp)))
  (ldr dest (:@ src (:$ arm::cons.car))))

(define-arm-vinsn (%set-car :predicatable)
    (()
     ((cell :lisp)
      (new :lisp)))
  (str cell (:@ new (:$ arm::cons.car))))

(define-arm-vinsn (%set-cdr :predicatable)
    (()
     ((cell :lisp)
      (new :lisp)))
  (str cell (:@ new (:$ arm::cons.cdr))))


(define-arm-vinsn (load-adl :predicatable)
    (()
     ((n :u32const)))
  (mov nargs (:$ (:apply logand #x00ff0000 n)))
  ((:not (:pred = 0 (:apply logand #xff000000 n)))
   (orr nargs nargs (:$ (:apply logand #xff000000 n))))
  ((:not (:pred = 0 (:apply logand #x0000ff00 n)))
   (orr nargs nargs (:$ (:apply logand #x0000ff00 n))))
  ((:not (:pred = 0 (:apply logand #x000000ff n)))
   (orr nargs nargs (:$ (:apply logand #x000000ff n)))))
                            
(define-arm-vinsn (set-nargs :predicatable)
    (()
     ((n :s16const)))
  ((:pred arm::encode-arm-immediate (:apply ash n arm::word-shift))
   (mov nargs (:$ (:apply ash n arm::word-shift))))
  ((:not (:pred arm::encode-arm-immediate (:apply ash n arm::word-shift)))
   (mov nargs (:$ (:apply ash (:apply logand #xff00 n) arm::word-shift)))
   (orr nargs nargs (:$ (:apply ash (:apply logand #xff n) arm::word-shift)))))


(define-arm-vinsn (scale-nargs :predicatable)
    (()
     ((nfixed :s16const)))
  ((:pred > nfixed 0)
   (add nargs nargs (:$ (:apply - (:apply ash nfixed arm::word-shift))))))
                           


(define-arm-vinsn (vpush-register :push :node :vsp :predicatable)
    
    (()
     ((reg :lisp)))
  (str reg (:@! vsp (:$ (- arm::node-size)))))

(define-arm-vinsn (vpush-multiple-registers :push :node :multiple :vsp :predicatable)
    (()
     ((mask :u16const)))
  (stmdb (:! arm::vsp) (:$ mask)))

(define-arm-vinsn (vpush-register-arg :push :node :vsp :outgoing-argument :predicatable)
    (()
     ((reg :lisp)))
  (str reg (:@! vsp (:$ (- arm::node-size)))))

(define-arm-vinsn (vpush-register-arg :push :node :vsp :outgoing-argument :predicatable)
    (()
     ((reg :lisp)))
  (str reg (:@! vsp (:$ (- arm::node-size)))))

(define-arm-vinsn (vpush-xyz :push :node :vsp :predicatable)
    (() ())
  (stmdb (:! vsp) (arg_z arg_y arg_x)))

(define-arm-vinsn (vpush-yz :push :node :vsp :predicatable)
    (() ())
  (stmdb (:! vsp) (arg_z arg_y)))

(define-arm-vinsn (vpush-argregs :push :node :vsp) (()
                                                    ((num-fixed-args :u16const)))
  ((:pred = num-fixed-args 0)
   (cmp nargs (:$ 0))
   (beq :done))
  ((:pred < num-fixed-args 2)
   (cmp nargs (:$ (ash 2 arm::fixnumshift)))
   (strlo arg_z (:@! vsp (:$ (- arm::node-size))))
   (stmdbeq (:! vsp) (arg_z arg_y))
   (stmdbhi (:! vsp) (arg_z arg_y arg_x))
   :done)
  ((:pred = num-fixed-args 2)
   (cmp nargs (:$ (ash 2 arm::fixnumshift)))
   (stmdbeq (:! vsp) (arg_z arg_y))
   (stmdbhi (:! vsp) (arg_z arg_y arg_x)))
  ((:pred > num-fixed-args 2)
   (stmdb (:! vsp) (arg_z arg_y arg_x))))


(define-arm-vinsn (vpop-register :pop :node :vsp :predicatable)
    
    (((dest :lisp))
     ())
  (ldr dest (:@+ vsp (:$ arm::node-size))))

(define-arm-vinsn (pop-argument-registers :pop :node :vsp) (()
                                                            ())
  (cmp nargs (:$ 0))
  (beq :done)
  (cmp nargs (:$ (* 2 arm::node-size)))
  (ldrlt arg_z (:@+ vsp (:$ arm::node-size)))
  (ldmiaeq (:! vsp) (arg_z arg_y))
  (ldmiagt (:! vsp) (arg_z arg_y arg_x))
  :done)



(define-arm-vinsn (copy-node-gpr :predicatable)
    (((dest :lisp))
     ((src :lisp)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mov dest src)))

(define-arm-vinsn (copy-gpr :predicatable)
    (((dest t))
     ((src t)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mov dest src)))


(define-arm-vinsn (double-to-double :predicatable)
    (((dest :double-float))
     ((src :double-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fcpyd dest src)))

(define-arm-vinsn (single-to-single :predicatable)
    (((dest :single-float))
     ((src :single-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fcpys dest src)))

(define-arm-vinsn (complex-single-float-to-complex-single-float :predicatable)
    (((dest :complex-single-float))
     ((src :complex-single-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fcpyd dest src)))

(define-arm-vinsn (complex-double-float-to-complex-double-float :predicatable)
    (((dest :complex-double-float))
     ((src :complex-double-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fcpyd dest src)
   (fcpyd (:apply 1+ (:apply %hard-regspec-value dest))
          (:apply 1+ (:apply %hard-regspec-value src)))))

(define-arm-vinsn (vcell-ref :predicatable)
    (((dest :lisp))
     ((vcell :lisp)))
  (ldr dest (:@ vcell (:$ arm::misc-data-offset))))


(define-arm-vinsn make-vcell (((dest :lisp))
                              ((closed (:lisp :ne dest)))
                              ((header :u32)))
  (mov header (:$ (ash arm::value-cell.element-count arm::num-subtag-bits)))
  (orr header header (:$  arm::subtag-value-cell))
  (sub allocptr allocptr (:$ (- arm::value-cell.size arm::fulltag-misc)))
  (ldr dest (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr dest)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str header (:@ allocptr (:$ arm::misc-header-offset)))
  (mov dest allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  (str closed (:@ dest (:$ arm::value-cell.value))))

(define-arm-vinsn (make-stack-vcell :predicatable)
    (((dest :lisp))
     ((closed :lisp))
     ((header :u32)))
  (mov header (:$ (ash arm::value-cell.element-count arm::num-subtag-bits)))
  (orr header header (:$ arm::subtag-value-cell))
  (stmdb (:! sp) (closed header))
  (add dest sp (:$ arm::fulltag-misc)))

(define-arm-vinsn (make-stack-cons :predicatable)
    (((dest :lisp))
     ((car :lisp) (cdr :lisp))
     ((header (:u32 #.arm::imm0))
      (zero (:u32 #.arm::imm1))))
  (mov header (:$ (ash 3 arm::num-subtag-bits)))
  (orr header header (:$ arm::subtag-value-cell))
  (mov zero (:$ 0))
  ((:pred <
          (:apply %hard-regspec-value cdr)
          (:apply %hard-regspec-value car))
   (stmdb (:! sp) (car cdr zero header)))
  ((:not (:pred <
                (:apply %hard-regspec-value cdr)
                (:apply %hard-regspec-value car)))
   (stmdb (:! sp) (cdr car zero header))
   (str car (:@ sp (:$ 12)))
   (str cdr (:@ sp (:$ 8))))
  (add dest sp (:$ (+ arm::dnode-size arm::fulltag-cons))))


(define-arm-vinsn (%closure-code% :predicatable)
    (((dest :lisp))
     ())
  (mov dest (:$ arm::nil-value))
  (ldr dest (:@ dest (:$ (:apply + arm::symbol.vcell (arm::nrs-offset %closure-code%))))))

;;; DEST pretty much has to be the LR, which won't stay alive very long.
(define-arm-vinsn %codevector-entry (((dest t))
                                     ((cv :lisp)))
  (add dest cv (:$ arm::misc-data-offset)))


(define-arm-vinsn (single-float-bits :predicatable)
    (((dest :u32))
     ((src :lisp)))
  (ldr dest (:@ src (:$ arm::single-float.value))))

(define-arm-vinsn (call-subprim :call :subprim) (()
                                                      ((spno :s32const)))
  (sploadlr spno)
  (blx lr))

(define-arm-vinsn (jump-subprim :jumpLR) (()
                                          ((spno :s32const)))
  (spjump spno))

;;; Same as "call-subprim", but gives us a place to 
;;; track args, results, etc.
(define-arm-vinsn (call-subprim-0 :call :subprim) (((dest t))
                                                        ((spno :s32const)))
  (sploadlr spno)
  (blx lr))

(define-arm-vinsn (call-subprim-1 :call :subprim) (((dest t))
                                                        ((spno :s32const)
                                                         (z t)))
  (sploadlr spno)
  (blx lr))
  
(define-arm-vinsn (call-subprim-2 :call :subprim) (((dest t))
                                                        ((spno :s32const)
                                                         (y t)
                                                         (z t)))
  (sploadlr spno)
  (blx lr))

(define-arm-vinsn (call-subprim-3 :call :subprim) (((dest t))
                                                        ((spno :s32const)
                                                         (x t)
                                                         (y t)
                                                         (z t)))
  (sploadlr spno)
  (blx lr))



(define-arm-vinsn (ref-interrupt-level :predicatable)
    (((dest :imm))
     ()
     ((temp :u32)))
  (ldr temp (:@ rcontext (:$ arm::tcr.tlb-pointer)))
  (ldr dest (:@ temp (:$ arm::INTERRUPT-LEVEL-BINDING-INDEX))))

                         
;;; Unconditional (pc-relative) branch
(define-arm-vinsn (jump :jump :predicatable)
    (()
     ((label :label)))
  (b label))

(define-arm-vinsn (call-label :call) (()
                                      ((label :label)))
  (bl label))

;;; just like JUMP, only (implicitly) asserts that the following 
;;; code is somehow reachable.
(define-arm-vinsn (non-barrier-jump :xref) (()
                                            ((label :label)))
  (b label))

(define-arm-vinsn lock-constant-pool (()
                                      ())
  (:lock-constant-pool))

(define-arm-vinsn unlock-constant-pool (()
                                      ())
  (:unlock-constant-pool))

(define-arm-vinsn set-carry-if-fixnum-in-range
    (((idx :u32)
      (flags :crf))
     ((reg :imm)
      (minval :s32const)
      (maxval :u32const))
     ((temp :s32)))
  (mov idx (:asr reg (:$  arm::fixnumshift)))
  ((:not (:pred zerop minval))
   ((:pred arm::encode-arm-immediate minval)
    (sub idx idx (:$ minval)))
   ((:not (:pred arm::encode-arm-immediate minval))
    ((:pred arm::encode-arm-immediate (:apply - minval))
     (add idx idx (:$ (:apply - minval))))
    ((:not (:pred arm::encode-arm-immediate (:apply - minval)))
     ((:and (:pred >=  minval 0)
            (:pred < minval #x10000))
      (mov temp (:$ (:apply logand #xff00 minval)))
      (orr temp temp (:$ (:apply logand #xff minval))))
     ((:not (:and
             (:pred >=  minval 0)
             (:pred < minval #x10000)))
      (mov temp (:$ (:apply logand #xff minval)))
      ((:not (:pred = 0 (:apply logand #xff00 minval)))
       (orr temp temp (:$ (:apply logand #xff00 minval))))
      ((:not (:pred = 0 (:apply logand #xff0000 minval)))
       (orr temp temp (:$ (:apply logand #xff0000 minval))))
      ((:not (:pred = 0 (:apply logand #xff000000 minval)))
       (orr temp temp (:$ (:apply logand #xff000000 minval)))))
     (sub idx idx temp))))
  ((:pred arm::encode-arm-immediate maxval)
   (cmp idx (:$ maxval)))
  ((:not (:pred arm::encode-arm-immediate maxval))
   ((:pred arm::encode-arm-immediate (:apply lognot maxval))
    (cmn idx (:$ (:apply lognot maxval))))
   ((:not (:pred arm::encode-arm-immediate (:apply lognot maxval)))
    ((:pred (< maxval #x10000))     
     (mov temp (:$ (:apply logand #xff00 maxval)))
     (orr temp temp (:$ (:apply logand #xff maxval))))
    ((:not (:pred (< maxval #x10000)))
      (mov temp (:$ (:apply logand #xff maxval)))
      ((:not (:pred = 0 (:apply logand #xff00 maxval)))
       (orr temp temp (:$ (:apply logand #xff00 maxval))))
      ((:not (:pred = 0 (:apply logand #xff0000 maxval)))
       (orr temp temp (:$ (:apply logand #xff0000 maxval))))
      ((:not (:pred = 0 (:apply logand #xff000000 maxval)))
       (orr temp temp (:$ (:apply logand #xff000000 maxval)))))
    (cmp idx temp))))

(define-arm-vinsn (ijmp :branch) (()
                                  ((idx :u32)))
  (add arm::pc arm::pc (:lsl idx (:$ 2)))
  (nop))

(define-arm-vinsn nop (()
                       ())
  (nop))



     
                                   

(define-arm-vinsn (cbranch-true :branch) (()
                                          ((label :label)
                                           (crf :crf)
                                           (crbit :u8const)))
  (b (:? crbit) label))

(define-arm-vinsn (cbranch-false :branch) (()
                                           ((label :label)
                                            (crf :crf)
                                            (crbit :u8const)))
  (b (:~ crbit) label))

(define-arm-vinsn cond->boolean (((dest :imm))
                                 ((cond :u8const)))
  (mov dest (:$ arm::nil-value))
  (add (:? cond) dest dest (:$ arm::t-offset)))



(define-arm-vinsn (lisp-word-ref :predicatable)
    (((dest t))
     ((base t)
      (offset t)))
  (ldr dest (:@ base offset)))

(define-arm-vinsn (lisp-word-ref-c :predicatable)
    (((dest t))
     ((base t)
      (offset :s16const)))
  (ldr dest (:@ base (:$ offset))))

  

;; Load an unsigned, 32-bit constant into a destination register.
(define-arm-vinsn (lri :constant-ref :predicatable)
    (((dest :imm))
     ((intval :u32const))
     ())
  ((:pred arm::encode-arm-immediate intval)
   (mov dest (:$ intval)))
  ((:not (:pred arm::encode-arm-immediate intval))
   ((:pred arm::encode-arm-immediate (:apply lognot intval))
    (mvn dest (:$ (:apply lognot intval))))
   ((:not (:pred arm::encode-arm-immediate (:apply lognot intval)))
    ((:not (:pred = 0 (:apply logand #xff intval)))
     (mov dest (:$ (:apply logand #xff intval))))
    ((:not (:pred = 0 (:apply logand #xff00 intval)))
     ((:pred = 0 (:apply logand #xff intval))
      (mov dest (:$ (:apply logand #xff00 intval))))
     ((:not (:pred = 0 (:apply logand #xff intval)))
      (orr dest dest (:$ (:apply logand #xff00 intval)))))
    ((:not (:pred = 0 (:apply logand #xff0000 intval)))
      ((:pred = 0 (:apply logand #xffff intval))
       (mov dest (:$ (:apply logand #xff0000 intval))))
      ((:not (:pred = 0 (:apply logand #xffff intval)))
       (orr dest dest (:$ (:apply logand #xff0000 intval)))))
    ((:not (:pred = 0 (:apply logand #xff000000 intval)))
     (orr dest dest (:$ (:apply logand #xff000000 intval)))))))





(define-arm-vinsn (alloc-eabi-c-frame :predicatable)
    (()
     ((n-c-args :u16const))
     ((header :u32)
      (prevsp :imm)))
  (mov header (:$ (:apply ash (:apply + 1 (:apply logandc2 (:apply + 4 4 1 n-c-args) 1)) arm::num-subtag-bits)))
  (orr header header (:$ arm::subtag-u32-vector))
  (mov prevsp sp)
  (str header (:@! sp (:$ (:apply - (:apply ash (:apply + 2 (:apply logandc2 (:apply + 4 4 1 n-c-args) 1)) arm::word-shift)))))
  (str prevsp (:@ sp (:$ 4))))

(define-arm-vinsn (alloc-variable-eabi-c-frame :predicatable)
    (()
     ((n-c-args :lisp))
     ((header :u32)
      (size :imm)
      (prevsp :imm)))
  (add size n-c-args (:$ (ash (+ 4 4 1) arm::word-shift)))
  (bic size size (:$ arm::fixnumone))
  (add size size (:$ arm::fixnumone))
  (mov prevsp sp)
  (mov header (:lsl size (:$ (- arm::num-subtag-bits arm::fixnumshift))))
  (add size size (:$ arm::fixnumone))
  (orr header header (:$ arm::subtag-u32-vector))
  (str header (:-@! sp size))
  (str prevsp (:@ sp (:$ 4))))



;;; We should rarely have to do this - (#_foo x y (if .. (return-from ...)))
;;; is one of the few cases that I can think of - but if we ever do, we
;;; might as well exploit the fact that we stored the previous sp at
;;; offset 4 in the C frame.
(define-arm-vinsn (discard-c-frame :csp :pop :discard :predicatable)
    (()
     ())
  (ldr sp (:@ sp (:$ 4))))

;;; Soft-float return.
(define-arm-vinsn (gpr-to-single-float :predicatable)
    (((dest :single-float))
     ((src :u32)))
  (fmsr dest src))

(define-arm-vinsn (gpr-pair-to-double-float :predicatable)
    (((dest :double-float))
     ((low :u32)
      (high :u32)))
  (fmdrr dest low high))


(define-arm-vinsn (set-eabi-c-arg :predicatable)
    (()
     ((argval :u32)
      (argnum :u16const)))
  (str argval (:@ sp (:$ (:apply + arm::dnode-size (:apply ash argnum arm::word-shift))))))


(define-arm-vinsn (set-single-eabi-c-arg :predicatable)
    (()
     ((argval :single-float)
      (argnum :u16const)))
  (fsts argval (:@ sp (:$ (:apply + arm::dnode-size (:apply ash argnum arm::word-shift))))))

(define-arm-vinsn (set-double-eabi-c-arg :predicatable)
    (()
     ((argval :double-float)
      (argnum :u16const)))
  (fstd argval (:@ sp (:$ (:apply + arm::dnode-size (:apply ash argnum arm::word-shift))))))



(define-arm-vinsn (load-nil :constant-ref :predicatable)
    (((dest t))
     ())
  (mov dest (:$ arm::nil-value)))

(define-arm-vinsn (load-t :constant-ref :predicatable)
    (((dest t))
     ())
  (mov dest (:$ arm::nil-value))
  (add dest dest (:$ arm::t-offset)))



(define-arm-vinsn (ref-constant :constant-ref :predicatable)
    (((dest :lisp))
     ((src :s16const)))
  (ldr dest (:@ fn (:$ (:apply + arm::misc-data-offset (:apply ash (:apply + src 2) 2))))))

(define-arm-vinsn (ref-indexed-constant :predicatable)
    (((dest :lisp))
     ((idxreg :s32)))
  (ldr dest (:@ arm::fn idxreg)))


(define-arm-vinsn cons (((dest :lisp))
                        ((newcar :lisp)
                         (newcdr :lisp))
                        ((allocbase :imm)))
  (sub allocptr allocptr (:$ (- arm::cons.size arm::fulltag-cons)))
  (ldr allocbase (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr allocbase)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str newcdr (:@ allocptr (:$ arm::cons.cdr)))
  (str newcar (:@ allocptr (:$ arm::cons.car)))
  (mov dest allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask)))



;; subtag had better be a ARM-NODE-SUBTAG of some sort!
(define-arm-vinsn %arm-gvector (((dest :lisp))
                                ((Rheader :u32) 
                                 (nbytes :u32const))
                                ((immtemp0 :u32)
                                 (nodetemp :lisp)))
  
  (sub allocptr allocptr (:$ (:apply logand #xff
                                     (:apply -
                                             (:apply logand (lognot 7)
                                                     (:apply + (+ 7 4) nbytes))
                                             arm::fulltag-misc))))
  ((:pred > (:apply -
                    (:apply logand (lognot 7)
                            (:apply + (+ 7 4) nbytes))
                    arm::fulltag-misc) #xff)
   (sub allocptr allocptr (:$ (:apply logand #xff00
                                      (:apply -
                                              (:apply logand (lognot 7)
                                                      (:apply + (+ 7 4) nbytes))
                                              arm::fulltag-misc)))))
  (ldr dest (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr dest)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str Rheader (:@ allocptr (:$ arm::misc-header-offset)))
  (mov dest allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  ((:not (:pred = nbytes 0))
   (mov immtemp0 (:$ (:apply logand #xff (:apply + arm::misc-data-offset nbytes))))
   ((:not (:pred = (:apply logand #xff00 (:apply + arm::misc-data-offset nbytes)) 0))
    (orr immtemp0 immtemp0 (:$ (:apply logand #xff00 (:apply + arm::misc-data-offset nbytes)))))
   :loop
   (sub immtemp0 immtemp0 (:$ 4))
   (cmp immtemp0 (:$ arm::misc-data-offset))
   (ldr nodetemp (:@+ vsp (:$ arm::node-size)))
   (str nodetemp (:@ dest immtemp0))
   (bne :loop)))

;; allocate a small (phys size <= 32K bytes) misc obj of known size/subtag
(define-arm-vinsn %alloc-misc-fixed (((dest :lisp))
                                     ((Rheader :u32)
                                      (nbytes :u32const)))
  (sub allocptr allocptr (:$ (:apply
                              logand #xff
                              (:apply -  (:apply logand (lognot 7)
                                                 (:apply + (+ 7 4) nbytes))
                                      arm::fulltag-misc))))
  ((:pred > (:apply - (:apply logand (lognot 7)
                              (:apply + (+ 7 4) nbytes))
                    arm::fulltag-misc) #xff)
   (sub allocptr allocptr (:$ (:apply logand #xff00
                                      (:apply -
                                              (:apply logand (lognot 7)
                                                      (:apply + (+ 7 4) nbytes))
                                              arm::fulltag-misc)))))
  (ldr dest (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr dest)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str Rheader (:@ allocptr (:$ arm::misc-header-offset)))
  (mov dest allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask)))

(define-arm-vinsn (vstack-discard :vsp :pop :discard :predicatable)
    (()
     ((nwords :u32const)))
  ((:not (:pred = nwords 0))
   (add vsp vsp (:$ (:apply ash nwords arm::word-shift)))))




(define-arm-vinsn (vframe-load :predicatable)
    (((dest :lisp))
     ((frame-offset :u16const)
      (cur-vsp :u16const)))
  ((:pred < (:apply - (:apply - cur-vsp 4) frame-offset) 4096)
   (ldr dest (:@ vsp (:$ (:apply - (:apply - cur-vsp 4) frame-offset)))))
  ((:pred >= (:apply - (:apply - cur-vsp 4) frame-offset) 4096)
   ((:pred arm::encode-arm-immediate (:apply - (:apply - cur-vsp 4) frame-offset))
    (mov dest (:$ (:apply - (:apply - cur-vsp 4) frame-offset))))
   ((:not (:pred arm::encode-arm-immediate (:apply - (:apply - cur-vsp 4) frame-offset)))
    (mov dest (:$ (:apply logand #xff00 (:apply - (:apply - cur-vsp 4) frame-offset))))
    (orr dest dest (:$ (:apply logand #xff (:apply - (:apply - cur-vsp 4) frame-offset)))))
   (ldr dest (:@ vsp dest))))



(define-arm-vinsn (vframe-store :predicatable)
    (()
     ((src :lisp)
      (frame-offset :u16const)
      (cur-vsp :u16const)))
  ((:pred < (:apply - (:apply - cur-vsp 4) frame-offset) 4096)
   (str src (:@ vsp (:$ (:apply - (:apply - cur-vsp 4) frame-offset)))))
  ((:pred >= (:apply - (:apply - cur-vsp 4) frame-offset) 4096)
   (str rcontext (:@! vsp (:$ (- arm::node-size))))
   ((:pred arm::encode-arm-immediate (:apply - cur-vsp frame-offset))
    (mov rcontext (:$ (:apply - cur-vsp frame-offset))))
   ((:not (:pred arm::encode-arm-immediate (:apply - cur-vsp frame-offset)))
    (mov rcontext (:$ (:apply logand #xff00 (:apply - cur-vsp frame-offset))))
    (orr rcontext rcontext (:$ (:apply logand #xff (:apply - cur-vsp frame-offset)))))
   (str src (:+@ vsp rcontext))
   (ldr rcontext (:@+ vsp (:$ arm::node-size))))) 

(define-arm-vinsn (load-vframe-address :predicatable)
    (((dest :imm))
     ((offset :s16const)))
  (add dest vsp (:$ offset)))

(define-arm-vinsn (copy-lexpr-argument :predicatable)
    (()
     ()
     ((temp :lisp)))
  (ldr temp (:@ vsp nargs))
  (str temp (:@! vsp (:$ (- arm::node-size)))))

;;; Boxing/unboxing of integers.

;;; Treat the low 8 bits of VAL as an unsigned integer; set RESULT to the equivalent fixnum.
(define-arm-vinsn (u8->fixnum :predicatable)
    (((result :imm)) 
     ((val :u8)) 
     ())
  (mov result (:lsl val (:$ 24)))
  (mov result (:lsr result (:$ (- 24 arm::fixnumshift)))))

;;; Treat the low 8 bits of VAL as a signed integer; set RESULT to the equivalent fixnum.
(define-arm-vinsn (s8->fixnum :predicatable)
    (((result :imm)) 
     ((val :s8)) 
     ())
  (mov result (:lsl val (:$ 24)))
  (mov result (:asr result (:$ (- 24 arm::fixnumshift)))))


;;; Treat the low 16 bits of VAL as an unsigned integer; set RESULT to the equivalent fixnum.
(define-arm-vinsn (u16->fixnum :predicatable)
    (((result :imm)) 
     ((val :u16)) 
     ())
  (mov result (:lsl val (:$ 16)))
  (mov result (:lsr result (:$ (- 16 arm::fixnumshift)))))

;;; Treat the low 16 bits of VAL as a signed integer; set RESULT to the equivalent fixnum.
(define-arm-vinsn (s16->fixnum :predicatable)
    (((result :imm)) 
     ((val :s16)) 
     ())
  (mov result (:lsl val (:$ 16)))
  (mov result (:asr result (:$ (- 16 arm::fixnumshift)))))

(define-arm-vinsn (fixnum->s16 :predicatable)
    (((result :s16))
     ((src :imm)))
  (mov result (:asr src (:$ arm::fixnumshift))))

;;; A signed 32-bit untagged value can be at worst a 1-digit bignum.
;;; There should be something very much like this that takes a stack-consed
;;; bignum result ...
(define-arm-vinsn s32->integer (((result :lisp))
                                ((src :s32))
                                ((temp :s32)))        
  (adds temp src src)
  (addsvc result temp temp)
  (bvc :done)
  (mov temp (:$ (logand #xff00 arm::one-digit-bignum-header)))
  (orr temp temp (:$ (logand #xff arm::one-digit-bignum-header)))
  (add allocptr allocptr (:$ (- arm::fulltag-misc 8)))
  (ldr result (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr result)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str temp (:@ allocptr (:$ arm::misc-header-offset)))
  (mov result allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  (str src (:@ result (:$ arm::misc-data-offset)))
  :done)


;;; An unsigned 32-bit untagged value can be either a 1 or a 2-digit bignum.
(define-arm-vinsn u32->integer (((result :lisp))
                                ((src :u32))
                                ((temp :s32)
                                 (size :u32)))
  (tst src (:$ #xe0000000))
  (moveq result (:lsl src (:$ arm::fixnumshift)))
  (beq :done)
  (cmp src (:$ 0))
  (mov temp (:$ arm::subtag-bignum))
  (movgt size (:$ (- (* 1 arm::dnode-size) arm::fulltag-misc)))
  (orrgt temp temp (:$ (ash 1 arm::num-subtag-bits)))
  (movlt size (:$ (- (* 2 arm::dnode-size) arm::fulltag-misc)))
  (orrlt temp temp (:$ (ash 2 arm::num-subtag-bits)))
  (sub allocptr allocptr size)
  (ldr result (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr result)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str temp (:@ allocptr (:$ arm::misc-header-offset)))
  (mov result allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  (str src (:@ result (:$ arm::misc-data-offset)))
  :done)

(define-arm-vinsn (u16->u32 :predicatable)
    (((dest :u32))
     ((src :u16)))
  (mov dest (:$ #xff))
  (orr dest dest (:$ #xff00))
  (and dest dest src))

(define-arm-vinsn (u8->u32 :predicatable)
    (((dest :u32))
     ((src :u8)))
  (and dest src (:$ #xff)))


(define-arm-vinsn (s16->s32 :predicatable)
    (((dest :s32))
     ((src :s16)))
  (mov dest (:lsl src (:$ 16)))
  (mov dest (:asr src (:$ 16))))

(define-arm-vinsn (s8->s32 :predicatable)
    (((dest :s32))
     ((src :s8)))
  (mov dest (:lsl src (:$ 24)))
  (mov dest (:asr src (:$ 24))))


;;; ... of floats ...

;;; Heap-cons a double-float to store contents of FPREG.  Hope that we don't do
;;; this blindly.
(define-arm-vinsn (double->heap :sets-lr) (((result :lisp)) ; tagged as a double-float
                                ((fpreg :double-float)) 
                                ((header-temp (:u32 #.arm::imm0))
                                 (high (:u32 #.arm::imm1))))
  (mov header-temp (:$ (logand #xff00 arm::double-float-header)))
  (orr header-temp header-temp (:$ (logand #xff arm::double-float-header)))
  (sub allocptr allocptr (:$ (- arm::double-float.size arm::fulltag-misc)))
  (ldr result (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr result)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str header-temp (:@ allocptr (:$ arm::misc-header-offset)))
  (mov result allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  (add lr result (:$ arm::double-float.value))
  (fstd fpreg (:@ lr (:$ 0)))
  (mov lr (:$ 0)))

(define-arm-vinsn (complex-double-float->heap :sets-lr) (((result :lisp)) ; tagged as a double-float
                                ((fpreg :complex-double-float)) 
                                ((header-temp (:u32 #.arm::imm0))
                                 (high (:u32 #.arm::imm1))))
  (mov header-temp (:$ (logand #xff00 arm::complex-double-float-header)))
  (orr header-temp header-temp (:$ (logand #xff arm::complex-double-float-header)))
  (sub allocptr allocptr (:$ (- arm::complex-double-float.size arm::fulltag-misc)))
  (ldr result (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr result)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str header-temp (:@ allocptr (:$ arm::misc-header-offset)))
  (mov result allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  (add lr result (:$ arm::complex-double-float.realpart))
  (fstmiad fpreg lr 2)
  (mov lr (:$ 0)))


;;; This is about as bad as heap-consing a double-float.  (In terms of
;;; verbosity).  Wouldn't kill us to do either/both out-of-line, but
;;; need to make visible to compiler so unnecessary heap-consing can
;;; be elided.
(define-arm-vinsn (single->node :sets-lr)
    (((result :lisp)) ; tagged as a single-float
     ((fpreg :single-float))
     ((header-temp :u32)))
  (mov header-temp (:$ (logand #xff00 arm::single-float-header)))
  (orr header-temp header-temp (:$ (logand #xff arm::single-float-header)))
  (sub allocptr allocptr (:$ (- arm::single-float.size arm::fulltag-misc)))
  (ldr result (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr result)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str header-temp (:@ allocptr (:$ arm::misc-header-offset)))
  (mov result allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  (add lr result (:$ arm::single-float.value))
  (fsts fpreg (:@ lr (:$ 0)))
  (mov lr (:$ 0)))

(define-arm-vinsn (complex-single-float->node :sets-lr)
    (((result :lisp)) ; tagged as a complex-single-float
     ((fpreg :complex-single-float))
     ((header-temp :u32)))
  (mov header-temp (:$ (logand #xff00 arm::complex-single-float-header)))
  (orr header-temp header-temp (:$ (logand #xff arm::complex-single-float-header)))
  (sub allocptr allocptr (:$ (- arm::complex-single-float.size arm::fulltag-misc)))
  (ldr result (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr result)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str header-temp (:@ allocptr (:$ arm::misc-header-offset)))
  (mov result allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  (add lr result (:$ arm::complex-single-float.realpart))
  (fstd fpreg (:@ lr (:$ 0)))
  (mov lr (:$ 0)))



;;; "dest" is preallocated, presumably on a stack somewhere.
(define-arm-vinsn (store-double :predicatable :sets-lr)
    (()
     ((dest :lisp)
      (source :double-float)))
  (add lr dest (:$ arm::double-float.value))
  (fstd source (:@ lr (:$ 0)))
  (mov lr (:$ 0)))

(define-arm-vinsn (get-double :predicatable :sets-lr)
    (((target :double-float))
     ((source :lisp)))
  (add lr source (:$ arm::double-float.value))
  (fldd target (:@ lr (:$ 0)))
  (mov lr (:$ 0)))

(define-arm-vinsn (get-complex-single-float :predicatable :sets-lr)
    (((target :complex-single-float))
     ((source :lisp)))
  (add lr source (:$ arm::complex-single-float.realpart))
  (fldd target (:@ lr (:$ 0)))
  (mov lr (:$ 0)))

(define-arm-vinsn (get-complex-double-float :predicatable :sets-lr)
    (((target :complex-double-float))
     ((source :lisp)))
  (add lr source (:$ arm::complex-double-float.realpart))
  (fldmiad target lr 2)
  (mov lr (:$ 0)))

;;; Extract a double-float value, typechecking in the process.
;;; IWBNI we could simply call the "trap-unless-typecode=" vinsn here,
;;; instead of replicating it ..

(define-arm-vinsn (get-double? :sets-lr)
    (((target :double-float))
     ((source :lisp))
     ((tag :u8)))
  (and tag source (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ source (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-double-float))
  (beq :ok)
  (uuo-error-reg-not-xtype source (:$ arm::subtag-double-float))
  :ok
  (add lr source (:$ arm::double-float.pad))
  (fldd target (:@ lr (:$ (- arm::double-float.value arm::double-float.pad)))))
  

(define-arm-vinsn double-to-single (((result :single-float))
                                    ((arg :double-float)))
  (fcvtsd result arg))



(define-arm-vinsn single-to-double (((result :double-float))
                                    ((arg :single-float)))
  (fcvtds result arg))



(define-arm-vinsn (store-single :predicatable :sets-lr)
    (()
     ((dest :lisp)
      (source :single-float)))
  (add arm::lr dest (:$ arm::single-float.value))
  (fsts source (:@ arm::lr (:$ 0)))
  (mov lr (:$ 0)))

(define-arm-vinsn (get-single :predicatable :sets-lr)
    (((target :single-float))
     ((source :lisp)))
  (add arm::lr source (:$ arm::single-float.value))
  (flds target (:@ arm::lr (:$ 0)))
  (mov lr (:$ 0)))

;;; ... of characters ...


(define-arm-vinsn (character->fixnum :predicatable)
    (((dest :lisp))
     ((src :lisp))
     ())
  (bic dest src (:$ arm::subtag-mask))
  (mov dest (:lsr dest (:$ (- arm::charcode-shift arm::fixnumshift)))))

(define-arm-vinsn (character->code :predicatable)
    (((dest :u32))
     ((src :lisp)))
  (mov dest (:lsr src (:$ arm::charcode-shift))))


(define-arm-vinsn fixnum->char (((dest :lisp))
                                ((src :imm))
                                ((tempa :u32)
                                 (tempb :u32)))
  (mov tempb (:$ #xff))
  (orr tempb tempb (:$ #x7f00))
  (mov tempa (:lsr src (:$ (+ arm::fixnumshift 1))))
  (cmp tempa tempb)
  (mov tempa (:lsr src (:$ (+ arm::fixnumshift 11))))
  (beq :bad)
  (cmp tempa (:$ 27))
  (mov dest (:lsl src (:$ (- arm::charcode-shift arm::fixnumshift))))
  :bad
  (moveq dest (:$ arm::nil-value))
  (addne dest dest (:$ arm::subtag-character)))

;;; src is known to be a code for which CODE-CHAR returns non-nil.
(define-arm-vinsn (code-char->char :predicatable)
    (((dest :lisp))
     ((src :imm))
     ())
  (mov dest (:lsl src (:$ (- arm::charcode-shift arm::fixnum-shift))))
  (orr dest dest (:$ arm::subtag-character)))

(define-arm-vinsn (u32->char :predicatable)
    (((dest :lisp))
     ((src :u32))
     ())
  (mov dest (:lsl src (:$ arm::charcode-shift)))
  (orr dest dest (:$ arm::subtag-character)))

;; ... Macptrs ...

(define-arm-vinsn (deref-macptr :predicatable)
    (((addr :address))
     ((src :lisp))
     ())
  (ldr addr (:@ src (:$ arm::macptr.address))))

(define-arm-vinsn (set-macptr-address :predicatable)
    (()
     ((addr :address)
      (src :lisp))
     ())
  (str addr (:@ src (:$ arm::macptr.address))))


(define-arm-vinsn macptr->heap (((dest :lisp))
                                ((address :address))
                                ((header :u32)))
  (mov header (:$ (logand #xff00 arm::macptr-header)))
  (orr header header (:$ (logand #xff arm::macptr-header)))
  (sub allocptr allocptr (:$ (- arm::macptr.size arm::fulltag-misc)))
  (ldr dest (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr dest)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str header (:@ allocptr (:$ arm::misc-header-offset)))
  (mov dest allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  ;; It's not necessary to zero out the domain/type fields, since newly
  ;; heap-allocated memory's guaranteed to be 0-filled.
  (str address (:@ dest (:$ arm::macptr.address))))

(define-arm-vinsn (macptr->stack :predicatable)
    (((dest :lisp))
     ((address :address))
     ((header :u32)))
  (mov header (:$ (logand #xff00 arm::macptr-header)))
  (orr header header (:$ (logand #xff arm::macptr-header)))
  (str header (:@! sp (:$ (- arm::macptr.size))))
  (mov header (:$ 0))
  (str header  (:@ sp (:$ (+ arm::fulltag-misc arm::macptr.domain))))
  (str header  (:@ sp (:$ (+ arm::fulltag-misc arm::macptr.type))))
  (str address (:@ sp (:$ (+ arm::fulltag-misc arm::macptr.address))))
  (add dest sp (:$ arm::fulltag-misc)))


  


(define-arm-vinsn (adjust-vsp :predicatable :vsp :pop :discard)
    (()
     ((amount :s16const)))
  (add vsp vsp (:$ amount)))

(define-arm-vinsn (adjust-sp :predicatable)
    (()
     ((amount :s16const)))
  (add sp sp (:$ amount)))

;; Arithmetic on fixnums & unboxed numbers

(define-arm-vinsn (u32-lognot :predicatable)
    (((dest :u32))
     ((src :u32))
     ())
  (mvn dest src))

(define-arm-vinsn (fixnum-lognot :predicatable)
    (((dest :imm))
     ((src :imm))
     ((temp :u32)))
  (mvn temp src)
  (bic dest temp (:$ arm::fixnummask)))


(define-arm-vinsn negate-fixnum-set-flags  (((dest :lisp)
                                             (flags :crf))
                                            ((src :imm)))
  (rsbs dest src (:$ 0)))
  
  
                                                  
                                       
(define-arm-vinsn (negate-fixnum-no-ovf :predicatable)
    (((dest :lisp))
     ((src :imm)))
  (rsb dest src (:$ 0)))
  

(define-arm-vinsn (logior-immediate :predicatable)
    (((dest :imm))
     ((src :imm)
      (imm :u32const)))
  (orr dest src (:$ imm)))

                           
                           
(define-arm-vinsn (%logior2 :predicatable)
    (((dest :imm))
     ((x :imm)
      (y :imm))
     ())
  (orr dest x y))

(define-arm-vinsn (logand-immediate :predicatable)
    (((dest :imm))
     ((src :imm)
      (imm :u32const)))
  (and dest src (:$ imm)))


(define-arm-vinsn (%logand2 :predicatable)
    (((dest :imm))
     ((x :imm)
      (y :imm))
     ())
  (and dest x y))

(define-arm-vinsn (logxor-immediate :predicatable)
    (((dest :imm))
     ((src :imm)
      (imm :u32const)))
  (eor dest src (:$ imm)))
                                    

                               

(define-arm-vinsn (%logxor2 :predicatable)
    (((dest :imm))
     ((x :imm)
      (y :imm))
     ())
  (eor dest x y))

;;; ARM register shifts shift by the low byte of RS.
(define-arm-vinsn (%ilsl :predicatable)
    (((dest :imm))
     ((count :imm)
      (src :imm))
     ((temp :u32)))
  (mov temp (:asr count (:$ arm::fixnumshift)))
  (mov dest (:lsl src temp)))

;;; Shift by a constant = -> shift by 32.  Don't do that.
(define-arm-vinsn (%ilsl-c :predicatable)
    (((dest :imm))
     ((count :u8const)
      (src :imm)))
  ((:pred = count 0)
   (mov dest src))
  ((:not (:pred = count 0))
   (mov dest (:lsl src (:$ (:apply logand count 31))))))


(define-arm-vinsn (%ilsr-c :predicatable)
    (((dest :imm))
     ((count :u8const)
      (src :imm))
     ((temp :s32)))
  (mov temp (:lsr src (:$ count)))
  (bic dest temp (:$ arm::fixnummask)))


(define-arm-vinsn (%iasr :predicatable)
    (((dest :imm))
     ((count :imm)
      (src :imm))
     ((temp :s32)))
  (mov temp (:asr count (:$ arm::fixnumshift)))
  (mov temp (:asr src temp))
  (bic dest temp (:$ arm::fixnummask)))

(define-arm-vinsn %iasr-c (((dest :imm))
                           ((count :u8const)
                            (src :imm))
                           ((temp :s32)))
  ((:pred = count 0)
   (mov dest src))
  ((:not (:pred = count 0))
   (mov temp (:asr src (:$ count)))
   (bic dest temp (:$ arm::fixnummask))))

(define-arm-vinsn (%ilsr :predicatable)
    (((dest :imm))
     ((count :imm)
      (src :imm))
     ((temp :s32)))
  (mov temp (:asr count (:$ arm::fixnumshift)))
  (mov temp (:lsr src temp))
  (bic dest temp (:$ arm::fixnummask)))


(define-arm-vinsn (%ilsr-c :predicatable)
    (((dest :imm))
     ((count :u8const)
      (src :imm))
     ((temp :s32)))
  ((:pred = count 0)
   (mov dest src))
  ((:not (:pred = count 0))
   (mov temp (:lsr src (:$ count)))
   (bic dest temp (:$ arm::fixnummask))))

(define-arm-vinsn (natural-shift-left :predicatable)
    (((dest :u32))
     ((src :u32)
      (count :u8const)))
  ((:pred = count 0)
   (mov dest src))
  ((:not (:pred = count 0))
   (mov dest (:lsl src (:$ count)))))

(define-arm-vinsn (natural-shift-right :predicatable)
    (((dest :u32))
     ((src :u32)
      (count :u8const)))
  ((:pred = count 0)
   (mov dest src))
  ((:not (:pred = count 0))
   (mov dest (:lsr src (:$ count)))))

(define-arm-vinsn fixnum-ash-left (((dest :lisp))
                                   ((num :lisp)
                                    (amt :lisp))
                                   ((temp :s32)))
  (mov temp (:asr amt (:$ arm::fixnumshift)))
  (mov dest (:lsl num temp)))

(define-arm-vinsn fixnum-ash (((dest :lisp))
                              ((num :lisp)
                               (amt :lisp))
                              ((temp :s32)))
  (movs temp (:asr amt (:$ arm::fixnumshift)))
  (movge dest (:lsl num temp))
  (bge :done)
  (rsb temp temp (:$ 0))
  (mov temp (:asr num temp))
  (bic dest temp (:$ arm::fixnummask))
  :done)


(define-arm-vinsn trap-unless-simple-array-2 (()
                                              ((object :lisp)
                                               (rexpected-flags :imm))
                                              ((tag :u8)
                                               (flags :u32)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-arrayH))
  (bne :bad-if-ne)
  (mov flags (:$ (ash 2 arm::fixnumshift)))
  (ldr tag (:@ object (:$ arm::arrayH.rank)))
  (cmp tag flags)
  (beq :ok)
  (uuo-error-array-rank flags object)
  :ok
  (ldr flags (:@ object (:$ arm::arrayH.flags)))
  (cmp flags rexpected-flags)
  :bad-if-ne
  (beq :done)
  (uuo-error-array-flags rexpected-flags object )
  :done)

(define-arm-vinsn trap-unless-typed-array-2 (()
                                              ((object :lisp)
                                               (subtag :u8const))
                                              ((flags :u32)
                                               (tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-arrayH))
  (bne :bad)
  (mov flags (:$ (ash 2 arm::fixnumshift)))
  (ldr tag (:@ object (:$ arm::arrayH.rank)))
  (cmp tag flags)
  (beq :ok)
  (uuo-error-array-rank flags object)
  :ok
  (ldr flags (:@ object (:$ arm::arrayH.flags)))
  (and flags flags (:$ (ash #xff (+ 8 arm::fixnumshift))))
  (cmp flags (:$ (:apply ash subtag (+ 8 arm::fixnumshift))))
  (beq :done)
  :bad
  (mov flags (:$ (:apply ash subtag (+ 8 arm::fixnumshift))))  
  (uuo-error-array-flags flags object )
  :done)

(define-arm-vinsn trap-unless-simple-array-3 (()
                                              ((object :lisp)
                                               (rexpected-flags :imm))
                                              ((tag :u8)
                                               (flags :u32)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-arrayH))
  (bne :bad-if-ne)
  (ldr tag (:@ object (:$ arm::arrayH.rank)))
  (mov flags (:$ (ash 3 arm::fixnumshift)))
  (cmp tag flags)
  (beq :rank-ok)
  (uuo-error-array-rank flags object)
  :rank-ok
  (ldr flags (:@ object (:$ arm::arrayH.flags)))
  (cmp rexpected-flags flags)
  :bad-if-ne
  (beq :done)
  (uuo-error-array-flags rexpected-flags object)
  :done)

(define-arm-vinsn trap-unless-typed-array-3 (()
                                              ((object :lisp)
                                               (subtag :u8const))
                                              ((flags :u32)
                                               (tag :u8)))
  (and tag object (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ object (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-arrayH))
  (bne :bad)
  (mov flags (:$ (ash 3 arm::fixnumshift)))
  (ldr tag (:@ object (:$ arm::arrayH.rank)))
  (cmp tag flags)
  (beq :ok)
  (uuo-error-array-rank flags object)
  :ok
  (ldr flags (:@ object (:$ arm::arrayH.flags)))
  (and flags flags (:$ (ash #xff (+ 8 arm::fixnumshift))))
  (cmp flags (:$ (:apply ash subtag (+ 8 arm::fixnumshift))))
  (beq :done)
  :bad
  (mov flags (:$ (:apply ash subtag (+ 8 arm::fixnumshift))))  
  (uuo-error-array-flags flags object )
  :done)
  
  
  
  
(define-arm-vinsn (sign-extend-halfword :predicatable)
    (((dest :imm))
     ((src :imm)))
  (mov dest (:lsl src (:$ (- 16 arm::fixnumshift))))
  (mov dest (:asr dest (:$ (- 16 arm::fixnumshift)))))


                            

(define-arm-vinsn (fixnum-add :predicatable)
    (((dest t))
     ((x t)
      (y t)))
  (add dest x y))

(define-arm-vinsn fixnum-add-set-flags (((dest t)
                                         (flags :crf))
                                        ((x t)
                                         (y t)))
  (adds dest x y))

(define-arm-vinsn fixnum-sub-set-flags (((dest t)
                                         (flags :crf))
                                        ((x t)
                                         (y t)))
  (subs dest x y))




(define-arm-vinsn handle-fixnum-overflow-inline (((dest :lisp))
                                                 ((src :imm))
                                                 ((unboxed :s32)
                                                  (header :u32)))
  (mov unboxed (:asr dest (:$ arm::fixnumshift)))
  (mov header (:$ (logand #xff00 arm::one-digit-bignum-header)))
  (orr header header (:$ (logand #xff arm::one-digit-bignum-header)))
  (eor unboxed unboxed (:$ #xc0000000))
  (sub allocptr allocptr (:$ (- arm::dnode-size arm::fulltag-misc)))
  (ldr dest (:@ rcontext (:$ arm::tcr.save-allocbase)))
  (cmp allocptr dest)
  (bhi :no-trap)
  (uuo-alloc-trap)
  :no-trap
  (str header (:@ allocptr (:$ arm::misc-header-offset)))
  (mov dest allocptr)
  (bic allocptr allocptr (:$ arm::fulltagmask))
  (str unboxed (:@ dest (:$ arm::misc-data-offset))))

  

  

;;;  (setq dest (- x y))
(define-arm-vinsn (fixnum-sub :predicatable)
    (((dest t))
     ((x t)
      (y t)))
  (sub dest x y))


(define-arm-vinsn fixnum-sub-set-flags
    (((dest t)
      (flags :crf))
     ((x t)
      (y t)))
  (subs dest x y))

(define-arm-vinsn (fixnum-sub-constant :predicatable) (((dest t))
                                                       ((x t)
                                                        (y :s32const)))
  (sub dest x (:$ y)))

(define-arm-vinsn fixnum-sub-constant-set-flags (((dest t)
                                                  (flags :crf))
                                                 ((x t)
                                                  (y :s32const)))
  (subs dest x (:$ y)))
                                                       

(define-arm-vinsn (fixnum-sub-from-constant :predicatable)
    (((dest :imm))
     ((x :s32const)
      (y :imm)))
  (rsb dest y (:$ x)))


(define-arm-vinsn fixnum-sub-from-constant-set-flags
    (((dest :imm)
      (flags :crf))
     ((x :s32const)
      (y :imm)))
  (rsbs dest y (:$ x)))



;;; This is, of course, also "subtract-immediate."
(define-arm-vinsn (add-immediate :predicatable)
    (((dest t))
     ((src t)
      (imm :s32const)))
  (add dest src (:$ imm)))

(define-arm-vinsn add-immediate-set-flags
    (((dest t)
      (crf :crf))
     ((src t)
      (imm :s32const)))
  (adds dest src (:$ imm)))

(define-arm-vinsn (multiply-fixnums :predicatable)
    (((dest :imm))
     ((a :imm)
      (b :imm))
     ((unboxed :s32)))
  (mov unboxed (:asr b (:$ arm::fixnumshift)))
  (mul dest a unboxed))



;;; Mask out the code field of a base character; the result
;;; should be EXACTLY = to subtag-base-char
(define-arm-vinsn (mask-base-char :predicatable)
    (((dest :u32))
     ((src :imm)))
  (and dest src (:$ arm::subtag-mask)))


(define-arm-vinsn istruct-type (((dest :lisp))
                                ((val :lisp))
                                ((temp :u8)))
  (and temp val (:$ arm::tagmask))
  (cmp temp (:$ arm::tag-misc))
  (ldrbeq temp (:@ val (:$ arm::misc-subtag-offset)))
  (cmp temp (:$ arm::subtag-istruct))
  (movne dest (:$ arm::nil-value))
  (ldreq dest (:@ val (:$ arm::misc-data-offset))))
  
  
;; Boundp, fboundp stuff.
(define-arm-vinsn (ref-symbol-value :call :subprim)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (sploadlr .SPspecrefcheck)
  (blx lr))

(define-arm-vinsn ref-symbol-value-inline (((dest :lisp))
                                           ((src (:lisp (:ne dest))))
                                           ((table :imm)
                                            (idx :imm)))
  (ldr idx (:@ src (:$ arm::symbol.binding-index)))
  (ldr table (:@ rcontext (:$ arm::tcr.tlb-limit)))
  (cmp idx table)
  (ldr table (:@ rcontext (:$ arm::tcr.tlb-pointer)))
  (movhs idx (:$ 0))
  (ldr dest (:@ table idx))
  (cmp dest (:$ arm::subtag-no-thread-local-binding))
  (ldreq dest (:@ src (:$ arm::symbol.vcell)))
  (cmp dest (:$ arm::unbound-marker))
  (bne :bound)
  (uuo-error-unbound src)
  :bound)

(define-arm-vinsn (%ref-symbol-value :call :subprim)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (sploadlr .SPspecref)
  (blx lr))

(define-arm-vinsn %ref-symbol-value-inline (((dest :lisp))
                                            ((src (:lisp (:ne dest))))
                                            ((table :imm)
                                             (idx :imm)))
  (ldr idx (:@ src (:$ arm::symbol.binding-index)))
  (ldr table (:@ rcontext (:$ arm::tcr.tlb-limit)))
  (cmp idx table)
  (ldr table (:@ rcontext (:$ arm::tcr.tlb-pointer)))
  (movhs idx (:$ 0))
  (ldr dest (:@ table idx))
  (cmp dest (:$ arm::subtag-no-thread-local-binding))
  (ldreq dest (:@ src (:$ arm::symbol.vcell))))

(define-arm-vinsn (setq-special :call :subprim)
    (()
     ((sym :lisp)
      (val :lisp)))
  (sploadlr .SPspecset)
  (blx lr))


(define-arm-vinsn symbol-function (((val :lisp))
                                   ((sym (:lisp (:ne val))))
                                   ((crf :crf)
                                    (tag :u32)))
  (ldr val (:@ sym (:$ arm::symbol.fcell)))
  (and tag val (:$ arm::tagmask))
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ val (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-function))
  (beq :defined)
  (uuo-error-udf sym)
  :defined)


(define-arm-vinsn save-nfp (()
                            ()
                            ((temp :imm)))
  ((:pred  > (:apply arm2-max-nfp-depth) 0)
   ;; screw: handle > 4K case
   (str sp (:@! sp (:$ (:apply - (:apply + (:apply arm2-max-nfp-depth) 8)))))
   (ldr temp (:@ rcontext (:$ arm::tcr.nfp)))
   (str temp (:@ sp (:$ 4)))
   (str sp (:@ rcontext (:$ arm::tcr.nfp)))))

(define-arm-vinsn restore-nfp (()
                               ()
                               ((temp :imm)))
  ((:pred > (:apply  arm2-max-nfp-depth) 0)
   (ldr temp (:@ sp (:$ 4)))
   (str temp (:@ rcontext (:$ arm::tcr.nfp)))
   (ldr sp (:@ sp (:$ 0)))))


(define-arm-vinsn (nfp-store-double-float :nfp :set) (()
                                                      ((val :double-float)
                                                       (offset :u16const)))
  (fstd val (:@ sp (:$ (:apply + 8 offset)))))

(define-arm-vinsn (nfp-store-double-float-nested :nfp :set) (()
                                                             ((val :double-float)
                                                              (offset :u16const)))
  (ldr lr (:@ rcontext (:$ arm::tcr.nfp)))
  (fstd val (:@ lr (:$ (:apply + 8 offset)))))


(define-arm-vinsn (nfp-load-double-float :nfp :ref)  (((val :double-float))
                                                      ((offset :u16const)))
  (fldd val (:@ sp (:$ (:apply + 8 offset)))))

(define-arm-vinsn (nfp-load-double-float-nested :nfp :ref) (((val :double-float))
                                                            ((offset :u16const)))
  (ldr lr (:@ rcontext (:$ arm::tcr.nfp)))
  (fldd val (:@ lr (:$ (:apply + 8 offset)))))

(define-arm-vinsn (nfp-store-complex-double-float :nfp :set :doubleword)
    (()
     ((val :complex-double-float)
      (offset :u16const)))
  (fstd val (:@ sp (:$ (:apply + 8 offset))))
  (fstd (:apply 1+ (:apply %hard-regspec-value val))
        (:@ sp (:$ (:apply + 8 8 offset)))))

(define-arm-vinsn (nfp-store-complex-double-float-nested :nfp :set :doubleword)
    (()
     ((val :complex-double-float)
      (offset :u16const)))
  (ldr lr (:@ rcontext (:$ arm::tcr.nfp)))
  (fstd val (:@ lr (:$ (:apply + 8 offset))))
  (fstd (:apply 1+ (:apply %hard-regspec-value val))
        (:@ lr (:$ (:apply + 8 8 offset)))))
  

(define-arm-vinsn (nfp-load-complex-double-float :nfp :ref :doubleword)
    (((val :complex-double-float))
     ((offset :u16const)))
  (fldd val (:@ sp (:$ (:apply + 8 offset))))
  (fldd (:apply 1+ (:apply %hard-regspec-value val))
        (:@ sp (:$ (:apply + 8 8 offset)))))

(define-arm-vinsn (nfp-load-complex-double-float-nested :nfp :ref :doubleword)
    (((val :complex-double-float))
     ((offset :u16const)))
  (ldr lr (:@ rcontext (:$ arm::tcr.nfp)))
  (fldd val (:@ lr (:$ (:apply + 8 offset))))
  (fldd (:apply 1+ (:apply %hard-regspec-value val))
        (:@ lr (:$ (:apply + 8 8 offset)))))

(define-arm-vinsn (nfp-store-single-float :nfp :set) (()
                                                      ((val :single-float)
                                                       (offset :u16const)))
  (fsts val (:@ sp (:$ (:apply + 8 offset)))))




(define-arm-vinsn (nfp-store-single-float-nested :nfp :set) (()
                                                             ((val :single-float)
                                                              (offset :u16const)))
  (ldr lr (:@ rcontext (:$ arm::tcr.nfp)))
  (fsts val (:@ lr (:$ (:apply + 8 offset)))))



(define-arm-vinsn (nfp-load-single-float :nfp :ref) (((val :single-float))
                                                     ((offset :u16const)))
  (flds val (:@ sp (:$ (:apply + 8 offset)))))

(define-arm-vinsn (nfp-load-single-float-nested :nfp :ref) (((val :single-float))
                                                            ((offset :u16const)))
  (ldr lr (:@ rcontext (:$ arm::tcr.nfp)))
  (flds val (:@ lr (:$ (:apply + 8 offset)))))

(define-arm-vinsn (nfp-store-unboxed-word :nfp :set) (()
                                                      ((val :u32)
                                                       (offset :u16const)))
  (str val (:@ sp (:$ (:apply + 8 offset)))))

(define-arm-vinsn (nfp-store-unboxed-word-nested :nfp :set) (()
                                                             ((val :u32)
                                                              (offset :u16const)))
  (ldr lr (:@ rcontext (:$ arm::tcr.nfp)))
  (str val (:@ lr (:$ (:apply + 8 offset)))))

(define-arm-vinsn (nfp-load-unboxed-word :nfp :ref) (((val :u32))
                                                     ((offset :u16const)))
  (ldr val (:@ sp (:$ (:apply + 8 offset)))))

(define-arm-vinsn (nfp-load-unboxed-word-nested :nfp :ref) (((val :u32))
                                                            ((offset :u16const)))
  (ldr lr (:@ rcontext (:$ arm::tcr.nfp)))
  (ldr val (:@ lr (:$ (:apply + 8 offset)))))
  
(define-arm-vinsn (%current-frame-ptr :predicatable)
    (((dest :imm))
     ())
  (mov dest arm::sp))

(define-arm-vinsn (%current-tcr :predicatable)
    (((dest :imm))
     ())
  (mov dest rcontext))

(define-arm-vinsn (dpayback :call :subprim) (()
                                                  ((n :s16const))
                                                  ((temp (:u32 #.arm::imm0))))
  ((:pred > n 1)
   (mov temp (:$ n))
   (sploadlr .SPunbind-n))
  ((:pred = n 1)
   (sploadlr .SPunbind))
  (blx lr))

(define-arm-vinsn (zero-double-float-register :predicatable)
    (((dest :double-float))
     ())
  (fcpyd dest arm::double-float-zero))

(define-arm-vinsn (zero-single-float-register :predicatable)
    (((dest :single-float))
     ())
  (fcpys dest arm::single-float-zero))

(define-arm-vinsn (load-single-float-constant-from-data :predicatable)
    (((dest :double-float))
     ((val :u32const)))
  (flds dest (:= :x))
  (:data)
  :x
  (:word val)
  :code)

(define-arm-vinsn (load-double-float-constant-from-data :predicatable)
    (((dest :double-float))
     ((high :u32const)
      (low :u32const)))
  (fldd dest (:= :x))
  (:data)
  :x
  (:word low)
  (:word high)
  :code)

(define-arm-vinsn (load-single-float-constant :predicatable)
    (((dest :single-float))
     ((src t)))
  (fmsr dest src))

(define-arm-vinsn (load-indexed-node :predicatable)
    (((node :lisp))
     ((base :lisp)
      (offset :s16const)))
  (ldr node (:@ base (:$ offset))))

(define-arm-vinsn check-exact-nargs (()
                                     ((n :u16const)))
  (cmp nargs (:$ (:apply ash n 2)))
  (beq :ok)
  (uuo-error-wrong-nargs (:? ne))
  :ok)

(define-arm-vinsn check-exact-nargs-large (()
                                           ((n :u16const))
                                           ((preserve (:u32 #.arm::nargs))
                                            (temp :u32)))
  (mov temp (:$ (:apply logand #xff00 (:apply ash n 2))))
  (orr temp temp (:$ (:apply logand #xff (:apply ash n 2))))
  (cmp nargs temp)
  (beq :ok)
  (uuo-error-wrong-nargs (:? ne))
  :ok)

(define-arm-vinsn check-min-nargs (()
                                   ((min :u16const)))
  (cmp nargs (:$ (:apply ash min 2)))
  (bhs :ok)
  (uuo-error-wrong-nargs (:? lo))
  :ok)

(define-arm-vinsn check-min-nargs-large (()
                                         ((min :u16const))
                                         ((preserve (:u32 #.arm::nargs))
                                          (temp :u32)))
  (mov temp (:$ (:apply logand #xff00 (:apply ash min 2))))
  (orr temp temp (:$ (:apply logand #xff (:apply ash min 2))))
  (cmp nargs temp)
  (bhs :ok)
  (uuo-error-wrong-nargs (:? lo))
  :ok)


(define-arm-vinsn check-max-nargs (()
                                   ((max :u16const)))
  (cmp nargs (:$ (:apply ash max 2)))
  (bls :ok)
  (uuo-error-wrong-nargs (:? hi))
  :ok)

(define-arm-vinsn check-max-nargs-large (()
                                         ((max :u16const))
                                         ((preserve (:u32 #.arm::nargs))
                                          (temp :u32)))
  (mov temp (:$ (:apply logand #xff00 (:apply ash max 2))))
  (orr temp temp (:$ (:apply logand #xff (:apply ash max 2))))
  (cmp nargs temp)
  (bls :ok)
  (uuo-error-wrong-nargs (:? hi))
  :ok)

;;; Save context and establish FN.  The current VSP is the the
;;; same as the caller's, e.g., no arguments were vpushed.
(define-arm-vinsn save-lisp-context-vsp (()
                                         ()
                                         ((imm :u32)))
  (mov imm (:$ arm::lisp-frame-marker))
  (stmdb (:! sp) (imm vsp fn lr))
  (mov fn nfn))



(define-arm-vinsn save-lisp-context-offset (()
                                            ((nbytes-vpushed :u16const))
                                            ((imm (:u32 #.arm::imm1))
                                             (clobbered (:u32 #.arm::imm0))))
  ((:pred arm::encode-arm-immediate nbytes-vpushed)
   (add imm vsp (:$ nbytes-vpushed)))
  ((:not (:pred arm::encode-arm-immediate nbytes-vpushed))
   (mov imm (:$ (:apply logand #xff00 nbytes-vpushed)))
   (orr imm imm (:$ (:apply logand #xff nbytes-vpushed)))
   (add imm imm vsp))
  (mov imm0 (:$ arm::lisp-frame-marker))
  (stmdb (:! sp) (imm0 imm fn lr))
  (mov fn nfn))

(define-arm-vinsn save-lisp-context-variable (()
                                              ()
                                              ((imm (:u32 #.arm::imm1))))
  (subs imm nargs (:$ (ash $numarmargregs arm::word-shift)))
  (movmi imm (:$ 0))
  (add imm imm vsp)
  (mov imm0 (:$ arm::lisp-frame-marker))
  (stmdb (:! sp) (imm0 imm fn lr))
  (mov fn nfn))  



  
(define-arm-vinsn save-cleanup-context (()
                                        ())
  (mov temp2 (:$ 0))
  (mov imm0 (:$ arm::lisp-frame-marker))  
  (stmdb (:! sp) (imm0 vsp fn lr))
  (str temp2 (:@ sp (:$ arm::lisp-frame.savefn))))



;; Vpush the argument registers.  We got at least "min-fixed" args;
;; that knowledge may help us generate better code.
(define-arm-vinsn save-lexpr-argregs
    (()
     ((min-fixed :u16const))
     ((entry-vsp (:u32 #.arm::imm1))
      (arg-temp (:u32 #.arm::imm0))
      (preserve (:u32 #.arm::nargs))
      (other-temp :imm)))
  ((:pred >= min-fixed $numarmargregs)
   (stmdb (:! vsp) (arg_z arg_y arg_x)))
  ((:pred = min-fixed 2)                ; at least 2 args
   (cmp nargs (:$ (ash 2 arm::word-shift)))
   (strne arg_x (:@! vsp (:$ -4)))
   (stmdb (:! vsp) (arg_z arg_y)))
  ((:pred = min-fixed 1)                ; at least one arg
   (cmp nargs (:$ (ash 2 arm::word-shift)))
   (strlo arg_z (:@! vsp (:$ (- arm::node-size))))
   (stmdbeq (:! vsp) (arg_z arg_y))
   (stmdbhi (:! vsp) (arg_z arg_y arg_x)))
  ((:pred = min-fixed 0)
   (cmp nargs (:$ 0))
   (beq :done)
   (cmp nargs (:$ (ash 2 arm::word-shift)))
   (strlo arg_z (:@! vsp (:$ (- arm::node-size))))
   (stmdbeq (:! vsp) (arg_z arg_y))
   (stmdbhi (:! vsp) (arg_z arg_y arg_x))
   :done
   )
  ((:pred = min-fixed 0)
   (str nargs (:@! vsp (:$ -4))))
  ((:not (:pred = min-fixed 0))
   (sub arg-temp nargs (:$ (:apply ash min-fixed arm::word-shift)))
   (str arg-temp (:@! vsp (:$ -4))))
  (add entry-vsp vsp nargs)
  (mov other-temp (:$ (- arm::nil-value arm::fulltag-nil)))
  (ldr other-temp (:@ other-temp (:$ (arm::%kernel-global 'arm::ret1valaddr))))
  (add entry-vsp entry-vsp (:$ 4))
  (cmp other-temp lr)
  (mov arg-temp (:$ arm::lisp-frame-marker))
  (stmdb (:! sp) (arg-temp entry-vsp fn lr))
  (mov fn (:$ 0))
  (moveq lr (:$ (- arm::nil-value arm::fulltag-nil)))
  (ldreq lr (:@ lr (:$ (arm::%kernel-global 'arm::lexpr-return))))
  (stmdbeq (:! sp) (arg-temp entry-vsp fn lr))
  (moveq lr other-temp)
  (movne lr (:$ (- arm::nil-value arm::fulltag-nil)))
  (ldrne lr (:@ lr (:$ (arm::%kernel-global 'arm::lexpr-return1v))))
  )



(define-arm-vinsn (jump-return-pc :jumpLR :predicatable)
    (()
     ())
  (bx lr))

(define-arm-vinsn (restore-full-lisp-context :lispcontext :pop :csp :lrRestore :predicatable)
    (()
     ())
  (ldmia (:! sp) (imm0 vsp fn lr)))





(define-arm-vinsn (popj :lispcontext :pop :csp :lrRestore :jumpLR :predicatable)
    (() 
     ())
  (ldmia (:! sp) (imm0 vsp fn pc)))

;;; Exiting from an UNWIND-PROTECT cleanup is similar to
;;; (and a little simpler than) returning from a function.
(define-arm-vinsn restore-cleanup-context (()
                                           ())
  (ldr lr (:@ sp (:$ arm::lisp-frame.savelr)))
  (add sp sp (:$ arm::lisp-frame.size)))



(define-arm-vinsn default-1-arg (()
                                 ((min :u16const)))
  (cmp nargs (:$ (:apply ash min 2)))
  (bne :done)
  ((:pred >= min 3)
   (str arg_x (:@! vsp (:$ (- arm::node-size)))))
  ((:pred >= min 2)
   (mov arg_x arg_y))
  ((:pred >= min 1)
   (mov arg_y arg_z))
  (mov arm::arg_z (:$ arm::nil-value))
  :done)

(define-arm-vinsn default-2-args (()
                                  ((min :u16const)))
  (cmp nargs (:$ (:apply ash (:apply 1+ min) 2)))
  (bgt :done)
  (beq :one)
  ;; We got "min" args; arg_y & arg_z default to nil
  ((:pred >= min 3)
   (str arg_x (:@! vsp (:$ (- arm::node-size)))))   
  ((:pred >= min 2)
   (str arg_y (:@! vsp (:$ (- arm::node-size)))))
  ((:pred >= min 1)
   (mov arg_x arg_z))
  (mov arg_y (:$ arm::nil-value))
  (b :last)
  :one
  ;; We got min+1 args: arg_y was supplied, arg_z defaults to nil.
  ((:pred >= min 2)
   (str arg_x (:@! vsp (:$ (- arm::node-size)))))
  ((:pred >= min 1)
   (mov arg_x arg_y))
  (mov arm::arg_y arm::arg_z)
  :last
  (mov arg_z (:$ arm::nil-value))
  :done)

(define-arm-vinsn default-3-args (()
                                  ((min :u16const)))
  (cmp nargs (:$ (:apply ash min 2)))
  (beq :none)
  (cmp nargs (:$ (:apply ash (:apply + 2 min) 2)))

  (bgt :done)
  (beq :two)
  ;; The first (of three) &optional args was supplied.
  ((:pred >= min 2)
   (str arg_x (:@! vsp (:$ (- arm::node-size)))))
  ((:pred >= min 1)
   (str arg_y (:@! vsp (:$ (- arm::node-size)))))
  (mov arg_x arg_z)
  (b :last-2)
  :two
  ;; The first two (of three) &optional args were supplied.
  ((:pred >= min 1)
   (str arg_x (:@! vsp (:$ (- arm::node-size)))))
  (mov arg_x arg_y)
  (mov arg_y arg_z)
  (b :last-1)
  ;; None of the three &optional args was provided.
  :none
  ((:pred >= min 3)
   (str arg_x (:@! vsp (:$ (- arm::node-size)))))
  ((:pred >= min 2)
   (str arg_y (:@! vsp (:$ (- arm::node-size)))))
  ((:pred >= min 1)
   (str arg_z (:@! vsp (:$ (- arm::node-size)))))
  (mov arg_x (:$ arm::nil-value))
  :last-2
  (mov arg_y (:$ arm::nil-value))
  :last-1
  (mov arg_z (:$ arm::nil-value))
  :done)



;;; "n" is the sum of the number of required args + 
;;; the number of &optionals.  
(define-arm-vinsn (default-optionals :call :subprim) (()
                                                           ((n :u16const)))
  (mov imm0 (:$ (:apply ash n 2)))
  (sploadlr .SPdefault-optional-args)
  (blx lr))

;;; fname contains a known symbol
(define-arm-vinsn (call-known-symbol :call) (((result (:lisp arm::arg_z)))
                                             ())
  (ldr nfn (:@ fname (:$ arm::symbol.fcell)))
  (ldr lr (:@ nfn (:$ arm::function.entrypoint)))
  (blx lr))

(define-arm-vinsn (jump-known-symbol :jumplr) (()
                                               ())
  (ldr nfn (:@ fname (:$ arm::symbol.fcell)))
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))

(define-arm-vinsn (call-known-function :call) (()
                                               ())
  (ldr lr (:@ nfn (:$ arm::function.entrypoint)))
  (blx lr))

(define-arm-vinsn (jump-known-function :jumplr) (()
                                                 ())
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))

(define-arm-vinsn (%schar8 :predicatable)
    (((char :imm))
     ((str :lisp)
      (idx :imm))
     ((imm :u32)))
  (mov imm (:lsr idx (:$ arm::fixnumshift)))
  (add imm imm (:$ arm::misc-data-offset))
  (ldrb imm (:@ str imm))
  (mov imm (:lsl imm (:$ arm::charcode-shift)))
  (orr char imm (:$ arm::subtag-character)))

(define-arm-vinsn (%schar32 :predicatable)
    (((char :imm))
     ((str :lisp)
      (idx :imm))
     ((imm :u32)))
  (add imm idx (:$ arm::misc-data-offset))
  (ldr imm (:@ str imm))
  (mov imm (:lsl imm (:$ arm::charcode-shift)))
  (orr char imm (:$ arm::subtag-character)))


(define-arm-vinsn (%set-schar8 :predicatable)
    (()
     ((str :lisp)
      (idx :imm)
      (char :imm))
     ((imm :u32)
      (imm1 :u32)))
  (mov imm (:lsr idx (:$ arm::fixnumshift)))
  (add imm imm (:$ arm::misc-data-offset))
  (mov imm1 (:lsr char (:$ arm::charcode-shift)))
  (strb imm1 (:@ str imm)))

(define-arm-vinsn (%set-schar32 :predicatable)
    (()
     ((str :lisp)
      (idx :imm)
      (char :imm))
     ((imm :u32)
      (imm1 :u32)))
  (add imm idx (:$ arm::misc-data-offset))
  (mov imm1 (:lsr char (:$ arm::charcode-shift)))
  (str imm1 (:@ str imm)))

(define-arm-vinsn (%set-scharcode8 :predicatable)
    (()
     ((str :lisp)
      (idx :imm)
      (code :imm))
     ((imm :u32)
      (imm1 :u32)))
  (mov imm (:lsr idx (:$ arm::fixnumshift)))
  (add imm imm (:$ arm::misc-data-offset))
  (mov imm1 (:lsr code (:$ arm::fixnumshift)))
  (strb imm1 (:@ str imm)))


(define-arm-vinsn (%set-scharcode32 :predicatable)
    (()
     ((str :lisp)
      (idx :imm)
      (code :imm))
     ((imm :u32)
      (imm1 :u32)))
  (add imm idx (:$ arm::misc-data-offset))
  (mov imm1 (:lsr code (:$ arm::fixnumshift)))
  (str imm1 (:@ str imm)))

(define-arm-vinsn (%scharcode8 :predicatable)
    (((code :imm))
     ((str :lisp)
      (idx :imm))
     ((imm :u32)))
  (mov imm (:lsr idx (:$ arm::fixnumshift)))
  (add imm imm (:$ arm::misc-data-offset))
  (ldrb imm (:@ str imm))
  (mov code (:lsl imm (:$ arm::fixnumshift))))

(define-arm-vinsn (%scharcode32 :predicatable)
    (((code :imm))
     ((str :lisp)
      (idx :imm))
     ((imm :u32)))
  (add imm idx (:$ arm::misc-data-offset))
  (ldr imm (:@ str imm))
  (mov code (:lsl imm (:$ arm::fixnumshift))))

;;; Clobbers LR
(define-arm-vinsn %debug-trap (()
                               ())
  (uuo-debug-trap))


(define-arm-vinsn eep.address (((dest t))
                               ((src (:lisp (:ne dest )))))
  (ldr dest (:@ src (:$ (+ (ash 1 2) arm::misc-data-offset))))
  (cmp dest (:$ arm::nil-value))
  (bne :ok)
  (uuo-eep-unresolved  dest src)
  :ok)
                 
(define-arm-vinsn (%natural+ :predicatable)
    (((dest :u32))
     ((x :u32) (y :u32)))
  (add dest x y))

(define-arm-vinsn (%natural+-c :predicatable)
    (((dest :u32))
     ((x :u32) (y :u16const)))
  (add dest x (:$ y)))

(define-arm-vinsn (%natural- :predicatable)
    (((dest :u32))
     ((x :u32) (y :u32)))
  (sub dest x y))

(define-arm-vinsn (%natural--c :predicatable)
    (((dest :u32))
     ((x :u32) (y :u16const)))
  (sub dest x (:$ y)))

(define-arm-vinsn (%natural-logior :predicatable)
    (((dest :u32))
     ((x :u32) (y :u32)))
  (orr dest x y))

(define-arm-vinsn (%natural-logior-c :predicatable)
    (((dest :u32))
     ((x :u32) (c :u32const)))
  (orr dest x (:$ c)))

(define-arm-vinsn (%natural-logxor :predicatable)
    (((dest :u32))
     ((x :u32) (y :u32)))
  (eor dest x y))

(define-arm-vinsn (%natural-logxor-c :predicatable)
    (((dest :u32))
     ((x :u32) (c :u32const)))
  (eor dest x (:$ c)))

(define-arm-vinsn (%natural-logand :predicatable)
    (((dest :u32))
     ((x :u32) (y :u32)))
  (and dest x y))

(define-arm-vinsn %natural-logand-c (((dest :u32))
                                          ((x :u32) (c :u16const))
                                     )
  (and dest x (:$ c)))

(define-arm-vinsn %ilogbitp-constant-bit (((dest :crf))
                                          ((fixnum :imm)
                                           (bitnum :u8const)))
  (tst fixnum (:$ (:apply ash 1 (:apply + bitnum arm::fixnumshift)))))

(define-arm-vinsn %ilogbitp-variable-bit (((dest :crf))
                                          ((fixnum :imm)
                                           (bitnum :u8))
                                          ((mask :imm)
                                           (unboxed :u8)))
  (mov unboxed (:asr bitnum (:$ arm::fixnumshift)))
  (mov mask (:$ arm::fixnumone))
  (tst fixnum (:lsl mask unboxed)))





(define-arm-vinsn (disable-interrupts :predicatable)
    (((dest :lisp))
     ()
     ((temp :imm)
      (temp2 :imm)))
  (ldr temp2 (:@ rcontext (:$ arm::tcr.tlb-pointer)))
  (mov temp (:$ -4))
  (ldr dest (:@ temp2 (:$ arm::interrupt-level-binding-index)))
  (str temp (:@ temp2 (:$ arm::interrupt-level-binding-index))))

(define-arm-vinsn (load-character-constant :predicatable)
    (((dest :lisp))
     ((code :u32const)))
  (mov dest (:$ (:apply ash (:apply logand code #xff) arm::charcode-shift)))
  (orr dest dest (:$ arm::subtag-character))
  ((:not (:pred = 0 (:apply logand #xff000000 (:apply ash code arm::charcode-shift))))
   (orr dest dest (:$ (:apply logand #xff000000 (:apply ash code arm::charcode-shift)))))
  ((:not (:pred = 0 (:apply logand #x00ff0000 (:apply ash code arm::charcode-shift))))
   (orr dest dest (:$ (:apply logand #x00ff0000 (:apply ash code arm::charcode-shift))))))


(define-arm-vinsn %symbol->symptr (((dest :lisp))
                                   ((src :lisp))
                                   ((tag :u8)))
  (cmp src (:$ arm::nil-value))
  (and tag src (:$ arm::tagmask))
  (beq :nilsym)
  (cmp tag (:$ arm::tag-misc))
  (ldrbeq tag (:@ src (:$ arm::misc-subtag-offset)))
  (cmp tag (:$ arm::subtag-symbol))
  (beq :symbol)
  (uuo-error-reg-not-xtype src (:$ arm::subtag-symbol))
  :symbol
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mov dest src))
  (b :done)
  :nilsym
  (add dest src (:$ arm::nilsym-offset))
  :done)

;;; Subprim calls.  Done this way for the benefit of VINSN-OPTIMIZE.
(defmacro define-arm-subprim-call-vinsn ((name &rest other-attrs) spno)
  `(define-arm-vinsn (,name :call :subprim ,@other-attrs) (() ())
    (sploadlr ,spno)
    (blx lr)))

(defmacro define-arm-subprim-jump-vinsn ((name &rest other-attrs) spno &optional)
  `(define-arm-vinsn (,name  :jumpLR ,@other-attrs) (() ())
    (spjump ,spno)))


(define-arm-subprim-call-vinsn (save-values) .SPsave-values)

(define-arm-subprim-call-vinsn (recover-values)  .SPrecover-values)

(define-arm-subprim-call-vinsn (add-values) .SPadd-values)


(define-arm-subprim-call-vinsn (pass-multiple-values)  .SPmvpass)

(define-arm-subprim-call-vinsn (pass-multiple-values-symbol) .SPmvpasssym)

(define-arm-subprim-jump-vinsn (jump-known-symbol-ool) .SPjmpsym)

(define-arm-subprim-call-vinsn (call-known-symbol-ool)  .SPjmpsym)


(define-arm-subprim-jump-vinsn (tail-call-sym-gen) .SPtcallsymgen)

(define-arm-subprim-jump-vinsn (tail-call-fn-gen) .SPtcallnfngen)

(define-arm-subprim-jump-vinsn (tail-call-sym-slide) .SPtcallsymslide)

(define-arm-subprim-jump-vinsn (tail-call-fn-slide) .SPtcallnfnslide)


(define-arm-subprim-call-vinsn (funcall)  .SPfuncall)

(define-arm-subprim-jump-vinsn (tail-funcall-gen) .SPtfuncallgen)

(define-arm-subprim-jump-vinsn (tail-funcall-slide) .SPtfuncallslide)

(define-arm-vinsn (tail-funcall-vsp :jumpLR :predicatable) (() ())
  (ldmia (:! sp) (imm0 vsp fn lr))
  (spjump .SPfuncall))

(define-arm-subprim-call-vinsn (spread-lexpr)  .SPspread-lexprz)

(define-arm-subprim-call-vinsn (spread-list)  .SPspreadargz)


(define-arm-subprim-call-vinsn (getu32) .SPgetu32)

(define-arm-subprim-call-vinsn (gets32) .SPgets32)


(define-arm-subprim-call-vinsn (stack-cons-list)  .SPstkconslist)

(define-arm-subprim-call-vinsn (list) .SPconslist)

(define-arm-subprim-call-vinsn (stack-cons-list*)  .SPstkconslist-star)

(define-arm-subprim-call-vinsn (list*) .SPconslist-star)

(define-arm-subprim-call-vinsn (make-stack-block)  .SPmakestackblock)

(define-arm-subprim-call-vinsn (make-stack-block0)  .Spmakestackblock0)

(define-arm-subprim-call-vinsn (make-stack-list)  .Spmakestacklist)

(define-arm-subprim-call-vinsn (make-stack-vector)  .SPmkstackv)

(define-arm-subprim-call-vinsn (make-stack-gvector)  .SPstkgvector)
(define-arm-vinsn (make-stack-closure :call :subprim) (() ())
  (sploadlr .SPstkgvector)
  (blx lr)
  (ldr lr (:@ arg_z (:$ arm::function.codevector)))
  (add lr lr (:$ arm::misc-data-offset))
  (str lr (:@ arg_z (:$ arm::function.entrypoint))))
                  

(define-arm-subprim-call-vinsn (stack-misc-alloc)  .SPstack-misc-alloc)

(define-arm-subprim-call-vinsn (stack-misc-alloc-init)  .SPstack-misc-alloc-init)

(define-arm-subprim-call-vinsn (bind-nil)  .SPbind-nil)

(define-arm-subprim-call-vinsn (bind-self)  .SPbind-self)

(define-arm-subprim-call-vinsn (bind-self-boundp-check)  .SPbind-self-boundp-check)

(define-arm-subprim-call-vinsn (bind)  .SPbind)

(define-arm-subprim-jump-vinsn (nvalret :jumpLR) .SPnvalret)

(define-arm-subprim-call-vinsn (nthrowvalues) .SPnthrowvalues)

(define-arm-subprim-call-vinsn (nthrow1value) .SPnthrow1value)

(define-arm-subprim-call-vinsn (slide-values) .SPmvslide)


(define-arm-subprim-call-vinsn (debind) .SPdebind)


(define-arm-subprim-call-vinsn (keyword-bind) .SPkeyword-bind)

(define-arm-subprim-call-vinsn (stack-rest-arg) .SPstack-rest-arg)

(define-arm-subprim-call-vinsn (req-stack-rest-arg) .SPreq-stack-rest-arg)

(define-arm-subprim-call-vinsn (stack-cons-rest-arg) .SPstack-cons-rest-arg)

(define-arm-subprim-call-vinsn (heap-rest-arg) .SPheap-rest-arg)

(define-arm-subprim-call-vinsn (req-heap-rest-arg) .SPreq-heap-rest-arg)

(define-arm-subprim-call-vinsn (heap-cons-rest-arg) .SPheap-cons-rest-arg)

(define-arm-subprim-call-vinsn (opt-supplied-p) .SPopt-supplied-p)

(define-arm-subprim-call-vinsn (gvector) .SPgvector)

(define-arm-subprim-call-vinsn (discard-temp-frame) .SPdiscard_stack_object)

(define-arm-vinsn (nth-value :call :subprim) (((result :lisp))
                                                   ())
  (sploadlr .SPnthvalue)
  (blx lr))

(define-arm-subprim-call-vinsn (fitvals) .SPfitvals)

(define-arm-subprim-call-vinsn (misc-alloc) .SPmisc-alloc)

(define-arm-subprim-call-vinsn (misc-alloc-init) .SPmisc-alloc-init)

(define-arm-subprim-call-vinsn (integer-sign) .SPinteger-sign)

;;; Even though it's implemented by calling a subprim, THROW is really
;;; a JUMP (to a possibly unknown destination).  If the destination's
;;; really known, it should probably be inlined (stack-cleanup, value
;;; transfer & jump ...)
(define-arm-vinsn (throw :jump-unknown) (()
                                         ())
  (sploadlr .SPthrow)
  (blx lr))

(define-arm-subprim-call-vinsn (mkcatchmv) .SPmkcatchmv)

(define-arm-subprim-call-vinsn (mkcatch1v) .SPmkcatch1v)

(define-arm-subprim-call-vinsn (setqsym) .SPsetqsym)

(define-arm-subprim-call-vinsn (ksignalerr) .SPksignalerr)

(define-arm-subprim-call-vinsn (subtag-misc-ref) .SPsubtag-misc-ref)

(define-arm-subprim-call-vinsn (subtag-misc-set) .SPsubtag-misc-set)

(define-arm-subprim-call-vinsn (mkunwind) .SPmkunwind)
(define-arm-subprim-call-vinsn (nmkunwind) .SPmkunwind)


(define-arm-subprim-call-vinsn (progvsave) .SPprogvsave)

(define-arm-subprim-jump-vinsn (progvrestore) .SPprogvrestore)


(define-arm-subprim-call-vinsn (misc-ref) .SPmisc-ref)

(define-arm-subprim-call-vinsn (misc-set) .SPmisc-set)

(define-arm-subprim-call-vinsn (gets64) .SPgets64)

(define-arm-subprim-call-vinsn (getu64) .SPgetu64)

(define-arm-subprim-call-vinsn (makeu64) .SPmakeu64)

(define-arm-subprim-call-vinsn (makes64) .SPmakes64)







(define-arm-subprim-call-vinsn (bind-interrupt-level-0) .SPbind-interrupt-level-0)


(define-arm-vinsn bind-interrupt-level-0-inline (()
                                                 ()
                                                 ((tlb :imm)
                                                  (value :imm)
                                                  (link :imm)
                                                  (temp :imm)))
  (ldr tlb (:@ rcontext (:$ arm::tcr.tlb-pointer)))
  (ldr value (:@ tlb (:$ arm::interrupt-level-binding-index)))
  (ldr link (:@ rcontext (:$ arm::tcr.db-link)))
  (cmp value (:$ 0))
  (mov temp (:$ arm::interrupt-level-binding-index))
  (str value (:@! vsp (:$ -4)))
  (str temp (:@! vsp (:$ -4)))
  (str link (:@! vsp (:$ -4)))
  (mov temp (:$ 0))
  (str temp (:@ tlb (:$ arm::interrupt-level-binding-index)))
  (str vsp  (:@ rcontext (:$ arm::tcr.db-link)))
  (bge :done)
  (ldr nargs (:@ rcontext (:$ arm::tcr.interrupt-pending)))
  (cmp nargs (:$ 0))
  (beq :done)
  (uuo-interrupt-now)
  :done)
                                                    
  
                                                   
(define-arm-subprim-call-vinsn (bind-interrupt-level-m1) .SPbind-interrupt-level-m1)

(define-arm-vinsn bind-interrupt-level-m1-inline (()
                                                  ()
                                                  ((tlb :imm)
                                                   (oldvalue :imm)
                                                   (link :imm)
                                                   (newvalue :imm)
                                                   (idx :imm)))
  (mov newvalue (:$ (ash -1 arm::fixnumshift)))
  (mov idx (:$ arm::interrupt-level-binding-index))
  (ldr tlb (:@ rcontext (:$ arm::tcr.tlb-pointer)))
  (ldr oldvalue (:@ tlb (:$ arm::interrupt-level-binding-index)))
  (ldr link (:@ rcontext (:$ arm::tcr.db-link)))
  (str oldvalue (:@! vsp (:$ (- arm::node-size))))
  (str idx (:@! vsp (:$ (- arm::node-size))))
  (str link (:@! vsp (:$ (- arm::node-size))))
  (str newvalue (:@ tlb (:$ arm::interrupt-level-binding-index)))
  (str vsp  (:@ rcontext (:$ arm::tcr.db-link))))

(define-arm-subprim-call-vinsn (bind-interrupt-level) .SPbind-interrupt-level)

(define-arm-subprim-call-vinsn (unbind-interrupt-level) .SPunbind-interrupt-level)

(define-arm-subprim-call-vinsn (eabi-ff-call-simple) .SPeabi-ff-call-simple)

(define-arm-subprim-call-vinsn (eabi-ff-callhf) .SPeabi-ff-callhf)

(define-arm-vinsn unbind-interrupt-level-inline (()
                                                 ()
                                                 ((preserve (:lisp #.arm::arg_z))
                                                  (preserved (:u32 #.arm::nargs))
                                                  (tlb :imm)
                                                  (link :imm)
                                                  (saved-value :imm)
                                                  (restored-value :imm)))
  (ldr tlb (:@ rcontext (:$ arm::tcr.tlb-pointer)))
  (ldr saved-value (:@ tlb (:$ arm::interrupt-level-binding-index)))
  (ldr link (:@ rcontext (:$ arm::tcr.db-link)))
  (ldr restored-value (:@ link (:$ 8)))
  (ldr link (:@ link (:$ 0)))
  (cmp restored-value (:$ 0))
  (str restored-value (:@ tlb (:$ arm::interrupt-level-binding-index)))
  (str link (:@ rcontext (:$ arm::tcr.db-link)))
  (blt :done)
  (cmp saved-value (:$ 0))
  (bge :done)
  (ldr link (:@ rcontext (:$ arm::tcr.interrupt-pending)))
  (cmp link (:$ 0))
  (beq :done)
  (uuo-interrupt-now)
  :done)

(define-arm-vinsn test-fixnum  (((dest :crf))
                                ((src :lisp)))
  (tst src (:$ arm::fixnummask)))
                  
(define-arm-vinsn test-fixnums (((dest :crf))
                                ((x :lisp)
                                 (y :lisp))
                                ((temp :u32)))
  (orr temp x y)
  (tst temp (:$ arm::fixnummask)))
  




(define-arm-vinsn %ilognot (((dest :imm))
                            ((src :imm))
                            ((temp :u32)))
  (orr temp src (:$ arm::fixnummask))
  (mvn dest temp))

(define-arm-vinsn fixnum-ref-c-double-float (((dest :double-float))
                                             ((base :imm)
                                              (idx :u32const)))
  (fldd dest (:@ base (:$ (:apply ash idx 3)))))

(define-arm-vinsn fixnum-ref-double-float (((dest :double-float))
                                           ((base :imm)
                                            (idx :imm))
                                           ((temp :imm)))
  (add temp base (:lsl idx (:$ 1)))
  (fldd dest (:@ temp (:$ 0))))

(define-arm-vinsn fixnum-set-c-double-float (()
                                             ((base :imm)
                                              (idx :u32const)
                                              (val :double-float)))
  (fstd val (:@ base (:$ (:apply ash idx 3)))))


(define-arm-vinsn fixnum-set-double-float (()
                                           ((base :imm)
                                            (idx :imm)
                                            (val :double-float))
                                           ((temp :imm)))
  (add temp base (:lsl idx (:$ 1)))
  (fstd val (:@ temp (:$ 0))))

(define-arm-vinsn check-soft-float (((flags :crf))
                                    ()
                                    ((temp :u32)))
  (mov temp (:$ (- arm::nil-value arm::fulltag-nil)))
  (ldr temp (:@ temp (:$ (arm::%kernel-global 'arm::float-abi))))
  (tst temp temp))

;;; Do something that sets the Z bit
(define-arm-vinsn set-eq-bit (((flags :crf))
                              ())
  (cmp sp sp))

(define-arm-vinsn ivector-typecode-p (((dest :lisp))
                                      ((src :lisp))
                                      ((temp :u32)))

  (and temp src (:$ (logior arm::tagmask (ash arm::fulltagmask arm::fixnumshift))))
  (cmp temp (:$ (ash arm::fulltag-immheader arm::fixnumshift)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (moveq dest src))
  (movne dest (:$ 0)))

(define-arm-vinsn gvector-typecode-p (((dest :lisp))
                                      ((src :lisp))
                                      ((temp :u32)))

  (and temp src (:$ (logior arm::tagmask (ash arm::fulltagmask arm::fixnumshift))))
  (cmp temp (:$ (ash arm::fulltag-nodeheader arm::fixnumshift)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (moveq dest src))
  (movne dest (:$ 0)))

(define-arm-vinsn %complex-single-float-realpart  (((dest :single-float))
                                                   ((src :complex-single-float)))
  (fcpys dest (:apply * 2 (:apply %hard-regspec-value src))))

(define-arm-vinsn %complex-single-float-imagpart  (((dest :single-float))
                                                   ((src :complex-single-float)))
  (fcpys dest (:apply 1+ (:apply * 2 (:apply %hard-regspec-value src)))))

(define-arm-vinsn %complex-double-float-realpart  (((dest :double-float))
                                                   ((src :complex-double-float)))
  (fcpyd dest src))

(define-arm-vinsn %complex-double-float-imagpart  (((dest :double-float))
                                                   ((src :complex-double-float)))
  (fcpyd dest (:apply 1+ (:apply %hard-regspec-value src))))

  




;;; In case arm::*arm-opcodes* was changed since this file was compiled.
#+maybe-never
(queue-fixup
 (fixup-vinsn-templates *arm-vinsn-templates* arm::*arm-opcode-numbers*))

(provide "ARM-VINSNS")

