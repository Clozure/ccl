;;;-*- Mode: Lisp; Package: CCL -*-
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "PPC32-BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "PPCENV"))

(defmacro define-ppc32-vinsn (vinsn-name (results args &optional temps) &body body)
  (%define-vinsn *ppc32-backend* vinsn-name results args temps body))


;;; Index "scaling" and constant-offset misc-ref vinsns.

(define-ppc32-vinsn scale-node-misc-index (((dest :u32))
                                            ((idx :imm) ; A fixnum
                                             )
                                            ())
  (addi dest idx ppc32::misc-data-offset))

(define-ppc32-vinsn scale-32bit-misc-index (((dest :u32))
                                            ((idx :imm) ; A fixnum
                                             )
                                            ())
  (addi dest idx ppc32::misc-data-offset))

(define-ppc32-vinsn scale-16bit-misc-index (((dest :u32))
                                            ((idx :imm) ; A fixnum
                                             )
                                            ())
  (srwi dest idx 1)
  (addi dest dest ppc32::misc-data-offset))

(define-ppc32-vinsn scale-8bit-misc-index (((dest :u32))
                                           ((idx :imm) ; A fixnum
                                            )
                                           ())
  (srwi dest idx 2)
  (addi dest dest ppc32::misc-data-offset))

(define-ppc32-vinsn scale-64bit-misc-index (((dest :u32))
					    ((idx :imm) ; A fixnum
					     )
					    ())
  (slwi dest idx 1)
  (addi dest dest ppc32::misc-dfloat-offset))

(define-ppc32-vinsn scale-1bit-misc-index (((word-index :u32)
					    (bitnum :u8)) ; (unsigned-byte 5)
					   ((idx :imm) ; A fixnum
					    )
					   )
                                        ; Logically, we want to:
                                        ; 1) Unbox the index by shifting it right 2 bits.
                                        ; 2) Shift (1) right 5 bits
                                        ; 3) Scale (2) by shifting it left 2 bits.
                                        ; We get to do all of this with one instruction
  (rlwinm word-index idx (- ppc32::nbits-in-word 5) 5 (- ppc32::least-significant-bit ppc32::fixnum-shift))
  (addi word-index word-index ppc32::misc-data-offset) ; Hmmm. Also one instruction, but less impressive somehow.
  (extrwi bitnum idx 5 (- ppc32::nbits-in-word (+ ppc32::fixnum-shift 5))))



(define-ppc32-vinsn misc-ref-u32  (((dest :u32))
                                   ((v :lisp)
                                    (scaled-idx :u32))
                                   ())
  (lwzx dest v scaled-idx))


(define-ppc32-vinsn misc-ref-c-u32  (((dest :u32))
                                     ((v :lisp)
                                      (idx :u32const))
                                     ())
  (lwz dest (:apply + ppc32::misc-data-offset (:apply ash idx 2)) v))

(define-ppc32-vinsn misc-ref-s32 (((dest :s32))
                                  ((v :lisp)
                                   (scaled-idx :u32))
                                  ())
  (lwzx dest v scaled-idx))

(define-ppc32-vinsn misc-ref-c-s32  (((dest :s32))
                                     ((v :lisp)
                                      (idx :u32const))
                                     ())
  (lwz dest (:apply + ppc32::misc-data-offset (:apply ash idx 2)) v))


(define-ppc32-vinsn misc-set-c-u32 (()
                                    ((val :u32)
                                     (v :lisp)
                                     (idx :u32const)))
  (stw val (:apply + ppc32::misc-data-offset (:apply ash idx 2)) v))

(define-ppc32-vinsn misc-set-c-s32 (()
                                    ((val :s32)
                                     (v :lisp)
                                     (idx :u32const)))
  (stw val (:apply + ppc32::misc-data-offset (:apply ash idx 2)) v))

(define-ppc32-vinsn misc-set-u32 (()
                                  ((val :u32)
                                   (v :lisp)
                                   (scaled-idx :u32)))
  (stwx val v scaled-idx))

(define-ppc32-vinsn misc-set-s32 (()
                                  ((val :s32)
                                   (v :lisp)
                                   (scaled-idx :u32)))
  (stwx val v scaled-idx))

                              
(define-ppc32-vinsn misc-ref-single-float  (((dest :single-float))
					    ((v :lisp)
					     (scaled-idx :u32))
					    ())
  (lfsx dest v scaled-idx))

(define-ppc32-vinsn misc-ref-c-single-float  (((dest :single-float))
					      ((v :lisp)
					       (idx :u32const))
					      ())
  (lfs dest (:apply + ppc32::misc-data-offset (:apply ash idx 2)) v))

(define-ppc32-vinsn misc-ref-double-float  (((dest :double-float))
					    ((v :lisp)
					     (scaled-idx :u32))
					    ())
  (lfdx dest v scaled-idx))


(define-ppc32-vinsn misc-ref-c-double-float  (((dest :double-float))
					      ((v :lisp)
					       (idx :u32const))
					      ())
  (lfd dest (:apply + ppc32::misc-dfloat-offset (:apply ash idx 3)) v))

(define-ppc32-vinsn misc-set-c-double-float (((val :double-float))
					     ((v :lisp)
					      (idx :u32const)))
  (stfd val (:apply + ppc32::misc-dfloat-offset (:apply ash idx 3)) v))

(define-ppc32-vinsn misc-set-double-float (()
					   ((val :double-float)
					    (v :lisp)
					    (scaled-idx :u32)))
  (stfdx val v scaled-idx))

(define-ppc32-vinsn misc-set-c-single-float (((val :single-float))
					     ((v :lisp)
					      (idx :u32const)))
  (stfs val (:apply + ppc32::misc-data-offset (:apply ash idx 2)) v))

(define-ppc32-vinsn misc-set-single-float (()
					   ((val :single-float)
					    (v :lisp)
					    (scaled-idx :u32)))
  (stfsx val v scaled-idx))


(define-ppc32-vinsn misc-ref-u16  (((dest :u16))
                                   ((v :lisp)
                                    (scaled-idx :u32))
                                   ())
  (lhzx dest v scaled-idx))

(define-ppc32-vinsn misc-ref-c-u16  (((dest :u16))
                                     ((v :lisp)
                                      (idx :u32const))
                                     ())
  (lhz dest (:apply + ppc32::misc-data-offset (:apply ash idx 1)) v))

(define-ppc32-vinsn misc-set-c-u16  (((val :u16))
                                     ((v :lisp)
                                      (idx :u32const))
                                     ())
  (sth val (:apply + ppc32::misc-data-offset (:apply ash idx 1)) v))

(define-ppc32-vinsn misc-set-u16 (((val :u16))
                                  ((v :lisp)
                                   (scaled-idx :s32)))
  (sthx val v scaled-idx))

(define-ppc32-vinsn misc-ref-s16  (((dest :s16))
                                   ((v :lisp)
                                    (scaled-idx :u32))
                                   ())
  (lhax dest v scaled-idx))

(define-ppc32-vinsn misc-ref-c-s16  (((dest :s16))
                                     ((v :lisp)
                                      (idx :u32const))
                                     ())
  (lha dest (:apply + ppc32::misc-data-offset (:apply ash idx 1)) v))


(define-ppc32-vinsn misc-set-c-s16  (((val :s16))
                                     ((v :lisp)
                                      (idx :u32const))
                                     ())
  (sth val (:apply + ppc32::misc-data-offset (:apply ash idx 1)) v))

(define-ppc32-vinsn misc-set-s16 (((val :s16))
                                  ((v :lisp)
                                   (scaled-idx :s32)))
  (sthx val v scaled-idx))

(define-ppc32-vinsn misc-ref-u8  (((dest :u8))
                                  ((v :lisp)
                                   (scaled-idx :u32))
                                  ())
  (lbzx dest v scaled-idx))

(define-ppc32-vinsn misc-ref-c-u8  (((dest :u8))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  (lbz dest (:apply + ppc32::misc-data-offset idx) v))

(define-ppc32-vinsn misc-set-c-u8  (((val :u8))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  (stb val (:apply + ppc32::misc-data-offset idx) v))

(define-ppc32-vinsn misc-set-u8  (((val :u8))
                                  ((v :lisp)
                                   (scaled-idx :u32))
                                  ())
  (stbx val v scaled-idx))

(define-ppc32-vinsn misc-ref-s8  (((dest :s8))
                                  ((v :lisp)
                                   (scaled-idx :u32))
                                  ())
  (lbzx dest v scaled-idx)
  (extsb dest dest))

(define-ppc32-vinsn misc-ref-c-s8  (((dest :s8))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  (lbz dest (:apply + ppc32::misc-data-offset idx) v)
  (extsb dest dest))

(define-ppc32-vinsn misc-set-c-s8  (((val :s8))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  (stb val (:apply + ppc32::misc-data-offset idx) v))

(define-ppc32-vinsn misc-set-s8  (((val :s8))
                                  ((v :lisp)
                                   (scaled-idx :u32))
                                  ())
  (stbx val v scaled-idx))

(define-ppc32-vinsn misc-ref-c-bit (((dest :u8))
                                    ((v :lisp)
                                     (idx :u32const))
                                    ())
  (lwz dest (:apply + ppc32::misc-data-offset (:apply ash idx -5)) v)
  (rlwinm dest dest (:apply 1+ (:apply logand idx #x1f)) 31 31))

(define-ppc32-vinsn misc-ref-c-bit-fixnum (((dest :imm))
                                           ((v :lisp)
                                            (idx :u32const))
                                           ((temp :u32)))
  (lwz temp (:apply + ppc32::misc-data-offset (:apply ash idx -5)) v)
  (rlwinm dest 
          temp
          (:apply + 1 ppc32::fixnumshift (:apply logand idx #x1f)) 
          (- ppc32::least-significant-bit ppc32::fixnumshift)
          (- ppc32::least-significant-bit ppc32::fixnumshift)))


(define-ppc32-vinsn misc-ref-node  (((dest :lisp))
                                    ((v :lisp)
                                     (scaled-idx :s32))
                                    ())
  (lwzx dest v scaled-idx))




(define-ppc32-vinsn misc-ref-c-node (((dest :lisp))
                                     ((v :lisp)
                                      (idx :s16const))
                                     ())
  (lwz dest (:apply + ppc32::misc-data-offset (:apply ash idx 2)) v))

(define-ppc32-vinsn misc-set-node (()
                                  ((val :lisp)
                                   (v :lisp)
                                   (scaled-idx :u32)))
  (stwx val v scaled-idx))

;;; This should only be used for initialization (when the value being
;;; stored is known to be older than the vector V.)
(define-ppc32-vinsn misc-set-c-node (()
                                     ((val :lisp)
                                      (v :lisp)
                                      (idx :s16const))
                                     ())
  (stw val (:apply + ppc32::misc-data-offset (:apply ash idx 2)) v))


(define-ppc32-vinsn misc-element-count-fixnum (((dest :imm))
                                               ((v :lisp))
                                               ((temp :u32)))
  (lwz temp ppc32::misc-header-offset v)
  (rlwinm dest 
          temp 
          (- ppc32::nbits-in-word (- ppc32::num-subtag-bits ppc32::fixnumshift))
          (- ppc32::num-subtag-bits ppc32::fixnumshift) 
          (- ppc32::least-significant-bit ppc32::fixnumshift)))

(define-ppc32-vinsn check-misc-bound (()
                                      ((idx :imm)
                                       (v :lisp))
                                      ((temp :u32)))
  (lwz temp ppc32::misc-header-offset v)
  (rlwinm temp 
          temp 
          (- ppc32::nbits-in-word (- ppc32::num-subtag-bits ppc32::fixnumshift))
          (- ppc32::num-subtag-bits ppc32::fixnumshift) 
          (- ppc32::least-significant-bit ppc32::fixnumshift))
  (twlge idx temp))

(define-ppc32-vinsn 2d-unscaled-index (((dest :imm)
                                        (dim1 :u32))
				       ((dim1 :u32)
                                        (i :imm)
					(j :imm)))
  (mullw dim1 i dim1)
  (add dest dim1 j))

;; dest <- (+ (* i dim1 dim2) (* j dim2) k)
(define-ppc32-vinsn 3d-unscaled-index (((dest :imm)
                                        (dim1 :u32)
                                        (dim2 :u32))
				       ((dim1 :u32)
                                        (dim2 :u32)
                                        (i :imm)
					(j :imm)
                                        (k :imm)))
  (mullw dim1 dim1 dim2)
  (mullw dim2 j dim2)
  (mullw dim1 i dim1)
  (add dim2 dim1 dim2)
  (add dest dim2 k))


(define-ppc32-vinsn 2d-dim1 (((dest :u32))
                             ((header :lisp)))
  (lwz dest (+ ppc32::misc-data-offset (* 4 (1+ ppc32::arrayH.dim0-cell))) header)
  (srawi dest dest ppc32::fixnumshift))

(define-ppc32-vinsn 3d-dims (((dim1 :u32)
                              (dim2 :u32))
                             ((header :lisp)))
  (lwz dim1 (+ ppc32::misc-data-offset (* 4 (1+ ppc32::arrayH.dim0-cell))) header)
  (lwz dim2 (+ ppc32::misc-data-offset (* 4 (+ 2 ppc32::arrayH.dim0-cell))) header)
  (srawi dim1 dim1 ppc32::fixnumshift)
  (srawi dim2 dim2 ppc32::fixnumshift))

;; Return dim1 (unboxed)
(define-ppc32-vinsn check-2d-bound (((dim :u32))
                                    ((i :imm)
                                     (j :imm)
                                     (header :lisp)))
  (lwz dim (+ ppc32::misc-data-offset (* 4 ppc32::arrayH.dim0-cell)) header)
  (twlge i dim)
  (lwz dim (+ ppc32::misc-data-offset (* 4 (1+ ppc32::arrayH.dim0-cell))) header)
  (twlge j dim)
  (srawi dim dim ppc32::fixnumshift))

(define-ppc32-vinsn check-3d-bound (((dim1 :u32)
                                     (dim2 :u32))
                                    ((i :imm)
                                     (j :imm)
                                     (k :imm)
                                     (header :lisp)))
  (lwz dim1 (+ ppc32::misc-data-offset (* 4 ppc32::arrayH.dim0-cell)) header)
  (twlge i dim1)
  (lwz dim1 (+ ppc32::misc-data-offset (* 4 (1+ ppc32::arrayH.dim0-cell))) header)
  (twlge j dim1)
  (lwz dim2 (+ ppc32::misc-data-offset (* 4 (+ 2 ppc32::arrayH.dim0-cell))) header)
  (twlge k dim2)
  (srawi dim1 dim1 ppc32::fixnumshift)
  (srawi dim2 dim2 ppc32::fixnumshift))

(define-ppc32-vinsn array-data-vector-ref (((dest :lisp))
                                           ((header :lisp)))
  (lwz dest ppc32::arrayH.data-vector header))
  

(define-ppc32-vinsn check-arrayH-rank (()
                                       ((header :lisp)
                                        (expected :u32const))
                                       ((rank :imm)))
  (lwz rank ppc32::arrayH.rank header)
  (twi 27 rank (:apply ash expected ppc32::fixnumshift)))

(define-ppc32-vinsn check-arrayH-flags (()
                                        ((header :lisp)
                                         (expected :u16const))
                                        ((flags :imm)
                                         (xreg :u32)))
  (lis xreg (:apply ldb (byte 16 16) (:apply ash expected ppc32::fixnumshift)))
  (ori xreg xreg (:apply ldb (byte 16 0) (:apply ash expected ppc32::fixnumshift)))
  (lwz flags ppc32::arrayH.flags header)
  (tw 27 flags xreg))

  


  
(define-ppc32-vinsn node-slot-ref  (((dest :lisp))
                                    ((node :lisp)
                                     (cellno :u32const)))
  (lwz dest (:apply + ppc32::misc-data-offset (:apply ash cellno 2)) node))



(define-ppc32-vinsn  %slot-ref (((dest :lisp))
                                ((instance (:lisp (:ne dest)))
                                 (index :lisp))
                                ((scaled :u32)))
  (la scaled ppc32::misc-data-offset index)
  (lwzx dest instance scaled)
  (tweqi dest ppc32::slot-unbound-marker))


;;; Untagged memory reference & assignment.

(define-ppc32-vinsn mem-ref-c-fullword (((dest :u32))
                                        ((src :address)
                                         (index :s16const)))
  (lwz dest index src))


(define-ppc32-vinsn mem-ref-c-signed-fullword (((dest :s32))
                                               ((src :address)
                                                (index :s16const)))
  (lwz dest index src))

(define-ppc32-vinsn mem-ref-c-natural (((dest :u32))
                                       ((src :address)
                                        (index :s16const)))
  (lwz dest index src))
  

(define-ppc32-vinsn mem-ref-fullword (((dest :u32))
                                      ((src :address)
                                       (index :s32)))
  (lwzx dest src index))

(define-ppc32-vinsn mem-ref-signed-fullword (((dest :u32))
                                             ((src :address)
                                              (index :s32)))
  (lwzx dest src index))

(define-ppc32-vinsn mem-ref-natural (((dest :u32))
                                     ((src :address)
                                      (index :s32)))
  (lwzx dest src index))


(define-ppc32-vinsn mem-ref-c-u16 (((dest :u16))
                                   ((src :address)
                                    (index :s16const)))
  (lhz dest index src))


(define-ppc32-vinsn mem-ref-u16 (((dest :u16))
                                 ((src :address)
                                  (index :s32)))
  (lhzx dest src index))



(define-ppc32-vinsn mem-ref-c-s16 (((dest :s16))
                                   ((src :address)
                                    (index :s16const)))
  (lha dest index src))

(define-ppc32-vinsn mem-ref-s16 (((dest :s16))
                                 ((src :address)
                                  (index :s32)))
  (lhax dest src index))

(define-ppc32-vinsn mem-ref-c-u8 (((dest :u8))
                                  ((src :address)
                                   (index :s16const)))
  (lbz dest index src))

(define-ppc32-vinsn mem-ref-u8 (((dest :u8))
                                ((src :address)
                                 (index :s32)))
  (lbzx dest src index))

(define-ppc32-vinsn mem-ref-c-s8 (((dest :s8))
                                  ((src :address)
                                   (index :s16const)))
  (lbz dest index src)
  (extsb dest dest))

(define-ppc32-vinsn mem-ref-s8 (((dest :s8))
                                ((src :address)
                                 (index :s32)))
  (lbzx dest src index)
  (extsb dest dest))

(define-ppc32-vinsn mem-ref-c-bit (((dest :u8))
                                   ((src :address)
                                    (byte-index :s16const)
                                    (bit-shift :u8const)))
  (lbz dest byte-index src)
  (rlwinm dest dest bit-shift 31 31))

(define-ppc32-vinsn mem-ref-c-bit-fixnum (((dest :lisp))
                                          ((src :address)
                                           (byte-index :s16const)
                                           (bit-shift :u8const))
                                          ((byteval :u8)))
  (lbz byteval byte-index src)
  (rlwinm dest byteval bit-shift 29 29))

(define-ppc32-vinsn mem-ref-bit (((dest :u8))
                                 ((src :address)
                                  (bit-index :lisp))
                                 ((byte-index :s16)
                                  (bit-shift :u8)))
  (srwi byte-index bit-index (+ ppc32::fixnumshift 3))
  (extrwi bit-shift bit-index 3 27)
  (addi bit-shift bit-shift 29)
  (lbzx dest src byte-index)
  (rlwnm dest dest bit-shift 31 31))


(define-ppc32-vinsn mem-ref-bit-fixnum (((dest :lisp))
                                        ((src :address)
                                         (bit-index :lisp))
                                        ((byte-index :s16)
                                         (bit-shift :u8)))
  (srwi byte-index bit-index (+ ppc32::fixnumshift 3))
  (extrwi bit-shift bit-index 3 27)
  (addi bit-shift bit-shift 27)
  (lbzx byte-index src byte-index)
  (rlwnm dest
         byte-index
         bit-shift
         (- ppc32::least-significant-bit ppc32::fixnum-shift)
         (- ppc32::least-significant-bit ppc32::fixnum-shift)))

(define-ppc32-vinsn mem-ref-c-double-float (((dest :double-float))
					    ((src :address)
					     (index :s16const)))
  (lfd dest index src))

(define-ppc32-vinsn mem-ref-double-float (((dest :double-float))
					  ((src :address)
					   (index :s32)))
  (lfdx dest src index))

(define-ppc32-vinsn mem-set-c-double-float (()
					    ((val :double-float)
					     (src :address)
					     (index :s16const)))
  (stfd val index src))

(define-ppc32-vinsn mem-set-double-float (()
					  ((val :double-float)
					   (src :address)
					   (index :s32)))
  (stfdx val src index))

(define-ppc32-vinsn mem-ref-c-single-float (((dest :single-float))
					    ((src :address)
					     (index :s16const)))
  (lfs dest index src))

(define-ppc32-vinsn mem-ref-single-float (((dest :single-float))
					  ((src :address)
					   (index :s32)))
  (lfsx dest src index))

(define-ppc32-vinsn mem-set-c-single-float (()
					    ((val :single-float)
					     (src :address)
					     (index :s16const)))
  (stfs val index src))

(define-ppc32-vinsn mem-set-single-float (()
					  ((val :single-float)
					   (src :address)
					   (index :s32)))
  (stfsx val src index))


(define-ppc32-vinsn mem-set-c-address (()
                                       ((val :address)
                                        (src :address)
                                        (index :s16const)))
  (stw val index src))

(define-ppc32-vinsn mem-set-address (()
                                     ((val :address)
                                      (src :address)
                                      (index :s32)))
  (stwx val src index))

(define-ppc32-vinsn mem-set-c-fullword (()
					((val :u32)
					 (src :address)
					 (index :s16const)))
  (stw val index src))

(define-ppc32-vinsn mem-set-fullword (()
				      ((val :u32)
				       (src :address)
				       (index :s32)))
  (stwx val src index))

(define-ppc32-vinsn mem-set-c-halfword (()
					((val :u16)
					 (src :address)
					 (index :s16const)))
  (sth val index src))

(define-ppc32-vinsn mem-set-halfword (()
				      ((val :u16)
				       (src :address)
				       (index :s32)))
  (sthx val src index))

(define-ppc32-vinsn mem-set-c-byte (()
				    ((val :u16)
				     (src :address)
				     (index :s16const)))
  (stb val index src))

(define-ppc32-vinsn mem-set-byte (()
				  ((val :u8)
				   (src :address)
				   (index :s32)))
  (stbx val src index))

(define-ppc32-vinsn mem-set-c-bit-0 (()
				     ((src :address)
				      (byte-index :s16const)
				      (mask-begin :u8const)
				      (mask-end :u8const))
				     ((val :u8)))
  (lbz val byte-index src)
  (rlwinm val val 0 mask-begin mask-end)
  (stb val byte-index src))

(define-ppc32-vinsn mem-set-c-bit-1 (()
				     ((src :address)
				      (byte-index :s16const)
				      (mask :u8const))
				     ((val :u8)))
  (lbz val byte-index src)
  (ori val val mask)
  (stb val byte-index src))

(define-ppc32-vinsn mem-set-c-bit (()
				   ((src :address)
				    (byte-index :s16const)
				    (bit-index :u8const)
				    (val :imm))
				   ((byteval :u8)))
  (lbz byteval byte-index src)
  (rlwimi byteval val (:apply logand 31 (:apply - 29 bit-index)) bit-index bit-index)
  (stb byteval byte-index src))

;;; Hey, they should be happy that it even works.  Who cares how big it is or how
;;; long it takes ...
(define-ppc32-vinsn mem-set-bit (()
				 ((src :address)
				  (bit-index :lisp)
				  (val :lisp))
				 ((bit-shift :u32)
				  (mask :u32)
				  (byte-index :u32)
				  (crf :crf)))
  (cmplwi crf val (ash 1 ppc32::fixnumshift))
  (extrwi bit-shift bit-index 3 27)
  (li mask #x80)
  (srw mask mask bit-shift)
  (ble+ crf :got-it)
  (uuo_interr arch::error-object-not-bit src)
  :got-it
  (srwi bit-shift bit-index (+ 3 ppc32::fixnumshift))
  (lbzx bit-shift src bit-shift)
  (beq crf :set)
  (andc mask bit-shift mask)
  (b :done)
  :set
  (or mask bit-shift mask)
  :done
  (srwi bit-shift bit-index (+ 3 ppc32::fixnumshift))
  (stbx mask src bit-shift))
     
;;; Tag and subtag extraction, comparison, checking, trapping ...

(define-ppc32-vinsn extract-tag (((tag :u8)) 
				 ((object :lisp)) 
				 ())
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits)))

(define-ppc32-vinsn extract-tag-fixnum (((tag :imm))
                                        ((object :lisp)))
  (rlwinm tag 
          object 
          ppc32::fixnum-shift 
          (- ppc32::nbits-in-word 
             (+ ppc32::nlisptagbits ppc32::fixnum-shift)) 
          (- ppc32::least-significant-bit ppc32::fixnum-shift)))

(define-ppc32-vinsn extract-fulltag (((tag :u8))
                                     ((object :lisp))
                                     ())
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::ntagbits)))


(define-ppc32-vinsn extract-fulltag-fixnum (((tag :imm))
                                            ((object :lisp)))
  (rlwinm tag 
          object 
          ppc32::fixnum-shift 
          (- ppc32::nbits-in-word 
             (+ ppc32::ntagbits ppc32::fixnum-shift)) 
          (- ppc32::least-significant-bit ppc32::fixnum-shift)))

(define-ppc32-vinsn extract-typecode (((code :u8))
                                      ((object :lisp))
                                      ((crf :crf)))
  (clrlwi code object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf code ppc32::tag-misc)
  (bne crf :not-misc)
  (lbz code ppc32::misc-subtag-offset object)
  :not-misc)

(define-ppc32-vinsn extract-typecode-fixnum (((code :imm))
                                             ((object (:lisp (:ne code))))
                                             ((crf :crf) (subtag :u8)))
  (rlwinm code 
          object 
          ppc32::fixnum-shift 
          (- ppc32::nbits-in-word 
             (+ ppc32::nlisptagbits ppc32::fixnum-shift)) 
          (- ppc32::least-significant-bit ppc32::fixnum-shift))
  (cmpwi crf code (ash ppc32::tag-misc ppc32::fixnum-shift))
  (bne crf :not-misc)
  (lbz subtag ppc32::misc-subtag-offset object)
  (slwi code subtag ppc32::fixnum-shift)
  :not-misc)


(define-ppc32-vinsn require-fixnum (()
                                    ((object :lisp))
                                    ((crf0 (:crf 0))
                                     (tag :u8)))
  :again
  (clrlwi. tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (beq+ crf0 :got-it)
  (uuo_intcerr arch::error-object-not-fixnum object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-integer (()
                                     ((object :lisp))
                                     ((crf0 (:crf 0))
                                      (tag :u8)))
  :again
  (clrlwi. tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (beq+ crf0 :got-it)
  (cmpwi crf0 tag ppc32::tag-misc)
  (bne crf0 :no-got)
  (lbz tag ppc32::misc-subtag-offset object)
  (cmpwi crf0 tag ppc32::subtag-bignum)
  (beq+ crf0 :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-integer object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-simple-vector (()
                                           ((object :lisp))
                                           ((tag :u8)
                                            (crf :crf)))
  :again
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :no-got)
  (lbz tag ppc32::misc-subtag-offset object)
  (cmpwi crf tag ppc32::subtag-simple-vector)
  (beq+ crf :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-simple-vector object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-simple-string (()
                                           ((object :lisp))
                                           ((tag :u8)
                                            (crf :crf)
                                            (crf2 :crf)))
  :again
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :no-got)
  (lbz tag ppc32::misc-subtag-offset object)
  (cmpwi crf tag ppc32::subtag-simple-base-string)
  (beq+ crf :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-simple-string object)
  (b :again)
  :got-it)

  
(define-ppc32-vinsn require-real (()
                                  ((object :lisp))
                                  ((crf0 (:crf 0))
                                   (tag :u8)))
  :again
  (clrlwi. tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (beq+ crf0 :got-it)
  (cmpwi crf0 tag ppc32::tag-misc)
  (bne crf0 :no-got)
  (lbz tag ppc32::misc-subtag-offset object)
  (cmplwi crf0 tag ppc32::max-real-subtag)
  (ble+ crf0 :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-real object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-number (()
                                    ((object :lisp))
                                    ((crf0 (:crf 0))
                                     (tag :u8)))
  :again
  (clrlwi. tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (beq+ crf0 :got-it)
  (cmpwi crf0 tag ppc32::tag-misc)
  (bne crf0 :no-got)
  (lbz tag ppc32::misc-subtag-offset object)
  (cmplwi crf0 tag ppc32::max-numeric-subtag)
  (ble+ crf0 :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-number object)
  (b :again)
  :got-it)


(define-ppc32-vinsn require-list (()
                                  ((object :lisp))
                                  ((tag :u8)
                                   (crf :crf)))
  :again
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-list)
  (beq+ crf :got-it)
  (uuo_intcerr arch::error-object-not-list object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-symbol (()
                                    ((object :lisp))
                                    ((tag :u8)
                                     (crf :crf)))
  :again
  (cmpwi crf object (:apply target-nil-value))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (beq crf :got-it)
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :no-got)
  (lbz tag ppc32::misc-subtag-offset object)
  (cmpwi crf tag ppc32::subtag-symbol)
  (beq+ crf :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-symbol object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-character (()
                                       ((object :lisp))
                                       ((tag :u8)
                                        (crf :crf)))
  :again
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::num-subtag-bits))
  (cmpwi crf tag ppc32::subtag-character)
  (beq+ crf :got-it)
  (uuo_intcerr arch::error-object-not-character object)
  (b :again)
  :got-it)


(define-ppc32-vinsn require-s8 (()
                                ((object :lisp))
                                ((crf :crf)
                                 (tag :u32)))
  :again
  (slwi tag object (- ppc32::nbits-in-word (+ 8 ppc32::fixnumshift)))
  (srawi tag tag (- ppc32::nbits-in-word 8 ))
  (slwi tag tag ppc32::fixnumshift)
  (cmpw crf tag object)
  (beq+ crf :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-signed-byte-8 object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-u8 (()
                                ((object :lisp))
                                ((crf0 (:crf 0))
                                 (tag :u32)))
  :again
  ;; The bottom ppc32::fixnumshift bits and the top (- 32 (+
  ;; ppc32::fixnumshift 8)) must all be zero.
  (rlwinm. tag object 0 (- ppc32::nbits-in-word ppc32::fixnumshift) (- ppc32::least-significant-bit (+ ppc32::fixnumshift 8)))
  (beq+ crf0 :got-it)
  (uuo_intcerr arch::error-object-not-unsigned-byte-8 object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-s16 (()
                                ((object :lisp))
                                ((crf :crf)
                                 (tag :u32)))
  :again
  (slwi tag object (- ppc32::nbits-in-word (+ 16 ppc32::fixnumshift)))
  (srawi tag tag (- ppc32::nbits-in-word 16))
  (slwi tag tag ppc32::fixnumshift)
  (cmpw crf tag object)
  (beq+ crf :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-signed-byte-16 object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-u16 (()
                                ((object :lisp))
                                ((crf0 (:crf 0))
                                 (tag :u32)))
  :again
  ;; The bottom ppc32::fixnumshift bits and the top (- 32 (+
  ;; ppc32::fixnumshift 16)) must all be zero.
  (rlwinm. tag object 0 (- ppc32::nbits-in-word ppc32::fixnumshift) (- ppc32::least-significant-bit (+ ppc32::fixnumshift 16)))
  (beq+ crf0 :got-it)
  (uuo_intcerr arch::error-object-not-unsigned-byte-16 object)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-s32 (()
                                 ((src :lisp))
                                 ((crfx :crf)
                                  (crfy :crf)
                                  (tag :u32)))
  :again
  (clrlwi tag src (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crfx tag ppc32::tag-fixnum)
  (cmpwi crfy tag ppc32::tag-misc)
  (beq+ crfx :got-it)
  (bne- crfy :bad)
  (lwz tag ppc32::misc-header-offset src)
  (cmpwi crfx tag ppc32::one-digit-bignum-header)
  (beq+ crfx :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-signed-byte-32 src)
  (b :again)
  :got-it)


(define-ppc32-vinsn require-u32 (()
                                 ((src :lisp))
                                 ((crf0 (:crf 0))
                                  (crf1 :crf)
                                  (temp :u32)))
  :again
  (rlwinm. temp src 0 (- ppc32::nbits-in-word ppc32::fixnumshift) 0)
  (beq+ crf0 :got-it)
  (clrlwi temp src (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf0 temp ppc32::tag-misc)
  (bne- crf0 :bad)
  (lwz temp ppc32::misc-header-offset src)
  (cmpwi crf1 temp ppc32::two-digit-bignum-header)
  (cmpwi crf0 temp ppc32::one-digit-bignum-header)
  (lwz temp ppc32::misc-data-offset src)
  (beq crf1 :two)
  (bne crf0 :bad)
  (cmpwi crf0 temp 0)
  (bgt+ crf0 :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-unsigned-byte-32 src)
  (b :again)
  :two
  (lwz temp (+ ppc32::misc-data-offset 4) src)
  (cmpwi crf0 temp 0)
  (bne- crf0 :bad)
  :got-it)

(define-ppc32-vinsn require-s64 (()
                                 ((src :lisp))
                                 ((crfx :crf)
                                  (crfy :crf)
                                  (tag :u32)))
  :again
  (clrlwi tag src (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crfx tag ppc32::tag-fixnum)
  (cmpwi crfy tag ppc32::tag-misc)
  (beq+ crfx :got-it)
  (bne- crfy :bad)
  (lwz tag ppc32::misc-header-offset src)
  (cmpwi crfx tag ppc32::one-digit-bignum-header)
  (cmpwi crfy tag ppc32::two-digit-bignum-header)
  (lwz tag ppc32::misc-data-offset src)
  (beq+ crfx :got-it)
  (beq+ crfy :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-signed-byte-64 src)
  (b :again)
  :got-it)

(define-ppc32-vinsn require-u64 (()
                                 ((src :lisp))
                                 ((crf0 (:crf 0))
                                  (crf1 :crf)
                                  (crf2 :crf)
                                  (temp :u32)))
  :again
  (rlwinm. temp src 0 (- ppc32::nbits-in-word ppc32::fixnumshift) 0)
  (beq+ crf0 :got-it)
  (clrlwi temp src (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf0 temp ppc32::tag-misc)
  (bne- crf0 :bad)
  (lwz temp ppc32::misc-header-offset src)
  (cmpwi crf2 temp ppc32::three-digit-bignum-header)
  (cmpwi crf1 temp ppc32::two-digit-bignum-header)
  (cmpwi crf0 temp ppc32::one-digit-bignum-header)
  (lwz temp ppc32::misc-data-offset src)
  (beq crf2 :three)
  (beq crf1 :two)
  (bne crf0 :bad)
  (cmpwi crf0 temp 0)
  (bgt+ crf0 :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-unsigned-byte-64 src)
  (b :again)
  :three
  (lwz temp (+ ppc32::misc-data-offset 8) src)
  (cmpwi crf0 temp 0)
  (beq+ crf0 :got-it)
  (b :bad)
  :two
  (lwz temp (+ ppc32::misc-data-offset 4) src)
  (cmpwi crf0 temp 0)
  (blt- crf0 :bad)
  :got-it)



(define-ppc32-vinsn require-char-code (()
                                       ((object :lisp))
                                       ((crf0 (:crf 0))
                                        (crf1 :crf)
                                        (tag :u32)))
  :again
  (clrlwi. tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (lis tag (ash (ash #x110000 ppc32::fixnumshift) -16))
  (cmplw crf1 object tag)
  (bne crf0 :bad)
  (blt+ crf1 :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-mod-char-code-limit object)
  (b :again)
  :got-it)


(define-ppc32-vinsn box-fixnum (((dest :imm))
                                ((src :s32)))
  (slwi dest src ppc32::fixnumshift))

(define-ppc32-vinsn fixnum->signed-natural (((dest :s32))
                                            ((src :imm)))
  (srawi dest src ppc32::fixnumshift))

(define-ppc32-vinsn fixnum->unsigned-natural (((dest :u32))
                                              ((src :imm)))
  (srwi dest src ppc32::fixnumshift))

;;; An object is of type (UNSIGNED-BYTE 32) iff
;;;  a) it's of type (UNSIGNED-BYTE 30) (e.g., an unsigned fixnum)
;;;  b) it's a bignum of length 1 and the 0'th digit is positive
;;;  c) it's a bignum of length 2 and the sign-digit is 0.

(define-ppc32-vinsn unbox-u32 (((dest :u32))
                               ((src :lisp))
                               ((crf0 (:crf 0))
                                (crf1 :crf)))
  (rlwinm. dest src 0 (- ppc32::nbits-in-word ppc32::fixnumshift) 0)
  (srwi dest src ppc32::fixnumshift)
  (beq+ crf0 :got-it)
  (clrlwi dest src (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf0 dest ppc32::tag-misc)
  (bne- crf0 :bad)
  (lwz dest ppc32::misc-header-offset src)
  (cmpwi crf1 dest ppc32::two-digit-bignum-header)
  (cmpwi crf0 dest ppc32::one-digit-bignum-header)
  (lwz dest ppc32::misc-data-offset src)
  (beq crf1 :two)
  (bne crf0 :bad)
  (cmpwi crf0 dest 0)
  (bgt+ crf0 :got-it)
  :bad
  (uuo_interr arch::error-object-not-unsigned-byte-32 src)
  :two
  (lwz dest (+ ppc32::misc-data-offset 4) src)
  (cmpwi crf0 dest 0)
  (bne- crf0 :bad)
  (lwz dest ppc32::misc-data-offset src)
  :got-it)

;;; an object is of type (SIGNED-BYTE 32) iff
;;; a) it's a fixnum
;;; b) it's a bignum with exactly one digit.

(define-ppc32-vinsn unbox-s32 (((dest :s32))
                               ((src :lisp))
                               ((crfx :crf)
                                (crfy :crf)
                                (tag :u32)))
  (clrlwi tag src (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crfx tag ppc32::tag-fixnum)
  (cmpwi crfy tag ppc32::tag-misc)
  (srawi dest src ppc32::fixnumshift)
  (beq+ crfx :got-it)
  (bne- crfy :bad)
  (lwz tag ppc32::misc-header-offset src)
  (cmpwi crfx tag ppc32::one-digit-bignum-header)
  (lwz dest ppc32::misc-data-offset src)
  (beq+ crfx :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-32 src)
  :got-it)

;;; For the sake of argument, "dest" is u32.
;;; Return dest if src is either (signed-byte 32) or (unsigned-byte 32).
;;; Say that it's not (signed-byte 32) if neither.
(define-ppc32-vinsn unbox-x32 (((dest :u32))
                               ((src :lisp))
                               ((crfx :crf)
                                (crfy :crf)
                                (tag :u32)))
  (clrlwi tag src (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crfx tag ppc32::tag-fixnum)
  (cmpwi crfy tag ppc32::tag-misc)
  (srawi dest src ppc32::fixnumshift)
  (beq+ crfx :got-it)
  (bne- crfy :bad)
  (lwz tag ppc32::misc-header-offset src)
  (cmpwi crfx tag (logior (ash 1 ppc32::num-subtag-bits) ppc32::subtag-bignum))
  (cmpwi crfy tag (logior (ash 2 ppc32::num-subtag-bits) ppc32::subtag-bignum))
  (lwz dest ppc32::misc-data-offset src)
  (beq crfx :got-it)
  (lwz tag (+ 4 ppc32::misc-data-offset) src)
  (cmpwi crfx tag 0)
  (bne crfy :bad)
  (beq+ crfx :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-32 src)
  :got-it)

(define-ppc32-vinsn unbox-u16 (((dest :u16))
                               ((src :lisp))
                               ((crf0 (:crf 0))))
                                        ; The bottom ppc32::fixnumshift bits and the top (- 31 (+ ppc32::fixnumshift 16)) must all be zero.
  (rlwinm. dest src 0 (- ppc32::nbits-in-word ppc32::fixnumshift) (- ppc32::least-significant-bit (+ ppc32::fixnumshift 16)))
  (rlwinm dest src (- 32 ppc32::fixnumshift) 16 31)
  (beq+ crf0 :got-it)
  (uuo_interr arch::error-object-not-unsigned-byte-16 src)
  :got-it)

(define-ppc32-vinsn unbox-s16 (((dest :s16))
                               ((src :lisp))
                               ((crf :crf)))
  (slwi dest src (- ppc32::nbits-in-word (+ 16 ppc32::fixnumshift)))
  (srawi dest dest (- ppc32::nbits-in-word 16))
  (slwi dest dest ppc32::fixnumshift)
  (cmpw crf dest src)
  (srawi dest src ppc32::fixnumshift)
  (beq+ crf :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-16 src)
  :got-it)

  
  
(define-ppc32-vinsn unbox-u8 (((dest :u8))
                              ((src :lisp))
                              ((crf0 (:crf 0))))
  ;; The bottom ppc32::fixnumshift bits and the top (- 31 (+
  ;; ppc32::fixnumshift 8)) must all be zero.
  (rlwinm. dest src 0 (- ppc32::nbits-in-word ppc32::fixnumshift) (- ppc32::least-significant-bit (+ ppc32::fixnumshift 8)))
  (rlwinm dest src (- 32 ppc32::fixnumshift) 24 31)
  (beq+ crf0 :got-it)
  (uuo_interr arch::error-object-not-unsigned-byte-8 src)
  :got-it)

(define-ppc32-vinsn %unbox-u8 (((dest :u8))
                              ((src :lisp))
)
  (rlwinm dest src (- 32 ppc32::fixnumshift) 24 31))

(define-ppc32-vinsn unbox-s8 (((dest :s8))
                              ((src :lisp))
                              ((crf :crf)))
  (slwi dest src (- ppc32::nbits-in-word (+ 8 ppc32::fixnumshift)))
  (srawi dest dest (- ppc32::nbits-in-word 8))
  (slwi dest dest ppc32::fixnumshift)
  (cmpw crf dest src)
  (srawi dest src ppc32::fixnumshift)
  (beq+ crf :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-8 src)
  :got-it)

(define-ppc32-vinsn unbox-base-char (((dest :u32))
                                     ((src :lisp))
                                     ((crf :crf)))
  (rlwinm dest src 0 24 31)
  (cmpwi crf dest ppc32::subtag-character)
  (srwi dest src ppc32::charcode-shift)
  (beq+ crf :got-it)
  (uuo_interr arch::error-object-not-base-char src)
  :got-it)


(define-ppc32-vinsn unbox-bit (((dest :u32))
                               ((src :lisp))
                               ((crf :crf)))
  (cmplwi crf src (ash 1 ppc32::fixnumshift))
  (srawi dest src ppc32::fixnumshift)
  (ble+ crf :got-it)
  (uuo_interr arch::error-object-not-bit src)
  :got-it)

(define-ppc32-vinsn unbox-bit-bit0 (((dest :u32))
                                    ((src :lisp))
                                    ((crf :crf)))
  (cmplwi crf src (ash 1 ppc32::fixnumshift))
  (rlwinm dest src (- 32 (1+ ppc32::fixnumshift)) 0 0)
  (ble+ crf :got-it)
  (uuo_interr arch::error-object-not-bit src)
  :got-it)

(define-ppc32-vinsn fixnum->fpr (((dest :double-float))
                                 ((src :lisp))
                                 ((imm :s32)))
  (stfd ppc::fp-s32conv -8 ppc::sp)
  (srawi imm src ppc32::fixnumshift)
  (xoris imm imm #x8000)
  (stw imm -4 ppc::sp)
  (lfd dest -8 ppc::sp)
  (fsub dest dest ppc::fp-s32conv))


(define-ppc32-vinsn shift-right-variable-word (((dest :u32))
                                               ((src :u32)
                                                (sh :u32)))
  (srw dest src sh))

(define-ppc32-vinsn u32logandc2 (((dest :u32))
                                 ((x :u32)
                                  (y :u32)))
  (andc dest x y))

(define-ppc32-vinsn u32logior (((dest :u32))
                               ((x :u32)
                                (y :u32)))
  (or dest x y))

(define-ppc32-vinsn rotate-left-variable-word (((dest :u32))
                                               ((src :u32)
                                                (rot :u32)))
  (rlwnm dest src rot 0 31))

(define-ppc32-vinsn complement-shift-count (((dest :u32))
                                            ((src :u32)))
  (subfic dest src 32))

(define-ppc32-vinsn extract-lowbyte (((dest :u32))
                                     ((src :lisp)))
  (clrlwi dest src (- ppc32::nbits-in-word ppc32::num-subtag-bits)))

;;; Set DEST to the difference between the low byte of SRC and BYTEVAL.
(define-ppc32-vinsn extract-compare-lowbyte (((dest :u32))
                                             ((src :lisp)
                                              (byteval :u8const)))
  (clrlwi dest src (- ppc32::nbits-in-word ppc32::num-subtag-bits))
  (subi dest dest byteval))


;;; Set the "EQ" bit in condition-register field CRF if object is
;;; a fixnum.  Leave the object's tag in TAG.
;;; This is a little easier if CRF is CR0.
(define-ppc32-vinsn eq-if-fixnum (((crf :crf)
                                   (tag :u8))
                                  ((object :lisp))
                                  ())
  ((:eq crf 0)
   (clrlwi. tag object (- ppc32::nbits-in-word ppc32::nlisptagbits)))
  ((:not (:eq crf 0))
   (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
   (cmpwi crf tag ppc32::tag-fixnum)))



(define-ppc32-vinsn trap-unless-fixnum (()
					((object :lisp))
					((tag :u8)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (twnei tag ppc32::tag-fixnum))

(define-ppc32-vinsn trap-unless-list (()
                                      ((object :lisp))
                                      ((tag :u8)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (twnei tag ppc32::tag-list))

(define-ppc32-vinsn trap-unless-single-float (()
                                              ((object :lisp))
                                              ((tag :u8)
                                               (crf :crf)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :do-trap)
  (lbz tag ppc32::misc-subtag-offset object)
  :do-trap
  (twnei tag ppc32::subtag-single-float))

(define-ppc32-vinsn trap-unless-double-float (()
                                              ((object :lisp))
                                              ((tag :u8)
                                               (crf :crf)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :do-trap)
  (lbz tag ppc32::misc-subtag-offset object)
  :do-trap
  (twnei tag ppc32::subtag-double-float))


(define-ppc32-vinsn trap-unless-array-header (()
                                              ((object :lisp))
                                              ((tag :u8)
                                               (crf :crf)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :do-trap)
  (lbz tag ppc32::misc-subtag-offset object)
  :do-trap
  (twnei tag ppc32::subtag-arrayH))

(define-ppc32-vinsn trap-unless-macptr (()
                                        ((object :lisp))
                                        ((tag :u8)
                                         (crf :crf)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :do-trap)
  (lbz tag ppc32::misc-subtag-offset object)
  :do-trap
  (twnei tag ppc32::subtag-macptr))



(define-ppc32-vinsn trap-unless-uvector (()
					 ((object :lisp))
                                         ((tag :u8)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (twnei tag ppc32::tag-misc))

(define-ppc32-vinsn trap-unless-fulltag= (()
                                          ((object :lisp)
                                           (tagval :u16const))
                                          ((tag :u8)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::ntagbits))
  (twnei tag tagval))

(define-ppc32-vinsn trap-unless-lowbyte= (()
                                          ((object :lisp)
                                           (tagval :u16const))
                                          ((tag :u8)))
  (clrlwi tag object (- ppc32::nbits-in-word 8))
  (twnei tag tagval))

(define-ppc32-vinsn trap-unless-character (()
                                           ((object :lisp))
                                           ((tag :u8)))
  (clrlwi tag object (- ppc32::nbits-in-word 8))
  (twnei tag ppc32::subtag-character))

(define-ppc32-vinsn trap-unless-cons (()
                                      ((object :lisp))
                                      ((tag :u8)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::ntagbits))
  (twnei tag ppc32::fulltag-cons))

(define-ppc32-vinsn trap-unless-typecode= (()
                                           ((object :lisp)
                                            (tagval :u16const))
                                           ((tag :u8)
                                            (crf :crf)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :do-trap)
  (lbz tag ppc32::misc-subtag-offset object)
  :do-trap
  (twnei tag tagval))
  
(define-ppc32-vinsn subtract-constant (((dest :imm))
                                       ((src :imm)
                                        (const :s16const)))
  (subi dest src const))

(define-ppc32-vinsn trap-unless-numeric-type (()
                                              ((object :lisp)
                                               (maxtype :u16const))
                                              ((crf0 (:crf 0))
                                               (tag :u8)
                                               (crfX :crf)))
  (clrlwi. tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi tag ppc32::tag-misc)
  (beq+ crf0 :fixnum)
  (bne crfX :scale-tag)
  (lbz tag ppc32::misc-subtag-offset object)
  :scale-tag
  (subi tag tag ppc32::min-numeric-subtag)
  (twlgti tag (:apply - maxtype ppc32::min-numeric-subtag))
  :fixnum)


;; Bit-extraction & boolean operations

(eval-when (:compile-toplevel :execute)
  (assert (= ppc32::t-offset #b10001))) ; PPC-bits 31 and 27 set

;; For some mind-numbing reason, IBM decided to call the most significant
;; bit in a 32-bit word "bit 0" and the least significant bit "bit 31"
;; (this despite the fact that it's essentially a big-endian architecture
;; (it was exclusively big-endian when this decision was made.))
;; We'll probably be least confused if we consistently use this backwards
;; bit ordering (letting things that have a "sane" bit-number worry about
;; it at compile-time or run-time (subtracting the "sane" bit number from
;; 31.))

(define-ppc32-vinsn extract-variable-bit (((dest :u8))
                                          ((src :u32)
                                           (bitnum :u8))
                                          ())
  (rotlw dest src bitnum)
  (extrwi dest dest 1 0))


(define-ppc32-vinsn extract-variable-bit-fixnum (((dest :imm))
                                                 ((src :u32)
                                                  (bitnum :u8))
                                                 ((temp :u32)))
  (rotlw temp src bitnum)
  (rlwinm dest
          temp 
          (1+ ppc32::fixnumshift) 
          (- ppc32::least-significant-bit ppc32::fixnumshift)
          (- ppc32::least-significant-bit ppc32::fixnumshift)))


;; Sometimes we try to extract a single bit from some source register
;; into a destination bit (typically 31, sometimes fixnum bit 0 = 29).
;; If we copy bit 0 (whoops, I mean "bit 31") to bit 4 (aka 27) in a
;; given register, we get a value that's either 17 (the arithmetic difference
;; between T and NIL) or 0.

(define-ppc32-vinsn lowbit->truth (((dest :lisp)
                                    (bits :u32))
                                   ((bits :u32))
                                   ())
  (rlwimi bits bits (- ppc32::least-significant-bit 27) 27 27) ; bits = 0000...X000X
  (addi dest bits (:apply target-nil-value)))

(define-ppc32-vinsn invert-lowbit (((bits :u32))
                                   ((bits :u32))
                                   ())
  (xori bits bits 1))

                           

;; Some of the obscure-looking instruction sequences - which map some relation
;; to PPC bit 31 of some register - were found by the GNU SuperOptimizer.
;; Some of them use extended-precision instructions (which may cause interlocks
;; on some superscalar PPCs, if I remember correctly.)  In general, sequences
;; that GSO found that -don't- do extended precision are longer and/or use
;; more temporaries.
;; On the 604, the penalty for using an instruction that uses the CA bit is
;; "at least" one cycle: it can't complete execution until all "older" instructions
;; have.  That's not horrible, especially given that the alternative is usually
;; to use more instructions (and, more importantly, more temporaries) to avoid
;; using extended-precision.


(define-ppc32-vinsn eq0->bit31 (((bits :u32))
                                ((src (t (:ne bits)))))
  (cntlzw bits src)
  (srwi bits bits 5))                   ; bits = 0000...000X

(define-ppc32-vinsn ne0->bit31 (((bits :u32))
                                ((src (t (:ne bits)))))
  (cntlzw bits src)
  (slw bits src bits)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc32-vinsn lt0->bit31 (((bits :u32))
                                ((src (t (:ne bits)))))
  (srwi bits src 31))                   ; bits = 0000...000X


(define-ppc32-vinsn ge0->bit31 (((bits :u32))
                                ((src (t (:ne bits)))))
  (srwi bits src 31)       
  (xori bits bits 1))                   ; bits = 0000...000X


(define-ppc32-vinsn le0->bit31 (((bits :u32))
                                ((src (t (:ne bits)))))
  (neg bits src)
  (orc bits bits src)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc32-vinsn gt0->bit31 (((bits :u32))
                                ((src (t (:ne bits)))))
  (subi bits src 1)       
  (nor bits bits src)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc32-vinsn ne->bit31 (((bits :u32))
                               ((x t)
                                (y t))
                               ((temp :u32)))
  (subf temp x y)
  (cntlzw bits temp)
  (slw bits temp bits)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc32-vinsn fulltag->bit31 (((bits :u32))
                                    ((lispobj :lisp)
                                     (tagval :u8const))
                                    ())
  (clrlwi bits lispobj (- ppc32::nbits-in-word ppc32::ntagbits))
  (subi bits bits tagval)
  (cntlzw bits bits)
  (srwi bits bits 5))


(define-ppc32-vinsn eq->bit31 (((bits :u32))
                               ((x t)
                                (y t)))
  (subf bits x y)
  (cntlzw bits bits)
  (srwi bits bits 5))                   ; bits = 0000...000X

(define-ppc32-vinsn eqnil->bit31 (((bits :u32))
                                  ((x t)))
  (subi bits x (:apply target-nil-value))
  (cntlzw bits bits)
  (srwi bits bits 5))

(define-ppc32-vinsn ne->bit31 (((bits :u32))
                               ((x t)
                                (y t)))
  (subf bits x y)
  (cntlzw bits bits)
  (srwi bits bits 5)
  (xori bits bits 1))

(define-ppc32-vinsn nenil->bit31 (((bits :u32))
                                  ((x t)))
  (subi bits x (:apply target-nil-value))
  (cntlzw bits bits)
  (srwi bits bits 5)
  (xori bits bits 1))

(define-ppc32-vinsn lt->bit31 (((bits :u32))
                               ((x (t (:ne bits)))
                                (y (t (:ne bits)))))

  (xor bits x y)
  (srawi bits bits 31)
  (or bits bits x)
  (subf bits y bits)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc32-vinsn ltu->bit31 (((bits :u32))
                                ((x :u32)
                                 (y :u32)))
  (subfc bits y x)
  (subfe bits bits bits)
  (neg bits bits))

(define-ppc32-vinsn le->bit31 (((bits :u32))
                               ((x (t (:ne bits)))
                                (y (t (:ne bits)))))

  (xor bits x y)
  (srawi bits bits 31)
  (nor bits bits y)
  (add bits bits x)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc32-vinsn leu->bit31  (((bits :u32))
                                 ((x :u32)
                                  (y :u32)))
  (subfc bits x y)
  (addze bits ppc::rzero))

(define-ppc32-vinsn gt->bit31 (((bits :u32))
                               ((x (t (:ne bits)))
                                (y (t (:ne bits)))))

  (eqv bits x y)
  (srawi bits bits 31)
  (and bits bits x)
  (subf bits bits y)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc32-vinsn gtu->bit31 (((bits :u32))
                                ((x :u32)
                                 (y :u32)))
  (subfc bits x y)
  (subfe bits bits bits)
  (neg bits bits))

(define-ppc32-vinsn ge->bit31 (((bits :u32))
                               ((x (t (:ne bits)))
                                (y (t (:ne bits)))))
  (eqv bits x y)
  (srawi bits bits 31)
  (andc bits bits x)
  (add bits bits y)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc32-vinsn geu->bit31 (((bits :u32))
                                ((x :u32)
                                 (y :u32)))
  (subfc bits y x)
  (addze bits ppc::rzero))


;;; there are big-time latencies associated with MFCR on more heavily
;;; pipelined processors; that implies that we should avoid this like
;;; the plague.
;;; GSO can't find anything much quicker for LT or GT, even though
;;; MFCR takes three cycles and waits for previous instructions to complete.
;;; Of course, using a CR field costs us something as well.
(define-ppc32-vinsn crbit->bit31 (((bits :u32))
                                  ((crf :crf)
                                   (bitnum :crbit))
                                  ())
  (mfcr bits)                           ; Suffer.
  (rlwinm bits bits (:apply + 1  bitnum (:apply ash crf 2)) 31 31)) ; bits = 0000...000X


(define-ppc32-vinsn compare (((crf :crf))
                             ((arg0 t)
                              (arg1 t))
                             ())
  (cmpw crf arg0 arg1))

(define-ppc32-vinsn compare-to-nil (((crf :crf))
                                    ((arg0 t)))
  (cmpwi crf arg0 (:apply target-nil-value)))

(define-ppc32-vinsn compare-logical (((crf :crf))
                                     ((arg0 t)
                                      (arg1 t))
                                     ())
  (cmplw crf arg0 arg1))

(define-ppc32-vinsn double-float-compare (((crf :crf))
                                          ((arg0 :double-float)
                                           (arg1 :double-float))
                                          ())
  (fcmpo crf arg0 arg1))
              

(define-ppc32-vinsn double-float+-2 (((result :double-float))
                                     ((x :double-float)
                                      (y :double-float))
                                     ((crf (:crf 4))))
  (fadd result x y))

(define-ppc32-vinsn double-float--2 (((result :double-float))
                                     ((x :double-float)
                                      (y :double-float))
                                     ((crf (:crf 4))))
  (fsub result x y))

(define-ppc32-vinsn double-float*-2 (((result :double-float))
                                     ((x :double-float)
                                      (y :double-float))
                                     ((crf (:crf 4))))
  (fmul result x y))

(define-ppc32-vinsn double-float/-2 (((result :double-float))
                                     ((x :double-float)
                                      (y :double-float))
                                     ((crf (:crf 4))))
  (fdiv result x y))

(define-ppc32-vinsn single-float+-2 (((result :single-float))
                                     ((x :single-float)
                                      (y :single-float))
                                     ((crf (:crf 4))))
  (fadds result x y))

(define-ppc32-vinsn single-float--2 (((result :single-float))
                                     ((x :single-float)
                                      (y :single-float))
                                     ((crf (:crf 4))))
  (fsubs result x y))

(define-ppc32-vinsn single-float*-2 (((result :single-float))
                                     ((x :single-float)
                                      (y :single-float))
                                     ((crf (:crf 4))))
  (fmuls result x y))

(define-ppc32-vinsn single-float/-2 (((result :single-float))
                                     ((x :single-float)
                                      (y :single-float))
                                     ((crf (:crf 4))))
  (fdivs result x y))





(define-ppc32-vinsn compare-unsigned (((crf :crf))
                                      ((arg0 :imm)
                                       (arg1 :imm))
                                      ())
  (cmplw crf arg0 arg1))

(define-ppc32-vinsn compare-signed-s16const (((crf :crf))
                                             ((arg0 :imm)
                                              (imm :s16const))
                                             ())
  (cmpwi crf arg0 imm))

(define-ppc32-vinsn compare-unsigned-u16const (((crf :crf))
                                               ((arg0 :u32)
                                                (imm :u16const))
                                               ())
  (cmplwi crf arg0 imm))



;; Extract a constant bit (0-31) from src; make it be bit 31 of dest.
;; Bitnum is treated mod 32.
(define-ppc32-vinsn extract-constant-ppc-bit (((dest :u32))
                                              ((src :imm)
                                               (bitnum :u16const))
                                              ())
  (rlwinm dest src (:apply + 1 bitnum) 31 31))


(define-ppc32-vinsn set-constant-ppc-bit-to-variable-value (((dest :u32))
                                                            ((src :u32)
                                                             (bitval :u32) ; 0 or 1
                                                             (bitnum :u8const)))
  (rlwimi dest bitval (:apply - 32 bitnum) bitnum bitnum))

(define-ppc32-vinsn set-constant-ppc-bit-to-1 (((dest :u32))
                                               ((src :u32)
                                                (bitnum :u8const)))
  ((:pred < bitnum 16)
   (oris dest src (:apply ash #x8000 (:apply - bitnum))))
  ((:pred >= bitnum 16)
   (ori dest src (:apply ash #x8000 (:apply - (:apply - bitnum 16))))))

(define-ppc32-vinsn set-constant-ppc-bit-to-0 (((dest :u32))
                                               ((src :u32)
                                                (bitnum :u8const)))
  (rlwinm dest src 0 (:apply logand #x1f (:apply 1+ bitnum)) (:apply logand #x1f (:apply 1- bitnum))))

  
(define-ppc32-vinsn insert-bit-0 (((dest :u32))
                                  ((src :u32)
                                   (val :u32)))
  (rlwimi dest val 0 0 0))
  
;;; The bit number is boxed and wants to think of the least-significant bit as 0.
;;; Imagine that.
;;; To turn the boxed, lsb-0 bitnumber into an unboxed, msb-0 rotate count,
;;; we (conceptually) unbox it, add ppc32::fixnumshift to it, subtract it from
;;; 31, and add one.  This can also be done as "unbox and subtract from 28",
;;; I think ...
;;; Actually, it'd be "unbox, then subtract from 30".
(define-ppc32-vinsn extract-variable-non-insane-bit (((dest :u32))
                                                     ((src :imm)
                                                      (bit :imm))
                                                     ((temp :u32)))
  (srwi temp bit ppc32::fixnumshift)
  (subfic temp temp (- 32 ppc32::fixnumshift))
  (rlwnm dest src temp 31 31))
                                               
;;; Operations on lists and cons cells

(define-ppc32-vinsn %cdr (((dest :lisp))
                          ((src :lisp)))
  (lwz dest ppc32::cons.cdr src))

(define-ppc32-vinsn %car (((dest :lisp))
                          ((src :lisp)))
  (lwz dest ppc32::cons.car src))

(define-ppc32-vinsn %set-car (()
                              ((cell :lisp)
                               (new :lisp)))
  (stw new ppc32::cons.car cell))

(define-ppc32-vinsn %set-cdr (()
                              ((cell :lisp)
                               (new :lisp)))
  (stw new ppc32::cons.cdr cell))

(define-ppc32-vinsn load-adl (()
                              ((n :u32const)))
  (lis ppc::nargs (:apply ldb (byte 16 16) n))
  (ori ppc::nargs ppc::nargs (:apply ldb (byte 16 0) n)))
                            
(define-ppc32-vinsn set-nargs (()
                               ((n :s16const)))
  (li ppc::nargs (:apply ash n ppc32::word-shift)))

(define-ppc32-vinsn scale-nargs (()
                                 ((nfixed :s16const)))
  ((:pred > nfixed 0)
   (la ppc::nargs (:apply - (:apply ash nfixed ppc32::word-shift)) ppc::nargs)))
                           


(define-ppc32-vinsn (vpush-register :push :node :vsp)
    (()
     ((reg :lisp)))
  (stwu reg -4 ppc::vsp))

(define-ppc32-vinsn (vpush-register-arg :push :node :vsp :outgoing-argument)
    (()
     ((reg :lisp)))
  (stwu reg -4 ppc::vsp))

(define-ppc32-vinsn (vpop-register :pop :node :vsp)
    (((dest :lisp))
     ())
  (lwz dest 0 ppc::vsp)
  (la ppc::vsp 4 ppc::vsp))


(define-ppc32-vinsn copy-node-gpr (((dest :lisp))
                                   ((src :lisp)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src)))

(define-ppc32-vinsn copy-gpr (((dest t))
			      ((src t)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src)))


(define-ppc32-vinsn copy-fpr (((dest :double-float))
			      ((src :double-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fmr dest src)))

(define-ppc32-vinsn vcell-ref (((dest :lisp))
			       ((vcell :lisp)))
  (lwz dest ppc32::misc-data-offset vcell))


(define-ppc32-vinsn make-vcell (((dest :lisp))
                                ((closed (:lisp :ne dest)))
                                ((header :u32)))
  (li header ppc32::value-cell-header)
  (la ppc::allocptr (- ppc32::fulltag-misc ppc32::value-cell.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header ppc32::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stw closed ppc32::value-cell.value dest))

(define-ppc32-vinsn make-tsp-vcell (((dest :lisp))
                                    ((closed :lisp))
                                    ((header :u32)))
  (li header ppc32::value-cell-header)
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stfd ppc::fp-zero 8 ppc::tsp)
  (stw ppc::rzero 4 ppc::tsp)
  (stw header (+ 8 ppc32::fulltag-misc ppc32::value-cell.header) ppc::tsp)
  (stw closed (+ 8 ppc32::fulltag-misc ppc32::value-cell.value) ppc::tsp)
  (la dest (+ 8 ppc32::fulltag-misc) ppc::tsp))

(define-ppc32-vinsn make-tsp-cons (((dest :lisp))
                                   ((car :lisp) (cdr :lisp))
                                   ())
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stfd ppc::fp-zero 8 ppc::tsp)
  (stw ppc::rzero 4 ppc::tsp)
  (stw car (+ 8 ppc32::fulltag-cons ppc32::cons.car) ppc::tsp)
  (stw cdr (+ 8 ppc32::fulltag-cons ppc32::cons.cdr) ppc::tsp)
  (la dest (+ 8 ppc32::fulltag-cons) ppc::tsp))


(define-ppc32-vinsn %closure-code% (((dest :lisp))
                                    ())
  (lwz dest (:apply + ppc32::symbol.vcell (ppc32::nrs-offset %closure-code%) (:apply target-nil-value)) 0))


(define-ppc32-vinsn single-float-bits (((dest :u32))
                                       ((src :lisp)))
  (lwz dest ppc32::single-float.value src))

(define-ppc32-vinsn (call-subprim :call :subprim-call) (()
                                                        ((spno :s32const)))
  (bla spno))

(define-ppc32-vinsn (jump-subprim :jumpLR) (()
                                            ((spno :s32const)))
  (ba spno))

;;; Same as "call-subprim", but gives us a place to 
;;; track args, results, etc.
(define-ppc32-vinsn (call-subprim-0 :call :subprim-call) (((dest t))
                                                          ((spno :s32const)))
  (bla spno))

(define-ppc32-vinsn (call-subprim-1 :call :subprim-call) (((dest t))
                                                          ((spno :s32const)
                                                           (z t)))
  (bla spno))
  
(define-ppc32-vinsn (call-subprim-2 :call :subprim-call) (((dest t))
                                                          ((spno :s32const)
                                                           (y t)
                                                           (z t)))
  (bla spno))

(define-ppc32-vinsn (call-subprim-3 :call :subprim-call) (((dest t))
                                                          ((spno :s32const)
                                                           (x t)
                                                           (y t)
                                                           (z t)))
  (bla spno))

(define-ppc32-vinsn event-poll (()
				()
                                ((crf :crf)))
  (lwz ppc::nargs ppc32::tcr.tlb-pointer ppc32::rcontext)
  (lwz ppc::nargs ppc32::interrupt-level-binding-index ppc::nargs)
  (cmpwi crf ppc::nargs 0)
  (blt crf :done)
  (bgt crf :trap)
  (lwz ppc::nargs ppc32::tcr.interrupt-pending ppc32::rcontext)
  :trap
  (twgti ppc::nargs 0)
  :done)

(define-ppc32-vinsn ref-interrupt-level (((dest :imm))
                                         ()
                                         ((temp :u32)))
  (lwz temp ppc32::tcr.tlb-pointer ppc32::rcontext)
  (lwz dest ppc32::INTERRUPT-LEVEL-BINDING-INDEX temp))

                         
;;; Unconditional (pc-relative) branch
(define-ppc32-vinsn (jump :jump) (()
                                  ((label :label)))
  (b label))

(define-ppc32-vinsn (call-label :call) (()
                                        ((label :label)))
  (bl label))

;;; just like JUMP, only (implicitly) asserts that the following 
;;; code is somehow reachable.
(define-ppc32-vinsn (non-barrier-jump :xref) (()
                                              ((label :label)))
  (b label))


(define-ppc32-vinsn (cbranch-true :branch) (()
                                            ((label :label)
                                             (crf :crf)
                                             (crbit :u8const)))
  (bt (:apply + crf crbit) label))

(define-ppc32-vinsn (cbranch-false :branch) (()
                                             ((label :label)
                                              (crf :crf)
                                              (crbit :u8const)))
  (bf (:apply + crf crbit) label))

(define-ppc32-vinsn check-trap-error (()
                                      ())
  (beq+ 0 :no-error)
  (uuo_interr arch::error-reg-regnum ppc::arg_z)
  :no-error)


(define-ppc32-vinsn lisp-word-ref (((dest t))
                                   ((base t)
                                    (offset t)))
  (lwzx dest base offset))

(define-ppc32-vinsn lisp-word-ref-c (((dest t))
                                     ((base t)
                                      (offset :s16const)))
  (lwz dest offset base))

  

;; Load an unsigned, 32-bit constant into a destination register.
(define-ppc32-vinsn (lri :constant-ref) (((dest :imm))
                                         ((intval :u32const))
                                         ())
  ((:or (:pred = (:apply ash intval -15) #x1ffff)
        (:pred = (:apply ash intval -15) #x0))
   (li dest (:apply %word-to-int (:apply logand #xffff intval))))
  ((:not                                ; that's :else to you, bub.
    (:or (:pred = (:apply ash intval -15) #x1ffff)
         (:pred = (:apply ash intval -15) #x0)))
   ((:pred = (:apply ash intval -15) 1)
    (ori dest ppc::rzero (:apply logand intval #xffff)))
   ((:not (:pred = (:apply ash intval -15) 1))
    (lis dest (:apply ash intval -16))
    ((:not (:pred = 0 (:apply logand intval #xffff)))
     (ori dest dest (:apply logand intval #xffff))))))


(define-ppc32-vinsn (discard-temp-frame :tsp :pop :discard) (()
                                                             ())
  (lwz ppc::tsp 0 ppc::tsp))


;;; Somewhere, deep inside the "OS_X_PPC_RuntimeConventions.pdf"
;;; document, they bother to document the fact that SP should
;;; maintain 16-byte alignment on OSX.  (The example prologue
;;; code in that document incorrectly assumes 8-byte alignment.
;;; Or something.  It's wrong in a number of other ways.)
;;; The caller always has to reserve a 24-byte linkage area
;;; (large chunks of which are unused).
(define-ppc32-vinsn alloc-c-frame (()
                                   ((n-c-args :u16const)))
  ;; Always reserve space for at least 8 args and space for a lisp
  ;; frame (for the kernel) underneath it.
  ;; Zero the c-frame's savelr field, not that the GC cares ..
  ((:pred <= n-c-args 10)
   (stwu ppc::sp (- (+ 8 ppc32::c-frame.size ppc32::lisp-frame.size)) ppc::sp))
  ((:pred > n-c-args 10)
   ;; A normal C frame has room for 10 args (when padded out to
   ;; 16-byte alignment. Add enough double words to accomodate the
   ;; remaining args, in multiples of 4.
   (stwu ppc::sp (:apply - (:apply +
                                   8
                                   (+ ppc32::c-frame.size ppc32::lisp-frame.size)
                                   (:apply ash
                                           (:apply logand
                                                   (lognot 3)
                                                   (:apply
                                                    +
                                                    3
                                                    (:apply - n-c-args 10)))
                                           2)))
         ppc::sp))
  (stw ppc::rzero ppc32::c-frame.savelr ppc::sp))

(define-ppc32-vinsn alloc-variable-c-frame (()
                                            ((n-c-args :lisp))
                                            ((crf :crf)
                                             (size :s32)))
  (cmpwi crf n-c-args (ash 10 ppc32::fixnumshift))
  (subi size n-c-args (ash 10 ppc32::fixnumshift))
  (bgt :variable)
  ;; Always reserve space for at least 8 args and space for a lisp
  ;; frame (for the kernel) underneath it.
  (stwu ppc::sp (- (+ 8 ppc32::c-frame.size ppc32::lisp-frame.size)) ppc::sp)
  (b :done)
  :variable
  (addi size size (+  (+ 8 ppc32::c-frame.size ppc32::lisp-frame.size) (ash 3 ppc32::fixnumshift)))
  (clrrwi size size 3)
  (neg size size)
  (stwux ppc::sp ppc::sp size)
  :done
  (stw ppc::rzero ppc32::c-frame.savelr ppc::sp))

(define-ppc32-vinsn alloc-eabi-c-frame (()
                                        ((n-c-args :u16const)))
  ;; Always reserve space for at least 8 args and space for a lisp
  ;; frame (for the kernel) underneath it.  Store NIL inthe c-frame's
  ;; savelr field, so that the kernel doesn't mistake this for a lisp
  ;; frame.
  ((:pred <= n-c-args 8)
   (stwu ppc::sp (- (+ ppc32::eabi-c-frame.size ppc32::lisp-frame.size)) ppc::sp))
  ((:pred > n-c-args 8)
   ;; A normal C frame has room for 8 args. Add enough double words to
   ;; accomodate the remaining args
   (stwu ppc::sp (:apply - (:apply + 
                                   (+ ppc32::eabi-c-frame.size ppc32::lisp-frame.size)
                                   (:apply ash
                                           (:apply logand
                                                   (lognot 1)
                                                   (:apply
                                                    1+
                                                    (:apply - n-c-args 8)))
                                           2)))
         ppc::sp))
  (stw ppc::sp ppc32::eabi-c-frame.savelr ppc::sp))

(define-ppc32-vinsn alloc-variable-eabi-c-frame (()
                                                 ((n-c-args :lisp))
                                                 ((crf :crf)
                                                  (size :s32)))
  (cmpwi crf n-c-args (ash 8 ppc32::fixnumshift))
  (subi size n-c-args (ash 8 ppc32::fixnumshift))
  (bgt :variable)
  ;; Always reserve space for at least 8 args and space for a lisp
  ;; frame (for the kernel) underneath it.
  (stwu ppc::sp (- (+ ppc32::eabi-c-frame.size ppc32::lisp-frame.size)) ppc::sp)
  (b :done)
  :variable
  (addi size size (+  (+ ppc32::eabi-c-frame.size ppc32::lisp-frame.size) (ash 1 ppc32::fixnumshift)))
  (clrrwi size size 2)
  (neg size size)
  (stwux ppc::sp ppc::sp size)
  :done
  (stw ppc::rzero ppc32::c-frame.savelr ppc::sp))



;;; We should rarely have to do this.  It's easier to just generate code
;;; to do the memory reference than it would be to keep track of the size
;;; of each frame.
(define-ppc32-vinsn (discard-c-frame :csp :pop :discard) (()
                                                          ())
  (lwz ppc::sp 0 ppc::sp))




(define-ppc32-vinsn set-c-arg (()
                               ((argval :u32)
                                (argnum :u16const)))
  (stw argval (:apply + ppc32::c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn set-single-c-arg (()
                                      ((argval :single-float)
                                       (argnum :u16const)))
  (stfs argval (:apply + ppc32::c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn set-double-c-arg (()
                                      ((argval :double-float)
                                       (argnum :u16const)))
  (stfd argval (:apply + ppc32::c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn reload-single-c-arg (((argval :single-float))
                                         ((argnum :u16const)))
  (lfs argval (:apply + ppc32::c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn reload-double-c-arg (((argval :double-float))
                                         ((argnum :u16const)))
  (lfd argval (:apply + ppc32::c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn set-eabi-c-arg (()
                                    ((argval :u32)
                                     (argnum :u16const)))
  (stw argval (:apply + ppc32::eabi-c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn set-single-eabi-c-arg (()
                                           ((argval :single-float)
                                            (argnum :u16const)))
  (stfs argval (:apply + ppc32::eabi-c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn set-double-eabi-c-arg (()
                                           ((argval :double-float)
                                            (argnum :u16const)))
  (stfd argval (:apply + ppc32::eabi-c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn reload-single-eabi-c-arg (((argval :single-float))
                                              ((argnum :u16const)))
  (lfs argval (:apply + ppc32::eabi-c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn reload-double-eabi-c-arg (((argval :double-float))
                                              ((argnum :u16const)))
  (lfd argval (:apply + ppc32::eabi-c-frame.param0 (:apply ash argnum ppc32::word-shift)) ppc::sp))

(define-ppc32-vinsn (load-nil :constant-ref) (((dest t))
                                              ())
  (li dest (:apply target-nil-value)))

(define-ppc32-vinsn (load-t :constant-ref) (((dest t))
                                            ())
  (li dest (:apply + ppc32::t-offset (:apply target-nil-value))))

(define-ppc32-vinsn set-eq-bit (((dest :crf))
                                ())
  (creqv (:apply + ppc::ppc-eq-bit dest)
	 (:apply + ppc::ppc-eq-bit dest)
	 (:apply + ppc::ppc-eq-bit dest)))

(define-ppc32-vinsn (ref-constant :constant-ref) (((dest :lisp))
                                                  ((src :s16const)))
  (lwz dest (:apply + ppc32::misc-data-offset (:apply ash (:apply 1+ src) 2)) ppc::fn))

(define-ppc32-vinsn ref-indexed-constant (((dest :lisp))
                                          ((idxreg :s32)))
  (lwzx dest ppc::fn idxreg))


(define-ppc32-vinsn cons (((dest :lisp))
                          ((newcar :lisp)
                           (newcdr :lisp)))
  (la ppc::allocptr (- ppc32::fulltag-cons ppc32::cons.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw newcdr ppc32::cons.cdr ppc::allocptr)
  (stw newcar ppc32::cons.car ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits))



;; subtag had better be a PPC-NODE-SUBTAG of some sort!
(define-ppc32-vinsn %ppc-gvector (((dest :lisp))
                                  ((Rheader :u32) 
                                   (nbytes :u32const))
                                  ((immtemp0 :u32)
                                   (nodetemp :lisp)
                                   (crf :crf)))
  (la ppc::allocptr (:apply - ppc32::fulltag-misc
                            (:apply logand (lognot 7)
                                    (:apply + (+ 7 4) nbytes)))
      ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw Rheader ppc32::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  ((:not (:pred = nbytes 0))
   (li immtemp0 (:apply + ppc32::misc-data-offset nbytes))
   :loop
   (subi immtemp0 immtemp0 4)
   (cmpwi crf immtemp0 ppc32::misc-data-offset)
   (lwz nodetemp 0 ppc::vsp)
   (la ppc::vsp 4 ppc::vsp)   
   (stwx nodetemp dest immtemp0)
   (bne crf :loop)))

;; allocate a small (phys size <= 32K bytes) misc obj of known size/subtag
(define-ppc32-vinsn %alloc-misc-fixed (((dest :lisp))
                                       ((Rheader :u32)
                                        (nbytes :u32const)))
  (la ppc::allocptr (:apply - ppc32::fulltag-misc
                            (:apply logand (lognot 7)
                                    (:apply + (+ 7 4) nbytes)))
      ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw Rheader ppc32::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits))

(define-ppc32-vinsn (vstack-discard :vsp :pop :discard) (()
                                                         ((nwords :u32const)))
  ((:not (:pred = nwords 0))
   (la ppc::vsp (:apply ash nwords ppc32::word-shift) ppc::vsp)))


(define-ppc32-vinsn lcell-load (((dest :lisp))
                                ((cell :lcell)
                                 (top :lcell)))
  (lwz dest (:apply - 
                    (:apply - (:apply calc-lcell-depth top) 4)
                    (:apply calc-lcell-offset cell)) ppc::vsp))

(define-ppc32-vinsn vframe-load (((dest :lisp))
                                 ((frame-offset :u16const)
                                  (cur-vsp :u16const)))
  (lwz dest (:apply - (:apply - cur-vsp 4) frame-offset) ppc::vsp))

(define-ppc32-vinsn lcell-store (()
                                 ((src :lisp)
                                  (cell :lcell)
                                  (top :lcell)))
  (stw src (:apply - 
                   (:apply - (:apply calc-lcell-depth top) 4)
                   (:apply calc-lcell-offset cell)) ppc::vsp))

(define-ppc32-vinsn vframe-store (()
                                  ((src :lisp)
                                   (frame-offset :u16const)
                                   (cur-vsp :u16const)))
  (stw src (:apply - (:apply - cur-vsp 4) frame-offset) ppc::vsp))

(define-ppc32-vinsn load-vframe-address (((dest :imm))
                                         ((offset :s16const)))
  (la dest offset ppc::vsp))

(define-ppc32-vinsn copy-lexpr-argument (()
                                         ()
                                         ((temp :lisp)))
  (lwzx temp ppc::vsp ppc::nargs)
  (stwu temp -4 ppc::vsp))

;;; Boxing/unboxing of integers.

;;; Treat the low 8 bits of VAL as an unsigned integer; set RESULT to the equivalent fixnum.
(define-ppc32-vinsn u8->fixnum (((result :imm)) 
                                ((val :u8)) 
                                ())
  (rlwinm result val ppc32::fixnumshift (- ppc32::nbits-in-word (+ 8 ppc32::fixnumshift)) (- ppc32::least-significant-bit ppc32::fixnumshift)))

;;; Treat the low 8 bits of VAL as a signed integer; set RESULT to the equivalent fixnum.
(define-ppc32-vinsn s8->fixnum (((result :imm)) 
                                ((val :s8)) 
                                ())
  (extlwi result val 8 (- ppc32::nbits-in-word 8))
  (srawi result result (- (- ppc32::nbits-in-word 8) ppc32::fixnumshift)))


;;; Treat the low 16 bits of VAL as an unsigned integer; set RESULT to the equivalent fixnum.
(define-ppc32-vinsn u16->fixnum (((result :imm)) 
                                 ((val :u16)) 
                                 ())
  (rlwinm result val ppc32::fixnumshift (- ppc32::nbits-in-word (+ 16 ppc32::fixnumshift)) (- ppc32::least-significant-bit ppc32::fixnumshift)))

;;; Treat the low 16 bits of VAL as a signed integer; set RESULT to the equivalent fixnum.
(define-ppc32-vinsn s16->fixnum (((result :imm)) 
                                 ((val :s16)) 
                                 ())
  (extlwi result val 16 (- ppc32::nbits-in-word 16))
  (srawi result result (- (- ppc32::nbits-in-word 16) ppc32::fixnumshift)))

(define-ppc32-vinsn fixnum->s16 (((result :s16))
                                 ((src :imm)))
  (srawi result src ppc32::fixnumshift))

;;; A signed 32-bit untagged value can be at worst a 1-digit bignum.
;;; There should be something very much like this that takes a stack-consed
;;; bignum result ...
(define-ppc32-vinsn s32->integer (((result :lisp))
                                  ((src :s32))
                                  ((crf (:crf 0)) ; a casualty
                                   (temp :s32)))        
  (addo temp src src)
  (addo. result temp temp)
  (bns+ :done)
  (mtxer ppc::rzero)
  (li temp ppc32::one-digit-bignum-header)
  (la ppc::allocptr (- ppc32::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw temp ppc32::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stw src ppc32::misc-data-offset result)
  :done)


;;; An unsigned 32-bit untagged value can be either a 1 or a 2-digit bignum.
(define-ppc32-vinsn u32->integer (((result :lisp))
                                  ((src :u32))
                                  ((crf (:crf 0)) ; a casualty
                                   (temp :s32)
                                   (size :u32)))
  (clrrwi. temp src (- ppc32::least-significant-bit ppc32::nfixnumtagbits))
  (slwi result src ppc32::fixnumshift)
  (beq+ crf :done)
  (cmpwi src 0)
  (li temp ppc32::one-digit-bignum-header)
  (li size (- 8 ppc32::fulltag-misc))
  (bgt :common)
  (li temp ppc32::two-digit-bignum-header)
  (li size (- 16 ppc32::fulltag-misc))
  :common
  (sub ppc::allocptr ppc::allocptr size)
  (twllt ppc::allocptr ppc::allocbase)
  (stw temp ppc32::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stw src ppc32::misc-data-offset result)
  :done)

(define-ppc32-vinsn u16->u32 (((dest :u32))
                              ((src :u16)))
  (clrlwi dest src 16))

(define-ppc32-vinsn u8->u32 (((dest :u32))
                             ((src :u8)))
  (clrlwi dest src 24))


(define-ppc32-vinsn s16->s32 (((dest :s32))
                              ((src :s16)))
  (extsh dest src))

(define-ppc32-vinsn s8->s32 (((dest :s32))
                             ((src :s8)))
  (extsb dest src))


;;; ... of floats ...

;;; Heap-cons a double-float to store contents of FPREG.  Hope that we don't do
;;; this blindly.
(define-ppc32-vinsn double->heap (((result :lisp)) ; tagged as a double-float
                                  ((fpreg :double-float)) 
                                  ((header-temp :u32)))
  (li header-temp (arch::make-vheader ppc32::double-float.element-count ppc32::subtag-double-float))
  (la ppc::allocptr (- ppc32::fulltag-misc ppc32::double-float.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header-temp ppc32::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stfd fpreg ppc32::double-float.value result)  )


;;; This is about as bad as heap-consing a double-float.  (In terms of
;;; verbosity).  Wouldn't kill us to do either/both out-of-line, but
;;; need to make visible to compiler so unnecessary heap-consing can
;;; be elided.
(define-ppc32-vinsn single->node (((result :lisp)) ; tagged as a single-float
				  ((fpreg :single-float))
				  ((header-temp :u32)))
  (li header-temp (arch::make-vheader ppc32::single-float.element-count ppc32::subtag-single-float))
  (la ppc::allocptr (- ppc32::fulltag-misc ppc32::single-float.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header-temp ppc32::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stfs fpreg ppc32::single-float.value result))


;;; "dest" is preallocated, presumably on a stack somewhere.
(define-ppc32-vinsn store-double (()
                                  ((dest :lisp)
                                   (source :double-float))
                                  ())
  (stfd source ppc32::double-float.value dest))

(define-ppc32-vinsn get-double (((target :double-float))
                                ((source :lisp))
                                ())
  (lfd target ppc32::double-float.value source))

;;; Extract a double-float value, typechecking in the process.
;;; IWBNI we could simply call the "trap-unless-typecode=" vinsn here,
;;; instead of replicating it ..

(define-ppc32-vinsn get-double? (((target :double-float))
                                 ((source :lisp))
                                 ((tag :u8)
                                  (crf :crf)))
  (clrlwi tag source (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :do-trap)
  (lbz tag ppc32::misc-subtag-offset source)
  :do-trap
  (twnei tag ppc32::subtag-double-float)
  (lfd target ppc32::double-float.value source))
  

(define-ppc32-vinsn double-to-single (((result :single-float))
                                       ((arg :double-float)))
  (frsp result arg))

(define-ppc32-vinsn store-single (()
                                  ((dest :lisp)
                                   (source :single-float))
                                  ())
  (stfs source ppc32::single-float.value dest))

(define-ppc32-vinsn get-single (((target :single-float))
                                ((source :lisp))
                                ())
  (lfs target ppc32::single-float.value source))

;;; ... of characters ...


(define-ppc32-vinsn character->fixnum (((dest :lisp))
                                       ((src :lisp))
                                       ())
  (rlwinm dest
          src
          (- ppc32::nbits-in-word (- ppc32::charcode-shift ppc32::fixnumshift))
          (- ppc32::nbits-in-word (+ ppc32::ncharcodebits ppc32::fixnumshift)) 
          (- ppc32::least-significant-bit ppc32::fixnumshift)))

(define-ppc32-vinsn character->code (((dest :u32))
                                     ((src :lisp)))
  (srwi dest src ppc32::charcode-shift))


(define-ppc32-vinsn fixnum->char (((dest :lisp))
                                  ((src :imm))
                                  ((temp :u32)
                                   (crf0 (:crf 0))))
  (srwi temp src (+ ppc32::fixnumshift 1))
  (cmplwi temp (ash #xffff -1))
  (srwi temp src (+ ppc32::fixnumshift 11))
  (beq :bad)
  (cmpwi temp 27)
  (slwi dest src (- ppc32::charcode-shift ppc32::fixnumshift))
  (bne+ :ok)
  :bad
  (li dest (:apply target-nil-value))
  (b :done)
  :ok
  (addi dest dest ppc32::subtag-character)
  :done)

;;; src is known to be a code for which CODE-CHAR returns non-nil.
(define-ppc32-vinsn code-char->char (((dest :lisp))
				     ((src :imm))
				     ())
  (slwi dest src (- ppc32::charcode-shift ppc32::fixnum-shift))
  (addi dest dest ppc32::subtag-character))

(define-ppc32-vinsn u32->char (((dest :lisp))
                              ((src :u32))
                              ())
  (slwi dest src ppc32::charcode-shift)
  (addi dest dest ppc32::subtag-character))

;; ... Macptrs ...

(define-ppc32-vinsn deref-macptr (((addr :address))
                                  ((src :lisp))
                                  ())
  (lwz addr ppc32::macptr.address src))

(define-ppc32-vinsn set-macptr-address (()
                                        ((addr :address)
                                         (src :lisp))
                                        ())
  (stw addr ppc32::macptr.address src))


(define-ppc32-vinsn macptr->heap (((dest :lisp))
                                  ((address :address))
                                  ((header :u32)))
  (li header (logior (ash ppc32::macptr.element-count ppc32::num-subtag-bits) ppc32::subtag-macptr))
  (la ppc::allocptr (- ppc32::fulltag-misc ppc32::macptr.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header ppc32::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  ;; It's not necessary to zero out the domain/type fields, since newly
  ;; heap-allocated memory's guaranteed to be 0-filled.
  (stw address ppc32::macptr.address dest))

(define-ppc32-vinsn macptr->stack (((dest :lisp))
                                   ((address :address))
                                   ((header :u32)))
  (li header ppc32::macptr-header)
  (stwu ppc::tsp (- (+ 8 ppc32::macptr.size)) ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stw header (+ 8 ppc32::fulltag-misc ppc32::macptr.header) ppc::tsp)
  (stw address (+ 8 ppc32::fulltag-misc ppc32::macptr.address) ppc::tsp)
  ;; It -is- necessary to zero out the domain/type fields here, since
  ;; stack-allocated memory isn't guaranteed to be 0-filled.
  (stfd ppc::fp-zero (+ 8 ppc32::fulltag-misc ppc32::macptr.domain) ppc::tsp)
  (la dest (+ 8 ppc32::fulltag-misc) ppc::tsp))

  
(define-ppc32-vinsn adjust-stack-register (()
                                           ((reg t)
                                            (amount :s16const)))
  (la reg amount reg))

(define-ppc32-vinsn adjust-vsp (()
                                ((amount :s16const)))
  (la ppc::vsp amount ppc::vsp))

(define-ppc32-vinsn adjust-sp (()
                               ((amount :s16const)))
  (la ppc::sp amount ppc::sp))

;; Arithmetic on fixnums & unboxed numbers

(define-ppc32-vinsn u32-lognot (((dest :u32))
                                ((src :u32))
                                ())
  (not dest src))

(define-ppc32-vinsn fixnum-lognot (((dest :imm))
                                   ((src :imm))
                                   ((temp :u32)))
  (not temp src)
  (clrrwi dest temp ppc32::nfixnumtagbits))


(define-ppc32-vinsn negate-fixnum-overflow-inline (((dest :lisp))
                                                   ((src :imm))
                                                   ((unboxed :s32)
                                                    (header :u32)))
  (nego. dest src)
  (bns+ :done)
  (mtxer ppc::rzero)
  (srawi unboxed dest ppc32::fixnumshift)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc32::fixnumshift))))
  (li header ppc32::one-digit-bignum-header)
  (la ppc::allocptr (- ppc32::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header ppc32::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stw unboxed ppc32::misc-data-offset dest)
  :done)

(define-ppc32-vinsn negate-fixnum-overflow-ool (()
                                                ((src :imm))
                                                )
  (nego. ppc::arg_z src)
  (bsola- .SPfix-overflow)
  :done)
  
                                                  
                                       
(define-ppc32-vinsn negate-fixnum-no-ovf (((dest :lisp))
                                          ((src :imm)))
  
  (neg dest src))
  

(define-ppc32-vinsn logior-high (((dest :imm))
                                 ((src :imm)
                                  (high :u16const)))
  (oris dest src high))

(define-ppc32-vinsn logior-low (((dest :imm))
                                ((src :imm)
                                 (low :u16const)))
  (ori dest src low))

                           
                           
(define-ppc32-vinsn %logior2 (((dest :imm))
                              ((x :imm)
                               (y :imm))
                              ())
  (or dest x y))

(define-ppc32-vinsn logand-high (((dest :imm))
                                 ((src :imm)
                                  (high :u16const))
                                 ((crf0 (:crf 0))))
  (andis. dest src high))

(define-ppc32-vinsn logand-low (((dest :imm))
                                ((src :imm)
                                 (low :u16const))
                                ((crf0 (:crf 0))))
  (andi. dest src low))


(define-ppc32-vinsn %logand2 (((dest :imm))
                              ((x :imm)
                               (y :imm))
                              ())
  (and dest x y))

(define-ppc32-vinsn clear-left (((dest :imm))
                                ((src :imm)
                                 (nbits :s8const)))
  (rlwinm dest src 0 (:apply 1+ nbits) 31))

(define-ppc32-vinsn clear-right (((dest :imm))
                                 ((src :imm)
                                  (nbits :s8const)))
  (rlwinm dest src 0 0 (:apply - 31 nbits)))

                               
(define-ppc32-vinsn logxor-high (((dest :imm))
                                 ((src :imm)
                                  (high :u16const)))
  (xoris dest src high))

(define-ppc32-vinsn logxor-low (((dest :imm))
                                ((src :imm)
                                 (low :u16const)))
  (xori dest src low))

                           

(define-ppc32-vinsn %logxor2 (((dest :imm))
                              ((x :imm)
                               (y :imm))
                              ())
  (xor dest x y))

(define-ppc32-vinsn %ilsl (((dest :imm))
                           ((count :imm)
                            (src :imm))
                           ((temp :u32)
                            (crx :crf)))
  (cmpwi crx count (ash 31 ppc32::fixnumshift))
  (srwi temp count ppc32::fixnumshift)
  (slw dest src temp)
  (ble+ crx :foo)
  (li dest 0)
  :foo)

(define-ppc32-vinsn %ilsl-c (((dest :imm))
                             ((count :u8const)
                              (src :imm)))
                                        ; Hard to use ppcmacroinstructions that expand into expressions involving variables.
  (rlwinm dest src count 0 (:apply - ppc32::least-significant-bit count)))


(define-ppc32-vinsn %ilsr-c (((dest :imm))
                             ((count :u8const)
                              (src :imm)))
  (rlwinm dest src (:apply - ppc32::nbits-in-word count) count (- ppc32::least-significant-bit
                                                                  ppc32::fixnumshift)))



;;; 68k did the right thing for counts < 64 - fixnumshift but not if greater
;;; so load-byte fails in 3.0 also


(define-ppc32-vinsn %iasr (((dest :imm))
                           ((count :imm)
                            (src :imm))
                           ((temp :s32)
                            (crx :crf)))
  (cmpwi crx count (ash 31 ppc32::fixnumshift))
  (srawi temp count ppc32::fixnumshift)
  (sraw temp src temp)
  (ble+ crx :foo)
  (srawi temp src 31)
  :foo
  (clrrwi dest temp ppc32::fixnumshift))

(define-ppc32-vinsn %iasr-c (((dest :imm))
                             ((count :u8const)
                              (src :imm))
                             ((temp :s32)))
  (srawi temp src count)
  (clrrwi dest temp ppc32::fixnumshift))

(define-ppc32-vinsn %ilsr (((dest :imm))
                           ((count :imm)
                            (src :imm))
                           ((temp :s32)
                            (crx :crf)))
  (cmpwi crx count (ash 31 ppc32::fixnumshift))
  (srwi temp count ppc32::fixnumshift)
  (srw temp src temp)
  (clrrwi dest temp ppc32::fixnumshift)
  (ble+ crx :foo)
  (li dest 0)
  :foo  
  )

#+maybe
(define-ppc32-vinsn %ilsr-c (((dest :imm))
                             ((count :u8const)
                              (src :imm))
                             ((temp :s32)))
  (rlwinm temp src (:apply - 32 count) count 31)
  (clrrwi dest temp ppc32::fixnumshift))

(define-ppc32-vinsn natural-shift-left (((dest :u32))
                                        ((src :u32)
                                         (count :u8const)))
  (rlwinm dest src count 0 (:apply - 31 count)))

(define-ppc32-vinsn natural-shift-right (((dest :u32))
                                         ((src :u32)
                                          (count :u8const)))
  (rlwinm dest src (:apply - 32 count) count 31))


(define-ppc32-vinsn trap-unless-simple-array-2 (()
                                                ((object :lisp)
                                                 (expected-flags :u32const)
                                                 (type-error :u8const))
                                                ((tag :u8)
                                                 (flags :u32)
                                                 (crf :crf)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :bad)
  (lbz tag ppc32::misc-subtag-offset object)
  (cmpwi crf tag ppc32::subtag-arrayH)
  (bne crf :bad) 
  (lwz tag ppc32::arrayH.rank object)
  (cmpwi crf tag (ash 2 ppc32::fixnumshift))
  (lis tag (:apply ldb (byte 16 16) (:apply ash expected-flags ppc32::fixnumshift)))
       
  (lwz flags ppc32::arrayH.flags object)
  (ori tag tag (:apply ldb (byte 16 0) (:apply ash expected-flags ppc32::fixnumshift)))
  (bne crf :bad)
  (cmpw crf tag flags)
  (beq crf :good)
  :bad
  (uuo_interr type-error object)
  :good)

(define-ppc32-vinsn trap-unless-simple-array-3 (()
                                                ((object :lisp)
                                                 (expected-flags :u32const)
                                                 (type-error :u8const))
                                                ((tag :u8)
                                                 (flags :u32)
                                                 (crf :crf)))
  (clrlwi tag object (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne crf :bad)
  (lbz tag ppc32::misc-subtag-offset object)
  (cmpwi crf tag ppc32::subtag-arrayH)
  (bne crf :bad) 
  (lwz tag ppc32::arrayH.rank object)
  (cmpwi crf tag (ash 3 ppc32::fixnumshift))
  (lis tag (:apply ldb (byte 16 16) (:apply ash expected-flags ppc32::fixnumshift)))
       
  (lwz flags ppc32::arrayH.flags object)
  (ori tag tag (:apply ldb (byte 16 0) (:apply ash expected-flags ppc32::fixnumshift)))
  (bne crf :bad)
  (cmpw crf tag flags)
  (beq crf :good)
  :bad
  (uuo_interr type-error object)
  :good)
  
  
  
  
(define-ppc32-vinsn sign-extend-halfword (((dest :imm))
                                          ((src :imm)))
  (slwi dest src (- 16 ppc32::fixnumshift))
  (srawi dest dest (- 16 ppc32::fixnumshift)))

(define-ppc32-vinsn s32-highword (((dest :imm))
                                  ((src :s32))
                                  ((temp :s32)))
  (srawi temp src 16)
  (slwi dest temp ppc32::fixnumshift))

                            

(define-ppc32-vinsn fixnum-add (((dest t))
                                ((x t)
                                 (y t)))
  (add dest x y))


(define-ppc32-vinsn fixnum-add-overflow-ool (()
                                             ((x :imm)
                                              (y :imm))
                                             ((cr0 (:crf 0))))
  (addo. ppc::arg_z x y)
  (bsola- .SPfix-overflow))

(define-ppc32-vinsn fixnum-add-overflow-inline (((dest :lisp))
                                                ((x :imm)
                                                 (y :imm))
                                                ((cr0 (:crf 0))
                                                 (unboxed :s32)
                                                 (header :u32)))
  (addo. dest x y)
  (bns+ cr0 :done)
  (mtxer ppc::rzero)
  (srawi unboxed dest ppc32::fixnumshift)
  (li header ppc32::one-digit-bignum-header)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc32::fixnumshift))))
  (la ppc::allocptr (- ppc32::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header ppc32::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stw unboxed ppc32::misc-data-offset dest)
  :done)

(define-ppc32-vinsn fixnum-add-overflow-inline-skip (((dest :lisp))
                                                ((x :imm)
                                                 (y :imm)
                                                 (target :label))
                                                ((cr0 (:crf 0))
                                                 (unboxed :s32)
                                                 (header :u32)))
  (addo. dest x y)
  (bns+ cr0 target)
  (mtxer ppc::rzero)
  (srawi unboxed dest ppc32::fixnumshift)
  (li header ppc32::one-digit-bignum-header)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc32::fixnumshift))))
  (la ppc::allocptr (- ppc32::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header ppc32::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stw unboxed ppc32::misc-data-offset dest)
  (b target))
  

  

;;;  (setq dest (- x y))
(define-ppc32-vinsn fixnum-sub (((dest t))
                                ((x t)
                                 (y t)))
  (subf dest y x))

(define-ppc32-vinsn fixnum-sub-from-constant (((dest :imm))
                                              ((x :s16const)
                                               (y :imm)))
  (subfic dest y (:apply ash x ppc32::fixnumshift)))




(define-ppc32-vinsn fixnum-sub-overflow-ool (()
                                             ((x :imm)
                                              (y :imm)))
  (subo. ppc::arg_z x y)
  (bsola- .SPfix-overflow))

(define-ppc32-vinsn fixnum-sub-overflow-inline (((dest :lisp))
                                                ((x :imm)
                                                 (y :imm))
                                                ((cr0 (:crf 0))
                                                 (unboxed :s32)
                                                 (header :u32)))
  (subo. dest x y)
  (bns+ cr0 :done)
  (mtxer ppc::rzero)
  (srawi unboxed dest ppc32::fixnumshift)
  (li header ppc32::one-digit-bignum-header)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc32::fixnumshift))))
  (la ppc::allocptr (- ppc32::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header ppc32::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stw unboxed ppc32::misc-data-offset dest)
  :done)

(define-ppc32-vinsn fixnum-sub-overflow-inline-skip (((dest :lisp))
                                                     ((x :imm)
                                                      (y :imm)
                                                      (target :label))
                                                     ((cr0 (:crf 0))
                                                      (unboxed :s32)
                                                      (header :u32)))
  (subo. dest x y)
  (bns+ cr0 target)
  (mtxer ppc::rzero)
  (srawi unboxed dest ppc32::fixnumshift)
  (li header ppc32::one-digit-bignum-header)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc32::fixnumshift))))
  (la ppc::allocptr (- ppc32::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header ppc32::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr ppc32::ntagbits)
  (stw unboxed ppc32::misc-data-offset dest)
  (b target))

;;; This is, of course, also "subtract-immediate."
(define-ppc32-vinsn add-immediate (((dest t))
                                   ((src t)
                                    (upper :u32const)
                                    (lower :u32const)))
  ((:not (:pred = upper 0))
   (addis dest src upper)
   ((:not (:pred = lower 0))
    (addi dest dest lower)))
  ((:and (:pred = upper 0) (:not (:pred = lower 0)))
   (addi dest src lower)))

;This must unbox one reg, but hard to tell which is better.
;(The one with the smaller absolute value might be)
(define-ppc32-vinsn multiply-fixnums (((dest :imm))
                                      ((a :imm)
                                       (b :imm))
                                      ((unboxed :s32)))
  (srawi unboxed b ppc32::fixnumshift)
  (mullw dest a unboxed))

(define-ppc32-vinsn multiply-immediate (((dest :imm))
                                        ((boxed :imm)
                                         (const :s16const)))
  (mulli dest boxed const))

;;; Mask out the code field of a base character; the result
;;; should be EXACTLY = to subtag-base-char
(define-ppc32-vinsn mask-base-char (((dest :u32))
                                    ((src :imm)))
  (clrlwi dest src (- ppc32::nbits-in-word ppc32::charcode-shift)))

;;; Set dest (of type :s32!) to 0 iff VAL is an istruct of type TYPE
(define-ppc32-vinsn istruct-typep (((dest :s32))
                                   ((val :lisp)
                                    (type :lisp))
                                   ((crf :crf)
                                    (temp :lisp)))
  (clrlwi dest val (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf dest ppc32::tag-misc)
  (li dest -1)
  (bne crf :done)
  (lbz dest ppc32::misc-subtag-offset val)
  (cmpwi crf dest ppc32::subtag-istruct)
  (bne crf :done)
  (lwz temp ppc32::misc-data-offset val)
  (subf dest type temp)
  :done)
  
  
;; Boundp, fboundp stuff.
(define-ppc32-vinsn (ref-symbol-value :call :subprim-call)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (bla .SPspecrefcheck))

(define-ppc32-vinsn ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((table :imm)
                                               (idx :imm)))
  (lwz idx ppc32::symbol.binding-index src)
  (lwz table ppc32::tcr.tlb-limit ppc32::rcontext)
  (cmpw idx table)
  (lwz table ppc32::tcr.tlb-pointer ppc32::rcontext)
  (bge :symbol)
  (lwzx dest table idx)
  (cmpwi dest ppc32::subtag-no-thread-local-binding)
  (bne :done)
  :symbol
  (lwz dest ppc32::symbol.vcell src)
  :done
  (tweqi dest ppc32::unbound-marker))

(define-ppc32-vinsn (%ref-symbol-value :call :subprim-call)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (bla .SPspecref))

(define-ppc32-vinsn %ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((table :imm)
                                               (idx :imm)))
  (lwz idx ppc32::symbol.binding-index src)
  (lwz table ppc32::tcr.tlb-limit ppc32::rcontext)
  (cmpw idx table)
  (lwz table ppc32::tcr.tlb-pointer ppc32::rcontext)
  (bge :symbol)
  (lwzx dest table idx)
  (cmpwi dest ppc32::subtag-no-thread-local-binding)
  (bne :done)
  :symbol
  (lwz dest ppc32::symbol.vcell src)
  :done
  )

(define-ppc32-vinsn (setq-special :call :subprim-call)
    (()
     ((sym :lisp)
      (val :lisp)))
  (bla .SPspecset))


(define-ppc32-vinsn symbol-function (((val :lisp))
                                     ((sym (:lisp (:ne val))))
                                     ((crf :crf)
                                      (tag :u32)))
  (lwz val ppc32::symbol.fcell sym)
  (clrlwi tag val (- 32 ppc32::nlisptagbits))
  (cmpwi crf tag ppc32::tag-misc)
  (bne- crf :bad)
  (lbz tag ppc32::misc-subtag-offset val)
  (cmpwi crf tag ppc32::subtag-function)
  (beq+ crf :good)
  :bad 
  (uuo_interr arch::error-udf sym)
  :good)

(define-ppc32-vinsn (temp-push-unboxed-word :push :word :tsp)
    (()
     ((w :u32)))
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stw w 8 ppc::tsp))

(define-ppc32-vinsn (temp-pop-unboxed-word :pop :word :tsp)
    (((w :u32))
     ())
  (lwz w 8 ppc::tsp)
  (la ppc::tsp 16 ppc::tsp))

(define-ppc32-vinsn (temp-push-double-float :push :doubleword :tsp)
    (((d :double-float))
     ())
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stfd d 8 ppc::tsp))

(define-ppc32-vinsn (temp-pop-double-float :pop :doubleword :tsp)
    (()
     ((d :double-float)))
  (lfd d 8 ppc::tsp)
  (la ppc::tsp 16 ppc::tsp))

(define-ppc32-vinsn (temp-push-single-float :push :word :tsp)
    (((s :single-float))
     ())
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stfs s 8 ppc::tsp))

(define-ppc32-vinsn (temp-pop-single-float :pop :word :tsp)
    (()
     ((s :single-float)))
  (lfs s 8 ppc::tsp)
  (la ppc::tsp 16 ppc::tsp))


(define-ppc32-vinsn (save-nvrs-individually :push :node :vsp :multiple)
    (()
     ((first :u8const)))
  (stwu ppc::save0 -4 ppc::vsp)  
  ((:pred <= first ppc::save1)
   (stwu ppc::save1 -4 ppc::vsp)
   ((:pred <= first ppc::save2)
    (stwu ppc::save2 -4 ppc::vsp)
    ((:pred <= first ppc::save3)
     (stwu ppc::save3 -4 ppc::vsp)
     ((:pred <= first ppc::save4)
      (stwu ppc::save4 -4 ppc::vsp)
      ((:pred <= first ppc::save5)
       (stwu ppc::save5 -4 ppc::vsp)
       ((:pred <= first ppc::save6)
        (stwu ppc::save6 -4 ppc::vsp)
        ((:pred = first ppc::save7)
         (stwu ppc::save7 -4 ppc::vsp)))))))))

(define-ppc32-vinsn (save-nvrs :push :node :vsp :multiple)
    (()
     ((first :u8const)))
  ((:pred <= first ppc::save3)
   (subi ppc::vsp ppc::vsp (:apply * 4 (:apply - 32 first)))   
   (stmw first 0 ppc::vsp))
  ((:pred >= first ppc::save2)
   (stwu ppc::save0 -4 ppc::vsp)
   ((:pred <= first ppc::save1)
    (stwu ppc::save1 -4 ppc::vsp)
    ((:pred = first ppc::save2)
     (stwu ppc::save2 -4 ppc::vsp)))))


(define-ppc32-vinsn (restore-nvrs :pop :node :vsp :multiple)
    (()
     ((firstreg :u8const)
      (basereg :imm)
      (offset :s16const)))
  ((:pred <= firstreg ppc::save3)
   (lmw firstreg offset basereg))
  ((:pred = firstreg ppc::save2)
   (lwz ppc::save2 offset basereg)
   (lwz ppc::save1 (:apply + offset 4) basereg)
   (lwz ppc::save0 (:apply + offset 8) basereg))
  ((:pred = firstreg ppc::save1)
   (lwz ppc::save1 offset basereg)
   (lwz ppc::save0 (:apply + offset 4) basereg))
  ((:pred = firstreg ppc::save0)
   (lwz ppc::save0 offset basereg)))

(define-ppc32-vinsn %current-frame-ptr (((dest :imm))
                                        ())
  (mr dest ppc::sp))

(define-ppc32-vinsn %current-tcr (((dest :imm))
                                  ())
  (mr dest ppc32::rcontext))

(define-ppc32-vinsn (dpayback :call :subprim-call) (()
                                                    ((n :s16const))
                                                    ((temp (:u32 #.ppc::imm0))))
  ((:pred > n 1)
   (li temp n)
   (bla .SPunbind-n))
  ((:pred = n 1)
   (bla .SPunbind)))

(define-ppc32-vinsn zero-double-float-register 
    (((dest :double-float))
     ())
  (fmr dest ppc::fp-zero))

(define-ppc32-vinsn zero-single-float-register 
    (((dest :single-float))
     ())
  (fmr dest ppc::fp-zero))

(define-ppc32-vinsn load-double-float-constant
    (((dest :double-float))
     ((high t)
      (low t)))
  (stw high -8 ppc::sp)
  (stw low -4 ppc::sp)
  (lfd dest -8 ppc::sp ))

(define-ppc32-vinsn load-single-float-constant
    (((dest :single-float))
     ((src t)))
  (stw src -4 ppc::sp)
  (lfs dest -4 ppc::sp))

(define-ppc32-vinsn load-indexed-node (((node :lisp))
                                       ((base :lisp)
                                        (offset :s16const)))
  (lwz node offset base))

(define-ppc32-vinsn check-exact-nargs (()
                                       ((n :u16const)))
  (twnei ppc::nargs (:apply ash n 2)))

(define-ppc32-vinsn check-min-nargs (()
                                     ((min :u16const)))
  (twllti ppc::nargs (:apply ash min 2)))

(define-ppc32-vinsn check-max-nargs (()
                                     ((max :u16const)))
  (twlgti ppc::nargs (:apply ash max 2)))

;;; Save context and establish FN.  The current VSP is the the
;;; same as the caller's, e.g., no arguments were vpushed.
(define-ppc32-vinsn save-lisp-context-vsp (()
                                           ()
                                           ((imm :u32)))
  (lwz imm ppc32::tcr.cs-limit ppc32::rcontext)
  (stwu ppc::sp (- ppc32::lisp-frame.size) ppc::sp)
  (stw ppc::fn ppc32::lisp-frame.savefn ppc::sp)
  (stw ppc::loc-pc ppc32::lisp-frame.savelr ppc::sp)
  (stw ppc::vsp ppc32::lisp-frame.savevsp ppc::sp)
  (mr ppc::fn ppc::nfn)
  ;; Do a stack-probe ...
  (twllt ppc::sp imm))

;;; Do the same thing via a subprim call.
(define-ppc32-vinsn (save-lisp-context-vsp-ool :call :subprim-call)
    (()
     ()
     ((imm (:u32 #.ppc::imm0))))
  (bla .SPsavecontextvsp))

(define-ppc32-vinsn save-lisp-context-offset (()
                                              ((nbytes-vpushed :u16const))
                                              ((imm :u32)))
  (la imm nbytes-vpushed ppc::vsp)
  (stwu ppc::sp (- ppc32::lisp-frame.size) ppc::sp)
  (stw ppc::fn ppc32::lisp-frame.savefn ppc::sp)
  (stw ppc::loc-pc ppc32::lisp-frame.savelr ppc::sp)
  (stw imm ppc32::lisp-frame.savevsp ppc::sp)
  (mr ppc::fn ppc::nfn)
  ;; Do a stack-probe ...
  (lwz imm ppc32::tcr.cs-limit ppc32::rcontext)
  (twllt ppc::sp imm))

(define-ppc32-vinsn save-lisp-context-offset-ool (()
                                                  ((nbytes-vpushed :u16const))
                                                  ((imm (:u32 #.ppc::imm0))))
  (li imm nbytes-vpushed)
  (bla .SPsavecontext0))


(define-ppc32-vinsn save-lisp-context-lexpr (()
                                             ()
                                             ((imm :u32)))
  (stwu ppc::sp (- ppc32::lisp-frame.size) ppc::sp)
  (stw ppc::rzero ppc32::lisp-frame.savefn ppc::sp)
  (stw ppc::loc-pc ppc32::lisp-frame.savelr ppc::sp)
  (stw ppc::vsp ppc32::lisp-frame.savevsp ppc::sp)
  (mr ppc::fn ppc::nfn)
  ;; Do a stack-probe ...
  (lwz imm ppc32::tcr.cs-limit ppc32::rcontext)
  (twllt ppc::sp imm))
  
(define-ppc32-vinsn save-cleanup-context (()
                                          ())
  ;; SP was this deep just a second ago, so no need to do a stack-probe.
  (mflr ppc::loc-pc)
  (stwu ppc::sp (- ppc32::lisp-frame.size) ppc::sp)
  (stw ppc::rzero ppc32::lisp-frame.savefn ppc::sp)
  (stw ppc::loc-pc ppc32::lisp-frame.savelr ppc::sp)
  (stw ppc::vsp ppc32::lisp-frame.savevsp ppc::sp))

;; Vpush the argument registers.  We got at least "min-fixed" args;
;; that knowledge may help us generate better code.
(define-ppc32-vinsn (save-lexpr-argregs :call :subprim-call)
    (()
     ((min-fixed :u16const))
     ((crfx :crf)
      (crfy :crf)
      (entry-vsp (:u32 #.ppc::imm0))
      (arg-temp :u32)))
  ((:pred >= min-fixed $numppcargregs)
   (stwu ppc::arg_x -4 ppc::vsp)   
   (stwu ppc::arg_y -4 ppc::vsp)   
   (stwu ppc::arg_z -4 ppc::vsp))
  ((:pred = min-fixed 2)                ; at least 2 args
   (cmplwi crfx ppc::nargs (ash 2 ppc32::word-shift))
   (beq crfx :yz2)                      ; skip arg_x if exactly 2
   (stwu ppc::arg_x -4 ppc::vsp)
   :yz2
   (stwu ppc::arg_y -4 ppc::vsp)
   (stwu ppc::arg_z -4 ppc::vsp))
  ((:pred = min-fixed 1)                ; at least one arg
   (cmplwi crfx ppc::nargs (ash 2 ppc32::word-shift))
   (blt crfx :z1)                       ; branch if exactly one
   (beq crfx :yz1)                      ; branch if exactly two
   (stwu ppc::arg_x -4 ppc::vsp)
   :yz1
   (stwu ppc::arg_y -4 ppc::vsp)   
   :z1
   (stwu ppc::arg_z -4 ppc::vsp))
  ((:pred = min-fixed 0)
   (cmplwi crfx ppc::nargs (ash 2 ppc32::word-shift))
   (cmplwi crfy ppc::nargs 0)
   (beq crfx :yz0)                      ; exactly two
   (beq crfy :none)                     ; exactly zero
   (blt crfx :z0)                       ; one
                                        ; Three or more ...
   (stwu ppc::arg_x -4 ppc::vsp)
   :yz0
   (stwu ppc::arg_y -4 ppc::vsp)
   :z0
   (stwu ppc::arg_z -4 ppc::vsp)
   :none
   )
  ((:pred = min-fixed 0)
   (stwu ppc::nargs -4 ppc::vsp))
  ((:not (:pred = min-fixed 0))
   (subi arg-temp ppc::nargs (:apply ash min-fixed ppc32::word-shift))
   (stwu arg-temp -4 ppc::vsp))
  (add entry-vsp ppc::vsp ppc::nargs)
  (la entry-vsp 4 entry-vsp)
  (bla .SPlexpr-entry))


(define-ppc32-vinsn (jump-return-pc :jumpLR)
    (()
     ())
  (blr))

(define-ppc32-vinsn (restore-full-lisp-context :lispcontext :pop :csp :lrRestore)
    (()
     ())
  (lwz ppc::loc-pc ppc32::lisp-frame.savelr ppc::sp)
  (lwz ppc::vsp ppc32::lisp-frame.savevsp ppc::sp)  
  (lwz ppc::fn ppc32::lisp-frame.savefn ppc::sp)
  (mtlr ppc::loc-pc)
  (la ppc::sp ppc32::lisp-frame.size ppc::sp))



(define-ppc32-vinsn (restore-full-lisp-context-ool :lispcontext :pop :csp :lrRestore)
    (()
     ())
  (bla .SPrestorecontext)
  (mtlr ppc::loc-pc))

(define-ppc32-vinsn (popj :lispcontext :pop :csp :lrRestore :jumpLR)
    (() 
     ())
  (ba .SPpopj))

;;; Exiting from an UNWIND-PROTECT cleanup is similar to
;;; (and a little simpler than) returning from a function.
(define-ppc32-vinsn restore-cleanup-context (()
                                             ())
  (lwz ppc::loc-pc ppc32::lisp-frame.savelr ppc::sp)
  (mtlr ppc::loc-pc)
  (la ppc::sp ppc32::lisp-frame.size ppc::sp))



(define-ppc32-vinsn default-1-arg (()
                                   ((min :u16const))
                                   ((crf :crf)))
  (cmplwi crf ppc::nargs (:apply ash min 2))
  (bne crf :done)
  ((:pred >= min 3)
   (stwu ppc::arg_x -4 ppc::vsp))
  ((:pred >= min 2)
   (mr ppc::arg_x ppc::arg_y))
  ((:pred >= min 1)
   (mr ppc::arg_y ppc::arg_z))
  (li ppc::arg_z (:apply target-nil-value))
  :done)

(define-ppc32-vinsn default-2-args (()
                                    ((min :u16const))
                                    ((crf :crf)))
  (cmplwi crf ppc::nargs (:apply ash (:apply 1+ min) 2))
  (bgt crf :done)
  (beq crf :one)
                                        ; We got "min" args; arg_y & arg_z default to nil
  ((:pred >= min 3)
   (stwu ppc::arg_x -4 ppc::vsp))   
  ((:pred >= min 2)
   (stwu ppc::arg_y -4 ppc::vsp))
  ((:pred >= min 1)
   (mr ppc::arg_x ppc::arg_z))
  (li ppc::arg_y (:apply target-nil-value))
  (b :last)
  :one
                                        ; We got min+1 args: arg_y was supplied, arg_z defaults to nil.
  ((:pred >= min 2)
   (stwu ppc::arg_x -4 ppc::vsp))
  ((:pred >= min 1)
   (mr ppc::arg_x ppc::arg_y))
  (mr ppc::arg_y ppc::arg_z)
  :last
  (li ppc::arg_z (:apply target-nil-value))
  :done)

(define-ppc32-vinsn default-3-args (()
                                    ((min :u16const))
                                    ((crfx :crf)
                                     (crfy :crf)))
  (cmplwi crfx ppc::nargs (:apply ash (:apply + 2 min) 2))
  (cmplwi crfy ppc::nargs (:apply ash min 2))
  (bgt crfx :done)
  (beq crfx :two)
  (beq crfy :none)
                                        ; The first (of three) &optional args was supplied.
  ((:pred >= min 2)
   (stwu ppc::arg_x -4 ppc::vsp))
  ((:pred >= min 1)
   (stwu ppc::arg_y -4 ppc::vsp))
  (mr ppc::arg_x ppc::arg_z)
  (b :last-2)
  :two
                                        ; The first two (of three) &optional args were supplied.
  ((:pred >= min 1)
   (stwu ppc::arg_x -4 ppc::vsp))
  (mr ppc::arg_x ppc::arg_y)
  (mr ppc::arg_y ppc::arg_z)
  (b :last-1)
                                        ; None of the three &optional args was provided.
  :none
  ((:pred >= min 3)
   (stwu ppc::arg_x -4 ppc::vsp))
  ((:pred >= min 2)
   (stwu ppc::arg_y -4 ppc::vsp))
  ((:pred >= min 1)
   (stwu ppc::arg_z -4 ppc::vsp))
  (li ppc::arg_x (:apply target-nil-value))
  :last-2
  (li ppc::arg_y (:apply target-nil-value))
  :last-1
  (li ppc::arg_z (:apply target-nil-value))
  :done)

(define-ppc32-vinsn save-lr (()
                             ())
  (mflr ppc::loc-pc))

;;; "n" is the sum of the number of required args + 
;;; the number of &optionals.  
(define-ppc32-vinsn (default-optionals :call :subprim-call) (()
                                                             ((n :u16const)))
  (li ppc::imm0 (:apply ash n 2))
  (bla .SPdefault-optional-args))

;;; fname contains a known symbol
(define-ppc32-vinsn (call-known-symbol :call) (((result (:lisp ppc::arg_z)))
                                               ())
  (lwz ppc::nfn ppc32::symbol.fcell ppc::fname)
  (lwz ppc::temp0 ppc32::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctrl))

(define-ppc32-vinsn (jump-known-symbol :jumplr) (()
                                                 ())
  (lwz ppc::nfn ppc32::symbol.fcell ppc::fname)
  (lwz ppc::temp0 ppc32::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctr))

(define-ppc32-vinsn (call-known-function :call) (()
                                                 ())
  (lwz ppc::temp0 ppc32::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctrl))

(define-ppc32-vinsn (jump-known-function :jumplr) (()
                                                   ())
  (lwz ppc::temp0 ppc32::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctr))

(define-ppc32-vinsn %schar8 (((char :imm))
                            ((str :lisp)
                             (idx :imm))
                            ((imm :u32)))
  (srwi imm idx ppc32::fixnumshift)
  (addi imm imm ppc32::misc-data-offset)
  (lbzx imm str imm)
  (slwi imm imm ppc32::charcode-shift)
  (addi char imm ppc32::subtag-character))

(define-ppc32-vinsn %schar32 (((char :imm))
                              ((str :lisp)
                               (idx :imm))
                              ((imm :u32)))
  (addi imm idx ppc32::misc-data-offset)
  (lwzx imm str imm)
  (slwi imm imm ppc32::charcode-shift)
  (addi char imm ppc32::subtag-character))


(define-ppc32-vinsn %set-schar8 (()
                                ((str :lisp)
                                 (idx :imm)
                                 (char :imm))
                                ((imm :u32)
                                 (imm1 :u32)
                                 (cr0 (:crf 0))))
  (srwi imm idx ppc32::fixnumshift)
  (addi imm imm ppc32::misc-data-offset)
  (srwi imm1 char ppc32::charcode-shift)
  (stbx imm1 str imm)
  )

(define-ppc32-vinsn %set-schar32 (()
                                  ((str :lisp)
                                   (idx :imm)
                                   (char :imm))
                                  ((imm :u32)
                                   (imm1 :u32)
                                   (cr0 (:crf 0))))
  (addi imm idx ppc32::misc-data-offset)
  (srwi imm1 char ppc32::charcode-shift)
  (stwx imm1 str imm)
  )

(define-ppc32-vinsn %set-scharcode8 (()
                                    ((str :lisp)
                                     (idx :imm)
                                     (code :imm))
                                    ((imm :u32)
                                     (imm1 :u32)
                                     (cr0 (:crf 0))))
  (srwi imm idx ppc32::fixnumshift)
  (addi imm imm ppc32::misc-data-offset)
  (srwi imm1 code ppc32::fixnumshift)
  (stbx imm1 str imm)
  )


(define-ppc32-vinsn %set-scharcode32 (()
                                    ((str :lisp)
                                     (idx :imm)
                                     (code :imm))
                                    ((imm :u32)
                                     (imm1 :u32)))
  (addi imm idx ppc32::misc-data-offset)
  (srwi imm1 code ppc32::fixnumshift)
  (stwx imm1 str imm)
  )

(define-ppc32-vinsn %scharcode8 (((code :imm))
                                 ((str :lisp)
                                  (idx :imm))
                                 ((imm :u32)
                                  (cr0 (:crf 0))))
  (srwi imm idx ppc32::fixnumshift)
  (addi imm imm ppc32::misc-data-offset)
  (lbzx imm str imm)
  (slwi code imm ppc32::fixnumshift))

(define-ppc32-vinsn %scharcode32 (((code :imm))
                                 ((str :lisp)
                                  (idx :imm))
                                 ((imm :u32)
                                  (cr0 (:crf 0))))
  (addi imm idx ppc32::misc-data-offset)
  (lwzx imm str imm)
  (slwi code imm ppc32::fixnumshift))

;;; Clobbers LR
(define-ppc32-vinsn (%debug-trap :call :subprim-call) (()
                                                       ())
  (bla .SPbreakpoint)
  )


(define-ppc32-vinsn eep.address (((dest t))
                                 ((src (:lisp (:ne dest )))))
  (lwz dest (+ (ash 1 2) ppc32::misc-data-offset) src)
  (tweqi dest (:apply target-nil-value)))
                 
(define-ppc32-vinsn %natural+ (((dest :u32))
                               ((x :u32) (y :u32)))
  (add dest x y))

(define-ppc32-vinsn %natural+-c (((dest :u32))
                                 ((x :u32) (y :u16const)))
  (addi dest x y))

(define-ppc32-vinsn %natural- (((dest :u32))
                               ((x :u32) (y :u32)))
  (sub dest x y))

(define-ppc32-vinsn %natural--c (((dest :u32))
                                 ((x :u32) (y :u16const)))
  (subi dest x y))

(define-ppc32-vinsn %natural-logior (((dest :u32))
                                     ((x :u32) (y :u32)))
  (or dest x y))

(define-ppc32-vinsn %natural-logior-c (((dest :u32))
                                       ((x :u32) (high :u16const) (low :u16const)))
  ((:not (:pred = high 0))
   (oris dest x high))
  ((:not (:pred = low 0))
   (ori dest x low)))

(define-ppc32-vinsn %natural-logxor (((dest :u32))
                                     ((x :u32) (y :u32)))
  (xor dest x y))

(define-ppc32-vinsn %natural-logxor-c (((dest :u32))
                                       ((x :u32) (high :u16const) (low :u16const)))
  ((:not (:pred = high 0))
   (xoris dest x high))
  ((:not (:pred = low 0))
   (xori dest x low)))

(define-ppc32-vinsn %natural-logand (((dest :u32))
                                     ((x :u32) (y :u32)))
  (and dest x y))

(define-ppc32-vinsn %natural-logand-high-c (((dest :u32))
                                            ((x :u32) (high :u16const))
                                            ((cr0 (:crf 0))))
  (andis. dest x high))

(define-ppc32-vinsn %natural-logand-low-c (((dest :u64))
                                           ((x :u64) (low :u16const))
                                           ((cr0 (:crf 0))))
  (andi. dest x low))

(define-ppc32-vinsn %natural-logand-mask-c (((dest :u32))
                                            ((x :u32)
                                             (start :u8const)
                                             (end :u8const)))
  (rlwinm dest x 0 start end))

(define-ppc32-vinsn disable-interrupts (((dest :lisp))
                                        ()
                                        ((temp :imm)
                                         (temp2 :imm)))
  (lwz temp2 ppc32::tcr.tlb-pointer ppc32::rcontext)
  (li temp -4)
  (lwz dest ppc32::interrupt-level-binding-index temp2)
  (stw temp ppc32::interrupt-level-binding-index temp2))

(define-ppc32-vinsn load-character-constant (((dest :lisp))
                                             ((code :u32const)))
  (ori dest ppc::rzero (:apply logior (:apply ash (:apply logand #xff code) ppc32::charcode-shift) ppc32::subtag-character))
  ((:not (:pred = 0 (:apply ldb (byte 16 8) code)))
   (oris dest dest (:apply ldb (byte 16 8) code))))


(define-ppc32-vinsn %symbol->symptr (((dest :lisp))
                                     ((src :lisp))
                                     ((tag :u8)
                                      (crf0 :crf)
                                      (crf1 :crf)))
  (clrlwi tag src (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (cmpwi crf0 src (:apply target-nil-value))
  (cmpwi crf1 tag ppc32::tag-misc)
  (beq crf0 :nilsym)
  (bne crf1 :do-trap)
  (lbz tag ppc32::misc-subtag-offset src)
  :do-trap
  (twnei tag ppc32::subtag-symbol)
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src))
  (b :done)
  :nilsym
  (li dest (:apply + ppc32::nilsym-offset (:apply target-nil-value)))
  :done)

;;; Subprim calls.  Done this way for the benefit of VINSN-OPTIMIZE.
(defmacro define-ppc32-subprim-call-vinsn ((name &rest other-attrs) spno)
  `(define-ppc32-vinsn (,name :call :subprim-call ,@other-attrs) (() ())
    (bla ,spno)))

(defmacro define-ppc32-subprim-jump-vinsn ((name &rest other-attrs) spno)
  `(define-ppc32-vinsn (,name  :jumpLR ,@other-attrs) (() ())
    (ba ,spno)))

(define-ppc32-subprim-jump-vinsn (restore-interrupt-level) .SPrestoreintlevel)

(define-ppc32-subprim-call-vinsn (save-values) .SPsave-values)

(define-ppc32-subprim-call-vinsn (recover-values)  .SPrecover-values)

(define-ppc32-subprim-call-vinsn (add-values) .SPadd-values)

(define-ppc32-subprim-jump-vinsn (jump-known-symbol-ool) .SPjmpsym)

(define-ppc32-subprim-call-vinsn (call-known-symbol-ool)  .SPjmpsym)

(define-ppc32-subprim-call-vinsn (pass-multiple-values)  .SPmvpass)

(define-ppc32-subprim-call-vinsn (pass-multiple-values-symbol) .SPmvpasssym)

(define-ppc32-subprim-jump-vinsn (tail-call-sym-gen) .SPtcallsymgen)

(define-ppc32-subprim-jump-vinsn (tail-call-fn-gen) .SPtcallnfngen)

(define-ppc32-subprim-jump-vinsn (tail-call-sym-slide) .SPtcallsymslide)

(define-ppc32-subprim-jump-vinsn (tail-call-fn-slide) .SPtcallnfnslide)

(define-ppc32-subprim-jump-vinsn (tail-call-sym-vsp) .SPtcallsymvsp)

(define-ppc32-subprim-jump-vinsn (tail-call-fn-vsp) .SPtcallnfnvsp)

(define-ppc32-subprim-call-vinsn (funcall)  .SPfuncall)

(define-ppc32-subprim-jump-vinsn (tail-funcall-gen) .SPtfuncallgen)

(define-ppc32-subprim-jump-vinsn (tail-funcall-slide) .SPtfuncallslide)

(define-ppc32-subprim-jump-vinsn (tail-funcall-vsp) .SPtfuncallvsp)

(define-ppc32-subprim-call-vinsn (spread-lexpr)  .SPspread-lexpr-z)

(define-ppc32-subprim-call-vinsn (spread-list)  .SPspreadargz)

(define-ppc32-subprim-call-vinsn (pop-argument-registers)  .SPvpopargregs)

(define-ppc32-subprim-call-vinsn (getu32) .SPgetu32)

(define-ppc32-subprim-call-vinsn (gets32) .SPgets32)

(define-ppc32-subprim-call-vinsn (getxlong)  .SPgetXlong)

(define-ppc32-subprim-call-vinsn (stack-cons-list)  .SPstkconslist)

(define-ppc32-subprim-call-vinsn (list) .SPconslist)

(define-ppc32-subprim-call-vinsn (stack-cons-list*)  .SPstkconslist-star)

(define-ppc32-subprim-call-vinsn (list*) .SPconslist-star)

(define-ppc32-subprim-call-vinsn (make-stack-block)  .SPmakestackblock)

(define-ppc32-subprim-call-vinsn (make-stack-block0)  .Spmakestackblock0)

(define-ppc32-subprim-call-vinsn (make-stack-list)  .Spmakestacklist)

(define-ppc32-subprim-call-vinsn (make-stack-vector)  .SPmkstackv)

(define-ppc32-subprim-call-vinsn (make-stack-gvector)  .SPstkgvector)

(define-ppc32-subprim-call-vinsn (stack-misc-alloc)  .SPstack-misc-alloc)

(define-ppc32-subprim-call-vinsn (stack-misc-alloc-init)  .SPstack-misc-alloc-init)

(define-ppc32-subprim-call-vinsn (bind-nil)  .SPbind-nil)

(define-ppc32-subprim-call-vinsn (bind-self)  .SPbind-self)

(define-ppc32-subprim-call-vinsn (bind-self-boundp-check)  .SPbind-self-boundp-check)

(define-ppc32-subprim-call-vinsn (bind)  .SPbind)

(define-ppc32-subprim-jump-vinsn (nvalret :jumpLR) .SPnvalret)

(define-ppc32-subprim-call-vinsn (nthrowvalues) .SPnthrowvalues)

(define-ppc32-subprim-call-vinsn (nthrow1value) .SPnthrow1value)

(define-ppc32-subprim-call-vinsn (slide-values) .SPmvslide)

(define-ppc32-subprim-call-vinsn (macro-bind) .SPmacro-bind)

(define-ppc32-subprim-call-vinsn (destructuring-bind-inner) .SPdestructuring-bind-inner)

(define-ppc32-subprim-call-vinsn (destructuring-bind) .SPdestructuring-bind)

(define-ppc32-subprim-call-vinsn (simple-keywords) .SPsimple-keywords)

(define-ppc32-subprim-call-vinsn (keyword-args) .SPkeyword-args)

(define-ppc32-subprim-call-vinsn (keyword-bind) .SPkeyword-bind)

(define-ppc32-subprim-call-vinsn (stack-rest-arg) .SPstack-rest-arg)

(define-ppc32-subprim-call-vinsn (req-stack-rest-arg) .SPreq-stack-rest-arg)

(define-ppc32-subprim-call-vinsn (stack-cons-rest-arg) .SPstack-cons-rest-arg)

(define-ppc32-subprim-call-vinsn (heap-rest-arg) .SPheap-rest-arg)

(define-ppc32-subprim-call-vinsn (req-heap-rest-arg) .SPreq-heap-rest-arg)

(define-ppc32-subprim-call-vinsn (heap-cons-rest-arg) .SPheap-cons-rest-arg)

(define-ppc32-subprim-call-vinsn (opt-supplied-p) .SPopt-supplied-p)

(define-ppc32-subprim-call-vinsn (gvector) .SPgvector)

(define-ppc32-vinsn (nth-value :call :subprim-call) (((result :lisp))
                                                     ())
  (bla .SPnthvalue))

(define-ppc32-subprim-call-vinsn (fitvals) .SPfitvals)

(define-ppc32-subprim-call-vinsn (misc-alloc) .SPmisc-alloc)

(define-ppc32-subprim-call-vinsn (misc-alloc-init) .SPmisc-alloc-init)

(define-ppc32-subprim-call-vinsn (integer-sign) .SPinteger-sign)

;;; Even though it's implemented by calling a subprim, THROW is really
;;; a JUMP (to a possibly unknown destination).  If the destination's
;;; really known, it should probably be inlined (stack-cleanup, value
;;; transfer & jump ...)
(define-ppc32-vinsn (throw :jump-unknown) (()
                                                 ())
  (bla .SPthrow))

(define-ppc32-subprim-call-vinsn (mkcatchmv) .SPmkcatchmv)

(define-ppc32-subprim-call-vinsn (mkcatch1v) .SPmkcatch1v)

(define-ppc32-subprim-call-vinsn (setqsym) .SPsetqsym)

(define-ppc32-subprim-call-vinsn (ksignalerr) .SPksignalerr)

(define-ppc32-subprim-call-vinsn (subtag-misc-ref) .SPsubtag-misc-ref)

(define-ppc32-subprim-call-vinsn (subtag-misc-set) .SPsubtag-misc-set)

(define-ppc32-subprim-call-vinsn (mkunwind) .SPmkunwind)
(define-ppc32-subprim-call-vinsn (nmkunwind) .SPnmkunwind)


(define-ppc32-subprim-call-vinsn (progvsave) .SPprogvsave)

(define-ppc32-subprim-jump-vinsn (progvrestore) .SPprogvrestore)

(define-ppc32-subprim-call-vinsn (eabi-syscall) .SPeabi-syscall)

(define-ppc32-subprim-call-vinsn (misc-ref) .SPmisc-ref)

(define-ppc32-subprim-call-vinsn (misc-set) .SPmisc-set)

(define-ppc32-subprim-call-vinsn (gets64) .SPgets64)

(define-ppc32-subprim-call-vinsn (getu64) .SPgetu64)

(define-ppc32-subprim-call-vinsn (makeu64) .SPmakeu64)

(define-ppc32-subprim-call-vinsn (makes64) .SPmakes64)

(define-ppc32-vinsn (poweropen-syscall :call :subprim-call) (()
                                                          ())
  (stw ppc::rzero ppc32::c-frame.crsave ppc::sp)
  (bla .SPpoweropen-syscall))

(define-ppc32-vinsn (poweropen-syscall-s64 :call :subprim-call) (()
                                                              ())
  (stw ppc::sp ppc32::c-frame.crsave ppc::sp)
  (bla .SPpoweropen-syscall))

(define-ppc32-subprim-call-vinsn (eabi-ff-call) .SPeabi-ff-call)

(define-ppc32-subprim-call-vinsn (poweropen-ff-call) .SPpoweropen-ffcall)

(define-ppc32-subprim-call-vinsn (poweropen-ff-callX) .SPpoweropen-ffcallX)

(define-ppc32-subprim-call-vinsn (bind-interrupt-level-0) .SPbind-interrupt-level-0)

(define-ppc32-vinsn bind-interrupt-level-0-inline (()
                                                   ()
                                                   ((tlb :imm)
                                                    (value :imm)
                                                    (link :imm)
                                                    (temp :imm)))
  (lwz tlb ppc32::tcr.tlb-pointer ppc32::rcontext)
  (lwz value ppc32::interrupt-level-binding-index tlb)
  (lwz link ppc32::tcr.db-link ppc32::rcontext)
  (cmpwi value 0)
  (li temp ppc32::interrupt-level-binding-index)
  (stwu value -4 ppc::vsp)
  (stwu temp -4 ppc::vsp)
  (stwu link -4 ppc::vsp)
  (stw ppc::rzero ppc32::interrupt-level-binding-index tlb)
  (stw ppc::vsp  ppc32::tcr.db-link ppc32::rcontext)
  (beq+ :done)
  (mr ppc::nargs value)
  (bgt :do-trap)
  (lwz ppc::nargs ppc32::tcr.interrupt-pending ppc32::rcontext)
  :do-trap
  (twgti ppc::nargs 0)
  :done)
                                                    
  
                                                   
(define-ppc32-subprim-call-vinsn (bind-interrupt-level-m1) .SPbind-interrupt-level-m1)

(define-ppc32-vinsn bind-interrupt-level-m1-inline (()
                                                   ()
                                                   ((tlb :imm)
                                                    (oldvalue :imm)
                                                    (link :imm)
                                                    (newvalue :imm)
                                                    (idx :imm)))
  (li newvalue (ash -1 ppc32::fixnumshift))
  (li idx ppc32::interrupt-level-binding-index)
  (lwz tlb ppc32::tcr.tlb-pointer ppc32::rcontext)
  (lwz oldvalue ppc32::interrupt-level-binding-index tlb)
  (lwz link ppc32::tcr.db-link ppc32::rcontext)
  (stwu oldvalue -4 ppc::vsp)
  (stwu idx -4 ppc::vsp)
  (stwu link -4 ppc::vsp)
  (stw newvalue ppc32::interrupt-level-binding-index tlb)
  (stw ppc::vsp  ppc32::tcr.db-link ppc32::rcontext)
  :done)

(define-ppc32-subprim-call-vinsn (bind-interrupt-level) .SPbind-interrupt-level)

(define-ppc32-subprim-call-vinsn (unbind-interrupt-level) .SPunbind-interrupt-level)

(define-ppc32-vinsn unbind-interrupt-level-inline (()
                                                   ()
                                                   ((tlb :imm)
                                                    (link :imm)
                                                    (value :imm)
                                                    (save-nargs :u32)
                                                    (crf0 :crf)
                                                    (crf1 :crf)))
  (lwz tlb ppc32::tcr.tlb-pointer ppc32::rcontext)
  (lwz value ppc32::interrupt-level-binding-index tlb)
  (lwz link ppc32::tcr.db-link ppc32::rcontext)
  (cmpwi crf1 value 0)
  (lwz value 8 link)
  (lwz link 0 link)
  (cmpwi crf0 value 0)
  (stw value ppc32::interrupt-level-binding-index tlb)
  (stw link ppc32::tcr.db-link ppc32::rcontext)
  (bge crf1 :done)
  (blt crf0 :done)
  (mr save-nargs ppc::nargs)
  (lwz ppc::nargs ppc32::tcr.interrupt-pending ppc32::rcontext)
  (twgti ppc::nargs 0)
  (mr ppc::nargs save-nargs)
  :done)
  


(define-ppc32-vinsn branch-unless-arg-fixnum (()
                                              ((arg :lisp)
                                               (lab :label))
                                              ((cr0 (:crf 0))
                                               (tag :u8)))
  (clrlwi. tag arg (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (bne cr0 lab))

(define-ppc32-vinsn branch-unless-both-args-fixnums (()
                                              ((arg0 :lisp)
                                               (arg1 :lisp)
                                               (lab :label))
                                              ((cr0 (:crf 0))
                                               (tag :u8)))
  (clrlwi tag arg0 (- ppc32::nbits-in-word ppc32::nlisptagbits))
  (rlwimi. tag arg1 ppc32::nlisptagbits 28 29)
  (bne cr0 lab))

;;; In case ppc32::*ppc-opcodes* was changed since this file was compiled.
(queue-fixup
 (fixup-vinsn-templates *ppc32-vinsn-templates* ppc::*ppc-opcode-numbers*))

(provide "PPC32-VINSNS")

