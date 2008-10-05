;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2004-2005, Clozure Associates
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "PPC64-BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "PPCENV"))

(defmacro define-ppc64-vinsn (vinsn-name (results args &optional temps) &body body)
  (%define-vinsn *ppc64-backend* vinsn-name results args temps body))


;;; Index "scaling" and constant-offset misc-ref vinsns.


(define-ppc64-vinsn scale-node-misc-index (((dest :u64))
                                           ((idx :imm)	; A fixnum
                                            )
                                           ())
  (addi dest idx ppc64::misc-data-offset))

(define-ppc64-vinsn scale-32bit-misc-index (((dest :u64))
					    ((idx :imm)	; A fixnum
					     )
					    ())
  (srdi dest idx 1)
  (addi dest dest ppc64::misc-data-offset))

(define-ppc64-vinsn scale-16bit-misc-index (((dest :u64))
					    ((idx :imm)	; A fixnum
					     )
					    ())
  (srdi dest idx 2)
  (addi dest dest ppc64::misc-data-offset))

(define-ppc64-vinsn scale-8bit-misc-index (((dest :u64))
					   ((idx :imm) ; A fixnum
					    )
					   ())
  (srdi dest idx ppc64::word-shift)
  (addi dest dest ppc64::misc-data-offset))


(define-ppc64-vinsn scale-64bit-misc-index (((dest :u64))
					    ((idx :imm) ; A fixnum
					     )
					    ())
  (addi dest idx ppc64::misc-data-offset))

(define-ppc64-vinsn scale-1bit-misc-index (((word-index :s64)
					    (bitnum :u8)) ; (unsigned-byte 5)
					   ((idx :imm) ; A fixnum
					    )
					   )
  (srdi word-index idx  (+ 5 ppc64::fixnum-shift))
  (sldi word-index word-index 2)
  (addi word-index word-index ppc64::misc-data-offset) ; Hmmm. Also one instruction, but less impressive somehow.
  (extrwi bitnum idx 5 (- 32 (+ ppc64::fixnum-shift 5))))



(define-ppc64-vinsn misc-ref-u64  (((dest :u64))
				   ((v :lisp)
				    (scaled-idx :u64))
				   ())
  (ldx dest v scaled-idx))

(define-ppc64-vinsn misc-ref-s64  (((dest :s64))
				   ((v :lisp)
				    (scaled-idx :u64))
				   ())
  (ldx dest v scaled-idx))

(define-ppc64-vinsn misc-ref-c-u64  (((dest :u64))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (ld dest (:apply + ppc64::misc-data-offset (:apply ash idx ppc64::word-shift)) v))

(define-ppc64-vinsn misc-ref-c-s64  (((dest :s64))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (ld dest (:apply + ppc64::misc-data-offset (:apply ash idx ppc64::word-shift)) v))

  
(define-ppc64-vinsn misc-set-u64 (()
                                  ((val :u64)
                                   (v :lisp)
                                   (scaled-idx :u64)))
  (stdx val v scaled-idx))

(define-ppc64-vinsn misc-set-c-u64 (()
				    ((val :u64)
				     (v :lisp)
				     (idx :u32const)))
  (std val (:apply + ppc64::misc-data-offset (:apply ash idx 3)) v))

(define-ppc64-vinsn misc-set-s64 (()
                                  ((val :s64)
                                   (v :lisp)
                                   (scaled-idx :u64)))
  (stdx val v scaled-idx))


(define-ppc64-vinsn misc-set-c-s64 (()
				    ((val :s64)
				     (v :lisp)
				     (idx :u32const)))
  (std val (:apply + ppc64::misc-data-offset (:apply ash idx 3)) v))

(define-ppc64-vinsn misc-ref-u32  (((dest :u32))
				   ((v :lisp)
				    (scaled-idx :u64))
				   ())
  (lwzx dest v scaled-idx))

(define-ppc64-vinsn misc-ref-c-u32  (((dest :u32))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (lwz dest (:apply + ppc64::misc-data-offset (:apply ash idx 2)) v))

(define-ppc64-vinsn misc-ref-s32  (((dest :s32))
				   ((v :lisp)
				    (scaled-idx :u64))
				   ())
  (lwax dest v scaled-idx))

(define-ppc64-vinsn misc-ref-c-s32  (((dest :s32))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (lwa dest (:apply + ppc64::misc-data-offset (:apply ash idx 2)) v))


(define-ppc64-vinsn misc-set-c-u32 (()
				    ((val :u32)
				     (v :lisp)
				     (idx :u32const)))
  (stw val (:apply + ppc64::misc-data-offset (:apply ash idx 2)) v))

(define-ppc64-vinsn misc-set-u32 (()
				  ((val :u32)
				   (v :lisp)
				   (scaled-idx :u64)))
  (stwx val v scaled-idx))

(define-ppc64-vinsn misc-set-c-s32 (()
				    ((val :s32)
				     (v :lisp)
				     (idx :u32const)))
  (stw val (:apply + ppc64::misc-data-offset (:apply ash idx 2)) v))

(define-ppc64-vinsn misc-set-s32 (()
				  ((val :s32)
				   (v :lisp)
				   (scaled-idx :u64)))
  (stwx val v scaled-idx))
                              
(define-ppc64-vinsn misc-ref-single-float  (((dest :single-float))
					    ((v :lisp)
					     (scaled-idx :u64))
					    ())
  (lfsx dest v scaled-idx))

(define-ppc64-vinsn misc-ref-c-single-float  (((dest :single-float))
					      ((v :lisp)
					       (idx :u32const))
					      ())
  (lfs dest (:apply + ppc64::misc-data-offset (:apply ash idx 2)) v))

(define-ppc64-vinsn misc-ref-double-float  (((dest :double-float))
					    ((v :lisp)
					     (scaled-idx :u32))
					    ())
  (lfdx dest v scaled-idx))


(define-ppc64-vinsn misc-ref-c-double-float  (((dest :double-float))
					      ((v :lisp)
					       (idx :u32const))
					      ())
  (lfd dest (:apply + ppc64::misc-dfloat-offset (:apply ash idx 3)) v))

(define-ppc64-vinsn misc-set-c-double-float (((val :double-float))
					     ((v :lisp)
					      (idx :u32const)))
  (stfd val (:apply + ppc64::misc-dfloat-offset (:apply ash idx 3)) v))

(define-ppc64-vinsn misc-set-double-float (()
					   ((val :double-float)
					    (v :lisp)
					    (scaled-idx :u32)))
  (stfdx val v scaled-idx))

(define-ppc64-vinsn misc-set-c-single-float (((val :single-float))
					     ((v :lisp)
					      (idx :u32const)))
  (stfs val (:apply + ppc64::misc-data-offset (:apply ash idx 2)) v))

(define-ppc64-vinsn misc-set-single-float (()
					   ((val :single-float)
					    (v :lisp)
					    (scaled-idx :u32)))
  (stfsx val v scaled-idx))


(define-ppc64-vinsn misc-ref-u16  (((dest :u16))
				   ((v :lisp)
				    (scaled-idx :u64))
				   ())
  (lhzx dest v scaled-idx))

(define-ppc64-vinsn misc-ref-c-u16  (((dest :u16))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (lhz dest (:apply + ppc64::misc-data-offset (:apply ash idx 1)) v))

(define-ppc64-vinsn misc-set-c-u16  (((val :u16))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (sth val (:apply + ppc64::misc-data-offset (:apply ash idx 1)) v))

(define-ppc64-vinsn misc-set-u16 (((val :u16))
				  ((v :lisp)
				   (scaled-idx :s64)))
  (sthx val v scaled-idx))

(define-ppc64-vinsn misc-ref-s16  (((dest :s16))
				   ((v :lisp)
				    (scaled-idx :s64))
				   ())
  (lhax dest v scaled-idx))

(define-ppc64-vinsn misc-ref-c-s16  (((dest :s16))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (lha dest (:apply + ppc64::misc-data-offset (:apply ash idx 1)) v))

(define-ppc64-vinsn misc-set-c-s16  (((val :s16))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (sth val (:apply + ppc64::misc-data-offset (:apply ash idx 1)) v))

(define-ppc64-vinsn misc-set-s16 (((val :s16))
				  ((v :lisp)
				   (scaled-idx :s64)))
  (sthx val v scaled-idx))

(define-ppc64-vinsn misc-ref-u8  (((dest :u8))
				  ((v :lisp)
				   (scaled-idx :u64))
				  ())
  (lbzx dest v scaled-idx))

(define-ppc64-vinsn misc-ref-c-u8  (((dest :u8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (lbz dest (:apply + ppc64::misc-data-offset idx) v))

(define-ppc64-vinsn misc-set-c-u8  (((val :u8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (stb val (:apply + ppc64::misc-data-offset idx) v))

(define-ppc64-vinsn misc-set-u8  (((val :u8))
				  ((v :lisp)
				   (scaled-idx :u64))
				  ())
  (stbx val v scaled-idx))

(define-ppc64-vinsn misc-ref-s8  (((dest :s8))
				  ((v :lisp)
				   (scaled-idx :u64))
				  ())
  (lbzx dest v scaled-idx)
  (extsb dest dest))

(define-ppc64-vinsn misc-ref-c-s8  (((dest :s8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (lbz dest (:apply + ppc64::misc-data-offset idx) v)
  (extsb dest dest))

(define-ppc64-vinsn misc-set-c-s8  (((val :s8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (stb val (:apply + ppc64::misc-data-offset idx) v))

(define-ppc64-vinsn misc-set-s8  (((val :s8))
				  ((v :lisp)
				   (scaled-idx :u64))
				  ())
  (stbx val v scaled-idx))

(define-ppc64-vinsn misc-ref-c-bit (((dest :u8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (lwz dest (:apply + ppc64::misc-data-offset (:apply ash idx -5)) v)
  (rlwinm dest dest (:apply 1+ (:apply logand idx #x1f)) 31 31))

(define-ppc64-vinsn misc-ref-c-bit-fixnum (((dest :imm))
					   ((v :lisp)
					    (idx :u32const))
					   ((temp :u32)))
  (lwz temp (:apply + ppc64::misc-data-offset (:apply ash idx -5)) v)
  (rlwinm dest 
	  temp
	  (:apply + 1 ppc64::fixnumshift (:apply logand idx #x1f)) 
	  (- ppc64::least-significant-bit ppc64::fixnumshift)
	  (- ppc64::least-significant-bit ppc64::fixnumshift)))


(define-ppc64-vinsn misc-ref-node  (((dest :lisp))
				    ((v :lisp)
				     (scaled-idx :s64))
				    ())
  (ldx dest v scaled-idx))

(define-ppc64-vinsn misc-set-node (()
				   ((val :lisp)
				    (v :lisp)
				    (scaled-idx :s64))
				   ())
  (stdx val v scaled-idx))




(define-ppc64-vinsn misc-ref-c-node (((dest :lisp))
				     ((v :lisp)
				      (idx :s16const))
				     ())
  (ld dest (:apply + ppc64::misc-data-offset (:apply ash idx 3)) v))

(define-ppc64-vinsn misc-set-c-node (()
				     ((val :lisp)
				      (v :lisp)
				      (idx :s16const))
				     ())
  (std val (:apply + ppc64::misc-data-offset (:apply ash idx 3)) v))


(define-ppc64-vinsn misc-element-count-fixnum (((dest :imm))
					       ((v :lisp))
					       ((temp :u64)))
  (ld temp ppc64::misc-header-offset v)
  (srdi temp temp ppc64::num-subtag-bits)
  (sldi dest temp ppc64::fixnumshift))

(define-ppc64-vinsn check-misc-bound (()
				      ((idx :imm)
				       (v :lisp))
				      ((temp :u64)))
  (ld temp ppc64::misc-header-offset v)
  (srdi temp temp ppc64::num-subtag-bits)
  (sldi temp temp ppc64::fixnumshift)
  (tdlge idx temp))

(define-ppc64-vinsn 2d-unscaled-index (((dest :imm)
                                        (dim1 :u32))
				       ((dim1 :u32)
                                        (i :imm)
					(j :imm)))
  (mulld dim1 i dim1)
  (add dest dim1 j))


;; dest <- (+ (* i dim1 dim2) (* j dim2) k)
(define-ppc64-vinsn 3d-unscaled-index (((dest :imm)
                                        (dim1 :u64)
                                        (dim2 :u64))
				       ((dim1 :u64)
                                        (dim2 :u64)
                                        (i :imm)
					(j :imm)
                                        (k :imm)))
  (mulld dim1 dim1 dim2)
  (mulld dim2 j dim2)
  (mulld dim1 i dim1)
  (add dim2 dim1 dim2)
  (add dest dim2 k))


(define-ppc64-vinsn 2d-32-scaled-index (((dest :u64))
					((array :lisp)
					 (i :imm)
					 (j :imm)
					 (dim1 :u32)))
  (mulld dest i dim1)
  (add dest dest j)
  (la dest ppc64::misc-data-offset dest))

(define-ppc64-vinsn 2d-dim1 (((dest :u64))
			     ((header :lisp)))
  (ld dest (+ ppc64::misc-data-offset (* 8 (1+ ppc64::arrayH.dim0-cell))) header)
  (sradi dest dest ppc64::fixnumshift))

(define-ppc64-vinsn 3d-dims (((dim1 :u64)
                              (dim2 :u64))
                             ((header :lisp)))
  (ld dim1 (+ ppc64::misc-data-offset (* 8 (1+ ppc64::arrayH.dim0-cell))) header)
  (ld dim2 (+ ppc64::misc-data-offset (* 8 (+ 2 ppc64::arrayH.dim0-cell))) header)
  (sradi dim1 dim1 ppc64::fixnumshift)
  (sradi dim2 dim2 ppc64::fixnumshift))

;;; Return dim1 (unboxed)
(define-ppc64-vinsn check-2d-bound (((dim :u64))
				    ((i :imm)
				     (j :imm)
				     (header :lisp)))
  (ld dim (+ ppc64::misc-data-offset (* 8 ppc64::arrayH.dim0-cell)) header)
  (tdlge i dim)
  (ld dim (+ ppc64::misc-data-offset (* 8 (1+ ppc64::arrayH.dim0-cell))) header)
  (tdlge j dim)
  (sradi dim dim ppc64::fixnumshift))

(define-ppc64-vinsn check-3d-bound (((dim1 :u64)
                                     (dim2 :u64))
                                    ((i :imm)
                                     (j :imm)
                                     (k :imm)
                                     (header :lisp)))
  (ld dim1 (+ ppc64::misc-data-offset (* 8 ppc64::arrayH.dim0-cell)) header)
  (tdlge i dim1)
  (ld dim1 (+ ppc64::misc-data-offset (* 8 (1+ ppc64::arrayH.dim0-cell))) header)
  (tdlge j dim1)
  (ld dim2 (+ ppc64::misc-data-offset (* 8 (+ 2 ppc64::arrayH.dim0-cell))) header)
  (tdlge k dim2)
  (sradi dim1 dim1 ppc64::fixnumshift)
  (sradi dim2 dim2 ppc64::fixnumshift))

(define-ppc64-vinsn array-data-vector-ref (((dest :lisp))
					   ((header :lisp)))
  (ld dest ppc64::arrayH.data-vector header))
  

(define-ppc64-vinsn check-arrayH-rank (()
				       ((header :lisp)
					(expected :u32const))
				       ((rank :imm)))
  (ld rank ppc64::arrayH.rank header)
  (tdi 27 rank (:apply ash expected ppc64::fixnumshift)))

(define-ppc64-vinsn check-arrayH-flags (()
					((header :lisp)
					 (expected :u16const))
					((flags :imm)
					 (xreg :u32)))
  (lis xreg (:apply ldb (byte 16 16) (:apply ash expected ppc64::fixnumshift)))
  (ori xreg xreg (:apply ldb (byte 16 0) (:apply ash expected ppc64::fixnumshift)))
  (ld flags ppc64::arrayH.flags header)
  (td 27 flags xreg))


(define-ppc64-vinsn trap-unless-simple-array-2 (()
                                               ((object :lisp)
                                                (expected-flags :u64const)
                                                (type-error :u8const))
                                               ((tag :u8)
                                                (flags :u64)
                                                (crf :crf)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne crf :bad)
  (lbz tag ppc64::misc-subtag-offset object)
  (cmpdi crf tag ppc64::subtag-arrayH)
  (bne crf :bad) 
  (ld tag ppc64::arrayH.rank object)
  (cmpdi crf tag (ash 2 ppc64::fixnumshift))
  (lis tag (:apply ldb (byte 16 16) (:apply ash expected-flags ppc64::fixnumshift)))
  (ld flags ppc64::arrayH.flags object)
  (ori tag tag (:apply ldb (byte 16 0) (:apply ash expected-flags ppc64::fixnumshift)))
  (bne crf :bad)
  (cmpd crf tag flags)
  (beq crf :good)
  :bad
  (uuo_interr type-error object)
  :good)

(define-ppc64-vinsn trap-unless-simple-array-3 (()
                                               ((object :lisp)
                                                (expected-flags :u64const)
                                                (type-error :u8const))
                                               ((tag :u8)
                                                (flags :u64)
                                                (crf :crf)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne crf :bad)
  (lbz tag ppc64::misc-subtag-offset object)
  (cmpdi crf tag ppc64::subtag-arrayH)
  (bne crf :bad) 
  (ld tag ppc64::arrayH.rank object)
  (cmpdi crf tag (ash 3 ppc64::fixnumshift))
  (lis tag (:apply ldb (byte 16 16) (:apply ash expected-flags ppc64::fixnumshift)))
  (ld flags ppc64::arrayH.flags object)
  (ori tag tag (:apply ldb (byte 16 0) (:apply ash expected-flags ppc64::fixnumshift)))
  (bne crf :bad)
  (cmpd crf tag flags)
  (beq crf :good)
  :bad
  (uuo_interr type-error object)
  :good)

  
(define-ppc64-vinsn node-slot-ref  (((dest :lisp))
				    ((node :lisp)
				     (cellno :u32const)))
  (ld dest (:apply + ppc64::misc-data-offset (:apply ash cellno 3)) node))



(define-ppc64-vinsn  %slot-ref (((dest :lisp))
				((instance (:lisp (:ne dest)))
				 (index :lisp))
				((scaled :s64)))
  (la scaled ppc64::misc-data-offset index)
  (ldx dest instance scaled)
  (tdeqi dest ppc64::slot-unbound-marker))


;;; Untagged memory reference & assignment.

(define-ppc64-vinsn mem-ref-c-fullword (((dest :u32))
					((src :address)
					 (index :s16const)))
  (lwz dest index src))

(define-ppc64-vinsn mem-ref-c-signed-fullword (((dest :s32))
                                               ((src :address)
                                                (index :s16const)))
  (lwa dest index src))

(define-ppc64-vinsn mem-ref-c-doubleword (((dest :u64))
                                          ((src :address)
                                           (index :s16const)))
  (ld dest index src))

(define-ppc64-vinsn mem-ref-c-signed-doubleword (((dest :s64))
                                                 ((src :address)
                                                  (index :s16const)))
  (ld dest index src))

(define-ppc64-vinsn mem-ref-c-natural (((dest :u64))
                                       ((src :address)
                                        (index :s16const)))
  (ld dest index src))

(define-ppc64-vinsn mem-ref-fullword (((dest :u32))
				      ((src :address)
				       (index :s64)))
  (lwzx dest src index))

(define-ppc64-vinsn mem-ref-signed-fullword (((dest :s32))
                                             ((src :address)
                                              (index :s64)))
  (lwax dest src index))

(define-ppc64-vinsn mem-ref-doubleword (((dest :u64))
                                        ((src :address)
                                         (index :s64)))
  (ldx dest src index))

(define-ppc64-vinsn mem-ref-natural (((dest :u64))
                                        ((src :address)
                                         (index :s64)))
  (ldx dest src index))

(define-ppc64-vinsn mem-ref-signed-doubleword (((dest :s64))
                                               ((src :address)
                                                (index :s64)))
  (ldx dest src index))

(define-ppc64-vinsn mem-ref-c-u16 (((dest :u16))
				   ((src :address)
				    (index :s16const)))
  (lhz dest index src))

(define-ppc64-vinsn mem-ref-u16 (((dest :u16))
				 ((src :address)
				  (index :s32)))
  (lhzx dest src index))


(define-ppc64-vinsn mem-ref-c-s16 (((dest :s16))
				   ((src :address)
				    (index :s16const)))
  (lha dest index src))

(define-ppc64-vinsn mem-ref-s16 (((dest :s16))
				 ((src :address)
				  (index :s32)))
  (lhax dest src index))

(define-ppc64-vinsn mem-ref-c-u8 (((dest :u8))
				  ((src :address)
				   (index :s16const)))
  (lbz dest index src))

(define-ppc64-vinsn mem-ref-u8 (((dest :u8))
				((src :address)
				 (index :s32)))
  (lbzx dest src index))

(define-ppc64-vinsn mem-ref-c-s8 (((dest :s8))
				  ((src :address)
				   (index :s16const)))
  (lbz dest index src)
  (extsb dest dest))

(define-ppc64-vinsn mem-ref-s8 (((dest :s8))
				((src :address)
				 (index :s32)))
  (lbzx dest src index)
  (extsb dest dest))

(define-ppc64-vinsn mem-ref-c-bit (((dest :u8))
				   ((src :address)
				    (byte-index :s16const)
				    (bit-shift :u8const)))
  (lbz dest byte-index src)
  (rlwinm dest dest bit-shift 31 31))

(define-ppc64-vinsn mem-ref-c-bit-fixnum (((dest :lisp))
					  ((src :address)
					   (byte-index :s16const)
					   (bit-shift :u8const))
					  ((byteval :u8)))
  (lbz byteval byte-index src)
  (rlwinm dest byteval bit-shift 29 29))

(define-ppc64-vinsn mem-ref-bit (((dest :u8))
				 ((src :address)
				  (bit-index :lisp))
				 ((byte-index :s16)
				  (bit-shift :u8)))
  (srwi byte-index bit-index (+ ppc64::fixnumshift 3))
  (extrwi bit-shift bit-index 3 27)
  (addi bit-shift bit-shift 29)
  (lbzx dest src byte-index)
  (rlwnm dest dest bit-shift 31 31))


(define-ppc64-vinsn mem-ref-bit-fixnum (((dest :lisp))
					((src :address)
					 (bit-index :lisp))
					((byte-index :s16)
					 (bit-shift :u8)))
  (srwi byte-index bit-index (+ ppc64::fixnumshift 3))
  (extrwi bit-shift bit-index 3 27)
  (addi bit-shift bit-shift 27)
  (lbzx byte-index src byte-index)
  (rlwnm dest
         byte-index
         bit-shift
         (- ppc64::least-significant-bit ppc64::fixnum-shift)
         (- ppc64::least-significant-bit ppc64::fixnum-shift)))

(define-ppc64-vinsn mem-ref-c-double-float (((dest :double-float))
					    ((src :address)
					     (index :s16const)))
  (lfd dest index src))

(define-ppc64-vinsn mem-ref-double-float (((dest :double-float))
					  ((src :address)
					   (index :s32)))
  (lfdx dest src index))

(define-ppc64-vinsn mem-set-c-double-float (()
					    ((val :double-float)
					     (src :address)
					     (index :s16const)))
  (stfd val index src))

(define-ppc64-vinsn mem-set-double-float (()
					  ((val :double-float)
					   (src :address)
					   (index :s32)))
  (stfdx val src index))

(define-ppc64-vinsn mem-ref-c-single-float (((dest :single-float))
					    ((src :address)
					     (index :s16const)))
  (lfs dest index src))

(define-ppc64-vinsn mem-ref-single-float (((dest :single-float))
					  ((src :address)
					   (index :s32)))
  (lfsx dest src index))

(define-ppc64-vinsn mem-set-c-single-float (()
					    ((val :single-float)
					     (src :address)
					     (index :s16const)))
  (stfs val index src))

(define-ppc64-vinsn mem-set-single-float (()
					  ((val :single-float)
					   (src :address)
					   (index :s32)))
  (stfsx val src index))


(define-ppc64-vinsn mem-set-c-doubleword (()
                                          ((val :u64)
                                           (src :address)
                                           (index :s16const)))
  (std val index src))

(define-ppc64-vinsn mem-set-doubleword (()
                                        ((val :u64)
                                         (src :address)
                                         (index :s64)))
  (stdx val index src))

(define-ppc64-vinsn mem-set-c-address (()
                                       ((val :address)
                                        (src :address)
                                        (index :s16const)))
  (std val index src))

(define-ppc64-vinsn mem-set-address (()
                                     ((val :address)
                                      (src :address)
                                      (index :s64)))
  (stdx val src index))

(define-ppc64-vinsn mem-set-c-fullword (()
					((val :u32)
					 (src :address)
					 (index :s16const)))
  (stw val index src))

(define-ppc64-vinsn mem-set-fullword (()
				      ((val :u32)
				       (src :address)
				       (index :s32)))
  (stwx val src index))

(define-ppc64-vinsn mem-set-c-halfword (()
					((val :u16)
					 (src :address)
					 (index :s16const)))
  (sth val index src))

(define-ppc64-vinsn mem-set-halfword (()
				      ((val :u16)
				       (src :address)
				       (index :s32)))
  (sthx val src index))

(define-ppc64-vinsn mem-set-c-byte (()
				    ((val :u16)
				     (src :address)
				     (index :s16const)))
  (stb val index src))

(define-ppc64-vinsn mem-set-byte (()
				  ((val :u8)
				   (src :address)
				   (index :s32)))
  (stbx val src index))

(define-ppc64-vinsn mem-set-c-bit-0 (()
				     ((src :address)
				      (byte-index :s16const)
				      (mask-begin :u8const)
				      (mask-end :u8const))
				     ((val :u8)))
  (lbz val byte-index src)
  (rlwinm val val 0 mask-begin mask-end)
  (stb val byte-index src))

(define-ppc64-vinsn mem-set-c-bit-1 (()
				     ((src :address)
				      (byte-index :s16const)
				      (mask :u8const))
				     ((val :u8)))
  (lbz val byte-index src)
  (ori val val mask)
  (stb val byte-index src))

(define-ppc64-vinsn mem-set-c-bit (()
				   ((src :address)
				    (byte-index :s16const)
				    (bit-index :u8const)
				    (val :imm))
				   ((byteval :u8)))
  (lbz byteval byte-index src)
  (rlwimi byteval val (:apply logand 31 (:apply - 29 bit-index)) bit-index bit-index)
  (stb byteval byte-index src))


(define-ppc64-vinsn mem-set-bit (()
				 ((src :address)
				  (bit-index :lisp)
				  (val :lisp))
				 ((bit-shift :u32)
				  (mask :u32)
				  (byte-index :u32)
				  (crf :crf)))
  (cmplwi crf val (ash 1 ppc64::fixnumshift))
  (extrwi bit-shift bit-index 3 27)
  (li mask #x80)
  (srw mask mask bit-shift)
  (ble+ crf :got-it)
  (uuo_interr arch::error-object-not-bit src)
  :got-it
  (srwi bit-shift bit-index (+ 3 ppc64::fixnumshift))
  (lbzx bit-shift src bit-shift)
  (beq crf :set)
  (andc mask bit-shift mask)
  (b :done)
  :set
  (or mask bit-shift mask)
  :done
  (srwi bit-shift bit-index (+ 3 ppc64::fixnumshift))
  (stbx mask src bit-shift))
     
;;; Tag and subtag extraction, comparison, checking, trapping ...

(define-ppc64-vinsn extract-tag (((tag :u8)) 
				 ((object :lisp)) 
				 ())
  (clrldi tag object (- ppc64::nbits-in-word ppc64::nlisptagbits)))

(define-ppc64-vinsn extract-tag-fixnum (((tag :imm))
					((object :lisp)))
  (clrlsldi tag object (- ppc64::nbits-in-word ppc64::nlisptagbits) ppc64::fixnum-shift))

(define-ppc64-vinsn extract-fulltag (((tag :u8))
				     ((object :lisp))
				     ())
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits)))


(define-ppc64-vinsn extract-fulltag-fixnum (((tag :imm))
					    ((object :lisp)))
  (clrlsldi tag object (- ppc64::nbits-in-word ppc64::ntagbits) ppc64::fixnum-shift))


(define-ppc64-vinsn extract-typecode (((code :u8))
				      ((object :lisp))
				      ((crf :crf)))
  (clrldi code object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf code ppc64::fulltag-misc)
  (clrldi code code (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (bne crf :not-misc)
  (lbz code ppc64::misc-subtag-offset object)
  :not-misc)

(define-ppc64-vinsn extract-typecode-fixnum (((code :imm))
					     ((object (:lisp (:ne code))))
					     ((crf :crf) (subtag :u8)))
  (clrldi subtag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf subtag ppc64::fulltag-misc)
  (clrldi subtag object (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (bne crf :not-misc)
  (lbz subtag ppc64::misc-subtag-offset object)
  :not-misc
  (sldi code subtag ppc64::fixnum-shift))


(define-ppc64-vinsn require-fixnum (()
				    ((object :lisp))
				    ((crf0 (:crf 0))
				     (tag :u8)))
  :again
  (clrldi. tag object (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (beq+ crf0 :got-it)
  (uuo_intcerr arch::error-object-not-fixnum object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-integer (()
				     ((object :lisp))
				     ((crf0 (:crf 0))
				      (tag :u8)))
  :again
  (clrldi. tag object (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (beq+ crf0 :got-it)
  (cmpdi crf0 tag ppc64::fulltag-misc)
  (bne crf0 :no-got)
  (lbz tag ppc64::misc-subtag-offset object)
  (cmpdi crf0 tag ppc64::subtag-bignum)
  (beq+ crf0 :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-integer object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-simple-vector (()
					   ((object :lisp))
					   ((tag :u8)
					    (crf :crf)))
  :again
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne crf :no-got)
  (lbz tag ppc64::misc-subtag-offset object)
  (cmpdi crf tag ppc64::subtag-simple-vector)
  (beq+ crf :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-simple-vector object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-simple-string (()
					   ((object :lisp))
					   ((tag :u8)
					    (crf :crf)))
  :again
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne crf :no-got)
  (lbz tag ppc64::misc-subtag-offset object)
  (cmpdi crf tag ppc64::subtag-simple-base-string)
  (beq+ crf :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-simple-string object)
  (b :again)
  :got-it)


(define-ppc64-vinsn require-real (()
                                  ((object :lisp))
                                  ((crf0 (:crf 0))
                                   (crf1 :crf)
                                   (tag :u8)
                                   (mask :u64)))
  :again
  (lis mask (ash 1 (- ppc64::subtag-double-float (+ 32 16))))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (ori mask mask (ash 1 (- ppc64::subtag-bignum 32)))
  (cmpdi crf0 tag ppc64::fulltag-misc)
  (sldi mask mask 32)
  (bne crf0 :have-typecode)
  (lbz tag ppc64::misc-subtag-offset object)
  :have-typecode
  (ori mask mask (logior (ash 1 ppc64::subtag-ratio)
                         (ash 1 ppc64::fulltag-odd-fixnum)
                         (ash 1 ppc64::subtag-single-float)
                         (ash 1 ppc64::fulltag-even-fixnum)))
  (cmpdi crf1 tag ppc64::subtag-double-float)
  (srd mask mask tag)
  (clrldi. mask mask 63)
  (bgt crf1 :no-got)
  (bne+ :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-real object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-number (()
                                    ((object :lisp))
                                    ((crf0 (:crf 0))
                                     (crf1 :crf)
                                     (tag :u8)
                                     (mask :u64)))
  :again
  (lis mask (ash 1 (- ppc64::subtag-double-float (+ 32 16))))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (ori mask mask (ash 1 (- ppc64::subtag-bignum 32)))
  (cmpdi crf0 tag ppc64::fulltag-misc)
  (sldi mask mask 32)
  (bne crf0 :have-typecode)
  (lbz tag ppc64::misc-subtag-offset object)
  :have-typecode
  (ori mask mask (logior (ash 1 ppc64::subtag-ratio)
                         (ash 1 ppc64::fulltag-odd-fixnum)
                         (ash 1 ppc64::subtag-single-float)
                         (ash 1 ppc64::fulltag-even-fixnum)))
  (cmpdi crf1 tag ppc64::subtag-double-float)
  (oris mask mask (ash 1 (- ppc64::subtag-complex 16)))
  (srd mask mask tag)
  (clrldi. mask mask 63)
  (bgt crf1 :no-got)
  (bne+ :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-number object)
  (b :again)
  :got-it)


(define-ppc64-vinsn require-list (()
				  ((object :lisp))
				  ((tag :u8)
				   (crfx :crf)
				   (crfy :crf)))
  :again
  (cmpdi crfx object (:apply target-nil-value))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crfy tag ppc64::fulltag-cons)
  (beq crfx :got-it)
  (beq+ crfy :got-it)
  (uuo_intcerr arch::error-object-not-list object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-symbol (()
				    ((object :lisp))
				    ((tag :u8)
				     (crf :crf)))
  :again
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne crf :no-got)
  (lbz tag ppc64::misc-subtag-offset object)
  (cmpdi crf tag ppc64::subtag-symbol)
  (beq+ crf :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-symbol object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-character (()
				       ((object :lisp))
				       ((tag :u8)
					(crf :crf)))
  :again
  (clrldi tag object (- ppc64::nbits-in-word ppc64::num-subtag-bits))
  (cmpdi crf tag ppc64::subtag-character)
  (beq+ crf :got-it)
  (uuo_intcerr arch::error-object-not-character object)
  (b :again)
  :got-it)


(define-ppc64-vinsn require-s8 (()
				((object :lisp))
				((crf :crf)
				 (tag :s64)))
  :again
  (sldi tag object (- ppc64::nbits-in-word (+ 8 ppc64::fixnumshift)))
  (sradi tag tag (- ppc64::nbits-in-word 8))
  (sldi tag tag ppc64::fixnumshift)
  (cmpd crf tag object)
  (beq+ crf :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-signed-byte-8 object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-u8 (()
				((object :lisp))
				((crf0 (:crf 0))
				 (tag :u32)))
  :again
  ;; The bottom ppc64::fixnumshift bits and the top (- 64 (+
  ;; ppc64::fixnumshift 8)) must all be zero.
  (rldicr. tag object (- 64 ppc64::fixnumshift) 55)
  (beq+ crf0 :got-it)
  (uuo_intcerr arch::error-object-not-unsigned-byte-8 object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-s16 (()
                                 ((object :lisp))
                                 ((crf :crf)
                                  (tag :s64)))
  :again
  (sldi tag object (- ppc64::nbits-in-word (+ 16 ppc64::fixnumshift)))
  (sradi tag tag (- ppc64::nbits-in-word 16))
  (sldi tag tag ppc64::fixnumshift)
  (cmpd crf tag object)
  (beq+ crf :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-signed-byte-16 object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-u16 (()
				((object :lisp))
				((crf0 (:crf 0))
				 (tag :s64)))
  :again
  ;; The bottom ppc64::fixnumshift bits and the top (- 64 (+
  ;; ppc64::fixnumshift 8)) must all be zero.
  (rldicr. tag object (- 64 ppc64::fixnumshift) 47)
  (beq+ crf0 :got-it)
  (uuo_intcerr arch::error-object-not-unsigned-byte-16 object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-s32 (()
                                 ((object :lisp))
                                 ((crf :crf)
                                  (tag :s64)))
  :again
  (sldi tag object (- ppc64::nbits-in-word (+ 32 ppc64::fixnumshift)))
  (sradi tag tag (- ppc64::nbits-in-word 32))
  (sldi tag tag ppc64::fixnumshift)
  (cmpd crf tag object)
  (beq+ crf :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-signed-byte-32 object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-u32 (()
				((object :lisp))
				((crf0 (:crf 0))
				 (tag :s64)))
  :again
  ;; The bottom ppc64::fixnumshift bits and the top (- 64 (+
  ;; ppc64::fixnumshift 32)) must all be zero.
  (rldicr. tag object (- 64 ppc64::fixnumshift) 31)
  (beq+ crf0 :got-it)
  (uuo_intcerr arch::error-object-not-unsigned-byte-32 object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-s64 (()
                                 ((object :lisp))
                                 ((crf0 (:crf 0))
                                  (crf1 :crf)
                                  (tag :s64)))
  :again
  (clrldi. tag object (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (beq+ crf0 :got-it)
  (cmpdi crf1 tag ppc64::fulltag-misc)
  (bne- crf1 :bad)
  (ld tag ppc64::misc-header-offset object)
  (cmpdi crf0 tag ppc64::two-digit-bignum-header)
  (beq+ crf0 :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-signed-byte-64 object)
  (b :again)
  :got-it)

(define-ppc64-vinsn require-u64 (()
                                 ((object :lisp))
                                 ((crf0 (:crf 0))
                                  (crf1 :crf)
                                  (crf2 :crf)
                                  (temp :u64)))
  (clrldi. temp object (- ppc64::nbits-in-word ppc64::fixnumshift))
  (clrldi temp object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf1 temp ppc64::fulltag-misc)
  (sradi temp object ppc64::fixnumshift)
  (beq crf0 :good-if-positive)
  (bne crf1 :bad)
  (ld temp ppc64::misc-header-offset object)
  (cmpdi crf0 temp  ppc64::three-digit-bignum-header)
  (cmpdi crf2 temp ppc64::two-digit-bignum-header)
  (beq crf0 :three-digit)
  (bne crf2 :bad)
  ;; two-digit case.  Must be positive.
  (ld temp ppc64::misc-data-offset object)
  (rotldi temp temp 32)
  :good-if-positive
  (cmpdi crf1 temp 0)
  (bge crf1 :good)
  :bad
  (uuo_interr arch::error-object-not-unsigned-byte-64 object)
  :three-digit
  (lwz temp (+ ppc64::misc-data-offset 8) object)
  (cmpwi crf1 temp 0)
  (bne crf1 :bad)
  :good
  )


(define-ppc64-vinsn require-char-code (()
                                       ((object :lisp))
                                       ((crf0 (:crf 0))
                                        (crf1 :crf)
                                        (tag :u32)))
  :again
  (clrldi. tag object (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (lis tag (ash (ash #x110000 ppc64::fixnumshift) -16))
  (cmpld crf1 object tag)
  (bne crf0 :bad)
  (blt+ crf1 :got-it)
  :bad
  (uuo_intcerr arch::error-object-not-mod-char-code-limit object)
  (b :again)
  :got-it)


(define-ppc64-vinsn box-fixnum (((dest :imm))
				((src :s64)))
  (sldi dest src ppc64::fixnumshift))

(define-ppc64-vinsn fixnum->signed-natural (((dest :s64))
                                            ((src :imm)))
  (sradi dest src ppc64::fixnumshift))

(define-ppc64-vinsn fixnum->unsigned-natural (((dest :u64))
                                              ((src :imm)))
  (srdi dest src ppc64::fixnumshift))



(define-ppc64-vinsn unbox-u64 (((dest :u64))
                               ((src :lisp))
                               ((crf0 (:crf 0))
                                (crf1 :crf)
                                (crf2 :crf)))
  (clrldi. dest src (- ppc64::nbits-in-word ppc64::fixnumshift))
  (clrldi dest src (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf1 dest ppc64::fulltag-misc)
  (sradi dest src ppc64::fixnumshift)
  (beq crf0 :good-if-positive)
  (bne crf1 :bad)
  (ld dest ppc64::misc-header-offset src)
  (cmpdi crf0 dest  ppc64::three-digit-bignum-header)
  (cmpdi crf2 dest ppc64::two-digit-bignum-header)
  (beq crf0 :three-digit)
  (bne crf2 :bad)
  ;; two-digit case.  Must be positive.
  (ld dest ppc64::misc-data-offset src)
  (rotldi dest dest 32)
  :good-if-positive
  (cmpdi crf1 dest 0)
  (bge crf1 :good)
  :bad
  (uuo_interr arch::error-object-not-unsigned-byte-64 src)
  :three-digit
  (lwz dest (+ ppc64::misc-data-offset 8) src)
  (cmpwi crf1 dest 0)
  (ld dest ppc64::misc-data-offset src)
  (rotldi dest dest 32)
  (bne crf1 :bad)
  :good
  )

(define-ppc64-vinsn unbox-s64 (((dest :s64))
                               ((src :lisp))
                               ((crf0 :crf)
                                (crf1 :crf)
                                (tag :u64)))
  
  (clrldi. tag src (- ppc64::nbits-in-word ppc64::fixnumshift))
  (clrldi tag src (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf1 tag ppc64::fulltag-misc)
  (sradi dest src ppc64::fixnumshift)
  (beq+ crf0 :good)
  (beq+ crf1 :bignum)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-64 src)
  :bignum
  (ld tag ppc64::misc-header-offset src)
  (ld dest ppc64::misc-data-offset src)
  (cmpdi crf0 tag ppc64::two-digit-bignum-header)
  (rotldi dest dest 32)
  (bne- crf0 :bad)
  :good
  )

;;; An object is of type (UNSIGNED-BYTE 32) iff
;;;  a) it's of type (UNSIGNED-BYTE 32)
;;; That pretty much narrows it down.
  
(define-ppc64-vinsn unbox-u32 (((dest :u32))
			       ((src :lisp))
			       ((crf0 (:crf 0))))
  (rldicr. dest src (- 64 ppc64::fixnumshift) 31)
  (srdi dest src ppc64::fixnumshift)
  (beq crf0 :got-it)
  :bad
  (uuo_interr arch::error-object-not-unsigned-byte-32 src)
  :got-it)

;;; an object is of type (SIGNED-BYTE 32) iff
;;; a) it's of type (SIGNED-BYTE 32)
;;; b) see (a).


(define-ppc64-vinsn unbox-s32 (((dest :s32))
			       ((src :lisp))
			       ((crf :crf)))
  (sldi dest src (- ppc64::nbits-in-word (+ 32 ppc64::fixnumshift)))
  (sradi dest dest (- ppc64::nbits-in-word 32))
  (sldi dest dest ppc64::fixnumshift)
  (cmpd crf dest src)
  (sradi dest src ppc64::fixnumshift)
  (beq crf :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-32 src)
  :got-it)


(define-ppc64-vinsn unbox-u16 (((dest :u16))
			       ((src :lisp))
			       ((crf0 (:crf 0))))
  ;; The bottom ppc64::fixnumshift bits and the top (- 31 (+
  ;; ppc64::fixnumshift 16)) must all be zero.
  (rldicr. dest src (- 64 ppc64::fixnumshift) 47)
  (srdi dest src ppc64::fixnumshift)
  (beq+ crf0 :got-it)
  (uuo_interr arch::error-object-not-unsigned-byte-16 src)
  :got-it)

(define-ppc64-vinsn unbox-s16 (((dest :s16))
			       ((src :lisp))
			       ((crf :crf)))
  (sldi dest src (- ppc64::nbits-in-word (+ 16 ppc64::fixnumshift)))
  (sradi dest dest (- ppc64::nbits-in-word 16))
  (sldi dest dest ppc64::fixnumshift)
  (cmpd crf dest src)
  (sradi dest src ppc64::fixnumshift)
  (beq+ crf :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-16 src)
  :got-it)

  
  
(define-ppc64-vinsn unbox-u8 (((dest :u8))
			      ((src :lisp))
			      ((crf0 (:crf 0))))
  ;; The bottom ppc64::fixnumshift bits and the top (- 63 (+
  ;; ppc64::fixnumshift 8)) must all be zero.
  (rldicr. dest src (- 64 ppc64::fixnumshift) 55)
  (srdi dest src ppc64::fixnumshift)
  (beq+ crf0 :got-it)
  (uuo_interr arch::error-object-not-unsigned-byte-8 src)
  :got-it)

(define-ppc64-vinsn %unbox-u8 (((dest :u8))
			      ((src :lisp)))
  ;; The bottom ppc64::fixnumshift bits and the top (- 63 (+
  ;; ppc64::fixnumshift 8)) must all be zero.
  (rldicl dest src (- 64 ppc64::fixnumshift) 56))

(define-ppc64-vinsn unbox-s8 (((dest :s8))
			      ((src :lisp))
			      ((crf :crf)))
  (sldi dest src (- ppc64::nbits-in-word (+ 8 ppc64::fixnumshift)))
  (sradi dest dest (- ppc64::nbits-in-word 8))
  (sldi dest dest ppc64::fixnumshift)
  (cmpd crf dest src)
  (sradi dest src ppc64::fixnumshift)
  (beq+ crf :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-16 src)
  :got-it)

(define-ppc64-vinsn unbox-base-char (((dest :u32))
				     ((src :lisp))
				     ((crf :crf)))
  (clrldi dest src (- 64 ppc64::num-subtag-bits))
  (cmpdi crf dest ppc64::subtag-character)
  (srdi dest src ppc64::charcode-shift)
  (beq+ crf :got-it)
  (uuo_interr arch::error-object-not-character src)
  :got-it)

(define-ppc64-vinsn unbox-bit (((dest :u32))
			       ((src :lisp))
			       ((crf :crf)))
  (cmplwi crf src (ash 1 ppc64::fixnumshift))
  (srawi dest src ppc64::fixnumshift)
  (ble+ crf :got-it)
  (uuo_interr arch::error-object-not-bit src)
  :got-it)

(define-ppc64-vinsn unbox-bit-bit0 (((dest :u32))
				    ((src :lisp))
				    ((crf :crf)))
  (cmplwi crf src (ash 1 ppc64::fixnumshift))
  (rlwinm dest src (- 32 (1+ ppc64::fixnumshift)) 0 0)
  (ble+ crf :got-it)
  (uuo_interr arch::error-object-not-bit src)
  :got-it)




(define-ppc64-vinsn shift-right-variable-word (((dest :u32))
					       ((src :u32)
						(sh :u32)))
  (srw dest src sh))

;;; These vinsns are used in bit extraction operations, which
;;; currently do 32-bit memory references on both platforms.
(define-ppc64-vinsn u32logandc2 (((dest :u32))
				 ((x :u32)
				  (y :u32)))
  (andc dest x y))

(define-ppc64-vinsn u32logior (((dest :u32))
			       ((x :u32)
				(y :u32)))
  (or dest x y))


(define-ppc64-vinsn trap-unless-fixnum (()
					((object :lisp))
					((tag :u8)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (tdnei tag ppc64::tag-fixnum))

(define-ppc64-vinsn trap-unless-character (()
                                           ((object :lisp))
                                           ((tag :u8)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::num-subtag-bits))
  (tdnei tag ppc64::subtag-character))


(define-ppc64-vinsn trap-unless-cons (()
					((object :lisp))
					((tag :u8)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (tdnei tag ppc64::fulltag-cons))

(define-ppc64-vinsn trap-unless-list (()
				      ((object :lisp))
				      ((tag :u8)
				       (crf :crf)))
  (cmpldi crf object (:apply target-nil-value))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (beq crf :ok)
  (tdi 3 tag ppc64::fulltag-cons)
  :ok)

(define-ppc64-vinsn trap-unless-uvector (()
					 ((object :lisp))
                                         ((tag :u8)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (tdnei tag ppc64::fulltag-misc))

(define-ppc64-vinsn trap-unless-single-float (()
                                              ((object :lisp))
                                              ((tag :u8)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (tdnei tag ppc64::subtag-single-float))

(define-ppc64-vinsn trap-unless-double-float (()
                                              ((object :lisp))
                                              ((tag :u8)
                                               (crf :crf)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne crf :do-trap)
  (lbz tag ppc64::misc-subtag-offset object)
  :do-trap
  (tdnei tag ppc64::subtag-double-float))

(define-ppc64-vinsn trap-unless-array-header (()
                                              ((object :lisp))
                                              ((tag :u8)
                                               (crf :crf)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne crf :do-trap)
  (lbz tag ppc64::misc-subtag-offset object)
  :do-trap
  (tdnei tag ppc64::subtag-arrayH))

(define-ppc64-vinsn trap-unless-macptr (()
                                        ((object :lisp))
                                        ((tag :u8)
                                         (crf :crf)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne crf :do-trap)
  (lbz tag ppc64::misc-subtag-offset object)
  :do-trap
  (tdnei tag ppc64::subtag-macptr))


(define-ppc64-vinsn trap-unless-typecode= (()
					   ((object :lisp)
					    (tagval :u16const))
					   ((tag :u8)
					    (crf :crf)))
  (clrldi tag object (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (clrldi tag object (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (bne crf :do-trap)
  (lbz tag ppc64::misc-subtag-offset object)
  :do-trap
  (tdnei tag tagval))
  
(define-ppc64-vinsn subtract-constant (((dest :imm))
				       ((src :imm)
					(const :s16const)))
  (subi dest src const))




;;; Bit-extraction & boolean operations


;;; For some mind-numbing reason, IBM decided to call the most significant
;;; bit in a 32-bit word "bit 0" and the least significant bit "bit 31"
;;; (this despite the fact that it's essentially a big-endian architecture
;;; (it was exclusively big-endian when this decision was made.))
;;; We'll probably be least confused if we consistently use this backwards
;;; bit ordering (letting things that have a "sane" bit-number worry about
;;; it at compile-time or run-time (subtracting the "sane" bit number from
;;; 31.))

(define-ppc64-vinsn extract-variable-bit (((dest :u8))
					  ((src :u32)
					   (bitnum :u8))
					  ())
  (rotlw dest src bitnum)
  (extrwi dest dest 1 0))


(define-ppc64-vinsn extract-variable-bit-fixnum (((dest :imm))
						 ((src :u32)
						  (bitnum :u8))
						 ((temp :u32)))
  (rotlw temp src bitnum)
  (rlwinm dest
          temp 
          (1+ ppc64::fixnumshift) 
          (- ppc64::least-significant-bit ppc64::fixnumshift)
          (- ppc64::least-significant-bit ppc64::fixnumshift)))


(define-ppc64-vinsn lowbit->truth (((dest :lisp)
                                    (bits :u64))
                                   ((bits :u64))
                                   ())
  (mulli bits bits ppc64::t-offset)
  (addi dest bits (:apply target-nil-value)))

(define-ppc64-vinsn invert-lowbit (((bits :u64))
                                   ((bits :u64))
                                   ())
  (xori bits bits 1))

                           

;;; Some of the obscure-looking instruction sequences - which map some
;;; relation to PPC bit 31 of some register - were found by the GNU
;;; SuperOptimizer.  Some of them use extended-precision instructions
;;; (which may cause interlocks on some superscalar PPCs, if I
;;; remember correctly.)  In general, sequences that GSO found that
;;; -don't- do extended precision are longer and/or use more
;;; temporaries.  On the 604, the penalty for using an instruction
;;; that uses the CA bit is "at least" one cycle: it can't complete
;;; execution until all "older" instructions have.  That's not
;;; horrible, especially given that the alternative is usually to use
;;; more instructions (and, more importantly, more temporaries) to
;;; avoid using extended-precision.


(define-ppc64-vinsn eq0->bit31 (((bits :u64))
				((src (t (:ne bits)))))
  (cntlzd bits src)
  (srdi bits bits 6))			; bits = 0000...000X

(define-ppc64-vinsn ne0->bit31 (((bits :u64))
				((src (t (:ne bits)))))
  (cntlzd bits src)
  (sld bits src bits)
  (srdi bits bits 63))			; bits = 0000...000X

(define-ppc64-vinsn lt0->bit31 (((bits :u64))
				((src (t (:ne bits)))))
  (srdi bits src 63))                   ; bits = 0000...000X


(define-ppc64-vinsn ge0->bit31 (((bits :u64))
				((src (t (:ne bits)))))
  (srdi bits src 63)       
  (xori bits bits 1))                   ; bits = 0000...000X


(define-ppc64-vinsn le0->bit31 (((bits :u64))
				((src (t (:ne bits)))))
  (neg bits src)
  (orc bits bits src)
  (srdi bits bits 63))                  ; bits = 0000...000X

(define-ppc64-vinsn gt0->bit31 (((bits :u64))
				((src (t (:ne bits)))))
  (subi bits src 1)       
  (nor bits bits src)
  (srdi bits bits 63))                  ; bits = 0000...000X

(define-ppc64-vinsn ne->bit31 (((bits :u64))
			       ((x t)
				(y t))
			       ((temp :u64)))
  (subf temp x y)
  (cntlzd bits temp)
  (sld bits temp bits)
  (srdi bits bits 63))			; bits = 0000...000X

(define-ppc64-vinsn fulltag->bit31 (((bits :u64))
				    ((lispobj :lisp)
				     (tagval :u8const))
				    ())
  (clrldi bits lispobj (- ppc64::nbits-in-word ppc64::ntagbits))
  (subi bits bits tagval)
  (cntlzd bits bits)
  (srdi bits bits 6))


(define-ppc64-vinsn eq->bit31 (((bits :u64))
			       ((x t)
				(y t)))
  (subf bits x y)
  (cntlzd bits bits)
  (srdi bits bits 6))			; bits = 0000...000X

(define-ppc64-vinsn eqnil->bit31 (((bits :u64))
				  ((x t)))
  (subi bits x (:apply target-nil-value))
  (cntlzd bits bits)
  (srdi bits bits 6))

(define-ppc64-vinsn ne->bit31 (((bits :u64))
			       ((x t)
				(y t)))
  (subf bits x y)
  (cntlzd bits bits)
  (srdi bits bits 6)
  (xori bits bits 1))

(define-ppc64-vinsn nenil->bit31 (((bits :u64))
				  ((x t)))
  (subi bits x (:apply target-nil-value))
  (cntlzd bits bits)
  (srdi bits bits 6)
  (xori bits bits 1))

(define-ppc64-vinsn lt->bit31 (((bits :u64))
			       ((x (t (:ne bits)))
				(y (t (:ne bits)))))

  (xor bits x y)
  (sradi bits bits 63)
  (or bits bits x)
  (subf bits y bits)
  (srdi bits bits 63))			; bits = 0000...000X

(define-ppc64-vinsn ltu->bit31 (((bits :u64))
				((x :u64)
				 (y :u64)))
  (subfc bits y x)
  (subfe bits bits bits)
  (neg bits bits))

(define-ppc64-vinsn le->bit31 (((bits :u64))
			       ((x (t (:ne bits)))
				(y (t (:ne bits)))))

  (xor bits x y)
  (sradi bits bits 63)
  (nor bits bits y)
  (add bits bits x)
  (srdi bits bits 63))			; bits = 0000...000X

(define-ppc64-vinsn leu->bit31  (((bits :u32))
				 ((x :u32)
				  (y :u32)))
  (subfc bits x y)
  (addze bits ppc::rzero))

(define-ppc64-vinsn gt->bit31 (((bits :u32))
			       ((x (t (:ne bits)))
				(y (t (:ne bits)))))

  (eqv bits x y)
  (sradi bits bits 63)
  (and bits bits x)
  (subf bits bits y)
  (srdi bits bits 63))			; bits = 0000...000X

(define-ppc64-vinsn gtu->bit31 (((bits :u64))
				((x :u64)
				 (y :u64)))
  (subfc bits x y)
  (subfe bits bits bits)
  (neg bits bits))

(define-ppc64-vinsn ge->bit31 (((bits :u64))
			       ((x (t (:ne bits)))
				(y (t (:ne bits)))))
  (eqv bits x y)
  (sradi bits bits 63)
  (andc bits bits x)
  (add bits bits y)
  (srdi bits bits 63))			; bits = 0000...000X

(define-ppc64-vinsn geu->bit31 (((bits :u64))
				((x :u64)
				 (y :u64)))
  (subfc bits y x)
  (addze bits ppc::rzero))


;;; there are big-time latencies associated with MFCR on more heavily
;;; pipelined processors; that implies that we should avoid this like
;;; the plague.
;;; GSO can't find anything much quicker for LT or GT, even though
;;; MFCR takes three cycles and waits for previous instructions to complete.
;;; Of course, using a CR field costs us something as well.
(define-ppc64-vinsn crbit->bit31 (((bits :u64))
				  ((crf :crf)
				   (bitnum :crbit))
				  ())
  (mfcr bits)                           ; Suffer.
  (rlwinm bits bits (:apply + 1  bitnum (:apply ash crf 2)) 31 31)) ; bits = 0000...000X


(define-ppc64-vinsn compare (((crf :crf))
			     ((arg0 t)
			      (arg1 t))
			     ())
  (cmpd crf arg0 arg1))

(define-ppc64-vinsn compare-to-nil (((crf :crf))
				    ((arg0 t)))
  (cmpdi crf arg0 (:apply target-nil-value)))

(define-ppc64-vinsn compare-logical (((crf :crf))
				     ((arg0 t)
				      (arg1 t))
				     ())
  (cmpld crf arg0 arg1))

(define-ppc64-vinsn double-float-compare (((crf :crf))
					  ((arg0 :double-float)
					   (arg1 :double-float))
					  ())
  (fcmpo crf arg0 arg1))
              

(define-ppc64-vinsn double-float+-2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float))
				     ((crf (:crf 4))))
  (fadd result x y))

(define-ppc64-vinsn double-float--2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float))
				     ((crf (:crf 4))))
  (fsub result x y))

(define-ppc64-vinsn double-float*-2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float))
				     ((crf (:crf 4))))
  (fmul result x y))

(define-ppc64-vinsn double-float/-2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float))
				     ((crf (:crf 4))))
  (fdiv result x y))

(define-ppc64-vinsn single-float+-2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float))
				     ((crf (:crf 4))))
  (fadds result x y))

(define-ppc64-vinsn single-float--2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float))
				     ((crf (:crf 4))))
  (fsubs result x y))

(define-ppc64-vinsn single-float*-2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float))
				     ((crf (:crf 4))))
  (fmuls result x y))

(define-ppc64-vinsn single-float/-2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float))
				     ((crf (:crf 4))))
  (fdivs result x y))



(define-ppc64-vinsn compare-signed-s16const (((crf :crf))
					     ((arg0 :imm)
					      (imm :s16const))
					     ())
  (cmpdi crf arg0 imm))

(define-ppc64-vinsn compare-unsigned-u16const (((crf :crf))
					       ((arg0 :u32)
						(imm :u16const))
					       ())
  (cmpldi crf arg0 imm))



;;; Extract a constant bit (0-63) from src; make it be bit 63 of dest.
;;; Bitnum is treated mod 64. (This is used in LOGBITP).
(define-ppc64-vinsn extract-constant-ppc-bit (((dest :u64))
					      ((src :imm)
					       (bitnum :u16const))
					      ())
  (rldicl dest src (:apply + 1 bitnum) 63))


(define-ppc64-vinsn set-constant-ppc-bit-to-variable-value (((dest :u32))
							    ((src :u32)
							     (bitval :u32) ; 0 or 1
							     (bitnum :u8const)))
  (rlwimi dest bitval (:apply - 31 bitnum) bitnum bitnum))

(define-ppc64-vinsn set-constant-ppc-bit-to-1 (((dest :u32))
					       ((src :u32)
						(bitnum :u8const)))
  ((:pred < bitnum 16)
   (oris dest src (:apply ash #x8000 (:apply - bitnum))))
  ((:pred >= bitnum 16)
   (ori dest src (:apply ash #x8000 (:apply - (:apply - bitnum 16))))))

(define-ppc64-vinsn set-constant-ppc-bit-to-0 (((dest :u32))
					       ((src :u32)
						(bitnum :u8const)))
  (rlwinm dest src 0 (:apply logand #x1f (:apply 1+ bitnum)) (:apply logand #x1f (:apply 1- bitnum))))

  
(define-ppc64-vinsn insert-bit-0 (((dest :u32))
				  ((src :u32)
				   (val :u32)))
  (rlwimi dest val 0 0 0))
  
;;; The bit number is boxed and wants to think of the
;;; least-significant bit as 0.  Imagine that.  To turn the boxed,
;;; lsb-0 bitnumber into an unboxed, msb-0 rotate count, we
;;; (conceptually) unbox it, add ppc64::fixnumshift to it, subtract it
;;; from 31, and add one.  This can also be done as "unbox and
;;; subtract from 28", I think ...  Actually, it'd be "unbox, then
;;; subtract from 30".
(define-ppc64-vinsn extract-variable-non-insane-bit (((dest :u64))
						     ((src :imm)
						      (bit :imm))
						     ((temp :u64)))
  (srdi temp bit ppc64::fixnumshift)
  (subfic temp temp (- 64 ppc64::fixnumshift))
  (rldcl dest src temp 63))
                                               
;;; Operations on lists and cons cells

(define-ppc64-vinsn %cdr (((dest :lisp))
			  ((src :lisp)))
  (ld dest ppc64::cons.cdr src))

(define-ppc64-vinsn %car (((dest :lisp))
			  ((src :lisp)))
  (ld dest ppc64::cons.car src))

(define-ppc64-vinsn %set-car (()
			      ((cell :lisp)
			       (new :lisp)))
  (std new ppc64::cons.car cell))

(define-ppc64-vinsn %set-cdr (()
			      ((cell :lisp)
			       (new :lisp)))
  (std new ppc64::cons.cdr cell))

(define-ppc64-vinsn load-adl (()
			      ((n :u32const)))
  (lis ppc::nargs (:apply ldb (byte 16 16) n))
  (ori ppc::nargs ppc::nargs (:apply ldb (byte 16 0) n)))
                            
(define-ppc64-vinsn set-nargs (()
			       ((n :s16const)))
  (li ppc::nargs (:apply ash n ppc64::word-shift)))

(define-ppc64-vinsn scale-nargs (()
				 ((nfixed :s16const)))
  ((:pred > nfixed 0)
   (la ppc::nargs (:apply - (:apply ash nfixed ppc64::word-shift)) ppc::nargs)))
                           


(define-ppc64-vinsn (vpush-register :push :node :vsp)
    (()
     ((reg :lisp)))
  (stdu reg -8 ppc::vsp))

(define-ppc64-vinsn (vpush-register-arg :push :node :vsp :outgoing-argument)
    (()
     ((reg :lisp)))
  (stdu reg -8 ppc::vsp))

(define-ppc64-vinsn (vpop-register :pop :node :vsp)
    (((dest :lisp))
     ())
  (ld dest 0 ppc::vsp)
  (la ppc::vsp ppc64::word-size-in-bytes ppc::vsp))


(define-ppc64-vinsn copy-node-gpr (((dest :lisp))
				   ((src :lisp)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src)))

(define-ppc64-vinsn copy-gpr (((dest t))
			      ((src t)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src)))


(define-ppc64-vinsn copy-fpr (((dest :double-float))
			      ((src :double-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fmr dest src)))

(define-ppc64-vinsn vcell-ref (((dest :lisp))
			       ((vcell :lisp)))
  (ld dest ppc64::misc-data-offset vcell))

(define-ppc64-vinsn vcell-set (()
			       ((vcell :lisp)
				(value :lisp)))
  (std value ppc64::misc-data-offset vcell))


(define-ppc64-vinsn make-vcell (((dest :lisp))
				((closed (:lisp :ne dest)))
				((header :u64)))
  (li header ppc64::value-cell-header)
  (la ppc::allocptr (- ppc64::fulltag-misc ppc64::value-cell.size) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (rldicr ppc::allocptr ppc::allocptr 0 (- 63 ppc64::ntagbits))
  (std closed ppc64::value-cell.value dest))

(define-ppc64-vinsn make-tsp-vcell (((dest :lisp))
				    ((closed :lisp))
				    ((header :u64)))
  (li header ppc64::value-cell-header)
  (stdu ppc::tsp -32 ppc::tsp)
  (std ppc::tsp 8 ppc::tsp)
  (stfd ppc::fp-zero 16 ppc::tsp)
  (stfd ppc::fp-zero 24 ppc::tsp)
  (std ppc::rzero 8 ppc::tsp)
  (std header (+ 16 ppc64::fulltag-misc ppc64::value-cell.header) ppc::tsp)
  (std closed (+ 16 ppc64::fulltag-misc ppc64::value-cell.value) ppc::tsp)
  (la dest (+ 16 ppc64::fulltag-misc) ppc::tsp))

(define-ppc64-vinsn make-tsp-cons (((dest :lisp))
				   ((car :lisp) (cdr :lisp))
				   ())
  (stdu ppc::tsp -32 ppc::tsp)
  (std ppc::tsp 8 ppc::tsp)
  (stfd ppc::fp-zero 16 ppc::tsp)
  (stfd ppc::fp-zero 24 ppc::tsp)
  (std ppc::rzero 8 ppc::tsp)
  (std car (+ 16 ppc64::fulltag-cons ppc64::cons.car) ppc::tsp)
  (std cdr (+ 16 ppc64::fulltag-cons ppc64::cons.cdr) ppc::tsp)
  (la dest (+ 16 ppc64::fulltag-cons) ppc::tsp))


(define-ppc64-vinsn %closure-code% (((dest :lisp))
				    ())
  (ld dest (:apply + ppc64::symbol.vcell (ppc64::nrs-offset %closure-code%) (:apply target-nil-value)) 0))

(define-ppc64-vinsn single-float-bits (((dest :u32))
                                       ((src :lisp)))
  (srdi dest  src 32))

(define-ppc64-vinsn (call-subprim :call :subprim-call) (()
							((spno :s32const)))
  (bla spno))

(define-ppc64-vinsn (jump-subprim :jumpLR) (()
					    ((spno :s32const)))
  (ba spno))

;;; Same as "call-subprim", but gives us a place to 
;;; track args, results, etc.
(define-ppc64-vinsn (call-subprim-0 :call :subprim-call) (((dest t))
							  ((spno :s32const)))
  (bla spno))

(define-ppc64-vinsn (call-subprim-1 :call :subprim-call) (((dest t))
							  ((spno :s32const)
							   (z t)))
  (bla spno))
  
(define-ppc64-vinsn (call-subprim-2 :call :subprim-call) (((dest t))
							  ((spno :s32const)
							   (y t)
							   (z t)))
  (bla spno))

(define-ppc64-vinsn (call-subprim-3 :call :subprim-call) (((dest t))
							  ((spno :s32const)
							   (x t)
							   (y t)
							   (z t)))
  (bla spno))

(define-ppc64-vinsn event-poll (()
				()
                                ((crf :crf)))
  (ld ppc::nargs ppc64::tcr.tlb-pointer ppc64::rcontext)
  (ld ppc::nargs ppc64::interrupt-level-binding-index ppc::nargs)
  (cmpdi crf ppc::nargs 0)
  (blt crf :done)
  (bgt crf :trap)
  (ld ppc::nargs ppc64::tcr.interrupt-pending ppc64::rcontext)
  :trap
  (tdgti ppc::nargs 0)
  :done)

(define-ppc64-vinsn ref-interrupt-level (((dest :imm))
                                         ()
                                         ((temp :u64)))
  (ld temp ppc64::tcr.tlb-pointer ppc64::rcontext)
  (ld dest ppc64::INTERRUPT-LEVEL-BINDING-INDEX temp))
                         
;;; Unconditional (pc-relative) branch
(define-ppc64-vinsn (jump :jump)
    (()
     ((label :label)))
  (b label))

(define-ppc64-vinsn (call-label :call) (()
					((label :label)))
  (bl label))

;;; just like JUMP, only (implicitly) asserts that the following 
;;; code is somehow reachable.
(define-ppc64-vinsn (non-barrier-jump :xref) (()
					      ((label :label)))
  (b label))


(define-ppc64-vinsn (cbranch-true :branch) (()
					    ((label :label)
					     (crf :crf)
					     (crbit :u8const)))
  (bt (:apply + crf crbit) label))

(define-ppc64-vinsn (cbranch-false :branch) (()
					     ((label :label)
					      (crf :crf)
					      (crbit :u8const)))
  (bf (:apply + crf crbit) label))

(define-ppc64-vinsn check-trap-error (()
				      ())
  (beq+ 0 :no-error)
  (uuo_interr arch::error-reg-regnum ppc::arg_z)
  :no-error)


(define-ppc64-vinsn lisp-word-ref (((dest t))
				   ((base t)
				    (offset t)))
  (ldx dest base offset))

(define-ppc64-vinsn lisp-word-ref-c (((dest t))
				     ((base t)
				      (offset :s16const)))
  (ld dest offset base))


(define-ppc64-vinsn (lri :constant-ref) (((dest :imm))
                                         ((intval :u64const))
                                         ())
  ((:or (:pred = (:apply ash intval -15) #x1FFFFFFFFFFFF)
        (:pred = (:apply ash intval -15) 0))
   (li dest (:apply %word-to-int (:apply logand #xffff intval))))
  ((:not
    (:or (:pred = (:apply ash intval -15) #x1FFFFFFFFFFFF)
         (:pred = (:apply ash intval -15) 0)))
   ((:or (:pred = (:apply ash intval -31) 0)
         (:pred = (:apply ash intval -31) #x1ffffffff))
    (lis dest (:apply %word-to-int (:apply ldb (:apply byte 16 16) intval)))
    ((:not (:pred = (:apply ldb (:apply byte 16 0) intval) 0))
     (ori dest dest (:apply ldb (:apply byte 16 0) intval))))
   ((:not (:or (:pred = (:apply ash intval -31) 0)
               (:pred = (:apply ash intval -31) #x1ffffffff)))
    ((:pred = (:apply ash intval -32) 0)
     (oris dest ppc::rzero (:apply ldb (:apply byte 16 16) intval))
     ((:not (:pred = (:apply ldb (:apply byte 16 0) intval) 0))
      (ori dest dest (:apply ldb (:apply byte 16 0) intval))))
    ((:not (:pred = (:apply ash intval -32) 0))
     ;; This is the general case, where all halfwords are significant.
     ;; Hopefully, something above catches lots of other cases.
     (lis dest (:apply %word-to-int (:apply ldb (:apply byte 16 48) intval)))
     ((:not (:pred = (:apply ldb (:apply byte 16 32) intval) 0))
      (ori dest dest (:apply ldb (:apply byte 16 32) intval)))
     (sldi dest dest 32)
     ((:not (:pred = (:apply ldb (:apply byte 16 16) intval) 0))
      (oris dest dest (:apply ldb (:apply byte 16 16) intval)))
     ((:not (:pred = (:apply ldb (:apply byte 16 0) intval) 0))
      (ori dest dest (:apply ldb (:apply byte 16 0) intval)))))))


(define-ppc64-vinsn discard-temp-frame (()
					())
  (ld ppc::tsp 0 ppc::tsp))


;;; Somewhere, deep inside the "OS_X_PPC_RuntimeConventions.pdf"
;;; document, they bother to document the fact that SP should
;;; maintain 32-byte alignment on OSX.  (The example prologue
;;; code in that document incorrectly assumes 8-byte alignment.
;;; Or something.  It's wrong in a number of other ways.)
;;; The caller always has to reserve a 24-byte linkage area
;;; (large chunks of which are unused).
(define-ppc64-vinsn alloc-c-frame (()
				   ((n-c-args :u16const)))
  ;; Always reserve space for at least 8 args and space for a lisp
  ;; frame (for the kernel) underneath it.
  ;; Zero the c-frame's savelr field, not that the GC cares ..
  ((:pred <= n-c-args 10)
   (stdu ppc::sp (- (+ 16 ppc64::c-frame.size ppc64::lisp-frame.size)) ppc::sp))
  ((:pred > n-c-args 10)
   ;; A normal C frame has room for 10 args (when padded out to
   ;; 32-byte alignment. Add enough double words to accomodate the
   ;; remaining args, in multiples of 4.
   (stdu ppc::sp (:apply - (:apply +
                                   16
                                   (+ ppc64::c-frame.size ppc64::lisp-frame.size)
                                   (:apply ash
                                           (:apply logand
                                                   (lognot 7)
                                                   (:apply
                                                    +
                                                    7
                                                    (:apply - n-c-args 10)))
                                           3)))
         ppc::sp))
  (std ppc::rzero ppc64::c-frame.savelr ppc::sp))


(define-ppc64-vinsn alloc-variable-c-frame (()
                                            ((n-c-args :lisp))
                                            ((crf :crf)
                                             (size :s64)))
  (cmpdi crf n-c-args (ash 10 ppc64::fixnumshift))
  (subi size n-c-args (ash 10 ppc64::fixnumshift))
  (bgt :variable)
  ;; Always reserve space for at least 8 args and space for a lisp
  ;; frame (for the kernel) underneath it.
  (stdu ppc::sp (- (+ 16 ppc64::c-frame.size ppc64::lisp-frame.size)) ppc::sp)
  (b :done)
  :variable
  (addi size size (+  (+ 16 ppc64::c-frame.size ppc64::lisp-frame.size) (ash 3 ppc64::fixnumshift)))
  (clrrdi size size 4)
  (neg size size)
  (stdux ppc::sp ppc::sp size)
  :done
  (stw ppc::rzero ppc64::c-frame.savelr ppc::sp))

;;; We should rarely have to do this.  It's easier to just generate code
;;; to do the memory reference than it would be to keep track of the size
;;; of each frame.
(define-ppc64-vinsn discard-c-frame (()
				     ())
  (ld ppc::sp 0 ppc::sp))




(define-ppc64-vinsn set-c-arg (()
			       ((argval :u32)
				(argnum :u16const)))
  (std argval (:apply + ppc64::c-frame.param0 (:apply ash argnum ppc64::word-shift)) ppc::sp))

(define-ppc64-vinsn set-single-c-arg (()
				      ((argval :single-float)
				       (argnum :u16const)))
  (stfs argval (:apply + ppc64::c-frame.param0 4 (:apply ash argnum ppc64::word-shift)) ppc::sp))

(define-ppc64-vinsn set-double-c-arg (()
				      ((argval :double-float)
				       (argnum :u16const)))
  (stfd argval (:apply + ppc64::c-frame.param0 (:apply ash argnum ppc64::word-shift)) ppc::sp))

(define-ppc64-vinsn reload-single-c-arg (((argval :single-float))
					 ((argnum :u16const)))
  (lfs argval (:apply + ppc64::c-frame.param0 4 (:apply ash argnum ppc64::word-shift)) ppc::sp))

(define-ppc64-vinsn reload-single-c-arg-high (((argval :single-float))
                                              ((argnum :u16const)))
  (lfs argval (:apply + ppc64::c-frame.param0 (:apply ash argnum ppc64::word-shift)) ppc::sp))

(define-ppc64-vinsn reload-double-c-arg (((argval :double-float))
					 ((argnum :u16const)))
  (lfd argval (:apply + ppc64::c-frame.param0 (:apply ash argnum ppc64::word-shift)) ppc::sp))

(define-ppc64-vinsn (load-nil :constant-ref) (((dest t))
					      ())
  (li dest (:apply target-nil-value)))


(define-ppc64-vinsn (load-t :constant-ref) (((dest t))
					    ())
  (li dest (:apply + ppc64::t-offset (:apply target-nil-value))))

(define-ppc64-vinsn set-eq-bit (((dest :crf))
				())
  (creqv (:apply + ppc::ppc-eq-bit dest)
	 (:apply + ppc::ppc-eq-bit dest)
	 (:apply + ppc::ppc-eq-bit dest)))

(define-ppc64-vinsn (ref-constant :constant-ref) (((dest :lisp))
						  ((src :s16const)))
  (ld dest (:apply + ppc64::misc-data-offset (:apply ash (:apply 1+ src) 3)) ppc::fn))

(define-ppc64-vinsn ref-indexed-constant (((dest :lisp))
					  ((idxreg :s64)))
  (ldx dest ppc::fn idxreg))


(define-ppc64-vinsn cons (((dest :lisp))
			  ((newcar :lisp)
			   (newcdr :lisp)))
  (la ppc::allocptr (- ppc64::fulltag-cons ppc64::cons.size) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std newcdr ppc64::cons.cdr ppc::allocptr)
  (std newcar ppc64::cons.car ppc::allocptr)
  (mr dest ppc::allocptr)
  (rldicr ppc::allocptr ppc::allocptr 0 (- 63 ppc64::ntagbits)))



;;; subtag had better be a PPC-NODE-SUBTAG of some sort!
(define-ppc64-vinsn %ppc-gvector (((dest :lisp))
				  ((Rheader :u32) 
				   (nbytes :u32const))
				  ((immtemp0 :u32)
				   (nodetemp :lisp)
				   (crf :crf)))
  (la ppc::allocptr (:apply - ppc64::fulltag-misc
                            (:apply logand (lognot 15)
                                    (:apply + (+ 15 8) nbytes)))
      ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std Rheader ppc64::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (rldicr ppc::allocptr ppc::allocptr 0 (- 63 ppc64::ntagbits))
  ((:not (:pred = nbytes 0))
   (li immtemp0 (:apply + ppc64::misc-data-offset nbytes))
   :loop
   (subi immtemp0 immtemp0 8)
   (cmpdi crf immtemp0 ppc64::misc-data-offset)
   (ld nodetemp 0 ppc::vsp)
   (la ppc::vsp 8 ppc::vsp)
   (stdx nodetemp dest immtemp0)
   (bne crf :loop)))

;;; allocate a small (phys size <= 32K bytes) misc obj of known size/subtag
(define-ppc64-vinsn %alloc-misc-fixed (((dest :lisp))
				       ((Rheader :u64)
					(nbytes :u32const)))
  (la ppc::allocptr (:apply - ppc64::fulltag-misc
                            (:apply logand (lognot 15)
                                    (:apply + (+ 15 8) nbytes)))
      ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std Rheader ppc64::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (rldicr ppc::allocptr ppc::allocptr 0 (- 63 ppc64::ntagbits)))

(define-ppc64-vinsn vstack-discard (()
				    ((nwords :u32const)))
  ((:not (:pred = nwords 0))
   (la ppc::vsp (:apply ash nwords ppc64::word-shift) ppc::vsp)))


(define-ppc64-vinsn lcell-load (((dest :lisp))
				((cell :lcell)
				 (top :lcell)))
  (ld dest (:apply - 
		   (:apply - (:apply calc-lcell-depth top) ppc64::word-size-in-bytes)
		   (:apply calc-lcell-offset cell)) ppc::vsp))

(define-ppc64-vinsn vframe-load (((dest :lisp))
				 ((frame-offset :u16const)
				  (cur-vsp :u16const)))
  (ld dest (:apply - (:apply - cur-vsp ppc64::word-size-in-bytes) frame-offset) ppc::vsp))

(define-ppc64-vinsn lcell-store (()
				 ((src :lisp)
				  (cell :lcell)
				  (top :lcell)))
  (stw src (:apply - 
                   (:apply - (:apply calc-lcell-depth top) 4)
                   (:apply calc-lcell-offset cell)) ppc::vsp))

(define-ppc64-vinsn vframe-store (()
				  ((src :lisp)
				   (frame-offset :u16const)
				   (cur-vsp :u16const)))
  (std src (:apply - (:apply - cur-vsp 8) frame-offset) ppc::vsp))

(define-ppc64-vinsn load-vframe-address (((dest :imm))
					 ((offset :s16const)))
  (la dest offset ppc::vsp))

(define-ppc64-vinsn copy-lexpr-argument (()
					 ()
					 ((temp :lisp)))
  (ldx temp ppc::vsp ppc::nargs)
  (stdu temp -8 ppc::vsp))

;;; Boxing/unboxing of integers.

;;; Treat the low 8 bits of VAL as an unsigned integer; set RESULT to
;;; the equivalent fixnum.
(define-ppc64-vinsn u8->fixnum (((result :imm)) 
				((val :u8)) 
				())
  (clrlsldi result val (- ppc64::nbits-in-word 8) ppc64::fixnumshift))

;;; Treat the low 8 bits of VAL as a signed integer; set RESULT to the
;;; equivalent fixnum.
(define-ppc64-vinsn s8->fixnum (((result :imm)) 
				((val :s8)) 
				())
  (sldi result val (- ppc64::nbits-in-word 8))
  (sradi result result (- (- ppc64::nbits-in-word 8) ppc64::fixnumshift)))


;;; Treat the low 16 bits of VAL as an unsigned integer; set RESULT to
;;; the equivalent fixnum.
(define-ppc64-vinsn u16->fixnum (((result :imm)) 
				 ((val :u16)) 
				 ())
  (clrlsldi result val (- ppc64::nbits-in-word 16) ppc64::fixnumshift))

;;; Treat the low 16 bits of VAL as a signed integer; set RESULT to
;;; the equivalent fixnum.
(define-ppc64-vinsn s16->fixnum (((result :imm)) 
				 ((val :s16)) 
				 ())
  (sldi result val (- ppc64::nbits-in-word 16))
  (sradi result result (- (- ppc64::nbits-in-word 16) ppc64::fixnumshift)))

(define-ppc64-vinsn fixnum->s16 (((result :s16))
				 ((src :imm)))
  (sradi result src ppc64::fixnumshift))

(define-ppc64-vinsn s32->integer (((result :lisp))
                                  ((src :s32))
                                  ((temp :s64)))
  (extsw temp src)
  (sldi result temp ppc64::fixnumshift))


;;; A signed 64-bit untagged value can be at worst a 2-digit
;;; (minimal-sized) bignum.  There should be something very much like
;;; this that takes a stack-consed bignum result ...
(define-ppc64-vinsn s64->integer (((result :lisp))
				  ((src :s64))
				  ((crf (:crf 0)) ; a casualty
				   (temp :s64)
                                   (header :s64)))
  (addo temp src src)
  (addo temp temp temp)
  (addo. result temp temp)
  (rotldi temp src 32)
  (bns+ :done)
  (mtxer ppc::rzero)
  (li header ppc64::two-digit-bignum-header)
  (la ppc::allocptr (- ppc64::fulltag-misc 16) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (rldicr ppc::allocptr ppc::allocptr 0 (- 63 ppc64::ntagbits))
  (std temp ppc64::misc-data-offset result)
  :done)


;;; An unsigned 32-bit untagged value is a fixnum.
(define-ppc64-vinsn u32->integer (((result :lisp))
				  ((src :u32)))
  (sldi result src ppc64::fixnumshift))

;;; An unsigned 64-bit untagged value is either a fixnum, a 2 (32-bit)
;;; digit bignum, or a 3 (32-bit) digit bignum.
(define-ppc64-vinsn u64->integer (((result :lisp))
                                  ((src :u64))
                                  ((temp :u64)
                                   (header :u64)
                                   (crf0 (:crf 0))
                                   (crf1 :crf)))
  (clrrdi. temp src (- 63 ppc64::nfixnumtagbits))
  (cmpdi crf1 src 0)
  (sldi result src ppc64::fixnumshift)
  (beq crf0 :done)
  (rotldi temp src 32)
  (li header ppc64::two-digit-bignum-header)
  (blt crf1 :three)
  (la ppc::allocptr (- ppc64::fulltag-misc 16) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (rldicr ppc::allocptr ppc::allocptr 0 (- 63 ppc64::ntagbits))
  (b :store)
  :three
  (la ppc::allocptr (- ppc64::fulltag-misc 32) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (rldicr ppc::allocptr ppc::allocptr 0 (- 63 ppc64::ntagbits))
  :store
  (std temp ppc64::misc-data-offset result)
  :done)

(define-ppc64-vinsn u16->u32 (((dest :u32))
			      ((src :u16)))
  (clrlwi dest src 16))

(define-ppc64-vinsn u8->u32 (((dest :u32))
			     ((src :u8)))
  (clrlwi dest src 24))


(define-ppc64-vinsn s16->s32 (((dest :s32))
			      ((src :s16)))
  (extsh dest src))

(define-ppc64-vinsn s8->s32 (((dest :s32))
			     ((src :s8)))
  (extsb dest src))


;;; ... of floats ...

;;; Heap-cons a double-float to store contents of FPREG.  Hope that we
;;; don't do this blindly.
(define-ppc64-vinsn double->heap (((result :lisp)) ; tagged as a double-float
				  ((fpreg :double-float)) 
				  ((header-temp :u32)))
  (li header-temp (arch::make-vheader ppc64::double-float.element-count ppc64::subtag-double-float))
  (la ppc::allocptr (- ppc64::fulltag-misc ppc64::double-float.size) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header-temp ppc64::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (rldicr ppc::allocptr ppc::allocptr 0 (- 63 ppc64::ntagbits))
  (stfd fpreg ppc64::double-float.value result)  )


(define-ppc64-vinsn single->node (((result :lisp)) ; tagged as a single-float
				  ((fpreg :single-float)))
  (stfs fpreg ppc64::tcr.single-float-convert ppc64::rcontext)
  (ld result  ppc64::tcr.single-float-convert ppc64::rcontext))


;;; "dest" is preallocated, presumably on a stack somewhere.
(define-ppc64-vinsn store-double (()
				  ((dest :lisp)
				   (source :double-float))
				  ())
  (stfd source ppc64::double-float.value dest))

(define-ppc64-vinsn get-double (((target :double-float))
				((source :lisp))
				())
  (lfd target ppc64::double-float.value source))

;;; Extract a double-float value, typechecking in the process.
;;; IWBNI we could simply call the "trap-unless-typecode=" vinsn here,
;;; instead of replicating it ..

(define-ppc64-vinsn get-double? (((target :double-float))
				 ((source :lisp))
				 ((tag :u8)
				  (crf :crf)))
  (clrldi tag source (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne crf :do-trap)
  (lbz tag ppc64::misc-subtag-offset source)
  :do-trap
  (tdnei tag ppc64::subtag-double-float)
  (lfd target ppc64::double-float.value source))

(define-ppc64-vinsn double-to-single (((result :single-float))
                                       ((arg :double-float)))
  (frsp result arg))


(define-ppc64-vinsn store-single (()
				  ((dest :lisp)
				   (source :single-float))
				  ())
  (stfs source ppc64::tcr.single-float-convert ppc64::rcontext)
  (ld dest ppc64::tcr.single-float-convert ppc64::rcontext))

(define-ppc64-vinsn get-single (((target :single-float))
				((source :lisp)))
  (std source ppc64::tcr.single-float-convert ppc64::rcontext)
  (lfs target ppc64::tcr.single-float-convert ppc64::rcontext))

;;; ... of characters ...
(define-ppc64-vinsn charcode->u16 (((dest :u16))
				   ((src :imm))
				   ())
  (srdi dest src ppc64::charcode-shift))

(define-ppc64-vinsn character->fixnum (((dest :lisp))
				       ((src :lisp))
				       ())
  (srdi dest src (- ppc64::charcode-shift ppc64::fixnumshift)))

(define-ppc64-vinsn character->code (((dest :u32))
				     ((src :lisp)))
  (srdi dest src ppc64::charcode-shift))


(define-ppc64-vinsn fixnum->char (((dest :lisp))
				  ((src :imm))
				  ((temp :u64)
                                   (crf0 (:crf 0))))
  (srdi temp src (+ ppc64::fixnumshift 1))
  (cmpldi temp (ash #xffff -1))
  (srdi temp src (+ ppc64::fixnumshift 11))
  (beq :bad)
  (cmpdi temp 27)
  (sldi dest src (- ppc64::charcode-shift ppc64::fixnumshift))
  (bne+ :ok)
  :bad
  (li dest (:apply target-nil-value))
  (b :done)
  :ok
  (addi dest dest ppc64::subtag-character)
  :done)

(define-ppc64-vinsn code-char->char (((dest :lisp))
				     ((src :imm))
                               ())
  (sldi dest src (- ppc64::charcode-shift ppc64::fixnumshift))
  (ori dest dest ppc64::subtag-character))


(define-ppc64-vinsn u32->char (((dest :lisp))
			      ((src :u32))
                               ())
  (sldi dest src ppc64::charcode-shift)
  (ori dest dest ppc64::subtag-character))

;;; ... Macptrs ...

(define-ppc64-vinsn deref-macptr (((addr :address))
				  ((src :lisp))
				  ())
  (ld addr ppc64::macptr.address src))

(define-ppc64-vinsn set-macptr-address (()
					((addr :address)
					 (src :lisp))
					())
  (std addr ppc64::macptr.address src))


(define-ppc64-vinsn macptr->heap (((dest :lisp))
				  ((address :address))
				  ((header :u64)))
  (li header (logior (ash ppc64::macptr.element-count ppc64::num-subtag-bits) ppc64::subtag-macptr))
  (la ppc::allocptr (- ppc64::fulltag-misc ppc64::macptr.size) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (rldicr ppc::allocptr ppc::allocptr 0 (- 63 ppc64::ntagbits))
  ;; It's not necessary to zero out the domain/type fields, since newly
  ;; heap-allocated memory's guaranteed to be 0-filled.
  (std address ppc64::macptr.address dest))

(define-ppc64-vinsn macptr->stack (((dest :lisp))
				   ((address :address))
				   ((header :u64)))
  (li header ppc64::macptr-header)
  (stdu ppc::tsp (- (+ 16 ppc64::macptr.size)) ppc::tsp)
  (std ppc::tsp 8 ppc::tsp)
  (std header (+ 16 ppc64::fulltag-misc ppc64::macptr.header) ppc::tsp)
  (std address (+ 16 ppc64::fulltag-misc ppc64::macptr.address) ppc::tsp)
  ;; It -is- necessary to zero out the domain/type fields here, since
  ;; stack-allocated memory isn't guaranteed to be 0-filled.
  (std ppc::rzero (+ 16 ppc64::fulltag-misc ppc64::macptr.domain) ppc::tsp)
  (std ppc::rzero (+ 16 ppc64::fulltag-misc ppc64::macptr.type) ppc::tsp)
  (la dest (+ 16 ppc64::fulltag-misc) ppc::tsp))

  
(define-ppc64-vinsn adjust-stack-register (()
					   ((reg t)
					    (amount :s16const)))
  (la reg amount reg))

(define-ppc64-vinsn adjust-vsp (()
				((amount :s16const)))
  (la ppc::vsp amount ppc::vsp))

(define-ppc64-vinsn adjust-sp (()
                               ((amount :s16const)))
  (la ppc::sp amount ppc::sp))

;;; Arithmetic on fixnums & unboxed numbers

(define-ppc64-vinsn u64-lognot (((dest :u64))
				((src :u64))
				())
  (not dest src))

(define-ppc64-vinsn fixnum-lognot (((dest :imm))
				   ((src :imm))
				   ((temp :u64)))
  (not temp src)
  (rldicr dest temp 0 (- 63 ppc64::nfixnumtagbits)))


(define-ppc64-vinsn negate-fixnum-overflow-inline (((dest :lisp))
						   ((src :imm))
						   ((unboxed :s64)
						    (header :u64)))
  (nego. dest src)
  (bns+ :done)
  (mtxer ppc::rzero)
  (sradi unboxed dest ppc64::fixnumshift)
  (li header ppc64::two-digit-bignum-header)
  (rotldi unboxed unboxed 32)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc64::fixnumshift))))
  (la ppc::allocptr (- ppc64::fulltag-misc 16) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrdi ppc::allocptr ppc::allocptr ppc64::ntagbits)
  (std unboxed ppc64::misc-data-offset dest)
  :done)

(define-ppc64-vinsn negate-fixnum-overflow-ool (()
						((src :imm))
						)
  (nego. ppc::arg_z src)
  (bsola- .SPfix-overflow)
  :done)
  
                                                  
                                       
(define-ppc64-vinsn negate-fixnum-no-ovf (((dest :lisp))
					  ((src :imm)))
  
  (neg dest src))
  

(define-ppc64-vinsn logior-high (((dest :imm))
				 ((src :imm)
				  (high :u16const)))
  (oris dest src high))

(define-ppc64-vinsn logior-low (((dest :imm))
				((src :imm)
				 (low :u16const)))
  (ori dest src low))

                           
                           
(define-ppc64-vinsn %logior2 (((dest :imm))
			      ((x :imm)
			       (y :imm))
			      ())
  (or dest x y))

(define-ppc64-vinsn logand-high (((dest :imm))
				 ((src :imm)
				  (high :u16const))
				 ((crf0 (:crf 0))))
  (andis. dest src high))

(define-ppc64-vinsn logand-low (((dest :imm))
				((src :imm)
				 (low :u16const))
				((crf0 (:crf 0))))
  (andi. dest src low))


(define-ppc64-vinsn %logand2 (((dest :imm))
			      ((x :imm)
			       (y :imm))
			      ())
  (and dest x y))

(define-ppc64-vinsn clear-left (((dest :imm))
                                ((src :imm)
                                 (nbits :s8const)))
  (rldicl dest src 0 (:apply 1+ nbits)))

(define-ppc64-vinsn clear-right (((dest :imm))
                                 ((src :imm)
                                  (nbits :s8const)))
  (rldicr dest src 0 (:apply - 63 nbits)))

(define-ppc64-vinsn logxor-high (((dest :imm))
				 ((src :imm)
				  (high :u16const)))
  (xoris dest src high))

(define-ppc64-vinsn logxor-low (((dest :imm))
				((src :imm)
				 (low :u16const)))
  (xori dest src low))

                           

(define-ppc64-vinsn %logxor2 (((dest :imm))
			      ((x :imm)
			       (y :imm))
			      ())
  (xor dest x y))

(define-ppc64-vinsn %ilsl (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((temp :u32)
			    (crx :crf)))
  (cmpdi crx count (ash 63 ppc64::fixnumshift))
  (srdi temp count ppc64::fixnumshift)
  (sld dest src temp)
  (ble+ crx :foo)
  (li dest 0)
  :foo)

(define-ppc64-vinsn %ilsl-c (((dest :imm))
			     ((count :u8const)
			      (src :imm)))
  ;; Hard to use ppcmacroinstructions that expand into expressions
  ;; involving variables.
  (rldicr dest src count (:apply - ppc64::least-significant-bit count)))


(define-ppc64-vinsn %ilsr-c (((dest :imm))
			     ((count :u8const)
			      (src :imm))
                             ((temp :s64)))
  (rldicl temp src (:apply - 64 count) count)
  (rldicr dest temp 0 (- 63 ppc64::fixnumshift)))



;;; 68k did the right thing for counts < 64 - fixnumshift but not if greater
;;; so load-byte fails in 3.0 also


(define-ppc64-vinsn %iasr (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((temp :s32)
			    (crx :crf)))
  (cmpdi crx count (ash 63 ppc64::fixnumshift))
  (sradi temp count ppc64::fixnumshift)
  (srad temp src temp)
  (ble+ crx :foo)
  (sradi temp src 63)
  :foo
  (rldicr dest temp 0 (- 63 ppc64::fixnumshift)))

(define-ppc64-vinsn %iasr-c (((dest :imm))
			     ((count :u8const)
			      (src :imm))
			     ((temp :s32)))
  (sradi temp src count)
  (rldicr dest temp 0 (- 63 ppc64::fixnumshift)))

(define-ppc64-vinsn %ilsr (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((temp :s32)
			    (crx :crf)))
  (cmpdi crx count (ash 63 ppc64::fixnumshift))
  (srdi temp count ppc64::fixnumshift)
  (srd temp src temp)
  (rldicr dest temp 0 (- 63 ppc64::fixnumshift))
  (ble+ crx :foo)
  (li dest 0)
  :foo  
  )

(define-ppc64-vinsn natural-shift-left (((dest :u64))
                                        ((src :u64)
                                         (count :u8const)))
  (rldicr dest src count  (:apply - 63 count)))

(define-ppc64-vinsn natural-shift-right (((dest :u64))
                                         ((src :u64)
                                          (count :u8const)))
  (rldicr dest src (:apply - 64 count) count))

(define-ppc64-vinsn sign-extend-halfword (((dest :imm))
					  ((src :imm)))
  (sldi dest src (- 48 ppc64::fixnumshift))
  (sradi dest dest (- 48 ppc64::fixnumshift)))



(define-ppc64-vinsn fixnum-add (((dest t))
				((x t)
				 (y t)))
  (add dest x y))


(define-ppc64-vinsn fixnum-add-overflow-ool (()
					     ((x :imm)
					      (y :imm))
					     ((cr0 (:crf 0))))
  (addo. ppc::arg_z x y)
  (bsola- .SPfix-overflow))

(define-ppc64-vinsn fixnum-add-overflow-inline (((dest :lisp))
						((x :imm)
						 (y :imm))
						((cr0 (:crf 0))
						 (unboxed :s64)
						 (header :u64)))
  (addo. dest x y)
  (bns+ cr0 :done)
  (mtxer ppc::rzero)
  (sradi unboxed dest ppc64::fixnumshift)
  (li header ppc64::two-digit-bignum-header)
  (rotldi unboxed unboxed 32)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc64::fixnumshift))))
  (la ppc::allocptr (- ppc64::fulltag-misc 16) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrdi ppc::allocptr ppc::allocptr ppc64::ntagbits)
  (std unboxed ppc64::misc-data-offset dest)
  :done)

(define-ppc64-vinsn fixnum-add-overflow-inline-skip (((dest :lisp))
                                                     ((x :imm)
                                                      (y :imm)
                                                      (done :label))
                                                     ((cr0 (:crf 0))
                                                      (unboxed :s64)
                                                      (header :u64)))
  (addo. dest x y)
  (bns+ cr0 done)
  (mtxer ppc::rzero)
  (sradi unboxed dest ppc64::fixnumshift)
  (li header ppc64::two-digit-bignum-header)
  (rotldi unboxed unboxed 32)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc64::fixnumshift))))
  (la ppc::allocptr (- ppc64::fulltag-misc 16) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrdi ppc::allocptr ppc::allocptr ppc64::ntagbits)
  (std unboxed ppc64::misc-data-offset dest)
  (b done))
  

  

;;;  (setq dest (- x y))
(define-ppc64-vinsn fixnum-sub (((dest t))
				((x t)
				 (y t)))
  (subf dest y x))

(define-ppc64-vinsn fixnum-sub-from-constant (((dest :imm))
					      ((x :s16const)
					       (y :imm)))
  (subfic dest y (:apply ash x ppc64::fixnumshift)))




(define-ppc64-vinsn fixnum-sub-overflow-ool (()
					     ((x :imm)
					      (y :imm)))
  (subo. ppc::arg_z x y)
  (bsola- .SPfix-overflow))

(define-ppc64-vinsn fixnum-sub-overflow-inline (((dest :lisp))
						((x :imm)
						 (y :imm))
						((cr0 (:crf 0))
						 (unboxed :s64)
						 (header :u64)))
  (subo. dest x y)
  (bns+ cr0 :done)
  (mtxer ppc::rzero)
  (sradi unboxed dest ppc64::fixnumshift)
  (li header ppc64::two-digit-bignum-header)
  (rotldi unboxed unboxed 32)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc64::fixnumshift))))
  (la ppc::allocptr (- ppc64::fulltag-misc 16) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrdi ppc::allocptr ppc::allocptr ppc64::ntagbits)
  (std unboxed ppc64::misc-data-offset dest)
  :done)

(define-ppc64-vinsn fixnum-sub-overflow-inline-skip (((dest :lisp))
                                                     ((x :imm)
                                                      (y :imm)
                                                      (done :label))
                                                     ((cr0 (:crf 0))
                                                      (unboxed :s64)
                                                      (header :u64)))
  (subo. dest x y)
  (bns+ cr0 done)
  (mtxer ppc::rzero)
  (sradi unboxed dest ppc64::fixnumshift)
  (li header ppc64::two-digit-bignum-header)
  (rotldi unboxed unboxed 32)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 ppc64::fixnumshift))))
  (la ppc::allocptr (- ppc64::fulltag-misc 16) ppc::allocptr)
  (tdlt ppc::allocptr ppc::allocbase)
  (std header ppc64::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrdi ppc::allocptr ppc::allocptr ppc64::ntagbits)
  (std unboxed ppc64::misc-data-offset dest)
  (b done))

;;; This is, of course, also "subtract-immediate."
(define-ppc64-vinsn add-immediate (((dest t))
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
(define-ppc64-vinsn multiply-fixnums (((dest :imm))
				      ((a :imm)
				       (b :imm))
				      ((unboxed :s32)))
  (sradi unboxed b ppc64::fixnumshift)
  (mulld dest a unboxed))

(define-ppc64-vinsn multiply-immediate (((dest :imm))
					((boxed :imm)
					 (const :s16const)))
  (mulli dest boxed const))

;;; Mask out the code field of a base character; the result
;;; should be EXACTLY = to subtag-base-char
(define-ppc64-vinsn mask-base-char (((dest :u32))
				    ((src :imm)))
  (clrldi dest src (- ppc64::nbits-in-word ppc64::num-subtag-bits)))

;;; Set dest (of type :s64!) to 0 iff VAL is an istruct of type TYPE
(define-ppc64-vinsn istruct-typep (((dest :s64))
                                   ((val :lisp)
                                    (type :lisp))
                                   ((crf :crf)
                                    (temp :lisp)))
  (clrldi dest val (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf dest ppc64::fulltag-misc)
  (li dest -1)
  (bne crf :done)
  (lbz dest ppc64::misc-subtag-offset val)
  (cmpdi crf dest ppc64::subtag-istruct)
  (bne crf :done)
  (ld temp ppc64::misc-data-offset val)
  (subf dest type temp)
  :done)
                             
;;; Boundp, fboundp stuff.
(define-ppc64-vinsn (ref-symbol-value :call :subprim-call)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (bla .SPspecrefcheck))

(define-ppc64-vinsn ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((table :imm)
                                               (idx :imm)))
  (ld idx ppc64::symbol.binding-index src)
  (ld table ppc64::tcr.tlb-limit ppc64::rcontext)
  (cmpd idx table)
  (ld table ppc64::tcr.tlb-pointer ppc64::rcontext)
  (bge :symbol)
  (ldx dest table idx)
  (cmpdi dest ppc64::subtag-no-thread-local-binding)
  (bne :done)
  :symbol
  (ld dest ppc64::symbol.vcell src)
  :done
  (tdeqi dest ppc64::unbound-marker))

(define-ppc64-vinsn (%ref-symbol-value :call :subprim-call)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (bla .SPspecref))

(define-ppc64-vinsn %ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((table :imm)
                                               (idx :imm)))
  (ld idx ppc64::symbol.binding-index src)
  (ld table ppc64::tcr.tlb-limit ppc64::rcontext)
  (cmpd idx table)
  (ld table ppc64::tcr.tlb-pointer ppc64::rcontext)
  (bge :symbol)
  (ldx dest table idx)
  (cmpdi dest ppc64::subtag-no-thread-local-binding)
  (bne :done)
  :symbol
  (ld dest ppc64::symbol.vcell src)
  :done
  )

(define-ppc64-vinsn (setq-special :call :subprim-call)
    (()
     ((sym :lisp)
      (val :lisp)))
  (bla .SPspecset))


(define-ppc64-vinsn symbol-function (((val :lisp))
				     ((sym (:lisp (:ne val))))
				     ((crf :crf)
				      (tag :u32)))
  (ld val ppc64::symbol.fcell sym)
  (clrldi tag val (- 64 ppc64::ntagbits))
  (cmpdi crf tag ppc64::fulltag-misc)
  (bne- crf :bad)
  (lbz tag ppc64::misc-subtag-offset val)
  (cmpdi crf tag ppc64::subtag-function)
  (beq+ crf :good)
  :bad 
  (uuo_interr arch::error-udf sym)
  :good)

(define-ppc64-vinsn (temp-push-unboxed-word :push :word :tsp)
    (()
     ((w :u64)))
  (stdu ppc::tsp -32 ppc::tsp)
  (std ppc::tsp 8 ppc::tsp)
  (std w 16 ppc::tsp))

(define-ppc64-vinsn (temp-pop-unboxed-word :pop :word :tsp)
    (((w :u64))
     ())
  (ld w 16 ppc::tsp)
  (la ppc::tsp 32 ppc::tsp))

(define-ppc64-vinsn (temp-push-double-float :push :doubleword :tsp)
    (((d :double-float))
     ())
  (stdu ppc::tsp -32 ppc::tsp)
  (std ppc::tsp 8 ppc::tsp)
  (stfd d 16 ppc::tsp))

(define-ppc64-vinsn (temp-pop-double-float :pop :doubleword :tsp)
    (()
     ((d :double-float)))
  (lfd d 16 ppc::tsp)
  (la ppc::tsp 32 ppc::tsp))

(define-ppc64-vinsn (temp-push-single-float :push :word :tsp)
    (((s :single-float))
     ())
  (stdu ppc::tsp -32 ppc::tsp)
  (std ppc::tsp 8 ppc::tsp)
  (stfs s 16 ppc::tsp))

(define-ppc64-vinsn (temp-pop-single-float :pop :word :tsp)
    (()
     ((s :single-float)))
  (lfs s 16 ppc::tsp)
  (la ppc::tsp 32 ppc::tsp))


(define-ppc64-vinsn (save-nvrs-individually :push :node :vsp :multiple)
    (()
     ((first :u8const)))
  (stdu ppc::save0 -8 ppc::vsp)
  ((:pred <= first ppc::save1)
   (stdu ppc::save1 -8 ppc::vsp)
   ((:pred <= first ppc::save2)
    (stdu ppc::save2 -8 ppc::vsp)
    ((:pred <= first ppc::save3)
     (stdu ppc::save3 -8 ppc::vsp)
     ((:pred <= first ppc::save4)
      (stdu ppc::save4 -8 ppc::vsp)
      ((:pred <= first ppc::save5)
       (stdu ppc::save5 -8 ppc::vsp)
       ((:pred <= first ppc::save6)
	(stdu ppc::save6 -8 ppc::vsp)
	((:pred = first ppc::save7)
	 (stdu ppc::save7 -8 ppc::vsp)))))))))

(define-ppc64-vinsn (save-nvrs :push :node :vsp :multiple)
    (()
     ((first :u8const)))
  ;; There's no "stmd" instruction.
  (stdu ppc::save0 -8 ppc::vsp)
  ((:pred <= first ppc::save1)
   (stdu ppc::save1 -8 ppc::vsp)
   ((:pred <= first ppc::save2)
    (stdu ppc::save2 -8 ppc::vsp)
    ((:pred <= first ppc::save3)
     (stdu ppc::save3 -8 ppc::vsp)
     ((:pred <= first ppc::save4)
      (stdu ppc::save4 -8 ppc::vsp)
      ((:pred <= first ppc::save5)
       (stdu ppc::save5 -8 ppc::vsp)
       ((:pred <= first ppc::save6)
	(stdu ppc::save6 -8 ppc::vsp)
	((:pred = first ppc::save7)
	 (stdu ppc::save7 -8 ppc::vsp)))))))))


(define-ppc64-vinsn (restore-nvrs :pop :node :vsp :multiple)
    (()
     ((firstreg :u8const)
      (basereg :imm)
      (offset :s16const)))
  ((:pred = firstreg ppc::save7)
   (ld ppc::save7 offset basereg)
   (ld ppc::save6 (:apply + offset 8) basereg)
   (ld ppc::save5 (:apply + offset 16) basereg)
   (ld ppc::save4 (:apply + offset 24) basereg)
   (ld ppc::save3 (:apply + offset 32) basereg)
   (ld ppc::save2 (:apply + offset 40) basereg)
   (ld ppc::save1 (:apply + offset 48) basereg)
   (ld ppc::save0 (:apply + offset 56) basereg))
  ((:pred = firstreg ppc::save6)
   (ld ppc::save6 offset basereg)
   (ld ppc::save5 (:apply + offset 8) basereg)
   (ld ppc::save4 (:apply + offset 16) basereg)
   (ld ppc::save3 (:apply + offset 24) basereg)
   (ld ppc::save2 (:apply + offset 32) basereg)
   (ld ppc::save1 (:apply + offset 40) basereg)
   (ld ppc::save0 (:apply + offset 48) basereg))
  ((:pred = firstreg ppc::save5)
   (ld ppc::save5 offset basereg)
   (ld ppc::save4 (:apply + offset 8) basereg)
   (ld ppc::save3 (:apply + offset 16) basereg)
   (ld ppc::save2 (:apply + offset 24) basereg)
   (ld ppc::save1 (:apply + offset 32) basereg)
   (ld ppc::save0 (:apply + offset 40) basereg))
  ((:pred = firstreg ppc::save4)
   (ld ppc::save4 offset basereg)
   (ld ppc::save3 (:apply + offset 8) basereg)
   (ld ppc::save2 (:apply + offset 16) basereg)
   (ld ppc::save1 (:apply + offset 24) basereg)
   (ld ppc::save0 (:apply + offset 32) basereg))
  ((:pred = firstreg ppc::save3)
   (ld ppc::save3 offset basereg)
   (ld ppc::save2 (:apply + offset 8) basereg)
   (ld ppc::save1 (:apply + offset 16) basereg)
   (ld ppc::save0 (:apply + offset 24) basereg))
  ((:pred = firstreg ppc::save2)
   (ld ppc::save2 offset basereg)
   (ld ppc::save1 (:apply + offset 8) basereg)
   (ld ppc::save0 (:apply + offset 16) basereg))
  ((:pred = firstreg ppc::save1)
   (ld ppc::save1 offset basereg)
   (ld ppc::save0 (:apply + offset 8) basereg))
  ((:pred = firstreg ppc::save0)
   (ld ppc::save0 offset basereg)))

(define-ppc64-vinsn %current-frame-ptr (((dest :imm))
					())
  (mr dest ppc::sp))

(define-ppc64-vinsn %current-tcr (((dest :imm))
				  ())
  (mr dest ppc64::rcontext))

(define-ppc64-vinsn (dpayback :call :subprim-call) (()
                                                    ((n :s16const))
                                                    ((temp (:u32 #.ppc::imm0))))
  ((:pred > n 1)
   (li temp n)
   (bla .SPunbind-n))
  ((:pred = n 1)
   (bla .SPunbind)))

(define-ppc64-vinsn zero-double-float-register 
    (((dest :double-float))
     ())
  (fmr dest ppc::fp-zero))

(define-ppc64-vinsn zero-single-float-register 
    (((dest :single-float))
     ())
  (fmr dest ppc::fp-zero))

(define-ppc64-vinsn load-double-float-constant
    (((dest :double-float))
     ((high :u32)
      (low :u32)))
  (stw high -8 ppc::sp)
  (stw low -4 ppc::sp)
  (lfd dest -8 ppc::sp))

(define-ppc64-vinsn load-single-float-constant
    (((dest :single-float))
     ((src t)))
  (stw src -4 ppc::sp)
  (lfs dest -4 ppc::sp))

(define-ppc64-vinsn load-indexed-node (((node :lisp))
				       ((base :lisp)
					(offset :s16const)))
  (ld node offset base))

(define-ppc64-vinsn recover-saved-vsp (((dest :imm))
				       ())
  (ld dest ppc64::lisp-frame.savevsp ppc::sp))


(define-ppc64-vinsn check-exact-nargs (()
				       ((n :u16const)))
  (tdnei ppc::nargs (:apply ash n ppc64::word-shift)))

(define-ppc64-vinsn check-min-nargs (()
				     ((min :u16const)))
  (tdllti ppc::nargs (:apply ash min ppc64::word-shift)))

(define-ppc64-vinsn check-max-nargs (()
				     ((max :u16const)))
  (tdlgti ppc::nargs (:apply ash max ppc64::word-shift)))

;;; Save context and establish FN.  The current VSP is the the
;;; same as the caller's, e.g., no arguments were vpushed.
(define-ppc64-vinsn save-lisp-context-vsp (()
					   ()
					   ((imm :u64)))
  (stdu ppc::sp (- ppc64::lisp-frame.size) ppc::sp)
  (std ppc::fn ppc64::lisp-frame.savefn ppc::sp)
  (std ppc::loc-pc ppc64::lisp-frame.savelr ppc::sp)
  (std ppc::vsp ppc64::lisp-frame.savevsp ppc::sp)
  (mr ppc::fn ppc::nfn)
  ;; Do a stack-probe ...
  (ld imm ppc64::tcr.cs-limit ppc64::rcontext)
  (tdllt ppc::sp imm))

;;; Do the same thing via a subprim call.
(define-ppc64-vinsn (save-lisp-context-vsp-ool :call :subprim-call)
    (()
     ()
     ((imm (:u64 #.ppc::imm0))))
  (bla .SPsavecontextvsp))

(define-ppc64-vinsn save-lisp-context-offset (()
					      ((nbytes-vpushed :u16const))
					      ((imm :u64)))
  (la imm nbytes-vpushed ppc::vsp)
  (stdu ppc::sp (- ppc64::lisp-frame.size) ppc::sp)
  (std ppc::fn ppc64::lisp-frame.savefn ppc::sp)
  (std ppc::loc-pc ppc64::lisp-frame.savelr ppc::sp)
  (std imm ppc64::lisp-frame.savevsp ppc::sp)
  (mr ppc::fn ppc::nfn)
  ;; Do a stack-probe ...
  (ld imm ppc64::tcr.cs-limit ppc64::rcontext)
  (tdllt ppc::sp imm))

(define-ppc64-vinsn save-lisp-context-offset-ool (()
						  ((nbytes-vpushed :u16const))
						  ((imm (:u64 #.ppc::imm0))))
  (li imm nbytes-vpushed)
  (bla .SPsavecontext0))


(define-ppc64-vinsn save-lisp-context-lexpr (()
					     ()
					     ((imm :u64)))
  (stdu ppc::sp (- ppc64::lisp-frame.size) ppc::sp)
  (std ppc::rzero ppc64::lisp-frame.savefn ppc::sp)
  (std ppc::loc-pc ppc64::lisp-frame.savelr ppc::sp)
  (std ppc::vsp ppc64::lisp-frame.savevsp ppc::sp)
  (mr ppc::fn ppc::nfn)
  ;; Do a stack-probe ...
  (ld imm ppc64::tcr.cs-limit ppc64::rcontext)
  (tdllt ppc::sp imm))
  
(define-ppc64-vinsn save-cleanup-context (()
					  ())
  ;; SP was this deep just a second ago, so no need to do a stack-probe.
  (mflr ppc::loc-pc)
  (stdu ppc::sp (- ppc64::lisp-frame.size) ppc::sp)
  (std ppc::rzero ppc64::lisp-frame.savefn ppc::sp)
  (std ppc::loc-pc ppc64::lisp-frame.savelr ppc::sp)
  (std ppc::vsp ppc64::lisp-frame.savevsp ppc::sp))

;;; Vpush the argument registers.  We got at least "min-fixed" args;
;;; that knowledge may help us generate better code.
(define-ppc64-vinsn (save-lexpr-argregs :call :subprim-call)
    (()
     ((min-fixed :u16const))
     ((crfx :crf)
      (crfy :crf)
      (entry-vsp (:u64 #.ppc::imm0))
      (arg-temp :u64)))
  ((:pred >= min-fixed $numppcargregs)
   (stdu ppc::arg_x -8 ppc::vsp)
   (stdu ppc::arg_y -8 ppc::vsp)
   (stdu ppc::arg_z -8 ppc::vsp))
  ((:pred = min-fixed 2)                ; at least 2 args
   (cmpldi crfx ppc::nargs (ash 2 ppc64::word-shift))
   (beq crfx :yz2)                      ; skip arg_x if exactly 2
   (stdu ppc::arg_x -8 ppc::vsp)
   :yz2
   (stdu ppc::arg_y -8 ppc::vsp)
   (stdu ppc::arg_z -8 ppc::vsp))
  ((:pred = min-fixed 1)                ; at least one arg
   (cmpldi crfx ppc::nargs (ash 2 ppc64::word-shift))
   (blt crfx :z1)                       ; branch if exactly one
   (beq crfx :yz1)                      ; branch if exactly two
   (stdu ppc::arg_x -8 ppc::vsp)
   :yz1
   (stdu ppc::arg_y -8 ppc::vsp)
   :z1
   (stdu ppc::arg_z -8 ppc::vsp))
  ((:pred = min-fixed 0)
   (cmpldi crfx ppc::nargs (ash 2 ppc64::word-shift))
   (cmpldi crfy ppc::nargs 0)
   (beq crfx :yz0)                      ; exactly two
   (beq crfy :none)                     ; exactly zero
   (blt crfx :z0)                       ; one
                                        ; Three or more ...
   (stdu ppc::arg_x -8 ppc::vsp)
   :yz0
   (stdu ppc::arg_y -8 ppc::vsp)
   :z0
   (stdu ppc::arg_z -8 ppc::vsp)
   :none
   )
  ((:pred = min-fixed 0)
   (stdu ppc::nargs -8 ppc::vsp))
  ((:not (:pred = min-fixed 0))
   (subi arg-temp ppc::nargs (:apply ash min-fixed ppc64::word-shift))
   (stdu arg-temp -8 ppc::vsp))
  (add entry-vsp ppc::vsp ppc::nargs)
  (la entry-vsp 8 entry-vsp)
  (bla .SPlexpr-entry))


(define-ppc64-vinsn (jump-return-pc :jumpLR)
    (()
     ())
  (blr))

(define-ppc64-vinsn (restore-full-lisp-context :lispcontext :pop :csp :lrRestore)
    (()
     ())
  (ld ppc::loc-pc ppc64::lisp-frame.savelr ppc::sp)
  (ld ppc::vsp ppc64::lisp-frame.savevsp ppc::sp)
  (ld ppc::fn ppc64::lisp-frame.savefn ppc::sp)
  (mtlr ppc::loc-pc)
  (la ppc::sp ppc64::lisp-frame.size ppc::sp))

(define-ppc64-vinsn (restore-full-lisp-context-ool :lispcontext :pop :csp :lrRestore)
    (()
     ())
  (bla .SPrestorecontext)
  (mtlr ppc::loc-pc))

(define-ppc64-vinsn (popj :lispcontext :pop :csp :lrRestore :jumpLR)
    (() 
     ())
  (ba .SPpopj))

;;; Exiting from an UNWIND-PROTECT cleanup is similar to
;;; (and a little simpler than) returning from a function.
(define-ppc64-vinsn restore-cleanup-context (()
					     ())
  (ld ppc::loc-pc ppc64::lisp-frame.savelr ppc::sp)
  (mtlr ppc::loc-pc)
  (la ppc::sp ppc64::lisp-frame.size ppc::sp))



(define-ppc64-vinsn default-1-arg (()
				   ((min :u16const))
				   ((crf :crf)))
  (cmpldi crf ppc::nargs (:apply ash min ppc64::word-shift))
  (bne crf :done)
  ((:pred >= min 3)
   (stdu ppc::arg_x -8 ppc::vsp))
  ((:pred >= min 2)
   (mr ppc::arg_x ppc::arg_y))
  ((:pred >= min 1)
   (mr ppc::arg_y ppc::arg_z))
  (li ppc::arg_z (:apply target-nil-value))
  :done)

(define-ppc64-vinsn default-2-args (()
				    ((min :u16const))
				    ((crf :crf)))
  (cmpldi crf ppc::nargs (:apply ash (:apply 1+ min) ppc64::word-shift))
  (bgt crf :done)
  (beq crf :one)
  ;; We got "min" args; arg_y & arg_z default to nil
  ((:pred >= min 3)
   (stdu ppc::arg_x -8 ppc::vsp))   
  ((:pred >= min 2)
   (stdu ppc::arg_y -8 ppc::vsp))
  ((:pred >= min 1)
   (mr ppc::arg_x ppc::arg_z))
  (li ppc::arg_y (:apply target-nil-value))
  (b :last)
  :one
  ;; We got min+1 args: arg_y was supplied, arg_z defaults to nil.
  ((:pred >= min 2)
   (stdu ppc::arg_x -8 ppc::vsp))
  ((:pred >= min 1)
   (mr ppc::arg_x ppc::arg_y))
  (mr ppc::arg_y ppc::arg_z)
  :last
  (li ppc::arg_z (:apply target-nil-value))
  :done)

(define-ppc64-vinsn default-3-args (()
				    ((min :u16const))
				    ((crfx :crf)
				     (crfy :crf)))
  (cmpldi crfx ppc::nargs (:apply ash (:apply + 2 min) ppc64::word-shift))
  (cmpldi crfy ppc::nargs (:apply ash min ppc64::word-shift))
  (bgt crfx :done)
  (beq crfx :two)
  (beq crfy :none)
  ;; The first (of three) &optional args was supplied.
  ((:pred >= min 2)
   (stdu ppc::arg_x -8 ppc::vsp))
  ((:pred >= min 1)
   (stdu ppc::arg_y -8 ppc::vsp))
  (mr ppc::arg_x ppc::arg_z)
  (b :last-2)
  :two
  ;; The first two (of three) &optional args were supplied.
  ((:pred >= min 1)
   (stdu ppc::arg_x -8 ppc::vsp))
  (mr ppc::arg_x ppc::arg_y)
  (mr ppc::arg_y ppc::arg_z)
  (b :last-1)
  ;; None of the three &optional args was provided.
  :none
  ((:pred >= min 3)
   (stdu ppc::arg_x -8 ppc::vsp))
  ((:pred >= min 2)
   (stdu ppc::arg_y -8 ppc::vsp))
  ((:pred >= min 1)
   (stdu ppc::arg_z -8 ppc::vsp))
  (li ppc::arg_x (:apply target-nil-value))
  :last-2
  (li ppc::arg_y (:apply target-nil-value))
  :last-1
  (li ppc::arg_z (:apply target-nil-value))
  :done)

(define-ppc64-vinsn save-lr (()
			     ())
  (mflr ppc::loc-pc))

;;; "n" is the sum of the number of required args + 
;;; the number of &optionals.  
(define-ppc64-vinsn (default-optionals :call :subprim-call) (()
							     ((n :u16const)))
  (li ppc::imm0 (:apply ash n ppc64::word-shift))
  (bla .SPdefault-optional-args))

;;; fname contains a known symbol
(define-ppc64-vinsn (call-known-symbol :call) (((result (:lisp ppc::arg_z)))
					       ())
  (ld ppc::nfn ppc64::symbol.fcell ppc::fname)
  (ld ppc::temp0 ppc64::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctrl))

(define-ppc64-vinsn (jump-known-symbol :jumplr) (()
						 ())
  (ld ppc::nfn ppc64::symbol.fcell ppc::fname)
  (ld ppc::temp0 ppc64::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctr))

(define-ppc64-vinsn (call-known-function :call) (()
						 ())
  (ld ppc::temp0 ppc64::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctrl))

(define-ppc64-vinsn (jump-known-function :jumplr) (()
						   ())
  (ld ppc::temp0 ppc64::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctr))

(define-ppc64-vinsn %schar8 (((char :imm))
                             ((str :lisp)
                              (idx :imm))
                             ((imm :u32)
                              (cr0 (:crf 0))))
  (srdi imm idx ppc64::fixnumshift)
  (addi imm imm ppc64::misc-data-offset)
  (lbzx imm str imm)
  (rldicr imm imm ppc64::charcode-shift (- 63 ppc64::charcode-shift))
  (ori char imm ppc64::subtag-character))

(define-ppc64-vinsn %schar32 (((char :imm))
                              ((str :lisp)
                               (idx :imm))
                              ((imm :u32)
                               (cr0 (:crf 0))))
  (srdi imm idx 1)
  (addi imm imm ppc64::misc-data-offset)
  (lwzx imm str imm)
  (rldicr imm imm ppc64::charcode-shift (- 63 ppc64::charcode-shift))
  (ori char imm ppc64::subtag-character))

(define-ppc64-vinsn %set-schar8 (()
				((str :lisp)
				 (idx :imm)
				 (char :imm))
				((imm :u64)
				 (imm1 :u64)
				 (cr0 (:crf 0))))
  (srdi imm idx ppc64::fixnumshift)
  (addi imm imm ppc64::misc-data-offset)
  (srdi imm1 char ppc64::charcode-shift)
  (stbx imm1 str imm)
  )

(define-ppc64-vinsn %set-schar32 (()
				((str :lisp)
				 (idx :imm)
				 (char :imm))
				((imm :u64)
				 (imm1 :u64)
				 (cr0 (:crf 0))))
  (srdi imm idx 1)
  (addi imm imm ppc64::misc-data-offset)
  (srdi imm1 char ppc64::charcode-shift)
  (stwx imm1 str imm)
  )

(define-ppc64-vinsn %set-scharcode8 (()
                                     ((str :lisp)
                                      (idx :imm)
                                      (code :imm))
                                     ((imm :u64)
                                      (imm1 :u64)
                                      (cr0 (:crf 0))))
  (srdi imm idx ppc64::fixnumshift)
  (addi imm imm ppc64::misc-data-offset)
  (srdi imm1 code ppc64::fixnumshift)
  (stbx imm1 str imm)
  )

(define-ppc64-vinsn %set-scharcode32 (()
                                      ((str :lisp)
                                       (idx :imm)
                                       (code :imm))
                                      ((imm :u64)
                                       (imm1 :u64)
                                       (cr0 (:crf 0))))
  (srdi imm idx 1)
  (addi imm imm ppc64::misc-data-offset)
  (srdi imm1 code ppc64::fixnumshift)
  (stwx imm1 str imm)
  )


(define-ppc64-vinsn %scharcode8 (((code :imm))
                                 ((str :lisp)
                                  (idx :imm))
                                 ((imm :u64)
                                  (cr0 (:crf 0))))
  (srdi imm idx ppc64::fixnumshift)
  (addi imm imm ppc64::misc-data-offset)
  (lbzx imm str imm)
  (sldi code imm ppc64::fixnumshift))

(define-ppc64-vinsn %scharcode32 (((code :imm))
                                  ((str :lisp)
                                   (idx :imm))
                                  ((imm :u64)
                                   (cr0 (:crf 0))))
  (srdi imm idx 1)
  (addi imm imm ppc64::misc-data-offset)
  (lwzx imm str imm)
  (sldi code imm ppc64::fixnumshift))

;;; Clobbers LR
(define-ppc64-vinsn (%debug-trap :call :subprim-call) (()
						       ())
  (bla .SPbreakpoint)
  )


(define-ppc64-vinsn eep.address (((dest t))
				 ((src (:lisp (:ne dest )))))
  (ld dest (+ (ash 1 ppc64::word-shift) ppc64::misc-data-offset) src)
  (tdeqi dest (:apply target-nil-value)))

(define-ppc64-vinsn %natural+ (((dest :u64))
                               ((x :u64) (y :u64)))
  (add dest x y))

(define-ppc64-vinsn %natural+-c (((dest :u64))
                                 ((x :u64) (y :u16const)))
  (addi dest x y))

(define-ppc64-vinsn %natural- (((dest :u64))
                               ((x :u64) (y :u64)))
  (sub dest x y))

(define-ppc64-vinsn %natural--c (((dest :u64))
                                 ((x :u64) (y :u16const)))
  (subi dest x y))

(define-ppc64-vinsn %natural-logior (((dest :u64))
                                     ((x :u64) (y :u64)))
  (or dest x y))

(define-ppc64-vinsn %natural-logior-c (((dest :u64))
				   ((x :u64) (high :u16const) (low :u16const)))
  ((:not (:pred = high 0))
   (oris dest x high))
  ((:not (:pred = low 0))
   (ori dest x low)))

(define-ppc64-vinsn %natural-logxor (((dest :u64))
                                     ((x :u64) (y :u64)))
  (xor dest x y))

(define-ppc64-vinsn %natural-logxor-c (((dest :u64))
                                       ((x :u64) (high :u16const) (low :u16const)))
  ((:not (:pred = high 0))
   (xoris dest x high))
  ((:not (:pred = low 0))
   (xori dest x low)))

(define-ppc64-vinsn %natural-logand (((dest :u64))
                                     ((x :u64) (y :u64)))
  (and dest x y))

(define-ppc64-vinsn %natural-logand-high-c (((dest :u64))
                                            ((x :u64) (high :u16const))
                                            ((cr0 (:crf 0))))
  (andis. dest x high))

(define-ppc64-vinsn %natural-logand-low-c (((dest :u64))
                                           ((x :u64) (low :u16const))
                                           ((cr0 (:crf 0))))
  (andi. dest x low))

(define-ppc64-vinsn %natural-logand-mask-c (((dest :u32))
                                            ((x :u32)
                                             (start :u8const)
                                             (end :u8const)))
  (rlwinm dest x 0 start end))

(define-ppc64-vinsn disable-interrupts (((dest :lisp))
					()
					((temp :imm)
                                         (temp2 :imm)))
  (ld temp2 ppc64::tcr.tlb-pointer ppc64::rcontext)
  (li temp -8)
  (ld dest ppc64::interrupt-level-binding-index temp2)
  (std temp ppc64::interrupt-level-binding-index temp2))

(define-ppc64-vinsn load-character-constant (((dest :lisp))
                                             ((code :u32const))
                                             ())
  (ori dest ppc::rzero (:apply logior (:apply ash (:apply logand #xff code) 8) ppc64::subtag-character))
  ((:not (:pred = 0 (:apply ldb (byte 16 8) code)))
   (oris dest dest (:apply ldb (byte 16 8) code))))


(define-ppc64-vinsn %symbol->symptr (((dest :lisp))
                                     ((src :lisp))
                                     ((tag :u8)
                                      (crf0 :crf)))
  (clrldi tag src (- ppc64::nbits-in-word ppc64::ntagbits))
  (cmpdi crf0 tag ppc64::fulltag-misc)
  (bne crf0 :do-trap)
  (lbz tag ppc64::misc-subtag-offset src)
  :do-trap
  (tdnei tag ppc64::subtag-symbol)
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src)))



;;; Subprim calls.  Done this way for the benefit of VINSN-OPTIMIZE.
(defmacro define-ppc64-subprim-call-vinsn ((name &rest other-attrs) spno)
  `(define-ppc64-vinsn (,name :call :subprim-call ,@other-attrs) (() ())
    (bla ,spno)))

(defmacro define-ppc64-subprim-jump-vinsn ((name &rest other-attrs) spno)
  `(define-ppc64-vinsn (,name :jumpLR ,@other-attrs) (() ())
    (ba ,spno)))

(define-ppc64-subprim-jump-vinsn (restore-interrupt-level) .SPrestoreintlevel)

(define-ppc64-subprim-call-vinsn (save-values) .SPsave-values)

(define-ppc64-subprim-call-vinsn (recover-values)  .SPrecover-values)

(define-ppc64-subprim-call-vinsn (add-values) .SPadd-values)

(define-ppc64-subprim-jump-vinsn (jump-known-symbol-ool) .SPjmpsym)

(define-ppc64-subprim-call-vinsn (call-known-symbol-ool)  .SPjmpsym)

(define-ppc64-subprim-call-vinsn (pass-multiple-values)  .SPmvpass)

(define-ppc64-subprim-call-vinsn (pass-multiple-values-symbol) .SPmvpasssym)

(define-ppc64-subprim-jump-vinsn (tail-call-sym-gen) .SPtcallsymgen)

(define-ppc64-subprim-jump-vinsn (tail-call-fn-gen) .SPtcallnfngen)

(define-ppc64-subprim-jump-vinsn (tail-call-sym-slide) .SPtcallsymslide)

(define-ppc64-subprim-jump-vinsn (tail-call-fn-slide) .SPtcallnfnslide)

(define-ppc64-subprim-jump-vinsn (tail-call-sym-vsp) .SPtcallsymvsp)

(define-ppc64-subprim-jump-vinsn (tail-call-fn-vsp) .SPtcallnfnvsp)

(define-ppc64-subprim-call-vinsn (funcall)  .SPfuncall)

(define-ppc64-subprim-jump-vinsn (tail-funcall-gen) .SPtfuncallgen)

(define-ppc64-subprim-jump-vinsn (tail-funcall-slide) .SPtfuncallslide)

(define-ppc64-subprim-jump-vinsn (tail-funcall-vsp) .SPtfuncallvsp)

(define-ppc64-subprim-call-vinsn (spread-lexpr)  .SPspread-lexpr-z)

(define-ppc64-subprim-call-vinsn (spread-list)  .SPspreadargz)

(define-ppc64-subprim-call-vinsn (pop-argument-registers)  .SPvpopargregs)

(define-ppc64-subprim-call-vinsn (getxlong)  .SPgetXlong)

(define-ppc64-subprim-call-vinsn (stack-cons-list)  .SPstkconslist)

(define-ppc64-subprim-call-vinsn (list) .SPconslist)

(define-ppc64-subprim-call-vinsn (stack-cons-list*)  .SPstkconslist-star)

(define-ppc64-subprim-call-vinsn (list*) .SPconslist-star)

(define-ppc64-subprim-call-vinsn (make-stack-block)  .SPmakestackblock)

(define-ppc64-subprim-call-vinsn (make-stack-block0)  .Spmakestackblock0)

(define-ppc64-subprim-call-vinsn (make-stack-list)  .Spmakestacklist)

(define-ppc64-subprim-call-vinsn (make-stack-vector)  .SPmkstackv)

(define-ppc64-subprim-call-vinsn (make-stack-gvector)  .SPstkgvector)

(define-ppc64-subprim-call-vinsn (stack-misc-alloc)  .SPstack-misc-alloc)

(define-ppc64-subprim-call-vinsn (stack-misc-alloc-init)  .SPstack-misc-alloc-init)

(define-ppc64-subprim-call-vinsn (bind-nil)  .SPbind-nil)

(define-ppc64-subprim-call-vinsn (bind-self)  .SPbind-self)

(define-ppc64-subprim-call-vinsn (bind-self-boundp-check)  .SPbind-self-boundp-check)

(define-ppc64-subprim-call-vinsn (bind)  .SPbind)

(define-ppc64-subprim-jump-vinsn (nvalret :jumpLR) .SPnvalret)

(define-ppc64-subprim-call-vinsn (nthrowvalues) .SPnthrowvalues)

(define-ppc64-subprim-call-vinsn (nthrow1value) .SPnthrow1value)

(define-ppc64-subprim-call-vinsn (slide-values) .SPmvslide)

(define-ppc64-subprim-call-vinsn (macro-bind) .SPmacro-bind)

(define-ppc64-subprim-call-vinsn (destructuring-bind-inner) .SPdestructuring-bind-inner)

(define-ppc64-subprim-call-vinsn (destructuring-bind) .SPdestructuring-bind)

(define-ppc64-subprim-call-vinsn (simple-keywords) .SPsimple-keywords)

(define-ppc64-subprim-call-vinsn (keyword-args) .SPkeyword-args)

(define-ppc64-subprim-call-vinsn (keyword-bind) .SPkeyword-bind)

(define-ppc64-subprim-call-vinsn (stack-rest-arg) .SPstack-rest-arg)

(define-ppc64-subprim-call-vinsn (req-stack-rest-arg) .SPreq-stack-rest-arg)

(define-ppc64-subprim-call-vinsn (stack-cons-rest-arg) .SPstack-cons-rest-arg)

(define-ppc64-subprim-call-vinsn (heap-rest-arg) .SPheap-rest-arg)

(define-ppc64-subprim-call-vinsn (req-heap-rest-arg) .SPreq-heap-rest-arg)

(define-ppc64-subprim-call-vinsn (heap-cons-rest-arg) .SPheap-cons-rest-arg)

(define-ppc64-subprim-call-vinsn (opt-supplied-p) .SPopt-supplied-p)

(define-ppc64-subprim-call-vinsn (gvector) .SPgvector)

(define-ppc64-vinsn (nth-value :call :subprim-call) (((result :lisp))
						     ())
  (bla .SPnthvalue))

(define-ppc64-subprim-call-vinsn (fitvals) .SPfitvals)

(define-ppc64-subprim-call-vinsn (misc-alloc) .SPmisc-alloc)

(define-ppc64-subprim-call-vinsn (misc-alloc-init) .SPmisc-alloc-init)

(define-ppc64-subprim-call-vinsn (integer-sign) .SPinteger-sign)

;;; Even though it's implemented by calling a subprim, THROW is really
;;; a JUMP (to a possibly unknown destination).  If the destination's
;;; really known, it should probably be inlined (stack-cleanup, value
;;; transfer & jump ...)
(define-ppc64-vinsn (throw :jump-unknown) (()
						 ())
  (bla .SPthrow))

(define-ppc64-subprim-call-vinsn (mkcatchmv) .SPmkcatchmv)

(define-ppc64-subprim-call-vinsn (mkcatch1v) .SPmkcatch1v)

(define-ppc64-subprim-call-vinsn (setqsym) .SPsetqsym)

(define-ppc64-subprim-call-vinsn (ksignalerr) .SPksignalerr)

(define-ppc64-subprim-call-vinsn (subtag-misc-ref) .SPsubtag-misc-ref)

(define-ppc64-subprim-call-vinsn (subtag-misc-set) .SPsubtag-misc-set)

(define-ppc64-subprim-call-vinsn (mkunwind) .SPmkunwind)
(define-ppc64-subprim-call-vinsn (nmkunwind) .SPnmkunwind)

(define-ppc64-subprim-call-vinsn (progvsave) .SPprogvsave)

(define-ppc64-subprim-jump-vinsn (progvrestore) .SPprogvrestore)

(define-ppc64-subprim-call-vinsn (eabi-syscall) .SPeabi-syscall)

(define-ppc64-subprim-call-vinsn (misc-ref) .SPmisc-ref)

(define-ppc64-subprim-call-vinsn (misc-set) .SPmisc-set)

(define-ppc64-subprim-call-vinsn (gets64) .SPgets64)

(define-ppc64-subprim-call-vinsn (getu64) .SPgetu64)

(define-ppc64-subprim-call-vinsn (makeu64) .SPmakeu64)

(define-ppc64-subprim-call-vinsn (makes64) .SPmakes64)

(define-ppc64-vinsn (poweropen-syscall :call :subprim-call) (()
							  ())
  (stw ppc::rzero ppc64::c-frame.crsave ppc::sp)
  (bla .SPpoweropen-syscall))

(define-ppc64-vinsn (poweropen-syscall-s64 :call :subprim-call) (()
							      ())
  (std ppc::sp ppc64::c-frame.crsave ppc::sp)
  (bla .SPpoweropen-syscall))

(define-ppc64-subprim-call-vinsn (eabi-ff-call) .SPeabi-ff-call)

(define-ppc64-subprim-call-vinsn (poweropen-ff-call) .SPpoweropen-ffcall)

(define-ppc64-subprim-call-vinsn (poweropen-ff-call-regs) .SPpoweropen-ffcall-return-registers)

(define-ppc64-subprim-call-vinsn (poweropen-ff-callX) .SPpoweropen-ffcallX)

(define-ppc64-subprim-call-vinsn (bind-interrupt-level-0) .SPbind-interrupt-level-0)

(define-ppc64-vinsn bind-interrupt-level-0-inline (()
                                                   ()
                                                   ((tlb :imm)
                                                    (value :imm)
                                                    (link :imm)
                                                    (temp :imm)))
  (ld tlb ppc64::tcr.tlb-pointer ppc64::rcontext)
  (ld value ppc64::interrupt-level-binding-index tlb)
  (ld link ppc64::tcr.db-link ppc64::rcontext)
  (cmpdi value 0)
  (li temp ppc64::interrupt-level-binding-index)
  (stdu value -8 ppc::vsp)
  (stdu temp -8 ppc::vsp)
  (stdu link -8 ppc::vsp)
  (std ppc::rzero ppc64::interrupt-level-binding-index tlb)
  (std ppc::vsp  ppc64::tcr.db-link ppc64::rcontext)
  (beq+ :done)
  (mr ppc::nargs value)
  (bgt :do-trap)
  (ld ppc::nargs ppc64::tcr.interrupt-pending ppc64::rcontext)
  :do-trap
  (tdgti ppc::nargs 0)
  :done)

(define-ppc64-subprim-call-vinsn (bind-interrupt-level-m1) .SPbind-interrupt-level-m1)

(define-ppc64-vinsn bind-interrupt-level-m1-inline (()
                                                   ()
                                                   ((tlb :imm)
                                                    (oldvalue :imm)
                                                    (link :imm)
                                                    (newvalue :imm)
                                                    (idx :imm)))
  (li newvalue (ash -1 ppc64::fixnumshift))
  (li idx ppc64::interrupt-level-binding-index)
  (ld tlb ppc64::tcr.tlb-pointer ppc64::rcontext)
  (ld oldvalue ppc64::interrupt-level-binding-index tlb)
  (ld link ppc64::tcr.db-link ppc64::rcontext)
  (stdu oldvalue -8 ppc::vsp)
  (stdu idx -8 ppc::vsp)
  (stdu link -8 ppc::vsp)
  (std newvalue ppc64::interrupt-level-binding-index tlb)
  (std ppc::vsp  ppc64::tcr.db-link ppc64::rcontext)
  :done)

(define-ppc64-subprim-call-vinsn (bind-interrupt-level) .SPbind-interrupt-level)

(define-ppc64-subprim-call-vinsn (unbind-interrupt-level) .SPunbind-interrupt-level)

(define-ppc64-vinsn unbind-interrupt-level-inline (()
                                                   ()
                                                   ((tlb :imm)
                                                    (link :imm)
                                                    (value :imm)
                                                    (save-nargs :u32)
                                                    (crf0 :crf)
                                                    (crf1 :crf)))
  (ld tlb ppc64::tcr.tlb-pointer ppc64::rcontext)
  (ld value ppc64::interrupt-level-binding-index tlb)
  (ld link ppc64::tcr.db-link ppc64::rcontext)
  (cmpdi crf1 value 0)
  (ld value 16 link)
  (ld link 0 link)
  (cmpdi crf0 value 0)
  (std value ppc64::interrupt-level-binding-index tlb)
  (std link ppc64::tcr.db-link ppc64::rcontext)
  (bge crf1 :done)
  (blt crf0 :done)
  (mr save-nargs ppc::nargs)
  (ld ppc::nargs ppc64::tcr.interrupt-pending ppc64::rcontext)
  (tdgti ppc::nargs 0)
  (mr ppc::nargs save-nargs)
  :done)

(define-ppc64-vinsn fixnum->fpr (((f :double-float))
                                          ((fixnum :imm))
                                          ((imm :s64)))
  (srawi imm fixnum ppc64::fixnumshift)
  (std imm -8 ppc::sp)
  (lfd f -8 ppc::sp)
  (fcfid f f))

(define-ppc64-vinsn branch-unless-arg-fixnum (()
                                              ((arg :lisp)
                                               (lab :label))
                                              ((cr0 (:crf 0))
                                               (tag :u8)))
  (clrldi. tag arg (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (bne cr0 lab))

(define-ppc64-vinsn branch-unless-both-args-fixnums (()
                                              ((arg0 :lisp)
                                               (arg1 :lisp)
                                               (lab :label))
                                              ((cr0 (:crf 0))
                                               (tag :u8)))
  (clrldi tag arg0 (- ppc64::nbits-in-word ppc64::nlisptagbits))
  (rldimi. tag arg1 ppc64::nlisptagbits 58)
  (bne cr0 lab))
  
                                              
                                           

;;; In case ppc64::*ppc-opcodes* was changed since this file was compiled.
(queue-fixup
 (fixup-vinsn-templates *ppc64-vinsn-templates* ppc::*ppc-opcode-numbers*))

(provide "PPC64-VINSNS")
