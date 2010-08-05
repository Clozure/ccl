;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
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

;;; Do an FF-CALL to MakeDataExecutable so that the data cache gets flushed.
;;; If the GC moves this function while we're trying to flush the cache,
;;; it'll flush the cache: no harm done in that case.

(defun %make-code-executable (codev)
  (with-macptrs (p)
    (let* ((nbytes (ash (uvsize codev) arm::word-shift)))
      (%vect-data-to-macptr codev p)
      (ff-call (%kernel-import arm::kernel-import-MakeDataExecutable)
               :address p
               :unsigned-fullword nbytes
               :void))))

(defarmlapfunction %get-kernel-global-from-offset ((offset arg_z))
  (check-nargs 1)
  (mov imm0 (:$ (- arm::nil-value arm::fulltag-nil)))
  (ldr arg_z (:-@ imm0 (:asr offset (:$ arm::fixnumshift))))
  (bx lr))


(defarmlapfunction %set-kernel-global-from-offset ((offset arg_y) (new-value arg_z))
  (check-nargs 2)
  (mov imm0 (:$ (- arm::nil-value arm::fulltag-nil)))
  (ldr new-value (:-@ imm0 (:asr offset (:$ arm::fixnumshift))))
  (bx lr))



(defarmlapfunction %get-kernel-global-ptr-from-offset ((offset arg_y)
						       (ptr arg_z))
  (check-nargs 2)
  (mov imm0 (:$ (- arm::nil-value arm::fulltag-nil)))
  (ldr imm0 (:-@ imm0 (:asr offset (:$ arm::fixnumshift))))
  (str imm0 (:@ ptr (:$ target::macptr.address)))
  (bx lr))




(defarmlapfunction %fixnum-ref ((fixnum arg_y) #| &optional |# (offset arg_z))
  (:arglist (fixnum &optional offset))
  (check-nargs 1 2)
  (cmp nargs '1)
  (moveq fixnum offset)
  (moveq offset (:$ 0))
  (unbox-fixnum imm0 offset)
  (ldr arg_z (:@ imm0 fixnum))
  (bx lr))


(defarmlapfunction %fixnum-ref-natural ((fixnum arg_y) #| &optional |# (offset arg_z))
  (:arglist (fixnum &optional offset))
  (check-nargs 1 2)
  (cmp nargs '1)
  (moveq fixnum offset)
  (moveq offset (:$ 0))
  (unbox-fixnum imm0 offset)
  (ldr imm0 (:@ imm0 fixnum))
  (ba .SPmakeu32))



(defarmlapfunction %fixnum-set ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (:arglist (fixnum offset &optional new-value))
  (check-nargs 2 3)
  (cmp nargs '2)
  (moveq fixnum offset)
  (moveq offset (:$ 0))
  (unbox-fixnum imm0 offset)
  (str new-value (:@ imm0 fixnum))
  (mov arg_z new-value)
  (bx lr))

(defarmlapfunction %fixnum-set-natural ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (check-nargs 2 3)
  (cmp nargs '2)
  (moveq fixnum offset)
  (moveq offset (:$ 0))
  (unbox-fixnum imm0 offset)
  (test-fixnum new-value)
  (unbox-fixnum imm2 new-value)
  (beq @store)
  (extract-subtag imm1 new-value)
  (cmp imm1 (:$ arm::subtag-bignum))
  (uuo-error-reg-not-xtype (:? ne) new-value (:$ arm::xtype-u32))
  (getvheader imm0 new-value)
  (header-length temp0 imm0)
  (cmp temp0 '2)
  (ldr imm2 (:@ new-value (:$ arm::misc-data-offset)))
  (ldreq imm1 (:@ new-value (:$ (+ arm::misc-data-offset))))
  (uuo-error-reg-not-xtype (:? gt) new-value (:$ arm::xtype-u32))
  (bne @one)
  (cmp imm1 ($ 0))
  (beq @store)
  (uuo-error-reg-not-xtype (:? ne) new-value (:$ arm::xtype-u32))
  @one
  (cmp imm2 ($ 0))
  (uuo-error-reg-not-xtype (:? mi) new-value (:$ arm::xtype-u32))
  @store
  (str imm2 (:@ imm0 fixnum))
  (mov arg_z new-value)
  (bx lr))



(defarmlapfunction %current-frame-ptr ()
  (check-nargs 0)
  (mov arg_z sp)
  (bx lr))

(defarmlapfunction %current-vsp ()
  (check-nargs 0)
  (mov arg_z vsp)
  (bx lr))




(defarmlapfunction %set-current-vsp ((new-vsp arg_z))
  (check-nargs 1)
  (mov vsp new-vsp)
  (bx lr))



(defarmlapfunction %%frame-backlink ((p arg_z))
  (check-nargs 1)
  (ldr imm0 (:@ p))
  (cmp imm0 (:$ arm::lisp-frame-marker))
  (addeq arg_z p (:$ arm::lisp-frame.size))
  (bxeq lr)
  (cmp imm0 (:$ arm::stack-alloc-marker))
  (and imm1 imm0 (:$ arm::fulltagmask))
  (addeq arg_z p '2)
  (bxeq lr)
  (cmp imm1 (:$ arm::fulltag-immheader))
  (beq @imm)
  (cmp imm1 (:$ arm::fulltag-nodeheader))
  (movne arg_z (:$ 0))
  (bxne lr)
  (header-length imm0 imm0)
  
  (add imm0 imm0 (:$ (* 2 arm::node-size)))
  (bic imm0 imm0 (:$ arm::node-size))
  (add arg_z p imm0)
  (bx lr)
  @imm
  (extract-lowbyte imm1 imm0)
  (mov imm0 (:lsr imm0 (:$ arm::num-subtag-bits)))
  (cmp imm1 (:$ arm::max-32-bit-ivector-subtag))
  (bhi @8)
  (mov imm0 (:lsl imm0 (:$ arm::word-shift)))
  @align
  (add imm0 imm0 (:$ (+ 4 7)))
  (bic imm0 imm0 (:$ arm::fulltagmask))
  (add arg_z p imm0)
  (bx lr)
  @8
  (cmp imm1 (:$ arm::max-8-bit-ivector-subtag))
  (bls @align)
  (cmp imm1 (:$ arm::max-16-bit-ivector-subtag))
  (movls imm0 (:lsl imm0 (:$ 1)))
  (bls @align)
  (cmp imm1 (:$ arm::subtag-double-float))
  (moveq imm0 (:lsl imm0 (:$ 3)))
  (beq @align)
  (add imm0 imm0 (:$ 7))
  (mov imm0 (:lsr imm0 (:$ 3)))
  (b @align))
 
  
  





(defarmlapfunction %%frame-savefn ((p arg_z))
  (check-nargs 1)
  (ldr arg_z (:@ arg_z (:$ arm::lisp-frame.savefn)))
  (bx lr))

(defarmlapfunction %cfp-lfun ((p arg_z))
  (build-lisp-frame)
  (ldr arg_y (:@ p (:$ arm::lisp-frame.savefn)))
  (extract-typecode imm0 arg_y)
  (cmp imm0 (:$ arm::subtag-function))
  (ldr lr (:@ p (:$ arm::lisp-frame.savelr)))
  (bne @no)
  (ldr arg_x (:@ arg_y (:$ (+ arm::node-size arm::misc-data-offset))))
  (sub imm1 lr arg_x)
  (add imm1 imm1 (:$ (- arm::misc-data-offset)))
  (getvheader imm0 arg_x)
  (header-length imm0 imm0)
  (cmp imm1 imm0)
  (box-fixnum imm1 imm1)
  (bhs @no)
  (vpush1 arg_y)
  (vpush1 imm1)
  @go
  (set-nargs 2)
  (ba .SPnvalret)
  @no
  (mov imm0 'nil)
  (vpush1 imm0)
  (vpush1 imm0)
  (b @go))




(defarmlapfunction %%frame-savevsp ((p arg_z))
  (check-nargs 1)
  (ldr arg_z (:@ arg_z (:$ arm::lisp-frame.savevsp)))
  (bx lr))







(defarmlapfunction %uvector-data-fixnum ((uv arg_z))
  (check-nargs 1)
  (trap-unless-fulltag= arg_z arm::fulltag-misc)
  (add arg_z arg_z (:$ arm::misc-data-offset))
  (bx lr))

(defarmlapfunction %catch-top ((tcr arg_z))
  (check-nargs 1)
  (ldr arg_z (:@ tcr (:$ arm::tcr.catch-top)))
  (cmp arg_z (:$ 0))
  (moveq arg_z 'nil)
  (bx lr))





;;; Same as %address-of, but doesn't cons any bignums
;;; It also left shift fixnums just like everything else.
(defarmlapfunction %fixnum-address-of ((x arg_z))
  (check-nargs 1)
  (box-fixnum arg_z x)
  (bx lr))

(defarmlapfunction %dnode-address-of ((x arg_z))
  (check-nargs 1)
  (bic arg_z x (:$ arm::fulltagmask))
  (bx lr))

(defarmlapfunction %save-standard-binding-list ((bindings arg_z))
  (ldr imm0 (:@ arm::rcontext (:$ arm::tcr.vs-area)))
  (ldr imm1 (:@ imm0 (:$ arm::area.high)))
  (push1 bindings imm1)
  (bx lr))

(defarmlapfunction %saved-bindings-address ()
  (ldr imm0 (:@ arm::rcontext (:$ arm::tcr.vs-area)))
  (ldr imm1 (:@ imm0 (:$ arm::area.high)))
  (add arg_z imm1 (:$ (- arm::node-size)))
  (bx lr))

(defarmlapfunction %code-vector-pc ((code-vector arg_y) (pcptr arg_z))
  (build-lisp-frame)
  (macptr-ptr imm0 pcptr)
  (ldr lr (:@ imm0 (:$ 0)))
  (sub imm0 lr code-vector)
  (sub imm0 imm0 (:$ arm::misc-data-offset))
  (getvheader imm1 code-vector)
  (header-size imm1 imm1)
  (mov imm1 (:lsr imm1 (:$ 2)))
  (cmp imm0 imm1)
  (movhs arg_z 'nil)
  (movlo arg_z (:lsl imm0 (:$ arm::fixnumshift)))
  (return-lisp-frame))

#+notyet
(progn
;;; FF-call, in LAP.
#+eabi-target
(progn
  (defarmlapfunction %%ff-call ((fploads 8)
                                (single-offset 4)
                                (double-offset 0)
                                (framesize arg_x) ;always even, negative, includes frame overhead
                                (buf arg_y)
                                (entry arg_z))
    (check-nargs 6)
    (la imm0 12 vsp)
    (save-lisp-context imm0)
    (stwux sp sp framesize)
    (stw sp 4 sp)
    (macptr-ptr imm2 buf)
    (mov imm1 imm2)
    (la imm3 ppc32::eabi-c-frame.param0 sp)
    (li imm0 0)
    (lwz temp1 single-offset vsp)
    (lwz temp2 double-offset vsp)
    @copy
    (addi imm0 imm0 8)
    (cmpw imm0 temp1)
    (lfd fp0 0 imm2)
    (la imm2 8 imm2)
    (stfd fp0 0 imm3)
    (la imm3 8 imm3)
    (blt @copy)
    ;; We've copied the gpr-save area and the "other" arg words.
    ;; Sadly, we may still need to load up to 8 FPRs, and we have
    ;; to use some pretty ugly code to do so.
    (add temp1 temp1 imm1)
    (add temp2 temp2 imm1)
    (lwz temp0 fploads vsp)
    @load-fp1
    (lbz imm0 (+ ppc32::misc-data-offset 0) temp0)
    (cmpwi imm0 1)
    (blt @loaded)
    (bne @load-fp1-double)
    (lfs fp1 0 temp1)
    (la temp1 4 temp1)
    (b @load-fp2)
    @load-fp1-double
    (lfd fp1 0 temp2)
    (la temp2 8 temp2)
    @load-fp2
    (lbz imm0 (+ ppc32::misc-data-offset 1) temp0)
    (cmpwi imm0 1)
    (blt @loaded)
    (bne @load-fp2-double)
    (lfs fp2 0 temp1)
    (la temp1 4 temp1)
    (b @load-fp3)
    @load-fp2-double
    (lfd fp2 0 temp2)
    (la temp2 8 temp2)
    @load-fp3
    (lbz imm0 (+ ppc32::misc-data-offset 2) temp0)
    (cmpwi imm0 1)
    (blt @loaded)
    (bne @load-fp3-double)
    (lfs fp3 0 temp1)
    (la temp1 4 temp1)
    (b @load-fp4)
    @load-fp3-double
    (lfd fp3 0 temp2)
    (la temp2 8 temp2)
    @load-fp4
    (lbz imm0 (+ ppc32::misc-data-offset 3) temp0)
    (cmpwi imm0 1)
    (blt @loaded)
    (bne @load-fp4-double)
    (lfs fp4 0 temp1)
    (la temp1 4 temp1)
    (b @load-fp5)
    @load-fp4-double
    (lfd fp4 0 temp2)
    (la temp2 8 temp2)
    @load-fp5
    (lbz imm0 (+ ppc32::misc-data-offset 4) temp0)
    (cmpwi imm0 1)
    (blt @loaded)
    (bne @load-fp5-double)
    (lfs fp5 0 temp1)
    (la temp1 4 temp1)
    (b @load-fp6)
    @load-fp5-double
    (lfd fp5 0 temp2)
    (la temp2 8 temp2)
    @load-fp6
    (lbz imm0 (+ ppc32::misc-data-offset 5) temp0)
    (cmpwi imm0 1)
    (blt @loaded)
    (bne @load-fp6-double)
    (lfs fp6 0 temp1)
    (la temp1 4 temp1)
    (b @load-fp7)
    @load-fp6-double
    (lfd fp6 0 temp2)
    (la temp2 8 temp2)
    @load-fp7
    (lbz imm0 (+ ppc32::misc-data-offset 6) temp0)
    (cmpwi imm0 1)
    (blt @loaded)
    (bne @load-fp7-double)
    (lfs fp7 0 temp1)
    (la temp1 4 temp1)
    (b @load-fp8)
    @load-fp7-double
    (lfd fp7 0 temp2)
    (la temp2 8 temp2)
    @load-fp8
    (lbz imm0 (+ ppc32::misc-data-offset 0) temp0)
    (cmpwi imm0 1)
    (blt @loaded)
    (bne @load-fp8-double)
    (lfs fp8 0 temp1)
    (b @loaded)
    @load-fp8-double
    (lfd fp8 0 temp2)
    @loaded
    (vpush buf)
    (bla .SPeabi-ff-call)
    (vpop buf)
    (macptr-ptr imm2 buf)
    (stw imm0 0 imm2)
    (stw imm1 4 imm2)
    (stfs fp1 8 imm2)
    (stfd fp1 16 imm2)
    (restore-full-lisp-context)
    (li arg_z (target-nil-value))
    (bx lr))
  
  (defun %ff-call (entry &rest specs-and-vals)
    "Call the foreign function at address entrypoint passing the values of
each arg as a foreign argument of type indicated by the corresponding
arg-type-keyword. Returns the foreign function result (coerced to a Lisp
object of type indicated by result-type-keyword), or NIL if
result-type-keyword is :VOID or NIL"
    (declare (dynamic-extent specs-and-vals))
    (let* ((len (length specs-and-vals))
           (other-offset 8)
           (single-float-offset 8)
           (double-float-offset 0)
           (nsingle-floats 0)
           (ndouble-floats 0)
           (nother-words 0)
           (nfpr-args 0)
           (ngpr-args 0))
      (declare (fixnum len  other-offset single-float-offset double-float-offset
                       nsingle-floats ndouble-floats nother-words nfpr-args ngpr-args))
      (unless (oddp len)
        (error "Length of ~s is even.  Missing result ?" specs-and-vals))

      (let* ((result-spec (or (car (last specs-and-vals)) :void))
             (nargs (ash (the fixnum (1- len)) -1))
             (fpr-reloads (make-array 8 :element-type '(unsigned-byte 8))))
        (declare (fixnum nargs) (dynamic-extent fpr-reloads))
        (do* ((i 0 (1+ i))
              (specs specs-and-vals (cddr specs))
              (spec (car specs) (car specs)))
             ((= i nargs))
          (declare (fixnum i))
          (ecase spec
            (:double-float (incf nfpr-args)
                           (if (<= nfpr-args 8)
                             (incf ndouble-floats)
                             (progn
                               (if (oddp nother-words)
                                 (incf nother-words))
                               (incf nother-words 2))))
            (:single-float (incf nfpr-args)
                           (if (<= nfpr-args 8)
                             (incf nsingle-floats)
                             (incf nother-words)))
	    ((:signed-doubleword :unsigned-doubleword)
	     (if (oddp ngpr-args)
	       (incf ngpr-args))
	     (incf ngpr-args 2)
	     (when (> ngpr-args 8)
	       (if (oddp nother-words)
		 (incf nother-words))
	       (incf nother-words 2)))
            ((:signed-byte :unsigned-byte :signed-halfword :unsigned-halfword
                           :signed-fullword :unsigned-fullword :address)
	     (incf ngpr-args)
             (if (> ngpr-args 8)
               (incf nother-words)))))
        (let* ((single-words (+ 8 nother-words nsingle-floats))
               (total-words (if (zerop ndouble-floats)
                              single-words
                              (+ (the fixnum (+ ndouble-floats ndouble-floats))
                                 (the fixnum (logand (lognot 1)
                                                     (the fixnum (1+ single-words))))))))
          (declare (fixnum total-words single-words))
          (%stack-block
              ((buf (ash total-words 2)))
            (setq single-float-offset (+ other-offset nother-words))
            (setq double-float-offset
                  (logand (lognot 1)
                          (the fixnum (1+ (the fixnum (+ single-float-offset nsingle-floats))))))
           ;;; Make another pass through the arg/value pairs, evaluating each arg into
           ;;; the buffer.
            (do* ((i 0 (1+ i))
                  (specs specs-and-vals (cddr specs))
                  (spec (car specs) (car specs))
                  (val (cadr specs) (cadr specs))
                  (ngpr 0)
                  (nfpr 0)
                  (gpr-byte-offset 0)
                  (other-byte-offset (ash other-offset 2))
                  (single-byte-offset (ash single-float-offset 2))
                  (double-byte-offset (ash double-float-offset 2)))
                 ((= i nargs))
              (declare (fixnum i gpr-byte-offset single-byte-offset double-byte-offset
                               ngpr nfpr))
              (case spec
                (:double-float
                 (cond ((< nfpr 8)
                        (setf (uvref fpr-reloads nfpr) 2
                              (%get-double-float buf double-byte-offset) val
                              double-byte-offset (+ double-byte-offset 8)))
                       (t
                        (setq other-byte-offset (logand (lognot 7)
                                                        (the fixnum (+ other-byte-offset 4))))
                        (setf (%get-double-float buf other-byte-offset) val)
                        (setq other-byte-offset (+ other-byte-offset 8))))
                 (incf nfpr))
                (:single-float
                 (cond ((< nfpr 8)
                        (setf (uvref fpr-reloads nfpr) 1
                              (%get-single-float buf single-byte-offset) val
                              single-byte-offset (+ single-byte-offset 4)))
                             
                       (t
                        (setf (%get-single-float buf other-byte-offset) val
                              other-byte-offset (+ other-byte-offset 4))))
                 (incf nfpr))
                (:address
                 (cond ((< ngpr 8)
                        (setf (%get-ptr buf gpr-byte-offset) val
                              gpr-byte-offset (+ gpr-byte-offset 4)))
                       (t
                        (setf (%get-ptr buf other-byte-offset) val
                              other-byte-offset (+ other-byte-offset 4))))
                 (incf ngpr))
                ((:signed-doubleword :unsigned-doubleword)
                 (when (oddp ngpr)
                   (incf ngpr)
                   (incf gpr-byte-offset 4))
                 (cond ((< ngpr 8)
                        (if (eq spec :signed-doubleword)
                          (setf (%get-signed-long-long buf gpr-byte-offset) val)
                          (setf (%get-unsigned-long-long buf gpr-byte-offset) val))
                        (incf gpr-byte-offset 8))
                       (t
                        (when (logtest other-byte-offset 7)
                          (incf other-byte-offset 4))
                        (if (eq spec :signed-doubleword)
                          (setf (%get-signed-long-long buf other-byte-offset) val)
                          (setf (%get-unsigned-long-long buf other-byte-offset) val))
                        (incf other-byte-offset 8)))
                 (incf ngpr 2))
		((:unsigned-byte :unsigned-halfword :unsigned-fullword)
                 (cond ((< ngpr 8)
                        (setf (%get-unsigned-long buf gpr-byte-offset) val
                              gpr-byte-offset (+ gpr-byte-offset 4)))
                       (t
                        (setf (%get-unsigned-long buf other-byte-offset) val
                              other-byte-offset (+ other-byte-offset 4))))
		 (incf ngpr))
                (t
                 (cond ((< ngpr 8)
                        (setf (%get-long buf gpr-byte-offset) val
                              gpr-byte-offset (+ gpr-byte-offset 4)))
                       (t
                        (setf (%get-long buf other-byte-offset) val
                              other-byte-offset (+ other-byte-offset 4))))
                 (incf ngpr))))
            (%%ff-call fpr-reloads
                       single-float-offset
                       double-float-offset
                       (the fixnum (-
                                    (ash (the fixnum
                                           (+ 6
                                              (the fixnum (logand
                                                           (lognot 1)
                                                           (the fixnum (1+ total-words))))))
                                         2)))
                       buf
                       entry)
            (ecase result-spec
              (:void nil)
              (:single-float (%get-single-float buf 8))
              (:double-float (%get-double-float buf 16))
              (:address (%get-ptr buf))
              (:signed-doubleword (%get-signed-long-long buf 0))
              (:unsigned-doubleword (%get-unsigned-long-long buf 0))
              (:signed-fullword (%get-signed-long buf))
              (:unsigned-fullword (%get-unsigned-long buf))
              (:signed-halfword (%get-signed-word buf 2))
              (:unsigned-halfword (%get-unsigned-word buf 2))
              (:signed-byte (%get-signed-byte buf 3))
              (:unsigned-byte (%get-unsigned-byte buf 3))))))))
  )





;;; In the PowerOpen ABI, all arguments are passed in a contiguous
;;; block.  The first 13 (!) FP args are passed in FP regs; doubleword
;;; arguments are aligned on word boundaries.
#+poweropen-target
(progn
  #+ppc32-target
  (progn
    (defun %ff-call (entry &rest specs-and-vals)
      (declare (dynamic-extent specs-and-vals))
      (let* ((len (length specs-and-vals))
             (total-words 0))
        (declare (fixnum len total-words))
        (unless (oddp len)
          (error "Length of ~s is even.  Missing result ?" specs-and-vals))
        (let* ((result-spec (or (car (last specs-and-vals)) :void))
               (nargs (ash (the fixnum (1- len)) -1))
               (fpr-reload-sizes (make-array 13 :element-type '(unsigned-byte 8)))
               (fpr-reload-offsets (make-array 13 :element-type '(unsigned-byte 16))))
          (declare (fixnum nargs) (dynamic-extent fpr-reload-sizes fpr-reload-offsets))
          (do* ((i 0 (1+ i))
                (specs specs-and-vals (cddr specs))
                (spec (car specs) (car specs)))
               ((= i nargs))
            (declare (fixnum i))
            (case spec
              ((:double-float :signed-doubleword :unsigned-doubleword)
               (incf total-words 2))
              ((:single-float :signed-byte :unsigned-byte :signed-halfword
                              :unsigned-halfword :signed-fullword
                              :unsigned-fullword :address)
               (incf total-words))
              (t (if (typep spec 'unsigned-byte)
                   (incf total-words spec)
                   (error "Invalid argument spec ~s" spec)))))
          (%stack-block ((buf (ash (logand (lognot 1) (1+ (max 6  total-words))) 2)))
            (do* ((i 0 (1+ i))
                  (fpr 0)
                  (offset 0 (+ offset 4))
                  (specs specs-and-vals (cddr specs))
                  (spec (car specs) (car specs))
                  (val (cadr specs) (cadr specs)))
                 ((= i nargs))
              (declare (fixnum i offset fpr))
              (case spec
                (:double-float
                 (when (< fpr 13)
                   (setf (uvref fpr-reload-sizes fpr) 2
                         (uvref fpr-reload-offsets fpr) offset))
                 (incf fpr)
                 (setf (%get-double-float buf offset) val)
                 (incf offset 4))
                (:single-float
                 (when (< fpr 13)
                   (setf (uvref fpr-reload-sizes fpr) 1
                         (uvref fpr-reload-offsets fpr) offset))
                 (incf fpr)
                 (setf (%get-single-float buf offset) val))
                (:signed-doubleword
                 (setf (%get-signed-long-long buf offset) val)
                 (incf offset 4))
                (:unsigned-doubleword
                 (setf (%get-unsigned-long-long buf offset) val)
                 (incf offset 4))
                (:address
                 (setf (%get-ptr buf offset) val))
		((:unsigned-byte :unsigned-halfword :unsigned-fullword)
		 (setf (%get-unsigned-long buf offset) val))
                (t
                 (if (typep spec 'unsigned-byte)
                   (dotimes (i spec (decf offset 4))
                     (setf (%get-ptr buf offset)
                           (%get-ptr val (* i 4)))
                     (incf offset 4))
                   (setf (%get-long buf offset) val)))))
            (let* ((frame-size (if (<= total-words 8)
                                 (ash
                                  (+ ppc32::c-frame.size ppc32::lisp-frame.size)
                                  -2)
                                 (+
                                  (ash
                                   (+ ppc32::c-frame.size ppc32::lisp-frame.size)
                                   -2)
                                  (logand (lognot 1)
                                          (1+ (- total-words 8)))))))
              
              (%%ff-call
               fpr-reload-sizes
               fpr-reload-offsets
               (- (logandc2 (+ frame-size 3) 3))
               total-words
               buf
               entry))
            (ecase result-spec
              (:void nil)
              (:single-float (%get-single-float buf 8))
              (:double-float (%get-double-float buf 16))
              (:address (%get-ptr buf))
              (:signed-doubleword (%get-signed-long-long buf 0))
              (:unsigned-doubleword (%get-unsigned-long-long buf 0))
              (:signed-fullword (%get-signed-long buf))
              (:unsigned-fullword (%get-unsigned-long buf))
              (:signed-halfword (%get-signed-word buf 2))
              (:unsigned-halfword (%get-unsigned-word buf 2))
              (:signed-byte (%get-signed-byte buf 3))
              (:unsigned-byte (%get-unsigned-byte buf 3)))))))


    (defarmlapfunction %%ff-call ((reload-sizes 8)
                                  (reload-offsets 4)
                                  (frame-size 0)			     
                                  (total-words arg_x)
                                  (buf arg_y)
                                  (entry arg_z))
      (check-nargs 6)
      (la imm0 12 vsp)
      (save-lisp-context imm0)
      (lwz imm0 frame-size vsp)
      (stwux sp sp imm0)
      (stw sp ppc32::c-frame.savelr sp)
      (macptr-ptr imm2 buf)
      (mov imm1 imm2)
      (la imm3 ppc32::c-frame.param0 sp)
      (li temp1 0)
      @copy
      (addi temp1 temp1 '1)
      (cmpw temp1 total-words)
      (lwz imm0 0 imm2)
      (la imm2 4 imm2)
      (stw imm0 0 imm3)
      (la imm3 4 imm3)
      (blt @copy)
      (lwz temp0 reload-sizes vsp)
      (lwz temp1 reload-offsets vsp)
      @load-fp1
      (lbz imm0 (+ ppc32::misc-data-offset 0) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 0) temp1)
      (blt @loaded)
      (bne @load-fp1-double)
      (lfsx fp1 imm1 imm2)
      (b @load-fp2)
      @load-fp1-double
      (lfdx fp1 imm1 imm2)

      @load-fp2
      (lbz imm0 (+ ppc32::misc-data-offset 1) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 2) temp1)
      (blt @loaded)
      (bne @load-fp2-double)
      (lfsx fp2 imm1 imm2)
      (b @load-fp3)
      @load-fp2-double
      (lfdx fp2 imm1 imm2)

      @load-fp3
      (lbz imm0 (+ ppc32::misc-data-offset 2) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 4) temp1)
      (blt @loaded)
      (bne @load-fp3-double)
      (lfsx fp3 imm1 imm2)
      (b @load-fp4)
      @load-fp3-double
      (lfdx fp3 imm1 imm2)

      @load-fp4
      (lbz imm0 (+ ppc32::misc-data-offset 3) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 6) temp1)
      (blt @loaded)
      (bne @load-fp4-double)
      (lfsx fp4 imm1 imm2)
      (b @load-fp5)
      @load-fp4-double
      (lfdx fp4 imm1 imm2)

      @load-fp5
      (lbz imm0 (+ ppc32::misc-data-offset 4) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 8) temp1)
      (blt @loaded)
      (bne @load-fp5-double)
      (lfsx fp5 imm1 imm2)
      (b @load-fp6)
      @load-fp5-double
      (lfdx fp5 imm1 imm2)

      @load-fp6
      (lbz imm0 (+ ppc32::misc-data-offset 5) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 10) temp1)
      (blt @loaded)
      (bne @load-fp1-double)
      (lfsx fp6 imm1 imm2)
      (b @load-fp7)
      @load-fp6-double
      (lfdx fp6 imm1 imm2)

      @load-fp7
      (lbz imm0 (+ ppc32::misc-data-offset 6) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 12) temp1)
      (blt @loaded)
      (bne @load-fp1-double)
      (lfsx fp7 imm1 imm2)
      (b @load-fp8)
      @load-fp7-double
      (lfdx fp7 imm1 imm2)

      @load-fp8
      (lbz imm0 (+ ppc32::misc-data-offset 7) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 14) temp1)
      (blt @loaded)
      (bne @load-fp8-double)
      (lfsx fp8 imm1 imm2)
      (b @load-fp9)
      @load-fp8-double
      (lfdx fp8 imm1 imm2)

      @load-fp9
      (lbz imm0 (+ ppc32::misc-data-offset 8) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 16) temp1)
      (blt @loaded)
      (bne @load-fp9-double)
      (lfsx fp9 imm1 imm2)
      (b @load-fp10)
      @load-fp9-double
      (lfdx fp9 imm1 imm2)

      @load-fp10
      (lbz imm0 (+ ppc32::misc-data-offset 9) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 18) temp1)
      (blt @loaded)
      (bne @load-fp10-double)
      (lfsx fp10 imm1 imm2)
      (b @load-fp11)
      @load-fp10-double
      (lfdx fp10 imm1 imm2)

      @load-fp11
      (lbz imm0 (+ ppc32::misc-data-offset 10) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 20) temp1)
      (blt @loaded)
      (bne @load-fp11-double)
      (lfsx fp11 imm1 imm2)
      (b @load-fp12)
      @load-fp11-double
      (lfdx fp11 imm1 imm2)

      @load-fp12
      (lbz imm0 (+ ppc32::misc-data-offset 11) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 22) temp1)
      (blt @loaded)
      (bne @load-fp12-double)
      (lfsx fp12 imm1 imm2)
      (b @load-fp13)
      @load-fp12-double
      (lfdx fp12 imm1 imm2)

      @load-fp13
      (lbz imm0 (+ ppc32::misc-data-offset 12) temp0)
      (cmpwi imm0 1)
      (lhz imm2 (+ ppc32::misc-data-offset 24) temp1)
      (blt @loaded)
      (bne @load-fp13-double)
      (lfsx fp13 imm1 imm2)
      (b @loaded)
      @load-fp13-double
      (lfdx fp13 imm1 imm2)
      @loaded
      (vpush buf)
      (bla .SPpoweropen-ffcall)
      @called
      (vpop buf)
      (macptr-ptr imm2 buf)
      (stw imm0 0 imm2)
      (stw imm1 4 imm2)
      (stfs fp1 8 imm2)
      (stfd fp1 16 imm2)
      (restore-full-lisp-context)
      (li arg_z (target-nil-value))
      (bx lr))
    )

  #+ppc64-target
  (progn
  ;;; There are a few funky, non-obvious things going on here.
  ;;; The main %FF-CALL function uses WITH-VARIABLE-C-FRAME;
  ;;; the compiler will generate code to pop that frame off
  ;;; of the C/control stack, but the subprim that implements
  ;;; %ff-call has already popped it off.  To put things back
  ;;; in balance, the LAP function %%FF-RESULT pushes an
  ;;; extra frame on the cstack.
  ;;; %FF-CALL calls %%FF-RESULT to box the result, which may
  ;;; be in r3/imm0 or in fp1.  It's critical that the call
  ;;; to %%FF-RESULT not be compiled as "multiple-value returning",
  ;;; since the MV machinery may clobber IMM0.
    (defarmlapfunction %%ff-result ((spec arg_z))
      (stdu sp -160 sp)
      (ld arg_y ':void nfn)
      (cmpd cr0 spec arg_y)
      (ld arg_x ':address nfn)
      (cmpd cr1 spec arg_x)
      (ld temp3 ':single-float nfn)
      (cmpd cr2 spec temp3)
      (ld arg_y ':double-float nfn)
      (cmpd cr3 spec arg_y)
      (ld arg_x ':unsigned-doubleword nfn)
      (cmpd cr4 spec arg_x)
      (ld temp3 ':signed-doubleword nfn)
      (cmpd cr5 spec temp3)
      (beq cr0 @void)
      (beq cr1 @address)
      (beq cr2 @single-float)
      (beq cr3 @double-float)
      (beq cr4 @unsigned-doubleword)
      (beq cr5 @signed-doubleword)
      (box-fixnum arg_z imm0)
      (bx lr)
      @void
      (li arg_z nil)
      (bx lr)
      @address
      (li imm1 ppc64::macptr-header)
      (subi allocptr allocptr (- ppc64::macptr.size ppc64::fulltag-misc))
      (tdlt allocptr allocbase)
      (std imm1 ppc64::misc-header-offset allocptr)
      (mov arg_z allocptr)
      (clrrdi allocptr allocptr 4)
      (std imm0 ppc64::macptr.address arg_z)
      (bx lr)
      @single-float
      (put-single-float fp1 arg_z)
      (bx lr)
      @double-float
      (li imm1 ppc64::double-float-header)
      (subi allocptr allocptr (- ppc64::double-float.size ppc64::fulltag-misc))
      (tdlt allocptr allocbase)
      (std imm1 ppc64::misc-header-offset allocptr)
      (mov arg_z allocptr)
      (clrrdi allocptr allocptr 4)
      (stfd fp1 ppc64::macptr.address arg_z)
      (bx lr)
      @unsigned-doubleword
      (ba .SPmakeu64)
      @signed-doubleword
      (ba .SPmakes64))

  ;;; This is just here so that we can jump to a subprim from lisp.
    (defarmlapfunction %do-ff-call ((regbuf arg_y) (entry arg_z))
      (cmpdi cr0 regbuf nil)
      (bnea cr0 .SPpoweropen-ffcall-return-registers)
      (ba .SPpoweropen-ffcall))
  
    (defun %ff-call (entry &rest specs-and-vals)
      (declare (dynamic-extent specs-and-vals))
      (let* ((len (length specs-and-vals))
             (total-words 0)
             (registers nil))
        (declare (fixnum len total-words))
        (let* ((result-spec (or (car (last specs-and-vals)) :void))
               (nargs (ash (the fixnum (1- len)) -1)))
          (declare (fixnum nargs))
          (ecase result-spec
            ((:address :unsigned-doubleword :signed-doubleword
                       :single-float :double-float
                       :signed-fullword :unsigned-fullword
                       :signed-halfword :unsigned-halfword
                       :signed-byte :unsigned-byte
                       :void)
             (do* ((i 0 (1+ i))
                   (specs specs-and-vals (cddr specs))
                   (spec (car specs) (car specs)))
                  ((= i nargs))
               (declare (fixnum i))
               (case spec
                 (:registers nil)
                 ((:address :unsigned-doubleword :signed-doubleword
                            :single-float :double-float
                            :signed-fullword :unsigned-fullword
                            :signed-halfword :unsigned-halfword
                            :signed-byte :unsigned-byte
                            :hybrid-int-float :hybrid-float-float
                            :hybrid-float-int)
                  (incf total-words))
                 (t (if (typep spec 'unsigned-byte)
                      (incf total-words spec)
                      (error "unknown arg spec ~s" spec)))))
             (%stack-block ((fp-args (* 13 8)))
               (with-variable-c-frame
                   total-words frame
                   (with-macptrs ((argptr))
                     (%setf-macptr-to-object argptr frame)
                     (let* ((offset ppc64::c-frame.param0)
                            (n-fp-args 0))
                       (declare (fixnum offset n-fp-args))
                       (do* ((i 0 (1+ i))
                             (specs specs-and-vals (cddr specs))
                             (spec (car specs) (car specs))
                             (val (cadr specs) (cadr specs)))
                            ((= i nargs))
                         (declare (fixnum i))
                         (case spec
                           (:registers (setq registers val))
                           (:address (setf (%get-ptr argptr offset) val)
                                     (incf offset 8))
                           ((:signed-doubleword :signed-fullword :signed-halfword
                                                :signed-byte)
                          
                            (setf (%%get-signed-longlong argptr offset) val)
                            (incf offset 8))
                           ((:unsigned-doubleword :unsigned-fullword :unsigned-halfword
                                                  :unsigned-byte)
                            (setf (%%get-unsigned-longlong argptr offset) val)
                            (incf offset 8))
                           (:hybrid-int-float
                            (setf (%%get-unsigned-longlong argptr offset) val)
                            (when (< n-fp-args 13)
                              (setf (%get-double-float fp-args (* n-fp-args 8))
                                    (%double-float (%get-single-float argptr (+ offset 4)))))
                            (incf n-fp-args)
                            (incf offset 8))
                           (:hybrid-float-int
                            (setf (%%get-unsigned-longlong argptr offset) val)
                            (when (< n-fp-args 13)
                              (setf (%get-double-float fp-args (* n-fp-args 8))
                                    (%double-float (%get-single-float argptr offset))))
                            (incf n-fp-args)
                            (incf offset 8))
                           (:hybrid-float-float
                            (setf (%%get-unsigned-longlong argptr offset) val)
                            (when (< n-fp-args 13)
                              (setf (%get-double-float fp-args (* n-fp-args 8))
                                    (%double-float (%get-single-float argptr offset))))
                            (incf n-fp-args)
                            (when (< n-fp-args 13)
                              (setf (%get-double-float fp-args (* n-fp-args 8))
                                    (%double-float (%get-single-float argptr (+ offset 4)))))
                            (incf n-fp-args)
                            (incf offset 8))
                           (:double-float
                            (setf (%get-double-float argptr offset) val)
                            (when (< n-fp-args 13)
                              (setf (%get-double-float fp-args (* n-fp-args 8)) val))
                            (incf n-fp-args)
                            (incf offset 8))
                           (:single-float
                            (setf (%get-single-float argptr offset) val)
                            (when (< n-fp-args 13)
                              (setf (%get-double-float fp-args (* n-fp-args 8))
                                    (%double-float val)))
                            (incf n-fp-args)
                            (incf offset 8))
                           (t
                            (let* ((p 0))
                              (declare (fixnum p))
                              (dotimes (i (the fixnum spec))
                                (setf (%get-ptr argptr offset) (%get-ptr val p))
                                (incf p 8)
                                (incf offset 8))))))
                       (%load-fp-arg-regs n-fp-args fp-args)
                       (%do-ff-call registers entry)
                       (values (%%ff-result result-spec)))))))))))

    )
  )
)



(defarmlapfunction %get-object ((macptr arg_y) (offset arg_z))
  (check-nargs 2)
  (trap-unless-xtype= arg_y arm::subtag-macptr)
  (macptr-ptr imm0 arg_y)
  (trap-unless-fixnum arg_z)
  (unbox-fixnum imm1 arg_z)
  (ldr arg_z (:@ imm0 imm1))
  (bx lr))


(defarmlapfunction %set-object ((macptr arg_x) (offset arg_y) (value arg_z))
  (check-nargs 3)
  (trap-unless-xtype= arg_x arm::subtag-macptr)
  (macptr-ptr imm0 arg_x)
  (trap-unless-fixnum arg_y)
  (unbox-fixnum imm1 arg_y)
  (str arg_z (:@ imm0 imm1))
  (bx lr))


(defarmlapfunction %apply-lexpr-with-method-context ((magic arg_x)
                                                     (function arg_y)
                                                     (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in arm::next-method-context (= arm::temp1).
  ;; Put function in arm::nfn (= arm::temp2).
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves arm::nfn/arm::next-method-context.
  ;; Jump to the function in arm::nfn.
  (mov arm::next-method-context magic)
  (mov arm::nfn function)
  (set-nargs 0)
  (build-lisp-frame)
  (bla .SPspread-lexprz)
  (ldr lr (:@ sp (:$ arm::lisp-frame.savelr)))
  ;; Nothing's changed FN.
  ;;(ldr fn (:@ sp (:$ arm::lisp-frame.savefn)))
  (discard-lisp-frame)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))


(defarmlapfunction %apply-with-method-context ((magic arg_x)
                                               (function arg_y)
                                               (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in arm::next-method-context (= arm::temp1).
  ;; Put function in arm::nfn (= arm::temp2).
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves arm::nfn/arm::next-method-context.
  ;; Jump to the function in arm::nfn.
  (mov arm::next-method-context magic)
  (mov arm::nfn function)
  (set-nargs 0)
  (build-lisp-frame)
  (bla .SPspreadargZ)
  (ldr lr (:@ sp (:$ arm::lisp-frame.savelr)))
  ;; Nothing's changed FN.
  ;; (ldr fn (:@ sp (:$ arm::lisp-frame.savefn)))
  (discard-lisp-frame)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))




(defarmlapfunction %apply-lexpr-tail-wise ((method arg_y) (args arg_z))
  ;; This assumes
  ;; a) that "args" is a lexpr made via the .SPlexpr-entry mechanism
  ;; b) That the LR on entry to this function points to the lexpr-cleanup
  ;;    code that .SPlexpr-entry set up
  ;; c) That there weren't any required args to the lexpr, e.g. that
  ;;    (%lexpr-ref args (%lexpr-count args) 0) was the first arg to the gf.
  ;; The lexpr-cleanup code will be EQ to either (lisp-global ret1valaddr)
  ;; or (lisp-global lexpr-return1v).  In the former case, discard a frame
  ;; from the cstack (multiple-value tossing).  Restore FN and LR from
  ;; the first frame that .SPlexpr-entry pushed, restore vsp from (+
  ;; args node-size), pop the argregs, and jump to the function.
  (ref-global imm0 ret1valaddr)
  (cmp lr imm0)
  (ldr nargs (:@ args (:$ 0)))
  (mov nfn method)
  (addeq sp sp (:$ arm::lisp-frame.size))
  (ldr lr (:@ sp (:$ arm::lisp-frame.savelr)))
  (ldr fn (:@ sp (:$ arm::lisp-frame.savefn)))
  (ldr imm0 (:@ sp (:$ arm::lisp-frame.savevsp)))
  (sub vsp imm0 nargs)
  (add sp sp (:$ arm::lisp-frame.size))
  (cmp nargs (:$ 0))
  (ldreq pc (:@ nfn (:$ arm::function.entrypoint)))
  (cmp nargs '2)
  (vpop1 arg_z)
  (ldrlo pc (:@ nfn (:$ arm::function.entrypoint)))
  (vpop1 arg_y)
  (ldreq pc (:@ nfn (:$ arm::function.entrypoint)))
  (vpop1 arg_x)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))


(defun %copy-function (proto &optional target)
  (let* ((total-size (uvsize proto))
         (new (or target (allocate-typed-vector :function total-size))))
    (declare (fixnum total-size))
    (when target
      (unless (eql total-size (uvsize target))
        (error "Wrong size target ~s" target)))
    (%copy-gvector-to-gvector proto 0 new 0 total-size)
    (setf (%svref new 0 )arm::*function-initial-entrypoint*)
    new))

(defun replace-function-code (target-fn proto-fn)
  (if (typep target-fn 'function)
    (if (typep proto-fn 'function)
      (setf (uvref target-fn 0) arm::*function-initial-entrypoint*
            (uvref target-fn 1) (uvref proto-fn 1))
      (report-bad-arg proto-fn 'function))
    (report-bad-arg target-fn 'function)))

(defun closure-function (fun)
  (while (and (functionp fun)  (not (compiled-function-p fun)))
    (setq fun (%svref fun 2))
    (when (vectorp fun)
      (setq fun (svref fun 0))))
  fun)


;;; For use by (setf (apply ...) ...)
;;; (apply+ f butlast last) = (apply f (append butlast (list last)))
(defarmlapfunction apply+ ()
  (:arglist (function arg1 arg2 &rest other-args))
  (check-nargs 3 nil)
  (vpush1 arg_x)
  (mov temp0 arg_z)                     ; last
  (mov arg_z arg_y)                     ; butlast
  (sub nargs nargs '2)                  ; remove count for butlast & last
  (build-lisp-frame)
  (bla .SPspreadargz)
  (cmp nargs '3)
  (ldr lr (:@ sp (:$ arm::lisp-frame.savelr)))
  (discard-lisp-frame)
  (add nargs nargs '1)                  ; count for last
  (strhs arg_x (:@! vsp (:$ -4)))
  (mov arg_x arg_y)
  (mov arg_y arg_z)
  (mov arg_z temp0)
  (ldr nfn (:@ nfn 'funcall))
  (ba .SPfuncall))



;;; end of arm-def.lisp
