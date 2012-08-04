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

(defarmlapfunction %fix-fn-entrypoint ((func arg_z))
  (build-lisp-frame imm0)
  (ldr temp0 (:@ func (:$  arm::function.codevector)))
  (add lr temp0 (:$ arm::misc-data-offset))
  (str lr (:@ func (:$ arm::function.entrypoint)))
  (return-lisp-frame imm0))

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
  (spjump .SPmakeu32))



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
  (beq @ok1)
  (uuo-error-reg-not-xtype new-value (:$ arm::xtype-u32))
  @ok1
  (getvheader imm0 new-value)
  (header-length temp0 imm0)
  (cmp temp0 '2)
  (ldr imm2 (:@ new-value (:$ arm::misc-data-offset)))
  (ldreq imm1 (:@ new-value (:$ (+ arm::misc-data-offset))))
  (ble @ok2)
  (uuo-error-reg-not-xtype new-value (:$ arm::xtype-u32))
  @ok2
  (bne @one)
  (cmp imm1 (:$ 0))
  (beq @store)
  (uuo-error-reg-not-xtype new-value (:$ arm::xtype-u32))
  @one
  (cmp imm2 (:$ 0))
  (bge @store)
  (uuo-error-reg-not-xtype new-value (:$ arm::xtype-u32))
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
  (cmp imm1 (:$ arm::subtag-double-float-vector))
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
  (spjump .SPnvalret)
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
  (cmp imm0 (:lsl imm1 (:$ 2)))
  (movhs arg_z 'nil)
  (movlo arg_z (:lsl imm0 (:$ arm::fixnumshift)))
  (return-lisp-frame))

(defarmlapfunction %do-ff-call ((tag arg_x) (result arg_y) (entry arg_z))
  (stmdb (:! vsp) (tag result))
  (sploadlr .SPeabi-ff-call)
  (blx lr)
  (ldmia (:! vsp) (tag result))
  (macptr-ptr imm2 result)
  (str imm0 (:@ imm2 (:$ 0)))
  (str imm1 (:@ imm2 (:$ 4)))
  (vpush1 tag)
  (mov arg_z 'nil)
  (vpush1 arg_z)
  (set-nargs 1)
  (sploadlr .SPthrow)
  (blx lr))
  
(defun %ff-call (entry &rest specs-and-vals)
  (declare (dynamic-extent specs-and-vals))
  (let* ((len (length specs-and-vals))
         (total-words 0))
    (declare (fixnum len total-words))
    (let* ((result-spec (or (car (last specs-and-vals)) :void))
           (nargs (ash (the fixnum (1- len)) -1)))
      (declare (fixnum nargs))
      (if (and (arm-hard-float-p)
               (or (eq result-spec :double-float)
                   (eq result-spec :single-float)
                   (let* ((specs specs-and-vals))
                     (dotimes (i nargs)
                       (let* ((spec (car specs)))
                         (when (or (eq spec :double-float)
                                   (eq spec :single-float))
                           (return t)))))))
        (%ff-call-hard-float entry specs-and-vals)
               
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
               ((:address :single-float
                          :signed-fullword :unsigned-fullword
                          :signed-halfword :unsigned-halfword
                          :signed-byte :unsigned-byte)
                (incf total-words))
               ((:double-float :unsigned-doubleword :signed-doubleword)
                #-darwin-target
                (setq total-words (+ total-words (logand total-words 1)))
                (incf total-words 2))

               (t (if (typep spec 'unsigned-byte)
                    (incf total-words spec)
                    (error "unknown arg spec ~s" spec)))))
           ;; It's necessary to ensure that the C frame is the youngest thing on
           ;; the foreign stack here.
           (let* ((tag (cons nil nil)))
             (declare (dynamic-extent tag))
             (%stack-block ((result 8))
               (catch tag
                 (with-macptrs ((argptr))
                   (with-variable-c-frame
                       total-words frame
                       (%setf-macptr-to-object argptr frame)
                       (let* ((arg-offset 8))
                         (declare (fixnum arg-offset))
                         (do* ((i 0 (1+ i))
                               (specs specs-and-vals (cddr specs))
                               (spec (car specs) (car specs))
                               (val (cadr specs) (cadr specs)))
                              ((= i nargs))
                           (declare (fixnum i))
                           (case spec
                             (:address
                              (setf (%get-ptr argptr arg-offset) val)
                              (incf arg-offset 4))
                             (:signed-doubleword
                              #-darwin-target
                              (when (logtest 7 arg-offset)
                                (incf arg-offset 4))
                              (setf (%%get-signed-longlong argptr arg-offset) val)
                              (incf arg-offset 8))
                             ((:signed-fullword :signed-halfword :signed-byte)
                              (setf (%get-signed-long argptr arg-offset) val)
                              (incf arg-offset 4))
                             (:unsigned-doubleword
                              #-darwin-target
                              (when (logtest 7 arg-offset)
                                (incf arg-offset 4))
                              (setf (%%get-unsigned-longlong argptr arg-offset) val)
                              (incf arg-offset 8))
                             ((:unsigned-fullword :unsigned-halfword :unsigned-byte)
                              (setf (%get-unsigned-long argptr arg-offset) val)
                              (incf arg-offset 4))
                             (:double-float
                              #-darwin-target
                              (when (logtest 7 arg-offset)
                                (incf arg-offset 4))
                              (setf (%get-double-float argptr arg-offset) val)
                              (incf arg-offset 8))
                             (:single-float
                              (setf (%get-single-float argptr arg-offset) val)
                              (incf arg-offset 4))
                             (t
                              (let* ((p 0))
                                (declare (fixnum p))
                                (dotimes (i (the fixnum spec))
                                  (setf (%get-ptr argptr arg-offset) (%get-ptr val p))
                                  (incf p 4)
                                  (incf arg-offset 4)))))))
                       (%do-ff-call tag result entry))))
               (ecase result-spec
                 (:void nil)
                 (:address (%get-ptr result 0))
                 (:unsigned-byte (%get-unsigned-byte result 0))
                 (:signed-byte (%get-signed-byte result 0))
                 (:unsigned-halfword (%get-unsigned-word result 0))
                 (:signed-halfword (%get-signed-word result 0))
                 (:unsigned-fullword (%get-unsigned-long result 0))
                 (:signed-fullword (%get-signed-long result 0))
                 (:unsigned-doubleword (%%get-unsigned-longlong result 0))
                 (:signed-doubleword (%%get-signed-longlong result 0))
                 (:single-float (%get-single-float result 0))
                 (:double-float (%get-double-float result 0)))))))))))


(defarmlapfunction %do-ff-call-hard-float ((tag arg_x) (result arg_y) (entry arg_z))
  (stmdb (:! vsp) (tag result))
  (sploadlr .SPeabi-ff-callhf)
  (blx lr)
  (ldmia (:! vsp) (tag result))
  (macptr-ptr imm2 result)
  (str imm0 (:@ imm2 (:$ 0)))
  (str imm1 (:@ imm2 (:$ 4)))
  (fstd d0 (:@ imm2 (:$ 8)))
  (vpush1 tag)
  (mov arg_z 'nil)
  (vpush1 arg_z)
  (set-nargs 1)
  (sploadlr .SPthrow)
  (blx lr))

(defun %ff-call-hard-float (entry specs-and-vals)
  (let* ((len (length specs-and-vals))
         (total-words 0)
         (fp-words 16))
    (declare (fixnum len total-words fp-words))
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
             ((:address :signed-fullword :unsigned-fullword
                        :signed-halfword :unsigned-halfword
                        :signed-byte :unsigned-byte)
              (incf total-words))
             (:single-float
              (if (> fp-words 0)
                (decf fp-words)
                (incf total-words)))
             (:double-float
              (if (>= fp-words 2)
                (if (oddp fp-words)
                  (decf fp-words 3)
                  (decf fp-words 2))
                (if (oddp total-words)
                  (incf total-words 3)
                  (incf total-words 2))))
             ((:unsigned-doubleword :signed-doubleword)
              (setq total-words (+ total-words (logand total-words 1)))
              (incf total-words 2))

             (t (if (typep spec 'unsigned-byte)
                  (incf total-words spec)
                  (error "unknown arg spec ~s" spec)))))
         ;; It's necessary to ensure that the C frame is the youngest thing on
         ;; the foreign stack here.
         (let* ((tag (cons nil nil)))
           (declare (dynamic-extent tag))
           (%stack-block ((result 16))
             (catch tag
               (with-macptrs ((argptr))
                 (with-variable-c-frame
                     (+ total-words 16) frame
                     (%setf-macptr-to-object argptr frame)
                     (let* ((fp-arg-offset 8)
                            (arg-offset 72))
                       (declare (fixnum arg-offset fp-arg-offset))
                       (do* ((i 0 (1+ i))
                             (specs specs-and-vals (cddr specs))
                             (spec (car specs) (car specs))
                             (val (cadr specs) (cadr specs)))
                            ((= i nargs))
                         (declare (fixnum i))
                         (case spec
                           (:address
                            (setf (%get-ptr argptr arg-offset) val)
                            (incf arg-offset 4))
                           (:signed-doubleword
                            (when (logtest 7 arg-offset)
                              (incf arg-offset 4))
                            (setf (%%get-signed-longlong argptr arg-offset) val)
                            (incf arg-offset 8))
                           ((:signed-fullword :signed-halfword :signed-byte)
                            (setf (%get-signed-long argptr arg-offset) val)
                            (incf arg-offset 4))
                           (:unsigned-doubleword
                             (when (logtest 7 arg-offset)
                               (incf arg-offset 4))
                             (setf (%%get-unsigned-longlong argptr arg-offset) val)
                             (incf arg-offset 8))
                           ((:unsigned-fullword :unsigned-halfword :unsigned-byte)
                            (setf (%get-unsigned-long argptr arg-offset) val)
                            (incf arg-offset 4))
                           (:double-float
                            (cond ((<= fp-arg-offset 64)
                                   (when (logtest 7 fp-arg-offset)
                                     (incf fp-arg-offset 4))
                                   (setf (%get-double-float argptr fp-arg-offset) val)
                                   (incf fp-arg-offset 8))
                                  (t
                                   (when (logtest 7 arg-offset)
                                     (incf arg-offset 4))
                                   (setf (%get-double-float argptr arg-offset) val)
                                   (incf arg-offset 8))))
                           (:single-float
                            (cond ((< fp-arg-offset 72)
                                   (setf (%get-single-float argptr fp-arg-offset) val)
                                   (incf fp-arg-offset 4))
                                  (t
                                   (setf (%get-single-float argptr arg-offset) val)
                                   (incf arg-offset 4))))
                           (t
                              (let* ((p 0))
                                (declare (fixnum p))
                                (dotimes (i (the fixnum spec))
                                  (setf (%get-ptr argptr arg-offset) (%get-ptr val p))
                                  (incf p 4)
                                  (incf arg-offset 4)))))))
                         (%do-ff-call-hard-float tag result entry))))
             (ecase result-spec
               (:void nil)
               (:address (%get-ptr result 0))
               (:unsigned-byte (%get-unsigned-byte result 0))
               (:signed-byte (%get-signed-byte result 0))
               (:unsigned-halfword (%get-unsigned-word result 0))
               (:signed-halfword (%get-signed-word result 0))
               (:unsigned-fullword (%get-unsigned-long result 0))
               (:signed-fullword (%get-signed-long result 0))
               (:unsigned-doubleword (%%get-unsigned-longlong result 0))
               (:signed-doubleword (%%get-signed-longlong result 0))
               (:single-float (%get-single-float result 8))
               (:double-float (%get-double-float result 8))))))))))



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
  (sploadlr .SPspread-lexprz)
  (blx lr)
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
  (sploadlr .SPspreadargZ)
  (blx lr)
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
    (%fix-fn-entrypoint new)))

(defun replace-function-code (target-fn proto-fn)
  (if (typep target-fn 'function)
    (if (typep proto-fn 'function)
      (progn
        (setf (uvref target-fn 0) (%lookup-subprim-address
                                   #.(arm::arm-subprimitive-offset '.SPfix-nfn-entrypoint))
              (uvref target-fn 1) (uvref proto-fn 1))
        (%fix-fn-entrypoint target-fn))
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
  (sploadlr .SPspreadargz)
  (blx lr)
  (cmp nargs '3)
  (ldr lr (:@ sp (:$ arm::lisp-frame.savelr)))
  (discard-lisp-frame)
  (add nargs nargs '1)                  ; count for last
  (strhs arg_x (:@! vsp (:$ -4)))
  (mov arg_x arg_y)
  (mov arg_y arg_z)
  (mov arg_z temp0)
  (ldr nfn (:@ nfn 'funcall))
  (spjump .SPfuncall))

(defarmlapfunction %lookup-subprim-address ((subp arg_z))
  (ldr imm0 (:@ rcontext (:lsr subp (:$ arm::fixnumshift))))
  (box-fixnum arg_z imm0)
  (bx lr))

(defarmlapfunction arm-hard-float-p ()
  (check-nargs 0)
  (ref-global arg_z arm::float-abi)
  (tst arg_z arg_z)
  (mov arg_z 'nil)
  (addne arg_z arg_z (:$ arm::t-offset))
  (bx lr))
  
;;; end of arm-def.lisp
