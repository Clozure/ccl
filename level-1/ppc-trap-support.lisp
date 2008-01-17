;;; ppc-trap-support
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

;;; Support for PPC traps, this includes the event-poll trap
;;; and all the trxxx traps for type checks & arg count checks.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")

  
  (defparameter *ppc-instruction-fields*
    `((:opcode . ,(byte 6 26))
      (:rt . ,(byte 5 21))
      (:to . ,(byte 5 21))
      (:ra . ,(byte 5 16))
      (:rb . ,(byte 5 11))
      (:d . ,(byte 16 0))
      (:ds . ,(byte 14 2))
      (:ds-xo . ,(byte 2 0))
      (:sh . ,(byte 5 11))
      (:mb . ,(byte 5 6))
      (:me . ,(byte 5 1))
      (:mb6 . ,(byte 6 5))
      (:me6 . ,(byte 6 5))
      (:sh6 . ,(byte 1 1))
      (:x-minor . ,(byte 10 1))
      (:fulltag32 . ,(byte ppc32::ntagbits 0))
      (:lisptag32 . ,(byte ppc32::nlisptagbits 0))
      (:fulltag64 . ,(byte ppc64::ntagbits 0))
      (:lisptag64 . ,(byte ppc64::nlisptagbits 0))
      (:lowtag64 . ,(byte ppc64::nlowtagbits 0))))
  
  (defun ppc-instruction-field (field-name)
    (or (cdr (assoc field-name *ppc-instruction-fields*))
	(error "Unknown PPC instruction field: ~s" field-name)))
  
  (defun ppc-instruction-field-mask (field-spec)
    (let* ((name (if (atom field-spec) field-spec (car field-spec)))
	   (value (if (atom field-spec) -1 (cadr field-spec))))
      (dpb value (ppc-instruction-field name) 0)))

  #+darwinppc-target
  (progn
    (def-foreign-type nil
        (:struct :darwin-ppc-float-state
                 (:fpregs (:array :double 32))
                 (:fpscr-pad (:unsigned 32))
                 (:fpscr (:unsigned 32))))
    (def-foreign-type nil
        (:struct :darwin-ppc-vector-state
                 (:save-vr (:array (:array (:unsigned 32) 4) 32))
                 (:save-vscr (:array (:unsigned 32) 4))
                 (:save-pad5 (:array (:unsigned 32) 4))
                 (:save-vrvalid (:unsigned 32))
                 (:save-pad6 (:array (:unsigned 32) 7))))
    #+ppc64-target
    (progn
      (def-foreign-type nil
          (:struct :darwin-ppc-exception-state64
                   (:dar (:unsigned 64))
                   (:dsisr (:unsigned 32))
                   (:exception (:unsigned 32))
                   (:pad1 (:array (:unsigned 32) 4))))
      (def-foreign-type nil
          ;; The real record type is defined with
          ;; #pragma pack(4) in effect.
          ;; The :struct parser should really accept
          ;; some option to deal with that, but Apple
          ;; should also stop mis-aligning things.
          (:struct :darwin-ppc-thread-state64
                   (:srr0 (:unsigned 64))
                   (:srr1 (:unsigned 64))
                   (:r0  (:unsigned 64))
                   (:r1  (:unsigned 64))
                   (:r2  (:unsigned 64))
                   (:r3  (:unsigned 64))
                   (:r4  (:unsigned 64))
                   (:r5  (:unsigned 64))
                   (:r6  (:unsigned 64))
                   (:r7  (:unsigned 64))
                   (:r8  (:unsigned 64))
                   (:r9  (:unsigned 64))
                   (:r10  (:unsigned 64))
                   (:r11  (:unsigned 64))
                   (:r12 (:unsigned 64))
                   (:r13  (:unsigned 64))
                   (:r14  (:unsigned 64))
                   (:r15  (:unsigned 64))
                   (:r16  (:unsigned 64))
                   (:r17  (:unsigned 64))
                   (:r18  (:unsigned 64))
                   (:r19  (:unsigned 64))
                   (:r20  (:unsigned 64))
                   (:r21  (:unsigned 64))
                   (:r22  (:unsigned 64))
                   (:r23  (:unsigned 64))
                   (:r24  (:unsigned 64))
                   (:r25  (:unsigned 64))
                   (:r26  (:unsigned 64))
                   (:r27  (:unsigned 64))
                   (:r28  (:unsigned 64))
                   (:r29  (:unsigned 64))
                   (:r30  (:unsigned 64))
                   (:r31  (:unsigned 64))
                   (:cr   (:unsigned 32))
                   (:xer  (:unsigned 32))
                   (:xer-low (:unsigned 32))
                   (:lr   (:unsigned 32))
                   (:lr-low (:unsigned 32))
                   (:ctr  (:unsigned 32))
                   (:ctr-low (:unsigned 32))
                   (:vrsave (:unsigned 32))))
      (def-foreign-type nil
          (:struct :darwin-sigaltstack64
                   (:ss-sp (:* :void))
                   (:ss-size (:unsigned 64))
                   (:ss-flags (:unsigned 32))))
      (def-foreign-type nil
          (:struct :darwin-mcontext64
                   (:es (:struct :darwin-ppc-exception-state64))
                   (:ss (:struct :darwin-ppc-thread-state64))
                   (:fs (:struct :darwin-ppc-float-state))
                   (:vs (:struct :darwin-ppc-vector-state))))
      (def-foreign-type nil
          (:struct :darwin-ucontext64
                   (:uc-onstack (:signed 32))
                   (:uc-sigmask (:signed 32))
                   (:uc-stack (:struct :darwin-sigaltstack64))
                   (:uc-link (:* (:struct :darwin-ucontext64)))
                   (:uc-mcsize (:signed 64))
                   (:uc-mcontext64 (:* (:struct :darwin-mcontext64)))))
      )
    #+ppc32-target
    (progn
      (def-foreign-type nil
          (:struct :darwin-ppc-exception-state32
                   (:dar (:unsigned 32))
                   (:dsisr (:unsigned 32))
                   (:exception (:unsigned 32))
                   (:pad0 (:unsigned 32))
                   (:pad1 (:array (:unsigned 32) 4))))
      (def-foreign-type nil
          (:struct :darwin-ppc-thread-state32
                   (:srr0 (:unsigned 32))
                   (:srr1 (:unsigned 32))
                   (:r0  (:unsigned 32))
                   (:r1  (:unsigned 32))
                   (:r2  (:unsigned 32))
                   (:r3  (:unsigned 32))
                   (:r4  (:unsigned 32))
                   (:r5  (:unsigned 32))
                   (:r6  (:unsigned 32))
                   (:r7  (:unsigned 32))
                   (:r8  (:unsigned 32))
                   (:r9  (:unsigned 32))
                   (:r10  (:unsigned 32))
                   (:r11  (:unsigned 32))
                   (:r12 (:unsigned 32))
                   (:r13  (:unsigned 32))
                   (:r14  (:unsigned 32))
                   (:r15  (:unsigned 32))
                   (:r16  (:unsigned 32))
                   (:r17  (:unsigned 32))
                   (:r18  (:unsigned 32))
                   (:r19  (:unsigned 32))
                   (:r20  (:unsigned 32))
                   (:r21  (:unsigned 32))
                   (:r22  (:unsigned 32))
                   (:r23  (:unsigned 32))
                   (:r24  (:unsigned 32))
                   (:r25  (:unsigned 32))
                   (:r26  (:unsigned 32))
                   (:r27  (:unsigned 32))
                   (:r28  (:unsigned 32))
                   (:r29  (:unsigned 32))
                   (:r30  (:unsigned 32))
                   (:r31  (:unsigned 32))
                   (:cr   (:unsigned 32))
                   (:xer  (:unsigned 32))
                   (:lr   (:unsigned 32))
                   (:ctr  (:unsigned 32))
                   (:mq (:unsigned 32)) ; ppc 601!
                   (:vrsave (:unsigned 32))))
      (def-foreign-type nil
          (:struct :darwin-sigaltstack32
                   (:ss-sp (:* :void))
                   (:ss-size (:unsigned 32))
                   (:ss-flags (:unsigned 32))))
      (def-foreign-type nil
          (:struct :darwin-mcontext32
                   (:es (:struct :darwin-ppc-exception-state32))
                   (:ss (:struct :darwin-ppc-thread-state32))
                   (:fs (:struct :darwin-ppc-float-state))
                   (:vs (:struct :darwin-ppc-vector-state))))
      (def-foreign-type nil
          (:struct :darwin-ucontext32
                   (:uc-onstack (:signed 32))
                   (:uc-sigmask (:signed 32))
                   (:uc-stack (:struct :darwin-sigaltstack32))
                   (:uc-link (:* (:struct :darwin-ucontext32)))
                   (:uc-mcsize (:signed 32))
                   (:uc-mcontext32 (:* (:struct :darwin-mcontext32)))))
      )
    )
      
                   
            

  (defmacro with-xp-registers-and-gpr-offset ((xp register-number) (registers offset) &body body)
    (let* ((regform  #+linuxppc-target
                     `(pref ,xp :ucontext.uc_mcontext.regs)
                     #+darwinppc-target
                     (target-arch-case
                      ;; Gak.  Apple gratuitously renamed things
                      ;; for Leopard.  Hey, it's not as if anyone
                      ;; has better things to do than to deal with
                      ;; this crap ...
                      (:ppc32 `(pref ,xp :darwin-ucontext32.uc-mcontext32.ss))
                      (:ppc64 `(pref ,xp :darwin-ucontext64.uc-mcontext64.ss)))))
    `(with-macptrs ((,registers ,regform))
      (let ((,offset (xp-gpr-offset ,register-number)))
	,@body))))

  (defmacro RA-field (instr)
    `(ldb (byte 5 16) ,instr))

  (defmacro RB-field (instr)
    `(ldb (byte 5 11) ,instr))

  (defmacro D-field (instr)
    `(ldb (byte 16 0) ,instr))

  (defmacro RS-field (instr)
    `(ldb (byte 5 21) ,instr))
  
  (defmacro lisp-reg-p (reg)
    `(>= ,reg ppc::fn))
  
  (defmacro ppc-lap-word (instruction-form)
    (uvref (uvref (compile nil
                           `(lambda (&lap 0)
			     (ppc-lap-function () ((?? 0))
			      ,instruction-form)))
		  
                  0) #+ppc32-host 0 #+ppc64-host 1))
  
  (defmacro ppc-instruction-mask (&rest fields)
    `(logior ,@(mapcar #'ppc-instruction-field-mask (cons :opcode fields))))
  
  )  



(defun xp-gpr-offset (register-number)
  (unless (and (fixnump register-number)
               (<= -2 (the fixnum register-number))
               (< (the fixnum register-number) 48))
    (setq register-number (require-type register-number '(integer -2 48))))
  (the fixnum 
    (* (the fixnum #+linuxppc-target register-number
	           #+darwinppc-target (+ register-number 2))
       target::node-size)))



(defun xp-gpr-lisp (xp register-number)
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%get-object registers offset))))

(defun (setf xp-gpr-lisp) (value xp register-number)
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (%set-object registers offset value)))

(defun xp-gpr-signed-long (xp register-number)
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%get-signed-long registers offset))))

(defun xp-gpr-signed-doubleword (xp register-number)
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%%get-signed-longlong registers offset))))
  

(defun xp-gpr-macptr (xp register-number)
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%get-ptr registers offset))))

(defun xp-argument-list (xp)
  (let ((nargs (xp-gpr-lisp xp ppc::nargs))     ; tagged as a fixnum (how convenient)
        (arg-x (xp-gpr-lisp xp ppc::arg_x))
        (arg-y (xp-gpr-lisp xp ppc::arg_y))
        (arg-z (xp-gpr-lisp xp ppc::arg_z)))
    (cond ((eql nargs 0) nil)
          ((eql nargs 1) (list arg-z))
          ((eql nargs 2) (list arg-y arg-z))
          (t (let ((args (list arg-x arg-y arg-z)))
               (if (eql nargs 3)
                 args
                 (let ((vsp (xp-gpr-macptr xp ppc::vsp)))
                   (dotimes (i (- nargs 3))
                     (push (%get-object vsp (* i target::node-size)) args))
                   args)))))))
    
(defun xp-fpscr-info (xp)
  (let* ((fpscr #+(and linuxppc-target 32-bit-target) (%get-unsigned-long (pref xp :ucontext.uc_mcontext.regs) (ash #$PT_FPSCR 2))
                #+(and linuxppc-target 64-bit-target)
                (%get-unsigned-long (pref xp :ucontext.uc_mcontext.fp_regs) (ash 65 2))
		#+(and darwinppc-target ppc32-target)
                (pref xp :darwin-ucontext32.uc-mcontext32.fs.fpscr)
                #+(and darwinppc-target ppc64-target)
                (pref xp :darwin-ucontext64.uc-mcontext64.fs.fpscr)))
    (values (ldb (byte 24 8) fpscr) (ldb (byte 8 0) fpscr))))

#+linuxppc-target
(defun xp-double-float (xp fpr)
  #+32-bit-target
  (%get-double-float (pref xp :ucontext.uc_mcontext.regs) (+ (ash #$PT_FPR0 2)  (ash fpr 3)))
  #+64-bit-target
  (%get-double-float (pref xp :ucontext.uc_mcontext.fp_regs) (ash fpr 3))
  )

#+darwinppc-target
(defun xp-double-float (xp fpr)
  (%get-double-float
     #+ppc32-target (pref xp :darwin-ucontext32.uc-mcontext32.fs)
     #+ppc64-target (pref xp :darwin-ucontext64.uc-mcontext64.fs)
     (ash fpr 3)))


(defparameter *trap-lookup-tries* 5)



(defun %scan-for-instr (mask opcode fn pc-index tries)
  (let ((code-vector (and fn (uvref fn 0)))
        (offset 0))
    (declare (fixnum offset))
    (flet ((get-instr ()
             (if code-vector
               (let ((index (+ pc-index offset)))
                 (when (< index 0) (return-from %scan-for-instr nil))
                 (uvref code-vector index))
               (%get-long pc-index (the fixnum (* 4 offset))))))
      (declare (dynamic-extent #'get-instr))
      (dotimes (i tries)
        (decf offset)
        (let ((instr (get-instr)))
          (when (match-instr instr mask opcode)
            (return instr))
          (when (codevec-header-p instr)
            (return nil)))))))






(defun return-address-offset (xp fn machine-state-offset)
  (with-macptrs ((regs (pref xp #+linuxppc-target :ucontext.uc_mcontext.regs
			        #+(and darwinppc-target ppc32-target)
                                :darwin-ucontext32.uc-mcontext32
                                #+(and darwinppc-target ppc64-target)
                                :darwin-ucontext64.uc-mcontext64)))
    (if (functionp fn)
      (or (%code-vector-pc (uvref fn 0) (%inc-ptr regs machine-state-offset))
           (%get-ptr regs machine-state-offset))
      (%get-ptr regs machine-state-offset))))

(defconstant lr-offset-in-register-context
  #+linuxppc-target (ash #$PT_LNK target::word-shift)
  #+(and darwinppc-target ppc32-target)
  (+ (get-field-offset :darwin-mcontext32.ss)
     (get-field-offset :darwin-ppc-thread-state32.lr))
  #+(and darwinppc-target ppc64-target)
  (+ (get-field-offset :darwin-mcontext64.ss)
     (get-field-offset :darwin-ppc-thread-state64.lr)))

(defconstant pc-offset-in-register-context
  #+linuxppc-target (ash #$PT_NIP target::word-shift)
  #+(and darwinppc-target ppc32-target)
  (+ (get-field-offset :darwin-mcontext32.ss)
     (get-field-offset :darwin-ppc-thread-state32.srr0))
  #+(and darwinppc-target ppc64-target)
  (+ (get-field-offset :darwin-mcontext64.ss)
     (get-field-offset :darwin-ppc-thread-state64.srr0)))

;;; When a trap happens, we may have not yet created control
;;; stack frames for the functions containing PC & LR.
;;; If that is the case, we add fake-stack-frame's to *fake-stack-frames*
;;; There are 4 cases:
;;;
;;; PC in FN
;;;   Push 1 stack frame: PC/FN
;;;   This might miss one recursive call, but it won't miss any variables
;;; PC in NFN
;;;   Push 2 stack frames:
;;;   1) PC/NFN/VSP
;;;   2) LR/FN/VSP
;;;   This might think some of NFN's variables are part of FN's stack frame,
;;;   but that's the best we can do.
;;; LR in FN
;;;   Push 1 stack frame: LR/FN
;;; None of the above
;;;   Push no new stack frames
;;;
;;; The backtrace support functions in "ccl:l1;l1-lisp-threads.lisp" know how
;;; to find the fake stack frames and handle them as arguments.
(defun funcall-with-xp-stack-frames (xp trap-function thunk)
  (cond ((null trap-function)
         ; Maybe inside a subprim from a lisp function
         (let* ((fn (xp-gpr-lisp xp ppc::fn))
                (lr (return-address-offset
                     xp fn lr-offset-in-register-context)))
           (if (fixnump lr)
             (let* ((sp (xp-gpr-lisp xp ppc::sp))
                    (vsp (xp-gpr-lisp xp ppc::vsp))
                    (frame (%cons-fake-stack-frame sp sp fn lr vsp xp *fake-stack-frames*))
                    (*fake-stack-frames* frame))
               (declare (dynamic-extent frame))
               (funcall thunk frame))
             (funcall thunk (xp-gpr-lisp xp ppc::sp)))))
        ((eq trap-function (xp-gpr-lisp xp ppc::fn))
         (let* ((sp (xp-gpr-lisp xp ppc::sp))
                (fn trap-function)
                (lr (return-address-offset
                     xp fn pc-offset-in-register-context))
                (vsp (xp-gpr-lisp xp ppc::vsp))
                (frame (%cons-fake-stack-frame sp sp fn lr vsp xp *fake-stack-frames*))
                (*fake-stack-frames* frame))
           (declare (dynamic-extent frame))
           (funcall thunk frame)))
        ((eq trap-function (xp-gpr-lisp xp ppc::nfn))
         (let* ((sp (xp-gpr-lisp xp ppc::sp))
                (fn (xp-gpr-lisp xp ppc::fn))
                (lr (return-address-offset
                     xp fn lr-offset-in-register-context))
                (vsp (xp-gpr-lisp xp ppc::vsp))
                (lr-frame (%cons-fake-stack-frame sp sp fn lr vsp xp))
                (pc-fn trap-function)
                (pc-lr (return-address-offset
                        xp pc-fn pc-offset-in-register-context))
                (pc-frame (%cons-fake-stack-frame sp lr-frame pc-fn pc-lr vsp xp *fake-stack-frames*))
                (*fake-stack-frames* pc-frame))
           (declare (dynamic-extent lr-frame pc-frame))
           (funcall thunk pc-frame)))
        (t (funcall thunk (xp-gpr-lisp xp ppc::sp)))))



;;; Enter here from handle-trap in "lisp-exceptions.c".
;;; xp is a pointer to an ExceptionInformationPowerPC record.
;;; the-trap is the trap instruction that got us here.
;;; fn-reg is either fn, nfn or 0. If it is fn or nfn, then
;;; the trap occcurred in that register's code vector.
;;; If it is 0, then the trap occurred somewhere else.
;;; pc-index is either the index in fn-reg's code vector
;;; or, if fn-reg is 0, the address of the PC at the trap instruction.
;;; This code parallels the trap decoding code in
;;; "lisp-exceptions.c" that runs if (symbol-value 'cmain)
;;; is not a macptr.
;;; Some of these could probably call %err-disp instead of error,
;;; but I was too lazy to look them up.

#+ppc32-target
(defcallback xcmain (:without-interrupts t
					:address xp 
					:unsigned-fullword fn-reg 
					:address pc-or-index 
					:unsigned-fullword the-trap
					:signed-fullword  arg-0
					:signed-fullword arg-1)
  ;; twgti nargs,0
  ;; time for event polling.
  ;; This used to happen a lot so we test for it first.
  (let ((fn (unless (eql fn-reg 0) (xp-gpr-lisp xp fn-reg))))
    (with-xp-stack-frames (xp fn frame-ptr)
      (if (eql the-trap (ppc-lap-word (twgti nargs 0)))
        (cmain)
        (with-error-reentry-detection
          (let ((pc-index (if (eql fn-reg 0) pc-or-index (%ptr-to-int pc-or-index)))
                instr ra temp rs condition)
            (cond
              ((= the-trap #$SIGBUS)
               (%error (make-condition 'invalid-memory-access
                                       :address arg-0
                                       :write-p (not (zerop arg-1)))
                       ()
                       frame-ptr))              
             ;; tweqi RA nil-value - resolve-eep, or resolve-foreign-variable
	      ((and (match-instr the-trap
				 (ppc-instruction-mask  :opcode :to :d)
				 (ppc-lap-word (tweqi ?? ppc32::nil-value)))
		    (setq instr (scan-for-instr
				 (ppc-instruction-mask :opcode :d)
				 (ppc-lap-word (lwz ??
						    (+ 4 ppc32::misc-data-offset)
						    ??))
                                               fn pc-index)))
	       (let* ((eep-or-fv (xp-gpr-lisp xp (RA-field instr))))
                 (etypecase eep-or-fv
                   (external-entry-point
                    (resolve-eep eep-or-fv)
                    (setf (xp-gpr-lisp xp (RA-field the-trap))
                          (eep.address eep-or-fv)))
                   (foreign-variable
                    (resolve-foreign-variable eep-or-fv)
                    (setf (xp-gpr-lisp xp (RA-field the-trap))
                          (fv.addr eep-or-fv))))))
             ;; twnei RA,N; RA = nargs
             ;; nargs check, no optional or rest involved
	      ((match-instr the-trap
                           (ppc-instruction-mask :opcode :to :ra)
                           (ppc-lap-word (twnei nargs ??)))
              (%error (if (< (xp-GPR-signed-long xp ppc::nargs) (D-field the-trap))
                        'too-few-arguments
                        'too-many-arguments )
                      (list :nargs (ash (xp-GPR-signed-long xp ppc::nargs)
					(- ppc32::fixnumshift))
			    :fn  fn)
                      frame-ptr))
             
             ;; twnei RA,N; RA != nargs, N = fulltag_node/immheader
             ;; type check; look for "lbz rt-imm,-3(ra-node)"
             ((and (or (match-instr the-trap
                                    (ppc-instruction-mask :opcode :to :fulltag32)
                                    (ppc-lap-word (twnei ?? ppc32::fulltag-nodeheader)))
                       (match-instr the-trap
                                    (ppc-instruction-mask :opcode :to :fulltag32)
                                    (ppc-lap-word (twnei ?? ppc32::fulltag-immheader))))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :d)
                                               (ppc-lap-word (lbz ?? ppc32::misc-subtag-offset ??))
                                               fn pc-index))
                   (lisp-reg-p (setq ra (RA-field instr))))
              (let* ((typecode (D-field the-trap))
                     (type-tag (logand typecode ppc32::fulltagmask))
                     (type-name (svref (if (eql type-tag ppc32::fulltag-nodeheader)
                                         *nodeheader-types*
                                         *immheader-types*)
                                       (ldb (byte (- ppc32::num-subtag-bits ppc32::ntagbits) ppc32::ntagbits) typecode))))
                (%error (make-condition 'type-error
                                        :format-control (%rsc-string $XWRONGTYPE)
                                        :datum (xp-GPR-lisp xp ra)
                                        :expected-type type-name)
                        nil
                        frame-ptr)))

             ;; twnei RA,N; RA != nargs, N = subtag_character
             ;; type check; look for "clrlwi rs-node,ra-imm,24" = "rlwinm rs,ra,0,24,31"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :d)
                                (ppc-lap-word (twnei ?? ppc32::subtag-character)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :rb :mb :me)
                                               (ppc-lap-word (rlwinm ?? ?? 0 24 31))
                                               fn pc-index))
                   (lisp-reg-p (setq rs (RS-field instr))))
              (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type 'character)
                        nil
                        frame-ptr))

             ;; twnei RA,N; RA != nargs, N != fulltag_node/immheader
             ;; (since that case was handled above.)
             ;; type check; look for "clrlwi rs-node,ra-imm,29/30" = "rlwinm rs,ra,0,29/30,31"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to) 
                                (ppc-lap-word (twnei ?? ??)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :rb (:mb 28) :me)
                                               (ppc-lap-word (rlwinm ?? ?? 0 28 31))                                               
                                               fn pc-index))
                   (or (eql (- 32 ppc32::ntagbits) (setq temp (ldb #.(ppc-instruction-field :mb) instr)))
                       (eql (- 32 ppc32::nlisptagbits) temp))
                   (lisp-reg-p (setq rs (RS-field instr))))
              (let* ((tag (logand the-trap ppc32::tagmask))
                     (type-name 
                      (case tag
                        (#.ppc32::tag-fixnum 'fixnum)
                        (#.ppc32::tag-list (if (eql temp (- 32 ppc32::ntagbits)) 'cons 'list))
                        (#.ppc32::tag-misc 'uvector)
                        (#.ppc32::tag-imm 'immediate))))                                      
                (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type type-name)
                        nil
                        frame-ptr)))
             
             ;; twlgti RA,N; RA = nargs (xy = 01)
             ;; twllti RA,N; RA = nargs (xy = 10)
             ;; nargs check, optional or rest involved
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode (:to #x1c) :ra)
                                (ppc-lap-word (twi ?? ppc::nargs ??)))
                   (or (eql #b01 (setq temp (ldb #.(ppc-instruction-field :to) the-trap)))
	               (eql #b10 temp)))
              (%error (if (eql temp #b10)
                        'too-few-arguments
                        'too-many-arguments)
                      (list :nargs (ash (xp-GPR-signed-long xp ppc::nargs)
					(- ppc32::fixnumshift))
			    :fn  fn)
                      frame-ptr))
             
             ;; tweqi RA,N; N = unbound
             ;; symeval boundp check; look for "lwz RA,symbol.vcell(nodereg)"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :d)                                
                                (ppc-lap-word (tweqi ?? ppc32::unbound-marker)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :d)
                                               (ppc-lap-word (lwz ?? ppc32::symbol.vcell ??))                                               
                                               fn pc-index))
                   (lisp-reg-p (setq ra (RA-field instr))))
              (setf (xp-GPR-lisp xp (RA-field the-trap))
                    (%kernel-restart-internal $xvunbnd (list (xp-GPR-lisp xp ra)) frame-ptr)))
	     ;; tweqi RA,N: n = (%slot-unbound-marker)
	     ;; slot-unbound trap.  Look for preceding "lwzx RA,rx,ry".
	     ;; rx = slots-vector, ry = scaled index in slots vector.
	     ((and (match-instr the-trap
				(ppc-instruction-mask :opcode :to :d)
				(ppc-lap-word (tweqi ?? ppc32::slot-unbound-marker)))
		   (setq instr (scan-for-instr (ppc-instruction-mask
						:opcode :rt  :x-minor)
					       (dpb
						(RA-field the-trap)
						(byte 5 21)
						(ppc-lap-word
						 (lwzx ?? ?? ??)))
					       fn pc-index)))
              (setq *error-reentry-count* 0)  ; succesfully reported error

              ;; %SLOT-UNBOUND-TRAP will decode the arguments further,
              ;; then call the generic function SLOT-UNBOUND.  That
              ;; might return a value; if so, set the value of the
              ;; register that caused the trap to that value.
              (setf (xp-gpr-lisp xp (ra-field the-trap))
                    (%slot-unbound-trap (xp-gpr-lisp xp (RA-field instr))
                                        (ash (- (xp-gpr-signed-long xp (RB-field instr))
                                                ppc32::misc-data-offset)
                                             (- ppc32::word-shift))
                                        frame-ptr)))
             ;; twlge RA,RB
             ;; vector bounds check; look for "lwz immreg, misc_header_offset(nodereg)"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :x-minor)                                
                                (ppc-lap-word (twlge 0 0)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode #|:d|#)
                                               (ppc-lap-word (lwz ?? ?? #|ppc32::misc-header-offset|# ??))
                                               fn pc-index))
                   (lisp-reg-p (setq ra (RA-field instr))))
              (%error (%rsc-string $xarroob)
                      (list (xp-GPR-lisp xp (RA-field the-trap))
                            (xp-GPR-lisp xp ra))
                      frame-ptr))
             ;; twi 27 ra d - array header rank check
	     ((and (match-instr the-trap
				(ppc-instruction-mask :opcode :to)
				(ppc-lap-word (twi 27 ?? ??)))
		   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :d)
                                               (ppc-lap-word (lwz ?? ppc32::arrayH.rank ??))
                                               fn pc-index))
		   (lisp-reg-p (setq ra (RA-field instr))))
	      (%error (%rsc-string $xndims)
		      (list (xp-gpr-lisp xp ra)
			    (ash (ldb (byte 16 0) the-trap) (- ppc32::fixnumshift)))
		      frame-ptr))
	     ;; tw 27 ra rb - array flags check
	     ((and (match-instr the-trap
				(ppc-instruction-mask :opcode :to :x-minor)
				(ppc-lap-word (tw 27 ?? ??)))
		   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :d)
                                               (ppc-lap-word (lwz ?? ppc32::arrayH.flags ??))
                                               fn pc-index))
		   (lisp-reg-p (setq ra (RA-field instr)))
		   (let* ((expected (xp-gpr-lisp xp (RB-field the-trap)))
			  (expected-subtype (ldb
					     ppc32::arrayH.flags-cell-subtag-byte
					     expected))
			  (expect-simple (=
					  (ldb ppc32::arrayH.flags-cell-bits-byte
					       expected)
					  (ash 1 $arh_simple_bit)))
			  (type-name
			   (case expected-subtype
			     (#.ppc32::subtag-double-float-vector 'double-float))))

		     (and type-name expect-simple
			  (setq condition
				(make-condition 'type-error
						:datum (xp-gpr-lisp xp ra)
						:expected-type
						`(simple-array ,type-name))))))
	      (%error condition nil frame-ptr))
			       
             ;; Unknown trap
             (t (%error "Unknown trap: #x~x~%xp: ~s, fn: ~s, pc: #x~x"
                        (list the-trap xp fn (ash pc-index ppc32::fixnumshift))
                        frame-ptr)))))))))

#+ppc64-target
(defcallback xcmain (:without-interrupts t
					:address xp 
					:unsigned-fullword fn-reg 
					:address pc-or-index 
					:unsigned-fullword the-trap
					:signed-doubleword  arg0
					:signed-doubleword arg1)
  ;; tdgti nargs,0
  ;; time for event polling.
  ;; This used to happen a lot so we test for it first.
  (let ((fn (unless (eql fn-reg 0) (xp-gpr-lisp xp fn-reg))))
    (with-xp-stack-frames (xp fn frame-ptr)
      (if (eql the-trap (ppc-lap-word (tdgti nargs 0)))
        (cmain)
        (with-error-reentry-detection
          (let ((pc-index (if (eql fn-reg 0) pc-or-index (%ptr-to-int pc-or-index)))
                instr ra temp rs condition)
            (cond
              ;; tdeqi RA nil-value - resolve-eep, or resolve-foreign-variable
	      ((and (match-instr the-trap
				 (ppc-instruction-mask  :opcode :to :d)
				 (ppc-lap-word (tdeqi ?? ppc64::nil-value)))
		    (setq instr (scan-for-instr
				 (ppc-instruction-mask :opcode :ds :ds-xo)
				 (ppc-lap-word (ld ??
						    (+ 8 ppc64::misc-data-offset)
						    ??))
                                               fn pc-index)))
	       (let* ((eep-or-fv (xp-gpr-lisp xp (RA-field instr))))
                 (etypecase eep-or-fv
                   (external-entry-point
                    (resolve-eep eep-or-fv)
                    (setf (xp-gpr-lisp xp (RA-field the-trap))
                          (eep.address eep-or-fv)))
                   (foreign-variable
                    (resolve-foreign-variable eep-or-fv)
                    (setf (xp-gpr-lisp xp (RA-field the-trap))
                          (fv.addr eep-or-fv))))))
              ((= the-trap #$SIGBUS)
               (%error (make-condition 'invalid-memory-access
                                       :address arg0
                                       :write-p (not (zerop arg1)))
                       ()
                       frame-ptr))
              ;; tdnei RA,N; RA = nargs
              ;; nargs check, no optional or rest involved
	      ((match-instr the-trap
                           (ppc-instruction-mask :opcode :to :ra)
                           (ppc-lap-word (tdnei nargs ??)))
              (%error (if (< (xp-GPR-signed-doubleword xp ppc::nargs) (D-field the-trap))
                        'too-few-arguments
                        'too-many-arguments )
                      (list :nargs (ash (xp-GPR-signed-doubleword xp ppc::nargs)
					(- ppc64::fixnumshift))
			    :fn  fn)
                      frame-ptr))
             
             ;; tdnei RA,N; RA != nargs, N = lowtag_node/immheader
             ;; type check; look for "lbz rt-imm,-5(ra-node)"
             ((and (or (match-instr the-trap
                                    (ppc-instruction-mask :opcode :to :lowtag64)
                                    (ppc-lap-word (tdnei ?? ppc64::lowtag-nodeheader)))
                       (match-instr the-trap
                                    (ppc-instruction-mask :opcode :rt :lowtag64)
                                    (ppc-lap-word (tdnei ?? ppc64::lowtag-immheader))))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :d)
                                               (ppc-lap-word (lbz ?? ppc64::misc-subtag-offset ??))
                                               fn pc-index))
                   (lisp-reg-p (setq ra (RA-field instr))))
              (let* ((typecode (D-field the-trap))
                     (type-tag (logand typecode ppc64::lowtagmask))
                     (type-name (svref (if (eql type-tag ppc64::lowtag-nodeheader)
                                         *nodeheader-types*
                                         *immheader-types*)
                                       (ash typecode (- ppc64::nlowtagbits)))))
                (%error (make-condition 'type-error
                                        :format-control (%rsc-string $XWRONGTYPE)
                                        :datum (xp-GPR-lisp xp ra)
                                        :expected-type type-name)
                        nil
                        frame-ptr)))
             ;; tdnei RA,N; RA != nargs, N = subtag_character type
             ;; check; look for "clrldi rs-node,ra-imm,56" = "rldicl
             ;; rs,ra,0,55"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :rt :d)
                                (ppc-lap-word (tdnei ?? ppc64::subtag-character)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :sh :mb6 :sh6)
                                               (ppc-lap-word (rldicl ?? ?? 0 56))
                                               fn pc-index))
                   (lisp-reg-p (setq rs (RS-field instr))))
              (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type 'character)
                        nil
                        frame-ptr))

             ;; tdnei RA,N; RA != nargs, N = ppc64::tag-fixnum.  type
             ;; check; look for "clrldi rs-node,ra-imm,61" = "rldicl
             ;; rs,ra,61"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :rt)
                                (ppc-lap-word (tdnei ?? ppc64::tag-fixnum)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :sh :mb6 :sh6)
                                               (ppc-lap-word (rldicl ?? ?? 0 61))                                               
                                               fn pc-index))

                   (lisp-reg-p (setq rs (RS-field instr))))
                (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type 'fixnum)
                        nil
                        frame-ptr))
             ;; tdi 3,RA,ppc64::fulltag-cons; RA != nargs type check;
             ;; look for "clrldi rs-node,ra-imm,60" = "rldicl
             ;; rs,ra,60"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :d)
                                (ppc-lap-word (tdi 3 ?? ppc64::fulltag-cons)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :sh :mb6 :sh6)
                                               (ppc-lap-word (rldicl ?? ?? 0 60))                                               
                                               fn pc-index))

                   (lisp-reg-p (setq rs (RS-field instr))))
                (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type 'list)
                        nil
                        frame-ptr))             
             ;; tdnei RA,ppc64::fulltag-cons; RA != nargs type check;
             ;; look for "clrldi rs-node,ra-imm,60" = "rldicl
             ;; rs,ra,60"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :d)
                                (ppc-lap-word (tdnei ?? ppc64::fulltag-cons)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :sh :mb6 :sh6)
                                               (ppc-lap-word (rldicl ?? ?? 0 60))                                               
                                               fn pc-index))

                   (lisp-reg-p (setq rs (RS-field instr))))
                (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type 'cons)
                        nil
                        frame-ptr))
             ;; tdnei RA,ppc64::subtag-single-float; RA != nargs type check;
             ;; look for "clrldi rs-node,ra-imm,60" = "rldicl
             ;; rs,ra,60"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :d)
                                (ppc-lap-word (tdnei ?? ppc64::subtag-single-float)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :sh :mb6 :sh6)
                                               (ppc-lap-word (rldicl ?? ?? 0 60))                                               
                                               fn pc-index))

                   (lisp-reg-p (setq rs (RS-field instr))))
                (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type 'short-float)
                        nil
                        frame-ptr))
             ;; tdnei RA,ppc64::fulltag-misc; RA != nargs type check;
             ;; look for "clrldi rs-node,ra-imm,60" = "rldicl
             ;; rs,ra,60"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :d)
                                (ppc-lap-word (tdnei ?? ppc64::fulltag-misc)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :sh :mb6 :sh6)
                                               (ppc-lap-word (rldicl ?? ?? 0 60))                                               
                                               fn pc-index))

                   (lisp-reg-p (setq rs (RS-field instr))))
                (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type 'uvector)
                        nil
                        frame-ptr))
             ;; tdlgti RA,N; RA = nargs (xy = 01)
             ;; tdllti RA,N; RA = nargs (xy = 10)
             ;; nargs check, optional or rest involved
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode (:to #x1c) :ra)
                                (ppc-lap-word (tdi ?? ppc::nargs ??)))
                   (or (eql #b01 (setq temp (ldb #.(ppc-instruction-field :to) the-trap)))
	               (eql #b10 temp)))
              (%error (if (eql temp #b10)
                        'too-few-arguments
                        'too-many-arguments)
                      (list :nargs (ash (xp-GPR-signed-doubleword xp ppc::nargs)
					(- ppc64::fixnumshift))
			    :fn  fn)
                      frame-ptr))
             
             ;; tdeqi RA,N; N = unbound
             ;; symeval boundp check; look for "ld RA,symbol.vcell(nodereg)"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :d) 
                                (ppc-lap-word (tdeqi ?? ppc64::unbound-marker)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :ds :ds-xo)
                                               (ppc-lap-word (ld ?? ppc64::symbol.vcell ??))                                               
                                               fn pc-index))
                   (lisp-reg-p (setq ra (RA-field instr))))
              (setf (xp-GPR-lisp xp (RA-field the-trap))
                    (%kernel-restart-internal $xvunbnd (list (xp-GPR-lisp xp ra)) frame-ptr)))
	     ;; tdeqi RA,N: n = (%slot-unbound-marker)
	     ;; slot-unbound trap.  Look for preceding "ldx RA,rx,ry".
	     ;; rx = slots-vector, ry = scaled index in slots vector.
	     ((and (match-instr the-trap
				(ppc-instruction-mask :opcode :to :d)
				(ppc-lap-word (tdeqi ?? ppc64::slot-unbound-marker)))
		   (setq instr (scan-for-instr (ppc-instruction-mask
						:opcode :rt  :x-minor)
					       (dpb
						(RA-field the-trap)
						(byte 5 21)
						(ppc-lap-word
						 (ldx ?? ?? ??)))
					       fn pc-index)))
              (setq *error-reentry-count* 0)  ; succesfully reported error
              ;; %SLOT-UNBOUND-TRAP will decode the arguments further,
              ;; then call the generic function SLOT-UNBOUND.  That
              ;; might return a value; if so, set the value of the
              ;; register that caused the trap to that value.
              (setf (xp-gpr-lisp xp (ra-field the-trap))
                    (%slot-unbound-trap (xp-gpr-lisp xp (RA-field instr))
                                        (ash (- (xp-gpr-signed-doubleword xp (RB-field instr))
                                                ppc64::misc-data-offset)
                                             (- ppc64::word-shift))
                                        frame-ptr)))
             ;; tdlge RA,RB
             ;; vector bounds check; look for "ld immreg, misc_header_offset(nodereg)"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :x-minor)
                                (ppc-lap-word (tdlge ?? ??)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode #|:d|# :ds-xo)
                                               (ppc-lap-word (ld ?? ?? #|ppc32::misc-header-offset|# ??))
                                               fn pc-index))
                   (lisp-reg-p (setq ra (RA-field instr))))
              (%error (%rsc-string $xarroob)
                      (list (xp-GPR-lisp xp (RA-field the-trap))
                            (xp-GPR-lisp xp ra))
                      frame-ptr))
             ;; tdi 27 ra d - array header rank check
	     ((and (match-instr the-trap
				(ppc-instruction-mask :opcode :to)
				(ppc-lap-word (tdi 27 ?? ??)))
		   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :ds :ds-xo)
                                               (ppc-lap-word (ld ?? ppc64::arrayH.rank ??))
                                               fn pc-index))
		   (lisp-reg-p (setq ra (RA-field instr))))
	      (%error (%rsc-string $xndims)
		      (list (xp-gpr-lisp xp ra)
			    (ash (ldb (byte 16 0) the-trap) (- ppc64::fixnumshift)))
		      frame-ptr))
	     ;; td 27 ra rb - array flags check
	     ((and (match-instr the-trap
				(ppc-instruction-mask :opcode :to :x-minor)
				(ppc-lap-word (td 27 ?? ??)))
		   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :ds :ds-xo)
                                               (ppc-lap-word (ld ?? ppc64::arrayH.flags ??))
                                               fn pc-index))
		   (lisp-reg-p (setq ra (RA-field instr)))
		   (let* ((expected (xp-gpr-lisp xp (RB-field the-trap)))
			  (expected-subtype (ldb
					     ppc64::arrayH.flags-cell-subtag-byte
					     expected))
			  (expect-simple (=
					  (ldb ppc64::arrayH.flags-cell-bits-byte
					       expected)
					  (ash 1 $arh_simple_bit)))
			  (type-name
			   (case expected-subtype
			     (#.ppc64::subtag-double-float-vector 'double-float))))

		     (and type-name expect-simple
			  (setq condition
				(make-condition 'type-error
						:datum (xp-gpr-lisp xp ra)
						:expected-type
						`(simple-array ,type-name))))))
	      (%error condition nil frame-ptr))
			       
             ;; Unknown trap
             (t (%error "Unknown trap: #x~x~%xp: ~s, fn: ~s, pc: #x~x"
                        (list the-trap xp fn (ash pc-index ppc64::fixnumshift))
                        frame-ptr)))))))))





