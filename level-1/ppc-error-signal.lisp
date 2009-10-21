;;; PPC-specific code to handle trap and uuo callbacks.
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



;;; callback here from C exception handler

(defcallback 
    %xerr-disp 
    (:address xp :unsigned-fullword fn-reg :unsigned-fullword pc-or-index :signed-fullword errnum :unsigned-fullword rb :signed-fullword continuable)
  (block %err-disp
    (let ((fn (unless (eql fn-reg 0) (xp-gpr-lisp xp fn-reg)))
	  (err-fn (if (eql continuable 0) '%err-disp-internal '%kernel-restart-internal)))
      (if (eql errnum arch::error-stack-overflow)
	(handle-stack-overflow xp fn rb)
	(with-xp-stack-frames (xp fn frame-ptr)	; execute body with dummy stack frame(s)
	  (with-error-reentry-detection
	      (let* ((rb-value (xp-gpr-lisp xp rb))
		     (res
		      (cond ((< errnum 0)
			     (%err-disp-internal errnum nil frame-ptr))
			    ((logtest errnum arch::error-type-error)
			     (funcall err-fn 
				      #.(car (rassoc 'type-error *kernel-simple-error-classes*))
				      (list rb-value (logandc2 errnum arch::error-type-error))
				      frame-ptr))
			    ((eql errnum arch::error-udf)
			     (funcall err-fn $xfunbnd (list rb-value) frame-ptr))
			    ((eql errnum arch::error-throw-tag-missing)
			     (%error (make-condition 'cant-throw-error
						     :tag rb-value)
				     nil frame-ptr))
			    ((eql errnum arch::error-cant-call)
			     (%error (make-condition 'type-error
						     :datum  rb-value
						     :expected-type '(or symbol function)
						     :format-control
						     "~S is not of type ~S, and can't be FUNCALLed or APPLYed")
				     nil frame-ptr))
			    ((eql errnum arch::error-udf-call)
			     (return-from %err-disp
			       (handle-udf-call xp frame-ptr)))
			    ((eql errnum arch::error-alloc-failed)
			     (%error (make-condition 
				      'simple-storage-condition
				      :format-control (%rsc-string $xmemfull))
				     nil frame-ptr))
			    ((eql errnum arch::error-memory-full)
			     (%error (make-condition 
				      'simple-storage-condition
				      :format-control (%rsc-string $xnomem))
				     nil frame-ptr))
			    ((or (eql errnum arch::error-fpu-exception-double) 
				 (eql errnum arch::error-fpu-exception-single))
			     (let* ((code-vector (and fn  (uvref fn 0)))
				    (instr (if code-vector 
					     (uvref code-vector pc-or-index)
					     (%get-long (%int-to-ptr pc-or-index)))))
			       (let* ((minor (ldb (byte 5 1) instr))
				      (fra (ldb (byte 5 16) instr))
				      (frb (ldb (byte 5 11) instr))
				      (frc (ldb (byte 5 6) instr)))
				 (declare (fixnum minor fra frb frc))
				 (if (= minor 12) ; FRSP
				   (%err-disp-internal $xcoerce (list (xp-double-float xp frc) 'short-float) frame-ptr)
				   (flet ((coerce-to-op-type (double-arg)
					    (if (eql errnum arch::error-fpu-exception-double)
					      double-arg
					      (handler-case (coerce double-arg 'short-float)
						(error (c) (declare (ignore c)) double-arg)))))
				     (multiple-value-bind (status control) (xp-fpscr-info xp)
				       (%error (make-condition (fp-condition-from-fpscr status control)
							       :operation (fp-minor-opcode-operation minor)
							       :operands
                                                               (if (= minor 22)
                                                                 (list (coerce-to-op-type (xp-double-float xp frb)))
                                                                 (list (coerce-to-op-type 
                                                                        (xp-double-float xp fra))
                                                                       (if (= minor 25)
                                                                         (coerce-to-op-type 
                                                                          (xp-double-float xp frc))
                                                                         (coerce-to-op-type 
                                                                          (xp-double-float xp frb))))))
					       nil
					       frame-ptr)))))))
			    ((eql errnum arch::error-excised-function-call)
			     (%error "~s: code has been excised." (list (xp-gpr-lisp xp ppc::nfn)) frame-ptr))
			    ((eql errnum arch::error-too-many-values)
			     (%err-disp-internal $xtoomanyvalues (list rb-value) frame-ptr))
			    (t (%error "Unknown error #~d with arg: ~d" (list errnum rb-value) frame-ptr)))))
		(setf (xp-gpr-lisp xp rb) res) ; munge register for continuation
		)))))))



(defun handle-udf-call (xp frame-ptr)
  (let* ((args (xp-argument-list xp))
         (values (multiple-value-list
                  (%kernel-restart-internal
                   $xudfcall
                   (list (maybe-setf-name (xp-gpr-lisp xp ppc::fname)) args)
                   frame-ptr)))
         (stack-argcnt (max 0 (- (length args) 3)))
         (vsp (%i+ (xp-gpr-lisp xp ppc::vsp) stack-argcnt))
         (f #'(lambda (values) (apply #'values values))))
    (setf (xp-gpr-lisp xp ppc::vsp) vsp
          (xp-gpr-lisp xp ppc::nargs) 1
          (xp-gpr-lisp xp ppc::arg_z) values
          (xp-gpr-lisp xp ppc::nfn) f)
    ;; handle_uuo() (in the lisp kernel) will not bump the PC here.
    (setf (xp-gpr-lisp xp #+linuxppc-target #$PT_NIP #+darwinppc-target -2)
	  (uvref f 0))))






;;; rb is the register number of the stack that overflowed.
;;; xp & fn are passed so that we can establish error context.
(defun handle-stack-overflow (xp fn rb)
  (unwind-protect
       (with-xp-stack-frames (xp fn frame-ptr) ; execute body with dummy stack frame(s)
	 (%error
	  (make-condition
	   'stack-overflow-condition 
	   :format-control "Stack overflow on ~a stack."
	   :format-arguments (list
			      (if (eql rb ppc::sp)
				"control"
				(if (eql rb ppc::vsp)
				  "value"
				  (if (eql rb ppc::tsp)
				    "temp"
				    "unknown")))))
	  nil frame-ptr))
    (ff-call (%kernel-import target::kernel-import-restore-soft-stack-limit)
	     :unsigned-fullword rb
	     :void)))


