;;; x86-trap-support
;;;
;;;   Copyright (C) 2005-2009 Clozure Associates and contributors
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

;;; The order in which GPRs appear in an exception context generally
;;; has nothing to do with how they're encoded in instructions/uuos,
;;; and is OS-dependent.

#+linuxx8664-target
(progn
  (defconstant gp-regs-offset (+ (get-field-offset :ucontext.uc_mcontext)
                                 (get-field-offset :mcontext_t.gregs)))
  (defmacro xp-gp-regs (xp) xp)
  (defconstant flags-register-offset #$REG_EFL)
  (defconstant rip-register-offset #$REG_RIP)
  (defun xp-mxcsr (xp)
    (pref xp :ucontext.uc_mcontext.fpregs.mxcsr))
  (defmacro xp-xmm-regs (xp)
    `(pref ,xp :ucontext.uc_mcontext.fpregs._xmm))
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(13                                ;rax
      14                                ;rcx
      12                                ;rdx
      11                                ;rbx
      15                                ;rsp
      10                                ;rbp
      9                                 ;rsi
      8                                 ;rdi
      0                                 ;r8
      1                                 ;r9
      2                                 ;r10
      3                                 ;r11
      4                                 ;r12
      5                                 ;r13
      6                                 ;r14
      7                                 ;r15
      )))

#+freebsdx8664-target
(progn
  (defconstant gp-regs-offset (get-field-offset :ucontext_t.uc_mcontext))
  (defmacro xp-gp-regs (xp) xp)
  (defconstant flags-register-offset 22)
  (defconstant rip-register-offset 20)
  (defun xp-mxcsr (xp)
    (with-macptrs ((state (pref xp :__ucontext.uc_mcontext.mc_fpstate)))
      (pref state :savefpu.sv_env.en_mxcsr)))
  (defmacro xp-xmm-regs (xp)
    (let* ((state (gensym)))
      `(with-macptrs ((,state (pref ,xp :__ucontext.uc_mcontext.mc_fpstate)))
        (pref ,state :savefpu.sv_xmm))))
      
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(7					;rax
      4					;rcx
      3					;rdx
      8					;rbx
      23                                ;rsp
      9					;rbp
      2                                 ;rsi
      1                                 ;rdi
      5                                 ;r8
      6                                 ;r9
      10				;r10
      11                                ;r11
      12				;r12
      13				;r13
      14				;r14
      15                                ;r15
      )))

#+darwinx8664-target
(progn
  (defconstant gp-regs-offset 0)
  (defun xp-mxcsr (xp)
     (pref xp :ucontext_t.uc_mcontext.__fs.__fpu_mxcsr))
  (defmacro xp-gp-regs (xp)
    `(pref ,xp :ucontext_t.uc_mcontext.__ss))
  (defmacro xp-xmm-regs (xp)
    `(pref ,xp :ucontext_t.uc_mcontext.__fs.__fpu_xmm0))

  (defconstant flags-register-offset 17)
  (defconstant rip-register-offset 16)  
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(0					;rax
      2					;rcx
      3					;rdx
      1					;rbx
      7                                 ;rsp
      6					;rbp
      5                                 ;rsi
      4                                 ;rdi
      8                                 ;r8
      9                                 ;r9
      10				;r10
      11                                ;r11
      12				;r12
      13				;r13
      14				;r14
      15                                ;r15
      )))

#+solarisx8664-target
(progn
  (defconstant gp-regs-offset (+ (get-field-offset :ucontext.uc_mcontext)
                                 (get-field-offset :mcontext_t.gregs)))
  (defmacro xp-gp-regs (xp) xp)
  (defconstant flags-register-offset #$REG_RFL)
  (defconstant rip-register-offset #$REG_RIP)
  (defun xp-mxcsr (xp)
    (pref xp :ucontext.uc_mcontext.fpregs.fp_reg_set.fpchip_state.mxcsr))
  (defmacro xp-mmx-regs (xp)
    `(pref ,xp :ucontext.uc_mcontext.fpregs.fp_reg_set.fpchip_state.xmm))
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(14                                ;rax
      13                                ;rcx
      12                                ;rdx
      11                                ;rbx
      20                                ;rsp
      10                                ;rbp
      9                                 ;rsi
      8                                 ;rdi
      7                                 ;r8
      6                                 ;r9
      5                                 ;r10
      4                                 ;r11
      3                                 ;r12
      2                                 ;r13
      1                                 ;r14
      0                                 ;r15
      )))

#+win64-target
(progn
  (defconstant gp-regs-offset (get-field-offset #>CONTEXT.Rax))
  (defmacro xp-gp-regs (xp) xp)
  (defconstant rip-register-offset 16)
  (defun xp-mxcsr (xp)
    (pref xp #>CONTEXT.MxCsr))
  (defmacro xp-xmm-regs (xp)
    `(pref ,xp #>CONTEXT.nil.FltSave.XmmRegisters))
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(0					;rax
      1					;rcx
      2					;rdx
      3					;rbx
      4                                 ;rsp
      5					;rbp
      6                                 ;rsi
      7                                 ;rdi
      8                                 ;r8
      9                                 ;r9
      10				;r10
      11                                ;r11
      12				;r12
      13				;r13
      14				;r14
      15                                ;r15
      )))

#+darwinx8632-target
(progn
  (defconstant gp-regs-offset 0)
  (defmacro xp-gp-regs (xp)
    `(pref ,xp :ucontext_t.uc_mcontext.__ss))
  (defun xp-mxcsr (xp)
    (pref xp :ucontext_t.uc_mcontext.__fs.__fpu_mxcsr))
  (defmacro xp-xmm-regs (xp)
    `(pref ,xp :ucontext_t.uc_mcontext.__fs.__fpu_xmm0))
  (defconstant flags-register-offset 9)
  (defconstant eip-register-offset 10)
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(0					;eax
      2					;ecx
      3					;edx
      1					;ebx
      7					;esp
      6					;ebp
      5					;esi
      4					;edi
      )))

#+linuxx8632-target
(progn
  (defconstant gp-regs-offset 0)
  (defmacro xp-gp-regs (xp)
    `(pref (pref ,xp :ucontext.uc_mcontext) :mcontext_t.gregs))
  (defun xp-mxcsr (xp)
    (pref (pref (pref xp :ucontext.uc_mcontext) :mcontext_t.fpregs)
          :_fpstate.mxcsr))
  (defmacro xp-xmm-regs (xp)
    `(pref ,xp :ucontext.uc_mcontext.fpregs._xmm))
  (defconstant flags-register-offset #$REG_EFL)
  (defconstant eip-register-offset #$REG_EIP)
  (defparameter *encoded-gpr-to-indexed-gpr*
    (vector
     #$REG_EAX                         ;eax
      #$REG_ECX                         ;ecx
      #$REG_EDX                         ;edx
      #$REG_EBX                         ;ebx
      #$REG_ESP                         ;esp
      #$REG_EBP                         ;ebp
      #$REG_ESI                         ;esi
      #$REG_EDI                         ;edi
      )))

#+win32-target
(progn
  (defconstant gp-regs-offset 0)
  (defmacro xp-gp-regs (xp)
    `,xp)
  (defun xp-mxcsr (xp)
    (%get-unsigned-long (pref xp #>CONTEXT.ExtendedRegisters) 24))
  (defmacro xp-xmm-regs (xp)
    `(%inc-ptr ,xp #x16c))
  (defconstant flags-register-offset 48)
  (defconstant eip-register-offset 45)
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(
     44                                ;eax
     43                                ;ecx
     42                                ;edx
     41                                ;ebx
     49                                ;esp
     45                                ;ebp
     40                                ;esi
     39                                ;edi
      )))

#+solarisx8632-target
(progn
  (defconstant gp-regs-offset 0)
  (defmacro xp-gp-regs (xp)
    `(pref (pref ,xp :ucontext.uc_mcontext) :mcontext_t.gregs))
  (defun xp-mxcsr (xp)
    (pref xp :ucontext.uc_mcontext.fpregs.fp_reg_set.fpchip_state.mxcsr))
  (defconstant flags-register-offset #$EFL)
  (defmacro xp-xmm-regs (xp)
    `(pref ,xp :ucontext.uc_mcontext.fpregs.fp_reg_set.fpchip_state.xmm))
  (defconstant eip-register-offset #$EIP)
  (defparameter *encoded-gpr-to-indexed-gpr*
    (vector
     #$EAX
     #$ECX
     #$EDX
     #$EBX
     #$ESP
     #$EBP
     #$ESI
     #$EDI)
      ))

#+freebsdx8632-target
(progn
  (defconstant gp-regs-offset 0)
  (defmacro xp-gp-regs (xp)
    `(pref ,xp :ucontext_t.uc_mcontext))
  (defun xp-mxcsr (xp)
    (pref (pref xp :ucontext_t.uc_mcontext.mc_fpstate) :savexmm.sv_env.en_mxcsr))
  (defmacro xp-xmm-regs (xp)
    `(pref (pref ,xp :ucontext_t.uc_mcontext.mc_fpstate) :savexmm.sv_xmm))
  (defconstant flags-register-offset 17)
  (defconstant eip-register-offset 15)
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(
      12                                ;eax
      11                                ;ecx
      10                                ;edx
      9                                 ;ebx
      18                                ;esp
      7                                 ;ebp
      6                                 ;esi
      5                                 ;edi
      )
      ))

(defun indexed-gpr-lisp (xp igpr)
  (%get-object (xp-gp-regs xp) (+ gp-regs-offset (ash igpr target::word-shift))))
(defun (setf indexed-gpr-lisp) (new xp igpr)
  (%set-object (xp-gp-regs xp) (+ gp-regs-offset (ash igpr target::word-shift)) new))
(defun encoded-gpr-lisp (xp gpr)
  (indexed-gpr-lisp xp (aref *encoded-gpr-to-indexed-gpr* gpr)))
(defun (setf encoded-gpr-lisp) (new xp gpr)
  (setf (indexed-gpr-lisp xp (aref *encoded-gpr-to-indexed-gpr* gpr)) new))
(defun indexed-gpr-integer (xp igpr)
  #+x8664-target
  (%get-signed-long-long (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8664::word-shift)))
  #+x8632-target
  (%get-signed-long (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8632::word-shift))))
(defun (setf indexed-gpr-integer) (new xp igpr)
  (setf
   #+x8664-target
   (%get-signed-long-long (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8664::word-shift)))
   #+x8632-target
   (%get-signed-long (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8632::word-shift)))
   new))
(defun encoded-gpr-integer (xp gpr)
  (indexed-gpr-integer xp (aref *encoded-gpr-to-indexed-gpr* gpr)))
(defun (setf encoded-gpr-integer) (new xp gpr)
  (setf (indexed-gpr-integer xp (aref *encoded-gpr-to-indexed-gpr* gpr)) new))
(defun indexed-gpr-macptr (xp igpr)
  (%get-ptr (xp-gp-regs xp) (+ gp-regs-offset (ash igpr target::word-shift))))
(defun (setf indexed-gpr-macptr) (new xp igpr)
  (setf (%get-ptr (xp-gp-regs xp) (+ gp-regs-offset (ash igpr target::word-shift))) new))
(defun encoded-gpr-macptr (xp gpr)
  (indexed-gpr-macptr xp (aref *encoded-gpr-to-indexed-gpr* gpr)))
(defun (setf encoded-gpr-macptr) (new xp gpr)
  (setf (indexed-gpr-macptr xp (aref *encoded-gpr-to-indexed-gpr* gpr)) new))
(defun xp-flags-register (xp)
  #+windows-target (pref xp #>CONTEXT.EFlags)
  #-windows-target
  (progn
  #+x8664-target
  (%get-signed-long-long (xp-gp-regs xp) (+ gp-regs-offset (ash flags-register-offset x8664::fixnumshift)))
  #+x8632-target
  (%get-signed-long (xp-gp-regs xp) (+ gp-regs-offset (ash flags-register-offset x8632::fixnumshift)))))
(defmacro xp-xmm-single-float (xp n)
  `(%get-single-float (xp-xmm-regs ,xp) (ash ,n 4)))
(defmacro xp-xmm-double-float (xp n)
  `(%get-double-float (xp-xmm-regs ,xp) (ash ,n 4)))

(defun %get-xcf-byte (xcf-ptr delta)
  (let* ((containing-object (%get-object xcf-ptr target::xcf.containing-object))
         (byte-offset (%get-object xcf-ptr target::xcf.relative-pc)))
    (if containing-object
      (locally (declare (optimize (speed 3) (safety 0))
                        (type (simple-array (unsigned-byte 8) (*)) containing-object))
        (aref containing-object (the fixnum (+ byte-offset delta))))
      ;; xcf.relative-pc is a fixnum, but it might be negative.
      (let* ((encoded-pc (%get-ptr xcf-ptr target::xcf.relative-pc))
	     (pc (ash (%ptr-to-int encoded-pc) (- target::fixnumshift))))
	(%get-unsigned-byte (%int-to-ptr pc) delta)))))

;;; If the byte following a uuo (which is "skip" bytes long, set
;;; the xcf's relative PC to the value contained in the 32-bit
;;; word preceding the current relative PC and return -1, else return skip.
(defun %check-anchored-uuo (xcf skip)
  (if (eql 0 (%get-xcf-byte xcf skip))
    (let* ((new-rpc (+ #+x8664-target target::tag-function
		       #+x8632-target target::fulltag-misc
                       (logior (ash (%get-xcf-byte xcf -1) 24)
                               (ash (%get-xcf-byte xcf -2) 16)
                               (ash (%get-xcf-byte xcf -3) 8)
                               (%get-xcf-byte xcf -4)))))
      (%set-object xcf target::xcf.relative-pc new-rpc)
      -1)
    skip))

(defun arithmetic-error-operation-from-instruction (instruction)
  (let* ((name (make-keyword (string-upcase (x86-di-mnemonic instruction)))))
    (case name
      ((:divss :divsd :idivl :idivd) '/)
      ((:mulss :mulsd) '*)
      ((:addss :addsd) '+)
      ((:subss :subsd) '-)
      (t 'coerce))))

(defun arithmetic-error-operands-from-instruction (instruction xp)
  (let* ((name (make-keyword (string-upcase (x86-di-mnemonic instruction)))))
    (let* ((op0 (x86-di-op0 instruction))
           (op1 (x86-di-op1 instruction))
           (xmmop0 (register-operand-regno op0 #.x86::+operand-type-RegXMM+))
           (xmmop1 (register-operand-regno op1 #.x86::+operand-type-RegXMM+)))
      (collect ((opvals))
        (case name
          ((:divss :mulss :addss :subss)
           (when (and xmmop0 xmmop1)
             (opvals (xp-xmm-single-float xp xmmop1))
             (opvals (xp-xmm-single-float xp xmmop0))))
          ((:divsd :mulsd :addsd :subsd)
           (when (and xmmop0 xmmop1)
             (opvals (xp-xmm-double-float xp xmmop1))
             (opvals (xp-xmm-double-float xp xmmop0))))
            
          )
        (opvals)))))


                                  
(defun decode-arithmetic-error (xp xcf)
  (declare (ignorable xp xcf))
  (let* ((code-vector (make-array 15 :element-type '(unsigned-byte 8)))
         (xfunction (%alloc-misc 1 target::subtag-xfunction)))
    (dotimes (i 15)                     ;maximum instructon size
      (setf (aref code-vector i) (%get-xcf-byte xcf i)))
    (setf (uvref xfunction 0) code-vector)
    (let* ((ds (make-x86-disassembly-state
                :mode-64 #+x8664-target t #+x8632-target nil
                :code-vector code-vector
                :constants-vector xfunction
                :entry-point 0
                :code-pointer 0           ; for next-u32/next-u16 below
                :symbolic-names nil
                :pending-labels (list 0)
                :code-limit 15
                :code-pointer 0))
           (instruction (ignore-errors (x86-disassemble-instruction ds nil))))
      (if instruction
        (values (arithmetic-error-operation-from-instruction  instruction)
                (arithmetic-error-operands-from-instruction instruction xp))
        (values 'unknown nil)))))

(eval-when (:compile-toplevel :execute)
  (progn
    (defun conditional-os-constant (alternatives)
      (dolist (c alternatives (error "None of the constants in ~s could be loaded" alternatives))
        (if (load-os-constant c t)
          (return (load-os-constant c)))))

    (defconstant integer-divide-by-zero-code
      (conditional-os-constant '(os::EXCEPTION_INT_DIVIDE_BY_ZERO os::FPE_INTDIV))
)
    (defconstant float-divide-by-zero-code
      (conditional-os-constant '(os::EXCEPTION_FLT_DIVIDE_BY_ZERO os::FPE_FLTDIV)))
    (defconstant float-overflow-code
      (conditional-os-constant '(os::FPE_FLTOVF os::EXCEPTION_FLT_OVERFLOW)))
    (defconstant float-underflow-code
      (conditional-os-constant '(os::FPE_FLTUND os::EXCEPTION_FLT_UNDERFLOW)))
    (defconstant float-inexact-code
      (conditional-os-constant '(os::FPE_FLTRES os::EXCEPTION_FLT_INEXACT_RESULT)))))

(defparameter *pending-gc-notification-hook* nil)

;;; UUOs are handled elsewhere.  This should handle all signals other than
;;; those generated by UUOs (and the non-UUO cases of things like SIGSEGV.)
;;; If the signal number is 0, other arguments (besides the exception context XP)
;;; may not be meaningful.
(defcallback xcmain (:address xp :address xcf :int signal :long code :long addr :long other :int)
  (let* ((frame-ptr (macptr->fixnum xcf))
	 (skip 0))
    (cond ((zerop signal)               ;thread interrupt
           (cmain))
          ((< signal 0)
           (%err-disp-internal code () frame-ptr))
          ((= signal #$SIGFPE)
           (setq code (logand #xffffffff code))
           (multiple-value-bind (operation operands)
               (decode-arithmetic-error xp xcf)
             (let* ((condition-name
                     (cond ((or (= code integer-divide-by-zero-code)
                                (= code float-divide-by-zero-code))
                            'division-by-zero)
                           ((= code float-overflow-code)
                            'floating-point-overflow)
                           ((= code float-underflow-code)
                            'floating-point-underflow)
                           ((= code float-inexact-code)
                            'floating-point-inexact)
                           (t
                            'floating-point-invalid-operation))))
               (%error (make-condition condition-name
                                       :operation operation
                                       :operands operands
                                       :status (xp-mxcsr xp))
                       ()
                       frame-ptr))))
          ((= signal #$SIGSEGV)
	   (cond
	     ((or (= code 0) (= code 1))
	      ;; Stack overflow.
	      (let* ((on-tsp (= code 1)))
		(unwind-protect
		     (%error
		      (make-condition
		       'stack-overflow-condition 
		       :format-control "Stack overflow on ~a stack."
		       :format-arguments (list (if on-tsp "temp" "value")))
		      nil frame-ptr)
		  (ff-call (%kernel-import target::kernel-import-restore-soft-stack-limit)
			   :unsigned-fullword code
			   :void))))
	     ((= code 2)
	      ;; Write to a watched object.
	      (let* ((offset other)
		     ;; The kernel exception handler leaves the
		     ;; watched object on the lisp stack under the
		     ;; xcf.
		     (object (%get-object xcf target::xcf.size)))
		(multiple-value-bind (insn insn-length)
		    (ignore-errors (x86-faulting-instruction xp))
		  (restart-case (%error (make-condition
					 'write-to-watched-object
					 :offset offset
					 :object object
					 :instruction insn)
					nil frame-ptr)
		    #-windows-target
		    (emulate ()
		      :test (lambda (c)
			      (declare (ignore c))
			      (x86-can-emulate-instruction insn))
		      :report
		      "Emulate this instruction, leaving the object watched."
		      (flet ((watchedp (object)
			       (%map-areas #'(lambda (x)
					       (when (eq object x)
						 (return-from watchedp t)))
					   area-watched)))
			(let ((result nil))
			  (with-other-threads-suspended
			    (when (watchedp object)
			      ;; We now trust that the object is in a
			      ;; static gc area.
			      (let* ((a (+ (%address-of object) offset))
				     (ptr (%int-to-ptr
					   (logandc2 a (1- *host-page-size*)))))
				(#_mprotect ptr *host-page-size* #$PROT_WRITE)
				(setq result (x86-emulate-instruction xp insn))
				(#_mprotect ptr *host-page-size*
					    (logior #$PROT_READ #$PROT_EXEC)))))
			  (if result
			    (setq skip insn-length)
			    (error "could not emulate the instrution")))))
		    (skip ()
		      :test (lambda (c)
			      (declare (ignore c))
			      insn)
		      :report "Skip over this write instruction."
		      (setq skip insn-length))
		    (unwatch ()
		      :report "Unwatch the object and retry the write."
		      (unwatch object))))))))
          ((= signal #+win32-target 10 #-win32-target #$SIGBUS)
           (if (= code -1)
             (%error (make-condition 'invalid-memory-operation)
                     ()
                     frame-ptr)
             (%error (make-condition 'invalid-memory-access
                                     :address addr
                                     :write-p (not (zerop code)))
                     ()
                     frame-ptr)))
          ((= signal #-win32-target #$SIGTRAP #+win32-target 5)
           (when (= code 0)
             (let* ((hook *pending-gc-notification-hook*))
               (declare (special *pending-gc-notification-hook*))
               (when hook (funcall hook))))))
    skip))

(defun x86-faulting-instruction (xp)
  (let* ((code-bytes (make-array 15 :element-type '(unsigned-byte 8)))
         (pc (indexed-gpr-macptr xp #+x8632-target eip-register-offset
                                    #+x8664-target rip-register-offset)))
    (dotimes (i (length code-bytes))
      (setf (aref code-bytes i) (%get-unsigned-byte pc i)))
    (let* ((ds (make-x86-disassembly-state
                :mode-64 #+x8664-target t #+x8632-target nil
                :code-vector code-bytes
                :code-pointer 0))
           (insn (x86-disassemble-instruction ds nil))
           (len (- (x86-ds-code-pointer ds) (x86-ds-insn-start ds))))
      (values insn len))))
