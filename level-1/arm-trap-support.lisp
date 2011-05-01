;;;-*-Mode: LISP; Package: CCL -*-
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

(in-package "CCL")

#+androidarm-target
(eval-when (:compile-toplevel :execute)
  (def-foreign-type :__sigset_t
      (:struct nil
        (:__val (:array :unsigned-long 32)))) 
  (def-foreign-type nil
      (:struct :sigcontext
        (:trap_no :unsigned-long)
        (:error_code :unsigned-long)
        (:oldmask :unsigned-long)
        (:arm_r0 :unsigned-long)
        (:arm_r1 :unsigned-long)
        (:arm_r2 :unsigned-long)
        (:arm_r3 :unsigned-long)
        (:arm_r4 :unsigned-long)
        (:arm_r5 :unsigned-long)
        (:arm_r6 :unsigned-long)
        (:arm_r7 :unsigned-long)
        (:arm_r8 :unsigned-long)
        (:arm_r9 :unsigned-long)
        (:arm_r10 :unsigned-long)
        (:arm_fp :unsigned-long)
        (:arm_ip :unsigned-long)
        (:arm_sp :unsigned-long)
        (:arm_lr :unsigned-long)
        (:arm_pc :unsigned-long)
        (:arm_cpsr :unsigned-long)
        (:fault_address :unsigned-long)))
  (def-foreign-type :mcontext_t (:struct :sigcontext))
  (def-foreign-type nil
      (:struct :sigaltstack
        (:ss_sp :address)
        (:ss_flags :int)
        (:ss_size :size_t)))
  (def-foreign-type :stack_t (:struct :sigaltstack))
  (def-foreign-type nil
      (:struct :ucontext
        (:uc_flags :unsigned-long)
        (:uc_link (:* (:struct :ucontext)))
        (:uc_stack :stack_t)
        (:uc_mcontext :mcontext_t)
        (:uc_sigmask :__sigset_t)
        (:uc_regspace (:array :unsigned-long 128))))
  (def-foreign-type :ucontext_t (:struct :ucontext)))
  
    
  
  
        
        
#+linuxarm-target
(progn
(defmacro with-xp-registers-and-gpr-offset ((xp register-number)
                                            (registers offset) &body body)
  (let* ((regform `(pref ,xp :ucontext.uc_mcontext)))
    `(with-macptrs ((,registers ,regform))
      (let ((,offset (xp-gpr-offset ,register-number)))
        ,@body))))
(defun xp-gpr-offset (register-number)
  (unless (and (fixnump register-number)
               (<= -3 (the fixnum register-number))
               (< (the fixnum register-number) 18))
    (setq register-number (require-type register-number '(integer -3 (18)))))
  (the fixnum (* (the fixnum (+ register-number 3)) arm::node-size)))
(defconstant xp-cpsr-regno 16)
)

#+darwinarm-target
(progn
(defmacro with-xp-registers-and-gpr-offset ((xp register-number)
                                            (registers offset) &body body)
  (let* ((regform `(pref ,xp :ucontext_t.uc_mcontext.__ss)))
    `(with-macptrs ((,registers ,regform))
      (let ((,offset (xp-gpr-offset ,register-number)))
        ,@body))))
(defun xp-gpr-offset (register-number)
  (unless (and (fixnump register-number)
               (<= 0 (the fixnum register-number))
               (< (the fixnum register-number) 17))
    (setq register-number (require-type register-number '(integer 0 (17)))))
  (the fixnum (* (the fixnum register-number) arm::node-size)))
(defconstant xp-cpsr-regno 16)
)

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

(defun return-address-offset (xp fn machine-state-offset)
  (with-macptrs ((regs (pref xp #+linuxarm-target :ucontext.uc_mcontext
                                #+darwinarm-target :ucontext_t.uc_mcontext.__ss)))
    (if (functionp fn)
      (or (%code-vector-pc (uvref fn 1) (%inc-ptr regs machine-state-offset))
           (%get-ptr regs machine-state-offset))
      (%get-ptr regs machine-state-offset))))

(defconstant lr-offset-in-register-context
  #+linuxarm-target (get-field-offset :sigcontext.arm_lr)
  #+darwinarm-target (get-field-offset :__darwin_arm_thread_state.__lr))

(defconstant pc-offset-in-register-context
  #+linuxarm-target (get-field-offset :sigcontext.arm_pc)
  #+darwinarm-target (get-field-offset :__darwin_arm_thread_state.__pc))

(defun funcall-with-xp-stack-frames (xp trap-function thunk)
  (cond ((null trap-function)
         ; Maybe inside a subprim from a lisp function
         (let* ((fn (xp-gpr-lisp xp arm::fn))
                (lr (return-address-offset
                     xp fn lr-offset-in-register-context)))
           (if (fixnump lr)
             (let* ((sp (xp-gpr-lisp xp arm::sp))
                    (vsp (xp-gpr-lisp xp arm::vsp))
                    (frame (make-fake-stack-frame sp sp fn lr vsp xp)))
               (declare (dynamic-extent frame))
               (funcall thunk (%dnode-address-of frame)))
             (funcall thunk (xp-gpr-lisp xp arm::sp)))))
        ((eq trap-function (xp-gpr-lisp xp arm::fn))
         (let* ((sp (xp-gpr-lisp xp arm::sp))
                (fn trap-function)
                (lr (return-address-offset
                     xp fn pc-offset-in-register-context))
                (vsp (xp-gpr-lisp xp arm::vsp))
                (frame (make-fake-stack-frame sp sp fn lr vsp xp)))
           (declare (dynamic-extent frame))
           (funcall thunk (%dnode-address-of frame))))
        ((eq trap-function (xp-gpr-lisp xp arm::nfn))
         (let* ((sp (xp-gpr-lisp xp arm::sp))
                (fn (xp-gpr-lisp xp arm::fn))
                (lr (return-address-offset
                     xp fn lr-offset-in-register-context))
                (vsp (xp-gpr-lisp xp arm::vsp))
                (lr-frame (make-fake-stack-frame sp sp fn lr vsp xp))
                (pc-fn trap-function)
                (pc-lr (return-address-offset
                        xp pc-fn pc-offset-in-register-context))
                (pc-frame (make-fake-stack-frame sp (%dnode-address-of lr-frame) pc-fn pc-lr vsp xp)))
           (declare (dynamic-extent lr-frame pc-frame))
           (funcall thunk (%dnode-address-of pc-frame))))
        (t (funcall thunk (xp-gpr-lisp xp arm::sp)))))

(defcallback xcmain (:address xp
                              :signed-fullword signal
                              :signed-fullword arg
                              :signed-fullword fnreg
                              :signed-fullword offset)
  (with-xp-stack-frames (xp (unless (eql 0 fnreg) (xp-gpr-lisp xp fnreg)) frame-ptr)
    (cond ((eql signal 0) (cmain))
          ((or (eql signal #$SIGBUS)
               (eql signal #$SIGSEGV))
           (%error (make-condition 'invalid-memory-access
                                   :address arg
                                   :write-p (eql signal #$SIGBUS))
                   ()
                   frame-ptr))
          (t
           (error "cmain callback: signal = ~d, arg = #x~x, fnreg = ~d, offset = ~d"
                  signal arg fnreg offset)))))