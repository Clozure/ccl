;;; Copyright 2009 Clozure Associates
;;; This file is part of Clozure CL.  
;;;
;;; Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;; Public License , known as the LLGPL and distributed with Clozure
;;; CL as the file "LICENSE".  The LLGPL consists of a preamble and
;;; the LGPL, which is distributed with Clozure CL as the file "LGPL".
;;; Where these conflict, the preamble takes precedence.
;;;
;;; Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(in-package "CCL")

;;; Return the effective address of a memory operand by using the
;;; register state in xp, or NIL if we can't figure it out.
;;; Needs to run inside a without-gcing form.
(defun x86-memory-operand-ea (xp op)
  (let* ((seg (x86::x86-memory-operand-seg op))
	 (disp (x86::x86-memory-operand-disp op))
	 (base (x86::x86-memory-operand-base op))
	 (index (x86::x86-memory-operand-index op))
	 (scale (x86::x86-memory-operand-scale op)))
    (cond
      ((and base index (not seg))
       (let* ((base-re (x86::x86-register-operand-entry base))
	      (index-re (x86::x86-register-operand-entry index))
	      (base-num (x86::reg-entry-reg-num base-re))
	      (index-num (x86::reg-entry-reg-num index-re))
	      (base-val nil)
	      (index-val nil))
	 (when (logtest (x86::reg-entry-reg-flags base-re) x86::+regrex+)
	   (incf base-num 8))
	 (setq base-val (encoded-gpr-integer xp base-num))
	 (when (logtest (x86::reg-entry-reg-flags index-re) x86::+regrex+)
	   (incf index-num 8))
	 (setq index-val (encoded-gpr-integer xp index-num))
	 (when scale
	   (setq index-val (ash index-val scale)))
	 (+ (or disp 0) base-val index-val))))))

;;; Try to emulate the disassembled instruction using the
;;; register state in xp.  Return NIL if we couldn't do it.
;;; This will run with other threads suspended.
(defun x86-emulate-instruction (xp instruction)
  (let* ((mnemonic (x86-di-mnemonic instruction))
	 (op0 (x86-di-op0 instruction))
	 (op1 (x86-di-op1 instruction))
	 (op2 (x86-di-op2 instruction)))
    (when (and op0 op1 (not op2)
	       (typep op0 'x86::x86-register-operand)
	       (typep op1 'x86::x86-memory-operand))
      (without-gcing
	(let* ((src-re (x86::x86-register-operand-entry op0))
	       (src-num (x86::reg-entry-reg-num src-re))
	       (src-val nil)
	       (ea (x86-memory-operand-ea xp op1)))
	  (when (logtest (x86::reg-entry-reg-flags src-re) x86::+regrex+)
	    (incf src-num 8))
	  (setq src-val (encoded-gpr-integer xp src-num))
	  (when ea
	    (with-macptrs ((p (%int-to-ptr ea)))
	      (cond
		((string= mnemonic "movb")
		 (setf (%get-signed-byte p) (ldb (byte 8 0) src-val)))
		((string= mnemonic "movw")
		 (setf (%get-signed-word p) (ldb (byte 16 0) src-val)))
		((string= mnemonic "movl")
		 (setf (%get-signed-long p) (ldb (byte 32 0) src-val)))
		((string= mnemonic "movq")
		 (setf (%%get-signed-longlong p 0) (ldb (byte 64 0) src-val)))))))))))

(defun x86-can-emulate-instruction (instruction)
  (let* ((mnemonic (x86-di-mnemonic instruction))
	 (op0 (x86-di-op0 instruction))
	 (op1 (x86-di-op1 instruction))
	 (op2 (x86-di-op2 instruction)))
    (when (and op0 op1 (not op2)
	       (typep op0 'x86::x86-register-operand)
	       (typep op1 'x86::x86-memory-operand)
	       (member mnemonic '("movb" "movw" "movl" "movq") :test 'string=))
      (let* ((seg (x86::x86-memory-operand-seg op1))
	     (base (x86::x86-memory-operand-base op1))
	     (index (x86::x86-memory-operand-index op1)))
	(and base index (not seg))))))
