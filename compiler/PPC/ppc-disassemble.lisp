;;;-*- Mode: Lisp; Package: CCL -*-
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "NXENV")
  (require "DLL-NODE")
  (require "PPC-ASM")
  (require "PPC-LAP"))

(defparameter *ppc-disassembly-backend* *host-backend*)
(defparameter *ppc-disassemble-raw-instructions* nil)

(eval-when (:compile-toplevel :execute)
  (require "PPCENV"))

(defun ppc-gpr (r)
  (or
   (case (backend-target-arch-name *ppc-disassembly-backend*)
     (:ppc32 (and (eql r ppc32::rcontext) 'ppc32::rcontext))
     (:ppc64 (and (eql r ppc64::rcontext) 'ppc64::rcontext)))
   (svref ppc::*gpr-register-names* r)))

(defun ppc-fpr (r)
  (svref ppc::*fpr-register-names* r))

(defun ppc-vr (r)
    (svref ppc::*vector-register-names* r))

;;; To "unmacroexpand" something is to undo the effects of
;;; some sort of macroexpansion, returning some presumably
;;; more meaningful equivalent form.  Some cases of this
;;; are trivial (e.g., turning (stwu rX -4 vsp) into (vpush rX);
;;; some would depend on surrounding context and are still
;;; heuristic.  A few cases can probably benefit from state
;;; maintained by preceding instructions, e.g., (twnei rX 1)
;;; is presumably looking at the low 2 or three bits of rX; we
;;; have to know what set rX to know which.

;;; For now, just try to handle a few simple cases.
;;; Return a new form (new-opcode-name &rest new-operands) or NIL.
;;;

(defparameter *ppc-unmacroexpanders* (make-hash-table :test #'equalp))

(defun ppc-unmacroexpand-function (name)
  (let* ((pname (string name))
         (opnum (gethash pname ppc::*ppc-opcode-numbers*)))
    (unless opnum (error "Unknown ppc opcode name ~s." name))
    (values (gethash pname *ppc-unmacroexpanders*))))

(defun (setf ppc-unmacroexpand-function) (def name)
  (let* ((pname (string name))
         (opnum (gethash pname ppc::*ppc-opcode-numbers*)))
    (unless opnum (error "Unknown ppc opcode name ~s." name))
    (setf (gethash pname *ppc-unmacroexpanders*) def)))

(defmacro def-ppc-unmacroexpand (name insn-var lambda-list &body body)
  `(setf (ppc-unmacroexpand-function ',name)
         #'(lambda (,insn-var)
             (destructuring-bind ,lambda-list (lap-instruction-parsed-operands ,insn-var)
               ,@body))))

(def-ppc-unmacroexpand stwu insn (rs d ra)
  (case (backend-target-arch-name *ppc-disassembly-backend*)
    (:ppc32
     (if (and (= ra ppc::vsp) (= d -4))
       `(vpush ,(ppc-gpr rs))))))

(def-ppc-unmacroexpand stdu insn (rs d ra)
  (case (backend-target-arch-name *ppc-disassembly-backend*)
    (:ppc64
     (if (and (= ra ppc::vsp) (= d -8))
       `(vpush ,(ppc-gpr rs))))))

(def-ppc-unmacroexpand rlwinm insn (rt ra b mb &optional (me mb me-p))
  (if (not me-p)
    (setq mb 0))                        ; That's what's happening now to fake operands.
  (if (and (= me 31) (= (+ b mb) 32))
    `(srwi ,(ppc-gpr rt) ,(ppc-gpr ra) ,mb)
    (if (and (= mb 0) (= (+ b me) 31))
      (if (and (case (backend-target-arch-name *ppc-disassembly-backend*)
                 (:ppc32 t))
             (logbitp rt ppc-node-regs)
             (not (logbitp ra ppc-node-regs))
             (= b (arch::target-fixnum-shift (backend-target-arch
                                               *ppc-disassembly-backend*))))
        `(box-fixnum ,(ppc-gpr rt) ,(ppc-gpr ra))
        `(slwi ,(ppc-gpr rt) ,(ppc-gpr ra) ,b)))))

(def-ppc-unmacroexpand rldicr insn (rt ra sh me)
  (if (= (+ sh me) 63)
    (if (and (case (backend-target-arch-name *ppc-disassembly-backend*)
               (:ppc64 t))
             (logbitp rt ppc-node-regs)
             (not (logbitp ra ppc-node-regs))
             (= sh (arch::target-fixnum-shift (backend-target-arch
                                               *ppc-disassembly-backend*))))
      `(box-fixnum ,(ppc-gpr rt) ,(ppc-gpr ra))
      `(sldi ,(ppc-gpr rt) ,(ppc-gpr ra) ,sh))))

(def-ppc-unmacroexpand rldicl insn (rt ra sh mb)
  (if (= (+ sh mb) 64)
    `(srdi ,(ppc-gpr rt) ,(ppc-gpr ra) ,mb)))

(def-ppc-unmacroexpand srawi insn (rt ra sh)
  (if (and (case (backend-target-arch-name *ppc-disassembly-backend*)
             (:ppc32 t))
           (not (logbitp rt ppc-node-regs))
           (logbitp ra ppc-node-regs)
           (= sh (arch::target-fixnum-shift (backend-target-arch
                                             *ppc-disassembly-backend*))))
    `(unbox-fixnum ,(ppc-gpr rt) ,(ppc-gpr ra))))

(def-ppc-unmacroexpand sradi insn (rt ra sh)
  (if (and (case (backend-target-arch-name *ppc-disassembly-backend*)
             (:ppc64 t))
           (not (logbitp rt ppc-node-regs))
           (logbitp ra ppc-node-regs)
           (= sh (arch::target-fixnum-shift (backend-target-arch
                                             *ppc-disassembly-backend*))))
    `(unbox-fixnum ,(ppc-gpr rt) ,(ppc-gpr ra))))

(def-ppc-unmacroexpand li insn (rt imm)
  (let* ((fixnumshift (arch::target-fixnum-shift (backend-target-arch *ppc-disassembly-backend*))))
    (if (not (logtest (1- (ash 1 fixnumshift)) imm))
      (if (logbitp rt ppc-node-regs)
        `(li ,(ppc-gpr rt) ',(ash imm (- fixnumshift)))
        (if (eql rt ppc::nargs)
          `(set-nargs ,(ash imm (- fixnumshift))))))))



(def-ppc-unmacroexpand cmpwi insn (crf ra simm)
  (let* ((fixnumshift (arch::target-fixnum-shift (backend-target-arch *ppc-disassembly-backend*))))
    (if (and (not (logtest (1- (ash 1 fixnumshift)) simm))
             (logbitp ra ppc-node-regs))
      `(cmpwi ,@(unless (eql 0 crf) `(,(aref *ppc-cr-names* (ash crf -2))))
	,(ppc-gpr ra)
	',(ash simm (- fixnumshift))))))

(def-ppc-unmacroexpand cmpdi insn (crf ra simm)
  (let* ((fixnumshift (arch::target-fixnum-shift (backend-target-arch *ppc-disassembly-backend*))))
    (if (and (not (logtest (1- (ash 1 fixnumshift)) simm))
             (logbitp ra ppc-node-regs))
      `(cmpdi ,@(unless (eql 0 crf) `(,(aref *ppc-cr-names* (ash crf -2))))
	,(ppc-gpr ra)
	',(ash simm (- fixnumshift))))))

(def-ppc-unmacroexpand addi insn (rd ra simm)
  (let* ((fixnumshift (arch::target-fixnum-shift (backend-target-arch *ppc-disassembly-backend*)))
         (disp-d (ppc-gpr rd))
	 (disp-a (ppc-gpr ra)))
    (if (or (eql ra ppc::sp)
            (eql ra ppc::tsp)
	    (eql ra ppc::vsp))
	`(la ,disp-d ,simm ,disp-a)
	(let* ((opcode 'addi)
	       (val (abs simm)))
	  (if (< simm 0)
	      (setq opcode 'subi))
	  (if (and (not (logtest (1- (ash 1 fixnumshift)) simm))
		   (logbitp rd ppc-node-regs)
		   (logbitp ra ppc-node-regs))
	    `(,opcode ,disp-d ,disp-a ',(ash val (- fixnumshift)))
	    `(,opcode ,disp-d ,disp-a ,(if (eq val
                                               (arch::target-nil-value (backend-target-arch *ppc-disassembly-backend*))) nil val)))))))

(defun ppc-unmacroexpand (insn)
  (unless *ppc-disassemble-raw-instructions*
    (let* ((expander (ppc-unmacroexpand-function (opcode-name (lap-instruction-opcode insn))))
           (expansion (if expander (funcall expander insn))))
      (when expansion
        (setf (lap-instruction-opcode insn) (car expansion)
              (lap-instruction-parsed-operands insn) (cdr expansion))
        expansion))))


(defun find-ppc-opcode (i)
  (let* ((op (ldb (byte 6 26) i))
         (k (svref ppc::*ppc-opcode-indices* op)))
    (declare (type (unsigned-byte 12) k)
             (type (unsigned-byte 6) op))
    (unless (= k -1)
      (dotimes (j (svref ppc::*ppc-opcode-counts* op))
        (declare (type (unsigned-byte 10) j))
        (let* ((code (svref ppc::*ppc-opcodes* (+ k j))))
          (if (= (logand (opcode-mask code) i)
                 (opcode-opcode code))
            (if (dolist (op (opcode-operands code) t)
                  (let* ((xfun (operand-extract-function op)))
                    (unless (or (null xfun)
                                (funcall xfun i))
                      (return nil))))
              (return code))))))))

(defun ppc-disasm-1 (i pc header)
  (let* ((opcode (find-ppc-opcode i)))
    (if (null opcode)
      (error "Unknown PPC instruction : #x~8,'0x" i)    ; should handle somehow
      (let* ((vals ()))
        (dolist (operand (opcode-operands opcode))
          (unless (logbitp operand-fake (operand-flags operand))
            (let* ((extract-fn (operand-extract-function operand)))
              (push (if extract-fn
                      (funcall extract-fn i)
                      (ppc::extract-default operand i))
                    vals))))
        (let* ((insn (%make-lap-instruction opcode)))
          (setf (lap-instruction-parsed-operands insn)
                (nreverse vals))
          (setf (lap-instruction-address insn)
                pc)
          (append-dll-node insn header))))))
                

(defvar *disassembled-ppc-instructions* ())
(defvar *disassembled-ppc-labels* ())



(defun ppc-label-at-address (address)
  (dolist (l *disassembled-ppc-labels* 
             (let* ((label (%make-lap-label (intern (format nil "L~d" address)))))
               (setf (lap-label-address label) address)
               (push label *disassembled-ppc-labels*)
               label))
    (when (= address (lap-label-address l))
      (return l))))

(defun insert-ppc-label (l instructions)
  (let* ((labaddr (lap-label-address l)))
   (do-dll-nodes (insn instructions (append-dll-node l instructions))
     (when (>= (lap-instruction-address insn) labaddr)
       (return (insert-dll-node-after l (lap-instruction-pred insn)))))))

(defun ppc-disassemble-cr (val operand-spec)
  (declare (type (mod 32) val))
  (let* ((width (operand-width operand-spec))
         (crnum (ash val -2))
         (ccnum (logand val 3)))
    (declare (fixnum width crnum ccnum))
    (if (= width 3)
      (unless (= crnum 0) (aref *ppc-cr-names* crnum))
      (if (= ccnum 0)
        (unless (= crnum 0) (aref *ppc-cr-names* crnum))
        (list (aref *ppc-cr-field-names* crnum) (aref *ppc-cc-bit-names* ccnum))))))

(defun ppc-analyze-operands (instructions constants)
  (let* ((pc 0)
         (regsave-pseudo nil)
         (arch (backend-target-arch *ppc-disassembly-backend*))
         (nil-value (arch::target-nil-value arch))
         (misc-data-offset (arch::target-misc-data-offset arch))
         (word-shift (arch::target-word-shift arch))
         (align-mask (1- (ash 1 word-shift))))
    (declare (fixnum pc))
    (let* ((last (dll-header-last instructions)))
      (when (eq (lap-instruction-opcode last) *ppc-lwz-instruction*)
        (remove-dll-node last)
        (setq regsave-pseudo last)))
    (do-dll-nodes (insn instructions)
      (unless (ppc-unmacroexpand insn)
        (let* ((opcode (lap-instruction-opcode insn))
               (opvalues (lap-instruction-parsed-operands insn)))
          (do* ((operands (opcode-operands opcode) (cdr operands))
                (operand (car operands) (car operands))
                (header (cons nil opvalues))
                (tail header))
               ((null operands) (setf (lap-instruction-parsed-operands insn) (cdr header)))
            (declare (dynamic-extent header))
            (let* ((flags (operand-flags operand))
		   (opidx (operand-index operand))
                   (val (cadr tail)))
              (declare (fixnum flags))
              (if (and (logbitp operand-optional flags)
                       (eql 0 val))
                (rplacd tail (cddr tail))
                (progn
		  (if (and (or (eq opidx ppc::$si)
			       (eq opidx ppc::$nsi)
			       (eq opidx ppc::$ui))
			   (eql val nil-value))
		    (setf (cadr tail) nil)
		    (if (logbitp ppc::$ppc-operand-relative flags)
		      (let* ((label (ppc-label-at-address (+ pc val))))
			(setf (cadr tail) (lap-label-name label)))
		      (if (logbitp ppc::$ppc-operand-cr flags)
			(let* ((cr (ppc-disassemble-cr val operand)))
			  (when cr (setf (cadr tail) cr)))
			(if (logbitp ppc::$ppc-operand-absolute flags)
			  (let* ((info (find val ppc::*ppc-subprims* :key #'subprimitive-info-offset)))
			    (when info (setf (cadr tail) (subprimitive-info-name info))))
			  (if (logbitp ppc::$ppc-operand-fpr flags)
			    (setf (cadr tail) (ppc-fpr val))
			    (if (logbitp ppc::$ppc-operand-vr flags) ; SVS
			      (setf (cadr tail) (ppc-vr val))
			      (when (logbitp ppc::$ppc-operand-gpr flags)
				(setf (cadr tail) (ppc-gpr val))
				(when (eq val ppc::fn)
				  (let* ((disp (car tail)))
				    (when (and disp (typep disp 'fixnum))
				      (let* ((unscaled (+ (- misc-data-offset) disp)))
					(unless (logtest align-mask unscaled)
					  (let* ((idx (ash unscaled (- word-shift))))
					    (if (< idx (uvsize constants))
					      (rplaca tail (list 'quote (uvref constants idx)))))))))))))))))
		  (setq tail (cdr tail))))))))
      (incf pc 4))
    (dolist (l *disassembled-ppc-labels*) (insert-ppc-label l instructions))
    (when regsave-pseudo
      (destructuring-bind (reg offset pc) (lap-instruction-parsed-operands regsave-pseudo)
        (declare (fixnum reg offset pc))
        (let* ((nregs (- 32 reg)))
          (declare (fixnum nregs))
          (setq pc (ash (the fixnum (dpb (ldb (byte 2 0) offset) (byte 2 5) pc)) 2)
                offset (- (logand (lognot 3) (- offset)) (ash nregs target::word-shift))))
        (setf (lap-instruction-opcode regsave-pseudo) :regsave
              (lap-instruction-parsed-operands regsave-pseudo)
              (list (ppc-gpr reg) offset)
              (lap-instruction-address regsave-pseudo) pc)
        (do-dll-nodes (node instructions)
          (when (>= (lap-instruction-address node) pc)
            (insert-dll-node-after regsave-pseudo (dll-node-pred node))
            (return)))))))
              
      
; This returns a doubly-linked list of INSTRUCTION-ELEMENTs; the caller (disassemble, INSPECT)
; can format the contents however it wants.
(defun disassemble-ppc-function (code-vector constants-vector &optional (start-word 0))
  (let* ((*disassembled-ppc-labels* nil)
         (header (make-dll-header)))
    (let* ((n (uvsize code-vector)))
      (declare (fixnum n))
      (do* ((i start-word (1+ i))
            (pc 0 (+ pc 4)))
           ((= i n))
        (declare (fixnum i))
        (let* ((opcode (uvref code-vector i)))
          (declare (integer opcode))
          (if (= opcode 0)
            (return)
            (ppc-disasm-1 opcode pc header))))
      (ppc-analyze-operands header constants-vector))
    header))

(defun print-ppc-instruction (stream tabcount opcode parsed-operands)
  (let* ((name (if (symbolp opcode) opcode (opcode-name opcode))))
    (if (keywordp name)
      (format stream "~&~V,t(~s" tabcount name)
      (format stream "~&~V,t(~a" tabcount name))
    (dolist (op parsed-operands (format stream ")"))
      (format stream (if (and (consp op) (eq (car op) 'quote)) " ~s" " ~a") op))))

(defun print-ppc-instructions (stream instructions &optional for-lap backend)
  (declare (ignorable backend))
  (let* ((tab (if for-lap 6 2)))
    (when for-lap 
      (let* ((lap-function-name (car for-lap)))
        (format stream "~&(~S ~S ~&  (~S (~s) ~&    (~s ~s ()" 
                'nfunction lap-function-name 'lambda '&lap 'ppc-lap-function lap-function-name)))
    (do-dll-nodes (i instructions)
      (etypecase i
        (lap-label (format stream "~&~a " (lap-label-name i)))
        (lap-instruction 
         (print-ppc-instruction stream tab (lap-instruction-opcode i) (lap-instruction-parsed-operands i)))))
    (when for-lap (format stream ")))~&"))))


(defun ppc-Xdisassemble (fn-vector &key (for-lap nil) (stream *standard-output*) target ((:raw *ppc-disassemble-raw-instructions*) nil))
  (let* ((backend (if target (find-backend target) *host-backend*))
         (prefix-length (length (arch::target-code-vector-prefix (backend-target-arch backend))))
         (*ppc-disassembly-backend* backend))
    (print-ppc-instructions stream (function-to-dll-header fn-vector prefix-length)
                            (if for-lap (list (uvref fn-vector (- (uvsize fn-vector) 2)))))
    (values)))

(defun function-to-dll-header (fn-vector &optional (prefix #+ppc32-target 0 #+ppc64-target 1))
  (let* ((codev (uvref fn-vector 0)))
    (disassemble-ppc-function codev fn-vector prefix)))


(defun disassemble-list (thing)
  (let ((dll (function-to-dll-header (function-for-disassembly thing)))
        (address 0)
        (label-p nil)
        (res nil))
    (do-dll-nodes (i dll)
      (setq address (instruction-element-address i))
      (etypecase i
        (lap-label
         (setq label-p (lap-label-name i)))
        (lap-instruction
         (let ((opcode (lap-instruction-opcode i))
               (operands (lap-instruction-parsed-operands i)))
           (push (list* (if label-p `(label ,address) address)
                        (if (symbolp opcode) opcode (opcode-name opcode))
                        operands)
                 res)
           (setq label-p nil)))))
    (nreverse res)))

#+ppc-target
(defun disasm-prin1 (thing stream)
  (if (and (consp thing) (consp (cdr thing)) (null (cddr thing)))
    (cond ((eq (%car thing) 'quote)
           (prin1 thing stream))
          ((eq (%car thing) 'function)
           (format stream "#'~S" (cadr thing)))
          ((eq (%car thing) 16)
             (format stream "#x~X" (cadr thing)))
          ((eq (%car thing) 'label)
           (let ((*print-radix* nil))
             (princ (cadr thing) stream)))
          (t (princ thing stream)))
    (princ thing stream)))


