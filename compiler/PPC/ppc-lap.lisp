;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.


(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "PPC32-ARCH")
  (require "PPC64-ARCH")
  (require "RISC-LAP")
  (require "DLL-NODE")
  (require "PPC-ASM")
  (require "SUBPRIMS"))


(defun ppc-lap-macro-function (name)
  (gethash (string name) (backend-lap-macros *ppc-backend*)))

(defun (setf ppc-lap-macro-function) (def name)
  (let* ((s (string name)))
    (when (gethash s ppc::*ppc-opcode-numbers*)
      (error "~s already defines a PowerPC instruction . " name))
    (when (ppc::ppc-macro-function s)
      (error "~s already defines a PowerPC macro instruction . " name))
    (setf (gethash s (backend-lap-macros *ppc-backend*)) def)))

(defmacro defppclapmacro (name arglist &body body)
  `(progn
     (setf (ppc-lap-macro-function ',name)
           (nfunction (ppc-lap-macro ,name) ,(parse-macro name arglist body)))
     (record-source-file ',name 'lap-macro)
     ',name))

(defvar *ppc-lap-constants* ())
(defvar *ppc-lap-regsave-reg* ())
(defvar *ppc-lap-regsave-addr* ())
(defvar *ppc-lap-regsave-label* ())
(defparameter *ppc-lwz-instruction* (svref ppc::*ppc-opcodes* (gethash "LWZ" ppc::*ppc-opcode-numbers*)))
(defvar *ppc-lap-lfun-bits* 0)





(defun ppc-lap-macroexpand-1 (form)
  (unless (and (consp form) (atom (car form)))
    (values form nil))
  (let* ((expander (ppc-lap-macro-function (car form))))
    (if expander
      (values (funcall expander form nil) t)
      (values form nil))))



(defun ppc-lap-encode-regsave-info (maxpc)
  (declare (fixnum maxpc))
  (if *ppc-lap-regsave-label*
    (let* ((regsave-pc (ash (the fixnum (lap-label-address *ppc-lap-regsave-label*)) -2)))
      (declare (fixnum regsave-pc))
      (if (< regsave-pc #x80)
        (let* ((instr (ppc-emit-lap-instruction *ppc-lwz-instruction*
                                                (list *ppc-lap-regsave-reg*
                                                      (dpb (ldb (byte 2 5) regsave-pc) 
                                                           (byte 2 0) 
                                                           *ppc-lap-regsave-addr*)
                                                      (ldb (byte 5 0) regsave-pc)))))
          (setf (lap-instruction-address instr) maxpc)
          (incf maxpc 4))
        (warn "Can't encode register save information."))))
  maxpc)

(defun %define-ppc-lap-function (name body &optional (bits 0))
  (with-dll-node-freelist (*lap-instructions* *lap-instruction-freelist*)
      (let* ((*lap-labels* ())
             (*ppc-lap-regsave-label* ())
             (*ppc-lap-regsave-reg* ())
             (*ppc-lap-regsave-addr* ())
             (*ppc-lap-constants* ())
             (*ppc-lap-lfun-bits* bits))
        (dolist (form body)
          (ppc-lap-form form))
        #+ppc-lap-scheduler (ppc-schedule-instuctions)       ; before resolving branch targets
        (ppc-lap-generate-code name (ppc-lap-encode-regsave-info (ppc-lap-do-labels)) *ppc-lap-lfun-bits*))))

;;; Any conditional branch that the compiler generates is currently just of the form
;;; BT or BF, but it'd be nice to recognize all of the other extended branch mnemonics
;;; as well.
;;; A conditional branch is "conditional" if bit 2 of the BO field is set.
(defun ppc-lap-conditional-branch-p (insn)
  (let* ((opcode (lap-instruction-opcode insn)))
    (if (= (the fixnum (opcode-majorop opcode)) 16)    ; it's a BC instruction ...
      (unless (logbitp 1 (the fixnum (opcode-op-low opcode)))          ; not absolute
        (let* ((bo-field (if (= #xf (ldb (byte 4 6) (the fixnum (opcode-mask-high opcode))))
                           (ldb (byte 5 5) (the fixnum (opcode-op-high opcode)))
                           (svref (lap-instruction-parsed-operands insn) 0))))
          (declare (fixnum bo-field))
          (if (logbitp 2 bo-field)
            bo-field))))))

;;; Turn an instruction that's of the form 
;;;   (bc[l] bo bi label) 
;;; into the sequence
;;;   (bc (invert bo) bi @new)
;;;   (b[l] label)
;;; @new
;;; Do so only if the instruction's a conditional branch
;;; and the label is more than 16 bits away from the instruction.
;;; Return true if we do this, false otherwise.
(defun ppc-lap-invert-conditional-branch (insn label)
  (if (ppc-lap-conditional-branch-p insn)      
    (let* ((diff (- (lap-label-address label) (lap-instruction-address insn))))
      (declare (fixnum diff))
      (if (or (< diff #x-8000) (> diff #x7ffc))
        ; Too far away, will have to invert.
        ; It's necessary to "partially assemble" the BC instruction in order to 
        ; get explicit values for the BO and BI fields of the instruction.
        (let* ((original-opcode (lap-instruction-opcode insn))
               (vals (lap-instruction-parsed-operands insn))
               (high (opcode-op-high original-opcode))
               (low (opcode-op-low original-opcode))
               (link-p (logbitp 0 low))
               (new-label (make-lap-label (gensym)))
               (idx -1))
          (declare (fixnum high low))
          ; Assemble all operands but the last
          (do* ((ops (opcode-operands original-opcode) next)
                (next (cdr ops) (cdr next)))
               ((null next))
            (declare (list ops next))
            (let* ((operand (car ops))
                   (val (if (logbitp operand-fake (operand-flags operand))
                    0
                    (svref vals (incf idx))))
                   (insert-function (operand-insert-function operand)))
              (setq high (if insert-function
                           (funcall insert-function high low val)
                           (ppc::insert-default operand high low val)))))
          ;; "high" now contains the major opcode, BO, and BI fields
          ;; of the original branch instruction.  Generate a (BC
          ;; (invert BO) BI new-label) instruction, and insert it
          ;; before the original instruction.
          (let* ((bc-opcode (svref ppc::*ppc-opcodes* (gethash "BC" ppc::*ppc-opcode-numbers*)))
                 (bo (logxor #b1000 (the fixnum (ldb (byte 5 5) high))))
                 (bi (ldb (byte 5 0) high))
                 (new-instruction (make-lap-instruction bc-opcode))
                 (opvect (alloc-lap-operand-vector)))
            (setf (lap-instruction-parsed-operands new-instruction) opvect
                  (svref opvect 0) bo
                  (svref opvect 1) bi
                  (svref opvect 2) new-label)
            (push new-instruction (lap-label-refs new-label))
            (insert-dll-node-after new-instruction (dll-node-pred insn))
            (insert-dll-node-after new-label insn))
          ;; Now, change INSN's opcode to B or BL, and make sure that
          ;; it references nothing but the old label.
          (let* ((long-branch (svref ppc::*ppc-opcodes* (gethash (if link-p "BL" "B") ppc::*ppc-opcode-numbers*)))
                 (opvect (alloc-lap-operand-vector)))
            (setf (svref opvect 0) label
                  (lap-instruction-opcode insn) long-branch
                  (lap-instruction-parsed-operands insn) opvect)
            ;; We're finally done.  Return t.
            t))))))
            

; Build & return list of all labels that are targets of conditional branches.
(defun ppc-lap-conditional-branch-targets ()
  (let* ((branch-target-labels ()))
    (do-lap-labels (lab branch-target-labels)
      (dolist (insn (lap-label-refs lab))
        (when (ppc-lap-conditional-branch-p insn)
          (push lab branch-target-labels))))))

(defun ppc-lap-assign-addresses (delete-labels-p)
  (let* ((pc 0))
    (declare (fixnum pc))
    (do-dll-nodes (node *lap-instructions*)
      (setf (instruction-element-address node) pc)
      (if (typep node 'lap-label)
        (if delete-labels-p (remove-dll-node node))
        (incf pc 4)))
    ;; Don't bother checking code-vector size yet.
    pc))

;;; The function's big enough that we might have generated conditional
;;; branches that are too far away from their targets.  Find the set
;;; of all labels that are the target of conditional branches, then
;;; repeatedly assign (tentative) addresses to all instructions and
;;; labels and iterate over the set of conditional branch targets,
;;; "lengthening" any condtional branches that are too far away from
;;; the target label.  Since lengthening a branch instruction can
;;; cause a spanning branch to become a candidate for lengthening, we
;;; have to repeat the process until all labels are the targets of
;;; valid (short enough or unconditional) branch instructions.
(defun ppc-lap-remove-long-branches ()
  (let* ((branch-target-labels (ppc-lap-conditional-branch-targets)))
    (do* ((done nil))
         (done (ppc-lap-assign-addresses t))
      (setq done t)
      (ppc-lap-assign-addresses nil)
      (dolist (lab branch-target-labels)
        (dolist (insn (lap-label-refs lab))
          (when (ppc-lap-invert-conditional-branch insn lab)
            (setq done nil)))))))

(defun ppc-lap-do-labels ()
  (do-lap-labels (lab)
    (if (and (lap-label-refs lab) (not (lap-label-emitted-p lab)))
      (error "Label ~S was referenced but never defined. " 
             (lap-label-name lab)))
    ;; Repeatedly iterate through label's refs, until none of them is
    ;; the preceding instruction.  This eliminates
    ;; (b @next)
    ;;@next
    ;;
    ;; but can probably be fooled by hairier nonsense.
    (loop
      (when (dolist (ref (lap-label-refs lab) t)
              (when (eq lab (lap-instruction-succ ref))
                (remove-dll-node ref)
                (setf (lap-label-refs lab) (delete ref (lap-label-refs lab)))
                (return)))
        (return))))
  ;; Assign pc to emitted labels, splice them out of the list.
  
  (if (> (the fixnum (dll-header-length *lap-instructions*)) 8191)
    ;; -Might- have some conditional branches that are too long.
    ;; Definitely don't  otherwise, so only bother to check in this case
    (ppc-lap-remove-long-branches)
    (ppc-lap-assign-addresses t)))

;;; Replace each label with the difference between the label's address
;;; and the referencing instruction's address.
(defun ppc-lap-resolve-labels ()
  (do-lap-labels (label)
    (let* ((label-address (lap-label-address label)))
      (declare (fixnum label-address))          ; had BETTER be ...
      (dolist (insn (lap-label-refs label))
        (let* ((diff (- label-address (lap-instruction-address insn))))
          (declare (fixnum diff))
          (let* ((opvals (lap-instruction-parsed-operands insn))
                 (pos (position label opvals)))
            (unless pos
              (error "Bug: label ~s should be referenced by instruction ~s, but isn't." label insn))
            (setf (svref opvals pos) diff)))))))

(defun ppc-lap-generate-instruction (code-vector index insn)
  (let* ((op (lap-instruction-opcode insn))
         (vals (lap-instruction-parsed-operands insn))
         (high (opcode-op-high op))
         (low (opcode-op-low op))
         (idx -1))
    (dolist (operand (opcode-operands op))
      (let* ((val (if (logbitp operand-fake (operand-flags operand))
                    0
                    (svref vals (incf idx))))
             (insert-function (operand-insert-function operand)))
        (multiple-value-setq (high low)
          (if insert-function
            (funcall insert-function high low val)
            (ppc::insert-default operand high low val)))
        (if (null high)
          (error "Invalid operand for ~s instruction: ~d" (opcode-name op) val))))
    (setf (lap-instruction-parsed-operands insn) nil)
    (free-lap-operand-vector vals)
    (locally (declare (type (simple-array (unsigned-byte 16) (*)) code-vector)
                      (optimize (speed 3) (safety 0)))
      (setf (aref code-vector (+ index index)) high
            (aref code-vector (+ index index 1)) low)
     nil)))

(defparameter *use-traceback-tables* nil)

(defun traceback-fullwords (pname)
  (if (and *use-traceback-tables* pname (typep pname 'simple-base-string))
    (ceiling (+ 22 (length pname)) 4)
    0))

(defun add-traceback-table (code-vector start pname)
  (flet ((out-byte (v i8 b)
            (declare (type (simple-array (unsigned-byte 8) (*)) v)
                    (optimize (speed 3) (safety 0))
                    (fixnum i8))
            (setf (aref v i8) b)))          
    (flet ((out-bytes (v i32 b0 b1 b2 b3)
           (declare (type (simple-array (unsigned-byte 8) (*)) v)
                    (optimize (speed 3) (safety 0))
                    (fixnum i32))
           (let* ((i8 (ash i32 2)))
             (declare (fixnum i8))
             (setf (aref v i8) b0
                   (aref v (%i+ i8 1)) b1
                   (aref v (%i+ i8 2)) b2
                   (aref v (%i+ i8 3)) b3))))
      (setf (uvref code-vector start) 0)
      (out-bytes code-vector (1+ start)
                 0                          ; traceback table version
                 0                          ; language id 7 - try 0 instead (means C) or 9 means C++
                 #x20                       ; ???
                 #x41)                      ; ???
      (out-bytes code-vector (+ start 2)
                 #x80 #x06 #x01 #x00)       ; ??? ??? ??? ???
      (setf (uvref code-vector (+ start 3)) #x0)
      (setf (uvref code-vector (+ start 4)) (ash start 2))
      (let* ((namelen (length pname))
             (pos (ash (the fixnum (+ start 5)) 2)))
        (declare (fixnum namelen pos))
        (out-byte code-vector pos (ldb (byte 8 8) namelen))
        (incf pos)
        (out-byte code-vector pos (ldb (byte 8 0) namelen))
        (incf pos)
        (dotimes (i namelen) 
          (out-byte code-vector pos (char-code (schar pname i)))
          (incf pos))))))

(defun ppc-lap-generate-code (name maxpc bits &optional (traceback nil))
  (declare (fixnum maxpc))
  (let* ((target-backend *target-backend*)
         (cross-compiling (not (eq *host-backend* target-backend)))
	 (traceback-size
	  (traceback-fullwords (and traceback
				    name
				    (setq traceback (symbol-name name)))))
         (prefix (arch::target-code-vector-prefix (backend-target-arch *target-backend*)))
         (prefix-size (length prefix))
         (code-vector-size (+ (ash maxpc -2) traceback-size prefix-size))

         (constants-size (+ 3 (length *ppc-lap-constants*)))
         (constants-vector (%alloc-misc
                            constants-size
			    (if cross-compiling
			      target::subtag-xfunction
			      target::subtag-function)))
         (i prefix-size))
    (declare (fixnum i constants-size))
    #+ppc32-target
    (if (>= code-vector-size (ash 1 19)) (compiler-function-overflow))
    (let* ((code-vector (%alloc-misc
                         code-vector-size
                         (if cross-compiling
                           target::subtag-xcode-vector
                           target::subtag-code-vector))))
      (dotimes (j prefix-size)
        (setf (uvref code-vector j) (pop prefix)))
      (ppc-lap-resolve-labels)          ; all operands fully evaluated now.
      (do-dll-nodes (insn *lap-instructions*)
        (ppc-lap-generate-instruction code-vector i insn)
        (incf i))
      (unless (eql 0 traceback-size)
        (add-traceback-table code-vector i traceback))
      (dolist (immpair *ppc-lap-constants*)
        (let* ((imm (car immpair))
               (k (cdr immpair)))
          (declare (fixnum k))
          (setf (uvref constants-vector
                       (ash
                        (- k (arch::target-misc-data-offset (backend-target-arch target-backend)))
                        (- (arch::target-word-shift (backend-target-arch target-backend)))))
                imm)))
      (setf (uvref constants-vector (1- constants-size)) bits ; lfun-bits
            (uvref constants-vector (- constants-size 2)) name
            (uvref constants-vector 0) code-vector)
      #+ppc-target (%make-code-executable code-vector)
      constants-vector)))

(defun ppc-lap-pseudo-op (form)
  (case (car form)
    (:regsave
     (if *ppc-lap-regsave-label*
       (warn "Duplicate :regsave form not handled (yet ?) : ~s" form)
       (destructuring-bind (reg addr) (cdr form)
         (let* ((regno (ppc-register-name-or-expression reg)))
           (if (not (<= ppc::save7 regno ppc::save0))
             (warn "Not a save register: ~s.  ~s ignored." reg form)
             (let* ((addrexp (ppc-register-name-or-expression addr)))   ; parses 'fixnum
               (if (not (and (typep addrexp 'fixnum)
                             (<= 0 addrexp #x7ffc)      ; not really right
                             (not (logtest 3 addrexp))))
                 (warn "Invalid logical VSP: ~s.  ~s ignored." addr form)
                 (setq *ppc-lap-regsave-label* (emit-lap-label (gensym))
                       *ppc-lap-regsave-reg* regno
                       *ppc-lap-regsave-addr* (- (+ addrexp)
                                                 (* 4 (1+ (- ppc::save0 regno))))))))))))
    (:arglist (setq *ppc-lap-lfun-bits* (encode-lambda-list (cadr form))))))

       
(defun ppc-lap-form (form)
  (if (and form (symbolp form))
    (emit-lap-label form)
    (if (or (atom form) (not (symbolp (car form))))
      (error "~& unknown PPC-LAP form: ~S ." form)
      (multiple-value-bind (expansion expanded)
                           (ppc-lap-macroexpand-1 form)
        (if expanded
          (ppc-lap-form expansion)
          (let* ((name (car form)))
            (if (keywordp name)
              (ppc-lap-pseudo-op form)
              (case name
                ((progn) (dolist (f (cdr form)) (ppc-lap-form f)))
                ((let) (ppc-lap-equate-form (cadr form) (cddr form)))
                (t
                 ; instruction macros expand into instruction forms
                 ; (with some operands reordered/defaulted.)
                 (let* ((expander (ppc::ppc-macro-function name)))
                   (if expander
                     (ppc-lap-form (funcall expander form nil))
                     (ppc-lap-instruction name (cdr form)))))))))))))

;;; (let ((name val) ...) &body body)
;;; each "val" gets a chance to be treated as a PPC register name
;;; before being evaluated.
(defun ppc-lap-equate-form (eqlist body) 
  (let* ((symbols (mapcar #'(lambda (x)
                              (let* ((name (car x)))
                                (or
                                 (and name 
                                      (symbolp name)
                                      (not (constant-symbol-p name))
                                      name)
                                 (error 
                                  "~S is not a bindable symbol name ." name))))
                          eqlist))
         (values (mapcar #'(lambda (x) (or (ppc-vr-name-p (cadr x))
					   (ppc-fpr-name-p (cadr x))
					   (ppc-register-name-or-expression
					    (cadr x))))
                         eqlist)))
    (progv symbols values
                   (dolist (form body)
                     (ppc-lap-form form)))))

(defun ppc-lap-constant-offset (x)
  (or (cdr (assoc x *ppc-lap-constants* :test #'equal))
      (let* ((target-backend *target-backend*)
             (n (+ (arch::target-misc-data-offset (backend-target-arch target-backend))
                   (ash (1+ (length *ppc-lap-constants*))
                        (arch::target-word-shift (backend-target-arch target-backend))))))
        (push (cons x n) *ppc-lap-constants*)
        n)))

; Evaluate an arbitrary expression; warn if the result isn't a fixnum.
(defun ppc-lap-evaluated-expression (x)
  (if (typep x 'fixnum)
    x
    (if (null x)
      (arch::target-nil-value (backend-target-arch *target-backend*))
      (if (eq x t)
        (+ (arch::target-nil-value (backend-target-arch *target-backend*))
           (arch::target-t-offset  (backend-target-arch *target-backend*)))
        (let* ((val (handler-case (eval x) ; Look! Expression evaluation!
                      (error (condition) (error "~&Evaluation of ~S signalled assembly-time error ~& ~A ."
                                                x condition)))))
          (unless (typep val 'fixnum)
            (warn "assembly-time evaluation of ~S returned ~S, which may not have been intended ."
                  x val))
          val)))))

(defparameter *ppc-lap-register-aliases*
  `((nfn . ,ppc::nfn)
    (fname . ,ppc::fname)))

(defparameter *ppc-lap-fp-register-aliases*
  ())

(defparameter *ppc-lap-vector-register-aliases*
  ())

(defun ppc-gpr-name-p (x)
  (and (or (symbolp x) (stringp x))
           (or
            (position (string x) ppc::*gpr-register-names* :test #'string-equal)
            (cdr (assoc x *ppc-lap-register-aliases* :test #'string-equal)))))

(defun ppc-register-name-or-expression (x)
  (if x
    (or (ppc-gpr-name-p x)
        (if (and (consp x) (eq (car x) 'quote))
          (let* ((quoted-form (cadr x)))
            (if (null quoted-form)
              (arch::target-nil-value (backend-target-arch *target-backend*))
              (if (eq quoted-form t)
                (+ (arch::target-nil-value (backend-target-arch *target-backend*))
                   (arch::target-t-offset (backend-target-arch *target-backend*)))
                (if (typep quoted-form 'fixnum)
                  (ash quoted-form (arch::target-fixnum-shift (backend-target-arch *target-backend*)))
                  (ppc-lap-constant-offset quoted-form)))))
          (ppc-lap-evaluated-expression x)))
    (arch::target-nil-value (backend-target-arch *target-backend*))))

(defun ppc-fpr-name-p (x)
  (and (or (symbolp x) (stringp x))
                   (or
                    (position (string x) ppc::*fpr-register-names* :test #'string-equal)
                    (cdr (assoc x *ppc-lap-fp-register-aliases* :test #'string-equal)))))

(defun ppc-fp-register-name-or-expression (x)
  (or (ppc-fpr-name-p x)
      (ppc-lap-evaluated-expression x)))

(defun ppc-vr-name-p (x)
  (and (or (symbolp x) (stringp x))
	     (or
	      (position (string x) ppc::*vector-register-names* :test #'string-equal)
	      (cdr (assoc x *ppc-lap-vector-register-aliases* :test #'string-equal)))))

(defun ppc-vector-register-name-or-expression (x)
  (or (ppc-vr-name-p x)
      (ppc-lap-evaluated-expression x)))


(defparameter *ppc-cr-field-names* #(:crf0 :crf1 :crf2 :crf3 :crf4 :crf5 :crf6 :crf7))
(defparameter *ppc-cr-names* #(:cr0 :cr1 :cr2 :cr3 :cr4 :cr5 :cr6 :cr7))
(defparameter *ppc-cc-bit-names* #(:lt :gt :eq :so :un))
(defparameter *ppc-cc-bit-inverse-names* #(:ge :le :ne :ns :nu))

; This wants a :CC, a negated :CC, or either (:CRn :CC) or (:CRn :~CC).
; Returns the fully-qualified CR bit and an indication of whether or not the CC was 
; negated.
(defun ppc-lap-parse-test (x)
  (if (or (symbolp x) (stringp x))
    (let* ((pos (position x *ppc-cc-bit-names* :test #'string-equal)))
      (if pos
        (values (min pos 3) nil)
        (if (setq pos (position x *ppc-cc-bit-inverse-names* :test #'string-equal))
          (values (min pos 3) t)
          (error "Unknown PPC lap condition form : ~s" x))))
    (if (and (consp x) (keywordp (car x)) (consp (cdr x)) (keywordp (cadr x)))
      (let* ((field (position (car x) *ppc-cr-names*)))
        (unless field (error "Unknown CR field name : ~s" (car x)))
        (let* ((bit (position (cadr x) *ppc-cc-bit-names*)))
          (if bit 
            (values (logior (ash field 2) (min bit 3)) nil)
            (if (setq bit (position (cadr x) *ppc-cc-bit-inverse-names*))
              (values (logior (ash field 2) (min bit 3)) t)
              (error "Unknown condition name : ~s" (cadr x))))))
      (error "Unknown PPC lap condition form : ~s" x))))

; Accept either :CRn, :CC,  or (:CRFn :CC), or evaluate an expression.
(defun ppc-lap-cr-field-expression (x)
  (if (or (symbolp x) (stringp x))
    (let* ((pos (position x *ppc-cr-names* :test #'string-equal)))
      (if pos 
        (ash pos 2)
        (let* ((cc-pos (position x *ppc-cc-bit-names* :test #'string-equal)))
          (if cc-pos 
            (min cc-pos 3)
            (ppc-lap-evaluated-expression x)))))
    (if (and (consp x) (keywordp (car x)) (consp (cdr x)) (keywordp (cadr x)))
      (let* ((field (position (car x) *ppc-cr-field-names*))
             (bit (position (cadr x) *ppc-cc-bit-names*)))
        (if (and field bit)
          (logior (min bit 3) (ash field 2))
          (error "Bad ppc-cr-field-expression: ~s" x)))
      (ppc-lap-evaluated-expression x))))
  
(defun ppc-lap-instruction (name opvals)
  (let* ((opnum (gethash (string name) ppc::*ppc-opcode-numbers*))
         (opcode (and opnum 
                          (< -1 opnum (length ppc::*ppc-opcodes*))
                          (svref ppc::*ppc-opcodes* opnum))))
    (unless opcode
          (error "Unknown PPC opcode: ~a" name))
    ;; Unless either
    ;;  a) The number of operand values in the macro call exactly
    ;;      matches the number of operands accepted by the instruction or
    ;;  b) The number of operand values is one less, and the instuction
    ;;     takes an optional operand
    ;;  we've got a wrong-number-of-args error.
    ;;  In case (b), there's at most one optional argument per instruction;
    ;;   provide 0 for the missing value.
    (let* ((operands (opcode-operands opcode))
           (nmin (opcode-min-args opcode))
           (nmax (opcode-max-args opcode))
           (nhave (length opvals)))
      (declare (fixnum nmin nmax nhave))
      (if (= nhave nmax)
        (ppc-emit-lap-instruction opcode opvals)
        (if (> nhave nmax)
          (error "Too many operands in ~s (~a accepts at most ~d)"
                 opvals name nmax)
          (if (= nhave nmin)
            (let* ((newops ()))
              (dolist (op operands (ppc-emit-lap-instruction opcode (nreverse newops)))
                (let* ((flags (operand-flags op)))
                  (unless (logbitp operand-fake flags)
                    (push (if (logbitp operand-optional flags)
                            0
                            (pop opvals))
                          newops)))))
            (error "Too few operands in ~s : (~a requires at least ~d)"
                   opvals name nmin)))))))

; This is pretty rudimentary: if the operand has the "ppc::$ppc-operand-relative" bit
; set, we demand a label name and note the fact that we reference the label in question.
; Otherwise, we use the "register-name-or-expression" thing.
; Like most PPC assemblers, this lets you treat everything as an expression, even if
; you've got the order of some arguments wrong ...

(defun ppc-parse-lap-operand (opvalx operand insn)
  (let* ((flags (operand-flags operand)))
    (declare (fixnum flags))
    (if (logbitp ppc::$ppc-operand-relative flags)
      (lap-note-label-reference opvalx insn)
      (if (logbitp ppc::$ppc-operand-cr flags)
        (ppc-lap-cr-field-expression opvalx)
        (if (logbitp ppc::$ppc-operand-absolute flags)
          (ppc-subprimitive-address opvalx)
          (if (logbitp ppc::$ppc-operand-fpr flags)
            (ppc-fp-register-name-or-expression opvalx)
	    (if (logbitp ppc::$ppc-operand-vr flags) ; SVS
	      (ppc-vector-register-name-or-expression opvalx)
	      (ppc-register-name-or-expression opvalx))))))))

(defun ppc-subprimitive-address (x)
  (if (and x (or (symbolp x) (stringp x)))
    (let* ((info (find x ppc::*ppc-subprims* :test #'string-equal :key #'subprimitive-info-name)))
      (when info (return-from ppc-subprimitive-address
                   (subprimitive-info-offset info)))))
  (ppc-lap-evaluated-expression x))


;;; We've checked that the number of operand values match the number
;;; expected (and have set "fake" operand values to 0.)  Labels - and
;;; some constructs that might someday do arithmetic on them - are
;;; about the only class of forward references we need to deal with.
;;; This whole two-pass scheme seems overly general, but if/when we
;;; ever do instruction scheduling it'll probably make it simpler.
(defun ppc-emit-lap-instruction (opcode opvals)
  (let* ((operands (opcode-operands opcode))
         (parsed-values (alloc-lap-operand-vector))
         (insn (make-lap-instruction opcode))
         (idx -1))
    (declare (fixnum idx))
    (dolist (op operands)
      (let* ((flags (operand-flags op))
             (val (if (logbitp operand-fake flags)
                    0
                    (ppc-parse-lap-operand (pop opvals) op insn))))
        (declare (fixnum flags))
        (setf (svref parsed-values (incf idx)) val)))
    (setf (lap-instruction-parsed-operands insn) parsed-values)
    (append-dll-node insn *lap-instructions*)))



(defmacro defppclapfunction (&environment env name arglist &body body
                             &aux doc)
  (if (not (endp body))
      (and (stringp (car body))
           (cdr body)
           (setq doc (car body))
           (setq body (cdr body))))
  `(progn
     (eval-when (:compile-toplevel)
       (note-function-info ',name t ,env))
     #-ppc-target
     (progn
       (eval-when (:load-toplevel)
         (%defun (nfunction ,name (lambda (&lap 0) (ppc-lap-function ,name ,arglist ,@body))) ,doc))
       (eval-when (:execute)
         (%define-ppc-lap-function ',name '((let ,arglist ,@body)))))
     #+ppc-target	; just shorthand for defun
     (%defun (nfunction ,name (lambda (&lap 0) (ppc-lap-function ,name ,arglist ,@body))) ,doc)))
 


(provide "PPC-LAP")
