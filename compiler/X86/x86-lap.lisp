;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005, Clozure Associates and contributors.
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

(require "X86-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "DLL-NODE"))

(def-standard-initial-binding *x86-lap-label-freelist* (make-dll-node-freelist))

(def-standard-initial-binding *x86-lap-frag-vector-freelist* (%cons-pool))

(defun %allocate-vector-list-segment ()
  (without-interrupts
   (let* ((data (pool.data *x86-lap-frag-vector-freelist*)))
     (if data
       (progn
         (when (null (list-length data))
           (compiler-bug "frag-vector freelist is circular"))
         (setf (pool.data *x86-lap-frag-vector-freelist*) (cdr data))
         (rplacd data nil))
       (cons (make-array 24 :element-type '(unsigned-byte 8)) nil)))))

(defun %free-vector-list-segment (segment)
  (without-interrupts
   (setf (pool.data *x86-lap-frag-vector-freelist*)
         (nconc segment (pool.data *x86-lap-frag-vector-freelist*)))))

(defun %vector-list-ref (vector-list index)
  (do* ((i index (- i len))
        (vl vector-list (cdr vl))
        (v (car vl) (car vl))
        (len (length v) (length v)))
       ((null vl) (error "Index ~s is out of bounds for ~s" index vector-list))
    (if (< i len)
      (return (aref v i)))))

(defun (setf %vector-list-ref) (new vector-list index)
  (do* ((i index (- i len))
        (vl vector-list (cdr vl))
        (v (car vl) (car vl))
        (len (length v) (length v)))
       ((< i len) (setf (aref v i) new))
    (when (null (cdr vl))
      (setf (cdr vl) (%allocate-vector-list-segment)))))

(defun %truncate-vector-list (vector-list newlen)
  (do* ((vl vector-list (cdr vl))
        (v (car vl) (car vl))
        (len (length v) (length v))
        (total len (+ total len)))
       ((null (cdr vl)))
    (when (> total newlen)
      (%free-vector-list-segment (cdr vl))
      (return (setf (cdr vl) nil)))))
        
  



(eval-when (:execute :load-toplevel)

  (defstruct (x86-lap-note (:include ccl::dll-node))
    peer
    id)

  (defstruct (x86-lap-note-begin (:include x86-lap-note)))
  (defstruct (x86-lap-note-end (:include x86-lap-note)))
    
  (defstruct (x86-lap-label (:constructor %%make-x86-lap-label (name)))
    name
    frag
    offset
    )

  (defstruct (frag (:include ccl::dll-node)
                   (:constructor %make-frag))
    address
    last-address                        ; address may change during relax
    type                                ; nil, or (:TYPE &rest args)
    relocs                              ; relocations against this frag
    (position 0)                        ; position in code-buffer
    (code-buffer (%allocate-vector-list-segment))     ; a VECTOR-LIST
    labels                              ; labels defined in this frag
    ))

(def-standard-initial-binding *frag-freelist* (make-dll-node-freelist))


(defun frag-push-byte (frag b)
  (let* ((pos (frag-position frag)))
    (setf (%vector-list-ref (frag-code-buffer frag) pos) b
          (frag-position frag) (1+ pos))
    b))

(defun frag-ref (frag index)
  (%vector-list-ref (frag-code-buffer frag) index))

(defun (setf frag-ref) (new frag index)
  (setf (%vector-list-ref (frag-code-buffer frag) index) new))

;;; get/set little-endian 32 bit word in frag at index
(defun frag-ref-32 (frag index)
  (let ((result 0))
    (setf (ldb (byte 8 0) result) (frag-ref frag index)
	  (ldb (byte 8 8) result) (frag-ref frag (+ index 1))
	  (ldb (byte 8 16) result) (frag-ref frag (+ index 2))
	  (ldb (byte 8 24) result) (frag-ref frag (+ index 3)))
    result))

(defun (setf frag-ref-32) (new frag index)
  (setf (frag-ref frag index) (ldb (byte 8 0) new)
	(frag-ref frag (+ index 1)) (ldb (byte 8 8) new)
	(frag-ref frag (+ index 2)) (ldb (byte 8 16) new)
	(frag-ref frag (+ index 3)) (ldb (byte 8 24) new)))

(defun frag-length (frag)
  (frag-position frag))

(defun (setf frag-length) (new frag)
  (%truncate-vector-list (frag-code-buffer frag) new)
  (setf (frag-position frag) new))


;;; Push 1, 2, 4, or 8 bytes onto the frag-list's current-frag's buffer.
;;; (If pushing more than one byte, do so in little-endian order.)
(defun frag-list-push-byte (frag-list b)
  (frag-push-byte (frag-list-current frag-list) b))

(defun frag-list-push-16 (frag-list w)
  (let* ((frag (frag-list-current frag-list)))
    (frag-push-byte frag (ldb (byte 8 0) w))
    (frag-push-byte frag (ldb (byte 8 8) w))))

(defun frag-list-push-32 (frag-list w)
  (let* ((frag (frag-list-current frag-list)))
    (frag-push-byte frag (ldb (byte 8 0) w))
    (frag-push-byte frag (ldb (byte 8 8) w))
    (frag-push-byte frag (ldb (byte 8 16) w))
    (frag-push-byte frag (ldb (byte 8 24) w))
    w))

(defun frag-list-push-64 (frag-list w)
  (let* ((frag (frag-list-current frag-list)))
    (frag-push-byte frag (ldb (byte 8 0) w))
    (frag-push-byte frag (ldb (byte 8 8) w))
    (frag-push-byte frag (ldb (byte 8 16) w))
    (frag-push-byte frag (ldb (byte 8 24) w))
    (frag-push-byte frag (ldb (byte 8 32) w))
    (frag-push-byte frag (ldb (byte 8 40) w))
    (frag-push-byte frag (ldb (byte 8 48) w))
    (frag-push-byte frag (ldb (byte 8 56) w))
    w))

;;; Returns the length of the current frag
(defun frag-list-position (frag-list)
  (frag-length (frag-list-current frag-list)))

(defun frag-output-bytes (frag target target-offset)
  (let* ((buffer (frag-code-buffer frag))
         (n (frag-length frag))
         (remain n))
    (loop
      (when (zerop remain) (return n))
      (let* ((v (pop buffer))
             (len (length v))
             (nout (min remain len)))
        (%copy-ivector-to-ivector v
                                  0
                                  target
                                  target-offset
                                  nout)
        (incf target-offset nout)
        (decf remain nout)))))

(defun make-frag ()
  (let* ((frag (alloc-dll-node *frag-freelist*)))
    (if frag
      (let* ((buffer (frag-code-buffer frag)))
        (when buffer
          (setf (frag-length frag) 0))
        (setf (frag-address frag) nil
              (frag-last-address frag) nil
              (frag-type frag) nil
              (frag-relocs frag) nil
              (frag-labels frag) nil)
        frag)
      (%make-frag))))
  

;;; Intentionally very similar to RISC-LAP, but with some extensions
;;; to deal with alignment and with variable-length and/or span-
;;; dependent instructions.

(defvar *x86-lap-labels* ())
(defvar *x86-lap-constants* ())
(defparameter *x86-lap-entry-offset* nil)
(defparameter *x86-lap-fixed-code-words* nil)
(defvar *x86-lap-lfun-bits* 0)

(defun x86-lap-macro-function (name)
  (gethash (string name) (backend-lap-macros *target-backend*)))

(defun (setf x86-lap-macro-function) (def name)
  (let* ((s (string name)))
    (when (gethash s x86::*x86-opcode-template-lists*)
      (error "~s already defines an x86 instruction." name))
    (setf (gethash s (backend-lap-macros *target-backend*)) def)))

(defmacro defx86lapmacro (name arglist &body body)
  `(progn
     (setf (x86-lap-macro-function ',name)
           (nfunction (x86-lap-macro ,name) ,(ccl::parse-macro name arglist body)))
     (record-source-file ',name 'x86-lap)
     ',name))

(defun x86-lap-macroexpand-1 (form)
  (unless (and (consp form) (atom (car form)))
    (values form nil))
  (let* ((expander (x86-lap-macro-function (car form))))
    (if expander
      (values (funcall expander form nil) t)
      (values form nil))))


(defmethod print-object ((l x86-lap-label) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~a" (x86-lap-label-name l))))

;;; Labels

(defun %make-x86-lap-label (name)
  (let* ((lab (alloc-dll-node *x86-lap-label-freelist*)))
    (if lab
      (progn
        (setf (x86-lap-label-frag lab) nil
              (x86-lap-label-offset lab) nil
              (x86-lap-label-name lab) name)
        lab)
      (%%make-x86-lap-label name))))
  
(defun make-x86-lap-label (name)
  (let* ((lab (%make-x86-lap-label name)))
    (if (typep *x86-lap-labels* 'hash-table)
      (setf (gethash name *x86-lap-labels*) lab)
      (progn
        (push lab *x86-lap-labels*)
        (if (> (length *x86-lap-labels*) 255)
          (let* ((hash (make-hash-table :size 512 :test #'eq)))
            (dolist (l *x86-lap-labels* (setq *x86-lap-labels* hash))
              (setf (gethash (x86-lap-label-name l) hash) l))))))
    lab))

(defun find-x86-lap-label (name)
  (if (typep *x86-lap-labels* 'hash-table)
    (gethash name *x86-lap-labels*)
    (car (member name *x86-lap-labels* :test #'eq :key #'x86-lap-label-name))))

(defun find-or-create-x86-lap-label (name)
  (or (find-x86-lap-label name)
      (make-x86-lap-label name)))


;;; A label can only be emitted once.  Once it's been emitted, its frag
;;; slot will be non-nil.

(defun x86-lap-label-emitted-p (lab)
  (not (null (x86-lap-label-frag lab))))

(defun emit-x86-lap-label (frag-list name)
  (let* ((lab (find-or-create-x86-lap-label name))
         (current (frag-list-current frag-list)))
    (when (x86-lap-label-emitted-p lab)
      (error "Label ~s: multiply defined." name))
    (setf (x86-lap-label-frag lab) current
          (x86-lap-label-offset lab) (frag-list-position frag-list))
    (push lab (frag-labels current))
    lab))





(defstruct reloc
  type                                  ; a keyword
  arg                                   ; a label-operand or an expression, etc.
  frag                                  ; the (redundant) containing frag
  pos                                   ; octet position withing frag
  )




(defstruct (frag-list (:include ccl::dll-header)))

;;; ccl::dll-header-last is unit-time
(defun frag-list-current (frag-list)
  (ccl::dll-header-last frag-list))

;;; Add a new (empty) frag to the end of FRAG-LIST and make the new frag
;;; current
(defun new-frag (frag-list)
  (ccl::append-dll-node (make-frag) frag-list))

;;; Make a frag list, and make an empty frag be its current frag.
(defun make-frag-list ()
  (let* ((header (ccl::make-dll-header)))         
    (new-frag header)
    header))



;;; Finish the current frag, marking it as containing a PC-relative
;;; branch to the indicated label, with a one-byte opcode and
;;; one byte of displacement.
(defun finish-frag-for-branch (frag-list opcode label)
  (let* ((frag (frag-list-current frag-list)))
    (frag-push-byte frag opcode)
    (let* ((pos (frag-length frag))
           (reloc (make-reloc :type :branch8
                              :arg label
                              :pos pos)))
      (push reloc (frag-relocs frag))
      (frag-push-byte frag 0)
      (setf (frag-type frag) (list (if (eql opcode #xeb)
                                     :assumed-short-branch
                                     :assumed-short-conditional-branch)
                                   label
                                   pos
                                   reloc))
      (new-frag frag-list))))

;;; Mark the current frag as -ending- with an align directive.
;;; p2align is the power of 2 at which code in the next frag
;;; should be aligned.
;;; Start a new frag.
(defun finish-frag-for-align (frag-list p2align)
  (let* ((frag (frag-list-current frag-list)))
    (setf (frag-type frag) (list :align p2align))
    (new-frag frag-list)))

;;; Make the current frag be of type :talign; set that frag-type's
;;; argument to NIL initially.  Start a new frag of type :pending-talign;
;;; that frag will contain at most one instruction.  When an
;;; instuction is ouput in the pending-talign frag, adjust the preceding
;;; :talign frag's argument and set the type of the :pending-talign
;;; frag to NIL.  (The :talign frag will have 0-7 NOPs of some form
;;; appended to it, so the first instruction in the successor will end
;;; on an address that matches the argument below.)
;;; That instruction can not be a relaxable branch.
(defun finish-frag-for-talign (frag-list arg)
  (let* ((current (frag-list-current frag-list))
         (new (new-frag frag-list)))
    (setf (frag-type current) (list :talign nil))
    (setf (frag-type new) (list :pending-talign arg))))

;;; Having generated an instruction in a :pending-talign frag, set the
;;; frag-type argument of the preceding :talign frag to the :pendint-talign
;;; frag's argument - the length of the pending-talign's first instruction
;;; mod 8, and clear the type of the "pending" frag.
;;; cadr of the frag-type 
(defun finish-pending-talign-frag (frag-list)
  (let* ((frag (frag-list-current frag-list))
         (pred (frag-pred frag))
         (arg (cadr (frag-type frag)))
         (pred-arg (frag-type pred)))
    (setf (cadr pred-arg) (logand 7 (- arg (frag-length frag)))
          (frag-type frag) nil)
    (new-frag frag-list)))

(defun finish-frag-for-org (frag-list org)
  (let* ((frag (frag-list-current frag-list)))
    (setf (frag-type frag) (list :org org))
    (new-frag frag-list)))


(defun lookup-x86-register (regname designator)
  (let* ((registers (target-arch-case (:x8632 x86::*x8632-registers*)
				      (:x8664 x86::*x8664-registers*)))
	 (register-entries (target-arch-case (:x8632 x86::*x8632-register-entries*)
					     (:x8664 x86::*x8664-register-entries*)))
	 (r (typecase regname
              (symbol (or (gethash (string regname) registers)
                          (if (eq regname :rcontext)
                            (svref register-entries
                                   (ccl::backend-lisp-context-register *target-backend*)))
                          (and (boundp regname)
                               (let* ((val (symbol-value regname)))
                                 (and (typep val 'fixnum)
                                      (>= val 0)
                                      (< val (length register-entries))
                                      (svref register-entries val))))))
              (string (gethash regname registers))
              (fixnum (if (and (typep regname 'fixnum)
                                      (>= regname 0)
                                      (< regname (length register-entries)))
                        (svref register-entries regname))))))
                               
    (when r
      (if (eq designator :%)
        r
        (let* ((regtype (x86::reg-entry-reg-type r))
	       (oktypes (target-arch-case
			(:x8632 (x86::encode-operand-type :reg8 :reg16 :reg32))
			(:x8664 (x86::encode-operand-type :reg8 :reg16 :reg32 :reg64)))))
          (unless (logtest regtype oktypes)
            (error "Designator ~a can't be used with register ~a"
                   designator (x86::reg-entry-reg-name r)))
          (case designator
            (:%b (if (x86-byte-reg-p (x86::reg-entry-reg-name r))
		   (x86::x86-reg8 r)
		   (error "Designator ~a can't be used with register ~a"
			  designator (x86::reg-entry-reg-name r))))
            (:%w (x86::x86-reg16 r))
            (:%l (x86::x86-reg32 r))
            (:%q (x86::x86-reg64 r))))))))

(defun x86-register-ordinal-or-expression (form)
  (let* ((r (if (typep form 'symbol)
              (lookup-x86-register form :%))))
    (if r
      (target-arch-case (:x8632 (x86::reg-entry-ordinal32 r))
			(:x8664 (x86::reg-entry-ordinal64 r)))
      (multiple-value-bind (val condition)
          (ignore-errors (eval form))
        (if condition
          (error "Condition ~a signaled during assembly-time evalation of ~s."
                 condition form)
          val)))))

(defun x86-acc-reg-p (regname)
  (let ((r (lookup-x86-register regname :%)))
    (if r
      (logtest (x86::encode-operand-type :acc) (x86::reg-entry-reg-type r)))))

(defun x86-byte-reg-p (regname)
  (let ((r (lookup-x86-register regname :%)))
    (if r
      (target-arch-case
       (:x8632
	(or (<= (x86::reg-entry-reg-num r) x8632::ebx)
	    (member (x86::reg-entry-reg-name r) '("ah" "ch" "dh" "bh") :test #'string=)))
       (:x8664 t)))))
      
;;; It may seem strange to have an expression language in a lisp-based
;;; assembler, since lisp is itself a fairly reasonable expression
;;; language and EVAL is (in this context, at least) an adequate evaluation
;;; mechanism.  This may indeed be overkill, but there are reasons for
;;; wanting something beyond EVAL.
;;; This assumes that any expression that doesn't involve label addresses
;;; will always evaluate to the same value (in "the same" execution context).
;;; Expressions that do involve label references might only be evaluable
;;; after all labels are defined, and the value of such an expression may
;;; change (as label addresses are adjusted.)

;;; A "label address expression" looks like (:^ lab), syntactically.  Tree-walk
;;; FORM, and return T if it contains a label address expression.

(defun label-address-expression-p (form)
  (and (consp form)
       (eq (car form) :^)
       (consp (cdr form))
       (null (cddr form))))

(defun contains-label-address-expression (form)
  (cond ((label-address-expression-p form) t)
        ((typep form 'application-x86-lap-expression) t)
        ((atom form) nil)
        (t (dolist (sub (cdr form))
              (when (contains-label-address-expression sub)
                (return t))))))

(defstruct x86-lap-expression
  )


(defstruct (label-x86-lap-expression (:include x86-lap-expression))
  label)


;;; Represents a constant
(defstruct (constant-x86-lap-expression (:include x86-lap-expression))
  value)



;;; Also support 0, 1, 2, and many args, where at least one of those args
;;; is or contains a label reference.
(defstruct (application-x86-lap-expression (:include x86-lap-expression))
  operator)


(defstruct (unary-x86-lap-expression (:include application-x86-lap-expression))
  operand)


(defstruct (binary-x86-lap-expression (:include application-x86-lap-expression))
  operand0
  operand1)

(defstruct (n-ary-x86-lap-expression (:include application-x86-lap-expression))
  operands)

;;; Looks like a job for DEFMETHOD.
(defun x86-lap-expression-value (exp)
  (typecase exp
    (label-x86-lap-expression (- (x86-lap-label-address (label-x86-lap-expression-label exp)) *x86-lap-entry-offset*))
    (unary-x86-lap-expression (funcall (unary-x86-lap-expression-operator exp)
                                       (x86-lap-expression-value (unary-x86-lap-expression-operand exp))))
    (binary-x86-lap-expression (funcall (binary-x86-lap-expression-operator exp) 
                                        (x86-lap-expression-value (binary-x86-lap-expression-operand0 exp))
                                        (x86-lap-expression-value (binary-x86-lap-expression-operand1 exp))))
    (n-ary-x86-lap-expression (apply (n-ary-x86-lap-expression-operator exp)
                                     (mapcar #'x86-lap-expression-value (n-ary-x86-lap-expression-operands exp))))
    (constant-x86-lap-expression (constant-x86-lap-expression-value exp))
    (t exp)))

;;; Expression might contain unresolved labels.  Return nil if so (even
;;; if everything -could- be resolved.)
(defun early-x86-lap-expression-value (expression)
  (typecase expression
    (constant-x86-lap-expression (constant-x86-lap-expression-value expression))
    (x86-lap-expression nil)
    (t expression)))

(define-condition undefined-x86-lap-label (simple-program-error)
  ((label-name :initarg :label-name))
  (:report (lambda (c s)
             (format s "Label ~s was referenced but not defined."
                     (slot-value c 'label-name)))))

(defun x86-lap-label-address (lab)
  (let* ((frag (or (x86-lap-label-frag lab)
                   (error 'undefined-x86-lap-label :label-name (x86-lap-label-name lab)))))
    (+ (frag-address frag)
       (x86-lap-label-offset lab))))


(defun ensure-x86-lap-constant-label (val)
  (or (cdr (assoc val *x86-lap-constants*
                  :test #'eq))
      (let* ((label (make-x86-lap-label
                     (gensym)))
             (pair (cons val label)))
        (push pair *x86-lap-constants*)
        label)))

(defun parse-x86-lap-expression (form)
  (if (typep form 'x86-lap-expression)
    form
    (progn
      (when (quoted-form-p form)
        (let* ((val (cadr form)))
          (if (typep val 'fixnum)
	    (setq form (ash val (arch::target-fixnum-shift (backend-target-arch *target-backend*))))
            (let* ((constant-label (ensure-x86-lap-constant-label val )))
              (setq form `(:^ ,(x86-lap-label-name constant-label)))))))
      (if (null form)
        (setq form (arch::target-nil-value (backend-target-arch *target-backend*)))
        (if (eq form t)
          (setq form
                (+ (arch::target-nil-value (backend-target-arch *target-backend*))
                   (arch::target-t-offset  (backend-target-arch *target-backend*))))))
      
      (if (label-address-expression-p form)
        (make-label-x86-lap-expression :label (find-or-create-x86-lap-label (cadr form)))
        (if (contains-label-address-expression form)
          (destructuring-bind (op &rest args) form
            (case (length args)
              (1 (make-unary-x86-lap-expression :operator op :operand (parse-x86-lap-expression (car args))))
              (2 (make-binary-x86-lap-expression :operator op :operand0 (parse-x86-lap-expression (car args))
                                                 :operand1 (parse-x86-lap-expression (cadr args))))
              (t (make-n-ary-x86-lap-expression :operator op :operands (mapcar #'parse-x86-lap-expression args)))))
          (multiple-value-bind (value condition)
              (ignore-errors
                (eval (if (atom form)
                        form
                        (cons (car form)
                            (mapcar #'(lambda (x)
                                        (if (typep x 'constant-x86-lap-expression)
                                          (constant-x86-lap-expression-value
                                           x)
                                          x))
                                    (cdr form))))))
            (if condition
              (error "~a signaled during assembly-time evaluation of form ~s" condition form)
              value #|(make-constant-x86-lap-expression :value value)|#)))))))

(defun parse-x86-register-operand (regname designator)
  (let* ((r (lookup-x86-register regname designator)))
    (if r
      (x86::make-x86-register-operand :type (logandc2 (x86::reg-entry-reg-type r)
                                                      (x86::encode-operand-type :baseIndex))
                                 :entry r)
      (error "Unknown X86 register ~s" regname))))

(defun parse-x86-label-reference (name)
  (let* ((lab (find-or-create-x86-lap-label name)))
    (x86::make-x86-label-operand :type (x86::encode-operand-type :label)
                                 :label lab)))



(defun x86-register-designator (form)
  (when (and (consp form)
             (symbolp (car form)))
    (let* ((sym (car form)))
      (cond ((string= sym '%) :%)
            ((string= sym '%b) :%b)
            ((string= sym '%w) :%w)
            ((string= sym '%l) :%l)
            ((string= sym '%q) :%q)))))


;;; Syntax is:
;;; ([seg] [disp] [base] [index] [scale])
;;; A [seg] by itself isn't too meaningful; the same is true
;;; of a few other combinations.
(defun parse-x86-memory-operand (form)
  (flet ((register-operand-p (form)
           (let* ((designator (x86-register-designator form)))
             (when designator
               (destructuring-bind (regname) (cdr form)
                 (or (lookup-x86-register regname designator)
                     (error "Unknown register ~s" regname)))))))
  (let* ((seg nil)
         (disp nil)
         (base nil)
         (index nil)
         (scale nil))
    (do* ((f form (cdr f)))
         ((null f)
          (if (or disp base index)
            (progn
              ;;(check-base-and-index-regs instruction base index)
              (x86::make-x86-memory-operand 
               :type (if (or base index)
                       (if disp
                         (logior (optimize-displacement-type disp)
                                 (x86::encode-operand-type  :baseindex))
                         (x86::encode-operand-type :baseindex))
                       (optimize-displacement-type disp))
               :seg seg
               :disp disp
               :base base
               :index index
               :scale scale))
            (error "No displacement, base,  or index in ~s" form)))
      (let* ((head (car f))
             (r (register-operand-p head)))
        (if r
          (if (logtest (x86::reg-entry-reg-type r)
                       (x86::encode-operand-type :sreg2 :sreg3))
            ;; A segment register - if present - must be first
            (if (eq f form)
              (setq seg (svref x86::*x86-seg-entries* (x86::reg-entry-reg-num r))) 
              (error "Segment register ~s not valid in ~s" form))
            ;; Some other register.  Assume base if this is the
            ;; first gpr.  If we find only one gpr and a significant
            ;; scale factor, make that single gpr be the index.
            (if base
              (if index
                (error "Extra register ~s in memory address ~s" head form)
                (setq index r))
              (setq base r)))
          ;; Not a register, so head is either a displacement or
          ;; a scale factor.
          (if (and (null (cdr f))
                   (or disp base index))
            (let* ((exp (parse-x86-lap-expression head))
                   (val (if (or (typep exp 'constant-x86-lap-expression)
                                (not (x86-lap-expression-p exp)))
                          (x86-lap-expression-value exp))))
              (case val
                ((1 2 4 8)
                 (if (and base (not index))
                   (setq index base base nil))
                 (setq scale (1- (integer-length val))))
                (t
                 (error "Invalid scale factor ~s in ~s" head form))))
            (if (not (or disp base index))
              (setq disp (parse-x86-lap-expression head))
              (error "~& not expected in ~s" head form)))))))))

     
    

;;; Operand syntax:
;;; (% x) -> register
;;; ($ x) -> immediate
;;; (@ x) -> memory operand
;;; x -> labelref
(defun parse-x86-operand (form)
  (if (consp form)
    (let* ((head (car form))
           (designator nil))
      (if (symbolp head)
        (cond ((string= head '$)
               (destructuring-bind (immval) (cdr form)
                 (let* ((expr (parse-x86-lap-expression immval))
                        (val (early-x86-lap-expression-value expr))
                        (type (if val
                                (smallest-imm-type val)
                                (x86::encode-operand-type :imm32s))))
		   ;; special case
		   (when (eq val :self)
		     (setq type (x86::encode-operand-type :self)))
                   (x86::make-x86-immediate-operand :type type
                                             :value expr))))
              ((setq designator (x86-register-designator form))
               (destructuring-bind (reg) (cdr form)
                 (parse-x86-register-operand reg designator)))
              ((string= head '@)
               (parse-x86-memory-operand  (cdr form)))
              (t (error "unknown X86 operand: ~s" form)))
        (error "unknown X86 operand: ~s" form)))
    ;; Treat an atom as a label.
    (parse-x86-label-reference form)))




;;; Initialize some fields in the instruction from the template;
;;; set other fields (which depend on operand values) to NIL.
(defun set-x86-instruction-template (i template)
  (setf (x86::x86-instruction-opcode-template i) template
        (x86::x86-instruction-base-opcode i) (x86::x86-opcode-template-base-opcode template)
        (x86::x86-instruction-modrm-byte i) (x86::x86-opcode-template-modrm-byte template)
        (x86::x86-instruction-rex-prefix i) (target-arch-case
					     (:x8632 nil)
					     (:x8664
					      (x86::x86-opcode-template-rex-prefix template)))
        (x86::x86-instruction-sib-byte i) nil
        (x86::x86-instruction-seg-prefix i) nil
        (x86::x86-instruction-disp i) nil
        (x86::x86-instruction-imm i) nil
        (x86::x86-instruction-extra i) nil))


(defun init-x86-instruction (instruction template parsed-operands)
  (set-x86-instruction-template instruction template)
  (let* ((insert-classes (x86::x86-opcode-template-operand-classes template))
         (insert-functions x86::*x86-operand-insert-functions*))
    (dotimes (i (length parsed-operands) instruction)
      (funcall (svref insert-functions (svref insert-classes i))
               instruction
               (pop parsed-operands)))))



(defun smallest-imm-type (val)
  (if (eql val 1)
    (x86::encode-operand-type :Imm1 :Imm8 :Imm8S :Imm16 :Imm32 :Imm32S :Imm64)
    (typecase val
      ((signed-byte 8)
       (x86::encode-operand-type :Imm8S :imm8 :Imm16 :Imm32 :Imm32S :Imm64))
      ((unsigned-byte 8)
       (x86::encode-operand-type  :imm8 :Imm16 :Imm32 :Imm32S :Imm64))
      ((signed-byte 16)
       (x86::encode-operand-type  :Imm16 :Imm32 :Imm32S :Imm64))
      ((unsigned-byte 16)
       (x86::encode-operand-type  :Imm16 :Imm32 :Imm32S :Imm64))
      ((signed-byte 32)
       (x86::encode-operand-type :Imm32 :Imm32S :Imm64))
      ((unsigned-byte 32)
       (x86::encode-operand-type :Imm32 :Imm64))
      (t (x86::encode-operand-type :Imm64)))))

    
(defun x86-optimize-imm (operands suffix)
  (unless suffix
    ;; See if we can determine an implied suffix from operands.
    (do* ((i (1- (length operands)) (1- i)))
         ((< i 0))
      (declare (fixnum i))
      (let* ((op (svref operands i))
             (optype (x86::x86-operand-type op)))
        (when (logtest optype (x86::encode-operand-type :reg))
          (cond ((logtest optype (x86::encode-operand-type :reg8))
                 (setq suffix #\b))
                ((logtest optype (x86::encode-operand-type :reg16))
                 (setq suffix #\w))
                ((logtest optype (x86::encode-operand-type :reg32))
                 (setq suffix #\l))
                ((logtest optype (x86::encode-operand-type :reg64))
                 (setq suffix #\q)))
          (return)))))
  (dotimes (i (length operands))
    (let* ((op (svref operands i))
           (optype (x86::x86-operand-type op)))
      (when (logtest optype (x86::encode-operand-type :imm))
        (let* ((val (x86::x86-immediate-operand-value op)))
          (cond ((typep val 'constant-x86-lap-expression)
                 (case suffix
                   (#\l (setf (x86::x86-operand-type op)
                              (logior optype (x86::encode-operand-type
                                              :imm32 :imm64))))
                   (#\w (setf (x86::x86-operand-type op)
                              (logior optype (x86::encode-operand-type
                                              :imm16 :imm32S  :imm32 :imm64))))
                   (#\b (setf (x86::x86-operand-type op)
                              (logior optype (x86::encode-operand-type
                                              :imm8 :imm16 :imm32S  :imm32 :imm64)))))
                 (setf (x86::x86-operand-type op)
                       (logior (x86::x86-operand-type op)
                               (smallest-imm-type (x86-lap-expression-value val))))
                 (when (eql suffix #\q)
                   (setf (x86::x86-operand-type op)
                         (logandc2 (x86::x86-operand-type op)
                                   (x86::encode-operand-type :imm32)))))
                (t ; immediate value not constant
                 (case suffix
                   (#\q (setf (x86::x86-operand-type op)
                              (logior optype
                                      (x86::encode-operand-type :imm64 :imm32S))))
                   (#\l (setf (x86::x86-operand-type op)
                              (logior optype
                                      (x86::encode-operand-type :imm32))))
                   (#\w (setf (x86::x86-operand-type op)
                              (logior optype
                                      (x86::encode-operand-type :imm16))))
                   (#\b  (setf (x86::x86-operand-type op)
                              (logior optype
                                      (x86::encode-operand-type :imm8))))))))))))

(defun get-x86-opcode-templates (form)
  (let* ((name (string (car form))))
    (or
     (gethash name x86::*x86-opcode-template-lists*)
     ;; Try to determine a suffix, based on the size of the last
     ;; register argument (if any.)  If that can be determined,
     ;; tack it on to the end of NAME and try again.
     (let* ((suffix nil))
       (dolist (arg (cdr form))
         (let* ((designator (x86-register-designator arg)))
           (when designator
             (destructuring-bind (regname) (cdr arg)
               (let* ((reg (lookup-x86-register regname designator)))
                 (when reg
                   (let* ((type (x86::reg-entry-reg-type reg)))
                     (cond ((logtest type (x86::encode-operand-type :reg8))
                            (setq suffix #\b))
                           ((logtest type (x86::encode-operand-type :reg16))
                            (setq suffix #\w))
                           ((logtest type (x86::encode-operand-type :reg32))
                            (setq suffix #\l))
                           ((logtest type (x86::encode-operand-type :reg64))
                            (setq suffix #\q))))))))))
       (when suffix
         (let* ((n (length name))
                (m (1+ n))
                (s (make-string m)))
           (declare (fixnum n m) (dynamic-extent s))
           (dotimes (i n) (setf (schar s i) (char name i)))
           (setf (schar s n) suffix)
           (gethash s x86::*x86-opcode-template-lists*)))))))
         
                
         
     
  
;;; FORM is a list; its car doesn't name a macro or pseudo op.  If we
;;; can find a matching opcode template, initialize the
;;; x86-instruction with that template and these operands.
;;; Note that this doesn't handle "prefix" instructions at all.
;;; Things that would change the operand or address size are
;;; of limited utility, as are REP* prefixes on string instructions
;;; (because of the way that the lisp used %[E|R]DI and %[E|R]SI).
;;; LOCK can be used in the preceding instruction.
(defun parse-x86-instruction (form instruction)
    (let* ((templates (or
                       (get-x86-opcode-templates form)
                       (error "Unknown X86 instruction ~s" form)))
           (operands (cdr form)))
      (let* ((parsed-operands (if operands
                                (mapcar #'parse-x86-operand operands)))
             (operand-types (mapcar #'x86::x86-operand-type parsed-operands))
             (type0 (pop operand-types))
             (type1 (pop operand-types))
             (type2 (car operand-types)))

        ;; (x86-optimize-imm parsed-operands suffix)
        (dolist (template templates (error "Operands or suffix invalid in ~s" form))
          (when (x86::match-template-types template type0 type1 type2)
            (init-x86-instruction instruction template parsed-operands)
            ;(check-suffix instruction form)
            ;(x86-finalize-operand-types instruction)
            (return instruction))))))




              
;;; xxx - might want to omit disp64 when doing 32 bit code
(defun optimize-displacement-type (disp)
  (if disp
    (let* ((value (early-x86-lap-expression-value disp)))
      (if value
        (if (typep value '(signed-byte 8))
          (x86::encode-operand-type :disp8 :disp32 :disp32s :disp64)
          (if (typep value '(signed-byte 32))
            (x86::encode-operand-type :disp32s :disp64)
            (if (typep value '(unsigned-byte 32))
              (x86::encode-operand-type :disp32 :disp64)
              (x86::encode-operand-type :disp64))))
        (x86::encode-operand-type :disp32s :disp64)))
    0))

(defun optimize-displacements (operands)
  (dotimes (i (length operands))
    (let* ((op (svref operands i)))
      (when (typep op 'x86::x86-memory-operand)
        (let* ((disp (x86::x86-memory-operand-disp op))
               (val (if disp (early-x86-lap-expression-value disp))))
          (if (typep val '(signed-byte 32))
            (setf (x86::x86-operand-type op)
                  (logior (x86::x86-operand-type op) (x86::encode-operand-type :disp32s))))
          (if (typep val '(unsigned-byte 32))
            (setf (x86::x86-operand-type op)
                  (logior (x86::x86-operand-type op) (x86::encode-operand-type :disp32))))
          (if (and (logtest (x86::x86-operand-type op)
                            (x86::encode-operand-type :disp32 :disp32S :disp16))
                   (typep val '(signed-byte 8)))
            (setf (x86::x86-operand-type op)
                  (logior (x86::x86-operand-type op) (x86::encode-operand-type :disp8)))))))))

(defun x86-output-branch (frag-list insn)
  (dolist (b (x86::x86-opcode-template-prefixes
              (x86::x86-instruction-opcode-template insn)))
    (when (or (= b x86::+data-prefix-opcode+)
              (= b x86::+cs-prefix-opcode+)
              (= b x86::+ds-prefix-opcode+))
      (frag-list-push-byte frag-list b)))
  (finish-frag-for-branch frag-list
                          (x86::x86-instruction-base-opcode insn)
                          (x86::x86-instruction-extra insn)))

(defun x86-generate-instruction-code (frag-list insn)
  (let* ((template (x86::x86-instruction-opcode-template insn))
         (flags (x86::x86-opcode-template-flags template))
         (prefixes (x86::x86-opcode-template-prefixes template)))
    (let* ((explicit-seg-prefix (x86::x86-instruction-seg-prefix insn)))
      (when explicit-seg-prefix
        (push explicit-seg-prefix prefixes)))
    (cond
      ((logtest (x86::encode-opcode-flags :jump) flags)
       ;; a variable-length pc-relative branch, possibly preceded
       ;; by prefixes (used for branch prediction, mostly.)
       (x86-output-branch frag-list insn))
      (t
       (let* ((base-opcode (x86::x86-instruction-base-opcode insn)))
         (declare (fixnum base-opcode))
         (dolist (b prefixes)
           (frag-list-push-byte frag-list b))
         (let* ((rex-bits (logand #x8f
                                  (or (x86::x86-instruction-rex-prefix insn)
                                      0))))
           (declare (fixnum rex-bits))
           (unless (= 0 rex-bits)
             (frag-list-push-byte frag-list (logior #x40 (logand rex-bits #xf)))))
         (when (logtest base-opcode #xff00)
           (frag-list-push-byte frag-list (ldb (byte 8 8) base-opcode)))
         (frag-list-push-byte frag-list (ldb (byte 8 0) base-opcode)))
       (let* ((modrm (x86::x86-instruction-modrm-byte insn)))
         (when modrm
           (frag-list-push-byte frag-list modrm)
           (let* ((sib (x86::x86-instruction-sib-byte insn)))
             (when sib
               (frag-list-push-byte frag-list sib)))))
       (let* ((operands (x86::x86-opcode-template-operand-types template)))
         (if (and (= (length operands) 1)
                  (= (x86::encode-operand-type :label) (aref operands 0)))
           (let* ((label (x86::x86-instruction-extra insn))
                  (frag (frag-list-current frag-list))
                  (pos (frag-list-position frag-list)))
             (push (make-reloc :type :branch32
                               :arg label
                               :frag frag
                               :pos pos)
                   (frag-relocs frag))
             (frag-list-push-32 frag-list 0))
           (let* ((disp (x86::x86-instruction-disp insn)))
             (when disp
               (let* ((optype (x86::x86-instruction-extra insn))
                      (pcrel (and (logtest (x86::encode-operand-type :label) optype)
                              (typep disp 'label-x86-lap-expression)))
                  (val (unless pcrel (early-x86-lap-expression-value disp))))
             (if (null val)
               ;; We can do better job here, but (for now)
               ;; generate a 32-bit relocation
               (let* ((frag (frag-list-current frag-list))
                      (pos (frag-list-position frag-list)))
                 (push (make-reloc :type (if pcrel :branch32 :expr32)
                                   :arg (if pcrel (label-x86-lap-expression-label disp) disp)
                                   :frag frag
                                   :pos pos)
                       (frag-relocs frag))
                 (frag-list-push-32 frag-list 0))
               (if (logtest optype (x86::encode-operand-type :disp8))
                 (frag-list-push-byte frag-list (logand val #xff))
                 (if (logtest optype (x86::encode-operand-type :disp32 :disp32s))
                   (frag-list-push-32 frag-list val)
                   (frag-list-push-64 frag-list val)))))))))
       ;; Emit immediate operand(s).
       (let* ((op (x86::x86-instruction-imm insn)))
         (when op
           (let* ((optype (x86::x86-operand-type op))
                  (expr (x86::x86-immediate-operand-value op))
                  (val (early-x86-lap-expression-value expr)))
             (if (null val)
               (let* ((frag (frag-list-current frag-list))
                      (pos (frag-list-position frag-list))
                      (size 4)
                      (reloctype :expr32))
                 (when (logtest optype
                                (x86::encode-operand-type
                                 :imm8 :imm8S :imm16 :imm64))
                   (setq size 2 reloctype :expr16)
                   (if (logtest optype (x86::encode-operand-type
                                        :imm8 :imm8s))
                     (setq size 1 reloctype :expr8)
                     (if (logtest optype (x86::encode-operand-type :imm64))
                       (setq size 8 reloctype :expr64))))
                 (push (make-reloc :type reloctype
                                   :arg expr
                                   :frag frag
                                   :pos pos)
                       (frag-relocs frag))
                 (dotimes (b size)
                   (frag-list-push-byte frag-list 0)))
               (if (logtest optype (x86::encode-operand-type :imm8 :imm8s))
                 (frag-list-push-byte frag-list (logand val #xff))
                 (if (logtest optype (x86::encode-operand-type :imm16))
                   (frag-list-push-16 frag-list (logand val #xffff))
                   (if (logtest optype (x86::encode-operand-type :imm64))
                     (frag-list-push-64 frag-list val)
		     ;; magic value denoting function object's
		     ;; actual runtime address
		     (if (logtest optype (x86::encode-operand-type :self))
		       (let* ((frag (frag-list-current frag-list))
			      (pos (frag-list-position frag-list)))
			 (frag-list-push-32 frag-list 0)
			 (push (make-reloc :type :self
					   :arg 0
					   :frag frag
					   :pos pos)
			       (frag-relocs frag)))
		       (frag-list-push-32 frag-list val)))))))))))
    (let* ((frag (frag-list-current frag-list)))
      (if (eq (car (frag-type frag)) :pending-talign)
        (finish-pending-talign-frag frag-list)))))

;;; Returns the active frag list after processing directive(s).
(defun x86-lap-directive (frag-list directive arg &optional main-frag-list exception-frag-list)
  (declare (ignorable main-frag-list exception-frag-list))
  (case directive
    (:tra
     (finish-frag-for-align frag-list 3)
     (x86-lap-directive frag-list :long `(:^ ,arg))
     (emit-x86-lap-label frag-list arg))
    (:fixed-constants
     (dolist (constant arg)
       (ensure-x86-lap-constant-label constant)))
    (:arglist (setq *x86-lap-lfun-bits* (encode-lambda-list arg)))
    ((:uuo :uuo-section)
     (if exception-frag-list
       (progn
         (setq frag-list exception-frag-list)
         (finish-frag-for-align frag-list 2))))
    ((:main :main-section)
     (when main-frag-list (setq frag-list main-frag-list)))
    (:anchored-uuo-section
     (setq frag-list (x86-lap-directive frag-list :uuo-section nil main-frag-list exception-frag-list))
     (setq frag-list (x86-lap-directive frag-list :long `(:^ ,arg) main-frag-list exception-frag-list)))
    (t (let* ((exp (parse-x86-lap-expression arg))
              (constantp (or (constant-x86-lap-expression-p exp)
                             (not (x86-lap-expression-p exp)))))
         
         (if constantp
           (let* ((val (x86-lap-expression-value exp)))
             (ecase directive
               (:code-size
                (if *x86-lap-fixed-code-words*
                  (error "Duplicate :CODE-SIZE directive")
                  (setq *x86-lap-fixed-code-words* val)))
               (:byte (frag-list-push-byte frag-list val))
               (:short (frag-list-push-16 frag-list val))
               (:long (frag-list-push-32 frag-list val))
               (:quad (frag-list-push-64 frag-list val))
               (:align (finish-frag-for-align frag-list val))
               (:talign (finish-frag-for-talign frag-list val))
               (:org (finish-frag-for-org frag-list val))))
           (let* ((pos (frag-list-position frag-list))
                  (frag (frag-list-current frag-list))
                  (reloctype nil))
             (ecase directive
               (:byte (frag-list-push-byte frag-list 0)
                      (setq reloctype :expr8))
               (:short (frag-list-push-16 frag-list 0)
                       (setq reloctype :expr16))
               (:long (frag-list-push-32 frag-list 0)
                      (setq reloctype :expr32))
               (:quad (frag-list-push-64 frag-list 0)
                      (setq reloctype :expr64))
               (:align (error ":align expression ~s not constant" arg))
               (:talign (error ":talign expression ~s not constant" arg)))
             (when reloctype
               (push
                (make-reloc :type reloctype
                            :arg exp
                            :pos pos
                            :frag frag)
                (frag-relocs frag))))))))
  frag-list)


(defun x862-lap-process-regsave-info (frag-list regsave-label regsave-mask regsave-addr)
  (when regsave-label
    (let* ((label-diff (min (- (x86-lap-label-address regsave-label)
                               *x86-lap-entry-offset*)
                            255))
           (first-frag (frag-list-succ frag-list)))
      (setf (frag-ref first-frag 4) label-diff
            (frag-ref first-frag 5) regsave-addr
            (frag-ref first-frag 6) regsave-mask))
    t))
                       
         

(defun x86-lap-form (form frag-list instruction  main-frag-list exception-frag-list)
  (if (and form (symbolp form))
    (emit-x86-lap-label frag-list form)
    (if (or (atom form) (not (symbolp (car form))))
      (error "Unknown X86-LAP form ~s ." form)
      (multiple-value-bind (expansion expanded)
          (x86-lap-macroexpand-1 form)
        (if expanded
          (x86-lap-form expansion frag-list instruction main-frag-list exception-frag-list)
          (if (typep (car form) 'keyword)
            (destructuring-bind (op &optional arg) form
              (setq frag-list (x86-lap-directive frag-list op arg main-frag-list exception-frag-list)))
            (case (car form)
              (progn
                (dolist (f (cdr form))
                  (setq frag-list (x86-lap-form f frag-list instruction main-frag-list exception-frag-list))))
              (let
                  (destructuring-bind (equates &body body)
                      (cdr form)
                    (setq frag-list (x86-lap-equate-form equates frag-list instruction body main-frag-list exception-frag-list))))
              (t
               (parse-x86-instruction form instruction)
               (x86-generate-instruction-code frag-list instruction))))))))
  frag-list)

(defun relax-align (address bits)
  (let* ((mask (1- (ash 1 bits))))
    (- (logandc2 (+ address mask) mask) address)))

(defun relax-talign (address mask)
  (do* ((i 0 (1+ i)))
       ((= (logand address 7) mask) i)
    (incf address)))


(defun relax-frag-list (frag-list)
  ;; First, assign tentative addresses to all frags, assuming that
  ;; span-dependent instructions have short displacements.
  ;; While doing that, find branches to the next instruction and
  ;; remove them.  In some cases, that'll cause the containing
  ;; frag to become empty; that could introduce branches to the
  ;; next instruction, so we repeat this process until we can
  ;; make it all the way through the frag-list.
  (loop
    (let* ((address (target-arch-case (:x8632 4) (:x8664 8)))) ;after header
      (declare (fixnum address))
      (when (do-dll-nodes (frag frag-list t)
              (setf (frag-address frag) address)
              (incf address (frag-length frag))
              (case (car (frag-type frag))
                (:org
                 ;; Do nothing, for now
                 )
                (:align
                 (incf address (relax-align address (cadr (frag-type frag)))))
                (:talign
                 (let* ((arg (cadr (frag-type frag))))
                   (if (null arg)
                     ;;; Never generated code in :pending-talign frag
                     (setf (frag-type frag) nil)
                     (incf address (relax-talign address arg)))))
                ((:assumed-short-branch :assumed-short-conditional-branch)
                 (destructuring-bind (label pos reloc) (cdr (frag-type frag))
                   (let* ((next (frag-succ frag)))
                     (when (and (eq (x86-lap-label-frag label) next)
                                (eql (x86-lap-label-offset label) 0))
                       ;; Delete the reloc associated with this branch.
                       (setf (frag-relocs frag)
                             (delete reloc (frag-relocs frag)))
                       ;; This will be a "normal" frag
                       (setf (frag-type frag) nil)
                       ;; Remove the (short) branch, and remove the frag
                       ;; if it becomes empty.  If the frag does become
                       ;; empty, migrate any labels to the next frag.
                       (when (zerop (setf (frag-length frag)
                                        (1- pos)))

                         (do* ((labels (frag-labels frag)))
                              ((null labels))
                           (let* ((lab (pop labels)))
                             (setf (x86-lap-label-frag lab) next
                                   (x86-lap-label-offset lab) 0)
                             (push lab (frag-labels next))))
                         (remove-dll-node frag))
                       (return nil)))))))
        (return))))
  ;; Repeatedly "stretch" frags containing span-dependent instructions
  ;; until nothing's stretched.  It may take several iterations to
  ;; converge; is convergence guaranteed ?
  (loop
    (let* ((stretch 0)                  ;cumulative growth in frag sizes
           (stretched nil))             ;any change on this pass ?
      (do-dll-nodes (frag frag-list)
        (let* ((growth 0)
               (fragtype (frag-type frag))
               (was-address (frag-address frag))
               (address (incf (frag-address frag) stretch)))
          (case (car fragtype)
            (:org
             (let* ((target (cadr (frag-type frag)))
                    (next-address (frag-address (frag-succ frag))))
               (setq growth (- target next-address))
               (if (< growth 0)
                 (error "Code size exceeds :CODE-SIZE constraint ~s"
                        (ash target -3))
                 (decf growth stretch))))
            (:align
             (let* ((bits (cadr fragtype))
                    (len (frag-length frag))
                    (oldoff (relax-align (+ was-address len) bits))
                    (newoff (relax-align (+ address len) bits)))
               (setq growth (- newoff oldoff))))
            (:talign
             (let* ((arg (cadr fragtype))
                    (len (frag-length frag))
                    (oldoff (relax-talign (+ was-address len) arg))
                    (newoff (relax-talign (+ address len) arg)))
               (setq growth (- newoff oldoff))))
            ;; If we discover - on any iteration - that a short
            ;; branch doesn't fit, we change the type (and the reloc)
            ;; destructively to a wide branch indicator and will
            ;; never change our minds about that, so we only have
            ;; to look here at conditional branches that may still
            ;; be able to use a 1-byte displacement.
            ((:assumed-short-branch :assumed-short-conditional-branch)
             (destructuring-bind (label pos reloc) (cdr (frag-type frag))
               (declare (fixnum pos))
               (let* ((label-address (x86-lap-label-address label))
                      (branch-pos (+ address (1+ pos)))
                      (diff (- label-address branch-pos)))
                 (unless (typep diff '(signed-byte 8))
                   (cond ((eq (car fragtype) :assumed-short-branch)
                          ;; replace the opcode byte
                          (setf (frag-ref frag (the fixnum (1- pos)))
                                x86::+jump-pc-relative+)
                          (frag-push-byte frag 0)
                          (frag-push-byte frag 0)
                          (frag-push-byte frag 0)
                          (setf (reloc-type reloc) :branch32)
                          (setf (car fragtype) :long-branch)
                          (setq growth 3))
                         (t
                          ;; Conditional branch: must change
                          ;; 1-byte opcode to 2 bytes, add 4-byte
                          ;; displacement
                          (let* ((old-opcode (frag-ref frag (1- pos))))
                            (setf (frag-ref frag (1- pos)) #x0f
                                  (frag-ref frag pos) (+ old-opcode #x10))
                            (frag-push-byte frag 0)
                            (frag-push-byte frag 0)
                            (frag-push-byte frag 0)
                            (frag-push-byte frag 0)
                            (setf (reloc-type reloc) :branch32
                                  (reloc-pos reloc) (1+ pos))
                            (setf (car fragtype) :long-conditional-branch
                                  (caddr fragtype) (1+ pos))
                            (setq growth 4)))))))))
          (unless (eql 0 growth)
            (incf stretch growth)
            (setq stretched t))))
      (unless stretched (return)))))

(defun apply-relocs (frag-list)
  (flet ((emit-byte (frag pos b)
           (setf (frag-ref frag pos) (logand b #xff))))
    (flet ((emit-short (frag pos s)
             (setf (frag-ref frag pos) (ldb (byte 8 0) s)
                   (frag-ref frag (1+ pos)) (ldb (byte 8 8) s))))
      (flet ((emit-long (frag pos l)
               (emit-short frag pos (ldb (byte 16 0) l))
               (emit-short frag (+ pos 2) (ldb (byte 16 16) l))))
        (flet ((emit-quad (frag pos q)
                 (emit-long frag pos (ldb (byte 32 0) q))
                 (emit-long frag (+ pos 4) (ldb (byte 32 32) q))))
          (do-dll-nodes (frag frag-list)
            (let* ((address (frag-address frag)))
              (dolist (reloc (frag-relocs frag))
                (let* ((pos (reloc-pos reloc))
                       (arg (reloc-arg reloc)))
                  (ecase (reloc-type reloc)
                    (:branch8 (let* ((target (x86-lap-label-address arg))
                                     (refpos (+ address (1+ pos))))
                                (emit-byte frag pos (- target refpos))))
                    (:branch32 (let* ((target (x86-lap-label-address arg))
                                     (refpos (+ address pos 4)))
                                (emit-long frag pos (- target refpos))))
                    (:expr8 (emit-byte frag pos  (x86-lap-expression-value arg)))
                    (:expr16 (emit-short frag pos (x86-lap-expression-value arg)))
                    (:expr32 (emit-long frag pos (x86-lap-expression-value arg)))
                    (:expr64 (emit-quad frag pos (x86-lap-expression-value arg)))
		    (:self (emit-long frag pos (x86-lap-expression-value arg)))))))))))))

(defun frag-emit-nops (frag count)
  (let* ((nnops (ash (+ count 3) -2))
         (len (floor count nnops))
         (remains (- count (* nnops len))))
    (dotimes (i remains)
      (dotimes (k len) (frag-push-byte frag #x66))
      (frag-push-byte frag #x90))
    (do* ((i remains (1+ i)))
         ((= i nnops))
      (dotimes (k (1- len)) (frag-push-byte frag #x66))
      (frag-push-byte frag #x90))))
  
(defun fill-for-alignment (frag-list)
  (ccl::do-dll-nodes (frag frag-list)
    (let* ((next (ccl::dll-node-succ frag)))
      (unless (eq next frag-list)
        (let* ((addr (frag-address frag))
               (nextaddr (frag-address next))
               (pad (- nextaddr (+ addr (frag-length frag)))))
          (unless (eql 0 pad)
            (frag-emit-nops frag pad)))))))

(defun show-frag-bytes (frag-list)
  (ccl::do-dll-nodes (frag frag-list)
    (format t "~& frag at #x~x" (frag-address frag))
    (dotimes (i (frag-length frag))
      (unless (logtest 15 i)
        (format t "~&"))
      (format t "~2,'0x " (frag-ref frag i)))))

(defun x86-lap-equate-form (eqlist fraglist instruction  body main-frag exception-frag) 
  (let* ((symbols (mapcar #'(lambda (x)
                              (let* ((name (car x)))
                                (or
                                 (and name 
                                      (symbolp name)
                                      (not (constant-symbol-p name))
                                      (or (not (gethash (string name)
							(target-arch-case
							 (:x8632 x86::*x8632-registers*)
							 (:x8664 x86::*x8664-registers*))))
                                          (error "Symbol ~s already names an x86 register" name))
                                      name)
                                 (error 
                                  "~S is not a bindable symbol name ." name))))
                          eqlist))
         (values (mapcar #'(lambda (x) (x86-register-ordinal-or-expression
                                        (cadr x)))
                         eqlist)))
    (progv symbols values
      (dolist (form body fraglist)
        (setq fraglist (x86-lap-form form fraglist instruction main-frag exception-frag))))))
                
(defun cross-create-x86-function (name frag-list constants bits debug-info)
  (let* ((constants-vector (%alloc-misc (+ (length constants)
                                           (+ 2
                                              (if name 1 0)
                                              (if debug-info 1 0)))
                                        target::subtag-xfunction)))
    (unless name (setq bits (logior bits (ash -1 $lfbits-noname-bit))))
    (let* ((last (1- (uvsize constants-vector))))
      (declare (fixnum last))
      (setf (uvref constants-vector last) bits)
      (when name
        (setf (uvref constants-vector (decf last)) name))
      (when debug-info
        (setf (uvref constants-vector (decf last)) debug-info))
      (dolist (c constants)
        (setf (uvref constants-vector (decf last)) (car c)))
      (let* ((nbytes 0))
        (do-dll-nodes (frag frag-list)
          (incf nbytes (frag-length frag)))
	#+x8632-target
	(when (>= nbytes (ash 1 18)) (compiler-function-overflow))
        (let* ((code-vector (make-array nbytes
                                        :element-type '(unsigned-byte 8)))
               (target-offset 0))
          (declare (fixnum target-offset))
          (setf (uvref constants-vector 0) code-vector)
          (do-dll-nodes (frag frag-list)
            (incf target-offset (frag-output-bytes frag code-vector target-offset)))
          constants-vector)))))

#+x86-target
(defun create-x86-function (name frag-list constants bits debug-info)
  (unless name (setq bits (logior bits (ash -1 $lfbits-noname-bit))))
  (let* ((code-bytes (let* ((nbytes 0))
                       (do-dll-nodes (frag frag-list nbytes)
                         (incf nbytes (frag-length frag)))))
         (code-words (ash code-bytes (- target::word-shift)))
         (function-vector (allocate-typed-vector :function code-words)))
    (declare (fixnum code-bytes code-words))
    (let* ((target-offset 0))
      (declare (fixnum target-offset))
      (do-dll-nodes (frag frag-list)
        (incf target-offset (frag-output-bytes frag function-vector target-offset))))
    (let* ((last (1- (uvsize function-vector))))
      (declare (fixnum last))
      (setf (uvref function-vector last) bits)
      (when name
        (setf (uvref function-vector (decf last)) name))
      (when debug-info
        (setf (uvref function-vector (decf last)) debug-info))
      (dolist (c constants)
        (setf (uvref function-vector (decf last)) (car c)))
      #+x8632-target
      (%update-self-references function-vector)
      (function-vector-to-function function-vector))))

(defun %define-x86-lap-function (name forms &optional (bits 0))
  (target-arch-case
   (:x8632
    (%define-x8632-lap-function name forms bits))
   (:x8664
    (%define-x8664-lap-function name forms bits))))

(defun %define-x8664-lap-function (name forms &optional (bits 0))
  (let* ((*x86-lap-labels* ())
         (*x86-lap-constants* ())
	 (*x86-lap-entry-offset* x8664::fulltag-function)
         (*x86-lap-fixed-code-words* nil)
         (*x86-lap-lfun-bits* bits)
         (end-code-tag (gensym))
         (entry-code-tag (gensym))
         (instruction (x86::make-x86-instruction))
         (main-frag-list (make-frag-list))
         (exception-frag-list (make-frag-list))
         (frag-list main-frag-list))
    (make-x86-lap-label end-code-tag)
    (make-x86-lap-label entry-code-tag)
    (x86-lap-directive frag-list :long `(ash (+ (- (:^ ,end-code-tag ) 8)
                                              *x86-lap-entry-offset*) -3))
    (x86-lap-directive frag-list :byte 0) ;regsave pc
    (x86-lap-directive frag-list :byte 0) ;regsave ea
    (x86-lap-directive frag-list :byte 0) ;regsave mask
    (emit-x86-lap-label frag-list entry-code-tag)

    (x86-lap-form `(lea (@ (:^ ,entry-code-tag) (% rip)) (% fn)) frag-list instruction main-frag-list exception-frag-list)
    (dolist (f forms)
      (setq frag-list (x86-lap-form f frag-list instruction main-frag-list exception-frag-list)))
    (setq frag-list main-frag-list)
    (merge-dll-nodes frag-list exception-frag-list)
    (x86-lap-directive frag-list :align 3)
    (when *x86-lap-fixed-code-words*
      (x86-lap-directive frag-list :org (ash *x86-lap-fixed-code-words* 3)))
    (x86-lap-directive frag-list :quad x8664::function-boundary-marker)
    (emit-x86-lap-label frag-list end-code-tag)
    (dolist (c (reverse *x86-lap-constants*))
      (emit-x86-lap-label frag-list (x86-lap-label-name (cdr c)))
      (x86-lap-directive frag-list :quad 0))
    (when name
      (x86-lap-directive frag-list :quad 0))
    ;; room for lfun-bits
    (x86-lap-directive frag-list :quad 0)
    (relax-frag-list frag-list)
    (apply-relocs frag-list)
    (fill-for-alignment frag-list)
    ;;(show-frag-bytes frag-list)
    (funcall #-x86-target #'cross-create-x86-function
             #+x86-target (if (eq *target-backend* *host-backend*)
                            #'create-x86-function
                            #'cross-create-x86-function)
             name frag-list *x86-lap-constants* *x86-lap-lfun-bits* nil)))

(defun %define-x8632-lap-function (name forms &optional (bits 0))
  (let* ((*x86-lap-labels* ())
         (*x86-lap-constants* ())
	 (*x86-lap-entry-offset* x8632::fulltag-misc)
         (*x86-lap-fixed-code-words* nil)
         (*x86-lap-lfun-bits* bits)
	 (srt-tag (gensym))
         (end-code-tag (gensym))
         (entry-code-tag (gensym))
         (instruction (x86::make-x86-instruction))
         (main-frag-list (make-frag-list))
         (exception-frag-list (make-frag-list))
         (frag-list main-frag-list))
    (make-x86-lap-label entry-code-tag)
    (make-x86-lap-label srt-tag)
    (make-x86-lap-label end-code-tag)
    ;; count of 32-bit words from header to function boundary
    ;; marker, inclusive.
    (x86-lap-directive frag-list :short `(ash (+ (- (:^ ,end-code-tag) 4)
						 *x86-lap-entry-offset*) -2))
    (emit-x86-lap-label frag-list entry-code-tag)
    (x86-lap-form '(movl ($ :self) (% x8632::fn)) frag-list instruction main-frag-list exception-frag-list)
    (dolist (f forms)
      (x86-lap-form f frag-list instruction main-frag-list exception-frag-list))
    (x86-lap-directive frag-list :align 2)
    (when *x86-lap-fixed-code-words*
      ;; We have a code-size that we're trying to get to.  We need to
      ;; include the self-reference table in the code-size, so decrement
      ;; the size of the padding we would otherwise insert by the srt size.
      (let ((srt-words 1))		;for zero between end of code and srt
	(do-dll-nodes (frag frag-list)
	  (dolist (reloc (frag-relocs frag))
	    (when (eq (reloc-type reloc) :self)
	      (incf srt-words))))
	(decf *x86-lap-fixed-code-words* srt-words)
	(if (plusp *x86-lap-fixed-code-words*)
	  (x86-lap-directive frag-list :org (ash *x86-lap-fixed-code-words* 2)))))
    ;; self reference table
    (x86-lap-directive frag-list :long 0)
    (emit-x86-lap-label frag-list srt-tag)
    ;; reserve space for self-reference offsets
    (do-dll-nodes (frag frag-list)
      (dolist (reloc (frag-relocs frag))
	(when (eq (reloc-type reloc) :self)
	  (x86-lap-directive frag-list :long 0))))
    (x86-lap-directive frag-list :long x8632::function-boundary-marker)
    (emit-x86-lap-label frag-list end-code-tag)
    (dolist (c (reverse *x86-lap-constants*))
      (emit-x86-lap-label frag-list (x86-lap-label-name (cdr c)))
      (x86-lap-directive frag-list :long 0))
    (when name
      (x86-lap-directive frag-list :long 0))
    ;; room for lfun-bits
    (x86-lap-directive frag-list :long 0)
    (relax-frag-list frag-list)
    (apply-relocs frag-list)
    (fill-for-alignment frag-list)
    ;; determine start of self-reference-table
    (let* ((label (find srt-tag *x86-lap-labels* :test #'eq
						 :key #'x86-lap-label-name))
	   (srt-frag (x86-lap-label-frag label))
	   (srt-index (x86-lap-label-offset label)))
      ;; fill in self-reference offsets
      (do-dll-nodes (frag frag-list)
	(dolist (reloc (frag-relocs frag))
	  (when (eq (reloc-type reloc) :self)
	    (setf (frag-ref-32 srt-frag srt-index)
		  (+ (frag-address frag) (reloc-pos reloc)))
	    (incf srt-index 4)))))
    ;;(show-frag-bytes frag-list)
    (funcall #-x8632-target #'cross-create-x86-function
             #+x8632-target (if (eq *target-backend* *host-backend*)
			      #'create-x86-function
			      #'cross-create-x86-function)
             name frag-list *x86-lap-constants* *x86-lap-lfun-bits* nil)))

(defmacro defx86lapfunction (&environment env name arglist &body body
                             &aux doc)
  (if (not (endp body))
      (and (stringp (car body))
           (cdr body)
           (setq doc (car body))
           (setq body (cdr body))))
  `(progn
     (eval-when (:compile-toplevel)
       (note-function-info ',name t ,env))
     #-x8664-target
     (progn
       (eval-when (:load-toplevel)
         (%defun (nfunction ,name (lambda (&lap 0) (x86-lap-function ,name ,arglist ,@body))) ,doc))
       (eval-when (:execute)
         (%define-x86-lap-function ',name '((let ,arglist ,@body)))))
     #+x8664-target	; just shorthand for defun
     (%defun (nfunction ,name (lambda (&lap 0) (x86-lap-function ,name ,arglist ,@body))) ,doc)))

(defmacro defx8632lapfunction (&environment env name arglist &body body
                             &aux doc)
  (if (not (endp body))
      (and (stringp (car body))
           (cdr body)
           (setq doc (car body))
           (setq body (cdr body))))
  `(progn
     (eval-when (:compile-toplevel)
       (note-function-info ',name t ,env))
     #-x8632-target
     (progn
       (eval-when (:load-toplevel)
         (%defun (nfunction ,name (lambda (&lap 0) (x86-lap-function ,name ,arglist ,@body))) ,doc))
       (eval-when (:execute)
         (%define-x8632-lap-function ',name '((let ,arglist ,@body)))))
     #+x8632-target
     (%defun (nfunction ,name (lambda (&lap 0) (x86-lap-function ,name ,arglist ,@body))) ,doc)))
