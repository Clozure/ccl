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


(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (require "DLL-NODE")
  (require "BACKEND"))


(cl:in-package "CCL")

;;; Specifying the same name for a result and an argument basically
;;; says that it's ok for the vinsn to clobber that argument.  (In all
;;; other cases, arguments are assumed to be "read-only", and damned
;;; well better be.)  Any results that are also arguments must follow
;;; all results that aren't in the "results" list; any arguments that
;;; are also results must precede all arguments that aren't in the
;;; "arguments" list, and all hybrids must appear in the same order in
;;; both lists. This is what "nhybrids" is about (and why it defaults
;;; to 0 ...)  Sometimes (often) these hybrid "results" aren't very
;;; interesting as results;;; it might be clearer to consider
;;; "mutable" arguments as quasi-temporaries.
(defstruct vinsn-template
  name                                  ; a symbol in the target package
  result-vreg-specs                     ; one or more vreg specs for values defined by the vinsn
  argument-vreg-specs                   ; may ultimately overlap some result vreg(s)
  ; one or more vreg specs for temporaries used in vinsn.
  ; all such temporaries are assumed to have lifetimes which span all
  ; machine instructions in the vinsn (e.g., they can't conflict with any
  ; registers used for args/results and may have further constraints.
  temp-vreg-specs                  
  local-labels
  body                                  ; list of target instructions, local labels
  (nhybrids 0)
  (nvp 0)
  results&args                          ;
  (attributes 0)                        ; attribute bitmask
  opcode-alist                          ; ((number1 . name1) (number2 . name2) ...)
)

(defmethod make-load-form ((v vinsn-template) &optional env)
  (make-load-form-saving-slots v :environment env))


(defun get-vinsn-template-cell (name templates)
  (let* ((n (intern (string name) *ccl-package*)))
    (or (gethash n templates)
        (setf (gethash n templates) (cons n nil)))))

(defun need-vinsn-template (name templates)
  (or (cdr (if (consp name) name (get-vinsn-template-cell name templates)))
      (error "Unknown vinsn: ~s" name)))

(defun set-vinsn-template (name template templates)
  (setf (cdr (get-vinsn-template-cell name templates)) template))

(defstruct (vinsn (:include dll-node)
                  (:print-function print-vinsn)
                  (:constructor %make-vinsn (template)))
  template                              ; The vinsn-template of which this is an instance
  variable-parts                        ; vector of result-vregs, arguments, temps, local-labels
  annotation
  (gprs-set 0)
  (fprs-set 0)
)

(def-standard-initial-binding *vinsn-freelist* (make-dll-node-freelist))

(defun make-vinsn (template)
  (let* ((vinsn (alloc-dll-node *vinsn-freelist*)))
    (loop
      ; Sometimes, the compiler seems to return its node list
      ; to the freelist without first removing the vinsn-labels in it.
      (if (or (null vinsn) (typep vinsn 'vinsn)) (return))
      (setq vinsn (alloc-dll-node *vinsn-freelist*)))
    (if vinsn
      (progn
        (setf (vinsn-template vinsn) template
              (vinsn-variable-parts vinsn) nil
              (vinsn-annotation vinsn) nil
	      (vinsn-gprs-set vinsn) 0
	      (vinsn-fprs-set vinsn) 0)
        vinsn)
      (%make-vinsn template))))

(eval-when (:load-toplevel :execute)
(defstruct (vinsn-label (:include dll-node)
                        (:print-function print-vinsn-label)
                        (:predicate %vinsn-label-p)
                        (:constructor %make-vinsn-label (id)))
  id
  refs                                  ; vinsns in which this label appears as an operand
  info                                  ; code-generation stuff
)
)

(def-standard-initial-binding *vinsn-label-freelist* (make-dll-node-freelist))

(defun make-vinsn-label (id)
  (let* ((lab (alloc-dll-node *vinsn-label-freelist*)))
    (if lab
      (progn
        (setf (vinsn-label-id lab) id
              (vinsn-label-refs lab) nil
              (vinsn-label-info lab) nil)
        lab)
      (%make-vinsn-label id))))

; "Real" labels have fixnum IDs.
(defun vinsn-label-p (l)
  (if (%vinsn-label-p l) 
    (typep (vinsn-label-id l) 'fixnum)))


(defun print-vinsn-label (l s d)
  (declare (ignore d))
  (print-unreadable-object (l s :type t)
    (format s "~d" (vinsn-label-id l))))

;;; Notes are attached to (some) vinsns.  They're used to attach
;;; semantic information to an execution point.  The vinsn
;;; points to the note via its LABEL-ID; the note has a backpointer to
;;; the vinsn.

(defstruct (vinsn-note
            (:constructor %make-vinsn-note)
            (:print-function print-vinsn-note))
  (label (make-vinsn-label nil))
  (peer nil :type (or null vinsn-note))
  (class nil)
  (info nil :type (or null simple-vector)))


(defun print-vinsn-note (n s d)
  (declare (ignore d))
  (print-unreadable-object (n s :type t)
    (format s "~d" (vinsn-note-class n))
    (let* ((info (vinsn-note-info n)))
      (when info (format s " / ~S" info)))))
  
(defun make-vinsn-note (class info)
  (let* ((n (%make-vinsn-note :class class :info (if info (apply #'vector info))))
         (lab (vinsn-note-label n)))
    (setf (vinsn-label-id lab) n)
    n))

(defun close-vinsn-note (n)
  (let* ((end (%make-vinsn-note :peer n)))
    (setf (vinsn-label-id (vinsn-note-label end)) end
          (vinsn-note-peer end) n
          (vinsn-note-peer n) end)
    end))
        

(defun vinsn-vreg-description (value spec)
  (case (cadr spec)
    ((:u32 :s32 :u16 :s16 :u8 :s8 :lisp :address :imm)
     (let* ((mode (if (typep value 'fixnum)
                    (get-regspec-mode value))))
       (if (and mode (not (eql 0 mode)))
         (list (hard-regspec-value value)
               (car (rassoc mode *mode-name-value-alist* :test #'eq)))
         value)))
    (t value)))

(defun collect-vinsn-variable-parts (v start n &optional specs)
  (declare (fixnum start n))
  (let* ((varparts (vinsn-variable-parts v)))
    (when varparts
      (let* ((head (cons nil nil))
	     (tail head))
	(declare (dynamic-extent head) (cons head tail))
	(do* ((j start (1+ j))
              (i 0 (1+ i)))
             ((= i n) (cdr head))
          (declare (fixnum i j))
          (setq tail (cdr (rplacd tail (cons (vinsn-vreg-description (svref varparts j) (pop specs)) nil)))))))))

      
(defun collect-vinsn-results (v)
  (let* ((template (vinsn-template v))
         (result-specs (vinsn-template-result-vreg-specs template)))
    (collect-vinsn-variable-parts v 0 (length result-specs) result-specs)))

(defun collect-vinsn-arguments (v)
  (let* ((template (vinsn-template v))
         (arg-specs (vinsn-template-argument-vreg-specs template)))
    (collect-vinsn-variable-parts v
                                  (- (length (vinsn-template-result-vreg-specs template)) 
                                     (vinsn-template-nhybrids template))
                                  (length arg-specs)
                                  arg-specs)))

(defun collect-vinsn-temps (v)
  (let* ((template (vinsn-template v)))
    (collect-vinsn-variable-parts v 
                                  (+
                                   (length (vinsn-template-result-vreg-specs template)) 
                                   (length (vinsn-template-argument-vreg-specs template)))
                                  (length (vinsn-template-temp-vreg-specs template)))))

(defun template-infix-p (template)
  (declare (ignore template))
  nil)

(defun print-vinsn (v stream d)
  (declare (ignore d))
  (let* ((template (vinsn-template v))
         (results (collect-vinsn-results v))
         (args (collect-vinsn-arguments v))
         (opsym (if (cdr results) :== :=))
         (infix (and (= (length args) 2) (template-infix-p template)))
         (opname (vinsn-template-name template)))
    (print-unreadable-object (v stream)
      (if results (format stream "~A ~S " (if (cdr results) results (car results)) opsym))
      (if infix
        (format stream "~A ~A ~A" (car args) opname (cadr args))
        (format stream "~A~{ ~A~}" opname args))
      (let* ((annotation (vinsn-annotation v)))
	(when annotation
	  (format stream " ||~a|| " annotation))))))
  
(defparameter *known-vinsn-attributes*
  '(
    :jump				; an unconditional branch
    :branch				; a conditional branch
    :call				; a jump that returns
    :funcall				; A full function call, assumed to bash all volatile registers
    :subprim-call			; A subprimitive call; bashes some volatile registers
    :jumpLR				; Jumps to the LR, possibly stopping off at a function along the way.
    :lrsave				; saves LR in LOC-PC
    :lrrestore				; restores LR from LOC-PC
    :lispcontext			; references lisp frame LOC-PC, FN, and entry VSP
    :node				; saves/restores a node value in stack-like memory
    :word				; saves/restores an unboxed word in stack-like memory
    :doubleword				; saves/restores an unboxed doubleword (fp-reg) in stack-like memory
    :vsp				; uses the vsp to save/restore
    :tsp				; uses the tsp to save/restore
    :csp				; uses sp to save/restore
    :push				; saves something
    :pop				; restores something
    :multiple				; saves/restores multiple nodes/words/doublewords
    :ref				; references memory
    :set				; sets memory
    :outgoing-argument			; e.g., pushed as an argument, not to avoid clobbering
    :xref				; makes some label externally visible
    :jump-unknown			; Jumps, but we don't know where ...
    :constant-ref
    :sets-cc                            ; vinsn sets condition codes based on result
    ))

(defparameter *nvp-max* 10 "size of *vinsn-varparts* freelist elements")
(def-standard-initial-binding *vinsn-varparts* (%cons-pool))

(defun alloc-varparts-vector ()
  (without-interrupts
   (let* ((v (pool.data *vinsn-varparts*)))
     (if v
       (progn
         (setf (pool.data *vinsn-varparts*)
               (svref v 0))
          (%init-misc 0 v)
         v)
       (make-array (the fixnum *nvp-max*) :initial-element 0)))))

(defun free-varparts-vector (v)
  (without-interrupts
   (setf (svref v 0) (pool.data *vinsn-varparts*)
         (pool.data *vinsn-varparts*) v)
   nil))

(defun elide-vinsn (vinsn)
  (let* ((nvp (vinsn-template-nvp (vinsn-template vinsn)))
	 (vp (vinsn-variable-parts vinsn)))
    (dotimes (i nvp)
      (let* ((v (svref vp i)))
	(when (typep v 'lreg)
	  (setf (lreg-defs v) (delete vinsn (lreg-defs v)))
	  (setf (lreg-refs v) (delete vinsn (lreg-refs v))))))
    (free-varparts-vector vp)
    (remove-dll-node vinsn)))
    
(defun encode-vinsn-attributes (attribute-list)
  (flet ((attribute-weight (k)
           (let* ((pos (position k *known-vinsn-attributes*)))
             (if pos (ash 1 pos) (error "Unknown vinsn attribute: ~s" k)))))
    (let* ((attr 0))
      (declare (fixnum attr))
      (dolist (a attribute-list attr)
        (setq attr (logior attr (the fixnum (attribute-weight a))))))))


(defun %define-vinsn (backend vinsn-name results args temps body)
  (funcall (backend-define-vinsn backend)
           backend
           vinsn-name
           results
           args
           temps
           body))


;; Fix the opnum's in the vinsn-template-body to agree with the
;; backend's opcode hash table.
(defun fixup-vinsn-template (orig-template opcode-hash)
  (let ((template (cdr orig-template)))
    (when template
      (unless (vinsn-template-p template)
        (setq template (require-type template 'vinsn-template)))
      (let ((new-opcode-alist nil)
            (changes nil)
            (opcode-alist (vinsn-template-opcode-alist template)))
        ;; this is patterned after ppc2-expand-vinsn
        (labels ((walk-form (f)
                   (unless (atom f)
                     (if (fixnump (car f))
                       (got-one f)
                       (dolist (subform (cdr f))
                         (walk-form subform)))))
                 (got-one (f)
                   (let* ((old-opcode (car f))
                          (name (cdr (assq old-opcode opcode-alist)))
                          (new-opcode (and name (gethash name opcode-hash))))
                     (unless new-opcode
                       (cerror "Continue" "Can't find new opcode number ~
                                   for ~s in ~s" (car f) template))
                     (setf (assq new-opcode new-opcode-alist) name)
                     (unless (eq new-opcode old-opcode)
                       (push (cons f new-opcode) changes)))))
          (mapc #'walk-form (vinsn-template-body template))
          (without-interrupts
           (dolist (change changes)
             (setf (caar change) (cdr change)))
           (setf (vinsn-template-opcode-alist template)
                 new-opcode-alist))))
      orig-template)))

(defun fixup-vinsn-templates (templates opcode-hash-table)
  (maphash #'(lambda (name template)
               (declare (ignore name))
               (fixup-vinsn-template template opcode-hash-table))
           templates))
                                       
;;; Could probably split this up and do some arg checking at macroexpand time.
(defun match-template-vregs (template vinsn supplied-vregs)
  (declare (list supplied-vregs))
  (let* ((nsupp (length supplied-vregs))
         (results&args (vinsn-template-results&args template))
         (nra (length results&args))
         (temp-specs (vinsn-template-temp-vreg-specs template))
         (ntemps (length temp-specs))
         (nvp (vinsn-template-nvp template))
         (vp (alloc-varparts-vector))
         (*available-backend-node-temps* *available-backend-node-temps*)
	 (*available-backend-fp-temps* *available-backend-fp-temps*)
         (*available-backend-imm-temps* *available-backend-imm-temps*)
         (*available-backend-crf-temps* *available-backend-crf-temps*))
    (declare (fixnum nvp ntemps nsupp)
             (list result-specs temp-specs arg-specs))
    (unless (= nsupp nra)
      (error "Vinsn ~A expects ~D result/argument specs, received ~D ."
             (vinsn-template-name template) nra nsupp))
    (do* ((i 0 (1+ i))
          (supp supplied-vregs (cdr supp))
          (spec results&args (cdr spec)))
         ((null supp))
      (declare (fixnum i) (list spec supp))
      (setf (svref vp i) (match-vreg (car supp) (cadar spec) vinsn vp i)))
    ;; Allocate some temporaries.
    (do* ((i (- nvp ntemps) (1+ i))
          (temps temp-specs (cdr temps)))
         ((null temps) vp)
      (declare (fixnum i))
      (let* ((spec (cadar temps)))
        (if (and (consp spec) (eq (car spec) :label))
          (let* ((label (aref *backend-labels* (cadr spec))))
            (push vinsn (vinsn-label-refs label))
            (setf (svref vp i) label))
          (let* ((lreg (allocate-temporary-vreg (car temps)))
                 (class (hard-regspec-class lreg))
                 (value (hard-regspec-value lreg)))
            (when value
              (case class
                (#.hard-reg-class-gpr (note-vinsn-sets-gpr vinsn value))
                (#.hard-reg-class-fpr (note-vinsn-sets-fpr vinsn value))))
            (setf (svref vp i) lreg)
            (pushnew vinsn (lreg-defs lreg))
            (pushnew vinsn (lreg-refs lreg))))))))

;;; "spec" is (<name> <class>).
;;;  <class> is keyword or (<keyword> <val>)
(defun allocate-temporary-vreg (spec)
  (setq spec (cadr spec))
  (let* ((class (if (atom spec) spec (car spec)))
         (value (if (atom spec) nil (cadr spec))))
    (if value
      (ecase class
        (:crf (make-wired-lreg (use-crf-temp value) :class hard-reg-class-crf))
        ((:u8 :s8 :u16 :s16 :u32 :s32 :u64 :s64) 
         (make-wired-lreg (use-imm-temp value)
			  :class hard-reg-class-gpr
			  :mode (gpr-mode-name-value class)))
        (:lisp (make-wired-lreg 
                (use-node-temp value) 
                :class hard-reg-class-gpr
                :mode hard-reg-class-gpr-mode-node)))
      (ecase class
        ((:imm :wordptr) 
         (make-unwired-lreg
          (if (= *available-backend-imm-temps* 0) (select-node-temp) (select-imm-temp))
              :class hard-reg-class-gpr
              :mode hard-reg-class-gpr-mode-node)) 
        ((:u8 :s8 :u16 :s16 :u32 :s32 :u64 :s64) 
         (make-unwired-lreg (select-imm-temp)
			    :class hard-reg-class-gpr
			    :mode (gpr-mode-name-value class)))
        (:lisp 
         (make-unwired-lreg 
	  (select-node-temp) 
	  :class hard-reg-class-gpr
	  :mode hard-reg-class-gpr-mode-node))
        (:crf 
         (make-unwired-lreg (select-crf-temp) :class hard-reg-class-crf))))))



(defun select-vinsn (template-or-name template-hash vregs)
  (let* ((template (need-vinsn-template template-or-name template-hash))
         (vinsn (make-vinsn template)))
    (setf (vinsn-variable-parts vinsn) (match-template-vregs template vinsn vregs))
    vinsn))

(defun %emit-vinsn (vlist name vinsn-table &rest vregs)
  (append-dll-node (select-vinsn name vinsn-table vregs) vlist))

(defun varpart-matches-reg (varpart-value class regval spec)
  (setq spec (if (atom spec) spec (car spec)))
  (and
   (or
    (and (eq class hard-reg-class-fpr)
	 (memq spec '(:single-float :double-float)))
    (and (eq class hard-reg-class-gpr)
	 (memq spec '(:u32 :s32 :u16 :s16 :u8 :s8 :lisp :address :imm))))
   (eq (hard-regspec-value varpart-value) regval)))

(defun vinsn-sets-reg-p (element reg)
  (if (typep element 'vinsn)
    (if (vinsn-attribute-p element :call)
      t
      (let* ((class (hard-regspec-class reg))
	     (value (hard-regspec-value reg)))
	(if (eq class hard-reg-class-gpr)
	  (logbitp value (vinsn-gprs-set element))
	  (if (eq class hard-reg-class-fpr)
	    (logbitp value (vinsn-fprs-set element))))))))

;;; Return bitmasks of all GPRs and all FPRs set in the vinsns between
;;; START and END, exclusive.  Any :call vinsn implicitly clobbers
;;; all registers.
(defun regs-set-in-vinsn-sequence (start end)
  (let* ((gprs-set 0)
	 (fprs-set 0))
    (do* ((element (vinsn-succ start) (vinsn-succ element)))
	 ((eq element end) (values gprs-set fprs-set))n
      (if (typep element 'vinsn)
	(if (vinsn-attribute-p element :call)
	  (return (values #xffffffff #xffffffff))
	  (setq gprs-set (logior (vinsn-gprs-set element))
		fprs-set (logior (vinsn-fprs-set element))))))))
      
;;; Return T if any vinsn between START and END (exclusive) sets REG.
(defun vinsn-sequence-sets-reg-p (start end reg)
  (do* ((element (vinsn-succ start) (vinsn-succ element)))
       ((eq element end))
    (if (vinsn-sets-reg-p element reg)
      (return t))))
	

;;; Flow-graph nodes (FGNs)

(defstruct (fgn (:include dll-header))
  (id 0 :type unsigned-byte)
  (inedges ())                          ; list of nodes which reference this node
  (visited nil)                         ; Boolean
)



;;; FGNs which don't terminate with an "external jump"
;;; (jump-return-pc/jump-subprim, etc) jump to their successor, either
;;; explicitly or by falling through.  We can introduce or remove
;;; jumps when linearizing the program.
(defstruct (jumpnode (:include fgn)
		     (:constructor %make-jumpnode (id)))
  (outedge)                             ; the FGN we jump/fall in to.
)

(defun make-jumpnode (id)
  (init-dll-header (%make-jumpnode id)))
    
;;; A node that ends in a conditional branch, followed by an implicit
;;; or explicit jump.  Keep track of the conditional branch and the
;;; node it targets.
(defstruct (condnode (:include jumpnode)
		     (:constructor %make-condnode (id)))
  (condbranch)                          ; the :branch vinsn
  (branchedge)                          ; the FGN it targets
)

(defun make-condnode (id)
  (init-dll-header (%make-condnode id)))
	  
;;; A node that terminates with a return i.e., a jump-return-pc or
;;; jump-subprim.
(defstruct (returnnode (:include fgn)
		       (:constructor %make-returnnode (id)))
)

(defun make-returnnode (id)
  (init-dll-header (%make-returnnode id)))

;;; Some specified attribute is true.
(defun %vinsn-attribute-p (vinsn mask)
  (declare (fixnum mask))
  (if (vinsn-p vinsn)
    (let* ((template (vinsn-template vinsn)))
      (not (eql 0 (logand mask (the fixnum (vinsn-template-attributes template))))))))

;;; All specified attributes are true.
(defun %vinsn-attribute-= (vinsn mask)
  (declare (fixnum mask))
  (if (vinsn-p vinsn)
    (let* ((template (vinsn-template vinsn)))
      (= mask (the fixnum (logand mask (the fixnum (vinsn-template-attributes template))))))))
  
(defmacro vinsn-attribute-p (vinsn &rest attrs)
  `(%vinsn-attribute-p ,vinsn ,(encode-vinsn-attributes attrs)))

(defmacro vinsn-attribute-= (vinsn &rest attrs)
  `(%vinsn-attribute-= ,vinsn ,(encode-vinsn-attributes attrs)))

;;; Ensure that conditional branches that aren't followed by jumps are
;;; followed by (jump lab-next) @lab-next.  Ensure that JUMPs and
;;; JUMPLRs are followed by labels.  It's easiest to do this by
;;; walking backwards.  When we're all done, labels will mark the
;;; start of each block.

(defun normalize-vinsns (header)
  (do* ((prevtype :label currtype)
        (current (dll-header-last header) (dll-node-pred current))
        (currtype nil))
       ((eq current header)
	(unless (eq prevtype :label)
	  (insert-dll-node-after
	   (aref *backend-labels* (backend-get-next-label))
	   current)))
    (setq currtype (cond ((vinsn-label-p current) :label)
                         ((vinsn-attribute-p current :branch) :branch)
                         ((vinsn-attribute-p current :jump) :jump)
                         ((vinsn-attribute-p current :jumplr) :jumplr)))
    (case currtype
      ((:jump :jumplr)
       (unless (eq prevtype :label)
         (let* ((lab (aref *backend-labels* (backend-get-next-label))))
           (insert-dll-node-after lab current))))
      (:branch
       (unless (eq prevtype :jump)
         (let* ((lab
                 (if (eq prevtype :label)
                   (dll-node-succ current)
                   (aref *backend-labels* (backend-get-next-label))))
                (jump (select-vinsn "JUMP" *backend-vinsns* (list lab))))
           (unless (eq prevtype :label)
             (insert-dll-node-after lab current))
           (insert-dll-node-after jump current))))
      ((nil)
       (if (eq prevtype :label)
	 (let* ((lab (dll-node-succ current)))
	   (when (vinsn-label-p lab)
             (insert-dll-node-after
              (select-vinsn "JUMP" *backend-vinsns* (list lab))
	      current))))))))


;;; Unless the header is empty, remove the last vinsn and all preceding
;;; vinsns up to and including the preceding label.  (Since the vinsns
;;; have been normalized, there will always be a preceding label.)
;;; Return the label and the last vinsn, or (values nil nil.)
(defun remove-last-basic-block (vinsns)
  (do* ((i 1 (1+ i))
	(current (dll-header-last vinsns) (dll-node-pred current)))
       ((eq current vinsns) (values nil nil))
    (declare (fixnum i))
    (if (vinsn-label-p current)
      (return (remove-dll-node current i)))))

;;; Create a flow graph from vinsns and return the entry node.
(defun create-flow-graph (vinsns)
  (let* ((nodes ()))
    (declare (fixnum id))
    (flet ((label->fgn (label) (dll-node-pred label)))
      (loop
	  (multiple-value-bind (label last) (remove-last-basic-block vinsns)
	    (when (null label) (return))
	    (let* ((id (vinsn-label-id label))
		   (node (if (vinsn-attribute-p last :jumpLR)
			   (make-returnnode id)
			   (if (vinsn-attribute-p (dll-node-pred last) :branch)
			     (make-condnode id)
			     (make-jumpnode id)))))
	      (insert-dll-node-after label node last)
	      (push node nodes))))
      (dolist (node nodes nodes)
	(if (typep node 'jumpnode)
	  (let* ((jump (dll-header-last node))
		 (jmptarget (branch-target-node jump)))
	    (setf (jumpnode-outedge node) jmptarget)
	    (pushnew node (fgn-inedges jmptarget))
	    (if (typep node 'condnode)	; a subtype of jumpnode
	      (let* ((branch (dll-node-pred jump))
		     (branchtarget (branch-target-node branch)))
		(setf (condnode-condbranch node) branch)
		(pushnew node (fgn-inedges branchtarget))))))))))
  
                         
(defun delete-unreferenced-labels (labels)
  (delete #'(lambda (l)
              (unless (vinsn-label-refs l)
                (when (vinsn-label-succ l)
                  (remove-dll-node l))
                t)) labels :test #'funcall))

(defun branch-target-node (v)
  (dll-node-pred (svref (vinsn-variable-parts v) 0)))

(defun replace-label-refs (vinsn old-label new-label)
  (let ((vp (vinsn-variable-parts vinsn)))
    (dotimes (i (length vp))
      (when (eq (svref vp i) old-label)
        (setf (svref vp i) new-label)))))
  
;;; Try to remove jumps/branches to jumps.
(defun maximize-jumps (header)
  (do* ((prev nil next)
        (next (dll-header-first header) (dll-node-succ next)))
       ((eq next header))
    (when (and (vinsn-attribute-p next :jump)
               (vinsn-label-p  prev))
      (let* ((target (svref (vinsn-variable-parts next) 0)))
        (unless (eq target prev)
          (dolist (ref (vinsn-label-refs prev) (setf (vinsn-label-refs prev) nil))
            (replace-label-refs ref prev target)
            (push ref (vinsn-label-refs target))))))))

(defun optimize-vinsns (header)
  ;; Delete unreferenced labels that the compiler might have emitted.
  ;; Subsequent operations may cause other labels to become
  ;; unreferenced.
  (let* ((labels (collect ((labs)) 
                   (do-dll-nodes (v header)
                     (when (vinsn-label-p v) (labs v)))
                   (labs))))
    ;; Look for pairs of adjacent, referenced labels.
    ;; Merge them together (so that one of them becomes unreferenced.)
    ;; Repeat the process until no pairs are found.
    (do* ((repeat t))
         ((not repeat))
      (setq repeat nil 
            labels (delete-unreferenced-labels labels))
      (dolist (l labels)
        (let* ((succ (vinsn-label-succ l)))
          (when (vinsn-label-p succ)
            (backend-merge-labels l succ)
            (setq repeat t)
            (return)))))
    (maximize-jumps header)
    (delete-unreferenced-labels labels)
    (normalize-vinsns header)
  ))

(defun show-vinsns (vinsns indent)
  (do-dll-nodes (n vinsns)
    (format t "~&~v@t~s" indent n)))

(defun show-fgn (node)
  (format t "~&~s (~d) {~a}" (type-of node) (fgn-id node) (mapcar #'fgn-id (fgn-inedges node)))
  (show-vinsns node 2)
  (terpri)
  (terpri))

(defun dfs-walk (fgns &key
		      process-before process-after
		      process-succ-before process-succ-after)
  (labels ((dfs (node)
	     (when process-before
	       (funcall process-before node))
	     (setf (fgn-visited node) t)
	     (when (typep node 'jumpnode)
	       (let* ((outedge (jumpnode-outedge node)))
		 (unless (fgn-visited outedge)
		   (when process-succ-before
		     (funcall process-succ-before outedge))
		   (dfs outedge)
		   (when process-succ-after
		     (funcall process-succ-after outedge))))
	       (when (typep node 'condnode)
		 (let* ((branchedge (branch-target-node
				     (condnode-condbranch node))))
		   (unless (fgn-visited branchedge)
		     (when process-succ-before
		       (funcall process-succ-before branchedge))
		     (dfs branchedge)
		     (when process-succ-after
		       (funcall process-succ-after branchedge))))))
	     (when process-after
	       (funcall process-after node))))
    (dolist (n fgns)
      (setf (fgn-visited n) nil))
    (dfs (car fgns))))

(defun dfs-postorder (fgns)
  (let* ((n (length fgns))
	 (v (make-array n))
	 (p -1)
	 (process-after #'(lambda (node)
			    (setf (svref v (incf p)) node))))
    (declare (fixnum p) (dynamic-extent process-after))
    (dfs-walk fgns :process-after process-after)
    v))

;;; This generally only gives a meaningful result if pass 2 of the
;;; compiler has been compiled in the current session.
;;; TODO (maybe): keep track of the "expected missing vinsns" for
;;; each backend, call this function after compiling pass 2.  That's
;;; a little weird, since it'd require modifying backend X whenever
;;; backend Y changes, but it's probably better than blowing up when
;;; compiling user code.
(defun missing-vinsns (&optional (backend *target-backend*))
  (let* ((missing ()))
    (maphash #'(lambda (name info)
                 (unless (cdr info)
                   (push name missing)))
             (backend-p2-vinsn-templates backend))
    missing))
		      
(provide "VINSN")
