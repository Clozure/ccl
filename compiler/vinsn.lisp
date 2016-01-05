;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Clozure CL.  
;;;
;;;   clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(cl:in-package "CCL")

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (require "DLL-NODE")
  (require "BACKEND"))

(defparameter *linear-scan-verbose* nil)


(defun ls-format (&rest args )
  (when (and *backend-use-linear-scan* *linear-scan-verbose*)
    (apply #'format *debug-io* args)))
(defun ls-break (&rest args)
  (when (and *backend-use-linear-scan* *linear-scan-verbose*)
    (apply #'break args)))
(defun ls-note (&rest args)
  (when (and *backend-use-linear-scan* *linear-scan-verbose*)
    (apply #'warn args)))

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

(defstatic *empty-vinsn-template* (make-vinsn-template))

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
  (gprs-read 0)
  (fprs-read 0)
  (notes ())
  (sequence 0 :type fixnum)
  fgn
)


(defun make-vinsn (template)
  (%make-vinsn template))

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
(defstruct (vinsn-list (:include dll-header)
                       (:constructor %make-vinsn-list))
  (lregs (make-array 64 :fill-pointer 0 :adjustable t))
  flow-graph
  (intervals (make-array 64 :fill-pointer 0 :adjustable t))
  max-seq
  (spill-base 0 :type fixnum)
  (spill-depth 0 :type fixnum)
  (max-spill-depth 0 :type fixnum )
  (spill-area-used (make-array 64 :element-type 'bit))
  (available-physical-registers #() :type simple-vector)
  (nfp-spill-offset 0 :type fixnum)
  (max-nfp-spill-depth 0 :type fixnum)
  )

(defun make-vinsn-list ()
  (init-dll-header (%make-vinsn-list)))

(defvar *vinsn-list*)
                    
(defun make-vinsn-label (id)
  (%make-vinsn-label id))

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
  (address nil)                           ; lap label
  (peer nil :type (or null vinsn-note))
  (class nil)
  (info nil :type (or null simple-vector))
  (container nil  :type (or null vinsn)))


(defun add-vinsn-note (note vinsn)
  (push note (vinsn-notes vinsn))
  (setf (vinsn-note-container note) vinsn))





(defun print-vinsn-note (n s d)
  (declare (ignore d))
  (print-unreadable-object (n s :type t)
    (format s "~d" (vinsn-note-class n))
    (let* ((info (vinsn-note-info n)))
      (when info (format s " / ~S" info)))))
  
(defun make-vinsn-note (class info)
  (%make-vinsn-note :class class :info (if info (apply #'vector info))))

(defun enqueue-vinsn-note (seg class &rest info)
  (let* ((note (make-vinsn-note class info)))
    (push note (dll-header-info seg))
    note))

(defun close-vinsn-note (seg n)
  (let* ((vinsn (last-vinsn seg)))
    (unless vinsn
      (nx-error "No last vinsn in ~s." seg))
    (let* ((end (%make-vinsn-note :peer n :class :close)))
      #+debug
      (format t "~& adding note ~s to vinsn ~s, closing ~s" end vinsn n)
      (add-vinsn-note end vinsn)
      (setf (vinsn-note-peer n) end))))

        

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
         (opname (vinsn-template-name template))
         (sequence (vinsn-sequence v)))
    (when (and (vinsn-attribute-p v :subprim)
               (typep (car args) 'integer))
      (let* ((spinfo (find (car args)
                           (arch::target-subprims-table
                            (backend-target-arch *target-backend*))
                           :key #'subprimitive-info-offset)))
        (when spinfo
          (setf (car args) (subprimitive-info-name spinfo)))))
    (print-unreadable-object (v stream)
      (when sequence (format stream "@~d " sequence))
      (if results (format stream "~A ~S " (if (cdr results) results (car results)) opsym))
      (if infix
        (format stream "~A ~A ~A" (car args) opname (cadr args))
        (format stream "~A~{ ~A~}" opname args))
      (let* ((annotation (vinsn-annotation v)))
        (when annotation
          (format stream " ||~a|| " annotation))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *known-vinsn-attributes*
  '(
    :jump				; an unconditional branch
    :branch				; a conditional branch
    :call				; a jump that returns
    :align				; aligns FOLLOWING label
    :subprim                            ; first argument is a subprim address
    :jumpLR				; Jumps to the LR, possibly stopping off at a function along the way.
    :extended-call			; extend call interval
    :lrrestore                               ; suppress ref/def tracking
    :lispcontext			; references lisp frame LOC-PC, FN, and entry VSP
    :node				; saves/restores a node value in stack-like memory
    :word				; saves/restores an unboxed word in stack-like memory
    :spill
    :vsp				; uses the vsp to save/restore
    :tsp				; uses the tsp to save/restore
    :reload				; 
    :push				; saves something
    :pop				; restores something
    :multiple				; saves/restores multiple nodes/words/doublewords
    :ref				; references memory
    :set				; sets memory
    :uses-frame-pointer                 ; uses frame pointer
    :needs-frame-pointer                ; needs to use frame pointer
    :jump-unknown			; Jumps, but we don't know where ...
    :constant-ref
    :trivial-copy                       ; may be a useless vinsn
    :discard                            ; adjusts a stack pointer
    :nfp                                ; references the nfp
    :predicatable                       ; all instructions can be predicated, no instructions set or test condition codes.
    :sets-lr                            ; uses the link register, if there is one.
    )))


;;; This should only be called by old code during bootstrapping.
(defun free-varparts-vector (v)
  (declare (ignore v)))



(defun distribute-vinsn-notes (notes pred succ)
  (or (null notes)
      (and (dolist (note notes t)
             (unless (if (eq :close (vinsn-note-class note))
                       (typep pred 'vinsn)
                       (typep succ 'vinsn))
               (return nil)))
           (dolist (note notes t)
             (if (eq :close (vinsn-note-class note))
               (add-vinsn-note note pred)
               (add-vinsn-note note succ))))))
               

(defun elide-vinsn (vinsn)
  (let* ((template (vinsn-template vinsn))
             (nvp (vinsn-template-nvp template))
             (vp (vinsn-variable-parts vinsn)))
        (dotimes (i nvp)
          (let* ((v (svref vp i)))
            (when (typep v 'vinsn-label)
              (setf (vinsn-label-refs v)
                    (delete vinsn (vinsn-label-refs v))))
            (when (typep v 'lreg)
              (setf (lreg-defs v) (delete vinsn (lreg-defs v)))
              (setf (lreg-refs v) (delete vinsn (lreg-refs v))))))
        (setf (vinsn-variable-parts vinsn) nil)
        (if (distribute-vinsn-notes (vinsn-notes vinsn) (vinsn-pred vinsn) (vinsn-succ vinsn))
          (remove-dll-node vinsn)
          (setf (vinsn-template vinsn) *empty-vinsn-template*))))
    
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
         (vp (make-array nvp))
         (*available-backend-node-temps* *available-backend-node-temps*)
	 (*available-backend-fp-temps* *available-backend-fp-temps*)
         (*available-backend-imm-temps* *available-backend-imm-temps*)
         (*available-backend-crf-temps* *available-backend-crf-temps*))
    (declare (fixnum nvp ntemps nsupp)
             (list temp-specs))
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
                (#.hard-reg-class-fpr (note-vinsn-sets-fpr-lreg vinsn lreg))))
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
        (:crf (make-wired-lreg (use-crf-temp value) :local-p t  :class hard-reg-class-crf))
        ((:u8 :s8 :u16 :s16 :u32 :s32 :u64 :s64) 
         (make-wired-lreg (use-imm-temp value)
                          :local-p t
			  :class hard-reg-class-gpr
			  :mode (gpr-mode-name-value class)))
        (:lisp (make-wired-lreg
                (use-node-temp value) 
                :local-p t
                :class hard-reg-class-gpr
                :mode hard-reg-class-gpr-mode-node))
        ((:single-float :double-float :complex-double-float :complex-single-float)
         (let* ((lreg (make-wired-lreg value
                                       :local-p t
                                       :class hard-reg-class-fpr
                                       :mode (fpr-mode-name-value class))))
                         (use-fp-reg lreg)
                         lreg)))
      (ecase class
        ((:imm :wordptr) 
         (make-unwired-lreg
          (if (= *available-backend-imm-temps* 0) (select-node-temp) (select-imm-temp))
              :class hard-reg-class-gpr
              :mode hard-reg-class-gpr-mode-node)) 
        ((:u8 :s8 :u16 :s16 :u32 :s32 :u64 :s64 :address) 
         (make-unwired-lreg (select-imm-temp)
			    :class hard-reg-class-gpr
			    :mode (gpr-mode-name-value class)))
        ((:double-float :single-float :complex-double-float :complex-single-float)
         (let* ((lreg (make-unwired-lreg (select-fp-temp class)
                                         :class hard-reg-class-fpr
                                         :mode (fpr-mode-name-value class))))
           (use-fp-reg lreg)
           lreg))
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
  (let* ((vinsn (select-vinsn name vinsn-table vregs))
         (notes (dll-header-info vlist)))
    (when notes
      (dolist (note notes (setf (dll-header-info vlist) nil))
        (push note (vinsn-notes vinsn))))
    (append-dll-node vinsn vlist)))

(defun varpart-matches-reg (varpart-value class regval spec)
  (setq spec (if (atom spec) spec (car spec)))
  (and
   (or
    (and (eq class hard-reg-class-fpr)
	 (memq spec '(:single-float :double-float :complex-single-float :complex-double-float)))
    (and (eq class hard-reg-class-gpr)
	 (memq spec '(:u32 :s32 :u16 :s16 :u8 :s8 :lisp :address :imm))))
   (eq (hard-regspec-value varpart-value) regval)))

(defun vinsn-refs-reg-p (element reg)
  (if (typep element 'vinsn)
    (if (vinsn-attribute-p element :call)
      t
      (let* ((class (hard-regspec-class reg))
	     (value (hard-regspec-value reg)))
	(if (eq class hard-reg-class-gpr)
	  (logbitp value (vinsn-gprs-read element))
	  (if (eq class hard-reg-class-fpr)
            ;; The FPR is logically read in the vinsn if it or any
            ;; conflicting FPR is physically read in the vinsn.
            (logtest (fpr-mask-for-vreg reg) (vinsn-fprs-read element))))))))

(defun vinsn-sets-reg-p (element reg)
  (if (typep element 'vinsn)
    (if (vinsn-attribute-p element :call)
      t
      (let* ((class (hard-regspec-class reg))
	     (value (hard-regspec-value reg)))
	(if (eq class hard-reg-class-gpr)
	  (logbitp value (vinsn-gprs-set element))
	  (if (eq class hard-reg-class-fpr)
            ;; The FPR is logically set in the vinsn if it or any
            ;; conflicting FPR is physically set in the vinsn.
            (logtest (fpr-mask-for-vreg reg) (vinsn-fprs-set element))))))))

;;; Return bitmasks of all GPRs and all FPRs set in the vinsns between
;;; START and END, exclusive.  Any :call vinsn implicitly clobbers
;;; all registers.
(defun regs-set-in-vinsn-sequence (start end)
  (let* ((gprs-set 0)
	 (fprs-set 0))
    (do* ((element (dll-node-succ start) (dll-node-succ element)))
	 ((eq element end) (values gprs-set fprs-set))
      (if (typep element 'vinsn)
	(if (vinsn-attribute-p element :call)
	  (return (values #xffffffff #xffffffff))
	  (setq gprs-set (logior gprs-set (vinsn-gprs-set element))
		fprs-set (logior fprs-set (vinsn-fprs-set element))))))))


      
;;; If any vinsn between START and END (exclusive) sets REG, return
;;; that vinsn; otherwise, return NIL.
(defun vinsn-sequence-sets-reg-p (start end reg)
  (do* ((element (dll-node-succ start) (dll-node-succ element)))
       ((eq element end))
    (if (vinsn-sets-reg-p element reg)
      (return element))))
	
;;; If any vinsn between START and END (exclusive) refs REG, return
;;; the last such vinsn; otherwise, return NIL.
(defun vinsn-sequence-refs-reg-p (start end reg)
  (do* ((element (dll-node-pred end) (dll-node-pred element)))
       ((eq element start))
    (if (vinsn-refs-reg-p element reg)
      (return element))))


;;; Return T if any vinsn between START and END (exclusive) has all
;;; attributes set in ATTR set.
(defun %vinsn-sequence-has-attribute-p (start end attr)
  (do* ((element (dll-node-succ start) (dll-node-succ element)))
       ((eq element end))
    (when (typep element 'vinsn)
      (when (eql attr (logand (vinsn-template-attributes (vinsn-template element)) attr))
        (return t)))))

;;; Return T if any vinsn between START and END (exclusive) has some
;;; some attributes set in attr set.
(defun %vinsn-sequence-has-some-attribute-p (start end attr)
  (do* ((element (dll-node-succ start) (dll-node-succ element)))
       ((eq element end))
    (when (typep element 'vinsn)
      (when (logtest attr (vinsn-template-attributes (vinsn-template element)))
        (return t)))))

(defmacro vinsn-sequence-has-attribute-p (start end &rest attrs)
  `(%vinsn-sequence-has-attribute-p ,start ,end ,(encode-vinsn-attributes attrs)))

(defmacro vinsn-sequence-has-some-attribute-p (start end &rest attrs)
  `(%vinsn-sequence-has-some-attribute-p ,start ,end ,(encode-vinsn-attributes attrs)))

;;; Return T iff vinsn is between START and END (exclusive).
(defun vinsn-in-sequence-p (vinsn start end)
  (do* ((element (dll-node-succ start) (dll-node-succ element)))
       ((eq element end))
    (when (eq vinsn element)
      (return t))))

(defun last-vinsn (seg &optional (after seg))
  ;; Try to find something that isn't a SOURCE-NOTE.  Go ahead.  I dare you.
  (do* ((element (dll-header-last seg) (dll-node-pred element)))
       ((eq element after))               ;told ya!
    (when (typep element 'vinsn)
      (return element))))


;;; Flow-graph nodes (FGNs)

(defstruct (fgn (:include dll-header))
  (id 0 :type unsigned-byte)
  (inedges ())                          ; list of nodes which reference this node
  (outedges ())
  (visited nil)                         ; Boolean
  live-gen
  live-kill
  live-out
  live-in
  spills                                ; alist of intervals spilled here
  reloads                               ; same as above
  extended-pred
  extended-succ
  call-vinsns
)

(defmethod print-object (( node fgn) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "(~s)" (fgn-id node))))



(defconstant interval-pre-spilled-bit 1)
(defconstant interval-flag-pre-spilled (ash 1 interval-pre-spilled-bit))




;;; FGNs which don't terminate with an "external jump"
;;; (jump-return-pc/jump-subprim, etc) jump to their successor, either
;;; explicitly or by falling through.  We can introduce or remove
;;; jumps when linearizing the program.
(defstruct (jumpnode (:include fgn)
                               (:constructor %make-jumpnode (id)))
  (outedge)                             ; the FGN we jump/fall in to.
                              ; true if outedge is next in emit order
)

(defun make-jumpnode (id)
  (init-dll-header (%make-jumpnode id)))
    
;;; A node that ends in a conditional branch, followed by an implicit
;;; or explicit jump.  Keep track of the conditional branch and the
;;; node it targets.
(defstruct (condnode (:include jumpnode)
		     (:constructor %make-condnode (id)))
                                        ; the FGN it targets
  condbranch
  branchedge
)

(defun make-condnode (id)
  (init-dll-header (%make-condnode id)))


;;; A node that ends with a CALL, followed by an implicit or explict jump.
(defstruct (callnode (:include jumpnode)
                     (:constructor %make-callnode (id mycall)))
  mycall

)
                              
(defun make-callnode (id mycall)
  (init-dll-header (%make-callnode id mycall)))        

;;; A node that terminates with a return i.e., a jump-return-pc or
;;; jump-subprim.
(defstruct (returnnode (:include fgn)
		       (:constructor %make-returnnode (id)))
)

(defun make-returnnode (id)
  (init-dll-header (%make-returnnode id)))


(defun find-extended-block-bounds (fgn)
  (let* ((first (do* ((first fgn pred)
                      (pred (fgn-extended-pred first) (fgn-extended-pred first)))
                     ((null pred) first)))
         (last  (do* ((last fgn succ)
                      (succ (fgn-extended-succ last) (fgn-extended-succ last)))
                     ((null succ) last))))
    (values (vinsn-sequence (dll-node-succ (dll-header-first first)))
              (vinsn-sequence (dll-header-last last)))))
         
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
	   current))
        (merge-adjacent-labels header))
    (setq currtype (cond ((vinsn-label-p current) :label)
                         ((vinsn-attribute-p current :branch) :branch)
                         ((vinsn-attribute-p current :call) :call)
                         ((vinsn-attribute-p current :jump) :jump)
                         ((vinsn-attribute-p current :jumplr) :jumplr)))
    (case currtype
      ((:jump :jumplr)
       (unless (eq prevtype :label)
         (let* ((lab (aref *backend-labels* (backend-get-next-label))))
           (insert-dll-node-after lab current))))
      ((:branch :call)
       (unless (eq prevtype :jump)
         (let* ((lab
                 (if (eq prevtype :label)
                   (dll-node-succ current)
                   (aref *backend-labels* (backend-get-next-label))))
                (jump (select-vinsn "JUMP" *backend-vinsns* (list lab))))
           (push jump (vinsn-label-refs lab))
           (unless (eq prevtype :label)
             (insert-dll-node-after lab current))
           (insert-dll-node-after jump current))))
      ((nil)
       (if (eq prevtype :label)
	 (let* ((lab (dll-node-succ current)))
	   (when (vinsn-label-p lab)
             (insert-dll-node-after
              (let* ((jump (select-vinsn "JUMP" *backend-vinsns* (list lab))))

                (push jump (vinsn-label-refs lab))
                jump)
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
    (when (vinsn-label-p current)
      (when (eql i 1) (break))
      (return (remove-dll-node current i)))))


                              


                         
(defun compute-live-sets (fg header)
  (let* ((regs (vinsn-list-lregs header))
         (nregs (length regs)))
    (declare (fixnum nregs)
             (type (vector t) regs))
    (dolist (block fg)
      (setf (fgn-live-gen block) (make-array nregs :element-type 'bit)
            (fgn-live-kill block) (make-array nregs :element-type 'bit)
            (fgn-live-out block) (make-array nregs :element-type 'bit)
            (fgn-live-in block)  (make-array nregs :element-type 'bit)))
   
    (dolist (block fg)
      (do-dll-nodes (vinsn block)
        (when (typep vinsn 'vinsn)
          (let* ((template (vinsn-template vinsn))
                 (vp (vinsn-variable-parts vinsn))
                 (nres (length (vinsn-template-result-vreg-specs template)))
                 (nargs (length (vinsn-template-argument-vreg-specs template)))
                 (ntemps (length (vinsn-template-temp-vreg-specs template)))
                 (nhybrids (vinsn-template-nhybrids template))
                 (nntemps (- (the fixnum (+ nres nargs)) nhybrids))
                 (nvp (+ nntemps ntemps))
                 (gen (fgn-live-gen block))
                 (kill (fgn-live-kill block)))
            (declare (simple-vector vp) (fixnum nres nargs ntemps nntemps nvp nhybrids)
                     (simple-bit-vector gen kill))
            (do* ((i (the fixnum (- nres nhybrids)) (1+ i)))
                 ((= i nntemps))
              (declare (fixnum i))
              (let* ((part (svref vp i)))
                (when (typep part 'lreg)
                  (let* ((id (lreg-id part)))
                    (when (eql 0 (sbit kill id))
                      (setf (sbit gen id) 1))))))
            (dotimes (i nres)
              (let* ((part (svref vp i)))
                (when (typep part 'lreg)
                  (setf (sbit kill (lreg-id part)) 1))))
            (do* ((i nntemps (1+ i)))
                 ((= i nvp))
              (declare (fixnum i))
              (let* ((part (svref vp i)))
                (when (typep part 'lreg)
                  (setf (sbit kill (lreg-id part)) 1))))))))
    (let* ((rnodes (coerce (dfs-postorder fg) 'list))
           (changed nil))
      (unless (eql (length fg) (length rnodes)) (break))
      (loop
        (setq changed nil)
        (dolist (block rnodes)
          (let* ((in (make-array nregs :element-type 'bit))
                 (out (make-array nregs :element-type 'bit)))
            (declare (dynamic-extent in out))
            (when (typep block 'condnode)
              (bit-ior out (fgn-live-in (branch-target-node (condnode-condbranch block))) out))
            (when (typep block 'jumpnode)
              (bit-ior out (fgn-live-in (jumpnode-outedge block)) out ))
            (bit-andc2 in (fgn-live-kill block) in)
            (bit-ior in (fgn-live-gen block) in)
            (unless (equal out (fgn-live-out block))
              (setq changed t)
              (bit-boole boole-1 out out (fgn-live-out block)))
            (unless (equal in (fgn-live-in block))
              (setq changed t)
              (bit-boole boole-1 in in (fgn-live-in block)))))
        (unless changed   (return fg))))))


;;; Create a flow graph from vinsns and return the entry node.
(defun create-flow-graph (vinsns)
  (show-vinsns vinsns 2)
  (let* ((nodes ()))
    
    (flet ((label->fgn (label) (dll-node-pred label)))
      (loop
        (multiple-value-bind (label last) (remove-last-basic-block vinsns)
          (when (null label) (return))
          (let* ((id (vinsn-label-id label))
                 (node (if (vinsn-attribute-p last :jumpLR)
                         (make-returnnode id)
                         (let* ((pred (dll-node-pred last)))
                           (if (vinsn-attribute-p pred :branch)
                             (make-condnode id)
                             (if (vinsn-attribute-p pred :call)
                               (make-callnode id pred)
                               (make-jumpnode id)))))))
            (declare (fixnum id))
            (insert-dll-node-after label node last)
            (do-dll-nodes (v node) (if (vinsn-attribute-p v :call)
                                     (push v (fgn-call-vinsns node))))
            (push node nodes))))
      
      (dolist (node nodes)
	(if (typep node 'jumpnode)
	  (let* ((jump (dll-header-last node))
		 (jmptarget (branch-target-node jump)))
	    (setf (jumpnode-outedge node) jmptarget)
	    (pushnew node (fgn-inedges jmptarget))
            (pushnew jmptarget (fgn-outedges node))
	    (if (typep node 'condnode)	; a subtype of jumpnode
	      (let* ((branch (dll-node-pred jump))
		     (branchtarget (branch-target-node branch)))
		(setf (condnode-condbranch node) branch
                      (condnode-branchedge node) branchtarget)
                
                (pushnew branchtarget (fgn-outedges node))
		(pushnew node (fgn-inedges branchtarget)))))))
      ;; Merge adjacent nodes where the first "falls into" the second
      (do* ((nodes1 nodes (cdr nodes1))
            (first (car nodes1) (car nodes1))
            (second (cadr nodes1) (cadr nodes1)))
           ((or (null first) (null second)) (setq nodes (delete nil nodes)))
        (when (and (null (cdr (fgn-inedges second))) (eq first (car (fgn-inedges second)))
                   (null (cdr (fgn-outedges first))) (eq second (car (fgn-outedges first))))
          (setf (fgn-extended-pred second) first
                (fgn-extended-succ first) second)
                   


          (unless (typep first 'callnode)
            (dolist (ref (fgn-inedges first))
              (nsubstitute second first (fgn-outedges ref))
              (when (typep ref 'jumpnode)
                (when (eq first (jumpnode-outedge ref))
                  (setf (jumpnode-outedge ref) second))
                (when (typep ref 'condnode)
                  (when (eq first (condnode-branchedge ref))
                    (setf (condnode-branchedge ref) second)))))

            (if (setf (fgn-extended-pred second) (fgn-extended-pred first))
              (setf (fgn-extended-succ (fgn-extended-pred first)) second))
            (setf (fgn-inedges second) (fgn-inedges first))
            (multiple-value-bind (label1 jump) (detach-dll-nodes first)
              (let* ((label2 (dll-header-succ second)))
                (insert-dll-node-before label1 label2 jump)
                (when (null (delete jump (vinsn-label-refs label2)))
                  (remove-dll-node label2))
                (remove-dll-node jump)
                (setf (fgn-id second) (fgn-id first))))
          
            (setf (car nodes1) nil))))



      
      (setf (vinsn-list-flow-graph vinsns) (refine-flow-graph nodes)))))

(defun remove-block-vinsns (fgn)
  (do-tail-dll-nodes (vinsn fgn) (elide-vinsn vinsn)))

(defun refine-flow-graph (fg)
  (dfs-walk fg)
  (dolist (block fg) (unless (fgn-visited block) (remove-block-vinsns block)))
  (remove-if-not #'fgn-visited fg))


(defun linearize-flow-graph (fg header)
  (do* ((head (car fg) (car tail))
        (tail (cdr fg) (cdr tail)))
       ((null head) header)
    (multiple-value-bind (first last) (detach-dll-nodes head)
      (when first
        (insert-dll-node-before first header last)
        (when (and (vinsn-attribute-p last :jump)
                   (eq (car tail) (branch-target-node last)))
          (let* ((lab (dll-node-succ (car tail))))
            (when (null (setf (vinsn-label-refs lab)
                              (delete last (vinsn-label-refs lab))))
              (remove-dll-node lab)))
          (elide-vinsn last))))))
                   
    
                         
(defun delete-unreferenced-labels (labels)
  (delete #'(lambda (l)
              (unless (vinsn-label-refs l)
                (when (vinsn-label-succ l)
                  (remove-dll-node l))
                t)) labels :test #'funcall))

(defun branch-target-node (v)
  (check-type v vinsn)
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

(defparameter *nx-do-dead-code-elimination* t)


(defun eliminate-dead-code (header)
  (when *nx-do-dead-code-elimination*
    (let* ((eliding nil)
           (won nil))
      (do-dll-nodes (element header won)
        ;; If a label, leave it.
        (etypecase element
          (vinsn-label
           (when (typep (vinsn-label-id element) 'fixnum)
             (if (vinsn-label-refs element)
               (setq eliding nil))))
          (vinsn
           (when (vinsn-attribute-p element :align)
             (let* ((next (vinsn-succ element)))
               (when (and (typep next 'vinsn-label)
                          (typep (vinsn-label-id next) 'fixnum)
                          (not (null (vinsn-label-refs next))))
                 (setq eliding nil))))
           (cond (eliding
                  (setq won t)
                  (let* ((operands (vinsn-variable-parts element)))
                    (dotimes (i (length operands) (elide-vinsn element))
                      (let* ((op (svref operands i)))
                        (when (typep op 'vinsn-label)
                          (setf (vinsn-label-refs op)
                                (delete element (vinsn-label-refs op))))))))
                 (t (setq eliding (vinsn-attribute-p element :jump))))))))))
         

(defvar *backend-use-linear-scan* nil)

(define-condition linear-scan-bailout ()
  ())

(defconstant interval-regtype-node 0)
(defconstant interval-regtype-imm 1)
(defconstant interval-regtype-float 2)
(defconstant interval-regtype-cr 3)

(defstruct (interval (:include dll-node)
                     (:constructor make-interval (lreg begin end regtype preg)))
  lreg
  (begin 0)
  (end 0)
  (regtype 0)
  (preg nll)
  (avail 0 :type fixnum)                             ; available regs before we assigned preg
  idx
  parent
  (spill-offset nil)
  (killed #() :type simple-vector)
  child
  (flags 0 :type fixnum)
  (use-positions () :type list) ; sequence numbers of lreg-refs and defs
  (active-before () :type list)
  (active-after () :type list)
  (trivial-def nil)
  (non-trivial-conflicts () :type list)
  (alt-preg 0 :type (unsigned-byte 4))
  (conflicts-with () :type list)
)


(defmethod print-object ((i interval) stream)
  (print-unreadable-object (i stream :type t)
    (format stream "~c ~d:(~d) ~s ~s/~s ~s (~s)" (if (interval-trivial-def i) #\? #\space )(interval-idx i) (interval-flags i) (interval-lreg i) (interval-begin i) (interval-end i) (interval-regtype i) (interval-preg i))))


                   



(defun registers-killed-by-call (vinsn masks)
    (declare (simple-vector masks) (ignorable vinsn))
  (let* ((nodes *backend-node-temps*)
         (imms  *backend-imm-temps*)
         (fprs *backend-fp-temps*)
         (crfs *backend-crf-temps*))
    (when (vinsn-attribute-p vinsn :subprim)
      (case (vinsn-template-name (vinsn-template vinsn))

        
        
        ))

    (setf (aref masks interval-regtype-node) nodes
          (aref masks  interval-regtype-imm) imms
          (aref masks interval-regtype-float) fprs
          (aref masks interval-regtype-cr) crfs)))

(defun find-end-of-extended-call (seg call-vinsn)
  (declare (ignorable seg))
  ;; cheat
  (let* ((label (svref (vinsn-variable-parts call-vinsn) 0)))
    (vinsn-label-succ label)))
  
(defun extend-intervals-for-loops (seg)
  (let* ((fg (vinsn-list-flow-graph seg))
         (lregs (vinsn-list-lregs seg))
         (nregs (length lregs))
         (templates (backend-p2-vinsn-templates *target-backend*)))
    (declare (fixnum nregs) (type (vector t) lregs))
    (dolist (node fg)
      (let* ((live-in (fgn-live-in node)))
        (declare (simple-bit-vector live-in))
        (dotimes (i nregs)
          (when (eql 1 (sbit live-in i))
            (let* ((lreg (aref lregs i))
                   (interval (lreg-interval lreg))
                   (end (interval-end interval)))
              (unless (or (lreg-wired lreg) (lreg-local-p lreg))
                (dolist (pred (fgn-inedges node))
                  (let* ((xfer ())
                         (ref-vinsn ()))
                  
                  
                    (when (and (typep pred 'condnode)
                               (eq node (condnode-branchedge pred))
                               (setq xfer (condnode-condbranch pred))
                               (>  (vinsn-sequence xfer) end))
                      (setq ref-vinsn (select-vinsn 'ref templates (list lreg))))
                    
                    (when (and (not xfer)
                               (eq node (jumpnode-outedge pred))
                               (setq xfer (dll-header-last pred))
                               (>  (vinsn-sequence xfer) end))
                      (setq ref-vinsn (select-vinsn 'ref templates (list lreg))) )
                    (when ref-vinsn
                      (insert-vinsn-before ref-vinsn xfer)
                      (let*  ((refpos (vinsn-sequence ref-vinsn)))
                        (setf (interval-use-positions interval)
                              (append (interval-use-positions interval) (list refpos))
                              (interval-end interval) refpos)))))))))))))
                
              
                       
                            
              
            
          
                 
(defun build-interval-list (seg)
  (let* ((list (vinsn-list-intervals seg)) 
         (fg (vinsn-list-flow-graph seg))
         (nregs (length (vinsn-list-lregs seg))))
    (declare (fixnum nregs))
                 
         

    (dolist (block fg)
      (dolist (v (fgn-call-vinsns block))
        (let* ((end-vinsn v)
               (start-vinsn end-vinsn)
               (low (1+ (vinsn-sequence start-vinsn)))
               (high low)
               (killed (make-array 4)))
          (declare (simple-vector killed) )

          (when (vinsn-attribute-p v :extended-call)
            (setq high (vinsn-sequence (find-end-of-extended-call seg v))))
          (registers-killed-by-call v killed)
          (let* ((interval (make-interval nil low high nil nil)))
            (setf (interval-killed interval) killed)
            (vector-push-extend interval  list))
          )))
              
               
        
    (setf (vinsn-list-spill-area-used seg) (make-array nregs :element-type 'bit))

    (dovector (lreg (vinsn-list-lregs seg))
              
      (let* ((all (append (lreg-defs lreg) (lreg-refs lreg))))
        
        (when all
          
          (let* ((use-positions (sort (mapcar #'vinsn-sequence all) #'<))
                 (min (car use-positions))
                 (max (car (last use-positions)))
                 (class (lreg-class lreg))
                 (regtype (cond ((eql class hard-reg-class-fpr)
                                 interval-regtype-float)
                                ((eql class hard-reg-class-crf)
                                 interval-regtype-cr)
                                ((eql class hard-reg-class-gpr)
                                 (if (eql (lreg-mode lreg) hard-reg-class-gpr-mode-node)
                                   interval-regtype-node
                                   interval-regtype-imm)))))
            
            (let* ((interval (make-interval lreg min max regtype nil))
                   )
              
              (setf (lreg-interval lreg) interval)
              (setf (interval-use-positions interval)
                    use-positions)
              (when (logbitp lreg-pre-spill-bit (lreg-flags lreg))
                (when (interval-parent interval) (break))
                (process-pre-spilled-interval seg interval lreg (lreg-spill-offset lreg)))
              (vector-push-extend
               interval
               list))))))
    (extend-intervals-for-loops seg)
    (let* ((max (vinsn-list-max-seq seg)))
      (vector-push-extend (make-interval  nil max max -1 -1) list))
                    
                    
                    

                          
    (setf (vinsn-list-intervals seg)

          (sort list (lambda (x y)
                       (let* ((beginx (interval-begin x))
                              (beginy (interval-begin y)))
                         (or (< beginx beginy)
                             (and (= beginx beginy)
                                  (or (null (interval-lreg x))
                                      (lreg-local-p (interval-lreg x)))))))))))



(defun spill-vinsn-for-interval (interval)
  (let* ((regtype (interval-regtype interval)))
    (if (eql regtype interval-regtype-node)
      'spill
      (if (eql regtype interval-regtype-imm)
        'spill-natural
        (if (eql regtype interval-regtype-float)
          (case (fpr-mode-value-name (get-regspec-mode (interval-lreg interval)))
            (:double-float 'spill-double-float)
            (:single-float 'spill-single-float)
            (:complex-double-float 'spill-complex-double-float)
            (:complex-single-float 'spill-complex-single-float)))))))
  

(defun reload-vinsn-for-interval (interval)
  (let* ((regtype (interval-regtype interval)))
    (if (eql regtype interval-regtype-node)
      'reload
      (if (eql regtype interval-regtype-imm)
        'reload-natural
        (if (eql regtype interval-regtype-float)
          (case (fpr-mode-value-name (get-regspec-mode (interval-lreg interval)))
            (:double-float 'reload-double-float)
            (:single-float 'reload-single-float)
            (:complex-double-float 'reload-complex-double-float)
            (:complex-single-float 'reload-complex-single-float)))))))
          

(defun insert-vinsn-before (vinsn target)
  (let* ((target-seq (vinsn-sequence target))
         (pred-seq (1- target-seq))
         (pred (vinsn-pred target))
         (target-fgn (vinsn-fgn target)))
    (declare (fixnum target-seq pred-seq))
    (if (and (typep pred 'vinsn)
             (eql (vinsn-sequence pred) pred-seq))
      (insert-vinsn-before vinsn pred)
      (progn
        (insert-dll-node-before vinsn target)
        (setf (vinsn-sequence vinsn) pred-seq
             (vinsn-fgn vinsn) target-fgn)))))

(defun insert-vinsn-after (vinsn target)
  (let* ((target-seq (vinsn-sequence target))
         (succ-seq (1+ target-seq))
         (succ (vinsn-succ target))
         (target-fgn (vinsn-fgn target)))
    (declare (fixnum target-seq succ-seq))
    (if (and (typep succ 'vinsn)
             (eql (vinsn-sequence succ) succ-seq))
      (insert-vinsn-after vinsn succ)
      (progn
        (insert-dll-node-after vinsn target)
        (setf (vinsn-sequence vinsn) succ-seq
              (vinsn-fgn vinsn) target-fgn)))))

(defun note-reload (interval ref)
  (let* ((node (vinsn-fgn ref))
         (reloads (fgn-reloads node))
         (already (assoc interval reloads)))
    (if already
      (pushnew ref (cdr already))
      (push (cons interval (list ref)) (fgn-reloads node)))))      

(defun note-spill (interval  def)
  (let* ((node (vinsn-fgn def))
         (spills (fgn-spills node))
         (already (assoc interval spills)))
    (if already
      (pushnew def (cdr already))
      (push (cons interval (list def))  (fgn-spills node)))))
         


         
;;; treat incoming stack arguments as if they had
;;; been spilled to the stack.
(defun process-pre-spilled-interval (seg interval lreg offset)
  (linear-scan-bailout)
  (setf (interval-lreg interval) lreg
        (interval-spill-offset interval) offset)
  (let* ((used (vinsn-list-spill-area-used seg))
        (id (lreg-id lreg)))
    (setf (sbit used id) 1))
  (let* ((next-offset (1+ offset)))
    (when (> next-offset (vinsn-list-spill-base seg))
      (setf (vinsn-list-spill-base seg) next-offset))
    (setf (interval-flags interval) interval-flag-pre-spilled)
    

    '(dolist (ref (lreg-refs lreg))
      (note-reload interval ref)
      )
    '(dolist (def (lreg-defs lreg))
      (note-spill interval def)
      )))
                  

(defun spill-offset-for-interval (seg interval)
  (let* ((used (vinsn-list-spill-area-used seg))
         (base (vinsn-list-spill-base seg))
         (nregs (length (vinsn-list-lregs seg))))
    (or (interval-spill-offset interval)
        (setf (interval-spill-offset interval)
              (if (eql (interval-regtype interval) interval-regtype-node)
                (dotimes (i nregs)
                  (when (eql 0 (sbit used i))
                    (setf (sbit used i) 1)
                    (incf (vinsn-list-spill-depth seg))
                    (when (> (vinsn-list-spill-depth seg)
                             (vinsn-list-max-spill-depth seg))
                      (setf  (vinsn-list-max-spill-depth seg)
                             (vinsn-list-spill-depth seg)))
                    (return (+ i base))))
                (prog1 (vinsn-list-nfp-spill-offset seg)
                  (incf (vinsn-list-nfp-spill-offset seg) 16)
                  (when (> (vinsn-list-nfp-spill-offset seg)
                           (vinsn-list-max-nfp-spill-depth seg))
                    (setf (vinsn-list-max-nfp-spill-depth seg)
                          (vinsn-list-nfp-spill-offset seg))))))))) 


(defun interval-containing-vinsn (interval vinsn)
  (let* ((seq (vinsn-sequence vinsn)))
    (if (and (>= seq (interval-begin interval))
             (<= seq (interval-end interval)))
      interval
      (let* ((child (interval-child interval)))
        (if child (interval-containing-vinsn child vinsn))))))

          
         
;;; Return the first use of INTERVAL within BLOCK and the spanning interval
(defun first-use-of-interval-in-block (seg interval block)
  (multiple-value-bind (start end) (find-extended-block-bounds block)
    (declare (fixnum start end))
    (do* ((i interval (interval-child i)))
         ((null i) (values nil nil))
      (dolist (use (interval-use-positions i))
        (declare (fixnum use))
        (if (and (>= use start)
                 (< use end))
          (return-from first-use-of-interval-in-block (values i (find-vinsn seg use))))))))

;; Harder
(defun last-use-of-interval-in-block (seg interval block)
  (multiple-value-bind (start end) (find-extended-block-bounds block)
    (declare (fixnum start end))
    (let* ((child nil)
           (last-use))
      (do* ((i interval (interval-child i)))
           ((null i) (values child (if last-use (find-vinsn seg last-use))))
        (dolist (use (interval-use-positions i))
          (declare (fixnum use))
          (if (and (>= use start)
                   (< use end))
            (setq child i last-use use)))))))
  
         
    
(defun end-of-fgn-containing (vinsn)
  (let* ((fgn (vinsn-fgn vinsn)))
    (vinsn-sequence (dll-header-last fgn))))

(defparameter *bailout-on-spill* t)

(defun spill-and-split-interval (seg why parent new-end vector list)
  (when *bailout-on-spill*
    (linear-scan-bailout (format nil "need to spill due to ~a" why)))

  (let* ((lreg (interval-lreg parent)))
    (unless lreg (break "no lreg for interval ~s" parent))
    
    (let* ((used (vinsn-list-spill-area-used seg))
           (base (vinsn-list-spill-base seg)))
      (declare (simple-bit-vector used)
               (fixnum base))
      (let* ((nregs (length (vinsn-list-lregs seg))))
        (declare (fixnum nregs))
        (let* ((offset (or (interval-spill-offset parent)
                           (setf (interval-spill-offset parent) 
                                 (if (eql (interval-regtype parent) interval-regtype-node)
                                   (dotimes (i nregs)
                                     (when (eql 0 (sbit used i))
                                       (setf (sbit used i) 1)
                                       (incf (vinsn-list-spill-depth seg))
                                       (when (> (vinsn-list-spill-depth seg)
                                                (vinsn-list-max-spill-depth seg))
                                         (setf  (vinsn-list-max-spill-depth seg)
                                                (vinsn-list-spill-depth seg)))
                                       (return (+ i base))))
                                   (prog1 (vinsn-list-nfp-spill-offset seg)
                                     (incf (vinsn-list-nfp-spill-offset seg) 16)
                                     (when (> (vinsn-list-nfp-spill-offset seg)
                                              (vinsn-list-max-nfp-spill-depth seg))
                                       (setf (vinsn-list-max-nfp-spill-depth seg)
                                             (vinsn-list-nfp-spill-offset seg))))))))
               (child-used (member-if (lambda (pos) (> pos new-end)) (interval-use-positions parent)))
               (ncu (length child-used)))
          '(let* ((defs (lreg-defs lreg)))
           (when (cdr defs)  (linear-scan-bailout (format nil "not yet - assignment/multiple definitions in spilled interval ~s" defs))))
          

          (let* ((min (car child-used))
                 (max (car (last child-used))))
            (let* ((child (make-interval  lreg min max (interval-regtype parent) nil  )))
              (setf (interval-parent child) parent
                    (interval-child parent) child
                    (interval-spill-offset child) offset
                    (interval-flags child) (interval-flags parent)
                    (interval-use-positions child) child-used
                    (interval-use-positions parent) (butlast (interval-use-positions parent) ncu))
              (do-dll-nodes (r list (error "no next interval"))
                (when (> (interval-begin r) min)
                  (insert-dll-node-before child r)
                  (rebuild-interval-vector seg vector  child r)    
                  (return)))
                


              ;; Ready to expire
              (setf (interval-end parent) (car (last (interval-use-positions parent)))))))))))

(defun assign-interval-indices (vector)
  (declare (type (vector t) vector))
  (dotimes (i (length vector))
    (setf (interval-idx (aref vector i)) i)))

(defun rebuild-interval-vector (seg vector new-element succ)
  (declare (type (vector t) vector))
  (let* ((idx (interval-idx succ)))
    (declare (fixnum idx) (ignorable idx))
    (let* ((n (length vector)))
      (declare (Fixnum n) (ignorable n))
      (progn
      (vector-push-extend new-element vector)
      (setf (vinsn-list-intervals seg)
      (stable-sort vector (lambda (x y)
                       (let* ((beginx (interval-begin x))
                              (beginy (interval-begin y)))
                         (or (< beginx beginy)
                             (and (= beginx beginy)
                                  (or (null (interval-lreg x))
                                      (lreg-local-p (interval-lreg x))))))))))
      #+no
      (progn
      (vector-push-extend nil vector)   ; make room
      
      (do* ((j n (1- j))
            (i (1- j) (1- i)))
           ((= j idx)
            (setf (aref vector idx) new-element)
)

        (declare (fixnum i j))
                                
        (setf (aref vector j) (aref vector i))
        ))
      (assign-interval-indices vector)
      )))
            

(defun replace-vinsn-operands (vinsn old new start end)
  (declare (fixnum start end))
  (let* ((seq (vinsn-sequence vinsn)))
    (declare (fixnum seq))
    (unless (or (< seq start) (> seq end))
      (let* ((v (vinsn-variable-parts vinsn)))
        (declare (simple-vector v))
        (dotimes (i (length v))
          (when (eq old (svref v i))
            (setf (svref v i) new)))))))


(defun expire-interval (seg interval)
  (let* ((avail (vinsn-list-available-physical-registers seg))
         (used (vinsn-list-spill-area-used seg))
         (preg (interval-preg interval)))
    (declare (simple-vector avail) (simple-bit-vector used) (ignorable used))
    (flet ((unuse-reg (regno type)
             ;;(ls-format "~&unuse ~d/~d for ~s" regno type interval)
             (setf (svref avail type)
                   (logior (svref avail type) (ash 1 regno)))
))

      (when preg
        (unuse-reg preg (interval-regtype interval))))
    ;; we have to retain the (shared) spill slot until the last
    ;; child expires.
    )
  (postprocess-interval interval)
  )
        
(defun postprocess-interval (interval)
  (let*  ((lreg (interval-lreg interval))
          (preg (interval-preg interval))
          (start (interval-begin interval))
          (end (interval-end interval)))
    (declare (ignorable start end))
    (when lreg
      (let* ((defs (lreg-defs lreg)))
        (when (and defs (null (cdr defs)) (vinsn-attribute-p (car defs) :trivial-copy))
          (setf (interval-trivial-def interval) (car defs)))))
    (when (and lreg preg)
      (if (eql 0 (interval-flags interval))
        (setf (lreg-value lreg) preg)
        (progn
          (dolist (def (lreg-defs lreg))
            (replace-vinsn-operands def lreg preg start end))
          (dolist (ref (lreg-refs lreg))
            (replace-vinsn-operands ref lreg preg start end)))))))

                       
                       
  
;;; try to pick an interval whose next use is farthest away.
(defun find-spill-candidate (intervals regtype  at)
  (let* ((max at) (best nil))
    (do-dll-nodes (interval intervals (or best (progn (break)(linear-scan-bailout "no interval to spill"))))
      (let* ((lreg (interval-lreg interval)))
        (unless (or (lreg-wired lreg) (lreg-local-p lreg) (not (eql regtype (interval-regtype interval))))
          (let*  ((nextuse (member-if  (lambda (x) (> x at)) (interval-use-positions interval))))
            (when (and nextuse (> (car nextuse) max))
              (setq max (car nextuse) best interval))))))
    best))
       

(defun pregs-used-in-intervals (intervals)
  (let* ((mask 0))
    (declare (fixnum mask))
    (dolist (interval intervals mask)
      (let* ((preg (interval-preg interval)))
        (declare (type (mod 16) preg))
        (setq mask (logior mask (ash 1 preg)))))))


(defun linear-scan (seg )
  (let* ((avail (vinsn-list-available-physical-registers seg)))
    (flet ((use-reg (regno type i)
             (declare (ignorable i))t
             ;;(ls-format "~& using ~s/~d in ~s" regno type i)
             (setf (svref avail type)
                   (logandc2 (svref avail type) (ash 1 regno))))

           (select-available-register (mask)
             (declare (type (unsigned-byte 16) mask))
             (unless (eql 0 mask)
               (do* ((i 0 (1+ i)))
                    ((> i 15))
                 (when (logbitp i mask) (return i)))))
           (select-available-register-high (mask)
             (declare (type (unsigned-byte 16) mask))
             (unless (eql 0 mask)
               (do* ((i 15 (1- i)))
                    ((< i 0))
                 (when (logbitp i mask) (return i))))))
      
      (let* ((intervals (vinsn-list-intervals seg)))
        (declare (type (vector t) intervals))
        (let* ((active (make-dll-header))
               (unhandled (make-dll-header))
               ;;(expired (make-dll-header))
               (limit (vinsn-list-max-seq seg)))
          (assign-interval-indices intervals)
          (dotimes (i (length intervals))
            (let*  ((interval (aref intervals i)))
              (append-dll-node interval unhandled)))
          (do* ((i (pop-dll-node unhandled) (pop-dll-node unhandled))
                (begin (if i (interval-begin I) limit) (if i (interval-begin I) limit)))
               ((= begin limit) (progn (do-dll-nodes (a active ) (expire-interval seg a )) t   ))
            (ls-format  "~&i=~s" i)


            (do-dll-nodes (other active)
              (let* ((other-end (interval-end other)))
                (when (< other-end begin)
                  (remove-dll-node other)
                  (expire-interval seg other ))))

            
            (if (null (interval-lreg i))
              (let* ((caller-save ())
                     )
                (do-dll-nodes (a active)
                  (when (>= (interval-end a) (interval-end i))
                    ;; should see if preg is in the killed set
                    (push a caller-save)))
                (ls-note "caller-save = ~s call @ ~s" caller-save begin)
                (dolist (cs caller-save)
                  (spill-and-split-interval    seg 'call cs begin intervals unhandled)
                  )
                  
                         
                )
              (progn
                (do-dll-nodes (live active)
                  (when (eql (interval-regtype i) (interval-regtype live))
                    (push live (interval-active-before i))
                    (push i (interval-active-after live))))
              (let* ((regtype (interval-regtype i))
                     (mask (svref avail regtype))
                     (idx (interval-idx i)))
                (setf (interval-avail i) mask)
                (when (eql 0 mask)
                  (let* ((victim (find-spill-candidate active regtype begin)))
                    (ls-break)
                    (progn (spill-and-split-interval   seg 'pressure victim begin intervals unhandled) (expire-interval seg victim ) (setq mask (svref avail regtype)) (when (eql mask 0) (break "mask is still 0 after spilling ~s" victim)))))
                                 


                (let* ((lreg (interval-lreg i))
                       (regtype (interval-regtype i))
                       (mask (svref avail regtype)))
                  (let* ((fixed (interval-preg i))
                         (targeted (and lreg (or (lreg-wired lreg) (lreg-local-p lreg)) (lreg-value lreg)))
                         (preg (or fixed (if (and targeted (logbitp targeted mask))
                                           targeted
                                           (select-available-register-high mask)))))



                    (when (and fixed (not (logbitp fixed mask)))
                      (let* ((other (do-dll-nodes (x active (error "can't find interval with ~d" fixed))
                                      (when (and (eql regtype (interval-regtype x))
                                                 (eql fixed (interval-preg x))
                                                 (interval-lreg x))
                                      
                                        (return x)))))
                        (spill-and-split-interval seg 'conflict other begin intervals unhandled)))

                    (when (and targeted (not (eql targeted preg)))

                      (let*  ((rival (do-dll-nodes (other active (error "can't find rival on active-list"))
                                       (when (and (eql (interval-preg other) targeted)
                                                  (eql (interval-regtype other) regtype)
                                                  )
                                         (return other))))
                              (rival-lreg (and rival (interval-lreg rival)))
                              )

                                        
                        (when (> (interval-idx rival) idx) (break "???"))
                        (ls-format "~&want to use reg ~d, for ~s in use by ~s. ~d may be free" targeted lreg rival-lreg preg)
                        (cond ((null rival-lreg) (break "no lreg for conflicting interval ~s" rival))
                              ((or (lreg-wired rival-lreg) (lreg-local-p rival-lreg))
                               (if (or (and (lreg-wired lreg)
                                            (lreg-wired rival-lreg))
                                       (eql (interval-end rival) begin)
                                       (null (lreg-refs rival-lreg))
                                       (null (lreg-refs lreg)))
                                 (setq preg targeted)
                                 (error "conflicting intervals overlap")))

                              ((eql (interval-end rival) begin)
                               (setq preg targeted))
                              (rival
                               (when (or (lreg-wired rival-lreg)
                                         (lreg-local-p rival-lreg))
                                 (break "bad idea"))
                               (do* ((rival-idx (interval-idx rival) (1+ rival-idx))
                                     (q rival (aref intervals rival-idx))
                                     (rival-avail (interval-avail q) (logand rival-avail (if (eql regtype (interval-regtype q)) (interval-avail q) -1))))
                                    ((= rival-idx idx)
                                     (if (eql rival-avail 0)
                                       ;; we made an unfortunate choice when we
                                       ;; assigned rhe register we want now  to
                                       ;; the rival interval, and can't back out
                                       ;; of that choice.  copy the rival's
                                       ;; preg to something that is now free and
                                       ;; split the rival.
                                       (progn


                                         (spill-and-split-interval seg 'conflict2 rival begin intervals unhandled))
                                       (let*  ((other-preg (select-available-register-high rival-avail)))
                                         ;;(ls-format "should have used ~d" other-preg)
                                         (use-reg other-preg regtype rival)
                                     
                                         (setf (interval-preg rival) other-preg)
                                         (do* ((qidx (1+ (interval-idx rival)) (1+ qidx)))
                                              ((= qidx idx)
                                               (setf (svref avail regtype)
                                                     (logior (svref avail regtype)
                                                             (ash 1 targeted))))
                                           (let* ((q (aref intervals qidx)))
                                             (when (eql (interval-regtype q) regtype)
                                               (setf (interval-avail q)
                                                     (logandc2 (interval-avail q)
                                                               (ash 1 other-preg)))))))))
                                 (setq preg targeted))))))


                    (use-reg preg regtype i)
                    (setf (interval-preg i) preg)
                    (append-dll-node i active))))))))))))

;;; we don't need to do nearly as much of this as we have been doing.
(defun process-spills-and-reloads (fg)
  (let* ((templates (backend-p2-vinsn-templates *target-backend*)))
    (dolist (node fg)
      (let* ((spills (fgn-spills node))
             (reloads (fgn-reloads node)))
        (dolist (s spills)
          (destructuring-bind (i . defs) s
            (dolist (def defs)
              (let* ((preg (interval-preg i))
                     (offset (interval-spill-offset i))
                     (spill-vinsn (select-vinsn (spill-vinsn-for-interval i) templates (list preg offset))))
                (insert-vinsn-after spill-vinsn def)))))
        (dolist (r reloads)
          (destructuring-bind (i . refs) r
            (dolist (ref refs)

              (let* ((preg (interval-preg i))
                     (offset (interval-spill-offset i))
                     (reload-vinsn (select-vinsn (reload-vinsn-for-interval i) templates (list preg offset))))
                (insert-vinsn-before reload-vinsn ref)))))))))

  

(defun resolve-split-intervals (seg)
  (let* (
         (lregs (vinsn-list-lregs seg))
         (nregs (length lregs)))

               
    (dovector (lreg lregs )
      (let* ((interval (lreg-interval lreg))
             (offset (if interval (interval-spill-offset interval))))
       (when offset
         (dolist (use (interval-use-positions interval))
o           (unless (and (eql use (interval-begin interval))
                        (null (interval-parent interval)))
             (let* ((vinsn (find-vinsn seg use)))
               (when (memq vinsn (lreg-defs lreg))
                 (note-spill interval vinsn)))))
         (do* ((child (interval-child interval) (interval-child child)))
              ((null child))
           (dolist (use (interval-use-positions child))
             (let* ((def (find-vinsn seg use)))
               (when (memq def (lreg-defs lreg))
                 (note-spill child def)))))
         
         (if (eql 0 (interval-flags interval))
          (do* ((child (interval-child interval) (interval-child child)))
               ((null child))
            (let* ((parent (interval-parent child))

                   (parent-end-vinsn (find-vinsn seg (interval-end parent)))
                   (child-start-vinsn (find-vinsn seg (interval-begin child))))
              (note-reload child child-start-vinsn)
              (note-spill parent parent-end-vinsn)))
           (let* ((family ())
                  (lreg (interval-lreg interval)))
             (push interval family)
             (do* ((child (interval-child interval) (interval-child child)))
                  ((null child))
               (push child family))
             (dolist (x family)
               (dolist (use (interval-use-positions x))
                 (let* ((v (find-vinsn seg use)))
                   (when (memq v (lreg-refs lreg))
                   (note-reload x v)))))
           )))))

    
    (dolist (from (vinsn-list-flow-graph seg))
      (dolist (to (fgn-outedges from))
        (unless (eq to (fgn-extended-succ from))

          (let* ((live-in (fgn-live-in to)))
            (declare (simple-bit-vector live-in))
            (dotimes (i nregs)
              (when (= (sbit live-in i) 1)
                (let* ((interval (lreg-interval (aref lregs i)))
                       (offset (interval-spill-offset interval)))
                     
                  (when offset

                    (multiple-value-bind (to-interval to-vinsn)
                        (first-use-of-interval-in-block seg interval to)
                      (multiple-value-bind (from-interval from-vinsn)
                          (last-use-of-interval-in-block seg interval from)

                        (when (not (eq from-interval to-interval))
                          (when from-vinsn (note-spill from-interval from-vinsn))
                          (when to-vinsn(note-reload to-interval to-vinsn)))))))))))))))
                             
                         
                           
                       
            
                   
                 
            

(defstatic *linear-scan-won* 0)
(defstatic *linear-scan-lost* 0)
(defparameter *report-linear-scan-success* nil)


(defun linear-scan-bailout (&optional (reason "generic failure" reason-p))
  (when *backend-use-linear-scan*
    (when (and reason-p *report-linear-scan-success*)
      (format *error-output* "~%~%bailing-out of linear-scan for ~a :~&~&~a" *current-function-name* reason ))
    (incf *linear-scan-lost*)
    (signal 'linear-scan-bailout)))


;;; This is not "unsafe".  it may affect debugging and error
;;;  reporting, but so do other things in the new backend, and
;;; we have already decided that those things (reducing stack
;; access) are important.
(defun try-to-omit-frame-pointer (seg)
  (declare (ignorable seg))
  #+x86-target
  (when t
    (let* ((uses ()))
      (when
          (do-dll-nodes (v seg t)
            (when (vinsn-attribute-p v :needs-frame-pointer)
              (return nil))
            (if (vinsn-attribute-p v :uses-frame-pointer)
              (push v uses)))
        (dolist (v uses t) (elide-vinsn v))))))

(defun merge-adjacent-labels (header)
  
  (let* ((labels (collect ((labs)) 
                   (do-dll-nodes (v header)
                     (when (vinsn-label-p v) (labs v)))
                   (labs))))
    (do* ((repeat t))
         ((not repeat))
      (setq repeat nil 
            labels (delete-unreferenced-labels labels))
      (dolist (l labels)
        (let* ((succ (vinsn-label-succ l)))
          (when (vinsn-label-p succ)
            (backend-merge-labels l succ)
            (setq repeat t)
            (return)))))))



;;; Intervals X and Y overlap if X begins before Y ends
;;; and X ends after Y begins.  If two intervals overlap,
;;; they can't use the same physical register.
;;; "intervals" is a vector ordered by start address. and we
;;; might be able to avoid linear search here by doing
;;; two binary searches of two ordered vectors.
(defun find-conflicting-intervals (interval preg)
  (declare (type (unsigned-byte 4) preg))
  (let* ((conflicts ()))
    (dolist (after (interval-active-after interval) conflicts)
      (when (eql preg (interval-preg after))
        (push after conflicts)))))

(defun other-pregs-for-conflicting-interval (i other-mask)
  (declare (fixnum other-mask))
  (let* ((mask (interval-avail i)))
    (declare (fixnum mask))
    (dolist (after (interval-active-after i) (logandc2 mask other-mask))
      (setq mask (logand mask (interval-avail after))))))


(defun pregs-used-before (interval)
  (let* ((mask 0))
    (declare (fixnum mask))
    (dolist (before (interval-active-before interval) mask)
      (let* ((preg (interval-preg before)))
        (setq mask (logior (ash 1 preg) mask))))))

 
      


(defun rebuild-avail-before (seg)
  (let* ((intervals (vinsn-list-intervals seg)))
    (declare (type (vector t) intervals))
    (dovector (i intervals)
      (when (interval-lreg i)
          
        (let* ((avail (svref (vinsn-list-available-physical-registers seg) (interval-regtype i)))
               (used (pregs-used-before i)))
          (declare (fixnum avail used))
          (setf (interval-avail i) (logandc2 avail used)))))))


;;; Choose another physical register for interval
                
(defun resolve-interval-conflict (interval)
  (let* ((mask 0))
    (declare (fixnum mask))
    (dolist (other (interval-conflicts-with interval))
      (setq mask (logior mask (ash 1 (interval-preg other)))))
    (block resolve
      (let* ((avail (logandc2 (interval-avail interval) mask)))
        (declare (fixnum avail))
        (do* (( i 16 (1- i)))
             ((< i 0))
          (declare (fixnum i))
          (let* ((preg (1- i)))
            (declare (type (integer 0 15) preg))
            (when (and (logbitp preg avail)
                       (not (find-conflicting-intervals interval preg)))
              (let* ((lreg (interval-lreg interval)))
                (setf (lreg-value lreg) preg
                      (interval-preg interval) preg))
            (return-from resolve preg))))))))



                                            
(defun nullify-trivial-copy (vinsn resolve)
  (when (vinsn-attribute-p vinsn :trivial-copy)
    (let* ((vp (vinsn-variable-parts vinsn))
           (dest (svref vp 0))
           (src (svref vp 1)))
      ;; if both src and dest are lregs, dest
      ;; is not fixed, and doing so would not
      ;; introduce any conflicts throughout
      ;; the lifetime of dest, make the copy
      ;; a nop and change uses of dest to use
      ;; src directly
      ;; we are considering changing uses
      ;; of "dest" to use the same preg
      ;; as "src" does.  some other interval(s)
      ;; which did not conflict with "dest"
      ;; during register allocation may do
      ;; so now (if we back out of the copy)
      ;; if we find any such conflicting
      ;; intervals (which try to use the preg
      ;; from the src interval. change the
      ;; conflicting interval to use another
      ;; preg if we can.
      (when (and (typep src 'lreg)
                 (typep dest 'lreg))
        (let* ((src-interval (lreg-interval src))
               (dest-interval (lreg-interval dest))
               (src-preg (interval-preg src-interval))
               (dest-preg (interval-preg dest-interval)))
          (declare (type (unsigned-byte 4) src-preg dest-preg))
          (when (and resolve (interval-non-trivial-conflicts dest-interval))
            (dolist (conflict (interval-non-trivial-conflicts dest-interval) )
              (resolve-interval-conflict conflict)
              (setf (interval-conflicts-with conflict) nil)))
          (unless (or (eql src-preg dest-preg)
                      (lreg-local-p dest)
                      (lreg-wired dest))

            (when (not resolve)
              (dolist (i (find-conflicting-intervals dest-interval src-preg))
                (unless (or (eq i src-interval) (interval-trivial-def i))
                  (push dest-interval (interval-conflicts-with i))
                  (push i (interval-non-trivial-conflicts  dest-interval)))))
            (setf (interval-preg dest-interval)src-preg
                  (lreg-value dest) src-preg
                  (svref vp 0) src)
            t))))))

(defparameter *remove-trivial-copies* nil)

;; see postprocess-interval; this assumes that all trivial-copy operands
;; are lregs.
(defun remove-trivial-copies (seg)
  (declare (ignorable seg))
  (when *remove-trivial-copies*
    (let* ((intervals (vinsn-list-intervals seg)))
      (declare (type (vector t) intervals))
      (dovector (i intervals)
        (when (interval-lreg i)
          (setf (interval-alt-preg i) (interval-preg i))
          (unless (and (logbitp (interval-preg i) (interval-avail i))
                       (dolist (after (interval-active-after i) t)
                         (when (logbitp (interval-preg i) (interval-avail after))
                           
                           (return nil)))))))
      
                                                                  

    (rebuild-avail-before seg)
    (dolist (block (vinsn-list-flow-graph seg))
      (do-tail-dll-nodes (v block)
        (when (vinsn-attribute-p v :trivial-copy)

          (nullify-trivial-copy v nil))))
    (rebuild-avail-before seg)
    (dolist (block (vinsn-list-flow-graph seg))
      (do-tail-dll-nodes (v block)
        (when (vinsn-attribute-p v :trivial-copy)
          (nullify-trivial-copy v t))))
  
    )))




(defun optimize-vinsns (header)

  
  ;; Delete unreferenced labels that the compiler might have emitted.
  ;; Subsequent operations may cause other labels to become
  ;; unreferenced.
  (let* ((regs (vinsn-list-lregs header)))

    (merge-adjacent-labels header)
    ;; Look for pairs of adjacent, referenced labels.
    ;; Merge them together (so that one of them becomes unreferenced.)
    ;; Repeat the process until no pairs are found.

    (maximize-jumps header)
    (eliminate-dead-code header) 
    (cond (*backend-use-linear-scan*
           (let* ((size (dll-header-length header)))
             (when (> size 10000)
               (linear-scan-bailout "function size exceeds compiler limitation"))
             )
           (normalize-vinsns header)
           (let* ((fg (create-flow-graph header))
                  (seq 0))
             (declare (fixnum seq))
             (dolist (node fg (setf (vinsn-list-max-seq header) seq))
               (do-dll-nodes (v node)
                 (when (typep v 'vinsn)
                   (setf (vinsn-fgn v) node
                         (vinsn-sequence v) (incf seq 5)))))
             (compute-live-sets fg header)
             (setf (vinsn-list-available-physical-registers header)
                   (vector *backend-node-temps*
                           *backend-imm-temps*
                           *backend-fp-temps*
                           *backend-crf-temps*))
             (build-interval-list header)
             #+x86-target
             (when *linear-scan-verbose*
               (ls-format "~&**********************************************~s" (afunc-name *x862-cur-afunc*))
               (dolist (n fg )
                 (terpri)
                 (show-fgn n (vinsn-list-lregs header))))
             (unless (linear-scan header )
               (linear-scan-bailout "register allocation failed"))

             (remove-trivial-copies header)
             (when *report-linear-scan-success*
               (format *debug-io*  "~&;; Won on ~a" *current-function-name*))
             (incf *linear-scan-won*)
             (resolve-split-intervals header)
             (process-spills-and-reloads fg)
             (when *linear-scan-verbose*
               (dolist (n fg )
                 (terpri)
                 (show-fgn n regs)))

             (linearize-flow-graph fg header)
             (try-to-omit-frame-pointer header)
             t))
          (t t))))

(defun find-vinsn (seg seq)
  (flet ((find-vinsn-in-node (node seq)
           (do-tail-dll-nodes (v node)
             (when (typep v 'vinsn)
               (let*  ((vseq (vinsn-sequence v)))
                 (if (eq vseq seq)
                   (return v)))))))
    (dolist (node (vinsn-list-flow-graph seg))
      (let* ((lastv (dll-header-last node)))
        (when (>= (vinsn-sequence lastv) seq)
          (return (find-vinsn-in-node node seq)))))))

(defun show-vinsns (vinsns indent)
 (do-dll-nodes (n vinsns)
  (ls-format "~&~v@t~s" indent n)))

(defun show-vinsn-range (first last indent)
 (do* ((vinsn first (dll-node-succ vinsn)))
    ()
  (ls-format "~&~v@t~s" indent vinsn)
  (when (eq vinsn last) (return))))


(defun show-fgn (node regs &optional (show-live-sets t))
  (declare (ignorable regs))
  (format t "~&~s (~d) {~a}" (type-of node) (fgn-id node) (mapcar #'fgn-id (fgn-inedges node)))
  (show-vinsns node 2)
  (when show-live-sets
    (show-live-set "live-kill" (fgn-live-kill node) regs)
    (show-live-set "live-gen " (fgn-live-gen node) regs)
    (show-live-set "live-in " (fgn-live-in node) regs)
    (show-live-set "live-out " (fgn-live-out node) regs))
 
  (terpri)
  (terpri))

(defun show-live-set (herald bits regs)
 (format t "~&~a" herald)
 (collect ((reg))
  (dotimes (i (length regs) (format t " ~s" (reg)))
   (let* ((lreg (aref regs i)))
    (when (eql 1 (sbit bits i))
     (reg lreg))))))
                   
 

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
	    (funcall process-after node)))
          )
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

    
(defun last-vinsn-unless-label (seg)
  ;; Look at the last element(s) of seg. If a vinsn-note,
  ;; keep looking. If a vinsn, return it; if a vinsn-label,
  ;; return nil
  (do* ((element (dll-header-last seg) (dll-node-pred element)))
       ((eq element seg))
    (etypecase element
      (vinsn (return element))
      (vinsn-label (if (typep (vinsn-label-id element) 'fixnum)
                     (return nil))))))
    



;;; This generally only gives a meaningful result if pass 2 of the
;;; compiler has been compiled in the current session.
;;; TODO (maybe): keep track of the "expected missing vinsns" for
;;; each backend, call this function after compiling pass 2. That's
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
