;;;-*- Mode: Lisp; Package: CCL -*-
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

(eval-when (:compile-toplevel :execute)
  (require "ARCH"))

(defconstant platform-word-size-mask 64)
(defconstant platform-os-mask 7)
(defconstant platform-cpu-mask (logandc2 (1- platform-word-size-mask)
                                         platform-os-mask))
(defconstant platform-word-size-32 0)
(defconstant platform-word-size-64 64)
(defconstant platform-cpu-ppc (ash 0 3))
(defconstant platform-cpu-sparc (ash 1 3))
(defconstant platform-cpu-x86 (ash 2 3))
(defconstant platform-cpu-arm (ash 3 3))
(defconstant platform-os-vxworks 0)
(defconstant platform-os-linux 1)
(defconstant platform-os-solaris 2)
(defconstant platform-os-darwin 3)
(defconstant platform-os-freebsd 4)
(defconstant platform-os-windows 5)
(defconstant platform-os-android 6)

(defun backend-real-lowmem-bias (backend)
  (let* ((b (backend-lowmem-bias backend)))
    (if (atom b) b (car b))))



(defstruct backend
  (name :a :type keyword)
  (num-arg-regs 3 :type fixnum)    ; number of args passed in registers
  (num-nvrs 0 :type fixnum)        ; number of callee-save node regs
  (num-node-regs 0 :type fixnum)     ; number of node temps/arg regs
  (lap-opcodes #() :type simple-vector)
  (lookup-opcode #'false :type (or symbol function))
  (lookup-macro #'false :type (or symbol function))
  (p2-dispatch #() :type simple-vector)
  (p2-compile 'error :type (or symbol function))
  (p2-vinsn-templates (error "Missing arg") :type hash-table)
  (p2-template-hash-name 'bogus :type symbol)
  (target-specific-features () :type list)
  (target-fasl-pathname "" :type (or string pathname))
  (target-platform 0 :type fixnum)
  (target-os ())
  (target-arch-name nil :type symbol)
  (target-foreign-type-data nil :type (or null foreign-type-data))
  (lap-macros nil)
  (target-arch nil)
  (define-vinsn nil)
  (platform-syscall-mask 0)
  (define-callback nil)
  (defcallback-body nil)
  (lisp-context-register 0)
  ;; difference between canonical static address for arch and this
  ;; target's. Usually 0.
  ;; Can be a cons of (static-area-bias . subprims-bias)
  (lowmem-bias 0))



(defmethod print-object ((b backend) s)
  (print-unreadable-object (b s :type t :identity t)
    (format s "~A" (backend-name b))))


(defun target-os-name (&optional (backend *target-backend*))
  (cdr (assoc (logand platform-os-mask (backend-target-platform backend))
              *platform-os-names*)))


(defparameter *backend-node-regs* 0)
(defparameter *backend-node-temps* 0)
(defparameter *available-backend-node-temps* 0)
(defparameter *backend-imm-temps* 0)
(defparameter *available-backend-imm-temps* 0)
(defparameter *backend-fp-temps* 0)
(defparameter *available-backend-fp-temps* 0)
(defparameter *backend-crf-temps* 0)
(defparameter *available-backend-crf-temps* 0)
(defparameter *backend-allocate-high-node-temps* nil)

(defparameter *mode-name-value-alist*
  '((:lisp . 0)
    (:u32 . 1)
    (:s32 . 2)
    (:u16 . 3)
    (:s16 . 4)
    (:u8 . 5)
    (:s8 . 6)
    (:address . 7)
    (:u64 . 8)
    (:s64 . 9)))

(defun gpr-mode-name-value (name)
  (if (eq name :natural)
    (setq name
          (target-word-size-case
           (32 :u32)
           (64 :u64)))
    (if (eq name :signed-natural)
      (setq name
          (target-word-size-case
           (32 :s32)
           (64 :s64)))))
  (or (cdr (assq name *mode-name-value-alist*))
      (error "Unknown gpr mode name: ~s" name)))

(defparameter *mode-specifier-types*
  (vector
   (specifier-type t)                   ;:lisp
   (specifier-type '(unsigned-byte 32)) ;:u32
   (specifier-type '(signed-byte 32))   ;:s32
   (specifier-type '(unsigned-byte 16)) ;:u16
   (specifier-type '(signed-byte 16))   ;:s16
   (specifier-type '(unsigned-byte 8))  ;:u8
   (specifier-type '(signed-byte 8))    ;:s8
   (specifier-type 'macptr)             ;:address
   (specifier-type '(unsigned-byte 64)) ;:u64
   (specifier-type '(signed-byte 64)))) ;:s64

(defun mode-specifier-type (mode-name)
  (svref *mode-specifier-types* (gpr-mode-name-value mode-name)))
   

(defun use-node-temp (n)
  (declare (fixnum n))
  (if (logbitp n *available-backend-node-temps*)
    (setq *available-backend-node-temps*
	  (logand *available-backend-node-temps* (lognot (ash 1 n)))))
  n)

(defun node-reg-p (reg)
  (and (= (hard-regspec-class reg) hard-reg-class-gpr)
       (= (get-regspec-mode reg) hard-reg-class-gpr-mode-node)))

(defun imm-reg-p (reg)
  (and (= (hard-regspec-class reg) hard-reg-class-gpr)
       (/= (get-regspec-mode reg) hard-reg-class-gpr-mode-node))) 

(defun node-reg-value (reg)
  (if (node-reg-p reg)
    (hard-regspec-value reg)))

; if EA is a register-spec of the indicated class, return
; the register #.
(defun backend-ea-physical-reg (ea class)
  (declare (fixnum class))
  (and ea
       (register-spec-p ea)
       (= (hard-regspec-class ea) class)
       (hard-regspec-value ea)))

(defun backend-crf-p (vreg)
  (backend-ea-physical-reg vreg hard-reg-class-crf))

(defun %available-node-temp (mask)
  (unless (eql 0 mask)
    (if *backend-allocate-high-node-temps*
      (do* ((bit 31 (1- bit)))
           ((< bit 0))
        (when (logbitp bit mask)
          (return bit)))    
      (dotimes (bit 32)
        (when (logbitp bit mask)
          (return bit))))))

(defun available-node-temp (mask)
  (or (%available-node-temp mask)
      (error "Bug: ran out of node temp registers.")))


(defun ensure-node-target (reg)
  (if (node-reg-p reg)
    reg
    (available-node-temp *available-backend-node-temps*)))

(defun select-node-temp ()
  (let* ((mask *available-backend-node-temps*))
    (if *backend-allocate-high-node-temps*
      (do* ((bit 31 (1- bit)))
           ((< bit 0) (error "Bug: ran out of node temp registers."))
        (when (logbitp bit mask)
          (setq *available-backend-node-temps* (bitclr bit mask))
          (return bit)))
      (dotimes (bit 32 (error "Bug: ran out of node temp registers."))
        (when (logbitp bit mask)
          (setq *available-backend-node-temps* (bitclr bit mask))
          (return bit))))))

(defun available-imm-temp (mask &optional (mode-name :natural))
  (dotimes (bit 32 (error "Bug: ran out of imm temp registers."))
    (when (logbitp bit mask)
      (return (set-regspec-mode bit (gpr-mode-name-value mode-name))))))

(defun use-imm-temp (n)
  (declare (fixnum n))
  (setq *available-backend-imm-temps* (logand *available-backend-imm-temps* (lognot (ash 1 n))))
  n)


(defun select-imm-temp (&optional (mode-name :u32))
  (let* ((mask *available-backend-imm-temps*))
    (dotimes (bit 32 (error "Bug: ran out of imm temp registers."))
      (when (logbitp bit mask)
        (setq *available-backend-imm-temps* (bitclr bit mask))
        (return (set-regspec-mode bit (gpr-mode-name-value mode-name)))))))

;;; Condition-register fields are PPC-specific, but we might as well have
;;; a portable interface to them.

(defun use-crf-temp (n)
  (declare (fixnum n))
  (setq *available-backend-crf-temps* (logand *available-backend-crf-temps* (lognot (ash 1 (ash n -2)))))
  n)

(defun select-crf-temp ()
  (let* ((mask *available-backend-crf-temps*))
    (dotimes (bit 8 (error "Bug: ran out of CR fields."))
      (declare (fixnum bit))
      (when (logbitp bit mask)
        (setq *available-backend-crf-temps* (bitclr bit mask))
        (return (make-hard-crf-reg (the fixnum (ash bit 2))))))))

(defun available-crf-temp (mask)
  (dotimes (bit 8 (error "Bug: ran out of CR fields."))
    (when (logbitp bit mask)
      (return (make-hard-crf-reg (the fixnum (ash bit 2)))))))

(defun single-float-reg-p (reg)
  (and (= (hard-regspec-class reg) hard-reg-class-fpr)
       (= (get-regspec-mode reg) hard-reg-class-fpr-mode-single)))

(defun target-fpr-mask (fpreg mode-name)
  (target-arch-case
   (:arm
    (ecase mode-name
      (:double-float (ash 3 (ash fpreg 1)))
      (:single-float (ash 1 fpreg))))
   (t
    (ash 1 fpreg))))

(defun fpr-mask-for-vreg (vreg)
  (let* ((value (hard-regspec-value vreg)))
    (target-arch-case
     (:arm
      (if (eql (get-regspec-mode vreg) hard-reg-class-fpr-mode-single)
        (ash 1 value)
        (ash 3 (ash value 1))))
     (t (ash 1 value)))))

(defun use-fp-reg (fpr)
    (setq *available-backend-fp-temps* (logand *available-backend-fp-temps* (lognot (fpr-mask-for-vreg fpr)))))



(defun available-fp-temp (mask &optional (mode-name :double-float))
  (do* ((regno 0 (1+ regno))
        (regmask (target-fpr-mask regno mode-name)  (target-fpr-mask regno mode-name)))
       ((> regmask mask) (error "Bug: ran out of fp registers."))
    (when (eql regmask (logand mask regmask))
      (let* ((mode (if (eq mode-name :double-float) 
                     hard-reg-class-fpr-mode-double
                     hard-reg-class-fpr-mode-single)))
        (return (make-hard-fp-reg regno mode))))))

(defun select-fp-temp (mode-name)
  (do* ((i 0 (1+ i))
        (fpr-mask (target-fpr-mask i mode-name) (target-fpr-mask i mode-name)))
       ((> fpr-mask *available-backend-fp-temps*) (compiler-bug "Bug: ran out of fp registers."))
    (when (= fpr-mask (logand fpr-mask *available-backend-fp-temps*))
      (return i))))


(defparameter *backend-all-lregs* ())
(defun note-logical-register (l)
  (push l *backend-all-lregs*)
  l)

(defun free-logical-registers ()
  (without-interrupts
   (let* ((prev (pool.data *lreg-freelist*)))
     (dolist (r *backend-all-lregs*)
       (setf (lreg-value r) prev
             prev r))
     (setf (pool.data *lreg-freelist*) prev)
     (setq *backend-all-lregs* nil))))


(defun make-unwired-lreg (value &key 
				(class (if value (hard-regspec-class value) 0))
				(mode (if value (get-regspec-mode value) 0))
				(type (if value (get-node-regspec-type-modes value) 0)))
  (note-logical-register (make-lreg (if value (hard-regspec-value value)) class mode type nil)))

;;; Make an lreg with the same class, mode, & type as the prototype.
(defun make-unwired-lreg-like (proto)
  (make-unwired-lreg nil
		     :class (hard-regspec-class proto)
		     :mode (get-regspec-mode proto)
		     :type (get-node-regspec-type-modes proto)))
  
(defun make-wired-lreg (value &key 
			      (class (hard-regspec-class value))
			      (mode (get-regspec-mode value))
			      (type (get-node-regspec-type-modes value)))
  (note-logical-register (make-lreg (hard-regspec-value value) class mode type t)))

(defvar *backend-immediates*)

(defun backend-new-immediate (imm)
  (vector-push-extend imm *backend-immediates*))

(defun backend-immediate-index (imm)
  (or (position imm *backend-immediates*)
      (backend-new-immediate imm)))

(defvar *backend-vinsns*)

(defvar *backend-labels*)

(defun backend-gen-label (seg labelnum)
  (append-dll-node (aref *backend-labels* labelnum) seg)
  labelnum)

(defconstant $backend-compound-branch-target-bit 18)
(defconstant $backend-compound-branch-target-mask (ash 1 $backend-compound-branch-target-bit))

(defconstant $backend-mvpass-bit 19)
(defconstant $backend-mvpass-mask (ash 1 $backend-mvpass-bit))

(defconstant $backend-return (- (ash 1 18) 1))
(defconstant $backend-mvpass (- (ash 1 18) 2))

(defconstant $backend-compound-branch-false-byte (byte 18 0))
(defconstant $backend-compound-branch-true-byte (byte 18 20))


(defun backend-get-next-label ()
  (let* ((lnum (length *backend-labels*)))
    (if (>= lnum $backend-mvpass)
      (compiler-function-overflow)
      (vector-push-extend (make-vinsn-label lnum) *backend-labels*))))


;;; Loop through all labels in *backend-labels*; if the label has been
;;; emitted, remove it from vinsns and return it to the
;;; *vinsn-label-freelist*.  "vinsns" should then contain nothing but
;;; ... vinsns

(defun backend-remove-labels ()
  (let* ((labels *backend-labels*)
         (freelist *vinsn-label-freelist*))
    (dotimes (i (the fixnum (length labels)))
      (let* ((lab (aref labels i)))
        (if lab
          (if (vinsn-label-succ lab)
            (remove-and-free-dll-node lab freelist)
            (free-dll-node lab freelist)))))))

(defun backend-copy-label (from to)
  (let* ((from-lab (aref *backend-labels* from))
         (to-lab (aref *backend-labels* to)))
    (when (null (vinsn-label-succ from-lab))
      (error "Copy label: not defined yet!"))
    (backend-merge-labels from-lab to-lab)
    (setf (aref *backend-labels* to) from-lab)))

(defun backend-merge-labels (from-lab to-lab)
  (let* ((to-refs (vinsn-label-refs to-lab)))
    (when to-refs
      ;; Make all extant refs to TO-LAB refer to FROM-LAB
      (setf (vinsn-label-refs to-lab) nil)
      (dolist (vinsn to-refs)
        (push vinsn (vinsn-label-refs from-lab))
        (let* ((vp (vinsn-variable-parts vinsn)))
          (declare (simple-vector vp))
          (dotimes (i (the fixnum (length vp)))
            (when (eq to-lab (svref vp i))
              (setf (svref vp i) from-lab))))))))

;;; For now, the register-spec must be 
;;; a) non-nil
;;; c) of an expected class.
;;; Return the class and value.
(defun regspec-class-and-value (regspec expected)
  (declare (fixnum expected))
  (let* ((class (hard-regspec-class regspec)))
    (declare (type (unsigned-byte 8) class))
    (if (logbitp class expected)
      (values class (if (typep regspec 'lreg)
		      regspec
		      (hard-regspec-value regspec)))
      (error "bug: Register spec class (~d) is not one  of ~s." class expected))))

(defmacro with-node-temps ((&rest reserved) (&rest nodevars) &body body)
  `(let* ((*available-backend-node-temps* (logand *available-backend-node-temps* (lognot (logior ,@(mapcar #'(lambda (r) `(ash 1 (hard-regspec-value ,r))) reserved)))))
          ,@(mapcar #'(lambda (v) `(,v (make-unwired-lreg (select-node-temp)))) nodevars))
     ,@body))

(defmacro with-imm-temps ((&rest reserved) (&rest immvars) &body body)
  `(let* ((*available-backend-imm-temps* (logand *available-backend-imm-temps* (lognot (logior ,@(mapcar #'(lambda (r) `(ash 1 (hard-regspec-value ,r))) reserved)))))
          ,@(mapcar #'(lambda (v) (let* ((var (if (atom v) v (car v)))
                                         (mode-name (if (atom v) :u32 (cadr v)))) 
                                    `(,var (select-imm-temp ',mode-name)))) immvars))
          ,@body))


(defmacro with-crf-target ((&rest reserved) name &body body)
  `(let* ((,name (make-unwired-lreg 
                  (available-crf-temp 
                   (logand *available-backend-crf-temps* 
                           (lognot (logior ,@(mapcar #'(lambda (r) `(ash 1 (ash (hard-regspec-value ,r) -2))) reserved))))))))
     ,@body))

(defmacro regspec-crf-gpr-case ((regspec regval) crf-form gpr-form)
  (let* ((class (gensym)))
    `(if ,regspec
       (multiple-value-bind (,class ,regval) (regspec-class-and-value ,regspec hard-reg-class-gpr-crf-mask)
         (declare (fixnum ,class))
         (if (= ,class hard-reg-class-crf)
           ,crf-form
           ,gpr-form)))))

;;; The NODE case may need to use ENSURING-NODE-TARGET.
(defmacro unboxed-other-case ((regspec &rest mode-names)
                              unboxed-case other-case)
  `(if (and ,regspec
        (= (hard-regspec-class ,regspec) hard-reg-class-gpr)
        (logbitp  (get-regspec-mode ,regspec)
         (logior ,@(mapcar #'(lambda (x) (ash 1 (gpr-mode-name-value x)))
                           mode-names))))
    ,unboxed-case
    ,other-case))




;;; Choose an immediate register (for targeting), but don't "reserve" it.
(defmacro with-imm-target ((&rest reserved) spec &body body)
  (let* ((name (if (atom spec) spec (car spec)))
         (mode-name (if (atom spec) :natural (cadr spec))))
    `(let* ((,name (make-unwired-lreg
		    (available-imm-temp
		     (logand
		      *available-backend-imm-temps* 
		      (lognot (logior ,@(mapcar
					 #'(lambda (r)
                                             `(if ,r
                                               (ash 1 (hard-regspec-value ,r))
                                               0))
					 reserved))))
		     ',mode-name))))
       ,@body)))

(defmacro with-node-target ((&rest reserved) name &body body)
  `(let* ((,name (make-unwired-lreg
                  (available-node-temp
                   (logand
                    *available-backend-node-temps* 
                    (lognot (logior ,@(mapcar
                                       #'(lambda (r)
                                           `(if ,r
                                             (ash 1 (hard-regspec-value ,r))
                                             0))
                                       reserved))))))))
    ,@body))




(defmacro with-fp-target ((&rest reserved) spec &body body)
  (let* ((name (if (atom spec) spec (car spec)))
         (mode-name (if (atom spec) :double-float (cadr spec))))
    `(let* ((,name
	     (make-unwired-lreg
	      (available-fp-temp
	       (logand *available-backend-fp-temps*
		       (lognot (logior
				,@(mapcar
				   #'(lambda (r) 
				       `(fpr-mask-for-vreg ,r))
				   reserved))))
	       ',mode-name))))
       ,@body)))

(defmacro ensuring-node-target ((target-var vreg-var) &body body)
  `(let* ((*available-backend-node-temps* *available-backend-node-temps*)
          (,target-var (ensure-node-target ,vreg-var)))
    (declare (special *available-backend-node-temps*))
    (macrolet ((<- (&whole call &rest args)
                 (declare (ignore args))
                 (error "Invalid use of <- inside ENSURING-NODE-TARGET: ~s" call))
               (^ (&whole call &rest args)
                 (declare (ignore args))
                 (error "Invalid use of ^ inside ENSURING-NODE-TARGET: ~s" call)))
      (progn
        ,@body))
    (<- ,target-var)))

(defun acode-invert-condition-keyword (k)
  (or 
   (cdr (assq k '((:eq . :ne) (:ne . :eq) (:le . :gt) (:lt . :ge) (:ge . :lt) (:gt . :le))))
   (error "Unknown condition: ~s" k)))

(defun backend-arch-macroexpand (whole env)
  (let* ((expander (arch::arch-macro-function
                    (backend-target-arch-name *target-backend*)
                    (car whole))))
    (if expander
      (funcall expander whole env)
      (error "No arch-specific macro function for ~s in arch ~s"
             (car whole) (backend-target-arch-name *target-backend*)))))

(defmacro declare-arch-specific-macro (name)
  `(progn
    (setf (macro-function ',name) #'backend-arch-macroexpand)
    ',name))

(defun target-nil-value (&optional (backend *target-backend*))
  (+ (arch::target-nil-value (backend-target-arch backend))
     (backend-real-lowmem-bias backend)))

(defun target-t-value (&optional (backend *target-backend*))
  (let* ((arch (backend-target-arch backend)))
    (+ (arch::target-nil-value arch)
       (arch::target-t-offset arch)
       (backend-real-lowmem-bias backend))))


     
