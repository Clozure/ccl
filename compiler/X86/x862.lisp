;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2005, Clozure Associates
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

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "X8632ENV")
  (require "X8664ENV"))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (require "X86-BACKEND"))

(defparameter *x862-debug-mask* 0)
(defconstant x862-debug-verbose-bit 0)
(defconstant x862-debug-vinsns-bit 1)
(defconstant x862-debug-lcells-bit 2)
(defparameter *x862-target-lcell-size* 0)
(defparameter *x862-target-node-size* 0)
(defparameter *x862-target-dnode-size* 0)
(defparameter *x862-target-fixnum-shift* 0)
(defparameter *x862-target-node-shift* 0)
(defparameter *x862-target-bits-in-word* 0)
(defparameter *x862-target-num-arg-regs* 0)
(defparameter *x862-target-num-save-regs* 0)
(defparameter *x862-target-half-fixnum-type* nil)

(defparameter *x862-operator-supports-u8-target* ())
(defparameter *x862-operator-supports-push* ())
(defparameter *x862-tos-reg* ())

;; probably should be elsewhere

(defmacro with-additional-imm-reg ((&rest reserved) &body body)
  (let ((node (gensym))
	(bit (gensym)))
    `(target-arch-case
      (:x8632
       (with-node-target (,@reserved) ,node
	 (let* ((,bit (ash 1 (hard-regspec-value ,node)))
		(*backend-node-temps* (logandc2 *backend-node-temps* ,bit))
		(*available-backend-node-temps* (logandc2 *available-backend-node-temps* ,bit))
		(*backend-imm-temps* (logior *backend-imm-temps* ,bit))
		(*available-backend-imm-temps* (logior *available-backend-imm-temps* ,bit)))
	   (! mark-as-imm ,node)
	   ,@body
	   (! mark-as-node ,node))))
      (:x8664
       (progn
	 ,@body)))))

  
(defun x862-immediate-operand (x)
  (if (eq (acode-operator x) (%nx1-operator immediate))
    (cadr x)
    (compiler-bug "not an immediate: ~s" x)))

(defmacro with-x86-p2-declarations (declsform &body body)
  `(let* ((*x862-tail-allow* *x862-tail-allow*)
          (*x862-reckless* *x862-reckless*)
          (*x862-open-code-inline* *x862-open-code-inline*)
          (*x862-trust-declarations* *x862-trust-declarations*)
          (*x862-full-safety* *x862-full-safety*))
     (x862-decls ,declsform)
     ,@body))


(defmacro with-x86-local-vinsn-macros ((segvar &optional vreg-var xfer-var) &body body)
  (declare (ignorable xfer-var))
  (let* ((template-name-var (gensym))
         (template-temp (gensym))
         (args-var (gensym))
         (labelnum-var (gensym))
         (retvreg-var (gensym))
         (label-var (gensym)))
    `(macrolet ((! (,template-name-var &rest ,args-var)                 
                  (let* ((,template-temp (get-vinsn-template-cell ,template-name-var (backend-p2-vinsn-templates *target-backend*))))
                    (unless ,template-temp
                      (warn "VINSN \"~A\" not defined" ,template-name-var))
                    `(prog1
                      (%emit-vinsn ,',segvar ',,template-name-var (backend-p2-vinsn-templates *target-backend*) ,@,args-var)
                      (setq *x862-tos-reg* nil)))))
       (macrolet ((<- (,retvreg-var)
                    `(x862-copy-register ,',segvar ,',vreg-var ,,retvreg-var))
                  (@  (,labelnum-var)
                    `(backend-gen-label ,',segvar ,,labelnum-var))
                  (@= (,labelnum-var)
                    `(x862-emit-aligned-label ,',segvar ,,labelnum-var))
                  (-> (,label-var)
                    `(! jump (aref *backend-labels* ,,label-var)))
                  (^ (&rest branch-args)
                    `(x862-branch ,',segvar ,',xfer-var ,@branch-args))
                  (? (&key (class :gpr)
                          (mode :lisp))
                   (let* ((class-val
                           (ecase class
                             (:gpr hard-reg-class-gpr)
                             (:fpr hard-reg-class-fpr)
                             (:crf hard-reg-class-crf)))
                          (mode-val-or-form
                           (if (eq class :gpr)
			     (if (member mode '(:natural :signed-natural))
			       `(gpr-mode-name-value ,mode)
			       (gpr-mode-name-value mode))
                             (if (eq class :fpr)
                               (if (eq mode :single-float)
                                 hard-reg-class-fpr-mode-single
                                 hard-reg-class-fpr-mode-double)
                               0))))
                     `(make-unwired-lreg nil
                       :class ,class-val
                       :mode ,mode-val-or-form)))
                  ($ (reg &key (class :gpr) (mode :lisp))
                   (let* ((class-val
                           (ecase class
                             (:gpr hard-reg-class-gpr)
                             (:fpr hard-reg-class-fpr)
                             (:crf hard-reg-class-crf)))
                          (mode-val-or-form
                           (if (eq class :gpr)
			     (if (member mode '(:natural :signed-natural))
			       `(gpr-mode-name-value ,mode)
			       (gpr-mode-name-value mode))
                             (if (eq class :fpr)
                               (if (eq mode :single-float)
                                 hard-reg-class-fpr-mode-single
                                 hard-reg-class-fpr-mode-double)
                               0))))
                     `(make-wired-lreg ,reg
                       :class ,class-val
                       :mode ,mode-val-or-form))))
         ,@body))))



(defvar *x86-current-context-annotation* nil)
(defvar *x862-woi* nil)
(defvar *x862-open-code-inline* nil)
(defvar *x862-register-restore-count* 0)
(defvar *x862-register-restore-ea* nil)
(defvar *x862-compiler-register-save-label* nil)
(defvar *x862-valid-register-annotations* 0)
(defvar *x862-register-annotation-types* nil)
(defvar *x862-register-ea-annotations* nil)
(defvar *x862-constant-alist* nil)
(defvar *x862-double-float-constant-alist* nil)
(defvar *x862-single-float-constant-alist* nil)

(defparameter *x862-tail-call-aliases*
  ()
  #| '((%call-next-method . (%tail-call-next-method . 1))) |#
  
)

(defvar *x862-popreg-labels* nil)
(defvar *x862-popj-labels* nil)
(defvar *x862-valret-labels* nil)
(defvar *x862-nilret-labels* nil)

(defvar *x862-icode* nil)
(defvar *x862-undo-stack* nil)
(defvar *x862-undo-because* nil)


(defvar *x862-cur-afunc* nil)
(defvar *x862-vstack* 0)
(defvar *x862-cstack* 0)
(defvar *x862-undo-count* 0)
(defvar *x862-returning-values* nil)
(defvar *x862-vcells* nil)
(defvar *x862-fcells* nil)
(defvar *x862-entry-vsp-saved-p* nil)

(defvar *x862-entry-label* nil)
(defvar *x862-tail-label* nil)
(defvar *x862-tail-vsp* nil)
(defvar *x862-tail-nargs* nil)
(defvar *x862-tail-allow* t)
(defvar *x862-reckless* nil)
(defvar *x862-full-safety* nil)
(defvar *x862-trust-declarations* nil)
(defvar *x862-entry-vstack* nil)
(defvar *x862-fixed-nargs* nil)
(defvar *x862-need-nargs* t)

(defparameter *x862-inhibit-register-allocation* nil)
(defvar *x862-record-symbols* nil)
(defvar *x862-recorded-symbols* nil)
(defvar *x862-emitted-source-notes* nil)

(defvar *x862-result-reg* x8664::arg_z)

(defvar *x862-arg-z* nil)
(defvar *x862-arg-y* nil)
(defvar *x862-imm0* nil)
(defvar *x862-temp0* nil)
(defvar *x862-temp1* nil)
(defvar *x862-fn* nil)
(defvar *x862-fname* nil)
(defvar *x862-ra0* nil)

(defvar *x862-allocptr* nil)

(defvar *x862-fp0* nil)
(defvar *x862-fp1* nil)

(declaim (fixnum *x862-vstack* *x862-cstack*))

 



(defvar *x862-all-lcells* ())

(defun x86-immediate-label (imm)
  (or (cdr (assoc imm *x862-constant-alist* :test #'eq))
      (let* ((lab (aref *backend-labels* (backend-get-next-label))))
        (push (cons imm lab) *x862-constant-alist*)
        lab)))

(defun x86-double-float-constant-label (imm)
  (or (cdr (assoc imm *x862-double-float-constant-alist*))
      (let* ((lab (aref *backend-labels* (backend-get-next-label))))
        (push (cons imm lab) *x862-double-float-constant-alist*)
        lab)))

(defun x86-single-float-constant-label (imm)
  (or (cdr (assoc imm *x862-single-float-constant-alist*))
      (let* ((lab (aref *backend-labels* (backend-get-next-label))))
        (push (cons imm lab) *x862-single-float-constant-alist*)
        lab)))


(defun x862-free-lcells ()
  (without-interrupts 
   (let* ((prev (pool.data *lcell-freelist*)))
     (dolist (r *x862-all-lcells*)
       (setf (lcell-kind r) prev
             prev r))
     (setf (pool.data *lcell-freelist*) prev)
     (setq *x862-all-lcells* nil))))

(defun x862-note-lcell (c)
  (push c *x862-all-lcells*)
  c)

(defvar *x862-top-vstack-lcell* ())
(defvar *x862-bottom-vstack-lcell* ())

(defun x862-new-lcell (kind parent width attributes info)
  (x862-note-lcell (make-lcell kind parent width attributes info)))

(defun x862-new-vstack-lcell (kind width attributes info)
  (setq *x862-top-vstack-lcell* (x862-new-lcell kind *x862-top-vstack-lcell* width attributes info)))

(defun x862-reserve-vstack-lcells (n)
  (dotimes (i n) (x862-new-vstack-lcell :reserved *x862-target-lcell-size* 0 nil)))

(defun x862-vstack-mark-top ()
  (x862-new-lcell :tos *x862-top-vstack-lcell* 0 0 nil))

;;; Alist mapping VARs to lcells/lregs
(defvar *x862-var-cells* ())

(defun x862-note-var-cell (var cell)
  ;(format t "~& ~s -> ~s" (var-name var) cell)
  (push (cons var cell) *x862-var-cells*))

(defun x862-note-top-cell (var)
  (x862-note-var-cell var *x862-top-vstack-lcell*))

(defun x862-lookup-var-cell (var)
  (or (cdr (assq var *x862-var-cells*))
      (and nil (warn "Cell not found for ~s" (var-name var)))))

(defun x862-collect-lcells (kind &optional (bottom *x862-bottom-vstack-lcell*) (top *x862-top-vstack-lcell*))
  (do* ((res ())
        (cell top (lcell-parent cell)))
       ((eq cell bottom) res)
    (if (null cell)
      (compiler-bug "Horrible compiler bug.")
      (if (eq (lcell-kind cell) kind)
        (push cell res)))))


  
;;; ensure that lcell's offset matches what we expect it to.
;;; For bootstrapping.

(defun x862-ensure-lcell-offset (c expected)
  (if c (= (calc-lcell-offset c) expected) (zerop expected)))

(defun x862-check-lcell-depth (&optional (context "wherever"))
  (when (logbitp x862-debug-verbose-bit *x862-debug-mask*)
    (let* ((depth (calc-lcell-depth *x862-top-vstack-lcell*)))
      (or (= depth *x862-vstack*)
          (warn "~a: lcell depth = ~d, vstack = ~d" context depth *x862-vstack*)))))

(defun x862-do-lexical-reference (seg vreg ea)
  (when vreg
    (with-x86-local-vinsn-macros (seg vreg)
      (if (eq vreg :push)
        (if (memory-spec-p ea)
          (if (addrspec-vcell-p ea)
            (with-node-target () target
              (x862-stack-to-register seg ea target)
              (! vcell-ref target target)
              (! vpush-register target))
            (! vframe-push (memspec-frame-address-offset ea) *x862-vstack*))
          (! vpush-register ea))
        (if (memory-spec-p ea)
          (ensuring-node-target (target vreg)
            (progn
              (x862-stack-to-register seg ea target)
              (if (addrspec-vcell-p ea)
                (! vcell-ref target target))))
          (<- ea))))))

(defun x862-do-lexical-setq (seg vreg ea valreg)
  (with-x86-local-vinsn-macros (seg vreg)
    (cond ((typep ea 'lreg)
            (x862-copy-register seg ea valreg))
          ((addrspec-vcell-p ea)     ; closed-over vcell
           (x862-copy-register seg *x862-arg-z* valreg)
	   (let* ((gvector (target-arch-case (:x8632 x8632::temp0)
					     (:x8664 x8664::arg_x))))
	     (x862-stack-to-register seg ea gvector)
	     (x862-lri seg *x862-arg-y* 0)
	     (! call-subprim-3 *x862-arg-z* (subprim-name->offset '.SPgvset) gvector *x862-arg-y* *x862-arg-z*)))
          ((memory-spec-p ea)    ; vstack slot
           (x862-register-to-stack seg valreg ea))
          (t
           (x862-copy-register seg ea valreg)))
    (when vreg
      (<- valreg))))

;;; ensure that next-method-var is heap-consed (if it's closed over.)
;;; it isn't ever setqed, is it ?
(defun x862-heap-cons-next-method-var (seg var)
  (with-x86-local-vinsn-macros (seg)
    (when (eq (ash 1 $vbitclosed)
              (logand (logior (ash 1 $vbitclosed)
                              (ash 1 $vbitcloseddownward))
                      (the fixnum (nx-var-bits var))))
      (let* ((ea (var-ea var))
             (arg ($ *x862-arg-z*))
             (result ($ *x862-arg-z*)))
        (x862-do-lexical-reference seg arg ea)
        (x862-set-nargs seg 1)
        (! ref-constant ($ *x862-fname*) (x86-immediate-label (x862-symbol-entry-locative '%cons-magic-next-method-arg)))
        (! call-known-symbol arg)
        (x862-do-lexical-setq seg nil ea result)))))

;;; If we change the order of operands in a binary comparison operation,
;;; what should the operation change to ? (eg., (< X Y) means the same
;;; thing as (> Y X)).
(defparameter *x862-reversed-cr-bits*
  (vector
   nil                                  ;o ?
   nil                                  ;no ?
   x86::x86-a-bits                      ;b -> a
   x86::x86-be-bits                     ;ae -> be
   x86::x86-e-bits                      ;e->e
   x86::x86-ne-bits                     ;ne->ne
   x86::x86-ae-bits                     ;be->ae
   x86::x86-b-bits                      ;a->b
   nil                                  ;s ?
   nil                                  ;ns ?
   nil                                  ;pe ?
   nil                                  ;po ?
   x86::x86-g-bits                      ;l->g
   x86::x86-le-bits                     ;ge->le
   x86::x86-ge-bits                     ;le->ge
   x86::x86-l-bits                      ;g->l
   ))

(defun x862-reverse-cr-bit (cr-bit)
  (or (svref *x862-reversed-cr-bits* cr-bit)
      (compiler-bug "Can't reverse CR bit ~d" cr-bit)))


(defun acode-condition-to-x86-cr-bit (cond)
  (condition-to-x86-cr-bit (cadr cond)))

(defun condition-to-x86-cr-bit (cond)
  (case cond
    (:EQ (values x86::x86-e-bits t))
    (:NE (values x86::x86-e-bits nil))
    (:GT (values x86::x86-le-bits nil))
    (:LE (values x86::x86-le-bits t))
    (:LT (values x86::x86-l-bits t))
    (:GE (values x86::x86-l-bits nil))))

;;; Generate the start and end bits for a RLWINM instruction that
;;; would be equivalent to to LOGANDing the constant with some value.
;;; Return (VALUES NIL NIL) if the constant contains more than one
;;; sequence of consecutive 1-bits, else bit indices.
;;; The caller generally wants to treat the constant as an (UNSIGNED-BYTE 32);
;;; since this uses LOGCOUNT and INTEGER-LENGTH to find the significant
;;; bits, it ensures that the constant is a (SIGNED-BYTE 32) that has
;;; the same least-significant 32 bits.
(defun x862-mask-bits (constant)
  (if (< constant 0) (setq constant (logand #xffffffff constant)))
  (if (= constant #xffffffff)
    (values 0 31)
    (if (zerop constant)
      (values nil nil)
      (let* ((signed (if (and (logbitp 31 constant)
                              (> constant 0))
                       (- constant (ash 1 32))
                       constant))
             (count (logcount signed))
             (len (integer-length signed))
             (highbit (logbitp (the fixnum (1- len)) constant)))
        (declare (fixnum count len))
        (do* ((i 1 (1+ i))
              (pos (- len 2) (1- pos)))
             ((= i count)
              (let* ((start (- 32 len))
                     (end (+ count start)))
                (declare (fixnum start end))
                (if highbit
                  (values start (the fixnum (1- end)))
                  (values (logand 31 end)
                          (the fixnum (1- start))))))
          (declare (fixnum i pos))
          (unless (eq (logbitp pos constant) highbit)
            (return (values nil nil))))))))
    

(defun x862-ensure-binding-indices-for-vcells (vcells)
  (dolist (cell vcells)
    (ensure-binding-index (car cell)))
  vcells)

(defun x862-register-mask-byte (count)
  (if (> count 0)
    (logior
     (ash 1 (- x8664::save0 8))
     (if (> count 1)
       (logior
        (ash 1 (- x8664::save1 8))
        (if (> count 2)
          (logior
           (ash 1 (- x8664::save2 8))
           (if (> count 3)
             (ash 1 (- x8664::save3 8))
             0))
          0))
       0))
    0))

(defun x862-encode-register-save-ea (ea count)
  (if (zerop count)
    0 
    (min (- (ash ea (- *x862-target-node-shift*)) count) #xff)))


(defun x862-compile (afunc &optional lambda-form *x862-record-symbols*)
  (progn
    (dolist (a  (afunc-inner-functions afunc))
      (unless (afunc-lfun a)
        (x862-compile a 
                      (if lambda-form (afunc-lambdaform a))
                      *x862-record-symbols*))) ; always compile inner guys
    (let* ((*x862-cur-afunc* afunc)
           (*x862-returning-values* nil)
           (*x86-current-context-annotation* nil)
           (*x862-woi* nil)
           (*next-lcell-id* -1)
           (*x862-open-code-inline* nil)
           (*x862-register-restore-count* nil)
           (*x862-compiler-register-save-label* nil)
           (*x862-valid-register-annotations* 0)
           (*x862-register-ea-annotations* (x862-make-stack 16))
           (*x862-register-restore-ea* nil)
           (*x862-constant-alist* nil)
           (*x862-double-float-constant-alist* nil)
           (*x862-single-float-constant-alist* nil)
           (*x862-vstack* 0)
           (*x862-cstack* 0)
	   (*x86-lap-entry-offset* (target-arch-case
				    (:x8632 x8632::fulltag-misc)
				    (:x8664 x8664::fulltag-function)))
	   (*x862-result-reg* (target-arch-case
			       (:x8632 x8632::arg_z)
			       (:x8664 x8664::arg_z)))
	   (*x862-imm0* (target-arch-case (:x8632 x8632::imm0)
					  (:x8664 x8664::imm0)))
	   (*x862-arg-z* (target-arch-case (:x8632 x8632::arg_z)
					   (:x8664 x8664::arg_z)))
	   (*x862-arg-y* (target-arch-case (:x8632 x8632::arg_y)
					   (:x8664 x8664::arg_y)))
	   (*x862-temp0* (target-arch-case (:x8632 x8632::temp0)
					   (:x8664 x8664::temp0)))
	   (*x862-temp1* (target-arch-case (:x8632 x8632::temp1)
					   (:x8664 x8664::temp1)))
	   (*x862-fn* (target-arch-case (:x8632 x8632::fn)
					(:x8664 x8664::fn)))
	   (*x862-fname* (target-arch-case (:x8632 x8632::fname)
					   (:x8664 x8664::fname)))
	   (*x862-ra0* (target-arch-case (:x8632 x8632::ra0)
					 (:x8664 x8664::ra0)))
	   (*x862-allocptr* (target-arch-case (:x8632 x8632::allocptr)
					      (:x8664 x8664::allocptr)))
	   (*x862-fp0* (target-arch-case (:x8632 x8632::fp0)
					 (:x8664 x8664::fp0)))
	   (*x862-fp1* (target-arch-case (:x8632 x8632::fp1)
					 (:x8664 x8664::fp1)))
           (*x862-target-num-arg-regs* (target-arch-case
					(:x8632 $numx8632argregs)
					(:x8664  $numx8664argregs)))
           (*x862-target-num-save-regs* (target-arch-case
					 (:x8632 $numx8632saveregs)
					 (:x8664  $numx8664saveregs)))
	   (*x862-target-lcell-size* (arch::target-lisp-node-size (backend-target-arch *target-backend*)))
           (*x862-target-fixnum-shift* (arch::target-fixnum-shift (backend-target-arch *target-backend*)))
           (*x862-target-node-shift* (arch::target-word-shift  (backend-target-arch *target-backend*)))
           (*x862-target-bits-in-word* (arch::target-nbits-in-word (backend-target-arch *target-backend*)))
	   (*x862-target-node-size* *x862-target-lcell-size*)
           (*x862-target-half-fixnum-type* `(signed-byte ,(- *x862-target-bits-in-word*
                                                            (1+ *x862-target-fixnum-shift*))))
           (*x862-target-dnode-size* (* 2 *x862-target-lcell-size*))
           (*x862-tos-reg* nil)
           (*x862-all-lcells* ())
           (*x862-top-vstack-lcell* nil)
           (*x862-bottom-vstack-lcell* (x862-new-vstack-lcell :bottom 0 0 nil))
           (*x862-var-cells* nil)
           (*backend-vinsns* (backend-p2-vinsn-templates *target-backend*))
           (*backend-node-regs* (target-arch-case
				 (:x8632 x8632-node-regs)
				 (:x8664 x8664-node-regs)))
           (*backend-node-temps* (target-arch-case
				  (:x8632 x8632-temp-node-regs)
				  (:x8664 x8664-temp-node-regs)))
           (*available-backend-node-temps* (target-arch-case
					    (:x8632 x8632-temp-node-regs)
					    (:x8664 x8664-temp-node-regs)))
           (*backend-imm-temps* (target-arch-case
				 (:x8632 x8632-imm-regs)
				 (:x8664 x8664-imm-regs)))
           (*available-backend-imm-temps* (target-arch-case
					   (:x8632 x8632-imm-regs)
					   (:x8664 x8664-imm-regs)))
           (*backend-crf-temps* (target-arch-case
				 (:x8632 x8632-cr-fields)
				 (:x8664 x8664-cr-fields)))
           (*available-backend-crf-temps* (target-arch-case
					   (:x8632 x8632-cr-fields)
					   (:x8664 x8664-cr-fields)))
           (*backend-fp-temps* (target-arch-case
				(:x8632 x8632-temp-fp-regs)
				(:x8664 x8664-temp-fp-regs)))
           (*available-backend-fp-temps* (target-arch-case
					  (:x8632 x8632-temp-fp-regs)
					  (:x8664 x8664-temp-fp-regs)))
           (bits 0)
           (*logical-register-counter* -1)
           (*backend-all-lregs* ())
           (*x862-popj-labels* nil)
           (*x862-popreg-labels* nil)
           (*x862-valret-labels* nil)
           (*x862-nilret-labels* nil)
           (*x862-undo-count* 0)
           (*backend-labels* (x862-make-stack 64 target::subtag-simple-vector))
           (*x862-undo-stack* (x862-make-stack 64  target::subtag-simple-vector))
           (*x862-undo-because* (x862-make-stack 64))
           (*x862-entry-label* nil)
           (*x862-tail-label* nil)
           (*x862-tail-vsp* nil)
           (*x862-tail-nargs* nil)
           (*x862-inhibit-register-allocation* nil)
           (*x862-tail-allow* t)
           (*x862-reckless* nil)
           (*x862-full-safety* nil)
           (*x862-trust-declarations* t)
           (*x862-entry-vstack* nil)
           (*x862-fixed-nargs* nil)
           (*x862-need-nargs* t)
           (fname (afunc-name afunc))
           (*x862-entry-vsp-saved-p* nil)
           (*x862-vcells* (x862-ensure-binding-indices-for-vcells (afunc-vcells afunc)))
           (*x862-fcells* (afunc-fcells afunc))
           *x862-recorded-symbols*
           (*x862-emitted-source-notes* '()))
      (set-fill-pointer
       *backend-labels*
       (set-fill-pointer
        *x862-undo-stack*
        (set-fill-pointer 
         *x862-undo-because*
         0)))
      (backend-get-next-label)          ; start @ label 1, 0 is confused with NIL in compound cd
      (with-dll-node-freelist (vinsns *vinsn-freelist*)
        (unwind-protect
             (progn
               (setq bits (x862-form vinsns (make-wired-lreg *x862-result-reg*) $backend-return (afunc-acode afunc)))
               (do* ((constants *x862-constant-alist* (cdr constants)))
                    ((null constants))
                 (let* ((imm (caar constants)))
                   (when (x862-symbol-locative-p imm)
                     (setf (caar constants) (car imm)))))
               (optimize-vinsns vinsns)
               (when (logbitp x862-debug-vinsns-bit *x862-debug-mask*)
                 (format t "~% vinsns for ~s (after generation)" (afunc-name afunc))
                 (do-dll-nodes (v vinsns) (format t "~&~s" v))
                 (format t "~%~%"))
            
               (with-dll-node-freelist ((frag-list make-frag-list) *frag-freelist*)
                 (with-dll-node-freelist ((uuo-frag-list make-frag-list) *frag-freelist*)
                 (let* ((*x86-lap-labels* nil)
                        (instruction (x86::make-x86-instruction))
                        (end-code-tag (gensym))
			(start-tag (gensym))
			(srt-tag (gensym))
                        debug-info)
                   (make-x86-lap-label end-code-tag)
		   (target-arch-case
		    (:x8664
		     (x86-lap-directive frag-list :long `(ash (+ (- (:^ ,end-code-tag ) 8)
								 *x86-lap-entry-offset*) -3))
		     (x86-lap-directive frag-list :byte 0) ;regsave PC 
		     (x86-lap-directive frag-list :byte 0) ;regsave ea
		     (x86-lap-directive frag-list :byte 0)) ;regsave mask
		    (:x8632
		     (make-x86-lap-label start-tag)
		     (make-x86-lap-label srt-tag)
		     (x86-lap-directive frag-list :short `(ash (+ (- (:^ ,end-code-tag) 4)
								  *x86-lap-entry-offset*) -2))
		     (emit-x86-lap-label frag-list start-tag)))
                   (x862-expand-vinsns vinsns frag-list instruction uuo-frag-list)
                   (when (or *x862-double-float-constant-alist*
                             *x862-single-float-constant-alist*)
                     (x86-lap-directive frag-list :align 3)
                     (dolist (double-pair *x862-double-float-constant-alist*)
                       (destructuring-bind (dfloat . lab) double-pair
                         (setf (vinsn-label-info lab) (emit-x86-lap-label frag-list lab))
                         (multiple-value-bind (high low)
                             (x862-double-float-bits dfloat)
                           (x86-lap-directive frag-list :long low)
                           (x86-lap-directive frag-list :long high))))
                     (dolist (single-pair *x862-single-float-constant-alist*)
                       (destructuring-bind (sfloat . lab) single-pair
                         (setf (vinsn-label-info lab) (emit-x86-lap-label frag-list lab))
                         (let* ((val (single-float-bits sfloat)))
                           (x86-lap-directive frag-list :long val)))))
                   (target-arch-case
		    (:x8632
		     (x86-lap-directive frag-list :align 2)
		     ;; start of self reference table
		     (x86-lap-directive frag-list :long 0)
		     (emit-x86-lap-label frag-list srt-tag)
		     ;; make space for self-reference offsets
		     (do-dll-nodes (frag frag-list)
		       (dolist (reloc (frag-relocs frag))
			 (when (eq (reloc-type reloc) :self)
			   (x86-lap-directive frag-list :long 0))))
		     (x86-lap-directive frag-list :long x8632::function-boundary-marker))
		    (:x8664
		     (x86-lap-directive frag-list :align 3)
		     (x86-lap-directive frag-list :quad x8664::function-boundary-marker)))

                   (emit-x86-lap-label frag-list end-code-tag)
		   
                   (dolist (c (reverse *x862-constant-alist*))
                     (let* ((vinsn-label (cdr c)))
                       (or (vinsn-label-info vinsn-label)
                           (setf (vinsn-label-info vinsn-label)
                                 (find-or-create-x86-lap-label
                                  vinsn-label)))
                       (emit-x86-lap-label frag-list vinsn-label)
		       (target-arch-case
			(:x8632
			 (x86-lap-directive frag-list :long 0))
			(:x8664
			 (x86-lap-directive frag-list :quad 0)))))

                   (if (logbitp $fbitnonnullenv (the fixnum (afunc-bits afunc)))
                     (setq bits (+ bits (ash 1 $lfbits-nonnullenv-bit))))
                   (setq debug-info (afunc-lfun-info afunc))
                   (when lambda-form
                     (setq debug-info
                           (list* 'function-lambda-expression lambda-form debug-info)))
                   (when *x862-record-symbols*
                     (setq debug-info
                           (list* 'function-symbol-map *x862-recorded-symbols* debug-info)))
                   (when (and (getf debug-info 'function-source-note) *x862-emitted-source-notes*)
                     (setq debug-info                     ;; Compressed below
                           (list* 'pc-source-map *x862-emitted-source-notes* debug-info)))
                   (when debug-info
                     (setq bits (logior (ash 1 $lfbits-info-bit) bits)))
                   (unless (or fname lambda-form *x862-recorded-symbols*)
                     (setq bits (logior (ash 1 $lfbits-noname-bit) bits)))
                   (unless (afunc-parent afunc)
                     (x862-fixup-fwd-refs afunc))
                   (setf (afunc-all-vars afunc) nil)
                   (setf (afunc-argsword afunc) bits)
                   (let* ((regsave-label (if (typep *x862-compiler-register-save-label* 'vinsn-note)
                                           (vinsn-label-info (vinsn-note-label *x862-compiler-register-save-label*))))
                          (regsave-mask (if regsave-label (x862-register-mask-byte
                                                           *x862-register-restore-count*)))
                          (regsave-addr (if regsave-label (x862-encode-register-save-ea
                                                           *x862-register-restore-ea*
                                                           *x862-register-restore-count*))))
		     (target-arch-case
		      (:x8632
		       (when debug-info
			 (x86-lap-directive frag-list :long 0))
		       (when fname
			 (x86-lap-directive frag-list :long 0))
		       (x86-lap-directive frag-list :long 0))
		      (:x8664
		       (when debug-info
			 (x86-lap-directive frag-list :quad 0))
		       (when fname
			 (x86-lap-directive frag-list :quad 0))
		       (x86-lap-directive frag-list :quad 0)))

                     (relax-frag-list frag-list)
                     (apply-relocs frag-list)
                     (fill-for-alignment frag-list)
		     (target-arch-case
		      (:x8632
		       (let* ((label (find-x86-lap-label srt-tag))
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
		       ))

                     (x862-lap-process-regsave-info frag-list regsave-label regsave-mask regsave-addr)

                     (when (getf debug-info 'pc-source-map)
                       (setf (getf debug-info 'pc-source-map) (x862-generate-pc-source-map debug-info)))
                     (when (getf debug-info 'function-symbol-map)
                       (setf (getf debug-info 'function-symbol-map) (x862-digest-symbols)))

                     (setf (afunc-lfun afunc)
                           #+x86-target
                           (if (eq *host-backend* *target-backend*)
                             (create-x86-function fname frag-list *x862-constant-alist* bits debug-info)
                             (cross-create-x86-function fname frag-list *x862-constant-alist* bits debug-info))
                           #-x86-target
                           (cross-create-x86-function fname frag-list *x862-constant-alist* bits debug-info)))))))
          (backend-remove-labels))))
    afunc))


      
    
(defun x862-make-stack (size &optional (subtype target::subtag-s16-vector))
  (make-uarray-1 subtype size t 0 nil nil nil nil t nil))

(defun x862-fixup-fwd-refs (afunc)
  (dolist (f (afunc-inner-functions afunc))
    (x862-fixup-fwd-refs f))
  (let ((fwd-refs (afunc-fwd-refs afunc)))
    (when fwd-refs
      (let* ((native-x86-functions #-x86-target nil
                                   #+x86-target (eq *target-backend*
                                                    *host-backend*))
             (v (if native-x86-functions
                  (function-to-function-vector (afunc-lfun afunc))
                  (afunc-lfun afunc)))
             (vlen (uvsize v)))
        (declare (fixnum vlen))
        (dolist (ref fwd-refs)
          (let* ((ref-fun (afunc-lfun ref)))
            (do* ((i (if native-x86-functions
                       (%function-code-words
                        (function-vector-to-function v))
                       1)
                     (1+ i)))
                 ((= i vlen))
              (declare (fixnum i))
              (if (eq (%svref v i) ref)
                (setf (%svref v i) ref-fun)))))))))

(defun x862-generate-pc-source-map (debug-info)
  (let* ((definition-source-note (getf debug-info 'function-source-note))
         (emitted-source-notes (getf debug-info 'pc-source-map))
         (def-start (source-note-start-pos definition-source-note))
         (n (length emitted-source-notes))
         (nvalid 0)
         (max 0)
         (pc-starts (make-array n))
         (pc-ends (make-array n))
         (text-starts (make-array n))
         (text-ends (make-array n)))
    (declare (fixnum n nvalid)
             (dynamic-extent pc-starts pc-ends text-starts text-ends))
    (dolist (start emitted-source-notes)
      (let* ((pc-start (x862-vinsn-note-label-address start t))
             (pc-end (x862-vinsn-note-label-address (vinsn-note-peer start) nil))
             (source-note (aref (vinsn-note-info start) 0))
             (text-start (- (source-note-start-pos source-note) def-start))
             (text-end (- (source-note-end-pos source-note) def-start)))
        (declare (fixnum pc-start pc-end text-start text-end))
        (when (and (plusp pc-start)
                   (plusp pc-end)
                   (plusp text-start)
                   (plusp text-end))
          (if (> pc-start max) (setq max pc-start))
          (if (> pc-end max) (setq max pc-end))
          (if (> text-start max) (setq max text-start))
          (if (> text-end max) (setq max text-end))
          (setf (svref pc-starts nvalid) pc-start
                (svref pc-ends nvalid) pc-end
                (svref text-starts nvalid) text-start
                (svref text-ends nvalid) text-end)
          (incf nvalid))))
    (let* ((nentries (* nvalid 4))
           (vec (cond ((< max #x100) (make-array nentries :element-type '(unsigned-byte 8)))
                      ((< max #x10000) (make-array nentries :element-type '(unsigned-byte 16)))
                      (t (make-array nentries :element-type '(unsigned-byte 32))))))
      (declare (fixnum nentries))
      (do* ((i 0 (+ i 4))
            (j 1 (+ j 4))
            (k 2 (+ k 4))
            (l 3 (+ l 4))
            (idx 0 (1+ idx)))
          ((= i nentries) vec)
        (declare (fixnum i j k l idx))
        (setf (aref vec i) (svref pc-starts idx)
              (aref vec j) (svref pc-ends idx)
              (aref vec k) (svref text-starts idx)
              (aref vec l) (svref text-ends idx))))))

(defun x862-vinsn-note-label-address (note &optional start-p sym)
  (-
   (let* ((label (vinsn-note-label note))
          (lap-label (if label (vinsn-label-info label))))
     (if lap-label
       (x86-lap-label-address lap-label)
       (compiler-bug "Missing or bad ~s label~@[: ~s~]"
                     (if start-p 'start 'end)
                     sym)))
   (target-arch-case
    (:x8632 x8632::fulltag-misc)        ;xxx?
    (:x8664 x8664::fulltag-function))))

(defun x862-digest-symbols ()
  (when *x862-recorded-symbols*
    (let* ((symlist *x862-recorded-symbols*)
           (len (length symlist))
           (syms (make-array len))
           (ptrs (make-array (%i+  (%i+ len len) len)))
           (i -1)
           (j -1))
      (declare (fixnum i j))
      (dolist (info symlist (progn (%rplaca symlist syms)
                                   (%rplacd symlist ptrs)))
        (destructuring-bind (var sym startlab endlab) info
          (let* ((ea (var-ea var))
                 (ea-val (ldb (byte 16 0) ea)))
            (setf (aref ptrs (incf i)) (if (memory-spec-p ea)
                                         (logior (ash ea-val 6) #o77)
                                         ea-val)))
          (setf (aref syms (incf j)) sym)
          (setf (aref ptrs (incf i)) (x862-vinsn-note-label-address startlab t sym))
          (setf (aref ptrs (incf i)) (x862-vinsn-note-label-address endlab nil sym))))
      *x862-recorded-symbols*)))

(defun x862-decls (decls)
  (if (fixnump decls)
    (locally (declare (fixnum decls))
      (setq *x862-tail-allow* (neq 0 (%ilogand2 $decl_tailcalls decls))
            *x862-open-code-inline* (neq 0 (%ilogand2 $decl_opencodeinline decls))
            *x862-full-safety* (neq 0 (%ilogand2 $decl_full_safety decls))
            *x862-reckless* (neq 0 (%ilogand2 $decl_unsafe decls))
            *x862-trust-declarations* (neq 0 (%ilogand2 $decl_trustdecls decls))))))


(defun %x862-bigger-cdr-than (x y)
  (declare (cons x y))
  (> (the fixnum (cdr x)) (the fixnum (cdr y))))

;;; Return an unordered list of "varsets": each var in a varset can be
;;; assigned a register and all vars in a varset can be assigned the
;;; same register (e.g., no scope conflicts.)

(defun x862-partition-vars (vars)
  (labels ((var-weight (var)
             (let* ((bits (nx-var-bits var)))
               (declare (fixnum bits))
               (if (eql 0 (logand bits (logior
                                        (ash 1 $vbitpuntable)
                                        (ash -1 $vbitspecial)
                                        (ash 1 $vbitnoreg))))
                 (if (eql (logior (ash 1 $vbitclosed) (ash 1 $vbitsetq))
                          (logand bits (logior (ash 1 $vbitclosed) (ash 1 $vbitsetq))))
                   0
                   (%i+ (%ilogand $vrefmask bits) (%ilsr 8 (%ilogand $vsetqmask bits))))
                 0)))
           (sum-weights (varlist) 
             (let ((sum 0))
               (dolist (v varlist sum) (incf sum (var-weight v)))))
           (vars-disjoint-p (v1 v2)
             (if (eq v1 v2)
               nil
               (if (memq v1 (var-binding-info v2))
                 nil
                 (if (memq v2 (var-binding-info v1))
                   nil
                   t)))))
    (setq vars (%sort-list-no-key
                ;(delete-if #'(lambda (v) (eql (var-weight v) 0)) vars) 
                (do* ((handle (cons nil vars))
                      (splice handle))
                     ((null (cdr splice)) (cdr handle))                  
                  (declare (dynamic-extent handle) (type cons handle splice))
                  (if (eql 0 (var-weight (%car (cdr splice))))
                    (rplacd splice (%cdr (cdr splice)))
                    (setq splice (cdr splice))))
                #'(lambda (v1 v2) (%i> (var-weight v1) (var-weight v2)))))
    ;; This isn't optimal.  It partitions all register-allocatable
    ;; variables into sets such that
    ;; 1) no variable is a member of more than one set and
    ;; 2) all variables in a given set are disjoint from each other
    ;; A set might have exactly one member.
    ;; If a register is allocated for any member of a set, it's
    ;; allocated for all members of that set.
    (let* ((varsets nil))
      (do* ((all vars (cdr all)))
           ((null all))
        (let* ((var (car all)))
          (when (dolist (already varsets t)
                  (when (memq var (car already)) (return)))
            (let* ((varset (cons var nil)))
              (dolist (v (cdr all))
                (when (dolist (already varsets t)
                        (when (memq v (car already)) (return)))
                  (when (dolist (d varset t)
                          (unless (vars-disjoint-p v d) (return)))
                    (push v varset))))
              (let* ((weight (sum-weights varset)))
                (declare (fixnum weight))
                (if (>= weight 3)
                  (push (cons (nreverse varset) weight) varsets)))))))
      varsets)))

;;; Maybe globally allocate registers to symbols naming functions & variables,
;;; and to simple lexical variables.
(defun x862-allocate-global-registers (fcells vcells all-vars no-regs)
  (if (or no-regs (target-arch-case (:x8632 t)))
    (progn
      (dolist (c fcells) (%rplacd c nil))
      (dolist (c vcells) (%rplacd c nil))
      (values 0 nil))
    (let* ((maybe (x862-partition-vars all-vars)))
      (dolist (c fcells) 
        (if (>= (the fixnum (cdr c)) 3) (push c maybe)))
      (dolist (c vcells) 
        (if (>= (the fixnum (cdr c)) 3) (push c maybe)))
      (do* ((things (%sort-list-no-key maybe #'%x862-bigger-cdr-than) (cdr things))
            (n 0 (1+ n))
            (registers (target-arch-case
			(:x8632 (error "no nvrs on x8632"))
			(:x8664
                         (if (= (backend-lisp-context-register *target-backend*) x8664::save3)
                           (list x8664::save0 x8664::save1 x8664::save2)
                           (list x8664::save0 x8664::save1 x8664::save2 x8664::save3)))))
            (regno (pop registers) (pop registers))
            (constant-alist ()))
           ((or (null things) (null regno))
            (dolist (cell fcells) (%rplacd cell nil))
            (dolist (cell vcells) (%rplacd cell nil))
            (values n constant-alist))
        (declare (list things)
                 (fixnum n regno))
        (let* ((thing (car things)))
          (if (or (memq thing fcells)
                  (memq thing vcells))
            (push (cons thing regno) constant-alist)
            (dolist (var (car thing))
              (nx-set-var-bits var 
                               (%ilogior (%ilogand (%ilognot $vrefmask) (nx-var-bits var))
                                 regno
                                 (%ilsl $vbitreg 1))))))))))


    
;;; Vpush the last N non-volatile-registers.
(defun x862-save-nvrs (seg n)
  (declare (fixnum n))
  (target-arch-case
   ;; no nvrs on x8632
   (:x8664
    (when (> n 0)
      (setq *x862-compiler-register-save-label* (x862-emit-note seg :regsave))
      (with-x86-local-vinsn-macros (seg)
	(let* ((mask x8664-nonvolatile-node-regs))
	  (dotimes (i n)
	    (let* ((reg (1- (integer-length mask))))
	      (x862-vpush-register seg reg :regsave reg 0)
	      (setq mask (logandc2 mask (ash 1 reg)))))))
      (setq *x862-register-restore-ea* *x862-vstack*
	    *x862-register-restore-count* n)))))


;;; If there are an indefinite number of args/values on the vstack,
;;; we have to restore from a register that matches the compiler's
;;; notion of the vstack depth.  This can be computed by the caller 
;;; (sum of vsp & nargs, or copy of vsp  before indefinite number of 
;;; args pushed, etc.)


(defun x862-restore-nvrs (seg ea nregs &optional (can-pop t))
  (target-arch-case
   ;; no nvrs on x8632
   (:x8664
    (when (and ea nregs)
      (with-x86-local-vinsn-macros (seg)
	(let* ((mask x8664-nonvolatile-node-regs)
	       (regs ()))
	  (dotimes (i nregs)
	    (let* ((reg (1- (integer-length mask))))
	      (push reg regs)
	      (setq mask (logandc2 mask (ash 1 reg)))))
	  (cond (can-pop
		 (let* ((diff-in-bytes (- *x862-vstack* ea)))
		   (unless (zerop diff-in-bytes)
		     (x862-adjust-vstack diff-in-bytes)
		     (! vstack-discard (floor diff-in-bytes *x862-target-node-size*)))
		   (dolist (reg regs)
		     (! vpop-register reg))))
		(t
		 (dolist (reg regs)
		   (! vframe-load reg (- ea *x862-target-node-size*) ea)
		   (decf ea *x862-target-node-size*))))))))))


(defun x862-bind-lambda (seg lcells req opt rest keys auxen optsupvloc passed-in-regs lexpr &optional inherited
                             &aux (vloc 0) (numopt (list-length (%car opt)))
                             (nkeys (list-length (%cadr keys))) 
                             reg)
  (declare (fixnum vloc))
  (x862-check-lcell-depth)
  (dolist (arg inherited)
    (if (memq arg passed-in-regs)
      (x862-set-var-ea seg arg (var-ea arg))
      (let* ((lcell (pop lcells)))
        (if (setq reg (x862-assign-register-var arg))
          (x862-init-regvar seg arg reg (x862-vloc-ea vloc))
          (x862-bind-var seg arg vloc lcell))
        (setq vloc (%i+ vloc *x862-target-node-size*)))))
  (dolist (arg req)
    (if (memq arg passed-in-regs)
      (x862-set-var-ea seg arg (var-ea arg))
      (let* ((lcell (pop lcells)))
        (if (setq reg (x862-assign-register-var arg))
          (x862-init-regvar seg arg reg (x862-vloc-ea vloc))
          (x862-bind-var seg arg vloc lcell))
        (setq vloc (%i+ vloc *x862-target-node-size*)))))
  (when opt
    (if (x862-hard-opt-p opt)
      (setq vloc (apply #'x862-initopt seg vloc optsupvloc lcells (nthcdr (- (length lcells) numopt) lcells) opt)
            lcells (nthcdr numopt lcells))

      (dolist (var (%car opt))
        (if (memq var passed-in-regs)
          (x862-set-var-ea seg var (var-ea var))
          (let* ((lcell (pop lcells)))
            (if (setq reg (x862-assign-register-var var))
              (x862-init-regvar seg var reg (x862-vloc-ea vloc))
              (x862-bind-var seg var vloc lcell))
            (setq vloc (+ vloc *x862-target-node-size*)))))))

  (when rest
    (if lexpr
      (progn
        (if (setq reg (x862-assign-register-var rest))
          (progn
            (x862-copy-register seg reg *x862-arg-z*)
            (x862-set-var-ea seg rest reg))
            (let* ((loc *x862-vstack*))
              (x862-vpush-register seg *x862-arg-z* :reserved)
              (x862-note-top-cell rest)
              (x862-bind-var seg rest loc *x862-top-vstack-lcell*))))
      (let* ((rvloc (+ vloc (* 2 *x862-target-node-size* nkeys))))
        (if (setq reg (x862-assign-register-var rest))
          (x862-init-regvar seg rest reg (x862-vloc-ea rvloc))
          (x862-bind-var seg rest rvloc (pop lcells))))))
    (when keys
      (apply #'x862-init-keys seg vloc lcells keys))
  (x862-seq-bind seg (%car auxen) (%cadr auxen)))


(defun x862-initopt (seg vloc spvloc lcells splcells vars inits spvars)
  (with-x86-local-vinsn-macros (seg)
    (dolist (var vars vloc)
      (let* ((initform (pop inits))
             (spvar (pop spvars))
             (lcell (pop lcells))
             (splcell (pop splcells))
             (reg (x862-assign-register-var var))
             (regloadedlabel (if reg (backend-get-next-label))))
        (unless (nx-null initform)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (x862-compare-ea-to-nil seg crf (x862-make-compound-cd 0 skipinitlabel) (x862-vloc-ea spvloc)  x86::x86-e-bits t))
            (if reg
              (x862-form seg reg regloadedlabel initform)
              (x862-register-to-stack seg (x862-one-untargeted-reg-form seg initform ($ *x862-arg-z*)) (x862-vloc-ea vloc)))
            (@ skipinitlabel)))
        (if reg
          (progn
            (x862-init-regvar seg var reg (x862-vloc-ea vloc))
            (@ regloadedlabel))
          (x862-bind-var seg var vloc lcell))
        (when spvar
          (if (setq reg (x862-assign-register-var spvar))
            (x862-init-regvar seg spvar reg (x862-vloc-ea spvloc))
            (x862-bind-var seg spvar spvloc splcell))))
      (setq vloc (%i+ vloc *x862-target-node-size*))
      (if spvloc (setq spvloc (%i+ spvloc *x862-target-node-size*))))))

(defun x862-init-keys (seg vloc lcells allow-others keyvars keysupp keyinits keykeys)
  (declare (ignore keykeys allow-others))
  (with-x86-local-vinsn-macros (seg)
    (dolist (var keyvars)
      (let* ((spvar (pop keysupp))
             (initform (pop keyinits))
             (reg (x862-assign-register-var var))
             (regloadedlabel (if reg (backend-get-next-label)))
             (var-lcell (pop lcells))
             (sp-lcell (pop lcells))
             (sploc (%i+ vloc *x862-target-node-size*)))
        (unless (nx-null initform)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (x862-compare-ea-to-nil seg crf (x862-make-compound-cd 0 skipinitlabel) (x862-vloc-ea sploc)  x86::x86-e-bits t))
            (if reg
              (x862-form seg reg regloadedlabel initform)
              (x862-register-to-stack seg (x862-one-untargeted-reg-form seg initform ($ *x862-arg-z*)) (x862-vloc-ea vloc)))
            (@ skipinitlabel)))
        (if reg
          (progn
            (x862-init-regvar seg var reg (x862-vloc-ea vloc))
            (@ regloadedlabel))
          (x862-bind-var seg var vloc var-lcell))
        (when spvar
          (if (setq reg (x862-assign-register-var spvar))
            (x862-init-regvar seg spvar reg (x862-vloc-ea sploc))
            (x862-bind-var seg spvar sploc sp-lcell))))
      (setq vloc (%i+ vloc (* 2 *x862-target-node-size*))))))

;;; Vpush register r, unless var gets a globally-assigned register.
;;; Return NIL if register was vpushed, else var.
(defun x862-vpush-arg-register (seg reg var)
  (when var
    (let* ((bits (nx-var-bits var)))
      (declare (fixnum bits))
      (if (logbitp $vbitreg bits)
        var
        (progn 
          (x862-vpush-register seg reg :reserved)
          nil)))))


;;; nargs has been validated, arguments defaulted and canonicalized.
;;; Save caller's context, then vpush any argument registers that
;;; didn't get global registers assigned to their variables.
;;; Return a list of vars/nils for each argument register 
;;;  (nil if vpushed, var if still in arg_reg).
(defun x862-argregs-entry (seg revargs &optional variable-args-entry)
  (with-x86-local-vinsn-macros (seg)
    (let* ((nargs (length revargs))
           (reg-vars ()))
      (declare (type (unsigned-byte 16) nargs))
      (unless variable-args-entry
        (if (<= nargs *x862-target-num-arg-regs*) ; caller didn't vpush anything
          (! save-lisp-context-no-stack-args)
          (let* ((offset (* (the fixnum (- nargs *x862-target-num-arg-regs*)) *x862-target-node-size*)))
            (declare (fixnum offset))
            (! save-lisp-context-offset offset))))
      (target-arch-case
       (:x8632
	(destructuring-bind (&optional zvar yvar &rest stack-args) revargs
	  (let* ((nstackargs (length stack-args)))
	    (x862-set-vstack (* nstackargs *x862-target-node-size*))
	    (dotimes (i nstackargs)
	      (x862-new-vstack-lcell :reserved *x862-target-lcell-size* 0 nil))
	    (if (>= nargs 2)
	      (push (x862-vpush-arg-register seg ($ *x862-arg-y*) yvar) reg-vars))
	    (if (>= nargs 1)
	      (push (x862-vpush-arg-register seg ($ *x862-arg-z*) zvar) reg-vars)))))
       (:x8664
	(destructuring-bind (&optional zvar yvar xvar &rest stack-args) revargs
	  (let* ((nstackargs (length stack-args)))
	    (x862-set-vstack (* nstackargs *x862-target-node-size*))
	    (dotimes (i nstackargs)
	      (x862-new-vstack-lcell :reserved *x862-target-lcell-size* 0 nil))
	    (if (>= nargs 3)
	      (push (x862-vpush-arg-register seg ($ x8664::arg_x) xvar) reg-vars))
	    (if (>= nargs 2)
	      (push (x862-vpush-arg-register seg ($ *x862-arg-y*) yvar) reg-vars))
	    (if (>= nargs 1)
	      (push (x862-vpush-arg-register seg ($ *x862-arg-z*) zvar) reg-vars))))))
      reg-vars)))

;;; Just required args.
;;; Since this is just a stupid bootstrapping port, always save 
;;; lisp context.
(defun x862-req-nargs-entry (seg rev-fixed-args)
  (let* ((nargs (length rev-fixed-args)))
    (declare (type (unsigned-byte 16) nargs))
    (with-x86-local-vinsn-macros (seg)
      (unless *x862-reckless*
        (! check-exact-nargs nargs))
      (x862-argregs-entry seg rev-fixed-args))))

;;; No more &optional args than register args; all &optionals default
;;; to NIL and none have supplied-p vars.  No &key/&rest.
(defun x862-simple-opt-entry (seg rev-opt-args rev-req-args)
  (let* ((min (length rev-req-args))
         (nopt (length rev-opt-args))
         (max (+ min nopt)))
    (declare (type (unsigned-byte 16) min nopt max))
    (with-x86-local-vinsn-macros (seg)
      (unless *x862-reckless*
        (if rev-req-args
          (! check-min-max-nargs min max)
          (! check-max-nargs max)))
      (if (> min *x862-target-num-arg-regs*)
        (! save-lisp-context-in-frame)
        (if (<= max *x862-target-num-arg-regs*)
          (! save-lisp-context-no-stack-args)
          (! save-lisp-context-variable-arg-count)))
      (if (= nopt 1)
        (! default-1-arg min)
        (if (= nopt 2)
          (! default-2-args min)
          (! default-3-args min)))
      (x862-argregs-entry seg (append rev-opt-args rev-req-args) t))))

;;; if "num-fixed" is > 0, we've already ensured that at least that many args
;;; were provided; that may enable us to generate better code for saving the
;;; argument registers.
;;; We're responsible for computing the caller's VSP and saving
;;; caller's state.
(defun x862-lexpr-entry (seg num-fixed)
  (with-x86-local-vinsn-macros (seg)
    (! save-lexpr-argregs num-fixed)
    ;; The "lexpr" (address of saved nargs register, basically
    ;; is now in arg_z
    (! build-lexpr-frame)
    (dotimes (i num-fixed)
      (! copy-lexpr-argument (- num-fixed i)))))


(defun x862-structured-initopt (seg lcells vloc context vars inits spvars)
  (with-x86-local-vinsn-macros (seg)
    (dolist (var vars vloc)
      (let* ((initform (pop inits))
             (spvar (pop spvars))
             (spvloc (%i+ vloc *x862-target-node-size*))
             (var-lcell (pop lcells))
             (sp-lcell (pop lcells)))
        (unless (nx-null initform)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (x862-compare-ea-to-nil seg crf (x862-make-compound-cd 0 skipinitlabel) (x862-vloc-ea spvloc) x86::x86-e-bits t))
            (x862-register-to-stack seg (x862-one-untargeted-reg-form seg initform ($ *x862-arg-z*)) (x862-vloc-ea vloc))
            (@ skipinitlabel)))
        (x862-bind-structured-var seg var vloc var-lcell context)
        (when spvar
          (x862-bind-var seg spvar spvloc sp-lcell)))
      (setq vloc (%i+ vloc (* 2 *x862-target-node-size*))))))



(defun x862-structured-init-keys (seg lcells vloc context allow-others keyvars keysupp keyinits keykeys)
  (declare (ignore keykeys allow-others))
  (with-x86-local-vinsn-macros (seg)
    (dolist (var keyvars)
      (let* ((spvar (pop keysupp))
             (initform (pop keyinits))
             (sploc (%i+ vloc *x862-target-node-size*))
             (var-lcell (pop lcells))
             (sp-reg ($ *x862-arg-z*))
             (sp-lcell (pop lcells)))
        (unless (nx-null initform)
          (x862-stack-to-register seg (x862-vloc-ea sploc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (x862-compare-register-to-nil seg crf (x862-make-compound-cd 0 skipinitlabel) sp-reg x86::x86-e-bits t))
            (x862-register-to-stack seg (x862-one-untargeted-reg-form seg initform ($ *x862-arg-z*)) (x862-vloc-ea vloc))
            (@ skipinitlabel)))
        (x862-bind-structured-var seg var vloc var-lcell context)
        (when spvar
          (x862-bind-var seg spvar sploc sp-lcell)))
      (setq vloc (%i+ vloc (* 2 *x862-target-node-size*))))))

(defun x862-vloc-ea (n &optional vcell-p)
  (setq n (make-memory-spec (dpb memspec-frame-address memspec-type-byte n)))
  (if vcell-p
    (make-vcell-memory-spec n)
    n))

(defun x862-form (seg vreg xfer form &aux (note (acode-source-note form)))
  (flet ((main (seg vreg xfer form)
           (if (nx-null form)
             (x862-nil seg vreg xfer)
             (if (nx-t form)
               (x862-t seg vreg xfer)
               (let* ((op nil)
                      (fn nil))
                 (if (and (consp form)
                          (setq fn (svref *x862-specials* (%ilogand #.operator-id-mask (setq op (acode-operator form))))))
                   (if (and (null vreg)
                            (%ilogbitp operator-acode-subforms-bit op)
                            (%ilogbitp operator-assignment-free-bit op))
                     (dolist (f (%cdr form) (x862-branch seg xfer))
                       (x862-form seg nil nil f ))
                     (apply fn seg vreg xfer (%cdr form)))
                   (compiler-bug "x862-form ? ~s" form)))))))
    (if note
      (let* ((start (x862-emit-note seg :source-location-begin note))
             (bits (main seg vreg xfer form))
             (end (x862-emit-note seg :source-location-end)))
        (setf (vinsn-note-peer start) end
              (vinsn-note-peer end) start)
        (push start *x862-emitted-source-notes*)
        bits)
      (main seg vreg xfer form))))

;;; dest is a float reg - form is acode
(defun x862-form-float (seg freg xfer form)
  (declare (ignore xfer))
  (when (or (nx-null form)(nx-t form))(compiler-bug "x862-form to freg ~s" form))
  (when (and (= (get-regspec-mode freg) hard-reg-class-fpr-mode-double)
             (x862-form-typep form 'double-float))
    ;; kind of screwy - encoding the source type in the dest register spec
    (set-node-regspec-type-modes freg hard-reg-class-fpr-type-double))
  (let* ((fn nil))
    (if (and (consp form)
             (setq fn (svref *x862-specials* (%ilogand #.operator-id-mask (acode-operator form)))))      
      (apply fn seg freg nil (%cdr form))
      (compiler-bug "x862-form ? ~s" form))))



(defun x862-form-typep (form type)
  (acode-form-typep form type *x862-trust-declarations*)
)

(defun x862-form-type (form)
  (acode-form-type form *x862-trust-declarations*))
  
(defun x862-use-operator (op seg vreg xfer &rest forms)
  (declare (dynamic-extent forms))
  (apply (svref *x862-specials* (%ilogand operator-id-mask op)) seg vreg xfer forms))

;;; Returns true iff lexical variable VAR isn't setq'ed in FORM.
;;; Punts a lot ...
(defun x862-var-not-set-by-form-p (var form)
  (let* ((bits (nx-var-bits var)))
    (or (not (%ilogbitp $vbitsetq bits))
        (x862-setqed-var-not-set-by-form-p var form (logbitp $vbitclosed bits)))))

(defun x862-setqed-var-not-set-by-form-p (var form &optional closed)
  (setq form (acode-unwrapped-form form))
  (or (atom form)
      (x86-constant-form-p form)
      (x862-lexical-reference-p form)
      (let ((op (acode-operator form))
            (subforms nil))
        (if (eq op (%nx1-operator setq-lexical))
          (and (neq var (cadr form))
               (x862-setqed-var-not-set-by-form-p var (caddr form)))
          (and (or (not closed)
                   (logbitp operator-side-effect-free-bit op))
               (flet ((not-set-in-formlist (formlist)
                        (dolist (subform formlist t)
                          (unless (x862-setqed-var-not-set-by-form-p var subform closed) (return)))))
                 (if
                   (cond ((%ilogbitp operator-acode-subforms-bit op) (setq subforms (%cdr form)))
                         ((%ilogbitp operator-acode-list-bit op) (setq subforms (cadr form))))
                   (not-set-in-formlist subforms)
                   (and (or (eq op (%nx1-operator call))
                            (eq op (%nx1-operator lexical-function-call)))
                        (x862-setqed-var-not-set-by-form-p var (cadr form))
                        (setq subforms (caddr form))
                        (not-set-in-formlist (car subforms))
                        (not-set-in-formlist (cadr subforms))))))))))

(defun x862-check-fixnum-overflow (seg target &optional labelno)
  (with-x86-local-vinsn-macros (seg)
    (if *x862-open-code-inline*
      (let* ((no-overflow (backend-get-next-label)))
        (! set-bigits-and-header-for-fixnum-overflow target (aref *backend-labels* (or labelno no-overflow)))
        (! %allocate-uvector target)
        (! set-bigits-after-fixnum-overflow target)
        (when labelno
          (-> labelno))
        (@ no-overflow))
      (if labelno
        (! fix-fixnum-overflow-ool-and-branch target (aref *backend-labels* labelno))
        (! fix-fixnum-overflow-ool target)))))

(defun x862-nil (seg vreg xfer)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if (eq vreg :push)
      (progn
        (! vpush-fixnum (target-nil-value))
        (^))
      (progn
        (if (x862-for-value-p vreg)
          (ensuring-node-target (target vreg)
            (! load-nil target)))
        (x862-branch seg (x862-cd-false xfer))))))

(defun x862-t (seg vreg xfer)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if (eq vreg :push)
      (progn
        (! vpush-fixnum (target-t-value))
        (^))
      (progn
        (if (x862-for-value-p vreg)
          (ensuring-node-target (target vreg)
            (! load-t target)))
        (x862-branch seg (x862-cd-true xfer))))))

(defun x862-for-value-p (vreg)
  (and vreg (not (backend-crf-p vreg))))

(defun x862-mvpass (seg form &optional xfer)
  (with-x86-local-vinsn-macros (seg)
    (x862-form seg  ($ *x862-arg-z*) (logior (or xfer 0) $backend-mvpass-mask) form)))

(defun x862-adjust-vstack (delta)
  (x862-set-vstack (%i+ *x862-vstack* delta)))

(defun x862-set-vstack (new)
  (setq *x862-vstack* new))


;;; Emit a note at the end of the segment.
(defun x862-emit-note (seg class &rest info)
  (declare (dynamic-extent info))
  (let* ((note (make-vinsn-note class info)))
    (append-dll-node (vinsn-note-label note) seg)
    note))

;;; Emit a note immediately before the target vinsn.
(defun x86-prepend-note (vinsn class &rest info)
  (declare (dynamic-extent info))
  (let* ((note (make-vinsn-note class info)))
    (insert-dll-node-before (vinsn-note-label note) vinsn)
    note))

(defun x862-close-note (seg note)
  (let* ((end (close-vinsn-note note)))
    (append-dll-node (vinsn-note-label end) seg)
    end))






(defun x862-stack-to-register (seg memspec reg)
  (with-x86-local-vinsn-macros (seg)
    (let* ((offset (memspec-frame-address-offset memspec)))
      (if (and *x862-tos-reg*
               (= offset (- *x862-vstack* *x862-target-node-size*)))
        (x862-copy-register seg reg *x862-tos-reg*)
        (! vframe-load reg offset  *x862-vstack*)))))

(defun x862-lcell-to-register (seg lcell reg)
  (with-x86-local-vinsn-macros (seg)
    (! lcell-load reg lcell (x862-vstack-mark-top))))

(defun x862-register-to-lcell (seg reg lcell)
  (with-x86-local-vinsn-macros (seg)
    (! lcell-store reg lcell (x862-vstack-mark-top))))

(defun x862-register-to-stack (seg reg memspec)
  (with-x86-local-vinsn-macros (seg)
    (! vframe-store reg (memspec-frame-address-offset memspec) *x862-vstack*)))


(defun x862-ea-open (ea)
  (if (and ea (not (typep ea 'lreg)) (addrspec-vcell-p ea))
    (make-memory-spec (memspec-frame-address-offset ea))
    ea))

(defun x862-set-NARGS (seg n)
  (if (> n call-arguments-limit)
    (error "~s exceeded." call-arguments-limit)
    (with-x86-local-vinsn-macros (seg)
      (! set-nargs n))))

(defun x862-assign-register-var (v)
  (let ((bits (nx-var-bits v)))
    (when (%ilogbitp $vbitreg bits)
      (%ilogand bits $vrefmask))))

(defun x862-single-float-bits (the-sf)
  (single-float-bits the-sf))

(defun x862-double-float-bits (the-df)
  (double-float-bits the-df))

(defun x862-push-immediate (seg xfer form)
  (with-x86-local-vinsn-macros (seg)
    (if (typep form 'character)
      (! vpush-fixnum (logior (ash (char-code form) 8)
			      (arch::target-subtag-char (backend-target-arch *target-backend*))))
      (let* ((reg (x862-register-constant-p form)))
        (if reg
          (! vpush-register reg)
          (let* ((lab (x86-immediate-label form)))
            (! vpush-constant lab)))))
    (x862-branch seg xfer)))

      
(pushnew (%nx1-operator immediate) *x862-operator-supports-push*)  
(defun x862-immediate (seg vreg xfer form)
  (if (eq vreg :push)
    (x862-push-immediate seg xfer form)
    (with-x86-local-vinsn-macros (seg vreg xfer)
      (if vreg
        (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (or (and (typep form 'double-float) (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
                     (and (typep form 'short-float)(= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))))
          (if (zerop form)
            (if (eql form 0.0d0)
              (! zero-double-float-register vreg)
              (! zero-single-float-register vreg))
            (if (typep form 'short-float)
              (let* ((lab (x86-single-float-constant-label form)))
                (! load-single-float-constant vreg lab))
              (let* ((lab (x86-double-float-constant-label form)))
                (! load-double-float-constant vreg lab))))
	  (target-arch-case
	   (:x8632
	    (if (and (= (hard-regspec-class vreg) hard-reg-class-gpr)
		     (member (get-regspec-mode vreg)
			     '(hard-reg-class-gpr-mode-u32
			       hard-reg-class-gpr-mode-s32
			       hard-reg-class-gpr-mode-address))
		     (or (typep form '(unsigned-byte 32))
			 (typep form '(signed-byte 32))))
	      ;; The bits fit.  Get them in the register somehow.
	      (if (typep form '(signed-byte 32))
		(x862-lri seg vreg form)
		(x862-lriu seg vreg form))
	      (ensuring-node-target (target vreg)
		(if (characterp form)
		  (! load-character-constant target (char-code form))
		  (x862-store-immediate seg form target)))))
	   (:x8664
	    (if (and (typep form '(unsigned-byte 32))
		     (= (hard-regspec-class vreg) hard-reg-class-gpr)
		     (= (get-regspec-mode vreg)
			hard-reg-class-gpr-mode-u32))
	      (x862-lri seg vreg form)
	      (ensuring-node-target
		  (target vreg)
		(if (characterp form)
		  (! load-character-constant target (char-code form))
		  (x862-store-immediate seg form target)))))))
        (if (and (listp form) *load-time-eval-token* (eq (car form) *load-time-eval-token*))
          (x862-store-immediate seg form ($ *x862-temp0*))))
      (^))))

(defun x862-register-constant-p (form)
  (and (consp form)
           (or (memq form *x862-vcells*)
               (memq form *x862-fcells*))
           (%cdr form)))

(defun x862-store-immediate (seg imm dest)
  (with-x86-local-vinsn-macros (seg)
    (let* ((reg (x862-register-constant-p imm)))
      (if reg
        (x862-copy-register seg dest reg)
        (let* ((lab (x86-immediate-label imm)))
          (! ref-constant dest lab)))
      dest)))


;;; Returns label iff form is (local-go <tag>) and can go without adjusting stack.
(defun x862-go-label (form)
  (let ((current-stack (x862-encode-stack)))
    (while (and (acode-p form) (or (eq (acode-operator form) (%nx1-operator progn))
                                   (eq (acode-operator form) (%nx1-operator local-tagbody))))
      (setq form (caadr form)))
    (when (acode-p form)
      (let ((op (acode-operator form)))
        (if (and (eq op (%nx1-operator local-go))
                 (x862-equal-encodings-p (%caddr (%cadr form)) current-stack))
          (%cadr (%cadr form))
          (if (and (eq op (%nx1-operator local-return-from))
                   (nx-null (caddr form)))
            (let ((tagdata (car (cadr form))))
              (and (x862-equal-encodings-p (cdr tagdata) current-stack)
                   (null (caar tagdata))
                   (< 0 (cdar tagdata) $backend-mvpass)
                   (cdar tagdata)))))))))

(defun x862-single-valued-form-p (form)
  (setq form (acode-unwrapped-form-value form))
  (or (nx-null form)
      (nx-t form)
      (if (acode-p form)
        (let ((op (acode-operator form)))
          (or (%ilogbitp operator-single-valued-bit op)
              (and (eql op (%nx1-operator values))
                   (let ((values (cadr form)))
                     (and values (null (cdr values)))))
              nil                       ; Learn about functions someday
              )))))

(defun x862-box-s32 (seg node-dest s32-src)
  (with-x86-local-vinsn-macros (seg)
    (if (target-arch-case
         (:x8632 nil)
         (:x8664 t))
      (! box-fixnum node-dest s32-src)
      (let* ((arg_z ($ *x862-arg-z*))
             (imm0 ($ *x862-imm0* :mode :s32)))
        (x862-copy-register seg imm0 s32-src)
        (! call-subprim (subprim-name->offset '.SPmakes32))
        (x862-copy-register seg node-dest arg_z)))))

(defun x862-box-s64 (seg node-dest s64-src)
  (with-x86-local-vinsn-macros (seg)
    (if (target-arch-case
	 (:x8632 (error "bug"))
         (:x8664 *x862-open-code-inline*))
      (let* ((no-overflow (backend-get-next-label)))
        (! %set-z-flag-if-s64-fits-in-fixnum node-dest s64-src)
        (! cbranch-true (aref *backend-labels* no-overflow) x86::x86-e-bits)
        (! setup-bignum-alloc-for-s64-overflow s64-src)
        (! %allocate-uvector node-dest)
        (! set-bigits-after-fixnum-overflow node-dest)
        (@ no-overflow))
      (let* ((arg_z ($ *x862-arg-z*))
             (imm0 (make-wired-lreg *x862-imm0* :mode (get-regspec-mode s64-src))))
        (x862-copy-register seg imm0 s64-src)
        (! call-subprim (subprim-name->offset '.SPmakes64))
        (x862-copy-register seg node-dest arg_z)))))

(defun x862-box-u32 (seg node-dest u32-src)
  (with-x86-local-vinsn-macros (seg)
    (target-arch-case
     (:x8632
      (let* ((arg_z ($ *x862-arg-z*))
	     (imm0 ($ *x862-imm0* :mode :u32)))
	(x862-copy-register seg imm0 u32-src)
	(! call-subprim (subprim-name->offset '.SPmakeu32))
	(x862-copy-register seg node-dest arg_z)))
     (:x8664
      (! box-fixnum node-dest u32-src)))))

(defun x862-box-u64 (seg node-dest u64-src)
  (with-x86-local-vinsn-macros (seg)
    (if (target-arch-case
         (:x8632 (error "bug"))
         (:x8664 *x862-open-code-inline*))
      (let* ((no-overflow (backend-get-next-label)))
        (! %set-z-flag-if-u64-fits-in-fixnum node-dest u64-src)
        (! cbranch-true (aref *backend-labels* no-overflow) x86::x86-e-bits)
        (! setup-bignum-alloc-for-u64-overflow u64-src)
        (! %allocate-uvector node-dest)
        (! set-bigits-after-fixnum-overflow node-dest)
        (@ no-overflow))
      (let* ((arg_z ($ *x862-arg-z*))
             (imm0 ($ *x862-imm0* :mode :u64)))
        (x862-copy-register seg imm0 u64-src)
        (! call-subprim (subprim-name->offset '.SPmakeu64))
        (x862-copy-register seg node-dest arg_z)))))

(defun x862-single->heap (seg dest src)
  (with-x86-local-vinsn-macros (seg)
    (! setup-single-float-allocation)
    (! %allocate-uvector dest)
    (! set-single-float-value dest src)))

(defun x862-double->heap (seg dest src)
  (with-x86-local-vinsn-macros (seg)
    (! setup-double-float-allocation)
    (! %allocate-uvector dest)
    (! set-double-float-value dest src)))


(defun x862-vref1 (seg vreg xfer type-keyword src unscaled-idx index-known-fixnum)  
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (let* ((arch (backend-target-arch *target-backend*))
             (is-node (member type-keyword (arch::target-gvector-types arch)))
             (is-1-bit (member type-keyword (arch::target-1-bit-ivector-types arch)))

             (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
             (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
             (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
             (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
             (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector)))
             (vreg-class (and (not (eq vreg :push)) (hard-regspec-class vreg)))
             (vreg-mode
              (if (or (eql vreg-class hard-reg-class-gpr)
                      (eql vreg-class hard-reg-class-fpr))
                (get-regspec-mode vreg)
                hard-reg-class-gpr-mode-invalid)))
        (cond
          (is-node
           (if (eq vreg :push)
             (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
               (! push-misc-ref-c-node  src index-known-fixnum)
               (! push-misc-ref-node src unscaled-idx))
             (ensuring-node-target (target vreg)
               (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                 (! misc-ref-c-node target src index-known-fixnum)
                 (if unscaled-idx
                   (! misc-ref-node target src unscaled-idx)
                   (with-node-target (src) unscaled-idx
                     (x862-absolute-natural seg unscaled-idx  nil (ash index-known-fixnum *x862-target-fixnum-shift*))
                     (! misc-ref-node target src unscaled-idx)))))))
          (is-32-bit
           (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-32-bit-constant-index arch)))
             (cond ((eq type-keyword :single-float-vector)
                    (with-fp-target () (fp-val :single-float)
                      (if (and (eql vreg-class hard-reg-class-fpr)
                               (eql vreg-mode hard-reg-class-fpr-mode-single))
                        (setq fp-val vreg))
                      (! misc-ref-c-single-float fp-val src index-known-fixnum)
                      (if (eql vreg-class hard-reg-class-fpr)
                        (<- fp-val)
                        (ensuring-node-target (target vreg)
			  (target-arch-case
			   (:x8632 (x862-single->heap seg target fp-val))
			   (:x8664 (! single->node target fp-val)))))))
                   (t
		    (with-additional-imm-reg ()
		      (with-imm-target () temp
			(if is-signed
			  (! misc-ref-c-s32 temp src index-known-fixnum)
			  (! misc-ref-c-u32 temp src index-known-fixnum))
			(ensuring-node-target (target vreg)
			  (if (eq type-keyword :simple-string)
			    (! u32->char target temp)
			    (target-arch-case
			     (:x8632
			      (if is-signed
				(x862-box-s32 seg target temp)
				(x862-box-u32 seg target temp)))
			     (:x8664
			      (! box-fixnum target temp)))))))))
             (with-imm-target () idx-reg
               (if index-known-fixnum
		 (x862-absolute-natural seg idx-reg nil (ash index-known-fixnum 2))
		 (! scale-32bit-misc-index idx-reg unscaled-idx))
	       (cond ((eq type-keyword :single-float-vector)
		      (with-fp-target () (fp-val :single-float)
			(if (and (eql vreg-class hard-reg-class-fpr)
				 (eql vreg-mode hard-reg-class-fpr-mode-single))
			  (setq fp-val vreg))
			(! misc-ref-single-float fp-val src idx-reg)
			(if (eq vreg-class hard-reg-class-fpr)
			  (<- fp-val)
			  (ensuring-node-target (target vreg)
			    (target-arch-case
			     (:x8632 (x862-single->heap seg target fp-val))
			     (:x8664 (! single->node target fp-val)))))))
		     (t
		      (with-imm-target () temp
			(if is-signed
			  (! misc-ref-s32 temp src idx-reg)
			  (! misc-ref-u32 temp src idx-reg))
			(ensuring-node-target (target vreg)
			  (if (eq type-keyword :simple-string)
			    (! u32->char target temp)
			    (target-arch-case
			     (:x8632 (if is-signed
				       (x862-box-s32 seg target temp)
				       (x862-box-u32 seg target temp)))
			     (:x8664 (! box-fixnum target temp)))))))))))
          (is-8-bit
           (with-imm-target () temp
             (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-8-bit-constant-index arch)))
               (if is-signed
                 (! misc-ref-c-s8 temp src index-known-fixnum)
                 (! misc-ref-c-u8 temp src index-known-fixnum))
	       (with-additional-imm-reg ()
		 (with-imm-target () idx-reg
		   (if index-known-fixnum
		     (x862-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) index-known-fixnum))
		     (! scale-8bit-misc-index idx-reg unscaled-idx))
		   (if is-signed
		     (! misc-ref-s8 temp src idx-reg)
		     (! misc-ref-u8 temp src idx-reg)))))
             (if (eq type-keyword :simple-string)
               (ensuring-node-target (target vreg)
                 (! u32->char target temp))
               (if (and (= vreg-mode hard-reg-class-gpr-mode-u8)
                        (eq type-keyword :unsigned-8-bit-vector))
                 (x862-copy-register seg vreg temp)
                 (ensuring-node-target (target vreg)
                   (! box-fixnum target temp))))))
          (is-16-bit
           (with-imm-target () temp
             (ensuring-node-target (target vreg)
               (if (and index-known-fixnum
                        (<= index-known-fixnum (arch::target-max-16-bit-constant-index arch)))
                 (if is-signed
                   (! misc-ref-c-s16 temp src index-known-fixnum)
                   (! misc-ref-c-u16 temp src index-known-fixnum))
		 (with-imm-target () idx-reg
		   (if index-known-fixnum
		     (x862-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 1)))
		     (! scale-16bit-misc-index idx-reg unscaled-idx))
		   (if is-signed
		     (! misc-ref-s16 temp src idx-reg)
		     (! misc-ref-u16 temp src idx-reg))))
               (! box-fixnum target temp))))
          ;; Down to the dregs.
          (is-64-bit
           (with-node-target (src) extra
             (unless unscaled-idx (setq unscaled-idx extra)))
           (case type-keyword
             (:double-float-vector
              (with-fp-target () (fp-val :double-float)
                (if (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-double))
                  (setq fp-val vreg))
                (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                  (! misc-ref-c-double-float fp-val src index-known-fixnum)
                  (progn
                    (if index-known-fixnum
                      (x862-absolute-natural seg unscaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3))))
                    (! misc-ref-double-float fp-val src unscaled-idx)))
                (if (eq vreg-class hard-reg-class-fpr)
                  (<- fp-val)
                  (ensuring-node-target (target vreg)
                    (x862-double->heap seg target fp-val)))))
             ((:signed-64-bit-vector :fixnum-vector)
              (ensuring-node-target (target vreg)

                (with-imm-target () (s64-reg :s64)
                  (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                    (! misc-ref-c-s64 s64-reg src index-known-fixnum)
                    (progn
                      (if index-known-fixnum
                        (x862-absolute-natural seg unscaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3))))
                      (! misc-ref-s64 s64-reg src unscaled-idx)))
                  (if (eq type-keyword :fixnum-vector)
                    (! box-fixnum target s64-reg)
                    (x862-box-s64 seg target s64-reg)))))
             (t
                (with-imm-target () (u64-reg :u64)
                  (if (eql vreg-mode hard-reg-class-gpr-mode-u64)
                    (setq u64-reg vreg))
                  (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                    (! misc-ref-c-u64 u64-reg src index-known-fixnum)
                    (progn
                      (if index-known-fixnum
                        (x862-absolute-natural seg unscaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3))))
                      (! misc-ref-u64 u64-reg src unscaled-idx)))
                  (unless (eq u64-reg vreg)
                    (ensuring-node-target (target vreg)
                      (x862-box-u64 seg target u64-reg)))))))
          (t
           (unless is-1-bit
             (nx-error "~& unsupported vector type: ~s"
                       type-keyword))
           (ensuring-node-target (target vreg)
             (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-1-bit-constant-index arch)))
               (! misc-ref-c-bit-fixnum target src index-known-fixnum)
	       (with-imm-target () bitnum
		 (if index-known-fixnum
		   (x862-lri seg bitnum index-known-fixnum)
		   (! scale-1bit-misc-index bitnum unscaled-idx))
                 (! nref-bit-vector-fixnum target bitnum src))))))))
    (^)))

;;; safe = T means assume "vector" is miscobj, do bounds check.
;;; safe = fixnum means check that subtag of vector = "safe" and do
;;;        bounds check.
;;; safe = nil means crash&burn.
;;; This mostly knows how to reference the elements of an immediate miscobj.
(defun x862-vref (seg vreg xfer type-keyword vector index safe)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (when *x862-full-safety*
      (unless vreg (setq vreg *x862-arg-z*)))
    (if (null vreg)
      (progn
        (x862-form seg nil nil vector)
        (x862-form seg nil xfer index))
      (let* ((index-known-fixnum (acode-fixnum-form-p index))
             (unscaled-idx nil)
             (src nil))
        (if (or safe (not index-known-fixnum))
          (multiple-value-setq (src unscaled-idx)
            (x862-two-untargeted-reg-forms seg vector *x862-arg-y* index *x862-arg-z*))
          (setq src (x862-one-untargeted-reg-form seg vector *x862-arg-z*)))
        (when safe
          (if (typep safe 'fixnum)
            (! trap-unless-typecode= src safe))
          (unless index-known-fixnum
            (! trap-unless-fixnum unscaled-idx))
          (! check-misc-bound unscaled-idx src))
        (x862-vref1 seg vreg xfer type-keyword src unscaled-idx index-known-fixnum)))))



(defun x862-aset2 (seg vreg xfer  array i j new safe type-keyword  dim0 dim1)
  (target-arch-case
   (:x8632 (error "not for x8632 yet")))
  (with-x86-local-vinsn-macros (seg target)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (x862-constant-value-ok-for-type-keyword type-keyword new))
           (needs-memoization (and is-node (x862-acode-needs-memoization new)))
           (src)
           (unscaled-i)
           (unscaled-j)
           (val-reg (x862-target-reg-for-aset vreg type-keyword))
           (constidx
            (and dim0 dim1 i-known-fixnum j-known-fixnum
                 (>= i-known-fixnum 0)
                 (>= j-known-fixnum 0)
                 (< i-known-fixnum dim0)
                 (< j-known-fixnum dim1)
                 (+ (* i-known-fixnum dim1) j-known-fixnum))))
      (progn
        (if constidx
          (multiple-value-setq (src val-reg)
            (x862-two-targeted-reg-forms seg array ($ *x862-temp0*) new val-reg))
          (multiple-value-setq (src unscaled-i unscaled-j val-reg)
            (if needs-memoization
              (progn
                (x862-four-targeted-reg-forms seg
                                              array ($ *x862-temp0*)
                                              i ($ x8664::arg_x)
                                              j ($ *x862-arg-y*)
                                              new val-reg)
                (values ($ *x862-temp0*) ($ x8664::arg_x) ($ *x862-arg-y*) ($ *x862-arg-z*)))
              (x862-four-untargeted-reg-forms seg
                                              array ($ *x862-temp0*)
                                              i ($ x8664::arg_x)
                                              j ($ *x862-arg-y*)
                                              new val-reg))))
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*))
          (when (and (= (hard-regspec-class val-reg) hard-reg-class-gpr)
                     (logbitp (hard-regspec-value val-reg)
                              *backend-imm-temps*))
            (use-imm-temp (hard-regspec-value val-reg)))
          (when safe      
            (when (typep safe 'fixnum)
              (! trap-unless-simple-array-2
                 src
                 (dpb safe target::arrayH.flags-cell-subtag-byte
                      (ash 1 $arh_simple_bit))
                 (nx-error-for-simple-2d-array-type type-keyword)))
            (unless i-known-fixnum
              (! trap-unless-fixnum unscaled-i))
            (unless j-known-fixnum
              (! trap-unless-fixnum unscaled-j)))
          (with-imm-target () dim1
            (let* ((idx-reg ($ *x862-arg-y*)))
              (if constidx
                (if needs-memoization
                  (x862-lri seg *x862-arg-y* (ash constidx *x862-target-fixnum-shift*)))
                (progn
                  (if safe                  
                    (! check-2d-bound dim1 unscaled-i unscaled-j src)
                    (! 2d-dim1 dim1 src))
                  (! 2d-unscaled-index idx-reg dim1 unscaled-i unscaled-j)))
              (let* ((v ($ x8664::arg_x)))
                (! array-data-vector-ref v src)
                (x862-vset1 seg vreg xfer type-keyword v idx-reg constidx val-reg (x862-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization)))))))))


(defun x862-aset3 (seg vreg xfer  array i j k new safe type-keyword  dim0 dim1 dim2)
  (target-arch-case
   (:x8632 (error "not for x8632 yet")))
  (with-x86-local-vinsn-macros (seg target)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (k-known-fixnum (acode-fixnum-form-p k))
           (arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (x862-constant-value-ok-for-type-keyword type-keyword new))
           (needs-memoization (and is-node (x862-acode-needs-memoization new)))
           (src)
           (unscaled-i)
           (unscaled-j)
           (unscaled-k)
           (val-reg (x862-target-reg-for-aset vreg type-keyword))
           (constidx
            (and dim0 dim1 dim2 i-known-fixnum j-known-fixnum k-known-fixnum
                 (>= i-known-fixnum 0)
                 (>= j-known-fixnum 0)
                 (>= k-known-fixnum 0)
                 (< i-known-fixnum dim0)
                 (< j-known-fixnum dim1)
                 (< k-known-fixnum dim2)
                 (+ (* i-known-fixnum dim1 dim2)
                    (* j-known-fixnum dim2)
                    k-known-fixnum))))
      (progn
        (if constidx
          (multiple-value-setq (src val-reg)
            (x862-two-targeted-reg-forms seg array ($ *x862-temp0*) new val-reg))
          (progn
            (setq src ($ x8664::temp1)
                  unscaled-i ($ *x862-temp0*)
                  unscaled-j ($ x8664::arg_x)
                  unscaled-k ($ *x862-arg-y*))
            (x862-push-register
             seg
             (x862-one-untargeted-reg-form seg array ($ *x862-arg-z*)))
            (x862-four-targeted-reg-forms seg
                                          i ($ *x862-temp0*)
                                          j ($ x8664::arg_x)
                                          k ($ *x862-arg-y*)
                                          new val-reg)
            (x862-pop-register seg src)))
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*))
          (when (and (= (hard-regspec-class val-reg) hard-reg-class-gpr)
                     (logbitp (hard-regspec-value val-reg)
                              *backend-imm-temps*))
            (use-imm-temp (hard-regspec-value val-reg)))
        
          (when safe      
            (when (typep safe 'fixnum)
              (! trap-unless-simple-array-3
                 src
                 (dpb safe target::arrayH.flags-cell-subtag-byte
                      (ash 1 $arh_simple_bit))
                 (nx-error-for-simple-3d-array-type type-keyword)))
            (unless i-known-fixnum
              (! trap-unless-fixnum unscaled-i))
            (unless j-known-fixnum
              (! trap-unless-fixnum unscaled-j))
            (unless k-known-fixnum
              (! trap-unless-fixnum unscaled-k)))
          (with-imm-target () dim1
            (with-imm-target (dim1) dim2
              (let* ((idx-reg ($ *x862-arg-y*)))
                (if constidx
                  (when needs-memoization
                    (x862-lri seg idx-reg (ash constidx *x862-target-fixnum-shift*)))
                  (progn
                    (if safe                  
                      (! check-3d-bound dim1 dim2 unscaled-i unscaled-j unscaled-k src)
                      (! 3d-dims dim1 dim2 src))
                    (! 3d-unscaled-index idx-reg dim1 dim2 unscaled-i unscaled-j unscaled-k)))
                (let* ((v ($ x8664::arg_x)))
                  (! array-data-vector-ref v src)
                  (x862-vset1 seg vreg xfer type-keyword v idx-reg constidx val-reg (x862-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization))))))))))


(defun x862-aref2 (seg vreg xfer array i j safe typekeyword &optional dim0 dim1)
  (target-arch-case
   (:x8632 (error "not for x8632 yet")))
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (src)
           (unscaled-i)
           (unscaled-j)
           (constidx
            (and dim0 dim1 i-known-fixnum j-known-fixnum
                 (>= i-known-fixnum 0)
                 (>= j-known-fixnum 0)
                 (< i-known-fixnum dim0)
                 (< j-known-fixnum dim1)
                 (+ (* i-known-fixnum dim1) j-known-fixnum))))
      (if constidx
        (setq src (x862-one-targeted-reg-form seg array ($ *x862-arg-z*)))
        (multiple-value-setq (src unscaled-i unscaled-j)
          (x862-three-untargeted-reg-forms seg
                                           array x8664::arg_x
                                           i *x862-arg-y*
                                           j *x862-arg-z*)))
      (when safe        
        (when (typep safe 'fixnum)
          (! trap-unless-simple-array-2
             src
             (dpb safe target::arrayH.flags-cell-subtag-byte
                  (ash 1 $arh_simple_bit))
             (nx-error-for-simple-2d-array-type typekeyword)))
        (unless i-known-fixnum
          (! trap-unless-fixnum unscaled-i))
        (unless j-known-fixnum
          (! trap-unless-fixnum unscaled-j)))
      (with-node-target (src) idx-reg
        (with-imm-target () dim1
          (unless constidx
            (if safe                    
              (! check-2d-bound dim1 unscaled-i unscaled-j src)
              (! 2d-dim1 dim1 src))
            (! 2d-unscaled-index idx-reg dim1 unscaled-i unscaled-j))
          (with-node-target (idx-reg) v
            (! array-data-vector-ref v src)
            (x862-vref1 seg vreg xfer typekeyword v idx-reg constidx)))))))

(defun x862-aref3 (seg vreg xfer array i j k safe typekeyword &optional dim0 dim1 dim2)
  (target-arch-case
   (:x8632 (error "not for x8632 yet")))
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (k-known-fixnum (acode-fixnum-form-p k))
           (src)
           (unscaled-i)
           (unscaled-j)
           (unscaled-k)
           (constidx
            (and dim0 dim1 i-known-fixnum j-known-fixnum k-known-fixnum
                 (>= i-known-fixnum 0)
                 (>= j-known-fixnum 0)
                 (>= k-known-fixnum 0)
                 (< i-known-fixnum dim0)
                 (< j-known-fixnum dim1)
                 (< k-known-fixnum dim2)
                 (+ (* i-known-fixnum dim1 dim2)
                    (* j-known-fixnum dim2)
                    k-known-fixnum))))
      (if constidx
        (setq src (x862-one-targeted-reg-form seg array ($ *x862-arg-z*)))
        (multiple-value-setq (src unscaled-i unscaled-j unscaled-k)
          (x862-four-untargeted-reg-forms seg
                                           array *x862-temp0*
                                           i x8664::arg_x
                                           j *x862-arg-y*
                                           k *x862-arg-z*)))
      (when safe        
        (when (typep safe 'fixnum)
          (! trap-unless-simple-array-3
             src
             (dpb safe target::arrayH.flags-cell-subtag-byte
                  (ash 1 $arh_simple_bit))
             (nx-error-for-simple-3d-array-type typekeyword)))
        (unless i-known-fixnum
          (! trap-unless-fixnum unscaled-i))
        (unless j-known-fixnum
          (! trap-unless-fixnum unscaled-j))
        (unless k-known-fixnum
          (! trap-unless-fixnum unscaled-k)))
      (with-node-target (src) idx-reg
        (with-imm-target () dim1
          (with-imm-target (dim1) dim2
            (unless constidx
              (if safe                    
                (! check-3d-bound dim1 dim2 unscaled-i unscaled-j unscaled-k src)
                (! 3d-dims dim1 dim2 src))
              (! 3d-unscaled-index idx-reg dim1 dim2 unscaled-i unscaled-j unscaled-k))))
        (with-node-target (idx-reg) v
          (! array-data-vector-ref v src)
          (x862-vref1 seg vreg xfer typekeyword v idx-reg constidx))))))



(defun x862-natural-vset (seg vreg xfer vector index value safe)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((index-known-fixnum (acode-fixnum-form-p index))
           (arch (backend-target-arch *target-backend*))
           (src nil)
           (unscaled-idx nil))
      (with-imm-target () (target :natural)
        (if (or safe (not index-known-fixnum))
          (multiple-value-setq (src unscaled-idx target)
            (x862-three-untargeted-reg-forms seg vector *x862-arg-y* index *x862-arg-z* value (or vreg target)))
          (multiple-value-setq (src target)
            (x862-two-untargeted-reg-forms seg vector *x862-arg-y* value (or vreg target))))
        (when safe
          (with-imm-temps (target) ()   ; Don't use target in type/bounds check
            (if (typep safe 'fixnum)
              (! trap-unless-typecode= src safe))
            (unless index-known-fixnum
              (! trap-unless-fixnum unscaled-idx))
            (! check-misc-bound unscaled-idx src)))
        (target-arch-case
         
         (:x8664
          (if (and index-known-fixnum
                   (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
            (! misc-set-c-u64 target src index-known-fixnum)
            (progn
              (if index-known-fixnum
                (x862-absolute-natural seg unscaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3))))
              (! misc-set-u64 target src unscaled-idx)))))
        (<- target)                     ; should be a no-op in this case
        (^)))))


(defun x862-constant-value-ok-for-type-keyword (type-keyword form)
  (let* ((arch (backend-target-arch *target-backend*))
         (is-node  (member type-keyword (arch::target-gvector-types arch))))
    (if is-node
      (cond ((eq form *nx-nil*)
             (target-nil-value))
            ((eq form *nx-t*)
             (+ (target-nil-value) (arch::target-t-offset arch)))
            (t
             (let* ((fixval (acode-fixnum-form-p form)))
               (if fixval
                 (ash fixval (arch::target-fixnum-shift arch))))))
      (if (and (acode-p form)
               (or (eq (acode-operator form) (%nx1-operator immediate))
                   (eq (acode-operator form) (%nx1-operator fixnum))))
        (let* ((val (%cadr form))

               (typep (cond ((eq type-keyword :signed-32-bit-vector)
                             (typep val '(signed-byte 32)))
                            ((eq type-keyword :single-float-vector)
                             (typep val 'short-float))
                            ((eq type-keyword :double-float-vector)
                             (typep val 'double-float))
                            ((eq type-keyword :simple-string)
                             (typep val 'base-char))
                            ((eq type-keyword :signed-8-bit-vector)
                             (typep val '(signed-byte 8)))
                            ((eq type-keyword :unsigned-8-bit-vector)
                             (typep val '(unsigned-byte 8)))
                            ((eq type-keyword :signed-16-bit-vector) 
                             (typep val '(signed-byte 16)))
                            ((eq type-keyword :unsigned-16-bit-vector)
                             (typep val '(unsigned-byte 16)))
                            ((eq type-keyword :bit-vector)
                             (typep val 'bit)))))
          (if typep val))))))

(defun x862-target-reg-for-aset (vreg type-keyword)
  (let* ((arch (backend-target-arch *target-backend*))
         (is-node (member type-keyword (arch::target-gvector-types arch)))
         (is-1-bit (member type-keyword (arch::target-1-bit-ivector-types arch)))
         (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
         (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
         (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
         (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
         (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector)))
         (vreg-class (if (and vreg (not (eq vreg :push))) (hard-regspec-class vreg)))
         (vreg-mode (if (or (eql vreg-class hard-reg-class-gpr)
                            (eql vreg-class hard-reg-class-fpr))
                      (get-regspec-mode vreg)))
         (next-imm-target (available-imm-temp  *available-backend-imm-temps*))
         (next-fp-target (available-fp-temp *available-backend-fp-temps*))
         (acc (make-wired-lreg *x862-arg-z*)))
    (cond ((or is-node
               (eq vreg :push)
               is-1-bit
               (eq type-keyword :simple-string)
               (eq type-keyword :fixnum-vector)
               (and (eql vreg-class hard-reg-class-gpr)
                    (eql vreg-mode hard-reg-class-gpr-mode-node)))
           acc)
          ;; If there's no vreg - if we're setting for effect only, and
          ;; not for value - we can target an unboxed register directly.
          ;; Usually.
          ((null vreg)
           (cond (is-64-bit
                  (if (eq type-keyword :double-float-vector)
                    (make-unwired-lreg next-fp-target :mode hard-reg-class-fpr-mode-double)
                    (make-unwired-lreg next-imm-target :mode (if is-signed hard-reg-class-gpr-mode-s64 hard-reg-class-gpr-mode-u64))))
                 (is-32-bit
                  (if (eq type-keyword :single-float-vector)
                    (make-unwired-lreg next-fp-target :mode hard-reg-class-fpr-mode-single)
                    (make-unwired-lreg next-imm-target :mode (if is-signed hard-reg-class-gpr-mode-s32 hard-reg-class-gpr-mode-u32))))
                 (is-16-bit
                  (make-unwired-lreg next-imm-target :mode (if is-signed hard-reg-class-gpr-mode-s16 hard-reg-class-gpr-mode-u16)))
                 (is-8-bit
                  (make-unwired-lreg next-imm-target :mode (if is-signed hard-reg-class-gpr-mode-s8 hard-reg-class-gpr-mode-u8)))
                 (t "Bug: can't determine operand size for ~s" type-keyword)))
          ;; Vreg is non-null.  We might be able to use it directly.
          (t
           (let* ((lreg (if vreg-mode
                          (make-unwired-lreg (lreg-value vreg)))))
             (if 
               (cond
                 (is-64-bit
                  (if (eq type-keyword :double-float-vector)
                    (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-double))
                      (if is-signed
                        (and (eql vreg-class hard-reg-class-gpr)
                                 (eql vreg-mode hard-reg-class-gpr-mode-s64))
                        (and (eql vreg-class hard-reg-class-gpr)
                                 (eql vreg-mode hard-reg-class-gpr-mode-u64)))))
                   (is-32-bit
                    (if (eq type-keyword :single-float-vector)
                      (and (eql vreg-class hard-reg-class-fpr)
                               (eql vreg-mode hard-reg-class-fpr-mode-single))
                      (if is-signed
                        (and (eql vreg-class hard-reg-class-gpr)
                                 (or (eql vreg-mode hard-reg-class-gpr-mode-s32)
                                     (eql vreg-mode hard-reg-class-gpr-mode-s64)))
                        (and (eql vreg-class hard-reg-class-gpr)
                                 (or (eql vreg-mode hard-reg-class-gpr-mode-u32)
                                     (eql vreg-mode hard-reg-class-gpr-mode-u64)
                                     (eql vreg-mode hard-reg-class-gpr-mode-s64))))))
                   (is-16-bit
                    (if is-signed
                      (and (eql vreg-class hard-reg-class-gpr)
                               (or (eql vreg-mode hard-reg-class-gpr-mode-s16)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s32)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s64)))
                      (and (eql vreg-class hard-reg-class-gpr)
                               (or (eql vreg-mode hard-reg-class-gpr-mode-u16)
                                   (eql vreg-mode hard-reg-class-gpr-mode-u32)
                                   (eql vreg-mode hard-reg-class-gpr-mode-u64)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s32)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s64)))))
                   (t
                    (if is-signed
                      (and (eql vreg-class hard-reg-class-gpr)
                               (or (eql vreg-mode hard-reg-class-gpr-mode-s8)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s16)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s32)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s64)))
                      (and (eql vreg-class hard-reg-class-gpr)
                               (or (eql vreg-mode hard-reg-class-gpr-mode-u8)
                                   (eql vreg-mode hard-reg-class-gpr-mode-u16)
                                   (eql vreg-mode hard-reg-class-gpr-mode-u32)
                                   (eql vreg-mode hard-reg-class-gpr-mode-u64)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s16)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s32)
                                   (eql vreg-mode hard-reg-class-gpr-mode-s64))))))
               lreg
               acc))))))

(defun x862-unboxed-reg-for-aset (seg type-keyword result-reg safe constval)
  (with-x86-local-vinsn-macros (seg)
    (let* ((arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
           (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
           (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
           (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
           (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector)))
           (result-is-node-gpr (and (eql (hard-regspec-class result-reg)
                                         hard-reg-class-gpr)
                                    (eql (get-regspec-mode result-reg)
                                         hard-reg-class-gpr-mode-node)))
           (next-imm-target (available-imm-temp *available-backend-imm-temps*))
           (next-fp-target (available-fp-temp *available-backend-fp-temps*)))
      (if (or is-node (not result-is-node-gpr))
        result-reg
        (cond (is-64-bit
               (if (eq type-keyword :double-float-vector)
                 (let* ((reg (make-unwired-lreg next-fp-target :mode hard-reg-class-fpr-mode-double)))
                   (if safe
                     (! get-double? reg result-reg)
                     (! get-double reg result-reg))
                   reg)
                 (if is-signed
                   (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s64)))
                     (if (eq type-keyword :fixnum-vector)
                       (progn
                         (when safe
                           (! trap-unless-fixnum result-reg))
                         (! fixnum->signed-natural reg result-reg))
                       (! unbox-s64 reg result-reg))
                     reg)
                   (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u64)))
                     (! unbox-u64 reg result-reg)
                     reg))))
              (is-32-bit
               ;; Generally better to use a GPR for the :SINGLE-FLOAT-VECTOR
               ;; case here.
               (if is-signed             
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s32)))
                   (if (eq type-keyword :fixnum-vector)
                     (progn
                       (when safe
                         (! trap-unless-fixnum result-reg))
                       (! fixnum->signed-natural reg result-reg))
                     (! unbox-s32 reg result-reg))
                   reg)
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u32)))
                   (cond ((eq type-keyword :simple-string)
                          (if (characterp constval)
                            (x862-lri seg reg (char-code constval))
                            (! unbox-base-char reg result-reg)))
                         ((eq type-keyword :single-float-vector)
                          (if (typep constval 'single-float)
                            (x862-lri seg reg (single-float-bits constval))
                            (progn
                              (when safe
                                (! trap-unless-single-float result-reg))
                              (! single-float-bits reg result-reg))))
                         (t
                          (if (typep constval '(unsigned-byte 32))
                            (x862-lri seg reg constval)
                            (if *x862-reckless*
			      (target-arch-case
			       (:x8632 (! unbox-u32 reg result-reg))
			       (:x8664 (! %unbox-u32 reg result-reg)))
                              (! unbox-u32 reg result-reg)))))
                   reg)))
              (is-16-bit
               (if is-signed
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s16)))
                   (if (typep constval '(signed-byte 16))
                     (x862-lri seg reg constval)
                     (if *x862-reckless*
                       (! %unbox-s16 reg result-reg)
                       (! unbox-s16 reg result-reg)))
                   reg)
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u16)))
                   (if (typep constval '(unsigned-byte 16))
                     (x862-lri seg reg constval)
                     (if *x862-reckless*
                       (! %unbox-u16 reg result-reg)
                       (! unbox-u16 reg result-reg)))
                   reg)))
              (is-8-bit
               (if is-signed
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s8)))
                   (if (typep constval '(signed-byte 8))
                     (x862-lri seg reg constval)
                     (if *x862-reckless*
                       (! %unbox-s8 reg result-reg)
                       (! unbox-s8 reg result-reg)))
                   reg)
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u8)))
                   (if (typep constval '(unsigned-byte 8))
                     (x862-lri seg reg constval)
                     (if *x862-reckless*
                       (! %unbox-u8 reg result-reg)
                       (! unbox-u8 reg result-reg)))
                   reg)))
              (t
                 (let* ((reg result-reg))
                   (unless (typep constval 'bit)
                     (when safe
                       (! trap-unless-bit reg )))
                   reg)))))))


;;; xxx
(defun x862-vset1 (seg vreg xfer type-keyword src unscaled-idx index-known-fixnum val-reg unboxed-val-reg constval node-value-needs-memoization)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (is-1-bit (member type-keyword (arch::target-1-bit-ivector-types arch)))
           (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
           (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
           (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
           (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
           (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector))))
      (cond ((and is-node node-value-needs-memoization)
             (unless (and (eql (hard-regspec-value src) (target-arch-case
							 (:x8632 x8632::temp0)
							 (:x8664 x8664::arg_x)))
                          (eql (hard-regspec-value unscaled-idx) *x862-arg-y*)
                          (eql (hard-regspec-value val-reg) *x862-arg-z*))
               (compiler-bug "Bug: invalid register targeting for gvset: ~s" (list src unscaled-idx val-reg)))
             (! call-subprim-3 val-reg (subprim-name->offset '.SPgvset) src unscaled-idx val-reg))
            (is-node
             (if (and index-known-fixnum (<= index-known-fixnum
                                             (target-word-size-case
                                              (32 (arch::target-max-32-bit-constant-index arch))
                                              (64 (arch::target-max-64-bit-constant-index arch)))))
               (if (typep constval '(signed-byte 32))
                 (! misc-set-immediate-c-node constval src index-known-fixnum)
                 (! misc-set-c-node val-reg src index-known-fixnum))
               (progn
                 (if index-known-fixnum
                   (x862-absolute-natural seg unscaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum *x862-target-node-shift*))))
                 (if (typep constval '(signed-byte 32))
                   (! misc-set-immediate-node constval src unscaled-idx)
                   (! misc-set-node val-reg src unscaled-idx)))))
            (t
	     (with-additional-imm-reg (src unscaled-idx val-reg)
	       (with-imm-target (unboxed-val-reg) scaled-idx
		 (cond
		   (is-64-bit
		    (if (and index-known-fixnum
			     (<= index-known-fixnum
				 (arch::target-max-64-bit-constant-index arch)))
		      (if (eq type-keyword :double-float-vector)
			(! misc-set-c-double-float unboxed-val-reg src index-known-fixnum)
			(if is-signed
			  (! misc-set-c-s64 unboxed-val-reg src index-known-fixnum)
			  (! misc-set-c-u64 unboxed-val-reg src index-known-fixnum)))
		      (progn
			(if index-known-fixnum
			  (x862-absolute-natural seg unscaled-idx nil (+ (arch::target-misc-dfloat-offset arch) (ash index-known-fixnum 3))))
			(if (eq type-keyword :double-float-vector)
			  (! misc-set-double-float unboxed-val-reg src unscaled-idx)
			  (if is-signed
			    (! misc-set-s64 unboxed-val-reg src unscaled-idx)
			    (! misc-set-u64 unboxed-val-reg src unscaled-idx))))))
		   (is-32-bit
		    (if (and index-known-fixnum
			     (<= index-known-fixnum
				 (arch::target-max-32-bit-constant-index arch)))
		      (if (eq type-keyword :single-float-vector)
			(if (eq (hard-regspec-class unboxed-val-reg)
				hard-reg-class-fpr)
			  (! misc-set-c-single-float unboxed-val-reg src index-known-fixnum)
			  (! misc-set-c-u32 unboxed-val-reg src index-known-fixnum))
			(if is-signed
			  (! misc-set-c-s32 unboxed-val-reg src index-known-fixnum)
			  (! misc-set-c-u32 unboxed-val-reg src index-known-fixnum)))
		      (progn
			(if index-known-fixnum
			  (x862-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 2)))
			  (! scale-32bit-misc-index scaled-idx unscaled-idx))
			(if (and (eq type-keyword :single-float-vector)
				 (eql (hard-regspec-class unboxed-val-reg)
				      hard-reg-class-fpr))
			  (! misc-set-single-float unboxed-val-reg src scaled-idx)
			  (if is-signed
			    (! misc-set-s32 unboxed-val-reg src scaled-idx)
			    (! misc-set-u32 unboxed-val-reg src scaled-idx))))))
		   (is-16-bit
		    (if (and index-known-fixnum
			     (<= index-known-fixnum
				 (arch::target-max-16-bit-constant-index arch)))
		      (if is-signed
			(! misc-set-c-s16 unboxed-val-reg src index-known-fixnum)
			(! misc-set-c-u16 unboxed-val-reg src index-known-fixnum))
		      (progn
			(if index-known-fixnum
			  (x862-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 1)))
			  (! scale-16bit-misc-index scaled-idx unscaled-idx))
			(if is-signed
			  (! misc-set-s16 unboxed-val-reg src scaled-idx)
			  (! misc-set-u16 unboxed-val-reg src scaled-idx)))))
		   (is-8-bit
		    (if (and index-known-fixnum
			     (<= index-known-fixnum
				 (arch::target-max-8-bit-constant-index arch)))
		      (if is-signed
			(! misc-set-c-s8 unboxed-val-reg src index-known-fixnum)
			(! misc-set-c-u8  unboxed-val-reg src index-known-fixnum))
		      (progn
			(if index-known-fixnum
			  (x862-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) index-known-fixnum))
			  (! scale-8bit-misc-index scaled-idx unscaled-idx))
			(if is-signed
			  (! misc-set-s8 unboxed-val-reg src scaled-idx)
			  (! misc-set-u8 unboxed-val-reg src scaled-idx)))))
		   (is-1-bit
		    (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-1-bit-constant-index arch)))
		      (if constval
			(if (zerop constval)
			  (! set-constant-bit-to-zero src index-known-fixnum)
			  (! set-constant-bit-to-one src index-known-fixnum))
			(progn
			  (! set-constant-bit-to-variable-value src index-known-fixnum val-reg)))
		      (progn
			(if index-known-fixnum
			  (x862-lri seg scaled-idx index-known-fixnum)
			  (! scale-1bit-misc-index scaled-idx unscaled-idx))
			(if constval
			  (if (zerop constval)
			    (! nset-variable-bit-to-zero src scaled-idx)
			    (! nset-variable-bit-to-one src scaled-idx))
			  (progn
			    (! nset-variable-bit-to-variable-value src scaled-idx val-reg)))))))))))
      (when (and vreg val-reg) (<- val-reg))
      (^))))


(defun x862-vset (seg vreg xfer type-keyword vector index value safe)
  (with-x86-local-vinsn-macros (seg)
    (let* ((arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (x862-constant-value-ok-for-type-keyword type-keyword value))
           (needs-memoization (and is-node (x862-acode-needs-memoization value)))
           (index-known-fixnum (acode-fixnum-form-p index)))
      (let* ((src (target-arch-case
		   (:x8632 ($ x8632::temp0))
		   (:x8664 ($ x8664::arg_x))))
             (unscaled-idx ($ *x862-arg-y*))
             (result-reg ($ *x862-arg-z*)))
        (cond (needs-memoization
               (x862-three-targeted-reg-forms seg
                                              vector src
                                              index unscaled-idx
                                              value result-reg))
              (t
               (setq result-reg (x862-target-reg-for-aset vreg type-keyword))
	       (target-arch-case
		(:x8632
		 (with-node-temps (src) ()
		   (x862-three-targeted-reg-forms seg
						  vector src
						  index unscaled-idx
						  value result-reg)))
		(:x8664
                 (multiple-value-setq (src unscaled-idx result-reg)
                   (x862-three-untargeted-reg-forms seg
                                                  vector src
                                                  index unscaled-idx
                                                  value result-reg))))))
        (when safe
	  (let* ((*available-backend-imm-temps* *available-backend-imm-temps*)
		 (value (if (eql (hard-regspec-class result-reg)
				 hard-reg-class-gpr)
			  (hard-regspec-value result-reg)))
		 (result-is-imm nil))
	    (when (and value (logbitp value *available-backend-imm-temps*))
	      (setq *available-backend-imm-temps* (bitclr value *available-backend-imm-temps*))
	      (setq result-is-imm t))
	    (if (typep safe 'fixnum)
	      (if result-is-imm
		(with-additional-imm-reg (src safe)
		  (! trap-unless-typecode= src safe))
		(! trap-unless-typecode= src safe)))
	    (unless index-known-fixnum
	      (! trap-unless-fixnum unscaled-idx))
	    (if result-is-imm
	      (with-additional-imm-reg (unscaled-idx src)
		(! check-misc-bound unscaled-idx src))
	      (! check-misc-bound unscaled-idx src))))
        (x862-vset1 seg vreg xfer type-keyword src unscaled-idx index-known-fixnum result-reg (x862-unboxed-reg-for-aset seg type-keyword result-reg safe constval) constval needs-memoization)))))



(defun x862-tail-call-alias (immref sym &optional arglist)
  (let ((alias (cdr (assq sym *x862-tail-call-aliases*))))
    (if (and alias (or (null arglist) (eq (+ (length (car arglist)) (length (cadr arglist))) (cdr alias))))
      (make-acode (%nx1-operator immediate) (car alias))
      immref)))

;;; If BODY is essentially an APPLY involving an &rest arg, try to avoid
;;; consing it.
(defun x862-eliminate-&rest (body rest key-p auxen rest-values)
  (when (and rest (not key-p) (not (cadr auxen)) rest-values)
    (when (eq (logand (the fixnum (nx-var-bits rest))
                      (logior $vsetqmask (ash -1 $vbitspecial)
                              (ash 1 $vbitclosed) (ash 1 $vbitsetq) (ash 1 $vbitcloseddownward)))
              0)               ; Nothing but simple references
      (do* ()
           ((not (acode-p body)))
        (let* ((op (acode-operator body)))
          (if (or (eq op (%nx1-operator lexical-function-call))
                  (eq op (%nx1-operator call)))
            (destructuring-bind (fn-form (stack-args reg-args) &optional spread-p) (%cdr body)
               (unless (and (eq spread-p t)
                           (eq (x862-lexical-reference-p (%car reg-args)) rest))
                (return nil))
              (flet ((independent-of-all-values (form)        
                       (setq form (acode-unwrapped-form-value form))
                       (or (x86-constant-form-p form)
                           (let* ((lexref (x862-lexical-reference-p form)))
                             (and lexref 
                                  (neq lexref rest)
                                  (dolist (val rest-values t)
                                    (unless (x862-var-not-set-by-form-p lexref val)
                                      (return))))))))
                (unless (or (eq op (%nx1-operator lexical-function-call))
                            (independent-of-all-values fn-form))
                  (return nil))
                (if (dolist (s stack-args t)
                          (unless (independent-of-all-values s)
                            (return nil)))
                  (let* ((arglist (append stack-args rest-values)))
                    (return
                     (make-acode op 
                                 fn-form 
                                 (if (<= (length arglist) *x862-target-num-arg-regs*)
                                   (list nil (reverse arglist))
                                   (list (butlast arglist *x862-target-num-arg-regs*)
                                         (reverse (last arglist *x862-target-num-arg-regs*))))
                                 nil)))
                  (return nil))))
            (if (eq op (%nx1-operator local-block))
              (setq body (%cadr body))
              (if (and (eq op (%nx1-operator if))
                       (eq (x862-lexical-reference-p (%cadr body)) rest))
                (setq body (%caddr body))
                (return nil)))))))))

(defun x862-call-fn (seg vreg xfer fn arglist spread-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (when spread-p
      (destructuring-bind (stack-args reg-args) arglist
        (when (and (null (cdr reg-args))
                   (nx-null (acode-unwrapped-form-value (car reg-args))))
          (setq spread-p nil)
          (let* ((nargs (length stack-args)))
            (declare (fixnum nargs))
            (if (<= nargs *x862-target-num-arg-regs*)
              (setq arglist (list nil (reverse stack-args)))
              (setq arglist (list (butlast stack-args *x862-target-num-arg-regs*) (reverse (last stack-args *x862-target-num-arg-regs*)))))))))
    (let* ((lexref (x862-lexical-reference-p fn))
           (simple-case (or (fixnump fn)
                            (typep fn 'lreg)
                            (x862-immediate-function-p fn)
                            (and 
                             lexref
                             (not spread-p)
                             (flet ((all-simple (args)
                                      (dolist (arg args t)
                                        (when (and arg (not (x862-var-not-set-by-form-p lexref arg)))
                                          (return)))))
                               (and (all-simple (car arglist))
                                    (all-simple (cadr arglist))
                                    (setq fn (var-ea lexref)))))))
           (cstack *x862-cstack*)
           (top *x862-top-vstack-lcell*)
           (vstack *x862-vstack*))
      (setq xfer (or xfer 0))
      (when (and (eq xfer $backend-return)
                 (eq 0 *x862-undo-count*)
                 (acode-p fn)
                 (eq (acode-operator fn) (%nx1-operator immediate))
                 (symbolp (cadr fn)))
        (setq fn (x862-tail-call-alias fn (%cadr fn) arglist)))
      
      (if (and (eq xfer $backend-return) (not (x862-tailcallok xfer)))
        (progn
          (x862-call-fn seg vreg $backend-mvpass fn arglist spread-p)
          (x862-set-vstack (%i+ (if simple-case 0 *x862-target-node-size*) vstack))
          (setq  *x862-cstack* cstack)
          (let ((*x862-returning-values* t)) (x862-do-return seg)))
        (let* ((mv-p (x862-mv-p xfer))
               (mv-return-label (if (and mv-p
                                         (not (x862-tailcallok xfer)))
                                  (backend-get-next-label))))
          (unless simple-case
            (x862-vpush-register seg (x862-one-untargeted-reg-form seg fn *x862-arg-z*))
            (setq fn (x862-vloc-ea vstack)))
          (x862-invoke-fn seg fn (x862-arglist seg arglist mv-return-label) spread-p xfer mv-return-label)
          (if (and (logbitp $backend-mvpass-bit xfer)
                   (not simple-case))
            (progn
              (! save-values)
              (! vstack-discard 1)
              (x862-set-nargs seg 0)
              (! recover-values))
            (unless (or mv-p simple-case)
              (! vstack-discard 1)))
          (x862-set-vstack vstack)
          (setq *x862-top-vstack-lcell* top)
          (setq *x862-cstack* cstack)
          (when (or (logbitp $backend-mvpass-bit xfer) (not mv-p))
            (<- *x862-arg-z*)
            (x862-branch seg (logand (lognot $backend-mvpass-mask) xfer)))))
      nil)))

(defun x862-restore-full-lisp-context (seg)
  (with-x86-local-vinsn-macros (seg)
    (! restore-full-lisp-context)))

(defun x862-emit-aligned-label (seg labelnum)
  (with-x86-local-vinsn-macros (seg)
    (! emit-aligned-label (aref *backend-labels* labelnum))
    (@ labelnum)
    (target-arch-case
     (:x8632
      (! recover-fn))
     (:x8664
      (! recover-fn-from-rip)))))

  
(defun x862-call-symbol (seg jump-p)
  (with-x86-local-vinsn-macros (seg)
    (if jump-p
      (! jump-known-symbol)
      (! call-known-symbol *x862-arg-z*))))

;;; Nargs = nil -> multiple-value case.
(defun x862-invoke-fn (seg fn nargs spread-p xfer &optional mvpass-label)
  (with-x86-local-vinsn-macros (seg)
    (let* ((f-op (acode-unwrapped-form-value fn))
           (immp (and (consp f-op)
                      (eq (%car f-op) (%nx1-operator immediate))))
           (symp (and immp (symbolp (%cadr f-op))))
           (label-p (and (fixnump fn) 
                         (locally (declare (fixnum fn))
                           (and (= fn -2) (- fn)))))
           (tail-p (eq xfer $backend-return))
           (func (if (consp f-op) (%cadr f-op)))
           (a-reg nil)
           (lfunp (and (acode-p f-op) 
                       (eq (acode-operator f-op) (%nx1-operator simple-function))))
           (expression-p (or (typep fn 'lreg) (and (fixnump fn) (not label-p))))
           (callable (or symp lfunp label-p))
           (destreg (if symp ($ *x862-fname*) (unless label-p ($ *x862-temp0*))))
           (alternate-tail-call
            (and tail-p label-p *x862-tail-label* (eql nargs *x862-tail-nargs*) (not spread-p))))
      (when expression-p
        ;;Have to do this before spread args, since might be vsp-relative.
        (if nargs
          (x862-do-lexical-reference seg destreg fn)
          (x862-copy-register seg destreg fn)))
      (if (or symp lfunp)
        (setq func (if symp
                     (x862-symbol-entry-locative func)
                     (x862-afunc-lfun-ref func))
              a-reg (x862-register-constant-p func)))
      (when tail-p
        #-no-compiler-bugs
        (unless (or immp symp lfunp (typep fn 'lreg) (fixnump fn)) (compiler-bug "Well, well, well.  How could this have happened ?"))
        (when a-reg
          (x862-copy-register seg destreg a-reg))
        (unless spread-p
          (unless alternate-tail-call
            (x862-restore-nvrs seg *x862-register-restore-ea* *x862-register-restore-count* (and nargs (<= nargs *x862-target-num-arg-regs*))))))
      (if spread-p
        (progn
          (x862-set-nargs seg (%i- nargs 1))
                                        ; .SPspread-lexpr-z & .SPspreadargz preserve temp1
	  (target-arch-case
	   (:x8632
	    (! save-node-register-to-spill-area *x862-temp0*)))
          (if (eq spread-p 0)
	    (! spread-lexpr)
            (! spread-list))
	  (target-arch-case
	   (:x8632
	    (! load-node-register-from-spill-area *x862-temp0*)))

          (when (and tail-p *x862-register-restore-count*)
            (x862-restore-nvrs seg *x862-register-restore-ea* *x862-register-restore-count* nil)))
        (if nargs
          (unless alternate-tail-call (x862-set-nargs seg nargs))
          (! pop-argument-registers)))
      (if callable
        (if (not tail-p)
          (if (x862-mvpass-p xfer)
            (let* ((call-reg (if symp ($ *x862-fname*) ($ *x862-temp0*))))
              (unless mvpass-label (compiler-bug "no label for mvpass"))
              (if label-p
                (x862-copy-register seg call-reg ($ *x862-fn*))
                (if a-reg
                  (x862-copy-register seg call-reg  a-reg)
                  (x862-store-immediate seg func call-reg)))
              (if symp
                (! pass-multiple-values-symbol)
                (! pass-multiple-values))
              (when mvpass-label
                (@= mvpass-label)))
            (progn 
              (if label-p
                (progn
                  (! call-label (aref *backend-labels* 2)))
                (progn
                  (if a-reg
                    (x862-copy-register seg destreg a-reg)
                    (x862-store-immediate seg func destreg))
                  (if symp
                    (x862-call-symbol seg nil)
                    (! call-known-function))))))
          (if alternate-tail-call
            (progn
              (x862-unwind-stack seg xfer 0 0 *x862-tail-vsp*)
              (! jump (aref *backend-labels* *x862-tail-label*)))
            (progn
              (x862-unwind-stack seg xfer 0 0 #x7fffff)
              (if (and (not spread-p) nargs (%i<= nargs *x862-target-num-arg-regs*))
                (progn
                  (unless (or label-p a-reg) (x862-store-immediate seg func destreg))
                  (x862-restore-full-lisp-context seg)
                  (if label-p
                    (! jump (aref *backend-labels* 1))
                    (progn
                      (if symp
                        (x862-call-symbol seg t)
                        (! jump-known-function)))))
                (progn
                  (unless (or label-p a-reg) (x862-store-immediate seg func destreg))
                  (when label-p
                    (x862-copy-register seg *x862-temp0* *x862-fn*))

                  (cond ((or spread-p (null nargs))
                         (if symp
                           (! tail-call-sym-gen)
                           (! tail-call-fn-gen)))
                        ((%i> nargs *x862-target-num-arg-regs*)
                         (if symp
                           (! tail-call-sym-slide)
                           (! tail-call-fn-slide)))
                        (t
                         (if symp
                           (! tail-call-sym-vsp)
                           (! tail-call-fn-vsp)))))))))
        ;; The general (funcall) case: we don't know (at compile-time)
        ;; for sure whether we've got a symbol or a (local, constant)
        ;; function.
        (progn
          (unless (or (fixnump fn) (typep fn 'lreg))
            (x862-one-targeted-reg-form seg fn destreg))
          (if (not tail-p)
            (if (x862-mvpass-p xfer)
              (progn (! pass-multiple-values)
                     (when mvpass-label
                       (@= mvpass-label)))
              (! funcall))                  
            (cond ((or (null nargs) spread-p)
                   (! tail-funcall-gen))
                  ((%i> nargs *x862-target-num-arg-regs*)
                   (! tail-funcall-slide))
                  (t
                   (! restore-full-lisp-context)
                   (! tail-funcall)))))))
    nil))

(defun x862-seq-fbind (seg vreg xfer vars afuncs body p2decls)
  (let* ((old-stack (x862-encode-stack))
         (copy afuncs)
         (func nil))
    (with-x86-p2-declarations p2decls 
      (dolist (var vars) 
        (when (neq 0 (afunc-fn-refcount (setq func (pop afuncs))))
          (x862-seq-bind-var seg var (nx1-afunc-ref func))))
      (x862-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (when (neq 0 (afunc-fn-refcount (setq func (pop copy))))
          (x862-close-var seg var))))))

(defun x862-make-closure (seg afunc downward-p)
  (with-x86-local-vinsn-macros (seg)
    (flet ((var-to-reg (var target)
             (let* ((ea (var-ea (var-bits var))))
               (if ea
                 (x862-addrspec-to-reg seg (x862-ea-open ea) target)
                 (! load-nil target))
               target))
           (set-some-cells (dest cellno c0 c1 c2 c3)
             (declare (fixnum cellno))
             (! misc-set-c-node c0 dest cellno)
             (incf cellno)
             (when c1
               (! misc-set-c-node c1 dest cellno)
               (incf cellno)
               (when c2
                 (! misc-set-c-node c2 dest cellno)
                 (incf cellno)
                 (when c3
                   (! misc-set-c-node c3 dest cellno)
                   (incf cellno))))
             cellno))
      (let* ((inherited-vars (afunc-inherited-vars afunc))
             (arch (backend-target-arch *target-backend*))
             (dest ($ *x862-arg-z*))
             (vsize (+ (length inherited-vars)
		       (target-arch-case
			(:x8632 7)
			(:x8664 5))	; %closure-code%, afunc
                       1)))             ; lfun-bits
        (declare (list inherited-vars))
        (let* ((cell (target-arch-case (:x8632 6)
				       (:x8664 4))))
          (declare (fixnum cell))
          (if downward-p
            (progn
              (! make-fixed-stack-gvector
                 dest
                 (ash (logandc2 (+ vsize 2) 1) (arch::target-word-shift arch))
                 (arch::make-vheader vsize (nx-lookup-target-uvector-subtag :function)))
              (x862-open-undo $undostkblk))
            (progn
              (x862-lri seg
                        *x862-imm0*
                        (arch::make-vheader vsize (nx-lookup-target-uvector-subtag :function)))
	      (target-arch-case
	       (:x8632
		(! setup-uvector-allocation *x862-imm0*)
		(x862-lri seg *x862-imm0* (- (ash (logandc2 (+ vsize 2) 1) (arch::target-word-shift arch)) x8632::fulltag-misc)))
	       (:x8664
		(x862-lri seg x8664::imm1 (- (ash (logandc2 (+ vsize 2) 1) (arch::target-word-shift arch)) x8664::fulltag-misc))))
              (! %allocate-uvector dest)))
          (! init-nclosure *x862-arg-z*)
	  ;;; xxx --- x8632 likely to have register conflicts with *x862-ra0*
          (x862-store-immediate seg (x862-afunc-lfun-ref afunc) *x862-ra0*)
	  (target-arch-case
	   (:x8632
	    (with-node-temps (*x862-arg-z*) (t0)
	      (do* ((func *x862-ra0* nil))
		   ((null inherited-vars))
		(let* ((t0r (or func (if inherited-vars
				       (var-to-reg (pop inherited-vars) t0)))))
		  (! misc-set-c-node t0r dest cell)
		  (incf cell)))))
	   (:x8664
	    (with-node-temps (*x862-arg-z*) (t0 t1 t2 t3)
	      (do* ((func *x862-ra0* nil))
		   ((null inherited-vars))
		(let* ((t0r (or func (if inherited-vars (var-to-reg (pop inherited-vars) t0))))
		       (t1r (if inherited-vars (var-to-reg (pop inherited-vars) t1)))
		       (t2r (if inherited-vars (var-to-reg (pop inherited-vars) t2)))
		       (t3r (if inherited-vars (var-to-reg (pop inherited-vars) t3))))
		  (setq cell (set-some-cells dest cell t0r t1r t2r t3r)))))))
	  (x862-lri seg *x862-arg-y* (ash (logior (ash -1 $lfbits-noname-bit) (ash 1 $lfbits-trampoline-bit)) *x862-target-fixnum-shift*))
          (! misc-set-c-node *x862-arg-y* dest cell))
        (! finalize-closure dest)
        dest))))
        
(defun x862-symbol-entry-locative (sym)
  (setq sym (require-type sym 'symbol))
  (when (eq sym '%call-next-method-with-args)
    (setf (afunc-bits *x862-cur-afunc*)
          (%ilogior (%ilsl $fbitnextmethargsp 1) (afunc-bits *x862-cur-afunc*))))
  (or (assq sym *x862-fcells*)
      (let ((new (list sym)))
        (push new *x862-fcells*)
        new)))

(defun x862-symbol-value-cell (sym)
  (setq sym (require-type sym 'symbol))
  (or (assq sym *x862-vcells*)
      (let ((new (list sym)))
        (push new *x862-vcells*)
        (ensure-binding-index sym)
        new)))


(defun x862-symbol-locative-p (imm)
  (and (consp imm)
       (or (memq imm *x862-vcells*)
           (memq imm *x862-fcells*))))




(defun x862-immediate-function-p (f)
  (setq f (acode-unwrapped-form-value f))
  (and (acode-p f)
       (or (eq (%car f) (%nx1-operator immediate))
           (eq (%car f) (%nx1-operator simple-function)))))

(defun x86-constant-form-p (form)
  (setq form (nx-untyped-form form))
  (if form
    (or (nx-null form)
        (nx-t form)
        (and (consp form)
             (or (eq (acode-operator form) (%nx1-operator immediate))
                 (eq (acode-operator form) (%nx1-operator fixnum))
                 (eq (acode-operator form) (%nx1-operator simple-function)))))))


  
(defun x862-integer-constant-p (form mode)
  (let* ((val 
         (or (acode-fixnum-form-p (setq form (acode-unwrapped-form form)))
             (and (acode-p form)
                  (eq (acode-operator form) (%nx1-operator immediate))
                  (setq form (%cadr form))
                  (if (typep form 'integer)
                    form)))))
    (when val
      (let* ((type (mode-specifier-type mode))
             (high (numeric-ctype-high type))
             (low (numeric-ctype-low type)))
        (if (and (>= val low)
                 (<= val high))
          val
          (if (<= (integer-length val) (integer-length (- high low)))
            (if (eql 0 low)             ; type is unsigned, value is negative
              (logand high val)
              (- val (1+ (- high low))))))))))

         


(defun x86-side-effect-free-form-p (form)
  (when (consp (setq form (acode-unwrapped-form-value form)))
    (or (x86-constant-form-p form)
        ;(eq (acode-operator form) (%nx1-operator bound-special-ref))
        (if (eq (acode-operator form) (%nx1-operator lexical-reference))
          (not (%ilogbitp $vbitsetq (nx-var-bits (%cadr form))))))))

(defun x862-formlist (seg stkargs &optional revregargs)
  (with-x86-local-vinsn-macros (seg)  
    (let* ((nregs (length revregargs))
           (n nregs))
      (declare (fixnum n))
      (dolist (arg stkargs)
        (let* ((pushform (x862-acode-operator-supports-push arg)))
          (if pushform
            (progn
              (x862-form seg :push nil pushform)
              (x862-new-vstack-lcell :outgoing-argument *x862-target-lcell-size* 0 nil)
              (x862-adjust-vstack *x862-target-node-size*))
              
            (let* ((reg (x862-one-untargeted-reg-form seg arg *x862-arg-z*)))
              (x862-vpush-register-arg seg reg)))
          (incf n)))
      (when revregargs
        (let* ((zform (%car revregargs))
               (yform (%cadr revregargs))
               (xform (%caddr revregargs)))
	  (if (eq 3 nregs)
	    (progn
	      (target-arch-case (:x8632 (compiler-bug "3 reg args on x8632?")))
	      (x862-three-targeted-reg-forms seg xform ($ x8664::arg_x)
					     yform ($ *x862-arg-y*)
					     zform ($ *x862-arg-z*)))
	    (if (eq 2 nregs)
	      (x862-two-targeted-reg-forms seg yform ($ *x862-arg-y*) zform ($ *x862-arg-z*))
	      (x862-one-targeted-reg-form seg zform ($ *x862-arg-z*))))))
      n)))

(defun x862-arglist (seg args &optional mv-label)
  (with-x86-local-vinsn-macros (seg)
    (when mv-label
      (x862-vpush-label seg (aref *backend-labels* mv-label)))
    (when (car args)
      (! reserve-outgoing-frame)
      (x862-new-vstack-lcell :reserved *x862-target-lcell-size* 0 nil)
      (x862-new-vstack-lcell :reserved *x862-target-lcell-size* 0 nil)
      (setq *x862-vstack* (+  *x862-vstack* (* 2 *x862-target-node-size*))))
    (x862-formlist seg (car args) (cadr args))))


(defun x862-unboxed-integer-arg-to-reg (seg form immreg &optional ffi-arg-type)
  (let* ((mode (ecase ffi-arg-type
                 ((nil) :natural)
                 (:signed-byte :s8)
                 (:unsigned-byte :u8)
                 (:signed-halfword :s16)
                 (:unsigned-halfword :u16)
                 (:signed-fullword :s32)
                 (:unsigned-fullword :u32)
                 (:unsigned-doubleword :u64)
                 (:signed-doubleword :s64)))
         (modeval (gpr-mode-name-value mode)))
    (with-x86-local-vinsn-macros (seg)
      (let* ((value (x862-integer-constant-p form mode)))
        (if value
          (progn
            (unless (typep immreg 'lreg)
              (setq immreg (make-unwired-lreg immreg :mode modeval)))
            (if (< value 0)
              (x862-lri seg immreg value)
              (x862-lriu seg immreg value))
            immreg)
          (progn 
            (x862-one-targeted-reg-form seg form (make-wired-lreg *x862-imm0* :mode modeval))))))))


(defun x862-macptr-arg-to-reg (seg form address-reg)  
  (x862-one-targeted-reg-form seg
                              form 
                              address-reg))


(defun x862-one-lreg-form (seg form lreg)
  (let ((is-float (= (hard-regspec-class lreg) hard-reg-class-fpr)))
    (if is-float
      (x862-form-float seg lreg nil form)
      (x862-form seg lreg nil form))
    lreg))

(defun x862-one-targeted-reg-form (seg form reg)
  (x862-one-lreg-form seg form reg))

(defun x862-one-untargeted-lreg-form (seg form reg)
  (x862-one-lreg-form seg form (if (typep reg 'lreg) reg (make-unwired-lreg reg))))

(defun x862-one-untargeted-reg-form (seg form suggested)
  (with-x86-local-vinsn-macros (seg)
    (let* ((gpr-p (= (hard-regspec-class suggested) hard-reg-class-gpr))
           (node-p (if gpr-p (= (get-regspec-mode suggested) hard-reg-class-gpr-mode-node))))
      (if node-p
        (let* ((ref (x862-lexical-reference-ea form))
               (reg (backend-ea-physical-reg ref hard-reg-class-gpr)))
          (if reg
            ref
            (if (nx-null form)
              (progn
                (! load-nil suggested)
                suggested)
              (if (and (acode-p form) 
                       (eq (acode-operator form) (%nx1-operator immediate)) 
                       (setq reg (x862-register-constant-p (cadr form))))
                reg
                (x862-one-untargeted-lreg-form seg form suggested)))))
        (x862-one-untargeted-lreg-form seg form suggested)))))
             

(defun x862-push-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (a-single (if a-float (= (get-regspec-mode areg) hard-reg-class-fpr-mode-single)))
         (a-node (unless a-float (= (get-regspec-mode areg) hard-reg-class-gpr-mode-node)))
         vinsn)
    (with-x86-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (x862-vpush-register seg areg :node-temp))
        (if a-single
	  (target-arch-case
	   (:x8632
	    (setq vinsn (! temp-push-single-float areg))
            (x862-open-undo $undo-x86-c-frame))
	   (:x8664
	    (setq vinsn (! vpush-single-float areg))
	    (x862-new-vstack-lcell :single-float *x862-target-lcell-size* 0 nil)
	    (x862-open-undo $undo-x86-c-frame)))
	  (target-arch-case
	   (:x8632
	    (if a-float
	      (progn
		(setq vinsn (! temp-push-double-float areg))
                (x862-open-undo $undo-x86-c-frame))
	      (progn
		(setq vinsn (! temp-push-unboxed-word areg))
		(x862-open-undo $undo-x86-c-frame))))
	   (:x8664
            (setq vinsn
                  (if a-float
                    (! temp-push-double-float areg)
                    (! temp-push-unboxed-word areg)))
            (x862-open-undo $undo-x86-c-frame)))))
      vinsn)))

(defun x862-pop-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (a-single (if a-float (= (get-regspec-mode areg) hard-reg-class-fpr-mode-single)))
         (a-node (unless a-float (= (get-regspec-mode areg) hard-reg-class-gpr-mode-node)))
         vinsn)
    (with-x86-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (x862-vpop-register seg areg))
        (if a-single
          (target-arch-case
	   (:x8632
	    (setq vinsn (! temp-pop-single-float areg))
            (x862-close-undo))
	   (:x8664
            (setq vinsn (! vpop-single-float areg))
            (setq *x862-top-vstack-lcell* (lcell-parent *x862-top-vstack-lcell*))
            (x862-adjust-vstack (- *x862-target-node-size*))))
          (target-arch-case
	   (:x8632
	    (if a-float
	      (progn
		(setq vinsn (! temp-pop-double-float areg))
		(x862-close-undo))
	      (progn
		(setq vinsn (! temp-pop-unboxed-word areg))
		(x862-close-undo))))
	   (:x8664
            (setq vinsn
                  (if a-float
                    (! temp-pop-double-float areg)
                    (! temp-pop-unboxed-word areg)))
            (x862-close-undo)))))
      vinsn)))

;;; If reg is a GPR and of mode node, use arg_z, otherwise, just return
;;; reg.
(defun x862-acc-reg-for (reg)
  (with-x86-local-vinsn-macros (seg)
    (if (and (eql (hard-regspec-class reg) hard-reg-class-gpr)
           (eql (get-regspec-mode reg) hard-reg-class-gpr-mode-node))
      ($ *x862-arg-z*)
      reg)))

;;; The compiler often generates superfluous pushes & pops.  Try to
;;; eliminate them.
(defun x862-elide-pushes (seg push-vinsn pop-vinsn)
  (with-x86-local-vinsn-macros (seg)
    (let* ((pushed-reg (svref (vinsn-variable-parts push-vinsn) 0))
           (popped-reg (svref (vinsn-variable-parts pop-vinsn) 0))
           (same-reg (eq (hard-regspec-value pushed-reg)
                         (hard-regspec-value popped-reg)))
           (csp-p (vinsn-attribute-p push-vinsn :csp)))
      (when csp-p                       ; vsp case is harder.
        (let* ((pushed-reg-is-set (vinsn-sequence-sets-reg-p
                                   push-vinsn pop-vinsn pushed-reg))
               (popped-reg-is-set (if same-reg
                                    pushed-reg-is-set
                                    (vinsn-sequence-sets-reg-p
                                     push-vinsn pop-vinsn popped-reg))))
          (unless (and pushed-reg-is-set popped-reg-is-set)
            (unless same-reg
              (let* ((copy (if (eq (hard-regspec-class pushed-reg)
                                   hard-reg-class-fpr)
                             (if (= (get-regspec-mode pushed-reg)
                                    hard-reg-class-fpr-mode-double)
                               (! copy-double-float popped-reg pushed-reg)
                               (! copy-single-float popped-reg pushed-reg))
                             (! copy-gpr popped-reg pushed-reg))))
                (remove-dll-node copy)
                (if pushed-reg-is-set
                  (insert-dll-node-after copy push-vinsn)
                  (insert-dll-node-before copy push-vinsn))))
            (elide-vinsn push-vinsn)
            (elide-vinsn pop-vinsn)))))))
                
        
;;; we never leave the first form pushed (the 68K compiler had some subprims that
;;; would vpop the first argument out of line.)
(defun x862-two-targeted-reg-forms (seg aform areg bform breg)
  (unless (typep areg 'lreg)
    (warn "~s is not an lreg (1/2)" areg))
  (unless (typep breg 'lreg)
    (warn "~s is not an lreg (2/2)" breg))
  (let* ((avar (x862-lexical-reference-p aform))
         (atriv (x862-trivial-p bform))
         (aconst (and (not atriv) (or (x86-side-effect-free-form-p aform)
                                      (if avar (x862-var-not-set-by-form-p avar bform)))))
         apushed)
    (progn
      (unless aconst
        (if atriv
          (x862-one-targeted-reg-form seg aform areg)
          (setq apushed (x862-push-register seg (x862-one-untargeted-reg-form seg aform (x862-acc-reg-for areg))))))
      (x862-one-targeted-reg-form seg bform breg)
      (if aconst
        (x862-one-targeted-reg-form seg aform areg)
        (if apushed
          (x862-elide-pushes seg apushed (x862-pop-register seg areg)))))
    (values areg breg)))


(defun x862-two-untargeted-reg-forms (seg aform areg bform breg)
  (with-x86-local-vinsn-macros (seg)
    (let* ((avar (x862-lexical-reference-p aform))
           (adest areg)
           (bdest breg)
           (atriv (x862-trivial-p bform))
           (aconst (and (not atriv) (or (x86-side-effect-free-form-p aform)
                                        (if avar (x862-var-not-set-by-form-p avar bform)))))
           (apushed (not (or atriv aconst))))
      (progn
        (unless aconst
          (if atriv
            (setq adest (x862-one-untargeted-reg-form seg aform areg))
            (setq apushed (x862-push-register seg (x862-one-untargeted-reg-form seg aform (x862-acc-reg-for areg))))))
        (setq bdest (x862-one-untargeted-reg-form seg bform breg))
        (if aconst
          (setq adest (x862-one-untargeted-reg-form seg aform areg))
          (if apushed
            (x862-elide-pushes seg apushed (x862-pop-register seg areg)))))
      (values adest bdest))))


(defun x862-three-targeted-reg-forms (seg aform areg bform breg cform creg)
  (unless (typep areg 'lreg)
    (warn "~s is not an lreg (1/3)" areg))
  (unless (typep breg 'lreg)
    (warn "~s is not an lreg (2/3)" breg))
  (unless (typep creg 'lreg)
    (warn "~s is not an lreg (3/3)" creg))
  (let* ((atriv (or (null aform) 
                    (and (x862-trivial-p bform)
                         (x862-trivial-p cform))))
         (btriv (or (null bform)
                    (x862-trivial-p cform)))
         (aconst (and (not atriv) 
                      (or (x86-side-effect-free-form-p aform)
                          (let ((avar (x862-lexical-reference-p aform)))
                            (and avar 
                                 (x862-var-not-set-by-form-p avar bform)
                                 (x862-var-not-set-by-form-p avar cform))))))
         (bconst (and (not btriv)
                      (or
                       (x86-side-effect-free-form-p bform)
                       (let ((bvar (x862-lexical-reference-p bform)))
                         (and bvar (x862-var-not-set-by-form-p bvar cform))))))
         (apushed nil)
         (bpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (x862-one-targeted-reg-form seg aform areg)
        (setq apushed (x862-push-register seg (x862-one-untargeted-reg-form seg aform (x862-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (x862-one-targeted-reg-form seg bform breg)
        (setq bpushed (x862-push-register seg (x862-one-untargeted-reg-form seg bform (x862-acc-reg-for breg))))))
    (x862-one-targeted-reg-form seg cform creg)
    (unless btriv 
      (if bconst
        (x862-one-targeted-reg-form seg bform breg)
        (x862-elide-pushes seg bpushed (x862-pop-register seg breg))))
    (unless atriv
      (if aconst
        (x862-one-targeted-reg-form seg aform areg)
        (x862-elide-pushes seg apushed (x862-pop-register seg areg))))
    (values areg breg creg)))

(defun x862-four-targeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (unless (typep areg 'lreg)
    (warn "~s is not an lreg (1/4)" areg))
  (unless (typep breg 'lreg)
    (warn "~s is not an lreg (2/4)" breg))
  (unless (typep creg 'lreg)
    (warn "~s is not an lreg (3/4)" creg))
  (unless (typep dreg 'lreg)
    (warn "~s is not an lreg (4/4)" dreg))
  (let* ((atriv (or (null aform) 
                    (and (x862-trivial-p bform)
                         (x862-trivial-p cform)
                         (x862-trivial-p dform))))
         (btriv (or (null bform)
                    (and (x862-trivial-p cform)
                         (x862-trivial-p dform))))
         (ctriv (or (null cform)
                    (x862-trivial-p dform)))
         (aconst (and (not atriv) 
                      (or (x86-side-effect-free-form-p aform)
                          (let ((avar (x862-lexical-reference-p aform)))
                            (and avar 
                                 (x862-var-not-set-by-form-p avar bform)
                                 (x862-var-not-set-by-form-p avar cform)
                                 (x862-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or
                       (x86-side-effect-free-form-p bform)
                       (let ((bvar (x862-lexical-reference-p bform)))
                         (and bvar
                              (x862-var-not-set-by-form-p bvar cform)
                              (x862-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or
                       (x86-side-effect-free-form-p cform)
                       (let ((cvar (x862-lexical-reference-p cform)))
                         (and cvar (x862-var-not-set-by-form-p cvar dform))))))
         (apushed nil)
         (bpushed nil)
         (cpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (x862-one-targeted-reg-form seg aform areg)
        (setq apushed (x862-push-register seg (x862-one-untargeted-reg-form seg aform (x862-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (x862-one-targeted-reg-form seg bform breg)
        (setq bpushed (x862-push-register seg (x862-one-untargeted-reg-form seg bform (x862-acc-reg-for breg))))))
    (if (and cform (not cconst))
      (if ctriv
        (x862-one-targeted-reg-form seg cform creg)
        (setq cpushed (x862-push-register seg (x862-one-untargeted-reg-form seg cform (x862-acc-reg-for creg))))))
    (x862-one-targeted-reg-form seg dform dreg)
    (unless ctriv
      (if cconst
        (x862-one-targeted-reg-form seg cform creg)
        (x862-elide-pushes seg cpushed (x862-pop-register seg creg))))
    (unless btriv 
      (if bconst
        (x862-one-targeted-reg-form seg bform breg)
        (x862-elide-pushes seg bpushed (x862-pop-register seg breg))))
    (unless atriv
      (if aconst
        (x862-one-targeted-reg-form seg aform areg)
        (x862-elide-pushes seg apushed (x862-pop-register seg areg))))
    (values areg breg creg)))

(defun x862-three-untargeted-reg-forms (seg aform areg bform breg cform creg)
  (with-x86-local-vinsn-macros (seg)
    (let* ((atriv (or (null aform) 
                      (and (x862-trivial-p bform)
                           (x862-trivial-p cform))))
           (btriv (or (null bform)
                      (x862-trivial-p cform)))
           (aconst (and (not atriv) 
                        (or (x86-side-effect-free-form-p aform)
                            (let ((avar (x862-lexical-reference-p aform)))
                              (and avar 
                                   (x862-var-not-set-by-form-p avar bform)
                                   (x862-var-not-set-by-form-p avar cform))))))
           (bconst (and (not btriv)
                        (or
                         (x86-side-effect-free-form-p bform)
                         (let ((bvar (x862-lexical-reference-p bform)))
                           (and bvar (x862-var-not-set-by-form-p bvar cform))))))
           (adest areg)
           (bdest breg)
           (cdest creg)
           (apushed nil)
           (bpushed nil))
      (if (and aform (not aconst))
        (if atriv
          (setq adest (x862-one-untargeted-reg-form seg aform ($ areg)))
          (setq apushed (x862-push-register seg (x862-one-untargeted-reg-form seg aform (x862-acc-reg-for areg))))))
      (if (and bform (not bconst))
        (if btriv
          (setq bdest (x862-one-untargeted-reg-form seg bform ($ breg)))
          (setq bpushed (x862-push-register seg (x862-one-untargeted-reg-form seg bform (x862-acc-reg-for breg))))))
      (setq cdest (x862-one-untargeted-reg-form seg cform creg))
      (unless btriv 
        (if bconst
          (setq bdest (x862-one-untargeted-reg-form seg bform breg))
          (x862-elide-pushes seg bpushed (x862-pop-register seg breg))))
      (unless atriv
        (if aconst
          (setq adest (x862-one-untargeted-reg-form seg aform areg))
          (x862-elide-pushes seg apushed (x862-pop-register seg areg))))
      (values adest bdest cdest))))

(defun x862-four-untargeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (let* ((atriv (or (null aform) 
                    (and (x862-trivial-p bform)
                         (x862-trivial-p cform)
                         (x862-trivial-p dform))))
         (btriv (or (null bform)
                    (and (x862-trivial-p cform)
                         (x862-trivial-p dform))))
         (ctriv (or (null cform)
                    (x862-trivial-p dform)))
         (aconst (and (not atriv) 
                      (or (x86-side-effect-free-form-p aform)
                          (let ((avar (x862-lexical-reference-p aform)))
                            (and avar 
                                 (x862-var-not-set-by-form-p avar bform)
                                 (x862-var-not-set-by-form-p avar cform)
                                 (x862-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or
                       (x86-side-effect-free-form-p bform)
                       (let ((bvar (x862-lexical-reference-p bform)))
                         (and bvar
                              (x862-var-not-set-by-form-p bvar cform)
                              (x862-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or
                       (x86-side-effect-free-form-p cform)
                       (let ((cvar (x862-lexical-reference-p cform)))
                         (and cvar
                              (x862-var-not-set-by-form-p cvar dform))))))
         (adest areg)
         (bdest breg)
         (cdest creg)
         (ddest dreg)
         (apushed nil)
         (bpushed nil)
         (cpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (setq adest (x862-one-targeted-reg-form seg aform areg))
        (setq apushed (x862-push-register seg (x862-one-untargeted-reg-form seg aform (x862-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (setq bdest (x862-one-untargeted-reg-form seg bform breg))
        (setq bpushed (x862-push-register seg (x862-one-untargeted-reg-form seg bform (x862-acc-reg-for breg))))))
    (if (and cform (not cconst))
      (if ctriv
        (setq cdest (x862-one-untargeted-reg-form seg cform creg))
        (setq cpushed (x862-push-register seg (x862-one-untargeted-reg-form seg cform (x862-acc-reg-for creg))))))
    (setq ddest (x862-one-untargeted-reg-form seg dform dreg))
    (unless ctriv 
      (if cconst
        (setq cdest (x862-one-untargeted-reg-form seg cform creg))
        (x862-elide-pushes seg cpushed (x862-pop-register seg creg))))
    (unless btriv 
      (if bconst
        (setq bdest (x862-one-untargeted-reg-form seg bform breg))
        (x862-elide-pushes seg bpushed (x862-pop-register seg breg))))
    (unless atriv
      (if aconst
        (setq adest (x862-one-untargeted-reg-form seg aform areg))
        (x862-elide-pushes seg apushed (x862-pop-register seg areg))))
    (values adest bdest cdest ddest)))

(defun x862-lri (seg reg value)
  (with-x86-local-vinsn-macros (seg)
    (! lri reg value)))

;;; unsigned variant
(defun x862-lriu (seg reg value)
  (with-x86-local-vinsn-macros (seg)
    (! lriu reg value)))

(defun x862-multiple-value-body (seg form)
  (let* ((lab (backend-get-next-label))
         (*x862-vstack* *x862-vstack*)
         (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
         (old-stack (x862-encode-stack)))
    (with-x86-local-vinsn-macros (seg)
      (x862-open-undo $undomvexpect)
      (x862-undo-body seg nil (logior $backend-mvpass-mask lab) form old-stack)
      (@ lab))))

(defun x862-afunc-lfun-ref (afunc)
  (or
   (afunc-lfun afunc)
   (progn (pushnew afunc (afunc-fwd-refs *x862-cur-afunc*) :test #'eq)
          afunc)))

(defun x862-augment-arglist (afunc arglist &optional (maxregs *x862-target-num-arg-regs*))
  (let ((inherited-args (afunc-inherited-vars afunc)))
    (when inherited-args
      (let* ((current-afunc *x862-cur-afunc*)
             (stkargs (car arglist))
             (regargs (cadr arglist))
             (inhforms nil)
             (numregs (length regargs))
             (own-inhvars (afunc-inherited-vars current-afunc)))
        (dolist (var inherited-args)
          (let* ((root-var (nx-root-var var))
                 (other-guy 
                  (dolist (v own-inhvars #|(error "other guy not found")|# root-var)
                    (when (eq root-var (nx-root-var v)) (return v)))))
            (push (make-acode (%nx1-operator inherited-arg) other-guy) inhforms)))
        (dolist (form inhforms)
          (if (%i< numregs maxregs)
            (progn
              (setq regargs (nconc regargs (list form)))
              (setq numregs (%i+ numregs 1)))
            (push form stkargs)))
        (%rplaca (%cdr arglist) regargs) ; might have started out NIL.
        (%rplaca arglist stkargs)))) 
  arglist)

(defun x862-acode-operator-supports-u8 (form)
  (setq form (acode-unwrapped-form-value form))
  (when (acode-p form)
    (let* ((operator (acode-operator form)))
      (if (member operator *x862-operator-supports-u8-target*)
        (values operator (acode-operand 1 form))))))

(defun x862-acode-operator-supports-push (form)
  (let ((value (acode-unwrapped-form-value form)))
    (when (acode-p value)
      (if (or (eq value *nx-t*)
              (eq value *nx-nil*)
              (let* ((operator (acode-operator value)))
                (member operator *x862-operator-supports-push*)))
        value))))

(defun x862-compare-u8 (seg vreg xfer form u8constant cr-bit true-p u8-operator)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (with-imm-target () (u8 :u8)
      (if (and (eql u8-operator (%nx1-operator lisptag))
               (eql 0 u8constant))
        (let* ((formreg (x862-one-untargeted-reg-form seg form *x862-arg-z*)))
          
          (! set-flags-from-lisptag formreg))
        (progn
          (x862-use-operator u8-operator seg u8 nil form)
          (if (zerop u8constant)
            (! compare-u8-reg-to-zero u8)
            (! compare-u8-constant u8 u8constant))))
      ;; Flags set.  Branch or return a boolean value ?
      (setq cr-bit (x862-cr-bit-for-unsigned-comparison cr-bit))
      (regspec-crf-gpr-case 
       (vreg dest)
       (^ cr-bit true-p)
       (progn
         (ensuring-node-target (target dest)
           (if (not true-p)
             (setq cr-bit (logxor 1 cr-bit)))
           (! cr-bit->boolean target cr-bit))
         (^))))))

;;; There are other cases involving constants that are worth exploiting.
(defun x862-compare (seg vreg xfer i j cr-bit true-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((iu8 (let* ((i-fixnum (acode-fixnum-form-p i)))
                  (if (typep i-fixnum '(unsigned-byte 8))
                    i-fixnum)))
           (ju8 (let* ((j-fixnum (acode-fixnum-form-p j)))
                  (if (typep j-fixnum '(unsigned-byte 8))
                    j-fixnum)))
           (u8 (or iu8 ju8))
           (other-u8 (if iu8 j (if ju8 i)))
           (js32 (acode-s32-constant-p j))
           (is32 (acode-s32-constant-p i))
           (boolean (backend-crf-p vreg)))
      (multiple-value-bind (u8-operator u8-operand) (if other-u8 (x862-acode-operator-supports-u8 other-u8))
        (if u8-operator
          (x862-compare-u8 seg vreg xfer u8-operand u8 (if (and iu8 (not (eq cr-bit x86::x86-e-bits))) (logxor 1 cr-bit) cr-bit) true-p u8-operator)
          (if (and boolean (or js32 is32))
            (let* ((reg (x862-one-untargeted-reg-form seg (if js32 i j) *x862-arg-z*))
                   (constant (or js32 is32)))
              (if (zerop constant)
                (! compare-reg-to-zero reg)
                (! compare-s32-constant reg (or js32 is32)))
              (unless (or js32 (eq cr-bit x86::x86-e-bits))
                (setq cr-bit (x862-reverse-cr-bit cr-bit)))
              (^ cr-bit true-p))
            (if (and ;(eq cr-bit x86::x86-e-bits) 
                     (or js32 is32))
              (progn
                (unless (or js32 (eq cr-bit x86::x86-e-bits))
                  (setq cr-bit (x862-reverse-cr-bit cr-bit)))
              (x862-test-reg-%izerop
               seg 
               vreg 
               xfer 
               (x862-one-untargeted-reg-form 
                seg 
                (if js32 i j) 
                *x862-arg-z*) 
               cr-bit 
               true-p 
               (or js32 is32)))
              (multiple-value-bind (ireg jreg) (x862-two-untargeted-reg-forms seg i *x862-arg-y* j *x862-arg-z*)
                (x862-compare-registers seg vreg xfer ireg jreg cr-bit true-p)))))))))

(defun x862-natural-compare (seg vreg xfer i j cr-bit true-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((jconstant (acode-fixnum-form-p j))
           (ju31 (typep jconstant '(unsigned-byte 31)))
           (iconstant (acode-fixnum-form-p i))
           (iu31 (typep iconstant '(unsigned-byte 31)))
           (boolean (backend-crf-p vreg)))
      (if (and boolean (or ju31 iu31))
        (with-imm-target
            () (reg :natural)
            (x862-one-targeted-reg-form seg (if ju31 i j) reg)
            (! compare-u31-constant reg (if ju31 jconstant iconstant))
            (unless (or ju31 (eq cr-bit x86::x86-e-bits)) 
                (setq cr-bit (x862-reverse-cr-bit cr-bit)))
            (^ cr-bit true-p))
        (target-arch-case
         (:x8664
          (with-imm-target () (ireg :natural)
            (with-imm-target (ireg) (jreg :natural)
              (x862-two-targeted-reg-forms seg i ireg j jreg)
              (x862-compare-natural-registers seg vreg xfer ireg jreg cr-bit true-p))))
         (:x8632
          (with-imm-target () (jreg :natural) 
            (x862-one-targeted-reg-form seg i jreg)
            (x862-push-register seg jreg)
            (x862-one-targeted-reg-form seg j jreg)
            (! temp-pop-temp1-as-unboxed-word)
            (x862-close-undo)
            (x862-compare-natural-registers seg vreg xfer ($ x8632::temp1) jreg cr-bit true-p))))))))



                 
(defun x862-compare-natural-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (progn
        (setq cr-bit (x862-cr-bit-for-unsigned-comparison cr-bit))
        (! compare ireg jreg)
        (target-arch-case
         (:x8664)
         (:x8632 (! mark-temp1-as-node-preserving-flags)))
        (regspec-crf-gpr-case 
         (vreg dest)
         (^ cr-bit true-p)
         (progn
           (ensuring-node-target (target dest)
             (if (not true-p)
               (setq cr-bit (logxor 1 cr-bit)))
             (! cr-bit->boolean target cr-bit))
           (^))))
      (^))))


(defun x862-compare-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (progn
        (! compare ireg jreg)
        (regspec-crf-gpr-case 
         (vreg dest)
         (^ cr-bit true-p)
         (progn
           (ensuring-node-target (target dest)
             (if (not true-p)
               (setq cr-bit (logxor 1 cr-bit)))
             (! cr-bit->boolean target cr-bit))
           (^))))
      (^))))

(defun x862-compare-register-to-constant (seg vreg xfer ireg cr-bit true-p constant)
  (cond ((eq constant *nx-nil*)
         (x862-compare-register-to-nil seg vreg xfer ireg cr-bit true-p))
        (t
         (with-x86-local-vinsn-macros (seg vreg xfer)
           (when vreg
             (if (eq constant *nx-t*)
               (! compare-to-t ireg)
               (let* ((imm (x862-immediate-operand constant))
                      (reg (x862-register-constant-p imm))) 
                 (if reg
                   (! compare-registers reg ireg)
                   (! compare-constant-to-register (x86-immediate-label imm) ireg))))
             (regspec-crf-gpr-case 
              (vreg dest)
              (^ cr-bit true-p)
              (progn
                (ensuring-node-target (target dest)
                  (if (not true-p)
                    (setq cr-bit (logxor 1 cr-bit)))
                  (! cr-bit->boolean target cr-bit))
                (^))))))))
         
(defun x862-compare-register-to-nil (seg vreg xfer ireg cr-bit true-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (! compare-to-nil ireg)
      (regspec-crf-gpr-case 
       (vreg dest)
       (^ cr-bit true-p)
       (progn
       (ensuring-node-target (target dest)
         (if (not true-p)
           (setq cr-bit (logxor 1 cr-bit)))
         (! cr-bit->boolean target cr-bit))
       (^))))))

(defun x862-compare-ea-to-nil (seg vreg xfer ea cr-bit true-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (if (addrspec-vcell-p ea)
        (with-node-target () temp
          (x862-stack-to-register seg ea temp)
          (! compare-value-cell-to-nil temp))
        (! compare-vframe-offset-to-nil (memspec-frame-address-offset ea) *x862-vstack*))
      (regspec-crf-gpr-case 
       (vreg dest)
       (^ cr-bit true-p)
       (progn
       (ensuring-node-target (target dest)
         (if (not true-p)
           (setq cr-bit (logxor 1 cr-bit)))
         (! cr-bit->boolean target cr-bit))
       (^))))))

(defun x862-cr-bit-for-unsigned-comparison (cr-bit)
  (ecase cr-bit
    (#.x86::x86-e-bits #.x86::x86-e-bits)
    (#.x86::x86-ne-bits #.x86::x86-ne-bits)
    (#.x86::x86-l-bits #.x86::x86-b-bits)
    (#.x86::x86-le-bits #.x86::x86-be-bits)
    (#.x86::x86-ge-bits #.x86::x86-ae-bits)
    (#.x86::x86-g-bits #.x86::x86-a-bits)))


(defun x862-compare-double-float-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (progn
        (setq cr-bit (x862-cr-bit-for-unsigned-comparison cr-bit))
        (regspec-crf-gpr-case 
         (vreg dest)
         (progn
           (! double-float-compare ireg jreg)
           (^ cr-bit true-p))
         (progn
           (! double-float-compare ireg jreg)
           (ensuring-node-target (target dest)
             (if (not true-p)
               (setq cr-bit (logxor 1 cr-bit)))
             (! cr-bit->boolean target cr-bit))
           (^))))
      (^))))

(defun x862-compare-single-float-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (progn
        (setq cr-bit (x862-cr-bit-for-unsigned-comparison cr-bit))
        (regspec-crf-gpr-case 
         (vreg dest)
         (progn
           (! single-float-compare ireg jreg)
           (^ cr-bit true-p))
         (progn
           (! single-float-compare ireg jreg)
           (ensuring-node-target (target dest)
             (if (not true-p)
               (setq cr-bit (logxor 1 cr-bit)))
             (! cr-bit->boolean target cr-bit))
         (^))))
      (^))))


(defun x862-immediate-form-p (form)
  (if (and (consp form)
           (or (eq (%car form) (%nx1-operator immediate))
               (eq (%car form) (%nx1-operator simple-function))))
    t))

(defun x862-test-%izerop (seg vreg xfer form cr-bit true-p)
  (x862-test-reg-%izerop seg vreg xfer (x862-one-untargeted-reg-form seg form *x862-arg-z*) cr-bit true-p 0))

(defun x862-test-reg-%izerop (seg vreg xfer reg cr-bit true-p  zero)
  (declare (fixnum reg zero))
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if (zerop zero)
      (! compare-reg-to-zero reg)
      (! compare-s32-constant reg zero))
    (regspec-crf-gpr-case 
     (vreg dest)
     (^ cr-bit true-p)
     (progn
       (ensuring-node-target (target dest)
         (if (not true-p)
           (setq cr-bit (logxor 1 cr-bit)))
         (! cr-bit->boolean target cr-bit))
       (^)))))

(defun x862-lexical-reference-ea (form &optional (no-closed-p t))
  (when (acode-p (setq form (acode-unwrapped-form-value form)))
    (if (eq (acode-operator form) (%nx1-operator lexical-reference))
      (let* ((addr (var-ea (%cadr form))))
        (if (typep addr 'lreg)
          addr
          (unless (and no-closed-p (addrspec-vcell-p addr ))
            addr))))))


(defun x862-vpush-register (seg src &optional why info attr)
  (with-x86-local-vinsn-macros (seg)
    (prog1
      (! vpush-register src)
      (setq *x862-tos-reg* src)
      (x862-new-vstack-lcell (or why :node) *x862-target-lcell-size* (or attr 0) info)
      (x862-adjust-vstack *x862-target-node-size*))))


;;; Need to track stack usage when pushing label for mv-call.
(defun x862-vpush-label (seg label)
  (with-x86-local-vinsn-macros (seg)
    (prog1
      (! vpush-label label)
      (x862-new-vstack-lcell :label *x862-target-lcell-size* 0 nil)
      (x862-adjust-vstack *x862-target-node-size*))))

(defun x862-temp-push-node (seg reg)
  (with-x86-local-vinsn-macros (seg)
    (! temp-push-node reg)
    (x862-open-undo $undostkblk)))

(defun x862-temp-pop-node (seg reg)
  (with-x86-local-vinsn-macros (seg)
    (! temp-pop-node reg)
    (x862-close-undo)))

(defun x862-vpush-register-arg (seg src)
  (x862-vpush-register seg src :outgoing-argument))


(defun x862-vpop-register (seg dest)
  (with-x86-local-vinsn-macros (seg)
    (prog1
      (! vpop-register dest)
      (setq *x862-top-vstack-lcell* (lcell-parent *x862-top-vstack-lcell*))
      (x862-adjust-vstack (- *x862-target-node-size*)))))

(defun x862-macptr->heap (seg dest src)
  (with-x86-local-vinsn-macros (seg)
    (! setup-macptr-allocation src)
    (! %allocate-uvector dest)
    (! %set-new-macptr-value dest)))

(defun x862-copy-register (seg dest src)
  (with-x86-local-vinsn-macros (seg)
    (when dest
      (let* ((dest-gpr (backend-ea-physical-reg dest hard-reg-class-gpr))
             (src-gpr (if src (backend-ea-physical-reg src hard-reg-class-gpr)))
             (dest-fpr (backend-ea-physical-reg dest hard-reg-class-fpr))
             (src-fpr (if src (backend-ea-physical-reg src hard-reg-class-fpr)))
             (src-mode (if src (get-regspec-mode src)))
             (dest-mode (get-regspec-mode dest))
             (dest-crf (backend-ea-physical-reg dest hard-reg-class-crf)))
        (if (null src)
          (if dest-gpr
            (! load-nil dest-gpr)
            (if dest-crf
              (! set-eq-bit)))
          (if (and dest-crf src-gpr)
            ;; "Copying" a GPR to a CR field means comparing it to rnil
            (! compare-to-nil src)
            (if (and dest-gpr src-gpr)
              (if (eq src-mode dest-mode)
                (unless (eq src-gpr dest-gpr)
                  (! copy-gpr dest src))
                ;; This is the "GPR <- GPR" case.  There are
                ;; word-size dependencies, but there's also
                ;; lots of redundancy here.
                (target-arch-case
		 (:x8632
		  (ecase dest-mode
		    (#.hard-reg-class-gpr-mode-node ; boxed result.
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       (#.hard-reg-class-gpr-mode-u32
                        (x862-box-u32 seg dest src))
                       (#.hard-reg-class-gpr-mode-s32
                        (x862-box-s32 seg dest src))
                       (#.hard-reg-class-gpr-mode-u16
                        (! box-fixnum dest src))
                       (#.hard-reg-class-gpr-mode-s16
                        (! box-fixnum dest src))
                       (#.hard-reg-class-gpr-mode-u8
                        (! box-fixnum dest src))
                       (#.hard-reg-class-gpr-mode-s8
                        (! box-fixnum dest src))
                       (#.hard-reg-class-gpr-mode-address
                        (x862-macptr->heap seg dest src))))
		    ((#.hard-reg-class-gpr-mode-u32
                      #.hard-reg-class-gpr-mode-address)
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (let* ((src-type (get-node-regspec-type-modes src)))
                          (declare (fixnum src-type))
                          (case dest-mode
                            (#.hard-reg-class-gpr-mode-u32
                             (! unbox-u32 dest src))
                            (#.hard-reg-class-gpr-mode-address
                             (unless (or (logbitp #.hard-reg-class-gpr-mode-address src-type)
                                         *x862-reckless*)
                               (! trap-unless-macptr src))
                             (! deref-macptr dest src)))))
                       ((#.hard-reg-class-gpr-mode-u32
                         #.hard-reg-class-gpr-mode-s32
                         #.hard-reg-class-gpr-mode-address)
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       (#.hard-reg-class-gpr-mode-u16
                        (! u16->u32 dest src))                 
                       (#.hard-reg-class-gpr-mode-s16
                        (! s16->s32 dest src))
                       (#.hard-reg-class-gpr-mode-u8
                        (! u8->u32 dest src))
                       (#.hard-reg-class-gpr-mode-s8
                        (! s8->s32 dest src))))
                    (#.hard-reg-class-gpr-mode-s32
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (! unbox-s32 dest src))
                       ((#.hard-reg-class-gpr-mode-u32
                         #.hard-reg-class-gpr-mode-s32
                         #.hard-reg-class-gpr-mode-address)
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       (#.hard-reg-class-gpr-mode-u16
                        (! u16->u32 dest src))                 
                       (#.hard-reg-class-gpr-mode-s16
                        (! s16->s32 dest src))
                       (#.hard-reg-class-gpr-mode-u8
                        (! u8->u32 dest src))
                       (#.hard-reg-class-gpr-mode-s8
                        (! s8->s32 dest src))))
		    (#.hard-reg-class-gpr-mode-u16
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (! unbox-u16 dest src))
                       ((#.hard-reg-class-gpr-mode-u8
                         #.hard-reg-class-gpr-mode-s8)
                        (! u8->u32 dest src))
                       (t
                        (unless (eql dest-gpr src-gpr)
                          (! copy-gpr dest src)))))
                    (#.hard-reg-class-gpr-mode-s16
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (! unbox-s16 dest src))
                       (#.hard-reg-class-gpr-mode-s8
                        (! s8->s32 dest src))
                       (#.hard-reg-class-gpr-mode-u8
                        (! u8->u32 dest src))
                       (t
                        (unless (eql dest-gpr src-gpr)
                          (! copy-gpr dest src)))))
                    (#.hard-reg-class-gpr-mode-u8
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (if *x862-reckless*
                          (! %unbox-u8 dest src)
                          (! unbox-u8 dest src)))
                       (t
                        (unless (eql dest-gpr src-gpr)
                          (! copy-gpr dest src)))))
                    (#.hard-reg-class-gpr-mode-s8
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (! unbox-s8 dest src))
                       (t
                        (unless (eql dest-gpr src-gpr)
                          (! copy-gpr dest src)))))))
                 (:x8664
                  (ecase dest-mode
                    (#.hard-reg-class-gpr-mode-node ; boxed result.
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       (#.hard-reg-class-gpr-mode-u64
                        (x862-box-u64 seg dest src))
                       (#.hard-reg-class-gpr-mode-s64
                        (x862-box-s64 seg dest src))
                       (#.hard-reg-class-gpr-mode-u32
                        (x862-box-u32 seg dest src))
                       (#.hard-reg-class-gpr-mode-s32
                        (x862-box-s32 seg dest src))
                       (#.hard-reg-class-gpr-mode-u16
                        (! box-fixnum dest src))
                       (#.hard-reg-class-gpr-mode-s16
                        (! box-fixnum dest src))
                       (#.hard-reg-class-gpr-mode-u8
                        (! box-fixnum dest src))
                       (#.hard-reg-class-gpr-mode-s8
                        (! box-fixnum dest src))
                       (#.hard-reg-class-gpr-mode-address
                        (x862-macptr->heap seg dest src))))
                    ((#.hard-reg-class-gpr-mode-u64
                      #.hard-reg-class-gpr-mode-address)
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (let* ((src-type (get-node-regspec-type-modes src)))
                          (declare (fixnum src-type))
                          (case dest-mode
                            (#.hard-reg-class-gpr-mode-u64
                             (! unbox-u64 dest src))
                            (#.hard-reg-class-gpr-mode-address
                             (unless (or (logbitp #.hard-reg-class-gpr-mode-address src-type)
                                         *x862-reckless*)
                               (! trap-unless-macptr src))
                             (! deref-macptr dest src)))))
                       ((#.hard-reg-class-gpr-mode-u64
                         #.hard-reg-class-gpr-mode-s64
                         #.hard-reg-class-gpr-mode-address)
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       ((#.hard-reg-class-gpr-mode-u16
                         #.hard-reg-class-gpr-mode-s16)
                        (! u16->u32 dest src))
                       ((#.hard-reg-class-gpr-mode-u8
                         #.hard-reg-class-gpr-mode-s8)
                        (! u8->u32 dest src))))
                    (#.hard-reg-class-gpr-mode-s64
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (! unbox-s64 dest src))
                       ((#.hard-reg-class-gpr-mode-u64
                         #.hard-reg-class-gpr-mode-s64
                         #.hard-reg-class-gpr-mode-address)
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       ((#.hard-reg-class-gpr-mode-u16
                         #.hard-reg-class-gpr-mode-s16)
                        (! s16->s32 dest src))
                       ((#.hard-reg-class-gpr-mode-u8
                         #.hard-reg-class-gpr-mode-s8)
                        (! s8->s32 dest src))))
                    (#.hard-reg-class-gpr-mode-s32
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (! unbox-s32 dest src))
                       ((#.hard-reg-class-gpr-mode-u32
                         #.hard-reg-class-gpr-mode-s32
                         #.hard-reg-class-gpr-mode-address)
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       (#.hard-reg-class-gpr-mode-u16
                        (! u16->u32 dest src))                 
                       (#.hard-reg-class-gpr-mode-s16
                        (! s16->s32 dest src))
                       (#.hard-reg-class-gpr-mode-u8
                        (! u8->u32 dest src))
                       (#.hard-reg-class-gpr-mode-s8
                        (! s8->s32 dest src))))
                    (#.hard-reg-class-gpr-mode-u32
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (if *x862-reckless*
                          (! %unbox-u32 dest src)
                          (! unbox-u32 dest src)))
                       ((#.hard-reg-class-gpr-mode-u32
                         #.hard-reg-class-gpr-mode-s32)
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       (#.hard-reg-class-gpr-mode-u16
                        (! u16->u32 dest src))                 
                       (#.hard-reg-class-gpr-mode-s16
                        (! s16->s32 dest src))
                       (#.hard-reg-class-gpr-mode-u8
                        (! u8->u32 dest src))
                       (#.hard-reg-class-gpr-mode-s8
                        (! s8->s32 dest src))))
                    (#.hard-reg-class-gpr-mode-u16
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (if *x862-reckless*
                          (! %unbox-u16 dest src)
                          (! unbox-u16 dest src)))
                       ((#.hard-reg-class-gpr-mode-u8
                         #.hard-reg-class-gpr-mode-s8)
                        (! u8->u32 dest src))
                       (t
                        (unless (eql dest-gpr src-gpr)
                          (! copy-gpr dest src)))))
                    (#.hard-reg-class-gpr-mode-s16
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (! unbox-s16 dest src))
                       (#.hard-reg-class-gpr-mode-s8
                        (! s8->s32 dest src))
                       (#.hard-reg-class-gpr-mode-u8
                        (! u8->u32 dest src))
                       (t
                        (unless (eql dest-gpr src-gpr)
                          (! copy-gpr dest src)))))
                    (#.hard-reg-class-gpr-mode-u8
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (if *x862-reckless*
                          (! %unbox-u8 dest src)
                          (! unbox-u8 dest src)))
                       (t
                        (unless (eql dest-gpr src-gpr)
                          (! copy-gpr dest src)))))
                    (#.hard-reg-class-gpr-mode-s8
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (! unbox-s8 dest src))
                       (t
                        (unless (eql dest-gpr src-gpr)
                          (! copy-gpr dest src)))))))))
              (if src-gpr
                (if dest-fpr
                  (progn
                    (case src-mode
                      (#.hard-reg-class-gpr-mode-node
                       (case dest-mode
                         (#.hard-reg-class-fpr-mode-double
                          (unless (or (logbitp hard-reg-class-fpr-type-double 
                                           (get-node-regspec-type-modes src))
                                      *x862-reckless*)
                            (! trap-unless-double-float src))
                          (! get-double dest src))
                         (#.hard-reg-class-fpr-mode-single
                          (unless *x862-reckless* (! trap-unless-single-float src))
                          (! get-single dest src)))))))
                (if dest-gpr
                  (case dest-mode
                    (#.hard-reg-class-gpr-mode-node
                     (case src-mode
                       (#.hard-reg-class-fpr-mode-double
                        (x862-double->heap seg dest src))
                       (#.hard-reg-class-fpr-mode-single
			(target-arch-case
			 (:x8632
			  (x862-single->heap seg dest src))
			 (:x8664
			  (! single->node dest src)))))))
                  (if (and src-fpr dest-fpr)
                    (unless (eql dest-fpr src-fpr)
                      (if (= src-mode hard-reg-class-fpr-mode-double)
                        (if (= dest-mode hard-reg-class-fpr-mode-double)
                          (! copy-double-float dest src)
                          (! copy-double-to-single dest src))
                        (if (= dest-mode hard-reg-class-fpr-mode-double)
                          (! copy-single-to-double dest src)
                          (! copy-single-float dest src))))))))))))))
  
(defun x862-unreachable-store (&optional vreg)
  ;; I don't think that anything needs to be done here,
  ;; but leave this guy around until we're sure.
  ;; (X862-VPUSH-REGISTER will always vpush something, even
  ;; if code to -load- that "something" never gets generated.
  ;; If I'm right about this, that means that the compile-time
  ;; stack-discipline problem that this is supposed to deal
  ;; with can't happen.)
  (declare (ignore vreg))
  nil)

;;; bind vars to initforms, as per let*, &aux.
(defun x862-seq-bind (seg vars initforms)
  (dolist (var vars)
    (x862-seq-bind-var seg var (pop initforms))))

(defun x862-target-is-imm-subtag (subtag)
  (when subtag
    (target-arch-case
     (:x8632
      (let* ((masked (logand subtag x8632::fulltagmask)))
	(declare (fixnum masked))
	(= masked x8632::fulltag-immheader)))
     (:x8664
      (let* ((masked (logand subtag x8664::fulltagmask)))
        (declare (fixnum masked))
        (or (= masked x8664::fulltag-immheader-0)
            (= masked x8664::fulltag-immheader-1)
            (= masked x8664::fulltag-immheader-2)))))))

(defun x862-target-is-node-subtag (subtag)
  (when subtag
    (target-arch-case
     (:x8632
      (let* ((masked (logand subtag x8632::fulltagmask)))
	(declare (fixnum masked))
	(= masked x8632::fulltag-nodeheader)))
     (:x8664
      (let* ((masked (logand subtag x8664::fulltagmask)))
        (declare (fixnum masked))
        (or (= masked x8664::fulltag-nodeheader-0)
            (= masked x8664::fulltag-nodeheader-1)))))))

(defun x862-dynamic-extent-form (seg curstack val)
  (when (acode-p val)
    (with-x86-local-vinsn-macros (seg)
      (let* ((op (acode-operator val)))
        (cond ((eq op (%nx1-operator list))
               (let* ((*x862-vstack* *x862-vstack*)
                      (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
                 (x862-set-nargs seg (x862-formlist seg (%cadr val) nil))
                 (x862-open-undo $undostkblk curstack)
                 (! stack-cons-list))
               (setq val *x862-arg-z*))
              ((eq op (%nx1-operator list*))
               (let* ((arglist (%cadr val)))                   
                 (let* ((*x862-vstack* *x862-vstack*)
                        (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
                   (x862-formlist seg (car arglist) (cadr arglist)))
                 (when (car arglist)
                   (x862-set-nargs seg (length (%car arglist)))
                   (! stack-cons-list*)
                   (x862-open-undo $undostkblk curstack))
                 (setq val *x862-arg-z*)))
              ((eq op (%nx1-operator multiple-value-list))
               (x862-multiple-value-body seg (%cadr val))
               (x862-open-undo $undostkblk curstack)
               (! stack-cons-list)
               (setq val *x862-arg-z*))
              ((eq op (%nx1-operator cons))
               (let* ((y ($ *x862-arg-y*))
                      (z ($ *x862-arg-z*))
                      (result ($ *x862-arg-z*)))
                 (x862-two-targeted-reg-forms seg (%cadr val) y (%caddr val) z)
                 (x862-open-undo $undostkblk )
                 (! make-tsp-cons result y z) 
                 (setq val result)))
              ((eq op (%nx1-operator %consmacptr%))
               (with-imm-target () (address :address)
                 (x862-one-targeted-reg-form seg val address)
                 (with-node-target () node
                   (! macptr->stack node address)
                   (x862-open-undo $undo-x86-c-frame)
                   (setq val node))))
              ((eq op (%nx1-operator %new-ptr))
               (let* ((clear-form (caddr val))
                      (cval (nx-constant-form-p clear-form)))
                 (if cval
                   (progn 
                     (x862-one-targeted-reg-form seg (%cadr val) ($ *x862-arg-z*))
                     (if (nx-null cval)
                       (! make-stack-block)
                       (! make-stack-block0)))
                   (with-crf-target () crf
                     (let ((stack-block-0-label (backend-get-next-label))
                           (done-label (backend-get-next-label))
                           (rval ($ *x862-arg-z*))
                           (rclear ($ *x862-arg-y*)))
                       (x862-two-targeted-reg-forms seg (%cadr val) rval clear-form rclear)
                       (! compare-to-nil crf rclear)
                       (! cbranch-false (aref *backend-labels* stack-block-0-label) crf x86::x86-e-bits)
                       (! make-stack-block)
                       (-> done-label)
                       (@ stack-block-0-label)
                       (! make-stack-block0)
                       (@ done-label)))))
               (x862-open-undo $undo-x86-c-frame)
               (setq val ($ *x862-arg-z*)))
              ((eq op (%nx1-operator make-list))
               (x862-two-targeted-reg-forms seg (%cadr val) ($ *x862-arg-y*) (%caddr val) ($ *x862-arg-z*))
               (x862-open-undo $undostkblk curstack)
               (! make-stack-list)
               (setq val *x862-arg-z*))       
              ((eq op (%nx1-operator vector))
               (let* ((*x862-vstack* *x862-vstack*)
                      (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
                 (x862-set-nargs seg (x862-formlist seg (%cadr val) nil))
                 (! make-stack-vector))
               (x862-open-undo $undostkblk)
               (setq val *x862-arg-z*))
              ((eq op (%nx1-operator %gvector))
               (let* ((*x862-vstack* *x862-vstack*)
                      (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
                      (arglist (%cadr val)))
                 (x862-set-nargs seg (x862-formlist seg (append (car arglist) (reverse (cadr arglist))) nil))
                 (! make-stack-gvector))
               (x862-open-undo $undostkblk)
               (setq val *x862-arg-z*)) 
              ((eq op (%nx1-operator closed-function)) 
               (setq val (x862-make-closure seg (cadr val) t))) ; can't error
              ((eq op (%nx1-operator %make-uvector))
               (destructuring-bind (element-count subtag &optional (init 0 init-p)) (%cdr val)
                 (let* ((fix-subtag (acode-fixnum-form-p subtag))
                        (is-node (x862-target-is-node-subtag fix-subtag))
                        (is-imm  (x862-target-is-imm-subtag fix-subtag)))
                   (when (or is-node is-imm)
                     (if init-p
                       (progn
                         (x862-three-targeted-reg-forms seg element-count
							(target-arch-case
							 (:x8632
							  ($ x8632::temp1))
							 (:x8664
							  ($ x8664::arg_x)))
							subtag ($ *x862-arg-y*)
							init ($ *x862-arg-z*))
                         (! stack-misc-alloc-init))
                       (progn
                         (x862-two-targeted-reg-forms seg element-count ($ *x862-arg-y*)  subtag ($ *x862-arg-z*))
                         (! stack-misc-alloc)))
                     (if is-node
                       (x862-open-undo $undostkblk)
                       (x862-open-undo $undo-x86-c-frame))
                     (setq val ($ *x862-arg-z*))))))))))
  val)

(defun x862-addrspec-to-reg (seg addrspec reg)
  (if (memory-spec-p addrspec)
    (x862-stack-to-register seg addrspec reg)
    (x862-copy-register seg reg addrspec)))
  
(defun x862-seq-bind-var (seg var val)
  (with-x86-local-vinsn-macros (seg)
    (let* ((sym (var-name var))
           (bits (nx-var-bits var))
           (closed-p (and (%ilogbitp $vbitclosed bits)
                          (%ilogbitp $vbitsetq bits)))
           (curstack (x862-encode-stack))
           (make-vcell (and closed-p (eq bits (var-bits var))))
           (closed-downward (and closed-p (%ilogbitp $vbitcloseddownward bits))))
      (unless (fixnump val)
        (setq val (nx-untyped-form val))
        (when (and (%ilogbitp $vbitdynamicextent bits) (acode-p val))
          (setq val (x862-dynamic-extent-form seg curstack val))))
      (if (%ilogbitp $vbitspecial bits)
        (progn
          (x862-dbind seg val sym)
          (x862-set-var-ea seg var (x862-vloc-ea (- *x862-vstack* *x862-target-node-size*))))
        (let ((puntval nil))
          (flet ((x862-puntable-binding-p (var initform)
                   ;; The value returned is acode.
                   (let* ((bits (nx-var-bits var)))
                     (if (%ilogbitp $vbitpuntable bits)
                       (nx-untyped-form initform)))))
            (declare (inline x862-puntable-binding-p))
            (if (and (not (x862-load-ea-p val))
                     (setq puntval (x862-puntable-binding-p var val)))
              (progn
                (nx-set-var-bits var (%ilogior (%ilsl $vbitpunted 1) bits))
                (x862-set-var-ea seg var puntval))
              (progn
                (let* ((vloc *x862-vstack*)
                       (reg (let* ((r (x862-assign-register-var var)))
                              (if r ($ r)))))
                  (if (x862-load-ea-p val)
                    (if reg
                      (x862-addrspec-to-reg seg val reg)
                      (if (memory-spec-p val)
                        (with-node-temps () (temp)
                          (x862-addrspec-to-reg seg val temp)
                          (x862-vpush-register seg temp :node var bits))
                        (x862-vpush-register seg val :node var bits)))
                    (if reg
                      (x862-one-targeted-reg-form seg val reg)
                      (let* ((pushform (x862-acode-operator-supports-push val)))
                        (if pushform
                          (progn
                            (x862-form seg :push nil pushform)
                            (x862-new-vstack-lcell :node *x862-target-lcell-size* bits var)
                            (x862-adjust-vstack *x862-target-node-size*))
                          (x862-vpush-register seg (x862-one-untargeted-reg-form seg val *x862-arg-z*) :node var bits)))))
                  (x862-set-var-ea seg var (or reg (x862-vloc-ea vloc closed-p)))
                  (if reg
                    (x862-note-var-cell var reg)
                    (x862-note-top-cell var))
                  (when make-vcell
                    (with-node-target (*x862-allocptr*) closed
                      (with-node-target (*x862-allocptr* closed) vcell
                        (x862-stack-to-register seg vloc closed)
                        (if closed-downward
                          (progn
                            (! make-tsp-vcell vcell closed)
                            (x862-open-undo $undostkblk))
                          (progn
                            (! setup-vcell-allocation)
                            (! %allocate-uvector vcell)
                            (! %init-vcell vcell closed)))
                        (x862-register-to-stack seg vcell vloc)))))))))))))



;;; Never make a vcell if this is an inherited var.
;;; If the var's inherited, its bits won't be a fixnum (and will
;;; therefore be different from what NX-VAR-BITS returns.)
(defun x862-bind-var (seg var vloc &optional lcell &aux 
                          (bits (nx-var-bits var)) 
                          (closed-p (and (%ilogbitp $vbitclosed bits) (%ilogbitp $vbitsetq bits)))
                          (closed-downward (if closed-p (%ilogbitp $vbitcloseddownward bits)))
                          (make-vcell (and closed-p (eq bits (var-bits var))))
                          (addr (x862-vloc-ea vloc)))
  (with-x86-local-vinsn-macros (seg)
    (if (%ilogbitp $vbitspecial bits)
      (progn
        (x862-dbind seg addr (var-name var))
        (x862-set-var-ea seg var (x862-vloc-ea (- *x862-vstack* *x862-target-node-size*)))
        t)
      (progn
        (when (%ilogbitp $vbitpunted bits)
          (compiler-bug "bind-var: var ~s was punted" var))
        (when make-vcell
          (with-node-target (*x862-allocptr*) closed
            (with-node-target (*x862-allocptr* closed) vcell
              (x862-stack-to-register seg vloc closed)
              (if closed-downward
                (progn
                  (! make-tsp-vcell vcell closed)
                  (x862-open-undo $undostkblk))
                (progn
                  (! setup-vcell-allocation)
                  (! %allocate-uvector vcell)
                  (! %init-vcell vcell closed)))
              (x862-register-to-stack seg vcell vloc))))
        (when lcell
          (setf (lcell-kind lcell) :node
                (lcell-attributes lcell) bits
                (lcell-info lcell) var)
          (x862-note-var-cell var lcell))          
        (x862-set-var-ea seg var (x862-vloc-ea vloc closed-p))        
        closed-downward))))

(defun x862-set-var-ea (seg var ea)
  (setf (var-ea var) ea)
  (when (and *x862-record-symbols* (or (typep ea 'lreg) (typep ea 'fixnum)))
    (let* ((start (x862-emit-note seg :begin-variable-scope)))
      (push (list var (var-name var) start (close-vinsn-note start))
            *x862-recorded-symbols*)))
  ea)

(defun x862-close-var (seg var)
  (let ((bits (nx-var-bits var)))
    (when (and *x862-record-symbols*
               (or (logbitp $vbitspecial bits)
                   (not (logbitp $vbitpunted bits))))
      (let ((endnote (%car (%cdddr (assq var *x862-recorded-symbols*)))))
        (unless endnote (compiler-bug "x862-close-var for ~s" (var-name var)))
        (setf (vinsn-note-class endnote) :end-variable-scope)
        (append-dll-node (vinsn-note-label endnote) seg)))))

(defun x862-load-ea-p (ea)
  (or (typep ea 'fixnum)
      (typep ea 'lreg)
      (typep ea 'lcell)))

(defun x862-dbind (seg value sym)
  (with-x86-local-vinsn-macros (seg)
    (let* ((ea-p (x862-load-ea-p value))
           (nil-p (unless ea-p (eq (setq value (nx-untyped-form value)) *nx-nil*)))
           (self-p (unless ea-p (and (or
                                      (eq (acode-operator value) (%nx1-operator bound-special-ref))
                                      (eq (acode-operator value) (%nx1-operator special-ref)))
                                     (eq (cadr value) sym)))))
      (cond ((eq sym '*interrupt-level*)
             (let* ((fixval (acode-fixnum-form-p value)))
               (cond ((eql fixval 0)
                      (if *x862-open-code-inline*
                        (! bind-interrupt-level-0-inline)
                        (! bind-interrupt-level-0)))
                     ((eql fixval -1)
                      (if *x862-open-code-inline*
                        (! bind-interrupt-level-m1-inline)
                        (! bind-interrupt-level-m1)))
                     (t
                      (if ea-p 
                        (x862-store-ea seg value *x862-arg-z*)
                        (x862-one-targeted-reg-form seg value ($ *x862-arg-z*)))
                      (! bind-interrupt-level))))
             (x862-open-undo $undointerruptlevel))
            (t
             (if (or nil-p self-p)
               (progn
                 (x862-store-immediate seg (x862-symbol-value-cell sym) *x862-arg-z*)
                 (if nil-p
                   (! bind-nil)
                   (if (or *x862-reckless* (eq (acode-operator value) (%nx1-operator special-ref)))
                     (! bind-self)
                     (! bind-self-boundp-check))))
               (progn
                 (if ea-p 
                   (x862-store-ea seg value *x862-arg-z*)
                   (x862-one-targeted-reg-form seg value ($ *x862-arg-z*)))
                 (x862-store-immediate seg (x862-symbol-value-cell sym) ($ *x862-arg-y*))
                 (! bind)))
             (x862-open-undo $undospecial)))
      (x862-new-vstack-lcell :special-value *x862-target-lcell-size* 0 sym)
      (x862-new-vstack-lcell :special *x862-target-lcell-size* (ash 1 $vbitspecial) sym)
      (x862-new-vstack-lcell :special-link *x862-target-lcell-size* 0 sym)
      (x862-adjust-vstack (* 3 *x862-target-node-size*)))))

;;; Store the contents of EA - which denotes either a vframe location
;;; or a hard register - in reg.

(defun x862-store-ea (seg ea reg)
  (if (typep ea 'fixnum)
    (if (memory-spec-p ea)
      (x862-stack-to-register seg ea reg)
      (x862-copy-register seg reg ea))
    (if (typep ea 'lreg)
      (x862-copy-register seg reg ea)
      (if (typep ea 'lcell)
        (x862-lcell-to-register seg ea reg)))))


      

;;; Callers should really be sure that this is what they want to use.
(defun x862-absolute-natural (seg vreg xfer value)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (x862-lri seg vreg value))
    (^)))



(defun x862-store-macptr (seg vreg address-reg)
  (with-x86-local-vinsn-macros (seg vreg)
    (when (x862-for-value-p vreg)
      (if (logbitp vreg *backend-imm-temps*)
        (<- address-reg)
        (x862-macptr->heap seg vreg address-reg)))))

(defun x862-store-signed-longword (seg vreg imm-reg)
  (with-x86-local-vinsn-macros (seg vreg)
    (when (x862-for-value-p vreg)
      (if (logbitp vreg *backend-imm-temps*)
        (<- imm-reg)
        (x862-box-s32 seg vreg imm-reg)))))



;; xxx imm regs
(defun x862-%immediate-set-ptr (seg vreg xfer  ptr offset val)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((intval (acode-absolute-ptr-p val t))
           (offval (acode-fixnum-form-p offset))
           (for-value (x862-for-value-p vreg)))
      (flet ((address-and-node-regs ()
               (if for-value
                 (progn
                   (x862-one-targeted-reg-form seg val ($ *x862-arg-z*))
                   (progn
                       (if intval
                         (x862-lri seg *x862-imm0* intval)
                         (! deref-macptr *x862-imm0* *x862-arg-z*))
                       (values *x862-imm0* *x862-arg-z*)))
                 (values (x862-macptr-arg-to-reg seg val ($ *x862-imm0* :mode :address)) nil))))
        (unless (typep offval '(signed-byte 32))
          (setq offval nil))
        (unless (typep intval '(signed-byte 32))
          (setq intval nil))
        (cond (intval
               (cond (offval
                      (with-imm-target () (ptr-reg :address)
                        (let* ((ptr-reg (x862-one-untargeted-reg-form seg
                                                                      ptr
                                                                      ptr-reg)))
			  (target-word-size-case
			   (32
			    (! mem-set-c-constant-fullword intval ptr-reg offval))
			   (64
			    (! mem-set-c-constant-doubleword intval ptr-reg offval))))))
                     (t
		      (with-additional-imm-reg ()
			(with-imm-target () (ptr-reg :address)
			  (with-imm-target (ptr-reg) (offsetreg :signed-natural)
			    (x862-two-targeted-reg-forms seg ptr ptr-reg offset ($ *x862-arg-z*))
			    (! fixnum->signed-natural offsetreg *x862-arg-z*)
			    (target-word-size-case
			     (32 (! mem-set-constant-fullword intval ptr-reg offsetreg))
			     (64 (! mem-set-constant-doubleword intval ptr-reg offsetreg))))))))
               (if for-value
                 (with-imm-target () (val-reg (target-word-size-case (32 :s32) (64 :s64)))
                   (x862-lri seg val-reg intval)
                   (<- (set-regspec-mode val-reg (gpr-mode-name-value :address))))))
              (offval
               ;; Still simpler than the general case
               (with-imm-target () (ptr-reg :address)
                 (x862-push-register seg
                                     (x862-one-untargeted-reg-form seg ptr ptr-reg)))
               (multiple-value-bind (address node)
                   (address-and-node-regs)
		 (with-additional-imm-reg ()
		   (with-imm-target (address) (ptr-reg :address)
		     (x862-pop-register seg ptr-reg)
		     (target-word-size-case
		      (32 (! mem-set-c-fullword address ptr-reg offval))
		      (64 (! mem-set-c-doubleword address ptr-reg offval)))))
                 (if for-value
                   (<- node))))
              (t
               (with-imm-target () (ptr-reg :address)
		 (with-additional-imm-reg ()
		   (with-imm-target (ptr-reg) (offset-reg :address)
		     (x862-two-targeted-reg-forms seg ptr ptr-reg offset ($ *x862-arg-z*))
		     (! fixnum->signed-natural offset-reg *x862-arg-z*)
		     (! fixnum-add2 ptr-reg offset-reg)
		     (x862-push-register seg ptr-reg))))
               (multiple-value-bind (address node)
                   (address-and-node-regs)
		 (with-additional-imm-reg ()
		   (with-imm-target (address) (ptr-reg :address)
		     (x862-pop-register seg ptr-reg)
		     (target-word-size-case
		      (32 (! mem-set-c-fullword address ptr-reg 0))
		      (64 (! mem-set-c-doubleword address ptr-reg 0)))))
                 (if for-value
                   (<- node))))))
      (^))))
                     
  

      
(defun x862-%immediate-store  (seg vreg xfer bits ptr offset val)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if (eql 0 (%ilogand #xf bits))
      (x862-%immediate-set-ptr seg vreg xfer  ptr offset val)
      (let* ((size (logand #xf bits))
             (signed (not (logbitp 5 bits)))
             (nbits (ash size 3))
             (intval (acode-integer-constant-p val nbits))
             (ncbits (if (eql nbits 64) 32 nbits))
             (signed-intval (or (and intval
                                     (> intval 0)
                                     (logbitp (1- ncbits) intval)
                                     (- intval (ash 1 ncbits)))
                                intval))
             (offval (acode-fixnum-form-p offset))
             (for-value (x862-for-value-p vreg)))
        (declare (fixnum size))
        (flet ((val-to-argz-and-imm0 ()
                 (x862-one-targeted-reg-form seg val ($ *x862-arg-z*))
                 (if (eq size 8)
                   (if signed
                     (! gets64)
                     (! getu64))
		   (if (and (eq size 4)
			    (target-arch-case
			     (:x8632 t)
			     (:x8664 nil)))
		     (if signed
		       (! gets32)
		       (! getu32))
		     (! fixnum->signed-natural *x862-imm0* *x862-arg-z*)))))

          (and offval (%i> (integer-length offval) 31) (setq offval nil))
          (and intval (%i> (integer-length intval) 31) (setq intval nil))
          (and intval
               (case size
                 (2
                  (if (>= intval 32768) (setq intval (- intval 65536))))
                 (1
                  (if (>= intval 128) (setq intval (- intval 256))))))
	  (cond (intval
		 (cond (offval
			(with-imm-target () (ptr-reg :address)
			  (let* ((ptr-reg (x862-one-untargeted-reg-form seg
									ptr
									ptr-reg)))
			    (case size
			      (8 (! mem-set-c-constant-doubleword signed-intval ptr-reg offval))
			      (4 (! mem-set-c-constant-fullword signed-intval ptr-reg offval))
			      (2 (! mem-set-c-constant-halfword signed-intval ptr-reg offval))
			      (1 (! mem-set-c-constant-byte signed-intval ptr-reg offval))))))
		       (t
			(with-imm-target () (ptr-reg :address)
			  (with-additional-imm-reg (*x862-arg-z*)
			    (with-imm-target (ptr-reg) (offsetreg :signed-natural)
			      (x862-two-targeted-reg-forms seg ptr ptr-reg offset ($ *x862-arg-z*))
			      (! fixnum->signed-natural offsetreg *x862-arg-z*)
			      (case size
				(8 (! mem-set-constant-doubleword intval ptr-reg offsetreg))
				(4 (! mem-set-constant-fullword intval ptr-reg offsetreg))
				(2 (! mem-set-constant-halfword intval ptr-reg offsetreg))
				(1 (! mem-set-constant-byte intval ptr-reg offsetreg))))))))
		 (if for-value
		   (ensuring-node-target (target vreg)
		     (x862-lri seg vreg (ash intval *x862-target-fixnum-shift*)))))
		(offval
		 ;; simpler than the general case
		 (with-imm-target () (ptr-reg :address)
		   (x862-push-register seg
				       (x862-one-untargeted-reg-form seg ptr ptr-reg)))
		 (val-to-argz-and-imm0)
		 (target-arch-case
		  (:x8632
		   (with-additional-imm-reg (*x862-arg-z*)
		     (with-imm-temps (x8632::imm0) (ptr-reg)
		       (x862-pop-register seg ptr-reg)
		       (case size
			 (8 (! mem-set-c-doubleword *x862-imm0* ptr-reg offval))
			 (4 (! mem-set-c-fullword *x862-imm0* ptr-reg offval))
			 (2 (! mem-set-c-halfword *x862-imm0* ptr-reg offval))
			 (1 (! mem-set-c-byte *x862-imm0* ptr-reg offval))))))
		  (:x8664
		   (with-imm-target (x8664::imm0) (ptr-reg :address)
		     (x862-pop-register seg ptr-reg)
		     (case size
		       (8 (! mem-set-c-doubleword *x862-imm0* ptr-reg offval))
		       (4 (! mem-set-c-fullword *x862-imm0* ptr-reg offval))
		       (2 (! mem-set-c-halfword *x862-imm0* ptr-reg offval))
		       (1 (! mem-set-c-byte *x862-imm0* ptr-reg offval))))))
		 (if for-value
		   (<- *x862-arg-z*)))
		(t
		 (with-imm-target () (ptr-reg :address)
		   (with-additional-imm-reg (*x862-arg-z* ptr-reg)
		     (with-imm-target (ptr-reg) (offset-reg :address)
		       (x862-two-targeted-reg-forms seg ptr ptr-reg offset ($ *x862-arg-z*))
		       (! fixnum->signed-natural offset-reg *x862-arg-z*)
		       (! fixnum-add2 ptr-reg offset-reg)
		       (x862-push-register seg ptr-reg))))
		 (val-to-argz-and-imm0)
		 (target-arch-case
		  (:x8632
		     ;; Ensure imm0 is marked as in use so that some
		     ;; vinsn doesn't decide to use it a temp.
		     (with-additional-imm-reg ()
		       (with-imm-temps (x8632::imm0) (ptr-reg)
			 (x862-pop-register seg ptr-reg)
			 (case size
			   (8 (! mem-set-c-doubleword *x862-imm0* ptr-reg 0))
			   (4 (! mem-set-c-fullword *x862-imm0* ptr-reg 0))
			   (2 (! mem-set-c-halfword *x862-imm0* ptr-reg 0))
			   (1 (! mem-set-c-byte *x862-imm0* ptr-reg 0))))))
		  (:x8664
		   (with-imm-target (x8664::imm0) (ptr-reg :address)
		     (x862-pop-register seg ptr-reg)
		     (case size
		       (8 (! mem-set-c-doubleword *x862-imm0* ptr-reg 0))
		       (4 (! mem-set-c-fullword *x862-imm0* ptr-reg 0))
		       (2 (! mem-set-c-halfword *x862-imm0* ptr-reg 0))
		       (1 (! mem-set-c-byte *x862-imm0* ptr-reg 0))))))
		 (if for-value
		   (< *x862-arg-z*))))

          (^))))))





(defun x862-encoding-undo-count (encoding)
 (svref encoding 0))

(defun x862-encoding-cstack-depth (encoding)    ; hardly ever interesting
  (svref encoding 1))

(defun x862-encoding-vstack-depth (encoding)
  (svref encoding 2))

(defun x862-encoding-vstack-top (encoding)
  (svref encoding 3))

(defun x862-encode-stack ()
  (vector *x862-undo-count* *x862-cstack* *x862-vstack* *x862-top-vstack-lcell*))

(defun x862-decode-stack (encoding)
  (values (x862-encoding-undo-count encoding)
          (x862-encoding-cstack-depth encoding)
          (x862-encoding-vstack-depth encoding)
          (x862-encoding-vstack-top encoding)))

(defun x862-equal-encodings-p (a b)
  (dotimes (i 3 t)
    (unless (eq (svref a i) (svref b i)) (return))))

(defun x862-open-undo (&optional (reason $undocatch) (curstack (x862-encode-stack)))
  (set-fill-pointer 
   *x862-undo-stack*
   (set-fill-pointer *x862-undo-because* *x862-undo-count*))
  (vector-push-extend curstack *x862-undo-stack*)
  (vector-push-extend reason *x862-undo-because*)
  (setq *x862-undo-count* (%i+ *x862-undo-count* 1)))

(defun x862-close-undo (&aux
                        (new-count (%i- *x862-undo-count* 1))
                        (i (aref *x862-undo-stack* new-count)))
  (multiple-value-setq (*x862-undo-count* *x862-cstack* *x862-vstack* *x862-top-vstack-lcell*)
    (x862-decode-stack i))
  (set-fill-pointer 
   *x862-undo-stack*
   (set-fill-pointer *x862-undo-because* new-count)))





;;; "Trivial" means can be evaluated without allocating or modifying registers.
;;; Interim definition, which will probably stay here forever.
(defun x862-trivial-p (form &aux op bits)
  (setq form (nx-untyped-form form))
  (and
   (consp form)
   (not (eq (setq op (%car form)) (%nx1-operator call)))
   (or
    (nx-null form)
    (nx-t form)
    (eq op (%nx1-operator simple-function))
    (eq op (%nx1-operator fixnum))
    (eq op (%nx1-operator immediate))
    #+nil
    (eq op (%nx1-operator bound-special-ref))
    (and (or (eq op (%nx1-operator inherited-arg)) 
             (eq op (%nx1-operator lexical-reference)))
         (or (%ilogbitp $vbitpunted (setq bits (nx-var-bits (cadr form))))
             (neq (%ilogior (%ilsl $vbitclosed 1) (%ilsl $vbitsetq 1))
                  (%ilogand (%ilogior (%ilsl $vbitclosed 1) (%ilsl $vbitsetq 1)) bits)))))))

(defun x862-lexical-reference-p (form)
  (when (acode-p form)
    (let ((op (acode-operator (setq form (acode-unwrapped-form-value form)))))
      (when (or (eq op (%nx1-operator lexical-reference))
                (eq op (%nx1-operator inherited-arg)))
        (%cadr form)))))

(defun x862-ref-symbol-value (seg vreg xfer sym check-boundp)
  (declare (ignorable check-boundp))
  (setq check-boundp (not *x862-reckless*))
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (when (or check-boundp vreg)
      (unless vreg (setq vreg ($ *x862-arg-z*)))
      (if (eq sym '*interrupt-level*)
          (ensuring-node-target (target vreg)
            (! ref-interrupt-level target))
          (if *x862-open-code-inline*
            (ensuring-node-target (target vreg)
              (with-node-target (target) src
                (let* ((vcell (x862-symbol-value-cell sym))
                       (reg (x862-register-constant-p vcell)))
                  (if reg
                    (setq src reg)
                    (x862-store-immediate seg vcell src)))
                (if check-boundp
                  (! ref-symbol-value-inline target src)
                  (! %ref-symbol-value-inline target src))))
            (let* ((src ($ *x862-arg-z*))
                   (dest ($ *x862-arg-z*)))
              (x862-store-immediate seg (x862-symbol-value-cell sym) src)
              (if check-boundp
                (! ref-symbol-value dest src)
                (! %ref-symbol-value dest src))
              (<- dest)))))
    (^)))

;;; Should be less eager to box result
(defun x862-extract-charcode (seg vreg xfer char safe)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((src (x862-one-untargeted-reg-form seg char *x862-arg-z*)))
      (when safe
        (! trap-unless-character src))
      (if vreg
        (ensuring-node-target (target vreg)
          (! character->fixnum target src)))
      (^))))
  

(defun x862-reference-list (seg vreg xfer listform safe refcdr)
  (if (x862-form-typep listform 'list)
    (setq safe nil))                    ; May also have been passed as NIL.
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((src (x862-one-untargeted-reg-form seg listform *x862-arg-z*)))
      (when safe
        (! trap-unless-list src))
      (if vreg
        (if (eq vreg :push)
          (if refcdr
            (! %vpush-cdr src)
            (! %vpush-car src))
          (ensuring-node-target (target vreg)
            (if refcdr
              (! %cdr target src)
              (! %car target src)))))
      (^))))



(defun x862-misc-byte-count (subtag element-count)
  (funcall (arch::target-array-data-size-function
            (backend-target-arch *target-backend*))
           subtag element-count))


;;; The naive approach is to vpush all of the initforms, allocate the
;;; miscobj, then sit in a loop vpopping the values into the vector.
;;; That's "naive" when most of the initforms in question are
;;; "side-effect-free" (constant references or references to un-SETQed
;;; lexicals), in which case it makes more sense to just store the
;;; things into the vector cells, vpushing/ vpopping only those things
;;; that aren't side-effect-free.  (It's necessary to evaluate any
;;; non-trivial forms before allocating the miscobj, since that
;;; ensures that the initforms are older (in the EGC sense) than it
;;; is.)  The break-even point space-wise is when there are around 3
;;; non-trivial initforms to worry about.


(defun x862-allocate-initialized-gvector (seg vreg xfer subtag initforms)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if (null vreg)
      (dolist (f initforms) (x862-form seg nil nil f))
      (let* ((*x862-vstack* *x862-vstack*)
             (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
             (arch (backend-target-arch *target-backend*))
             (n (length initforms))
             (nntriv (let* ((count 0)) 
                       (declare (fixnum count))
                       (dolist (f initforms count) 
                         (unless (x86-side-effect-free-form-p f)
                           (incf count)))))
             (header (arch::make-vheader n subtag)))
        (declare (fixnum n nntriv))
        (cond ((or *x862-open-code-inline* (> nntriv 3))
               (x862-formlist seg initforms nil)
	       (target-arch-case
		(:x8632
		 (x862-lri seg *x862-imm0* header)
		 (! setup-uvector-allocation *x862-imm0*)
		 (x862-lri seg *x862-imm0* (- (ash (logandc2 (+ n 2) 1) (arch::target-word-shift arch)) x8632::fulltag-misc)))
		(:x8664
		 (x862-lri seg *x862-imm0* header)
		 (x862-lri seg x8664::imm1 (- (ash (logandc2 (+ n 2) 1) (arch::target-word-shift arch)) x8664::fulltag-misc))))
               (! %allocate-uvector vreg)
               (unless (eql n 0)
                 (! %init-gvector vreg  (ash n (arch::target-word-shift arch)))))
              (t
               (let* ((pending ())
                      (vstack *x862-vstack*))
                 (declare (fixnum vstack))
                 (dolist (form initforms)
                   (if (x86-side-effect-free-form-p form)
                     (push form pending)
                     (progn
                       (push nil pending)
                       (x862-vpush-register seg (x862-one-untargeted-reg-form seg form *x862-arg-z*)))))
		 (target-arch-case
		  (:x8632
		   (x862-lri seg *x862-imm0* header)
		   (! setup-uvector-allocation *x862-imm0*)
		   (x862-lri seg *x862-imm0* (- (ash (logandc2 (+ n 2) 1) (arch::target-word-shift arch)) x8632::fulltag-misc)))
		  (:x8664
		   (x862-lri seg *x862-imm0* header)
		   (x862-lri seg x8664::imm1 (- (ash (logandc2 (+ n 2) 1) (arch::target-word-shift arch)) x8664::fulltag-misc))))
                 (ensuring-node-target (target vreg)
                   (! %allocate-uvector target)
                   (with-node-temps (target) (nodetemp)
                     (do* ((forms pending (cdr forms))
                           (index (1- n) (1- index))
                           (pushed-cell (+ vstack (the fixnum (ash nntriv (arch::target-word-shift arch))))))
                          ((null forms))
                       (declare (list forms) (fixnum pushed-cell))
                       (let* ((form (car forms))
                              (reg nodetemp))
                         (if form
                           (setq reg (x862-one-untargeted-reg-form seg form nodetemp))
                           (progn
                             (decf pushed-cell *x862-target-node-size*)
                             (x862-stack-to-register seg (x862-vloc-ea pushed-cell) nodetemp)))
                         (! misc-set-c-node reg target index)))))
                 (! vstack-discard nntriv))
               ))))
     (^)))

;;; Heap-allocated constants -might- need memoization: they might be newly-created,
;;; as in the case of synthesized toplevel functions in .pfsl files.
(defun x862-acode-needs-memoization (valform)
  (if (x862-form-typep valform 'fixnum)
    nil
    (let* ((val (acode-unwrapped-form-value valform)))
      (if (or (eq val *nx-t*)
              (eq val *nx-nil*)
              (and (acode-p val)
                   (let* ((op (acode-operator val)))
                     (or (eq op (%nx1-operator fixnum)) #|(eq op (%nx1-operator immediate))|#))))
        nil
        t))))

(defun x862-modify-cons (seg vreg xfer ptrform valform safe setcdr returnptr)
  (if (x862-form-typep ptrform 'cons)
    (setq safe nil))                    ; May also have been passed as NIL.
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (ptr-vreg val-vreg) (x862-two-targeted-reg-forms seg ptrform ($ *x862-arg-y*) valform ($ *x862-arg-z*))
      (when safe
        (! trap-unless-cons ptr-vreg))
      (if setcdr
        (! call-subprim-2 ($ *x862-arg-z*) (subprim-name->offset '.SPrplacd) ptr-vreg val-vreg)
        (! call-subprim-2 ($ *x862-arg-z*) (subprim-name->offset '.SPrplaca) ptr-vreg val-vreg))
      (if returnptr
        (<- ptr-vreg)
        (<- val-vreg))
      (^))))



(defun x862-find-nilret-label ()
  (dolist (l *x862-nilret-labels*)
    (destructuring-bind (label vsp csp register-restore-count register-restore-ea &rest agenda) l
      (and (or (and (eql 0 register-restore-count)
                    (or (not (eql 0 vsp))
                        (eq vsp *x862-vstack*)))
                (and 
                 (eq register-restore-count *x862-register-restore-count*)
                 (eq vsp *x862-vstack*)))
           (or agenda (eq csp *x862-cstack*))
           (eq register-restore-ea *x862-register-restore-ea*)
           (eq (%ilsr 1 (length agenda)) *x862-undo-count*)
           (dotimes (i (the fixnum *x862-undo-count*) t) 
             (unless (and (eq (pop agenda) (aref *x862-undo-because* i))
                          (eq (pop agenda) (aref *x862-undo-stack* i)))
               (return)))
           (return label)))))

(defun x862-record-nilret-label ()
  (let* ((lab (backend-get-next-label))
         (info nil))
    (dotimes (i (the fixnum *x862-undo-count*))
      (push (aref *x862-undo-because* i) info)
      (push (aref *x862-undo-stack* i) info))
    (push (cons
                 lab 
                 (cons
                  *x862-vstack*
                  (cons 
                   *x862-cstack*
                   (cons
                    *x862-register-restore-count*
                    (cons
                     *x862-register-restore-ea*
                     (nreverse info))))))
          *x862-nilret-labels*)
    lab))

;;; If we know that the form is something that sets a CR bit,
;;; allocate a CR field and evaluate the form in such a way
;;; as to set that bit.
;;; If it's a compile-time constant, branch accordingly and
;;; let the dead code die.
;;; Otherwise, evaluate it to some handy register and compare
;;; that register to RNIL.
;;; "XFER" is a compound destination.
(defun x862-conditional-form (seg xfer form)
  (let* ((uwf (acode-unwrapped-form-value form)))
    (if (nx-null uwf)
      (x862-branch seg (x862-cd-false xfer))
      (if (x86-constant-form-p uwf)
        (x862-branch seg (x862-cd-true xfer))
        (with-crf-target () crf
          (let* ((ea (x862-lexical-reference-ea form nil)))
            (if (and ea (memory-spec-p ea))
              (x862-compare-ea-to-nil seg crf xfer ea x86::x86-e-bits nil)
              (x862-form seg crf xfer form))))))))

      
(defun x862-branch (seg xfer &optional cr-bit true-p)
  (let* ((*x862-vstack* *x862-vstack*)
         (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
    (with-x86-local-vinsn-macros (seg)
      (setq xfer (or xfer 0))
      (when (logbitp $backend-mvpass-bit xfer) ;(x862-mvpass-p cd)
        (setq xfer (logand (lognot $backend-mvpass-mask) xfer))
        (unless *x862-returning-values*
          (x862-vpush-register seg *x862-arg-z*)
          (x862-set-nargs seg 1)))
      (if (neq 0 xfer)
        (if (eq xfer $backend-return)    ;; xfer : RETURN ==> popj
          (x862-do-return seg)
          (if (not (x862-cd-compound-p xfer))
            (-> xfer)  ;; xfer : label# ==> jmp label#
            ;; cd is compound : (<true> / <false>)
            (let* ((truebranch (x862-cd-true xfer))
                   (falsebranch (x862-cd-false xfer))
                   (tbranch (if true-p truebranch falsebranch))
                   (nbranch (if true-p falsebranch truebranch))
                   (tn0 (neq 0 tbranch))
                   (tnret (neq $backend-return tbranch))
                   (nn0 (neq 0 nbranch))
                   (nnret (neq $backend-return nbranch))
                   (tlabel (if (and tnret tn0) (aref *backend-labels* tbranch)))
                   (nlabel (if (and nnret nn0) (aref *backend-labels* nbranch))))
              (unless cr-bit (setq cr-bit x86::x86-e-bits))
              (if (and tn0 tnret nn0 nnret)
                (progn
                  (! cbranch-true tlabel cr-bit )    ;; (label# /  label#)
                  (-> nbranch)))
                (if (and nnret tnret)
                  (if nn0
                    (! cbranch-false nlabel cr-bit)
                    (! cbranch-true tlabel cr-bit))
                  (let* ((aux-label (backend-get-next-label))
                         (auxl (aref *backend-labels* aux-label)))
                    (if tn0
                      (! cbranch-true auxl cr-bit)
                      (! cbranch-false auxl cr-bit) )
                    (x862-do-return seg)
                    (@ aux-label))))))))))

(defun x862-cd-merge (cd label)
  (setq cd (or cd 0))
  (let ((mvpass (logbitp $backend-mvpass-bit cd)))
    (if (neq 0 (logand (lognot $backend-mvpass-mask) cd))
      (if (x862-cd-compound-p cd)
        (x862-make-compound-cd
         (x862-cd-merge (x862-cd-true cd) label)
         (x862-cd-merge (x862-cd-false cd) label)
         mvpass)
        cd)
      (if mvpass 
        (logior $backend-mvpass-mask label)
        label))))

(defun x862-mvpass-p (xfer)
  (if xfer (or (logbitp $backend-mvpass-bit xfer) (eq xfer $backend-mvpass))))

(defun x862-cd-compound-p (xfer)
  (if xfer (logbitp $backend-compound-branch-target-bit xfer)))

(defun x862-cd-true (xfer)
 (if (x862-cd-compound-p xfer)
   (ldb  $backend-compound-branch-true-byte xfer)
  xfer))

(defun x862-cd-false (xfer)
 (if (x862-cd-compound-p xfer)
   (ldb  $backend-compound-branch-false-byte xfer)
   xfer))

(defun x862-make-compound-cd (tpart npart &optional mvpass-p)
  (dpb (or npart 0) $backend-compound-branch-false-byte
       (dpb (or tpart 0) $backend-compound-branch-true-byte
            (logior (if mvpass-p $backend-mvpass-mask 0) $backend-compound-branch-target-mask))))

(defun x862-invert-cd (cd)
  (if (x862-cd-compound-p cd)
    (x862-make-compound-cd (x862-cd-false cd) (x862-cd-true cd) (logbitp $backend-mvpass-bit cd))
    cd))



;;; execute body, cleanup afterwards (if need to)
(defun x862-undo-body (seg vreg xfer body old-stack)
  (let* ((current-stack (x862-encode-stack))
         (numundo (%i- *x862-undo-count* (x862-encoding-undo-count old-stack))))
    (declare (fixnum numundo))
    (with-x86-local-vinsn-macros (seg vreg xfer)
      (if (eq current-stack old-stack)
        (x862-form seg vreg xfer body)
        (if (eq xfer $backend-return)
          (progn
            (x862-form seg vreg xfer body)
            (dotimes (i numundo) (x862-close-undo)))
          (if (x862-mvpass-p xfer)
            (progn
              (x862-mvpass seg body) ; presumed to be ok
              (let* ((*x862-returning-values* :pass))
                (x862-nlexit seg xfer numundo)
                (^))
              (dotimes (i numundo) (x862-close-undo)))
            (progn
              ;; There are some cases where storing thru *x862-arg-z*
              ;; can be avoided (stores to vlocs, specials, etc.) and
              ;; some other case where it can't ($test, $vpush.)  The
              ;; case of a null vd can certainly avoid it; the check
              ;; of numundo is to keep $acc boxed in case of nthrow.
              (x862-form  seg (if (or vreg (not (%izerop numundo))) *x862-arg-z*) nil body)
              (x862-unwind-set seg xfer old-stack)
              (when vreg (<- *x862-arg-z*))
              (^))))))))


(defun x862-unwind-set (seg xfer encoding)
  (multiple-value-bind (target-catch target-cstack target-vstack target-vstack-lcell)
                       (x862-decode-stack encoding)
    (x862-unwind-stack seg xfer target-catch target-cstack target-vstack)
    (setq *x862-undo-count* target-catch 
          *x862-cstack* target-cstack
          *x862-vstack* target-vstack
          *x862-top-vstack-lcell* target-vstack-lcell)))

(defun x862-unwind-stack (seg xfer target-catch target-cstack target-vstack)
  (let* ((current-catch *x862-undo-count*)
         (current-cstack *x862-cstack*)
         (current-vstack *x862-vstack*)
         (diff (%i- current-catch target-catch))
         target
         (exit-vstack current-vstack))
    (declare (ignorable target))
    (when (neq 0 diff)
      (setq exit-vstack (x862-nlexit seg xfer diff))
      (multiple-value-setq (target current-cstack current-vstack)
                           (x862-decode-stack (aref *x862-undo-stack* target-catch))))
    (if (%i< 0 (setq diff (%i- current-cstack target-cstack)))
      (compiler-bug "Bug: adjust foreign stack ?"))
    (if (%i< 0 (setq diff (%i- current-vstack target-vstack)))
      (with-x86-local-vinsn-macros (seg)
        (! vstack-discard (ash diff (- *x862-target-fixnum-shift*)))))
    exit-vstack))

;;; We can sometimes combine unwinding the catch stack with returning
;;; from the function by jumping to a subprim that knows how to do
;;; this.  If catch frames were distinguished from unwind-protect
;;; frames, we might be able to do this even when saved registers are
;;; involved (but the subprims restore them from the last catch
;;; frame.)  *** there are currently only subprims to handle the "1
;;; frame" case; add more ***
(defun x862-do-return (seg)
  (let* ((*x862-vstack* *x862-vstack*)
         (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
         (mask *x862-register-restore-count*)
         (ea *x862-register-restore-ea*)
         (label nil)
         (vstack nil)
         (foldp (not *x862-open-code-inline*)))
    (if (%izerop mask) (setq mask nil))
    (with-x86-local-vinsn-macros (seg)
      (progn
        (setq vstack (x862-set-vstack (x862-unwind-stack seg $backend-return 0 0 #x7fffff)))
        (if *x862-returning-values*
          (cond ((and mask foldp (setq label (%cdr (assq vstack *x862-valret-labels*))))
                 (-> label))
                (t
                 (@ (setq label (backend-get-next-label)))
                 (push (cons vstack label) *x862-valret-labels*)
                 (x862-restore-nvrs seg ea mask nil)
                 (! nvalret)))
          (if (null mask)
            (! popj)
            (if (and foldp (setq label (assq *x862-vstack* *x862-popreg-labels*)))
              (-> (cdr label))
              (let* ((new-label (backend-get-next-label)))
                (@ new-label)
                (push (cons *x862-vstack* new-label) *x862-popreg-labels*)
                (x862-set-vstack (x862-restore-nvrs seg ea mask))
                (! popj)))))))
    nil))


(defun x862-mvcall (seg vreg xfer fn arglist &optional recursive-p)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (if (and (eq xfer $backend-return) (not (x862-tailcallok xfer)))
      (progn
        (x862-mvcall seg vreg $backend-mvpass fn arglist t)
        (let* ((*x862-returning-values* t)) (^)))
      (let* ((mv-p (x862-mv-p xfer)))
        (if (null arglist)
          (x862-call-fn seg vreg xfer fn arglist nil)
          (let* ((label (when (or recursive-p (x862-mvpass-p xfer)) (backend-get-next-label))))
            (when label
              (x862-vpush-label seg (aref *backend-labels* label)))
            (x862-temp-push-node seg (x862-one-untargeted-reg-form seg fn *x862-arg-z*))
            (x862-multiple-value-body seg (pop arglist))
            (x862-open-undo $undostkblk)
            (! save-values)
            (dolist (form arglist)
              (x862-multiple-value-body seg form)
              (! add-values))
            (! recover-values-for-mvcall)
            (x862-close-undo)
            (x862-temp-pop-node seg *x862-temp0*)
            (x862-invoke-fn seg *x862-temp0* nil nil xfer label)
            (when label
              ;; Pushed a label earlier, then returned to it.
              (setq *x862-top-vstack-lcell* (lcell-parent *x862-top-vstack-lcell*))
              (x862-adjust-vstack (- *x862-target-node-size*)))))
        (unless recursive-p
          (if mv-p
            (unless (eq xfer $backend-return)
              (let* ((*x862-returning-values* t))
                (^)))
            (progn 
              (<- *x862-arg-z*)
              (^))))))))




(defun x862-hard-opt-p (opts)
  (or
   (dolist (x (%cadr opts))
     (unless (nx-null x) (return t)))
   (dolist (x (%caddr opts))
     (when x (return t)))))

(defun x862-close-lambda (seg req opt rest keys auxen)
  (dolist (var req)
    (x862-close-var seg var))
  (dolist (var (%car opt))
    (x862-close-var seg var))
  (dolist (var (%caddr opt))
    (when var
      (x862-close-var seg var)))
  (if rest
    (x862-close-var seg rest))
  (dolist (var (%cadr keys))
    (x862-close-var seg var))
  (dolist (var (%caddr keys))
    (if var (x862-close-var seg var)))
  (dolist (var (%car auxen))
    (x862-close-var seg var)))

(defun x862-close-structured-var (seg var)
  (if (x862-structured-var-p var)
    (apply #'x862-close-structured-lambda seg (cdr var))
    (x862-close-var seg var)))

(defun x862-close-structured-lambda (seg whole req opt rest keys auxen)
  (if whole
    (x862-close-var seg whole))
  (dolist (var req)
    (x862-close-structured-var seg var))
  (dolist (var (%car opt))
    (x862-close-structured-var seg var))
  (dolist (var (%caddr opt))
    (when var
      (x862-close-var seg var)))
  (if rest
    (x862-close-structured-var seg rest))
  (dolist (var (%cadr keys))
    (x862-close-structured-var seg var))
  (dolist (var (%caddr keys))
    (if var (x862-close-var seg var)))
  (dolist (var (%car auxen))
    (x862-close-var seg var)))


(defun x862-init-regvar (seg var reg addr)
  (with-x86-local-vinsn-macros (seg)
    (x862-stack-to-register seg addr reg)
    (x862-set-var-ea seg var ($ reg))))

(defun x862-bind-structured-var (seg var vloc lcell &optional context)
  (if (not (x862-structured-var-p var))
    (let* ((reg (x862-assign-register-var var)))
      (if reg
        (x862-init-regvar seg var reg (x862-vloc-ea vloc))
        (x862-bind-var seg var vloc lcell)))
    (let* ((v2 (%cdr var))
           (v v2)
           (vstack *x862-vstack*)
           (whole (pop v))
           (req (pop v))
           (opt (pop v))
           (rest (pop v))
           (keys (pop v)))
      
      (apply #'x862-bind-structured-lambda seg 
             (x862-spread-lambda-list seg (x862-vloc-ea vloc) whole req opt rest keys context)
             vstack context v2))))

(defun x862-bind-structured-lambda (seg lcells vloc context whole req opt rest keys auxen
                        &aux (nkeys (list-length (%cadr keys))))
  (declare (fixnum vloc))
  (when whole
    (x862-bind-structured-var seg whole vloc (pop lcells))
    (incf vloc *x862-target-node-size*))
  (dolist (arg req)
    (x862-bind-structured-var seg arg vloc (pop lcells) context)
    (incf vloc *x862-target-node-size*))
  (when opt
   (if (x862-hard-opt-p opt)
     (setq vloc (apply #'x862-structured-initopt seg lcells vloc context opt)
           lcells (nthcdr (ash (length (car opt)) 1) lcells))
     (dolist (var (%car opt))
       (x862-bind-structured-var seg var vloc (pop lcells) context)
       (incf vloc *x862-target-node-size*))))
  (when rest
    (x862-bind-structured-var seg rest vloc (pop lcells) context)
    (incf vloc *x862-target-node-size*))
  (when keys
    (apply #'x862-structured-init-keys seg lcells vloc context keys)
    (setq vloc (%i+ vloc (* *x862-target-node-size* (+ nkeys nkeys)))))
  (x862-seq-bind seg (%car auxen) (%cadr auxen)))

(defun x862-structured-var-p (var)
  (and (consp var) (or (eq (%car var) *nx-lambdalist*)
                       (eq (%car var) (%nx1-operator lambda-list)))))

(defun x862-simple-var (var &aux (bits (cadr var)))
  (if (or (%ilogbitp $vbitclosed bits)
          (%ilogbitp $vbitspecial bits))
    (nx-error "Non-simple-variable ~S" (%car var))
    var))

(defun x862-nlexit (seg xfer &optional (nlevels 0))
  (let* ((numnthrow 0)
         (n *x862-undo-count*)
         (cstack *x862-cstack*)
         (vstack *x862-vstack*)
         (target-vstack)
         (lastcatch n)
         (returning (eq xfer $backend-return))
         (junk1 nil)
         (unbind ())
         (dest (%i- n nlevels))
         (retval *x862-returning-values*)
         reason)
    (declare (ignorable junk1))
    (with-x86-local-vinsn-macros (seg)
      (when (neq 0 nlevels)
        (let* ((num-temp-frames 0)
               (num-c-frames 0))
          (declare (fixnum num-temp-frames num-c-frames))
          (flet ((pop-temp-frames ()
                   (dotimes (i num-temp-frames)
                     (! discard-temp-frame)))
                 (pop-c-frames ()
                   (dotimes (i num-c-frames)
                     (! discard-c-frame)))
                 (throw-through-numnthrow-catch-frames ()
                   (when (neq 0 numnthrow)
                     (let* ((tag-label (backend-get-next-label))
                            (tag-label-value (aref *backend-labels* tag-label)))
                       (x862-lri seg *x862-imm0* (ash numnthrow *x862-target-fixnum-shift*))
                       (if retval
                         (! nthrowvalues tag-label-value)
                         (! nthrow1value tag-label-value))
                       (@= tag-label))
                     (setq numnthrow 0)
                     (multiple-value-setq (junk1 cstack vstack)
                       (x862-decode-stack (aref *x862-undo-stack* lastcatch)))))
                 (find-last-catch ()
                   (do* ((n n)
                         (reasons *x862-undo-because*))
                        ((= n dest))
                     (declare (fixnum n))
                     (when (eql $undocatch (aref reasons (decf n)))
                       (incf numnthrow)
                       (setq lastcatch n)))))
                            
            (find-last-catch)
            (throw-through-numnthrow-catch-frames)
            (setq n lastcatch)
            (while (%i> n dest)
              (setq reason (aref *x862-undo-because* (setq n (%i- n 1))))
              (cond ((eql $undostkblk reason)
                     (incf num-temp-frames))
                    ((eql $undo-x86-c-frame reason)
                     (incf num-c-frames))
                    ((or (eql reason $undospecial)
                        (eql reason $undointerruptlevel))
                  (push reason unbind))))
            (if unbind
	      (target-arch-case
	       (:x8632
		(let* ((*available-backend-node-temps* *available-backend-node-temps*))
		  (when retval (use-node-temp x8632::nargs))
		  (x862-dpayback-list seg (nreverse unbind))))
	       (:x8664
		(let* ((*available-backend-imm-temps* *available-backend-imm-temps*))
		  (when retval (use-imm-temp x8664::nargs.q))
		  (x862-dpayback-list seg (nreverse unbind))))))
            (when (and (neq lastcatch dest)
                       (%i>
                        vstack
                        (setq target-vstack 
                              (nth-value 2 (x862-decode-stack (aref *x862-undo-stack* dest)))))
                       (neq retval t))
              (unless returning
                (let ((vdiff (%i- vstack target-vstack)))
                  (if retval
                    (progn
                      (x862-lri seg *x862-imm0* vdiff)
                      (! slide-values))
                    (! adjust-vsp vdiff)))))
            (pop-temp-frames)
            (pop-c-frames)))
        vstack))))


;;; Restore the most recent dynamic bindings.  Bindings
;;; of *INTERRUPT-LEVEL* get special treatment.
(defun x862-dpayback-list (seg reasons)
  (with-x86-local-vinsn-macros (seg)
    (let* ((n 0))
      (declare (fixnum n))
      (dolist (r reasons (if (> n 0) (! dpayback n)))
        (if (eql r $undospecial)
          (incf n)
          (if (eql r $undointerruptlevel)
            (progn
              (when (> n 0)
                (! dpayback n)
                (setq n 0))
              (if (and *x862-open-code-inline*
		       (target-arch-case
			(:x8632 nil)
			(:x8664 t)))
                (let* ((*available-backend-node-temps* (bitclr *x862-arg-z* (bitclr x8664::rcx *available-backend-node-temps*))))
                  (! unbind-interrupt-level-inline))
                (! unbind-interrupt-level)))
            (compiler-bug "unknown payback token ~s" r)))))))

(defun x862-spread-lambda-list (seg listform whole req opt rest keys 
                                    &optional enclosing-ea cdr-p)
  (with-x86-local-vinsn-macros (seg)
    (let* ((numopt (length (%car opt)))
           (nkeys (length (%cadr keys)))
           (numreq (length req))
           (vtotal numreq)
           (old-top *x862-top-vstack-lcell*)
           (argreg ($ (target-arch-case
		       (:x8632 ($ x8632::temp1))
		       (:x8664 ($ x8664::temp0)))))
           (keyvectreg (target-arch-case
			(:x8632 ($ x8632::arg_y))
			(:x8664 ($ x8664::arg_x))))
           (doadlword (dpb nkeys (byte 8 16) (dpb numopt (byte 8 8) (dpb numreq (byte 8 0) 0 )))))
      (declare (fixnum numopt nkeys numreq vtotal doadlword))
      (when (or (> numreq 255) (> numopt 255) (> nkeys 255))
        (compiler-bug "A lambda list can contain a maximum of 255 required, 255 optional, and 255 keywords args"))
      (if (fixnump listform)
        (x862-store-ea seg listform argreg)
        (x862-one-targeted-reg-form seg listform argreg))
      (when whole
        (x862-vpush-register seg argreg :reserved))
      (when keys
        (setq doadlword (%ilogior2 (ash #x80000000 -6) doadlword))
        (incf  vtotal (%ilsl 1 nkeys))
        (if (%car keys)                 ; &allow-other-keys
          (setq doadlword (%ilogior doadlword (ash #x80000000 -5))))
        (x862-store-immediate seg (%car (%cdr (%cdr (%cdr (%cdr keys))))) keyvectreg))
      (when opt
        (setq vtotal (%i+ vtotal numopt))
        (when (x862-hard-opt-p opt)
          (setq doadlword (%ilogior2 doadlword (ash #x80000000 -7)))
          (setq vtotal (%i+ vtotal numopt))))
      (when rest
        (setq doadlword (%ilogior2 (ash #x80000000 -4) doadlword) vtotal (%i+ vtotal 1)))
      (x862-reserve-vstack-lcells vtotal)
      (! load-adl doadlword)
      (if cdr-p
        (! macro-bind)
        (if enclosing-ea
          (progn
            (x862-store-ea seg enclosing-ea *x862-arg-z*)
            (! destructuring-bind-inner))
          (! destructuring-bind)))
      (x862-set-vstack (%i+ *x862-vstack* (* *x862-target-node-size* vtotal)))
      (x862-collect-lcells :reserved old-top))))


(defun x862-tailcallok (xfer)
  (and (eq xfer $backend-return)
       *x862-tail-allow*
       (eq 0 *x862-undo-count*)))

(defun x862-mv-p (cd)
  (or (eq cd $backend-return) (x862-mvpass-p cd)))

(defun x862-expand-note (frag-list note)
  (let* ((lab (vinsn-note-label note)))
    (case (vinsn-note-class note)
      ((:regsave :begin-variable-scope :end-variable-scope
        :source-location-begin :source-location-end)
       (setf (vinsn-label-info lab) (emit-x86-lap-label frag-list lab))))))

(defun x86-emit-instruction-from-vinsn (opcode-template
                                        form
                                        frag-list
                                        instruction
                                        immediate-operand)
  #+debug
  (format t "~&~a" (cons (x86::x86-opcode-template-mnemonic opcode-template)
                         form))
  (set-x86-instruction-template instruction opcode-template)
  (let* ((operand-classes (x86::x86-opcode-template-operand-classes
                           opcode-template))
         (operand-types  (x86::x86-opcode-template-operand-types
                          opcode-template))
         (register-table (target-arch-case
			  (:x8632 x86::*x8632-register-entries*)
                          (:x8664 x86::*x8664-register-entries*))))
    (dotimes (i (length operand-classes))
      (let* ((operand (pop form))
             (insert-function (svref operand-classes i))
             (type (svref operand-types i))
             (insert-keyword (svref x86::*x86-operand-insert-function-keywords*
				    insert-function)))
        #+debug
        (format t "~& insert-function = ~s, operand = ~s"
                insert-keyword
                operand)
        (ecase insert-keyword
          (:insert-nothing )
          ((:insert-modrm-reg :insert-xmm-reg)
           (x86::insert-modrm-reg-entry instruction
                                        (if (logtest (x86::encode-operand-type
                                                      :reg8)
                                                     type)
                                          (x86::x86-reg8 operand)
                                          (svref register-table operand))))
          ((:insert-modrm-rm :insert-xmm-rm)
           (x86::insert-modrm-rm-entry instruction
                                       (if (logtest (x86::encode-operand-type
                                                     :reg8)
                                                    type)
                                         (x86::x86-reg8 operand)
                                         (svref register-table operand))))
          (:insert-memory
           (destructuring-bind (seg disp base index scale) operand
             (when seg (setq seg
                             (svref x86::*x86-seg-entries* (x86::reg-entry-reg-num (svref register-table seg)))))
             ;; Optimize things like this later; almost all
             ;; displacements will be constants at this point.
             (when disp  (setq disp (parse-x86-lap-expression disp)))
             (when base (setq base (svref register-table base)))
             (when index (setq index (svref register-table index)))
             (when scale (setq scale (1- (integer-length scale))))
             (x86::insert-memory-operand-values
              instruction
              seg
              disp
              base
              index
              scale
              (if (or base index)
                (if disp
                  (logior (optimize-displacement-type disp)
                          (x86::encode-operand-type  :baseindex))
                  (x86::encode-operand-type :baseindex))
                (optimize-displacement-type disp)))))          
          (:insert-opcode-reg
           (x86::insert-opcode-reg-entry instruction
                                         (if (logtest (x86::encode-operand-type
                                                       :reg8)
                                                      type)
                                           (x86::x86-reg8 operand)
                                           (svref register-table operand))))
          (:insert-opcode-reg4
           (x86::insert-opcode-reg4-entry instruction
                                          (if (logtest (x86::encode-operand-type
                                                        :reg8)
                                                       type)
                                            (x86::x86-reg8 operand)
                                            (svref register-table operand))))
          (:insert-reg4-pseudo-rm-high
           (x86::insert-reg4-pseudo-rm-high-entry instruction
                                                  (svref register-table operand)))
          (:insert-reg4-pseudo-rm-low
           (x86::insert-reg4-pseudo-rm-low-entry instruction
                                                  (svref register-table operand)))
          (:insert-cc
           (unless (typep operand 'x86-lap-expression)
             (setq operand (parse-x86-lap-expression operand)))
           (setf (ldb (byte 4 0)
                      (x86::x86-instruction-base-opcode instruction))
                 (x86-lap-expression-value operand)))
          (:insert-label
           (setf (x86::x86-instruction-extra instruction)
                 (find-or-create-x86-lap-label operand)))
          (:insert-imm8-for-int
           )
          (:insert-extra
           )
          (:insert-imm8
           (setf (x86::x86-immediate-operand-type immediate-operand)
                 (x86::encode-operand-type :imm8)
                 (x86::x86-immediate-operand-value immediate-operand)
                 (parse-x86-lap-expression operand)
                 (x86::x86-instruction-imm instruction)
                 immediate-operand))
          (:insert-imm8s
           (setf (x86::x86-immediate-operand-type immediate-operand)
                 (x86::encode-operand-type :imm8s)
                 (x86::x86-immediate-operand-value immediate-operand)
                 (parse-x86-lap-expression operand)
                 (x86::x86-instruction-imm instruction)
                 immediate-operand))
          (:insert-imm16
           (setf (x86::x86-immediate-operand-type immediate-operand)
                 (x86::encode-operand-type :imm16)
                 (x86::x86-immediate-operand-value immediate-operand)
                 (parse-x86-lap-expression operand)
                 (x86::x86-instruction-imm instruction)
                 immediate-operand))
          (:insert-imm32s
           (setf (x86::x86-immediate-operand-type immediate-operand)
                 (x86::encode-operand-type :imm32s)
                 (x86::x86-immediate-operand-value immediate-operand)
                 (parse-x86-lap-expression operand)
                 (x86::x86-instruction-imm instruction)
                 immediate-operand))
          (:insert-imm32
           (setf (x86::x86-immediate-operand-type immediate-operand)
                 (x86::encode-operand-type :imm32)
                 (x86::x86-immediate-operand-value immediate-operand)
                 (parse-x86-lap-expression operand)
                 (x86::x86-instruction-imm instruction)
                 immediate-operand))
          (:insert-imm64
           (setf (x86::x86-immediate-operand-type immediate-operand)
                 (x86::encode-operand-type :imm64)
                 (x86::x86-immediate-operand-value immediate-operand)
                 (parse-x86-lap-expression operand)
                 (x86::x86-instruction-imm instruction)
                 immediate-operand))
          (:insert-mmx-reg
           (x86::insert-mmx-reg-entry instruction
                                      (svref register-table operand)))
          (:insert-mmx-rm
           (x86::insert-mmx-rm-entry instruction
                                     (svref register-table operand)))
	  (:insert-self
	   (setf (x86::x86-immediate-operand-type immediate-operand)
		 (x86::encode-operand-type :self)
		 (x86::x86-immediate-operand-value immediate-operand)
		 (parse-x86-lap-expression operand)
		 (x86::x86-instruction-imm instruction)
		 immediate-operand)))))
    (x86-generate-instruction-code frag-list instruction)))
          
    
(defun x862-expand-vinsns (header frag-list instruction &optional uuo-frag-list)
  (let* ((immediate-operand (x86::make-x86-immediate-operand)))
    (do-dll-nodes (v header)
      (if (%vinsn-label-p v)
        (let* ((id (vinsn-label-id v)))
          (if (or (typep id 'fixnum) (null id))
            (when (or t (vinsn-label-refs v) (null id))
              (setf (vinsn-label-info v) (emit-x86-lap-label frag-list v)))
            (x862-expand-note frag-list id)))
        (x862-expand-vinsn v frag-list instruction immediate-operand uuo-frag-list)))
    (when uuo-frag-list
      (merge-dll-nodes frag-list uuo-frag-list)))
  ;;; This doesn't have too much to do with anything else that's
  ;;; going on here, but it needs to happen before the lregs
  ;;; are freed.  There really shouldn't be such a thing as a
  ;;; var-ea, of course ...
  (dolist (s *x862-recorded-symbols*)
    (let* ((var (car s))
           (ea (var-ea var)))
      (when (typep ea 'lreg)
        (setf (var-ea var) (lreg-value ea)))))
  (free-logical-registers)
  (x862-free-lcells))

;;; It's not clear whether or not predicates, etc. want to look
;;; at an lreg or just at its value slot.
;;; It's clear that the assembler just wants the value, and that
;;; the value had better be assigned by the time we start generating
;;; machine code.
;;; For now, we replace lregs in the operand vector with their values
;;; on entry, but it might be reasonable to make PARSE-OPERAND-FORM
;;; deal with lregs ...
(defun x862-expand-vinsn (vinsn frag-list instruction immediate-operand &optional uuo-frag-list)
  (let* ((template (vinsn-template vinsn))
         (main-frag-list frag-list)
         (vp (vinsn-variable-parts vinsn))
         (nvp (vinsn-template-nvp template))
         (unique-labels ()))
    (declare (fixnum nvp))
    (dotimes (i nvp)
      (let* ((val (svref vp i)))
        (when (typep val 'lreg)
          (setf (svref vp i) (lreg-value val)))))                       
    (dolist (name (vinsn-template-local-labels template))
      (let* ((unique (cons name nil)))
        (push unique unique-labels)
        (make-x86-lap-label unique)))
    (labels ((parse-operand-form (valform &optional for-pred)
               (cond ((typep valform 'keyword)
                      (if (eq valform :rcontext)
                        (backend-lisp-context-register *target-backend*)
                        (or (assq valform unique-labels)
                            (compiler-bug
                             "unknown vinsn label ~s" valform))))
                     ((atom valform) valform)
                     ((eq (car valform) :rcontext)
                      (if (>= (backend-lisp-context-register *target-backend*)
                              (target-arch-case
                               (:x8632 x86::+x8632-segment-register-offset+)
                               (:x8664 x86::+x8664-segment-register-offset+)))
                        (mapcar #'parse-operand-form `(:rcontext ,(cadr valform) nil nil nil))
                        (mapcar #'parse-operand-form `(nil ,(cadr valform) :rcontext nil nil))))
                     ((and (atom (cdr valform))
                           (typep (car valform) 'fixnum))
                      (svref vp (car valform)))
                     ((eq (car valform) :@)
                      (mapcar #'parse-operand-form (cdr valform)))
                     ((eq (car valform) :^)
                      (list :^ (parse-operand-form (cadr valform))))
                     (t (let* ((op-vals (cdr valform))
                               (parsed-ops (make-list (length op-vals)))
                               (tail parsed-ops))
                          (declare (dynamic-extent parsed-ops)
                                   (list parsed-ops tail))
                          (dolist (op op-vals
                                   (if for-pred
                                     (apply (car valform) parsed-ops)
                                     (parse-x86-lap-expression (cons (car valform) parsed-ops))))
                            (setq tail (cdr (rplaca tail (parse-operand-form op)))))))))
             (expand-insn-form (f)
               (let* ((operands (cdr f))
                      (head (make-list (length operands)))
                      (tail head))
                 (declare (dynamic-extent head)
                          (cons (head tail)))
                 (dolist (op operands)
                   (rplaca tail (parse-operand-form op))
                   (setq tail (cdr tail)))
                 (x86-emit-instruction-from-vinsn
                  (svref x86::*x86-opcode-templates* (car f))
                  head
                  frag-list
                  instruction
                  immediate-operand)))
             (eval-predicate (f)
               (case (car f)
                 (:pred (let* ((op-vals (cddr f))
                               (parsed-ops (make-list (length op-vals)))
                               (tail parsed-ops))
                          (declare (dynamic-extent parsed-ops)
                                   (cons parsed-ops tail))
                          (dolist (op op-vals (apply (cadr f) parsed-ops))
                            (setq tail (cdr (rplaca tail (parse-operand-form op t)))))))
                 (:not (not (eval-predicate (cadr f))))
                 (:or (dolist (pred (cadr f))
                        (when (eval-predicate pred)
                          (return t))))
                 (:and (dolist (pred (cadr f) t)
                         (unless (eval-predicate pred)
                           (return nil))))
                 (t (compiler-bug "Unknown predicate: ~s" f))))
             (expand-pseudo-op (f)
               (case (car f)
                 (:anchored-uuo-section
                  (expand-form '(:uuo-section))
                  (expand-form `(:long (:^ ,(cadr f)))))
                 (:anchored-uuo
                  (expand-form (cadr f))
                  ;; add a trailing 0 byte after the uu0
                  (frag-list-push-byte frag-list 0))
                 ((:uuo :uuo-section)
                      (if uuo-frag-list
                        (progn
                          (setq frag-list uuo-frag-list)
                          (finish-frag-for-align frag-list 2))
                        (compiler-bug "No frag-list for :uuo")))
                 ((:main :main-section)
                  (setq frag-list main-frag-list))
                 (t
                  (destructuring-bind (directive arg) f
                     (setq arg (parse-operand-form arg))
                     (let* ((exp (parse-x86-lap-expression arg))
                            (constantp (or (not (x86-lap-expression-p exp))
                                           (constant-x86-lap-expression-p exp))))
                       (if constantp
                         (let* ((val (x86-lap-expression-value exp)))
                           (ecase directive
                             (:byte (frag-list-push-byte frag-list val))
                             (:short (frag-list-push-16 frag-list val))
                             (:long (frag-list-push-32 frag-list val))
                             (:quad (frag-list-push-64 frag-list val))
                             (:align (finish-frag-for-align frag-list val))
                             (:talign (finish-frag-for-talign frag-list val))))
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
                             ((:align :talign) (compiler-bug "~s expression ~s not constant" directive arg)))
                           (when reloctype
                             (push
                              (make-reloc :type reloctype
                                          :arg exp
                                          :pos pos
                                          :frag frag)
                              (frag-relocs frag))))))))))
                   
             (expand-form (f)
               (if (keywordp f)
                 (emit-x86-lap-label frag-list (assq f unique-labels))
                 (if (atom f)
                   (compiler-bug "Invalid form in vinsn body: ~s" f)
                   (if (atom (car f))
                     (if (keywordp (car f))
                       (expand-pseudo-op f)
                       (expand-insn-form f))
                     (if (eval-predicate (car f))
                       (dolist (subform (cdr f))
                         (expand-form subform))))))))
      (declare (dynamic-extent #'expand-form #'parse-operand-form #'expand-insn-form #'eval-predicate))
      ;;(format t "~& vinsn = ~s" vinsn)
      (dolist (form (vinsn-template-body template))
	;;(format t "~&form = ~s" form)
        (expand-form form ))
      (setf (vinsn-variable-parts vinsn) nil)
      (when vp
        (free-varparts-vector vp)))))





(defun x862-builtin-index-subprim (idx)
  (let* ((arch (backend-target-arch *target-backend*))
         (table (arch::target-primitive->subprims  arch))
         (shift (arch::target-subprims-shift arch)))
    (dolist (cell table)
      (destructuring-bind ((low . high) . base) cell
        (if (and (>= idx low)
                 (< idx high))
          (return (+ base (ash (- idx low) shift))))))))

(defun x862-fixed-call-builtin (seg vreg xfer name subprim)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((index (arch::builtin-function-name-offset name))
           (idx-subprim (if index (x862-builtin-index-subprim index)))
           (tail-p (x862-tailcallok xfer)))
      (when tail-p
        (x862-restore-nvrs seg *x862-register-restore-ea* *x862-register-restore-count*)
        (x862-restore-full-lisp-context seg))
      (if idx-subprim
        (setq subprim idx-subprim)
        (if index (! lri ($ *x862-imm0*) (ash index *x862-target-fixnum-shift*))))
      (if tail-p
        (! jump-subprim subprim)
        (progn
          (! call-subprim subprim)
          (<- ($ *x862-arg-z*))
          (^))))))

(defun x862-unary-builtin (seg vreg xfer name form)
  (with-x86-local-vinsn-macros (seg)
    (x862-one-targeted-reg-form seg form ($ *x862-arg-z*))
    (x862-fixed-call-builtin seg vreg xfer name (subprim-name->offset '.SPcallbuiltin1))))

(defun x862-binary-builtin (seg vreg xfer name form1 form2)
  (with-x86-local-vinsn-macros (seg)
    (x862-two-targeted-reg-forms seg form1 ($ *x862-arg-y*) form2 ($ *x862-arg-z*))
    (x862-fixed-call-builtin seg vreg xfer name (subprim-name->offset '.SPcallbuiltin2))))

(defun x862-ternary-builtin (seg vreg xfer name form1 form2 form3)
  (with-x86-local-vinsn-macros (seg)
    (x862-three-targeted-reg-forms seg form1 (target-arch-case
					      (:x8632 ($ x8632::temp0))
					      (:x8664 ($ x8664::arg_x)))
				   form2 ($ *x862-arg-y*)
				   form3 ($ *x862-arg-z*))
    (x862-fixed-call-builtin seg vreg xfer name (subprim-name->offset '.SPcallbuiltin3))))


(eval-when (:compile-toplevel :execute :load-toplevel)


(defmacro defx862 (name locative arglist &body forms)
  (multiple-value-bind (body decls)
                       (parse-body forms nil t)
    (destructuring-bind (vcode-block dest control &rest other-args) arglist
      (let* ((fun `(nfunction ,name 
                              (lambda (,vcode-block ,dest ,control ,@other-args) ,@decls 
                                      (block ,name (with-x86-local-vinsn-macros (,vcode-block ,dest ,control) ,@body))))))
        `(progn
           (record-source-file ',name 'function)
           (svset *x862-specials* (%ilogand #.operator-id-mask (%nx1-operator ,locative)) ,fun))))))
)
  
(defx862 x862-lambda lambda-list (seg vreg xfer req opt rest keys auxen body p2decls)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((stack-consed-rest nil)
           (next-method-var-scope-info nil)
           (lexprp (if (consp rest) (progn (setq rest (car rest)) t)))
           (rest-var-bits (and rest (nx-var-bits rest)))
           (rest-ignored-p (and rest (not lexprp) (%ilogbitp $vbitignore rest-var-bits)))
           (want-stack-consed-rest (or rest-ignored-p
                                       (and rest (not lexprp) (%ilogbitp $vbitdynamicextent rest-var-bits))))
           (afunc *x862-cur-afunc*)
           (inherited-vars (afunc-inherited-vars afunc))
           (fbits (afunc-bits afunc))
           (methodp (%ilogbitp $fbitmethodp fbits))
           (method-var (if methodp (pop req)))
           (next-method-p (%ilogbitp $fbitnextmethp fbits))
           (allow-other-keys-p (%car keys))
           (hardopt (x862-hard-opt-p opt))
           (lap-p (when (and (consp (%car req)) (eq (%caar req) '&lap))
                    (prog1 (%cdar req) (setq req nil))))
           (num-inh (length inherited-vars))
           (num-req (length req))
           (num-opt (length (%car opt)))
           (no-regs nil)
           (arg-regs nil)
           optsupvloc
           reglocatives
           pregs
           (reserved-lcells nil)
           (*x862-vstack* 0))
      (declare (type (unsigned-byte 16) num-req num-opt num-inh))
      (with-x86-p2-declarations p2decls
        (setq *x862-inhibit-register-allocation*
              (setq no-regs (%ilogbitp $fbitnoregs fbits)))
        (multiple-value-setq (pregs reglocatives) 
          (x862-allocate-global-registers *x862-fcells* *x862-vcells* (afunc-all-vars afunc) no-regs))
        (@ (backend-get-next-label)) ; generic self-reference label, should be label #1
        (! establish-fn)
        (@ (backend-get-next-label))    ; self-call label
        (unless next-method-p
          (setq method-var nil))
        
        (let* ((rev-req (reverse req))
               (rev-fixed (if inherited-vars (reverse (append inherited-vars req)) rev-req))
               (num-fixed (length rev-fixed))
               (rev-opt (reverse (car opt)))
               (max-args (unless (or rest keys) (+ num-fixed num-opt))))
          (if (not (or opt rest keys))
            (setq arg-regs (x862-req-nargs-entry seg rev-fixed))
            (if (and (not (or hardopt rest keys))
                     (<= num-opt *x862-target-num-arg-regs*))
              (setq arg-regs (x862-simple-opt-entry seg rev-opt rev-fixed))
              (progn
                ;; If the minumum acceptable number of args is
                ;; non-zero, ensure that at least that many were
                ;; received.  If there's an upper bound, enforce it.
                
                (cond (rev-fixed
                       (x862-reserve-vstack-lcells num-fixed)
                       (if max-args
                         (! check-min-max-nargs num-fixed max-args)
                         (! check-min-nargs num-fixed)))
                      (max-args
                       (! check-max-nargs max-args)))
                (if (not (or rest keys))
                  (if (<= (+ num-fixed num-opt) *x862-target-num-arg-regs*)
                    (! save-lisp-context-no-stack-args)
                    (! save-lisp-context-variable-arg-count))
                  (! save-lisp-context-variable-arg-count))
                ;; If there were &optional args, initialize their values
                ;; to NIL.  All of the argregs get vpushed as a result of this.
                (when opt
                  (x862-reserve-vstack-lcells num-opt)
                  (if max-args
                    (! push-max-argregs max-args)
                    (! push-argregs))
                  (! default-optionals (+ num-fixed num-opt)))
                (when keys
                  (let* ((keyvect (%car (%cdr (%cdr (%cdr (%cdr keys))))))
                         (flags (the fixnum (logior (the fixnum (if rest 4 0)) 
                                                    (the fixnum (if (or methodp allow-other-keys-p) 1 0)))))
                         (nkeys (length keyvect))
                         (nprev (+ num-fixed num-opt)))
                    (declare (fixnum flags nkeys nprev))
                    (dotimes (i (the fixnum (+ nkeys nkeys)))
                      (x862-new-vstack-lcell :reserved *x862-target-lcell-size* 0 nil))
		    (target-arch-case
		     ;; xxx hack alert (see SPkeyword_bind in x86-spentry32.s)
		     (:x8632
		      (! set-high-halfword *x862-temp1* flags))
		     (:x8664
		      (x862-lri seg *x862-temp1* (ash flags *x862-target-fixnum-shift*))))
                    (unless (= nprev 0)
                      (x862-lri seg *x862-imm0* (ash nprev *x862-target-fixnum-shift*)))
                    (x86-immediate-label keyvect)
                    (if (= 0 nprev)
                      (! simple-keywords)
                      (if (= 0 num-opt)
                        (! keyword-args)
                        (! keyword-bind)))))
                (when rest
                  ;; If any keyword-binding's happened, the key/value
                  ;; pairs have been slid to the top-of-stack for us.
                  ;; There'll be an even number of them (nargs - the
                  ;; "previous" (required/&optional) count.)
                  (if lexprp
                    (x862-lexpr-entry seg num-fixed)
                    (progn
                      (if want-stack-consed-rest
                        (setq stack-consed-rest t))
                      (let* ((nprev (+ num-fixed num-opt))
                             (simple (and (not keys) (= 0 nprev))))
                        (declare (fixnum nprev))
                        (unless simple
                          (x862-lri seg *x862-imm0* (ash nprev *x862-target-fixnum-shift*)))
                        (if stack-consed-rest
                          (if simple
                            (! stack-rest-arg)
                            (if (and (not keys) (= 0 num-opt))
                              (! req-stack-rest-arg)
                              (! stack-cons-rest-arg)))
                          (if simple
                            (! heap-rest-arg)
                            (if (and (not keys) (= 0 num-opt))
                              (! req-heap-rest-arg)
                              (! heap-cons-rest-arg)))))
                      ;; Make an lcell for the &rest arg
                      (x862-reserve-vstack-lcells 1))))
                (when hardopt
                  (x862-reserve-vstack-lcells num-opt)
                  

                  ;; ! opt-supplied-p wants nargs to contain the
                  ;; actual arg-count minus the number of "fixed"
                  ;; (required, inherited) args.

                  (unless (= 0 num-fixed)
                    (! scale-nargs num-fixed))
                  (cond ((= 1 num-opt)
                         (! one-opt-supplied-p))
                        ((= 2 num-opt)
                         (! two-opt-supplied-p))
                        (t
                         (! opt-supplied-p num-opt))))
                (let* ((nwords-vpushed (+ num-fixed 
                                          num-opt 
                                          (if hardopt num-opt 0) 
                                          (if lexprp 0 (if rest 1 0))
                                          (ash (length (%cadr keys)) 1)))
                       (nbytes-vpushed (* nwords-vpushed *x862-target-node-size*)))
                  (declare (fixnum nwords-vpushed nbytes-vpushed))
                  (x862-set-vstack nbytes-vpushed)
                  (setq optsupvloc (- *x862-vstack* (* num-opt *x862-target-node-size*)))))))
          ;; Caller's context is saved; *x862-vstack* is valid.  Might
          ;; still have method-var to worry about.
          (unless (= 0 pregs)
            ;; Save NVRs; load constants into any that get constants.
            (x862-save-nvrs seg pregs)
            (dolist (pair reglocatives)
              (declare (cons pair))
              (let* ((constant (car pair))
                     (reg (cdr pair)))
                (declare (cons constant))
                (rplacd constant reg)
                (! ref-constant reg (x86-immediate-label (car constant))))))
          (when (and (not (or opt rest keys))
                     (<= max-args *x862-target-num-arg-regs*)
                     (not (some #'null arg-regs)))
            (setq *x862-tail-vsp* *x862-vstack*
                  *x862-tail-nargs* max-args)
            (@ (setq *x862-tail-label* (backend-get-next-label))))
          (when method-var
	    (target-arch-case
	     (:x8632
	      (with-node-target () next-method-context
		(! load-next-method-context next-method-context)
		(x862-seq-bind-var seg method-var next-method-context)))
	     (:x8664
	      (x862-seq-bind-var seg method-var x8664::next-method-context)))
	    (when *x862-recorded-symbols*
              (setq next-method-var-scope-info (pop *x862-recorded-symbols*))))

          ;; If any arguments are still in arg_x, arg_y, arg_z, that's
          ;; because they weren't vpushed in a "simple" entry case and
          ;; belong in some NVR.  Put them in their NVRs, so that we
          ;; can handle arbitrary expression evaluation (special
          ;; binding, value-cell consing, etc.) without clobbering the
          ;; argument registers.
          (when arg-regs
            (do* ((vars arg-regs (cdr vars))
                  (arg-reg-numbers (target-arch-case
				    (:x8632 (list *x862-arg-z* *x862-arg-y*))
                                    (:x8664 (list *x862-arg-z* *x862-arg-y* x8664::arg_x))))
                  (arg-reg-num (pop arg-reg-numbers) (pop arg-reg-numbers)))
                 ((null vars))
              (declare (list vars) (fixnum arg-reg-num))
              (let* ((var (car vars)))
                (when var
                  (let* ((reg (x862-assign-register-var var)))
                    (x862-copy-register seg reg arg-reg-num)
                    (setf (var-ea var) reg))))))
          (setq *x862-entry-vsp-saved-p* t)
          (when stack-consed-rest
            (x862-open-undo $undostkblk))
          (setq *x862-entry-vstack* *x862-vstack*)
          (setq reserved-lcells (x862-collect-lcells :reserved))
          (x862-bind-lambda seg reserved-lcells req opt rest keys auxen optsupvloc arg-regs lexprp inherited-vars)
          (when next-method-var-scope-info
            (push next-method-var-scope-info *x862-recorded-symbols*)))
        (when method-var (x862-heap-cons-next-method-var seg method-var))
        (x862-form seg vreg xfer body)
        (x862-close-lambda seg req opt rest keys auxen)
        (dolist (v inherited-vars)
          (x862-close-var seg v))
        (when method-var
          (x862-close-var seg method-var))
        (let* ((bits 0))
          (when (%i> num-inh (ldb $lfbits-numinh -1))
            (setq num-inh (ldb $lfbits-numinh -1)))
          (setq bits (dpb num-inh $lfbits-numinh bits))
          (unless lap-p
            (when (%i> num-req (ldb $lfbits-numreq -1))
              (setq num-req (ldb $lfbits-numreq -1)))
            (setq bits (dpb num-req $lfbits-numreq bits))
            (when (%i> num-opt (ldb $lfbits-numopt -1))
              (setq num-opt (ldb $lfbits-numopt -1)))
            (setq bits (dpb num-opt $lfbits-numopt bits))
            (when hardopt (setq bits (%ilogior (%ilsl $lfbits-optinit-bit 1) bits)))
            (when rest (setq bits (%ilogior (if lexprp (%ilsl $lfbits-restv-bit 1) (%ilsl $lfbits-rest-bit 1)) bits)))
            (when keys (setq bits (%ilogior (%ilsl $lfbits-keys-bit 1) bits)))
            (when allow-other-keys-p (setq bits (%ilogior (%ilsl $lfbits-aok-bit 1) bits)))
            (when (%ilogbitp $fbitnextmethargsp (afunc-bits afunc))
              (if methodp
                (setq bits (%ilogior (%ilsl $lfbits-nextmeth-with-args-bit 1) bits))
                (let ((parent (afunc-parent afunc)))
                  (when parent
                    (setf (afunc-bits parent) (bitset $fbitnextmethargsp (afunc-bits parent)))))))
            (when methodp
              (setq bits (logior (ash 1 $lfbits-method-bit) bits))
              (when next-method-p
                (setq bits (logior (%ilsl $lfbits-nextmeth-bit 1) bits)))))
          bits)))))


(defx862 x862-progn progn (seg vreg xfer forms)
  (declare (list forms))
  (if (null forms)
    (x862-nil seg vreg xfer)
    (loop
      (let* ((form (pop forms)))
        (if forms
          (x862-form seg nil nil form)
          (return (x862-form seg vreg xfer form)))))))



(defx862 x862-prog1 prog1 (seg vreg xfer forms)
  (if (eq (list-length forms) 1)
    (x862-use-operator (%nx1-operator values) seg vreg xfer forms)
    (if (null vreg)
      (x862-use-operator (%nx1-operator progn) seg vreg xfer forms)
      (let* ((float-p (= (hard-regspec-class vreg) hard-reg-class-fpr))
             (crf-p (= (hard-regspec-class vreg) hard-reg-class-crf))
             (node-p (unless (or float-p crf-p)
                       (= (get-regspec-mode vreg) hard-reg-class-gpr-mode-node)))
             (first (pop forms)))
        (if (and node-p
                 (nx-null (car forms))
                 (null (cdr forms)))
          (x862-form seg vreg xfer first)
          (progn
            (x862-push-register seg 
                                (if (or node-p crf-p)
                                  (x862-one-untargeted-reg-form seg first *x862-arg-z*)
                                  (x862-one-targeted-reg-form seg first vreg)))
            (dolist (form forms)
              (x862-form seg nil nil form))
            (if crf-p
              (progn
                (x862-vpop-register seg *x862-arg-z*)
                (<- *x862-arg-z*))
              (x862-pop-register seg vreg))
            (^)))))))

(defx862 x862-free-reference free-reference (seg vreg xfer sym)
  (x862-ref-symbol-value seg vreg xfer sym t))

(defx862 x862-special-ref special-ref (seg vreg xfer sym)
  (x862-ref-symbol-value seg vreg xfer sym t))

(defx862 x862-bound-special-ref bound-special-ref (seg vreg xfer sym)
  (x862-ref-symbol-value seg vreg xfer sym t))

(defx862 x862-%slot-ref %slot-ref (seg vreg xfer instance idx)
  (ensuring-node-target (target (or vreg ($ *x862-arg-z*)))
    (multiple-value-bind (v i)
        (x862-two-untargeted-reg-forms seg instance *x862-arg-y* idx *x862-arg-z*)
      (unless *x862-reckless*
        (! check-misc-bound i v))
      (with-node-temps (v) (temp)
        (! %slot-ref temp v i)
        (x862-copy-register seg target temp))))
  (^))

(pushnew (%nx1-operator %svref) *x862-operator-supports-push*)
(defx862 x862-%svref %svref (seg vreg xfer vector index)
  (x862-vref seg vreg xfer :simple-vector vector index nil))

(pushnew (%nx1-operator svref) *x862-operator-supports-push*)
(defx862 x862-svref svref (seg vreg xfer vector index)
  (x862-vref seg vreg xfer :simple-vector vector index (unless *x862-reckless* (nx-lookup-target-uvector-subtag :simple-vector))))

;;; It'd be nice if this didn't box the result.  Worse things happen ...
;;;  Once there's a robust mechanism, adding a CHARCODE storage class shouldn't be hard.
(defx862 x862-%sbchar %sbchar (seg vreg xfer string index)
  (x862-vref seg vreg xfer :simple-string string index (unless *x862-reckless* (nx-lookup-target-uvector-subtag :simple-string))))


(defx862 x862-%svset %svset (seg vreg xfer vector index value)
  (x862-vset seg vreg xfer :simple-vector vector index value nil))

(defx862 x862-svset svset (seg vreg xfer vector index value)
   (x862-vset seg vreg xfer :simple-vector vector  index value (nx-lookup-target-uvector-subtag :simple-vector)))

(defx862 x862-typed-form typed-form (seg vreg xfer typespec form &optional check)
  (if check
    (x862-typechecked-form seg vreg xfer typespec form)
    (x862-form seg vreg xfer form)))

(defx862 x862-%primitive %primitive (seg vreg xfer &rest ignore)
  (declare (ignore seg vreg xfer ignore))
  (compiler-bug "You're probably losing big: using %primitive ..."))

(defx862 x862-consp consp (seg vreg xfer cc form)
  (if (null vreg)
    (x862-form seg vreg xfer form)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
      (! set-z-flag-if-consp (x862-one-untargeted-reg-form seg form *x862-arg-z*))
      (regspec-crf-gpr-case
       (vreg dest)
       (^ cr-bit true-p)
       (progn
	 (ensuring-node-target (target dest)
	   (if (not true-p)
	     (setq cr-bit (logxor 1 cr-bit)))
	   (! cr-bit->boolean target cr-bit))
	 (^))))))
      
(defx862 x862-cons cons (seg vreg xfer y z)
  (if (null vreg)
    (progn
      (x862-form seg nil nil y)
      (x862-form seg nil xfer z))
    (multiple-value-bind (yreg zreg) (x862-two-untargeted-reg-forms seg y *x862-arg-y* z *x862-arg-z*)
      (ensuring-node-target (target vreg)
        (! cons target yreg zreg))
      (^))))



(defx862 x862-%rplaca %rplaca (seg vreg xfer ptr val)
  (x862-modify-cons seg vreg xfer ptr val nil nil t))

(defx862 x862-%rplacd %rplacd (seg vreg xfer ptr val)
  (x862-modify-cons seg vreg xfer ptr val nil t t))

(defx862 x862-rplaca rplaca (seg vreg xfer ptr val)
  (x862-modify-cons seg vreg xfer ptr val t nil t))

(defx862 x862-set-car set-car (seg vreg xfer ptr val)
  (x862-modify-cons seg vreg xfer ptr val t nil nil))

(defx862 x862-rplacd rplacd (seg vreg xfer ptr val)
  (x862-modify-cons seg vreg xfer ptr val t t t))

(defx862 x862-set-cdr set-cdr (seg vreg xfer ptr val)
  (x862-modify-cons seg vreg xfer ptr val t t nil))

(pushnew (%nx1-operator %car) *x862-operator-supports-push*)
(defx862 x862-%car %car (seg vreg xfer form)
  (x862-reference-list seg vreg xfer form nil nil))

(pushnew (%nx1-operator %cdr) *x862-operator-supports-push*)
(defx862 x862-%cdr %cdr (seg vreg xfer form)
  (x862-reference-list seg vreg xfer form nil t))

(pushnew (%nx1-operator car) *x862-operator-supports-push*)
(defx862 x862-car car (seg vreg xfer form)
  (x862-reference-list seg vreg xfer form t nil))

(pushnew (%nx1-operator cdr) *x862-operator-supports-push*)
(defx862 x862-cdr cdr (seg vreg xfer form)
  (x862-reference-list seg vreg xfer form t t))

(defx862 x862-vector vector (seg vreg xfer arglist)
  (x862-allocate-initialized-gvector seg vreg xfer
                                     (nx-lookup-target-uvector-subtag
                                      :simple-vector) arglist))

(defx862 x862-%gvector %gvector (seg vreg xfer arglist)
  (let* ((all-on-stack (append (car arglist) (reverse (cadr arglist))))
         (subtag-form (car all-on-stack))
         (subtag (acode-fixnum-form-p subtag-form)))
    (if (null vreg)
      (dolist (form all-on-stack (^)) (x862-form seg nil nil form))
      (if (null subtag)
        (progn                            ; Vpush everything and call subprim
          (let* ((*x862-vstack* *x862-vstack*)
                 (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
            (x862-set-nargs seg (x862-formlist seg all-on-stack nil))
            (! gvector))
          (<- *x862-arg-z*)
          (^))
        (x862-allocate-initialized-gvector seg vreg xfer subtag (cdr all-on-stack))))))

;;; Should be less eager to box result
(defx862 x862-%char-code %char-code (seg vreg xfer c)
  (x862-extract-charcode seg vreg xfer c nil))

(defx862 x862-char-code char-code (seg vreg xfer c)
  (x862-extract-charcode seg vreg xfer c (not (x862-form-typep c 'character))))

(defx862 x862-%ilogior2 %ilogior2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (x862-use-operator (%nx1-operator fixnum) seg vreg xfer (logior fix1 fix2)))
    (let* ((fixval (or fix1 fix2))
           (fiximm (if fixval (<= (integer-length fixval)
                                  (- 31 *x862-target-fixnum-shift*))))
           (otherform (when fiximm (if fix1 form2 form1))))
      (if otherform
        (if (null vreg)
          (x862-form seg nil xfer otherform)
          (ensuring-node-target (target vreg)
            (x862-one-targeted-reg-form seg otherform target)
            (! %logior-c target target (ash fixval *x862-target-fixnum-shift*))))
         (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg form1 *x862-arg-y* form2 *x862-arg-z*)
            (if vreg (ensuring-node-target (target vreg) (! %logior2 target r1 r2)))))
      (^))))

;;; in a lot of (typical ?) cases, it might be possible to use a
;;; rotate-and-mask instead of andi./andis.

(defx862 x862-%ilogand2 %ilogand2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (x862-use-operator (%nx1-operator fixnum) seg vreg xfer (logand fix1 fix2)))
    (let* ((fixval (or fix1 fix2))
           (fiximm (if fixval (<= (integer-length fixval)
                                  (- 31 *x862-target-fixnum-shift*))))
           (otherform (when fiximm (if fix1 form2 form1))))
      (if otherform
        (if (null vreg)
          (x862-form seg nil xfer otherform)
          (ensuring-node-target (target vreg)
            (x862-one-targeted-reg-form seg otherform target)
            (! %logand-c target target (ash fixval *x862-target-fixnum-shift*))))
         (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg form1 *x862-arg-y* form2 *x862-arg-z*)
            (if vreg (ensuring-node-target (target vreg) (! %logand2 target r1 r2)))))
      (^))))

(defx862 x862-%ilogxor2 %ilogxor2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (x862-use-operator (%nx1-operator fixnum) seg vreg xfer (logxor fix1 fix2)))
    (let* ((fixval (or fix1 fix2))
           (fiximm (if fixval (<= (integer-length fixval)
                                  (- 31 *x862-target-fixnum-shift*))))
           (otherform (when fiximm (if fix1 form2 form1))))
      (if otherform
        (if (null vreg)
          (x862-form seg nil xfer otherform)
          (ensuring-node-target (target vreg)
            (x862-one-targeted-reg-form seg otherform target)
            (! %logxor-c target target (ash fixval *x862-target-fixnum-shift*))))
         (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg form1 *x862-arg-y* form2 *x862-arg-z*)
            (if vreg (ensuring-node-target (target vreg) (! %logxor2 target r1 r2)))))
      (^))))

(defx862 x862-%ineg %ineg (seg vreg xfer n)
  (if (null vreg)
    (x862-form seg vreg xfer n)
    (progn
      (ensuring-node-target (target vreg)
        (x862-one-targeted-reg-form seg n target)
        (! negate-fixnum target)
        (x862-check-fixnum-overflow seg target))
      (^ ))))

(defx862 x862-%%ineg %%ineg (seg vreg xfer n)
  (if (null vreg)
    (x862-form seg vreg xfer n)
    (progn
      (ensuring-node-target (target vreg)
        (x862-one-targeted-reg-form seg n target)
        (when vreg
          (! negate-fixnum target)))
      (^))))

(defx862 x862-characterp characterp (seg vreg xfer cc form)
  (x862-char-p seg vreg xfer cc form))

(pushnew (%nx1-operator struct-ref) *x862-operator-supports-push*)
(defx862 x862-struct-ref struct-ref (seg vreg xfer struct offset)
  (x862-vref seg vreg xfer :struct struct offset (unless *x862-reckless* (nx-lookup-target-uvector-subtag :struct))))

(defx862 x862-struct-set struct-set (seg vreg xfer struct offset value)
  (x862-vset seg vreg xfer :struct struct offset value (unless *x862-reckless* (nx-lookup-target-uvector-subtag :struct))))

(defx862 x862-istruct-typep istruct-typep (seg vreg xfer cc form type)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
    (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg form *x862-arg-y* type *x862-arg-z*)
      (! set-z-flag-if-istruct-typep r1 r2)
      (regspec-crf-gpr-case 
       (vreg dest)
       (^ cr-bit true-p)
       (progn
         (ensuring-node-target (target dest)
           (if (not true-p)
             (setq cr-bit (logxor 1 cr-bit)))
           (! cr-bit->boolean target cr-bit))
         (^))))))


(pushnew (%nx1-operator lisptag) *x862-operator-supports-u8-target*)
(defx862 x862-lisptag lisptag (seg vreg xfer node)
  (if (null vreg)
    (x862-form seg vreg xfer node)
    (progn
      (unboxed-other-case (vreg :u8)
        (! extract-tag vreg (x862-one-untargeted-reg-form seg node *x862-arg-z*))
        (ensuring-node-target (target vreg) 
         (! extract-tag-fixnum target (x862-one-untargeted-reg-form seg node *x862-arg-z*))))
      (^))))

(pushnew (%nx1-operator fulltag) *x862-operator-supports-u8-target*)
(defx862 x862-fulltag fulltag (seg vreg xfer node)
  (if (null vreg)
    (x862-form seg vreg xfer node)
    (progn
      (unboxed-other-case (vreg :u8)
        (! extract-fulltag vreg (x862-one-untargeted-reg-form seg node *x862-arg-z*))
        (ensuring-node-target (target vreg) 
          (! extract-fulltag-fixnum target (x862-one-untargeted-reg-form seg node *x862-arg-z*))))
      (^))))

(pushnew (%nx1-operator typecode) *x862-operator-supports-u8-target*)
(defx862 x862-typecode typecode (seg vreg xfer node)
  (if (null vreg)
    (x862-form seg vreg xfer node)
    (progn
      (unboxed-other-case (vreg :u8)
         (! extract-typecode vreg (x862-one-untargeted-reg-form seg node *x862-arg-z*))
         (let* ((reg (x862-one-untargeted-reg-form seg node (if (eq (hard-regspec-value vreg) *x862-arg-z*) 
                                                              *x862-arg-y* *x862-arg-z*))))
           (ensuring-node-target (target vreg) 
             (! extract-typecode-fixnum target reg ))))
      (^))))

(defx862 x862-setq-special setq-special (seg vreg xfer sym val)
  (let* ((symreg ($ *x862-arg-y*))
         (valreg ($ *x862-arg-z*)))
    (x862-one-targeted-reg-form seg val valreg)
    (x862-store-immediate seg (x862-symbol-value-cell sym) symreg)
    (! setq-special symreg valreg)
    (<- valreg))
  (^))


(defx862 x862-local-go local-go (seg vreg xfer tag)
  (declare (ignorable xfer))
  (let* ((curstack (x862-encode-stack))
         (label (cadr tag))
         (deststack (caddr tag)))
    (if (not (x862-equal-encodings-p curstack deststack))
      (multiple-value-bind (catch cstack vstack)
                           (x862-decode-stack deststack)
        (x862-unwind-stack seg nil catch cstack vstack)))
    (-> label)
    (x862-unreachable-store vreg)))

(defx862 x862-local-block local-block (seg vreg xfer blocktag body)
  (let* ((curstack (x862-encode-stack))
         (compound (x862-cd-compound-p xfer))
         (mvpass-p (x862-mvpass-p xfer))
         (need-label (if xfer (or compound mvpass-p) t))
         end-of-block
         last-cd
         (dest (if (backend-crf-p vreg) *x862-arg-z* vreg)))
    (if need-label
      (setq end-of-block (backend-get-next-label)))
    (setq last-cd (if need-label (%ilogior2 (if mvpass-p $backend-mvpass-mask 0) end-of-block) xfer))
    (%rplaca blocktag (cons (cons dest last-cd) curstack))
    (if mvpass-p
      (x862-multiple-value-body seg body)
      (x862-form seg dest (if xfer last-cd) body))
    (when need-label
      (@ end-of-block)
      (if compound
        (<- dest))
      (x862-branch seg (logand (lognot $backend-mvpass-mask) (or xfer 0))))))

(defx862 x862-%izerop %izerop (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
    (x862-test-%izerop seg vreg xfer form cr-bit true-p)))


(defx862 x862-uvsize uvsize (seg vreg xfer v)
  (let* ((misc-reg (x862-one-untargeted-reg-form seg v *x862-arg-z*)))
    (unless *x862-reckless* (! trap-unless-uvector misc-reg))
    (if vreg 
      (ensuring-node-target (target vreg)
        (! misc-element-count-fixnum target misc-reg)))
    (^)))

(defx862 x862-%ilsl %ilsl (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (x862-form seg nil nil form1)
      (x862-form seg nil xfer form2))
    (let* ((const (acode-fixnum-form-p form1))
           (max (target-arch-case (:x8632 31) (:x8664 63))))
      (ensuring-node-target (target vreg)
        (if const
          (let* ((src (x862-one-untargeted-reg-form seg form2 *x862-arg-z*)))
            (if (<= const max)
              (! %ilsl-c target const src)
              (!  lri target 0)))
          (multiple-value-bind (count src) (x862-two-untargeted-reg-forms seg form1 *x862-arg-y* form2 *x862-arg-z*)
            (! %ilsl target count src))))
      (^))))

(defx862 x862-endp endp (seg vreg xfer cc form)
  (let* ((formreg (x862-one-untargeted-reg-form seg form *x862-arg-z*)))
    (! trap-unless-list formreg)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
      (x862-compare-register-to-nil seg vreg xfer formreg  cr-bit true-p))))



(defx862 x862-%code-char %code-char (seg vreg xfer c)
  (if (null vreg)
    (x862-form seg nil xfer c)
    (progn
      (ensuring-node-target (target vreg)
        (with-imm-target () (dest :u8)
          (! u32->char target (x862-one-untargeted-reg-form seg c dest))))
      (^))))

(defx862 x862-%schar %schar (seg vreg xfer str idx)
  (multiple-value-bind (src unscaled-idx)
      (x862-two-untargeted-reg-forms seg str *x862-arg-y* idx *x862-arg-z*)
    (if vreg
      (ensuring-node-target (target vreg)
        (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
          (256 (! %schar8 target src unscaled-idx))
          (t (! %schar32 target src unscaled-idx)))))
    (^)))

(defx862 x862-%set-schar %set-schar (seg vreg xfer str idx char)
  (multiple-value-bind (src unscaled-idx char)
      (x862-three-untargeted-reg-forms seg
                                       str (target-arch-case
					    (:x8632 x8632::temp0)
					    (:x8664 x8664::arg_x))
                                       idx *x862-arg-y*
                                       char *x862-arg-z*)
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256 (! %set-schar8 src unscaled-idx char))
      (t (! %set-schar32 src unscaled-idx char)))
    (when vreg (<- char)) 
    (^)))

(defx862 x862-%set-scharcode %set-scharcode (seg vreg xfer str idx char)
  (multiple-value-bind (src unscaled-idx char)
      (x862-three-untargeted-reg-forms seg str (target-arch-case
						(:x8632 x8632::temp0)
						(:x8664 x8664::arg_x))
				       idx *x862-arg-y*
                                       char *x862-arg-z*)
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256
       (! %set-scharcode8 src unscaled-idx char))
      (t 
       (! %set-scharcode32 src unscaled-idx char)))
    (when vreg (<- char)) 
    (^)))

(defx862 x862-%scharcode %scharcode (seg vreg xfer str idx)
  (multiple-value-bind (src unscaled-idx)
      (x862-two-untargeted-reg-forms seg str *x862-arg-y* idx *x862-arg-z*)
    (if vreg
      (ensuring-node-target (target vreg)
        (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
          (256 (! %scharcode8 target src unscaled-idx))
          (t (! %scharcode32 target src unscaled-idx)))))
    (^)))

      

(defx862 x862-code-char code-char (seg vreg xfer c)
  (let* ((reg (x862-one-untargeted-reg-form seg c *x862-arg-z*)))
    ;; Typecheck even if result unused.
    (! require-char-code reg)
    (if vreg
      (ensuring-node-target (target vreg)
        (! fixnum->char target reg)))
    (^)))

(defx862 x862-%valid-code-char %valid-code-char (seg vreg xfer c)
  (let* ((reg (x862-one-untargeted-reg-form seg c *x862-arg-z*)))
    (when *x862-full-safety* (! require-char-code reg))
    (if vreg
      (ensuring-node-target (target vreg)
        (! code-char->char target reg)))
    (^)))

(defun x862-eq-test (seg vreg xfer cc form1 form2)
  (with-x86-local-vinsn-macros (seg)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
      (let* ((f1 (acode-unwrapped-form form1))
             (f2 (acode-unwrapped-form form2)))
        (cond ((or (eq f1 *nx-nil*)
                   (eq f1 *nx-t*)
                   (and (acode-p f1)
                        (eq (acode-operator f1) (%nx1-operator immediate))))
               (x862-compare-register-to-constant seg vreg xfer (x862-one-untargeted-reg-form seg form2 ($ *x862-arg-z*)) cr-bit true-p f1))
              ((or (eq f2 *nx-nil*)
                   (eq f2 *nx-t*)
                   (and (acode-p f2)
                        (eq (acode-operator f2) (%nx1-operator immediate))))
               (x862-compare-register-to-constant seg vreg xfer
                                                  (x862-one-untargeted-reg-form seg form1 ($ *x862-arg-z*))
                                                  cr-bit true-p f2))
              (t (x862-compare seg vreg xfer form1 form2 cr-bit true-p)))))))

(defx862 x862-eq eq (seg vreg xfer cc form1 form2)
  (x862-eq-test seg vreg xfer cc form1 form2))

(defx862 x862-neq neq (seg vreg xfer cc form1 form2)
  (x862-eq-test seg vreg xfer cc form1 form2))

(defx862 x862-numcmp numcmp (seg vreg xfer cc form1 form2)
  (let* ((name (ecase (cadr cc)
                 (:eq '=-2)
                 (:ne '/=-2)
                 (:lt '<-2)
                 (:le '<=-2)
                 (:gt '>-2)
                 (:ge '>=-2))))
    (if (or (x862-explicit-non-fixnum-type-p form1)
            (x862-explicit-non-fixnum-type-p form2))
      (x862-binary-builtin seg vreg xfer name form1 form2)
      (let* ((fix1 (acode-fixnum-form-p form1))
             (fix2 (acode-fixnum-form-p form2)))
        (if (and fix1 fix2)
          (if (funcall name fix1 fix2)
            (x862-t seg vreg xfer)
            (x862-nil seg vreg xfer))
          (x862-inline-numcmp seg vreg xfer cc name form1 form2))))))

(defun x862-inline-numcmp (seg vreg xfer cc name form1 form2)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2))
           (fixval (or fix1 fix2))
           (fiximm (if fixval (<= (integer-length fixval)
                                  (- 31 *x862-target-fixnum-shift*))))
           (otherform (when fiximm (if fix1 form2 form1)))
           (out-of-line (backend-get-next-label))
           (done (backend-get-next-label)))
      (if otherform
        (x862-one-targeted-reg-form seg otherform ($ *x862-arg-y*))
        (x862-two-targeted-reg-forms seg form1 ($ *x862-arg-y*) form2 ($ *x862-arg-z*)))
      (if otherform
        (unless (acode-fixnum-form-p otherform)
          (! branch-unless-arg-fixnum ($ *x862-arg-y*) (aref *backend-labels* out-of-line)))
        (if (acode-fixnum-form-p form1)
          (! branch-unless-arg-fixnum ($ *x862-arg-z*) (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (! branch-unless-arg-fixnum ($ *x862-arg-y*) (aref *backend-labels* out-of-line))  
            (! branch-unless-both-args-fixnums ($ *x862-arg-y*) ($ *x862-arg-z*) (aref *backend-labels* out-of-line)))))
      (if otherform
        (if (zerop fixval)
          (! compare-reg-to-zero ($ *x862-arg-y*))
          (! compare-s32-constant ($ *x862-arg-y*) (ash fixval *x862-target-fixnum-shift*)))
        (! compare ($ *x862-arg-y*) ($ *x862-arg-z*)))
      (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
        (when otherform
          (unless (or (and fix2 (not fix1)) (eq cr-bit x86::x86-e-bits))
            (setq cr-bit (x862-reverse-cr-bit cr-bit))))
        (if (not true-p)
          (setq cr-bit (logxor 1 cr-bit)))
        (! cr-bit->boolean ($ *x862-arg-z*) cr-bit)
        (-> done)
        (@ out-of-line)
        (when otherform
          (x862-lri seg ($ *x862-arg-z*) (ash fixval *x862-target-fixnum-shift*))
          (unless (or fix2 (eq cr-bit x86::x86-e-bits))
            (! xchg-registers ($ *x862-arg-z*) ($ *x862-arg-y*))))
        (let* ((index (arch::builtin-function-name-offset name))
               (idx-subprim (x862-builtin-index-subprim index)))
          (! call-subprim-2 ($ *x862-arg-z*) idx-subprim ($ *x862-arg-y*) ($ *x862-arg-z*)))
        (@ done)
        (<- ($ *x862-arg-z*))
        (^)))))
         
        
    

(defx862 x862-%word-to-int %word-to-int (seg vreg xfer form)
  (if (null vreg)
    (x862-form seg nil xfer form)
    (progn
      (ensuring-node-target (target vreg)
        (! sign-extend-halfword target (x862-one-untargeted-reg-form seg form *x862-arg-z*)))
      (^))))

(defx862 x862-multiple-value-list multiple-value-list (seg vreg xfer form)
  (x862-multiple-value-body seg form)
  (! list)
  (when vreg
    (<- *x862-arg-z*))
  (^))

(defx862 x862-immform immediate (seg vreg xfer form)
  (x862-immediate seg vreg xfer form))

(pushnew (%nx1-operator lexical-reference) *x862-operator-supports-push*)
(defx862 x862-lexical-reference lexical-reference (seg vreg xfer varnode)
  (let* ((ea-or-form (var-ea varnode)))
    (if (and (acode-punted-var-p varnode) (not (fixnump ea-or-form)))
      (if (or (not (eq vreg :push))
              (x862-acode-operator-supports-push ea-or-form))
        (x862-form seg vreg xfer ea-or-form)
        (ensuring-node-target (target vreg)
          (x862-form seg target xfer ea-or-form)
          (! vpush-register target)))
      (let* ((cell (x862-lookup-var-cell varnode)))
        (if (and cell (typep cell 'lcell))
          (if (x862-ensure-lcell-offset cell (logand ea-or-form #xffff))
            (and nil (format t "~& could use cell ~s for var ~s" cell (var-name varnode)))
            (if (logbitp x862-debug-verbose-bit *x862-debug-mask*)
              (compiler-bug "wrong ea for lcell for var ~s: got ~d, expected ~d" 
                            (var-name varnode) (calc-lcell-offset cell) (logand ea-or-form #xffff))))
          (if (not cell)
            (when (memory-spec-p ea-or-form)
              (if (logbitp x862-debug-verbose-bit *x862-debug-mask*)
                (compiler-bug "no lcell for ~s." (var-name varnode))))))
        (unless (or (typep ea-or-form 'lreg) (fixnump ea-or-form))
          (compiler-bug "bogus ref to var ~s (~s) : ~s " varnode (var-name varnode) ea-or-form))
        (x862-do-lexical-reference seg vreg ea-or-form)
        (^)))))

(defx862 x862-setq-lexical setq-lexical (seg vreg xfer varspec form)
  (let* ((ea (var-ea varspec)))
    ;(unless (fixnump ea) compiler-bug "setq lexical is losing BIG"))
    (let* ((valreg (x862-one-untargeted-reg-form seg form (if (and (register-spec-p ea) 
                                                                   (or (null vreg) (eq ea vreg)))
                                                            ea
                                                            *x862-arg-z*))))
      (x862-do-lexical-setq seg vreg ea valreg))
    (^)))

(pushnew (%nx1-operator fixnum) *x862-operator-supports-push*)
(defx862 x862-fixnum fixnum (seg vreg xfer value)
  (if (null vreg)
    (^)
    (if (eq vreg :push)
      (let* ((boxed (ash value *x862-target-fixnum-shift*)))
        (if (typep boxed '(signed-byte 32))
          (! vpush-fixnum boxed)
          (with-node-target () target
            (x862-absolute-natural seg target nil boxed)
            (! vpush-register target)))
        (^))
      (let* ((class (hard-regspec-class vreg))
           (mode (get-regspec-mode vreg))
           (unboxed (if (= class hard-reg-class-gpr)
                      (not (or (= hard-reg-class-gpr-mode-node mode)
                               (= hard-reg-class-gpr-mode-address mode))))))
      (if unboxed
        (x862-absolute-natural seg vreg xfer value)
        (if (= class hard-reg-class-crf)
          (progn
            ;compiler-bug "Would have clobbered a GPR!")
            (x862-branch seg (x862-cd-true xfer)))
          (progn
            (ensuring-node-target (target vreg)
              (x862-absolute-natural seg target nil (ash value *x862-target-fixnum-shift*)))
            (^))))))))

(defx862 x862-%ilogbitp %ilogbitp (seg vreg xfer cc bitnum form)
  (if (null vreg)
    (progn
      (x862-form seg nil nil bitnum)
      (x862-form seg vreg xfer form))
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
      (unless (eq cr-bit x86::x86-e-bits)
        (bug "bad cr-bit"))
      (setq cr-bit x86::x86-b-bits true-p (not true-p))
      (let* ((fixbit (acode-fixnum-form-p bitnum)))
        (if fixbit
          (let* ((reg (x862-one-untargeted-reg-form seg form *x862-arg-z*))
                 (x86-bit (min (+ fixbit *x862-target-fixnum-shift*) (1- *x862-target-bits-in-word*))))
            (! set-c-flag-if-constant-logbitp x86-bit reg))
          (multiple-value-bind (rbit rform) (x862-two-untargeted-reg-forms seg bitnum *x862-arg-y* form *x862-arg-z*)
            (! set-c-flag-if-variable-logbitp rbit rform)))
        (regspec-crf-gpr-case 
         (vreg dest)
         (^ cr-bit true-p)
         (progn
           (ensuring-node-target (target dest)
             (if (not true-p)
               (setq cr-bit (logxor 1 cr-bit)))
             (! cr-bit->boolean target cr-bit))
           (^)))))))


(defx862 x862-uvref uvref (seg vreg xfer vector index)
  (x862-two-targeted-reg-forms seg vector ($ *x862-arg-y*) index ($ *x862-arg-z*))
  (! misc-ref)
  (<- ($ *x862-arg-z*))
  (^))

(defx862 x862-uvset uvset (seg vreg xfer vector index value)
  (x862-three-targeted-reg-forms seg
				 vector (target-arch-case
					 (:x8632 ($ x8632::temp0))
					 (:x8664 ($ x8664::arg_x)))
				 index ($ *x862-arg-y*)
				 value ($ *x862-arg-z*))
  (! misc-set)
  (<- ($ *x862-arg-z*))
  (^))

(defx862 x862-%decls-body %decls-body (seg vreg xfer form p2decls)
  (with-x86-p2-declarations p2decls
    (x862-form seg vreg xfer form)))



(defx862 x862-%err-disp %err-disp (seg vreg xfer arglist)
  (let* ((*x862-vstack* *x862-vstack*))
    (x862-set-nargs seg (x862-arglist seg arglist))
    (! ksignalerr))
  (x862-nil seg vreg xfer))


(defx862 x862-local-tagbody local-tagbody (seg vreg xfer taglist body)
  (let* ((encstack (x862-encode-stack))
         (tagop (%nx1-operator tag-label)))
    (dolist (tag taglist)
      (rplacd tag (cons (backend-get-next-label) (cons encstack (cadr (cddr (cddr tag)))))))
    (dolist (form body)
      (if (eq (acode-operator form) tagop)
        (let ((tag (cddr form)))
          (when (cddr tag) (! align-loop-head))
          (@ (car tag)))
        (x862-form seg nil nil form)))
    (x862-nil seg vreg xfer)))

(defx862 x862-call call (seg vreg xfer fn arglist &optional spread-p)
  (when (and (null vreg)
             (acode-p fn)
             (eq (acode-operator fn) (%nx1-operator immediate)))
    (let* ((name (cadr fn)))
      (when (memq name *warn-if-function-result-ignored*)
        (p2-whine *x862-cur-afunc*  :result-ignored name))))
  (x862-call-fn seg vreg xfer fn arglist spread-p))

(defx862 x862-self-call self-call (seg vreg xfer arglist &optional spread-p)
  (setq arglist (x862-augment-arglist *x862-cur-afunc* arglist (if spread-p 1 *x862-target-num-arg-regs*)))
  (x862-call-fn seg vreg xfer -2 arglist spread-p))


(defx862 x862-lexical-function-call lexical-function-call (seg vreg xfer afunc arglist &optional spread-p)
  (x862-call-fn seg vreg xfer (list (%nx1-operator simple-function) afunc)
                (x862-augment-arglist afunc arglist (if spread-p 1 *x862-target-num-arg-regs*))
                spread-p))

(defx862 x862-builtin-call builtin-call (seg vreg xfer index arglist)
  (let* ((nargs (x862-arglist seg arglist))
         (tail-p (and (x862-tailcallok xfer) (<= nargs *x862-target-num-arg-regs*)))
         (idx (acode-fixnum-form-p index))
         (idx-subprim (x862-builtin-index-subprim idx))
         (subprim
          (or idx-subprim
              (case nargs
                (0 (subprim-name->offset '.SPcallbuiltin0))
                (1 (subprim-name->offset '.SPcallbuiltin1))
                (2 (subprim-name->offset '.SPcallbuiltin2))
                (3 (subprim-name->offset '.SPcallbuiltin3))
                (t (subprim-name->offset '.SPcallbuiltin))))))
    (when tail-p
      (x862-restore-nvrs seg *x862-register-restore-ea* *x862-register-restore-count*)
      (x862-restore-full-lisp-context seg))
    (unless idx-subprim
      (! lri *x862-imm0* (ash idx *x862-target-fixnum-shift*))
      (when (eql subprim (subprim-name->offset '.SPcallbuiltin))
        (x862-set-nargs seg nargs)))
    (if tail-p
      (! jump-subprim subprim)
      (progn
        (! call-subprim subprim)
        (<- *x862-arg-z*)
        (^)))))
      

(defx862 x862-if if (seg vreg xfer testform true false &aux test-val)
  (if (setq test-val (nx-constant-form-p (acode-unwrapped-form-value testform)))
    (x862-form seg vreg xfer (if (nx-null test-val) false true))
    (let* ((cstack *x862-cstack*)
           (vstack *x862-vstack*)
           (top-lcell *x862-top-vstack-lcell*)
           (entry-stack (x862-encode-stack))
           (true-stack nil)
           (false-stack nil)
           (true-cleanup-label nil)
           (same-stack-effects nil)
           (true-is-goto (x862-go-label true))
           (false-is-goto (and (not true-is-goto) (x862-go-label false)))
           (endlabel (backend-get-next-label))
           (falselabel (backend-get-next-label))
           (need-else (unless false-is-goto (or (not (nx-null false)) (x862-for-value-p vreg))))
           (both-single-valued (and (not *x862-open-code-inline*)
                                    (eq xfer $backend-return)
                                    (x862-for-value-p vreg)
                                    need-else
                                    (x862-single-valued-form-p true) 
                                    (x862-single-valued-form-p false))))
      (if (eq 0 xfer) 
        (setq xfer nil))
      (if both-single-valued            ; it's implied that we're returning
        (let* ((result *x862-arg-z*))
          (let ((merge-else-branch-label (if (nx-null false) (x862-find-nilret-label))))
            (x862-conditional-form seg (x862-make-compound-cd 0 falselabel) testform)
            (x862-form seg result endlabel true)
            (if (and merge-else-branch-label (neq -1 (aref *backend-labels* merge-else-branch-label)))
              (backend-copy-label merge-else-branch-label falselabel)
              (progn
                (@ falselabel)
                (if (nx-null false) (@ (x862-record-nilret-label)))
                (x862-form seg result nil false)))
            (@ endlabel)
            (<- result)
            (^)))
        (progn
          (if (and need-else (x862-mvpass-p xfer))
            (setq true-cleanup-label (backend-get-next-label)))         
          (x862-conditional-form 
           seg
           (x862-make-compound-cd 
            (or true-is-goto 0)
            (or false-is-goto 
                (if need-else 
                  (if true-is-goto 0 falselabel) 
                  (if true-is-goto xfer (x862-cd-merge xfer falselabel))))) 
           testform)  
          (if true-is-goto
            (x862-unreachable-store)
            (if true-cleanup-label
              (progn
                (x862-open-undo $undomvexpect)
                (x862-form seg vreg (logior $backend-mvpass-mask true-cleanup-label) true))
              (x862-form seg vreg (if need-else (x862-cd-merge xfer endlabel) xfer) true)))
          (setq true-stack (x862-encode-stack))
          (setq *x862-cstack* cstack)
          (x862-set-vstack vstack)
          (setq *x862-top-vstack-lcell* top-lcell)
          (if false-is-goto (x862-unreachable-store))
          (let ((merge-else-branch-label (if (and (nx-null false) (eq xfer $backend-return)) (x862-find-nilret-label))))
            (if (and merge-else-branch-label (neq -1 (aref *backend-labels* merge-else-branch-label)))
              (backend-copy-label merge-else-branch-label falselabel)
              (progn
                (@ falselabel)
                (when need-else
                  (if true-cleanup-label
                    (x862-mvpass seg false)
                    (x862-form seg vreg xfer false))
                  (setq false-stack (x862-encode-stack))))))
          (when true-cleanup-label
            (if (setq same-stack-effects (x862-equal-encodings-p true-stack false-stack)) ; can share cleanup code
              (@ true-cleanup-label))
            (let* ((*x862-returning-values* :pass))
              (x862-nlexit seg xfer 1)
              (x862-branch seg (if (and xfer (neq xfer $backend-mvpass-mask)) xfer (if (not same-stack-effects) endlabel))))
            (unless same-stack-effects
              (@ true-cleanup-label)
              (multiple-value-setq (true *x862-cstack* *x862-vstack* *x862-top-vstack-lcell*)
                (x862-decode-stack true-stack))
              (let* ((*x862-returning-values* :pass))
                (x862-nlexit seg xfer 1)
                (^)))
            (x862-close-undo)
            (multiple-value-setq (*x862-undo-count* *x862-cstack* *x862-vstack* *x862-top-vstack-lcell*) 
              (x862-decode-stack entry-stack)))
          (@ endlabel))))))

(defx862 x862-or or (seg vreg xfer forms)
  (let* ((mvpass (x862-mvpass-p xfer))
         (tag1 (backend-get-next-label))
         (tag2 (backend-get-next-label))
         (vstack *x862-vstack*)
         (cstack *x862-cstack*)
         (dest (if (backend-crf-p vreg) vreg (if vreg *x862-arg-z* (available-crf-temp *available-backend-crf-temps*))))
         (cd1 (x862-make-compound-cd 
               (if (eq dest *x862-arg-z*) tag1 (x862-cd-merge (x862-cd-true xfer) tag1)) 0)))
    (while (cdr forms)
      (x862-form seg dest (if (eq dest *x862-arg-z*) nil cd1) (car forms))
      (when (eq dest *x862-arg-z*)
        (with-crf-target () val-crf
          (x862-copy-register seg val-crf dest)
          (x862-branch seg cd1)))
      (setq forms (%cdr forms)))
    (if mvpass
      (progn (x862-multiple-value-body seg (car forms)) 
             (let* ((*x862-returning-values* t)) (x862-branch seg (x862-cd-merge xfer tag2))))
      (x862-form seg  vreg (if (eq dest *x862-arg-z*) (x862-cd-merge xfer tag2) xfer) (car forms)))
    (setq *x862-vstack* vstack *x862-cstack* cstack)
    (@ tag1)
    (when (eq dest *x862-arg-z*)
      (<- *x862-arg-z*)
      (^))
    (@ tag2)))

(defx862 x862-simple-function simple-function (seg vreg xfer afunc)
  (x862-immediate seg vreg xfer (x862-afunc-lfun-ref afunc)))

(defx862 x862-list list (seg vreg xfer arglist)
  (if (null vreg)
    (dolist (form arglist)
      (x862-form seg vreg nil form)) 
    (let* ((*x862-vstack* *x862-vstack*)
           (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
           (nargs (x862-formlist seg arglist nil)))
      (x862-set-nargs seg nargs)
      (! list)
      (<- *x862-arg-z*)))
  (^))

(defx862 x862-list* list* (seg vreg xfer arglist)
  (if (null vreg)
    (dolist (arg (apply #'append arglist))
      (x862-form seg nil nil arg))
    (let* ((*x862-vstack* *x862-vstack*)
           (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
           (nargs (x862-formlist seg (car arglist) (cadr arglist))))
      (declare (fixnum nargs))
      (when (> nargs 1)
        (x862-set-nargs seg (1- nargs))
        (! list*))
      (<- *x862-arg-z*)))
  (^))

(defx862 x862-minus1 minus1 (seg vreg xfer form)
  (x862-unary-builtin seg vreg xfer '%negate form))

;;; Return T if form is declare to be something that couldn't be a fixnum.
(defun x862-explicit-non-fixnum-type-p (form)
  (let* ((type (x862-form-type form))
         (target-fixnum-type (nx-target-type 'fixnum)))
    (and (not (subtypep type target-fixnum-type))
         (not (subtypep target-fixnum-type type)))))

(defun x862-inline-sub2 (seg vreg xfer form1 form2)
  (let* ((v2 (acode-fixnum-form-p form2)))
    (if (and v2 (not (eql v2 most-negative-fixnum)))
      (x862-inline-add2 seg vreg xfer form1 (make-acode (%nx1-operator fixnum) (- v2)))
      (with-x86-local-vinsn-macros (seg vreg xfer)
        (x862-two-targeted-reg-forms seg form1 ($ *x862-arg-y*) form2 ($ *x862-arg-z*))
    (let* ((out-of-line (backend-get-next-label))
           (done (backend-get-next-label)))
      (ensuring-node-target (target vreg)
        (if (acode-fixnum-form-p form1)
          (! branch-unless-arg-fixnum ($ *x862-arg-z*) (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (! branch-unless-arg-fixnum ($ *x862-arg-y*) (aref *backend-labels* out-of-line))  
            (! branch-unless-both-args-fixnums ($ *x862-arg-y*) ($ *x862-arg-z*) (aref *backend-labels* out-of-line))))
        (! fixnum-sub2 ($ *x862-arg-z*) ($ *x862-arg-y*) ($ *x862-arg-z*))
        (x862-check-fixnum-overflow seg ($ *x862-arg-z*) done)
        (@ out-of-line)
        (! call-subprim-2 ($ *x862-arg-z*) (subprim-name->offset '.SPbuiltin-minus) ($ *x862-arg-y*) ($ *x862-arg-z*))
        (@ done)
        (x862-copy-register seg target ($ *x862-arg-z*)))
      (^))))))

(defun x862-inline-add2 (seg vreg xfer form1 form2)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((fix1 (acode-fixnum-form-p form1))
	   (fix2 (acode-fixnum-form-p form2))
	   (otherform (if (and fix1
			       (typep (ash fix1 *x862-target-fixnum-shift*)
				      '(signed-byte 32)))
			form2
			(if (and fix2
				 (typep (ash fix2 *x862-target-fixnum-shift*)
					'(signed-byte 32)))
			  form1))))
      (if otherform
        (x862-one-targeted-reg-form seg otherform ($ *x862-arg-z*))
        (x862-two-targeted-reg-forms seg form1 ($ *x862-arg-y*) form2 ($ *x862-arg-z*)))
      (let* ((out-of-line (backend-get-next-label))
             (done (backend-get-next-label)))
      
        (ensuring-node-target (target vreg)
          (if otherform
            (unless (acode-fixnum-form-p otherform)
              (! branch-unless-arg-fixnum ($ *x862-arg-z*) (aref *backend-labels* out-of-line)))          
            (if (acode-fixnum-form-p form1)
              (! branch-unless-arg-fixnum ($ *x862-arg-z*) (aref *backend-labels* out-of-line))
              (if (acode-fixnum-form-p form2)
                (! branch-unless-arg-fixnum ($ *x862-arg-y*) (aref *backend-labels* out-of-line))  
                (! branch-unless-both-args-fixnums ($ *x862-arg-y*) ($ *x862-arg-z*) (aref *backend-labels* out-of-line)))))
          (if otherform
            (! add-constant ($ *x862-arg-z*) (ash (or fix1 fix2) *x862-target-fixnum-shift*))
            (! fixnum-add2 ($ *x862-arg-z*) ($ *x862-arg-y*)))
          (x862-check-fixnum-overflow seg ($ *x862-arg-z*) done)
          (@ out-of-line)
          (if otherform
            (x862-lri seg ($ *x862-arg-y*) (ash (or fix1 fix2) *x862-target-fixnum-shift*)))
          (! call-subprim-2 ($ *x862-arg-z*) (subprim-name->offset '.SPbuiltin-plus) ($ *x862-arg-y*) ($ *x862-arg-z*))
          (@ done)
          (x862-copy-register seg target ($ *x862-arg-z*)))
        (^)))))
           
(defx862 x862-add2 add2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *x862-trust-declarations*)
    (if (and (x862-form-typep form1 'double-float)
             (x862-form-typep form2 'double-float))
      (x862-use-operator (%nx1-operator %double-float+-2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (x862-form-typep form1 'single-float)
               (x862-form-typep form2 'single-float))
        (x862-use-operator (%nx1-operator %short-float+-2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
        (if (and (x862-form-typep form1 'fixnum)
                 (x862-form-typep form2 'fixnum))
          (x862-use-operator (%nx1-operator %i+)
                             seg
                             vreg
                             xfer
                             form1
                             form2
                             t)
          (if (or (x862-explicit-non-fixnum-type-p form1)
                  (x862-explicit-non-fixnum-type-p form2))
            (x862-binary-builtin seg vreg xfer '+-2 form1 form2)
            (x862-inline-add2 seg vreg xfer form1 form2)))))))

(defx862 x862-sub2 sub2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *x862-trust-declarations*)
    (if (and (x862-form-typep form1 'double-float)
             (x862-form-typep form2 'double-float))
      (x862-use-operator (%nx1-operator %double-float--2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (x862-form-typep form1 'single-float)
               (x862-form-typep form2 'single-float))
        (x862-use-operator (%nx1-operator %short-float--2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
        (if (and (x862-form-typep form1 'fixnum)
                 (x862-form-typep form2 'fixnum))
          (x862-use-operator (%nx1-operator %i-)
                             seg
                             vreg
                             xfer
                             form1
                             form2
                             t)
          (if (or (x862-explicit-non-fixnum-type-p form1)
                  (x862-explicit-non-fixnum-type-p form2))
            (x862-binary-builtin seg vreg xfer '--2 form1 form2)
            (x862-inline-sub2 seg vreg xfer form1 form2)))))))

(defx862 x862-mul2 mul2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *x862-trust-declarations*)
    (if (and (x862-form-typep form1 'double-float)
             (x862-form-typep form2 'double-float))
      (x862-use-operator (%nx1-operator %double-float*-2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (x862-form-typep form1 'single-float)
               (x862-form-typep form2 'single-float))
        (x862-use-operator (%nx1-operator %short-float*-2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
        (x862-binary-builtin seg vreg xfer '*-2 form1 form2)))))

(defx862 x862-div2 div2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *x862-trust-declarations*)
    (if (and (x862-form-typep form1 'double-float)
             (x862-form-typep form2 'double-float))
      (x862-use-operator (%nx1-operator %double-float/-2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (x862-form-typep form1 'single-float)
               (x862-form-typep form2 'single-float))
        (x862-use-operator (%nx1-operator %short-float/-2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
                (let* ((f2 (acode-fixnum-form-p form2))
               (unwrapped (acode-unwrapped-form form1))
               (f1 nil)
               (f1/f2 nil))
          (if (and f2
                   (not (zerop f2))
                   (acode-p unwrapped)
                   (or (eq (acode-operator unwrapped) (%nx1-operator mul2))
                       (eq (acode-operator unwrapped) (%nx1-operator %i*)))
                   (setq f1 (acode-fixnum-form-p (cadr unwrapped)))
                   (typep (setq f1/f2 (/ f1 f2)) 'fixnum))
            (x862-use-operator (%nx1-operator mul2)
                               seg
                               vreg
                               xfer
                               (make-acode (%nx1-operator fixnum) f1/f2)
                               (caddr unwrapped))
            (x862-binary-builtin seg vreg xfer '/-2 form1 form2)))))))

(defx862 x862-logbitp logbitp (seg vreg xfer bitnum int)
  (x862-binary-builtin seg vreg xfer 'logbitp bitnum int))

(defun x862-inline-logior2 (seg vreg xfer form1 form2)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2)))
      (if (and fix1 fix2)
        (x862-use-operator (%nx1-operator fixnum) seg vreg xfer (logior fix1 fix2))
        (let* ((fixval (or fix1 fix2))
               (fiximm (if fixval (<= (integer-length fixval)
                                      (- 31 *x862-target-fixnum-shift*))))
               (otherform (when fiximm (if fix1 form2 form1))))
          (let* ((out-of-line (backend-get-next-label))
                 (done (backend-get-next-label)))
            (ensuring-node-target (target vreg)
              (if otherform
                (x862-one-targeted-reg-form seg otherform ($ *x862-arg-z*))
                (x862-two-targeted-reg-forms seg form1 ($ *x862-arg-y*) form2 ($ *x862-arg-z*)))
              (if otherform
                (unless (acode-fixnum-form-p otherform)
                  (! branch-unless-arg-fixnum ($ *x862-arg-z*) (aref *backend-labels* out-of-line)))
                (if (acode-fixnum-form-p form1)
                  (! branch-unless-arg-fixnum ($ *x862-arg-z*) (aref *backend-labels* out-of-line))
                  (if (acode-fixnum-form-p form2)
                    (! branch-unless-arg-fixnum ($ *x862-arg-y*) (aref *backend-labels* out-of-line))  
                    (! branch-unless-both-args-fixnums ($ *x862-arg-y*) ($ *x862-arg-z*) (aref *backend-labels* out-of-line)))))
              (if otherform
                (! %logior-c ($ *x862-arg-z*) ($ *x862-arg-z*) (ash fixval *x862-target-fixnum-shift*))
                (! %logior2 ($ *x862-arg-z*) ($ *x862-arg-z*) ($ *x862-arg-y*)))
              (-> done)
              (@ out-of-line)
              (if otherform
                (x862-lri seg ($ *x862-arg-y*) (ash fixval *x862-target-fixnum-shift*)))
              (! call-subprim-2 ($ *x862-arg-z*) (subprim-name->offset '.SPbuiltin-logior) ($ *x862-arg-y*) ($ *x862-arg-z*))
              (@ done)
              (x862-copy-register seg target ($ *x862-arg-z*)))
            (^)))))))

(defx862 x862-logior2 logior2 (seg vreg xfer form1 form2)
  (if (or (x862-explicit-non-fixnum-type-p form1)
          (x862-explicit-non-fixnum-type-p form2))
    (x862-binary-builtin seg vreg xfer 'logior-2 form1 form2)
    (x862-inline-logior2 seg vreg xfer form1 form2)))

(defx862 x862-logxor2 logxor2 (seg vreg xfer form1 form2)
  (x862-binary-builtin seg vreg xfer 'logxor-2 form1 form2))

(defun x862-inline-logand2 (seg vreg xfer form1 form2)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2)))
      (if (and fix1 fix2)
        (x862-use-operator (%nx1-operator fixnum) seg vreg xfer (logand fix1 fix2))
        (let* ((fixval (or fix1 fix2))
               (fiximm (if fixval (<= (integer-length fixval)
                                      (- 31 *x862-target-fixnum-shift*))))
               (otherform (when fiximm (if fix1 form2 form1))))
          (let* ((out-of-line (backend-get-next-label))
                 (done (backend-get-next-label)))
            (ensuring-node-target (target vreg)
              (if otherform
                (x862-one-targeted-reg-form seg otherform ($ *x862-arg-z*))
                (x862-two-targeted-reg-forms seg form1 ($ *x862-arg-y*) form2 ($ *x862-arg-z*)))
              (if otherform
                (unless (acode-fixnum-form-p otherform)
                  (! branch-unless-arg-fixnum ($ *x862-arg-z*) (aref *backend-labels* out-of-line)))
                (if (acode-fixnum-form-p form1)
                  (! branch-unless-arg-fixnum ($ *x862-arg-z*) (aref *backend-labels* out-of-line))
                  (if (acode-fixnum-form-p form2)
                    (! branch-unless-arg-fixnum ($ *x862-arg-y*) (aref *backend-labels* out-of-line))  
                    (! branch-unless-both-args-fixnums ($ *x862-arg-y*) ($ *x862-arg-z*) (aref *backend-labels* out-of-line)))))
              (if otherform
                (! %logand-c ($ *x862-arg-z*) ($ *x862-arg-z*) (ash fixval *x862-target-fixnum-shift*))
                (! %logand2 ($ *x862-arg-z*) ($ *x862-arg-z*) ($ *x862-arg-y*)))
              (-> done)
              (@ out-of-line)
              (if otherform
                (x862-lri seg ($ *x862-arg-y*) (ash fixval *x862-target-fixnum-shift*)))
              (! call-subprim-2 ($ *x862-arg-z*) (subprim-name->offset '.SPbuiltin-logand) ($ *x862-arg-y*) ($ *x862-arg-z*))
              (@ done)
              (x862-copy-register seg target ($ *x862-arg-z*)))
            (^)))))))

(defx862 x862-logand2 logand2 (seg vreg xfer form1 form2)

  (if (or (x862-explicit-non-fixnum-type-p form1)
          (x862-explicit-non-fixnum-type-p form2))
    (x862-binary-builtin seg vreg xfer 'logand-2 form1 form2)
    (x862-inline-logand2 seg vreg xfer form1 form2)))

(defx862 x862-%quo2 %quo2 (seg vreg xfer form1 form2)
  (x862-binary-builtin seg vreg xfer '/-2 form1 form2))

(defx862 x862-%aref1 %aref1 (seg vreg xfer v i)
  (let* ((vtype (acode-form-type v t))
         (ctype (if vtype (specifier-type vtype)))
         (atype (if (array-ctype-p ctype) ctype))
         (keyword (if (and atype
                           (let* ((dims (array-ctype-dimensions atype)))
                             (or (eq dims '*)
                                 (and (not (atom dims))
                                      (= (length dims) 1))))
                           (not (array-ctype-complexp atype)))
                    (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (if keyword
      (x862-vref  seg vreg xfer keyword v i (unless *x862-reckless*
                                              (nx-lookup-target-uvector-subtag keyword)))
      (x862-binary-builtin seg vreg xfer '%aref1 v i))))

(defx862 x862-%aset1 aset1 (seg vreg xfer v i n)
  (let* ((vtype (acode-form-type v t))
         (atype (if vtype (specifier-type vtype)))
         (keyword (if (and atype
                           (let* ((dims (array-ctype-dimensions atype)))
                             (or (eq dims '*)
                                 (and (not (atom dims))
                                      (= (length dims) 1))))
                           (not (array-ctype-complexp atype)))
                    (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (if keyword
      (x862-vset seg vreg xfer keyword v i n (not *x862-reckless*))
      (target-arch-case
       (:x8632
	(with-x86-local-vinsn-macros (seg vreg xfer)
	  (let* ((subprim (subprim-name->offset '.SPaset1))
		 (tail-p (x862-tailcallok xfer)))
	    (x862-three-targeted-reg-forms seg
					   v ($ x8632::temp0)
					   i ($ x8632::arg_y)
					   n ($ x8632::arg_z))
	    (if tail-p
	      (progn
		(x862-restore-full-lisp-context seg)
		(! jump-subprim subprim))
	      (progn
		(! call-subprim subprim)
		(when vreg
		  (<- ($ x8632::arg_z)))
		(^))))))
       (:x8664
	(x862-ternary-builtin seg vreg xfer '%aset1 v i n))))))

(defx862 x862-%i+ %i+ (seg vreg xfer form1 form2 &optional overflow)
  (when overflow
    (let* ((type *x862-target-half-fixnum-type*))
      (when (and (x862-form-typep form1 type)
                 (x862-form-typep form2 type))
        (setq overflow nil))))
  (cond ((null vreg) 
         (x862-form seg nil nil form1) 
         (x862-form seg nil xfer form2))
        
        (t                              
         (let* ((fix1 (acode-fixnum-form-p form1))
                (fix2 (acode-fixnum-form-p form2))
                (other (if (and fix1
                                (typep (ash fix1 *x862-target-fixnum-shift*)
                                       '(signed-byte 32)))
                         form2
                         (if (and fix2
                                  (typep (ash fix2 *x862-target-fixnum-shift*)
                                         '(signed-byte 32)))
                           form1))))
           (if (and fix1 fix2)
             (x862-lri seg vreg (ash (+ fix1 fix2) *x862-target-fixnum-shift*))
             (if other
               (let* ((constant (ash (or fix1 fix2) *x862-target-fixnum-shift*)))
                 (if (zerop constant)
                   (x862-form seg vreg nil other)
                   (if overflow
                     (ensuring-node-target (target vreg)
                       (x862-one-targeted-reg-form seg other target)
                       (! add-constant target constant)
                       (x862-check-fixnum-overflow seg target))
                     (ensuring-node-target (target vreg)
                       (let* ((reg (x862-one-untargeted-reg-form seg other target)))
                         (! add-constant3 target reg constant))))))
               (if (not overflow)
                 (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg form1 *x862-arg-y* form2 *x862-arg-z*)
                   ;; This isn't guaranteed to set the overflow flag,
                   ;; but may do so.
                   (ensuring-node-target (target vreg)
                     (! fixnum-add3 target r1 r2)))
                 (ensuring-node-target (target vreg)
                   (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg form1 *x862-arg-y* form2 *x862-arg-z*)
                     (cond ((= (hard-regspec-value target)
                               (hard-regspec-value r1))
                            (! fixnum-add2 target r2))
                           ((= (hard-regspec-value target)
                               (hard-regspec-value r2))
                            (! fixnum-add2 target r1))
                           (t
                            (x862-copy-register seg target r1)
                            (! fixnum-add2 target r2)))
                     (x862-check-fixnum-overflow seg target))))))
           (^)))))

(defx862 x862-%i- %i- (seg vreg xfer num1 num2 &optional overflow)
  (when overflow
    (let* ((type *x862-target-half-fixnum-type*))
      (when (and (x862-form-typep num1 type)
                 (x862-form-typep num2 type))
        (setq overflow nil))))
  (let* ((v1 (acode-fixnum-form-p num1))
         (v2 (acode-fixnum-form-p num2)))
    (if (and v1 v2)
      (x862-use-operator (%nx1-operator fixnum) seg vreg xfer (%i- v1 v2))
      (if (and v2 (neq v2 most-negative-fixnum))
        (x862-use-operator (%nx1-operator %i+) seg vreg xfer num1 (make-acode (%nx1-operator fixnum) (- v2)) overflow) 
          (cond
           ((null vreg)
            (x862-form seg nil nil num1)
            (x862-form seg nil xfer num2))
           (t                              
            (let* ((fix1 (acode-fixnum-form-p num1))
                   (fix2 (acode-fixnum-form-p num2)))
              (if (and fix1 fix2 (not overflow))
                (x862-lri seg vreg (ash (- fix1 fix2) *x862-target-fixnum-shift*))
                (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg num1 *x862-arg-y* num2 *x862-arg-z*)
                      ;; This isn't guaranteed to set the overflow flag,
                      ;; but may do so.
                      (ensuring-node-target (target vreg)
                        (! fixnum-sub2 target r1 r2)
                        (if overflow
                          (x862-check-fixnum-overflow seg target))))))
            (^)))))))

(defx862 x862-%i* %i* (seg vreg xfer num1 num2)
  (if (null vreg)
    (progn
      (x862-form seg nil nil num1)
      (x862-form seg nil xfer num2))  
    (let* ((fix1 (acode-fixnum-form-p num1))
           (fix2 (acode-fixnum-form-p num2))
           (other (if (typep fix1 '(signed-byte 32)) num2 (if (typep fix2 '(signed-byte 32)) num1))))
      (if (and fix1 fix2)
        (x862-lri seg vreg (ash (* fix1 fix2) *x862-target-fixnum-shift*))
        (if other
          (! multiply-immediate vreg (x862-one-untargeted-reg-form seg other *x862-arg-z*) (or fix1 fix2))
          (multiple-value-bind (rx ry) (x862-two-untargeted-reg-forms seg num1 *x862-arg-y* num2 *x862-arg-z*)
            (ensuring-node-target (target vreg)
              (! multiply-fixnums target rx ry)))))
      (^))))

(defx862 x862-nth-value nth-value (seg vreg xfer n form)
  (let* ((*x862-vstack* *x862-vstack*)
         (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
    (let* ((nreg (x862-one-untargeted-reg-form seg n *x862-arg-z*)))
      (unless (acode-fixnum-form-p n)
        (! trap-unless-fixnum nreg))
      (x862-vpush-register seg nreg))
     (x862-multiple-value-body seg form) ; sets nargs
    (! nth-value *x862-arg-z*))
  (<- *x862-arg-z*)
  (^))

(defx862 x862-values values (seg vreg xfer forms)
  (if (eq (list-length forms) 1)
    (if (x862-cd-compound-p xfer)
      (x862-form seg vreg xfer (%car forms))
      (progn
        (x862-form seg vreg nil (%car forms))
        (^)))
    (if (not (x862-mv-p xfer))
      (if forms
        (x862-use-operator (%nx1-operator prog1) seg vreg xfer forms)
        (x862-nil seg vreg xfer))
      (progn
        (let* ((*x862-vstack* *x862-vstack*)
               (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
          (x862-set-nargs seg (x862-formlist seg forms nil)))
        (let* ((*x862-returning-values* t))
          (^))))))

(defx862 x862-base-char-p base-char-p (seg vreg xfer cc form)
  (x862-char-p seg vreg xfer cc form))

(defun x862-char-p (seg vreg xfer cc form)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
      (! mask-base-char *x862-imm0* (x862-one-untargeted-reg-form seg form *x862-arg-z*))
      (x862-test-reg-%izerop seg vreg xfer *x862-imm0* cr-bit true-p
                             (target-arch-case
                              (:x8632 x8632::subtag-character)
                              (:x8664 x8664::subtag-character))))))



(defx862 x862-let* let* (seg vreg xfer vars vals body p2decls &aux
                             (old-stack (x862-encode-stack)))
  (x862-check-lcell-depth)
  (with-x86-p2-declarations p2decls
    (x862-seq-bind seg vars vals)
    (x862-undo-body seg vreg xfer body old-stack))
  (dolist (v vars) (x862-close-var seg v)))

(defx862 x862-multiple-value-bind multiple-value-bind (seg vreg xfer vars valform body p2decls)
  (let* ((n (list-length vars))
         (vloc *x862-vstack*)
         (nbytes (* n *x862-target-node-size*))
         (old-stack (x862-encode-stack)))
    (with-x86-p2-declarations p2decls
      (x862-multiple-value-body seg valform)
      (! fitvals n)
      (x862-set-vstack (%i+ vloc nbytes))
      (let* ((old-top *x862-top-vstack-lcell*)
             (lcells (progn (x862-reserve-vstack-lcells n) (x862-collect-lcells :reserved old-top))))
        (dolist (var vars)
          (let* ((lcell (pop lcells))
                 (reg (x862-assign-register-var var)))
            (if reg
              (x862-init-regvar seg var reg (x862-vloc-ea vloc))
              (x862-bind-var seg var vloc lcell))          
            (setq vloc (%i+ vloc *x862-target-node-size*)))))
      (x862-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (x862-close-var seg var)))))

(defx862 x862-debind debind (seg vreg xfer lambda-list bindform req opt rest keys auxen whole body p2decls cdr-p)
  (declare (ignore lambda-list))
  (let* ((old-stack (x862-encode-stack))
         (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
         (vloc *x862-vstack*))
    (with-x86-p2-declarations p2decls      
      (x862-bind-structured-lambda
       seg 
       (x862-spread-lambda-list seg bindform whole req opt rest keys nil cdr-p)
       vloc (x862-vloc-ea vloc) whole req opt rest keys auxen)
      (x862-undo-body seg vreg xfer body old-stack)
      (x862-close-structured-lambda seg whole req opt rest keys auxen))))

(defx862 x862-multiple-value-prog1 multiple-value-prog1 (seg vreg xfer forms)
  (if (or (not (x862-mv-p xfer)) (x862-single-valued-form-p (%car forms)))
    (x862-use-operator (%nx1-operator prog1) seg vreg xfer forms)
    (progn
      (let* ((*x862-vstack* *x862-vstack*)
             (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
        (x862-multiple-value-body seg (%car forms))
        (x862-open-undo $undostkblk)
        (! save-values))
      (dolist (form (cdr forms))
        (x862-form seg nil nil form))
      (x862-set-nargs seg 0)
      (! recover-values)
      (x862-close-undo)
      (let* ((*x862-returning-values* t))
        (^)))))

(defx862 x862-not not (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
    (let* ((ea (x862-lexical-reference-ea form nil)))
      (if (and ea (memory-spec-p ea))
        (x862-compare-ea-to-nil
         seg
         vreg
         xfer
         ea
         cr-bit
         true-p)
        (x862-compare-register-to-nil
         seg 
         vreg 
         xfer
         (x862-one-untargeted-reg-form seg form *x862-arg-z*) 
         cr-bit
         true-p)))))


(defx862 x862-%alloc-misc %make-uvector (seg vreg xfer element-count st &optional initval)
  (if (null vreg)
    (progn
      (x862-form seg nil nil element-count)
      (x862-form seg nil xfer st))
    (let* ((subtag (acode-fixnum-form-p st))
           (nelements (acode-fixnum-form-p element-count))         
           (nbytes (if (and subtag nelements) (x862-misc-byte-count subtag nelements))))
      (if (and  nbytes (null initval)
                (< (logand
                    (lognot (1- *x862-target-dnode-size*))
                    (+ nbytes *x862-target-node-size*
                       (1- *x862-target-dnode-size*))) #x8000))
	(let* ((header *x862-imm0*)
	       (n (- (* (ceiling (+ nbytes *x862-target-node-size*) *x862-target-dnode-size*) *x862-target-dnode-size*)
		     (target-arch-case
		      (:x8632 x8632::fulltag-misc)
		      (:x8664 x8664::fulltag-misc)))))
	  (x862-lri seg header (arch::make-vheader nelements subtag))
	  (target-arch-case
	   (:x8632
	    (! setup-uvector-allocation header)
	    (x862-lri seg x8632::imm0 n))
	   (:x8664
	    (x862-lri seg x8664::imm1 n)))
          (ensuring-node-target (target vreg)
            (! %allocate-uvector target)))
        (progn
          (if initval
            (progn
              (x862-three-targeted-reg-forms seg element-count
					     (target-arch-case
					      (:x8632 ($ x8632::temp0))
					      (:x8664 ($ x8664::arg_x)))
					     st ($ *x862-arg-y*)
					     initval ($ *x862-arg-z*))
              (! misc-alloc-init)
              (<- ($ *x862-arg-z*)))
            (progn
              (x862-two-targeted-reg-forms seg element-count ($ *x862-arg-y*) st ($ *x862-arg-z*))
              (! misc-alloc)
              (<- ($ *x862-arg-z*))))))
        (^))))

(defx862 x862-%iasr %iasr (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (x862-form seg nil nil form1)
      (x862-form seg vreg xfer form2))
    (let* ((count (acode-fixnum-form-p form1))
           (max (target-arch-case (:x8632 31) (:x8664 63))))
      (declare (fixnum max))
      (ensuring-node-target (target vreg)
        (if count
          (! %iasr-c target (if (> count max) max count)
             (x862-one-untargeted-reg-form seg form2 *x862-arg-z*))
          (multiple-value-bind (cnt src) (x862-two-targeted-reg-forms seg form1 ($ *x862-arg-y*) form2 ($ *x862-arg-z*))
            (! %iasr target cnt src))))
      (^))))

(defx862 x862-%ilsr %ilsr (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (x862-form seg nil nil form1)
      (x862-form seg vreg xfer form2))
    (let* ((count (acode-fixnum-form-p form1)))
      (ensuring-node-target (target vreg)
        (if count
          (let ((src (x862-one-untargeted-reg-form seg form2 ($ *x862-arg-z*))))
            (if (<= count 31)
              (! %ilsr-c target count src)
              (!  lri target 0)))
          (multiple-value-bind (cnt src) (x862-two-targeted-reg-forms seg form1 ($ *x862-arg-y*) form2 ($ *x862-arg-z*))
            (! %ilsr target cnt src))))
      (^))))


(defx862 x862-%i<> %i<> (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
    (x862-compare seg vreg xfer form1 form2 cr-bit true-p)))

(defx862 x862-%natural<> %natural<> (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
    (x862-natural-compare seg vreg xfer form1 form2 cr-bit true-p)))

(defx862 x862-double-float-compare double-float-compare (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
    (with-fp-target () (r1 :double-float)
      (with-fp-target (r1) (r2 :double-float)
        (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg form1 r1 form2 r2)
          (x862-compare-double-float-registers seg vreg xfer r1 r2 cr-bit true-p))))))

(defx862 x862-short-float-compare short-float-compare (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
    (with-fp-target () (r1 :single-float)
      (with-fp-target (r1) (r2 :single-float)
        (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg form1 r1 form2 r2)
          (x862-compare-single-float-registers seg vreg xfer r1 r2 cr-bit true-p))))))
 
(eval-when (:compile-toplevel :execute)
  (defmacro defx862-df-op (fname opname vinsn)
    `(defx862 ,fname ,opname (seg vreg xfer f0 f1)
      (if (null vreg)
        (progn
          (x862-form seg nil nil f0)
          (x862-form seg vreg xfer f1))
        (with-fp-target () (r1 :double-float)
          (with-fp-target (r1) (r2 :double-float)
            (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg f0 r1 f1 r2)
              (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                       (or (not (= (hard-regspec-value vreg)
                                   (hard-regspec-value r2)))
                           ,(and 
                             (not (eq opname '%double-float--2))
                             (not (eq opname '%double-float/-2)))))
                (! ,vinsn vreg r1 r2)
                (with-fp-target (r2) (result :double-float)
                  (! ,vinsn result r1 r2)
                  (if (= (hard-regspec-class vreg) hard-reg-class-fpr)
                    (<- result)
                    (ensuring-node-target (target vreg)
                      (x862-copy-register seg target result)))))
              (^)))))))
  
  (defmacro defx862-sf-op (fname opname vinsn)
    `(defx862 ,fname ,opname (seg vreg xfer f0 f1)
      (if (null vreg)
        (progn
          (x862-form seg nil nil f0)
          (x862-form seg vreg xfer f1))
        (with-fp-target () (r1 :single-float)
          (with-fp-target (r1) (r2 :single-float)
            (multiple-value-bind (r1 r2) (x862-two-untargeted-reg-forms seg f0 r1 f1 r2)
              (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                       (or (not (= (hard-regspec-value vreg)
                                   (hard-regspec-value r2)))
                           ,(and 
                             (not (eq opname '%short-float--2))
                             (not (eq opname '%short-float/-2)))))
                (! ,vinsn vreg r1 r2)
                (with-fp-target (r2) (result :single-float)
                  (! ,vinsn result r1 r2)
                  (if (= (hard-regspec-class vreg) hard-reg-class-fpr)
                    (<- result)
                    (ensuring-node-target (target vreg)
                      (x862-copy-register seg target result)))))
              (^)))))))
  )

(defx862-df-op x862-%double-float+-2 %double-float+-2 double-float+-2)
(defx862-df-op x862-%double-float--2 %double-float--2 double-float--2)
(defx862-df-op x862-%double-float*-2 %double-float*-2 double-float*-2)
(defx862-df-op x862-%double-float/-2 %double-float/-2 double-float/-2)

(defx862-sf-op x862-%short-float+-2 %short-float+-2 single-float+-2)
(defx862-sf-op x862-%short-float--2 %short-float--2 single-float--2)
(defx862-sf-op x862-%short-float*-2 %short-float*-2 single-float*-2)
(defx862-sf-op x862-%short-float/-2 %short-float/-2 single-float/-2)

(defun x862-get-float (seg vreg xfer ptr offset double-p fp-reg)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (cond ((null vreg)
           (x862-form seg nil nil ptr)
           (x862-form seg nil xfer offset))
          (t
           (let* ((fixoffset (acode-fixnum-form-p offset)))
             (if (typep fixoffset '(signed-byte 32))
               (with-imm-target () (ptrreg :address)
                 (x862-form seg ptrreg nil ptr)
                 (if double-p
                   (! mem-ref-c-double-float fp-reg ptrreg fixoffset)
                   (! mem-ref-c-single-float fp-reg ptrreg fixoffset)))
               (with-imm-target () (ptrreg :address)
		 (with-additional-imm-reg ()
		   (with-imm-target (ptrreg) (offsetreg :signed-natural)
		     (x862-two-targeted-reg-forms seg
						  ptr ptrreg
						  offset ($ *x862-arg-z*))
		     (! fixnum->signed-natural offsetreg *x862-arg-z*)
		     (if double-p
		       (! mem-ref-double-float fp-reg ptrreg offsetreg)
		       (! mem-ref-single-float fp-reg ptrreg offsetreg))))))
             (<- fp-reg))
           (^)))))
    

(defx862 x862-%get-double-float %get-double-float (seg vreg xfer ptr offset)
  (with-fp-target () (fp-reg :double-float)
    (x862-get-float seg vreg xfer ptr offset t fp-reg)))

(defx862 x862-%get-single-float %get-single-float (seg vreg xfer ptr offset)
  (with-fp-target () (fp-reg :single-float)
    (x862-get-float seg vreg xfer ptr offset nil fp-reg)))

(defun x862-set-float (seg vreg xfer ptr offset newval double-p fp-reg)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((fixoffset (acode-fixnum-form-p offset))
           (immoffset (typep fixoffset '(unsigned-byte 15))))
      (with-imm-target () (ptr-reg :address) 
        (cond ((or (null vreg)
                   (= (hard-regspec-class vreg) hard-reg-class-fpr))
               (cond (immoffset
                      (x862-push-register
                       seg
                       (x862-one-untargeted-reg-form seg
                                                     ptr
                                                     ptr-reg))
                      (x862-one-targeted-reg-form seg newval fp-reg)
                      (x862-pop-register seg ptr-reg)
                      (if double-p
                        (! mem-set-c-double-float fp-reg ptr-reg fixoffset)
                        (! mem-set-c-single-float fp-reg ptr-reg fixoffset)))
                     (t
		      (with-additional-imm-reg ()
			(with-imm-target (ptr-reg) (offset-reg :s32)
			  (x862-push-register
			   seg
			   (x862-one-untargeted-reg-form seg
							 ptr
							 ptr-reg))
			  (x862-push-register
			   seg
			   (x862-one-untargeted-reg-form seg
							 offset
							 *x862-arg-z*))
			  (x862-one-targeted-reg-form seg newval fp-reg)
			  (x862-pop-register seg *x862-arg-z*)
			  (x862-pop-register seg ptr-reg)
			  (! fixnum->signed-natural offset-reg *x862-arg-z*)
			  (if double-p
			    (! mem-set-double-float fp-reg ptr-reg offset-reg)
			    (! mem-set-single-float fp-reg ptr-reg offset-reg))))))
               (<- fp-reg))
              (t
               (cond (immoffset
                      (let* ((rnew ($ *x862-arg-z*)))
                        (x862-push-register
                         seg
                         (x862-one-untargeted-reg-form seg
                                                       ptr
                                                       ptr-reg))
                        (x862-one-targeted-reg-form seg newval rnew)
                        (x862-pop-register seg ptr-reg)
			(with-additional-imm-reg ()
			  (with-imm-temps (ptr-reg) ()
			    (x862-copy-register seg fp-reg rnew)
			    (if double-p
			      (! mem-set-c-double-float fp-reg ptr-reg fixoffset)
			      (! mem-set-c-single-float fp-reg ptr-reg fixoffset))))))
                     (t
                      (let* ((roffset ($ *x862-arg-y*))
                             (rnew ($ *x862-arg-z*)))
                        (x862-push-register
                         seg
                         (x862-one-untargeted-reg-form
                          seg
                          ptr ptr-reg))
                        (x862-two-targeted-reg-forms seg
                                                   offset roffset
                                                   newval rnew)
                        (x862-pop-register seg ptr-reg)
			(with-additional-imm-reg ()
			  (with-imm-target (ptr-reg) (offset-reg :s32)
			    (with-imm-temps (ptr-reg) ()
			      (x862-copy-register seg fp-reg rnew)
			      (! fixnum->signed-natural offset-reg roffset))
			    (if double-p
			      (! mem-set-double-float fp-reg ptr-reg offset-reg)
			      (! mem-set-single-float fp-reg ptr-reg offset-reg)))))))
               (<- *x862-arg-z*)))
        (^)))))

(defx862 x862-%set-double-float %set-double-float (seg vreg xfer ptr offset newval)
  (with-fp-target () (fp-reg :double-float)
    (x862-set-float seg vreg xfer ptr offset newval t fp-reg)))
      
(defx862 x862-%set-single-float %set-single-float (seg vreg xfer ptr offset newval)
  (with-fp-target () (fp-reg :single-float)
    (x862-set-float seg vreg xfer ptr offset newval nil fp-reg)))

(defx862 x862-immediate-get-ptr immediate-get-ptr (seg vreg xfer ptr offset)
  (let* ((absptr (acode-absolute-ptr-p ptr))
         (triv-p (x862-trivial-p offset))
         (dest vreg)
         (offval (acode-fixnum-form-p offset)))
    (cond ((not vreg)
           (x862-form seg nil nil ptr)
           (x862-form seg nil xfer offset))
          (t
           (if (and absptr offval) 
             (setq absptr (+ absptr offval) offval 0)
             (setq absptr nil))
           (and offval (%i> (integer-length offval) 31) (setq offval nil))
           (and absptr (%i> (integer-length absptr) 31) (setq absptr nil))
           (if absptr
             (! mem-ref-c-absolute-natural dest absptr)
             (if offval
               (let* ((src (x862-macptr-arg-to-reg seg ptr ($ *x862-imm0* :mode :address))))
                 (! mem-ref-c-natural dest src offval))
               (let* ((src (x862-macptr-arg-to-reg seg ptr ($ *x862-imm0* :mode :address))))
                 (if triv-p
		   (with-additional-imm-reg ()
		     (with-imm-temps (src) (x)
		       (if (acode-fixnum-form-p offset)
			 (x862-lri seg x (acode-fixnum-form-p offset))
			 (! fixnum->signed-natural x (x862-one-untargeted-reg-form seg offset *x862-arg-z*)))
		       (! mem-ref-natural dest src x)))
                   (progn
                     (! temp-push-unboxed-word src)
                     (x862-open-undo $undostkblk)
                     (let* ((oreg (x862-one-untargeted-reg-form seg offset *x862-arg-z*)))
		       (with-additional-imm-reg (*x862-arg-z*)
			 (with-imm-temps () (src x)
			   (! temp-pop-unboxed-word src)
			   (x862-close-undo)
			   (! fixnum->signed-natural x oreg)
			   (! mem-ref-natural dest src x)))))))))
           (^)))))

(defx862 x862-get-bit %get-bit (seg vreg xfer ptr offset)
  (if (null vreg)
    (progn
      (x862-form seg nil nil ptr)
      (x862-form seg nil xfer offset))
    (let* ((offval (acode-fixnum-form-p offset)))
      (if (typep offval '(signed-byte 32)) ; or thereabouts
        (with-imm-target () (src-reg :address)
            (x862-one-targeted-reg-form seg ptr src-reg)
          (if (node-reg-p vreg)
	    (! mem-ref-c-bit-fixnum vreg src-reg offval)
	    (with-imm-target ()         ;OK if src-reg & dest overlap
		(dest :u8)
	      (! mem-ref-c-bit dest src-reg offval)
	      (<- dest))))
        (with-imm-target () (src-reg :address)
          (x862-two-targeted-reg-forms seg ptr src-reg offset ($ *x862-arg-z*))
          (if (node-reg-p vreg)
            (! mem-ref-bit-fixnum vreg src-reg ($ *x862-arg-z*))
            (with-imm-target ()           ;OK if src-reg & dest overlap
                (dest :u8)
              (! mem-ref-bit dest src-reg offset)
              (<- dest)))))
      (^))))

    
      
;;; gonna run out of imm regs here                                      
;;; This returns an unboxed object, unless the caller wants to box it.
(defx862 x862-immediate-get-xxx immediate-get-xxx (seg vreg xfer bits ptr offset)
  (declare (fixnum bits))
  (let* ((fixnump (logbitp 6 bits))
         (signed (logbitp 5 bits))
         (size (logand 15 bits))
         (absptr (acode-absolute-ptr-p ptr))
         (triv-p (x862-trivial-p offset))
         (offval (acode-fixnum-form-p offset)))
    (declare (fixnum size))
    (cond ((null vreg)
           (x862-form seg nil nil ptr)
           (x862-form seg nil xfer offset))
          (t 
           (if (and absptr offval) 
             (setq absptr (+ absptr offval) offval 0)
             (setq absptr nil))
           (and offval (%i> (integer-length offval) 31) (setq offval nil))
           (and absptr (%i> (integer-length absptr) 31) (setq absptr nil))
	   ;;; huh?
           (target-arch-case
            (:x8632 (when (or fixnump (eql size 4) (and (eql size 4) signed))
		      (and offval (logtest 2 offval) (setq offval nil))
		      (and absptr (logtest 2 absptr) (setq absptr nil))))
            (:x8664 (when (or fixnump (eql size 8) (and (eql size 8) signed))
                      (and offval (logtest 3 offval) (setq offval nil))
                      (and absptr (logtest 3 absptr) (setq absptr nil))))) 
	   (cond
	     (fixnump
	      (with-imm-target () (dest :signed-natural)
		(cond
		  (absptr                              
		   (target-arch-case
		    (:x8632 (! mem-ref-c-absolute-fullword dest absptr))
		    (:x8664 (! mem-ref-c-absolute-doubleword dest  absptr))))
		  (offval
		   (with-imm-target () (src-reg :address)
		     (x862-one-targeted-reg-form seg ptr src-reg)
		     (target-arch-case
		      (:x8632 (! mem-ref-c-fullword dest src-reg offval))
		      (:x8664 (! mem-ref-c-doubleword dest src-reg offval)))))
		  (t
		   (with-imm-target () (src-reg :address)
		     (with-additional-imm-reg ()
		       (with-imm-target (src-reg) (offset-reg :signed-natural)
			 (x862-one-targeted-reg-form seg ptr src-reg)
			 (if triv-p
			   (if (acode-fixnum-form-p offset)
			     (x862-lri seg offset-reg (acode-fixnum-form-p offset))
			     (! fixnum->signed-natural offset-reg (x862-one-untargeted-reg-form seg offset *x862-arg-z*)))
			   (progn
			     (! temp-push-unboxed-word src-reg)
			     (x862-open-undo $undostkblk)
			     (! fixnum->signed-natural offset-reg (x862-one-untargeted-reg-form seg offset *x862-arg-z*))
			     (! temp-pop-unboxed-word src-reg)
			     (x862-close-undo)))
			 (target-arch-case
			  (:x8632 (! mem-ref-fullword dest src-reg offset-reg))
			  (:x8664 (! mem-ref-doubleword dest src-reg offset-reg))))))))
		(if (node-reg-p vreg)
		  (! box-fixnum vreg dest)
		  (<- dest))))
	     (signed
	      (with-imm-target () (dest :signed-natural)
		(cond
		  (absptr
		   (case size
		     (8 (! mem-ref-c-absolute-signed-doubleword dest absptr))
		     (4 (! mem-ref-c-absolute-signed-fullword dest  absptr))
		     (2 (! mem-ref-c-absolute-s16 dest absptr))
		     (1 (! mem-ref-c-absolute-s8 dest absptr))))
		  (offval
		   (with-additional-imm-reg ()
		     (with-imm-target (dest) (src-reg :address)
		       (x862-one-targeted-reg-form seg ptr src-reg)
		       (case size
			 (8 (! mem-ref-c-signed-doubleword dest src-reg offval))
			 (4 (! mem-ref-c-signed-fullword dest src-reg offval))
			 (2 (! mem-ref-c-s16 dest src-reg offval))
			 (1 (! mem-ref-c-s8 dest src-reg offval))))))
		  (t
		   (with-imm-target () (src-reg :address)
		     (with-additional-imm-reg ()
		       (with-imm-target (src-reg) (offset-reg :signed-natural)
			 (x862-one-targeted-reg-form seg ptr src-reg)
			 (if triv-p
			   (if (acode-fixnum-form-p offset)
			     (x862-lri seg offset-reg (acode-fixnum-form-p offset))
			     (! fixnum->signed-natural offset-reg (x862-one-untargeted-reg-form seg offset *x862-arg-z*)))
			   (progn
			     (! temp-push-unboxed-word src-reg)
			     (x862-open-undo $undostkblk)
			     (! fixnum->signed-natural offset-reg (x862-one-untargeted-reg-form seg offset *x862-arg-z*))
			     (! temp-pop-unboxed-word src-reg)
			     (x862-close-undo)))
			 (case size
			   (8 (! mem-ref-signed-doubleword dest src-reg offset-reg))
			   (4 (! mem-ref-signed-fullword dest src-reg offset-reg))
			   (2 (! mem-ref-s16 dest src-reg offset-reg))
			   (1 (! mem-ref-s8 dest src-reg offset-reg))))))))
		(if (node-reg-p vreg)
		  (case size
		    ((1 2) (! box-fixnum vreg dest))
		    (4 (target-arch-case
			(:x8632 (<- dest))
			(:x8664 (! box-fixnum vreg dest))))
		    (8 (<- dest)))
		  (<- dest))))
	     (t
	      (with-imm-target () (dest :natural)
		(cond
		  (absptr
		   (case size
		     (8 (! mem-ref-c-absolute-doubleword dest absptr))
		     (4 (! mem-ref-c-absolute-fullword dest absptr))
		     (2 (! mem-ref-c-absolute-u16 dest absptr))
		     (1 (! mem-ref-c-absolute-u8 dest absptr))))
		  (offval
		   (with-additional-imm-reg ()
		     (with-imm-target (dest) (src-reg :address)
		       (x862-one-targeted-reg-form seg ptr src-reg)
		       (case size
			 (8 (! mem-ref-c-doubleword dest src-reg offval))
			 (4 (! mem-ref-c-fullword dest src-reg offval))
			 (2 (! mem-ref-c-u16 dest src-reg offval))
			 (1 (! mem-ref-c-u8 dest src-reg offval))))))
		  (t
		   (with-additional-imm-reg ()
		     (with-imm-target () (src-reg :address)
		       (with-imm-target (src-reg) (offset-reg :signed-natural)
			 (x862-one-targeted-reg-form seg ptr src-reg)
			 (if triv-p
			   (if (acode-fixnum-form-p offset)
			     (x862-lri seg offset-reg (acode-fixnum-form-p offset))
			     (! fixnum->signed-natural offset-reg (x862-one-untargeted-reg-form seg offset *x862-arg-z*)))
			   (progn
			     (! temp-push-unboxed-word src-reg)
			     (x862-open-undo $undostkblk)
			     (! fixnum->signed-natural offset-reg (x862-one-untargeted-reg-form seg offset *x862-arg-z*))
			     (! temp-pop-unboxed-word src-reg)
			     (x862-close-undo)))
			 (case size
			   (8 (! mem-ref-doubleword dest src-reg offset-reg))
			   (4 (! mem-ref-fullword dest src-reg offset-reg))
			   (2 (! mem-ref-u16 dest src-reg offset-reg))
			   (1 (! mem-ref-u8 dest src-reg offset-reg))))))))
		(<- (set-regspec-mode 
		     dest 
		     (gpr-mode-name-value
		      (case size
			(8 :u64)
			(4 :u32)
			(2 :u16)
			(1 :u8))))))))
           (^)))))

(defx862 x862-let let (seg vreg xfer vars vals body p2decls)
  (let* ((old-stack (x862-encode-stack))
         (val nil)
         (bits nil)
         (valcopy vals))
    (with-x86-p2-declarations p2decls
      (dolist (var vars)
        (setq val (%car valcopy))
        (cond ((or (%ilogbitp $vbitspecial (setq bits (nx-var-bits var)))
                   (and (%ilogbitp $vbitreg bits)
                        (dolist (val (%cdr valcopy))
                          (unless (x862-trivial-p val) (return t)))))
               (let* ((pair (cons (x862-vloc-ea *x862-vstack*) nil)))
                 (%rplaca valcopy pair)
                 (if (and (%ilogbitp $vbitdynamicextent bits)
                          (progn
                            (setq val 
                                  (x862-dynamic-extent-form seg (x862-encode-stack) val))
                            (x862-load-ea-p val)))
                   (progn
                     (%rplaca pair (x862-vloc-ea *x862-vstack*))
                     (x862-vpush-register seg val :reserved))
                 (x862-vpush-register seg (x862-one-untargeted-reg-form seg val *x862-arg-z*) :reserved))
                 (%rplacd pair *x862-top-vstack-lcell*)))
              (t (x862-seq-bind-var seg var val)
                 (%rplaca valcopy nil)))
        (setq valcopy (%cdr valcopy)))
      (dolist (var vars)
        (declare (list val))
        (when (setq val (pop vals))
          (if (%ilogbitp $vbitspecial (nx-var-bits var))
            (progn
              (x862-dbind seg (car val) (var-name var))
              (x862-set-var-ea seg var (x862-vloc-ea (- *x862-vstack* *x862-target-node-size*)))
              )
            (x862-seq-bind-var seg var (car val)))))
      (x862-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (x862-close-var seg var)))))

(defx862 x862-closed-function closed-function (seg vreg xfer afunc)
  (x862-make-closure seg afunc nil)
  (when vreg (<- *x862-arg-z*))
  (^))

(defx862 x862-flet flet (seg vreg xfer vars afuncs body p2decls)
  (x862-seq-fbind seg vreg xfer vars afuncs body p2decls))

(defx862 x862-labels labels (seg vreg xfer vars afuncs body p2decls)
  (let* ((fwd-refs nil)
         (func nil)
         (togo vars)
         (real-vars ())
         (real-funcs ())
         (funs afuncs))
    (dolist (v vars)
      (when (neq 0 (afunc-fn-refcount (setq func (pop funs))))
        (push v real-vars)
        (push func real-funcs)
        (let* ((i (target-arch-case
		   (:x8632 7)
		   (:x8664 5))) ; skip 4 words of code, inner function
               (our-var nil)
               (item nil))
          (declare (fixnum i))
          (dolist (ref (afunc-inherited-vars func))
            (when (memq (setq our-var (var-bits ref)) togo)
              (setq item (cons i our-var))
              (let* ((refs (assq v fwd-refs)))
                (if refs
                  (push item (cdr refs))
                  (push (list v item) fwd-refs))))
            (incf i)))
        (setq togo (%cdr togo))))       
    (if (null fwd-refs)
      (x862-seq-fbind seg vreg xfer (nreverse real-vars) (nreverse real-funcs) body p2decls)
      (let* ((old-stack (x862-encode-stack)))
        (setq real-vars (nreverse real-vars) real-funcs (nreverse real-funcs))
        (with-x86-p2-declarations p2decls
          (dolist (var real-vars)
            (x862-seq-bind-var seg var (nx1-afunc-ref (pop real-funcs))))
          (dolist (ref fwd-refs)
            (let ((ea (var-ea (pop ref))))
              (x862-addrspec-to-reg seg ea *x862-temp0*)
              (dolist (r ref)
                (let* ((v-ea (var-ea (cdr r))))
                  (let* ((val-reg (if (eq v-ea ea)
                                    *x862-temp0*
                                    (progn
                                      (x862-addrspec-to-reg seg v-ea *x862-temp1*)
                                      *x862-temp1*))))
                    (! set-closure-forward-reference val-reg *x862-temp0* (car r)))))))
          (x862-undo-body seg vreg xfer body old-stack)
          (dolist (var real-vars)
            (x862-close-var seg var)))))))

;;; Make a function call (e.g., to mapcar) with some of the toplevel arguments
;;; stack-consed (downward) closures.  Bind temporaries to these closures so
;;; that tail-recursion/non-local exits work right.
;;; (all of the closures are distinct: FLET and LABELS establish dynamic extent themselves.)
(defx862 x862-with-downward-closures with-downward-closures (seg vreg xfer tempvars closures callform)
  (let* ((old-stack (x862-encode-stack)))
    (x862-seq-bind seg tempvars closures)
    (x862-undo-body seg vreg xfer callform old-stack)
    (dolist (v tempvars) (x862-close-var seg v))))


(defx862 x862-local-return-from local-return-from (seg vreg xfer blocktag value)
  (declare (ignorable vreg xfer))
  (let* ((*x862-undo-count* *x862-undo-count*)
         (tagdata (car blocktag))
         (cur-stack (x862-encode-stack))
         (dest-vd (caar tagdata))
         (dest-cd (cdar tagdata))
         (mv-p (x862-mvpass-p dest-cd))
         (dest-stack  (cdr tagdata))
         (need-break (neq cur-stack dest-stack)))
    (let* ((*x862-vstack* *x862-vstack*)
           (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
           (*x862-cstack* *x862-cstack*))
      (if 
        (or
         (eq dest-cd $backend-return)
         (and mv-p 
              (eq (x862-encoding-undo-count cur-stack)
                  (x862-encoding-undo-count dest-stack)) 
              (eq (x862-encoding-cstack-depth cur-stack)
                  (x862-encoding-cstack-depth dest-stack))))
        (x862-form seg dest-vd dest-cd value)
        (if mv-p
          (progn
            (x862-multiple-value-body seg value)
            (let* ((*x862-returning-values* :pass))
              (x862-nlexit seg dest-cd (%i- *x862-undo-count* (x862-encoding-undo-count dest-stack)))
              (x862-branch seg dest-cd)))
          (progn
            (x862-form 
             seg
             (if need-break (if dest-vd *x862-arg-z*) dest-vd) 
             (if need-break nil dest-cd)
             value)
            (when need-break
              (x862-unwind-set seg dest-cd dest-stack)
              (when dest-vd (x862-copy-register seg dest-vd *x862-arg-z*))
              (x862-branch seg dest-cd))))))
    (x862-unreachable-store)))

(defx862 x862-inherited-arg inherited-arg (seg vreg xfer arg)
  (when vreg
    (x862-addrspec-to-reg seg (x862-ea-open (var-ea arg)) vreg))
  (^))

(defx862 x862-%lisp-word-ref %lisp-word-ref (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (x862-form seg nil nil base)
           (x862-form seg nil xfer offset))
          ((target-arch-case
            (:x8632 (typep fixoffset '(signed-byte 30)))
            (:x8664 (typep fixoffset '(signed-byte 13)))) ;xxx needlessly small
           (ensuring-node-target (target vreg)
             (! lisp-word-ref-c target 
                (x862-one-untargeted-reg-form seg base *x862-arg-z*) 
                (ash fixoffset *x862-target-fixnum-shift*)))
           (^))
          (t (multiple-value-bind (breg oreg)
                                  (x862-two-untargeted-reg-forms seg base *x862-arg-y* offset *x862-arg-z*)
               (ensuring-node-target (target vreg)
                 (! lisp-word-ref target breg oreg))
               (^))))))

(defx862 x862-%fixnum-ref %fixnum-ref (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (x862-form seg nil nil base)
           (x862-form seg nil xfer offset))
          ((typep fixoffset '(signed-byte 16))
           (ensuring-node-target (target vreg)
             (! lisp-word-ref-c target 
                (x862-one-untargeted-reg-form seg base *x862-arg-z*) 
                fixoffset))
           (^))
          (t (multiple-value-bind (breg oreg)
                                  (x862-two-untargeted-reg-forms seg base *x862-arg-y* offset *x862-arg-z*)
               (with-imm-target () (otemp :s32)
                 (! fixnum->signed-natural otemp oreg)
                 (ensuring-node-target (target vreg)
                   (! lisp-word-ref target breg otemp)))
               (^))))))

(defx862 x862-%fixnum-ref-natural %fixnum-ref-natural (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (x862-form seg nil nil base)
           (x862-form seg nil xfer offset))
          ((typep fixoffset '(signed-byte 16))
           (with-imm-target () (val :natural)
             (! lisp-word-ref-c val
                (x862-one-untargeted-reg-form seg base *x862-arg-z*) 
                fixoffset)
             (<- val))
           (^))
          (t (multiple-value-bind (breg oreg)
		 (x862-two-untargeted-reg-forms seg base *x862-arg-y* offset *x862-arg-z*)
               (with-imm-target () (otemp :s32)
                 (! fixnum->signed-natural otemp oreg)
		 (with-imm-target () (val :natural)
		   (! lisp-word-ref val breg otemp)
		   (<- val)))
               (^))))))

(defx862 x862-int>0-p int>0-p (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
    (x862-one-targeted-reg-form seg form ($ *x862-arg-z*))
    (! integer-sign)
    (x862-test-reg-%izerop seg vreg xfer *x862-imm0* cr-bit true-p 0)))


(defx862 x862-throw throw (seg vreg xfer tag valform )
  (declare (ignorable vreg xfer))
  (let* ((*x862-vstack* *x862-vstack*)
         (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
    (x862-vpush-register seg (x862-one-untargeted-reg-form seg tag *x862-arg-z*))
    (if (x862-trivial-p valform)
      (progn
        (x862-vpush-register seg (x862-one-untargeted-reg-form seg valform *x862-arg-z*))
        (x862-set-nargs seg 1))
      (x862-multiple-value-body seg valform))
    (! throw)))

;;; This (and unwind-protect and things like that) are a little funky in that
;;; they have no good way of specifying the exit-point.  The bad way is to
;;; follow the call to the catch-frame-creating subprim with a branch to that
;;; exit-point; the subprim returns to the following instruction.
;;; If the compiler ever gets smart about eliminating dead code, it has to
;;; be careful not to consider the block following the jump to be dead.
;;; Use a vinsn other than JUMP to reference the label.
(defx862 x862-catch catch (seg vreg xfer tag valform)
  (let* ((tag-label (backend-get-next-label))
         (tag-label-value (aref *backend-labels* tag-label))
         (mv-pass (x862-mv-p xfer)))
    (x862-one-targeted-reg-form seg tag ($ *x862-arg-z*))
    (if mv-pass
      (! nmkcatchmv tag-label-value)
      (! nmkcatch1v tag-label-value))
    (x862-open-undo)
    (if mv-pass
      (x862-multiple-value-body seg valform)  
      (x862-one-targeted-reg-form seg valform ($ *x862-arg-z*)))
    (x862-lri seg *x862-imm0* (ash 1 *x862-target-fixnum-shift*))
    (if mv-pass
      (! nthrowvalues tag-label-value)
      (! nthrow1value tag-label-value))
    (x862-close-undo)
    (@= tag-label)
    (unless mv-pass (if vreg (<- *x862-arg-z*)))
    (let* ((*x862-returning-values* mv-pass)) ; nlexit keeps values on stack
      (^))))


(defx862 x862-fixnum-overflow fixnum-overflow (seg vreg xfer form)
  (destructuring-bind (op n0 n1) (acode-unwrapped-form form)
    (x862-use-operator op seg vreg xfer n0 n1 *nx-t*)))

(defx862 x862-%aref2 simple-typed-aref2 (seg vreg xfer typename arr i j &optional dim0 dim1)
  (if (null vreg)
    (progn
      (x862-form seg nil nil arr)
      (x862-form seg nil nil i)
      (x862-form seg nil xfer j)))
  (let* ((type-keyword (x862-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword ))
         (safe (unless *x862-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1)))
    (x862-aref2 seg vreg xfer arr i j safe type-keyword dim0 dim1)))

(defx862 x862-generic-aref2 general-aref2 (seg vreg xfer arr i j)
  (target-arch-case
   (:x8632 (error "not for x8632 yet")))
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (keyword (and atype
                       (let* ((dims (array-ctype-dimensions atype)))
                         (or (eq dims '*)
                             (and (typep dims 'list)
                                  (= 2 (length dims)))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (let* ((dims (array-ctype-dimensions atype))
                  (dim0 (car dims))
                  (dim1 (cadr dims)))
             (x862-aref2 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         (if *x862-reckless*
                           nil
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword ;(make-acode (%nx1-operator immediate) )
                         (if (typep dim0 'fixnum) dim0) (if (typep dim1 'fixnum) dim1))))
          (t
           (x862-three-targeted-reg-forms seg
                                          arr (target-arch-case
					       (:x8632 ($ x8632::temp0))
					       (:x8664 ($ x8664::arg_x)))
                                          i ($ *x862-arg-y*)
                                          j ($ *x862-arg-z*))
           (x862-fixed-call-builtin seg vreg xfer nil (subprim-name->offset '.SParef2))))))

(defx862 x862-%aref3 simple-typed-aref3 (seg vreg xfer typename arr i j k &optional dim0 dim1 dim2)
  (if (null vreg)
    (progn
      (x862-form seg nil nil arr)
      (x862-form seg nil nil i)
      (x862-form seg nil nil j)
      (x862-form seg nil xfer k)))
  (let* ((type-keyword (x862-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword ))
         (safe (unless *x862-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1))
         (dim2 (acode-fixnum-form-p dim2)))
    (x862-aref3 seg vreg xfer arr i j k safe type-keyword dim0 dim1 dim2)))


(defx862 x862-general-aref3 general-aref3 (seg vreg xfer arr i j k)
  (target-arch-case
   (:x8632 (error "not for x8632 yet")))
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (keyword (and atype
                       (let* ((dims (array-ctype-dimensions atype)))
                         (or (eq dims '*)
                             (and (typep dims 'list)
                                  (= 3 (length dims)))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (let* ((dims (array-ctype-dimensions atype))
                  (dim0 (car dims))
                  (dim1 (cadr dims))
                  (dim2 (caddr dims)))
             (x862-aref3 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         k
                         (if *x862-reckless*
                           nil
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword ;(make-acode (%nx1-operator immediate) )
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1)
                         (if (typep dim2 'fixnum) dim2))))
          (t
           (x862-four-targeted-reg-forms seg
                                         arr ($ *x862-temp0*)
                                         i ($ x8664::arg_x)
                                         j ($ *x862-arg-y*)
                                         k ($ *x862-arg-z*))
           (x862-fixed-call-builtin seg vreg xfer nil (subprim-name->offset '.SParef3))))))
                                          
(defx862 x862-general-aset2 general-aset2 (seg vreg xfer arr i j new)
  (target-arch-case
   (:x8632 (error "not for x8632 yet")))
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (keyword (and atype
                       (let* ((dims (array-ctype-dimensions atype)))
                         (or (eq dims '*)
                             (and (typep dims 'list)
                                  (= 2 (length dims)))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (let* ((dims (array-ctype-dimensions atype))
                  (dim0 (car dims))
                  (dim1 (cadr dims)))
             (x862-aset2 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         new
                         (unless *x862-reckless*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1))))
          (t
           (x862-four-targeted-reg-forms seg
                                         arr ($ *x862-temp0*)
                                         i ($ x8664::arg_x)
                                         j ($ *x862-arg-y*)
                                         new ($ *x862-arg-z*))
           (x862-fixed-call-builtin seg vreg xfer nil (subprim-name->offset '.SPaset2))))))

(defx862 x862-general-aset3 general-aset3 (seg vreg xfer arr i j k new)
  (target-arch-case
   (:x8632 (error "not for x8632 yet")))
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (keyword (and atype
                       (let* ((dims (array-ctype-dimensions atype)))
                         (or (eq dims '*)
                             (unless (atom dims)
                               (= 3 (length dims)))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (let* ((dims (array-ctype-dimensions atype))
                  (dim0 (car dims))
                  (dim1 (cadr dims))
                  (dim2 (caddr dims)))
             (x862-aset3 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         k
                         new
                         (unless *x862-reckless*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1)
                         (if (typep dim2 'fixnum) dim2))))
          (t
           (x862-push-register seg (x862-one-untargeted-reg-form seg arr ($ *x862-arg-z*)))
           (x862-four-targeted-reg-forms seg
                                         i ($ *x862-temp0*)
                                         j ($ x8664::arg_x)
                                         k ($ *x862-arg-y*)
                                         new ($ *x862-arg-z*))
           (x862-pop-register seg ($ x8664::temp1))
           (x862-fixed-call-builtin seg vreg xfer nil (subprim-name->offset '.SPaset3))))))


(defx862 x862-%aset2 simple-typed-aset2 (seg vreg xfer typename arr i j new &optional dim0 dim1)
  (let* ((type-keyword (x862-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword))
         (safe (unless *x862-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1)))
    (x862-aset2 seg vreg xfer arr i j new safe type-keyword dim0 dim1)))


(defx862 x862-%aset3 simple-typed-aset3 (seg vreg xfer typename arr i j k new &optional dim0 dim1 dim2)
  (let* ((type-keyword (x862-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword))
         (safe (unless *x862-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1))
         (dim2 (acode-fixnum-form-p dim2)))
    (x862-aset3 seg vreg xfer arr i j k new safe type-keyword dim0 dim1 dim2)))

(defx862 x862-%typed-uvref %typed-uvref (seg vreg xfer subtag uvector index)
  (let* ((type-keyword
          (let* ((fixtype (acode-fixnum-form-p subtag)))
            (if fixtype
              (nx-target-uvector-subtag-name fixtype)
              (x862-immediate-operand subtag)))))
    (if type-keyword
      (x862-vref seg vreg xfer type-keyword uvector index (unless *x862-reckless* (nx-lookup-target-uvector-subtag type-keyword)))
      (progn
        (x862-three-targeted-reg-forms seg
				       subtag (target-arch-case
					       (:x8632 ($ x8632::temp0))
					       (:x8664 ($ x8664::arg_x)))
				       uvector ($ *x862-arg-y*)
				       index ($ *x862-arg-z*))
        (! subtag-misc-ref)
        (when vreg (<- ($ *x862-arg-z*)))
        (^)) )))

(defx862 x862-%typed-uvset %typed-uvset (seg vreg xfer subtag uvector index newval)
  (let* ((type-keyword
          (let* ((fixtype (acode-fixnum-form-p subtag)))
            (if fixtype
              (nx-target-uvector-subtag-name fixtype)
              (x862-immediate-operand subtag)))))
    (if type-keyword
      (x862-vset seg vreg xfer type-keyword uvector index newval (unless *x862-reckless* (nx-lookup-target-uvector-subtag type-keyword)))
      (progn
	(target-arch-case
	 (:x8632
	  (x862-four-targeted-reg-forms seg subtag ($ x8632::temp1) uvector ($ x8632::temp0) index ($ x8632::arg_y) newval ($ x8632::arg_z)))
	 (:x8664
	  (x862-four-targeted-reg-forms seg subtag ($ x8664::temp0) uvector ($ x8664::arg_x) index ($ x8664::arg_y) newval ($ x8664::arg_z))))
        (! subtag-misc-set)
        (when vreg (<- ($ *x862-arg-z*)))
        (^)))))

(defx862 x862-%macptrptr% %macptrptr% (seg vreg xfer form)
  (with-imm-target () (target :address)
    (x862-one-targeted-reg-form seg form (or vreg target)))
  (^))
           

;;; cons a macptr, unless "vreg" is an immediate register of mode :address.
(defx862 x862-%consmacptr% %consmacptr% (seg vreg xfer form)
  (cond ((null vreg) (x862-form seg nil xfer form))
        ((eql (get-regspec-mode vreg) hard-reg-class-gpr-mode-address)
         (x862-form seg vreg xfer form))
        (t         
         (with-imm-target () (temp :address)
           (<- (x862-one-targeted-reg-form seg form temp))
           (^)))))

(defx862 x862-%immediate-ptr-to-int %immediate-ptr-to-int (seg vreg xfer form)
  (if (null vreg)
    (x862-form seg nil xfer form)
    (with-imm-target () (address-reg :address)
      (x862-form seg address-reg nil form)
      (<- (set-regspec-mode address-reg (gpr-mode-name-value :natural)))
      (^))))

(defx862 x862-%immediate-int-to-ptr %immediate-int-to-ptr (seg vreg xfer form)
  (if (null vreg)
    (x862-form seg nil xfer form)
    (progn
      (unless (logbitp (hard-regspec-value vreg) *backend-imm-temps*)
        (compiler-bug "I give up.  When will I get this right ?"))
      (let* ((natural-reg (x862-one-targeted-reg-form seg 
                                                      form
                                                      ($ vreg :mode :natural))))
        (<- natural-reg)
        (^)))))


(defx862 x862-%function %function (seg vreg xfer sym)
  (when vreg
    (let* ((symreg (x862-one-untargeted-reg-form seg (make-acode (%nx1-operator immediate)
                                                                 (x862-symbol-entry-locative sym)) *x862-arg-z*)))
      (with-node-temps (vreg symreg) (val)
        (! symbol-function val symreg)
        (<- val))))
  (^))

(defx862 x862-%unbound-marker %unbound-marker (seg vreg xfer)
  (when vreg       
    (ensuring-node-target (target vreg)
      (x862-lri seg target (target-arch-case
                            (:x8632 x8632::unbound-marker)
                            (:x8664 x8664::unbound-marker)))))
  (^))

(defx862 x862-slot-unbound-marker %slot-unbound-marker (seg vreg xfer)
  (when vreg    
    (ensuring-node-target (target vreg)
      (x862-lri seg target (target-arch-case
			    (:x8632 x8632::slot-unbound-marker)
                            (:x8664 x8664::slot-unbound-marker)))))
  (^))

(defx862 x862-illegal-marker %illegal-marker (seg vreg xfer)
  (when vreg    
    (ensuring-node-target (target vreg)
      (x862-lri seg target (target-arch-case
			    (:x8632 x8632::illegal-marker)
                            (:x8664 x8664::illegal-marker)))))
  (^))

(defx862 x862-lambda-bind lambda-bind (seg vreg xfer vals req rest keys-p auxen body p2decls)
  (let* ((old-stack (x862-encode-stack))
         (nreq (list-length req))
         (rest-arg (nthcdr nreq vals))
         (apply-body (x862-eliminate-&rest body rest keys-p auxen rest-arg)))
    (x862-seq-bind seg req vals)
    (when apply-body (setq rest nil body apply-body))
    (let*
      ((vloc *x862-vstack*)
       (restloc vloc)
       (nvloc (progn (if (or rest keys-p) (x862-formlist seg rest-arg)) *x862-vstack*)))
      (with-x86-p2-declarations p2decls
        (when rest
          (when keys-p
            (until (eq restloc nvloc)
              (with-node-temps () (temp)
                (x862-stack-to-register seg (x862-vloc-ea restloc) temp)
                (x862-vpush-register seg temp))
              (setq restloc (%i+ restloc *x862-target-node-size*))))
          (x862-set-nargs seg (length rest-arg))
          (x862-set-vstack restloc)
          (if (%ilogbitp $vbitdynamicextent (nx-var-bits rest))
            (progn
              (! stack-cons-list)
              (x862-open-undo $undostkblk))
            (! list))
          (x862-vpush-register seg *x862-arg-z*))
        (when rest (x862-bind-var seg rest restloc))
        (destructuring-bind (vars inits) auxen
          (while vars
            (let ((val (%car inits))) 
              (if (fixnump val)
                (progn
                  (when rest (setq val (%i+ (%i+ val val) 1)))
                  (x862-bind-var seg (%car vars) (%i+ vloc (* val *x862-target-node-size*))))
                (x862-seq-bind-var seg (%car vars) val)))
            (setq vars (%cdr vars) inits (%cdr inits))))
        (x862-undo-body seg vreg xfer body old-stack)
        (dolist (var req) (x862-close-var seg var))
        (when rest (x862-close-var seg rest))
        (dolist (var (%car auxen)) (x862-close-var seg var))))))

(macrolet 
  ((def-x862-require (function op &optional (vinsn op))
     `(defx862 ,function ,op (seg vreg xfer val)
        (let* ((val-reg (x862-one-untargeted-reg-form 
                         seg 
                         val 
                         (if (eq vreg *x862-arg-z*) *x862-arg-y* *x862-arg-z*))))
          (! ,vinsn val-reg)
          (when vreg (<- val-reg))
          (^)))))
  (def-x862-require x862-require-simple-vector require-simple-vector)
  (def-x862-require x862-require-simple-string require-simple-string)
  (def-x862-require x862-require-integer require-integer)
  (def-x862-require x862-require-fixnum require-fixnum)
  (def-x862-require x862-require-real require-real)
  (def-x862-require x862-require-list require-list)
  (def-x862-require x862-require-character require-character)
  (def-x862-require x862-require-number require-number)
  (def-x862-require x862-require-symbol require-symbol)
  (def-x862-require x862-require-s8 require-s8)
  (def-x862-require x862-require-s8 require-u8)
  (def-x862-require x862-require-s8 require-s16)
  (def-x862-require x862-require-s8 require-u16)
  (def-x862-require x862-require-s8 require-s32)
  (def-x862-require x862-require-s8 require-u32)
  (def-x862-require x862-require-s8 require-s64)
  (def-x862-require x862-require-s8 require-u64))

(defun x862-typechecked-form (seg vreg xfer typespec form)
  (with-x86-local-vinsn-macros (seg vreg xfer)
    (let* ((op
            (cond ((eq typespec 'fixnum) (%nx1-operator require-fixnum))
                  ((eq typespec 'integer) (%nx1-operator require-integer))
                  ((memq typespec '(base-char character))
                   (%nx1-operator require-character))
                  ((eq typespec 'symbol) (%nx1-operator require-symbol))
                  ((eq typespec 'list) (%nx1-operator require-list))
                  ((eq typespec 'real) (%nx1-operator require-real))
                  ((memq typespec '(simple-base-string simple-string))
                   (%nx1-operator require-simple-string))
                  ((eq typespec 'number) (%nx1-operator require-number))
                  ((eq typespec 'simple-vector) (%nx1-operator require-simple-vector))
                  (t
                   (let* ((ctype (specifier-type typespec)))
                     (cond ((type= ctype (load-time-value (specifier-type '(signed-byte 8))))
                            (%nx1-operator require-s8))
                           ((type= ctype (load-time-value (specifier-type '(unsigned-byte 8))))
                            (%nx1-operator require-u8))
                           ((type= ctype (load-time-value (specifier-type '(signed-byte 16))))
                            (%nx1-operator require-s16))
                           ((type= ctype (load-time-value (specifier-type '(unsigned-byte 16))))
                            (%nx1-operator require-u16))
                           ((type= ctype (load-time-value (specifier-type '(signed-byte 32))))                            
                            (%nx1-operator require-s32))
                           ((type= ctype (load-time-value (specifier-type '(unsigned-byte 32))))
                            (%nx1-operator require-u32))
                           ((type= ctype (load-time-value (specifier-type '(signed-byte 64))))
                            (%nx1-operator require-s64))
                           ((type= ctype (load-time-value (specifier-type '(unsigned-byte 64))))
                            (%nx1-operator require-u64))))))))
      (if op
        (x862-use-operator op seg vreg xfer form)
        (if (or (eq typespec t)
                (eq typespec '*))
          (x862-form seg vreg xfer form)
          (let* ((ok (backend-get-next-label)))
            (x862-one-targeted-reg-form seg form ($ *x862-arg-y*))
            (x862-store-immediate seg typespec ($ *x862-arg-z*))
            (x862-store-immediate seg 'typep ($ *x862-fname*))
            (x862-set-nargs seg 2)
            (x862-vpush-register seg ($ *x862-arg-y*))
            (! call-known-symbol ($ *x862-arg-z*))
            (! compare-to-nil ($ *x862-arg-z*))
            (x862-vpop-register seg ($ *x862-arg-y*))
            (! cbranch-false (aref *backend-labels* ok) x86::x86-e-bits)
	    (target-arch-case
	     (:x8632
	      (let* ((*x862-vstack* *x862-vstack*)
		     (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
		(! reserve-outgoing-frame)
		(incf *x862-vstack* (* 2 *x862-target-node-size*))
		(! vpush-fixnum (ash $XWRONGTYPE *x862-target-fixnum-shift*))
		(x862-store-immediate seg typespec ($ *x862-arg-z*))
		(x862-set-nargs seg 3)
		(! ksignalerr)))
	     (:x8664
	      (x862-lri seg ($ x8664::arg_x) (ash $XWRONGTYPE *x862-target-fixnum-shift*))
	      (x862-store-immediate seg typespec ($ *x862-arg-z*))
	      (x862-set-nargs seg 3)
	      (! ksignalerr)))
            (@ ok)
            (<- ($ *x862-arg-y*))
            (^)))))))
          
          
                  
                  
                   

(defx862 x862-%badarg2 %badarg2 (seg vreg xfer badthing goodthing)
  (x862-two-targeted-reg-forms seg badthing ($ *x862-arg-y*) goodthing ($ *x862-arg-z*))
  (target-arch-case
   (:x8632
    (let* ((*x862-vstack* *x862-vstack*)
	   (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
      (! reserve-outgoing-frame)
      (incf *x862-vstack* (* 2 *x862-target-node-size*))
      (! vpush-fixnum (ash $XWRONGTYPE *x862-target-fixnum-shift*))
      (x862-set-nargs seg 3)
      (! ksignalerr))
    (<- nil)
    (^))
   (:x8664
    (x862-lri seg ($ x8664::arg_x) (ash $XWRONGTYPE *x862-target-fixnum-shift*))
    (x862-set-nargs seg 3)
    (! ksignalerr)
    (<- nil)
    (^))))
          
(defx862 x862-%set-sbchar %set-sbchar (seg vreg xfer string index value)
  (x862-vset 
   seg 
   vreg 
   xfer 
   :simple-string 
   string 
   index
   value 
   (unless *x862-reckless* (nx-lookup-target-uvector-subtag :simple-string))))


;;; If we didn't use this for stack consing, turn it into a call.  Ugh.

(defx862 x862-make-list make-list (seg vreg xfer size initial-element)
  (let* ((args (list size
		     (make-acode (%nx1-operator immediate) :initial-element)
		     initial-element)))
    (x862-form seg vreg xfer
	       (make-acode (%nx1-operator call)
			   (make-acode (%nx1-operator immediate) 'make-list)
			   (if (<= (length args) *x862-target-num-arg-regs*)
			     (list nil (reverse args))
			     (list (butlast args *x862-target-num-arg-regs*)
				   (reverse (last args *x862-target-num-arg-regs*))))))))

(defx862 x862-setq-free setq-free (seg vreg xfer sym val)
  (let* ((rsym ($ *x862-arg-y*))
         (rval ($ *x862-arg-z*)))
    (x862-one-targeted-reg-form seg val rval)
    (x862-immediate seg rsym nil (x862-symbol-value-cell sym))
    (! setqsym)
    (<- rval)
    (^)))

(defx862 x862-%setf-macptr %setf-macptr (seg vreg xfer x y)
  (x862-vpush-register seg (x862-one-untargeted-reg-form seg x *x862-arg-z*))
  (with-imm-target () (src-reg :address)
    (x862-one-targeted-reg-form seg y src-reg)
    (x862-vpop-register seg *x862-arg-z*)
    (unless (or *x862-reckless* (x862-form-typep x 'macptr))
      (with-additional-imm-reg (*x862-arg-z*)
	(with-imm-temps (src-reg) ()
	  (! trap-unless-macptr *x862-arg-z*))))
    (! set-macptr-address src-reg *x862-arg-z*)
    (<- *x862-arg-z*)
    (^)))

;; used for x8632 only
(defx862 x862-%setf-short-float %setf-short-float (seg vref xfer fnode fval)
  (target-arch-case
   (:x8664 (error "%setf-short-float makes no sense on x8664")))
  (x862-vpush-register seg (x862-one-untargeted-reg-form seg fnode *x862-arg-z*))
  (let* ((target ($ *x862-fp1* :class :fpr :mode :single-float))
         (node ($ *x862-arg-z*)))
    (x862-one-targeted-reg-form seg fval target)
    (x862-vpop-register seg node)
    (unless (or *x862-reckless* (x862-form-typep fnode 'single-float))
      (! trap-unless-single-float node))
    (! store-single node target)
    (<- node)
    (^)))

(defx862 x862-%setf-double-float %setf-double-float (seg vref xfer fnode fval)
  (x862-vpush-register seg (x862-one-untargeted-reg-form seg fnode *x862-arg-z*))
  (let* ((target ($ *x862-fp1* :class :fpr :mode :double-float))
         (node ($ *x862-arg-z*)))
    (x862-one-targeted-reg-form seg fval target)
    (x862-vpop-register seg node)
    (unless (or *x862-reckless* (x862-form-typep fnode 'double-float))
      (! trap-unless-double-float node))
    (! store-double node target)
    (<- node)
    (^)))

    

(defx862 x862-unwind-protect unwind-protect (seg vreg xfer protected-form cleanup-form)
  (let* ((cleanup-label (backend-get-next-label))
         (protform-label (backend-get-next-label))
         (old-stack (x862-encode-stack))
         (ilevel '*interrupt-level*))
    (! nmkunwind
       (aref *backend-labels* protform-label)
       (aref *backend-labels* cleanup-label))
    (x862-open-undo $undointerruptlevel)
    (x862-new-vstack-lcell :special-value *x862-target-lcell-size* 0 ilevel)
    (x862-new-vstack-lcell :special *x862-target-lcell-size* (ash 1 $vbitspecial) ilevel)
    (x862-new-vstack-lcell :special-link *x862-target-lcell-size* 0 ilevel)
    (x862-adjust-vstack (* 3 *x862-target-node-size*))    
    (@= cleanup-label)
    (let* ((*x862-vstack* *x862-vstack*)
           (*x862-top-vstack-lcell* *x862-top-vstack-lcell*))
      (x862-open-undo $undostkblk)      ; tsp frame created by nthrow.
      (x862-new-vstack-lcell :cleanup-return *x862-target-lcell-size* 0 nil)
      (x862-adjust-vstack *x862-target-node-size*)      
      (x862-form seg nil nil cleanup-form)
      (x862-close-undo)
      (! jump-return-pc))
    (x862-open-undo)
    (@=  protform-label)
    (x862-open-undo $undointerruptlevel)
    (x862-new-vstack-lcell :special-value *x862-target-lcell-size* 0 ilevel)
    (x862-new-vstack-lcell :special *x862-target-lcell-size* (ash 1 $vbitspecial) ilevel)
    (x862-new-vstack-lcell :special-link *x862-target-lcell-size* 0 ilevel)
    (x862-adjust-vstack (* 3 *x862-target-node-size*))
    (x862-undo-body seg vreg xfer protected-form old-stack)))

(defx862 x862-progv progv (seg vreg xfer symbols values body)
  (let* ((cleanup-label (backend-get-next-label))
         (protform-label (backend-get-next-label))
         (old-stack (x862-encode-stack)))
    (x862-two-targeted-reg-forms seg symbols ($ *x862-arg-y*) values ($ *x862-arg-z*))
    (! progvsave)
    (x862-open-undo $undostkblk)
    (! mkunwind
       (aref *backend-labels* protform-label)
       (aref *backend-labels* cleanup-label))
    (@= cleanup-label)
    (! progvrestore)
    (x862-open-undo)
    (@= protform-label)
    (x862-undo-body seg vreg xfer body old-stack)))

(defx862 x862-%ptr-eql %ptr-eql (seg vreg xfer cc x y )
  (if (null vreg)
    (progn
      (x862-form seg nil nil x)
      (x862-form seg nil xfer y))
    (let* ((x-abs (acode-absolute-ptr-p x t))
           (y-abs (acode-absolute-ptr-p y t))
           (abs (or x-abs y-abs))
           (other (if abs (if x-abs y x))))
      (multiple-value-bind (cr-bit true-p) (acode-condition-to-x86-cr-bit cc)
        (if other
          (with-imm-target () (other-target :address)
            (x862-one-targeted-reg-form seg other other-target)
            (if (typep abs '(signed-byte 16))              
              (x862-test-reg-%izerop seg vreg xfer other-target cr-bit true-p abs)
	      (with-additional-imm-reg ()
		(with-imm-temps (other-target) ((abs-target :address))
		  (use-imm-temp other-target)
		  (x862-lri seg abs-target abs)
		  (x862-compare-registers seg vreg xfer other-target abs-target cr-bit true-p)))))
          ;; Neither expression is obviously a constant-valued macptr.
          (with-imm-target () (target-a :address)
            (x862-one-targeted-reg-form seg x target-a)
            (! temp-push-unboxed-word target-a)
            (x862-open-undo $undostkblk)
            (x862-one-targeted-reg-form seg y target-a)
	    (with-additional-imm-reg ()
	      (with-imm-target (target-a) (target-b :address)
		(! temp-pop-unboxed-word target-b)
		(x862-close-undo)
		(x862-compare-registers seg vreg xfer target-b target-a cr-bit true-p)))))))))

(defx862 x862-set-bit %set-bit (seg vreg xfer ptr offset newval)
  (let* ((offval (acode-fixnum-form-p offset))
         (constval (acode-fixnum-form-p newval)))
      (if (typep offval '(signed-byte 32))
        (with-imm-target () (src :address)
          (x862-one-targeted-reg-form seg ptr src)
          (if constval
            (progn
              (if (eql constval 0)
                (! mem-set-c-bit-0 src offval)
                (! mem-set-c-bit-1 src offval))
              (when vreg
                (x862-form seg vreg nil newval)))
            (with-imm-target () (src :address)
              (x862-two-targeted-reg-forms seg ptr src newval ($ *x862-arg-z*))
              (! mem-set-c-bit-variable-value src offval ($ *x862-arg-z*))
              (<- ($ *x862-arg-z*)))))
        (if constval
          (with-imm-target () (src :address)
            (x862-two-targeted-reg-forms seg ptr src offset ($ *x862-arg-z*))
            (if (eql constval 0)
              (! mem-set-bit-0 src ($ *x862-arg-z*))
              (! mem-set-bit-1 src ($ *x862-arg-z*)))
            (when vreg
              (x862-form seg vreg nil newval)))
          (with-imm-target () (src :address)
            (x862-three-targeted-reg-forms seg ptr src offset ($ *x862-arg-y*) newval ($ *x862-arg-z*))
            (! mem-set-bit-variable-value src ($ *x862-arg-y*) ($ *x862-arg-z*))
            (<- ($ *x862-arg-z*)))))
      (^)))

(defx862 x862-%immediate-set-xxx %immediate-set-xxx (seg vreg xfer bits ptr offset val)
  (x862-%immediate-store seg vreg xfer bits ptr offset val))



(defx862 x862-%immediate-inc-ptr %immediate-inc-ptr (seg vreg xfer ptr by)
  (let* ((triv-by (x862-trivial-p by))
         (fixnum-by (acode-fixnum-form-p by)))
    (if (and fixnum-by (eql 0 fixnum-by))
      (x862-form seg vreg xfer ptr)
      (let* ((ptr-reg (with-imm-target () (ptr-reg :address)
                        (x862-one-targeted-reg-form seg ptr ptr-reg))))
        (if fixnum-by
          (let* ((result ptr-reg))
            (! add-constant result fixnum-by)
            (<- result))
            (progn
              (unless triv-by
                (x862-push-register seg ptr-reg))
              (let* ((boxed-by (x862-one-targeted-reg-form seg by *x862-arg-z*)))
                (unless triv-by
                  (x862-pop-register seg ptr-reg))
		(with-additional-imm-reg ()
		  (with-imm-target (ptr-reg) (by-reg :signed-natural)
		    (! fixnum->signed-natural by-reg boxed-by)
		    (let* ((result ptr-reg))
		      (! fixnum-add2 result by-reg)
		      (<- result)))))))
        (^)))))



(defx862 x862-multiple-value-call multiple-value-call (seg vreg xfer fn arglist)
  (x862-mvcall seg vreg xfer fn arglist))

(defx862 x862-i386-syscall i386-syscall (seg vreg xfer idx argspecs argvals resultspec &optional monitor-exception-ports)
  (declare (ignore monitor-exception-ports))
  (let* ((*x862-vstack* *x862-vstack*)
	 (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
	 (*x862-cstack* *x862-cstack*)
	 (offset 0)
	 (nwords 0))
    (dolist (argspec argspecs)
      (case argspec
	((:unsigned-doubleword :signed-doubleword)
	 (incf nwords 2))
	(t (incf nwords))))
    (! alloc-c-frame nwords)
    (x862-open-undo $undo-x86-c-frame)
    (x862-vpush-register seg (x862-one-untargeted-reg-form seg idx x8632::arg_z))
    ;; Evaluate each form into the C frame, according to the
    ;; matching argspec.
    (do* ((specs argspecs (cdr specs))
	  (vals argvals (cdr vals)))
	 ((null specs))
      (declare (list specs vals))
      (let* ((valform (car vals))
	     (spec (car specs))
	     (absptr (acode-absolute-ptr-p valform)))
	(case spec
	  ((:unsigned-doubleword :signed-doubleword)
	   (x862-one-targeted-reg-form seg valform ($ x8632::arg_z))
	   (if (eq spec :signed-doubleword)
	     (! gets64)
	     (! getu64))
	   (! set-c-arg-from-mm0 offset)
	   (incf offset 2))
	  (:address
	   (with-imm-target () (ptr :address)
	     (if absptr
	       (x862-lri seg ptr absptr)
	       (x862-form seg ptr nil valform))
	     (! set-c-arg ptr offset))
	   (incf offset))
	  (t
	   (with-imm-target () (valreg :natural)
	     (let* ((reg (x862-unboxed-integer-arg-to-reg seg valform valreg spec)))
	       (! set-c-arg reg offset)
	       (incf offset)))))))
    (x862-vpop-register seg ($ x8632::arg_z))
    (case resultspec
      ((:unsigned-doubleword :signed-doubleword)
       (! syscall2))			;copies doubleword result into %mm0
      (t
       (! syscall)))
    (x862-close-undo)
    (when vreg
      (cond ((eq resultspec :void) (<- nil))
	    ((eq resultspec :unsigned-doubleword)
	     (ensuring-node-target (target vreg)
	       (! makeu64)
	       (x862-copy-register seg target ($ *x862-arg-z*))))
	    ((eq resultspec :signed-doubleword)
	     (ensuring-node-target (target vreg)
	       (! makes64)
	       (x862-copy-register seg target ($ *x862-arg-z*))))
	    (t
	     (case resultspec
	       (:signed-byte (! sign-extend-s8 *x862-imm0* *x862-imm0*))
	       (:signed-halfword (! sign-extend-s16 *x862-imm0* *x862-imm0*))
	       (:unsigned-byte (! zero-extend-u8 *x862-imm0* *x862-imm0*))
	       (:unsigned-halfword (! zero-extend-u16 *x862-imm0* *x862-imm0*)))
	     (<- (make-wired-lreg x8632::imm0
				  :mode
				  (gpr-mode-name-value
				   (case resultspec
				     (:address :address)
				     (:signed-byte :s8)
				     (:unsigned-byte :u8)
				     (:signed-halfword :s16)
				     (:unsigned-halfword :u16)
				     (:signed-fullword :s32)
				     (t :u32))))))))
    (^)))


(defx862 x862-syscall syscall (seg vreg xfer idx argspecs argvals resultspec &optional monitor-exception-ports)
  (declare (ignore monitor-exception-ports))
  (let* ((*x862-vstack* *x862-vstack*)
         (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
         (*x862-cstack* *x862-cstack*)
         (gpr-offset 0)
         (other-offset 6)
         (nother-words 0)
         (ngpr-args 0)
         (simple-foreign-args nil))
      (declare (fixnum  ngpr-args nother-words
                        gpr-offset other-offset))
      (dolist (argspec argspecs)
        (declare (ignorable argspec))
        (incf ngpr-args)
        (if (> ngpr-args 6)
          (incf nother-words)))
      (let* ((total-words nother-words))
        (when (zerop total-words)
          (setq simple-foreign-args nil))
        (! alloc-c-frame total-words))
      (x862-open-undo $undo-x86-c-frame)
      (setq ngpr-args 0)
      (unless simple-foreign-args
        (x862-vpush-register seg (x862-one-untargeted-reg-form seg idx *x862-arg-z*)))
      ;; Evaluate each form into the C frame, according to the
      ;; matching argspec.
      (do* ((specs argspecs (cdr specs))
            (vals argvals (cdr vals)))
           ((null specs))
        (declare (list specs vals))
        (let* ((valform (car vals))
               (spec (car specs))
               (absptr (acode-absolute-ptr-p valform)))
          (case spec
            (:address
             (with-imm-target () (ptr :address)
               (if absptr
                 (x862-lri seg ptr absptr)
                 (x862-form seg ptr nil valform))
               (incf ngpr-args)
               (cond ((<= ngpr-args 6)
                      (! set-c-arg ptr gpr-offset)
                      (incf gpr-offset))
                     (t
                      (! set-c-arg ptr other-offset)
                      (incf other-offset)))))
            (t
             (with-imm-target () (valreg :natural)
                (let* ((reg (x862-unboxed-integer-arg-to-reg seg valform valreg spec)))
                  (incf ngpr-args)
                  (cond ((<= ngpr-args 8)
                         (! set-c-arg reg gpr-offset)
                         (incf gpr-offset))
                        (t
                         (! set-c-arg reg other-offset)
                         (incf other-offset)))))))))      
      (unless simple-foreign-args
        (x862-vpop-register seg ($ *x862-arg-z*)))
      (! syscall) 
      (x862-close-undo)
      (when vreg
        (cond ((eq resultspec :void) (<- nil))
              ((eq resultspec :unsigned-doubleword)
               (ensuring-node-target (target vreg)
                 (! makeu64)
                 (x862-copy-register seg target ($ *x862-arg-z*))))
              ((eq resultspec :signed-doubleword)
               (ensuring-node-target (target vreg)
                 (! makes64)
                 (x862-copy-register seg target ($ *x862-arg-z*))))
              (t
               (case resultspec
                 (:signed-byte (! sign-extend-s8 *x862-imm0* *x862-imm0*))
                 (:signed-halfword (! sign-extend-s16 *x862-imm0* *x862-imm0*))
                 (:signed-fullword (! sign-extend-s32 *x862-imm0* *x862-imm0*))
                 (:unsigned-byte (! zero-extend-u8 *x862-imm0* *x862-imm0*))
                 (:unsigned-halfword (! zero-extend-u16 *x862-imm0* *x862-imm0*))
                 (:unsigned-fullword (! zero-extend-u32 *x862-imm0* *x862-imm0*)))               
               (<- (make-wired-lreg *x862-imm0*
                                    :mode
                                    (gpr-mode-name-value
                                     (case resultspec
                                       (:address :address)
                                       (:signed-byte :s8)
                                       (:unsigned-byte :u8)
                                       (:signed-halfword :s16)
                                       (:unsigned-halfword :u16)
                                       (:signed-fullword :s32)
                                       (t :u32))))))))
      (^)))

(defx862 x862-i386-ff-call i386-ff-call (seg vreg xfer address argspecs argvals resultspec &optional monitor)
  (declare (ignore monitor))
  #+debug
  (format t "~&~%i386-ff-call: argspecs = ~s, argvals = ~s, resultspec = ~s"
	  argspecs argvals resultspec)
  (let* ((*x862-vstack* *x862-vstack*)
         (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
         (*x862-cstack* *x862-cstack*)
	 (offset 0)
	 (simple-foreign-args nil)
	 (nwords 0))
    (dolist (argspec argspecs)
      (case argspec
	((:double-float :unsigned-doubleword :signed-doubleword)
	 (incf nwords 2))
	(t (incf nwords))))
    (when (null argspecs)
      (setq simple-foreign-args t))
    (! alloc-c-frame nwords)
    (x862-open-undo $undo-x86-c-frame)
    (unless simple-foreign-args
      (x862-vpush-register seg (x862-one-untargeted-reg-form seg address x8632::arg_z)))
    ;; Evaluate each form into the C frame, according to the
    ;; matching argspec.
    (do* ((specs argspecs (cdr specs))
	  (vals argvals (cdr vals)))
	 ((null specs))
      (declare (list specs vals))
      (let* ((valform (car vals))
	     (spec (car specs))
	     (absptr (acode-absolute-ptr-p valform)))
	(case spec
	  (:registers
	   (error "don't know what to do with argspec ~s" spec))
	  (:double-float
	   (let* ((df ($ x8632::fp0 :class :fpr :mode :double-float)))
	     (x862-one-targeted-reg-form seg valform df)
	     (! set-double-c-arg df offset))
	   (incf offset 2))
	  (:single-float
	   (let* ((sf ($ x8632::fp0 :class :fpr :mode :single-float)))
	     (x862-one-targeted-reg-form seg valform sf)
	     (! set-single-c-arg sf offset))
	   (incf offset))
	  (:address
	   (with-imm-target () (ptr :address)
	     (if absptr
	       (x862-lri seg ptr absptr)
	       (x862-form seg ptr nil valform))
	     (! set-c-arg ptr offset))
	   (incf offset))
          ((:signed-doubleword :unsigned-doubleword)
           (x862-one-targeted-reg-form seg valform x8632::arg_z)
           ;; Subprims return 64-bit result in mm0
           (if (eq spec :unsigned-doubleword)
             (! getu64)
             (! gets64))
           (! set-c-arg-from-mm0 offset)
           (incf offset 2))
	  (t
	   (if (typep spec 'unsigned-byte)
	     (progn
	       (with-imm-target () (ptr :address)
		 (x862-one-targeted-reg-form seg valform ptr)
		 (with-additional-imm-reg (ptr)
		   (with-imm-target (ptr) (r :natural)
		     (dotimes (i spec)
		       (! mem-ref-c-fullword r ptr (ash i x8632::word-shift))
		       (! set-c-arg r offset)))))
	       (incf offset spec))
	     (with-imm-target () (valreg :natural)
	       (let* ((reg (x862-unboxed-integer-arg-to-reg seg valform valreg spec)))
		 (! set-c-arg reg offset)
		 (incf offset))))))))
    (if simple-foreign-args
      (x862-one-targeted-reg-form seg address x8632::arg_z)
      (x862-vpop-register seg ($ x8632::arg_z)))
    (! ff-call)
    (x862-close-undo)
    (when vreg
      (cond ((eq resultspec :void) (<- nil))
	    ;; Floating-point results are returned on the x87 stack.
	    ((eq resultspec :double-float)
	     (let ((fpreg ($ x8632::fp0 :class :fpr :mode :double-float)))
	       (! fp-stack-to-double fpreg)
	       (<- fpreg)))
	    ((eq resultspec :single-float)
	     (let ((fpreg ($ x8632::fp0 :class :fpr :mode :single-float)))
	       (! fp-stack-to-single fpreg)
	       (<- fpreg)))
	    ((eq resultspec :unsigned-doubleword)
	     (ensuring-node-target (target vreg)
               (! get-64-bit-ffcall-result)
	       (! makeu64)
	       (x862-copy-register seg target ($ *x862-arg-z*))))
	    ((eq resultspec :signed-doubleword)
	     (ensuring-node-target (target vreg)
               (! get-64-bit-ffcall-result)
	       (! makes64)
	       (x862-copy-register seg target ($ *x862-arg-z*))))
	    (t
	     (case resultspec
	       (:signed-byte (! sign-extend-s8 *x862-imm0* *x862-imm0*))
	       (:signed-halfword (! sign-extend-s16 *x862-imm0* *x862-imm0*))
	       (:unsigned-byte (! zero-extend-u8 *x862-imm0* *x862-imm0*))
	       (:unsigned-halfword (! zero-extend-u16 *x862-imm0* *x862-imm0*)))
	     (<- (make-wired-lreg x8632::imm0
				  :mode
				  (gpr-mode-name-value
				   (case resultspec
				     (:address :address)
				     (:signed-byte :s8)
				     (:unsigned-byte :u8)
				     (:signed-halfword :s16)
				     (:unsigned-halfword :u16)
				     (:signed-fullword :s32)
				     (t :u32))))))))
    (^)))

(defx862 x862-ff-call ff-call (seg vreg xfer address argspecs argvals resultspec &optional monitor)
  (declare (ignore monitor))
  (let* ((*x862-vstack* *x862-vstack*)
         (*x862-top-vstack-lcell* *x862-top-vstack-lcell*)
         (*x862-cstack* *x862-cstack*)
         (gpr-offset 0)
         (other-offset 6)
         (single-float-offset 6)
         (double-float-offset 6)
         (nsingle-floats 0)              ; F
         (ndouble-floats 0)             ; D
         (nother-words 0)
         (nfpr-args 0)
         (ngpr-args 0)
         (simple-foreign-args nil)
         (fp-loads ())
         (return-registers ()))
      (declare (fixnum  nsingle-floats ndouble-floats nfpr-args ngpr-args nother-words
                        gpr-offset other-offset single-float-offset double-float-offset))
      (dolist (argspec argspecs)
        (case argspec
          (:double-float (incf nfpr-args)
                         (if (<= nfpr-args 8)
                           (incf ndouble-floats)
                           (incf nother-words)))
          (:single-float (incf nfpr-args)
                         (if (<= nfpr-args 8)
                           (incf nsingle-floats)
                           (incf nother-words)))
          (:registers (setq return-registers t))
          (t
           (if (typep argspec 'unsigned-byte)
             (incf nother-words argspec)
             (progn
               (incf ngpr-args)
               (if (> ngpr-args 6)
                 (incf nother-words)))))))
      (let* ((total-words (+ nother-words nsingle-floats ndouble-floats)))
        (when (null argspecs)
          (setq simple-foreign-args t))
        (! alloc-c-frame total-words))
      (x862-open-undo $undo-x86-c-frame)
      (setq single-float-offset (+ other-offset nother-words))
      (setq double-float-offset
            (+ single-float-offset nsingle-floats))
      (setq ngpr-args 0 nfpr-args 0)
      (unless simple-foreign-args
        (x862-vpush-register seg (x862-one-untargeted-reg-form seg address x8664::arg_z)))
      ;; Evaluate each form into the C frame, according to the
      ;; matching argspec.  Remember type and arg offset of any FP
      ;; args, since FP regs will have to be loaded later.
      (do* ((specs argspecs (cdr specs))
            (vals argvals (cdr vals)))
           ((null specs))
        (declare (list specs vals))
        (let* ((valform (car vals))
               (spec (car specs))
               (absptr (acode-absolute-ptr-p valform)))
          (case spec
            (:registers
             (let* ((reg (x862-one-untargeted-reg-form seg valform x8664::arg_z)))
               (unless *x862-reckless*
                 (! trap-unless-macptr reg))
               (x862-vpush-register seg reg)))
            (:double-float
             (let* ((df ($ x8664::fp1 :class :fpr :mode :double-float)))
               (incf nfpr-args)
               (x862-one-targeted-reg-form seg valform df )
               (cond ((<= nfpr-args 8)
                      (! set-double-c-arg df double-float-offset)
                      (push (cons :double-float double-float-offset) fp-loads)
                      (incf double-float-offset))
                     (t
                      (! set-double-c-arg df other-offset)
                      (incf other-offset)))))
            (:single-float
             (let* ((sf ($ x8664::fp1 :class :fpr :mode :single-float)))
               (incf nfpr-args)
               (x862-one-targeted-reg-form
                seg valform sf)
               (cond ((<= nfpr-args 8)
                      (! set-single-c-arg sf single-float-offset)
                      (push (cons :single-float single-float-offset) fp-loads)
                      (incf single-float-offset))
                     (t
                      (! set-single-c-arg sf other-offset)
                      (incf other-offset)))))            
            (:address
             (with-imm-target () (ptr :address)
               (if absptr
                 (x862-lri seg ptr absptr)
                 (x862-form seg ptr nil valform))
               (incf ngpr-args)
               (cond ((<= ngpr-args 6)
                      (! set-c-arg ptr gpr-offset)
                      (incf gpr-offset))
                     (t
                      (! set-c-arg ptr other-offset)
                      (incf other-offset)))))
            (t
             (if (typep spec 'unsigned-byte)
               (progn
                 (with-imm-target () (ptr :address)
                   (x862-one-targeted-reg-form seg valform ptr)
                   (with-imm-target (ptr) (r :natural)
                     (dotimes (i spec)
                       (! mem-ref-c-doubleword r ptr (ash i x8664::word-shift))
                       (! set-c-arg r other-offset)
                       (incf other-offset)))))               
               (with-imm-target () (valreg :natural)
                 (let* ((reg (x862-unboxed-integer-arg-to-reg seg valform valreg spec)))
                   (incf ngpr-args)
                   (cond ((<= ngpr-args 6)
                          (! set-c-arg reg gpr-offset)
                          (incf gpr-offset))
                         (t
                          (! set-c-arg reg other-offset)
                          (incf other-offset))))))))))
      (do* ((fpreg x8664::fp0 (1+ fpreg))
            (reloads (nreverse fp-loads) (cdr reloads)))
           ((or (null reloads) (= fpreg x8664::fp8)))
        (declare (list reloads) (fixnum fpreg))
        (let* ((reload (car reloads))
               (size (car reload))
               (from (cdr reload)))
          (if (eq size :double-float)
            (! reload-double-c-arg ($ fpreg :class :fpr :mode :double-float) from)
            (! reload-single-c-arg ($ fpreg :class :fpr :mode :single-float) from))))
      (if return-registers
        (x862-vpop-register seg ($ x8664::arg_y)))
      (if simple-foreign-args
        (x862-one-targeted-reg-form seg address x8664::arg_z)
        (x862-vpop-register seg ($ x8664::arg_z)))
      (x862-lri seg x8664::rax (min 8 nfpr-args))
      (if return-registers
        (! ff-call-return-registers)
        (! ff-call) )
      (x862-close-undo)
      (when vreg
        (cond ((eq resultspec :void) (<- nil))
              ((eq resultspec :double-float)
               (<- ($  x8664::fp0 :class :fpr :mode :double-float)))
              ((eq resultspec :single-float)
               (<- ($ x8664::fp0 :class :fpr :mode :single-float)))
              ((eq resultspec :unsigned-doubleword)
               (if (node-reg-p vreg)
                 (progn
                   (! makeu64)
                   (<- ($ x8664::arg_z)))
                 (<- ($  x8664::rax :class :gpr :mode :u64))))
              ((eq resultspec :signed-doubleword)
               (if (node-reg-p vreg)
                 (progn
                   (! makes64)
                   (<- ($ x8664::arg_z)))
                 (<- ($  x8664::rax :class :gpr :mode :s64))))
              (t
               (case resultspec
                 (:signed-byte (! sign-extend-s8 x8664::imm0 x8664::imm0))
                 (:signed-halfword (! sign-extend-s16 x8664::imm0 x8664::imm0))
                 (:signed-fullword (! sign-extend-s32 x8664::imm0 x8664::imm0))
                 (:unsigned-byte (! zero-extend-u8 x8664::imm0 x8664::imm0))
                 (:unsigned-halfword (! zero-extend-u16 x8664::imm0 x8664::imm0))
                 (:unsigned-fullword (! zero-extend-u32 x8664::imm0 x8664::imm0)))
               (<- (make-wired-lreg x8664::imm0
                                    :mode
                                    (gpr-mode-name-value
                                     (case resultspec
                                       (:address :address)
                                       (:signed-byte :s8)
                                       (:unsigned-byte :u8)
                                       (:signed-halfword :s16)
                                       (:unsigned-halfword :u16)
                                       (:signed-fullword :s32)
                                       (t :u32))))))))
      (^)))


             
(defx862 x862-%temp-list %temp-list (seg vreg xfer arglist)
  (x862-use-operator (%nx1-operator list) seg vreg xfer arglist))

(defx862 x862-%temp-cons %temp-cons (seg vreg xfer car cdr)
  (x862-use-operator (%nx1-operator cons) seg vreg xfer car cdr))



(defx862 x862-%debug-trap %debug-trap (seg vreg xfer arg)
  (x862-one-targeted-reg-form seg arg ($ *x862-arg-z*))
  (! %debug-trap)
  (<- ($ *x862-arg-z*))
  (^))

(defx862 x862-%reference-external-entry-point %reference-external-entry-point
  (seg vreg xfer arg)
  (ensuring-node-target (target vreg)
    (let* ((reg (if (eq (hard-regspec-value target) *x862-arg-z*) ($ *x862-arg-y*) ($ *x862-arg-z*))))
      (x862-one-targeted-reg-form seg arg reg)
      (! eep.address target reg)))
  (^))

(defx862 x862-%natural+ %natural+ (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (x862-form seg nil nil x)
      (x862-form seg nil xfer y))
    (let* ((fix-x (acode-fixnum-form-p x))
           (fix-y (acode-fixnum-form-p y)))
      (if (and fix-x fix-y)
        (x862-absolute-natural seg vreg xfer (+ fix-x fix-y))
        (let* ((u31x (and (typep fix-x '(unsigned-byte 31)) fix-x))
               (u31y (and (typep fix-y '(unsigned-byte 31)) fix-y)))
          (if (not (or u31x u31y))
            (with-imm-target () (xreg :natural)
	      (with-additional-imm-reg ()
		(with-imm-target (xreg) (yreg :natural)
		  (x862-two-targeted-reg-forms seg x xreg y yreg)
		  (! %natural+ xreg yreg)))
              (<- xreg))
            (let* ((other (if u31x y x)))
              (with-imm-target () (other-reg :natural)
                (x862-one-targeted-reg-form seg other other-reg)
                (! %natural+-c  other-reg (or u31x u31y))
                (<- other-reg))))
          (^))))))

(defx862 x862-%natural- %natural- (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (x862-form seg nil nil x)
      (x862-form seg nil xfer y))
    (let* ((fix-x (acode-fixnum-form-p x))
           (fix-y (acode-fixnum-form-p y)))
      (if (and fix-x fix-y)
        (x862-absolute-natural seg vreg xfer (- fix-x fix-y))
        (let* ((u31y (and (typep fix-y '(unsigned-byte 31)) fix-y)))
          (if (not u31y)
	    (with-imm-target () (xreg :natural)
	      (with-additional-imm-reg ()
		(with-imm-target (xreg) (yreg :natural)
		  (x862-two-targeted-reg-forms seg x xreg y yreg)
		  (! %natural- xreg yreg))
		(<- xreg)))
            (progn
              (with-imm-target () (xreg :natural)
                (x862-one-targeted-reg-form seg x xreg)
                (! %natural--c xreg u31y)
                (<- xreg))))
          (^))))))

(defx862 x862-%natural-logior %natural-logior (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (x862-form seg nil nil x)
      (x862-form seg nil xfer y))
    (let* ((naturalx (nx-natural-constant-p x))
           (naturaly (nx-natural-constant-p y)))
      (if (and naturalx naturaly) 
        (x862-absolute-natural seg vreg xfer (logior naturalx naturaly))
        (let* ((u31x (nx-u31-constant-p x))
               (u31y (nx-u31-constant-p y))
               (constant (or u31x u31y)))
          (if (not constant)
            (with-imm-target () (xreg :natural)
	      (with-additional-imm-reg ()
		(with-imm-target (xreg) (yreg :natural)
		  (x862-two-targeted-reg-forms seg x xreg y yreg)
		  (! %natural-logior xreg yreg)))
              (<- xreg))
            (let* ((other (if u31x y x)))
              (with-imm-target () (other-reg :natural)
                (x862-one-targeted-reg-form seg other other-reg)
                (! %natural-logior-c other-reg constant)
                (<- other-reg))))
          (^))))))

(defx862 x862-%natural-logxor %natural-logxor (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (x862-form seg nil nil x)
      (x862-form seg nil xfer y))
    (let* ((naturalx (nx-natural-constant-p x))
           (naturaly (nx-natural-constant-p y)))
      (if (and naturalx naturaly) 
        (x862-absolute-natural seg vreg xfer (logxor naturalx naturaly))
        (let* ((u32x (nx-u32-constant-p x))
               (u32y (nx-u32-constant-p y))
               (constant (or u32x u32y)))
          (if (not constant)
            (with-imm-target () (xreg :natural)
	      (with-additional-imm-reg ()
		(with-imm-target (xreg) (yreg :natural)
		  (x862-two-targeted-reg-forms seg x xreg y yreg)
		  (! %natural-logxor xreg yreg)))
              (<- xreg))
            (let* ((other (if u32x y x)))
              (with-imm-target () (other-reg :natural)
                (x862-one-targeted-reg-form seg other other-reg)
                (! %natural-logxor-c other-reg constant)
                (<- other-reg))))
          (^))))))

(defx862 x862-%natural-logand %natural-logand (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (x862-form seg nil nil x)
      (x862-form seg nil xfer y))
    (let* ((naturalx (nx-natural-constant-p x))
           (naturaly (nx-natural-constant-p y)))
      (if (and naturalx naturaly) 
        (x862-absolute-natural seg vreg xfer (logand naturalx naturaly))
        (let* ((u31x (nx-u31-constant-p x))
               (u31y (nx-u31-constant-p y))
               (constant (or u31x u31y)))
          (if (not constant)
            (with-imm-target () (xreg :natural)
	      (with-additional-imm-reg ()
		(with-imm-target (xreg) (yreg :natural)
		  (x862-two-targeted-reg-forms seg x xreg y yreg)
		  (! %natural-logand xreg yreg)))
              (<- xreg))
            (let* ((other (if u31x y x)))
              (with-imm-target () (other-reg :natural)
                (x862-one-targeted-reg-form seg other other-reg)
                (! %natural-logand-c  other-reg constant)
                (<- other-reg))))
          (^))))))

(defx862 x862-natural-shift-right natural-shift-right (seg vreg xfer num amt)
  (with-imm-target () (dest :natural)
    (x862-one-targeted-reg-form seg num dest)
    (! natural-shift-right dest (acode-fixnum-form-p amt))
    (<- dest)
    (^)))

(defx862 x862-natural-shift-left natural-shift-left (seg vreg xfer num amt)
  (with-imm-target () (dest :natural)
    (x862-one-targeted-reg-form seg num dest)
    (! natural-shift-left dest  (acode-fixnum-form-p amt))
    (<- dest)
    (^)))

;;; This assumes that "global" variables are always boundp.
(defx862 x862-global-ref global-ref (seg vreg xfer sym)
  (when vreg
    (ensuring-node-target (target vreg)
      (with-node-temps () (symreg)
        (setq symreg (or (x862-register-constant-p sym)
                         (x862-store-immediate seg sym symreg)))
        (! symbol-ref target symreg (target-arch-case
				     (:x8632 x8632::symbol.vcell-cell)
				     (:x8664 x8664::symbol.vcell-cell))))))
  (^))

(defx862 x862-global-setq global-setq (seg vreg xfer sym val)
  (x862-vset seg 
             vreg 
             xfer
             :symbol
             (make-acode (%nx1-operator %symptr->symvector)
                         (make-acode (%nx1-operator immediate) sym))
             (make-acode (%nx1-operator fixnum)
                         (target-arch-case
			  (:x8632 x8632::symbol.vcell-cell)
                          (:x8664 x8664::symbol.vcell-cell)))
             val
             nil))

(defx862 x862-%current-frame-ptr %current-frame-ptr (seg vreg xfer)
  (cond ((x862-tailcallok xfer)
	 (x862-restore-nvrs seg *x862-register-restore-ea* *x862-register-restore-count*)
	 (x862-restore-full-lisp-context seg)
	 (! %current-frame-ptr ($ *x862-arg-z*))
	 (! jump-return-pc))
	(t
	 (when vreg
	   (ensuring-node-target (target vreg)
				 (! %current-frame-ptr target)))
	 (^))))

(defx862 x862-%foreign-stack-pointer %foreign-stack-pointer (seg vreg xfer)
   (when vreg
     (ensuring-node-target (target vreg)
       (! %foreign-stack-pointer target)))
   (^))


(defx862 x862-%current-tcr %current-tcr (seg vreg xfer)
  (when vreg
    (ensuring-node-target (target vreg)
      (! %current-tcr target)))
  (^))



(defx862 x862-%interrupt-poll %interrupt-poll (seg vreg xfer)
  (! event-poll)
  (x862-nil seg vreg xfer))


(defx862 x862-with-c-frame with-c-frame (seg vreg xfer body &aux
                                             (old-stack (x862-encode-stack)))
  (! alloc-c-frame 0)
  (x862-open-undo $undo-x86-c-frame)
  (x862-undo-body seg vreg xfer body old-stack))

(defx862 x862-with-variable-c-frame with-variable-c-frame (seg vreg xfer size body &aux
                                                               (old-stack (x862-encode-stack)))
  (let* ((reg (x862-one-untargeted-reg-form seg size *x862-arg-z*)))
    (! alloc-variable-c-frame reg)
    (x862-open-undo $undo-x86-c-frame)
    (x862-undo-body seg vreg xfer body old-stack)))

(defx862 x862-%symbol->symptr %symbol->symptr (seg vreg xfer sym)
  (let* ((src (x862-one-untargeted-reg-form seg sym *x862-arg-z*)))
    (ensuring-node-target (target vreg)
      (! %symbol->symptr target src))
    (^)))

(defx862 x862-%double-to-single %double-to-single (seg vreg xfer arg)
  (if (null vreg)
    (x862-form seg vreg xfer arg)
    (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
      (let* ((dreg (x862-one-untargeted-reg-form 
                    seg arg
                    (make-wired-lreg (hard-regspec-value vreg)
                                     :class hard-reg-class-fpr
                                     :mode hard-reg-class-fpr-mode-double))))
        (! double-to-single vreg dreg)
        (^))
      (with-fp-target () (argreg :double-float)
        (x862-one-targeted-reg-form seg arg argreg)
        (with-fp-target ()  (sreg :single-float)
          (! double-to-single sreg argreg)
          (<- sreg)
          (^))))))

(defx862 x862-%single-to-double %single-to-double (seg vreg xfer arg)
  (if (null vreg)
    (x862-form seg vreg xfer arg)
    (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
      (let* ((sreg (x862-one-untargeted-reg-form 
                    seg arg
                    (make-wired-lreg (hard-regspec-value vreg)
                                     :class hard-reg-class-fpr
                                     :mode hard-reg-class-fpr-mode-double))))
        (! single-to-double vreg sreg)
        (^))
      (with-fp-target () (sreg :single-float)
        (x862-one-targeted-reg-form seg arg sreg)
        (with-fp-target () (dreg :double-float)
          (! single-to-double dreg sreg)
          (<- dreg)
          (^))))))

(defx862 x862-%symptr->symvector %symptr->symvector (seg vreg xfer arg)
  (if (null vreg)
    (x862-form seg vreg xfer arg)
    (progn
      (ensuring-node-target (target vreg)
        (x862-one-targeted-reg-form seg arg target)
        (! %symptr->symvector target))
      (^))))

(defx862 x862-%symvector->symptr %symvector->symptr (seg vreg xfer arg)
  (if (null vreg)
    (x862-form seg vreg xfer arg)
    (progn
      (ensuring-node-target (target vreg)
        (x862-one-targeted-reg-form seg arg target)
        (! %symvector->symptr target))
      (^))))

(defx862 x862-%fixnum-to-single %fixnum-to-single (seg vreg xfer arg)
  (with-fp-target () (sreg :single-float)
    (let* ((r (x862-one-untargeted-reg-form seg arg *x862-arg-z*)))
      (unless (or (acode-fixnum-form-p arg)
                  *x862-reckless*)
        (! trap-unless-fixnum r))
      (! fixnum->single-float sreg r)
      (<- sreg)
      (^))))

(defx862 x862-%fixnum-to-double %fixnum-to-double (seg vreg xfer arg)
  (with-fp-target () (dreg :double-float)
    (let* ((r (x862-one-untargeted-reg-form seg arg *x862-arg-z*)))
      (unless (or (acode-fixnum-form-p arg)
                  *x862-reckless*)
        (! trap-unless-fixnum r))
      (! fixnum->double-float dreg r)
      (<- dreg)
      (^))))

(defx862 x862-%double-float %double-float (seg vreg xfer arg)
  (let* ((real (or (acode-fixnum-form-p arg)
                   (let* ((form (acode-unwrapped-form-value arg)))
                     (if (and (acode-p form)
                              (eq (acode-operator form)
                                  (%nx1-operator immediate))
                              (typep (cadr form) 'real))
                       (cadr form)))))
         (dconst (and real (ignore-errors (float real 0.0d0)))))
    (if dconst
      (x862-immediate seg vreg xfer dconst)
      (if (x862-form-typep arg 'single-float)
        (x862-use-operator (%nx1-operator %single-to-double)
                           seg
                           vreg
                           xfer
                           arg)
        (if (x862-form-typep arg 'fixnum)
          (x862-use-operator (%nx1-operator %fixnum-to-double)
                             seg
                             vreg
                             xfer
                             arg)
          (x862-use-operator (%nx1-operator call)
                             seg
                             vreg
                             xfer
                             (make-acode (%nx1-operator immediate)
                                         '%double-float)
                             (list nil (list arg))))))))

(defx862 x862-%single-float %single-float (seg vreg xfer arg)
  (let* ((real (or (acode-fixnum-form-p arg)
                   (let* ((form (acode-unwrapped-form-value arg)))
                     (if (and (acode-p form)
                              (eq (acode-operator form)
                                  (%nx1-operator immediate))
                              (typep (cadr form) 'real))
                       (cadr form)))))
         (sconst (and real (ignore-errors (float real 0.0f0)))))
    (if sconst
      (x862-immediate seg vreg xfer sconst)
      (if (x862-form-typep arg 'double-float)
        (x862-use-operator (%nx1-operator %double-to-single)
                           seg
                           vreg
                           xfer
                           arg)
        (if (x862-form-typep arg 'fixnum)
          (x862-use-operator (%nx1-operator %fixnum-to-single)
                             seg
                             vreg
                             xfer
                             arg)
          (x862-use-operator (%nx1-operator call)
                             seg
                             vreg
                             xfer
                             (make-acode (%nx1-operator immediate)
                                         '%short-float)
                             (list nil (list arg))))))))


(defx862 x862-%new-ptr %new-ptr (seg vreg xfer size clear-p )
  (x862-call-fn seg
                vreg
                xfer
                (make-acode (%nx1-operator immediate)
                            '%new-gcable-ptr)
                (list nil (list clear-p size))
                nil))

;------

#+not-yet
(progn


;;;Make a gcable macptr.




)

#-x86-target
(defun x8664-xcompile-lambda (def &key show-vinsns (symbolic-names t)
                                  (target :darwinx8664)
                                  (disassemble t))
  (let* ((*x862-debug-mask* (if show-vinsns
                              (ash 1 x862-debug-vinsns-bit)
                              0))
         (backend (find-backend target))
         (*target-ftd* (if backend
                         (backend-target-foreign-type-data backend)
                         *target-ftd*)))
    (multiple-value-bind (xlfun warnings)
        (compile-named-function def :target target)
      (signal-or-defer-warnings warnings nil)
      (when disassemble
        (format t "~%~%")
        (apply #'x8664-disassemble-xfunction
               xlfun
               (unless symbolic-names (list nil))))
      xlfun)))

#-x8632-target
(defun x8632-xcompile-lambda (def &key show-vinsns (symbolic-names t)
                                  (target :darwinx8632)
                                  (disassemble t))
  (let* ((*x862-debug-mask* (if show-vinsns
                              (ash 1 x862-debug-vinsns-bit)
                              0))
         (backend (find-backend target))
         (*target-ftd* (if backend
                         (backend-target-foreign-type-data backend)
                         *target-ftd*)))
    (multiple-value-bind (xlfun warnings)
        (compile-named-function def :target target)
      (signal-or-defer-warnings warnings nil)
      (when disassemble
	(let ((*target-backend* backend))
	  (format t "~%~%")
	  (apply #'x8632-disassemble-xfunction
		 xlfun
		 (unless symbolic-names (list nil)))))
      xlfun)))




