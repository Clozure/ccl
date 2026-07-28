;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "ARM64ENV"))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (require "ARM64-BACKEND"))

(defparameter *arm642-debug-mask* 0)
(defconstant arm642-debug-verbose-bit 0)
(defconstant arm642-debug-vinsns-bit 1)
(defparameter *arm642-target-node-size* 0)
(defparameter *arm642-target-dnode-size* 0)
(defparameter *arm642-target-fixnum-shift* 0)
(defparameter *arm642-target-node-shift* 0)
(defparameter *arm642-target-bits-in-word* 0)
(defparameter *arm642-target-num-arg-regs* 0)
(defparameter *arm642-target-num-save-regs* 0)
(defparameter *arm642-target-half-fixnum-type* '(signed-byte 60))
(defparameter *arm642-nfp-depth* 0)
(defparameter *arm642-max-nfp-depth* 0)
(defparameter *arm642-all-nfp-pushes* ())
(defparameter *arm642-nfp-vars* ())
(defparameter *arm642-incoming-args-on-stack* most-positive-fixnum)
(defparameter *arm642-stack-vars* ())
(defparameter *arm642-tagbody-info* ())

(defun arm642-max-nfp-depth ()
 ;; Maximum extent, in bytes, of the NFP data area. This is the size
 ;; of the unboxed data region only; the frame's header word and
 ;; saved-nfp link are added by ARM642-NFP-FRAME-SIZE.
  (or *arm642-max-nfp-depth*
      (setq *arm642-max-nfp-depth*
            (let* ((max 0))
              (declare (fixnum max))
              (dolist (v *arm642-all-nfp-pushes* max)
                (when (and v (vinsn-succ v))    ;not elided
                  (let* ((depth (+ (the fixnum
                                        (svref (vinsn-variable-parts v) 1))
                                   (if (vinsn-attribute-p v :uses-frame-pointer)
                                     16
                                     8))))
                    (declare (fixnum depth))
                    (when (> depth max)
                      (setq max depth)))))))))

;;; The NFP frame is a u64-vector on the control stack.
;;; Layout:
;;; word 0 : u64-vector header (immheader)
;;; word 1 (element 0): saved previous tcr.nfp (the frame link; unboxed)
;;; word 2.. (elt 1..): unboxed NFP data (ARM642-MAX-NFP-DEPTH bytes)
;;; Data therefore lives at frame-base + (2 * node-size) + offset. The whole
;;; frame is rounded up to a dnode so SP stays 16-byte aligned.
(defun arm642-nfp-frame-size ()
  (logandc2 (+ (arm642-max-nfp-depth)
               (* 2 arm64::node-size)           ;header + saved-nfp
               (1- arm64::dnode-size))
            (1- arm64::dnode-size)))

(defun arm642-nfp-header ()
 ;; u64-vector header whose element count covers the whole frame, so
 ;; skip_over_ivector lands exactly on the caller's frame. Constant at
 ;; compile time (ARM642-MAX-NFP-DEPTH is known).
  (logior (ash (1- (ash (arm642-nfp-frame-size) (- arm64::word-shift)))
               arm64::num-subtag-bits)
          arm64::subtag-u64-vector))

(defmacro with-arm64-p2-declarations (declsform &body body)
  `(let* ((*arm642-tail-allow* *arm642-tail-allow*)
          (*arm642-reckless* *arm642-reckless*)
          (*arm642-open-code-inline* *arm642-open-code-inline*)
          (*arm642-trust-declarations* *arm642-trust-declarations*)
	  (*arm642-full-safety* *arm642-full-safety*)
          (*arm642-float-safety* *arm642-float-safety*))
     (arm642-decls ,declsform)
     ,@body))

(defun arm642-emit-vinsn (vlist name vinsn-table &rest vregs)
  (arm642-update-regmap (apply #'%emit-vinsn vlist name vinsn-table vregs)))

(defmacro with-arm64-local-vinsn-macros ((segvar &optional vreg-var xfer-var)
                                         &body body)
  (declare (ignorable xfer-var))
  (let* ((template-name-var (gensym))
         (template-temp (gensym))
         (args-var (gensym))
         (labelnum-var (gensym))
         (retvreg-var (gensym))
         (label-var (gensym)))
    `(macrolet ((! (,template-name-var &rest ,args-var)
                  (let* ((,template-temp (get-vinsn-template-cell
                                          ,template-name-var
                                          (backend-p2-vinsn-templates
 *target-backend*))))
                    (unless ,template-temp
                      (warn "VINSN \"~A\" not defined" ,template-name-var))
                    `(arm642-emit-vinsn ,',segvar
                                        ',,template-name-var
                                        (backend-p2-vinsn-templates
 *target-backend*)
                                        ,@,args-var))))
       (macrolet ((<- (,retvreg-var)
                    `(arm642-copy-register ,',segvar ,',vreg-var
                                           ,,retvreg-var))
                  (@  (,labelnum-var)
                      `(progn
                         (arm642-invalidate-regmap)
                         (backend-gen-label ,',segvar ,,labelnum-var)))
                  (@+ (,labelnum-var)
                    `(progn             ;keep regmap
                       (backend-gen-label ,',segvar ,,labelnum-var)))
                  (-> (,label-var)
                    `(! jump (aref *backend-labels* ,,label-var)))
                  (^ (&rest branch-args)
                    `(arm642-branch ,',segvar ,',xfer-var ,',vreg-var
                                    ,@branch-args))
                  (? (&key (class :gpr)
                        (mode :lisp))
                    (let* ((class-val
                             (ecase class
                               (:gpr hard-reg-class-gpr)
                               (:fpr hard-reg-class-fpr)
                               (:crf hard-reg-class-crf)))
                           (mode-val
                             (if (eq class :gpr)
                               (gpr-mode-name-value mode)
                               (if (eq class :fpr)
                                 (if (eq mode :single-float)
                                   hard-reg-class-fpr-mode-single
                                   hard-reg-class-fpr-mode-double)
                                 0))))
                      `(make-unwired-lreg nil
                                          :class ,class-val
                                          :mode ,mode-val)))
                  ($ (reg &key (class :gpr) (mode :lisp))
                    (let* ((class-val
                             (ecase class
                               (:gpr hard-reg-class-gpr)
                               (:fpr hard-reg-class-fpr)
                               (:crf hard-reg-class-crf)))
                           (mode-val
                             (if (eq class :gpr)
                               (gpr-mode-name-value mode)
                               (if (eq class :fpr)
                                 (if (eq mode :single-float)
                                   hard-reg-class-fpr-mode-single
                                   hard-reg-class-fpr-mode-double)
                                 0))))
                      `(make-wired-lreg ,reg
                                        :class ,class-val
                                        :mode ,mode-val))))
         ,@body))))


(defvar *arm642-woi* nil)
(defvar *arm642-open-code-inline* nil)
(defvar *arm642-optimize-for-space* nil)
(defvar *arm642-register-restore-count* 0)
(defvar *arm642-register-restore-ea* nil)
(defvar *arm642-non-volatile-fpr-count* 0)
(defvar *arm642-compiler-register-save-note* nil)

(defparameter *arm642-tail-call-aliases*
  ()
 #| '((%call-next-method . (%tail-call-next-method . 1))) |#
)


(defvar *arm642-icode* nil)
(defvar *arm642-undo-stack* nil)
(defvar *arm642-undo-because* nil)


(defvar *arm642-cur-afunc* nil)
(defvar *arm642-vstack* 0)
(defvar *arm642-cstack* 0)
(defvar *arm642-undo-count* 0)
(defvar *arm642-returning-values* nil)
(defvar *arm642-vcells* nil)
(defvar *arm642-fcells* nil)
(defvar *arm642-entry-vsp-saved-p* nil)

(defvar *arm642-entry-label* nil)
(defvar *arm642-fixed-args-label* nil)
(defvar *arm642-fixed-args-tail-label* nil)
(defvar *arm642-fixed-nargs* nil)
(defvar *arm642-tail-allow* t)
(defvar *arm642-reckless* nil)
(defvar *arm642-full-safety* nil)
(defvar *arm642-float-safety* nil)
(defvar *arm642-trust-declarations* nil)
(defvar *arm642-entry-vstack* nil)
(defvar *arm642-need-nargs* t)

(defparameter *arm642-inhibit-register-allocation* nil)
(defvar *arm642-record-symbols* nil)
(defvar *arm642-recorded-symbols* nil)
(defvar *arm642-emitted-source-notes* nil)

(defvar *arm642-result-reg* arm64::arg_z)
(defparameter *arm642-nvrs* nil)
(defparameter *arm642-first-nvr* -1)

(defvar *arm642-gpr-locations* nil)
(defvar *arm642-gpr-locations-valid-mask* 0)
(defvar *arm642-gpr-constants* nil)
(defvar *arm642-gpr-constants-valid-mask* 0)

(declaim (fixnum *arm642-vstack* *arm642-cstack*))

(defun arm642-gprs-containing-constant (c)
  (let* ((in *arm642-gpr-constants-valid-mask*)
         (vals *arm642-gpr-constants*)
         (out 0))
    (declare (fixnum in out) (simple-vector vals))
    (dotimes (i 32 out)
      (declare (type (mod 32) i))
      (when (and (logbitp i in)
                 (eql c (svref vals i)))
        (setq out (logior out (ash 1 i)))))))

(defun arm642-nfp-ref (seg vreg ea)
  (with-arm64-local-vinsn-macros (seg vreg)
    (let* ((offset (logand #xfff8 ea))
           (type (logand #x7 ea))
           (vreg-class (hard-regspec-class vreg))
           (vreg-mode (get-regspec-mode vreg))
           (nested (> *arm642-undo-count* 0))
           (vinsn nil)
           (reg vreg))
      (ecase type
        (#. memspec-nfp-type-natural
         (unless (and (eql vreg-class hard-reg-class-gpr)
                      (eql vreg-mode hard-reg-class-gpr-mode-u64))
           (setq reg (available-imm-temp
 *available-backend-imm-temps*
                      :u64)))
         (setq vinsn
               (if nested
                 (! nfp-load-unboxed-word-nested reg offset)
                 (! nfp-load-unboxed-word reg offset))))
        (#. memspec-nfp-type-double-float
         (unless (and (eql vreg-class hard-reg-class-fpr)
                      (eql vreg-mode hard-reg-class-fpr-mode-double))
           (setq reg (available-fp-temp
 *available-backend-fp-temps*
                      :double-float)))
         (setq vinsn
               (if nested
                 (! nfp-load-double-float-nested reg offset)
                 (! nfp-load-double-float reg offset))))
        (#. memspec-nfp-type-single-float
         (unless (and (eql vreg-class hard-reg-class-fpr)
                      (eql vreg-mode hard-reg-class-fpr-mode-single))
           (setq reg (available-fp-temp
 *available-backend-fp-temps*
                      :single-float)))
         (setq vinsn
               (if nested
                 (! nfp-load-single-float-nested reg offset)
                 (! nfp-load-single-float  reg offset))))
        (#. memspec-nfp-type-complex-double-float
         (unless (and (eql vreg-class hard-reg-class-fpr)
                      (eql vreg-mode
                           hard-reg-class-fpr-mode-complex-double-float))
           (setq reg (available-fp-temp
 *available-backend-fp-temps*
                      :complex-double-float)))
         (setq vinsn
               (if nested
                 (! nfp-load-complex-double-float-nested reg offset)
                 (! nfp-load-complex-double-float reg offset))))
        (#. memspec-nfp-type-complex-single-float
         (unless (and (eql vreg-class hard-reg-class-fpr)
                      (eql vreg-mode
                           hard-reg-class-fpr-mode-complex-single-float))
           (setq reg (available-fp-temp
 *available-backend-fp-temps*
                      :complex-single-float)))
         (setq vinsn
               (if nested
                 (! nfp-load-complex-single-float-nested reg offset)
                 (! nfp-load-complex-single-float  reg offset)))))
      (when (memspec-single-ref-p ea)
        (let* ((push-vinsn
                 (find offset *arm642-all-nfp-pushes*
                       :key (lambda (v)
                              (when (typep v 'vinsn)
                                (svref (vinsn-variable-parts v) 1))))))
          (when push-vinsn
            (arm642-elide-pushes seg push-vinsn vinsn))))
      (<- reg))))

(defun arm642-reg-for-nfp-set (vreg ea)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((type (logand #x7 ea))
           (vreg-class (if vreg (hard-regspec-class vreg)))
           (vreg-mode (if vreg (get-regspec-mode vreg))))
      (ecase type
        (#. memspec-nfp-type-natural
         (if (and (eql vreg-class hard-reg-class-gpr)
                  (eql vreg-mode hard-reg-class-gpr-mode-u64))
           vreg
           (make-unwired-lreg
            (available-imm-temp *available-backend-imm-temps* :u64))))
        (#. memspec-nfp-type-double-float
         (if (and (eql vreg-class hard-reg-class-fpr)
                  (eql vreg-mode hard-reg-class-fpr-mode-double))
           vreg
           (make-unwired-lreg
            (available-fp-temp *available-backend-fp-temps* :double-float))))
        (#. memspec-nfp-type-single-float
         (if (and (eql vreg-class hard-reg-class-fpr)
                  (eql vreg-mode hard-reg-class-fpr-mode-single))
           vreg
           (make-unwired-lreg
            (available-fp-temp *available-backend-fp-temps* :single-float))))
        (#. memspec-nfp-type-complex-double-float
         (if (and (eql vreg-class hard-reg-class-fpr)
                  (eql vreg-mode hard-reg-class-fpr-mode-complex-double-float))
           vreg
           (make-unwired-lreg
            (available-fp-temp *available-backend-fp-temps*
                               :complex-double-float))))
        (#. memspec-nfp-type-complex-single-float
         (if (and (eql vreg-class hard-reg-class-fpr)
                  (eql vreg-mode hard-reg-class-fpr-mode-complex-single-float))
           vreg
           (make-unwired-lreg
            (available-fp-temp *available-backend-fp-temps*
                               :complex-single-float))))))))

(defun arm642-nfp-set (seg reg ea)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((offset (logand #xfff8 ea))
           (nested (> *arm642-undo-count* 0)))
      (ecase (logand #x7 ea)
        (#. memspec-nfp-type-natural
            (if nested
              (! nfp-store-unboxed-word-nested reg offset)
              (! nfp-store-unboxed-word reg offset)))
        (#. memspec-nfp-type-double-float
            (if nested
              (! nfp-store-double-float-nested reg offset)
              (! nfp-store-double-float reg offset)))
        (#. memspec-nfp-type-single-float
            (if nested
              (! nfp-store-single-float-nested reg offset)
              (! nfp-store-single-float  reg offset)))
        (#. memspec-nfp-type-complex-double-float
            (if nested
              (! nfp-store-complex-double-float-nested reg offset)
              (! nfp-store-complex-double-float reg offset)))
        (#. memspec-nfp-type-complex-single-float
            (if nested
              (! nfp-store-complex-single-float-nested reg offset)
              (! nfp-store-complex-single-float reg offset)))))))

;;; Depending on the variable's type and other attributes, maybe
;;; push it on the NFP. Return the nfp-relative EA if we push it.
(defun arm642-nfp-bind (seg var initform)
  (let* ((bits (nx-var-bits var)))
    (unless (logtest bits (logior (ash 1 $vbitspecial)
                                  (ash 1 $vbitclosed)
                                  (ash 1 $vbitdynamicextent)))
      (let* ((type (acode-var-type var *arm642-trust-declarations*))
             (reg nil)
             (nfp-bits 0))
        (cond ((and (subtypep type '(unsigned-byte 64))
                    nil ;note early out
                    (not (subtypep type '(signed-byte 61))))
               (setq reg (available-imm-temp
 *available-backend-imm-temps* :u64)
                     nfp-bits memspec-nfp-type-natural))
              ((subtypep type 'single-float)
               (setq reg (available-fp-temp *available-backend-fp-temps*
                                            :single-float)
                     nfp-bits memspec-nfp-type-single-float))
              ((subtypep type 'double-float)
               (setq reg (available-fp-temp *available-backend-fp-temps*
                                            :double-float)
                     nfp-bits memspec-nfp-type-double-float))
              ((subtypep type 'complex-single-float)
               (setq reg (available-fp-temp *available-backend-fp-temps*
                                            :complex-single-float)
                     nfp-bits memspec-nfp-type-complex-single-float))
              ((subtypep type 'complex-double-float)
               (setq reg (available-fp-temp *available-backend-fp-temps*
                                            :complex-double-float)
                     nfp-bits memspec-nfp-type-complex-double-float)))
        (when reg
          (let* ((vinsn (arm642-push-register
                         seg
                         (arm642-one-untargeted-reg-form seg initform reg))))
            (when vinsn
              (push (cons vinsn var) *arm642-nfp-vars*)
              (make-nfp-address
               (svref (vinsn-variable-parts vinsn) 1)
               nfp-bits))))))))

(defun arm642-do-lexical-reference (seg vreg ea)
  (when vreg
    (with-arm64-local-vinsn-macros (seg vreg)
      (if (memory-spec-p ea)
        (if (eql (memspec-type ea) memspec-nfp-offset)
          (arm642-nfp-ref seg vreg ea)
          (ensuring-node-target (target vreg)
            (let* ((reg (unless (node-reg-p vreg)
                          (or (arm642-reg-for-ea ea)
                              (arm642-try-non-conflicting-reg target 0)))))
              (when reg (setq target reg))
              (arm642-stack-to-register seg ea target)
              (if (addrspec-vcell-p ea)
                (! vcell-ref target target)))))
        (<- ea)))))

;;; Donor: arm2.lisp arm2-do-lexical-setq @443 (v2 @393 is the same shape
;;; but zeroes arg_y via a copy from xzr and reaches .SPgvset through v2's
;;; sptab machinery; arm2's lri-0 + subprim-offset-by-name kept — his
;;; call-subprim-3-style resolver is subprim-name->offset).
;;; .SPgvset is NOT in his 6-entry subprims table — ratify item, see U5v.
(defun arm642-do-lexical-setq (seg vreg ea valreg)
  (with-arm64-local-vinsn-macros (seg vreg)
    (cond ((typep ea 'lreg)
            (arm642-copy-register seg ea valreg))
          ((addrspec-vcell-p ea)     ; closed-over vcell
           (arm642-copy-register seg arm64::arg_z valreg)
           (arm642-stack-to-register seg ea arm64::arg_x)
           (arm642-lri seg arm64::arg_y 0)
           (! call-subprim-3 arm64::arg_z (subprim-name->offset '.SPgvset) arm64::arg_x arm64::arg_y arm64::arg_z)
           (setq valreg arm64::arg_z))
          ((memory-spec-p ea)    ; vstack slot
           (arm642-register-to-stack seg valreg ea))
          (t
           (arm642-copy-register seg ea valreg)))
    (when vreg
      (<- valreg))))

;;; ensure that next-method-var is heap-consed (if it's closed over.)
(defun arm642-heap-cons-next-method-var (seg var)
  (with-arm64-local-vinsn-macros (seg)
    (when (eq (ash 1 $vbitclosed)
              (logand (logior (ash 1 $vbitclosed)
                              (ash 1 $vbitcloseddownward))
                      (the fixnum (nx-var-bits var))))
      (let* ((ea (var-ea var))
             (arg ($ arm64::arg_z))
             (result ($ arm64::arg_z)))
        (arm642-do-lexical-reference seg arg ea)
        (arm642-set-nargs seg 1)
        (let ((idx (backend-immediate-index (arm642-symbol-entry-locative
                                             '%cons-magic-next-method-arg))))
          (if (< (+ arm64::misc-data-offset (ash (1+ idx) arm64::word-shift))
                 256)
            (! ref-constant ($ arm64::fname) idx)
            (with-imm-target () (idxreg :s64)
              (arm642-lri seg idxreg (+ arm64::misc-data-offset
                                        (ash (1+ idx) arm64::word-shift)))
              (! ref-indexed-constant ($ arm64::fname) idxreg))))
        (! call-known-symbol arg)
        (arm642-do-lexical-setq seg nil ea result)))))

(defun acode-condition-to-arm64-cr-bit (cond)
  (condition-to-arm64-cr-bit (car (acode-operands cond))))

(defun condition-to-arm64-cr-bit (cond)
  (case cond
    (:eq (values arm64::cond-eq t))
    (:ne (values arm64::cond-eq nil))
    (:gt (values arm64::cond-gt t))
    (:le (values arm64::cond-gt nil))
    (:lt (values arm64::cond-lt t))
    (:ge (values arm64::cond-lt nil))))

(defun arm64-cr-bit-to-arm64-unsigned-cr-bit (cr-bit)
  (case cr-bit
    (#.arm64::cond-eq arm64::cond-eq)
    (#.arm64::cond-ne arm64::cond-ne)
    (#.arm64::cond-gt arm64::cond-hi)
    (#.arm64::cond-le arm64::cond-ls)
    (#.arm64::cond-lt arm64::cond-lo)
    (#.arm64::cond-ge arm64::cond-hs)))

;;; If we have to change the order of operands in a comparison, we
;;; generally need to change the condition we're testing.
(defun arm642-cr-bit-for-reversed-comparison (cr-bit)
  (ecase cr-bit
    (#.arm64::cond-eq arm64::cond-eq)
    (#.arm64::cond-ne arm64::cond-ne)
    (#.arm64::cond-lt arm64::cond-gt)
    (#.arm64::cond-le arm64::cond-ge)
    (#.arm64::cond-gt arm64::cond-lt)
    (#.arm64::cond-ge arm64::cond-le)
    (#.arm64::cond-lo arm64::cond-hi)
    (#.arm64::cond-ls arm64::cond-hs)
    (#.arm64::cond-hi arm64::cond-lo)
    (#.arm64::cond-hs arm64::cond-ls)))

(defun arm642-ensure-binding-indices-for-vcells (vcells)
  (dolist (cell vcells)
    (ensure-binding-index (car cell)))
  vcells)

(defun arm642-compile (afunc &optional lambda-form *arm642-record-symbols*)
  (progn
    (dolist (a  (afunc-inner-functions afunc))
      (unless (afunc-lfun a)
        (arm642-compile a
                      (if lambda-form
                        (afunc-lambdaform a))
 *arm642-record-symbols*))) ; always compile inner guys
    (let* ((*arm642-cur-afunc* afunc)
           (*arm642-returning-values* nil)
           (*arm642-woi* nil)
           (*encoded-reg-value-byte* (byte 5 0))
           (*arm642-open-code-inline* nil)
           (*arm642-optimize-for-space* nil)
           (*arm642-register-restore-count* nil)
           (*arm642-compiler-register-save-note* nil)
           (*arm642-non-volatile-fpr-count* 0)
           (*arm642-register-restore-ea* nil)
           (*arm642-vstack* 0)
           (*arm642-cstack* 0)
           (*arm642-target-fixnum-shift* (arch::target-fixnum-shift
                                          (backend-target-arch
 *target-backend*)))
           (*arm642-target-node-shift* (arch::target-word-shift
                                        (backend-target-arch
 *target-backend*)))
           (*arm642-target-bits-in-word* (arch::target-nbits-in-word
                                          (backend-target-arch
 *target-backend*)))
	   (*arm642-target-node-size* (arch::target-lisp-node-size
                                       (backend-target-arch *target-backend*)))
           (*arm642-target-half-fixnum-type* *arm642-target-half-fixnum-type*)
           (*backend-vinsns* (backend-p2-vinsn-templates *target-backend*))
           (*backend-node-regs* arm64-node-regs)
           (*backend-node-temps* arm64-temp-node-regs)
           (*available-backend-node-temps* arm64-temp-node-regs)
           (*backend-imm-temps* arm64-imm-regs)
           (*available-backend-imm-temps* arm64-imm-regs)
           (*backend-fp-temps* arm64-temp-fp-regs)
           (*available-backend-fp-temps* arm64-temp-fp-regs)
           (*backend-crf-temps* arm64-cr-fields)
           (*available-backend-crf-temps* arm64-cr-fields)
           (bits 0)
           (*logical-register-counter* -1)
           (*arm642-undo-count* 0)
           (*backend-labels* (arm642-make-stack 64
                                                target::subtag-simple-vector))
           (*arm642-undo-stack* (arm642-make-stack
                                 64 target::subtag-simple-vector))
           (*arm642-undo-because* (arm642-make-stack 64))
           (*backend-immediates* (arm642-make-stack
                                  64 target::subtag-simple-vector))
           (*arm642-entry-label* nil)
           (*arm642-fixed-args-label* nil)
           (*arm642-fixed-args-tail-label*)
           (*arm642-fixed-nargs* nil)
           (*arm642-inhibit-register-allocation* nil)
           (*arm642-tail-allow* t)
           (*arm642-reckless* nil)
	   (*arm642-full-safety* nil)
           (*arm642-float-safety* nil)
           (*arm642-trust-declarations* t)
           (*arm642-entry-vstack* nil)
           (*arm642-need-nargs* t)
           (fname (afunc-name afunc))
           (*arm642-entry-vsp-saved-p* nil)
           (*arm642-vcells* (arm642-ensure-binding-indices-for-vcells (afunc-vcells afunc)))
           (*arm642-fcells* (afunc-fcells afunc))
 *arm642-recorded-symbols*
           (*arm642-emitted-source-notes* '())
           (*arm642-gpr-locations-valid-mask* 0)
           (*arm642-gpr-locations* (make-array 32 :initial-element nil))
           (*arm642-gpr-constants-valid-mask* 0)
           (*arm642-gpr-constants* (make-array 32 :initial-element nil))
           (*arm642-nfp-depth* 0)
           (*arm642-max-nfp-depth* ())
           (*arm642-all-nfp-pushes* ())
           (*arm642-nfp-vars* ()))
      (declare (dynamic-extent *arm642-gpr-locations*))
      (set-fill-pointer
 *backend-labels*
       (set-fill-pointer
 *arm642-undo-stack*
        (set-fill-pointer
 *arm642-undo-because*
         (set-fill-pointer
 *backend-immediates* 0))))
      (backend-get-next-label)          ; start @ label 1, 0 is confused with NIL in compound cd
      (let* ((vinsns (make-vinsn-list))
             (*vinsn-list* vinsns))
        (unwind-protect
             (progn
               (setq bits (arm642-toplevel-form vinsns (make-wired-lreg *arm642-result-reg*) $backend-return (afunc-acode afunc)))
               (dotimes (i (length *backend-immediates*))
                 (let ((imm (aref *backend-immediates* i)))
                   (when (arm642-symbol-locative-p imm) (aset *backend-immediates* i (car imm)))))
               (optimize-vinsns vinsns)
               (when (logbitp arm642-debug-vinsns-bit *arm642-debug-mask*)
                 (format t "~% vinsns for ~s (after generation)" (afunc-name afunc))
                 (do-dll-nodes (v vinsns) (format t "~&~s" v))
                 (format t "~%~%"))

               (with-dll-node-freelist (code arm64::*instruction-freelist*)
                 (let* ((arm64::*labels* nil)
                        debug-info)
                     (arm642-expand-vinsns vinsns code)
                     (if (logbitp $fbitnonnullenv (the fixnum (afunc-bits afunc)))
                       (setq bits (+ bits (ash 1 $lfbits-nonnullenv-bit))))
                     (setq debug-info (afunc-lfun-info afunc))
                     (when lambda-form
                       (setq debug-info (list* 'function-lambda-expression lambda-form debug-info)))
                     (when *arm642-recorded-symbols*
                       (setq debug-info (list* 'function-symbol-map *arm642-recorded-symbols* debug-info)))
                     (when (and (getf debug-info '%function-source-note) *arm642-emitted-source-notes*)
                       (setq debug-info (list* 'pc-source-map *arm642-emitted-source-notes* debug-info)))
                     (when debug-info
                       (setq bits (logior (ash 1 $lfbits-info-bit) bits))
                       (backend-new-immediate debug-info))
                     (if (or fname lambda-form *arm642-recorded-symbols*)
                       (backend-new-immediate fname)
                       (setq bits (logior (ash -1 $lfbits-noname-bit) bits)))

                     (unless (afunc-parent afunc)
                       (arm642-fixup-fwd-refs afunc))
                     (setf (afunc-all-vars afunc) nil)
                     (setf (afunc-argsword afunc) bits)
                     (setf (afunc-lfun afunc)
                           (arm642-xmake-function
                            code
 *backend-immediates*
                            bits))
                     (when (getf debug-info 'pc-source-map)
                       (setf (getf debug-info 'pc-source-map)
                             (arm642-generate-pc-source-map debug-info)))
                     (when (getf debug-info 'function-symbol-map)
                       (setf (getf debug-info 'function-symbol-map)
                             (arm642-digest-symbols)))))))))
    afunc))

(defun arm642-xmake-function (code imms bits)
  (collect ((lap-imms))
    (dotimes (i (length imms))
      (lap-imms (cons (aref imms i) i)))
    (let* ((arm64::*constants* (lap-imms)))
      (arm64-lap-generate-code code (arm64::finalize code) bits))))

(defun arm642-make-stack (size &optional (subtype target::subtag-s16-vector))
  (make-uarray-1 subtype size t 0 nil nil nil nil t nil))

(defun arm642-fixup-fwd-refs (afunc)
  (dolist (f (afunc-inner-functions afunc))
    (arm642-fixup-fwd-refs f))
  (let ((fwd-refs (afunc-fwd-refs afunc)))
    (when fwd-refs
 ;; ARM64-DEVIATION (): afunc-lfun is now fulltag_function on
 ;; the resident path; %svref/uvsize below are misc accessors, so
 ;; take a misc view. Cross-compile (host != target) leaves it raw.
      (let* ((v (let ((raw (afunc-lfun afunc)))
                  (if (eq *host-backend* *target-backend*)
                    (%function-to-function-vector raw)
                    raw)))
             (vlen (uvsize v)))
        (declare (fixnum vlen))
        (dolist (ref fwd-refs)
          (let* ((ref-fun (afunc-lfun ref)))
            (do* ((i 1 (1+ i)))
                 ((= i vlen))
              (declare (fixnum i))
              (if (eq (%svref v i) ref)
                (setf (%svref v i) ref-fun)))))))))

(eval-when (:compile-toplevel)
  (declaim (inline arm642-invalidate-regmap)))

(defun arm642-invalidate-regmap ()
  (setq *arm642-gpr-locations-valid-mask* 0
 *arm642-gpr-constants-valid-mask* 0))

(defun arm642-update-regmap (vinsn)
  (if (vinsn-attribute-p vinsn :call)
    (arm642-invalidate-regmap)
    (let* ((clobbered-regs (vinsn-gprs-set vinsn)))
      (setq *arm642-gpr-locations-valid-mask*
            (logandc2 *arm642-gpr-locations-valid-mask* clobbered-regs)
 *arm642-gpr-constants-valid-mask*
            (logandc2 *arm642-gpr-constants-valid-mask* clobbered-regs))))
  vinsn)

(defun arm642-invalidate-regmap-entry (i loc)
  (when (and (logbitp i *arm642-gpr-locations-valid-mask*)
             (memq loc (svref *arm642-gpr-locations* i)))
    (when (null (setf (svref *arm642-gpr-locations* i)
                      (delete loc (svref *arm642-gpr-locations* i))))
      (setq *arm642-gpr-locations-valid-mask*
            (logandc2 *arm642-gpr-locations-valid-mask* (ash 1 i))))))

(defun arm642-regmap-note-store (gpr loc)
  (let* ((gpr (%hard-regspec-value gpr)))
 ;; Any other GPRs that had contained loc no longer do so.
    (dotimes (i 32)
      (unless (eql i gpr)
        (arm642-invalidate-regmap-entry i loc)))
    (if (logbitp gpr *arm642-gpr-locations-valid-mask*)
      (push loc (svref *arm642-gpr-locations* gpr))
      (setf (svref *arm642-gpr-locations* gpr) (list loc)))
    (setq *arm642-gpr-locations-valid-mask*
          (logior *arm642-gpr-locations-valid-mask* (ash 1 gpr)))))

;;; For vpush: nothing else should claim to contain loc.
(defun arm642-regmap-note-reg-location (gpr loc)
  (let* ((gpr (%hard-regspec-value gpr)))
    (if (logbitp gpr *arm642-gpr-locations-valid-mask*)
      (push loc (svref *arm642-gpr-locations* gpr))
      (setf (svref *arm642-gpr-locations* gpr) (list loc)))
    (setq *arm642-gpr-locations-valid-mask*
          (logior *arm642-gpr-locations-valid-mask* (ash 1 gpr)))))

(defun arm642-regmap-note-vstack-delta (new old)
  (when (< new old)
    (let* ((mask *arm642-gpr-locations-valid-mask*)
           (info *arm642-gpr-locations*))
    (unless (eql 0 mask)
      (dotimes (i 32 (setq *arm642-gpr-locations-valid-mask* mask))
        (when (logbitp i mask)
          (let* ((locs (svref info i))
                 (head (cons nil locs))
                 (tail head))
            (declare (dynamic-extent head))
            (dolist (loc locs)
              (if (>= loc new)
                (setf (cdr tail) (cddr tail))
                (setq tail (cdr tail))))
            (when (null (setf (svref info i) (cdr head)))
              (setq mask (logandc2 mask (ash 1 i)))))))))))

(defun arm642-copy-regmap (mask from to)
  (dotimes (i 32)
    (when (logbitp i mask)
      (setf (svref to i) (copy-list (svref from i))))))

(defun arm642-copy-constmap (mask from to)
  (dotimes (i 32)
    (when (logbitp i mask)
      (setf (svref to i) (svref from i)))))

(defmacro with-arm642-saved-regmaps ((mask constmask map constmap) &body body)
  `(let* ((,mask *arm642-gpr-locations-valid-mask*)
          (,constmask *arm642-gpr-constants-valid-mask*)
          (,map (make-array 32 :initial-element nil))
          (,constmap (make-array 32)))
    (declare (dynamic-extent ,map ,constmap))
    (arm642-copy-regmap ,mask *arm642-gpr-locations* ,map)
    (arm642-copy-constmap ,constmask *arm642-gpr-constants* ,constmap)
    ,@body))

(defun arm642-generate-pc-source-map (debug-info)
  (let* ((definition-source-note (getf debug-info '%function-source-note))
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
      (let* ((pc-start (arm642-vinsn-note-label-address start t))
             (pc-end (arm642-vinsn-note-label-address (vinsn-note-peer start)
                                                      nil))
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
           (vec (cond
                  ((< max #x100)
                   (make-array nentries :element-type '(unsigned-byte 8)))
                  ((< max #x10000)
                   (make-array nentries :element-type '(unsigned-byte 16)))
                  (t
                   (make-array nentries :element-type '(unsigned-byte 32))))))
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

(defun arm642-vinsn-note-label-address (note &optional start-p sym)
  (let* ((lap-label (vinsn-note-address note)))
    (if lap-label
      (arm64::label-address lap-label)
      (compiler-bug "Missing or bad ~s label: ~s"
                    (if start-p 'start 'end) sym))))

(defun arm642-digest-symbols ()
  (when *arm642-recorded-symbols*
    (setq *arm642-recorded-symbols* (nx2-recorded-symbols-in-arglist-order
 *arm642-recorded-symbols*
 *arm642-cur-afunc*))
    (let* ((symlist *arm642-recorded-symbols*)
           (len (length symlist))
           (syms (make-array len))
           (ptrs (make-array (%i+  (%i+ len len) len)
                             :element-type '(unsigned-byte 32)))
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
          (setf (aref ptrs (incf i))
                (arm642-vinsn-note-label-address startlab t sym))
          (setf (aref ptrs (incf i))
                (arm642-vinsn-note-label-address endlab nil sym))))
 *arm642-recorded-symbols*)))

(defun arm642-decls (decls)
  (if (fixnump decls)
    (locally (declare (fixnum decls))
      (setq *arm642-tail-allow* (neq 0 (%ilogand2 $decl_tailcalls decls))
 *arm642-open-code-inline* (neq 0 (%ilogand2 $decl_opencodeinline
                                                        decls))
 *arm642-full-safety* (neq 0 (%ilogand2 $decl_full_safety decls))
 *arm642-reckless* (neq 0 (%ilogand2 $decl_unsafe decls))
 *arm642-float-safety* (neq 0 (%ilogand2 $decl_float_safety decls))
 *arm642-trust-declarations* (neq 0 (%ilogand2 $decl_trustdecls
                                                          decls))))))

;;; Vpush the first N non-volatile-registers.
(defun arm642-save-nvrs (seg n)
  (declare (fixnum n))
  (when (> n 0)
    (setq *arm642-compiler-register-save-note* (enqueue-vinsn-note seg :regsave))
    (with-arm64-local-vinsn-macros (seg)
      (! save-nvrs n))
    (incf *arm642-vstack* (the fixnum (* n *arm642-target-node-size*)))
    (setq *arm642-register-restore-ea* *arm642-vstack*
 *arm642-register-restore-count* n)))

(defun arm642-restore-nvrs (seg multiple-values-on-stack)
  (let* ((ea *arm642-register-restore-ea*)
         (n *arm642-register-restore-count*))
    (when (and ea n)
      (with-arm64-local-vinsn-macros (seg)
        (let* ((diff (- *arm642-vstack* ea)))
          (if (and (eql 0 diff)
                   (not multiple-values-on-stack))
            (! restore-nvrs n arm64::vsp)
            (let* ((reg (make-unwired-lreg
                         (if (= *available-backend-imm-temps* 0)
                           (select-node-temp)
                           (select-imm-temp))
                         :class hard-reg-class-gpr
                         :mode hard-reg-class-gpr-mode-node)))
              (if (eql 0 diff)
                (! fixnum-add reg arm64::vsp arm64::nargs)
                (progn
                  (if (< diff 4096)
                    (! add-immediate reg arm64::vsp diff)
                    (progn
                      (arm642-lri seg reg diff)
                      (! fixnum-add reg arm64::vsp reg)))
                  (when multiple-values-on-stack
                    (! fixnum-add reg reg arm64::nargs))))
              (! restore-nvrs n reg))))))))

;;; The 32-bit ARM port does this, but no others do.
(defun arm642-save-non-volatile-fprs (seg n)
  (declare (ignore seg n)))

(defun arm642-restore-non-volatile-fprs (seg)
  (declare (ignore seg)))

(defun arm642-bind-lambda (seg req opt rest keys auxen optsupvloc passed-in-regs lexpr &optional inherited
                             &aux (vloc 0)
                             (nkeys (list-length (%cadr keys)))
                             reg)
  (declare (fixnum vloc))
  (dolist (arg inherited)
    (if (memq arg passed-in-regs)
      (arm642-set-var-ea seg arg (var-ea arg))
      (progn
        (if (setq reg (nx2-assign-register-var arg))
          (arm642-init-regvar seg arg reg (arm642-vloc-ea vloc))
          (arm642-bind-var seg arg vloc))
        (setq vloc (%i+ vloc *arm642-target-node-size*)))))
  (dolist (arg req)
    (if (memq arg passed-in-regs)
      (arm642-set-var-ea seg arg (var-ea arg))
      (progn
        (if (setq reg (nx2-assign-register-var arg))
          (arm642-init-regvar seg arg reg (arm642-vloc-ea vloc))
          (arm642-bind-var seg arg vloc))
        (setq vloc (%i+ vloc *arm642-target-node-size*)))))
  (when opt
    (if (arm642-hard-opt-p opt)
      (setq vloc (apply #'arm642-initopt seg vloc optsupvloc opt))
      (dolist (var (%car opt))
        (if (memq var passed-in-regs)
          (arm642-set-var-ea seg var (var-ea var))
          (progn
            (if (setq reg (nx2-assign-register-var var))
              (arm642-init-regvar seg var reg (arm642-vloc-ea vloc))
              (arm642-bind-var seg var vloc))
            (setq vloc (+ vloc *arm642-target-node-size*)))))))
  (when rest
    (if lexpr
      (progn
        (if (setq reg (nx2-assign-register-var rest))
          (progn
            (arm642-load-lexpr-address seg reg)
            (arm642-set-var-ea seg rest reg))
          (with-imm-temps () ((nargs-cell :natural))
            (arm642-load-lexpr-address seg nargs-cell)
            (let* ((loc *arm642-vstack*))
              (arm642-vpush-register seg nargs-cell)
              (arm642-bind-var seg rest loc)))))
      (let* ((rvloc (+ vloc (* 2 *arm642-target-node-size* nkeys))))
        (if (setq reg (nx2-assign-register-var rest))
          (arm642-init-regvar seg rest reg (arm642-vloc-ea rvloc))
          (arm642-bind-var seg rest rvloc)))))
  (when keys
    (apply #'arm642-init-keys seg vloc  keys))
  (arm642-seq-bind seg (%car auxen) (%cadr auxen)))

(defun arm642-initopt (seg vloc spvloc vars inits spvars)
  (with-arm64-local-vinsn-macros (seg)
    (dolist (var vars vloc)
      (let* ((initform (pop inits))
             (spvar (pop spvars))
             (reg (nx2-assign-register-var var))
             (sp-reg ($ arm64::arg_z))
             (regloadedlabel (if reg (backend-get-next-label))))
        (unless (nx-null initform)
          (arm642-stack-to-register seg (arm642-vloc-ea spvloc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (arm642-compare-register-to-nil seg crf
                                              (arm642-make-compound-cd
                                               0 skipinitlabel)
                                              sp-reg arm64::cond-eq t))
            (if reg
              (arm642-form seg reg regloadedlabel initform)
              (arm642-register-to-stack seg
                                        (arm642-one-untargeted-reg-form
                                         seg initform ($ arm64::arg_z))
                                        (arm642-vloc-ea vloc)))
            (@ skipinitlabel)))
        (if reg
          (progn
            (arm642-init-regvar seg var reg (arm642-vloc-ea vloc))
            (@ regloadedlabel))
          (arm642-bind-var seg var vloc))
        (when spvar
          (if (setq reg (nx2-assign-register-var spvar))
            (arm642-init-regvar seg spvar reg (arm642-vloc-ea spvloc))
            (arm642-bind-var seg spvar spvloc))))
      (setq vloc (%i+ vloc *arm642-target-node-size*))
      (if spvloc (setq spvloc (%i+ spvloc *arm642-target-node-size*))))))

(defun arm642-init-keys (seg vloc allow-others keyvars keysupp keyinits keykeys)
  (declare (ignore keykeys allow-others))
  (with-arm64-local-vinsn-macros (seg)
    (dolist (var keyvars)
      (let* ((spvar (pop keysupp))
             (initform (pop keyinits))
             (reg (nx2-assign-register-var var))
             (regloadedlabel (if reg (backend-get-next-label)))
             (sp-reg ($ arm64::arg_z))
             (sploc (%i+ vloc *arm642-target-node-size*)))
        (unless (nx-null initform)
          (arm642-stack-to-register seg (arm642-vloc-ea sploc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (arm642-compare-register-to-nil seg crf (arm642-make-compound-cd
                                                       0 skipinitlabel)
                                              sp-reg arm64::cond-eq t))
            (if reg
              (arm642-form seg reg regloadedlabel initform)
              (arm642-register-to-stack seg (arm642-one-untargeted-reg-form
                                             seg initform ($ arm64::arg_z))
                                        (arm642-vloc-ea vloc)))
            (@ skipinitlabel)))
        (if reg
          (progn
            (arm642-init-regvar seg var reg (arm642-vloc-ea vloc))
            (@ regloadedlabel))
          (arm642-bind-var seg var vloc))
        (when spvar
          (if (setq reg (nx2-assign-register-var spvar))
            (arm642-init-regvar seg spvar reg (arm642-vloc-ea sploc))
            (arm642-bind-var seg spvar sploc))))
      (setq vloc (%i+ vloc (* 2 *arm642-target-node-size*))))))

;;; Return NIL if arg register should be vpushed, else var.
(defun arm642-retain-arg-register (var)
  (if var
    (when (var-nvr var)
      var)
    (compiler-bug "Missing var!")))

;;; nargs has been validated, arguments defaulted and canonicalized.
;;; Save caller's context, then vpush any argument registers that
;;; didn't get global registers assigned to their variables.
;;; Return a list of vars/nils for each argument register
;;; (nil if vpushed, var if still in arg_reg).
(defun arm642-argregs-entry (seg revargs)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((nargs (length revargs))
           (reg-vars ()))
      (declare (type (unsigned-byte 16) nargs))
      (when (and
             (<= nargs $numarm64argregs)
             (not (some #'null revargs)))
        (setq *arm642-fixed-nargs* nargs))
      (if (<= nargs $numarm64argregs)       ; caller didn't vpush anything
        (! save-lisp-context-no-stack-args)
        (let* ((offset (* (the fixnum (- nargs $numarm64argregs))
 *arm642-target-node-size*)))
          (declare (fixnum offset))
          (! save-lisp-context-offset offset)))
      (when *arm642-fixed-args-label*
        (@ (setq *arm642-fixed-args-tail-label* (backend-get-next-label))))
      (destructuring-bind (&optional zvar yvar xvar &rest stack-args) revargs
        (let* ((nstackargs (length stack-args)))
          (arm642-set-vstack (* nstackargs *arm642-target-node-size*))
 ;; ARM64: no vpush-multiple-registers, push individually
          (when (>= nargs 3)
            (let* ((retain-x (arm642-retain-arg-register xvar)))
              (push retain-x reg-vars)
              (unless retain-x
                (arm642-regmap-note-store arm64::arg_x *arm642-vstack*)
                (arm642-adjust-vstack *arm642-target-node-size*)
                (! vpush-register ($ arm64::arg_x)))))
          (when (>= nargs 2)
            (let* ((retain-y (arm642-retain-arg-register yvar)))
              (push retain-y reg-vars)
              (unless retain-y
                (arm642-regmap-note-store arm64::arg_y *arm642-vstack*)
                (arm642-adjust-vstack *arm642-target-node-size*)
                (! vpush-register ($ arm64::arg_y)))))
          (when (>= nargs 1)
            (let* ((retain-z (arm642-retain-arg-register zvar)))
              (push retain-z reg-vars)
              (unless retain-z
                (arm642-regmap-note-store arm64::arg_z *arm642-vstack*)
                (arm642-adjust-vstack *arm642-target-node-size*)
                (! vpush-register ($ arm64::arg_z)))))))
      reg-vars)))

(defun arm642-req-nargs-entry (seg rev-fixed-args)
  (let* ((nargs (length rev-fixed-args)))
    (declare (type (unsigned-byte 16) nargs))
    (with-arm64-local-vinsn-macros (seg)
      (unless *arm642-reckless*
        (if (arm642-aimm-p (ash nargs arm64::fixnumshift))
          (! check-exact-nargs nargs)
          (! check-exact-nargs-large nargs)))
      (arm642-argregs-entry seg rev-fixed-args))))

;;; No more than three &optional args; all default to NIL and none have
;;; supplied-p vars. No &key/&rest.
(defun arm642-simple-opt-entry (seg rev-opt-args rev-req-args)
  (let* ((min (length rev-req-args))
         (nopt (length rev-opt-args))
         (max (+ min nopt)))
    (declare (type (unsigned-byte 16) min nopt max))
    (with-arm64-local-vinsn-macros (seg)
      (unless *arm642-reckless*
        (when rev-req-args
          (! check-min-nargs min))
        (! check-max-nargs max))
      (if (= nopt 1)
        (! default-1-arg min)
        (if (= nopt 2)
          (! default-2-args min)
          (! default-3-args min)))
      (arm642-argregs-entry seg (append rev-opt-args rev-req-args)))))

;;; if "num-fixed" is > 0, we've already ensured that at least that many args
;;; were provided; that may enable us to generate better code for saving the
;;; argument registers.
;;; We're responsible for computing the caller's VSP and saving
;;; caller's state.
(defun arm642-lexpr-entry (seg num-fixed)
  (with-arm64-local-vinsn-macros (seg)
    (! save-lexpr-argregs num-fixed)
    (dotimes (i num-fixed)
      (! copy-lexpr-argument))
    (! save-lisp-context-lexpr)))

(defun arm642-load-lexpr-address (seg dest)
  (with-arm64-local-vinsn-macros (seg)
    (! load-vframe-address dest *arm642-vstack*)))

(defun arm642-vloc-ea (n &optional vcell-p)
  (setq n (make-memory-spec (dpb memspec-frame-address memspec-type-byte n)))
  (if vcell-p
    (make-vcell-memory-spec n)
    n))

(defun arm642-acode-operator-function (form)
  (or (and (acode-p form)
           (svref *arm642-specials*
                  (%ilogand #.operator-id-mask (acode-operator form))))
      (compiler-bug "arm642-form ? ~s" form)))

(defmacro arm64-with-note ((form-var seg-var &rest other-vars) &body body)
  (let* ((note (gensym "NOTE"))
         (code-note (gensym "CODE-NOTE"))
         (source-note (gensym "SOURCE-NOTE"))
         (start (gensym "START"))
         (arm64-with-note-body (gensym "ARM64-WITH-NOTE-BODY")))
    `(flet ((,arm64-with-note-body (,form-var ,seg-var ,@other-vars) ,@body))
       (let ((,note (acode-note ,form-var)))
         (if ,note
           (let* ((,code-note (and ,note (code-note-p ,note) ,note))
                  (,source-note (if ,code-note
                                  (code-note-source-note ,note)
                                  ,note))
                  (,start (and ,source-note
                               (enqueue-vinsn-note ,seg-var :source-location-begin ,source-note))))
             (prog2
                 (when ,code-note
                   (with-arm64-local-vinsn-macros (,seg-var)
                     (arm642-store-immediate ,seg-var ,code-note arm64::temp0)
                     (with-node-temps (arm64::temp0) (zero)
                       (! lri zero 0)
                       (! misc-set-c-node ($ zero) ($ arm64::temp0) 1))))
                 (,arm64-with-note-body ,form-var ,seg-var ,@other-vars)
               (when ,source-note
                 (close-vinsn-note ,seg-var ,start))))
           (,arm64-with-note-body ,form-var ,seg-var ,@other-vars))))))

(defun arm642-toplevel-form (seg vreg xfer form)
  (let* ((code-note (acode-note form))
         (args (if code-note
                 `(,@(acode-operands form) ,code-note)
                 (acode-operands form))))
    (apply (arm642-acode-operator-function form) seg vreg xfer args)))

(defun arm642-form (seg vreg xfer form)
  (arm64-with-note (form seg vreg xfer)
    (if (nx-null form)
      (arm642-nil seg vreg xfer)
      (if (nx-t form)
        (arm642-t seg vreg xfer)
        (let ((fn (arm642-acode-operator-function form))
              (op (acode-operator form)))
          (if (and (null vreg)
                   (%ilogbitp operator-acode-subforms-bit op)
                   (%ilogbitp operator-assignment-free-bit op)
                   (%ilogbitp operator-side-effect-free-bit op))
            (dolist (f (acode-operands form) (arm642-branch seg xfer nil))
              (arm642-form seg nil nil f))
            (apply fn seg vreg xfer (acode-operands form))))))))

(defun arm642-form-typep (form type)
  (acode-form-typep form type *arm642-trust-declarations*))

(defun arm642-form-type (form)
  (acode-form-type form *arm642-trust-declarations*))

(defun arm642-use-operator (op seg vreg xfer &rest forms)
  (declare (dynamic-extent forms))
  (apply (svref *arm642-specials* (%ilogand operator-id-mask op))
         seg vreg xfer forms))

(defun arm642-nil (seg vreg xfer)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (if (arm642-for-value-p vreg)
      (ensuring-node-target (target vreg)
        (! load-nil target)))
    (arm642-branch seg (arm642-cd-false xfer) vreg)))

(defun arm642-t (seg vreg xfer)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (if (arm642-for-value-p vreg)
      (ensuring-node-target (target vreg)
        (! load-t target)))
    (arm642-branch seg (arm642-cd-true xfer) vreg)))

(defun arm642-for-value-p (vreg)
  (and vreg (not (backend-crf-p vreg))))

(defun arm642-mvpass (seg form &optional xfer)
  (with-arm64-local-vinsn-macros (seg)
    (arm642-form seg ($ arm64::arg_z)
                 (logior (or xfer 0) $backend-mvpass-mask) form)))

(defun arm642-adjust-vstack (delta)
  (arm642-set-vstack (%i+ *arm642-vstack* delta)))

(defun arm642-set-vstack (new)
  (arm642-regmap-note-vstack-delta new *arm642-vstack*)
  (setq *arm642-vstack* new))

(defun arm642-register-for-frame-offset (offset &optional suggested)
  (let* ((mask *arm642-gpr-locations-valid-mask*)
         (info *arm642-gpr-locations*))
    (if (and suggested
             (logbitp suggested mask)
             (memq offset (svref info suggested)))
      suggested
      (dotimes (reg 32)
        (when (and (logbitp reg mask)
                   (memq offset (svref info reg)))
          (return reg))))))

(defun arm642-reg-for-ea (ea)
  (when (and (memory-spec-p ea)
             (eql (memspec-type ea) memspec-frame-address)
             (not (addrspec-vcell-p ea)))
    (let* ((offset (memspec-frame-address-offset ea))
           (mask *arm642-gpr-locations-valid-mask*)
           (info *arm642-gpr-locations*))
      (declare (fixnum mask) (simple-vector info))
      (dotimes (reg 32)
        (when (and (logbitp reg mask)
                   (memq offset (svref info reg)))
          (return reg))))))

(defun arm642-reg-for-form (form hint)
  (let* ((var (arm642-lexical-reference-p form)))
    (cond ((node-reg-p hint)
           (if var
             (arm642-reg-for-ea (var-ea var))
             (multiple-value-bind (value constantp) (acode-constant-p form)
               (when constantp
                 (let* ((regs (arm642-gprs-containing-constant value))
                        (regno (hard-regspec-value hint)))
                   (if (logbitp regno regs)
                     hint
                     (unless (eql 0 regs)
                       (1- (integer-length regs)))))))))
          ((eql (hard-regspec-class hint) hard-reg-class-fpr)
           (if var
             (let* ((ea (var-ea var)))
               (when (register-spec-p ea)
                 (and (eql (hard-regspec-class ea) hard-reg-class-fpr)
                      (eql (get-regspec-mode ea) (get-regspec-mode hint))
                      ea)))
 ;; No zero FPR; just use fmov Dd, XZR
             nil)))))

(defun arm642-stack-to-register (seg memspec reg)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((offset (memspec-frame-address-offset memspec)))
      (if (eql (hard-regspec-class reg) hard-reg-class-fpr)
        (with-node-target () temp
          (arm642-stack-to-register seg memspec temp)
          (arm642-copy-register seg reg temp))
        (let* ((mask *arm642-gpr-locations-valid-mask*)
               (info *arm642-gpr-locations*)
               (regno (%hard-regspec-value reg))
               (other (arm642-register-for-frame-offset offset regno)))
          (unless (eql regno other)
            (cond (other
                   (let* ((vinsn (! copy-node-gpr reg other)))
                     (setq *arm642-gpr-locations-valid-mask*
                           (logior mask (ash 1 regno)))
                     (setf (svref info regno)
                           (copy-list (svref info other)))
                     vinsn))
                  (t
                   (let* ((vinsn (! vframe-load reg offset *arm642-vstack*)))
                     (setq *arm642-gpr-locations-valid-mask*
                           (logior mask (ash 1 regno)))
                     (setf (svref info regno) (list offset))
                     vinsn)))))))))

(defun arm642-register-to-stack (seg reg memspec)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((offset (memspec-frame-address-offset memspec))
           (vinsn (! vframe-store reg offset *arm642-vstack*)))
      (arm642-regmap-note-store (%hard-regspec-value reg) offset)
      vinsn)))

(defun arm642-ea-open (ea)
  (if (and ea (not (typep ea 'lreg)) (addrspec-vcell-p ea))
    (make-memory-spec (memspec-frame-address-offset ea))
    ea))

(defun arm642-set-nargs (seg n)
  (if (> n call-arguments-limit)
    (compiler-bug "~s exceeded." 'call-arguments-limit)
    (with-arm64-local-vinsn-macros (seg)
      (! set-nargs n))))

(defun arm642-single-float-bits (the-sf)
  (single-float-bits the-sf))

(defun arm642-double-float-bits (the-df)
  (double-float-bits the-df))

(defun arm642-immediate (seg vreg xfer form)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
               (or (and (typep form 'double-float)
                        (= (get-regspec-mode vreg)
                           hard-reg-class-fpr-mode-double))
                   (and (typep form 'short-float)
                        (= (get-regspec-mode vreg)
                           hard-reg-class-fpr-mode-single))))
        (if (zerop form)
          (if (eql form 0.0d0)
            (! zero-double-float-register vreg)
            (! zero-single-float-register vreg))
          (if (typep form 'short-float)
            (let* ((bits (arm642-single-float-bits form)))
              (with-imm-temps () ((bitsreg :u32))
                (! lri bitsreg bits)
                (! load-single-float-constant vreg bitsreg)))
            (multiple-value-bind (high low) (arm642-double-float-bits form)
              (declare (integer high low))
              (with-imm-temps () ((highreg :u32) (lowreg :u32))
                (if (zerop high)
                  (setq highreg ($ arm64::xzr))
                  (! lri highreg high))
                (if (zerop low)
                  (setq lowreg ($ arm64::xzr))
                  (! lri lowreg low))
                (! load-double-float-constant vreg highreg lowreg)))))
        (if (and (typep form '(unsigned-byte 32))
                 (= (hard-regspec-class vreg) hard-reg-class-gpr)
                 (= (get-regspec-mode vreg)
                    hard-reg-class-gpr-mode-u32))
          (arm642-lri seg vreg form)
          (ensuring-node-target
              (target vreg)
            (if (characterp form)
              (! load-character-constant target (char-code form))
              (arm642-store-immediate seg form target)))))
      (if (and (listp form)
 *load-time-eval-token*
               (eq (car form) *load-time-eval-token*))
        (arm642-store-immediate seg form ($ arm64::temp0))))
    (^)))

(defun arm642-register-constant-p (form)
  (and (acode-p form)
       (or (memq form *arm642-vcells*)
           (memq form *arm642-fcells*))
       (car (acode-operands form))))

(defun arm642-store-immediate (seg imm dest)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((reg (arm642-register-constant-p imm)))
      (if reg
        (arm642-copy-register seg dest reg)
        (let* ((idx (backend-immediate-index imm)))
 ;; The reach of (ldur dest (:@ fn (:$ (+ function.constants
 ;; (* 8 idx)))))
 ;; is relatively small: in this case, idx can be at most 31,
 ;; making the offset 249, which still fits into an (signed-byte 9).
          (if (<= idx 31)
            (! ref-constant dest idx)
            (with-imm-target () (idxreg :s64)
              (arm642-lri seg idxreg (+ arm64::function.constants (ash idx 3)))
              (! ref-indexed-constant dest idxreg)))))
      dest)))

;;; Returns label iff form is (local-go <tag>) and can go without
;;; adjusting stack.
(defun arm642-go-label (form)
  (let ((current-stack (arm642-encode-stack)))
    (while (and (acode-p form)
                (or (eq (acode-operator form) (%nx1-operator progn))
                    (eq (acode-operator form) (%nx1-operator local-tagbody))))
      (setq form (caar  (acode-operands form))))
    (when (acode-p form)
      (let ((op (acode-operator form)))
        (if (and (eq op (%nx1-operator local-go))
                 (arm642-equal-encodings-p
                  (%caddr (car (acode-operands form))) current-stack))
          (%cadr (car (acode-operands form)))
          (if (and (eq op (%nx1-operator local-return-from))
                   (nx-null (cadr (acode-operands form))))
            (let ((tagdata (car (car (acode-operands form)))))
              (and (arm642-equal-encodings-p (cdr tagdata) current-stack)
                   (null (caar tagdata))
                   (< 0 (cdar tagdata) $backend-mvpass)
                   (cdar tagdata)))))))))

(defun arm642-single-valued-form-p (form)
  (setq form (acode-unwrapped-form-value form))
  (or (nx-null form)
      (nx-t form)
      (if (acode-p form)
        (let ((op (acode-operator form)))
          (or (%ilogbitp operator-single-valued-bit op)
              (and (eql op (%nx1-operator values))
                   (let ((values (car (acode-operands form))))
                     (and values (null (cdr values)))))
              nil)))))                 ;learn about functions someday

(defun arm642-box-s64 (seg node-dest s64-src)
  (with-arm64-local-vinsn-macros (seg)
    (if *arm642-open-code-inline*
      (! s64->integer node-dest s64-src)
      (let ((arg_z ($ arm64::arg_z))
            (imm0 ($ arm64::imm0 :mode :s64)))
        (arm642-copy-register seg imm0 s64-src)
        (! call-subprim (subprim-name->offset '.SPmakes64))
        (arm642-copy-register seg node-dest arg_z)))))

(defun arm642-box-u64 (seg node-dest u64-src)
  (with-arm64-local-vinsn-macros (seg)
    (if *arm642-open-code-inline*
      (! u64->integer node-dest u64-src)
      (let ((arg_z ($ arm64::arg_z))
            (imm0 ($ arm64::imm0 :mode :u64)))
        (arm642-copy-register seg imm0 u64-src)
        (! call-subprim (subprim-name->offset '.SPmakeu64))
        (arm642-copy-register seg node-dest arg_z)))))

;;; A u32 or an s32 always fits in a 61-bit fixnum, so boxing one is a
;;; shift -- no subprim call, no bignum case, and nothing to condition on
;;; *arm642-open-code-inline* (PPC64 makes exactly this distinction:
;;; ppc2-box-u32/-box-s32 take the inline path unconditionally under
;;; :ppc64 and only ppc32 can reach .SPmakeu32/.SPmakes32; ppc2.lisp:1188
;;; and :1212). u32->fixnum / s32->fixnum are the ubfiz/sbfiz forms of
;;; PPC64's u32->integer / s32->integer (sldi, and extsw+sldi).
;;;
;;; arm642-vref1 calls both for :signed-32-bit-vector and for the default
;;; 32-bit element case, so a cross-compile of level-0 stops in l0-array
;;; with "Undefined function CCL::ARM642-BOX-U32". arm642-box-u64 was
;;; already here; these two are its missing siblings.
(defun arm642-box-u32 (seg node-dest u32-src)
  (with-arm64-local-vinsn-macros (seg)
    (! u32->fixnum node-dest u32-src)))

(defun arm642-box-s32 (seg node-dest s32-src)
  (with-arm64-local-vinsn-macros (seg)
    (! s32->fixnum node-dest s32-src)))

(defun arm642-vref1 (seg vreg xfer type-keyword src unscaled-idx
                     index-known-fixnum)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (let* ((arch (backend-target-arch *target-backend*))
             (is-node (member type-keyword (arch::target-gvector-types arch)))
             (is-1-bit (member type-keyword
                               (arch::target-1-bit-ivector-types arch)))
             (is-8-bit (member type-keyword
                               (arch::target-8-bit-ivector-types arch)))
             (is-16-bit (member type-keyword
                                (arch::target-16-bit-ivector-types arch)))
             (is-32-bit (member type-keyword
                                (arch::target-32-bit-ivector-types arch)))
             (is-64-bit (member type-keyword
                                (arch::target-64-bit-ivector-types arch)))
             (is-128-bit (eq type-keyword :complex-double-float-vector))
             (is-signed (member type-keyword '(:signed-8-bit-vector
                                               :signed-16-bit-vector
                                               :signed-32-bit-vector
                                               :signed-64-bit-vector
                                               :fixnum-vector)))
             (vreg-class (hard-regspec-class vreg))
             (vreg-mode
              (if (or (eql vreg-class hard-reg-class-gpr)
                      (eql vreg-class hard-reg-class-fpr))
                (get-regspec-mode vreg)
                hard-reg-class-gpr-mode-invalid))
             (temp-is-vreg nil))
        (cond
          (is-node
           (ensuring-node-target (target vreg)
             (if (and index-known-fixnum
                      (<= index-known-fixnum
                          (arch::target-max-64-bit-constant-index arch)))
               (! misc-ref-c-node target src index-known-fixnum)
               (with-imm-target () (idx-reg :u64)
                 (if index-known-fixnum
                   (arm642-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum *arm642-target-node-shift*)))
                   (! scale-node-misc-index idx-reg unscaled-idx))
                 (! misc-ref-node target src idx-reg)))))
          (is-32-bit
           (with-imm-target () (temp :u32)
             (with-fp-target () (fp-val :single-float)
               (if (eql vreg-class hard-reg-class-gpr)
                 (if
                   (if is-signed
                     (or (eql vreg-mode hard-reg-class-gpr-mode-s32)
                         (eql vreg-mode hard-reg-class-gpr-mode-s64))
                     (or (eql vreg-mode hard-reg-class-gpr-mode-u32)
                         (eql vreg-mode hard-reg-class-gpr-mode-u64)))
                   (setq temp vreg temp-is-vreg t)
                   (if is-signed
                     (set-regspec-mode temp hard-reg-class-gpr-mode-s32)))
                 (if (and (eql vreg-class hard-reg-class-fpr)
                          (eql vreg-mode hard-reg-class-fpr-mode-single))
                   (setf fp-val vreg temp-is-vreg t)))
               (if (and index-known-fixnum
                        (<= index-known-fixnum
                            (if (eq type-keyword :single-float-vector)
                              255
                              (arch::target-max-32-bit-constant-index arch))))
                 (cond ((eq type-keyword :single-float-vector)
                        (! misc-ref-c-single-float fp-val src index-known-fixnum))
                       (t
                        (if is-signed
                          (! misc-ref-c-s32 temp src index-known-fixnum)
                          (! misc-ref-c-u32 temp src index-known-fixnum))))
                 (with-imm-target () idx-reg
                   (if index-known-fixnum
                     (arm642-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 2)))
                     (! scale-32bit-misc-index idx-reg unscaled-idx))
                   (cond ((eq type-keyword :single-float-vector)
                          (! misc-ref-single-float fp-val src idx-reg))
                         (t
                          (if is-signed
                            (! misc-ref-s32 temp src idx-reg)
                            (! misc-ref-u32 temp src idx-reg))))))
               (case type-keyword
                 (:single-float-vector
                  (if (eq vreg-class hard-reg-class-fpr)
                    (<- fp-val)
                    (ensuring-node-target (target vreg)
                      (! single->node target fp-val))))
                 (:signed-32-bit-vector
                  (unless temp-is-vreg
                    (ensuring-node-target (target vreg)
                      (arm642-box-s32 seg target temp))))
                 (:fixnum-vector
                  (unless temp-is-vreg
                    (ensuring-node-target (target vreg)
                      (! box-fixnum target temp))))
                 (:simple-string
                  (ensuring-node-target (target vreg)
                    (! u32->char target temp)))
                 (t
                  (unless temp-is-vreg
                    (ensuring-node-target (target vreg)
                      (arm642-box-u32 seg target temp))))))))
          (is-8-bit
           (with-imm-target () (temp :u8)
             (if (and (eql vreg-class hard-reg-class-gpr)
                      (or
                       (and is-signed
                            (or (eql vreg-mode hard-reg-class-gpr-mode-s8)
                                (eql vreg-mode hard-reg-class-gpr-mode-s16)
                                (eql vreg-mode hard-reg-class-gpr-mode-s32)
                                (eql vreg-mode hard-reg-class-gpr-mode-s64)))
                       (and (not is-signed)
                            (or (eql vreg-mode hard-reg-class-gpr-mode-u8)
                                (eql vreg-mode hard-reg-class-gpr-mode-s16)
                                (eql vreg-mode hard-reg-class-gpr-mode-u16)
                                (eql vreg-mode hard-reg-class-gpr-mode-s32)
                                (eql vreg-mode hard-reg-class-gpr-mode-u32)
                                (eql vreg-mode hard-reg-class-gpr-mode-s64)
                                (eql vreg-mode hard-reg-class-gpr-mode-u64)))))
               (setq temp vreg temp-is-vreg t)
               (if is-signed
                 (set-regspec-mode temp hard-reg-class-gpr-mode-s8)))
             (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-8-bit-constant-index arch)))
               (if is-signed
                 (! misc-ref-c-s8 temp src index-known-fixnum)
                 (! misc-ref-c-u8 temp src index-known-fixnum))
               (with-imm-target () idx-reg
                 (if index-known-fixnum
                   (arm642-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) index-known-fixnum))
                   (! scale-8bit-misc-index idx-reg unscaled-idx))
                 (if is-signed
                   (! misc-ref-s8 temp src idx-reg)
                   (! misc-ref-u8 temp src idx-reg))))
             (ecase type-keyword
               (:unsigned-8-bit-vector
                (unless temp-is-vreg
                  (ensuring-node-target (target vreg)
                    (! box-fixnum target temp))))
               (:signed-8-bit-vector
                (unless temp-is-vreg
                  (ensuring-node-target (target vreg)
                    (! box-fixnum target temp))))
               (:simple-string
                (ensuring-node-target (target vreg)
                  (! u32->char target temp))))))
          (is-16-bit
           (ensuring-node-target (target vreg)
             (with-imm-target () temp
               (if (and index-known-fixnum
                        (<= index-known-fixnum (arch::target-max-16-bit-constant-index arch)))
                 (if is-signed
                   (! misc-ref-c-s16 temp src index-known-fixnum)
                   (! misc-ref-c-u16 temp src index-known-fixnum))
                 (with-imm-target () idx-reg
                   (if index-known-fixnum
                     (arm642-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 1)))
                     (! scale-16bit-misc-index idx-reg unscaled-idx))
                   (if is-signed
                     (! misc-ref-s16 temp src idx-reg)
                     (! misc-ref-u16 temp src idx-reg))))
               (! box-fixnum target temp))))
          (is-64-bit
           (case type-keyword
             (:double-float-vector
              (with-fp-target () (fp-val :double-float)
                (if (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-double))
                  (setq fp-val vreg))
                (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                  (! misc-ref-c-double-float fp-val src index-known-fixnum)
                  (with-imm-target () idx-reg
                    (if index-known-fixnum
                      (arm642-absolute-natural
                       seg idx-reg nil
                       (+ (arch::target-misc-data-offset arch)
                          (ash index-known-fixnum arm64::word-shift)))
                      (! scale-64bit-misc-index idx-reg unscaled-idx))
                    (! misc-ref-double-float fp-val src idx-reg)))
                (if (eq vreg-class hard-reg-class-fpr)
                  (<- fp-val)
                  (ensuring-node-target (target vreg)
                    (! double->heap target fp-val)))))
             (:complex-single-float-vector
              (with-fp-target () (fp-val :complex-single-float)
                (if (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-complex-single-float))
                  (setq fp-val vreg))
                (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                  (! misc-ref-c-double-float fp-val src index-known-fixnum)
                  (with-imm-target () idx-reg
                    (if index-known-fixnum
                      (arm642-absolute-natural
                       seg idx-reg nil
                       (+ (arch::target-misc-data-offset arch)
                          (ash index-known-fixnum arm64::word-shift)))
                      (! scale-64bit-misc-index idx-reg unscaled-idx))
                    (! misc-ref-double-float fp-val src idx-reg)))
                (if (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-complex-single-float))
                  (<- fp-val)
                  (ensuring-node-target (target vreg)
                    (! complex-single-float->node target fp-val)))))
 ;; The integer 64-bit element types. Without these the CASE fell
 ;; off the end and emitted NOTHING, so a compiled AREF on a
 ;; (simple-array (signed-byte 64)) left VREG holding whatever was
 ;; already there -- in practice the index, so (aref a i) returned i.
 ;; PPC64 ppc2.lisp:1410-1432 has the shape; the index plumbing here
 ;; is the double-float arm's, not PPC's scale-64bit-misc-index,
 ;; because low-tag fixnumshift=3 already makes a boxed fixnum index
 ;; equal to the byte offset of an 8-byte element.
             ((:signed-64-bit-vector :fixnum-vector)
              (with-imm-target () (temp :s64)
                (if (and (eql vreg-class hard-reg-class-gpr)
                         (eql vreg-mode hard-reg-class-gpr-mode-s64))
                  (setq temp vreg))
                (if (and index-known-fixnum
                         (<= index-known-fixnum
                             (arch::target-max-64-bit-constant-index arch)))
                  (! misc-ref-c-s64 temp src index-known-fixnum)
 ;; TEMP is an imm GPR here, so it must be excluded from the
 ;; index temp's candidates -- unlike the float arms above,
 ;; where FP-VAL is an FPR and cannot alias.
                  (with-imm-target (temp) idx-reg
                    (if index-known-fixnum
                      (arm642-absolute-natural
                       seg idx-reg nil
                       (+ (arch::target-misc-data-offset arch)
                          (ash index-known-fixnum arm64::word-shift)))
                      (! scale-64bit-misc-index idx-reg unscaled-idx))
                    (! misc-ref-s64 temp src idx-reg)))
 ;; arm642-copy-register boxes s64 -> node via arm642-box-s64,
 ;; which has both the inline s64->integer path and the
 ;; .SPmakes64 subprim path, so the bignum case is covered.
                (<- temp)))
             (t
 ;; :unsigned-64-bit-vector -- the only remaining 64-bit ivector
 ;; type (arm64-arch.lisp:1121 lists exactly five).
              (with-imm-target () (temp :u64)
                (if (and (eql vreg-class hard-reg-class-gpr)
                         (eql vreg-mode hard-reg-class-gpr-mode-u64))
                  (setq temp vreg))
                (if (and index-known-fixnum
                         (<= index-known-fixnum
                             (arch::target-max-64-bit-constant-index arch)))
                  (! misc-ref-c-u64 temp src index-known-fixnum)
                  (with-imm-target (temp) idx-reg
                    (if index-known-fixnum
                      (arm642-absolute-natural
                       seg idx-reg nil
                       (+ (arch::target-misc-data-offset arch)
                          (ash index-known-fixnum arm64::word-shift)))
                      (! scale-64bit-misc-index idx-reg unscaled-idx))
                    (! misc-ref-u64 temp src idx-reg)))
                (<- temp)))))
          (is-128-bit
              (with-fp-target () (fp-val :complex-double-float)
 ;; ppc2.lisp:1434 -- picking VREG as the destination is a
 ;; one-armed IF, not the alternative to loading. Making the
 ;; load the ELSE of this test skipped it entirely whenever VREG
 ;; was already a complex-double-float FPR.
                (when (and (eql vreg-class hard-reg-class-fpr)
                           (eql vreg-mode hard-reg-class-fpr-mode-complex-double-float))
                  (setq fp-val vreg))
                (if (and index-known-fixnum
                         (<= index-known-fixnum
                             (- (ash arm64::max-64-bit-constant-index -1) 4)))
                  (! misc-ref-c-complex-double-float fp-val src index-known-fixnum)
                  (with-imm-target () idx-reg
                    (if index-known-fixnum
                      (arm642-absolute-natural
                       seg idx-reg nil
                       (+ arm64::complex-double-float.realpart
                          (ash index-known-fixnum 4)))
                      (! scale-128bit-misc-index idx-reg unscaled-idx))
                    (! misc-ref-complex-double-float fp-val src idx-reg)))
                (if (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-complex-double-float))
                  (<- fp-val)
                  (ensuring-node-target (target vreg)
                    (! complex-double-float->heap target fp-val)))))
          (t
           (unless is-1-bit
             (nx-error "~& unsupported vector type: ~s"
                       type-keyword))
           (ensuring-node-target (target vreg)
             (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-1-bit-constant-index arch)))
               (! misc-ref-c-bit-fixnum target src index-known-fixnum)
               (with-imm-temps () (word-index bitnum)
                 (if index-known-fixnum
                   (progn
                     (arm642-lri seg word-index (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum -5)))
                     (arm642-lri seg bitnum (logand index-known-fixnum #x1f)))
                   (! scale-1bit-misc-index word-index bitnum unscaled-idx))
                 (let* ((dest word-index))
                   (! misc-ref-u32 dest src word-index)
                   (! extract-variable-bit-fixnum target dest bitnum)))))))))
    (^)))

;;; safe = T means assume "vector" is miscobj, do bounds check.
;;; safe = fixnum means check that subtag of vector = "safe" and do
;;; bounds check.
;;; safe = nil means crash&burn.
(defun arm642-vref (seg vreg xfer type-keyword vector index safe)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((index-known-fixnum (acode-fixnum-form-p index))
           (unscaled-idx nil)
           (src nil))
      (if (or safe (not index-known-fixnum))
        (multiple-value-setq (src unscaled-idx)
          (arm642-two-untargeted-reg-forms seg vector
                                           arm64::arg_y index arm64::arg_z))
        (setq src (arm642-one-untargeted-reg-form seg vector arm64::arg_z)))
      (when safe
        (if (typep safe 'fixnum)
          (! trap-unless-typecode= src safe))
        (unless index-known-fixnum
          (! trap-unless-fixnum unscaled-idx))
        (! check-misc-bound unscaled-idx src))
      (arm642-vref1 seg vreg xfer type-keyword src unscaled-idx
                    index-known-fixnum))))

(defun arm642-1d-vref (seg vreg xfer type-keyword vector index safe)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((simple-case (backend-get-next-label))
           (common-case (backend-get-next-label)))
      (multiple-value-bind (src unscaled-idx)
          (arm642-two-untargeted-reg-forms seg vector ($ arm64::arg_y)
                                           index ($ arm64::arg_z))
        (with-crf-target () crf
          (! set-z-if-vector-header crf src)
          (arm642-branch seg (arm642-make-compound-cd simple-case 0) crf
                         arm64::cond-eq nil)
          (when safe
            (! trap-unless-fixnum unscaled-idx)
            (! check-vector-header-bound src unscaled-idx)
            (when (typep safe 'fixnum)
              (! trap-unless-vector-type src safe)))
          (! deref-vector-header src unscaled-idx)
          (-> common-case)
          (@ simple-case)
          (when safe
            (if (typep safe 'fixnum)
              (! trap-unless-simple-1d-array src safe))
            (! trap-unless-fixnum unscaled-idx)
            (! check-misc-bound unscaled-idx src))
          (@ common-case)
          (arm642-vref1 seg vreg xfer type-keyword src unscaled-idx nil))))))

(defun arm642-aset2-via-gvset (seg vreg xfer array i j new safe type-keyword constval &optional (simple t))
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (src ($ arm64::temp0))
           (unscaled-i ($ arm64::arg_x))
           (unscaled-j ($ arm64::arg_y))
           (val-reg ($ arm64::arg_z)))
      (arm642-four-targeted-reg-forms seg
                                    array src
                                    i unscaled-i
                                    j unscaled-j
                                    new val-reg)
      (when safe
        (when (typep safe 'fixnum)
          (if simple
            (! trap-unless-simple-array-2
               src
               (dpb safe target::arrayH.flags-cell-subtag-byte
                    (ash 1 $arh_simple_bit))
               (nx-error-for-simple-2d-array-type type-keyword))
            (! trap-unless-typed-array-2 src safe)))
        (unless i-known-fixnum
          (! trap-unless-fixnum unscaled-i))
        (unless j-known-fixnum
          (! trap-unless-fixnum unscaled-j)))
      (with-imm-target () dim1
        (let* ((idx-reg ($ arm64::arg_y)))
          (progn
            (if safe
              (! check-2d-bound dim1 unscaled-i unscaled-j src)
              (! 2d-dim1 dim1 src))
            (! 2d-unscaled-index idx-reg dim1 unscaled-i unscaled-j))
          (let* ((v ($ arm64::arg_x)))
            (if simple
              (! array-data-vector-ref v src)
              (progn
                (arm642-copy-register seg v src)
                (! deref-vector-header v idx-reg)))
            (arm642-vset1 seg vreg xfer type-keyword v idx-reg nil val-reg (arm642-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval t)))))))

(defun arm642-aset2 (seg vreg xfer array i j new safe type-keyword dim0 dim1 &optional (simple t))
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (arm642-constant-value-ok-for-type-keyword type-keyword new))
           (needs-memoization (and is-node (arm642-acode-needs-memoization new))))
      (if needs-memoization
        (arm642-aset2-via-gvset seg vreg xfer array i j new safe type-keyword constval simple)
        (let* ((constidx
                (and *arm642-reckless*
                     dim0 dim1 i-known-fixnum j-known-fixnum
                     (>= i-known-fixnum 0)
                     (>= j-known-fixnum 0)
                     (< i-known-fixnum dim0)
                     (< j-known-fixnum dim1)
                     (+ (* i-known-fixnum dim1) j-known-fixnum)))
               (val-reg (arm642-target-reg-for-aset vreg type-keyword))
               (node-val (if (node-reg-p val-reg) val-reg))
               (imm-val (if (imm-reg-p val-reg) val-reg)))
          (with-node-target (node-val) src
            (with-node-target (node-val src) unscaled-i
              (with-node-target (node-val src unscaled-i) unscaled-j
                (if constidx
                  (multiple-value-setq (src val-reg)
                    (arm642-two-untargeted-reg-forms seg array ($ arm64::temp0) new val-reg))
                  (multiple-value-setq (src unscaled-i unscaled-j val-reg)
                    (arm642-four-untargeted-reg-forms seg
                                                    array src
                                                    i unscaled-i
                                                    j unscaled-j
                                                    new val-reg)))
                (if (node-reg-p val-reg) (setq node-val val-reg))
                (if (imm-reg-p val-reg) (setq imm-val val-reg))
                (let* ((*available-backend-imm-temps* *available-backend-imm-temps*))
                  (when (and (= (hard-regspec-class val-reg) hard-reg-class-gpr)
                             (logbitp (hard-regspec-value val-reg)
 *backend-imm-temps*))
                    (use-imm-temp (hard-regspec-value val-reg)))
                  (when safe
                    (when (typep safe 'fixnum)
                      (if simple
                        (! trap-unless-simple-array-2
                           src
                           (dpb safe target::arrayH.flags-cell-subtag-byte
                                (ash 1 $arh_simple_bit))
                           (nx-error-for-simple-2d-array-type type-keyword))
                        (! trap-unless-typed-array-2 src safe)))
                    (unless i-known-fixnum
                      (! trap-unless-fixnum unscaled-i))
                    (unless j-known-fixnum
                      (! trap-unless-fixnum unscaled-j)))
                  (with-imm-target (imm-val) dim1
                    (with-node-target (src node-val) idx-reg
                      (unless constidx
                        (if safe
                          (! check-2d-bound dim1 unscaled-i unscaled-j src)
                          (! 2d-dim1 dim1 src))
                        (! 2d-unscaled-index idx-reg dim1 unscaled-i unscaled-j))
                      (with-node-target (idx-reg node-val) v
                        (if simple
                          (! array-data-vector-ref v src)
                          (progn
                            (setq v src)
                            (! deref-vector-header src idx-reg)))
                        (arm642-vset1 seg vreg xfer type-keyword
                                      v idx-reg constidx val-reg (arm642-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization)))))))))))))

(defun arm642-aset3 (seg vreg xfer array i j k new safe type-keyword dim0 dim1 dim2 &optional (simple t))
  (with-arm64-local-vinsn-macros (seg target)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (k-known-fixnum (acode-fixnum-form-p k))
           (arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (arm642-constant-value-ok-for-type-keyword type-keyword new))
           (needs-memoization (and is-node (arm642-acode-needs-memoization new)))
           (src)
           (unscaled-i)
           (unscaled-j)
           (unscaled-k)
           (val-reg (arm642-target-reg-for-aset vreg type-keyword))
           (constidx
            (and *arm642-reckless*
                 (not needs-memoization) dim0 dim1 dim2 i-known-fixnum j-known-fixnum k-known-fixnum
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
            (arm642-two-targeted-reg-forms seg array ($ arm64::temp0) new val-reg))
          (progn
            (setq src ($ arm64::temp1)
                  unscaled-i ($ arm64::temp0)
                  unscaled-j ($ arm64::arg_x)
                  unscaled-k ($ arm64::arg_y))
            (arm642-push-register
             seg
             (arm642-one-untargeted-reg-form seg array ($ arm64::arg_z)))
            (arm642-four-targeted-reg-forms seg
                                          i ($ arm64::temp0)
                                          j ($ arm64::arg_x)
                                          k ($ arm64::arg_y)
                                          new val-reg)
            (arm642-pop-register seg src)))
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*))
          (when (and (= (hard-regspec-class val-reg) hard-reg-class-gpr)
                     (logbitp (hard-regspec-value val-reg)
 *backend-imm-temps*))
            (use-imm-temp (hard-regspec-value val-reg)))
          (when safe
            (when (typep safe 'fixnum)
              (if simple
                (! trap-unless-simple-array-3
                   src
                   (dpb safe target::arrayH.flags-cell-subtag-byte
                        (ash 1 $arh_simple_bit))
                   (nx-error-for-simple-3d-array-type type-keyword))
                (! trap-unless-typed-array-3 src safe)))
            (unless i-known-fixnum
              (! trap-unless-fixnum unscaled-i))
            (unless j-known-fixnum
              (! trap-unless-fixnum unscaled-j))
            (unless k-known-fixnum
              (! trap-unless-fixnum unscaled-k)))
          (with-imm-target () dim1
            (with-imm-target (dim1) dim2
              (let* ((idx-reg ($ arm64::arg_y)))
                (unless constidx
                  (if safe
                    (! check-3d-bound dim1 dim2 unscaled-i unscaled-j unscaled-k src)
                    (! 3d-dims dim1 dim2 src))
                  (! 3d-unscaled-index idx-reg dim1 dim2 unscaled-i unscaled-j unscaled-k))
                (let* ((v ($ arm64::arg_x)))
                  (if simple
                    (! array-data-vector-ref v src)
                    (progn
                      (arm642-copy-register seg v src)
                      (! deref-vector-header v idx-reg v idx-reg)))
                  (arm642-vset1 seg vreg xfer type-keyword v idx-reg constidx val-reg (arm642-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization))))))))))

(defun arm642-aref3 (seg vreg xfer array i j k safe typekeyword dim0 dim1 dim2 &optional (simple t))
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (k-known-fixnum (acode-fixnum-form-p k))
           (src)
           (unscaled-i)
           (unscaled-j)
           (unscaled-k)
           (constidx
            (and *arm642-reckless*
                 dim0 dim1 i-known-fixnum j-known-fixnum k-known-fixnum
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
        (setq src (arm642-one-targeted-reg-form seg array ($ arm64::arg_z)))
        (multiple-value-setq (src unscaled-i unscaled-j unscaled-k)
          (arm642-four-untargeted-reg-forms seg
                                           array arm64::temp0
                                           i arm64::arg_x
                                           j arm64::arg_y
                                           k arm64::arg_z)))
      (when safe
        (when (typep safe 'fixnum)
          (if simple
            (! trap-unless-simple-array-3
               src
               (dpb safe target::arrayH.flags-cell-subtag-byte
                    (ash 1 $arh_simple_bit))
               (nx-error-for-simple-3d-array-type typekeyword))
            (! trap-unless-typed-array-3 src safe)))
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
          (if simple
            (! array-data-vector-ref v src)
            (progn
              (arm642-copy-register seg v src)
              (! deref-vector-header v idx-reg)))
          (arm642-vref1 seg vreg xfer typekeyword v idx-reg constidx))))))

(defun arm642-constant-value-ok-for-type-keyword (type-keyword form)
  (if (and (acode-p (setq form (acode-unwrapped-form form)))
           (or (eq (acode-operator form) (%nx1-operator immediate))
               (eq (acode-operator form) (%nx1-operator fixnum))))
    (let* ((val (car (acode-operands form)))
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
      (if typep val))))

(defun arm642-target-reg-for-aset (vreg type-keyword)
  (let* ((arch (backend-target-arch *target-backend*))
         (is-node (member type-keyword (arch::target-gvector-types arch)))
         (is-1-bit (member type-keyword (arch::target-1-bit-ivector-types arch)))
         (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
         (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
         (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
         (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
         (is-128-bit (eq type-keyword :complex-double-float-vector))
         (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector)))
         (vreg-class (if vreg (hard-regspec-class vreg)))
         (vreg-mode (if (or (eql vreg-class hard-reg-class-gpr)
                            (eql vreg-class hard-reg-class-fpr))
                      (get-regspec-mode vreg)))
         (next-imm-target (available-imm-temp *available-backend-imm-temps*))
         (acc (make-wired-lreg arm64::arg_z)))
    (cond ((or is-node
               is-1-bit
               (eq type-keyword :simple-string)
               (eq type-keyword :fixnum-vector)
               (and (eql vreg-class hard-reg-class-gpr)
                    (eql vreg-mode hard-reg-class-gpr-mode-node)))
           acc)
          ((null vreg)
           (cond (is-64-bit
                  (case type-keyword
                    (:double-float-vector (available-fp-temp *available-backend-fp-temps* :double-float))
                    (:complex-single-float-vector (available-fp-temp *available-backend-fp-temps* :complex-single-float))
                    (t (make-unwired-lreg next-imm-target :mode (if is-signed hard-reg-class-gpr-mode-s64 hard-reg-class-gpr-mode-u64)))))
                 (is-128-bit
                  (available-fp-temp *available-backend-fp-temps* :complex-double-float))
                 (is-32-bit
                  (if (eq type-keyword :single-float-vector)
                    (available-fp-temp *available-backend-fp-temps* :single-float)
                    (make-unwired-lreg next-imm-target :mode (if is-signed hard-reg-class-gpr-mode-s32 hard-reg-class-gpr-mode-u32))))
                 (is-16-bit
                  (make-unwired-lreg next-imm-target :mode (if is-signed hard-reg-class-gpr-mode-s16 hard-reg-class-gpr-mode-u16)))
                 (is-8-bit
                  (make-unwired-lreg next-imm-target :mode (if is-signed hard-reg-class-gpr-mode-s8 hard-reg-class-gpr-mode-u8)))
                 (t "Bug: can't determine operand size for ~s" type-keyword)))
          (t
           (let* ((lreg (if vreg-mode
                          (make-unwired-lreg (lreg-value vreg)))))
             (if
               (cond
                 (is-64-bit
                  (cond ((eq type-keyword :double-float-vector)
                         (and (eql vreg-class hard-reg-class-fpr)
                              (eql vreg-mode hard-reg-class-fpr-mode-double)))
                        (t (and (eql vreg-class hard-reg-class-gpr)
                                (if is-signed
                                  (or (eql vreg-mode hard-reg-class-gpr-mode-s64)
                                      (eql vreg-mode hard-reg-class-gpr-mode-s32))
                                  (or (eql vreg-mode hard-reg-class-gpr-mode-u64)
                                      (eql vreg-mode hard-reg-class-gpr-mode-u32)
                                      (eql vreg-mode hard-reg-class-gpr-mode-s64)))))))
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

(defun arm642-unboxed-reg-for-aset (seg type-keyword result-reg safe constval)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
           (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
           (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
           (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
           (is-128-bit (eq type-keyword :complex-double-float-vector))
           (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector)))
           (result-is-node-gpr (and (eql (hard-regspec-class result-reg)
                                         hard-reg-class-gpr)
                                    (eql (get-regspec-mode result-reg)
                                         hard-reg-class-gpr-mode-node)))
           (next-imm-target (available-imm-temp *available-backend-imm-temps*)))
      (if (or is-node (not result-is-node-gpr))
        result-reg
        (cond (is-128-bit
               (let* ((reg (available-fp-temp *available-backend-fp-temps* :complex-double-float)))
                 (when reg
                   (! trap-unless-typecode= result-reg arm64::subtag-complex-double-float))
                 (! get-complex-double-float reg result-reg)
                 reg))
              (is-64-bit
               (case type-keyword
                 (:double-float-vector
                  (let* ((reg (available-fp-temp *available-backend-fp-temps* :double-float)))
                    (if safe
                      (! get-double? reg result-reg)
                      (! get-double reg result-reg))
                    reg))
                 (:complex-single-float-vector
                  (let* ((reg (available-fp-temp *available-backend-fp-temps* :complex-single-float)))
                    (when safe
                      (! trap-unless-typecode= result-reg arm64::subtag-complex-single-float))
                    (! get-complex-single-float reg result-reg)
                    reg))
 ;; The integer 64-bit types. This CASE had no default arm, so
 ;; it RETURNED NIL for them and that NIL travelled to
 ;; arm642-vset1 as UNBOXED-VAL-REG, where the first
 ;; %hard-regspec-value on it signalled "bad regspec: NIL".
 ;; Fixing vset1's own dispatch is not enough on its own --
 ;; there has to be an unboxed register to dispatch TO.
 ;; PPC64 ppc2.lisp:1908-1919 is the shape.
                 ((:signed-64-bit-vector :fixnum-vector)
                  (let* ((reg (make-unwired-lreg next-imm-target
                                                 :mode hard-reg-class-gpr-mode-s64)))
                    (if (eq type-keyword :fixnum-vector)
                      (progn
                        (when safe
                          (! trap-unless-fixnum result-reg))
                        (! fixnum->signed-natural reg result-reg))
                      (! unbox-s64 reg result-reg))
                    reg))
                 (t
 ;; :unsigned-64-bit-vector
                  (let* ((reg (make-unwired-lreg next-imm-target
                                                 :mode hard-reg-class-gpr-mode-u64)))
                    (! unbox-u64 reg result-reg)
                    reg))))
              (is-32-bit
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
                            (arm642-lri seg reg (char-code constval))
                            (! unbox-base-char reg result-reg)))
                         ((eq type-keyword :single-float-vector)
                          (if (typep constval 'single-float)
                            (arm642-lri seg reg (single-float-bits constval))
                            (progn
                              (when safe
                                (! trap-unless-single-float result-reg))
                              (! single-float-bits reg result-reg))))
                         (t
                          (if (typep constval '(unsigned-byte 32))
                            (arm642-lri seg reg constval)
                            (if *arm642-reckless*
                              (! %unbox-u32 reg result-reg)
                              (! unbox-u32 reg result-reg)))))
                   reg)))
              (is-16-bit
               (if is-signed
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s16)))
                   (if (typep constval '(signed-byte 16))
                     (arm642-lri seg reg constval)
                     (! unbox-s16 reg result-reg))
                   reg)
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u16)))
                   (if (typep constval '(unsigned-byte 16))
                     (arm642-lri seg reg constval)
                     (! unbox-u16 reg result-reg))
                   reg)))
              (is-8-bit
               (if is-signed
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s8)))
                   (if (typep constval '(signed-byte 8))
                     (arm642-lri seg reg constval)
                     (! unbox-s8 reg result-reg))
                   reg)
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u8)))
                   (if (typep constval '(unsigned-byte 8))
                     (arm642-lri seg reg constval)
                     (! unbox-u8 reg result-reg))
                   reg)))
              (t
               (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u8)))
                 (unless (typep constval 'bit)
                   (! unbox-bit reg result-reg))
                 reg)))))))

;;; "val-reg" might be boxed, if the vreg requires it to be.
(defun arm642-vset1 (seg vreg xfer type-keyword src unscaled-idx index-known-fixnum val-reg unboxed-val-reg constval &optional (node-value-needs-memoization t))
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (is-1-bit (member type-keyword (arch::target-1-bit-ivector-types arch)))
           (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
           (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
           (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
           (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
           (is-128-bit (eq type-keyword :complex-double-float-vector))
           (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector))))
      (cond ((and is-node node-value-needs-memoization)
             (unless (and (eql (hard-regspec-value src) arm64::arg_x)
                          (eql (hard-regspec-value unscaled-idx) arm64::arg_y)
                          (eql (hard-regspec-value val-reg) arm64::arg_z))
               (compiler-bug "Bug: invalid register targeting for gvset: ~s" (list src unscaled-idx val-reg)))
             (! call-subprim-3 val-reg (arm64::arm64-subprimitive-offset '.SPgvset) src unscaled-idx val-reg))
            (is-node
             (if (and index-known-fixnum (<= index-known-fixnum
                                             (arch::target-max-64-bit-constant-index arch)))
               (! misc-set-c-node val-reg src index-known-fixnum)
               (with-imm-target () scaled-idx
                 (if index-known-fixnum
                   (arm642-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum *arm642-target-node-shift*)))
                   (! scale-node-misc-index scaled-idx unscaled-idx))
                 (! misc-set-node val-reg src scaled-idx))))
            (t
             (cond
               (is-128-bit
 ;; ppc2.lisp:2046. NB ppc2 emits `scale-misc-128-bit-index'
 ;; here, which PPC defines nowhere -- only the vref spelling
 ;; scale-128bit-misc-index exists (ppc64-vinsns.lisp:68). Use
 ;; the defined name.
                (if (and index-known-fixnum
                         (<= index-known-fixnum
                             (- (ash arm64::max-64-bit-constant-index -1) 4)))
                  (! misc-set-c-complex-double-float unboxed-val-reg src
                     index-known-fixnum)
                  (with-imm-target () scaled-idx
                    (if index-known-fixnum
                      (arm642-absolute-natural
                       seg scaled-idx nil
                       (+ arm64::complex-double-float.realpart
                          (ash index-known-fixnum 4)))
                      (! scale-128bit-misc-index scaled-idx unscaled-idx))
                    (! misc-set-complex-double-float unboxed-val-reg src
                       scaled-idx))))
               (is-64-bit
 ;; Dispatch on the element type. This used to emit
 ;; misc-set-{c-,}double-float unconditionally, so every INTEGER
 ;; 64-bit store handed an imm GPR to a vinsn declaring an FPR
 ;; operand and died in %HARD-REGSPEC-VALUE with "bad regspec:
 ;; NIL" -- (setf (aref a n) v) on a (simple-array (signed-byte
 ;; 64)) at speed 3 / safety 0 would not compile at all.
 ;; PPC64 ppc2.lisp:2024-2045 is the shape. A complex-single-float
 ;; deliberately keeps the double-float vinsns: it is two S lanes
 ;; in one 64-bit slot, so a D-form store moves it bit-for-bit.
                (with-imm-target (arm64::imm0 arm64::imm1) scaled-idx
                  (if (and index-known-fixnum
                           (<= index-known-fixnum
                               (arch::target-max-64-bit-constant-index arch)))
                    (case type-keyword
                      ((:double-float-vector :complex-single-float-vector)
                       (! misc-set-c-double-float unboxed-val-reg src index-known-fixnum))
                      (t
                       (if is-signed
                         (! misc-set-c-s64 unboxed-val-reg src index-known-fixnum)
                         (! misc-set-c-u64 unboxed-val-reg src index-known-fixnum))))
                    (progn
                      (if index-known-fixnum
                        (arm642-absolute-natural
                         seg scaled-idx nil
                         (+ (arch::target-misc-data-offset arch)
                            (ash index-known-fixnum arm64::word-shift)))
                        (! scale-64bit-misc-index scaled-idx unscaled-idx))
                      (case type-keyword
                        ((:double-float-vector :complex-single-float-vector)
                         (! misc-set-double-float unboxed-val-reg src scaled-idx))
                        (t
                         (if is-signed
                           (! misc-set-s64 unboxed-val-reg src scaled-idx)
                           (! misc-set-u64 unboxed-val-reg src scaled-idx))))))))
               (t
                (with-imm-target (unboxed-val-reg) scaled-idx
                  (cond
                    (is-32-bit
                     (if (and index-known-fixnum
                              (<= index-known-fixnum
                                  (if (and (eq type-keyword :single-float-vector)
                                           (eq (hard-regspec-class unboxed-val-reg)
                                               hard-reg-class-fpr))
                                    255
                                    (arch::target-max-32-bit-constant-index arch))))
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
                           (arm642-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 2)))
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
                           (arm642-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 1)))
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
                         (! misc-set-c-u8 unboxed-val-reg src index-known-fixnum))
                       (progn
                         (if index-known-fixnum
                           (arm642-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) index-known-fixnum))
                           (! scale-8bit-misc-index scaled-idx unscaled-idx))
                         (if is-signed
                           (! misc-set-s8 unboxed-val-reg src scaled-idx)
                           (! misc-set-u8 unboxed-val-reg src scaled-idx)))))
                    (t
                     (unless is-1-bit
                       (nx-error "~& unsupported vector type: ~s"
                                 type-keyword))
                     (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-1-bit-constant-index arch)))
                       (with-imm-target (unboxed-val-reg) word
                         (let* ((word-index (ash index-known-fixnum -5))
                                (bit-number (logand index-known-fixnum #x1f)))
                           (! misc-ref-c-u32 word src word-index)
                           (if constval
                             (if (zerop constval)
                               (! set-constant-bit-to-0 word word bit-number)
                               (! set-constant-bit-to-1 word word bit-number))
                             (! set-constant-bit-to-variable-value word word unboxed-val-reg bit-number))
                           (! misc-set-c-u32 word src word-index)))
                       (with-crf-target () crf
                         (with-imm-temps () (word-index bit-number temp)
                           (unless constval
                             (! compare-immediate crf unboxed-val-reg 0))
                           (! scale-1bit-misc-index word-index bit-number unscaled-idx)
                           (! lri temp 1)
                           (! shift-left-variable-word bit-number temp bit-number)
                           (! misc-ref-u32 temp src word-index)
                           (if constval
                             (if (zerop constval)
                               (! u32logandc2 temp temp bit-number)
                               (! u32logior temp temp bit-number))
                             (progn
                               (! set-or-clear-bit temp temp bit-number crf)))
                           (! misc-set-u32 temp src word-index)))))))))))
      (when (and vreg val-reg) (<- val-reg))
      (^))))

(defun arm642-code-coverage-entry (seg note)
  (let* ((afunc *arm642-cur-afunc*))
    (setf (afunc-bits afunc) (%ilogior (afunc-bits afunc) (ash 1 $fbitccoverage)))
    (with-arm64-local-vinsn-macros (seg)
      (let* ((ccreg ($ arm64::temp0)))
        (arm642-store-immediate seg note ccreg)
        (with-node-temps (ccreg) (zero)
          (! lri zero 0)
          (! misc-set-c-node zero ccreg 1))))))

(defun arm642-vset (seg vreg xfer type-keyword vector index value safe)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (arm642-constant-value-ok-for-type-keyword type-keyword value))
           (needs-memoization (and is-node (arm642-acode-needs-memoization value)))
           (index-known-fixnum (acode-fixnum-form-p index)))
      (let* ((src ($ arm64::arg_x))
             (unscaled-idx ($ arm64::arg_y))
             (result-reg ($ arm64::arg_z)))
        (cond (needs-memoization
               (arm642-three-targeted-reg-forms seg
                                              vector src
                                              index unscaled-idx
                                              value result-reg))
              (t
               (if (and (not safe) index-known-fixnum)
                 (multiple-value-setq (src result-reg unscaled-idx)
                   (arm642-two-untargeted-reg-forms seg
                                                  vector src
                                                  value (arm642-target-reg-for-aset vreg type-keyword)))
                 (multiple-value-setq (src unscaled-idx result-reg)
                   (arm642-three-untargeted-reg-forms seg
                                                    vector src
                                                    index unscaled-idx
                                                    value (arm642-target-reg-for-aset vreg type-keyword))))))
        (when safe
          (let* ((*available-backend-imm-temps* *available-backend-imm-temps*)
                 (value (if (eql (hard-regspec-class result-reg)
                                 hard-reg-class-gpr)
                          (hard-regspec-value result-reg))))
            (when (and value (logbitp value *available-backend-imm-temps*))
              (setq *available-backend-imm-temps* (bitclr value *available-backend-imm-temps*)))
            (if (typep safe 'fixnum)
              (! trap-unless-typecode= src safe))
            (unless index-known-fixnum
              (! trap-unless-fixnum unscaled-idx))
            (! check-misc-bound unscaled-idx src)))
        (arm642-vset1 seg vreg xfer type-keyword src unscaled-idx index-known-fixnum result-reg (arm642-unboxed-reg-for-aset seg type-keyword result-reg safe constval) constval needs-memoization)))))

(defun arm642-1d-vset (seg vreg xfer type-keyword vector index value safe)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((arch (backend-target-arch *target-backend*))
           (simple-case (backend-get-next-label))
           (common-case (backend-get-next-label))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (arm642-constant-value-ok-for-type-keyword type-keyword value))
           (needs-memoization (and is-node (arm642-acode-needs-memoization value)))
           (index-known-fixnum (acode-fixnum-form-p index)))
      (let* ((src ($ arm64::arg_x))
             (unscaled-idx ($ arm64::arg_y))
             (result-reg ($ arm64::arg_z)))
        (cond (needs-memoization
               (arm642-three-targeted-reg-forms seg
                                              vector src
                                              index unscaled-idx
                                              value result-reg))
              (t
               (multiple-value-setq (src unscaled-idx result-reg)
                   (arm642-three-untargeted-reg-forms seg
                                                    vector src
                                                    index unscaled-idx
                                                    value (arm642-target-reg-for-aset vreg type-keyword)))))
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*)
               (value (if (eql (hard-regspec-class result-reg)
                                 hard-reg-class-gpr)
                          (hard-regspec-value result-reg))))
            (when (and value (logbitp value *available-backend-imm-temps*))
              (setq *available-backend-imm-temps* (bitclr value *available-backend-imm-temps*)))
          (with-crf-target () crf
            (! set-z-if-vector-header crf src)
            (arm642-branch seg (arm642-make-compound-cd simple-case 0) crf arm64::cond-eq nil))
          (when safe
            (! trap-unless-fixnum unscaled-idx)
            (! check-vector-header-bound src unscaled-idx)
            (when (typep safe 'fixnum)
              (! trap-unless-vector-type src safe)))
          (! deref-vector-header src unscaled-idx)
          (-> common-case)
          (@ simple-case)
          (when safe
            (if (typep safe 'fixnum)
              (! trap-unless-simple-1d-array src safe))
            (! trap-unless-fixnum unscaled-idx)
            (! check-misc-bound unscaled-idx src))
          (@ common-case)
          (arm642-vset1 seg vreg xfer type-keyword src unscaled-idx index-known-fixnum result-reg (arm642-unboxed-reg-for-aset seg type-keyword result-reg safe constval) constval needs-memoization))))))

(defun arm642-tail-call-alias (immref sym &optional arglist)
  (let ((alias (cdr (assq sym *arm642-tail-call-aliases*))))
    (if (and alias (or (null arglist)
                       (eq (+ (length (car arglist)) (length (cadr arglist)))
                           (cdr alias))))
      (make-acode (%nx1-operator immediate) (car alias))
      immref)))

;;; If BODY is essentially an APPLY involving an &rest arg, try to avoid
;;; consing it.
(defun arm642-eliminate-&rest (body rest key-p auxen rest-values)
  (when (and rest (not key-p) (not (cadr auxen)) rest-values)
    (when (eq (logand (the fixnum (nx-var-bits rest))
                      (logior (ash -1 $vbitspecial)
                              (ash 1 $vbitclosed) (ash 1 $vbitsetq) (ash 1 $vbitcloseddownward)))
              0)
      (do* ()
           ((not (acode-p body)))
        (let* ((op (acode-operator body)))
          (if (or (eq op (%nx1-operator lexical-function-call))
                  (eq op (%nx1-operator call)))
            (destructuring-bind (fn-form (stack-args reg-args) &optional spread-p) (acode-operands body)
               (unless (and (eq spread-p t)
                           (eq (arm642-lexical-reference-p (%car reg-args)) rest))
                (return nil))
              (flet ((independent-of-all-values (form)
                       (setq form (acode-unwrapped-form-value form))
                       (or (arm64-constant-form-p form)
                           (let* ((lexref (arm642-lexical-reference-p form)))
                             (and lexref
                                  (neq lexref rest)
                                  (dolist (val rest-values t)
                                    (unless (nx2-var-not-set-by-form-p lexref val)
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
                                 (if (<= (length arglist) $numarm64argregs)
                                   (list nil (reverse arglist))
                                   (list (butlast arglist $numarm64argregs)
                                         (reverse (last arglist $numarm64argregs))))
                                 nil)))
                  (return nil))))
            (if (eq op (%nx1-operator local-block))
              (setq body (cadr (acode-operands body)))
              (if (and (eq op (%nx1-operator if))
                       (eq (arm642-lexical-reference-p (car (acode-operands body))) rest))
                (setq body (car (cdr (acode-operands body))))
                (return nil)))))))))

;;; Donor: arm2.lisp arm2-call-fn @2692 (64-bit port arm64-p2-call-fn @2758
;;; structurally identical apart from lcell bookkeeping, which this backend
;;; doesn't do). ARGLIST is (stack-arg-forms reversed-register-arg-forms);
;;; SPREAD-P is 0 for lexpr spread, non-nil-non-0 for list spread.
;;; Already called by arm642-%new-ptr / arm642-complex / arm642-realpart /
;;; arm642-imagpart in arm642.lisp.
(defun arm642-call-fn (seg vreg xfer fn arglist &optional spread-p)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (when spread-p
      (destructuring-bind (stack-args reg-args) arglist
        (when (and (null (cdr reg-args))
                   (nx-null (acode-unwrapped-form-value (car reg-args))))
          (setq spread-p nil)
          (let* ((nargs (length stack-args)))
            (declare (fixnum nargs))
            (if (<= nargs $numarm64argregs)
              (setq arglist (list nil (reverse stack-args)))
              (setq arglist (list (butlast stack-args $numarm64argregs)
                                  (reverse (last stack-args $numarm64argregs)))))))))
    (let* ((lexref (arm642-lexical-reference-p fn))
           (simple-case (or (fixnump fn)
                            (typep fn 'lreg)
                            (arm642-immediate-function-p fn)
                            (and
                             lexref
                             (not spread-p)
                             (flet ((all-simple (args)
                                      (dolist (arg args t)
                                        (when (and arg (not (nx2-var-not-set-by-form-p lexref arg)))
                                          (return)))))
                               (and (all-simple (car arglist))
                                    (all-simple (cadr arglist))
                                    (setq fn (var-ea lexref)))))))
           (cstack *arm642-cstack*)
           (vstack *arm642-vstack*))
      (setq xfer (or xfer 0))
      (when (and (eq xfer $backend-return)
                 (eq 0 *arm642-undo-count*)
                 (acode-p fn)
                 (eq (acode-operator fn) (%nx1-operator immediate))
                 (symbolp (car (acode-operands fn))))
        (setq fn (arm642-tail-call-alias fn (car (acode-operands fn)) arglist)))

      (if (and (eq xfer $backend-return) (not (arm642-tailcallok xfer)))
        (progn
          (arm642-call-fn seg vreg $backend-mvpass fn arglist spread-p)
          (arm642-set-vstack (%i+ (if simple-case 0 *arm642-target-node-size*) vstack))
          (setq *arm642-cstack* cstack)
          (let ((*arm642-returning-values* t)) (arm642-do-return seg)))
        (let* ((mv-p (arm642-mv-p xfer)))
          (unless simple-case
            (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg fn arm64::arg_z))
            (setq fn (arm642-vloc-ea vstack)))
          (arm642-invoke-fn seg fn (arm642-arglist seg arglist) spread-p xfer)
          (if (and (logbitp $backend-mvpass-bit xfer)
                   (not simple-case))
            (progn
              (! save-values)
              (! vstack-discard 1)
              (arm642-set-nargs seg 0)
              (! recover-values))
            (unless (or mv-p simple-case)
              (! vstack-discard 1)))
          (arm642-set-vstack vstack)
          (setq *arm642-cstack* cstack)
          (when (or (logbitp $backend-mvpass-bit xfer) (not mv-p))
            (<- arm64::arg_z)
            (arm642-branch seg (logand (lognot $backend-mvpass-mask) xfer) vreg))))
      nil)))

(defun arm642-restore-full-lisp-context (seg)
  (with-arm64-local-vinsn-macros (seg)
    (! restore-full-lisp-context)))

(defun arm642-call-symbol (seg jump-p)
  (with-arm64-local-vinsn-macros (seg)
    (if *arm642-optimize-for-space*
      (if jump-p
        (! jump-known-symbol-ool)
        (! call-known-symbol-ool))
      (if jump-p
        (! jump-known-symbol)
        (! call-known-symbol arm64::arg_z)))))

;;; Nargs = nil -> multiple-value case.
;;; Donor: arm2.lisp arm2-invoke-fn @2779 (chosen over the 64-bit port's
;;; arm64-p2-invoke-fn @2849: arm642.lisp already has arm2's newer
;;; *arm642-fixed-args-label* / *arm642-fixed-nargs* known-fixed-nargs
;;; machinery, where the 64-bit port used the older tail-label scheme).
;;; Nargs = nil -> multiple-value case.
;;; NFP unit 2026-07-15: (! restore-nfp) re-added on the tail/spread
;;; paths per arm2.lisp:2822-2834 (save-nfp now emitted by the
;;; arm642-lambda redef in w4; vinsn self-gates on max-nfp-depth>0).
;;; REMAINING DEVIATION (U1): (arm642-restore-non-volatile-fprs seg)
;;; still omitted — his prologue saves no nvfprs.
(defun arm642-invoke-fn (seg fn nargs spread-p xfer)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((f-op (acode-unwrapped-form-value fn))
           (immp (and (acode-p f-op)
                      (eq (acode-operator f-op) (%nx1-operator immediate))))
           (symp (and immp (symbolp (car (acode-operands f-op)))))
           (label-p (and (fixnump fn)
                         (locally (declare (fixnum fn))
                           (and (= fn -1) (- fn)))))
           (tail-p (eq xfer $backend-return))
           (func (if (acode-p f-op) (car (acode-operands f-op))))
           (a-reg nil)
           (lfunp (and (acode-p f-op)
                       (eq (acode-operator f-op) (%nx1-operator simple-function))))
           (expression-p (or (typep fn 'lreg) (and (fixnump fn) (not label-p))))
           (callable (or symp lfunp label-p))
           (destreg (if symp ($ arm64::fname) (if lfunp ($ arm64::nfn) (unless label-p ($ arm64::nfn)))))
           (known-fixed-nargs nil)
           (label (when label-p
                    (if (and *arm642-fixed-args-label*
                             (eql nargs *arm642-fixed-nargs*)
                             (not spread-p)
                             (not (arm642-mvpass-p xfer)))
                      (progn
                        (setq known-fixed-nargs t)
                        (if tail-p
 *arm642-fixed-args-tail-label*
 *arm642-fixed-args-label*))
                      1))))
      (when expression-p
 ;; Have to do this before spread args, since might be vsp-relative.
        (if nargs
          (arm642-do-lexical-reference seg destreg fn)
          (arm642-copy-register seg destreg fn)))
      (if (or symp lfunp)
        (setq func (if symp (arm642-symbol-entry-locative func)
                     (arm642-afunc-lfun-ref func))
              a-reg (arm642-register-constant-p func)))
      (when tail-p
 #-no-compiler-bugs
        (unless (or immp symp lfunp (typep fn 'lreg) (fixnump fn)) (compiler-bug "Well, well, well.  How could this have happened ?"))
        (when a-reg
          (arm642-copy-register seg destreg a-reg))
        (unless spread-p
          (arm642-restore-nvrs seg (null nargs))
          (! restore-nfp)))
      (if spread-p
        (progn
          (arm642-set-nargs seg (%i- nargs 1))
          (if (eq spread-p 0)
            (! spread-lexpr)
            (! spread-list))
          (arm642-restore-nvrs seg nil)
          (! restore-nfp))
        (if nargs
          (unless known-fixed-nargs (arm642-set-nargs seg nargs))
          (! pop-argument-registers)))
      (if callable
        (if (not tail-p)
          (if (arm642-mvpass-p xfer)
            (let* ((call-reg (if symp ($ arm64::fname) ($ arm64::nfn))))
              (if label-p
                (arm642-copy-register seg call-reg ($ arm64::fn))
                (if a-reg
                  (arm642-copy-register seg call-reg a-reg)
                  (arm642-store-immediate seg func call-reg)))
              (if symp
                (! pass-multiple-values-symbol)
                (! pass-multiple-values)))
            (progn
              (if label-p
                (progn
                  (arm642-copy-register seg ($ arm64::nfn) ($ arm64::fn))
                  (! call-label (aref *backend-labels* label)))
                (progn
                  (if a-reg
                    (arm642-copy-register seg destreg a-reg)
                    (arm642-store-immediate seg func destreg))
                  (if symp
                    (arm642-call-symbol seg nil)
                    (! call-known-function))))))
          (progn
            (arm642-unwind-stack seg xfer 0 0 #x7fffff)
            (if (and (not spread-p) nargs (%i<= nargs $numarm64argregs))
              (progn
                (if label-p
                  (unless known-fixed-nargs
                    (arm642-copy-register seg arm64::nfn arm64::fn)))
                (unless (or label-p a-reg) (arm642-store-immediate seg func destreg))
                (unless known-fixed-nargs
                  (arm642-restore-full-lisp-context seg))
                (if label-p
                  (! jump (aref *backend-labels* label))
                  (progn
                    (if symp
                      (arm642-call-symbol seg t)
                      (! jump-known-function)))))
              (progn
                (if label-p
                  (arm642-copy-register seg arm64::nfn arm64::fn)
                  (unless a-reg (arm642-store-immediate seg func destreg)))
                (cond ((or spread-p (null nargs))
                       (if symp
                         (! tail-call-sym-gen)
                         (! tail-call-fn-gen)))
                      ((%i> nargs $numarm64argregs)
                       (if symp
                         (! tail-call-sym-slide)
                         (! tail-call-fn-slide)))
                      (t
                       (! restore-full-lisp-context)
                       (if symp
                         (! jump-known-symbol)
                         (! jump-known-function))))))))
 ;; The general (funcall) case: we don't know (at compile-time)
 ;; for sure whether we've got a symbol or a (local, constant)
 ;; function.
        (progn
          (unless (or (fixnump fn) (typep fn 'lreg))
            (arm642-one-targeted-reg-form seg fn destreg))
          (if (not tail-p)
            (if (arm642-mvpass-p xfer)
              (! pass-multiple-values)
              (! funcall))
            (cond ((or (null nargs) spread-p)
                   (! tail-funcall-gen))
                  ((%i> nargs $numarm64argregs)
                   (! tail-funcall-slide))
                  (t
                   (! tail-funcall-vsp)))))))
    nil))

(defun arm642-seq-fbind (seg vreg xfer vars afuncs body p2decls)
  (let* ((old-stack (arm642-encode-stack))
         (copy afuncs)
         (func nil))
    (with-arm64-p2-declarations p2decls
      (dolist (var vars)
        (when (neq 0 (afunc-fn-refcount (setq func (pop afuncs))))
          (arm642-seq-bind-var seg var (nx1-afunc-ref func))))
      (arm642-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (when (neq 0 (afunc-fn-refcount (setq func (pop copy))))
          (arm642-close-var seg var))))))

(defun arm642-make-closure (seg afunc downward-p)
  (with-arm64-local-vinsn-macros (seg)
    (flet ((var-to-reg (var target)
             (let* ((ea (var-ea (var-bits var))))
               (if ea
                 (arm642-addrspec-to-reg seg (arm642-ea-open ea) target)
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
             (dest ($ arm64::arg_z))
             (vsize (+ (length inherited-vars)
                       2                ; %closure-code%, afunc
                       2)))             ; name, lfun-bits
        (declare (list inherited-vars))
        (if downward-p
          (progn
            (let* ((*arm642-vstack* *arm642-vstack*))
              (arm642-lri seg arm64::arg_x (ash (nx-lookup-target-uvector-subtag :function) *arm642-target-fixnum-shift*))
              (! %closure-code% arm64::arg_y)
              (arm642-store-immediate seg (arm642-afunc-lfun-ref afunc) arm64::arg_z)
              (arm642-vpush-register-arg seg arm64::arg_x)
              (arm642-vpush-register-arg seg arm64::arg_y)
              (arm642-vpush-register-arg seg arm64::arg_z)
 ; Could be smarter about memory traffic here.
              (dolist (v inherited-vars)
                (arm642-vpush-register-arg seg (var-to-reg v arm64::arg_z)))
              (! load-nil arm64::arg_z)
              (arm642-vpush-register-arg seg arm64::arg_z)
              (arm642-lri seg arm64::arg_z (ash (ash 1 $lfbits-trampoline-bit) *arm642-target-fixnum-shift*))
              (arm642-vpush-register-arg seg arm64::arg_z)
              (arm642-set-nargs seg (1+ vsize)) ; account for subtag
              (! make-stack-gvector))
            (arm642-open-undo $undostkblk))
          (let* ((cell 0))
            (declare (fixnum cell))
            (progn
              (arm642-lri seg arm64::imm0
                          (arch::make-vheader
                           vsize (nx-lookup-target-uvector-subtag :function)))
              (! %alloc-misc-fixed dest arm64::imm0
                 (ash vsize (arch::target-word-shift arch))))
            (! %closure-code% arm64::arg_x)
            (arm642-store-immediate seg (arm642-afunc-lfun-ref afunc) arm64::arg_y)
            (with-node-temps (arm64::arg_z) (t0 t1 t2 t3)
              (do* ((ccode arm64::arg_x nil)
                    (func arm64::arg_y nil))
                   ((null inherited-vars))
                (let* ((t0r (or ccode (if inherited-vars (var-to-reg (pop inherited-vars) t0))))
                       (t1r (or func (if inherited-vars (var-to-reg (pop inherited-vars) t1))))
                       (t2r (if inherited-vars (var-to-reg (pop inherited-vars) t2)))
                       (t3r (if inherited-vars (var-to-reg (pop inherited-vars) t3))))
                  (setq cell (set-some-cells dest cell t0r t1r t2r t3r)))))
            (arm642-lri seg arm64::arg_y (ash (ash 1 $lfbits-trampoline-bit) *arm642-target-fixnum-shift*))
            (! load-nil arm64::arg_x)
            (! misc-set-c-node arm64::arg_x dest cell)
            (! misc-set-c-node arm64::arg_y dest (1+ cell))))
 ;; Both legs above build the closure through the MISC allocator
 ;; (%alloc-misc-fixed / .SPstkgvector), so DEST is fulltag-misc
 ;; even though its header subtag is subtag-function. arm64 has a
 ;; dedicated fulltag-function, so the finished closure has to be
 ;; retagged before it escapes -- otherwise every require-function
 ;; check on it fails (%defun's did, at cold load). The cell
 ;; stores above are misc-relative, so this must come last.
        (! tag-as-function dest dest)
        dest))))

(defun arm642-symbol-entry-locative (sym)
  (setq sym (require-type sym 'symbol))
  (when (eq sym '%call-next-method-with-args)
    (setf (afunc-bits *arm642-cur-afunc*)
          (%ilogior (%ilsl $fbitnextmethargsp 1) (afunc-bits
 *arm642-cur-afunc*))))
  (or (assq sym *arm642-fcells*)
      (let ((new (list sym)))
        (push new *arm642-fcells*)
        new)))

(defun arm642-symbol-value-cell (sym)
  (setq sym (require-type sym 'symbol))
  (or (assq sym *arm642-vcells*)
      (let ((new (list sym)))
        (push new *arm642-vcells*)
        (ensure-binding-index sym)
        new)))

(defun arm642-symbol-locative-p (imm)
  (and (consp imm)
       (or (memq imm *arm642-vcells*)
           (memq imm *arm642-fcells*))))

(defun arm642-immediate-function-p (f)
  (setq f (acode-unwrapped-form-value f))
  (and (acode-p f)
       (or (eq (acode-operator f) (%nx1-operator immediate))
           (eq (acode-operator f) (%nx1-operator simple-function)))))

(defun arm64-constant-form-p (form)
  (setq form (nx-untyped-form form))
  (if form
    (or (nx-null form)
        (nx-t form)
        (and (acode-p form)
             (or (eq (acode-operator form) (%nx1-operator immediate))
                 (eq (acode-operator form) (%nx1-operator fixnum))
                 (eq (acode-operator form)
                     (%nx1-operator simple-function)))))))

(defun arm642-integer-constant-p (form mode)
  (let* ((val
          (or (acode-fixnum-form-p (setq form (acode-unwrapped-form form)))
              (and (acode-p form)
                   (eq (acode-operator form) (%nx1-operator immediate))
                   (setq form (car (acode-operands form)))
                   (if (typep form 'integer)
                     form)))))
    (and val (%typep val (mode-specifier-type mode)) val)))

;;; Called (but not defined) by arm642-two-targeted-reg-forms in arm642.lisp.
;;; Donor: arm2.lisp arm-side-effect-free-form-p @3059, minus the
;;; arm2-nfp-ref-p test (no nfp memspecs exist in this backend yet); the
;;; 64-bit port's arm64-side-effect-free-form-p has the same shape and name.
(defun arm64-side-effect-free-form-p (form)
  (when (acode-p (setq form (acode-unwrapped-form-value form)))
    (or (arm64-constant-form-p form)
 ;;(eq (acode-operator form) (%nx1-operator bound-special-ref))
        (if (eq (acode-operator form) (%nx1-operator lexical-reference))
          (not (%ilogbitp $vbitsetq (nx-var-bits (car (acode-operands form)))))))))

(defun arm642-formlist (seg stkargs &optional revregargs)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((nregs (length revregargs))
           (n nregs))
      (declare (fixnum n))
      (dolist (arg stkargs)
        (let* ((reg (arm642-one-untargeted-reg-form seg arg arm64::arg_z)))
          (arm642-vpush-register-arg seg reg)
          (incf n)))
      (when revregargs
        (let* ((zform (%car revregargs))
               (yform (%cadr revregargs))
               (xform (%caddr revregargs)))
          (if (eq 3 nregs)
            (arm642-three-targeted-reg-forms seg xform ($ arm64::arg_x)
                                             yform ($ arm64::arg_y)
                                             zform ($ arm64::arg_z))
            (if (eq 2 nregs)
              (arm642-two-targeted-reg-forms seg yform ($ arm64::arg_y)
                                             zform ($ arm64::arg_z))
              (arm642-one-targeted-reg-form seg zform ($ arm64::arg_z))))))
      n)))

(defun arm642-arglist (seg args)
  (arm642-formlist seg (car args) (cadr args)))

;;; Donor ppc2.lisp:7217 ppc2-unboxed-integer-arg-to-reg (v2
;;; arm64-p2.lisp:3206 identical shape). Evaluates FORM into an imm
;;; reg in the mode named by the FFI arg spec; constant 0 rides xzr.
(defun arm642-unboxed-integer-arg-to-reg (seg form immreg &optional ffi-arg-type)
  (let* ((mode (case ffi-arg-type
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
    (with-arm64-local-vinsn-macros (seg)
      (let* ((value (arm642-integer-constant-p form mode)))
        (if value
          (if (eql value 0)
            (make-wired-lreg arm64::xzr :mode modeval)
            (progn
              (unless (typep immreg 'lreg)
                (setq immreg (make-unwired-lreg immreg :mode modeval)))
              (arm642-lri seg immreg value)
              immreg))
          (progn
            (arm642-one-targeted-reg-form
             seg form (make-wired-lreg arm64::imm0 :mode modeval))))))))

(defun arm642-macptr-arg-to-reg (seg form address-reg)
  (arm642-one-targeted-reg-form seg form address-reg))

(defun arm642-push-reg-for-form (seg form suggested &optional targeted)
  (let* ((reg (if (and (node-reg-p suggested)
                       (nx2-acode-call-p form)) ;probably ...
                (arm642-one-targeted-reg-form seg form arm64::arg_z)
                (if targeted
                  (arm642-one-targeted-reg-form seg form suggested)
                  (arm642-one-untargeted-reg-form seg form suggested)))))
    (arm642-push-register seg reg)))

(defun arm642-one-lreg-form (seg form lreg)
  (arm642-form seg lreg nil form)
  lreg)

(defun arm642-one-targeted-reg-form (seg form reg)
  (arm642-one-lreg-form seg form reg))

(defun arm642-one-untargeted-lreg-form (seg form reg)
  (arm642-one-lreg-form seg form (if (typep reg 'lreg) reg
                                     (make-unwired-lreg reg))))

(defun same-arm64-reg-p (x y)
  (and (eq (hard-regspec-value x) (hard-regspec-value y))
       (let* ((class (hard-regspec-class x)))
         (and (eq class (hard-regspec-class y))
              (or (not (eql class hard-reg-class-fpr))
                  (eq (%get-regspec-mode x)
                      (%get-regspec-mode y)))))))

;;; If REG is a node reg, add it to the bitmask.
(defun arm642-restrict-node-target (reg mask)
  (if (node-reg-p reg)
    (logior mask (ash 1 (hard-regspec-value reg)))
    mask))

;;; If suggested reg is a node reg that contains a stack location,
;;; try to use some other node temp.
(defun arm642-try-non-conflicting-reg (suggested reserved)
  (let* ((mask *arm642-gpr-locations-valid-mask*))
    (or (when (and (node-reg-p suggested)
                   (logbitp (hard-regspec-value suggested) mask))
          (setq mask (logior mask reserved))
          (%available-node-temp (logand *available-backend-node-temps*
                                        (lognot mask))))
        suggested)))

(defun arm642-push-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (fpr-mode-name (if a-float (fpr-mode-value-name
                                     (get-regspec-mode areg))))
         (a-node (unless a-float (= (get-regspec-mode areg)
                                    hard-reg-class-gpr-mode-node)))
         (nested (> *arm642-undo-count* 0))
         vinsn)
    (with-arm64-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (arm642-vpush-register seg areg))
        (let* ((offset *arm642-nfp-depth*))
          (setq vinsn
                (if a-float
                  (case fpr-mode-name
                    ((:double-float :complex-single-float)
                     (if nested
                       (! nfp-store-double-float-nested areg offset)
                       (! nfp-store-double-float areg offset)))
                    (:complex-double-float
 ;; Store the 16-byte value at OFFSET (its slot base), THEN
 ;; bump OFFSET by an extra 8 so the shared trailing
 ;; (incf offset 8) advances the depth by 16 total. The old
 ;; code did the incf *before* the store, writing the value
 ;; 8 bytes above its slot -- asymmetric with
 ;; arm642-pop-register, which retreats a full 16 and reads
 ;; at the slot base.
                     (prog1
                         (if nested
                           (! nfp-store-complex-double-float-nested areg offset)
                           (! nfp-store-complex-double-float areg offset))
                       (incf offset 8)))
                    (:single-float
                     (if nested
                       (! nfp-store-single-float-nested areg offset)
                       (! nfp-store-single-float areg offset))))
                  (if nested
                    (! nfp-store-unboxed-word-nested areg offset)
                    (! nfp-store-unboxed-word areg offset))))
          (push vinsn *arm642-all-nfp-pushes*)
          (incf offset 8)
          (setq *arm642-nfp-depth* offset)))
      vinsn)))

(defun arm642-one-untargeted-reg-form (seg form suggested &optional (reserved 0))
  (or (arm642-reg-for-form form suggested)
      (if (and (acode-p form)
               (eq (acode-operator form) (%nx1-operator %current-tcr)))
        arm64::rcontext
        (if (node-reg-p suggested)
          (arm642-one-untargeted-lreg-form
           seg form (arm642-try-non-conflicting-reg suggested reserved))
          (arm642-one-untargeted-lreg-form seg form suggested)))))

(defun arm642-pop-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (fpr-mode-name (if a-float (fpr-mode-value-name (get-regspec-mode
                                                          areg))))
         (a-node (unless a-float (= (get-regspec-mode areg)
                                    hard-reg-class-gpr-mode-node)))
         (nested (> *arm642-undo-count* 0))
         vinsn)
    (with-arm64-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (arm642-vpop-register seg areg))
        (let* ((offset (- *arm642-nfp-depth* 8)))
          (setq vinsn
                (if a-float
                  (case fpr-mode-name
                    ((:double-float :complex-single-float)
                     (if nested
                       (! nfp-load-double-float-nested areg offset)
                       (! nfp-load-double-float areg offset)))
                    (:complex-double-float
                     (decf offset 8)
                     (if nested
                       (! nfp-load-complex-double-float-nested areg offset)
                       (! nfp-load-complex-double-float areg offset)))
                    (:single-float
                     (if nested
                       (! nfp-load-single-float-nested areg offset)
                       (! nfp-load-single-float areg offset))))
                  (if nested
                    (! nfp-load-unboxed-word-nested areg offset)
                    (! nfp-load-unboxed-word areg offset))))
          (setq *arm642-nfp-depth* offset)))
      vinsn)))

(defun arm642-acc-reg-for (reg)
  (with-arm64-local-vinsn-macros (seg)
    (if (and (eql (hard-regspec-class reg) hard-reg-class-gpr)
             (eql (get-regspec-mode reg) hard-reg-class-gpr-mode-node))
      ($ arm64::arg_z)
      reg)))

(defun arm642-copy-fpr (seg dest src)
 ;; src and dest are distinct FPRs with the same mode.
  (with-arm64-local-vinsn-macros (seg)
    (case (fpr-mode-value-name (get-regspec-mode src))
      (:single-float (! copy-single-float dest src))
      (:double-float (! copy-double-float dest src))
      (:complex-single-float (! copy-complex-single-float dest src))
      (:complex-double-float (! copy-complex-double-float dest src)))))

(defun arm642-elide-pushes (seg push-vinsn pop-vinsn)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((operands (vinsn-variable-parts push-vinsn))
           (pushed-reg (svref operands 0))
           (popped-reg (svref (vinsn-variable-parts pop-vinsn) 0))
           (same-reg (eq (hard-regspec-value pushed-reg)
                         (hard-regspec-value popped-reg)))
           (nfp-p (vinsn-attribute-p push-vinsn :nfp)))
      (when nfp-p
        (let* ((pushed-reg-is-set (vinsn-sequence-sets-reg-p
                                   push-vinsn pop-vinsn pushed-reg))
               (popped-reg-is-set (if same-reg
                                    pushed-reg-is-set
                                    (vinsn-sequence-sets-reg-p
                                     push-vinsn pop-vinsn popped-reg)))
               (popped-reg-is-reffed (unless same-reg
                                       (vinsn-sequence-refs-reg-p
                                        push-vinsn pop-vinsn popped-reg)))
               (offset (svref operands 1))
               (nested ())
               (conflicts ())
               (win nil))
          (declare (fixnum offset))
          (do* ((element (dll-node-succ push-vinsn) (dll-node-succ element)))
               ((eq element pop-vinsn))
            (when (typep element 'vinsn)
              (when (vinsn-attribute-p element :nfp)
                (let* ((element-offset (svref (vinsn-variable-parts element) 1)))
                  (declare (fixnum element-offset))
                  (if (= element-offset offset)
                    (push element conflicts)
                    (if (> element-offset offset)
                      (push element nested)))))))
          (cond
            (conflicts nil)
            ((and (not (and pushed-reg-is-set popped-reg-is-set))
 ;; When PUSHED-REG is reassigned in the sequence the copy
 ;; has to be emitted before that assignment, and it must
 ;; still come after POPPED-REG's last reference -- we are
 ;; about to overwrite POPPED-REG. Require such a point to
 ;; exist; the vsp leg below makes the identical test.
 ;; Falling through costs an elision, never correctness.
                  (or (null popped-reg-is-reffed)
                      (null pushed-reg-is-set)
                      (vinsn-in-sequence-p pushed-reg-is-set
                                           popped-reg-is-reffed
                                           pop-vinsn)))
             (unless same-reg
               (let* ((copy (if (eq (hard-regspec-class pushed-reg)
                                    hard-reg-class-fpr)
                              (arm642-copy-fpr seg popped-reg pushed-reg)
                              (! copy-gpr popped-reg pushed-reg))))
                 (remove-dll-node copy)
                 (if pushed-reg-is-set
                   (insert-dll-node-after copy
                                          (or popped-reg-is-reffed push-vinsn))
                   (insert-dll-node-before copy pop-vinsn))))
             (setq win t))
            ((eql (hard-regspec-class pushed-reg) hard-reg-class-fpr)
             (let* ((mode (get-regspec-mode pushed-reg))
                    (mode-name (fpr-mode-value-name mode)))
 ;; If we're pushing a float register that gets
 ;; set by the intervening vinsns, try to copy it to and
 ;; from a free FPR instead.
               (multiple-value-bind (used-gprs used-fprs)
                   (regs-set-in-vinsn-sequence push-vinsn pop-vinsn)
                 (declare (ignore used-gprs))
                 (let* ((nfprs (case mode-name
                                 ((:double-float :complex-single-float) 7)
                                 (:complex-double-float 3)
                                 (:single-float 14)))
                        (free-fpr
                          (dotimes (r nfprs nil)
                            (unless (logtest (target-fpr-mask r mode)
                                             used-fprs)
                              (return r)))))
                   (when free-fpr
                     (let* ((reg (make-wired-lreg free-fpr
                                                  :class hard-reg-class-fpr
                                                  :mode mode))
                            (save (arm642-copy-fpr seg reg pushed-reg))
                            (restore (arm642-copy-fpr seg popped-reg reg)))
                       (remove-dll-node save)
                       (insert-dll-node-after save push-vinsn)
                       (remove-dll-node restore)
                       (insert-dll-node-before restore pop-vinsn)
                       (setq win t))))))))
          (when win
            (setq *arm642-all-nfp-pushes*
                  (delete push-vinsn *arm642-all-nfp-pushes*))
            (let* ((pair (assq push-vinsn *arm642-nfp-vars*)))
              (when pair
                (setf (car pair) nil)))
            (when nested
              (let* ((size (if (vinsn-attribute-p push-vinsn :uses-frame-pointer)
                             16
                             8)))
                (declare (fixnum size))
                (dolist (inner nested)
                  (let* ((inner-operands (vinsn-variable-parts inner)))
                    (setf (svref inner-operands 1)
                          (the fixnum
                               (- (the fixnum (svref inner-operands 1))
                                  size))))
                  (let* ((var (cdr (assq inner *arm642-nfp-vars*))))
                    (when var (setf (var-ea var)
                                    (- (var-ea var) size)))))))
            (elide-vinsn push-vinsn)
            (elide-vinsn pop-vinsn)
            t)))
      (when (and (vinsn-attribute-p push-vinsn :vsp))
        (unless (or
                 (vinsn-sequence-has-attribute-p push-vinsn pop-vinsn :vsp :push)
                 (vinsn-sequence-has-attribute-p push-vinsn pop-vinsn :vsp :pop)
                 (let* ((pushed-reg-is-set (vinsn-sequence-sets-reg-p
                                            push-vinsn pop-vinsn pushed-reg))
                        (popped-reg-is-set (if same-reg
                                             pushed-reg-is-set
                                             (vinsn-sequence-sets-reg-p
                                              push-vinsn pop-vinsn popped-reg)))
                        (popped-reg-is-reffed (unless same-reg
                                                (vinsn-sequence-refs-reg-p
                                                 push-vinsn pop-vinsn
                                                 popped-reg))))
                   (cond ((and (not (and pushed-reg-is-set popped-reg-is-set))
                               (not (vinsn-sequence-has-some-attribute-p
                                     push-vinsn pop-vinsn :branch :jump))
                               (or (null popped-reg-is-reffed)
                                   (null pushed-reg-is-set)
 ;; If the popped register is
 ;; referenced and the pushed
 ;; register is set, we want to be
 ;; sure that the last reference
 ;; happens before the first
 ;; assignent. We can't be sure
 ;; that either of these things
 ;; actually happened or happen
 ;; unconditionally, and can't
 ;; be sure of the order in which
 ;; they might happen if the sequence
 ;; contains jumps or branches.
                                   (vinsn-in-sequence-p pushed-reg-is-set
                                                        popped-reg-is-reffed
                                                        pop-vinsn)))
 ;; We don't try this if anything's pushed on
 ;; or popped from the vstack in the
 ;; sequence, but there can be references to
 ;; other things that were pushed earlier.
 ;; Those references use the vstack depth at
 ;; the time of the reference and the
 ;; canonical frame offset to address
 ;; relative to the vsp. If we elide the
 ;; push, the vstack depth will be smaller
 ;; than when the reference was
 ;; generated. Fix that up ...
                          (do* ((element (dll-node-succ push-vinsn)
                                         (dll-node-succ element)))
                               ((eq element pop-vinsn))
                            (when (typep element 'vinsn)
                              (let* ((template (vinsn-template element))
                                     (opidx (case (vinsn-template-name template)
                                              (vframe-store 2)
                                              (vframe-load 2))))
                                (when opidx
                                  (let* ((ops (vinsn-variable-parts element)))
                                    (declare (simple-vector ops))
                                    (setf (svref ops opidx)
                                          (the fixnum
                                               (- (the fixnum (svref ops opidx))
                                                  arm64::node-size))))))))
                          (unless same-reg
                            (let* ((copy (! copy-gpr popped-reg pushed-reg)))
                              (remove-dll-node copy)
                              (if pushed-reg-is-set
 ;; PUSHED-REG is assigned later in the
 ;; sequence, so the copy has to happen
 ;; before that assignment. It also has to
 ;; happen after the last reference to
 ;; POPPED-REG, whose value we are about to
 ;; overwrite; the test above has already
 ;; established that that last reference
 ;; precedes the first assignment, so there
 ;; is a slot between them. Inserting after
 ;; PUSH-VINSN instead clobbers POPPED-REG
 ;; ahead of its own uses.
                                (insert-dll-node-after copy
                                                       (or popped-reg-is-reffed
                                                           push-vinsn))
                                (insert-dll-node-before copy pop-vinsn))))
                          (elide-vinsn push-vinsn)
                          (elide-vinsn pop-vinsn)
                          t)
                         (t             ; maybe allocate a node temp
                          nil)))))))))

(defun arm642-two-targeted-reg-forms (seg aform areg bform breg)
  (let* ((avar (arm642-lexical-reference-p aform))
         (atriv (and (arm642-trivial-p bform) (nx2-node-gpr-p breg)))
         (aconst (and (not atriv)
                      (or (arm64-side-effect-free-form-p aform)
                          (if avar (nx2-var-not-set-by-form-p avar bform)))))
         (apushed (not (or atriv aconst))))
    (progn
      (unless aconst
        (if atriv
          (arm642-one-targeted-reg-form seg aform areg)
          (setq apushed
                (arm642-push-register
                 seg (arm642-one-untargeted-reg-form
                      seg aform (arm642-acc-reg-for areg))))))
      (arm642-one-targeted-reg-form seg bform breg)
      (if aconst
        (arm642-one-targeted-reg-form seg aform areg)
        (if apushed
          (arm642-elide-pushes seg apushed (arm642-pop-register seg areg)))))
    (values areg breg)))

(defun arm642-two-untargeted-reg-forms (seg aform areg bform breg)
  (let* ((*arm642-nfp-depth* *arm642-nfp-depth*)
         (aalready (arm642-reg-for-form aform areg))
         (balready (arm642-reg-for-form bform breg)))
    (if (and aalready balready)
      (values aalready balready)
      (with-arm64-local-vinsn-macros (seg)
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*)
               (avar (arm642-lexical-reference-p aform))
               (adest nil)
               (bdest nil)
               (atriv (and (arm642-trivial-p bform) (nx2-node-gpr-p breg)))
               (aconst (and (not atriv) (or (arm64-side-effect-free-form-p aform)
                                            (if avar (nx2-var-not-set-by-form-p avar bform)))))
               (apushed nil)
               (restricted 0))
          (progn
            (unless aconst
              (if atriv
                (progn
                  (setq adest (arm642-one-untargeted-reg-form seg aform areg)
                        restricted (arm642-restrict-node-target adest 0))
                  (when (imm-reg-p adest)
                    (use-imm-temp (%hard-regspec-value adest)))
                  (when (same-arm64-reg-p adest breg)
                    (setq breg areg)))
                (setq apushed (arm642-push-reg-for-form seg aform areg))))
            (setq bdest (arm642-one-untargeted-reg-form seg bform breg restricted)
                  restricted (arm642-restrict-node-target bdest restricted))
            (unless adest
              (if (same-arm64-reg-p areg bdest)
                (setq areg breg)))
            (if aconst
              (progn
                (if (imm-reg-p bdest)
                  (use-imm-temp (%hard-regspec-value bdest)))
                (setq adest (arm642-one-untargeted-reg-form seg aform areg restricted)))
              (if apushed
                (arm642-elide-pushes seg apushed (arm642-pop-register seg (setq adest areg))))))
          (values adest bdest))))))

;;; Donor: v2 arm64-p2.lisp arm64-p2-four-targeted-reg-forms @3374
;;; (= arm2 shape). Sibling of arm642-three-targeted-reg-forms
;;; (arm642-additions @422); same trivial/const/push-elide protocol,
;;; extended to a fourth (d) operand.
(defun arm642-four-targeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (let* ((bnode (nx2-node-gpr-p breg))
         (cnode (nx2-node-gpr-p creg))
         (dnode (nx2-node-gpr-p dreg))
         (atriv (or (null aform)
                    (and (arm642-trivial-p bform)
                         (arm642-trivial-p cform)
                         (arm642-trivial-p dform)
                         bnode
                         cnode
                         dnode)))
         (btriv (or (null bform)
                    (and (arm642-trivial-p cform)
                         (arm642-trivial-p dform)
                         cnode
                         dnode)))
         (ctriv (or (null cform)
                    (and (arm642-trivial-p dform) dnode)))
         (aconst (and (not atriv)
                      (or (arm64-side-effect-free-form-p aform)
                          (let ((avar (arm642-lexical-reference-p aform)))
                            (and avar
                                 (nx2-var-not-set-by-form-p avar bform)
                                 (nx2-var-not-set-by-form-p avar cform)
                                 (nx2-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or (arm64-side-effect-free-form-p bform)
                          (let ((bvar (arm642-lexical-reference-p bform)))
                            (and bvar
                                 (nx2-var-not-set-by-form-p bvar cform)
                                 (nx2-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or (arm64-side-effect-free-form-p cform)
                          (let ((cvar (arm642-lexical-reference-p cform)))
                            (and cvar
                                 (nx2-var-not-set-by-form-p cvar dform))))))
         (apushed nil)
         (bpushed nil)
         (cpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (arm642-one-targeted-reg-form seg aform areg)
        (setq apushed (arm642-push-register seg (arm642-one-untargeted-reg-form seg aform (arm642-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (arm642-one-targeted-reg-form seg bform breg)
        (setq bpushed (arm642-push-register seg (arm642-one-untargeted-reg-form seg bform (arm642-acc-reg-for breg))))))
    (if (and cform (not cconst))
      (if ctriv
        (arm642-one-targeted-reg-form seg cform creg)
        (setq cpushed (arm642-push-register seg (arm642-one-untargeted-reg-form seg cform (arm642-acc-reg-for creg))))))
    (arm642-one-targeted-reg-form seg dform dreg)
    (unless ctriv
      (if cconst
        (arm642-one-targeted-reg-form seg cform creg)
        (arm642-elide-pushes seg cpushed (arm642-pop-register seg creg))))
    (unless btriv
      (if bconst
        (arm642-one-targeted-reg-form seg bform breg)
        (arm642-elide-pushes seg bpushed (arm642-pop-register seg breg))))
    (unless atriv
      (if aconst
        (arm642-one-targeted-reg-form seg aform areg)
        (arm642-elide-pushes seg apushed (arm642-pop-register seg areg))))
    (values areg breg creg dreg)))

(defun arm642-three-targeted-reg-forms (seg aform areg bform breg cform creg)
  (let* ((*arm642-nfp-depth* *arm642-nfp-depth*)
         (bnode (nx2-node-gpr-p breg))
         (cnode (nx2-node-gpr-p creg))
         (atriv (or (null aform)
                    (and (arm642-trivial-p bform)
                         (arm642-trivial-p cform)
                         bnode
                         cnode)))
         (btriv (or (null bform)
                    (and (arm642-trivial-p cform)
                         cnode)))
         (aconst (and (not atriv)
                      (or (arm64-side-effect-free-form-p aform)
                          (let ((avar (arm642-lexical-reference-p aform)))
                            (and avar
                                 (nx2-var-not-set-by-form-p avar bform)
                                 (nx2-var-not-set-by-form-p avar cform))))))
         (bconst (and (not btriv)
                      (or
                       (arm64-side-effect-free-form-p bform)
                       (let ((bvar (arm642-lexical-reference-p bform)))
                         (and bvar (nx2-var-not-set-by-form-p bvar cform))))))
         (apushed nil)
         (bpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (arm642-one-targeted-reg-form seg aform areg)
        (setq apushed (arm642-push-reg-for-form seg aform areg t))))
    (if (and bform (not bconst))
      (if btriv
        (arm642-one-targeted-reg-form seg bform breg)
        (setq bpushed (arm642-push-reg-for-form seg bform breg t))))
    (arm642-one-targeted-reg-form seg cform creg)
    (unless btriv
      (if bconst
        (arm642-one-targeted-reg-form seg bform breg)
        (arm642-elide-pushes seg bpushed (arm642-pop-register seg breg))))
    (unless atriv
      (if aconst
        (arm642-one-targeted-reg-form seg aform areg)
        (arm642-elide-pushes seg apushed (arm642-pop-register seg areg))))
    (values areg breg creg)))

(defun arm642-three-untargeted-reg-forms (seg aform areg bform breg cform creg)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((*arm642-nfp-depth* *arm642-nfp-depth*)
           (bnode (nx2-node-gpr-p breg))
           (cnode (nx2-node-gpr-p creg))
           (atriv (or (null aform)
                      (and (arm642-trivial-p bform)
                           (arm642-trivial-p cform)
                           bnode
                           cnode)))
           (btriv (or (null bform)
                      (and (arm642-trivial-p cform)
                           cnode)))
           (aconst (and (not atriv)
                        (or (arm64-side-effect-free-form-p aform)
                            (let ((avar (arm642-lexical-reference-p aform)))
                              (and avar
                                   (nx2-var-not-set-by-form-p avar bform)
                                   (nx2-var-not-set-by-form-p avar cform))))))
           (bconst (and (not btriv)
                        (or
                         (arm64-side-effect-free-form-p bform)
                         (let ((bvar (arm642-lexical-reference-p bform)))
                           (and bvar (nx2-var-not-set-by-form-p bvar cform))))))
           (adest nil)
           (bdest nil)
           (cdest nil)
           (apushed nil)
           (bpushed nil)
           (restricted 0))
      (when (and aform (not aconst))
        (if atriv
          (progn
            (setq adest (arm642-one-untargeted-reg-form seg aform ($ areg))
                  restricted (arm642-restrict-node-target adest 0))
            (when (same-arm64-reg-p adest breg)
              (setq breg areg))
            (when (same-arm64-reg-p adest creg)
              (setq creg areg)))
          (setq apushed (arm642-push-reg-for-form seg aform areg))))
      (when (and bform (not bconst))
        (if btriv
          (progn
            (setq bdest (arm642-one-untargeted-reg-form seg bform ($ breg) restricted)
                  restricted (arm642-restrict-node-target bdest restricted))
            (when (same-arm64-reg-p bdest creg)
              (setq creg breg))
            (when (same-arm64-reg-p bdest areg)
              (setq areg breg)))
          (setq bpushed (arm642-push-reg-for-form seg bform breg))))
      (setq cdest (arm642-one-untargeted-reg-form seg cform creg restricted)
            restricted (arm642-restrict-node-target cdest restricted))
      (when (same-arm64-reg-p cdest areg)
        (setq areg creg))
      (when (same-arm64-reg-p cdest breg)
        (setq breg creg))
      (unless btriv
        (if bconst
          (setq bdest (arm642-one-untargeted-reg-form seg bform breg restricted))
          (arm642-elide-pushes seg bpushed (arm642-pop-register seg (setq bdest breg))))
        (setq restricted (arm642-restrict-node-target bdest restricted))
        (when (same-arm64-reg-p bdest areg)
          (setq areg breg)))
      (unless atriv
        (if aconst
          (setq adest (arm642-one-untargeted-reg-form seg aform areg restricted))
          (arm642-elide-pushes seg apushed (arm642-pop-register seg (setq adest areg)))))
      (values adest bdest cdest))))

(defun arm642-four-untargeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (let* ((*arm642-nfp-depth* *arm642-nfp-depth*)
         (bnode (nx2-node-gpr-p breg))
         (cnode (nx2-node-gpr-p creg))
         (dnode (nx2-node-gpr-p dreg))
         (atriv (or (null aform)
                    (and (arm642-trivial-p bform)
                         (arm642-trivial-p cform)
                         (arm642-trivial-p dform)
                         bnode
                         cnode
                         dnode)))
         (btriv (or (null bform)
                    (and (arm642-trivial-p cform)
                         (arm642-trivial-p dform)
                         cnode
                         dnode)))
         (ctriv (or (null cform)
                    (and (arm642-trivial-p dform) dnode)))
         (aconst (and (not atriv)
                      (or (arm64-side-effect-free-form-p aform)
                          (let ((avar (arm642-lexical-reference-p aform)))
                            (and avar
                                 (nx2-var-not-set-by-form-p avar bform)
                                 (nx2-var-not-set-by-form-p avar cform)
                                 (nx2-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or
                       (arm64-side-effect-free-form-p bform)
                       (let ((bvar (arm642-lexical-reference-p bform)))
                         (and bvar
                              (nx2-var-not-set-by-form-p bvar cform)
                              (nx2-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or
                       (arm64-side-effect-free-form-p cform)
                       (let ((cvar (arm642-lexical-reference-p cform)))
                         (and cvar
                              (nx2-var-not-set-by-form-p cvar dform))))))
         (adest nil)
         (bdest nil)
         (cdest nil)
         (ddest nil)
         (apushed nil)
         (bpushed nil)
         (cpushed nil)
         (restricted 0))
    (if (and aform (not aconst))
      (if atriv
        (progn
          (setq adest (arm642-one-untargeted-reg-form seg aform areg)
                restricted (arm642-restrict-node-target adest restricted))
          (when (same-arm64-reg-p adest breg)
            (setq breg areg))
          (when (same-arm64-reg-p adest creg)
            (setq creg areg))
          (when (same-arm64-reg-p adest dreg)
            (setq dreg areg)))
        (setq apushed (arm642-push-reg-for-form seg aform areg))))
    (if (and bform (not bconst))
      (if btriv
        (progn
          (setq bdest (arm642-one-untargeted-reg-form seg bform breg restricted)
                restricted (arm642-restrict-node-target bdest restricted))
          (unless adest
            (when (same-arm64-reg-p areg bdest)
              (setq areg breg)))
          (when (same-arm64-reg-p bdest creg)
            (setq creg breg))
          (when (same-arm64-reg-p bdest dreg)
            (setq dreg breg)))
        (setq bpushed (arm642-push-reg-for-form seg bform breg))))
    (if (and cform (not cconst))
      (if ctriv
        (progn
          (setq cdest (arm642-one-untargeted-reg-form seg cform creg restricted)
                restricted (arm642-restrict-node-target cdest restricted))
          (unless adest
            (when (same-arm64-reg-p areg cdest)
              (setq areg creg)))
          (unless bdest
            (when (same-arm64-reg-p breg cdest)
              (setq breg creg)))
          (when (same-arm64-reg-p cdest dreg)
            (setq dreg creg)))
        (setq cpushed (arm642-push-reg-for-form seg cform creg))))
    (setq ddest (arm642-one-untargeted-reg-form seg dform dreg restricted)
          restricted (arm642-restrict-node-target ddest restricted))
    (unless adest
      (when (same-arm64-reg-p ddest areg)
        (setq areg dreg)))
    (unless bdest
      (when (same-arm64-reg-p ddest breg)
        (setq breg dreg)))
    (unless cdest
      (when (same-arm64-reg-p ddest creg)
        (setq creg dreg)))
    (unless ctriv
      (if cconst
        (setq cdest (arm642-one-untargeted-reg-form seg cform creg restricted))
        (arm642-elide-pushes seg cpushed (arm642-pop-register seg (setq cdest creg))))
      (setq restricted (arm642-restrict-node-target cdest restricted))
      (unless adest
        (when (same-arm64-reg-p cdest areg)
          (setq areg creg)))
      (unless bdest
        (when (same-arm64-reg-p ddest breg)
          (setq breg creg))))
    (unless btriv
      (if bconst
        (setq bdest (arm642-one-untargeted-reg-form seg bform breg restricted))
        (arm642-elide-pushes seg bpushed (arm642-pop-register seg (setq bdest breg))))
      (setq restricted (arm642-restrict-node-target bdest restricted))
      (unless adest
        (when (same-arm64-reg-p bdest areg)
          (setq areg breg))))
    (unless atriv
      (if aconst
        (setq adest (arm642-one-untargeted-reg-form seg aform areg restricted))
        (arm642-elide-pushes seg apushed (arm642-pop-register seg (setq adest areg)))))
    (values adest bdest cdest ddest)))

(defun arm642-lri (seg reg value)
  (with-arm64-local-vinsn-macros (seg)
    (if (>= value 0)
      (! lri reg value)
      (! lri reg (logand value #xffffffffffffffff)))))

(defun arm642-multiple-value-body (seg form)
  (let* ((lab (backend-get-next-label))
         (*arm642-vstack* *arm642-vstack*)
         (old-stack (arm642-encode-stack)))
    (with-arm64-local-vinsn-macros (seg)
      (arm642-open-undo $undomvexpect)
      (arm642-undo-body seg nil (logior $backend-mvpass-mask lab) form old-stack)
      (@ lab))))

(defun arm642-afunc-lfun-ref (afunc)
  (or (afunc-lfun afunc)
      (progn
        (pushnew afunc (afunc-fwd-refs *arm642-cur-afunc*) :test #'eq)
        afunc)))

(defun arm642-augment-arglist (afunc arglist &optional (maxregs $numarm64argregs))
  (let ((inherited-args (afunc-inherited-vars afunc)))
    (when inherited-args
      (let* ((current-afunc *arm642-cur-afunc*)
             (stkargs (car arglist))
             (regargs (cadr arglist))
             (inhforms nil)
             (numregs (length regargs))
             (own-inhvars (afunc-inherited-vars current-afunc)))
        (dolist (var inherited-args)
          (let* ((root-var (nx-root-var var))
                 (other-guy
                  (dolist (v own-inhvars #|(compiler-bug "other guy not found")|# root-var)
                    (when (eq root-var (nx-root-var v)) (return v)))))
            (push (make-acode (%nx1-operator inherited-arg) other-guy) inhforms)))
        (dolist (form inhforms)
          (if (%i< numregs maxregs)
            (progn
              (setq regargs (nconc regargs (list form)))
              (setq numregs (%i+ numregs 1)))
            (push form stkargs)))
        (%rplaca (%cdr arglist) regargs)
        (%rplaca arglist stkargs))))
  arglist)

(defun arm642-constant-for-compare-p (form &optional unboxed)
  (setq form (acode-unwrapped-form form))
  (when (acode-p form)
    (let* ((op (acode-operator form)))
      (if (eql op (%nx1-operator fixnum))
        (let* ((val (if unboxed
                      (car (acode-operands form))
                      (ash (car (acode-operands form)) arm64::fixnumshift))))
 ;; cmp (or cmn) takes an aimm; i.e., an (unsigned-byte 12),
 ;; optionally left-shifted by 12 (which we don't exploit here)
          (if (< (abs val) 4096)
                 val))
        (if (eql op (%nx1-operator %unbound-marker))
          arm64::unbound-marker
          (if (eql op (%nx1-operator %slot-unbound-marker))
            arm64::slot-unbound-marker))))))

(defun arm642-compare (seg vreg xfer i j cr-bit true-p)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((jconst (arm642-constant-for-compare-p j))
           (iconst (arm642-constant-for-compare-p i))
           (boolean (backend-crf-p vreg)))
      (if (and boolean (or iconst jconst))
        (let* ((reg (arm642-one-untargeted-reg-form seg (if jconst i j) arm64::arg_z)))
          (if (typep (or jconst iconst) '(unsigned-byte 12))
            (! compare-immediate vreg reg (or jconst iconst))
            (with-node-target (reg) other
              (arm642-lri seg other (or jconst iconst))
              (! compare vreg reg other)))
          (unless (or jconst (eq cr-bit arm64::cond-eq))
            (setq cr-bit (arm642-cr-bit-for-reversed-comparison cr-bit)))
          (^ cr-bit true-p))
        (if (or jconst iconst)
          (progn
            (unless (or jconst (eq cr-bit arm64::cond-eq))
              (setq cr-bit (arm642-cr-bit-for-reversed-comparison cr-bit)))
            (arm642-test-reg-%izerop
              seg vreg xfer
              (arm642-one-untargeted-reg-form seg (if jconst i j) arm64::arg_z)
              cr-bit true-p
              (or jconst iconst)))
          (multiple-value-bind (ireg jreg) (arm642-two-untargeted-reg-forms seg i arm64::arg_y j arm64::arg_z)
            (arm642-compare-registers seg vreg xfer ireg jreg cr-bit true-p)))))))

(defun arm642-natural-compare (seg vreg xfer i j cr-bit true-p)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((jconst (arm642-constant-for-compare-p j t))
           (iconst (arm642-constant-for-compare-p i t))
           (boolean (backend-crf-p vreg)))
      (if (and boolean (or iconst jconst))
        (let* ((reg (arm642-one-untargeted-reg-form seg (if jconst i j) ($ arm64::imm0 :mode :u64))))
          (! compare-immediate vreg reg (or jconst iconst))
          (unless (or jconst (eq cr-bit arm64::cond-eq))
            (setq cr-bit (arm642-cr-bit-for-reversed-comparison cr-bit)))
          (^ cr-bit true-p))
        (if (and (eq cr-bit arm64::cond-eq)
                 (or jconst iconst))
          (arm642-test-reg-%izerop
           seg vreg xfer
           (arm642-one-untargeted-reg-form
            seg (if jconst i j) ($ arm64::imm0 :mode :u64))
           cr-bit true-p
           (or jconst iconst))
          (multiple-value-bind (ireg jreg) (arm642-two-untargeted-reg-forms seg i ($ arm64::imm0 :mode :u64) j ($ arm64::imm1 :mode :u64))
            (arm642-compare-registers seg vreg xfer ireg jreg cr-bit true-p)))))))

(defun arm642-compare-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case
       (vreg)
       (progn
         (! compare vreg ireg jreg)
         (^ cr-bit true-p))
       (with-crf-target () crf
         (! compare crf ireg jreg)
         (ensuring-node-target (target vreg)
           (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
         (^)))
      (^))))

(defun arm642-compare-register-to-nil (seg vreg xfer ireg cr-bit true-p)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case
       (vreg)
       (progn
         (! compare-to-nil vreg ireg)
         (^ cr-bit true-p))
       (with-crf-target () crf
         (! compare-to-nil crf ireg)
         (ensuring-node-target (target vreg)
           (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
         (^)))
      (^))))

(defun arm642-compare-double-float-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case
       (vreg)
       (progn
         (! double-float-compare vreg ireg jreg)
         (^ cr-bit true-p))
       (progn
         (with-crf-target () flags
           (! double-float-compare flags ireg jreg)
           (! cond->boolean vreg (if true-p cr-bit (logxor cr-bit 1))))
         (^)))
      (^))))

(defun arm642-compare-single-float-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case
       (vreg)
       (progn
         (! single-float-compare vreg ireg jreg)
         (^ cr-bit true-p))
       (progn
         (with-crf-target () flags
           (! single-float-compare flags ireg jreg)
           (! cond->boolean vreg (if true-p cr-bit (logxor cr-bit 1))))
         (^)))
      (^))))

(defun arm642-immediate-form-p (form)
  (if (and (acode-p form)
           (or (eq (acode-operator form) (%nx1-operator immediate))
               (eq (acode-operator form) (%nx1-operator simple-function))))
    t))

(defun arm642-test-%izerop (seg vreg xfer form cr-bit true-p)
  (arm642-test-reg-%izerop seg vreg xfer (arm642-one-untargeted-reg-form seg form arm64::arg_z) cr-bit true-p 0))

(defun arm642-test-reg-%izerop (seg vreg xfer reg cr-bit true-p zero)
  (declare (fixnum reg))
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (regspec-crf-gpr-case
     (vreg)
     (progn
       (if (typep zero '(unsigned-byte 12))
         (! compare-immediate vreg reg zero)
         (with-node-target (reg) other
           (arm642-lri seg other zero)
           (! compare vreg reg other)))
       (^ cr-bit true-p))
     (with-crf-target () crf
       (if (typep zero '(unsigned-byte 12))
         (! compare-immediate crf reg zero)
         (with-node-target (reg) other
           (arm642-lri seg other zero)
           (! compare crf reg other)))
       (ensuring-node-target (target vreg)
         (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
       (^)))))

(defun arm642-lexical-reference-ea (form &optional (no-closed-p t))
  (when (acode-p (setq form (acode-unwrapped-form-value form)))
    (if (eq (acode-operator form) (%nx1-operator lexical-reference))
      (let* ((addr (var-ea (car (acode-operands form)))))
        (if (typep addr 'lreg)
          addr
          (unless (and no-closed-p (addrspec-vcell-p addr ))
            addr))))))

(defun arm642-vpush-register (seg src &optional inhibit-note)
  (with-arm64-local-vinsn-macros (seg)
    (prog1
        (! vpush-register src)
      (unless inhibit-note
        (arm642-regmap-note-store src *arm642-vstack*))
      (arm642-adjust-vstack *arm642-target-node-size*))))

(defun arm642-vpush-register-arg (seg src)
  (arm642-vpush-register seg src))

(defun arm642-vpop-register (seg dest)
  (with-arm64-local-vinsn-macros (seg)
    (prog1
        (! vpop-register dest)
      (arm642-adjust-vstack (- *arm642-target-node-size*)))))

(defun arm642-copy-register (seg dest src)
  (with-arm64-local-vinsn-macros (seg)
    (when dest
      (let ((dest-gpr (backend-ea-physical-reg dest hard-reg-class-gpr))
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
          (if dest-crf
            (if src-gpr
 ;; "Copying" a GPR to a CR field means comparing it to rnil
              (! compare-to-nil dest src)
              (! compare-to-nil dest arm64::rnil))
            (if (and dest-gpr src-gpr)
 ;; This is the "GPR <- GPR" case. There are
 ;; word-size dependencies, but there's also
 ;; lots of redundancy here.
              (ecase dest-mode
                (#.hard-reg-class-gpr-mode-node ;boxed result
                 (case src-mode
                   (#.hard-reg-class-gpr-mode-node
                    (unless (eql dest-gpr src-gpr)
                      (! copy-gpr dest src)))
                   (#.hard-reg-class-gpr-mode-u64
                    (arm642-box-u64 seg dest src))
                   (#.hard-reg-class-gpr-mode-s64
                    (arm642-box-s64 seg dest src))
                   (#.hard-reg-class-gpr-mode-u32
                    (! u32->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-s32
                    (! s32->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-u16
                    (! u16->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-s16
                    (! s16->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-u8
                    (! u8->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-s8
                    (! s8->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-address
                    (arm642-macptr->heap seg dest src))))
                ((#.hard-reg-class-gpr-mode-u64
 #.hard-reg-class-gpr-mode-address) ;u64 or address dest
                 (case src-mode
                   (#.hard-reg-class-gpr-mode-node
                    (let* ((src-type (get-node-regspec-type-modes src)))
                      (declare (fixnum src-type))
                      (case dest-mode
                        (#.hard-reg-class-gpr-mode-u64
                         (! unbox-u64 dest src))
                        (#.hard-reg-class-gpr-mode-address
                         (unless (or (logbitp #.hard-reg-class-gpr-mode-address
                                              src-type)
 *arm642-reckless*)
                           (! trap-unless-macptr src))
                         (! deref-macptr dest src)))))
                   ((#.hard-reg-class-gpr-mode-u64
 #.hard-reg-class-gpr-mode-s64
 #.hard-reg-class-gpr-mode-address)
                    (unless (eql dest-gpr src-gpr)
                      (! copy-gpr dest src)))
                   ((#.hard-reg-class-gpr-mode-u16
 #.hard-reg-class-gpr-mode-s16)
                    (! u16->u64 dest src))
                   ((#.hard-reg-class-gpr-mode-u8
 #.hard-reg-class-gpr-mode-s8)
                    (! u8->u64 dest src))))
                (#.hard-reg-class-gpr-mode-s64 ;s64 dest
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
                    (! s16->s64 dest src))
                   ((#.hard-reg-class-gpr-mode-u8
 #.hard-reg-class-gpr-mode-s8)
                    (! s8->s64 dest src))))
                (#.hard-reg-class-gpr-mode-s32 ;s32 dest
                 (case src-mode
                   (#.hard-reg-class-gpr-mode-node
                    (! unbox-s32 dest src))
                   ((#.hard-reg-class-gpr-mode-u32
 #.hard-reg-class-gpr-mode-s32
 #.hard-reg-class-gpr-mode-address)
                    (unless (eql  dest-gpr src-gpr)
                      (! copy-gpr dest src)))
                   (#.hard-reg-class-gpr-mode-u16
                    (! u16->s32 dest src))
                   (#.hard-reg-class-gpr-mode-s16
                    (! s16->s32 dest src))
                   (#.hard-reg-class-gpr-mode-u8
                    (! u8->s32 dest src))
                   (#.hard-reg-class-gpr-mode-s8
                    (! s8->s32 dest src))))
                (#.hard-reg-class-gpr-mode-u32 ;u32 dest
                 (case src-mode
                   (#.hard-reg-class-gpr-mode-node
                    (if *arm642-reckless*
                      (! %unbox-u32 dest src)
                      (! unbox-u32 dest src)))
                   ((#.hard-reg-class-gpr-mode-u32
 #.hard-reg-class-gpr-mode-s32)
                    (unless (eql dest-gpr src-gpr)
                      (! copy-gpr dest src)))
                   ((#.hard-reg-class-gpr-mode-u16
 #.hard-reg-class-gpr-mode-s16)
                    (! u16->u32 dest src))
                   ((#.hard-reg-class-gpr-mode-u8
 #.hard-reg-class-gpr-mode-s8)
                    (! u8->u32 dest src))))
                (#.hard-reg-class-gpr-mode-u16 ;u16 dest
                 (case src-mode
                   (#.hard-reg-class-gpr-mode-node
                    (if *arm642-reckless*
                      (! %unbox-u16 dest src)
                      (! unbox-u16 dest src)))
                   ((#.hard-reg-class-gpr-mode-u8
 #.hard-reg-class-gpr-mode-s8)
                    (! u8->u16 dest src))
                   (t
                    (unless (eql dest-gpr src-gpr)
                      (! copy-gpr dest src)))))
                (#.hard-reg-class-gpr-mode-s16 ;s16 dest
                 (case src-mode
                   (#.hard-reg-class-gpr-mode-node
                    (! unbox-s16 dest src))
                   (#.hard-reg-class-gpr-mode-s8
                    (! s8->s16 dest src))
                   (#.hard-reg-class-gpr-mode-u8
                    (! u8->s16 dest src))
                   (t
                    (unless (eql dest-gpr src-gpr)
                      (! copy-gpr dest src)))))
                (#.hard-reg-class-gpr-mode-u8 ;u8 dest
                 (case src-mode
                   (#.hard-reg-class-gpr-mode-node
                    (if *arm642-reckless*
                      (! %unbox-u8 dest src)
                      (! unbox-u8 dest src)))
                   (t
                    (unless (eql dest-gpr src-gpr)
                      (! copy-gpr dest src)))))
                (#.hard-reg-class-gpr-mode-s8 ;s8 dest
                 (case src-mode
                   (#.hard-reg-class-gpr-mode-node
                    (! unbox-s8 dest src))
                   (t
                    (unless (eql dest-gpr src-gpr)
                      (! copy-gpr dest src))))))
              (if src-gpr
                (if dest-fpr
                  (progn
                    (case src-mode
                      (#.hard-reg-class-gpr-mode-node
                       (case dest-mode
                         (#.hard-reg-class-fpr-mode-double
                          (unless (or (logbitp hard-reg-class-fpr-type-double
                                               (get-node-regspec-type-modes
                                                src))
 *arm642-reckless*)
                            (! trap-unless-double-float src))
                          (! get-double dest src))
                         (#.hard-reg-class-fpr-mode-single
                          (unless *arm642-reckless*
                            (! trap-unless-single-float src))
                          (! get-single dest src))
                         (#.hard-reg-class-fpr-mode-complex-single-float
                          (unless *arm642-reckless*
                            (! trap-unless-complex-single-float src))
                          (! get-complex-single-float dest src))
                         (#.hard-reg-class-fpr-mode-complex-double-float
                          (unless *arm642-reckless*
                            (! trap-unless-complex-double-float src))
                          (! get-complex-double-float dest src)))))))
                (if dest-gpr
                  (case dest-mode
                    (#.hard-reg-class-gpr-mode-node
                     (case src-mode
                       (#.hard-reg-class-fpr-mode-double
                        (! double->heap dest src))
                       (#.hard-reg-class-fpr-mode-complex-double-float
                        (! complex-double-float->heap dest src))
                       (#.hard-reg-class-fpr-mode-complex-single-float
                        (! complex-single-float->heap dest src))
                       (#.hard-reg-class-fpr-mode-single
                        (! single->node dest src)))))
                  (if (and src-fpr dest-fpr)
                    (if (eql src-mode dest-mode)
                      (case (fpr-mode-value-name src-mode)
                        (:single-float (! copy-single-float dest src))
                        (:double-float (! copy-double-float dest src))
                        (:complex-single-float
                         (! copy-complex-single-float dest src))
                        (:complex-double-float
                         (! copy-complex-double-float dest src)))
                      (if (and (eql src-mode hard-reg-class-fpr-mode-double)
                               (eql dest-mode hard-reg-class-fpr-mode-single))
                        (! copy-double-to-single dest src)
                        (if (and (eql dest-mode hard-reg-class-fpr-mode-double)
                                 (eql src-mode hard-reg-class-fpr-mode-single))
                          (! copy-single-to-double dest src))))))))))))))

(defun arm642-unreachable-store (&optional vreg)
 ;; I don't think that anything needs to be done here,
 ;; but leave this guy around until we're sure.
  (declare (ignore vreg))
  nil)

(defun arm642-seq-bind (seg vars initforms)
  (dolist (var vars)
    (arm642-seq-bind-var seg var (pop initforms))))

;;; Donor: arm2.lisp arm2-dynamic-extent-form @4326 (v2 @4317 identical
;;; logic + lcell rebinds, dropped). Vinsn names are arm2's stack-consing
;;; family (make-stack-cons etc.) for consistency with HIS make-stack-vcell
;;; choice; v2's tsp spellings (make-tsp-cons) not carried even though his
;;; design HAS a tsp register — the vinsn wave decides the bodies, the names
;;; stay arm2-lineage (U8v).
;;; The closed-function leg references arm642-make-closure, which is NOT
;;; defined here or in his file — function-object wave (U3v).
(defun arm642-dynamic-extent-form (seg curstack val &aux (form val))
  (when (acode-p form)
    (arm64-with-note (form seg curstack) ; note this rebinds form/seg/curstack so can't setq
      (with-arm64-local-vinsn-macros (seg)
        (let* ((op (acode-operator form))
               (operands (acode-operands form)))
          (cond ((eq op (%nx1-operator list))
                 (let* ((*arm642-vstack* *arm642-vstack*))
                   (arm642-set-nargs seg (arm642-formlist seg (car operands) nil))
                   (arm642-open-undo $undostkblk curstack)
                   (! stack-cons-list))
                 (setq val arm64::arg_z))
                ((eq op (%nx1-operator list*))
                 (let* ((arglist (car operands)))
                   (let* ((*arm642-vstack* *arm642-vstack*))
                     (arm642-arglist seg arglist))
                   (when (car arglist)
                     (arm642-set-nargs seg (length (%car arglist)))
                     (! stack-cons-list*)
                     (arm642-open-undo $undostkblk curstack))
                   (setq val arm64::arg_z)))
                ((eq op (%nx1-operator multiple-value-list))
                 (arm642-multiple-value-body seg (car operands))
                 (arm642-open-undo $undostkblk curstack)
                 (! stack-cons-list)
                 (setq val arm64::arg_z))
                ((eq op (%nx1-operator cons))
                 (let* ((y ($ arm64::arg_y))
                        (z ($ arm64::arg_z))
                        (result ($ arm64::arg_z)))
                   (arm642-two-targeted-reg-forms seg (car operands) y (cadr operands) z)
                   (arm642-open-undo $undostkblk)
                   (! make-stack-cons result y z)
                   (setq val result)))
                ((eq op (%nx1-operator %consmacptr%))
                 (with-imm-target () (address :address)
                   (arm642-one-targeted-reg-form seg form address)
                   (with-node-temps () (node)
                     (! macptr->stack node address)
                     (arm642-open-undo $undostkblk)
                     (setq val node))))
                ((eq op (%nx1-operator %new-ptr))
                 (let* ((clear-form (cadr operands))
                        (cval (nx2-constant-form-value clear-form)))
                   (if cval
                       (progn
                         (arm642-one-targeted-reg-form seg (car operands) ($ arm64::arg_z))
                         (if (nx-null cval)
                             (! make-stack-block)
                             (! make-stack-block0)))
                       (with-crf-target () crf
                         (let ((stack-block-0-label (backend-get-next-label))
                               (done-label (backend-get-next-label))
                               (rval ($ arm64::arg_z))
                               (rclear ($ arm64::arg_y)))
                           (arm642-two-targeted-reg-forms seg (car operands) rval clear-form rclear)
                           (! compare-to-nil crf rclear)
                           (! cbranch-false (aref *backend-labels* stack-block-0-label) crf arm64::cond-eq)
                           (! make-stack-block)
                           (-> done-label)
                           (@ stack-block-0-label)
                           (! make-stack-block0)
                           (@ done-label)))))
                 (arm642-open-undo $undostkblk)
                 (setq val ($ arm64::arg_z)))
                ((eq op (%nx1-operator make-list))
                 (arm642-two-targeted-reg-forms seg (car operands) ($ arm64::arg_y) (cadr operands) ($ arm64::arg_z))
                 (arm642-open-undo $undostkblk curstack)
                 (! make-stack-list)
                 (setq val arm64::arg_z))
                ((eq op (%nx1-operator vector))
                 (let* ((*arm642-vstack* *arm642-vstack*))
                   (arm642-set-nargs seg (arm642-formlist seg (car operands) nil))
                   (! make-stack-vector))
                 (arm642-open-undo $undostkblk)
                 (setq val arm64::arg_z))
                ((eq op (%nx1-operator %gvector))
                 (let* ((*arm642-vstack* *arm642-vstack*)
                        (arglist (car operands)))
                   (arm642-set-nargs seg (arm642-formlist seg (append (car arglist) (reverse (cadr arglist))) nil))
                   (! make-stack-gvector))
                 (arm642-open-undo $undostkblk)
                 (setq val arm64::arg_z))
                ((eq op (%nx1-operator closed-function))
                 (setq val (arm642-make-closure seg (car operands) t))) ; can't error
                ((eq op (%nx1-operator %make-uvector))
                 (destructuring-bind (element-count subtag &optional (init 0 init-p)) operands
                   (if init-p
                       (progn
                         (arm642-three-targeted-reg-forms seg element-count ($ arm64::arg_x) subtag ($ arm64::arg_y) init ($ arm64::arg_z))
                         (! stack-misc-alloc-init))
                       (progn
                         (arm642-two-targeted-reg-forms seg element-count ($ arm64::arg_y) subtag ($ arm64::arg_z))
                         (! stack-misc-alloc)))
                   (arm642-open-undo $undostkblk)
                   (setq val ($ arm64::arg_z)))))))))
  val)

(defun arm642-addrspec-to-reg (seg addrspec reg)
  (if (memory-spec-p addrspec)
    (arm642-stack-to-register seg addrspec reg)
    (arm642-copy-register seg reg addrspec)))

;;; ---------------------------------------------------------------------------
;;; Binding machinery. His arm642-seq-bind @1236 and his lambda handler
;;; @1924 already call arm642-seq-bind-var without defining it.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-seq-bind-var @4429 (v2 @4424 structurally
;;; identical). Drops vs the donors:
;;; - v2's lcell notes (arm64-p2-note-var-cell / note-top-cell) and its
;;; 5-arg vpush-register spelling — no lcells; his vpush-register is
;;; (seg src &optional inhibit-note).
;;; - arm2's (arm2-nfp-bind seg var val) leg — no NFP frame in his backend
;;; yet (nfp vinsns commented out); re-add with NFP support (U7v).
;;; Register wiring: ($ r) per v2 and HIS arm642-init-regvar @1490 (arm2
;;; spells it make-wired-lreg — same object).
;;; vcell vinsns make-stack-vcell / make-vcell per HIS arm642-bind-var
;;; @1258-1262 (arm2 names; v2's make-tsp-vcell not carried).
(defun arm642-seq-bind-var (seg var val)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((sym (var-name var))
           (bits (nx-var-bits var))
           (closed-p (and (%ilogbitp $vbitclosed bits)
                          (%ilogbitp $vbitsetq bits)))
           (curstack (arm642-encode-stack))
           (make-vcell (and closed-p (eq bits (var-bits var))))
           (closed-downward (and closed-p (%ilogbitp $vbitcloseddownward bits))))
      (declare (ignorable sym))
      (unless (fixnump val)
        (setq val (nx-untyped-form val))
        (when (and (%ilogbitp $vbitdynamicextent bits) (acode-p val))
          (setq val (arm642-dynamic-extent-form seg curstack val))))
      (if (%ilogbitp $vbitspecial bits)
        (progn
          (arm642-dbind seg val sym)
          (arm642-set-var-ea seg var (arm642-vloc-ea (- *arm642-vstack* *arm642-target-node-size*))))
        (let ((puntval nil))
          (flet ((arm642-puntable-binding-p (var initform)
 ;; The value returned is acode.
                   (let* ((bits (nx-var-bits var)))
                     (if (%ilogbitp $vbitpuntable bits)
                       initform))))
            (declare (inline arm642-puntable-binding-p))
            (if (and (not (arm642-load-ea-p val))
                     (setq puntval (arm642-puntable-binding-p var val)))
              (progn
                (nx-set-var-bits var (%ilogior (%ilsl $vbitpunted 1) bits))
                (nx2-replace-var-refs var puntval)
                (arm642-set-var-ea seg var puntval))
              (progn
                (let* ((vloc *arm642-vstack*)
                       (reg (let* ((r (nx2-assign-register-var var)))
                              (if r ($ r)))))
                  (if (arm642-load-ea-p val)
                    (if reg
                      (arm642-addrspec-to-reg seg val reg)
                      (if (memory-spec-p val)
                        (with-node-temps () (temp)
                          (arm642-addrspec-to-reg seg val temp)
                          (arm642-vpush-register seg temp))
                        (arm642-vpush-register seg val)))
                    (if reg
                      (arm642-one-targeted-reg-form seg val reg)
                      (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg val arm64::arg_z))))
                  (arm642-set-var-ea seg var (or reg (arm642-vloc-ea vloc closed-p)))
                  (when make-vcell
                    (with-node-temps () (vcell closed)
                        (arm642-stack-to-register seg vloc closed)
                        (if closed-downward
                          (progn
                            (! make-stack-vcell vcell closed)
                            (arm642-open-undo $undostkblk))
                          (! make-vcell vcell closed))
                        (arm642-register-to-stack seg vcell vloc))))))))))))

(defun arm642-bind-var (seg var vloc &aux
                          (bits (nx-var-bits var))
                          (closed-p (and (%ilogbitp $vbitclosed bits) (%ilogbitp $vbitsetq bits)))
                          (closed-downward (if closed-p (%ilogbitp $vbitcloseddownward bits)))
                          (make-vcell (and closed-p (eq bits (var-bits var))))
                          (addr (arm642-vloc-ea vloc)))
  (with-arm64-local-vinsn-macros (seg)
    (if (%ilogbitp $vbitspecial bits)
      (progn
        (arm642-dbind seg addr (var-name var))
        (arm642-set-var-ea seg var (arm642-vloc-ea (- *arm642-vstack* *arm642-target-node-size*)))
        t)
      (progn
        (when (%ilogbitp $vbitpunted bits)
          (compiler-bug "bind-var: var ~s was punted" var))
        (when make-vcell
          (with-node-temps () (vcell closed)
            (arm642-stack-to-register seg vloc closed)
            (if closed-downward
              (progn
                (! make-stack-vcell vcell closed)
                (arm642-open-undo $undostkblk))
              (! make-vcell vcell closed))
            (arm642-register-to-stack seg vcell vloc)))

        (arm642-set-var-ea seg var (arm642-vloc-ea vloc closed-p))
        closed-downward))))

(defun arm642-set-var-ea (seg var ea)
  (setf (var-ea var) ea)
  (when (and *arm642-record-symbols* (or (typep ea 'lreg) (typep ea 'fixnum)))
    (let* ((start (enqueue-vinsn-note seg :begin-variable-scope var)))
      (push (list var (var-name var) start nil)
 *arm642-recorded-symbols*)))
  ea)

(defun arm642-close-var (seg var)
  (let ((bits (nx-var-bits var)))
    (when (and *arm642-record-symbols*
               (or (logbitp $vbitspecial bits)
                   (not (logbitp $vbitpunted bits))))
      (let* ((info (%cdr (assq var *arm642-recorded-symbols*))))
        (unless info (compiler-bug "arm642-close-var for ~s ?" (var-name var)))
        (setf (caddr info) (close-vinsn-note seg (cadr info)))))))

(defun arm642-load-ea-p (ea)
  (or (typep ea 'fixnum)
      (typep ea 'lreg)))

(defun arm642-dbind (seg value sym)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((ea-p (arm642-load-ea-p value))
           (nil-p (unless ea-p (nx-null (setq value (nx-untyped-form value)))))
           (self-p (unless ea-p (and (or
                                      (eq (acode-operator value) (%nx1-operator bound-special-ref))
                                      (eq (acode-operator value) (%nx1-operator special-ref)))
                                     (eq (car (acode-operands value)) sym)))))
      (cond ((eq sym '*interrupt-level*)
             (let* ((fixval (acode-fixnum-form-p value)))
               (cond ((eql fixval 0) (if *arm642-open-code-inline*
                                       (! bind-interrupt-level-0-inline)
                                       (! bind-interrupt-level-0)))
                     ((eql fixval -1) (if *arm642-open-code-inline*
                                        (! bind-interrupt-level-m1-inline)
                                        (! bind-interrupt-level-m1)))
                     (t
                      (if ea-p
                        (arm642-store-ea seg value arm64::arg_z)
                        (arm642-one-targeted-reg-form seg value ($ arm64::arg_z)))
                      (! bind-interrupt-level))))
             (arm642-open-undo $undointerruptlevel))
            (t
             (if (or nil-p self-p)
               (progn
                 (arm642-store-immediate seg (arm642-symbol-value-cell sym) arm64::arg_z)
                 (if nil-p
                   (! bind-nil)
                   (if (or *arm642-reckless* (eq (acode-operator value) (%nx1-operator special-ref)))
                     (! bind-self)
                     (! bind-self-boundp-check))))
               (progn
                 (if ea-p
                   (arm642-store-ea seg value arm64::arg_z)
                   (arm642-one-targeted-reg-form seg value ($ arm64::arg_z)))
                 (arm642-store-immediate seg (arm642-symbol-value-cell sym) ($ arm64::arg_y))
                 (! bind)))
             (arm642-open-undo $undospecial)))
      (arm642-adjust-vstack (* 3 *arm642-target-node-size*)))))

;;; Store the contents of EA - which denotes either a vframe location
;;; or a hard register - in reg.
(defun arm642-store-ea (seg ea reg)
  (if (typep ea 'fixnum)
    (if (memory-spec-p ea)
      (arm642-stack-to-register seg ea reg)
      (arm642-copy-register seg reg ea))
    (if (typep ea 'lreg)
      (arm642-copy-register seg reg ea))))

;;; Callers should really be sure that this is what they want to use.
(defun arm642-absolute-natural (seg vreg xfer value)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (arm642-lri seg vreg value))
    (^)))

(defun arm642-natural-constant (seg vreg xfer value)
  (arm642-use-operator
   (if (typep value *nx-target-fixnum-type*)
     (%nx1-operator fixnum)
     (%nx1-operator immediate))
   seg vreg xfer value))

(defun arm642-store-macptr (seg vreg address-reg)
  (with-arm64-local-vinsn-macros (seg vreg)
    (when (arm642-for-value-p vreg)
      (if (logbitp vreg arm64-imm-regs)
        (<- address-reg)
        (! macptr->heap vreg address-reg)))))

(defun arm642-store-signed-longword (seg vreg imm-reg)
  (with-arm64-local-vinsn-macros (seg vreg)
    (when (arm642-for-value-p vreg)
      (if (logbitp vreg arm64-imm-regs)
        (<- imm-reg)
        (arm642-box-s32 seg vreg imm-reg)))))

(defun arm2-store-signed-halfword (seg vreg imm-reg)
  (with-arm64-local-vinsn-macros (seg vreg)
    (when (arm642-for-value-p vreg)
      (if (logbitp vreg arm64-imm-regs)
        (<- imm-reg)
        (! s16->fixnum vreg imm-reg)))))

(defun arm2-store-unsigned-halfword (seg vreg imm-reg)
  (with-arm64-local-vinsn-macros (seg vreg)
    (when (arm642-for-value-p vreg)
      (if (logbitp vreg arm64-imm-regs)
        (<- imm-reg)
        (! u16->fixnum vreg imm-reg)))))

;;; If "value-first-p" is true and both "offset" and "val" need to be
;;; evaluated, evaluate "val" before evaluating "offset".
;;; Donor: ppc2.lisp:4011 ppc2-%immediate-set-ptr. The (eql size 0)
;;; arm of %immediate-store: store a whole pointer.
(defun arm642-%immediate-set-ptr (seg vreg xfer ptr offset val)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((intval (acode-absolute-ptr-p val))
           (offval (acode-fixnum-form-p offset))
           (for-value (arm642-for-value-p vreg)))
      (flet ((address-and-node-regs ()
               (if for-value
                 (progn
                   (arm642-one-targeted-reg-form seg val ($ arm64::arg_z))
                   (progn
                     (if intval
                       (arm642-lri seg arm64::imm0 intval) ; deviation 3: covers intval=0
                       (! deref-macptr arm64::imm0 arm64::arg_z))
                     (values arm64::imm0 arm64::arg_z)))
                 (if (eq intval 0)
                   (progn
                     (arm642-lri seg arm64::imm0 0) ; deviation 3
                     (values arm64::imm0 nil))
                   (values (arm642-macptr-arg-to-reg seg val ($ arm64::imm0 :mode :address)) nil)))))
 ;; Deviation 1: no absolute-pointer fast path (donor folds
 ;; absptr+offval here); ptr-as-constant falls through to the
 ;; forms below, which materialize it.
 ;; Deviation 2: STUR window; donor's 15-bit + logtest-3 checks.
        (and offval (%i> (integer-length offval) 8) (setq offval nil))
        (if offval
 ;; Easier: need one less register than in the general case.
          (with-imm-target () (ptr-reg :address)
            (arm642-one-targeted-reg-form seg ptr ptr-reg)
            (if intval
              (with-imm-target (ptr-reg) (val-target :address)
                (arm642-lri seg val-target intval) ; deviation 3: covers intval=0
                (! mem-set-c-address val-target ptr-reg offval)
                (if for-value
                  (<- (set-regspec-mode val-target (gpr-mode-name-value :address)))))
              (progn
                (arm642-push-register seg ptr-reg)
                (multiple-value-bind (address node) (address-and-node-regs)
                  (with-imm-target (address) (ptr-reg :address)
                    (arm642-pop-register seg ptr-reg)
                    (! mem-set-c-address address ptr-reg offval)
                    (if for-value
                      (<- node)))))))
 ;; No (simm9) constant offset. Might still have a larger
 ;; constant offset; might have a constant value. Might not.
 ;; Easiest to special-case the constant-value case first ...
          (let* ((xptr-reg nil)
                 (xoff-reg nil)
                 (xval-reg nil)
                 (node-arg_z nil)
                 (constant-offset (acode-fixnum-form-p offset)))
            (if intval
              (if constant-offset
                (with-imm-target () (ptr-reg :address)
                  (arm642-one-targeted-reg-form seg ptr ptr-reg)
                  (with-imm-target (ptr-reg) (off-reg :signed-natural)
                    (arm642-lri seg off-reg constant-offset)
                    (with-imm-target (ptr-reg off-reg) (val-reg :address)
                      (arm642-lri seg val-reg intval) ; deviation 3
                      (setq xptr-reg ptr-reg
                            xoff-reg off-reg
                            xval-reg val-reg))))
 ; Offset's non-constant. Temp-push the pointer, evaluate
 ; and unbox the offset, load the value, pop the pointer.
                (progn
                  (with-imm-target () (ptr-reg :address)
                    (arm642-one-targeted-reg-form seg ptr ptr-reg)
                    (arm642-push-register seg ptr-reg))
                  (with-imm-target () (off-reg :signed-natural)
                    (! fixnum->signed-natural off-reg (arm642-one-targeted-reg-form seg offset ($ arm64::arg_z)))
                    (with-imm-target (off-reg) (val-reg :signed-natural)
                      (arm642-lri seg val-reg intval) ; deviation 3
                      (with-imm-target (off-reg val-reg) (ptr-reg :address)
                        (arm642-pop-register seg ptr-reg)
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg val-reg))))))
 ;; No intval; maybe constant-offset.
              (with-imm-target () (ptr-reg :address)
                (arm642-one-targeted-reg-form seg ptr ptr-reg)
                (arm642-push-register seg ptr-reg)
                (progn
                  (if (not constant-offset)
                    (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg offset arm64::arg_z)))
                  (multiple-value-bind (address node) (address-and-node-regs)
                    (with-imm-target (address) (off-reg :s32)
                      (if constant-offset
                        (arm642-lri seg off-reg constant-offset)
                        (with-node-temps (arm64::arg_z) (temp)
                          (arm642-vpop-register seg temp)
                          (! fixnum->signed-natural off-reg temp)))
                      (with-imm-target (arm64::imm0 off-reg) (ptr-reg :address)
                        (arm642-pop-register seg ptr-reg)
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg address
                              node-arg_z node)))))))
            (! mem-set-address xval-reg xptr-reg xoff-reg)
            (when for-value
              (if node-arg_z
                (<- node-arg_z)
                (<- (set-regspec-mode
                     xval-reg
                     (gpr-mode-name-value :address)))))))
        (^)))))

;;; Donors: ppc2.lisp:4135/:4143.
(defun arm642-memory-store-displaced (seg valreg basereg displacement size)
  (with-arm64-local-vinsn-macros (seg)
    (case size
      (8 (! mem-set-c-doubleword valreg basereg displacement))
      (4 (! mem-set-c-fullword valreg basereg displacement))
      (2 (! mem-set-c-halfword valreg basereg displacement))
      (1 (! mem-set-c-byte valreg basereg displacement)))))

(defun arm642-memory-store-indexed (seg valreg basereg idxreg size)
  (with-arm64-local-vinsn-macros (seg)
    (case size
      (8 (! mem-set-doubleword valreg basereg idxreg))
      (4 (! mem-set-fullword valreg basereg idxreg))
      (2 (! mem-set-halfword valreg basereg idxreg))
      (1 (! mem-set-byte valreg basereg idxreg)))))

;;; Donor: ppc2.lisp:4151 ppc2-%immediate-store.
(defun arm642-%immediate-store (seg vreg xfer bits ptr offset val)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (if (eql 0 (%ilogand #xf bits))
      (arm642-%immediate-set-ptr seg vreg xfer ptr offset val)
      (let* ((size (logand #xf bits))
             (nbits (ash size 3))
             (signed (not (logbitp 5 bits)))
             (intval (acode-integer-constant-p val nbits))
             (offval (acode-fixnum-form-p offset))
             (for-value (arm642-for-value-p vreg)))
        (declare (fixnum size))
        (flet ((val-to-argz-and-imm0 ()
                 (arm642-one-targeted-reg-form seg val ($ arm64::arg_z))
                 (if (eql size 8)
                   (if signed
                     (! gets64)
                     (! getu64))
 ;; sizes 1/2/4: value provably fits a fixnum
 ;; (nbits ≤ 32 < fixnum precision); ppc32's
 ;; gets32/getu32 leg does not apply.
                   (! fixnum->signed-natural arm64::imm0 arm64::arg_z))))
 ;; Deviations 1 & 2 (see header): no absptr fast path; STUR
 ;; window for constant displacements.
          (and offval (%i> (integer-length offval) 8) (setq offval nil))
          (if offval
 ;; Easier: need one less register than in the general case.
            (with-imm-target () (ptr-reg :address)
              (arm642-one-targeted-reg-form seg ptr ptr-reg)
              (if intval
                (with-imm-target (ptr-reg) (val-target :s32)
                  (arm642-lri seg val-target intval) ; deviation 3: covers intval=0
                  (arm642-memory-store-displaced seg val-target ptr-reg offval size)
                  (if for-value
                    (<- (set-regspec-mode
                         val-target
                         (gpr-mode-name-value
                          (case size
                            (8 (if signed :s64 :u64))
                            (4 (if signed :s32 :u32))
                            (2 (if signed :s16 :u16))
                            (1 (if signed :s8 :u8))))))))
                (progn
                  (arm642-push-register seg ptr-reg)
                  (val-to-argz-and-imm0)
                  (with-imm-target (arm64::imm0) (ptr-reg :address)
                    (arm642-pop-register seg ptr-reg)
                    (arm642-memory-store-displaced seg arm64::imm0 ptr-reg offval size)
                    (if for-value
                      (<- arm64::arg_z))))))
 ;; No (simm9) constant offset. Might still have a larger
 ;; constant offset; might have a constant value. Might not.
 ;; Might not. Easiest to special-case the constant-value
 ;; case first ...
            (let* ((xptr-reg nil)
                   (xoff-reg nil)
                   (xval-reg nil)
                   (node-arg_z nil)
                   (constant-offset (acode-fixnum-form-p offset)))
              (if intval
                (if constant-offset
                  (with-imm-target () (ptr-reg :address)
                    (arm642-one-targeted-reg-form seg ptr ptr-reg)
                    (with-imm-target (ptr-reg) (off-reg :s32)
                      (arm642-lri seg off-reg constant-offset)
                      (with-imm-target (ptr-reg off-reg) (val-reg :s32)
                        (arm642-lri seg val-reg intval) ; deviation 3
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg val-reg))))
 ; Offset's non-constant. Temp-push the pointer, evaluate
 ; and unbox the offset, load the value, pop the pointer.
                  (progn
                    (with-imm-target () (ptr-reg :address)
                      (arm642-one-targeted-reg-form seg ptr ptr-reg)
                      (arm642-push-register seg ptr-reg))
                    (with-imm-target () (off-reg :s32)
                      (! fixnum->signed-natural off-reg (arm642-one-targeted-reg-form seg offset ($ arm64::arg_z)))
                      (with-imm-target (off-reg) (val-reg :s32)
                        (arm642-lri seg val-reg intval) ; deviation 3
                        (with-imm-target (off-reg val-reg) (ptr-reg :address)
                          (arm642-pop-register seg ptr-reg)
                          (setq xptr-reg ptr-reg
                                xoff-reg off-reg
                                xval-reg val-reg))))))
 ;; No intval; maybe constant-offset.
                (with-imm-target () (ptr-reg :address)
                  (arm642-one-targeted-reg-form seg ptr ptr-reg)
                  (arm642-push-register seg ptr-reg)
                  (progn
                    (if (not constant-offset)
                      (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg offset arm64::arg_z)))
                    (val-to-argz-and-imm0)
                    (with-imm-target (arm64::imm0) (off-reg :signed-natural)
                      (if constant-offset
                        (arm642-lri seg off-reg constant-offset)
                        (with-node-temps (arm64::arg_z) (temp)
                          (arm642-vpop-register seg temp)
                          (! fixnum->signed-natural off-reg temp)))
                      (with-imm-target (arm64::imm0 off-reg) (ptr-reg :address)
                        (arm642-pop-register seg ptr-reg)
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg arm64::imm0
                              node-arg_z t))))))
              (arm642-memory-store-indexed seg xval-reg xptr-reg xoff-reg size)
              (when for-value
                (if node-arg_z
                  (<- arm64::arg_z)
                  (<- (set-regspec-mode
                       xval-reg
                       (gpr-mode-name-value
                        (case size
                          (8 (if signed :s64 :u64))
                          (4 (if signed :s32 :u32))
                          (2 (if signed :s16 :u16))
                          (1 (if signed :s8 :u8))))))))))
          (^))))))

(defun arm642-encoding-undo-count (encoding)
  (svref encoding 0))

(defun arm642-encoding-cstack-depth (encoding)
  (svref encoding 1))

(defun arm642-encoding-vstack-depth (encoding)
  (svref encoding 2))

(defun arm642-encode-stack ()
  (vector *arm642-undo-count* *arm642-cstack* *arm642-vstack* ))

(defun arm642-decode-stack (encoding)
  (values (arm642-encoding-undo-count encoding)
          (arm642-encoding-cstack-depth encoding)
          (arm642-encoding-vstack-depth encoding)))

(defun arm642-equal-encodings-p (a b)
  (dotimes (i 3 t)
    (unless (eq (svref a i) (svref b i)) (return))))

(defun arm642-open-undo (&optional (reason $undocatch)
                           (curstack (arm642-encode-stack)))
  (set-fill-pointer
 *arm642-undo-stack*
   (set-fill-pointer *arm642-undo-because* *arm642-undo-count*))
  (vector-push-extend curstack *arm642-undo-stack*)
  (vector-push-extend reason *arm642-undo-because*)
  (setq *arm642-undo-count* (%i+ *arm642-undo-count* 1)))

(defun arm642-close-undo (&aux
                        (new-count (%i- *arm642-undo-count* 1))
                        (i (aref *arm642-undo-stack* new-count)))
  (multiple-value-setq (*arm642-undo-count* *arm642-cstack* *arm642-vstack* )
    (arm642-decode-stack i))
  (set-fill-pointer
 *arm642-undo-stack*
   (set-fill-pointer *arm642-undo-because* new-count)))

(defun arm642-nfp-ref-p (form)
  (let* ((op (if (acode-p form) (acode-operator form))))
    (if (or (eq op (%nx1-operator inherited-arg))
            (eq op (%nx1-operator lexical-reference)))
      (let* ((var (car (acode-operands form))))
        (not (null (rassoc var *arm642-nfp-vars*)))))))

;;; "Trivial" means can be evaluated without allocating or modifying registers.
;;; Interim definition, which will probably stay here forever.
(defun arm642-trivial-p (form &aux op bits)
  (setq form (nx-untyped-form form))
  (and (acode-p form)
       (not (eq (setq op (acode-operator form)) (%nx1-operator call)))
       (or (nx-null form)
           (nx-t form)
           (eq op (%nx1-operator simple-function))
           (eq op (%nx1-operator fixnum))
           (eq op (%nx1-operator immediate))
 #+nil
           (eq op (%nx1-operator bound-special-ref))
           (and
            (or (eq op (%nx1-operator inherited-arg))
                (eq op (%nx1-operator lexical-reference)))
            (or (%ilogbitp $vbitpunted (setq bits (nx-var-bits
                                                   (car (acode-operands
                                                         form)))))
                (neq
                 (%ilogior (%ilsl $vbitclosed 1) (%ilsl $vbitsetq 1))
                 (%ilogand (%ilogior (%ilsl $vbitclosed 1) (%ilsl $vbitsetq 1))
                           bits)))))))

(defun arm642-lexical-reference-p (form)
  (when (acode-p form)
    (let ((op (acode-operator (setq form (acode-unwrapped-form-value form)))))
      (when (or (eq op (%nx1-operator lexical-reference))
                (eq op (%nx1-operator inherited-arg)))
        (car (acode-operands form))))))

(defun arm642-ref-symbol-value (seg vreg xfer sym check-boundp)
  (declare (ignorable check-boundp))
  (setq check-boundp (not *arm642-reckless*))
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (when (or check-boundp vreg)
      (unless vreg (setq vreg ($ arm64::arg_z)))
      (if (eq sym '*interrupt-level*)
        (ensuring-node-target (target vreg)
          (! ref-interrupt-level target))
        (if *arm642-open-code-inline*
          (ensuring-node-target (target vreg)
            (with-node-target (target) src
              (let* ((vcell (arm642-symbol-value-cell sym))
                     (reg (arm642-register-constant-p vcell)))
                (if reg
                  (setq src reg)
                  (arm642-store-immediate seg vcell src)))
              (if check-boundp
                (! ref-symbol-value-inline target src)
                (! %ref-symbol-value-inline target src))))
          (let* ((src ($ arm64::arg_z))
                 (dest ($ arm64::arg_z)))
            (arm642-store-immediate seg (arm642-symbol-value-cell sym) src)
            (if check-boundp
              (! ref-symbol-value dest src)
              (! %ref-symbol-value dest src))
            (<- dest)))))
    (^)))

(defun arm642-extract-charcode (seg vreg xfer char safe)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((src (arm642-one-untargeted-reg-form seg char arm64::arg_z)))
      (when safe
        (! trap-unless-character src))
      (if vreg
        (ensuring-node-target (target vreg)
          (! character->fixnum target src)))
      (^))))

(defun arm642-reference-list (seg vreg xfer listform safe refcdr)
  (if (arm642-form-typep listform 'list)
    (setq safe nil))
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((src (arm642-one-untargeted-reg-form seg listform arm64::arg_z)))
      (when safe
        (! trap-unless-list src))
      (if vreg
        (ensuring-node-target (target vreg)
          (if refcdr
            (! %cdr target src)
            (! %car target src))))
      (^))))

(defun arm642-misc-byte-count (subtag element-count)
  (funcall (arch::target-array-data-size-function
            (backend-target-arch *target-backend*))
           subtag element-count))

(defun arm642-allocate-initialized-gvector (seg vreg xfer subtag initforms)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (if (null vreg)
      (dolist (f initforms) (arm642-form seg nil nil f))
      (let* ((*arm642-vstack* *arm642-vstack*)
             (arch (backend-target-arch *target-backend*))
             (n (length initforms))
             (nntriv (let* ((count 0))
                       (declare (fixnum count))
                       (dolist (f initforms count)
                         (unless (arm64-side-effect-free-form-p f)
                           (incf count)))))
             (header (arch::make-vheader n subtag)))
        (declare (fixnum n nntriv))
        (cond ((or *arm642-open-code-inline* (> nntriv 3))
               (arm642-formlist seg initforms nil)
               (arm642-lri seg arm64::imm0 header)
               (! %arm64-gvector vreg arm64::imm0 (ash n (arch::target-word-shift arch))))
              (t
               (let* ((pending ())
                      (vstack *arm642-vstack*))
                 (declare (fixnum vstack))
                 (dolist (form initforms)
                   (if (arm64-side-effect-free-form-p form)
                     (push form pending)
                     (progn
                       (push nil pending)
                       (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg form arm64::arg_z)))))
                 (arm642-lri seg arm64::imm0 header)
                 (ensuring-node-target (target vreg)
                   (! %alloc-misc-fixed target arm64::imm0 (ash n (arch::target-word-shift arch)))
                   (with-node-temps (target) (nodetemp)
                     (do* ((forms pending (cdr forms))
                           (index (1- n) (1- index))
                           (pushed-cell (+ vstack (the fixnum (ash nntriv (arch::target-word-shift arch))))))
                          ((null forms))
                       (declare (list forms) (fixnum pushed-cell))
                       (let* ((form (car forms))
                              (reg nodetemp))
                         (if form
                           (setq reg (arm642-one-untargeted-reg-form seg form nodetemp))
                           (progn
                             (decf pushed-cell *arm642-target-node-size*)
                             (arm642-stack-to-register seg (arm642-vloc-ea pushed-cell) nodetemp)))
                         (! misc-set-c-node reg target index)))))
                 (! vstack-discard nntriv))))))
    (^)))

(defun arm642-acode-needs-memoization (valform)
  (if (arm642-form-typep valform 'fixnum)
    nil
    (let* ((val (acode-unwrapped-form-value valform)))
      (if (or (nx-t val)
              (nx-null val)
              (and (acode-p val)
                   (let* ((op (acode-operator val)))
                     (or (eq op (%nx1-operator fixnum))))))
        nil
        t))))

(defun arm642-modify-cons (seg vreg xfer ptrform valform safe setcdr returnptr)
  (if (arm642-form-typep ptrform 'cons)
    (setq safe nil))
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (ptr-vreg val-vreg) (arm642-two-targeted-reg-forms seg ptrform ($ arm64::arg_y) valform ($ arm64::arg_z))
      (when safe
        (! trap-unless-cons ptr-vreg))
      (if setcdr
        (! call-subprim-2 ($ arm64::arg_z) (arm64::arm64-subprimitive-offset '.SPrplacd) ptr-vreg val-vreg)
        (! call-subprim-2 ($ arm64::arg_z) (arm64::arm64-subprimitive-offset '.SPrplaca) ptr-vreg val-vreg))
      (if returnptr
        (<- ptr-vreg)
        (<- val-vreg))
      (^))))

;;; If we know that the form is something that sets a CR bit,
;;; allocate a CR field and evaluate the form in such a way
;;; as to set that bit.
;;; If it's a compile-time constant, branch accordingly and
;;; let the dead code die.
;;; Otherwise, evaluate it to some handy register and compare
;;; that register to RNIL.
;;; "XFER" is a compound destination.
(defun arm642-conditional-form (seg xfer form)
  (let* ((uwf (acode-unwrapped-form-value form)))
    (if (nx-null uwf)
      (arm642-branch seg (arm642-cd-false xfer) nil)
      (if (arm64-constant-form-p uwf)
        (arm642-branch seg (arm642-cd-true xfer) nil)
        (with-crf-target () crf
          (arm642-form seg crf xfer form))))))

(defun arm642-branch (seg xfer crf &optional cr-bit true-p)
  (declare (notinline arm642-branch))
  (let* ((*arm642-vstack* *arm642-vstack*))
    (with-arm64-local-vinsn-macros (seg)
      (setq xfer (or xfer 0))
      (when (logbitp $backend-mvpass-bit xfer)
        (setq xfer (logand (lognot $backend-mvpass-mask) xfer))
        (unless *arm642-returning-values*
          (arm642-vpush-register seg arm64::arg_z)
          (arm642-set-nargs seg 1)))
      (if (neq 0 xfer)
        (if (eq xfer $backend-return)
          (arm642-do-return seg)
          (if (not (arm642-cd-compound-p xfer))
            (-> xfer)
 ;; cd is compound : (<true> / <false>)
            (let* ((truebranch (arm642-cd-true xfer))
                   (falsebranch (arm642-cd-false xfer))
                   (tbranch (if true-p truebranch falsebranch))
                   (nbranch (if true-p falsebranch truebranch))
                   (tn0 (neq 0 tbranch))
                   (tnret (neq $backend-return tbranch))
                   (nn0 (neq 0 nbranch))
                   (nnret (neq $backend-return nbranch))
                   (tlabel (if (and tnret tn0) (aref *backend-labels* tbranch)))
                   (nlabel (if (and nnret nn0) (aref *backend-labels* nbranch))))
              (unless cr-bit (setq cr-bit
                                   (arm64::lookup-arm64-condition-name "eq")))
              (if (and tn0 tnret nn0 nnret)
                (progn
                  (! cbranch-true tlabel crf cr-bit)
                  (-> nbranch))
                (if (and nnret tnret)
                  (if nn0
                    (! cbranch-false nlabel crf cr-bit)
                    (! cbranch-true tlabel crf cr-bit))
                  (let* ((aux-label (backend-get-next-label))
                         (auxl (aref *backend-labels* aux-label)))
                    (if tn0
                      (! cbranch-true auxl crf cr-bit)
                      (! cbranch-false auxl crf cr-bit))
                    (arm642-do-return seg)
                    (@ aux-label)))))))))))

;; cd means compound destination, probably
(defun arm642-cd-merge (cd label)
  (setq cd (or cd 0))
  (let ((mvpass (logbitp $backend-mvpass-bit cd)))
    (if (neq 0 (logand (lognot $backend-mvpass-mask) cd))
      (if (arm642-cd-compound-p cd)
        (arm642-make-compound-cd
         (arm642-cd-merge (arm642-cd-true cd) label)
         (arm642-cd-merge (arm642-cd-false cd) label)
         mvpass)
        cd)
      (if mvpass
        (logior $backend-mvpass-mask label)
        label))))

(defun arm642-mvpass-p (xfer)
  (if xfer (or (logbitp $backend-mvpass-bit xfer) (eq xfer $backend-mvpass))))

(defun arm642-cd-compound-p (xfer)
  (if xfer (logbitp $backend-compound-branch-target-bit xfer)))

(defun arm642-cd-true (xfer)
 (if (arm642-cd-compound-p xfer)
   (ldb $backend-compound-branch-true-byte xfer)
  xfer))

(defun arm642-cd-false (xfer)
 (if (arm642-cd-compound-p xfer)
   (ldb $backend-compound-branch-false-byte xfer)
   xfer))

(defun arm642-make-compound-cd (tpart npart &optional mvpass-p)
  (dpb (or npart 0) $backend-compound-branch-false-byte
       (dpb (or tpart 0) $backend-compound-branch-true-byte
            (logior (if mvpass-p $backend-mvpass-mask 0) $backend-compound-branch-target-mask))))

(defun arm642-invert-cd (cd)
  (if (arm642-cd-compound-p cd)
    (arm642-make-compound-cd (arm642-cd-false cd) (arm642-cd-true cd) (logbitp $backend-mvpass-bit cd))
    cd))

;;; Execute body, cleanup afterwards (if need to)
(defun arm642-undo-body (seg vreg xfer body old-stack)
  (let* ((current-stack (arm642-encode-stack))
         (numundo (%i- *arm642-undo-count*
                       (arm642-encoding-undo-count old-stack))))
    (declare (fixnum numundo))
    (with-arm64-local-vinsn-macros (seg vreg xfer)
      (if (arm642-equal-encodings-p current-stack old-stack)
        (arm642-form seg vreg xfer body)
        (if (eq xfer $backend-return)
          (progn
            (arm642-form seg vreg xfer body)
            (dotimes (i numundo) (arm642-close-undo)))
          (if (arm642-mvpass-p xfer)
            (progn
              (arm642-mvpass seg body)
              (let* ((*arm642-returning-values* :pass))
                (arm642-nlexit seg xfer numundo)
                (^))
              (dotimes (i numundo) (arm642-close-undo)))
            (progn
              (arm642-form seg (if (or vreg (not (%izerop numundo)))
                                 arm64::arg_z)
                           nil body)
              (arm642-unwind-set seg xfer old-stack)
              (when vreg (<- arm64::arg_z))
              (^))))))))

(defun arm642-unwind-set (seg xfer encoding)
  (multiple-value-bind (target-catch target-cstack target-vstack)
      (arm642-decode-stack encoding)
    (arm642-unwind-stack seg xfer target-catch target-cstack target-vstack)
    (arm642-regmap-note-vstack-delta target-vstack *arm642-vstack*)
    (setq *arm642-undo-count* target-catch
 *arm642-cstack* target-cstack
 *arm642-vstack* target-vstack)))

(defun arm642-unwind-stack (seg xfer target-catch target-cstack target-vstack)
  (let* ((current-catch *arm642-undo-count*)
         (current-cstack *arm642-cstack*)
         (current-vstack *arm642-vstack*)
         (diff (%i- current-catch target-catch))
         target
         (exit-vstack current-vstack))
    (declare (ignore-if-unused target))
    (when (neq 0 diff)
      (setq exit-vstack (arm642-nlexit seg xfer diff))
      (multiple-value-setq (target current-cstack current-vstack)
        (arm642-decode-stack (aref *arm642-undo-stack* target-catch))))
    (if (%i< 0 (setq diff (%i- current-cstack target-cstack)))
      (with-arm64-local-vinsn-macros (seg)
        (! adjust-sp diff)))
    (if (%i< 0 (setq diff (%i- current-vstack target-vstack)))
      (with-arm64-local-vinsn-macros (seg)
        (! vstack-discard (ash diff (- arm64::word-shift)))))
    exit-vstack))

;;; arm642-do-return â€” NFP unit (2026-07-25, ): the balancing
;;; `restore-nfp` for the `(! save-nfp)` this file emits at the
;;; arm642-lambda convergence point (@440).
;;;
;;; His arm642.lisp:4987 emits NO restore-nfp on either return leg. That
;;; was correct while nothing emitted save-nfp -- and the -additions report
;;; recorded it as U1: "balance requires omission. When Matt lands nfp
;;; support these must be re-added." We landed nfp support ourselves in
;;; this file (@440) and never re-added them, so every function with an nfp
;;; frame returned through do-return with the frame still pushed: `popj`
;;; then tore down the 4-word nfp frame {header, saved-link, data, data} AS
;;; IF it were a lisp_frame, loading fn from the data word and vsp from the
;;; saved link (0). Observed as vsp=0 at pc 0x3000000778e8 with
;;; fn=0x477fe0 and tcr.nfp left dangling at the dead frame.
;;;
;;; Donor: ppc2.lisp:4793-4810 -- restore-nfp AFTER restore-nvrs and
;;; IMMEDIATELY BEFORE the frame teardown (nvalret / popj), on every leg.
;;; PPC's four sites collapse to two here because his do-return has no
;;; mask/foldp/open-code-inline branches. Mirrors v2 arm64-p2.lisp:5462-5479.
(defun arm642-do-return (seg)
  (let* ((*arm642-vstack* *arm642-vstack*))
    (with-arm64-local-vinsn-macros (seg)
      (progn
        (arm642-set-vstack (arm642-unwind-stack seg $backend-return
                                                0 0 #x7fffff))
        (if *arm642-returning-values*
          (progn
            (arm642-restore-nvrs seg t)
            (! restore-nfp)
            (! nvalret))
          (progn
            (arm642-restore-nvrs seg nil)
            (! restore-nfp)
            (! popj)))))
    nil))
(defun arm642-mvcall (seg vreg xfer fn arglist &optional recursive-p)
  (let* ((cstack *arm642-cstack*)
         (vstack *arm642-vstack*))
    (with-arm64-local-vinsn-macros (seg vreg xfer)
      (if (and (eq xfer $backend-return) (not (arm642-tailcallok xfer)))
        (progn
          (arm642-mvcall seg vreg $backend-mvpass fn arglist t)
          (arm642-set-vstack (%i+ (if arglist *arm642-target-node-size* 0) vstack))
          (setq *arm642-cstack* cstack)
          (let* ((*arm642-returning-values* t)) (^)))
        (let* ((mv-p (arm642-mv-p xfer)))
          (if (null arglist)
            (arm642-call-fn seg vreg xfer fn arglist nil)
            (progn
              (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg fn arm64::arg_z))
              (arm642-multiple-value-body seg (pop arglist))
              (when arglist
                (arm642-open-undo $undostkblk)
                (! save-values)
                (dolist (form arglist)
                  (arm642-multiple-value-body seg form)
                  (! add-values))
                (arm642-set-nargs seg 0)
                (! recover-values)
                (arm642-close-undo))
              (! lisp-word-ref arm64::nfn arm64::vsp arm64::nargs)
              (arm642-invoke-fn seg arm64::nfn nil nil xfer)))
          (unless recursive-p
            (if mv-p
              (unless (eq xfer $backend-return)
                (let* ((*arm642-returning-values* t))
                  (^)))
              (progn
                (arm642-adjust-vstack (- *arm642-target-node-size*))
                (! vstack-discard 1)
                (<- arm64::arg_z)
                (^)))))))))

(defun arm642-hard-opt-p (opts)
  (or
   (dolist (x (%cadr opts))
     (unless (nx-null x) (return t)))
   (dolist (x (%caddr opts))
     (when x (return t)))))

(defun arm642-close-lambda (seg req opt rest keys auxen)
  (dolist (var req)
    (arm642-close-var seg var))
  (dolist (var (%car opt))
    (arm642-close-var seg var))
  (dolist (var (%caddr opt))
    (when var
      (arm642-close-var seg var)))
  (if rest
    (arm642-close-var seg rest))
  (dolist (var (%cadr keys))
    (arm642-close-var seg var))
  (dolist (var (%caddr keys))
    (if var (arm642-close-var seg var)))
  (dolist (var (%car auxen))
    (arm642-close-var seg var)))

(defun arm642-init-regvar (seg var reg addr)
  (with-arm64-local-vinsn-macros (seg)
    (arm642-stack-to-register seg addr reg)
    (arm642-set-var-ea seg var ($ reg))))

(defun arm642-simple-var (var &aux (bits (cadr var)))
  (if (or (%ilogbitp $vbitclosed bits)
          (%ilogbitp $vbitspecial bits))
    (nx-error "Non-simple-variable ~S" (%car var))
    var))

(defun arm642-nlexit (seg xfer &optional (nlevels 0))
  (let* ((numnthrow 0)
         (n *arm642-undo-count*)
         (cstack *arm642-cstack*)
         (vstack *arm642-vstack*)
         (target-cstack)
         (target-vstack)
         (lastcatch n)
         (i nil)
         (returning (eq xfer $backend-return))
         (junk1 nil)
         (unbind ())
         (dest (%i- n nlevels))
         (retval *arm642-returning-values*)
         reason)
    (declare (ignorable junk1))
    (with-arm64-local-vinsn-macros (seg)
      (when (neq 0 nlevels)
        (let* ((numnlispareas 0))
          (declare (fixnum numnlispareas))
          (flet ((popnlispareas ()
                   (dotimes (i numnlispareas)
                     (! discard-temp-frame)))
                 (throw-through-numnthrow-catch-frames ()
                   (when (neq 0 numnthrow)
                     (arm642-lri seg arm64::imm0 (ash numnthrow *arm642-target-fixnum-shift*))
                     (if retval
                       (! nthrowvalues)
                       (! nthrow1value))
                     (setq numnthrow 0)
                     (multiple-value-setq (junk1 cstack vstack)
                       (arm642-decode-stack (aref *arm642-undo-stack* lastcatch))))))
            (while (%i> n dest)
              (cond ((eql $undocatch (setq reason (aref *arm642-undo-because* (setq n (%i- n 1)))))
                     (popnlispareas)
                     (setq numnthrow (%i+ numnthrow 1) lastcatch n))
                    ((eql $undostkblk reason)
                     (throw-through-numnthrow-catch-frames)
                     (incf numnlispareas))
                    ((eql $undo-arm64-c-frame reason)
                     (! discard-c-frame))))
            (throw-through-numnthrow-catch-frames)
            (setq i lastcatch)
            (while (%i> i dest)
              (let ((reason (aref *arm642-undo-because* (setq i (%i- i 1)))))
                (if (or (eql reason $undospecial)
                        (eql reason $undointerruptlevel))
                  (push reason unbind))))
            (if unbind
              (arm642-dpayback-list seg (nreverse unbind)))
            (when (and (neq lastcatch dest)
                       (%i>
                        vstack
                        (setq target-vstack
                              (nth-value 2 (arm642-decode-stack
                                            (aref *arm642-undo-stack* dest)))))
                       (neq retval t))
              (unless returning
                (let ((vdiff (%i- vstack target-vstack)))
                  (if retval
                    (progn
                      (arm642-lri seg arm64::imm0 vdiff)
                      (! slide-values))
                    (! adjust-vsp vdiff)))))
            (setq numnlispareas 0)
            (while (%i> lastcatch dest)
              (let ((reason (aref *arm642-undo-because* (setq lastcatch (%i- lastcatch 1)))))
                (setq target-cstack
                      (nth-value 1 (arm642-decode-stack
                                    (aref *arm642-undo-stack* lastcatch))))
                (if (eq reason $undostkblk)
                  (incf numnlispareas))
                (if (%i> cstack target-cstack)
                  (with-arm64-local-vinsn-macros (seg)
                    (! adjust-sp (%i- cstack target-cstack))))
                (setq cstack target-cstack)))
            (popnlispareas)))
        vstack))))

;;; Restore the most recent dynamic bindings. Bindings
;;; of *INTERRUPT-LEVEL* get special treatment.
(defun arm642-dpayback-list (seg reasons)
  (with-arm64-local-vinsn-macros (seg)
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
              (if *arm642-open-code-inline*
                (! unbind-interrupt-level-inline)
                (! unbind-interrupt-level)))
            (compiler-bug "unknown payback token ~s" r)))))))

(defun arm642-tailcallok (xfer)
  (and (eq xfer $backend-return)
 *arm642-tail-allow*
       (eq 0 *arm642-undo-count*)))

(defun arm642-mv-p (cd)
  (or (eq cd $backend-return) (arm642-mvpass-p cd)))

;; The cmp instruction takes a 12-bit immediate; return true if n fits.
(defun arm642-aimm-p (n)
  (< n 4096))

;;; vinsn expansion

(defun arm642-expand-vinsns (header current)
  (do-dll-nodes (v header)
    (if (%vinsn-label-p v)
      (let* ((id (vinsn-label-id v)))
        (if (or (typep id 'fixnum) (null id))
          (when (or t (vinsn-label-refs v) (null id))
            (setf (vinsn-label-info v) (arm64::emit-label current v)))))
      (arm642-expand-vinsn v current)))
 ;;; Fix up var-eas from lregs to their values before lregs are freed.
  (dolist (s *arm642-recorded-symbols*)
    (let* ((var (car s))
           (ea (var-ea var)))
      (when (typep ea 'lreg)
        (setf (var-ea var) (lreg-value ea))))))

;;; Build a register-operand for a filled-in vinsn operand. DESC is the
;;; register descriptor -- (:opnd i) / (:reg n) for a plain register,
;;; (:shifted-reg reg-desc modifier amount) for a shifted/extended one, or
;;; (:velt-opnd i esize index) / (:velt-reg n esize index) for a vector
;;; lane. SPEC is the template's operand spec (role class), giving the
;;; register view. Mirrors the class mapping in DECODE-REGISTER-OPERAND.
(defun arm642-vinsn-register-operand (desc spec vp)
  (case (car desc)
    ((:velt-opnd :velt-reg)
 ;; A vector lane operand. The number goes to the role field and
 ;; esize/index to imm5/imm4 at encode; the 128-bit Vn view is just a
 ;; carrier for the number.
     (destructuring-bind (reg-or-index esize index) (cdr desc)
       (arm64::make-vector-element-operand
        :register (arm64::fpr-ref (ecase (car desc)
                                    (:velt-opnd (svref vp reg-or-index))
                                    (:velt-reg reg-or-index))
                                  128)
        :esize esize :index index)))
    ((:varr-opnd :varr-reg)
 ;; A whole-vector arrangement operand. The number goes to the role
 ;; field, the arrangement to the Q and size bits, at encode.
     (destructuring-bind (reg-or-index arrangement) (cdr desc)
       (arm64::make-vector-arrangement-operand
        :register (arm64::fpr-ref (ecase (car desc)
                                    (:varr-opnd (svref vp reg-or-index))
                                    (:varr-reg reg-or-index))
                                  128)
        :arrangement arrangement)))
    (t
     (multiple-value-bind (number modifier amount)
         (ecase (car desc)
           (:opnd (values (svref vp (cadr desc)) nil 0))
           (:reg  (values (cadr desc) nil 0))
           (:shifted-reg
            (destructuring-bind (reg-desc modifier amount) (cdr desc)
              (values (ecase (car reg-desc)
                        (:opnd (svref vp (cadr reg-desc)))
                        (:reg  (cadr reg-desc)))
                      modifier amount))))
       (arm64::make-register-operand
        :register
        (ecase (cadr spec)
          ((:x :x-shift :x-shift-ror :x-ext) (arm64::gpr-ref number 64))
          (:x/sp (arm64::gpr-ref number 64 t))
          ((:w :w-shift :w-shift-ror :w-ext) (arm64::gpr-ref number 32))
          (:w/sp (arm64::gpr-ref number 32 t))
          (:sp (arm64::gpr-ref 31 64 t))
          (:wsp (arm64::gpr-ref 31 32 t))
          (:s (arm64::fpr-ref number 32))
          (:d (arm64::fpr-ref number 64))
          (:q (arm64::fpr-ref number 128))
          (:b (arm64::fpr-ref number 8))
          (:h (arm64::fpr-ref number 16)))
        :modifier modifier :amount amount)))))

;;; Build the operand struct for one filled-in body operand. DESC is the
;;; stored descriptor, SPEC the template's operand spec at this position,
;;; VP the expanding vinsn's variable-parts, and UNIQUE-LABELS the map
;;; from each template-local label keyword to a fresh per-expansion label
;;; object. A (:label class) spec builds a label-operand -- naming either
;;; a backend vinsn-label passed in through VP, or a template-local label;
;;; any other spec builds a register-operand.
;;; Evaluate one (:imm-apply ...) argument descriptor against the vp
;;; vector: (:opnd i) is a hole filled from vp, (:apply fn arg...) is a
;;; nested apply evaluated recursively, anything else is a constant.
(defun arm642-eval-imm-apply-arg (a vp)
  (cond
    ((and (consp a) (eq (car a) :opnd)) (svref vp (cadr a)))
    ((and (consp a) (eq (car a) :apply))
     (apply (cadr a) (mapcar #'(lambda (x) (arm642-eval-imm-apply-arg x vp))
                             (cddr a))))
    (t a)))

;;; Build an immediate-operand for a filled-in body operand. DESC is
;;; (:imm value shift), (:imm-opnd vp-index shift), or
;;; (:imm-apply shift fn . args) where each arg is a constant, an
;;; (:opnd vp-index) hole, or a nested (:apply fn arg...). SPEC is the
;;; immediate class. Since a wild immediate's value wasn't known when the
;;; template was chosen, its range is only checkable now: matching
;;; guaranteed the class, not the fit.
(defun arm642-vinsn-immediate-operand (desc spec vp)
  (multiple-value-bind (value shift)
      (ecase (car desc)
        (:imm (values (cadr desc) (caddr desc)))
        (:imm-opnd (values (svref vp (cadr desc)) (caddr desc)))
        (:imm-apply
         (destructuring-bind (shift fn . args) (cdr desc)
           (values (apply fn (mapcar #'(lambda (a)
                                         (arm642-eval-imm-apply-arg a vp))
                                     args))
                   shift))))
    (let ((imm (arm64::make-immediate-operand :value value :shift shift)))
      (unless (arm64::match-immediate-operand imm spec)
        (compiler-bug "vinsn immediate ~s (shift ~s) out of range for ~
                       operand class ~s" value shift spec))
      imm)))

;;; Build a register-offset index operand. Its register view follows the
;;; extend modifier -- uxtw/sxtw take a 32-bit W index; a bare register,
;;; lsl, or sxtx take a 64-bit X index -- so the width can't come from the
;;; spec (which only carries the scale, e.g. :regoff3).
(defun arm642-vinsn-index-operand (desc vp)
  (multiple-value-bind (number modifier amount)
      (ecase (car desc)
        (:opnd (values (svref vp (cadr desc)) nil 0))
        (:reg  (values (cadr desc) nil 0))
        (:shifted-reg
         (destructuring-bind (reg-desc modifier amount) (cdr desc)
           (values (ecase (car reg-desc)
                     (:opnd (svref vp (cadr reg-desc)))
                     (:reg  (cadr reg-desc)))
                   modifier amount))))
    (arm64::make-register-operand
     :register (arm64::gpr-ref number (if (member modifier '(:uxtw :sxtw)) 32 64))
     :modifier modifier :amount amount)))

;;; Build a memory-operand. DESC is (:mem marker base-desc off-desc); SPEC
;;; is (:mem-FORM (:base class) ...). The base view comes from (:base
;;; class). An immediate offset is range-checked against (:imm class); a
;;; register index (the :mem-regoff form) is built by its extend/scale.
(defun arm642-vinsn-memory-operand (desc spec vp)
  (destructuring-bind (marker base-desc off-desc) (cdr desc)
    (arm64::make-memory-operand
     :base (arm642-vinsn-register-operand base-desc (assoc :base (cdr spec)) vp)
     :offset (when off-desc
               (if (eq (car spec) :mem-regoff)
                 (arm642-vinsn-index-operand off-desc vp)
                 (arm642-vinsn-immediate-operand
                  off-desc (cadr (assoc :imm (cdr spec))) vp)))
     :pre-indexed (eq marker :@!)
     :post-indexed (eq marker :@+))))

;;; Build a condition-operand. DESC is (:cond value) for a literal
;;; condition, or (:cond-opnd vp-index [:invert]) for a parameter whose
;;; 4-bit condition value is supplied at expand time. A literal (:~ cc)
;;; is already inverted at definition time, so only the parameter form
;;; carries :invert, meaning "XOR 1 the value read from vp" (used by the
;;; cbranch-false vinsn). (A :cond-inv spec, as on the cset/cinc aliases,
;;; is instead inverted later by ENCODE-CONDITION-OPERAND.)
(defun arm642-vinsn-condition-operand (desc vp)
  (let ((value (ecase (car desc)
                 (:cond (cadr desc))
                 (:cond-opnd (svref vp (cadr desc))))))
    (when (and (eq (car desc) :cond-opnd) (eq (caddr desc) :invert))
      (unless (< value 14)
        (error "condition value ~s has no inverse" value))
      (setq value (logxor value 1)))
    (arm64::make-condition-operand
     :name (arm64::lookup-arm64-condition-value value) :value value)))

(defun arm642-vinsn-operand (desc spec vp unique-labels)
  (cond
    ((arm64::label-spec-p spec)
     (arm64::make-label-operand
      :name (ecase (car desc)
              (:opnd (svref vp (cadr desc)))           ;backend (vinsn) label
              (:local-label (cdr (assq (cadr desc) unique-labels))))))
    ((arm64::mem-spec-p spec)
     (arm642-vinsn-memory-operand desc spec vp))
    ((member spec '(:cond :cond-inv :cond-b))
     (arm642-vinsn-condition-operand desc vp))
 ;; A bare-keyword spec that isn't a condition is an immediate class.
    ((keywordp spec)
     (arm642-vinsn-immediate-operand desc spec vp))
    (t
     (arm642-vinsn-register-operand desc spec vp))))

;;; Expand one instruction of a vinsn template's body into a machine
;;; instruction and emit it into the section. FORM is one simplified
;;; body element -- (template-index . operand-descriptors) -- as produced
;;; at definition time by VINSN-SIMPLIFY-INSTRUCTION. VP is the
;;; variable-parts vector of the vinsn (instance) being expanded, with
;;; lregs already replaced by physical register numbers. UNIQUE-LABELS
;;; maps template-local label keywords to this expansion's label objects.
;;; We fill the operand holes, build the operand structs, encode, and
;;; append the resulting machine instruction.
(defun arm642-emit-instruction-from-vinsn (form vp current unique-labels)
  (let* ((template (svref arm64::*instruction-templates* (car form)))
         (specs (arm64::instruction-template-operand-specs template))
         (insn (arm64::make-instruction form)))
    (setf (arm64::instruction-template insn) template
          (arm64::instruction-parsed-operands insn)
          (mapcar #'(lambda (desc spec)
                      (arm642-vinsn-operand desc spec vp unique-labels))
                  (cdr form) specs))
    (arm64::encode-operands insn)
    (arm64::emit-element current insn)))

;;; His arm642-expand-vinsn (arm642.lisp:1705-1759) verbatim, plus the
;;; two blocks marked ">>> NOTE FIX".
(defun arm642-expand-vinsn (vinsn current)
  (let* ((template (vinsn-template vinsn))
         (vp (vinsn-variable-parts vinsn))
         (nvp (vinsn-template-nvp template))
         (notes (vinsn-notes vinsn))     ; >>> NOTE FIX
         (unique-labels '()))
    (declare (fixnum nvp))
 ;; Replace lregs in the variable-parts vector with their assigned
 ;; physical register numbers.
    (dotimes (i nvp)
      (let ((val (svref vp i)))
        (when (typep val 'lreg)
          (setf (svref vp i) (lreg-value val)))))
 ;; Give each template-local label a fresh object for this expansion,
 ;; so that repeated uses of the same vinsn don't collide in the
 ;; section's label namespace.
    (dolist (name (vinsn-template-local-labels template))
      (push (cons name (cons name nil)) unique-labels))
    (labels ((pred-operand (vf)
 ;; A predicate operand in %DEFINE-ARM64-VINSN's simplified
 ;; form: (index) is a parameter hole, (fn args...) an
 ;; (:apply), and an atom a constant.
               (cond ((atom vf) vf)
                     ((and (null (cdr vf)) (typep (car vf) 'fixnum))
                      (svref vp (car vf)))
                     (t (apply (car vf) (mapcar #'pred-operand (cdr vf))))))
             (eval-predicate (f)
               (ecase (car f)
                 (:pred (apply (cadr f) (mapcar #'pred-operand (cddr f))))
                 (:not  (not  (eval-predicate (cadr f))))
                 (:or   (some  #'eval-predicate (cadr f)))
                 (:and  (every #'eval-predicate (cadr f)))))
             (expand-form (form)
               (cond
                 ((keywordp form)
 ;; A template-local label definition point (e.g. :ok).
                  (arm64::emit-label current (cdr (assq form unique-labels))))
                 ((and (consp form) (typep (car form) 'fixnum))
 ;; A simplified instruction: (template-index . descriptors).
                  (arm642-emit-instruction-from-vinsn form vp current
                                                      unique-labels))
                 ((and (consp form) (consp (car form)))
 ;; A predicate group: ((:pred ...) subform...). Expand the
 ;; body only when the predicate holds at this expansion.
                  (when (eval-predicate (car form))
                    (dolist (sub (cdr form)) (expand-form sub))))
                 (t
 ;; Nothing else is expected. arm64 code vectors are
 ;; purely instructions (plus the leading udf #0 sentinel),
 ;; so there are no :code/:data/:word pseudo-ops: constants
 ;; live in the function's constants vector, reached
 ;; fn-relative. A form landing here is a bug.
                  (format t "~&; arm642-expand-vinsn: unhandled form ~s"
                          form)))))
 ;; >>> NOTE FIX (from arm2-expand-vinsn, arm2.lisp:5663-5673):
 ;; open notes get a label at the carrier vinsn's first instruction.
 ;; One shared zero-size label per group; the note object is the
 ;; label's (unique) name, as arm642-expand-vinsns:1508 does for
 ;; vinsn-labels. ARM64::FINALIZE assigns its address before
 ;; arm642-digest-symbols / -generate-pc-source-map read it.
      (when notes
        (let* ((lab nil))
          (dolist (note notes)
            (unless (eq :close (vinsn-note-class note))
              (when (eq :source-location-begin (vinsn-note-class note))
                (push note *arm642-emitted-source-notes*))
              (when (null lab)
                (setq lab (arm64::emit-label current note)))
              (setf (vinsn-note-address note) lab)))))
      (dolist (form (vinsn-template-body template))
        (expand-form form))
 ;; >>> NOTE FIX (from arm2-expand-vinsn, arm2.lisp:5676-5683):
 ;; :close notes get a label AFTER the carrier's last instruction
 ;; (a zero-size label inherits the address of whatever follows).
      (when notes
        (let* ((lab nil))
          (dolist (note notes)
            (when (eq :close (vinsn-note-class note))
              (when (null lab)
                (setq lab (arm64::emit-label current note)))
              (setf (vinsn-note-address note) lab))))))))

(defun arm642-builtin-index-subprim (idx)
  (let* ((arch (backend-target-arch *target-backend*))
         (table (arch::target-primitive->subprims arch))
         (shift (arch::target-subprims-shift arch)))
    (dolist (cell table)
      (destructuring-bind ((low . high) . base) cell
        (if (and (>= idx low)
                 (< idx high))
          (return (+ base (ash (- idx low) shift))))))))

(defun arm642-fixed-call-builtin (seg vreg xfer name)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((index (arch::builtin-function-name-offset name))
           (subprim (if index
                      (arm642-builtin-index-subprim index)
                      (or (arm64::arm64-subprimitive-offset name)
                          (compiler-bug "Unknown builtin subprim index for ~s" name))))
           (tail-p (arm642-tailcallok xfer)))
      (when tail-p
        (arm642-restore-nvrs seg nil)
        (arm642-restore-non-volatile-fprs seg)
        (! restore-nfp)
        (arm642-restore-full-lisp-context seg))
      (if tail-p
        (! jump-subprim subprim)
        (progn
          (! call-subprim subprim)
          (<- ($ arm64::arg_z))
          (^))))))

(defun arm642-unary-builtin (seg vreg xfer name form)
  (with-arm64-local-vinsn-macros (seg)
    (arm642-one-targeted-reg-form seg form ($ arm64::arg_z))
    (arm642-fixed-call-builtin seg vreg xfer name)))

(defun arm642-binary-builtin (seg vreg xfer name form1 form2)
  (with-arm64-local-vinsn-macros (seg)
    (arm642-two-targeted-reg-forms seg form1 ($ arm64::arg_y)
                                   form2 ($ arm64::arg_z))
    (arm642-fixed-call-builtin seg vreg xfer name)))

(defun arm642-ternary-builtin (seg vreg xfer name form1 form2 form3)
  (with-arm64-local-vinsn-macros (seg)
    (arm642-three-targeted-reg-forms seg form1 ($ arm64::arg_x)
                                     form2 ($ arm64::arg_y)
                                     form3 ($ arm64::arg_z))
    (arm642-fixed-call-builtin seg vreg xfer name)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro defarm642 (name locative arglist &body forms)
    (multiple-value-bind (body decls)
        (parse-body forms nil t)
      (destructuring-bind (vcode-block dest control &rest other-args) arglist
        (let* ((fun `(nfunction ,name
                                (lambda (,vcode-block ,dest ,control
                                         ,@other-args)
                                  ,@decls
                                  (block ,name
                                    (with-arm64-local-vinsn-macros
                                        (,vcode-block ,dest ,control)
                                      ,@body))))))
          `(progn
             (record-source-file ',name 'function)
             (svset *arm642-specials*
                    (%ilogand #.operator-id-mask (%nx1-operator ,locative))
                    ,fun)))))))



;;; PROPOSED FIX (redefinition of HIS arm642.lisp:1758 arm642-lambda,
;;; VERBATIM except one line): arm2 emits (! save-nfp) at the
;;; "Caller's context is saved" convergence point (arm2.lisp:5933);
;;; his copy omits it. The vinsn self-gates on max-nfp-depth>0 at
;;; expansion time, so functions without unboxed spills are unchanged.
;;; %current-frame-ptr (arm2.lisp:9572) also restores nfp on its tail
;;; leg â€” that handler is not drafted yet; carry the note to its wave.
(defarm642 arm642-lambda lambda-list (seg vreg xfer req opt rest keys auxen
                                          body p2decls &optional code-note)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((stack-consed-rest nil)
           (lexprp (if (consp rest) (progn (setq rest (car rest)) t)))
           (rest-var-bits (and rest (nx-var-bits rest)))
           (rest-ignored-p (and rest (not lexprp)
                                (%ilogbitp $vbitignore rest-var-bits)))
           (want-stack-consed-rest (or rest-ignored-p
                                       (and rest
                                            (not lexprp)
                                            (%ilogbitp $vbitdynamicextent
                                                       rest-var-bits))))
           (afunc *arm642-cur-afunc*)
           (inherited-vars (afunc-inherited-vars afunc))
           (fbits (afunc-bits afunc))
           (methodp (%ilogbitp $fbitmethodp fbits))
           (method-var (if methodp (pop req)))
           (next-method-p (%ilogbitp $fbitnextmethp fbits))
           (allow-other-keys-p (%car keys))
           (hardopt (arm642-hard-opt-p opt))
           (lap-p (when (and (consp (%car req)) (eq (%caar req) '&lap))
                    (prog1 (%cdar req) (setq req nil))))
           (num-inh (length inherited-vars))
           (num-req (length req))
           (num-opt (length (%car opt)))
           (arg-regs nil)
           optsupvloc
           reglocatives
           pregs
           no-regs
           (nsaved-fprs 0)
           (*arm642-vstack* 0)
           (*arm642-nfp-depth* *arm642-nfp-depth*)
           (*arm642-nfp-vars* *arm642-nfp-vars*))
      (declare (type (unsigned-byte 16) num-req num-opt num-inh))
      (with-arm64-p2-declarations p2decls
        (setq *arm642-inhibit-register-allocation*
              (setq no-regs (%ilogbitp $fbitnoregs fbits)))
        (multiple-value-setq (pregs reglocatives)

          (nx2-afunc-allocate-global-registers afunc (unless no-regs *arm642-nvrs*)))
        (@ (backend-get-next-label))    ; generic self-reference label, should be label #1
        (when keys;; Ensure keyvect is the first immediate
          (backend-immediate-index (%cadr (%cdddr keys))))
        (when code-note
          (arm642-code-coverage-entry seg code-note))
        (unless next-method-p
          (setq method-var nil))

        (let* ((rev-req (reverse req))
               (rev-fixed (if inherited-vars (reverse (append inherited-vars req)) rev-req))
               (num-fixed (length rev-fixed))
               (rev-opt (reverse (car opt))))
          (if (not (or opt rest keys))
            (progn
              (setq arg-regs (arm642-req-nargs-entry seg rev-fixed)))
 ;; FLET.20: simple-opt-entry and the default-N-args
 ;; vinsns it emits compare the BOXED count against an imm12
 ;; :aimm, so a boxed max past 4095 must take the general
 ;; path below, whose checks have -large fallbacks.
            (if (and (not (or hardopt rest keys))
                     (<= num-opt $numarm64argregs)
                     (arm642-aimm-p (ash (+ num-fixed num-opt)
                                         arm64::fixnumshift)))
              (setq arg-regs (arm642-simple-opt-entry seg rev-opt rev-fixed))
              (progn
 ;; If the minumum acceptable number of args is
 ;; non-zero, ensure that at least that many were
 ;; received. If there's an upper bound, enforce it.

 ;; : aimm-p must see the BOXED count -- the vinsns
 ;; compare (ash n 3), and ARM32's raw-value test was
 ;; sound only because its rotated-immediate encoder
 ;; makes "raw encodable => raw<<2 encodable" true.
                (when rev-fixed
                  (if (arm642-aimm-p (ash num-fixed arm64::fixnumshift))
                    (! check-min-nargs num-fixed)
                    (! check-min-nargs-large num-fixed)))
                (unless (or rest keys)
                  (let* ((max (+ num-fixed num-opt)))
                    (if (arm642-aimm-p (ash max arm64::fixnumshift))
                      (! check-max-nargs max)
                      (! check-max-nargs-large max))))
                (unless lexprp
                  (! save-lisp-context-variable))
 ;; If there were &optional args, initialize their values
 ;; to NIL. All of the argregs get vpushed as a result of this.
                (when opt
                  (! default-optionals (+ num-fixed num-opt)))
                (when keys
                  (unless opt
                    (! vpush-argregs num-fixed))
                  (let* ((keyvect (%car (%cdr (%cdr (%cdr (%cdr keys))))))
                         (flags (the fixnum (logior (the fixnum (if rest 4 0))
                                                    (the fixnum (if (or methodp allow-other-keys-p) 1 0)))))
                         (nkeys (length keyvect))
                         (nprev (+ num-fixed num-opt))
                         (kvidx (backend-immediate-index keyvect)))
                    (declare (fixnum flags nkeys nprev))
 ;; PPC2:5317-5319 contract (ppc-spentry keyword_bind):
 ;; keyword vector -> temp3/fname, flags -> imm2,
 ;; keycount -> imm3, nprev -> imm0. (: this
 ;; block loaded flags into ARG_Y and never seeded
 ;; temp3/imm3 at all -- keyword_bind scanned code
 ;; bytes as its keyword vector.) fn already holds
 ;; the function (save-lisp-context-variable's
 ;; trailing mov fn,nfn), so HIS ref-constant reach
 ;; idiom (arm642.lisp:840-848) applies directly.
                    (if (<= kvidx 31)
                      (! ref-constant ($ arm64::fname) kvidx)
                      (with-imm-target () (idxreg :s64)
                        (arm642-lri seg idxreg
                                    (+ arm64::function.constants (ash kvidx 3)))
                        (! ref-indexed-constant ($ arm64::fname) idxreg)))
                    (arm642-lri seg arm64::imm2
                                (ash flags *arm642-target-fixnum-shift*))
                    (arm642-lri seg arm64::imm3
                                (ash nkeys *arm642-target-fixnum-shift*))
                    (arm642-lri seg arm64::imm0
                                (ash nprev *arm642-target-fixnum-shift*))
                    (! keyword-bind)))
                (when rest
 ;; If any keyword-binding's happened, the key/value
 ;; pairs have been slid to the top-of-stack for us.
 ;; There'll be an even number of them (nargs - the
 ;; "previous" (required/&optional) count.)
                  (if lexprp
                    (arm642-lexpr-entry seg num-fixed)
                    (progn
                      (if want-stack-consed-rest
                        (setq stack-consed-rest t))
                      (let* ((nprev (+ num-fixed num-opt))
                             (simple (and (not keys) (= 0 nprev))))
                        (declare (fixnum nprev))
                        (unless simple
                          (arm642-lri seg arm64::imm0 (ash nprev *arm642-target-fixnum-shift*)))
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
                              (! heap-cons-rest-arg))))))))
                (when hardopt
                  (arm642-lri seg arm64::imm0 (ash num-opt *arm642-target-fixnum-shift*))

 ;; .SPopt-supplied-p wants nargs to contain the
 ;; actual arg-count minus the number of "fixed"
 ;; (required, inherited) args.

                  (unless (= 0 num-fixed)
                    (! scale-nargs num-fixed))
                  (! opt-supplied-p))
                (let* ((nwords-vpushed (+ num-fixed
                                          num-opt
                                          (if hardopt num-opt 0)
                                          (if lexprp 0 (if rest 1 0))
                                          (ash (length (%cadr keys)) 1)))
                       (nbytes-vpushed (* nwords-vpushed *arm642-target-node-size*)))
                  (declare (fixnum nwords-vpushed nbytes-vpushed))

                  (arm642-set-vstack nbytes-vpushed)
                  (setq optsupvloc (- *arm642-vstack* (* num-opt *arm642-target-node-size*)))))))
 ;; Caller's context is saved; *arm642-vstack* is valid.
 ;; Might still have method-var to worry about.
          (! save-nfp)              ;NFP unit 2026-07-15 (sole change)
          (unless (= 0 pregs)
 ;; Save NVRs; load constants into any that get constants.
            (arm642-save-nvrs seg pregs)


            (dolist (pair reglocatives)
              (declare (cons pair))
              (let* ((constant (car pair))
                     (reg (cdr pair))
                     (temp ($ arm64::temp2)))
                (declare (cons constant))
                (rplacd constant reg)
                (let* ((idx (backend-immediate-index (car constant))))
                  (if (< (+ arm64::misc-data-offset (ash (+ idx 2) 2)) 4096)
                    (! ref-constant temp idx)
                    (with-imm-target () (idxreg :s32)
                      (arm642-lri seg idxreg (+ arm64::misc-data-offset (ash (+ idx 2) 2)))
                      (! ref-indexed-constant temp idxreg))))
                (arm642-copy-register seg reg temp))))
          (when method-var
            (arm642-seq-bind-var seg method-var arm64::next-method-context))
 ;; If any arguments are still in arg_x, arg_y, arg_z, that's
 ;; because they weren't vpushed in a "simple" entry case and
 ;; belong in some NVR. Put them in their NVRs, so that we
 ;; can handle arbitrary expression evaluation (special
 ;; binding, value-cell consing, etc.) without clobbering the
 ;; argument registers.
          (when arg-regs
            (do* ((vars arg-regs (cdr vars))
                  (arg-reg-num arm64::arg_z (1+ arg-reg-num)))
                 ((null vars))
              (declare (list vars) (fixnum arg-reg-num))
              (let* ((var (car vars)))
                (when var
                  (let* ((reg (nx2-assign-register-var var)))
                    (arm642-copy-register seg reg arg-reg-num)
                    (setf (var-ea var) reg))))))
          (setq *arm642-entry-vsp-saved-p* t)
          (when stack-consed-rest
            (arm642-open-undo $undostkblk))
          (setq *arm642-entry-vstack* *arm642-vstack*)
          (arm642-bind-lambda seg req opt rest keys auxen optsupvloc arg-regs lexprp inherited-vars))
        (when method-var (arm642-heap-cons-next-method-var seg method-var))
        (arm642-form seg vreg xfer body)
        (arm642-close-lambda seg req opt rest keys auxen)
        (dolist (v inherited-vars)
          (arm642-close-var seg v))
        (when method-var
          (arm642-close-var seg method-var))
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


(defarm642 arm642-progn progn (seg vreg xfer forms)
  (declare (list forms))
  (if (null forms)
    (arm642-nil seg vreg xfer)
    (loop
      (let* ((form (pop forms)))
        (if forms
          (arm642-form seg nil nil form)
          (return (arm642-form seg vreg xfer form)))))))

(defarm642 arm642-prog1 prog1 (seg vreg xfer forms)
  (if (eq (list-length forms) 1)
    (arm642-use-operator (%nx1-operator values) seg vreg xfer forms)
    (if (null vreg)
      (arm642-use-operator (%nx1-operator progn) seg vreg xfer forms)
      (let* ((*arm642-nfp-depth* *arm642-nfp-depth*)
             (float-p (= (hard-regspec-class vreg) hard-reg-class-fpr))
             (crf-p (= (hard-regspec-class vreg) hard-reg-class-crf))
             (node-p (unless (or float-p crf-p)
                       (= (get-regspec-mode vreg) hard-reg-class-gpr-mode-node)))
             (first (pop forms)))
        (arm642-push-register seg
                              (if (or node-p crf-p)
                                (arm642-one-untargeted-reg-form seg first arm64::arg_z)
                                (arm642-one-targeted-reg-form seg first vreg)))
        (dolist (form forms)
          (arm642-form seg nil nil form))
        (if crf-p
          (progn
            (arm642-vpop-register seg arm64::arg_z)
            (<- arm64::arg_z))
          (arm642-pop-register seg vreg))
        (^)))))

(defarm642 arm642-free-reference free-reference (seg vreg xfer sym)
  (arm642-ref-symbol-value seg vreg xfer sym t))

(defarm642 arm642-special-ref special-ref (seg vreg xfer sym)
  (arm642-ref-symbol-value seg vreg xfer sym t))

(defarm642 arm642-bound-special-ref bound-special-ref (seg vreg xfer sym)
  (arm642-ref-symbol-value seg vreg xfer sym nil))

(defarm642 arm642-%slot-ref %slot-ref (seg vreg xfer instance idx)
  (ensuring-node-target (target (or vreg ($ arm64::arg_z)))
    (multiple-value-bind (v i)
        (arm642-two-untargeted-reg-forms seg instance arm64::arg_y
                                         idx arm64::arg_z)
      (unless *arm642-reckless*
        (! check-misc-bound i v))
      (with-node-temps (v i) (temp)
        (! %slot-ref temp v i)
        (arm642-copy-register seg target temp))))
  (^))

(defarm642 arm642-%svref %svref (seg vreg xfer vector index)
  (arm642-vref seg vreg xfer :simple-vector vector index nil))

(defarm642 arm642-svref svref (seg vreg xfer vector index)
  (arm642-vref seg vreg xfer :simple-vector vector index
               (unless *arm642-reckless*
                 (nx-lookup-target-uvector-subtag :simple-vector))))

(defarm642 arm642-%sbchar %sbchar (seg vreg xfer string index)
  (arm642-vref seg vreg xfer :simple-string string index
               (unless *arm642-reckless*
                 (nx-lookup-target-uvector-subtag :simple-string))))

(defarm642 arm642-%svset %svset (seg vreg xfer vector index value)
  (arm642-vset seg vreg xfer :simple-vector vector index value nil))

(defarm642 arm642-svset svset (seg vreg xfer vector index value)
  (arm642-vset seg vreg xfer :simple-vector vector index value
               (nx-lookup-target-uvector-subtag :simple-vector)))

(defarm642 arm642-typed-form typed-form (seg vreg xfer typespec form &optional check)
  (if check
    (arm642-typechecked-form seg vreg xfer typespec form)
    (arm642-form seg vreg xfer form)))

(defarm642 arm642-type-asserted-form type-asserted-form (seg vreg xfer typespec form &optional check)
  (declare (ignore typespec check))
  (arm642-form seg vreg xfer form))

;;; ---------------------------------------------------------------------------
;;; Typed predicates.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-consp @6105 (LOW-TAG lineage: extract-fulltag +
;;; test against fulltag-cons). TAG-SCHEME REVERSION: v2 @6349 carried an
;;; ARM64-DEVIATION testing tag-cons (its high-tag top-byte extract can
;;; never equal its node-biased fulltag-cons); under Matt's x8664-style low
;;; tags the arm2/PPC64 shape is the correct one again â€” extract-fulltag's
;;; contract is (and src fulltagmask), compared against HIS
;;; arm64::fulltag-cons (#b0011, arm64-arch.lisp @59).
(defarm642 arm642-consp consp (seg vreg xfer cc form)
  (if (null vreg)
    (arm642-form seg vreg xfer form)
    (let* ((tagreg arm64::imm0))
      (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
        (! extract-fulltag tagreg (arm642-one-untargeted-reg-form seg form arm64::arg_z))
        (arm642-test-reg-%izerop seg vreg xfer tagreg cr-bit true-p
                                 arm64::fulltag-cons)))))

(defarm642 arm642-cons cons (seg vreg xfer y z)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil y)
      (arm642-form seg nil xfer z))
    (multiple-value-bind (yreg zreg) (arm642-two-untargeted-reg-forms seg y arm64::arg_y z arm64::arg_z)
      (ensuring-node-target (target vreg)
        (! cons target yreg zreg))
      (^))))

(defarm642 arm642-%rplaca %rplaca (seg vreg xfer ptr val)
  (arm642-modify-cons seg vreg xfer ptr val nil nil t))

(defarm642 arm642-%rplacd %rplacd (seg vreg xfer ptr val)
  (arm642-modify-cons seg vreg xfer ptr val nil t t))

(defarm642 arm642-rplaca rplaca (seg vreg xfer ptr val)
  (arm642-modify-cons seg vreg xfer ptr val t nil t))

(defarm642 arm642-set-car set-car (seg vreg xfer ptr val)
  (arm642-modify-cons seg vreg xfer ptr val t nil nil))

(defarm642 arm642-rplacd rplacd (seg vreg xfer ptr val)
  (arm642-modify-cons seg vreg xfer ptr val t t t))

(defarm642 arm642-set-cdr set-cdr (seg vreg xfer ptr val)
  (arm642-modify-cons seg vreg xfer ptr val t t nil))

(defarm642 arm642-%car %car (seg vreg xfer form)
  (arm642-reference-list seg vreg xfer form nil nil))

(defarm642 arm642-%cdr %cdr (seg vreg xfer form)
  (arm642-reference-list seg vreg xfer form nil t))

(defarm642 arm642-car car (seg vreg xfer form)
  (arm642-reference-list seg vreg xfer form t nil))

(defarm642 arm642-cdr cdr (seg vreg xfer form)
  (arm642-reference-list seg vreg xfer form t t))

;;; ---------------------------------------------------------------------------
;;; vector â€” w5's %gvector precedent: PPC64 always calls .SPgvector; the
;;; boxed subtag is the LAST vpush (gvector pops it first). Donor shape =
;;; ppc2-%gvector's non-constant leg with the subtag known (simple-vector).
(defarm642 arm642-vector vector (seg vreg xfer arglist)
  (if (null vreg)
    (dolist (form arglist)
      (arm642-form seg nil nil form))
    (progn
      (let* ((*arm642-vstack* *arm642-vstack*)
             (subtag (nx-lookup-target-uvector-subtag :simple-vector)))
 ;; .SPgvector reads the boxed subtag from the DEEPEST vpushed
 ;; word ([vsp+nargs-8], spentry-B; same protocol as PPC64/x8664
 ;; _SPgvector) â€” so the subtag form goes FIRST, elements after.
 ;; ROOT: appending it LAST made the subprim build the
 ;; header from element 0 (a pointer) and store the subtag as
 ;; the final element â€” every (vector ...) object had a garbage
 ;; header that blew up the FIRST GC's rmark with a ~51GB
 ;; suffix_dnodes memset (hw-watchpoint-observed at _SPgvector's
 ;; header store, boot ).
        (arm642-set-nargs seg
                          (arm642-formlist
                           seg
                           (cons (make-acode (%nx1-operator fixnum)
                                             subtag)
                                 arglist)
                           nil))
        (! gvector))
      (<- arm64::arg_z)))
  (^))

;;; Donor: PPC64 ppc2.lisp ppc2-%gvector (LINE-PORT; chosen over v2's
;;; allocate-initialized-gvector open-code split — PPC64 always calls
;;; .SPgvector; simplest correct form, and the subprim body is already
;;; verified in the draft lane). All-on-stack = initvals + boxed
;;; subtag, nargs = count, result in arg_z.
(defarm642 arm642-%gvector %gvector (seg vreg xfer arglist)
  (let* ((all-on-stack (append (car arglist) (reverse (cadr arglist)))))
    (if (null vreg)
      (dolist (form all-on-stack (^)) (arm642-form seg nil nil form))
      (progn
        (let* ((*arm642-vstack* *arm642-vstack*))
          (arm642-set-nargs seg (arm642-formlist seg all-on-stack nil))
          (! gvector))
        (<- arm64::arg_z)
        (^)))))
(defarm642 arm642-%char-code %char-code (seg vreg xfer c)
  (arm642-extract-charcode seg vreg xfer c nil))

(defarm642 arm642-char-code char-code (seg vreg xfer c)
  (arm642-extract-charcode seg vreg xfer c
                           (not (arm642-form-typep c 'character))))

(defarm642 arm642-%ilognot %ilognot (seg vreg xfer form)
  (ensuring-node-target (target vreg)
    (! %ilognot target (arm642-one-untargeted-reg-form seg form target)))
  (^))

(defarm642 arm642-%ilogior2 %ilogior2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (arm642-use-operator (%nx1-operator fixnum) seg vreg xfer
                           (logior fix1 fix2))
      (let* ((fixval (or fix1 fix2))
             (fiximm (if fixval (ash fixval *arm642-target-fixnum-shift*)))
             (ok-imm (and fiximm (arm64::encode-logical-immediate fiximm)))
             (otherform (if ok-imm (if fix1 form2 form1))))
        (if otherform
          (let* ((other-reg (arm642-one-untargeted-reg-form seg otherform
                                                            arm64::arg_z)))
            (when vreg
              (ensuring-node-target (target vreg)
                (! logior-imm target other-reg fiximm))))
          (multiple-value-bind (r1 r2)
              (arm642-two-untargeted-reg-forms seg form1 arm64::arg_y
                                               form2 arm64::arg_z)
            (if vreg
              (ensuring-node-target (target vreg) (! %logior2 target r1 r2)))))
        (^)))))

(defarm642 arm642-%ilogand2 %ilogand2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (arm642-use-operator (%nx1-operator fixnum) seg vreg xfer
                           (logand fix1 fix2))
      (let* ((fixval (or fix1 fix2))
             (fiximm (if fixval (ash fixval *arm642-target-fixnum-shift*)))
             (ok-imm (and fiximm (arm64::encode-logical-immediate fiximm)))
             (otherform (if ok-imm (if fix1 form2 form1))))
        (if otherform
          (let* ((other-reg (arm642-one-untargeted-reg-form seg otherform
                                                            arm64::arg_z)))
            (when vreg
              (ensuring-node-target (target vreg)
                (! logand-imm target other-reg fiximm))))
          (multiple-value-bind (r1 r2)
              (arm642-two-untargeted-reg-forms seg form1 arm64::arg_y
                                               form2 arm64::arg_z)
            (if vreg
              (ensuring-node-target (target vreg) (! %logand2 target r1 r2)))))
        (^)))))

(defarm642 arm642-%ilogxor2 %ilogxor2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (arm642-use-operator (%nx1-operator fixnum) seg vreg xfer
                           (logxor fix1 fix2))
      (let* ((fixval (or fix1 fix2))
             (fiximm (if fixval (ash fixval *arm642-target-fixnum-shift*)))
             (ok-imm (and fiximm (arm64::encode-logical-immediate fiximm)))
             (otherform (if ok-imm (if fix1 form2 form1))))
        (if otherform
          (let* ((other-reg (arm642-one-untargeted-reg-form seg otherform
                                                            arm64::arg_z)))
            (when vreg
              (ensuring-node-target (target vreg)
                (! logxor-imm target other-reg fiximm))))
          (multiple-value-bind (r1 r2)
              (arm642-two-untargeted-reg-forms seg form1 arm64::arg_y
                                               form2 arm64::arg_z)
            (if vreg
              (ensuring-node-target (target vreg) (! %logxor2 target r1 r2)))))
        (^)))))


(defarm642 arm642-lexical-reference lexical-reference (seg vreg xfer varnode)
  (let* ((ea-or-form (var-ea varnode)))
    (if (and (acode-punted-var-p varnode) (not (fixnump ea-or-form)))
      (arm642-form seg vreg xfer ea-or-form)
      (progn
        (unless (or (typep ea-or-form 'lreg) (fixnump ea-or-form))
          (compiler-bug "bogus ref to var ~s (~s) : ~s " varnode (var-name varnode) ea-or-form))
        (arm642-do-lexical-reference seg vreg ea-or-form)
        (^)))))

;;; ---------------------------------------------------------------------------
;;; The `fixnum' handler. Required by this wave: every constant-folding
;;; handler above reaches (arm642-use-operator (%nx1-operator fixnum) ...),
;;; which dispatches through *arm642-specials* â€” unregistered, (logand 3 5)
;;; would be a compiler bug.
;;; ---------------------------------------------------------------------------

;;; Donor: v2 arm64-p2-fixnum @6909 (simple absolute-natural form; arm2's
;;; @6595 adds regmap constant-reuse via arm2-gprs-containing-constant,
;;; machinery his backend doesn't define â€” see U9w).
(defarm642 arm642-fixnum fixnum (seg vreg xfer value)
  (if (null vreg)
    (^)
    (let* ((class (hard-regspec-class vreg))
           (mode (get-regspec-mode vreg))
           (unboxed (if (= class hard-reg-class-gpr)
                      (not (or (= hard-reg-class-gpr-mode-node mode)
                               (= hard-reg-class-gpr-mode-address mode))))))
      (if unboxed
        (arm642-absolute-natural seg vreg xfer value)
        (if (= class hard-reg-class-crf)
          (arm642-branch seg (arm642-cd-true xfer) nil)
          (progn
            (ensuring-node-target (target vreg)
              (arm642-absolute-natural seg target nil (ash value *arm642-target-fixnum-shift*)))
            (^)))))))
(defun arm642-branch-unless-arg-fixnum (seg reg label)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((flags (make-hard-crf-reg 0)))
      (! test-fixnum flags reg)
      (! cbranch-false label flags arm64::cond-eq))))

(defun arm642-branch-unless-both-args-fixnums (seg x y label)
  (with-arm64-local-vinsn-macros (seg)
    (let* ((flags (make-hard-crf-reg 0)))
      (! test-fixnums flags x y)
      (! cbranch-false label flags arm64::cond-eq))))

(defun arm642-check-fixnum-overflow (seg crf target &optional labelno)
  (with-arm64-local-vinsn-macros  (seg)
    (let* ((no-overflow (backend-get-next-label))
           (label (if labelno (aref *backend-labels* labelno))))
      (! cbranch-false (or label (aref *backend-labels* no-overflow))
         crf arm64::cond-vs)
      (if *arm642-open-code-inline*
        (! handle-fixnum-overflow-inline target target)
        (let* ((target-other (not (eql (hard-regspec-value target)
                                       arm64::arg_z)))
               (arg (if target-other
                      (make-wired-lreg arm64::arg_z)
                      target))
               (result (make-wired-lreg arm64::arg_z)))
          (when target-other
            (arm642-copy-register seg arg target))
          (! call-subprim-1 result (subprim-name->offset '.SPfix-overflow) arg)
          (when target-other
            (arm642-copy-register seg target result))))
      (when labelno (-> labelno))
      (@ no-overflow))))

(defun arm642-inline-add2 (seg vreg xfer form1 form2)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (arm642-two-targeted-reg-forms seg
                                   form1 ($ arm64::arg_y)
                                   form2 ($ arm64::arg_z))
    (let* ((out-of-line (backend-get-next-label))
           (done (backend-get-next-label)))
      (ensuring-node-target (target vreg)
        (if (acode-fixnum-form-p form1)
          (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_z)
                                           (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_y)
                                               (aref *backend-labels*
                                                     out-of-line))
            (arm642-branch-unless-both-args-fixnums
             seg ($ arm64::arg_y) ($ arm64::arg_z) (aref *backend-labels*
                                                         out-of-line))))
        (let* ((flags (make-hard-crf-reg 0)))
          (! fixnum-add-set-flags ($ arm64::arg_z) flags ($ arm64::arg_y)
             ($ arm64::arg_z))
          (arm642-check-fixnum-overflow seg flags ($ arm64::arg_z) done))
        (@ out-of-line)
        (! call-subprim-2 ($ arm64::arg_z)
           (subprim-name->offset '.SPbuiltin-plus) ($ arm64::arg_y)
           ($ arm64::arg_z))
        (@ done)
        (arm642-copy-register seg target ($ arm64::arg_z)))
      (^))))

;;; Return T if form is declared to be something that couldn't be a fixnum.
(defun arm642-explicit-non-fixnum-type-p (form)
  (let* ((type (arm642-form-type form))
         (target-fixnum-type (nx-target-type 'fixnum)))
    (and (not (subtypep type target-fixnum-type))
         (not (subtypep target-fixnum-type type)))))

(defarm642 arm642-add2 add2 (seg vreg xfer form1 form2)
  (if (or (arm642-explicit-non-fixnum-type-p form1)
          (arm642-explicit-non-fixnum-type-p form2))
    (arm642-binary-builtin seg vreg xfer '+-2 form1 form2)
    (arm642-inline-add2 seg vreg xfer form1 form2)))

;;; Make a gcable macptr.
(defarm642 arm642-%new-ptr %new-ptr (seg vreg xfer size clear-p )
  (arm642-call-fn seg vreg xfer
                  (make-acode (%nx1-operator immediate) '%new-gcable-ptr)
                  (list nil (list clear-p size))
                  nil))

(defarm642 arm642-ash ash (seg vreg xfer num amt)
  (arm642-two-targeted-reg-forms seg num ($ arm64::arg_y) amt ($ arm64::arg_z))
  (arm642-fixed-call-builtin seg vreg xfer '.SPbuiltin-ash))

(defarm642 arm642-fixnum-ash fixnum-ash (seg vreg xfer num amt)
  (multiple-value-bind (rnum ramt)
      (arm642-two-untargeted-reg-forms seg num ($ arm64::arg_y)
                                       amt ($ arm64::arg_z))
    (let* ((amttype (specifier-type (acode-form-type
                                     amt *arm642-trust-declarations*))))
      (ensuring-node-target (target vreg)
        (if (and (typep amttype 'numeric-ctype)
                 (>= (numeric-ctype-low amttype) 0))
          (! fixnum-ash-left target rnum ramt)
          (! fixnum-ash target rnum ramt)))
      (^))))

(defarm642 arm642-fixnum-ref-double-float %fixnum-ref-double-float
    (seg vreg xfer base index)
  (if (null vreg)
    (progn
      (arm642-form base seg nil nil)
      (arm642-form index seg nil xfer))
    (let* ((fix (acode-fixnum-form-p index)))
      (unless (typep fix '(unsigned-byte 12))
        (setq fix nil))
      (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
               (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
        (cond (fix
               (! fixnum-ref-c-double-float vreg
                  (arm642-one-untargeted-reg-form seg base arm64::arg_z) fix))
              (t
               (multiple-value-bind (rbase rindex)
                   (arm642-two-untargeted-reg-forms seg base arm64::arg_y
                                                    index arm64::arg_z)
                 (! fixnum-ref-double-float vreg rbase rindex))))
        (with-fp-target () (target :double-float)
          (cond (fix
                 (! fixnum-ref-c-double-float target
                    (arm642-one-untargeted-reg-form seg base arm64::arg_z) fix))
                (t
                 (multiple-value-bind (rbase rindex)
                     (arm642-two-untargeted-reg-forms seg base arm64::arg_y
                                                      index arm64::arg_z)
                   (! fixnum-ref-double-float target rbase rindex))))
          (<- target)))
      (^))))

(defarm642 arm642-fixnum-set-double-float %fixnum-set-double-float
    (seg vreg xfer base index val)
  (let* ((fix (acode-fixnum-form-p index)))
    (unless (typep fix '(unsigned-byte 12))
      (setq fix nil))
    (cond ((or (null vreg)
               (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                    (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double)))
           (let* ((fhint (or vreg ($ arm64::d0 :class :fpr
                                               :mode :double-float))))
             (if fix
               (multiple-value-bind (rbase rval)
                   (arm642-two-untargeted-reg-forms seg base
                                                    ($ arm64::arg_z) val fhint)
                 (! fixnum-set-c-double-float rbase fix rval)
                 (<- rval))
               (multiple-value-bind (rbase rindex rval)
                   (arm642-three-untargeted-reg-forms seg base ($ arm64::arg_y)
                                                      index ($ arm64::arg_z) val
                                                      fhint)
                 (! fixnum-set-double-float rbase rindex rval)
                 (<- rval)))))
          (t
           (if fix
             (multiple-value-bind (rbase rboxed)
                 (arm642-two-untargeted-reg-forms seg base ($ arm64::arg_y) val
                                                  ($ arm64::arg_z))
               (with-fp-target () (rval :double-float)
                 (arm642-copy-register seg rval rboxed)
                 (! fixnum-set-c-double-float rbase fix rval))
               (<- rboxed))
             (multiple-value-bind (rbase rindex rboxed)
                 (arm642-three-untargeted-reg-forms seg base ($ arm64::arg_x)
                                                    index ($ arm64::arg_y)
                                                    val ($ arm64::arg_z))
               (with-fp-target () (rval :double-float)
                 (arm642-copy-register seg rval rboxed)
                 (! fixnum-set-double-float rbase rindex rval))
               (<- rboxed)))))
    (^)))

(defarm642 arm642-t t (seg vreg xfer)
  (arm642-t seg vreg xfer))

(defarm642 arm642-nil nil (seg vreg xfer)
  (arm642-nil seg vreg xfer))

(defarm642 arm642-ivector-typecode-p ivector-typecode-p (seg vreg xfer val)
  (cond ((null vreg) (arm642-form seg vreg xfer val))
        (t (ensuring-node-target (target vreg)
             (! ivector-typecode-p target
                (arm642-one-untargeted-reg-form seg val arm64::arg_z)))
           (^))))

(defarm642 arm642-gvector-typecode-p gvector-typecode-p (seg vreg xfer val)
  (cond ((null vreg) (arm642-form seg vreg xfer val))
        (t (ensuring-node-target (target vreg)
             (! gvector-typecode-p target
                (arm642-one-untargeted-reg-form seg val arm64::arg_z)))
           (^))))

(defarm642 arm642-%complex-single-float-realpart %complex-single-float-realpart
    (seg vreg xfer arg)
  (if (null vreg)
    (arm642-form  seg  nil xfer arg)
    (with-fp-target () (target :single-float)
      (when (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
        (setq target vreg))
      (with-fp-target (target) (val :complex-single-float)
        (! %complex-single-float-realpart target
           (arm642-one-untargeted-reg-form seg arg val))
        (<- target)
        (^ )))))

(defarm642 arm642-%complex-single-float-imagpart %complex-single-float-imagpart
    (seg vreg xfer arg)
  (if (null vreg)
    (arm642-form  seg  nil xfer arg)
    (with-fp-target () (target :single-float)
      (when (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
        (setq target vreg))
      (with-fp-target (target) (val :complex-single-float)
        (! %complex-single-float-imagpart target
           (arm642-one-untargeted-reg-form seg arg val))
        (<- target)
        (^ )))))

(defarm642 arm642-%complex-double-float-realpart %complex-double-float-realpart
    (seg vreg xfer arg)
  (if (null vreg)
    (arm642-form  seg  nil xfer arg)
    (with-fp-target () (target :double-float)
      (when (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
        (setq target vreg))
      (with-fp-target (target) (val :complex-double-float)
        (! %complex-double-float-realpart target
           (arm642-one-untargeted-reg-form seg arg val))
        (<- target)
        (^ )))))

(defarm642 arm642-%complex-double-float-imagpart %complex-double-float-imagpart
    (seg vreg xfer arg)
  (if (null vreg)
    (arm642-form  seg  nil xfer arg)
    (with-fp-target () (target :double-float)
      (when (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
        (setq target vreg))
      (with-fp-target (target) (val :complex-double-float)
        (! %complex-double-float-imagpart target
           (arm642-one-untargeted-reg-form seg arg val))
        (<- target)
        (^ )))))

(defarm642 arm642-%make-complex-single-float %make-complex-single-float
    (seg vreg xfer r i)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil r)
      (arm642-form seg nil xfer i))
    (with-fp-target () (target :complex-single-float)
      (if (and (eql (hard-regspec-class vreg) hard-reg-class-fpr)
               (eql (get-regspec-mode vreg)
                    hard-reg-class-fpr-mode-complex-single-float))
        (setq target vreg))
      (let* ((rreg (make-unwired-lreg target
                                      :mode hard-reg-class-fpr-mode-single)))
        (with-fp-target (rreg) (ireg :single-float)
          (arm642-two-targeted-reg-forms seg r rreg i ireg)
          (! %make-complex-single-float target rreg ireg)))
      (<- target)
      (^))))

(defarm642 arm642-%make-complex-double-float %make-complex-double-float
    (seg vreg xfer r i)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil r)
      (arm642-form seg nil xfer i))
    (with-fp-target () (target :complex-double-float)
      (if (and (eql (hard-regspec-class vreg) hard-reg-class-fpr)
               (eql (get-regspec-mode vreg)
                    hard-reg-class-fpr-mode-complex-double-float))
        (setq target vreg))
      (let* ((rreg (make-unwired-lreg target
                                      :mode hard-reg-class-fpr-mode-double)))
        (with-fp-target (rreg) (ireg :double-float)
          (arm642-two-targeted-reg-forms seg r rreg i ireg)
          (! %make-complex-double-float target rreg ireg)))
      (<- target)
      (^))))

(defarm642 arm642-complex complex (seg vreg xfer r i)
  (arm642-call-fn seg vreg xfer (make-acode (%nx1-operator immediate) 'complex)
                  (list nil (list i r)) nil))

(defarm642 arm642-realpart realpart (seg vreg xfer n)
  (arm642-call-fn seg vreg xfer (make-acode (%nx1-operator immediate) 'realpart)
                  (list nil (list n)) nil))

(defarm642 arm642-imagpart imagpart (seg vreg xfer n)
  (arm642-call-fn seg vreg xfer (make-acode (%nx1-operator immediate) 'imagpart)
                 (list nil (list n)) nil))




;;; C-frame geometry for outgoing foreign calls; see ALLOC-C-FRAME in
;;; arm64-vinsns.lisp. A C frame is a u64-vector wrapping, from low address
;;; to high: the header word, the saved previous SP (element 0, consumed by
;;; DISCARD-C-FRAME), N-C-ARG-WORDS of outgoing stack arguments, and 4 words
;;; reserved for the boundary lisp frame. That frame lands at the HIGH end,
;;; just below the caller's old SP -- i.e. the C frame is stacked on top of
;;; it. Rounded up to an even number of words so SP stays 16-byte aligned.
;;;
;;; The element count deliberately COVERS the reserved frame, so the GC skips
;;; its uninitialized words; the ff-call sequence builds the frame there and
;;; then shrinks the count by 4 to publish it. See ALLOC-C-FRAME.
(defun arm642-c-frame-words (n-c-arg-words)
  (let ((words (+ 1                     ;header
                  1                     ;saved previous SP (element 0)
                  n-c-arg-words         ;outgoing stack arguments
                  4)))                  ;reserved boundary lisp frame
    (logandc2 (1+ words) 1)))           ;round up to even

(defun arm642-c-frame-header (n-c-arg-words)
  (logior (ash (1- (arm642-c-frame-words n-c-arg-words)) arm64::num-subtag-bits)
          arm64::subtag-u64-vector))

;;; ------------------------------------------------------------------
;;; Merged from the linuxarm64 port's compiler overlay. Appended in the
;;; order our build concatenated the fasls, so the definition that wins
;;; is the one that won when the suite was measured.
;;; ------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Acode handler registrations.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-immform @6567. arm642-immediate itself already
;;; exists (arm642.lisp @783); it just was never registered for the
;;; `immediate' operator.
(defarm642 arm642-immform immediate (seg vreg xfer form)
  (arm642-immediate seg vreg xfer form))


;;; Donor: arm2.lisp arm2-simple-function @7029.
(defarm642 arm642-simple-function simple-function (seg vreg xfer afunc)
  (arm642-immediate seg vreg xfer (arm642-afunc-lfun-ref afunc)))


;;; Donor: arm2.lisp arm2-call @6696 (v2 arm64-p2-call @7002 identical).
(defarm642 arm642-call call (seg vreg xfer fn arglist &optional spread-p)
  (when (and (null vreg)
             (acode-p fn)
             (eq (acode-operator fn) (%nx1-operator immediate)))
    (let* ((name (car (acode-operands fn))))
      (when (memq name *warn-if-function-result-ignored*)
        (p2-whine *arm642-cur-afunc* :result-ignored name))))
  (arm642-call-fn seg vreg xfer fn arglist spread-p))

;;; ---------------------------------------------------------------------------
;;; Condition-code plumbing.
;;; ---------------------------------------------------------------------------

;;; Donor: v2 arm64-p2.lisp @451/@454 (arm2 @487/@490 identical); constants
;;; renamed arm64::arm64-cond-* -> arm64::cond-* per his arm64-asm.lisp
;;; (@1380-1419; his arm642-branch-unless-arg-fixnum uses arm64::cond-eq).
(defun acode-condition-to-arm64-cond-bit (cond)
  (condition-to-arm64-cond-bit (car (acode-operands cond))))


(defun condition-to-arm64-cond-bit (cond)
  (case cond
    (:EQ (values arm64::cond-eq t))
    (:NE (values arm64::cond-eq nil))
    (:GT (values arm64::cond-gt t))
    (:LE (values arm64::cond-gt nil))
    (:LT (values arm64::cond-lt t))
    (:GE (values arm64::cond-lt nil))))


;;; Donor: v2 arm64-p2-swap-compare-cond-bit @3764 (NOT arm2's
;;; arm2-cr-bit-for-reversed-comparison table: ARM64 cond bits are the 4-bit
;;; b.cc field, and only gt/lt can reach here â€” eq is order-independent and
;;; le/ge are carried as true-p on gt/lt by condition-to-arm64-cond-bit).
(defun arm642-swap-compare-cond-bit (cr-bit)
  (cond ((eql cr-bit arm64::cond-gt) arm64::cond-lt)
        ((eql cr-bit arm64::cond-lt) arm64::cond-gt)
        (t cr-bit)))




;;; Donor: v2 arm64-add-sub-imm-single-insn-p @3750 â€” value encodable in ONE
;;; ARM64 CMP/ADD/SUB immediate (u12, optionally LSL #12). Gates the
;;; s16-constant compare paths: an s16 like #x3ff8 passes acode-s16-constant-p
;;; but cannot be a single cmp immediate.
(defun arm642-add-sub-imm-single-insn-p (val)
  (and (integerp val)
       (>= val 0)
       (or (<= val 4095)
           (and (<= val (ash 4095 12))
                (zerop (logand val 4095))))))








;;; ---------------------------------------------------------------------------
;;; Comparison handlers.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-%i<> @7639 / v2 arm64-p2-%i<> @7796 (identical).
(defarm642 arm642-%i<> %i<> (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
    (arm642-compare seg vreg xfer form1 form2 cr-bit true-p)))


;;; Donor: arm2.lisp arm2-%izerop @6365 / v2 @6678 (identical).
(defarm642 arm642-%izerop %izerop (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
    (arm642-test-%izerop seg vreg xfer form cr-bit true-p)))


;;; Donor: arm2.lisp arm2-not @7568 / v2 arm64-p2-not @7713 (identical).
(defarm642 arm642-not not (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
    (arm642-compare-register-to-nil
     seg
     vreg
     xfer
     (arm642-one-untargeted-reg-form seg form arm64::arg_z)
     cr-bit
     true-p)))


;;; Donor: arm2.lisp arm2-numcmp @6484 / v2 @6796 (identical dispatch).
(defarm642 arm642-numcmp numcmp (seg vreg xfer cc form1 form2)
  (let* ((name (ecase (car (acode-operands cc))
                 (:eq '=-2)
                 (:ne '/=-2)
                 (:lt '<-2)
                 (:le '<=-2)
                 (:gt '>-2)
                 (:ge '>=-2))))
    (if (or (arm642-explicit-non-fixnum-type-p form1)
            (arm642-explicit-non-fixnum-type-p form2))
      (arm642-binary-builtin seg vreg xfer name form1 form2)
      (arm642-inline-numcmp seg vreg xfer cc name form1 form2))))


;;; Donor: arm2.lisp arm2-inline-numcmp @6510 (flags shape â€” v2's @6809 uses
;;; the bit31 family in the inline leg, not carried; see Group B note).
;;; Fixnum guards + fixnum compare inline, out-of-line call-subprim-2 to the
;;; indexed =-2/<-2/... builtin (his call-subprim-2 vinsn is LIVE; his
;;; inline-add2 @2046 is the call-subprim-2-with-index precedent).
(defun arm642-inline-numcmp (seg vreg xfer cc name form1 form2)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
      (let* ((otherform (and (eql cr-bit arm64::cond-eq)
                             (if (eql (acode-fixnum-form-p form2) 0)
                               form1
                               (if (eql (acode-fixnum-form-p form1) 0)
                                 form2)))))
        (if otherform
          (arm642-one-targeted-reg-form seg otherform ($ arm64::arg_z))
          (arm642-two-targeted-reg-forms seg form1 ($ arm64::arg_y) form2 ($ arm64::arg_z)))
        (let* ((out-of-line (backend-get-next-label))
               (done (backend-get-next-label))
               (continue (backend-get-next-label)))
          (if otherform
            (unless (acode-fixnum-form-p otherform)
              (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_z) (aref *backend-labels* out-of-line)))
            (if (acode-fixnum-form-p form1)
              (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_z) (aref *backend-labels* out-of-line))
              (if (acode-fixnum-form-p form2)
                (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_y) (aref *backend-labels* out-of-line))
                (arm642-branch-unless-both-args-fixnums seg ($ arm64::arg_y) ($ arm64::arg_z) (aref *backend-labels* out-of-line)))))
          (with-crf-target () crf
            (if otherform
              (! compare-signed-s16const crf ($ arm64::arg_z) 0)
              (! compare crf ($ arm64::arg_y) ($ arm64::arg_z)))
            (if (and vreg (eql (hard-regspec-class vreg) hard-reg-class-crf))
              (arm642-branch seg (arm642-cd-merge xfer continue) crf cr-bit true-p)
              (progn
                (! cond->boolean ($ arm64::arg_z) (if true-p cr-bit (logxor cr-bit 1)))
                (-> done))))
          (@ out-of-line)
          (if otherform
            (arm642-lri seg ($ arm64::arg_y) 0))
          (let* ((index (arch::builtin-function-name-offset name))
                 (idx-subprim (arm642-builtin-index-subprim index)))
            (! call-subprim-2 ($ arm64::arg_z) idx-subprim ($ arm64::arg_y) ($ arm64::arg_z)))
          (@ done)
          (<- ($ arm64::arg_z))
          (^)
          (@ continue))))))


;;; ---------------------------------------------------------------------------
;;; Logical operations.
;;; ---------------------------------------------------------------------------

;;; RETIRED 2026-07-25 (): arm642-%ilognot / -%ilogior2 / -%ilogand2 /
;;; -%ilogxor2 all lived here and SHADOWED his tip's own definitions (our
;;; overlay loads after arm642.lisp). Ours passed the shifted immediate to
;;; logior-imm/logand-imm/logxor-imm as its u64 two's-complement, on the
;;; assumption that the operand was :u64const. It is :s64const
;;; (arm64-vinsns.lisp:554), so any negative mask died with
;;; 18446744071562067968 : value doesn't match constraint :S64CONST
;;; — the l0-hash wall. His versions pass the value signed and are correct;
;;; deleting ours is the fix. NOTE for upstream: our -%ilogand2 also had a
;;; clear-left/clear-right ubfx path for contiguous low-run masks that his
;;; does not — a perf win to offer him as a patch, not to re-shadow here.

;;; Donor: v2 arm64-p2-inline-logior2 @7340 (fixnum-guarded inline ORR with
;;; out-of-line .SPbuiltin-logior; bitmask strategy as in %ilogior2). The
;;; builtin subprim is reached via the logior-2 builtin INDEX â€” his subprims
;;; table has no named .SPbuiltin-logior (only plus/minus/times/div), but his
;;; :primitive->subprims maps index 16 to the right slot.
(defun arm642-inline-logior2 (seg vreg xfer form1 form2)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2)))
      (if (and fix1 fix2)
        (arm642-use-operator (%nx1-operator fixnum) seg vreg xfer (logior fix1 fix2))
        (let* ((fixval (or fix1 fix2))
               (unboxed-fixval (if fixval (ash fixval *arm642-target-fixnum-shift*)))
               (otherform (if fixval (if fix1 form2 form1)))
               (out-of-line (backend-get-next-label))
               (done (backend-get-next-label)))
          (if otherform
            (arm642-one-targeted-reg-form seg otherform ($ arm64::arg_z))
            (arm642-two-targeted-reg-forms seg form1 ($ arm64::arg_y) form2 ($ arm64::arg_z)))
          (ensuring-node-target (target vreg)
            (if otherform
              (unless (acode-fixnum-form-p otherform)
                (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_z) (aref *backend-labels* out-of-line)))
              (if (acode-fixnum-form-p form1)
                (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_z) (aref *backend-labels* out-of-line))
                (if (acode-fixnum-form-p form2)
                  (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_y) (aref *backend-labels* out-of-line))
                  (arm642-branch-unless-both-args-fixnums seg ($ arm64::arg_y) ($ arm64::arg_z) (aref *backend-labels* out-of-line)))))
            (if otherform
              (if (arm64::encode-logical-immediate unboxed-fixval)
                (! logior-imm ($ arm64::arg_z) ($ arm64::arg_z) unboxed-fixval)
                (progn
                  (arm642-lri seg ($ arm64::arg_y) unboxed-fixval)
                  (! %logior2 ($ arm64::arg_z) ($ arm64::arg_z) ($ arm64::arg_y))))
              (! %logior2 ($ arm64::arg_z) ($ arm64::arg_z) ($ arm64::arg_y)))
            (-> done)
            (@ out-of-line)
            (if otherform
              (arm642-lri seg ($ arm64::arg_y) (ash fixval *arm642-target-fixnum-shift*)))
            (! call-subprim-2 ($ arm64::arg_z)
               (arm642-builtin-index-subprim (arch::builtin-function-name-offset 'logior-2))
               ($ arm64::arg_y) ($ arm64::arg_z))
            (@ done)
            (arm642-copy-register seg target ($ arm64::arg_z)))
          (^))))))


;;; Donor: v2 arm64-p2-inline-logand2 @7394 (same notes as inline-logior2;
;;; keeps the clear-left/clear-right maskable path).
(defun arm642-inline-logand2 (seg vreg xfer form1 form2)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2)))
      (if (and fix1 fix2)
        (arm642-use-operator (%nx1-operator fixnum) seg vreg xfer (logand fix1 fix2))
        (let* ((fixval (or fix1 fix2))
               (fixlen (if fixval (integer-length fixval)))
               (unboxed-fixval (if fixval (ash fixval *arm642-target-fixnum-shift*)))
               (maskable (and fixval (= fixlen (logcount fixval))))
               (otherform (if fixval (if fix1 form2 form1)))
               (out-of-line (backend-get-next-label))
               (done (backend-get-next-label)))
          (if otherform
            (arm642-one-targeted-reg-form seg otherform ($ arm64::arg_z))
            (arm642-two-targeted-reg-forms seg form1 ($ arm64::arg_y) form2 ($ arm64::arg_z)))
          (ensuring-node-target (target vreg)
            (if otherform
              (unless (acode-fixnum-form-p otherform)
                (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_z) (aref *backend-labels* out-of-line)))
              (if (acode-fixnum-form-p form1)
                (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_z) (aref *backend-labels* out-of-line))
                (if (acode-fixnum-form-p form2)
                  (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_y) (aref *backend-labels* out-of-line))
                  (arm642-branch-unless-both-args-fixnums seg ($ arm64::arg_y) ($ arm64::arg_z) (aref *backend-labels* out-of-line)))))
            (if otherform
              (cond
                (maskable
                 (let* ((nbits (- *arm642-target-bits-in-word*
                                  (1+ (+ *arm642-target-fixnum-shift* fixlen)))))
                   (if (> fixval 0)
                     (! clear-left ($ arm64::arg_z) ($ arm64::arg_z) nbits)
                     (! clear-right ($ arm64::arg_z) ($ arm64::arg_z) (+ fixlen
 *arm642-target-fixnum-shift*)))))
                ((arm64::encode-logical-immediate unboxed-fixval)
                 (! logand-imm ($ arm64::arg_z) ($ arm64::arg_z) unboxed-fixval))
                (t
                 (arm642-lri seg ($ arm64::arg_y) unboxed-fixval)
                 (! %logand2 ($ arm64::arg_z) ($ arm64::arg_z) ($ arm64::arg_y))))
              (! %logand2 ($ arm64::arg_z) ($ arm64::arg_z) ($ arm64::arg_y)))
            (-> done)
            (@ out-of-line)
            (if otherform
              (arm642-lri seg ($ arm64::arg_y) (ash fixval *arm642-target-fixnum-shift*)))
            (! call-subprim-2 ($ arm64::arg_z)
               (arm642-builtin-index-subprim (arch::builtin-function-name-offset 'logand-2))
               ($ arm64::arg_y) ($ arm64::arg_z))
            (@ done)
            (arm642-copy-register seg target ($ arm64::arg_z)))
          (^))))))


;;; Donor: arm2.lisp arm2-logior2 @7198 / v2 @7382 (identical dispatch).
(defarm642 arm642-logior2 logior2 (seg vreg xfer form1 form2)
  (if (or (arm642-explicit-non-fixnum-type-p form1)
          (arm642-explicit-non-fixnum-type-p form2))
    (arm642-binary-builtin seg vreg xfer 'logior-2 form1 form2)
    (arm642-inline-logior2 seg vreg xfer form1 form2)))


;;; Donor: arm2.lisp arm2-logxor2 @7204 / v2 @7388 (identical â€” no inline
;;; path in either donor).
(defarm642 arm642-logxor2 logxor2 (seg vreg xfer form1 form2)
  (arm642-binary-builtin seg vreg xfer 'logxor-2 form1 form2))


;;; Donor: arm2.lisp arm2-logand2 @7245 / v2 @7445 (identical dispatch).
(defarm642 arm642-logand2 logand2 (seg vreg xfer form1 form2)
  (if (or (arm642-explicit-non-fixnum-type-p form1)
          (arm642-explicit-non-fixnum-type-p form2))
    (arm642-binary-builtin seg vreg xfer 'logand-2 form1 form2)
    (arm642-inline-logand2 seg vreg xfer form1 form2)))


;;; Donor: arm2.lisp arm2-logbitp @7157 / v2 @7331 (identical).
(defarm642 arm642-logbitp logbitp (seg vreg xfer bitnum int)
  (arm642-binary-builtin seg vreg xfer 'logbitp bitnum int))


;;; Donor: arm2.lisp arm2-%ilogbitp @6627 (tst-based flags shape, matching
;;; this backend's crf/cond->boolean style; v2 @6928 used a PPC-artifact
;;; msb0-extract + lowbit->truth family, not carried). Constant-bit clamp
;;; RE-DERIVED for 64/shift-3: the vinsn tests bit (+ bitnum fixnumshift) of
;;; the boxed word (cf. ARM32 vinsn @3990), so bitnum caps at
;;; (- 63 fixnumshift) = 60 â€” bits >= 60 of a fixnum all equal the sign bit,
;;; so the clamped test is still correct for larger constant indices.
;;; (arm2's own clamp (- nbits-in-word fixnumshift) = bit 32 of a 32-bit word
;;; looks off-by-one-word; PPC64's (min (+ fixbit shift) 63) semantics used
;;; instead.)
(defarm642 arm642-%ilogbitp %ilogbitp (seg vreg xfer cc bitnum form)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil bitnum)
      (arm642-form seg vreg xfer form))
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
      (let* ((fixbit (acode-fixnum-form-p bitnum)))
        (if fixbit
          (let* ((reg (arm642-one-untargeted-reg-form seg form arm64::arg_z))
                 (arm64-bit (min (- (1- *arm642-target-bits-in-word*)
 *arm642-target-fixnum-shift*)
                                 (max fixbit 0))))
            (regspec-crf-gpr-case
             (vreg)
             (progn
               (! %ilogbitp-constant-bit vreg reg arm64-bit)
               (^ cr-bit true-p))
             (with-crf-target () crf
               (! %ilogbitp-constant-bit crf reg arm64-bit)
               (ensuring-node-target (target vreg)
                 (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
               (^))))
          (multiple-value-bind (rbit rform) (arm642-two-untargeted-reg-forms seg bitnum arm64::arg_y form arm64::arg_z)
            (regspec-crf-gpr-case
             (vreg)
             (progn
               (! %ilogbitp-variable-bit vreg rform rbit)
               (^ cr-bit true-p))
             (with-crf-target () crf
               (! %ilogbitp-variable-bit crf rform rbit)
               (ensuring-node-target (target vreg)
                 (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
               (^)))))))))


;;; ---------------------------------------------------------------------------
;;; Arithmetic.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-inline-sub2 @7107 â€” the exact shape of HIS
;;; arm642-inline-add2 (@2025), with .SPbuiltin-minus (a named subprim in his
;;; arch, arm64-arch.lisp @372) out of line. v2's @7247 is the same logic
;;; with v2 sptab spelling.
(defun arm642-inline-sub2 (seg vreg xfer form1 form2)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (arm642-two-targeted-reg-forms seg form1 ($ arm64::arg_y) form2 ($ arm64::arg_z))
    (let* ((out-of-line (backend-get-next-label))
           (done (backend-get-next-label)))
      (ensuring-node-target (target vreg)
        (if (acode-fixnum-form-p form1)
          (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_z) (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (arm642-branch-unless-arg-fixnum seg ($ arm64::arg_y) (aref *backend-labels* out-of-line))
            (arm642-branch-unless-both-args-fixnums seg ($ arm64::arg_y) ($ arm64::arg_z) (aref *backend-labels* out-of-line))))
        (let* ((flags (make-hard-crf-reg 0)))
          (! fixnum-sub-set-flags ($ arm64::arg_z) flags ($ arm64::arg_y) ($ arm64::arg_z))
          (arm642-check-fixnum-overflow seg flags ($ arm64::arg_z) done))
        (@ out-of-line)
        (! call-subprim-2 ($ arm64::arg_z)
           (subprim-name->offset '.SPbuiltin-minus) ($ arm64::arg_y)
           ($ arm64::arg_z))
        (@ done)
        (arm642-copy-register seg target ($ arm64::arg_z)))
      (^))))


;;; Donor: arm2.lisp arm2-sub2 @7144 / v2 @7283 (identical dispatch).
(defarm642 arm642-sub2 sub2 (seg vreg xfer form1 form2)
  (if (or (arm642-explicit-non-fixnum-type-p form1)
          (arm642-explicit-non-fixnum-type-p form2))
    (arm642-binary-builtin seg vreg xfer '--2 form1 form2)
    (arm642-inline-sub2 seg vreg xfer form1 form2)))


;;; Donor: arm2.lisp arm2-mul2 @7150 / v2 @7289 (identical).
(defarm642 arm642-mul2 mul2 (seg vreg xfer form1 form2)
  (arm642-binary-builtin seg vreg xfer '*-2 form1 form2))


;;; Donor: v2 arm64-p2-fixnum-add @7487 (flags shape = his check-fixnum-
;;; overflow/fixnum-add-set-flags style; fixnum-add-set-flags is one of his
;;; LIVE vinsns). The immediate window is v2's ARM64-specific contract:
;;; the SHIFTED constant must fit (signed-byte 24) â€” the add-immediate vinsn
;;; splits it into 1-2 ADD/SUB instructions; larger constants take the
;;; register form. All shift arithmetic symbolic via
;;; *arm642-target-fixnum-shift* (donor comment block @7512 carried).
(defun arm642-fixnum-add (seg vreg xfer form1 form2 overflow)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (when overflow
      (let* ((type *arm642-target-half-fixnum-type*))
        (when (and (arm642-form-typep form1 type)
                   (arm642-form-typep form2 type))
          (setq overflow nil))))
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2))
           (sum (and fix1 fix2 (if overflow (+ fix1 fix2) (%i+ fix1 fix2)))))
      (cond ((null vreg)
             (arm642-form seg nil nil form1)
             (arm642-form seg nil xfer form2))
            (sum
             (if (nx1-target-fixnump sum)
               (arm642-use-operator (%nx1-operator fixnum) seg vreg xfer sum)
               (arm642-use-operator (%nx1-operator immediate) seg vreg xfer sum)))
            (overflow
             (multiple-value-bind (r1 r2) (arm642-two-untargeted-reg-forms seg form1 arm64::arg_y form2 arm64::arg_z)
               (ensuring-node-target (target vreg)
                 (let* ((flags (make-hard-crf-reg 0)))
                   (! fixnum-add-set-flags target flags r1 r2)
                   (arm642-check-fixnum-overflow seg flags target)))
               (^)))
            (t
             (let* ((other (if (and fix1
                                    (typep (ash fix1 *arm642-target-fixnum-shift*)
                                           '(signed-byte 24)))
                             form2
                             (if (and fix2
                                      (typep (ash fix2 *arm642-target-fixnum-shift*)
                                             '(signed-byte 24)))
                               form1))))
               (if (and fix1 fix2)
                 (arm642-lri seg vreg (ash (+ fix1 fix2) *arm642-target-fixnum-shift*))
                 (if other
                   (let* ((constant (ash (or fix1 fix2) *arm642-target-fixnum-shift*))
                          (reg (arm642-one-untargeted-reg-form seg other arm64::arg_z)))
                     (if (zerop constant)
                       (<- reg)
                       (ensuring-node-target (target vreg)
                         (! add-immediate target reg constant))))
                   (multiple-value-bind (r1 r2) (arm642-two-untargeted-reg-forms seg form1 arm64::arg_y form2 arm64::arg_z)
                     (ensuring-node-target (target vreg)
                       (! fixnum-add target r1 r2)))))
               (^)))))))


;;; Donor: arm2.lisp arm2-fixnum-add-overflow/-no-overflow @7362/@7365
;;; (v2 @7548/@7551 identical).
(defarm642 arm642-fixnum-add-overflow fixnum-add-overflow (seg vreg xfer form1 form2)
  (arm642-fixnum-add seg vreg xfer form1 form2 t))


(defarm642 arm642-fixnum-add-no-overflow fixnum-add-no-overflow (seg vreg xfer form1 form2)
  (arm642-fixnum-add seg vreg xfer form1 form2 nil))


;;; Donor: v2 arm64-p2-fixnum-sub @7554 (flags shape; PPC64 ppc2.lisp @6706
;;; is line-identical â€” this is the PPC64-lineage body, already
;;; fixnumshift-3-correct symbolically). fixnum-sub-from-constant takes the
;;; UNSHIFTED constant and the vinsn applies the fixnum shift (PPC64 vinsn
;;; precedent: subfic dest y (:apply ash x fixnumshift)); the
;;; (- 15 fixnumshift) gate keeps the shifted value within s16 â€” at shift 3
;;; that is 12 bits, within a single ARM64 SUB/RSB-style immediate window.
(defun arm642-fixnum-sub (seg vreg xfer num1 num2 overflow)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (when overflow
      (let* ((type *arm642-target-half-fixnum-type*))
        (when (and (arm642-form-typep num1 type)
                   (arm642-form-typep num2 type))
          (setq overflow nil))))
    (let* ((v1 (acode-fixnum-form-p num1))
           (v2 (acode-fixnum-form-p num2))
           (diff (and v1 v2 (if overflow (- v1 v2) (%i- v1 v2)))))
      (if diff
        (if (nx1-target-fixnump diff)
          (arm642-use-operator (%nx1-operator fixnum) seg vreg xfer diff)
          (arm642-use-operator (%nx1-operator immediate) seg vreg xfer diff))
        (if (and v2 (neq v2 most-negative-fixnum))
          (arm642-fixnum-add seg vreg xfer num1 (make-acode (%nx1-operator fixnum) (- v2)) overflow)
          (if (eq v2 0)
            (arm642-form seg vreg xfer num1)
            (cond
              ((null vreg)
               (arm642-form seg nil nil num1)
               (arm642-form seg nil xfer num2))
              (overflow
               (multiple-value-bind (r1 r2) (arm642-two-untargeted-reg-forms seg num1 arm64::arg_y num2 arm64::arg_z)
                 (ensuring-node-target (target vreg)
                   (let* ((flags (make-hard-crf-reg 0)))
                     (! fixnum-sub-set-flags target flags r1 r2)
                     (arm642-check-fixnum-overflow seg flags target)))
                 (^)))
              ((and v1 (<= (integer-length v1) (- 15 *arm642-target-fixnum-shift*)))
               (ensuring-node-target (target vreg)
                 (! fixnum-sub-from-constant target v1 (arm642-one-untargeted-reg-form seg num2 arm64::arg_z)))
               (^))
              (t
               (multiple-value-bind (r1 r2) (arm642-two-untargeted-reg-forms seg num1 arm64::arg_y num2 arm64::arg_z)
                 (ensuring-node-target (target vreg)
                   (! fixnum-sub target r1 r2))
                 (^))))))))))


;;; Donor: arm2.lisp arm2-fixnum-sub-no-overflow/-overflow @7463/@7466
;;; (v2 @7595/@7598 identical).
(defarm642 arm642-fixnum-sub-no-overflow fixnum-sub-no-overflow (seg vreg xfer num1 num2)
  (arm642-fixnum-sub seg vreg xfer num1 num2 nil))


(defarm642 arm642-fixnum-sub-overflow fixnum-sub-overflow (seg vreg xfer num1 num2)
  (arm642-fixnum-sub seg vreg xfer num1 num2 t))


;;; %i+ / %i- registrations. Donor: arm2.lisp arm2-%i- @7460; arm2 does NOT
;;; register %i+ (nor does v2 register either), but the operators exist in
;;; nxenv.lisp @120-121 with the same (form1 form2 &optional overflow)
;;; arglist and x862 registers both â€” registered here for completeness at
;;; zero cost (both bodies exist anyway). See U8w.
(defarm642 arm642-%i+ %i+ (seg vreg xfer form1 form2 &optional overflow)
  (arm642-fixnum-add seg vreg xfer form1 form2 overflow))


(defarm642 arm642-%i- %i- (seg vreg xfer num1 num2 &optional overflow)
  (arm642-fixnum-sub seg vreg xfer num1 num2 overflow))


;;; Donor: v2 arm64-p2-%i* @7601 (adds the multiply-immediate leg over arm2's
;;; @7470; the immediate is the UNSHIFTED (signed-byte 16) fixnum â€” dest =
;;; boxed-src * raw-constant preserves boxing at any fixnum shift).
(defarm642 arm642-%i* %i* (seg vreg xfer num1 num2)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil num1)
      (arm642-form seg nil xfer num2))
    (let* ((fix1 (acode-fixnum-form-p num1))
           (fix2 (acode-fixnum-form-p num2))
           (other (if (typep fix1 '(signed-byte 16)) num2 (if (typep fix2 '(signed-byte 16)) num1))))
      (if (and fix1 fix2)
        (arm642-lri seg vreg (ash (* fix1 fix2) *arm642-target-fixnum-shift*))
        (if other
          (! multiply-immediate vreg (arm642-one-untargeted-reg-form seg other arm64::arg_z) (or fix1 fix2))
          (multiple-value-bind (rx ry) (arm642-two-untargeted-reg-forms seg num1 arm64::arg_y num2 arm64::arg_z)
            (ensuring-node-target (target vreg)
              (! multiply-fixnums target rx ry)))))
      (^))))


;;; ---------------------------------------------------------------------------
;;; Fixnum shifts. %ilsl/%ilsr/%iasr operate on BOXED fixnums; at any fixnum
;;; shift the vinsns shift the boxed word (left shifts preserve the 000 tag;
;;; right shifts must re-clear the low fixnumshift bits â€” vinsn contract
;;; notes in the report).
;;; ---------------------------------------------------------------------------

;;; Donor: v2 arm64-p2-%ilsl @6691 (arm2 @6378 identical shape; max = 63,
;;; spelled symbolically instead of donor's target-arch-case).
(defarm642 arm642-%ilsl %ilsl (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil form1)
      (arm642-form seg nil xfer form2))
    (let* ((const (acode-fixnum-form-p form1))
           (max (1- *arm642-target-bits-in-word*)))
      (ensuring-node-target (target vreg)
        (if const
          (let* ((src (arm642-one-untargeted-reg-form seg form2 arm64::arg_z)))
            (if (<= const max)
              (! %ilsl-c target const src)
              (arm642-lri seg target 0)))
          (multiple-value-bind (count src) (arm642-two-untargeted-reg-forms seg form1 arm64::arg_y form2 arm64::arg_z)
            (! %ilsl target count src))))
      (^))))


;;; Donor: v2 arm64-p2-%ilsr @7779 (= PPC64 ppc2.lisp @6921 line-for-line,
;;; including the (<= count 31) gate â€” see U7w; NOT silently changed).
(defarm642 arm642-%ilsr %ilsr (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil form1)
      (arm642-form seg vreg xfer form2))
    (let* ((count (acode-fixnum-form-p form1)))
      (ensuring-node-target (target vreg)
        (if count
          (let ((src (arm642-one-untargeted-reg-form seg form2 ($ arm64::arg_z))))
            (if (<= count 31)
              (! %ilsr-c target count src)
              (arm642-lri seg target 0)))
          (multiple-value-bind (cnt src) (arm642-two-targeted-reg-forms seg form1 ($ arm64::arg_y) form2 ($ arm64::arg_z))
            (! %ilsr target cnt src))))
      (^))))


;;; Donor: v2 arm64-p2-%iasr @7763 (arm2 @7608 identical shape; max 63
;;; symbolic as in %ilsl).
(defarm642 arm642-%iasr %iasr (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil form1)
      (arm642-form seg vreg xfer form2))
    (let* ((count (acode-fixnum-form-p form1))
           (max (1- *arm642-target-bits-in-word*)))
      (declare (fixnum max))
      (ensuring-node-target (target vreg)
        (if count
          (! %iasr-c target (if (> count max) max count)
             (arm642-one-untargeted-reg-form seg form2 arm64::arg_z))
          (multiple-value-bind (cnt src) (arm642-two-targeted-reg-forms seg form1 ($ arm64::arg_y) form2 ($ arm64::arg_z))
            (! %iasr target cnt src))))
      (^))))


;;; Donor: arm2.lisp arm2-values @7491 / v2 @7630 (identical minus lcell
;;; rebind). Required by prog1's single-form leg.
(defarm642 arm642-values values (seg vreg xfer forms)
  (if (eq (list-length forms) 1)
    (if (arm642-cd-compound-p xfer)
      (arm642-form seg vreg xfer (%car forms))
      (progn
        (arm642-form seg vreg nil (%car forms))
        (^)))
    (if (not (arm642-mv-p xfer))
      (if forms
        (arm642-use-operator (%nx1-operator prog1) seg vreg xfer forms)
        (arm642-nil seg vreg xfer))
      (progn
        (let* ((*arm642-vstack* *arm642-vstack*))
          (arm642-set-nargs seg (arm642-formlist seg forms nil)))
        (let* ((*arm642-returning-values* t))
          (^))))))


;;; Donor: v2 arm64-p2-setq-lexical @6899 (arm2 @6580 adds an nfp-memspec
;;; leg via arm2-reg-for-nfp-set/arm2-nfp-set — no NFP memspecs in his
;;; backend yet, U7v; v2's shape is arm2's minus that leg).
(defarm642 arm642-setq-lexical setq-lexical (seg vreg xfer varspec form)
  (let* ((ea (var-ea varspec)))
 ;;(unless (fixnump ea) (compiler-bug "setq lexical is losing BIG"))
    (let* ((valreg (arm642-one-untargeted-reg-form seg form (if (and (register-spec-p ea)
                                                                     (or (null vreg) (eq ea vreg)))
                                                              ea
                                                              arm64::arg_z))))
      (arm642-do-lexical-setq seg vreg ea valreg))
    (^)))


;;; Donor: arm2.lisp arm2-setq-special @6320 / v2 @6635 (identical).
(defarm642 arm642-setq-special setq-special (seg vreg xfer sym val)
  (let* ((symreg ($ arm64::arg_y))
         (valreg ($ arm64::arg_z)))
    (arm642-one-targeted-reg-form seg val valreg)
    (arm642-store-immediate seg (arm642-symbol-value-cell sym) symreg)
    (! setq-special symreg valreg)
    (<- valreg))
  (^))


;;; ---------------------------------------------------------------------------
;;; Local control transfer: tagbody / go / block / return-from.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-local-go @6332 / v2 @6645 (identical).
(defarm642 arm642-local-go local-go (seg vreg xfer tag)
  (declare (ignorable xfer))
  (let* ((curstack (arm642-encode-stack))
         (label (cadr tag))
         (deststack (caddr tag)))
    (if (not (arm642-equal-encodings-p curstack deststack))
      (multiple-value-bind (catch cstack vstack)
                           (arm642-decode-stack deststack)
        (arm642-unwind-stack seg nil catch cstack vstack)))
    (-> label)
    (arm642-unreachable-store vreg)))


;;; Donor: arm2.lisp arm2-local-tagbody @6684 / v2 @6990 (identical).
(defarm642 arm642-local-tagbody local-tagbody (seg vreg xfer taglist body)
  (let* ((encstack (arm642-encode-stack))
         (tagop (%nx1-operator tag-label)))
    (dolist (tag taglist)
      (rplacd tag (cons (backend-get-next-label) (cons encstack (cadr (cddr (cddr tag)))))))
    (dolist (form body)
      (if (eq (acode-operator form) tagop)
        (let ((tag (cdar (acode-operands form))))
          (@ (car tag)))
        (arm642-form seg nil nil form)))
    (arm642-nil seg vreg xfer)))


;;; Donor: arm2.lisp arm2-local-block @6344 / v2 @6657 (identical).
(defarm642 arm642-local-block local-block (seg vreg xfer blocktag body)
  (let* ((curstack (arm642-encode-stack))
         (compound (arm642-cd-compound-p xfer))
         (mvpass-p (arm642-mvpass-p xfer))
         (need-label (if xfer (or compound mvpass-p) t))
         end-of-block
         last-cd
         (dest (if (backend-crf-p vreg) arm64::arg_z vreg)))
    (if need-label
      (setq end-of-block (backend-get-next-label)))
    (setq last-cd (if need-label (%ilogior2 (if mvpass-p $backend-mvpass-mask 0) end-of-block) xfer))
    (%rplaca blocktag (cons (cons dest last-cd) curstack))
    (if mvpass-p
      (arm642-multiple-value-body seg body)
      (arm642-form seg dest (if xfer last-cd) body))
    (when need-label
      (@ end-of-block)
      (if compound
        (<- dest))
      (arm642-branch seg (logand (lognot $backend-mvpass-mask) (or xfer 0)) vreg))))


;;; Donor: arm2.lisp arm2-local-return-from @8219 (v2 @8352 identical +
;;; lcell binding, dropped). NOTE both donors test (neq cur-stack
;;; dest-stack) — object identity of the encoding VECTORS, deliberately
;;; conservative (need-break may be true when encodings are equal in
;;; content; the unwind then degenerates harmlessly) — carried verbatim.
(defarm642 arm642-local-return-from local-return-from (seg vreg xfer blocktag value)
  (declare (ignorable vreg xfer))
  (let* ((*arm642-undo-count* *arm642-undo-count*)
         (tagdata (car blocktag))
         (cur-stack (arm642-encode-stack))
         (dest-vd (caar tagdata))
         (dest-cd (cdar tagdata))
         (mv-p (arm642-mvpass-p dest-cd))
         (dest-stack (cdr tagdata))
         (need-break (neq cur-stack dest-stack)))
    (let* ((*arm642-vstack* *arm642-vstack*)
           (*arm642-cstack* *arm642-cstack*))
      (if
        (or
         (eq dest-cd $backend-return)
         (and mv-p
              (eq (arm642-encoding-undo-count cur-stack)
                  (arm642-encoding-undo-count dest-stack))
              (eq (arm642-encoding-cstack-depth cur-stack)
                  (arm642-encoding-cstack-depth dest-stack))))
        (arm642-form seg dest-vd dest-cd value)
        (if mv-p
          (progn
            (arm642-multiple-value-body seg value)
            (let* ((*arm642-returning-values* :pass))
              (arm642-nlexit seg dest-cd (%i- *arm642-undo-count* (arm642-encoding-undo-count dest-stack)))
              (arm642-branch seg dest-cd vreg)))
          (progn
            (arm642-form
             seg
             (if need-break (if dest-vd arm64::arg_z) dest-vd)
             (if need-break nil dest-cd)
             value)
            (when need-break
              (arm642-unwind-set seg dest-cd dest-stack)
              (when dest-vd (arm642-copy-register seg dest-vd arm64::arg_z))
              (arm642-branch seg dest-cd dest-vd))))))
    (arm642-unreachable-store)))


;;; ---------------------------------------------------------------------------
;;; Conditionals.
;;; ---------------------------------------------------------------------------

;;; Donor: v2 arm64-p2-if @7050 STRUCTURE (arm2 @6885 is the same skeleton
;;; wrapped in newer machinery his backend lacks: nx2-reconstruct-case /
;;; arm2-generate-casejump, regmap/constmap save-restore, predicate-block —
;;; all dropped by necessity, U10v). Two deviations from v2:
;;; - lcell bookkeeping dropped (3-value decode-stack, no top-lcell save).
;;; - the nilret-label merge (v2 @7076/@7083/@7112:
;;; arm64-p2-find/record-nilret-label + *arm64-p2-nilret-labels*) is
;;; dropped: the special is bound per-compile in v2's arm64-p2-compile
;;; @555 and HIS arm642-compile does not bind it — a global defvar would
;;; leak stale label indices across compiles. arm2 PRECEDENT: its `if'
;;; has no nilret machinery either. Pure size optimization (shared
;;; NIL-return tails), zero semantic loss. See U11v.
(defarm642 arm642-if if (seg vreg xfer testform true false &aux test-val)
  (if (setq test-val (nx2-constant-form-value (acode-unwrapped-form-value testform)))
    (arm642-form seg vreg xfer (if (nx-null test-val) false true))
    (let* ((cstack *arm642-cstack*)
           (vstack *arm642-vstack*)
           (entry-stack (arm642-encode-stack))
           (true-stack nil)
           (false-stack nil)
           (true-cleanup-label nil)
           (same-stack-effects nil)
           (true-is-goto (arm642-go-label true))
           (false-is-goto (and (not true-is-goto) (arm642-go-label false)))
           (endlabel (backend-get-next-label))
           (falselabel (backend-get-next-label))
           (need-else (unless false-is-goto (or (not (nx-null false)) (arm642-for-value-p vreg))))
           (both-single-valued (and (not *arm642-open-code-inline*)
                                    (eq xfer $backend-return)
                                    (arm642-for-value-p vreg)
                                    need-else
                                    (arm642-single-valued-form-p true)
                                    (arm642-single-valued-form-p false))))
      (if (eq 0 xfer)
        (setq xfer nil))
      (if both-single-valued            ; it's implied that we're returning
        (let* ((result arm64::arg_z))
          (arm642-conditional-form seg (arm642-make-compound-cd 0 falselabel) testform)
          (arm642-form seg result endlabel true)
          (@ falselabel)
          (arm642-form seg result nil false)
          (@ endlabel)
          (<- result)
          (^))
        (progn
          (if (and need-else (arm642-mvpass-p xfer))
            (setq true-cleanup-label (backend-get-next-label)))
          (arm642-conditional-form
           seg
           (arm642-make-compound-cd
            (or true-is-goto 0)
            (or false-is-goto
                (if need-else
                  (if true-is-goto 0 falselabel)
                  (if true-is-goto xfer (arm642-cd-merge xfer falselabel)))))
           testform)
          (if true-is-goto
            (arm642-unreachable-store)
            (if true-cleanup-label
              (progn
                (arm642-open-undo $undomvexpect)
                (arm642-form seg vreg (logior $backend-mvpass-mask true-cleanup-label) true))
              (arm642-form seg vreg (if need-else (arm642-cd-merge xfer endlabel) xfer) true)))
          (setq true-stack (arm642-encode-stack))
          (setq *arm642-cstack* cstack)
          (arm642-set-vstack vstack)
          (if false-is-goto (arm642-unreachable-store))
          (@ falselabel)
          (when need-else
            (if true-cleanup-label
              (arm642-mvpass seg false)
              (arm642-form seg vreg xfer false))
            (setq false-stack (arm642-encode-stack)))
          (when true-cleanup-label
            (if (setq same-stack-effects (arm642-equal-encodings-p true-stack false-stack)) ; can share cleanup code
              (@ true-cleanup-label))
            (let* ((*arm642-returning-values* :pass))
              (arm642-nlexit seg xfer 1)
              (arm642-branch seg (if (and xfer (neq xfer $backend-mvpass-mask)) xfer (if (not same-stack-effects) endlabel)) vreg))
            (unless same-stack-effects
              (@ true-cleanup-label)
              (multiple-value-setq (true *arm642-cstack* *arm642-vstack*)
                (arm642-decode-stack true-stack))
              (let* ((*arm642-returning-values* :pass))
                (arm642-nlexit seg xfer 1)
                (^)))
            (arm642-close-undo)
            (multiple-value-setq (*arm642-undo-count* *arm642-cstack* *arm642-vstack*)
              (arm642-decode-stack entry-stack)))
          (@ endlabel))))))


;;; Donor: arm2.lisp arm2-or @7002 / v2 @7140 (identical).
(defarm642 arm642-or or (seg vreg xfer forms)
  (let* ((mvpass (arm642-mvpass-p xfer))
         (tag1 (backend-get-next-label))
         (tag2 (backend-get-next-label))
         (vstack *arm642-vstack*)
         (cstack *arm642-cstack*)
         (dest (if (backend-crf-p vreg) vreg (if vreg arm64::arg_z (available-crf-temp *available-backend-crf-temps*))))
         (cd1 (arm642-make-compound-cd
               (if (eq dest arm64::arg_z) tag1 (arm642-cd-merge (arm642-cd-true xfer) tag1)) 0)))
    (while (cdr forms)
      (arm642-form seg dest (if (eq dest arm64::arg_z) nil cd1) (car forms))
      (when (eq dest arm64::arg_z)
        (with-crf-target () val-crf
          (arm642-copy-register seg val-crf dest)
          (arm642-branch seg cd1 val-crf)))
      (setq forms (%cdr forms)))
    (if mvpass
      (progn (arm642-multiple-value-body seg (car forms))
             (let* ((*arm642-returning-values* t)) (arm642-branch seg (arm642-cd-merge xfer tag2) vreg)))
      (arm642-form seg vreg (if (eq dest arm64::arg_z) (arm642-cd-merge xfer tag2) xfer) (car forms)))
    (setq *arm642-vstack* vstack *arm642-cstack* cstack)
    (@ tag1)
    (when (eq dest arm64::arg_z)
      (<- arm64::arg_z)
      (^))
    (@ tag2)))


;;; ---------------------------------------------------------------------------
;;; LET / LET*.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-let @8117 (v2 @8251 identical logic + lcell
;;; %rplacd/:reserved spellings, dropped). with-arm64-p2-declarations is
;;; HIS macro @33.
(defarm642 arm642-let let (seg vreg xfer vars vals body p2decls)
  (let* ((old-stack (arm642-encode-stack))
         (*arm642-nfp-depth* *arm642-nfp-depth*)
         (val nil)
         (bits nil)
         (valcopy vals))
    (with-arm64-p2-declarations p2decls
      (dolist (var vars)
        (setq val (%car valcopy))
        (cond ((or (%ilogbitp $vbitspecial (setq bits (nx-var-bits var)))
                   (and (var-nvr var)
                        (dolist (val (%cdr valcopy))
                          (unless (arm642-trivial-p val) (return t)))))
               (let* ((pair (cons (arm642-vloc-ea *arm642-vstack*) nil)))
                 (%rplaca valcopy pair)
                 (if (and (%ilogbitp $vbitdynamicextent bits)
                          (progn
                            (setq val
                                  (arm642-dynamic-extent-form seg (arm642-encode-stack) val))
                            (arm642-load-ea-p val)))
                   (progn
                     (%rplaca pair (arm642-vloc-ea *arm642-vstack*))
                     (arm642-vpush-register seg val))
                   (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg val arm64::arg_z)))))
              (t (arm642-seq-bind-var seg var val)
                 (%rplaca valcopy nil)))
        (setq valcopy (%cdr valcopy)))
      (dolist (var vars)
        (declare (list val))
        (when (setq val (pop vals))
          (if (%ilogbitp $vbitspecial (nx-var-bits var))
            (progn
              (arm642-dbind seg (car val) (var-name var))
              (arm642-set-var-ea seg var (arm642-vloc-ea (- *arm642-vstack* *arm642-target-node-size*))))
            (arm642-seq-bind-var seg var (car val)))))
      (arm642-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (arm642-close-var seg var)))))


;;; Donor: arm2.lisp arm2-let* @7519 (v2 @7661 identical + its
;;; check-lcell-depth debug probe, dropped — lcell-only).
(defarm642 arm642-let* let* (seg vreg xfer vars vals body p2decls &aux
                                 (old-stack (arm642-encode-stack)))
  (let* ((*arm642-nfp-depth* *arm642-nfp-depth*))
    (with-arm64-p2-declarations p2decls
      (arm642-seq-bind seg vars vals)
      (arm642-undo-body seg vreg xfer body old-stack)))
  (dolist (v vars) (arm642-close-var seg v)))


;;; ---------------------------------------------------------------------------
;;; Multiple values.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-multiple-value-prog1 @7550 / v2 @7694 (identical
;;; minus lcell rebind).
(defarm642 arm642-multiple-value-prog1 multiple-value-prog1 (seg vreg xfer forms)
  (if (or (not (arm642-mv-p xfer)) (arm642-single-valued-form-p (%car forms)))
    (arm642-use-operator (%nx1-operator prog1) seg vreg xfer forms)
    (if (null (cdr forms))
      (arm642-form seg vreg xfer (car forms))
      (progn
        (let* ((*arm642-vstack* *arm642-vstack*))
          (arm642-multiple-value-body seg (%car forms))
          (arm642-open-undo $undostkblk)
          (! save-values))
        (dolist (form (cdr forms))
          (arm642-form seg nil nil form))
        (arm642-set-nargs seg 0)
        (! recover-values)
        (arm642-close-undo)
        (let* ((*arm642-returning-values* t))
          (^))))))


;;; Donor: arm2.lisp arm2-multiple-value-list @6560 / v2 @6868 (identical).
(defarm642 arm642-multiple-value-list multiple-value-list (seg vreg xfer form)
  (arm642-multiple-value-body seg form)
  (! list)
  (when vreg
    (<- arm64::arg_z))
  (^))


;;; Donor: arm2.lisp arm2-multiple-value-call @9051 / v2 @9241 (identical
;;; one-liners).
(defarm642 arm642-multiple-value-call multiple-value-call (seg vreg xfer fn arglist)
  (arm642-mvcall seg vreg xfer fn arglist))


;;; ---------------------------------------------------------------------------
;;; Self / lexical-function calls.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-self-call @6705 / v2 @7011 ($numarm64argregs; v2's
;;; $numppcargregs leftover NOT carried).
(defarm642 arm642-self-call self-call (seg vreg xfer arglist &optional spread-p)
  (setq arglist (arm642-augment-arglist *arm642-cur-afunc* arglist (if spread-p 1 $numarm64argregs)))
  (arm642-call-fn seg vreg xfer -1 arglist spread-p))


;;; Donor: arm2.lisp arm2-lexical-function-call @6710 / v2 @7016 (same
;;; $numarm64argregs note).
(defarm642 arm642-lexical-function-call lexical-function-call (seg vreg xfer afunc arglist &optional spread-p)
  (arm642-call-fn seg vreg xfer (make-acode (%nx1-operator simple-function) afunc)
                  (arm642-augment-arglist afunc arglist (if spread-p 1 $numarm64argregs))
                  spread-p))

;;; ---------------------------------------------------------------------------
;;; Predicate plumbing.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-char-p @7511 / v2 @7651 (identical shape).
;;; mask-base-char extracts the character subtag byte so comparing against
;;; arm64::subtag-character (raw, unshifted â€” his arm64-arch.lisp, cf. the
;;; :subtag-char arch slot @963) classifies the object; the vinsn contract
;;; must match HIS character representation (subtag in the LOW byte,
;;; x8664-style, vs. v2's TOP-byte tag â€” vinsn-wave decision, see U2m).
(defun arm642-char-p (seg vreg xfer cc form)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
      (! mask-base-char arm64::imm0 (arm642-one-untargeted-reg-form seg form arm64::arg_z))
      (arm642-test-reg-%izerop seg vreg xfer arm64::imm0 cr-bit true-p
                               arm64::subtag-character))))












;;; ---------------------------------------------------------------------------
;;; Type-checked forms (typed-form's check leg) and the require-* family.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-typechecked-form @8762 / v2 @8907 (line-identical;
;;; the cbranch condition constant renamed arm64::arm64-cond-eq ->
;;; arm64::cond-eq per his arm64-asm.lisp, as in w2's dynamic-extent-form).
;;; The generic leg's (ash $XWRONGTYPE *arm642-target-fixnum-shift*) is
;;; symbolic â€” correct at shift 3.
(defun arm642-typechecked-form (seg vreg xfer typespec form)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
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
        (arm642-use-operator op seg vreg xfer form)
        (if (or (eq typespec t)
                (eq typespec '*))
          (arm642-form seg vreg xfer form)
          (let* ((ok (backend-get-next-label)))
            (arm642-one-targeted-reg-form seg form ($ arm64::arg_y))
            (arm642-store-immediate seg typespec ($ arm64::arg_z))
            (arm642-store-immediate seg 'typep ($ arm64::fname))
            (arm642-set-nargs seg 2)
            (arm642-vpush-register seg ($ arm64::arg_y))
            (! call-known-symbol ($ arm64::arg_z))
            (with-crf-target () crf
              (! compare-to-nil crf ($ arm64::arg_z))
              (arm642-vpop-register seg ($ arm64::arg_y))
              (! cbranch-false (aref *backend-labels* ok) crf arm64::cond-eq))
            (arm642-lri seg ($ arm64::arg_x) (ash $XWRONGTYPE *arm642-target-fixnum-shift*))
            (arm642-store-immediate seg typespec ($ arm64::arg_z))
            (arm642-set-nargs seg 3)
            (! ksignalerr)
            (@ ok)
            (<- ($ arm64::arg_y))
            (^)))))))


;;; ---------------------------------------------------------------------------
;;; Acode handler registrations â€” equality.
;;; ---------------------------------------------------------------------------

;;; Donor: arm2.lisp arm2-eq @6475 / v2 @6788 (identical). The acode
;;; carries the condition as its FIRST operand (an (immediate :EQ) form at
;;; the live gate frontier); acode-condition-to-arm64-cond-bit (w1) unwraps
;;; it. arm642-compare (w1) handles the fixnum-0 / s16-immediate fast
;;; paths â€” including the gate's (eq lexical-ref fixnum-0) shape, which
;;; lands on w1's arm642-test-reg-%izerop leg.
(defarm642 arm642-eq eq (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
    (arm642-compare seg vreg xfer form1 form2 cr-bit true-p)))


;;; Donor: arm2.lisp arm2-neq @6479 / v2 @6792 (identical; the :NE
;;; inversion is already inside cc â€” condition-to-arm64-cond-bit returns
;;; (values cond-eq nil)).
(defarm642 arm642-neq neq (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
    (arm642-compare seg vreg xfer form1 form2 cr-bit true-p)))


;;; Donor: arm2.lisp arm2-characterp @6266 / v2 @6593 (identical one-liners).
(defarm642 arm642-characterp characterp (seg vreg xfer cc form)
  (arm642-char-p seg vreg xfer cc form))


;;; Donor: arm2.lisp arm2-base-char-p @7508 / v2 @7648 (identical; both
;;; donors treat base-char-p = char-p since all their chars are base chars).
(defarm642 arm642-base-char-p base-char-p (seg vreg xfer cc form)
  (arm642-char-p seg vreg xfer cc form))


;;; Donor: arm2.lisp arm2-endp @6395 / v2 @6708 (identical). The list
;;; typecheck traps even when the value is unused (CL endp contract).
(defarm642 arm642-endp endp (seg vreg xfer cc form)
  (let* ((formreg (arm642-one-untargeted-reg-form seg form arm64::arg_z)))
    (! trap-unless-list formreg)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
      (arm642-compare-register-to-nil seg vreg xfer formreg cr-bit true-p))))


;;; Donor: arm2.lisp arm2-istruct-typep @6280 / v2 @6602 (identical).
;;; istruct-typep vinsn contract: signed-natural dest = 0 iff FORM is an
;;; istruct whose istruct-cell EQs TYPE (subprim-backed in both lineages).
(defarm642 arm642-istruct-typep istruct-typep (seg vreg xfer cc form type)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
    (multiple-value-bind (r1 r2) (arm642-two-untargeted-reg-forms seg form arm64::arg_y type arm64::arg_z)
      (with-imm-target () (target :signed-natural)
        (! istruct-typep target r1 r2)
        (arm642-test-reg-%izerop seg vreg xfer target cr-bit true-p 0)))))


;;; ---------------------------------------------------------------------------
;;; Tag readers. These ARE the p2 face of fixnump/listp/symbolp etc. â€”
;;; the host compiler-macros lower those predicates to (eql (lisptag x) k)
;;; / (eql (typecode x) k) (optimizers.lisp @1838/@1874/@1899), which
;;; reach `eql''s compiler-macro and finally the eq/%i<>/typecode operators.
;;; Donor bodies: v2 (simple ensuring-node-target shape). arm2 @6287-6318
;;; additionally supports :u8-targeted results via unboxed-other-case +
;;; *arm2-operator-supports-u8-target* â€” machinery his backend doesn't
;;; define; dropped like v2 dropped it (U7m: pure optimization, the boxed
;;; path is always correct).
;;; ---------------------------------------------------------------------------

;;; Donor: v2 arm64-p2-lisptag @6610 (arm2 @6287 modulo u8 leg).
;;; extract-tag-fixnum contract: dest := (and src tagmask) as a FIXNUM.
(defarm642 arm642-lisptag lisptag (seg vreg xfer node)
  (if (null vreg)
    (arm642-form seg vreg xfer node)
    (progn
      (ensuring-node-target (target vreg)
        (! extract-tag-fixnum target (arm642-one-untargeted-reg-form seg node arm64::arg_z)))
      (^))))


;;; Donor: v2 arm64-p2-fulltag @6618 (arm2 @6298 modulo u8 leg).
;;; extract-fulltag-fixnum contract: dest := (and src fulltagmask) as FIXNUM.
(defarm642 arm642-fulltag fulltag (seg vreg xfer node)
  (if (null vreg)
    (arm642-form seg vreg xfer node)
    (progn
      (ensuring-node-target (target vreg)
        (! extract-fulltag-fixnum target (arm642-one-untargeted-reg-form seg node arm64::arg_z)))
      (^))))


;;; Donor: v2 arm64-p2-typecode @6626 (arm2 @6310 modulo u8 leg). The
;;; source register dodges arg_z when the vreg IS arg_z (donor quirk,
;;; carried â€” extract-typecode-fixnum may need scratch).
(defarm642 arm642-typecode typecode (seg vreg xfer node)
  (if (null vreg)
    (arm642-form seg vreg xfer node)
    (let* ((reg (arm642-one-untargeted-reg-form seg node (if (eq (hard-regspec-value vreg) arm64::arg_z)
                                                           arm64::arg_y arm64::arg_z))))
      (ensuring-node-target (target vreg)
        (! extract-typecode-fixnum target reg))
      (^))))


;;; Donor: arm2.lisp def-arm2-require macrolet @8735 / v2 @8879
;;; (line-identical). Each require-X vinsn traps (uuo) unless val-reg is
;;; an X; value passes through. DONOR QUIRK carried verbatim (U8m): the
;;; (eq vreg arm64::arg_z) test compares an lreg to a raw register NUMBER
;;; and is thus always false in practice â€” both donors ship it; the
;;; consequence is only that val is always evaluated to arg_z (never
;;; arg_y), which is harmless.
(macrolet
  ((def-arm642-require (function op &optional (vinsn op))
     `(defarm642 ,function ,op (seg vreg xfer val)
        (let* ((val-reg (arm642-one-untargeted-reg-form
                         seg
                         val
                         (if (eq vreg arm64::arg_z) arm64::arg_y arm64::arg_z))))
          (! ,vinsn val-reg)
          (when vreg (<- val-reg))
          (^)))))
  (def-arm642-require arm642-require-simple-vector require-simple-vector)
  (def-arm642-require arm642-require-simple-string require-simple-string)
  (def-arm642-require arm642-require-integer require-integer)
  (def-arm642-require arm642-require-fixnum require-fixnum)
  (def-arm642-require arm642-require-real require-real)
  (def-arm642-require arm642-require-list require-list)
  (def-arm642-require arm642-require-character require-character)
  (def-arm642-require arm642-require-number require-number)
  (def-arm642-require arm642-require-symbol require-symbol)
  (def-arm642-require arm642-require-s8 require-s8)
  (def-arm642-require arm642-require-s8 require-u8)
  (def-arm642-require arm642-require-s8 require-s16)
  (def-arm642-require arm642-require-s8 require-u16)
  (def-arm642-require arm642-require-s8 require-s32)
  (def-arm642-require arm642-require-s8 require-u32)
  (def-arm642-require arm642-require-s8 require-s64)
  (def-arm642-require arm642-require-s8 require-u64))


;;; Donor: arm2.lisp arm2-uvref @6660 / v2 @6966 (identical). Generic
;;; uvector ref: subprim-shaped misc-ref vinsn, args in arg_y (vector) /
;;; arg_z (index), boxed result in arg_z; typechecks/bounds live in the
;;; subprim.
(defarm642 arm642-uvref uvref (seg vreg xfer vector index)
  (arm642-two-targeted-reg-forms seg vector ($ arm64::arg_y) index ($ arm64::arg_z))
  (! misc-ref)
  (<- ($ arm64::arg_z))
  (^))


;;; Donor: arm2.lisp arm2-uvset @6666 / v2 @6972 (identical). misc-set:
;;; arg_x=vector arg_y=index arg_z=value; the subprim-side store performs
;;; the GC write barrier for node vectors (same class as .SPgvset â€” U4m).
(defarm642 arm642-uvset uvset (seg vreg xfer vector index value)
  (arm642-three-targeted-reg-forms seg vector ($ arm64::arg_x) index ($ arm64::arg_y) value ($ arm64::arg_z))
  (! misc-set)
  (<- ($ arm64::arg_z))
  (^))


;;; Donor: arm2.lisp arm2-struct-ref @6269 / v2 @6596 (identical).
(defarm642 arm642-struct-ref struct-ref (seg vreg xfer struct offset)
  (arm642-vref seg vreg xfer :struct struct offset (unless *arm642-reckless* (nx-lookup-target-uvector-subtag :struct))))


;;; Donor: arm2.lisp arm2-struct-set @6277 / v2 @6599 (identical).
(defarm642 arm642-struct-set struct-set (seg vreg xfer struct offset value)
  (arm642-vset seg vreg xfer :struct struct offset value (unless *arm642-reckless* (nx-lookup-target-uvector-subtag :struct))))


;;; %function â€” the LIVE gate frontier (`#<ACODE %function (HEAP-AREA-CODE)>`):
;;; load the symbol's function cell with fdefinition-check semantics (trap if
;;; unbound). Donor: v2 arm64-p2.lisp @8804 = arm2.lisp @8665 LINE-IDENTICAL
;;; (verified side-by-side). The symbol reaches symreg as an IMMEDIATE acode
;;; whose value is the fcell locative interned by arm642-symbol-entry-locative
;;; (additions.lisp:73, already defined); HIS arm642-store-immediate @834 +
;;; backend-immediate-index already handle *arm642-fcells* members (@831/@919),
;;; so the constant-pool (ref-constant) plumbing is entirely HIS.
;;; arm642-one-untargeted-reg-form is HIS @950; with-node-temps is vendor
;;; backend.lisp. Vinsn `symbol-function' is MISSING (recorded): contract
;;; ((dest :lisp) (sym :lisp)) â€” dest := sym.fcell, uuo-trap (fdefinition/
;;; udf-call error, arch::error-udf class) when the cell holds the
;;; unbound/nil-function marker. PPC64 ppc64-vinsns `symbol-function' is the
;;; LOGIC lineage; symbol.fcell offset must come from HIS arm64-arch.lisp
;;; symbol layout (pointer-tagged fulltag-symbol #b0111) â€” no invented
;;; offsets (Uw4-2).
(defarm642 arm642-%function %function (seg vreg xfer sym)
  (when vreg
    (let* ((symreg (arm642-one-untargeted-reg-form
                    seg
                    (make-acode (%nx1-operator immediate)
                                (arm642-symbol-entry-locative sym))
                    arm64::arg_z)))
      (with-node-temps (vreg symreg) (val)
        (! symbol-function val symreg)
        (<- val))))
  (^))


(defarm642 arm642-lambda-bind lambda-bind
  (seg vreg xfer vals req rest keys-p auxen body p2decls)
  (let* ((old-stack (arm642-encode-stack))
         (nreq (list-length req))
         (rest-arg (nthcdr nreq vals))
         (apply-body (arm642-eliminate-&rest body rest keys-p auxen
                                             rest-arg)))
    (arm642-seq-bind seg req vals)
    (when apply-body (setq rest nil body apply-body))
    (let* ((vloc *arm642-vstack*)
           (restloc vloc)
           (nvloc (progn (if (or rest keys-p) (arm642-formlist seg rest-arg))
 *arm642-vstack*)))
      (with-arm64-p2-declarations p2decls
        (when rest
          (when keys-p
            (until (eq restloc nvloc)
              (with-node-temps () (temp)
                (arm642-stack-to-register seg (arm642-vloc-ea restloc) temp)
                (arm642-vpush-register seg temp))
              (setq restloc (%i+ restloc *arm642-target-node-size*))))
          (arm642-set-nargs seg (length rest-arg))
          (arm642-set-vstack restloc)
          (if (%ilogbitp $vbitdynamicextent (nx-var-bits rest))
            (progn
              (! stack-cons-list)
              (arm642-open-undo $undostkblk))
            (! list))
          (arm642-vpush-register seg arm64::arg_z))
        (when rest (arm642-bind-var seg rest restloc))
        (destructuring-bind (vars inits) auxen
          (while vars
            (let ((val (%car inits)))
              (if (fixnump val)
                (progn
                  (when rest (setq val (%i+ (%i+ val val) 1)))
                  (arm642-bind-var seg (%car vars)
                                   (%i+ vloc (* val
 *arm642-target-node-size*))))
                (arm642-seq-bind-var seg (%car vars) val)))
            (setq vars (%cdr vars) inits (%cdr inits))))
        (arm642-undo-body seg vreg xfer body old-stack)
        (dolist (var req) (arm642-close-var seg var))
        (when rest (arm642-close-var seg rest))
        (dolist (var (%car auxen)) (arm642-close-var seg var))))))


;;; arm64-fpr-mask â€” named by HIS arch struct (:fpr-mask-function
;;; 'arm64-fpr-mask, arm64-arch.lisp:984) but never DEFINED in his tree
;;; (undefined-function abort in l0-array/complex/numbers; demand scan
;;; 42e1c20). ARM32's arm-fpr-mask (arm-arch.lisp:1061) spreads
;;; doubles/complexes over CONSECUTIVE S-register bits because ARM32
;;; FPRs alias (S2n/S2n+1 = Dn); on arm64 every scalar/vector view
;;; (S/D/Q) of Vn is the SAME register n â€” including his
;;; complex-double-float, which is ONE 128-bit register (lanes D[0]/
;;; D[1], w3a evidence) â€” so the mask is one bit for every mode.
;;; Mail: proposed for his tree (with this rationale).
(defun arm64::arm64-fpr-mask (value mode)
  (declare (ignore mode))
  (ash 1 value))


;;; %badarg2 â€” gate-38 frontier. Donor: PPC64 ppc2.lisp:8057 verbatim
;;; (arg_y=badthing, arg_z=goodthing, arg_x=$XWRONGTYPE fixnum,
;;; nargs=3, .SPksignalerr via the w3b ksignalerr vinsn).
(defarm642 arm642-%badarg2 %badarg2 (seg vreg xfer badthing goodthing)
  (arm642-two-targeted-reg-forms seg badthing ($ arm64::arg_y)
                                 goodthing ($ arm64::arg_z))
  (arm642-lri seg ($ arm64::arg_x)
              (ash $XWRONGTYPE *arm642-target-fixnum-shift*))
  (arm642-set-nargs seg 3)
  (! ksignalerr)
  (<- nil)
  (^))


;;; multiple-value-bind â€” gate-37 frontier. Donor: PPC64 ppc2.lisp
;;; ppc2-multiple-value-bind, transferred on the w2 no-lcell precedent
;;; (his arm642-bind-var takes no lcell; the reserve/collect-lcells
;;; block is dropped wholesale). MV body â†’ fitvals (imm0 = byte count)
;;; â†’ bind each var at its vstack slot. All helpers pre-exist: his
;;; bind-var/init-regvar/vloc-ea/set-vstack/close-var +
;;; with-arm64-p2-declarations; our w2 multiple-value-body/undo-body.
(defarm642 arm642-multiple-value-bind multiple-value-bind
  (seg vreg xfer vars valform body p2decls)
  (let* ((n (list-length vars))
         (vloc *arm642-vstack*)
         (nbytes (* n *arm642-target-node-size*))
         (old-stack (arm642-encode-stack)))
    (with-arm64-p2-declarations p2decls
      (arm642-multiple-value-body seg valform)
      (arm642-lri seg arm64::imm0 nbytes)
      (! fitvals)
      (arm642-set-vstack (%i+ vloc nbytes))
      (dolist (var vars)
        (let* ((reg (nx2-assign-register-var var)))
          (if reg
            (arm642-init-regvar seg var reg (arm642-vloc-ea vloc))
            (arm642-bind-var seg var vloc))
          (setq vloc (%i+ vloc *arm642-target-node-size*))))
      (arm642-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (arm642-close-var seg var)))))



(defarm642 arm642-%alloc-misc %make-uvector (seg vreg xfer element-count st
                                                 &optional initval)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil element-count)
      (arm642-form seg nil xfer st))
    (let* ((subtag (acode-fixnum-form-p st))
           (nelements (acode-fixnum-form-p element-count))
           (nbytes (if (and subtag nelements)
                     (arm642-misc-byte-count subtag nelements))))
      (if (and nbytes (null initval)
               (< (logand
                   (lognot (1- (* 2 *arm642-target-node-size*)))
                   (+ nbytes *arm642-target-node-size*
                      (1- (* 2 *arm642-target-node-size*)))) #x1000))
        (with-imm-temps () (header)
          (arm642-lri seg header (arch::make-vheader nelements subtag))
          (ensuring-node-target (target vreg)
            (! %alloc-misc-fixed target header nbytes)))
        (progn
          (if initval
            (progn
              (arm642-three-targeted-reg-forms seg
                                               element-count ($ arm64::arg_x)
                                               st ($ arm64::arg_y)
                                               initval ($ arm64::arg_z))
              (! misc-alloc-init)
              (<- ($ arm64::arg_z)))
            (progn
              (arm642-two-targeted-reg-forms seg
                                             element-count ($ arm64::arg_y)
                                             st ($ arm64::arg_z))
              (! misc-alloc)
              (<- ($ arm64::arg_z))))))
      (^))))


;;; builtin-call â€” gate-35 frontier. Donor: PPC64 ppc2.lisp:6187
;;; (arglist â†’ nargs; per-index subprim via the arch's
;;; primitive->subprims table, else .SPcallbuiltinN dispatch with the
;;; builtin index in imm0). Tail path follows the ESTABLISHED lane
;;; shape (w1 arm642-fixed-call-builtin): restore-nvrs (his 2-arg
;;; signature) + restore-nfp (NFP unit 2026-07-15, arm2.lisp:6734) +
;;; restore-full-lisp-context; nvfprs = remaining U1 deviation.
;;; .SPcallbuiltin{,0-3} kernel bodies: spentry-D 1160-1210,
;;; registered in the extension.
(defarm642 arm642-builtin-call builtin-call (seg vreg xfer index arglist)
  (let* ((nargs (arm642-arglist seg arglist))
         (tail-p (and (arm642-tailcallok xfer)
                      (<= nargs $numarm64argregs)))
         (idx (acode-fixnum-form-p index))
         (idx-subprim (arm642-builtin-index-subprim idx))
         (subprim
          (or idx-subprim
              (case nargs
                (0 (subprim-name->offset '.SPcallbuiltin0))
                (1 (subprim-name->offset '.SPcallbuiltin1))
                (2 (subprim-name->offset '.SPcallbuiltin2))
                (3 (subprim-name->offset '.SPcallbuiltin3))
                (t (subprim-name->offset '.SPcallbuiltin))))))
    (when tail-p
      (arm642-restore-nvrs seg nil)
      (! restore-nfp)
      (arm642-restore-full-lisp-context seg))
    (unless idx-subprim
      (arm642-lri seg arm64::imm0 (ash idx *arm642-target-fixnum-shift*))
      (when (eql subprim (subprim-name->offset '.SPcallbuiltin))
        (arm642-set-nargs seg nargs)))
    (if tail-p
      (! jump-subprim subprim)
      (progn
        (! call-subprim subprim)
        (<- ($ arm64::arg_z))
        (^)))))


(defarm642 arm642-%lisp-word-ref %lisp-word-ref (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (arm642-form seg nil nil base)
           (arm642-form seg nil xfer offset))
          ((typep fixoffset '(integer -32 4095))
           (ensuring-node-target (target vreg)
             (! lisp-word-ref-c target
                (arm642-one-untargeted-reg-form seg base arm64::arg_z)
                (ash fixoffset *arm642-target-fixnum-shift*)))
           (^))
          (t (multiple-value-bind (breg oreg)
                 (arm642-two-untargeted-reg-forms seg base arm64::arg_y
                                                  offset arm64::arg_z)
               (ensuring-node-target (target vreg)
                 (! lisp-word-ref target breg oreg))
               (^))))))






;;; Donor: v2 arm64-p2.lisp @6978 (= ppc2/arm2 verbatim shape); his
;;; with-arm64-p2-declarations @33.
(defarm642 arm642-%decls-body %decls-body (seg vreg xfer form p2decls)
  (with-arm64-p2-declarations p2decls
    (arm642-form seg vreg xfer form)))


;;; Donor: ppc2/arm2/v2 identical shape (v2 @6984). ksignalerr traps
;;; to the kernel error dispatcher with the vpushed arglist.
(defarm642 arm642-%err-disp %err-disp (seg vreg xfer arglist)
  (arm642-set-nargs seg (arm642-arglist seg arglist))
  (! ksignalerr)
  (arm642-nil seg vreg xfer))


;;; Donor: v2 arm64-p2.lisp @6683 (= ppc2 shape). reckless skips the
;;; tag trap, PPC64-identical.
(defarm642 arm642-uvsize uvsize (seg vreg xfer v)
  (let* ((misc-reg (arm642-one-untargeted-reg-form seg v arm64::arg_z)))
    (unless *arm642-reckless* (! trap-unless-uvector misc-reg))
    (if vreg
      (ensuring-node-target (target vreg)
        (! misc-element-count-fixnum target misc-reg)))
    (^)))


;;; Donor: v2 arm64-p2.lisp @7196 (= arm2 @6420 shape).
;;; acode-optimize-minus1 is host nx2.lisp:414 (backend-neutral).
(defarm642 arm642-minus1 minus1 (seg vreg xfer form)
  (or (acode-optimize-minus1 seg vreg xfer form *arm642-trust-declarations*)
      (arm642-unary-builtin seg vreg xfer '%negate form)))


;;; Donor: v2 arm64-p2.lisp @7293 (= arm2 shape). Float-typed pairs
;;; dispatch to the %double-float/-2 / %short-float/-2 operators (their
;;; handlers are later in this wave's demand list; until they land the
;;; scan shim records them — no wrong code is emitted). The fixnum
;;; (f1*x)/f2 strength-reduction and the /-2 builtin fallback are
;;; backend-neutral. nx-binop-numeric-contagion = host nx0.lisp:644.
(defarm642 arm642-div2 div2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *arm642-trust-declarations*)
    (if (and (arm642-form-typep form1 'double-float)
             (arm642-form-typep form2 'double-float))
      (arm642-use-operator (%nx1-operator %double-float/-2)
                           seg vreg xfer form1 form2)
      (if (and (arm642-form-typep form1 'single-float)
               (arm642-form-typep form2 'single-float))
        (arm642-use-operator (%nx1-operator %short-float/-2)
                             seg vreg xfer form1 form2)
        (let* ((f2 (acode-fixnum-form-p form2))
               (unwrapped (acode-unwrapped-form form1))
               (f1 nil)
               (f1/f2 nil))
          (if (and f2
                   (not (zerop f2))
                   (acode-p unwrapped)
                   (or (eq (acode-operator unwrapped) (%nx1-operator mul2))
                       (eq (acode-operator unwrapped) (%nx1-operator %i*)))
                   (setq f1 (acode-fixnum-form-p (car (acode-operands unwrapped))))
                   (typep (setq f1/f2 (/ f1 f2)) 'fixnum))
            (arm642-use-operator (%nx1-operator mul2)
                                 seg vreg xfer
                                 (make-acode (%nx1-operator fixnum) f1/f2)
                                 (cadr (acode-operands unwrapped)))
            (arm642-binary-builtin seg vreg xfer '/-2 form1 form2)))))))


;;; Donor: v2 arm64-p2.lisp arm64-p2-%typed-uvref @8658 (= ppc2 @7828).
;;; Constant subtag -> arm642-vref (W3); else subtag-misc-ref subprim.
(defarm642 arm642-%typed-uvref %typed-uvref (seg vreg xfer subtag uvector index)
  (let* ((type-keyword
          (let* ((fixtype (acode-fixnum-form-p subtag)))
            (if fixtype
              (nx-target-uvector-subtag-name fixtype)
              (acode-immediate-operand subtag)))))
    (if type-keyword
      (arm642-vref seg vreg xfer type-keyword uvector index
                   (unless *arm642-reckless* (nx-lookup-target-uvector-subtag type-keyword)))
      (progn
        (arm642-three-targeted-reg-forms seg
                                         subtag ($ arm64::arg_x)
                                         uvector ($ arm64::arg_y)
                                         index ($ arm64::arg_z))
        (! subtag-misc-ref)
        (when vreg (<- ($ arm64::arg_z)))
        (^)))))


;;; Donor: v2 arm64-p2.lisp arm64-p2-%typed-uvset @8672 (= ppc2 @7842).
;;; Constant subtag -> arm642-vset (W3); else subtag-misc-set subprim.
(defarm642 arm642-%typed-uvset %typed-uvset (seg vreg xfer subtag uvector index newval)
  (let* ((type-keyword
          (let* ((fixtype (acode-fixnum-form-p subtag)))
            (if fixtype
              (nx-target-uvector-subtag-name fixtype)
              (acode-immediate-operand subtag)))))
    (if type-keyword
      (arm642-vset seg vreg xfer type-keyword uvector index newval
                   (unless *arm642-reckless* (nx-lookup-target-uvector-subtag type-keyword)))
      (progn
        (arm642-four-targeted-reg-forms seg
                                        subtag ($ arm64::temp0)
                                        uvector ($ arm64::arg_x)
                                        index ($ arm64::arg_y)
                                        newval ($ arm64::arg_z))
        (! subtag-misc-set)
        (when vreg (<- ($ arm64::arg_z)))
        (^)))))

;;; Donor: PPC64 ppc2.lisp ppc2-aref2 @1639 (LINE-PORT; v2 @2128
;;; shape-identical). safe = fixnum: check subtag-arrayH/rank/flags;
;;; constant in-bounds i/j fold to a constant row-major index.
;;; target::arrayH.flags-cell-subtag-byte -> arm64:: (lane rule: his
;;; arch constants only); $arh_simple_bit is host lispequ, verbatim.
(defun arm642-aref2 (seg vreg xfer array i j safe typekeyword &optional dim0 dim1)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
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
        (setq src (arm642-one-targeted-reg-form seg array ($ arm64::arg_z)))
        (multiple-value-setq (src unscaled-i unscaled-j)
          (arm642-three-untargeted-reg-forms seg
                                             array arm64::arg_x
                                             i arm64::arg_y
                                             j arm64::arg_z)))
      (when safe
        (when (typep safe 'fixnum)
          (! trap-unless-simple-array-2
             src
             (dpb safe arm64::arrayH.flags-cell-subtag-byte
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
            (arm642-vref1 seg vreg xfer typekeyword v idx-reg constidx)))))))





;;; Donor: PPC64 ppc2.lisp ppc2-general-aref2 @7629 (LINE-PORT; v2 @8535).
;;; Known 2d array ctype -> inline arm642-aref2; else .SParef2 subprim
;;; (arr/i/j in arg_x/arg_y/arg_z, PPC-identical). Fallback uses the
;;; lane's name-resolving arm642-fixed-call-builtin (w1 @78), which
;;; subsumes PPC's explicit (subprim-name->offset '.SParef2).
(defarm642 arm642-general-aref2 general-aref2 (seg vreg xfer arr i j)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (dims (and atype (array-ctype-dimensions atype)))
         (keyword (and atype
                       (or (eq dims '*)
                           (and (typep dims 'list)
                                (= 2 (length dims))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (when (eq dims '*)
             (setq dims nil))
           (let* ((dim0 (car dims))
                  (dim1 (cadr dims)))
             (arm642-aref2 seg
                           vreg
                           xfer
                           arr
                           i
                           j
                           (if *arm642-reckless*
                             (make-nx-nil)
                             (nx-lookup-target-uvector-subtag keyword))
                           keyword
                           (if (typep dim0 'fixnum) dim0) (if (typep dim1 'fixnum) dim1))))
          (t
           (arm642-three-targeted-reg-forms seg
                                            arr ($ arm64::arg_x)
                                            i ($ arm64::arg_y)
                                            j ($ arm64::arg_z))
           (arm642-fixed-call-builtin seg vreg xfer '.SParef2)))))


;;; Donor: PPC64 ppc2.lisp ppc2-general-aref3 @7682 (LINE-PORT; v2 @8588).
(defarm642 arm642-general-aref3 general-aref3 (seg vreg xfer arr i j k)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (dims (and atype (array-ctype-dimensions atype)))
         (keyword (and atype
                       (or (eq dims '*)
                           (and (typep dims 'list)
                                (= 3 (length dims))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (when (eq dims '*)
             (setq dims nil))
           (let* ((dim0 (car dims))
                  (dim1 (cadr dims))
                  (dim2 (caddr dims)))
             (arm642-aref3 seg
                           vreg
                           xfer
                           arr
                           i
                           j
                           k
                           (if *arm642-reckless*
                             (make-nx-nil)
                             (nx-lookup-target-uvector-subtag keyword))
                           keyword
                           (if (typep dim0 'fixnum) dim0)
                           (if (typep dim1 'fixnum) dim1)
                           (if (typep dim2 'fixnum) dim2))))
          (t
           (arm642-four-targeted-reg-forms seg
                                           arr ($ arm64::temp0)
                                           i ($ arm64::arg_x)
                                           j ($ arm64::arg_y)
                                           k ($ arm64::arg_z))
           (arm642-fixed-call-builtin seg vreg xfer '.SParef3)))))


;;; Donor: PPC64 ppc2.lisp ppc2-general-aset2 @7733 (LINE-PORT; v2 @8639).
(defarm642 arm642-general-aset2 general-aset2 (seg vreg xfer arr i j new)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (dims (and atype (array-ctype-dimensions atype)))
         (keyword (and atype
                       (or (eq dims '*)
                           (and (typep dims 'list)
                                (= 2 (length dims))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (when (eq dims '*)
             (setq dims nil))
           (let* ((dim0 (car dims))
                  (dim1 (cadr dims)))
             (arm642-aset2 seg
                           vreg
                           xfer
                           arr
                           i
                           j
                           new
                           (unless *arm642-reckless*
                             (nx-lookup-target-uvector-subtag keyword))
                           keyword
                           (if (typep dim0 'fixnum) dim0)
                           (if (typep dim1 'fixnum) dim1))))
          (t
           (arm642-four-targeted-reg-forms seg
                                           arr ($ arm64::temp0)
                                           i ($ arm64::arg_x)
                                           j ($ arm64::arg_y)
                                           new ($ arm64::arg_z))
           (arm642-fixed-call-builtin seg vreg xfer '.SPaset2)))))


;;; Donor: PPC64 ppc2.lisp ppc2-general-aset3 @7773 (LINE-PORT; v2 @8679).
;;; NOTE the donor's (unless (atom dims) ...) here where the 2d twin
;;; wrote (and (typep dims 'list) ...) â€” preserved verbatim.
(defarm642 arm642-general-aset3 general-aset3 (seg vreg xfer arr i j k new)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (dims (and atype (array-ctype-dimensions atype)))
         (keyword (and atype
                       (or (eq dims '*)
                           (unless (atom dims)
                             (= 3 (length dims))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (when (eq dims '*)
             (setq dims nil))
           (let* ((dim0 (car dims))
                  (dim1 (cadr dims))
                  (dim2 (caddr dims)))
             (arm642-aset3 seg
                           vreg
                           xfer
                           arr
                           i
                           j
                           k
                           new
                           (unless *arm642-reckless*
                             (nx-lookup-target-uvector-subtag keyword))
                           keyword
                           (if (typep dim0 'fixnum) dim0)
                           (if (typep dim1 'fixnum) dim1)
                           (if (typep dim2 'fixnum) dim2))))
          (t
           (arm642-push-register seg (arm642-one-untargeted-reg-form seg arr ($ arm64::arg_z)))
           (arm642-four-targeted-reg-forms seg
                                           i ($ arm64::temp0)
                                           j ($ arm64::arg_x)
                                           k ($ arm64::arg_y)
                                           new ($ arm64::arg_z))
           (arm642-pop-register seg ($ arm64::temp1))
           (arm642-fixed-call-builtin seg vreg xfer '.SPaset3)))))

;;; Donor: ppc2.lisp ppc2-%set-sbchar @8065. Set-side mirror of %sbchar
;;; over the landed W3 arm642-vset helper with :simple-string.
(defarm642 arm642-%set-sbchar %set-sbchar (seg vreg xfer string index value)
  (arm642-vset seg vreg xfer :simple-string string index value
               (unless *arm642-reckless* (nx-lookup-target-uvector-subtag :simple-string))))


;;; Donor: x862.lisp x862-global-ref @11571 ( REBASE — the ppc2/v2
;;; donor fed the symbol-tagged reg to the misc-tagged slot reader,
;;; valid only where symbols ARE misc-tagged; under his fulltag-symbol=7
;;; that read [sym+4] instead of the vcell at [sym+9] — gdb-observed as
;;; the SPECIFIER-TYPE wall: *TYPE-SYSTEM-INITIALIZED* "read" returned
;;; pname/vcell byte soup 0x1300b0000302000 while the vcell held NIL).
;;; x8664 is the tag-shape donor: a dedicated symbol-ref vinsn keyed off
;;; fulltag-symbol. Assumes globals always boundp, per the x862 comment.
(defarm642 arm642-global-ref global-ref (seg vreg xfer sym)
  (when vreg
    (ensuring-node-target (target vreg)
      (with-node-temps () (symreg)
        (setq symreg (or (arm642-register-constant-p sym)
                         (arm642-store-immediate seg sym symreg)))
        (! symbol-ref target symreg arm64::symbol.vcell-cell))))
  (^))

;;; Donor: v2 arm64-p2-double-float-compare @7804.
(defarm642 arm642-double-float-compare double-float-compare (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
    (with-fp-target () (r1 :double-float)
      (with-fp-target (r1) (r2 :double-float)
        (multiple-value-bind (r1 r2) (arm642-two-untargeted-reg-forms seg form1 r1 form2 r2)
          (arm642-compare-double-float-registers seg vreg xfer r1 r2 cr-bit true-p))))))


;;; Donor: v2 arm64-p2-short-float-compare @7838.
(defarm642 arm642-short-float-compare short-float-compare (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
    (with-fp-target () (r1 :single-float)
      (with-fp-target (r1) (r2 :single-float)
        (multiple-value-bind (r1 r2) (arm642-two-untargeted-reg-forms seg form1 r1 form2 r2)
          (arm642-compare-single-float-registers seg vreg xfer r1 r2 cr-bit true-p))))))


;;; ---------------------------------------------------------------------------
;;; FP arith. Donor: v2 defarm64-p2-df-op / defarm64-p2-sf-op @7846-7878.
;;; Result boxing rides arm642-copy-register (double->heap / single->node).
(eval-when (:compile-toplevel :execute)
  (defmacro defarm642-df-op (fname opname vinsn)
    `(defarm642 ,fname ,opname (seg vreg xfer f0 f1)
       (if (null vreg)
         (progn
           (arm642-form seg nil nil f0)
           (arm642-form seg vreg xfer f1))
         (with-fp-target () (r1 :double-float)
           (with-fp-target (r1) (r2 :double-float)
             (multiple-value-bind (r1 r2) (arm642-two-untargeted-reg-forms seg f0 r1 f1 r2)
               (if (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (! ,vinsn vreg r1 r2)
                 (with-fp-target (r1 r2) (result :double-float)
                   (! ,vinsn result r1 r2)
                   (ensuring-node-target (target vreg)
                     (arm642-copy-register seg target result))))
               (^)))))))

  (defmacro defarm642-sf-op (fname opname vinsn)
    `(defarm642 ,fname ,opname (seg vreg xfer f0 f1)
       (if (null vreg)
         (progn
           (arm642-form seg nil nil f0)
           (arm642-form seg vreg xfer f1))
         (with-fp-target () (r1 :single-float)
           (with-fp-target (r1) (r2 :single-float)
             (multiple-value-bind (r1 r2) (arm642-two-untargeted-reg-forms seg f0 r1 f1 r2)
               (if (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (! ,vinsn vreg r1 r2)
                 (with-fp-target (r1 r2) (result :single-float)
                   (! ,vinsn result r1 r2)
                   (ensuring-node-target (target vreg)
                     (arm642-copy-register seg target result))))
               (^)))))))
)


(defarm642-df-op arm642-%double-float+-2 %double-float+-2 double-float+-2)

(defarm642-df-op arm642-%double-float--2 %double-float--2 double-float--2)

(defarm642-df-op arm642-%double-float*-2 %double-float*-2 double-float*-2)

(defarm642-df-op arm642-%double-float/-2 %double-float/-2 double-float/-2)


(defarm642-sf-op arm642-%short-float+-2 %short-float+-2 single-float+-2)

(defarm642-sf-op arm642-%short-float--2 %short-float--2 single-float--2)

(defarm642-sf-op arm642-%short-float*-2 %short-float*-2 single-float*-2)

(defarm642-sf-op arm642-%short-float/-2 %short-float/-2 single-float/-2)


;;; ---------------------------------------------------------------------------
;;; Precision / fixnum conversions (re-dispatch targets of %double-float /
;;; %single-float below).

;;; Donor: v2 arm64-p2-%double-to-single @9928. Vinsn renamed
;;; double-to-single â†’ copy-double-to-single (the name his
;;; arm642-copy-register already demands; one fcvt vinsn, not two).
(defarm642 arm642-%double-to-single %double-to-single (seg vreg xfer arg)
  (if (null vreg)
    (arm642-form seg vreg xfer arg)
    (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
      (let* ((dreg (arm642-one-untargeted-reg-form
                    seg arg
                    (make-wired-lreg (hard-regspec-value vreg)
                                     :class hard-reg-class-fpr
                                     :mode hard-reg-class-fpr-mode-double))))
        (! copy-double-to-single vreg dreg)
        (^))
      (with-fp-target () (argreg :double-float)
        (arm642-one-targeted-reg-form seg arg argreg)
        (with-fp-target () (sreg :single-float)
          (! copy-double-to-single sreg argreg)
          (<- sreg)
          (^))))))


;;; Donor: v2 arm64-p2-%single-to-double @9947. ARM64-DEVIATION from the
;;; donor: v2's untargeted leg re-moded the S lreg to double WITHOUT a
;;; convert ((<- (set-regspec-mode sreg ..-double))) â€” a PPC FPR artifact
;;; (PPC FPRs always hold double format). On AArch64 an S register holds
;;; IEEE single bits, so widening REQUIRES the fcvt; emit
;;; copy-single-to-double explicitly in both legs.
(defarm642 arm642-%single-to-double %single-to-double (seg vreg xfer arg)
  (if (null vreg)
    (arm642-form seg vreg xfer arg)
    (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
      (with-fp-target () (sreg :single-float)
        (arm642-one-targeted-reg-form seg arg sreg)
        (! copy-single-to-double vreg sreg)
        (^))
      (with-fp-target () (sreg :single-float)
        (arm642-one-targeted-reg-form seg arg sreg)
        (with-fp-target (sreg) (dreg :double-float)
          (! copy-single-to-double dreg sreg)
          (<- dreg)
          (^))))))


;;; Donor: v2 arm64-p2-%fixnum-to-double @9970.
(defarm642 arm642-%fixnum-to-double %fixnum-to-double (seg vreg xfer arg)
  (with-fp-target () (dreg :double-float)
    (let* ((r (arm642-one-untargeted-reg-form seg arg arm64::arg_z)))
      (unless (or (acode-fixnum-form-p arg)
 *arm642-reckless*)
        (! trap-unless-fixnum r))
      (! fixnum->fpr dreg r)
      (<- dreg)
      (^))))


;;; Donor: v2 single-float-reg-p (predicate used by %fixnum-to-single);
;;; arm642- prefix to avoid colliding with any host-backend definition.
(defun arm642-single-float-reg-p (reg)
  (and reg
       (= (hard-regspec-class reg) hard-reg-class-fpr)
       (= (get-regspec-mode reg) hard-reg-class-fpr-mode-single)))


;;; Donor: v2 arm64-p2-%fixnum-to-single @9980 (convert via double, then
;;; narrow â€” scvtfâ†’fcvt, exactly the donor's fixnum->fpr + double-to-single).
(defarm642 arm642-%fixnum-to-single %fixnum-to-single (seg vreg xfer arg)
  (with-fp-target () (dreg :double-float)
    (let* ((r (arm642-one-untargeted-reg-form seg arg arm64::arg_z)))
      (unless (or (acode-fixnum-form-p arg)
 *arm642-reckless*)
        (! trap-unless-fixnum r))
      (! fixnum->fpr dreg r)
      (if (arm642-single-float-reg-p vreg)
        (! copy-double-to-single vreg dreg)
        (with-fp-target (dreg) (sreg :single-float)
          (! copy-double-to-single sreg dreg)
          (<- sreg)))
      (^))))


;;; ---------------------------------------------------------------------------
;;; Coercion dispatchers. Donor: v2 arm64-p2-%double-float @9994 /
;;; arm64-p2-%single-float @10025 â€” constant-fold, else re-dispatch on the
;;; arg's known type, else call the runtime coercion function.
(defarm642 arm642-%double-float %double-float (seg vreg xfer arg)
  (let* ((real (or (acode-fixnum-form-p arg)
                   (let* ((form (acode-unwrapped-form-value arg)))
                     (if (and (acode-p form)
                              (eq (acode-operator form)
                                  (%nx1-operator immediate))
                              (typep (car (acode-operands form)) 'real))
                       (car (acode-operands form))))))
         (dconst (and real (ignore-errors (float real 0.0d0)))))
    (if dconst
      (arm642-immediate seg vreg xfer dconst)
      (if (arm642-form-typep arg 'single-float)
        (arm642-use-operator (%nx1-operator %single-to-double)
                             seg vreg xfer arg)
        (if (arm642-form-typep arg 'fixnum)
          (arm642-use-operator (%nx1-operator %fixnum-to-double)
                               seg vreg xfer arg)
          (arm642-use-operator (%nx1-operator call)
                               seg vreg xfer
                               (make-acode (%nx1-operator immediate)
                                           '%double-float)
                               (list nil (list arg))))))))


(defarm642 arm642-%single-float %single-float (seg vreg xfer arg)
  (let* ((real (or (acode-fixnum-form-p arg)
                   (let* ((form (acode-unwrapped-form-value arg)))
                     (if (and (acode-p form)
                              (eq (acode-operator form)
                                  (%nx1-operator immediate))
                              (typep (car (acode-operands form)) 'real))
                       (car (acode-operands form))))))
         (sconst (and real (ignore-errors (float real 0.0f0)))))
    (if sconst
      (arm642-immediate seg vreg xfer sconst)
      (if (arm642-form-typep arg 'double-float)
        (arm642-use-operator (%nx1-operator %double-to-single)
                             seg vreg xfer arg)
        (if (arm642-form-typep arg 'fixnum)
          (arm642-use-operator (%nx1-operator %fixnum-to-single)
                               seg vreg xfer arg)
          (arm642-use-operator (%nx1-operator call)
                               seg vreg xfer
                               (make-acode (%nx1-operator immediate)
                                           '%short-float)
                               (list nil (list arg))))))))


;;; ---------------------------------------------------------------------------
;;; %setf-double-float. Donor: v2 arm64-p2-%setf-double-float @9016
;;; (donor's wired fp1 target â†’ with-fp-target; donor's `vref' arg name
;;; normalized to vreg). No %setf-short-float sibling: singles are
;;; IMMEDIATES in his scheme (fulltag #b0001) â€” nothing to mutate â€” and
;;; the demand scan shows zero hits.
(defarm642 arm642-%setf-double-float %setf-double-float (seg vreg xfer fnode fval)
  (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg fnode arm64::arg_z))
  (with-fp-target () (target :double-float)
    (let* ((node ($ arm64::arg_z)))
      (arm642-one-targeted-reg-form seg fval target)
      (arm642-vpop-register seg node)
      (unless (or *arm642-reckless* (arm642-form-typep fnode 'double-float))
        (! trap-unless-double-float node))
      (! store-double node target)
      (<- node)))
  (^))


;;; ---------------------------------------------------------------------------
;;; Fixnum negate. Donor: v2 arm64-p2-%ineg @6577 / arm64-p2-%%ineg @6586.
;;; His arm642-check-fixnum-overflow (arm642.lisp:2000) consumes the negs
;;; flags pseudo (cond-vs; V flags the most-negative-fixnum case).
(defarm642 arm642-%ineg %ineg (seg vreg xfer n)
  (let* ((src (arm642-one-untargeted-reg-form seg n arm64::arg_z)))
    (when vreg
      (ensuring-node-target (target vreg)
        (let* ((flags (make-hard-crf-reg 0)))
          (! negate-fixnum-set-flags target flags src)
          (arm642-check-fixnum-overflow seg flags target))))
    (^)))


(defarm642 arm642-%%ineg %%ineg (seg vreg xfer n)
  (let* ((src (arm642-one-untargeted-reg-form seg n arm64::arg_z)))
    (when vreg
      (ensuring-node-target (target vreg)
        (! negate-fixnum-no-ovf target src)))
    (^)))


;;; ---------------------------------------------------------------------------
;;; list / nth-value. Donors: v2 arm64-p2-list @7170, arm64-p2-nth-value
;;; @7618. list/list* vinsns landed in w4; nth-value vinsn is w9.
;;; (list* handler not ported: zero demand-scan hits.)
(defarm642 arm642-list list (seg vreg xfer arglist)
  (if (null vreg)
    (dolist (form arglist)
      (arm642-form seg vreg nil form))
 ;; v2 additionally rebinds *top-vstack-lcell* â€” dropped, per the
 ;; lane's arm642-multiple-value-body (w2) convention.
    (let* ((*arm642-vstack* *arm642-vstack*)
           (nargs (arm642-formlist seg arglist nil)))
      (arm642-set-nargs seg nargs)
      (! list)
      (<- arm64::arg_z)))
  (^))



(defarm642 arm642-closed-function closed-function (seg vreg xfer afunc)
  (arm642-make-closure seg afunc nil)
  (when vreg (<- arm64::arg_z))
  (^))


(defarm642 arm642-flet flet (seg vreg xfer vars afuncs body p2decls)
  (if (dolist (afunc afuncs)
        (unless (eql 0 (afunc-fn-refcount afunc))
          (return t)))
    (arm642-seq-fbind seg vreg xfer vars afuncs body p2decls)
    (with-arm64-p2-declarations p2decls
      (arm642-form seg vreg xfer body))))


;;; labels: sibling of flet (forward-referencing variant). Donor @8303.
(defarm642 arm642-labels labels (seg vreg xfer vars afuncs body p2decls)
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
        (let* ((i 2)
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
      (arm642-seq-fbind seg vreg xfer (nreverse real-vars) (nreverse real-funcs) body p2decls)
      (let* ((old-stack (arm642-encode-stack)))
        (setq real-vars (nreverse real-vars) real-funcs (nreverse real-funcs))
        (with-arm64-p2-declarations p2decls
          (dolist (var real-vars)
            (arm642-seq-bind-var seg var (nx1-afunc-ref (pop real-funcs))))
          (dolist (ref fwd-refs)
            (let ((ea (var-ea (pop ref))))
              (arm642-addrspec-to-reg seg ea arm64::temp0)
              (dolist (r ref)
                (let* ((v-ea (var-ea (cdr r))))
                  (let* ((val-reg (if (eq v-ea ea)
                                    arm64::temp0
                                    (progn
                                      (arm642-addrspec-to-reg seg v-ea arm64::temp1)
                                      arm64::temp1))))
 ;; NOT the donor's misc-set-c-node: temp0 is
 ;; function-tagged here (tag-as-function ran inside
 ;; make-closure), and misc addressing off a tag-7
 ;; pointer lands +3 past the slot ( root).
 ;; x8664 (split tags like this port) uses this
 ;; dedicated vinsn â€” x862.lisp:10053.
                    (! set-closure-forward-reference val-reg arm64::temp0 (car r)))))))
          (arm642-undo-body seg vreg xfer body old-stack)
          (dolist (var real-vars)
            (arm642-close-var seg var)))))))



(defarm642 arm642-%scharcode %scharcode (seg vreg xfer str idx)
  (multiple-value-bind (src unscaled-idx)
      (arm642-two-untargeted-reg-forms seg str arm64::arg_y idx arm64::arg_z)
    (if vreg
      (ensuring-node-target (target vreg)
        (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
          (256 (! %scharcode8 target src unscaled-idx))
          (t (! %scharcode32 target src unscaled-idx)))))
    (^)))


(defarm642 arm642-%set-scharcode %set-scharcode (seg vreg xfer str idx char)
  (multiple-value-bind (src unscaled-idx char)
                       (arm642-three-untargeted-reg-forms seg str arm64::arg_x idx arm64::arg_y
                                                          char arm64::arg_z)
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256 (! %set-scharcode8 src unscaled-idx char))
      (t (! %set-scharcode32 src unscaled-idx char)))
    (when vreg (<- char))
    (^)))


(defarm642 arm642-code-char code-char (seg vreg xfer c)
  (let* ((reg (arm642-one-untargeted-reg-form seg c arm64::arg_z)))
 ;; Typecheck even if result unused.
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256 (! require-u8 reg))
      (t (! require-char-code reg)))
    (if vreg
      (ensuring-node-target (target vreg)
        (! fixnum->char target reg)))
    (^)))


(defarm642 arm642-%valid-code-char %valid-code-char (seg vreg xfer c)
  (let* ((reg (arm642-one-untargeted-reg-form seg c arm64::arg_z)))
    (when *arm642-full-safety* (! require-char-code reg))
    (if vreg
      (ensuring-node-target (target vreg)
        (! code-char->char target reg)))
    (^)))


;;; ---------------------------------------------------------------------------
;;; int>0-p â€” PPC64 ppc2.lisp:7562: .SPinteger-sign puts the sign in
;;; imm0 (unboxed -1/0/1... boxed? PPC64's integer-sign subprim returns
;;; a FIXNUM in imm0, and test-reg-%izerop compares it against 0 â€”
;;; carried). test-reg-%izerop landed in w1.
(defarm642 arm642-int>0-p int>0-p (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
    (arm642-one-targeted-reg-form seg form ($ arm64::arg_z))
    (! integer-sign)
    (arm642-test-reg-%izerop seg vreg xfer arm64::imm0 cr-bit true-p 0)))


;;; %setf-macptr â€” PPC64 ppc2.lisp:8098.
(defarm642 arm642-%setf-macptr %setf-macptr (seg vreg xfer x y)
  (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg x arm64::arg_z))
  (with-imm-target () (src-reg :address)
    (arm642-one-targeted-reg-form seg y src-reg)
    (arm642-vpop-register seg arm64::arg_z)
    (unless (or *arm642-reckless* (arm642-form-typep x 'macptr))
      (with-imm-temps (src-reg) ()
        (! trap-unless-macptr arm64::arg_z)))
    (! set-macptr-address src-reg arm64::arg_z)
    (<- arm64::arg_z)
    (^)))


;;; %fixnum-ref-natural â€” PPC64 ppc2.lisp:7541. lisp-word-ref-c (w4) /
;;; lisp-word-ref (index leg) â€” check the w4 constant-offset variant's
;;; contract before assuming; the reg-index leg mirrors the donor's
;;; fixnum->signed-natural + lisp-word-ref pair.
(defarm642 arm642-%fixnum-ref-natural %fixnum-ref-natural (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (arm642-form seg nil nil base)
           (arm642-form seg nil xfer offset))
 ;; Reach gate matches lisp-word-ref-c's two encodable
 ;; windows (w4): scaled LDR needs â‰¥0 AND 8-aligned; 8k-4
 ;; slot constants (HIS misc-data-offset=-4) take LDUR simm9.
          ((and fixoffset
                (if (and (>= fixoffset 0) (eql 0 (logand fixoffset 7)))
                  (<= fixoffset 32760)
                  (and (>= fixoffset -256) (<= fixoffset 255))))
           (with-imm-target () (val :natural)
             (! lisp-word-ref-c val
                (arm642-one-untargeted-reg-form seg base arm64::arg_z)
                fixoffset)
             (<- val))
           (^))
          (t (multiple-value-bind (breg oreg)
                 (arm642-two-untargeted-reg-forms seg base arm64::arg_y offset arm64::arg_z)
               (with-imm-target () (otemp :s64)
                 (! fixnum->signed-natural otemp oreg)
                 (with-imm-target () (val :natural)
                   (! lisp-word-ref val breg otemp)
                   (<- val)))
               (^))))))


;;; ---------------------------------------------------------------------------
;;; arm642-macptr->heap â€” his arm642-copy-register's addressâ†’node leg
;;; calls this helper (arm642.lisp:1066) but nothing defines it. x862
;;; splits it into three vinsns (@5005); ours is the single PPC64-shaped
;;; macptr->heap vinsn (w9).
(defun arm642-macptr->heap (seg dest src)
  (with-arm64-local-vinsn-macros (seg)
    (! macptr->heap dest src)))


;;; ---------------------------------------------------------------------------
;;; macptr / markers. Donors: PPC64 ppc2.lisp @7868/@7885/@7907
;;; (= v2 @8774/@8791/@8813).
(defarm642 arm642-%consmacptr% %consmacptr% (seg vreg xfer form)
  (cond ((null vreg) (arm642-form seg nil xfer form))
        ((eql (get-regspec-mode vreg) hard-reg-class-gpr-mode-address)
         (arm642-form seg vreg xfer form))
        (t
         (with-imm-target () (temp :address)
           (<- (arm642-one-targeted-reg-form seg form temp))
           (^)))))


(defarm642 arm642-%immediate-ptr-to-int %immediate-ptr-to-int (seg vreg xfer form)
  (if (null vreg)
    (arm642-form seg nil xfer form)
    (with-imm-target () (address-reg :address)
      (arm642-form seg address-reg nil form)
      (<- (set-regspec-mode address-reg (gpr-mode-name-value :natural)))
      (^))))


(defarm642 arm642-%immediate-int-to-ptr %immediate-int-to-ptr (seg vreg xfer form)
  (if (null vreg)
    (arm642-form seg nil xfer form)
    (progn
      (unless (logbitp (hard-regspec-value vreg) arm64-imm-regs)
        (compiler-bug "I give up.  When will I get this right ?"))
      (let* ((natural-reg (arm642-one-targeted-reg-form seg
                                                        form
                                                        ($ vreg :mode :natural))))
        (<- natural-reg)
        (^)))))


(defarm642 arm642-%unbound-marker %unbound-marker (seg vreg xfer)
  (when vreg
    (ensuring-node-target (target vreg)
      (arm642-lri seg target arm64::unbound-marker)))
  (^))


(defarm642 arm642-slot-unbound-marker %slot-unbound-marker (seg vreg xfer)
  (when vreg
    (ensuring-node-target (target vreg)
      (arm642-lri seg target arm64::slot-unbound-marker)))
  (^))


;;; ---------------------------------------------------------------------------
;;; Symbol cluster. Handler shape = v2 %symbol->symptr @9922 (untargeted
;;; src + ensuring-node-target); the IDENTITY bodies v2/PPC64 used for
;;; symptr<->symvector are a high-tag artifact â€” these route through the
;;; w9 retag vinsns (x8664 canon; see vinsn file).
(defarm642 arm642-%symbol->symptr %symbol->symptr (seg vreg xfer sym)
  (let* ((src (arm642-one-untargeted-reg-form seg sym arm64::arg_z)))
    (ensuring-node-target (target vreg)
      (! %symbol->symptr target src))
    (^)))


(defarm642 arm642-%symptr->symvector %symptr->symvector (seg vreg xfer arg)
  (let* ((src (arm642-one-untargeted-reg-form seg arg arm64::arg_z)))
    (ensuring-node-target (target vreg)
      (! %symptr->symvector target src))
    (^)))


(defarm642 arm642-%symvector->symptr %symvector->symptr (seg vreg xfer arg)
  (let* ((src (arm642-one-untargeted-reg-form seg arg arm64::arg_z)))
    (ensuring-node-target (target vreg)
      (! %symvector->symptr target src))
    (^)))


;;; %debug-trap â€” donor v2 @9678 (= ppc2:5527 shape).
(defarm642 arm642-%debug-trap %debug-trap (seg vreg xfer arg)
  (arm642-one-targeted-reg-form seg arg ($ arm64::arg_z))
  (! %debug-trap)
  (<- ($ arm64::arg_z))
  (^))


;;; inherited-arg â€” surfaced as the shared next wall once flet went live
;;; (l0-float/l0-numbers/l0-bignum64: closures referencing an inherited
;;; var). Donor: v2 @8392; ea-open/addrspec-to-reg landed in w2.
(defarm642 arm642-inherited-arg inherited-arg (seg vreg xfer arg)
  (when vreg
    (arm642-addrspec-to-reg seg (arm642-ea-open (var-ea arg)) vreg))
  (^))


(defarm642 arm642-nth-value nth-value (seg vreg xfer n form)
 ;; lcell rebind dropped (lane convention, see arm642-list above).
  (let* ((*arm642-vstack* *arm642-vstack*))
    (let* ((nreg (arm642-one-untargeted-reg-form seg n arm64::arg_z)))
      (unless (acode-fixnum-form-p n)
        (! trap-unless-fixnum nreg))
      (arm642-vpush-register seg nreg))
    (arm642-multiple-value-body seg form)  ; sets nargs
    (! nth-value arm64::arg_z))
  (<- arm64::arg_z)
  (^))


;;; ---------------------------------------------------------------------------
;;; %single-float-negate / %double-float-negate. Donor: ppc2.lisp:6380 and
;;; :6366 -- the two bodies are identical apart from the precision, and this
;;; is a line-port of both.
;;;
;;; . acode-handler-coverage.py listed %single-float-negate as an
;;; ALL-THREE gap (ppc64, arm32, x8664 all handle it; arm642 did not), and
;;; %double-float-negate sat in its VERIFY bucket only because the SUBSTRING
;;; matched %double-float-negate! -- the level-0 LAP function, a different
;;; thing. Neither operator had a handler.
;;;
;;; Reaching an unhandled operator is not a slow path, it is
;;; Compiler bug or inconsistency:
;;; arm642-form ? #<ACODE %single-float-negate (#<ACODE lexical-reference
;;; (#<VAR RRAD>)>)>
;;; which is what ANSI ROUND/FROUND.13/15/17/19 hit: they assert
;;; (eql r (- rrad)) over a known single-float, nx lowers `(- x)' on a
;;; single-float to %single-float-negate (compiler/nx2.lisp:427,
;;; acode-rewrite.lisp:399), and the deftest therefore failed to COMPILE.
;;; Their even-numbered siblings compare against rrad with no negation and
;;; passed all along -- so the odd/even split in the failure list is about
;;; the ASSERTION, not about negative-remainder arithmetic.
;;;
;;; PPC emits its DOUBLE vinsn for both precisions (a PPC FPR is 64-bit and
;;; fneg only flips the sign bit). Here each precision has its own vinsn --
;;; AArch64 encodes fneg per precision and the vinsn operand classes differ
;;; with it -- so the single handler emits single-float-negate. The
;;; register-class/mode test, the with-fp-target scratch and the
;;; ensuring-node-target boxing fallback are PPC's, unchanged: if VREG is
;;; already an FPR of the right precision, negate straight into it;
;;; otherwise negate into a scratch and let arm642-copy-register box it.
(defarm642 arm642-%single-float-negate %single-float-negate (seg vreg xfer form)
  (with-fp-target () (r1 :single-float)
    (setq r1 (arm642-one-untargeted-reg-form seg form r1))
    (if (and vreg
             (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
      (! single-float-negate vreg r1)
      (with-fp-target (r1) (r2 :single-float)
        (! single-float-negate r2 r1)
        (ensuring-node-target (target vreg)
          (arm642-copy-register seg target r2))))
    (^)))


(defarm642 arm642-%double-float-negate %double-float-negate (seg vreg xfer form)
  (with-fp-target () (r1 :double-float)
    (setq r1 (arm642-one-untargeted-reg-form seg form r1))
    (if (and vreg
             (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
      (! double-float-negate vreg r1)
      (with-fp-target (r1) (r2 :double-float)
        (! double-float-negate r2 r1)
        (ensuring-node-target (target vreg)
          (arm642-copy-register seg target r2))))
    (^)))

;;; Donor: ppc2.lisp:7861.
(defarm642 arm642-%macptrptr% %macptrptr% (seg vreg xfer form)
  (with-imm-target () (target :address)
    (arm642-one-targeted-reg-form seg form (or vreg target)))
  (^))


;;; Donor: ppc2.lisp:8265.
(defarm642 arm642-%immediate-set-xxx %immediate-set-xxx (seg vreg xfer bits ptr offset val)
  (arm642-%immediate-store seg vreg xfer bits ptr offset val))


;;; Donors: ppc2.lisp:6606 (%aref1) / :6623 (%aset1) — the rank-1
;;; general array ref/set: known simple-1d ctype inlines through the w3
;;; vref/vset chain, else the %aref1/%aset1 builtins. The l0-int wall
;;; (realgate-w9i frontier #3); w7's general-aref2/3 are the rank-2/3
;;; precedent for this shape.
(defarm642 arm642-%aref1 %aref1 (seg vreg xfer v i)
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
      (arm642-vref seg vreg xfer keyword v i (not *arm642-reckless*))
      (arm642-binary-builtin seg vreg xfer '%aref1 v i))))


(defarm642 arm642-%aset1 aset1 (seg vreg xfer v i n)
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
      (arm642-vset seg vreg xfer keyword v i n (not *arm642-reckless*))
      (arm642-ternary-builtin seg vreg xfer '%aset1 v i n))))


;;; Donor: ppc2.lisp:7208 immediate-get-xxx — the load twin of
;;; %immediate-store above; the l0-utils wall after w10 batch 1
;;; (realgate-w10a). Same deviations 1/2 (no absptr fast path; simm9
;;; LDUR window, alignment resets dropped); the non-trivial-offset legs
;;; take arm2's NFP push/pop shape (@8008, deviation 4) instead of
;;; ppc2's tstack temp-push — arm2 also drops the open-undo pair there
;;; (NFP frames are function-scoped, not undo-scoped).
(defarm642 arm642-immediate-get-xxx immediate-get-xxx (seg vreg xfer bits ptr offset)
  (declare (fixnum bits))
  (let* ((fixnump (logbitp 6 bits))
         (signed (logbitp 5 bits))
         (size (logand 15 bits))
         (triv-p (arm642-trivial-p offset))
         (offval (acode-fixnum-form-p offset)))
    (declare (fixnum size))
    (cond ((null vreg)
           (arm642-form seg nil nil ptr)
           (arm642-form seg nil xfer offset))
          (t
           (and offval (%i> (integer-length offval) 8) (setq offval nil))
           (cond
             (fixnump
              (with-imm-target () (dest :signed-natural)
                (cond
                  (offval
                    (with-imm-target () (src-reg :address)
                      (arm642-one-targeted-reg-form seg ptr src-reg)
                      (! mem-ref-c-doubleword dest src-reg offval)))
                  (t
                   (with-imm-target () (src-reg :address)
                     (with-imm-target (src-reg) (offset-reg :signed-natural)
                       (arm642-one-targeted-reg-form seg ptr src-reg)
                       (if triv-p
                         (if (acode-fixnum-form-p offset)
                           (arm642-lri seg offset-reg (acode-fixnum-form-p offset))
                           (! fixnum->signed-natural offset-reg (arm642-one-untargeted-reg-form seg offset arm64::arg_z)))
                         (let* ((*arm642-nfp-depth* *arm642-nfp-depth*))
                           (arm642-push-register seg src-reg)
                           (! fixnum->signed-natural offset-reg (arm642-one-untargeted-reg-form seg offset arm64::arg_z))
                           (arm642-pop-register seg src-reg)))
                       (! mem-ref-doubleword dest src-reg offset-reg)))))
                (if (node-reg-p vreg)
                  (! box-fixnum vreg dest)
                  (<- dest))))
             (signed
              (with-imm-target () (dest :signed-natural)
               (cond
                 (offval
                  (with-imm-target (dest) (src-reg :address)
                   (arm642-one-targeted-reg-form seg ptr src-reg)
                     (case size
                       (8 (! mem-ref-c-signed-doubleword dest src-reg offval))
                       (4 (! mem-ref-c-signed-fullword dest src-reg offval))
                       (2 (! mem-ref-c-s16 dest src-reg offval))
                       (1 (! mem-ref-c-s8 dest src-reg offval)))))
                 (t
                  (with-imm-target () (src-reg :address)
                    (with-imm-target (src-reg) (offset-reg :signed-natural)
                     (arm642-one-targeted-reg-form seg ptr src-reg)
                     (if triv-p
                       (if (acode-fixnum-form-p offset)
                         (arm642-lri seg offset-reg (acode-fixnum-form-p offset))
                         (! fixnum->signed-natural offset-reg (arm642-one-untargeted-reg-form seg offset arm64::arg_z)))
                       (let* ((*arm642-nfp-depth* *arm642-nfp-depth*))
                         (arm642-push-register seg src-reg)
                         (! fixnum->signed-natural offset-reg (arm642-one-untargeted-reg-form seg offset arm64::arg_z))
                         (arm642-pop-register seg src-reg)))
                  (case size
                    (8 (! mem-ref-signed-doubleword dest src-reg offset-reg))
                    (4 (! mem-ref-signed-fullword dest src-reg offset-reg))
                    (2 (! mem-ref-s16 dest src-reg offset-reg))
                    (1 (! mem-ref-s8 dest src-reg offset-reg)))))))
               (if (node-reg-p vreg)
                 (case size
                   ((1 2) (! box-fixnum vreg dest))
                   (4 (! box-fixnum vreg dest))
                   (8 (<- dest)))
                 (<- dest))))
             (t
              (with-imm-target () (dest :natural)
               (cond
                 (offval
                  (with-imm-target (dest) (src-reg :address)
                    (arm642-one-targeted-reg-form seg ptr src-reg)
                    (case size
                      (8 (! mem-ref-c-doubleword dest src-reg offval))
                      (4 (! mem-ref-c-fullword dest src-reg offval))
                      (2 (! mem-ref-c-u16 dest src-reg offval))
                      (1 (! mem-ref-c-u8 dest src-reg offval)))))
                 (t
                  (with-imm-target () (src-reg :address)
                    (with-imm-target (src-reg) (offset-reg :signed-natural)
                     (arm642-one-targeted-reg-form seg ptr src-reg)
                     (if triv-p
                       (if (acode-fixnum-form-p offset)
                         (arm642-lri seg offset-reg (acode-fixnum-form-p offset))
                         (! fixnum->signed-natural offset-reg (arm642-one-untargeted-reg-form seg offset arm64::arg_z)))
                       (let* ((*arm642-nfp-depth* *arm642-nfp-depth*))
                         (arm642-push-register seg src-reg)
                         (! fixnum->signed-natural offset-reg (arm642-one-untargeted-reg-form seg offset arm64::arg_z))
                         (arm642-pop-register seg src-reg)))
                  (case size
                    (8 (! mem-ref-doubleword dest src-reg offset-reg))
                    (4 (! mem-ref-fullword dest src-reg offset-reg))
                    (2 (! mem-ref-u16 dest src-reg offset-reg))
                    (1 (! mem-ref-u8 dest src-reg offset-reg)))))))
                  (<- (set-regspec-mode
                       dest
                       (gpr-mode-name-value
                        (case size
                          (8 :u64)
                          (4 :u32)
                          (2 :u16)
                          (1 :u8))))))))
           (^)))))


;;; ---------------------------------------------------------------------------
;;; Class-G codegen cluster: unwind-protect / catch / throw / progv.
;;; Donors: ppc2.lisp:8136/:7588/:7569/:8174, lcell lines STRIPPED (w2
;;; precedent — this lane has no lcells; the vstack accounting keeps
;;; the donor's arm642-adjust-vstack deltas, which is what the Class-G
;;; arch doc says actually matters: binding-frame pops are the
;;; COMPILER's vstack bookkeeping). comms/CATCH-THROW-BIND-VSTACK-ARCH
;;; read per standing directive; that doc's runtime findings are v2's
;;; (tstack catch frames, 0x38060 tail) — the CODEGEN contract here is
;;; the PPC64 shape against OUR spentry-C (mkcatch decodes ONE forward
;;; B at [lr]; nmkunwind binds *interrupt-level* pushing a 3-word
;;; binding frame = the two adjust-vstack deltas below).
;;; The cleanup-context cstack accounting uses this lane's lisp-frame
;;; size 32 (marker frame; spentry-A:56 layout).
(defarm642 arm642-unwind-protect unwind-protect (seg vreg xfer protected-form cleanup-form)
  (let* ((cleanup-label (backend-get-next-label))
         (protform-label (backend-get-next-label))
         (old-stack (arm642-encode-stack)))
    (! nmkunwind)
    (arm642-open-undo $undointerruptlevel)
    (arm642-adjust-vstack (* 3 *arm642-target-node-size*))
    (! non-barrier-jump (aref *backend-labels* cleanup-label))
    (-> protform-label)
    (@ cleanup-label)
    (let* ((*arm642-vstack* *arm642-vstack*)
           (*arm642-cstack* (%i+ *arm642-cstack* 32)))
      (arm642-open-undo $undostkblk)      ; tsp frame created by nthrow.
      (! save-cleanup-context)
      (setq *arm642-cstack* (%i+ *arm642-cstack* 32)) ; the frame we just pushed
      (arm642-form seg nil nil cleanup-form)
      (arm642-close-undo)
      (! restore-cleanup-context)
      (! jump-return-pc))
    (arm642-open-undo)
    (@ protform-label)
    (arm642-adjust-vstack (* 3 *arm642-target-node-size*))
    (arm642-undo-body seg vreg xfer protected-form old-stack)))


;;; ppc2:7588. "Use a vinsn other than JUMP to reference the label" —
;;; the mkcatch subprim returns PAST the non-barrier-jump (lr+4); the
;;; jump is only reached as the throw target (donor comment carried).
(defarm642 arm642-catch catch (seg vreg xfer tag valform)
  (let* ((tag-label (backend-get-next-label))
         (mv-pass (arm642-mv-p xfer)))
    (arm642-one-targeted-reg-form seg tag ($ arm64::arg_z))
    (if mv-pass
      (! mkcatchmv)
      (! mkcatch1v))
    (! non-barrier-jump (aref *backend-labels* tag-label))
    (arm642-open-undo)
    (if mv-pass
      (arm642-multiple-value-body seg valform)
      (arm642-one-targeted-reg-form seg valform ($ arm64::arg_z)))
    (arm642-lri seg arm64::imm0 (ash 1 *arm642-target-fixnum-shift*))
    (if mv-pass
      (! nthrowvalues)
      (! nthrow1value))
    (arm642-close-undo)
    (@ tag-label)
    (unless mv-pass (if vreg (<- arm64::arg_z)))
    (let* ((*arm642-returning-values* mv-pass)) ; nlexit keeps values on stack
      (^))))


;;; ppc2:7569.
(defarm642 arm642-throw throw (seg vreg xfer tag valform)
  (declare (ignorable vreg xfer))
  (let* ((*arm642-vstack* *arm642-vstack*))
    (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg tag arm64::arg_z))
    (if (arm642-trivial-p valform)
      (progn
        (arm642-vpush-register seg (arm642-one-untargeted-reg-form seg valform arm64::arg_z))
        (arm642-set-nargs seg 1))
      (arm642-multiple-value-body seg valform))
    (! throw)))


;;; ppc2:8174.
(defarm642 arm642-progv progv (seg vreg xfer symbols values body)
  (let* ((cleanup-label (backend-get-next-label))
         (protform-label (backend-get-next-label))
         (old-stack (arm642-encode-stack)))
    (arm642-two-targeted-reg-forms seg symbols ($ arm64::arg_y) values ($ arm64::arg_z))
    (! progvsave)
    (arm642-open-undo $undostkblk)
    (! mkunwind)
    (! non-barrier-jump (aref *backend-labels* cleanup-label))
    (-> protform-label)
    (@ cleanup-label)
    (! progvrestore)
    (arm642-open-undo)
    (@ protform-label)
    (arm642-undo-body seg vreg xfer body old-stack)))


;;; Donor: ppc2.lisp:5882 — the unchecked twin of w9's code-char
;;; (u32->char landed w3b). l0-io wall (realgate-w10e).
(defarm642 arm642-%code-char %code-char (seg vreg xfer c)
  (if (null vreg)
    (arm642-form seg nil xfer c)
    (progn
      (ensuring-node-target (target vreg)
        (with-imm-target () (dest :u8)
          (! u32->char target (arm642-one-untargeted-reg-form seg c dest))))
      (^))))


;;; Donor: ppc2.lisp:7923 (w9 marker-handler idiom; arm64::illegal-marker
;;; = his arch @184). The l0-hash wall after w10 batch 1.
(defarm642 arm642-illegal-marker %illegal-marker (seg vreg xfer)
  (when vreg
    (ensuring-node-target (target vreg)
      (arm642-lri seg target arm64::illegal-marker)))
  (^))


;;; Donor: ppc2.lisp:8909. The l0-def wall after w10 batch 1.
(defarm642 arm642-%current-tcr %current-tcr (seg vreg xfer)
  (when vreg
    (ensuring-node-target (target vreg)
      (! %current-tcr target)))
  (^))


;;; Donor: arm2.lisp:9018 (deviation 4: NFP shape, this lane's
;;; push/pop-register; ppc2:8270 uses tstack temp-push-unboxed-word).
;;; Constant window = the landed add-immediate contract, (signed-byte
;;; 24) of the BYTE offset (w2; arm2 gates on encode-arm-immediate,
;;; its own ISA's window).
(defarm642 arm642-%immediate-inc-ptr %immediate-inc-ptr (seg vreg xfer ptr by)
  (let* ((triv-by (arm642-trivial-p by))
         (fixnum-by (acode-fixnum-form-p by)))
    (if (and fixnum-by (eql 0 fixnum-by))
      (arm642-form seg vreg xfer ptr)
      (with-imm-target () (ptr-reg :address)
        (arm642-one-targeted-reg-form seg ptr ptr-reg)
        (if (setq fixnum-by (and fixnum-by
                                 (typep fixnum-by '(signed-byte 24))
                                 fixnum-by))
          (with-imm-target (ptr-reg) (result :address)
            (! add-immediate result ptr-reg fixnum-by)
            (<- result))
          (let* ((*arm642-nfp-depth* *arm642-nfp-depth*))
            (unless triv-by
              (arm642-push-register seg ptr-reg))
            (with-imm-target (ptr-reg) (by-reg :s32)
              (let* ((mask *available-backend-imm-temps*)
                     (*available-backend-imm-temps* mask))
                (when triv-by
                  (use-imm-temp (%hard-regspec-value ptr-reg)))
                (arm642-one-targeted-reg-form seg by by-reg)
                (setq *available-backend-imm-temps* mask)
                (unless triv-by
                  (arm642-pop-register seg ptr-reg))
                (with-imm-target () (result :address)
                  (! fixnum-add result ptr-reg by-reg)
                  (<- result))))))
        (^)))))

;;; Donor: ppc2.lisp:6348 ppc2-list* (v2 arm64-p2.lisp:7182 is the
;;; identical shape). Lcell rebinding dropped (w2 precedent â€” this
;;; lane has no lcells); otherwise line-for-line. For value:
;;; arm642-arglist evaluates/pushes the args and returns the count;
;;; nargs>1 sets nargs to the number of CONSED pairs (1- nargs) and
;;; calls the `list*` vinsn (arm64-vinsns-additions-w4.lisp:205,
;;; subprim dispatch to .SPconslist-star â€” registered in
;;; arm64-subprims-additions.lisp); nargs=1 means (list* x) = x,
;;; already in arg_z. Not for value: evaluate args for effect only.
(defarm642 arm642-list* list* (seg vreg xfer arglist)
  (if (null vreg)
    (dolist (arg (apply #'append arglist))
      (arm642-form seg nil nil arg))
    (let* ((*arm642-vstack* *arm642-vstack*)
           (nargs (arm642-arglist seg arglist)))
      (declare (fixnum nargs))
      (when (> nargs 1)
        (arm642-set-nargs seg (1- nargs))
        (! list*))
      (<- arm64::arg_z)))
  (^))


;;; %fixnum-ref â€” donor ppc2.lisp:7522 (v2 arm64-p2.lisp:8428
;;; identical). The node-target twin of the ALREADY-LANDED
;;; %fixnum-ref-natural (arm642-additions-w9.lisp:498) â€” every leg
;;; contract copied from it verbatim (same reach gate, raw byte
;;; offset into lisp-word-ref-c's ldr/ldur pred split, otemp :s64
;;; per w9 not v2's :s32): only the destination differs
;;; (ensuring-node-target instead of the :natural imm target).
;;; Gate = the two encodable windows (see lisp-word-ref-c, w4):
;;; 8-aligned non-negative â‡’ scaled LDR (â‰¤32760), anything else â‡’
;;; LDUR simm9 (-256..255) â€” raw slot constants are 8k-4 under HIS
;;; misc-data-offset=-4.
(defarm642 arm642-%fixnum-ref %fixnum-ref (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (arm642-form seg nil nil base)
           (arm642-form seg nil xfer offset))
          ((and fixoffset
                (if (and (>= fixoffset 0) (eql 0 (logand fixoffset 7)))
                  (<= fixoffset 32760)
                  (and (>= fixoffset -256) (<= fixoffset 255))))
           (ensuring-node-target (target vreg)
             (! lisp-word-ref-c target
                (arm642-one-untargeted-reg-form seg base arm64::arg_z)
                fixoffset))
           (^))
          (t (multiple-value-bind (breg oreg)
                 (arm642-two-untargeted-reg-forms seg base arm64::arg_y offset arm64::arg_z)
               (with-imm-target () (otemp :s64)
                 (! fixnum->signed-natural otemp oreg)
                 (ensuring-node-target (target vreg)
                   (! lisp-word-ref target breg otemp)))
               (^))))))


;;; global-setq â€” donor x862.lisp x862-global-setq @11582 ( REBASE
;;; from the ppc2:8878 shape, sibling of the global-ref fix in w8): the
;;; vset path does misc-tagged slot math, so the symbol-tagged constant
;;; must be retagged through %symptr->symvector first (handler+vinsn =
;;; w9 symbol cluster). ppc2 passes the symbol raw only because PPC
;;; symbols are misc-tagged; under fulltag-symbol=7 that WRITES [sym+4],
;;; corrupting the pname/vcell bytes â€” write-side twin of the 
;;; SPECIFIER-TYPE wall. target-arch-case folded â€” backend is :arm64.
(defarm642 arm642-global-setq global-setq (seg vreg xfer sym val)
  (arm642-vset seg
               vreg
               xfer
               :symbol
               (make-acode (%nx1-operator %symptr->symvector)
                           (make-acode (%nx1-operator immediate) sym))
               (make-acode (%nx1-operator fixnum) arm64::symbol.vcell-cell)
               val
               nil))


;;; %ptr-eql â€” donor ppc2.lisp:8190 (v2 arm64-p2.lisp:9128 identical
;;; shape). Two lane reshapes, both w10-precedented:
;;; 1. ppc2's tstack temp-push/pop-unboxed-word pair -> NFP
;;; arm642-push-register/arm642-pop-register with *arm642-nfp-depth*
;;; rebinding and NO open-undo (w10 immediate-get-xxx deviation 4 â€”
;;; NFP frames are function-scoped, not undo-scoped).
;;; 2. The constant-comparand fast path adds the lane's
;;; arm642-add-sub-imm-single-insn-p gate on top of the donor's
;;; (signed-byte 16) (w1 arm642-compare precedent â€” cmp-immediate
;;; window is A64 aimm, not PPC's full s16).
;;; use-imm-temp takes the hard regspec VALUE in this lane (w7:249,
;;; w10:652), not the raw regspec.
(defarm642 arm642-%ptr-eql %ptr-eql (seg vreg xfer cc x y)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil x)
      (arm642-form seg nil xfer y))
    (let* ((x-abs (acode-absolute-ptr-p x t))
           (y-abs (acode-absolute-ptr-p y t))
           (abs (or x-abs y-abs))
           (other (if abs (if x-abs y x))))
      (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-cond-bit cc)
        (if other
          (with-imm-target () (other-target :address)
            (arm642-one-targeted-reg-form seg other other-target)
            (if (and (typep abs '(signed-byte 16))
                     (arm642-add-sub-imm-single-insn-p abs))
              (arm642-test-reg-%izerop seg vreg xfer other-target cr-bit true-p abs)
              (with-imm-temps (other-target) ((abs-target :address))
                (use-imm-temp (hard-regspec-value other-target))
                (arm642-lri seg abs-target abs)
                (arm642-compare-registers seg vreg xfer other-target abs-target cr-bit true-p))))
 ;; Neither expression is obviously a constant-valued macptr.
          (with-imm-target () (target-a :address)
            (arm642-one-targeted-reg-form seg x target-a)
            (let* ((*arm642-nfp-depth* *arm642-nfp-depth*))
              (arm642-push-register seg target-a)
              (arm642-one-targeted-reg-form seg y target-a)
              (with-imm-target (target-a) (target-b :address)
                (arm642-pop-register seg target-b)
                (arm642-compare-registers seg vreg xfer target-b target-a cr-bit true-p)))))))))


;;; immediate-get-ptr â€” donor ppc2.lisp:7123 (v2 arm64-p2.lisp:8008).
;;; Structured on the landed immediate-get-xxx (w10:399, the sibling
;;; that loads non-pointer immediates) â€” all three w10 deviations
;;; carried:
;;; 1. NO absolute-pointer fast path (v2's `mem-ref-c-natural dest
;;; arm64::xzr absptr` puts reg 31 in a BASE slot = SP, not xzr â€”
;;; the documented misencode; the general path materializes the
;;; constant ptr the same way).
;;; 2. Constant-offset window = LDUR simm9 (integer-length <= 8), not
;;; PPC's 16-bit D-form.
;;; 3. Non-trivial offset leg = NFP push/pop, no open-undo.
;;; Result flows through an explicit :address imm target + (<-) so a
;;; node vreg gets the macptr box via copy-register (w9
;;; arm642-macptr->heap seam), instead of v2's raw `dest vreg` write.
(defarm642 arm642-immediate-get-ptr immediate-get-ptr (seg vreg xfer ptr offset)
  (let* ((triv-p (arm642-trivial-p offset))
         (offval (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (arm642-form seg nil nil ptr)
           (arm642-form seg nil xfer offset))
          (t
           (and offval (%i> (integer-length offval) 8) (setq offval nil))
           (with-imm-target () (dest :address)
             (cond
               (offval
                (with-imm-target (dest) (src-reg :address)
                  (arm642-one-targeted-reg-form seg ptr src-reg)
                  (! mem-ref-c-address dest src-reg offval)))
               (t
                (with-imm-target () (src-reg :address)
                  (with-imm-target (src-reg) (offset-reg :signed-natural)
                    (arm642-one-targeted-reg-form seg ptr src-reg)
                    (if triv-p
                      (if (acode-fixnum-form-p offset)
                        (arm642-lri seg offset-reg (acode-fixnum-form-p offset))
                        (! fixnum->signed-natural offset-reg (arm642-one-untargeted-reg-form seg offset arm64::arg_z)))
                      (let* ((*arm642-nfp-depth* *arm642-nfp-depth*))
                        (arm642-push-register seg src-reg)
                        (! fixnum->signed-natural offset-reg (arm642-one-untargeted-reg-form seg offset arm64::arg_z))
                        (arm642-pop-register seg src-reg)))
                    (! mem-ref-address dest src-reg offset-reg)))))
             (<- dest))
           (^)))))


;;; %current-frame-ptr â€” donor ppc2.lisp:8890 (v2 arm64-p2.lisp:9873,
;;; which carries the same restore-nfp line our w4 return/tail legs
;;; use). Tail leg = the lane's standard tail-call trio (w1:87/w4:417):
;;; restore-nvrs (lane signature (seg values-p), NOT v2's (seg ea
;;; count)), restore-nfp, restore-full-lisp-context. Emits the
;;; %current-frame-ptr vinsn (w11 vinsns â€” SP as boxed fixnum).
(defarm642 arm642-%current-frame-ptr %current-frame-ptr (seg vreg xfer)
  (cond ((arm642-tailcallok xfer)
         (arm642-restore-nvrs seg nil)
         (! restore-nfp)
         (arm642-restore-full-lisp-context seg)
         (! %current-frame-ptr ($ arm64::arg_z))
         (! jump-return-pc))
        (t
         (when vreg
           (ensuring-node-target (target vreg)
             (! %current-frame-ptr target)))
         (^))))


;;; make-list â€” donor ppc2.lisp:8079 (v2 arm64-p2.lisp:8985 identical):
;;; a pure acode rewrite into an ordinary CALL of the make-list
;;; function with :initial-element â€” no vinsns, no subprims.
(defarm642 arm642-make-list make-list (seg vreg xfer size initial-element)
  (arm642-form seg vreg xfer (make-acode (%nx1-operator call)
                                         (make-acode (%nx1-operator immediate) 'make-list)
                                         (list nil
                                               (list initial-element
                                                     (make-acode (%nx1-operator immediate)
                                                                 :initial-element)
                                                     size)))))


;;; %word-to-int â€” donor ppc2.lisp:6026 (v2 arm64-p2.lisp:6860
;;; identical). Emits sign-extend-halfword (w11 vinsns â€” boxed
;;; halfword sign-extension, PPC64 ppc64-vinsns.lisp:2993).
(defarm642 arm642-%word-to-int %word-to-int (seg vreg xfer form)
  (if (null vreg)
    (arm642-form seg nil xfer form)
    (progn
      (ensuring-node-target (target vreg)
        (! sign-extend-halfword target (arm642-one-untargeted-reg-form seg form arm64::arg_z)))
      (^))))


;;; ---------------------------------------------------------------------------
;;; %natural<> â€” donor ppc2.lisp:6942 -> ppc2-natural-compare.
;;; PPC encodes unsignedness in the INSTRUCTION (cmpld/compare-logical,
;;; same CR bit); A64 has one CMP and encodes unsignedness in the
;;; CONDITION (lo/hi instead of lt/gt). So the lane rendering is: an
;;; UNSIGNED variant of w1's condition mapper, then the very same
;;; compare machinery (compare-signed-s16const is a bare `cmp reg,#imm`
;;; despite the name; arm642-compare-registers is a bare `cmp reg,reg`
;;; + cond->boolean, whose (logxor cr-bit 1) inversion also holds for
;;; hi/ls (8/9) and lo/hs (3/2)). v2's compare-logical/bit31 family
;;; deliberately not carried (w1 arm642-compare-registers note).
;;; âš  arm64::cond-hi / arm64::cond-lo name the A64 b.cc conditions in
;;; HIS arm64-asm.lisp cond table (@1380-1419, whence w1's cond-eq/gt/
;;; lt) â€” spelling unverifiable locally; if his table says cond-cc/
;;; cond-cs instead, this is a two-token fix.

;;; Unsigned twin of w1's condition-to-arm64-cond-bit (same LE/GE-as-
;;; negated-GT/LT carry via true-p).
(defun acode-condition-to-arm64-unsigned-cond-bit (cond)
  (case (car (acode-operands cond))
    (:EQ (values arm64::cond-eq t))
    (:NE (values arm64::cond-eq nil))
    (:GT (values arm64::cond-hi t))
    (:LE (values arm64::cond-hi nil))
    (:LT (values arm64::cond-lo t))
    (:GE (values arm64::cond-lo nil))))


;;; Unsigned twin of w1's arm642-swap-compare-cond-bit.
(defun arm642-swap-unsigned-cond-bit (cr-bit)
  (cond ((eql cr-bit arm64::cond-hi) arm64::cond-lo)
        ((eql cr-bit arm64::cond-lo) arm64::cond-hi)
        (t cr-bit)))



(defarm642 arm642-%natural<> %natural<> (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm64-unsigned-cond-bit cc)
    (arm642-natural-compare seg vreg xfer form1 form2 cr-bit true-p)))


(defarm642 arm642-aapcs64-ff-call aapcs64-ff-call (seg vreg xfer address argspecs argvals resultspec &optional monitor)
  (declare (ignore monitor))
  (let* ((*arm642-vstack* *arm642-vstack*)
         (*arm642-cstack* *arm642-cstack*)
         (pre-frame-cstack *arm642-cstack*) ;restored after (! ff-call): see below
         (gpr-offset 0)
         (other-offset 8)
         (single-float-offset 8)
         (double-float-offset 8)
         (nsingle-floats 0)             ; F
         (ndouble-floats 0)             ; D
         (nother-words 0)
         (nfpr-args 0)
         (ngpr-args 0)
         (fp-loads ()))
    (declare (fixnum nsingle-floats ndouble-floats nfpr-args ngpr-args
                     nother-words gpr-offset other-offset
                     single-float-offset double-float-offset))
 ;; Pass 1: count slots per class. AAPCS64: first 8 GPR-class args
 ;; in x0-x7 (a 64-bit integer is ONE slot — v2 s86 lesson), first
 ;; 8 FP-class in d0-d7, rest overflow to the stack.
    (dolist (argspec argspecs)
      (case argspec
        ((:double-float :single-float)
         (incf nfpr-args)
         (if (<= nfpr-args 8)
           (if (eq argspec :double-float)
             (incf ndouble-floats)
             (incf nsingle-floats))
 ;; Overflow FP: v2's legs carry PPC32-EABI slotting; loud
 ;; stop until the AAPCS64 packing is ratified (header note).
           (compiler-bug "aapcs64-ff-call: more than 8 floating-point ~
                          args (~s) not yet supported" argspecs)))
        (t (incf ngpr-args)
           (if (> ngpr-args 8)
             (incf nother-words)))))
    (let* ((single-words (+ 8 nother-words nsingle-floats))
           (total-words (if (zerop ndouble-floats)
                          single-words
                          (+ (the fixnum (+ ndouble-floats ndouble-floats))
                             (the fixnum (logand (lognot 1)
                                                 (the fixnum (1+ single-words))))))))
      (declare (fixnum single-words total-words))
      (when (> total-words 490)         ;alloc-c-frame SUB imm12 reach
        (compiler-bug "aapcs64-ff-call: c-frame too large (~s words)"
                      total-words))
      (setq single-float-offset (+ other-offset nother-words))
      (setq double-float-offset
            (logand (lognot 1)
                    (the fixnum (1+ (the fixnum (+ single-float-offset
                                                   nsingle-floats))))))
      (! alloc-c-frame total-words)
 ;; Nonlocal exits crossing this point pop the frame via his
 ;; generic cstack-diff adjust-sp (header note).
      (setq *arm642-cstack*
            (+ *arm642-cstack*
               (+ arm64::c-frame.minsize
                  (ash (logand -2 (1+ total-words)) 3)))))
    (setq ngpr-args 0 nfpr-args 0)
    (arm642-vpush-register seg (arm642-one-untargeted-reg-form
                                seg address arm64::arg_z))
 ;; Pass 2: evaluate each arg form into its frame slot. FP args
 ;; remember (kind . offset) so d0-d7 can be loaded after the last
 ;; form (arbitrary lisp code in later forms would clobber them).
    (do* ((specs argspecs (cdr specs))
          (vals argvals (cdr vals)))
         ((null specs))
      (declare (list specs vals))
      (let* ((valform (car vals))
             (spec (car specs))
             (absptr (acode-absolute-ptr-p valform)))
        (case spec
 ;; FPR regspecs are raw FPR numbers under :class :fpr (his
 ;; arm642-immediate idiom); no dN name constants in his arch.
 ;; Staging register d1 mirrors the v2 donor's fp1 choice.
          (:double-float
           (let* ((df ($ 1 :class :fpr :mode :double-float)))
             (incf nfpr-args)
             (arm642-one-targeted-reg-form seg valform df)
             (! set-double-c-arg df double-float-offset)
             (push (cons :double-float double-float-offset) fp-loads)
             (incf double-float-offset 2)))
          (:single-float
           (let* ((sf ($ 1 :class :fpr :mode :single-float)))
             (incf nfpr-args)
             (arm642-one-targeted-reg-form seg valform sf)
             (! set-single-c-arg sf single-float-offset)
             (push (cons :single-float single-float-offset) fp-loads)
             (incf single-float-offset)))
 ;; 64-bit integer: full value in imm0 via gets64/getu64
 ;; (w10), ONE GPR slot (v2 s86 AAPCS64 deviation kept).
          ((:signed-doubleword :unsigned-doubleword)
           (arm642-one-targeted-reg-form seg valform ($ arm64::arg_z))
           (if (eq spec :signed-doubleword)
             (! gets64)
             (! getu64))
           (incf ngpr-args)
           (cond ((<= ngpr-args 8)
                  (! set-c-arg ($ arm64::imm0) gpr-offset)
                  (incf gpr-offset))
                 (t
                  (! set-c-arg ($ arm64::imm0) other-offset)
                  (incf other-offset))))
          (:address
           (with-imm-target () (ptr :address)
             (if absptr
               (arm642-lri seg ptr absptr)
               (arm642-form seg ptr nil valform))
             (incf ngpr-args)
             (cond ((<= ngpr-args 8)
                    (! set-c-arg ptr gpr-offset)
                    (incf gpr-offset))
                   (t
                    (! set-c-arg ptr other-offset)
                    (incf other-offset)))))
          (t
           (with-imm-target () (valreg :natural)
             (let* ((reg (arm642-unboxed-integer-arg-to-reg
                          seg valform valreg spec)))
               (incf ngpr-args)
               (cond ((<= ngpr-args 8)
                      (! set-c-arg reg gpr-offset)
                      (incf gpr-offset))
                     (t
                      (! set-c-arg reg other-offset)
                      (incf other-offset)))))))))
 ;; Load the FP argument registers from the staging slots. AAPCS64
 ;; FP args index from d0 (v2 s36 deviation kept; fp1-based indexing
 ;; is the PPC numbering).
    (do* ((fpreg 0 (1+ fpreg))          ;d0..d7 — AAPCS64 FP arg regs
          (reloads (nreverse fp-loads) (cdr reloads)))
         ((or (null reloads) (= fpreg 8)))
      (declare (list reloads) (fixnum fpreg))
      (let* ((reload (car reloads))
             (size (car reload))
             (from (cdr reload)))
        (if (eq size :double-float)
          (! reload-double-c-arg ($ fpreg :class :fpr :mode :double-float) from)
          (! reload-single-c-arg ($ fpreg :class :fpr :mode :single-float) from))))
 ;; No stack args reach the callee: _SPffcall keeps SP at the frame
 ;; head during the call ( — popping freed the saved lr/backlink
 ;; under the callee). Args 9+ would sit in never-read stack slots,
 ;; so refuse loudly until the stack-arg frame layout is ratified.
    (when (> ngpr-args 8)
      (compiler-bug "aapcs64-ff-call: more than 8 GPR args (~d) — stack-arg ~
                     frame layout not ratified ()" ngpr-args))
    (arm642-vpop-register seg ($ arm64::arg_z))
    (! ff-call)
 ;; .SPffcall popped the c-frame at runtime; restore the static
 ;; accounting NOW, not via the let*-shadow. The shadow only
 ;; unwinds after this handler returns — but (^) below emits the
 ;; FULL EPILOGUE for tail-position ff-calls, and with the bump
 ;; still live the epilogue emitted an extra `add sp,#framesize`
 ;; (the double-pop: tail sites like MALLOC returned through
 ;; a skewed frame; non-tail sites were unaffected). The bump must
 ;; be live only from alloc-c-frame to here, so nonlocal exits out
 ;; of argument forms still pop the frame via his cstack-diff
 ;; adjust-sp.
    (setq *arm642-cstack* pre-frame-cstack)
    (when vreg
      (cond ((eq resultspec :void) (<- nil))
 ;; FP results in d0 (v2 s36 AAPCS64 deviation kept).
            ((eq resultspec :double-float)
             (<- ($ 0 :class :fpr :mode :double-float)))
            ((eq resultspec :single-float)
             (<- ($ 0 :class :fpr :mode :single-float)))
            ((eq resultspec :unsigned-doubleword)
             (ensuring-node-target (target vreg)
               (arm642-box-u64 seg target ($ arm64::imm0 :mode :u64))))
            ((eq resultspec :signed-doubleword)
             (ensuring-node-target (target vreg)
               (arm642-box-s64 seg target ($ arm64::imm0 :mode :s64))))
            (t
             (<- (make-wired-lreg arm64::imm0
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


;;; %reference-external-entry-point — donor ppc2.lisp:8703 (v2
;;; arm64-p2.lisp:9684 identical): evaluate the eep object into
;;; whichever of arg_z/arg_y the target isn't, then eep.address
;;; (w13 vinsn: slot-1 load + unresolved trap). 15 level-1 files'
;;; real-mode wall (external-call path; scan mode had shimmed it).
(defarm642 arm642-%reference-external-entry-point %reference-external-entry-point
  (seg vreg xfer arg)
  (ensuring-node-target (target vreg)
    (let* ((reg (if (eq (hard-regspec-value target) arm64::arg_z)
                  ($ arm64::arg_y)
                  ($ arm64::arg_z))))
      (arm642-one-targeted-reg-form seg arg reg)
      (! eep.address target reg)))
  (^))

(defun arm642-get-float (seg vreg xfer ptr offset double-p fp-reg)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (cond ((null vreg)
           (arm642-form seg nil nil ptr)
           (arm642-form seg nil xfer offset))
          (t
           (let* ((fixoffset (acode-fixnum-form-p offset)))
 ;; scaled-ldr guard: aligned + in range for the C form
             (if (and (typep fixoffset '(unsigned-byte 15))
                      (not (logtest fixoffset (if double-p 7 3))))
               (with-imm-target () (ptrreg :address)
                 (arm642-form seg ptrreg nil ptr)
                 (if double-p
                   (! mem-ref-c-double-float fp-reg ptrreg fixoffset)
                   (! mem-ref-c-single-float fp-reg ptrreg fixoffset)))
               (with-imm-target () (ptrreg :address)
                 (with-imm-target (ptrreg) (offsetreg :s64)
                   (arm642-two-targeted-reg-forms seg
                                                  ptr ptrreg
                                                  offset ($ arm64::arg_z))
                   (! fixnum->signed-natural offsetreg ($ arm64::arg_z))
                   (if double-p
                     (! mem-ref-double-float fp-reg ptrreg offsetreg)
                     (! mem-ref-single-float fp-reg ptrreg offsetreg)))))
             (<- fp-reg))
           (^)))))


(defarm642 arm642-%get-double-float %get-double-float (seg vreg xfer ptr offset)
  (with-fp-target () (fp-reg :double-float)
    (arm642-get-float seg vreg xfer ptr offset t fp-reg)))


(defarm642 arm642-%get-single-float %get-single-float (seg vreg xfer ptr offset)
  (with-fp-target () (fp-reg :single-float)
    (arm642-get-float seg vreg xfer ptr offset nil fp-reg)))


(defarm642 arm642-get-bit %get-bit (seg vreg xfer ptr offset)
  (if (null vreg)
    (progn
 ;; x8664 arg order (x862.lisp:9771-9774); ppc2's swapped
 ;; (seg nil ptr nil) looks like an upstream typo
      (arm642-form seg nil nil ptr)
      (arm642-form seg nil xfer offset))
    (let* ((offval (acode-fixnum-form-p offset))
           (byte-index (if offval (ash offval -3)))
           (bit (if offval (logand offval 7))))
 ;; ldrb-scaled guard for the C form
      (if (and byte-index (< byte-index 4096))
        (with-imm-target () (src-reg :address)
          (arm642-one-targeted-reg-form seg ptr src-reg)
          (if (node-reg-p vreg)
            (! mem-ref-c-bit-fixnum vreg src-reg byte-index bit)
            (with-imm-target () (dest :u8)
              (! mem-ref-c-bit dest src-reg byte-index bit)
              (<- dest))))
        (with-imm-target () (src-reg :address)
          (arm642-two-targeted-reg-forms seg ptr src-reg
                                         offset ($ arm64::arg_z))
          (if (node-reg-p vreg)
            (! mem-ref-bit-fixnum vreg src-reg ($ arm64::arg_z))
            (with-imm-target () (dest :u8)
              (! mem-ref-bit dest src-reg ($ arm64::arg_z))
              (<- dest)))))
      (^))))


;;; ============ set side (demanded one cycle after the readers) ============
;;; %set-bit (lib/level-2.lisp), %set-single-float (lib/db-io.lisp) +
;;; %set-double-float sibling. arm642-set-float follows ppc2-set-float
;;; (ppc2.lisp:7040-7113) including the node-vreg tail; the set-bit
;;; handler follows x862-set-bit (x862.lisp:10886, no rotate math) with
;;; the same byte-index/bit-number vinsn contract as the reader family.
(defun arm642-set-float (seg vreg xfer ptr offset newval double-p fp-reg)
  (with-arm64-local-vinsn-macros (seg vreg xfer)
    (let* ((fixoffset (acode-fixnum-form-p offset))
 ;; scaled-str guard: aligned + in range for the C form
           (immoffset (and (typep fixoffset '(unsigned-byte 15))
                           (not (logtest fixoffset (if double-p 7 3))))))
      (with-imm-target () (ptr-reg :address)
        (cond ((or (null vreg)
                   (= (hard-regspec-class vreg) hard-reg-class-fpr))
               (cond (immoffset
                      (arm642-push-register
                       seg
                       (arm642-one-untargeted-reg-form seg ptr ptr-reg))
                      (arm642-one-targeted-reg-form seg newval fp-reg)
                      (arm642-pop-register seg ptr-reg)
                      (if double-p
                        (! mem-set-c-double-float fp-reg ptr-reg fixoffset)
                        (! mem-set-c-single-float fp-reg ptr-reg fixoffset)))
                     (t
                      (with-imm-target (ptr-reg) (offset-reg :s64)
                        (arm642-push-register
                         seg
                         (arm642-one-untargeted-reg-form seg ptr ptr-reg))
                        (arm642-push-register
                         seg
                         (arm642-one-untargeted-reg-form seg offset
                                                         ($ arm64::arg_z)))
                        (arm642-one-targeted-reg-form seg newval fp-reg)
                        (arm642-pop-register seg ($ arm64::arg_z))
                        (arm642-pop-register seg ptr-reg)
                        (! fixnum->signed-natural offset-reg ($ arm64::arg_z))
                        (if double-p
                          (! mem-set-double-float fp-reg ptr-reg offset-reg)
                          (! mem-set-single-float fp-reg ptr-reg offset-reg)))))
               (<- fp-reg))
              (t
 ;; vreg is a node reg: caller wants the (boxed) value
               (cond (immoffset
                      (let* ((rnew ($ arm64::arg_z)))
                        (arm642-push-register
                         seg
                         (arm642-one-untargeted-reg-form seg ptr ptr-reg))
                        (arm642-one-targeted-reg-form seg newval rnew)
                        (arm642-pop-register seg ptr-reg)
                        (with-imm-temps (ptr-reg) ()
                          (arm642-copy-register seg fp-reg rnew)
                          (if double-p
                            (! mem-set-c-double-float fp-reg ptr-reg fixoffset)
                            (! mem-set-c-single-float fp-reg ptr-reg fixoffset)))))
                     (t
                      (let* ((roffset ($ arm64::arg_y))
                             (rnew ($ arm64::arg_z)))
                        (arm642-push-register
                         seg
                         (arm642-one-untargeted-reg-form seg ptr ptr-reg))
                        (arm642-two-targeted-reg-forms seg
                                                       offset roffset
                                                       newval rnew)
                        (arm642-pop-register seg ptr-reg)
                        (with-imm-target (ptr-reg) (offset-reg :s64)
                          (with-imm-temps (ptr-reg offset-reg) ()
                            (! fixnum->signed-natural offset-reg roffset)
                            (arm642-copy-register seg fp-reg rnew))
                          (if double-p
                            (! mem-set-double-float fp-reg ptr-reg offset-reg)
                            (! mem-set-single-float fp-reg ptr-reg offset-reg))))))
               (<- ($ arm64::arg_z)))))
      (^))))


(defarm642 arm642-%set-double-float %set-double-float (seg vreg xfer ptr offset newval)
  (with-fp-target () (fp-reg :double-float)
    (arm642-set-float seg vreg xfer ptr offset newval t fp-reg)))


(defarm642 arm642-%set-single-float %set-single-float (seg vreg xfer ptr offset newval)
  (with-fp-target () (fp-reg :single-float)
    (arm642-set-float seg vreg xfer ptr offset newval nil fp-reg)))


(defarm642 arm642-set-bit %set-bit (seg vreg xfer ptr offset newval)
  (let* ((offval (acode-fixnum-form-p offset))
         (byte-index (if offval (ash offval -3)))
         (bit (if offval (logand offval 7)))
         (constval (acode-fixnum-form-p newval)))
 ;; ldrb/strb-scaled guard for the C forms
    (if (and byte-index (< byte-index 4096))
      (with-imm-target () (src :address)
        (if constval
          (progn
            (arm642-one-targeted-reg-form seg ptr src)
            (if (eql constval 0)
              (! mem-set-c-bit-0 src byte-index bit)
              (! mem-set-c-bit-1 src byte-index bit))
            (when vreg
              (arm642-form seg vreg nil newval)))
          (progn
            (arm642-two-targeted-reg-forms seg ptr src
                                           newval ($ arm64::arg_z))
            (! mem-set-c-bit src byte-index bit ($ arm64::arg_z))
            (<- ($ arm64::arg_z)))))
      (with-imm-target () (src :address)
        (if constval
          (progn
            (arm642-two-targeted-reg-forms seg ptr src
                                           offset ($ arm64::arg_z))
            (if (eql constval 0)
              (! mem-set-bit-0 src ($ arm64::arg_z))
              (! mem-set-bit-1 src ($ arm64::arg_z)))
            (when vreg
              (arm642-form seg vreg nil newval)))
          (progn
            (arm642-three-targeted-reg-forms seg ptr src
                                             offset ($ arm64::arg_y)
                                             newval ($ arm64::arg_z))
            (! mem-set-bit-variable-value src ($ arm64::arg_y)
                                          ($ arm64::arg_z))
            (<- ($ arm64::arg_z)))))))
  (^))


;;; ============ natural (unboxed u64) logical handlers ============
;;; x862.lisp x862-%natural-logand @11519 structure, x8664 leg only
;;; (the x8632 branch is nfp spill plumbing 32-bit needs); constant
;;; folding via arm642-absolute-natural (w1). The u31-constant fast
;;; paths (%natural-logand-c etc.) are dropped: always two-reg, the
;;; constant materializes through the normal reg-form path.
(macrolet ((def-natural-logical (handler-name op-name folder)
             `(defarm642 ,handler-name ,op-name (seg vreg xfer x y)
                (if (null vreg)
                  (progn
                    (arm642-form seg nil nil x)
                    (arm642-form seg nil xfer y))
                  (let* ((naturalx (nx-natural-constant-p x))
                         (naturaly (nx-natural-constant-p y)))
                    (if (and naturalx naturaly)
                      (arm642-absolute-natural seg vreg xfer
                                               (,folder naturalx naturaly))
                      (progn
                        (with-imm-target () (xreg :natural)
                          (with-imm-target (xreg) (yreg :natural)
                            (arm642-two-targeted-reg-forms seg x xreg y yreg)
                            (! ,op-name xreg xreg yreg))
                          (<- xreg))
                        (^))))))))
  (def-natural-logical arm642-%natural-logand %natural-logand logand)
  (def-natural-logical arm642-%natural-logior %natural-logior logior)
  (def-natural-logical arm642-%natural-logxor %natural-logxor logxor))


;;; ============ load-time-value ============
;;; demand: LIVE REPL wall. (defclass ex-pt () ((x ...))) typed at
;;; the REPL died with
;;; Error: Compiler bug or inconsistency:
;;; arm642-form ? #<ACODE load-time-value (#<ACODE immediate
;;; (#<CLASS-CELL for EX-PT>)>)>
;;; i.e. arm642-form reached a load-time-value acode node with no handler.
;;;
;;; DONOR = ARM32 arm2.lisp:6329 and x8664 x862.lisp:7932, which are the
;;; same single line. DECLARED DOCTRINE EXCEPTION: ppc2.lisp has NO
;;; load-time-value handler at all, so PPC64 cannot be the donor here --
;;; PPC is the outlier, not the reference (x8664 and ARM32 both have it).
;;; This is a latent bug in PPC's backend, recorded, not inherited.
;;;
;;; Why PPC gets away with it (nx1.lisp:1151 nx1-load-time-value): when
;;; *nx-load-time-eval-token* is set -- i.e. COMPILE-FILE, where the value
;;; must be deferred to load time -- nx1 emits an `immediate` acode via
;;; nx1-immediate and no load-time-value node is ever built. The operator
;;; node is produced ONLY on the in-core path (`compile`/`eval` of a form
;;; containing load-time-value), where nx1 has already called the value's
;;; thunk and wraps the result:
;;; (make-acode (%nx1-operator load-time-value)
;;; (make-acode (%nx1-operator immediate) (funcall function)))
;;; So the handler's whole job is to unwrap: compile the single subform,
;;; which is that already-computed immediate. Nothing target-specific.
;;;
;;; This lane hits it where PPC64 never did because we now HAVE a resident
;;; compiler at a live REPL, and defclass expands to a find-class-cell
;;; wrapped in load-time-value.
(defarm642 arm642-load-time-value load-time-value (seg vreg xfer val)
  (arm642-form seg vreg xfer val))

;;; The four TYPED multi-dimensional acode handlers.  nx1.lisp:884 emits
;;; simple-typed-aref2/aref3 (and the aset pair) whenever both the array
;;; element type and the rank are known at compile time; with no handler for
;;; the operator the compiler signals CCL::COMPILER-BUG, which is what
;;; CCL.ISSUE#335 and CCL.BUG#620 report.  Only the DECLARED path was
;;; affected -- an undeclared (aref a i j) routes through general-aref2 and
;;; the .SParef2 subprim, which is why 2d arrays otherwise worked.
;;;
;;; Placement: these MUST sit after the defarm642 macro (defined at the
;;; DEFPARAMETER block above, arm642.lisp:5671 at the pin).  Putting them next
;;; to the arm642-aref2/aref3 helpers instead -- which reads better -- makes
;;; each form compile as a FUNCTION CALL, so loading arm642 dies with
;;; "Unbound variable: CCL::ARM642-%AREF2" and takes every arm642-additions
;;; file down with it, because the aborted load never defines the macro.
;;;
;;; Adopted VERBATIM from upstream arm642.lisp @1db0ab68 (commit 41400d3c),
;;; which adapts PPC64 ppc2.lisp:7615/7667/7724/7817.  Taking his text rather
;;; than writing our own keeps the pin advance a no-op here instead of a
;;; divergent shadow.  The helpers they call are his own arm642-aset2/aset3/
;;; aref3 (this file) plus arm642-aref2, which he defines at tip and which our
;;; compiler overlay supplies at this pin.
;;;
;;; TWO UPSTREAM NOTES, both preserved deliberately rather than fixed:
;;;   * the eight vinsns these paths need -- 2d-dim1, 2d-unscaled-index,
;;;     check-2d-bound, 3d-dims, 3d-unscaled-index, check-3d-bound,
;;;     array-data-vector-ref, trap-unless-simple-array-2 -- are defined
;;;     NOWHERE in his tree at pin or tip, so these handlers are dead code
;;;     upstream until the vinsns land.  Ours come from the compiler overlay.
;;;   * simple-typed-aref3's (if (null vreg) ...) has no else branch and falls
;;;     through into the aref3 call, so a null-vreg 3d aref evaluates its
;;;     subforms TWICE.  PPC64 has the same shape at ppc2.lisp:7667.  Kept
;;;     byte-identical to upstream and reported rather than silently diverged.
(defarm642 arm642-%aref2 simple-typed-aref2 (seg vreg xfer typename arr i j &optional dim0 dim1)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil arr)
      (arm642-form seg nil nil i)
      (arm642-form seg nil xfer j))
    (let* ((type-keyword (acode-immediate-operand typename))
           (fixtype (nx-lookup-target-uvector-subtag type-keyword))
           (safe (unless *arm642-reckless* fixtype))
           (dim0 (acode-fixnum-form-p dim0))
           (dim1 (acode-fixnum-form-p dim1)))
      (arm642-aref2 seg vreg xfer arr i j safe type-keyword dim0 dim1))))

(defarm642 arm642-%aref3 simple-typed-aref3 (seg vreg xfer typename arr i j k &optional dim0 dim1 dim2)
  (if (null vreg)
    (progn
      (arm642-form seg nil nil arr)
      (arm642-form seg nil nil i)
      (arm642-form seg nil nil j)
      (arm642-form seg nil xfer k)))
  (let* ((type-keyword (acode-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword))
         (safe (unless *arm642-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1))
         (dim2 (acode-fixnum-form-p dim2)))
    (arm642-aref3 seg vreg xfer arr i j k safe type-keyword dim0 dim1 dim2)))

(defarm642 arm642-%aset2 simple-typed-aset2 (seg vreg xfer typename arr i j new &optional dim0 dim1)
  (let* ((type-keyword (acode-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword))
         (safe (unless *arm642-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1)))
    (arm642-aset2 seg vreg xfer arr i j new safe type-keyword dim0 dim1)))

(defarm642 arm642-%aset3 simple-typed-aset3 (seg vreg xfer typename arr i j k new &optional dim0 dim1 dim2)
  (let* ((type-keyword (acode-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword))
         (safe (unless *arm642-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1))
         (dim2 (acode-fixnum-form-p dim2)))
    (arm642-aset3 seg vreg xfer arr i j k new safe type-keyword dim0 dim1 dim2)))
