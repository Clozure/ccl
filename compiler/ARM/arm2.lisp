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

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "ARMENV"))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (require "ARM-BACKEND"))

(defparameter *arm2-debug-mask* 0)
(defconstant arm2-debug-verbose-bit 0)
(defconstant arm2-debug-vinsns-bit 1)
(defparameter *arm2-target-node-size* 0)
(defparameter *arm2-target-fixnum-shift* 0)
(defparameter *arm2-target-node-shift* 0)
(defparameter *arm2-target-bits-in-word* 0)
(defparameter *arm2-half-fixnum-type* '(signed-byte 29))
(defparameter *arm2-target-half-fixnum-type* nil)
(defparameter *arm2-operator-supports-u8-target* ())
(defparameter *arm2-autodrain-constant-pool* ())
(defparameter *arm2-nfp-depth* 0)
(defparameter *arm2-max-nfp-depth* ())
(defparameter *arm2-all-nfp-pushes* ())
(defparameter *arm2-nfp-vars* ())

(defun arm2-max-nfp-depth ()
  (or *arm2-max-nfp-depth*
      (setq *arm2-max-nfp-depth*
            (let* ((max 0))
              (declare (fixnum max))
              (dolist (v *arm2-all-nfp-pushes* max)
                (when (vinsn-succ v)    ;not elided
                  (let* ((depth (+ (the fixnum (svref (vinsn-variable-parts v) 1))
                                   (if (vinsn-attribute-p v :uses-frame-pointer)
                                     16
                                     8))))
                    (declare (fixnum depth))
                    (if (> depth max)
                      (setq max depth)))))))))



  

(defmacro with-arm-p2-declarations (declsform &body body)
  `(let* ((*arm2-tail-allow* *arm2-tail-allow*)
          (*arm2-reckless* *arm2-reckless*)
          (*arm2-open-code-inline* *arm2-open-code-inline*)
          (*arm2-trust-declarations* *arm2-trust-declarations*)
	  (*arm2-full-safety* *arm2-full-safety*)
          (*arm2-float-safety* *arm2-float-safety*))
     (arm2-decls ,declsform)
     ,@body))


(defun arm2-emit-vinsn (vlist name vinsn-table &rest vregs)
  (arm2-update-regmap (apply #'%emit-vinsn vlist name vinsn-table vregs)))

(defmacro with-arm-local-vinsn-macros ((segvar &optional vreg-var xfer-var) &body body)
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
                    `(arm2-emit-vinsn ,',segvar ',,template-name-var (backend-p2-vinsn-templates *target-backend*) ,@,args-var))))
       (macrolet ((<- (,retvreg-var)
                    `(arm2-copy-register ,',segvar ,',vreg-var ,,retvreg-var))
                  (@  (,labelnum-var)
                    `(progn
                      (arm2-invalidate-regmap)
                      (backend-gen-label ,',segvar ,,labelnum-var)))
                  (@+ (,labelnum-var)
                    `(progn             ;keep regmap
                      (backend-gen-label ,',segvar ,,labelnum-var)))
                  (-> (,label-var)
                    `(! jump (aref *backend-labels* ,,label-var)))
                  (^ (&rest branch-args)
                    `(arm2-branch ,',segvar ,',xfer-var ,',vreg-var ,@branch-args))
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


(defvar *arm-current-context-annotation* nil)
(defvar *arm2-woi* nil)
(defvar *arm2-open-code-inline* nil)
(defvar *arm2-optimize-for-space* nil)
(defvar *arm2-register-restore-count* 0)
(defvar *arm2-register-restore-ea* nil)
(defvar *arm2-non-volatile-fpr-count* 0)
(defvar *arm2-compiler-register-save-note* nil)

(defparameter *arm2-tail-call-aliases*
  ()
  #| '((%call-next-method . (%tail-call-next-method . 1))) |#
  
)


(defvar *arm2-icode* nil)
(defvar *arm2-undo-stack* nil)
(defvar *arm2-undo-because* nil)


(defvar *arm2-cur-afunc* nil)
(defvar *arm2-vstack* 0)
(defvar *arm2-cstack* 0)
(defvar *arm2-undo-count* 0)
(defvar *arm2-returning-values* nil)
(defvar *arm2-vcells* nil)
(defvar *arm2-fcells* nil)
(defvar *arm2-entry-vsp-saved-p* nil)

(defvar *arm2-entry-label* nil)
(defvar *arm2-fixed-args-label* nil)
(defvar *arm2-fixed-args-tail-label* nil)
(defvar *arm2-fixed-nargs* nil)
(defvar *arm2-tail-allow* t)
(defvar *arm2-reckless* nil)
(defvar *arm2-full-safety* nil)
(defvar *arm2-float-safety* nil)
(defvar *arm2-trust-declarations* nil)
(defvar *arm2-entry-vstack* nil)
(defvar *arm2-need-nargs* t)

(defparameter *arm2-inhibit-register-allocation* nil)
(defvar *arm2-record-symbols* nil)
(defvar *arm2-recorded-symbols* nil)
(defvar *arm2-emitted-source-notes* nil)

(defvar *arm2-result-reg* arm::arg_z)
(defparameter *arm2-nvrs* nil)
(defparameter *arm2-first-nvr* -1)

(defvar *arm2-gpr-locations* nil)
(defvar *arm2-gpr-locations-valid-mask* 0)
(defvar *arm2-gpr-constants* nil)
(defvar *arm2-gpr-constants-valid-mask* 0)









(declaim (fixnum *arm2-vstack* *arm2-cstack*))

 


;;; Before any defarm2's, make the *arm2-specials* vector.




     










  


(defun arm2-gprs-containing-constant (c)
  (let* ((in *arm2-gpr-constants-valid-mask*)
         (vals *arm2-gpr-constants*)
         (out 0))
    (declare (fixnum in out) (simple-vector vals))
    (dotimes (i 16 out)
      (declare (type (mod 16) i))
      (when (and (logbitp i in)
                 (eql c (svref vals i)))
        (setq out (logior out (ash 1 i)))))))

(defun arm2-nfp-ref (seg vreg ea)
  (with-arm-local-vinsn-macros (seg vreg)
    (let* ((offset (logand #xfff8 ea))
           (type (logand #x7 ea))
           (vreg-class (hard-regspec-class vreg))
           (vreg-mode (get-regspec-mode vreg))
           (nested (> *arm2-undo-count* 0))
           (vinsn nil)
           (reg vreg))
      (ecase type
        (#. memspec-nfp-type-natural
            (unless (and (eql vreg-class hard-reg-class-gpr)
                         (eql vreg-mode hard-reg-class-gpr-mode-u32))
              (setq reg (available-imm-temp
                         *available-backend-imm-temps*
                         :u32)))
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
                         (eql vreg-mode hard-reg-class-fpr-mode-complex-double-float))
              (setq reg (available-fp-temp
                         *available-backend-fp-temps*
                         :complex-double-float)))
            (setq vinsn
                  (if nested
                    (! nfp-load-complex-double-float-nested reg offset)
                    (! nfp-load-complex-double-float reg offset))))
        (#. memspec-nfp-type-complex-single-float
            (unless (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-complex-single-float))
              (setq reg (available-fp-temp
                         *available-backend-fp-temps*
                         :complex-single-float)))
            (setq vinsn
                  (if nested
                    (! nfp-load-complex-single-float-nested reg offset)
                    (! nfp-load-complex-single-float  reg offset)))))
      (when (memspec-single-ref-p ea)
        (let* ((push-vinsn
                (find offset *arm2-all-nfp-pushes*
                      :key (lambda (v)
                             (when (typep v 'vinsn)
                               (svref (vinsn-variable-parts v) 1))))))
          (when push-vinsn
            (arm2-elide-pushes seg push-vinsn vinsn))))
      (<- reg))))

(defun arm2-reg-for-nfp-set (vreg ea)
  (with-arm-local-vinsn-macros (seg )
    (let* ((type (logand #x7 ea))
           (vreg-class (if vreg (hard-regspec-class vreg)))
           (vreg-mode (if vreg (get-regspec-mode vreg))))
      (ecase type
        (#. memspec-nfp-type-natural
            (if (and (eql vreg-class hard-reg-class-gpr)
                     (eql vreg-mode hard-reg-class-gpr-mode-u32))
              vreg
              (make-unwired-lreg
               (available-imm-temp *available-backend-imm-temps* :u32))))
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
               (available-fp-temp *available-backend-fp-temps* :complex-double-float))))
        (#. memspec-nfp-type-complex-single-float
            (if (and (eql vreg-class hard-reg-class-fpr)
                     (eql vreg-mode hard-reg-class-fpr-mode-complex-single-float))
              vreg
              (make-unwired-lreg
               (available-fp-temp *available-backend-fp-temps* :complex-single-float))))))))
      
(defun arm2-nfp-set (seg reg ea)
  (with-arm-local-vinsn-macros (seg )
    (let* ((offset (logand #xfff8 ea))
           (nested (> *arm2-undo-count* 0)))
      (ecase (logand #x7 ea)
        (#. memspec-nfp-type-natural
            (if nested
              (! nfp-store-unboxed-word-nested reg offset)
              (! nfp-store-unboxed-word reg offset)))
        (#. memspec-nfp-type-double-float
            (if nested
              (! nfp-store-unboxed-double-float-nested reg offset)
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
              (! nfp-store-complex-single-float  reg offset)))))))

;;; Depending on the variable's type and other attributes, maybe
;;; push it on te NFP.  Return the nfp-relative EA if we push it.
(defun arm2-nfp-bind (seg var initform)
  (let* ((bits (nx-var-bits var)))
    (unless (logtest bits (logior (ash 1 $vbitspecial)
                                  (ash 1 $vbitclosed)
                                  (ash 1 $vbitdynamicextent)))
      (let* ((type (acode-var-type var *arm2-trust-declarations*))
             (reg nil)
             (nfp-bits 0))
        (cond ((and (subtypep type '(unsigned-byte 32))
                    NIL
                    (not (subtypep type '(signed-byte 30))))
               (setq reg (available-imm-temp
                          *available-backend-imm-temps* :u32)
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
          (let* ((vinsn (arm2-push-register
                         seg
                         (arm2-one-untargeted-reg-form seg initform reg))))
            (when vinsn
              (push (cons vinsn var) *arm2-nfp-vars*)
              (make-nfp-address
               (svref (vinsn-variable-parts vinsn) 1)
               nfp-bits
               #|                       ;
               (and (eql 0 (var-root-nsetqs var))
               (eql 1 (var-root-nrefs var)))
               |#))))))))


               



                                            
              
                      


(defun arm2-do-lexical-reference (seg vreg ea)
  (when vreg
    (with-arm-local-vinsn-macros (seg vreg) 
      (if (memory-spec-p ea)
        (if (eql (memspec-type ea) memspec-nfp-offset)
          (arm2-nfp-ref seg vreg ea)
          (ensuring-node-target (target vreg)
            (let* ((reg (unless (node-reg-p vreg)
                          (or (arm2-reg-for-ea ea)
                              (arm2-try-non-conflicting-reg target 0)))))
              (when reg (setq target reg))
              (arm2-stack-to-register seg ea target)
              (if (addrspec-vcell-p ea)
                (! vcell-ref target target)))))
        (<- ea)))))

(defun arm2-do-lexical-setq (seg vreg ea valreg)
  (with-arm-local-vinsn-macros (seg vreg)
    (cond ((typep ea 'lreg)
            (arm2-copy-register seg ea valreg))
          ((addrspec-vcell-p ea)     ; closed-over vcell
           (arm2-copy-register seg arm::arg_z valreg)
           (arm2-stack-to-register seg ea arm::arg_x)
           (arm2-lri seg arm::arg_y 0)
           (! call-subprim-3 arm::arg_z (arm::arm-subprimitive-offset  '.SPgvset) arm::arg_x arm::arg_y arm::arg_z)
           (setq valreg arm::arg_z))
          ((memory-spec-p ea)    ; vstack slot or fp offset
           (arm2-register-to-stack seg valreg ea))
          (t
           (arm2-copy-register seg ea valreg)))
    (when vreg
      (<- valreg))))

;;; ensure that next-method-var is heap-consed (if it's closed over.)
;;; it isn't ever setqed, is it ?
(defun arm2-heap-cons-next-method-var (seg var)
  (with-arm-local-vinsn-macros (seg)
    (when (eq (ash 1 $vbitclosed)
              (logand (logior (ash 1 $vbitclosed)
                              (ash 1 $vbitcloseddownward))
                      (the fixnum (nx-var-bits var))))
      (let* ((ea (var-ea var))
             (arg ($ arm::arg_z))
             (result ($ arm::arg_z)))
        (arm2-do-lexical-reference seg arg ea)
        (arm2-set-nargs seg 1)
        (! ref-constant ($ arm::fname) (backend-immediate-index (arm2-symbol-entry-locative '%cons-magic-next-method-arg)))
        (! call-known-symbol arg)
        (arm2-do-lexical-setq seg nil ea result)))))






(defun acode-condition-to-arm-cr-bit (cond)
  (condition-to-arm-cr-bit (car (acode-operands cond))))

(defun condition-to-arm-cr-bit (cond)
  (case cond
    (:EQ (values arm::arm-cond-eq t))
    (:NE (values arm::arm-cond-eq nil))
    (:GT (values arm::arm-cond-gt t))
    (:LE (values arm::arm-cond-gt nil))
    (:LT (values arm::arm-cond-lt t))
    (:GE (values arm::arm-cond-lt nil))))


(defun arm-cr-bit-to-arm-unsigned-cr-bit (cr-bit)
  (case cr-bit
    (#.arm::arm-cond-eq arm::arm-cond-eq)
    (#.arm::arm-cond-ne arm::arm-cond-ne)
    (#.arm::arm-cond-gt arm::arm-cond-hi)
    (#.arm::arm-cond-le arm::arm-cond-ls)
    (#.arm::arm-cond-lt arm::arm-cond-lo)
    (#.arm::arm-cond-ge arm::arm-cond-hs)))

;;; If we have to change the order of operands in a comparison, we
;;; generally need to change the condition we're testing.
(defun arm2-cr-bit-for-reversed-comparison (cr-bit)
  (ecase cr-bit
    (#.arm::arm-cond-eq arm::arm-cond-eq)
    (#.arm::arm-cond-ne arm::arm-cond-ne)
    (#.arm::arm-cond-lt arm::arm-cond-gt)
    (#.arm::arm-cond-le arm::arm-cond-ge)
    (#.arm::arm-cond-gt arm::arm-cond-lt)
    (#.arm::arm-cond-ge arm::arm-cond-le)
    (#.arm::arm-cond-lo arm::arm-cond-hi)
    (#.arm::arm-cond-ls arm::arm-cond-hs)
    (#.arm::arm-cond-hi arm::arm-cond-lo)
    (#.arm::arm-cond-hs arm::arm-cond-ls)))

    
    

(defun arm2-ensure-binding-indices-for-vcells (vcells)
  (dolist (cell vcells)
    (ensure-binding-index (car cell)))
  vcells)

(defun arm2-compile (afunc &optional lambda-form *arm2-record-symbols*)
  (progn
    (dolist (a  (afunc-inner-functions afunc))
      (unless (afunc-lfun a)
        (arm2-compile a 
                      (if lambda-form 
                        (afunc-lambdaform a)) 
                      *arm2-record-symbols*))) ; always compile inner guys
    (let* ((*arm2-cur-afunc* afunc)
           (*arm2-returning-values* nil)
           (*arm-current-context-annotation* nil)
           (*arm2-woi* nil)
           (*encoded-reg-value-byte* (byte 5 0))           
           (*arm2-open-code-inline* nil)
           (*arm2-optimize-for-space* nil)
           (*arm2-register-restore-count* nil)
           (*arm2-compiler-register-save-note* nil)
           (*arm2-non-volatile-fpr-count* 0)
           (*arm2-register-restore-ea* nil)
           (*arm2-vstack* 0)
           (*arm2-cstack* 0)
           (*arm2-target-fixnum-shift* (arch::target-fixnum-shift (backend-target-arch *target-backend*)))
           (*arm2-target-node-shift* (arch::target-word-shift  (backend-target-arch *target-backend*)))
           (*arm2-target-bits-in-word* (arch::target-nbits-in-word (backend-target-arch *target-backend*)))
	   (*arm2-target-node-size* (arch::target-lisp-node-size (backend-target-arch *target-backend*)))
           (*arm2-target-half-fixnum-type* *arm2-half-fixnum-type*)
           (*backend-vinsns* (backend-p2-vinsn-templates *target-backend*))
           (*backend-node-regs* arm-node-regs)
           (*backend-node-temps* arm-temp-node-regs)
           (*available-backend-node-temps* arm-temp-node-regs)
           (*backend-imm-temps* arm-imm-regs)
           (*available-backend-imm-temps* arm-imm-regs)
           (*backend-fp-temps* arm-temp-fp-regs)
           (*available-backend-fp-temps* arm-temp-fp-regs)
           (*backend-crf-temps* arm-cr-fields)
           (*available-backend-crf-temps* arm-cr-fields)
           (bits 0)
           (*logical-register-counter* -1)
           (*backend-all-lregs* ())
           (*arm2-undo-count* 0)
           (*backend-labels* (arm2-make-stack 64 target::subtag-simple-vector))
           (*arm2-undo-stack* (arm2-make-stack 64  target::subtag-simple-vector))
           (*arm2-undo-because* (arm2-make-stack 64))
           (*backend-immediates* (arm2-make-stack 64  target::subtag-simple-vector))
           (*arm2-entry-label* nil)
           (*arm2-fixed-args-label* nil)
           (*arm2-fixed-args-tail-label*)
           (*arm2-fixed-nargs* nil)
           (*arm2-inhibit-register-allocation* nil)
           (*arm2-tail-allow* t)
           (*arm2-reckless* nil)
	   (*arm2-full-safety* nil)
           (*arm2-float-safety* nil)
           (*arm2-trust-declarations* t)
           (*arm2-entry-vstack* nil)
           (*arm2-need-nargs* t)
           (fname (afunc-name afunc))
           (*arm2-entry-vsp-saved-p* nil)
           (*arm2-vcells* (arm2-ensure-binding-indices-for-vcells (afunc-vcells afunc)))
           (*arm2-fcells* (afunc-fcells afunc))
           *arm2-recorded-symbols*
           (*arm2-autodrain-constant-pool* t)
           (*arm2-emitted-source-notes* '())
           (*arm2-gpr-locations-valid-mask* 0)
           (*arm2-gpr-locations* (make-array 16 :initial-element nil))
           (*arm2-gpr-constants-valid-mask* 0)
           (*arm2-gpr-constants*(make-array 16 :initial-element nil))
           (*arm2-nfp-depth* 0)
           (*arm2-max-nfp-depth* ())
           (*arm2-all-nfp-pushes* ())
           (*arm2-nfp-vars* ()))
      (declare (dynamic-extent *arm2-gpr-locations* *arm2-gpr-constants*))
      (set-fill-pointer
       *backend-labels*
       (set-fill-pointer
        *arm2-undo-stack*
        (set-fill-pointer 
         *arm2-undo-because*
         (set-fill-pointer
          *backend-immediates* 0))))
      (backend-get-next-label)          ; start @ label 1, 0 is confused with NIL in compound cd
      (let* ((vinsns (make-vinsn-list))
             (*vinsn-list* vinsns))
        (unwind-protect
             (progn
               (setq bits (arm2-toplevel-form vinsns (make-wired-lreg *arm2-result-reg*) $backend-return (afunc-acode afunc)))
               (dotimes (i (length *backend-immediates*))
                 (let ((imm (aref *backend-immediates* i)))
                   (when (arm2-symbol-locative-p imm) (aset *backend-immediates* i (car imm)))))
               (optimize-vinsns vinsns)
               (when (logbitp arm2-debug-vinsns-bit *arm2-debug-mask*)
                 (format t "~% vinsns for ~s (after generation)" (afunc-name afunc))
                 (do-dll-nodes (v vinsns) (format t "~&~s" v))
                 (format t "~%~%"))
            
               (with-dll-node-freelist (code arm::*lap-instruction-freelist*)
                 (with-dll-node-freelist (data arm::*lap-instruction-freelist*)
                   (let* ((arm::*lap-labels* nil)
                          (sections (vector code data))
                          debug-info)
                     (declare (dynamic-extent sections))
                     (arm2-expand-vinsns vinsns code sections)
                     (if (logbitp $fbitnonnullenv (the fixnum (afunc-bits afunc)))
                       (setq bits (+ bits (ash 1 $lfbits-nonnullenv-bit))))
                     (setq debug-info (afunc-lfun-info afunc))
                     (when lambda-form
                       (setq debug-info (list* 'function-lambda-expression lambda-form debug-info)))
                     (when *arm2-recorded-symbols*
                       (setq debug-info (list* 'function-symbol-map *arm2-recorded-symbols* debug-info)))
                     (when (and (getf debug-info '%function-source-note) *arm2-emitted-source-notes*)
                       (setq debug-info (list* 'pc-source-map *arm2-emitted-source-notes* debug-info)))
                     (when debug-info
                       (setq bits (logior (ash 1 $lfbits-info-bit) bits))
                       (backend-new-immediate debug-info))
                     (if (or fname lambda-form *arm2-recorded-symbols*)
                       (backend-new-immediate fname)
                       (setq bits (logior (ash -1 $lfbits-noname-bit) bits)))
                     
                     (unless (afunc-parent afunc)
                       (arm2-fixup-fwd-refs afunc))
                     (setf (afunc-all-vars afunc) nil)
                     (setf (afunc-argsword afunc) bits)
                     (setf (afunc-lfun afunc)
                           (arm2-xmake-function
                            code
                            *backend-immediates*
                            bits
                            data))
                     (when (getf debug-info 'pc-source-map)
                       (setf (getf debug-info 'pc-source-map) (arm2-generate-pc-source-map debug-info)))
                     (when (getf debug-info 'function-symbol-map)
                       (setf (getf debug-info 'function-symbol-map) (arm2-digest-symbols))))))))))
    afunc))

(defun arm2-xmake-function (code imms bits &optional data)
  (collect ((lap-imms))
    (dotimes (i (length imms))
      (lap-imms (cons (aref imms i) i)))
    (let* ((arm::*arm-constants* (lap-imms)))
      (arm-lap-generate-code code
                             (arm::arm-finalize code (if data (arm-drain-constant-pool code data)))
                             bits))))


      
    
(defun arm2-make-stack (size &optional (subtype target::subtag-s16-vector))
  (make-uarray-1 subtype size t 0 nil nil nil nil t nil))

(defun arm2-fixup-fwd-refs (afunc)
  (dolist (f (afunc-inner-functions afunc))
    (arm2-fixup-fwd-refs f))
  (let ((fwd-refs (afunc-fwd-refs afunc)))
    (when fwd-refs
      (let* ((v (afunc-lfun afunc))
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
  (declaim (inline arm2-invalidate-regmap)))

(defun arm2-invalidate-regmap ()
  (setq *arm2-gpr-locations-valid-mask* 0
        *arm2-gpr-constants-valid-mask* 0))


(defun arm2-update-regmap (vinsn)
  (if (vinsn-attribute-p vinsn :call)
    (arm2-invalidate-regmap)
    (let* ((clobbered-regs (vinsn-gprs-set vinsn)))
      (setq *arm2-gpr-locations-valid-mask* (logandc2 *arm2-gpr-locations-valid-mask* clobbered-regs)
            *arm2-gpr-constants-valid-mask* (logandc2 *arm2-gpr-constants-valid-mask* clobbered-regs))
))
  vinsn)

(defun arm2-invalidate-regmap-entry (i loc)
  (when (and (logbitp i *arm2-gpr-locations-valid-mask*)
             (memq loc (svref *arm2-gpr-locations* i)))
    (when (null (setf (svref *arm2-gpr-locations* i)
                      (delete loc (svref *arm2-gpr-locations* i))))
      (setq *arm2-gpr-locations-valid-mask* (logandc2 *arm2-gpr-locations-valid-mask* (ash 1 i)))))  )

(defun arm2-regmap-note-store (gpr loc)
  (let* ((gpr (%hard-regspec-value gpr)))
    ;; Any other GPRs that had contained loc no longer do so.
    (dotimes (i 16)
      (unless (eql i gpr)
        (arm2-invalidate-regmap-entry i loc)))
    (if (logbitp gpr *arm2-gpr-locations-valid-mask*)
      (push loc (svref *arm2-gpr-locations* gpr))
      (setf (svref *arm2-gpr-locations* gpr) (list loc)))
    
    (setq *arm2-gpr-locations-valid-mask* (logior *arm2-gpr-locations-valid-mask* (ash 1 gpr)))))
  
;;; For vpush: nothing else should claim to contain loc.
(defun arm2-regmap-note-reg-location (gpr loc)
  (let* ((gpr (%hard-regspec-value gpr)))
    (if (logbitp gpr *arm2-gpr-locations-valid-mask*)
      (push loc (svref *arm2-gpr-locations* gpr))
      (setf (svref *arm2-gpr-locations* gpr) (list loc)))
    (setq *arm2-gpr-locations-valid-mask* (logior *arm2-gpr-locations-valid-mask* (ash 1 gpr)))))  
  
(defun arm2-regmap-note-vstack-delta (new old)
  (when (< new old)
    (let* ((mask *arm2-gpr-locations-valid-mask*)
           (info *arm2-gpr-locations*))
    (unless (eql 0 mask)
      (dotimes (i 16 (setq *arm2-gpr-locations-valid-mask* mask))
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

(defun arm2-copy-regmap (mask from to)
  (dotimes (i 16)
    (when (logbitp i mask)
      (setf (svref to i) (copy-list (svref from i))))))

(defun arm2-copy-constmap (mask from to)
  (dotimes (i 16)
    (when (logbitp i mask)
      (setf (svref to i) (svref from i)))))
    

(defmacro with-arm2-saved-regmaps ((mask constmask map constmap) &body body)
  `(let* ((,mask *arm2-gpr-locations-valid-mask*)
          (,constmask *arm2-gpr-constants-valid-mask*)
          (,map (make-array 16 :initial-element nil))
          (,constmap (make-array 16)))
    (declare (dynamic-extent ,map ,constmap))
    (arm2-copy-regmap ,mask *arm2-gpr-locations* ,map)
    (arm2-copy-constmap ,constmap *arm2-gpr-constants* ,constmap)
    ,@body))



(defun arm2-generate-pc-source-map (debug-info)
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
      (let* ((pc-start (arm2-vinsn-note-label-address start t))
             (pc-end (arm2-vinsn-note-label-address (vinsn-note-peer start) nil))
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

(defun arm2-vinsn-note-label-address (note &optional start-p sym)
  (let* ((lap-label (vinsn-note-address note)))
    (if lap-label
      (arm::lap-label-address lap-label)
      (compiler-bug "Missing or bad ~s label: ~s" 
                    (if start-p 'start 'end) sym))))

(defun arm2-digest-symbols ()
  (when *arm2-recorded-symbols*
    (setq *arm2-recorded-symbols* (nx2-recorded-symbols-in-arglist-order *arm2-recorded-symbols* *arm2-cur-afunc*))
 (let* ((symlist *arm2-recorded-symbols*)
           (len (length symlist))
           (syms (make-array len))
           (ptrs (make-array (%i+  (%i+ len len) len) :element-type '(unsigned-byte 32)))
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
          (setf (aref ptrs (incf i)) (arm2-vinsn-note-label-address startlab t sym))
          (setf (aref ptrs (incf i)) (arm2-vinsn-note-label-address endlab nil sym))))
      *arm2-recorded-symbols*)))

(defun arm2-decls (decls)
  (if (fixnump decls)
    (locally (declare (fixnum decls))
      (setq *arm2-tail-allow* (neq 0 (%ilogand2 $decl_tailcalls decls))
            *arm2-open-code-inline* (neq 0 (%ilogand2 $decl_opencodeinline decls))
	    *arm2-full-safety* (neq 0 (%ilogand2 $decl_full_safety decls))
            *arm2-reckless* (neq 0 (%ilogand2 $decl_unsafe decls))
            *arm2-float-safety*  (neq 0 (%ilogand2 $decl_float_safety decls))
            *arm2-trust-declarations* (neq 0 (%ilogand2 $decl_trustdecls decls))))))


(defun arm2-check-fixnum-overflow (seg crf target &optional labelno)
  (with-arm-local-vinsn-macros (seg)
    (let* ((no-overflow (backend-get-next-label))
           (label (if labelno (aref *backend-labels* labelno))))
      (! cbranch-false (or label (aref *backend-labels* no-overflow)) crf arm::arm-cond-vs)
      (if *arm2-open-code-inline*
        (! handle-fixnum-overflow-inline target target)
        (let* ((target-other (not (eql (hard-regspec-value target)
                                       arm::arg_z)))
               (arg (if target-other
                      (make-wired-lreg arm::arg_z)
                      target))
               (result (make-wired-lreg arm::arg_z)))
          (when target-other
            (arm2-copy-register seg arg target))
          (! call-subprim-1 result (subprim-name->offset '.SPfix-overflow) arg)
          (when target-other
            (arm2-copy-register seg target result))))
        (when labelno (-> labelno))
        (@ no-overflow))))



          
    
;;; Vpush the first N non-volatile-registers.
(defun arm2-save-nvrs (seg n)
  (declare (fixnum n))
  (when (> n 0)
    (setq *arm2-compiler-register-save-note* (enqueue-vinsn-note seg :regsave))
    (with-arm-local-vinsn-macros (seg)
      (! save-nvrs n))

    (incf *arm2-vstack* (the fixnum (* n *arm2-target-node-size*)))
    (setq *arm2-register-restore-ea* *arm2-vstack*
          *arm2-register-restore-count* n)))

(defun arm2-save-non-volatile-fprs (seg n)
  (unless (eql n 0)
    (with-arm-local-vinsn-macros (seg)
      (! push-nvfprs n (logior (ash n arm::num-subtag-bits) arm::subtag-double-float-vector)))
    (setq *arm2-non-volatile-fpr-count* n)))

(defun arm2-restore-non-volatile-fprs (seg)
  (let* ((n *arm2-non-volatile-fpr-count*))
    (unless (eql n 0)
    (with-arm-local-vinsn-macros (seg)
      (! pop-nvfprs n)))))

      


;;; If there are an indefinite number of args/values on the vstack,
;;; we have to restore from a register that matches the compiler's
;;; notion of the vstack depth.  This can be computed by the caller 
;;; (sum of vsp & nargs, or copy of vsp  before indefinite number of 
;;; args pushed, etc.)
;;; We DON'T try to compute this from the saved context, since the
;;; saved vsp may belong to a different stack segment.  (It's cheaper
;;; to compute/copy than to load it, anyway.)

(defun arm2-restore-nvrs (seg multiple-values-on-stack)
  (let* ((ea *arm2-register-restore-ea*)
         (n *arm2-register-restore-count*))
    (when (and ea n)
      (with-arm-local-vinsn-macros (seg)
        (let* ((diff (- *arm2-vstack* ea)))
          (if (and (eql 0 diff)
                   (not multiple-values-on-stack))
            (! restore-nvrs n arm::vsp)
            (let* ((reg (make-unwired-lreg
                         (if (= *available-backend-imm-temps* 0)
                           (select-node-temp)
                           (select-imm-temp))
                         :class hard-reg-class-gpr
                         :mode hard-reg-class-gpr-mode-node)))
              (if (eql 0 diff)
                (! fixnum-add reg arm::vsp arm::nargs)
                (progn
                  (if (arm::encode-arm-immediate diff)
                    (! add-immediate reg arm::vsp diff)
                    (progn
                      (arm2-lri seg reg diff)
                      (! fixnum-add reg arm::vsp reg)))
                  (when multiple-values-on-stack
                    (! fixnum-add reg reg arm::nargs))))
              (! restore-nvrs n reg))))))))




(defun arm2-bind-lambda (seg req opt rest keys auxen optsupvloc passed-in-regs lexpr &optional inherited
                             &aux (vloc 0)
                             (nkeys (list-length (%cadr keys))) 
                             reg)
  (declare (fixnum vloc))
  (dolist (arg inherited)
    (if (memq arg passed-in-regs)
      (arm2-set-var-ea seg arg (var-ea arg))
      (progn
        (if (setq reg (nx2-assign-register-var arg))
          (arm2-init-regvar seg arg reg (arm2-vloc-ea vloc))
          (arm2-bind-var seg arg vloc ))
        (setq vloc (%i+ vloc *arm2-target-node-size*)))))
  (dolist (arg req)
    (if (memq arg passed-in-regs)
      (arm2-set-var-ea seg arg (var-ea arg))
      (progn

        (if (setq reg (nx2-assign-register-var arg))
          (arm2-init-regvar seg arg reg (arm2-vloc-ea vloc))
          (arm2-bind-var seg arg vloc))
        (setq vloc (%i+ vloc *arm2-target-node-size*)))))
  (when opt
    (if (arm2-hard-opt-p opt)
      (setq vloc (apply #'arm2-initopt seg vloc optsupvloc opt))

      (dolist (var (%car opt))
        (if (memq var passed-in-regs)
          (arm2-set-var-ea seg var (var-ea var))
          (progn
            (if (setq reg (nx2-assign-register-var var))
              (arm2-init-regvar seg var reg (arm2-vloc-ea vloc))
              (arm2-bind-var seg var vloc))
            (setq vloc (+ vloc *arm2-target-node-size*)))))))
  (when rest
    (if lexpr
      (progn
        (if (setq reg (nx2-assign-register-var rest))
          (progn
            (arm2-load-lexpr-address seg reg)
            (arm2-set-var-ea seg rest reg))
          (with-imm-temps () ((nargs-cell :natural))
            (arm2-load-lexpr-address seg nargs-cell)
            (let* ((loc *arm2-vstack*))
              (arm2-vpush-register seg nargs-cell)
              (arm2-bind-var seg rest loc)))))
      (let* ((rvloc (+ vloc (* 2 *arm2-target-node-size* nkeys))))
        (if (setq reg (nx2-assign-register-var rest))
          (arm2-init-regvar seg rest reg (arm2-vloc-ea rvloc))
          (arm2-bind-var seg rest rvloc )))))
  (when keys
    (apply #'arm2-init-keys seg vloc  keys))  
  (arm2-seq-bind seg (%car auxen) (%cadr auxen)))

(defun arm2-initopt (seg vloc spvloc vars inits spvars)
  (with-arm-local-vinsn-macros (seg)
    (dolist (var vars vloc)
      (let* ((initform (pop inits))
             (spvar (pop spvars))
             (reg (nx2-assign-register-var var))
             (sp-reg ($ arm::arg_z))
             (regloadedlabel (if reg (backend-get-next-label))))
        (unless (nx-null initform)
          (arm2-stack-to-register seg (arm2-vloc-ea spvloc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (arm2-compare-register-to-nil seg crf (arm2-make-compound-cd 0 skipinitlabel) sp-reg  arm::arm-cond-eq t))
            (if reg
              (arm2-form seg reg regloadedlabel initform)
              (arm2-register-to-stack seg (arm2-one-untargeted-reg-form seg initform ($ arm::arg_z)) (arm2-vloc-ea vloc)))
            (@ skipinitlabel)))
        (if reg
          (progn
            (arm2-init-regvar seg var reg (arm2-vloc-ea vloc))
            (@ regloadedlabel))
          (arm2-bind-var seg var vloc))
        (when spvar
          (if (setq reg (nx2-assign-register-var spvar))
            (arm2-init-regvar seg spvar reg (arm2-vloc-ea spvloc))
            (arm2-bind-var seg spvar spvloc))))
      (setq vloc (%i+ vloc *arm2-target-node-size*))
      (if spvloc (setq spvloc (%i+ spvloc *arm2-target-node-size*))))))

(defun arm2-init-keys (seg vloc allow-others keyvars keysupp keyinits keykeys)
  (declare (ignore keykeys allow-others))
  (with-arm-local-vinsn-macros (seg)
    (dolist (var keyvars)
      (let* ((spvar (pop keysupp))
             (initform (pop keyinits))
             (reg (nx2-assign-register-var var))
             (regloadedlabel (if reg (backend-get-next-label)))

             (sp-reg ($ arm::arg_z))
             (sploc (%i+ vloc *arm2-target-node-size*)))
        (unless (nx-null initform)
          (arm2-stack-to-register seg (arm2-vloc-ea sploc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (arm2-compare-register-to-nil seg crf (arm2-make-compound-cd 0 skipinitlabel) sp-reg  arm::arm-cond-eq t))
            (if reg
              (arm2-form seg reg regloadedlabel initform)
              (arm2-register-to-stack seg (arm2-one-untargeted-reg-form seg initform ($ arm::arg_z)) (arm2-vloc-ea vloc)))
            (@ skipinitlabel)))
        (if reg
          (progn
            (arm2-init-regvar seg var reg (arm2-vloc-ea vloc))
            (@ regloadedlabel))
          (arm2-bind-var seg var vloc ))
        (when spvar
          (if (setq reg (nx2-assign-register-var spvar))
            (arm2-init-regvar seg spvar reg (arm2-vloc-ea sploc))
            (arm2-bind-var seg spvar sploc ))))
      (setq vloc (%i+ vloc (* 2 *arm2-target-node-size*))))))

;;; Return NIL if arg register should be vpushed, else var.
(defun arm2-retain-arg-register (var)
  (if var
    (when (var-nvr var)
      var)
    (compiler-bug "Missing var!")))


;;; nargs has been validated, arguments defaulted and canonicalized.
;;; Save caller's context, then vpush any argument registers that
;;; didn't get global registers assigned to their variables.
;;; Return a list of vars/nils for each argument register 
;;;  (nil if vpushed, var if still in arg_reg).
(defun arm2-argregs-entry (seg revargs)
  (with-arm-local-vinsn-macros (seg)
    (let* ((nargs (length revargs))
           (reg-vars ()))
      (declare (type (unsigned-byte 16) nargs))
      (when (and 
             (<= nargs $numarmargregs)
             (not (some #'null revargs)))
        (setq *arm2-fixed-nargs* nargs))
      (if (<= nargs $numarmargregs)       ; caller didn't vpush anything
        (! save-lisp-context-vsp)
        (let* ((offset (* (the fixnum (- nargs $numarmargregs)) *arm2-target-node-size*)))
          (declare (fixnum offset))
          (! save-lisp-context-offset offset)))
      (when *arm2-fixed-args-label*
        (@ (setq *arm2-fixed-args-tail-label* (backend-get-next-label))))
      (destructuring-bind (&optional zvar yvar xvar &rest stack-args) revargs
        (let* ((nstackargs (length stack-args)))
          (arm2-set-vstack (* nstackargs *arm2-target-node-size*))

          (let* ((mask 0))
            (declare (fixnum mask))
            (when (>= nargs 3)
              (let* ((retain-x (arm2-retain-arg-register xvar)))
                (push retain-x reg-vars)
                (unless retain-x
                  (setq mask (logior (ash 1 arm::arg_x) mask))
                  (arm2-regmap-note-store arm::arg_x *arm2-vstack*)
                  (arm2-adjust-vstack *arm2-target-node-size*))))
            (when (>= nargs 2)
              (let* ((retain-y (arm2-retain-arg-register yvar)))
                (push retain-y reg-vars)
                (unless retain-y
                  (setq mask (logior (ash 1 arm::arg_y) mask))
                  (arm2-regmap-note-store arm::arg_y *arm2-vstack*)

                  (arm2-adjust-vstack *arm2-target-node-size*))))
            (when (>= nargs 1)
              (let* ((retain-z (arm2-retain-arg-register zvar)))
                (push retain-z reg-vars)
                (unless retain-z
                  (setq mask (logior (ash 1 arm::arg_z) mask))
                  (arm2-regmap-note-store arm::arg_z *arm2-vstack*)

                  (arm2-adjust-vstack *arm2-target-node-size*))))
            (unless (eql 0 mask)
              (if (eql (logcount mask) 1)
                (! vpush-register ($ (1- (integer-length mask))))
                (! vpush-multiple-registers mask))))))
      reg-vars)))

;;; Just required args.
;;; Since this is just a stupid bootstrapping port, always save 
;;; lisp context.
(defun arm2-req-nargs-entry (seg rev-fixed-args)
  (let* ((nargs (length rev-fixed-args)))
    (declare (type (unsigned-byte 16) nargs))
    (with-arm-local-vinsn-macros (seg)
      (unless *arm2-reckless*
        (if (arm::encode-arm-immediate (ash nargs arm::fixnumshift))
          (! check-exact-nargs nargs)
          (! check-exact-nargs-large nargs)))
      (arm2-argregs-entry seg rev-fixed-args))))

;;; No more than three &optional args; all default to NIL and none have
;;; supplied-p vars.  No &key/&rest.
(defun arm2-simple-opt-entry (seg rev-opt-args rev-req-args)
  (let* ((min (length rev-req-args))
         (nopt (length rev-opt-args))
         (max (+ min nopt)))
    (declare (type (unsigned-byte 16) min nopt max))
    (with-arm-local-vinsn-macros (seg)
      (unless *arm2-reckless*
        (when rev-req-args
          (if (arm::encode-arm-immediate min)
            (! check-min-nargs min)
            (! check-min-nargs-large min)))
        (if (arm::encode-arm-immediate max)
          (! check-max-nargs max)
          (! check-max-nargs-large max)))
      (if (= nopt 1)
        (! default-1-arg min)
        (if (= nopt 2)
          (! default-2-args min)
          (! default-3-args min)))
      (arm2-argregs-entry seg (append rev-opt-args rev-req-args)))))

;;; if "num-fixed" is > 0, we've already ensured that at least that many args
;;; were provided; that may enable us to generate better code for saving the
;;; argument registers.
;;; We're responsible for computing the caller's VSP and saving
;;; caller's state.
(defun arm2-lexpr-entry (seg num-fixed)
  (with-arm-local-vinsn-macros (seg)
    (! save-lexpr-argregs num-fixed)
    (dotimes (i num-fixed)
      (! copy-lexpr-argument))
    (! save-lisp-context-vsp)))

(defun arm2-load-lexpr-address (seg dest)
  (with-arm-local-vinsn-macros (seg)
    (! load-vframe-address dest *arm2-vstack*)))








(defun arm2-vloc-ea (n &optional vcell-p)
  (setq n (make-memory-spec (dpb memspec-frame-address memspec-type-byte n)))
  (if vcell-p
    (make-vcell-memory-spec n)
    n))


(defun arm2-acode-operator-function (form)
  (or (and (acode-p form)
           (svref *arm2-specials* (%ilogand #.operator-id-mask (acode-operator form))))
      (compiler-bug "arm2-form ? ~s" form)))

(defmacro arm-with-note ((form-var seg-var &rest other-vars) &body body)
  (let* ((note (gensym "NOTE"))
         (code-note (gensym "CODE-NOTE"))
         (source-note (gensym "SOURCE-NOTE"))
         (start (gensym "START"))
         (arm-with-note-body (gensym "ARM-WITH-NOTE-BODY")))
    `(flet ((,arm-with-note-body (,form-var ,seg-var ,@other-vars) ,@body))
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
                   (with-arm-local-vinsn-macros (,seg-var)
                     (arm2-store-immediate ,seg-var ,code-note arm::temp0)
                     (with-node-temps (arm::temp0) (zero)
                       (! lri zero 0)
                       (! misc-set-c-node ($ zero) ($ arm::temp0) 1))))
                 (,arm-with-note-body ,form-var ,seg-var ,@other-vars)
               (when ,source-note
                 (close-vinsn-note ,seg-var ,start))))
           (,arm-with-note-body ,form-var ,seg-var ,@other-vars))))))

(defun arm2-toplevel-form (seg vreg xfer form)
  (let* ((code-note (acode-note form))
         (args (if code-note `(,@(acode-operands form) ,code-note) (acode-operands form))))
    (apply (arm2-acode-operator-function form) seg vreg xfer args)))

(defun arm2-form (seg vreg xfer form)
  (arm-with-note (form seg vreg xfer)
    (if (nx-null form)
      (arm2-nil seg vreg xfer)
      (if (nx-t form)
        (arm2-t seg vreg xfer)
        (let ((fn (arm2-acode-operator-function form))
              (op (acode-operator form)))
          (if (and (null vreg)
                   (%ilogbitp operator-acode-subforms-bit op)
                   (%ilogbitp operator-assignment-free-bit op)
                   (%ilogbitp operator-side-effect-free-bit op))
            (dolist (f (acode-operands form) (arm2-branch seg xfer nil))
              (arm2-form seg nil nil f ))
            (apply fn seg vreg xfer (acode-operands form))))))))

;;; dest is a float reg - form is acode
(defun arm2-form-float (seg freg xfer form)
  (declare (ignore xfer))
  (arm-with-note (form seg freg)
    (when (or (nx-null form)(nx-t form))(compiler-bug "arm2-form to freg ~s" form))
    (when (and (= (get-regspec-mode freg) hard-reg-class-fpr-mode-double)
               (arm2-form-typep form 'double-float))
                                        ; kind of screwy - encoding the source type in the dest register spec
      (set-node-regspec-type-modes freg hard-reg-class-fpr-type-double))
    (let* ((fn (arm2-acode-operator-function form)))
      (apply fn seg freg nil (acode-operands form)))))



(defun arm2-form-typep (form type)
  (acode-form-typep form type *arm2-trust-declarations*)
)

(defun arm2-form-type (form)
  (acode-form-type form *arm2-trust-declarations*))
  
(defun arm2-use-operator (op seg vreg xfer &rest forms)
  (declare (dynamic-extent forms))
  (apply (svref *arm2-specials* (%ilogand operator-id-mask op)) seg vreg xfer forms))


  
(defun arm2-nil (seg vreg xfer)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if (arm2-for-value-p vreg)
      (ensuring-node-target (target vreg)
        (let* ((regval (hard-regspec-value target))
               (regs (arm2-gprs-containing-constant nil)))
          (unless (logbitp regval regs)
            (! load-nil target)
            (setf *arm2-gpr-constants-valid-mask*
                  (logior *arm2-gpr-constants-valid-mask* (ash 1 regval))
                  (svref *arm2-gpr-constants* regval) nil)))))
    (arm2-branch seg (arm2-cd-false xfer) vreg)))

(defun arm2-t (seg vreg xfer)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if (arm2-for-value-p vreg)
      (ensuring-node-target (target vreg)
        (let* ((regval (hard-regspec-value target))
               (regs (arm2-gprs-containing-constant t)))
          (declare (fixnum regval regs))
          (unless (logbitp regval regs)
            (if (zerop regs)
              (! load-t target)
              (let* ((r (1- (integer-length regs))))
                (! copy-node-gpr target r)))
            (setf *arm2-gpr-constants-valid-mask*
                  (logior *arm2-gpr-constants-valid-mask* (ash 1 regval))
                  (svref *arm2-gpr-constants* regval) t)))))
    (arm2-branch seg (arm2-cd-true xfer) vreg)))

    

(defun arm2-for-value-p (vreg)
  (and vreg (not (backend-crf-p vreg))))

(defun arm2-mvpass (seg form &optional xfer)
  (with-arm-local-vinsn-macros (seg)
    (arm2-form seg  ($ arm::arg_z) (logior (or xfer 0) $backend-mvpass-mask) form)))

(defun arm2-adjust-vstack (delta)
  (arm2-set-vstack (%i+ *arm2-vstack* delta)))

(defun arm2-set-vstack (new)
  (arm2-regmap-note-vstack-delta new *arm2-vstack*)
  (setq *arm2-vstack* new))



(defun arm2-register-for-frame-offset (offset &optional suggested)
  (let* ((mask *arm2-gpr-locations-valid-mask*)
         (info *arm2-gpr-locations*))
    (if (and suggested
             (logbitp suggested mask)
             (memq offset (svref info suggested)))
      suggested
      (dotimes (reg 16)
        (when (and (logbitp reg mask)
                   (memq offset (svref info reg)))
          (return reg))))))

(defun arm2-reg-for-ea (ea)
  (when (and (memory-spec-p ea)
             (eql (memspec-type ea) memspec-frame-address)
             (not (addrspec-vcell-p ea)))
    (let* ((offset (memspec-frame-address-offset ea))
           (mask *arm2-gpr-locations-valid-mask*)
           (info *arm2-gpr-locations*))
      (declare (fixnum mask) (simple-vector info))
      (dotimes (reg 16)
        (when (and (logbitp reg mask)
                   (memq offset (svref info reg)))
          (return reg))))))

(defun arm2-reg-for-form (form hint)
  (let* ((var (arm2-lexical-reference-p form)))
    (cond ((node-reg-p hint)
           (if var
             (arm2-reg-for-ea (var-ea var))
             (multiple-value-bind (value constantp) (acode-constant-p form)
               (when constantp
                 (let* ((regs (arm2-gprs-containing-constant value))
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
             (let* ((val (acode-constant-p form)))
               (if (and (= (get-regspec-mode hint) hard-reg-class-fpr-mode-single)
                        (eql val 0.0f0))
                 (make-hard-fp-reg (hard-regspec-value arm::single-float-zero) hard-reg-class-fpr-mode-single)
                 (if (and (= (get-regspec-mode hint) hard-reg-class-fpr-mode-double)
                          (eql val 0.0d0))
                   (make-hard-fp-reg (hard-regspec-value arm::double-float-zero))))))))))
         
    
                 
            
          

(defun arm2-stack-to-register (seg memspec reg)
  (with-arm-local-vinsn-macros (seg)
    (let* ((offset (memspec-frame-address-offset memspec)))
      (if (eql (hard-regspec-class reg) hard-reg-class-fpr)
        (with-node-target () temp
          (arm2-stack-to-register seg memspec temp)
          (arm2-copy-register seg reg temp))
        (let*((mask *arm2-gpr-locations-valid-mask*)
              (info *arm2-gpr-locations*)
              (regno (%hard-regspec-value reg))
              (other (arm2-register-for-frame-offset offset regno)))
          (unless (eql regno other)
            (cond (other
                   (let* ((vinsn (! copy-node-gpr reg other)))
                     (setq *arm2-gpr-locations-valid-mask*
                           (logior mask (ash 1 regno)))
                     (setf (svref info regno)
                           (copy-list (svref info other)))
                     vinsn))
                  (t
                   (let* ((vinsn (! vframe-load reg offset *arm2-vstack*)))
                     (setq *arm2-gpr-locations-valid-mask*
                           (logior mask (ash 1 regno)))
                     (setf (svref info regno) (list offset))
                     vinsn)))))))))




(defun arm2-register-to-stack (seg reg memspec)
  (with-arm-local-vinsn-macros (seg)
    (let* ((offset (memspec-frame-address-offset memspec))
           (vinsn (! vframe-store reg offset *arm2-vstack*)))
      (arm2-regmap-note-store (%hard-regspec-value reg) offset)
      vinsn)))


(defun arm2-ea-open (ea)
  (if (and ea (not (typep ea 'lreg)) (addrspec-vcell-p ea))
    (make-memory-spec (memspec-frame-address-offset ea))
    ea))

(defun arm2-set-NARGS (seg n)
  (if (> n call-arguments-limit)
    (compiler-bug "~s exceeded." call-arguments-limit)
    (if (< n 256)      
      (with-arm-local-vinsn-macros (seg)
        (! set-nargs n))
      (arm2-lri seg arm::nargs (ash n arm::word-shift)))))

(defun arm2-single-float-bits (the-sf)
  (single-float-bits the-sf))

(defun arm2-double-float-bits (the-df)
  (double-float-bits the-df))

(defun arm2-immediate (seg vreg xfer form)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
               (or (and (typep form 'double-float) (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
                   (and (typep form 'short-float)(= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))))
        (if (zerop form)
          (if (eql form 0.0d0)
            (! zero-double-float-register vreg)
            (! zero-single-float-register vreg))
          (if (typep form 'short-float)
            (let* ((bits (arm2-single-float-bits form)))
              (! load-single-float-constant-from-data vreg bits))
            (multiple-value-bind (high low) (arm2-double-float-bits form)
              (declare (integer high low))
              (! load-double-float-constant-from-data vreg high low))))
        (if (and (typep form '(unsigned-byte 32))
                 (= (hard-regspec-class vreg) hard-reg-class-gpr)
                 (= (get-regspec-mode vreg)
                    hard-reg-class-gpr-mode-u32))
          (arm2-lri seg vreg form)
          (ensuring-node-target (target vreg)
            (let* ((regno (hard-regspec-value target))
                   (regs (arm2-gprs-containing-constant form)))
              (unless (logbitp regno regs)
                (if (eql 0 regs)
                  (if (characterp form)
                    (! load-character-constant target (char-code form))
                    (arm2-store-immediate seg form target))
                  (let* ((r (1- (integer-length regs))))
                    (! copy-node-gpr target r)))
                (setf *arm2-gpr-constants-valid-mask*
                      (logior *arm2-gpr-constants-valid-mask*
                              (ash 1 regno))
                      (svref *arm2-gpr-constants* regno) form))))))
        (if (and (listp form) *load-time-eval-token* (eq (car form) *load-time-eval-token*))
          (arm2-store-immediate seg form ($ arm::temp0))))
    (^)))

(defun arm2-register-constant-p (form)
  (and (consp form)
           (or (memq form *arm2-vcells*)
               (memq form *arm2-fcells*))
           (%cdr form)))

(defun arm2-store-immediate (seg imm dest)
  (with-arm-local-vinsn-macros (seg)
    (let* ((reg (arm2-register-constant-p imm)))
      (if reg
        (arm2-copy-register seg dest reg)
        (let* ((idx (backend-immediate-index imm)))
          (if (< idx 4094)
            (! ref-constant dest idx)
            (with-imm-target () (idxreg :s32)
              (arm2-lri seg idxreg (+ arm::misc-data-offset (ash (1+ idx) 2)))
              (! ref-indexed-constant dest idxreg)))))
      dest)))


;;; Returns label iff form is (local-go <tag>) and can go without adjusting stack.
(defun arm2-go-label (form)
  (let ((current-stack (arm2-encode-stack)))
    (while (and (acode-p form) (or (eq (acode-operator form) (%nx1-operator progn))
                                   (eq (acode-operator form) (%nx1-operator local-tagbody))))
      (setq form (caar  (acode-operands form))))
    (when (acode-p form)
      (let ((op (acode-operator form)))
        (if (and (eq op (%nx1-operator local-go))
                 (arm2-equal-encodings-p (%caddr (car (acode-operands form))) current-stack))
          (%cadr (car (acode-operands form)))
          (if (and (eq op (%nx1-operator local-return-from))
                   (nx-null (cadr (acode-operands form))))
            (let ((tagdata (car (car (acode-operands form)))))
              (and (arm2-equal-encodings-p (cdr tagdata) current-stack)
                   (null (caar tagdata))
                   (< 0 (cdar tagdata) $backend-mvpass)
                   (cdar tagdata)))))))))

(defun arm2-single-valued-form-p (form)
  (setq form (acode-unwrapped-form-value form))
  (or (nx-null form)
      (nx-t form)
      (if (acode-p form)
        (let ((op (acode-operator form)))
          (or (%ilogbitp operator-single-valued-bit op)
              (and (eql op (%nx1-operator values))
                   (let ((values (car (acode-operands form))))
                     (and values (null (cdr values)))))
              nil                       ; Learn about functions someday
              )))))


(defun arm2-box-s32 (seg node-dest s32-src)
  (with-arm-local-vinsn-macros (seg)
    (if *arm2-open-code-inline*
      (! s32->integer node-dest s32-src)
      (let* ((arg_z ($ arm::arg_z))
             (imm0 ($ arm::imm0 :mode :s32)))
        (arm2-copy-register seg imm0 s32-src)
        (! call-subprim (arm::arm-subprimitive-offset '.SPmakes32))
        (arm2-copy-register seg node-dest arg_z)))))



(defun arm2-box-u32 (seg node-dest u32-src)
  (with-arm-local-vinsn-macros (seg)
    (if *arm2-open-code-inline*
      (! u32->integer node-dest u32-src)
      (let* ((arg_z ($ arm::arg_z))
             (imm0 ($ arm::imm0 :mode :u32)))
        (arm2-copy-register seg imm0 u32-src)
        (! call-subprim (arm::arm-subprimitive-offset '.SPmakeu32))
        (arm2-copy-register seg node-dest arg_z)))))



(defun arm2-vref1 (seg vreg xfer type-keyword src unscaled-idx index-known-fixnum)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (let* ((arch (backend-target-arch *target-backend*))
             (is-node (member type-keyword (arch::target-gvector-types arch)))
             (is-1-bit (member type-keyword (arch::target-1-bit-ivector-types arch)))

             (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
             (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
             (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
             (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
             (is-128-bit (eq type-keyword :complex-double-float-vector))
             (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector)))
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
             (if (and index-known-fixnum (<= index-known-fixnum
                                             (arch::target-max-32-bit-constant-index arch)))
               (! misc-ref-c-node target src index-known-fixnum)
               (with-imm-target () (idx-reg :u64)
                 (if index-known-fixnum
                   (arm2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum *arm2-target-node-shift*)))
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
               (if (and index-known-fixnum (<= index-known-fixnum
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
                     (arm2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 2)))
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
                      (arm2-box-s32 seg target temp))))
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
                      (arm2-box-u32 seg target temp))))))))
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
                   (arm2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) index-known-fixnum))
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
                     (arm2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 1)))
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
                  (setq fp-val vreg)
                  )
                (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                  (! misc-ref-c-double-float fp-val src index-known-fixnum)
                  (with-imm-target () idx-reg
                    (if index-known-fixnum
                      (unless unscaled-idx
                        (setq unscaled-idx idx-reg)
                        (arm2-absolute-natural seg unscaled-idx nil (ash index-known-fixnum arm::fixnumshift))))
                    (! misc-ref-double-float fp-val src unscaled-idx)))
                (if (eq vreg-class hard-reg-class-fpr)
                  (<- fp-val)
                  (ensuring-node-target (target vreg)
                    (! double->heap target fp-val)))))
             (:complex-single-float-vector
              (with-fp-target () (fp-val :complex-single-float)

                (if (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-complex-single-float))
                  (setq fp-val vreg)
                  )
                (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                  (! misc-ref-c-double-float fp-val src index-known-fixnum)
                  (with-imm-target () idx-reg
                    (if index-known-fixnum
                      (unless unscaled-idx
                        (setq unscaled-idx idx-reg)
                        (arm2-absolute-natural seg unscaled-idx nil (ash index-known-fixnum arm::fixnumshift))))
                    (! misc-ref-double-float fp-val src unscaled-idx)))
                (if (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-complex-single-float))
                  (<- fp-val)
                  (ensuring-node-target (target vreg)
                    (! complex-single-float->node target fp-val)))))))
          (is-128-bit
              (with-fp-target () (fp-val :complex-double-float)

                (if (and (eql vreg-class hard-reg-class-fpr)
                         (eql vreg-mode hard-reg-class-fpr-mode-complex-double-float))
                  (setq fp-val vreg)
                  (with-imm-target () idx-reg
                    (if index-known-fixnum
                      (unless unscaled-idx
                        (setq unscaled-idx idx-reg)
                        (arm2-absolute-natural seg unscaled-idx nil (ash index-known-fixnum arm::fixnumshift))))
                    (! misc-ref-complex-double-float fp-val src unscaled-idx))                  )
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
                     (arm2-lri seg word-index (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum -5)))
                     (arm2-lri seg bitnum (logand index-known-fixnum #x1f)))
                   (! scale-1bit-misc-index word-index bitnum unscaled-idx))
                 (let* ((dest word-index))
                   (! misc-ref-u32 dest src word-index)
                   (! extract-variable-bit-fixnum target dest bitnum)))))))))
    (^)))
             
    

;;; safe = T means assume "vector" is miscobj, do bounds check.
;;; safe = fixnum means check that subtag of vector = "safe" and do
;;;        bounds check.
;;; safe = nil means crash&burn.
;;; This mostly knows how to reference the elements of an immediate miscobj.
(defun arm2-vref (seg vreg xfer type-keyword vector index safe)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((index-known-fixnum (acode-fixnum-form-p index))
           (unscaled-idx nil)
           (src nil))
      (if (or safe (not index-known-fixnum))
        (multiple-value-setq (src unscaled-idx)
          (arm2-two-untargeted-reg-forms seg vector arm::arg_y index arm::arg_z))
        (setq src (arm2-one-untargeted-reg-form seg vector arm::arg_z)))
      (when safe
        (if (typep safe 'fixnum)
          (! trap-unless-typecode= src safe))
        (unless index-known-fixnum
          (! trap-unless-fixnum unscaled-idx))
        (! check-misc-bound unscaled-idx src))
      (arm2-vref1 seg vreg xfer type-keyword src unscaled-idx index-known-fixnum))))

(defun arm2-1d-vref (seg vreg xfer type-keyword vector index safe)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((simple-case (backend-get-next-label))
           (common-case (backend-get-next-label)))
      (multiple-value-bind (src unscaled-idx)
          (arm2-two-untargeted-reg-forms seg vector ($ arm::arg_y) index ($ arm::arg_z))
        (with-crf-target () crf
          (! set-z-if-vector-header crf src)
          (arm2-branch seg (arm2-make-compound-cd simple-case 0) crf arm::arm-cond-eq nil)
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
          (arm2-vref1 seg vreg xfer type-keyword src unscaled-idx nil))))))
        
        





(defun arm2-aset2-via-gvset (seg vreg xfer  array i j new safe type-keyword  constval &optional (simple t))
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (src ($ arm::temp0))
           (unscaled-i ($ arm::arg_x))
           (unscaled-j ($ arm::arg_y))
           (val-reg ($ arm::arg_z)))
      (arm2-four-targeted-reg-forms seg
                                    array src
                                    i unscaled-i
                                    j unscaled-j
                                    new val-reg)
      (when safe
        (when (typep safe 'fixnum)
          (with-node-target (src unscaled-i unscaled-j val-reg) expected
            (if simple
              (progn
                (! lri expected
                   (ash (dpb safe target::arrayH.flags-cell-subtag-byte
                             (ash 1 $arh_simple_bit))
                        arm::fixnumshift))
                (! trap-unless-simple-array-2 src expected))
              (! trap-unless-typed-array-2 src safe))))
        (unless i-known-fixnum
          (! trap-unless-fixnum unscaled-i))
        (unless j-known-fixnum
          (! trap-unless-fixnum unscaled-j)))
      (with-imm-target () dim1
        (let* ((idx-reg ($ arm::arg_y)))
          (progn
            (if safe                  
              (! check-2d-bound dim1 unscaled-i unscaled-j src)
              (! 2d-dim1 dim1 src))
            (! 2d-unscaled-index idx-reg dim1 unscaled-i unscaled-j))
          (let* ((v ($ arm::arg_x)))
            (if simple
              (! array-data-vector-ref v src)
              (progn
                (arm2-copy-register seg v src)
                (! deref-vector-header v idx-reg)))
            (arm2-vset1 seg vreg xfer type-keyword v idx-reg nil val-reg (arm2-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval t)))))))
      
  
(defun arm2-aset2 (seg vreg xfer  array i j new safe type-keyword dim0 dim1 &optional (simple t))
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (arm2-constant-value-ok-for-type-keyword type-keyword new))
           (needs-memoization (and is-node (arm2-acode-needs-memoization new))))
      (if needs-memoization
        (arm2-aset2-via-gvset seg vreg xfer array i j new safe type-keyword constval simple)
        (let* ((constidx
                (and *arm2-reckless*
                     dim0 dim1 i-known-fixnum j-known-fixnum
                     (>= i-known-fixnum 0)
                     (>= j-known-fixnum 0)
                     (< i-known-fixnum dim0)
                     (< j-known-fixnum dim1)
                     (+ (* i-known-fixnum dim1) j-known-fixnum)))
               (val-reg (arm2-target-reg-for-aset vreg type-keyword))
               (node-val (if (node-reg-p val-reg) val-reg))
               (imm-val (if (imm-reg-p val-reg) val-reg)))
          (with-node-target (node-val) src
            (with-node-target (node-val src) unscaled-i
              (with-node-target (node-val src unscaled-i) unscaled-j
                (if constidx
                  (multiple-value-setq (src val-reg)
                    (arm2-two-untargeted-reg-forms seg array ($ arm::temp0) new val-reg))
                  (multiple-value-setq (src unscaled-i unscaled-j val-reg)
                    (arm2-four-untargeted-reg-forms seg
                                                    array src
                                                    i unscaled-i
                                                    j unscaled-j
                                                    new val-reg)))
                (if (node-reg-p val-reg) (setq node-val val-reg))
                (if (imm-reg-p val-reg) (setq imm-val val-reg))
                (let* ((*available-backend-imm-temps* *available-backend-imm-temps*)
                       )
                  (when (and (= (hard-regspec-class val-reg) hard-reg-class-gpr)
                             (logbitp (hard-regspec-value val-reg)
                                      *backend-imm-temps*))
                    (use-imm-temp (hard-regspec-value val-reg)))
                  (when safe      
                    (when (typep safe 'fixnum)
                      (with-node-target (src node-val unscaled-i unscaled-j) expected
                        (if simple
                          (progn
                            (! lri expected
                               (ash (dpb safe target::arrayH.flags-cell-subtag-byte
                                         (ash 1 $arh_simple_bit))
                                    arm::fixnumshift))
                            (! trap-unless-simple-array-2 src expected))
                          (! trap-unless-typed-array-2 src safe))))
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
                        (if safe
                          (! array-data-vector-ref v src)
                          (progn
                            (setq v src)
                            (! deref-vector-header src idx-reg)))
                        (arm2-vset1 seg vreg xfer type-keyword 
v idx-reg constidx val-reg (arm2-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization)))))))))))))


(defun arm2-aset3 (seg vreg xfer  array i j k new safe type-keyword  dim0 dim1 dim2 &optional (simple t))
  (with-arm-local-vinsn-macros (seg target)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (k-known-fixnum (acode-fixnum-form-p k))
           (arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (arm2-constant-value-ok-for-type-keyword type-keyword new))
           (needs-memoization (and is-node (arm2-acode-needs-memoization new)))
           (src)
           (unscaled-i)
           (unscaled-j)
           (unscaled-k)
           (val-reg (arm2-target-reg-for-aset vreg type-keyword))
           (constidx
            (and *arm2-reckless*
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
            (arm2-two-targeted-reg-forms seg array ($ arm::temp0) new val-reg))
          (progn
            (setq src ($ arm::temp1)
                  unscaled-i ($ arm::temp0)
                  unscaled-j ($ arm::arg_x)
                  unscaled-k ($ arm::arg_y))
            (arm2-push-register
             seg
             (arm2-one-untargeted-reg-form seg array ($ arm::arg_z)))
            (arm2-four-targeted-reg-forms seg
                                          i ($ arm::temp0)
                                          j ($ arm::arg_x)
                                          k ($ arm::arg_y)
                                          new val-reg)
            (arm2-pop-register seg src)))
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*))
          (when (and (= (hard-regspec-class val-reg) hard-reg-class-gpr)
                     (logbitp (hard-regspec-value val-reg)
                              *backend-imm-temps*))
            (use-imm-temp (hard-regspec-value val-reg)))

          (when safe      
            (when (typep safe 'fixnum)
              (if simple
                (let* ((expected (if constidx
                                   
                                   (with-node-target (src val-reg) expected
                                     expected)
                                   (with-node-target (src unscaled-i unscaled-j unscaled-k val-reg) expected
                                     expected))))
                  (! lri expected (ash (dpb safe target::arrayH.flags-cell-subtag-byte
                                            (ash 1 $arh_simple_bit))
                                       arm::fixnumshift))
                  (! trap-unless-simple-array-3
                     src
                     expected))
                (! trap-unless-typed-array-3 src safe)))
            (unless i-known-fixnum
              (! trap-unless-fixnum unscaled-i))
            (unless j-known-fixnum
              (! trap-unless-fixnum unscaled-j))
            (unless k-known-fixnum
              (! trap-unless-fixnum unscaled-k)))
          (with-imm-target () dim1
            (with-imm-target (dim1) dim2
              (let* ((idx-reg ($ arm::arg_y)))
                (unless constidx
                  (if safe                  
                    (! check-3d-bound dim1 dim2 unscaled-i unscaled-j unscaled-k src)
                    (! 3d-dims dim1 dim2 src))
                  (! 3d-unscaled-index idx-reg dim1 dim2 unscaled-i unscaled-j unscaled-k))
                (let* ((v ($ arm::arg_x)))
                  (if simple
                    (! array-data-vector-ref v src)
                    (progn
                      (arm2-copy-register seg v src)
                      (! deref-vector-header v idx-reg v idx-reg)))
                  (arm2-vset1 seg vreg xfer type-keyword v idx-reg constidx val-reg (arm2-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization))))))))))

(defun arm2-aref2 (seg vreg xfer array i j safe typekeyword &optional dim0 dim1(simple t))
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (src)
           (unscaled-i)
           (unscaled-j)
           (constidx
            (and *arm2-reckless*
                 dim0 dim1 i-known-fixnum j-known-fixnum
                 (>= i-known-fixnum 0)
                 (>= j-known-fixnum 0)
                 (< i-known-fixnum dim0)
                 (< j-known-fixnum dim1)
                 (+ (* i-known-fixnum dim1) j-known-fixnum))))
      (if constidx
        (setq src (arm2-one-targeted-reg-form seg array ($ arm::arg_z)))
        (multiple-value-setq (src unscaled-i unscaled-j)
          (arm2-three-untargeted-reg-forms seg
                                           array arm::arg_x
                                           i arm::arg_y
                                           j arm::arg_z)))
      (when safe        
        (when (typep safe 'fixnum)
          (let* ((*available-backend-node-temps* *available-backend-node-temps*))
            (when unscaled-i
              (setq *available-backend-node-temps* (logandc2 *available-backend-node-temps*
                                                             (ash 1 (hard-regspec-value unscaled-i)))))
            (when unscaled-j
              (setq *available-backend-node-temps* (logandc2 *available-backend-node-temps*
                                                             (ash 1 (hard-regspec-value unscaled-j)))))
            (with-node-target (src) expected
              (if simple
                (progn
                  (! lri expected (ash (dpb safe target::arrayH.flags-cell-subtag-byte
                                            (ash 1 $arh_simple_bit))
                                       arm::fixnumshift))
                  (! trap-unless-simple-array-2 src expected))
                (! trap-unless-typed-array-2 src safe)))))
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
          (with-node-target (idx-reg src) v
            (if simple
              (! array-data-vector-ref v src)
              (progn
                (setq v src)
                (! deref-vector-header src idx-reg)))
            (arm2-vref1 seg vreg xfer typekeyword v idx-reg constidx)))))))



(defun arm2-aref3 (seg vreg xfer array i j k safe typekeyword  dim0 dim1 dim2  &optional (simple t))
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (k-known-fixnum (acode-fixnum-form-p k))
           (src)
           (unscaled-i)
           (unscaled-j)
           (unscaled-k)
           (constidx
            (and *arm2-reckless*
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
        (setq src (arm2-one-targeted-reg-form seg array ($ arm::arg_z)))
        (multiple-value-setq (src unscaled-i unscaled-j unscaled-k)
          (arm2-four-untargeted-reg-forms seg
                                           array arm::temp0
                                           i arm::arg_x
                                           j arm::arg_y
                                           k arm::arg_z)))
      (when safe        
        (when (typep safe 'fixnum)
          (if simple
            (let* ((expected (if constidx
                               (with-node-target (src) expected
                                 expected)
                               (with-node-target (src unscaled-i unscaled-j unscaled-k) expected
                                 expected))))
              (! lri expected (ash (dpb safe target::arrayH.flags-cell-subtag-byte
                                        (ash 1 $arh_simple_bit))
                                   arm::fixnumshift))
              (! trap-unless-simple-array-3 src expected))
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
              (arm2-copy-register seg v src)
              (! deref-vector-header v idx-reg)))
          (arm2-vref1 seg vreg xfer typekeyword v idx-reg constidx))))))


(defun arm2-constant-value-ok-for-type-keyword (type-keyword form)
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

(defun arm2-target-reg-for-aset (vreg type-keyword)
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
         (next-imm-target (available-imm-temp  *available-backend-imm-temps*))
         (acc (make-wired-lreg arm::arg_z)))
    (cond ((or is-node
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
                  (ecase type-keyword
                    (:double-float-vector (available-fp-temp *available-backend-fp-temps* :double-float))
                    (:complex-single-float-vector (available-fp-temp *available-backend-fp-temps* :complex-single-float))))
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
                      ))
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



(defun arm2-unboxed-reg-for-aset (seg type-keyword result-reg safe constval)
  (with-arm-local-vinsn-macros (seg)
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
                   (! trap-unless-typecode= result-reg arm::subtag-complex-double-float))
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
                      (! trap-unless-typecode= result-reg arm::subtag-complex-single-float))
                    (! get-complex-single-float reg result-reg)
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
                            (arm2-lri seg reg (char-code constval))
                            (! unbox-base-char reg result-reg)))
                         ((eq type-keyword :single-float-vector)
                          (if (typep constval 'single-float)
                            (arm2-lri seg reg (single-float-bits constval))
                            (progn
                              (when safe
                                (! trap-unless-single-float result-reg))
                              (! single-float-bits reg result-reg))))
                         (t
                          (if (typep constval '(unsigned-byte 32))
                            (arm2-lri seg reg constval)
			    (! unbox-u32 reg result-reg))))
                   reg)))
              (is-16-bit
               (if is-signed
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s16)))
                   (if (typep constval '(signed-byte 16))
                     (arm2-lri seg reg constval)
                     (! unbox-s16 reg result-reg))
                   reg)
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u16)))
                   (if (typep constval '(unsigned-byte 16))
                     (arm2-lri seg reg constval)
                     (! unbox-u16 reg result-reg))
                   reg)))
              (is-8-bit
               (if is-signed
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s8)))
                   (if (typep constval '(signed-byte 8))
                     (arm2-lri seg reg constval)
                     (! unbox-s8 reg result-reg))
                   reg)
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u8)))
                   (if (typep constval '(unsigned-byte 8))
                     (arm2-lri seg reg constval)
                     (! unbox-u8 reg result-reg))
                   reg)))
              (t
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u8)))
                   (unless (typep constval 'bit)
                     (! unbox-bit reg result-reg))
                   reg)))))))
                   
      
;;; "val-reg" might be boxed, if the vreg requires it to be.
(defun arm2-vset1 (seg vreg xfer type-keyword src  unscaled-idx index-known-fixnum val-reg unboxed-val-reg constval &optional (node-value-needs-memoization t))
  (with-arm-local-vinsn-macros (seg vreg xfer)
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
             (unless (and (eql (hard-regspec-value src) arm::arg_x)
                          (eql (hard-regspec-value unscaled-idx) arm::arg_y)
                          (eql (hard-regspec-value val-reg) arm::arg_z))
               (compiler-bug "Bug: invalid register targeting for gvset: ~s" (list src unscaled-idx val-reg)))
             (! call-subprim-3 val-reg (arm::arm-subprimitive-offset '.SPgvset) src unscaled-idx val-reg))
            (is-node
             (if (and index-known-fixnum (<= index-known-fixnum
                                             (arch::target-max-32-bit-constant-index arch)))
               (! misc-set-c-node val-reg src index-known-fixnum)
               (with-imm-target () scaled-idx

                 (if index-known-fixnum
                   (arm2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum *arm2-target-node-shift*)))
                   (! scale-node-misc-index scaled-idx unscaled-idx))
                 (! misc-set-node val-reg src scaled-idx))))
            (t
             (cond
               (is-128-bit
                (with-imm-target () scaled-idx
                  (if index-known-fixnum
                    (unless unscaled-idx
                      (setq unscaled-idx scaled-idx)                      
                      (arm2-absolute-natural seg unscaled-idx nil (ash index-known-fixnum arm::fixnumshift))))
                  (! misc-set-complex-double-float unboxed-val-reg src unscaled-idx)))
               (is-64-bit
                ;; :double-float-vector, :complex-single-float-vector 
                (with-imm-target (arm::imm0 arm::imm1) scaled-idx
                  (if (and index-known-fixnum
                           (<= index-known-fixnum
                               (arch::target-max-64-bit-constant-index arch)))
                    (! misc-set-c-double-float unboxed-val-reg src index-known-fixnum)
                    (progn
                      (if index-known-fixnum
                        (unless unscaled-idx
                          (setq unscaled-idx scaled-idx)
                          (arm2-absolute-natural seg unscaled-idx nil (ash index-known-fixnum arm::fixnumshift))))
                      (! misc-set-double-float unboxed-val-reg src unscaled-idx)))))
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
                             (arm2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 2)))
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
                             (arm2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 1)))
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
                             (arm2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) index-known-fixnum))
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
                                 (! set-constant-arm-bit-to-0 word word bit-number)
                                 (! set-constant-arm-bit-to-1 word word bit-number))
                               (! set-constant-arm-bit-to-variable-value word word unboxed-val-reg bit-number))
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
                    

(defun arm2-code-coverage-entry (seg note)
  (let* ((afunc *arm2-cur-afunc*))
    (setf (afunc-bits afunc) (%ilogior (afunc-bits afunc) (ash 1 $fbitccoverage)))
    (with-arm-local-vinsn-macros (seg)
      (let* ((ccreg ($ arm::temp0)))
        (arm2-store-immediate seg note ccreg)
        (with-node-temps (ccreg) (zero)
          (! lri zero 0)
          (! misc-set-c-node zero ccreg 1))))))

(defun arm2-vset (seg vreg xfer type-keyword vector index value safe)
  (with-arm-local-vinsn-macros (seg)
    (let* ((arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (arm2-constant-value-ok-for-type-keyword type-keyword value))
           (needs-memoization (and is-node (arm2-acode-needs-memoization value)))
           (index-known-fixnum (acode-fixnum-form-p index)))
      (let* ((src ($ arm::arg_x))
             (unscaled-idx ($ arm::arg_y))
             (result-reg ($ arm::arg_z)))
        (cond (needs-memoization
               (arm2-three-targeted-reg-forms seg
                                              vector src
                                              index unscaled-idx
                                              value result-reg))
              (t
               (if (and (not safe) index-known-fixnum)
                 (multiple-value-setq (src result-reg unscaled-idx)
                   (arm2-two-untargeted-reg-forms seg
                                                  vector src
                                                  value (arm2-target-reg-for-aset vreg type-keyword)))
                 (multiple-value-setq (src unscaled-idx result-reg)
                   (arm2-three-untargeted-reg-forms seg
                                                    vector src
                                                    index unscaled-idx
                                                    value (arm2-target-reg-for-aset vreg type-keyword))))))
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
        (arm2-vset1 seg vreg xfer type-keyword src unscaled-idx index-known-fixnum result-reg (arm2-unboxed-reg-for-aset seg type-keyword result-reg safe constval) constval needs-memoization)))))

(defun arm2-1d-vset (seg vreg xfer type-keyword vector index value safe)
  (with-arm-local-vinsn-macros (seg)
    (let* ((arch (backend-target-arch *target-backend*))
           (simple-case (backend-get-next-label))
           (common-case (backend-get-next-label))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (arm2-constant-value-ok-for-type-keyword type-keyword value))
           (needs-memoization (and is-node (arm2-acode-needs-memoization value)))
           (index-known-fixnum (acode-fixnum-form-p index)))
      (let* ((src ($ arm::arg_x))
             (unscaled-idx ($ arm::arg_y))
             (result-reg ($ arm::arg_z)))
        (cond (needs-memoization
               (arm2-three-targeted-reg-forms seg
                                              vector src
                                              index unscaled-idx
                                              value result-reg))
              (t
               (multiple-value-setq (src unscaled-idx result-reg)
                   (arm2-three-untargeted-reg-forms seg
                                                    vector src
                                                    index unscaled-idx
                                                    value (arm2-target-reg-for-aset vreg type-keyword)))))
          (let* ((*available-backend-imm-temps* *available-backend-imm-temps*)
                 (value (if (eql (hard-regspec-class result-reg)
                                 hard-reg-class-gpr)
                          (hard-regspec-value result-reg))))
            (when (and value (logbitp value *available-backend-imm-temps*))
              (setq *available-backend-imm-temps* (bitclr value *available-backend-imm-temps*)))
        (with-crf-target () crf
          (! set-z-if-vector-header crf src)
          (arm2-branch seg (arm2-make-compound-cd simple-case 0) crf arm::arm-cond-eq nil))
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


        (arm2-vset1 seg vreg xfer type-keyword src unscaled-idx index-known-fixnum result-reg (arm2-unboxed-reg-for-aset seg type-keyword result-reg safe constval) constval needs-memoization))))))


(defun arm2-tail-call-alias (immref sym &optional arglist)
  (let ((alias (cdr (assq sym *arm2-tail-call-aliases*))))
    (if (and alias (or (null arglist) (eq (+ (length (car arglist)) (length (cadr arglist))) (cdr alias))))
      (make-acode (%nx1-operator immediate) (car alias))
      immref)))

;;; If BODY is essentially an APPLY involving an &rest arg, try to avoid
;;; consing it.
(defun arm2-eliminate-&rest (body rest key-p auxen rest-values)
  (when (and rest (not key-p) (not (cadr auxen)) rest-values)
    (when (eq (logand (the fixnum (nx-var-bits rest))
                      (logior (ash -1 $vbitspecial)
                              (ash 1 $vbitclosed) (ash 1 $vbitsetq) (ash 1 $vbitcloseddownward)))
              0)               ; Nothing but simple references
      (do* ()
           ((not (acode-p body)))
        (let* ((op (acode-operator body)))
          (if (or (eq op (%nx1-operator lexical-function-call))
                  (eq op (%nx1-operator call)))
            (destructuring-bind (fn-form (stack-args reg-args) &optional spread-p) (acode-operands body)
               (unless (and (eq spread-p t)
                           (eq (arm2-lexical-reference-p (%car reg-args)) rest))
                (return nil))
              (flet ((independent-of-all-values (form)        
                       (setq form (acode-unwrapped-form-value form))
                       (or (arm-constant-form-p form)
                           (let* ((lexref (arm2-lexical-reference-p form)))
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
                                 (if (<= (length arglist) $numarmargregs)
                                   (list nil (reverse arglist))
                                   (list (butlast arglist $numarmargregs)
                                         (reverse (last arglist $numarmargregs))))
                                 nil)))
                  (return nil))))
            (if (eq op (%nx1-operator local-block))
              (setq body (cadr (acode-operands body)))
              (if (and (eq op (%nx1-operator if))
                       (eq (arm2-lexical-reference-p (car (acode-operands body))) rest))
                (setq body (car (cdr (acode-operands body))))
                (return nil)))))))))

(defun arm2-call-fn (seg vreg xfer fn arglist spread-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (when spread-p
      (destructuring-bind (stack-args reg-args) arglist
        (when (and (null (cdr reg-args))
                   (nx-null (acode-unwrapped-form-value (car reg-args))))
          (setq spread-p nil)
          (let* ((nargs (length stack-args)))
            (declare (fixnum nargs))
            (if (<= nargs $numarmargregs)
              (setq arglist (list nil (reverse stack-args)))
              (setq arglist (list (butlast stack-args $numarmargregs) (reverse (last stack-args $numarmargregs)))))))))
    (let* ((lexref (arm2-lexical-reference-p fn))
           (simple-case (or (fixnump fn)
                            (typep fn 'lreg)
                            (arm2-immediate-function-p fn)
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
           (cstack *arm2-cstack*)
           (vstack *arm2-vstack*))
      (setq xfer (or xfer 0))
      (when (and (eq xfer $backend-return)
                 (eq 0 *arm2-undo-count*)
                 (acode-p fn)
                 (eq (acode-operator fn) (%nx1-operator immediate))
                 (symbolp (car (acode-operands fn))))
        (setq fn (arm2-tail-call-alias fn (car (acode-operands fn)) arglist)))
      
      (if (and (eq xfer $backend-return) (not (arm2-tailcallok xfer)))
        (progn
          (arm2-call-fn seg vreg $backend-mvpass fn arglist spread-p)
          (arm2-set-vstack (%i+ (if simple-case 0 *arm2-target-node-size*) vstack))
          (setq  *arm2-cstack* cstack)
          (let ((*arm2-returning-values* t)) (arm2-do-return seg)))
        (let* ((mv-p (arm2-mv-p xfer)))
          (unless simple-case
            (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg fn arm::arg_z))
            (setq fn (arm2-vloc-ea vstack)))
          (arm2-invoke-fn seg fn (arm2-arglist seg arglist) spread-p xfer)
          (if (and (logbitp $backend-mvpass-bit xfer)
                   (not simple-case))
            (progn
              (! save-values)
              (! vstack-discard 1)
              (arm2-set-nargs seg 0)
              (! recover-values))
            (unless (or mv-p simple-case)
              (! vstack-discard 1)))
          (arm2-set-vstack vstack)
          (setq *arm2-cstack* cstack)
          (when (or (logbitp $backend-mvpass-bit xfer) (not mv-p))
            (<- arm::arg_z)
            (arm2-branch seg (logand (lognot $backend-mvpass-mask) xfer) vreg))))
      nil)))

(defun arm2-restore-full-lisp-context (seg)
  (with-arm-local-vinsn-macros (seg)
    (! restore-full-lisp-context)))

(defun arm2-call-symbol (seg jump-p)
  ; fname contains a symbol; we can either call it via
  ; a call to .SPjmpsym or expand the instructions inline.
  ; Since the branches are unconditional, the call doesn't
  ; cost much, but doing the instructions inline would give
  ; an instruction scheduler some opportunities to improve
  ; performance, so this isn't a strict time/speed tradeoff.
  ; This should probably dispatch on something other than
  ; *arm2-open-code-inline*, since that does imply a time/speed
  ; tradeoff.
  (with-arm-local-vinsn-macros (seg)
    (if *arm2-optimize-for-space*
      (if jump-p
        (! jump-known-symbol-ool)
        (! call-known-symbol-ool))
      (if jump-p
        (! jump-known-symbol)
        (! call-known-symbol arm::arg_z)))))

;;; Nargs = nil -> multiple-value case.
(defun arm2-invoke-fn (seg fn nargs spread-p xfer)
  (with-arm-local-vinsn-macros (seg)
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
           (destreg (if symp ($ arm::fname) (if lfunp ($ arm::nfn) (unless label-p ($ arm::nfn)))))
           (known-fixed-nargs nil)
           (label (when label-p
                    (if (and *arm2-fixed-args-label*
                             (eql nargs *arm2-fixed-nargs*)
                             (not spread-p)
                             (not (arm2-mvpass-p xfer)))
                      (progn
                        (setq known-fixed-nargs t)
                        (if tail-p
                          *arm2-fixed-args-tail-label*
                          *arm2-fixed-args-label*))
                      1))))
      (when expression-p
        ;;Have to do this before spread args, since might be vsp-relative.
        (if nargs
          (arm2-do-lexical-reference seg destreg fn)
          (arm2-copy-register seg destreg fn)))
      (if (or symp lfunp)
        (setq func (if symp (arm2-symbol-entry-locative func)
                     (arm2-afunc-lfun-ref func))
              a-reg (arm2-register-constant-p func)))
      (when tail-p
        #-no-compiler-bugs
        (unless (or immp symp lfunp (typep fn 'lreg) (fixnump fn)) (compiler-bug "Well, well, well.  How could this have happened ?"))
        (when a-reg
          (arm2-copy-register seg destreg a-reg))
        (unless spread-p
          (arm2-restore-nvrs seg (null nargs))
          (arm2-restore-non-volatile-fprs seg)
          (! restore-nfp)))
      (if spread-p
        (progn
          (arm2-set-nargs seg (%i- nargs 1))
          (if (eq spread-p 0)
            (! spread-lexpr)
            (! spread-list))
          (arm2-restore-nvrs seg nil)
          (arm2-restore-non-volatile-fprs seg)
          (! restore-nfp))
        (if nargs
          (unless known-fixed-nargs (arm2-set-nargs seg nargs))
          (! pop-argument-registers)))
      (if callable
        (if (not tail-p)
          (if (arm2-mvpass-p xfer)
            (let* ((call-reg (if symp ($ arm::fname) ($ arm::nfn))))
              (if label-p
                (arm2-copy-register seg call-reg ($ arm::fn))
                (if a-reg
                  (arm2-copy-register seg call-reg  a-reg)
                  (arm2-store-immediate seg func call-reg)))
              (if symp
                (! pass-multiple-values-symbol)
                (! pass-multiple-values)))
            (progn 
              (if label-p
                (progn
                  (arm2-copy-register seg ($ arm::nfn) ($  arm::fn))
                  (! call-label (aref *backend-labels* label)))
                (progn
                  (if a-reg
                    (arm2-copy-register seg destreg a-reg)
                    (arm2-store-immediate seg func destreg))
                  (if symp
                    (arm2-call-symbol seg nil)
                    (! call-known-function))))))
          (progn
            (arm2-unwind-stack seg xfer 0 0 #x7fffff)
            (if (and (not spread-p) nargs (%i<= nargs $numarmargregs))
              (progn
                (if label-p
                  (unless known-fixed-nargs
                    (arm2-copy-register seg arm::nfn arm::fn)))
                (unless (or label-p a-reg) (arm2-store-immediate seg func destreg))
                (unless known-fixed-nargs
                  (arm2-restore-full-lisp-context seg))
                (if label-p
                  (! jump (aref *backend-labels* label))
                  (progn
                    (if symp
                      (arm2-call-symbol seg t)
                      (! jump-known-function)))))
              (progn
                (if label-p
                  (arm2-copy-register seg arm::nfn arm::fn)
                  (unless a-reg (arm2-store-immediate seg func destreg)))
                (cond ((or spread-p (null nargs))
                       (if symp
                         (! tail-call-sym-gen)
                         (! tail-call-fn-gen)))
                      ((%i> nargs $numarmargregs)
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
            (arm2-one-targeted-reg-form seg fn destreg))
          (if (not tail-p)
            (if (arm2-mvpass-p xfer)
              (! pass-multiple-values)
              (! funcall))                  
            (cond ((or (null nargs) spread-p)
                   (! tail-funcall-gen))
                  ((%i> nargs $numarmargregs)
                   (! tail-funcall-slide))
                  (t
                   (! tail-funcall-vsp)))))))
    nil))

(defun arm2-seq-fbind (seg vreg xfer vars afuncs body p2decls)
  (let* ((old-stack (arm2-encode-stack))
         (copy afuncs)
         (func nil))
    (with-arm-p2-declarations p2decls 
      (dolist (var vars) 
        (when (neq 0 (afunc-fn-refcount (setq func (pop afuncs))))
          (arm2-seq-bind-var seg var (nx1-afunc-ref func))))
      (arm2-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (when (neq 0 (afunc-fn-refcount (setq func (pop copy))))
          (arm2-close-var seg var))))))

(defun arm2-make-closure (seg afunc downward-p)
  (with-arm-local-vinsn-macros (seg)
    (flet ((var-to-reg (var target)
             (let* ((ea (var-ea (var-bits var))))
               (if ea
                 (arm2-addrspec-to-reg seg (arm2-ea-open ea) target)
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
             (dest ($ arm::arg_z))
             (vsize (+ (length inherited-vars) 
                       3                ; entrypoint,%closure-code%, afunc
                       2)))             ; name, lfun-bits
        (declare (list inherited-vars))
        (if downward-p
          (progn
            (let* ((*arm2-vstack* *arm2-vstack*))
              (arm2-lri seg arm::arg_x (ash (nx-lookup-target-uvector-subtag :function) *arm2-target-fixnum-shift*))
              (arm2-lri seg arm::temp0 0)
              (! %closure-code% arm::arg_y)
              (arm2-store-immediate seg (arm2-afunc-lfun-ref afunc) arm::arg_z)
              (arm2-vpush-register-arg seg arm::arg_x)
              (arm2-vpush-register-arg seg arm::temp0)
              (arm2-vpush-register-arg seg arm::arg_y)
              (arm2-vpush-register-arg seg arm::arg_z)
              ;; Could be smarter about memory traffic here.
              (dolist (v inherited-vars)
                (arm2-vpush-register-arg seg (var-to-reg v arm::arg_z)))
              (! load-nil arm::arg_z)
              (arm2-vpush-register-arg seg arm::arg_z)
              (arm2-lri seg arm::arg_z (ash (ash 1 $lfbits-trampoline-bit) *arm2-target-fixnum-shift*))
              (arm2-vpush-register-arg seg arm::arg_z)
              (arm2-set-nargs seg (1+ vsize)) ; account for subtag
              (! make-stack-closure))
            (arm2-open-undo $undostkblk))
          (let* ((cell 1))
            (declare (fixnum cell))
            (progn
              (arm2-lri seg
                        arm::imm0
                        (arch::make-vheader vsize (nx-lookup-target-uvector-subtag :function)))
              (! %alloc-misc-fixed dest arm::imm0 (ash vsize (arch::target-word-shift arch)))
              )
            (! %closure-code% arm::arg_x)
            (! %codevector-entry arm::lr arm::arg_x)
            (! misc-set-c-node arm::lr dest 0)
            (arm2-store-immediate seg (arm2-afunc-lfun-ref afunc) arm::arg_y)
            (with-node-temps (arm::arg_z) (t0 t1 t2 t3)
              (do* ((ccode arm::arg_x nil)
                    (func arm::arg_y nil))
                   ((null inherited-vars))
                (let* ((t0r (or ccode (if inherited-vars (var-to-reg (pop inherited-vars) t0))))
                       (t1r (or func (if inherited-vars (var-to-reg (pop inherited-vars) t1))))
                       (t2r (if inherited-vars (var-to-reg (pop inherited-vars) t2)))
                       (t3r (if inherited-vars (var-to-reg (pop inherited-vars) t3))))
                  (setq cell (set-some-cells dest cell t0r t1r t2r t3r)))))
            (arm2-lri seg arm::arg_y (ash (ash 1 $lfbits-trampoline-bit) *arm2-target-fixnum-shift*))
            (! load-nil arm::arg_x)
            (! misc-set-c-node arm::arg_x dest cell)
            (! misc-set-c-node arm::arg_y dest (1+ cell))))
        dest))))
        
(defun arm2-symbol-entry-locative (sym)
  (setq sym (require-type sym 'symbol))
  (when (eq sym '%call-next-method-with-args)
    (setf (afunc-bits *arm2-cur-afunc*)
          (%ilogior (%ilsl $fbitnextmethargsp 1) (afunc-bits *arm2-cur-afunc*))))
  (or (assq sym *arm2-fcells*)
      (let ((new (list sym)))
        (push new *arm2-fcells*)
        new)))

(defun arm2-symbol-value-cell (sym)
  (setq sym (require-type sym 'symbol))
  (or (assq sym *arm2-vcells*)
      (let ((new (list sym)))
        (push new *arm2-vcells*)
        (ensure-binding-index sym)
        new)))


(defun arm2-symbol-locative-p (imm)
  (and (consp imm)
       (or (memq imm *arm2-vcells*)
           (memq imm *arm2-fcells*))))




(defun arm2-immediate-function-p (f)
  (setq f (acode-unwrapped-form-value f))
  (and (acode-p f)
       (or (eq (acode-operator f) (%nx1-operator immediate))
           (eq (acode-operator f) (%nx1-operator simple-function)))))

(defun arm-constant-form-p (form)
  (setq form (nx-untyped-form form))
  (if form
    (or (nx-null form)
        (nx-t form)
        (and (acode-p form)
             (or (eq (acode-operator form) (%nx1-operator immediate))
                 (eq (acode-operator form) (%nx1-operator fixnum))
                 (eq (acode-operator form) (%nx1-operator simple-function)))))))


  
(defun arm2-integer-constant-p (form mode)
  (let* ((val 
         (or (acode-fixnum-form-p (setq form (acode-unwrapped-form form)))
             (and (acode-p form)
                  (eq (acode-operator form) (%nx1-operator immediate))
                  (setq form (car (acode-operands form)))
                  (if (typep form 'integer)
                    form)))))
    (and val (%typep val (mode-specifier-type mode)) val)))


(defun arm-side-effect-free-form-p (form)
  (when (acode-p (setq form (acode-unwrapped-form-value form)))
    (unless (arm2-nfp-ref-p form)
      (or (arm-constant-form-p form)
                                        ;(eq (acode-operator form) (%nx1-operator bound-special-ref))
          (if (eq (acode-operator form) (%nx1-operator lexical-reference))
            (not (%ilogbitp $vbitsetq (nx-var-bits (car (acode-operands form))))))))))

(defun arm2-formlist (seg stkargs &optional revregargs)
  (with-arm-local-vinsn-macros (seg)  
    (let* ((nregs (length revregargs))
           (n nregs))
      (declare (fixnum n))
      (dolist (arg stkargs)
        (let* ((reg (arm2-one-untargeted-reg-form seg arg arm::arg_z)))
          (arm2-vpush-register-arg seg reg)
          (incf n)))
      (when revregargs
        (let* ((zform (%car revregargs))
               (yform (%cadr revregargs))
               (xform (%caddr revregargs)))
          (if (eq 3 nregs)
            (arm2-three-targeted-reg-forms seg xform ($ arm::arg_x) yform ($ arm::arg_y) zform ($ arm::arg_z))
            (if (eq 2 nregs)
              (arm2-two-targeted-reg-forms seg yform ($ arm::arg_y) zform ($ arm::arg_z))
              (arm2-one-targeted-reg-form seg zform ($ arm::arg_z))))))
      n)))

(defun arm2-arglist (seg args)
  (arm2-formlist seg (car args) (cadr args)))





(defun arm2-unboxed-integer-arg-to-reg (seg form immreg &optional ffi-arg-type)
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
    (with-arm-local-vinsn-macros (seg)
      (let* ((value (arm2-integer-constant-p form mode)))
        (if value
            (progn
              (unless (typep immreg 'lreg)
                (setq immreg (make-unwired-lreg immreg :mode modeval)))
              (arm2-lri seg immreg value)
              immreg)
          (progn 
            (arm2-one-targeted-reg-form seg form (make-wired-lreg arm::imm0 :mode modeval))))))))


(defun arm2-macptr-arg-to-reg (seg form address-reg)  
  (arm2-one-targeted-reg-form seg
                              form 
                              address-reg))

(defun arm2-push-reg-for-form (seg form suggested &optional targeted)
  (let* ((reg (if (and (node-reg-p suggested)
                         (nx2-acode-call-p form))     ;probably ...
                (arm2-one-targeted-reg-form seg form  arm::arg_z)
                (if targeted
                  (arm2-one-targeted-reg-form seg form suggested)
                  (arm2-one-untargeted-reg-form seg form suggested)))))
    (arm2-push-register seg reg)))

(defun arm2-one-lreg-form (seg form lreg)
  (let ((is-float (= (hard-regspec-class lreg) hard-reg-class-fpr)))
    (if is-float
      (arm2-form-float seg lreg nil form)
      (arm2-form seg lreg nil form))
    lreg))

(defun arm2-one-targeted-reg-form (seg form reg)
  (arm2-one-lreg-form seg form reg))

(defun arm2-one-untargeted-lreg-form (seg form reg)
  (arm2-one-lreg-form seg form (if (typep reg 'lreg) reg (make-unwired-lreg reg))))

(defun same-arm-reg-p (x y)
  (and (eq (hard-regspec-value x) (hard-regspec-value y))
       (let* ((class (hard-regspec-class x)))
         (and (eq class (hard-regspec-class y))
              (or (not (eql class hard-reg-class-fpr))
                  (eq (%get-regspec-mode x)
                      (%get-regspec-mode y)))))))

;;; If REG is a node reg, add it to the bitmask.
(defun arm2-restrict-node-target (reg mask)
  (if (node-reg-p reg)
    (logior mask (ash 1 (hard-regspec-value reg)))
    mask))

;;; If suggested reg is a node reg that contains a stack location,
;;; try to use some other node temp.
(defun arm2-try-non-conflicting-reg (suggested reserved)
  (let* ((mask *arm2-gpr-locations-valid-mask*))
    (or (when (and (node-reg-p suggested)
                   (logbitp (hard-regspec-value suggested) mask))
          (setq mask (logior mask reserved))
          (%available-node-temp (logand *available-backend-node-temps*
                                        (lognot mask))))
        suggested)))

(defun arm2-push-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (fpr-mode-name (if a-float (fpr-mode-value-name (get-regspec-mode areg))))
         (a-node (unless a-float (= (get-regspec-mode areg) hard-reg-class-gpr-mode-node)))
         (nested (> *arm2-undo-count* 0))
         vinsn)
    (with-arm-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (arm2-vpush-register seg areg))
        (let* ((offset *arm2-nfp-depth*))
          (setq vinsn
                (if a-float
                  (case fpr-mode-name
                    ((:double-float :omplex-single-float)
                     (if nested
                       (! nfp-store-double-float-nested areg offset)
                       (! nfp-store-double-float areg  offset)))
                    (:complex-double-float
                     (incf offset 8)
                     (if nested
                       (! nfp-store-complex-double-float-nested areg offset)
                       (! nfp-store-complex-double-float areg  offset)))
                    (:single-float
                     (if nested
                       (! nfp-store-single-float-nested areg offset)
                       (! nfp-store-single-float areg offset))))
                  (if nested
                    (! nfp-store-unboxed-word-nested areg offset)
                    (! nfp-store-unboxed-word areg offset))))
          (push vinsn *arm2-all-nfp-pushes*)
          (incf offset 8)
          (setq *arm2-nfp-depth* offset)))
      vinsn)))
             

(defun arm2-one-untargeted-reg-form (seg form suggested &optional (reserved 0))
  (or (arm2-reg-for-form form suggested)
      (if (and (acode-p form)
               (eq (acode-operator form) (%nx1-operator %current-tcr)))
        arm::rcontext
        (if (node-reg-p suggested)
          (arm2-one-untargeted-lreg-form seg form (arm2-try-non-conflicting-reg suggested reserved))
          (arm2-one-untargeted-lreg-form seg form suggested)))))

(defun arm2-pop-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (fpr-mode-name (if a-float (fpr-mode-value-name (get-regspec-mode areg))))
         (a-node (unless a-float (= (get-regspec-mode areg) hard-reg-class-gpr-mode-node)))
         (nested (> *arm2-undo-count* 0))
         vinsn)
    (with-arm-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (arm2-vpop-register seg areg))
        (let* ((offset (- *arm2-nfp-depth* 8)))
          (setq vinsn
                (if a-float
                  (case fpr-mode-name
                    ((:double-float :complex-single-float)
                     (if nested
                       (! nfp-load-double-float-nested areg offset)
                       (! nfp-load-double-float areg  offset)))
                    (:complex-double-float
                     (decf offset 8)
                     (if nested
                       (! nfp-load-complex-double-float-nested areg offset)
                       (! nfp-load-complex-double-float areg  offset)))
                    (:single-float
                     (if nested
                       (! nfp-load-single-float-nested areg offset)
                       (! nfp-load-single-float areg  offset))))
                  (if nested
                    (! nfp-load-unboxed-word-nested areg offset)
                    (! nfp-load-unboxed-word areg offset))))
          (setq *arm2-nfp-depth* offset)))
      vinsn)))

(defun arm2-acc-reg-for (reg)
  (with-arm-local-vinsn-macros (seg)
    (if (and (eql (hard-regspec-class reg) hard-reg-class-gpr)
             (eql (get-regspec-mode reg) hard-reg-class-gpr-mode-node))
      ($ arm::arg_z)
      reg)))

(defun arm2-copy-fpr (seg dest src)
  ;; src and dest are distinct FPRs with the same mode.
  (with-arm-local-vinsn-macros (seg)
    (case (fpr-mode-value-name (get-regspec-mode src))
      (:single-float (! single-to-single dest src))
      (:double-float (! double-to-double dest src))
      (:complex-single-float (! complex-single-float-to-complex-single-float
                                dest src))
      (:complex-double-float (! complex-double-float-to-complex-dooble-float
                                dest src)))))

;;; The compiler often generates superfluous pushes & pops.  Try to
;;; eliminate them.
;;; It's easier to elide pushes and pops to the SP.
(defun arm2-elide-pushes (seg push-vinsn pop-vinsn)
  (with-arm-local-vinsn-macros (seg)
    (let* ((operands (vinsn-variable-parts push-vinsn))
           (pushed-reg (svref operands  0))
           (popped-reg (svref (vinsn-variable-parts pop-vinsn) 0))
           (same-reg (eq (hard-regspec-value pushed-reg)
                         (hard-regspec-value popped-reg)))
           (nfp-p (vinsn-attribute-p push-vinsn :nfp)))
      (when nfp-p               ; vsp case is harder.
        (let* ((pushed-reg-is-set (vinsn-sequence-sets-reg-p
                                   push-vinsn pop-vinsn pushed-reg))
               (popped-reg-is-set (if same-reg
                                    pushed-reg-is-set
                                    (vinsn-sequence-sets-reg-p
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
            ((not (and pushed-reg-is-set popped-reg-is-set))
             (unless same-reg
               (let* ((copy (if (eq (hard-regspec-class pushed-reg)
                                    hard-reg-class-fpr)
                              (arm2-copy-fpr seg popped-reg pushed-reg)
                              (! copy-gpr popped-reg pushed-reg))))
                 (remove-dll-node copy)
                 (if pushed-reg-is-set
                   (insert-dll-node-after copy push-vinsn)
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
                 ;; We have 14 volatile single-floats or 7
                 ;; volatile double-floats
                 (let* ((nfprs (case mode-name
                                 ((:double-float :complex-single-float) 7)
                                 (:complex-double-float 3)
                                 (:single-float 14)))
                        (free-fpr
                         (dotimes (r nfprs nil)
                           (unless (logtest (target-fpr-mask
                                             r mode)
                                            used-fprs)
                             (return r)))))
                   (when free-fpr
                     (let* ((reg (make-wired-lreg free-fpr :class hard-reg-class-fpr :mode mode))
                            (save (arm2-copy-fpr seg reg pushed-reg))
                                  (restore (arm2-copy-fpr seg popped-reg reg)))
                       (remove-dll-node save)
                       (insert-dll-node-after save push-vinsn)
                       (remove-dll-node restore)
                       (insert-dll-node-before restore pop-vinsn)
                       (setq win t))))))))
          (when win
            (setq *arm2-all-nfp-pushes*
                  (delete push-vinsn *arm2-all-nfp-pushes*))
            (let* ((pair (assq push-vinsn *arm2-nfp-vars*)))
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
                  (let* ((var (cdr (assq inner *arm2-nfp-vars*))))
                    (when var (setf (var-ea var)
                                    (- (var-ea var) size)))))))
            (elide-vinsn push-vinsn)
            (elide-vinsn pop-vinsn)
            t) ))
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
                                                 push-vinsn pop-vinsn popped-reg))))
                   (cond ((and (not (and pushed-reg-is-set popped-reg-is-set))
                               (or (null popped-reg-is-reffed)
                                   (null pushed-reg-is-set)
                                   ;; If the popped register is
                                   ;; referenced and the pushed
                                   ;; register is set, we want to be
                                   ;; sure that the last reference
                                   ;; happens before the first
                                   ;; assignent.  We can't be sure
                                   ;; that either of these things
                                   ;; actually happened or happen
                                   ;; unconditionally, and can't
                                   ;; be sure of the order in which
                                   ;; they might happen if the sequence
                                   ;; contains jumps or branches.
                                   (vinsn-in-sequence-p pushed-reg-is-set popped-reg-is-reffed pop-vinsn)
                                   (not (vinsn-sequence-has-some-attribute-p push-vinsn pop-vinsn :branch :jump))))
                          ;; We don't try this if anything's pushed on
                          ;; or popped from the vstack in the
                          ;; sequence, but there can be references to
                          ;; other things that were pushed earlier.
                          ;; Those references use the vstack depth at
                          ;; the time of the reference and the
                          ;; canonical frame offset to address
                          ;; relative to the vsp.  If we elide the
                          ;; push, the vstack depth will be 4 bytes
                          ;; less than when the reference was
                          ;; generated.  Fix that up ...  There was
                          ;; (once) a notion of modeling the vstack as
                          ;; a list of "lcells"; lcells had a width
                          ;; attribute that was usually the native
                          ;; word size.  Eliding a push involved
                          ;; setting the width of the lcell
                          ;; representing the pushed word to 0.
                          ;; That whole idea was never fully implemented,
                          ;; though we generally try to maintain the model.
                          ;; If it ever is implemented, we need to dtrt
                          ;; here.
                          (do* ((element (dll-node-succ push-vinsn) (dll-node-succ element)))
                               ((eq element pop-vinsn))
                            (when (typep element 'vinsn)
                              (let* ((template (vinsn-template element))
                                     (opidx (case (vinsn-template-name template)
                                              (vframe-store 2)
                                              (vframe-load 2))))
                                (when opidx
                                  (let* ((operands (vinsn-variable-parts element)))
                                    (declare (simple-vector operands))
                                    (setf (svref operands opidx)
                                          (the fixnum
                                            (- (the fixnum (svref operands opidx))
                                               arm::node-size))))))))
                          (unless same-reg
                            (let* ((copy (! copy-gpr popped-reg pushed-reg)))
                              (remove-dll-node copy)
                              (if pushed-reg-is-set
                                  (insert-dll-node-after copy push-vinsn)
                                  (insert-dll-node-before copy pop-vinsn))))
                          (elide-vinsn push-vinsn)
                          (elide-vinsn pop-vinsn)
                          t)
                   (t                   ; maybe allocate a node temp
                    nil)))))))))
                
        
;;; we never leave the first form pushed (the 68K compiler had some subprims that
;;; would vpop the first argument out of line.)
(defun arm2-two-targeted-reg-forms (seg aform areg bform breg)
  (let* ((*arm2-nfp-depth* *arm2-nfp-depth*)
         (avar (arm2-lexical-reference-p aform))
         (atriv (and (arm2-trivial-p bform) (nx2-node-gpr-p breg)))
         (aconst (and (not atriv) (or (arm-side-effect-free-form-p aform)
                                      (if avar (nx2-var-not-set-by-form-p avar bform)))))
         (apushed))
    (progn
      (unless aconst
        (if atriv
          (arm2-one-targeted-reg-form seg aform areg)
          (setq apushed (arm2-push-reg-for-form seg aform areg t))))
      (arm2-one-targeted-reg-form seg bform breg)
      (if aconst
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*)
               (*available-backend-node-temps* *available-backend-node-temps*)
               (*available-backend-fp-temps* *available-backend-fp-temps*)
               (bclass (hard-regspec-class breg))
               (bregval (hard-regspec-value breg)))
          (if (eq bclass hard-reg-class-fpr)
            (use-fp-reg breg)
            (if (eq bclass hard-reg-class-gpr)
              (if (eq (get-regspec-mode breg) hard-reg-class-gpr-mode-node)
                (use-node-temp bregval)
                (use-imm-temp bregval))))
          (arm2-one-targeted-reg-form seg aform areg))
        (if apushed
          (arm2-elide-pushes seg apushed (arm2-pop-register seg areg)))))
    (values areg breg)))


(defun arm2-two-untargeted-reg-forms (seg aform areg bform breg)
  (let* ((*arm2-nfp-depth* *arm2-nfp-depth*)
         (aalready (arm2-reg-for-form aform areg))
         (balready (arm2-reg-for-form bform breg)))
    (if (and aalready balready)
      (values aalready balready)
      (with-arm-local-vinsn-macros (seg)
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*)
               (avar (arm2-lexical-reference-p aform))
               (adest nil)
               (bdest nil)
               (atriv (and (arm2-trivial-p bform) (nx2-node-gpr-p breg)))
               (aconst (and (not atriv) (or (arm-side-effect-free-form-p aform)
                                            (if avar (nx2-var-not-set-by-form-p avar bform)))))
               (apushed nil)
               (restricted 0))
          (progn
            (unless aconst
              (if atriv
                (progn
                  (setq adest (arm2-one-untargeted-reg-form seg aform areg)
                        restricted (arm2-restrict-node-target adest 0))
                  (when (imm-reg-p adest)
                    (use-imm-temp (%hard-regspec-value adest)))
                  (when (same-arm-reg-p adest breg)
                    (setq breg areg)))
                (setq apushed (arm2-push-reg-for-form seg aform areg))))
            (setq bdest (arm2-one-untargeted-reg-form seg bform breg restricted)
                  restricted (arm2-restrict-node-target bdest restricted))
            (unless adest
              (if (same-arm-reg-p areg bdest)
                (setq areg breg)))
            (if aconst
              (progn
                (if (imm-reg-p bdest)
                  (use-imm-temp (%hard-regspec-value bdest)))
                (setq adest (arm2-one-untargeted-reg-form seg aform areg restricted)))
              (if apushed
                (arm2-elide-pushes seg apushed (arm2-pop-register seg (setq adest areg))))))
          (values adest bdest))))))


(defun arm2-four-targeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (let* ((*arm2-nfp-depth* *arm2-nfp-depth*)
         (bnode (nx2-node-gpr-p breg))
         (cnode (nx2-node-gpr-p creg))
         (dnode (nx2-node-gpr-p dreg))
         (atriv (or (null aform) 
                    (and (arm2-trivial-p bform)
                         (arm2-trivial-p cform)
                         (arm2-trivial-p dform)
                         bnode
                         cnode
                         dnode)))
         (btriv (or (null bform)
                    (and (arm2-trivial-p cform)
                         (arm2-trivial-p dform)
                         cnode
                         dnode)))
         (ctriv (or (null cform)
                    (and (arm2-trivial-p dform) dnode)))
          
         (aconst (and (not atriv) 
                      (or (arm-side-effect-free-form-p aform)
                          (let ((avar (arm2-lexical-reference-p aform)))
                            (and avar 
                                 (nx2-var-not-set-by-form-p avar bform)
                                 (nx2-var-not-set-by-form-p avar cform)
                                 (nx2-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or (arm-side-effect-free-form-p bform)
                          (let ((bvar (arm2-lexical-reference-p bform)))
                            (and bvar
                                 (nx2-var-not-set-by-form-p bvar cform)
                                 (nx2-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or (arm-side-effect-free-form-p cform)
                          (let ((cvar (arm2-lexical-reference-p cform)))
                            (and cvar
                                 (nx2-var-not-set-by-form-p cvar dform))))))
         (apushed nil)
         (bpushed nil)
         (cpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (arm2-one-targeted-reg-form seg aform areg)
        (setq apushed (arm2-push-reg-for-form seg aform areg t))))
    (if (and bform (not bconst))
      (if btriv
        (arm2-one-targeted-reg-form seg bform breg)
        (setq bpushed (arm2-push-reg-for-form seg bform breg t))))
    (if (and cform (not cconst))
      (if ctriv
        (arm2-one-targeted-reg-form seg cform creg)
        (setq cpushed (arm2-push-reg-for-form seg cform creg t))))
    (arm2-one-targeted-reg-form seg dform dreg)
    (unless ctriv
      (if cconst
        (arm2-one-targeted-reg-form seg cform creg)
        (arm2-elide-pushes seg cpushed (arm2-pop-register seg creg))))
    (unless btriv 
      (if bconst
        (arm2-one-targeted-reg-form seg bform breg)
        (arm2-elide-pushes seg bpushed (arm2-pop-register seg breg))))
    (unless atriv
      (if aconst
        (arm2-one-targeted-reg-form seg aform areg)
        (arm2-elide-pushes seg apushed (arm2-pop-register seg areg))))
    (values areg breg creg dreg)))

(defun arm2-three-targeted-reg-forms (seg aform areg bform breg cform creg)
  (let* ((*arm2-nfp-depth* *arm2-nfp-depth*)
         (bnode (nx2-node-gpr-p breg))
         (cnode (nx2-node-gpr-p creg))
         (atriv (or (null aform) 
                    (and (arm2-trivial-p bform)
                         (arm2-trivial-p cform)
                         bnode
                         cnode)))
         (btriv (or (null bform)
                    (and (arm2-trivial-p cform)
                         cnode)))
         (aconst (and (not atriv) 
                      (or (arm-side-effect-free-form-p aform)
                          (let ((avar (arm2-lexical-reference-p aform)))
                            (and avar 
                                 (nx2-var-not-set-by-form-p avar bform)
                                 (nx2-var-not-set-by-form-p avar cform))))))
         (bconst (and (not btriv)
                      (or
                       (arm-side-effect-free-form-p bform)
                       (let ((bvar (arm2-lexical-reference-p bform)))
                         (and bvar (nx2-var-not-set-by-form-p bvar cform))))))
         (apushed nil)
         (bpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (arm2-one-targeted-reg-form seg aform areg)
        (setq apushed (arm2-push-reg-for-form seg aform areg t))))
    (if (and bform (not bconst))
      (if btriv
        (arm2-one-targeted-reg-form seg bform breg)
        (setq bpushed (arm2-push-reg-for-form seg bform breg t))))
    (arm2-one-targeted-reg-form seg cform creg)
    (unless btriv 
      (if bconst
        (arm2-one-targeted-reg-form seg bform breg)
        (arm2-elide-pushes seg bpushed (arm2-pop-register seg breg))))
    (unless atriv
      (if aconst
        (arm2-one-targeted-reg-form seg aform areg)
        (arm2-elide-pushes seg apushed (arm2-pop-register seg areg))))
    (values areg breg creg)))

(defun arm2-three-untargeted-reg-forms (seg aform areg bform breg cform creg)
  (with-arm-local-vinsn-macros (seg)
    (let* ((*arm2-nfp-depth* *arm2-nfp-depth*)
           (bnode (nx2-node-gpr-p breg))
           (cnode (nx2-node-gpr-p creg))
           (atriv (or (null aform) 
                      (and (arm2-trivial-p bform)
                           (arm2-trivial-p cform)
                           bnode
                           cnode)))
           (btriv (or (null bform)
                      (and (arm2-trivial-p cform)
                           cnode)))
           (aconst (and (not atriv) 
                        (or (arm-side-effect-free-form-p aform)
                            (let ((avar (arm2-lexical-reference-p aform)))
                              (and avar 
                                   (nx2-var-not-set-by-form-p avar bform)
                                   (nx2-var-not-set-by-form-p avar cform))))))
           (bconst (and (not btriv)
                        (or
                         (arm-side-effect-free-form-p bform)
                         (let ((bvar (arm2-lexical-reference-p bform)))
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
            (setq adest (arm2-one-untargeted-reg-form seg aform ($ areg))
                  restricted (arm2-restrict-node-target adest 0))
            (when (same-arm-reg-p adest breg)
              (setq breg areg))
            (when (same-arm-reg-p adest creg)
              (setq creg areg)))
          (setq apushed (arm2-push-reg-for-form seg aform areg ))))
      (when (and bform (not bconst))
        (if btriv
          (progn
            (setq bdest (arm2-one-untargeted-reg-form seg bform ($ breg) restricted)
                  restricted (arm2-restrict-node-target bdest restricted))
            (when (same-arm-reg-p bdest creg)
              (setq creg breg))
            (when (same-arm-reg-p bdest areg)
              (setq areg breg)))
          (setq bpushed (arm2-push-reg-for-form seg bform breg))))
      (setq cdest (arm2-one-untargeted-reg-form seg cform creg restricted)
            restricted (arm2-restrict-node-target cdest restricted))
      (when (same-arm-reg-p cdest areg)
        (setq areg creg))
      (when (same-arm-reg-p cdest breg)
        (setq breg creg))
      (unless btriv 
        (if bconst
          (setq bdest (arm2-one-untargeted-reg-form seg bform breg restricted))
          (arm2-elide-pushes seg bpushed (arm2-pop-register seg (setq bdest breg))))
        (setq restricted (arm2-restrict-node-target bdest restricted))
        (when (same-arm-reg-p bdest areg)
          (setq areg breg)))
      (unless atriv
        (if aconst
          (setq adest (arm2-one-untargeted-reg-form seg aform areg restricted))
          (arm2-elide-pushes seg apushed (arm2-pop-register seg (setq adest areg)))))
      (values adest bdest cdest))))


(defun arm2-four-untargeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (let* ((*arm2-nfp-depth* *arm2-nfp-depth*)
         (bnode (nx2-node-gpr-p breg))
         (cnode (nx2-node-gpr-p creg))
         (dnode (nx2-node-gpr-p dreg))
         (atriv (or (null aform) 
                    (and (arm2-trivial-p bform)
                         (arm2-trivial-p cform)
                         (arm2-trivial-p dform)
                         bnode
                         cnode
                         dnode)))
         (btriv (or (null bform)
                    (and (arm2-trivial-p cform)
                         (arm2-trivial-p dform)
                         cnode
                         dnode)))
         (ctriv (or (null cform)
                    (and (arm2-trivial-p dform) dnode)))
         (aconst (and (not atriv) 
                      (or (arm-side-effect-free-form-p aform)
                          (let ((avar (arm2-lexical-reference-p aform)))
                            (and avar 
                                 (nx2-var-not-set-by-form-p avar bform)
                                 (nx2-var-not-set-by-form-p avar cform)
                                 (nx2-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or
                       (arm-side-effect-free-form-p bform)
                       (let ((bvar (arm2-lexical-reference-p bform)))
                         (and bvar
                              (nx2-var-not-set-by-form-p bvar cform)
                              (nx2-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or
                       (arm-side-effect-free-form-p cform)
                       (let ((cvar (arm2-lexical-reference-p cform)))
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
          (setq adest (arm2-one-untargeted-reg-form seg aform areg)
                restricted (arm2-restrict-node-target adest restricted))
          (when (same-arm-reg-p adest breg)
            (setq breg areg))
          (when (same-arm-reg-p adest creg)
            (setq creg areg))
          (when (same-arm-reg-p adest dreg)
            (setq dreg areg)))
        (setq apushed (arm2-push-reg-for-form seg aform areg))))
    (if (and bform (not bconst))
      (if btriv
        (progn
          (setq bdest (arm2-one-untargeted-reg-form seg bform breg restricted)
                restricted (arm2-restrict-node-target bdest restricted))
          (unless adest
            (when (same-arm-reg-p areg bdest)
              (setq areg breg)))
          (when (same-arm-reg-p bdest creg)
            (setq creg breg))
          (when (same-arm-reg-p bdest dreg)
            (setq dreg breg)))
        (setq bpushed (arm2-push-reg-for-form seg bform breg))))
    (if (and cform (not cconst))
      (if ctriv
        (progn
          (setq cdest (arm2-one-untargeted-reg-form seg cform creg restricted)
                restricted (arm2-restrict-node-target cdest restricted))
          (unless adest
            (when (same-arm-reg-p areg cdest)
              (setq areg creg)))
          (unless bdest
            (when (same-arm-reg-p breg cdest)
              (setq breg creg)))          
          (when (same-arm-reg-p cdest dreg)
            (setq dreg creg)))
        (setq cpushed (arm2-push-reg-for-form seg cform creg))))
    (setq ddest (arm2-one-untargeted-reg-form seg dform dreg restricted)
          restricted (arm2-restrict-node-target ddest restricted))
    (unless adest
      (when (same-arm-reg-p ddest areg)
        (setq areg dreg)))
    (unless bdest
      (when (same-arm-reg-p ddest breg)
        (setq breg dreg)))
    (unless cdest
      (when (same-arm-reg-p ddest creg)
        (setq creg dreg)))
    (unless ctriv 
      (if cconst
        (setq cdest (arm2-one-untargeted-reg-form seg cform creg restricted))
        (arm2-elide-pushes seg cpushed (arm2-pop-register seg (setq cdest creg))))
      (setq restricted (arm2-restrict-node-target cdest restricted))
      (unless adest
        (when (same-arm-reg-p cdest areg)
          (setq areg creg)))
      (unless bdest
        (when (same-arm-reg-p ddest breg)
          (setq breg creg))))
    (unless btriv 
      (if bconst
        (setq bdest (arm2-one-untargeted-reg-form seg bform breg restricted))
        (arm2-elide-pushes seg bpushed (arm2-pop-register seg (setq bdest breg))))
      (setq restricted (arm2-restrict-node-target bdest restricted))
      (unless adest
        (when (same-arm-reg-p bdest areg)
          (setq areg breg))))
    (unless atriv
      (if aconst
        (setq adest (arm2-one-untargeted-reg-form seg aform areg restricted))
        (arm2-elide-pushes seg apushed (arm2-pop-register seg (setq adest areg)))))
    (values adest bdest cdest ddest)))

(defun arm2-lri (seg reg value)
  (with-arm-local-vinsn-macros (seg)
    (if (>= value 0)
      (! lri reg value)
      (! lri reg (logand value #xffffffff)))))


(defun arm2-multiple-value-body (seg form)
  (let* ((lab (backend-get-next-label))
         (*arm2-vstack* *arm2-vstack*)
         (old-stack (arm2-encode-stack)))
    (with-arm-local-vinsn-macros (seg)
      (arm2-open-undo $undomvexpect)
      (arm2-undo-body seg nil (logior $backend-mvpass-mask lab) form old-stack)
      (@ lab))))

(defun arm2-afunc-lfun-ref (afunc)
  (or
   (afunc-lfun afunc)
   (progn (pushnew afunc (afunc-fwd-refs *arm2-cur-afunc*) :test #'eq)
          afunc)))

(defun arm2-augment-arglist (afunc arglist &optional (maxregs $numarmargregs))
  (let ((inherited-args (afunc-inherited-vars afunc)))
    (when inherited-args
      (let* ((current-afunc *arm2-cur-afunc*)
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
        (%rplaca (%cdr arglist) regargs) ; might have started out NIL.
        (%rplaca arglist stkargs)))) 
  arglist)

(defun arm2-constant-for-compare-p (form &optional unboxed)
  (setq form (acode-unwrapped-form form))
  (when (acode-p form)
    (let* ((op (acode-operator form)))
      (if (eql op (%nx1-operator fixnum))
        (let* ((val (if unboxed
                      (car (acode-operands form))
                      (ash (car (acode-operands form)) arm::fixnumshift))))
          (if (or (arm::encode-arm-immediate val)
                  (arm::encode-arm-immediate (- val)))
            (logand val #xffffffff)))
        (if (eql op (%nx1-operator %unbound-marker))
          arm::unbound-marker
          (if (eql op (%nx1-operator %slot-unbound-marker))
            arm::slot-unbound-marker))))))

(defun arm2-acode-operator-supports-u8 (form)
  (setq form (acode-unwrapped-form-value form))
  (when (acode-p form)
    (let* ((operator (acode-operator form)))
      (if (member operator *arm2-operator-supports-u8-target*)
        (values operator (car (acode-operands form)))))))

(defun arm2-compare-u8 (seg vreg xfer form u8constant cr-bit true-p u8-operator)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (with-imm-target () (u8 :u8)
      (with-crf-target () crf
        (if (and (eql u8-operator (%nx1-operator lisptag))
                 (eql 0 u8constant)
                 (eql cr-bit arm::arm-cond-eq))
          (let* ((formreg (arm2-one-untargeted-reg-form seg form arm::arg_z)))
            (! test-fixnum crf formreg))
          (progn
           (arm2-use-operator u8-operator seg u8 nil form)
           (! compare-immediate crf u8 u8constant))))
      ;; Flags set.  Branch or return a boolean value ?
      (regspec-crf-gpr-case 
       (vreg)
       (^ cr-bit true-p)
       (progn
         (ensuring-node-target (target vreg)
           (if (not true-p)
             (setq cr-bit (logxor 1 cr-bit)))
           (! cond->boolean target cr-bit))
         (^))))))

;;; There are other cases involving constants that are worth exploiting.
(defun arm2-compare (seg vreg xfer i j cr-bit true-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((iu8 (let* ((i-fixnum (acode-fixnum-form-p i)))
                  (if (typep i-fixnum '(unsigned-byte 8))
                    i-fixnum)))
           (ju8 (let* ((j-fixnum (acode-fixnum-form-p j)))
                  (if (typep j-fixnum '(unsigned-byte 8))
                    j-fixnum)))
           (u8 (or iu8 ju8))
           (other-u8 (if iu8 j (if ju8 i)))
           (jconst (arm2-constant-for-compare-p j))
           (iconst (arm2-constant-for-compare-p i))
           (boolean (backend-crf-p vreg)))
      (multiple-value-bind (u8-operator u8-operand) (if other-u8 (arm2-acode-operator-supports-u8 other-u8))
        (if u8-operator
          (arm2-compare-u8 seg vreg xfer u8-operand u8 (if (and iu8 (not (eq cr-bit arm::arm-cond-eq))) (logxor 1 cr-bit) cr-bit) true-p u8-operator)
          (if (and boolean (or iconst jconst))
            (let* ((reg (arm2-one-untargeted-reg-form seg (if jconst i j) arm::arg_z)))
              (! compare-immediate vreg reg (or jconst iconst))
              (unless (or jconst (eq cr-bit arm::arm-cond-eq))
                (setq cr-bit (arm2-cr-bit-for-reversed-comparison cr-bit)))
              (^ cr-bit true-p))
            (if (or jconst iconst)
              (progn
                (unless (or jconst (eq cr-bit arm::arm-cond-eq))
                  (setq cr-bit (arm2-cr-bit-for-reversed-comparison cr-bit)))                (arm2-test-reg-%izerop 
                  seg 
                  vreg 
                  xfer 
                  (arm2-one-untargeted-reg-form 
                   seg 
                   (if jconst i j) 
                   arm::arg_z) 
                  cr-bit 
                  true-p 
                  (or jconst iconst)))
              (multiple-value-bind (ireg jreg) (arm2-two-untargeted-reg-forms seg i arm::arg_y j arm::arg_z)
                (arm2-compare-registers seg vreg xfer ireg jreg cr-bit true-p)))))))))

(defun arm2-natural-compare (seg vreg xfer i j cr-bit true-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((jconst (arm2-constant-for-compare-p j t))
           (iconst (arm2-constant-for-compare-p i t))
           (boolean (backend-crf-p vreg)))
          (if (and boolean (or iconst jconst))
            (let* ((reg (arm2-one-untargeted-reg-form seg (if jconst i j) ($ arm::imm0 :mode :u32))))
              (! compare-immediate vreg reg (or jconst iconst))
              (unless (or jconst (eq cr-bit arm::arm-cond-eq))
                (setq cr-bit (arm2-cr-bit-for-reversed-comparison cr-bit)))
              (^ cr-bit true-p))
            (if (and (eq cr-bit arm::arm-cond-eq) 
                     (or jconst iconst))
              (arm2-test-reg-%izerop 
               seg 
               vreg 
               xfer 
               (arm2-one-untargeted-reg-form 
                seg 
                (if jconst i j) 
                ($ arm::imm0 :mode :u32))
               cr-bit 
               true-p 
               (or jconst iconst))
              (multiple-value-bind (ireg jreg) (arm2-two-untargeted-reg-forms seg i ($ arm::imm0 :mode :u32)  j ($ arm::imm1 :mode :u32))
                (arm2-compare-registers seg vreg xfer ireg jreg cr-bit true-p)))))))



(defun arm2-compare-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
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

(defun arm2-compare-register-to-nil (seg vreg xfer ireg cr-bit true-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
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

(defun arm2-compare-double-float-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
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

(defun arm2-compare-single-float-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
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




(defun arm2-immediate-form-p (form)
  (if (and (acode-p form)
           (or (eq (acode-operator form) (%nx1-operator immediate))
               (eq (acode-operator form) (%nx1-operator simple-function))))
    t))

(defun arm2-test-%izerop (seg vreg xfer form cr-bit true-p)
  (arm2-test-reg-%izerop seg vreg xfer (arm2-one-untargeted-reg-form seg form arm::arg_z) cr-bit true-p 0))

(defun arm2-test-reg-%izerop (seg vreg xfer reg cr-bit true-p  zero)
  (declare (fixnum reg))
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (regspec-crf-gpr-case 
     (vreg)
     (progn
       (if (or (arm::encode-arm-immediate zero)
               (arm::encode-arm-immediate (- zero)))
         (! compare-immediate vreg reg zero)
         (with-node-target (reg) other
           (arm2-lri seg other zero)
           (! compare vreg reg other)))
       (^ cr-bit true-p))
     (with-crf-target () crf
       (if (or (arm::encode-arm-immediate zero)
               (arm::encode-arm-immediate (- zero)))
         (! compare-immediate crf reg (logand #xffffffff zero))
         (with-node-target (reg) other
           (arm2-lri seg other zero)
           (! compare crf reg other)))
       (ensuring-node-target (target vreg)
         (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
       (^)))))

(defun arm2-lexical-reference-ea (form &optional (no-closed-p t))
  (when (acode-p (setq form (acode-unwrapped-form-value form)))
    (if (eq (acode-operator form) (%nx1-operator lexical-reference))
      (let* ((addr (var-ea (car (acode-operands form)))))
        (if (typep addr 'lreg)
          addr
          (unless (and no-closed-p (addrspec-vcell-p addr ))
            addr))))))


(defun arm2-vpush-register (seg src)
  (with-arm-local-vinsn-macros (seg)
    (prog1
      (! vpush-register src)
      (arm2-regmap-note-store src *arm2-vstack*)
      (arm2-adjust-vstack *arm2-target-node-size*))))

(defun arm2-vpush-register-arg (seg src)
  (arm2-vpush-register seg src))


(defun arm2-vpop-register (seg dest)
  (with-arm-local-vinsn-macros (seg)
    (prog1
      (! vpop-register dest)
      (arm2-adjust-vstack (- *arm2-target-node-size*)))))



     
        

  
      
      
(defun arm2-copy-register (seg dest src)
  (with-arm-local-vinsn-macros (seg)
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
              (! set-eq-bit dest-crf)))
          (if dest-crf
            ;; "Copying" a GPR to a CR field means comparing it to nil
            (if src-gpr
              (! compare-to-nil dest src)
              (! compare-to-nil dest arm::sp))
            (if (and dest-gpr src-gpr)
              (case dest-mode
                (#.hard-reg-class-gpr-mode-node ; boxed result.
                 (case src-mode
                   (#.hard-reg-class-gpr-mode-node
                    (unless (eql  dest-gpr src-gpr)
                      (! copy-gpr dest src)))
                   (#.hard-reg-class-gpr-mode-u32
                    (arm2-box-u32 seg dest src))
                   (#.hard-reg-class-gpr-mode-s32
                    (arm2-box-s32 seg dest src))
                   (#.hard-reg-class-gpr-mode-u16
                    (! u16->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-s16
                    (! s16->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-u8
                    (! u8->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-s8
                    (! s8->fixnum dest src))
                   (#.hard-reg-class-gpr-mode-address
                    (! macptr->heap dest src))))
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
                                     *arm2-reckless*)
                           (! trap-unless-macptr src))
                         (! deref-macptr dest src)))))
                   ((#.hard-reg-class-gpr-mode-u32
                     #.hard-reg-class-gpr-mode-s32
                     #.hard-reg-class-gpr-mode-address)
                    (unless (eql  dest-gpr src-gpr)
                      (! copy-gpr dest src)))
                   ((#.hard-reg-class-gpr-mode-u16
                     #.hard-reg-class-gpr-mode-s16)
                    (! u16->u32 dest src))
                   ((#.hard-reg-class-gpr-mode-u8
                     #.hard-reg-class-gpr-mode-s8)
                    (! u8->u32 dest src))))
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
                    (if *arm2-reckless*
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
                      (! copy-gpr dest src))))))
              (if src-gpr
                (if dest-fpr
                  (progn
                    (case src-mode
                      (#.hard-reg-class-gpr-mode-node
                       (case dest-mode
                         (#.hard-reg-class-fpr-mode-double
                          (unless (or (logbitp hard-reg-class-fpr-type-double 
                                               (get-node-regspec-type-modes src))
                                      *arm2-reckless*)
                            (! trap-unless-double-float src))
                          (! get-double dest src))
                         (#.hard-reg-class-fpr-mode-single
                          (unless *arm2-reckless*
                            (! trap-unless-single-float src))
                          (! get-single dest src))
                                                  (#.hard-reg-class-fpr-mode-complex-single-float
                          (unless *arm2-reckless*
                            (! trap-unless-typecode= src arm::subtag-complex-single-float))
                          (! get-complex-single-float dest src))
                         (#.hard-reg-class-fpr-mode-complex-double-float
                          (unless *arm2-reckless*
                            (! trap-unless-typecode= src arm::subtag-complex-double-float))
                          (! get-complex-double-float dest src)))))))
                (if dest-gpr
                  (case dest-mode
                    (#.hard-reg-class-gpr-mode-node
                     (if src-fpr
                       (case src-mode
                         (#.hard-reg-class-fpr-mode-double
                          (! double->heap dest src))
                         (#.hard-reg-class-fpr-mode-complex-double-float
                            (! complex-double-float->heap dest src))
                         (#.hard-reg-class-fpr-mode-single
                          (! single->node dest src))
                         (#.hard-reg-class-fpr-mode-complex-single-float
                            (! complex-single-float->node dest src))))))
                  (if (and src-fpr dest-fpr)
                    (unless (and (eql dest-fpr src-fpr)
                                 (eql dest-mode src-mode))
                      (case src-mode
                        (#.hard-reg-class-fpr-mode-single
                         (case dest-mode
                           (#.hard-reg-class-fpr-mode-single
                            (! single-to-single dest src))
                           (#.hard-reg-class-fpr-mode-double
                            (if *arm2-float-safety*
                              (with-fp-target (src dest) (temp :double-float)
                                (! clear-pending-fpu-exceptions)
                                (! single-to-double temp src)
                                (! trap-if-fpu-exception)
                                (! double-to-double dest temp))
                              (! single-to-double dest src)))))
                        (#.hard-reg-class-fpr-mode-double
                         (case dest-mode
                           (#.hard-reg-class-fpr-mode-single
                            (if *arm2-float-safety*
                              (with-fp-target (dest src) (temp :single-float)
                                (! clear-pending-fpu-exceptions)
                                (! double-to-single temp src)
                                (! trap-if-fpu-execption)
                                (! single-to-single dest temp))
                              (! double-to-single dest src)))
                           (#.hard-reg-class-fpr-mode-double
                            (! double-to-double dest src))
                           ))
                        (#.hard-reg-class-fpr-mode-complex-single-float
                           (case dest-mode
                             (#.hard-reg-class-fpr-mode-complex-single-float
                              (! complex-single-float-to-complex-single-float dest src))))
                        (#.hard-reg-class-fpr-mode-complex-double-float
                           (case dest-mode
                             (#.hard-reg-class-fpr-mode-complex-double-float
                              (! complex-double-float-to-complex-double-float dest src))))))))))))))))
  
(defun arm2-unreachable-store (&optional vreg)
  ;; I don't think that anything needs to be done here,
  ;; but leave this guy around until we're sure.
  ;; (ARM2-VPUSH-REGISTER will always vpush something, even
  ;; if code to -load- that "something" never gets generated.
  ;; If I'm right about this, that means that the compile-time
  ;; stack-discipline problem that this is supposed to deal
  ;; with can't happen.)
  (declare (ignore vreg))
  nil)

;;; bind vars to initforms, as per let*, &aux.
(defun arm2-seq-bind (seg vars initforms)
    (dolist (var vars)
      (arm2-seq-bind-var seg var (pop initforms))))

(defun arm2-dynamic-extent-form (seg curstack val &aux (form val))
  (when (acode-p form)
    (arm-with-note (form seg curstack) ; note this rebinds form/seg/curstack so can't setq
      (with-arm-local-vinsn-macros (seg)
	(let* ((op (acode-operator form))
               (operands (acode-operands form)))
	  (cond ((eq op (%nx1-operator list))
		 (let* ((*arm2-vstack* *arm2-vstack*))
		   (arm2-set-nargs seg (arm2-formlist seg (car operands) nil))
		   (arm2-open-undo $undostkblk curstack)
		   (! stack-cons-list))
		 (setq val arm::arg_z))
		((eq op (%nx1-operator list*))
		 (let* ((arglist (car operands)))
		   (let* ((*arm2-vstack* *arm2-vstack*))
		     (arm2-arglist seg arglist))
		   (when (car arglist)
		     (arm2-set-nargs seg (length (%car arglist)))
		     (! stack-cons-list*)
		     (arm2-open-undo $undostkblk curstack))
		   (setq val arm::arg_z)))
		((eq op (%nx1-operator multiple-value-list))
		 (arm2-multiple-value-body seg (car operands))
		 (arm2-open-undo $undostkblk curstack)
		 (! stack-cons-list)
		 (setq val arm::arg_z))
		((eq op (%nx1-operator cons))
		 (let* ((y ($ arm::arg_y))
			(z ($ arm::arg_z))
			(result ($ arm::arg_z)))
		   (arm2-two-targeted-reg-forms seg (car operands) y (cadr operands) z)
		   (arm2-open-undo $undostkblk )
		   (! make-stack-cons result y z) 
		   (setq val result)))
		((eq op (%nx1-operator %consmacptr%))
		 (with-imm-target () (address :address)
		   (arm2-one-targeted-reg-form seg form address)
		   (with-node-temps () (node)
		     (! macptr->stack node address)
		     (arm2-open-undo $undostkblk)
		     (setq val node))))
		((eq op (%nx1-operator %new-ptr))
		 (let* ((clear-form (cadr operands))
			(cval (nx2-constant-form-value clear-form)))
		   (if cval
		       (progn 
			 (arm2-one-targeted-reg-form seg (car operands) ($ arm::arg_z))
			 (if (nx-null cval)
			     (! make-stack-block)
			     (! make-stack-block0)))
		       (with-crf-target () crf
			 (let ((stack-block-0-label (backend-get-next-label))
			       (done-label (backend-get-next-label))
			       (rval ($ arm::arg_z))
			       (rclear ($ arm::arg_y)))
			   (arm2-two-targeted-reg-forms seg (car operands) rval clear-form rclear)
			   (! compare-to-nil crf rclear)
			   (! cbranch-false (aref *backend-labels* stack-block-0-label) crf arm::arm-cond-eq)
			   (! make-stack-block)
			   (-> done-label)
			   (@ stack-block-0-label)
			   (! make-stack-block0)
			   (@ done-label)))))
		 (arm2-open-undo $undostkblk)
		 (setq val ($ arm::arg_z)))
		((eq op (%nx1-operator make-list))
		 (arm2-two-targeted-reg-forms seg (car (acode-operands form)) ($ arm::arg_y) (cadr (acode-operands form)) ($ arm::arg_z))
		 (arm2-open-undo $undostkblk curstack)
		 (! make-stack-list)
		 (setq val arm::arg_z))       
		((eq op (%nx1-operator vector))
		 (let* ((*arm2-vstack* *arm2-vstack*))
		   (arm2-set-nargs seg (arm2-formlist seg (car operands) nil))
		   (! make-stack-vector))
		 (arm2-open-undo $undostkblk)
		 (setq val arm::arg_z))
		((eq op (%nx1-operator %gvector))
		 (let* ((*arm2-vstack* *arm2-vstack*)
			(arglist (car (acode-operands form))))
		   (arm2-set-nargs seg (arm2-formlist seg (append (car arglist) (reverse (cadr arglist))) nil))
		   (! make-stack-gvector))
		 (arm2-open-undo $undostkblk)
		 (setq val arm::arg_z)) 
		((eq op (%nx1-operator closed-function)) 
		 (setq val (arm2-make-closure seg (car operands) t))) ; can't error
		((eq op (%nx1-operator %make-uvector))
		 (destructuring-bind (element-count subtag &optional (init 0 init-p)) operands
		   (if init-p
		       (progn
			 (arm2-three-targeted-reg-forms seg element-count ($ arm::arg_x) subtag ($ arm::arg_y) init ($ arm::arg_z))
			 (! stack-misc-alloc-init))
		       (progn
			 (arm2-two-targeted-reg-forms seg element-count ($ arm::arg_y)  subtag ($ arm::arg_z))
			 (! stack-misc-alloc)))
		   (arm2-open-undo $undostkblk)
		   (setq val ($ arm::arg_z)))))))))
  val)
;;; this far
(defun arm2-addrspec-to-reg (seg addrspec reg)
  (if (memory-spec-p addrspec)
    (arm2-stack-to-register seg addrspec reg)
    (arm2-copy-register seg reg addrspec)))
  
(defun arm2-seq-bind-var (seg var val)

  (with-arm-local-vinsn-macros (seg)
    (let* ((sym (var-name var))
           (bits (nx-var-bits var))
           (ea nil)
           (closed-p (and (%ilogbitp $vbitclosed bits)
                          (%ilogbitp $vbitsetq bits)))
           (curstack (arm2-encode-stack))
           (make-vcell (and closed-p (eq bits (var-bits var))))
           (closed-downward (and closed-p (%ilogbitp $vbitcloseddownward bits))))
      (unless (fixnump val)
        (setq val (nx-untyped-form val))
        (when (and (%ilogbitp $vbitdynamicextent bits) (acode-p val))
          (setq val (arm2-dynamic-extent-form seg curstack val))))
      (if (%ilogbitp $vbitspecial bits)
        (progn
          (arm2-dbind seg val sym)
          (arm2-set-var-ea seg var (arm2-vloc-ea (- *arm2-vstack* *arm2-target-node-size*))))
        (let ((puntval nil))
          (flet ((arm2-puntable-binding-p (var initform)
                   ; The value returned is acode.
                   (let* ((bits (nx-var-bits var)))
                     (if (%ilogbitp $vbitpuntable bits)
                       initform))))
            (declare (inline arm2-puntable-binding-p))
            (if (and (not (arm2-load-ea-p val))
                     (setq puntval (arm2-puntable-binding-p var val)))
              (progn
                (nx-set-var-bits var (%ilogior (%ilsl $vbitpunted 1) bits))
                (nx2-replace-var-refs var puntval)
                (arm2-set-var-ea seg var puntval))
              (progn
                (let* ((vloc *arm2-vstack*)
                       (reg (let* ((r (nx2-assign-register-var var)))
                              (if r (make-wired-lreg r :class (hard-regspec-class r) :mode (get-regspec-mode r))))))
                  (if (arm2-load-ea-p val)
                    (if reg
                      (arm2-addrspec-to-reg seg val reg)
                      (if (memory-spec-p val)
                        (with-node-temps () (temp)
                          (arm2-addrspec-to-reg seg val temp)
                          (arm2-vpush-register seg temp))
                        (arm2-vpush-register seg val)))
                    (if reg
                      (arm2-one-targeted-reg-form seg val reg)
                      (or (setq ea (arm2-nfp-bind seg var val))
                          (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg val arm::arg_z)))))
                  (arm2-set-var-ea seg var (or ea reg (arm2-vloc-ea vloc closed-p)))
                  (when make-vcell
                    (with-node-temps () (vcell closed)
                        (arm2-stack-to-register seg vloc closed)
                        (if closed-downward
                          (progn
                            (! make-stack-vcell vcell closed)
                            (arm2-open-undo $undostkblk))
                          (! make-vcell vcell closed))
                        (arm2-register-to-stack seg vcell vloc))))))))))))



;;; Never make a vcell if this is an inherited var.
;;; If the var's inherited, its bits won't be a fixnum (and will
;;; therefore be different from what NX-VAR-BITS returns.)
(defun arm2-bind-var (seg var vloc &aux 
                          (bits (nx-var-bits var)) 
                          (closed-p (and (%ilogbitp $vbitclosed bits) (%ilogbitp $vbitsetq bits)))
                          (closed-downward (if closed-p (%ilogbitp $vbitcloseddownward bits)))
                          (make-vcell (and closed-p (eq bits (var-bits var))))
                          (addr (arm2-vloc-ea vloc)))
  (with-arm-local-vinsn-macros (seg)
    (if (%ilogbitp $vbitspecial bits)
      (progn
        (arm2-dbind seg addr (var-name var))
        (arm2-set-var-ea seg var (arm2-vloc-ea (- *arm2-vstack* *arm2-target-node-size*)))
        t)
      (progn
        (when (%ilogbitp $vbitpunted bits)
          (compiler-bug "bind-var: var ~s was punted" var))
        (when make-vcell
          (with-node-temps () (vcell closed)
            (arm2-stack-to-register seg vloc closed)
            (if closed-downward
              (progn
                (! make-stack-vcell vcell closed)
                (arm2-open-undo $undostkblk))
              (! make-vcell vcell closed))
            (arm2-register-to-stack seg vcell vloc)))
          
        (arm2-set-var-ea seg var (arm2-vloc-ea vloc closed-p))        
        closed-downward))))

(defun arm2-set-var-ea (seg var ea)
  (setf (var-ea var) ea)
  (when (and *arm2-record-symbols* (or (typep ea 'lreg) (typep ea 'fixnum)))
    (let* ((start (enqueue-vinsn-note seg :begin-variable-scope var)))
      (push (list var (var-name var) start nil)
            *arm2-recorded-symbols*)))
  ea)

(defun arm2-close-var (seg var)
  (let ((bits (nx-var-bits var)))
    (when (and *arm2-record-symbols*
               (or (logbitp $vbitspecial bits)
                   (not (logbitp $vbitpunted bits))))
      (let* ((info (%cdr (assq var *arm2-recorded-symbols*))))
        (unless info (compiler-bug "arm2-close-var for ~s ?" (var-name var)))
        (setf (caddr info) (close-vinsn-note seg (cadr info)))))))

(defun arm2-load-ea-p (ea)
  (or (typep ea 'fixnum)
      (typep ea 'lreg)))

(defun arm2-dbind (seg value sym)
  (with-arm-local-vinsn-macros (seg)
    (let* ((ea-p (arm2-load-ea-p value))
           (nil-p (unless ea-p (nx-null (setq value (nx-untyped-form value)))))
           (self-p (unless ea-p (and (or
                                      (eq (acode-operator value) (%nx1-operator bound-special-ref))
                                      (eq (acode-operator value) (%nx1-operator special-ref)))
                                     (eq (car (acode-operands value)) sym)))))
      (cond ((eq sym '*interrupt-level*)
             (let* ((fixval (acode-fixnum-form-p value)))
               (cond ((eql fixval 0) (if *arm2-open-code-inline*
                                       (! bind-interrupt-level-0-inline)
                                       (! bind-interrupt-level-0)))
                     ((eql fixval -1) (if *arm2-open-code-inline*
                                        (! bind-interrupt-level-m1-inline)
                                        (! bind-interrupt-level-m1)))
                     (t
                      (if ea-p 
                        (arm2-store-ea seg value arm::arg_z)
                        (arm2-one-targeted-reg-form seg value ($ arm::arg_z)))
                      (! bind-interrupt-level))))
             (arm2-open-undo $undointerruptlevel))
            (t
             (if (or nil-p self-p)
               (progn
                 (arm2-store-immediate seg (arm2-symbol-value-cell sym) arm::arg_z)
                 (if nil-p
                   (! bind-nil)
                   (if (or *arm2-reckless* (eq (acode-operator value) (%nx1-operator special-ref)))
                     (! bind-self)
                     (! bind-self-boundp-check))))
               (progn
                 (if ea-p 
                   (arm2-store-ea seg value arm::arg_z)
                   (arm2-one-targeted-reg-form seg value ($ arm::arg_z)))
                 (arm2-store-immediate seg (arm2-symbol-value-cell sym) ($ arm::arg_y))
                 (! bind)))
             (arm2-open-undo $undospecial)))
      (arm2-adjust-vstack (* 3 *arm2-target-node-size*)))))

;;; Store the contents of EA - which denotes either a vframe location
;;; or a hard register - in reg.

(defun arm2-store-ea (seg ea reg)
  (if (typep ea 'fixnum)
    (if (memory-spec-p ea)
      (arm2-stack-to-register seg ea reg)
      (arm2-copy-register seg reg ea))
    (if (typep ea 'lreg)
      (arm2-copy-register seg reg ea))))


      

;;; Callers should really be sure that this is what they want to use.
(defun arm2-absolute-natural (seg vreg xfer value)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (arm2-lri seg vreg value))
    (^)))

(defun arm2-natural-constant (seg vreg xfer value)
  (arm2-use-operator
   (if (typep value *nx-target-fixnum-type*)
     (%nx1-operator fixnum)
     (%nx1-operator immediate))
   seg vreg xfer value))
    


(defun arm2-store-macptr (seg vreg address-reg)
  (with-arm-local-vinsn-macros (seg vreg)
    (when (arm2-for-value-p vreg)
      (if (logbitp vreg arm-imm-regs)
        (<- address-reg)
        (! macptr->heap vreg address-reg)))))

(defun arm2-store-signed-longword (seg vreg imm-reg)
  (with-arm-local-vinsn-macros (seg vreg)
    (when (arm2-for-value-p vreg)
      (if (logbitp vreg arm-imm-regs)
        (<- imm-reg)
        (arm2-box-s32 seg vreg imm-reg)))))

(defun arm2-store-signed-halfword (seg vreg imm-reg)
  (with-arm-local-vinsn-macros (seg vreg)
    (when (arm2-for-value-p vreg)
      (if (logbitp vreg arm-imm-regs)
        (<- imm-reg)
        (! s16->fixnum vreg imm-reg)))))


(defun arm2-store-unsigned-halfword (seg vreg imm-reg)
  (with-arm-local-vinsn-macros (seg vreg)
    (when (arm2-for-value-p vreg)
      (if (logbitp vreg arm-imm-regs)
        (<- imm-reg)
        (! u16->fixnum vreg imm-reg)))))



;;; If "value-first-p" is true and both "offset" and "val" need to be 
;;; evaluated, evaluate "val" before evaluating "offset".
(defun arm2-%immediate-set-ptr (seg vreg xfer  ptr offset val)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((intval (acode-absolute-ptr-p val))
           (offval (acode-fixnum-form-p offset))
           (for-value (arm2-for-value-p vreg)))
      (flet ((address-and-node-regs ()
               (if for-value
                 (progn
                   (arm2-one-targeted-reg-form seg val ($ arm::arg_z))
                   (progn
                     (if intval
                       (arm2-lri seg arm::imm0 intval)
                       (! deref-macptr arm::imm0 arm::arg_z))
                     (values arm::imm0 arm::arg_z)))
                 (values (arm2-macptr-arg-to-reg seg val ($ arm::imm0 :mode :address)) nil))))

        (and offval (%i> (integer-length offval) 11) (setq offval nil))
        (if offval
                                        ; Easier: need one less register than in the general case.
          (with-imm-target () (ptr-reg :address)
            (arm2-one-targeted-reg-form seg ptr ptr-reg)
            (if intval
              (with-imm-target (ptr-reg) (val-target :address)
                (arm2-lri seg val-target intval)
                (! mem-set-c-address val-target ptr-reg offval)
                (if for-value
                  (<- (set-regspec-mode val-target (gpr-mode-name-value :address)))))
              (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                (arm2-push-register seg  ptr-reg)
                (multiple-value-bind (address node) (address-and-node-regs)
                  (with-imm-target (address) (ptr-reg :address)
                    (arm2-pop-register seg ptr-reg)
                    (! mem-set-c-address address ptr-reg offval)
                    (if for-value
                      (<- node)))))))
          ;; No (16-bit) constant offset.  Might still have a 32-bit
          ;; constant offset; might have a constant value.  Might
          ;; not.  Might not.  Easiest to special-case the
          ;; constant-value case first ...
          (let* ((xptr-reg nil)
                 (xoff-reg nil)
                 (xval-reg nil)
                 (node-arg_z nil)
                 (constant-offset (acode-fixnum-form-p offset)))
            (if intval
              (if constant-offset
                (with-imm-target () (ptr-reg :address)
                  (arm2-one-targeted-reg-form seg ptr ptr-reg)
                  (with-imm-target (ptr-reg) (off-reg :signed-natural)
                    (arm2-lri seg off-reg constant-offset)
                    (with-imm-target (ptr-reg off-reg) (val-reg :address)
                      (arm2-lri seg val-reg intval)
                      (setq xptr-reg ptr-reg
                            xoff-reg off-reg
                            xval-reg val-reg))))
                ;; Offset's non-constant.  Temp-push the pointer, evaluate
                ;; and unbox the offset, load the value, pop the pointer.
                (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                  (with-imm-target () (ptr-reg :address)
                    (arm2-one-targeted-reg-form seg ptr ptr-reg)
                    (arm2-push-register seg ptr-reg))
                  (with-imm-target () (off-reg :signed-natural)
                    (! fixnum->signed-natural off-reg (arm2-one-targeted-reg-form seg offset ($ arm::arg_z)))
                    (with-imm-target (off-reg) (val-reg :signed-natural)
                      (arm2-lri seg val-reg intval)
                      (with-imm-target (off-reg val-reg) (ptr-reg :address)
                        (arm2-pop-register seg ptr-reg)
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg val-reg))))))
              ;; No intval; maybe constant-offset.
              (with-imm-target () (ptr-reg :address)
                (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                (arm2-one-targeted-reg-form seg ptr ptr-reg)
                (arm2-push-register seg ptr-reg)
                (progn
                  (if (not constant-offset)
                    (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg offset arm::arg_z)))
                  (multiple-value-bind (address node) (address-and-node-regs)
                    (with-imm-target (address) (off-reg :s32)
                      (if constant-offset
                        (arm2-lri seg off-reg constant-offset)
                        (with-node-temps (arm::arg_z) (temp)
                          (arm2-vpop-register seg temp)
                          (! fixnum->signed-natural off-reg temp)))
                      (with-imm-target (arm::imm0 off-reg) (ptr-reg :address)
                        (arm2-pop-register seg ptr-reg)
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg address
                              node-arg_z node))))))))
            (! mem-set-address xval-reg xptr-reg xoff-reg)
            (when for-value
              (if node-arg_z
                (<- node-arg_z)
                (<- (set-regspec-mode 
                     xval-reg
                     (gpr-mode-name-value :address)))))))
        (^)))))
  
(defun arm2-memory-store-displaced (seg valreg basereg displacement size)
  (with-arm-local-vinsn-macros (seg)
    (case size
      ;;(8 (! mem-set-c-doubleword valreg basereg displacement))
      (4 (! mem-set-c-fullword valreg basereg displacement))
      (2 (! mem-set-c-halfword valreg basereg displacement))
      (1 (! mem-set-c-byte valreg basereg displacement)))))

(defun arm2-memory-store-indexed (seg valreg basereg idxreg size)
  (with-arm-local-vinsn-macros (seg)
    (case size
      ;;(8 (! mem-set-doubleword valreg basereg idxreg))
      (4 (! mem-set-fullword valreg basereg idxreg))
      (2 (! mem-set-halfword valreg basereg idxreg))
      (1 (! mem-set-byte valreg basereg idxreg)))))
      
(defun arm2-%immediate-store  (seg vreg xfer bits ptr offset val)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if (eql 0 (%ilogand #xf bits))
      (arm2-%immediate-set-ptr seg vreg xfer  ptr offset val)
      (let* ((size (logand #xf bits))
             (nbits (ash size 3))
             (signed (not (logbitp 5 bits)))
             (intval (acode-integer-constant-p val nbits))
             (offval (acode-fixnum-form-p offset))
             (for-value (arm2-for-value-p vreg)))
        (declare (fixnum size))
        (flet ((val-to-argz-and-imm0 ()
                 (arm2-one-targeted-reg-form seg val ($ arm::arg_z))
                 (if (eq size 8)
                   (if signed
                     (! gets64)
                     (! getu64))
                   (if (eq size 4)
                     (if signed
                       (! gets32)
                       (! getu32))
                     (! fixnum->signed-natural arm::imm0 arm::arg_z)))))

          (and offval (%i> (integer-length offval) 11) (setq offval nil))
          (if offval
            ;; Easier: need one less register than in the general case.
            (with-imm-target () (ptr-reg :address)
              (arm2-one-targeted-reg-form seg ptr ptr-reg)
              (if intval
                (with-imm-target (ptr-reg) (val-target :s32)                    
                  (arm2-lri seg val-target intval)
                  (arm2-memory-store-displaced seg val-target ptr-reg offval size)
                  (if for-value
                    (<- (set-regspec-mode 
                         val-target 
                         (gpr-mode-name-value
                          (case size
                            (8 (if signed :s64 :u64))
                            (4 (if signed :s32 :u32))
                            (2 (if signed :s16 :u16))
                            (1 (if signed :s8 :u8))))))))
                (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                  (arm2-push-register seg ptr-reg)
                  
                  (val-to-argz-and-imm0)                  
                  (with-imm-target (arm::imm0) (ptr-reg :address)
                    (arm2-pop-register seg ptr-reg)
                    (arm2-memory-store-displaced seg arm::imm0 ptr-reg offval size)                    
                    (if for-value
                      (<- arm::arg_z))))))
            ;; No (16-bit) constant offset.  Might still have a 32-bit constant offset;
            ;; might have a constant value.  Might not.  Might not.
            ;; Easiest to special-case the constant-value case first ...
            (let* ((xptr-reg nil)
                   (xoff-reg nil)
                   (xval-reg nil)
                   (node-arg_z nil)
                   (constant-offset (acode-fixnum-form-p offset)))
              (if intval
                (if constant-offset
                  (with-imm-target () (ptr-reg :address)
                    (arm2-one-targeted-reg-form seg ptr ptr-reg)
                    (with-imm-target (ptr-reg) (off-reg :s32)
                      (arm2-lri seg off-reg constant-offset)
                      (with-imm-target (ptr-reg off-reg) (val-reg :s32)
                        (arm2-lri seg val-reg intval)
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg val-reg))))
                                        ; Offset's non-constant.  Temp-push the pointer, evaluate
                                        ; and unbox the offset, load the value, pop the pointer.
                  (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                    (with-imm-target () (ptr-reg :address)
                        (arm2-one-targeted-reg-form seg ptr ptr-reg)
                        (arm2-push-register seg ptr-reg)
                      (with-imm-target () (off-reg :s32)
                        (! fixnum->signed-natural off-reg (arm2-one-targeted-reg-form seg offset ($ arm::arg_z)))
                        (with-imm-target (off-reg) (val-reg :s32)
                          (arm2-lri seg val-reg intval)
                          (with-imm-target (off-reg val-reg) (ptr-reg :address)
                            (arm2-pop-register seg ptr-reg)
                            (setq xptr-reg ptr-reg
                                  xoff-reg off-reg
                                  xval-reg val-reg)))))))
                ;; No intval; maybe constant-offset.
                (with-imm-target () (ptr-reg :address)
                  (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                    (arm2-one-targeted-reg-form seg ptr ptr-reg)
                    (arm2-push-register seg ptr-reg)
                    (progn
                      (if (not constant-offset)
                        (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg offset arm::arg_z)))
                      (val-to-argz-and-imm0)
                      (with-imm-target (arm::imm0) (off-reg :signed-natural)
                        (if constant-offset
                          (arm2-lri seg off-reg constant-offset)
                          (with-node-temps (arm::arg_z) (temp)
                              (arm2-vpop-register seg temp)
                            (! fixnum->signed-natural off-reg temp)))
                        (with-imm-target (arm::imm0 off-reg) (ptr-reg :address)
                          (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                            (arm2-pop-register seg ptr-reg))
                          (setq xptr-reg ptr-reg
                                xoff-reg off-reg
                                xval-reg arm::imm0
                                node-arg_z t)))))))
              (arm2-memory-store-indexed seg xval-reg xptr-reg xoff-reg size)
              (when for-value
                (if node-arg_z
                  (<- arm::arg_z)
                  (<- (set-regspec-mode 
                       xval-reg
                       (gpr-mode-name-value
                        (case size
                          (8 (if signed :s64 :u64))
                          (4 (if signed :s32 :u32))
                          (2 (if signed :s16 :u16))
                          (1 (if signed :s8 :u8))))))))))
          (^))))))





(defun arm2-encoding-undo-count (encoding)
 (svref encoding 0))

(defun arm2-encoding-cstack-depth (encoding)    ; hardly ever interesting
  (svref encoding 1))

(defun arm2-encoding-vstack-depth (encoding)
  (svref encoding 2))



(defun arm2-encode-stack ()
  (vector *arm2-undo-count* *arm2-cstack* *arm2-vstack* ))

(defun arm2-decode-stack (encoding)
  (values (arm2-encoding-undo-count encoding)
          (arm2-encoding-cstack-depth encoding)
          (arm2-encoding-vstack-depth encoding)))

(defun arm2-equal-encodings-p (a b)
  (dotimes (i 3 t)
    (unless (eq (svref a i) (svref b i)) (return))))

(defun arm2-open-undo (&optional (reason $undocatch) (curstack (arm2-encode-stack)))
  (set-fill-pointer 
   *arm2-undo-stack*
   (set-fill-pointer *arm2-undo-because* *arm2-undo-count*))
  (vector-push-extend curstack *arm2-undo-stack*)
  (vector-push-extend reason *arm2-undo-because*)
  (setq *arm2-undo-count* (%i+ *arm2-undo-count* 1)))

(defun arm2-close-undo (&aux
                        (new-count (%i- *arm2-undo-count* 1))
                        (i (aref *arm2-undo-stack* new-count)))
  (multiple-value-setq (*arm2-undo-count* *arm2-cstack* *arm2-vstack* )
    (arm2-decode-stack i))
  (set-fill-pointer 
   *arm2-undo-stack*
   (set-fill-pointer *arm2-undo-because* new-count)))


(defun arm2-nfp-ref-p (form)
  (let* ((op (if (acode-p form) (acode-operator form))))
    (if (or (eq op (%nx1-operator inherited-arg)) 
            (eq op (%nx1-operator lexical-reference)))
      (let* ((var (car (acode-operands form))))
        (not (null (rassoc var *arm2-nfp-vars*)))))))



;;; "Trivial" means can be evaluated without allocating or modifying registers.
;;; Interim definition, which will probably stay here forever.
(defun arm2-trivial-p (form &aux op bits)
  (setq form (nx-untyped-form form))
  (and
   (acode-p form)
   (not (eq (setq op (acode-operator form)) (%nx1-operator call)))
   (not (arm2-nfp-ref-p form))
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
         (or (%ilogbitp $vbitpunted (setq bits (nx-var-bits (car (acode-operands form)))))
             (neq (%ilogior (%ilsl $vbitclosed 1) (%ilsl $vbitsetq 1))
                  (%ilogand (%ilogior (%ilsl $vbitclosed 1) (%ilsl $vbitsetq 1)) bits)))))))

(defun arm2-lexical-reference-p (form)
  (when (acode-p form)
    (let ((op (acode-operator (setq form (acode-unwrapped-form-value form)))))
      (when (or (eq op (%nx1-operator lexical-reference))
                (eq op (%nx1-operator inherited-arg)))
        (car (acode-operands form))))))



(defun arm2-ref-symbol-value (seg vreg xfer sym check-boundp)
  (declare (ignorable check-boundp))
  (setq check-boundp (not *arm2-reckless*))
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (when (or check-boundp vreg)
      (unless vreg (setq vreg ($ arm::arg_z)))
      (if (eq sym '*interrupt-level*)
          (ensuring-node-target (target vreg)
            (! ref-interrupt-level target))
          (if *arm2-open-code-inline*
            (ensuring-node-target (target vreg)
              (with-node-target (target) src
                (let* ((vcell (arm2-symbol-value-cell sym))
                       (reg (arm2-register-constant-p vcell)))
                  (if reg
                    (setq src reg)
                    (arm2-store-immediate seg vcell src)))
                (if check-boundp
                  (! ref-symbol-value-inline target src)
                  (! %ref-symbol-value-inline target src))))
            (let* ((src ($ arm::arg_z))
                   (dest ($ arm::arg_z)))
              (arm2-store-immediate seg (arm2-symbol-value-cell sym) src)
              (if check-boundp
                (! ref-symbol-value dest src)
                (! %ref-symbol-value dest src))
              (<- dest)))))
    (^)))

#|
               (defun arm2-ref-symbol-value (seg vreg xfer sym check-boundp)  
(with-arm-local-vinsn-macros (seg vreg xfer)
(when vreg
(if (eq sym '*interrupt-level*)
(ensuring-node-target (target vreg)
(! ref-interrupt-level target))
(let* ((src ($ arm::arg_z))
(dest ($ arm::arg_z)))
(arm2-store-immediate seg (arm2-symbol-value-cell sym) src)
(if check-boundp
(! ref-symbol-value dest src)
(! %ref-symbol-value dest src))
(<- dest))))
(^)))
               ||#

;;; Should be less eager to box result
(defun arm2-extract-charcode (seg vreg xfer char safe)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((src (arm2-one-untargeted-reg-form seg char arm::arg_z)))
      (when safe
        (! trap-unless-character src))
      (if vreg
        (ensuring-node-target (target vreg)
          (! character->fixnum target src)))
      (^))))
  

(defun arm2-reference-list (seg vreg xfer listform safe refcdr)
  (if (arm2-form-typep listform 'list)
    (setq safe nil))     ; May also have been passed as NIL.
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((src (arm2-one-untargeted-reg-form seg listform arm::arg_z)))
      (when safe
        (! trap-unless-list src))
      (if vreg
        (ensuring-node-target (target vreg)
          (if refcdr
            (! %cdr target src)
            (! %car target src))))
      (^))))







(defun arm2-misc-byte-count (subtag element-count)
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


(defun arm2-allocate-initialized-gvector (seg vreg xfer subtag initforms)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if (null vreg)
      (dolist (f initforms) (arm2-form seg nil nil f))
      (let* ((*arm2-vstack* *arm2-vstack*)
             (arch (backend-target-arch *target-backend*))
             (n (length initforms))
             (nntriv (let* ((count 0)) 
                       (declare (fixnum count))
                       (dolist (f initforms count) 
                         (unless (arm-side-effect-free-form-p f)
                           (incf count)))))
             (header (arch::make-vheader n subtag)))
        (declare (fixnum n nntriv))
        (cond ( (or *arm2-open-code-inline* (> nntriv 3))
               (arm2-formlist seg initforms nil)
                (arm2-lri seg arm::imm0 header)
                (! %arm-gvector vreg arm::imm0 (ash n (arch::target-word-shift arch))))
              (t
               (let* ((pending ())
                      (vstack *arm2-vstack*))
                 (declare (fixnum vstack))
                 (dolist (form initforms)
                   (if (arm-side-effect-free-form-p form)
                     (push form pending)
                     (progn
                       (push nil pending)
                       (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg form arm::arg_z)))))
                 (arm2-lri seg arm::imm0 header)
                 (ensuring-node-target (target vreg)
                   (! %alloc-misc-fixed target arm::imm0 (ash n (arch::target-word-shift arch)))
                   (with-node-temps (target) (nodetemp)
                     (do* ((forms pending (cdr forms))
                           (index (1- n) (1- index))
                           (pushed-cell (+ vstack (the fixnum (ash nntriv (arch::target-word-shift arch))))))
                          ((null forms))
                       (declare (list forms) (fixnum pushed-cell))
                       (let* ((form (car forms))
                              (reg nodetemp))
                         (if form
                           (setq reg (arm2-one-untargeted-reg-form seg form nodetemp))
                           (progn
                             (decf pushed-cell *arm2-target-node-size*)
                             (arm2-stack-to-register seg (arm2-vloc-ea pushed-cell) nodetemp)))
                         (! misc-set-c-node reg target index)))))
                 (! vstack-discard nntriv))
               ))))
    (^)))

;;; Heap-allocated constants -might- need memoization: they might be newly-created,
;;; as in the case of synthesized toplevel functions in .pfsl files.
(defun arm2-acode-needs-memoization (valform)
  (if (arm2-form-typep valform 'fixnum)
    nil
    (let* ((val (acode-unwrapped-form-value valform)))
      (if (or (nx-t val)
              (nx-null val)
              (and (acode-p val)
                   (let* ((op (acode-operator val)))
                     (or (eq op (%nx1-operator fixnum)) #|(eq op (%nx1-operator immediate))|#))))
        nil
        t))))

(defun arm2-modify-cons (seg vreg xfer ptrform valform safe setcdr returnptr)
  (if (arm2-form-typep ptrform 'cons)
    (setq safe nil))                    ; May also have been passed as NIL.
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (ptr-vreg val-vreg) (arm2-two-targeted-reg-forms seg ptrform ($ arm::arg_y) valform ($ arm::arg_z))
      (when safe
        (! trap-unless-cons ptr-vreg))
      (if setcdr
        (! call-subprim-2 ($ arm::arg_z) (arm::arm-subprimitive-offset '.SPrplacd) ptr-vreg val-vreg)
        (! call-subprim-2 ($ arm::arg_z) (arm::arm-subprimitive-offset '.SPrplaca) ptr-vreg val-vreg))
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
(defun arm2-conditional-form (seg xfer form)
  (let* ((uwf (acode-unwrapped-form-value form)))
    (if (nx-null uwf)
      (arm2-branch seg (arm2-cd-false xfer) nil)
      (if (arm-constant-form-p uwf)
        (arm2-branch seg (arm2-cd-true xfer) nil)
        (with-crf-target () crf
          (arm2-form seg crf xfer form))))))

      
(defun arm2-branch (seg xfer crf &optional cr-bit true-p)
  (declare (notinline arm2-branch))
  (let* ((*arm2-vstack* *arm2-vstack*))
    (with-arm-local-vinsn-macros (seg)
      (setq xfer (or xfer 0))
      (when (logbitp $backend-mvpass-bit xfer) ;(arm2-mvpass-p cd)
        (setq xfer (logand (lognot $backend-mvpass-mask) xfer))
        (unless *arm2-returning-values*
          (arm2-vpush-register seg arm::arg_z)
          (arm2-set-nargs seg 1)))
      (if (neq 0 xfer)
        (if (eq xfer $backend-return);; xfer : RETURN ==> popj
          (arm2-do-return seg)
          (if (not (arm2-cd-compound-p xfer))
            (-> xfer);; xfer : label# ==> BRA label#
            ;; cd is compound : (<true> / <false>)
            (let* ((truebranch (arm2-cd-true xfer))
                   (falsebranch (arm2-cd-false xfer))
                   (tbranch (if true-p truebranch falsebranch))
                   (nbranch (if true-p falsebranch truebranch))
                   (tn0 (neq 0 tbranch))
                   (tnret (neq $backend-return tbranch))
                   (nn0 (neq 0 nbranch))
                   (nnret (neq $backend-return nbranch))
                   (tlabel (if (and tnret tn0) (aref *backend-labels* tbranch)))
                   (nlabel (if (and nnret nn0) (aref *backend-labels* nbranch))))
              (unless cr-bit (setq cr-bit arm::arm-cond-eq))
              (if (and tn0 tnret nn0 nnret)
                (progn
                  (! cbranch-true tlabel crf cr-bit );; (label# /  label#)
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
                    (arm2-do-return seg)
                    (@ aux-label)))))))))))

(defun arm2-cd-merge (cd label)
  (setq cd (or cd 0))
  (let ((mvpass (logbitp $backend-mvpass-bit cd)))
    (if (neq 0 (logand (lognot $backend-mvpass-mask) cd))
      (if (arm2-cd-compound-p cd)
        (arm2-make-compound-cd
         (arm2-cd-merge (arm2-cd-true cd) label)
         (arm2-cd-merge (arm2-cd-false cd) label)
         mvpass)
        cd)
      (if mvpass 
        (logior $backend-mvpass-mask label)
        label))))

(defun arm2-mvpass-p (xfer)
  (if xfer (or (logbitp $backend-mvpass-bit xfer) (eq xfer $backend-mvpass))))

(defun arm2-cd-compound-p (xfer)
  (if xfer (logbitp $backend-compound-branch-target-bit xfer)))

(defun arm2-cd-true (xfer)
  (if (arm2-cd-compound-p xfer)
    (ldb  $backend-compound-branch-true-byte xfer)
    xfer))

(defun arm2-cd-false (xfer)
  (if (arm2-cd-compound-p xfer)
    (ldb  $backend-compound-branch-false-byte xfer)
    xfer))

(defun arm2-make-compound-cd (tpart npart &optional mvpass-p)
  (dpb (or npart 0) $backend-compound-branch-false-byte
       (dpb (or tpart 0) $backend-compound-branch-true-byte
            (logior (if mvpass-p $backend-mvpass-mask 0) $backend-compound-branch-target-mask))))

(defun arm2-invert-cd (cd)
  (if (arm2-cd-compound-p cd)
    (arm2-make-compound-cd (arm2-cd-false cd) (arm2-cd-true cd) (logbitp $backend-mvpass-bit cd))
    cd))



;;; execute body, cleanup afterwards (if need to)
(defun arm2-undo-body (seg vreg xfer body old-stack)
  (let* ((current-stack (arm2-encode-stack))
         (numundo (%i- *arm2-undo-count* (arm2-encoding-undo-count old-stack))))
    (declare (fixnum numundo))
    (with-arm-local-vinsn-macros (seg vreg xfer)
      (if (arm2-equal-encodings-p  current-stack old-stack)
        (arm2-form seg vreg xfer body)
        (if (eq xfer $backend-return)
          (progn
            (arm2-form seg vreg xfer body)
            (dotimes (i numundo) (arm2-close-undo)))
          (if (arm2-mvpass-p xfer)
            (progn
              (arm2-mvpass seg body)    ; presumed to be ok
              (let* ((*arm2-returning-values* :pass))
                (arm2-nlexit seg xfer numundo)
                (^))
              (dotimes (i numundo) (arm2-close-undo)))
            (progn
              ;; There are some cases where storing thru arm::arg_z
              ;; can be avoided (stores to vlocs, specials, etc.) and
              ;; some other case where it can't ($test, $vpush.)  The
              ;; case of a null vd can certainly avoid it; the check
              ;; of numundo is to keep $acc boxed in case of nthrow.
              (arm2-form  seg (if (or vreg (not (%izerop numundo))) arm::arg_z) nil body)
              (arm2-unwind-set seg xfer old-stack)
              (when vreg (<- arm::arg_z))
              (^))))))))


(defun arm2-unwind-set (seg xfer encoding)
  (multiple-value-bind (target-catch target-cstack target-vstack)
      (arm2-decode-stack encoding)
    (arm2-unwind-stack seg xfer target-catch target-cstack target-vstack)
    (arm2-regmap-note-vstack-delta target-vstack *arm2-vstack*)
    (setq *arm2-undo-count* target-catch 
          *arm2-cstack* target-cstack
          *arm2-vstack* target-vstack)))

(defun arm2-unwind-stack (seg xfer target-catch target-cstack target-vstack)
  (let* ((current-catch *arm2-undo-count*)
         (current-cstack *arm2-cstack*)
         (current-vstack *arm2-vstack*)
         (diff (%i- current-catch target-catch))
         target
         (exit-vstack current-vstack))
    (declare (ignore-if-unused target))
    (when (neq 0 diff)
      (setq exit-vstack (arm2-nlexit seg xfer diff))
      (multiple-value-setq (target current-cstack current-vstack)
        (arm2-decode-stack (aref *arm2-undo-stack* target-catch))))
    (if (%i< 0 (setq diff (%i- current-cstack target-cstack)))
      (with-arm-local-vinsn-macros (seg)
        (! adjust-sp diff)))
    (if (%i< 0 (setq diff (%i- current-vstack target-vstack)))
      (with-arm-local-vinsn-macros (seg)
        (! vstack-discard (ash diff (- *arm2-target-fixnum-shift*)))))
    exit-vstack))

;;; We can sometimes combine unwinding the catch stack with returning from the function
;;; by jumping to a subprim that knows how to do this.  If catch frames were distinguished
;;; from unwind-protect frames, we might be able to do this even when saved registers
;;; are involved (but the subprims restore them from the last catch frame.)
;;; *** there are currently only subprims to handle the "1 frame" case; add more ***
(defun arm2-do-return (seg)
  (let* ((*arm2-vstack* *arm2-vstack*))
    (with-arm-local-vinsn-macros (seg)
      (progn
        (arm2-set-vstack (arm2-unwind-stack seg $backend-return 0 0 #x7fffff))
        (if *arm2-returning-values*
          (progn
            (arm2-restore-nvrs seg t)
            (arm2-restore-non-volatile-fprs seg)
            (! restore-nfp)                 
            (! nvalret))
          (progn
            (arm2-restore-nvrs seg nil)
            (arm2-restore-non-volatile-fprs seg)
            (! restore-nfp)            
            (! popj)))))
    nil))



(defun arm2-mvcall (seg vreg xfer fn arglist &optional recursive-p)
  (let* ((cstack *arm2-cstack*)
         (vstack *arm2-vstack*))
    (with-arm-local-vinsn-macros (seg vreg xfer)
      (if (and (eq xfer $backend-return) (not (arm2-tailcallok xfer)))
        (progn
          (arm2-mvcall seg vreg $backend-mvpass fn arglist t)
          (arm2-set-vstack (%i+ (if arglist *arm2-target-node-size* 0) vstack))
          (setq *arm2-cstack* cstack)
          (let* ((*arm2-returning-values* t)) (^)))
        (let* ((mv-p (arm2-mv-p xfer)))
          (if (null arglist)
            (arm2-call-fn seg vreg xfer fn arglist nil)
            (progn
              (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg fn arm::arg_z))
              (arm2-multiple-value-body seg (pop arglist))
              (when arglist
                (arm2-open-undo $undostkblk)
                (! save-values)
                (dolist (form arglist)
                  (arm2-multiple-value-body seg form)
                  (! add-values))
                (arm2-set-nargs seg 0)
                (! recover-values)
                (arm2-close-undo))
              (! lisp-word-ref arm::nfn arm::vsp arm::nargs)
              (arm2-invoke-fn seg arm::nfn nil nil xfer)))
          (unless recursive-p
            (if mv-p
              (unless (eq xfer $backend-return)
                (let* ((*arm2-returning-values* t))
                  (^)))
              (progn 
                (arm2-adjust-vstack (- *arm2-target-node-size*)) ; discard function
                (! vstack-discard 1)
                (<- arm::arg_z)
                (^)))))))))


(defun arm2-hard-opt-p (opts)
  (or
   (dolist (x (%cadr opts))
     (unless (nx-null x) (return t)))
   (dolist (x (%caddr opts))
     (when x (return t)))))

(defun arm2-close-lambda (seg req opt rest keys auxen)
  (dolist (var req)
    (arm2-close-var seg var))
  (dolist (var (%car opt))
    (arm2-close-var seg var))
  (dolist (var (%caddr opt))
    (when var
      (arm2-close-var seg var)))
  (if rest
    (arm2-close-var seg rest))
  (dolist (var (%cadr keys))
    (arm2-close-var seg var))
  (dolist (var (%caddr keys))
    (if var (arm2-close-var seg var)))
  (dolist (var (%car auxen))
    (arm2-close-var seg var)))




(defun arm2-init-regvar (seg var reg addr)
  (with-arm-local-vinsn-macros (seg)
    (arm2-stack-to-register seg addr reg)
    (arm2-set-var-ea seg var (make-wired-lreg reg :class (hard-regspec-class reg) :mode (get-regspec-mode reg)))))


(defun arm2-simple-var (var &aux (bits (cadr var)))
  (if (or (%ilogbitp $vbitclosed bits)
          (%ilogbitp $vbitspecial bits))
    (nx-error "Non-simple-variable ~S" (%car var))
    var))

(defun arm2-nlexit (seg xfer &optional (nlevels 0))
  (let* ((numnthrow 0)
         (n *arm2-undo-count*)
         (cstack *arm2-cstack*)
         (vstack *arm2-vstack*)
         (target-cstack)
         (target-vstack)
         (lastcatch n)
         (i nil)
         (returning (eq xfer $backend-return))
         (junk1 nil)
         (unbind ())
         (dest (%i- n nlevels))
         (retval *arm2-returning-values*)
         reason)
    (declare (ignorable junk1))
    (with-arm-local-vinsn-macros (seg)
      (when (neq 0 nlevels)
        (let* ((numnlispareas 0))
          (declare (fixnum numnlispareas))
          (flet ((popnlispareas ()
                   (dotimes (i numnlispareas)
                     (! discard-temp-frame)))
                 (throw-through-numnthrow-catch-frames ()
                   (when (neq 0 numnthrow)
                     (arm2-lri seg arm::imm0 (ash numnthrow *arm2-target-fixnum-shift*))
                     (if retval
                       (! nthrowvalues)
                       (! nthrow1value))
                     (setq numnthrow 0)
                     (multiple-value-setq (junk1 cstack vstack)
                       (arm2-decode-stack (aref *arm2-undo-stack* lastcatch))))))
            (while (%i> n dest)
              (cond ((eql $undocatch (setq reason (aref *arm2-undo-because* (setq n (%i- n 1)))))
                     (popnlispareas)
                     (setq numnthrow (%i+ numnthrow 1) lastcatch n))
                    ((eql $undostkblk reason)
                     (throw-through-numnthrow-catch-frames)
                     (incf numnlispareas))
                    ((eql $undo-arm-c-frame reason)
                     (! discard-c-frame))))
            (throw-through-numnthrow-catch-frames)
            (setq i lastcatch)
            (while (%i> i dest)
              (let ((reason (aref *arm2-undo-because* (setq i (%i- i 1)))))
                (if (or (eql reason $undospecial)
                        (eql reason $undointerruptlevel))
                  (push reason unbind))))
            (if unbind
              (arm2-dpayback-list seg (nreverse unbind)))
            (when (and (neq lastcatch dest)
                       (%i>
                        vstack
                        (setq target-vstack 
                              (nth-value 2 (arm2-decode-stack (aref *arm2-undo-stack* dest)))))
                       (neq retval t))
              (unless returning
                (let ((vdiff (%i- vstack target-vstack)))
                  (if retval
                    (progn
                      (arm2-lri seg arm::imm0 vdiff)
                      (! slide-values))
                    (! adjust-vsp vdiff)))))
            (setq numnlispareas 0)
            (while (%i> lastcatch dest)
              (let ((reason (aref *arm2-undo-because* (setq lastcatch (%i- lastcatch 1)))))
                (setq target-cstack (nth-value 1
                                               (arm2-decode-stack (aref *arm2-undo-stack* lastcatch))))
                (if (eq reason $undostkblk)
                  (incf numnlispareas))
                (if (%i> cstack target-cstack)
                  (with-arm-local-vinsn-macros (seg)
                    (! adjust-sp (%i- cstack target-cstack))))
                                        ; else what's going on? $sp-stkcons, for one thing
                (setq cstack target-cstack)))
            (popnlispareas)))
        vstack))))


;;; Restore the most recent dynamic bindings.  Bindings
;;; of *INTERRUPT-LEVEL* get special treatment.
(defun arm2-dpayback-list (seg reasons)
  (with-arm-local-vinsn-macros (seg)
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
              (if *arm2-open-code-inline*
                (! unbind-interrupt-level-inline)
                (! unbind-interrupt-level)))
            (compiler-bug "unknown payback token ~s" r)))))))




(defun arm2-tailcallok (xfer)
  (and (eq xfer $backend-return)
       *arm2-tail-allow*
       (eq 0 *arm2-undo-count*)))

(defun arm2-mv-p (cd)
  (or (eq cd $backend-return) (arm2-mvpass-p cd)))




(defun arm2-expand-vinsns (header current &optional sections)
  (declare (ignorable sections))
  (do-dll-nodes (v header)
    (if (%vinsn-label-p v)
      (let* ((id (vinsn-label-id v)))
        (if (or (typep id 'fixnum) (null id))
          (when (or t (vinsn-label-refs v) (null id))
            (setf (vinsn-label-info v) (arm::emit-lap-label current v)))))
      (arm2-expand-vinsn v current sections)))
  ;;; This doesn't have too much to do with anything else that's
  ;;; going on here, but it needs to happen before the lregs
  ;;; are freed.  There really shouldn't be such a thing as a
  ;;; var-ea, of course ...
  (dolist (s *arm2-recorded-symbols*)
    (let* ((var (car s))
           (ea (var-ea var)))
      (when (typep ea 'lreg)
        (setf (var-ea var) (lreg-value ea)))))
)

;;; It's not clear whether or not predicates, etc. want to look
;;; at an lreg or just at its value slot.
;;; It's clear that the assembler just wants the value, and that
;;; the value had better be assigned by the time we start generating
;;; machine code.
;;; For now, we replace lregs in the operand vector with their values
;;; on entry, but it might be reasonable to make PARSE-OPERAND-FORM
;;; deal with lregs ...
(defun arm2-expand-vinsn (vinsn current &optional sections)
  (declare (ignorable sections))
  (let* ((template (vinsn-template vinsn))
         (code (svref sections 0))
         (data (svref sections 1))
         (vp (vinsn-variable-parts vinsn))
         (nvp (vinsn-template-nvp template))
         (predicate (getf (vinsn-annotation vinsn) :predicate))
         (unique-labels ())
         (notes (vinsn-notes vinsn))
         (operand-insert-functions arm::*arm-vinsn-insert-functions*))
    (declare (fixnum nvp))
    (dotimes (i nvp)
      (let* ((val (svref vp i)))
        (when (typep val 'lreg)
          (setf (svref vp i) (lreg-value val)))))                       
    (dolist (name (vinsn-template-local-labels template))
      (let* ((unique (cons name nil)))
        (push unique unique-labels)
        (arm::make-lap-label unique)))
    (labels ((parse-operand-form (valform)
               ;;(break "valform = ~s" valform)
               (cond ((typep valform 'keyword)
                      (or (assq valform unique-labels)
                          (compiler-bug "unknown vinsn label ~s" valform)))
                     ((atom valform) valform)
                     ((and (atom (cdr valform))
                           (typep (car valform) 'fixnum))
                      (svref vp (car valform)))
                     (t (let* ((op-vals (cdr valform))
                               (parsed-ops (make-list (length op-vals)))
                               (tail parsed-ops))
                          (declare (dynamic-extent parsed-ops)
                                   (cons parsed-ops tail))
                          (dolist (op op-vals (apply (car valform) parsed-ops))
                            (setq tail (cdr (rplaca tail (parse-operand-form op)))))))))
             (expand-insn-form (f)
               (case (car f)
                 (:code (setq current code))
                 (:data (setq current data)
                        (when (logtest 7 (arm::section-size data))
                          (expand-insn-form '((0 . 0)))))
                 (:word (let* ((val (parse-operand-form (cadr f))))
                          (expand-insn-form (list (cons (ldb (byte 16 16) val)
                                                        (ldb (byte 16 0) val))))))
                 (:lock-constant-pool (setq *arm2-autodrain-constant-pool* nil))
                 (:unlock-constant-pool (setq *arm2-autodrain-constant-pool* t))
                 (t
                  
                  (let* ((insn (arm::make-lap-instruction nil))
                         (opcode (car f))
                         (operands (cdr f)))
                    (setf (arm::lap-instruction-opcode-high insn) (car opcode)
                          (arm::lap-instruction-opcode-low insn) (cdr opcode))
                    (when (and predicate (eq current code))
                      (funcall (svref operand-insert-functions
                                      (arm::encode-vinsn-field-type :cond))
                               insn
                               predicate))
                    (dolist (op operands (arm::emit-lap-instruction-element insn current))
                      (let* ((insert-function (svref operand-insert-functions (car op))))
                        (funcall insert-function insn (parse-operand-form (cdr op)))))
                    ;; If we just emitted an unconditional control transfer
                    ;; and we have data in the constant pool, drain the pool.
                    (when (and (eql current code)
                               (not (eq (dll-header-succ data) data))
                               *arm2-autodrain-constant-pool*)
                      (let* ((high (arm::lap-instruction-opcode-high insn)))
                        (declare (type (unsigned-byte 16) high))
                        (when (>= high #xe000)
                          (let* ((low (arm::lap-instruction-opcode-low insn)))
                            (declare (type (unsigned-byte 16) low))
                            (when (or (eql #x0a00 (logand high #x0f00)) ;b
                                      (and (eql #x012f (logand high #x0fff))
                                           (eql #xff10 (logand low #xfff0))) ;bx
                                      (and (eql #x0890 (logand high #x0fd0))
                                           (logbitp 15 low)) ;ldm w/PC
                                      (and (eql #x0590 (logand #x0ff0 high))
                                           (eql #xf000 (logand #xf000 low)))) ;ldr pc
                              (arm-drain-constant-pool code data))))))))))
             (eval-predicate (f)
               (case (car f)
                 (:pred (let* ((op-vals (cddr f))
                               (parsed-ops (make-list (length op-vals)))
                               (tail parsed-ops))
                          (declare (dynamic-extent parsed-ops)
                                   (cons parsed-ops tail))
                          (dolist (op op-vals (apply (cadr f) parsed-ops))
                            (setq tail (cdr (rplaca tail (parse-operand-form op)))))))
                 (:not (not (eval-predicate (cadr f))))
                 (:or (dolist (pred (cadr f))
                        (when (eval-predicate pred)
                          (return t))))
                 (:and (dolist (pred (cadr f) t)
                         (unless (eval-predicate pred)
                           (return nil))))
                 (t (compiler-bug "Unknown predicate: ~s" f))))
             (expand-form (f)
               (if (keywordp f)
                 (arm::emit-lap-label current (assq f unique-labels))
                 (if (atom f)
                   (compiler-bug "Invalid form in vinsn body: ~s" f)
                   (if (or (atom (car f))
                           (typep (caar f) 'fixnum))
                     (expand-insn-form f)
                     (if (eval-predicate (car f))
                       (dolist (subform (cdr f))
                         (expand-form subform))))))))
      (declare (dynamic-extent #'expand-form #'parse-operand-form #'expand-insn-form #'eval-predicate))
                                        ;(format t "~& vinsn = ~s" vinsn)
      (when notes
        (let* ((lab ()))
          (dolist (note notes)
            (unless (eq :close (vinsn-note-class note))
              (when (eq :source-location-begin
                        (vinsn-note-class note))
                (push note *arm2-emitted-source-notes*))
              (when (null lab)
                (setq lab (arm::make-lap-label note))
                (arm::emit-lap-label current note))
              (setf (vinsn-note-address note) lab)))))
      (dolist (form (vinsn-template-body template))
        (expand-form form ))
      (when notes
        (let* ((lab ()))
          (dolist (note notes)
            (when (eq :close (vinsn-note-class note))
              (when (null lab)
                (setq lab (arm::make-lap-label note))
                (arm::emit-lap-label current note))
              (setf (vinsn-note-address note) lab)))))
      (setf (vinsn-variable-parts vinsn) nil)
      (when vp
        (free-varparts-vector vp))))
  current)





(defun arm2-builtin-index-subprim (idx)
  (let* ((arch (backend-target-arch *target-backend*))
         (table (arch::target-primitive->subprims  arch))
         (shift (arch::target-subprims-shift arch)))
    (dolist (cell table)
      (destructuring-bind ((low . high) . base) cell
        (if (and (>= idx low)
                 (< idx high))
          (return (+ base (ash (- idx low) shift))))))))

(defun arm2-fixed-call-builtin (seg vreg xfer name)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((index (arch::builtin-function-name-offset name))
           (subprim (if index
                      (arm2-builtin-index-subprim index)
                      (or (arm::arm-subprimitive-offset name)
                          (compiler-bug "Unknown builtin subprim index for ~s" name))))
           (tail-p (arm2-tailcallok xfer)))
      (when tail-p
        (arm2-restore-nvrs seg nil)
        (arm2-restore-non-volatile-fprs seg)
        (! restore-nfp)
        (arm2-restore-full-lisp-context seg))
      (if tail-p
        (! jump-subprim subprim)
        (progn
          (! call-subprim subprim)
          (<- ($ arm::arg_z))
          (^))))))

(defun arm2-unary-builtin (seg vreg xfer name form)
  (with-arm-local-vinsn-macros (seg)
    (arm2-one-targeted-reg-form seg form ($ arm::arg_z))
    (arm2-fixed-call-builtin seg vreg xfer name)))

(defun arm2-binary-builtin (seg vreg xfer name form1 form2)
  (with-arm-local-vinsn-macros (seg)
    (arm2-two-targeted-reg-forms seg form1 ($ arm::arg_y) form2 ($ arm::arg_z))
    (arm2-fixed-call-builtin seg vreg xfer name)))

(defun arm2-ternary-builtin (seg vreg xfer name form1 form2 form3)
  (with-arm-local-vinsn-macros (seg)
    (arm2-three-targeted-reg-forms seg form1 ($ arm::arg_x) form2 ($ arm::arg_y) form3 ($ arm::arg_z))
    (arm2-fixed-call-builtin seg vreg xfer name)))


(eval-when (:compile-toplevel :execute :load-toplevel)


  (defmacro defarm2 (name locative arglist &body forms)
    (multiple-value-bind (body decls)
        (parse-body forms nil t)
      (destructuring-bind (vcode-block dest control &rest other-args) arglist
        (let* ((fun `(nfunction ,name 
                      (lambda (,vcode-block ,dest ,control ,@other-args) ,@decls 
                              (block ,name (with-arm-local-vinsn-macros (,vcode-block ,dest ,control) ,@body))))))
          `(progn
            (record-source-file ',name 'function)
            (svset *arm2-specials* (%ilogand #.operator-id-mask (%nx1-operator ,locative)) ,fun))))))
  )

(defun arm2-allocate-global-fprs (varsets)
  (do* ((done nil)
        (mask (1- (ash 1 16)))
        (last-allocated-double -1)
        (varsets varsets (cdr varsets))
        (varset (caar varsets) (caar varsets)))
       ((or done (null varsets)) (1+ last-allocated-double))
    (let* ((need-double
            (dolist (var varset)
              (when (eq (var-declared-type var) 'double-float)
                (return t)))))
      (if need-double
        (do* ((i 0 (1+ i))
              (regmask (target-fpr-mask i hard-reg-class-fpr-mode-double)
                       (target-fpr-mask i hard-reg-class-fpr-mode-double)))
             ((> regmask mask) (setq done t))
          (when (eql regmask (logand mask regmask))
            (setq mask (logandc2 mask regmask))
            (let* ((double (make-hard-fp-reg (+ (hard-regspec-value arm::d8) i) hard-reg-class-fpr-mode-double))
                   (single (make-hard-fp-reg (+ (hard-regspec-value arm::s16) (ash i -1)) hard-reg-class-fpr-mode-single)))
              (dolist (var varset)
                (if (eq 'double-float (var-declared-type var))
                  (setf (var-nvr var) double)
                  (setf (var-nvr var) single)))
              (setq last-allocated-double i)
              (return))))
        (do* ((i 0 (1+ i))
              (regmask (target-fpr-mask i hard-reg-class-fpr-mode-single)
                       (target-fpr-mask i hard-reg-class-fpr-mode-single)))
             ((> regmask mask) (setq done t))
          (when (eql regmask (logand mask regmask))
            (setq mask (logandc2 mask regmask))
            (let* ((single (make-hard-fp-reg (+ (hard-regspec-value arm::s16) i) hard-reg-class-fpr-mode-single))
                   )
              (dolist (var varset)
                (setf (var-nvr var) single))
              (let* ((idx (ash (1+ i) -1)))
                (when (> idx last-allocated-double)
                  (setq last-allocated-double idx)))
              (return))))))))
        

(defarm2 arm2-lambda lambda-list (seg vreg xfer req opt rest keys auxen body p2decls &optional code-note)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((stack-consed-rest nil)
           (lexprp (if (consp rest) (progn (setq rest (car rest)) t)))
           (rest-var-bits (and rest (nx-var-bits rest)))
           (rest-ignored-p (and rest (not lexprp) (%ilogbitp $vbitignore rest-var-bits)))
           (want-stack-consed-rest (or rest-ignored-p
                                       (and rest (not lexprp) (%ilogbitp $vbitdynamicextent rest-var-bits))))
           (afunc *arm2-cur-afunc*)
           (inherited-vars (afunc-inherited-vars afunc))
           (fbits (afunc-bits afunc))
           (methodp (%ilogbitp $fbitmethodp fbits))
           (method-var (if methodp (pop req)))
           (next-method-p (%ilogbitp $fbitnextmethp fbits))
           (allow-other-keys-p (%car keys))
           (hardopt (arm2-hard-opt-p opt))
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
           (*arm2-vstack* 0)
           (*arm2-nfp-depth* *arm2-nfp-depth*)
           (*arm2-nfp-vars* *arm2-nfp-vars*))
      (declare (type (unsigned-byte 16) num-req num-opt num-inh))
      (with-arm-p2-declarations p2decls
        (setq *arm2-inhibit-register-allocation*
              (setq no-regs (%ilogbitp $fbitnoregs fbits)))
        (multiple-value-setq (pregs reglocatives) 
         
          (nx2-afunc-allocate-global-registers afunc (unless no-regs *arm2-nvrs*)))
        (@ (backend-get-next-label))    ; generic self-reference label, should be label #1
        (when keys;; Ensure keyvect is the first immediate
          (backend-immediate-index (%cadr (%cdddr keys))))
        (when code-note
          (arm2-code-coverage-entry seg code-note))
        (unless next-method-p
          (setq method-var nil))
        
        (let* ((rev-req (reverse req))
               (rev-fixed (if inherited-vars (reverse (append inherited-vars req)) rev-req))
               (num-fixed (length rev-fixed))
               (rev-opt (reverse (car opt))))
          (if (not (or opt rest keys))
            (progn
              (setq arg-regs (arm2-req-nargs-entry seg rev-fixed)))
            (if (and (not (or hardopt rest keys))
                     (<= num-opt $numarmargregs))
              (setq arg-regs (arm2-simple-opt-entry seg rev-opt rev-fixed))
              (progn
                ;; If the minumum acceptable number of args is
                ;; non-zero, ensure that at least that many were
                ;; received.  If there's an upper bound, enforce it.
                
                (when rev-fixed
                  (if (arm::encode-arm-immediate num-fixed)
                    (! check-min-nargs num-fixed)
                    (! check-min-nargs-large num-fixed)))
                (unless (or rest keys)
                  (let* ((max (+ num-fixed num-opt)))
                    (if (arm::encode-arm-immediate max)
                      (! check-max-nargs max)
                      (! check-max-nargs-large max))))
                (unless lexprp
                  (! save-lisp-context-variable))
                ;; If there were &optional args, initialize their values
                ;; to NIL.  All of the argregs get vpushed as a result of this.
                (when opt
                  (! default-optionals (+ num-fixed num-opt)))
                (when keys
                  (unless opt
                    (! vpush-argregs num-fixed))
                  (let* ((keyvect (%car (%cdr (%cdr (%cdr (%cdr keys))))))
                         (flags (the fixnum (logior (the fixnum (if rest 4 0)) 
                                                    (the fixnum (if (or methodp allow-other-keys-p) 1 0)))))
                         (nprev (+ num-fixed num-opt)))
                    (declare (fixnum flags nprev))

                    (backend-immediate-index keyvect)
                    (arm2-lri seg arm::arg_y (ash flags *arm2-target-fixnum-shift*))
                    (arm2-lri seg arm::imm0 (ash nprev *arm2-target-fixnum-shift*))
                    (! keyword-bind)))
                (when rest
                  ;; If any keyword-binding's happened, the key/value
                  ;; pairs have been slid to the top-of-stack for us.
                  ;; There'll be an even number of them (nargs - the
                  ;; "previous" (required/&optional) count.)
                  (if lexprp
                    (arm2-lexpr-entry seg num-fixed)
                    (progn
                      (if want-stack-consed-rest
                        (setq stack-consed-rest t))
                      (let* ((nprev (+ num-fixed num-opt))
                             (simple (and (not keys) (= 0 nprev))))
                        (declare (fixnum nprev))
                        (unless simple
                          (arm2-lri seg arm::imm0 (ash nprev *arm2-target-fixnum-shift*)))
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
                  (arm2-lri seg arm::imm0 (ash num-opt *arm2-target-fixnum-shift*))

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
                       (nbytes-vpushed (* nwords-vpushed *arm2-target-node-size*)))
                  (declare (fixnum nwords-vpushed nbytes-vpushed))

                  (arm2-set-vstack nbytes-vpushed)
                  (setq optsupvloc (- *arm2-vstack* (* num-opt *arm2-target-node-size*)))))))
          ;; Caller's context is saved; *arm2-vstack* is valid.  Might still have method-var
          ;; to worry about.
          (! save-nfp)

          (arm2-save-non-volatile-fprs seg nsaved-fprs)
          (unless (= 0 pregs)
            ;; Save NVRs; load constants into any that get constants.
            (arm2-save-nvrs seg pregs)
            

            (dolist (pair reglocatives)
              (declare (cons pair))
              (let* ((constant (car pair))
                     (reg (cdr pair))
                     (temp ($ arm::temp2)))
                (declare (cons constant))
                (rplacd constant reg)
                (! ref-constant temp (backend-immediate-index (car constant)))
                (arm2-copy-register seg reg temp))))
          (when method-var
            (arm2-seq-bind-var seg method-var arm::next-method-context))
          ;; If any arguments are still in arg_x, arg_y, arg_z, that's
          ;; because they weren't vpushed in a "simple" entry case and
          ;; belong in some NVR.  Put them in their NVRs, so that we
          ;; can handle arbitrary expression evaluation (special
          ;; binding, value-cell consing, etc.) without clobbering the
          ;; argument registers.
          (when arg-regs
            (do* ((vars arg-regs (cdr vars))
                  (arg-reg-num arm::arg_z (1+ arg-reg-num)))
                 ((null vars))
              (declare (list vars) (fixnum arg-reg-num))
              (let* ((var (car vars)))
                (when var
                  (let* ((reg (nx2-assign-register-var var)))
                    (arm2-copy-register seg reg arg-reg-num)
                    (setf (var-ea var) reg))))))
          (setq *arm2-entry-vsp-saved-p* t)
          #|                            ;
          (when stack-consed-rest
          (if rest-ignored-p
          (if nil (arm2-jsrA5 $sp-popnlisparea))
          (progn
          (arm2-open-undo $undostkblk))))
          |#
          (when stack-consed-rest
            (arm2-open-undo $undostkblk))
          (setq *arm2-entry-vstack* *arm2-vstack*)
          (arm2-bind-lambda seg  req opt rest keys auxen optsupvloc arg-regs lexprp inherited-vars))
        (when method-var (arm2-heap-cons-next-method-var seg method-var))
        (arm2-form seg vreg xfer body)
        (arm2-close-lambda seg req opt rest keys auxen)
        (dolist (v inherited-vars)
          (arm2-close-var seg v))
        (when method-var
          (arm2-close-var seg method-var))
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


(defarm2 arm2-progn progn (seg vreg xfer forms)
  (declare (list forms))
  (if (null forms)
    (arm2-nil seg vreg xfer)
    (loop
      (let* ((form (pop forms)))
        (if forms
          (arm2-form seg nil nil form)
          (return (arm2-form seg vreg xfer form)))))))



(defarm2 arm2-prog1 prog1 (seg vreg xfer forms)
  (if (eq (list-length forms) 1)
    (arm2-use-operator (%nx1-operator values) seg vreg xfer forms)
    (if (null vreg)
      (arm2-use-operator (%nx1-operator progn) seg vreg xfer forms)
      (let* ((*arm2-nfp-depth* *arm2-nfp-depth*)
             (float-p (= (hard-regspec-class vreg) hard-reg-class-fpr))
             (crf-p (= (hard-regspec-class vreg) hard-reg-class-crf))
             (node-p (unless (or float-p crf-p)
                       (= (get-regspec-mode vreg) hard-reg-class-gpr-mode-node)))
             (first (pop forms)))
        (arm2-push-register seg 
                            (if (or node-p crf-p)
                              (arm2-one-untargeted-reg-form seg first arm::arg_z)
                              (arm2-one-targeted-reg-form seg first vreg)))
        (dolist (form forms)
          (arm2-form seg nil nil form))
        (if crf-p
          (progn
            (arm2-vpop-register seg arm::arg_z)
            (<- arm::arg_z))
          (arm2-pop-register seg vreg))
        (^)))))

(defarm2 arm2-free-reference free-reference (seg vreg xfer sym)
  (arm2-ref-symbol-value seg vreg xfer sym t))

(defarm2 arm2-special-ref special-ref (seg vreg xfer sym)
  (arm2-ref-symbol-value seg vreg xfer sym t))

(defarm2 arm2-bound-special-ref bound-special-ref (seg vreg xfer sym)
  (arm2-ref-symbol-value seg vreg xfer sym nil))

(defarm2 arm2-%slot-ref %slot-ref (seg vreg xfer instance idx)
  (ensuring-node-target (target (or vreg ($ arm::arg_z)))
    (multiple-value-bind (v i)
        (arm2-two-untargeted-reg-forms seg instance arm::arg_y idx arm::arg_z)
      (unless *arm2-reckless*
        (! check-misc-bound i v))
      (with-node-temps (v i) (temp)
        (! %slot-ref temp v i)
        (arm2-copy-register seg target temp))))
  (^))
  
(defarm2 arm2-%svref %svref (seg vreg xfer vector index)
  (arm2-vref seg vreg xfer :simple-vector vector index nil))

(defarm2 arm2-svref svref (seg vreg xfer vector index)
  (arm2-vref seg vreg xfer :simple-vector  vector index (unless *arm2-reckless* (nx-lookup-target-uvector-subtag :simple-vector))))

;;; It'd be nice if this didn't box the result.  Worse things happen ...
;;;  Once there's a robust mechanism, adding a CHARCODE storage class shouldn't be hard.
(defarm2 arm2-%sbchar %sbchar (seg vreg xfer string index)
  (arm2-vref seg vreg xfer :simple-string string index (unless *arm2-reckless* (nx-lookup-target-uvector-subtag :simple-string))))


(defarm2 arm2-%svset %svset (seg vreg xfer vector index value)
  (arm2-vset seg vreg xfer :simple-vector vector index value nil))

(defarm2 arm2-svset svset (seg vreg xfer vector index value)
  (arm2-vset seg vreg xfer :simple-vector  vector index value (nx-lookup-target-uvector-subtag :simple-vector)))

(defarm2 arm2-typed-form typed-form (seg vreg xfer typespec form &optional check)
  (if check
    (arm2-typechecked-form seg vreg xfer typespec form)
    (arm2-form seg vreg xfer form)))

(defarm2 arm2-type-asserted-form type-asserted-form (seg vreg xfer typespec form &optional check)
  (declare (ignore typespec check))
  (arm2-form seg vreg xfer form))




(defarm2 arm2-consp consp (seg vreg xfer cc form)
  (if (null vreg)
    (arm2-form seg vreg xfer form)
    (let* ((tagreg arm::imm0))
      (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
        (! extract-fulltag tagreg (arm2-one-untargeted-reg-form seg form arm::arg_z))
        (arm2-test-reg-%izerop seg vreg xfer tagreg cr-bit true-p
                               arm::fulltag-cons)))))
      
(defarm2 arm2-cons cons (seg vreg xfer y z)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil y)
      (arm2-form seg nil xfer z))
    (multiple-value-bind (yreg zreg) (arm2-two-untargeted-reg-forms seg y arm::arg_y z arm::arg_z)
      (ensuring-node-target (target vreg)
        (! cons target yreg zreg))
      (^))))



(defarm2 arm2-%rplaca %rplaca (seg vreg xfer ptr val)
  (arm2-modify-cons seg vreg xfer ptr val nil nil t))

(defarm2 arm2-%rplacd %rplacd (seg vreg xfer ptr val)
  (arm2-modify-cons seg vreg xfer ptr val nil t t))

(defarm2 arm2-rplaca rplaca (seg vreg xfer ptr val)
  (arm2-modify-cons seg vreg xfer ptr val t nil t))

(defarm2 arm2-set-car set-car (seg vreg xfer ptr val)
  (arm2-modify-cons seg vreg xfer ptr val t nil nil))

(defarm2 arm2-rplacd rplacd (seg vreg xfer ptr val)
  (arm2-modify-cons seg vreg xfer ptr val t t t))

(defarm2 arm2-set-cdr set-cdr (seg vreg xfer ptr val)
  (arm2-modify-cons seg vreg xfer ptr val t t nil))

(defarm2 arm2-%car %car (seg vreg xfer form)
  (arm2-reference-list seg vreg xfer form nil nil))

(defarm2 arm2-%cdr %cdr (seg vreg xfer form)
  (arm2-reference-list seg vreg xfer form nil t))

(defarm2 arm2-car car (seg vreg xfer form)
  (arm2-reference-list seg vreg xfer form t nil))

(defarm2 arm2-cdr cdr (seg vreg xfer form)
  (arm2-reference-list seg vreg xfer form t t))


(defarm2 arm2-vector vector (seg vreg xfer arglist)
  (arm2-allocate-initialized-gvector seg vreg xfer
                                     (nx-lookup-target-uvector-subtag
                                      :simple-vector) arglist))

(defarm2 arm2-%gvector %gvector (seg vreg xfer arglist)
  (let* ((all-on-stack (append (car arglist) (reverse (cadr arglist))))
         (subtag-form (car all-on-stack))
         (subtag (acode-fixnum-form-p subtag-form)))
    (if (null vreg)
      (dolist (form all-on-stack (^)) (arm2-form seg nil nil form))
      (if (null subtag)
        (progn                            ; Vpush everything and call subprim
          (let* ((*arm2-vstack* *arm2-vstack*))
            (arm2-set-nargs seg (arm2-formlist seg all-on-stack nil))
            (! gvector))
          (<- arm::arg_z)
          (^))
        (arm2-allocate-initialized-gvector seg vreg xfer subtag (cdr all-on-stack))))))

;;; Should be less eager to box result
(defarm2 arm2-%char-code %char-code (seg vreg xfer c)
  (arm2-extract-charcode seg vreg xfer c nil))

(defarm2 arm2-char-code char-code (seg vreg xfer c)
  (arm2-extract-charcode seg vreg xfer c (not (arm2-form-typep c 'character))))

(defarm2 arm2-%ilognot %ilognot (seg vreg xfer form)
  (ensuring-node-target (target vreg)
    (! %ilognot target (arm2-one-untargeted-reg-form seg form target)))
  (^))


(defarm2 arm2-%ilogior2 %ilogior2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer (logior fix1 fix2))
      (let* ((fixval (or fix1 fix2))
           (unboxed-fixval (if fixval (ash fixval *arm2-target-fixnum-shift*)))
           (ok-imm (and unboxed-fixval
                        (arm::encode-arm-immediate unboxed-fixval)))
           (otherform (if ok-imm (if fix1 form2 form1))))
      (if otherform
        (let* ((other-reg (arm2-one-untargeted-reg-form seg otherform arm::arg_z)))
          (when vreg
            (ensuring-node-target (target vreg)
              (! logior-immediate target other-reg (logand #xffffffff unboxed-fixval)))))
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg form1 arm::arg_y form2 arm::arg_z)
          (if vreg (ensuring-node-target (target vreg) (! %logior2 target r1 r2)))))   
      (^)))))


(defarm2 arm2-%ilogand2 %ilogand2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer (logand fix1 fix2))
      (let* ((fixval (or fix1 fix2))
             (unboxed-fixval (if fixval (ash fixval arm::fixnum-shift)))
             (ok-imm (and unboxed-fixval
                          (or (arm::encode-arm-immediate unboxed-fixval)
                              (arm::encode-arm-immediate
                               (logand #xffffffff (lognot unboxed-fixval))))))
                                                                 
             (otherform (if ok-imm (if fix1 form2 form1))))
        (if otherform
          (let* ((other-reg (arm2-one-untargeted-reg-form seg otherform arm::arg_z)))
            (when vreg
              (ensuring-node-target (target vreg)
                (! logand-immediate target other-reg (logand #xffffffff unboxed-fixval)))))
            (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg form1 arm::arg_y form2 arm::arg_z)
              (if vreg (ensuring-node-target (target vreg) (! %logand2 target r1 r2)))))
        (^)))))

(defarm2 arm2-%ilogxor2 %ilogxor2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer (logxor fix1 fix2))
    (let* ((fixval (or fix1 fix2))
           (unboxed-fixval (if fixval (ash fixval *arm2-target-fixnum-shift*)))
           (ok-imm (if unboxed-fixval (arm::encode-arm-immediate unboxed-fixval)))
           (otherform (if ok-imm (if fix1 form2 form1))))
      (if otherform
        (let* ((other-reg (arm2-one-untargeted-reg-form seg otherform arm::arg_z)))
          (when vreg
            (ensuring-node-target (target vreg)
              (! logxor-immediate target other-reg (logand unboxed-fixval #xffffffff)))))
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg form1 arm::arg_y form2 arm::arg_z)
          (if vreg (ensuring-node-target (target vreg) (! %logxor2 target r1 r2)))))
      (^)))))

(defarm2 arm2-%ineg %ineg (seg vreg xfer n)
  (let* ((src (arm2-one-untargeted-reg-form seg n arm::arg_z)))
    (when vreg
      (with-crf-target () crf
        (ensuring-node-target (target vreg)
          (! negate-fixnum-set-flags target crf src)
          (arm2-check-fixnum-overflow seg crf target))))
    (^)))

(defarm2 arm2-%%ineg %%ineg (seg vreg xfer n)
  (let* ((src (arm2-one-untargeted-reg-form seg n arm::arg_z)))
    (when vreg
      (ensuring-node-target (target vreg) 
        (! negate-fixnum-no-ovf target src)))
    (^)))

(defarm2 arm2-characterp characterp (seg vreg xfer cc form)
  (arm2-char-p seg vreg xfer cc form))

(defarm2 arm2-struct-ref struct-ref (seg vreg xfer struct offset)
  ;; Assume that typechecking for a specific structure type has already
  ;; occurred, unless we're generating unsafe code.  If we're not, we
  ;; still want to bounds-check, since the typecheck doesn't really
  ;; give us a whole lot of safety: we may be dealing with an instance
  ;; of a different version of the structure type than we expect.
  (arm2-vref seg vreg xfer :struct struct offset (unless *arm2-reckless* t)))

(defarm2 arm2-struct-set struct-set (seg vreg xfer struct offset value)
  (arm2-vset seg vreg xfer :struct  struct offset value (unless *arm2-reckless* (nx-lookup-target-uvector-subtag :struct))))

(defarm2 arm2-istruct-typep istruct-typep (seg vreg xfer cc form type)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
    (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg form arm::arg_y type arm::arg_z)
      (! istruct-type r1 r1)
      (arm2-compare-registers seg vreg xfer r1 r2 cr-bit true-p))))

(pushnew (%nx1-operator lisptag) *arm2-operator-supports-u8-target*)
(defarm2 arm2-lisptag lisptag (seg vreg xfer node)
  (if (null vreg)
    (arm2-form seg vreg xfer node)
    (let* ((reg (arm2-one-untargeted-reg-form seg node arm::arg_z)))
      (unboxed-other-case (vreg :u8)
        (! extract-tag vreg reg)
        (ensuring-node-target (target vreg) 
          (! extract-tag-fixnum target reg)))
      (^))))

(pushnew (%nx1-operator fulltag) *arm2-operator-supports-u8-target*)
(defarm2 arm2-fulltag fulltag (seg vreg xfer node)
  (if (null vreg)
    (arm2-form seg vreg xfer node)
    (let* ((reg (arm2-one-untargeted-reg-form seg node arm::arg_z)))
      (unboxed-other-case (vreg :u8)
       (! extract-fulltag vreg reg)
      (ensuring-node-target (target vreg) 
        (! extract-fulltag-fixnum target reg)))
      (^))))


(pushnew (%nx1-operator typecode) *arm2-operator-supports-u8-target*)
(defarm2 arm2-typecode typecode (seg vreg xfer node)
  (if (null vreg)
    (arm2-form seg vreg xfer node)
    (let* ((reg (arm2-one-untargeted-reg-form seg node arm::arg_z)))
      (unboxed-other-case (vreg :u8)
      (! extract-typecode vreg reg)                          
      (ensuring-node-target (target vreg) 
        (! extract-typecode-fixnum target reg )))
      (^))))

(defarm2 arm2-setq-special setq-special (seg vreg xfer sym val)
  (let* ((symreg ($ arm::arg_y))
         (valreg ($ arm::arg_z)))
    (arm2-one-targeted-reg-form seg val valreg)
    (arm2-store-immediate seg (arm2-symbol-value-cell sym) symreg)
    (! setq-special symreg valreg)
    (<- valreg))
  (^))

(defarm2 arm2-load-time-value load-time-value (seg vreg xfer val)
  (arm2-form seg vreg xfer val))

(defarm2 arm2-local-go local-go (seg vreg xfer tag)
  (declare (ignorable xfer))
  (let* ((curstack (arm2-encode-stack))
         (label (cadr tag))
         (deststack (caddr tag)))
    (if (not (arm2-equal-encodings-p curstack deststack))
      (multiple-value-bind (catch cstack vstack)
                           (arm2-decode-stack deststack)
        (arm2-unwind-stack seg nil catch cstack vstack)))
    (-> label)
    (arm2-unreachable-store vreg)))

(defarm2 arm2-local-block local-block (seg vreg xfer blocktag body)
  (let* ((curstack (arm2-encode-stack))
         (compound (arm2-cd-compound-p xfer))
         (mvpass-p (arm2-mvpass-p xfer))
         (need-label (if xfer (or compound mvpass-p) t))
         end-of-block
         last-cd
         (dest (if (backend-crf-p vreg) arm::arg_z vreg)))
    (if need-label
      (setq end-of-block (backend-get-next-label)))
    (setq last-cd (if need-label (%ilogior2 (if mvpass-p $backend-mvpass-mask 0) end-of-block) xfer))
    (%rplaca blocktag (cons (cons dest last-cd) curstack))
    (if mvpass-p
      (arm2-multiple-value-body seg body)
      (arm2-form seg dest (if xfer last-cd) body))
    (when need-label
      (@ end-of-block)
      (if compound
        (<- dest))
      (arm2-branch seg (logand (lognot $backend-mvpass-mask) (or xfer 0)) vreg))))

(defarm2 arm2-%izerop %izerop (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
    (arm2-test-%izerop seg vreg xfer form cr-bit true-p)))


(defarm2 arm2-uvsize uvsize (seg vreg xfer v)
  (let* ((misc-reg (arm2-one-untargeted-reg-form seg v arm::arg_z)))
    (unless *arm2-reckless* (! trap-unless-uvector misc-reg))
    (if vreg 
      (ensuring-node-target (target vreg)
        (! misc-element-count-fixnum target misc-reg)))
    (^)))

(defarm2 arm2-%ilsl %ilsl (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil form1)
      (arm2-form seg nil xfer form2))
    (let* ((const (acode-fixnum-form-p form1))
           (max 31))
      (ensuring-node-target (target vreg)
        (if const
          (let* ((src (arm2-one-untargeted-reg-form seg form2 arm::arg_z)))
            (if (<= const max)
              (! %ilsl-c target const src)
              (!  lri target 0)))
          (multiple-value-bind (count src) (arm2-two-untargeted-reg-forms seg form1 arm::arg_y form2 arm::arg_z)
            (! %ilsl target count src))))
      (^))))

(defarm2 arm2-endp endp (seg vreg xfer cc form)
  (let* ((formreg (arm2-one-untargeted-reg-form seg form arm::arg_z)))
    (! trap-unless-list formreg)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
      (arm2-compare-register-to-nil seg vreg xfer formreg  cr-bit true-p))))



(defarm2 arm2-%code-char %code-char (seg vreg xfer c)
  (if (null vreg)
    (arm2-form seg nil xfer c)
    (progn
      (ensuring-node-target (target vreg)
        (with-imm-target () (dest :u8)
          (! u32->char target (arm2-one-untargeted-reg-form seg c dest))))
      (^))))

(defarm2 arm2-%schar %schar (seg vreg xfer str idx)
  (multiple-value-bind (src unscaled-idx)
                       (arm2-two-untargeted-reg-forms seg str arm::arg_y idx arm::arg_z)
    (if vreg
      (ensuring-node-target (target vreg)
        (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
          (256 (! %schar8 target src unscaled-idx))
          (t (! %schar32 target src unscaled-idx)))))
    (^)))

(defarm2 arm2-%set-schar %set-schar (seg vreg xfer str idx char)
  (multiple-value-bind (src unscaled-idx char)
                       (arm2-three-untargeted-reg-forms seg
                                                        str arm::arg_x
                                                        idx arm::arg_y
                                                        char arm::arg_z)
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256 (! %set-schar8 src unscaled-idx char))
      (t (! %set-schar32 src unscaled-idx char)))
    (when vreg (<- char)) 
    (^)))

(defarm2 arm2-%set-scharcode %set-scharcode (seg vreg xfer str idx char)
  (multiple-value-bind (src unscaled-idx char)
                       (arm2-three-untargeted-reg-forms seg str arm::arg_x idx arm::arg_y
                                                        char arm::arg_z)
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256 (! %set-scharcode8 src unscaled-idx char))
      (t (! %set-scharcode32 src unscaled-idx char)))
    (when vreg (<- char)) 
    (^)))

(defarm2 arm2-%scharcode %scharcode (seg vreg xfer str idx)
  (multiple-value-bind (src unscaled-idx)
      (arm2-two-untargeted-reg-forms seg str arm::arg_y idx arm::arg_z)
    (if vreg
      (ensuring-node-target (target vreg)
        (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
          (256 (! %scharcode8 target src unscaled-idx))
          (t (! %scharcode32 target src unscaled-idx)))))
    (^)))

      

(defarm2 arm2-code-char code-char (seg vreg xfer c)
  (let* ((reg (arm2-one-untargeted-reg-form seg c arm::arg_z)))
    ;; Typecheck even if result unused.
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256 (! require-u8 reg))
      (t (! require-char-code reg)))
    (if vreg
      (ensuring-node-target (target vreg)
        (! fixnum->char target reg)))
    (^)))

(defarm2 arm2-%valid-code-char %valid-code-char (seg vreg xfer c)
  (let* ((reg (arm2-one-untargeted-reg-form seg c arm::arg_z)))
    (when *arm2-full-safety* (! require-char-code reg))
    (if vreg
      (ensuring-node-target (target vreg)
        (! code-char->char target reg)))
    (^)))

(defarm2 arm2-eq eq (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
    (arm2-compare seg vreg xfer form1 form2 cr-bit true-p)))

(defarm2 arm2-neq neq (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
    (arm2-compare seg vreg xfer form1 form2 cr-bit true-p)))


(defarm2 arm2-numcmp numcmp (seg vreg xfer cc form1 form2)
  (let* ((name (ecase (car (acode-operands cc))
                 (:eq '=-2)
                 (:ne '/=-2)
                 (:lt '<-2)
                 (:le '<=-2)
                 (:gt '>-2)
                 (:ge '>=-2))))
    (if (or (arm2-explicit-non-fixnum-type-p form1)
            (arm2-explicit-non-fixnum-type-p form2))
      (arm2-binary-builtin seg vreg xfer name form1 form2)
      (arm2-inline-numcmp seg vreg xfer cc name form1 form2))))

(defun arm2-branch-unless-arg-fixnum (seg reg label)
  (with-arm-local-vinsn-macros (seg)
    (with-crf-target () crf
      (! test-fixnum crf reg)
      (! cbranch-false label crf arm::arm-cond-eq))))

(defun arm2-branch-unless-both-args-fixnums (seg x y label)
  (with-arm-local-vinsn-macros (seg)
    (with-crf-target () crf
      (! test-fixnums crf x y)
      (! cbranch-false label crf arm::arm-cond-eq))))
  

(defun arm2-inline-numcmp (seg vreg xfer cc name form1 form2)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
      (let* ((otherform (and (eql cr-bit arm::arm-cond-eq)
                             (if (eql (acode-fixnum-form-p form2) 0)
                               form1
                               (if (eql (acode-fixnum-form-p form1) 0)
                                 form2)))))
        (if otherform
          (arm2-one-targeted-reg-form seg otherform ($ arm::arg_z))
          (arm2-two-targeted-reg-forms seg  form1 ($ arm::arg_y) form2 ($ arm::arg_z)))
        (let* ((out-of-line (backend-get-next-label))
               (done (backend-get-next-label))
               (continue (backend-get-next-label)))
          (if otherform
            (unless (acode-fixnum-form-p otherform)
              (arm2-branch-unless-arg-fixnum seg ($ arm::arg_z) (aref *backend-labels* out-of-line)))
            (if (acode-fixnum-form-p form1)
              (arm2-branch-unless-arg-fixnum seg ($ arm::arg_z) (aref *backend-labels* out-of-line))
              (if (acode-fixnum-form-p form2)
                (arm2-branch-unless-arg-fixnum seg ($ arm::arg_y) (aref *backend-labels* out-of-line))  
                (arm2-branch-unless-both-args-fixnums seg ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line)))))
          (with-crf-target () crf
                           (if otherform
                             (! compare-immediate crf ($ arm::arg_z) 0)
                             (! compare crf ($ arm::arg_y) ($ arm::arg_z)))
                           (if (and vreg (eql (hard-regspec-class vreg) hard-reg-class-crf))
                             (arm2-branch seg (arm2-cd-merge xfer continue) crf cr-bit true-p)
                             (progn
                               (! cond->boolean ($ arm::arg_z) (if true-p cr-bit (logxor cr-bit 1)))
                               (-> done))))
          (@ out-of-line)
          (if otherform
            (arm2-lri seg ($ arm::arg_y) 0))
          (let* ((index (arch::builtin-function-name-offset name))
                 (idx-subprim (arm2-builtin-index-subprim index)))
            (! call-subprim-2 ($ arm::arg_z) idx-subprim ($ arm::arg_y) ($ arm::arg_z)))
          (@ done)
          (<- ($ arm::arg_z))
          (^)
          (@ continue))))))
    
(defarm2 arm2-%word-to-int %word-to-int (seg vreg xfer form)
  (if (null vreg)
    (arm2-form seg nil xfer form)
    (progn
      (ensuring-node-target (target vreg)
        (! sign-extend-halfword target (arm2-one-untargeted-reg-form seg form arm::arg_z)))
      (^))))

(defarm2 arm2-multiple-value-list multiple-value-list (seg vreg xfer form)
  (arm2-multiple-value-body seg form)
  (! list)
  (when vreg
    (<- arm::arg_z))
  (^))

(defarm2 arm2-immform immediate (seg vreg xfer form)
  (arm2-immediate seg vreg xfer form))

(defarm2 arm2-lexical-reference lexical-reference (seg vreg xfer varnode)
  (let* ((ea-or-form (var-ea varnode)))
    (if (and (acode-punted-var-p varnode) (not (fixnump ea-or-form)))
      (arm2-form seg vreg xfer ea-or-form)
      (progn      
        (unless (or (typep ea-or-form 'lreg) (fixnump ea-or-form))
          (compiler-bug "bogus ref to var ~s (~s) : ~s " varnode (var-name varnode) ea-or-form))
        (arm2-do-lexical-reference seg vreg ea-or-form)
        (^)))))

(defarm2 arm2-setq-lexical setq-lexical (seg vreg xfer varspec form)
  (let* ((ea (var-ea varspec)))
    ;(unless (fixnump ea) (compiler-bug "setq lexical is losing BIG"))
    (if (and (memory-spec-p ea)
             (eql (memspec-type ea) memspec-nfp-offset))
      (let* ((reg (arm2-one-untargeted-reg-form seg form (arm2-reg-for-nfp-set vreg ea))))
        (arm2-nfp-set seg reg ea)
        (when vreg (arm2-copy-register seg vreg reg)))
    (let* ((valreg (arm2-one-untargeted-reg-form seg form (if (and (register-spec-p ea) 
                                                                   (or (null vreg) (eq ea vreg)))
                                                            ea
                                                            arm::arg_z))))
      (arm2-do-lexical-setq seg vreg ea valreg)))
    (^)))

(defarm2 arm2-fixnum fixnum (seg vreg xfer value)
  (if (null vreg)
    (^)
    (let* ((class (hard-regspec-class vreg))
           (mode (get-regspec-mode vreg))
           (unboxed (if (= class hard-reg-class-gpr)
                      (not (or (= hard-reg-class-gpr-mode-node mode)
                               (= hard-reg-class-gpr-mode-address mode))))))
      (if unboxed
        (arm2-absolute-natural seg vreg xfer value)
        (if (= class hard-reg-class-crf)
          (progn
            ;(compiler-bug "Would have clobbered a GPR!")
            (arm2-branch seg (arm2-cd-true xfer) nil))
          (progn
            (ensuring-node-target (target vreg)
              (let* ((boxed (ash value *arm2-target-fixnum-shift*))
                     (regval (hard-regspec-value target))
                     (regs (arm2-gprs-containing-constant value))
                     (small (or (arm::encode-arm-immediate boxed)
                                (arm::encode-arm-immediate (lognot boxed)))))
                (unless (logbitp regval regs)
                  (if (or small (eql 0 regs))
                    (arm2-absolute-natural seg target nil boxed)
                    (let* ((r (1- (integer-length regs))))
                      (! copy-node-gpr target r)))
                  (setf *arm2-gpr-constants-valid-mask*
                        (logior *arm2-gpr-constants-valid-mask*
                                (ash 1 regval))
                        (svref *arm2-gpr-constants* regval) value))))
            (^)))))))

(defarm2 arm2-%ilogbitp %ilogbitp (seg vreg xfer cc bitnum form)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil bitnum)
      (arm2-form seg vreg xfer form))
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
      (let* ((fixbit (acode-fixnum-form-p bitnum)))
        (if fixbit
          (let* ((reg (arm2-one-untargeted-reg-form seg form arm::arg_z))
                 (arm-bit (min (- arm::nbits-in-word arm::fixnumshift)
                               (max fixbit 0))))
            (regspec-crf-gpr-case 
             (vreg)
             (progn
               (! %ilogbitp-constant-bit vreg reg arm-bit)
               (^ cr-bit true-p))
             (with-crf-target () crf
                (! %ilogbitp-constant-bit crf reg arm-bit)
                (ensuring-node-target (target vreg)
                  (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
                (^))))
          (multiple-value-bind (rbit rform) (arm2-two-untargeted-reg-forms seg bitnum arm::arg_y form arm::arg_z)
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

(defarm2 arm2-uvref uvref (seg vreg xfer vector index)
  (arm2-two-targeted-reg-forms seg vector ($ arm::arg_y) index ($ arm::arg_z))
  (! misc-ref)
  (<- ($ arm::arg_z))
  (^))

(defarm2 arm2-uvset uvset (seg vreg xfer vector index value)
  (arm2-three-targeted-reg-forms seg vector ($ arm::arg_x) index ($ arm::arg_y) value ($ arm::arg_z))
  (! misc-set)
  (<- ($ arm::arg_z))
  (^))

(defarm2 arm2-%decls-body %decls-body (seg vreg xfer form p2decls)
  (with-arm-p2-declarations p2decls
    (arm2-form seg vreg xfer form)))



(defarm2 arm2-%err-disp %err-disp (seg vreg xfer arglist)
  (arm2-set-nargs seg (arm2-arglist seg arglist))
  (! ksignalerr)
  (arm2-nil seg vreg xfer))


(defarm2 arm2-local-tagbody local-tagbody (seg vreg xfer taglist body)
  (let* ((encstack (arm2-encode-stack))
         (tagop (%nx1-operator tag-label)))
    (dolist (tag taglist)
      (rplacd tag (cons (backend-get-next-label) (cons encstack (cadr (cddr (cddr tag)))))))
    (dolist (form body)
      (if (eq (acode-operator form) tagop)
        (let ((tag (cdar (acode-operands form))))
          (@ (car tag)))
        (arm2-form seg nil nil form)))
    (arm2-nil seg vreg xfer)))

(defarm2 arm2-call call (seg vreg xfer fn arglist &optional spread-p)
  (when (and (null vreg)
             (acode-p fn)
             (eq (acode-operator fn) (%nx1-operator immediate)))
    (let* ((name (car (acode-operands fn))))
      (when (memq name *warn-if-function-result-ignored*)
        (p2-whine *arm2-cur-afunc*  :result-ignored name))))
  (arm2-call-fn seg vreg xfer fn arglist spread-p))

(defarm2 arm2-self-call self-call (seg vreg xfer arglist &optional spread-p)
  (setq arglist (arm2-augment-arglist *arm2-cur-afunc* arglist (if spread-p 1 $numarmargregs)))
  (arm2-call-fn seg vreg xfer -1 arglist spread-p))


(defarm2 arm2-lexical-function-call lexical-function-call (seg vreg xfer afunc arglist &optional spread-p)
  (arm2-call-fn seg vreg xfer (make-acode (%nx1-operator simple-function) afunc)
                (arm2-augment-arglist afunc arglist (if spread-p 1 $numarmargregs))
                spread-p))

(defarm2 arm2-builtin-call builtin-call (seg vreg xfer index arglist)
  (let* ((nargs (arm2-arglist seg arglist))
         (tail-p (and (arm2-tailcallok xfer) (<= nargs $numarmargregs)))
         (idx (acode-fixnum-form-p index))
         (idx-subprim (arm2-builtin-index-subprim idx))
         (subprim
          (or idx-subprim
              (compiler-bug "Isn't this code long since unused ?")
              #+nil
              (case nargs
                (0 (arm::arm-subprimitive-offset '.SPcallbuiltin0))
                (1 (arm::arm-subprimitive-offset '.SPcallbuiltin1))
                (2 (arm::arm-subprimitive-offset '.SPcallbuiltin2))
                (3 (arm::arm-subprimitive-offset '.SPcallbuiltin3))
                (t (arm::arm-subprimitive-offset '.SPcallbuiltin))))))
    (when tail-p
      (arm2-restore-nvrs seg nil)
      (arm2-restore-non-volatile-fprs seg)
      (! restore-nfp)
      (arm2-restore-full-lisp-context seg))
    #+nil
    (unless idx-subprim
      (! lri arm::imm0 (ash idx *arm2-target-fixnum-shift*))
      (when (eql subprim (arm::arm-subprimitive-offset '.SPcallbuiltin))
        (arm2-set-nargs seg nargs)))
    (if tail-p
      (! jump-subprim subprim)
      (progn
        (! call-subprim subprim)
        (<- arm::arg_z)
        (^)))))

;;; If exactly one vinsn references LAB and that vinsn is a conditional
;;; branch, it's a forward branch.  If every vinsn between the branch
;;; and label can be predicated, do so and remove both the branch and
;;; the label.
;;; "predicate" is being used as a verb here - "to make predicated".
(defun arm2-predicate-block (labelnum)
  (declare (ignorable labelnum))
  #+later
  
  (let* ((lab (aref *backend-labels* labelnum))
         (refs (vinsn-label-refs lab))
         (branch (car refs)))
    (if (and (vinsn-attribute-p branch :branch)
             (null (cdr refs)))
      (when (do* ((next (dll-node-succ branch) (dll-node-succ next))
                  (count 0 (1+ count))
                  (vinsn-p nil))
                 ((eq next lab) (return vinsn-p))
              (declare (fixnum count))
              (if (typep next 'vinsn-label)
                (unless (typep (vinsn-label-id next) 'vinsn-note)
                  (return))
                (progn
                  (when (= count 2)
                    (return))
                  (unless (and (typep next 'vinsn)
                               (null (getf (vinsn-annotation next) :predicate))
                               (vinsn-attribute-p next :predicatable)
                               (or (eq lab (dll-node-succ next))
                                   (not (vinsn-attribute-p next :jump :call  :jumpLR))))
                    (return))
                  (setq vinsn-p t))))
        (multiple-value-bind (branch-true-p branch-condition cond-operand-index)
            (let* ((branch-instr (car (vinsn-template-body (vinsn-template branch))))
                   (values (vinsn-variable-parts branch))
                   (operands (cdr branch-instr)))
              (dolist (op operands (values nil nil nil))
                (cond ((eql (car op) (arm::encode-vinsn-field-type :cond))
                       (return (values t (svref values (cadr op)) (cadr op))))
                      ((eql (car op) (arm::encode-vinsn-field-type :negated-cond))
                       (return (values nil (svref values (cadr op)) (cadr op)))))))
          (when branch-condition
            (let* ((condition (if branch-true-p (logxor 1 branch-condition) branch-condition)))
              (do* ((next (dll-node-succ branch) (dll-node-succ next)))
                   ((eq next lab) 
                    (elide-vinsn branch) 
                    (remove-dll-node lab)
                    t)
                (cond ((typep next 'vinsn-label))
                      ((vinsn-attribute-p next :jump)
                       (break)
                       (setf (vinsn-template next)
                             (need-vinsn-template 'cbranch-true
                                                  (backend-p2-vinsn-templates
                                                   *target-backend*))
                             (svref (vinsn-variable-parts next) cond-operand-index)
                             condition))
                      (t (setf (getf (vinsn-annotation next) :predicate) condition)))))))))))

(defparameter *arm2-generate-casejump* t)

(defun arm2-generate-casejump (seg vreg xfer ranges trueforms var otherwise)
  (when *arm2-generate-casejump*
    (with-arm-local-vinsn-macros (seg vreg xfer)
      (when ranges
        (let* ((min (caar ranges))
               (max min)
               (count 0)
               (all ())
               (labeled-trueforms ()))
          (declare (fixnum min max count))
          (when                         ; determine min,max, count; punt on duplicate keys
              (dolist (range ranges t)
                (let* ((info (cons (backend-get-next-label) (pop trueforms))))
                  (push info labeled-trueforms)
                  (unless (dolist (val range t)
                            (declare (fixnum val))
                            (when (assoc val all)
                              (return nil))
                            (push (cons val info) all)
                            (if (< val min)
                              (setq min val)
                              (if (> val max)
                                (setq max val)))
                            (incf count))
                    (return nil))))
            (let* ((span (1+ (- max min))))
              (when (and (> count 4)
                         (>= count (the fixnum (- span (the fixnum (ash span -2))))))
                (let* ((defaultlabel (backend-get-next-label))
                       (endlabel (backend-get-next-label))
                       (single-clause (and (eql count span)
                                           (eql (length labeled-trueforms) 1))))
                  (let* ((reg (arm2-one-untargeted-reg-form seg (make-acode (%nx1-operator lexical-reference) var) arm::arg_z)))
                    (! lock-constant-pool)
                    (with-imm-target () (idx :u32)
                      (with-crf-target () flags
                        (arm2-branch-unless-arg-fixnum seg reg (aref *backend-labels* defaultlabel))
                        (! set-carry-if-fixnum-in-range idx flags reg min span)
                        (! cbranch-false (aref *backend-labels* defaultlabel) flags arm::arm-cond-lo)
                        (unless single-clause
                          (! ijmp idx)
                          (do* ((val min (1+ val)))
                               ((> val max) (! nop))
                            (declare (fixnum val))
                            (let* ((info (assoc val all)))
                              (! non-barrier-jump (aref *backend-labels* (if info (cadr info) defaultlabel)))))))
                      (! unlock-constant-pool)
                      (let* ((target (if (arm2-mvpass-p xfer)
                                       (logior $backend-mvpass-mask endlabel)
                                       (arm2-cd-merge xfer endlabel)))
                             (entry-stack (arm2-encode-stack)))
                        (dolist (case (nreverse labeled-trueforms))
                          (let* ((lab (car case))
                                 (form (cdr case)))
                            (@ lab)
                            (multiple-value-setq (*arm2-undo-count*
                                                  *arm2-cstack*
                                                  *arm2-vstack*)
                              (arm2-decode-stack entry-stack))
                            (when (arm2-mvpass-p xfer)
                              (arm2-open-undo $undomvexpect))
                            (arm2-undo-body seg vreg target form entry-stack)))
                        (@ defaultlabel)
                        (multiple-value-setq (*arm2-undo-count*
                                              *arm2-cstack*
                                              *arm2-vstack*)
                          (arm2-decode-stack entry-stack))
                        (when (arm2-mvpass-p xfer)
                          (arm2-open-undo $undomvexpect)) 
                        (arm2-undo-body seg vreg target otherwise entry-stack)
                        (@ endlabel)
                        (when (arm2-mvpass-p xfer)
                          (let* ((*arm2-returning-values* :mvpass))
                            (^)))
                        t))))))))))))
                        

(defarm2 arm2-if if (seg vreg xfer testform true false &aux test-val)
  (if (setq test-val (nx2-constant-form-value (acode-unwrapped-form-value testform)))
    (arm2-form seg vreg xfer (if (nx-null test-val) false true))
    (multiple-value-bind (ranges trueforms var otherwise)
        (nx2-reconstruct-case testform true false)
      (or (arm2-generate-casejump seg vreg xfer ranges trueforms var otherwise)
          (let* ((cstack *arm2-cstack*)
                 (vstack *arm2-vstack*)
                 (entry-stack (arm2-encode-stack))
                 (true-stack nil)
                 (false-stack nil)
                 (true-cleanup-label nil)
                 (same-stack-effects nil)
                 (true-is-goto (arm2-go-label true))
                 (false-is-goto (and (not true-is-goto) (arm2-go-label false)))
                 (endlabel (backend-get-next-label))
                 (falselabel (backend-get-next-label))
                 (need-else (unless false-is-goto (or (not (nx-null false)) (arm2-for-value-p vreg))))
                 (both-single-valued (and (not *arm2-open-code-inline*)
                                          (eq xfer $backend-return)
                                          (arm2-for-value-p vreg)
                                          need-else
                                          (arm2-single-valued-form-p true) 
                                          (arm2-single-valued-form-p false)))
                 (saved-reg-mask 0)
                 (saved-constants-mask 0)
                 (saved-reg-map (make-array 16 :initial-element nil))
                 (saved-constants-map (make-array 16)))
            (declare (dynamic-extent saved-reg-map saved-constants-map))
            (if (eq 0 xfer) 
              (setq xfer nil))
            (if both-single-valued      ; it's implied that we're returning
              (let* ((result arm::arg_z))
                (arm2-conditional-form seg (arm2-make-compound-cd 0 falselabel) testform)
                (arm2-copy-regmap (setq saved-reg-mask *arm2-gpr-locations-valid-mask*)
                                  *arm2-gpr-locations*
                                  saved-reg-map)
                (arm2-copy-constmap (setq saved-constants-mask *arm2-gpr-constants-valid-mask*)
                                    *arm2-gpr-constants*
                                    saved-constants-map)
                (arm2-form seg result endlabel true)
                (progn
                  (@ falselabel)
                  (arm2-predicate-block falselabel)
                  (let* ((*arm2-gpr-locations-valid-mask* saved-reg-mask)
                         (*arm2-gpr-locations* saved-reg-map)
                         (*arm2-gpr-constants-valid-mask* saved-constants-mask)
                         (*arm2-gpr-constants* saved-constants-map))
                    (arm2-form seg result nil false)))
                (@ endlabel)
                (arm2-predicate-block endlabel)
                (<- result)
                (^))
              (progn
                (if (and need-else (arm2-mvpass-p xfer))
                  (setq true-cleanup-label (backend-get-next-label)))         
                (arm2-conditional-form 
                 seg
                 (arm2-make-compound-cd 
                  (or true-is-goto 0)
                  (or false-is-goto 
                      (if need-else 
                        (if true-is-goto 0 falselabel) 
                        (if true-is-goto xfer (arm2-cd-merge xfer falselabel))))) 
                 testform)
                (arm2-copy-regmap (setq saved-reg-mask *arm2-gpr-locations-valid-mask*)
                                  *arm2-gpr-locations*
                                  saved-reg-map)
                (arm2-copy-constmap (setq saved-constants-mask *arm2-gpr-constants-valid-mask*)
                                    *arm2-gpr-constants*
                                    saved-constants-map)
                (if true-is-goto
                  (arm2-unreachable-store)
                  (if true-cleanup-label
                    (progn
                      (arm2-open-undo $undomvexpect)
                      (arm2-form seg vreg (logior $backend-mvpass-mask true-cleanup-label) true))
                    (arm2-form seg vreg (if need-else (arm2-cd-merge xfer endlabel) xfer) true)))
                (setq true-stack (arm2-encode-stack))
                (setq *arm2-cstack* cstack)
                (arm2-set-vstack vstack)
                (if false-is-goto (arm2-unreachable-store))
                (progn
                  (if (and (not need-else) nil)
                    (@+ falselabel)
                    (@ falselabel))
                  (arm2-predicate-block falselabel)
                  (when need-else
                    (if true-cleanup-label
                      (arm2-mvpass seg false)
                      (let* ((*arm2-gpr-locations-valid-mask* saved-reg-mask)
                             (*arm2-gpr-locations* saved-reg-map)
                             (*arm2-gpr-constants-valid-mask* saved-constants-mask)
                             (*arm2-gpr-constants* saved-constants-map))
                        (arm2-form seg vreg xfer false)))
                    (setq false-stack (arm2-encode-stack))))
                (when true-cleanup-label
                  (if (setq same-stack-effects (arm2-equal-encodings-p true-stack false-stack)) ; can share cleanup code
                    (@ true-cleanup-label))
                  (let* ((*arm2-returning-values* :pass))
                    (arm2-nlexit seg xfer 1)
                    (arm2-branch seg (if (and xfer (neq xfer $backend-mvpass-mask)) xfer (if (not same-stack-effects) endlabel)) vreg))
                  (unless same-stack-effects
                    (@ true-cleanup-label)
                    (multiple-value-setq (true *arm2-cstack* *arm2-vstack* )
                      (arm2-decode-stack true-stack))
                    (let* ((*arm2-returning-values* :pass))
                      (arm2-nlexit seg xfer 1)
                      (^)))
                  (arm2-close-undo)
                  (multiple-value-setq (*arm2-undo-count* *arm2-cstack* *arm2-vstack*) 
                    (arm2-decode-stack entry-stack)))
                (if (and (not need-else) (backend-crf-p vreg) nil)
                  (@+ endlabel)
                  (@ endlabel)) 
                (arm2-predicate-block endlabel))))))))

(defarm2 arm2-or or (seg vreg xfer forms)
  (let* ((mvpass (arm2-mvpass-p xfer))
         (tag1 (backend-get-next-label))
         (tag2 (backend-get-next-label))
         (vstack *arm2-vstack*)
         (cstack *arm2-cstack*)
         (dest (if (backend-crf-p vreg) vreg (if vreg arm::arg_z (available-crf-temp *available-backend-crf-temps*))))
         (cd1 (arm2-make-compound-cd 
               (if (eq dest arm::arg_z) tag1 (arm2-cd-merge (arm2-cd-true xfer) tag1)) 0)))
    (while (cdr forms)
      (arm2-form seg dest (if (eq dest arm::arg_z) nil cd1) (car forms))
      (when (eq dest arm::arg_z)
        (with-crf-target () val-crf
          (arm2-copy-register seg val-crf dest)
          (arm2-branch seg cd1 val-crf)))
      (setq forms (%cdr forms)))
    (if mvpass
      (progn (arm2-multiple-value-body seg (car forms)) 
             (let* ((*arm2-returning-values* t)) (arm2-branch seg (arm2-cd-merge xfer tag2) vreg)))
      (arm2-form seg  vreg (if (eq dest arm::arg_z) (arm2-cd-merge xfer tag2) xfer) (car forms)))
    (setq *arm2-vstack* vstack *arm2-cstack* cstack)
    (@ tag1)
    (when (eq dest arm::arg_z)
      (<- arm::arg_z)
      (^))
    (@ tag2)))

(defarm2 arm2-simple-function simple-function (seg vreg xfer afunc)
  (arm2-immediate seg vreg xfer (arm2-afunc-lfun-ref afunc)))

(defarm2 arm2-list list (seg vreg xfer arglist)
  (if (null vreg)
    (dolist (form arglist)
      (arm2-form seg vreg nil form)) 
    (let* ((*arm2-vstack* *arm2-vstack*)
           (nargs (arm2-formlist seg arglist nil)))
      (arm2-set-nargs seg nargs)
      (! list)
      (<- arm::arg_z)))
  (^))

(defarm2 arm2-list* list* (seg vreg xfer arglist)
  (if (null vreg)
    (dolist (arg (apply #'append arglist))
      (arm2-form seg nil nil arg))
    (let* ((*arm2-vstack* *arm2-vstack*)
           (nargs (arm2-arglist seg arglist)))
      (declare (fixnum nargs))
      (when (> nargs 1)
        (arm2-set-nargs seg (1- nargs))
        (! list*))
      (<- arm::arg_z)))
  (^))

(defarm2 arm2-minus1 minus1 (seg vreg xfer form)
  (arm2-unary-builtin seg vreg xfer '%negate form))

(defarm2 arm2-%double-float-negate %double-float-negate (seg vreg xfer form)
  (with-fp-target () (r1 :double-float)
    (setq r1 (arm2-one-untargeted-reg-form seg form r1))
    (if (and vreg
             (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
      (! double-float-negate vreg r1)
      (with-fp-target (r1) (r2 :double-float)
        (! double-float-negate r2 r1)
        (ensuring-node-target (target vreg)
          (arm2-copy-register seg target r2))))
    (^)))
             

(defarm2 arm2-%single-float-negate %single-float-negate (seg vreg xfer form)
  (with-fp-target () (r1 :single-float)
    (setq r1 (arm2-one-untargeted-reg-form seg form r1))
    (if (and vreg
             (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
      (! single-float-negate vreg r1)
      (with-fp-target (r1) (r2 :single-float)
        (! single-float-negate r2 r1)
        (ensuring-node-target (target vreg)
          (arm2-copy-register seg target r2))))
    (^)))


(defun arm2-inline-add2 (seg vreg xfer form1 form2)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (arm2-two-targeted-reg-forms seg form1 ($ arm::arg_y) form2 ($ arm::arg_z))
    (let* ((out-of-line (backend-get-next-label))
           (done (backend-get-next-label)))
      (ensuring-node-target (target vreg)
        (if (acode-fixnum-form-p form1)
          (arm2-branch-unless-arg-fixnum seg ($ arm::arg_z) (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (arm2-branch-unless-arg-fixnum seg ($ arm::arg_y) (aref *backend-labels* out-of-line))  
            (arm2-branch-unless-both-args-fixnums seg ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line))))
        (with-crf-target () flags
          (! fixnum-add-set-flags ($ arm::arg_z) flags ($ arm::arg_y) ($ arm::arg_z))
          (arm2-check-fixnum-overflow seg flags ($ arm::arg_z) done))
        (@ out-of-line)
        (! call-subprim-2 ($ arm::arg_z) (arm::arm-subprimitive-offset '.SPbuiltin-plus) ($ arm::arg_y) ($ arm::arg_z))
        (@ done)
        (arm2-copy-register seg target ($ arm::arg_z)))
      (^))))

(defun arm2-inline-sub2 (seg vreg xfer form1 form2)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (arm2-two-targeted-reg-forms seg form1 ($ arm::arg_y) form2 ($ arm::arg_z))
    (let* ((out-of-line (backend-get-next-label))
           (done (backend-get-next-label)))
      (ensuring-node-target (target vreg)
        (if (acode-fixnum-form-p form1)
          (arm2-branch-unless-arg-fixnum seg ($ arm::arg_z) (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (arm2-branch-unless-arg-fixnum seg ($ arm::arg_y) (aref *backend-labels* out-of-line))  
            (arm2-branch-unless-both-args-fixnums seg ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line))))
        (with-crf-target () flags
          (! fixnum-sub-set-flags ($ arm::arg_z) flags ($ arm::arg_y) ($ arm::arg_z))
          (arm2-check-fixnum-overflow seg flags ($ arm::arg_z) done))
        
        (@ out-of-line)
        (! call-subprim-2 ($ arm::arg_z) (arm::arm-subprimitive-offset '.SPbuiltin-minus) ($ arm::arg_y) ($ arm::arg_z))
        (@ done)
        (arm2-copy-register seg target ($ arm::arg_z)))
      (^))))

;;; Return T if form is declared to be something that couldn't be a fixnum.
(defun arm2-explicit-non-fixnum-type-p (form)
  (let* ((type (arm2-form-type form))
         (target-fixnum-type (nx-target-type 'fixnum)))
    (and (not (subtypep type target-fixnum-type))
         (not (subtypep target-fixnum-type type)))))


    

(defarm2 arm2-add2 add2 (seg vreg xfer form1 form2)
  (if (or (arm2-explicit-non-fixnum-type-p form1)
          (arm2-explicit-non-fixnum-type-p form2))
    (arm2-binary-builtin seg vreg xfer '+-2 form1 form2)
    (arm2-inline-add2 seg vreg xfer form1 form2)))

(defarm2 arm2-sub2 sub2 (seg vreg xfer form1 form2)
  (if (or (arm2-explicit-non-fixnum-type-p form1)
          (arm2-explicit-non-fixnum-type-p form2))
    (arm2-binary-builtin seg vreg xfer '--2 form1 form2)
    (arm2-inline-sub2 seg vreg xfer form1 form2)))

(defarm2 arm2-mul2 mul2 (seg vreg xfer form1 form2)
  (arm2-binary-builtin seg vreg xfer '*-2 form1 form2))


(defarm2 arm2-div2 div2 (seg vreg xfer form1 form2)
  (arm2-binary-builtin seg vreg xfer '/-2 form1 form2))

(defarm2 arm2-logbitp logbitp (seg vreg xfer bitnum int)
  (arm2-binary-builtin seg vreg xfer 'logbitp bitnum int))


(defun arm2-inline-logior2 (seg vreg xfer form1 form2)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2)))
      (if (and fix1 fix2)
        (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer (logior fix1 fix2))
        (let* ((fixval (or fix1 fix2))
               (unboxed-fixval (if fixval (ash fixval *arm2-target-fixnum-shift*)))
               (ok-imm (and unboxed-fixval (arm::encode-arm-immediate unboxed-fixval)))
               (otherform (if ok-imm (if fix1 form2 form1)))
               (out-of-line (backend-get-next-label))
               (done (backend-get-next-label)))

          (if otherform
            (arm2-one-targeted-reg-form seg otherform ($ arm::arg_z))
            (arm2-two-targeted-reg-forms seg form1 ($ arm::arg_y) form2 ($ arm::arg_z)))
          (ensuring-node-target (target vreg)
            (if otherform
              (unless (acode-fixnum-form-p otherform)
                (arm2-branch-unless-arg-fixnum seg ($ arm::arg_z) (aref *backend-labels* out-of-line)))
              (if (acode-fixnum-form-p form1)
                (arm2-branch-unless-arg-fixnum seg ($ arm::arg_z) (aref *backend-labels* out-of-line))
                (if (acode-fixnum-form-p form2)
                  (arm2-branch-unless-arg-fixnum seg ($ arm::arg_y) (aref *backend-labels* out-of-line))  
                  (arm2-branch-unless-both-args-fixnums seg ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line)))))
            (if otherform
              (! logior-immediate ($ arm::arg_z) ($ arm::arg_z) (logand #xffffffff unboxed-fixval))
              (! %logior2 ($ arm::arg_z) ($ arm::arg_z) ($ arm::arg_y)))
            (-> done)
            (@ out-of-line)
            (if otherform
              (arm2-lri seg ($ arm::arg_y) (ash fixval *arm2-target-fixnum-shift*)))
            (! call-subprim-2 ($ arm::arg_z) (arm::arm-subprimitive-offset '.SPbuiltin-logior) ($ arm::arg_y) ($ arm::arg_z))
            (@ done)
            (arm2-copy-register seg target ($ arm::arg_z)))
          (^))))))

(defarm2 arm2-logior2 logior2 (seg vreg xfer form1 form2)
  (if (or (arm2-explicit-non-fixnum-type-p form1)
          (arm2-explicit-non-fixnum-type-p form2))
    (arm2-binary-builtin seg vreg xfer 'logior-2 form1 form2)
    (arm2-inline-logior2 seg vreg xfer form1 form2)))

(defarm2 arm2-logxor2 logxor2 (seg vreg xfer form1 form2)
  (arm2-binary-builtin seg vreg xfer 'logxor-2 form1 form2))

(defun arm2-inline-logand2 (seg vreg xfer form1 form2)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2)))
      (if (and fix1 fix2)
        (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer (logand fix1 fix2))
        (let* ((fixval (or fix1 fix2))
               (unboxed-fixval (if fixval (ash fixval *arm2-target-fixnum-shift*)))
               (ok-imm (and unboxed-fixval
                            (or (arm::encode-arm-immediate unboxed-fixval)
                                (arm::encode-arm-immediate (lognot unboxed-fixval)))))
               (otherform (if ok-imm (if fix1 form2 form1)))
               (out-of-line (backend-get-next-label))
               (done (backend-get-next-label)))
          (if otherform
            (arm2-one-targeted-reg-form seg otherform ($ arm::arg_z))
            (arm2-two-targeted-reg-forms  seg form1 ($ arm::arg_y) form2 ($ arm::arg_z)))
          (ensuring-node-target (target vreg)
            (if otherform
              (unless (acode-fixnum-form-p otherform)
                (arm2-branch-unless-arg-fixnum seg ($ arm::arg_z) (aref *backend-labels* out-of-line)))
              (if (acode-fixnum-form-p form1)
                (arm2-branch-unless-arg-fixnum seg ($ arm::arg_z) (aref *backend-labels* out-of-line))
                (if (acode-fixnum-form-p form2)
                  (arm2-branch-unless-arg-fixnum seg ($ arm::arg_y) (aref *backend-labels* out-of-line))  
                  (arm2-branch-unless-both-args-fixnums seg ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line)))))
            (if otherform
              (! logand-immediate ($ arm::arg_z) ($ arm::arg_z) (logand #xffffffff unboxed-fixval))
              (! %logand2 ($ arm::arg_z) ($ arm::arg_z) ($ arm::arg_y)))
            (-> done)
            (@ out-of-line)
            (if otherform
              (arm2-lri seg ($ arm::arg_y) (ash fixval *arm2-target-fixnum-shift*)))
            (! call-subprim-2 ($ arm::arg_z) (arm::arm-subprimitive-offset '.SPbuiltin-logand) ($ arm::arg_y) ($ arm::arg_z))          
            (@ done)
            (arm2-copy-register seg target ($ arm::arg_z)))
          (^))))))

(defarm2 arm2-logand2 logand2 (seg vreg xfer form1 form2)
  (if (or (arm2-explicit-non-fixnum-type-p form1)
          (arm2-explicit-non-fixnum-type-p form2))
    (arm2-binary-builtin seg vreg xfer 'logand-2 form1 form2)
    (arm2-inline-logand2 seg vreg xfer form1 form2)))



(defarm2 arm2-%aref1 %aref1 (seg vreg xfer v i)
  (let* ((vtype (acode-form-type v t))
         (atype (if vtype (let* ((a (specifier-type vtype)))
                            (if (typep a 'array-ctype) a))))
         (maybe-1d (and atype
                        (let* ((dims (array-ctype-dimensions atype)))
                          (or (eq dims '*)
                              (and (not (atom dims))
                                   (= (length dims) 1))))))
         
         (complexp (and atype (array-ctype-complexp atype)))
         (keyword (and atype
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype)))
         (typecode (and keyword (not *arm2-reckless*)
                        (nx-lookup-target-uvector-subtag keyword))))
    (if (and maybe-1d keyword)
      (if (not complexp)
        (arm2-vref  seg vreg xfer keyword v i typecode)
        (arm2-1d-vref seg vreg xfer keyword v i typecode))
      (arm2-binary-builtin seg vreg xfer '%aref1 v i))))

(defarm2 arm2-%aset1 aset1 (seg vreg xfer v i n)
  (let*   ((vtype (acode-form-type v t))
         (atype (if vtype (let* ((a (specifier-type vtype)))
                            (if (typep a 'array-ctype) a))))
         (maybe-1d (and atype
                        (let* ((dims (array-ctype-dimensions atype)))
                          (or (eq dims '*)
                              (and (not (atom dims))
                                   (= (length dims) 1))))))
         
         (complexp (and atype (array-ctype-complexp atype)))
         (keyword (and atype
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype)))
         (typecode (and keyword (not *arm2-reckless*)
                        (nx-lookup-target-uvector-subtag keyword))))

    (if (and maybe-1d keyword)
      (if (not complexp)
        (arm2-vset seg vreg xfer keyword v i n typecode)

        (arm2-1d-vset seg vreg xfer keyword v i n typecode))
      (arm2-ternary-builtin seg vreg xfer '%aset1 v i n))))

(defun arm2-fixnum-add (seg vreg xfer form1 form2 overflow)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (when overflow
      (let* ((type *arm2-target-half-fixnum-type*))
        (when (and (arm2-form-typep form1 type)
                   (arm2-form-typep form2 type))
          (setq overflow nil))))
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2))
           (sum (and fix1 fix2 (if overflow (+ fix1 fix2) (%i+ fix1 fix2))))
           (other (unless sum
                    (if (and
                         fix1
                         (typep (ash fix1 *arm2-target-fixnum-shift*)
                                '(signed-byte 32))
                         (or (arm::encode-arm-immediate
                              (ash fix1 *arm2-target-fixnum-shift*))
                             (arm::encode-arm-immediate
                              (- (ash fix1 *arm2-target-fixnum-shift*)))))
                      form2
                      (if (and fix2
                               (typep (ash fix2 *arm2-target-fixnum-shift*)
                                      '(signed-byte 32))
                               (or (arm::encode-arm-immediate
                                    (ash fix2 *arm2-target-fixnum-shift*))
                                   (arm::encode-arm-immediate
                                    (- (ash fix2 *arm2-target-fixnum-shift*)))))
                        form1))))
           (constant (and other (ash (or fix1 fix2) *arm2-target-fixnum-shift*)))
           r1
           r2)
      (cond ((null vreg) 
             (arm2-form seg nil nil form1) 
             (arm2-form seg nil xfer form2))
            (sum
             (if (nx1-target-fixnump sum)
               (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer sum)
               (arm2-use-operator (%nx1-operator immediate) seg vreg xfer sum)))
            ((eql 0 constant)
             (arm2-form seg vreg xfer other))
            (t
             (if constant
               (setq r1 (arm2-one-untargeted-reg-form seg other arm::arg_z))
               (multiple-value-setq (r1 r2) (arm2-two-untargeted-reg-forms seg form1 arm::arg_y form2 arm::arg_z)))
             (ensuring-node-target (target vreg)
               (cond ((null overflow)
                      (if constant
                        (! add-immediate target r1 constant)
                        (! fixnum-add target r1 r2)))
                     (t
                      (with-crf-target () flags
                        (if constant
                          (! add-immediate-set-flags target flags r1 constant)
                          (! fixnum-add-set-flags target flags r1 r2))
                        (arm2-check-fixnum-overflow seg flags target)))))
               (^))))))



(defarm2 arm2-fixnum-add-overflow fixnum-add-overflow (seg vreg xfer form1 form2)
  (arm2-fixnum-add seg vreg xfer form1 form2 t))

(defarm2 arm2-fixnum-add-no-overflow fixnum-add-no-overflow (seg vreg xfer form1 form2)
  (arm2-fixnum-add seg vreg xfer form1 form2 nil))
    

(defun arm2-fixnum-sub (seg vreg xfer form1 form2 overflow)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (when overflow
      (let* ((type *arm2-target-half-fixnum-type*))
        (when (and (arm2-form-typep form1 type)
                   (arm2-form-typep form2 type))
          (setq overflow nil))))
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2))
           (diff (and fix1 fix2 (if overflow (- fix1 fix2) (%i- fix1 fix2))))
           (other (unless diff
                    (if (and fix2
                               (typep (ash fix2 *arm2-target-fixnum-shift*)
                                      '(signed-byte 32))
                               (or (arm::encode-arm-immediate
                                    (ash fix2 *arm2-target-fixnum-shift*))
                                   (arm::encode-arm-immediate
                                    (- (ash fix2 *arm2-target-fixnum-shift*)))))
                        form1)))
           (constant (and other (ash (or fix1 fix2) *arm2-target-fixnum-shift*)))
           r1
           r2)
      (cond ((null vreg) 
             (arm2-form seg nil nil form1) 
             (arm2-form seg nil xfer form2))
            (diff
             (if (nx1-target-fixnump diff)
               (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer diff)
               (arm2-use-operator (%nx1-operator immediate) seg vreg xfer diff)))
            ((eql 0 constant)
             (arm2-form seg vreg xfer other))
            (t
             (if constant
               (setq r1 (arm2-one-untargeted-reg-form seg other arm::arg_z))
               (multiple-value-setq (r1 r2) (arm2-two-untargeted-reg-forms seg form1 arm::arg_y form2 arm::arg_z)))
             (ensuring-node-target (target vreg)
               (cond ((null overflow)
                      (if constant
                        (if fix1
                          (! fixnum-sub-from-constant target constant r1)
                          (! fixnum-sub-constant target r1 constant))
                        (! fixnum-sub target r1 r2)))
                     (t
                      (with-crf-target () flags
                        (if constant
                          (if fix1
                            (! fixnum-sub-from-constant-set-flags target flags constant r1)
                            (! fixnum-sub-constant-set-flags target flags r1 constant))
                          (! fixnum-sub-set-flags target flags r1 r2))
                        (arm2-check-fixnum-overflow seg flags target)))))
               (^))))))

#||
(defun arm2-fixnum-sub (seg vreg xfer num1 num2 overflow)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (when overflow
      (let* ((type *arm2-target-half-fixnum-type*))
        (when (and (arm2-form-typep num1 type)
                   (arm2-form-typep num2 type))
          (setq overflow nil))))
    (let* ((v1 (acode-fixnum-form-p num1))
           (v2 (acode-fixnum-form-p num2))
           (diff (and v1 v2 (if overflow (- v1 v2) (%i- v1 v2)))))
      (if diff
        (if (nx1-target-fixnump diff)
          (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer diff)
          (arm2-use-operator (%nx1-operator immediate) seg vreg xfer diff))
        (if (and v2 (neq v2 most-negative-fixnum))
          (arm2-use-operator (%nx1-operator %i+) seg vreg xfer num1 (make-acode (%nx1-operator fixnum) (- v2)) overflow) 
          (if (eq v2 0)
            (arm2-form seg vreg xfer num1)
            (cond
              ((null vreg)
               (arm2-form seg nil nil num1)
               (arm2-form seg nil xfer num2))
              (overflow
               (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg num1 arm::arg_y num2 arm::arg_z)
                 (ensuring-node-target (target vreg)
                   (if *arm2-open-code-inline*
                     (! fixnum-sub-overflow-inline target r1 r2)
                     (progn
                       (! fixnum-sub-overflow-ool ($ arm::arg_z) r1 r2)
                       (arm2-copy-register seg target ($ arm::arg_z)))))
                 (^)))
              (t
               (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg num1 arm::arg_y num2 arm::arg_z)
                 (ensuring-node-target (target vreg)
                   (! fixnum-sub target r1 r2))
                 (^))))))))))
||#

(defarm2 arm2-%i- %i- (seg vreg xfer num1 num2 &optional overflow)
  (arm2-fixnum-sub seg vreg xfer num1 num2 overflow))

(defarm2 arm2-fixnum-sub-no-overflow fixnum-sub-no-overflow (seg vreg xfer num1 num2)
  (arm2-fixnum-sub seg vreg xfer num1 num2 nil))

(defarm2 arm2-fixnum-sub-overflow fixnum-sub-overflow (seg vreg xfer num1 num2)
  (arm2-fixnum-sub seg vreg xfer num1 num2 t))


(defarm2 arm2-%i* %i* (seg vreg xfer num1 num2)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil num1)
      (arm2-form seg nil xfer num2))
    (multiple-value-bind (rx ry) (arm2-two-untargeted-reg-forms seg num1 arm::arg_y num2 arm::arg_z)
      (ensuring-node-target (target vreg)
        (! multiply-fixnums target rx ry))
      (^))))

(defarm2 arm2-nth-value nth-value (seg vreg xfer n form)
  (let* ((*arm2-vstack* *arm2-vstack*))
    (let* ((nreg (arm2-one-untargeted-reg-form seg n arm::arg_z)))
      (unless (acode-fixnum-form-p n)
        (! trap-unless-fixnum nreg))
      (arm2-vpush-register seg nreg))
     (arm2-multiple-value-body seg form) ; sets nargs
    (! nth-value arm::arg_z))
  (<- arm::arg_z)
  (^))

(defarm2 arm2-values values (seg vreg xfer forms)
  (if (eq (list-length forms) 1)
    (if (arm2-cd-compound-p xfer)
      (arm2-form seg vreg xfer (%car forms))
      (progn
        (arm2-form seg vreg nil (%car forms))
        (^)))
    (if (not (arm2-mv-p xfer))
      (if forms
        (arm2-use-operator (%nx1-operator prog1) seg vreg xfer forms)
        (arm2-nil seg vreg xfer))
      (progn
        (let* ((*arm2-vstack* *arm2-vstack*))
          (arm2-set-nargs seg (arm2-formlist seg forms nil)))
        (let* ((*arm2-returning-values* t))
          (^))))))

(defarm2 arm2-base-char-p base-char-p (seg vreg xfer cc form)
  (arm2-char-p seg vreg xfer cc form))

(defun arm2-char-p (seg vreg xfer cc form)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
      (! mask-base-char arm::imm0 (arm2-one-untargeted-reg-form seg form arm::arg_z))
      (arm2-test-reg-%izerop seg vreg xfer arm::imm0 cr-bit true-p
                             arm::subtag-character))))


(defarm2 arm2-let* let* (seg vreg xfer vars vals body p2decls &aux
                             (old-stack (arm2-encode-stack)))
  (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
    (with-arm-p2-declarations p2decls
      (arm2-seq-bind seg vars vals)
      (arm2-undo-body seg vreg xfer body old-stack)))
  (dolist (v vars) (arm2-close-var seg v)))

(defarm2 arm2-multiple-value-bind multiple-value-bind (seg vreg xfer vars valform body p2decls)
  (let* ((n (list-length vars))
         (vloc *arm2-vstack*)
         (*arm2-nfp-depth* *arm2-nfp-depth*)
         (nbytes (* n *arm2-target-node-size*))
         (old-stack (arm2-encode-stack)))
    (with-arm-p2-declarations p2decls
      (arm2-multiple-value-body seg valform)
      (arm2-lri seg arm::imm0 nbytes)
      (! fitvals)
      (arm2-set-vstack (%i+ vloc nbytes))
      (dolist (var vars)
        (let* ((reg (nx2-assign-register-var var)))
          (if reg
            (arm2-init-regvar seg var reg (arm2-vloc-ea vloc))
            (arm2-bind-var seg var vloc ))          
          (setq vloc (%i+ vloc *arm2-target-node-size*))))
      (arm2-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (arm2-close-var seg var)))))



(defarm2 arm2-multiple-value-prog1 multiple-value-prog1 (seg vreg xfer forms)
  (if (or (not (arm2-mv-p xfer)) (arm2-single-valued-form-p (%car forms)))
    (arm2-use-operator (%nx1-operator prog1) seg vreg xfer forms)
    (if (null (cdr forms))
      (arm2-form seg vreg xfer(car forms))
      (progn
        (let* ((*arm2-vstack* *arm2-vstack*))
          (arm2-multiple-value-body seg (%car forms))
          (arm2-open-undo $undostkblk)
          (! save-values))
        (dolist (form (cdr forms))
          (arm2-form seg nil nil form))
        (arm2-set-nargs seg 0)
        (! recover-values)
        (arm2-close-undo)
        (let* ((*arm2-returning-values* t))
          (^))))))

(defarm2 arm2-not not (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
  (arm2-compare-register-to-nil
   seg 
   vreg 
   xfer
   (arm2-one-untargeted-reg-form seg form arm::arg_z) 
   cr-bit
   true-p)))


(defarm2 arm2-%alloc-misc %make-uvector (seg vreg xfer element-count st &optional initval)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil element-count)
      (arm2-form seg nil xfer st))
    (let* ((subtag (acode-fixnum-form-p st))
           (nelements (acode-fixnum-form-p element-count))         
           (nbytes (if (and subtag nelements) (arm2-misc-byte-count subtag nelements))))
      (if (and  nbytes (null initval)
                (< (logand
                    (lognot (1- (* 2 *arm2-target-node-size*)))
                    (+ nbytes *arm2-target-node-size*
                       (1- (* 2 *arm2-target-node-size*)))) #x8000))
        (with-imm-temps () (header)
          (arm2-lri seg header (arch::make-vheader nelements subtag))
          (ensuring-node-target (target vreg)
            (! %alloc-misc-fixed target header nbytes)))
        (progn
          (if initval
            (progn
              (arm2-three-targeted-reg-forms seg element-count ($ arm::arg_x) st ($ arm::arg_y) initval ($ arm::arg_z))
              (! misc-alloc-init)
              (<- ($ arm::arg_z)))
            (progn
              (arm2-two-targeted-reg-forms seg element-count ($ arm::arg_y) st ($ arm::arg_z))
              (! misc-alloc)
              (<- ($ arm::arg_z))))))
        (^))))

(defarm2 arm2-%iasr %iasr (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil form1)
      (arm2-form seg vreg xfer form2))
    (let* ((count (acode-fixnum-form-p form1))
           (max 31))
      (declare (fixnum max))
      (ensuring-node-target (target vreg)
        (if count
          (! %iasr-c target (if (> count max) max count)
             (arm2-one-untargeted-reg-form seg form2 arm::arg_z))
          (multiple-value-bind (cnt src) (arm2-two-targeted-reg-forms seg form1 ($ arm::arg_y) form2 ($ arm::arg_z))
            (! %iasr target cnt src))))
      (^))))

(defarm2 arm2-%ilsr %ilsr (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil form1)
      (arm2-form seg vreg xfer form2))
    (let* ((count (acode-fixnum-form-p form1)))
      (ensuring-node-target (target vreg)
        (if count
          (let ((src (arm2-one-untargeted-reg-form seg form2 ($ arm::arg_z))))
            (if (<= count 31)
              (! %ilsr-c target count src)
              (!  lri target 0)))
          (multiple-value-bind (cnt src) (arm2-two-targeted-reg-forms seg form1 ($ arm::arg_y) form2 ($ arm::arg_z))
            (! %ilsr target cnt src))))
      (^))))


(defarm2 arm2-%i<> %i<> (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
    (arm2-compare seg vreg xfer form1 form2 cr-bit true-p)))

(defarm2 arm2-%natural<> %natural<> (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
    (setq cr-bit (arm-cr-bit-to-arm-unsigned-cr-bit cr-bit))
    (arm2-natural-compare seg vreg xfer form1 form2 cr-bit true-p)))

(defarm2 arm2-double-float-compare double-float-compare (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
    (with-fp-target () (r1 :double-float)
      (with-fp-target (r1) (r2 :double-float)
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg form1 r1 form2 r2)
          (arm2-compare-double-float-registers seg vreg xfer r1 r2 cr-bit true-p))))))

(defarm2 arm2-short-float-compare short-float-compare (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
    (with-fp-target () (r1 :single-float)
      (with-fp-target (r1) (r2 :single-float)
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg form1 r1 form2 r2)
          (arm2-compare-single-float-registers seg vreg xfer r1 r2 cr-bit true-p))))))
 
(eval-when (:compile-toplevel :execute)
  (defmacro defarm2-df-op (fname opname vinsn)
    `(defarm2 ,fname ,opname (seg vreg xfer f0 f1)
       (if (null vreg)
         (progn
           (arm2-form seg nil nil f0)
           (arm2-form seg vreg xfer f1))
         (with-fp-target () (r1 :double-float)
           (with-fp-target (r1) (r2 :double-float)
             (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg f0 r1 f1 r2)
               (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                        (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double)
                        (not *arm2-float-safety*))
                 (! ,vinsn vreg r1 r2)
                 (with-fp-target (r1 r2) (result :double-float)
                   (when *arm2-float-safety*
                     (! clear-pending-fpu-exceptions))
                   (! ,vinsn result r1 r2)
                   (when *arm2-float-safety*
                     (! trap-if-fpu-exception))
                   (<- result)))
               (^)))))))

  (defmacro defarm2-sf-op (fname opname vinsn)
    `(defarm2 ,fname ,opname (seg vreg xfer f0 f1)
       (if (null vreg)
         (progn
           (arm2-form seg nil nil f0)
           (arm2-form seg vreg xfer f1))
         (with-fp-target () (r1 :single-float)
           (with-fp-target (r1) (r2 :single-float)
             (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg f0 r1 f1 r2)
               (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                        (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single)
                        (not *arm2-float-safety*))
                 (! ,vinsn vreg r1 r2)
                 (with-fp-target (r1 r2) (result :single-float)
                   (when *arm2-float-safety*
                     (! clear-pending-fpu-exceptions))
                   (! ,vinsn result r1 r2)
                   (when *arm2-float-safety*
                     (! trap-if-fpu-exception))
                   (<- result)))
               (^)))))))
)

(defarm2-df-op arm2-%double-float+-2 %double-float+-2 double-float+-2)
(defarm2-df-op arm2-%double-float--2 %double-float--2 double-float--2)
(defarm2-df-op arm2-%double-float*-2 %double-float*-2 double-float*-2)
(defarm2-df-op arm2-%double-float/-2 %double-float/-2 double-float/-2)

(defarm2-sf-op arm2-%short-float+-2 %short-float+-2 single-float+-2)
(defarm2-sf-op arm2-%short-float--2 %short-float--2 single-float--2)
(defarm2-sf-op arm2-%short-float*-2 %short-float*-2 single-float*-2)
(defarm2-sf-op arm2-%short-float/-2 %short-float/-2 single-float/-2)

(defarm2 arm2-%complex-double-float+-2  %complex-double-float+-2 (seg vreg xfer x y)
  (let* ((*available-backend-fp-temps* *available-backend-fp-temps*)
         (target (if (and vreg (eql (hard-regspec-class vreg) hard-reg-class-fpr)
                          (eql (get-regspec-mode vreg) hard-reg-class-fpr-mode-complex-double-float))
                   vreg
                   (available-fp-temp  *available-backend-fp-temps* :complex-double-float))))
    (with-fp-target () (r1 :complex-double-float)
      (with-fp-target (r1) (r2 :complex-double-float)
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg x r1 y r2)
          (! complex-double-float+-2 target r1 r2)
          (unless (eq target vreg)
            (ensuring-node-target (node vreg)
              (arm2-copy-register seg node target))))))
    (^)))

(defarm2 arm2-%complex-double-float--2  %complex-double-float--2 (seg vreg xfer x y)
  (let* ((*available-backend-fp-temps* *available-backend-fp-temps*)
         (target (if (and vreg (eql (hard-regspec-class vreg) hard-reg-class-fpr)
                          (eql (get-regspec-mode vreg) hard-reg-class-fpr-mode-complex-double-float))
                   vreg
                   (available-fp-temp  *available-backend-fp-temps* :complex-double-float))))
    (with-fp-target () (r1 :complex-double-float)
      (with-fp-target (r1 target) (r2 :complex-double-float)
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg x r1 y r2)
          (! complex-double-float--2 target r1 r2)
          (unless (eq target vreg)
            (ensuring-node-target (node vreg)
              (arm2-copy-register seg node target))))))
    (^)))

(defarm2 arm2-%complex-double-float*-2  %complex-double-float*-2 (seg vreg xfer x y)
  (let* ((*available-backend-fp-temps* *available-backend-fp-temps*)
         (target (if (and vreg (eql (hard-regspec-class vreg) hard-reg-class-fpr)
                          (eql (get-regspec-mode vreg) hard-reg-class-fpr-mode-complex-double-float))
                   vreg
                   (available-fp-temp  *available-backend-fp-temps* :complex-double-float))))
    (with-fp-target (target) (r1 :complex-double-float)
      (with-fp-target (r1 target) (r2 :complex-double-float)
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg x r1 y r2)
          (! complex-double-float*-2 target r1 r2)
          (unless (eq target vreg)
            (ensuring-node-target (node vreg)
              (arm2-copy-register seg node target))))))
    (^)))

(defarm2 arm2-%complex-single-float+-2  %complex-single-float+-2 (seg vreg xfer x y)
  (let* ((*available-backend-fp-temps* *available-backend-fp-temps*)
         (target (if (and vreg (eql (hard-regspec-class vreg) hard-reg-class-fpr)
                          (eql (get-regspec-mode vreg) hard-reg-class-fpr-mode-complex-single-float))
                   vreg
                   (available-fp-temp  *available-backend-fp-temps* :complex-single-float))))
    (with-fp-target () (r1 :complex-single-float)
      (with-fp-target (r1) (r2 :complex-single-float)
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg x r1 y r2)
          (! complex-single-float+-2 target r1 r2)
          (unless (eq target vreg)
            (ensuring-node-target (node vreg)
              (arm2-copy-register seg node target))))))
    (^)))

(defarm2 arm2-%complex-single-float--2  %complex-single-float--2 (seg vreg xfer x y)
  (let* ((*available-backend-fp-temps* *available-backend-fp-temps*)
         (target (if (and vreg (eql (hard-regspec-class vreg) hard-reg-class-fpr)
                          (eql (get-regspec-mode vreg) hard-reg-class-fpr-mode-complex-single-float))
                   vreg
                   (available-fp-temp  *available-backend-fp-temps* :complex-single-float))))
    (with-fp-target () (r1 :complex-single-float)
      (with-fp-target (r1) (r2 :complex-single-float)
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg x r1 y r2)
          (! complex-single-float--2 target r1 r2)
          (unless (eq target vreg)
            (ensuring-node-target (node vreg)
              (arm2-copy-register seg node target))))))
    (^)))

(defarm2 arm2-%complex-single-float*-2  %complex-single-float*-2 (seg vreg xfer x y)
  (let* ((*available-backend-fp-temps* *available-backend-fp-temps*)
         (target (if (and vreg (eql (hard-regspec-class vreg) hard-reg-class-fpr)
                          (eql (get-regspec-mode vreg) hard-reg-class-fpr-mode-complex-single-float))
                   vreg
                   (available-fp-temp  *available-backend-fp-temps* :complex-single-float))))
    (with-fp-target (target) (r1 :complex-single-float)
      (with-fp-target (r1 target) (r2 :complex-single-float)
        (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg x r1 y r2)
          (! complex-single-float*-2 target r1 r2)
          (unless (eq target vreg)
            (ensuring-node-target (node vreg)
              (arm2-copy-register seg node target))))))
    (^)))

(defun arm2-get-float (seg vreg xfer ptr offset double-p fp-reg)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (cond ((null vreg)
           (arm2-form seg nil nil ptr)
           (arm2-form seg nil xfer offset))
          (t
           (let* ((fixoffset (acode-fixnum-form-p offset)))
             (if (and (typep fixoffset '(signed-byte 10))
                      (not (logtest fixoffset #x3)))
               (with-imm-target () (ptrreg :address)
                 (arm2-form seg ptrreg nil ptr)
                 (if double-p
                   (! mem-ref-c-double-float fp-reg ptrreg fixoffset)
                   (! mem-ref-c-single-float fp-reg ptrreg fixoffset)))
               (with-imm-target () (ptrreg :address)
                 (with-node-target (ptrreg) offsetreg
                   (multiple-value-setq (ptrreg offsetreg)
                     (arm2-two-untargeted-reg-forms seg
                                                    ptr ptrreg
                                                    offset offsetreg))
                   (progn
                         (if double-p
                           (! mem-ref-double-float fp-reg ptrreg offsetreg)
                           (! mem-ref-single-float fp-reg ptrreg offsetreg))))))
             (<- fp-reg))
           (^)))))
    

(defarm2 arm2-%get-double-float %get-double-float (seg vreg xfer ptr offset)
  (if (and vreg (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
    (arm2-get-float seg vreg xfer ptr offset t vreg)
    (with-fp-target () (fp-reg :double-float)
      (arm2-get-float seg vreg xfer ptr offset t fp-reg))))

(defarm2 arm2-%get-single-float %get-single-float (seg vreg xfer ptr offset)
  (with-fp-target () (fp-reg :single-float)
    (arm2-get-float seg vreg xfer ptr offset nil fp-reg)))

(defun arm2-set-float (seg vreg xfer ptr offset newval double-p fp-reg)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((fixoffset (acode-fixnum-form-p offset))
           (immoffset (and (typep fixoffset '(unsigned-byte 10))
                           (not (logtest fixoffset #x3)))))
      (with-imm-target () (ptr-reg :address) 
        (cond ((or (null vreg)
                   (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                        (= (get-regspec-mode vreg) (if double-p hard-reg-class-fpr-mode-double hard-reg-class-fpr-mode-single))))
               (cond (immoffset
                      (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                        (arm2-push-register
                         seg
                         (arm2-one-untargeted-reg-form seg
                                                       ptr
                                                       ptr-reg))
                        (arm2-one-targeted-reg-form seg newval fp-reg)
                        (arm2-pop-register seg ptr-reg))
                      (if double-p
                        (! mem-set-c-double-float fp-reg ptr-reg fixoffset)
                        (! mem-set-c-single-float fp-reg ptr-reg fixoffset)))
                     (t
                      (with-imm-target (ptr-reg) (offset-reg :s32)
                        (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                          (arm2-push-register
                           seg
                           (arm2-one-untargeted-reg-form seg
                                                         ptr
                                                         ptr-reg))
                          (arm2-push-register
                           seg
                           (arm2-one-untargeted-reg-form seg
                                                         offset
                                                         arm::arg_z))
                          (arm2-one-targeted-reg-form seg newval fp-reg)
                          (arm2-pop-register seg arm::arg_z)
                          (arm2-pop-register seg ptr-reg))
                        (! fixnum->signed-natural offset-reg arm::arg_z)
                        (if double-p
                          (! mem-set-double-float fp-reg ptr-reg offset-reg)
                          (! mem-set-single-float fp-reg ptr-reg offset-reg)))))
               (<- fp-reg))
              (t
               (cond (immoffset
                      (let* ((rnew ($ arm::arg_z))
                             (*arm2-nfp-depth* *arm2-nfp-depth*))
                        (arm2-push-register
                         seg
                         (arm2-one-untargeted-reg-form seg
                                                       ptr
                                                       ptr-reg))
                        (arm2-one-targeted-reg-form seg newval rnew)
                        (arm2-pop-register seg ptr-reg)
                        (with-imm-temps (ptr-reg) ()
                          (arm2-copy-register seg fp-reg rnew)
                          (if double-p
                            (! mem-set-c-double-float fp-reg ptr-reg fixoffset)
                            (! mem-set-c-single-float fp-reg ptr-reg fixoffset)))))
                     (t
                      (let* ((roffset ($ arm::arg_y))
                             (rnew ($ arm::arg_z))
                             (*arm2-nfp-depth* *arm2-nfp-depth*))
                        (arm2-push-register
                         seg
                         (arm2-one-untargeted-reg-form
                          seg
                          ptr ptr-reg))
                        (arm2-two-targeted-reg-forms seg
                                                     offset roffset
                                                     newval rnew)
                        (arm2-pop-register seg ptr-reg)
                        (with-imm-target (ptr-reg) (offset-reg :s32)
                          (with-imm-temps (ptr-reg offset-reg) ()
                            (! fixnum->signed-natural offset-reg roffset)
                            (arm2-copy-register seg fp-reg rnew))
                          (if double-p
                            (! mem-set-double-float fp-reg ptr-reg offset-reg)
                            (! mem-set-single-float fp-reg ptr-reg offset-reg))))))
               (<- arm::arg_z)))
        (^)))))

(defarm2 arm2-%set-double-float %set-double-float (seg vreg xfer ptr offset newval)
  (with-fp-target () (fp-reg :double-float)
    (arm2-set-float seg vreg xfer ptr offset newval t fp-reg)))
      
(defarm2 arm2-%set-single-float %set-single-float (seg vreg xfer ptr offset newval)
  (with-fp-target () (fp-reg :single-float)
    (arm2-set-float seg vreg xfer ptr offset newval nil fp-reg)))

(defarm2 arm2-immediate-get-ptr immediate-get-ptr (seg vreg xfer ptr offset)
  (let* ((triv-p (arm2-trivial-p offset))
         (*arm2-nfp-depth* *arm2-nfp-depth*)
         (dest vreg)
         (offval (acode-fixnum-form-p offset)))
    (cond ((not vreg)
           (arm2-form seg nil nil ptr)
           (arm2-form seg nil xfer offset))
          (t
           (and offval (%i> (integer-length offval) 11) (setq offval nil))
           (if offval
             (let* ((src (arm2-macptr-arg-to-reg seg ptr ($ arm::imm0 :mode :address))))
               (! mem-ref-c-natural dest src offval))
             (let* ((src (arm2-macptr-arg-to-reg seg ptr ($ arm::imm0 :mode :address))))
               (if triv-p
                 (with-imm-temps (src) (x)
                   (if (acode-fixnum-form-p offset)
                     (arm2-lri seg x (acode-fixnum-form-p offset))
                     (! fixnum->signed-natural x (arm2-one-untargeted-reg-form seg offset arm::arg_z)))
                   (! mem-ref-natural dest src x))
                 (progn
                   (arm2-push-register seg src)
                   (let* ((oreg (arm2-one-untargeted-reg-form seg offset arm::arg_z)))
                     (with-imm-temps () (src x)
                       (arm2-pop-register seg src)
                       (! fixnum->signed-natural x oreg)
                       (! mem-ref-natural dest src x))))))) 
           (^)))))

(defarm2 arm2-get-bit %get-bit (seg vreg xfer ptr offset)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil ptr)
      (arm2-form seg nil ptr nil))
    (let* ((offval (acode-fixnum-form-p offset))
           (byte-index (if offval (ash offval -3)))
           (bit-shift (if (and byte-index (< byte-index #x8000))
                        (logand 31 (+ 25 (logand offval 7))))))
      (if bit-shift
        (with-imm-target () (src-reg :address)
          (arm2-one-targeted-reg-form seg ptr src-reg)
          (if (node-reg-p vreg)
            (! mem-ref-c-bit-fixnum vreg src-reg byte-index (logand 31 (+ bit-shift
                                                                           *arm2-target-fixnum-shift*)))
            (with-imm-target ()           ;OK if src-reg & dest overlap
              (dest :u8)
              (! mem-ref-c-bit dest src-reg  byte-index bit-shift)
              (<- dest))))
        (let* ((triv-p (arm2-trivial-p offset))
               (offset-reg nil)
               (*arm2-nfp-depth* *arm2-nfp-depth*))
          (with-imm-target ()
            (src-reg :address)
            (arm2-one-targeted-reg-form seg ptr src-reg)
            (unless triv-p
              (arm2-push-register seg src-reg))
            (setq offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z))
            (unless triv-p
              (arm2-pop-register seg src-reg))
            (if (node-reg-p vreg)
              (! mem-ref-bit-fixnum vreg src-reg offset-reg)
              (with-imm-target ()
                (dest :u8)
                (! mem-ref-bit dest src-reg offset-reg)
                (<- dest))))))))
  (^))
    
      
                                      
;;; This returns an unboxed object, unless the caller wants to box it.
(defarm2 arm2-immediate-get-xxx immediate-get-xxx (seg vreg xfer bits ptr offset)
  (declare (fixnum bits))
  (let* ((fixnump (logbitp 6 bits))
         (signed (logbitp 5 bits))
         (size (logand 15 bits))
         (triv-p (arm2-trivial-p offset))
         (offval (acode-fixnum-form-p offset)))
    (declare (fixnum size))
    (cond ((null vreg)
           (arm2-form seg nil nil ptr)
           (arm2-form seg nil xfer offset))
          (t 
           (and offval (%i> (integer-length offval) 11) (setq offval nil))
           (cond
             (fixnump
              (with-imm-target () (dest :signed-natural)
                (cond
                  (offval
                    (with-imm-target () (src-reg :address)
                      (arm2-one-targeted-reg-form seg ptr src-reg)
                      (! mem-ref-c-fullword dest src-reg offval)))
                  (t
                   (with-imm-target () (src-reg :address)
                     (with-imm-target (src-reg) (offset-reg :signed-natural)
                       (arm2-one-targeted-reg-form seg ptr src-reg)
                       (if triv-p
                         (if (acode-fixnum-form-p offset)
                           (arm2-lri seg offset-reg (acode-fixnum-form-p offset))
                           (! fixnum->signed-natural offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z)))
                         (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                           (arm2-push-register seg src-reg)
                           (! fixnum->signed-natural offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z))
                           (arm2-pop-register seg src-reg)))
                       (! mem-ref-fullword dest src-reg offset-reg)))))
                (if (node-reg-p vreg)
                  (! box-fixnum vreg dest)
                  (<- dest))))
             (signed
              (with-imm-target () (dest :signed-natural)
               (cond
                 (offval
                  (with-imm-target (dest) (src-reg :address)
                   (arm2-one-targeted-reg-form seg ptr src-reg)
                     (case size
                       ;;(8 (! mem-ref-c-signed-doubleword dest src-reg offval))
                       (4 (! mem-ref-c-signed-fullword dest src-reg offval))
                       (2 (! mem-ref-c-s16 dest src-reg offval))
                       (1 (! mem-ref-c-s8 dest src-reg offval)))))
                 (t
                  (with-imm-target () (src-reg :address)
                    (with-imm-target (src-reg) (offset-reg :signed-natural)
                     (arm2-one-targeted-reg-form seg ptr src-reg)
                     (if triv-p
                       (if (acode-fixnum-form-p offset)
                         (arm2-lri seg offset-reg (acode-fixnum-form-p offset))
                         (! fixnum->signed-natural offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z)))
                       (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                         (arm2-push-register seg src-reg)
                         (! fixnum->signed-natural offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z))
                         (arm2-pop-register seg src-reg)))
                  (case size
                    ;;(8 (! mem-ref-signed-doubleword dest src-reg offset-reg))
                    (4 (! mem-ref-signed-fullword dest src-reg offset-reg))
                    (2 (! mem-ref-s16 dest src-reg offset-reg))
                    (1 (! mem-ref-s8 dest src-reg offset-reg)))))))
               (if (node-reg-p vreg)
                 (case size
                   ((1 2) (! box-fixnum vreg dest))
                   (4 (<- dest))
                   (8 (<- dest)))
                 (<- dest))))
             (t
              (with-imm-target () (dest :natural)
               (cond
                 (offval
                  (with-imm-target (dest) (src-reg :address)
                    (arm2-one-targeted-reg-form seg ptr src-reg)
                    (case size
                      ;;(8 (! mem-ref-c-doubleword dest src-reg offval))
                      (4 (! mem-ref-c-fullword dest src-reg offval))
                      (2 (! mem-ref-c-u16 dest src-reg offval))
                      (1 (! mem-ref-c-u8 dest src-reg offval)))))
                 (t
                  (with-imm-target () (src-reg :address)
                    (with-imm-target (src-reg) (offset-reg :signed-natural)
                     (arm2-one-targeted-reg-form seg ptr src-reg)
                     (if triv-p
                       (if (acode-fixnum-form-p offset)
                         (arm2-lri seg offset-reg (acode-fixnum-form-p offset))
                         (! fixnum->signed-natural offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z)))
                       (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
                         (arm2-push-register seg src-reg)
                         (! fixnum->signed-natural offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z))
                         (arm2-pop-register seg src-reg)))
                  (case size
                    ;;(8 (! mem-ref-doubleword dest src-reg offset-reg))
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

(defarm2 arm2-let let (seg vreg xfer vars vals body p2decls)
  (let* ((old-stack (arm2-encode-stack))
         (*arm2-nfp-depth* *arm2-nfp-depth*)
         (val nil)
         (bits nil)
         (valcopy vals))
    (with-arm-p2-declarations p2decls
      (dolist (var vars)
        (setq val (%car valcopy))
        (cond ((or (%ilogbitp $vbitspecial (setq bits (nx-var-bits var)))
                   (and (var-nvr var)
                        (dolist (val (%cdr valcopy))
                          (unless (arm2-trivial-p val) (return t)))))
               (let* ((pair (cons (arm2-vloc-ea *arm2-vstack*) nil)))
                 (%rplaca valcopy pair)
                 (if (and (%ilogbitp $vbitdynamicextent bits)
                          (progn
                            (setq val 
                                  (arm2-dynamic-extent-form seg (arm2-encode-stack) val))
                            (arm2-load-ea-p val)))
                   (progn
                     (%rplaca pair (arm2-vloc-ea *arm2-vstack*))
                     (arm2-vpush-register seg val))
                 (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg val arm::arg_z)))
                 ))
              (t (arm2-seq-bind-var seg var val)
                 (%rplaca valcopy nil)))
        (setq valcopy (%cdr valcopy)))
      (dolist (var vars)
        (declare (list val))
        (when (setq val (pop vals))
          (if (%ilogbitp $vbitspecial (nx-var-bits var))
            (progn
              (arm2-dbind seg (car val) (var-name var))
              (arm2-set-var-ea seg var (arm2-vloc-ea (- *arm2-vstack* *arm2-target-node-size*))))
            (arm2-seq-bind-var seg var (car val)))))
      (arm2-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (arm2-close-var seg var)))))

(defarm2 arm2-closed-function closed-function (seg vreg xfer afunc)
  (arm2-make-closure seg afunc nil)
  (when vreg (<- arm::arg_z))
  (^))

(defarm2 arm2-flet flet (seg vreg xfer vars afuncs body p2decls)
  (if (dolist (afunc afuncs)
        (unless (eql 0 (afunc-fn-refcount afunc))
          (return t)))
    (arm2-seq-fbind seg vreg xfer vars afuncs body p2decls)
    (with-arm-p2-declarations p2decls
      (arm2-form seg vreg xfer body))))

(defarm2 arm2-labels labels (seg vreg xfer vars afuncs body p2decls)
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
        (let* ((i 3)
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
      (arm2-seq-fbind seg vreg xfer (nreverse real-vars) (nreverse real-funcs) body p2decls)
      (let* ((old-stack (arm2-encode-stack)))
        (setq real-vars (nreverse real-vars) real-funcs (nreverse real-funcs))
        (with-arm-p2-declarations p2decls
          (dolist (var real-vars)
            (arm2-seq-bind-var seg var (nx1-afunc-ref (pop real-funcs))))
          (dolist (ref fwd-refs)
            (let ((ea (var-ea (pop ref))))
              (arm2-addrspec-to-reg seg ea arm::temp0)
              (dolist (r ref)
                (let* ((v-ea (var-ea (cdr r))))
                  (let* ((val-reg (if (eq v-ea ea)
                                    arm::temp0
                                    (progn
                                      (arm2-addrspec-to-reg seg v-ea arm::temp1)
                                      arm::temp1))))
                    (! misc-set-c-node val-reg arm::temp0 (car r)))))))
          (arm2-undo-body seg vreg xfer body old-stack)
          (dolist (var real-vars)
            (arm2-close-var seg var)))))))




(defarm2 arm2-local-return-from local-return-from (seg vreg xfer blocktag value)
  (declare (ignorable vreg xfer))
  (let* ((*arm2-undo-count* *arm2-undo-count*)
         (tagdata (car blocktag))
         (cur-stack (arm2-encode-stack))
         (dest-vd (caar tagdata))
         (dest-cd (cdar tagdata))
         (mv-p (arm2-mvpass-p dest-cd))
         (dest-stack  (cdr tagdata))
         (need-break (neq cur-stack dest-stack)))
    (let* ((*arm2-vstack* *arm2-vstack*)
           (*arm2-cstack* *arm2-cstack*))
      (if 
        (or
         (eq dest-cd $backend-return)
         (and mv-p 
              (eq (arm2-encoding-undo-count cur-stack)
                  (arm2-encoding-undo-count dest-stack)) 
              (eq (arm2-encoding-cstack-depth cur-stack)
                  (arm2-encoding-cstack-depth dest-stack))))
        (arm2-form seg dest-vd dest-cd value)
        (if mv-p
          (progn
            (arm2-multiple-value-body seg value)
            (let* ((*arm2-returning-values* :pass))
              (arm2-nlexit seg dest-cd (%i- *arm2-undo-count* (arm2-encoding-undo-count dest-stack)))
              (arm2-branch seg dest-cd vreg)))
          (progn
            (arm2-form 
             seg
             (if need-break (if dest-vd arm::arg_z) dest-vd) 
             (if need-break nil dest-cd)
             value)
            (when need-break
              (arm2-unwind-set seg dest-cd dest-stack)
              (when dest-vd (arm2-copy-register seg dest-vd arm::arg_z))
              (arm2-branch seg dest-cd dest-vd))))))
    (arm2-unreachable-store)))

(defarm2 arm2-inherited-arg inherited-arg (seg vreg xfer arg)
  (when vreg
    (arm2-addrspec-to-reg seg (arm2-ea-open (var-ea arg)) vreg))
  (^))


(defarm2 arm2-%lisp-word-ref %lisp-word-ref (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (arm2-form seg nil nil base)
           (arm2-form seg nil xfer offset))
          ((typep fixoffset '(signed-byte 10))
           (ensuring-node-target (target vreg)
             (! lisp-word-ref-c target 
                (arm2-one-untargeted-reg-form seg base arm::arg_z) 
                (ash fixoffset *arm2-target-fixnum-shift*)))
           (^))
          (t (multiple-value-bind (breg oreg)
                                  (arm2-two-untargeted-reg-forms seg base arm::arg_y offset arm::arg_z)
               (ensuring-node-target (target vreg)
                 (! lisp-word-ref target breg oreg))
               (^))))))

(defarm2 arm2-%fixnum-ref %fixnum-ref (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (arm2-form seg nil nil base)
           (arm2-form seg nil xfer offset))
          ((typep fixoffset '(signed-byte 16))
           (ensuring-node-target (target vreg)
             (! lisp-word-ref-c target 
                (arm2-one-untargeted-reg-form seg base arm::arg_z) 
                fixoffset))
           (^))
          (t (multiple-value-bind (breg oreg)
                                  (arm2-two-untargeted-reg-forms seg base arm::arg_y offset arm::arg_z)
               (with-imm-target () (otemp :s32)
                 (! fixnum->signed-natural otemp oreg)
                 (ensuring-node-target (target vreg)
                   (! lisp-word-ref target breg otemp)))
               (^))))))

(defarm2 arm2-%fixnum-ref-natural %fixnum-ref-natural (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (arm2-form seg nil nil base)
           (arm2-form seg nil xfer offset))
          ((typep fixoffset '(signed-byte 16))
           (with-imm-target () (val :natural)
             (! lisp-word-ref-c val
                (arm2-one-untargeted-reg-form seg base arm::arg_z) 
                fixoffset)
             (<- val))
           (^))
          (t (multiple-value-bind (breg oreg)
		 (arm2-two-untargeted-reg-forms seg base arm::arg_y offset arm::arg_z)
               (with-imm-target () (otemp :s32)
                 (! fixnum->signed-natural otemp oreg)
                 (with-imm-target () (val :natural)
                   (! lisp-word-ref val breg otemp)
                   (<- val)))
               (^))))))

(defarm2 arm2-int>0-p int>0-p (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
    (arm2-one-targeted-reg-form seg form ($ arm::arg_z))
    (! integer-sign)
    (arm2-test-reg-%izerop seg vreg xfer arm::imm0 cr-bit true-p 0)))


(defarm2 arm2-throw throw (seg vreg xfer tag valform )
  (declare (ignorable vreg xfer))
  (let* ((*arm2-vstack* *arm2-vstack*))
    (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg tag arm::arg_z))
    (if (arm2-trivial-p valform)
      (progn
        (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg valform arm::arg_z))
        (arm2-set-nargs seg 1))
      (arm2-multiple-value-body seg valform))
    (! throw)))

;;; This (and unwind-protect and things like that) are a little funky in that
;;; they have no good way of specifying the exit-point.  The bad way is to
;;; follow the call to the catch-frame-creating subprim with a branch to that
;;; exit-point; the subprim returns to the following instruction.
;;; If the compiler ever gets smart about eliminating dead code, it has to
;;; be careful not to consider the block following the jump to be dead.
;;; Use a vinsn other than JUMP to reference the label.
(defarm2 arm2-catch catch (seg vreg xfer tag valform)
  (let* ((tag-label (backend-get-next-label))
         (mv-pass (arm2-mv-p xfer)))
    (arm2-one-targeted-reg-form seg tag ($ arm::arg_z))
    (! lock-constant-pool)
    (if mv-pass
      (! mkcatchmv)
      (! mkcatch1v))
    (! non-barrier-jump (aref *backend-labels* tag-label))
    (! unlock-constant-pool)
    (arm2-open-undo)
    (if mv-pass
      (arm2-multiple-value-body seg valform)  
      (arm2-one-targeted-reg-form seg valform ($ arm::arg_z)))
    (arm2-lri seg arm::imm0 (ash 1 *arm2-target-fixnum-shift*))
    (if mv-pass
      (! nthrowvalues)
      (! nthrow1value))
    (arm2-close-undo)
    (@ tag-label)
    (unless mv-pass (if vreg (<- arm::arg_z)))
    (let* ((*arm2-returning-values* mv-pass)) ; nlexit keeps values on stack
      (^))))


(defarm2 arm2-fixnum-overflow fixnum-overflow (seg vreg xfer form)
  (destructuring-bind (op n0 n1) (acode-unwrapped-form form)
    (arm2-use-operator op seg vreg xfer n0 n1 (make-nx-t))))



(defarm2 arm2-%aref2 simple-typed-aref2 (seg vreg xfer typename arr i j &optional dim0 dim1)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil arr)
      (arm2-form seg nil nil i)
      (arm2-form seg nil xfer j))
    (let* ((type-keyword (acode-immediate-operand typename))
           (fixtype (nx-lookup-target-uvector-subtag type-keyword ))
           (safe (unless *arm2-reckless* fixtype))
           (dim0 (acode-fixnum-form-p dim0))
           (dim1 (acode-fixnum-form-p dim1)))
      (arm2-aref2 seg vreg xfer arr i j safe type-keyword dim0 dim1))))


(defarm2 arm2-general-aref2 general-aref2 (seg vreg xfer arr i j)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
	 (dims (and atype (array-ctype-dimensions atype)))
         (simple (and atype (not (array-ctype-complexp atype))))
	 (keyword (and atype
		       (or (eq dims '*)
			   (and (typep dims 'list)
				(= 2 (length dims))))
                 
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
	   (when (eq dims '*)
	     (setq dims nil))
           (let* ((dim0 (car dims))
                  (dim1 (cadr dims)))
             (arm2-aref2 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         (if *arm2-reckless*
                           (make-nx-nil)
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword        ;(make-acode (%nx1-operator immediate) )
                         (if (typep dim0 'fixnum) dim0) (if (typep dim1 'fixnum) dim1) simple)))
          (t
           (arm2-three-targeted-reg-forms seg
                                          arr ($ arm::arg_x)
                                          i ($ arm::arg_y)
                                          j ($ arm::arg_z))
           (arm2-fixed-call-builtin seg vreg xfer '.SParef2))))  )


(defarm2 arm2-%aref3 simple-typed-aref3 (seg vreg xfer typename arr i j k &optional dim0 dim1 dim2)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil arr)
      (arm2-form seg nil nil i)
      (arm2-form seg nil nil j)
      (arm2-form seg nil xfer k)))
  (let* ((type-keyword (acode-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword ))
         (safe (unless *arm2-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1))
         (dim2 (acode-fixnum-form-p dim2)))
    (arm2-aref3 seg vreg xfer arr i j k safe type-keyword dim0 dim1 dim2)))

(defarm2 arm2-general-aref3 general-aref3 (seg vreg xfer arr i j k)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
	 (dims (and atype (array-ctype-dimensions atype)))
         (simple (and atype (not (array-ctype-complexp atype))))
         (keyword (and atype
		       (or (eq dims '*)
			   (and (typep dims 'list)
				(= 3 (length dims))))               
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
             (arm2-aref3 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         k
                         (if *arm2-reckless*
                           (make-nx-nil)
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword ;(make-acode (%nx1-operator immediate) )
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1)
                         (if (typep dim2 'fixnum) dim2)
                         simple)))
          (t
           (arm2-four-targeted-reg-forms seg
                                         arr ($ arm::temp0)
                                         i ($ arm::arg_x)
                                         j ($ arm::arg_y)
                                         k ($ arm::arg_z))
           (arm2-fixed-call-builtin seg vreg xfer '.SParef3)))))

(defarm2 arm2-%aset2 simple-typed-aset2 (seg vreg xfer typename arr i j new &optional dim0 dim1)
  (let* ((type-keyword (acode-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword ))
         (safe (unless *arm2-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1)))
    (arm2-aset2 seg vreg xfer arr i j new safe type-keyword dim0 dim1))
)

(defarm2 arm2-general-aset2 general-aset2 (seg vreg xfer arr i j new)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
	 (dims (and atype (array-ctype-dimensions atype)))
         (simple (and atype (not (array-ctype-complexp atype))))
         (keyword (and atype
		       (or (eq dims '*)
			   (and (typep dims 'list)
				(= 2 (length dims))))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
	   (when (eq dims '*)
	     (setq dims nil))
           (let* ((dim0 (car dims))
                  (dim1 (cadr dims)))
             (arm2-aset2 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         new
                         (unless *arm2-reckless*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1)
                         simple)))
          (t
           (arm2-four-targeted-reg-forms seg
                                         arr ($ arm::temp0)
                                         i ($ arm::arg_x)
                                         j ($ arm::arg_y)
                                         new ($ arm::arg_z))
           (arm2-fixed-call-builtin seg vreg xfer '.SPaset2)))))


(defarm2 arm2-general-aset3 general-aset3 (seg vreg xfer arr i j k new)
  (let* ((*arm2-nfp-depth* *arm2-nfp-depth*)
         (atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
	 (dims (and atype (array-ctype-dimensions atype)))
         (simple (and atype (not (array-ctype-complexp atype))))
         (keyword (and atype
		       (or (eq dims '*)
			   (unless (atom dims)
			     (= 3 (length dims))))
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
             (arm2-aset3 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         k
                         new
                         (unless *arm2-reckless*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1)
                         (if (typep dim2 'fixnum) dim2)
                         simple)))
          (t
           (arm2-push-register seg (arm2-one-untargeted-reg-form seg arr ($ arm::arg_z)))
           (arm2-four-targeted-reg-forms seg
                                         i ($ arm::temp0)
                                         j ($ arm::arg_x)
                                         k ($ arm::arg_y)
                                         new ($ arm::arg_z))
           (arm2-pop-register seg ($ arm::temp1))
           (arm2-fixed-call-builtin seg vreg xfer '.SPaset3)))))

(defarm2 arm2-%aset3 simple-typed-aset3 (seg vreg xfer typename arr i j k new &optional dim0 dim1 dim2)
  (let* ((type-keyword (acode-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword))
         (safe (unless *arm2-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1))
         (dim2 (acode-fixnum-form-p dim2)))
    (arm2-aset3 seg vreg xfer arr i j k new safe type-keyword dim0 dim1 dim2)))



(defarm2 arm2-%typed-uvref %typed-uvref (seg vreg xfer subtag uvector index)
  (let* ((type-keyword
          (let* ((fixtype (acode-fixnum-form-p subtag)))
            (if fixtype
              (nx-target-uvector-subtag-name fixtype)
              (acode-immediate-operand subtag)))))
    (if type-keyword
      (arm2-vref seg vreg xfer type-keyword uvector index (unless *arm2-reckless* (nx-lookup-target-uvector-subtag type-keyword)))
      (progn
        (arm2-three-targeted-reg-forms seg subtag ($ arm::arg_x) uvector ($ arm::arg_y) index ($ arm::arg_z))
        (! subtag-misc-ref)
        (when vreg (<- ($ arm::arg_z)))
        (^)) )))

(defarm2 arm2-%typed-uvset %typed-uvset (seg vreg xfer subtag uvector index newval)
  (let* ((type-keyword
          (let* ((fixtype (acode-fixnum-form-p subtag)))
            (if fixtype
              (nx-target-uvector-subtag-name fixtype)
              (acode-immediate-operand subtag)))))
    (if type-keyword
      (arm2-vset seg vreg xfer type-keyword uvector index newval (unless *arm2-reckless* (nx-lookup-target-uvector-subtag type-keyword)))
      (progn
        (arm2-four-targeted-reg-forms seg
                                      subtag ($ arm::temp0)
                                      uvector ($ arm::arg_x)
                                      index ($ arm::arg_y)
                                      newval ($ arm::arg_z))

        (! subtag-misc-set)
        (when vreg (<- ($ arm::arg_z)))
        (^)))))

(defarm2 arm2-%macptrptr% %macptrptr% (seg vreg xfer form)
  (with-imm-target () (target :address)
    (arm2-one-targeted-reg-form seg form (or vreg target)))
  (^))
           

;;; cons a macptr, unless "vreg" is an immediate register of mode :address.
(defarm2 arm2-%consmacptr% %consmacptr% (seg vreg xfer form)
  (cond ((null vreg) (arm2-form seg nil xfer form))
        ((eql (get-regspec-mode vreg) hard-reg-class-gpr-mode-address)
         (arm2-form seg vreg xfer form))
        (t         
         (with-imm-target () (temp :address)
           (<- (arm2-one-targeted-reg-form seg form temp))
           (^)))))

(defarm2 arm2-%immediate-ptr-to-int %immediate-ptr-to-int (seg vreg xfer form)
  (if (null vreg)
    (arm2-form seg nil xfer form)
    (with-imm-target () (address-reg :address)
      (arm2-form seg address-reg nil form)
      (<- (set-regspec-mode address-reg (gpr-mode-name-value :natural)))
      (^))))

(defarm2 arm2-%immediate-int-to-ptr %immediate-int-to-ptr (seg vreg xfer form)
  (if (null vreg)
    (arm2-form seg nil xfer form)
    (progn
      (unless (logbitp (hard-regspec-value vreg) arm-imm-regs)
        (compiler-bug "I give up.  When will I get this right ?"))
      (let* ((natural-reg (arm2-one-targeted-reg-form seg 
                                                      form
                                                      ($ vreg :mode :natural))))
        (<- natural-reg)
        (^)))))


(defarm2 arm2-%function %function (seg vreg xfer sym)
  (when vreg
    (let* ((symreg (arm2-one-untargeted-reg-form seg (make-acode (%nx1-operator immediate)
                                                                 (arm2-symbol-entry-locative sym)) arm::arg_z)))
      (with-node-temps (vreg symreg) (val)
        (! symbol-function val symreg)
        (<- val))))
  (^))

(defarm2 arm2-%unbound-marker %unbound-marker (seg vreg xfer)
  (when vreg       
    (ensuring-node-target (target vreg)
      (arm2-lri seg target arm::unbound-marker)))
  (^))

(defarm2 arm2-slot-unbound-marker %slot-unbound-marker (seg vreg xfer)
  (when vreg    
    (ensuring-node-target (target vreg)
      (arm2-lri seg target arm::slot-unbound-marker)))
  (^))

(defarm2 arm2-illegal-marker %illegal-marker (seg vreg xfer)
  (when vreg    
    (ensuring-node-target (target vreg)
      (arm2-lri seg target arm::illegal-marker)))
  (^))

(defarm2 arm2-lambda-bind lambda-bind (seg vreg xfer vals req rest keys-p auxen body p2decls)
  (let* ((old-stack (arm2-encode-stack))
         (nreq (list-length req))
         (rest-arg (nthcdr nreq vals))
         (apply-body (arm2-eliminate-&rest body rest keys-p auxen rest-arg)))
    (arm2-seq-bind seg req vals)
    (when apply-body (setq rest nil body apply-body))
    (let*
      ((vloc *arm2-vstack*)
       (restloc vloc)
       (nvloc (progn (if (or rest keys-p) (arm2-formlist seg rest-arg)) *arm2-vstack*)))
      (with-arm-p2-declarations p2decls
        (when rest
          (when keys-p
            (until (eq restloc nvloc)
              (with-node-temps () (temp)
                (arm2-stack-to-register seg (arm2-vloc-ea restloc) temp)
                (arm2-vpush-register seg temp))
              (setq restloc (%i+ restloc *arm2-target-node-size*))))
          (arm2-set-nargs seg (length rest-arg))
          (arm2-set-vstack restloc)
          (if (%ilogbitp $vbitdynamicextent (nx-var-bits rest))
            (progn
              (! stack-cons-list)
              (arm2-open-undo $undostkblk))
            (! list))
          (arm2-vpush-register seg arm::arg_z))
        (when rest (arm2-bind-var seg rest restloc))
        (destructuring-bind (vars inits) auxen
          (while vars
            (let ((val (%car inits))) 
              (if (fixnump val)
                (progn
                  (when rest (setq val (%i+ (%i+ val val) 1)))
                  (arm2-bind-var seg (%car vars) (%i+ vloc (* val *arm2-target-node-size*))))
                (arm2-seq-bind-var seg (%car vars) val)))
            (setq vars (%cdr vars) inits (%cdr inits))))
        (arm2-undo-body seg vreg xfer body old-stack)
        (dolist (var req) (arm2-close-var seg var))
        (when rest (arm2-close-var seg rest))
        (dolist (var (%car auxen)) (arm2-close-var seg var))))))

(macrolet 
  ((def-arm2-require (function op &optional (vinsn op))
     `(defarm2 ,function ,op (seg vreg xfer val)
        (let* ((val-reg (arm2-one-untargeted-reg-form 
                         seg 
                         val 
                         (if (eq vreg arm::arg_z) arm::arg_y arm::arg_z))))
          (! ,vinsn val-reg)
          (when vreg (<- val-reg))
          (^)))))
  (def-arm2-require arm2-require-simple-vector require-simple-vector)
  (def-arm2-require arm2-require-simple-string require-simple-string)
  (def-arm2-require arm2-require-integer require-integer)
  (def-arm2-require arm2-require-fixnum require-fixnum)
  (def-arm2-require arm2-require-real require-real)
  (def-arm2-require arm2-require-list require-list)
  (def-arm2-require arm2-require-character require-character)
  (def-arm2-require arm2-require-number require-number)
  (def-arm2-require arm2-require-symbol require-symbol)
  (def-arm2-require arm2-require-s8 require-s8)
  (def-arm2-require arm2-require-s8 require-u8)
  (def-arm2-require arm2-require-s8 require-s16)
  (def-arm2-require arm2-require-s8 require-u16)
  (def-arm2-require arm2-require-s8 require-s32)
  (def-arm2-require arm2-require-s8 require-u32)
  (def-arm2-require arm2-require-s8 require-s64)
  (def-arm2-require arm2-require-s8 require-u64))

(defun arm2-typechecked-form (seg vreg xfer typespec form)
  (with-arm-local-vinsn-macros (seg vreg xfer)
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
        (arm2-use-operator op seg vreg xfer form)
        (if (or (eq typespec t)
                (eq typespec '*))
          (arm2-form seg vreg xfer form)
          (let* ((ok (backend-get-next-label)))
            (arm2-one-targeted-reg-form seg form ($ arm::arg_y))
            (arm2-store-immediate seg typespec ($ arm::arg_z))
            (arm2-store-immediate seg 'typep ($ arm::fname))
            (arm2-set-nargs seg 2)
            (arm2-vpush-register seg ($ arm::arg_y))
            (! call-known-symbol ($ arm::arg_z))
	    (with-crf-target () crf
               (! compare-to-nil crf ($ arm::arg_z))
	       (arm2-vpop-register seg ($ arm::arg_y))
	       (! cbranch-false (aref *backend-labels* ok) crf arm::arm-cond-eq))
            (arm2-lri seg ($ arm::arg_x) (ash $XWRONGTYPE *arm2-target-fixnum-shift*))
            (arm2-store-immediate seg typespec ($ arm::arg_z))
            (arm2-set-nargs seg 3)
            (! ksignalerr)
            (@ ok)
            (<- ($ arm::arg_y))
            (^)))))))

(defarm2 arm2-%badarg2 %badarg2 (seg vreg xfer badthing goodthing)
  (arm2-two-targeted-reg-forms seg badthing ($ arm::arg_y) goodthing ($ arm::arg_z))
  (arm2-lri seg ($ arm::arg_x) (ash $XWRONGTYPE *arm2-target-fixnum-shift*))
  (arm2-set-nargs seg 3)
  (! ksignalerr)
  (<- nil)
  (^))  
          
(defarm2 arm2-%set-sbchar %set-sbchar (seg vreg xfer string index value)
  (arm2-vset 
   seg 
   vreg 
   xfer 
   :simple-string 
   string 
   index
   value 
   (unless *arm2-reckless* (nx-lookup-target-uvector-subtag :simple-string))))


;;; If we didn't use this for stack consing, turn it into a call.  Ugh.

(defarm2 arm2-make-list make-list (seg vreg xfer size initial-element)
  (arm2-form seg vreg xfer (make-acode (%nx1-operator call)
                                       (make-acode (%nx1-operator immediate) 'make-list)
                                       (list nil
                                             (list initial-element 
                                                   (make-acode (%nx1-operator immediate)
                                                               :initial-element)
                                                   size)))))


(defarm2 arm2-setq-free setq-free (seg vreg xfer sym val)
  (let* ((rsym ($ arm::arg_y))
         (rval ($ arm::arg_z)))
    (arm2-one-targeted-reg-form seg val rval)
    (arm2-immediate seg rsym nil (arm2-symbol-value-cell sym))
    (! setqsym)
    (<- rval)
    (^)))

(defarm2 arm2-%setf-macptr %setf-macptr (seg vreg xfer x y)
  (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg x arm::arg_z))
  (with-imm-target () (src-reg :address)
    (arm2-one-targeted-reg-form seg y src-reg)
    (arm2-vpop-register seg arm::arg_z)
    (unless (or *arm2-reckless* (arm2-form-typep x 'macptr))
      (with-imm-temps (src-reg) ()
        (! trap-unless-macptr arm::arg_z)))
    (! set-macptr-address src-reg arm::arg_z)
    (<- arm::arg_z)
    (^)))

(defarm2 arm2-%setf-double-float %setf-double-float (seg vref xfer fnode fval)
  (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg fnode arm::arg_z))
  (let* ((target ($ arm::d0 :class :fpr :mode :double-float))
         (node ($ arm::arg_z)))
    (arm2-one-targeted-reg-form seg fval target)
    (arm2-vpop-register seg node)
    (unless (or *arm2-reckless* (arm2-form-typep fnode 'double-float))
      (! trap-unless-double-float node))
    (! store-double node target)
    (<- node)
    (^)))

(defarm2 arm2-%setf-short-float %setf-short-float (seg vreg xfer fnode fval)
  (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg fnode arm::arg_z))
  (let* ((target ($ arm::s0 :class :fpr :mode :single-float))
         (freg ($ arm::arg_z)))
    (arm2-one-targeted-reg-form seg fval target)
    (arm2-vpop-register seg freg)
    (unless (or *arm2-reckless* (arm2-form-typep fnode 'short-float))
      (! trap-unless-single-float freg))
    (! store-single freg target)
    (<- freg)
    (^)))

    

(defarm2 arm2-unwind-protect unwind-protect (seg vreg xfer protected-form cleanup-form)
  (let* ((cleanup-label (backend-get-next-label))
         (protform-label (backend-get-next-label))
         (old-stack (arm2-encode-stack)))
    (! lock-constant-pool)
    (! nmkunwind)
    (arm2-open-undo $undointerruptlevel)
    (arm2-adjust-vstack (* 3 *arm2-target-node-size*))    
    (! non-barrier-jump (aref *backend-labels* cleanup-label))
    (-> protform-label)
    (! unlock-constant-pool)
    (@ cleanup-label)
    (let* ((*arm2-vstack* *arm2-vstack*)
           (*arm2-cstack* (%i+ *arm2-cstack* arm::lisp-frame.size)))
      ;;(arm2-open-undo $undostkblk)      ; saved fpr vector
      (arm2-open-undo $undostkblk)      ; tsp frame created by nthrow.
      (! save-cleanup-context)
      (setq *arm2-cstack* (%i+ *arm2-cstack*
                               arm::lisp-frame.size))       ; the frame we just pushed
      (arm2-form seg nil nil cleanup-form)
      (arm2-close-undo)
      ;;(arm2-close-undo)
      (! restore-cleanup-context)
      (! jump-return-pc)) ; blr
    (arm2-open-undo)
    (@ protform-label)
    (arm2-adjust-vstack (* 3 *arm2-target-node-size*))
    (arm2-undo-body seg vreg xfer protected-form old-stack)))

(defarm2 arm2-progv progv (seg vreg xfer symbols values body)
  (let* ((cleanup-label (backend-get-next-label))
         (protform-label (backend-get-next-label))
         (old-stack (arm2-encode-stack)))
    (arm2-two-targeted-reg-forms seg symbols ($ arm::arg_y) values ($ arm::arg_z))
    (! lock-constant-pool)
    (! progvsave)                       ;creates an unwind-protect
    (arm2-open-undo $undostkblk)
    (! non-barrier-jump (aref *backend-labels* cleanup-label))
    (-> protform-label)
    (! unlock-constant-pool)
    (@ cleanup-label)
    (! progvrestore)
    (arm2-open-undo)
    (@ protform-label)
    (arm2-undo-body seg vreg xfer body old-stack)))

(defarm2 arm2-%ptr-eql %ptr-eql (seg vreg xfer cc x y )
  (if (null vreg)
    (progn
      (arm2-form seg nil nil x)
      (arm2-form seg nil xfer y))
    (let* ((x-abs (acode-absolute-ptr-p x t))
           (y-abs (acode-absolute-ptr-p y t))
           (abs (or x-abs y-abs))
           (other (if abs (if x-abs y x))))
      (multiple-value-bind (cr-bit true-p) (acode-condition-to-arm-cr-bit cc)
        (if other
          (with-imm-target () (other-target :address)
            (arm2-one-targeted-reg-form seg other other-target)
            (if (typep abs '(signed-byte 16))              
              (arm2-test-reg-%izerop seg vreg xfer other-target cr-bit true-p abs)
              (with-imm-temps (other-target) ((abs-target :address))
                (use-imm-temp other-target)
                (arm2-lri seg abs-target abs)
                (arm2-compare-registers seg vreg xfer other-target abs-target cr-bit true-p))))
          ; Neither expression is obviously a constant-valued macptr.
          (with-imm-target () (target-a :address)
            (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
            (arm2-one-targeted-reg-form seg x target-a)
            (arm2-push-register seg target-a)
            (arm2-one-targeted-reg-form seg y target-a)
            (with-imm-target (target-a) (target-b :address)
              (arm2-pop-register seg target-b)
              (arm2-compare-registers seg vreg xfer target-b target-a cr-bit true-p)))))))))

(defarm2 arm2-set-bit %set-bit (seg vreg xfer ptr offset newval)
  (let* ((offval (acode-fixnum-form-p offset))
         (byte-index (if offval (ash offval -3)))
         (bit-index (if (and byte-index (< byte-index #x8000))
                      (logand offval #x7)))
         (triv-offset (arm2-trivial-p offset))
         (triv-val (arm2-trivial-p newval)))
    (with-imm-target ()
      (src :address)
      (arm2-one-targeted-reg-form seg ptr src)
      (if bit-index
        (let* ((mask-start (logand 31 (+ bit-index 25)))
               (mask-end (logand 31 (+ bit-index 23)))
               (mask (ash #x80 (- bit-index)))
               (constval (acode-fixnum-form-p newval)))
          (if constval
            (progn
              (if (eql constval 0)
                (! mem-set-c-bit-0 src byte-index mask-start mask-end)
                (! mem-set-c-bit-1 src byte-index mask))
              (when vreg
                (arm2-form seg vreg nil newval)))
            (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
              (unless triv-val
                (arm2-push-register seg src))
              (let* ((target (arm2-one-untargeted-reg-form seg newval arm::arg_z)))
                (unless triv-val
                  (arm2-pop-register seg src))
                (! mem-set-c-bit src byte-index (+ 24 bit-index) target)
                (<- target)))))
        (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
          (unless (and triv-val triv-offset)
            (arm2-push-register seg src))
          (multiple-value-bind (idx-reg val-reg)
              (arm2-two-untargeted-reg-forms seg offset arm::arg_y newval arm::arg_z)
            (unless (and triv-val triv-offset)
              (arm2-pop-register seg src))
            (! mem-set-bit src idx-reg val-reg)
            (<- val-reg)))))
    (^)))

(defarm2 arm2-%immediate-set-xxx %immediate-set-xxx (seg vreg xfer bits ptr offset val)
  (arm2-%immediate-store seg vreg xfer bits ptr offset val))



(defarm2 arm2-%immediate-inc-ptr %immediate-inc-ptr (seg vreg xfer ptr by)
  (let* ((triv-by (arm2-trivial-p by))
         (fixnum-by (acode-fixnum-form-p by)))
    (if (and fixnum-by (eql 0 fixnum-by))
      (arm2-form seg vreg xfer ptr)
      (with-imm-target () (ptr-reg :address)
        (arm2-one-targeted-reg-form seg ptr ptr-reg)
        (if (setq fixnum-by (and fixnum-by
                                 (or (arm::encode-arm-immediate fixnum-by)
                                     (arm::encode-arm-immediate (- fixnum-by)))
                                 fixnum-by))
          (with-imm-target (ptr-reg) (result :address)
            (! add-immediate result ptr-reg fixnum-by)
            (<- result))
          (let* ((*arm2-nfp-depth* *arm2-nfp-depth*))
            (unless triv-by
              (arm2-push-register seg ptr-reg))
            (with-imm-target (ptr-reg) (by-reg :s32)
              (let* ((mask *available-backend-imm-temps*)
                     (*available-backend-imm-temps* mask))
                (when triv-by
                  (use-imm-temp (%hard-regspec-value ptr-reg)))
                (arm2-one-targeted-reg-form seg by by-reg)
                (setq *available-backend-imm-temps* mask)
                (unless triv-by
                  (arm2-pop-register seg ptr-reg))
                (with-imm-target () (result :address)
                  (! fixnum-add result ptr-reg by-reg)
                  (<- result))))))
        (^)))))



(defarm2 arm2-multiple-value-call multiple-value-call (seg vreg xfer fn arglist)
  (arm2-mvcall seg vreg xfer fn arglist))








(defun arm2-identity (seg vreg xfer arg)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if (null vreg)
      (arm2-form seg vreg xfer arg)
      (progn
        (ensuring-node-target (target vreg)
          (arm2-one-targeted-reg-form seg arg target))
      (^)))))

      
(defun arm2-eabi-ff-call-simple (seg vreg xfer address argspec argval resultspec)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if argspec
      (let* ((push-vinsn (arm2-push-register seg (arm2-one-untargeted-reg-form seg address arm::arg_z))))
        (case argspec
          ((:signed-doubleword :unsigned-doubleword)
           (arm2-one-targeted-reg-form seg argval arm::arg_z)
           (if (eq argspec :singed-doubleword)
             (! gets64)
             (! getu64)))
          (:address
           (arm2-one-targeted-reg-form seg argval ($ 0 :mode :address)))
          (:double-float
           (arm2-one-targeted-reg-form seg argval ($ 0
                                                     :class :fpr
                                                     :mode :double-float)))
          (:single-float
           (arm2-one-targeted-reg-form seg argval ($ 0
                                                     :class :fpr
                                                     :mode :single-float)))
          (t (arm2-unboxed-integer-arg-to-reg seg argval 0 argspec)))
        (arm2-elide-pushes seg push-vinsn (arm2-pop-register seg arm::arg_z)))
      (arm2-one-targeted-reg-form seg address arm::arg_z))
    (! eabi-ff-call-simple)
    (when vreg
      (cond ((eq resultspec :void) (<- nil))
            ((eq resultspec :double-float)
             (<- ($  arm::d0 :class :fpr :mode :double-float)))
            ((eq resultspec :single-float)
             (<- ($ arm::s0 :class :fpr :mode :single-float)))
            ((eq resultspec :unsigned-doubleword)
             (ensuring-node-target (target vreg)
               (! makeu64)
               (arm2-copy-register seg target arm::arg_z)))
            ((eq resultspec :signed-doubleword)
             (ensuring-node-target (target vreg)
               (! makes64)
               (arm2-copy-register seg target arm::arg_z)))
            (t
             (<- (make-wired-lreg arm::imm0
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

(defun arm2-soft-float-eabi-ff-call (seg vreg xfer address argspecs argvals resultspec)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((*arm2-vstack* *arm2-vstack*)
           (*arm2-cstack* *arm2-cstack*)
           (next-arg-word 0)
           (natural-64-bit-alignment
            (case (backend-target-os *target-backend*)
              (:darwinarm nil)
              (t t)))
           (continue-label (backend-get-next-label)))
      (declare (fixnum next-arg-word))
      (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg address arm::arg_z))
      (dolist (argspec argspecs)
        (case argspec
          ((:double-float :unsigned-doubleword :signed-doubleword)
           (when (and natural-64-bit-alignment (oddp next-arg-word))
             (incf next-arg-word))
           (incf next-arg-word 2))
          (t (incf next-arg-word))))
      (! alloc-eabi-c-frame next-arg-word)
      (arm2-open-undo $undo-arm-c-frame)

      ;; Evaluate each form into the C frame, according to the
      ;; matching argspec.
      (setq next-arg-word 0)
      (do* ((specs argspecs (cdr specs))
            (vals argvals (cdr vals)))
           ((null specs))
        (declare (list specs vals))
        (let* ((valform (car vals))
               (spec (car specs))
               (absptr (acode-absolute-ptr-p valform)))
          (case spec
            (:double-float
             (with-fp-target () (df :double-float)
               (when (and natural-64-bit-alignment (oddp next-arg-word))
                 (incf next-arg-word))
               (arm2-one-targeted-reg-form seg valform df)
               (! set-double-eabi-c-arg df next-arg-word)
               (incf next-arg-word 2)))
            (:single-float
             (with-fp-target () (sf :single-float)
               (arm2-one-targeted-reg-form seg valform sf)
               (! set-single-eabi-c-arg sf next-arg-word)
               (incf next-arg-word)))
            ((:signed-doubleword :unsigned-doubleword)
             (arm2-one-targeted-reg-form seg valform ($ arm::arg_z))
             (if (eq spec :signed-doubleword)
               (! gets64)
               (! getu64))
             (when (and natural-64-bit-alignment (oddp next-arg-word))
               (incf next-arg-word))
             (! set-eabi-c-arg ($ arm::imm0) next-arg-word)
             (incf next-arg-word)
             (! set-eabi-c-arg ($ arm::imm1) next-arg-word)
             (incf next-arg-word))
            (:address
             (with-imm-target () (ptr :address)
               (if absptr
                 (arm2-lri seg ptr absptr)
                 (arm2-form seg ptr nil valform))
               (! set-eabi-c-arg ptr next-arg-word)
               (incf next-arg-word)))
            (t
             (if (typep spec 'fixnum)
               (with-imm-target () (addr :address)
                 (arm2-form seg addr nil valform)
                 (with-imm-target (addr) (valreg :natural)
                   (dotimes (i spec)
                     (! mem-ref-c-natural valreg addr (* i *arm2-target-node-size*))
                     (! set-eabi-c-arg valreg next-arg-word)
                     (incf next-arg-word))))
               (with-imm-target () (valreg :natural)
                 (let* ((reg (arm2-unboxed-integer-arg-to-reg seg valform valreg spec)))
                   (! set-eabi-c-arg reg next-arg-word)
                   (incf next-arg-word))))))))
      (arm2-vpop-register seg ($ arm::arg_z))
      (! eabi-ff-call-simple) 
      (arm2-close-undo)
      (case resultspec
        (:double-float
         
         (! gpr-pair-to-double-float ($ arm::d0 :class :fpr :mode :double-float)  arm::imm0 arm::imm1))
        (:single-float
         (! gpr-to-single-float ($ arm::s0 :class :fpr :mode :single-float)  arm::imm0)))
      (@ continue-label)
      (when vreg
        (cond ((eq resultspec :void) (<- nil))
              ((eq resultspec :double-float)
               (<- ($  arm::d0 :class :fpr :mode :double-float)))
              ((eq resultspec :single-float)
               (<- ($ arm::s0 :class :fpr :mode :single-float)))
              ((eq resultspec :unsigned-doubleword)
               (ensuring-node-target (target vreg)
                 (! makeu64)
                 (arm2-copy-register seg target arm::arg_z)))
              ((eq resultspec :signed-doubleword)
               (ensuring-node-target (target vreg)
                 (! makes64)
                 (arm2-copy-register seg target arm::arg_z)))
              (t
               (<- (make-wired-lreg arm::imm0
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
      (^))))

(defarm2 arm2-eabi-ff-call eabi-ff-call (seg vreg xfer address argspecs argvals resultspec &optional monitor)
  (declare (ignore monitor))
  (target-os-case 
   (:androidarm
    (arm2-soft-float-eabi-ff-call seg vreg xfer address argspecs argvals resultspec))
   ((:linuxarm)
    (if (and (< (length argspecs) 2)
             (not (typep (car argspecs) 'fixnum)))
      (arm2-eabi-ff-call-simple seg vreg xfer address (car argspecs) (car argvals) resultspec)
      (progn
        (let* ((*arm2-vstack* *arm2-vstack*)
               (*arm2-cstack* *arm2-cstack*)
               (next-arg-word 0)
               (next-fp-arg-word 0))
          (declare (fixnum next-arg-word))
          (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg address arm::arg_z))
          (dolist (spec argspecs)
            (case spec
              ((:signed-doubleword :unsigned-doubleword)
               (if (oddp next-arg-word)
                 (incf next-arg-word 3)
                 (incf next-arg-word 2)))
              (:double-float
               (if (<= next-fp-arg-word 14)
                 (if (oddp next-fp-arg-word)
                   (incf next-fp-arg-word 3)
                   (incf next-fp-arg-word 2))
                 (if (oddp next-arg-word)
                   (incf next-arg-word 3)
                   (incf next-arg-word 2))))
              (:single-float
               (if (< next-fp-arg-word 16)
                 (incf next-fp-arg-word)
                 (incf next-arg-word)))
              (t
               (if (typep spec 'fixnum)
                 (incf next-arg-word spec)
                 (incf next-arg-word)))))
          (! alloc-eabi-c-frame (+ next-arg-word 16))
          (arm2-open-undo $undo-arm-c-frame)
          (setq next-fp-arg-word 0
                next-arg-word 16)
          (do* ((specs argspecs (cdr specs))
                (vals argvals (cdr vals)))
               ((null specs))
            (declare (list specs vals))
            (let* ((valform (car vals))
                   (spec (car specs))
                   (absptr (acode-absolute-ptr-p valform)))
              (case spec
                (:double-float
                 (with-fp-target () (df :double-float)
                   (arm2-one-targeted-reg-form seg valform df)
                   (cond ((<= next-fp-arg-word 14)
                          (when (oddp next-fp-arg-word)
                            (incf next-fp-arg-word))
                          (! set-double-eabi-c-arg df next-fp-arg-word)
                          (incf next-fp-arg-word 2))
                         (t
                          (when (oddp next-arg-word)
                            (incf next-arg-word))
                          (! set-double-eabi-c-arg df next-arg-word)
                          (incf next-arg-word 2)))))
                (:single-float
                 (with-fp-target () (sf :single-float)
                   (arm2-one-targeted-reg-form seg valform sf)
                   (cond ((< next-fp-arg-word 16)
                          (! set-single-eabi-c-arg sf next-fp-arg-word)
                          (incf next-fp-arg-word))
                         (t
                          (! set-single-eabi-c-arg sf next-arg-word)
                          (incf next-arg-word)))))
                ((:signed-doubleword :unsigned-doubleword)
                 (arm2-one-targeted-reg-form seg valform ($ arm::arg_z))
                 (if (eq spec :signed-doubleword)
                   (! gets64)
                   (! getu64))
                 (when (oddp next-arg-word)
                   (incf next-arg-word))
                 (! set-eabi-c-arg ($ arm::imm0) next-arg-word)
                 (incf next-arg-word)
                 (! set-eabi-c-arg ($ arm::imm1) next-arg-word)
                 (incf next-arg-word))
                (:address
                 (with-imm-target () (ptr :address)
                   (if absptr
                     (arm2-lri seg ptr absptr)
                     (arm2-form seg ptr nil valform))
                   (! set-eabi-c-arg ptr next-arg-word)
                   (incf next-arg-word)))
                (t
                 (if (typep spec 'fixnum)
                   (with-imm-target () (addr :address)
                     (arm2-form seg addr nil valform)
                     (with-imm-target (addr) (valreg :natural)
                       (dotimes (i spec)
                         (! mem-ref-c-natural valreg addr (* i *arm2-target-node-size*))
                         (! set-eabi-c-arg valreg next-arg-word)
                         (incf next-arg-word))))
                   (with-imm-target () (valreg :natural)
                     (let* ((reg (arm2-unboxed-integer-arg-to-reg seg valform valreg spec)))
                       (! set-eabi-c-arg reg next-arg-word)
                       (incf next-arg-word))))))))
          (arm2-vpop-register seg ($ arm::arg_z))
          (! eabi-ff-callhf) 
          (arm2-close-undo))
     

        (when vreg
          (cond ((eq resultspec :void) (<- nil))
                ((eq resultspec :double-float)
                 (<- ($  arm::d0 :class :fpr :mode :double-float)))
                ((eq resultspec :single-float)
                 (<- ($ arm::s0 :class :fpr :mode :single-float)))
                ((eq resultspec :unsigned-doubleword)
                 (ensuring-node-target (target vreg)
                   (! makeu64)
                   (arm2-copy-register seg target arm::arg_z)))
                ((eq resultspec :signed-doubleword)
                 (ensuring-node-target (target vreg)
                   (! makes64)
                   (arm2-copy-register seg target arm::arg_z)))
                (t
                 (<- (make-wired-lreg arm::imm0
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
        (^))))))





             
(defarm2 arm2-%temp-list %temp-list (seg vreg xfer arglist)
  (arm2-use-operator (%nx1-operator list) seg vreg xfer arglist))

(defarm2 arm2-%temp-cons %temp-cons (seg vreg xfer car cdr)
  (arm2-use-operator (%nx1-operator cons) seg vreg xfer car cdr))


;;; Under MacsBug 5.3 (and some others ?), this'll do a low-level user
;;; break.  If the debugger doesn't recognize the trap instruction,
;;; you'll have to manually advance the PC past it.  "arg" winds up in the
;;; arg_z register; whatever's in arg_z on return is returned by
;;; the %debug-trap construct.

(defarm2 arm2-%debug-trap %debug-trap (seg vreg xfer arg)
  (arm2-one-targeted-reg-form seg arg ($ arm::arg_z))
  (! %debug-trap)
  (<- ($ arm::arg_z))
  (^))

(defarm2 arm2-%reference-external-entry-point %reference-external-entry-point
  (seg vreg xfer arg)
  (ensuring-node-target (target vreg)
    (let* ((reg (if (eq (hard-regspec-value target) arm::arg_z) ($ arm::arg_y) ($ arm::arg_z))))
      (arm2-one-targeted-reg-form seg arg reg)
      (! eep.address target reg)))
  (^))

(defarm2 arm2-%natural+ %natural+ (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil x)
      (arm2-form seg nil xfer y))
    (let* ((fix-x (acode-fixnum-form-p x))
           (fix-y (acode-fixnum-form-p y)))
      (if (and fix-x fix-y)
        (arm2-natural-constant seg vreg xfer (+ fix-x fix-y))
        (let* ((u15x (and (typep fix-x '(unsigned-byte 15)) fix-x))
               (u15y (and (typep fix-y '(unsigned-byte 15)) fix-y)))
          (if (not (or u15x u15y))
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (arm2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural+ xreg xreg yreg))
              (<- xreg))
            (let* ((other (if u15x y x)))
              (with-imm-target () (other-reg :natural)
                (arm2-one-targeted-reg-form seg other other-reg)
                (! %natural+-c other-reg other-reg (or u15x u15y))
                (<- other-reg))))
          (^))))))

(defarm2 arm2-%natural- %natural- (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil x)
      (arm2-form seg nil xfer y))
    (let* ((fix-x (acode-fixnum-form-p x))
           (fix-y (acode-fixnum-form-p y)))
      (if (and fix-x fix-y)
        (arm2-natural-constant seg vreg xfer (- fix-x fix-y))
        (let* ((u15y (and (typep fix-y '(unsigned-byte 15)) fix-y)))
          (if (not u15y)
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (arm2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural- xreg xreg yreg))
              (<- xreg))
            (progn
              (with-imm-target () (xreg :natural)
                (arm2-one-targeted-reg-form seg x xreg)
                (! %natural--c xreg xreg u15y)
                (<- xreg))))
          (^))))))

(defarm2 arm2-%natural-logior %natural-logior (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil x)
      (arm2-form seg nil xfer y))
    (let* ((naturalx (nx-natural-constant-p x))
           (naturaly (nx-natural-constant-p y)))
      (if (and naturalx naturaly) 
        (arm2-natural-constant seg vreg xfer (logior naturalx naturaly))
        (let* ((constant (let* ((c (or naturalx naturaly)))
                           (when c
                             (if (arm::encode-arm-immediate c)
                               c)))))
          (if (not constant)
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (arm2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural-logior xreg xreg yreg))
              (<- xreg))
            (let* ((other (if naturalx y x)))
              (with-imm-target () (other-reg :natural)
                (arm2-one-targeted-reg-form seg other other-reg)
                (! logior-immediate other-reg other-reg (logand constant #xffffffff))
                (<- other-reg))))
          (^))))))

(defarm2 arm2-%natural-logxor %natural-logxor (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil x)
      (arm2-form seg nil xfer y))
    (let* ((naturalx (nx-natural-constant-p x))
           (naturaly (nx-natural-constant-p y)))
      (if (and naturalx naturaly) 
        (arm2-natural-constant seg vreg xfer (logxor naturalx naturaly))
        (let* ((constant (let* ((c (or naturalx naturaly)))
                           (when c
                             (if (arm::encode-arm-immediate c)
                               c)))))
          (if (not constant)
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (arm2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural-logxor xreg xreg yreg))
              (<- xreg))
            (let* ((other (if naturalx y x)))
              (with-imm-target () (other-reg :natural)
                (arm2-one-targeted-reg-form seg other other-reg)
                (! logxor-immediate other-reg other-reg (logand constant #xffffffff))
                (<- other-reg))))
          (^))))))

(defarm2 arm2-%natural-logand %natural-logand (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (arm2-form seg nil nil x)
      (arm2-form seg nil xfer y))
    (let* ((naturalx (nx-natural-constant-p x))
           (naturaly (nx-natural-constant-p y)))
      (if (and naturalx naturaly) 
        (arm2-natural-constant seg vreg xfer (logand naturalx naturaly))
        (let* ((constant (let* ((c (or naturalx naturaly)))
                           (when c
                             (if (or (arm::encode-arm-immediate c)
                                     (arm::encode-arm-immediate (lognot c)))
                               c)))))
          (if (not constant)
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (arm2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural-logand xreg xreg yreg))
              (<- xreg))
            (let* ((other (if naturalx y x)))
              (with-imm-target () (other-reg :natural)
                (arm2-one-targeted-reg-form seg other other-reg)
                (! logand-immediate other-reg other-reg (logand constant #xffffffff))
                (if (and (typep constant '(unsigned-byte 29))
                         (node-reg-p vreg))
                  (! box-fixnum vreg other-reg)
                  (<- other-reg)))))
          (^))))))

(defarm2 arm2-natural-shift-right natural-shift-right (seg vreg xfer num amt)
  (with-imm-target () (dest :natural)
    (arm2-one-targeted-reg-form seg num dest)
    (! natural-shift-right dest dest (acode-fixnum-form-p amt))
    (<- dest)
    (^)))

(defarm2 arm2-natural-shift-left natural-shift-left (seg vreg xfer num amt)
  (with-imm-target () (dest :natural)
    (arm2-one-targeted-reg-form seg num dest)
    (! natural-shift-left dest dest (acode-fixnum-form-p amt))
    (<- dest)
    (^)))

;;; This assumes that "global" variables are always boundp.
(defarm2 arm2-global-ref global-ref (seg vreg xfer sym)
  (when vreg
    (ensuring-node-target (target vreg)
      (with-node-temps () (symreg)
        (setq symreg (or (arm2-register-constant-p sym)
                         (arm2-store-immediate seg sym symreg)))
        (! node-slot-ref target symreg arm::symbol.vcell-cell))))
  (^))

(defarm2 arm2-global-setq global-setq (seg vreg xfer sym val)
  (arm2-vset seg
             vreg
             xfer
             :symbol
             (make-acode (%nx1-operator immediate) sym)
             (make-acode (%nx1-operator fixnum)
                         arm::symbol.vcell-cell)
             val
             nil))

(defarm2 arm2-%current-frame-ptr %current-frame-ptr (seg vreg xfer)
  (cond ((arm2-tailcallok xfer)
         (arm2-restore-nvrs seg nil)
         (arm2-restore-non-volatile-fprs seg)
         (! restore-nfp)
	 (arm2-restore-full-lisp-context seg)
	 (! %current-frame-ptr ($ arm::arg_z))
	 (! jump-return-pc))
	(t
	 (when vreg
	   (ensuring-node-target (target vreg)
				 (! %current-frame-ptr target)))
	 (^))))

(defarm2 arm2-%foreign-stack-pointer %foreign-stack-pointer (seg vreg xfer)
  (when vreg
    (ensuring-node-target (target vreg)
      (! %current-frame-ptr target)))
  (^))

(defarm2 arm2-%current-tcr %current-tcr (seg vreg xfer)
  (when vreg
    (ensuring-node-target (target vreg)
      (! %current-tcr target)))
  (^))



(defarm2 arm2-%interrupt-poll %interrupt-poll (seg vreg xfer)
  (! event-poll)
  (arm2-nil seg vreg xfer))


(defarm2 arm2-with-c-frame with-c-frame (seg vreg xfer body &aux
                                             (old-stack (arm2-encode-stack)))
  (ecase (backend-name *target-backend*)
    (:linuxarm32 (! alloc-eabi-c-frame 0))
    ((:darwinarm32 :darwinarm64 :linuxarm64) (! alloc-c-frame 0)))
  (arm2-open-undo $undo-arm-c-frame)
  (arm2-undo-body seg vreg xfer body old-stack))

(defarm2 arm2-with-variable-c-frame with-variable-c-frame (seg vreg xfer size body &aux
                                                               (old-stack (arm2-encode-stack)))
  (let* ((reg (arm2-one-untargeted-reg-form seg size arm::arg_z)))
    (ecase (backend-name *target-backend*)
      ((:linuxarm :darwinarm :androidarm) (! alloc-variable-eabi-c-frame reg)))
    (arm2-open-undo $undo-arm-c-frame)
    (arm2-undo-body seg vreg xfer body old-stack)))

(defarm2 arm2-%symbol->symptr %symbol->symptr (seg vreg xfer sym)
  (let* ((src (arm2-one-untargeted-reg-form seg sym arm::arg_z)))
    (ensuring-node-target (target vreg)
      (! %symbol->symptr target src))
    (^)))

(defarm2 arm2-%double-to-single %double-to-single (seg vreg xfer arg)
  (if (null vreg)
    (arm2-form seg vreg xfer arg)
    (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
      (let* ((dreg (arm2-one-untargeted-reg-form 
                    seg arg
                    (make-wired-lreg (hard-regspec-value vreg)
                                     :class hard-reg-class-fpr
                                     :mode hard-reg-class-fpr-mode-double))))
        (arm2-copy-register seg vreg dreg)
        (^))
      (with-fp-target () (argreg :double-float)
        (arm2-one-targeted-reg-form seg arg argreg)
        (with-fp-target ()  (sreg :single-float)
          (arm2-copy-register seg sreg argreg)
          (<- sreg)
          (^))))))

(defarm2 arm2-%single-to-double %single-to-double (seg vreg xfer arg)
  (if (null vreg)
    (arm2-form seg vreg xfer arg)
    (with-fp-target () (sreg :single-float)
      (setq sreg (arm2-one-untargeted-reg-form seg arg sreg))
      (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
               (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
        (<- sreg)
        (with-fp-target (sreg) (dreg :double-float)
          (arm2-copy-register seg dreg sreg)
          (<- dreg)))
      (^))))

(defarm2 arm2-%symptr->symvector %symptr->symvector (seg vreg xfer arg)
  (arm2-identity seg vreg xfer arg))

(defarm2 arm2-%symvector->symptr %symvector->symptr (seg vreg xfer arg)
  (arm2-identity seg vreg xfer arg))

(defarm2 arm2-%fixnum-to-double %fixnum-to-double (seg vreg xfer arg)
  (with-fp-target () (dreg :double-float)
    (let* ((sreg (make-wired-lreg (* (hard-regspec-value dreg) 2) :mode hard-reg-class-fpr-mode-single))
           (r (arm2-one-untargeted-reg-form seg arg arm::arg_z)))
      (unless (or (acode-fixnum-form-p arg)
                  *arm2-reckless*)
        (! trap-unless-fixnum r))
      (! fixnum->double dreg r sreg)
      (<- dreg)
      (^))))

(defarm2 arm2-%fixnum-to-single %fixnum-to-single (seg vreg xfer arg)
  (with-fp-target () (dreg :single-float)
    (let* ((r (arm2-one-untargeted-reg-form seg arg arm::arg_z)))
      (unless (or (acode-fixnum-form-p arg)
                  *arm2-reckless*)
        (! trap-unless-fixnum r))
      (when *arm2-float-safety*
        (! clear-pending-fpu-exceptions))
      (! fixnum->single dreg r)
      (when *arm2-float-safety*
        (! trap-if-fpu-exception))
      (<- dreg)
      (^))))

(defarm2 arm2-%double-float %double-float (seg vreg xfer arg)
  (let* ((real (or (acode-fixnum-form-p arg)
                   (let* ((form (acode-unwrapped-form-value arg)))
                     (if (and (acode-p form)
                              (eq (acode-operator form)
                                  (%nx1-operator immediate))
                              (typep (car (acode-operands form)) 'real))
                       (car (acode-operands form))))))
         (dconst (and real (ignore-errors (float real 0.0d0)))))
    (if dconst
      (arm2-immediate seg vreg xfer dconst)
      (if (arm2-form-typep arg 'single-float)
        (arm2-use-operator (%nx1-operator %single-to-double)
                           seg
                           vreg
                           xfer
                           arg)
        (if (arm2-form-typep arg 'fixnum)
          (arm2-use-operator (%nx1-operator %fixnum-to-double)
                             seg
                             vreg
                             xfer
                             arg)
          (arm2-use-operator (%nx1-operator call)
                             seg
                             vreg
                             xfer
                             (make-acode (%nx1-operator immediate)
                                         '%double-float)
                             (list nil (list arg))))))))

(defarm2 arm2-%single-float %single-float (seg vreg xfer arg)
  (let* ((real (or (acode-fixnum-form-p arg)
                   (let* ((form (acode-unwrapped-form-value arg)))
                     (if (and (acode-p form)
                              (eq (acode-operator form)
                                  (%nx1-operator immediate))
                              (typep (car (acode-operands form)) 'real))
                       (car (acode-operands form))))))
         (sconst (and real (ignore-errors (float real 0.0f0)))))
    (if sconst
      (arm2-immediate seg vreg xfer sconst)
      (if (arm2-form-typep arg 'double-float)
        (arm2-use-operator (%nx1-operator %double-to-single)
                           seg
                           vreg
                           xfer
                           arg)
        (if (arm2-form-typep arg 'fixnum)
          (arm2-use-operator (%nx1-operator %fixnum-to-single)
                             seg
                             vreg
                             xfer
                             arg)
          (arm2-use-operator (%nx1-operator call)
                             seg
                             vreg
                             xfer
                             (make-acode (%nx1-operator immediate)
                                         '%short-float)
                             (list nil (list arg))))))))

(defun show-function-constants (f)
  (cond ((typep f 'function)
	 (do* ((i 0 j)
	       (n (uvsize f))
	       (j 1 (1+ j)))
	      ((= j n))
	   (format t "~&~d: ~s" i (uvref f j))))
	(t (report-bad-arg f 'function))))

(defun arm2-complex-single-float-access (seg vreg xfer arg offset)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (cond ((null vreg) (arm2-form seg vreg xfer arg))
          (t (with-fp-target () (target :single-float)
               (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                        (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
               (setq target vreg))
             (! %complex-single-float-access target (arm2-one-untargeted-reg-form seg arg arm::arg_z) offset)
             (<- target)
             )
             (^)))))





	
;------


;;;Make a gcable macptr.
(defarm2 arm2-%new-ptr %new-ptr (seg vreg xfer size clear-p )
  (arm2-call-fn seg
                vreg
                xfer
                (make-acode (%nx1-operator immediate)
                            '%new-gcable-ptr)
                (list nil (list clear-p size))
                nil))

(defarm2 arm2-ash ash (seg vreg xfer num amt)
  (arm2-two-targeted-reg-forms seg num ($ arm::arg_y) amt ($ arm::arg_z))
  (arm2-fixed-call-builtin seg vreg xfer '.SPbuiltin-ash))


(defarm2 arm2-fixnum-ash fixnum-ash (seg vreg xfer num amt)
  (multiple-value-bind (rnum ramt) (arm2-two-untargeted-reg-forms seg num ($ arm::arg_y) amt ($ arm::arg_z))
    (let* ((amttype (specifier-type (acode-form-type amt *arm2-trust-declarations*))))
      (ensuring-node-target (target vreg)
        (if (and (typep amttype 'numeric-ctype)
                 (>= (numeric-ctype-low amttype) 0))
          (! fixnum-ash-left target rnum ramt)
          (! fixnum-ash target rnum ramt)))
      (^))))


(defarm2 arm2-fixnum-ref-double-float %fixnum-ref-double-float (seg vreg xfer base index)
  (if (null vreg)
    (progn
      (arm2-form base seg nil nil)
      (arm2-form index seg nil xfer))
    (let* ((fix (acode-fixnum-form-p index)))
      (unless (typep fix '(integer 0 (128)))
        (setq fix nil))
      (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
               (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double) )
        (cond (fix
               (! fixnum-ref-c-double-float vreg (arm2-one-untargeted-reg-form seg base arm::arg_z) fix))
              (t
               (multiple-value-bind (rbase rindex) (arm2-two-untargeted-reg-forms seg base arm::arg_y index arm::arg_z)
                 (! fixnum-ref-double-float vreg rbase rindex))))
        (with-fp-target () (target :double-float)
        (cond (fix
               (! fixnum-ref-c-double-float target (arm2-one-untargeted-reg-form seg base arm::arg_z) fix))
              (t
               (multiple-value-bind (rbase rindex) (arm2-two-untargeted-reg-forms seg base arm::arg_y index arm::arg_z)
                 (! fixnum-ref-double-float target rbase rindex))))
        (<- target)))
      (^))))

(defarm2 arm2-fixnum-set-double-float %fixnum-set-double-float (seg vreg xfer base index val)
  (let* ((fix (acode-fixnum-form-p index)))
    (unless (typep fix '(integer 0 (128)))
      (setq fix nil))
    (cond ((or (null vreg)
               (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                    (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double)))
           (let* ((fhint (or vreg ($ arm::d0 :class :fpr :mode :double-float))))
             (if fix
               (multiple-value-bind (rbase rval)
                   (arm2-two-untargeted-reg-forms seg base ($ arm::arg_z) val fhint)
                 (! fixnum-set-c-double-float rbase fix rval)
                 (<- rval))
               (multiple-value-bind (rbase rindex rval)
                   (arm2-three-untargeted-reg-forms seg base ($ arm::arg_y) index ($ arm::arg_z) val fhint)
                 (! fixnum-set-double-float rbase rindex rval)
                 (<- rval)))))
          (t
           (if fix
             (multiple-value-bind (rbase rboxed)
                 (arm2-two-untargeted-reg-forms seg base ($ arm::arg_y) val ($ arm::arg_z))
               (with-fp-target () (rval :double-float)
                 (arm2-copy-register seg rval rboxed)
                 (! fixnum-set-c-double-float rbase fix rval))
               (<- rboxed))
             (multiple-value-bind (rbase rindex rboxed)
                 (arm2-three-untargeted-reg-forms seg base ($ arm::arg_x) index ($ arm::arg_y) val ($ arm::arg_z))
               (with-fp-target () (rval :double-float)
                 (arm2-copy-register seg rval rboxed)
                 (! fixnum-set-double-float rbase rindex rval))
               (<- rboxed)))))
    (^)))

(defarm2 arm2-t t (seg vreg xfer)
  (arm2-t seg vreg xfer))


(defarm2 arm2-nil nil (seg vreg xfer)
  (arm2-nil seg vreg xfer))

(defarm2 arm2-ivector-typecode-p ivector-typecode-p (seg vreg xfer val)
  (cond ((null vreg) (arm2-form seg vreg xfer val))
        (t (ensuring-node-target (target vreg)
             (! ivector-typecode-p target (arm2-one-untargeted-reg-form seg val arm::arg_z)))
           (^))))

(defarm2 arm2-gvector-typecode-p gvector-typecode-p (seg vreg xfer val)
  (cond ((null vreg) (arm2-form seg vreg xfer val))
        (t (ensuring-node-target (target vreg)
             (! gvector-typecode-p target (arm2-one-untargeted-reg-form seg val arm::arg_z)))
           (^))))


(defarm2 arm2-%complex-single-float-realpart %complex-single-float-realpart (seg vreg xfer arg)
  (if (null vreg)
    (arm2-form  seg  nil xfer arg)
    (with-fp-target () (target :single-float)
      (when (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
        (setq target vreg))
      (with-fp-target (target) (val :complex-single-float)
        (! %complex-single-float-realpart target (arm2-one-untargeted-reg-form seg arg val))
        (<- target)
        (^ )))))


(defarm2 arm2-%complex-single-float-imagpart %complex-single-float-imagpart (seg vreg xfer arg)
  (if (null vreg)
    (arm2-form  seg  nil xfer arg)
    (with-fp-target () (target :single-float)
      (when (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
        (setq target vreg))
      (with-fp-target (target) (val :complex-single-float)
        (! %complex-single-float-imagpart target (arm2-one-untargeted-reg-form seg arg val))
        (<- target)
        (^ )))))


(defarm2 arm2-%complex-double-float-realpart %complex-double-float-realpart (seg vreg xfer arg)
  (if (null vreg)
    (arm2-form  seg  nil xfer arg)
    (with-fp-target () (target :double-float)
      (when (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
        (setq target vreg))
      (with-fp-target (target) (val :complex-double-float)
        (! %complex-double-float-realpart target (arm2-one-untargeted-reg-form seg arg val))
        (<- target)
        (^ )))))


(defarm2 arm2-%complex-double-float-imagpart %complex-double-float-imagpart (seg vreg xfer arg)
  (if (null vreg)
    (arm2-form  seg  nil xfer arg)
    (with-fp-target () (target :double-float)
      (when (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
        (setq target vreg))
      (with-fp-target (target) (val :complex-double-float)
        (! %complex-double-float-imagpart target (arm2-one-untargeted-reg-form seg arg val))
        (<- target)
        (^ )))))

(defarm2 arm2-%make-complex-single-float %make-complex-single-float (seg vreg xfer r i)
    (if (null vreg)
    (progn
      (arm2-form seg nil nil r)
      (arm2-form seg nil xfer i))
    (with-fp-target () (target :complex-single-float)
      (if (and (eql (hard-regspec-class vreg) hard-reg-class-fpr)
               (eql (get-regspec-mode vreg) hard-reg-class-fpr-mode-complex-single-float))
        (setq target vreg))
      (arm2-two-targeted-reg-forms seg
                                   r ($ (* 2 (%hard-regspec-value target))
                                        :class :fpr
                                        :mode :single-float)
                                   i ($ (1+ (* 2 (%hard-regspec-value target)))
                                        :class :fpr
                                        :mode :single-float))
      (<- target)
      (^))))

(defarm2 arm2-%make-complex-double-float %make-complex-double-float (seg vreg xfer r i)
    (if (null vreg)
    (progn
      (arm2-form seg nil nil r)
      (arm2-form seg nil xfer i))
    (with-fp-target () (target :complex-double-float)
      (if (and (eql (hard-regspec-class vreg) hard-reg-class-fpr)
               (eql (get-regspec-mode vreg) hard-reg-class-fpr-mode-complex-double-float))
        (setq target vreg))
      (arm2-two-targeted-reg-forms seg
                                   r ($ (%hard-regspec-value target)
                                        :class :fpr
                                        :mode :double-float)
                                   i ($ (1+  (%hard-regspec-value target))
                                        :class :fpr
                                        :mode :double-float))
      (<- target)
      (^))))

                                   
(defarm2 arm2-complex complex (seg vreg xfer r i)
  (arm2-call-fn seg vreg xfer (make-acode (%nx1-operator immediate) 'complex)
                (list nil (list i r)) nil))

(defarm2 arm2-realpart realpart (seg vreg xfer n)
  (arm2-call-fn  seg vreg xfer (make-acode (%nx1-operator immediate) 'realpart)
                 (list nil (list n)) nil))

(defarm2 arm2-imagpart imagpart (seg vreg xfer n)
  (arm2-call-fn  seg vreg xfer (make-acode (%nx1-operator immediate) 'imagpart)
                 (list nil (list n)) nil))