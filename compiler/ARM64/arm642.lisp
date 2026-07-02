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
(defparameter *arm642-target-half-fixnum-type*
  `(signed-byte ,(- *arm642-target-bits-in-word*
                    (1+ *arm642-target-fixnum-shift*))))
(defparameter *arm642-nfp-depth* 0)
(defparameter *arm642-max-nfp-depth* 0)
(defparameter *arm642-all-nfp-pushes* ())
(defparameter *arm642-nfp-vars* ())
(defparameter *arm642-incoming-args-on-stack* most-positive-fixnum)
(defparameter *arm642-stack-vars* ())
(defparameter *arm642-tagbody-info* ())

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

(defun arm642-invalidate-regmap ()
  (setq *arm642-gpr-locations-valid-mask* 0
        *arm642-gpr-constants-valid-mask* 0))

(defun arm642-update-regmap (vinsn)
  (if (vinsn-attribute-p vinsn :call)
    (arm642-invalidate-regmap)
    (let* ((clobbered-regs (vinsn-gprs-set vinsn)))
      (setq *arm642-gpr-locations-valid-mask* (logandc2 *arm642-gpr-locations-valid-mask* clobbered-regs)
            *arm642-gpr-constants-valid-mask* (logandc2 *arm642-gpr-constants-valid-mask* clobbered-regs))))
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
           (*arm642-gpr-locations* (make-array 16 :initial-element nil))
           (*arm642-gpr-constants-valid-mask* 0)
           (*arm642-gpr-constants* (make-array 16 :initial-element nil))
           (*arm642-nfp-depth* 0)
           (*arm642-max-nfp-depth* ())
           (*arm642-all-nfp-pushes* ())
           (*arm642-nfp-vars* ()))
      (declare (dynamic-extent *arm642-gpr-locations* *arm642-gpr-constants*))
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
                          (sections (vector code))
                          debug-info)
                     (declare (dynamic-extent sections))
                     (arm642-expand-vinsns vinsns code sections)
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

(defun arm642-adjust-vstack (delta)
  (arm642-set-vstack (%i+ *arm642-vstack* delta)))

(defun arm642-set-vstack (new)
  (arm642-regmap-note-vstack-delta new *arm642-vstack*)
  (setq *arm642-vstack* new))

(defun arm642-symbol-locative-p (imm)
  (and (consp imm)
       (or (memq imm *arm642-vcells*)
           (memq imm *arm642-fcells*))))

(defun arm642-lri (seg reg value)
  (with-arm64-local-vinsn-macros (seg)
    (if (>= value 0)      
      (! lri reg value)     
      (! lri reg (logand value #xffffffffffffffff)))))

(defun arm642-box-u64 (seg node-dest u64-src)
  (with-arm64-local-vinsn-macros (seg)
    (let ((arg_z ($ arm64::arg_z))
          (imm0 ($ arm64::imm0 :mode :u64)))
      (arm642-copy-register seg imm0 u64-src)
      (! call-subprim (subprim-name->offset '.SPmakeu64))
      (arm642-copy-register seg node-dest arg_z))))

(defun arm642-box-s64 (seg node-dest s64-src)
  (with-arm64-local-vinsn-macros (seg)
    (let ((arg_z ($ arm64::arg_z))
          (imm0 ($ arm64::imm0 :mode :s64)))
      (arm642-copy-register seg imm0 s64-src)
      (! call-subprim (subprim-name->offset '.SPmakes64))
      (arm642-copy-register seg node-dest arg_z))))

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
              ;; This is the "GPR <- GPR" case.  There are
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

(defun arm64-vpush-register (seg src &optional inhibit-note)
  (with-arm64-local-vinsn-macros (seg)
    (prog1
        (! vpush-register src)
      (unless inhibit-note
        (arm64-regmap-note-store src *arm642-vstack*))
      (arm64-adjust-vstack *arm642-target-node-size*))))

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
;;;  (nil if vpushed, var if still in arg_reg).
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
        (! check-exact-nargs nargs))
      (arm642-argregs-entry seg rev-fixed-args))))

(defun arm642-decls (decls)
  (if (fixnump decls)
    (locally (declare (fixnum decls))
      (setq *arm642-tail-allow* (neq 0 (%ilogand2 $decl_tailcalls decls))
            *arm642-open-code-inline* (neq 0 (%ilogand2 $decl_opencodeinline
                                                        decls))
            *arm642-full-safety* (neq 0 (%ilogand2 $decl_full_safety decls))
            *arm642-reckless* (neq 0 (%ilogand2 $decl_unsafe decls))
            *arm642-float-safety*  (neq 0 (%ilogand2 $decl_float_safety decls))
            *arm642-trust-declarations* (neq 0 (%ilogand2 $decl_trustdecls
                                                          decls))))))

(defun arm642-save-non-volatile-fprs (seg n)
  (unless (eql n 0)
    (with-arm64-local-vinsn-macros (seg)
      (! push-nvfprs n (logior (ash n arm64::num-subtag-bits)
                               arm64::subtag-double-float-vector)))
    (setq *arm642-non-volatile-fpr-count* n)))

(defun arm642-restore-non-volatile-fprs (seg)
  (let* ((n *arm642-non-volatile-fpr-count*))
    (unless (eql n 0)
      (with-arm64-local-vinsn-macros (seg)
        (! pop-nvfprs n)))))

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

(defun arm642-seq-bind (seg vars initforms)
  (dolist (var vars)
    (arm642-seq-bind-var seg var (pop initforms))))

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

;; The cmp instruction takes a 12-bit immediate; return true if n fits.
(defun arm642-aimm-p (n)
  (< (ash n arm64::fixnumshift) 4096))

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
            (if (and (not (or hardopt rest keys))
                     (<= num-opt $numarm64argregs))
              (setq arg-regs (arm642-simple-opt-entry seg rev-opt rev-fixed))
              (progn
                ;; If the minumum acceptable number of args is
                ;; non-zero, ensure that at least that many were
                ;; received.  If there's an upper bound, enforce it.
                
                (when rev-fixed
                  (if (arm642-aimm-p num-fixed)
                    (! check-min-nargs num-fixed)
                    (! check-min-nargs-large num-fixed)))
                (unless (or rest keys)
                  (let* ((max (+ num-fixed num-opt)))
                    (if (arm642-aimm-p max)
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
                    (arm642-lri seg arm64::arg_y
                                (ash flags *arm642-target-fixnum-shift*))
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
          ;; Caller's context is saved; *arm642-vstack* is valid.  Might still have method-var
          ;; to worry about.
          (! save-nfp)

          (arm642-save-non-volatile-fprs seg nsaved-fprs)
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
          ;; belong in some NVR.  Put them in their NVRs, so that we
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

(defarm642 arm642-lexical-reference lexical-reference (seg vreg xfer varnode)
  (let* ((ea-or-form (var-ea varnode)))
    (if (and (acode-punted-var-p varnode) (not (fixnump ea-or-form)))
      (arm642-form seg vreg xfer ea-or-form)
      (progn
        (unless (or (typep ea-or-form 'lreg) (fixnump ea-or-form))
          (compiler-bug "bogus ref to var ~s (~s) : ~s " varnode (var-name varnode) ea-or-form))
        (arm642-do-lexical-reference seg vreg ea-or-form)
        (^)))))

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

(defun arm642-vloc-ea (n &optional vcell-p)
  (setq n (make-memory-spec (dpb memspec-frame-address memspec-type-byte n)))
  (if vcell-p
    (make-vcell-memory-spec n)
    n))

(defun arm642-set-var-ea (seg var ea)
  (setf (var-ea var) ea)
  (when (and *arm642-record-symbols* (or (typep ea 'lreg) (typep ea 'fixnum)))
    (let* ((start (enqueue-vinsn-note seg :begin-variable-scope var)))
      (push (list var (var-name var) start nil)
            *arm642-recorded-symbols*)))
  ea)

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

(defun arm642-do-return (seg)
  (let* ((*arm642-vstack* *arm642-vstack*))
    (with-arm64-local-vinsn-macros (seg)
      (progn    
        (arm642-set-vstack (arm642-unwind-stack seg $backend-return 0 0 #x7fffff))              
        (if *arm642-returning-values*
          (progn  
            (arm642-restore-nvrs seg t)
            (arm642-restore-non-volatile-fprs seg)
            (! restore-nfp)
            (! nvalret))
          (progn
            (arm642-restore-nvrs seg nil)
            (arm642-restore-non-volatile-fprs seg)
            (! restore-nfp)
            (! popj)))))
    nil))

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

(defun arm642-save-nvrs (seg n)
  (declare (ignore seg n)))

(defun arm642q-restore-nvrs (seg multiple-values-on-stack)
  (declare (ignore seg multiple-values-on-stack)))

(defun arm642-close-var (seg var)
  (let ((bits (nx-var-bits var)))
    (when (and *arm642-record-symbols*
               (or (logbitp $vbitspecial bits)
                   (not (logbitp $vbitpunted bits))))
      (let* ((info (%cdr (assq var *arm642-recorded-symbols*))))
        (unless info (compiler-bug "arm642-close-var for ~s ?" (var-name var)))
        (setf (caddr info) (close-vinsn-note seg (cadr info)))))))

(defun arm642-expand-vinsns (header current &optional sections)
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

;;; Build a register-operand for a filled-in vinsn operand.  NUMBER is
;;; the physical register number; SPEC is the template's operand spec
;;; (role class) at this position, which tells us the register view.
;;; Mirrors the class mapping in DECODE-REGISTER-OPERAND.
(defun arm642-vinsn-register-operand (number spec)
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
     (:d (arm64::fpr-ref number 64)))))

;;; Build the operand struct for one filled-in body operand.  DESC is the
;;; stored descriptor, SPEC the template's operand spec at this position,
;;; VP the expanding vinsn's variable-parts, and UNIQUE-LABELS the map
;;; from each template-local label keyword to a fresh per-expansion label
;;; object.  A (:label class) spec builds a label-operand -- naming either
;;; a backend vinsn-label passed in through VP, or a template-local label;
;;; any other spec builds a register-operand.
;;; Build an immediate-operand for a filled-in body operand.  DESC is
;;; (:imm value shift), (:imm-opnd vp-index shift), or
;;; (:imm-apply shift fn . args) where each arg is a constant or an
;;; (:opnd vp-index) hole.  SPEC is the immediate class.  Since a wild
;;; immediate's value wasn't known when the template was chosen, its range
;;; is only checkable now: matching guaranteed the class, not the fit.
(defun arm642-vinsn-immediate-operand (desc spec vp)
  (multiple-value-bind (value shift)
      (ecase (car desc)
        (:imm (values (cadr desc) (caddr desc)))
        (:imm-opnd (values (svref vp (cadr desc)) (caddr desc)))
        (:imm-apply
         (destructuring-bind (shift fn . args) (cdr desc)
           (values (apply fn (mapcar #'(lambda (a)
                                         (if (and (consp a) (eq (car a) :opnd))
                                           (svref vp (cadr a))
                                           a))
                                     args))
                   shift))))
    (let ((imm (arm64::make-immediate-operand :value value :shift shift)))
      (unless (arm64::match-immediate-operand imm spec)
        (compiler-bug "vinsn immediate ~s (shift ~s) out of range for ~
                       operand class ~s" value shift spec))
      imm)))

(defun arm642-vinsn-operand (desc spec vp unique-labels)
  (cond
    ((arm64::label-spec-p spec)
     (arm64::make-label-operand
      :name (ecase (car desc)
              (:opnd (svref vp (cadr desc)))           ;backend (vinsn) label
              (:local-label (cdr (assq (cadr desc) unique-labels))))))
    ;; A bare-keyword spec that isn't a condition is an immediate class.
    ((and (keywordp spec) (not (member spec '(:cond :cond-inv))))
     (arm642-vinsn-immediate-operand desc spec vp))
    (t
     (arm642-vinsn-register-operand
      (ecase (car desc)
        (:opnd (svref vp (cadr desc)))
        (:reg (cadr desc)))
      spec))))

;;; Expand one instruction of a vinsn template's body into a machine
;;; instruction and emit it into the section.  FORM is one simplified
;;; body element -- (template-index . operand-descriptors) -- as produced
;;; at definition time by VINSN-SIMPLIFY-INSTRUCTION.  VP is the
;;; variable-parts vector of the vinsn (instance) being expanded, with
;;; lregs already replaced by physical register numbers.  UNIQUE-LABELS
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

(defun arm642-expand-vinsn (vinsn current)
  (let* ((template (vinsn-template vinsn))
         (vp (vinsn-variable-parts vinsn))
         (nvp (vinsn-template-nvp template))
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
    (dolist (form (vinsn-template-body template))
      (cond
        ((keywordp form)
         ;; A template-local label definition point (e.g. :ok).
         (arm64::emit-label current (cdr (assq form unique-labels))))
        ((and (consp form) (typep (car form) 'fixnum))
         ;; A simplified instruction: (template-index . descriptors).
         (arm642-emit-instruction-from-vinsn form vp current unique-labels))
        (t
         ;; Not yet handled by the prototype (predicate groups, pseudo-ops,
         ;; and instructions with immediate/memory operands).  Retain a
         ;; diagnostic for now.
         (format t "~&; arm642-expand-vinsn: unhandled form ~s" form))))))
