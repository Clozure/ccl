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
(defconstant arm2-debug-lcells-bit 2)
(defparameter *arm2-target-lcell-size* 0)
(defparameter *arm2-target-node-size* 0)
(defparameter *arm2-target-fixnum-shift* 0)
(defparameter *arm2-target-node-shift* 0)
(defparameter *arm2-target-bits-in-word* 0)
(defparameter *arm2-half-fixnum-type* '(signed-byte 29))
(defparameter *arm2-target-half-fixnum-type* nil)
(defparameter *arm2-operator-supports-u8-target* ())




  

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
(defvar *arm2-compiler-register-save-label* nil)

(defparameter *arm2-tail-call-aliases*
  ()
  #| '((%call-next-method . (%tail-call-next-method . 1))) |#
  
)

(defvar *arm2-popreg-labels* nil)
(defvar *arm2-popj-labels* nil)
(defvar *arm2-valret-labels* nil)
(defvar *arm2-nilret-labels* nil)

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
(defvar *arm2-tail-label* nil)
(defvar *arm2-tail-vsp* nil)
(defvar *arm2-tail-nargs* nil)
(defvar *arm2-tail-allow* t)
(defvar *arm2-reckless* nil)
(defvar *arm2-full-safety* nil)
(defvar *arm2-float-safety* nil)
(defvar *arm2-trust-declarations* nil)
(defvar *arm2-entry-vstack* nil)
(defvar *arm2-fixed-nargs* nil)
(defvar *arm2-need-nargs* t)

(defparameter *arm2-inhibit-register-allocation* nil)
(defvar *arm2-record-symbols* nil)
(defvar *arm2-recorded-symbols* nil)
(defvar *arm2-emitted-source-notes* nil)

(defvar *arm2-result-reg* arm::arg_z)
(defvar *arm2-gpr-locations* nil)
(defvar *arm2-gpr-locations-valid-mask* 0)







(declaim (fixnum *arm2-vstack* *arm2-cstack*))

 


;;; Before any defarm2's, make the *arm2-specials* vector.

(defvar *arm2-all-lcells* ())




     
(defun arm2-free-lcells ()
  (without-interrupts 
   (let* ((prev (pool.data *lcell-freelist*)))
     (dolist (r *arm2-all-lcells*)
       (setf (lcell-kind r) prev
             prev r))
     (setf (pool.data *lcell-freelist*) prev)
     (setq *arm2-all-lcells* nil))))

(defun arm2-note-lcell (c)
  (push c *arm2-all-lcells*)
  c)

(defvar *arm2-top-vstack-lcell* ())
(defvar *arm2-bottom-vstack-lcell* ())

(defun arm2-new-lcell (kind parent width attributes info)
  (arm2-note-lcell (make-lcell kind parent width attributes info)))

(defun arm2-new-vstack-lcell (kind width attributes info)
  (setq *arm2-top-vstack-lcell* (arm2-new-lcell kind *arm2-top-vstack-lcell* width attributes info)))

(defun arm2-reserve-vstack-lcells (n)
  (dotimes (i n) (arm2-new-vstack-lcell :reserved *arm2-target-lcell-size* 0 nil)))

(defun arm2-vstack-mark-top ()
  (arm2-new-lcell :tos *arm2-top-vstack-lcell* 0 0 nil))

;;; Alist mapping VARs to lcells/lregs
(defvar *arm2-var-cells* ())

(defun arm2-note-var-cell (var cell)
  ;(format t "~& ~s -> ~s" (var-name var) cell)
  (push (cons var cell) *arm2-var-cells*))

(defun arm2-note-top-cell (var)
  (arm2-note-var-cell var *arm2-top-vstack-lcell*))

(defun arm2-lookup-var-cell (var)
  (or (cdr (assq var *arm2-var-cells*))
      (and nil (warn "Cell not found for ~s" (var-name var)))))

(defun arm2-collect-lcells (kind &optional (bottom *arm2-bottom-vstack-lcell*) (top *arm2-top-vstack-lcell*))
  (do* ((res ())
        (cell top (lcell-parent cell)))
       ((eq cell bottom) res)
    (if (null cell)
      (compiler-bug "Horrible compiler bug.")
      (if (eq (lcell-kind cell) kind)
        (push cell res)))))



  
;;; ensure that lcell's offset matches what we expect it to.
;;; For bootstrapping.

(defun arm2-ensure-lcell-offset (c expected)
  (if c (= (calc-lcell-offset c) expected) (zerop expected)))

(defun arm2-check-lcell-depth (&optional (context "wherever"))
  (when (logbitp arm2-debug-verbose-bit *arm2-debug-mask*)
    (let* ((depth (calc-lcell-depth *arm2-top-vstack-lcell*)))
      (or (= depth *arm2-vstack*)
          (warn "~a: lcell depth = ~d, vstack = ~d" context depth *arm2-vstack*)))))

(defun arm2-do-lexical-reference (seg vreg ea)
  (when vreg
    (with-arm-local-vinsn-macros (seg vreg) 
      (if (memory-spec-p ea)
        (ensuring-node-target (target vreg)
          (progn
            (arm2-stack-to-register seg ea target)
            (if (addrspec-vcell-p ea)
              (! vcell-ref target target))))
        (<- ea)))))

(defun arm2-do-lexical-setq (seg vreg ea valreg)
  (with-arm-local-vinsn-macros (seg vreg)
    (cond ((typep ea 'lreg)
            (arm2-copy-register seg ea valreg))
          ((addrspec-vcell-p ea)     ; closed-over vcell
           (arm2-copy-register seg arm::arg_z valreg)
           (arm2-stack-to-register seg ea arm::arg_x)
           (arm2-lri seg arm::arg_y 0)
           (! call-subprim-3 arm::arg_z (subprim-name->offset '.SPgvset) arm::arg_x arm::arg_y arm::arg_z))
          ((memory-spec-p ea)    ; vstack slot
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
  (condition-to-arm-cr-bit (cadr cond)))

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
    (#.arm::arm-cond-ge arm::arm-cond-le)))

    
    

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
           (*next-lcell-id* -1)
           (*arm2-open-code-inline* nil)
           (*arm2-optimize-for-space* nil)
           (*arm2-register-restore-count* nil)
           (*arm2-compiler-register-save-label* nil)
           (*arm2-register-restore-ea* nil)
           (*arm2-vstack* 0)
           (*arm2-cstack* 0)
	   (*arm2-target-lcell-size* (arch::target-lisp-node-size (backend-target-arch *target-backend*)))
           (*arm2-target-fixnum-shift* (arch::target-fixnum-shift (backend-target-arch *target-backend*)))
           (*arm2-target-node-shift* (arch::target-word-shift  (backend-target-arch *target-backend*)))
           (*arm2-target-bits-in-word* (arch::target-nbits-in-word (backend-target-arch *target-backend*)))
	   (*arm2-target-node-size* *arm2-target-lcell-size*)
           (*arm2-target-half-fixnum-type* *arm2-half-fixnum-type*)
           (*arm2-all-lcells* ())
           (*arm2-top-vstack-lcell* nil)
           (*arm2-bottom-vstack-lcell* (arm2-new-vstack-lcell :bottom 0 0 nil))
           (*arm2-var-cells* nil)
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
           (*arm2-popj-labels* nil)
           (*arm2-popreg-labels* nil)
           (*arm2-valret-labels* nil)
           (*arm2-nilret-labels* nil)
           (*arm2-undo-count* 0)
           (*backend-labels* (arm2-make-stack 64 target::subtag-simple-vector))
           (*arm2-undo-stack* (arm2-make-stack 64  target::subtag-simple-vector))
           (*arm2-undo-because* (arm2-make-stack 64))
           (*backend-immediates* (arm2-make-stack 64  target::subtag-simple-vector))
           (*arm2-entry-label* nil)
           (*arm2-tail-label* nil)
           (*arm2-tail-vsp* nil)
           (*arm2-tail-nargs* nil)
           (*arm2-inhibit-register-allocation* nil)
           (*arm2-tail-allow* t)
           (*arm2-reckless* nil)
	   (*arm2-full-safety* nil)
           (*arm2-float-safety* nil)
           (*arm2-trust-declarations* t)
           (*arm2-entry-vstack* nil)
           (*arm2-fixed-nargs* nil)
           (*arm2-need-nargs* t)
           (fname (afunc-name afunc))
           (*arm2-entry-vsp-saved-p* nil)
           (*arm2-vcells* (arm2-ensure-binding-indices-for-vcells (afunc-vcells afunc)))
           (*arm2-fcells* (afunc-fcells afunc))
           *arm2-recorded-symbols*
           (*arm2-emitted-source-notes* '())
           (*arm2-gpr-locations-valid-mask* 0)
           (*arm2-gpr-locations* (make-array 16 :initial-element nil)))
      (declare (dynamic-extent *arm2-gpr-locations*))
      (set-fill-pointer
       *backend-labels*
       (set-fill-pointer
        *arm2-undo-stack*
        (set-fill-pointer 
         *arm2-undo-because*
         (set-fill-pointer
          *backend-immediates* 0))))
      (backend-get-next-label)          ; start @ label 1, 0 is confused with NIL in compound cd
      (with-dll-node-freelist (vinsns *vinsn-freelist*)
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
                   (let* ((arm::*lap-labels* nil)
                          (arm::*called-subprim-jmp-labels* nil)
                          debug-info)
                     (arm2-expand-vinsns vinsns code)
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
                            bits))
                     (when (getf debug-info 'pc-source-map)
                       (setf (getf debug-info 'pc-source-map) (arm2-generate-pc-source-map debug-info)))
                     (when (getf debug-info 'function-symbol-map)
                       (setf (getf debug-info 'function-symbol-map) (arm2-digest-symbols))))))
          (backend-remove-labels))))
    afunc))

(defun arm2-xmake-function (code imms bits)
  (collect ((lap-imms))
    (dotimes (i (length imms))
      (lap-imms (cons (aref imms i) i)))
    (let* ((arm::*arm-constants* (lap-imms)))
      (arm-lap-generate-code code
                             (arm::arm-finalize code)
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
  (setq *arm2-gpr-locations-valid-mask* 0))

(defun arm2-update-regmap (vinsn)
  (if (vinsn-attribute-p vinsn :call)
    (arm2-invalidate-regmap)
    (setq *arm2-gpr-locations-valid-mask* (logandc2 *arm2-gpr-locations-valid-mask* (vinsn-gprs-set vinsn))))
  vinsn)

(defun arm2-regmap-note-store (gpr loc)
  (let* ((gpr (%hard-regspec-value gpr)))
    ;; Any other GPRs that had contained loc no longer do so.
    (dotimes (i 16)
      (unless (eql i gpr)
        (when (and (logbitp i *arm2-gpr-locations-valid-mask*)
                   (memq loc (svref *arm2-gpr-locations* i)))
          (when (null (setf (svref *arm2-gpr-locations* i)
                            (delete loc (svref *arm2-gpr-locations* i))))
            (setq *arm2-gpr-locations-valid-mask* (logandc2 *arm2-gpr-locations-valid-mask* (ash 1 i)))))))
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

(defmacro with-arm2-saved-regmap ((mask map) &body body)
  `(let* ((,mask *arm2-gpr-locations-valid-mask*)
          (,map (make-array 16 :initial-element nil)))
    (declare (dynamic-extent ,map))
    (arm2-copy-regmap ,mask *arm2-gpr-locations* ,map)
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
  (let* ((label (vinsn-note-label note))
         (lap-label (if label (vinsn-label-info label))))
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
            *arm2-float-safety* (not *arm2-reckless*)
            *arm2-trust-declarations* (neq 0 (%ilogand2 $decl_trustdecls decls))))))





          
    
;;; Vpush the last N non-volatile-registers.
;;; Could use a STM here, especially if N is largish or optimizing for space.
#+maybe-someday
(defun arm2-save-nvrs (seg n)
  (declare (fixnum n))
  (when (> n 0)
    (setq *arm2-compiler-register-save-label* (arm2-emit-note seg :regsave))
    (with-arm-local-vinsn-macros (seg)
      (if *arm2-open-code-inline*
	(! save-nvrs-individually (- 32 n))
	(! save-nvrs (- 32 n))))
    (dotimes (i n)
      (arm2-new-vstack-lcell :regsave *arm2-target-lcell-size* 0 (- arm::save0 i)))
    (incf *arm2-vstack* (the fixnum (* n *arm2-target-node-size*)))
    (setq *arm2-register-restore-ea* *arm2-vstack*
          *arm2-register-restore-count* n)))


;;; If there are an indefinite number of args/values on the vstack,
;;; we have to restore from a register that matches the compiler's
;;; notion of the vstack depth.  This can be computed by the caller 
;;; (sum of vsp & nargs, or copy of vsp  before indefinite number of 
;;; args pushed, etc.)
;;; We DON'T try to compute this from the saved context, since the
;;; saved vsp may belong to a different stack segment.  (It's cheaper
;;; to compute/copy than to load it, anyway.)

#+maybe-later-that-same-day
(defun arm2-restore-nvrs (seg ea nregs &optional from-fp)
  (when (null from-fp)
    (setq from-fp arm::vsp))
  (when (and ea nregs)
    (with-arm-local-vinsn-macros (seg)
      (let* ((first (- 32 nregs)))
        (declare (fixnum first))
        (! restore-nvrs first from-fp (- *arm2-vstack* ea))))))



(defun arm2-bind-lambda (seg lcells req opt rest keys auxen optsupvloc passed-in-regs lexpr &optional inherited
                             &aux (vloc 0) (numopt (list-length (%car opt)))
                             (nkeys (list-length (%cadr keys))) 
                             reg)
  (declare (fixnum vloc))
  (arm2-check-lcell-depth)
  (dolist (arg inherited)
    (if (memq arg passed-in-regs)
      (arm2-set-var-ea seg arg (var-ea arg))
      (let* ((lcell (pop lcells)))
        (if (setq reg (nx2-assign-register-var arg))
          (arm2-init-regvar seg arg reg (arm2-vloc-ea vloc))
          (arm2-bind-var seg arg vloc lcell))
        (setq vloc (%i+ vloc *arm2-target-node-size*)))))
  (dolist (arg req)
    (if (memq arg passed-in-regs)
      (arm2-set-var-ea seg arg (var-ea arg))
      (let* ((lcell (pop lcells)))
        (if (setq reg (nx2-assign-register-var arg))
          (arm2-init-regvar seg arg reg (arm2-vloc-ea vloc))
          (arm2-bind-var seg arg vloc lcell))
        (setq vloc (%i+ vloc *arm2-target-node-size*)))))
  (when opt
    (if (arm2-hard-opt-p opt)
      (setq vloc (apply #'arm2-initopt seg vloc optsupvloc lcells (nthcdr (- (length lcells) numopt) lcells) opt)
            lcells (nthcdr numopt lcells))

      (dolist (var (%car opt))
        (if (memq var passed-in-regs)
          (arm2-set-var-ea seg var (var-ea var))
          (let* ((lcell (pop lcells)))
            (if (setq reg (nx2-assign-register-var var))
              (arm2-init-regvar seg var reg (arm2-vloc-ea vloc))
              (arm2-bind-var seg var vloc lcell))
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
              (arm2-vpush-register seg nargs-cell :reserved)
              (arm2-note-top-cell rest)
              (arm2-bind-var seg rest loc *arm2-top-vstack-lcell*)))))
      (let* ((rvloc (+ vloc (* 2 *arm2-target-node-size* nkeys))))
        (if (setq reg (nx2-assign-register-var rest))
          (arm2-init-regvar seg rest reg (arm2-vloc-ea rvloc))
          (arm2-bind-var seg rest rvloc (pop lcells))))))
  (when keys
    (apply #'arm2-init-keys seg vloc lcells keys))  
  (arm2-seq-bind seg (%car auxen) (%cadr auxen)))

(defun arm2-initopt (seg vloc spvloc lcells splcells vars inits spvars)
  (with-arm-local-vinsn-macros (seg)
    (dolist (var vars vloc)
      (let* ((initform (pop inits))
             (spvar (pop spvars))
             (lcell (pop lcells))
             (splcell (pop splcells))
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
          (arm2-bind-var seg var vloc lcell))
        (when spvar
          (if (setq reg (nx2-assign-register-var spvar))
            (arm2-init-regvar seg spvar reg (arm2-vloc-ea spvloc))
            (arm2-bind-var seg spvar spvloc splcell))))
      (setq vloc (%i+ vloc *arm2-target-node-size*))
      (if spvloc (setq spvloc (%i+ spvloc *arm2-target-node-size*))))))

(defun arm2-init-keys (seg vloc lcells allow-others keyvars keysupp keyinits keykeys)
  (declare (ignore keykeys allow-others))
  (with-arm-local-vinsn-macros (seg)
    (dolist (var keyvars)
      (let* ((spvar (pop keysupp))
             (initform (pop keyinits))
             (reg (nx2-assign-register-var var))
             (regloadedlabel (if reg (backend-get-next-label)))
             (var-lcell (pop lcells))
             (sp-lcell (pop lcells))
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
          (arm2-bind-var seg var vloc var-lcell))
        (when spvar
          (if (setq reg (nx2-assign-register-var spvar))
            (arm2-init-regvar seg spvar reg (arm2-vloc-ea sploc))
            (arm2-bind-var seg spvar sploc sp-lcell))))
      (setq vloc (%i+ vloc (* 2 *arm2-target-node-size*))))))

;;; Vpush register r, unless var gets a globally-assigned register.
;;; Return NIL if register was vpushed, else var.
(defun arm2-vpush-arg-register (seg reg var)
  (when var
    (if (var-nvr var)
      var
      (progn 
        (arm2-vpush-register seg reg :reserved)
        nil))))


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
      (if (<= nargs $numarmargregs)       ; caller didn't vpush anything
        (! save-lisp-context-vsp)
        (let* ((offset (* (the fixnum (- nargs $numarmargregs)) *arm2-target-node-size*)))
          (declare (fixnum offset))
          (! save-lisp-context-offset offset)))
      (destructuring-bind (&optional zvar yvar xvar &rest stack-args) revargs
        (declare (ignore xvar yvar))
        (let* ((nstackargs (length stack-args)))
          (arm2-set-vstack (* nstackargs *arm2-target-node-size*))
          (dotimes (i nstackargs)
            (arm2-new-vstack-lcell :reserved *arm2-target-lcell-size* 0 nil))
          (if (>= nargs 3)
            (progn
              (! vpush-xyz)
              (arm2-regmap-note-store arm::arg_x *arm2-vstack*)
              (arm2-regmap-note-store arm::arg_y (+ *arm2-target-node-size* *arm2-vstack*))
              (arm2-regmap-note-store arm::arg_z (+ (* 2 *arm2-target-node-size*) *arm2-vstack*))
              (dotimes (i 3)
                (arm2-new-vstack-lcell :reserved *arm2-target-lcell-size* 0 nil))
              (arm2-adjust-vstack (* 3 *arm2-target-node-size*)))
            (if (= nargs 2)
              (progn
                (! vpush-yz)
                (arm2-regmap-note-store arm::arg_y *arm2-vstack*)
                (arm2-regmap-note-store arm::arg_z (+ *arm2-target-node-size* *arm2-vstack*))
                (dotimes (i 2)
                  (arm2-new-vstack-lcell :reserved *arm2-target-lcell-size* 0 nil))
                (arm2-adjust-vstack (* 2 *arm2-target-node-size*)))
              (if (= nargs 1)
                (push (arm2-vpush-arg-register seg ($ arm::arg_z) zvar) reg-vars))))))
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


(defun arm2-structured-initopt (seg lcells vloc context vars inits spvars)
  (with-arm-local-vinsn-macros (seg)
    (dolist (var vars vloc)
      (let* ((initform (pop inits))
             (spvar (pop spvars))
             (spvloc (%i+ vloc *arm2-target-node-size*))
             (var-lcell (pop lcells))
             (sp-reg ($ arm::arg_z))
             (sp-lcell (pop lcells)))
        (unless (nx-null initform)
          (arm2-stack-to-register seg (arm2-vloc-ea spvloc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (arm2-compare-register-to-nil seg crf (arm2-make-compound-cd 0 skipinitlabel) sp-reg arm::arm-cond-eq t))
            (arm2-register-to-stack seg (arm2-one-untargeted-reg-form seg initform ($ arm::arg_z)) (arm2-vloc-ea vloc))
            (@ skipinitlabel)))
        (arm2-bind-structured-var seg var vloc var-lcell context)
        (when spvar
          (arm2-bind-var seg spvar spvloc sp-lcell)))
      (setq vloc (%i+ vloc (* 2 *arm2-target-node-size*))))))



(defun arm2-structured-init-keys (seg lcells vloc context allow-others keyvars keysupp keyinits keykeys)
  (declare (ignore keykeys allow-others))
  (with-arm-local-vinsn-macros (seg)
    (dolist (var keyvars)
      (let* ((spvar (pop keysupp))
             (initform (pop keyinits))
             (sploc (%i+ vloc *arm2-target-node-size*))
             (var-lcell (pop lcells))
             (sp-reg ($ arm::arg_z))
             (sp-lcell (pop lcells)))
        (unless (nx-null initform)
          (arm2-stack-to-register seg (arm2-vloc-ea sploc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (arm2-compare-register-to-nil seg crf (arm2-make-compound-cd 0 skipinitlabel) sp-reg arm::arm-cond-eq t))
            (arm2-register-to-stack seg (arm2-one-untargeted-reg-form seg initform ($ arm::arg_z)) (arm2-vloc-ea vloc))
            (@ skipinitlabel)))
        (arm2-bind-structured-var seg var vloc var-lcell context)
        (when spvar
          (arm2-bind-var seg spvar sploc sp-lcell)))
      (setq vloc (%i+ vloc (* 2 *arm2-target-node-size*))))))

(defun arm2-vloc-ea (n &optional vcell-p)
  (setq n (make-memory-spec (dpb memspec-frame-address memspec-type-byte n)))
  (if vcell-p
    (make-vcell-memory-spec n)
    n))


(defun arm2-acode-operator-function (form)
  (or (and (acode-p form)
           (svref *arm2-specials* (%ilogand #.operator-id-mask (acode-operator form))))
      (compiler-bug "arm2-form ? ~s" form)))

(defmacro with-note ((form-var seg-var &rest other-vars) &body body)
  (let* ((note (gensym "NOTE"))
         (code-note (gensym "CODE-NOTE"))
         (source-note (gensym "SOURCE-NOTE"))
         (start (gensym "START"))
         (end (gensym "END"))
         (with-note-body (gensym "WITH-NOTE-BODY")))
    `(flet ((,with-note-body (,form-var ,seg-var ,@other-vars) ,@body))
       (let ((,note (acode-note ,form-var)))
         (if ,note
           (let* ((,code-note (and (code-note-p ,note) ,note))
                  (,source-note (if ,code-note
                                  (code-note-source-note ,note)
                                  ,note))
                  (,start (and ,source-note
                               (arm2-emit-note ,seg-var :source-location-begin ,source-note))))
             (prog2
                 (when ,code-note
                   (with-arm-local-vinsn-macros (,seg-var)
                     (arm2-store-immediate ,seg-var ,code-note arm::temp0)
                     (with-node-temps (arm::temp0) (zero)
                       (! lri zero 0)
                       (! misc-set-c-node ($ zero) ($ arm::temp0) 1))))
                 (,with-note-body ,form-var ,seg-var ,@other-vars)
               (when ,source-note
                 (let ((,end (arm2-emit-note ,seg-var :source-location-end)))
                   (setf (vinsn-note-peer ,start) ,end
                         (vinsn-note-peer ,end) ,start)
                   (push ,start *arm2-emitted-source-notes*)))))
           (,with-note-body ,form-var ,seg-var ,@other-vars))))))

(defun arm2-toplevel-form (seg vreg xfer form)
  (let* ((code-note (acode-note form))
         (args (if code-note `(,@(%cdr form) ,code-note) (%cdr form))))
    (apply (arm2-acode-operator-function form) seg vreg xfer args)))

(defun arm2-form (seg vreg xfer form)
  (with-note (form seg vreg xfer)
    (if (nx-null form)
      (arm2-nil seg vreg xfer)
      (if (nx-t form)
        (arm2-t seg vreg xfer)
        (let ((fn (arm2-acode-operator-function form))
              (op (acode-operator form)))
          (if (and (null vreg)
                   (%ilogbitp operator-acode-subforms-bit op)
                   (%ilogbitp operator-assignment-free-bit op))
            (dolist (f (%cdr form) (arm2-branch seg xfer nil))
              (arm2-form seg nil nil f ))
            (apply fn seg vreg xfer (%cdr form))))))))

;;; dest is a float reg - form is acode
(defun arm2-form-float (seg freg xfer form)
  (declare (ignore xfer))
  (with-note (form seg freg)
    (when (or (nx-null form)(nx-t form))(compiler-bug "arm2-form to freg ~s" form))
    (when (and (= (get-regspec-mode freg) hard-reg-class-fpr-mode-double)
               (arm2-form-typep form 'double-float))
                                        ; kind of screwy - encoding the source type in the dest register spec
      (set-node-regspec-type-modes freg hard-reg-class-fpr-type-double))
    (let* ((fn (arm2-acode-operator-function form)))
      (apply fn seg freg nil (%cdr form)))))



(defun arm2-form-typep (form type)
  (acode-form-typep form type *arm2-trust-declarations*)
)

(defun arm2-form-type (form)
  (acode-form-type form *arm2-trust-declarations*))
  
(defun arm2-use-operator (op seg vreg xfer &rest forms)
  (declare (dynamic-extent forms))
  (apply (svref *arm2-specials* (%ilogand operator-id-mask op)) seg vreg xfer forms))

;;; Returns true iff lexical variable VAR isn't setq'ed in FORM.
;;; Punts a lot ...
(defun arm2-var-not-set-by-form-p (var form)
  (or (not (%ilogbitp $vbitsetq (nx-var-bits var)))
      (arm2-setqed-var-not-set-by-form-p var form)))

(defun arm2-setqed-var-not-set-by-form-p (var form)
  (setq form (acode-unwrapped-form form))
  (or (atom form)
      (arm-constant-form-p form)
      (arm2-lexical-reference-p form)
      (let ((op (acode-operator form))
            (subforms nil))
        (if (eq op (%nx1-operator setq-lexical))
          (and (neq var (cadr form))
               (arm2-setqed-var-not-set-by-form-p var (caddr form)))
          (and (%ilogbitp operator-side-effect-free-bit op)
               (flet ((not-set-in-formlist (formlist)
                        (dolist (subform formlist t)
                          (unless (arm2-setqed-var-not-set-by-form-p var subform) (return)))))
                 (if
                   (cond ((%ilogbitp operator-acode-subforms-bit op) (setq subforms (%cdr form)))
                         ((%ilogbitp operator-acode-list-bit op) (setq subforms (cadr form))))
                   (not-set-in-formlist subforms)
                   (and (or (eq op (%nx1-operator call))
                            (eq op (%nx1-operator lexical-function-call)))
                        (arm2-setqed-var-not-set-by-form-p var (cadr form))
                        (setq subforms (caddr form))
                        (not-set-in-formlist (car subforms))
                        (not-set-in-formlist (cadr subforms))))))))))
  
(defun arm2-nil (seg vreg xfer)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if (arm2-for-value-p vreg)
      (ensuring-node-target (target vreg)
        (! load-nil target)))
    (arm2-branch seg (arm2-cd-false xfer) vreg)))

(defun arm2-t (seg vreg xfer)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if (arm2-for-value-p vreg)
      (ensuring-node-target (target vreg)
        (! load-t target)))
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


;;; Emit a note at the end of the segment.
(defun arm2-emit-note (seg class &rest info)
  (declare (dynamic-extent info))
  (let* ((note (make-vinsn-note class info)))
    (append-dll-node (vinsn-note-label note) seg)
    note))

;;; Emit a note immediately before the target vinsn.
(defun arm-prepend-note (vinsn class &rest info)
  (declare (dynamic-extent info))
  (let* ((note (make-vinsn-note class info)))
    (insert-dll-node-before (vinsn-note-label note) vinsn)
    note))

(defun arm2-close-note (seg note)
  (let* ((end (close-vinsn-note note)))
    (append-dll-node (vinsn-note-label end) seg)
    end))


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

  



(defun arm2-stack-to-register (seg memspec reg)
  (with-arm-local-vinsn-macros (seg)
    (let* ((offset (memspec-frame-address-offset memspec))
           (mask *arm2-gpr-locations-valid-mask*)
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
                   vinsn)))))))

(defun arm2-lcell-to-register (seg lcell reg)
  (with-arm-local-vinsn-macros (seg)
    (! lcell-load reg lcell (arm2-vstack-mark-top))))

(defun arm2-register-to-lcell (seg reg lcell)
  (with-arm-local-vinsn-macros (seg)
    (! lcell-store reg lcell (arm2-vstack-mark-top))))

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
              (with-imm-temps () ((bitsreg :u32))
                (! lri bitsreg bits)
                (! load-single-float-constant vreg bitsreg)))
            (multiple-value-bind (high low) (arm2-double-float-bits form)
              (declare (integer high low))
              (with-imm-temps () ((highreg :u32) (lowreg :u32))
                (! lri highreg high)
                (! lri lowreg low)
                (! load-double-float-constant vreg highreg lowreg)))))
        (if (and (typep form '(unsigned-byte 32))
                 (= (hard-regspec-class vreg) hard-reg-class-gpr)
                 (= (get-regspec-mode vreg)
                    hard-reg-class-gpr-mode-u32))
          (arm2-lri seg vreg form)
          (ensuring-node-target
           (target vreg)
           (if (characterp form)
             (! load-character-constant target (char-code form))
             (arm2-store-immediate seg form target)))))
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
      (setq form (caadr form)))
    (when (acode-p form)
      (let ((op (acode-operator form)))
        (if (and (eq op (%nx1-operator local-go))
                 (arm2-equal-encodings-p (%caddr (%cadr form)) current-stack))
          (%cadr (%cadr form))
          (if (and (eq op (%nx1-operator local-return-from))
                   (nx-null (caddr form)))
            (let ((tagdata (car (cadr form))))
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
                   (let ((values (cadr form)))
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
        (! call-subprim (subprim-name->offset '.SPmakes32))
        (arm2-copy-register seg node-dest arg_z)))))



(defun arm2-box-u32 (seg node-dest u32-src)
  (with-arm-local-vinsn-macros (seg)
    (if *arm2-open-code-inline*
      (! u32->integer node-dest u32-src)
      (let* ((arg_z ($ arm::arg_z))
             (imm0 ($ arm::imm0 :mode :u32)))
        (arm2-copy-register seg imm0 u32-src)
        (! call-subprim (subprim-name->offset '.SPmakeu32))
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
               (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-32-bit-constant-index arch)))
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
           (with-fp-target () (fp-val :double-float)
             (with-imm-target () (temp :u64)
               (if (and (eql vreg-class hard-reg-class-fpr)
                        (eql vreg-mode hard-reg-class-fpr-mode-double))
                 (setq fp-val vreg)
                 (if (eql vreg-class hard-reg-class-gpr)
                   (if (or (and is-signed
                                (eql vreg-mode hard-reg-class-gpr-mode-s64))
                           (and (not is-signed)
                                (eql vreg-mode hard-reg-class-gpr-mode-u64)))
                     (setf temp vreg temp-is-vreg t)
                     (if is-signed
                       (set-regspec-mode temp hard-reg-class-gpr-mode-s64)))))
               (case type-keyword
                 (:double-float-vector
                  (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                    (! misc-ref-c-double-float fp-val src index-known-fixnum)
                    (with-imm-target () idx-reg
                      (if index-known-fixnum
                        (arm2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3)))
                        (! scale-64bit-misc-index idx-reg unscaled-idx))
                      (! misc-ref-double-float fp-val src idx-reg)))
                  (if (eq vreg-class hard-reg-class-fpr)
                    (<- fp-val)
                    (ensuring-node-target (target vreg)
                      (! double->heap target fp-val))))
                 ((:signed-64-bit-vector :fixnum-vector)
                  (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                    (! misc-ref-c-s64 temp src index-known-fixnum)
                    (with-imm-target () idx-reg
                      (if index-known-fixnum
                        (arm2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3)))
                        (! scale-64bit-misc-index idx-reg unscaled-idx))
                      (! misc-ref-s64 temp src idx-reg)))
                  (if (eq type-keyword :fixnum-vector)
                    (ensuring-node-target (target vreg)
                      (! box-fixnum target temp))
                    (unless temp-is-vreg
                      (ensuring-node-target (target vreg)
                        (! s64->integer target temp)))))
                 (t
                  (if (and index-known-fixnum (<= index-known-fixnum (arch::target-max-64-bit-constant-index arch)))
                    (! misc-ref-c-u64 temp src index-known-fixnum)
                    (with-imm-target () idx-reg
                      (if index-known-fixnum
                        (arm2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3)))
                        (! scale-64bit-misc-index idx-reg unscaled-idx))
                      (! misc-ref-u64  temp src idx-reg)))
                  (unless temp-is-vreg
                    (ensuring-node-target (target vreg)
                      (! u64->integer target temp))))))))
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



(defun arm2-aset2 (seg vreg xfer  array i j new safe type-keyword dim0 dim1)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (arm2-constant-value-ok-for-type-keyword type-keyword new))
           (needs-memoization (and is-node (arm2-acode-needs-memoization new)))
           (src)
           (unscaled-i)
           (unscaled-j)
           (val-reg (arm2-target-reg-for-aset vreg type-keyword))
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
            (arm2-two-targeted-reg-forms seg array ($ arm::temp0) new val-reg))
          (multiple-value-setq (src unscaled-i unscaled-j val-reg)
            (if needs-memoization
              (progn
                (arm2-four-targeted-reg-forms seg
                                              array ($ arm::temp0)
                                              i ($ arm::arg_x)
                                              j ($ arm::arg_y)
                                              new val-reg)
                (values ($ arm::temp0) ($ arm::arg_x) ($ arm::arg_y) ($ arm::arg_z)))
              (arm2-four-untargeted-reg-forms seg
                                              array ($ arm::temp0)
                                              i ($ arm::arg_x)
                                              j ($ arm::arg_y)
                                              new val-reg))))
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*))
          (when (and (= (hard-regspec-class val-reg) hard-reg-class-gpr)
                     (logbitp (hard-regspec-value val-reg)
                              *backend-imm-temps*))
            (use-imm-temp (hard-regspec-value val-reg)))
          (when safe      
            (when (typep safe 'fixnum)
              (let* ((*available-backend-node-temps* *available-backend-node-temps*))
                (when unscaled-i
                  (use-node-temp (hard-regspec-value unscaled-i)))
                (when unscaled-j
                  (use-node-temp (hard-regspec-value unscaled-j)))
                (with-node-target (src val-reg) expected
                  (! lri expected
                     (ash (dpb safe target::arrayH.flags-cell-subtag-byte
                               (ash 1 $arh_simple_bit))
                          arm::fixnumshift))
                  (! trap-unless-simple-array-2 src expected))))
            (unless i-known-fixnum
              (! trap-unless-fixnum unscaled-i))
            (unless j-known-fixnum
              (! trap-unless-fixnum unscaled-j)))
          (with-imm-target () dim1
            (let* ((idx-reg ($ arm::arg_y)))
              (unless constidx
                (if safe                  
                  (! check-2d-bound dim1 unscaled-i unscaled-j src)
                  (! 2d-dim1 dim1 src))
                (! 2d-unscaled-index idx-reg dim1 unscaled-i unscaled-j))
              (let* ((v ($ arm::arg_x)))
                (! array-data-vector-ref v src)
                (arm2-vset1 seg vreg xfer type-keyword v idx-reg constidx val-reg (arm2-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization)))))))))


(defun arm2-aset3 (seg vreg xfer  array i j k new safe type-keyword  dim0 dim1 dim2)
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
              (with-node-target (src unscaled-i unscaled-j unscaled-k val-reg) expected
                (! lri expected (ash (dpb safe target::arrayH.flags-cell-subtag-byte
                                          (ash 1 $arh_simple_bit))
                                     arm::fixnumshift))
              (! trap-unless-simple-array-3
                 src
                 expected)))
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
                  (! array-data-vector-ref v src)
                  (arm2-vset1 seg vreg xfer type-keyword v idx-reg constidx val-reg (arm2-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization))))))))))

(defun arm2-aref2 (seg vreg xfer array i j safe typekeyword &optional dim0 dim1)
  (with-arm-local-vinsn-macros (seg vreg xfer)
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
              (! lri expected (ash (dpb safe target::arrayH.flags-cell-subtag-byte
                                        (ash 1 $arh_simple_bit))
                                   arm::fixnumshift))
              (! trap-unless-simple-array-2 src expected))))
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
            (arm2-vref1 seg vreg xfer typekeyword v idx-reg constidx)))))))



(defun arm2-aref3 (seg vreg xfer array i j k safe typekeyword &optional dim0 dim1 dim2)
  (with-arm-local-vinsn-macros (seg vreg xfer)
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
        (setq src (arm2-one-targeted-reg-form seg array ($ arm::arg_z)))
        (multiple-value-setq (src unscaled-i unscaled-j unscaled-k)
          (arm2-four-untargeted-reg-forms seg
                                           array arm::temp0
                                           i arm::arg_x
                                           j arm::arg_y
                                           k arm::arg_z)))
      (when safe        
        (when (typep safe 'fixnum)
          (with-node-target (src unscaled-i unscaled-j unscaled-k) expected
            (! lri expected (ash (dpb safe target::arrayH.flags-cell-subtag-byte
                                      (ash 1 $arh_simple_bit))
                                 arm::fixnumshift))
            (! trap-unless-simple-array-3 src expected)))
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
          (arm2-vref1 seg vreg xfer typekeyword v idx-reg constidx))))))


(defun arm2-constant-value-ok-for-type-keyword (type-keyword form)
  (if (and (acode-p (setq form (acode-unwrapped-form form)))
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
      (if typep val))))

(defun arm2-target-reg-for-aset (vreg type-keyword)
  (let* ((arch (backend-target-arch *target-backend*))
         (is-node (member type-keyword (arch::target-gvector-types arch)))
         (is-1-bit (member type-keyword (arch::target-1-bit-ivector-types arch)))
         (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
         (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
         (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
         (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
         (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector)))
         (vreg-class (if vreg (hard-regspec-class vreg)))
         (vreg-mode (if (or (eql vreg-class hard-reg-class-gpr)
                            (eql vreg-class hard-reg-class-fpr))
                      (get-regspec-mode vreg)))
         (next-imm-target (available-imm-temp  *available-backend-imm-temps*))
         (next-fp-target (available-fp-temp *available-backend-fp-temps*))
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

(defun arm2-unboxed-reg-for-aset (seg type-keyword result-reg safe constval)
  (with-arm-local-vinsn-macros (seg)
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
                   reg)))
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
           (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector))))
      (cond ((and is-node node-value-needs-memoization)
             (unless (and (eql (hard-regspec-value src) arm::arg_x)
                          (eql (hard-regspec-value unscaled-idx) arm::arg_y)
                          (eql (hard-regspec-value val-reg) arm::arg_z))
               (compiler-bug "Bug: invalid register targeting for gvset: ~s" (list src unscaled-idx val-reg)))
             (! call-subprim-3 val-reg (subprim-name->offset '.SPgvset) src unscaled-idx val-reg))
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
               (is-64-bit
                (with-imm-target (arm::imm0 arm::imm1) scaled-idx
                  (if (and index-known-fixnum
                           (<= index-known-fixnum
                               (arch::target-max-64-bit-constant-index arch)))
                    (! misc-set-c-double-float unboxed-val-reg src index-known-fixnum)
                    (progn
                      (if index-known-fixnum
                        (arm2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-dfloat-offset arch) (ash index-known-fixnum 3)))
                        (! scale-64bit-misc-index scaled-idx unscaled-idx))
                      (! misc-set-double-float unboxed-val-reg src scaled-idx)))))
                 (t
                  (with-imm-target (unboxed-val-reg) scaled-idx
                    (cond
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
               (multiple-value-setq (src unscaled-idx result-reg)
                 (arm2-three-untargeted-reg-forms seg
                                              vector src
                                              index unscaled-idx
                                              value (arm2-target-reg-for-aset vreg type-keyword)))))
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
                           (eq (arm2-lexical-reference-p (%car reg-args)) rest))
                (return nil))
              (flet ((independent-of-all-values (form)        
                       (setq form (acode-unwrapped-form-value form))
                       (or (arm-constant-form-p form)
                           (let* ((lexref (arm2-lexical-reference-p form)))
                             (and lexref 
                                  (neq lexref rest)
                                  (dolist (val rest-values t)
                                    (unless (arm2-var-not-set-by-form-p lexref val)
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
              (setq body (%cadr body))
              (if (and (eq op (%nx1-operator if))
                       (eq (arm2-lexical-reference-p (%cadr body)) rest))
                (setq body (%caddr body))
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
                                        (when (and arg (not (arm2-var-not-set-by-form-p lexref arg)))
                                          (return)))))
                               (and (all-simple (car arglist))
                                    (all-simple (cadr arglist))
                                    (setq fn (var-ea lexref)))))))
           (cstack *arm2-cstack*)
           (top *arm2-top-vstack-lcell*)
           (vstack *arm2-vstack*))
      (setq xfer (or xfer 0))
      (when (and (eq xfer $backend-return)
                 (eq 0 *arm2-undo-count*)
                 (acode-p fn)
                 (eq (acode-operator fn) (%nx1-operator immediate))
                 (symbolp (cadr fn)))
        (setq fn (arm2-tail-call-alias fn (%cadr fn) arglist)))
      
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
          (setq *arm2-top-vstack-lcell* top)
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
           (immp (and (consp f-op)
                      (eq (%car f-op) (%nx1-operator immediate))))
           (symp (and immp (symbolp (%cadr f-op))))
           (label-p (and (fixnump fn) 
                         (locally (declare (fixnum fn))
                           (and (= fn -1) (- fn)))))
           (tail-p (eq xfer $backend-return))
           (func (if (consp f-op) (%cadr f-op)))
           (a-reg nil)
           (lfunp (and (acode-p f-op) 
                       (eq (acode-operator f-op) (%nx1-operator simple-function))))
           (expression-p (or (typep fn 'lreg) (and (fixnump fn) (not label-p))))
           (callable (or symp lfunp label-p))
           (destreg (if symp ($ arm::fname) (if lfunp ($ arm::nfn) (unless label-p ($ arm::nfn)))))
           (alternate-tail-call
            (and tail-p label-p *arm2-tail-label* (eql nargs *arm2-tail-nargs*) (not spread-p)))
           )
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
          (arm2-copy-register seg destreg a-reg)))
      (if spread-p
        (progn
          (arm2-set-nargs seg (%i- nargs 1))
          (if (eq spread-p 0)
            (! spread-lexpr)
            (! spread-list)))
        (if nargs
          (unless alternate-tail-call (arm2-set-nargs seg nargs))
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
                  (! call-label (aref *backend-labels* 1)))
                (progn
                  (if a-reg
                    (arm2-copy-register seg destreg a-reg)
                    (arm2-store-immediate seg func destreg))
                  (if symp
                    (arm2-call-symbol seg nil)
                    (! call-known-function))))))
          (if alternate-tail-call
            (progn
              (arm2-unwind-stack seg xfer 0 0 *arm2-tail-vsp*)
              (! jump (aref *backend-labels* *arm2-tail-label*)))
            (progn
              (arm2-unwind-stack seg xfer 0 0 #x7fffff)
              (if (and (not spread-p) nargs (%i<= nargs $numarmargregs))
                (progn
                  (if label-p
                    (arm2-copy-register seg arm::nfn arm::fn))
                  (unless (or label-p a-reg) (arm2-store-immediate seg func destreg))
                  (arm2-restore-full-lisp-context seg)
                  (if label-p
                    (! jump (aref *backend-labels* 1))
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
                           (! jump-known-function)))))))))
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
            (let* ((*arm2-vstack* *arm2-vstack*)
                   (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
              (arm2-lri seg arm::arg_x (ash (nx-lookup-target-uvector-subtag :function) *arm2-target-fixnum-shift*))
              (arm2-lri seg arm::temp0 (subprim-name->offset '.SPfix-nfn-entrypoint))
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
              (! make-stack-gvector))
            (arm2-open-undo $undostkblk))
          (let* ((cell 1))
            (declare (fixnum cell))
            (progn
              (arm2-lri seg
                        arm::imm0
                        (arch::make-vheader vsize (nx-lookup-target-uvector-subtag :function)))
              (! %alloc-misc-fixed dest arm::imm0 (ash vsize (arch::target-word-shift arch)))
              )
            (! lri arm::arg_x (subprim-name->offset '.SPfix-nfn-entrypoint))
            (! misc-set-c-node arm::arg_x dest 0)
            (! %closure-code% arm::arg_x)
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
       (or (eq (%car f) (%nx1-operator immediate))
           (eq (%car f) (%nx1-operator simple-function)))))

(defun arm-constant-form-p (form)
  (setq form (nx-untyped-form form))
  (if form
    (or (nx-null form)
        (nx-t form)
        (and (consp form)
             (or (eq (acode-operator form) (%nx1-operator immediate))
                 (eq (acode-operator form) (%nx1-operator fixnum))
                 (eq (acode-operator form) (%nx1-operator simple-function)))))))


  
(defun arm2-integer-constant-p (form mode)
  (let* ((val 
         (or (acode-fixnum-form-p (setq form (acode-unwrapped-form form)))
             (and (acode-p form)
                  (eq (acode-operator form) (%nx1-operator immediate))
                  (setq form (%cadr form))
                  (if (typep form 'integer)
                    form)))))
    (and val (%typep val (mode-specifier-type mode)) val)))


(defun arm-side-effect-free-form-p (form)
  (when (consp (setq form (acode-unwrapped-form-value form)))
    (or (arm-constant-form-p form)
        ;(eq (acode-operator form) (%nx1-operator bound-special-ref))
        (if (eq (acode-operator form) (%nx1-operator lexical-reference))
          (not (%ilogbitp $vbitsetq (nx-var-bits (%cadr form))))))))

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

(defun arm2-one-untargeted-reg-form (seg form suggested)
  (with-arm-local-vinsn-macros (seg)
    (let* ((gpr-p (= (hard-regspec-class suggested) hard-reg-class-gpr))
           (node-p (if gpr-p (= (get-regspec-mode suggested) hard-reg-class-gpr-mode-node))))
      (if node-p
        (let* ((ref (arm2-lexical-reference-ea form))
               (reg (backend-ea-physical-reg ref hard-reg-class-gpr)))
          (if reg
            ref
            (if (nx-null form)
              (progn
                (! load-nil suggested)
                suggested)
              (if (and (acode-p form) 
                       (eq (acode-operator form) (%nx1-operator immediate)) 
                       (setq reg (arm2-register-constant-p (cadr form))))
                reg
                (if (and (acode-p form)
                         (eq (acode-operator form) (%nx1-operator %current-tcr)))
                  arm::rcontext
                  (arm2-one-untargeted-lreg-form seg form suggested))))))
        (arm2-one-untargeted-lreg-form seg form suggested)))))
             

(defun arm2-push-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (a-double (if a-float (= (get-regspec-mode areg) hard-reg-class-fpr-mode-double)))
         (a-node (unless a-float (= (get-regspec-mode areg) hard-reg-class-gpr-mode-node)))
         vinsn)
    (with-arm-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (arm2-vpush-register seg areg :node-temp))
        (progn
          (setq vinsn
                (if a-float
                  (if a-double
                    (! temp-push-double-float areg)
                    (! temp-push-single-float areg))
                  (! temp-push-unboxed-word areg)))
          (arm2-open-undo $undostkblk)))
      vinsn)))

(defun arm2-pop-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (a-double (if a-float (= (get-regspec-mode areg) hard-reg-class-fpr-mode-double)))
         (a-node (unless a-float (= (get-regspec-mode areg) hard-reg-class-gpr-mode-node)))
         vinsn)
    (with-arm-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (arm2-vpop-register seg areg))
        (progn
          (setq vinsn
                (if a-float
                  (if a-double
                    (! temp-pop-double-float areg)
                    (! temp-pop-single-float areg))
                  (! temp-pop-unboxed-word areg)))
          (arm2-close-undo)))
      vinsn)))

(defun arm2-acc-reg-for (reg)
  (with-arm-local-vinsn-macros (seg)
    (if (and (eql (hard-regspec-class reg) hard-reg-class-gpr)
             (eql (get-regspec-mode reg) hard-reg-class-gpr-mode-node))
      ($ arm::arg_z)
      reg)))

;;; The compiler often generates superfluous pushes & pops.  Try to
;;; eliminate them.
;;; It's easier to elide pushes and pops to the TSP.
(defun arm2-elide-pushes (seg push-vinsn pop-vinsn)
  (with-arm-local-vinsn-macros (seg)
    (let* ((pushed-reg (svref (vinsn-variable-parts push-vinsn) 0))
           (popped-reg (svref (vinsn-variable-parts pop-vinsn) 0))
           (same-reg (eq (hard-regspec-value pushed-reg)
                         (hard-regspec-value popped-reg)))
           (sp-p (vinsn-attribute-p push-vinsn :sp)))
      (when (and sp-p t)               ; vsp case is harder.
        (unless (vinsn-sequence-has-attribute-p push-vinsn pop-vinsn :tsp :discard)
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
                               (if (eql (get-regspec-mode pushed-reg)
                                        hard-reg-class-fpr-mode-single)
                                 (! single-to-single popped-reg pushed-reg)
                                 (! double-to-double popped-reg pushed-reg))
                               (! copy-gpr popped-reg pushed-reg))))
                  (remove-dll-node copy)
                  (if pushed-reg-is-set
                    (insert-dll-node-after copy push-vinsn)
                    (insert-dll-node-before copy push-vinsn))))
              (elide-vinsn push-vinsn)
              (elide-vinsn pop-vinsn))))))))
                
        
;;; we never leave the first form pushed (the 68K compiler had some subprims that
;;; would vpop the first argument out of line.)
(defun arm2-two-targeted-reg-forms (seg aform areg bform breg)
  (let* ((avar (arm2-lexical-reference-p aform))
         (atriv (and (arm2-trivial-p bform) (nx2-node-gpr-p breg)))
         (aconst (and (not atriv) (or (arm-side-effect-free-form-p aform)
                                      (if avar (arm2-var-not-set-by-form-p avar bform)))))
         (apushed))
    (progn
      (unless aconst
        (if atriv
          (arm2-one-targeted-reg-form seg aform areg)
          (setq apushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg aform (arm2-acc-reg-for areg))))))
      (arm2-one-targeted-reg-form seg bform breg)
      (if aconst
        (let* ((*available-backend-imm-temps* *available-backend-imm-temps*)
               (*available-backend-node-temps* *available-backend-node-temps*)
               (*available-backend-fp-temps* *available-backend-fp-temps*)
               (bclass (hard-regspec-class breg))
               (bregval (hard-regspec-value breg)))
          (if (eq bclass hard-reg-class-fpr)
            (use-fp-temp bregval)
            (if (eq bclass hard-reg-class-gpr)
              (if (eq (get-regspec-mode breg) hard-reg-class-gpr-mode-node)
                (use-node-temp bregval)
                (use-imm-temp bregval))))
          (arm2-one-targeted-reg-form seg aform areg))
        (if apushed
          (arm2-elide-pushes seg apushed (arm2-pop-register seg areg)))))
    (values areg breg)))


(defun arm2-two-untargeted-reg-forms (seg aform areg bform breg)
  (with-arm-local-vinsn-macros (seg)
    (let* ((avar (arm2-lexical-reference-p aform))
           (adest areg)
           (bdest breg)
           (atriv (and (arm2-trivial-p bform) (nx2-node-gpr-p breg)))
           (aconst (and (not atriv) (or (arm-side-effect-free-form-p aform)
                                        (if avar (arm2-var-not-set-by-form-p avar bform)))))
           (apushed (not (or atriv aconst))))
      (progn
        (unless aconst
          (if atriv
            (setq adest (arm2-one-untargeted-reg-form seg aform areg))
            (setq apushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg aform (arm2-acc-reg-for areg))))))
        (setq bdest (arm2-one-untargeted-reg-form seg bform breg))
        (if aconst
          (setq adest (arm2-one-untargeted-reg-form seg aform areg))
          (if apushed
            (arm2-elide-pushes seg apushed (arm2-pop-register seg areg)))))
      (values adest bdest))))


(defun arm2-four-targeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (let* ((bnode (nx2-node-gpr-p breg))
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
                                 (arm2-var-not-set-by-form-p avar bform)
                                 (arm2-var-not-set-by-form-p avar cform)
                                 (arm2-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or (arm-side-effect-free-form-p bform)
                          (let ((bvar (arm2-lexical-reference-p bform)))
                            (and bvar
                                 (arm2-var-not-set-by-form-p bvar cform)
                                 (arm2-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or (arm-side-effect-free-form-p cform)
                          (let ((cvar (arm2-lexical-reference-p cform)))
                            (and cvar
                                 (arm2-var-not-set-by-form-p cvar dform))))))
         (apushed nil)
         (bpushed nil)
         (cpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (arm2-one-targeted-reg-form seg aform areg)
        (setq apushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg aform (arm2-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (arm2-one-targeted-reg-form seg bform breg)
        (setq bpushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg bform (arm2-acc-reg-for breg))))))
    (if (and cform (not cconst))
      (if ctriv
        (arm2-one-targeted-reg-form seg cform creg)
        (setq cpushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg cform (arm2-acc-reg-for creg))))))
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
  (let* ((bnode (nx2-node-gpr-p breg))
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
                                 (arm2-var-not-set-by-form-p avar bform)
                                 (arm2-var-not-set-by-form-p avar cform))))))
         (bconst (and (not btriv)
                      (or
                       (arm-side-effect-free-form-p bform)
                       (let ((bvar (arm2-lexical-reference-p bform)))
                         (and bvar (arm2-var-not-set-by-form-p bvar cform))))))
         (apushed nil)
         (bpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (arm2-one-targeted-reg-form seg aform areg)
        (setq apushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg aform (arm2-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (arm2-one-targeted-reg-form seg bform breg)
        (setq bpushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg bform (arm2-acc-reg-for breg))))))
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
    (let* ((bnode (nx2-node-gpr-p breg))
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
                                   (arm2-var-not-set-by-form-p avar bform)
                                   (arm2-var-not-set-by-form-p avar cform))))))
           (bconst (and (not btriv)
                        (or
                         (arm-side-effect-free-form-p bform)
                         (let ((bvar (arm2-lexical-reference-p bform)))
                           (and bvar (arm2-var-not-set-by-form-p bvar cform))))))
           (adest areg)
           (bdest breg)
           (cdest creg)
           (apushed nil)
           (bpushed nil))
      (if (and aform (not aconst))
        (if atriv
          (setq adest (arm2-one-untargeted-reg-form seg aform ($ areg)))
          (setq apushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg aform (arm2-acc-reg-for areg))))))
      (if (and bform (not bconst))
        (if btriv
          (setq bdest (arm2-one-untargeted-reg-form seg bform ($ breg)))
          (setq bpushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg bform (arm2-acc-reg-for breg))))))
      (setq cdest (arm2-one-untargeted-reg-form seg cform creg))
      (unless btriv 
        (if bconst
          (setq bdest (arm2-one-untargeted-reg-form seg bform breg))
          (arm2-elide-pushes seg bpushed (arm2-pop-register seg breg))))
      (unless atriv
        (if aconst
          (setq adest (arm2-one-untargeted-reg-form seg aform areg))
          (arm2-elide-pushes seg apushed (arm2-pop-register seg areg))))
      (values adest bdest cdest))))

(defun arm2-four-untargeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (let* ((bnode (nx2-node-gpr-p breg))
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
                                 (arm2-var-not-set-by-form-p avar bform)
                                 (arm2-var-not-set-by-form-p avar cform)
                                 (arm2-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or
                       (arm-side-effect-free-form-p bform)
                       (let ((bvar (arm2-lexical-reference-p bform)))
                         (and bvar
                              (arm2-var-not-set-by-form-p bvar cform)
                              (arm2-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or
                       (arm-side-effect-free-form-p cform)
                       (let ((cvar (arm2-lexical-reference-p cform)))
                         (and cvar
                              (arm2-var-not-set-by-form-p cvar dform))))))
         (adest areg)
         (bdest breg)
         (cdest creg)
         (ddest dreg)
         (apushed nil)
         (bpushed nil)
         (cpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (setq adest (arm2-one-targeted-reg-form seg aform areg))
        (setq apushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg aform (arm2-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (setq bdest (arm2-one-untargeted-reg-form seg bform breg))
        (setq bpushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg bform (arm2-acc-reg-for breg))))))
    (if (and cform (not cconst))
      (if ctriv
        (setq cdest (arm2-one-untargeted-reg-form seg cform creg))
        (setq cpushed (arm2-push-register seg (arm2-one-untargeted-reg-form seg cform (arm2-acc-reg-for creg))))))
    (setq ddest (arm2-one-untargeted-reg-form seg dform dreg))
    (unless ctriv 
      (if cconst
        (setq cdest (arm2-one-untargeted-reg-form seg cform creg))
        (arm2-elide-pushes seg cpushed (arm2-pop-register seg creg))))
    (unless btriv 
      (if bconst
        (setq bdest (arm2-one-untargeted-reg-form seg bform breg))
        (arm2-elide-pushes seg bpushed (arm2-pop-register seg breg))))
    (unless atriv
      (if aconst
        (setq adest (arm2-one-untargeted-reg-form seg aform areg))
        (arm2-elide-pushes seg apushed (arm2-pop-register seg areg))))
    (values adest bdest cdest ddest)))

(defun arm2-lri (seg reg value)
  (with-arm-local-vinsn-macros (seg)
    (if (>= value 0)
      (! lri reg value)
      (! lri reg (logand value #xffffffff)))))


(defun arm2-multiple-value-body (seg form)
  (let* ((lab (backend-get-next-label))
         (*arm2-vstack* *arm2-vstack*)
         (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
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

(defun arm2-constant-for-compare-p (form)
  (setq form (acode-unwrapped-form form))
  (when (acode-p form)
    (let* ((op (acode-operator form)))
      (if (eql op (%nx1-operator fixnum))
        (let* ((val (ash (cadr form) arm::fixnumshift)))
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
        (values operator (acode-operand 1 form))))))

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
       (vreg dest)
       (^ cr-bit true-p)
       (progn
         (ensuring-node-target (target dest)
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
            (if (and (eq cr-bit arm::arm-cond-eq) 
                     (or jconst iconst))
              (arm2-test-reg-%izerop 
               seg 
               vreg 
               xfer 
               (arm2-one-untargeted-reg-form 
                seg 
                (if jconst i j) 
                arm::arg_z) 
               cr-bit 
               true-p 
               (or jconst iconst))
              (multiple-value-bind (ireg jreg) (arm2-two-untargeted-reg-forms seg i arm::arg_y j arm::arg_z)
                (arm2-compare-registers seg vreg xfer ireg jreg cr-bit true-p)))))))))



(defun arm2-compare-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case 
       (vreg dest)
       (progn
         (! compare dest ireg jreg)
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
       (vreg dest)
       (progn
         (! compare-to-nil dest ireg)
         (^ cr-bit true-p))
       (with-crf-target () crf
         (! compare-to-nil crf ireg)
         (ensuring-node-target (target dest)
           (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
         (^)))
      (^))))

(defun arm2-compare-double-float-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case 
       (vreg dest)
       (progn
         (! double-float-compare dest ireg jreg)
         (^ cr-bit true-p))
       (progn
         (with-crf-target () flags
           (! double-float-compare flags ireg jreg)

           (! cond->boolean dest (if true-p cr-bit (logxor cr-bit 1))))
         (^)))
      (^))))

(defun arm2-compare-single-float-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case 
       (vreg dest)
       (progn
         (! single-float-compare dest ireg jreg)
         (^ cr-bit true-p))
       (progn
         (with-crf-target () flags
           (! single-float-compare flags ireg jreg)

           (! cond->boolean dest (if true-p cr-bit (logxor cr-bit 1))))
         (^)))
      (^))))




(defun arm2-immediate-form-p (form)
  (if (and (consp form)
           (or (eq (%car form) (%nx1-operator immediate))
               (eq (%car form) (%nx1-operator simple-function))))
    t))

(defun arm2-test-%izerop (seg vreg xfer form cr-bit true-p)
  (arm2-test-reg-%izerop seg vreg xfer (arm2-one-untargeted-reg-form seg form arm::arg_z) cr-bit true-p 0))

(defun arm2-test-reg-%izerop (seg vreg xfer reg cr-bit true-p  zero)
  (declare (fixnum reg))
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (regspec-crf-gpr-case 
     (vreg dest)
     (progn
       (if (or (arm::encode-arm-immediate zero)
               (arm::encode-arm-immediate (- zero)))
         (! compare-immediate dest reg zero)
         (with-node-target (reg) other
           (arm2-lri seg other zero)
           (! compare dest reg other)))
       (^ cr-bit true-p))
     (with-crf-target () crf
       (if (or (arm::encode-arm-immediate zero)
               (arm::encode-arm-immediate (- zero)))
         (! compare-immediate crf reg (logand #xffffffff zero))
         (with-node-target (reg) other
           (arm2-lri seg other zero)
           (! compare crf reg other)))
       (ensuring-node-target (target dest)
         (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
       (^)))))

(defun arm2-lexical-reference-ea (form &optional (no-closed-p t))
  (when (acode-p (setq form (acode-unwrapped-form-value form)))
    (if (eq (acode-operator form) (%nx1-operator lexical-reference))
      (let* ((addr (var-ea (%cadr form))))
        (if (typep addr 'lreg)
          addr
          (unless (and no-closed-p (addrspec-vcell-p addr ))
            addr))))))


(defun arm2-vpush-register (seg src &optional why info attr)
  (with-arm-local-vinsn-macros (seg)
    (prog1
      (! vpush-register src)
      (arm2-regmap-note-store src *arm2-vstack*)
      (arm2-new-vstack-lcell (or why :node) *arm2-target-lcell-size* (or attr 0) info)
      (arm2-adjust-vstack *arm2-target-node-size*))))

(defun arm2-vpush-register-arg (seg src)
  (arm2-vpush-register seg src :outgoing-argument))


(defun arm2-vpop-register (seg dest)
  (with-arm-local-vinsn-macros (seg)
    (prog1
      (! vpop-register dest)
      (setq *arm2-top-vstack-lcell* (lcell-parent *arm2-top-vstack-lcell*))
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
          (if (and dest-crf src-gpr)
            ;; "Copying" a GPR to a CR field means comparing it to rnil
            (! compare-to-nil dest src)
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
                          (! get-single dest src)))))))
                (if dest-gpr
                  (case dest-mode
                    (#.hard-reg-class-gpr-mode-node
                     (case src-mode
                       (#.hard-reg-class-fpr-mode-double
                        (! double->heap dest src))
                       (#.hard-reg-class-fpr-mode-single
                        (! single->node dest src)))))
                  (if (and src-fpr dest-fpr)
                    (unless (eql dest-fpr src-fpr)
                      (case src-mode
                        (#.hard-reg-class-fpr-mode-single
                         (case dest-mode
                           (#.hard-reg-class-fpr-mode-single
                            (! single-to-single dest src))
                           (#.hard-reg-class-fpr-mode-double
                            (! single-to-double dest src))))
                        (#.hard-reg-class-fpr-mode-double
                         (case dest-mode
                           (#.hard-reg-class-fpr-mode-single
                            (! double-to-single dest src))
                           (#.hard-reg-class-fpr-mode-double
                            (! double-to-double dest src))))))))))))))))
  
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
    (with-note (form seg curstack) ; note this rebinds form/seg/curstack so can't setq
      (with-arm-local-vinsn-macros (seg)
	(let* ((op (acode-operator form)))
	  (cond ((eq op (%nx1-operator list))
		 (let* ((*arm2-vstack* *arm2-vstack*)
			(*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
		   (arm2-set-nargs seg (arm2-formlist seg (%cadr form) nil))
		   (arm2-open-undo $undostkblk curstack)
		   (! stack-cons-list))
		 (setq val arm::arg_z))
		((eq op (%nx1-operator list*))
		 (let* ((arglist (%cadr form)))                   
		   (let* ((*arm2-vstack* *arm2-vstack*)
			  (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
		     (arm2-arglist seg arglist))
		   (when (car arglist)
		     (arm2-set-nargs seg (length (%car arglist)))
		     (! stack-cons-list*)
		     (arm2-open-undo $undostkblk curstack))
		   (setq val arm::arg_z)))
		((eq op (%nx1-operator multiple-value-list))
		 (arm2-multiple-value-body seg (%cadr form))
		 (arm2-open-undo $undostkblk curstack)
		 (! stack-cons-list)
		 (setq val arm::arg_z))
		((eq op (%nx1-operator cons))
		 (let* ((y ($ arm::arg_y))
			(z ($ arm::arg_z))
			(result ($ arm::arg_z)))
		   (arm2-two-targeted-reg-forms seg (%cadr form) y (%caddr form) z)
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
		 (let* ((clear-form (caddr form))
			(cval (nx2-constant-form-value clear-form)))
		   (if cval
		       (progn 
			 (arm2-one-targeted-reg-form seg (%cadr form) ($ arm::arg_z))
			 (if (nx-null cval)
			     (! make-stack-block)
			     (! make-stack-block0)))
		       (with-crf-target () crf
			 (let ((stack-block-0-label (backend-get-next-label))
			       (done-label (backend-get-next-label))
			       (rval ($ arm::arg_z))
			       (rclear ($ arm::arg_y)))
			   (arm2-two-targeted-reg-forms seg (%cadr form) rval clear-form rclear)
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
		 (arm2-two-targeted-reg-forms seg (%cadr form) ($ arm::arg_y) (%caddr form) ($ arm::arg_z))
		 (arm2-open-undo $undostkblk curstack)
		 (! make-stack-list)
		 (setq val arm::arg_z))       
		((eq op (%nx1-operator vector))
		 (let* ((*arm2-vstack* *arm2-vstack*)
			(*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
		   (arm2-set-nargs seg (arm2-formlist seg (%cadr form) nil))
		   (! make-stack-vector))
		 (arm2-open-undo $undostkblk)
		 (setq val arm::arg_z))
		((eq op (%nx1-operator %gvector))
		 (let* ((*arm2-vstack* *arm2-vstack*)
			(*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
			(arglist (%cadr form)))
		   (arm2-set-nargs seg (arm2-formlist seg (append (car arglist) (reverse (cadr arglist))) nil))
		   (! make-stack-gvector))
		 (arm2-open-undo $undostkblk)
		 (setq val arm::arg_z)) 
		((eq op (%nx1-operator closed-function)) 
		 (setq val (arm2-make-closure seg (cadr form) t))) ; can't error
		((eq op (%nx1-operator %make-uvector))
		 (destructuring-bind (element-count subtag &optional (init 0 init-p)) (%cdr form)
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

(defun arm2-addrspec-to-reg (seg addrspec reg)
  (if (memory-spec-p addrspec)
    (arm2-stack-to-register seg addrspec reg)
    (arm2-copy-register seg reg addrspec)))
  
(defun arm2-seq-bind-var (seg var val)
  (with-arm-local-vinsn-macros (seg)
    (let* ((sym (var-name var))
           (bits (nx-var-bits var))
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
                              (if r ($ r)))))
                  (if (arm2-load-ea-p val)
                    (if reg
                      (arm2-addrspec-to-reg seg val reg)
                      (if (memory-spec-p val)
                        (with-node-temps () (temp)
                          (arm2-addrspec-to-reg seg val temp)
                          (arm2-vpush-register seg temp :node var bits))
                        (arm2-vpush-register seg val :node var bits)))
                    (if reg
                      (arm2-one-targeted-reg-form seg val reg)
                      (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg val arm::arg_z) :node var bits)))
                  (arm2-set-var-ea seg var (or reg (arm2-vloc-ea vloc closed-p)))
                  (if reg
                    (arm2-note-var-cell var reg)
                    (arm2-note-top-cell var))
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
(defun arm2-bind-var (seg var vloc &optional lcell &aux 
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
        (when lcell
          (setf (lcell-kind lcell) :node
                (lcell-attributes lcell) bits
                (lcell-info lcell) var)
          (arm2-note-var-cell var lcell))          
        (arm2-set-var-ea seg var (arm2-vloc-ea vloc closed-p))        
        closed-downward))))

(defun arm2-set-var-ea (seg var ea)
  (setf (var-ea var) ea)
  (when (and *arm2-record-symbols* (or (typep ea 'lreg) (typep ea 'fixnum)))
    (let* ((start (arm2-emit-note seg :begin-variable-scope)))
      (push (list var (var-name var) start (close-vinsn-note start))
            *arm2-recorded-symbols*)))
  ea)

(defun arm2-close-var (seg var)
  (let ((bits (nx-var-bits var)))
    (when (and *arm2-record-symbols*
               (or (logbitp $vbitspecial bits)
                   (not (logbitp $vbitpunted bits))))
      (let ((endnote (%car (%cdddr (assq var *arm2-recorded-symbols*)))))
        (unless endnote (compiler-bug "arm2-close-var for ~s ?" (var-name var)))
        (setf (vinsn-note-class endnote) :end-variable-scope)
        (append-dll-node (vinsn-note-label endnote) seg)))))

(defun arm2-load-ea-p (ea)
  (or (typep ea 'fixnum)
      (typep ea 'lreg)
      (typep ea 'lcell)))

(defun arm2-dbind (seg value sym)
  (with-arm-local-vinsn-macros (seg)
    (let* ((ea-p (arm2-load-ea-p value))
           (nil-p (unless ea-p (nx-null (setq value (nx-untyped-form value)))))
           (self-p (unless ea-p (and (or
                                      (eq (acode-operator value) (%nx1-operator bound-special-ref))
                                      (eq (acode-operator value) (%nx1-operator special-ref)))
                                     (eq (cadr value) sym)))))
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
      (arm2-new-vstack-lcell :special-value *arm2-target-lcell-size* 0 sym)
      (arm2-new-vstack-lcell :special *arm2-target-lcell-size* (ash 1 $vbitspecial) sym)
      (arm2-new-vstack-lcell :special-link *arm2-target-lcell-size* 0 sym)
      (arm2-adjust-vstack (* 3 *arm2-target-node-size*)))))

;;; Store the contents of EA - which denotes either a vframe location
;;; or a hard register - in reg.

(defun arm2-store-ea (seg ea reg)
  (if (typep ea 'fixnum)
    (if (memory-spec-p ea)
      (arm2-stack-to-register seg ea reg)
      (arm2-copy-register seg reg ea))
    (if (typep ea 'lreg)
      (arm2-copy-register seg reg ea)
      (if (typep ea 'lcell)
        (arm2-lcell-to-register seg ea reg)))))


      

;;; Callers should really be sure that this is what they want to use.
(defun arm2-absolute-natural (seg vreg xfer value)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (arm2-lri seg vreg value))
    (^)))



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
              (progn
                (! temp-push-unboxed-word ptr-reg)
                (arm2-open-undo $undostkblk)
                (multiple-value-bind (address node) (address-and-node-regs)
                  (with-imm-target (address) (ptr-reg :address)
                    (! temp-pop-unboxed-word ptr-reg)
                    (arm2-close-undo)
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
                (progn
                  (with-imm-target () (ptr-reg :address)
                    (arm2-one-targeted-reg-form seg ptr ptr-reg)
                    (! temp-push-unboxed-word ptr-reg)
                    (arm2-open-undo $undostkblk))
                  (with-imm-target () (off-reg :signed-natural)
                    (! fixnum->signed-natural off-reg (arm2-one-targeted-reg-form seg offset ($ arm::arg_z)))
                    (with-imm-target (off-reg) (val-reg :signed-natural)
                      (arm2-lri seg val-reg intval)
                      (with-imm-target (off-reg val-reg) (ptr-reg :address)
                        (! temp-pop-unboxed-word ptr-reg)
                        (arm2-close-undo)
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg val-reg))))))
              ;; No intval; maybe constant-offset.
              (with-imm-target () (ptr-reg :address)
                (arm2-one-targeted-reg-form seg ptr ptr-reg)
                (! temp-push-unboxed-word ptr-reg)
                (arm2-open-undo $undostkblk)
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
                        (! temp-pop-unboxed-word ptr-reg)
                        (arm2-close-undo)
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
  
(defun arm2-memory-store-displaced (seg valreg basereg displacement size)
  (with-arm-local-vinsn-macros (seg)
    (case size
      (8 (! mem-set-c-doubleword valreg basereg displacement))
      (4 (! mem-set-c-fullword valreg basereg displacement))
      (2 (! mem-set-c-halfword valreg basereg displacement))
      (1 (! mem-set-c-byte valreg basereg displacement)))))

(defun arm2-memory-store-indexed (seg valreg basereg idxreg size)
  (with-arm-local-vinsn-macros (seg)
    (case size
      (8 (! mem-set-doubleword valreg basereg idxreg))
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
                                        ; Easier: need one less register than in the general case.
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
                (progn
                  (! temp-push-unboxed-word ptr-reg)
                  (arm2-open-undo $undostkblk)
                  (val-to-argz-and-imm0)                  
                  (with-imm-target (arm::imm0) (ptr-reg :address)
                    (! temp-pop-unboxed-word ptr-reg)
                    (arm2-close-undo)
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
                  (progn
                    (with-imm-target () (ptr-reg :address)
                      (arm2-one-targeted-reg-form seg ptr ptr-reg)
                      (! temp-push-unboxed-word ptr-reg)
                      (arm2-open-undo $undostkblk))
                    (with-imm-target () (off-reg :s32)
                      (! fixnum->signed-natural off-reg (arm2-one-targeted-reg-form seg offset ($ arm::arg_z)))
                      (with-imm-target (off-reg) (val-reg :s32)
                        (arm2-lri seg val-reg intval)
                        (with-imm-target (off-reg val-reg) (ptr-reg :address)
                          (! temp-pop-unboxed-word ptr-reg)
                          (arm2-close-undo)
                          (setq xptr-reg ptr-reg
                                xoff-reg off-reg
                                xval-reg val-reg))))))
                ;; No intval; maybe constant-offset.
                (with-imm-target () (ptr-reg :address)
                  (arm2-one-targeted-reg-form seg ptr ptr-reg)
                  (! temp-push-unboxed-word ptr-reg)
                  (arm2-open-undo $undostkblk)
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
                        (! temp-pop-unboxed-word ptr-reg)
                        (arm2-close-undo)
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg arm::imm0
                              node-arg_z t))))))
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

(defun arm2-encoding-vstack-top (encoding)
  (svref encoding 3))

(defun arm2-encode-stack ()
  (vector *arm2-undo-count* *arm2-cstack* *arm2-vstack* *arm2-top-vstack-lcell*))

(defun arm2-decode-stack (encoding)
  (values (arm2-encoding-undo-count encoding)
          (arm2-encoding-cstack-depth encoding)
          (arm2-encoding-vstack-depth encoding)
          (arm2-encoding-vstack-top encoding)))

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
  (multiple-value-setq (*arm2-undo-count* *arm2-cstack* *arm2-vstack* *arm2-top-vstack-lcell*)
    (arm2-decode-stack i))
  (set-fill-pointer 
   *arm2-undo-stack*
   (set-fill-pointer *arm2-undo-because* new-count)))





;;; "Trivial" means can be evaluated without allocating or modifying registers.
;;; Interim definition, which will probably stay here forever.
(defun arm2-trivial-p (form &aux op bits)
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

(defun arm2-lexical-reference-p (form)
  (when (acode-p form)
    (let ((op (acode-operator (setq form (acode-unwrapped-form-value form)))))
      (when (or (eq op (%nx1-operator lexical-reference))
                (eq op (%nx1-operator inherited-arg)))
        (%cadr form)))))



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
    (setq safe nil))                    ; May also have been passed as NIL.
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
             (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
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
        (! call-subprim-2 ($ arm::arg_z) (subprim-name->offset '.SPrplacd) ptr-vreg val-vreg)
        (! call-subprim-2 ($ arm::arg_z) (subprim-name->offset '.SPrplaca) ptr-vreg val-vreg))
      (if returnptr
        (<- ptr-vreg)
        (<- val-vreg))
      (^))))



(defun arm2-find-nilret-label ()
  (dolist (l *arm2-nilret-labels*)
    (destructuring-bind (label vsp csp register-restore-count register-restore-ea &rest agenda) l
      (and (or (and (eql 0 register-restore-count)
                    (or (not (eql 0 vsp))
                        (eq vsp *arm2-vstack*)))
                (and 
                 (eq register-restore-count *arm2-register-restore-count*)
                 (eq vsp *arm2-vstack*)))
           (or agenda (eq csp *arm2-cstack*))
           (eq register-restore-ea *arm2-register-restore-ea*)
           (eq (%ilsr 1 (length agenda)) *arm2-undo-count*)
           (dotimes (i (the fixnum *arm2-undo-count*) t) 
             (unless (and (eq (pop agenda) (aref *arm2-undo-because* i))
                          (eq (pop agenda) (aref *arm2-undo-stack* i)))
               (return)))
           (return label)))))

(defun arm2-record-nilret-label ()
  (let* ((lab (backend-get-next-label))
         (info nil))
    (dotimes (i (the fixnum *arm2-undo-count*))
      (push (aref *arm2-undo-because* i) info)
      (push (aref *arm2-undo-stack* i) info))
    (push (cons
                 lab 
                 (cons
                  *arm2-vstack*
                  (cons 
                   *arm2-cstack*
                   (cons
                    *arm2-register-restore-count*
                    (cons
                     *arm2-register-restore-ea*
                     (nreverse info))))))
          *arm2-nilret-labels*)
    lab))

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
  (let* ((*arm2-vstack* *arm2-vstack*)
         (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
    (with-arm-local-vinsn-macros (seg)
      (setq xfer (or xfer 0))
      (when (logbitp $backend-mvpass-bit xfer) ;(arm2-mvpass-p cd)
        (setq xfer (logand (lognot $backend-mvpass-mask) xfer))
        (unless *arm2-returning-values*
          (arm2-vpush-register seg arm::arg_z)
          (arm2-set-nargs seg 1)))
      (if (neq 0 xfer)
        (if (eq xfer $backend-return)    ;; xfer : RETURN ==> popj
          (arm2-do-return seg)
          (if (not (arm2-cd-compound-p xfer))
            (-> xfer)  ;; xfer : label# ==> BRA label#
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
                  (! cbranch-true tlabel crf cr-bit )    ;; (label# /  label#)
                  (-> nbranch)))
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
                    (@ aux-label))))))))))

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
      (if (eq current-stack old-stack)
        (arm2-form seg vreg xfer body)
        (if (eq xfer $backend-return)
          (progn
            (arm2-form seg vreg xfer body)
            (dotimes (i numundo) (arm2-close-undo)))
          (if (arm2-mvpass-p xfer)
            (progn
              (arm2-mvpass seg body) ; presumed to be ok
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
  (multiple-value-bind (target-catch target-cstack target-vstack target-vstack-lcell)
                       (arm2-decode-stack encoding)
    (arm2-unwind-stack seg xfer target-catch target-cstack target-vstack)
    (arm2-regmap-note-vstack-delta target-vstack *arm2-vstack*)
    (setq *arm2-undo-count* target-catch 
          *arm2-cstack* target-cstack
          *arm2-vstack* target-vstack
          *arm2-top-vstack-lcell* target-vstack-lcell)))

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
  (let* ((*arm2-vstack* *arm2-vstack*)
         (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
         (label nil)
         (vstack nil)
         (foldp (not *arm2-open-code-inline*)))
    (with-arm-local-vinsn-macros (seg)
      (progn
        (setq vstack (arm2-set-vstack (arm2-unwind-stack seg $backend-return 0 0 #x7fffff)))
        (if *arm2-returning-values*
          (cond ((and foldp (setq label (%cdr (assq vstack *arm2-valret-labels*))))
                 (-> label))
                (t
                 (@ (setq label (backend-get-next-label)))
                 (push (cons vstack label) *arm2-valret-labels*)

                 (! nvalret)))
          (! popj))))
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

(defun arm2-close-structured-var (seg var)
  (if (arm2-structured-var-p var)
    (apply #'arm2-close-structured-lambda seg (cdr var))
    (arm2-close-var seg var)))

(defun arm2-close-structured-lambda (seg whole req opt rest keys auxen)
  (if whole
    (arm2-close-var seg whole))
  (dolist (var req)
    (arm2-close-structured-var seg var))
  (dolist (var (%car opt))
    (arm2-close-structured-var seg var))
  (dolist (var (%caddr opt))
    (when var
      (arm2-close-var seg var)))
  (if rest
    (arm2-close-structured-var seg rest))
  (dolist (var (%cadr keys))
    (arm2-close-structured-var seg var))
  (dolist (var (%caddr keys))
    (if var (arm2-close-var seg var)))
  (dolist (var (%car auxen))
    (arm2-close-var seg var)))


(defun arm2-init-regvar (seg var reg addr)
  (with-arm-local-vinsn-macros (seg)
    (arm2-stack-to-register seg addr reg)
    (arm2-set-var-ea seg var ($ reg))))

(defun arm2-bind-structured-var (seg var vloc lcell &optional context)
  (declare (ignore context))
  (if (not (arm2-structured-var-p var))
    (let* ((reg (nx2-assign-register-var var)))
      (if reg
        (arm2-init-regvar seg var reg (arm2-vloc-ea vloc))
        (arm2-bind-var seg var vloc lcell)))
    (compiler-bug "Old destructuring code ...")))

(defun arm2-bind-structured-lambda (seg lcells vloc context whole req opt rest keys auxen
                        &aux (nkeys (list-length (%cadr keys))))
  (declare (fixnum vloc))
  (when whole
    (arm2-bind-structured-var seg whole vloc (pop lcells))
    (incf vloc *arm2-target-node-size*))
  (dolist (arg req)
    (arm2-bind-structured-var seg arg vloc (pop lcells) context)
    (incf vloc *arm2-target-node-size*))
  (when opt
   (if (arm2-hard-opt-p opt)
     (setq vloc (apply #'arm2-structured-initopt seg lcells vloc context opt)
           lcells (nthcdr (ash (length (car opt)) 1) lcells))
     (dolist (var (%car opt))
       (arm2-bind-structured-var seg var vloc (pop lcells) context)
       (incf vloc *arm2-target-node-size*))))
  (when rest
    (arm2-bind-structured-var seg rest vloc (pop lcells) context)
    (incf vloc *arm2-target-node-size*))
  (when keys
    (apply #'arm2-structured-init-keys seg lcells vloc context keys)
    (setq vloc (%i+ vloc (* *arm2-target-node-size* (+ nkeys nkeys)))))
  (arm2-seq-bind seg (%car auxen) (%cadr auxen)))

(defun arm2-structured-var-p (var)
  (and (consp var) (or (eq (%car var) *nx-lambdalist*)
                       (eq (%car var) (%nx1-operator lambda-list)))))

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

(defun arm2-spread-lambda-list (seg listform whole req opt rest keys)
  (with-arm-local-vinsn-macros (seg)
    (let* ((numopt (length (%car opt)))
           (nkeys (length (%cadr keys)))
           (numreq (length req))
           (vtotal numreq)
           (old-top *arm2-top-vstack-lcell*)
           (listreg ($ arm::arg_z))
           (doadlword (dpb nkeys (byte 8 16) (dpb numopt (byte 8 8) (dpb numreq (byte 8 0) 0 )))))
      (declare (fixnum numopt nkeys numreq vtotal doadlword))
      (when (or (> numreq 255) (> numopt 255) (> nkeys 255))
        (compiler-bug "A lambda list can contain a maximum of 255 required, 255 optional, and 255 keywords args"))
      (if (fixnump listform)
        (arm2-store-ea seg listform listreg)
        (arm2-one-targeted-reg-form seg listform listreg))
      (when whole
        (arm2-vpush-register seg listreg :reserved))
      (when keys
        (setq doadlword (%ilogior2 (ash 1 25) doadlword))
        (incf  vtotal (%ilsl 1 nkeys))
        (if (%car keys)                 ; &allow-other-keys
          (setq doadlword (%ilogior doadlword (ash 1 26))))
        (arm2-store-immediate seg (%car (%cdr (%cdr (%cdr (%cdr keys))))) arm::temp2))
      (when opt
        (setq vtotal (%i+ vtotal numopt))
        (when (arm2-hard-opt-p opt)
          (setq doadlword (logior doadlword (ash 1 29)))
          (setq vtotal (%i+ vtotal numopt))))
      (when rest
        (setq doadlword (%ilogior2 (ash 1 27) doadlword) vtotal (%i+ vtotal 1)))
      (arm2-reserve-vstack-lcells vtotal)
      (! load-adl doadlword)
      (! debind)
      (arm2-set-vstack (%i+ *arm2-vstack* (* *arm2-target-node-size* vtotal)))
      (arm2-collect-lcells :reserved old-top))))


(defun arm2-tailcallok (xfer)
  (and (eq xfer $backend-return)
       *arm2-tail-allow*
       (eq 0 *arm2-undo-count*)))

(defun arm2-mv-p (cd)
  (or (eq cd $backend-return) (arm2-mvpass-p cd)))

(defun arm2-expand-note (header note)
  (let* ((lab (vinsn-note-label note)))
    (case (vinsn-note-class note)
      ((:begin-variable-scope :end-variable-scope
        :source-location-begin :source-location-end)
       (setf (vinsn-label-info lab) (arm::emit-lap-label header lab))))))


(defun arm2-expand-vinsns (header seg)
  (do-dll-nodes (v header)
    (if (%vinsn-label-p v)
      (let* ((id (vinsn-label-id v)))
        (if (or (typep id 'fixnum) (null id))
          (when (or t (vinsn-label-refs v) (null id))
            (setf (vinsn-label-info v) (arm::emit-lap-label seg v)))
          (arm2-expand-note seg id)))
      (arm2-expand-vinsn v seg)))
  ;;; This doesn't have too much to do with anything else that's
  ;;; going on here, but it needs to happen before the lregs
  ;;; are freed.  There really shouldn't be such a thing as a
  ;;; var-ea, of course ...
  (dolist (s *arm2-recorded-symbols*)
    (let* ((var (car s))
           (ea (var-ea var)))
      (when (typep ea 'lreg)
        (setf (var-ea var) (lreg-value ea)))))
  (free-logical-registers)
  (arm2-free-lcells))

;;; It's not clear whether or not predicates, etc. want to look
;;; at an lreg or just at its value slot.
;;; It's clear that the assembler just wants the value, and that
;;; the value had better be assigned by the time we start generating
;;; machine code.
;;; For now, we replace lregs in the operand vector with their values
;;; on entry, but it might be reasonable to make PARSE-OPERAND-FORM
;;; deal with lregs ...
(defun arm2-expand-vinsn (vinsn seg)
  (let* ((template (vinsn-template vinsn))
         (vp (vinsn-variable-parts vinsn))
         (nvp (vinsn-template-nvp template))
         (predicate (vinsn-annotation vinsn))
         (unique-labels ())
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
                                        ;(break "valform = ~s" valform)
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
               (let* ((insn (arm::make-lap-instruction nil))
                      (opcode (car f))
                      (operands (cdr f)))
                 (setf (arm::lap-instruction-opcode-high insn) (car opcode)
                       (arm::lap-instruction-opcode-low insn) (cdr opcode))
                 (when predicate
                   (funcall (svref operand-insert-functions
                                   (arm::encode-vinsn-field-type :cond))
                            insn
                            predicate))
                 (dolist (op operands (arm::emit-lap-instruction-element insn seg))
                   (let* ((insert-function (svref operand-insert-functions (car op))))
                     (funcall insert-function insn (parse-operand-form (cdr op)))))))
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
                 (arm::emit-lap-label seg (assq f unique-labels))
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
      (dolist (form (vinsn-template-body template))
        (expand-form form ))
      (setf (vinsn-variable-parts vinsn) nil)
      (when vp
        (free-varparts-vector vp)))))





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
                      (or (arm::arm-subprimitive-address name)
                          (compiler-bug "Unknown builtin subprim index for ~s" name))))
           (tail-p (arm2-tailcallok xfer)))
      (when tail-p
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
           (reserved-lcells nil)
           (*arm2-vstack* 0))
      (declare (type (unsigned-byte 16) num-req num-opt num-inh))
      (with-arm-p2-declarations p2decls
        ;; Need to do this for effect here.
        (nx2-allocate-global-registers *arm2-fcells* *arm2-vcells* nil nil nil)
        (@ (backend-get-next-label)) ; generic self-reference label, should be label #1
        (when keys ;; Ensure keyvect is the first immediate
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
            (setq arg-regs (arm2-req-nargs-entry seg rev-fixed))
            (if (and (not (or hardopt rest keys))
                     (<= num-opt $numarmargregs))
              (setq arg-regs (arm2-simple-opt-entry seg rev-opt rev-fixed))
              (progn
                ; If the minumum acceptable number of args is non-zero, ensure
                ; that at least that many were received.  If there's an upper bound,
                ; enforce it.
                
                (when rev-fixed
                  (arm2-reserve-vstack-lcells num-fixed)
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
                  (arm2-reserve-vstack-lcells num-opt)
                  (! default-optionals (+ num-fixed num-opt)))
                (when keys
                  (unless opt
                    (! vpush-argregs num-fixed))
                  (let* ((keyvect (%car (%cdr (%cdr (%cdr (%cdr keys))))))
                         (flags (the fixnum (logior (the fixnum (if rest 4 0)) 
                                                    (the fixnum (if (or methodp allow-other-keys-p) 1 0)))))
                         (nkeys (length keyvect))
                         (nprev (+ num-fixed num-opt)))
                    (declare (fixnum flags nkeys nprev))
                    (dotimes (i (the fixnum (+ nkeys nkeys)))
                      (arm2-new-vstack-lcell :reserved *arm2-target-lcell-size* 0 nil))
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
                              (! heap-cons-rest-arg)))))
                      ; Make an lcell for the &rest arg
                      (arm2-reserve-vstack-lcells 1))))
                (when hardopt
                  (arm2-reserve-vstack-lcells num-opt)
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

          (when (and nil
                     (not (or opt rest keys))
                     (<= num-fixed $numarmargregs)
                     (not (some #'null arg-regs)))
            (setq *arm2-tail-vsp* *arm2-vstack*
                  *arm2-tail-nargs* num-fixed)
            (@ (setq *arm2-tail-label* (backend-get-next-label))))
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
                  (arg-reg-num arm::arg_z (1- arg-reg-num)))
                 ((null vars))
              (declare (list vars) (fixnum arg-reg-num))
              (let* ((var (car vars)))
                (when var
                  (let* ((reg (nx2-assign-register-var var)))
                    (arm2-copy-register seg reg arg-reg-num)
                    (setf (var-ea var) reg))))))
          (setq *arm2-entry-vsp-saved-p* t)
#|
          (when stack-consed-rest
            (if rest-ignored-p
              (if nil (arm2-jsrA5 $sp-popnlisparea))
              (progn
                (arm2-open-undo $undostkblk))))
|#
          (when stack-consed-rest
            (arm2-open-undo $undostkblk))
          (setq *arm2-entry-vstack* *arm2-vstack*)
          (setq reserved-lcells (arm2-collect-lcells :reserved))
          (arm2-bind-lambda seg reserved-lcells req opt rest keys auxen optsupvloc arg-regs lexprp inherited-vars))
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
      (let* ((float-p (= (hard-regspec-class vreg) hard-reg-class-fpr))
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


(defarm2 arm2-%primitive %primitive (seg vreg xfer &rest ignore)
  (declare (ignore seg vreg xfer ignore))
  (compiler-bug "You're probably losing big: using %primitive ..."))

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
          (let* ((*arm2-vstack* *arm2-vstack*)
                 (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
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
      (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer (logior fix1 fix2)))
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
      (^))))


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
      (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer (logxor fix1 fix2)))
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
          (if vreg (ensuring-node-target (target vreg) (! %logxor2 vreg r1 r2)))))
      (^))))

(defarm2 arm2-%ineg %ineg (seg vreg xfer n)
  (let* ((src (arm2-one-untargeted-reg-form seg n arm::arg_z)))
    (when vreg
      (ensuring-node-target (target vreg)
        (if *arm2-open-code-inline*
          (! negate-fixnum-overflow-inline target src)
          (progn
            (! negate-fixnum-overflow-ool ($ arm::arg_z) src)
            (arm2-copy-register seg target ($ arm::arg_z))))))
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
  (arm2-vref seg vreg xfer :struct struct offset (unless *arm2-reckless* (nx-lookup-target-uvector-subtag :struct))))

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
  (let* ((name (ecase (cadr cc)
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
               (done (backend-get-next-label)))
          (if otherform
            (unless (acode-fixnum-form-p otherform)
              (! branch-unless-arg-fixnum ($ arm::arg_z) (aref *backend-labels* out-of-line)))
            (if (acode-fixnum-form-p form1)
              (! branch-unless-arg-fixnum ($ arm::arg_z) (aref *backend-labels* out-of-line))
              (if (acode-fixnum-form-p form2)
                (! branch-unless-arg-fixnum ($ arm::arg_y) (aref *backend-labels* out-of-line))  
                (! branch-unless-both-args-fixnums ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line)))))
          (with-crf-target () crf
          (if otherform
            (! compare-immediate crf ($ arm::arg_z) 0)
            (! compare crf ($ arm::arg_y) ($ arm::arg_z)))
          (! cond->boolean ($ arm::arg_z) (if true-p cr-bit (logxor cr-bit 1))))
          (-> done)
          (@ out-of-line)
          (if otherform
            (arm2-lri seg ($ arm::arg_y) 0))
          (let* ((index (arch::builtin-function-name-offset name))
                 (idx-subprim (arm2-builtin-index-subprim index)))
            (! call-subprim-2 ($ arm::arg_z) idx-subprim ($ arm::arg_y) ($ arm::arg_z)))
          (@ done)
          (<- ($ arm::arg_z))
          (^))))))
    
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
      (let* ((cell (arm2-lookup-var-cell varnode)))
        (if (and cell (typep cell 'lcell))
          (if (arm2-ensure-lcell-offset cell (logand ea-or-form #xffff))
            (and nil (format t "~& could use cell ~s for var ~s" cell (var-name varnode)))
            (if (logbitp arm2-debug-verbose-bit *arm2-debug-mask*)
              (compiler-bug "wrong ea for lcell for var ~s: got ~d, expected ~d" 
                     (var-name varnode) (calc-lcell-offset cell) (logand ea-or-form #xffff))))
          (if (not cell)
            (when (memory-spec-p ea-or-form)
              (if (logbitp arm2-debug-verbose-bit *arm2-debug-mask*)
                (format t "~& no lcell for ~s." (var-name varnode))))))
        
        (unless (or (typep ea-or-form 'lreg) (fixnump ea-or-form))
          (compiler-bug "bogus ref to var ~s (~s) : ~s " varnode (var-name varnode) ea-or-form))
        (arm2-do-lexical-reference seg vreg ea-or-form)
        (^)))))

(defarm2 arm2-setq-lexical setq-lexical (seg vreg xfer varspec form)
  (let* ((ea (var-ea varspec)))
    ;(unless (fixnump ea) (compiler-bug "setq lexical is losing BIG"))
    (let* ((valreg (arm2-one-untargeted-reg-form seg form (if (and (register-spec-p ea) 
                                                                   (or (null vreg) (eq ea vreg)))
                                                            ea
                                                            arm::arg_z))))
      (arm2-do-lexical-setq seg vreg ea valreg))
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
              (arm2-absolute-natural seg target nil (ash value *arm2-target-fixnum-shift*)))
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
             (vreg dest)
             (progn
               (! %ilogbitp-constant-bit dest reg arm-bit)
               (^ cr-bit true-p))
             (with-crf-target () crf
                (! %ilogbitp-constant-bit crf reg arm-bit)
                (ensuring-node-target (target vreg)
                  (! cond->boolean target (if true-p cr-bit (logxor cr-bit 1))))
                (^))))
          (multiple-value-bind (rbit rform) (arm2-two-untargeted-reg-forms seg bitnum arm::arg_y form arm::arg_z)
            (regspec-crf-gpr-case 
               (vreg dest)
               (progn
                 (! %ilogbitp-variable-bit dest rform rbit)
                 (^ cr-bit true-p))
               (with-crf-target () crf
                 (! %ilogbitp-variable-bit crf rform rbit)
                 (ensuring-node-target (target dest)
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
        (let ((tag (cddr form)))
          (@ (car tag)))
        (arm2-form seg nil nil form)))
    (arm2-nil seg vreg xfer)))

(defarm2 arm2-call call (seg vreg xfer fn arglist &optional spread-p)
  (when (and (null vreg)
             (acode-p fn)
             (eq (acode-operator fn) (%nx1-operator immediate)))
    (let* ((name (cadr fn)))
      (when (memq name *warn-if-function-result-ignored*)
        (p2-whine *arm2-cur-afunc*  :result-ignored name))))
  (arm2-call-fn seg vreg xfer fn arglist spread-p))

(defarm2 arm2-self-call self-call (seg vreg xfer arglist &optional spread-p)
  (setq arglist (arm2-augment-arglist *arm2-cur-afunc* arglist (if spread-p 1 $numarmargregs)))
  (arm2-call-fn seg vreg xfer -1 arglist spread-p))


(defarm2 arm2-lexical-function-call lexical-function-call (seg vreg xfer afunc arglist &optional spread-p)
  (arm2-call-fn seg vreg xfer (list (%nx1-operator simple-function) afunc)
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
                (0 (subprim-name->offset '.SPcallbuiltin0))
                (1 (subprim-name->offset '.SPcallbuiltin1))
                (2 (subprim-name->offset '.SPcallbuiltin2))
                (3 (subprim-name->offset '.SPcallbuiltin3))
                (t (subprim-name->offset '.SPcallbuiltin))))))
    (when tail-p
      (arm2-restore-full-lisp-context seg))
    #+nil
    (unless idx-subprim
      (! lri arm::imm0 (ash idx *arm2-target-fixnum-shift*))
      (when (eql subprim (subprim-name->offset '.SPcallbuiltin))
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
  (let* ((lab (aref *backend-labels* labelnum))
         (refs (vinsn-label-refs lab))
         (branch (car refs)))
    (if (and (vinsn-attribute-p branch :branch)
             (null (cdr refs)))
      (when (do* ((next (dll-node-succ branch) (dll-node-succ next))
                  (vinsn-p nil))
                 ((eq next lab) (return vinsn-p))
              (if (typep next 'vinsn-label)
                (unless (typep (vinsn-label-id next) 'vinsn-note)
                  (return))
                (progn
                  (unless (and (typep next 'vinsn)
                               (null (vinsn-annotation next))
                               (vinsn-attribute-p next :predicatable)
                               (or (eq lab (dll-node-succ next))
                                   (not (vinsn-attribute-p next :jump :call :subprim-call :jumpLR))))
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
                    (remove-dll-node branch)
                    (remove-dll-node lab)
                    t)
                (cond ((typep next 'vinsn-label))
                      ((vinsn-attribute-p next :jump)
                       (setf (vinsn-template next)
                             (need-vinsn-template 'cbranch-true
                                                  (backend-p2-vinsn-templates
                                                   *target-backend*))
                             (svref (vinsn-variable-parts next) cond-operand-index)
                             condition))
                      (t (setf (vinsn-annotation next) condition)))))))))))


(defarm2 arm2-if if (seg vreg xfer testform true false &aux test-val)
  (if (setq test-val (nx2-constant-form-value (acode-unwrapped-form-value testform)))
    (arm2-form seg vreg xfer (if (nx-null test-val) false true))
    (let* ((cstack *arm2-cstack*)
           (vstack *arm2-vstack*)
           (top-lcell *arm2-top-vstack-lcell*)
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
           (saved-reg-map (make-array 16 :initial-element nil)))
      (declare (dynamic-extent saved-reg-map))
      (if (eq 0 xfer) 
        (setq xfer nil))
      (if both-single-valued            ; it's implied that we're returning
        (let* ((result arm::arg_z))
          (let ((merge-else-branch-label (if (nx-null false) (arm2-find-nilret-label))))
            (arm2-conditional-form seg (arm2-make-compound-cd 0 falselabel) testform)
            (arm2-copy-regmap (setq saved-reg-mask *arm2-gpr-locations-valid-mask*)
                              *arm2-gpr-locations*
                              saved-reg-map)
            (arm2-form seg result endlabel true)
            (if (and merge-else-branch-label (neq -1 (aref *backend-labels* merge-else-branch-label)))
              (backend-copy-label merge-else-branch-label falselabel)
              (progn
                (@ falselabel)
                (arm2-predicate-block falselabel)
                (if (nx-null false) (@ (arm2-record-nilret-label)))
                (let* ((*arm2-gpr-locations-valid-mask* saved-reg-mask)
                       (*arm2-gpr-locations* saved-reg-map))
                  (arm2-form seg result nil false))))
            (@ endlabel)
            (arm2-predicate-block endlabel)
            (<- result)
            (^)))
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
          (setq *arm2-top-vstack-lcell* top-lcell)
          (if false-is-goto (arm2-unreachable-store))
          (let ((merge-else-branch-label (if (and (nx-null false) (eq xfer $backend-return)) (arm2-find-nilret-label))))
            (if (and merge-else-branch-label (neq -1 (aref *backend-labels* merge-else-branch-label)))
              (backend-copy-label merge-else-branch-label falselabel)
              (progn
                (@ falselabel)
                (arm2-predicate-block falselabel)
                (when need-else
                  (if true-cleanup-label
                    (arm2-mvpass seg false)
                    (let* ((*arm2-gpr-locations-valid-mask* saved-reg-mask)
                           (*arm2-gpr-locations* saved-reg-map)) 
                      (arm2-form seg vreg xfer false)))
                  (setq false-stack (arm2-encode-stack))))))
          (when true-cleanup-label
            (if (setq same-stack-effects (arm2-equal-encodings-p true-stack false-stack)) ; can share cleanup code
              (@ true-cleanup-label))
            (let* ((*arm2-returning-values* :pass))
              (arm2-nlexit seg xfer 1)
              (arm2-branch seg (if (and xfer (neq xfer $backend-mvpass-mask)) xfer (if (not same-stack-effects) endlabel)) vreg))
            (unless same-stack-effects
              (@ true-cleanup-label)
              (multiple-value-setq (true *arm2-cstack* *arm2-vstack* *arm2-top-vstack-lcell*)
                (arm2-decode-stack true-stack))
              (let* ((*arm2-returning-values* :pass))
                (arm2-nlexit seg xfer 1)
                (^)))
            (arm2-close-undo)
            (multiple-value-setq (*arm2-undo-count* *arm2-cstack* *arm2-vstack* *arm2-top-vstack-lcell*) 
              (arm2-decode-stack entry-stack)))
          (@ endlabel)
          (arm2-predicate-block endlabel))))))

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
           (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
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
           (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
           (nargs (arm2-arglist seg arglist)))
      (declare (fixnum nargs))
      (when (> nargs 1)
        (arm2-set-nargs seg (1- nargs))
        (! list*))
      (<- arm::arg_z)))
  (^))

(defarm2 arm2-minus1 minus1 (seg vreg xfer form)
  (arm2-unary-builtin seg vreg xfer '%negate form))

(defun arm2-inline-add2 (seg vreg xfer form1 form2)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (arm2-two-targeted-reg-forms seg form1 ($ arm::arg_y) form2 ($ arm::arg_z))
    (let* ((out-of-line (backend-get-next-label))
           (done (backend-get-next-label)))
      (ensuring-node-target (target vreg)
        (if (acode-fixnum-form-p form1)
          (! branch-unless-arg-fixnum ($ arm::arg_z) (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (! branch-unless-arg-fixnum ($ arm::arg_y) (aref *backend-labels* out-of-line))  
            (! branch-unless-both-args-fixnums ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line))))
        (if *arm2-open-code-inline*
          (! fixnum-add-overflow-inline-skip ($ arm::arg_z) ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* done))
          (progn
            (! fixnum-add-overflow-ool ($ arm::arg_z) ($ arm::arg_y) ($ arm::arg_z))
            (-> done)))
        (@ out-of-line)
        (! call-subprim-2 ($ arm::arg_z) (subprim-name->offset '.SPbuiltin-plus) ($ arm::arg_y) ($ arm::arg_z))
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
          (! branch-unless-arg-fixnum ($ arm::arg_z) (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (! branch-unless-arg-fixnum ($ arm::arg_y) (aref *backend-labels* out-of-line))  
            (! branch-unless-both-args-fixnums ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line))))
        (if *arm2-open-code-inline*
          (! fixnum-sub-overflow-inline-skip ($ arm::arg_z) ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* done))
          (progn
            (! fixnum-sub-overflow-ool ($ arm::arg_z)($ arm::arg_y) ($ arm::arg_z))
            (-> done)))
        (@ out-of-line)
        (! call-subprim-2 ($ arm::arg_z) (subprim-name->offset '.SPbuiltin-minus) ($ arm::arg_y) ($ arm::arg_z))
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
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *arm2-trust-declarations*)
    (if (and (arm2-form-typep form1 'double-float)
             (arm2-form-typep form2 'double-float))
      (arm2-use-operator (%nx1-operator %double-float+-2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (arm2-form-typep form1 'single-float)
               (arm2-form-typep form2 'single-float))
        (arm2-use-operator (%nx1-operator %short-float+-2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
        (if (and (arm2-form-typep form1 'fixnum)
                 (arm2-form-typep form2 'fixnum))
          (arm2-use-operator (%nx1-operator %i+)
                             seg
                             vreg
                             xfer
                             form1
                             form2
                             t)
          (if (or (arm2-explicit-non-fixnum-type-p form1)
                  (arm2-explicit-non-fixnum-type-p form2))
            (arm2-binary-builtin seg vreg xfer '+-2 form1 form2)
            (arm2-inline-add2 seg vreg xfer form1 form2)))))))

(defarm2 arm2-sub2 sub2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *arm2-trust-declarations*)
    (if (and (arm2-form-typep form1 'double-float)
             (arm2-form-typep form2 'double-float))
      (arm2-use-operator (%nx1-operator %double-float--2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (arm2-form-typep form1 'single-float)
               (arm2-form-typep form2 'single-float))
        (arm2-use-operator (%nx1-operator %short-float--2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
        (if (and (arm2-form-typep form1 'fixnum)
                 (arm2-form-typep form2 'fixnum))
          (arm2-use-operator (%nx1-operator %i-)
                             seg
                             vreg
                             xfer
                             form1
                             form2
                             t)
          (if (or (arm2-explicit-non-fixnum-type-p form1)
                  (arm2-explicit-non-fixnum-type-p form2))
            (arm2-binary-builtin seg vreg xfer '--2 form1 form2)
            (arm2-inline-sub2 seg vreg xfer form1 form2)))))))

(defarm2 arm2-mul2 mul2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *arm2-trust-declarations*)
    (if (and (arm2-form-typep form1 'double-float)
             (arm2-form-typep form2 'double-float))
      (arm2-use-operator (%nx1-operator %double-float*-2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (arm2-form-typep form1 'single-float)
               (arm2-form-typep form2 'single-float))
        (arm2-use-operator (%nx1-operator %short-float*-2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
        (arm2-binary-builtin seg vreg xfer '*-2 form1 form2)))))


(defarm2 arm2-div2 div2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *arm2-trust-declarations*)
    (if (and (arm2-form-typep form1 'double-float)
             (arm2-form-typep form2 'double-float))
      (arm2-use-operator (%nx1-operator %double-float/-2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (arm2-form-typep form1 'single-float)
               (arm2-form-typep form2 'single-float))
        (arm2-use-operator (%nx1-operator %short-float/-2)
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
            (arm2-use-operator (%nx1-operator mul2)
                               seg
                               vreg
                               xfer
                               (make-acode (%nx1-operator fixnum) f1/f2)
                               (caddr unwrapped))
            (arm2-binary-builtin seg vreg xfer '/-2 form1 form2)))))))

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
                (! branch-unless-arg-fixnum ($ arm::arg_z) (aref *backend-labels* out-of-line)))
              (if (acode-fixnum-form-p form1)
                (! branch-unless-arg-fixnum ($ arm::arg_z) (aref *backend-labels* out-of-line))
                (if (acode-fixnum-form-p form2)
                  (! branch-unless-arg-fixnum ($ arm::arg_y) (aref *backend-labels* out-of-line))  
                  (! branch-unless-both-args-fixnums ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line)))))
            (if otherform
              (! logior-immediate ($ arm::arg_z) ($ arm::arg_z) (logand #xffffffff unboxed-fixval))
              (! %logior2 ($ arm::arg_z) ($ arm::arg_z) ($ arm::arg_y)))
            (-> done)
            (@ out-of-line)
            (if otherform
              (arm2-lri seg ($ arm::arg_y) (ash fixval *arm2-target-fixnum-shift*)))
            (! call-subprim-2 ($ arm::arg_z) (subprim-name->offset '.SPbuiltin-logior) ($ arm::arg_y) ($ arm::arg_z))
            (@ done)
            (arm2-copy-register seg target ($ arm::arg_z)))
          (^))))))

(defarm2 arm2-logior2 logior2 (seg vreg xfer form1 form2)
  (or (acode-optimize-logior2 seg vreg xfer form1 form2 *arm2-trust-declarations*)
      (if (or (arm2-explicit-non-fixnum-type-p form1)
              (arm2-explicit-non-fixnum-type-p form2))
        (arm2-binary-builtin seg vreg xfer 'logior-2 form1 form2)
        (arm2-inline-logior2 seg vreg xfer form1 form2))))

(defarm2 arm2-logxor2 logxor2 (seg vreg xfer form1 form2)
  (or (acode-optimize-logxor2 seg vreg xfer form1 form2 *arm2-trust-declarations*)
      (arm2-binary-builtin seg vreg xfer 'logxor-2 form1 form2)))

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
              (! branch-unless-arg-fixnum ($ arm::arg_z) (aref *backend-labels* out-of-line)))
            (if (acode-fixnum-form-p form1)
              (! branch-unless-arg-fixnum ($ arm::arg_z) (aref *backend-labels* out-of-line))
              (if (acode-fixnum-form-p form2)
                (! branch-unless-arg-fixnum ($ arm::arg_y) (aref *backend-labels* out-of-line))  
                (! branch-unless-both-args-fixnums ($ arm::arg_y) ($ arm::arg_z) (aref *backend-labels* out-of-line)))))
          (if otherform
            (! logand-immediate ($ arm::arg_z) ($ arm::arg_z) (logand #xffffffff unboxed-fixval))
            (! %logand2 ($ arm::arg_z) ($ arm::arg_z) ($ arm::arg_y)))
          (-> done)
          (@ out-of-line)
          (if otherform
            (arm2-lri seg ($ arm::arg_y) (ash fixval *arm2-target-fixnum-shift*)))
            (! call-subprim-2 ($ arm::arg_z) (subprim-name->offset '.SPbuiltin-logand) ($ arm::arg_y) ($ arm::arg_z))          
            (@ done)
            (arm2-copy-register seg target ($ arm::arg_z)))
        (^))))))

(defarm2 arm2-logand2 logand2 (seg vreg xfer form1 form2)
  (or (acode-optimize-logand2 seg vreg xfer form1 form2 *arm2-trust-declarations*)
      (if (or (arm2-explicit-non-fixnum-type-p form1)
              (arm2-explicit-non-fixnum-type-p form2))
        (arm2-binary-builtin seg vreg xfer 'logand-2 form1 form2)
        (arm2-inline-logand2 seg vreg xfer form1 form2))))



(defarm2 arm2-%aref1 %aref1 (seg vreg xfer v i)
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
      (arm2-vref  seg vreg xfer keyword v i (not *arm2-reckless*))
      (arm2-binary-builtin seg vreg xfer '%aref1 v i))))

(defarm2 arm2-%aset1 aset1 (seg vreg xfer v i n)
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
      (arm2-vset seg vreg xfer keyword v i n (not *arm2-reckless*))
      (arm2-ternary-builtin seg vreg xfer '%aset1 v i n))))

(defarm2 arm2-%i+ %i+ (seg vreg xfer form1 form2 &optional overflow)
  (when overflow
    (let* ((type *arm2-target-half-fixnum-type*))
      (when (and (arm2-form-typep form1 type)
                 (arm2-form-typep form2 type))
        (setq overflow nil))))
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2))
         (sum (and fix1 fix2 (if overflow (+ fix1 fix2) (%i+ fix1 fix2)))))
    (cond ((null vreg) 
           (arm2-form seg nil nil form1) 
           (arm2-form seg nil xfer form2))
          (sum
           (if (nx1-target-fixnump sum)
             (arm2-use-operator (%nx1-operator fixnum) seg vreg xfer sum)
             (arm2-use-operator (%nx1-operator immediate) seg vreg xfer sum)))
          (overflow
           (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg form1 arm::arg_y form2 arm::arg_z)
             (ensuring-node-target (target vreg)
               (if *arm2-open-code-inline*
                 (! fixnum-add-overflow-inline target r1 r2)
                 (progn
                   (! fixnum-add-overflow-ool ($ arm::arg_z) r1 r2)
                   (arm2-copy-register seg target ($ arm::arg_z)))))
             (^)))
          (t                              
           ;; There isn't any "addi" that checks for overflow, which is
           ;; why we didn't bother.
           (let* ((other (if (and fix1
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
             (if (and fix1 fix2)
               (arm2-lri seg vreg (ash (+ fix1 fix2) *arm2-target-fixnum-shift*))
               (if other
                 (let* ((constant (ash (or fix1 fix2) *arm2-target-fixnum-shift*))
                        (reg (arm2-one-untargeted-reg-form seg other arm::arg_z)))
                   (if (zerop constant)
                     (<- reg)
                     (ensuring-node-target (target vreg)
                       (! add-immediate target reg constant))))
                 (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg form1 arm::arg_y form2 arm::arg_z)
                   (ensuring-node-target (target vreg)
                     (! fixnum-add target r1 r2)))))
             (^))))))

(defarm2 arm2-%i- %i- (seg vreg xfer num1 num2 &optional overflow)
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
              (^)))))))))

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
  (let* ((*arm2-vstack* *arm2-vstack*)
         (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
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
        (let* ((*arm2-vstack* *arm2-vstack*)
               (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
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
  (arm2-check-lcell-depth)
  (with-arm-p2-declarations p2decls
    (arm2-seq-bind seg vars vals)
    (arm2-undo-body seg vreg xfer body old-stack))
  (dolist (v vars) (arm2-close-var seg v)))

(defarm2 arm2-multiple-value-bind multiple-value-bind (seg vreg xfer vars valform body p2decls)
  (let* ((n (list-length vars))
         (vloc *arm2-vstack*)
         (nbytes (* n *arm2-target-node-size*))
         (old-stack (arm2-encode-stack)))
    (with-arm-p2-declarations p2decls
      (arm2-multiple-value-body seg valform)
      (arm2-lri seg arm::imm0 nbytes)
      (! fitvals)
      (arm2-set-vstack (%i+ vloc nbytes))
      (let* ((old-top *arm2-top-vstack-lcell*)
             (lcells (progn (arm2-reserve-vstack-lcells n) (arm2-collect-lcells :reserved old-top))))
        (dolist (var vars)
          (let* ((lcell (pop lcells))
                 (reg (nx2-assign-register-var var)))
            (if reg
              (arm2-init-regvar seg var reg (arm2-vloc-ea vloc))
              (arm2-bind-var seg var vloc lcell))          
            (setq vloc (%i+ vloc *arm2-target-node-size*)))))
      (arm2-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (arm2-close-var seg var)))))

(defarm2 arm2-debind debind (seg vreg xfer lambda-list bindform req opt rest keys auxen whole body p2decls cdr-p)
  (declare (ignore lambda-list))
  (when cdr-p
    (compiler-bug "Unsupported: old destructuring code, cdr-p case."))
  (let* ((old-stack (arm2-encode-stack))
         (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
         (vloc *arm2-vstack*))
    (with-arm-p2-declarations p2decls      
      (arm2-bind-structured-lambda
       seg 
       (arm2-spread-lambda-list seg bindform whole req opt rest keys)
       vloc (arm2-vloc-ea vloc) whole req opt rest keys auxen)
      (arm2-undo-body seg vreg xfer body old-stack)
      (arm2-close-structured-lambda seg whole req opt rest keys auxen))))

(defarm2 arm2-multiple-value-prog1 multiple-value-prog1 (seg vreg xfer forms)
  (if (or (not (arm2-mv-p xfer)) (arm2-single-valued-form-p (%car forms)))
    (arm2-use-operator (%nx1-operator prog1) seg vreg xfer forms)
    (if (null (cdr forms))
      (arm2-form seg vreg xfer(car forms))
      (progn
        (let* ((*arm2-vstack* *arm2-vstack*)
               (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
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
    (arm2-compare seg vreg xfer form1 form2 cr-bit true-p)))

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
  (defmacro defarm2-df-op (fname opname vinsn safe-vinsn)
    `(defarm2 ,fname ,opname (seg vreg xfer f0 f1)
       (if (null vreg)
         (progn
           (arm2-form seg nil nil f0)
           (arm2-form seg vreg xfer f1))
         (with-fp-target () (r1 :double-float)
           (with-fp-target (r1) (r2 :double-float)
             (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg f0 r1 f1 r2)
               (if (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (if *arm2-float-safety*
                     (! ,safe-vinsn vreg r1 r2)
                     (! ,vinsn vreg r1 r2))
                 (with-fp-target (r1 r2) (result :double-float)
                   (if *arm2-float-safety*
                     (! ,safe-vinsn result r1 r2)
                     (! ,vinsn result r1 r2))
                   (ensuring-node-target (target vreg)
                     (arm2-copy-register seg target result))))
               (^)))))))
  
  (defmacro defarm2-sf-op (fname opname vinsn safe-vinsn)
    `(defarm2 ,fname ,opname (seg vreg xfer f0 f1)
       (if (null vreg)
         (progn
           (arm2-form seg nil nil f0)
           (arm2-form seg vreg xfer f1))
         (with-fp-target () (r1 :single-float)
           (with-fp-target (r1) (r2 :single-float)
             (multiple-value-bind (r1 r2) (arm2-two-untargeted-reg-forms seg f0 r1 f1 r2)
               (if (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (if *arm2-float-safety*
                   (! ,safe-vinsn vreg r1 r2)
                   (! ,vinsn vreg r1 r2))
                 (with-fp-target (r1 r2) (result :single-float)
                   (if *arm2-float-safety*
                     (! ,safe-vinsn result r1 r2)
                     (! ,vinsn result r1 r2))
                   (ensuring-node-target (target vreg)
                     (arm2-copy-register seg target result))))
               (^)))))))
)

(defarm2-df-op arm2-%double-float+-2 %double-float+-2 double-float+-2 double-float+-2-safe)
(defarm2-df-op arm2-%double-float--2 %double-float--2 double-float--2 double-float-2-safe)
(defarm2-df-op arm2-%double-float*-2 %double-float*-2 double-float*-2 double-float*-2-safe)
(defarm2-df-op arm2-%double-float/-2 %double-float/-2 double-float/-2 double-float/-2-safe)

(defarm2-sf-op arm2-%short-float+-2 %short-float+-2 single-float+-2 single-float+-2-safe)
(defarm2-sf-op arm2-%short-float--2 %short-float--2 single-float--2 single-float--2-safe)
(defarm2-sf-op arm2-%short-float*-2 %short-float*-2 single-float*-2 single-float*-2-safe)
(defarm2-sf-op arm2-%short-float/-2 %short-float/-2 single-float/-2 single-float/-2-safe)

(defun arm2-get-float (seg vreg xfer ptr offset double-p fp-reg)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (cond ((null vreg)
           (arm2-form seg nil nil ptr)
           (arm2-form seg nil xfer offset))
          (t
           (let* ((fixoffset (acode-fixnum-form-p offset)))
             (if (typep fixoffset '(unsigned-byte 15))
               (with-imm-target () (ptrreg :address)
                 (arm2-form seg ptrreg nil ptr)
                 (if double-p
                   (! mem-ref-c-double-float fp-reg ptrreg fixoffset)
                   (! mem-ref-c-single-float fp-reg ptrreg fixoffset)))
               (with-imm-target () (ptrreg :address)
                 (with-imm-target (ptrreg) (offsetreg :s32)
                   (arm2-two-targeted-reg-forms seg
                                                ptr ptrreg
                                                offset ($ arm::arg_z))
                   (! fixnum->signed-natural offsetreg arm::arg_z)
                   (if double-p
                     (! mem-ref-double-float fp-reg ptrreg offsetreg)
                     (! mem-ref-single-float fp-reg ptrreg offsetreg)))))
             (<- fp-reg))
           (^)))))
    

(defarm2 arm2-%get-double-float %get-double-float (seg vreg xfer ptr offset)
  (with-fp-target () (fp-reg :double-float)
    (arm2-get-float seg vreg xfer ptr offset t fp-reg)))

(defarm2 arm2-%get-single-float %get-single-float (seg vreg xfer ptr offset)
  (with-fp-target () (fp-reg :single-float)
    (arm2-get-float seg vreg xfer ptr offset nil fp-reg)))

(defun arm2-set-float (seg vreg xfer ptr offset newval double-p fp-reg)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (let* ((fixoffset (acode-fixnum-form-p offset))
           (immoffset (typep fixoffset '(unsigned-byte 15))))
      (with-imm-target () (ptr-reg :address) 
        (cond ((or (null vreg)
                   (= (hard-regspec-class vreg) hard-reg-class-fpr))
               (cond (immoffset
                      (arm2-push-register
                       seg
                       (arm2-one-untargeted-reg-form seg
                                                     ptr
                                                     ptr-reg))
                      (arm2-one-targeted-reg-form seg newval fp-reg)
                      (arm2-pop-register seg ptr-reg)
                      (if double-p
                        (! mem-set-c-double-float fp-reg ptr-reg fixoffset)
                        (! mem-set-c-single-float fp-reg ptr-reg fixoffset)))
                     (t
                      (with-imm-target (ptr-reg) (offset-reg :s32)
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
                        (arm2-pop-register seg ptr-reg)
                        (! fixnum->signed-natural offset-reg arm::arg_z)
                        (if double-p
                          (! mem-set-double-float fp-reg ptr-reg offset-reg)
                          (! mem-set-single-float fp-reg ptr-reg offset-reg)))))
               (<- fp-reg))
              (t
               (cond (immoffset
                      (let* ((rnew ($ arm::arg_z)))
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
                             (rnew ($ arm::arg_z)))
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
                   (! temp-push-unboxed-word src)
                   (arm2-open-undo $undostkblk)
                   (let* ((oreg (arm2-one-untargeted-reg-form seg offset arm::arg_z)))
                     (with-imm-temps () (src x)
                       (! temp-pop-unboxed-word src)
                       (arm2-close-undo)
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
        (with-imm-target ()
          (src-reg :address)
          (arm2-one-targeted-reg-form seg ptr src-reg)
          (if (node-reg-p vreg)
            (! mem-ref-c-bit-fixnum vreg src-reg byte-index (logand 31 (+ bit-shift
                                                                           *arm2-target-fixnum-shift*)))
            (with-imm-target ()           ;OK if src-reg & dest overlap
              (dest :u8)
              (! mem-ref-c-bit dest src-reg  byte-index bit-shift)
              (<- dest))))
        (let* ((triv-p (arm2-trivial-p offset))
               (offset-reg nil))
          (with-imm-target ()
            (src-reg :address)
            (arm2-one-targeted-reg-form seg ptr src-reg)
            (unless triv-p
              (! temp-push-unboxed-word src-reg)
              (arm2-open-undo $undostkblk))
            (setq offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z))
            (unless triv-p
              (! temp-pop-unboxed-word src-reg)
              (arm2-close-undo))
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
                         (progn
                           (! temp-push-unboxed-word src-reg)
                           (arm2-open-undo $undostkblk)
                           (! fixnum->signed-natural offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z))
                           (! temp-pop-unboxed-word src-reg)
                           (arm2-close-undo)))
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
                       (8 (! mem-ref-c-signed-doubleword dest src-reg offval))
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
                       (progn
                         (! temp-push-unboxed-word src-reg)
                         (arm2-open-undo $undostkblk)
                         (! fixnum->signed-natural offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z))
                         (! temp-pop-unboxed-word src-reg)
                         (arm2-close-undo)))
                  (case size
                    (8 (! mem-ref-signed-doubleword dest src-reg offset-reg))
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
                      (8 (! mem-ref-c-doubleword dest src-reg offval))
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
                       (progn
                         (! temp-push-unboxed-word src-reg)
                         (arm2-open-undo $undostkblk)
                         (! fixnum->signed-natural offset-reg (arm2-one-untargeted-reg-form seg offset arm::arg_z))
                         (! temp-pop-unboxed-word src-reg)
                         (arm2-close-undo)))
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

(defarm2 arm2-let let (seg vreg xfer vars vals body p2decls)
  (let* ((old-stack (arm2-encode-stack))
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
                     (arm2-vpush-register seg val :reserved))
                 (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg val arm::arg_z) :reserved))
                 (%rplacd pair *arm2-top-vstack-lcell*)))
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

;;; Make a function call (e.g., to mapcar) with some of the toplevel arguments
;;; stack-consed (downward) closures.  Bind temporaries to these closures so
;;; that tail-recursion/non-local exits work right.
;;; (all of the closures are distinct: FLET and LABELS establish dynamic extent themselves.)
(defarm2 arm2-with-downward-closures with-downward-closures (seg vreg xfer tempvars closures callform)
  (let* ((old-stack (arm2-encode-stack)))
    (arm2-seq-bind seg tempvars closures)
    (arm2-undo-body seg vreg xfer callform old-stack)
    (dolist (v tempvars) (arm2-close-var seg v))))


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
           (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
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
  (let* ((*arm2-vstack* *arm2-vstack*)
         (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*))
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
    (if mv-pass
      (! mkcatchmv)
      (! mkcatch1v))
    (! non-barrier-jump (aref *backend-labels* tag-label))
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
    (arm2-use-operator op seg vreg xfer n0 n1 *nx-t*)))



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
             (arm2-aref2 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         (if *arm2-reckless*
                           *nx-nil*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword        ;(make-acode (%nx1-operator immediate) )
                         (if (typep dim0 'fixnum) dim0) (if (typep dim1 'fixnum) dim1))))
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
             (arm2-aref3 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         k
                         (if *arm2-reckless*
                           *nx-nil*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword ;(make-acode (%nx1-operator immediate) )
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1)
                         (if (typep dim2 'fixnum) dim2))))
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
                         (if (typep dim1 'fixnum) dim1))))
          (t
           (arm2-four-targeted-reg-forms seg
                                         arr ($ arm::temp0)
                                         i ($ arm::arg_x)
                                         j ($ arm::arg_y)
                                         new ($ arm::arg_z))
           (arm2-fixed-call-builtin seg vreg xfer '.SPaset2)))))


(defarm2 arm2-general-aset3 general-aset3 (seg vreg xfer arr i j k new)
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
                         (if (typep dim2 'fixnum) dim2))))
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
         (old-stack (arm2-encode-stack))
         (ilevel '*interrupt-level*))
    (! nmkunwind)
    (arm2-open-undo $undointerruptlevel)
    (arm2-new-vstack-lcell :special-value *arm2-target-lcell-size* 0 ilevel)
    (arm2-new-vstack-lcell :special *arm2-target-lcell-size* (ash 1 $vbitspecial) ilevel)
    (arm2-new-vstack-lcell :special-link *arm2-target-lcell-size* 0 ilevel)
    (arm2-adjust-vstack (* 3 *arm2-target-node-size*))    
    (! non-barrier-jump (aref *backend-labels* cleanup-label))
    (-> protform-label)
    (@ cleanup-label)
    (let* ((*arm2-vstack* *arm2-vstack*)
           (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
           (*arm2-cstack* (%i+ *arm2-cstack* arm::lisp-frame.size)))
      (arm2-open-undo $undostkblk)      ; tsp frame created by nthrow.
      (! save-cleanup-context)
      (setq *arm2-cstack* (%i+ *arm2-cstack*
                               arm::lisp-frame.size))       ; the frame we just pushed
      (arm2-form seg nil nil cleanup-form)
      (arm2-close-undo)
      (! restore-cleanup-context)
      (! jump-return-pc)) ; blr
    (arm2-open-undo)
    (@ protform-label)
    (arm2-new-vstack-lcell :special-value *arm2-target-lcell-size* 0 ilevel)
    (arm2-new-vstack-lcell :special *arm2-target-lcell-size* (ash 1 $vbitspecial) ilevel)
    (arm2-new-vstack-lcell :special-link *arm2-target-lcell-size* 0 ilevel)
    (arm2-adjust-vstack (* 3 *arm2-target-node-size*))

    (arm2-undo-body seg vreg xfer protected-form old-stack)))

(defarm2 arm2-progv progv (seg vreg xfer symbols values body)
  (let* ((cleanup-label (backend-get-next-label))
         (protform-label (backend-get-next-label))
         (old-stack (arm2-encode-stack)))
    (arm2-two-targeted-reg-forms seg symbols ($ arm::arg_y) values ($ arm::arg_z))
    (! progvsave)                       ;creates an unwind-protect
    (arm2-open-undo $undostkblk)
    (! non-barrier-jump (aref *backend-labels* cleanup-label))
    (-> protform-label)
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
            (arm2-one-targeted-reg-form seg x target-a)
            (! temp-push-unboxed-word target-a)
            (arm2-open-undo $undostkblk)
            (arm2-one-targeted-reg-form seg y target-a)
            (with-imm-target (target-a) (target-b :address)
              (! temp-pop-unboxed-word target-b)
              (arm2-close-undo)
              (arm2-compare-registers seg vreg xfer target-b target-a cr-bit true-p))))))))

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
            (progn
              (unless triv-val
                (! temp-push-unboxed-word src)
                (arm2-open-undo $undostkblk))
              (let* ((target (arm2-one-untargeted-reg-form seg newval arm::arg_z)))
                (unless triv-val
                  (! temp-pop-unboxed-word src)
                  (arm2-close-undo))
                (! mem-set-c-bit src byte-index (+ 24 bit-index) target)
                (<- target)))))
        (progn
          (unless (and triv-val triv-offset)
            (! temp-push-unboxed-word src)
            (arm2-open-undo $undostkblk))
          (multiple-value-bind (idx-reg val-reg)
              (arm2-two-untargeted-reg-forms seg offset arm::arg_y newval arm::arg_z)
            (unless (and triv-val triv-offset)
              (! temp-pop-unboxed-word src)
              (arm2-close-undo ))
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
          (progn
            (unless triv-by
              (! temp-push-unboxed-word ptr-reg)
              (arm2-open-undo $undostkblk))
            (with-imm-target (ptr-reg) (by-reg :s32)
              (let* ((mask *available-backend-imm-temps*)
                     (*available-backend-imm-temps* mask))
                (when triv-by
                  (use-imm-temp (%hard-regspec-value ptr-reg)))
                (arm2-one-targeted-reg-form seg by by-reg)
                (setq *available-backend-imm-temps* mask)
                (unless triv-by
                  (! temp-pop-unboxed-word ptr-reg)
                  (arm2-close-undo))
                (with-imm-target () (result :address)
                  (! fixnum-add result ptr-reg by-reg)
                  (<- result))))))
        (^)))))



(defarm2 arm2-multiple-value-call multiple-value-call (seg vreg xfer fn arglist)
  (arm2-mvcall seg vreg xfer fn arglist))



(defarm2 arm2-eabi-syscall eabi-syscall (seg vreg xfer idx argspecs argvals resultspec)
  (let* ((*arm2-vstack* *arm2-vstack*)
         (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
         (*arm2-cstack* *arm2-cstack*)
         (nextarg 0))
    (declare (fixnum nextarg))
    (! alloc-c-frame (the fixnum (length argvals)))
    (arm2-open-undo $undo-arm-c-frame)
    ;; Evaluate each form into the C frame, according to the matching argspec.
    (do* ((specs argspecs (cdr specs))
          (vals argvals (cdr vals)))
         ((null specs))
      (declare (list specs vals))
      (let* ((valform (car vals))
             (spec (car specs))
             (absptr (acode-absolute-ptr-p valform)))
        (case spec
          (:address
           (with-imm-target ()
             (ptr :address)
             (if absptr
               (arm2-lri seg ptr absptr)
               (arm2-one-targeted-reg-form seg valform ptr))
             (! set-eabi-c-arg ptr nextarg)))
          (t
           (! set-eabi-c-arg
              (with-imm-target ()
                (valreg :natural)
                (arm2-unboxed-integer-arg-to-reg seg valform valreg spec))
              nextarg)))
        (incf nextarg)))
    (arm2-form seg arm::arg_z nil idx)
    (! eabi-syscall) 
    (arm2-close-undo)
    (when vreg
      (if (eq resultspec :void)
        (<- nil)
        (<- (set-regspec-mode arm::imm0 (gpr-mode-name-value
                                         (case resultspec
                                           (:address :address)
                                           (:signed-byte :s8)
                                           (:unsigned-byte :u8)
                                           (:signed-halfword :s16)
                                           (:unsigned-halfword :u16)
                                           (:signed-fullword :s32)
                                           (t :u32)))))))
    (^)))




(defun arm2-identity (seg vreg xfer arg)
  (with-arm-local-vinsn-macros (seg vreg xfer)
    (if (null vreg)
      (arm2-form seg vreg xfer arg)
      (progn
        (ensuring-node-target (target vreg)
          (arm2-one-targeted-reg-form seg arg target))
      (^)))))

(defarm2 arm2-eabi-ff-call eabi-ff-call (seg vreg xfer address argspecs argvals resultspec &optional monitor)
  (declare (ignore monitor))
  (let* ((*arm2-vstack* *arm2-vstack*)
         (*arm2-top-vstack-lcell* *arm2-top-vstack-lcell*)
         (*arm2-cstack* *arm2-cstack*)
         (next-arg-word 0)
         (natural-64-bit-alignment
          (case (backend-target-os *target-backend*)
            (:darwinarm nil)
            (t t))))
      (declare (fixnum next-arg-word))
      (dolist (argspec argspecs)
        (case argspec
          ((:double-float :unsigned-doubleword :signed-doubleword)
           (when (and natural-64-bit-alignment (oddp next-arg-word))
             (incf next-arg-word))
           (incf next-arg-word 2))
          (t (incf next-arg-word))))
      (! alloc-eabi-c-frame next-arg-word)
      (arm2-open-undo $undo-arm-c-frame)
      (arm2-vpush-register seg (arm2-one-untargeted-reg-form seg address arm::arg_z))
      ;; Evaluate each form into the C frame, according to the
      ;; matching argspec.
      ;; Remember type and arg offset of any FP args, since FP regs
      ;; will have to be loaded later.
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
             (let* ((df ($ arm::d0 :class :fpr :mode :double-float)))
               (when (and natural-64-bit-alignment (oddp next-arg-word))
                 (incf next-arg-word))
               (arm2-one-targeted-reg-form seg valform df)
               (! set-double-eabi-c-arg df next-arg-word)
               (incf next-arg-word 2)))
            (:single-float
             (let* ((sf ($ arm::s0 :class :fpr :mode :single-float)))
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
             (with-imm-target () (valreg :natural)
                (let* ((reg (arm2-unboxed-integer-arg-to-reg seg valform valreg spec)))
                  (! set-eabi-c-arg reg next-arg-word)
                  (incf next-arg-word)))))))
      #+hard-float
      (do* ((fpreg arm::fp1 (1+ fpreg))
            (reloads (nreverse fp-loads) (cdr reloads)))
           ((or (null reloads) (= fpreg arm::fp14)))
        (declare (list reloads) (fixnum fpreg))
        (let* ((reload (car reloads))
               (size (car reload))
               (from (cdr reload)))
          (if (eq size :double-float)
            (! reload-double-eabi-c-arg ($ fpreg :class :fpr :mode :double-float) from)
            (! reload-single-eabi-c-arg ($ fpreg :class :fpr :mode :single-float) from))))
      (arm2-vpop-register seg ($ arm::arg_z))
      (! eabi-ff-call) 
      (arm2-close-undo)
      (when vreg
        (cond ((eq resultspec :void) (<- nil))
              ((eq resultspec :double-float)
               (! gpr-pair-to-double-float arm::d0 arm::imm0 arm::imm1)
               (<- ($  arm::d0 :class :fpr :mode :double-float)))
              ((eq resultspec :single-float)
               (! gpr-to-single-float arm::s0 arm::imm0)
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
        (arm2-absolute-natural seg vreg xfer (+ fix-x fix-y))
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
        (arm2-absolute-natural seg vreg xfer (- fix-x fix-y))
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
        (arm2-absolute-natural seg vreg xfer (logior naturalx naturaly))
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
        (arm2-absolute-natural seg vreg xfer (logxor naturalx naturaly))
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
        (arm2-absolute-natural seg vreg xfer (logand naturalx naturaly))
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
                (<- other-reg))))
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
      ((:linuxarm :darwinarm) (! alloc-variable-eabi-c-frame reg)))
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
        (! double-to-single vreg dreg)
        (^))
      (with-fp-target () (argreg :double-float)
        (arm2-one-targeted-reg-form seg arg argreg)
        (with-fp-target ()  (sreg :single-float)
          (! double-to-single sreg argreg)
          (<- sreg)
          (^))))))

(defarm2 arm2-%single-to-double %single-to-double (seg vreg xfer arg)
  (if (null vreg)
    (arm2-form seg vreg xfer arg)
    (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
      (progn
        (arm2-one-untargeted-reg-form 
         seg arg
         (make-wired-lreg (hard-regspec-value vreg)
                          :class hard-reg-class-fpr
                          :mode hard-reg-class-fpr-mode-single))
        (^))
      (with-fp-target () (sreg :single-float)
        (arm2-one-targeted-reg-form seg arg sreg)
        (<- (set-regspec-mode sreg hard-reg-class-fpr-mode-double))
        (^)))))

(defarm2 arm2-%symptr->symvector %symptr->symvector (seg vreg xfer arg)
  (arm2-identity seg vreg xfer arg))

(defarm2 arm2-%symvector->symptr %symvector->symptr (seg vreg xfer arg)
  (arm2-identity seg vreg xfer arg))

(defarm2 arm2-%fixnum-to-double %fixnum-to-double (seg vreg xfer arg)
  (with-fp-target () (dreg :double-float)
    (let* ((r (arm2-one-untargeted-reg-form seg arg arm::arg_z)))
      (unless (or (acode-fixnum-form-p arg)
                  *arm2-reckless*)
        (! trap-unless-fixnum r))
      (! fixnum->double dreg r)
      (<- dreg)
      (^))))

(defarm2 arm2-%fixnum-to-single %fixnum-to-single (seg vreg xfer arg)
  (with-fp-target () (dreg :single-float)
    (let* ((r (arm2-one-untargeted-reg-form seg arg arm::arg_z)))
      (unless (or (acode-fixnum-form-p arg)
                  *arm2-reckless*)
        (! trap-unless-fixnum r))
      (if *arm2-float-safety*
        (! fixnum->single-safe dreg r)
        (! fixnum->single dreg r))
      (<- dreg)
      (^))))

(defarm2 arm2-%double-float %double-float (seg vreg xfer arg)
  (let* ((real (or (acode-fixnum-form-p arg)
                   (let* ((form (acode-unwrapped-form-value arg)))
                     (if (and (acode-p form)
                              (eq (acode-operator form)
                                  (%nx1-operator immediate))
                              (typep (cadr form) 'real))
                       (cadr form)))))
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
                              (typep (cadr form) 'real))
                       (cadr form)))))
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
  (or (acode-optimize-ash seg vreg xfer num amt *arm2-trust-declarations*)
      (progn
        (arm2-two-targeted-reg-forms seg num ($ arm::arg_y) amt ($ arm::arg_z))
        (arm2-fixed-call-builtin seg vreg xfer '.SPbuiltin-ash))))