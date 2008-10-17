;;;-*-Mode: LISP; Package: CCL -*-
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

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "PPCENV"))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (require "PPC-BACKEND"))

(defparameter *ppc2-debug-mask* 0)
(defconstant ppc2-debug-verbose-bit 0)
(defconstant ppc2-debug-vinsns-bit 1)
(defconstant ppc2-debug-lcells-bit 2)
(defparameter *ppc2-target-lcell-size* 0)
(defparameter *ppc2-target-node-size* 0)
(defparameter *ppc2-target-fixnum-shift* 0)
(defparameter *ppc2-target-node-shift* 0)
(defparameter *ppc2-target-bits-in-word* 0)
(defparameter *ppc2-ppc32-half-fixnum-type* '(signed-byte 29))
(defparameter *ppc2-ppc64-half-fixnum-type* `(signed-byte 60))
(defparameter *ppc2-target-half-fixnum-type* nil)



  
(defun ppc2-immediate-operand (x)
  (if (eq (acode-operator x) (%nx1-operator immediate))
    (cadr x)
    (compiler-bug "~&Bug: not an immediate: ~s" x)))

(defmacro with-ppc-p2-declarations (declsform &body body)
  `(let* ((*ppc2-tail-allow* *ppc2-tail-allow*)
          (*ppc2-reckless* *ppc2-reckless*)
          (*ppc2-open-code-inline* *ppc2-open-code-inline*)
          (*ppc2-trust-declarations* *ppc2-trust-declarations*)
	  (*ppc2-full-safety* *ppc2-full-safety*))
     (ppc2-decls ,declsform)
     ,@body))


(defmacro with-ppc-local-vinsn-macros ((segvar &optional vreg-var xfer-var) &body body)
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
                    `(%emit-vinsn ,',segvar ',,template-name-var (backend-p2-vinsn-templates *target-backend*) ,@,args-var))))
       (macrolet ((<- (,retvreg-var)
                    `(ppc2-copy-register ,',segvar ,',vreg-var ,,retvreg-var))
                  (@  (,labelnum-var)
                    `(backend-gen-label ,',segvar ,,labelnum-var))
                  (-> (,label-var)
                    `(! jump (aref *backend-labels* ,,label-var)))
                  (^ (&rest branch-args)
                    `(ppc2-branch ,',segvar ,',xfer-var ,',vreg-var ,@branch-args))
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












(defvar *ppc-current-context-annotation* nil)
(defvar *ppc2-woi* nil)
(defvar *ppc2-open-code-inline* nil)
(defvar *ppc2-register-restore-count* 0)
(defvar *ppc2-register-restore-ea* nil)
(defvar *ppc2-compiler-register-save-label* nil)
(defvar *ppc2-valid-register-annotations* 0)
(defvar *ppc2-register-annotation-types* nil)
(defvar *ppc2-register-ea-annotations* nil)

(defparameter *ppc2-tail-call-aliases*
  ()
  #| '((%call-next-method . (%tail-call-next-method . 1))) |#
  
)

(defvar *ppc2-popreg-labels* nil)
(defvar *ppc2-popj-labels* nil)
(defvar *ppc2-valret-labels* nil)
(defvar *ppc2-nilret-labels* nil)

(defvar *ppc2-icode* nil)
(defvar *ppc2-undo-stack* nil)
(defvar *ppc2-undo-because* nil)


(defvar *ppc2-cur-afunc* nil)
(defvar *ppc2-vstack* 0)
(defvar *ppc2-cstack* 0)
(defvar *ppc2-undo-count* 0)
(defvar *ppc2-returning-values* nil)
(defvar *ppc2-vcells* nil)
(defvar *ppc2-fcells* nil)
(defvar *ppc2-entry-vsp-saved-p* nil)

(defvar *ppc2-entry-label* nil)
(defvar *ppc2-tail-label* nil)
(defvar *ppc2-tail-vsp* nil)
(defvar *ppc2-tail-nargs* nil)
(defvar *ppc2-tail-allow* t)
(defvar *ppc2-reckless* nil)
(defvar *ppc2-full-safety* nil)
(defvar *ppc2-trust-declarations* nil)
(defvar *ppc2-entry-vstack* nil)
(defvar *ppc2-fixed-nargs* nil)
(defvar *ppc2-need-nargs* t)

(defparameter *ppc2-inhibit-register-allocation* nil)
(defvar *ppc2-record-symbols* nil)
(defvar *ppc2-recorded-symbols* nil)

(defvar *ppc2-result-reg* ppc::arg_z)





(declaim (fixnum *ppc2-vstack* *ppc2-cstack*))

 


;;; Before any defppc2's, make the *ppc2-specials* vector.

(defvar *ppc2-all-lcells* ())




     
(defun ppc2-free-lcells ()
  (without-interrupts 
   (let* ((prev (pool.data *lcell-freelist*)))
     (dolist (r *ppc2-all-lcells*)
       (setf (lcell-kind r) prev
             prev r))
     (setf (pool.data *lcell-freelist*) prev)
     (setq *ppc2-all-lcells* nil))))

(defun ppc2-note-lcell (c)
  (push c *ppc2-all-lcells*)
  c)

(defvar *ppc2-top-vstack-lcell* ())
(defvar *ppc2-bottom-vstack-lcell* ())

(defun ppc2-new-lcell (kind parent width attributes info)
  (ppc2-note-lcell (make-lcell kind parent width attributes info)))

(defun ppc2-new-vstack-lcell (kind width attributes info)
  (setq *ppc2-top-vstack-lcell* (ppc2-new-lcell kind *ppc2-top-vstack-lcell* width attributes info)))

(defun ppc2-reserve-vstack-lcells (n)
  (dotimes (i n) (ppc2-new-vstack-lcell :reserved *ppc2-target-lcell-size* 0 nil)))

(defun ppc2-vstack-mark-top ()
  (ppc2-new-lcell :tos *ppc2-top-vstack-lcell* 0 0 nil))

;;; Alist mapping VARs to lcells/lregs
(defvar *ppc2-var-cells* ())

(defun ppc2-note-var-cell (var cell)
  ;(format t "~& ~s -> ~s" (var-name var) cell)
  (push (cons var cell) *ppc2-var-cells*))

(defun ppc2-note-top-cell (var)
  (ppc2-note-var-cell var *ppc2-top-vstack-lcell*))

(defun ppc2-lookup-var-cell (var)
  (or (cdr (assq var *ppc2-var-cells*))
      (and nil (warn "Cell not found for ~s" (var-name var)))))

(defun ppc2-collect-lcells (kind &optional (bottom *ppc2-bottom-vstack-lcell*) (top *ppc2-top-vstack-lcell*))
  (do* ((res ())
        (cell top (lcell-parent cell)))
       ((eq cell bottom) res)
    (if (null cell)
      (compiler-bug "Horrible compiler bug.")
      (if (eq (lcell-kind cell) kind)
        (push cell res)))))



  
;;; ensure that lcell's offset matches what we expect it to.
;;; For bootstrapping.

(defun ppc2-ensure-lcell-offset (c expected)
  (if c (= (calc-lcell-offset c) expected) (zerop expected)))

(defun ppc2-check-lcell-depth (&optional (context "wherever"))
  (when (logbitp ppc2-debug-verbose-bit *ppc2-debug-mask*)
    (let* ((depth (calc-lcell-depth *ppc2-top-vstack-lcell*)))
      (or (= depth *ppc2-vstack*)
          (warn "~a: lcell depth = ~d, vstack = ~d" context depth *ppc2-vstack*)))))

(defun ppc2-do-lexical-reference (seg vreg ea)
  (when vreg
    (with-ppc-local-vinsn-macros (seg vreg) 
      (if (memory-spec-p ea)
        (ensuring-node-target (target vreg)
          (progn
            (ppc2-stack-to-register seg ea target)
            (if (addrspec-vcell-p ea)
              (! vcell-ref target target))))
        (<- ea)))))

(defun ppc2-do-lexical-setq (seg vreg ea valreg)
  (with-ppc-local-vinsn-macros (seg vreg)
    (cond ((typep ea 'lreg)
            (ppc2-copy-register seg ea valreg))
          ((addrspec-vcell-p ea)     ; closed-over vcell
           (ppc2-copy-register seg ppc::arg_z valreg)
           (ppc2-stack-to-register seg ea ppc::arg_x)
           (ppc2-copy-register seg ppc::arg_y ppc::rzero)
           (! call-subprim-3 ppc::arg_z (subprim-name->offset '.SPgvset) ppc::arg_x ppc::arg_y ppc::arg_z))
          ((memory-spec-p ea)    ; vstack slot
           (ppc2-register-to-stack seg valreg ea))
          (t
           (ppc2-copy-register seg ea valreg)))
    (when vreg
      (<- valreg))))

;;; ensure that next-method-var is heap-consed (if it's closed over.)
;;; it isn't ever setqed, is it ?
(defun ppc2-heap-cons-next-method-var (seg var)
  (with-ppc-local-vinsn-macros (seg)
    (when (eq (ash 1 $vbitclosed)
              (logand (logior (ash 1 $vbitclosed)
                              (ash 1 $vbitcloseddownward))
                      (the fixnum (nx-var-bits var))))
      (let* ((ea (var-ea var))
             (arg ($ ppc::arg_z))
             (result ($ ppc::arg_z)))
        (ppc2-do-lexical-reference seg arg ea)
        (ppc2-set-nargs seg 1)
        (! ref-constant ($ ppc::fname) (backend-immediate-index (ppc2-symbol-entry-locative '%cons-magic-next-method-arg)))
        (! call-known-symbol arg)
        (ppc2-do-lexical-setq seg nil ea result)))))

(defun ppc2-reverse-cc (cc)
  ;                NE  NE  EQ  EQ   LE   GE   LT   GT   GE   LE   GT   LT    MI   PL   PL   MI
  (%cdr (assq cc '((6 . 6) (7 . 7) (15 . 12) (13 . 14) (12 . 15) (14 . 13)  (11 . 10) (10 . 11)))))

  ;                NE  NE  EQ  EQ   LE   GE   LT   GT   GE   LE   GT   LT    MI   PL   PL   MI
(defun ppc2-reverse-condition-keyword (k)
  (cdr (assq k '((:ne . :ne) (:eq . :eq) (:le . :ge) (:lt . :gt) (:ge . :le) (:gt . :lt)))))




(defun acode-condition-to-ppc-cr-bit (cond)
  (condition-to-ppc-cr-bit (cadr cond)))

(defun condition-to-ppc-cr-bit (cond)
  (case cond
    (:EQ (values ppc::ppc-eq-bit t))
    (:NE (values ppc::ppc-eq-bit nil))
    (:GT (values ppc::ppc-gt-bit t))
    (:LE (values ppc::ppc-gt-bit nil))
    (:LT (values ppc::ppc-lt-bit t))
    (:GE (values ppc::ppc-lt-bit nil))))

;;; Generate the start and end bits for a RLWINM instruction that
;;; would be equivalent to to LOGANDing the constant with some value.
;;; Return (VALUES NIL NIL) if the constant contains more than one
;;; sequence of consecutive 1-bits, else bit indices.
;;; The caller generally wants to treat the constant as an (UNSIGNED-BYTE 32);
;;; since this uses LOGCOUNT and INTEGER-LENGTH to find the significant
;;; bits, it ensures that the constant is a (SIGNED-BYTE 32) that has
;;; the same least-significant 32 bits.
(defun ppc2-mask-bits (constant)
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
    

(defun ppc2-ensure-binding-indices-for-vcells (vcells)
  (dolist (cell vcells)
    (ensure-binding-index (car cell)))
  vcells)

(defun ppc2-compile (afunc &optional lambda-form *ppc2-record-symbols*)
  (progn
    (dolist (a  (afunc-inner-functions afunc))
      (unless (afunc-lfun a)
        (ppc2-compile a 
                      (if lambda-form 
                        (afunc-lambdaform a)) 
                      *ppc2-record-symbols*))) ; always compile inner guys
    (let* ((*ppc2-cur-afunc* afunc)
           (*ppc2-returning-values* nil)
           (*ppc-current-context-annotation* nil)
           (*ppc2-woi* nil)
           (*next-lcell-id* -1)
           (*ppc2-open-code-inline* nil)
           (*ppc2-register-restore-count* nil)
           (*ppc2-compiler-register-save-label* nil)
           (*ppc2-valid-register-annotations* 0)
           (*ppc2-register-ea-annotations* (ppc2-make-stack 16))
           (*ppc2-register-restore-ea* nil)
           (*ppc2-vstack* 0)
           (*ppc2-cstack* 0)
	   (*ppc2-target-lcell-size* (arch::target-lisp-node-size (backend-target-arch *target-backend*)))
           (*ppc2-target-fixnum-shift* (arch::target-fixnum-shift (backend-target-arch *target-backend*)))
           (*ppc2-target-node-shift* (arch::target-word-shift  (backend-target-arch *target-backend*)))
           (*ppc2-target-bits-in-word* (arch::target-nbits-in-word (backend-target-arch *target-backend*)))
	   (*ppc2-target-node-size* *ppc2-target-lcell-size*)
           (*ppc2-target-half-fixnum-type* (target-word-size-case
                                            (32 *ppc2-ppc32-half-fixnum-type*)
                                            (64 *ppc2-ppc64-half-fixnum-type*)))
           (*ppc2-all-lcells* ())
           (*ppc2-top-vstack-lcell* nil)
           (*ppc2-bottom-vstack-lcell* (ppc2-new-vstack-lcell :bottom 0 0 nil))
           (*ppc2-var-cells* nil)
           (*backend-vinsns* (backend-p2-vinsn-templates *target-backend*))
           (*backend-node-regs* ppc-node-regs)
           (*backend-node-temps* ppc-temp-node-regs)
           (*available-backend-node-temps* ppc-temp-node-regs)
           (*backend-imm-temps* ppc-imm-regs)
           (*available-backend-imm-temps* ppc-imm-regs)
           (*backend-crf-temps* ppc-cr-fields)
           (*available-backend-crf-temps* ppc-cr-fields)
           (*backend-fp-temps* ppc-temp-fp-regs)
           (*available-backend-fp-temps* ppc-temp-fp-regs)
           (bits 0)
           (*logical-register-counter* -1)
           (*backend-all-lregs* ())
           (*ppc2-popj-labels* nil)
           (*ppc2-popreg-labels* nil)
           (*ppc2-valret-labels* nil)
           (*ppc2-nilret-labels* nil)
           (*ppc2-undo-count* 0)
           (*backend-labels* (ppc2-make-stack 64 target::subtag-simple-vector))
           (*ppc2-undo-stack* (ppc2-make-stack 64  target::subtag-simple-vector))
           (*ppc2-undo-because* (ppc2-make-stack 64))
           (*backend-immediates* (ppc2-make-stack 64  target::subtag-simple-vector))
           (*ppc2-entry-label* nil)
           (*ppc2-tail-label* nil)
           (*ppc2-tail-vsp* nil)
           (*ppc2-tail-nargs* nil)
           (*ppc2-inhibit-register-allocation* nil)
           (*ppc2-tail-allow* t)
           (*ppc2-reckless* nil)
	   (*ppc2-full-safety* nil)
           (*ppc2-trust-declarations* t)
           (*ppc2-entry-vstack* nil)
           (*ppc2-fixed-nargs* nil)
           (*ppc2-need-nargs* t)
           (fname (afunc-name afunc))
           (*ppc2-entry-vsp-saved-p* nil)
           (*ppc2-vcells* (ppc2-ensure-binding-indices-for-vcells (afunc-vcells afunc)))
           (*ppc2-fcells* (afunc-fcells afunc))
           *ppc2-recorded-symbols*)
      (set-fill-pointer
       *backend-labels*
       (set-fill-pointer
        *ppc2-undo-stack*
        (set-fill-pointer 
         *ppc2-undo-because*
         (set-fill-pointer
          *backend-immediates* 0))))
      (backend-get-next-label)          ; start @ label 1, 0 is confused with NIL in compound cd
      (with-dll-node-freelist (vinsns *vinsn-freelist*)
        (unwind-protect
             (progn
               (setq bits (ppc2-form vinsns (make-wired-lreg *ppc2-result-reg*) $backend-return (afunc-acode afunc)))
               (dotimes (i (length *backend-immediates*))
                 (let ((imm (aref *backend-immediates* i)))
                   (when (ppc2-symbol-locative-p imm) (aset *backend-immediates* i (car imm)))))
               (optimize-vinsns vinsns)
               (when (logbitp ppc2-debug-vinsns-bit *ppc2-debug-mask*)
                 (format t "~% vinsns for ~s (after generation)" (afunc-name afunc))
                 (do-dll-nodes (v vinsns) (format t "~&~s" v))
                 (format t "~%~%"))
            
            
               (with-dll-node-freelist (*lap-instructions* *lap-instruction-freelist*)
                 (let* ((*lap-labels* nil))
                   (ppc2-expand-vinsns vinsns) 
                   (if (logbitp $fbitnonnullenv (the fixnum (afunc-bits afunc)))
                     (setq bits (+ bits (ash 1 $lfbits-nonnullenv-bit))))
                   (let* ((function-debugging-info (afunc-lfun-info afunc)))
                     (when (or function-debugging-info lambda-form *ppc2-record-symbols*)
                       (if lambda-form (setq function-debugging-info 
                                             (list* 'function-lambda-expression lambda-form function-debugging-info)))
                       (if *ppc2-record-symbols*
                         (setq function-debugging-info (nconc (list 'function-symbol-map *ppc2-recorded-symbols*)
                                                              function-debugging-info)))
                       (setq bits (logior (ash 1 $lfbits-info-bit) bits))
                       (backend-new-immediate function-debugging-info)))
                   (if (or fname lambda-form *ppc2-recorded-symbols*)
                     (backend-new-immediate fname)
                     (setq bits (logior (ash -1 $lfbits-noname-bit) bits)))                                     
                   (unless (afunc-parent afunc)
                     (ppc2-fixup-fwd-refs afunc))
                   (setf (afunc-all-vars afunc) nil)
                   (setf (afunc-argsword afunc) bits)
                   (let* ((regsave-label (if (typep *ppc2-compiler-register-save-label* 'vinsn-note)
                                           (vinsn-label-info (vinsn-note-label *ppc2-compiler-register-save-label*))))
                          (regsave-reg (if regsave-label (- 32 *ppc2-register-restore-count*)))
                          (regsave-addr (if regsave-label (- *ppc2-register-restore-ea*))))
                     (setf (afunc-lfun afunc)
                           (ppc2-xmake-function
                            *lap-instructions*
                            *lap-labels*
                            *backend-immediates*
                            bits
                            regsave-label
                            regsave-reg
                            regsave-addr
                            (if (and fname (symbolp fname)) (symbol-name fname)))))
                   (ppc2-digest-symbols))))
          (backend-remove-labels))))
    afunc))

(defun ppc2-xmake-function (codeheader labels imms bits *ppc-lap-regsave-label* *ppc-lap-regsave-reg* *ppc-lap-regsave-addr* &optional traceback-string)
  (let* ((*lap-instructions* codeheader)
         (*lap-labels* labels)
         (cross-compiling (not (eq *host-backend* *target-backend*)))
         (numimms (length imms))
         (function (%alloc-misc (+ numimms 2)
                                (if cross-compiling
                                  target::subtag-xfunction
                                  target::subtag-function))))
    (dotimes (i numimms)
      (setf (uvref function (1+ i)) (aref imms i)))
    (setf (uvref function (+ numimms 1)) bits)
    (let* ((maxpc (ppc-lap-encode-regsave-info (ppc-lap-do-labels)))
	   (traceback-size (traceback-fullwords traceback-string))
           (prefix (arch::target-code-vector-prefix (backend-target-arch *target-backend*)))
           (prefix-size (length prefix))
           (code-vector-size (+ traceback-size (ash maxpc -2) prefix-size)))
      #+ppc32-target
      (if (>= code-vector-size (ash 1 19)) (compiler-function-overflow))
      (let* ((code-vector (%alloc-misc code-vector-size
                                     (if cross-compiling
                                       target::subtag-xcode-vector
                                       target::subtag-code-vector)))
             (i prefix-size))
        (dotimes (i prefix-size)
          (setf (uvref code-vector i) (pop prefix)))
        (ppc-lap-resolve-labels)
        (do-dll-nodes (insn *lap-instructions*)
          (ppc-lap-generate-instruction code-vector i insn)
          (incf i))
        (unless (eql 0 traceback-size)
          (add-traceback-table code-vector i traceback-string))
        (setf (uvref function 0) code-vector)
        (%make-code-executable code-vector)
        function))))
      
    
(defun ppc2-make-stack (size &optional (subtype target::subtag-s16-vector))
  (make-uarray-1 subtype size t 0 nil nil nil nil t nil))

(defun ppc2-fixup-fwd-refs (afunc)
  (dolist (f (afunc-inner-functions afunc))
    (ppc2-fixup-fwd-refs f))
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

(defun ppc2-digest-symbols ()
  (if *ppc2-recorded-symbols*
    (let* ((symlist *ppc2-recorded-symbols*)
           (len (length symlist))
           (syms (make-array len))
           (ptrs (make-array (%i+  (%i+ len len) len)))
           (i -1)
           (j -1))
      (declare (fixnum i j))
      (dolist (info symlist (progn (%rplaca symlist syms)
                                   (%rplacd symlist ptrs)))
        (flet ((label-address (note start-p sym)
                 (let* ((label (vinsn-note-label note))
                        (lap-label (if label (vinsn-label-info label))))
                   (if lap-label
                     (lap-label-address lap-label)
                     (compiler-bug "Missing or bad ~s label: ~s" 
                       (if start-p 'start 'end) sym)))))
          (destructuring-bind (var sym startlab endlab) info
            (let* ((ea (var-ea var))
                   (ea-val (ldb (byte 16 0) ea)))
              (setf (aref ptrs (incf i)) (if (memory-spec-p ea)
                                           (logior (ash ea-val 6) #o77)
                                           ea-val)))
            (setf (aref syms (incf j)) sym)
            (setf (aref ptrs (incf i)) (label-address startlab t sym))
            (setf (aref ptrs (incf i)) (label-address endlab nil sym))))))))

(defun ppc2-decls (decls)
  (if (fixnump decls)
    (locally (declare (fixnum decls))
      (setq *ppc2-tail-allow* (neq 0 (%ilogand2 $decl_tailcalls decls))
            *ppc2-open-code-inline* (neq 0 (%ilogand2 $decl_opencodeinline decls))
	    *ppc2-full-safety* (neq 0 (%ilogand2 $decl_full_safety decls))
            *ppc2-reckless* (neq 0 (%ilogand2 $decl_unsafe decls))
            *ppc2-trust-declarations* (neq 0 (%ilogand2 $decl_trustdecls decls))))))


(defun %ppc2-bigger-cdr-than (x y)
  (declare (cons x y))
  (> (the fixnum (cdr x)) (the fixnum (cdr y))))

;;; Return an unordered list of "varsets": each var in a varset can be
;;; assigned a register and all vars in a varset can be assigned the
;;; same register (e.g., no scope conflicts.)

(defun ppc2-partition-vars (vars)
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
    ; This isn't optimal.  It partitions all register-allocatable variables into sets such that
    ; 1) no variable is a member of more than one set and
    ; 2) all variables in a given set are disjoint from each other
    ; A set might have exactly one member.
    ; If a register is allocated for any member of a set, it's allocated for all members of that
    ; set.
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
(defun ppc2-allocate-global-registers (fcells vcells all-vars no-regs)
  (if no-regs
    (progn
      (dolist (c fcells) (%rplacd c nil))
      (dolist (c vcells) (%rplacd c nil))
      (values 0 nil))
    (let* ((maybe (ppc2-partition-vars all-vars)))
      (dolist (c fcells) 
        (if (>= (the fixnum (cdr c)) 3) (push c maybe)))
      (dolist (c vcells) 
        (if (>= (the fixnum (cdr c)) 3) (push c maybe)))
      (do* ((things (%sort-list-no-key maybe #'%ppc2-bigger-cdr-than) (cdr things))
            (n 0 (1+ n))
            (regno ppc::save0 (1- regno))
            (constant-alist ()))
           ((or (null things) (= n $numppcsaveregs))
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
;;; Could use a STM here, especially if N is largish or optimizing for space.
(defun ppc2-save-nvrs (seg n)
  (declare (fixnum n))
  (when (> n 0)
    (setq *ppc2-compiler-register-save-label* (ppc2-emit-note seg :regsave))
    (with-ppc-local-vinsn-macros (seg)
      (if *ppc2-open-code-inline*
	(! save-nvrs-individually (- 32 n))
	(! save-nvrs (- 32 n))))
    (dotimes (i n)
      (ppc2-new-vstack-lcell :regsave *ppc2-target-lcell-size* 0 (- ppc::save0 i)))
    (incf *ppc2-vstack* (the fixnum (* n *ppc2-target-node-size*)))
    (setq *ppc2-register-restore-ea* *ppc2-vstack*
          *ppc2-register-restore-count* n)))


;;; If there are an indefinite number of args/values on the vstack,
;;; we have to restore from a register that matches the compiler's
;;; notion of the vstack depth.  This can be computed by the caller 
;;; (sum of vsp & nargs, or copy of vsp  before indefinite number of 
;;; args pushed, etc.)
;;; We DON'T try to compute this from the saved context, since the
;;; saved vsp may belong to a different stack segment.  (It's cheaper
;;; to compute/copy than to load it, anyway.)

(defun ppc2-restore-nvrs (seg ea nregs &optional from-fp)
  (when (null from-fp)
    (setq from-fp ppc::vsp))
  (when (and ea nregs)
    (with-ppc-local-vinsn-macros (seg)
      (let* ((first (- 32 nregs)))
        (declare (fixnum first))
        (! restore-nvrs first from-fp (- *ppc2-vstack* ea))))))



(defun ppc2-bind-lambda (seg lcells req opt rest keys auxen optsupvloc passed-in-regs lexpr &optional inherited
                             &aux (vloc 0) (numopt (list-length (%car opt)))
                             (nkeys (list-length (%cadr keys))) 
                             reg)
  (declare (fixnum vloc))
  (ppc2-check-lcell-depth)
  (dolist (arg inherited)
    (if (memq arg passed-in-regs)
      (ppc2-set-var-ea seg arg (var-ea arg))
      (let* ((lcell (pop lcells)))
        (if (setq reg (ppc2-assign-register-var arg))
          (ppc2-init-regvar seg arg reg (ppc2-vloc-ea vloc))
          (ppc2-bind-var seg arg vloc lcell))
        (setq vloc (%i+ vloc *ppc2-target-node-size*)))))
  (dolist (arg req)
    (if (memq arg passed-in-regs)
      (ppc2-set-var-ea seg arg (var-ea arg))
      (let* ((lcell (pop lcells)))
        (if (setq reg (ppc2-assign-register-var arg))
          (ppc2-init-regvar seg arg reg (ppc2-vloc-ea vloc))
          (ppc2-bind-var seg arg vloc lcell))
        (setq vloc (%i+ vloc *ppc2-target-node-size*)))))
  (when opt
    (if (ppc2-hard-opt-p opt)
      (setq vloc (apply #'ppc2-initopt seg vloc optsupvloc lcells (nthcdr (- (length lcells) numopt) lcells) opt)
            lcells (nthcdr numopt lcells))

      (dolist (var (%car opt))
        (if (memq var passed-in-regs)
          (ppc2-set-var-ea seg var (var-ea var))
          (let* ((lcell (pop lcells)))
            (if (setq reg (ppc2-assign-register-var var))
              (ppc2-init-regvar seg var reg (ppc2-vloc-ea vloc))
              (ppc2-bind-var seg var vloc lcell))
            (setq vloc (+ vloc *ppc2-target-node-size*)))))))
  (when rest
    (if lexpr
      (progn
        (if (setq reg (ppc2-assign-register-var rest))
          (progn
            (ppc2-load-lexpr-address seg reg)
            (ppc2-set-var-ea seg rest reg))
          (with-imm-temps () ((nargs-cell :natural))
            (ppc2-load-lexpr-address seg nargs-cell)
            (let* ((loc *ppc2-vstack*))
              (ppc2-vpush-register seg nargs-cell :reserved)
              (ppc2-note-top-cell rest)
              (ppc2-bind-var seg rest loc *ppc2-top-vstack-lcell*)))))
      (let* ((rvloc (+ vloc (* 2 *ppc2-target-node-size* nkeys))))
        (if (setq reg (ppc2-assign-register-var rest))
          (ppc2-init-regvar seg rest reg (ppc2-vloc-ea rvloc))
          (ppc2-bind-var seg rest rvloc (pop lcells))))))
  (when keys
    (apply #'ppc2-init-keys seg vloc lcells keys))  
  (ppc2-seq-bind seg (%car auxen) (%cadr auxen)))

(defun ppc2-initopt (seg vloc spvloc lcells splcells vars inits spvars)
  (with-ppc-local-vinsn-macros (seg)
    (dolist (var vars vloc)
      (let* ((initform (pop inits))
             (spvar (pop spvars))
             (lcell (pop lcells))
             (splcell (pop splcells))
             (reg (ppc2-assign-register-var var))
             (sp-reg ($ ppc::arg_z))
             (regloadedlabel (if reg (backend-get-next-label))))
        (unless (nx-null initform)
          (ppc2-stack-to-register seg (ppc2-vloc-ea spvloc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (ppc2-compare-register-to-nil seg crf (ppc2-make-compound-cd 0 skipinitlabel) sp-reg  ppc::ppc-eq-bit t))
            (if reg
              (ppc2-form seg reg regloadedlabel initform)
              (ppc2-register-to-stack seg (ppc2-one-untargeted-reg-form seg initform ($ ppc::arg_z)) (ppc2-vloc-ea vloc)))
            (@ skipinitlabel)))
        (if reg
          (progn
            (ppc2-init-regvar seg var reg (ppc2-vloc-ea vloc))
            (@ regloadedlabel))
          (ppc2-bind-var seg var vloc lcell))
        (when spvar
          (if (setq reg (ppc2-assign-register-var spvar))
            (ppc2-init-regvar seg spvar reg (ppc2-vloc-ea spvloc))
            (ppc2-bind-var seg spvar spvloc splcell))))
      (setq vloc (%i+ vloc *ppc2-target-node-size*))
      (if spvloc (setq spvloc (%i+ spvloc *ppc2-target-node-size*))))))

(defun ppc2-init-keys (seg vloc lcells allow-others keyvars keysupp keyinits keykeys)
  (declare (ignore keykeys allow-others))
  (with-ppc-local-vinsn-macros (seg)
    (dolist (var keyvars)
      (let* ((spvar (pop keysupp))
             (initform (pop keyinits))
             (reg (ppc2-assign-register-var var))
             (regloadedlabel (if reg (backend-get-next-label)))
             (var-lcell (pop lcells))
             (sp-lcell (pop lcells))
             (sp-reg ($ ppc::arg_z))
             (sploc (%i+ vloc *ppc2-target-node-size*)))
        (unless (nx-null initform)
          (ppc2-stack-to-register seg (ppc2-vloc-ea sploc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (ppc2-compare-register-to-nil seg crf (ppc2-make-compound-cd 0 skipinitlabel) sp-reg  ppc::ppc-eq-bit t))
            (if reg
              (ppc2-form seg reg regloadedlabel initform)
              (ppc2-register-to-stack seg (ppc2-one-untargeted-reg-form seg initform ($ ppc::arg_z)) (ppc2-vloc-ea vloc)))
            (@ skipinitlabel)))
        (if reg
          (progn
            (ppc2-init-regvar seg var reg (ppc2-vloc-ea vloc))
            (@ regloadedlabel))
          (ppc2-bind-var seg var vloc var-lcell))
        (when spvar
          (if (setq reg (ppc2-assign-register-var spvar))
            (ppc2-init-regvar seg spvar reg (ppc2-vloc-ea sploc))
            (ppc2-bind-var seg spvar sploc sp-lcell))))
      (setq vloc (%i+ vloc (* 2 *ppc2-target-node-size*))))))

;;; Vpush register r, unless var gets a globally-assigned register.
;;; Return NIL if register was vpushed, else var.
(defun ppc2-vpush-arg-register (seg reg var)
  (when var
    (let* ((bits (nx-var-bits var)))
      (declare (fixnum bits))
      (if (logbitp $vbitreg bits)
        var
        (progn 
          (ppc2-vpush-register seg reg :reserved)
          nil)))))


;;; nargs has been validated, arguments defaulted and canonicalized.
;;; Save caller's context, then vpush any argument registers that
;;; didn't get global registers assigned to their variables.
;;; Return a list of vars/nils for each argument register 
;;;  (nil if vpushed, var if still in arg_reg).
(defun ppc2-argregs-entry (seg revargs)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((nargs (length revargs))
           (reg-vars ()))
      (declare (type (unsigned-byte 16) nargs))
      (! save-lr)
      (if (<= nargs $numppcargregs)       ; caller didn't vpush anything
        (if *ppc2-open-code-inline*
          (! save-lisp-context-vsp)
          (! save-lisp-context-vsp-ool))
        (let* ((offset (* (the fixnum (- nargs $numppcargregs)) *ppc2-target-node-size*)))
          (declare (fixnum offset))
          (if *ppc2-open-code-inline*
            (! save-lisp-context-offset offset)
            (! save-lisp-context-offset-ool offset))))
      (destructuring-bind (&optional zvar yvar xvar &rest stack-args) revargs
        (let* ((nstackargs (length stack-args)))
          (ppc2-set-vstack (* nstackargs *ppc2-target-node-size*))
          (dotimes (i nstackargs)
            (ppc2-new-vstack-lcell :reserved *ppc2-target-lcell-size* 0 nil))
          (if (>= nargs 3)
            (push (ppc2-vpush-arg-register seg ($ ppc::arg_x) xvar) reg-vars))
          (if (>= nargs 2)
            (push (ppc2-vpush-arg-register seg ($ ppc::arg_y) yvar) reg-vars))
          (if (>= nargs 1)
            (push (ppc2-vpush-arg-register seg ($ ppc::arg_z) zvar) reg-vars))))
      reg-vars)))

;;; Just required args.
;;; Since this is just a stupid bootstrapping port, always save 
;;; lisp context.
(defun ppc2-req-nargs-entry (seg rev-fixed-args)
  (let* ((nargs (length rev-fixed-args)))
    (declare (type (unsigned-byte 16) nargs))
    (with-ppc-local-vinsn-macros (seg)
      (unless *ppc2-reckless*
        (! check-exact-nargs nargs))
      (ppc2-argregs-entry seg rev-fixed-args))))

;;; No more than three &optional args; all default to NIL and none have
;;; supplied-p vars.  No &key/&rest.
(defun ppc2-simple-opt-entry (seg rev-opt-args rev-req-args)
  (let* ((min (length rev-req-args))
         (nopt (length rev-opt-args))
         (max (+ min nopt)))
    (declare (type (unsigned-byte 16) min nopt max))
    (with-ppc-local-vinsn-macros (seg)
      (unless *ppc2-reckless*
        (when rev-req-args
          (! check-min-nargs min))
        (! check-max-nargs max))
      (if (= nopt 1)
        (! default-1-arg min)
        (if (= nopt 2)
          (! default-2-args min)
          (! default-3-args min)))
      (ppc2-argregs-entry seg (append rev-opt-args rev-req-args)))))

;;; if "num-fixed" is > 0, we've already ensured that at least that many args
;;; were provided; that may enable us to generate better code for saving the
;;; argument registers.
;;; We're responsible for computing the caller's VSP and saving
;;; caller's state.
(defun ppc2-lexpr-entry (seg num-fixed)
  (with-ppc-local-vinsn-macros (seg)
    (! save-lexpr-argregs num-fixed)
    (dotimes (i num-fixed)
      (! copy-lexpr-argument))
    (! save-lisp-context-lexpr)))

(defun ppc2-load-lexpr-address (seg dest)
  (with-ppc-local-vinsn-macros (seg)
    (! load-vframe-address dest *ppc2-vstack*)))


(defun ppc2-structured-initopt (seg lcells vloc context vars inits spvars)
  (with-ppc-local-vinsn-macros (seg)
    (dolist (var vars vloc)
      (let* ((initform (pop inits))
             (spvar (pop spvars))
             (spvloc (%i+ vloc *ppc2-target-node-size*))
             (var-lcell (pop lcells))
             (sp-reg ($ ppc::arg_z))
             (sp-lcell (pop lcells)))
        (unless (nx-null initform)
          (ppc2-stack-to-register seg (ppc2-vloc-ea spvloc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (ppc2-compare-register-to-nil seg crf (ppc2-make-compound-cd 0 skipinitlabel) sp-reg ppc::ppc-eq-bit t))
            (ppc2-register-to-stack seg (ppc2-one-untargeted-reg-form seg initform ($ ppc::arg_z)) (ppc2-vloc-ea vloc))
            (@ skipinitlabel)))
        (ppc2-bind-structured-var seg var vloc var-lcell context)
        (when spvar
          (ppc2-bind-var seg spvar spvloc sp-lcell)))
      (setq vloc (%i+ vloc (* 2 *ppc2-target-node-size*))))))



(defun ppc2-structured-init-keys (seg lcells vloc context allow-others keyvars keysupp keyinits keykeys)
  (declare (ignore keykeys allow-others))
  (with-ppc-local-vinsn-macros (seg)
    (dolist (var keyvars)
      (let* ((spvar (pop keysupp))
             (initform (pop keyinits))
             (sploc (%i+ vloc *ppc2-target-node-size*))
             (var-lcell (pop lcells))
             (sp-reg ($ ppc::arg_z))
             (sp-lcell (pop lcells)))
        (unless (nx-null initform)
          (ppc2-stack-to-register seg (ppc2-vloc-ea sploc) sp-reg)
          (let ((skipinitlabel (backend-get-next-label)))
            (with-crf-target () crf
              (ppc2-compare-register-to-nil seg crf (ppc2-make-compound-cd 0 skipinitlabel) sp-reg ppc::ppc-eq-bit t))
            (ppc2-register-to-stack seg (ppc2-one-untargeted-reg-form seg initform ($ ppc::arg_z)) (ppc2-vloc-ea vloc))
            (@ skipinitlabel)))
        (ppc2-bind-structured-var seg var vloc var-lcell context)
        (when spvar
          (ppc2-bind-var seg spvar sploc sp-lcell)))
      (setq vloc (%i+ vloc (* 2 *ppc2-target-node-size*))))))

(defun ppc2-vloc-ea (n &optional vcell-p)
  (setq n (make-memory-spec (dpb memspec-frame-address memspec-type-byte n)))
  (if vcell-p
    (make-vcell-memory-spec n)
    n))


(defun ppc2-form (seg vreg xfer form)
  (if (nx-null form)
    (ppc2-nil seg vreg xfer)
    (if (nx-t form)
      (ppc2-t seg vreg xfer)
      (let* ((op nil)
             (fn nil))
        (if (and (consp form)
                 (setq fn (svref *ppc2-specials* (%ilogand #.operator-id-mask (setq op (acode-operator form))))))
          (if (and (null vreg)
                   (%ilogbitp operator-acode-subforms-bit op)
                   (%ilogbitp operator-assignment-free-bit op))
            (dolist (f (%cdr form) (ppc2-branch seg xfer nil))
              (ppc2-form seg nil nil f ))
            (apply fn seg vreg xfer (%cdr form)))
          (compiler-bug "ppc2-form ? ~s" form))))))

;;; dest is a float reg - form is acode
(defun ppc2-form-float (seg freg xfer form)
  (declare (ignore xfer))
  (when (or (nx-null form)(nx-t form))(compiler-bug "ppc2-form to freg ~s" form))
  (when (and (= (get-regspec-mode freg) hard-reg-class-fpr-mode-double)
             (ppc2-form-typep form 'double-float))
    ; kind of screwy - encoding the source type in the dest register spec
    (set-node-regspec-type-modes freg hard-reg-class-fpr-type-double))
  (let* ((fn nil))
    (if (and (consp form)
             (setq fn (svref *ppc2-specials* (%ilogand #.operator-id-mask (acode-operator form)))))      
      (apply fn seg freg nil (%cdr form))
      (compiler-bug "ppc2-form ? ~s" form))))



(defun ppc2-form-typep (form type)
  (acode-form-typep form type *ppc2-trust-declarations*)
)

(defun ppc2-form-type (form)
  (acode-form-type form *ppc2-trust-declarations*))
  
(defun ppc2-use-operator (op seg vreg xfer &rest forms)
  (declare (dynamic-extent forms))
  (apply (svref *ppc2-specials* (%ilogand operator-id-mask op)) seg vreg xfer forms))

;;; Returns true iff lexical variable VAR isn't setq'ed in FORM.
;;; Punts a lot ...
(defun ppc2-var-not-set-by-form-p (var form)
  (or (not (%ilogbitp $vbitsetq (nx-var-bits var)))
      (ppc2-setqed-var-not-set-by-form-p var form)))

(defun ppc2-setqed-var-not-set-by-form-p (var form)
  (setq form (acode-unwrapped-form form))
  (or (atom form)
      (ppc-constant-form-p form)
      (ppc2-lexical-reference-p form)
      (let ((op (acode-operator form))
            (subforms nil))
        (if (eq op (%nx1-operator setq-lexical))
          (and (neq var (cadr form))
               (ppc2-setqed-var-not-set-by-form-p var (caddr form)))
          (and (%ilogbitp operator-side-effect-free-bit op)
               (flet ((not-set-in-formlist (formlist)
                        (dolist (subform formlist t)
                          (unless (ppc2-setqed-var-not-set-by-form-p var subform) (return)))))
                 (if
                   (cond ((%ilogbitp operator-acode-subforms-bit op) (setq subforms (%cdr form)))
                         ((%ilogbitp operator-acode-list-bit op) (setq subforms (cadr form))))
                   (not-set-in-formlist subforms)
                   (and (or (eq op (%nx1-operator call))
                            (eq op (%nx1-operator lexical-function-call)))
                        (ppc2-setqed-var-not-set-by-form-p var (cadr form))
                        (setq subforms (caddr form))
                        (not-set-in-formlist (car subforms))
                        (not-set-in-formlist (cadr subforms))))))))))
  
(defun ppc2-nil (seg vreg xfer)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if (ppc2-for-value-p vreg)
      (ensuring-node-target (target vreg)
        (! load-nil target)))
    (ppc2-branch seg (ppc2-cd-false xfer) vreg)))

(defun ppc2-t (seg vreg xfer)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if (ppc2-for-value-p vreg)
      (ensuring-node-target (target vreg)
        (! load-t target)))
    (ppc2-branch seg (ppc2-cd-true xfer) vreg)))

(defun ppc2-for-value-p (vreg)
  (and vreg (not (backend-crf-p vreg))))

(defun ppc2-mvpass (seg form &optional xfer)
  (with-ppc-local-vinsn-macros (seg)
    (ppc2-form seg  ($ ppc::arg_z) (logior (or xfer 0) $backend-mvpass-mask) form)))

(defun ppc2-adjust-vstack (delta)
  (ppc2-set-vstack (%i+ *ppc2-vstack* delta)))

(defun ppc2-set-vstack (new)
  (setq *ppc2-vstack* new))


;;; Emit a note at the end of the segment.
(defun ppc2-emit-note (seg class &rest info)
  (declare (dynamic-extent info))
  (let* ((note (make-vinsn-note class info)))
    (append-dll-node (vinsn-note-label note) seg)
    note))

;;; Emit a note immediately before the target vinsn.
(defun ppc-prepend-note (vinsn class &rest info)
  (declare (dynamic-extent info))
  (let* ((note (make-vinsn-note class info)))
    (insert-dll-node-before (vinsn-note-label note) vinsn)
    note))

(defun ppc2-close-note (seg note)
  (let* ((end (close-vinsn-note note)))
    (append-dll-node (vinsn-note-label end) seg)
    end))






(defun ppc2-stack-to-register (seg memspec reg)
  (with-ppc-local-vinsn-macros (seg)
    (! vframe-load reg (memspec-frame-address-offset memspec) *ppc2-vstack*)))

(defun ppc2-lcell-to-register (seg lcell reg)
  (with-ppc-local-vinsn-macros (seg)
    (! lcell-load reg lcell (ppc2-vstack-mark-top))))

(defun ppc2-register-to-lcell (seg reg lcell)
  (with-ppc-local-vinsn-macros (seg)
    (! lcell-store reg lcell (ppc2-vstack-mark-top))))

(defun ppc2-register-to-stack (seg reg memspec)
  (with-ppc-local-vinsn-macros (seg)
    (! vframe-store reg (memspec-frame-address-offset memspec) *ppc2-vstack*)))


(defun ppc2-ea-open (ea)
  (if (and ea (not (typep ea 'lreg)) (addrspec-vcell-p ea))
    (make-memory-spec (memspec-frame-address-offset ea))
    ea))

(defun ppc2-set-NARGS (seg n)
  (if (> n call-arguments-limit)
    (compiler-bug "~s exceeded." call-arguments-limit)
    (with-ppc-local-vinsn-macros (seg)
      (! set-nargs n))))

(defun ppc2-assign-register-var (v)
  (let ((bits (nx-var-bits v)))
    (when (%ilogbitp $vbitreg bits)
      (%ilogand bits $vrefmask))))

(defun ppc2-single-float-bits (the-sf)
  (single-float-bits the-sf))

(defun ppc2-double-float-bits (the-df)
  (double-float-bits the-df))

(defun ppc2-immediate (seg vreg xfer form)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
               (or (and (typep form 'double-float) (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
                   (and (typep form 'short-float)(= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))))
        (if (zerop form)
          (if (eql form 0.0d0)
            (! zero-double-float-register vreg)
            (! zero-single-float-register vreg))
          (if (typep form 'short-float)
            (let* ((bits (ppc2-single-float-bits form)))
              (with-imm-temps () ((bitsreg :u32))
                (! lri bitsreg bits)
                (! load-single-float-constant vreg bitsreg)))
            (multiple-value-bind (high low) (ppc2-double-float-bits form)
              (declare (integer high low))
              (with-imm-temps () ((highreg :u32) (lowreg :u32))
                (if (zerop high)
                  (setq highreg ($ ppc::rzero))
                  (! lri highreg high))
                (if (zerop low)
                  (setq lowreg ($ ppc::rzero))
                  (! lri lowreg low))
                (! load-double-float-constant vreg highreg lowreg)))))
        (if (and (typep form '(unsigned-byte 32))
                 (= (hard-regspec-class vreg) hard-reg-class-gpr)
                 (= (get-regspec-mode vreg)
                    hard-reg-class-gpr-mode-u32))
          (ppc2-lri seg vreg form)
          (ensuring-node-target
           (target vreg)
           (if (characterp form)
             (! load-character-constant target (char-code form))
             (ppc2-store-immediate seg form target)))))
      (if (and (listp form) *load-time-eval-token* (eq (car form) *load-time-eval-token*))
        (ppc2-store-immediate seg form ($ ppc::temp0))))
    (^)))

(defun ppc2-register-constant-p (form)
  (and (consp form)
           (or (memq form *ppc2-vcells*)
               (memq form *ppc2-fcells*))
           (%cdr form)))

(defun ppc2-store-immediate (seg imm dest)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((reg (ppc2-register-constant-p imm)))
      (if reg
        (ppc2-copy-register seg dest reg)
        (let* ((idx (backend-immediate-index imm)))
          (target-arch-case
           (:ppc32
            (if (< idx 8192)
              (! ref-constant dest idx)
              (with-imm-target () (idxreg :s32)
                (ppc2-lri seg idxreg (+ ppc32::misc-data-offset (ash (1+ idx) 2)))
                (! ref-indexed-constant dest idxreg))))
           (:ppc64
            (if (< idx 4096)
              (! ref-constant dest idx)
              (with-imm-target () (idxreg :s64)
                (ppc2-lri seg idxreg (+ ppc64::misc-data-offset (ash (1+ idx) 3)))
                (! ref-indexed-constant dest idxreg)))))))
      dest)))


;;; Returns label iff form is (local-go <tag>) and can go without adjusting stack.
(defun ppc2-go-label (form)
  (let ((current-stack (ppc2-encode-stack)))
    (while (and (acode-p form) (or (eq (acode-operator form) (%nx1-operator progn))
                                   (eq (acode-operator form) (%nx1-operator local-tagbody))))
      (setq form (caadr form)))
    (when (acode-p form)
      (let ((op (acode-operator form)))
        (if (and (eq op (%nx1-operator local-go))
                 (ppc2-equal-encodings-p (%caddr (%cadr form)) current-stack))
          (%cadr (%cadr form))
          (if (and (eq op (%nx1-operator local-return-from))
                   (nx-null (caddr form)))
            (let ((tagdata (car (cadr form))))
              (and (ppc2-equal-encodings-p (cdr tagdata) current-stack)
                   (null (caar tagdata))
                   (< 0 (cdar tagdata) $backend-mvpass)
                   (cdar tagdata)))))))))

(defun ppc2-single-valued-form-p (form)
  (setq form (acode-unwrapped-form form))
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


(defun ppc2-box-s32 (seg node-dest s32-src)
  (with-ppc-local-vinsn-macros (seg)
    (if (target-arch-case
         (:ppc32 *ppc2-open-code-inline*)
         (:ppc64 t))
      (! s32->integer node-dest s32-src)
      (let* ((arg_z ($ ppc::arg_z))
             (imm0 ($ ppc::imm0 :mode :s32)))
        (ppc2-copy-register seg imm0 s32-src)
        (! call-subprim (subprim-name->offset '.SPmakes32))
        (ppc2-copy-register seg node-dest arg_z)))))

(defun ppc2-box-s64 (seg node-dest s64-src)
  (with-ppc-local-vinsn-macros (seg)
    (if (target-arch-case
         (:ppc32 (compiler-bug "Bug!"))
         (:ppc64 *ppc2-open-code-inline*))
      (! s64->integer node-dest s64-src)
      (let* ((arg_z ($ ppc::arg_z))
             (imm0 ($ ppc::imm0 :mode :s64)))
        (ppc2-copy-register seg imm0 s64-src)
        (! call-subprim (subprim-name->offset '.SPmakes64))
        (ppc2-copy-register seg node-dest arg_z)))))

(defun ppc2-box-u32 (seg node-dest u32-src)
  (with-ppc-local-vinsn-macros (seg)
    (if (target-arch-case
         (:ppc32 *ppc2-open-code-inline*)
         (:ppc64 t))
      (! u32->integer node-dest u32-src)
      (let* ((arg_z ($ ppc::arg_z))
             (imm0 ($ ppc::imm0 :mode :u32)))
        (ppc2-copy-register seg imm0 u32-src)
        (! call-subprim (subprim-name->offset '.SPmakeu32))
        (ppc2-copy-register seg node-dest arg_z)))))

(defun ppc2-box-u64 (seg node-dest u64-src)
  (with-ppc-local-vinsn-macros (seg)
    (if (target-arch-case
         (:ppc32 (compiler-bug "Bug!"))
         (:ppc64 *ppc2-open-code-inline*))
      (! u64->integer node-dest u64-src)
      (let* ((arg_z ($ ppc::arg_z))
             (imm0 ($ ppc::imm0 :mode :u64)))
        (ppc2-copy-register seg imm0 u64-src)
        (! call-subprim (subprim-name->offset '.SPmakeu64))
        (ppc2-copy-register seg node-dest arg_z)))))

(defun ppc2-vref1 (seg vreg xfer type-keyword src unscaled-idx index-known-fixnum)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
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
                                             (target-word-size-case
                                              (32 (arch::target-max-32-bit-constant-index arch))
                                              (64 (arch::target-max-64-bit-constant-index arch)))))
               (! misc-ref-c-node target src index-known-fixnum)
               (with-imm-target () (idx-reg :u64)
                 (if index-known-fixnum
                   (ppc2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum *ppc2-target-node-shift*)))
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
                          (! misc-ref-c-u32 temp src index-known-fixnum)))))
               (with-imm-target () idx-reg
                 (if index-known-fixnum
                   (ppc2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 2)))
                   (! scale-32bit-misc-index idx-reg unscaled-idx))
                 (cond ((eq type-keyword :single-float-vector)
                        (! misc-ref-single-float fp-val src idx-reg))
                       (t
                        (if is-signed
                          (! misc-ref-s32 temp src idx-reg)
                          (! misc-ref-u32 temp src idx-reg)))))
               (case type-keyword
                 (:single-float-vector
                  (if (eq vreg-class hard-reg-class-fpr)
                    (<- fp-val)
                    (ensuring-node-target (target vreg)
                      (! single->node target fp-val))))
                 (:signed-32-bit-vector
                  (unless temp-is-vreg
                    (ensuring-node-target (target vreg)
                      (ppc2-box-s32 seg target temp))))
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
                      (ppc2-box-u32 seg target temp))))))))
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
                   (ppc2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) index-known-fixnum))
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
                     (ppc2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 1)))
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
                        (ppc2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3)))
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
                        (ppc2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3)))
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
                        (ppc2-absolute-natural seg idx-reg nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 3)))
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
               (with-imm-temps
                   () (word-index bitnum dest)
                 (if index-known-fixnum
                   (progn
                     (ppc2-lri seg word-index (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum -5)))
                     (ppc2-lri seg bitnum (logand index-known-fixnum #x1f)))
                   (! scale-1bit-misc-index word-index bitnum unscaled-idx))
                 (! misc-ref-u32 dest src word-index)
                 (! extract-variable-bit-fixnum target dest bitnum))))))))
    (^)))
             
    

;;; safe = T means assume "vector" is miscobj, do bounds check.
;;; safe = fixnum means check that subtag of vector = "safe" and do
;;;        bounds check.
;;; safe = nil means crash&burn.
;;; This mostly knows how to reference the elements of an immediate miscobj.
(defun ppc2-vref (seg vreg xfer type-keyword vector index safe)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((index-known-fixnum (acode-fixnum-form-p index))
           (unscaled-idx nil)
           (src nil))
      (if (or safe (not index-known-fixnum))
        (multiple-value-setq (src unscaled-idx)
          (ppc2-two-untargeted-reg-forms seg vector ppc::arg_y index ppc::arg_z))
        (setq src (ppc2-one-untargeted-reg-form seg vector ppc::arg_z)))
      (when safe
        (if (typep safe 'fixnum)
          (! trap-unless-typecode= src safe))
        (unless index-known-fixnum
          (! trap-unless-fixnum unscaled-idx))
        (! check-misc-bound unscaled-idx src))
      (ppc2-vref1 seg vreg xfer type-keyword src unscaled-idx index-known-fixnum))))



(defun ppc2-aset2 (seg vreg xfer  array i j new safe type-keyword dim0 dim1)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (ppc2-constant-value-ok-for-type-keyword type-keyword new))
           (needs-memoization (and is-node (ppc2-acode-needs-memoization new)))
           (src)
           (unscaled-i)
           (unscaled-j)
           (val-reg (ppc2-target-reg-for-aset vreg type-keyword))
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
            (ppc2-two-targeted-reg-forms seg array ($ ppc::temp0) new val-reg))
          (multiple-value-setq (src unscaled-i unscaled-j val-reg)
            (if needs-memoization
              (progn
                (ppc2-four-targeted-reg-forms seg
                                                array ($ ppc::temp0)
                                                i ($ ppc::arg_x)
                                                j ($ ppc::arg_y)
                                                new val-reg)
                (values ($ ppc::temp0) ($ ppc::arg_x) ($ ppc::arg_y) ($ ppc::arg_z)))
            (ppc2-four-untargeted-reg-forms seg
                                            array ($ ppc::temp0)
                                            i ($ ppc::arg_x)
                                            j ($ ppc::arg_y)
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
            (let* ((idx-reg ($ ppc::arg_y)))
              (unless constidx
                (if safe                  
                  (! check-2d-bound dim1 unscaled-i unscaled-j src)
                  (! 2d-dim1 dim1 src))
                (! 2d-unscaled-index idx-reg dim1 unscaled-i unscaled-j))
              (let* ((v ($ ppc::arg_x)))
                (! array-data-vector-ref v src)
                (ppc2-vset1 seg vreg xfer type-keyword v idx-reg constidx val-reg (ppc2-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization)))))))))


(defun ppc2-aset3 (seg vreg xfer  array i j k new safe type-keyword  dim0 dim1 dim2)
  (with-ppc-local-vinsn-macros (seg target)
    (let* ((i-known-fixnum (acode-fixnum-form-p i))
           (j-known-fixnum (acode-fixnum-form-p j))
           (k-known-fixnum (acode-fixnum-form-p k))
           (arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (ppc2-constant-value-ok-for-type-keyword type-keyword new))
           (needs-memoization (and is-node (ppc2-acode-needs-memoization new)))
           (src)
           (unscaled-i)
           (unscaled-j)
           (unscaled-k)
           (val-reg (ppc2-target-reg-for-aset vreg type-keyword))
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
            (ppc2-two-targeted-reg-forms seg array ($ ppc::temp0) new val-reg))
          (progn
            (setq src ($ ppc::temp1)
                  unscaled-i ($ ppc::temp0)
                  unscaled-j ($ ppc::arg_x)
                  unscaled-k ($ ppc::arg_y))
            (ppc2-push-register
             seg
             (ppc2-one-untargeted-reg-form seg array ($ ppc::arg_z)))
            (ppc2-four-targeted-reg-forms seg
                                          i ($ ppc::temp0)
                                          j ($ ppc::arg_x)
                                          k ($ ppc::arg_y)
                                          new val-reg)
            (ppc2-pop-register seg src)))
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
              (let* ((idx-reg ($ ppc::arg_y)))
                (unless constidx
                  (if safe                  
                    (! check-3d-bound dim1 dim2 unscaled-i unscaled-j unscaled-k src)
                    (! 3d-dims dim1 dim2 src))
                  (! 3d-unscaled-index idx-reg dim1 dim2 unscaled-i unscaled-j unscaled-k))
                (let* ((v ($ ppc::arg_x)))
                  (! array-data-vector-ref v src)
                  (ppc2-vset1 seg vreg xfer type-keyword v idx-reg constidx val-reg (ppc2-unboxed-reg-for-aset seg type-keyword val-reg safe constval) constval needs-memoization))))))))))

(defun ppc2-aref2 (seg vreg xfer array i j safe typekeyword &optional dim0 dim1)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
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
        (setq src (ppc2-one-targeted-reg-form seg array ($ ppc::arg_z)))
        (multiple-value-setq (src unscaled-i unscaled-j)
          (ppc2-three-untargeted-reg-forms seg
                                           array ppc::arg_x
                                           i ppc::arg_y
                                           j ppc::arg_z)))
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
            (ppc2-vref1 seg vreg xfer typekeyword v idx-reg constidx)))))))



(defun ppc2-aref3 (seg vreg xfer array i j k safe typekeyword &optional dim0 dim1 dim2)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
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
        (setq src (ppc2-one-targeted-reg-form seg array ($ ppc::arg_z)))
        (multiple-value-setq (src unscaled-i unscaled-j unscaled-k)
          (ppc2-four-untargeted-reg-forms seg
                                           array ppc::temp0
                                           i ppc::arg_x
                                           j ppc::arg_y
                                           k ppc::arg_z)))
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
          (ppc2-vref1 seg vreg xfer typekeyword v idx-reg constidx))))))


(defun ppc2-constant-value-ok-for-type-keyword (type-keyword form)
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
      (if typep val))))

(defun ppc2-target-reg-for-aset (vreg type-keyword)
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
         (acc (make-wired-lreg ppc::arg_z)))
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

(defun ppc2-unboxed-reg-for-aset (seg type-keyword result-reg safe constval)
  (with-ppc-local-vinsn-macros (seg)
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
                            (ppc2-lri seg reg (char-code constval))
                            (! unbox-base-char reg result-reg)))
                         ((eq type-keyword :single-float-vector)
                          (if (typep constval 'single-float)
                            (ppc2-lri seg reg (single-float-bits constval))
                            (progn
                              (when safe
                                (! trap-unless-single-float result-reg))
                              (! single-float-bits reg result-reg))))
                         (t
                          (if (typep constval '(unsigned-byte 32))
                            (ppc2-lri seg reg constval)
                            (! unbox-u32 reg result-reg))))
                   reg)))
              (is-16-bit
               (if is-signed
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s16)))
                   (if (typep constval '(signed-byte 16))
                     (ppc2-lri seg reg constval)
                     (! unbox-s16 reg result-reg))
                   reg)
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u16)))
                   (if (typep constval '(unsigned-byte 16))
                     (ppc2-lri seg reg constval)
                     (! unbox-u16 reg result-reg))
                   reg)))
              (is-8-bit
               (if is-signed
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-s8)))
                   (if (typep constval '(signed-byte 8))
                     (ppc2-lri seg reg constval)
                     (! unbox-s8 reg result-reg))
                   reg)
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u8)))
                   (if (typep constval '(unsigned-byte 8))
                     (ppc2-lri seg reg constval)
                     (! unbox-u8 reg result-reg))
                   reg)))
              (t
                 (let* ((reg (make-unwired-lreg next-imm-target :mode hard-reg-class-gpr-mode-u8)))
                   (unless (typep constval 'bit)
                     (! unbox-bit-bit0 reg result-reg))
                   reg)))))))
                   
      
;;; "val-reg" might be boxed, if the vreg requires it to be.
(defun ppc2-vset1 (seg vreg xfer type-keyword src  unscaled-idx index-known-fixnum val-reg unboxed-val-reg constval &optional (node-value-needs-memoization t))
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (is-1-bit (member type-keyword (arch::target-1-bit-ivector-types arch)))
           (is-8-bit (member type-keyword (arch::target-8-bit-ivector-types arch)))
           (is-16-bit (member type-keyword (arch::target-16-bit-ivector-types arch)))
           (is-32-bit (member type-keyword (arch::target-32-bit-ivector-types arch)))
           (is-64-bit (member type-keyword (arch::target-64-bit-ivector-types arch)))
           (is-signed (member type-keyword '(:signed-8-bit-vector :signed-16-bit-vector :signed-32-bit-vector :signed-64-bit-vector :fixnum-vector))))
      (cond ((and is-node node-value-needs-memoization)
             (unless (and (eql (hard-regspec-value src) ppc::arg_x)
                          (eql (hard-regspec-value unscaled-idx) ppc::arg_y)
                          (eql (hard-regspec-value val-reg) ppc::arg_z))
               (compiler-bug "Bug: invalid register targeting for gvset: ~s" (list src unscaled-idx val-reg)))
             (! call-subprim-3 val-reg (subprim-name->offset '.SPgvset) src unscaled-idx val-reg))
            (is-node
             (if (and index-known-fixnum (<= index-known-fixnum
                                             (target-word-size-case
                                              (32 (arch::target-max-32-bit-constant-index arch))
                                              (64 (arch::target-max-64-bit-constant-index arch)))))
               (! misc-set-c-node val-reg src index-known-fixnum)
               (with-imm-target () scaled-idx

                 (if index-known-fixnum
                   (ppc2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum *ppc2-target-node-shift*)))
                   (! scale-node-misc-index scaled-idx unscaled-idx))
                 (! misc-set-node val-reg src scaled-idx))))
            (t
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
                        (ppc2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-dfloat-offset arch) (ash index-known-fixnum 3)))
                        (! scale-64bit-misc-index scaled-idx unscaled-idx))
                      (if (eq type-keyword :double-float-vector)
                        (! misc-set-double-float unboxed-val-reg src scaled-idx)
                        (if is-signed
                          (! misc-set-s64 unboxed-val-reg src scaled-idx)
                          (! misc-set-u64 unboxed-val-reg src scaled-idx))))))
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
                        (ppc2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 2)))
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
                        (ppc2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) (ash index-known-fixnum 1)))
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
                        (ppc2-absolute-natural seg scaled-idx nil (+ (arch::target-misc-data-offset arch) index-known-fixnum))
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
                            (! set-constant-ppc-bit-to-0 word word bit-number)
                            (! set-constant-ppc-bit-to-1 word word bit-number))
                          (! set-constant-ppc-bit-to-variable-value word word unboxed-val-reg bit-number))
                        (! misc-set-c-u32 word src word-index)))
                    (with-imm-temps (unboxed-val-reg) (word-index bit-number temp)
                      (! scale-1bit-misc-index word-index bit-number unscaled-idx)
                      (if constval
                        (progn
                          (! lri temp #x80000000)
                          (! shift-right-variable-word bit-number temp bit-number)
                          (! misc-ref-u32 temp src word-index)
                          (if (zerop constval)
                            (! u32logandc2 temp temp bit-number)
                            (! u32logior temp temp bit-number)))
                        (with-imm-temps () (bitval)
                          (! shift-right-variable-word bitval unboxed-val-reg bit-number)
                          (! lri temp #x80000000)
                          (! shift-right-variable-word bit-number temp bit-number)
                          (! misc-ref-u32 temp src word-index)
                          (! u32logandc2 temp temp bit-number)
                          (! u32logior temp temp bitval)))
                      (! misc-set-u32 temp src word-index))))))))
      (when (and vreg val-reg) (<- val-reg))
      (^))))
                    

(defun ppc2-vset (seg vreg xfer type-keyword vector index value safe)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((arch (backend-target-arch *target-backend*))
           (is-node (member type-keyword (arch::target-gvector-types arch)))
           (constval (ppc2-constant-value-ok-for-type-keyword type-keyword value))
           (needs-memoization (and is-node (ppc2-acode-needs-memoization value)))
           (index-known-fixnum (acode-fixnum-form-p index)))
      (let* ((src ($ ppc::arg_x))
             (unscaled-idx ($ ppc::arg_y))
             (result-reg ($ ppc::arg_z)))
        (cond (needs-memoization
               (ppc2-three-targeted-reg-forms seg
                                              vector src
                                              index unscaled-idx
                                              value result-reg))
              (t
               (setq result-reg (ppc2-target-reg-for-aset vreg type-keyword))
               (ppc2-three-targeted-reg-forms seg
                                              vector src
                                              index unscaled-idx
                                              value result-reg)))
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
        (ppc2-vset1 seg vreg xfer type-keyword src unscaled-idx index-known-fixnum result-reg (ppc2-unboxed-reg-for-aset seg type-keyword result-reg safe constval) constval needs-memoization)))))


(defun ppc2-tail-call-alias (immref sym &optional arglist)
  (let ((alias (cdr (assq sym *ppc2-tail-call-aliases*))))
    (if (and alias (or (null arglist) (eq (+ (length (car arglist)) (length (cadr arglist))) (cdr alias))))
      (make-acode (%nx1-operator immediate) (car alias))
      immref)))

;;; If BODY is essentially an APPLY involving an &rest arg, try to avoid
;;; consing it.
(defun ppc2-eliminate-&rest (body rest key-p auxen rest-values)
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
                           (eq (ppc2-lexical-reference-p (%car reg-args)) rest))
                (return nil))
              (flet ((independent-of-all-values (form)        
                       (setq form (acode-unwrapped-form form))
                       (or (ppc-constant-form-p form)
                           (let* ((lexref (ppc2-lexical-reference-p form)))
                             (and lexref 
                                  (neq lexref rest)
                                  (dolist (val rest-values t)
                                    (unless (ppc2-var-not-set-by-form-p lexref val)
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
                                 (if (<= (length arglist) $numppcargregs)
                                   (list nil (reverse arglist))
                                   (list (butlast arglist $numppcargregs)
                                         (reverse (last arglist $numppcargregs))))
                                 nil)))
                  (return nil))))
            (if (eq op (%nx1-operator local-block))
              (setq body (%cadr body))
              (if (and (eq op (%nx1-operator if))
                       (eq (ppc2-lexical-reference-p (%cadr body)) rest))
                (setq body (%caddr body))
                (return nil)))))))))

(defun ppc2-call-fn (seg vreg xfer fn arglist spread-p)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (when spread-p
      (destructuring-bind (stack-args reg-args) arglist
        (when (and (null (cdr reg-args))
                   (nx-null (acode-unwrapped-form (car reg-args))))
          (setq spread-p nil)
          (let* ((nargs (length stack-args)))
            (declare (fixnum nargs))
            (if (<= nargs $numppcargregs)
              (setq arglist (list nil (reverse stack-args)))
              (setq arglist (list (butlast stack-args $numppcargregs) (reverse (last stack-args $numppcargregs)))))))))
    (let* ((lexref (ppc2-lexical-reference-p fn))
           (simple-case (or (fixnump fn)
                            (typep fn 'lreg)
                            (ppc2-immediate-function-p fn)
                            (and 
                             lexref
                             (not spread-p)
                             (flet ((all-simple (args)
                                      (dolist (arg args t)
                                        (when (and arg (not (ppc2-var-not-set-by-form-p lexref arg)))
                                          (return)))))
                               (and (all-simple (car arglist))
                                    (all-simple (cadr arglist))
                                    (setq fn (var-ea lexref)))))))
           (cstack *ppc2-cstack*)
           (top *ppc2-top-vstack-lcell*)
           (vstack *ppc2-vstack*))
      (setq xfer (or xfer 0))
      (when (and (eq xfer $backend-return)
                 (eq 0 *ppc2-undo-count*)
                 (acode-p fn)
                 (eq (acode-operator fn) (%nx1-operator immediate))
                 (symbolp (cadr fn)))
        (setq fn (ppc2-tail-call-alias fn (%cadr fn) arglist)))
      
      (if (and (eq xfer $backend-return) (not (ppc2-tailcallok xfer)))
        (progn
          (ppc2-call-fn seg vreg $backend-mvpass fn arglist spread-p)
          (ppc2-set-vstack (%i+ (if simple-case 0 *ppc2-target-node-size*) vstack))
          (setq  *ppc2-cstack* cstack)
          (let ((*ppc2-returning-values* t)) (ppc2-do-return seg)))
        (let* ((mv-p (ppc2-mv-p xfer)))
          (unless simple-case
            (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg fn ppc::arg_z))
            (setq fn (ppc2-vloc-ea vstack)))
          (ppc2-invoke-fn seg fn (ppc2-arglist seg arglist) spread-p xfer)
          (if (and (logbitp $backend-mvpass-bit xfer)
                   (not simple-case))
            (progn
              (! save-values)
              (! vstack-discard 1)
              (ppc2-set-nargs seg 0)
              (! recover-values))
            (unless (or mv-p simple-case)
              (! vstack-discard 1)))
          (ppc2-set-vstack vstack)
          (setq *ppc2-top-vstack-lcell* top)
          (setq *ppc2-cstack* cstack)
          (when (or (logbitp $backend-mvpass-bit xfer) (not mv-p))
            (<- ppc::arg_z)
            (ppc2-branch seg (logand (lognot $backend-mvpass-mask) xfer) vreg))))
      nil)))

(defun ppc2-restore-full-lisp-context (seg)
  (with-ppc-local-vinsn-macros (seg)
    (if *ppc2-open-code-inline*
      (! restore-full-lisp-context)
      (! restore-full-lisp-context-ool))))

(defun ppc2-call-symbol (seg jump-p)
  ; fname contains a symbol; we can either call it via
  ; a call to .SPjmpsym or expand the instructions inline.
  ; Since the branches are unconditional, the call doesn't
  ; cost much, but doing the instructions inline would give
  ; an instruction scheduler some opportunities to improve
  ; performance, so this isn't a strict time/speed tradeoff.
  ; This should probably dispatch on something other than
  ; *ppc2-open-code-inline*, since that does imply a time/speed
  ; tradeoff.
  (with-ppc-local-vinsn-macros (seg)
    (if *ppc2-open-code-inline*
      (if jump-p
        (! jump-known-symbol)
        (! call-known-symbol ppc::arg_z))
      (if jump-p
        (! jump-known-symbol-ool)
        (! call-known-symbol-ool)))))

;;; Nargs = nil -> multiple-value case.
(defun ppc2-invoke-fn (seg fn nargs spread-p xfer)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((f-op (acode-unwrapped-form fn))
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
           (destreg (if symp ($ ppc::fname) (if lfunp ($ ppc::nfn) (unless label-p ($ ppc::temp0)))))
           (alternate-tail-call
            (and tail-p label-p *ppc2-tail-label* (eql nargs *ppc2-tail-nargs*) (not spread-p)))
           )
      (when expression-p
                                        ;Have to do this before spread args, since might be vsp-relative.
        (if nargs
          (ppc2-do-lexical-reference seg destreg fn)
          (ppc2-copy-register seg destreg fn)))
      (if (or symp lfunp)
        (setq func (if symp (ppc2-symbol-entry-locative func)
                     (ppc2-afunc-lfun-ref func))
              a-reg (ppc2-register-constant-p func)))
      (when tail-p
        #-no-compiler-bugs
        (unless (or immp symp lfunp (typep fn 'lreg) (fixnump fn)) (compiler-bug "Well, well, well.  How could this have happened ?"))
        (when a-reg
          (ppc2-copy-register seg destreg a-reg))
        (unless spread-p
          (unless alternate-tail-call
            (if nargs
              (ppc2-restore-nvrs seg *ppc2-register-restore-ea* *ppc2-register-restore-count*)
              (when *ppc2-register-restore-count*
                (with-imm-temps () (vsp0)
                  (! fixnum-add vsp0 ppc::vsp ppc::nargs)
                  (ppc2-restore-nvrs seg *ppc2-register-restore-ea* *ppc2-register-restore-count* vsp0)))))))
      (if spread-p
        (progn
          (ppc2-set-nargs seg (%i- nargs 1))
          (when (and tail-p *ppc2-register-restore-count*)
            (! copy-gpr ppc::temp1 ppc::vsp)) ; .SPspread-lexpr-z & .SPspreadargz preserve temp1
          (if (eq spread-p 0)
            (! spread-lexpr)
            (! spread-list))
          (when (and tail-p *ppc2-register-restore-count*)
            (ppc2-restore-nvrs seg *ppc2-register-restore-ea* *ppc2-register-restore-count* ppc::temp1)))
        (if nargs
          (unless alternate-tail-call (ppc2-set-nargs seg nargs))
          (! pop-argument-registers)))
      (if callable
        (if (not tail-p)
          (if (ppc2-mvpass-p xfer)
            (let* ((call-reg (if symp ($ ppc::fname) ($ ppc::temp0))))
              (if label-p
                (ppc2-copy-register seg call-reg ($ ppc::fn))
                (if a-reg
                  (ppc2-copy-register seg call-reg  a-reg)
                  (ppc2-store-immediate seg func call-reg)))
              (if symp
                (! pass-multiple-values-symbol)
                (! pass-multiple-values)))
            (progn 
              (if label-p
                (progn
                  (ppc2-copy-register seg ($ ppc::nfn) ($  ppc::fn))
                  (! call-label (aref *backend-labels* 1)))
                (progn
                  (if a-reg
                    (ppc2-copy-register seg destreg a-reg)
                    (ppc2-store-immediate seg func destreg))
                  (if symp
                    (ppc2-call-symbol seg nil)
                    (! call-known-function))))))
          (if alternate-tail-call
            (progn
              (ppc2-unwind-stack seg xfer 0 0 *ppc2-tail-vsp*)
              (! jump (aref *backend-labels* *ppc2-tail-label*)))
            (progn
              (ppc2-unwind-stack seg xfer 0 0 #x7fffff)
              (if (and (not spread-p) nargs (%i<= nargs $numppcargregs))
                (progn
                  (if label-p
                    (ppc2-copy-register seg ppc::nfn ppc::fn))
                  (unless (or label-p a-reg) (ppc2-store-immediate seg func destreg))
                  (ppc2-restore-full-lisp-context seg)
                  (if label-p
                    (! jump (aref *backend-labels* 1))
                    (progn
                      (if symp
                        (ppc2-call-symbol seg t)
                        (! jump-known-function)))))
                (progn
                  (if label-p
                    (ppc2-copy-register seg ppc::nfn ppc::fn)
                    (unless a-reg (ppc2-store-immediate seg func destreg)))
                  (cond ((or spread-p (null nargs))
                         (if symp
                           (! tail-call-sym-gen)
                           (! tail-call-fn-gen)))
                        ((%i> nargs $numppcargregs)
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
            (ppc2-one-targeted-reg-form seg fn destreg))
          (if (not tail-p)
            (if (ppc2-mvpass-p xfer)
              (! pass-multiple-values)
              (! funcall))                  
            (cond ((or (null nargs) spread-p)
                   (! tail-funcall-gen))
                  ((%i> nargs $numppcargregs)
                   (! tail-funcall-slide))
                  (t
                   (! tail-funcall-vsp)))))))
    nil))

(defun ppc2-seq-fbind (seg vreg xfer vars afuncs body p2decls)
  (let* ((old-stack (ppc2-encode-stack))
         (copy afuncs)
         (func nil))
    (with-ppc-p2-declarations p2decls 
      (dolist (var vars) 
        (when (neq 0 (afunc-fn-refcount (setq func (pop afuncs))))
          (ppc2-seq-bind-var seg var (nx1-afunc-ref func))))
      (ppc2-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (when (neq 0 (afunc-fn-refcount (setq func (pop copy))))
          (ppc2-close-var seg var))))))

(defun ppc2-make-closure (seg afunc downward-p)
  (with-ppc-local-vinsn-macros (seg)
    (flet ((var-to-reg (var target)
             (let* ((ea (var-ea (var-bits var))))
               (if ea
                 (ppc2-addrspec-to-reg seg (ppc2-ea-open ea) target)
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
             (dest ($ ppc::arg_z))
             (vsize (+ (length inherited-vars) 
                       2                ; %closure-code%, afunc
                       2)))             ; name, lfun-bits
        (declare (list inherited-vars))
        (if downward-p
          (progn
            (let* ((*ppc2-vstack* *ppc2-vstack*)
                   (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
              (ppc2-lri seg ppc::arg_x (ash (nx-lookup-target-uvector-subtag :function) *ppc2-target-fixnum-shift*))
              (! %closure-code% ppc::arg_y)
              (ppc2-store-immediate seg (ppc2-afunc-lfun-ref afunc) ppc::arg_z)
              (ppc2-vpush-register-arg seg ppc::arg_x)
              (ppc2-vpush-register-arg seg ppc::arg_y)
              (ppc2-vpush-register-arg seg ppc::arg_z)
                                        ; Could be smarter about memory traffic here.
              (dolist (v inherited-vars)
                (ppc2-vpush-register-arg seg (var-to-reg v ppc::arg_z)))
              (! load-nil ppc::arg_z)
              (ppc2-vpush-register-arg seg ppc::arg_z)
              (ppc2-lri seg ppc::arg_z (ash (ash 1 $lfbits-trampoline-bit) *ppc2-target-fixnum-shift*))
              (ppc2-vpush-register-arg seg ppc::arg_z)
              (ppc2-set-nargs seg (1+ vsize)) ; account for subtag
              (! make-stack-gvector))
            (ppc2-open-undo $undostkblk))
          (let* ((cell 0))
            (declare (fixnum cell))
            (progn
              (ppc2-lri seg
                        ppc::imm0
                        (arch::make-vheader vsize (nx-lookup-target-uvector-subtag :function)))
              (! %alloc-misc-fixed dest ppc::imm0 (ash vsize (arch::target-word-shift arch)))
              )       
            (! %closure-code% ppc::arg_x)
            (ppc2-store-immediate seg (ppc2-afunc-lfun-ref afunc) ppc::arg_y)
            (with-node-temps (ppc::arg_z) (t0 t1 t2 t3)
              (do* ((ccode ppc::arg_x nil)
                    (func ppc::arg_y nil))
                   ((null inherited-vars))
                (let* ((t0r (or ccode (if inherited-vars (var-to-reg (pop inherited-vars) t0))))
                       (t1r (or func (if inherited-vars (var-to-reg (pop inherited-vars) t1))))
                       (t2r (if inherited-vars (var-to-reg (pop inherited-vars) t2)))
                       (t3r (if inherited-vars (var-to-reg (pop inherited-vars) t3))))
                  (setq cell (set-some-cells dest cell t0r t1r t2r t3r)))))
            (ppc2-lri seg ppc::arg_y (ash (ash 1 $lfbits-trampoline-bit) *ppc2-target-fixnum-shift*))
            (! load-nil ppc::arg_x)
            (! misc-set-c-node ppc::arg_x dest cell)
            (! misc-set-c-node ppc::arg_y dest (1+ cell))))
        dest))))
        
(defun ppc2-symbol-entry-locative (sym)
  (setq sym (require-type sym 'symbol))
  (when (eq sym '%call-next-method-with-args)
    (setf (afunc-bits *ppc2-cur-afunc*)
          (%ilogior (%ilsl $fbitnextmethargsp 1) (afunc-bits *ppc2-cur-afunc*))))
  (or (assq sym *ppc2-fcells*)
      (let ((new (list sym)))
        (push new *ppc2-fcells*)
        new)))

(defun ppc2-symbol-value-cell (sym)
  (setq sym (require-type sym 'symbol))
  (or (assq sym *ppc2-vcells*)
      (let ((new (list sym)))
        (push new *ppc2-vcells*)
        (ensure-binding-index sym)
        new)))


(defun ppc2-symbol-locative-p (imm)
  (and (consp imm)
       (or (memq imm *ppc2-vcells*)
           (memq imm *ppc2-fcells*))))




(defun ppc2-immediate-function-p (f)
  (setq f (acode-unwrapped-form f))
  (and (acode-p f)
       (or (eq (%car f) (%nx1-operator immediate))
           (eq (%car f) (%nx1-operator simple-function)))))

(defun ppc-constant-form-p (form)
  (setq form (nx-untyped-form form))
  (if form
    (or (nx-null form)
        (nx-t form)
        (and (consp form)
             (or (eq (acode-operator form) (%nx1-operator immediate))
                 (eq (acode-operator form) (%nx1-operator fixnum))
                 (eq (acode-operator form) (%nx1-operator simple-function)))))))


  
(defun ppc2-integer-constant-p (form mode)
  (let* ((val 
         (or (acode-fixnum-form-p (setq form (acode-unwrapped-form form)))
             (and (acode-p form)
                  (eq (acode-operator form) (%nx1-operator immediate))
                  (setq form (%cadr form))
                  (if (typep form 'integer)
                    form)))))
    (and val (%typep val (mode-specifier-type mode)) val)))


(defun ppc-side-effect-free-form-p (form)
  (when (consp (setq form (acode-unwrapped-form form)))
    (or (ppc-constant-form-p form)
        ;(eq (acode-operator form) (%nx1-operator bound-special-ref))
        (if (eq (acode-operator form) (%nx1-operator lexical-reference))
          (not (%ilogbitp $vbitsetq (nx-var-bits (%cadr form))))))))

(defun ppc2-formlist (seg stkargs &optional revregargs)
  (with-ppc-local-vinsn-macros (seg)  
    (let* ((nregs (length revregargs))
           (n nregs))
      (declare (fixnum n))
      (dolist (arg stkargs)
        (let* ((reg (ppc2-one-untargeted-reg-form seg arg ppc::arg_z)))
          (ppc2-vpush-register-arg seg reg)
          (incf n)))
      (when revregargs
        (let* ((zform (%car revregargs))
               (yform (%cadr revregargs))
               (xform (%caddr revregargs)))
          (if (eq 3 nregs)
            (ppc2-three-targeted-reg-forms seg xform ($ ppc::arg_x) yform ($ ppc::arg_y) zform ($ ppc::arg_z))
            (if (eq 2 nregs)
              (ppc2-two-targeted-reg-forms seg yform ($ ppc::arg_y) zform ($ ppc::arg_z))
              (ppc2-one-targeted-reg-form seg zform ($ ppc::arg_z))))))
      n)))

(defun ppc2-arglist (seg args)
  (ppc2-formlist seg (car args) (cadr args)))





(defun ppc2-unboxed-integer-arg-to-reg (seg form immreg &optional ffi-arg-type)
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
    (with-ppc-local-vinsn-macros (seg)
      (let* ((value (ppc2-integer-constant-p form mode)))
        (if value
          (if (eql value 0)
            (make-wired-lreg ppc::rzero :mode modeval)
            (progn
              (unless (typep immreg 'lreg)
                (setq immreg (make-unwired-lreg immreg :mode modeval)))
              (ppc2-lri seg immreg value)
              immreg))
          (progn 
            (ppc2-one-targeted-reg-form seg form (make-wired-lreg ppc::imm0 :mode modeval))))))))


(defun ppc2-macptr-arg-to-reg (seg form address-reg)  
  (ppc2-one-targeted-reg-form seg
                              form 
                              address-reg))


(defun ppc2-one-lreg-form (seg form lreg)
  (let ((is-float (= (hard-regspec-class lreg) hard-reg-class-fpr)))
    (if is-float
      (ppc2-form-float seg lreg nil form)
      (ppc2-form seg lreg nil form))
    lreg))

(defun ppc2-one-targeted-reg-form (seg form reg)
  (ppc2-one-lreg-form seg form reg))

(defun ppc2-one-untargeted-lreg-form (seg form reg)
  (ppc2-one-lreg-form seg form (if (typep reg 'lreg) reg (make-unwired-lreg reg))))

(defun ppc2-one-untargeted-reg-form (seg form suggested)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((gpr-p (= (hard-regspec-class suggested) hard-reg-class-gpr))
           (node-p (if gpr-p (= (get-regspec-mode suggested) hard-reg-class-gpr-mode-node))))
      (if node-p
        (let* ((ref (ppc2-lexical-reference-ea form))
               (reg (backend-ea-physical-reg ref hard-reg-class-gpr)))
          (if reg
            ref
            (if (nx-null form)
              (progn
                (! load-nil suggested)
                suggested)
              (if (eql 0 (acode-fixnum-form-p form))
                ($ ppc::rzero)
                (if (and (acode-p form) 
                         (eq (acode-operator form) (%nx1-operator immediate)) 
                         (setq reg (ppc2-register-constant-p (cadr form))))
                  reg
                  (if (and (acode-p form)
                           (eq (acode-operator form) (%nx1-operator %current-tcr)))
                    (target-arch-case
                     (:ppc32 ($ ppc32::rcontext))
                     (:ppc64 ($ ppc64::rcontext)))
                    (ppc2-one-untargeted-lreg-form seg form suggested)))))))
        (ppc2-one-untargeted-lreg-form seg form suggested)))))
             

(defun ppc2-push-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (a-double (if a-float (= (get-regspec-mode areg) hard-reg-class-fpr-mode-double)))
         (a-node (unless a-float (= (get-regspec-mode areg) hard-reg-class-gpr-mode-node)))
         vinsn)
    (with-ppc-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (ppc2-vpush-register seg areg :node-temp))
        (progn
          (setq vinsn
                (if a-float
                  (if a-double
                    (! temp-push-double-float areg)
                    (! temp-push-single-float areg))
                  (! temp-push-unboxed-word areg)))
          (ppc2-open-undo $undostkblk)))
      vinsn)))

(defun ppc2-pop-register (seg areg)
  (let* ((a-float (= (hard-regspec-class areg) hard-reg-class-fpr))
         (a-double (if a-float (= (get-regspec-mode areg) hard-reg-class-fpr-mode-double)))
         (a-node (unless a-float (= (get-regspec-mode areg) hard-reg-class-gpr-mode-node)))
         vinsn)
    (with-ppc-local-vinsn-macros (seg)
      (if a-node
        (setq vinsn (ppc2-vpop-register seg areg))
        (progn
          (setq vinsn
                (if a-float
                  (if a-double
                    (! temp-pop-double-float areg)
                    (! temp-pop-single-float areg))
                  (! temp-pop-unboxed-word areg)))
          (ppc2-close-undo)))
      vinsn)))

(defun ppc2-acc-reg-for (reg)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((class (hard-regspec-class reg))
           (mode (get-regspec-mode reg)))
      (declare (fixnum class mode))
      (cond ((= class hard-reg-class-fpr)
             (make-wired-lreg ppc::fp1 :class class :mode mode))
            ((= class hard-reg-class-gpr)
             (if (= mode hard-reg-class-gpr-mode-node)
               ($ ppc::arg_z)
               (make-wired-lreg ppc::imm0 :mode mode)))
            (t (compiler-bug "Unknown register class for reg ~s" reg))))))

;;; The compiler often generates superfluous pushes & pops.  Try to
;;; eliminate them.
;;; It's easier to elide pushes and pops to the TSP.
(defun ppc2-elide-pushes (seg push-vinsn pop-vinsn)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((pushed-reg (svref (vinsn-variable-parts push-vinsn) 0))
           (popped-reg (svref (vinsn-variable-parts pop-vinsn) 0))
           (same-reg (eq (hard-regspec-value pushed-reg)
                         (hard-regspec-value popped-reg)))
           (tsp-p (vinsn-attribute-p push-vinsn :tsp)))
      (when (and tsp-p t)                       ; vsp case is harder.
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
                             (! copy-fpr popped-reg pushed-reg)
                             (! copy-gpr popped-reg pushed-reg))))
                (remove-dll-node copy)
                (if pushed-reg-is-set
                  (insert-dll-node-after copy push-vinsn)
                  (insert-dll-node-before copy push-vinsn))))
            (elide-vinsn push-vinsn)
            (elide-vinsn pop-vinsn)))))))
                
        
;;; we never leave the first form pushed (the 68K compiler had some subprims that
;;; would vpop the first argument out of line.)
(defun ppc2-two-targeted-reg-forms (seg aform areg bform breg)
  (unless (typep areg 'lreg)
    (warn "~s is not an lreg (1/2)" areg))
  (unless (typep breg 'lreg)
    (warn "~s is not an lreg (2/2)" breg))
  (let* ((avar (ppc2-lexical-reference-p aform))
         (atriv (ppc2-trivial-p bform))
         (aconst (and (not atriv) (or (ppc-side-effect-free-form-p aform)
                                      (if avar (ppc2-var-not-set-by-form-p avar bform)))))
         (apushed (not (or atriv aconst))))
    (progn
      (unless aconst
        (if atriv
          (ppc2-one-targeted-reg-form seg aform areg)
          (setq apushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg aform (ppc2-acc-reg-for areg))))))
      (ppc2-one-targeted-reg-form seg bform breg)
      (if aconst
        (ppc2-one-targeted-reg-form seg aform areg)
        (if apushed
          (ppc2-elide-pushes seg apushed (ppc2-pop-register seg areg)))))
    (values areg breg)))


(defun ppc2-two-untargeted-reg-forms (seg aform areg bform breg)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((avar (ppc2-lexical-reference-p aform))
           (adest areg)
           (bdest breg)
           (atriv (ppc2-trivial-p bform))
           (aconst (and (not atriv) (or (ppc-side-effect-free-form-p aform)
                                        (if avar (ppc2-var-not-set-by-form-p avar bform)))))
           (apushed (not (or atriv aconst))))
      (progn
        (unless aconst
          (if atriv
            (setq adest (ppc2-one-untargeted-reg-form seg aform areg))
            (setq apushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg aform (ppc2-acc-reg-for areg))))))
        (setq bdest (ppc2-one-untargeted-reg-form seg bform breg))
        (if aconst
          (setq adest (ppc2-one-untargeted-reg-form seg aform areg))
          (if apushed
            (ppc2-elide-pushes seg apushed (ppc2-pop-register seg areg)))))
      (values adest bdest))))


(defun ppc2-four-targeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (unless (typep areg 'lreg)
    (warn "~s is not an lreg (1/4)" areg))
  (unless (typep breg 'lreg)
    (warn "~s is not an lreg (2/4)" breg))
  (unless (typep creg 'lreg)
    (warn "~s is not an lreg (3/4)" creg))
  (unless (typep dreg 'lreg)
    (warn "~s is not an lreg (4/4)" dreg))  
  (let* ((atriv (or (null aform) 
                    (and (ppc2-trivial-p bform)
                         (ppc2-trivial-p cform)
                         (ppc2-trivial-p dform))))
         (btriv (or (null bform)
                    (and (ppc2-trivial-p cform)
                         (ppc2-trivial-p dform))))
         (ctriv (or (null cform)
                    (ppc2-trivial-p dform)))
          
         (aconst (and (not atriv) 
                      (or (ppc-side-effect-free-form-p aform)
                          (let ((avar (ppc2-lexical-reference-p aform)))
                            (and avar 
                                 (ppc2-var-not-set-by-form-p avar bform)
                                 (ppc2-var-not-set-by-form-p avar cform)
                                 (ppc2-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or (ppc-side-effect-free-form-p bform)
                          (let ((bvar (ppc2-lexical-reference-p bform)))
                            (and bvar
                                 (ppc2-var-not-set-by-form-p bvar cform)
                                 (ppc2-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or (ppc-side-effect-free-form-p cform)
                          (let ((cvar (ppc2-lexical-reference-p cform)))
                            (and cvar
                                 (ppc2-var-not-set-by-form-p cvar dform))))))
         (apushed nil)
         (bpushed nil)
         (cpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (ppc2-one-targeted-reg-form seg aform areg)
        (setq apushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg aform (ppc2-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (ppc2-one-targeted-reg-form seg bform breg)
        (setq bpushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg bform (ppc2-acc-reg-for breg))))))
    (if (and cform (not cconst))
      (if ctriv
        (ppc2-one-targeted-reg-form seg cform creg)
        (setq cpushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg cform (ppc2-acc-reg-for creg))))))
    (ppc2-one-targeted-reg-form seg dform dreg)
    (unless ctriv
      (if cconst
        (ppc2-one-targeted-reg-form seg cform creg)
        (ppc2-elide-pushes seg cpushed (ppc2-pop-register seg creg))))
    (unless btriv 
      (if bconst
        (ppc2-one-targeted-reg-form seg bform breg)
        (ppc2-elide-pushes seg bpushed (ppc2-pop-register seg breg))))
    (unless atriv
      (if aconst
        (ppc2-one-targeted-reg-form seg aform areg)
        (ppc2-elide-pushes seg apushed (ppc2-pop-register seg areg))))
    (values areg breg creg dreg)))

(defun ppc2-three-targeted-reg-forms (seg aform areg bform breg cform creg)
  (unless (typep areg 'lreg)
    (warn "~s is not an lreg (1/3)" areg))
  (unless (typep breg 'lreg)
    (warn "~s is not an lreg (2/3)" breg))
  (unless (typep creg 'lreg)
    (warn "~s is not an lreg (3/3)" creg))
  (let* ((atriv (or (null aform) 
                    (and (ppc2-trivial-p bform)
                         (ppc2-trivial-p cform))))
         (btriv (or (null bform)
                    (ppc2-trivial-p cform)))
         (aconst (and (not atriv) 
                      (or (ppc-side-effect-free-form-p aform)
                          (let ((avar (ppc2-lexical-reference-p aform)))
                            (and avar 
                                 (ppc2-var-not-set-by-form-p avar bform)
                                 (ppc2-var-not-set-by-form-p avar cform))))))
         (bconst (and (not btriv)
                      (or
                       (ppc-side-effect-free-form-p bform)
                       (let ((bvar (ppc2-lexical-reference-p bform)))
                         (and bvar (ppc2-var-not-set-by-form-p bvar cform))))))
         (apushed nil)
         (bpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (ppc2-one-targeted-reg-form seg aform areg)
        (setq apushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg aform (ppc2-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (ppc2-one-targeted-reg-form seg bform breg)
        (setq bpushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg bform (ppc2-acc-reg-for breg))))))
    (ppc2-one-targeted-reg-form seg cform creg)
    (unless btriv 
      (if bconst
        (ppc2-one-targeted-reg-form seg bform breg)
        (ppc2-elide-pushes seg bpushed (ppc2-pop-register seg breg))))
    (unless atriv
      (if aconst
        (ppc2-one-targeted-reg-form seg aform areg)
        (ppc2-elide-pushes seg apushed (ppc2-pop-register seg areg))))
    (values areg breg creg)))

(defun ppc2-three-untargeted-reg-forms (seg aform areg bform breg cform creg)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((atriv (or (null aform) 
                      (and (ppc2-trivial-p bform)
                           (ppc2-trivial-p cform))))
           (btriv (or (null bform)
                      (ppc2-trivial-p cform)))
           (aconst (and (not atriv) 
                        (or (ppc-side-effect-free-form-p aform)
                            (let ((avar (ppc2-lexical-reference-p aform)))
                              (and avar 
                                   (ppc2-var-not-set-by-form-p avar bform)
                                   (ppc2-var-not-set-by-form-p avar cform))))))
           (bconst (and (not btriv)
                        (or
                         (ppc-side-effect-free-form-p bform)
                         (let ((bvar (ppc2-lexical-reference-p bform)))
                           (and bvar (ppc2-var-not-set-by-form-p bvar cform))))))
           (adest areg)
           (bdest breg)
           (cdest creg)
           (apushed nil)
           (bpushed nil))
      (if (and aform (not aconst))
        (if atriv
          (setq adest (ppc2-one-untargeted-reg-form seg aform ($ areg)))
          (setq apushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg aform (ppc2-acc-reg-for areg))))))
      (if (and bform (not bconst))
        (if btriv
          (setq bdest (ppc2-one-untargeted-reg-form seg bform ($ breg)))
          (setq bpushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg bform (ppc2-acc-reg-for breg))))))
      (setq cdest (ppc2-one-untargeted-reg-form seg cform creg))
      (unless btriv 
        (if bconst
          (setq bdest (ppc2-one-untargeted-reg-form seg bform breg))
          (ppc2-elide-pushes seg bpushed (ppc2-pop-register seg breg))))
      (unless atriv
        (if aconst
          (setq adest (ppc2-one-untargeted-reg-form seg aform areg))
          (ppc2-elide-pushes seg apushed (ppc2-pop-register seg areg))))
      (values adest bdest cdest))))

(defun ppc2-four-untargeted-reg-forms (seg aform areg bform breg cform creg dform dreg)
  (let* ((atriv (or (null aform) 
                    (and (ppc2-trivial-p bform)
                         (ppc2-trivial-p cform)
                         (ppc2-trivial-p dform))))
         (btriv (or (null bform)
                    (and (ppc2-trivial-p cform)
                         (ppc2-trivial-p dform))))
         (ctriv (or (null cform)
                    (ppc2-trivial-p dform)))
         (aconst (and (not atriv) 
                      (or (ppc-side-effect-free-form-p aform)
                          (let ((avar (ppc2-lexical-reference-p aform)))
                            (and avar 
                                 (ppc2-var-not-set-by-form-p avar bform)
                                 (ppc2-var-not-set-by-form-p avar cform)
                                 (ppc2-var-not-set-by-form-p avar dform))))))
         (bconst (and (not btriv)
                      (or
                       (ppc-side-effect-free-form-p bform)
                       (let ((bvar (ppc2-lexical-reference-p bform)))
                         (and bvar
                              (ppc2-var-not-set-by-form-p bvar cform)
                              (ppc2-var-not-set-by-form-p bvar dform))))))
         (cconst (and (not ctriv)
                      (or
                       (ppc-side-effect-free-form-p cform)
                       (let ((cvar (ppc2-lexical-reference-p cform)))
                         (and cvar
                              (ppc2-var-not-set-by-form-p cvar dform))))))
         (adest areg)
         (bdest breg)
         (cdest creg)
         (ddest dreg)
         (apushed nil)
         (bpushed nil)
         (cpushed nil))
    (if (and aform (not aconst))
      (if atriv
        (setq adest (ppc2-one-targeted-reg-form seg aform areg))
        (setq apushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg aform (ppc2-acc-reg-for areg))))))
    (if (and bform (not bconst))
      (if btriv
        (setq bdest (ppc2-one-untargeted-reg-form seg bform breg))
        (setq bpushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg bform (ppc2-acc-reg-for breg))))))
    (if (and cform (not cconst))
      (if ctriv
        (setq cdest (ppc2-one-untargeted-reg-form seg cform creg))
        (setq cpushed (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg cform (ppc2-acc-reg-for creg))))))
    (setq ddest (ppc2-one-untargeted-reg-form seg dform dreg))
    (unless ctriv 
      (if cconst
        (setq cdest (ppc2-one-untargeted-reg-form seg cform creg))
        (ppc2-elide-pushes seg cpushed (ppc2-pop-register seg creg))))
    (unless btriv 
      (if bconst
        (setq bdest (ppc2-one-untargeted-reg-form seg bform breg))
        (ppc2-elide-pushes seg bpushed (ppc2-pop-register seg breg))))
    (unless atriv
      (if aconst
        (setq adest (ppc2-one-untargeted-reg-form seg aform areg))
        (ppc2-elide-pushes seg apushed (ppc2-pop-register seg areg))))
    (values adest bdest cdest ddest)))

(defun ppc2-lri (seg reg value)
  (with-ppc-local-vinsn-macros (seg)
    (if (>= value 0)
      (! lri reg value)
      (target-arch-case
       (:ppc32 (! lri reg (logand value #xffffffff)))
       (:ppc64 (! lri reg (logand value #xffffffffffffffff)))))))


(defun ppc2-multiple-value-body (seg form)
  (let* ((lab (backend-get-next-label))
         (*ppc2-vstack* *ppc2-vstack*)
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
         (old-stack (ppc2-encode-stack)))
    (with-ppc-local-vinsn-macros (seg)
      (ppc2-open-undo $undomvexpect)
      (ppc2-undo-body seg nil (logior $backend-mvpass-mask lab) form old-stack)
      (@ lab))))

(defun ppc2-afunc-lfun-ref (afunc)
  (or
   (afunc-lfun afunc)
   (progn (pushnew afunc (afunc-fwd-refs *ppc2-cur-afunc*) :test #'eq)
          afunc)))

(defun ppc2-augment-arglist (afunc arglist &optional (maxregs $numppcargregs))
  (let ((inherited-args (afunc-inherited-vars afunc)))
    (when inherited-args
      (let* ((current-afunc *ppc2-cur-afunc*)
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



;;; There are other cases involving constants that are worth exploiting.
(defun ppc2-compare (seg vreg xfer i j cr-bit true-p)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((js16 (acode-s16-constant-p j))
           (is16 (acode-s16-constant-p i))
           (boolean (backend-crf-p vreg)))
      (if (and boolean (or js16 is16))
        (let* ((reg (ppc2-one-untargeted-reg-form seg (if js16 i j) ppc::arg_z)))
          (! compare-signed-s16const vreg reg (or js16 is16))
          (unless (or js16 (eq cr-bit ppc::ppc-eq-bit))
            (setq cr-bit (- 1 cr-bit)))
          (^ cr-bit true-p))
        (if (and (eq cr-bit ppc::ppc-eq-bit) 
                 (or js16 is16))
          (ppc2-test-reg-%izerop 
           seg 
           vreg 
           xfer 
           (ppc2-one-untargeted-reg-form 
            seg 
            (if js16 i j) 
            ppc::arg_z) 
           cr-bit 
           true-p 
           (or js16 is16))
          (multiple-value-bind (ireg jreg) (ppc2-two-untargeted-reg-forms seg i ppc::arg_y j ppc::arg_z)
            (ppc2-compare-registers seg vreg xfer ireg jreg cr-bit true-p)))))))

(defun ppc2-natural-compare (seg vreg xfer i j cr-bit true-p)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((jconstant (acode-fixnum-form-p j))
           (ju16 (typep jconstant '(unsigned-byte 16)))
           (iconstant (acode-fixnum-form-p i))
           (iu16 (typep iconstant '(unsigned-byte 16)))
           (boolean (backend-crf-p vreg)))
      (if (and boolean (or ju16 iu16))
        (with-imm-target
            () (reg :natural)
            (ppc2-one-targeted-reg-form seg (if ju16 i j) reg)
            (! compare-unsigned-u16const vreg reg (if ju16 jconstant iconstant))
            (unless (or ju16 (eq cr-bit ppc::ppc-eq-bit)) 
              (setq cr-bit (- 1 cr-bit)))
            (^ cr-bit true-p))
        (with-imm-target ()
          (ireg :natural)
            (with-imm-target 
                (ireg) (jreg :natural)
                (ppc2-two-targeted-reg-forms seg i ireg j jreg)
                (ppc2-compare-natural-registers seg vreg xfer ireg jreg cr-bit true-p)))))))

(defun ppc2-compare-natural-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case 
       (vreg dest)
       (progn
         (! compare-logical dest ireg jreg)
         (^ cr-bit true-p))
       (with-imm-temps () ((b31-reg :natural))
         (ecase cr-bit
           (#. ppc::ppc-eq-bit 
            (if true-p
              (! eq->bit31 b31-reg ireg jreg)
              (! ne->bit31 b31-reg ireg jreg)))
           (#. ppc::ppc-lt-bit
            (if true-p
              (! ltu->bit31 b31-reg ireg jreg)
              (! geu->bit31 b31-reg ireg jreg)))
           (#. ppc::ppc-gt-bit
            (if true-p
              (! gtu->bit31 b31-reg ireg jreg)
              (! leu->bit31 b31-reg ireg jreg))))
         (ensuring-node-target (target dest)
           (! lowbit->truth target b31-reg))
         (^)))
      (^))))

(defun ppc2-compare-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case 
       (vreg dest)
       (progn
         (! compare dest ireg jreg)
         (^ cr-bit true-p))
       (with-imm-temps () ((b31-reg :natural))
         (ecase cr-bit
           (#. ppc::ppc-eq-bit 
            (if true-p
              (! eq->bit31 b31-reg ireg jreg)
              (! ne->bit31 b31-reg ireg jreg)))
           (#. ppc::ppc-lt-bit
            (if true-p
              (! lt->bit31 b31-reg ireg jreg)
              (! ge->bit31 b31-reg ireg jreg)))
           (#. ppc::ppc-gt-bit
            (if true-p
              (! gt->bit31 b31-reg ireg jreg)
              (! le->bit31 b31-reg ireg jreg))))
         (ensuring-node-target (target dest)
           (! lowbit->truth target b31-reg))
         (^)))
      (^))))

(defun ppc2-compare-register-to-nil (seg vreg xfer ireg cr-bit true-p)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case 
       (vreg dest)
       (progn
         (! compare-to-nil dest ireg)
         (^ cr-bit true-p))
       (with-imm-temps () ((b31-reg :natural))
         (ecase cr-bit
           (#. ppc::ppc-eq-bit 
            (if true-p
              (! eqnil->bit31 b31-reg ireg)
              (! nenil->bit31 b31-reg ireg))))
         (ensuring-node-target (target dest)
           (! lowbit->truth target b31-reg))
         (^)))
      (^))))

;;; Have to extract a bit out of the CR when a boolean result needed.
(defun ppc2-compare-double-float-registers (seg vreg xfer ireg jreg cr-bit true-p)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if vreg
      (regspec-crf-gpr-case 
       (vreg dest)
       (progn
         (! double-float-compare dest ireg jreg)
         (^ cr-bit true-p))
       (with-imm-temps () ((lowbit-reg :natural))
         (with-crf-target () flags
           (! double-float-compare flags ireg jreg)
           (! crbit->bit31 lowbit-reg flags cr-bit))
         (unless true-p
           (! invert-lowbit lowbit-reg))
         (ensuring-node-target (target dest)
           (! lowbit->truth target lowbit-reg))
         (^)))
      (^))))


(defun ppc2-immediate-form-p (form)
  (if (and (consp form)
           (or (eq (%car form) (%nx1-operator immediate))
               (eq (%car form) (%nx1-operator simple-function))))
    t))

(defun ppc2-test-%izerop (seg vreg xfer form cr-bit true-p)
  (ppc2-test-reg-%izerop seg vreg xfer (ppc2-one-untargeted-reg-form seg form ppc::arg_z) cr-bit true-p 0))

(defun ppc2-test-reg-%izerop (seg vreg xfer reg cr-bit true-p  zero)
  (declare (fixnum reg zero))
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (regspec-crf-gpr-case 
     (vreg dest)
     (progn
       (! compare-signed-s16const dest reg zero)
       (^ cr-bit true-p))
     (with-imm-temps (reg) (b31-reg scaled)
       (if (zerop zero)
         (setq scaled reg)
         (! subtract-constant scaled reg zero))
       (ecase cr-bit
         (#. ppc::ppc-eq-bit 
          (if true-p
            (! eq0->bit31 b31-reg scaled)
            (! ne0->bit31 b31-reg scaled)))
         (#. ppc::ppc-lt-bit
          (if true-p
            (! lt0->bit31 b31-reg scaled)
            (! ge0->bit31 b31-reg scaled)))
         (#. ppc::ppc-gt-bit
          (if true-p
            (! gt0->bit31 b31-reg scaled)
            (! le0->bit31 b31-reg scaled))))
          (ensuring-node-target (target dest)
            (! lowbit->truth target b31-reg))
       (^)))))

(defun ppc2-lexical-reference-ea (form &optional (no-closed-p t))
  (when (acode-p (setq form (acode-unwrapped-form form)))
    (if (eq (acode-operator form) (%nx1-operator lexical-reference))
      (let* ((addr (var-ea (%cadr form))))
        (if (typep addr 'lreg)
          addr
          (unless (and no-closed-p (addrspec-vcell-p addr ))
            addr))))))


(defun ppc2-vpush-register (seg src &optional why info attr)
  (with-ppc-local-vinsn-macros (seg)
    (prog1
      (! vpush-register src)
      (ppc2-new-vstack-lcell (or why :node) *ppc2-target-lcell-size* (or attr 0) info)
      (ppc2-adjust-vstack *ppc2-target-node-size*))))

(defun ppc2-vpush-register-arg (seg src)
  (ppc2-vpush-register seg src :outgoing-argument))


(defun ppc2-vpop-register (seg dest)
  (with-ppc-local-vinsn-macros (seg)
    (prog1
      (! vpop-register dest)
      (setq *ppc2-top-vstack-lcell* (lcell-parent *ppc2-top-vstack-lcell*))
      (ppc2-adjust-vstack (- *ppc2-target-node-size*)))))

(defun ppc2-copy-register (seg dest src)
  (with-ppc-local-vinsn-macros (seg)
    (when dest
      (let* ((dest-gpr (backend-ea-physical-reg dest hard-reg-class-gpr))
             (src-gpr (if src (backend-ea-physical-reg src hard-reg-class-gpr)))
             (dest-fpr (backend-ea-physical-reg dest hard-reg-class-fpr))
             (src-fpr (if src (backend-ea-physical-reg src hard-reg-class-fpr)))
             (src-mode (if src (get-regspec-mode src)))
             (dest-mode (get-regspec-mode dest))
             (dest-crf (backend-ea-physical-reg dest hard-reg-class-crf)))
        (if (and dest-gpr (eql dest-gpr ppc::rzero))
          (compiler-bug "Bad destination register: ~s" dest-gpr))
        (if (null src)
          (if dest-gpr
            (! load-nil dest-gpr)
            (if dest-crf
              (! set-eq-bit dest-crf)))
          (if (and dest-crf src-gpr)
            ;; "Copying" a GPR to a CR field means comparing it to rnil
            (! compare-to-nil dest src)
            (if (and dest-gpr src-gpr)
              (if (eql src-gpr ppc::rzero)        
                ;; Rzero always contains 0, so we can
                ;; save ourselves some trouble.
                ;; This assumes that (LI dest-gpr 0) is easier
                ;; on the register-renaming pipeline nonsense than
                ;; (MR dest-gpr rzero) would be.
                (! lri dest-gpr 0)
                ;; This is the "GPR <- GPR" case.  There are
                ;; word-size dependencies, but there's also
                ;; lots of redundancy here.
                (target-word-size-case
                 (32
                  (case dest-mode
                    (#.hard-reg-class-gpr-mode-node ; boxed result.
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       (#.hard-reg-class-gpr-mode-u32
                        (ppc2-box-u32 seg dest src))
                       (#.hard-reg-class-gpr-mode-s32
                        (ppc2-box-s32 seg dest src))
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
                                         *ppc2-reckless*)
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
                        (if *ppc2-reckless*
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
                 (64
                  (case dest-mode
                    (#.hard-reg-class-gpr-mode-node ; boxed result.
                     (case src-mode
                       (#.hard-reg-class-gpr-mode-node
                        (unless (eql  dest-gpr src-gpr)
                          (! copy-gpr dest src)))
                       (#.hard-reg-class-gpr-mode-u64
                        (ppc2-box-u64 seg dest src))
                       (#.hard-reg-class-gpr-mode-s64
                        (ppc2-box-s64 seg dest src))
                       (#.hard-reg-class-gpr-mode-u32
                        (ppc2-box-u32 seg dest src))
                       (#.hard-reg-class-gpr-mode-s32
                        (ppc2-box-s32 seg dest src))
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
                                         *ppc2-reckless*)
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
                        (! unbox-u32 dest src))
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
                        (if *ppc2-reckless*
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
                                      *ppc2-reckless*)
                            (! trap-unless-double-float src))
                          (! get-double dest src))
                         (#.hard-reg-class-fpr-mode-single
                          (unless *ppc2-reckless*
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
                      (! copy-fpr dest src))))))))))))
  
(defun ppc2-unreachable-store (&optional vreg)
  ;; I don't think that anything needs to be done here,
  ;; but leave this guy around until we're sure.
  ;; (PPC2-VPUSH-REGISTER will always vpush something, even
  ;; if code to -load- that "something" never gets generated.
  ;; If I'm right about this, that means that the compile-time
  ;; stack-discipline problem that this is supposed to deal
  ;; with can't happen.)
  (declare (ignore vreg))
  nil)

;;; bind vars to initforms, as per let*, &aux.
(defun ppc2-seq-bind (seg vars initforms)
  (dolist (var vars)
    (ppc2-seq-bind-var seg var (pop initforms))))

(defun ppc2-dynamic-extent-form (seg curstack val)
  (when (acode-p val)
    (with-ppc-local-vinsn-macros (seg)
      (let* ((op (acode-operator val)))
        (cond ((eq op (%nx1-operator list))
               (let* ((*ppc2-vstack* *ppc2-vstack*)
                      (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
                 (ppc2-set-nargs seg (ppc2-formlist seg (%cadr val) nil))
                 (ppc2-open-undo $undostkblk curstack)
                 (! stack-cons-list))
               (setq val ppc::arg_z))
              ((eq op (%nx1-operator list*))
               (let* ((arglist (%cadr val)))                   
                 (let* ((*ppc2-vstack* *ppc2-vstack*)
                        (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
                   (ppc2-arglist seg arglist))
                 (when (car arglist)
                   (ppc2-set-nargs seg (length (%car arglist)))
                   (! stack-cons-list*)
                   (ppc2-open-undo $undostkblk curstack))
                 (setq val ppc::arg_z)))
              ((eq op (%nx1-operator multiple-value-list))
               (ppc2-multiple-value-body seg (%cadr val))
               (ppc2-open-undo $undostkblk curstack)
               (! stack-cons-list)
               (setq val ppc::arg_z))
              ((eq op (%nx1-operator cons))
               (let* ((y ($ ppc::arg_y))
                      (z ($ ppc::arg_z))
                      (result ($ ppc::arg_z)))
                 (ppc2-two-targeted-reg-forms seg (%cadr val) y (%caddr val) z)
                 (ppc2-open-undo $undostkblk )
                 (! make-tsp-cons result y z) 
                 (setq val result)))
              ((eq op (%nx1-operator %consmacptr%))
               (with-imm-target () (address :address)
                 (ppc2-one-targeted-reg-form seg val address)
                 (with-node-temps () (node)
                   (! macptr->stack node address)
                   (ppc2-open-undo $undostkblk)
                   (setq val node))))
              ((eq op (%nx1-operator %new-ptr))
               (let ((clear-form (caddr val)))
                 (if (nx-constant-form-p clear-form)
                   (progn 
                     (ppc2-one-targeted-reg-form seg (%cadr val) ($ ppc::arg_z))
                     (if (nx-null clear-form)
                       (! make-stack-block)
                       (! make-stack-block0)))
                   (with-crf-target () crf
                     (let ((stack-block-0-label (backend-get-next-label))
                           (done-label (backend-get-next-label))
                           (rval ($ ppc::arg_z))
                           (rclear ($ ppc::arg_y)))
                       (ppc2-two-targeted-reg-forms seg (%cadr val) rval clear-form rclear)
                       (! compare-to-nil crf rclear)
                       (! cbranch-false (aref *backend-labels* stack-block-0-label) crf ppc::ppc-eq-bit)
                       (! make-stack-block)
                       (-> done-label)
                       (@ stack-block-0-label)
                       (! make-stack-block0)
                       (@ done-label)))))
               (ppc2-open-undo $undostkblk)
               (setq val ($ ppc::arg_z)))
              ((eq op (%nx1-operator make-list))
               (ppc2-two-targeted-reg-forms seg (%cadr val) ($ ppc::arg_y) (%caddr val) ($ ppc::arg_z))
               (ppc2-open-undo $undostkblk curstack)
               (! make-stack-list)
               (setq val ppc::arg_z))       
              ((eq (%car val) (%nx1-operator vector))
               (let* ((*ppc2-vstack* *ppc2-vstack*)
                      (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
                 (ppc2-set-nargs seg (ppc2-formlist seg (%cadr val) nil))
                 (! make-stack-vector))
               (ppc2-open-undo $undostkblk)
               (setq val ppc::arg_z))
              ((eq op (%nx1-operator %gvector))
               (let* ((*ppc2-vstack* *ppc2-vstack*)
                      (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
                      (arglist (%cadr val)))
                 (ppc2-set-nargs seg (ppc2-formlist seg (append (car arglist) (reverse (cadr arglist))) nil))
                 (! make-stack-gvector))
               (ppc2-open-undo $undostkblk)
               (setq val ppc::arg_z)) 
              ((eq op (%nx1-operator closed-function)) 
               (setq val (ppc2-make-closure seg (cadr val) t))) ; can't error
              ((eq op (%nx1-operator %make-uvector))
               (destructuring-bind (element-count subtag &optional (init 0 init-p)) (%cdr val)
                 (if init-p
                   (progn
                     (ppc2-three-targeted-reg-forms seg element-count ($ ppc::arg_x) subtag ($ ppc::arg_y) init ($ ppc::arg_z))
                     (! stack-misc-alloc-init))
                   (progn
                     (ppc2-two-targeted-reg-forms seg element-count ($ ppc::arg_y)  subtag ($ ppc::arg_z))
                     (! stack-misc-alloc)))
                 (ppc2-open-undo $undostkblk)
                 (setq val ($ ppc::arg_z))))))))
  val)

(defun ppc2-addrspec-to-reg (seg addrspec reg)
  (if (memory-spec-p addrspec)
    (ppc2-stack-to-register seg addrspec reg)
    (ppc2-copy-register seg reg addrspec)))
  
(defun ppc2-seq-bind-var (seg var val)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((sym (var-name var))
           (bits (nx-var-bits var))
           (closed-p (and (%ilogbitp $vbitclosed bits)
                          (%ilogbitp $vbitsetq bits)))
           (curstack (ppc2-encode-stack))
           (make-vcell (and closed-p (eq bits (var-bits var))))
           (closed-downward (and closed-p (%ilogbitp $vbitcloseddownward bits))))
      (unless (fixnump val)
        (setq val (nx-untyped-form val))
        (when (and (%ilogbitp $vbitdynamicextent bits) (acode-p val))
          (setq val (ppc2-dynamic-extent-form seg curstack val))))
      (if (%ilogbitp $vbitspecial bits)
        (progn
          (ppc2-dbind seg val sym)
          (ppc2-set-var-ea seg var (ppc2-vloc-ea (- *ppc2-vstack* *ppc2-target-node-size*))))
        (let ((puntval nil))
          (flet ((ppc2-puntable-binding-p (var initform)
                   ; The value returned is acode.
                   (let* ((bits (nx-var-bits var)))
                     (if (%ilogbitp $vbitpuntable bits)
                       (nx-untyped-form initform)))))
            (declare (inline ppc2-puntable-binding-p))
            (if (and (not (ppc2-load-ea-p val))
                     (setq puntval (ppc2-puntable-binding-p var val)))
              (progn
                (nx-set-var-bits var (%ilogior (%ilsl $vbitpunted 1) bits))
                (ppc2-set-var-ea seg var puntval))
              (progn
                (let* ((vloc *ppc2-vstack*)
                       (reg (let* ((r (ppc2-assign-register-var var)))
                              (if r ($ r)))))
                  (if (ppc2-load-ea-p val)
                    (if reg
                      (ppc2-addrspec-to-reg seg val reg)
                      (if (memory-spec-p val)
                        (with-node-temps () (temp)
                          (ppc2-addrspec-to-reg seg val temp)
                          (ppc2-vpush-register seg temp :node var bits))
                        (ppc2-vpush-register seg val :node var bits)))
                    (if reg
                      (ppc2-one-targeted-reg-form seg val reg)
                      (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg val ppc::arg_z) :node var bits)))
                  (ppc2-set-var-ea seg var (or reg (ppc2-vloc-ea vloc closed-p)))
                  (if reg
                    (ppc2-note-var-cell var reg)
                    (ppc2-note-top-cell var))
                  (when make-vcell
                    (with-node-temps () (vcell closed)
                        (ppc2-stack-to-register seg vloc closed)
                        (if closed-downward
                          (progn
                            (! make-tsp-vcell vcell closed)
                            (ppc2-open-undo $undostkblk))
                          (! make-vcell vcell closed))
                        (ppc2-register-to-stack seg vcell vloc))))))))))))



;;; Never make a vcell if this is an inherited var.
;;; If the var's inherited, its bits won't be a fixnum (and will
;;; therefore be different from what NX-VAR-BITS returns.)
(defun ppc2-bind-var (seg var vloc &optional lcell &aux 
                          (bits (nx-var-bits var)) 
                          (closed-p (and (%ilogbitp $vbitclosed bits) (%ilogbitp $vbitsetq bits)))
                          (closed-downward (if closed-p (%ilogbitp $vbitcloseddownward bits)))
                          (make-vcell (and closed-p (eq bits (var-bits var))))
                          (addr (ppc2-vloc-ea vloc)))
  (with-ppc-local-vinsn-macros (seg)
    (if (%ilogbitp $vbitspecial bits)
      (progn
        (ppc2-dbind seg addr (var-name var))
        (ppc2-set-var-ea seg var (ppc2-vloc-ea (- *ppc2-vstack* *ppc2-target-node-size*)))
        t)
      (progn
        (when (%ilogbitp $vbitpunted bits)
          (compiler-bug "bind-var: var ~s was punted" var))
        (when make-vcell
          (with-node-temps () (vcell closed)
            (ppc2-stack-to-register seg vloc closed)
            (if closed-downward
              (progn
                (! make-tsp-vcell vcell closed)
                (ppc2-open-undo $undostkblk))
              (! make-vcell vcell closed))
            (ppc2-register-to-stack seg vcell vloc)))
        (when lcell
          (setf (lcell-kind lcell) :node
                (lcell-attributes lcell) bits
                (lcell-info lcell) var)
          (ppc2-note-var-cell var lcell))          
        (ppc2-set-var-ea seg var (ppc2-vloc-ea vloc closed-p))        
        closed-downward))))

(defun ppc2-set-var-ea (seg var ea)
  (setf (var-ea var) ea)
  (when (and *ppc2-record-symbols* (or (typep ea 'lreg) (typep ea 'fixnum)))
    (let* ((start (ppc2-emit-note seg :begin-variable-scope)))
      (push (list var (var-name var) start (close-vinsn-note start))
            *ppc2-recorded-symbols*)))
  ea)

(defun ppc2-close-var (seg var)
  (let ((bits (nx-var-bits var)))
    (when (and *ppc2-record-symbols*
               (or (logbitp $vbitspecial bits)
                   (not (logbitp $vbitpunted bits))))
      (let ((endnote (%car (%cdddr (assq var *ppc2-recorded-symbols*)))))
        (unless endnote (compiler-bug "ppc2-close-var for ~s ?" (var-name var)))
        (setf (vinsn-note-class endnote) :end-variable-scope)
        (append-dll-node (vinsn-note-label endnote) seg)))))

(defun ppc2-load-ea-p (ea)
  (or (typep ea 'fixnum)
      (typep ea 'lreg)
      (typep ea 'lcell)))

(defun ppc2-dbind (seg value sym)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((ea-p (ppc2-load-ea-p value))
           (nil-p (unless ea-p (eq (setq value (nx-untyped-form value)) *nx-nil*)))
           (self-p (unless ea-p (and (or
                                      (eq (acode-operator value) (%nx1-operator bound-special-ref))
                                      (eq (acode-operator value) (%nx1-operator special-ref)))
                                     (eq (cadr value) sym)))))
      (cond ((eq sym '*interrupt-level*)
             (let* ((fixval (acode-fixnum-form-p value)))
               (cond ((eql fixval 0) (if *ppc2-open-code-inline*
                                       (! bind-interrupt-level-0-inline)
                                       (! bind-interrupt-level-0)))
                     ((eql fixval -1) (if *ppc2-open-code-inline*
                                        (! bind-interrupt-level-m1-inline)
                                        (! bind-interrupt-level-m1)))
                     (t
                      (if ea-p 
                        (ppc2-store-ea seg value ppc::arg_z)
                        (ppc2-one-targeted-reg-form seg value ($ ppc::arg_z)))
                      (! bind-interrupt-level))))
             (ppc2-open-undo $undointerruptlevel))
            (t
             (if (or nil-p self-p)
               (progn
                 (ppc2-store-immediate seg (ppc2-symbol-value-cell sym) ppc::arg_z)
                 (if nil-p
                   (! bind-nil)
                   (if (or *ppc2-reckless* (eq (acode-operator value) (%nx1-operator special-ref)))
                     (! bind-self)
                     (! bind-self-boundp-check))))
               (progn
                 (if ea-p 
                   (ppc2-store-ea seg value ppc::arg_z)
                   (ppc2-one-targeted-reg-form seg value ($ ppc::arg_z)))
                 (ppc2-store-immediate seg (ppc2-symbol-value-cell sym) ($ ppc::arg_y))
                 (! bind)))
             (ppc2-open-undo $undospecial)))
      (ppc2-new-vstack-lcell :special-value *ppc2-target-lcell-size* 0 sym)
      (ppc2-new-vstack-lcell :special *ppc2-target-lcell-size* (ash 1 $vbitspecial) sym)
      (ppc2-new-vstack-lcell :special-link *ppc2-target-lcell-size* 0 sym)
      (ppc2-adjust-vstack (* 3 *ppc2-target-node-size*)))))

;;; Store the contents of EA - which denotes either a vframe location
;;; or a hard register - in reg.

(defun ppc2-store-ea (seg ea reg)
  (if (typep ea 'fixnum)
    (if (memory-spec-p ea)
      (ppc2-stack-to-register seg ea reg)
      (ppc2-copy-register seg reg ea))
    (if (typep ea 'lreg)
      (ppc2-copy-register seg reg ea)
      (if (typep ea 'lcell)
        (ppc2-lcell-to-register seg ea reg)))))


      

;;; Callers should really be sure that this is what they want to use.
(defun ppc2-absolute-natural (seg vreg xfer value)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (ppc2-lri seg vreg value))
    (^)))



(defun ppc2-store-macptr (seg vreg address-reg)
  (with-ppc-local-vinsn-macros (seg vreg)
    (when (ppc2-for-value-p vreg)
      (if (logbitp vreg ppc-imm-regs)
        (<- address-reg)
        (! macptr->heap vreg address-reg)))))

(defun ppc2-store-signed-longword (seg vreg imm-reg)
  (with-ppc-local-vinsn-macros (seg vreg)
    (when (ppc2-for-value-p vreg)
      (if (logbitp vreg ppc-imm-regs)
        (<- imm-reg)
        (ppc2-box-s32 seg vreg imm-reg)))))

(defun ppc2-store-signed-halfword (seg vreg imm-reg)
  (with-ppc-local-vinsn-macros (seg vreg)
    (when (ppc2-for-value-p vreg)
      (if (logbitp vreg ppc-imm-regs)
        (<- imm-reg)
        (! s16->fixnum vreg imm-reg)))))


(defun ppc2-store-unsigned-halfword (seg vreg imm-reg)
  (with-ppc-local-vinsn-macros (seg vreg)
    (when (ppc2-for-value-p vreg)
      (if (logbitp vreg ppc-imm-regs)
        (<- imm-reg)
        (! u16->fixnum vreg imm-reg)))))



;;; If "value-first-p" is true and both "offset" and "val" need to be 
;;; evaluated, evaluate "val" before evaluating "offset".
(defun ppc2-%immediate-set-ptr (seg vreg xfer  ptr offset val)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((intval (acode-absolute-ptr-p val))
           (offval (acode-fixnum-form-p offset))
           (absptr (and offval (acode-absolute-ptr-p ptr)))
           (for-value (ppc2-for-value-p vreg)))
      (flet ((address-and-node-regs ()
               (if for-value
                 (progn
                   (ppc2-one-targeted-reg-form seg val ($ ppc::arg_z))
                   (if (eq intval 0)
                     (values ppc::rzero ppc::arg_z)
                     (progn
                       (if intval
                         (ppc2-lri seg ppc::imm0 intval)
                         (! deref-macptr ppc::imm0 ppc::arg_z))
                       (values ppc::imm0 ppc::arg_z))))
                 (if (eq intval 0)
                   (values ppc::rzero nil)
                   (values (ppc2-macptr-arg-to-reg seg val ($ ppc::imm0 :mode :address)) nil)))))
        (if (and absptr offval)
          (setq absptr (+ absptr offval) offval 0)
          (setq absptr nil))
        (and offval (%i> (integer-length offval) 15) (setq offval nil))
        (and absptr (%i> (integer-length absptr) 15) (setq absptr nil))
        (target-arch-case
         (:ppc32 (progn))
         (:ppc64 (progn
                   (and offval (logtest 3 offval) (setq offval nil))
                   (and absptr (logtest 3 absptr) (setq absptr nil)))))
        (if absptr
          (multiple-value-bind (address node) (address-and-node-regs)
            (! mem-set-c-address address ppc::rzero absptr)
            (if for-value
              (<- node)))
          ; No absolute ptr (which is presumably a rare case anyway.)
          (if offval
            ; Easier: need one less register than in the general case.
            (with-imm-target () (ptr-reg :address)
              (ppc2-one-targeted-reg-form seg ptr ptr-reg)
              (if intval
                (with-imm-target (ptr-reg) (val-target :address)
                  (if (eql intval 0)
                    (setq val-target ppc::rzero)
                    (ppc2-lri seg val-target intval))
                  (! mem-set-c-address val-target ptr-reg offval)
                  (if for-value
                    (<- (set-regspec-mode val-target (gpr-mode-name-value :address)))))
                (progn
                  (! temp-push-unboxed-word ptr-reg)
                  (ppc2-open-undo $undostkblk)
                  (multiple-value-bind (address node) (address-and-node-regs)
                    (with-imm-target (address) (ptr-reg :address)
                      (! temp-pop-unboxed-word ptr-reg)
                      (ppc2-close-undo)
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
                    (ppc2-one-targeted-reg-form seg ptr ptr-reg)
                    (with-imm-target (ptr-reg) (off-reg :signed-natural)
                      (ppc2-lri seg off-reg constant-offset)
                      (with-imm-target (ptr-reg off-reg) (val-reg :address)
                        (if (eql intval 0)
                          (setq val-reg ppc::rzero)
                          (ppc2-lri seg val-reg intval))
                        (setq xptr-reg ptr-reg
                              xoff-reg off-reg
                              xval-reg val-reg))))
                  ; Offset's non-constant.  Temp-push the pointer, evaluate
                  ; and unbox the offset, load the value, pop the pointer.
                  (progn
                    (with-imm-target () (ptr-reg :address)
                      (ppc2-one-targeted-reg-form seg ptr ptr-reg)
                      (! temp-push-unboxed-word ptr-reg)
                      (ppc2-open-undo $undostkblk))
                    (with-imm-target () (off-reg :signed-natural)
                      (! fixnum->signed-natural off-reg (ppc2-one-targeted-reg-form seg offset ($ ppc::arg_z)))
                      (with-imm-target (off-reg) (val-reg :signed-natural)
                        (if (eql intval 0)
                          (setq val-reg ppc::rzero)
                          (ppc2-lri seg val-reg intval))
                        (with-imm-target (off-reg val-reg) (ptr-reg :address)
                          (! temp-pop-unboxed-word ptr-reg)
                          (ppc2-close-undo)
                          (setq xptr-reg ptr-reg
                                xoff-reg off-reg
                                xval-reg val-reg))))))
                ;; No intval; maybe constant-offset.
                (with-imm-target () (ptr-reg :address)
                  (ppc2-one-targeted-reg-form seg ptr ptr-reg)
                  (! temp-push-unboxed-word ptr-reg)
                  (ppc2-open-undo $undostkblk)
                  (progn
                    (if (not constant-offset)
                      (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z)))
                    (multiple-value-bind (address node) (address-and-node-regs)
                      (with-imm-target (address) (off-reg :s32)
                                       (if constant-offset
                                         (ppc2-lri seg off-reg constant-offset)
                                         (with-node-temps (ppc::arg_z) (temp)
                                           (ppc2-vpop-register seg temp)
                                           (! fixnum->signed-natural off-reg temp)))
                                       (with-imm-target (ppc::imm0 off-reg) (ptr-reg :address)
                                                        (! temp-pop-unboxed-word ptr-reg)
                                                        (ppc2-close-undo)
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
                       (gpr-mode-name-value :address))))))))
        (^)))))
  
(defun ppc2-memory-store-displaced (seg valreg basereg displacement size)
  (with-ppc-local-vinsn-macros (seg)
    (case size
      (8 (! mem-set-c-doubleword valreg basereg displacement))
      (4 (! mem-set-c-fullword valreg basereg displacement))
      (2 (! mem-set-c-halfword valreg basereg displacement))
      (1 (! mem-set-c-byte valreg basereg displacement)))))

(defun ppc2-memory-store-indexed (seg valreg basereg idxreg size)
  (with-ppc-local-vinsn-macros (seg)
    (case size
      (8 (! mem-set-doubleword valreg basereg idxreg))
      (4 (! mem-set-fullword valreg basereg idxreg))
      (2 (! mem-set-halfword valreg basereg idxreg))
      (1 (! mem-set-byte valreg basereg idxreg)))))
      
(defun ppc2-%immediate-store  (seg vreg xfer bits ptr offset val)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if (eql 0 (%ilogand #xf bits))
      (ppc2-%immediate-set-ptr seg vreg xfer  ptr offset val)
      (let* ((size (logand #xf bits))
             (nbits (ash size 3))
             (signed (not (logbitp 5 bits)))
             (intval (acode-integer-constant-p val nbits))
             (offval (acode-fixnum-form-p offset))
             (absptr (and offval (acode-absolute-ptr-p ptr)))
             (for-value (ppc2-for-value-p vreg)))
        (declare (fixnum size))
        (flet ((val-to-argz-and-imm0 ()
                 (ppc2-one-targeted-reg-form seg val ($ ppc::arg_z))
                 (if (eq size 8)
                   (if signed
                     (! gets64)
                     (! getu64))
                   (if (and (eq size 4)
                            (target-arch-case
                             (:ppc32 t)
                             (:ppc64 nil)))
                     (if signed
                       (! gets32)
                       (! getu32))
                     (! fixnum->signed-natural ppc::imm0 ppc::arg_z)))))
          (if (and absptr offval)
            (setq absptr (+ absptr offval) offval 0)
            (setq absptr nil))
          (and offval (%i> (integer-length offval) 15) (setq offval nil))
          (and absptr (%i> (integer-length absptr) 15) (setq absptr nil))
          (target-arch-case
           (:ppc32 (progn))
           (:ppc64 (when (eql size 8)
                     (and offval (logtest 3 offval) (setq offval nil))
                     (and absptr (logtest 3 absptr) (setq absptr nil)))))
          (if absptr
            (if intval
              (with-imm-target () (val-target :s32)
                (if (eql intval 0)
                  (setq val-target ppc::rzero)
                  (ppc2-lri seg val-target intval))
                (ppc2-memory-store-displaced seg val-target ppc::rzero absptr size)
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
                (val-to-argz-and-imm0)
                (ppc2-memory-store-displaced seg ppc::imm0 ppc::rzero absptr size)
                (<- ppc::arg_z)))
            ; No absolute ptr (which is presumably a rare case anyway.)
            (if offval
              ; Easier: need one less register than in the general case.
              (with-imm-target () (ptr-reg :address)
                (ppc2-one-targeted-reg-form seg ptr ptr-reg)
                (if intval
                  (with-imm-target (ptr-reg) (val-target :s32)                    
                    (if (eql intval 0)
                      (setq val-target ppc::rzero)
                      (ppc2-lri seg val-target intval))
                    (ppc2-memory-store-displaced seg val-target ptr-reg offval size)
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
                    (ppc2-open-undo $undostkblk)
                    (val-to-argz-and-imm0)                  
                    (with-imm-target (ppc::imm0) (ptr-reg :address)
                      (! temp-pop-unboxed-word ptr-reg)
                      (ppc2-close-undo)
                      (ppc2-memory-store-displaced seg ppc::imm0 ptr-reg offval size)                    
                      (if for-value
                        (<- ppc::arg_z))))))
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
                      (ppc2-one-targeted-reg-form seg ptr ptr-reg)
                      (with-imm-target (ptr-reg) (off-reg :s32)
                        (ppc2-lri seg off-reg constant-offset)
                        (with-imm-target (ptr-reg off-reg) (val-reg :s32)
                          (if (eql intval 0)
                            (setq val-reg ppc::rzero)
                            (ppc2-lri seg val-reg intval))
                          (setq xptr-reg ptr-reg
                                xoff-reg off-reg
                                xval-reg val-reg))))
                    ; Offset's non-constant.  Temp-push the pointer, evaluate
                    ; and unbox the offset, load the value, pop the pointer.
                    (progn
                      (with-imm-target () (ptr-reg :address)
                        (ppc2-one-targeted-reg-form seg ptr ptr-reg)
                        (! temp-push-unboxed-word ptr-reg)
                        (ppc2-open-undo $undostkblk))
                      (with-imm-target () (off-reg :s32)
                        (! fixnum->signed-natural off-reg (ppc2-one-targeted-reg-form seg offset ($ ppc::arg_z)))
                        (with-imm-target (off-reg) (val-reg :s32)
                          (if (eql intval 0)
                            (setq val-reg ppc::rzero)
                            (ppc2-lri seg val-reg intval))
                          (with-imm-target (off-reg val-reg) (ptr-reg :address)
                            (! temp-pop-unboxed-word ptr-reg)
                            (ppc2-close-undo)
                            (setq xptr-reg ptr-reg
                                  xoff-reg off-reg
                                  xval-reg val-reg))))))
                  ;; No intval; maybe constant-offset.
                  (with-imm-target () (ptr-reg :address)
                    (ppc2-one-targeted-reg-form seg ptr ptr-reg)
                    (! temp-push-unboxed-word ptr-reg)
                    (ppc2-open-undo $undostkblk)
                    (progn
                        (if (not constant-offset)
                          (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z)))
                        (val-to-argz-and-imm0)
                        (with-imm-target (ppc::imm0) (off-reg :signed-natural)
                          (if constant-offset
                            (ppc2-lri seg off-reg constant-offset)
                            (with-node-temps (ppc::arg_z) (temp)
                              (ppc2-vpop-register seg temp)
                              (! fixnum->signed-natural off-reg temp)))
                          (with-imm-target (ppc::imm0 off-reg) (ptr-reg :address)
                            (! temp-pop-unboxed-word ptr-reg)
                            (ppc2-close-undo)
                            (setq xptr-reg ptr-reg
                                  xoff-reg off-reg
                                  xval-reg ppc::imm0
                                  node-arg_z t))))))
                (ppc2-memory-store-indexed seg xval-reg xptr-reg xoff-reg size)
                (when for-value
                  (if node-arg_z
                    (<- ppc::arg_z)
                    (<- (set-regspec-mode 
                         xval-reg
                         (gpr-mode-name-value
                          (case size
                            (8 (if signed :s64 :u64))
                            (4 (if signed :s32 :u32))
                            (2 (if signed :s16 :u16))
                            (1 (if signed :s8 :u8)))))))))))
          (^))))))





(defun ppc2-encoding-undo-count (encoding)
 (svref encoding 0))

(defun ppc2-encoding-cstack-depth (encoding)    ; hardly ever interesting
  (svref encoding 1))

(defun ppc2-encoding-vstack-depth (encoding)
  (svref encoding 2))

(defun ppc2-encoding-vstack-top (encoding)
  (svref encoding 3))

(defun ppc2-encode-stack ()
  (vector *ppc2-undo-count* *ppc2-cstack* *ppc2-vstack* *ppc2-top-vstack-lcell*))

(defun ppc2-decode-stack (encoding)
  (values (ppc2-encoding-undo-count encoding)
          (ppc2-encoding-cstack-depth encoding)
          (ppc2-encoding-vstack-depth encoding)
          (ppc2-encoding-vstack-top encoding)))

(defun ppc2-equal-encodings-p (a b)
  (dotimes (i 3 t)
    (unless (eq (svref a i) (svref b i)) (return))))

(defun ppc2-open-undo (&optional (reason $undocatch) (curstack (ppc2-encode-stack)))
  (set-fill-pointer 
   *ppc2-undo-stack*
   (set-fill-pointer *ppc2-undo-because* *ppc2-undo-count*))
  (vector-push-extend curstack *ppc2-undo-stack*)
  (vector-push-extend reason *ppc2-undo-because*)
  (setq *ppc2-undo-count* (%i+ *ppc2-undo-count* 1)))

(defun ppc2-close-undo (&aux
                        (new-count (%i- *ppc2-undo-count* 1))
                        (i (aref *ppc2-undo-stack* new-count)))
  (multiple-value-setq (*ppc2-undo-count* *ppc2-cstack* *ppc2-vstack* *ppc2-top-vstack-lcell*)
    (ppc2-decode-stack i))
  (set-fill-pointer 
   *ppc2-undo-stack*
   (set-fill-pointer *ppc2-undo-because* new-count)))





;;; "Trivial" means can be evaluated without allocating or modifying registers.
;;; Interim definition, which will probably stay here forever.
(defun ppc2-trivial-p (form &aux op bits)
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

(defun ppc2-lexical-reference-p (form)
  (when (acode-p form)
    (let ((op (acode-operator (setq form (acode-unwrapped-form form)))))
      (when (or (eq op (%nx1-operator lexical-reference))
                (eq op (%nx1-operator inherited-arg)))
        (%cadr form)))))



(defun ppc2-ref-symbol-value (seg vreg xfer sym check-boundp)
  (declare (ignorable check-boundp))
  (setq check-boundp (not *ppc2-reckless*))
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (when (or check-boundp vreg)
      (unless vreg (setq vreg ($ ppc::arg_z)))
      (if (eq sym '*interrupt-level*)
          (ensuring-node-target (target vreg)
            (! ref-interrupt-level target))
          (if *ppc2-open-code-inline*
            (ensuring-node-target (target vreg)
              (with-node-target (target) src
                (let* ((vcell (ppc2-symbol-value-cell sym))
                       (reg (ppc2-register-constant-p vcell)))
                  (if reg
                    (setq src reg)
                    (ppc2-store-immediate seg vcell src)))
                (if check-boundp
                  (! ref-symbol-value-inline target src)
                  (! %ref-symbol-value-inline target src))))
            (let* ((src ($ ppc::arg_z))
                   (dest ($ ppc::arg_z)))
              (ppc2-store-immediate seg (ppc2-symbol-value-cell sym) src)
              (if check-boundp
                (! ref-symbol-value dest src)
                (! %ref-symbol-value dest src))
              (<- dest)))))
    (^)))

#|
(defun ppc2-ref-symbol-value (seg vreg xfer sym check-boundp)  
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (if (eq sym '*interrupt-level*)
        (ensuring-node-target (target vreg)
          (! ref-interrupt-level target))
        (let* ((src ($ ppc::arg_z))
               (dest ($ ppc::arg_z)))
          (ppc2-store-immediate seg (ppc2-symbol-value-cell sym) src)
          (if check-boundp
            (! ref-symbol-value dest src)
            (! %ref-symbol-value dest src))
          (<- dest))))
    (^)))
||#

;;; Should be less eager to box result
(defun ppc2-extract-charcode (seg vreg xfer char safe)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((src (ppc2-one-untargeted-reg-form seg char ppc::arg_z)))
      (when safe
        (! trap-unless-character src))
      (if vreg
        (ensuring-node-target (target vreg)
          (! character->fixnum target src)))
      (^))))
  

(defun ppc2-reference-list (seg vreg xfer listform safe refcdr)
  (if (ppc2-form-typep listform 'list)
    (setq safe nil))                    ; May also have been passed as NIL.
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((src (ppc2-one-untargeted-reg-form seg listform ppc::arg_z)))
      (when safe
        (! trap-unless-list src))
      (if vreg
        (ensuring-node-target (target vreg)
          (if refcdr
            (! %cdr target src)
            (! %car target src))))
      (^))))







(defun ppc2-misc-byte-count (subtag element-count)
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


(defun ppc2-allocate-initialized-gvector (seg vreg xfer subtag initforms)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if (null vreg)
      (dolist (f initforms) (ppc2-form seg nil nil f))
      (let* ((*ppc2-vstack* *ppc2-vstack*)
             (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
             (arch (backend-target-arch *target-backend*))
             (n (length initforms))
             (nntriv (let* ((count 0)) 
                       (declare (fixnum count))
                       (dolist (f initforms count) 
                         (unless (ppc-side-effect-free-form-p f)
                           (incf count)))))
             (header (arch::make-vheader n subtag)))
        (declare (fixnum n nntriv))
        (cond ( (or *ppc2-open-code-inline* (> nntriv 3))
               (ppc2-formlist seg initforms nil)
               (ppc2-lri seg ppc::imm0 header)
               (! %ppc-gvector vreg ppc::imm0 (ash n (arch::target-word-shift arch))))
              (t
               (let* ((pending ())
                      (vstack *ppc2-vstack*))
                 (declare (fixnum vstack))
                 (dolist (form initforms)
                   (if (ppc-side-effect-free-form-p form)
                     (push form pending)
                     (progn
                       (push nil pending)
                       (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg form ppc::arg_z)))))
                 (ppc2-lri seg ppc::imm0 header)
                 (ensuring-node-target (target vreg)
                   (! %alloc-misc-fixed target ppc::imm0 (ash n (arch::target-word-shift arch)))
                   (with-node-temps (target) (nodetemp)
                     (do* ((forms pending (cdr forms))
                           (index (1- n) (1- index))
                           (pushed-cell (+ vstack (the fixnum (ash nntriv (arch::target-word-shift arch))))))
                          ((null forms))
                       (declare (list forms) (fixnum pushed-cell))
                       (let* ((form (car forms))
                              (reg nodetemp))
                         (if form
                           (setq reg (ppc2-one-untargeted-reg-form seg form nodetemp))
                           (progn
                             (decf pushed-cell *ppc2-target-node-size*)
                             (ppc2-stack-to-register seg (ppc2-vloc-ea pushed-cell) nodetemp)))
                         (! misc-set-c-node reg target index)))))
                 (! vstack-discard nntriv))
               ))))
     (^)))

;;; Heap-allocated constants -might- need memoization: they might be newly-created,
;;; as in the case of synthesized toplevel functions in .pfsl files.
(defun ppc2-acode-needs-memoization (valform)
  (if (ppc2-form-typep valform 'fixnum)
    nil
    (let* ((val (acode-unwrapped-form valform)))
      (if (or (eq val *nx-t*)
              (eq val *nx-nil*)
              (and (acode-p val)
                   (let* ((op (acode-operator val)))
                     (or (eq op (%nx1-operator fixnum)) #|(eq op (%nx1-operator immediate))|#))))
        nil
        t))))

(defun ppc2-modify-cons (seg vreg xfer ptrform valform safe setcdr returnptr)
  (if (ppc2-form-typep ptrform 'cons)
    (setq safe nil))                    ; May also have been passed as NIL.
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (ptr-vreg val-vreg) (ppc2-two-targeted-reg-forms seg ptrform ($ ppc::arg_y) valform ($ ppc::arg_z))
      (when safe
        (! trap-unless-cons ptr-vreg))
      (if setcdr
        (! call-subprim-2 ($ ppc::arg_z) (subprim-name->offset '.SPrplacd) ptr-vreg val-vreg)
        (! call-subprim-2 ($ ppc::arg_z) (subprim-name->offset '.SPrplaca) ptr-vreg val-vreg))
      (if returnptr
        (<- ptr-vreg)
        (<- val-vreg))
      (^))))



(defun ppc2-find-nilret-label ()
  (dolist (l *ppc2-nilret-labels*)
    (destructuring-bind (label vsp csp register-restore-count register-restore-ea &rest agenda) l
      (and (or (and (eql 0 register-restore-count)
                    (or (not (eql 0 vsp))
                        (eq vsp *ppc2-vstack*)))
                (and 
                 (eq register-restore-count *ppc2-register-restore-count*)
                 (eq vsp *ppc2-vstack*)))
           (or agenda (eq csp *ppc2-cstack*))
           (eq register-restore-ea *ppc2-register-restore-ea*)
           (eq (%ilsr 1 (length agenda)) *ppc2-undo-count*)
           (dotimes (i (the fixnum *ppc2-undo-count*) t) 
             (unless (and (eq (pop agenda) (aref *ppc2-undo-because* i))
                          (eq (pop agenda) (aref *ppc2-undo-stack* i)))
               (return)))
           (return label)))))

(defun ppc2-record-nilret-label ()
  (let* ((lab (backend-get-next-label))
         (info nil))
    (dotimes (i (the fixnum *ppc2-undo-count*))
      (push (aref *ppc2-undo-because* i) info)
      (push (aref *ppc2-undo-stack* i) info))
    (push (cons
                 lab 
                 (cons
                  *ppc2-vstack*
                  (cons 
                   *ppc2-cstack*
                   (cons
                    *ppc2-register-restore-count*
                    (cons
                     *ppc2-register-restore-ea*
                     (nreverse info))))))
          *ppc2-nilret-labels*)
    lab))

;;; If we know that the form is something that sets a CR bit,
;;; allocate a CR field and evaluate the form in such a way
;;; as to set that bit.
;;; If it's a compile-time constant, branch accordingly and
;;; let the dead code die.
;;; Otherwise, evaluate it to some handy register and compare
;;; that register to RNIL.
;;; "XFER" is a compound destination.
(defun ppc2-conditional-form (seg xfer form)
  (let* ((uwf (acode-unwrapped-form form)))
    (if (nx-null uwf)
      (ppc2-branch seg (ppc2-cd-false xfer) nil)
      (if (ppc-constant-form-p uwf)
        (ppc2-branch seg (ppc2-cd-true xfer) nil)
        (with-crf-target () crf
          (ppc2-form seg crf xfer form))))))

      
(defun ppc2-branch (seg xfer crf &optional cr-bit true-p)
  (let* ((*ppc2-vstack* *ppc2-vstack*)
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
    (with-ppc-local-vinsn-macros (seg)
      (setq xfer (or xfer 0))
      (when (logbitp $backend-mvpass-bit xfer) ;(ppc2-mvpass-p cd)
        (setq xfer (logand (lognot $backend-mvpass-mask) xfer))
        (unless *ppc2-returning-values*
          (ppc2-vpush-register seg ppc::arg_z)
          (ppc2-set-nargs seg 1)))
      (if (neq 0 xfer)
        (if (eq xfer $backend-return)    ;; xfer : RETURN ==> popj
          (ppc2-do-return seg)
          (if (not (ppc2-cd-compound-p xfer))
            (-> xfer)  ;; xfer : label# ==> BRA label#
            ;; cd is compound : (<true> / <false>)
            (let* ((truebranch (ppc2-cd-true xfer))
                   (falsebranch (ppc2-cd-false xfer))
                   (tbranch (if true-p truebranch falsebranch))
                   (nbranch (if true-p falsebranch truebranch))
                   (tn0 (neq 0 tbranch))
                   (tnret (neq $backend-return tbranch))
                   (nn0 (neq 0 nbranch))
                   (nnret (neq $backend-return nbranch))
                   (tlabel (if (and tnret tn0) (aref *backend-labels* tbranch)))
                   (nlabel (if (and nnret nn0) (aref *backend-labels* nbranch))))
              (unless cr-bit (setq cr-bit ppc::ppc-eq-bit))
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
                    (ppc2-do-return seg)
                    (@ aux-label))))))))))

(defun ppc2-cd-merge (cd label)
  (setq cd (or cd 0))
  (let ((mvpass (logbitp $backend-mvpass-bit cd)))
    (if (neq 0 (logand (lognot $backend-mvpass-mask) cd))
      (if (ppc2-cd-compound-p cd)
        (ppc2-make-compound-cd
         (ppc2-cd-merge (ppc2-cd-true cd) label)
         (ppc2-cd-merge (ppc2-cd-false cd) label)
         mvpass)
        cd)
      (if mvpass 
        (logior $backend-mvpass-mask label)
        label))))

(defun ppc2-mvpass-p (xfer)
  (if xfer (or (logbitp $backend-mvpass-bit xfer) (eq xfer $backend-mvpass))))

(defun ppc2-cd-compound-p (xfer)
  (if xfer (logbitp $backend-compound-branch-target-bit xfer)))

(defun ppc2-cd-true (xfer)
 (if (ppc2-cd-compound-p xfer)
   (ldb  $backend-compound-branch-true-byte xfer)
  xfer))

(defun ppc2-cd-false (xfer)
 (if (ppc2-cd-compound-p xfer)
   (ldb  $backend-compound-branch-false-byte xfer)
   xfer))

(defun ppc2-make-compound-cd (tpart npart &optional mvpass-p)
  (dpb (or npart 0) $backend-compound-branch-false-byte
       (dpb (or tpart 0) $backend-compound-branch-true-byte
            (logior (if mvpass-p $backend-mvpass-mask 0) $backend-compound-branch-target-mask))))

(defun ppc2-invert-cd (cd)
  (if (ppc2-cd-compound-p cd)
    (ppc2-make-compound-cd (ppc2-cd-false cd) (ppc2-cd-true cd) (logbitp $backend-mvpass-bit cd))
    cd))



;;; execute body, cleanup afterwards (if need to)
(defun ppc2-undo-body (seg vreg xfer body old-stack)
  (let* ((current-stack (ppc2-encode-stack))
         (numundo (%i- *ppc2-undo-count* (ppc2-encoding-undo-count old-stack))))
    (declare (fixnum numundo))
    (with-ppc-local-vinsn-macros (seg vreg xfer)
      (if (eq current-stack old-stack)
        (ppc2-form seg vreg xfer body)
        (if (eq xfer $backend-return)
          (progn
            (ppc2-form seg vreg xfer body)
            (dotimes (i numundo) (ppc2-close-undo)))
          (if (ppc2-mvpass-p xfer)
            (progn
              (ppc2-mvpass seg body) ; presumed to be ok
              (let* ((*ppc2-returning-values* :pass))
                (ppc2-nlexit seg xfer numundo)
                (^))
              (dotimes (i numundo) (ppc2-close-undo)))
            (progn
              ;; There are some cases where storing thru ppc::arg_z
              ;; can be avoided (stores to vlocs, specials, etc.) and
              ;; some other case where it can't ($test, $vpush.)  The
              ;; case of a null vd can certainly avoid it; the check
              ;; of numundo is to keep $acc boxed in case of nthrow.
              (ppc2-form  seg (if (or vreg (not (%izerop numundo))) ppc::arg_z) nil body)
              (ppc2-unwind-set seg xfer old-stack)
              (when vreg (<- ppc::arg_z))
              (^))))))))


(defun ppc2-unwind-set (seg xfer encoding)
  (multiple-value-bind (target-catch target-cstack target-vstack target-vstack-lcell)
                       (ppc2-decode-stack encoding)
    (ppc2-unwind-stack seg xfer target-catch target-cstack target-vstack)
    (setq *ppc2-undo-count* target-catch 
          *ppc2-cstack* target-cstack
          *ppc2-vstack* target-vstack
          *ppc2-top-vstack-lcell* target-vstack-lcell)))

(defun ppc2-unwind-stack (seg xfer target-catch target-cstack target-vstack)
  (let* ((current-catch *ppc2-undo-count*)
         (current-cstack *ppc2-cstack*)
         (current-vstack *ppc2-vstack*)
         (diff (%i- current-catch target-catch))
         target
         (exit-vstack current-vstack))
    (declare (ignore-if-unused target))
    (when (neq 0 diff)
      (setq exit-vstack (ppc2-nlexit seg xfer diff))
      (multiple-value-setq (target current-cstack current-vstack)
                           (ppc2-decode-stack (aref *ppc2-undo-stack* target-catch))))
    (if (%i< 0 (setq diff (%i- current-cstack target-cstack)))
      (with-ppc-local-vinsn-macros (seg)
        (! adjust-sp diff)))
    (if (%i< 0 (setq diff (%i- current-vstack target-vstack)))
      (with-ppc-local-vinsn-macros (seg)
        (! vstack-discard (ash diff (- *ppc2-target-fixnum-shift*)))))
    exit-vstack))

;;; We can sometimes combine unwinding the catch stack with returning from the function
;;; by jumping to a subprim that knows how to do this.  If catch frames were distinguished
;;; from unwind-protect frames, we might be able to do this even when saved registers
;;; are involved (but the subprims restore them from the last catch frame.)
;;; *** there are currently only subprims to handle the "1 frame" case; add more ***
(defun ppc2-do-return (seg)
  (let* ((*ppc2-vstack* *ppc2-vstack*)
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
         (mask *ppc2-register-restore-count*)
         (ea *ppc2-register-restore-ea*)
         (label nil)
         (vstack nil)
         (foldp (not *ppc2-open-code-inline*)))
    (if (%izerop mask) (setq mask nil))
    (with-ppc-local-vinsn-macros (seg)
      (progn
        (setq vstack (ppc2-set-vstack (ppc2-unwind-stack seg $backend-return 0 0 #x7fffff)))
        (if *ppc2-returning-values*
          (cond ((and mask foldp (setq label (%cdr (assq vstack *ppc2-valret-labels*))))
                 (-> label))
                (t
                 (@ (setq label (backend-get-next-label)))
                 (push (cons vstack label) *ppc2-valret-labels*)
                 (when mask
                   (with-imm-temps () (vsp0)
                     (! fixnum-add vsp0 ppc::vsp ppc::nargs)
                     (ppc2-restore-nvrs seg ea mask vsp0)))
                 (! nvalret)))
          (if (null mask)
            (if *ppc2-open-code-inline*
              (progn
                (! restore-full-lisp-context)
                (! jump-return-pc))
              (! popj))
            (if (and foldp (setq label (assq *ppc2-vstack* *ppc2-popreg-labels*)))
              (-> (cdr label))
              (let* ((new-label (backend-get-next-label)))
                (@ new-label)
                (push (cons *ppc2-vstack* new-label) *ppc2-popreg-labels*)
                (ppc2-set-vstack (ppc2-restore-nvrs seg ea mask))
                (if *ppc2-open-code-inline*
                  (progn
                    (! restore-full-lisp-context)
                    (! jump-return-pc))
                  (! popj))))))))
    nil))



(defun ppc2-mvcall (seg vreg xfer fn arglist &optional recursive-p)
  (let* ((cstack *ppc2-cstack*)
         (vstack *ppc2-vstack*))
    (with-ppc-local-vinsn-macros (seg vreg xfer)
      (if (and (eq xfer $backend-return) (not (ppc2-tailcallok xfer)))
        (progn
          (ppc2-mvcall seg vreg $backend-mvpass fn arglist t)
          (ppc2-set-vstack (%i+ (if arglist *ppc2-target-node-size* 0) vstack))
          (setq *ppc2-cstack* cstack)
          (let* ((*ppc2-returning-values* t)) (^)))
        (let* ((mv-p (ppc2-mv-p xfer)))
          (if (null arglist)
            (ppc2-call-fn seg vreg xfer fn arglist nil)
            (progn
              (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg fn ppc::arg_z))
              (ppc2-multiple-value-body seg (pop arglist))
              (when arglist
                (ppc2-open-undo $undostkblk)
                (! save-values)
                (dolist (form arglist)
                  (ppc2-multiple-value-body seg form)
                  (! add-values))
                (ppc2-set-nargs seg 0)
                (! recover-values)
                (ppc2-close-undo))
              (! lisp-word-ref ppc::temp0 ppc::vsp ppc::nargs)
              (ppc2-invoke-fn seg ppc::temp0 nil nil xfer)))
          (unless recursive-p
            (if mv-p
              (unless (eq xfer $backend-return)
                (let* ((*ppc2-returning-values* t))
                  (^)))
              (progn 
                (ppc2-adjust-vstack (- *ppc2-target-node-size*)) ; discard function
                (! vstack-discard 1)
                (<- ppc::arg_z)
                (^)))))))))


(defun ppc2-hard-opt-p (opts)
  (or
   (dolist (x (%cadr opts))
     (unless (nx-null x) (return t)))
   (dolist (x (%caddr opts))
     (when x (return t)))))

(defun ppc2-close-lambda (seg req opt rest keys auxen)
  (dolist (var req)
    (ppc2-close-var seg var))
  (dolist (var (%car opt))
    (ppc2-close-var seg var))
  (dolist (var (%caddr opt))
    (when var
      (ppc2-close-var seg var)))
  (if rest
    (ppc2-close-var seg rest))
  (dolist (var (%cadr keys))
    (ppc2-close-var seg var))
  (dolist (var (%caddr keys))
    (if var (ppc2-close-var seg var)))
  (dolist (var (%car auxen))
    (ppc2-close-var seg var)))

(defun ppc2-close-structured-var (seg var)
  (if (ppc2-structured-var-p var)
    (apply #'ppc2-close-structured-lambda seg (cdr var))
    (ppc2-close-var seg var)))

(defun ppc2-close-structured-lambda (seg whole req opt rest keys auxen)
  (if whole
    (ppc2-close-var seg whole))
  (dolist (var req)
    (ppc2-close-structured-var seg var))
  (dolist (var (%car opt))
    (ppc2-close-structured-var seg var))
  (dolist (var (%caddr opt))
    (when var
      (ppc2-close-var seg var)))
  (if rest
    (ppc2-close-structured-var seg rest))
  (dolist (var (%cadr keys))
    (ppc2-close-structured-var seg var))
  (dolist (var (%caddr keys))
    (if var (ppc2-close-var seg var)))
  (dolist (var (%car auxen))
    (ppc2-close-var seg var)))


(defun ppc2-init-regvar (seg var reg addr)
  (with-ppc-local-vinsn-macros (seg)
    (ppc2-stack-to-register seg addr reg)
    (ppc2-set-var-ea seg var ($ reg))))

(defun ppc2-bind-structured-var (seg var vloc lcell &optional context)
  (if (not (ppc2-structured-var-p var))
    (let* ((reg (ppc2-assign-register-var var)))
      (if reg
        (ppc2-init-regvar seg var reg (ppc2-vloc-ea vloc))
        (ppc2-bind-var seg var vloc lcell)))
    (let* ((v2 (%cdr var))
           (v v2)
           (vstack *ppc2-vstack*)
           (whole (pop v))
           (req (pop v))
           (opt (pop v))
           (rest (pop v))
           (keys (pop v)))
      
      (apply #'ppc2-bind-structured-lambda seg 
             (ppc2-spread-lambda-list seg (ppc2-vloc-ea vloc) whole req opt rest keys context)
             vstack context v2))))

(defun ppc2-bind-structured-lambda (seg lcells vloc context whole req opt rest keys auxen
                        &aux (nkeys (list-length (%cadr keys))))
  (declare (fixnum vloc))
  (when whole
    (ppc2-bind-structured-var seg whole vloc (pop lcells))
    (incf vloc *ppc2-target-node-size*))
  (dolist (arg req)
    (ppc2-bind-structured-var seg arg vloc (pop lcells) context)
    (incf vloc *ppc2-target-node-size*))
  (when opt
   (if (ppc2-hard-opt-p opt)
     (setq vloc (apply #'ppc2-structured-initopt seg lcells vloc context opt)
           lcells (nthcdr (ash (length (car opt)) 1) lcells))
     (dolist (var (%car opt))
       (ppc2-bind-structured-var seg var vloc (pop lcells) context)
       (incf vloc *ppc2-target-node-size*))))
  (when rest
    (ppc2-bind-structured-var seg rest vloc (pop lcells) context)
    (incf vloc *ppc2-target-node-size*))
  (when keys
    (apply #'ppc2-structured-init-keys seg lcells vloc context keys)
    (setq vloc (%i+ vloc (* *ppc2-target-node-size* (+ nkeys nkeys)))))
  (ppc2-seq-bind seg (%car auxen) (%cadr auxen)))

(defun ppc2-structured-var-p (var)
  (and (consp var) (or (eq (%car var) *nx-lambdalist*)
                       (eq (%car var) (%nx1-operator lambda-list)))))

(defun ppc2-simple-var (var &aux (bits (cadr var)))
  (if (or (%ilogbitp $vbitclosed bits)
          (%ilogbitp $vbitspecial bits))
    (nx-error "Non-simple-variable ~S" (%car var))
    var))

(defun ppc2-nlexit (seg xfer &optional (nlevels 0))
  (let* ((numnthrow 0)
         (n *ppc2-undo-count*)
         (cstack *ppc2-cstack*)
         (vstack *ppc2-vstack*)
         (target-cstack)
         (target-vstack)
         (lastcatch n)
         (i nil)
         (returning (eq xfer $backend-return))
         (junk1 nil)
         (unbind ())
         (dest (%i- n nlevels))
         (retval *ppc2-returning-values*)
         reason)
    (declare (ignorable junk1))
    (with-ppc-local-vinsn-macros (seg)
      (when (neq 0 nlevels)
        (let* ((numnlispareas 0))
          (declare (fixnum numnlispareas))
          (flet ((popnlispareas ()
                   (dotimes (i numnlispareas)
                     (! discard-temp-frame)))
                 (throw-through-numnthrow-catch-frames ()
                   (when (neq 0 numnthrow)
                     (ppc2-lri seg ppc::imm0 (ash numnthrow *ppc2-target-fixnum-shift*))
                     (if retval
                       (! nthrowvalues)
                       (! nthrow1value))
                     (setq numnthrow 0)
                     (multiple-value-setq (junk1 cstack vstack)
                       (ppc2-decode-stack (aref *ppc2-undo-stack* lastcatch))))))
            (while (%i> n dest)
              (cond ((eql $undocatch (setq reason (aref *ppc2-undo-because* (setq n (%i- n 1)))))
                     (popnlispareas)
                     (setq numnthrow (%i+ numnthrow 1) lastcatch n))
                    ((eql $undostkblk reason)
                     (throw-through-numnthrow-catch-frames)
                     (incf numnlispareas))
                    ((eql $undo-ppc-c-frame reason)
                     (! discard-c-frame))))
            (throw-through-numnthrow-catch-frames)
            (setq i lastcatch)
            (while (%i> i dest)
              (let ((reason (aref *ppc2-undo-because* (setq i (%i- i 1)))))
                (if (or (eql reason $undospecial)
                        (eql reason $undointerruptlevel))
                  (push reason unbind))))
            (if unbind
              (ppc2-dpayback-list seg (nreverse unbind)))
            (when (and (neq lastcatch dest)
                       (%i>
                        vstack
                        (setq target-vstack 
                              (nth-value 2 (ppc2-decode-stack (aref *ppc2-undo-stack* dest)))))
                       (neq retval t))
              (unless returning
                (let ((vdiff (%i- vstack target-vstack)))
                  (if retval
                    (progn
                      (ppc2-lri seg ppc::imm0 vdiff)
                      (! slide-values))
                    (! adjust-vsp vdiff)))))
            (setq numnlispareas 0)
            (while (%i> lastcatch dest)
              (let ((reason (aref *ppc2-undo-because* (setq lastcatch (%i- lastcatch 1)))))
                (setq target-cstack (nth-value 1
                                               (ppc2-decode-stack (aref *ppc2-undo-stack* lastcatch))))
                (if (eq reason $undostkblk)
                  (incf numnlispareas))
                (if (%i> cstack target-cstack)
                  (with-ppc-local-vinsn-macros (seg)
                    (! adjust-sp (%i- cstack target-cstack))))
                ; else what's going on? $sp-stkcons, for one thing
                (setq cstack target-cstack)))
            (popnlispareas)))
        vstack))))


;;; Restore the most recent dynamic bindings.  Bindings
;;; of *INTERRUPT-LEVEL* get special treatment.
(defun ppc2-dpayback-list (seg reasons)
  (with-ppc-local-vinsn-macros (seg)
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
              (if *ppc2-open-code-inline*
                (! unbind-interrupt-level-inline)
                (! unbind-interrupt-level)))
            (compiler-bug "unknown payback token ~s" r)))))))

(defun ppc2-spread-lambda-list (seg listform whole req opt rest keys 
                                    &optional enclosing-ea cdr-p)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((numopt (length (%car opt)))
           (nkeys (length (%cadr keys)))
           (numreq (length req))
           (vtotal numreq)
           (old-top *ppc2-top-vstack-lcell*)
           (listreg ($ ppc::temp3))
           (doadlword (dpb nkeys (byte 8 16) (dpb numopt (byte 8 8) (dpb numreq (byte 8 0) 0 )))))
      (declare (fixnum numopt nkeys numreq vtotal doadlword))
      (when (or (> numreq 255) (> numopt 255) (> nkeys 255))
        (compiler-bug "A lambda list can contain a maximum of 255 required, 255 optional, and 255 keywords args"))
      (if (fixnump listform)
        (ppc2-store-ea seg listform listreg)
        (ppc2-one-targeted-reg-form seg listform listreg))
      (when whole
        (ppc2-vpush-register seg listreg :reserved))
      (when keys
        (setq doadlword (%ilogior2 (ash #x80000000 -6) doadlword))
        (incf  vtotal (%ilsl 1 nkeys))
        (if (%car keys)                 ; &allow-other-keys
          (setq doadlword (%ilogior doadlword (ash #x80000000 -5))))
        (ppc2-store-immediate seg (%car (%cdr (%cdr (%cdr (%cdr keys))))) ppc::temp2))
      (when opt
        (setq vtotal (%i+ vtotal numopt))
        (when (ppc2-hard-opt-p opt)
          (setq doadlword (%ilogior2 doadlword (ash #x80000000 -7)))
          (setq vtotal (%i+ vtotal numopt))))
      (when rest
        (setq doadlword (%ilogior2 (ash #x80000000 -4) doadlword) vtotal (%i+ vtotal 1)))
      (ppc2-reserve-vstack-lcells vtotal)
      (! load-adl doadlword)
      (if cdr-p
        (! macro-bind)
        (if enclosing-ea
          (progn
            (ppc2-store-ea seg enclosing-ea ppc::arg_z)
            (! destructuring-bind-inner))
          (! destructuring-bind)))
      (ppc2-set-vstack (%i+ *ppc2-vstack* (* *ppc2-target-node-size* vtotal)))
      (ppc2-collect-lcells :reserved old-top))))


(defun ppc2-tailcallok (xfer)
  (and (eq xfer $backend-return)
       *ppc2-tail-allow*
       (eq 0 *ppc2-undo-count*)))

(defun ppc2-mv-p (cd)
  (or (eq cd $backend-return) (ppc2-mvpass-p cd)))

(defun ppc2-expand-note (note)
  (let* ((lab (vinsn-note-label note)))
    (case (vinsn-note-class note)
      ((:regsave :begin-variable-scope :end-variable-scope)
       (setf (vinsn-label-info lab) (emit-lap-label lab))))))

(defun ppc2-expand-vinsns (header)
  (do-dll-nodes (v header)
    (if (%vinsn-label-p v)
      (let* ((id (vinsn-label-id v)))
        (if (typep id 'fixnum)
          (when (or t (vinsn-label-refs v))
            (setf (vinsn-label-info v) (emit-lap-label v)))
          (ppc2-expand-note id)))
      (ppc2-expand-vinsn v)))
  ;;; This doesn't have too much to do with anything else that's
  ;;; going on here, but it needs to happen before the lregs
  ;;; are freed.  There really shouldn't be such a thing as a
  ;;; var-ea, of course ...
  (dolist (s *ppc2-recorded-symbols*)
    (let* ((var (car s))
           (ea (var-ea var)))
      (when (typep ea 'lreg)
        (setf (var-ea var) (lreg-value ea)))))
  (free-logical-registers)
  (ppc2-free-lcells))

;;; It's not clear whether or not predicates, etc. want to look
;;; at an lreg or just at its value slot.
;;; It's clear that the assembler just wants the value, and that
;;; the value had better be assigned by the time we start generating
;;; machine code.
;;; For now, we replace lregs in the operand vector with their values
;;; on entry, but it might be reasonable to make PARSE-OPERAND-FORM
;;; deal with lregs ...
(defun ppc2-expand-vinsn (vinsn)
  (let* ((template (vinsn-template vinsn))
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
        (make-lap-label unique)))
    (labels ((parse-operand-form (valform)
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
               (let* ((operands (cdr f))
                      (head (make-list (length operands)))
                      (tail head))
                 (declare (dynamic-extent head)
                          (cons (head tail)))
                 (dolist (op operands)
                   (rplaca tail (parse-operand-form op))
                   (setq tail (cdr tail)))
                 (ppc-emit-lap-instruction (svref ppc::*ppc-opcodes* (car f)) 
                                           head)))
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
                 (emit-lap-label (assq f unique-labels))
                 (if (atom f)
                   (compiler-bug "Invalid form in vinsn body: ~s" f)
                   (if (atom (car f))
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





(defun ppc2-builtin-index-subprim (idx)
  (let* ((arch (backend-target-arch *target-backend*))
         (table (arch::target-primitive->subprims  arch))
         (shift (arch::target-subprims-shift arch)))
    (dolist (cell table)
      (destructuring-bind ((low . high) . base) cell
        (if (and (>= idx low)
                 (< idx high))
          (return (+ base (ash (- idx low) shift))))))))

(defun ppc2-fixed-call-builtin (seg vreg xfer name subprim)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((index (arch::builtin-function-name-offset name))
           (idx-subprim (if index (ppc2-builtin-index-subprim index)))
           (tail-p (ppc2-tailcallok xfer)))
      (when tail-p
        (ppc2-restore-nvrs seg *ppc2-register-restore-ea* *ppc2-register-restore-count*)
        (ppc2-restore-full-lisp-context seg))
      (if idx-subprim
        (setq subprim idx-subprim)
        (if index (! lri ($ ppc::imm0) (ash index *ppc2-target-fixnum-shift*))))
      (if tail-p
        (! jump-subprim subprim)
        (progn
          (! call-subprim subprim)
          (<- ($ ppc::arg_z))
          (^))))))

(defun ppc2-unary-builtin (seg vreg xfer name form)
  (with-ppc-local-vinsn-macros (seg)
    (ppc2-one-targeted-reg-form seg form ($ ppc::arg_z))
    (ppc2-fixed-call-builtin seg vreg xfer name (subprim-name->offset '.SPcallbuiltin1))))

(defun ppc2-binary-builtin (seg vreg xfer name form1 form2)
  (with-ppc-local-vinsn-macros (seg)
    (ppc2-two-targeted-reg-forms seg form1 ($ ppc::arg_y) form2 ($ ppc::arg_z))
    (ppc2-fixed-call-builtin seg vreg xfer name (subprim-name->offset '.SPcallbuiltin2))))

(defun ppc2-ternary-builtin (seg vreg xfer name form1 form2 form3)
  (with-ppc-local-vinsn-macros (seg)
    (ppc2-three-targeted-reg-forms seg form1 ($ ppc::arg_x) form2 ($ ppc::arg_y) form3 ($ ppc::arg_z))
    (ppc2-fixed-call-builtin seg vreg xfer name (subprim-name->offset '.SPcallbuiltin3))))


(eval-when (:compile-toplevel :execute :load-toplevel)


(defmacro defppc2 (name locative arglist &body forms)
  (multiple-value-bind (body decls)
                       (parse-body forms nil t)
    (destructuring-bind (vcode-block dest control &rest other-args) arglist
      (let* ((fun `(nfunction ,name 
                              (lambda (,vcode-block ,dest ,control ,@other-args) ,@decls 
                                      (block ,name (with-ppc-local-vinsn-macros (,vcode-block ,dest ,control) ,@body))))))
        `(progn
           (record-source-file ',name 'function)
           (svset *ppc2-specials* (%ilogand #.operator-id-mask (%nx1-operator ,locative)) ,fun))))))
)
  
(defppc2 ppc2-lambda lambda-list (seg vreg xfer req opt rest keys auxen body p2decls)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((stack-consed-rest nil)
           (lexprp (if (consp rest) (progn (setq rest (car rest)) t)))
           (rest-var-bits (and rest (nx-var-bits rest)))
           (rest-ignored-p (and rest (not lexprp) (%ilogbitp $vbitignore rest-var-bits)))
           (want-stack-consed-rest (or rest-ignored-p
                                       (and rest (not lexprp) (%ilogbitp $vbitdynamicextent rest-var-bits))))
           (afunc *ppc2-cur-afunc*)
           (inherited-vars (afunc-inherited-vars afunc))
           (fbits (afunc-bits afunc))
           (methodp (%ilogbitp $fbitmethodp fbits))
           (method-var (if methodp (pop req)))
           (next-method-p (%ilogbitp $fbitnextmethp fbits))
           (allow-other-keys-p (%car keys))
           (hardopt (ppc2-hard-opt-p opt))
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
           (*ppc2-vstack* 0))
      (declare (type (unsigned-byte 16) num-req num-opt num-inh))
      (with-ppc-p2-declarations p2decls
        (setq *ppc2-inhibit-register-allocation*
              (setq no-regs (%ilogbitp $fbitnoregs fbits)))
        (multiple-value-setq (pregs reglocatives) 
          (ppc2-allocate-global-registers *ppc2-fcells* *ppc2-vcells* (afunc-all-vars afunc) no-regs))
        (@ (backend-get-next-label)) ; generic self-reference label, should be label #1
        (unless next-method-p
          (setq method-var nil))
        
        (let* ((rev-req (reverse req))
               (rev-fixed (if inherited-vars (reverse (append inherited-vars req)) rev-req))
               (num-fixed (length rev-fixed))
               (rev-opt (reverse (car opt))))
          (if (not (or opt rest keys))
            (setq arg-regs (ppc2-req-nargs-entry seg rev-fixed))
            (if (and (not (or hardopt rest keys))
                     (<= num-opt $numppcargregs))
              (setq arg-regs (ppc2-simple-opt-entry seg rev-opt rev-fixed))
              (progn
                ; If the minumum acceptable number of args is non-zero, ensure
                ; that at least that many were received.  If there's an upper bound,
                ; enforce it.
                
                (when rev-fixed
                  (ppc2-reserve-vstack-lcells num-fixed)                    
                  (! check-min-nargs num-fixed))
                (unless (or rest keys)
                  (! check-max-nargs (+ num-fixed num-opt)))
                ;; Going to have to call one or more subprims.  First save
                ;; the LR in LOC-PC.
                (! save-lr)
                ;; If there were &optional args, initialize their values
                ;; to NIL.  All of the argregs get vpushed as a result of this.
                (when opt
                  (ppc2-reserve-vstack-lcells num-opt)
                  (! default-optionals (+ num-fixed num-opt)))
                (when keys
                  (let* ((keyvect (%car (%cdr (%cdr (%cdr (%cdr keys))))))
                         (flags (the fixnum (logior (the fixnum (if rest 4 0)) 
                                                    (the fixnum (if (or methodp allow-other-keys-p) 1 0)))))
                         (nkeys (length keyvect))
                         (nprev (+ num-fixed num-opt)))
                    (declare (fixnum flags nkeys nprev))
                    (dotimes (i (the fixnum (+ nkeys nkeys)))
                      (ppc2-new-vstack-lcell :reserved *ppc2-target-lcell-size* 0 nil))
                    (! misc-ref-c-node ppc::temp3 ppc::nfn (1+ (backend-immediate-index keyvect)))
                    (ppc2-lri seg ppc::imm2 (ash flags *ppc2-target-fixnum-shift*))
                    (ppc2-lri seg ppc::imm3 (ash nkeys *ppc2-target-fixnum-shift*))
                    (unless (= nprev 0)
                      (ppc2-lri seg ppc::imm0 (ash nprev *ppc2-target-fixnum-shift*)))
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
                    (ppc2-lexpr-entry seg num-fixed)
                    (progn
                      (if want-stack-consed-rest
                        (setq stack-consed-rest t))
                      (let* ((nprev (+ num-fixed num-opt))
                             (simple (and (not keys) (= 0 nprev))))
                        (declare (fixnum nprev))
                        (unless simple
                          (ppc2-lri seg ppc::imm0 (ash nprev *ppc2-target-fixnum-shift*)))
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
                      (ppc2-reserve-vstack-lcells 1))))
                (when hardopt
                  (ppc2-reserve-vstack-lcells num-opt)
                  (ppc2-lri seg ppc::imm0 (ash num-opt *ppc2-target-fixnum-shift*))

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
                       (nbytes-vpushed (* nwords-vpushed *ppc2-target-node-size*)))
                  (declare (fixnum nwords-vpushed nbytes-vpushed))
                  (unless (or lexprp keys) 
                    (if *ppc2-open-code-inline*
                      (! save-lisp-context-offset nbytes-vpushed)
                      (! save-lisp-context-offset-ool nbytes-vpushed)))
                  (ppc2-set-vstack nbytes-vpushed)
                  (setq optsupvloc (- *ppc2-vstack* (* num-opt *ppc2-target-node-size*)))))))
          ;; Caller's context is saved; *ppc2-vstack* is valid.  Might still have method-var
          ;; to worry about.
          (unless (= 0 pregs)
            ;; Save NVRs; load constants into any that get constants.
            (ppc2-save-nvrs seg pregs)

            (dolist (pair reglocatives)
              (declare (cons pair))
              (let* ((constant (car pair))
                     (reg (cdr pair)))
                (declare (cons constant))
                (rplacd constant reg)
                (! ref-constant reg (backend-immediate-index (car constant))))))
          (when (and (not (or opt rest keys))
                     (<= num-fixed $numppcargregs)
                     (not (some #'null arg-regs)))
            (setq *ppc2-tail-vsp* *ppc2-vstack*
                  *ppc2-tail-nargs* num-fixed)
            (@ (setq *ppc2-tail-label* (backend-get-next-label))))
          (when method-var
            (ppc2-seq-bind-var seg method-var ppc::next-method-context))
          ;; If any arguments are still in arg_x, arg_y, arg_z, that's
          ;; because they weren't vpushed in a "simple" entry case and
          ;; belong in some NVR.  Put them in their NVRs, so that we
          ;; can handle arbitrary expression evaluation (special
          ;; binding, value-cell consing, etc.) without clobbering the
          ;; argument registers.
          (when arg-regs
            (do* ((vars arg-regs (cdr vars))
                  (arg-reg-num ppc::arg_z (1- arg-reg-num)))
                 ((null vars))
              (declare (list vars) (fixnum arg-reg-num))
              (let* ((var (car vars)))
                (when var
                  (let* ((reg (ppc2-assign-register-var var)))
                    (ppc2-copy-register seg reg arg-reg-num)
                    (setf (var-ea var) reg))))))
          (setq *ppc2-entry-vsp-saved-p* t)
#|
          (when stack-consed-rest
            (if rest-ignored-p
              (if nil (ppc2-jsrA5 $sp-popnlisparea))
              (progn
                (ppc2-open-undo $undostkblk))))
|#
          (when stack-consed-rest
            (ppc2-open-undo $undostkblk))
          (setq *ppc2-entry-vstack* *ppc2-vstack*)
          (setq reserved-lcells (ppc2-collect-lcells :reserved))
          (ppc2-bind-lambda seg reserved-lcells req opt rest keys auxen optsupvloc arg-regs lexprp inherited-vars))
        (when method-var (ppc2-heap-cons-next-method-var seg method-var))
        (ppc2-form seg vreg xfer body)
        (ppc2-close-lambda seg req opt rest keys auxen)
        (dolist (v inherited-vars)
          (ppc2-close-var seg v))
        (when method-var
          (ppc2-close-var seg method-var))
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


(defppc2 ppc2-progn progn (seg vreg xfer forms)
  (declare (list forms))
  (if (null forms)
    (ppc2-nil seg vreg xfer)
    (loop
      (let* ((form (pop forms)))
        (if forms
          (ppc2-form seg nil nil form)
          (return (ppc2-form seg vreg xfer form)))))))



(defppc2 ppc2-prog1 prog1 (seg vreg xfer forms)
  (if (eq (list-length forms) 1)
    (ppc2-use-operator (%nx1-operator values) seg vreg xfer forms)
    (if (null vreg)
      (ppc2-use-operator (%nx1-operator progn) seg vreg xfer forms)
      (let* ((float-p (= (hard-regspec-class vreg) hard-reg-class-fpr))
             (crf-p (= (hard-regspec-class vreg) hard-reg-class-crf))
             (node-p (unless (or float-p crf-p)
                       (= (get-regspec-mode vreg) hard-reg-class-gpr-mode-node)))
             (first (pop forms)))
        (ppc2-push-register seg 
                            (if (or node-p crf-p)
                              (ppc2-one-untargeted-reg-form seg first ppc::arg_z)
                              (ppc2-one-targeted-reg-form seg first vreg)))
        (dolist (form forms)
          (ppc2-form seg nil nil form))
        (if crf-p
          (progn
            (ppc2-vpop-register seg ppc::arg_z)
            (<- ppc::arg_z))
          (ppc2-pop-register seg vreg))
        (^)))))

(defppc2 ppc2-free-reference free-reference (seg vreg xfer sym)
  (ppc2-ref-symbol-value seg vreg xfer sym t))

(defppc2 ppc2-special-ref special-ref (seg vreg xfer sym)
  (ppc2-ref-symbol-value seg vreg xfer sym t))

(defppc2 ppc2-bound-special-ref bound-special-ref (seg vreg xfer sym)
  (ppc2-ref-symbol-value seg vreg xfer sym nil))

(defppc2 ppc2-%slot-ref %slot-ref (seg vreg xfer instance idx)
  (ensuring-node-target (target (or vreg ($ ppc::arg_z)))
    (multiple-value-bind (v i)
        (ppc2-two-untargeted-reg-forms seg instance ppc::arg_y idx ppc::arg_z)
      (unless *ppc2-reckless*
        (! check-misc-bound i v))
      (with-node-temps (v) (temp)
        (! %slot-ref temp v i)
        (ppc2-copy-register seg target temp))))
  (^))
  
(defppc2 ppc2-%svref %svref (seg vreg xfer vector index)
  (ppc2-vref seg vreg xfer :simple-vector vector index nil))

(defppc2 ppc2-svref svref (seg vreg xfer vector index)
  (ppc2-vref seg vreg xfer :simple-vector  vector index (unless *ppc2-reckless* (nx-lookup-target-uvector-subtag :simple-vector))))

;;; It'd be nice if this didn't box the result.  Worse things happen ...
;;;  Once there's a robust mechanism, adding a CHARCODE storage class shouldn't be hard.
(defppc2 ppc2-%sbchar %sbchar (seg vreg xfer string index)
  (ppc2-vref seg vreg xfer :simple-string string index (unless *ppc2-reckless* (nx-lookup-target-uvector-subtag :simple-string))))


(defppc2 ppc2-%svset %svset (seg vreg xfer vector index value)
  (ppc2-vset seg vreg xfer :simple-vector vector index value nil))

(defppc2 ppc2-svset svset (seg vreg xfer vector index value)
  (ppc2-vset seg vreg xfer :simple-vector  vector index value (nx-lookup-target-uvector-subtag :simple-vector)))

(defppc2 ppc2-typed-form typed-form (seg vreg xfer typespec form &optional check)
  (if check
    (ppc2-typechecked-form seg vreg xfer typespec form)
    (ppc2-form seg vreg xfer form)))

(defppc2 ppc2-%primitive %primitive (seg vreg xfer &rest ignore)
  (declare (ignore seg vreg xfer ignore))
  (compiler-bug "You're probably losing big: using %primitive ..."))

(defppc2 ppc2-consp consp (seg vreg xfer cc form)
  (if (null vreg)
    (ppc2-form seg vreg xfer form)
    (let* ((tagreg ppc::imm0))
      (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
        (! extract-fulltag tagreg (ppc2-one-untargeted-reg-form seg form ppc::arg_z))
        (ppc2-test-reg-%izerop seg vreg xfer tagreg cr-bit true-p
                               (target-arch-case
                                (:ppc32 ppc32::fulltag-cons)
                                (:ppc64 ppc64::fulltag-cons)))))))
      
(defppc2 ppc2-cons cons (seg vreg xfer y z)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil y)
      (ppc2-form seg nil xfer z))
    (multiple-value-bind (yreg zreg) (ppc2-two-untargeted-reg-forms seg y ppc::arg_y z ppc::arg_z)
      (ensuring-node-target (target vreg)
        (! cons target yreg zreg))
      (^))))



(defppc2 ppc2-%rplaca %rplaca (seg vreg xfer ptr val)
  (ppc2-modify-cons seg vreg xfer ptr val nil nil t))

(defppc2 ppc2-%rplacd %rplacd (seg vreg xfer ptr val)
  (ppc2-modify-cons seg vreg xfer ptr val nil t t))

(defppc2 ppc2-rplaca rplaca (seg vreg xfer ptr val)
  (ppc2-modify-cons seg vreg xfer ptr val t nil t))

(defppc2 ppc2-set-car set-car (seg vreg xfer ptr val)
  (ppc2-modify-cons seg vreg xfer ptr val t nil nil))

(defppc2 ppc2-rplacd rplacd (seg vreg xfer ptr val)
  (ppc2-modify-cons seg vreg xfer ptr val t t t))

(defppc2 ppc2-set-cdr set-cdr (seg vreg xfer ptr val)
  (ppc2-modify-cons seg vreg xfer ptr val t t nil))

(defppc2 ppc2-%car %car (seg vreg xfer form)
  (ppc2-reference-list seg vreg xfer form nil nil))

(defppc2 ppc2-%cdr %cdr (seg vreg xfer form)
  (ppc2-reference-list seg vreg xfer form nil t))

(defppc2 ppc2-car car (seg vreg xfer form)
  (ppc2-reference-list seg vreg xfer form t nil))

(defppc2 ppc2-cdr cdr (seg vreg xfer form)
  (ppc2-reference-list seg vreg xfer form t t))


(defppc2 ppc2-vector vector (seg vreg xfer arglist)
  (ppc2-allocate-initialized-gvector seg vreg xfer
                                     (nx-lookup-target-uvector-subtag
                                      :simple-vector) arglist))

(defppc2 ppc2-%gvector %gvector (seg vreg xfer arglist)
  (let* ((all-on-stack (append (car arglist) (reverse (cadr arglist))))
         (subtag-form (car all-on-stack))
         (subtag (acode-fixnum-form-p subtag-form)))
    (if (null vreg)
      (dolist (form all-on-stack (^)) (ppc2-form seg nil nil form))
      (if (null subtag)
        (progn                            ; Vpush everything and call subprim
          (let* ((*ppc2-vstack* *ppc2-vstack*)
                 (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
            (ppc2-set-nargs seg (ppc2-formlist seg all-on-stack nil))
            (! gvector))
          (<- ppc::arg_z)
          (^))
        (ppc2-allocate-initialized-gvector seg vreg xfer subtag (cdr all-on-stack))))))

;;; Should be less eager to box result
(defppc2 ppc2-%char-code %char-code (seg vreg xfer c)
  (ppc2-extract-charcode seg vreg xfer c nil))

(defppc2 ppc2-char-code char-code (seg vreg xfer c)
  (ppc2-extract-charcode seg vreg xfer c (not (ppc2-form-typep c 'character))))

(defppc2 ppc2-%ilogior2 %ilogior2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (ppc2-use-operator (%nx1-operator fixnum) seg vreg xfer (logior fix1 fix2)))
    (let* ((fixval (or fix1 fix2))
           (unboxed-fixval (if fixval (ash fixval *ppc2-target-fixnum-shift*)))
           (high (if fixval (if (= unboxed-fixval (logand #xffff0000 unboxed-fixval)) (ash unboxed-fixval -16))))
           (low (if fixval (unless high (if (= unboxed-fixval (logand #x0000ffff unboxed-fixval)) unboxed-fixval))))
           (otherform (if (or high low) (if fix1 form2 form1))))
      (if otherform
        (let* ((other-reg (ppc2-one-untargeted-reg-form seg otherform ppc::arg_z)))
          (when vreg
            (ensuring-node-target (target vreg) 
              (if high
                (! logior-high target other-reg high)
                (! logior-low target other-reg low)))))
        (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg form1 ppc::arg_y form2 ppc::arg_z)
          (if vreg (ensuring-node-target (target vreg) (! %logior2 target r1 r2)))))   
      (^))))

;;; in a lot of (typical ?) cases, it might be possible to use a
;;; rotate-and-mask instead of andi./andis.

(defppc2 ppc2-%ilogand2 %ilogand2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (ppc2-use-operator (%nx1-operator fixnum) seg vreg xfer (logand fix1 fix2))
      (let* ((fixval (or fix1 fix2))
             (fixlen (if fixval (integer-length fixval)))
             (unboxed-fixval (if fixval (ash fixval *ppc2-target-fixnum-shift*)))
             (high (if fixval (if (= unboxed-fixval (logand #xffff0000 unboxed-fixval)) (ash unboxed-fixval -16))))
             (low (if fixval (unless high (if (= unboxed-fixval (logand #x0000ffff unboxed-fixval)) unboxed-fixval))))
             (otherform (if (or high low) (if fix1 form2 form1))))
        (if otherform
          (let* ((other-reg (ppc2-one-untargeted-reg-form seg otherform ppc::arg_z)))
            (when vreg
              (ensuring-node-target (target vreg) 
                (if high
                  (! logand-high target other-reg high)
                  (! logand-low target other-reg low)))))
          (if (and fixval (= fixlen (logcount fixval)))
            (let* ((nbits (- *ppc2-target-bits-in-word*
                             (1+ (+ *ppc2-target-fixnum-shift* fixlen))))
                   (otherreg (ppc2-one-untargeted-reg-form seg (if fix1 form2 form1) ppc::arg_z)))
            
              (if vreg (ensuring-node-target (target vreg)
                         (if (> fixval 0)
                           (! clear-left target otherreg nbits)
                           (! clear-right target otherreg (+ fixlen
                                                             *ppc2-target-fixnum-shift*))))))
          
            (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg form1 ppc::arg_y form2 ppc::arg_z)
              (if vreg (ensuring-node-target (target vreg) (! %logand2 target r1 r2))))))
        (^)))))

(defppc2 ppc2-%ilogxor2 %ilogxor2 (seg vreg xfer form1 form2)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (ppc2-use-operator (%nx1-operator fixnum) seg vreg xfer (logxor fix1 fix2)))
    (let* ((fixval (or fix1 fix2))
           (unboxed-fixval (if fixval (ash fixval *ppc2-target-fixnum-shift*)))
           (high (if fixval (if (= unboxed-fixval (logand #xffff0000 unboxed-fixval)) (ash unboxed-fixval -16))))
           (low (if fixval (unless high (if (= unboxed-fixval (logand #x0000ffff unboxed-fixval)) unboxed-fixval))))
           (otherform (if (or high low) (if fix1 form2 form1))))
      (if otherform
        (let* ((other-reg (ppc2-one-untargeted-reg-form seg otherform ppc::arg_z)))
          (when vreg
            (ensuring-node-target (target vreg) 
              (if high
                (! logxor-high target other-reg high)
                (! logxor-low target other-reg low)))))
        (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg form1 ppc::arg_y form2 ppc::arg_z)
          (if vreg (ensuring-node-target (target vreg) (! %logxor2 vreg r1 r2)))))
      (^))))

(defppc2 ppc2-%ineg %ineg (seg vreg xfer n)
  (let* ((src (ppc2-one-untargeted-reg-form seg n ppc::arg_z)))
    (when vreg
      (ensuring-node-target (target vreg)
        (if *ppc2-open-code-inline*
          (! negate-fixnum-overflow-inline target src)
          (progn
            (! negate-fixnum-overflow-ool src)
            (ppc2-copy-register seg target ($ ppc::arg_z))))))
    (^)))

(defppc2 ppc2-%%ineg %%ineg (seg vreg xfer n)
  (let* ((src (ppc2-one-untargeted-reg-form seg n ppc::arg_z)))
    (when vreg
      (ensuring-node-target (target vreg) 
        (! negate-fixnum-no-ovf target src)))
    (^)))

(defppc2 ppc2-characterp characterp (seg vreg xfer cc form)
  (ppc2-char-p seg vreg xfer cc form))

(defppc2 ppc2-struct-ref struct-ref (seg vreg xfer struct offset)
  (ppc2-vref seg vreg xfer :struct struct offset (unless *ppc2-reckless* (nx-lookup-target-uvector-subtag :struct))))

(defppc2 ppc2-struct-set struct-set (seg vreg xfer struct offset value)
  (ppc2-vset seg vreg xfer :struct  struct offset value (unless *ppc2-reckless* (nx-lookup-target-uvector-subtag :struct))))

(defppc2 ppc2-istruct-typep istruct-typep (seg vreg xfer cc form type)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
    (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg form ppc::arg_y type ppc::arg_z)
      (with-imm-target  () (target :signed-natural)
        (! istruct-typep target r1 r2)
        (ppc2-test-reg-%izerop seg vreg xfer target cr-bit true-p 0)))))


(defppc2 ppc2-lisptag lisptag (seg vreg xfer node)
  (if (null vreg)
    (ppc2-form seg vreg xfer node)
    (progn
      (ensuring-node-target (target vreg) 
        (! extract-tag-fixnum target (ppc2-one-untargeted-reg-form seg node ppc::arg_z)))
      (^))))

(defppc2 ppc2-fulltag fulltag (seg vreg xfer node)
  (if (null vreg)
    (ppc2-form seg vreg xfer node)
    (progn
      (ensuring-node-target (target vreg) 
        (! extract-fulltag-fixnum target (ppc2-one-untargeted-reg-form seg node ppc::arg_z)))
      (^))))

(defppc2 ppc2-typecode typecode (seg vreg xfer node)
  (if (null vreg)
    (ppc2-form seg vreg xfer node)
    (let* ((reg (ppc2-one-untargeted-reg-form seg node (if (eq (hard-regspec-value vreg) ppc::arg_z) 
                                                         ppc::arg_y ppc::arg_z))))
      (ensuring-node-target (target vreg) 
        (! extract-typecode-fixnum target reg ))
      (^))))

(defppc2 ppc2-setq-special setq-special (seg vreg xfer sym val)
  (let* ((symreg ($ ppc::arg_y))
         (valreg ($ ppc::arg_z)))
    (ppc2-one-targeted-reg-form seg val valreg)
    (ppc2-store-immediate seg (ppc2-symbol-value-cell sym) symreg)
    (! setq-special symreg valreg)
    (<- valreg))
  (^))


(defppc2 ppc2-local-go local-go (seg vreg xfer tag)
  (declare (ignorable xfer))
  (let* ((curstack (ppc2-encode-stack))
         (label (cadr tag))
         (deststack (caddr tag)))
    (if (not (ppc2-equal-encodings-p curstack deststack))
      (multiple-value-bind (catch cstack vstack)
                           (ppc2-decode-stack deststack)
        (ppc2-unwind-stack seg nil catch cstack vstack)))
    (-> label)
    (ppc2-unreachable-store vreg)))

(defppc2 ppc2-local-block local-block (seg vreg xfer blocktag body)
  (let* ((curstack (ppc2-encode-stack))
         (compound (ppc2-cd-compound-p xfer))
         (mvpass-p (ppc2-mvpass-p xfer))
         (need-label (if xfer (or compound mvpass-p) t))
         end-of-block
         last-cd
         (dest (if (backend-crf-p vreg) ppc::arg_z vreg)))
    (if need-label
      (setq end-of-block (backend-get-next-label)))
    (setq last-cd (if need-label (%ilogior2 (if mvpass-p $backend-mvpass-mask 0) end-of-block) xfer))
    (%rplaca blocktag (cons (cons dest last-cd) curstack))
    (if mvpass-p
      (ppc2-multiple-value-body seg body)
      (ppc2-form seg dest (if xfer last-cd) body))
    (when need-label
      (@ end-of-block)
      (if compound
        (<- dest))
      (ppc2-branch seg (logand (lognot $backend-mvpass-mask) (or xfer 0)) vreg))))

(defppc2 ppc2-%izerop %izerop (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
    (ppc2-test-%izerop seg vreg xfer form cr-bit true-p)))


(defppc2 ppc2-uvsize uvsize (seg vreg xfer v)
  (let* ((misc-reg (ppc2-one-untargeted-reg-form seg v ppc::arg_z)))
    (unless *ppc2-reckless* (! trap-unless-uvector misc-reg))
    (if vreg 
      (ensuring-node-target (target vreg)
        (! misc-element-count-fixnum target misc-reg)))
    (^)))

(defppc2 ppc2-%ilsl %ilsl (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil form1)
      (ppc2-form seg nil xfer form2))
    (let* ((const (acode-fixnum-form-p form1))
           (max (target-arch-case (:ppc32 31) (:ppc64 63))))
      (ensuring-node-target (target vreg)
        (if const
          (let* ((src (ppc2-one-untargeted-reg-form seg form2 ppc::arg_z)))
            (if (<= const max)
              (! %ilsl-c target const src)
              (!  lri target 0)))
          (multiple-value-bind (count src) (ppc2-two-untargeted-reg-forms seg form1 ppc::arg_y form2 ppc::arg_z)
            (! %ilsl target count src))))
      (^))))

(defppc2 ppc2-endp endp (seg vreg xfer cc form)
  (let* ((formreg (ppc2-one-untargeted-reg-form seg form ppc::arg_z)))
    (! trap-unless-list formreg)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
      (ppc2-compare-register-to-nil seg vreg xfer formreg  cr-bit true-p))))



(defppc2 ppc2-%code-char %code-char (seg vreg xfer c)
  (if (null vreg)
    (ppc2-form seg nil xfer c)
    (progn
      (ensuring-node-target (target vreg)
        (with-imm-target () (dest :u8)
          (! u32->char target (ppc2-one-untargeted-reg-form seg c dest))))
      (^))))

(defppc2 ppc2-%schar %schar (seg vreg xfer str idx)
  (multiple-value-bind (src unscaled-idx)
                       (ppc2-two-untargeted-reg-forms seg str ppc::arg_y idx ppc::arg_z)
    (if vreg
      (ensuring-node-target (target vreg)
        (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
          (256 (! %schar8 target src unscaled-idx))
          (t (! %schar32 target src unscaled-idx)))))
    (^)))

(defppc2 ppc2-%set-schar %set-schar (seg vreg xfer str idx char)
  (multiple-value-bind (src unscaled-idx char)
                       (ppc2-three-untargeted-reg-forms seg
                                                        str ppc::arg_x
                                                        idx ppc::arg_y
                                                        char ppc::arg_z)
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256 (! %set-schar8 src unscaled-idx char))
      (t (! %set-schar32 src unscaled-idx char)))
    (when vreg (<- char)) 
    (^)))

(defppc2 ppc2-%set-scharcode %set-scharcode (seg vreg xfer str idx char)
  (multiple-value-bind (src unscaled-idx char)
                       (ppc2-three-untargeted-reg-forms seg str ppc::arg_x idx ppc::arg_y
                                                        char ppc::arg_z)
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256 (! %set-scharcode8 src unscaled-idx char))
      (t (! %set-scharcode32 src unscaled-idx char)))
    (when vreg (<- char)) 
    (^)))

(defppc2 ppc2-%scharcode %scharcode (seg vreg xfer str idx)
  (multiple-value-bind (src unscaled-idx)
      (ppc2-two-untargeted-reg-forms seg str ppc::arg_y idx ppc::arg_z)
    (if vreg
      (ensuring-node-target (target vreg)
        (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
          (256 (! %scharcode8 target src unscaled-idx))
          (t (! %scharcode32 target src unscaled-idx)))))
    (^)))

      

(defppc2 ppc2-code-char code-char (seg vreg xfer c)
  (let* ((reg (ppc2-one-untargeted-reg-form seg c ppc::arg_z)))
    ;; Typecheck even if result unused.
    (case (arch::target-char-code-limit (backend-target-arch *target-backend*))
      (256 (! require-u8 reg))
      (t (! require-char-code reg)))
    (if vreg
      (ensuring-node-target (target vreg)
        (! fixnum->char target reg)))
    (^)))

(defppc2 ppc2-%valid-code-char %valid-code-char (seg vreg xfer c)
  (let* ((reg (ppc2-one-untargeted-reg-form seg c ppc::arg_z)))
    (when *ppc2-full-safety* (! require-char-code reg))
    (if vreg
      (ensuring-node-target (target vreg)
        (! code-char->char target reg)))
    (^)))

(defppc2 ppc2-eq eq (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
    (ppc2-compare seg vreg xfer form1 form2 cr-bit true-p)))

(defppc2 ppc2-neq neq (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
    (ppc2-compare seg vreg xfer form1 form2 cr-bit true-p)))

(defppc2 ppc2-numcmp numcmp (seg vreg xfer cc form1 form2)
  (let* ((name (ecase (cadr cc)
                 (:eq '=-2)
                 (:ne '/=-2)
                 (:lt '<-2)
                 (:le '<=-2)
                 (:gt '>-2)
                 (:ge '>=-2))))
    (if (or (ppc2-explicit-non-fixnum-type-p form1)
            (ppc2-explicit-non-fixnum-type-p form2))
      (ppc2-binary-builtin seg vreg xfer name form1 form2)
      (ppc2-inline-numcmp seg vreg xfer cc name form1 form2))))

(defun ppc2-inline-numcmp (seg vreg xfer cc name form1 form2)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
      (let* ((otherform (and (eql cr-bit ppc::ppc-eq-bit)
                             (if (eql (acode-fixnum-form-p form2) 0)
                               form1
                               (if (eql (acode-fixnum-form-p form1) 0)
                                 form2)))))
        (if otherform
          (ppc2-one-targeted-reg-form seg otherform ($ ppc::arg_z))
          (ppc2-two-targeted-reg-forms seg  form1 ($ ppc::arg_y) form2 ($ ppc::arg_z)))
        (let* ((out-of-line (backend-get-next-label))
               (done (backend-get-next-label)))
          (if otherform
            (unless (acode-fixnum-form-p otherform)
              (! branch-unless-arg-fixnum ($ ppc::arg_z) (aref *backend-labels* out-of-line)))
            (if (acode-fixnum-form-p form1)
              (! branch-unless-arg-fixnum ($ ppc::arg_z) (aref *backend-labels* out-of-line))
              (if (acode-fixnum-form-p form2)
                (! branch-unless-arg-fixnum ($ ppc::arg_y) (aref *backend-labels* out-of-line))  
                (! branch-unless-both-args-fixnums ($ ppc::arg_y) ($ ppc::arg_z) (aref *backend-labels* out-of-line)))))
          (with-imm-target () (b31-reg :natural)
            (if otherform
              (if true-p
                (! eq0->bit31 b31-reg ($ ppc::arg_z))
                (! ne0->bit31 b31-reg ($ ppc::arg_z)))
              (ecase cr-bit 
                (#. ppc::ppc-eq-bit 
                    (if true-p
                      (! eq->bit31 b31-reg ($ ppc::arg_y) ($ ppc::arg_z))
                      (! ne->bit31 b31-reg ($ ppc::arg_y) ($ ppc::arg_z))))
                (#. ppc::ppc-lt-bit
                    (if true-p
                      (! lt->bit31 b31-reg ($ ppc::arg_y) ($ ppc::arg_z))
                      (! ge->bit31 b31-reg ($ ppc::arg_y) ($ ppc::arg_z))))
                (#. ppc::ppc-gt-bit
                    (if true-p
                      (! gt->bit31 b31-reg ($ ppc::arg_y) ($ ppc::arg_z))
                      (! le->bit31 b31-reg ($ ppc::arg_y) ($ ppc::arg_z))))))
            (! lowbit->truth ($ ppc::arg_z) b31-reg)
            (-> done)
            (@ out-of-line)
            (if otherform
              (ppc2-lri seg ($ ppc::arg_y) 0))
            (let* ((index (arch::builtin-function-name-offset name))
                   (idx-subprim (ppc2-builtin-index-subprim index)))
              (! call-subprim-2 ($ ppc::arg_z) idx-subprim ($ ppc::arg_y) ($ ppc::arg_z)))
            (@ done)
            (<- ($ ppc::arg_z))
            (^)))))))
    
(defppc2 ppc2-%word-to-int %word-to-int (seg vreg xfer form)
  (if (null vreg)
    (ppc2-form seg nil xfer form)
    (progn
      (ensuring-node-target (target vreg)
        (! sign-extend-halfword target (ppc2-one-untargeted-reg-form seg form ppc::arg_z)))
      (^))))

(defppc2 ppc2-multiple-value-list multiple-value-list (seg vreg xfer form)
  (ppc2-multiple-value-body seg form)
  (! list)
  (when vreg
    (<- ppc::arg_z))
  (^))

(defppc2 ppc2-immform immediate (seg vreg xfer form)
  (ppc2-immediate seg vreg xfer form))

(defppc2 ppc2-lexical-reference lexical-reference (seg vreg xfer varnode)
  (let* ((ea-or-form (var-ea varnode)))
    (if (and (acode-punted-var-p varnode) (not (fixnump ea-or-form)))
      (ppc2-form seg vreg xfer ea-or-form)
      (let* ((cell (ppc2-lookup-var-cell varnode)))
        (if (and cell (typep cell 'lcell))
          (if (ppc2-ensure-lcell-offset cell (logand ea-or-form #xffff))
            (and nil (format t "~& could use cell ~s for var ~s" cell (var-name varnode)))
            (if (logbitp ppc2-debug-verbose-bit *ppc2-debug-mask*)
              (compiler-bug "wrong ea for lcell for var ~s: got ~d, expected ~d" 
                     (var-name varnode) (calc-lcell-offset cell) (logand ea-or-form #xffff))))
          (if (not cell)
            (when (memory-spec-p ea-or-form)
              (if (logbitp ppc2-debug-verbose-bit *ppc2-debug-mask*)
                (format t "~& no lcell for ~s." (var-name varnode))))))
        
        (unless (or (typep ea-or-form 'lreg) (fixnump ea-or-form))
          (compiler-bug "bogus ref to var ~s (~s) : ~s " varnode (var-name varnode) ea-or-form))
        (ppc2-do-lexical-reference seg vreg ea-or-form)
        (^)))))

(defppc2 ppc2-setq-lexical setq-lexical (seg vreg xfer varspec form)
  (let* ((ea (var-ea varspec)))
    ;(unless (fixnump ea) (compiler-bug "setq lexical is losing BIG"))
    (let* ((valreg (ppc2-one-untargeted-reg-form seg form (if (and (register-spec-p ea) 
                                                                   (or (null vreg) (eq ea vreg)))
                                                            ea
                                                            ppc::arg_z))))
      (ppc2-do-lexical-setq seg vreg ea valreg))
    (^)))

(defppc2 ppc2-fixnum fixnum (seg vreg xfer value)
  (if (null vreg)
    (^)
    (let* ((class (hard-regspec-class vreg))
           (mode (get-regspec-mode vreg))
           (unboxed (if (= class hard-reg-class-gpr)
                      (not (or (= hard-reg-class-gpr-mode-node mode)
                               (= hard-reg-class-gpr-mode-address mode))))))
      (if unboxed
        (ppc2-absolute-natural seg vreg xfer value)
        (if (= class hard-reg-class-crf)
          (progn
            ;(compiler-bug "Would have clobbered a GPR!")
            (ppc2-branch seg (ppc2-cd-true xfer) nil))
          (progn
            (ensuring-node-target (target vreg)
              (ppc2-absolute-natural seg target nil (ash value *ppc2-target-fixnum-shift*)))
            (^)))))))

(defppc2 ppc2-%ilogbitp %ilogbitp (seg vreg xfer cc bitnum form)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil bitnum)
      (ppc2-form seg vreg xfer form))
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
      (let* ((fixbit (acode-fixnum-form-p bitnum)))
        (if fixbit
          (let* ((reg (ppc2-one-untargeted-reg-form seg form ppc::arg_z))
                 (ppc-bit (- (1- *ppc2-target-bits-in-word*) (max (min (+ fixbit *ppc2-target-fixnum-shift*) (1- *ppc2-target-bits-in-word*)) *ppc2-target-fixnum-shift*))))
            (with-imm-temps () (bitreg)
              (! extract-constant-ppc-bit bitreg reg ppc-bit)
              (regspec-crf-gpr-case 
               (vreg dest)
               (progn
                 (! compare-signed-s16const dest bitreg 0)
                 (^ cr-bit true-p))
               (progn
                 (if true-p
                   (! invert-lowbit bitreg))
                 (ensuring-node-target (target dest)
                   (! lowbit->truth target bitreg))
                 (^)))))
          (multiple-value-bind (rbit rform) (ppc2-two-untargeted-reg-forms seg bitnum ppc::arg_y form ppc::arg_z)
             (with-imm-temps () (bitreg)
               (! extract-variable-non-insane-bit bitreg rform rbit)
               (regspec-crf-gpr-case 
               (vreg dest)
               (progn
                 (! compare-signed-s16const dest bitreg 0)
                 (^ cr-bit true-p))
               (progn
                 (if true-p
                   (! invert-lowbit bitreg))
                 (ensuring-node-target (target dest)
                   (! lowbit->truth target bitreg))
                 (^))))))))))

(defppc2 ppc2-uvref uvref (seg vreg xfer vector index)
  (ppc2-two-targeted-reg-forms seg vector ($ ppc::arg_y) index ($ ppc::arg_z))
  (! misc-ref)
  (<- ($ ppc::arg_z))
  (^))

(defppc2 ppc2-uvset uvset (seg vreg xfer vector index value)
  (ppc2-three-targeted-reg-forms seg vector ($ ppc::arg_x) index ($ ppc::arg_y) value ($ ppc::arg_z))
  (! misc-set)
  (<- ($ ppc::arg_z))
  (^))

(defppc2 ppc2-%decls-body %decls-body (seg vreg xfer form p2decls)
  (with-ppc-p2-declarations p2decls
    (ppc2-form seg vreg xfer form)))



(defppc2 ppc2-%err-disp %err-disp (seg vreg xfer arglist)
  (ppc2-set-nargs seg (ppc2-arglist seg arglist))
  (! ksignalerr)
  (ppc2-nil seg vreg xfer))


(defppc2 ppc2-local-tagbody local-tagbody (seg vreg xfer taglist body)
  (let* ((encstack (ppc2-encode-stack))
         (tagop (%nx1-operator tag-label)))
    (dolist (tag taglist)
      (rplacd tag (cons (backend-get-next-label) (cons encstack (cadr (cddr (cddr tag)))))))
    (dolist (form body)
      (if (eq (acode-operator form) tagop)
        (let ((tag (cddr form)))
          (@ (car tag)))
        (ppc2-form seg nil nil form)))
    (ppc2-nil seg vreg xfer)))

(defppc2 ppc2-call call (seg vreg xfer fn arglist &optional spread-p)
  (when (and (null vreg)
             (acode-p fn)
             (eq (acode-operator fn) (%nx1-operator immediate)))
    (let* ((name (cadr fn)))
      (when (memq name *warn-if-function-result-ignored*)
        (p2-whine *ppc2-cur-afunc*  :result-ignored name))))
  (ppc2-call-fn seg vreg xfer fn arglist spread-p))

(defppc2 ppc2-self-call self-call (seg vreg xfer arglist &optional spread-p)
  (setq arglist (ppc2-augment-arglist *ppc2-cur-afunc* arglist (if spread-p 1 $numppcargregs)))
  (ppc2-call-fn seg vreg xfer -1 arglist spread-p))


(defppc2 ppc2-lexical-function-call lexical-function-call (seg vreg xfer afunc arglist &optional spread-p)
  (ppc2-call-fn seg vreg xfer (list (%nx1-operator simple-function) afunc)
                (ppc2-augment-arglist afunc arglist (if spread-p 1 $numppcargregs))
                spread-p))

(defppc2 ppc2-builtin-call builtin-call (seg vreg xfer index arglist)
  (let* ((nargs (ppc2-arglist seg arglist))
         (tail-p (and (ppc2-tailcallok xfer) (<= nargs $numppcargregs)))
         (idx (acode-fixnum-form-p index))
         (idx-subprim (ppc2-builtin-index-subprim idx))
         (subprim
          (or idx-subprim
              (case nargs
                (0 (subprim-name->offset '.SPcallbuiltin0))
                (1 (subprim-name->offset '.SPcallbuiltin1))
                (2 (subprim-name->offset '.SPcallbuiltin2))
                (3 (subprim-name->offset '.SPcallbuiltin3))
                (t (subprim-name->offset '.SPcallbuiltin))))))
    (when tail-p
      (ppc2-restore-nvrs seg *ppc2-register-restore-ea* *ppc2-register-restore-count*)
      (ppc2-restore-full-lisp-context seg))
    (unless idx-subprim
      (! lri ppc::imm0 (ash idx *ppc2-target-fixnum-shift*))
      (when (eql subprim (subprim-name->offset '.SPcallbuiltin))
        (ppc2-set-nargs seg nargs)))
    (if tail-p
      (! jump-subprim subprim)
      (progn
        (! call-subprim subprim)
        (<- ppc::arg_z)
        (^)))))
      

(defppc2 ppc2-if if (seg vreg xfer testform true false)
  (if (nx-constant-form-p (acode-unwrapped-form testform))
    (ppc2-form seg vreg xfer (if (nx-null (acode-unwrapped-form testform)) false true))
    (let* ((cstack *ppc2-cstack*)
           (vstack *ppc2-vstack*)
           (top-lcell *ppc2-top-vstack-lcell*)
           (entry-stack (ppc2-encode-stack))
           (true-stack nil)
           (false-stack nil)
           (true-cleanup-label nil)
           (same-stack-effects nil)
           (true-is-goto (ppc2-go-label true))
           (false-is-goto (and (not true-is-goto) (ppc2-go-label false)))
           (endlabel (backend-get-next-label))
           (falselabel (backend-get-next-label))
           (need-else (unless false-is-goto (or (not (nx-null false)) (ppc2-for-value-p vreg))))
           (both-single-valued (and (not *ppc2-open-code-inline*)
                                    (eq xfer $backend-return)
                                    (ppc2-for-value-p vreg)
                                    need-else
                                    (ppc2-single-valued-form-p true) 
                                    (ppc2-single-valued-form-p false))))
      (if (eq 0 xfer) 
        (setq xfer nil))
      (if both-single-valued            ; it's implied that we're returning
        (let* ((result ppc::arg_z))
          (let ((merge-else-branch-label (if (nx-null false) (ppc2-find-nilret-label))))
            (ppc2-conditional-form seg (ppc2-make-compound-cd 0 falselabel) testform)
            (ppc2-form seg result endlabel true)
            (if (and merge-else-branch-label (neq -1 (aref *backend-labels* merge-else-branch-label)))
              (backend-copy-label merge-else-branch-label falselabel)
              (progn
                (@ falselabel)
                (if (nx-null false) (@ (ppc2-record-nilret-label)))
                (ppc2-form seg result nil false)))
            (@ endlabel)
            (<- result)
            (^)))
        (progn
          (if (and need-else (ppc2-mvpass-p xfer))
            (setq true-cleanup-label (backend-get-next-label)))         
          (ppc2-conditional-form 
           seg
           (ppc2-make-compound-cd 
            (or true-is-goto 0)
            (or false-is-goto 
                (if need-else 
                  (if true-is-goto 0 falselabel) 
                  (if true-is-goto xfer (ppc2-cd-merge xfer falselabel))))) 
           testform)  
          (if true-is-goto
            (ppc2-unreachable-store)
            (if true-cleanup-label
              (progn
                (ppc2-open-undo $undomvexpect)
                (ppc2-form seg vreg (logior $backend-mvpass-mask true-cleanup-label) true))
              (ppc2-form seg vreg (if need-else (ppc2-cd-merge xfer endlabel) xfer) true)))
          (setq true-stack (ppc2-encode-stack))
          (setq *ppc2-cstack* cstack)
          (ppc2-set-vstack vstack)
          (setq *ppc2-top-vstack-lcell* top-lcell)
          (if false-is-goto (ppc2-unreachable-store))
          (let ((merge-else-branch-label (if (and (nx-null false) (eq xfer $backend-return)) (ppc2-find-nilret-label))))
            (if (and merge-else-branch-label (neq -1 (aref *backend-labels* merge-else-branch-label)))
              (backend-copy-label merge-else-branch-label falselabel)
              (progn
                (@ falselabel)
                (when need-else
                  (if true-cleanup-label
                    (ppc2-mvpass seg false)
                    (ppc2-form seg vreg xfer false))
                  (setq false-stack (ppc2-encode-stack))))))
          (when true-cleanup-label
            (if (setq same-stack-effects (ppc2-equal-encodings-p true-stack false-stack)) ; can share cleanup code
              (@ true-cleanup-label))
            (let* ((*ppc2-returning-values* :pass))
              (ppc2-nlexit seg xfer 1)
              (ppc2-branch seg (if (and xfer (neq xfer $backend-mvpass-mask)) xfer (if (not same-stack-effects) endlabel)) vreg))
            (unless same-stack-effects
              (@ true-cleanup-label)
              (multiple-value-setq (true *ppc2-cstack* *ppc2-vstack* *ppc2-top-vstack-lcell*)
                (ppc2-decode-stack true-stack))
              (let* ((*ppc2-returning-values* :pass))
                (ppc2-nlexit seg xfer 1)
                (^)))
            (ppc2-close-undo)
            (multiple-value-setq (*ppc2-undo-count* *ppc2-cstack* *ppc2-vstack* *ppc2-top-vstack-lcell*) 
              (ppc2-decode-stack entry-stack)))
          (@ endlabel))))))

(defppc2 ppc2-or or (seg vreg xfer forms)
  (let* ((mvpass (ppc2-mvpass-p xfer))
         (tag1 (backend-get-next-label))
         (tag2 (backend-get-next-label))
         (vstack *ppc2-vstack*)
         (cstack *ppc2-cstack*)
         (dest (if (backend-crf-p vreg) vreg (if vreg ppc::arg_z (available-crf-temp *available-backend-crf-temps*))))
         (cd1 (ppc2-make-compound-cd 
               (if (eq dest ppc::arg_z) tag1 (ppc2-cd-merge (ppc2-cd-true xfer) tag1)) 0)))
    (while (cdr forms)
      (ppc2-form seg dest (if (eq dest ppc::arg_z) nil cd1) (car forms))
      (when (eq dest ppc::arg_z)
        (with-crf-target () val-crf
          (ppc2-copy-register seg val-crf dest)
          (ppc2-branch seg cd1 val-crf)))
      (setq forms (%cdr forms)))
    (if mvpass
      (progn (ppc2-multiple-value-body seg (car forms)) 
             (let* ((*ppc2-returning-values* t)) (ppc2-branch seg (ppc2-cd-merge xfer tag2) vreg)))
      (ppc2-form seg  vreg (if (eq dest ppc::arg_z) (ppc2-cd-merge xfer tag2) xfer) (car forms)))
    (setq *ppc2-vstack* vstack *ppc2-cstack* cstack)
    (@ tag1)
    (when (eq dest ppc::arg_z)
      (<- ppc::arg_z)
      (^))
    (@ tag2)))

(defppc2 ppc2-simple-function simple-function (seg vreg xfer afunc)
  (ppc2-immediate seg vreg xfer (ppc2-afunc-lfun-ref afunc)))

(defppc2 ppc2-list list (seg vreg xfer arglist)
  (if (null vreg)
    (dolist (form arglist)
      (ppc2-form seg vreg nil form)) 
    (let* ((*ppc2-vstack* *ppc2-vstack*)
           (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
           (nargs (ppc2-formlist seg arglist nil)))
      (ppc2-set-nargs seg nargs)
      (! list)
      (<- ppc::arg_z)))
  (^))

(defppc2 ppc2-list* list* (seg vreg xfer arglist)
  (if (null vreg)
    (dolist (arg (apply #'append arglist))
      (ppc2-form seg nil nil arg))
    (let* ((*ppc2-vstack* *ppc2-vstack*)
           (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
           (nargs (ppc2-arglist seg arglist)))
      (declare (fixnum nargs))
      (when (> nargs 1)
        (ppc2-set-nargs seg (1- nargs))
        (! list*))
      (<- ppc::arg_z)))
  (^))

(defppc2 ppc2-minus1 minus1 (seg vreg xfer form)
  (ppc2-unary-builtin seg vreg xfer '%negate form))

(defun ppc2-inline-add2 (seg vreg xfer form1 form2)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (ppc2-two-targeted-reg-forms seg form1 ($ ppc::arg_y) form2 ($ ppc::arg_z))
    (let* ((out-of-line (backend-get-next-label))
           (done (backend-get-next-label)))
      (ensuring-node-target (target vreg)
        (if (acode-fixnum-form-p form1)
          (! branch-unless-arg-fixnum ($ ppc::arg_z) (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (! branch-unless-arg-fixnum ($ ppc::arg_y) (aref *backend-labels* out-of-line))  
            (! branch-unless-both-args-fixnums ($ ppc::arg_y) ($ ppc::arg_z) (aref *backend-labels* out-of-line))))
        (if *ppc2-open-code-inline*
          (! fixnum-add-overflow-inline-skip ($ ppc::arg_z) ($ ppc::arg_y) ($ ppc::arg_z) (aref *backend-labels* done))
          (progn
            (! fixnum-add-overflow-ool ($ ppc::arg_y) ($ ppc::arg_z))
            (-> done)))
        (@ out-of-line)
        (! call-subprim-2 ($ ppc::arg_z) (subprim-name->offset '.SPbuiltin-plus) ($ ppc::arg_y) ($ ppc::arg_z))
        (@ done)
        (ppc2-copy-register seg target ($ ppc::arg_z)))
      (^))))

(defun ppc2-inline-sub2 (seg vreg xfer form1 form2)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (ppc2-two-targeted-reg-forms seg form1 ($ ppc::arg_y) form2 ($ ppc::arg_z))
    (let* ((out-of-line (backend-get-next-label))
           (done (backend-get-next-label)))
      (ensuring-node-target (target vreg)
        (if (acode-fixnum-form-p form1)
          (! branch-unless-arg-fixnum ($ ppc::arg_z) (aref *backend-labels* out-of-line))
          (if (acode-fixnum-form-p form2)
            (! branch-unless-arg-fixnum ($ ppc::arg_y) (aref *backend-labels* out-of-line))  
            (! branch-unless-both-args-fixnums ($ ppc::arg_y) ($ ppc::arg_z) (aref *backend-labels* out-of-line))))
        (if *ppc2-open-code-inline*
          (! fixnum-sub-overflow-inline-skip ($ ppc::arg_z) ($ ppc::arg_y) ($ ppc::arg_z) (aref *backend-labels* done))
          (progn
            (! fixnum-sub-overflow-ool ($ ppc::arg_y) ($ ppc::arg_z))
            (-> done)))
        (@ out-of-line)
        (! call-subprim-2 ($ ppc::arg_z) (subprim-name->offset '.SPbuiltin-minus) ($ ppc::arg_y) ($ ppc::arg_z))
        (@ done)
        (ppc2-copy-register seg target ($ ppc::arg_z)))
      (^))))

;;; Return T if form is declared to be something that couldn't be a fixnum.
(defun ppc2-explicit-non-fixnum-type-p (form)
  (let* ((type (ppc2-form-type form))
         (target-fixnum-type (nx-target-type 'fixnum)))
    (and (not (subtypep type target-fixnum-type))
         (not (subtypep target-fixnum-type type)))))


    

(defppc2 ppc2-add2 add2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *ppc2-trust-declarations*)
    (if (and (ppc2-form-typep form1 'double-float)
             (ppc2-form-typep form2 'double-float))
      (ppc2-use-operator (%nx1-operator %double-float+-2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (ppc2-form-typep form1 'single-float)
               (ppc2-form-typep form2 'single-float))
        (ppc2-use-operator (%nx1-operator %short-float+-2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
        (if (and (ppc2-form-typep form1 'fixnum)
                 (ppc2-form-typep form2 'fixnum))
          (ppc2-use-operator (%nx1-operator %i+)
                             seg
                             vreg
                             xfer
                             form1
                             form2
                             t)
          (if (or (ppc2-explicit-non-fixnum-type-p form1)
                  (ppc2-explicit-non-fixnum-type-p form2))
            (ppc2-binary-builtin seg vreg xfer '+-2 form1 form2)
            (ppc2-inline-add2 seg vreg xfer form1 form2)))))))

(defppc2 ppc2-sub2 sub2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *ppc2-trust-declarations*)
    (if (and (ppc2-form-typep form1 'double-float)
             (ppc2-form-typep form2 'double-float))
      (ppc2-use-operator (%nx1-operator %double-float--2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (ppc2-form-typep form1 'single-float)
               (ppc2-form-typep form2 'single-float))
        (ppc2-use-operator (%nx1-operator %short-float--2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
        (if (and (ppc2-form-typep form1 'fixnum)
                 (ppc2-form-typep form2 'fixnum))
          (ppc2-use-operator (%nx1-operator %i-)
                             seg
                             vreg
                             xfer
                             form1
                             form2
                             t)
          (if (or (ppc2-explicit-non-fixnum-type-p form1)
                  (ppc2-explicit-non-fixnum-type-p form2))
            (ppc2-binary-builtin seg vreg xfer '--2 form1 form2)
            (ppc2-inline-sub2 seg vreg xfer form1 form2)))))))

(defppc2 ppc2-mul2 mul2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *ppc2-trust-declarations*)
    (if (and (ppc2-form-typep form1 'double-float)
             (ppc2-form-typep form2 'double-float))
      (ppc2-use-operator (%nx1-operator %double-float*-2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (ppc2-form-typep form1 'single-float)
               (ppc2-form-typep form2 'single-float))
        (ppc2-use-operator (%nx1-operator %short-float*-2)
                           seg
                           vreg
                           xfer
                           form1
                           form2)
        (ppc2-binary-builtin seg vreg xfer '*-2 form1 form2)))))


(defppc2 ppc2-div2 div2 (seg vreg xfer form1 form2)
  (multiple-value-bind (form1 form2)
      (nx-binop-numeric-contagion form1 form2 *ppc2-trust-declarations*)
    (if (and (ppc2-form-typep form1 'double-float)
             (ppc2-form-typep form2 'double-float))
      (ppc2-use-operator (%nx1-operator %double-float/-2)
                         seg
                         vreg
                         xfer
                         form1
                         form2)
      (if (and (ppc2-form-typep form1 'single-float)
               (ppc2-form-typep form2 'single-float))
        (ppc2-use-operator (%nx1-operator %short-float/-2)
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
            (ppc2-use-operator (%nx1-operator mul2)
                               seg
                               vreg
                               xfer
                               (make-acode (%nx1-operator fixnum) f1/f2)
                               (caddr unwrapped))
            (ppc2-binary-builtin seg vreg xfer '/-2 form1 form2)))))))

(defppc2 ppc2-logbitp logbitp (seg vreg xfer bitnum int)
  (ppc2-binary-builtin seg vreg xfer 'logbitp bitnum int))


(defun ppc2-inline-logior2 (seg vreg xfer form1 form2)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((fix1 (acode-fixnum-form-p form1))
           (fix2 (acode-fixnum-form-p form2)))
      (if (and fix1 fix2)
        (ppc2-use-operator (%nx1-operator fixnum) seg vreg xfer (logior fix1 fix2))
        (let* ((fixval (or fix1 fix2))
               (unboxed-fixval (if fixval (ash fixval *ppc2-target-fixnum-shift*)))
               (high (if fixval (if (= unboxed-fixval (logand #xffff0000 unboxed-fixval)) (ash unboxed-fixval -16))))
               (low (if fixval (unless high (if (= unboxed-fixval (logand #x0000ffff unboxed-fixval)) unboxed-fixval))))
               (otherform (if (or high low) (if fix1 form2 form1)))
               (out-of-line (backend-get-next-label))
               (done (backend-get-next-label)))

          (if otherform
            (ppc2-one-targeted-reg-form seg otherform ($ ppc::arg_z))
            (ppc2-two-targeted-reg-forms seg form1 ($ ppc::arg_y) form2 ($ ppc::arg_z)))
          (ensuring-node-target (target vreg)
            (if otherform
              (unless (acode-fixnum-form-p otherform)
                (! branch-unless-arg-fixnum ($ ppc::arg_z) (aref *backend-labels* out-of-line)))
              (if (acode-fixnum-form-p form1)
                (! branch-unless-arg-fixnum ($ ppc::arg_z) (aref *backend-labels* out-of-line))
                (if (acode-fixnum-form-p form2)
                  (! branch-unless-arg-fixnum ($ ppc::arg_y) (aref *backend-labels* out-of-line))  
                  (! branch-unless-both-args-fixnums ($ ppc::arg_y) ($ ppc::arg_z) (aref *backend-labels* out-of-line)))))
            (if otherform
              (if high
                (! logior-high ($ ppc::arg_z) ($ ppc::arg_z) high)
                (! logior-low ($ ppc::arg_z) ($ ppc::arg_z) low))
              (! %logior2 ($ ppc::arg_z) ($ ppc::arg_z) ($ ppc::arg_y)))
            (-> done)
            (@ out-of-line)
            (if otherform
              (ppc2-lri seg ($ ppc::arg_y) (ash fixval *ppc2-target-fixnum-shift*)))
            (! call-subprim-2 ($ ppc::arg_z) (subprim-name->offset '.SPbuiltin-logior) ($ ppc::arg_y) ($ ppc::arg_z))
            (@ done)
            (ppc2-copy-register seg target ($ ppc::arg_z)))
          (^))))))

(defppc2 ppc2-logior2 logior2 (seg vreg xfer form1 form2)
  (if (or (ppc2-explicit-non-fixnum-type-p form1)
          (ppc2-explicit-non-fixnum-type-p form2))
    (ppc2-binary-builtin seg vreg xfer 'logior-2 form1 form2)
    (ppc2-inline-logior2 seg vreg xfer form1 form2)))

(defppc2 ppc2-logxor2 logxor2 (seg vreg xfer form1 form2)
  (ppc2-binary-builtin seg vreg xfer 'logxor-2 form1 form2))

(defun ppc2-inline-logand2 (seg vreg xfer form1 form2)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
  (let* ((fix1 (acode-fixnum-form-p form1))
         (fix2 (acode-fixnum-form-p form2)))
    (if (and fix1 fix2)
      (ppc2-use-operator (%nx1-operator fixnum) seg vreg xfer (logand fix1 fix2))
      (let* ((fixval (or fix1 fix2))
             (fixlen (if fixval (integer-length fixval)))
             (unboxed-fixval (if fixval (ash fixval *ppc2-target-fixnum-shift*)))
             (high (if fixval (if (= unboxed-fixval (logand #xffff0000 unboxed-fixval)) (ash unboxed-fixval -16))))
             (low (if fixval (unless high (if (= unboxed-fixval (logand #x0000ffff unboxed-fixval)) unboxed-fixval))))
             (maskable (and fixval (= fixlen (logcount fixval))))
             (otherform (if (or high low maskable) (if fix1 form2 form1)))
             (out-of-line (backend-get-next-label))
             (done (backend-get-next-label)))
        (if otherform
          (ppc2-one-targeted-reg-form seg otherform ($ ppc::arg_z))
          (ppc2-two-targeted-reg-forms  seg form1 ($ ppc::arg_y) form2 ($ ppc::arg_z)))
        (ensuring-node-target (target vreg)
          (if otherform
            (unless (acode-fixnum-form-p otherform)
              (! branch-unless-arg-fixnum ($ ppc::arg_z) (aref *backend-labels* out-of-line)))
            (if (acode-fixnum-form-p form1)
              (! branch-unless-arg-fixnum ($ ppc::arg_z) (aref *backend-labels* out-of-line))
              (if (acode-fixnum-form-p form2)
                (! branch-unless-arg-fixnum ($ ppc::arg_y) (aref *backend-labels* out-of-line))  
                (! branch-unless-both-args-fixnums ($ ppc::arg_y) ($ ppc::arg_z) (aref *backend-labels* out-of-line)))))
          (if otherform
            (if (or high low)
              (if high
                (! logand-high ($ ppc::arg_z) ($ ppc::arg_z) high)
                (! logand-low ($ ppc::arg_z) ($ ppc::arg_z) low))
              (let* ((nbits (- *ppc2-target-bits-in-word*
                             (1+ (+ *ppc2-target-fixnum-shift* fixlen)))))
                (if (> fixval 0)
                  (! clear-left ($ ppc::arg_z) ($ ppc::arg_z)  nbits)
                  (! clear-right ($ ppc::arg_z) ($ ppc::arg_z) (+ fixlen
                                                                  *ppc2-target-fixnum-shift*)))))
            (! %logand2 ($ ppc::arg_z) ($ ppc::arg_z) ($ ppc::arg_y)))
          (-> done)
          (@ out-of-line)
          (if otherform
            (ppc2-lri seg ($ ppc::arg_y) (ash fixval *ppc2-target-fixnum-shift*)))
            (! call-subprim-2 ($ ppc::arg_z) (subprim-name->offset '.SPbuiltin-logand) ($ ppc::arg_y) ($ ppc::arg_z))          
            (@ done)
            (ppc2-copy-register seg target ($ ppc::arg_z)))
        (^))))))

(defppc2 ppc2-logand2 logand2 (seg vreg xfer form1 form2)
  (if (or (ppc2-explicit-non-fixnum-type-p form1)
          (ppc2-explicit-non-fixnum-type-p form2))
    (ppc2-binary-builtin seg vreg xfer 'logand-2 form1 form2)
    (ppc2-inline-logand2 seg vreg xfer form1 form2)))



(defppc2 ppc2-%aref1 %aref1 (seg vreg xfer v i)
  (let* ((vtype (acode-form-type v t))
         (atype (if vtype (specifier-type vtype)))
         (keyword (if (and atype
                           (not (array-ctype-complexp atype)))
                    (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (if keyword
      (ppc2-vref  seg vreg xfer keyword v i (not *ppc2-reckless*))
      (ppc2-binary-builtin seg vreg xfer '%aref1 v i))))

(defppc2 ppc2-%aset1 aset1 (seg vreg xfer v i n)
  (let* ((vtype (acode-form-type v t))
         (atype (if vtype (specifier-type vtype)))
         (keyword (if (and atype
                           (not (array-ctype-complexp atype)))
                    (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (if keyword
      (ppc2-vset seg vreg xfer keyword v i n (not *ppc2-reckless*))
      (ppc2-ternary-builtin seg vreg xfer '%aset1 v i n))))

(defppc2 ppc2-%i+ %i+ (seg vreg xfer form1 form2 &optional overflow)
  (when overflow
    (let* ((type *ppc2-target-half-fixnum-type*))
      (when (and (ppc2-form-typep form1 type)
                 (ppc2-form-typep form2 type))
        (setq overflow nil))))
  (cond ((null vreg) 
         (ppc2-form seg nil nil form1) 
         (ppc2-form seg nil xfer form2))
        (overflow
         (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg form1 ppc::arg_y form2 ppc::arg_z)
           (ensuring-node-target (target vreg)
             (if *ppc2-open-code-inline*
               (! fixnum-add-overflow-inline target r1 r2)
               (progn
                 (! fixnum-add-overflow-ool r1 r2)
                 (ppc2-copy-register seg target ($ ppc::arg_z)))))
           (^)))
        (t                              
         ;; There isn't any "addi" that checks for overflow, which is
         ;; why we didn't bother.
         (let* ((fix1 (acode-fixnum-form-p form1))
                (fix2 (acode-fixnum-form-p form2))
                (other (if (and fix1
                                (typep (ash fix1 *ppc2-target-fixnum-shift*)
                                       '(signed-byte 32)))
                         form2
                         (if (and fix2
                                  (typep (ash fix2 *ppc2-target-fixnum-shift*)
                                              '(signed-byte 32)))
                           form1))))
           (if (and fix1 fix2)
             (ppc2-lri seg vreg (ash (+ fix1 fix2) *ppc2-target-fixnum-shift*))
             (if other
               (let* ((constant (ash (or fix1 fix2) *ppc2-target-fixnum-shift*))
                      (reg (ppc2-one-untargeted-reg-form seg other ppc::arg_z))
                      (high (ldb (byte 16 16) constant))
                      (low (ldb (byte 16 0) constant)))
                 (declare (fixnum high low))
                 (if (zerop constant)
                   (<- reg)
                   (progn
                     (if (logbitp 15 low) (setq high (ldb (byte 16 0) (1+ high))))
                     (if (and (eq vreg reg) (not (zerop high)))
                       (with-node-temps (vreg) (temp)
                         (! add-immediate temp reg high low)
                         (<- temp))
                       (ensuring-node-target (target vreg)
                         (! add-immediate target reg high low))))))
               (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg form1 ppc::arg_y form2 ppc::arg_z)
                 (ensuring-node-target (target vreg)
                   (! fixnum-add target r1 r2)))))
           (^)))))

(defppc2 ppc2-%i- %i- (seg vreg xfer num1 num2 &optional overflow)
  (when overflow
    (let* ((type *ppc2-target-half-fixnum-type*))
      (when (and (ppc2-form-typep num1 type)
                 (ppc2-form-typep num2 type))
        (setq overflow nil))))
  (let* ((v1 (acode-fixnum-form-p num1))
         (v2 (acode-fixnum-form-p num2)))
    (if (and v1 v2)
      (ppc2-use-operator (%nx1-operator fixnum) seg vreg xfer (%i- v1 v2))
      (if (and v2 (neq v2 most-negative-fixnum))
        (ppc2-use-operator (%nx1-operator %i+) seg vreg xfer num1 (make-acode (%nx1-operator fixnum) (- v2)) overflow) 
        (if (eq v2 0)
          (ppc2-form seg vreg xfer num1)
          (cond
           ((null vreg)
            (ppc2-form seg nil nil num1)
            (ppc2-form seg nil xfer num2))
           (overflow
            (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg num1 ppc::arg_y num2 ppc::arg_z)
               (ensuring-node-target (target vreg)
                 (if *ppc2-open-code-inline*
                   (! fixnum-sub-overflow-inline target r1 r2)
                   (progn
                     (! fixnum-sub-overflow-ool r1 r2)
                     (ppc2-copy-register seg target ($ ppc::arg_z)))))
              (^)))
           ((and v1 (<= (integer-length v1) (- 15 *ppc2-target-fixnum-shift*)))
            (ensuring-node-target (target vreg)
              (! fixnum-sub-from-constant target v1 (ppc2-one-untargeted-reg-form seg num2 ppc::arg_z)))
            (^))
           (t
            (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg num1 ppc::arg_y num2 ppc::arg_z)
              (ensuring-node-target (target vreg)
                (! fixnum-sub target r1 r2))
              (^)))))))))

(defppc2 ppc2-%i* %i* (seg vreg xfer num1 num2)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil num1)
      (ppc2-form seg nil xfer num2))  
    (let* ((fix1 (acode-fixnum-form-p num1))
           (fix2 (acode-fixnum-form-p num2))
           (other (if (typep fix1 '(signed-byte 16)) num2 (if (typep fix2 '(signed-byte 16)) num1))))
      (if (and fix1 fix2)
        (ppc2-lri seg vreg (ash (* fix1 fix2) *ppc2-target-fixnum-shift*))
        (if other
          (! multiply-immediate vreg (ppc2-one-untargeted-reg-form seg other ppc::arg_z) (or fix1 fix2))
          (multiple-value-bind (rx ry) (ppc2-two-untargeted-reg-forms seg num1 ppc::arg_y num2 ppc::arg_z)
            (ensuring-node-target (target vreg)
              (! multiply-fixnums target rx ry)))))
      (^))))

(defppc2 ppc2-nth-value nth-value (seg vreg xfer n form)
  (let* ((*ppc2-vstack* *ppc2-vstack*)
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
    (let* ((nreg (ppc2-one-untargeted-reg-form seg n ppc::arg_z)))
      (unless (acode-fixnum-form-p n)
        (! trap-unless-fixnum nreg))
      (ppc2-vpush-register seg nreg))
     (ppc2-multiple-value-body seg form) ; sets nargs
    (! nth-value ppc::arg_z))
  (<- ppc::arg_z)
  (^))

(defppc2 ppc2-values values (seg vreg xfer forms)
  (if (eq (list-length forms) 1)
    (if (ppc2-cd-compound-p xfer)
      (ppc2-form seg vreg xfer (%car forms))
      (progn
        (ppc2-form seg vreg nil (%car forms))
        (^)))
    (if (not (ppc2-mv-p xfer))
      (if forms
        (ppc2-use-operator (%nx1-operator prog1) seg vreg xfer forms)
        (ppc2-nil seg vreg xfer))
      (progn
        (let* ((*ppc2-vstack* *ppc2-vstack*)
               (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
          (ppc2-set-nargs seg (ppc2-formlist seg forms nil)))
        (let* ((*ppc2-returning-values* t))
          (^))))))

(defppc2 ppc2-base-char-p base-char-p (seg vreg xfer cc form)
  (ppc2-char-p seg vreg xfer cc form))

(defun ppc2-char-p (seg vreg xfer cc form)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
      (! mask-base-char ppc::imm0 (ppc2-one-untargeted-reg-form seg form ppc::arg_z))
      (ppc2-test-reg-%izerop seg vreg xfer ppc::imm0 cr-bit true-p
                             (target-arch-case
                              (:ppc32 ppc32::subtag-character)
                              (:ppc64 ppc64::subtag-character))))))


(defppc2 ppc2-let* let* (seg vreg xfer vars vals body p2decls &aux
                             (old-stack (ppc2-encode-stack)))
  (ppc2-check-lcell-depth)
  (with-ppc-p2-declarations p2decls
    (ppc2-seq-bind seg vars vals)
    (ppc2-undo-body seg vreg xfer body old-stack))
  (dolist (v vars) (ppc2-close-var seg v)))

(defppc2 ppc2-multiple-value-bind multiple-value-bind (seg vreg xfer vars valform body p2decls)
  (let* ((n (list-length vars))
         (vloc *ppc2-vstack*)
         (nbytes (* n *ppc2-target-node-size*))
         (old-stack (ppc2-encode-stack)))
    (with-ppc-p2-declarations p2decls
      (ppc2-multiple-value-body seg valform)
      (ppc2-lri seg ppc::imm0 nbytes)
      (! fitvals)
      (ppc2-set-vstack (%i+ vloc nbytes))
      (let* ((old-top *ppc2-top-vstack-lcell*)
             (lcells (progn (ppc2-reserve-vstack-lcells n) (ppc2-collect-lcells :reserved old-top))))
        (dolist (var vars)
          (let* ((lcell (pop lcells))
                 (reg (ppc2-assign-register-var var)))
            (if reg
              (ppc2-init-regvar seg var reg (ppc2-vloc-ea vloc))
              (ppc2-bind-var seg var vloc lcell))          
            (setq vloc (%i+ vloc *ppc2-target-node-size*)))))
      (ppc2-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (ppc2-close-var seg var)))))

(defppc2 ppc2-debind debind (seg vreg xfer lambda-list bindform req opt rest keys auxen whole body p2decls cdr-p)
  (declare (ignore lambda-list))
  (let* ((old-stack (ppc2-encode-stack))
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
         (vloc *ppc2-vstack*))
    (with-ppc-p2-declarations p2decls      
      (ppc2-bind-structured-lambda
       seg 
       (ppc2-spread-lambda-list seg bindform whole req opt rest keys nil cdr-p)
       vloc (ppc2-vloc-ea vloc) whole req opt rest keys auxen)
      (ppc2-undo-body seg vreg xfer body old-stack)
      (ppc2-close-structured-lambda seg whole req opt rest keys auxen))))

(defppc2 ppc2-multiple-value-prog1 multiple-value-prog1 (seg vreg xfer forms)
  (if (or (not (ppc2-mv-p xfer)) (ppc2-single-valued-form-p (%car forms)))
    (ppc2-use-operator (%nx1-operator prog1) seg vreg xfer forms)
    (if (null (cdr forms))
      (ppc2-form seg vreg xfer(car forms))
      (progn
        (let* ((*ppc2-vstack* *ppc2-vstack*)
               (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
          (ppc2-multiple-value-body seg (%car forms))
          (ppc2-open-undo $undostkblk)
          (! save-values))
        (dolist (form (cdr forms))
          (ppc2-form seg nil nil form))
        (ppc2-set-nargs seg 0)
        (! recover-values)
        (ppc2-close-undo)
        (let* ((*ppc2-returning-values* t))
          (^))))))

(defppc2 ppc2-not not (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
  (ppc2-compare-register-to-nil
   seg 
   vreg 
   xfer
   (ppc2-one-untargeted-reg-form seg form ppc::arg_z) 
   cr-bit
   true-p)))


(defppc2 ppc2-%alloc-misc %make-uvector (seg vreg xfer element-count st &optional initval)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil element-count)
      (ppc2-form seg nil xfer st))
    (let* ((subtag (acode-fixnum-form-p st))
           (nelements (acode-fixnum-form-p element-count))         
           (nbytes (if (and subtag nelements) (ppc2-misc-byte-count subtag nelements))))
      (if (and  nbytes (null initval)
                (< (logand
                    (lognot (1- (* 2 *ppc2-target-node-size*)))
                    (+ nbytes *ppc2-target-node-size*
                       (1- (* 2 *ppc2-target-node-size*)))) #x8000))
        (with-imm-temps () (header)
          (ppc2-lri seg header (arch::make-vheader nelements subtag))
          (ensuring-node-target (target vreg)
            (! %alloc-misc-fixed target header nbytes)))
        (progn
          (if initval
            (progn
              (ppc2-three-targeted-reg-forms seg element-count ($ ppc::arg_x) st ($ ppc::arg_y) initval ($ ppc::arg_z))
              (! misc-alloc-init)
              (<- ($ ppc::arg_z)))
            (progn
              (ppc2-two-targeted-reg-forms seg element-count ($ ppc::arg_y) st ($ ppc::arg_z))
              (! misc-alloc)
              (<- ($ ppc::arg_z))))))
        (^))))

(defppc2 ppc2-%iasr %iasr (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil form1)
      (ppc2-form seg vreg xfer form2))
    (let* ((count (acode-fixnum-form-p form1))
           (max (target-arch-case (:ppc32 31) (:ppc64 63))))
      (declare (fixnum max))
      (ensuring-node-target (target vreg)
        (if count
          (! %iasr-c target (if (> count max) max count)
             (ppc2-one-untargeted-reg-form seg form2 ppc::arg_z))
          (multiple-value-bind (cnt src) (ppc2-two-targeted-reg-forms seg form1 ($ ppc::arg_y) form2 ($ ppc::arg_z))
            (! %iasr target cnt src))))
      (^))))

(defppc2 ppc2-%ilsr %ilsr (seg vreg xfer form1 form2)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil form1)
      (ppc2-form seg vreg xfer form2))
    (let* ((count (acode-fixnum-form-p form1)))
      (ensuring-node-target (target vreg)
        (if count
          (let ((src (ppc2-one-untargeted-reg-form seg form2 ($ ppc::arg_z))))
            (if (<= count 31)
              (! %ilsr-c target count src)
              (!  lri target 0)))
          (multiple-value-bind (cnt src) (ppc2-two-targeted-reg-forms seg form1 ($ ppc::arg_y) form2 ($ ppc::arg_z))
            (! %ilsr target cnt src))))
      (^))))


(defppc2 ppc2-%i<> %i<> (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
    (ppc2-compare seg vreg xfer form1 form2 cr-bit true-p)))

(defppc2 ppc2-%natural<> %natural<> (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
    (ppc2-natural-compare seg vreg xfer form1 form2 cr-bit true-p)))

(defppc2 ppc2-double-float-compare double-float-compare (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
    (with-fp-target () (r1 :double-float)
      (with-fp-target (r1) (r2 :double-float)
        (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg form1 r1 form2 r2)
          (ppc2-compare-double-float-registers seg vreg xfer r1 r2 cr-bit true-p))))))

(defppc2 ppc2-short-float-compare short-float-compare (seg vreg xfer cc form1 form2)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
    (with-fp-target () (r1 :single-float)
      (with-fp-target (r1) (r2 :single-float)
        (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg form1 r1 form2 r2)
          (ppc2-compare-double-float-registers seg vreg xfer r1 r2 cr-bit true-p))))))
 
(eval-when (:compile-toplevel :execute)
  (defmacro defppc2-df-op (fname opname vinsn)
    `(defppc2 ,fname ,opname (seg vreg xfer f0 f1)
       (if (null vreg)
         (progn
           (ppc2-form seg nil nil f0)
           (ppc2-form seg vreg xfer f1))
         (with-fp-target () (r1 :double-float)
           (with-fp-target (r1) (r2 :double-float)
             (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg f0 r1 f1 r2)
               (if (= (hard-regspec-class vreg) hard-reg-class-fpr)
                 (! ,vinsn vreg r1 r2)
                 (with-fp-target (r1 r2) (result :double-float)
                   (! ,vinsn result r1 r2)
                   (ensuring-node-target (target vreg)
                     (ppc2-copy-register seg target result))))
               (^)))))))
  
  (defmacro defppc2-sf-op (fname opname vinsn)
    `(defppc2 ,fname ,opname (seg vreg xfer f0 f1)
       (if (null vreg)
         (progn
           (ppc2-form seg nil nil f0)
           (ppc2-form seg vreg xfer f1))
         (with-fp-target () (r1 :single-float)
           (with-fp-target (r1) (r2 :single-float)
             (multiple-value-bind (r1 r2) (ppc2-two-untargeted-reg-forms seg f0 r1 f1 r2)
               (if (= (hard-regspec-class vreg) hard-reg-class-fpr)
		 (! ,vinsn vreg r1 r2)
                 (with-fp-target (r1 r2) (result :single-float)
                   (! ,vinsn result r1 r2)
                   (ensuring-node-target (target vreg)
                     (ppc2-copy-register seg target result))))
               (^)))))))
)

(defppc2-df-op ppc2-%double-float+-2 %double-float+-2 double-float+-2)
(defppc2-df-op ppc2-%double-float--2 %double-float--2 double-float--2)
(defppc2-df-op ppc2-%double-float*-2 %double-float*-2 double-float*-2)
(defppc2-df-op ppc2-%double-float/-2 %double-float/-2 double-float/-2)

(defppc2-sf-op ppc2-%short-float+-2 %short-float+-2 single-float+-2)
(defppc2-sf-op ppc2-%short-float--2 %short-float--2 single-float--2)
(defppc2-sf-op ppc2-%short-float*-2 %short-float*-2 single-float*-2)
(defppc2-sf-op ppc2-%short-float/-2 %short-float/-2 single-float/-2)

(defun ppc2-get-float (seg vreg xfer ptr offset double-p fp-reg)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (cond ((null vreg)
           (ppc2-form seg nil nil ptr)
           (ppc2-form seg nil xfer offset))
          (t
           (let* ((fixoffset (acode-fixnum-form-p offset)))
             (if (typep fixoffset '(unsigned-byte 15))
               (with-imm-target () (ptrreg :address)
                 (ppc2-form seg ptrreg nil ptr)
                 (if double-p
                   (! mem-ref-c-double-float fp-reg ptrreg fixoffset)
                   (! mem-ref-c-single-float fp-reg ptrreg fixoffset)))
               (with-imm-target () (ptrreg :address)
                 (with-imm-target (ptrreg) (offsetreg :s32)
                   (ppc2-two-targeted-reg-forms seg
                                                ptr ptrreg
                                                offset ($ ppc::arg_z))
                   (! fixnum->signed-natural offsetreg ppc::arg_z)
                   (if double-p
                     (! mem-ref-double-float fp-reg ptrreg offsetreg)
                     (! mem-ref-single-float fp-reg ptrreg offsetreg)))))
             (<- fp-reg))
           (^)))))
    

(defppc2 ppc2-%get-double-float %get-double-float (seg vreg xfer ptr offset)
  (with-fp-target () (fp-reg :double-float)
    (ppc2-get-float seg vreg xfer ptr offset t fp-reg)))

(defppc2 ppc2-%get-single-float %get-single-float (seg vreg xfer ptr offset)
  (with-fp-target () (fp-reg :single-float)
    (ppc2-get-float seg vreg xfer ptr offset nil fp-reg)))

(defun ppc2-set-float (seg vreg xfer ptr offset newval double-p fp-reg)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (let* ((fixoffset (acode-fixnum-form-p offset))
           (immoffset (typep fixoffset '(unsigned-byte 15))))
      (with-imm-target () (ptr-reg :address) 
        (cond ((or (null vreg)
                   (= (hard-regspec-class vreg) hard-reg-class-fpr))
               (cond (immoffset
                      (ppc2-push-register
                       seg
                       (ppc2-one-untargeted-reg-form seg
                                                     ptr
                                                     ptr-reg))
                      (ppc2-one-targeted-reg-form seg newval fp-reg)
                      (ppc2-pop-register seg ptr-reg)
                      (if double-p
                        (! mem-set-c-double-float fp-reg ptr-reg fixoffset)
                        (! mem-set-c-single-float fp-reg ptr-reg fixoffset)))
                     (t
                      (with-imm-target (ptr-reg) (offset-reg :s32)
                        (ppc2-push-register
                         seg
                         (ppc2-one-untargeted-reg-form seg
                                                       ptr
                                                       ptr-reg))
                        (ppc2-push-register
                         seg
                         (ppc2-one-untargeted-reg-form seg
                                                       offset
                                                       ppc::arg_z))
                        (ppc2-one-targeted-reg-form seg newval fp-reg)
                        (ppc2-pop-register seg ppc::arg_z)
                        (ppc2-pop-register seg ptr-reg)
                        (! fixnum->signed-natural offset-reg ppc::arg_z)
                        (if double-p
                          (! mem-set-double-float fp-reg ptr-reg offset-reg)
                          (! mem-set-single-float fp-reg ptr-reg offset-reg)))))
               (<- fp-reg))
              (t
               (cond (immoffset
                      (let* ((rnew ($ ppc::arg_z)))
                        (ppc2-push-register
                         seg
                         (ppc2-one-untargeted-reg-form seg
                                                       ptr
                                                       ptr-reg))
                        (ppc2-one-targeted-reg-form seg newval rnew)
                        (ppc2-pop-register seg ptr-reg)
                        (with-imm-temps (ptr-reg) ()
                          (ppc2-copy-register seg fp-reg rnew)
                          (if double-p
                            (! mem-set-c-double-float fp-reg ptr-reg fixoffset)
                            (! mem-set-c-single-float fp-reg ptr-reg fixoffset)))))
                     (t
                      (let* ((roffset ($ ppc::arg_y))
                             (rnew ($ ppc::arg_z)))
                        (ppc2-push-register
                         seg
                         (ppc2-one-untargeted-reg-form
                          seg
                          ptr ptr-reg))
                        (ppc2-two-targeted-reg-forms seg
                                                   offset roffset
                                                   newval rnew)
                        (ppc2-pop-register seg ptr-reg)
                        (with-imm-target (ptr-reg) (offset-reg :s32)
                          (with-imm-temps (ptr-reg offset-reg) ()
                            (! fixnum->signed-natural offset-reg roffset)
                            (ppc2-copy-register seg fp-reg rnew))
                        (if double-p
                          (! mem-set-double-float fp-reg ptr-reg offset-reg)
                          (! mem-set-single-float fp-reg ptr-reg offset-reg))))))
               (<- ppc::arg_z)))
        (^)))))

(defppc2 ppc2-%set-double-float %set-double-float (seg vreg xfer ptr offset newval)
  (with-fp-target () (fp-reg :double-float)
    (ppc2-set-float seg vreg xfer ptr offset newval t fp-reg)))
      
(defppc2 ppc2-%set-single-float %set-single-float (seg vreg xfer ptr offset newval)
  (with-fp-target () (fp-reg :single-float)
    (ppc2-set-float seg vreg xfer ptr offset newval nil fp-reg)))

(defppc2 ppc2-immediate-get-ptr immediate-get-ptr (seg vreg xfer ptr offset)
  (let* ((absptr (acode-absolute-ptr-p ptr))
         (triv-p (ppc2-trivial-p offset))
         (dest vreg)
         (offval (acode-fixnum-form-p offset)))
    (cond ((not vreg)
           (ppc2-form seg nil nil ptr)
           (ppc2-form seg nil xfer offset))
          (t
           (if (and absptr offval) 
             (setq absptr (+ absptr offval) offval 0)
             (setq absptr nil))
           (and offval (%i> (integer-length offval) 15) (setq offval nil))
           (and absptr (%i> (integer-length absptr) 15) (setq absptr nil))
           (target-arch-case
            (:ppc32 (progn))
            (:ppc64 (progn
                      (and offval (logtest 3 offval) (setq offval nil))
                      (and absptr (logtest 3 absptr) (setq absptr nil)))))
           (if absptr
             (! mem-ref-c-natural dest ppc::rzero absptr)
             (if offval
               (let* ((src (ppc2-macptr-arg-to-reg seg ptr ($ ppc::imm0 :mode :address))))
                 (! mem-ref-c-natural dest src offval))
               (let* ((src (ppc2-macptr-arg-to-reg seg ptr ($ ppc::imm0 :mode :address))))
                 (if triv-p
                   (with-imm-temps (src) (x)
                     (if (acode-fixnum-form-p offset)
                       (ppc2-lri seg x (acode-fixnum-form-p offset))
                       (! fixnum->signed-natural x (ppc2-one-untargeted-reg-form seg offset ppc::arg_z)))
                     (! mem-ref-natural dest src x))
                   (progn
                     (! temp-push-unboxed-word src)
                     (ppc2-open-undo $undostkblk)
                     (let* ((oreg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z)))
                       (with-imm-temps () (src x)
                         (! temp-pop-unboxed-word src)
                         (ppc2-close-undo)
                         (! fixnum->signed-natural x oreg)
                         (! mem-ref-natural dest src x)))))))) 
           (^)))))

(defppc2 ppc2-get-bit %get-bit (seg vreg xfer ptr offset)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil ptr)
      (ppc2-form seg nil ptr nil))
    (let* ((offval (acode-fixnum-form-p offset))
           (byte-index (if offval (ash offval -3)))
           (bit-shift (if (and byte-index (< byte-index #x8000))
                        (logand 31 (+ 25 (logand offval 7))))))
      (if bit-shift
        (with-imm-target ()
          (src-reg :address)
          (ppc2-one-targeted-reg-form seg ptr src-reg)
          (if (node-reg-p vreg)
            (! mem-ref-c-bit-fixnum vreg src-reg byte-index (logand 31 (+ bit-shift
                                                                           *ppc2-target-fixnum-shift*)))
            (with-imm-target ()           ;OK if src-reg & dest overlap
              (dest :u8)
              (! mem-ref-c-bit dest src-reg  byte-index bit-shift)
              (<- dest))))
        (let* ((triv-p (ppc2-trivial-p offset))
               (offset-reg nil))
          (with-imm-target ()
            (src-reg :address)
            (ppc2-one-targeted-reg-form seg ptr src-reg)
            (unless triv-p
              (! temp-push-unboxed-word src-reg)
              (ppc2-open-undo $undostkblk))
            (setq offset-reg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z))
            (unless triv-p
              (! temp-pop-unboxed-word src-reg)
              (ppc2-close-undo))
            (if (node-reg-p vreg)
              (! mem-ref-bit-fixnum vreg src-reg offset-reg)
              (with-imm-target ()
                (dest :u8)
                (! mem-ref-bit dest src-reg offset-reg)
                (<- dest))))))))
  (^))
    
      
                                      
;;; This returns an unboxed object, unless the caller wants to box it.
(defppc2 ppc2-immediate-get-xxx immediate-get-xxx (seg vreg xfer bits ptr offset)
  (declare (fixnum bits))
  (let* ((fixnump (logbitp 6 bits))
         (signed (logbitp 5 bits))
         (size (logand 15 bits))
         (absptr (acode-absolute-ptr-p ptr))
         (triv-p (ppc2-trivial-p offset))
         (offval (acode-fixnum-form-p offset)))
    (declare (fixnum size))
    (cond ((null vreg)
           (ppc2-form seg nil nil ptr)
           (ppc2-form seg nil xfer offset))
          (t 
           (if (and absptr offval) 
             (setq absptr (+ absptr offval) offval 0)
             (setq absptr nil))
           (and offval (%i> (integer-length offval) 15) (setq offval nil))
           (and absptr (%i> (integer-length absptr) 15) (setq absptr nil))
           (target-arch-case
            (:ppc32 (progn))
            (:ppc64 (when (or fixnump (eql size 8) (and (eql size 8) signed))
                      (and offval (logtest 3 offval) (setq offval nil))
                      (and absptr (logtest 3 absptr) (setq absptr nil))))) 
           (cond
             (fixnump
              (with-imm-target () (dest :signed-natural)
                (cond
                  (absptr                              
                   (target-arch-case
                    (:ppc32 (! mem-ref-c-fullword dest ppc::rzero absptr))
                    (:ppc64 (! mem-ref-c-doubleword dest ppc::rzero absptr))))
                  (offval
                    (with-imm-target () (src-reg :address)
                      (ppc2-one-targeted-reg-form seg ptr src-reg)
                      (target-arch-case
                       (:ppc32 (! mem-ref-c-fullword dest src-reg offval))
                       (:ppc64 (! mem-ref-c-doubleword dest src-reg offval)))))
                  (t
                   (with-imm-target () (src-reg :address)
                     (with-imm-target (src-reg) (offset-reg :signed-natural)
                       (ppc2-one-targeted-reg-form seg ptr src-reg)
                       (if triv-p
                         (if (acode-fixnum-form-p offset)
                           (ppc2-lri seg offset-reg (acode-fixnum-form-p offset))
                           (! fixnum->signed-natural offset-reg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z)))
                         (progn
                           (! temp-push-unboxed-word src-reg)
                           (ppc2-open-undo $undostkblk)
                           (! fixnum->signed-natural offset-reg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z))
                           (! temp-pop-unboxed-word src-reg)
                           (ppc2-close-undo)))
                       (target-arch-case
                        (:ppc32 (! mem-ref-fullword dest src-reg offset-reg))
                        (:ppc64 (! mem-ref-doubleword dest src-reg offset-reg)))))))
                (if (node-reg-p vreg)
                  (! box-fixnum vreg dest)
                  (<- dest))))
             (signed
              (with-imm-target () (dest :signed-natural)
               (cond
                 (absptr
                  (case size
                    (8 (! mem-ref-c-signed-doubleword dest ppc::rzero absptr))
                    (4 (! mem-ref-c-signed-fullword dest ppc::rzero absptr))
                    (2 (! mem-ref-c-s16 dest ppc::rzero absptr))
                    (1 (! mem-ref-c-s8 dest ppc::rzero absptr))))
                 (offval
                  (with-imm-target (dest) (src-reg :address)
                   (ppc2-one-targeted-reg-form seg ptr src-reg)
                     (case size
                       (8 (! mem-ref-c-signed-doubleword dest src-reg offval))
                       (4 (! mem-ref-c-signed-fullword dest src-reg offval))
                       (2 (! mem-ref-c-s16 dest src-reg offval))
                       (1 (! mem-ref-c-s8 dest src-reg offval)))))
                 (t
                  (with-imm-target () (src-reg :address)
                    (with-imm-target (src-reg) (offset-reg :signed-natural)
                     (ppc2-one-targeted-reg-form seg ptr src-reg)
                     (if triv-p
                       (if (acode-fixnum-form-p offset)
                         (ppc2-lri seg offset-reg (acode-fixnum-form-p offset))
                         (! fixnum->signed-natural offset-reg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z)))
                       (progn
                         (! temp-push-unboxed-word src-reg)
                         (ppc2-open-undo $undostkblk)
                         (! fixnum->signed-natural offset-reg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z))
                         (! temp-pop-unboxed-word src-reg)
                         (ppc2-close-undo)))
                  (case size
                    (8 (! mem-ref-signed-doubleword dest src-reg offset-reg))
                    (4 (! mem-ref-signed-fullword dest src-reg offset-reg))
                    (2 (! mem-ref-s16 dest src-reg offset-reg))
                    (1 (! mem-ref-s8 dest src-reg offset-reg)))))))
               (if (node-reg-p vreg)
                 (case size
                   ((1 2) (! box-fixnum vreg dest))
                   (4 (target-arch-case
                       (:ppc32
                        (<- dest))
                       (:ppc64 (! box-fixnum vreg dest))))
                   (8 (<- dest)))
                 (<- dest))))
             (t
              (with-imm-target () (dest :natural)
               (cond
                 (absptr
                  (case size
                    (8 (! mem-ref-c-doubleword dest ppc::rzero absptr))
                    (4 (! mem-ref-c-fullword dest ppc::rzero absptr))
                    (2 (! mem-ref-c-u16 dest ppc::rzero absptr))
                    (1 (! mem-ref-c-u8 dest ppc::rzero absptr))))
                 (offval
                  (with-imm-target (dest) (src-reg :address)
                    (ppc2-one-targeted-reg-form seg ptr src-reg)
                    (case size
                      (8 (! mem-ref-c-doubleword dest src-reg offval))
                      (4 (! mem-ref-c-fullword dest src-reg offval))
                      (2 (! mem-ref-c-u16 dest src-reg offval))
                      (1 (! mem-ref-c-u8 dest src-reg offval)))))
                 (t
                  (with-imm-target () (src-reg :address)
                    (with-imm-target (src-reg) (offset-reg :signed-natural)
                     (ppc2-one-targeted-reg-form seg ptr src-reg)
                     (if triv-p
                       (if (acode-fixnum-form-p offset)
                         (ppc2-lri seg offset-reg (acode-fixnum-form-p offset))
                         (! fixnum->signed-natural offset-reg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z)))
                       (progn
                         (! temp-push-unboxed-word src-reg)
                         (ppc2-open-undo $undostkblk)
                         (! fixnum->signed-natural offset-reg (ppc2-one-untargeted-reg-form seg offset ppc::arg_z))
                         (! temp-pop-unboxed-word src-reg)
                         (ppc2-close-undo)))
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

(defppc2 ppc2-let let (seg vreg xfer vars vals body p2decls)
  (let* ((old-stack (ppc2-encode-stack))
         (val nil)
         (bits nil)
         (valcopy vals))
    (with-ppc-p2-declarations p2decls
      (dolist (var vars)
        (setq val (%car valcopy))
        (cond ((or (%ilogbitp $vbitspecial (setq bits (nx-var-bits var)))
                   (and (%ilogbitp $vbitreg bits)
                        (dolist (val (%cdr valcopy))
                          (unless (ppc2-trivial-p val) (return t)))))
               (let* ((pair (cons (ppc2-vloc-ea *ppc2-vstack*) nil)))
                 (%rplaca valcopy pair)
                 (if (and (%ilogbitp $vbitdynamicextent bits)
                          (progn
                            (setq val 
                                  (ppc2-dynamic-extent-form seg (ppc2-encode-stack) val))
                            (ppc2-load-ea-p val)))
                   (progn
                     (%rplaca pair (ppc2-vloc-ea *ppc2-vstack*))
                     (ppc2-vpush-register seg val :reserved))
                 (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg val ppc::arg_z) :reserved))
                 (%rplacd pair *ppc2-top-vstack-lcell*)))
              (t (ppc2-seq-bind-var seg var val)
                 (%rplaca valcopy nil)))
        (setq valcopy (%cdr valcopy)))
      (dolist (var vars)
        (declare (list val))
        (when (setq val (pop vals))
          (if (%ilogbitp $vbitspecial (nx-var-bits var))
            (progn
              (ppc2-dbind seg (car val) (var-name var))
              (ppc2-set-var-ea seg var (ppc2-vloc-ea (- *ppc2-vstack* *ppc2-target-node-size*))))
            (ppc2-seq-bind-var seg var (car val)))))
      (ppc2-undo-body seg vreg xfer body old-stack)
      (dolist (var vars)
        (ppc2-close-var seg var)))))

(defppc2 ppc2-closed-function closed-function (seg vreg xfer afunc)
  (ppc2-make-closure seg afunc nil)
  (when vreg (<- ppc::arg_z))
  (^))

(defppc2 ppc2-flet flet (seg vreg xfer vars afuncs body p2decls)
  (ppc2-seq-fbind seg vreg xfer vars afuncs body p2decls))

(defppc2 ppc2-labels labels (seg vreg xfer vars afuncs body p2decls)
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
      (ppc2-seq-fbind seg vreg xfer (nreverse real-vars) (nreverse real-funcs) body p2decls)
      (let* ((old-stack (ppc2-encode-stack)))
        (setq real-vars (nreverse real-vars) real-funcs (nreverse real-funcs))
        (with-ppc-p2-declarations p2decls
          (dolist (var real-vars)
            (ppc2-seq-bind-var seg var (nx1-afunc-ref (pop real-funcs))))
          (dolist (ref fwd-refs)
            (let ((ea (var-ea (pop ref))))
              (ppc2-addrspec-to-reg seg ea ppc::temp0)
              (dolist (r ref)
                (let* ((v-ea (var-ea (cdr r))))
                  (let* ((val-reg (if (eq v-ea ea)
                                    ppc::temp0
                                    (progn
                                      (ppc2-addrspec-to-reg seg v-ea ppc::temp1)
                                      ppc::temp1))))
                    (! misc-set-c-node val-reg ppc::temp0 (car r)))))))
          (ppc2-undo-body seg vreg xfer body old-stack)
          (dolist (var real-vars)
            (ppc2-close-var seg var)))))))

;;; Make a function call (e.g., to mapcar) with some of the toplevel arguments
;;; stack-consed (downward) closures.  Bind temporaries to these closures so
;;; that tail-recursion/non-local exits work right.
;;; (all of the closures are distinct: FLET and LABELS establish dynamic extent themselves.)
(defppc2 ppc2-with-downward-closures with-downward-closures (seg vreg xfer tempvars closures callform)
  (let* ((old-stack (ppc2-encode-stack)))
    (ppc2-seq-bind seg tempvars closures)
    (ppc2-undo-body seg vreg xfer callform old-stack)
    (dolist (v tempvars) (ppc2-close-var seg v))))


(defppc2 ppc2-local-return-from local-return-from (seg vreg xfer blocktag value)
  (declare (ignorable vreg xfer))
  (let* ((*ppc2-undo-count* *ppc2-undo-count*)
         (tagdata (car blocktag))
         (cur-stack (ppc2-encode-stack))
         (dest-vd (caar tagdata))
         (dest-cd (cdar tagdata))
         (mv-p (ppc2-mvpass-p dest-cd))
         (dest-stack  (cdr tagdata))
         (need-break (neq cur-stack dest-stack)))
    (let* ((*ppc2-vstack* *ppc2-vstack*)
           (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
           (*ppc2-cstack* *ppc2-cstack*))
      (if 
        (or
         (eq dest-cd $backend-return)
         (and mv-p 
              (eq (ppc2-encoding-undo-count cur-stack)
                  (ppc2-encoding-undo-count dest-stack)) 
              (eq (ppc2-encoding-cstack-depth cur-stack)
                  (ppc2-encoding-cstack-depth dest-stack))))
        (ppc2-form seg dest-vd dest-cd value)
        (if mv-p
          (progn
            (ppc2-multiple-value-body seg value)
            (let* ((*ppc2-returning-values* :pass))
              (ppc2-nlexit seg dest-cd (%i- *ppc2-undo-count* (ppc2-encoding-undo-count dest-stack)))
              (ppc2-branch seg dest-cd vreg)))
          (progn
            (ppc2-form 
             seg
             (if need-break (if dest-vd ppc::arg_z) dest-vd) 
             (if need-break nil dest-cd)
             value)
            (when need-break
              (ppc2-unwind-set seg dest-cd dest-stack)
              (when dest-vd (ppc2-copy-register seg dest-vd ppc::arg_z))
              (ppc2-branch seg dest-cd dest-vd))))))
    (ppc2-unreachable-store)))

(defppc2 ppc2-inherited-arg inherited-arg (seg vreg xfer arg)
  (when vreg
    (ppc2-addrspec-to-reg seg (ppc2-ea-open (var-ea arg)) vreg))
  (^))


(defppc2 ppc2-%lisp-word-ref %lisp-word-ref (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (ppc2-form seg nil nil base)
           (ppc2-form seg nil xfer offset))
          ((target-arch-case
            (:ppc32 (typep fixoffset '(signed-byte 14)))
            (:ppc64 (typep fixoffset '(signed-byte 13))))
           (ensuring-node-target (target vreg)
             (! lisp-word-ref-c target 
                (ppc2-one-untargeted-reg-form seg base ppc::arg_z) 
                (ash fixoffset *ppc2-target-fixnum-shift*)))
           (^))
          (t (multiple-value-bind (breg oreg)
                                  (ppc2-two-untargeted-reg-forms seg base ppc::arg_y offset ppc::arg_z)
               (ensuring-node-target (target vreg)
                 (! lisp-word-ref target breg oreg))
               (^))))))

(defppc2 ppc2-%fixnum-ref %fixnum-ref (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (ppc2-form seg nil nil base)
           (ppc2-form seg nil xfer offset))
          ((typep fixoffset '(signed-byte 16))
           (ensuring-node-target (target vreg)
             (! lisp-word-ref-c target 
                (ppc2-one-untargeted-reg-form seg base ppc::arg_z) 
                fixoffset))
           (^))
          (t (multiple-value-bind (breg oreg)
                                  (ppc2-two-untargeted-reg-forms seg base ppc::arg_y offset ppc::arg_z)
               (with-imm-target () (otemp :s32)
                 (! fixnum->signed-natural otemp oreg)
                 (ensuring-node-target (target vreg)
                   (! lisp-word-ref target breg otemp)))
               (^))))))

(defppc2 ppc2-%fixnum-ref-natural %fixnum-ref-natural (seg vreg xfer base offset)
  (let* ((fixoffset (acode-fixnum-form-p offset)))
    (cond ((null vreg)
           (ppc2-form seg nil nil base)
           (ppc2-form seg nil xfer offset))
          ((typep fixoffset '(signed-byte 16))
           (with-imm-target () (val :natural)
             (! lisp-word-ref-c val
                (ppc2-one-untargeted-reg-form seg base ppc::arg_z) 
                fixoffset)
             (<- val))
           (^))
          (t (multiple-value-bind (breg oreg)
		 (ppc2-two-untargeted-reg-forms seg base ppc::arg_y offset ppc::arg_z)
               (with-imm-target () (otemp :s32)
                 (! fixnum->signed-natural otemp oreg)
                 (with-imm-target () (val :natural)
                   (! lisp-word-ref val breg otemp)
                   (<- val)))
               (^))))))

(defppc2 ppc2-int>0-p int>0-p (seg vreg xfer cc form)
  (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
    (ppc2-one-targeted-reg-form seg form ($ ppc::arg_z))
    (! integer-sign)
    (ppc2-test-reg-%izerop seg vreg xfer ppc::imm0 cr-bit true-p 0)))


(defppc2 ppc2-throw throw (seg vreg xfer tag valform )
  (declare (ignorable vreg xfer))
  (let* ((*ppc2-vstack* *ppc2-vstack*)
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*))
    (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg tag ppc::arg_z))
    (if (ppc2-trivial-p valform)
      (progn
        (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg valform ppc::arg_z))
        (ppc2-set-nargs seg 1))
      (ppc2-multiple-value-body seg valform))
    (! throw)))

;;; This (and unwind-protect and things like that) are a little funky in that
;;; they have no good way of specifying the exit-point.  The bad way is to
;;; follow the call to the catch-frame-creating subprim with a branch to that
;;; exit-point; the subprim returns to the following instruction.
;;; If the compiler ever gets smart about eliminating dead code, it has to
;;; be careful not to consider the block following the jump to be dead.
;;; Use a vinsn other than JUMP to reference the label.
(defppc2 ppc2-catch catch (seg vreg xfer tag valform)
  (let* ((tag-label (backend-get-next-label))
         (mv-pass (ppc2-mv-p xfer)))
    (ppc2-one-targeted-reg-form seg tag ($ ppc::arg_z))
    (if mv-pass
      (! mkcatchmv)
      (! mkcatch1v))
    (! non-barrier-jump (aref *backend-labels* tag-label))
    (ppc2-open-undo)
    (if mv-pass
      (ppc2-multiple-value-body seg valform)  
      (ppc2-one-targeted-reg-form seg valform ($ ppc::arg_z)))
    (ppc2-lri seg ppc::imm0 (ash 1 *ppc2-target-fixnum-shift*))
    (if mv-pass
      (! nthrowvalues)
      (! nthrow1value))
    (ppc2-close-undo)
    (@ tag-label)
    (unless mv-pass (if vreg (<- ppc::arg_z)))
    (let* ((*ppc2-returning-values* mv-pass)) ; nlexit keeps values on stack
      (^))))


(defppc2 ppc2-fixnum-overflow fixnum-overflow (seg vreg xfer form)
  (destructuring-bind (op n0 n1) (acode-unwrapped-form form)
    (ppc2-use-operator op seg vreg xfer n0 n1 *nx-t*)))



(defppc2 ppc2-%aref2 simple-typed-aref2 (seg vreg xfer typename arr i j &optional dim0 dim1)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil arr)
      (ppc2-form seg nil nil i)
      (ppc2-form seg nil xfer j))
    (let* ((type-keyword (ppc2-immediate-operand typename))
           (fixtype (nx-lookup-target-uvector-subtag type-keyword ))
           (safe (unless *ppc2-reckless* fixtype))
           (dim0 (acode-fixnum-form-p dim0))
           (dim1 (acode-fixnum-form-p dim1)))
      (ppc2-aref2 seg vreg xfer arr i j safe type-keyword dim0 dim1))))


(defppc2 ppc2-general-aref2 general-aref2 (seg vreg xfer arr i j)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (keyword (and atype
                       (let* ((dims (array-ctype-dimensions atype)))
                         (and (typep dims 'list)
                              (= 2 (length dims))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (let* ((dims (array-ctype-dimensions atype))
                  (dim0 (car dims))
                  (dim1 (cadr dims)))
             (ppc2-aref2 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         (if *ppc2-reckless*
                           *nx-nil*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword        ;(make-acode (%nx1-operator immediate) )
                         (if (typep dim0 'fixnum) dim0) (if (typep dim1 'fixnum) dim1))))
          (t
           (ppc2-three-targeted-reg-forms seg
                                          arr ($ ppc::arg_x)
                                          i ($ ppc::arg_y)
                                          j ($ ppc::arg_z))
           (ppc2-fixed-call-builtin seg vreg xfer nil (subprim-name->offset '.SParef2)))))  )


(defppc2 ppc2-%aref3 simple-typed-aref3 (seg vreg xfer typename arr i j k &optional dim0 dim1 dim2)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil arr)
      (ppc2-form seg nil nil i)
      (ppc2-form seg nil nil j)
      (ppc2-form seg nil xfer k)))
  (let* ((type-keyword (ppc2-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword ))
         (safe (unless *ppc2-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1))
         (dim2 (acode-fixnum-form-p dim2)))
    (ppc2-aref3 seg vreg xfer arr i j k safe type-keyword dim0 dim1 dim2)))

(defppc2 ppc2-general-aref3 general-aref3 (seg vreg xfer arr i j k)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (keyword (and atype
                       (let* ((dims (array-ctype-dimensions atype)))
                         (and (typep dims 'list)
                           (= 3 (length dims))))
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
             (ppc2-aref3 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         k
                         (if *ppc2-reckless*
                           *nx-nil*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword ;(make-acode (%nx1-operator immediate) )
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1)
                         (if (typep dim2 'fixnum) dim2))))
          (t
           (ppc2-four-targeted-reg-forms seg
                                         arr ($ ppc::temp0)
                                         i ($ ppc::arg_x)
                                         j ($ ppc::arg_y)
                                         k ($ ppc::arg_z))
           (ppc2-fixed-call-builtin seg vreg xfer nil (subprim-name->offset '.SParef3))))))

(defppc2 ppc2-%aset2 simple-typed-aset2 (seg vreg xfer typename arr i j new &optional dim0 dim1)
  (let* ((type-keyword (ppc2-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword ))
         (safe (unless *ppc2-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1)))
    (ppc2-aset2 seg vreg xfer arr i j new safe type-keyword dim0 dim1))
)

(defppc2 ppc2-general-aset2 general-aset2 (seg vreg xfer arr i j new)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (keyword (and atype
                       (let* ((dims (array-ctype-dimensions atype)))
                         (and (typep dims 'list)
                           (= 2 (length dims))))
                       (not (array-ctype-complexp atype))
                       (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        atype))))
    (cond (keyword
           (let* ((dims (array-ctype-dimensions atype))
                  (dim0 (car dims))
                  (dim1 (cadr dims)))
             (ppc2-aset2 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         new
                         (unless *ppc2-reckless*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1))))
          (t
           (ppc2-four-targeted-reg-forms seg
                                         arr ($ ppc::temp0)
                                         i ($ ppc::arg_x)
                                         j ($ ppc::arg_y)
                                         new ($ ppc::arg_z))
           (ppc2-fixed-call-builtin seg vreg xfer nil (subprim-name->offset '.SPaset2))))))


(defppc2 ppc2-general-aset3 general-aset3 (seg vreg xfer arr i j k new)
  (let* ((atype0 (acode-form-type arr t))
         (ctype (if atype0 (specifier-type atype0)))
         (atype (if (array-ctype-p ctype) ctype))
         (keyword (and atype
                       (let* ((dims (array-ctype-dimensions atype)))
                         (unless (atom dims)
                           (= 3 (length dims))))
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
             (ppc2-aset3 seg
                         vreg
                         xfer
                         arr
                         i
                         j
                         k
                         new
                         (unless *ppc2-reckless*
                           (nx-lookup-target-uvector-subtag keyword ))
                         keyword
                         (if (typep dim0 'fixnum) dim0)
                         (if (typep dim1 'fixnum) dim1)
                         (if (typep dim2 'fixnum) dim2))))
          (t
           (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg arr ($ ppc::arg_z)))
           (ppc2-four-targeted-reg-forms seg
                                         i ($ ppc::temp0)
                                         j ($ ppc::arg_x)
                                         k ($ ppc::arg_y)
                                         new ($ ppc::arg_z))
           (ppc2-pop-register seg ($ ppc::temp1))
           (ppc2-fixed-call-builtin seg vreg xfer nil (subprim-name->offset '.SPaset3))))))

(defppc2 ppc2-%aset3 simple-typed-aset3 (seg vreg xfer typename arr i j k new &optional dim0 dim1 dim2)
  (let* ((type-keyword (ppc2-immediate-operand typename))
         (fixtype (nx-lookup-target-uvector-subtag type-keyword))
         (safe (unless *ppc2-reckless* fixtype))
         (dim0 (acode-fixnum-form-p dim0))
         (dim1 (acode-fixnum-form-p dim1))
         (dim2 (acode-fixnum-form-p dim2)))
    (ppc2-aset3 seg vreg xfer arr i j k new safe type-keyword dim0 dim1 dim2)))



(defppc2 ppc2-%typed-uvref %typed-uvref (seg vreg xfer subtag uvector index)
  (let* ((type-keyword
          (let* ((fixtype (acode-fixnum-form-p subtag)))
            (if fixtype
              (nx-target-uvector-subtag-name fixtype)
              (ppc2-immediate-operand subtag)))))
    (if type-keyword
      (ppc2-vref seg vreg xfer type-keyword uvector index (unless *ppc2-reckless* (nx-lookup-target-uvector-subtag type-keyword)))
      (progn
        (ppc2-three-targeted-reg-forms seg subtag ($ ppc::arg_x) uvector ($ ppc::arg_y) index ($ ppc::arg_z))
        (! subtag-misc-ref)
        (when vreg (<- ($ ppc::arg_z)))
        (^)) )))

(defppc2 ppc2-%typed-uvset %typed-uvset (seg vreg xfer subtag uvector index newval)
  (let* ((type-keyword
          (let* ((fixtype (acode-fixnum-form-p subtag)))
            (if fixtype
              (nx-target-uvector-subtag-name fixtype)
              (ppc2-immediate-operand subtag)))))
    (if type-keyword
      (ppc2-vset seg vreg xfer type-keyword uvector index newval (unless *ppc2-reckless* (nx-lookup-target-uvector-subtag type-keyword)))
      (progn
        (ppc2-four-targeted-reg-forms seg
                                      subtag ($ ppc::temp0)
                                      uvector ($ ppc::arg_x)
                                      index ($ ppc::arg_y)
                                      newval ($ ppc::arg_z))

        (! subtag-misc-set)
        (when vreg (<- ($ ppc::arg_z)))
        (^)))))

(defppc2 ppc2-%macptrptr% %macptrptr% (seg vreg xfer form)
  (with-imm-target () (target :address)
    (ppc2-one-targeted-reg-form seg form (or vreg target)))
  (^))
           

;;; cons a macptr, unless "vreg" is an immediate register of mode :address.
(defppc2 ppc2-%consmacptr% %consmacptr% (seg vreg xfer form)
  (cond ((null vreg) (ppc2-form seg nil xfer form))
        ((eql (get-regspec-mode vreg) hard-reg-class-gpr-mode-address)
         (ppc2-form seg vreg xfer form))
        (t         
         (with-imm-target () (temp :address)
           (<- (ppc2-one-targeted-reg-form seg form temp))
           (^)))))

(defppc2 ppc2-%immediate-ptr-to-int %immediate-ptr-to-int (seg vreg xfer form)
  (if (null vreg)
    (ppc2-form seg nil xfer form)
    (with-imm-target () (address-reg :address)
      (ppc2-form seg address-reg nil form)
      (<- (set-regspec-mode address-reg (gpr-mode-name-value :natural)))
      (^))))

(defppc2 ppc2-%immediate-int-to-ptr %immediate-int-to-ptr (seg vreg xfer form)
  (if (null vreg)
    (ppc2-form seg nil xfer form)
    (progn
      (unless (logbitp (hard-regspec-value vreg) ppc-imm-regs)
        (compiler-bug "I give up.  When will I get this right ?"))
      (let* ((natural-reg (ppc2-one-targeted-reg-form seg 
                                                      form
                                                      ($ vreg :mode :natural))))
        (<- natural-reg)
        (^)))))


(defppc2 ppc2-%function %function (seg vreg xfer sym)
  (when vreg
    (let* ((symreg (ppc2-one-untargeted-reg-form seg (make-acode (%nx1-operator immediate)
                                                                 (ppc2-symbol-entry-locative sym)) ppc::arg_z)))
      (with-node-temps (vreg symreg) (val)
        (! symbol-function val symreg)
        (<- val))))
  (^))

(defppc2 ppc2-%unbound-marker %unbound-marker (seg vreg xfer)
  (when vreg       
    (ensuring-node-target (target vreg)
      (ppc2-lri seg target (target-arch-case
                            (:ppc32 ppc32::unbound-marker)
                            (:ppc64 ppc64::unbound-marker)))))
  (^))

(defppc2 ppc2-slot-unbound-marker %slot-unbound-marker (seg vreg xfer)
  (when vreg    
    (ensuring-node-target (target vreg)
      (ppc2-lri seg target (target-arch-case
                            (:ppc32 ppc32::slot-unbound-marker)
                            (:ppc64 ppc64::slot-unbound-marker)))))
  (^))

(defppc2 ppc2-illegal-marker %illegal-marker (seg vreg xfer)
  (when vreg    
    (ensuring-node-target (target vreg)
      (ppc2-lri seg target (target-arch-case
                            (:ppc32 ppc32::illegal-marker)
                            (:ppc64 ppc64::illegal-marker)))))
  (^))

(defppc2 ppc2-lambda-bind lambda-bind (seg vreg xfer vals req rest keys-p auxen body p2decls)
  (let* ((old-stack (ppc2-encode-stack))
         (nreq (list-length req))
         (rest-arg (nthcdr nreq vals))
         (apply-body (ppc2-eliminate-&rest body rest keys-p auxen rest-arg)))
    (ppc2-seq-bind seg req vals)
    (when apply-body (setq rest nil body apply-body))
    (let*
      ((vloc *ppc2-vstack*)
       (restloc vloc)
       (nvloc (progn (if (or rest keys-p) (ppc2-formlist seg rest-arg)) *ppc2-vstack*)))
      (with-ppc-p2-declarations p2decls
        (when rest
          (when keys-p
            (until (eq restloc nvloc)
              (with-node-temps () (temp)
                (ppc2-stack-to-register seg (ppc2-vloc-ea restloc) temp)
                (ppc2-vpush-register seg temp))
              (setq restloc (%i+ restloc *ppc2-target-node-size*))))
          (ppc2-set-nargs seg (length rest-arg))
          (ppc2-set-vstack restloc)
          (if (%ilogbitp $vbitdynamicextent (nx-var-bits rest))
            (progn
              (! stack-cons-list)
              (ppc2-open-undo $undostkblk))
            (! list))
          (ppc2-vpush-register seg ppc::arg_z))
        (when rest (ppc2-bind-var seg rest restloc))
        (destructuring-bind (vars inits) auxen
          (while vars
            (let ((val (%car inits))) 
              (if (fixnump val)
                (progn
                  (when rest (setq val (%i+ (%i+ val val) 1)))
                  (ppc2-bind-var seg (%car vars) (%i+ vloc (* val *ppc2-target-node-size*))))
                (ppc2-seq-bind-var seg (%car vars) val)))
            (setq vars (%cdr vars) inits (%cdr inits))))
        (ppc2-undo-body seg vreg xfer body old-stack)
        (dolist (var req) (ppc2-close-var seg var))
        (when rest (ppc2-close-var seg rest))
        (dolist (var (%car auxen)) (ppc2-close-var seg var))))))

(macrolet 
  ((def-ppc2-require (function op &optional (vinsn op))
     `(defppc2 ,function ,op (seg vreg xfer val)
        (let* ((val-reg (ppc2-one-untargeted-reg-form 
                         seg 
                         val 
                         (if (eq vreg ppc::arg_z) ppc::arg_y ppc::arg_z))))
          (! ,vinsn val-reg)
          (when vreg (<- val-reg))
          (^)))))
  (def-ppc2-require ppc2-require-simple-vector require-simple-vector)
  (def-ppc2-require ppc2-require-simple-string require-simple-string)
  (def-ppc2-require ppc2-require-integer require-integer)
  (def-ppc2-require ppc2-require-fixnum require-fixnum)
  (def-ppc2-require ppc2-require-real require-real)
  (def-ppc2-require ppc2-require-list require-list)
  (def-ppc2-require ppc2-require-character require-character)
  (def-ppc2-require ppc2-require-number require-number)
  (def-ppc2-require ppc2-require-symbol require-symbol)
  (def-ppc2-require ppc2-require-s8 require-s8)
  (def-ppc2-require ppc2-require-s8 require-u8)
  (def-ppc2-require ppc2-require-s8 require-s16)
  (def-ppc2-require ppc2-require-s8 require-u16)
  (def-ppc2-require ppc2-require-s8 require-s32)
  (def-ppc2-require ppc2-require-s8 require-u32)
  (def-ppc2-require ppc2-require-s8 require-s64)
  (def-ppc2-require ppc2-require-s8 require-u64))

(defun ppc2-typechecked-form (seg vreg xfer typespec form)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
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
        (ppc2-use-operator op seg vreg xfer form)
        (if (or (eq typespec t)
                (eq typespec '*))
          (ppc2-form seg vreg xfer form)
          (let* ((ok (backend-get-next-label)))
            (ppc2-one-targeted-reg-form seg form ($ ppc::arg_y))
            (ppc2-store-immediate seg typespec ($ ppc::arg_z))
            (ppc2-store-immediate seg 'typep ($ ppc::fname))
            (ppc2-set-nargs seg 2)
            (ppc2-vpush-register seg ($ ppc::arg_y))
            (! call-known-symbol ($ ppc::arg_z))
	    (with-crf-target () crf
               (! compare-to-nil crf ($ ppc::arg_z))
	       (ppc2-vpop-register seg ($ ppc::arg_y))
	       (! cbranch-false (aref *backend-labels* ok) crf ppc::ppc-eq-bit))
            (ppc2-lri seg ($ ppc::arg_x) (ash $XWRONGTYPE *ppc2-target-fixnum-shift*))
            (ppc2-store-immediate seg typespec ($ ppc::arg_z))
            (ppc2-set-nargs seg 3)
            (! ksignalerr)
            (@ ok)
            (<- ($ ppc::arg_y))
            (^)))))))

(defppc2 ppc2-%badarg2 %badarg2 (seg vreg xfer badthing goodthing)
  (ppc2-two-targeted-reg-forms seg badthing ($ ppc::arg_y) goodthing ($ ppc::arg_z))
  (ppc2-lri seg ($ ppc::arg_x) (ash $XWRONGTYPE *ppc2-target-fixnum-shift*))
  (ppc2-set-nargs seg 3)
  (! ksignalerr)
  (<- nil)
  (^))  
          
(defppc2 ppc2-%set-sbchar %set-sbchar (seg vreg xfer string index value)
  (ppc2-vset 
   seg 
   vreg 
   xfer 
   :simple-string 
   string 
   index
   value 
   (unless *ppc2-reckless* (nx-lookup-target-uvector-subtag :simple-string))))


;;; If we didn't use this for stack consing, turn it into a call.  Ugh.

(defppc2 ppc2-make-list make-list (seg vreg xfer size initial-element)
  (ppc2-form seg vreg xfer (make-acode (%nx1-operator call)
                                       (make-acode (%nx1-operator immediate) 'make-list)
                                       (list nil
                                             (list initial-element 
                                                   (make-acode (%nx1-operator immediate)
                                                               :initial-element)
                                                   size)))))


(defppc2 ppc2-setq-free setq-free (seg vreg xfer sym val)
  (let* ((rsym ($ ppc::arg_y))
         (rval ($ ppc::arg_z)))
    (ppc2-one-targeted-reg-form seg val rval)
    (ppc2-immediate seg rsym nil (ppc2-symbol-value-cell sym))
    (! setqsym)
    (<- rval)
    (^)))

(defppc2 ppc2-%setf-macptr %setf-macptr (seg vreg xfer x y)
  (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg x ppc::arg_z))
  (with-imm-target () (src-reg :address)
    (ppc2-one-targeted-reg-form seg y src-reg)
    (ppc2-vpop-register seg ppc::arg_z)
    (unless (or *ppc2-reckless* (ppc2-form-typep x 'macptr))
      (with-imm-temps (src-reg) ()
        (! trap-unless-macptr ppc::arg_z)))
    (! set-macptr-address src-reg ppc::arg_z)
    (<- ppc::arg_z)
    (^)))

(defppc2 ppc2-%setf-double-float %setf-double-float (seg vref xfer fnode fval)
  (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg fnode ppc::arg_z))
  (let* ((target ($ ppc::fp1 :class :fpr :mode :double-float))
         (node ($ ppc::arg_z)))
    (ppc2-one-targeted-reg-form seg fval target)
    (ppc2-vpop-register seg node)
    (unless (or *ppc2-reckless* (ppc2-form-typep fnode 'double-float))
      (! trap-unless-double-float node))
    (! store-double node target)
    (<- node)
    (^)))

(defppc2 ppc2-%setf-short-float %setf-short-float (seg vreg xfer fnode fval)
  (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg fnode ppc::arg_z))
  (let* ((target ($ ppc::fp1 :class :fpr :mode :single-float))
         (freg ($ ppc::arg_z)))
    (ppc2-one-targeted-reg-form seg fval target)
    (ppc2-vpop-register seg freg)
    (unless (or *ppc2-reckless* (ppc2-form-typep fnode 'short-float))
      (! trap-unless-single-float freg))
    (! store-single freg target)
    (<- freg)
    (^)))

    

(defppc2 ppc2-unwind-protect unwind-protect (seg vreg xfer protected-form cleanup-form)
  (let* ((cleanup-label (backend-get-next-label))
         (protform-label (backend-get-next-label))
         (old-stack (ppc2-encode-stack))
         (ilevel '*interrupt-level*))
    (! nmkunwind)
    (ppc2-open-undo $undointerruptlevel)
    (ppc2-new-vstack-lcell :special-value *ppc2-target-lcell-size* 0 ilevel)
    (ppc2-new-vstack-lcell :special *ppc2-target-lcell-size* (ash 1 $vbitspecial) ilevel)
    (ppc2-new-vstack-lcell :special-link *ppc2-target-lcell-size* 0 ilevel)
    (ppc2-adjust-vstack (* 3 *ppc2-target-node-size*))    
    (! non-barrier-jump (aref *backend-labels* cleanup-label))
    (-> protform-label)
    (@ cleanup-label)
    (let* ((*ppc2-vstack* *ppc2-vstack*)
           (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
           (*ppc2-cstack* (%i+ *ppc2-cstack* (target-arch-case
                                              (:ppc32 ppc32::lisp-frame.size)
                                              (:ppc64 ppc64::lisp-frame.size)))))
      (ppc2-open-undo $undostkblk)      ; tsp frame created by nthrow.
      (! save-cleanup-context)
      (setq *ppc2-cstack* (%i+ *ppc2-cstack*
                               (target-arch-case
                                (:ppc32 ppc32::lisp-frame.size)
                                (:ppc64 ppc64::lisp-frame.size))))       ; the frame we just pushed
      (ppc2-form seg nil nil cleanup-form)
      (ppc2-close-undo)
      (! restore-cleanup-context)
      (! jump-return-pc)) ; blr
    (ppc2-open-undo)
    (@ protform-label)
    (ppc2-new-vstack-lcell :special-value *ppc2-target-lcell-size* 0 ilevel)
    (ppc2-new-vstack-lcell :special *ppc2-target-lcell-size* (ash 1 $vbitspecial) ilevel)
    (ppc2-new-vstack-lcell :special-link *ppc2-target-lcell-size* 0 ilevel)
    (ppc2-adjust-vstack (* 3 *ppc2-target-node-size*))

    (ppc2-undo-body seg vreg xfer protected-form old-stack)))

(defppc2 ppc2-progv progv (seg vreg xfer symbols values body)
  (let* ((cleanup-label (backend-get-next-label))
         (protform-label (backend-get-next-label))
         (old-stack (ppc2-encode-stack)))
    (ppc2-two-targeted-reg-forms seg symbols ($ ppc::arg_y) values ($ ppc::arg_z))
    (! progvsave)
    (ppc2-open-undo $undostkblk)
    (! mkunwind)
    (! non-barrier-jump (aref *backend-labels* cleanup-label))
    (-> protform-label)
    (@ cleanup-label)
    (! progvrestore)
    (ppc2-open-undo)
    (@ protform-label)
    (ppc2-undo-body seg vreg xfer body old-stack)))

(defppc2 ppc2-%ptr-eql %ptr-eql (seg vreg xfer cc x y )
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil x)
      (ppc2-form seg nil xfer y))
    (let* ((x-abs (acode-absolute-ptr-p x t))
           (y-abs (acode-absolute-ptr-p y t))
           (abs (or x-abs y-abs))
           (other (if abs (if x-abs y x))))
      (multiple-value-bind (cr-bit true-p) (acode-condition-to-ppc-cr-bit cc)
        (if other
          (with-imm-target () (other-target :address)
            (ppc2-one-targeted-reg-form seg other other-target)
            (if (typep abs '(signed-byte 16))              
              (ppc2-test-reg-%izerop seg vreg xfer other-target cr-bit true-p abs)
              (with-imm-temps (other-target) ((abs-target :address))
                (use-imm-temp other-target)
                (ppc2-lri seg abs-target abs)
                (ppc2-compare-registers seg vreg xfer other-target abs-target cr-bit true-p))))
          ; Neither expression is obviously a constant-valued macptr.
          (with-imm-target () (target-a :address)
            (ppc2-one-targeted-reg-form seg x target-a)
            (! temp-push-unboxed-word target-a)
            (ppc2-open-undo $undostkblk)
            (ppc2-one-targeted-reg-form seg y target-a)
            (with-imm-target (target-a) (target-b :address)
              (! temp-pop-unboxed-word target-b)
              (ppc2-close-undo)
              (ppc2-compare-registers seg vreg xfer target-b target-a cr-bit true-p))))))))

(defppc2 ppc2-set-bit %set-bit (seg vreg xfer ptr offset newval)
  (let* ((offval (acode-fixnum-form-p offset))
         (byte-index (if offval (ash offval -3)))
         (bit-index (if (and byte-index (< byte-index #x8000))
                      (logand offval #x7)))
         (triv-offset (ppc2-trivial-p offset))
         (triv-val (ppc2-trivial-p newval)))
    (with-imm-target ()
      (src :address)
      (ppc2-one-targeted-reg-form seg ptr src)
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
                (ppc2-form seg vreg nil newval)))
            (progn
              (unless triv-val
                (! temp-push-unboxed-word src)
                (ppc2-open-undo $undostkblk))
              (let* ((target (ppc2-one-untargeted-reg-form seg newval ppc::arg_z)))
                (unless triv-val
                  (! temp-pop-unboxed-word src)
                  (ppc2-close-undo))
                (! mem-set-c-bit src byte-index (+ 24 bit-index) target)
                (<- target)))))
        (progn
          (unless (and triv-val triv-offset)
            (! temp-push-unboxed-word src)
            (ppc2-open-undo $undostkblk))
          (multiple-value-bind (idx-reg val-reg)
              (ppc2-two-untargeted-reg-forms seg offset ppc::arg_y newval ppc::arg_z)
            (unless (and triv-val triv-offset)
              (! temp-pop-unboxed-word src)
              (ppc2-close-undo ))
            (! mem-set-bit src idx-reg val-reg)
            (<- val-reg)))))
    (^)))

(defppc2 ppc2-%immediate-set-xxx %immediate-set-xxx (seg vreg xfer bits ptr offset val)
  (ppc2-%immediate-store seg vreg xfer bits ptr offset val))



(defppc2 ppc2-%immediate-inc-ptr %immediate-inc-ptr (seg vreg xfer ptr by)
  (let* ((triv-by (ppc2-trivial-p by))
         (fixnum-by (acode-fixnum-form-p by)))
    (if (and fixnum-by (eql 0 fixnum-by))
      (ppc2-form seg vreg xfer ptr)
      (with-imm-target (vreg) (ptr-reg :address)
        (ppc2-one-targeted-reg-form seg ptr ptr-reg)
        (if fixnum-by
          (with-imm-target (vreg ptr-reg) (result :address)
            (let* ((high (ldb (byte 16 16) fixnum-by))
                   (low (ldb (byte 16 0) fixnum-by)))
              (declare (type (unsigned-byte 16) high low))
              (if (logbitp 15 low) (incf high))
              (! add-immediate result ptr-reg high low)
              (<- result)))
          (progn
            (unless triv-by
              (! temp-push-unboxed-word ptr-reg)
              (ppc2-open-undo $undostkblk))
            (with-imm-target (vreg ptr-reg) (by-reg :s32)
              (ppc2-one-targeted-reg-form seg by by-reg)
              (unless triv-by
                (! temp-pop-unboxed-word ptr-reg)
                (ppc2-close-undo))
              (with-imm-target (vreg ptr-reg by-reg) (result :address)
                (! fixnum-add result ptr-reg by-reg)
                (<- result)))))
        (^)))))



(defppc2 ppc2-multiple-value-call multiple-value-call (seg vreg xfer fn arglist)
  (ppc2-mvcall seg vreg xfer fn arglist))



(defppc2 ppc2-eabi-syscall eabi-syscall (seg vreg xfer idx argspecs argvals resultspec &optional monitor-exception-ports)
  (declare (ignore monitor-exception-ports))
  (let* ((*ppc2-vstack* *ppc2-vstack*)
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
         (*ppc2-cstack* *ppc2-cstack*)
         (nextarg 0))
    (declare (fixnum nextarg))
    (! alloc-eabi-c-frame (the fixnum (length argvals)))
    (ppc2-open-undo $undo-ppc-c-frame)
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
               (ppc2-lri seg ptr absptr)
               (ppc2-one-targeted-reg-form seg valform ptr))
             (! set-eabi-c-arg ptr nextarg)))
          (t
           (! set-eabi-c-arg
              (with-imm-target ()
                (valreg :natural)
                (ppc2-unboxed-integer-arg-to-reg seg valform valreg spec))
              nextarg)))
        (incf nextarg)))
    (ppc2-form seg ppc::arg_z nil idx)
    (! eabi-syscall) 
    (ppc2-close-undo)
    (when vreg
      (if (eq resultspec :void)
        (<- nil)
        (<- (set-regspec-mode ppc::imm0 (gpr-mode-name-value
                                         (case resultspec
                                           (:address :address)
                                           (:signed-byte :s8)
                                           (:unsigned-byte :u8)
                                           (:signed-halfword :s16)
                                           (:unsigned-halfword :u16)
                                           (:signed-fullword :s32)
                                           (t :u32)))))))
    (^)))


;;; Caller has allocated poweropen stack frame.
(defun ppc2-poweropen-foreign-args (seg argspecs argvals)
  (with-ppc-local-vinsn-macros (seg)
    (let* ((fp-loads ())
           (nextarg 0)
           (return-registers nil))
      ;; Evaluate each form into the C frame, according to the matching
      ;; argspec.  Remember type and arg offset of any FP args, since FP
      ;; regs will have to be loaded later.
      (do* ((specs argspecs (cdr specs))
            (vals argvals (cdr vals)))
           ((null specs) (if return-registers (ppc2-pop-register seg ($ ppc::arg_y))))
        (declare (list specs vals))
        (let* ((valform (car vals))
               (spec (car specs))
               (absptr (acode-absolute-ptr-p valform)))
          (case spec
            (:registers
             (setq return-registers t)
             (ppc2-push-register seg (ppc2-one-untargeted-reg-form seg valform ppc::arg_z)))
            ((:signed-doubleword :unsigned-doubleword :hybrid-int-float :hybrid-float-float :hybrid-float-int)
                                 
             (ppc2-one-targeted-reg-form seg valform ($ ppc::arg_z))
             (if (eq spec :signed-doubleword)
               (! gets64)
               (! getu64))
             (! set-c-arg ($ ppc::imm0) nextarg)
             (target-arch-case
              (:ppc32
               (incf nextarg)
               (! set-c-arg ($ ppc::imm1) nextarg))
              (:ppc64
               (case spec
                 (:hybrid-int-float (push (cons :single-float nextarg) fp-loads))
                 (:hybrid-float-int (push (cons :single-float-high nextarg) fp-loads))
                 (:hybrid-float-float
                  (push (cons :single-float-high nextarg) fp-loads)
                  (push (cons :single-float nextarg) fp-loads))))))
            (:double-float
             (let* ((df ($ ppc::fp1 :class :fpr :mode :double-float)))
               (ppc2-one-targeted-reg-form seg valform df)
               (! set-double-c-arg df nextarg)            
               (push (cons :double-float nextarg) fp-loads)
               (target-word-size-case
                (32 (incf nextarg))
                (64))))
            (:single-float
             (let* ((sf ($ ppc::fp1 :class :fpr :mode :single-float)))
               (ppc2-one-targeted-reg-form seg valform sf)
               (! set-single-c-arg sf nextarg)
               (push (cons :single-float nextarg) fp-loads)))
            (:address
             (with-imm-target ()
                 (ptr :address)
               (if absptr
                 (ppc2-lri seg ptr absptr)
                 (ppc2-one-targeted-reg-form seg valform ptr))
               (! set-c-arg ptr nextarg)))
            (t
             (if (typep spec 'unsigned-byte)
               (progn
                 (with-imm-target () (ptr :address)
                   (ppc2-one-targeted-reg-form seg valform ptr)
                   (with-imm-temps (ptr) (r)
                     (dotimes (i spec)
                       (target-arch-case
                        (:ppc32
                         (! mem-ref-c-fullword r ptr (ash i ppc32::word-shift)))
                        (:ppc64
                         (! mem-ref-c-doubleword r ptr (ash i ppc64::word-shift))))
                       (! set-c-arg r nextarg)
                       (incf nextarg))))
                 (decf nextarg))
               (with-imm-target ()
                   (valreg :natural)
                 (let* ((reg valreg))
                   (setq reg (ppc2-unboxed-integer-arg-to-reg seg valform valreg spec))
                   (! set-c-arg reg nextarg))))))
          (unless (eq spec :registers)(incf nextarg))))
      (do* ((fpreg ppc::fp1 (1+ fpreg))
            (reloads (nreverse fp-loads) (cdr reloads)))
           ((or (null reloads) (= fpreg ppc::fp14)))
        (declare (list reloads) (fixnum fpreg))
        (let* ((reload (car reloads))
               (size (car reload))
               (from (cdr reload)))
          (if (eq size :double-float)
            (! reload-double-c-arg fpreg from)
            (if (eq size :single-float-high)
              (! reload-single-c-arg-high fpreg from)
              (! reload-single-c-arg fpreg from)))))
      return-registers)))

(defun ppc2-poweropen-foreign-return (seg vreg xfer resultspec)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (when vreg
      (cond ((eq resultspec :void) (<- nil))
            ((eq resultspec :double-float)
             (<- ($ ppc::fp1 :class :fpr :mode :double-float)))
            ((eq resultspec :single-float)
             (<- ($ ppc::fp1 :class :fpr :mode :single-float)))
            ((eq resultspec :unsigned-doubleword)
             (ensuring-node-target
              (target vreg)
              (! makeu64)
              (ppc2-copy-register seg target ppc::arg_z)))
            ((eq resultspec :signed-doubleword)
             (ensuring-node-target
              (target vreg)
              (! makes64)
              (ppc2-copy-register seg target ppc::arg_z)))
            (t
             (<- (make-wired-lreg ppc::imm0
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

(defppc2 ppc2-poweropen-syscall poweropen-syscall (seg vreg xfer idx argspecs argvals resultspec &optional monitor-exception-ports)
  (declare (ignore monitor-exception-ports))
  (let* ((*ppc2-vstack* *ppc2-vstack*)
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
         (*ppc2-cstack* *ppc2-cstack*))
    (! alloc-c-frame (the fixnum
                       (+ (the fixnum (length argvals))
                          (the fixnnum
                            (let* ((n 0))
                              (declare (fixnum n))
                              (dolist (spec argspecs n)
                                (if (typep spec 'unsigned-byte)
                                  (incf n (the fixnum
                                            (1- (the fixnum spec))))))))
                          (the fixnum
                            (count-if
                             #'(lambda (x)
                                 (member x
                                         '(:double-float
                                           :unsigned-doubleword
                                           :signed-doubleword)))
                             argspecs)))))
    (ppc2-open-undo $undo-ppc-c-frame)
    (ppc2-poweropen-foreign-args seg argspecs argvals)
    (ppc2-form seg ppc::arg_z nil idx)
    (if (eq resultspec :signed-doubleword)
      (! poweropen-syscall-s64)
      (! poweropen-syscall))
    (ppc2-close-undo)
    (ppc2-poweropen-foreign-return seg vreg xfer resultspec)))

(defun ppc2-identity (seg vreg xfer arg)
  (with-ppc-local-vinsn-macros (seg vreg xfer)
    (if (null vreg)
      (ppc2-form seg vreg xfer arg)
      (progn
        (ensuring-node-target (target vreg)
          (ppc2-one-targeted-reg-form seg arg target))
      (^)))))

;;; Outgoing C stack frame will look like:
;;;  backptr
;;;  NIL  ; marker to keep GC happy, make GDB unhappy.
;;;  8 words of GPR arg vals - will be loaded & popped by subprim
;;;  N words of "other" (overflow) arguments
;;;  F words of single-float values, to be loaded into FPR before subprim call
;;;  D aligned doublewords of double-float values, to be loaded into FPR before call.
(defppc2 ppc2-eabi-ff-call eabi-ff-call (seg vreg xfer address argspecs argvals resultspec &optional monitor)
  (declare (ignore monitor))
  (let* ((*ppc2-vstack* *ppc2-vstack*)
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
         (*ppc2-cstack* *ppc2-cstack*)
         (gpr-offset 0)
         (other-offset 8)
         (single-float-offset 8)
         (double-float-offset 8)
         (nsingle-floats 0)              ; F
         (ndouble-floats 0)             ; D
         (nother-words 0)
         (nfpr-args 0)
         (ngpr-args 0)
         (fp-loads ()))
      (declare (fixnum  nsingle-floats ndouble-floats nfpr-args ngpr-args nother-words
                        gpr-offset other-offset single-float-offset double-float-offset))
      (dolist (argspec argspecs)
        (case argspec
          (:double-float (incf nfpr-args)
                         (if (<= nfpr-args 8)
                           (incf ndouble-floats)
                           (progn
                             (if (oddp nother-words)
                               (incf nother-words))
                             (incf nother-words 2))))
          (:single-float (incf nfpr-args)
                         (if (<= nfpr-args 8)
                           (incf nsingle-floats)
                           (progn
                             (if (oddp nother-words)
                               (incf nother-words))
                             (incf nother-words 2))))
          ((:unsigned-doubleword :signed-doubleword)
           (setq ngpr-args (logior 1 ngpr-args))
           (incf ngpr-args 2)
           (when (> ngpr-args 9)
             (if (oddp nother-words)
               (incf nother-words))
             (incf nother-words 2)))
          (t (incf ngpr-args)
             (if (> ngpr-args 8)
               (incf nother-words)))))
      (let* ((single-words (+ 8 nother-words nsingle-floats))
             (total-words (if (zerop ndouble-floats)
                            single-words
                            (+ (the fixnum (+ ndouble-floats ndouble-floats))
                               (the fixnum (logand (lognot 1) (the fixnum (1+ single-words))))))))
           
        (! alloc-eabi-c-frame total-words))
      (setq single-float-offset (+ other-offset nother-words))
      (setq double-float-offset
            (logand (lognot 1)
                    (the fixnum (1+ (the fixnum (+ single-float-offset nsingle-floats))))))
      (setq ngpr-args 0 nfpr-args 0)
      (ppc2-open-undo $undo-ppc-c-frame)
      (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg address ppc::arg_z))
      ;; Evaluate each form into the C frame, according to the
      ;; matching argspec.
      ;; Remember type and arg offset of any FP args, since FP regs
      ;; will have to be loaded later.
      (do* ((specs argspecs (cdr specs))
            (vals argvals (cdr vals)))
           ((null specs))
        (declare (list specs vals))
        (let* ((valform (car vals))
               (spec (car specs))
               (absptr (acode-absolute-ptr-p valform)))
          (case spec
            (:double-float
             (let* ((df ($ ppc::fp1 :class :fpr :mode :double-float)))
               (incf nfpr-args)
               (ppc2-one-targeted-reg-form seg valform df )
               (cond ((<= nfpr-args 8)
                      (! set-double-eabi-c-arg df double-float-offset)
                      (push (cons :double-float double-float-offset) fp-loads)
                      (incf double-float-offset 2))
                     (t
                      (setq other-offset (logand (lognot 1) (the fixnum (1+ other-offset))))
                      (! set-double-eabi-c-arg df other-offset)
                      (incf other-offset 2)))))
            (:single-float
             (let* ((sf ($ ppc::fp1 :class :fpr :mode :single-float)))
               (incf nfpr-args)
               (ppc2-one-targeted-reg-form
                seg valform sf)
               (cond ((<= nfpr-args 8)
                      (! set-single-eabi-c-arg sf single-float-offset)
                      (push (cons :single-float single-float-offset) fp-loads)
                      (incf single-float-offset))
                     (t
                      (setq other-offset (logand (lognot 1) (the fixnum (1+ other-offset))))
                      (! set-double-eabi-c-arg sf other-offset)
                      (incf other-offset 2)))))
            ((:signed-doubleword :unsigned-doubleword)
             (ppc2-one-targeted-reg-form seg valform ($ ppc::arg_z))
             (if (eq spec :signed-doubleword)
               (! gets64)
               (! getu64))
             (if (oddp ngpr-args)
               (incf ngpr-args))
             (incf ngpr-args 2)
             (if (oddp gpr-offset)
               (incf gpr-offset))
             (cond ((<= ngpr-args 8)
                    (! set-eabi-c-arg ($ ppc::imm0) gpr-offset)
                    (incf gpr-offset)
                    (! set-eabi-c-arg ($ ppc::imm1) gpr-offset)
                    (incf gpr-offset))
                   (t
                    (if (oddp other-offset)
                      (incf other-offset))
                    (! set-eabi-c-arg ($ ppc::imm0) other-offset)
                    (incf other-offset)
                    (! set-eabi-c-arg ($ ppc::imm1) other-offset)
                    (incf other-offset))))
            (:address
             (with-imm-target () (ptr :address)
               (if absptr
                 (ppc2-lri seg ptr absptr)
                 (ppc2-form seg ptr nil valform))
               (incf ngpr-args)
               (cond ((<= ngpr-args 8)
                      (! set-eabi-c-arg ptr gpr-offset)
                      (incf gpr-offset))
                     (t
                      (! set-eabi-c-arg ptr other-offset)
                      (incf other-offset)))))
            (t
             (with-imm-target () (valreg :natural)
                (let* ((reg (ppc2-unboxed-integer-arg-to-reg seg valform valreg spec)))
                  (incf ngpr-args)
                  (cond ((<= ngpr-args 8)
                         (! set-eabi-c-arg reg gpr-offset)
                         (incf gpr-offset))
                        (t
                         (! set-eabi-c-arg reg other-offset)
                         (incf other-offset)))))))))
      (do* ((fpreg ppc::fp1 (1+ fpreg))
            (reloads (nreverse fp-loads) (cdr reloads)))
           ((or (null reloads) (= fpreg ppc::fp14)))
        (declare (list reloads) (fixnum fpreg))
        (let* ((reload (car reloads))
               (size (car reload))
               (from (cdr reload)))
          (if (eq size :double-float)
            (! reload-double-eabi-c-arg ($ fpreg :class :fpr :mode :double-float) from)
            (! reload-single-eabi-c-arg ($ fpreg :class :fpr :mode :single-float) from))))
      (ppc2-vpop-register seg ($ ppc::arg_z))
      (! eabi-ff-call) 
      (ppc2-close-undo)
      (when vreg
        (cond ((eq resultspec :void) (<- nil))
              ((eq resultspec :double-float)
               (<- ($  ppc::fp1 :class :fpr :mode :double-float)))
              ((eq resultspec :single-float)
               (<- ($ ppc::fp1 :class :fpr :mode :single-float)))
              ((eq resultspec :unsigned-doubleword)
               (ensuring-node-target (target vreg)
                 (! makeu64)
                 (ppc2-copy-register seg target ppc::arg_z)))
              ((eq resultspec :signed-doubleword)
               (ensuring-node-target (target vreg)
                 (! makes64)
                 (ppc2-copy-register seg target ppc::arg_z)))
              (t
               (<- (make-wired-lreg ppc::imm0
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

(defppc2 ppc2-poweropen-ff-call poweropen-ff-call (seg vreg xfer address argspecs argvals resultspec &optional monitor-exception-ports)
  (let* ((*ppc2-vstack* *ppc2-vstack*)
         (*ppc2-top-vstack-lcell* *ppc2-top-vstack-lcell*)
         (*ppc2-cstack* *ppc2-cstack*)
         (return-registers nil))
    (! alloc-c-frame (the fixnum
                       (+ (the fixnum (length argvals)) 
                          (the fixnnum
                            (let* ((n 0))
                              (declare (fixnum n))
                              (dolist (spec argspecs n)
                                (if (typep spec 'unsigned-byte)
                                  (incf n (the fixnum
                                            (1- (the fixnum spec))))))))
                          (the fixnum
                            (count-if
                             #'(lambda (x)
                                 (member x
                                         '(:double-float
                                           :unsigned-doubleword
                                           :signed-doubleword)))
                             argspecs)))))
    (ppc2-open-undo $undo-ppc-c-frame)
    (ppc2-vpush-register seg (ppc2-one-untargeted-reg-form seg address ppc::arg_z))
    (setq return-registers (ppc2-poweropen-foreign-args seg argspecs argvals))
    (ppc2-vpop-register seg ppc::arg_z)
    (if return-registers
      (! poweropen-ff-call-regs)
      (if monitor-exception-ports
        (! poweropen-ff-callX)
        (! poweropen-ff-call)))
    (ppc2-close-undo)
    (when vreg
      (cond ((eq resultspec :void) (<- nil))
            ((eq resultspec :double-float)
             (<- (make-hard-fp-reg ppc::fp1 hard-reg-class-fpr-mode-double)))
            ((eq resultspec :single-float)
             (<- (make-hard-fp-reg ppc::fp1 hard-reg-class-fpr-mode-single)))
            ((eq resultspec :unsigned-doubleword)
             (ensuring-node-target
              (target vreg)
              (! makeu64)
              (ppc2-copy-register seg target ppc::arg_z)))
            ((eq resultspec :signed-doubleword)
             (ensuring-node-target
              (target vreg)
              (! makes64)
              (ppc2-copy-register seg target ppc::arg_z)))
            (t
             (<- (set-regspec-mode ppc::imm0 (gpr-mode-name-value
                                              (case resultspec
                                                (:address :address)
                                                (:signed-byte :s8)
                                                (:unsigned-byte :u8)
                                                (:signed-halfword :s16)
                                                (:unsigned-halfword :u16)
                                                (:signed-fullword :s32)
                                                (t :u32))))))))
      (^)))



             
(defppc2 ppc2-%temp-list %temp-list (seg vreg xfer arglist)
  (ppc2-use-operator (%nx1-operator list) seg vreg xfer arglist))

(defppc2 ppc2-%temp-cons %temp-cons (seg vreg xfer car cdr)
  (ppc2-use-operator (%nx1-operator cons) seg vreg xfer car cdr))


;;; Under MacsBug 5.3 (and some others ?), this'll do a low-level user
;;; break.  If the debugger doesn't recognize the trap instruction,
;;; you'll have to manually advance the PC past it.  "arg" winds up in the
;;; arg_z register; whatever's in arg_z on return is returned by
;;; the %debug-trap construct.

(defppc2 ppc2-%debug-trap %debug-trap (seg vreg xfer arg)
  (ppc2-one-targeted-reg-form seg arg ($ ppc::arg_z))
  (! %debug-trap)
  (<- ($ ppc::arg_z))
  (^))

(defppc2 ppc2-%reference-external-entry-point %reference-external-entry-point
  (seg vreg xfer arg)
  (ensuring-node-target (target vreg)
    (let* ((reg (if (eq (hard-regspec-value target) ppc::arg_z) ($ ppc::arg_y) ($ ppc::arg_z))))
      (ppc2-one-targeted-reg-form seg arg reg)
      (! eep.address target reg)))
  (^))

(defppc2 ppc2-%natural+ %natural+ (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil x)
      (ppc2-form seg nil xfer y))
    (let* ((fix-x (acode-fixnum-form-p x))
           (fix-y (acode-fixnum-form-p y)))
      (if (and fix-x fix-y)
        (ppc2-absolute-natural seg vreg xfer (+ fix-x fix-y))
        (let* ((u15x (and (typep fix-x '(unsigned-byte 15)) fix-x))
               (u15y (and (typep fix-y '(unsigned-byte 15)) fix-y)))
          (if (not (or u15x u15y))
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (ppc2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural+ xreg xreg yreg))
              (<- xreg))
            (let* ((other (if u15x y x)))
              (with-imm-target () (other-reg :natural)
                (ppc2-one-targeted-reg-form seg other other-reg)
                (! %natural+-c other-reg other-reg (or u15x u15y))
                (<- other-reg))))
          (^))))))

(defppc2 ppc2-%natural- %natural- (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil x)
      (ppc2-form seg nil xfer y))
    (let* ((fix-x (acode-fixnum-form-p x))
           (fix-y (acode-fixnum-form-p y)))
      (if (and fix-x fix-y)
        (ppc2-absolute-natural seg vreg xfer (- fix-x fix-y))
        (let* ((u15y (and (typep fix-y '(unsigned-byte 15)) fix-y)))
          (if (not u15y)
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (ppc2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural- xreg xreg yreg))
              (<- xreg))
            (progn
              (with-imm-target () (xreg :natural)
                (ppc2-one-targeted-reg-form seg x xreg)
                (! %natural--c xreg xreg u15y)
                (<- xreg))))
          (^))))))

(defppc2 ppc2-%natural-logior %natural-logior (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil x)
      (ppc2-form seg nil xfer y))
    (let* ((naturalx (nx-natural-constant-p x))
           (naturaly (nx-natural-constant-p y)))
      (if (and naturalx naturaly) 
        (ppc2-absolute-natural seg vreg xfer (logior naturalx naturaly))
        (let* ((u32x (nx-u32-constant-p x))
               (u32y (nx-u32-constant-p y))
               (constant (or u32x u32y)))
          (if (not constant)
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (ppc2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural-logior xreg xreg yreg))
              (<- xreg))
            (let* ((other (if u32x y x))
                   (high (ldb (byte 16 16) constant))
                   (low (ldb (byte 16 0) constant)))
              (with-imm-target () (other-reg :natural)
                (ppc2-one-targeted-reg-form seg other other-reg)
                (! %natural-logior-c other-reg other-reg high low)
                (<- other-reg))))
          (^))))))

(defppc2 ppc2-%natural-logxor %natural-logxor (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil x)
      (ppc2-form seg nil xfer y))
    (let* ((naturalx (nx-natural-constant-p x))
           (naturaly (nx-natural-constant-p y)))
      (if (and naturalx naturaly) 
        (ppc2-absolute-natural seg vreg xfer (logxor naturalx naturaly))
        (let* ((u32x (nx-u32-constant-p x))
               (u32y (nx-u32-constant-p y))
               (constant (or u32x u32y)))
          (if (not constant)
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (ppc2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural-logxor xreg xreg yreg))
              (<- xreg))
            (let* ((other (if u32x y x))
                   (high (ldb (byte 16 16) constant))
                   (low (ldb (byte 16 0) constant)))
              (with-imm-target () (other-reg :natural)
                (ppc2-one-targeted-reg-form seg other other-reg)
                (! %natural-logxor-c other-reg other-reg high low)
                (<- other-reg))))
          (^))))))

(defppc2 ppc2-%natural-logand %natural-logand (seg vreg xfer x y)
  (if (null vreg)
    (progn
      (ppc2-form seg nil nil x)
      (ppc2-form seg nil xfer y))
    (let* ((naturalx (nx-natural-constant-p x))
           (naturaly (nx-natural-constant-p y)))
      (if (and naturalx naturaly) 
        (ppc2-absolute-natural seg vreg xfer (logand naturalx naturaly))
        (let* ((u32x (nx-u32-constant-p x))
               (u32y (nx-u32-constant-p y))
               (constant (or u32x u32y)))
          (if (not constant)
            (with-imm-target () (xreg :natural)
              (with-imm-target (xreg) (yreg :natural)
                (ppc2-two-targeted-reg-forms seg x xreg y yreg)
                (! %natural-logand xreg xreg yreg))
              (<- xreg))
            (let* ((other (if u32x y x)))
              (with-imm-target () (other-reg :natural)
                (ppc2-one-targeted-reg-form seg other other-reg)
                (multiple-value-bind (start-bit stop-bit)
                    (ppc2-mask-bits constant)
                  (if start-bit
                    (! %natural-logand-mask-c other-reg other-reg start-bit stop-bit)
                    (let* ((high (ldb (byte 16 16) constant))
                           (low (ldb (byte 16 0) constant)))
                      (declare (type (unsigned-byte 16) high low))
                      (unless (and (= high #xffff)
                                   (= low high))
                        (if (= low 0)
                          (! %natural-logand-high-c other-reg other-reg high)
                          (if (= high 0)
                            (! %natural-logand-low-c other-reg other-reg low)
                            (with-imm-target (other-reg) (const-reg :natural)
                              (ppc2-absolute-natural seg const-reg nil constant)
                              (! %natural-logand other-reg other-reg const-reg))))))))
                (<- other-reg))))
          (^))))))

(defppc2 ppc2-natural-shift-right natural-shift-right (seg vreg xfer num amt)
  (with-imm-target () (dest :natural)
    (ppc2-one-targeted-reg-form seg num dest)
    (! natural-shift-right dest dest (acode-fixnum-form-p amt))
    (<- dest)
    (^)))

(defppc2 ppc2-natural-shift-left natural-shift-left (seg vreg xfer num amt)
  (with-imm-target () (dest :natural)
    (ppc2-one-targeted-reg-form seg num dest)
    (! natural-shift-left dest dest (acode-fixnum-form-p amt))
    (<- dest)
    (^)))

;;; This assumes that "global" variables are always boundp.
(defppc2 ppc2-global-ref global-ref (seg vreg xfer sym)
  (when vreg
    (ensuring-node-target (target vreg)
      (with-node-temps () (symreg)
        (setq symreg (or (ppc2-register-constant-p sym)
                         (ppc2-store-immediate seg sym symreg)))
        (! node-slot-ref target symreg (target-arch-case
                                        (:ppc32 ppc32::symbol.vcell-cell)
                                        (:ppc64 ppc64::symbol.vcell-cell))))))
  (^))

(defppc2 ppc2-global-setq global-setq (seg vreg xfer sym val)
  (ppc2-vset seg
             vreg
             xfer
             :symbol
             (make-acode (%nx1-operator immediate) sym)
             (make-acode (%nx1-operator fixnum)
                         (target-arch-case (:ppc32 ppc32::symbol.vcell-cell)
                                           (:ppc64 ppc64::symbol.vcell-cell)))
             val
             nil))

(defppc2 ppc2-%current-frame-ptr %current-frame-ptr (seg vreg xfer)
  (cond ((ppc2-tailcallok xfer)
	 (ppc2-restore-nvrs seg *ppc2-register-restore-ea* *ppc2-register-restore-count*)
	 (ppc2-restore-full-lisp-context seg)
	 (! %current-frame-ptr ($ ppc::arg_z))
	 (! jump-return-pc))
	(t
	 (when vreg
	   (ensuring-node-target (target vreg)
				 (! %current-frame-ptr target)))
	 (^))))

(defppc2 ppc2-%foreign-stack-pointer %foreign-stack-pointer (seg vreg xfer)
  (when vreg
    (ensuring-node-target (target vreg)
      (! %current-frame-ptr target)))
  (^))

(defppc2 ppc2-%current-tcr %current-tcr (seg vreg xfer)
  (when vreg
    (ensuring-node-target (target vreg)
      (! %current-tcr target)))
  (^))



(defppc2 ppc2-%interrupt-poll %interrupt-poll (seg vreg xfer)
  (! event-poll)
  (ppc2-nil seg vreg xfer))


(defppc2 ppc2-with-c-frame with-c-frame (seg vreg xfer body &aux
                                             (old-stack (ppc2-encode-stack)))
  (ecase (backend-name *target-backend*)
    (:linuxppc32 (! alloc-eabi-c-frame 0))
    ((:darwinppc32 :darwinppc64 :linuxppc64) (! alloc-c-frame 0)))
  (ppc2-open-undo $undo-ppc-c-frame)
  (ppc2-undo-body seg vreg xfer body old-stack))

(defppc2 ppc2-with-variable-c-frame with-variable-c-frame (seg vreg xfer size body &aux
                                                               (old-stack (ppc2-encode-stack)))
  (let* ((reg (ppc2-one-untargeted-reg-form seg size ppc::arg_z)))
    (ecase (backend-name *target-backend*)
      (:linuxppc32 (! alloc-variable-eabi-c-frame reg))
      ((:darwinppc32 :darwinppc64 :linuxppc64) (! alloc-variable-c-frame reg)))
    (ppc2-open-undo $undo-ppc-c-frame)
    (ppc2-undo-body seg vreg xfer body old-stack)))

(defppc2 ppc2-%symbol->symptr %symbol->symptr (seg vreg xfer sym)
  (let* ((src (ppc2-one-untargeted-reg-form seg sym ppc::arg_z)))
    (ensuring-node-target (target vreg)
      (! %symbol->symptr target src))
    (^)))

(defppc2 ppc2-%double-to-single %double-to-single (seg vreg xfer arg)
  (if (null vreg)
    (ppc2-form seg vreg xfer arg)
    (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-single))
      (let* ((dreg (ppc2-one-untargeted-reg-form 
                    seg arg
                    (make-wired-lreg (hard-regspec-value vreg)
                                     :class hard-reg-class-fpr
                                     :mode hard-reg-class-fpr-mode-double))))
        (! double-to-single vreg dreg)
        (^))
      (with-fp-target () (argreg :double-float)
        (ppc2-one-targeted-reg-form seg arg argreg)
        (with-fp-target ()  (sreg :single-float)
          (! double-to-single sreg argreg)
          (<- sreg)
          (^))))))

(defppc2 ppc2-%single-to-double %single-to-double (seg vreg xfer arg)
  (if (null vreg)
    (ppc2-form seg vreg xfer arg)
    (if (and (= (hard-regspec-class vreg) hard-reg-class-fpr)
             (= (get-regspec-mode vreg) hard-reg-class-fpr-mode-double))
      (progn
        (ppc2-one-untargeted-reg-form 
         seg arg
         (make-wired-lreg (hard-regspec-value vreg)
                          :class hard-reg-class-fpr
                          :mode hard-reg-class-fpr-mode-single))
        (^))
      (with-fp-target () (sreg :single-float)
        (ppc2-one-targeted-reg-form seg arg sreg)
        (<- (set-regspec-mode sreg hard-reg-class-fpr-mode-double))
        (^)))))

(defppc2 ppc2-%symptr->symvector %symptr->symvector (seg vreg xfer arg)
  (ppc2-identity seg vreg xfer arg))

(defppc2 ppc2-%symvector->symptr %symvector->symptr (seg vreg xfer arg)
  (ppc2-identity seg vreg xfer arg))

(defppc2 ppc2-%fixnum-to-double %fixnum-to-double (seg vreg xfer arg)
  (with-fp-target () (dreg :double-float)
    (let* ((r (ppc2-one-untargeted-reg-form seg arg ppc::arg_z)))
      (unless (or (acode-fixnum-form-p arg)
                  *ppc2-reckless*)
        (! trap-unless-fixnum r))
      (! fixnum->fpr dreg r)
      (<- dreg)
      (^))))

(defppc2 ppc2-%fixnum-to-single %fixnum-to-single (seg vreg xfer arg)
  (with-fp-target () (dreg :double-float)
    (let* ((r (ppc2-one-untargeted-reg-form seg arg ppc::arg_z)))
      (unless (or (acode-fixnum-form-p arg)
                  *ppc2-reckless*)
        (! trap-unless-fixnum r))
      (! fixnum->fpr dreg r)
      (<- (set-regspec-mode dreg hard-reg-class-fpr-mode-single))
      (^))))

(defppc2 ppc2-%double-float %double-float (seg vreg xfer arg)
  (let* ((real (or (acode-fixnum-form-p arg)
                   (let* ((form (acode-unwrapped-form arg)))
                     (if (and (acode-p form)
                              (eq (acode-operator form)
                                  (%nx1-operator immediate))
                              (typep (cadr form) 'real))
                       (cadr form))))))
    (if real
      (ppc2-immediate seg vreg xfer (float real 0.0d0))
      (if (ppc2-form-typep arg 'single-float)
        (ppc2-use-operator (%nx1-operator %single-to-double)
                           seg
                           vreg
                           xfer
                           arg)
        (if (ppc2-form-typep arg 'fixnum)
          (ppc2-use-operator (%nx1-operator %fixnum-to-double)
                             seg
                             vreg
                             xfer
                             arg)
          (ppc2-use-operator (%nx1-operator call)
                             seg
                             vreg
                             xfer
                             (make-acode (%nx1-operator immediate)
                                         '%double-float)
                             (list nil (list arg))))))))

(defppc2 ppc2-%single-float %single-float (seg vreg xfer arg)
  (let* ((real (or (acode-fixnum-form-p arg)
                   (let* ((form (acode-unwrapped-form arg)))
                     (if (and (acode-p form)
                              (eq (acode-operator form)
                                  (%nx1-operator immediate))
                              (typep (cadr form) 'real))
                       (cadr form))))))
    (if real
      (ppc2-immediate seg vreg xfer (float real 0.0f0))
      (if (ppc2-form-typep arg 'double-float)
        (ppc2-use-operator (%nx1-operator %double-to-single)
                           seg
                           vreg
                           xfer
                           arg)
        (if (ppc2-form-typep arg 'fixnum)
          (ppc2-use-operator (%nx1-operator %fixnum-to-single)
                             seg
                             vreg
                             xfer
                             arg)
          (ppc2-use-operator (%nx1-operator call)
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
(defppc2 ppc2-%new-ptr %new-ptr (seg vreg xfer size clear-p )
  (ppc2-call-fn seg
                vreg
                xfer
                (make-acode (%nx1-operator immediate)
                            '%new-gcable-ptr)
                (list nil (list clear-p size))
                nil))

