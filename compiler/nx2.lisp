;;;-*-Mode: LISP; Package: ccl -*-
;;;
;;;   Copyright (C) 2008-2009 Clozure Associates and contributors
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

;;; Shared compiler backend utilities and infrastructure.

(in-package "CCL")


(defun nx2-bigger-cdr-than (x y)
  (declare (cons x y))
  (> (cdr x) (cdr y)))


;;; Return an unordered list of "varsets": each var in a varset can be
;;; assigned a register and all vars in a varset can be assigned the
;;; same register (e.g., no scope conflicts.)

(defun nx2-partition-vars (vars inherited-vars &optional (afunc-flags 0))
  (labels ((var-weight (var)
             (let* ((bits (nx-var-bits var)))
               (declare (fixnum bits))
               (if (eql 0 (logand bits (logior
                                        (ash 1 $vbitpuntable)
                                        (ash -1 $vbitspecial)
                                        (ash 1 $vbitnoreg))))
                 (if (or (var-nvr var)  ; already assiged a register via other means
                         (eql (logior (ash 1 $vbitclosed) (ash 1 $vbitsetq))
                              (logand bits (logior (ash 1 $vbitclosed) (ash 1 $vbitsetq)))))
                   0
                   (let* ((w (var-refs var)))
                     (if (logbitp $fbittailcallsself afunc-flags)
                       (ash w 2)
                       w)))
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
    (dolist (iv inherited-vars)
      (dolist (v vars) (pushnew iv (var-binding-info v) :test #'eq))
      (push iv vars))
    (setq vars (%sort-list-no-key
                ;;(delete-if #'(lambda (v) (eql (var-weight v) 0)) vars) 
                (do* ((handle (cons nil vars))
                      (splice handle))
                     ((null (cdr splice)) (cdr handle))                  
                  (declare (dynamic-extent handle) (type cons handle splice))
                  (if (eql 0 (var-weight (%car (cdr splice))))
                    (rplacd splice (%cdr (cdr splice)))
                    (setq splice (cdr splice))))
                #'(lambda (v1 v2) (%i> (var-weight v1) (var-weight v2)))))
    ;; This isn't optimal.  It partitions all register-allocatable
    ;; variables into sets such that 1) no variable is a member of
    ;; more than one set and 2) all variables in a given set are
    ;; disjoint from each other A set might have exactly one member.
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
(defun nx2-afunc-allocate-global-registers (afunc nvrs)
  (let* ((vcells (afunc-vcells afunc))
         (fcells (afunc-fcells afunc))
         (all-vars (afunc-all-vars afunc))
         (inherited-vars (afunc-inherited-vars afunc)))
    (if (null nvrs)
      (progn
        (dolist (c fcells) (%rplacd c nil))
        (dolist (c vcells) (%rplacd c nil))
        (values 0 nil))
      (let* ((maybe (nx2-partition-vars
                     all-vars
                     inherited-vars
                     (afunc-bits afunc))))
        (dolist (c fcells) 
          (if (>= (the fixnum (cdr c)) 3) (push c maybe)))
        (dolist (c vcells) 
          (if (>= (the fixnum (cdr c)) 3) (push c maybe)))
        (do* ((things (%sort-list-no-key maybe #'nx2-bigger-cdr-than) (cdr things))
              (n 0 (1+ n))
              (registers nvrs)
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
                (setf (var-nvr var) regno)))))))))

(defun nx2-assign-register-var (v)
  (var-nvr v))

(defun nx2-select-fpr-candidates (vars &optional restricted)
  (let* ((fvars ()))
    (dolist (v vars (%sort-list-no-key (nx2-partition-vars fvars nil)
                                       #'nx2-bigger-cdr-than))
      (unless (member v restricted :test #'eq)
        (let* ((bits (nx-var-bits v)))
          (declare (fixnum bits))
          (when (eql 0 (logand bits (logior 
                                     (ash 1 $vbitpuntable)
                                     (ash -1 $vbitspecial)
                                     (ash 1 $vbitnoreg)
                                     (ash 1 $vbitdynamicextent)
                                     (ash 1 $vbitclosed))))
            (if (logbitp $vbitsetq bits)
              (setf (var-refs v) (ash (var-refs v) 2))
              (unless (var-declared-type v)
))
            (let* ((type (var-declared-type v)))
              (when (and (or (eq type 'single-float)
                             (eq type 'double-float))
                         (logbitp $vbitsetq bits))
                (push v fvars)))))))))
          
                
              

(defun nx2-constant-form-p (form)
  (setq form (nx-untyped-form form))
  (if form
    (or (nx-null form)
        (nx-t form)
        (and (acode-p form)
             (or (eq (acode-operator form) (%nx1-operator immediate))
                 (eq (acode-operator form) (%nx1-operator fixnum))
                 (eq (acode-operator form) (%nx1-operator simple-function)))))))

(defun nx2-lexical-reference-p (form)
  (when (acode-p form)
    (let ((op (acode-operator (setq form (acode-unwrapped-form-value form)))))
      (when (or (eq op (%nx1-operator lexical-reference))
                (eq op (%nx1-operator inherited-arg)))
        (car (acode-operands form))))))

(defun nx2-acode-call-p (form)
  (when (acode-p form)
    (let ((op (acode-operator (acode-unwrapped-form-value form))))
      (or (eq op (%nx1-operator multiple-value-call))
          (eq op (%nx1-operator call))
          (eq op (%nx1-operator lexical-function-call))
          (eq op (%nx1-operator self-call))
          (eq op (%nx1-operator builtin-call))))))
          
  

;;; Returns true iff lexical variable VAR isn't setq'ed in FORM.
;;; Punts a lot ...
(defun nx2-var-not-set-by-form-p (var form)
  (let* ((bits (nx-var-bits var)))
    (or (not (%ilogbitp $vbitsetq bits))
        (nx2-setqed-var-not-set-by-form-p var form (logbitp $vbitclosed bits)))))

(defun nx2-setqed-var-not-set-by-form-p (var form &optional closed)
  (setq form (acode-unwrapped-form form))
  (or (not (acode-p form))
      (nx2-constant-form-p form)
      (nx2-lexical-reference-p form)
      (let ((op (acode-operator form))
            (operands (acode-operands form))
            (subforms nil))
        (if (eq op (%nx1-operator setq-lexical))
          (and (neq var (car operands))
               (nx2-setqed-var-not-set-by-form-p var (cadr operands)))
          (and (or (not closed)
                   (logbitp operator-side-effect-free-bit op))
               (flet ((not-set-in-formlist (formlist)
                        (dolist (subform formlist t)
                          (unless (nx2-setqed-var-not-set-by-form-p var subform closed) (return)))))
                 (if
                   (cond ((%ilogbitp operator-acode-subforms-bit op) (setq subforms operands))
                         ((%ilogbitp operator-acode-list-bit op) (setq subforms (car operands))))
                   (not-set-in-formlist subforms)
                   (and (or (eq op (%nx1-operator call))
                            (eq op (%nx1-operator lexical-function-call)))
                        (nx2-setqed-var-not-set-by-form-p var (car operands))
                        (setq subforms (cadr operands))
                        (not-set-in-formlist (car subforms))
                        (not-set-in-formlist (cadr subforms))))))))))

(defun nx2-var-not-reffed-by-form-p (var form &optional closed)
  (setq form (acode-unwrapped-form form))
  (unless (eq var (nx2-lexical-reference-p form))
    (or (not (acode-p form))
        (nx2-lexical-reference-p form)  ;not us
        (nx2-constant-form-p form)
        (let ((op (acode-operator form))
              (operands (acode-operands form))
              (subforms nil))
          (if (eq op (%nx1-operator setq-lexical))
            (and (neq var (car operands))
                 (nx2-var-not-reffed-by-form-p var (cadr operands)))
            (and (or (not closed)
                     (logbitp operator-side-effect-free-bit op))
                 (flet ((not-reffed-in-formlist (formlist)
                          (dolist (subform formlist t)
                            (unless (nx2-var-not-reffed-by-form-p var subform closed) (return)))))
                   (if
                     (cond ((%ilogbitp operator-acode-subforms-bit op) (setq subforms operands))
                           ((%ilogbitp operator-acode-list-bit op) (setq subforms (car operands))))
                     (not-reffed-in-formlist subforms)
                     (and (or (eq op (%nx1-operator call))
                              (eq op (%nx1-operator lexical-function-call)))
                          (nx2-var-not-reffed-by-form-p var (car operands))
                          (setq subforms (cadr operands))
                          (not-reffed-in-formlist (car subforms))
                          (not-reffed-in-formlist (cadr subforms)))))))))))

(defun nx2-node-gpr-p (reg)
  (and reg
       (eql (hard-regspec-class reg) hard-reg-class-gpr)
       (eql (get-regspec-mode reg) hard-reg-class-gpr-mode-node)))

;;; ENTRIES is a list of recorded-symbol entries, built by pushing
;;; info for each variable referenced by the function AFUNC as it
;;; comes into scope.  (Inherited variables "come into scope" before
;;; anything else, then required arguments, etc.)  Supplied-p variables
;;; may come into scope before "real" arglist entries do, which confuses
;;; functions that try to construct a function's arglist from the symbol
;;; map.  I -think- that confusion only exists when supplied-p variables
;;; are involved, so this returns its first argument unless they are;
;;; otherwise, it ensures that all toplevel arglist symbols are followed
;;; only by any inherited variables, and that the arglist symbols are
;;; in the correct (reversed) order
(defun nx2-recorded-symbols-in-arglist-order (entries afunc)
  (let* ((alambda (afunc-acode afunc)))
    (when (and (acode-p alambda)
               (eq (acode-operator alambda) (%nx1-operator lambda-list)))
      (destructuring-bind (req opt rest keys &rest ignore) (acode-operands alambda)
        (declare (ignore ignore))
        (when (or (dolist (sp (caddr opt))
                    (when sp (return t)))
                  (dolist (sp (caddr keys))
                    (when sp (return t))))
          (let* ((new ()))
            (flet ((info-for-var (var)
                     (assoc var entries :test #'eq)))
              (flet ((add-new-info (var)
                       (let* ((info (info-for-var var)))
                         (when info
                           (push info new)))))
                (setq entries (nreverse entries))
                (dolist (var (afunc-inherited-vars afunc))
                  (add-new-info var))
                (dolist (r req)
                  (add-new-info r))
                (dolist (o (car opt))
                  (add-new-info o))
                (when (consp rest)
                  (setq rest (car rest)))
                (when rest
                  (add-new-info rest))
                (dolist (k (cadr keys))
                  (add-new-info k))
                (dolist (e entries)
                  (unless (member e new :test #'eq)
                    (push e new)))
                (setq entries new)))))))
    entries))

(defun nx2-replace-var-refs (var value)
  (when (acode-p value)
    (let* ((op (acode-operator value))
           (operands (acode-operands value)))
      (when (typep op 'fixnum)
        (dolist (ref (var-ref-forms var) (setf (var-ref-forms var) nil))
          (when (acode-p ref)
            (setf (acode-operator ref) op
                  (acode-operands ref) operands)))))))

(defun acode-immediate-operand (x)
  (let* ((x (acode-unwrapped-form x)))
    (if (eq (acode-operator x) (%nx1-operator immediate))
      (car (acode-operands x))
      (compiler-bug "not an immediate: ~s" x))))

(defun nx2-constant-index-ok-for-type-keyword (idx keyword)
  (when (>= idx 0)
    (let* ((arch (backend-target-arch *target-backend*))
           (limit
            (case keyword
              ((:bignum 
                :single-float 
                :double-float 
                :xcode-vector
                :signed-32-bit-vector 
                :unsigned-32-bit-vector 
                :single-float-vector 
                :simple-string)
               (arch::target-max-32-bit-constant-index arch))
              (:bit-vector (arch::target-max-1-bit-constant-index arch))
              ((:signed-8-bit-vector :unsigned-8-bit-vector)
               (arch::target-max-8-bit-constant-index arch))
              ((:signed-16-bit-vector :unsigned-16-bit-vector)
               (arch::target-max-16-bit-constant-index arch))
              ((:signed-64-bit-vector 
                :unsigned-64-bit-vector 
                :double-float-vector)
               (arch::target-max-64-bit-constant-index arch))
              (t
               ;; :fixnum or node
               (target-word-size-case
                (32 (arch::target-max-32-bit-constant-index arch))
                (64 (arch::target-max-64-bit-constant-index arch)))))))
      (and limit (< idx limit)))))

(defun backend-use-operator (op seg vreg xfer &rest forms)
  (declare (dynamic-extent forms))
  (apply (svref (backend-p2-dispatch *target-backend*)
                (%ilogand op operator-id-mask))
         seg vreg xfer forms))

(defun backend-apply-acode (acode seg vreg xfer)
  (apply (svref (backend-p2-dispatch *target-backend*)
                (%ilogand (acode-operator acode) operator-id-mask))
         seg vreg xfer (acode-operands acode)))



(defun acode-constant-p (form)
  ;; This returns (values constant-value constantp); some code
  ;; may need to check constantp if constant-value is nil.
  (let* ((form (acode-unwrapped-form-value form))
         (op (if (acode-p form) (acode-operator form))))
    (cond ((eql op (%nx1-operator nil))
           (values nil t))
          ((eql op (%nx1-operator t))
           (values t t))
          ((eql op (%nx1-operator fixnum))
           (values (car (acode-operands form)) t))
          ((eql op (%nx1-operator immediate))
           ;; recognize the acode produced for LOAD-TIME-VALUE by
           ;; COMPILE-FILE as something non-constant.
           (if (and 
                (consp (car (acode-operands form)))
                *load-time-eval-token*
                (eq (car (car (acode-operands form))) *load-time-eval-token*))
             (values nil nil)
                             
                    
             (values (car (acode-operands form)) t)))
          (t (values nil nil)))))

(defun acode-constant-fold-binop (seg vreg xfer x y function)
  (multiple-value-bind (const-x x-p) (acode-constant-p x)
    (when x-p
      (multiple-value-bind (const-y y-p) (acode-constant-p y)
        (when y-p
          (let* ((result (ignore-errors (funcall function const-x const-y))))
            (when result
              (backend-use-operator (if (nx1-target-fixnump result)
                                      (%nx1-operator fixnum)
                                      (%nx1-operator immediate))
                                    seg
                                    vreg
                                    xfer
                                    result)
              t)))))))






        



(defun acode-optimize-minus1 (seg vreg xfer form trust-decls &optional (result-type 'number))
  (declare (ignorable result-type))
  (multiple-value-bind (val constp) (acode-constant-p form)
    (cond ((and (and constp (ignore-errors (setq val (- val)))))
           (backend-use-operator (if (typep val *nx-target-fixnum-type*)
                                   (%nx1-operator fixnum)
                                   (%nx1-operator immediate))
                                 seg vreg xfer val)
           t)
          ((acode-form-typep form 'double-float trust-decls)
           (backend-use-operator (%nx1-operator %double-float-negate) seg vreg xfer form)
           t)
          ((acode-form-typep form 'single-float trust-decls)
           (backend-use-operator (%nx1-operator %single-float-negate) seg vreg xfer form)
           t)
          ((acode-form-typep form *nx-target-fixnum-type* trust-decls)
           (backend-use-operator (%nx1-operator %ineg) seg vreg xfer form)
           t))))

(defun nx2-is-comparison-of-var-to-fixnums (form)
  ;; Catches some cases.  May miss some.
  (flet ((is-simple-comparison-of-var-to-fixnum (form)
           (let* ((var nil)
                  (fixval nil))
             (setq form (acode-unwrapped-form form))
             (when (acode-p form)
               (let* ((op (acode-operator form)))
                 (cond ((eql op (%nx1-operator eq))
                        (destructuring-bind (cc x y) (acode-operands form)
                          (when (eq :eq (acode-immediate-operand cc))
                            (if (setq var (nx2-lexical-reference-p x))
                              (setq fixval (acode-fixnum-form-p y))
                              (if (setq var (nx2-lexical-reference-p y))
                                (setq fixval (acode-fixnum-form-p x)))))))
                       ((eql op (%nx1-operator %izerop))
                        (destructuring-bind (cc val) (acode-operands form)
                          (when (eq :eq (acode-immediate-operand cc))
                            (setq var (nx2-lexical-reference-p val)
                                  fixval 0)))))))
             (if (and var fixval)
               (values var fixval)
               (values nil nil)))))
    (setq form (acode-unwrapped-form form))
    (multiple-value-bind (var val) (is-simple-comparison-of-var-to-fixnum form)
      (if var
        (values var (list val))
        (if (and (acode-p form) (eql (acode-operator form) (%nx1-operator or)))
          (collect ((vals))
            (let* ((clauselist (car (acode-operands  form))))
              (if (multiple-value-setq (var val) (is-simple-comparison-of-var-to-fixnum (car clauselist)))
                (progn
                  (vals val)
                  (dolist (clause (cdr clauselist) (values var (vals)))
                    (multiple-value-bind (var1 val1)
                        (is-simple-comparison-of-var-to-fixnum clause)
                      (unless (eq var var1)
                        (return (values nil nil)))
                      (vals val1))))
                (values nil nil)))))))))
           


                    
               
        
                
;;; If an IF form (in acode) appears to be the expansion of a
;;; CASE/ECASE/CCASE where all values are fixnums, try to recover
;;; that information and let the backend decide what to do with it.
;;; (A backend might plausibly replace a sequence of comparisons with
;;; a jumptable.)
;;; Returns 4 values: a list of lists of fixnums, the corresponding true
;;; forms for each sublist, the variable being tested, and the "otherwise"
;;; or default form.
;;; Something like (IF (EQL X 1) (FOO) (BAR)) will return non-nil values.
;;; The backend -could- generate a jump table in that case, but probably
;;; wouldn't want to.
(defun nx2-reconstruct-case (test true false)
  (multiple-value-bind (var vals) (nx2-is-comparison-of-var-to-fixnums test)
    (if (not var)
      (values nil nil nil nil)
      (collect ((ranges)
                (trueforms))
        (let* ((otherwise nil))
          (ranges vals)
          (trueforms true)
          (labels ((descend (original)
                     (let* ((form (acode-unwrapped-form original)))
                       (if (or (not (acode-p form))
                               (not (eql (acode-operator form)
                                         (%nx1-operator if))))
                         (setq otherwise original)
                         (destructuring-bind (test true false) (acode-operands form)
                           (multiple-value-bind (v vals)
                               (nx2-is-comparison-of-var-to-fixnums test)
                             (cond ((eq v var)
                                    (ranges vals)
                                    (trueforms true)
                                    (descend false))
                                   (t (setq otherwise original)))))))))
            (descend false))
          (values (ranges) (trueforms) var otherwise))))))

(defun acode-var-type (var trust-decls)
  (do* ((var var bits)
        (bits (var-bits var) (var-bits var)))
       ((typep bits 'fixnum)
        (or (var-type var)
            (setf (var-type var)
                  (let* ((initform (var-initform var)))
                    (cond ((and initform (not (logbitp $vbitsetq bits)))
                           (acode-form-type initform trust-decls))
                          ((and trust-decls (var-declared-type var)))
                          (t '*))))))))

            