;;;-*-Mode: LISP; Package: ccl -*-
;;;
;;;   Copyright (C) 2008, Clozure Associates and contributors
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

;;; Shared compiler backend utilities and infrastructure.

(in-package "CCL")


(defun nx2-bigger-cdr-than (x y)
  (declare (cons x y))
  (> (the fixnum (cdr x)) (the fixnum (cdr y))))

;;; Return an unordered list of "varsets": each var in a varset can be
;;; assigned a register and all vars in a varset can be assigned the
;;; same register (e.g., no scope conflicts.)

(defun nx2-partition-vars (vars inherited-vars)
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
                   (var-refs var))
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
      (dolist (v vars) (push iv (var-binding-info v)))
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
(defun nx2-allocate-global-registers (fcells vcells all-vars inherited-vars nvrs)
  (if (null nvrs)
    (progn
      (dolist (c fcells) (%rplacd c nil))
      (dolist (c vcells) (%rplacd c nil))
      (values 0 nil))
    (let* ((maybe (nx2-partition-vars all-vars inherited-vars)))
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
              (setf (var-nvr var) regno))))))))

(defun nx2-assign-register-var (v)
  (var-nvr v))


(defun nx2-constant-form-p (form)
  (setq form (nx-untyped-form form))
  (if form
    (or (nx-null form)
        (nx-t form)
        (and (consp form)
             (or (eq (acode-operator form) (%nx1-operator immediate))
                 (eq (acode-operator form) (%nx1-operator fixnum))
                 (eq (acode-operator form) (%nx1-operator simple-function)))))))

(defun nx2-lexical-reference-p (form)
  (when (acode-p form)
    (let ((op (acode-operator (setq form (acode-unwrapped-form-value form)))))
      (when (or (eq op (%nx1-operator lexical-reference))
                (eq op (%nx1-operator inherited-arg)))
        (%cadr form)))))

;;; Returns true iff lexical variable VAR isn't setq'ed in FORM.
;;; Punts a lot ...
(defun nx2-var-not-set-by-form-p (var form)
  (let* ((bits (nx-var-bits var)))
    (or (not (%ilogbitp $vbitsetq bits))
        (nx2-setqed-var-not-set-by-form-p var form (logbitp $vbitclosed bits)))))

(defun nx2-setqed-var-not-set-by-form-p (var form &optional closed)
  (setq form (acode-unwrapped-form form))
  (or (atom form)
      (nx2-constant-form-p form)
      (nx2-lexical-reference-p form)
      (let ((op (acode-operator form))
            (subforms nil))
        (if (eq op (%nx1-operator setq-lexical))
          (and (neq var (cadr form))
               (nx2-setqed-var-not-set-by-form-p var (caddr form)))
          (and (or (not closed)
                   (logbitp operator-side-effect-free-bit op))
               (flet ((not-set-in-formlist (formlist)
                        (dolist (subform formlist t)
                          (unless (nx2-setqed-var-not-set-by-form-p var subform closed) (return)))))
                 (if
                   (cond ((%ilogbitp operator-acode-subforms-bit op) (setq subforms (%cdr form)))
                         ((%ilogbitp operator-acode-list-bit op) (setq subforms (cadr form))))
                   (not-set-in-formlist subforms)
                   (and (or (eq op (%nx1-operator call))
                            (eq op (%nx1-operator lexical-function-call)))
                        (nx2-setqed-var-not-set-by-form-p var (cadr form))
                        (setq subforms (caddr form))
                        (not-set-in-formlist (car subforms))
                        (not-set-in-formlist (cadr subforms))))))))))

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
      (destructuring-bind (req opt rest keys &rest ignore) (cdr alambda)
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
                