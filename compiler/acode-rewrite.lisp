;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2007-2009 Clozure Associates
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


(defvar *acode-post-trust-decls* nil)

;;; Rewrite acode trees.

(next-nx-defops)
(defvar *acode-rewrite-functions* nil)
(let* ((newsize (%i+ (next-nx-num-ops) 10))
       (old *acode-rewrite-functions*)
       (oldsize (length old)))
  (declare (fixnum newsize oldsize))
  (unless (>= oldsize newsize)
    (let* ((v (make-array newsize :initial-element nil)))
      (dotimes (i oldsize (setq *acode-rewrite-functions* v))
        (setf (svref v i) (svref old i))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-acode-rewrite (name operator-list arglist &body body)
    (if (atom operator-list)
      (setq operator-list (list operator-list)))
    (multiple-value-bind (body decls)
        (parse-body body nil t)
      (collect ((let-body))
        (dolist (operator operator-list)
          (let-body `(setf (svref *acode-rewrite-functions* (logand operator-id-mask (%nx1-operator ,operator))) fun)))
        (destructuring-bind (op whole type) arglist
        `(let* ((fun (nfunction ,name 
                                (lambda (,op ,whole ,type)
                                  (declare (ignorable ,op ,type))
                                  ,@decls
                                  (block ,name ,@body)))))
          ,@(let-body)))))))

;;; Don't walk the form (that's already happened.)
(defun acode-post-form-type (form)
  (when (acode-p form)
    (let* ((op (acode-operator form))
           (operands (cdr form)))
      (cond ((and *acode-post-trust-decls*
                  (eq op (%nx1-operator typed-form)))
             (acode-operand 0 operands))
            ((eq op (%nx1-operator fixnum))
             'fixnum)
            ((eq op (%nx1-operator immediate))
             (type-of (acode-operand 0 operands)))
            (t t)))))

(defun acode-constant-p (form)
  (let* ((form (acode-unwrapped-form-value form)))
    (or (eq form *nx-nil*)
        (eq form *nx-t*)
        (let* ((operator (if (acode-p form) (acode-operator form))))
          (or (eq operator (%nx1-operator fixnum))
              (eq operator (%nx1-operator immediate)))))))

(defun acode-post-form-typep (form type)
  (let* ((ctype (specifier-type type))
         (form (acode-unwrapped-form-value form)))
    (cond ((eq form *nx-nil*) (ctypep nil ctype))
          ((eq form *nx-t*) (ctypep t ctype))
          ((not (acode-p form)) (values nil nil))
          (t
           (let* ((op (acode-operator form))
                  (operands (cdr form)))
             (cond ((and *acode-post-trust-decls*
                         (eq op (%nx1-operator typed-form)))
                    (subtypep (acode-operand 0 operands) type))
                   ((or (eq op (%nx1-operator fixnum))
                        (eq op (%nx1-operator immediate)))
                    (ctypep (acode-operand 0 operands) (specifier-type type)))
                   (t (values nil nil))))))))

             

(defun rewrite-acode-ref (ref &optional (type t))
  (let* ((form (car ref)))
    (if (acode-p form)
      (let* ((op (acode-operator form))
             (rewrite (svref *acode-rewrite-functions* (logand op operator-id-mask))))
        (when rewrite
          (let* ((new (funcall rewrite op (cdr form) type)))
            (when new
              (setf (car ref) new)
              t)))))))

;;; Maybe ewrite the operands of a binary real arithmetic operation
(defun acode-post-binop-numeric-contagion (pform1 pform2)
  (let* ((form1 (car pform1))
         (form2 (car pform2)))
    (cond ((acode-post-form-typep form1 'double-float)
           (unless (acode-post-form-typep form2 'double-float)
             (let* ((c2 (acode-real-constant-p form2)))
               (if c2
                 (setf (car pform2)
                       (make-acode (%nx1-operator immediate)
                                   (float c2 0.0d0)))
                 (if (acode-post-form-typep form2 'fixnum)
                   (setf (car pform2)
                         (make-acode (%nx1-operator typed-form)
                                     'double-float
                                     (make-acode (%nx1-operator %fixnum-to-double)
                                                 form2))))))))
          ((acode-post-form-typep form2 'double-float)
           (let* ((c1 (acode-real-constant-p form1)))
             (if c1
               (setf (car pform1)
                     (make-acode (%nx1-operator immediate)
                                 (float c1 0.0d0)))
               (if (acode-post-form-typep form1 'fixnum)
                 (setf (car pform1)
                       (make-acode (%nx1-operator typed-form)
                                   'double-float
                                   (make-acode (%nx1-operator %fixnum-to-double)
                                               form1)))))))
          ((acode-post-form-typep form1 'single-float)
           (unless (acode-post-form-typep form2 'single-float)
             (let* ((c2 (acode-real-constant-p form2)))
               (if c2
                 (setf (car pform2) (make-acode (%nx1-operator immediate)
                                                (float c2 0.0f0)))
                 (if (acode-post-form-typep form2 'fixnum)
                   (setf (car pform2)
                         (make-acode (%nx1-operator typed-form)
                                     'single-float
                                     (make-acode (%nx1-operator %fixnum-to-single)
                                                 form2))))))))
          ((acode-post-form-typep form2 'single-float)
           (let* ((c1 (acode-real-constant-p form1)))
             (if c1
               (setf (car pform1) (make-acode (%nx1-operator immediate)
                                              (float c1 0.0f0)))

               (if (acode-post-form-typep form1 'fixnum)
                 (setf (car pform1)
                       (make-acode (%nx1-operator typed-form)
                                   'single-float
                                   (make-acode (%nx1-operator %fixnum-to-single)
                                               form1))))))))))

(defun constant-fold-acode-binop (function x y)
  (let* ((constant-x (acode-real-constant-p x))
         (constant-y (acode-real-constant-p y)))
    (if (and constant-x constant-y)
      (let* ((result (ignore-errors (funcall function x y))))
        (when result
          (nx1-form result))))))

(defun acode-rewrite-and-fold-binop (function args)
  (rewrite-acode-ref args)
  (rewrite-acode-ref (cdr args))
  (constant-fold-acode-binop function (car args) (cadr args)))

(defun rewrite-acode-forms (forms)
  (do* ((head forms (cdr head)))
       ((null head))
    (rewrite-acode-ref head)))

(defun acode-assert-type (actualtype operator operands assertedtype)
  (make-acode (%nx1-operator typed-form)
              (type-specifier (type-intersection (specifier-type actualtype)
                                                 (specifier-type assertedtype)))
              (cons operator operands)))

(def-acode-rewrite acode-rewrite-progn progn (op w type)
  (rewrite-acode-forms w))

(def-acode-rewrite acode-rewrite-not not (op w type)
  (rewrite-acode-ref w))

(def-acode-rewrite acode-rewrite-%i+ %i+ (op w type)
  (or 
   (acode-rewrite-and-fold-binop '+ w)
   ;; TODO: maybe cancel overflow check, assert FIXNUM result.
   (acode-assert-type 'integer op w type)))

(def-acode-rewrite acode-rewrite-%i- %i- (op w type)
  (or
   (acode-rewrite-and-fold-binop '- w))
   ;; TODO: maybe cancel overflow check, assert FIXNUM result.
   (acode-assert-type 'integer op w type))  

(def-acode-rewrite acode-rewrite-%ilsl %ilsl (op w type)
  (or
   (acode-rewrite-and-fold-binop '%ilsl w)
   (acode-assert-type 'fixnum op w type)))

(def-acode-rewrite acode-rewrite-%ilogand2 %ilogand2 (op w type)
  (or
   (acode-rewrite-and-fold-binop 'logand w)
   ;; If either argument's an UNSIGNED-BYTE constant, the result
   ;; is an UNSIGNED-BYTE no greater than that constant.
   (destructuring-bind (x y) w
     (let* ((fix-x (acode-fixnum-form-p x))
            (fix-y (acode-fixnum-form-p y)))
       (acode-assert-type (if fix-x
                            `(integer 0 ,fix-x)
                            (if fix-y
                              `(integer 0 ,fix-y)
                              'fixnum))
                          op w type)))))

(def-acode-rewrite acode-rewrite-%ilogior2 %ilogior2 (op w type)
  (or
   (acode-rewrite-and-fold-binop 'logior w)
   ;; If either argument's an UNSIGNED-BYTE constant, the result
   ;; is an UNSIGNED-BYTE no greater than that constant.
   (destructuring-bind (x y) w
     (let* ((fix-x (acode-fixnum-form-p x))
            (fix-y (acode-fixnum-form-p y)))
       (acode-assert-type (if fix-x
                            `(integer 0 ,fix-x)
                            (if fix-y
                              `(integer 0 ,fix-y)
                              'fixnum))
                          op w type)))))

(def-acode-rewrite acode-rewrite-ilogbitp (logbitp %ilogbitp) (op w type)
  (or (acode-rewrite-and-fold-binop 'logbitp w)
      (acode-assert-type 'boolean op w type)))

(def-acode-rewrite acode-rewrite-eq eq (op w type)
  (or (acode-rewrite-and-fold-binop 'eq w)
      (acode-assert-type 'boolean op w type)))

(def-acode-rewrite acode-rewrite-neq neq (op w type)
  (or (acode-rewrite-and-fold-binop 'neq w)
      (acode-assert-type 'boolean op w type))  )

(def-acode-rewrite acode-rewrite-list list (op w type)
  (rewrite-acode-forms (car w))
  (acode-assert-type 'list op w type))

(def-acode-rewrite acode-rewrite-values values (op w type)
  (rewrite-acode-forms (car w)))

(def-acode-rewrite acode-rewrite-if if (op w type)
  (rewrite-acode-forms w)
  (destructuring-bind (test true &optional (false *nx-nil*)) w
    (if (acode-constant-p test)
      (if (eq *nx-nil* (acode-unwrapped-form-value test))
        false
        true))))

(def-acode-rewrite acode-rewrite-or or (op w type)
  (rewrite-acode-forms (car w))
  ;; Try to short-circuit if there are any true constants.
  ;; The constant-valued case will return a single value.
  (do* ((forms w (cdr forms)))
       ((null (cdr forms)))
    (let* ((form (car forms)))
      (when (and (acode-constant-p form)
                 (not (eq *nx-nil* (acode-unwrapped-form-value form))))
        (progn
          (rplacd forms nil)
          (return))))))

(def-acode-rewrite acode-rewrite-%fixnum-ref (%fixnum-ref %fixnum-ref-natural) (op w type)
  (rewrite-acode-forms w))

(def-acode-rewrite acode-rewrite-multiple-value-prog1 multiple-value-prog1 (op w type)
  (rewrite-acode-forms w))

(def-acode-rewrite acode-rewrite-multiple-value-bind multiple-value-bind (op w type)
  (rewrite-acode-forms (cdr w)))

(def-acode-rewrite acode-rewrite-multiple-value-call multiple-value-call (op w type)
  (rewrite-acode-forms w))

(def-acode-rewrite acode-rewrite-typed-form typed-form (op w type)
  (let* ((ourtype (car w)))
    (rewrite-acode-ref (cdr w) ourtype)
    (let* ((subform (cadr w)))
      (and (acode-p subform) (eq (acode-operator subform) op) subform))))

;; w: vars, list of initial-value forms, body
(def-acode-rewrite acode-rewrite-let (let let*) (op w type)
  (collect ((newvars)
            (newvals))
    (do* ((vars (car w) (cdr vars))
          (vals (cadr w) (cdr vals)))
         ((null vars)
          (rplaca w (newvars))
          (rplaca (cdr w) (newvals))
          (rewrite-acode-ref (cddr w))
          (unless (car w) (caddr w)))
      (rewrite-acode-ref (car vals))
      (let* ((var (car vars))
             (bits (nx-var-bits var)))
        (cond ((logbitp $vbitpuntable bits)
               (setf (var-bits var)
                     (logior (ash 1 $vbitpunted) bits)
                     (var-ea var) (car vals)))
              (t
               (newvars var)
               (newvals (car vals))))))))
        
    
      



(def-acode-rewrite acode-rewrite-lexical-reference lexical-reference (op w type)
  (let* ((var (car w)))
    (if (acode-punted-var-p var)
      (var-ea var))))

(def-acode-rewrite acode-rewrite-add2 add2 (op w type)
  (or (acode-rewrite-and-fold-binop '+ w)
      (progn
        (acode-post-binop-numeric-contagion w (cdr w))
        (let* ((xtype (acode-post-form-type (car w)))
               (ytype (acode-post-form-type (cadr w))))
          (cond ((and (subtypep xtype 'double-float)
                      (subtypep ytype 'double-float))
                 (make-acode (%nx1-operator typed-form)
                             'double-float
                             (make-acode* (%nx1-operator %double-float+-2)
                                          w)))
                ((and (subtypep xtype 'single-float)
                      (subtypep ytype 'single-float))
                 (make-acode (%nx1-operator typed-form)
                             'single-float
                             (make-acode* (%nx1-operator %short-float+-2)
                                          w)))
                ((and (subtypep xtype 'fixnum)
                      (subtypep ytype 'fixnum))
                 (make-acode (%nx1-operator typed-form)
                             'fixnum
                             (make-acode (%nx1-operator %i+)
                                         (car w)
                                         (cadr w)
                                         (not (subtypep type 'fixnum))))))))))

(def-acode-rewrite acode-rewrite-sub2 sub2 (op w type)
  (or (acode-rewrite-and-fold-binop '- w)
      (progn
        (acode-post-binop-numeric-contagion w (cdr w))
        (let* ((xtype (acode-post-form-type (car w)))
               (ytype (acode-post-form-type (cadr w))))
          (cond ((and (subtypep xtype 'double-float)
                      (subtypep ytype 'double-float))
                 (make-acode (%nx1-operator typed-form)
                             'double-float
                             (make-acode* (%nx1-operator %double-float--2)
                                          w)))
                ((and (subtypep xtype 'single-float)
                      (subtypep ytype 'single-float))
                 (make-acode (%nx1-operator typed-form)
                             'single-float
                             (make-acode* (%nx1-operator %short-float--2)
                                          w)))
                ((and (subtypep xtype 'fixnum)
                      (subtypep ytype 'fixnum))
                 (make-acode (%nx1-operator typed-form)
                             'fixnum
                             (make-acode (%nx1-operator %i-)
                                         (car w)
                                         (cadr w)
                                         (not (subtypep type 'fixnum))))))))))
                 

