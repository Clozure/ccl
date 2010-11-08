;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2007-2010 Clozure Associates
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


(defvar *acode-rewrite-tail-allow* nil)
(defvar *acode-rewrite-reckless* nil)
(defvar *acode-rewrite-open-code-inline* nil)
(defvar *acode-rewrite-trust-declarations* nil)
(defvar *acode-rewrite-full-safety* nil)


;;; Rewrite acode trees.

;(next-nx-defops)
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
  (defmacro def-acode-rewrite (name operator-list typecons arglist &body body)
    (if (atom operator-list)
      (setq operator-list (list operator-list)))
    (multiple-value-bind (lambda-list whole)
        (normalize-lambda-list arglist t)
      (multiple-value-bind (body decls)
          (parse-body body nil t)
        (collect ((let-body))
          (dolist (operator operator-list)
            (let-body `(setf (svref *acode-rewrite-functions* (logand operator-id-mask (%nx1-operator ,operator))) fun)))
          (let* ((whole-var (gensym "WHOLE")))
            (multiple-value-bind (bindings binding-decls)
                (%destructure-lambda-list lambda-list whole-var nil nil
                                          :cdr-p t
                                          :whole-p nil
                                          :use-whole-var t
                                          :default-initial-value nil)
              (when whole
                (setq bindings (nconc bindings (list `(,whole ,whole-var)))))
              
        `(let* ((fun (nfunction ,name 
                                (lambda (,typecons ,whole-var)
                                  (declare (ignorable ,typecons))
                                  (block ,name
                                    (let* ,(nreverse bindings)
                                      ,@(when binding-decls `((declare ,@binding-decls)))
                                      ,@decls
                                      ,@body))))))
          ,@(let-body)))))))))

;;; Don't walk the form (that's already happened.)
(defun acode-post-form-type (form)
  (when (acode-p form)
    (let* ((op (acode-operator form))
           (operands (cdr form)))
      (cond ((and *acode-rewrite-trust-declarations*
                  (eq op (%nx1-operator typed-form)))
             (acode-operand 0 operands))
            ((eq op (%nx1-operator fixnum))
             'fixnum)
            ((eq op (%nx1-operator immediate))
             (type-of (acode-operand 0 operands)))
            (t t)))))

(defun acode-constant-p (form)
  ;; This returns (values constant-value constantp); some code
  ;; may need to check constantp if constant-value is nil.
  (let* ((form (acode-unwrapped-form-value form))
         (op (if (acode-p form) (acode-operator form))))
    (cond ((eql op (%nx1-operator nil))
           (values nil t))
          ((eql op (%nx1-operator t))
           (values t t))
          ((or (eql op (%nx1-operator fixnum))
               (eql op (%nx1-operator immediate)))
           (values (cadr form) t))
          (t (values nil nil)))))


(defun acode-post-form-typep (form type)
  (let* ((ctype (specifier-type type))
         (form (acode-unwrapped-form-value form)))
    (cond ((nx-null form) (ctypep nil ctype))
          ((nx-t form) (ctypep t ctype))
          ((not (acode-p form)) (values nil nil))
          (t
           (let* ((op (acode-operator form))
                  (operands (cdr form)))
             (cond ((and *acode-rewrite-trust-declarations*
                         (eq op (%nx1-operator typed-form)))
                    (subtypep (acode-operand 0 operands) type))
                   ((or (eq op (%nx1-operator fixnum))
                        (eq op (%nx1-operator immediate)))
                    (ctypep (acode-operand 0 operands) (specifier-type type)))
                   (t (values nil nil))))))))

(defun rewrite-acode-form (form type)
  (when (acode-p form)
    (let* ((op (acode-operator form))
           (rest (acode-operands form))
           (rewrite (svref *acode-rewrite-functions* (logand op operator-id-mask))))
      (when rewrite
        (let* ((new (cons op rest))
               (type-cons (list type new)))
          (setf (car form) (%nx1-operator type-asserted-form)
                (cdr form) type-cons)
          (funcall rewrite type-cons new))))))
      
    

(defun acode-constant-fold-numeric-binop (type-cons whole form1 form2 function)
  (rewrite-acode-form form1 t)
  (rewrite-acode-form form2 t)
  (let* ((v1 (acode-xxx-form-p form1 'number))
         (v2 (acode-xxx-form-p form2 'number))
         (val (and v1 v2 (ignore-errors (funcall function v1 v2)))))
    (when val
      (setf (car whole) (if (typep val *nx-target-fixnum-type*)
                          (%nx1-operator fixnum)
                          (%nx1-operator immediate))
            (cadr whole) val
            (cddr whole) nil
            (car type-cons) (if (typep val 'integer)
                             `(integer ,val ,val)
                             (type-of val)))
      val)))

(defun acode-rewrite-decls (decls)
  (if (fixnump decls)
    (locally (declare (fixnum decls))
      (setq *acode-rewrite-tail-allow* (neq 0 (%ilogand2 $decl_tailcalls decls))
            *acode-rewrite-open-code-inline* (neq 0 (%ilogand2 $decl_opencodeinline decls))
            *acode-rewrite-full-safety* (neq 0 (%ilogand2 $decl_full_safety decls))
            *acode-rewrite-reckless* (neq 0 (%ilogand2 $decl_unsafe decls))
            *acode-rewrite-trust-declarations* (neq 0 (%ilogand2 $decl_trustdecls decls))))))

(defmacro with-acode-declarations (declsform &body body)
  `(let* ((*acode-rewrite-tail-allow* *acode-rewrite-tail-allow*)
          (*acode-rewrite-reckless* *acode-rewrite-reckless*)
          (*acode-rewrite-open-code-inline* *acode-rewrite-open-code-inline*)
          (*acode-rewrite-trust-declarations* *acode-rewrite-trust-declarations*)
          (*acode-rewrite-full-safety* *acode-rewrite-full-safety*))
     (acode-rewrite-decls ,declsform)
     ,@body))

(defun acode-maybe-punt-var (var initform)
  (let* ((bits (nx-var-bits var)))
    (declare (fixnum bits))
    (cond ((and (logbitp $vbitpuntable var)
                (not (logbitp $vbitpunted var)))
           (nx-set-var-bits var (logior (ash 1 $vbitpunted) bits))
           (rewrite-acode-form initform (or (var-inittype var) t))
           (nx2-replace-var-refs var initform)
           (setf (var-ea var) initform))
          (t
           (rewrite-acode-form initform t)))))
           
(defun acode-type-merge (type-cons derived)
  (let* ((asserted (car type-cons))
         (intersection (ignore-errors (type-specifier (specifier-type `(and ,asserted ,derived))))))
    (when intersection
      (setf (car type-cons) intersection))))

         
    
  

(def-acode-rewrite acode-rewrite-lambda lambda-list type-cons (req opt rest keys auxen body p2-decls &optional code-note)
  (declare (ignore code-note req rest))
  (with-acode-declarations p2-decls
    (dolist (optinit (cadr opt))
      (rewrite-acode-form optinit t))
    (dolist (keyinit (nth 3 keys))
      (rewrite-acode-form keyinit t))
    (do* ((auxvars (car auxen) (cdr auxvars))
          (auxvals (cadr auxen) (cdr auxvals)))
         ((null auxvars))
      (acode-maybe-punt-var (car auxvars) (car auxvals)))
    (rewrite-acode-form body (car type-cons))
    (acode-type-merge type-cons (acode-form-type body *acode-rewrite-trust-declarations*))))

(def-acode-rewrite acode-rewrite-progn progn type-cons (&rest forms)
  (do* ((form (pop forms) (pop forms)))
       ((null forms))
    (if forms
      (rewrite-acode-form form t)
      (progn
        (rewrite-acode-form form (car type-cons))
        (acode-type-merge type-cons (acode-form-type form *acode-rewrite-trust-declarations*))))))

(def-acode-rewrite acode-rewrite-prog1 prog1 type-cons (first &rest others)
  (rewrite-acode-form first (car type-cons))
  (dolist (other others) (rewrite-acode-form other t))
  (acode-type-merge type-cons (acode-form-type first *acode-rewrite-trust-declarations*)))

(def-acode-rewrite acode-rewrite-%slot-ref %slot-ref type-cons (instance idx)
  (rewrite-acode-form instance t)
  (rewrite-acode-form idx t))

(def-acode-rewrite acode-rewrite-svref (%svref svref) type-cons (&whole w vector idx)
  (rewrite-acode-form vector t)
  (rewrite-acode-form idx t)
  (let* ((cv (acode-constant-p vector)))
    (when (if (eql (car w) (%nx1-operator svref))
            (typep cv 'simple-vector)
            (gvectorp cv))
      (let* ((cidx (acode-fixnum-form-p idx)))
        (when (and (typep cidx 'fixnum)
                   (>= (the fixnum cidx) 0)
                   (< (the fixnum cidx) (the fixnum (uvsize cv))))
          (let* ((val (%svref cv cidx)))
            (setf (car w) (if (nx1-target-fixnump val)
                            (%nx1-operator fixnum)
                            (%nx1-operator immediate))
                  (cadr w) val
                  (cddr w) nil)
            (acode-type-merge type-cons (type-of val))))))))

(def-acode-rewrite acode-rewrite-%sbchar %sbchar type-cons (&whole w string idx)
  (rewrite-acode-form string t)
  (rewrite-acode-form idx t)
  (let* ((cv (acode-constant-p string)))
    (when (typep cv 'simple-string)
      (let* ((cidx (acode-fixnum-form-p idx)))
        (when (and (typep cidx 'fixnum)
                   (>= (the fixnum cidx) 0)
                   (< (the fixnum cidx) (the fixnum (length cv))))
          (let* ((val (%schar cv cidx)))
            (setf (car w) (%nx1-operator immediate)
                  (cadr w) val
                  (cddr w) nil)
            (acode-type-merge type-cons 'character)))))))

(def-acode-rewrite acode-rewrite-svset (%svset svset) type-cons (vector idx value)
  (rewrite-acode-form vector t)
  (rewrite-acode-form idx t)
  (rewrite-acode-form value (car type-cons))
  (acode-type-merge type-cons (acode-form-type value *acode-rewrite-trust-declarations*)))

(def-acode-rewrite acode-rewrite-consp consp type-cons (&whole w cc thing)
  (rewrite-acode-form thing t)
  (multiple-value-bind (cthing constantp) (acode-constant-p thing)
    (if constantp
      (let* ((consp (consp cthing))
             (ccode (cadr cc))
             (val (if (eq ccode :eq) (not (not consp)) (not consp))))
        (setf (car w) (if val (%nx1-operator t) (%nx1-operator nil))
              (cdr w) nil)))))

(def-acode-rewrite acode-rewrite-cons cons type-cons (x y)
  (rewrite-acode-form x t)
  (rewrite-acode-form y t)
  (acode-type-merge type-cons 'cons))

(def-acode-rewrite acode-rewrite-rplacx (%rplaca %rplacd rplaca rplacd) type-cons (cell val)
  (rewrite-acode-form cell t)
  (rewrite-acode-form val t)
  (acode-type-merge type-cons 'cons))

(def-acode-rewrite acode-rewrite-set-cxr (set-car set-cdr) type-cons (cell val)
  (rewrite-acode-form cell t)
  (rewrite-acode-form val t)
  (acode-type-merge type-cons (acode-form-type val *acode-rewrite-trust-declarations*)))

(def-acode-rewrite acode-rewrite-cxr (%car %cdr car cdr) type-cons (cell)
  (rewrite-acode-form cell t))

(def-acode-rewrite acode-rewrite-vector vector type-cons (arglist)
  (dolist (f arglist) (rewrite-acode-form f t))
  (acode-type-merge type-cons 'simple-vector))

                   
        
        
