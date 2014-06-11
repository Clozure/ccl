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
  (defmacro def-acode-rewrite (name operator-list type-name arglist &body body)
    (when (atom operator-list)
      (setq operator-list (list operator-list)))
    (multiple-value-bind (lambda-list whole-var) (normalize-lambda-list arglist t)
      (unless whole-var (setq whole-var (gensym)))
      (multiple-value-bind (body decls)
          (parse-body body nil t)
        (collect ((let-body))
          (dolist (operator operator-list)
            (let-body `(setf (svref *acode-rewrite-functions* (logand operator-id-mask (%nx1-operator ,operator))) fun)))
          (let* ((operands (gensym "OPERANDS")))
            (multiple-value-bind (bindings binding-decls)
                (%destructure-lambda-list lambda-list operands nil nil
                                          :cdr-p nil
                                          :whole-p nil
                                          :use-whole-var t
                                          :default-initial-value nil)
              
              `(let* ((fun (nfunction ,name 
                                      (lambda (,whole-var &optional (,type-name t))
                                        (declare (ignorable ,type-name))
                                        (block ,name
                                          (let* ((,operands (acode-operands ,whole-var))
                                                 ,@(nreverse bindings))
                                            ,@(when binding-decls `((declare ,@binding-decls)))
                                            ,@decls
                                            ,@body))))))
          ,@(let-body)))))))))



(defun rewrite-acode-form (form &optional (type t))
  (when (acode-p form)
    (let* ((op (acode-operator form))
           (rewrite (svref *acode-rewrite-functions* (logand op operator-id-mask))))
      (if rewrite
        (funcall rewrite form type)
        (if (logbitp operator-acode-subforms-bit op)
          (dolist (operand (acode-operands form))
            (rewrite-acode-form operand))
          (format t "~&can't rewrite ~s : ~s" (acode-operator-name op) form))))))

(defun acode-wrap-in-unary-op (form op)
  (let* ((new (make-acode* (acode-operator form) (acode-operands form))))
    (setf (acode-operator form) op
          (acode-operands form) (list new)
          (acode.asserted-type form) nil)
    form))

    
(defun acode-rewrite-as-constant-ref (form constant)
  (case constant
    (nil (setf (acode-operator form) (%nx1-operator nil)
               (acode-operands form) nil))
    ((t) (setf (acode-operator form) (%nx1-operator t)
               (acode-operands form) nil))
    (t
     (setf (acode-operator form) (if (nx1-target-fixnump constant)
                     (%nx1-operator fixnum)
                     (%nx1-operator immediate))
           (car (acode-operands form)) constant
           (cdr (acode-operands form)) nil)) )
  (setf (acode.asserted-type form) nil)
  t)
  
(defun acode-constant-fold-numeric-binop (whole form1 form2 function)
  (rewrite-acode-form form1)
  (rewrite-acode-form form2)
  (let* ((v1 (acode-xxx-form-p form1 'number))
         (v2 (acode-xxx-form-p form2 'number))
         (val (and v1 v2 (ignore-errors (funcall function v1 v2)))))
    (when val
      (acode-rewrite-as-constant-ref whole val))))

(defun acode-strength-reduce-binop (whole form1 form2 type high low)
  (declare (fixnum high low))
  (when (and (eql (acode-operator whole) high)
             (acode-form-typep form1 type *acode-rewrite-trust-declarations*)
             (acode-form-typep form2 type *acode-rewrite-trust-declarations*))
    (setf (acode.asserted-type whole) nil
          (acode-operator whole) low)))

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
    (cond ((and (logbitp $vbitpuntable bits)
                (not (logbitp $vbitpunted bits)))
           (nx-set-var-bits var (logior (ash 1 $vbitpunted) bits))
           (rewrite-acode-form initform (or (var-declared-type var) t))
           (nx2-replace-var-refs var initform)
           (setf (var-ea var) initform))
          (t
           (rewrite-acode-form initform)))))

(def-acode-rewrite acode-rewrite-not not asserted-type (&whole w cc form)
  (rewrite-acode-form form)
  (multiple-value-bind (val constantp) (acode-constant-p form)
    (when constantp
      (let* ((condition (car (acode-operands cc))))
        (setf (acode-operator w)
              (if (or (and (eq condition :eq) (null val))
                      (and (eq condition :ne) (not (null val))))
                (%nx1-operator t)
                (%nx1-operator nil))
              (acode.asserted-type w) nil
              (acode-operands w) nil)))))

(defun acode-rewrite-binop-for-numeric-contagion (form1 form2 trust-decls)
  (rewrite-acode-form form1)
  (rewrite-acode-form form2)
  (cond ((acode-form-typep form1 'double-float trust-decls)
         (unless (acode-form-typep form2 'double-float trust-decls)
           (let* ((c2 (acode-real-constant-p form2)))
             (if c2
               (setf (acode-operator form2) (%nx1-operator immediate)
                     (acode-operands form2) (cons (float c2 0.0d0) nil))
               (if (acode-form-typep form2 'fixnum trust-decls)
                 (acode-wrap-in-unary-op form2 (%nx1-operator %fixnum-to-double)))))))
        ((acode-form-typep form2 'double-float trust-decls)
         (let* ((c1 (acode-real-constant-p form1)))
           (if c1
               (setf (acode-operator form1) (%nx1-operator immediate)
                     (acode-operands form1) (cons (float c1 0.0d0) nil))
             (if (acode-form-typep form1 'fixnum trust-decls)
               (acode-wrap-in-unary-op form1 (%nx1-operator %fixnum-to-double))))))
        ((acode-form-typep form1 'single-float trust-decls)
         (unless (acode-form-typep form2 'single-float trust-decls)
           (let* ((c2 (acode-real-constant-p form2)))
             (if c2
               (setf (acode-operator form2) (%nx1-operator immediate)
                     (acode-operands form2) (cons (float c2 0.0f0) nil))
               (if (acode-form-typep form2 'fixnum trust-decls)
                 (acode-wrap-in-unary-op form2 (%nx1-operator %fixnum-to-single)))))))
        ((acode-form-typep form2 'single-float trust-decls)
         (let* ((c1 (acode-real-constant-p form1)))
             (if c1
               (setf (acode-operator form1) (%nx1-operator immediate)
                     (acode-operands form1) (cons (float c1 0.0f0) nil))
               (if (acode-form-typep form1 'fixnum trust-decls)
                 (acode-wrap-in-unary-op form1 (%nx1-operator %fixnum-to-single))))))))
  
(def-acode-rewrite acode-rewrite-add2 add2 asserted-type (&whole w form1 form2)
  (let* ((trust-decls *acode-rewrite-trust-declarations*))
    (acode-rewrite-binop-for-numeric-contagion form1 form2 trust-decls)
    (or (acode-constant-fold-numeric-binop w form1 form2 '+)
        (let* ((target-fixnum-type *nx-target-fixnum-type*)               
               (target-natural-type *nx-target-natural-type*)
               (t1 (acode-form-type form1 trust-decls))
               (t2 (acode-form-type form2 trust-decls))
               (newtype nil)
               (newop (cond ((and (subtypep t1 'double-float)
                                  (subtypep t2 'double-float))
                             (setq newtype 'double-float)
                             (%nx1-operator %double-float+-2))
                            ((and (subtypep t1 'single-float)
                                  (subtypep t2 'single-float))
                             (setq newtype 'single-float)
                             (%nx1-operator %short-float+-2))

                            ((and (subtypep t1 target-fixnum-type)
                                  (subtypep t2 target-fixnum-type))
                             (setq newtype (ctype-specifier (bounded-integer-type-for-addition t1 t2)))
                             (if (or
                                  (subtypep newtype target-fixnum-type)
                                  (and trust-decls
                                      (subtypep asserted-type target-fixnum-type)))
                               (%nx1-operator fixnum-add-no-overflow)
                               (%nx1-operator fixnum-add-overflow)))
                            ((and (subtypep t1 target-natural-type)
                                  (subtypep t2 target-natural-type)
                                  (or (subtypep (setq newtype (ctype-specifier (bounded-integer-type-for-addition t1 t2))) target-natural-type)
                                      (and trust-decls
                                           (subtypep asserted-type target-natural-type))))
                             (%nx1-operator %natural+)))))
          (when newop
            (setf (acode.asserted-type w) newtype
                  (acode-operator w) newop))))))

(def-acode-rewrite acode-rewrite-sub2 sub2 asserted-type (&whole w form1 form2)
  (let* ((trust-decls *acode-rewrite-trust-declarations*))
    (acode-rewrite-binop-for-numeric-contagion form1 form2 trust-decls)
    (or (acode-constant-fold-numeric-binop w form1 form2 '-)
        (let* ((target-fixnum-type *nx-target-fixnum-type*)
               (target-natural-type *nx-target-natural-type*)
               (newtype nil)
               (t1 (acode-form-type form1 trust-decls))
               (t2 (acode-form-type form2 trust-decls))
               (newop (cond ((and (subtypep t1 'double-float)
                                  (subtypep t2 'double-float))
                             (setq newtype 'double-float)
                             (%nx1-operator %double-float--2))
                            ((and (subtypep t1 'single-float)
                                  (subtypep t2 'single-float))
                             (setq newtype 'single-float)
                             (%nx1-operator %short-float--2))

                            ((and (subtypep t1 target-fixnum-type)
                                  (subtypep t2 target-fixnum-type))
                             (if (or (subtypep (setq newtype
                                                     (ctype-specifier
                                                      (bounded-integer-type-for-subtraction t1 t2)))
                                               target-fixnum-type)
                                     (and trust-decls
                                          (subtypep asserted-type target-fixnum-type)))
                               (%nx1-operator fixnum-sub-no-overflow)
                               (%nx1-operator fixnum-sub-overflow)))
                            ((and (subtypep t1 target-natural-type)
                                  (subtypep t2 target-natural-type)
                                  (or (subtypep (setq newtype
                                                      (ctype-specifier
                                                       (bounded-integer-type-for-subtraction t1 t2)))
                                                target-natural-type)
                                      (and trust-decls
                                           (subtypep asserted-type target-natural-type))))
                             (%nx1-operator %natural-)))))
          (when newop
            (setf (acode.asserted-type w) newtype
                  (acode-operator w) newop))))))


(def-acode-rewrite acode-rewrite-mul2 mul2 asserted-type (&whole w form1 form2)
  (let* ((trust-decls *acode-rewrite-trust-declarations*))
    (acode-rewrite-binop-for-numeric-contagion form1 form2 trust-decls)
    (or (acode-constant-fold-numeric-binop w form1 form2 '*)
        (let* ((t1 (acode-form-type form1 trust-decls))
               (t2 (acode-form-type form2 trust-decls))
               (c1 (acode-fixnum-form-p form1))
               (c2 (acode-fixnum-form-p form2))
               (shift-count nil))
          (cond ((and c1 (> c1 0) (< c1 (ash 1 24)) (eql (logcount c1) 1)
                      (acode-form-typep form2 'integer trust-decls))
                 (setq shift-count (1- (integer-length c1)))
                 (setf (acode-operator w) (%nx1-operator ash)
                       (car (acode-operands w)) form2
                       (cadr (acode-operands w)) (make-acode (%nx1-operator fixnum) shift-count)
                       (acode.asserted-type w) nil)
                 (rewrite-acode-form w))
                ((and c2 (> c2 0) (< c1 (ash 1 24)) (eql (logcount c2) 1)
                      (acode-form-typep form1 'integer trust-decls))
                 (setq shift-count (1- (integer-length c1)))
                 (setf (acode-operator w) (%nx1-operator ash)
                       (cadr (acode-operands w)) (make-acode (%nx1-operator fixnum) shift-count)
                       (acode.asserted-type w) nil)
                 (rewrite-acode-form w))
                (t
                 (let* ((newtype nil)
                        (newop (cond ((and (subtypep t1 'double-float)
                                           (subtypep t2 'double-float))
                                      (setq newtype 'double-float)
                                      (%nx1-operator %double-float*-2))
                                     ((and (subtypep t1 'single-float)
                                           (subtypep t2 'single-float))
                                      (setq newtype 'single-float)
                                      (%nx1-operator %short-float*-2))
                                     ((let* ((multype (bounded-integer-type-for-multiplication t1 t2))
                                             (target-fixnum-type *nx-target-fixnum-type*))
                                        (and multype (subtypep (setq newtype (ctype-specifier multype))
                                                               target-fixnum-type)
                                             (subtypep t1 target-fixnum-type)
                                             (subtypep t2 target-fixnum-type)))
                                      (%nx1-operator %i*)))))
                   (when newop
                     (setf (acode.asserted-type w) newtype
                           (acode-operator w) newop)))))))))


(def-acode-rewrite acode-rewrite-div2 div2 asserted-type (&whole w form1 form2)
  (let* ((trust-decls *acode-rewrite-trust-declarations*))
    (acode-rewrite-binop-for-numeric-contagion form1 form2 trust-decls)
    (or (acode-constant-fold-numeric-binop w form1 form2 '/)
        (acode-strength-reduce-binop w form1 form2 'double-float (%nx1-operator div2) (%nx1-operator %double-float/-2))
        (acode-strength-reduce-binop w form1 form2 'single-float (%nx1-operator div2) (%nx1-operator %short-float/-2))
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
                (progn
                  (break "here")
                  (setf (acode-operator w) (%nx1-operator mul2)
                        (acode-operands w) (list (make-acode (%nx1-operator fixnum) f1/f2)
                                                 (cadr (acode-operands unwrapped)))
                        (acode.asserted-type w) nil)
                  (rewrite-acode-form w)
                  t))))))

(def-acode-rewrite acode-rewrite-minus1 minus1 asserted-type (&whole w form)
  (rewrite-acode-form form)
  (let* ((trust-decls *acode-rewrite-trust-declarations*)
         (type (acode-form-type form trust-decls)))
    (multiple-value-bind (val constp) (acode-constant-p form)
      (cond ((and constp (ignore-errors (setq val (- val))))
             (acode-rewrite-as-constant-ref w val))
            ((subtypep type 'double-float)
             (setf (acode-operator w) (%nx1-operator %double-float-negate)
                   (acode.asserted-type w) 'double-float))
            ((subtypep type 'single-float)
             (setf (acode-operator w) (%nx1-operator %single-float-negate)
                   (acode.asserted-type w) 'single-float))
            (t (let* ((target-fixnum-type *nx-target-fixnum-type*))
                 (when (subtypep type target-fixnum-type)
                   (let* ((result-type (bounded-integer-type-for-subtraction '(integer 0 0) type)))
                     (setf (acode-operator w)
                           (if (or
                                (and result-type
                                     (subtypep (type-specifier result-type)
                                               target-fixnum-type))
                                (subtypep (or asserted-type '*)
                                          target-fixnum-type)) 
                             (%nx1-operator %%ineg)
                             (%nx1-operator %ineg))
                           (acode.asserted-type w)
                           (type-specifier result-type))))))))))

(def-acode-rewrite acode-rewrite-realpart realpart asserted-type (&whole w arg) 
  (let* ((trust-decls *acode-rewrite-trust-declarations*))
    (rewrite-acode-form arg)
    (cond ((acode-form-typep arg '(complex single-float) trust-decls)
           (setf (acode-operator w) (%nx1-operator %complex-single-float-realpart)
                 (acode.asserted-type w) 'single-float))
          ((acode-form-typep arg '(complex double-float) trust-decls)
           (setf (acode-operator w) (%nx1-operator %complex-double-float-realpart)
                 (acode.asserted-type w) 'double-float)))))

(def-acode-rewrite acode-rewrite-imagpart imagpart asserted-type (&whole w arg) 
  (let* ((trust-decls *acode-rewrite-trust-declarations*))
    (rewrite-acode-form arg)
    (cond ((acode-form-typep arg '(complex single-float) trust-decls)
           (setf (acode-operator w) (%nx1-operator %complex-single-float-imagpart)
                 (acode.asserted-type w) 'single-float))
          ((acode-form-typep arg '(complex double-float) trust-decls)
           (setf (acode-operator w) (%nx1-operator %complex-double-float-imagpart)
                 (acode.asserted-type w) 'double-float)))))

(def-acode-rewrite acode-rewrite-complex complex asserted-type (&whole w r i)
  (let* ((trust-decls *acode-rewrite-trust-declarations*))
    (rewrite-acode-form r)
    (rewrite-acode-form i)
    (cond ((and (acode-form-typep r 'single-float trust-decls)
                (acode-form-typep i 'single-float trust-decls))
           (setf (acode-operator w) (%nx1-operator %make-complex-single-float)
                 (acode.asserted-type w) '(complex single-float)))
          ((and (acode-form-typep r 'double-float trust-decls)
                (acode-form-typep i 'double-float trust-decls))
           (setf (acode-operator w) (%nx1-operator %make-complex-double-float)
                 (acode.asserted-type w) '(complex double-float))))))

          

         
    
  

(def-acode-rewrite acode-rewrite-lambda lambda-list asserted-type  (&whole whole req opt rest keys auxen body p2-decls &optional code-note)
  (declare (ignore code-note req rest))
  (with-acode-declarations p2-decls
    (dolist (optinit (cadr opt))
      (rewrite-acode-form optinit))
    (dolist (keyinit (nth 3 keys))
      (rewrite-acode-form keyinit))
    (do* ((auxvars (car auxen) (cdr auxvars))
          (auxvals (cadr auxen) (cdr auxvals)))
         ((null auxvars))
      (acode-maybe-punt-var (car auxvars) (car auxvals)))
    (rewrite-acode-form body)
    ))

(def-acode-rewrite acode-rewrite-let (let* let) asserted-type (&whole w vars vals body p2decls)
  (collect ((new-vars)
            (new-vals))
    (dolist (var vars)
      (let* ((val (pop vals))
             (bits (nx-var-bits var)))
        (declare (fixnum bits))
        (unless (and (logbitp $vbitpunted bits)
                     (not (logbitp $vbitspecial bits)))
          (cond ((logbitp $vbitpuntable bits)
                 (nx-set-var-bits var (logior (ash 1 $vbitpunted) bits))
                 (rewrite-acode-form val)
                 (nx2-replace-var-refs var val)
                 (setf (var-ea var) val))
                (t
                 (rewrite-acode-form val)
                 (new-vars var)
                 (new-vals val))))))
    (setf (car (acode-operands w)) (new-vars)
          (cadr (acode-operands w)) (new-vals))
    (with-acode-declarations p2decls (rewrite-acode-form body asserted-type))))
    
      
(def-acode-rewrite acode-rewrite-progn progn asserted-type (forms)
  (do* ()
       ((null forms))
    (let* ((form (pop forms)))
      (if forms
        (rewrite-acode-form form)
        (rewrite-acode-form form asserted-type)))))

(def-acode-rewrite acode-rewrite-prog1 (prog1 multiple-value-prog1) asserted-type  (&whole w (first &rest others))
  (rewrite-acode-form first asserted-type)
  (dolist (other others) (rewrite-acode-form other)))


(def-acode-rewrite acode-rewrite-svref svref asserted-type (&whole w vector idx)
  (rewrite-acode-form vector)
  (rewrite-acode-form idx )
  (let* ((cv (acode-constant-p vector)))
    (when (and (typep cv 'simple-vector)
               (eql (acode-operator w) (%nx1-operator svref)))
      (let* ((cidx (acode-fixnum-form-p idx)))
        (when (and (typep cidx 'fixnum)
                   (>= (the fixnum cidx) 0)
                   (< (the fixnum cidx) (the fixnum (uvsize cv))))
          (let* ((val (%svref cv cidx)))
            (case val
              (nil (setf (acode-operator w) (%nx1-operator nil)
                         (acode-operands w) nil))
              ((t) (setf (acode-operator w) (%nx1-operator t)
                         (acode-operands w) nil))
              (t
               (setf (acode-operator w) (if (nx1-target-fixnump val)
                                          (%nx1-operator fixnum)
                                          (%nx1-operator immediate))
                     (acode-operands w) (cons val nil))))
            (setf (acode.asserted-type w) nil)
            t))))))

(def-acode-rewrite acode-rewrite-%svref %svref asserted-type (vector i)
  (rewrite-acode-form vector)
  (rewrite-acode-form i))


(def-acode-rewrite acode-rewrite-%sbchar %sbchar  asserted-type (&whole w string idx)
  (rewrite-acode-form string)
  (rewrite-acode-form idx)
  (let* ((cv (acode-constant-p string)))
    (when (typep cv 'simple-string)
      (let* ((cidx (acode-fixnum-form-p idx)))
        (when (and (typep cidx 'fixnum)
                   (>= (the fixnum cidx) 0)
                   (< (the fixnum cidx) (the fixnum (length cv))))
          (let* ((val (%schar cv cidx)))
            (setf (acode-operator w) (%nx1-operator immediate)
                  (car (acode-operands w)) val
                  (cdr (acode-operands w)) nil
                  (acode.asserted-type w) nil)
            t))))))


(def-acode-rewrite acode-rewrite-consp consp asserted-type (&whole w cc thing)
  (rewrite-acode-form thing)
  (multiple-value-bind (cthing constantp) (acode-constant-p thing)
    (if constantp
      (let* ((consp (consp cthing))
             (ccode (car (acode-operands cc)))
             (val (if (eq ccode :eq) (not (not consp)) (not consp))))
        (setf (acode-operator w) (if val (%nx1-operator t) (%nx1-operator nil))
              (acode-operands w) nil
              (acode.asserted-type w) nil)))))


(def-acode-rewrite acode-rewrite-cxr (%car %cdr car cdr) asserted-type (&whole w cell)
  (rewrite-acode-form cell)
  (multiple-value-bind (val constantp) (acode-constant-p cell)
    (when (and constantp (typep val 'list))
      (let* ((op (acode-operator w)))
        (acode-rewrite-as-constant-ref w (if (or (eql op (%nx1-operator car))
                                                 (eql op (%nx1-operator %car)))
                                           (car val)
                                           (cdr val)))))))



                   
(def-acode-rewrite acode-rewrite-%gvector %gvector asserted-type  (&whole w arglist)
  (let* ((all-args (append (car arglist) (reverse (cadr arglist)))))
    (dolist (arg all-args)
      (rewrite-acode-form arg))
    ;; Could try to map constant subtag to type here
    ))

(def-acode-rewrite acode-rewrite-char-code (%char-code char-code) asserted-type  (&whole w c)
  (rewrite-acode-form c)
  (let* ((char (acode-constant-p c)))
    (when (typep char 'character)
      (let* ((code (char-code char)))
        (setf (acode-operator w) (%nx1-operator fixnum)
              (acode-operands w) (list code)
              (acode.asserted-type w) nil)))))

(def-acode-rewrite acode-rewrite-logior (logior2 %ilogior2 %natural-logior) asserted-type  (&whole w x y) 
  (or (acode-constant-fold-numeric-binop  w x y 'logior)
      (acode-strength-reduce-binop w x y *nx-target-fixnum-type* (%nx1-operator logior2) (%nx1-operator %ilogior2))
      (acode-strength-reduce-binop w x y *nx-target-natural-type* (%nx1-operator logior2) (%nx1-operator %natural-logior)))
)

(def-acode-rewrite acode-rewrite-logand (logand2 %ilogand2 %natural-logand) asserted-type  (&whole w x y) 
  (or (acode-constant-fold-numeric-binop  w x y 'logand)
      (acode-strength-reduce-binop w x y *nx-target-fixnum-type* (%nx1-operator logand2) (%nx1-operator %ilogand2))
      (acode-strength-reduce-binop w x y *nx-target-natural-type* (%nx1-operator logand2) (%nx1-operator %natural-logand))
      (cond ((eql -1 (acode-fixnum-form-p x))
             (setf (acode-operator w) (%nx1-operator require-integer)
                   (acode-operands w) (list y)
                   (acode.asserted-type w) nil)
             t)
            ((eql -1 (acode-fixnum-form-p y))
             (setf (acode-operator w) (%nx1-operator require-integer)
                   (acode-operands w) (list x)
                   (acode.asserted-type w) nil)
             t))))

(def-acode-rewrite acode-rewrite-logxor (logxor2 %ilogxor2 %natural-logxor) asserted-type  (&whole w x y) 
  (or (acode-constant-fold-numeric-binop  w x y 'logxor)
      (acode-strength-reduce-binop w x y *nx-target-fixnum-type* (%nx1-operator logxor2) (%nx1-operator %ilogxor2))
      (acode-strength-reduce-binop w x y *nx-target-natural-type* (%nx1-operator logxor2) (%nx1-operator %natural-logxor))))
                                   

    
(def-acode-rewrite acode-rewrite-%ineg %ineg asserted-type (&whole w x)
  (rewrite-acode-form x)
  (let* ((val (acode-fixnum-form-p x))
         (negated (if val (- val))))
    (if negated
      (setf (acode-operator w) (if (typep negated *nx-target-fixnum-type*)
                                 (%nx1-operator fixnum)
                                 (%nx1-operator immediate))
            (acode-operands w) (list negated)
            (acode.asserted-type w) nil))))

(def-acode-rewrite rewrite-type-asserted-form type-asserted-form asserted-type (&whole w type form &optional check)
  (declare (ignore check))
  (rewrite-acode-form form type))

(def-acode-rewrite rewrite-typed-form typed-form asserted-type (&whole w type form &optional check)
  (rewrite-acode-form form (if (or check *acode-rewrite-trust-declarations*) type t)))

(def-acode-rewrite rewrite-trivial-unary (fixnum immediate simple-function closed-function lexical-reference bound-special-ref special-ref local-go %function global-ref free-reference inherited-arg) asserted-type (&whole w val)
  (declare (ignore val)))


(def-acode-rewrite rewrite-nullary (t nil %unbound-marker %slot-unbound-marker %illegal-marker %current-tcr %foreign-stack-pointer %current-frame-ptr %interrupt-poll) asserted-type (&whole w))

(def-acode-rewrite rewrite-call (call lexical-function-call builtin-call) asserted-type (&whole w callable arglist &optional spread-p)
  (declare (ignore spread-p))
  (when (acode-p callable)
    (rewrite-acode-form callable))
  (dolist (arg (car arglist))
    (rewrite-acode-form arg))
  (dolist (arg (cadr arglist))
    (rewrite-acode-form arg)))


(def-acode-rewrite acode-rewrite-arglist-form (list* %err-disp) asserted-type (&whole w arglist)
  (dolist (arg (car arglist))
    (rewrite-acode-form arg))
  (dolist (arg (cadr arglist))
    (rewrite-acode-form arg)))

(def-acode-rewrite acode-rewrite-self-call self-call asserted-type (arglist &optional spread-p)
  (declare (ignore spread-p))
  (dolist (arg (car arglist))
    (rewrite-acode-form arg))
  (dolist (arg (cadr arglist))
    (rewrite-acode-form arg)))


(def-acode-rewrite acode-rewrite-formlist (list values %temp-list vector) asserted-type (formlist)
  (dolist (form formlist) (rewrite-acode-form form)))

(def-acode-rewrite acode-rewrite-multiple-value-bind multiple-value-bind asserted-type (vars valform body p2decls)
  (declare (ignore vars))
  (rewrite-acode-form valform)
  (with-acode-declarations p2decls (rewrite-acode-form body asserted-type)))


(def-acode-rewrite acode-rewrite-local-tagbody local-tagbody asserted-type (tags forms)
  (declare (ignore tags))
  (dolist (form forms) (rewrite-acode-form form)))

(def-acode-rewrite acode-rewrite-tag-label tag-label asserted-type (&rest ignore)
  (declare (ignore ignore)))

(def-acode-rewrite acode-rewrite-local-block local-block asserted-type (tag body)
  (declare (ignore tag))
  (rewrite-acode-form body asserted-type))


(def-acode-rewrite acode-rewrite-local-return-from local-return-from asserted-type (block value)
  (declare (ignore block))
  (rewrite-acode-form value))

(def-acode-rewrite acode-rewrite-or or asserted-type (forms)
  (dolist (form forms) (rewrite-acode-form form))
  (do* ((forms forms (cdr forms)))
       ((null (cdr forms)))
    (multiple-value-bind (val constantp) (acode-constant-p (car forms))
      (when (and val constantp)
        (setf (cdr forms) nil)))))


(def-acode-rewrite acode-rewrite-labels-flet (labels flet)  asserted-type (vars funcs body p2decls)
  (declare (ignore vars funcs))
  (with-acode-declarations p2decls (rewrite-acode-form body asserted-type)))

(def-acode-rewrite acode-rewrite-%decls-body %decls-body asserted-type (form p2decls)
  (with-acode-declarations p2decls (rewrite-acode-form form asserted-type)))


;;; The backends may try to eliminate the &rest arg if the body is
;;; obviously an APPLY that uses it.  We could do that here.
(def-acode-rewrite acode-rewrite-lambda-bind lambda-bind asserted-type (vals req rest keys-p auxen body p2decls)
  (declare (ignore keys-p rest))
  (dolist (var req)
    (acode-maybe-punt-var var (pop vals)))
  (dolist (val vals)
    (rewrite-acode-form val))
  (do* ((auxvars (car auxen) (cdr auxvars))
        (auxvals (cadr auxen) (cdr auxvals)))
       ((null auxvars))
    (acode-maybe-punt-var (car auxvars) (car auxvals)))
  (with-acode-declarations p2decls (rewrite-acode-form body asserted-type))
)

;;; The frontend may have type-constrained the value.  That should probably
;;; happen here.
(def-acode-rewrite acode-rewrite-setq-lexical setq-lexical asserted-type (var value)
  (rewrite-acode-form value (or (and *acode-rewrite-trust-declarations*
                                     (var-declared-type var))
                                t)))

(def-acode-rewrite acode-rewrite-unwind-protect unwind-protect asserted-type (protected-form cleanup-form)
  (rewrite-acode-form protected-form asserted-type)
  (rewrite-acode-form cleanup-form))

(def-acode-rewrite acode-rewrite-setq-special (global-setq setq-special) asserted-type (sym val)
  (declare (ignore sym))
  (rewrite-acode-form val))

(def-acode-rewrite acode-rewrite-immediate-get-xxx immediate-get-xxx asserted-type (bits ptr offset)
  (declare (ignore bits))
  (rewrite-acode-form ptr)
  (rewrite-acode-form offset))

(def-acode-rewrite with-variable-c-frame with-variable-c-frame asserted-type (size body)
  (rewrite-acode-form size)
  (rewrite-acode-form body asserted-type))

(def-acode-rewrite acode-rewrite-ff-call (ff-call eabi-ff-call poweropen-ff-call i386-ff-call) asserted-type (address argspecs argvals resultspec &optional monitor)
  (declare (ignore argspecs resultspec monitor))
  (rewrite-acode-form address)
  (dolist (val argvals) (rewrite-acode-form val)))

(def-acode-rewrite acode-rewrite-%ilsl %ilsl asserted-type (&whole w count num)
  (acode-constant-fold-numeric-binop  w count num '%ilsl))

(def-acode-rewrite acode-rewrite-if if asserted-type (&whole w test true false)
  (rewrite-acode-form test)
  (rewrite-acode-form true asserted-type)
  (rewrite-acode-form false asserted-type)
  (multiple-value-bind (val constantp) (acode-constant-p test)
    (when constantp
      (let* ((form (if val true false)))
        (setf (acode-operator w) (acode-operator form)
              (acode-operands w) (acode-operands form)
              (acode.asserted-type w) nil)))))


(def-acode-rewrite acode-rewrite-%izerop %izerop asserted-type (&whole w cc form)
  (rewrite-acode-form form)
  (multiple-value-bind (val constantp) (acode-constant-p form)
    (when constantp
      (setf (acode-operator w)
            (if (if (eq (car (acode-operands cc)) :eq) (eql val 0) (not (eql val 0)))
                (%nx1-operator t)
                (%nx1-operator nil))
            (acode-operands w) nil
            (acode.asserted-type w) nil))))

(def-acode-rewrite acode-rewrite-eq eq asserted-type (&whole w cc x y)
  (rewrite-acode-form x)
  (rewrite-acode-form y)
  (multiple-value-bind (xval xconst) (acode-constant-p x)
    (multiple-value-bind (yval yconst) (acode-constant-p y)
      (when (and xconst yconst)
        (setf (acode-operator w)
              (if (if (eq (car (acode-operands cc)) :eq) (eql xval yval) (not (eql xval yval)))
                (%nx1-operator t)
                (%nx1-operator nil))
              (acode-operands w) nil
              (acode.asserted-type w) nil)))))

(def-acode-rewrite acode-rewrite-with-c-frame with-c-frame asserted-type (body)
  (rewrite-acode-form body asserted-type))

(def-acode-rewrite acode-rewrite-ash ash asserted-type (&whole w num amt)
  (or (acode-constant-fold-numeric-binop w num amt 'ash)
      (let* ((maxbits (target-word-size-case
                       (32 29)
                       (64 60)))
             (cnum (acode-constant-p num))
             (camt (acode-constant-p amt))
             (trust-decls *acode-rewrite-trust-declarations*)
             (fixnum-type *nx-target-fixnum-type*)
             (natural-type *nx-target-natural-type*))
        (cond ((eql camt 0) (setf (acode-operator w) (%nx1-operator require-integer)
                                  (cdr (acode-operands w)) nil
                                  (acode.asserted-type w) nil))
              ((and (typep camt fixnum-type)
                    (< camt 0))
               (if (acode-form-typep num fixnum-type trust-decls)
                 (setf (acode-operator w) (%nx1-operator %iasr)
                       (acode-operands w) (list (make-acode (%nx1-operator fixnum)
                                                            (- camt))
                                                num)
                       (acode.asserted-type w) nil)
                 (if (acode-form-typep num natural-type trust-decls)
                   (if (< (- camt) (arch::target-nbits-in-word
                                    (backend-target-arch *target-backend*)))
                     (setf (acode-operator w) (%nx1-operator natural-shift-right)
                           (cadr (acode-operands w)) (make-acode (%nx1-operator fixnum) (- camt))
                           (acode.asserted-type w) nil)

                     (setf (acode-operator w) (%nx1-operator progn)
                           (acode-operands w) (list (list (make-acode (%nx1-operator require-integer) num)
                                                          (make-acode (%nx1-operator fixnum) 0)))
                           (acode.asserted-type w) nil)))))
              ((and (typep camt 'fixnum)
                    (<= 0 camt maxbits)
                    (or (acode-form-typep num `(signed-byte ,(- (1+ maxbits) camt)) trust-decls)
                        (and (acode-form-typep num fixnum-type trust-decls)
                             (subtypep asserted-type fixnum-type))))
               (setf (acode-operator w) (%nx1-operator %ilsl)
                     (acode-operands w) (list amt num)
                     (acode.asserted-type w) nil))
              ((and (typep camt 'fixnum)
                    (< 0 camt (arch::target-nbits-in-word
                               (backend-target-arch *target-backend*)))
                    (acode-form-typep num natural-type trust-decls)
                    (subtypep asserted-type natural-type))
               (setf (acode-operator w) (%nx1-operator natural-shift-left)
                     (acode.asserted-type w) nil))
              ((typep cnum 'fixnum)
               (let* ((field-width (1+ (integer-length cnum)))
                      ;; num fits in a `(signed-byte ,field-width)
                      (max-shift (- (1+ maxbits) field-width)))
                 (if (and (>= max-shift 0)
                          (acode-form-typep amt `(mod ,(1+ max-shift)) trust-decls))
                   (setf (acode-operator w) (%nx1-operator %ilsl)
                         (acode-operands w) (list amt num)
                         (acode.asserted-type w) nil))))
              ((or (and (subtypep asserted-type fixnum-type)
                        (acode-form-typep num fixnum-type trust-decls)
                        (target-word-size-case
                         (32 (acode-form-typep amt '(signed-byte 5) trust-decls))
                         (64 (acode-form-typep amt '(signed-byte 6) trust-decls))))
                   (let* ((numtype (specifier-type (acode-form-type num trust-decls)))
                          (amttype (specifier-type (acode-form-type amt trust-decls)))
                          (fixtype (specifier-type fixnum-type)))
                     (if (and (typep numtype 'numeric-ctype)
                              (csubtypep numtype fixtype)
                              (typep amttype 'numeric-ctype)
                              (csubtypep amttype fixtype))
                       (let* ((highnum (numeric-ctype-high numtype))
                              (lownum (numeric-ctype-low numtype))
                              (widenum (if (> (integer-length highnum)
                                              (integer-length lownum))
                                         highnum
                                         lownum))
                              (maxleft (numeric-ctype-high amttype)))
                         
                         (and (>= (numeric-ctype-low amttype)
                                        (target-word-size-case
                                         (32 -31)
                                         (64 -63)))
                                    (< maxleft
                                       (arch::target-nbits-in-word (backend-target-arch *target-backend*)))
                                    (typep (ignore-errors (ash widenum maxleft))
                                           fixnum-type))))))
               (setf (acode-operator w) (%nx1-operator fixnum-ash)
                     (acode.asserted-type w) nil))))))

(def-acode-rewrite acode-rewrite-multiple-value-call multiple-value-call asserted-type (callable formlist)
  (when (acode-p callable)
    (rewrite-acode-form callable))
  (dolist (form formlist) (rewrite-acode-form form)))

(def-acode-rewrite acode-rewrite-numcmp numcmp asserted-type (&whole w cc num1 num2)
  (let* ((ccval (car (acode-operands cc)))
         (fn (case ccval
               (:lt '<)
               (:le '<=)
               (:eq '=)
               (:ne '/=)
               (:ge '>=)
               (:gt '>))))
    ;;(acode-rewrite-binop-for-numeric-contagion num1 num2 *acode-rewrite-trust-declarations*)
    (multiple-value-bind (v1 c1) (acode-constant-p num1)
      (multiple-value-bind (v2 c2) (acode-constant-p num2)
        (multiple-value-bind (constval error)
            (if (and c1 c2)
              (ignore-errors (funcall fn v1 v2))
              (values nil t))
          (if (not error)
            (acode-rewrite-as-constant-ref w constval)
            (let* ((op (acode-operator w)))
              (or (acode-strength-reduce-binop w num1 num2 *nx-target-fixnum-type* op (%nx1-operator %i<>))
                  (acode-strength-reduce-binop w num1 num2 *nx-target-natural-type* op (%nx1-operator %natural<>))
                  (acode-strength-reduce-binop w num1 num2 'double-float op (%nx1-operator double-float-compare))
                  (acode-strength-reduce-binop w num1 num2 'single-float op (%nx1-operator short-float-compare))
                  ;; Could try contagion here
                  ))))))))