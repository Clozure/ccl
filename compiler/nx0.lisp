;;; -*- Mode: Lisp; Package: CCL -*-
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



(in-package "CCL")

;; :compiler:nx0.lisp - part of the compiler


(defstruct pending-declarations
  vdecls
  fdecls
  mdecls)

; Phony AFUNC "defstruct":
(defun make-afunc (&aux (v (allocate-typed-vector :istruct $afunc-size nil)))
  (setf (%svref v 0) 'afunc)
  (setf (afunc-fn-refcount v) 0)
  (setf (afunc-fn-downward-refcount v) 0)
  (setf (afunc-bits v) 0)
  v)

(defvar *nx-blocks* nil)
(defvar *nx-tags* nil)
(defvar *nx-parent-function* nil)
(defvar *nx-current-function* nil)
(defvar *nx-lexical-environment* nil)
(defvar *nx-symbol-macros* nil)
(defvar *nx-inner-functions* nil)
(defvar *nx-cur-func-name* nil)
(defvar *nx-form-type* t)
;(defvar *nx-proclaimed-inline* nil)
;(defvar *nx-proclaimed-inline* (make-hash-table :size 400 :test #'eq))
(defvar *nx-proclaimed-ignore* nil)
(defvar *nx-parsing-lambda-decls* nil) ; el grosso.
(defparameter *nx-standard-declaration-handlers* nil)
(defparameter *nx-hoist-declarations* t)
(defparameter *nx-loop-nesting-level* 0)
(defvar *nx-break-on-program-errors* t)

(defvar *nx1-vcells* nil)
(defvar *nx1-fcells* nil)

(defvar *nx1-operators* (make-hash-table :size 160 :test #'eq))

                                         

; The compiler can (generally) use temporary vectors for VARs.
(defun nx-cons-var (name &optional (bits 0))
  (%istruct 'var name bits nil nil nil nil))




(defvar *nx-lambdalist* (make-symbol "lambdalist"))
(defvar *nx-nil* (list (make-symbol "nil")))
(defvar *nx-t* (list (make-symbol "t")))

(defparameter *nx-current-compiler-policy* (%default-compiler-policy))

(defvar *nx-next-method-var* nil)
(defvar *nx-call-next-method-function* nil)

(defvar *nx-sfname* nil)
(defvar *nx-operators* ())
(defvar *nx-warnings* nil)

(defvar *nx1-compiler-special-forms* nil "Real special forms")

(defparameter *nx-never-tail-call*
  '(error cerror break warn type-error file-error
    signal-program-error signal-simple-program-error
    #-bccl %get-frame-pointer
    #-bccl break-loop)
  "List of functions which never return multiple values and
   should never be tail-called.")

(defvar *cross-compiling* nil "bootstrapping")



(defparameter *nx-operator-result-types*
  '((#.(%nx1-operator list) . list)
    (#.(%nx1-operator memq) . list)
    (#.(%nx1-operator %temp-list) . list)
    (#.(%nx1-operator assq) . list)
    (#.(%nx1-operator cons) . cons)
    (#.(%nx1-operator rplaca) . cons)
    (#.(%nx1-operator %rplaca) . cons)
    (#.(%nx1-operator rplacd) . cons)
    (#.(%nx1-operator %rplacd) . cons)
    (#.(%nx1-operator %temp-cons) . cons)
    (#.(%nx1-operator %i+) . fixnum)
    (#.(%nx1-operator %i-) . fixnum)
    (#.(%nx1-operator %i*) . fixnum)
    (#.(%nx1-operator %ilsl) . fixnum)
    (#.(%nx1-operator %ilsr) . fixnum)
    (#.(%nx1-operator %iasr) . fixnum)
    (#.(%nx1-operator %ilogior2) . fixnum)
    (#.(%nx1-operator %ilogand2) . fixnum)
    (#.(%nx1-operator %ilogxor2) . fixnum)
    (#.(%nx1-operator %code-char) . character)
    (#.(%nx1-operator schar) . character)
    (#.(%nx1-operator length) . fixnum)
    (#.(%nx1-operator uvsize) . fixnum)
    (#.(%nx1-operator %double-float/-2) . double-float)
    (#.(%nx1-operator %double-float/-2!) . double-float) ; no such operator
    (#.(%nx1-operator %double-float+-2) . double-float)
    (#.(%nx1-operator %double-float+-2!) . double-float)
    (#.(%nx1-operator %double-float--2) . double-float)
    (#.(%nx1-operator %double-float--2!) . double-float)
    (#.(%nx1-operator %double-float*-2) . double-float)
    (#.(%nx1-operator %double-float*-2!) . double-float)
    (#.(%nx1-operator %short-float/-2) . double-float)
    (#.(%nx1-operator %short-float+-2) . double-float)
    (#.(%nx1-operator %short-float--2) . double-float)
    (#.(%nx1-operator %short-float*-2) . double-float)
    (#.(%nx1-operator %double-to-single) . single-float)
    (#.(%nx1-operator %single-to-double) . double-float)
    (#.(%nx1-operator %fixnum-to-single) . single-float)
    (#.(%nx1-operator %fixnum-to-double) . double-float)
    (#.(%nx1-operator char-code) . #.`(integer 0 (,char-code-limit)))
   ))

(defparameter *nx-operator-result-types-by-name*
  '((%ilognot . fixnum)
    (%ilogxor . fixnum)
    (%ilogand . fixnum)
    (%ilogior . fixnum)
    (char-code . #. `(integer 0 (,char-code-limit)))))

(setq *nx-known-declarations*
  '(special inline notinline type ftype function ignore optimize dynamic-extent ignorable
    ignore-if-unused settable unsettable
     notspecial global-function-name debugging-function-name resident))

(defun find-optimize-quantity (name env)
  (let ((pair ()))
    (loop
      (when (listp env) (return))
      (when (setq pair (assq name (lexenv.mdecls env)))
        (return (%cdr pair)))
      (setq env (lexenv.parent-env env)))))
    
(defun debug-optimize-quantity (env)
  (or (find-optimize-quantity 'debug env)
      *nx-debug*))

(defun space-optimize-quantity (env)
  (or (find-optimize-quantity 'space env)
      *nx-space*))

(defun safety-optimize-quantity (env)
  (or (find-optimize-quantity 'safety env)
      *nx-safety*))

(defun speed-optimize-quantity (env)
  (or (find-optimize-quantity 'speed env)
      *nx-speed*))

(defun compilation-speed-optimize-quantity (env)
  (or (find-optimize-quantity 'compilation-speed env)
      *nx-cspeed*))

(defvar *nx-ignore-if-unused* ())
(defvar *nx-new-p2decls* ())
(defvar *nx-inlined-self* t)
(defvar *nx-all-vars* nil)
(defvar *nx-bound-vars* nil)
(defvar *nx-punted-vars* nil)
(defvar *nx-inline-expansions* nil)
(defparameter *nx-compile-time-compiler-macros* nil)
(defvar *nx-global-function-name* nil)
(defvar *nx-can-constant-fold* ())
(defvar *nx-synonyms* ())
(defvar *nx-load-time-eval-token* ())

(define-condition compiler-function-overflow (condition) ())

(defun compiler-function-overflow ()
  (signal 'compiler-function-overflow)
  (error "Function size exceeds compiler limitation."))

(defvar *compiler-macros* (make-hash-table :size 100 :test #'eq))

;;; Just who was responsible for the "FUNCALL" nonsense ?
;;; Whoever it is deserves a slow and painful death ...

(defmacro define-compiler-macro  (name arglist &body body &environment env)
  "Define a compiler-macro for NAME."
  (let* ((block-name name)
         (def-name (validate-function-name name)))
    (unless (eq def-name block-name)
      (setq block-name (cadr block-name)))
    (let ((body (parse-macro-1 block-name arglist body env)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
        (eval-when (:load-toplevel :execute)
          (record-source-file ',name 'compiler-macro))
        (setf (compiler-macro-function ',name)
         (nfunction (compiler-macro-function ,name)  ,body))
        ',name))))

;;; This is silly (as may be the whole idea of actually -using-
;;; compiler-macros).  Compiler-macroexpand-1 will return a second
;;; value of NIL if the value returned by the expansion function is EQ
;;; to the original form.  This differs from the behavior of
;;; macroexpand-1, but users are not encouraged to write macros which
;;; return their &whole args (as the DEFINE-COMPILER-MACRO issue
;;; encourages them to do ...)  Cheer up! Neither of these things have
;;; to exist!
(defun compiler-macroexpand-1 (form &optional env)
  (let ((expander nil)
        (newdef nil))
    (if (and (consp form)
             (symbolp (car form))
             (setq expander (compiler-macro-function (car form) env)))
      (values (setq newdef (funcall *macroexpand-hook* expander form env)) (neq newdef form))
      (values form nil))))

; ... If this exists, it should probably be exported.
(defun compiler-macroexpand (form &optional env)
  (multiple-value-bind (new win) (compiler-macroexpand-1 form env)
    (do* ((won-at-least-once win))
         ((null win) (values new won-at-least-once))
      (multiple-value-setq (new win) (compiler-macroexpand-1 new env)))))




(defun compiler-macro-function (name &optional env)
  "If NAME names a compiler-macro in ENV, return the expansion function, else
   return NIL. Can be set with SETF when ENV is NIL."
  (setq name (validate-function-name name))
  (unless (nx-lexical-finfo name env)
    (or (cdr (assq name *nx-compile-time-compiler-macros*))
        (values (gethash name *compiler-macros*)))))

(defun set-compiler-macro-function (name def)
  (setq name (validate-function-name name))
  (if def
    (setf (gethash name *compiler-macros*) def)
    (remhash name *compiler-macros*))
  def)

(defsetf compiler-macro-function set-compiler-macro-function)

(defparameter *nx-add-xref-entry-hook* nil
  "When non-NIL, assumed to be a function of 3 arguments 
which asserts that the specied relation from the current
function to the indicated name is true.")

;; Cross-referencing
(defun nx-record-xref-info (relation name)
  (when (fboundp '%add-xref-entry)
    (funcall '%add-xref-entry relation *nx-cur-func-name* name)))



(defun nx-apply-env-hook (hook env &rest args)
  (declare (dynamic-extent args))
  (when (fixnump hook) (setq hook (uvref *nx-current-compiler-policy* hook)))
  (if hook
    (if (functionp hook)
      (apply hook env args)
      t)))

(defun nx-self-calls-inlineable (env)
  (nx-apply-env-hook policy.inline-self-calls env))

(defun nx-allow-register-allocation (env)
  (not (nx-apply-env-hook policy.inhibit-register-allocation env)))

(defun nx-trust-declarations (env)
  (unless (eq (safety-optimize-quantity env) 3)
    (nx-apply-env-hook policy.trust-declarations env)))

(defun nx-open-code-in-line (env)
  (nx-apply-env-hook policy.open-code-inline env))

(defun nx-inline-car-cdr (env)
  (unless (eq (safety-optimize-quantity env) 3)
    (nx-apply-env-hook policy.inhibit-safety-checking env)))

(defun nx-inhibit-safety-checking (env)
  (unless (eq (safety-optimize-quantity env) 3)
    (nx-apply-env-hook policy.inhibit-safety-checking env)))

(defun nx-tailcalls (env)
  (nx-apply-env-hook policy.allow-tail-recursion-elimination env))

(defun nx-allow-transforms (env)
  (nx-apply-env-hook policy.allow-transforms env))



(defun nx-force-boundp-checks (var env)
  (or (eq (safety-optimize-quantity env) 3)
      (nx-apply-env-hook policy.force-boundp-checks var env)))

(defun nx-substititute-constant-value (symbol value env)
  (nx-apply-env-hook policy.allow-constant-substitution symbol value env))

(defun nx-the-typechecks (env)
  (nx-apply-env-hook policy.the-typechecks env))

#-bccl
(defun nx1-default-operator ()
 (or (gethash *nx-sfname* *nx1-operators*)
     (error "Bug - operator not found for  ~S" *nx-sfname*)))

(defun nx-new-temp-var (pending &optional (pname "COMPILER-VAR"))
  (let ((var (nx-new-var pending (make-symbol pname))))
    (nx-set-var-bits var (%ilogior (%ilsl $vbitignoreunused 1)
                                   (%ilsl $vbittemporary 1)
                                   (nx-var-bits var)))
    var))

(defun nx-new-vdecl (pending name class &optional info)
  (push (cons name (cons class info)) (pending-declarations-vdecls pending)))

(defun nx-new-fdecl (pending name class &optional info)
  (push (cons name (cons class info)) (pending-declarations-fdecls pending)))

(defun nx-new-var (pending sym &optional (check t))
  (nx-init-var pending (nx-cons-var (nx-need-var sym check) 0)))
                    
(defun nx-proclaimed-special-p (sym)
  (setq sym (nx-need-sym sym))
  (let* ((defenv (definition-environment *nx-lexical-environment*))
         (specials (if defenv (defenv.specials defenv))))
    (or (assq sym specials)
        (proclaimed-special-p sym))))

(defun nx-proclaimed-parameter-p (sym)
  (or (constantp sym)
      (multiple-value-bind (special-p info) (nx-lex-info sym t)
        (or 
         (and (eq special-p :special) info)
         (let* ((defenv (definition-environment *nx-lexical-environment*)))
           (if defenv 
             (or (%cdr (assq sym (defenv.specials defenv)))
                 (assq sym (defenv.constants defenv)))))))))

(defun nx-process-declarations (pending decls &optional (env *nx-lexical-environment*) &aux s f)
  (dolist (decl decls pending)
    (dolist (spec (%cdr decl))
      (if (memq (setq s (car spec)) *nx-known-declarations*)
        (if (setq f (getf *nx-standard-declaration-handlers* s))
          (funcall f pending spec env))
        ; Any type name is now (ANSI CL) a valid declaration.
        ; We should probably do something to distinguish "type names" from "typo names",
        ; so that (declare (inliMe foo)) warns unless the compiler has some reason to
        ; believe that 'inliMe' (inlemon) has been DEFTYPEd.
        (dolist (var (%cdr spec))
          (if (symbolp var)
            (nx-new-vdecl pending var 'type s)))))))

; Put all variable decls for the symbol VAR into effect in environment ENV.  Now.
; Returns list of all new vdecls pertaining to VAR.
(defun nx-effect-vdecls (pending var env)
  (let ((vdecls (lexenv.vdecls env))
        (own nil))
    (dolist (decl (pending-declarations-vdecls pending) (setf (lexenv.vdecls env) vdecls))
      (when (eq (car decl) var) 
        (when (eq (cadr decl) 'type)
          (let* ((newtype (cddr decl))
                 (merged-type (nx1-type-intersect var newtype (nx-declared-type var env))))
             (unless (eq merged-type newtype)
              (rplacd (cdr decl) merged-type))))
        (push decl vdecls)
        (push (cdr decl) own)))
    own))


(defun nx1-typed-var-initform (pending sym form &optional (env *nx-lexical-environment*))
  (let* ((type t)
         (*nx-form-type* (if (nx-trust-declarations env)
                           (dolist (decl (pending-declarations-vdecls pending)  type)
                            (when (and (eq (car decl) sym) (eq (cadr decl) 'type))
                              (setq type (nx1-type-intersect sym (nx-target-type type) (cddr decl)))))
                           t)))
    (nx1-typed-form form env)))

; Guess.
(defun nx-effect-fdecls (pending var env)
  (let ((fdecls (lexenv.fdecls env))
        (own nil))
    (dolist (decl (pending-declarations-fdecls pending) (setf (lexenv.fdecls env) fdecls))
      (when (eq (car decl) var) 
        (push decl fdecls)
        (push (cdr decl) own)))
    own))




(defun nx-acode-form-typep (form type env)
  (acode-form-typep form type  (nx-trust-declarations env)))

(defun acode-form-typep (form type trust-decls)
  (if (acode-p form)
    (let* ((op (acode-operator form))
           (opval-p (or (eq op (%nx1-operator fixnum)) (eq op (%nx1-operator immediate))))
           (optype (acode-form-type form trust-decls)))
      (values
       (if optype 
         (subtypep optype (nx-target-type type))
         (if opval-p (typep (%cadr form) (nx-target-type type))))))))

(defun nx-acode-form-type (form env)
  (acode-form-type form (nx-trust-declarations env)))

(defparameter *numeric-acode-ops*
  (list (%nx1-operator add2)
        (%nx1-operator sub2)
        (%nx1-operator mul2)))


(defun acode-form-type (form trust-decls)
  (nx-target-type 
   (if (acode-p form)
     (let* ((op (acode-operator form)))
       (if (eq op (%nx1-operator fixnum))
         'fixnum
         (if (eq op (%nx1-operator immediate))
           (type-of (%cadr form))
           (and trust-decls
                (if (eq op (%nx1-operator typed-form))
                  (if (eq (%cadr form) 'number)
                    (or (acode-form-type (nx-untyped-form form) trust-decls)
                        'number)
                    (%cadr form))
                  (if (eq op (%nx1-operator lexical-reference))
                    (let* ((var (cadr form))
                           (bits (nx-var-bits var))
                           (punted (logbitp $vbitpunted bits)))
                      (if (or punted
                              (eql 0 (%ilogand $vsetqmask bits)))
                        (var-inittype var)))
                    (if (or (eq op (%nx1-operator %aref1))
                            (eq op (%nx1-operator simple-typed-aref2))
                            (eq op (%nx1-operator general-aref2))
                            (eq op (%nx1-operator simple-typed-aref3))
                            (eq op (%nx1-operator general-aref3)))
                      (let* ((atype (acode-form-type (cadr form) t))
                             (actype (if atype (specifier-type atype))))
                        (if (typep actype 'array-ctype)
                          (type-specifier (array-ctype-specialized-element-type
                                           actype))))
                      (if (member op *numeric-acode-ops*)
                        (multiple-value-bind (f1 f2)
                            (nx-binop-numeric-contagion (cadr form)
                                                        (caddr form)
                                                        trust-decls)
                          (if (and (acode-form-typep f1 'float trust-decls)
                                   (acode-form-typep f2 'float trust-decls))

                            (if (or (acode-form-typep f1 'double-float trust-decls)
                                    (acode-form-typep f2 'double-float trust-decls))
                            'double-float
                            'single-float)))
                        (cdr (assq op *nx-operator-result-types*)))))))))))))

(defun nx-binop-numeric-contagion (form1 form2 trust-decls)
  (cond ((acode-form-typep form1 'double-float trust-decls)
         (if (acode-form-typep form2 'double-float trust-decls)
           (values form1 form2)
           (let* ((c2 (acode-real-constant-p form2)))
             (if c2
               (values form1 (make-acode (%nx1-operator immediate)
                                         (float c2 0.0d0)))
               (if (acode-form-typep form2 'fixnum trust-decls)
                 (values form1 (make-acode (%nx1-operator %fixnum-to-double)
                                           form2))
                 (values form1 form2))))))
        ((acode-form-typep form2 'double-float trust-decls)
         (let* ((c1 (acode-real-constant-p form1)))
           (if c1
             (values (make-acode (%nx1-operator immediate)
                                 (float c1 0.0d0)) form2)
             (if (acode-form-typep form1 'fixnum trust-decls)
               (values (make-acode (%nx1-operator %fixnum-to-double)
                                   form1) form2)
               (values form1 form2)))))
        ((acode-form-typep form1 'single-float trust-decls)
         (if (acode-form-typep form2 'single-float trust-decls)
           (values form1 form2)
           (let* ((c2 (acode-real-constant-p form2)))
             (if c2
               (values form1 (make-acode (%nx1-operator immediate)
                                         (float c2 0.0f0)))
               (if (acode-form-typep form2 'fixnum trust-decls)
                 (values form1 (make-acode (%nx1-operator %fixnum-to-single)
                                           form2))
                 (values form1 form2))))))
        ((acode-form-typep form2 'single-float trust-decls)
         (let* ((c1 (acode-real-constant-p form1)))
           (if c1
             (values (make-acode (%nx1-operator immediate)
                                 (float c1 0.0f0)) form2)
             (if (acode-form-typep form1 'fixnum trust-decls)
               (values (make-acode (%nx1-operator %fixnum-to-single)
                                   form1) form2)
               (values form1 form2)))))
        (t
         (values form1 form2))))

(defun acode-punted-var-p (var)
  (let ((bits (nx-var-bits var)))
    (and (%ilogbitp $vbitpunted bits)
         (not (%ilogbitp $vbitspecial bits)))))

; Strip off any type info or "punted" lexical references.
; ??? Is it true that the "value" of the punted reference is unwrapped ? ???
(defun acode-unwrapped-form (form) 
  (while (and (consp (setq form (nx-untyped-form form)))
           (eq (%car form) (%nx1-operator lexical-reference))
           (acode-punted-var-p (cadr form)))
    (setq form (var-ea (cadr form))))
  form)

(defun acode-fixnum-form-p (x)
  (setq x (acode-unwrapped-form x))
  (if (acode-p x)
    (if (eq (acode-operator x) (%nx1-operator fixnum)) 
      (cadr x))))

(defun acode-integer-constant-p (x bits)
  (let* ((int (or (acode-fixnum-form-p x)
                  (progn
                    (setq x (acode-unwrapped-form x))
                    (if (acode-p x)
                      (if (and (eq (acode-operator x) (%nx1-operator immediate))
                               (typep (cadr x) 'fixnum))
                        (cadr x)))))))
    (and int
         (or
           (typep int `(signed-byte ,bits))
           (typep int `(unsigned-byte ,bits)))
         int)))

(defun acode-real-constant-p (x)
  (or (acode-fixnum-form-p x)
      (progn
        (setq x (acode-unwrapped-form x))
        (if (acode-p x)
          (if (and (eq (acode-operator x) (%nx1-operator immediate))
                   (typep (cadr x) 'real))
            (cadr x))))))



(defun nx-lookup-target-uvector-subtag (name)
  (or (cdr (assoc name (arch::target-uvector-subtags (backend-target-arch *target-backend*))))
      (nx-error "Type ~s not supported on target ~s"
                name (backend-target-arch-name *target-backend*))))

(defun nx-target-uvector-subtag-name (subtag)
  (or (car (rassoc subtag (arch::target-uvector-subtags (backend-target-arch *target-backend*))))
      (nx-error "Subtag ~s not native on target ~s"
                subtag (backend-target-arch-name *target-backend*))))

(defun nx-error-for-simple-2d-array-type (type-keyword)
  (ecase type-keyword
    (:simple-vector arch::error-object-not-simple-array-t-2d)
    (:simple-string arch::error-object-not-simple-array-char-2d)
    (:bit-vector arch::error-object-not-simple-array-bit-2d)
    (:unsigned-8-bit-vector arch::error-object-not-simple-array-u8-2d)
    (:signed-8-bit-vector arch::error-object-not-simple-array-s8-2d)
    (:unsigned-16-bit-vector arch::error-object-not-simple-array-u16-2d)
    (:signed-16-bit-vector arch::error-object-not-simple-array-s16-2d)
    (:unsigned-32-bit-vector arch::error-object-not-simple-array-u32-2d)
    (:signed-32-bit-vector arch::error-object-not-simple-array-s32-2d)
    (:unsigned-64-bit-vector arch::error-object-not-simple-array-u64-2d)
    (:signed-64-bit-vector arch::error-object-not-simple-array-s64-2d)
    (:double-float-vector arch::error-object-not-simple-array-double-float-2d)
    (:single-float-vector arch::error-object-not-simple-array-double-float-2d)
    (:fixnum-vector arch::error-object-not-simple-array-fixnum-2d)))

(defun nx-error-for-simple-3d-array-type (type-keyword)
  (ecase type-keyword
    (:simple-vector arch::error-object-not-simple-array-t-3d)
    (:simple-string arch::error-object-not-simple-array-char-3d)
    (:bit-vector arch::error-object-not-simple-array-bit-3d)
    (:unsigned-8-bit-vector arch::error-object-not-simple-array-u8-3d)
    (:signed-8-bit-vector arch::error-object-not-simple-array-s8-3d)
    (:unsigned-16-bit-vector arch::error-object-not-simple-array-u16-3d)
    (:signed-16-bit-vector arch::error-object-not-simple-array-s16-3d)
    (:unsigned-32-bit-vector arch::error-object-not-simple-array-u32-3d)
    (:signed-32-bit-vector arch::error-object-not-simple-array-s32-3d)
    (:unsigned-64-bit-vector arch::error-object-not-simple-array-u64-3d)
    (:signed-64-bit-vector arch::error-object-not-simple-array-s64-3d)
    (:double-float-vector arch::error-object-not-simple-array-double-float-3d)
    (:single-float-vector arch::error-object-not-simple-array-double-float-3d)
    (:fixnum-vector arch::error-object-not-simple-array-fixnum-3d)))

(defun acode-s16-constant-p (x)
  (setq x (acode-unwrapped-form x))
  (if (acode-p x)
    (let* ((op (acode-operator x)))
      (if (eql op (%nx1-operator fixnum))
        (let* ((val (cadr x)))
          (if (target-word-size-case
               (32 (typep val '(signed-byte #.(- 16 2))))
               (64 (typep val '(signed-byte #.(- 16 3)))))
            (ash val (target-word-size-case
                      (32 2)
                      (64 3)))))
        (if (eql op (%nx1-operator %unbound-marker))
          (arch::target-unbound-marker-value
           (backend-target-arch *target-backend*))
          (if (eql op (%nx1-operator %slot-unbound-marker))
            (arch::target-slot-unbound-marker-value
             (backend-target-arch *target-backend*))))))))

(defun acode-s32-constant-p (x)
  (setq x (acode-unwrapped-form x))
  (if (acode-p x)
    (let* ((op (acode-operator x)))
      (if (eql op (%nx1-operator fixnum))
        (let* ((val (cadr x)))
          (if (target-word-size-case
               (32 (typep val '(signed-byte #.(- 32 2))))
               (64 (typep val '(signed-byte #.(- 32 3)))))
            (ash val (target-word-size-case
                      (32 2)
                      (64 3)))))
        (if (eql op (%nx1-operator %unbound-marker))
          (arch::target-unbound-marker-value
           (backend-target-arch *target-backend*))
          (if (eql op (%nx1-operator %slot-unbound-marker))
            (arch::target-slot-unbound-marker-value
             (backend-target-arch *target-backend*))))))))

(defun acode-fixnum-type-p (form trust-decls)
  (or (acode-fixnum-form-p form)
      (and trust-decls
           (acode-p form)
           (eq (acode-operator form) (%nx1-operator typed-form))
           (subtypep (cadr form) 'fixnum))))


(defun nx-acode-fixnum-type-p (form env)
    (acode-fixnum-type-p form (nx-trust-declarations env)))

; Is acode-expression the result of alphatizing (%int-to-ptr <integer>) ?
(defun acode-absolute-ptr-p (acode-expression &optional skip)
  (and (acode-p acode-expression)
       (or skip (prog1 (eq (acode-operator acode-expression) (%nx1-operator %macptrptr%))
                  (setq acode-expression (%cadr acode-expression))))
       (eq (acode-operator acode-expression) (%nx1-operator %consmacptr%))
       (eq (acode-operator (setq acode-expression (%cadr acode-expression))) 
           (%nx1-operator %immediate-int-to-ptr))
       (let ((op (acode-operator (setq acode-expression (%cadr acode-expression)))))
         (if (or (eq op (%nx1-operator fixnum))
                 (and (eq op (%nx1-operator immediate))
                      (integerp (%cadr acode-expression))))
           (%cadr acode-expression)))))

(defun nx-check-vdecl-var-ref (decl)
  (unless (eq (cadr decl) 'special)
    (let* ((sym (car decl))
           (info (nx-lex-info sym)))
      (when (or (eq info :symbol-macro)
                (and (null info) (not (nx-proclaimed-special-p sym))))
        (nx1-whine :unknown-declaration-variable (cadr decl) sym)))))


(defun nx-effect-other-decls (pending env)
  (flet ((merge-decls (new old)
                      (dolist (decl new old) (pushnew decl old :test #'eq))))
    (let ((vdecls (pending-declarations-vdecls pending))
          (fdecls (pending-declarations-fdecls pending))
          (mdecls (pending-declarations-mdecls pending)))
      (when vdecls
        (let ((env-vdecls (lexenv.vdecls env)))
          (dolist (decl vdecls (setf (lexenv.vdecls env) env-vdecls))
            (unless (memq decl env-vdecls)
              (nx-check-vdecl-var-ref decl)
              (when (eq (cadr decl) 'type)
                (let* ((var (car decl))
                       (newtype (cddr decl))
                       (merged-type (nx1-type-intersect var newtype (nx-declared-type var env))))
                  (unless (eq merged-type newtype)
                    (rplacd (cdr decl) merged-type))))
              (push decl env-vdecls)))))
      (when fdecls (setf (lexenv.fdecls env) (merge-decls fdecls (lexenv.vdecls env))))
      (when mdecls (setf (lexenv.mdecls env) (merge-decls mdecls (lexenv.mdecls env))))
      (setq *nx-inlined-self* (and (nx-self-calls-inlineable env) 
                                   (let ((name *nx-global-function-name*)) 
                                     (and name (not (nx-declared-notinline-p name env))))))
      (unless (nx-allow-register-allocation env)
        (nx-inhibit-register-allocation))
      (setq *nx-new-p2decls* 
            (%ilogior 
             (if (nx-tailcalls env) $decl_tailcalls 0)
             (if (nx-open-code-in-line env) $decl_opencodeinline 0)
             (if (nx-inhibit-safety-checking env) $decl_unsafe 0)
             (if (nx-trust-declarations env) $decl_trustdecls 0))))))

#|     
(defun nx-find-misc-decl (declname env)
  (loop
    (unless (and env (eq (uvref env 0) 'lexical-environment)) (return))
    (dolist (mdecl (lexenv.mdecls env))
      (if (atom mdecl)
        (if (eq mdecl declname)
          (return-from nx-find-misc-decl t))
        (if (eq (%car mdecl) declname)
          (return-from nx-find-misc-decl (%cdr mdecl)))))
    (setq env (lexenv.parent-env env))))
|#


(defun nx-bad-decls (decls)
  (nx1-whine :unknown-declaration decls))



(defnxdecl special (pending decl env)
  (declare (ignore env))
  (dolist (s (%cdr decl))
    (if (symbolp s) 
      (nx-new-vdecl pending s 'special)
      (nx-bad-decls decl))))

(defnxdecl dynamic-extent (pending decl env)
  (declare (ignore env))
  (dolist (s (%cdr decl))
    (if (symbolp s) 
      (nx-new-vdecl pending s 'dynamic-extent t)
      (if (and (consp s)
               (eq (%car s) 'function)
               (consp (%cdr s))
               (valid-function-name-p (cadr s))
               (setq s (validate-function-name (cadr s))))
        (nx-new-fdecl pending s 'dynamic-extent t)
        (nx-bad-decls decl)))))

(defnxdecl ignorable (pending decl env)
  (declare (ignore env))
  (dolist (s (%cdr decl))
    (if (symbolp s) 
      (nx-new-vdecl pending s 'ignore-if-unused t)
      (if (and (consp s)
               (eq (%car s) 'function)
               (consp (%cdr s))
               (valid-function-name-p (cadr s))
               (setq s (validate-function-name (cadr s))))
        (nx-new-fdecl pending s 'ignore-if-unused t)
        (nx-bad-decls decl)))))

(defnxdecl ftype (pending decl env)
  (declare (ignore env))
  (destructuring-bind (type &rest fnames) (%cdr decl)
    (dolist (s fnames)
      (nx-new-fdecl pending s 'ftype type))))

(defnxdecl settable (pending decl env)
  (nx-settable-decls pending decl env t))

(defnxdecl unsettable (pending decl env)
  (nx-settable-decls pending decl env nil))

(defun nx-settable-decls (pending decl env val)
  (declare (ignore env))
  (dolist (s (%cdr decl))
    (if (symbolp s)
      (nx-new-vdecl pending s 'settable val)
      (nx-bad-decls decl))))

(defnxdecl type (pending decl env)
  (declare (ignore env))
  (labels ((kludge (type) ; 0 => known, 1 => unknown, 2=> illegal
             (cond ((type-specifier-p type)
                    0)
                   ((and (consp type)
                         (member (car type) '(and or))
                         (not (null (list-length type))))
                    (do ((result 0 (max result (kludge (car tail))))
                         (tail (cdr type) (cdr tail)))
                        ((null tail)
                         result)))
                   ((not (symbolp type))
                    ;;>>>> nx-bad-decls shouldn't signal a fatal error!!!!
                    ;;>>>> Most callers of nx-bad-decls should just ignore the
                    ;;>>>> losing decl element and proceed with the rest
                    ;;>>>>  (ie (declare (ignore foo (bar) baz)) should
                    ;;>>>>   have the effect of ignoring foo and baz as well
                    ;;>>>>   as WARNING about the mal-formed declaration.)
                    (nx-bad-decls decl)
                    2)
                   (t 1))))
    (let* ((spec (%cdr decl))
           (type (car spec)))
      (case (kludge type)
        ((0)
         (dolist (sym (cdr spec))
           (if (symbolp sym)
             (nx-new-vdecl pending sym 'type type)
             (nx-bad-decls decl))))
        ((1)
         (dolist (sym (cdr spec))
           (unless (symbolp sym)
             (nx-bad-decls decl))))
        ((2)
         (nx-bad-decls decl))))))



(defnxdecl global-function-name (pending decl env)
  (declare (ignore pending))
  (when *nx-parsing-lambda-decls*
    (let ((name (cadr decl)))
      (setq *nx-global-function-name* (setf (afunc-name *nx-current-function*) name))
      (setq *nx-inlined-self* (not (nx-declared-notinline-p name env))))))

(defnxdecl debugging-function-name (pending decl env)
  (declare (ignore pending env))
  (when *nx-parsing-lambda-decls*
    (setf (afunc-name *nx-current-function*) (cadr decl))))

(defnxdecl resident (pending decl env)
  (declare (ignore env pending))
  (declare (ignore decl))
  (nx-decl-set-fbit $fbitresident))


(defun nx-inline-decl (pending decl val &aux valid-name)
  (dolist (s (%cdr decl))
    (multiple-value-setq (valid-name s) (valid-function-name-p s))
    (if valid-name
      (progn
        (if (nx-self-call-p s nil t)
          (setq *nx-inlined-self* val))
        (nx-new-fdecl pending s 'inline (if val 'inline 'notinline)))
      (nx-bad-decls decl))))

(defnxdecl inline (pending decl env)
  (declare (ignore env))
  (nx-inline-decl pending decl t))

(defnxdecl notinline (pending decl env)
  (declare (ignore env))
  (nx-inline-decl pending decl nil))

(defnxdecl ignore (pending decl env)
  (declare (ignore env))
  (dolist (s (%cdr decl))
    (if (symbolp s)      
      (nx-new-vdecl pending s 'ignore t)
      (if (and (consp s)
               (eq (%car s) 'function)
               (consp (%cdr s))
               (valid-function-name-p (cadr s))
               (setq s (validate-function-name (cadr s))))
        (nx-new-fdecl pending s 'ignore t)
        (nx-bad-decls decl)))))

(defnxdecl ignore-if-unused (pending decl env)
  (declare (ignore env))
  (dolist (s (%cdr decl))
    (if (symbolp s) 
      (nx-new-vdecl pending s 'ignore-if-unused)
      (nx-bad-decls decl))))

(defun nx-self-call-p (name &optional ignore-lexical (allow *nx-inlined-self*))
  (when (and name (symbolp name))
    (let ((current-afunc *nx-current-function*)
          (target-afunc (unless ignore-lexical (nth-value 1 (nx-lexical-finfo name)))))
      (or (eq current-afunc target-afunc)
          (and allow
               (eq name *nx-global-function-name*)
               (null target-afunc)
               (null (afunc-parent current-afunc)))))))

(defun nx-check-var-usage (var)
  (let* ((sym (var-name var))
         (bits (nx-var-bits var))
         (expansion (var-ea var))
         (setqed (%ilogbitp $vbitsetq bits))
         (reffed (%ilogbitp $vbitreffed bits))
         (closed (%ilogbitp $vbitclosed bits))
         (special (%ilogbitp $vbitspecial bits))
         (ignored (%ilogbitp $vbitignore bits))
         (ignoreunused (%ilogbitp $vbitignoreunused bits)))
    (if (or special reffed closed)
      (progn
        (if ignored (nx1-whine :ignore sym))
        (nx-set-var-bits var (%ilogand (nx-check-downward-vcell var bits) (%ilognot (%ilsl $vbitignore 1)))))
      (progn
        (if (and setqed ignored) (nx1-whine :ignore sym))
        (or ignored ignoreunused 
            (progn (and (consp expansion) (eq (car expansion) :symbol-macro) (setq sym (list :symbol-macro sym))) (nx1-whine :unused sym)))
        (when (%izerop (%ilogand bits (%ilogior $vrefmask $vsetqmask)))
          (nx-set-var-bits var (%ilogior (%ilsl $vbitignore 1) bits)))))))

; if an inherited var isn't setqed, it gets no vcell.  If it -is- setqed, but
; all inheritors are downward, the vcell can be stack-consed.  Set a bit so that
; the right thing happens when the var is bound.
; Set the bit for the next-method var even if it is not setqed.
(defun nx-check-downward-vcell (v bits)
  (if (and (%ilogbitp $vbitclosed bits)
           (or (%ilogbitp $vbitsetq bits)
               (eq v *nx-next-method-var*))
           (nx-afuncs-downward-p v (afunc-inner-functions *nx-current-function*)))
    (%ilogior (%ilsl $vbitcloseddownward 1) bits)
    bits))

; afunc is "downward wrt v" if it doesn't inherit v or if all refs to afunc
; are "downward" and no inner function of afunc is not downward with respect to v.
(defun nx-afunc-downward-p (v afunc)
  (or (dolist (i (afunc-inherited-vars afunc) t)
        (when (eq (nx-root-var i) v) (return nil)))
      (if (nx-afuncs-downward-p v (afunc-inner-functions afunc))
        (eq (afunc-fn-refcount afunc)
            (afunc-fn-downward-refcount afunc)))))

(defun nx-afuncs-downward-p (v afuncs)
  (dolist (afunc afuncs t)
    (unless (nx-afunc-downward-p v afunc) (return nil))))

(defun nx1-punt-bindings (vars initforms)
  (dolist (v vars)
    (nx1-punt-var v (pop initforms))))


(defun nx1-note-var-binding (var initform)
  (let* ((init (nx-untyped-form initform))
         (inittype (nx-acode-form-type initform *nx-lexical-environment*))
         (bits (nx-var-bits var)))
    (when (%ilogbitp $vbitspecial bits) (nx-record-xref-info :binds (var-name var)))
    (when inittype (setf (var-inittype var) inittype))
    (when (and (not (%ilogbitp $vbitspecial bits))
               (consp init))
      (let* ((op (acode-operator init)))
        (if (eq op (%nx1-operator lexical-reference))
          (let* ((target (%cadr init))
                 (setq-count (%ilsr 8 (%ilogand $vsetqmask (nx-var-bits target)))))
            (unless (eq setq-count (%ilsr 8 $vsetqmask))
              (cons var (cons setq-count target))))
          (if (and (%ilogbitp $vbitdynamicextent bits)
                   (or (eq op (%nx1-operator closed-function))
                       (eq op (%nx1-operator simple-function))))
            (let* ((afunc (%cadr init)))
              (setf (afunc-fn-downward-refcount afunc)
                    (afunc-fn-refcount afunc)
                    (afunc-bits afunc) (logior (ash 1 $fbitdownward) (ash 1 $fbitbounddownward)
                                               (the fixnum (afunc-bits afunc))))
              nil)))))))


;;; Process entries involving variables bound to other variables at
;;; the end of a binding construct.  Each entry is of the form
;;; (source-var setq-count . target-var), where setq-count is the
;;; assignment count of TARGET-VAR at the time that the binding's
;;; initform was evaluated (not, in the case of LET, at the time that
;;; the bindinw was established.).  If the target isn't closed-over
;;; and SETQed (somewhere), and wasn't setqed in the body (e.g.,
;;; still has the same assignment-count as it had when the initform
;;; was executed), then we can "punt" the source (and replace references
;;; to it with references to the target.)
;;; It obviously makes no sense to do this if the source is SPECIAL;
;;; in some cases (LET), we create the source variable and add it to
;;; this alist before it's known whether or not the source variable
;;; is SPECIAL. so we have to ignore that case here.
(defun nx1-check-var-bindings (alist)
  (dolist (pair alist)
    (let* ((var (car pair))
           (target (cddr pair))
           (vbits (nx-var-bits var))
           (target-bits (nx-var-bits target)))
      (unless (or
               ;; var can't be special, setq'ed or closed; target can't be
               ;; setq'ed AND closed.
               (neq (%ilogand vbits (%ilogior (%ilsl $vbitsetq 1)
                                              (%ilsl $vbitclosed 1)
                                              (%ilsl $vbitspecial 1))) 0)
               (eq (%ilogior (%ilsl $vbitsetq 1) (%ilsl $vbitclosed 1)) 
                   (%ilogand
                     (%ilogior (%ilsl $vbitsetq 1) (%ilsl $vbitclosed 1))
                     target-bits))
               (neq (%ilsr 8 (%ilogand $vsetqmask target-bits)) (cadr pair)))
             (push (cons var target) *nx-punted-vars*)))))

(defun nx1-punt-var (var initform)
  (let* ((bits (nx-var-bits var))
         (mask (%ilogior (%ilsl $vbitsetq 1) (ash -1 $vbitspecial) (%ilsl $vbitclosed 1)))
         (nrefs (%ilogand $vrefmask bits))
         (val (nx-untyped-form initform))
         (op (if (acode-p val) (acode-operator val))))
    (when (%izerop (%ilogand mask bits))
      (if
        (or 
         (nx-t val)
         (nx-null val)
         (and (eql nrefs 1) (not (logbitp $vbitdynamicextent bits)) ( acode-absolute-ptr-p val t))
         (eq op (%nx1-operator fixnum))
         (eq op (%nx1-operator immediate)))
        (progn
          (nx-set-var-bits var (%ilogior (%ilsl $vbitpuntable 1) bits)))))
    (when (and (%ilogbitp $vbitdynamicextent bits)
               (or (eq op (%nx1-operator closed-function))
                   (eq op (%nx1-operator simple-function))))
      (let* ((afunc (cadr val)))
        (setf (afunc-bits afunc) (%ilogior (%ilsl $fbitbounddownward 1) (afunc-bits afunc))
              (afunc-fn-downward-refcount afunc) 1))) 
    nil))
            
(defnxdecl optimize (pending specs env)
  (declare (ignore env))
  (let* ((q nil)
         (v nil)
         (mdecls (pending-declarations-mdecls pending)))
    (dolist (spec (%cdr specs) (setf (pending-declarations-mdecls pending) mdecls))
      (if (atom spec)
        (setq q spec v 3)
        (setq q (%car spec) v (cadr spec)))
      (if (and (fixnump v) (<= 0 v 3) (memq q '(speed space compilation-speed safety debug)))
        (push (cons q v) mdecls)
        (nx-bad-decls specs)))))

(defun %proclaim-optimize (specs &aux q v)
 (dolist (spec specs)
  (if (atom spec)
   (setq q spec v 3)
   (setq q (%car spec) v (cadr spec)))
  (when (and (fixnump v) (<= 0 v 3))
   (if (eq q 'speed)
    (setq *nx-speed* v)
    (if (eq q 'space)
     (setq *nx-space* v)
     (if (eq q 'compilation-speed)
      (setq *nx-cspeed* v)
      (if (eq q 'safety)
       (setq *nx-safety* v)
       (if (eq q 'debug)
         (setq *nx-debug* v)))))))))

(defun nx-lexical-finfo (sym &optional (env *nx-lexical-environment*))
  (let* ((info nil)
         (barrier-crossed nil))
    (if env
      (loop
        (when (eq 'barrier (lexenv.variables env))
          (setq barrier-crossed t))
        (when (setq info (%cdr (assq sym (lexenv.functions env))))
          (return (values info (if (and (eq (car info) 'function)
                                        (consp (%cdr info)))
                                 (progn
                                   (when barrier-crossed
                                     (nx-error "Illegal reference to lexically-defined function ~S." sym))
                                   (%cadr info))))))
        (if (listp (setq env (lexenv.parent-env env)))
          (return (values nil nil))))
      (values nil nil))))

(defun nx-inline-expansion (sym &optional (env *nx-lexical-environment*) global-only)
  (let* ((lambda-form nil)
         (containing-env nil)
         (token nil))
    (if (and (nx-declared-inline-p sym env)
             (not (gethash sym *nx1-alphatizers*)))
      (multiple-value-bind (info afunc) (unless global-only (nx-lexical-finfo sym env))
        (if info (setq token afunc 
                       containing-env (afunc-environment afunc)
                       lambda-form (afunc-lambdaform afunc)))
        (let* ((defenv (definition-environment env)))
          (if (cdr (setq info (if defenv (cdr (assq sym (defenv.defined defenv))))))
            (setq lambda-form (cdr info)
                  token sym
                  containing-env (new-lexical-environment defenv))
            (unless info
              (if (cdr (setq info (assq sym *nx-globally-inline*)))
                (setq lambda-form (%cdr info)
                      token sym
                      containing-env (new-lexical-environment (new-definition-environment nil)))))))))
    (values lambda-form (nx-closed-environment env containing-env) token)))

(defun nx-closed-environment (current-env target)
  (when target
    (let* ((intervening-functions nil))
      (do* ((env current-env (lexenv.parent-env env)))
           ((or (eq env target) (null env) (eq (%svref env 0) 'definition-environment)))
        (let* ((fn (lexenv.lambda env)))
          (when fn (push fn intervening-functions))))
      (let* ((result target))
        (dolist (fn intervening-functions result)
          (setf (lexenv.lambda (setq result (new-lexical-environment result))) fn))))))

(defun nx-root-var (v)
  (do* ((v v bits)
        (bits (var-bits v) (var-bits v)))
       ((fixnump bits) v)))

(defun nx-reconcile-inherited-vars (more)
  (let ((last nil)) ; Bop 'til ya drop.
    (loop
      (setq last more more nil)
      (dolist (callee last)
        (dolist (caller (afunc-callers callee))
          (unless (or (eq caller callee)
                      (eq caller (afunc-parent callee)))
            (dolist (v (afunc-inherited-vars callee))
              (let ((root-v (nx-root-var v)))
                (unless (dolist (caller-v (afunc-inherited-vars caller))
                          (when (eq root-v (nx-root-var caller-v))
                            (return t)))
                  ; caller must inherit root-v in order to call callee without using closure.
                  ; can't just bind afunc & call nx-lex-info here, 'cause caller may have
                  ; already shadowed another var with same name.  So:
                  ; 1) find the ancestor of callee which bound v; this afunc is also an ancestor
                  ;    of caller
                  ; 2) ensure that each afunc on the inheritance path from caller to this common
                  ;    ancestor inherits root-v.
                  (let ((ancestor (afunc-parent callee))
                        (inheritors (list caller)))
                    (until (eq (setq v (var-bits v)) root-v)
                      (setq ancestor (afunc-parent ancestor)))
                    (do* ((p (afunc-parent caller) (afunc-parent p)))
                         ((eq p ancestor))
                      (push p inheritors))
                    (dolist (f inheritors)
                      (setq v (nx-cons-var (var-name v) v))
                      (unless (dolist (i (afunc-inherited-vars f))
                                (when (eq root-v (nx-root-var i))
                                  (return (setq v i))))
                        (pushnew f more)
                        (push v (afunc-inherited-vars f))
                        ; change shared structure of all refs in acode with one swell foop.
                        (nx1-afunc-ref f))))))))))    
      (unless more (return)))))

(defun nx-inherit-var (var binder current)
  (if (eq binder current)
    (progn
      (nx-set-var-bits var (%ilogior2 (%ilsl $vbitclosed 1) (nx-var-bits var)))
      var)
    (let ((sym (var-name var)))
      (or (dolist (already (afunc-inherited-vars current))
            (when (eq sym (var-name already)) (return already)))
          (progn
            (setq var (nx-cons-var sym (nx-inherit-var var binder (afunc-parent current))))
            (push var (afunc-inherited-vars current))
            var)))))

(defun nx-lex-info (sym &optional current-only)
  (let* ((current-function *nx-current-function*)
         (catch nil)
         (barrier-crossed nil))
    (multiple-value-bind 
      (info afunc)
      (do* ((env *nx-lexical-environment* (lexenv.parent-env env))
            (continue env (and env (neq (%svref env 0) 'definition-environment)))
            (binder current-function (or (if continue (lexenv.lambda env)) binder)))
           ((or (not continue) (and (neq binder current-function) current-only)) 
            (values nil nil))
        (let ((vars (lexenv.variables env)))
          (if (eq vars 'catch) 
            (setq catch t)
            (if (eq vars 'barrier)
              (setq barrier-crossed t)
              (let ((v (dolist (var vars)
                         (when (eq (var-name var) sym) (return var)))))
                (when v (return (values v binder)))
                (dolist (decl (lexenv.vdecls env))
                  (when (and (eq (car decl) sym)
                             (eq (cadr decl) 'special))
                    (return-from nx-lex-info (values :special nil nil)))))))))
      (if info
        (if (var-expansion info)
          (values :symbol-macro (cdr (var-expansion info)) info)
          (if (%ilogbitp $vbitspecial (nx-var-bits info))
            (values :special info nil)
            (if barrier-crossed
              (nx-error "Illegal reference to lexically defined variable ~S." sym)
              (if (eq afunc current-function)
                (values info nil catch)
                (values (nx-inherit-var info afunc current-function) t catch)))))
        (values nil nil nil)))))


(defun nx-block-info (blockname &optional (afunc *nx-current-function*) &aux
  blocks
  parent
  (toplevel (eq afunc *nx-current-function*))
  blockinfo)
 (when afunc
  (setq
   blocks (if toplevel *nx-blocks* (afunc-blocks afunc))
   blockinfo (assq blockname blocks)
   parent (afunc-parent afunc))
  (if blockinfo
   (values blockinfo nil)
   (when parent
    (when (setq blockinfo (nx-block-info blockname parent))
     (values blockinfo t))))))

(defun nx-tag-info (tagname &optional (afunc *nx-current-function*) &aux
                            tags
                            parent
                            index
                            counter
                            (toplevel (eq afunc *nx-current-function*))
                            taginfo)
  (when afunc
    (setq
     tags (if toplevel *nx-tags* (afunc-tags afunc))
     taginfo (assoc tagname tags)
     parent (afunc-parent afunc))
    (if taginfo
      (values taginfo nil)
      (when (and parent (setq taginfo (nx-tag-info tagname parent)))
        (unless (setq index (cadr taginfo))
          (setq counter (caddr taginfo))
          (%rplaca counter (%i+ (%car counter) 1))
          (setq index (%car counter))
          (%rplaca (%cdr taginfo) index))
        (values taginfo index)))))

(defun nx1-transitively-punt-bindings (pairs) 
  (dolist (pair (nreverse pairs))
    (let* ((var         (%car pair))
           (boundto     (%cdr pair))
           (varbits     (nx-var-bits var))
           (boundtobits (nx-var-bits boundto)))
      (declare (fixnum varbits boundtobits))
      (unless (eq (%ilogior
                    (%ilsl $vbitsetq 1)
                    (%ilsl $vbitclosed 1))
                  (%ilogand
                    (%ilogior
                      (%ilsl $vbitsetq 1)
                      (%ilsl $vbitclosed 1))
                    boundtobits))
        ;; Can't happen -
        (unless (%izerop (%ilogand (%ilogior
                                     (%ilsl $vbitsetq 1) 
                                     (ash -1 $vbitspecial)
                                     (%ilsl $vbitclosed 1)) varbits))
          (error "Bug-o-rama - \"punted\" var had bogus bits.
Or something. Right? ~s ~s" var varbits))
        (let* ((varcount     (%ilogand $vrefmask varbits)) 
               (boundtocount (%ilogand $vrefmask boundtobits)))
          (nx-set-var-bits var (%ilogior
                                 (%ilsl $vbitpuntable 1)
                                 (%i- varbits varcount)))
              (nx-set-var-bits
               boundto
                 (%i+ (%i- boundtobits boundtocount)
                      (%ilogand $vrefmask
                                (%i+ (%i- boundtocount 1) varcount)))))))))

;; Home-baked handler-case replacement.  About 10 times as fast as full handler-case.
;;(LET ((S 0)) (DOTIMES (I 1000000) (INCF S))) took 45,678 microseconds
;;(LET ((S 0)) (DOTIMES (I 1000000) (BLOCK X (ERROR (CATCH 'X (RETURN-FROM X (INCF S))))))) took 57,485
;;(LET ((S 0)) (DOTIMES (I 1000000) (HANDLER-CASE (INCF S) (ERROR (C) C)))) took 168,947
(defmacro with-program-error-handler (handler &body body)
  (let ((tag (gensym)))
    `(block ,tag
       (,handler (catch 'program-error-handler (return-from ,tag (progn ,@body)))))))

(defun runtime-program-error-form (c)
  `(signal-program-error "Invalid program: ~a" ,(princ-to-string c)))

(defun nx1-compile-lambda (name lambda-form &optional
                                 (p (make-afunc))
                                 q
                                 parent-env
                                 (policy *default-compiler-policy*)
                                 load-time-eval-token)
  (if q
     (setf (afunc-parent p) q))

  ;; In the case of a method function, the name will get reset at load time to the
  ;; method object.  However, during compilation, we want any inner functions to use
  ;; the fully qualified method name, so store that.
  (when (method-lambda-p lambda-form)
    (setq name (or *nx-method-warning-name* name)))

  (setf (afunc-name p)
        (let ((parent-name (and (afunc-parent p) (afunc-name (afunc-parent p)))))
          (if parent-name
            (if (and (consp parent-name) (eq (%car parent-name) :internal))
              (if name
                `(:internal ,name ,@(cdr parent-name))
                parent-name)
              (if name
                `(:internal ,name ,parent-name)
                `(:internal ,parent-name)))
            name)))

  (unless (lambda-expression-p lambda-form)
    (nx-error "~S is not a valid lambda expression." lambda-form))
  (let* ((*nx-current-function* p)
         (*nx-parent-function* q)
         (*nx-lexical-environment* (new-lexical-environment parent-env))
         (*nx-load-time-eval-token* load-time-eval-token)
         (*nx-all-vars* nil)
         (*nx-bound-vars* nil)
         (*nx-punted-vars* nil)
         (*nx-current-compiler-policy* policy)
         (*nx-blocks* nil)
         (*nx-tags* nil)
         (*nx-loop-nesting-level* 0)
         (*nx-inner-functions* nil)
         (*nx-global-function-name* nil)
         (*nx-warnings* nil)
         (*nx1-fcells* nil)
         (*nx1-vcells* nil)
         (*nx-inline-expansions* nil)
         (*nx-parsing-lambda-decls* nil)
         (*nx-next-method-var* (if q *nx-next-method-var*))
         (*nx-call-next-method-function* (if q *nx-call-next-method-function*))
         (*nx-cur-func-name* name))
    (if (%non-empty-environment-p *nx-lexical-environment*)
      (setf (afunc-bits p) (logior (ash 1 $fbitnonnullenv) (the fixnum (afunc-bits p)))))

    (setf (afunc-lambdaform p) lambda-form)
    (with-program-error-handler
	(lambda (c)
	  (setf (afunc-acode p) (nx1-lambda '(&rest args) `(args ,(runtime-program-error-form c)) nil)))
      (handler-bind ((warning (lambda (c)
                                (nx1-whine :program-error c)
                                (muffle-warning c)))
                     (program-error (lambda (c)
                                      (when *nx-break-on-program-errors*
                                        (cerror "continue compilation ignoring this form" c))
                                      (when (typep c 'compile-time-program-error)
                                        (setq c (make-condition 'simple-program-error
                                                                :format-control (simple-condition-format-control c)
                                                                :format-arguments (simple-condition-format-arguments c))))
                                      (unless *nx-break-on-program-errors*
                                        (nx1-whine :program-error c))
                                      (throw 'program-error-handler c))))
	(multiple-value-bind (body decls)
	    (with-program-error-handler (lambda (c) (runtime-program-error-form c))
	      (parse-body (%cddr lambda-form) *nx-lexical-environment* t))
	  (setf (afunc-acode p) (nx1-lambda (%cadr lambda-form) body decls)))))

    (nx1-transitively-punt-bindings *nx-punted-vars*)
    (setf (afunc-blocks p) *nx-blocks*)
    (setf (afunc-tags p) *nx-tags*)
    (setf (afunc-inner-functions p) *nx-inner-functions*)
    (setf (afunc-all-vars p) *nx-all-vars*)
    (setf (afunc-vcells p) *nx1-vcells*)
    (setf (afunc-fcells p) *nx1-fcells*)
    (let* ((warnings (merge-compiler-warnings *nx-warnings*))
	   (name *nx-cur-func-name*))        
      (dolist (inner *nx-inner-functions*)
	(dolist (w (afunc-warnings inner))
	  (push name (compiler-warning-function-name w))
	  (push w warnings)))
      (setf (afunc-warnings p) warnings))
    p))

(defun method-lambda-p (form)
  (and (consp form)
       (consp (setq form (%cdr form)))       
       (eq (caar form) '&method)))
         





(defun nx1-lambda (ll body decls &aux (l ll) methvar)
  (let ((old-env *nx-lexical-environment*)
        (*nx-bound-vars* *nx-bound-vars*))
    (with-nx-declarations (pending)
      (let* ((*nx-parsing-lambda-decls* t))
        (nx-process-declarations pending decls))
      (when (eq (car l) '&lap)
        (let ((bits nil))
          (unless (and (eq (length (%cdr l)) 1) (fixnump (setq bits (%cadr l))))
            (unless (setq bits (encode-lambda-list (%cdr l)))
              (nx-error "invalid lambda-list  - ~s" l)))
          (return-from nx1-lambda
                       (list
                        (%nx1-operator lambda-list)
                        (list (cons '&lap bits))
                        nil
                        nil
                        nil
                        nil
                        (nx1-env-body body old-env)
                        *nx-new-p2decls*))))
      (when (eq (car l) '&method)
        (setf (afunc-bits *nx-current-function*)
              (%ilogior (%ilsl $fbitmethodp 1)
                        (afunc-bits *nx-current-function*)))
        (setq *nx-inlined-self* nil)
        (setq *nx-next-method-var* (setq methvar (let ((var (nx-new-var
							     pending
							     (%cadr ll))))
                                                   (nx-set-var-bits var (%ilogior 
                                                                          (%ilsl $vbitignoreunused 1) 
                                                                          ;(%ilsl $vbitnoreg 1) 
                                                                          (nx-var-bits var)))
                                                   var)))
                                                   
        (setq ll (%cddr ll)))
      (multiple-value-bind (req opt rest keys auxen lexpr)
                           (nx-parse-simple-lambda-list pending ll)
        (nx-effect-other-decls pending *nx-lexical-environment*)
        (setq body (nx1-env-body body old-env))
        (nx1-punt-bindings (%car auxen) (%cdr auxen))          
        (when methvar
          (push methvar req)
          (unless (eq 0 (%ilogand (%ilogior (%ilsl $vbitreffed 1)
                                            (%ilsl $vbitclosed 1)
                                            (%ilsl $vbitsetq 1))
                                  (nx-var-bits methvar)))
            (setf (afunc-bits *nx-current-function*)
                  (%ilogior 
                   (%ilsl $fbitnextmethp 1)
                   (afunc-bits *nx-current-function*)))))
        (make-acode
         (%nx1-operator lambda-list) 
         req
         opt 
         (if lexpr (list rest) rest)
         keys
         auxen
         body
         *nx-new-p2decls*)))))
  
(defun nx-parse-simple-lambda-list (pending ll &aux
					      req
					      opt
					      rest
					      keys
					      lexpr
					      sym)
  (multiple-value-bind (ok reqsyms opttail resttail keytail auxtail)
                       (verify-lambda-list ll)
    (unless ok (nx-error "Bad lambda list : ~S" ll))
    (dolist (var reqsyms)
      (push (nx-new-var pending var t) req))
    (when (eq (pop opttail) '&optional)
      (let* (optvars optinits optsuppliedp)
        (until (eq opttail resttail) 
          (setq sym (pop opttail))
          (let* ((var sym)
                 (initform nil)
                 (spvar nil))
            (when (consp var)
              (setq sym (pop var) initform (pop var) spvar (%car var)))
            (push (nx1-typed-var-initform pending sym initform) optinits)
            (push (nx-new-var pending sym t) optvars)
            (push (if spvar (nx-new-var pending spvar t)) optsuppliedp)))
        (if optvars
          (setq opt (list (nreverse optvars) (nreverse optinits) (nreverse optsuppliedp)))
          (nx1-whine :lambda ll))))
    (let ((temp (pop resttail)))
      (when (or (eq temp '&rest)
                (setq lexpr (eq temp '&lexpr)))
        (setq rest (nx-new-var pending (%car resttail) t))))
    (when (eq (%car keytail) '&key) 
      (setq keytail (%cdr keytail))
      (let* ((keysyms ())
             (keykeys ())
             (keyinits ())
             (keysupp ())
             (kallowother (not (null (memq '&allow-other-keys ll))))
             (kvar ())
             (kkey ())
             (kinit ())
             (ksupp))
        (until (eq keytail auxtail)
          (unless (eq (setq sym (pop keytail)) '&allow-other-keys)      
            (setq kinit *nx-nil* ksupp nil)
            (if (atom sym)
              (setq kvar sym kkey (make-keyword sym))
              (progn
                (if (consp (%car sym))
                  (setq kkey (%caar sym) kvar (%cadar sym))
                  (progn
                    (setq kvar (%car sym))
                    (setq kkey (make-keyword kvar))))
                (setq kinit (nx1-typed-var-initform pending kvar (%cadr sym)))
                (setq ksupp (%caddr sym))))
            (push (nx-new-var pending kvar t) keysyms)
            (push kkey keykeys)
            (push kinit keyinits)
            (push (if ksupp (nx-new-var pending ksupp t)) keysupp)))
        (setq 
         keys
         (list
          kallowother
          (nreverse keysyms)
          (nreverse keysupp)
          (nreverse keyinits)
          (apply #'vector (nreverse keykeys))))))
    (let (auxvals auxvars)
      (dolist (pair (%cdr auxtail))
        (let* ((auxvar (nx-pair-name pair))
               (auxval (nx1-typed-var-initform pending auxvar (nx-pair-initform pair))))
          (push auxval auxvals)
          (push (nx-new-var pending auxvar t) auxvars)))
      (values
       (nreverse req) 
       opt 
       rest
       keys
       (list (nreverse auxvars) (nreverse auxvals))
       lexpr))))

(defun nx-new-structured-var (pending sym)
  (if sym
    (nx-new-var pending sym t)
    (nx-new-temp-var pending)))

(defun nx-parse-structured-lambda-list (pending ll &optional no-acode whole-p &aux
                                           req
                                           opt
                                           rest
                                           keys
                                           sym)
  (multiple-value-bind (ok reqsyms opttail resttail keytail auxtail all whole structured-p)
                       (verify-lambda-list ll t whole-p nil)
    (declare (ignore all))
    (unless ok (nx-error "Bad lambda list : ~S" ll))
    (if (or whole (and whole-p structured-p)) (setq whole (nx-new-structured-var pending whole)))
    (dolist (var reqsyms)
      (push (if (symbolp var)
                    (nx-new-structured-var pending var)
                    (nx-structured-lambda-form pending var no-acode))
                  req))
    (when (eq (pop opttail) '&optional)
      (let* (optvars optinits optsuppliedp)
        (until (eq opttail resttail) 
          (setq sym (pop opttail))
          (let* ((var sym)
                 (initform nil)
                 (spvar nil))
            (when (consp var)
              (setq sym (pop var) initform (pop var) spvar (%car var)))
            (push (if no-acode initform (nx1-form initform)) optinits)
            (push (if (symbolp sym)
                          (nx-new-structured-var pending sym)
                          (nx-structured-lambda-form pending sym no-acode))
                        optvars)
            (push (if spvar (nx-new-var pending spvar)) optsuppliedp)))
        (if optvars
          (setq opt (list (nreverse optvars) (nreverse optinits) (nreverse optsuppliedp)))
          (nx1-whine :lambda ll))))
    (let ((var (pop resttail)))
      (when (or (eq var '&rest)
                (eq var '&body))
        (setq var (pop resttail)
              rest (if (symbolp var)
                     (nx-new-structured-var pending var)
                     (nx-structured-lambda-form pending var no-acode)))))
    (when (eq (%car keytail) '&key) 
      (setq keytail (%cdr keytail))
      (let* ((keysyms ())
             (keykeys ())
             (keyinits ())
             (keysupp ())
             (kallowother (not (null (memq '&allow-other-keys ll))))
             (kvar ())
             (kkey ())
             (kinit ())
             (ksupp))
        (until (eq keytail auxtail)
          (unless (eq (setq sym (pop keytail)) '&allow-other-keys)      
            (setq kinit *nx-nil* ksupp nil)
            (if (atom sym)
              (setq kvar sym kkey (make-keyword sym))
              (progn
                (if (consp (%car sym))
                  (setq kkey (%caar sym) kvar (%cadar sym))
                  (progn
                    (setq kvar (%car sym))
                    (setq kkey (make-keyword kvar))))
                (setq kinit (if no-acode (%cadr sym) (nx1-form (%cadr sym))))
                (setq ksupp (%caddr sym))))
            (push (if (symbolp kvar)
                          (nx-new-structured-var pending kvar)
                          (nx-structured-lambda-form pending kvar no-acode))
                        keysyms)
            (push kkey keykeys)
            (push kinit keyinits)
            (push (if ksupp (nx-new-var pending ksupp)) keysupp)))
        (setq 
         keys
         (list
          kallowother
          (nreverse keysyms)
          (nreverse keysupp)
          (nreverse keyinits)
          (apply #'vector (nreverse keykeys))))))
    (let (auxvals auxvars)
      (dolist (pair (%cdr auxtail))
        (let ((auxvar (nx-pair-name pair))
              (auxval (nx-pair-initform pair)))
          (push (if no-acode auxval (nx1-form auxval)) auxvals)
          (push (nx-new-var pending auxvar) auxvars)))
      (values
       (nreverse req) 
       opt 
       rest 
       keys
       (list (nreverse auxvars) (nreverse auxvals))
       whole))))

(defun nx-structured-lambda-form (pending l &optional no-acode)
  (multiple-value-bind (req opt rest keys auxen whole)
                       (nx-parse-structured-lambda-list pending l no-acode t)
    (list (%nx1-operator lambda-list) whole req opt rest keys auxen)))

(defun nx1-form (form &optional (*nx-lexical-environment* *nx-lexical-environment*))
  (let* ((*nx-form-type* t))
    (when (and (consp form)(eq (car form) 'the))
      (setq *nx-form-type* (nx-target-type (cadr form))))
    (prog1
      (nx1-typed-form form *nx-lexical-environment*))))

(defun nx1-typed-form (original env)
  (with-program-error-handler
      (lambda (c)
        (nx1-transformed-form (nx-transform (runtime-program-error-form c) env) env))
    (nx1-transformed-form (nx-transform original env) env)))

(defun nx1-transformed-form (form &optional (env *nx-lexical-environment*))
  (if (consp form)
    (nx1-combination form env)
    (let* ((symbolp (non-nil-symbol-p form))
           (constant-value (unless symbolp form))
           (constant-symbol-p nil))
      (if symbolp 
        (multiple-value-setq (constant-value constant-symbol-p) 
          (nx-transform-defined-constant form env)))
      (if (and symbolp (not constant-symbol-p))
        (nx1-symbol form env)
        (nx1-immediate (nx-unquote constant-value))))))



(defun nx1-prefer-areg (form env)
  (nx1-form form env))

(defun nx1-target-fixnump (form)
  (when (typep form 'integer)
       (let* ((target (backend-target-arch *target-backend*)))
         (and
          (>= form (arch::target-most-negative-fixnum target))
          (<= form (arch::target-most-positive-fixnum target))))))


(defun nx1-immediate (form)
  (if (or (eq form t) (null form))
    (nx1-sysnode form)
    (make-acode 
     (if (nx1-target-fixnump form) 
       (%nx1-operator fixnum)
        (%nx1-operator immediate))   ; Screw: chars
     form)))

(defun nx-constant-form-p (form)
  (setq form (nx-untyped-form form))
  (if form
    (or (nx-null form)
        (nx-t form)
        (and (consp form)
             (or (eq (acode-operator form) (%nx1-operator immediate))
                 (eq (acode-operator form) (%nx1-operator fixnum))
                 (eq (acode-operator form) (%nx1-operator simple-function)))))))

(defun nx-natural-constant-p (form)
  (setq form (nx-untyped-form form))
  (if (consp form)
    (let* ((val (if (or (eq (acode-operator form) (%nx1-operator fixnum))
			(eq (acode-operator form) (%nx1-operator immediate)))
		  (cadr form))))
      (target-word-size-case
       (32 (and (typep val '(unsigned-byte 32)) val))
       (64 (and (typep val '(unsigned-byte 64)) val))))))

(defun nx-u32-constant-p (form)
  (setq form (nx-untyped-form form))
  (if (consp form)
    (let* ((val (if (or (eq (acode-operator form) (%nx1-operator fixnum))
			(eq (acode-operator form) (%nx1-operator immediate)))
		  (cadr form))))
      (and (typep val '(unsigned-byte 32)) val))))

(defun nx-u31-constant-p (form)
  (setq form (nx-untyped-form form))
  (if (consp form)
    (let* ((val (if (or (eq (acode-operator form) (%nx1-operator fixnum))
			(eq (acode-operator form) (%nx1-operator immediate)))
		  (cadr form))))
      (and (typep val '(unsigned-byte 31)) val))))


;;; Reference-count vcell, fcell refs.
(defun nx1-note-vcell-ref (sym)
  (let* ((there (assq sym *nx1-vcells*))
         (count (expt 4 *nx-loop-nesting-level*)))
    (if there
      (%rplacd there (%i+ (%cdr there) count))
      (push (cons sym count) *nx1-vcells*)))
  sym)

(defun nx1-note-fcell-ref (sym)
  (let* ((there (assq sym *nx1-fcells*))
         (count (expt 4 *nx-loop-nesting-level*)))
    (if there
      (%rplacd there (%i+ (%cdr there) count))
      (push (cons sym count) *nx1-fcells*))
    sym))

; Note that "simple lexical refs" may not be; that's the whole problem ...
(defun nx1-symbol (form &optional (env *nx-lexical-environment*))
  (let* ((type (nx-declared-type form))
         (form
          (multiple-value-bind (info inherited-p more)
                               (nx-lex-info form)
            (if (and info (neq info :special))
              (if (eq info :symbol-macro)
                (progn
                  (nx-set-var-bits more (%ilogior (%ilsl $vbitreffed 1) (nx-var-bits more)))
                  (if (eq type t)
                    (nx1-form inherited-p)
                    (nx1-form `(the ,(prog1 type (setq type t)) ,inherited-p))))
                (progn
                  (when (not inherited-p)
                    (nx-set-var-bits info (%ilogior2 (%ilsl $vbitreffed 1) (nx-var-bits info)))
                    (nx-adjust-ref-count info))
                  (make-acode (%nx1-operator lexical-reference) info)))
              (make-acode
	       (if (nx1-check-special-ref form info)
		   (progn
		     (nx-record-xref-info :references form)
		     (if (nx-global-p form env)
			 (%nx1-operator global-ref)
		         (if (and (not (nx-force-boundp-checks form env))
				  (or (nx-proclaimed-parameter-p form)
				  (assq form *nx-compile-time-types*)
				  (assq form *nx-proclaimed-types*)
				  (nx-open-code-in-line env)))
			     (%nx1-operator bound-special-ref)
			     (%nx1-operator special-ref))))
		   (%nx1-operator free-reference))
               (nx1-note-vcell-ref form))))))
    (if (eq type t)
	form
      (make-acode (%nx1-operator typed-form) type form))))

(defun nx1-check-special-ref (form auxinfo)
  (or (eq auxinfo :special) 
      (nx-proclaimed-special-p form)
      (let ((defenv (definition-environment *nx-lexical-environment*)))
        (unless (and defenv (eq (car (defenv.type defenv)) :execute) (boundp form))
          (nx1-whine :special form))
        nil)))



(defun nx1-whine (about &rest forms)
    (push (make-condition (or (cdr (assq about *compiler-whining-conditions*)) 'compiler-warning)
                          :function-name (list *nx-cur-func-name*)
                          :warning-type about
                          :args (or forms (list nil)))
          *nx-warnings*)
  nil)

(defun p2-whine (afunc about &rest forms)
  (let* ((warning (make-condition (or (cdr (assq about *compiler-whining-conditions*)) 'compiler-warning)
                                  :function-name (list (afunc-name afunc))
                                  :warning-type about
                                  :args (or forms (list nil)))))
    (push warning (afunc-warnings afunc))
    (do* ((p (afunc-parent afunc) (afunc-parent p)))
         ((null p) warning)
      (let* ((pname (afunc-name p)))
        (push pname (compiler-warning-function-name warning))
        (push warning (afunc-warnings p))))))

(defun nx1-type-intersect (form type1 type2 &optional env)
  (declare (ignore env)) ; use it when deftype records info in env.  Fix this then ...
  (let* ((ctype1 (if (typep type1 'ctype) type1 (specifier-type type1)))
         (ctype2 (if (typep type2 'ctype) type2 (specifier-type type2)))
         (intersection (type-intersection ctype1 ctype2)))
    (if (eq intersection *empty-type*)
      (let ((type1 (if (typep type1 'ctype)
                     (type-specifier type1)
                     type1))
            (type2 (if (typep type2 'ctype)
                     (type-specifier type2)
                     type2)))
        (nx1-whine :type-conflict form type1 type2)))
    (type-specifier intersection)))
                 


(defun nx-declared-notinline-p (sym env)
  (setq sym (maybe-setf-function-name sym))
  (loop
    (when (listp env)
      (return (and (symbolp sym)
                   (proclaimed-notinline-p sym))))
    (dolist (decl (lexenv.fdecls env))
      (when (and (eq (car decl) sym)
                 (eq (cadr decl) 'inline))
         (return-from nx-declared-notinline-p (eq (cddr decl) 'notinline))))
    (setq env (lexenv.parent-env env))))



(defun nx1-combination (form env)
  (destructuring-bind (sym &rest args)
                      form
    (if (symbolp sym)
      (let* ((*nx-sfname* sym) special)
        (if (and (setq special (gethash sym *nx1-alphatizers*))
                 (or (not (functionp (fboundp sym)))
                     (memq sym '(apply funcall ;; see bug #285
                                 %defun        ;; see bug #295
                                 ))
                     (< (safety-optimize-quantity env) 3))
                 ;(not (nx-lexical-finfo sym env))
                 (not (nx-declared-notinline-p sym *nx-lexical-environment*)))
          (funcall special form env) ; pass environment arg ...
          (progn            
            (nx1-typed-call sym args))))
      (if (lambda-expression-p sym)
        (nx1-lambda-bind (%cadr sym) args (%cddr sym))
      (nx-error "~S is not a symbol or lambda expression in the form ~S ." sym form)))))

(defun nx1-treat-as-call (args)
  (nx1-typed-call (car args) (%cdr args)))

(defun nx1-typed-call (sym args)
  (multiple-value-bind (type errors-p) (nx1-call-result-type sym args)
    (let ((form (nx1-call sym args nil nil errors-p)))
      (if (eq type t)
	form
	(list (%nx1-operator typed-form) type form)))))

;;; Wimpy.
(defun nx1-call-result-type (sym &optional (args nil args-p) spread-p)
  (let* ((env *nx-lexical-environment*)
         (global-def nil)
         (lexenv-def nil)
         (defenv-def nil)
         (somedef nil)
	 (whined nil))
    (when (and sym 
               (symbolp sym)
               (not (find-ftype-decl sym env))
               (not (setq lexenv-def (nth-value 1 (nx-lexical-finfo sym))))
               (null (setq defenv-def (retrieve-environment-function-info sym env)))
               (neq sym *nx-global-function-name*)
               (not (functionp (setq global-def (fboundp sym)))))
      (if args-p
        (nx1-whine :undefined-function sym args spread-p)
        (nx1-whine :undefined-function sym))
      (setq whined t))
    (when (and args-p (setq somedef (or lexenv-def defenv-def global-def)))
      (multiple-value-bind (deftype  reason)
          (nx1-check-call-args somedef args spread-p)
        (when deftype
          (nx1-whine deftype sym reason args spread-p)
	  (setq whined t))))
    (values (nx-target-type *nx-form-type*) whined)))

(defun find-ftype-decl (sym env)
  (setq sym (maybe-setf-function-name sym))
  (loop 
    (when (listp env)
      (return (and (symbolp sym)
                   (proclaimed-ftype sym))))
    (dolist (fdecl (lexenv.fdecls env))
      (declare (list fdecl))
      (when (and (eq (car fdecl) sym)
                 (eq (car (the list (cdr fdecl))) 'ftype))
        (return-from find-ftype-decl (cdr (the list (cdr fdecl))))))
    (setq env (lexenv.parent-env env))))

(defun innermost-lfun-bits-keyvect (def)
  (declare (notinline innermost-lfun-bits-keyvect))
  (let* ((inner-def (closure-function (find-unencapsulated-definition def)))
         (bits (lfun-bits inner-def))
         (keys (lfun-keyvect inner-def)))
    (declare (fixnum bits))
    (when (and (eq (ash 1 $lfbits-gfn-bit)
                   (logand bits (logior (ash 1 $lfbits-gfn-bit)
                                        (ash 1 $lfbits-method-bit))))
               (logbitp $lfbits-keys-bit bits))
      (setq bits (logior (ash 1 $lfbits-aok-bit) bits)
            keys nil))
    (values bits keys)))


(defun nx1-check-call-args (def arglist spread-p)
  (let* ((deftype (if (functionp def) 
                    :global-mismatch
                    (if (istruct-typep def 'afunc)
                      :lexical-mismatch
                      :environment-mismatch)))
         (reason nil))
    (multiple-value-bind (bits keyvect)
                         (case deftype
                           (:global-mismatch (innermost-lfun-bits-keyvect def))
                           (:environment-mismatch (values (caadr def) (cdadr def)))
                           (t (let* ((lambda-form (afunc-lambdaform def)))
                                (if (lambda-expression-p lambda-form)
                                  (encode-lambda-list (cadr lambda-form))))))
      (when bits
        (unless (typep bits 'fixnum) (bug "Bad bits!"))
        (let* ((nargs (length arglist))
               (minargs (if spread-p (1- nargs) nargs))
               (maxargs (if spread-p nil nargs))
               (required (ldb $lfbits-numreq bits))
               (max (if (logtest (logior (ash 1 $lfbits-rest-bit) (ash 1 $lfbits-restv-bit) (ash 1 $lfbits-keys-bit)) bits)
                      nil
                      (+ required (ldb $lfbits-numopt bits)))))
          ;; If the (apparent) number of args in the call doesn't
          ;; match the definition, complain.  If "spread-p" is true,
          ;; we can only be sure of the case when more than the
          ;; required number of args have been supplied.
          (if (or (if (and (not spread-p) (< minargs required))
                    (setq reason `(:toofew ,minargs ,required)))
                  (if (and max (or (> minargs max)) (if maxargs (> maxargs max)))
                    (setq reason (list :toomany (if (> minargs max) minargs maxargs) max)))
                  (setq reason (nx1-find-bogus-keywords arglist spread-p bits keyvect)))
            (values deftype reason)))))))

(defun nx1-find-bogus-keywords (args spread-p bits keyvect)
  (declare (fixnum bits))
  (when (logbitp $lfbits-aok-bit bits)
    (setq keyvect nil))                 ; only check for even length tail
  (when (and (logbitp $lfbits-keys-bit bits) 
             (not spread-p))     ; Can't be sure, last argform may contain :allow-other-keys
    (do* ((key-values (nthcdr (+ (ldb $lfbits-numreq bits)  (ldb $lfbits-numopt bits)) args))
          (key-args key-values  (cddr key-args)))
         ((null key-args))
      (if (null (cdr key-args))
        (return (list :odd-keywords key-values))
        (when keyvect
          (let* ((keyword (%car key-args)))
            (unless (constantp keyword)
              (return nil))
            (unless (eq keyword :allow-other-keys)
              (unless (position (nx-unquote keyword) keyvect)                
                (return (list :unknown-keyword
                              (nx-unquote keyword)
                              (coerce keyvect 'list)))))))))))

;;; we can save some space by going through subprims to call "builtin"
;;; functions for us.
(defun nx1-builtin-function-offset (name)
   (arch::builtin-function-name-offset name))

(defun nx1-call-form (global-name afunc arglist spread-p  &optional (env *nx-lexical-environment*))
  (if afunc
    (make-acode (%nx1-operator lexical-function-call) afunc (nx1-arglist arglist (if spread-p 1 (backend-num-arg-regs *target-backend*))) spread-p)
    (let* ((builtin (unless (or spread-p
                                (eql 3 (safety-optimize-quantity env)))
                      (nx1-builtin-function-offset global-name))))
      (if (and builtin
               (let* ((bits (lfun-bits (fboundp global-name))))
                 (and bits (eql (logand $lfbits-args-mask bits)
                                (dpb (length arglist)
                                     $lfbits-numreq
                                     0)))))
        (make-acode (%nx1-operator builtin-call) 
                    (make-acode (%nx1-operator fixnum) builtin)
                    (nx1-arglist arglist))
        (make-acode (%nx1-operator call)
                     (if (symbolp global-name)
                       (nx1-immediate (nx1-note-fcell-ref global-name))
                       global-name)
                     (nx1-arglist arglist (if spread-p 1 (backend-num-arg-regs *target-backend*)))
                     spread-p)))))
  
;;; If "sym" is an expression (not a symbol which names a function),
;;; the caller has already alphatized it.
(defun nx1-call (sym args &optional spread-p global-only inhibit-inline)
  (nx1-verify-length args 0 nil)
  (let ((args-in-regs (if spread-p 1 (backend-num-arg-regs *target-backend*))))
    (if (nx-self-call-p sym global-only)
      ;; Should check for downward functions here as well.
      (multiple-value-bind (deftype reason)
                           (nx1-check-call-args *nx-current-function* args spread-p)
        (when deftype
          (nx1-whine deftype sym reason args spread-p))
        (make-acode (%nx1-operator self-call) (nx1-arglist args args-in-regs) spread-p))
      (multiple-value-bind (lambda-form containing-env token) (nx-inline-expansion sym *nx-lexical-environment* global-only)
        (or (and (not inhibit-inline)
		 (nx1-expand-inline-call lambda-form containing-env token args spread-p *nx-lexical-environment*))
            (multiple-value-bind (info afunc) (if (and  (symbolp sym) (not global-only)) (nx-lexical-finfo sym))
              (when (eq 'macro (car info))
                (nx-error "Can't call macro function ~s" sym))
	      (nx-record-xref-info :direct-calls sym)
              (if (and afunc (%ilogbitp $fbitruntimedef (afunc-bits afunc)))
                (let ((sym (var-name (afunc-lfun afunc))))
                  (nx1-form 
                   (if spread-p
                     `(,(if (eql spread-p 0) 'applyv 'apply) ,sym ,args)
                     `(funcall ,sym ,@args))))
                (let* ((val (nx1-call-form sym afunc args spread-p)))
                    (when afunc
                      (let ((callers (afunc-callers afunc))
                            (self *nx-current-function*))
                        (unless (or (eq self afunc) (memq self callers))
                          (setf (afunc-callers afunc) (cons self callers)))))
                    (if (and (null afunc) (memq sym *nx-never-tail-call*))
                      (make-acode (%nx1-operator values) (list val))
                      val)))))))))

(defun nx1-expand-inline-call (lambda-form env token args spread-p old-env)
  (if (and (or (null spread-p) (eq (length args) 1)))
    (if (and token (not (memq token *nx-inline-expansions*)))
      (with-program-error-handler (lambda (c) (declare (ignore c)) nil)
	(let* ((*nx-inline-expansions* (cons token *nx-inline-expansions*))
	       (lambda-list (cadr lambda-form))
	       (body (cddr lambda-form))
	       (new-env (new-lexical-environment env)))
	  (setf (lexenv.mdecls new-env)
                `((speed . ,(speed-optimize-quantity old-env))
		  (space . ,(space-optimize-quantity old-env))
		  (safety . ,(space-optimize-quantity old-env))
		  (compilation-speed . ,(compilation-speed-optimize-quantity old-env))
		  (debug . ,(debug-optimize-quantity old-env))))
	  (if spread-p
	    (nx1-destructure lambda-list (car args) nil nil body new-env)
	    (nx1-lambda-bind lambda-list args body new-env)))))))
             
; note that regforms are reversed: arg_z is always in the car
(defun nx1-arglist (args &optional (nregargs (backend-num-arg-regs *target-backend*)))
  (declare (fixnum nregargs))
  (let* ((stkforms nil)
         (regforms nil)
         (nstkargs (%i- (length args) nregargs)))
    (declare (fixnum nstkargs))
      (list
       (dotimes (i nstkargs (nreverse stkforms))
         (declare (fixnum i))
         (push (nx1-form (%car args)) stkforms)
         (setq args (%cdr args)))
       (dolist (arg args regforms)
         (push (nx1-form arg) regforms)))))

(defun nx1-formlist (args)
  (let* ((a nil))
    (dolist (arg args)
      (push (nx1-form arg) a))
    (nreverse a)))

(defun nx1-verify-length (forms min max &aux (len (list-length forms)))
 (if (or (null len)
         (%i> min len)
         (and max (%i> len max)))
     (nx-error "Wrong number of args in form ~S." (cons *nx-sfname* forms))
     len))

(defun nx-unquote (form)
  (if (nx-quoted-form-p form)
    (%cadr form)
    form))

(defun nx-quoted-form-p (form &aux (f form))
 (and (consp form)
      (eq (pop form) 'quote)
      (or
       (and (consp form)
            (not (%cdr form)))
       (nx-error "Illegally quoted form ~S." f))))

; Returns two values: expansion & win
; win is true if expansion is not EQ to form.
; This is a bootstrapping version.
; The real one is in "ccl:compiler;optimizers.lisp".
(unless (fboundp 'maybe-optimize-slot-accessor-form)

(defun maybe-optimize-slot-accessor-form (form environment)
  (declare (ignore environment))
  (values form nil))

)

(defun nx-transform (form &optional (environment *nx-lexical-environment*))
  (let* (sym transforms lexdefs changed enabled macro-function compiler-macro)
    (tagbody
       (go START)
     LOOP
       (setq changed t)
       (when (and (consp form) 
		  (or (eq (%car form) 'the)
		      (and sym (eq (%car form) sym))))
	 (go DONE))
     START
       (when (non-nil-symbol-p form)
	 (multiple-value-bind (newform win) (nx-transform-symbol form environment)
	   (unless win (go DONE))
	   (setq form newform changed (or changed win))
	   (go LOOP)))
       (when (atom form) (go DONE))
       (unless (symbolp (setq sym (%car form)))
	 (go DONE))
       (when (eq sym 'the)
	 (destructuring-bind (typespec thing) (cdr form)
           (if (constantp thing)
             (progn
               (setq form thing form thing)
               (go LOOP))
             (multiple-value-bind (newform win) (nx-transform thing environment)
               (when win
                 (setq changed t)
                 (if (and (self-evaluating-p newform)
                          (typep newform typespec))
                   (setq form newform)
                   (setq form `(the ,typespec ,newform)))
                 (go DONE))))))
       (when (nx-quoted-form-p form)
	 (when (self-evaluating-p (%cadr form))
	   (setq form (%cadr form)))
	 (go DONE))
       (when (setq lexdefs (nx-lexical-finfo sym environment))
	 (if (eq 'function (%car lexdefs))
	   (go DONE)))
       (setq transforms (setq compiler-macro (compiler-macro-function sym environment))
	     macro-function (macro-function sym environment)
	     enabled (nx-allow-transforms environment))
       (unless macro-function
	 (let* ((win nil))
	   (when (and enabled (functionp (fboundp sym)))
	     (multiple-value-setq (form win) (nx-transform-arglist form environment))
	     (if win (setq changed t)))))
       (when (and enabled
		  (not (nx-declared-notinline-p sym environment)))
	 (multiple-value-bind (value folded) (nx-constant-fold form environment)
	   (when folded (setq form value changed t)  (unless (and (consp form) (eq (car form) sym)) (go START))))
	 (when compiler-macro
	   (multiple-value-bind (newform win) (compiler-macroexpand-1 form environment)
	     (when win
	       (when (and (consp newform) (eq (car newform) sym) (functionp (fboundp sym)))
		 (setq sym nil))
	       (setq form newform)
	       (go LOOP))))
	 (multiple-value-bind (newform win) (maybe-optimize-slot-accessor-form form environment)
	   (when win
	     (setq sym nil)
	     (setq form newform)
	     (go START)))
	 (unless macro-function
	   (when (setq transforms (or (environment-structref-info sym environment)
				      (and #-bccl (boundp '%structure-refs%)
					   (gethash sym %structure-refs%))))
	     (setq form (defstruct-ref-transform transforms (%cdr form)) changed T)
	     (go START))
	   (when (setq transforms (assq sym *nx-synonyms*))
	     (setq form (cons (%cdr transforms) (setq sym (%cdr form))))
	     (go LOOP))))
       (when (and macro-function
		  (or lexdefs
		      (not (and (gethash sym *nx1-alphatizers*) (not (nx-declared-notinline-p sym environment))))))
	 (nx-record-xref-info :macro-calls (function-name macro-function))
	 (setq form (macroexpand-1 form environment) changed t)
	 (go START))
     DONE)
    (values form changed)))

; Transform all of the arguments to the function call form.
; If any of them won, return a new call form (with the same operator as the original), else return the original
; call form unchanged.

(defun nx-transform-arglist (callform env)
    (let* ((any-wins nil)
           (transformed-call (cons (car callform) nil))
           (ptr transformed-call)
           (win nil))
      (declare (type cons ptr))
      (dolist (form (cdr callform) (if any-wins (values (copy-list transformed-call) t) (values callform nil)))
        (rplacd ptr (setq ptr (cons (multiple-value-setq (form win) (nx-transform form env)) nil)))
        (if win (setq any-wins t)))))

;This is needed by (at least) SETF.
(defun nxenv-local-function-p (name macro-env)
  (multiple-value-bind (type local-p) (function-information name macro-env)
    (and local-p (eq :function type))))

           
;;; This guy has to return multiple values.  The arguments have
;;; already been transformed; if they're all constant (or quoted), try
;;; to evaluate the expression at compile-time.
(defun nx-constant-fold (original-call &optional (environment *nx-lexical-environment*) &aux 
                                       (fn (car original-call)) form mv foldable foldfn)
  (flet ((quotify (x) (if (self-evaluating-p x) x (list 'quote x))))
    (if (and (nx-allow-transforms environment)
             (let* ((bits (if (symbolp fn) (%symbol-bits fn) 0)))
               (declare (fixnum bits))
               (if (setq foldable (logbitp $sym_fbit_constant_fold bits))
                 (if (logbitp $sym_fbit_fold_subforms bits)
                   (setq foldfn 'fold-constant-subforms))
                 (setq foldable (assq fn *nx-can-constant-fold*)
                       foldfn (cdr foldable)))
               foldable))
      (if foldfn
        (funcall foldfn original-call environment)
        (progn
          (let ((args nil))
            (dolist (arg (cdr original-call) (setq args (nreverse args)))
              (if (quoted-form-p arg)
                (setq arg (%cadr arg))
                (unless (self-evaluating-p arg) (return-from nx-constant-fold (values original-call nil))))
              (push arg args))
            (if (nx1-check-call-args (fboundp fn) args nil)
              (return-from nx-constant-fold (values original-call nil))
              (setq form (multiple-value-list 
                             (handler-case (apply fn args)
                               (error (condition)
                                      (warn "Error: \"~A\" ~&signalled during compile-time evaluation of ~S ."
                                            condition original-call)
                                      (return-from nx-constant-fold
                                        (values `(locally (declare (notinline ,fn))
                                                  ,original-call)
                                                t))))))))
          (if form
            (if (null (%cdr form))
              (setq form (%car form))
              (setq mv (setq form (cons 'values (mapcar #'quotify form))))))
          (values (if mv form (quotify form)) T)))
      (values original-call nil))))

(defun nx-transform-symbol (sym &optional (env *nx-lexical-environment*))
; Gak.  Can't call NX-LEX-INFO without establishing *nx-lexical-environment*.
; NX-LEX-INFO should take env arg!.
  (let* ((*nx-lexical-environment* env))
    (multiple-value-bind (expansion win) (macroexpand-1 sym env)
      (if win
        (let ((type (nx-declared-type sym))
              (var (nth-value 2 (nx-lex-info sym))))
          (unless (eq t type) (setq expansion `(the ,type ,expansion)))
          (if var (nx-set-var-bits var (%ilogior (%ilsl $vbitreffed 1) (nx-var-bits var)))))
        (progn
          (multiple-value-setq (expansion win)
            (nx-transform-defined-constant sym env))
          (if win (setq win (neq sym expansion)))))
      (values expansion win))))

; if sym has a substitutable constant value in env (or globally), return
; (values <value> t), else (values nil nil)
(defun nx-transform-defined-constant (sym env)
  (let* ((defenv (definition-environment env))
         (val (if defenv (assq sym (defenv.constants defenv))))
         (constant-value-p val))
    (if val
      (setq val (%cdr val))
      (if (constant-symbol-p sym)
        (setq constant-value-p t val (%sym-global-value sym))))
    (if (and (neq val (%unbound-marker-8))
             constant-value-p 
             (nx-substititute-constant-value sym val env))
      (values (if (self-evaluating-p val) val (list 'quote val)) t)
      (values nil nil))))


(defun nx-var-bits (var)
  (do* ((var var bits)
        (bits (var-bits var) (var-bits var)))
       ((fixnump bits) bits)))

(defun nx-set-var-bits (var newbits)
  (do* ((var var bits)
        (bits (var-bits var) (var-bits var)))
       ((fixnump bits) (setf (var-bits var) newbits))))

(defun nx-adjust-ref-count (var)
  (let* ((bits (nx-var-bits var))
         (temp-p (%ilogbitp $vbittemporary bits))
         (by (if temp-p 1 (expt  4 *nx-loop-nesting-level*)))
         (new (%imin (%i+ (%ilogand2 $vrefmask bits) by) 255)))
    (nx-set-var-bits var (%ilogior (%ilogand (%ilognot $vrefmask) bits) new))
    new))

;;; Treat (VALUES x . y) as X if it appears in a THE form
(defun nx-form-type (form &optional (env *nx-lexical-environment*))
  (if (quoted-form-p form)
    (type-of (nx-unquote form))
    (if (self-evaluating-p form)
      (type-of form)
      (if (and (consp form)             ; Kinda bogus now, but require-type
               (eq (%car form) 'require-type) ; should be special some day
               (quoted-form-p (caddr form)))
        (%cadr (%caddr form))
        (if (nx-trust-declarations env)
          (if (symbolp form)
            (nx-target-type (nx-declared-type form env))
            (if (consp form)
              (if (eq (%car form) 'the)
                (destructuring-bind (typespec val) (%cdr form)
                  (declare (ignore val))
                  (let* ((ctype (values-specifier-type typespec)))
                    (if (typep ctype 'values-ctype)
                      (let* ((req (values-ctype-required ctype)))
                        (if req
                          (nx-target-type (type-specifier (car req)))
                          '*))
                      (nx-target-type (type-specifier ctype)))))
                (if (eq (%car form) 'setq)
                  (nx-declared-type (cadr form) env)
                  (let* ((op (gethash (%car form) *nx1-operators*)))
                    (or (and op (cdr (assq op *nx-operator-result-types*)))
                        (and (not op)(cdr (assq (car form) *nx-operator-result-types-by-name*)))
                        (and (memq (car form) *numeric-ops*)
                             (grovel-numeric-form form env))
                        (and (memq (car form) *logical-ops*)
                             (grovel-logical-form form env))
                        ;; Sort of the right idea, but this should be done
                        ;; in a more general way.
                        (when (or (eq (car form) 'aref)
                                  (eq (car form) 'uvref))
                          (let* ((atype (nx-form-type (cadr form) env))
                                 (a-ctype (specifier-type atype)))
                            (when (array-ctype-p a-ctype)
                              (type-specifier (array-ctype-specialized-element-type
                                               a-ctype)))))
                        t))))
              t))
          t)))))

(defparameter *numeric-ops* '(+ -  / * +-2 --2 *-2 /-2))

(defparameter *logical-ops* '(logxor-2 logior-2 logand-2  lognot logxor))

(defun numeric-type-p (type &optional not-complex)
  (or (memq type '(fixnum integer double-float single-float float))
      (let ((ctype (specifier-type type)))
        (and (numeric-ctype-p ctype)
             (or (not not-complex)
                 (neq (numeric-ctype-complexp ctype) :complex))))))

(defun grovel-numeric-form (form env)
  (let* ((op (car form))
         (args (cdr form)))
    (if (every #'(lambda (x) (nx-form-typep x 'float env)) args)
      (if (some #'(lambda (x) (nx-form-typep x 'double-float env)) args)
        'double-float
        'single-float)
      (if (every #'(lambda (x) (nx-form-typep x 'integer env)) args)
        (if (or (eq op '/) (eq op '/-2))
          t
          'integer)))))

;; now e.g. logxor of 3 known fixnums is inline as is (logior a (logxor b c))
;; and (the fixnum (+ a (logxor b c)))

(defun grovel-logical-form (form env)
  (when (nx-trust-declarations env)
    (let (;(op (car form))
          type)
      (dolist (arg (cdr form))
        (let ((it (nx-form-type arg env)))          
          (if (not (subtypep it 'fixnum))
            (return (setq type nil))
            (setq type 'fixnum))))
      type)))

(defun nx-form-typep (arg type &optional (env *nx-lexical-environment*))
  (setq type (nx-target-type (type-expand type)))
  (if (constantp arg)
    (typep (nx-unquote arg) type env)
    (subtypep (nx-form-type arg env) type env)))


(defun nx-binary-fixnum-op-p (form1 form2 env &optional ignore-result-type)
  (setq form1 (nx-transform form1 env)
        form2 (nx-transform form2 env))
  (and
   (target-word-size-case
    (32 (nx-form-typep form1 '(signed-byte 30) env))
    (64 (nx-form-typep form1 '(signed-byte 61) env)))
   (target-word-size-case
    (32 (nx-form-typep form2 '(signed-byte 30) env))
    (64 (nx-form-typep form2 '(signed-byte 61) env)))
   (or ignore-result-type
        (and (nx-trust-declarations env)
                (target-word-size-case
                 (32 (subtypep *nx-form-type* '(signed-byte 30)))
                 (64 (subtypep *nx-form-type* '(signed-byte 61))))))))


(defun nx-binary-natural-op-p (form1 form2 env &optional (ignore-result-type t))
  (and
   (target-word-size-case
    (32
     (and (nx-form-typep form1 '(unsigned-byte 32)  env)
          (nx-form-typep form2 '(unsigned-byte 32)  env)))
    (64
     (and (nx-form-typep form1 '(unsigned-byte 64)  env)
          (nx-form-typep form2 '(unsigned-byte 64)  env))))
   (or ignore-result-type
       (and (nx-trust-declarations env)
            (target-word-size-case
             (32 (subtypep *nx-form-type* '(unsigned-byte 32)))
             (64 (subtypep *nx-form-type* '(unsigned-byte 64))))))))

    


(defun nx-binary-boole-op (whole env arg-1 arg-2 fixop intop naturalop)
  (let* ((use-fixop (nx-binary-fixnum-op-p arg-1 arg-2 env t))
	 (use-naturalop (nx-binary-natural-op-p arg-1 arg-2 env)))
    (if (or use-fixop use-naturalop intop)
      (make-acode (if use-fixop fixop (if use-naturalop naturalop intop))
		  (nx1-form arg-1)
		  (nx1-form arg-2))
      (nx1-treat-as-call whole))))

(defun nx-global-p (sym &optional (env *nx-lexical-environment*))
  (or 
   (logbitp $sym_vbit_global (the fixnum (%symbol-bits sym)))
   (let* ((defenv (definition-environment env)))
     (if defenv 
       (eq :global (%cdr (assq sym (defenv.specials defenv))))))))
  
(defun nx-need-var (sym &optional (check-bindable t))
  (if (and (nx-need-sym sym)
           (not (constantp sym))
           (let* ((defenv (definition-environment *nx-lexical-environment*)))
             (or (null defenv)
                 (not (assq sym (defenv.constants defenv)))))) ; check compile-time-constants, too
    (if (and check-bindable (nx-global-p sym))
      (nx-error "~S is declared static and can not be bound" sym)
      sym)
    (nx-error "Can't bind or assign to constant ~S." sym)))

(defun nx-need-sym (sym)
  (if (symbolp sym)
    sym
    (nx-error "~S is not a symbol." sym)))

(defun nx-need-function-name (name)
  (multiple-value-bind (valid nm) (valid-function-name-p name)
    (if valid nm (nx-error "Invalid function name ~S" name))))

(defun nx-pair-name (form)
  (nx-need-sym (if (consp form) (%car form) form)))

(defun nx-pair-initform (form)
  (if (atom form)
    nil
    (if (and (listp (%cdr form)) (null (%cddr form)))
      (%cadr form)
      (nx-error "Bad initialization form: ~S." form))))

; some callers might assume that this guy errors out if it can't conjure up
; a fixnum.  I certainly did ...
(defun nx-get-fixnum (form &aux (trans (nx-transform form *nx-lexical-environment*)))
 (if (fixnump trans)
  trans
  form))
 
(defun nx1-func-name (gizmo)
  (and (consp gizmo)
       (or (eq (%car gizmo) 'function) (eq (%car gizmo) 'quote))
       (consp (%cdr gizmo))
       (null (%cddr gizmo))
       (nth-value 1 (valid-function-name-p (%cadr gizmo)))))

; distinguish between program errors & incidental ones.
(defun nx-error (format-string &rest args)
  (error (make-condition 'compile-time-program-error 
                :context (nx-error-context)
                :format-control format-string
                :format-arguments args)))

(defun nx-compile-time-error (format-string &rest args)
  (error (make-condition 'compile-time-program-error 
                :context (nx-error-context)
                :format-control format-string
                :format-arguments args)))

; Should return information about file being compiled, nested functions, etc. ...
(defun nx-error-context ()
  (or *nx-cur-func-name* "an anonymous function"))

(defparameter *warn-if-function-result-ignored*
  '(sort stable-sort delete delete-if delete-if-not remf nreverse
    nunion nset-intersection)
  "Names of functions whos result(s) should ordinarily be used, because of their side-effects or lack of them.")
