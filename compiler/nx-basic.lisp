;;;-*- Mode: Lisp; Package: CCL -*-
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


;; used by compiler and eval - stuff here is not excised with rest of compiler


(in-package :ccl)

#|| Note: when MCL-AppGen 4.0 is built, the following form will need to be included in it:
; for compiler-special-form-p, called by cheap-eval-in-environment
(defparameter *nx1-compiler-special-forms*
  `(%DEFUN %FUNCTION %NEW-PTR %NEWGOTAG %PRIMITIVE %VREFLET BLOCK CATCH COMPILER-LET DEBIND
    DECLARE EVAL-WHEN FBIND FLET FUNCTION GO IF LABELS LAP LAP-INLINE LET LET* LOAD-TIME-VALUE
    LOCALLY MACRO-BIND MACROLET MAKE-LIST MULTIPLE-VALUE-BIND MULTIPLE-VALUE-CALL
    MULTIPLE-VALUE-LIST MULTIPLE-VALUE-PROG1 NEW-LAP NEW-LAP-INLINE NFUNCTION OLD-LAP
    OLD-LAP-INLINE OR PROG1 PROGN PROGV QUOTE RETURN-FROM SETQ STRUCT-REF STRUCT-SET
    SYMBOL-MACROLET TAGBODY THE THROW UNWIND-PROTECT WITH-STACK-DOUBLE-FLOATS WITHOUT-INTERRUPTS))
||#

(eval-when (:compile-toplevel)
  (require 'nxenv))

(defvar *lisp-compiler-version* 666 "I lost count.")

(defvar *nx-compile-time-types* nil)
(defvar *nx-proclaimed-types* nil)
(defvar *nx-method-warning-name* nil)

(defvar *nx1-alphatizers* (make-hash-table :size 180 :test #'eq))

(let ((policy (%istruct 'compiler-policy
               #'(lambda (env)
                   #+ccl-0711 (< (debug-optimize-quantity env) 2)
                   #-ccl-0711 (neq (debug-optimize-quantity env) 3))   ;  allow-tail-recursion-elimination
               #'(lambda (env)
                   (declare (ignorable env))
                   #+ccl-0711 nil
                   #-ccl-0711 (eq (debug-optimize-quantity env) 3))   ; inhibit-register-allocation
               #'(lambda (env)
                   (let* ((safety (safety-optimize-quantity env)))
                     (and (< safety 3)
                          (>= (speed-optimize-quantity env)
                              safety)))) ; trust-declarations
               #'(lambda (env)
                   #+ccl-0711 (> (speed-optimize-quantity env)
                                 (space-optimize-quantity env))
                   #-ccl-0711 (>= (speed-optimize-quantity env)
                                  (+ (space-optimize-quantity env) 2))) ; open-code-inline
               #'(lambda (env)
                   (and (eq (speed-optimize-quantity env) 3) 
                        (eq (safety-optimize-quantity env) 0)))   ; inhibit-safety-checking
               #'(lambda (env)
                   (let* ((safety (safety-optimize-quantity env)))
                     (or (eq safety 3)
                         (> safety (speed-optimize-quantity env)))))          ;the-typechecks 
               #'(lambda (env)
                   (neq (debug-optimize-quantity env) 3))   ; inline-self-calls
               #'(lambda (env)
                   (and (neq (compilation-speed-optimize-quantity env) 3)
                        (neq (safety-optimize-quantity env) 3)
                        #+ccl-0711 (neq (speed-optimize-quantity env) 0)
                        (neq (debug-optimize-quantity env) 3))) ; allow-transforms
               #'(lambda (var env)       ; force-boundp-checks
                   (declare (ignore var))
                   (eq (safety-optimize-quantity env) 3))
               #'(lambda (var val env)       ; allow-constant-substitution
                   (declare (ignore var val env))
                   t)
               nil           ; extensions
               )))
  (defun new-compiler-policy (&key (allow-tail-recursion-elimination nil atr-p)
                                   (inhibit-register-allocation nil ira-p)
                                   (trust-declarations nil td-p)
                                   (open-code-inline nil oci-p)
                                   (inhibit-safety-checking nil ischeck-p)
                                   (inline-self-calls nil iscall-p)
                                   (allow-transforms nil at-p)
                                   (force-boundp-checks nil fb-p)
                                   (allow-constant-substitution nil acs-p)
                                   (the-typechecks nil tt-p))
    (let ((p (copy-uvector policy)))
      (if atr-p (setf (policy.allow-tail-recursion-elimination p) allow-tail-recursion-elimination))
      (if ira-p (setf (policy.inhibit-register-allocation p) inhibit-register-allocation))
      (if td-p (setf (policy.trust-declarations p) trust-declarations))
      (if oci-p (setf (policy.open-code-inline p) open-code-inline))
      (if ischeck-p (setf (policy.inhibit-safety-checking p) inhibit-safety-checking))
      (if iscall-p (setf (policy.inline-self-calls p) inline-self-calls))
      (if at-p (setf (policy.allow-transforms p) allow-transforms))
      (if fb-p (setf (policy.force-boundp-checks p) force-boundp-checks))
      (if acs-p (setf (policy.allow-constant-substitution p) allow-constant-substitution))
      (if tt-p (setf (policy.the-typechecks p) the-typechecks))
      p))
  (defun %default-compiler-policy () policy))

(%include "ccl:compiler;lambda-list.lisp")



;Syntactic Environment Access.

(defun declaration-information (decl-name &optional env)
  (if (and env (not (istruct-typep env 'lexical-environment)))
    (report-bad-arg env 'lexical-environment))
; *** This needs to deal with things defined with DEFINE-DECLARATION ***
  (case decl-name
    (optimize
     (list 
      (list 'speed (speed-optimize-quantity env))
      (list 'safety (safety-optimize-quantity env))
      (list 'compilation-speed (compilation-speed-optimize-quantity env))
      (list 'space (space-optimize-quantity env))
      (list 'debug (debug-optimize-quantity env))))
    (declaration
     *nx-known-declarations*)))

(defun function-information (name &optional env &aux decls)
  (let ((name (ensure-valid-function-name name)))
    (if (and env (not (istruct-typep env 'lexical-environment)))
      (report-bad-arg env 'lexical-environment))
    (if (special-operator-p name)
      (values :special-form nil nil)
      (flet ((process-new-fdecls (fdecls)
                                 (dolist (fdecl fdecls)
                                   (when (eq (car fdecl) name)
                                     (let ((decl-type (cadr fdecl)))
                                       (when (and (memq decl-type '(dynamic-extent inline ftype))
                                                  (not (assq decl-type decls)))
                                         (push (cdr fdecl) decls)))))))
        (declare (dynamic-extent #'process-new-fdecls))
        (do* ((root t)
              (contour env (when root (lexenv.parent-env contour))))
             ((null contour)
              (if (macro-function name)
                (values :macro nil nil)
                (if (fboundp name)
                  (values :function 
                          nil 
                          (if (assq 'inline decls)
			    decls
                            (if (proclaimed-inline-p name)
			      (push '(inline . inline) decls)
                                (if (proclaimed-notinline-p name)
				  (push '(inline . notinline) decls)))))
                  (values nil nil decls))))
          (if (istruct-typep contour 'definition-environment)
            (if (assq name (defenv.functions contour))
              (return (values :macro nil nil))
              (progn (setq root nil) (process-new-fdecls (defenv.fdecls contour))))
            (progn
              (process-new-fdecls (lexenv.fdecls contour))
              (let ((found (assq name (lexenv.functions contour))))
                (when found
                  (return
                   (if (and (consp (cdr found))(eq (%cadr found) 'macro))
                     (values :macro t nil)
                     (values :function t decls))))))))))))

(defun variable-information (var &optional env)
  (setq var (require-type var 'symbol))
  (if (and env (not (istruct-typep env 'lexical-environment)))
    (report-bad-arg env 'lexical-environment))
  (let* ((vartype nil)
         (boundp nil)
         (envtype nil)
         (typedecls (nx-declared-type var env)) ; should grovel nested/shadowed special decls for us.
         (decls (if (and typedecls (neq t typedecls)) (list (cons 'type typedecls)))))
    (loop
      (cond ((null env)
             (if (constant-symbol-p var)
               (setq vartype :constant decls nil)
               (if (proclaimed-special-p var)
                 (setq vartype :special)
		 (let* ((not-a-symbol-macro (cons nil nil)))
		   (declare (dynamic-extent not-a-symbol-macro))
		   (unless (eq (gethash var *symbol-macros* not-a-symbol-macro)
			       not-a-symbol-macro)
		     (setq vartype :symbol-macro)))))
             (return))
            ((eq (setq envtype (istruct-type-name env)) 'definition-environment)
             (cond ((assq var (defenv.constants env))
                    (setq vartype :constant)
                    (return))
		   ((assq var (defenv.symbol-macros env))
		    (setq vartype :symbol-macro)
		    (return))
                   ((assq var (defenv.specials env))
                    (setq vartype :special)
                    (return))))
            (t
             (dolist (vdecl (lexenv.vdecls env))
               (when (eq (car vdecl) var)
                 (let ((decltype (cadr vdecl)))
                   (unless (assq decltype decls)
                     (case decltype
                       (special (setq vartype :special))
                       ((type dynamic-extent ignore) (push (cdr vdecl) decls)))))))
             (let ((vars (lexenv.variables env)))
	       (unless (atom vars)
                 (dolist (v vars)
                   (when (eq (var-name v) var)
                     (setq boundp t)
                     (if (and (consp (var-ea v))
                              (eq :symbol-macro (car (var-ea v))))
                       (setq vartype :symbol-macro)
                       (unless vartype (setq vartype
					     (let* ((bits (var-bits v)))
					       (if (and (typep bits 'integer)
							(logbitp $vbitspecial bits))
						 :special
						 :lexical)))))
                     (return)))
		 (when vartype (return))))))
      (setq env (if (eq envtype 'lexical-environment) (lexenv.parent-env env))))
    (values vartype boundp decls)))

(defun nx-target-type (typespec)
  ;; Could do a lot more here
  (if (or (eq *host-backend* *target-backend*)
          (not (eq typespec 'fixnum)))
    typespec
    (target-word-size-case
     (32 '(signed-byte 30))
     (64 '(signed-byte 61)))))

; Type declarations affect all references.
(defun nx-declared-type (sym &optional (env *nx-lexical-environment*))
  (loop
    (when (or (null env) (istruct-typep env 'definition-environment)) (return))
    (dolist (decl (lexenv.vdecls env))
      (if (and (eq (car decl) sym)
               (eq (cadr decl) 'type))
               (return-from nx-declared-type (nx-target-type (cddr decl)))))
    (let ((vars (lexenv.variables env)))
      (when (and (consp vars) 
                 (dolist (var vars) 
                   (when (eq (var-name var) sym) 
                     (return t))))
        (return-from nx-declared-type t)))
    (setq env (lexenv.parent-env env)))
  (let ((decl (or (assq sym *nx-compile-time-types*)
                     (assq sym *nx-proclaimed-types*))))
    (if decl (%cdr decl) t)))

(defmacro define-declaration (decl-name lambda-list &body body &environment env)
  (multiple-value-bind (body decls)
                       (parse-body body env)
    (let ((fn `(nfunction (define-declaration ,decl-name)
                          (lambda ,lambda-list
                            ,@decls
                            (block ,decl-name
                              ,@body)))))
      `(progn
         (proclaim '(declaration ,decl-name))
         (setf (getf *declaration-handlers* ',decl-name) ,fn)))))

(defun check-environment-args (variable symbol-macro function macro)
  (flet ((check-all-pairs (pairlist argname)
          (dolist (pair pairlist)
            (unless (and (consp pair) (consp (%cdr pair)) (null (%cddr pair)) (symbolp (%car pair)))
              (signal-simple-program-error "Malformed ~s argument: ~s is not of the form (~S ~S) in ~S" 
                                           argname
                                           pair
                                           'name
                                           'definition
                                           pairlist))))
         (check-all-symbols (symlist argname pairs pairsname)
          (dolist (v symlist)
            (unless (symbolp v) 
              (signal-simple-program-error "Malformed ~S list: ~S is not a symbol in ~S." argname v symlist))
            (when (assq v pairs) 
              (signal-simple-program-error "~S ~S conflicts with ~S ~S" argname v pairsname (assq v pairs))))))
    (check-all-pairs symbol-macro :symbol-macro)
    (check-all-pairs macro :macro)
    (check-all-symbols variable :variable symbol-macro :symbol-macro)
    (check-all-symbols function :function macro :macro)))


;; This -isn't- PARSE-DECLARATIONS.  It can't work; neither can this ...
(defun process-declarations (env decls symbol-macros)
  (let ((vdecls nil)
        (fdecls nil)
        (mdecls nil))
    (flet ((add-type-decl (spec)
            (destructuring-bind (typespec &rest vars) spec
              (dolist (var vars)
                (when (non-nil-symbol-p var)
                  (push (list* var 
                               'type
                               (let ((already (assq 'type (nth-value 2 (variable-information var env)))))
                                 (if already
                                   (let ((oldtype (%cdr already)))
                                     (if oldtype
                                       (if (subtypep oldtype typespec)
                                         oldtype
                                         (if (subtypep typespec oldtype)
                                           typespec))))
                                   typespec)))
                        vdecls))))))
      ; do SPECIAL declarations first - this approximates the right thing, but doesn't quite make it.
      (dolist (decl decls)
        (when (eq (car decl) 'special)
          (dolist (spec (%cdr decl))
            (when (non-nil-symbol-p spec)
              (if (assq spec symbol-macros)
                (signal-program-error "Special declaration cannot be applied to symbol-macro ~S" spec))
              (push (list* spec 'special t) vdecls)))))
      (dolist (decl decls)
        (let ((decltype (car decl)))
          (case decltype
              ((inline notinline)
               (dolist (spec (%cdr decl))
               (let ((fname nil))
                 (if (non-nil-symbol-p spec)
                   (setq fname spec)
                   (if (and (consp spec) (eq (%car spec) 'setf))
                     (setq fname (setf-function-name (cadr spec)))))
                 (if fname
                   (push (list* fname decltype t) fdecls)))))
              (optimize
               (dolist (spec (%cdr decl))
                 (let ((val 3)
                       (quantity spec))
                   (if (consp spec)
                     (setq quantity (car spec) val (cadr spec)))
                 (if (and (fixnump val) (<= 0 val 3) (memq quantity '(debug speed space safety compilation-speed)))
                   (push (cons quantity val) mdecls)))))
              (dynamic-extent
               (dolist (spec (%cdr decl))
               (if (non-nil-symbol-p spec)
                 (push (list* spec decltype t) vdecls)
                 (if (and (consp spec) (eq (%car spec) 'function))
                   (let ((fname (cadr spec)))
                     (if (not (non-nil-symbol-p fname))
                       (setq fname 
                             (if (and (consp fname) (eq (%car fname) 'setf))
                               (setf-function-name (cadr fname)))))
                     (if fname (push (list* fname decltype t) fdecls)))))))
              (type (add-type-decl (cdr decl)))
              (ftype (destructuring-bind (typespec &rest fnames) (%cdr decl)
                       (dolist (name fnames)
                         (let ((fname name))
                           (if (not (non-nil-symbol-p fname))
                             (setq fname 
                                   (if (and (consp fname) (eq (%car fname) 'setf))
                                     (setf-function-name (cadr fname)))))
                           (if fname (push (list* fname decltype typespec) fdecls))))))
              (special)
              (t
               (if (memq decltype *cl-types*)
                 (add-type-decl decl)
                 (let ((handler (getf *declaration-handlers* decltype)))
                   (when handler
                     (multiple-value-bind (type info) (funcall handler decl)
                       (ecase type
                         (:variable
                          (dolist (v info) (push (apply #'list* v) vdecls)))
                         (:function
                          (dolist (f info) (push (apply #'list* f) fdecls)))
                         (:declare  ;; N.B. CLtL/2 semantics
                          (push info mdecls)))))))))))
      (setf (lexenv.vdecls env) (nconc vdecls (lexenv.vdecls env))
            (lexenv.fdecls env) (nconc fdecls (lexenv.fdecls env))
            (lexenv.mdecls env) (nconc mdecls (lexenv.mdecls env))))))

 
(defun cons-var (name &optional (bits 0))
  (%istruct 'var name bits nil nil nil nil nil))


(defun augment-environment (env &key variable symbol-macro function macro declare)
  (if (and env (not (istruct-typep env 'lexical-environment)))
    (report-bad-arg env 'lexical-environment))
  (check-environment-args variable symbol-macro function macro)
  (let* ((vars (mapcar #'cons-var variable))
         (symbol-macros (mapcar #'(lambda (s)
				    (let* ((sym (car s)))
				      (unless (and (symbolp sym)
						   (not (constantp sym env))
						   (not (eq (variable-information sym env) :special)))
					(signal-program-error "~S can't by bound as a SYMBOL-MACRO" sym))
				      (let ((v (cons-var (car s)))) 
					(setf (var-expansion v) (cons :symbol-macro (cadr s)))
					v)))
				symbol-macro))
         (macros (mapcar #'(lambda (m) (list* (car m) 'macro (cadr m))) macro))
         (functions (mapcar #'(lambda (f) (list* f 'function nil)) function))
         (new-env (new-lexical-environment env)))
    (setf (lexenv.variables new-env) (nconc vars symbol-macros)
          (lexenv.functions new-env) (nconc functions macros))
    (process-declarations new-env declare symbol-macro)
    new-env))

(defun enclose (lambda-expression &optional env)
  (if (and env (not (istruct-typep env 'lexical-environment)))
    (report-bad-arg env 'lexical-environment))
  (unless (lambda-expression-p lambda-expression)
    (error "Invalid lambda-expression ~S." lambda-expression))
  (%make-function nil lambda-expression env))

#|| Might be nicer to do %declaim
(defmacro declaim (&rest decl-specs &environment env)
  `(progn
     (eval-when (:load-toplevel :execute)
       (proclaim ',@decl-specs))
     (eval-when (:compile-toplevel)
       (%declaim ',@decl-specs ,env))))
||#

(defmacro declaim (&environment env &rest decl-specs)
  "DECLAIM Declaration*
  Do a declaration or declarations for the global environment."
  (let* ((body (mapcar #'(lambda (spec) `(proclaim ',spec)) decl-specs)))
  `(progn
     (eval-when (:compile-toplevel)
       (compile-time-proclamation ',decl-specs ,env))
     (eval-when (:load-toplevel :execute)
       ,@body))))

;;; If warnings have more than a single entry on their
;;; args slot, don't merge them.
(defun merge-compiler-warnings (old-warnings)
  (let ((warnings nil))
    (dolist (w old-warnings)
      (let* ((w-args (compiler-warning-args w)))
        (if
          (or (cdr w-args)
              (dolist (w1 warnings t) 
                (let ((w1-args (compiler-warning-args w1)))
                  (when (and (eq (compiler-warning-warning-type w)
                                 (compiler-warning-warning-type w1))
                             w1-args
                             (null (cdr w1-args))
                             (eq (%car w-args)
                                 (%car w1-args)))
                    (incf (compiler-warning-nrefs w1))
                    (return)))))
          (push w warnings))))
    warnings))

;;; This is called by, e.g., note-function-info & so can't be -too- funky ...
;;; don't call proclaimed-inline-p or proclaimed-notinline-p with alphatized crap

(defun nx-declared-inline-p (sym env)
  (setq sym (maybe-setf-function-name sym))
  (loop
    (when (listp env)
      (return (and (symbolp sym)
                   (proclaimed-inline-p sym))))
    (dolist (decl (lexenv.fdecls env))
      (when (and (eq (car decl) sym)
                 (eq (cadr decl) 'inline))
        (return-from nx-declared-inline-p (eq (cddr decl) 'inline))))
    (setq env (lexenv.parent-env env))))

(defun report-compile-time-argument-mismatch (condition stream)
  (destructuring-bind (callee reason args spread-p)
      (compiler-warning-args condition)
    (format stream "In the ~a ~s with arguments ~:s,~%  "
            (if spread-p "application of" "call to")
            callee
            args)
    (case (car reason)
      (:toomany
       (destructuring-bind (provided max)
           (cdr reason)
         (format stream "~d argument~p were provided, but at most ~d ~a accepted~&  by " provided provided max (if (eql max 1) "is" "are"))))
      (:toofew
       (destructuring-bind (provided min)
           (cdr reason)
         (format stream "~d argument~p were provided, but at least ~d ~a required~&  by " provided provided min (if (eql min 1) "is" "are") )))
      (:odd-keywords
       (let* ((tail (cadr reason)))
         (format stream "the variable portion of the argument list ~s contains an odd number~&  of arguments and so can't be used to initialize keyword parameters~&  for " tail)))
      (:unknown-keyword
       (destructuring-bind (badguy goodguys)
           (cdr reason)
         (format stream "the keyword argument ~s is not one of ~s, which are recognized~&  by " badguy goodguys))))
    (format stream
            (ecase (compiler-warning-warning-type condition)       
              (:global-mismatch "the current global definition of ~s")
              (:environment-mismatch "the definition of ~s visible in the current compilation unit.")
              (:lexical-mismatch "the lexically visible definition of ~s"))
            callee)))

(defparameter *compiler-warning-formats*
  '((:special . "Undeclared free variable ~S")
    (:unused . "Unused lexical variable ~S")
    (:ignore . "Variable ~S not ignored.")
    (:undefined-function . "Undefined function ~S")
    (:unknown-declaration . "Unknown declaration ~S")
    (:unknown-type-declaration . "Unknown type ~S")
    (:unknown-declaration-variable . "~s declaration for unknown variable ~s")
    (:macro-used-before-definition . "Macro function ~S was used before it was defined.")
    (:unsettable . "Shouldn't assign to variable ~S")
    (:global-mismatch . report-compile-time-argument-mismatch)
    (:environment-mismatch . report-compile-time-argument-mismatch)
    (:lexical-mismatch . report-compile-time-argument-mismatch)    
    (:type . "Type declarations violated in ~S")
    (:type-conflict . "Conflicting type declarations for ~S")
    (:special-fbinding . "Attempt to bind compiler special name: ~s. Result undefined.")
    (:lambda . "Suspicious lambda-list: ~s")
    (:result-ignored . "Function result ignored in call to ~s")
    (:duplicate-definition . report-compile-time-duplicate-definition)
    (:program-error . "~a")))

(defun report-compile-time-duplicate-definition (condition stream)
  (destructuring-bind (name old-file new-file &optional from to) (compiler-warning-args condition)
    (format stream
            "Duplicate definitions of ~s~:[~*~;~:* (as a ~a and a ~a)~]~:[~;, in this file~:[~; and in ~s~]~]"
            name from to
            (and old-file new-file)
            (neq old-file new-file)
            old-file)))

(defun report-compiler-warning (condition stream)
  (let* ((warning-type (compiler-warning-warning-type condition))
         (format-string (cdr (assq warning-type *compiler-warning-formats*)))
         (name (reverse (compiler-warning-function-name condition))))
    (format stream "In ")
    (print-nested-name name stream)
    (when (every #'null name)
      (let ((position (compiler-warning-stream-position condition)))
        (when position (format stream " at position ~s" position))))
    (format stream ": ")
    (if (typep format-string 'string)
      (apply #'format stream format-string (compiler-warning-args condition))
      (funcall format-string condition stream))
    ;(format stream ".")
    (let ((nrefs (compiler-warning-nrefs condition)))
      (when (and nrefs (neq nrefs 1))
        (format stream " (~D references)" nrefs)))))

(defun environment-structref-info (name env)
  (let ((defenv (definition-environment env)))
    (when defenv
      (cdr (assq name (defenv.structrefs defenv))))))

; end
