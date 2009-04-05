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

(in-package "CCL")

;L1-readloop.lisp


(defvar *break-on-signals* nil
  "When (TYPEP condition *BREAK-ON-SIGNALS*) is true, then calls to SIGNAL will
   enter the debugger prior to signalling that condition.")
(defvar *break-on-warnings* nil)
(defvar *break-on-errors* t "Not CL.")
(defvar *debugger-hook* nil
  "This is either NIL or a function of two arguments, a condition and the value
   of *DEBUGGER-HOOK*. This function can either handle the condition or return
   which causes the standard debugger to execute. The system passes the value
   of this variable to the function because it binds *DEBUGGER-HOOK* to NIL
   around the invocation.")
(defvar *backtrace-on-break* nil)
(defvar *** nil
  "the previous value of **")
(defvar ** nil
  "the previous value of *")
(defvar * nil
  "the value of the most recent top level EVAL")
(defvar /// nil
  "the previous value of //")
(defvar // nil
  "the previous value of /")
(defvar / nil
  "a list of all the values returned by the most recent top level EVAL")
(defvar +++ nil
  "the previous value of ++")
(defvar ++ nil
  "the previous value of +")
(defvar + nil
  "the value of the most recent top level READ")
(defvar - nil
  "the form currently being evaluated")

(defvar *continuablep* nil)
(defvar *in-read-loop* nil 
 "Is T if waiting for input in the read loop")


(defvar *did-startup* nil)



(defmacro catch-cancel (&body body)
  `(catch :cancel ,@body))

(defmacro throw-cancel (&optional value)
  `(throw :cancel ,value))

;;; Throwing like this works in listeners and in the initial process.
;;; Can't easily tell if a process is a listener.  Should be able to.
(defun toplevel ()
  (throw :toplevel nil))


;;; It's not clear that this is the right behavior, but aborting CURRENT-PROCESS -
;;; when no one's sure just what CURRENT-PROCESS is - doesn't seem right either.
(defun interactive-abort ()
  (interactive-abort-in-process *current-process*))

(defun interactive-abort-in-process (p)
  (if p (process-interrupt p 
                           #'(lambda ()
                               (unless *inhibit-abort*
                                 (if *in-read-loop* 
                                        (abort-break)
                                        (abort))
                                 )))))


(defun abort (&optional condition)
  "Transfer control to a restart named ABORT, signalling a CONTROL-ERROR if
   none exists."
  (invoke-restart-no-return (find-restart 'abort condition)))

(defun continue (&optional condition)
  "Transfer control to a restart named CONTINUE, or return NIL if none exists."
  (let ((r (find-restart 'continue condition)))
    (if r (invoke-restart r))))

(defun muffle-warning (&optional condition)
  "Transfer control to a restart named MUFFLE-WARNING, signalling a
   CONTROL-ERROR if none exists."
  (invoke-restart-no-return (find-restart 'muffle-warning condition)))

(defun abort-break ()
  (invoke-restart-no-return 'abort-break))


(defun quit (&optional (exit-status 0))
  (unless (typep exit-status '(signed-byte 32))
    (report-bad-arg exit-status '(signed-byte 32)))
  (let* ((ip *initial-process*)
	 (cp *current-process*))
    (when (process-verify-quit ip)
      (process-interrupt ip
			 #'(lambda ()
			     (process-exit-application *current-process*
                                                       #'(lambda ()
                                                           (%set-toplevel nil)
                                                           (#__exit exit-status)))))
      (unless (eq cp ip)
	(process-kill cp)))))


(defloadvar *quitting* nil)


(defun prepare-to-quit (&optional part)
  (let-globally ((*quitting* t))
    (when (or (null part) (eql 0 part))
      (dolist (f *lisp-cleanup-functions*)
	(funcall f)))
    (let* ((stragglers ()))
      (dolist (p (all-processes))
	(unless (or (eq p *initial-process*)
		    (not (process-active-p p)))
	  (if (process-persistent p)
	    (process-reset p :shutdown)
	    (process-kill p))))
      (dolist (p (all-processes))
        (let* ((semaphore (process-termination-semaphore p)))
          (when semaphore
            (unless (eq p *initial-process*)
              (unless (timed-wait-on-semaphore semaphore 0.05)
                (push p stragglers))))))
      (dolist (p stragglers)
        (let* ((semaphore (process-termination-semaphore p)))
          (maybe-finish-process-kill p :kill)
          (when semaphore
            (timed-wait-on-semaphore semaphore 0.10)))))
    (shutdown-lisp-threads)
    (loop
      (let* ((streams (open-file-streams)))
        (when (null streams) (return))
        (let* ((ioblock (stream-ioblock (car streams) nil)))
          (when ioblock
            (setf (ioblock-inbuf-lock ioblock) nil
                  (ioblock-outbuf-lock ioblock) nil
                  (ioblock-owner ioblock) nil)))
        (close (car streams))))
    (setf (interrupt-level) -1)         ; can't abort after this
    )
  ;; Didn't abort, so really quitting.
  (setq *quitting* t))


(defun signal (condition &rest args)
  "Invokes the signal facility on a condition formed from DATUM and
   ARGUMENTS. If the condition is not handled, NIL is returned. If
   (TYPEP condition *BREAK-ON-SIGNALS*) is true, the debugger is invoked
   before any signalling is done."
  (setq condition (condition-arg condition args 'simple-condition))
  (let* ((*break-on-signals* *break-on-signals*))
     (let* ((old-bos *break-on-signals*))
       (when (unknown-ctype-p (let* ((*break-on-signals* nil)) (specifier-type old-bos)))
	 (setq *break-on-signals* nil)
	 (warn "~S : Ignoring invalid type specifier ~s." '*break-on-signals* old-bos)))
	 
   (when (typep condition *break-on-signals*)
     (let ((*break-on-signals* nil))
       (cbreak-loop "Signal" "Signal the condition." condition (%get-frame-ptr)))))
  (let ((%handlers% %handlers%))
    (while %handlers%
      (do* ((tag (pop %handlers%)) (handlers tag (cddr handlers)))
           ((null handlers))
        (when (typep condition (car handlers))
          (let ((fn (cadr handlers)))
            (cond ((null fn) (throw tag condition))
                  ((fixnump fn) (throw tag (cons fn condition)))
                  (t (funcall fn condition)))))))))

(defvar *error-print-circle* nil)   ; reset to T when we actually can print-circle



;;;***********************************
;;;Mini-evaluator
;;;***********************************

(defun new-lexical-environment (&optional parent)
  (%istruct 'lexical-environment parent nil nil nil nil nil nil))

(defmethod make-load-form ((e lexical-environment) &optional env)
  (declare (ignore env))
  nil)

(defun new-definition-environment (&optional (type 'compile-file))
  (%istruct 'definition-environment (list type)  nil nil nil nil nil nil nil nil nil nil nil nil ))

(defun definition-environment (env &optional clean-only &aux parent)
  (if (and env (not (istruct-typep env 'lexical-environment))) (report-bad-arg env 'lexical-environment))
  (do* () 
       ((or (null env) 
            (listp (setq parent (lexenv.parent-env env)))
            (and clean-only (or (lexenv.variables env) (lexenv.functions env)))))
    (setq env parent))
  (if (consp parent)
    env))

(defvar *symbol-macros* (make-hash-table :test #'eq))

(defun %define-symbol-macro (name expansion)
  (if (or (constant-symbol-p name)
	  (proclaimed-special-p name))
      (signal-program-error "Symbol ~s already globally defined as a ~A"
			    name (if (constant-symbol-p name)
				     'constant
				     'variable)))
  (setf (gethash name *symbol-macros*) expansion)
  name)

(defvar *macroexpand-hook* 'funcall
  "The value of this variable must be a designator for a function that can
  take three arguments, a macro expander function, the macro form to be
  expanded, and the lexical environment to expand in. The function should
  return the expanded form. This function is called by MACROEXPAND-1
  whenever a runtime expansion is needed. Initially this is set to
  FUNCALL.") ; Should be #'funcall. 
;(queue-fixup (setq *macroexpand-hook* #'funcall)) ;  No it shouldn't.

(defun %symbol-macroexpand-1 (sym env)
  (flet ((expand-it (expansion)
           (funcall *macroexpand-hook*
                    (constantly expansion)
                    sym
                    env)))
    (if (and env (not (istruct-typep env 'lexical-environment)))
      (report-bad-arg env 'lexical-environment))
    (do* ((env env (lexenv.parent-env env)))
         ((null env))
      (if (istruct-typep env 'definition-environment)
	(let* ((info (assq sym (defenv.symbol-macros env))))
	  (if info
	    (return-from %symbol-macroexpand-1 (values (expand-it (cdr info)) t))
	    (return)))
	(let* ((vars (lexenv.variables env)))
	  (when (consp vars)
	    (let* ((info (dolist (var vars)
			   (if (eq (var-name var) sym)
                             (return var)))))            
	      (when info
		(if (and (consp (setq info (var-expansion info)))
			 (eq (%car info) :symbol-macro))
                  (return-from %symbol-macroexpand-1 (values (expand-it (%cdr info)) t))
                  (return-from %symbol-macroexpand-1 (values sym nil)))))))))
    ;; Look it up globally.
    (multiple-value-bind (expansion win) (gethash sym *symbol-macros*)
      (if win (values (expand-it expansion) t) (values sym nil)))))

(defun macroexpand-all (form &optional (env (new-lexical-environment)))
  "Recursivly expand all macros in FORM."
  (flet ((mexpand (forms env)
           (mapcar (lambda (form) (macroexpand-all form env)) forms)))
    (macrolet ((destructuring-bind-body (binds form &body body)
                 (if (eql '&body (first (last binds)))
                   (let ((&body (gensym "&BODY")))
                     `(destructuring-bind ,(append (butlast binds) (list '&body &body))
                          ,form
                        (multiple-value-bind (body decls)
                            (parse-body ,&body env nil)
                          ,@body)))
                   `(destructuring-bind ,binds ,form ,@body))))
      (multiple-value-bind (expansion win)
          (macroexpand-1 form env)
        (if win
          (macroexpand-all expansion env)
          (if (atom form)
            form
            (case (first form)
              (macrolet
               (destructuring-bind-body (macros &body) (rest form)
                (setf env (augment-environment env
                                               :macro (mapcar (lambda (macro)
                                                                (destructuring-bind
                                                                      (name arglist &body body)
                                                                    macro
                                                                  (list name (enclose (parse-macro name arglist body env)))))
                                                              macros)
                                               :declare (decl-specs-from-declarations decls)))
                (let ((body (mexpand body env)))
                  (if decls
                    `(locally ,@decls ,@body)
                    `(progn ,@body)))))
              (symbol-macrolet
               (destructuring-bind-body (symbol-macros &body) (rest form)
                (setf env (augment-environment env :symbol-macro symbol-macros :declare (decl-specs-from-declarations decls)))
                (let ((body (mexpand body env)))
                  (if decls
                    `(locally ,@decls ,@body)
                    `(progn ,@body)))))
              ((let let* compiler-let)
               (destructuring-bind-body (bindings &body) (rest form)
                `(,(first form)
                   ,(mapcar (lambda (binding)
                              
                              (if (listp binding)
                                (list (first binding) (macroexpand-all (second binding) env))
                                binding))
                            bindings)
                   ,@decls
                   ,@(mexpand body env))))
              ((flet labels)
               (destructuring-bind-body (bindings &body) (rest form)
                `(,(first form)
                   ,(mapcar (lambda (binding)
                              (list* (first binding) (cdr (macroexpand-all `(lambda ,@(rest binding)) env))))
                            bindings)
                   ,@decls
                   ,@(mexpand body env))))
              (nfunction (list* 'nfunction (second form) (macroexpand-all (third form) env)))
              (function
                 (if (and (consp (second form))
                          (eql 'lambda (first (second form))))
                   (destructuring-bind (lambda arglist &body body&decls)
                       (second form)
                     (declare (ignore lambda))
                     (multiple-value-bind (body decls)
                         (parse-body body&decls env)
                       `(lambda ,arglist ,@decls ,@(mexpand body env))))
                   form))
              ((eval-when the locally block return-from)
                 (list* (first form) (second form) (mexpand (cddr form) env)))
              (setq
                 `(setq ,@(loop for (name value) on (rest form) by #'cddr
                                collect name
                                collect (macroexpand-all value env))))
              ((go quote) form)
              ((fbind with-c-frame with-variable-c-frame ppc-lap-function)
               (error "Unable to macroexpand ~S." form))
              ((catch if load-time-value multiple-value-call multiple-value-prog1 progn
                progv tagbody throw unwind-protect)
               (cons (first form) (mexpand (rest form) env)))
              (t
               ;; need to check that (first form) is either fboundp or a local function...
               (cons (first form) (mexpand (rest form) env))))))))))

(defun macroexpand-1 (form &optional env &aux fn)
  "If form is a macro (or symbol macro), expand it once. Return two values,
   the expanded form and a T-or-NIL flag indicating whether the form was, in
   fact, a macro. ENV is the lexical environment to expand in, which defaults
   to the null environment."
  (declare (resident))
  (if (and (consp form)
           (symbolp (%car form)))
    (if (setq fn (macro-function (%car form) env))
      (values (funcall *macroexpand-hook* fn form env) t)
      (values form nil))
    (if (and form (symbolp form))
      (%symbol-macroexpand-1 form env)
      (values form nil))))

(defun macroexpand (form &optional env)
  "Repetitively call MACROEXPAND-1 until the form can no longer be expanded.
   Returns the final resultant form, and T if it was expanded. ENV is the
   lexical environment to expand in, or NIL (the default) for the null
   environment."
  (declare (resident))
  (multiple-value-bind (new win) (macroexpand-1 form env)
    (do* ((won-at-least-once win))
         ((null win) (values new won-at-least-once))
      (multiple-value-setq (new win) (macroexpand-1 new env)))))

(defun %symbol-macroexpand (form env &aux win won)
  ; Keep expanding until no longer a symbol-macro or no longer a symbol.
  (loop
    (unless (and form (symbolp form)) (return))
    (multiple-value-setq (form win) (macroexpand-1 form env))
    (if win (setq won t) (return)))
  (values form won))

(defun retain-lambda-expression (name lambda-expression env)
  (if (and (let* ((lambda-list (cadr lambda-expression)))
             (and (not (memq '&lap lambda-list))
                  (not (memq '&method lambda-list))
                  (not (memq '&lexpr lambda-list))))
           (nx-declared-inline-p name env)
           (not (gethash name *nx1-alphatizers*))
           ; A toplevel definition defined inside a (symbol-)macrolet should
           ; be inlineable.  It isn't; call DEFINITION-ENVIRONMENT with a
           ; "clean-only" argument to ensure that there are no lexically
           ; bound macros or symbol-macros.
           (definition-environment env t))
    lambda-expression))


(defun %cons-def-info (type &optional lfbits keyvect lambda specializers qualifiers)
  (ecase type
    (defun nil)
    (defmacro (setq lambda '(macro) lfbits nil)) ;; some code assumes lfbits=nil
    (defgeneric (setq lambda (list :methods)))
    (defmethod (setq lambda (list :methods (cons qualifiers specializers)))))
  (vector lfbits keyvect *loading-file-source-file* lambda))

(defun def-info.lfbits (def-info)
  (and def-info (svref def-info 0)))

(defun def-info.keyvect (def-info)
  (and def-info (svref def-info 1)))

(defun def-info.file (def-info)
  (and def-info (svref def-info 2)))

(defun def-info.lambda (def-info)
  (let ((data (and def-info (svref def-info 3))))
    (and (eq (car data) 'lambda) data)))

(defun def-info.methods (def-info)
  (let ((data (and def-info (svref def-info 3))))
    (and (eq (car data) :methods) (%cdr data))))

(defun def-info-with-new-methods (def-info new-methods)
  (unless (eq (def-info.type def-info) 'defgeneric) (error "Bug: not method info: ~s" def-info))
  (if (eq new-methods (def-info.methods def-info))
    def-info
    (let ((new (copy-seq def-info)))
      (setf (svref new 3) (cons :methods new-methods))
      new)))

(defun def-info.macro-p (def-info)
  (let ((data (and def-info (svref def-info 3))))
    (eq (car data) 'macro)))

(defun def-info.type (def-info)
  (if (null def-info) nil  ;; means FTYPE decl or lap function
    (let ((data (svref def-info 3)))
      (ecase (car data)
        ((nil lambda) 'defun)
        (:methods 'defgeneric)
        (macro 'defmacro)))))

(defparameter *one-arg-defun-def-info* (%cons-def-info 'defun (encode-lambda-list '(x))))

(defvar *compiler-warn-on-duplicate-definitions* t)

(defun combine-function-infos (name old-info new-info)
  (let ((old-type (def-info.type old-info))
        (new-type (def-info.type new-info)))
    (cond ((and (eq old-type 'defgeneric) (eq new-type 'defgeneric))
           ;; TODO: Check compatibility of lfbits...
           ;; TODO: check that all methods implement defgeneric keys
           (let ((old-methods (def-info.methods old-info))
                 (new-methods (def-info.methods new-info)))
             (loop for new-method in new-methods
                   do (if (member new-method old-methods :test #'equal)
                        (when *compiler-warn-on-duplicate-definitions*
                          (nx1-whine :duplicate-definition
                                     `(method ,@(car new-method) ,name ,(cdr new-method))
                                     (def-info.file old-info)
                                     (def-info.file new-info)))
                        (push new-method old-methods)))
             (def-info-with-new-methods old-info old-methods)))
          ((or (eq (or old-type 'defun) (or new-type 'defun))
               (eq (or old-type 'defgeneric) (or new-type 'defgeneric)))
           (when (and old-type new-type *compiler-warn-on-duplicate-definitions*)
             (nx1-whine :duplicate-definition name (def-info.file old-info) (def-info.file new-info)))
           (or new-info old-info))
          (t
           (when *compiler-warn-on-duplicate-definitions*
             (apply #'nx1-whine :duplicate-definition
                    name
                    (def-info.file old-info)
                    (def-info.file new-info)
                    (cond ((eq old-type 'defmacro) '("macro" "function"))
                          ((eq new-type 'defmacro) '("function" "macro"))
                          ((eq old-type 'defgeneric) '("generic function" "function"))
                          (t '("function" "generic function")))))
           new-info))))

(defun record-function-info (name info env)
  (let* ((definition-env (definition-environment env)))
    (if definition-env
      (let* ((defs (defenv.defined definition-env))
             (already (if (listp defs) (assq name defs) (gethash name defs))))
        (if already
          (setf (%cdr already) (combine-function-infos name (%cdr already) info))
          (let ((outer (loop for defer = (cdr (defenv.type definition-env))
                               then (deferred-warnings.parent defer)
                             while (typep defer 'deferred-warnings)
                             thereis (gethash name (deferred-warnings.defs defer)))))
            (when outer
              (setq info (combine-function-infos name (%cdr outer) info)))
            (let ((new (cons name info)))
              (if (listp defs)
                (setf (defenv.defined definition-env) (cons new defs))
                (setf (gethash name defs) new)))))
        info))))


;;; This is different from AUGMENT-ENVIRONMENT.
(defun note-function-info (name lambda-expression env)
  (let* ((info nil)
         (name (maybe-setf-function-name name)))
    (when (lambda-expression-p lambda-expression)
      (multiple-value-bind (lfbits keyvect) (encode-lambda-list (cadr lambda-expression) t)
        (setq info (%cons-def-info 'defun lfbits keyvect
                                   (retain-lambda-expression name lambda-expression env)))))
    (record-function-info name info env))
  name)

; And this is different from FUNCTION-INFORMATION.
(defun retrieve-environment-function-info (name env)
 (let ((defenv (definition-environment env)))
   (when defenv
     (let ((defs (defenv.defined defenv))
           (sym (maybe-setf-function-name name)))
       (if (listp defs) (assq sym defs) (gethash sym defs))))))

(defun maybe-setf-function-name (name)
  (if (and (consp name) (eq (car name) 'setf))
    (setf-function-name (cadr name))
    name))

;;; Must differ from -something-, but not sure what ... 
(defun note-variable-info (name info env)
  (let ((definition-env (definition-environment env)))
    (if definition-env (push (cons name info) (defenv.specials definition-env)))
    name))

(defun compile-file-environment-p (env)
  (let ((defenv (definition-environment env)))
    (and defenv (eq 'compile-file (car (defenv.type defenv))))))

(defun cheap-eval (form)
  (cheap-eval-in-environment form nil))

; used by nfcomp too
; Should preserve order of decl-specs; it sometimes matters.
(defun decl-specs-from-declarations (declarations)
  (let ((decl-specs nil))
    (dolist (declaration declarations decl-specs)
      ;(unless (eq (car declaration) 'declare) (say "what"))
      (dolist (decl-spec (cdr declaration))
        (setq decl-specs (nconc decl-specs (list decl-spec)))))))

(defun cheap-eval-macroexpand-1 (form env)
  (multiple-value-bind (new win) (macroexpand-1 form env)
    (when win
      (note-source-transformation form new))
    (values new win)))

(defun cheap-eval-transform (original new)
  (note-source-transformation original new)
  new)

(defun cheap-eval-function (name lambda env)
  (multiple-value-bind (lfun warnings)
                       (compile-named-function lambda
                                               :name name
                                               :env env
                                               :function-note *loading-toplevel-location*
                                               :keep-lambda *save-definitions*
                                               :keep-symbols *save-local-symbols*
                                               :source-notes *nx-source-note-map*)
    (signal-or-defer-warnings warnings env)
    lfun))

(fset 'nx-source-note (nlambda bootstrapping-source-note (form) (declare (ignore form)) nil))

(defun cheap-eval-in-environment (form env &aux sym)
  (declare (resident))
  ;; records source locations if *nx-source-note-map* is bound by caller
  (setq *loading-toplevel-location* (or (nx-source-note form) *loading-toplevel-location*))
  (flet ((progn-in-env (body&decls parse-env base-env)
           (multiple-value-bind (body decls) (parse-body body&decls parse-env)
             (setq base-env (augment-environment base-env :declare (decl-specs-from-declarations decls)))
             (loop with default-location = *loading-toplevel-location*
               while (cdr body) as form = (pop body)
               do (cheap-eval-in-environment form base-env)
               do (setq *loading-toplevel-location* default-location))
             (cheap-eval-in-environment (car body) base-env))))
    (if form
      (cond ((symbolp form) 
             (multiple-value-bind (expansion win) (cheap-eval-macroexpand-1 form env)
               (if win 
                 (cheap-eval-in-environment expansion env)
                 (let* ((defenv (definition-environment env))
                        (constant (if defenv (assq form (defenv.constants defenv))))
                        (constval (%cdr constant)))
                   (if constant
                     (if (neq (%unbound-marker-8) constval)
                       constval
                       (error "Can't determine value of constant symbol ~s" form))
                     (if (constant-symbol-p form)
                       (%sym-global-value form)
                       (symbol-value form)))))))
            ((atom form) form)
            ((eq (setq sym (%car form)) 'quote)
             (verify-arg-count form 1 1)
             (%cadr form))
            ((eq sym 'function)
             (verify-arg-count form 1 1)
             (cond ((symbolp (setq sym (%cadr form)))
                    (multiple-value-bind (kind local-p)
                        (function-information sym env)
                      (if (and local-p (eq kind :macro))
                        (error "~s can't be used to reference lexically defined macro ~S" 'function sym)))
                    (%function sym))
                   ((and (consp sym) (eq (%car sym) 'setf) (consp (%cdr sym)) (null (%cddr sym)))
                    (multiple-value-bind (kind local-p)
                        (function-information sym env)
                      (if (and local-p (eq kind :macro))
                        (error "~s can't be used to reference lexically defined macro ~S" 'function sym)))
                    (%function (setf-function-name (%cadr sym))))
                   (t (cheap-eval-function nil sym env))))
            ((eq sym 'nfunction)
             (verify-arg-count form 2 2)
             (cheap-eval-function (%cadr form) (%caddr form) env))
            ((eq sym 'progn) (progn-in-env (%cdr form) env env))
            ((eq sym 'setq)
             (if (not (%ilogbitp 0 (list-length form)))
               (verify-arg-count form 0 0)) ;Invoke a "Too many args" error.
             (let* ((sym nil)
                    (val nil)
                    (original form))
               (while (setq form (%cdr form))
                 (setq sym (require-type (pop form) 'symbol))
                 (multiple-value-bind (expansion expanded)
                                      (cheap-eval-macroexpand-1 sym env)
                   (if expanded
                     (setq val (cheap-eval-in-environment
                                (cheap-eval-transform original `(setf ,expansion ,(%car form)))
                                env))
                     (set sym (setq val (cheap-eval-in-environment (%car form) env))))))
               val))
            ((eq sym 'eval-when)
             (destructuring-bind (when . body) (%cdr form)
               (when (or (memq 'eval when) (memq :execute when)) (progn-in-env body env env))))
            ((eq sym 'if)
             (destructuring-bind (test true &optional false) (%cdr form)
               (setq test (let ((*loading-toplevel-location* *loading-toplevel-location*))
                            (cheap-eval-in-environment test env)))
               (cheap-eval-in-environment (if test true false) env)))
            ((eq sym 'locally) (progn-in-env (%cdr form) env env))
            ((eq sym 'symbol-macrolet)
	     (multiple-value-bind (body decls) (parse-body (cddr form) env)
	       (progn-in-env body env (augment-environment env :symbol-macro (cadr form) :declare (decl-specs-from-declarations decls)))))
            ((eq sym 'macrolet)
             (let ((temp-env (augment-environment env
                                                  :macro 
                                                  (mapcar #'(lambda (m)
                                                              (destructuring-bind (name arglist &body body) m
                                                                (list name (enclose (parse-macro name arglist body env)
                                                                                    env))))
                                                          (cadr form)))))
               (progn-in-env (cddr form) temp-env temp-env)))
            ((and (symbolp sym) 
                  (compiler-special-form-p sym)
                  (not (functionp (fboundp sym))))
             (if (eq sym 'unwind-protect)
               (destructuring-bind (protected-form . cleanup-forms) (cdr form)
                 (unwind-protect
                     (let ((*loading-toplevel-location* *loading-toplevel-location*))
                       (cheap-eval-in-environment protected-form env))
                   (progn-in-env cleanup-forms env env)))
               (funcall (cheap-eval-function nil (cheap-eval-transform form `(lambda () (progn ,form))) env))))
            ((and (symbolp sym) (macro-function sym env))
             (cheap-eval-in-environment (cheap-eval-macroexpand-1 form env) env))
            ((or (symbolp sym)
                 (and (consp sym) (eq (%car sym) 'lambda)))
             (let ((args nil) (form-location *loading-toplevel-location*))
               (dolist (elt (%cdr form))
                 (push (cheap-eval-in-environment elt env) args)
                 (setq *loading-toplevel-location* form-location))
               (apply #'call-check-regs (if (symbolp sym) sym (cheap-eval-function nil sym env))
                      (nreverse args))))
            (t (signal-simple-condition 'simple-program-error "Car of ~S is not a function name or lambda-expression." form))))))


(%fhave 'eval #'cheap-eval)



  
(defun call-check-regs (fn &rest args)
  (declare (dynamic-extent args)
           (optimize (debug 3)))        ; don't use any saved registers
  (let ((old-regs (multiple-value-list (get-saved-register-values))))
    (declare (dynamic-extent old-regs))
    (multiple-value-prog1 (apply fn args)
      (let* ((new-regs (multiple-value-list (get-saved-register-values)))
             (new-regs-tail new-regs))
        (declare (dynamic-extent new-regs))
        (unless (dolist (old-reg old-regs t)
                  (unless (eq old-reg (car new-regs-tail))
                    (return nil))
                  (pop new-regs-tail))
          (apply 'error "Registers clobbered applying ~s to ~s~%~@{~a sb: ~s, Was: ~s~%~}"
                 fn args
                 (mapcan 'list
                         (let ((res nil))
                           (dotimes (i (length old-regs))
                             (push (format nil "save~d" i) res))
                           (nreverse res))
                         old-regs
                         new-regs)))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack frame accessors.

; Kinda scant, wouldn't you say ?


;end of L1-readloop.lisp

