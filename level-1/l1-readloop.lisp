;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

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


(defun quit (&optional (exit 0) &key error-handler)
  "exit must be either a (signed-byte 32) exit status or a function to call to exit lisp
   error-handler can be a function of one argument, the condition, that will be called if an
   error occurs while preparing to quit.  The error handler should exit"
  (if (or (null exit) (typep exit '(signed-byte 32)))
    (setq exit (let ((exit-status (or exit 0)))
                 #'(lambda () (#__exit exit-status))))
    (unless (typep exit 'function)
      (report-bad-arg exit '(or (signed-byte 32) function))))
  (let* ((ip *initial-process*)
	 (cp *current-process*))
    (when (process-verify-quit ip)
      (process-interrupt ip
			 #'(lambda ()
                             (handler-bind ((error (lambda (c)
                                                     (when error-handler
                                                       (funcall error-handler c)))))
                               (process-exit-application *current-process*
                                                         #'(lambda ()
                                                             (%set-toplevel nil)
                                                             (funcall exit) ;; must exit
                                                             (bug "Exit function didn't exit"))))))
      (unless (eq cp ip)
	(process-kill cp)))))


(defloadvar *quitting* nil)


(defun prepare-to-quit (&optional part)
  (let-globally ((*quitting* t))
    (when (or (null part) (eql 0 part))
      (dolist (f *lisp-cleanup-functions*)
	(funcall f)))
    (let* (#+forcibly-kill-threads (stragglers ()))
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
           #+forcibly-kill-threads     (push p stragglers))))))
      #+forcibly-kill-threads
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
          (dolist (vdecl (lexenv.vdecls env))
            (if (and (eq (car vdecl) sym)
                     (eq (cadr vdecl) 'special))
              (return-from %symbol-macroexpand-1 (values sym nil))))
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
                 (let ((augmented-env
                        (augment-environment env :function (mapcar #'car bindings))))
                  `(,(first form)
                     ,(mapcar (lambda (binding)
                                (list* (first binding)
                                       (cdr (macroexpand-all `(lambda ,@(rest binding))
                                                             (if (eq (first form) 'labels)
                                                                 augmented-env
                                                                 env)))))
                              bindings)
                     ,@decls
                     ,@(mexpand body augmented-env)))))
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

(defun %cons-def-info (type &optional lfbits keyvect data specializers qualifiers)
  (ecase type
    (defun nil)
    (defmacro (setq data '(macro) lfbits nil)) ;; some code assumes lfbits=nil
    (defgeneric (setq data (list :methods) lfbits (logior (ash 1 $lfbits-gfn-bit) lfbits)))
    (defmethod (setq data (list :methods
                                (%cons-def-info-method lfbits keyvect qualifiers specializers))
                     lfbits (logandc2 lfbits (ash 1 $lfbits-aok-bit))
                     keyvect nil))
    (deftype (setq data '(type) lfbits (cons nil *loading-file-source-file*))))
  (vector lfbits keyvect *loading-file-source-file* data))

(defun def-info.lfbits (def-info)
  (and def-info
       (let ((lfbits (svref def-info 0)))
	 (if (consp lfbits) (%car lfbits) lfbits))))

(defun def-info.keyvect (def-info)
  (and def-info (svref def-info 1)))

(defun def-info.file (def-info)
  (and def-info (svref def-info 2)))

(defun def-info.lambda (def-info)
  (%def-info.lambda def-info))

(defun def-info.environment (def-info)
  (%def-info.environment def-info))



(defun def-info.methods (def-info)
  (and def-info
       (let ((data (svref def-info 3)))
	 (and (eq (car data) :methods) (%cdr data)))))


(defun %cons-def-info-method (lfbits keyvect qualifiers specializers)
  (cons (cons (and keyvect
		   (if (logbitp $lfbits-aok-bit lfbits)
		     (and (not (logbitp $lfbits-rest-bit lfbits))
			  (list keyvect))
		     keyvect))
              *loading-file-source-file*)
        (cons qualifiers specializers)))

(defun def-info-method.keyvect (def-info-method)
  (let ((kv (caar def-info-method)))
    (if (listp kv)
      (values (car kv) t)
      (values kv  nil))))

(defun def-info-method.file (def-info-method)
  (cdar def-info-method))

(defun def-info-with-new-methods (def-info new-bits new-methods)
  (if (and (eq new-methods (def-info.methods def-info))
           (eql new-bits (def-info.lfbits def-info)))
    def-info
    (let ((new (copy-seq def-info))
          (old-bits (svref def-info 0)))
      (setf (svref new 0) (if (consp old-bits) (cons new-bits (cdr old-bits)) old-bits))
      (setf (svref new 3) (cons :methods new-methods))
      new)))

(defun def-info.macro-p (def-info)
  (let ((data (and def-info (svref def-info 3))))
    (eq (car data) 'macro)))

(defun def-info.function-p (def-info)
  (not (and def-info (eq (car (svref def-info 3)) 'type))))



(defun def-info.deftype (def-info)
  (and def-info
       (let ((bits (svref def-info 0)))
	 ;; bits or (bits . type-source-file)
	 (and (consp bits) bits))))

(defun def-info.deftype-type (def-info)
  ;; 'class (for defclass/defstruct) or 'macro (for deftype et. al.)
  (and def-info
       (consp (svref def-info 0))
       (svref def-info 1)))



(defvar *compiler-warn-on-duplicate-definitions* t)

(defun combine-deftype-infos (name def-info old-deftype new-deftype)
  (when (or new-deftype old-deftype)
    (when (and old-deftype new-deftype *compiler-warn-on-duplicate-definitions*)
      (nx1-whine :duplicate-definition
		 `(type ,name)
		 (cdr old-deftype)
		 (cdr new-deftype)))
    (let ((target (if new-deftype
		      (or (cdr new-deftype) (cdr old-deftype))
		      (cdr old-deftype)))
	  (target-deftype (def-info.deftype def-info)))
      (unless (and target-deftype (eq (cdr target-deftype) target))
	(setq def-info (copy-seq (or def-info '#(nil nil nil (ftype)))))
	(setf (svref def-info 0) (cons (def-info.lfbits def-info) target)))))
  def-info)

#+debug
(defun describe-def-info (def-info)
  (list :lfbits (def-info.lfbits def-info)
	:keyvect (def-info.keyvect def-info)
	:macro-p (def-info.macro-p def-info)
	:function-p (def-info.function-p def-info)
	:lambda (and (def-info.function-p def-info) (def-info.lambda def-info))
	:methods (and (def-info.function-p def-info) (def-info.methods def-info))
	:function-type (def-info.function-type def-info)
	:deftype (def-info.deftype def-info)
	:deftype-type (def-info.deftype-type def-info)))

(defun combine-gf-def-infos (name old-info new-info)
  (let* ((old-bits (def-info.lfbits old-info))
         (new-bits (def-info.lfbits new-info))
         (old-methods (def-info.methods old-info))
         (new-methods (def-info.methods new-info)))
    (when (and (logbitp $lfbits-gfn-bit old-bits) (logbitp $lfbits-gfn-bit new-bits))
      (when *compiler-warn-on-duplicate-definitions*
        (nx1-whine :duplicate-definition
                   name
                   (def-info.file old-info)
                   (def-info.file new-info)))
      (return-from combine-gf-def-infos new-info))
    (unless (congruent-lfbits-p old-bits new-bits)
      (if (logbitp $lfbits-gfn-bit new-bits)
        ;; A defgeneric, incongruent with previously defined methods
        (nx1-whine :incongruent-gf-lambda-list name)
        ;; A defmethod incongruent with previously defined explicit or implicit generic
        (nx1-whine :incongruent-method-lambda-list
                   (if new-methods `(:method ,@(cadar new-methods) ,name ,(cddar new-methods)) name)
                   name))
      ;; Perhaps once this happens, should just mark it somehow to not complain again
      (return-from combine-gf-def-infos 
        (if (logbitp $lfbits-gfn-bit old-bits) old-info new-info)))
    (loop for new-method in new-methods
          as old = (member (cdr new-method) old-methods :test #'equal :key #'cdr)
          do (when old
               (when *compiler-warn-on-duplicate-definitions*
                 (nx1-whine :duplicate-definition
                            `(:method ,@(cadr new-method) ,name ,(cddr new-method))
                            (def-info-method.file (car old))
                            (def-info-method.file new-method)))
               (setq old-methods (remove (car old) old-methods :test #'eq)))
          do (push new-method old-methods))
    (cond ((logbitp $lfbits-gfn-bit new-bits)
           ;; If adding a defgeneric, use its info.
           (setq old-info new-info old-bits new-bits))
          ((not (logbitp $lfbits-gfn-bit old-bits))
           ;; If no defgeneric (yet?) just remember whether any method has &key
           (setq old-bits (logior old-bits (logand new-bits (ash 1 $lfbits-keys-bit))))))
    ;; Check that all methods implement defgeneric keys
    (let ((gfkeys (and (logbitp $lfbits-gfn-bit old-bits) (def-info.keyvect old-info))))
      (when (> (length gfkeys) 0)
        (loop for minfo in old-methods
              do (multiple-value-bind (mkeys aok) (def-info-method.keyvect minfo)
                   (when (and mkeys
                              (not aok)
                              (setq mkeys (loop for gk across gfkeys
                                                unless (find gk mkeys) collect gk)))
                     (nx1-whine :gf-keys-not-accepted
                                `(:method ,@(cadr minfo) ,name ,(cddr minfo))
                                mkeys))))))
    (def-info-with-new-methods old-info old-bits old-methods)))

(defun combine-definition-infos (name old-info new-info)
  (let ((old-type (def-info.function-type old-info))
	(old-deftype (def-info.deftype old-info))
        (new-type (def-info.function-type new-info))
	(new-deftype (def-info.deftype new-info)))
    (cond ((and (eq old-type 'defgeneric) (eq new-type 'defgeneric))
           (setq new-info (combine-gf-def-infos name old-info new-info)))
	  ((or (eq (or old-type 'defun) (or new-type 'defun))
	       (eq (or old-type 'defgeneric) (or new-type 'defgeneric)))
           (when (and old-type new-type *compiler-warn-on-duplicate-definitions*)
             (nx1-whine :duplicate-definition name (def-info.file old-info) (def-info.file new-info)))
	   (unless new-info (setq new-info old-info)))
          (t
	   (when (and (def-info.function-p old-info) (def-info.function-p new-info)
		      *compiler-warn-on-duplicate-definitions*)
             (apply #'nx1-whine :duplicate-definition
                    name
                    (def-info.file old-info)
                    (def-info.file new-info)
                    (cond ((eq old-type 'defmacro) '("macro" "function"))
                          ((eq new-type 'defmacro) '("function" "macro"))
                          ((eq old-type 'defgeneric) '("generic function" "function"))
                          (t '("function" "generic function")))))
	   (unless new-type (setq new-info old-info))))
    (combine-deftype-infos name new-info old-deftype new-deftype)))

(defun record-definition-info (name info env)
  (let* ((definition-env (definition-environment env)))
    (if definition-env
      (let* ((defs (defenv.defined definition-env))
             (already (if (listp defs) (assq name defs) (gethash name defs))))
        (if already
          (setf (%cdr already) (combine-definition-infos name (%cdr already) info))
          (let ((outer (loop for defer = (cdr (defenv.type definition-env))
                               then (deferred-warnings.parent defer)
                             while (typep defer 'deferred-warnings)
                             thereis (gethash name (deferred-warnings.defs defer)))))
            (when outer
              (setq info (combine-definition-infos name (%cdr outer) info)))
            (let ((new (cons name info)))
              (if (listp defs)
                (setf (defenv.defined definition-env) (cons new defs))
                (setf (gethash name defs) new)))))
        info))))

(defun record-function-info (name info env)
  (record-definition-info name info env))

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

(defun note-type-info (name kind env)
  (record-definition-info name (%cons-def-info 'deftype nil kind) env))


; And this is different from FUNCTION-INFORMATION.
(defun retrieve-environment-function-info (name env)
 (let ((defenv (definition-environment env)))
   (when defenv
     (let* ((defs (defenv.defined defenv))
	    (sym (maybe-setf-function-name name))
	    (info (if (listp defs) (assq sym defs) (gethash sym defs))))
       (and info (def-info.function-p (cdr info)) info)))))

;;; Must differ from -something-, but not sure what ... 
(defun note-variable-info (name info env)
  (let ((definition-env (definition-environment env)))
    (if definition-env (push (cons name info) (defenv.specials definition-env)))
    name))

(defun compile-file-environment-p (env)
  (let ((defenv (definition-environment env)))
    (and defenv (eq 'compile-file (car (defenv.type defenv))))))

;; This is EVAL.
(defun cheap-eval (form)
  ;; Don't record source locations for explicit calls to EVAL.
  (let ((*nx-source-note-map* nil))
    (cheap-eval-in-environment form nil)))

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
  ;; Allow ADVICE, TRACE to have effects on self-calls.
  (declare (notinline cheap-eval-in-environment))
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
                   ((setf-function-name-p sym)
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
            #|
            ((eq sym 'symbol-macrolet)
	     (multiple-value-bind (body decls) (parse-body (cddr form) env)
	       (progn-in-env body env (augment-environment env :symbol-macro (cadr form) :declare (decl-specs-from-declarations decls)))))
            ((eq sym 'macrolet)
             (let ((temp-env (augment-environment env
                                                  :macro 
                                                  (mapcar #'(lambda (m)
                                                              (destructuring-bind (name arglist &body body) m
                                                                (setq name (nx-need-function-name name))
                                                                (list name (enclose (parse-macro name arglist body env)
                                                                                    env))))
                                                          (cadr form)))))
               (progn-in-env (cddr form) temp-env temp-env)))
            |#
            ((and (symbolp sym) 
                  (compiler-special-form-p sym)
                  (not (functionp (fboundp sym))))
             (if (eq sym 'unwind-protect)
               (destructuring-bind (protected-form . cleanup-forms) (cdr form)
                 (unwind-protect
                     (let ((*loading-toplevel-location* *loading-toplevel-location*))
                       (cheap-eval-in-environment protected-form env))
                   (progn-in-env cleanup-forms env env)))
               (let ((fn (cheap-eval-function nil (cheap-eval-transform form `(lambda () (progn ,form))) env)))
                 (funcall fn))))
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
            (t
             (signal-simple-condition 'simple-program-error "Car of ~S is not a function name or lambda-expression." form))))))


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

