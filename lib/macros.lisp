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

;;; Macros (and functions/constants used at macroexpand-time) ONLY.

(in-package "CCL")

(eval-when (eval compile)
  (require "LEVEL-2")
  (require "BACKQUOTE")
  (require "DEFSTRUCT-MACROS"))

;; Constants

(defmacro defconstant (sym val &optional (doc () doc-p) &environment env)
  "Define a global constant, saying that the value is constant and may be
  compiled into code. If the variable already has a value, and this is not
  EQL to the new value, the code is not portable (undefined behavior). The
  third argument is an optional documentation string for the variable."
  (setq sym (require-type sym 'symbol)
        doc (if doc-p (require-type doc 'string)))
  `(progn
     (eval-when (:compile-toplevel)
       (define-compile-time-constant ',sym ',val ,env))
     (eval-when (:load-toplevel :execute)
       (%defconstant ',sym ,val ,@(if doc-p (list doc))))))

;; Lists

(defmacro %car (x)
  `(car (the list ,x)))

(defmacro set-%car (x y)
  `(setf (car (the cons ,x)) ,y))

(defmacro %cdr (x)
  `(cdr (the list ,x)))

(defmacro set-%cdr (x y)
  `(setf (cdr (the cons ,x)) ,y))

(defmacro %caar (x)
 `(%car (%car ,x)))

(defmacro %cadr (x)
 `(%car (%cdr ,x)))

(defmacro %cdar (x)
 `(%cdr (%car ,x)))

(defmacro %cddr (x)
 `(%cdr (%cdr ,x)))

(defmacro %caaar (x)
 `(%car (%car (%car ,x))))

(defmacro %caadr (x)
 `(%car (%car (%cdr ,x))))

(defmacro %cadar (x)
 `(%car (%cdr (%car ,x))))

(defmacro %caddr (x)
 `(%car (%cdr (%cdr ,x))))

(defmacro %cdaar (x)
 `(%cdr (%car (%car ,x))))

(defmacro %cdadr (x)
 `(%cdr (%car (%cdr ,x))))

(defmacro %cddar (x)
 `(%cdr (%cdr (%car ,x))))

(defmacro %cdddr (x)
 `(%cdr (%cdr (%cdr ,x))))

(defmacro %rplaca (x y)
  `(rplaca (the cons ,x) ,y))

(defmacro %rplacd (x y)
  `(rplacd (the cons ,x) ,y))

; These are open-coded by the compiler to isolate platform
; dependencies.

(defmacro %unbound-marker-8 ()
  `(%unbound-marker))

(defmacro %slot-missing-marker ()
  `(%illegal-marker))




(defmacro %null-ptr () '(%int-to-ptr 0))

;;;Assorted useful macro definitions

(defmacro def-accessors (ref &rest names)
  (define-accessors ref names))

(defmacro def-accessor-macros (ref &rest names)
  (define-accessors ref names t))

(defun define-accessors (ref names &optional no-constants
                             &aux (arg (gensym)) (index 0) progn types)
  (when (listp ref)
    (setq types ref
          ref (pop names)))
  (dolist (name names)
    (when name
      (unless (listp name) (setq name (list name)))
      (dolist (sym name)
        (when sym
          (push `(defmacro ,sym (,arg) (list ',ref ,arg ,index)) progn)
          (unless no-constants
	    (push `(defconstant ,sym ,index) progn)))))
    (setq index (1+ index)))
 `(progn
    ,.(nreverse progn)
    ,@(if types `((add-accessor-types ',types ',names)))
    ,index))

(defmacro specialv (var)
  `(locally (declare (special ,var)) ,var))


(defmacro prog1 (valform &rest otherforms)
 (let ((val (gensym)))
 `(let ((,val ,valform))
   ,@otherforms
   ,val)))

(defmacro prog2 (first second &rest others)
 `(progn ,first (prog1 ,second ,@others)))

(defmacro prog (inits &body body &environment env)
  (multiple-value-bind (forms decls) (parse-body body env nil)
    `(block nil
       (let ,inits
         ,@decls
         (tagbody ,@forms)))))

(defmacro prog* (inits &body body &environment env)
  (multiple-value-bind (forms decls) (parse-body body env nil)
    `(block nil
       (let* ,inits
         ,@decls
         (tagbody ,@forms)))))


(defmacro %stack-block ((&rest specs) &body forms &aux vars lets)
  (dolist (spec specs)
    (destructuring-bind (var ptr &key clear) spec
      (push var vars)
      (push `(,var (%new-ptr ,ptr ,clear)) lets)))
  `(let* ,(nreverse lets)
     (declare (dynamic-extent ,@vars))
     (declare (type macptr ,@vars))
     (declare (unsettable ,@vars))
     ,@forms))

(defmacro %vstack-block (spec &body forms)
  `(%stack-block (,spec) ,@forms))

(defmacro dolist ((varsym list &optional ret) &body body &environment env)
  (if (not (symbolp varsym)) (signal-program-error $XNotSym varsym))
  (let* ((toplab (gensym))
         (tstlab (gensym))
         (lstsym (gensym)))
    (multiple-value-bind (forms decls) (parse-body body env nil)
      `(block nil
         (let* ((,lstsym ,list))
           (tagbody
              (go ,tstlab)
              ,toplab
              (let ((,varsym (car ,lstsym)))
                ,@decls
                (tagbody
                   ,@forms)
                (setq ,lstsym (cdr (the list ,lstsym))))
              ,tstlab
              (if ,lstsym (go ,toplab))))
         ,@(if ret `((let ((,varsym nil))
                       (declare (ignorable ,varsym))
                       ;;,@decls
                       ,ret)))))))


(defmacro dovector ((varsym vector &optional ret) &body body &environment env)
  (if (not (symbolp varsym))(signal-program-error $XNotSym varsym))
  (let* ((toplab (gensym))
         (tstlab (gensym))
         (lengthsym (gensym))
         (indexsym (gensym))
         (vecsym (gensym)))
    (multiple-value-bind (forms decls) (parse-body body env nil)
     `(let* ((,vecsym ,vector)
             (,lengthsym (length ,vecsym))
             (,indexsym 0)
             ,varsym)
        ,@decls
        ,@(let ((type (nx-form-type vector env)))
            (unless (eq type t)
              `((declare (type ,type ,vecsym)))))
        (block nil
          (tagbody
            (go ,tstlab)
            ,toplab
            (setq ,varsym (locally (declare (optimize (speed 3) (safety 0)))
                            (aref ,vecsym ,indexsym))
                  ,indexsym (%i+ ,indexsym 1))
            ,@forms
            ,tstlab
            (if (%i< ,indexsym ,lengthsym) (go ,toplab)))
          ,@(if ret `((progn (setq ,varsym nil) ,ret))))))))

(defmacro report-bad-arg (&rest args)
  `(values (%badarg ,@args)))

(defmacro %cons-restart (name action report interactive test)
 `(%istruct 'restart ,name ,action ,report ,interactive ,test))

(defmacro restart-bind (clauses &body body)
  "Executes forms in a dynamic context where the given restart bindings are
   in effect. Users probably want to use RESTART-CASE. When clauses contain
   the same restart name, FIND-RESTART will find the first such clause."
  (let* ((restarts (mapcar #'(lambda (clause) 
                               (list (make-symbol (symbol-name (require-type (car clause) 'symbol)))
                                     `(%cons-restart nil nil nil nil nil)))
                           clauses))
         (bindings (mapcar #'(lambda (clause name)
                              `(make-restart ,(car name) ',(car clause)
                                             ,@(cdr clause)))
                           clauses restarts))
        (cluster (gensym)))
    `(let* (,@restarts)
       (declare (dynamic-extent ,@(mapcar #'car restarts)))
       (let* ((,cluster (list ,@bindings))
              (%restarts% (cons ,cluster %restarts%)))
         (declare (dynamic-extent ,cluster %restarts%))
         (progn
           ,@body)))))

(defmacro handler-bind (clauses &body body)
  "(HANDLER-BIND ( {(type handler)}* )  body)
   Executes body in a dynamic context where the given handler bindings are
   in effect. Each handler must take the condition being signalled as an
   argument. The bindings are searched first to last in the event of a
   signalled condition."
  (let* ((fns)
         (decls)         
         (bindings (mapcan #'(lambda (clause)
                               (destructuring-bind (condition handler) clause
                                 (if (and (consp handler)(eq (car handler) 'function)
                                          (consp (cadr handler))(eq (car (cadr handler)) 'lambda))
                                   (let ((fn (gensym)))
                                     (push `(,fn ,handler) fns)
                                     (push `(declare (dynamic-extent ,fn)) decls)
                                     `(',condition ,fn))
                                   (list `',condition
                                         `,handler))))
                           clauses))
        (cluster (gensym)))    
    (if (null bindings)
      `(progn ,@body)
      `(let* (,@fns
              (,cluster (list ,@bindings))
              (%handlers% (cons ,cluster %handlers%)))
         (declare (dynamic-extent ,cluster %handlers%))
         ,@decls
         ,@body))))

(defmacro restart-case (&environment env form &rest clauses)
  "(RESTART-CASE form
   {(case-name arg-list {keyword value}* body)}*)
   The form is evaluated in a dynamic context where the clauses have special
   meanings as points to which control may be transferred (see INVOKE-RESTART).
   When clauses contain the same case-name, FIND-RESTART will find the first
   such clause. If Expression is a call to SIGNAL, ERROR, CERROR or WARN (or
   macroexpands into such) then the signalled condition will be associated with
   the new restarts."
  (let ((cluster nil))
    (when clauses (setq cluster (gensym) form (restart-case-form form env cluster)))
    (flet ((restart-case-1 (name arglist &rest forms)
             (let (interactive report test)
               (loop
                 (case (car forms)
                   (:interactive (setq interactive (cadr forms)))
                   (:report (setq report (cadr forms)))
                   (:test (setq test (cadr forms)))
                   (t (return nil)))
                 (setq forms (cddr forms)))
               (when (and report (not (stringp report)))
                 (setq report `#',report))
               (when interactive
                 (setq interactive `#',interactive))
               (when test
                 (setq test `#',test))
               (values (require-type name 'symbol) arglist report interactive test forms))))
      (cond ((null clauses) form)
            ((and (null (cdr clauses)) (null (cadr (car clauses))))
             (let ((block (gensym)) 
                   (restart-name (gensym)))
               (multiple-value-bind (name arglist report interactive test body)
                                    (apply #'restart-case-1 (car clauses))
                 (declare (ignore arglist))
                 `(block ,block
                    (let* ((,restart-name (%cons-restart ',name () ,report ,interactive ,test))
                           (,cluster (list ,restart-name)))
                      (declare (dynamic-extent ,restart-name ,cluster))
                      (catch ,cluster
                        (let ((%restarts% (cons ,cluster %restarts%)))
                          (declare (dynamic-extent %restarts%))
                          (return-from ,block ,form))))
                    ,@body))))
            (t
             (let ((block (gensym)) (val (gensym))
                   (index -1) restarts restart-names restart-name cases)
               (while clauses
                 (setq index (1+ index))
                 (multiple-value-bind (name arglist report interactive test body)
                                      (apply #'restart-case-1 (pop clauses))
                   (push (setq restart-name (make-symbol (symbol-name name))) restart-names)
                   (push (list restart-name `(%cons-restart ',name ,index ,report ,interactive ,test))
                         restarts)
                   (when (null clauses) (setq index t))
                   (push `(,index (apply #'(lambda ,arglist ,@body) ,val))
                         cases)))
               `(block ,block
                  (let ((,val (let* (,@restarts
                                     (,cluster (list ,@(reverse restart-names))))
                                (declare (dynamic-extent ,@restart-names ,cluster))
                                (catch ,cluster
                                  (let ((%restarts% (cons ,cluster %restarts%)))
                                    (declare (dynamic-extent %restarts%))
                                    (return-from ,block ,form))))))
                    (case (pop ,val)
                      ,@(nreverse cases))))))))))


; Anything this hairy should die a slow and painful death.
; Unless, of course, I grossly misunderstand...
(defun restart-case-form (form env clustername)
  (let ((expansion (macroexpand form env))
        (head nil))
    (if (and (listp expansion)          ; already an ugly hack, made uglier by %error case ...
             (memq (setq head (pop expansion)) '(signal error cerror warn %error)))
      (let ((condform nil)
            (signalform nil)
            (cname (gensym)))
        (case head
          (cerror
           (destructuring-bind 
             (continue cond &rest args) expansion
             (setq condform `(condition-arg ,cond (list ,@args) 'simple-error)
                   signalform `(cerror ,continue ,cname))))
          ((signal error warn)
           (destructuring-bind
             (cond &rest args) expansion
             (setq condform `(condition-arg ,cond (list ,@args) ,(if (eq head 'warning)
                                                                   ''simple-warning
                                                                   (if (eq head 'error)
                                                                     ''simple-error
                                                                     ''simple-condition)))
                   signalform `(,head ,cname))))
          (t ;%error
           (destructuring-bind (cond args fp) expansion
             (setq condform `(condition-arg ,cond ,args 'simple-error)
                   signalform `(%error ,cname nil ,fp)))))
        `(let ((,cname ,condform))
           (with-condition-restarts ,cname ,clustername
             ,signalform)))
      form)))
      

(defmacro handler-case (form &rest clauses)
  "(HANDLER-CASE form
   { (type ([var]) body) }* )
   Execute FORM in a context with handlers established for the condition
   types. A peculiar property allows type to be :NO-ERROR. If such a clause
   occurs, and form returns normally, all its values are passed to this clause
   as if by MULTIPLE-VALUE-CALL.  The :NO-ERROR clause accepts more than one
   var specification."
  (let* ((no-error-clause (assoc :no-error clauses)))
    (if no-error-clause
      (let* ((normal-return (gensym))
             (error-return (gensym)))
        `(block ,error-return
          (multiple-value-call #'(lambda ,@(cdr no-error-clause))
            (block ,normal-return
              (return-from ,error-return
                (handler-case (return-from ,normal-return ,form)
                  ,@(remove no-error-clause clauses)))))))
      (flet ((handler-case (type var &rest body)
               (when (eq type :no-error)
                 (signal-program-error "Duplicate :no-error clause. "))
           (values type var body)))
        (cond ((null clauses) form)
          ((null (cdr clauses))
           (let ((block   (gensym))
                 (cluster (gensym)))
             (multiple-value-bind (type var body)
                                  (apply #'handler-case (car clauses))
               (if var
                 `(block ,block
                    ((lambda ,var ,@body)
                      (let* ((,cluster (list ',type)))
                        (declare (dynamic-extent ,cluster))
                        (catch ,cluster
                          (let ((%handlers% (cons ,cluster %handlers%)))
                            (declare (dynamic-extent %handlers%))
                            (return-from ,block ,form))))))
                 `(block ,block
                    (let* ((,cluster (list ',type)))
                      (declare (dynamic-extent ,cluster))
                      (catch ,cluster
                        (let ((%handlers% (cons ,cluster %handlers%)))
                          (declare (dynamic-extent %handlers%))
                          (return-from ,block ,form)))
                      (locally ,@body)))))))
          (t (let ((block (gensym)) (cluster (gensym)) (val (gensym))
                   (index -1) handlers cases)
               (while clauses
                 (setq index (1+ index))
                 (multiple-value-bind (type var body)
                                      (apply #'handler-case (pop clauses))                   
                   (push `',type handlers)
                   (push index handlers)
                   (when (null clauses) (setq index t))
                   (push (if var
                           `(,index ((lambda ,var ,@body) ,val))
                           `(,index (locally ,@body))) cases)))
               `(block ,block
                  (let ((,val (let* ((,cluster (list ,@(nreverse handlers))))
                                (declare (dynamic-extent ,cluster))
                                (catch ,cluster
                                  (let ((%handlers% (cons ,cluster %handlers%)))
                                    (declare (dynamic-extent %handlers%))
                                    (return-from ,block ,form))))))
                    (case (pop ,val)
                      ,@(nreverse cases)))))))))))

(defmacro with-simple-restart ((restart-name format-string &rest format-args)
                               &body body
                               &aux (cluster (gensym)) (temp (make-symbol (symbol-name restart-name))))
  "(WITH-SIMPLE-RESTART (restart-name format-string format-arguments)
   body)
   If restart-name is not invoked, then all values returned by forms are
   returned. If control is transferred to this restart, it immediately
   returns the values NIL and T."
  (unless (and (stringp format-string)
               (null format-args)
               (not (%str-member #\~ (ensure-simple-string format-string))))
    (let ((stream (gensym)))
      (setq format-string `#'(lambda (,stream) (format ,stream ,format-string ,@format-args)))))
  `(let* ((,temp (%cons-restart ',restart-name
                                'simple-restart
                                ,format-string
                                nil
                                nil))
          (,cluster (list ,temp)))
     (declare (dynamic-extent ,temp ,cluster))
     (catch ,cluster
       (let ((%restarts% (cons ,cluster %restarts%)))
         (declare (dynamic-extent %restarts%))
         ,@body))))

;Like with-simple-restart but takes a pre-consed restart.  Not CL.
(defmacro with-restart (restart &body body &aux (cluster (gensym)))
  `(let* ((,cluster (list ,restart)))
     (declare (dynamic-extent ,cluster))
     (catch ,cluster
       (let ((%restarts% (cons ,cluster %restarts%)))
         (declare (dynamic-extent %restarts%))
         ,@body))))

(defmacro ignore-errors (&rest forms)
  "Execute FORMS handling ERROR conditions, returning the result of the last
  form, or (VALUES NIL the-ERROR-that-was-caught) if an ERROR was handled."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

(defmacro def-kernel-restart (&environment env errno name arglist &body body)
  (multiple-value-bind (body decls)
                       (parse-body body env)
    `(let* ((fn (nfunction ,name (lambda ,arglist ,@decls (block ,name ,@body))))
            (pair (assq ,errno ccl::*kernel-restarts*)))
       (if pair
         (rplacd pair fn)
         (push (cons ,errno fn) ccl::*kernel-restarts*))
       fn)))


;;; Setf.

;  If you change anything here, be sure to make the corresponding change
;  in get-setf-method.
(defmacro setf (&rest args &environment env)
  "Takes pairs of arguments like SETQ. The first is a place and the second
  is the value that is supposed to go into that place. Returns the last
  value. The place argument may be any of the access forms for which SETF
  knows a corresponding setting form."
  (let ((temp (length args))
        (accessor nil))
    (cond ((eq temp 2)
           (let* ((form (car args)) 
                  (value (cadr args)))
             ;This must match get-setf-method .
             (cond ((atom form)
                    (progn
                      (unless (symbolp form)(signal-program-error $XNotSym form))
                      `(setq ,form ,value)))
                   ((eq (car form) 'the)
                    (unless (eql (length form) 3)
                      (error "Bad THE place form in (SETF ~S ~S)" form value))
                    (destructuring-bind (type place) (cdr form)
                      `(setf ,place (the ,type ,value))))
                   (t
                    (multiple-value-bind (ftype local-p)
                        (function-information (setq accessor (car form)) ENV)
                      (if local-p
                        (if (eq ftype :function)
                                        ;Local function, so don't use global setf definitions.
                          (default-setf form value env)
                          `(setf ,(macroexpand-1 form env) ,value))
                        (cond
                          ((setq temp (%setf-method accessor))
                           (if (symbolp temp)
                             `(,temp ,@(cdar args) ,value)
                             (multiple-value-bind (dummies vals storevars setter #|getter|#)
                                 (funcall temp form env)
                               (do* ((d dummies (cdr d))
                                     (v vals (cdr v))
                                     (let-list nil))
                                    ((null d)
                                     (setq let-list (nreverse let-list))
                                     `(let* ,let-list
                                       (declare (ignorable ,@dummies))
                                       (multiple-value-bind ,storevars ,value
                                         #|,getter|#
                                         ,setter)))
                                 (push (list (car d) (car v)) let-list)))))
                          ((and (type-and-refinfo-p (setq temp (or (environment-structref-info accessor env)
                                                                   (and #-bccl (boundp '%structure-refs%)
                                                                        (gethash accessor %structure-refs%)))))
                                (not (refinfo-r/o (if (consp temp) (%cdr temp) temp))))
                           (if (consp temp)
                             ;; strip off type, but add in a require-type
                             (let ((type (%car temp)))
                               `(the ,type (setf ,(defstruct-ref-transform (%cdr temp) (%cdar args) env)
                                            (require-type ,value ',type))))
                             `(setf ,(defstruct-ref-transform temp (%cdar args) env)
                               ,value)))
                          (t
                           (multiple-value-bind (res win)
                               (macroexpand-1 form env)
                             (if win
                               `(setf ,res ,value)
                               (default-setf form value env)))))))))))
          ((oddp temp)
	   (signal-program-error "Odd number of args to SETF : ~s." args))
          (t (do* ((a args (cddr a)) (l nil))
                  ((null a) `(progn ,@(nreverse l)))
               (push `(setf ,(car a) ,(cadr a)) l))))))


(defun default-setf (setter value &optional env)
  (let* ((reader (car setter))
         (args (cdr setter))
         (gensyms (mapcar #'(lambda (sym) (declare (ignore sym)) (gensym)) args))
         types declares)
    (flet ((form-type (form)
             (nx-form-type form env)))
      (declare (dynamic-extent #'form-type))
      (setq types (mapcar #'form-type args)))
    (dolist (sym gensyms)
      (let ((sym-type (pop types)))
        (unless (eq sym-type t)
          (push `(type ,sym-type ,sym) declares))))
    `(let ,(mapcar #'list gensyms args)
       ,@(and declares (list `(declare ,@(nreverse declares))))
       (funcall #'(setf ,reader) ,value ,@gensyms))))

;; Establishing these setf-inverses is something that should
;; happen at compile-time
(defsetf elt set-elt)
(defsetf car set-car)
(defsetf %car set-%car)
(defsetf first set-car)
(defsetf cdr set-cdr)
(defsetf %cdr set-%cdr)
(defsetf rest set-cdr)
(defsetf uvref uvset)
(defsetf aref aset)
(defsetf svref svset)
(defsetf %svref %svset)
(defsetf char set-char)
(defsetf schar set-schar)
(defsetf %scharcode %set-scharcode)
(defsetf symbol-value set)
(defsetf symbol-plist set-symbol-plist)
(defsetf fill-pointer set-fill-pointer)

; This sux; it calls the compiler twice (once to shove the macro in the
; environment, once to dump it into the file.)
(defmacro defmacro  (name arglist &body body &environment env)
  (unless (symbolp name)(signal-program-error $XNotSym name))
  (unless (listp arglist) (signal-program-error "~S is not a list." arglist))
  (multiple-value-bind (lambda-form doc)
                       (parse-macro-1 name arglist body env)
    (let* ((normalized (normalize-lambda-list arglist t t))
           (body-pos (position '&body normalized))
           (argstring (let ((temp nil))
                        (dolist (arg normalized)
                          (if (eq arg '&aux)
                            (return)
                            (push arg temp)))
                        (format nil "~:a" (nreverse temp)))))
      (if (and body-pos (memq '&optional normalized)) (decf body-pos))
      `(progn
         (eval-when (:compile-toplevel)
           (define-compile-time-macro ',name ',lambda-form ',env))
         (eval-when (:load-toplevel :execute)
           (%macro 
            (nfunction ,name ,lambda-form)
            '(,doc ,body-pos . ,argstring))
           ',name)))))

(defmacro define-symbol-macro (name expansion &environment env)
  (unless (symbolp name)(signal-program-error $XNotSym name))
  `(progn
    (eval-when (:compile-toplevel)
      (define-compile-time-symbol-macro ',name ',expansion ',env))
    (eval-when (:load-toplevel :execute)
      (%define-symbol-macro ',name ',expansion))))

;; ---- allow inlining setf functions
(defmacro defun (spec args &body body &environment env &aux global-name inline-spec)
  "Define a function at top level."
  (validate-function-name spec)
  (setq args (require-type args 'list))
  (setq body (require-type body 'list))
  (multiple-value-bind (forms decls doc) (parse-body body env t)
    (cond ((symbolp spec)
           (setq global-name spec)
           (setq inline-spec spec)
           (setq body `(block ,spec ,@forms)))
          ((setf-function-name-p spec)
           (setq inline-spec spec)
           (setq body `(block ,(cadr spec) ,@forms)))
          (t (setq body `(progn ,@forms))))
    (let* ((lambda-expression `(lambda ,args 
                                ,@(if global-name
                                    `((declare (global-function-name ,global-name))))
                                ,@decls ,body))
           (info (if (and inline-spec
                          (or (null env)
                              (definition-environment env t))
                          (nx-declared-inline-p inline-spec env)
                          (not (and (symbolp inline-spec)
                                    (gethash inline-spec *NX1-ALPHATIZERS*))))
                   (cons doc lambda-expression)
                   doc)))
      `(progn
         (%defun (nfunction ,spec ,lambda-expression) ',info)
         ',spec))))

(defmacro %defvar-init (var initform doc)
  `(unless (%defvar ',var ,doc)
    (set ',var ,initform)))

(defmacro defvar (&environment env var &optional (value () value-p) doc)
  "Define a global variable at top level. Declare the variable
  SPECIAL and, optionally, initialize it. If the variable already has a
  value, the old value is not clobbered. The third argument is an optional
  documentation string for the variable."
  (if (and doc (not (stringp doc))) (report-bad-arg doc 'string))
  (if (and (compile-file-environment-p env) (not *fasl-save-doc-strings*))
    (setq doc nil))
 `(progn
    (eval-when (:compile-toplevel)
      (note-variable-info ',var ,value-p ,env))
    ,(if value-p
       `(%defvar-init ,var ,value ,doc)
       `(%defvar ',var))
    ',var))
         
(defmacro def-standard-initial-binding (name &optional (form name) (doc nil doc-p) &environment env)
  `(progn
    (eval-when (:compile-toplevel)
      (note-variable-info ',name t ,env))    
    (define-standard-initial-binding ',name #'(lambda () ,form))
    ,@(when doc-p
           `((set-documentation ',name 'variable ,doc)))
    ',name))

(defmacro defparameter (&environment env var value &optional doc)
  "Define a parameter that is not normally changed by the program,
  but that may be changed without causing an error. Declare the
  variable special and sets its value to VAL, overwriting any
  previous value. The third argument is an optional documentation
  string for the parameter."
  (if (and doc (not (stringp doc))) (signal-program-error "~S is not a string." doc))
  (if (and (compile-file-environment-p env) (not *fasl-save-doc-strings*))
    (setq doc nil))
  `(progn
     (eval-when (:compile-toplevel)
       (note-variable-info ',var t ,env))
     (%defparameter ',var ,value ,doc)))


(defmacro defstatic (&environment env var value &optional doc)
  "Syntax is like DEFPARAMETER.  Proclaims the symbol to be special,
but also asserts that it will never be given a per-thread dynamic
binding.  The value of the variable can be changed (via SETQ, etc.),
but since all threads access the same static binding of the variable,
such changes should be made with care."
  (if (and doc (not (stringp doc))) (signal-program-error "~S is not a string." doc))
  (if (and (compile-file-environment-p env) (not *fasl-save-doc-strings*))
    (setq doc nil))
  `(progn
     (eval-when (:compile-toplevel)
       (note-variable-info ',var :global ,env))
     (%defglobal ',var ,value ,doc)))

(defmacro defstaticvar (&environment env var value &optional doc)
  "Syntax is like DEFVAR.  Proclaims the symbol to be special,
but also asserts that it will never be given a per-thread dynamic
binding.  The value of the variable can be changed (via SETQ, etc.),
but since all threads access the same static binding of the variable,
such changes should be made with care.  Like DEFVAR, the initial value
form is not evaluated if the variable is already BOUNDP."
  (if (and doc (not (stringp doc))) (signal-program-error "~S is not a string." doc))
  (if (and (compile-file-environment-p env) (not *fasl-save-doc-strings*))
    (setq doc nil))
  `(progn
     (eval-when (:compile-toplevel)
       (note-variable-info ',var :global ,env))
      (%symbol-bits ',var (logior (ash 1 $sym_vbit_global) (the fixnum (%symbol-bits ',var))))
     (%defvar-init ,var ,value ,doc)))


(defmacro defglobal (&rest args)
  "Synonym for DEFSTATIC."
  `(defstatic ,@args))


(defmacro defloadvar (var value &optional doc)
  `(progn
     (defstaticvar ,var ,nil ,@(if doc `(,doc)))
     (def-ccl-pointers ,var ()
       (setq ,var ,value))
     ',var))




(defmacro qlfun (name args &body body)
  `(nfunction ,name (lambda ,args ,@body)))

(defmacro lfun-bits-known-function (f)
  (let* ((temp (gensym)))
    `(let* ((,temp (function-to-function-vector ,f)))
      (%svref ,temp (the fixnum (1- (the fixnum (uvsize ,temp))))))))

(defmacro lfunloop (for var in function &body loop-body)
  "Loop over immediates in function"
  (assert (and (or (equal (symbol-name for) "FOR") (equal (symbol-name for) "AS"))
               (equal (symbol-name in) "IN")))
  (let ((fn (gensym))
	(lfv (gensym))
	(i (gensym)))
    `(loop with ,fn = ,function
           with ,lfv = (function-to-function-vector ,fn)
           for ,i from #+ppc-target 1 #+x86-target (%function-code-words ,fn) below (%i- (uvsize  ,lfv) 1)
           as ,var = (%svref ,lfv ,i)
           ,@loop-body)))

(defmacro cond (&rest args &aux clause)
  (when args
     (setq clause (car args))
     (if (cdr clause)         
         `(if ,(car clause) (progn ,@(cdr clause)) (cond ,@(cdr args)))
       (if (cdr args) `(or ,(car clause) (cond ,@(cdr args)))
                      `(values ,(car clause))))))

(defmacro and (&rest args)
  "And Form*
AND evaluates each form in sequence, from left to right.  If any form
returns NIL, AND returns NIL; otherwise, AND returns the values(s) returned
by the last form.  If there are no forms, AND returns T."
  (if (null args) t
    (if (null (cdr args)) (car args)
      `(if ,(car args) (and ,@(cdr args))))))

(defmacro or (&rest args)
  "Or Form*
OR evaluates each Form, in sequence, from left to right.
If any Form but the last returns a non-NIL value, OR returns that
single value (without evaluating any subsequent Forms.)  If OR evaluates
the last Form, it returns all values returned by that Form.  If there
are no Forms, OR returns NIL."
  (if args
    (if (cdr args)
      (do* ((temp (gensym))
            (handle (list nil))
            (forms `(let ((,temp ,(pop args)))
                     (if ,temp ,temp ,@handle))))
           ((null (cdr args))
            (%rplaca handle (%car args))
            forms)
        (%rplaca handle `(if (setq ,temp ,(%car args)) 
                          ,temp 
                          ,@(setq handle (list nil))))
        (setq args (%cdr args)))
      (%car args))))

(defmacro case (key &body forms)
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If a singleton key is T then the clause is a default clause."
   (let ((key-var (gensym)))
     `(let ((,key-var ,key))
        (declare (ignorable ,key-var))
        (cond ,@(case-aux forms key-var nil nil)))))

(defmacro ccase (keyplace &body forms)
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then a correctable error is
  signalled."
  (let* ((key-var (gensym))
         (tag (gensym)))
    `(prog (,key-var)
       ,tag
       (setq ,key-var ,keyplace)
       (return (cond ,@(case-aux forms key-var tag keyplace))))))

(defmacro ecase (key &body forms)
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then an error is signalled."
  (let* ((key-var (gensym)))
    `(let ((,key-var ,key))
       (declare (ignorable ,key-var))
       (cond ,@(case-aux forms key-var 'ecase nil)))))
       
(defun case-aux (clauses key-var e-c-p placename &optional (used-keys (list (list '%case-core))))
  (if clauses
    (let* ((key-list (caar clauses))
           (stype (if e-c-p (if (eq e-c-p 'ecase) e-c-p 'ccase) 'case))
           (test (cond ((and (not e-c-p)
                             (or (eq key-list 't)
                                 (eq key-list 'otherwise)))
                        t)
                       (key-list
                        (cons 'or
                              (case-key-testers key-var used-keys key-list stype)))))
           (consequent-list (or (%cdar clauses) '(nil))))
      (if (eq test t)
        (progn
          (when (%cdr clauses) (warn "~s or ~s clause in the middle of a ~s statement.  Subsequent clauses ignored."
                                     't 'otherwise 'case))
          (cons (cons t consequent-list) nil))
        (cons (cons test consequent-list)
              (case-aux (%cdr clauses) key-var e-c-p placename used-keys))))
    (when e-c-p
      (setq used-keys `(member ,@(mapcar #'car (cdr used-keys))))
      (if (eq e-c-p 'ecase)
        `((t (values (%err-disp #.$XWRONGTYPE ,key-var ',used-keys))))
        `((t (setf ,placename (ensure-value-of-type ,key-var ',used-keys ',placename))
           (go ,e-c-p)))))))


;;; We don't want to descend list structure more than once (like this has
;;; been doing for the last 18 years or so.)
(defun case-key-testers (symbol used-keys atom-or-list statement-type &optional recursive)
  (if (or recursive (atom atom-or-list))
    (progn
      (if (assoc atom-or-list used-keys)
        (warn "Duplicate keyform ~s in ~s statement." atom-or-list statement-type)
        (setq used-keys (nconc used-keys (list (cons atom-or-list t)))))
      `((,(if (typep atom-or-list '(and number (not fixnum)))
              'eql
              'eq)
         ,symbol ',atom-or-list)))
    (nconc (case-key-testers symbol used-keys (car atom-or-list) statement-type t)
           (when (cdr atom-or-list)
             (case-key-testers symbol used-keys (%cdr atom-or-list) statement-type nil)))))


; generate the COND body of a {C,E}TYPECASE form
(defun typecase-aux (key-var clauses &optional e-c-p keyform)
  (let* ((construct (if e-c-p (if (eq e-c-p 'etypecase) e-c-p 'ctypecase) 'typecase))
         (types ())
         (body ())
         otherwise-seen-p)
    (flet ((bad-clause (c) 
             (signal-program-error "Invalid clause ~S in ~S form." c construct)))
      (dolist (clause clauses)
        (if (atom clause)
            (bad-clause clause))
        (if otherwise-seen-p
            (signal-program-error "OTHERWISE must be final clause in ~S form." construct))
        (destructuring-bind (typespec &body consequents) clause
          (when (eq construct 'typecase)
            (if (eq typespec 'otherwise)
                (progn (setq typespec t)
                       (setq otherwise-seen-p t))))
          (unless
              (dolist (already types nil)
                (when (subtypep typespec already)
                  (warn "Clause ~S ignored in ~S form - shadowed by ~S ." clause construct (assq already clauses))
                  (return t)))
            (push typespec types)
            (setq typespec `(typep ,key-var ',typespec))
            (push `(,typespec nil ,@consequents) body))))
      (when e-c-p
        (setq types `(or ,@(nreverse types)))
        (if (eq construct 'etypecase)
            (push `(t (values (%err-disp #.$XWRONGTYPE ,key-var ',types))) body)
            (push `(t (setf ,keyform (ensure-value-of-type  ,key-var ',types ',keyform))
                      (go ,e-c-p)) body))))
    `(cond ,@(nreverse body))))

(defmacro typecase (keyform &body clauses)
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (let ((key-var (gensym)))
    `(let ((,key-var ,keyform))
       (declare (ignorable ,key-var))
       ,(typecase-aux key-var clauses))))

(defmacro etypecase (keyform &body clauses)
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then an error is signalled."
  (let ((key-var (gensym)))
    `(let ((,key-var ,keyform))
       (declare (ignorable ,key-var))
       ,(typecase-aux key-var clauses 'etypecase))))

(defmacro ctypecase (keyplace &body clauses)
  "CTYPECASE Key {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then a correctable error is signalled."
  (let ((key-var (gensym))
        (tag (gensym)))
    `(prog (,key-var)
       ,tag
       (setq ,key-var ,keyplace)
       (return ,(typecase-aux key-var clauses tag keyplace)))))

(defmacro destructuring-bind (lambda-list expression &body body)
  "Bind the variables in LAMBDA-LIST to the contents of ARG-LIST."
  (multiple-value-bind (bindings decls)
      (%destructure-lambda-list  lambda-list expression nil nil)
    `(let* ,(nreverse bindings)
      ,@(when decls `((declare ,@decls)))
      ,@body)))

(defmacro make-destructure-state (tail whole lambda)
  `(%istruct 'destructure-state ,tail ,whole ,lambda))


; This is supposedly ANSI CL.
(defmacro lambda (&whole lambda-expression (&rest paramlist) &body body)
  (declare (ignore paramlist body))
  (unless (lambda-expression-p lambda-expression)
    (warn "Invalid lambda expression: ~s" lambda-expression))
  `(function ,lambda-expression))

; This isn't
(defmacro nlambda (name (&rest arglist) &body body)
  `(nfunction ,name (lambda ,arglist ,@body)))

(defmacro when (test &body body)
  "If the first argument is true, the rest of the forms are
  evaluated as a PROGN."
 `(if ,test
   (progn ,@body)))

(defmacro unless (test &body body)
  "If the first argument is not true, the rest of the forms are
  evaluated as a PROGN."
 `(if (not ,test)
   (progn ,@body)))

(defmacro return (&optional (form nil form-p))
  `(return-from nil ,@(if form-p `(,form))))

; since they use tagbody, while & until BOTH return NIL
(defmacro while (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
       (go ,testlab)
      ,toplab
      (progn ,@body)
      ,testlab
      (when ,test (go ,toplab)))))

(defmacro until (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
       (go ,testlab)
      ,toplab
      (progn ,@body)
      ,testlab
      (if (not ,test)
        (go ,toplab)))))

(defmacro psetq (&whole call &body pairs &environment env)
  "PSETQ {var value}*
   Set the variables to the values, like SETQ, except that assignments
   happen in parallel, i.e. no assignments take place until all the
   forms have been evaluated."
  (when pairs
   (if (evenp (length pairs))
     (do* ((l pairs (%cddr l))
           (sym (%car l) (%car l)))
          ((null l) (%pset pairs))
       (unless (symbolp sym) (report-bad-arg sym 'symbol))
       (when (nth-value 1 (macroexpand-1 sym env))
         (return `(psetf ,@pairs))))
     (signal-program-error "Uneven number of args in the call ~S" call))))

; generates body for psetq.
; "pairs" is a proper list whose length is not odd.
(defun %pset (pairs)
 (when pairs
   (let (vars vals gensyms let-list var val sets)
      (loop
        (setq var (pop pairs)
              val (pop pairs))
        (if (null pairs) (return))
        (push var vars)
        (push val vals)
        (push (gensym) gensyms))
      (dolist (g gensyms)
        (push g sets)
        (push (pop vars) sets)
        (push (list g (pop vals)) let-list))
      (push val sets)
      (push var sets)
      `(progn
         (let ,let-list
           (setq ,@sets))
         nil))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun do-loop (binder setter env var-init-steps end-test result body)
  (let ((toptag (gensym))
        (testtag (gensym)))
    (multiple-value-bind (forms decls) (parse-body body env nil)
      `(block nil
         (,binder ,(do-let-vars var-init-steps)
                  ,@decls
                  (tagbody ; crocks-r-us.
                    (go ,testtag)
                    ,toptag
                    (tagbody
                      ,@forms)
                    (,setter ,@(do-step-vars var-init-steps))
                    ,testtag
                    (unless ,end-test
                      (go ,toptag)))
                  ,@result)))))
)

(defmacro do (&environment env var-init-steps (&optional end-test &rest result) &body body)
  "DO ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized in parallel to the value of the
  specified Init form. On subsequent iterations, the Vars are assigned the
  value of the Step form (if any) in parallel. The Test is evaluated before
  each evaluation of the body Forms. When the Test is true, the Exit-Forms
  are evaluated as a PROGN, with the result being the value of the DO. A block
  named NIL is established around the entire expansion, allowing RETURN to be
  used as an alternate exit mechanism."
  (do-loop 'let 'psetq env var-init-steps end-test result body))

(defmacro do* (&environment env var-init-steps (&optional end-test &rest result) &body body)
  "DO* ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized sequentially (like LET*) to the
  value of the specified Init form. On subsequent iterations, the Vars are
  sequentially assigned the value of the Step form (if any). The Test is
  evaluated before each evaluation of the body Forms. When the Test is true,
  the Exit-Forms are evaluated as a PROGN, with the result being the value
  of the DO. A block named NIL is established around the entire expansion,
  allowing RETURN to be used as an laternate exit mechanism."
  (do-loop 'let* 'setq env var-init-steps end-test result body))


(defun do-let-vars (var-init-steps)
  (if var-init-steps
      (cons (list (do-let-vars-var (car var-init-steps))
                  (do-let-vars-init (car var-init-steps)))
             (do-let-vars (cdr var-init-steps)))))

(defun do-let-vars-var (var-init-step)
  (if (consp var-init-step)
       (car var-init-step)
       var-init-step))

(defun do-let-vars-init (var-init-step)
   (if (consp var-init-step)
        (cadr var-init-step)
        nil))

(defun do-step-vars (var-init-steps)
    (if var-init-steps
        (if (do-step-vars-step? (car var-init-steps))
             (append (list (do-let-vars-var (car var-init-steps))
                           (do-step-vars-step (car var-init-steps)))
                     (do-step-vars (cdr var-init-steps)))
             (do-step-vars (cdr var-init-steps)))))

(defun do-step-vars-step? (var-init-step)
  (if (consp var-init-step)
       (cddr var-init-step)))

(defun do-step-vars-step (var-init-step)
  (if (consp var-init-step)
       (caddr var-init-step)))


(defmacro dotimes ((i n &optional result) &body body &environment env)
  (multiple-value-bind (forms decls)
                       (parse-body body env)
    (if (not (symbolp i))(signal-program-error $Xnotsym i))
    (let* ((toptag (gensym))
           (limit (gensym)))
      `(block nil
        (let ((,limit ,n) (,i 0))
         ,@decls
         (declare (unsettable ,i))
           (if (int>0-p ,limit)
             (tagbody
               ,toptag
               ,@forms
               (locally
                (declare (settable ,i))
                (setq ,i (1+ ,i)))
               (unless (eql ,i ,limit) (go ,toptag))))
           ,result)))))
  
(defun do-syms-result (var resultform)
  (unless (eq var resultform)
    (if (and (consp resultform) (not (quoted-form-p resultform)))
      `(progn (setq ,var nil) ,resultform)
      resultform)))

(defun expand-package-iteration-macro (iteration-function var pkg-spec resultform body env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    (let* ((ftemp (gensym))
           (vtemp (gensym))
           (ptemp (gensym))
           (result (do-syms-result var resultform)))
      `(block nil
        (let* ((,var nil)
               (,ptemp ,pkg-spec))
          ,@decls
           (flet ((,ftemp (,vtemp) (declare (debugging-function-name nil)) (setq ,var ,vtemp) (tagbody ,@body)))
             (declare (dynamic-extent #',ftemp))
             (,iteration-function ,ptemp #',ftemp))
           ,@(when result `(,result)))))))

(defmacro do-symbols ((var &optional pkg result) &body body &environment env)
  "DO-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs at least once for each symbol accessible in the given
   PACKAGE with VAR bound to the current symbol."
  (expand-package-iteration-macro 'iterate-over-accessable-symbols var pkg result body env))

(defmacro do-present-symbols ((var &optional pkg result) &body body &environment env)
  (expand-package-iteration-macro 'iterate-over-present-symbols var pkg result body env))

(defmacro do-external-symbols ((var &optional pkg result) &body body &environment env)
  "DO-EXTERNAL-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECL}* {TAG | FORM}*
   Executes the FORMs once for each external symbol in the given PACKAGE with
   VAR bound to the current symbol."
  (expand-package-iteration-macro 'iterate-over-external-symbols var pkg result body env))

(defmacro do-all-symbols ((var &optional resultform)
                          &body body &environment env)
  "DO-ALL-SYMBOLS (VAR [RESULT-FORM]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs once for each symbol in every package with VAR bound
   to the current symbol."
  (multiple-value-bind (body decls) (parse-body body env nil)
    (let* ((ftemp (gensym))
           (vtemp (gensym))
           (result (do-syms-result var resultform)))
      `(block nil
        (let* ((,var nil))
         ,@decls
           (flet ((,ftemp (,vtemp) (declare (debugging-function-name nil)) (setq ,var ,vtemp) (tagbody ,@body)))
             (declare (dynamic-extent #',ftemp))
             (iterate-over-all-symbols #',ftemp))
           ,@(when result `(,result)))))))

(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))




(defmacro %i> (x y)
  `(> (the fixnum ,x) (the fixnum ,y)))

(defmacro %i< (x y)
  `(< (the fixnum ,x) (the fixnum ,y)))

(defmacro %i<= (x y)
 `(not (%i> ,x ,y)))

(defmacro %i>= (x y)
 `(not (%i< ,x ,y)))

(defmacro bitset (bit number)
  `(logior (ash 1 ,bit) ,number))

(defmacro bitclr (bit number)
  `(logand (lognot (ash 1 ,bit)) ,number))

(defmacro bitopf ((op bit place) &environment env)
  (multiple-value-bind (vars vals stores store-form access-form)
                       (get-setf-method place env)
    (let* ((constant-bit-p (constantp bit))
           (bitvar (if constant-bit-p bit (gensym))))
      `(let ,(unless constant-bit-p `((,bitvar ,bit)))          ; compiler isn't smart enough
         (let* ,(mapcar #'list `(,@vars ,@stores) `(,@vals (,op ,bitvar ,access-form)))
           ,store-form)))))

(defmacro bitsetf (bit place)
  `(bitopf (bitset ,bit ,place)))

(defmacro bitclrf (bit place)
  `(bitopf (bitclr ,bit ,place)))

(defmacro %svref (v i)
  (let* ((vtemp (make-symbol "VECTOR"))
           (itemp (make-symbol "INDEX")))
      `(let* ((,vtemp ,v)
              (,itemp ,i))
         (locally (declare (optimize (speed 3) (safety 0)))
           (svref ,vtemp ,itemp)))))

(defmacro %svset (v i new)
  (let* ((vtemp (make-symbol "VECTOR"))
         (itemp (make-symbol "INDEX"))
         (ntemp (make-symbol "NEW")))
    `(let* ((,vtemp ,v)
            (,itemp ,i)
            (,ntemp ,new))
      (locally (declare (optimize (speed 3) (safety 0)))
        (setf (svref ,vtemp ,itemp) ,ntemp)))))


(defmacro %schar (v i)
  (let* ((vtemp (make-symbol "STRING"))
         (itemp (make-symbol "INDEX")))
    `(let* ((,vtemp ,v)
            (,itemp ,i))
       (locally (declare (optimize (speed 3) (safety 0)))
         (schar ,vtemp ,itemp)))))

(defmacro %set-schar (v i new)
  (let* ((vtemp (make-symbol "STRING"))
         (itemp (make-symbol "INDEX"))
         (ntemp (make-symbol "NEW")))
      `(let* ((,vtemp ,v)
              (,itemp ,i)
              (,ntemp ,new))
         (locally (declare (optimize (speed 3) (safety 0)))
           (setf (schar ,vtemp ,itemp) ,ntemp)))))



(defmacro %char-code (c) `(char-code (the character ,c)))
(defmacro %code-char (i) `(code-char (the (mod 256) ,i)))

(defmacro %izerop (x) `(eq ,x 0))
(defmacro %iminusp (x) `(< (the fixnum ,x) 0))
(defmacro %i+ (&rest (&optional (n0 0) &rest others))
  (if others
    `(the fixnum (+ (the fixnum ,n0) (%i+ ,@others)))
    `(the fixnum ,n0)))
(defmacro %i- (x y &rest others) 
  (if (not others)
    `(the fixnum (- (the fixnum ,x) (the fixnum ,y)))
    `(the fixnum (- (the fixnum ,x) (the fixnum (%i+ ,y ,@others))))))


(defmacro %i* (x y) `(the fixnum (* (the fixnum ,x) (the fixnum ,y))))

(defmacro %ilogbitp (b i)
  (target-word-size-case
   (32
    `(logbitp (the (integer 0 29) ,b) (the fixnum ,i)))
   (64
    `(logbitp (the (integer 0 60) ,b) (the fixnum ,i)))))

;;; Seq-Dispatch does an efficient type-dispatch on the given Sequence.

(defmacro seq-dispatch (sequence list-form array-form)
  `(if (sequence-type ,sequence)
       ,list-form
       ,array-form))


(defsetf %get-byte %set-byte)
(defsetf %get-unsigned-byte %set-unsigned-byte)
(defsetf %get-signed-byte %set-byte)
(defsetf %get-word %set-word)
(defsetf %get-signed-word %set-word)
(defsetf %get-unsigned-word %set-unsigned-word)
(defsetf %get-long %set-long)
(defsetf %get-signed-long %set-long)
(defsetf %get-unsigned-long %set-unsigned-long)
(defsetf %get-full-long %set-long)
(defsetf %get-point %set-long)
(defsetf %get-ptr %set-ptr)
(defsetf %get-double-float %set-double-float)
(defsetf %get-single-float %set-single-float)
(defsetf %get-bit %set-bit)
(defsetf %get-unsigned-long-long %set-unsigned-long-long)
(defsetf %%get-unsigned-longlong %%set-unsigned-longlong)
(defsetf %get-signed-long-long %set-signed-long-long)
(defsetf %%get-signed-longlong %%set-signed-longlong)
(defsetf %get-bitfield %set-bitfield)

(defmacro %ilognot (int) `(%i- -1 ,int))

(defmacro %ilogior2 (x y) 
  `(logior (the fixnum ,x) (the fixnum ,y)))

(defmacro %ilogior (body &rest args)
   (while args
     (setq body (list '%ilogior2 body (pop args))))
   body)

(defmacro %ilogand2 (x y)
  `(logand (the fixnum ,x) (the fixnum ,y)))

(defmacro %ilogand (body &body args)
   (while args
     (setq body (list '%ilogand2 body (pop args))))
   body)

(defmacro %ilogxor2 (x y)
  `(logxor (the fixnum ,x) (the fixnum ,y)))

(defmacro %ilogxor (body &body args)
   (while args
     (setq body (list '%ilogxor2 body (pop args))))
   body)

(defmacro with-macptrs (varlist &rest body &environment env)
  (multiple-value-bind (body other-decls) (parse-body body env)
    (collect ((temp-bindings)
              (temp-decls)
              (bindings)
              (our-decls)
              (inits))
      (dolist (var varlist)
        (let* ((temp (gensym)))
          (temp-decls temp)
        (if (consp var)
          (progn
            (our-decls (car var))
            (temp-bindings `(,temp (%null-ptr)))
            (bindings `(,(car var) ,temp))
            (if (cdr var)
              (inits `(%setf-macptr ,temp ,@(cdr var)))))
          (progn
            (our-decls var)
            (temp-bindings  `(,temp  (%null-ptr)))
            (bindings `(,var ,temp))))))
  `(let* ,(temp-bindings)
    (declare (dynamic-extent ,@(temp-decls)))
    (declare (type macptr ,@(temp-decls)))
    ,@(inits)
    (let* ,(bindings)
      (declare (type macptr ,@(our-decls)))
      ,@other-decls
      ,@body)))))


(defmacro with-loading-file (filename &rest body)
   `(let ((*loading-files* (cons ,filename (locally (declare (special *loading-files*))
                                                    *loading-files*))))
      (declare (special *loading-files*))
      ,@body))

(defmacro with-input-from-string ((var string &key index start end) &body forms &environment env)
  "Create an input string stream, provide an opportunity to perform
operations on the stream (returning zero or more values), and then close
the string stream.

STRING is evaluated first, and VAR is bound to a character input string
stream that supplies characters from the subsequence of the resulting
string bounded by start and end. BODY is executed as an implicit progn."
  (multiple-value-bind (forms decls) (parse-body forms env nil)
    `(let ((,var
	    ,(cond ((null end)
		    `(make-string-input-stream ,string ,(or start 0)))
		   ((symbolp end)
		    `(if ,end
		      (make-string-input-stream ,string ,(or start 0) ,end)
		      (make-string-input-stream ,string ,(or start 0))))
		   (t
		    `(make-string-input-stream ,string ,(or start 0) ,end)))))
      ,@decls
      (unwind-protect
           (multiple-value-prog1
               (progn ,@forms)
             ,@(if index `((setf ,index (string-input-stream-index ,var)))))
        (close ,var)))))

(defmacro with-output-to-string ((var &optional string &key (element-type 'base-char element-type-p))
                                 &body body 
                                 &environment env)
  "Create a character output stream, perform a series of operations that
may send results to this stream, and then close the stream.  BODY is
executed as an implicit progn with VAR bound to an output string stream.
All output to that string stream is saved in a string."
  (let ((string-var (gensym "string")))
    (multiple-value-bind (forms decls) (parse-body body env nil)
      `(let* ((,string-var ,string)
              (,var (if ,string-var
                      ,@(if element-type-p
                            `((progn
                                ,element-type
                                (%make-string-output-stream ,string-var)))
                            `((%make-string-output-stream ,string-var)))
                      ,@(if element-type-p
                            `((make-string-output-stream :element-type ,element-type))
                            `((make-string-output-stream))))))
         ,@decls
         (unwind-protect
              (progn
                ,@forms
                ,@(if string () `((get-output-stream-string ,var))))
           (close ,var))))))

(defmacro with-output-to-truncating-string-stream ((var len) &body body
						   &environment env)
  (multiple-value-bind (forms decls) (parse-body body env nil)
    `(let* ((,var (make-truncating-string-stream ,len)))
      ,@decls
      (unwind-protect
	   (progn
	     ,@forms
	     (values (get-output-stream-string ,var)
		     (slot-value ,var 'truncated)))
	(close ,var)))))

(defmacro with-open-file ((var filename . args) &body body &aux (stream (gensym))(done (gensym)))
  "Use open to create a file stream to file named by filename. Filename is
the name of the file to be opened. Options are used as keyword arguments
to open."
  `(let (,stream ,done)
     (unwind-protect
       (multiple-value-prog1
         (let ((,var (setq ,stream (open ,filename ,@args))))
           ,@body)
         (setq ,done t))
       (when ,stream (close ,stream :abort (null ,done))))))

(defmacro with-compilation-unit ((&key override) &body body)
  "WITH-COMPILATION-UNIT ({Key Value}*) Form*
  This form affects compilations that take place within its dynamic extent. It
  is intended to be wrapped around the compilation of all files in the same
  system. These keywords are defined:
    :OVERRIDE Boolean-Form
        One of the effects of this form is to delay undefined warnings
        until the end of the form, instead of giving them at the end of each
        compilation. If OVERRIDE is NIL (the default), then the outermost
        WITH-COMPILATION-UNIT form grabs the undefined warnings. Specifying
        OVERRIDE true causes that form to grab any enclosed warnings, even if
        it is enclosed by another WITH-COMPILATION-UNIT."
  `(flet ((with-compilation-unit-body ()
            ,@body))
     (declare (dynamic-extent #'with-compilation-unit-body))
     (call-with-compilation-unit #'with-compilation-unit-body :override ,override)))

; Yow! Another Done Fun.
(defmacro with-standard-io-syntax (&body body &environment env)
  "Bind the reader and printer control variables to values that enable READ
   to reliably read the results of PRINT. These values are:
       *PACKAGE*                        the COMMON-LISP-USER package
       *PRINT-ARRAY*                    T
       *PRINT-BASE*                     10
       *PRINT-CASE*                     :UPCASE
       *PRINT-CIRCLE*                   NIL
       *PRINT-ESCAPE*                   T
       *PRINT-GENSYM*                   T
       *PRINT-LENGTH*                   NIL
       *PRINT-LEVEL*                    NIL
       *PRINT-LINES*                    NIL
       *PRINT-MISER-WIDTH*              NIL
       *PRINT-PRETTY*                   NIL
       *PRINT-RADIX*                    NIL
       *PRINT-READABLY*                 T
       *PRINT-RIGHT-MARGIN*             NIL
       *READ-BASE*                      10
       *READ-DEFAULT-FLOAT-FORMAT*      SINGLE-FLOAT
       *READ-EVAL*                      T
       *READ-SUPPRESS*                  NIL
       *READTABLE*                      the standard readtable"
  (multiple-value-bind (decls body) (parse-body body env)
    `(let ((*package* (pkg-arg "COMMON-LISP-USER"))
           (*print-array* t)
           (*print-base* 10.)
           (*print-case* :upcase)
           (*print-circle* nil)
           (*print-escape* t)
           (*print-gensym* t)
           (*print-length* nil)
           (*print-level* nil)
           (*print-lines* nil) ; This doesn't exist as of 5/15/90 - does now
           (*print-miser-width* nil)
           (*print-pprint-dispatch* nil)
           (*print-pretty* nil)
           (*print-radix* nil)
           (*print-readably* t)
           (*print-right-margin* nil)
           (*read-base* 10.)
           (*read-default-float-format* 'single-float)
           (*read-eval* t) ; Also MIA as of 5/15/90
           (*read-suppress* nil)
           (*readtable* %initial-readtable%)
	   ; ccl extensions (see l1-io.lisp)
	   (*print-abbreviate-quote* t)
	   (*print-structure* t)
	   (*print-simple-vector* nil)
	   (*print-simple-bit-vector* nil)
	   (*print-string-length* nil))
       ,@decls
       ,@body)))

(defmacro with-self-bound-io-control-vars (&body body)
  `(let (
         (*print-array* *print-array*)
         (*print-base* *print-base*)
         (*print-case* *print-case*)
         (*print-circle* *print-circle*)
         (*print-escape* *print-escape*)
         (*print-gensym* *print-gensym*)
         (*print-length* *print-length*)
         (*print-level* *print-level*)
         (*print-lines* *print-lines*)
         (*print-miser-width* *print-miser-width*)
         (*print-pprint-dispatch* *print-pprint-dispatch*)
         (*print-pretty* *print-pretty*)
         (*print-radix* *print-radix*)
         (*print-readably* *print-readably*)
         (*print-right-margin* *print-right-margin*)
         (*read-base* *read-base*)
         (*read-default-float-format* *read-default-float-format*)
         (*read-eval* *read-eval*)
         (*read-suppress* *read-suppress*)
         (*readtable* *readtable*))
     ,@body))

(defmacro print-unreadable-object (&environment env (object stream &key type identity) &body forms)
  "Output OBJECT to STREAM with \"#<\" prefix, \">\" suffix, optionally
  with object-type prefix and object-identity suffix, and executing the
  code in BODY to provide possible further output."
  (multiple-value-bind (body decls) (parse-body forms env)
    (if body
      (let ((thunk (gensym)))
        `(let ((,thunk #'(lambda () ,@decls ,@body)))
           (declare (dynamic-extent ,thunk))
          (%print-unreadable-object ,object ,stream ,type ,identity ,thunk)))
      `(%print-unreadable-object ,object ,stream ,type ,identity nil))))
;; Pointers and Handles

;;Add function to lisp system pointer functions, and run it if it's not already
;; there.
(defmacro def-ccl-pointers (name arglist &body body &aux (old (gensym)))
  `(flet ((,name ,arglist ,@body))
     (let ((,old (member ',name *lisp-system-pointer-functions* :key #'function-name)))
       (if ,old
         (rplaca ,old #',name)
         (progn
           (push #',name *lisp-system-pointer-functions*)
           (,name))))))

(defmacro def-load-pointers (name arglist &body body &aux (old (gensym)))
  `(flet ((,name ,arglist ,@body))
     (let ((,old (member ',name *lisp-user-pointer-functions* :key #'function-name)))
       (if ,old
         (rplaca ,old #',name)
         (progn
           (push #',name *lisp-user-pointer-functions*)
           (,name))))))

;Queue up some code to run after ccl all loaded up, or, if ccl is already
;loaded up, just run it right now.
(defmacro queue-fixup (&rest body &aux (fn (gensym)))
  `(let ((,fn #'(lambda () ,@body)))
     (if (eq %lisp-system-fixups% T)
       (funcall ,fn)
       (push (cons ,fn (or *loading-toplevel-location* *loading-file-source-file*)) %lisp-system-fixups%))))

(defmacro %incf-ptr (p &optional (by 1))
  (if (symbolp p)  ;once-only
    `(%setf-macptr (the macptr ,p) (%inc-ptr ,p ,by))
    (let ((var (gensym)))
      `(let ((,var ,p)) (%setf-macptr (the macptr ,var) (%inc-ptr ,var ,by))))))

(defmacro with-string-from-cstring ((s ptr) &body body)
  (let* ((len (gensym))
	 (p (gensym)))
    `(let* ((,p ,ptr)
	    (,len (%cstrlen ,p))
	    (,s (make-string ,len)))
      (declare (fixnum ,len))
      (%copy-ptr-to-ivector ,p 0 ,s 0 ,len)
      (locally
	  ,@body))))


(defmacro with-cstr ((sym str &optional start end) &rest body &environment env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    (if (and (base-string-p str) (null start) (null end))
      (let ((strlen (%i+ (length str) 1)))
        `(%stack-block ((,sym ,strlen))
           ,@decls
           (%cstr-pointer ,str ,sym)
           ,@body))
      (let ((strname (gensym))
            (start-name (gensym))
            (end-name (gensym)))
        `(let ((,strname ,str)
               ,@(if (or start end)
                   `((,start-name ,(or start 0))
                     (,end-name ,(or end `(length ,strname))))))
           (%vstack-block (,sym
                           (the fixnum
                             (1+
                              (the fixnum
                                ,(if (or start end)
                                     `(byte-length
                                       ,strname ,start-name ,end-name)
                                     `(length ,strname))))))
             ,@decls
             ,(if (or start end)
                `(%cstr-segment-pointer ,strname ,sym ,start-name ,end-name)
                `(%cstr-pointer ,strname ,sym))
             ,@body))))))

(defmacro with-utf-8-cstr ((sym str) &body body)
  (let* ((data (gensym))
         (offset (gensym))
         (string (gensym))
         (len (gensym))
         (noctets (gensym))
         (end (gensym)))
    `(let* ((,string ,str)
            (,len (length ,string)))
      (multiple-value-bind (,data ,offset) (array-data-and-offset ,string)
        (let* ((,end (+ ,offset ,len))
               (,noctets (utf-8-octets-in-string ,data ,offset ,end)))
          (%stack-block ((,sym (1+ ,noctets)))
            (utf-8-memory-encode ,data ,sym 0 ,offset ,end)
            (setf (%get-unsigned-byte ,sym ,noctets) 0)
            ,@body))))))



(defmacro with-native-utf-16-cstr ((sym str) &body body)
  (let* ((data (gensym))
         (offset (gensym))
         (string (gensym))
         (len (gensym))
         (noctets (gensym))
         (end (gensym)))
    `(let* ((,string ,str)
            (,len (length ,string)))
      (multiple-value-bind (,data ,offset) (array-data-and-offset ,string)
        (let* ((,end (+ ,offset ,len))
               (,noctets (utf-16-octets-in-string ,data ,offset ,end)))
          (%stack-block ((,sym (1+ ,noctets)))
            (native-utf-16-memory-encode ,data ,sym 0 ,offset ,end)
            (setf (%get-unsigned-word ,sym ,noctets) 0)
            ,@body))))))

(defmacro with-pointers (speclist &body body)
   (with-specs-aux 'with-pointer speclist body))



(defmacro with-cstrs (speclist &body body)
   (with-specs-aux 'with-cstr speclist body))

(defmacro with-utf-8-cstrs (speclist &body body)
   (with-specs-aux 'with-utf-8-cstr speclist body))

(defmacro with-native-utf-16-cstrs (speclist &body body)
  (with-specs-aux 'with-native-utf-16-cstr speclist body))

(defmacro with-encoded-cstr ((encoding-name (sym string &optional start end))
                             &rest body &environment env)
  (let* ((encoding (gensym))
         (str (gensym)))
      (multiple-value-bind (body decls) (parse-body body env nil)
        `(let* ((,str ,string)
                (,encoding (get-character-encoding ,encoding-name)))
          (%stack-block ((,sym (cstring-encoded-length-in-bytes ,encoding ,str ,start ,end) :clear t))
            ,@decls
            (encode-string-to-memory ,encoding ,sym 0 ,str ,start ,end)
            ,@body)))))

(defmacro with-encoded-cstrs (encoding-name bindings &body body)
  (with-specs-aux 'with-encoded-cstr (mapcar #'(lambda (b)
                                                 `(,encoding-name ,b))
                                             bindings) body))

(defmacro with-filename-cstrs (&rest rest)
  (case (target-os-name)
    (:darwin `(with-utf-8-cstrs ,@rest))
    (:windows `(with-native-utf-16-cstrs ,@rest))
    (t `(with-encoded-cstrs (pathname-encoding-name) ,@rest))))


(defun with-specs-aux (name spec-list original-body)
  (multiple-value-bind (body decls) (parse-body original-body nil)
    (when decls (signal-program-error "declarations not allowed in ~s" original-body))
    (setq body (cons 'progn body))
    (dolist (spec (reverse spec-list))
      (setq body (list name spec body)))
    body))


(defmacro type-predicate (type)
  `(get-type-predicate ,type))

(defsetf type-predicate set-type-predicate)

(defun adjust-defmethod-lambda-list (ll)
  ;; If the lambda list contains &key, ensure that it also contains
  ;; &allow-other-keys
  (if (or (not (memq '&key ll))
          (memq '&allow-other-keys ll))
    ll
    (if (memq '&aux ll)
      (let* ((ll (copy-list ll))
             (aux (memq '&aux ll)))
        (setf (car aux) '&allow-other-keys
              (cdr aux) (cons '&aux (cdr aux)))
        ll)
      (append ll '(&allow-other-keys)))))

(defun encode-gf-lambda-list (lambda-list)
  (let* ((bits (encode-lambda-list lambda-list)))
    (declare (fixnum bits))
    (if (logbitp $lfbits-keys-bit bits)
      (logior bits (ash 1 $lfbits-aok-bit))
      bits)))

(defmacro defmethod (name &rest args &environment env)
  (multiple-value-bind (function-form specializers-form qualifiers lambda-list documentation specializers)
      (parse-defmethod name args env)
    `(progn
       (eval-when (:compile-toplevel)
         (record-function-info ',(maybe-setf-function-name name)
                               ',(%cons-def-info 'defmethod (encode-gf-lambda-list lambda-list) nil nil
                                                 specializers qualifiers)
                               ,env))
       (compiler-let ((*nx-method-warning-name* '(,name ,@qualifiers ,specializers)))
         (ensure-method ',name ,specializers-form
                        :function ,function-form
                        :qualifiers ',qualifiers
                        :lambda-list ',lambda-list
                        ,@(if documentation `(:documentation ,documentation)))))))


(defun seperate-defmethod-decls (decls)
  (let (outer inner)
    (dolist (decl decls)
      (if (neq (car decl) 'declare)
        (push decl outer)
        (let (outer-list inner-list)
          (dolist (d (cdr decl))
            (if (and (listp d) (eq (car d) 'dynamic-extent))
              (let (in out)
                (dolist (fspec (cdr d))
                  (if (and (listp fspec)
                           (eq (car fspec) 'function)
                           (listp (cdr fspec))
                           (null (cddr fspec))
                           (memq (cadr fspec) '(call-next-method next-method-p)))
                    (push fspec in)
                    (push fspec out)))
                (when out
                  (push `(dynamic-extent ,@(nreverse out)) outer-list))
                (when in
                  (push `(dynamic-extent ,@(nreverse in)) inner-list)))
              (push d outer-list)))
          (when outer-list
            (push `(declare ,@(nreverse outer-list)) outer))
          (when inner-list
            (push `(declare ,@(nreverse inner-list)) inner)))))
    (values (nreverse outer) (nreverse inner))))
		   

(defvar *warn-about-unreferenced-required-args-in-methods* #+ccl-0711 nil #-ccl-0711 T)

(defun parse-defmethod (name args env)
  (validate-function-name name)
  (let (qualifiers lambda-list parameters specializers specializers-form refs types temp)
    (until (listp (car args))
      (push (pop args) qualifiers))
    (setq lambda-list (pop args))
    (while (and lambda-list (not (memq (car lambda-list) lambda-list-keywords)))
      (let ((p (pop lambda-list)))
        (cond ((consp p)
               (unless (and (consp (%cdr p)) (null (%cddr p)))
                 (signal-program-error "Illegal arg ~S" p))
               (push (%car p) parameters)
               (push (%car p) refs)
               (setq p (%cadr p))
               (cond ((and (consp p) (eq (%car p) 'eql)
                           (consp (%cdr p)) (null (%cddr p)))
                      (push `(list 'eql ,(%cadr p)) specializers-form)
                      (push p specializers))
                     ((or (setq temp (non-nil-symbol-p p))
                          (specializer-p p))
                      (push `',p specializers-form)
                      (push p specializers)
                      (unless (or (eq p t) (not temp))
                        ;Should be `(guaranteed-type ...).
                        (push `(type ,p ,(%car parameters)) types)))
                     (t (signal-program-error "Illegal arg ~S" p))))
              (t
               (push p parameters)
               (unless *warn-about-unreferenced-required-args-in-methods*
                 (push p refs))
               (push t specializers-form)
               (push t specializers)))))
    (setq lambda-list (nreconc parameters lambda-list))
    (multiple-value-bind (body decls doc) (parse-body args env t)
      (multiple-value-bind (outer-decls inner-decls) 
                           (seperate-defmethod-decls decls)
        (let* ((methvar (make-symbol "NEXT-METHOD-CONTEXT"))
               (cnm-args (gensym))
               (lambda-form `(lambda ,(list* '&method methvar lambda-list)
                               (declare ;,@types
                                (ignorable ,@refs))
                               ,@outer-decls
                               (block ,(if (consp name) (cadr name) name)
                                 (flet ((call-next-method (&rest ,cnm-args)
                                          (declare (dynamic-extent ,cnm-args))
                                          (if ,cnm-args
                                            (apply #'%call-next-method-with-args ,methvar ,cnm-args)
                                            (%call-next-method ,methvar)))
                                        (next-method-p () (%next-method-p ,methvar)))
                                   (declare (inline call-next-method next-method-p))
                                   ,@inner-decls
                                   ,@body)))))
          (values
           (if name `(nfunction ,name ,lambda-form) `(function ,lambda-form))
           `(list ,@(nreverse specializers-form))
           (nreverse qualifiers)
	   lambda-list
           doc
           (nreverse specializers)))))))

(defmacro anonymous-method (name &rest args &environment env)
  (multiple-value-bind (function-form specializers-form qualifiers method-class documentation)
                       (parse-defmethod name args env)
    
    `(%anonymous-method
      ,function-form
      ,specializers-form
      ',qualifiers
      ,@(if (or method-class documentation) `(',method-class))
      ,@(if documentation `(,documentation)))))



(defmacro defclass (class-name superclasses slots &rest class-options &environment env)
  (flet ((duplicate-options (where) (signal-program-error "Duplicate options in ~S" where))
         (illegal-option (option) (signal-program-error "Illegal option ~s" option))
         (make-initfunction (form)
           (cond ((or (eq form 't)
                      (equal form ''t))
                  '(function true))
                 ((or (eq form 'nil)
                      (equal form ''nil))
                  '(function false))
                 (t
                  `(function (lambda () ,form))))))
    (setq class-name (require-type class-name '(and symbol (not null))))
    (setq superclasses (mapcar #'(lambda (s) (require-type s 'symbol)) superclasses))
    (let* ((options-seen ())
           (signatures ())
           (slot-names ())
           (slot-initargs ()))
      (flet ((canonicalize-defclass-option (option)
               (let* ((option-name (car option)))
                 (if (member option-name options-seen :test #'eq)
                   (duplicate-options class-options)
                   (push option-name options-seen))
                 (case option-name
                   (:default-initargs
                       (let ((canonical ())
                             (initargs-seen ()))
                         (let (key val (tail (cdr option)))
                           (loop (when (null tail) (return nil))
                              (setq key (pop tail)
                                    val (pop tail))
                              (when (memq key initargs-seen)
                                (SIGNAL-PROGRAM-error "Duplicate initialization argument name ~S in :DEFAULT-INITARGS of DEFCLASS ~S" key class-name))
                              (push key initargs-seen)
                              (push ``(,',key ,',val  ,,(make-initfunction val)) canonical))
                           `(':direct-default-initargs (list ,@(nreverse canonical))))))
                   (:metaclass
                    (unless (and (cadr option)
                                 (typep (cadr option) 'symbol))
                      (illegal-option option))
                    `(:metaclass  ',(cadr option)))
                   (:documentation
                    `(:documentation ',(cadr option)))
                   (t
                     (list `',option-name `',(cdr option))))))
             (canonicalize-slot-spec (slot)
               (if (null slot) (signal-program-error "Illegal slot NIL"))
               (if (not (listp slot)) (setq slot (list slot)))
               (let* ((slot-name (require-type (car slot) 'symbol))
		      (initargs nil)
                      (other-options ())
		      (initform nil)
		      (initform-p nil)
		      (initfunction nil)
		      (type nil)
		      (type-p nil)
		      (allocation nil)
		      (allocation-p nil)
		      (documentation nil)
		      (documentation-p nil)
                      (readers nil)
		      (writers nil)
                      (reader-info (%cons-def-info 'defmethod (dpb 1 $lfbits-numreq 0) nil nil (list class-name)))
                      (writer-info (%cons-def-info 'defmethod (dpb 2 $lfbits-numreq 0) nil nil (list t class-name))))
                 (when (memq slot-name slot-names)
                   (signal-program-error "Multiple slots named ~S in DEFCLASS ~S" slot-name class-name))
                 (push slot-name slot-names)
                 (do ((options (cdr slot) (cddr options))
                      name)
                     ((null options))
                   (when (null (cdr options)) (signal-program-error "Illegal slot spec ~S" slot))
                   (case (car options)
                     (:reader
                      (setq name (cadr options))
                      (unless (memq name readers)
                        (push (cons name reader-info) signatures)
                        (push name readers)))
                     (:writer                      
                      (setq name (cadr options))
                      (unless (member name writers :test 'equal)
                        (push (cons name writer-info) signatures)
                        (push name writers)))
                     (:accessor
                      (setq name (cadr options))
                      (unless (memq name readers)
                        (push (cons name reader-info) signatures)
                        (push name readers))
                      (let ((setf-name `(setf ,name)))
                        (unless (member setf-name writers :test 'equal)
                          (push (cons (setf-function-name name) writer-info) signatures)
                          (push setf-name writers))))
                     (:initarg
                      (let* ((initarg (require-type (cadr options) 'symbol))
                             (other (position initarg slot-initargs :test #'memq)))
                        (when other
                          (warn "Initarg ~s occurs in both ~s and ~s slots"
                                initarg (nth (1+ other) slot-names) slot-name))
                        (push initarg initargs)))
                     (:type
                      (if type-p
			(duplicate-options slot)
			(setq type-p t))
                      (setq type (cadr options))
                      ;; complain about illegal typespecs and continue
                      (handler-case (specifier-type type env)
                        (program-error ()
                          (warn "Invalid type ~s in ~s slot definition ~s" type class-name slot))))
                     (:initform
                      (if initform-p
			(duplicate-options slot)
			(setq initform-p t))
                      (let ((option (cadr options)))
                        (setq initform `',option
                              initfunction
                              (if (constantp option)
                                `(constantly ,option)
                                `#'(lambda () ,option)))))
                     (:allocation
                      (if allocation-p
			(duplicate-options slot)
			(setq allocation-p t))
                      (setq allocation (cadr options)))
                     (:documentation
                      (if documentation-p
			(duplicate-options slot)
			(setq documentation-p t))
                      (setq documentation (cadr options)))
                     (t
                      (let* ((pair (or (assq (car options) other-options)
                                       (car (push (list (car options)) other-options)))))
                        (push (cadr options) (cdr pair))))))
                 (push initargs slot-initargs)
                 `(list :name ',slot-name
		   ,@(when allocation `(:allocation ',allocation))
		   ,@(when initform-p `(:initform ,initform
					:initfunction ,initfunction))
		   ,@(when initargs `(:initargs ',initargs))
		   ,@(when readers `(:readers ',readers))
		   ,@(when writers `(:writers ',writers))
		   ,@(when type-p `(:type ',type))
		   ,@(when documentation-p `(:documentation ,documentation))
                   ,@(mapcan #'(lambda (opt)
                                 `(',(car opt) ',(if (null (cddr opt))
                                                     (cadr opt)
                                                     (cdr opt)))) other-options)))))
	(let* ((direct-superclasses superclasses)
	       (direct-slot-specs (mapcar #'canonicalize-slot-spec slots))
	       (other-options (apply #'append (mapcar #'canonicalize-defclass-option class-options )))
	       (keyvect (class-keyvect class-name other-options)))
	  (when (vectorp keyvect)
	    (let ((illegal (loop for arg in other-options by #'cddr
			      as key = (if (quoted-form-p arg) (%cadr arg) arg)
			      unless (or (eq key :metaclass) (find key keyvect)) collect key)))
	      (when illegal
		(signal-program-error "Class option~p~{ ~s~} is not one of ~s"
				      (length illegal) illegal keyvect))))
	  `(progn
	     (when (memq ',class-name *nx-known-declarations*)
	       (check-declaration-redefinition ',class-name 'defclass))
	    (eval-when (:compile-toplevel)
	      (%compile-time-defclass ',class-name ,env)
	      (progn
		,@(mapcar #'(lambda (sig) `(record-function-info ',(car sig) ',(cdr sig) ,env))
			  signatures)))
	      (ensure-class-for-defclass ',class-name
			    :direct-superclasses ',direct-superclasses
			    :direct-slots ,`(list ,@direct-slot-specs)
			    ,@other-options)))))))

(defmacro define-method-combination (name &rest rest &environment env)
  (setq name (require-type name 'symbol))
  (cond ((or (null rest) (and (car rest) (symbolp (car rest))))
         `(short-form-define-method-combination ',name ',rest))
        ((listp (car rest))
         (destructuring-bind (lambda-list method-group-specifiers . forms) rest
           (long-form-define-method-combination 
            name lambda-list method-group-specifiers forms env)))
        (t (%badarg (car rest) '(or (and null symbol) list)))))

(defmacro defgeneric (function-name lambda-list &rest options-and-methods &environment env)
  (fboundp function-name)             ; type-check
  (multiple-value-bind (method-combination generic-function-class options methods)
      (parse-defgeneric function-name t lambda-list options-and-methods)
    (let ((gf (gensym)))
      `(progn
         (eval-when (:compile-toplevel)
           (record-function-info ',(maybe-setf-function-name function-name)
                                 ',(%cons-def-info 'defgeneric (encode-gf-lambda-list lambda-list))
                                 ,env))
         (let ((,gf (%defgeneric
                     ',function-name ',lambda-list ',method-combination ',generic-function-class 
                     ',(apply #'append options))))
           (%set-defgeneric-methods ,gf ,@methods)
           ,gf)))))



(defun parse-defgeneric (function-name global-p lambda-list options-and-methods)
  (check-generic-function-lambda-list lambda-list)
  (let ((method-combination '(standard))
        (generic-function-class 'standard-generic-function)
        options declarations methods option-keywords method-class)
    (flet ((bad-option (o)
             (signal-program-error "Bad option: ~s to ~s." o 'defgeneric)))
      (dolist (o options-and-methods)
        (let ((keyword (car o))
              (defmethod (if global-p 'defmethod 'anonymous-method)))
          (if (eq keyword :method)
	    (let ((defn `(,defmethod ,function-name ,@(%cdr o))))
	      (note-source-transformation o defn)
	      (push defn methods))
            (cond ((and (not (eq keyword 'declare))
			(memq keyword (prog1 option-keywords (push keyword option-keywords))))		   
                   (signal-program-error "Duplicate option: ~s to ~s" keyword 'defgeneric))
                  ((eq keyword :method-combination)
                   (unless (symbolp (cadr o))
                     (bad-option o))
                   (setq method-combination (cdr o)))
                  ((eq keyword :generic-function-class)
                   (unless (and (cdr o) (symbolp (cadr o)) (null (%cddr o)))
                     (bad-option o))
                   (setq generic-function-class (%cadr o)))
                  ((eq keyword 'declare)
		   (push (cadr o) declarations))
                  ((eq keyword :argument-precedence-order)
                   (dolist (arg (cdr o))
                     (unless (and (symbolp arg) (memq arg lambda-list))
                       (bad-option o)))
                   (push (list keyword (cdr o)) options))
                  ((eq keyword :method-class)
                   (push o options)
                   (when (or (cddr o) (not (symbolp (setq method-class (%cadr o)))))
                     (bad-option o)))
                  ((eq keyword :documentation)
                   (push o options)
                   (when (or (cddr o) (not (stringp (%cadr o))))
                     (bad-option o)))
                  (t (bad-option o)))))))
    (when method-class
      (dolist (m methods)
        (push `(:method-class ,method-class) (cddr m))))
    (when declarations
      (setq options `((:declarations ,declarations) ,@options)))
    (values method-combination generic-function-class options methods)))

                 
(defmacro def-aux-init-functions (class &rest functions)
  `(set-aux-init-functions ',class (list ,@functions)))






;;; A powerful way of defining REPORT-CONDITION...
;;; Do they really expect that each condition type has a unique method on PRINT-OBJECT
;;; which tests *print-escape* ?  Scary if so ...

(defmacro define-condition (name (&rest supers) (&rest slots) &body options)
  "DEFINE-CONDITION Name (Parent-Type*) (Slot-Spec*) Option*
   Define NAME as a condition type. This new type inherits slots and its
   report function from the specified PARENT-TYPEs. A slot spec is a list of:
     (slot-name :reader <rname> :initarg <iname> {Option Value}*

   The DEFINE-CLASS slot options :ALLOCATION, :INITFORM, [slot] :DOCUMENTATION
   and :TYPE and the overall options :DEFAULT-INITARGS and
   [type] :DOCUMENTATION are also allowed.

   The :REPORT option is peculiar to DEFINE-CONDITION. Its argument is either
   a string or a two-argument lambda or function name. If a function, the
   function is called with the condition and stream to report the condition.
   If a string, the string is printed.

   Condition types are classes, but (as allowed by ANSI and not as described in
   CLtL2) are neither STANDARD-OBJECTs nor STRUCTURE-OBJECTs. WITH-SLOTS and
   SLOT-VALUE may not be used on condition objects."
  ; If we could tell what environment we're being expanded in, we'd
  ; probably want to check to ensure that all supers name conditions
  ; in that environment.
  (let ((classopts nil)
        (duplicate nil)
        (docp nil)
	(default-initargs-p nil)
        (reporter nil))
    (dolist (option options)
      (unless (and (consp option)
                   (consp (%cdr option)))
        (signal-program-error "Invalid option ~s ." option))
      (ecase (%car option)
	(:default-initargs 
	    (unless (plistp (cdr option)) 
	      (signal-program-error "~S is not a plist." (%cdr option))) 
	    (if default-initargs-p 
	      (setq duplicate t) 
	      (push (setq default-initargs-p option) classopts))) 
        (:documentation 
	 (unless (null (%cddr option)) 
	   (signal-program-error "Invalid option ~s ." option)) 
	 (if docp
	   (setq duplicate t)
           (push (setq docp option) classopts)))
        (:report 
	 (unless (null (%cddr option)) 
	   (signal-program-error "Invalid option ~s ." option)) 
         (if reporter
           (setq duplicate t)
           (progn
             (if (or (lambda-expression-p (setq reporter (%cadr option)))
                     (symbolp reporter))
               (setq reporter `(function ,reporter))
               (if (stringp reporter)
                 (setq reporter `(function (lambda (c s) (declare (ignore c)) (write-string ,reporter s))))
                 (signal-program-error "~a expression is not a string, symbol, or lambda expression ." (%car option))))
             (setq reporter `((defmethod report-condition ((c ,name) s)
                                (funcall ,reporter c s))))))))
      (if duplicate (signal-program-error "Duplicate option ~s ." option)))
    `(progn
       (defclass ,name ,(or supers '(condition)) ,slots ,@classopts)
       ,@reporter
       ',name)))

(defmacro with-condition-restarts (&environment env condition restarts &body body)
  "Evaluates the BODY in a dynamic environment where the restarts in the list
   RESTARTS-FORM are associated with the condition returned by CONDITION-FORM.
   This allows FIND-RESTART, etc., to recognize restarts that are not related
   to the error currently being debugged. See also RESTART-CASE."
  (multiple-value-bind (body decls)
                       (parse-body body env)
    (let ((cond (gensym))
          (r (gensym)))
          `(let* ((*condition-restarts* *condition-restarts*))
             ,@decls
             (let ((,cond ,condition))
               (dolist (,r ,restarts) (push (cons ,r ,cond) *condition-restarts*))
               ,@body)))))
  
(defmacro setf-find-class (name arg1 &optional (arg2 () 2-p) (arg3 () 3-p))
  (cond (3-p ;might want to pass env (arg2) to find-class someday?
         `(set-find-class ,name (progn ,arg1 ,arg2 ,arg3)))
        (2-p
         `(set-find-class ,name (progn ,arg1 ,arg2)))
        (t `(set-find-class ,name ,arg1))))

(defsetf find-class setf-find-class)

(defmacro restoring-interrupt-level (var &body body)
  `(unwind-protect
    (progn ,@body)
    (restore-interrupt-level ,var)
    (%interrupt-poll)))

(defmacro without-interrupts (&body body)
  "Evaluate its body in an environment in which process-interrupt
requests are deferred."
  `(let* ((*interrupt-level* -1))
    ,@body))

(defmacro with-interrupts-enabled (&body body)
  "Evaluate its body in an environment in which process-interrupt
has immediate effect."
  `(let* ((*interrupt-level* 0))
    ,@body))

;;; undoes the effect of one enclosing without-interrupts during execution of body.
(defmacro ignoring-without-interrupts (&body body)
  `(let* ((*interrupt-level* 0))
    ,@body))



(defmacro error-ignoring-without-interrupts (format-string &rest format-args)
  `(ignoring-without-interrupts
    (error ,format-string ,@format-args)))


;init-list-default: if there is no init pair for <keyword>,
;    add a <keyword> <value> pair to init-list
(defmacro init-list-default (the-init-list &rest args)
  (let ((result)
       (init-list-sym (gensym)))
   (do ((args args (cddr args)))
       ((not args))
     (setq result 
           (cons `(if (eq '%novalue (getf ,init-list-sym ,(car args) 
                                          '%novalue))
                    (setq ,init-list-sym (cons ,(car args) 
                                               (cons ,(cadr args) 
                                                     ,init-list-sym))))
                 result)))                                                                                
   `(let ((,init-list-sym ,the-init-list))
      (progn ,@result)
      ,init-list-sym)
   ))

; This can only be partially backward-compatible: even if only
; the "name" arg is supplied, the old function would create the
; package if it didn't exist.
; Should see how well this works & maybe flush the whole idea.

(defmacro in-package (name)
  (let ((form nil))
    (when (quoted-form-p name)
      (warn "Unquoting argument ~S to ~S." name 'in-package )
      (setq name (cadr name)))    
    (setq form `(set-package ,(string name)))
    `(eval-when (:execute :load-toplevel :compile-toplevel)
      ,form)))

(defmacro defpackage (name &rest options)
  "Defines a new package called PACKAGE. Each of OPTIONS should be one of the 
   following: 
    (NICKNAMES {package-name}*)

    (SIZE <integer>)
    (SHADOW {symbol-name}*)
    (SHADOWING-IMPORT-FROM <package-name> {symbol-name}*)
    (USE {package-name}*)
    (IMPORT-FROM <package-name> {symbol-name}*)
    (INTERN {symbol-name}*)
    (EXPORT {symbol-name}*)
    (IMPLEMENT {package-name}*)
    (LOCK boolean)
    (DOCUMENTATION doc-string)
   All options except SIZE, LOCK, and :DOCUMENTATION can be used multiple 
   times."
  (let* ((size nil)
         (all-names-size 0)
         (intern-export-size 0)
         (shadow-etc-size 0)
	 (documentation nil)
         (all-names-hash (let ((all-options-alist nil))
                           (dolist (option options)
                             (let ((option-name (car option)))
                               (when (memq option-name
                                           '(:nicknames :shadow :shadowing-import-from
                                             :use :import-from :intern :export))
                                 (let ((option-size (length (cdr option)))
                                       (cell (assq option-name all-options-alist)))
                                   (declare (fixnum option-size))
                                   (if cell
                                     (incf (cdr cell) option-size)
                                     (push (cons option-name option-size) all-options-alist))
                                   (when (memq option-name '(:shadow :shadowing-import-from :import-from :intern))
                                     (incf shadow-etc-size option-size))
                                   (when (memq option-name '(:export :intern))
                                     (incf intern-export-size option-size))))))
                           (dolist (cell all-options-alist)
                             (let ((option-size (cdr cell)))
                               (when (> option-size all-names-size)
                                 (setq all-names-size option-size))))
                           (when (> all-names-size 0)
                             (make-hash-table :test 'equal :size all-names-size))))
         (intern-export-hash (when (> intern-export-size 0)
                               (make-hash-table :test 'equal :size intern-export-size)))
         (shadow-etc-hash (when (> shadow-etc-size 0)
                            (make-hash-table :test 'equal :size shadow-etc-size)))
         (external-size nil)
         (nicknames nil)
         (shadow nil)
         (shadowing-import-from-specs nil)
         (use :default)
         (import-from-specs nil)
         (intern nil)
         (export nil))
    (declare (fixnum all-names-size intern-export-size shadow-etc-size))
    (labels ((string-or-name (s) (string s))
             (duplicate-option (o)
               (signal-program-error "Duplicate ~S option in ~S ." o options))
             (duplicate-name (name option-name)
               (signal-program-error "Name ~s, used in ~s option, is already used in a conflicting option ." name option-name))
             (all-names (option-name tail already)
               (when (eq already :default) (setq already nil))
               (when all-names-hash
                 (clrhash all-names-hash))
               (dolist (name already)
                 (setf (gethash (string-or-name name) all-names-hash) t))
               (dolist (name tail already)
                 (setq name (string-or-name name))
                 (unless (gethash name all-names-hash)          ; Ok to repeat name in same option.
                   (when (memq option-name '(:shadow :shadowing-import-from :import-from :intern))
                     (if (gethash name shadow-etc-hash)
                       (duplicate-name name option-name))
                     (setf (gethash name shadow-etc-hash) t))
                   (when (memq option-name '(:export :intern))
                     (if (gethash name intern-export-hash)
                       (duplicate-name name option-name))
                     (setf (gethash name intern-export-hash) t))
                   (setf (gethash name all-names-hash) t)
                   (push name already)))))
      (dolist (option options)
        (let ((args (cdr option)))
          (ecase (%car option)
                 (:size 
                  (if size 
                    (duplicate-option :size) 
                    (setq size (car args))))		 
                 (:external-size 
                  (if external-size 
                    (duplicate-option :external-size) 
                    (setq external-size (car args))))
                 (:nicknames (setq nicknames (all-names nil args nicknames)))
                 (:shadow (setq shadow (all-names :shadow args shadow)))
                 (:shadowing-import-from
                  (destructuring-bind (from &rest shadowing-imports) args
                    (push (cons (string-or-name from)
                                (all-names :shadowing-import-from shadowing-imports nil))
                          shadowing-import-from-specs)))
                 (:use (setq use (all-names nil args use)))
                 (:import-from
                  (destructuring-bind (from &rest imports) args
                    (push (cons (string-or-name from)
                                (all-names :import-from imports nil))
                          import-from-specs)))
                 (:intern (setq intern (all-names :intern args intern)))
                 (:export (setq export (all-names :export args export)))
		 (:documentation
		  (if documentation
		    (duplicate-option :documentation)
		    (setq documentation (cadr option)))))))
      `(eval-when (:execute :compile-toplevel :load-toplevel)
         (%define-package ',(string-or-name name)
	  ',size 
	  ',external-size 
	  ',nicknames
	  ',shadow
	  ',shadowing-import-from-specs
	  ',use
	  ',import-from-specs
	  ',intern
	  ',export
	  ',documentation)))))



(defmacro with-package-iterator ((mname package-list first-type &rest other-types)
                                 &body body)
  "Within the lexical scope of the body forms, MNAME is defined via macrolet
   such that successive invocations of (MNAME) will return the symbols,
   one by one, from the packages in PACKAGE-LIST. SYMBOL-TYPES may be
   any of :INHERITED :EXTERNAL :INTERNAL."
  (setq mname (require-type mname 'symbol))
  (let ((state (make-symbol "WITH-PACKAGE-ITERATOR_STATE")))
    (dolist (type (push first-type other-types))
      (ecase type
        ((:external :internal :inherited))))
    `(let ((,state (%setup-pkg-iter-state ,package-list ',other-types)))
       (macrolet ((,mname () `(%pkg-iter-next ,',state)))
         ,@body))))

; Does NOT evaluate the constructor, but DOES evaluate the destructor & initializer
(defmacro defresource (name &key constructor destructor initializer)
  `(defparameter ,name (make-resource #'(lambda () ,constructor)
                                      ,@(when destructor
                                          `(:destructor ,destructor))
                                      ,@(when initializer
                                          `(:initializer ,initializer)))))

(defmacro using-resource ((var resource) &body body)
  (let ((resource-var (gensym)))
  `(let ((,resource-var ,resource)
         ,var)
     (unwind-protect
       (progn
         (setq ,var (allocate-resource ,resource-var))
         ,@body)
       (when ,var
         (free-resource ,resource-var ,var))))))

;;; Bind per-thread specials which help with lock accounting.
(defmacro with-lock-context (&body body)
  `(progn ,@body))

(defmacro with-lock-grabbed ((lock &optional
                                   (whostate "Lock"))
                             &body body)
  "Wait until a given lock can be obtained, then evaluate its body with
the lock held."
  (declare (ignore whostate))
    (let* ((locked (gensym))
           (l (gensym)))
      `  (with-lock-context
           (let ((,locked (make-lock-acquisition))
             (,l ,lock))
        (declare (dynamic-extent ,locked))
        (unwind-protect
             (progn
               (%lock-recursive-lock-object ,l ,locked )
               ,@body)
          (when (lock-acquisition.status ,locked) (%unlock-recursive-lock-object ,l)))))))

(defmacro with-lock-grabbed-maybe ((lock &optional
					 (whostate "Lock"))
				   &body body)
  (declare (ignore whostate))
  (let* ((l (gensym)))
    `(with-lock-context
      (let* ((,l ,lock))
        (when (%try-recursive-lock-object ,l)
          (unwind-protect
               (progn ,@body)
            (%unlock-recursive-lock-object ,l)))))))

(defmacro with-standard-abort-handling (abort-message &body body)
  (let ((stream (gensym)))
    `(restart-case
       (catch :abort
         (catch-cancel
           ,@body))
       (abort () ,@(when abort-message
                     `(:report (lambda (,stream)
                                 (write-string ,abort-message ,stream)))))
       (abort-break ()))))
       



(defmacro %lexpr-count (l)
  `(%lisp-word-ref ,l 0))

(defmacro %lexpr-ref (lexpr count i)
  `(%lisp-word-ref ,lexpr (%i- ,count ,i)))

;;; args will be list if old style clos
(defmacro apply-with-method-context (magic function args)
  (let ((m (gensym))
        (f (gensym))
        (as (gensym)))
      `((lambda (,m ,f ,as)
          (if (listp ,as)
            (%apply-with-method-context ,m ,f ,as)
            (%apply-lexpr-with-method-context ,m ,f ,as))) ,magic ,function ,args)))

(defmacro defcallback (name arglist &body body &environment env)
  "Proclaim name to be a special variable; sets its value to a MACPTR which,
when called by foreign code, calls a lisp function which expects foreign
arguments of the specified types and which returns a foreign value of the
specified result type. Any argument variables which correspond to foreign
arguments of type :ADDRESS are bound to stack-allocated MACPTRs.

If name is already a callback function pointer, its value is not changed;
instead, it's arranged that an updated version of the lisp callback function
will be called. This feature allows for callback functions to be redefined
incrementally, just like Lisp functions are.

defcallback returns the callback pointer, e.g., the value of name."
  (define-callback name arglist body env))

(declare-arch-specific-macro %get-single-float-from-double-ptr)

(declare-arch-specific-macro lfun-vector)
(declare-arch-specific-macro lfun-vector-lfun)

(declare-arch-specific-macro symptr->symvector)
(declare-arch-specific-macro symvector->symptr)

(declare-arch-specific-macro function-to-function-vector)
(declare-arch-specific-macro function-vector-to-function)

(declare-arch-specific-macro with-ffcall-results)

(defvar *trace-print-functions* nil)
(defun %trace-print-arg (stream arg val type)
  (format stream " ")
  (let ((fn (assoc type *trace-print-functions*)))
    (if fn
      (funcall (cdr fn) stream arg val)
      (progn
      (when arg
        (format stream "~A = " arg))
      (if (and type (not (eq type :void)))
          (format stream "[:~A] ~A~%" type val)
        (format stream ":VOID~%" val))))))

(defun def-trace-print-function (type fn)
  (push (cons type fn) *trace-print-functions*))

(defun define-callback (name args body env)
  (let* ((stack-word (gensym))
         (stack-ptr (gensym))
         (fp-args-ptr (gensym))
         (result-type-spec :void)
         (args args)
         (discard-stack-args nil)	;only meaningful on win32
	 (discard-hidden-arg nil)	;only meaningful on x8632
	 (info nil)
         (woi nil)
         (need-struct-arg)
         (struct-return-arg-name)
         (error-return nil))
    (collect ((arg-names)
              (arg-specs))
      (let* ((spec (car (last args)))
             (rtype (ignore-errors (parse-foreign-type spec))))
        (setq need-struct-arg (typep rtype 'foreign-record-type))
	(when need-struct-arg
	  (setq discard-hidden-arg
		(funcall (ftd-ff-call-struct-return-by-implicit-arg-function
			  *target-ftd*) rtype)))
        (if rtype
          (setq result-type-spec spec args (butlast args))))
      (loop
        (when (null args) (return))
        (if (eq (car args) :without-interrupts)
          (setq woi (cadr args) args (cddr args))
          (if (eq (car args) :discard-stack-args)
            (setq discard-stack-args (eq (backend-target-os *target-backend*) :win32) args (cdr args))
            (if (eq (car args) :error-return)
              (setq error-return
                    (cadr args)                  
                    args (cddr args))
              (if need-struct-arg
                (setq struct-return-arg-name (pop args) need-struct-arg nil)
                (progn
                  (arg-specs (pop args))
                  (arg-names (pop args))))))))
      (multiple-value-bind (rlets lets dynamic-extent-names inits foreign-return-type fp-args-form error-return-offset num-arg-bytes)
          (funcall (ftd-callback-bindings-function *target-ftd*)
                   stack-ptr fp-args-ptr (arg-names) (arg-specs) result-type-spec struct-return-arg-name)
	;; x8632 hair
	(when discard-hidden-arg
	  (if discard-stack-args
	    ;; We already have to discard some number of args, so just
	    ;; discard the extra hidden arg while we're at it.
	    (incf num-arg-bytes 4)
	    ;; Otherwise, indicate that we'll need to discard the
	    ;; hidden arg.
	    (setq info (ash 1 23))))
	(when discard-stack-args
	  (setq info 0)
	  ;; put number of words to discard in high-order byte
	  (setf (ldb (byte 8 24) info)
		(ash num-arg-bytes (- target::word-shift))))
        (multiple-value-bind (body decls doc) (parse-body body env t)
          `(progn
            (declaim (special ,name))
            (define-callback-function
                (nfunction ,name
                 (lambda (,stack-word)
                   (declare (ignorable ,stack-word))
                   (block ,name
                     (with-macptrs ((,stack-ptr))
                       (%setf-macptr-to-object ,stack-ptr ,stack-word)
                       (with-macptrs (,@(when fp-args-form
                                              `((,fp-args-ptr ,fp-args-form))))
                         ,(defcallback-body stack-ptr
                                            fp-args-ptr
                                            lets
                                            rlets
                                            inits
                                            `(declare (dynamic-extent ,@dynamic-extent-names))
                                            decls
                                            body
                                            foreign-return-type
                                            struct-return-arg-name
                                            error-return
                                            error-return-offset
                                            ))))))
                ,doc
              ,woi
              ,info)))))))


(defun defcallback-body (&rest args)
  (declare (dynamic-extent args))
  (destructuring-bind (stack-ptr fp-args-ptr lets rlets inits dynamic-extent-decls other-decls body return-type struct-return-arg error-return error-delta) args
    (declare (ignorable dynamic-extent-decls))
    (let* ((condition-name (if (atom error-return) 'error (car error-return)))
           (error-return-function (if (atom error-return) error-return (cadr error-return)))
           (result (if struct-return-arg (gensym)))
           (body
            `(rlet ,rlets
              (let ,lets
                ,dynamic-extent-decls
                ,@other-decls
                ,@inits
                ,(if result
                     `(let* ((,result ,@body))
                       (declare (dynamic-extent ,result)
                                (ignorable ,result))
                       ,(funcall (ftd-callback-return-value-function *target-ftd*)
                              stack-ptr
                              fp-args-ptr
                              result
                              return-type
                              struct-return-arg))
                     (if (eq return-type *void-foreign-type*)
                       `(progn ,@body)
                       (funcall (ftd-callback-return-value-function *target-ftd*)
                                stack-ptr
                                fp-args-ptr
                                `(progn ,@body)
                                return-type
                                struct-return-arg)))
                nil))))
      (if error-return
        (let* ((cond (gensym))
               (block (gensym))
               (handler (gensym)))
          `(block ,block
            (let* ((,handler (lambda (,cond)
                               (,error-return-function ,cond ,stack-ptr (%inc-ptr ,stack-ptr ,error-delta))
                               (return-from ,block
                                 nil))))
              (declare (dynamic-extent ,handler))
              (handler-bind ((,condition-name ,handler))
                (values ,body)))))
        body))))


(defmacro define-toplevel-command (group-name name arglist &body body &environment env)
  (let* ((key (make-keyword name)))
    (multiple-value-bind (body decls doc) (parse-body body env)
      `(%define-toplevel-command ',group-name ,key ',name 
	(nfunction ,name (lambda ,arglist
			   ,@decls
			   (block ,name
			     ,@body)))
	,doc
        ',(mapcar #'symbol-name arglist)))))

(defmacro with-toplevel-commands (group-name &body body)
  `(let* ((*active-toplevel-commands* *active-toplevel-commands*))
    (progn
      (%use-toplevel-commands ',group-name)
      ,@body)))

(defmacro assert (test-form &optional (places ()) string &rest args)
  "ASSERT Test-Form [(Place*) [String Arg*]]
  If the Test-Form is not true, then signal a correctable error.  If Places
  are specified, then new values are prompted for when the error is proceeded.
  String and Args are the format string and args to the error call."
  (let* ((TOP (gensym))
         (setf-places-p (not (null places))))
    `(without-compiling-code-coverage
      (tagbody
       ,TOP
       (unless ,test-form
         (%assertion-failure ,setf-places-p ',test-form ,string ,@args)
         ,@(if places
             `((write-line "Type expressions to set places to, or nothing to leave them alone."
                           *query-io*)
               ,@(mapcar #'(lambda (place &aux (new-val (gensym))
                                          (set-p (gensym)))
                             `(multiple-value-bind
                                (,new-val ,set-p)
                                (assertion-value-prompt ',place)
                                (when ,set-p (setf ,place (values-list ,new-val)))))
                         places)))
         (go ,TOP))))))


(defmacro check-type (place typespec &optional string)
  "CHECK-TYPE Place Typespec [String]
  Signal a restartable error of type TYPE-ERROR if the value of PLACE is
  not of the specified type. If an error is signalled and the restart is
  used to return, this can only return if the STORE-VALUE restart is
  invoked. In that case it will store into PLACE and start over."
  (let* ((val (gensym)))
    `(without-compiling-code-coverage
      (do* ((,val ,place ,place))
          ((typep ,val ',typespec))
       (setf ,place (%check-type ,val ',typespec ',place ,string))))))




(defmacro with-hash-table-iterator ((mname hash-table) &body body)
  "WITH-HASH-TABLE-ITERATOR ((function hash-table) &body body)
   provides a method of manually looping over the elements of a hash-table.
   FUNCTION is bound to a generator-macro that, within the scope of the
   invocation, returns one or three values. The first value tells whether
   any objects remain in the hash table. When the first value is non-NIL,
   the second and third values are the key and the value of the next object."
  (let* ((hash (gensym))
         (keys (gensym))
         (values (gensym))
         (count (gensym))
         (state (gensym)))
    `(let* ((,hash ,hash-table)
            (,count (hash-table-count ,hash))
            (,keys (make-array ,count))
            (,values (make-array ,count))
            (,state (vector ,hash 0 ,keys ,values (enumerate-hash-keys-and-values ,hash ,keys ,values))))
      (declare (dynamic-extent ,keys ,state)
               (fixnum ,count))
      (macrolet ((,mname () `(next-hash-table-iteration-1 ,',state)))
        ,@body))))


(eval-when (compile load eval)
(defmacro pprint-logical-block ((stream-symbol list
				 &key (prefix "" prefixp)
                                      (per-line-prefix "" per-line-prefix-p)
				      (suffix "" suffixp))
				&body body)
  (cond ((eq stream-symbol nil) (setq stream-symbol '*standard-output*))
	((eq stream-symbol T) (setq stream-symbol '*terminal-io*)))
  (when (not (symbolp stream-symbol))
    (warn "STREAM-SYMBOL arg ~S to PPRINT-LOGICAL-BLOCK is not a bindable symbol"
	  stream-symbol)
    (setq stream-symbol '*standard-output*))
  (when (and prefixp per-line-prefix-p)
    (warn "prefix ~S and per-line-prefix ~S cannot both be specified ~
           in PPRINT-LOGICAL-BLOCK" prefix per-line-prefix)
    (setq per-line-prefix nil))
  `(let ((*logical-block-p* t))
     (maybe-initiate-xp-printing
      #'(lambda (,stream-symbol)
          (let ((+l ,list)
                (+p (or (and ,prefixp
                             (require-type ,prefix 'string))
                        (and ,per-line-prefix-p
                             (require-type ,per-line-prefix 'string))))
                (+s (require-type ,suffix 'string)))
            (pprint-logical-block+
                (,stream-symbol +l +p +s ,per-line-prefix-p T nil)
              ,@ body nil)))
      (decode-stream-arg ,stream-symbol))))


;Assumes var and args must be variables.  Other arguments must be literals or variables.

(defmacro pprint-logical-block+ ((var args prefix suffix per-line? circle-check? atsign?)
				 &body body)
  "Group some output into a logical block. STREAM-SYMBOL should be either a
   stream, T (for *TERMINAL-IO*), or NIL (for *STANDARD-OUTPUT*). The printer
   control variable *PRINT-LEVEL* is automatically handled."
  (when (and circle-check? atsign?)
    (setq circle-check? 'not-first-p))
  `(let ((*current-level* (1+ *current-level*))
	 (*current-length* -1)
	 ;(*parents* *parents*)
	 ,@(if (and circle-check? atsign?) `((not-first-p (plusp *current-length*)))))
     (unless (check-block-abbreviation ,var ,args ,circle-check?)
       (start-block ,var ,prefix ,per-line? ,suffix)
       (when
         (catch 'line-limit-abbreviation-exit
           (block logical-block
             (macrolet ((pprint-pop () `(pprint-pop+ ,',args ,',var))
                        (pprint-exit-if-list-exhausted ()
                          `(if (null ,',args) (return-from logical-block nil))))
               ,@ body))
           (end-block ,var ,suffix)
           nil)
         (end-block ,var ,suffix)
         (throw 'line-limit-abbreviation-exit T)))))
) ; eval-when

(defmacro %old-class-local-shared-slotds (class &optional default)
  (if default                           ; so setf works
    `(%class-get ,class '%old-class-local-shared-slotds ,default)
    `(%class-get ,class '%old-class-local-shared-slotds)))

(defmacro with-slot-values (slot-entries instance-form &body body)
; Simplified form of with-slots.  Expands into a let instead of a symbol-macrolet
; Thus, you can access the slot values, but you can't setq them.
  (let ((instance (gensym)) var slot-name bindings)
    (dolist (slot-entry slot-entries)
      (cond ((symbolp slot-entry)
             (setq var slot-entry slot-name slot-entry))
            ((and (listp slot-entry) (cdr slot-entry) (null (cddr slot-entry))
                  (symbolp (car slot-entry)) (symbolp (cadr slot-entry)))
             (setq var (car slot-entry) slot-name (cadr slot-entry)))
            (t (signal-program-error "Malformed slot-entry: ~a to with-slot-values.~@
                                      Should be a symbol or a list of two symbols."
				     slot-entry)))
      (push `(,var (slot-value ,instance ',slot-name)) bindings))
    `(let ((,instance ,instance-form))
       (let ,(nreverse bindings)
         ,@body))))

(defmacro with-slots (slot-entries instance-form &body body)
  "Establish a lexical environment for referring to the slots in the
instance named by the given slot-names as though they were variables.
Within such a context the value of the slot can be specified by using
its slot name, as if it were a lexically bound variable. Both setf and
setq can be used to set the value of the slot."
  (let ((instance (gensym)) var slot-name bindings)
    (dolist (slot-entry slot-entries)
      (cond ((symbolp slot-entry)
             (setq var slot-entry slot-name slot-entry))
            ((and (listp slot-entry) (cdr slot-entry) (null (cddr slot-entry))
                  (symbolp (car slot-entry)) (symbolp (cadr slot-entry)))
             (setq var (car slot-entry) slot-name (cadr slot-entry)))
            (t (signal-program-error "Malformed slot-entry: ~a to with-slots.~@
                                      Should be a symbol or a list of two symbols."
				     slot-entry)))
      (push `(,var (slot-value ,instance ',slot-name)) bindings))
    `(let ((,instance ,instance-form))
       ,@(if bindings 
             (list `(declare (ignorable ,instance)))
             (list `(declare (ignore ,instance))))
       (symbol-macrolet ,(nreverse bindings)
         ,@body))))

(defmacro with-accessors (slot-entries instance-form &body body)
  "Create a lexical environment in which the slots specified by slot-entry
are lexically available through their accessors as if they were variables.
The appropriate accessors are invoked to access the slots specified by
slot-entry. Both setf and setq can be used to set the value of the slot."
  (let ((instance (gensym)) var reader bindings)
    (dolist (slot-entry slot-entries)
      (cond ((and (listp slot-entry) (cdr slot-entry) (null (cddr slot-entry))
                  (symbolp (car slot-entry)) (symbolp (cadr slot-entry)))
             (setq var (car slot-entry) reader (cadr slot-entry)))
            (t (signal-program-error "Malformed slot-entry: ~a to with-accessors.~@
                                     Should be a list of two symbols."
				     slot-entry)))
      (push `(,var (,reader ,instance)) bindings))
    `(let ((,instance ,instance-form))
       ,@(if bindings 
             (list `(declare (ignorable ,instance)))
             (list `(declare (ignore ,instance))))
       (symbol-macrolet ,(nreverse bindings)
         ,@body))))

; I wanted to call this ":method"
(defmacro reference-method (gf &rest qualifiers-and-specializers)
  (let ((qualifiers (butlast qualifiers-and-specializers))
        (specializers (car (last qualifiers-and-specializers))))
    (if (null specializers) (report-bad-arg qualifiers-and-specializers '(not null)))
    `(find-method #',gf ',qualifiers (mapcar #'find-specializer ',specializers))))

(defmacro time (form)
  "Execute FORM and print timing information on *TRACE-OUTPUT*."
  `(report-time ',form #'(lambda () (progn ,form))))

(defmacro with-error-reentry-detection (&body body)
  (let ((thunk (gensym)))
    `(let ((,thunk #'(lambda () ,@body)))
       (declare (dynamic-extent ,thunk))
       (funcall-with-error-reentry-detection ,thunk))))

(defmacro without-duplicate-definition-warnings (&body body)
  `(compiler-let ((*compiler-warn-on-duplicate-definitions* nil))
     ,@body))


#+ppc-target
(defmacro scan-for-instr (mask opcode fn pc-index &optional (tries *trap-lookup-tries*))
  `(%scan-for-instr ,mask ,opcode ,fn ,pc-index ,tries))


(declare-arch-specific-macro codevec-header-p)

#+ppc-target
(defmacro match-instr (instr mask bits-to-match)
  `(eql (logand ,instr ,mask) ,bits-to-match))

(defmacro with-xp-stack-frames ((xp trap-function &optional stack-frame) &body body)
  (let ((thunk (gensym))
        (sf (or stack-frame (gensym))))
    `(let ((,thunk #'(lambda (&optional ,sf)
                       ,@(unless stack-frame `((declare (ignore ,sf))))
                       ,@body)))
       (declare (dynamic-extent ,thunk))
       (funcall-with-xp-stack-frames ,xp ,trap-function ,thunk))))

(defmacro signal-eof-error (stream)
  `(error 'end-of-file :stream ,stream))

(defmacro check-eof (valform stream eof-error-p eof-value)
  (let* ((val (gensym)))
    `(let ((,val ,valform))
      (if (eq ,val :eof)
        (if ,eof-error-p
          (signal-eof-error ,stream)
          ,eof-value)
        ,val))))

(defmacro designated-input-stream (input-stream)
  `(if ,input-stream
    (if (eq t ,input-stream)
      *terminal-io*
      ,input-stream)
    *standard-input*))

(defmacro pref (pointer accessor)
  "Reference an instance of a foreign type (or a component of a foreign
type) accessible via ptr.

Expand into code which references the indicated scalar type or component,
or returns a pointer to a composite type."
  (let* ((*target-ftd* (backend-target-foreign-type-data *target-backend*)))
    (destructuring-bind (type-name &rest accessors) (decompose-record-accessor accessor)
      (%foreign-access-form pointer (%foreign-type-or-record type-name) 0 accessors))))

(defmacro paref (pointer type-name index)
  (let* ((*target-ftd* (backend-target-foreign-type-data *target-backend*)))
    (%foreign-array-access-form  pointer (%foreign-type-or-record type-name) index)))

(defmacro rref (pointer accessor &key (storage :pointer storage-p))
  (when storage-p
    (warn "Use of :storage option ignored: ~a" storage))
  `(pref ,pointer ,accessor))

(defmacro rlet (spec &body body)
  "Execute body in an environment in which each var is bound to a MACPTR
encapsulating the address of a stack-allocated foreign memory block,
allocated and initialized from typespec and initforms as per make-record.
Return whatever value(s) body returns."
  (let* ((*target-ftd* (backend-target-foreign-type-data *target-backend*)))
    `(%stack-block ,(rlet-sizes spec)
      ,@(rlet-inits spec)
      ,@body)))

(defmacro rletz (spec &body body)
  "Execute body in an environment in which each var is bound to a MACPTR
encapuslating the address of a stack-allocated foreign memory block,
allocated and initialized from typespec and initforms as per make-record.
Return whatever value(s) body returns.

Unlike rlet, record fields that aren't explicitly initialized are set
to binary 0."
  (let* ((*target-ftd* (backend-target-foreign-type-data *target-backend*)))
    `(%stack-block ,(rlet-sizes spec t)
      ,@(rlet-inits spec)
      ,@body)))

(defun rlet-sizes (inits &optional clear-p &aux result)
  (dolist (item inits (nreverse result))
    (push `(,(car item)
            ,(%foreign-type-or-record-size (cadr item) :bytes)
            ,@(if clear-p '(:clear t)))
          result)))

(defun rlet-inits (inits &aux result)
  (dolist (item inits result)
    (let* ((name (car item))
           (record-name (cadr item))
           (inits (cddr item))
           (ftype (%foreign-type-or-record record-name))
           (ordinal (foreign-type-ordinal ftype))
           (ordinal-form (if (< ordinal max-canonical-foreign-type-ordinal)
                           ordinal
                           `(foreign-type-ordinal (load-time-value (%foreign-type-or-record ',record-name))))))
      (when (eq *host-backend* *target-backend*)
        (setq result (nconc result `((%set-macptr-type ,name ,ordinal-form)))))
      (if (typep ftype 'foreign-record-type)
        (setq result
              (nconc result (%foreign-record-field-forms name ftype record-name inits)))
        (progn
          (when inits
            (if (and ftype (null (cdr inits)))
              (setq result
                    (nconc result
                           `((setf ,(%foreign-access-form name ftype 0 nil)
                              ,(car inits)))))
              (signal-program-error "Unexpected or malformed initialization forms: ~s in field type: ~s"
				    inits record-name))))))))

(defun %foreign-record-field-forms (ptr record-type record-name inits)
  (unless (evenp (length inits))
    (signal-program-error "Unexpected or malformed initialization forms: ~s in field type: ~s"
			  inits record-name))
  (let* ((result ()))
    (do* ()
	 ((null inits)
	  `((progn
	      ;(%assert-macptr-ftype ,ptr ,record-type)
	      ,@(nreverse result))))
      (let* ((accessor (decompose-record-accessor (pop inits)))
	     (valform (pop inits)))
	(push `(setf ,(%foreign-access-form ptr record-type 0  accessor) ,valform)
	      result)))))
  
(defmacro get-field-offset (accessor)
  (destructuring-bind (type-name field-name) (decompose-record-accessor accessor)
    (let* ((record-type (require-type (%foreign-type-or-record type-name) 'foreign-record-type))
           (field (%find-foreign-record-type-field record-type field-name))
           (bit-offset (foreign-record-field-offset field)))
      `(values ,(floor bit-offset 8) ,(foreign-record-field-type field) ,bit-offset))))

(defmacro record-length (recname)
  (%foreign-type-or-record-size recname :bytes))

(defun make-record-form (record-name allocator &rest initforms)
  (let* ((ftype (%foreign-type-or-record record-name))
         (ordinal (foreign-type-ordinal ftype))
         (ordinal-form (if (< ordinal max-canonical-foreign-type-ordinal)
                         ordinal
                         `(foreign-type-ordinal (load-time-value (%foreign-type-or-record ',record-name)))))
         (bits (ensure-foreign-type-bits ftype))
	 (bytes (if bits
		  (ceiling bits 8)
		  (signal-program-error "Unknown size for foreign type ~S."
					(unparse-foreign-type ftype))))
	 (p (gensym))
	 (memset (read-from-string "#_memset")))    
    `(let* ((,p (,allocator ,bytes)))
      ,@(when (eq *host-backend* *target-backend*)
              `((%set-macptr-type ,p ,ordinal-form)))
      (,memset ,p 0 ,bytes)
      ,@(%foreign-record-field-forms p ftype record-name initforms)
      ,p)))
  
(defmacro make-record (record-name &rest initforms)
  "Expand into code which allocates and initalizes an instance of the type
denoted by typespec, on the foreign heap. The record is allocated using the
C function malloc, and the user of make-record must explicitly call the C
function free to deallocate the record, when it is no longer needed."
  (apply 'make-record-form record-name 'malloc initforms))

(defmacro make-gcable-record (record-name &rest initforms)
  "Like MAKE-RECORD, only advises the GC that the foreign memory can
   be deallocated if the returned pointer becomes garbage."
  (apply 'make-record-form record-name '%new-gcable-ptr initforms))

(defmacro copy-record (type source dest)
  (let* ((size (* (%foreign-type-or-record-size type :words) #+64-bit-target 1 #+32-bit-target 2))
         (src (gensym "SRC"))
         (dst (gensym "DST"))
         (accessor #+64-bit-target '%get-unsigned-long #+32-bit-target '%get-unsigned-word)
         (i (gensym "I"))
         (j (gensym "J")))
    `(with-macptrs ((,src ,source)
                    (,dst ,dest))
      (do* ((,i 0 (+ ,i #+64-bit-target 4 #+32-bit-target 2))
            (,j 0 (+ ,j 1)))
           ((= ,j ,size))
        (declare (fixnum ,i))
        (setf (,accessor ,dst ,i) (,accessor ,src ,i))))))

(defmacro assert-pointer-type (pointer type)
  "Assert that the pointer points to an instance of the specified foreign type.
Return the pointer."
  (let* ((ptr (gensym)))
    `(let* ((,ptr ,pointer))
      (%set-macptr-type ,ptr (foreign-type-ordinal (load-time-value (parse-foreign-type ',type))))
      ,ptr)))

    

(defmacro with-terminal-input (&body body)
  "Execute body in an environment with exclusive read access to the terminal."
  (let* ((got-it (gensym)))
    `(let* ((,got-it (%request-terminal-input)))
      (unwind-protect
	   (progn ,@body)
	(%restore-terminal-input ,got-it)))))


(defmacro with-process-whostate ((whostate) &body body)
  `(let* ((*whostate* ,whostate))
    ,@body))





(defmacro with-read-lock ((lock) &body body)
  "Wait until a given lock is available for read-only access, then evaluate
its body with the lock held."
  (let* ((p (gensym)))
    `(with-lock-context
      (let* ((,p ,lock))
        (unwind-protect
             (progn
               (read-lock-rwlock ,p)
               ,@body)
          (unlock-rwlock ,p))))))


(defmacro with-write-lock ((lock) &body body)
  "Wait until the given lock is available for write access, then execute
its body with the lock held."
  (let* ((p (gensym)))
    `(with-lock-context
      (let* ((,p ,lock))
      (unwind-protect
           (progn
             (write-lock-rwlock ,p)
             ,@body)
        (unlock-rwlock ,p))))))



(defmacro without-gcing (&body body)
  `(unwind-protect
    (progn
      (%lock-gc-lock)
      ,@body)
    (%unlock-gc-lock)))

(defmacro with-deferred-gc (&body body)
  "Execute BODY without responding to the signal used to suspend
threads for GC.  BODY must be very careful not to do anything which
could cause an exception (note that attempting to allocate lisp memory
may cause an exception.)"
  `(let* ((*interrupt-level* -2))
    ,@body))

(defmacro allowing-deferred-gc (&body body)
  "Within the extent of a surrounding WITH-DEFERRED-GC, allow GC."
  `(let* ((*interrupt-level* -1))
    (%check-deferred-gc)
    ,@body))

(defmacro defer-gc ()
  `(setq *interrupt-level* -2))


(defmacro with-pointer-to-ivector ((ptr ivector) &body body)
  "Executes BODY with PTR bound to a pointer to the first byte of data
in IVECTOR.  The GC is disabled during execution of BODY; PTR has
has dynamic-extent (and the address it references may become invalid
after the BODY exits.)  IVECTOR should be a (SIMPLE-ARRAY (*)) whose
element-type is numeric."
  (let* ((v (gensym)))
    `(let* ((,v ,ivector))
       (unless (typep ,v 'ivector) (report-bad-arg ,v 'ivector))
       (without-gcing
         (with-macptrs ((,ptr))
           (%vect-data-to-macptr ,v ,ptr)
           ,@body)))))
      


(defmacro with-other-threads-suspended (&body body)
  `(unwind-protect
    (progn
      (%suspend-other-threads)
      ,@body)
    (%resume-other-threads)))

(defmacro with-package-read-lock ((p) &body body)
  `(with-read-lock ((pkg.lock ,p)) ,@body))

(defmacro with-package-write-lock ((p) &body body)
  `(with-write-lock ((pkg.lock ,p)) ,@body))

(defmacro with-package-lock ((p) &body body)
  `(with-package-write-lock (,p) ,@body))

;;; Lock %all-packages-lock%, for shared read access to %all-packages%

(defmacro with-package-list-read-lock (&body body)
  `(with-read-lock (%all-packages-lock%) ,@body))

;;; Lock %all-packages-lock%, to allow modification to %all-packages%
(defmacro with-package-list-write-lock (&body body)
  `(with-write-lock (%all-packages-lock%) ,@body))

(defmacro atomic-incf-decf (place delta &environment env)
  (setq place (macroexpand place env))
  (if (consp place)
    (let* ((sym (car place))
	   (struct-transform (or (environment-structref-info sym env)
                                 (gethash sym %structure-refs%))))
      (if struct-transform
        (setq place (defstruct-ref-transform struct-transform (cdr place) env)
              sym (car place)))
      (ecase sym
	(the `(the ,(cadr place) (atomic-incf-decf ,(caddr place) ,delta)))
         ;; Needed so can handle %svref (which macroexpands into a LET*)
         ((let let*) (multiple-value-bind (body decls) (parse-body (cddr place) env t)
                       (unless (eql (length body) 1)
                         (error "~S is not a valid atomic-incf/decf place" place))
                       `(,sym ,(cadr place) ,@decls (atomic-incf-decf ,@body ,delta))))
         ;; Ditto
         (locally (multiple-value-bind (body decls) (parse-body (cdr place) env t)
                    (unless (eql (length body) 1)
                      (error "~S is not a valid atomic-incf/decf place" place))
                    `(,sym ,@decls (atomic-incf-decf ,@body ,delta))))
	(car `(%atomic-incf-car ,(cadr place) ,delta))
	(cdr `(%atomic-incf-cdr ,(cadr place) ,delta))
	(svref `(%atomic-incf-gvector ,@(cdr place) ,delta))))
    (if (and (symbolp place) (eq :special (variable-information place env)))
      (let* ((base (gensym))
             (offset (gensym)))
        `(multiple-value-bind (,base ,offset)
          (%symbol-binding-address ',place)
          (%atomic-incf-node ,delta ,base ,offset)))
      (signal-program-error "~S is not a special variable"  place))))
    
(defmacro atomic-incf (place)
  `(atomic-incf-decf ,place 1))

(defmacro atomic-decf (place)
  `(atomic-incf-decf ,place -1))

; Some of these macros were stolen from CMUCL.  Sort of ...

(defmacro iterate (name binds &body body)
  "Iterate Name ({(Var Initial-Value)}*) Declaration* Form*
  This is syntactic sugar for Labels.  It creates a local function Name with
  the specified Vars as its arguments and the Declarations and Forms as its
  body.  This function is then called with the Initial-Values, and the result
  of the call is return from the macro."
  (dolist (x binds)
    (unless (and (listp x)
                 (= (length x) 2))
      (signal-program-error "Malformed iterate variable spec: ~S." x)))

  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

;;;; The Collect macro:

;;; Collect-Normal-Expander  --  Internal
;;;
;;;    This function does the real work of macroexpansion for normal collection
;;; macros.  N-Value is the name of the variable which holds the current
;;; value.  Fun is the function which does collection.  Forms is the list of
;;; forms whose values we are supposed to collect.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)


(defun collect-normal-expander (n-value fun forms)
  `(progn
     ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
     ,n-value))


)

(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*
  Create a Let* which evaluates each Value-Expression, binding a temporary
  variable to the result, and wrapping the Let* around the result of the
  evaluation of Body.  Within the body, each Var is bound to the corresponding
  temporary variable."
  (iterate frob
           ((specs specs)
            (body body))
    (if (null specs)
      `(progn ,@body)
      (let ((spec (first specs)))
        (when (/= (length spec) 2)
          (signal-program-error "Malformed ~s binding spec: ~S." 'once-only spec))
        (let ((name (first spec))
              (exp-temp (gensym)))
          `(let ((,exp-temp ,(second spec))
                 (,name (gensym)))
             `(let ((,,name ,,exp-temp))
                ,,(frob (rest specs) body))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun form-symbol (first &rest others)
  (intern (apply #'concatenate 'simple-base-string (string first) (mapcar #'string others))))
)


;;; Collect-List-Expander  --  Internal
;;;
;;;    This function deals with the list collection case.  N-Tail is the pointer
;;; to the current tail of the list, which is NIL if the list is empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
       ,@(mapcar #'(lambda (form)
                     `(let ((,n-res (cons ,form nil)))
                        (cond (,n-tail
                               (setf (cdr ,n-tail) ,n-res)
                               (setq ,n-tail ,n-res))
                              (t
                               (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                 forms)
       ,n-value)))

;;;
;;;    The ultimate collection macro...
;;;

(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow.  Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form.  The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done.  The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  Initial-Value is the value that the collection starts out with, which
  defaults to NIL.  Function is the function which does the collection.  It is
  a function which will accept two arguments: the value to be collected and the
  current collection.  The result of the function is made the new value for the
  collection.  As a totally magical special-case, the Function may be Collect,
  which tells us to build a list in forward order; this is the default.  If an
  Initial-Value is supplied for Collect, the stuff will be rplacd'd onto the
  end.  Note that Function may be anything that can appear in the functional
  position, including macros and lambdas."
  
  
  (let ((macros ())
        (binds ()))
    (dolist (spec collections)
      (unless (<= 1 (length spec) 3)
        (signal-program-error "Malformed collection specifier: ~S." spec))
      (let ((n-value (gensym))
            (name (first spec))
            (default (second spec))
            (kind (or (third spec) 'collect)))
        
        (push `(,n-value ,default) binds)
        (if (eq kind 'collect)
          (let ((n-tail (gensym)))
            (if default
              (push `(,n-tail (last ,n-value)) binds)
              (push n-tail binds))
            (push `(,name (&rest args)
                          (collect-list-expander ',n-value ',n-tail args))
                  macros))
          (push `(,name (&rest args)
                        (collect-normal-expander ',n-value ',kind args))
                macros))))
    `(macrolet ,macros (let* ,(nreverse binds) (declare (ignorable ,@binds)) ,@body))))


;;; DEFENUM -- Internal Interface.
;;;
(defmacro defenum ((&key (prefix "") (suffix "") (start 0) (step 1))
                   &rest identifiers)
  (let ((results nil)
        (index 0)
        (start (eval start))
        (step (eval step)))
    (dolist (id identifiers)
      (multiple-value-bind
        (root docs)
        (if (consp id)
          (values (car id) (cdr id))
          (values id nil))
        (push `(defconstant ,(intern (concatenate 'simple-base-string
                                                  (string prefix)
                                                  (string root)
                                                  (string suffix)))
                 ,(+ start (* step index))
                 ,@docs)
              results))
      (incf index))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(nreverse results))))


;;; This does something like special binding, but the "bindings" established
;;; aren't thread-specific.

(defmacro let-globally ((&rest vars) &body body &environment env)
  (multiple-value-bind (body decls) (parse-body body env)
    (let* ((initforms nil)
           (psetform nil)
           (specvars nil)
           (restoreform nil))
      (flet ((pair-name-value (p)
               (if (atom p)
                 (values (require-global-symbol p env) nil)
                 (if (and (consp (%cdr p)) (null (%cddr p)))
                   (values (require-global-symbol (%car p) env) (%cadr p))
                   (signal-program-error "Invalid variable initialization form : ~s")))))
        (declare (inline pair-name-value))
        (dolist (v vars)
          (let* ((oldval (gensym))
                 (newval (gensym)))
            (multiple-value-bind (var valueform) (pair-name-value v)
              (push var specvars)
              (push var restoreform)
              (push oldval restoreform)
              (push `(,oldval (uvref (symptr->symvector ',var) #.target::symbol.vcell-cell)) initforms)
              (push `(,newval ,valueform) initforms)
              (push var psetform)
              (push newval psetform))))
        `(let ,(nreverse initforms)
           ,@decls
           (locally (declare (special ,@(nreverse specvars)))
             (unwind-protect
               (progn (psetq ,@(nreverse psetform)) ,@body)
               (psetq ,@(nreverse restoreform)))))))))
;;; From CLX.

;;; The good news is that this uses an interlocked load/store sequence
;;; and is fairly efficient.
;;; The bad news is that it only handles a few types of "place" forms.
;;; The good news is that CLX only uses a few types of "place" forms.

(defmacro conditional-store (place old-value new-value &environment env)
  (setq place (macroexpand place env))
  (if (atom place)
    ;; CLX uses special variables' value cells as place forms.
    (if (and (symbolp place)
             (eq :special (ccl::variable-information place env)))
      (let* ((base (gensym))
             (offset (gensym)))
        `(multiple-value-bind (,base ,offset)
          (ccl::%symbol-binding-address ',place)
          (ccl::%store-node-conditional ,offset ,base ,old-value ,new-value)))
      (signal-program-error "~s is not a special variable ." place))
    (let* ((sym (car place))
           (struct-transform (or (ccl::environment-structref-info sym env)
                                 (gethash sym ccl::%structure-refs%))))
      (if struct-transform
        (setq place (defstruct-ref-transform struct-transform (cdr place) env)
              sym (car place)))
      (if (member  sym '(svref ccl::%svref ccl::struct-ref))
        (let* ((v (gensym)))
          `(let* ((,v ,(cadr place)))
            (ccl::store-gvector-conditional ,(caddr place)
             ,v ,old-value ,new-value)))
        (signal-program-error "Don't know how to do conditional store to ~s" place)))))

(defmacro step (form)
  "The form is evaluated with single stepping enabled. Function calls
outside the lexical scope of the form can be stepped into only if the
functions in question have been compiled with sufficient DEBUG policy
to be at least partially steppable."
  form)

(defmacro target-arch-case (&rest clauses)
  `(case (backend-target-arch-name *target-backend*)
    ,@clauses))

(defmacro target-os-case (&rest clauses)
  `(ecase (backend-target-os *target-backend*)
    ,@clauses))

(defmacro target-word-size-case (&rest clauses)
  `(ecase (arch::target-nbits-in-word (backend-target-arch *target-backend*))
    ,@clauses))

(defmacro %get-natural (&body body)
  "A free copy of the next OpenMCL release to anyone who remembers Flakey Foont"
  (target-word-size-case
   (32 `(%get-unsigned-long ,@body))
   (64 `(%%get-unsigned-longlong ,@body))))

(defmacro %get-signed-natural (&body body)
  "And that's my final offer."
  (target-word-size-case
   (32 `(%get-signed-long ,@body))
   (64 `(%%get-signed-longlong ,@body))))

(declare-arch-specific-macro %target-kernel-global)

;;; This behaves like a function, but looks up the kernel global
;;; at compile time if possible. Probably should be done as a function
;;; and a compiler macro, but we can't define compiler macros yet,
;;; and I don't want to add it to "ccl:compiler;optimizers.lisp"
(declare-arch-specific-macro %get-kernel-global)

(declare-arch-specific-macro %get-kernel-global-ptr)

(declare-arch-specific-macro area-code)

(declare-arch-specific-macro nth-immediate)

(declare-arch-specific-macro set-nth-immediate)

(defsetf nth-immediate set-nth-immediate)

(defmacro do-consing-areas ((area) &body body)
  (let ((code (gensym)))
  `(do-gc-areas (,area)
     (let ((,code (%fixnum-ref ,area  (area-code))))
       (when (or (eql ,code area-readonly)
                 (eql ,code area-managed-static)
                 (eql ,code area-static)
                 (eql ,code area-dynamic))
         ,@body)))))

(declare-arch-specific-macro area-succ)


(defmacro do-gc-areas ((area) &body body)
  (let ((initial-area (gensym)))
    `(let* ((,initial-area (%get-kernel-global 'all-areas))
            (,area ,initial-area))
       (declare (fixnum ,initial-area ,area))
       (loop
         (setq ,area (%fixnum-ref ,area (area-succ)))
         (when (eql ,area ,initial-area)
           (return))
         ,@body))))

(defmacro with-ioblock-input-lock-grabbed ((ioblock) &body body)
  (let* ((i (gensym)))
    `(let* ((,i ,ioblock))
      (with-lock-grabbed ((ioblock-inbuf-lock ,i))
        (cond ((ioblock-device ,i)
               ,@body)
              (t (stream-is-closed (ioblock-stream ,i))))))))

(defmacro with-ioblock-output-lock-grabbed ((ioblock) &body body)
  (let* ((i (gensym)))
    `(let* ((,i ,ioblock))
      (with-lock-grabbed ((ioblock-outbuf-lock ,i))
        (cond ((ioblock-device ,i)
               ,@body)
              (t (stream-is-closed (ioblock-stream ,i))))))))
  

(defmacro with-stream-ioblock-input ((ioblock stream &key
                                             speedy)
                                  &body body)
  `(let ((,ioblock (stream-ioblock ,stream t)))
     ,@(when speedy `((declare (optimize (speed 3) (safety 0)))))
     (with-ioblock-input-locked (,ioblock) ,@body)))

(defmacro with-stream-ioblock-output ((ioblock stream &key
                                             speedy)
                                  &body body)
  `(let ((,ioblock (stream-ioblock ,stream t)))
     ,@(when speedy `((declare (optimize (speed 3) (safety 0)))))
     (with-ioblock-output-locked (,ioblock) ,@body)))

(defmacro with-stream-ioblock-output-maybe ((ioblock stream &key
						     speedy)
					    &body body)
  `(let ((,ioblock (stream-ioblock ,stream t)))
    ,@(when speedy `((declare (optimize (speed 3) (safety 0)))))
    (with-ioblock-output-locked-maybe (,ioblock) ,@body)))

(defmacro with-ioblock-input-locked ((ioblock) &body body)
  (let* ((lock (gensym)))
    `(let* ((,lock (locally (declare (optimize (speed 3) (safety 0)))
                                  (ioblock-inbuf-lock ,ioblock))))
      (if ,lock
        (with-lock-grabbed (,lock)
          (cond ((ioblock-device ,ioblock)
                 ,@body)
                (t (stream-is-closed (ioblock-stream ,ioblock)))))
        (progn
          (check-ioblock-owner ,ioblock)
          ,@body)))))

(defmacro with-ioblock-output-locked ((ioblock) &body body)
  (let* ((lock (gensym)))
    `(let* ((,lock (locally (declare (optimize (speed 3) (safety 0)))
                                  (ioblock-outbuf-lock ,ioblock))))
      (if ,lock
        (with-lock-grabbed (,lock)
          (cond ((ioblock-device ,ioblock)
                 ,@body)
                (t (stream-is-closed (ioblock-stream ,ioblock)))))
        (progn
          (check-ioblock-owner ,ioblock)
          ,@body)))))



(defmacro with-ioblock-output-locked-maybe ((ioblock) &body body)
  (let* ((lock (gensym)))
    `(let* ((,lock (locally (declare (optimize (speed 3) (safety 0)))
                     (ioblock-outbuf-lock ,ioblock))))
      (if ,lock
        (with-lock-grabbed (,lock)
          (cond ((ioblock-device ,ioblock)
                 ,@body)
                (t (stream-is-closed (ioblock-stream ,ioblock)))))
        (progn
          (check-ioblock-owner ,ioblock)
          ,@body)))))

;;; Use this when it's possible that the fd might be in
;;; a non-blocking state.  Body must return a negative of
;;; the os error number on failure.
;;; The use of READ-FROM-STRING below is certainly ugly, but macros
;;; that expand into reader-macros don't generally trigger the reader-macro's
;;; side-effects.  (Besides, the reader-macro might return a different
;;; value when the macro function is expanded than it did when the macro
;;; function was defined; this can happen during cross-compilation.)
(defmacro with-eagain (fd direction &body body)
  (let* ((res (gensym))
	 (eagain (symbol-value (read-from-string "#$EAGAIN"))))
   `(loop
      (let ((,res (progn ,@body)))
	(if (eql ,res (- ,eagain))
          (progn
            (setq ,res
                  (,(ecase direction
                           (:input 'process-input-would-block)
                           (:output 'process-output-would-block))
                    ,fd))
            (unless (eq ,res t) (return ,res)))
	  (return ,res))))))

(defmacro ignoring-eintr (&body body)
  (let* ((res (gensym))
         (eintr (symbol-value (read-from-string "#$EINTR"))))
    `(loop
       (let* ((,res (progn ,@body)))
         (unless (eql ,res (- ,eintr))
           (return ,res))))))

(defmacro ff-call-ignoring-eintr (&body body)
  (let* ((res (gensym))
         (eintr (symbol-value (read-from-string "#$EINTR"))))
    `(loop
       (let* ((,res (progn ,@body)))
         (declare (fixnum ,res))
         (when (< ,res 0)
           (setq ,res (%get-errno)))
         (unless (eql ,res (- ,eintr))
           (return ,res))))))

(defmacro basic-stream-ioblock (s)
  `(or (basic-stream.state ,s)
    (stream-is-closed ,s)))

(defsetf interrupt-level set-interrupt-level)

(defmacro %swap-u16 (val)
  (let* ((arg (gensym)))
    `(let* ((,arg ,val))
      (declare (type (unsigned-byte 16) ,arg))
      (logand #xffff (the fixnum (logior (the fixnum (ash ,arg -8))
                                         (the fixnum (ash ,arg 8))))))))

(defmacro %swap-u32 (val)
  (let* ((arg (gensym)))
    `(let ((,arg ,val))
      (declare (type (unsigned-byte 32) ,arg))
      (the (unsigned-byte 32) (logior (the (unsigned-byte 32)
                                        (ash (logand #xff ,arg) 24))
                                      (the (unsigned-byte 24)
                                        (logior
                                         (the (unsigned-byte 24) (ash (logand #xff00 ,arg) 8))
                                         (the (unsigned-byte 16)
                                           (logior
                                            (the (unsigned-byte 16) (ash (logand #xff0000 ,arg) -8))
                                            (the (unsigned-byte 8) (ash ,arg -24)))))))))))
    

(defmacro multiple-value-bind (varlist values-form &body body &environment env)
  (multiple-value-bind (body decls)
                       (parse-body body env)
    (let ((ignore (make-symbol "IGNORE")))
      `(multiple-value-call #'(lambda (&optional ,@varlist &rest ,ignore)
                                (declare (ignore ,ignore))
                                ,@decls
                                ,@body)
                            ,values-form))))

(defmacro multiple-value-setq (vars val)
  (if vars
    `(values (setf (values ,@(mapcar #'(lambda (s) (require-type s 'symbol)) vars))  ,val))
    `(prog1 ,val)))

(defmacro nth-value (n form)
  "Evaluate FORM and return the Nth value (zero based). This involves no
  consing when N is a trivial constant integer."
  `(car (nthcdr ,n (multiple-value-list ,form))))



(defmacro with-input-timeout (((stream-var &optional (stream-form stream-var)) timeout) &body body)
  "Execute body with STREAM-VAR bound to STREAM-FORM and with that stream's
stream-input-timeout set to TIMEOUT."
  (let* ((old-input-timeout (gensym))
         (stream (gensym)))
    `(let* ((,stream ,stream-form)
            (,stream-var ,stream)
            (,old-input-timeout (stream-input-timeout ,stream)))
      (unwind-protect
           (progn
             (setf (stream-input-timeout ,stream) ,timeout)
             ,@body)
        (setf (stream-input-timeout ,stream) ,old-input-timeout)))))

(defmacro with-output-timeout (((stream-var &optional (stream-form stream-var)) timeout) &body body)
  "Execute body with STREAM-VAR bound to STREAM-FORM and with that stream's
stream-output-timeout set to TIMEOUT."
  (let* ((old-output-timeout (gensym))
         (stream (gensym)))
    `(let* ((,stream ,stream-form)
            (,stream-var ,stream)
            (,old-output-timeout (stream-output-timeout ,stream)))
      (unwind-protect
           (progn
             (setf (stream-output-timeout ,stream) ,timeout)
             ,@body)
        (setf (stream-output-timeout ,stream) ,old-output-timeout)))))

;;; FORM returns a signed integer.  If it's non-negative, return that
;;; value, otherwise, return the (negative) errnor value returned by
;;; %GET-ERRNO
(defmacro int-errno-call (form)
  (let* ((value (gensym)))
    `(let* ((,value ,form))
      (if (< ,value 0)
        (%get-errno)
        ,value))))

(defmacro int-errno-ffcall (entry &rest args)
  `(int-errno-call (ff-call ,entry ,@args)))
