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

    
(defmacro defnx1 (name sym contextvar arglist &body forms &environment env)
  (unless (verify-lambda-list arglist t t t)
    (error "Invalid lambda list ~s" arglist))
  (multiple-value-bind (lambda-list whole environment)
      (normalize-lambda-list arglist t t)
    (multiple-value-bind (body local-decs) (parse-body forms env)
      (let ((whole-var (gensym "WHOLE"))
            (env-var (gensym "ENVIRONMENT")))
        (multiple-value-bind (bindings binding-decls)
            (%destructure-lambda-list lambda-list whole-var nil nil
                                      :cdr-p t
                                      :whole-p nil
                                      :use-whole-var t
                                      :default-initial-value nil)
          (when environment
            (setq bindings (nconc bindings (list `(,environment ,env-var)))))
          (when whole
            (setq bindings (nconc bindings (list `(,whole ,whole-var)))))
          (let ((fn `(nfunction ,name
                      (lambda (,contextvar ,whole-var ,env-var)
                        (declare (ignorable ,contextvar ,whole-var ,env-var))
                        (block ,name
                          (let* ,(nreverse bindings)
                            ,@(when binding-decls `((declare ,@binding-decls)))
                            ,@local-decs
                            ,@body)))))
                (theprogn ())
                (ysym (gensym)))
            `(let ((,ysym ,fn))
              ,(if (symbolp sym)
                   `(progn
                     (setf (gethash ',sym *nx1-alphatizers*) ,ysym)
                                        ;(proclaim '(inline ,sym))
                     (pushnew ',sym *nx1-compiler-special-forms*))
                   (dolist (x sym `(progn ,@(nreverse theprogn)))
                     (if (consp x)
                       (setq x (%car x))
                       (push `(pushnew ',x *nx1-compiler-special-forms*) theprogn))
                                        ;(push `(proclaim '(inline ,x)) theprogn)
                     (push `(setf (gethash ',x *nx1-alphatizers*) ,ysym) theprogn)))
              (record-source-file ',name 'function)
              ,ysym)))))))

(defun nx1-typespec-for-typep (typespec env &key (whine t))
  ;; Allow VALUES types here (or user-defined types that
  ;; expand to VALUES types).  We could do a better job
  ;; of this, but treat them as wild types.
  ;; Likewise, complex FUNCTION types can be legally used
  ;; in type declarations, but aren't legal args to TYPEP;
  ;; treat them as the simple FUNCTION type.
  (labels ((ctype-spec (ctype)
             (typecase ctype
               (function-ctype 'function)
               (values-ctype '*)
               (array-ctype
                  (let ((new (ctype-spec (array-ctype-element-type ctype))))
                    (when new
                      (list (if (array-ctype-complexp ctype) 'array 'simple-array)
                            new
                            (array-ctype-dimensions ctype)))))
               (negation-ctype
                  (let ((new (ctype-spec (negation-ctype-type ctype))))
                    (when new
                      `(not ,new))))
               (union-ctype
                  (let* ((types (union-ctype-types ctype))
                         (new (mapcar #'ctype-spec types)))
                    (unless (every #'null new)
                      `(or ,@(mapcar (lambda (new old) (or new (type-specifier old))) new types)))))
               (intersection-ctype
                  (let* ((types (intersection-ctype-types ctype))
                         (new (mapcar #'ctype-spec types)))
                    (unless (every #'null new)
                      `(and ,@(mapcar (lambda (new old) (or new (type-specifier old))) new types)))))
               (t nil))))
    (let* ((ctype (handler-case (values-specifier-type (nx-target-type typespec) env)
                    (parse-unknown-type (c)
                      (if whine
			(progn
			  (nx1-whine :unknown-type-in-declaration
				     (parse-unknown-type-specifier c))
			  *wild-type*)
			(specifier-type typespec env)))
                    (program-error (c)
		      (if whine
			(progn
			  (nx1-whine :invalid-type typespec c)
			  *wild-type*)
			(specifier-type typespec)))))
           (new (ctype-spec ctype)))
      (nx-target-type (type-specifier (if new (specifier-type new) ctype))))))

(defnx1 nx1-the the context (&whole call typespec form &environment env)
  (let* ((typespec (nx1-typespec-for-typep typespec env))
         (*nx-form-type* typespec)
         (transformed (nx-transform form env)))
    (flet ((fold-the ()
             (do* ()
                 ((or (atom transformed)
                      (not (eq (car transformed) 'the))))
               (destructuring-bind (ftype form) (cdr transformed)
                 (setq typespec (nx-target-type (nx1-type-intersect call typespec (nx1-typespec-for-typep ftype env)))
                       *nx-form-type* typespec
                       transformed form)))))
      (fold-the)
      (do* ((last transformed transformed))
          ()
        (setq transformed (nx-transform transformed env))
        (when (or (atom transformed)
                  (not (eq (car transformed) 'the)))
          (return))
        (fold-the)
        (when (eq transformed last)
          (return)))
      (if (and (nx-form-constant-p transformed env)
               (or (equal typespec '(values))
                   (not (typep (nx-form-constant-value transformed env)
                               (single-value-type (values-specifier-type typespec))))))
        (progn
          (nx1-whine :type call)
          (setq typespec '*))
        (setq typespec (nx-target-type
                        (or (nx1-type-intersect call
                                                typespec
                                                (nx1-typespec-for-typep (nx-form-type transformed env)env))
                            '*))))
      ;; Wimp out, but don't choke on (the (values ...) form)
      (when (and (consp typespec) (eq (car typespec) 'values))
        (setq typespec '*))
      (make-acode
       (%nx1-operator typed-form)
       typespec
       (let* ((*nx-form-type* typespec))
         (nx1-transformed-form context transformed env))
       (nx-declarations-typecheck env)))))

(defnx1 nx1-struct-ref struct-ref context (&whole whole structure offset)
  (if (not (fixnump (setq offset (nx-get-fixnum offset))))
    (nx1-treat-as-call context whole)
    (make-acode (%nx1-operator struct-ref)
                (nx1-form :value structure)
                (nx1-form :value offset))))

(defnx1 nx1-struct-set struct-set context (&whole whole structure offset newval)
  (if (not (fixnump (setq offset (nx-get-fixnum offset))))
    (nx1-treat-as-call context whole)
    (make-acode
     (%nx1-operator struct-set)
     (nx1-form :value structure)
     (nx1-form :value offset)
     (nx1-form :value newval))))

(defnx1 nx1-istruct-typep ((istruct-typep)) context (&whole whole thing type &environment env)
  (if (and (nx-form-constant-p type env) (non-nil-symbol-p (nx-form-constant-value type env)))
    (let* ((inner :value))
      (make-acode (%nx1-operator istruct-typep)
                  (nx1-immediate inner :eq)
                  (nx1-form inner thing)
                  (nx1-form inner `(register-istruct-cell ,type))))
    (nx1-treat-as-call context whole)))

(defnx1 nx1-make-list make-list context (&whole whole size &rest keys &environment env)
  (if (and keys 
             (or 
              (neq (list-length keys) 2)
              (neq (nx-transform (%car keys) env) :initial-element)))
    (nx1-treat-as-call context whole)
    (make-acode
     (%nx1-operator make-list)
     (nx1-form :value size)
     (nx1-form :value (%cadr keys)))))

(defun nx1-progn-body (context args)
  (if (null (cdr args))
    (nx1-form context (%car args))
    (make-acode (%nx1-operator progn)
                (collect ((forms))
                  (do* ()
                       ((null (cdr args))
                        (forms (nx1-form context (car args)))
                        (forms))
                    (forms (nx1-form nil (car args)))
                    (setq args (cdr args)))))))

(defun nx1-check-local-function-binding (funcname env)
  (if (and (symbolp funcname) (special-operator-p funcname))
    (nx-error "Can't lexically bind special operator ~s." funcname)
    (unless (nx-declared-notinline-p funcname env)
      (or (maybe-warn-about-shadowing-cl-function-name funcname)
          (when (and (symbolp funcname)
                     (gethash funcname *nx1-alphatizers*))
            (nx1-whine :special-fbinding funcname))))))

;;; New semantics: expansion functions are defined in current lexical environment
;;; vice null environment.  May be meaningless ...
(defnx1 nx1-macrolet macrolet context (defs &body body)
  (let* ((old-env *nx-lexical-environment*)
         (new-env (new-lexical-environment old-env))
         (names ()))
    (dolist (def defs)
      (destructuring-bind (name arglist &body mbody) def
        (setq name (nx-need-function-name name))
        (push name names)
        (nx1-check-local-function-binding name old-env)
        (push 
         (cons 
          name
          (cons
           'macro
           (multiple-value-bind (function warnings)
               (compile-named-function (parse-macro name arglist mbody old-env) :name name :env old-env)
             (setq *nx-warnings* (append *nx-warnings* warnings))
             function)))
         (lexenv.functions new-env))))
    (nx1-check-duplicate-bindings names 'macrolet)
    (let* ((*nx-lexical-environment* new-env))
      (with-nx-declarations (pending)
        (multiple-value-bind (body decls) (parse-body body new-env)
          (nx-process-declarations pending decls)
          (nx-effect-other-decls pending new-env)
          (nx1-progn-body context body))))))

;;; Does SYMBOL-MACROLET allow declarations ?  Yes ...
(defnx1 nx1-symbol-macrolet symbol-macrolet context (defs &body forms)
  (let* ((old-env *nx-lexical-environment*))
    (with-nx-declarations (pending)
      (multiple-value-bind (body decls)
                           (parse-body forms old-env nil)
        (nx-process-declarations pending decls)
        (let ((env *nx-lexical-environment*)
              (*nx-bound-vars* *nx-bound-vars*))
          (collect ((vars)
                    (symbols))
            (dolist (def defs)
              (destructuring-bind (sym expansion) def
                (let* ((var (nx-new-var pending sym))
                       (bits (nx-var-bits var)))
                  (symbols sym)
                  (when (%ilogbitp $vbitspecial bits)
                    (nx-error "SPECIAL declaration applies to symbol macro ~s" sym))
                  (nx-set-var-bits var (%ilogior (%ilsl $vbitignoreunused 1) bits))
                  (setf (var-ea var) (cons :symbol-macro expansion))
                  (vars var))))
            (nx1-check-duplicate-bindings (symbols) 'symbol-macrolet))
          (nx-effect-other-decls pending env)
          (nx1-env-body context body old-env))))))

(defnx1 nx1-progn progn context (&body args)
  (nx1-progn-body context args))

(defnx1 nx1-with-c-frame with-c-frame context (var &body body)
  (make-acode (%nx1-operator with-c-frame)
              (nx1-form context `(let* ((,var (%foreign-stack-pointer)))
                          ,@body))))

(defnx1 nx1-with-variable-c-frame with-variable-c-frame context (size var &body body)
  (make-acode (%nx1-operator with-variable-c-frame)
              (nx1-form :value size)
              (nx1-form context `(let* ((,var (%foreign-stack-pointer)))
                                  ,@body))))



(defnx1 nx1-unaryop ((%word-to-int) (uvsize)  (%reference-external-entry-point)
                     (%symbol->symptr) (%complex-single-float-realpart)
                     (%complex-single-float-imagpart)
                     (%complex-double-float-realpart)
                     (%complex-double-float-imagpart)
                     (realpart) (imagpart)) context
        (arg)
  (make-acode
   (%nx1-default-operator) (nx1-form :value arg)))

(defnx1 nx1-nullaryop ((%current-tcr) (%interrupt-poll) (%foreign-stack-pointer) (%current-frame-ptr)) context ()
  (make-acode (%nx1-default-operator)))

(defnx1 nx1-fixnum-ref ((%fixnum-ref) (%fixnum-ref-natural)) context (base &optional (offset 0))
  (make-acode (%nx1-default-operator)
              (nx1-form :value base)
              (nx1-form :value offset)))

(defnx1 nx1-fixnum-ref-double-float ((%fixnum-ref-double-float)) context (base &optional (index 0))
  (make-acode (%nx1-operator typed-form)
               'double-float
               (make-acode (%nx1-operator %fixnum-ref-double-float)
                           (nx1-form :value base)
                           (nx1-form :value index))))

(defnx1 nx2-fixnum-set-double-float ((%fixnum-set-double-float)) context (base index-or-val &optional (val nil val-p))
  (unless val-p
    (setq val index-or-val index-or-val 0))
  (make-acode (%nx1-operator typed-form)
               'double-float
               (make-acode (%nx1-operator %fixnum-set-double-float)
                           (nx1-form :value  base)
                           (nx1-form :value index-or-val)
                           (nx1-form :value val))))
               

(defnx1 nx1-type-unaryop ((typecode) (lisptag) (fulltag)) context
  (arg)
  (let* ((operator
	  (case *nx-sfname*
	    ((typecode) (%nx1-operator typecode))
	    ((lisptag) (%nx1-operator lisptag))
	    (( fulltag) (%nx1-operator fulltag)))))
    (make-acode
     operator (nx1-form :value arg))))
        

(defnx1 nx1-code-char ((code-char)) context (arg &environment env)
  (make-acode (if (nx-form-typep arg '(unsigned-byte 8) env)
                (%nx1-operator %code-char)
                (if (nx-form-typep arg 'valid-char-code env)
                  (%nx1-operator %valid-code-char)
                  (%nx1-operator code-char)))
              (nx1-form :value arg)))

(defnx1 nx1-char-code ((char-code)) context (arg &environment env)
  (make-acode (if (nx-form-typep arg 'character env)
                (%nx1-operator %char-code)
                (%nx1-operator char-code))
              (nx1-form :value arg)))

(defnx1 nx1-cXr ((car) (cdr)) context (arg &environment env)
  (let* ((op (if (eq *nx-sfname* 'car) (%nx1-operator car) (%nx1-operator cdr)))
         (inline-op (if (eq op (%nx1-operator car)) (%nx1-operator %car) (%nx1-operator %cdr))))
    (make-acode (if (or (nx-inline-car-cdr env) (nx-form-typep arg 'list env))
                  inline-op
                  op)
                (nx1-form :value arg env))))

(defnx1 nx1-rplacX ((rplaca) (rplacd)) context (pairform valform &environment env)
  (let* ((op (if (eq *nx-sfname* 'rplaca) (%nx1-operator rplaca) (%nx1-operator rplacd)))
         (inline-op (if (eq op (%nx1-operator rplaca)) (%nx1-operator %rplaca) (%nx1-operator %rplacd))))
    (make-acode (if (or (nx-inline-car-cdr env)
                                 (and (nx-trust-declarations env)
                                      (or (subtypep *nx-form-type* 'cons)
                                          (nx-form-typep pairform 'cons env))))
                  inline-op
                  op)
                (nx1-form :value pairform env)
                (nx1-form :value valform env))))

(defnx1 nx1-set-cXr ((set-car) (set-cdr)) context (pairform valform &environment env)
  (let* ((op (if (eq *nx-sfname* 'set-car) (%nx1-operator set-car) (%nx1-operator set-cdr)))
         (inline-op (if (eq op (%nx1-operator set-car)) (%nx1-operator %rplaca) (%nx1-operator %rplacd)))
         (inline-p (or (nx-inline-car-cdr env)
                            (and (nx-trust-declarations env)
                                 (or (subtypep *nx-form-type* 'cons)
                                     (nx-form-typep pairform 'cons env)))))
         (acode (make-acode (if inline-p inline-op op)
                            (nx1-form :value pairform env)
                            (nx1-form :value valform))))
    (if inline-p
      (make-acode (if (eq op (%nx1-operator set-car)) (%nx1-operator %car) (%nx1-operator %cdr)) acode)
      acode)))

(defun nx1-cc-binaryop (context op cc form1 form2)
  (declare (ignorable context))
  (make-acode op
              (nx1-immediate :value cc)
              (nx1-form :value form1) (nx1-form :value form2)))

(defnx1 nx1-ccEQ-unaryop ((characterp)  (endp) (consp) (base-char-p)) context (arg)
  (make-acode (%nx1-default-operator)
              (nx1-immediate :value :EQ)
              (nx1-form :value arg)))



(defnx1 nx1-ccEQ-binaryop ( (%ptr-eql) (eq)) context
        (form1 form2)
  (nx1-cc-binaryop context (%nx1-default-operator) :eq form1 form2))


(defnx1 nx1-ccNE-binaryop ((neq)) context
        (form1 form2)
  (nx1-cc-binaryop context (%nx1-default-operator) :ne form1 form2))

(defnx1 nx1-logbitp ((logbitp)) context (bitnum int &environment env)
  (if (and (nx-form-typep bitnum
                          (target-word-size-case (32 '(integer 0 29))
                                                 (64 '(integer 0 60))) env)
           (nx-form-typep int 'fixnum env))
    (nx1-cc-binaryop context (%nx1-operator %ilogbitp) :ne bitnum int)
    (make-acode (%nx1-operator logbitp)
                (nx1-form :value bitnum)
                (nx1-form :value int))))


  
(defnx1 nx1-ccGT-unaryop ((int>0-p)) context (arg)
  (make-acode (%nx1-default-operator)
              (nx1-immediate :value :gt)
              (nx1-form :value arg)))

(defnx1 nx1-macro-unaryop (multiple-value-list) context (arg)
  (make-acode
   (%nx1-default-operator) (nx1-form :value arg)))

(defnx1 nx1-atom ((atom)) context (arg)
  (nx1-form context `(not (consp ,arg))))

(defnx1 nx1-locally locally context (&body forms)
  (with-nx-declarations (pending)
    (let ((env *nx-lexical-environment*))
      (multiple-value-bind (body decls) (parse-body forms env  nil)
        (nx-process-declarations pending decls)
        (nx-effect-other-decls pending env)
         (setq body (nx1-progn-body context body))
         (if decls
           (make-acode (%nx1-operator %decls-body) body *nx-new-p2decls*)
           body)))))

(defnx1 nx1-%new-ptr (%new-ptr) context (size &optional clear-p)
  (make-acode (%nx1-operator %new-ptr)
              (nx1-form :value size)
              (nx1-form :value clear-p)))

;;; This might also want to look at, e.g., the last form in a progn:
;;;  (not (progn ... x)) => (progn ... (not x)), etc.
(defnx1 nx1-negation ((not) (null)) context (arg)
  (if (nx1-negate-form (setq arg (nx1-form context arg)))
    arg
    (make-acode (%nx1-operator not) (nx1-immediate context :eq) arg)))

(defun nx1-negate-form (form)
  (let* ((subform (nx-untyped-form form)))
    (when (and (acode-p subform) (typep (acode-operator subform) 'fixnum))  
      (let* ((op (acode-operator subform)))
        (declare (fixnum op))
        (when (logbitp operator-cc-invertable-bit op)
          (%rplaca 
           (acode-operands (car (acode-operands subform)))
           (acode-invert-condition-keyword (car (acode-operands (car (acode-operands subform))))))
          t)))))

;;; This is called from pass 1, and therefore shouldn't mess with "puntable bindings"
;;; (assuming, of course, that anyone should ...)
(defun nx-untyped-form (form)
  (while (and (acode-p form)
              (or (and (eq (acode-operator form) (%nx1-operator typed-form))
                       (null (nth 2 (acode-operands form))))
                  (eq (acode-operator form) (%nx1-operator type-asserted-form))))
    (setq form (cadr (acode-operands form))))
  form)



(defnx1 nx1-cxxr ((caar) (cadr) (cdar) (cddr)) context (form)
  (let* ((op *nx-sfname*))
    (let* ((inner (case op 
                       ((cdar caar) 'car)
                       (t 'cdr)))
              (outer (case op
                       ((cdar cddr) 'cdr)
                       (t 'car))))
         (nx1-form :value `(,outer (,inner ,form))))))      

(defnx1 nx1-%int-to-ptr ((%int-to-ptr)) context (int)
  (make-acode 
   (%nx1-operator %consmacptr%)
   (make-acode (%nx1-operator %immediate-int-to-ptr) 
               (nx1-form :value int))))

(defnx1 nx1-%ptr-to-int ((%ptr-to-int)) context (ptr)
   (make-acode (%nx1-operator typed-form)
               *nx-target-natural-type*
               (make-acode 
                (%nx1-operator %immediate-ptr-to-int)
                (make-acode (%nx1-operator %macptrptr%) 
                            (nx1-form :value ptr)))))

(defnx1 nx1-%null-ptr-p ((%null-ptr-p)) context (ptr)
  (nx1-form :value `(%ptr-eql ,ptr (%int-to-ptr 0))))

(defnx1 nx1-binop ( (%ilsl) (%ilsr) (%iasr)
                   (cons) (%temp-cons)) context
        (arg1 arg2)
  (make-acode (%nx1-default-operator) (nx1-form :value arg1) (nx1-form :value arg2)))



(defnx1 nx1-%misc-ref ((%misc-ref)) context (v i)
  (make-acode (%nx1-operator uvref) (nx1-form :value v) (nx1-form :value i)))




(defnx1 nx1-schar ((schar)) context (s i &environment env)
  (make-acode (%nx1-operator %sbchar) (nx1-form :value s env) (nx1-form :value i env)))


;;; This has to be ultra-bizarre because %schar is a macro.
;;; %schar shouldn't be a macro.
(defnx1 nx1-%schar ((%schar)) context (arg idx &environment env)
        (let* ((arg (nx-transform arg env))
               (idx (nx-transform idx env))
               (argvar (make-symbol "STRING"))
               (idxvar (make-symbol "INDEX")))
          (nx1-form context
                    `(let* ((,argvar ,arg)
                            (,idxvar ,idx))
                      (declare (optimize (speed 3) (safety 0)))
                      (declare (simple-base-string ,argvar))
                      (schar ,argvar ,idxvar)) env)))
        
(defnx1 nx1-%scharcode ((%scharcode)) context (arg idx)
  (make-acode (%nx1-operator %scharcode) (nx1-form :value arg)(nx1-form :value idx)))


(defnx1 nx1-svref ((svref) (%svref)) context (&environment env v i)
  (make-acode (if (nx-inhibit-safety-checking env)
                (%nx1-operator %svref)
                (%nx1-default-operator))
              (nx1-form :value v env)
              (nx1-form :value i)))

(defnx1 nx1-%slot-ref ((%slot-ref)) context (instance idx)
  (make-acode (%nx1-default-operator)
              (nx1-form :value instance)
              (nx1-form :value idx)))


(defnx1 nx1-%err-disp ((%err-disp)) context (&rest args)
  (make-acode (%nx1-operator %err-disp)
              (nx1-arglist args)))                       
              
(defnx1 nx1-macro-binop ((nth-value)) context (arg1 arg2)
  (make-acode (%nx1-default-operator) (nx1-form :value arg1) (nx1-form :value arg2)))

(defnx1 nx1-%typed-miscref ((%typed-miscref) (%typed-misc-ref)) context (subtype uvector index)
  (make-acode (%nx1-operator %typed-uvref) 
                (nx1-form :value subtype) 
                (nx1-form :value uvector) 
                (nx1-form :value index)))



(defnx1 nx1-%typed-miscset ((%typed-miscset) (%typed-misc-set)) context (subtype uvector index newvalue)
  (make-acode (%nx1-operator %typed-uvset) 
                (nx1-form :value subtype) 
                (nx1-form :value uvector) 
                (nx1-form :value index) 
                (nx1-form :value newvalue)))

(defnx1 nx1-logior-2 ((logior-2)) context (arg-1 arg-2)
  (make-acode (%nx1-operator logior2)
              (nx1-form :value arg-1)
              (nx1-form :value arg-2)))

(defnx1 nx1-logxor-2 ((logxor-2)) context (arg-1 arg-2)
  (make-acode (%nx1-operator logxor2)
              (nx1-form :value arg-1)
              (nx1-form :value arg-2)))

(defnx1 nx1-logand-2 ((logand-2)) context (arg-1 arg-2)
  (make-acode (%nx1-operator logand2)
              (nx1-form :value arg-1)
              (nx1-form :value arg-2)))



(defnx1 nx1-require ((require-simple-vector)
                     (require-simple-string)
                     (require-integer)
                     (require-list)
                     (require-fixnum)
                     (require-real)
                     (require-character)
                     (require-number)
                     (require-symbol)
                     (require-s8)
                     (require-u8)
                     (require-s16)
                     (require-u16)
                     (require-s32)
                     (require-u32)
                     (require-s64)
                     (require-u64)) context
        (arg &environment env)

  (if (nx-inhibit-safety-checking env)
    (let* ((op *nx-sfname*)
           (type (case op
                   (require-simple-vector 'simple-vector)
                   (require-simple-string 'simple-string)
                   (require-integer 'integer)
		   (require-list 'list)
		   (require-fixnum 'fixnum)
		   (require-real 'real)
		   (require-character 'character)
		   (require-number 'number)
		   (require-symbol 'symbol)
		   (require-s8 '(signed-byte 8))
		   (require-u8 '(unsigned-byte 8))
		   (require-s16 '(signed-byte 16))
		   (require-u16 '(unsigned-byte 16))
		   (require-s32 '(signed-byte 32))
		   (require-u32 '(unsigned-byte 32))
		   (require-s64 '(signed-byte 64))
		   (require-u64 '(unsigned-byte 64)))))
      (nx1-form context `(the ,type ,arg)))
    (make-acode (%nx1-default-operator) (nx1-form :value arg))))

(defnx1 nx1-%marker-marker ((%unbound-marker) (%slot-unbound-marker) (%illegal-marker)) context ()
  (make-acode (%nx1-default-operator)))

(defnx1 nx1-throw (throw) context (tag valuesform)
  (make-acode (%nx1-operator throw) (nx1-form :value tag) (nx1-form :value valuesform)))


;;; This is still used in inlining/lambda application.
;;; The tricky parts of handling inlining reasonably have to do with
;;; processing the body (including &optional/&key forms) in the environment
;;; in which the lambda was defined (e.g., macros and symbol-macros.)
;;; (I'm not sure that the traditional MCL/OpenMCL frontend handles
;;; these cases 100% correctly, but it seems difficult to do this
;;;  correctly without being able to jerk around with the environment,
;;; for a variety of reasons.)
;;; A lambda application - ((lambda ()) ...) is applied in the same
;;; environment it's defined in, so the hard case involves inlining
;;; functions whose environment may contain syntactic constructs
;;; not present in the current environment (and which does -not- generally
;;; contain whatever randomness is floating around at the point of
;;; application.)
(defun nx1-destructure (context lambda-list bindform cdr-p &whole-allowed-p forms &optional (body-env *nx-lexical-environment*))
  (declare (ignore cdr-p))
  (let* ((old-env body-env)
         (*nx-bound-vars* *nx-bound-vars*)
         (bindform (nx1-form :value bindform)))
    (if (not (verify-lambda-list lambda-list t &whole-allowed-p))
      (nx-error "Invalid lambda-list ~s" lambda-list)
      (let* ((*nx-lexical-environment* body-env)
             (*nx-bound-vars* *nx-bound-vars*))
        (with-nx-declarations (pending)
          (multiple-value-bind (body decls) (parse-body forms body-env t)
          (let* ((temp-name (gensym))
                 (temp-var (nx-new-var pending temp-name))
                 (vars (list temp-var))
                 (vals (list bindform))
                 (binding (nx1-note-var-binding temp-var bindform))
                 (var-bound-vars (if binding (list binding))))
            (let* ((acode (make-acode (%nx1-operator let*)
                                      (list temp-var)
                                      (list bindform)
                                      (nx1-env-body context
                                                    `((destructuring-bind ,lambda-list ,temp-name
                                                      ,@decls
                                                      ,@body))
                                                    old-env)
                                      *nx-new-p2decls*)))
              (nx1-check-var-bindings var-bound-vars)
              (nx1-punt-bindings vars vals)
              acode))))))))

(defnx1 nx1-%setf-macptr ((%setf-macptr)) context (ptr newval)
  (let* ((arg1 (nx1-form :value ptr))
         (arg2 (nx1-form :value newval)))
    (if (and (consp arg1) (eq (%car arg1) (%nx1-operator %consmacptr%)))
      ;e.g. (%setf-macptr (%null-ptr) <foo>)
      (make-acode (%nx1-operator %consmacptr%)
                  (make-acode (%nx1-operator progn)
                              (list arg1 (make-acode (%nx1-operator %macptrptr%) arg2))))
      (make-acode (%nx1-operator %setf-macptr) arg1 arg2))))

(defnx1 nx1-%setf-double-float ((%setf-double-float)) context (double-node double-val)
  (make-acode (%nx1-operator %setf-double-float) (nx1-form :value double-node) (nx1-form :value double-val)))

(defnx1 nx1-%setf-short-float ((%setf-short-float) (%setf-single-float)) context (short-node short-val)
  (target-word-size-case
   (32
    (make-acode (%nx1-operator %setf-short-float) (nx1-form :value short-node) (nx1-form :value short-val)))
   (64
    (error "%SETF-SHORT-FLOAT makes no sense on 64-bit platforms."))))

   
(defnx1 nx1-%inc-ptr ((%inc-ptr)) context (ptr &optional (increment 1))
  (make-acode (%nx1-operator %consmacptr%)
              (make-acode (%nx1-operator %immediate-inc-ptr)
                          (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptr))
                          (nx1-form :value increment))))

(defnx1 nx1-svset ((svset) (%svset)) context (&environment env vector index value)
  (make-acode (if (nx-inhibit-safety-checking env)
                (%nx1-operator %svset)
                (%nx1-default-operator))
              (nx1-form :value vector env) (nx1-form :value index) (nx1-form :value value)))

(defnx1 nx1-+ ((+-2)) context (num1 num2)
  (make-acode (%nx1-operator add2)
              (nx1-form :value num1)
              (nx1-form :value num2)))

  



(defnx1 nx1-*-2 ((*-2)) context (&environment env num1 num2)
  (make-acode (%nx1-operator mul2) (nx1-form :value num1 env) (nx1-form :value num2 env)))

(defnx1 nx1-%negate ((%negate)) context (num &environment env)
  (if (nx-form-typep num 'fixnum env)
    (if (subtypep *nx-form-type* 'fixnum)
      (make-acode (%nx1-operator %%ineg)(nx1-form :value num))
      (make-acode (%nx1-operator %ineg) (nx1-form :value num)))
    (let* ((acode (make-acode (%nx1-operator minus1) (nx1-form :value num env))))
      (if (nx-form-typep num 'double-float env)
        (make-acode (%nx1-operator typed-form)
                    'double-float
                    acode)
        (if (nx-form-typep num 'single-float env)
          (make-acode (%nx1-operator typed-form)
                      'single-float
                      acode)
          acode)))))

          

        
(defnx1 nx1--2 ((--2)) context (num0 num1)
  (make-acode (%nx1-operator sub2)
                      (nx1-form :value num0)
                      (nx1-form :value num1)))
      
(defnx1 nx1-/-2 ((/-2)) context (num0 num1 &environment env)
  (if (and (nx-form-typep num0 'double-float env)
           (nx-form-typep num1 'double-float env))
    (make-acode (%nx1-operator %double-float/-2) (nx1-form :value num0) (nx1-form :value num1))
    (if (and (nx-form-typep num0 'short-float env)
             (nx-form-typep num1 'short-float env))
      (make-acode (%nx1-operator %short-float/-2) (nx1-form :value num0) (nx1-form :value num1))
      (make-acode (%nx1-operator div2) (nx1-form :value num0) (nx1-form :value num1)))))



(defnx1 nx1-numcmp ((<-2) (>-2) (<=-2) (>=-2) (=-2) (/=-2)) context (num1 num2)
  (let* ((op *nx-sfname*))
    (make-acode (%nx1-operator numcmp)
                (make-acode
                 (%nx1-operator immediate)
                 (ecase op
                   (<-2 :LT)
                   (<=-2 :LE)
                   (=-2 :EQ)
                   (/=-2 :NE)
                   (>=-2 :GE)
                   (>-2 :GT)))
                 (nx1-form :value num1)
                 (nx1-form :value num2))))


             

(defnx1 nx1-uvset ((uvset) (%misc-set)) context (vector index value)
  (make-acode (%nx1-operator uvset)
              (nx1-form :value vector)
              (nx1-form :value index)
              (nx1-form :value value)))

(defnx1 nx1-set-schar ((set-schar)) context (s i v)
  (make-acode (%nx1-operator %set-sbchar) (nx1-form :value s) (nx1-form :value i) (nx1-form :value v)))



(defnx1 nx1-%set-schar ((%set-schar)) context (arg idx char &environment env)
  (let* ((arg (nx-transform arg env))
         (idx (nx-transform idx env))
         (char (nx-transform char env))
         (argvar (make-symbol "ARG"))
         (idxvar (make-symbol "IDX"))
         (charvar (make-symbol "CHAR")))
    (nx1-form context
              `(let* ((,argvar ,arg)
                      (,idxvar ,idx)
                      (,charvar ,char))
                (declare (optimize (speed 3) (safety 0)))
                (declare (simple-base-string ,argvar))
                (setf (schar ,argvar ,idxvar) ,charvar))
              env)))

(defnx1 nx1-%set-scharcode ((%set-scharcode)) context (s i v)
    (make-acode (%nx1-operator %set-scharcode)
                (nx1-form :value s)
                (nx1-form :value i)
                (nx1-form :value v)))
              

(defnx1 nx1-list-vector-values ((list) (vector) (values) (%temp-list)) context (&rest args)
  (make-acode (%nx1-default-operator) (nx1-formlist context args)))



(defnx1 nx1-%gvector ( (%gvector)) context (&rest args)
  (make-acode (%nx1-operator %gvector) (nx1-arglist args)))

(defnx1 nx1-quote quote context (form)
  (nx1-immediate context form))

(defnx1 nx1-list* ((list*)) context (first &rest rest)
  (make-acode (%nx1-operator list*) (nx1-arglist (cons first rest) 1)))



(defnx1 nx1-or or context (&whole whole &optional (firstform nil firstform-p) &rest moreforms)
  (if (not firstform-p)
    (nx1-form context nil)
    (if (null moreforms)
      (nx1-form context firstform)
      (progn
        (make-acode (%nx1-operator or) (nx1-formlist context (%cdr whole)))))))

(defun nx1-1d-vref (context env arr dim0 &optional uvref-p)
  (declare (ignorable context))
  (let* ((simple-vector-p (nx-form-typep arr 'simple-vector env))
	 (simple-string-p (unless simple-vector-p (nx-form-typep arr 'simple-string env)))
         (simple-1d-array-p (unless (or simple-vector-p simple-string-p)
                              (nx-form-typep arr '(simple-array * (*)) env)))
         (array-type (specifier-type  (nx-form-type arr env)))
         (type-keyword (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        array-type)))
    (if (and simple-1d-array-p type-keyword)
      (make-acode (%nx1-operator %typed-uvref) 
                  (nx1-immediate :value type-keyword)
                  (nx1-form :value arr)
                  (nx1-form :value dim0))
      (let* ((op (cond (simple-1d-array-p (%nx1-operator uvref))
                       (simple-string-p (%nx1-operator %sbchar))
                       (simple-vector-p 
                        (if (nx-inhibit-safety-checking env) (%nx1-operator %svref) (%nx1-operator svref)))
                       (uvref-p (%nx1-operator uvref))
                       (t (%nx1-operator %aref1)))))
        (make-acode op (nx1-form :value arr) (nx1-form :value dim0))))))
  
(defnx1 nx1-aref ((aref)) context (&whole whole &environment env arr &optional (dim0 nil dim0-p)
                                  &rest other-dims)
   (if (and dim0-p (null other-dims))
     (nx1-1d-vref context env arr dim0)
     (nx1-treat-as-call context whole)))

(defnx1 nx1-uvref ((uvref)) context (&environment env arr dim0)
  (nx1-1d-vref context env arr dim0 t))

(defnx1 nx1-%aref2 ((%aref2)) context (&whole whole &environment env arr i j)
  ;; Bleah.  Breaks modularity.  Specialize later.
  (target-arch-case
   (:x8632
    (return-from nx1-%aref2 (nx1-treat-as-call context whole))))

  (let* ((arch (backend-target-arch *target-backend*))
         (ctype (specifier-type (nx-form-type arr env)))
         (atype (if (csubtypep ctype (specifier-type '(array * (* *)))) ctype))
         (simple-atype (if (and atype
                                (csubtypep atype (specifier-type '(simple-array * (* *)))))
                         atype))
         (type-keyword (if atype
                         (funcall
                          (arch::target-array-type-name-from-ctype-function arch)
                          atype))))
    (if (and type-keyword simple-atype)
      (let* ((dims (array-ctype-dimensions atype))
             (dim0 (car dims))
             (dim1 (cadr dims)))
        (make-acode (%nx1-operator simple-typed-aref2)
                    (nx1-form :value type-keyword)
                    (nx1-form :value arr)
                    (nx1-form :value i)
                    (nx1-form :value j)
                    (nx1-form :value (if (typep dim0 'fixnum) dim0))
                    (nx1-form :value (if (typep dim1 'fixnum) dim1))))
      (make-acode (%nx1-operator general-aref2)
                  (nx1-form :value arr)
                  (nx1-form :value i)
                  (nx1-form :value j)))))

(defnx1 nx1-%aref3 ((%aref3)) context (&whole whole &environment env arr i j k)
  ;; Bleah.  Breaks modularity.  Specialize later.
  (target-arch-case
   (:x8632
    (return-from nx1-%aref3 (nx1-treat-as-call context whole))))

  (let* ((arch (backend-target-arch *target-backend*))
         (ctype (specifier-type (nx-form-type arr env)))
         (atype (if (csubtypep ctype (specifier-type '(array * (* * *)))) ctype))
         (simple-atype (if (and atype
                                (csubtypep atype (specifier-type '(simple-array * (* * *)))))
                         atype))
         (type-keyword (if atype
                         (funcall
                          (arch::target-array-type-name-from-ctype-function arch)
                          atype))))
    (if (and type-keyword simple-atype)
      (let* ((dims (array-ctype-dimensions atype))
             (dim0 (car dims))
             (dim1 (cadr dims))
             (dim2 (caddr dims)))
        (make-acode (%nx1-operator simple-typed-aref3)
                    (nx1-form :value type-keyword)
                    (nx1-form :value arr)
                    (nx1-form :value i)
                    (nx1-form :value j)
                    (nx1-form :value k)
                    (nx1-form :value (if (typep dim0 'fixnum) dim0))
                    (nx1-form :value (if (typep dim1 'fixnum) dim1))
                    (nx1-form :value (if (typep dim2 'fixnum) dim2))))
      (make-acode (%nx1-operator general-aref3)
                  (nx1-form :value arr)
                  (nx1-form :value i)
                  (nx1-form :value j)
                  (nx1-form :value k)))))

(defun nx1-1d-vset (context arr newval dim0 env)
  (let* ((simple-vector-p (nx-form-typep arr 'simple-vector env))
         (string-p (unless simple-vector-p 
                     (if (nx-form-typep arr 'string env)
                       (or (nx-form-typep arr 'simple-string env)
                           (return-from nx1-1d-vset (nx1-form context `(set-char ,arr ,newval ,dim0)))))))
         (simple-1d-array-p (unless (or simple-vector-p string-p) 
                              (nx-form-typep arr '(simple-array * (*)) env)))
         (array-type (specifier-type  (nx-form-type arr env)))
         (type-keyword (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        array-type)))
         (if (and type-keyword simple-1d-array-p)
             (make-acode (%nx1-operator %typed-uvset) 
                         (nx1-immediate :value type-keyword)
                         (nx1-form :value arr)
                         (nx1-form :value newval)
                         (nx1-form :value dim0))
             (let* ((op (cond (simple-1d-array-p (%nx1-operator uvset))
                              (string-p (%nx1-operator %set-sbchar))
                              (simple-vector-p (if (nx-inhibit-safety-checking env) (%nx1-operator %svset) (%nx1-operator svset)))
                              (t (%nx1-operator aset1)))))
               (if op
                   (make-acode
                    op
                    (nx1-form :value arr)
                    (nx1-form :value newval)
                    (nx1-form :value dim0))
                   (nx1-form context `(,(if string-p 'set-schar '%aset1) ,arr ,newval ,dim0)))))))

(defnx1 nx1-aset ((aset)) context (&whole whole 
                                  arr newval 
                                  &optional (dim0 nil dim0-p)
                                  &environment env
                                  &rest other-dims)
   (if (and dim0-p (null other-dims))
       (nx1-1d-vset context arr newval dim0 env)
       (nx1-treat-as-call context whole)))
            
(defnx1 nx1-%aset2 ((%aset2)) context (&whole whole &environment env arr i j new)
  ;; Bleah.  Breaks modularity.  Specialize later.
  (target-arch-case
   (:x8632
    (return-from nx1-%aset2 (nx1-treat-as-call context whole))))

  (let* ((arch (backend-target-arch *target-backend*))
         (ctype (specifier-type (nx-form-type arr env)))
         (atype (if (csubtypep ctype (specifier-type '(array * (* *)))) ctype))
         (simple-atype (if (and atype
                                (csubtypep atype (specifier-type '(simple-array * (* *)))))
                         atype))
         (type-keyword (if atype
                         (funcall
                          (arch::target-array-type-name-from-ctype-function arch)
                          atype))))

    (if (and type-keyword simple-atype)
      (let* ((dims (array-ctype-dimensions atype))
             (dim0 (car dims))
             (dim1 (cadr dims)))
        (make-acode (%nx1-operator simple-typed-aset2)
                    (nx1-form :value type-keyword)
                    (nx1-form :value arr)
                    (nx1-form :value i)
                    (nx1-form :value j)
                    (nx1-form :value new)
                    (nx1-form :value (if (typep dim0 'fixnum) dim0))
                    (nx1-form :value (if (typep dim1 'fixnum) dim1))))
            (make-acode (%nx1-operator general-aset2)
                  (nx1-form :value arr)
                  (nx1-form :value i)
                  (nx1-form :value j)
                  (nx1-form :value new)))))

(defnx1 nx1-%aset3 ((%aset3)) context (&whole whole &environment env arr i j k new)
  ;; Bleah.  Breaks modularity.  Specialize later.
  (target-arch-case
   (:x8632
    (return-from nx1-%aset3 (nx1-treat-as-call context whole))))

  (let* ((arch (backend-target-arch *target-backend*))
         (ctype (specifier-type (nx-form-type arr env)))
         (atype (if (csubtypep ctype (specifier-type '(array * (* * *)))) ctype))
         (simple-atype (if (and atype
                                (csubtypep atype (specifier-type '(simple-array * (* * *)))))
                         atype))
         (type-keyword (if atype
                         (funcall
                          (arch::target-array-type-name-from-ctype-function arch)
                          atype))))

    (if (and type-keyword simple-atype)
      (let* ((dims (array-ctype-dimensions atype))
             (dim0 (car dims))
             (dim1 (cadr dims))
             (dim2 (caddr dims)))
        (make-acode (%nx1-operator simple-typed-aset3)
                    (nx1-form :value type-keyword)
                    (nx1-form :value arr)
                    (nx1-form :value i)
                    (nx1-form :value j)
                    (nx1-form :value k)
                    (nx1-form :value new)
                    (nx1-form :value (if (typep dim0 'fixnum) dim0))
                    (nx1-form :value (if (typep dim1 'fixnum) dim1))
                    (nx1-form :value (if (typep dim2 'fixnum) dim2))))
            (make-acode (%nx1-operator general-aset3)
                  (nx1-form :value arr)
                  (nx1-form :value i)
                  (nx1-form :value j)
                  (nx1-form :value k)
                  (nx1-form :value new)))))

(defnx1 nx1-prog1 (prog1 multiple-value-prog1) context (save &body args)
  (let* ((l (list (nx1-form :value save))))
    (make-acode 
     (%nx1-default-operator) 
     (dolist (arg args (nreverse l))
       (push (nx1-form nil arg) l)))))

(defnx1 nx1-if if context (test true &optional false)
  (if (null true)
    (if (null false)
      (return-from nx1-if (nx1-form context `(progn ,test nil)))
      (psetq test `(not ,test) true false false true)))
  (let ((test-form (nx1-form :value test))
        ;; Once hit a conditional, no more duplicate warnings
        (*compiler-warn-on-duplicate-definitions* nil))
    (make-acode (%nx1-operator if) test-form (nx1-form context true) (nx1-form context false))))

(defnx1 nx1-%debug-trap dbg context (&optional arg)
  (make-acode (%nx1-operator %debug-trap) (nx1-form :value arg)))
        
(defnx1 nx1-setq setq context (&whole whole &rest args &environment env &aux res)
  (when (%ilogbitp 0 (length args))
    (nx-error "Odd number of forms in ~s ." whole))
  (while args
    (let* ((sym (nx-need-var (%car args) nil))
           (val (%cadr args))
           (declared-type (nx-declared-type sym env)))
      (when (nx-declarations-typecheck env)
        (unless (or (eq declared-type t)
                    (and (consp val) (eq (%car val) 'the) (equal (cadr val) declared-type)))
          (setq val `(the ,declared-type ,val))
          (nx-note-source-transformation (caddr val) val)))
      (multiple-value-bind (expansion win) (macroexpand-1 sym env)
	(if win
            (push (nx1-form context `(setf ,expansion ,val)) res)
            (multiple-value-bind (info inherited catchp)
		(nx-lex-info sym)
	      (push
	       (if (eq info :symbol-macro)
		   (progn
		     (nx-set-var-bits catchp
				      (%ilogior
				       (%ilsl $vbitsetq 1)
				       (%ilsl $vbitreffed 1)
				       (nx-var-bits catchp)))
		     (nx1-form context `(setf ,inherited ,val)))
		   (progn
		     (let ((*nx-form-type* declared-type))
		       (setq val (nx1-typed-form context val env)))
		     (if (and info (neq info :special))
			 (progn
			   (nx1-check-assignment sym env)
                           (if inherited
                             (nx-set-var-bits info (%ilogior (%ilsl $vbitsetq 1)
                                                             (%ilsl $vbitnoreg 1) ; I know, I know ... Someday ...
                                                             (nx-var-bits info)))
                             (nx-set-var-bits info (%ilogior2 (%ilsl $vbitsetq 1) (nx-var-bits info))))
			   (nx-adjust-setq-count info 1 catchp) ; In the hope that that day will come ...
                           (let* ((type (var-declared-type info)))
                             (when type
                               (setq val (make-acode (%nx1-operator typed-form)
                                                     type val))))
			   (make-acode (%nx1-operator setq-lexical) info val))
			 (make-acode
			  (if (nx1-check-special-ref sym info)
			      (progn
				(nx-record-xref-info :references sym)
				(nx-record-xref-info :sets sym)
			        (if (nx-global-p sym env)
			          (%nx1-operator global-setq)
			          (%nx1-operator setq-special)))
			    (%nx1-operator setq-free)) ; Screw: no object lisp.  Still need setq-free ? For constants ?
			  (nx1-note-vcell-ref sym)
			  val))))
	       res)))
	(setq args (%cddr args)))))
  (make-acode (%nx1-operator progn) (nreverse res)))

;;; See if we're trying to setq something that's currently declared "UNSETTABLE"; whine if so.
;;; If we find a contour in which a "SETTABLE NIL" vdecl for the variable exists, whine.
;;; If we find a contour in which a "SETTABLE T" vdecl for the variable exists. or
;;;    the contour in which the variable's bound, return nil.
;;; Should find something ...
(defun nx1-check-assignment (sym env)
  (loop
    (unless (and env (istruct-typep env 'lexical-environment))
      (return))
    (dolist (decl (lexenv.vdecls env))
      (when (and (eq (car decl) sym)
               (eq (cadr decl) 'settable))
        (unless (cddr decl)
          (nx1-whine :unsettable sym))
        (return-from nx1-check-assignment nil)))
    (let ((vars (lexenv.variables env)))
      (unless (atom vars)
        (dolist (var vars)
          (when (eq (var-name var) sym) (return-from nx1-check-assignment nil)))))
    (setq env (lexenv.parent-env env))))

;;; The cleanup issue is a little vague (ok, it's a -lot- vague) about the environment in
;;; which the load-time form is defined, although it apparently gets "executed in a null
;;; lexical environment".  Ignoring the fact that it's meaningless to talk of executing
;;; something in a lexical environment, we can sort of infer that it must also be defined
;;; in a null lexical environment.

(defnx1 nx1-load-time-value (load-time-value) context (&environment env form &optional read-only-p)
  ;; Validate the "read-only-p" argument
  (if (and read-only-p (neq read-only-p t)) (require-type read-only-p '(member t nil)))
  ;; Then ignore it.
    (multiple-value-bind (function warnings)
                         (compile-named-function 
                          `(lambda () ,form)
                          ;; pass in the definition env for special decls
                          :env (definition-environment env)
                          :load-time-eval-token *nx-load-time-eval-token*
                          :target (backend-name *target-backend*))
      (setq *nx-warnings* (append *nx-warnings* warnings))
      (if *nx-load-time-eval-token*
        (nx1-immediate context (list *nx-load-time-eval-token* `(funcall ,function)))
    
      (make-acode (%nx1-operator load-time-value)
              (make-acode (%nx1-operator immediate)
                          (funcall function))))))

(defun nx1-catch-body (context body)
  (let* ((temp (new-lexical-environment *nx-lexical-environment*)))
    (setf (lexenv.variables temp) 'catch)
    (let* ((*nx-lexical-environment* (new-lexical-environment temp)))
      (nx1-progn-body context body))))

(defnx1 nx1-catch (catch) context (operation &body body)
  (make-acode (%nx1-operator catch) (nx1-form :value operation) (nx1-catch-body context body)))

(defnx1 nx1-%badarg ((%badarg)) context (badthing right-type &environment env)
  (make-acode (%nx1-operator %badarg2) 
              (nx1-form :value badthing) 
              (nx1-form :value (or (if (nx-form-constant-p right-type env) (%typespec-id (nx-form-constant-value right-type env)))
			    right-type))))

(defnx1 nx1-unwind-protect (unwind-protect) context (protected-form &body cleanup-form)
  (if cleanup-form
    (make-acode (%nx1-operator unwind-protect) 
                (nx1-catch-body context (list protected-form))
                (nx1-progn-body context cleanup-form))
    (nx1-form context protected-form)))

;;; Check that something that's supposed to be a proper list of
;;; symbols is; error otherwise.
;;; This is called only by the compiler output of a PROGV form.
;;; It checks for the maximum length that the progvsave subprim
;;; can handle.
(defun %progv-check-symbol-list (list)
  (let ((length (list-length list)))
    (when (not length)
      (error 'simple-program-error
             :format-control "PROGV: ~S is not a proper list."
             :format-arguments (list list)))
    (dolist (symbol list)
      (unless (symbolp symbol)
        (error 'simple-type-error
               :expected-type 'symbol :datum symbol
               :format-control "PROGV: ~S is not a symbol."
               :format-arguments (list symbol)))
      (when (constant-symbol-p symbol)
        (error 'simple-program-error
               :format-control "PROGV: symbol ~S names a constant variable which cannot be rebound."
               :format-arguments (list symbol)))
      (when (logbitp $sym_vbit_global (the fixnum (%symbol-bits symbol)))
        (error 'simple-program-error
               :format-control "PROGV: symbol ~S names a global variable which cannot be rebound."
               :format-arguments (list symbol)))
      (ensure-binding-index symbol))
    list))

(defnx1 nx1-progv progv context (symbols values &body body)
  (make-acode (%nx1-operator progv)
              (nx1-form :value `(%progv-check-symbol-list ,symbols))
              (nx1-form :value values)
              (nx1-catch-body context body)))

(defun nx1-apply-fn (context fn args spread)
  (let* ((sym (nx1-func-name fn))
	 (afunc (and (non-nil-symbol-p sym) (nth-value 1 (nx-lexical-finfo sym)))))
    (when (and afunc (eq afunc *nx-call-next-method-function*))
      (setq fn (let ((new (list 'quote (if (or (car args) (cdr args))
					 '%call-next-method-with-args
					 '%call-next-method))))
		 (nx-note-source-transformation fn new)
		 new)
	    sym nil
	    args (cons (var-name *nx-next-method-var*) args)))
    (nx1-typed-call context (if (non-nil-symbol-p sym) sym (nx1-form :value fn)) args spread)))


(defnx1 nx1-apply ((apply)) context (&whole call fn arg &rest args &environment env)
  (let ((last (%car (last (push arg args)))))
    (if (and (nx-form-constant-p last env)
	     (null (nx-form-constant-value last env)))
      (nx1-form context (let ((new `(funcall ,fn ,@(butlast args))))
		  (nx-note-source-transformation call new)
		  new))
      (nx1-apply-fn context fn args t))))

(defnx1 nx1-%apply-lexpr ((%apply-lexpr)) context (fn arg &rest args)
  (nx1-apply-fn context fn (cons arg args) 0))




(defnx1 nx1-%defun %defun context (&whole w def &optional (doc nil doc-p) &environment env)
  (declare (ignorable doc doc-p def))
  (when *backend-use-linear-scan*
    (linear-scan-bailout '%defun))
  ;; Pretty bogus.
  (if (and (consp def)
           (eq (%car def) 'nfunction)
           (consp (%cdr def))
           (or (symbolp (%cadr def)) (setf-function-name-p (%cadr def))))
    (note-function-info (%cadr def) (caddr def) env)) 
  (nx1-treat-as-call context w))

(defnx1 nx1-function function context (arg &aux fn afunc)
  (cond ((symbolp arg)
	 (when (macro-function arg *nx-lexical-environment*)
	   (nx-error
	    "~S can't be used to reference lexically visible macro ~S." 
	    'function arg))
	 (if (multiple-value-setq (fn afunc) (nx-lexical-finfo arg))
	   (progn
	     (when afunc 
	       (incf (afunc-fn-refcount afunc))
	       (when (%ilogbitp $fbitbounddownward (afunc-bits afunc))
		 (incf (afunc-fn-downward-refcount afunc))))
	     (nx1-symbol context (%cddr fn)))
	   (progn
	     (while (setq fn (assq arg *nx-synonyms*))
	       (setq arg (%cdr fn)))
             (let* ((env *nx-lexical-environment*))
               	(unless (or (nx1-find-call-def arg env)
		    (find-ftype-decl arg env)
		    (eq arg *nx-global-function-name*))
                  (nx1-whine :undefined-function arg)))
	     (nx1-form context `(%function ',arg)))))
	((setf-function-name-p arg)
	 (nx1-form context `(function ,(nx-need-function-name arg))))
	((lambda-expression-p arg)
	 (nx1-ref-inner-function nil arg))
	(t
	 (nx-error "~S is not a function name or lambda expression" arg))))

(defnx1 nx1-nfunction nfunction context (name def)
 (nx1-ref-inner-function name def))

(defun nx1-ref-inner-function (name def &optional afunc)
  (setq afunc (nx1-compile-inner-function name def afunc))
  (setf (afunc-fn-refcount afunc) 1)
  (nx1-afunc-ref afunc))

(defun nx1-compile-inner-function (name def p
                                        &optional (env *nx-lexical-environment*)
                                        &aux (q *nx-current-function*))
  (unless p (setq p (make-afunc)))
  (setf (afunc-parent p) q)
  (setf (afunc-parent q) *nx-parent-function*)
  (setf (afunc-tags q) *nx-tags*)
  (setf (afunc-blocks q) *nx-blocks*)
  (setf (afunc-inner-functions q) (push p *nx-inner-functions*))
  (setf (lexenv.lambda env) q)
  (if *nx-current-code-note*
    (let* ((*nx-current-code-note* (nx-ensure-code-note def *nx-current-code-note*)))
      (nx1-compile-lambda name def p q env *nx-current-compiler-policy* *nx-load-time-eval-token*)) ;returns p.
    (nx1-compile-lambda name def p q env *nx-current-compiler-policy* *nx-load-time-eval-token*)))

(defun nx1-afunc-ref (afunc)
  (let ((op (if (afunc-inherited-vars afunc)
              (%nx1-operator closed-function)
              (%nx1-operator simple-function)))
        (ref (acode-unwrapped-form (afunc-ref-form afunc))))
    (if ref
      (progn
        (setf (acode-operator ref) op)
        ref)
      (setf (afunc-ref-form afunc)
            (make-acode
             op
             afunc)))))
    
(defnx1 nx1-%function %function context (form &aux symbol)
  (let ((sym (nx1-form :value form)))
    (if (and (eq (acode-operator sym) (%nx1-operator immediate))
             (setq symbol (car (acode-operands sym)))
             (symbolp symbol))
      (make-acode (%nx1-default-operator) symbol)
      (make-acode (%nx1-operator call) (nx1-immediate context '%function) (list nil (list sym))))))

(defnx1 nx1-tagbody tagbody context (&rest args)
  (let* ((newtags nil)
         (*nx-lexical-environment* (new-lexical-environment *nx-lexical-environment*))
	 (pending (make-pending-declarations))
         (*nx-bound-vars* *nx-bound-vars*)
         (catchvar (nx-new-temp-var pending "tagbody-catch-tag"))
         (indexvar (nx-new-temp-var pending "tagbody-tag-index"))
         (counter (list 0))
         (looplabel (cons nil nil))
         (*nx-tags* *nx-tags*))
    (dolist (form args)
      (when (atom form)
        (if (or (symbolp form) (integerp form))
          (if (assoc form newtags)
            (nx-error "Duplicate tag in TAGBODY: ~S." form)
            (push (list form nil counter catchvar nil nil) newtags))
          (nx-error "Illegal form in TAGBODY: ~S." form))))
    (dolist (tag (setq newtags (nreverse newtags)))
      (push tag *nx-tags*))
    (let* ((body nil)
           (level *nx-loop-nesting-level*)
           (*nx-loop-nesting-level* level))
           
      (dolist (form args (setq body (nreverse body)))
        (push 
         (if (atom form)
           (let ((info (nx-tag-info form)))
             (when (eql level *nx-loop-nesting-level*)
               (setq *nx-loop-nesting-level* (1+ level)))
             (%rplaca (%cdr (%cdr (%cdr (%cdr info)))) t)
             (make-acode (%nx1-operator tag-label) info))
           (nx1-form nil form))
         body))
      (if (eq 0 (%car counter))
        (make-acode (%nx1-operator local-tagbody) newtags body)
        (progn
          (nx-set-var-bits catchvar (logior (nx-var-bits catchvar)
                                            (%ilsl $vbitdynamicextent 1)))
          (nx-inhibit-register-allocation)   ; There are alternatives ...
          (dolist (tag (reverse newtags))
            (when (%cadr tag)
              (push  
               (nx1-form context `(if (eql ,(var-name indexvar) ,(%cadr tag)) (go ,(%car tag))))
               body)))
          (make-acode
           (%nx1-operator let*)
           (list catchvar indexvar)
           (list (make-acode (%nx1-operator cons) (make-nx-nil) (make-nx-nil)) (make-nx-nil))
           (make-acode
            (%nx1-operator local-tagbody)
            (list looplabel)
            (list
             (make-acode (%nx1-operator tag-label) looplabel)
             (make-acode
              (%nx1-operator if)
              (make-acode 
               (%nx1-operator setq-lexical)
               indexvar
               (make-acode 
                (%nx1-operator catch)
                (nx1-form :value (var-name catchvar)) 
                (make-acode
                 (%nx1-operator local-tagbody)
                 newtags
                 body)))
              (make-acode (%nx1-operator local-go) looplabel)
              (make-nx-nil))))
           0))))))



(defnx1 nx1-go go context (tag)
  (multiple-value-bind (info closed)
                       (nx-tag-info tag)
    (unless info (nx-error "Can't GO to tag ~S." tag))
    (if (not closed)
      (let ((defnbackref (cdr (cdr (cdr (cdr info))))))
        (if (car defnbackref) 
          (rplaca (cdr defnbackref) t))
        (make-acode (%nx1-operator local-go) info))
      (progn

        (make-acode
         (%nx1-operator throw) (nx1-symbol :value (var-name (cadddr info))) (nx1-form :value closed))))))




;;; address-expression should return a fixnum; that's our little
;;; secret.  result spec can be NIL, :void, or anything that an
;;; arg-spec can be.  arg-spec can be :double, :single, :address,
;;; :signed-doubleword, :unsigned-doubleword, :signed-fullword,
;;; :unsigned-fullword, :signed-halfword, :unsigned-halfword,
;;; :signed-byte, or :unsigned-byte
;;; On ppc64, :hybrid-int-float, :hybrid-float-float, and :hybrid-float-int
;;; can also be used to express some struct-by-value cases.

(defparameter *arg-spec-keywords*
  '(:double-float :single-float :address :signed-doubleword
    :unsigned-doubleword :signed-fullword :unsigned-fullword
    :signed-halfword :unsigned-halfword :signed-byte :unsigned-byte
    :hybrid-int-float :hybrid-float-int :hybrid-float-float))

(defun nx1-ff-call-internal (context address-expression arg-specs-and-result-spec operator )
  (declare (ignorable context))
  (let* ((specs ())         
         (vals ())
         (register-spec-seen nil)
         (arg-specs (butlast arg-specs-and-result-spec))
         (result-spec (car (last arg-specs-and-result-spec))))
    (unless (evenp (length arg-specs))
      (error "odd number of arg-specs"))
    (loop
      (when (null arg-specs) (return))
      (let* ((arg-keyword (pop arg-specs))
	     (value (pop arg-specs)))
        (if (or (memq arg-keyword *arg-spec-keywords*)
		(typep arg-keyword 'unsigned-byte))
          (progn 
            (push arg-keyword specs)
            (push value vals))
          (if (eq arg-keyword :registers)
            (if register-spec-seen
              (error "duplicate :registers in ~s" arg-specs-and-result-spec)
              (progn
                (setq register-spec-seen t)
                (push arg-keyword specs)
                (push value vals)))
            (error "Unknown argument spec: ~s" arg-keyword)))))
    (unless (or (eq result-spec :void)
		(memq result-spec *arg-spec-keywords*))
      (error "Unknown result spec: ~s" result-spec))
    (make-acode (%nx1-operator typed-form)
                (case result-spec
                  (:double-float 'double-float)
                  (:single-float 'single-float)
                  (:address 'macptr)
                  (:signed-doubleword '(signed-byte 64))
                  (:unsigned-doubleword '(unsigned-byte 64))
                  (:signed-fullword '(signed-byte 32))
                  (:unsigned-fullword '(unsigned-byte 32))
                  (:signed-halfword '(signed-byte 16))
                  (:unsigned-halfword '(unsigned-byte 16))
                  (:signed-byte '(signed-byte 8))
                  (:unsigned-byte '(unsigned-byte 8))
                  (t t))
                (make-acode operator
                            (nx1-form :value address-expression)
                            (nreverse specs)
                            (mapcar (lambda (val) (nx1-form :value val)) (nreverse vals))
                            result-spec
                            nil)
                nil)))

(defnx1 nx1-ff-call ((%ff-call)) context (address-expression &rest arg-specs-and-result-spec)
   (nx1-ff-call-internal
    context address-expression arg-specs-and-result-spec
    (ecase (backend-name *target-backend*)
      ((:linuxppc32 :linuxarm :darwinarm :androidarm) (%nx1-operator eabi-ff-call))
      ((:darwinppc32 :linuxppc64 :darwinppc64) (%nx1-operator poweropen-ff-call))
      ((:darwinx8632 :linuxx8632 :win32 :solarisx8632 :freebsdx8632) (%nx1-operator i386-ff-call))
      ((:linuxx8664 :freebsdx8664 :darwinx8664 :solarisx8664 :win64) (%nx1-operator ff-call)))))


  
(defnx1 nx1-block block context (blockname &body forms)
  (let* ((*nx-blocks* *nx-blocks*)
         (*nx-lexical-environment* (new-lexical-environment *nx-lexical-environment*))
         (*nx-bound-vars* *nx-bound-vars*)
         (tagvar (nx-new-temp-var (make-pending-declarations)))
         (thisblock (cons (setq blockname (nx-need-sym blockname)) (cons tagvar context)))
         (body nil))
    (push thisblock *nx-blocks*)
    (setq body (nx1-progn-body context forms))
    (%rplacd thisblock nil)
    (let ((tagbits (nx-var-bits tagvar)))
      (if (not (%ilogbitp $vbitclosed tagbits))
        (if (neq 0 (nx-var-root-nrefs tagvar))
          (make-acode 
           (%nx1-operator local-block)
           thisblock
           body)
          body)
        (progn
          (nx-set-var-bits tagvar (%ilogior (%ilsl $vbitdynamicextent 1) tagbits))
          (nx-inhibit-register-allocation)   ; Could also set $vbitnoreg in all setqed vars, or keep track better
          (make-acode
           (%nx1-operator local-block)
           thisblock
           (make-acode
            (%nx1-operator let)
            (list tagvar)
            (list (make-acode (%nx1-operator cons) (nx1-form :value nil) (nx1-form :value nil)))
            (make-acode
             (%nx1-operator catch)
             (nx-make-lexical-reference tagvar)
             body)
            0)))))))

(defnx1 nx1-return-from return-from context (blockname &optional value)
  (multiple-value-bind (info closed)
      (nx-block-info (setq blockname (nx-need-sym blockname)))
    (unless info (nx-error "Can't RETURN-FROM block : ~S." blockname))
    (destructuring-bind (var . block-context) (cdr info)
      (unless closed (nx-adjust-ref-count var))
      (make-acode 
       (if closed
         (%nx1-operator throw)
         (%nx1-operator local-return-from))
       (if closed
         (nx1-symbol context (var-name var ))
         info)
     (nx1-form (if closed :value block-context) value)))))

(defnx1 nx1-funcall ((funcall)) context (&whole call func &rest args &environment env)
  (let ((name (nx1-func-name func)))
    (if (or (null name)
	    (and (symbolp name) (macro-function name env)))
      (nx1-typed-call context (nx1-form :value func) args nil)
      (progn
	(when (consp name) ;; lambda expression
	  (nx-note-source-transformation func name))
	;; This picks up call-next-method evil.
	(nx1-form context (let ((new-form (cons name args)))
                            (nx-note-source-transformation call new-form)
                            new-form))))))

(defnx1 nx1-multiple-value-call multiple-value-call context (value-form &rest args)
  (make-acode (%nx1-default-operator)
              (nx1-form :value value-form)
              (nx1-formlist context args)))

(defnx1 nx1-compiler-let compiler-let context (bindings &body forms)
  (let* ((vars nil)
         (varinits nil))
    (dolist (pair bindings)
      (push (nx-pair-name pair) vars)
      (push (eval (nx-pair-initform pair)) varinits))
   (progv (nreverse vars) (nreverse varinits) (nx1-catch-body context forms))))

(defnx1 nx1-fbind fbind context (fnspecs &body body &environment old-env)
  (let* ((fnames nil)
         (vars nil)
         (vals nil))
    (dolist (spec fnspecs (setq vals (nreverse vals)))
      (destructuring-bind (fname initform) spec
        (push (setq fname (nx-need-function-name fname)) fnames)
        (push (nx1-form :value initform) vals)))
    (let* ((new-env (new-lexical-environment old-env))
           (*nx-bound-vars* *nx-bound-vars*)
           (*nx-lexical-environment* new-env)
	   (pending (make-pending-declarations)))
      (dolist (fname fnames)        
        (let ((var (nx-new-var pending (make-symbol (symbol-name fname)))))
          (nx-set-var-bits var (%ilogior (%ilsl $vbitignoreunused 1)
                                         (nx-var-bits var)))
          (let ((afunc (make-afunc)))
            (setf (afunc-bits afunc) (%ilsl $fbitruntimedef 1))
            (setf (afunc-lfun afunc) var)
            (push var vars)
            (push (cons fname (cons 'function (cons afunc (var-name var)))) (lexenv.functions new-env)))))
      (make-acode
       (%nx1-operator let)
       vars
       vals
       (nx1-env-body context body old-env)
       *nx-new-p2decls*))))

(defun maybe-warn-about-shadowing-cl-function-name (funcname)
  (when (and (symbolp funcname)
             (fboundp funcname)
             (eq (symbol-package funcname) (find-package "CL")))
    (nx1-whine :shadow-cl-package-definition funcname)
    t))





(defnx1 nx1-flet flet context (defs &body forms)
  (with-nx-declarations (pending)
    (let* ((env *nx-lexical-environment*)
           (*nx-lexical-environment* env)
           (*nx-bound-vars* *nx-bound-vars*)
           (new-env (new-lexical-environment env))
           (names nil)
           (funcs nil)
           (pairs nil)
           (fname nil)
           (name nil)
           (fnames ()))
      (multiple-value-bind (body decls) (parse-body forms env nil)
        (nx-process-declarations pending decls)
        (dolist (def defs (setq names (nreverse names) funcs (nreverse funcs)))
          (destructuring-bind (funcname lambda-list &body flet-function-body) def
            (setq fname (nx-need-function-name funcname))
            (unless (verify-lambda-list lambda-list)
              (nx-error "Invalid lambda-list ~s in FLET local function ~s" lambda-list fname))
            (push fname fnames)
            (nx1-check-local-function-binding funcname env)            
            (multiple-value-bind (body decls)
                                 (parse-body flet-function-body env)
              (let ((func (make-afunc))
                    (expansion `(lambda ,lambda-list
                                  ,@decls
                                  (block ,(if (consp funcname) (%cadr funcname) funcname)
                                    ,@body))))
                (nx-note-source-transformation def expansion)
                (setf (afunc-environment func) env
                      (afunc-lambdaform func) expansion)
                (push func funcs)
                (when (and *nx-next-method-var*
                             (eq funcname 'call-next-method)
                             (null *nx-call-next-method-function*))
                    (setq *nx-call-next-method-function* func))             
                (push (cons funcname func) pairs)
                (if (consp funcname)
                  (setq funcname fname))
                (push (setq name (make-symbol (symbol-name funcname))) names)
                (push (cons funcname (cons 'function (cons func name))) (lexenv.functions new-env))))))
        (nx1-check-duplicate-bindings fnames 'flet)
        (let ((vars nil)
              (rvars nil)
              (rfuncs nil))
          (dolist (sym names vars) (push (nx-new-var pending sym) vars))
          (nx-effect-other-decls pending new-env)
          (setq body (let* ((*nx-lexical-environment* new-env))
                       (nx1-dynamic-extent-functions vars new-env)
                       (nx1-env-body context body env)))
          (dolist (pair pairs)
            (let ((afunc (cdr pair))
                  (var (pop vars)))
              (when (or (afunc-callers afunc)
                        (neq 0 (afunc-fn-refcount afunc))
                        (neq 0 (afunc-fn-downward-refcount afunc)))
                (push (nx1-compile-inner-function (%car pair)
                                                  (afunc-lambdaform afunc)
                                                  afunc
                                                  (afunc-environment afunc))
                      rfuncs)
                (push var rvars))))
          (nx-reconcile-inherited-vars rfuncs)
          (dolist (f rfuncs) (nx1-afunc-ref f))
          (make-acode
           (%nx1-operator flet)
           rvars
           rfuncs
           body
           *nx-new-p2decls*))))))

(defun nx1-dynamic-extent-functions (vars env)
  (let ((bits nil)
        (varinfo nil))
    (dolist (decl (lexenv.fdecls env))
      (let ((downward-guy (if (eq (cadr decl) 'dynamic-extent) (car decl))))
        (when downward-guy
          (multiple-value-bind (finfo afunc) (nx-lexical-finfo downward-guy)
            (when (and afunc 
                       (not (%ilogbitp $fbitdownward (setq bits (afunc-bits afunc))))
                       (setq varinfo (and (consp (%cdr finfo)) (nx-lex-info (%cddr finfo))))
                       (memq varinfo vars))
              (setf (afunc-bits afunc) 
                    (%ilogior 
                     bits 
                     (%ilsl $fbitdownward 1)
                     (%ilsl $fbitbounddownward 1)))
              (nx-set-var-bits varinfo (%ilogior (%ilsl $vbitdynamicextent 1) (nx-var-bits varinfo))))))))))
          
(defnx1 nx1-labels labels context (defs &body forms)
  (with-nx-declarations (pending)
    (let* ((env *nx-lexical-environment*)
           (old-env (lexenv.parent-env env))
           (*nx-bound-vars* *nx-bound-vars*)
           (func nil)
           (funcs nil)
           (funcrefs nil)
           (bodies nil)
           (vars nil)
           (blockname nil)
           (fname nil)
           (name nil)
           (fnames ()))
      (multiple-value-bind (body decls) (parse-body forms env nil)
        (dolist (def defs (setq funcs (nreverse funcs) bodies (nreverse bodies)))
          (destructuring-bind (funcname lambda-list &body labels-function-body) def
            (nx1-check-local-function-binding funcname env)
            (push (setq func (make-afunc)) funcs)
            (setq blockname funcname)
            (setq fname (nx-need-function-name funcname))
            (push fname fnames)
            (unless (verify-lambda-list lambda-list)
              (nx-error "Invalid lambda-list ~s in LABELS local function ~s" lambda-list funcname))
            (when (consp funcname)
              (setq blockname (%cadr funcname) funcname fname))
            (let ((var (nx-new-var pending (setq name (make-symbol (symbol-name funcname))))))
              (nx-set-var-bits var (%ilsl $vbitignoreunused 1))
              (push var vars))
            (push func funcrefs)
            (multiple-value-bind (body decls)
                                 (parse-body labels-function-body old-env)
              (push (cons funcname (cons 'function (cons func name))) (lexenv.functions env))
              (let* ((expansion `(lambda ,lambda-list 
                                   ,@decls 
                                   (block ,blockname
                                     ,@body))))
                (nx-note-source-transformation def expansion)
                (setf (afunc-lambdaform func) expansion
                      (afunc-environment func) env)
                (push (cons funcname expansion)
                      bodies)))))
        (nx1-dynamic-extent-functions vars env)
        (dolist (def bodies)
          (nx1-compile-inner-function (car def) (cdr def) (setq func (pop funcs))))
        (nx-process-declarations pending decls)
        (nx-effect-other-decls pending env)
        (setq body (nx1-env-body context body old-env))
        (nx-reconcile-inherited-vars funcrefs)
        (dolist (f funcrefs) (nx1-afunc-ref f))
        (nx1-check-duplicate-bindings fnames 'labels)
        (make-acode
         (%nx1-operator labels)
         (nreverse vars)
         (nreverse funcrefs)
         body
         *nx-new-p2decls*)))))



(defnx1 nx1-set-bit ((%set-bit)) context (ptr offset &optional (newval nil newval-p))
  (unless newval-p (setq newval offset offset 0))
  (make-acode
   (%nx1-operator %set-bit)
   (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptr))
   (nx1-form :value offset)
   (nx1-form :value newval)))
               
(defnx1 nx1-set-xxx ((%set-ptr) (%set-long)  (%set-word) (%set-byte)
                     (%set-unsigned-long) (%set-unsigned-word) (%set-unsigned-byte)) context
        (ptr offset &optional (newval nil new-val-p) &aux (op *nx-sfname*))
  (unless new-val-p (setq newval offset offset 0))
  (make-acode
   (%nx1-operator %immediate-set-xxx)
   (case op
     (%set-ptr 0)
     (%set-word 2)
     (%set-unsigned-word (logior 32 2))
     (%set-byte 1)
     (%set-unsigned-byte (logior 32 1))
     (%set-unsigned-long (logior 32 4))
     (t 4))
   (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptr))
   (nx1-form :value offset)
   (nx1-form :value newval)))

(defnx1 nx1-set-64-xxx ((%%set-unsigned-longlong) (%%set-signed-longlong)) context 
        (&whole w ptr offset newval &aux (op *nx-sfname*))
  (target-word-size-case
   (32 (nx1-treat-as-call context w))
   (64
    (make-acode
     (%nx1-operator %immediate-set-xxx)
     (case op
       (%%set-signed-longlong 8)
       (t (logior 32 8)))
     (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptr))
     (nx1-form :value offset)
     (nx1-form :value newval)))))


(defnx1 nx1-get-bit ((%get-bit)) context (ptrform &optional (offset 0))
  (make-acode
   (%nx1-operator typed-form)
   'bit
   (make-acode
    (%nx1-operator %get-bit)
    (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptrform))
    (nx1-form :value offset))))

(defnx1 nx1-get-64-xxx ((%%get-unsigned-longlong) (%%get-signed-longlong)) context
  (&whole w ptrform offsetform)
  (target-word-size-case
   (32 (nx1-treat-as-call context w))
   (64
    (let* ((flagbits (case *nx-sfname*
                       (%%get-unsigned-longlong 8)
                       (%%get-signed-longlong (logior 32 8))))
           (signed (logbitp 5 flagbits)))
      (make-acode (%nx1-operator typed-form)
                  (if signed
                    '(signed-byte 64)
                    '(unsigned-byte 64))
                (make-acode 
                 (%nx1-operator immediate-get-xxx)
                 flagbits
                 (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptrform))
                 (nx1-form :value  offsetform)))))))

(defnx1 nx1-get-xxx ((%get-long)  (%get-full-long)  (%get-signed-long)
                     (%get-fixnum) 
                     (%get-word) (%get-unsigned-word)
                     (%get-byte) (%get-unsigned-byte)
                     (%get-signed-word) 
                     (%get-signed-byte) 
                     (%get-unsigned-long)) context
  (ptrform &optional (offset 0))
  (let* ((sfname *nx-sfname*)
         (flagbits (case sfname
                     ((%get-long %get-full-long  %get-signed-long) (logior 4 32))
                     (%get-fixnum (logior 4 32 64))
		     
                     ((%get-word %get-unsigned-word) 2)
                     (%get-signed-word (logior 2 32))
                     ((%get-byte %get-unsigned-byte) 1)
                     (%get-signed-byte (logior 1 32))
                     (%get-unsigned-long 4)))
         (signed (logbitp 5 flagbits)))
    (declare (fixnum flagbits))
    (make-acode (%nx1-operator typed-form)
                (case (logand 15 flagbits)
                  (4 (if (logbitp 6 flagbits)
                       'fixnum
                       (if signed
                         '(signed-byte 32)
                         '(unsigned-byte 32))))
                  (2 (if signed
                       '(signed-byte 16)
                       '(unsigned-byte 16)))
                  (1 (if signed
                       '(signed-byte 8)
                       '(unsigned-byte 8))))
                (make-acode 
                 (%nx1-operator immediate-get-xxx)
                 flagbits
                 (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptrform))
                 (nx1-form :value offset)))))

(defnx1 nx1-%get-ptr ((%get-ptr) ) context (ptrform &optional (offset 0))
  (make-acode
   (%nx1-operator %consmacptr%)
   (make-acode
    (%nx1-operator immediate-get-ptr)
    (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptrform))
    (nx1-form :value offset))))

(defnx1 nx1-%get-float ((%get-single-float)
			(%get-double-float)) context (ptrform &optional (offset 0))
  (make-acode
   (%nx1-operator typed-form)
   (if (eq *nx-sfname* '%get-single-float)
     'single-float
     'double-float)
   (make-acode
    (%nx1-default-operator)
    (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptrform))
    (nx1-form :value offset))))

(defnx1 nx1-%set-float ((%set-single-float)
			(%set-double-float)) context (ptrform offset &optional (newval nil newval-p))
  (unless newval-p
    (setq newval offset
	  offset 0))
    (make-acode
     (%nx1-operator typed-form)
     (if (eq *nx-sfname* '%set-single-float)
       'single-float
       'double-float)
     (make-acode
      (%nx1-default-operator)
      (make-acode (%nx1-operator %macptrptr%) (nx1-form :value ptrform))
      (nx1-form :value offset)
      (nx1-form :value newval))))

(defnx1 nx1-let let context (pairs &body forms &environment old-env)
  (collect ((vars)
            (vals)
            (varbindings))
    (with-nx-declarations (pending)
      (multiple-value-bind (body decls)
                           (parse-body forms *nx-lexical-environment* nil)
        (nx-process-declarations pending decls)
        ;; Make sure that the initforms are processed in the outer
        ;; environment (in case any declaration handlers side-effected
        ;; the environment.)
        
        (let* ((*nx-lexical-environment* old-env))
          (dolist (pair pairs)
            (let* ((sym (nx-need-var (nx-pair-name pair)))
                   (var (nx-cons-var sym))
                   (val (nx1-typed-var-initform pending sym (nx-pair-initform pair)))
                   (binding (nx1-note-var-binding var val)))
              (vars var)
              (vals val)
              (when binding (varbindings binding)))))
        (let* ((*nx-bound-vars* *nx-bound-vars*)
               (varbindings (varbindings)))
          (dolist (v (vars)) (nx-init-var pending v))
          (let* ((form 
                  (make-acode 
                   (%nx1-operator let)
                   (vars)
                   (vals)
                   (progn
                     (nx-effect-other-decls pending *nx-lexical-environment*)
                     (nx1-env-body context body old-env))
                 *nx-new-p2decls*)))
          (nx1-check-var-bindings varbindings)
          (nx1-punt-bindings (vars) (vals))
          form))))))



;((lambda (lambda-list) . body) . args)
(defun nx1-lambda-bind (context lambda-list args body &optional (body-environment *nx-lexical-environment*))
  (let* ((old-env body-environment)
         (arg-env *nx-lexical-environment*)
         (arglist nil)
         var-bound-vars
         vars vals vars* vals*)
    ;; If the lambda list contains &LEXPR, we can't do it.  Yet.
    (multiple-value-bind (ok req opttail resttail keytail) (verify-lambda-list lambda-list)
      (declare (ignore req opttail))
      (when (and ok (or (eq (%car resttail) '&lexpr)
                        ;*backend-use-linear-scan*
                        (eq (%car keytail) '&key)))
        (return-from nx1-lambda-bind (nx1-call context (nx1-form context `(lambda ,lambda-list ,@body)) args))))
    (let* ((*nx-lexical-environment* body-environment)
           (*nx-bound-vars* *nx-bound-vars*))
      (with-nx-declarations (pending)
        (multiple-value-bind (body decls) (parse-body body *nx-lexical-environment*)
          (nx-process-declarations pending decls)
          (multiple-value-bind (req opt rest keys auxen)
              (nx-parse-simple-lambda-list pending lambda-list)
            (let* ((*nx-lexical-environment* arg-env))
              (setq arglist (nx1-formlist context args)))
            (nx-effect-other-decls pending *nx-lexical-environment*)
            (while req
              (when (null arglist)
                (nx-error "Not enough args ~S for (LAMBDA ~s ...)" args lambda-list))
              (let* ((var (pop req))
                     (val (pop arglist))
                     (binding (nx1-note-var-binding var val)))
                (push var vars)
                (push val vals)
                (when binding (push binding var-bound-vars))))
            (setq body (nx1-env-body context body old-env))
            (nx1-check-var-bindings var-bound-vars)
            (nx1-punt-bindings vars vals)
            (destructuring-bind (&optional optvars inits spvars) opt
              (while optvars
                (if arglist
                  (progn
                    (push (%car optvars) vars) (push (%car arglist) vals)
                    (when (%car spvars) (push (%car spvars) vars) (push (make-nx-t) vals)))
                  (progn
                    (push (%car optvars) vars*) (push (%car inits) vals*)
                    (when (%car spvars) (push (%car spvars) vars*) (push (make-nx-nil) vals*))))
                (setq optvars (%cdr optvars) spvars (%cdr spvars) inits (%cdr inits)
                      arglist (%cdr arglist))))
            (if arglist
              (when (and (not keys) (not rest))
                (nx-error "Extra args ~s for (LAMBDA ~s ...)" args lambda-list))
              (when rest
                (push rest vars*) (push (make-nx-nil) vals*)
                (nx1-punt-bindings (cons rest nil) (cons (make-nx-nil) nil))
                (setq rest nil)))
            (destructuring-bind (&optional auxvars auxvals) auxen
              (let ((vars!% (nreconc vars* auxvars))
                    (vals!& (nreconc vals* auxvals)))
                (make-acode (%nx1-operator lambda-bind)
                            (append (nreverse vals) arglist)
                            (nreverse vars)
                            rest
                            nil
                            (list vars!% vals!&)
                            body
                            *nx-new-p2decls*)))))))))

(defun nx-inhibit-register-allocation (&optional (why 0))
  (let ((afunc *nx-current-function*))
    (setf (afunc-bits afunc)
          (%ilogior (%ilsl $fbitnoregs 1)
                    why
                    (afunc-bits afunc)))))



(defnx1 nx1-lap-function (ppc-lap-function) context (name bindings &body body)
  (declare (ftype (function (t t t)) %define-ppc-lap-function))
  (require "PPC-LAP" "ccl:compiler;ppc;ppc-lap")
  (setf (afunc-lfun *nx-current-function*) 
        (%define-ppc-lap-function name `((let ,bindings ,@body))
                                  (dpb (length bindings) $lfbits-numreq 0))))

(defnx1 nx1-x86-lap-function (x86-lap-function) context (name bindings &body body)
  (declare (ftype (function (t t t)) %define-x86-lap-function))
  (require "X86-LAP")
  (setf (afunc-lfun *nx-current-function*) 
        (%define-x86-lap-function name `((let ,bindings ,@body))
				    (dpb (length bindings) $lfbits-numreq 0))))

(defnx1 nx1-arm-lap-function (arm-lap-function) context (name bindings &body body)
  (declare (ftype (function (t t t)) %define-arm-lap-function))
  (require "ARM-LAP")
  (setf (afunc-lfun *nx-current-function*)
        (%define-arm-lap-function name `((let ,bindings ,@body))
				    (dpb (length bindings) $lfbits-numreq 0))))

                    



(defun nx1-env-body (context body old-env &optional (typecheck (nx-declarations-typecheck *nx-lexical-environment*)))
  (do* ((form (nx1-progn-body context body))
        (typechecks nil)
        (env *nx-lexical-environment* (lexenv.parent-env env)))
       ((or (eq env old-env) (null env))
        (if typechecks
          (make-acode
           (%nx1-operator progn)
           (nconc (nreverse typechecks) (list form)))
          form))
    (let ((vars (lexenv.variables env)))
      (when (consp vars)
        (dolist (var vars)
          (nx-check-var-usage var)
          (when (and typecheck
                     (let ((expansion (var-expansion var)))
                       (or (atom expansion) (neq (%car expansion) :symbol-macro))))
            (let* ((sym (var-name var))
                   (type (nx-declared-type sym)))
              (unless (eq type t)
                (let ((old-bits (nx-var-bits var)))
                  (push (nx1-form :value `(the ,type ,sym)) typechecks)
                  (when (%izerop (logior
                                  (%ilogand2 old-bits
                                             (%ilogior (%ilsl $vbitspecial 1)
                                                       (%ilsl $vbitreffed 1)
                                                       (%ilsl $vbitclosed 1)))
                                  (nx-var-root-nrefs var)
                                  (nx-var-root-nsetqs var)))
                    (nx-set-var-bits var (%ilogand2 (nx-var-bits var)
                                                    (%ilognot (%ilsl $vbitignore 1))))))))))))))


(defnx1 nx1-let* (let*) context (varspecs &body forms)
  (let* ((vars nil)
         (vals nil)
         (val nil)
         (var-bound-vars nil)
         (*nx-bound-vars* *nx-bound-vars*)
         (old-env *nx-lexical-environment*))
    (with-nx-declarations (pending)
      (multiple-value-bind (body decls)
                           (parse-body forms *nx-lexical-environment* nil)
        (nx-process-declarations pending decls)
        (dolist (pair varspecs)          
          (let* ((sym (nx-need-var (nx-pair-name pair)))
                 (var (progn 
                        (push (setq val (nx1-typed-var-initform pending sym (nx-pair-initform pair))) vals)
                        (nx-new-var pending sym)))
                 (binding (nx1-note-var-binding var val)))
            (when binding (push binding var-bound-vars))
            (push var vars)))
        (nx-effect-other-decls pending *nx-lexical-environment*)
        (let* ((result
                (make-acode 
                 (%nx1-default-operator)
                 (setq vars (nreverse vars))
                 (setq vals (nreverse vals))
                 (nx1-env-body context body old-env)
                 *nx-new-p2decls*)))
          (nx1-check-var-bindings var-bound-vars)
          (nx1-punt-bindings vars vals)
          result)))))

(defnx1 nx1-multiple-value-bind multiple-value-bind context 
        (varspecs bindform &body forms)
  (if (= (length varspecs) 1)
    (nx1-form context `(let* ((,(car varspecs) ,bindform)) ,@forms))
    (let* ((vars nil)
           (*nx-bound-vars* *nx-bound-vars*)
           (old-env *nx-lexical-environment*)
           (mvform (nx1-form :value bindform)))
      (with-nx-declarations (pending)
        (multiple-value-bind (body decls)
                             (parse-body forms *nx-lexical-environment* nil)
          (nx-process-declarations pending decls)
          (dolist (sym varspecs)
            (push (nx-new-var pending sym t) vars))
          (nx-effect-other-decls pending *nx-lexical-environment*)
          (make-acode
           (%nx1-operator multiple-value-bind)
           (nreverse vars)
           mvform
           (nx1-env-body context body old-env)
           *nx-new-p2decls*))))))


;;; This isn't intended to be user-visible; there isn't a whole lot of 
;;; sanity-checking applied to the subtag.
(defnx1 nx1-%alloc-misc ((%alloc-misc)) context (element-count subtag &optional (init nil init-p))
  (if init-p                            ; ensure that "init" is evaluated before miscobj is created.
    (make-acode (%nx1-operator %make-uvector)
                (nx1-form :value element-count)
                (nx1-form :value subtag)
                (nx1-form :value init))
    (make-acode (%nx1-operator %make-uvector)
                (nx1-form :value element-count)
                (nx1-form :value subtag))))

(defnx1 nx1-%lisp-word-ref (%lisp-word-ref) context (base offset)
  (make-acode (%nx1-operator %lisp-word-ref)
              (nx1-form :value base)
              (nx1-form :value offset)))

(defnx1 nx1-%single-to-double ((%single-to-double)) context (arg)
  (make-acode (%nx1-operator %single-to-double)
              (nx1-form :value arg)))

(defnx1 nx1-%double-to-single ((%double-to-single)) context (arg)
  (make-acode (%nx1-operator %double-to-single)
              (nx1-form :value arg)))

(defnx1 nx1-%fixnum-to-double ((%fixnum-to-double)) context (arg)
  (make-acode (%nx1-operator %fixnum-to-double)
              (nx1-form :value arg)))

(defnx1 nx1-%fixnum-to-single ((%fixnum-to-single)) context (arg)
  (make-acode (%nx1-operator %fixnum-to-single)
              (nx1-form :value arg)))

(defnx1 nx1-%double-float ((%double-float)) context (&whole whole arg &optional (result nil result-p))
  (declare (ignore result))
  (if result-p
    (nx1-treat-as-call context whole)
    (make-acode (%nx1-operator %double-float) (nx1-form :value arg))))

(defnx1 nx1-%short-float ((%short-float)) context (&whole whole arg &optional (result nil result-p))
  (declare (ignore result))        
  (if result-p
    (nx1-treat-as-call context whole)
    (make-acode (%nx1-operator %single-float) (nx1-form :value arg))))


(defnx1 nx1-symvector ((%symptr->symvector) (%symvector->symptr)) context (arg)
  (make-acode (%nx1-default-operator) (nx1-form :value arg)))

(defnx1 nx1-%ilognot (%ilognot) context (n)
  ;; Bootstrapping nonsense.
  (if (aref (backend-p2-dispatch *target-backend*)
            (logand operator-id-mask (%nx1-operator %ilognot)))
    (make-acode (%nx1-operator typed-form)
                'fixnum
                (make-acode (%nx1-operator %ilognot)
                            (nx1-form :value n)))
    (nx1-form context (macroexpand `(%ilognot ,n)))))

    
(defnx1 nx1-ash ((ash)) context (num amt)
  (make-acode
   (%nx1-operator ash)
   (nx1-form :value num)
   (nx1-form :value amt)))

(defnx1 nx1-%make-complex-float ((%make-complex-single-float)
                                 (%make-complex-double-float)
                                 ) context (r i)
  (make-acode
   (%nx1-default-operator)
   (nx1-form :value r)
   (nx1-form :value i)))

(defnx1 nx1-complex ((complex)) context (r &optional (i 0))
  (make-acode
   (%nx1-operator complex)
   (nx1-form :value r)
   (nx1-form :value i)))
    
    
   
        
(defun nx-badformat (&rest args)
 (nx-error "Bad argument format in ~S ." args))

(defnx1 nx1-eval-when eval-when context (when &body body)
  (nx1-progn-body context (if (or (memq 'eval when) (memq :execute when)) body)))

(defnx1 nx1-ivector-typecode-p ((ivector-typecode-p)) context  (arg)
  (make-acode (%nx1-operator ivector-typecode-p) (nx1-form :value arg)))

(defnx1 nx1-ivector-typecode-p ((gvector-typecode-p)) context  (arg)
  (make-acode (%nx1-operator gvector-typecode-p) (nx1-form :value arg)))
    
(defnx1 nx1-misplaced (declare) context (&whole w &rest args)
  (declare (ignore args))
  (nx-error "The DECLARE expression ~s is being treated as a form,
possibly because it's the result of macroexpansion. DECLARE expressions
can only appear in specified contexts and must be actual subexpressions
of the containing forms." w))

