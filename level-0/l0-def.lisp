;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

;;; primitives that manipulate function & variable definitions.





(defun functionp (arg)
  "Return true if OBJECT is a FUNCTION, and NIL otherwise."
  (functionp arg))

(defun lfunp (arg)
  (functionp arg))

(defun %proclaim-special (sym &optional initp)
  (let* ((oldbits (%symbol-bits sym)))
    (declare (fixnum oldbits))
    (%symbol-bits sym (bitset $sym_vbit_special oldbits))
    initp))

(setq *lfun-names* (make-hash-table :test 'eq :weak t))

(defun lookup-lfun-name (lfun) 
  (gethash lfun *lfun-names*))


(defun function-name (fun)
  (or (and (functionp fun) (lfun-name fun))
      (if (compiled-function-p (setq fun (closure-function fun)))
        (lfun-name fun))))


(defun bootstrapping-fmakunbound (name)
  (when (consp name)
    (unless (eq (%car name) 'setf)
      (error "Function spec handler not loaded yet"))
    (setq name (setf-function-name (cadr name))))
  (%unfhave name)
  name)

;;; redefined in sysutils.
(%fhave 'fmakunbound #'bootstrapping-fmakunbound)

(defun bootstrapping-fset (name fn)
  (fmakunbound name)
  (%fhave name fn)
  fn)

;Redefined in sysutils.
(%fhave 'fset #'bootstrapping-fset)

(defun fset-symbol (name fn)
  (fset (require-type name 'symbol) fn))


(defun bootstrapping-record-source-file (fn &optional type)
  (declare (ignore fn type))
  nil)

;Redefined in l1-utils.
(%fhave 'record-source-file #'bootstrapping-record-source-file)


(setq *fasload-print* nil)
(setq *save-doc-strings* t)



(%fhave '%defun-encapsulated-maybe ;Redefined in encapsulate
        (qlfun bootstrapping-defun-encapsulated (name fn)
          (declare (ignore name fn))
          nil))

(%fhave 'encapsulated-function-name  ;Redefined in encapsulate - used in l1-io
        (qlfun bootstrapping-encapsulated-function-name (fn)
          (declare (ignore fn))
          nil))

(%fhave 'set-function-info (qlfun set-function-info  (name info)
                                  (if (typep info 'string)
                                    (set-documentation name 'function info))
                                  name))

(defun %defun (named-fn &optional info)
  (unless (typep named-fn 'function)
    (dbg named-fn))
  (let* ((name (function-name named-fn)))
    (unless (and name
                 (or (symbolp name)
                     (setf-function-name-p name)))
      (dbg named-fn))
  (record-source-file name 'function)
  (when (not (%defun-encapsulated-maybe name named-fn))
    (when (and (symbolp name) (macro-function name nil))
      (warn "The macro ~s is being redefined as a function." name)
      (fmakunbound name))
    (fset name named-fn))
  (set-function-info name info)
  (when *fasload-print* (format t "~&~S~%" name))
  name))

(defun validate-function-name (name)
  (if (symbolp name)
    name
    (if (setf-function-name-p name)
      (setf-function-name (cadr name))
      (report-bad-arg name 'function-name))))

;;;    There are three kinds of things which can go in the function
;;;    cell of a symbol: 1) A function.  2) The thing which is the
;;;    value of %unbound-function%: a 1-element vector whose 0th
;;;    element is a code vector which causes an "undefined function"
;;;    error to be signalled.  3) A macro or special-form definition,
;;;    which is a 2-element vector whose 0th element is a code vector
;;;    which signals a "can't apply macro or special form" error when
;;;    executed and whose 1st element is a macro or special-operator
;;;    name.  It doesn't matter what type of gvector cases 2 and 3
;;;    are.  Once that's decided, it wouldn't hurt if %FHAVE
;;;    typechecked its second arg.

(defun %fhave (name def)
  (let* ((fname (validate-function-name name)))
    (setf (%svref (symptr->symvector (%symbol->symptr fname)) target::symbol.fcell-cell) def)))

;;; FBOUNDP is true of any symbol whose function-cell contains something other
;;; than %unbound-function%; we expect FBOUNDP to return that something.
(defun fboundp (name)
  "Return true if name has a global function definition."
  (let* ((fname (validate-function-name name))
         (def (%svref (symptr->symvector (%symbol->symptr fname)) target::symbol.fcell-cell)))
    (unless (eq def %unbound-function%)
      def)))

;;; %UNFHAVE doesn't seem to want to deal with SETF names or function specs.
;;; Who does ?

(defun %unfhave (sym)
  (let* ((symvec (symptr->symvector (%symbol->symptr sym)))
         (old (%svref symvec target::symbol.fcell-cell))
         (unbound %unbound-function%))
    (setf (%svref symvec target::symbol.fcell-cell) unbound)
    (not (eq old unbound))))

;;; It's guaranteed that lfun-bits is a fixnum.  Might be a 30-bit fixnum ...





(defun lfun-vector-name (fun &optional (new-name nil set-name-p))
  (let* ((bits (lfun-bits fun)))
    (declare (fixnum bits))
    (if (and (logbitp $lfbits-gfn-bit bits)
	     (not (logbitp $lfbits-method-bit bits)))
      (progn
        (if set-name-p
          (%gf-name fun new-name)
          (%gf-name fun)))
      (let* ((has-name-cell (not (logbitp $lfbits-noname-bit bits))))
	(if has-name-cell
	  (let* ((lfv (lfun-vector fun))
                 (name-idx (- (the fixnum (uvsize lfv)) 2))
		 (old-name (%svref lfv name-idx)))
	    (declare (fixnum name-idx))
	    (if (and set-name-p (not (eq old-name new-name)))
	      (setf (%svref lfv name-idx) new-name))
	    old-name))))))

(defun lfun-name (fun &optional (new-name nil set-name-p))
  (multiple-value-bind (stored-name stored?) (lookup-lfun-name fun)
    (unless stored?
      (setq stored-name (lfun-vector-name fun)))
    (when (and set-name-p (neq new-name stored-name))
      (if (and stored? (eq new-name (lfun-vector-name fun)))
        (remhash fun *lfun-names*)
        (if (logbitp $lfbits-noname-bit (the fixnum (lfun-bits fun)))   ; no name-cell in function vector.
          (puthash fun *lfun-names* new-name)
          (lfun-vector-name fun new-name))))
    stored-name))

(defun lfun-bits (function &optional new)
  (unless (functionp function)
    (setq function (require-type function 'function)))
  (let* ((lfv (lfun-vector function))
         (idx (1- (the fixnum (uvsize lfv))))
         (old (%svref lfv idx)))
    (declare (fixnum idx))
    (if new
      (setf (%svref lfv idx) new))
    old))
    
(defun %macro-have (symbol macro-function)
  (declare (special %macro-code%))      ; magically set by xloader.
  (%fhave symbol (vector %macro-code% macro-function)))


(defun special-operator-p (symbol)
  "If the symbol globally names a special form, return T, otherwise NIL."
  (let ((def (fboundp symbol)))
    (and (typep def 'simple-vector)
         (not (lfunp (svref def 1))))))

(defun special-form-p (x) (special-operator-p x))

(defun setf-function-name-p (thing)
  (and (consp thing)
       (consp (%cdr thing))
       (null (%cddr thing))
       (eq (%car thing) 'setf)
       (symbolp (%cadr thing))))

(defun macro-function (form &optional env)
  "If SYMBOL names a macro in ENV, returns the expansion function,
   else returns NIL. If ENV is unspecified or NIL, use the global
   environment only."
  (setq form (require-type form 'symbol))
  (when env
    ; A definition-environment isn't a lexical environment, but it can
    ; be an ancestor of one.
    (unless (istruct-typep env 'lexical-environment)
        (report-bad-arg env 'lexical-environment))
      (let ((cell nil))
        (tagbody
          top
          (if (setq cell (%cdr (assq form (lexenv.functions env))))
            (return-from macro-function 
              (if (eq (car cell) 'macro) (%cdr cell))))
          (unless (listp (setq env (lexenv.parent-env env)))
            (go top)))))
      ; Not found in env, look in function cell.
  (%global-macro-function form))

(defun %fixnum-ref-macptr (fixnum &optional (offset 0))
  (%int-to-ptr (%fixnum-ref-natural fixnum offset)))

(defun %fixnum-set-macptr (fixnum offset &optional (newval offset newval-p))
  (%fixnum-set-natural fixnum (if newval-p offset 0) (%ptr-to-int newval))
  newval)

;;; end of l0-def.lisp
