;;;-*- Mode: Lisp; Package: CCL -*-
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
  (%fhave symbol
          #-arm-target (vector %macro-code% macro-function)
          #+arm-target (%fix-fn-entrypoint (gvector :pseudofunction 0 %macro-code% macro-function))))


(defun special-operator-p (symbol)
  "If the symbol globally names a special form, return T, otherwise NIL."
  (let ((def (fboundp symbol)))
    (and #-arm-target (typep def 'simple-vector)
         #+arm-target (= (typecode def) arm::subtag-pseudofunction)
         (not (lfunp #-arm-target (svref def 1)
                     #+arm-target (uvref def 2))))))

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
    ;; A definition-environment isn't a lexical environment, but it can
    ;; be an ancestor of one.
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
  ;; Not found in env, look in function cell.
  (let* ((fbinding (fboundp form)))
    (if (and #-arm-target (typep fbinding 'simple-vector)
             #+arm-target (= (typecode fbinding) arm::subtag-pseudofunction)
             (= (the fixnum (uvsize fbinding)) #-arm-target 2 #+arm-target 3))
        (let* ((fun (%svref fbinding #-arm-target 1 #+arm-target 2)))
          (if (functionp fun) fun)))))

(defun %fixnum-ref-macptr (fixnum &optional (offset 0))
  (%int-to-ptr (%fixnum-ref-natural fixnum offset)))

(defun %fixnum-set-macptr (fixnum offset &optional (newval offset newval-p))
  (%fixnum-set-natural fixnum (if newval-p offset 0) (%ptr-to-int newval))
  newval)

(defun nth-catch-frame-tag (n)
  (declare (fixnum n))
  (let* ((frame (%catch-top (%current-tcr))))
    (dotimes (i n (%svref frame target::catch-frame.catch-tag-cell))
      (setq frame (%svref frame target::catch-frame.link-cell)))))

;;; This function is magic, and it can only be called from
;;; an unwind-protect cleanup form (making it even more magic.)
;;; If we can tell that we reached the unwind-protect via THROW,
;;; return a list of the target catch tag and all values being
;;; thrown.
#+x86-target
(defun %throwing-through-cleanup-p ()
  ;; when we enter and unwind-protect cleanup on x8664, the
  ;; top frame on the tstack contains state information that's
  ;; used both by THROW and by normal exit from the protected
  ;; form.  That state information contains a count of the number
  ;; of catch/unwind-protect frames still to be processed (non-zero
  ;; only in the case where we're actually throwing), the value(s)
  ;; being thrown, and a return address that isn't interesting to
  ;; us.  It's an historical accident that that information is stored
  ;; differently in the cases where a single value is being thrown
  ;; and multiple values are thrown.
  ;; A tstack frame is always doubleword aligned, and the first two
  ;; words are a backpointer to the previous tstack frame and a
  ;; pointer into the main lisp stack.  In the single value case,
  ;; we then have 3 words: return address, frame count, value;
  ;; in the multiple-value we have 3 fixed words (value count,
  ;; return address, frame count) with the values following the
  ;; frame count (value 0 follows immediately.)
  ;; A cleanup form is always called from either .SPnthrowvalues
  ;; of .SPnthrow1value, and those subprims can be called either
  ;; by .SPthrow (in which case the return address in the frame
  ;; will have no function associated with it) or by Lisp code
  ;; (in which case it will.)
  ;; We (have to) just assume that the frame on top of the temp
  ;; stack is context info for the nthrow stuff.  Tracing this
  ;; function may violate this assumption and cause misbehavior
  ;; here.
  (let* ((frame (%current-tsp))
         (single-value-case (not (typep (%lisp-word-ref frame 2) 'fixnum)))
         (frame-count (%lisp-word-ref frame (if single-value-case 3 4)))
         (throwing (null (%return-address-function (if single-value-case
                                                     (%lisp-word-ref frame 2)
                                                     (%lisp-word-ref frame 3))))))
    (declare (fixnum frame))
    (if throwing
      (collect ((info))
        (info (nth-catch-frame-tag frame-count))
        (if single-value-case
          (info (%lisp-word-ref frame 4))
          (let* ((valptr (+ frame 5)))
            (declare (fixnum valptr))
            (dotimes (i (%lisp-word-ref frame 2))
              (declare (fixnum i))
              (info (%lisp-word-ref valptr i)))))
        (info)))))

;;; end of l0-def.lisp
