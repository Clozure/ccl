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

;; Lets try encapsulations
;; trace is here too
;; Make trace like 1.3, trace methods, trace (setf car)


(defvar *trace-alist* nil)
(defvar *trace-pfun-list* nil)
(defvar *trace-enable* t)
(defvar *trace-level* 0)
(defparameter *trace-max-indent* 40)
(defvar *trace-print-level* nil)
(defvar *trace-print-length* nil)
;(defparameter *trace-define-if-undefined* nil)
(defparameter *trace-bar-frequency* nil)
(defvar *trace-hook* nil)
(defvar *untrace-hook* nil)
(defvar *trace-print-hook* nil)


(defvar *advise-alist* nil)

(defparameter *encapsulation-table*
  (make-hash-table :test #'eq :rehash-size 2 :size 2))

(defstruct (encapsulation)
  symbol         ; the uninterned name containing original def
  type           ; trace or advise
  spec           ; the original function spec
  advice-name    ; optional
  advice-when    ; :before, :after, :around 
  owner          ; where encapsulation is installed
)

(defun setf-function-spec-name (spec)
  (if (and (consp spec) (eq (car spec) 'setf))
    (or (%setf-method (cadr spec)) ; this can be an anonymous function
        (setf-function-name (cadr spec)))
    spec))


(defun trace-tab (direction &aux (n (min *trace-level* *trace-max-indent*)))
  (fresh-line *trace-output*)
  (dotimes (i (1- n))
    (declare (fixnum i))
    (write-char (if (and *trace-bar-frequency* 
			 (eq 0 (mod i *trace-bar-frequency*)))
		  #\| #\Space) *trace-output*))
  (if (eq direction :in)
    (format *trace-output* "~d> " (1- *trace-level*))
    (format *trace-output* "<~d " (1- *trace-level*))))

(defun trace-before  (&rest args)
  (declare (dynamic-extent args))
  (trace-tab :in)
  (let* ((*print-level* *trace-print-level*)
         (*print-length* *trace-print-length*)
         (*print-readably* nil))
    (format *trace-output* "Calling ~S ~%" args)
    (force-output *trace-output*)))

(defun trace-after (sym &rest args &aux (n (length args)))
  (declare (dynamic-extent args))
  (let* ((*print-level* *trace-print-level*)
         (*print-length* *trace-print-length*)
         (*print-readably* nil))
    (if (eq n 1)
      (progn
        (trace-tab :out)
        (format *trace-output* "~S returned ~S~%" sym (%car args)))
      (progn
        (trace-tab :out)
        (format *trace-output* "~S returned ~S values :" sym n)
        (dolist (val args)
          (trace-tab :out)
          (format *trace-output* "     ~S" val))))
    (force-output *trace-output*)))

(defun forget-encapsulations (name)
  (when (%traced-p name)
    (format t "~%... Untracing ~a" name) 
    (%untrace-1 name))
  (when (%advised-p name nil nil t)
    (format t "~%... Unadvising ~a" name) 
    (unadvise-1 name))
  nil)

(defun function-encapsulated-p (fn-or-method)
  (typecase fn-or-method
    ((or method symbol cons)(function-encapsulation fn-or-method))
    (function
     (or (function-traced-p fn-or-method)
         (function-advised-p fn-or-method )))))

(defun function-traced-p (fn)
  (%function-in-alist fn *trace-alist*))

(defun function-advised-p (fn)
  (%function-in-alist fn *advise-alist*))                           

(defun %function-in-alist (def list)
  (dolist (cap list)
    (let ((symbol (encapsulation-owner cap)))
      (typecase symbol
        (symbol (when (eq (fboundp symbol) def)
                  (return cap)))
        (method (when (eq (%method-function symbol) def)
                  (return cap)))
        (standard-generic-function
         (when (eq symbol def) (return cap)))))))

(defun function-encapsulation (spec)
  (typecase spec
    ((or symbol method)
     (gethash spec *encapsulation-table*))
    (function (function-encapsulated-p spec))
    (cons (gethash (setf-function-spec-name spec) *encapsulation-table*))))
;; i.e. old 68K clos - vs 68K target with new clos




; she works now - does the equivalent of the original gf - called from traced def
(defun %%call-encapsulated-gf (thing args)
  ; (print 'one)(print thing)(print args)
  ; thing is gf . %%1st-arg-dcode
  ; args is ok
  (let* ((dcode (cdr thing))
         (proto (assq dcode dcode-proto-alist))  ; <<
         (dt (%gf-dispatch-table (car thing))))
    (if proto ; assume all of these special dudes want args individually 
      (if (listp args)
        (apply dcode dt args)
        (%apply-lexpr dcode dt args))
      (funcall dcode dt args))))
    


                     ; (apply encapsulation args)


;; the dcode function of the original gf has been bashed with a combined method whose
;; dcode function is this. So the combined method is called with 2 args (dispatch-table
;; and args to the gf). The combined method in turn makes a lexpr of those 2 args.

(defun %%call-gf-encapsulation (thing args)
  ; (print 'two)(print thing)(print (if (listp args) args (collect-lexpr-args args 0)))
  ; thing traced-blitz  gf-blitz . %%1st-arg-dcode  
  ; args = dispatch-table . original-args
  ;  dont need dispatch-table - its just there as a side effect
  (if (listp args)  ; this probably never happens
    (let ((orig-args (cadr args)))
      (if (listp orig-args)
        (apply (car thing) orig-args)
        (%apply-lexpr (car thing) orig-args)))
    (let* ((orig-args (%lexpr-ref args (%lexpr-count args) 1)))
      (if (listp orig-args)
        (apply (car thing) orig-args)
        ; knee deep in lexprs
        (%apply-lexpr (car thing) orig-args)))))
    

(defun encapsulate (fn-spec old-def type trace-spec newsym
                            &optional advice-name advice-when)
  (let ((capsule (function-encapsulation fn-spec))
        gf-dcode old-encapsulation)
    (%fhave newsym
            (if (standard-generic-function-p old-def)
              (let ((dcode (%gf-dcode old-def)))
                (setq gf-dcode
                      (if (and (combined-method-p dcode)
                               (eq '%%call-gf-encapsulation
                                   (function-name (%combined-method-dcode dcode))))
                        (let ((stuff (%combined-method-methods dcode)))
                          (setq old-encapsulation (car stuff))
                          (cdr stuff))
                        (cons old-def dcode)))
                (replace-function-code old-def *gf-proto*)  ; <<  gotta remember to fix it
                (or old-encapsulation
                    (%cons-combined-method old-def gf-dcode #'%%call-encapsulated-gf)))
              old-def))                 ; make new symbol call old definition
    ;; move the encapsulation from fn-spec to sym    
    (cond (capsule (put-encapsulation newsym capsule)))    
    (put-encapsulation fn-spec
                       (make-encapsulation
                        :symbol newsym
                        :type type
                        :spec trace-spec
                        :advice-name advice-name
                        :advice-when advice-when))
    (values newsym gf-dcode)))
 

;; call with cap nil to remove - for symbol anyway
;; maybe advising methods is silly - just define a before method

(defun put-encapsulation (spec cap)
  (when cap
    (setf (encapsulation-owner cap) spec)
    (record-encapsulation cap)
    )
  (let ((key (typecase spec
               ((or symbol method standard-generic-function) spec)
               (cons (setf-function-spec-name spec))
               (t (report-bad-arg spec '(or symbol method cons))))))
    (if cap
      (setf (gethash key *encapsulation-table*) cap)
      (remhash key *encapsulation-table*)))
  cap)

(defun remove-encapsulation (capsule &optional dont-replace)
  ; optional don't replace is for unadvising, tracing all on a method
  (let (spec nextsym newdef def)
    (setq spec (encapsulation-owner capsule))
    (setq def (typecase spec
                (symbol (fboundp spec))
                (method spec)))
    (setq nextsym (encapsulation-symbol capsule))
    (setq newdef (fboundp nextsym))
    (without-interrupts
     (if (standard-generic-function-p def)
       (if (and (combined-method-p newdef)
                (eq '%%call-encapsulated-gf (function-name (%combined-method-dcode newdef))))
         (let* ((orig-decode (require-type (cdr (%combined-method-methods newdef)) 'function))
                (proto (cdr (assq orig-decode dcode-proto-alist)))
                ) ; <<
           (setf (%gf-dcode def) orig-decode)
           (replace-function-code def (or proto *gf-proto*)))
         (setf (car (%combined-method-methods (%gf-dcode def))) newdef))
       (typecase spec
         (symbol (%fhave spec newdef))
         (method (setf (%method-function spec) newdef)
                 (remove-obsoleted-combined-methods spec)
                 newdef)))
     (put-encapsulation spec
                        (if (null dont-replace)
                          (function-encapsulation nextsym)))
     (put-encapsulation nextsym nil)
     (unrecord-encapsulation capsule)
     )))


(defun record-encapsulation (capsule)
  (ecase (encapsulation-type capsule)
    (trace
     (when (not (memq capsule *trace-alist*))
       (push capsule *trace-alist*)))
    (advice
     (when (not (memq capsule *advise-alist*))
       (push capsule *advise-alist*)))))

(defun unrecord-encapsulation (capsule)
  (ecase (encapsulation-type capsule)
    (trace
      (setq *trace-alist* (delq capsule *trace-alist*)))
    (advice
     (setq *advise-alist* (delq capsule *advise-alist*)))))


(defun find-unencapsulated-definition (spec)
  ;; spec is a symbol, function, or method object
  ;; returns a raw function ?? 
  (let (foo)
    (while (setq foo (function-encapsulation spec))
      (setq spec (encapsulation-symbol foo)))
    (values
    (typecase spec
      (symbol (fboundp spec))
      (method (%method-function spec))
      (t spec))
    spec)))

(defun %trace-fboundp (spec)
  (typecase spec
    (symbol (fboundp spec))
    (method (%method-function spec))))


(defun %trace-function-spec-p (spec &optional define-if-not undefined-ok (error-p t))
  ;; weed out macros and special-forms
  (typecase spec
    (symbol
     (if (or (null spec)(special-operator-p spec)(macro-function spec))
       (if error-p
	 (error "Cannot trace or advise ~S" spec)
	 (values nil nil))
       (let ((res (or (fboundp spec)(and define-if-not
					 (progn (warn "~S was undefined" spec)
						(%fhave spec (%function 'trace-null-def)))))))
	 (if res
	   (values res spec)
	   (if undefined-ok
	     (values nil spec)
	     (if error-p
	       (error "~S is undefined." spec)
	       (values nil nil)))))))
    (method
     (values (%method-function spec) spec))
    (cons
     (case (car spec)
       (:method 
        (let ((gf (cadr spec))
              (qualifiers (butlast (cddr spec)))
              (specializers (car (last (cddr spec))))
              method)
          (setq specializers (require-type specializers 'list))
          (prog ()
            AGN
            (cond ((setq method
                         (find-method-by-names gf qualifiers specializers))
                   (return (values (%method-function method) method)))
                  (define-if-not
                    (when (define-undefined-method spec gf qualifiers specializers)
                      (go AGN)))
                  (t (if error-p
		       (error "Method ~s qualifiers ~s specializers ~s not found."
			      gf qualifiers specializers)
		       (return (values nil nil))))))))
       (setf
        (let ((name-or-fn (setf-function-spec-name spec)))
          (cond ((symbolp name-or-fn)(%trace-function-spec-p name-or-fn))
                ((functionp name-or-fn) ; it's anonymous - give it a name
                 (let ((newname (gensym)))
                   (%fhave newname name-or-fn)
                   (store-setf-method (cadr spec) newname)
                   (values name-or-fn newname))))))))
    (t (if error-p
	 (error "Invalid trace spec ~s" spec)
	 (values nil nil)))))
    

(defun trace-null-def (&rest ignore)
  (declare (ignore ignore)))

(defun define-undefined-method (spec gf qualifiers specializers)
  (let (vars def)    
    (flet ((blob (e)
                 (let ((v (gensym)))
                   (push v vars)
                   (list v e))))
      (declare (dynamic-extent #'blob))
      (setq def
            (let ((lambda-list (mapcar #' blob specializers)))
              (eval
               `(defmethod ,gf ,@qualifiers (,@lambda-list &rest ignore)
                  (declare (ignore ignore ,@vars))))))
      (when def (warn "~S was undefined" spec))
      def)))

(defun traceable-symbol-p (sym)
  (and sym
       (not (special-operator-p sym))
       (not (macro-function sym))
       (fboundp sym)))

(defun %trace-package (pkg &rest args)
  (declare (dynamic-extent args))
  (do-present-symbols (sym pkg)
    ;; Don't auto-trace imported symbols, because too often these are imported
    ;; system functions...
    (when (eq (symbol-package sym) pkg)
      (when (traceable-symbol-p sym)
	(apply #'trace-function sym args))
      (when (or (%setf-method sym)
		;; Not really right.  Should construct the name if doesn't exist.
		;; But that would create a lot of garbage for little gain...
		(let ((name (existing-setf-function-name sym)))
                  (traceable-symbol-p name)))
	(apply #'trace-function `(setf ,sym) args)))))

(defun trace-print-body (print-form)
  (when print-form
    (if (and (consp print-form) (eq (car print-form) 'values))
      `((mapcar #'(lambda (name object)
		    (trace-tab :in)
		    (format *trace-output* "~s = ~s" name object))
	 ',(cdr print-form)
	 (list ,@(cdr print-form))))
      `((let ((objects (multiple-value-list ,print-form))
	      (i -1))
	  (if (and objects (not (cdr objects)))
	    (progn
	      (trace-tab :in)
	      (format *trace-output* "~s = ~s" ',print-form (car objects)))
	    (dolist (object objects)
	      (trace-tab :in)
	      (format *trace-output* "~s [~d] = ~s" ',print-form (incf i) object))))))))

(defun trace-backtrace-body (test-form)
  (when test-form
    `((let ((test ,test-form))
	(when test
	  (multiple-value-bind (detailed-p count)
	      (cond ((memq test '(:detailed :verbose :full))
		     (values t nil))
		    ((integerp test)
		     (values nil test))
		    ((and (consp test)
			  (keywordp (car test))
			  (consp (cdr test))
			  (null (cddr test)))
		     (values (memq (car test) '(:detailed :verbose :full))
			     (and (integerp (cadr test)) (cadr test))))
		    (t (values nil nil)))
	    (let ((*debug-io* *trace-output*))
	      (print-call-history :detailed-p detailed-p
				  :count (or count most-positive-fixnum))
	      (terpri *trace-output*))))))))

(defun trace-inside-frame-p (name)
  (if (packagep name)
    (map-call-frames #'(lambda (p)
                         (let* ((fn (cfp-lfun p))
                                (fname (and fn (function-name fn)))
                                (sym (typecase fname
                                       (method (method-name fname))
                                       (cons (and (setf-function-name-p fname) (cadr fname)))
                                       (symbol fname)
                                       (t nil))))
                           (when (and sym (eq (symbol-package sym) name))
                             (return-from trace-inside-frame-p t)))))
    (let ((fn (typecase name
                (symbol (fboundp name))
                (method (%method-function name)))))
      (when fn
        (map-call-frames #'(lambda (p)
                             (when (eq (cfp-lfun p) fn)
                               (return-from trace-inside-frame-p t))))))))

(defun trace-package-spec (spec)
  (when (or (stringp spec)
            (packagep spec)
            (and (consp spec) (eq (car spec) :package)))
    (let ((pkg (if (consp spec)
                 (destructuring-bind (pkg) (cdr spec) pkg)
                 spec)))
      (pkg-arg pkg))))

(defun trace-function (spec &rest args &key before after methods
                            (if t) (before-if t) (after-if t)
                            print print-before print-after
                            eval eval-before eval-after
                            break break-before break-after
                            backtrace backtrace-before backtrace-after
                            inside
                            define-if-not
                            ;; Some synonyms, just to be nice
                            (condition t) (if-before t) (if-after t) (wherein nil))

  (declare (dynamic-extent args))
  (let ((pkg (trace-package-spec spec)))
    (when pkg
      (return-from trace-function (apply #'%trace-package pkg args))))

  ;; A little bit of dwim, after all this _is_ an interactive tool...
  (unless (eq condition t)
    (setq if (if (eq if t) condition `(and ,if ,condition))))
  (unless (eq if-before t)
    (setq before-if (if (eq before-if t) if-before `(and ,before-if ,if-before))))
  (unless (eq if-after t)
    (setq after-if (if (eq after-if t) if-after `(and ,after-if ,if-after))))
  (when (and inside (trace-spec-p inside))
    (setq inside (list inside)))
  (when wherein
    (setq inside (append inside (if (trace-spec-p wherein) (list wherein) wherein))))
  (case break
    (:before (setq break-before (or break-before t) break nil))
    (:after (setq break-after (or break-after t) break nil)))
  (case backtrace
    (:before (setq backtrace-before (or backtrace-before t) backtrace nil))
    (:after (setq backtrace-after (or backtrace-after t) backtrace nil)))
  (case before
    (:break (setq before :print break-before t))
    (:backtrace (setq before :print backtrace-before t)))
  (case after
    (:break (setq after :print break-after t))
    (:backtrace (setq after :print backtrace-after t)))

  (when break
    (setq break-before (if break-before
			 `(and ,break ,break-before)
			 break))
    (setq break-after (if break-after
			`(and ,break ,break-after)
			break)))
  (unless backtrace-before
    (setq backtrace-before backtrace))
  (when (and (consp backtrace-before) (keywordp (car backtrace-before)))
    (setq backtrace-before `',backtrace-before))
  (when (and (consp backtrace-after) (keywordp (car backtrace-after)))
    (setq backtrace-after `',backtrace-after))

  (when (and (null before) (null after))
    (setq before :print)
    (setq after :print))
  (when (and (null before) backtrace-before)
    (setq before :print))

  (case before
    ((:print :default) (setq before #'trace-before)))
  (case after
    ((:print :default) (setq after #'trace-after)))

  (when (or (non-nil-symbol-p before) (functionp before))
    (setq before `',before))
  (when (or (non-nil-symbol-p after) (functionp after))
    (setq after `',after))

  (when inside
    (let ((tests (loop for spec in inside
		       as name = (or (trace-package-spec spec)
                                     (nth-value 1 (%trace-function-spec-p spec nil nil nil))
				     (error "Cannot trace inside ~s" spec))
		       collect `(trace-inside-frame-p ',name))))
      (setq if `(and ,if (or ,@tests)))))

  (setq eval-before `(,@(trace-print-body print-before)
		      ,@(trace-print-body print)
		      ,@(and eval-before `(,eval-before))
		      ,@(and eval `(,eval))
		      ,@(and before `((apply ,before ',spec args)))
		      ,@(trace-backtrace-body backtrace-before)
		      ,@(and break-before `((when ,break-before
					      (force-output *trace-output*)
					      (break "~s trace entry: ~s" ',spec args))))))
  (setq eval-after `(,@(trace-backtrace-body backtrace-after)
		     ,@(and after `((apply ,after ',spec vals)))
		     ,@(and eval `(,eval))
		     ,@(and eval-after `(,eval-after))
		     ,@(trace-print-body print)
		     ,@(trace-print-body print-after)
		     ,@(and break-after `((when ,break-after
					    (force-output *trace-output*)
					    (break "~s trace exit: ~s" ',spec vals))))))

  (prog1
      (block %trace-block
	;;
	;; see if we're a callback
	;;
	(when (and (typep spec 'symbol)
		   (boundp spec)
		   (macptrp (symbol-value spec)))
	  (let ((len (length %pascal-functions%))
		(sym-name (symbol-name spec)))
	    (declare (fixnum len))
	    (dotimes (i len)
	      (let ((pfe (%svref %pascal-functions% i)))
		(when (and (vectorp pfe)
			   (string= sym-name (symbol-name (pfe.sym pfe))))
		  (when backtrace
		    (if (null before)
		      (setq before :print)))
		  (setf (pfe.trace-p pfe)
			`(,@(if before `((:before . ,before)))
			  ,@(if after `((:after . ,after)))
			  ,@(if backtrace `((:backtrace . ,backtrace)))))
		  (push spec *trace-pfun-list*)))))
	  (return-from %trace-block))
	;;
	;; now look for tracible methods.
	;; It's possible, but not likely, that we will be both
	;; a callback and a function or method, if so we trace both.
	;; This isn't possible.
	;; If we're neither, signal an error.
	;;
	(multiple-value-bind (def trace-thing) 
	    (%trace-function-spec-p spec define-if-not)
	  (when (null def)
	    (return-from trace-function
	      (warn "Trace does not understand ~S, ignored." spec)))
	  (when (%traced-p trace-thing)
	    (%untrace-1 trace-thing)
	    (setq def (%trace-fboundp trace-thing)))
	  (when (and methods (typep def 'standard-generic-function))
	    (dolist (m (%gf-methods def))
	      (apply #'trace-function m args)))
	  #+old
	  (when step		   ; just check if has interpreted def
	    (if (typep def 'standard-generic-function)
	      (let ((methods (%gf-methods def)))
					; should we complain if no methods? naah
		(dolist (m methods) ; stick :step-gf in advice-when slot
		  (%trace m :step t)
		  (let ((e (function-encapsulation m)))
		    (when e (setf (encapsulation-advice-when e) :step-gf))))
					; we choose to believe that before and after are intended for the gf
		(if  (or before after)
		  (setq step nil)                
		  (return-from %trace-block)))
	      #|(uncompile-for-stepping trace-thing nil t)|#))
	  (let* ((newsym (gensym "TRACE"))
		 (method-p (typep trace-thing 'method))
		 (newdef (trace-global-def 
			  spec newsym if before-if eval-before after-if eval-after method-p)))
	    (when method-p
	      (copy-method-function-bits def newdef))
	    (without-interrupts
	      (multiple-value-bind (ignore gf-dcode) (encapsulate trace-thing def 'trace spec newsym)
		(declare (ignore ignore))
		(cond (gf-dcode 
		       (setf (%gf-dcode def)
			     (%cons-combined-method def (cons newdef gf-dcode) #'%%call-gf-encapsulation)))
		      ((symbolp trace-thing) (%fhave trace-thing newdef))
		      ((typep trace-thing 'method)
		       (setf (%method-function trace-thing) newdef)
		       (remove-obsoleted-combined-methods trace-thing)
		       newdef)))))))
    (when *trace-hook*
      (apply *trace-hook* spec args))))


;; sym is either a symbol or a method

(defun %traced-p (sym)
  (let ((foo (function-encapsulation sym)))
    (and foo (eq (encapsulation-type foo) 'trace))))

(defmacro untrace (&rest syms)
  "Remove tracing from the specified functions. With no args, untrace all
   functions."
  (if syms
    `(%untrace-0 ',syms)
    `(%untrace-all)))

(defun %untrace-0 (syms)
  (let (val x)
    (dolist (symbol syms)
      (setq x (%untrace symbol))
      (when x (push x val)))
    val))


(defun %untrace (sym)
  (when (and (consp sym)(consp (car sym)))
    (setq sym (car sym)))
  (cond
    ((and (typep sym 'symbol)
        (boundp sym)
        (macptrp (symbol-value sym)))
     (%untrace-pfun sym))
    (t 
     (multiple-value-bind (def trace-thing) (%trace-function-spec-p sym)
       (let (val)
	 (when (typep def 'standard-generic-function)
	   (let ((methods (%gf-methods def)))
	     (dolist (m methods)
	       (let ((e (function-encapsulation m)))
		 (when (and e (eq (encapsulation-advice-when e) :step-gf))
		   (remove-encapsulation e)
		   (push m  val))))))
					; gf could have first been traced :step, and then just plain traced
					; maybe the latter trace should undo the stepping??
	 (when (%traced-p trace-thing)
	   (%untrace-1 trace-thing)
	   (push trace-thing val))
	 (if (null (cdr val))(car val) val)))))
  (when *untrace-hook*
    (funcall *untrace-hook* sym)))

(defun %untrace-all ()
  (let ((val nil))
    (dolist (cap *trace-alist*)
      (push (encapsulation-spec cap) val)
       (remove-encapsulation cap)
       (when *untrace-hook*
       (funcall *untrace-hook* (encapsulation-spec cap))))
     (dolist (pfun *trace-pfun-list*)
       (%untrace pfun)
       (when *untrace-hook*
       (funcall *untrace-hook* pfun)))
    val))

;; thing is a symbol or method - def is current definition
;; we already know its traced
(defun %untrace-1 (thing)
  (let (capsule)
    (setq capsule (function-encapsulation thing))
    ;; trace encapsulations must be first      
    (when (neq (encapsulation-type capsule) 'trace)
      (error "~S was not traced." thing))
    (remove-encapsulation capsule)
    (encapsulation-spec capsule)))

(defun %untrace-pfun (sym)
  (let ((len (length %pascal-functions%))
	(sym-name (symbol-name sym)))
    (declare (fixnum len))
    (dotimes (i len)
      (let ((pfe (%svref %pascal-functions% i)))
	(when (and (vectorp pfe)
		   (string= sym-name (symbol-name (pfe.sym pfe))))
	  (setf (pfe.trace-p pfe) nil
		*trace-pfun-list* (remove sym *trace-pfun-list*))
	  (return-from %untrace-pfun sym))))
    nil))



(defmacro trace (&rest syms)
  "TRACE {Option Global-Value}* { Name | (Name {Option Value}*) }*

TRACE is a debugging tool that provides information when specified
functions are called."
  (if syms
    (let ((options (loop while (keywordp (car syms))
		     nconc (list (pop syms) (pop syms)))))
      `(%trace-0 ',syms ',options))
    `(%trace-list)))

(defun trace-spec-p (arg)
  (or (atom arg)
      (memq (car arg) '(:method setf :package))))


(defun %trace-0 (syms &optional global-options)
  (dolist (spec syms)
    (if (trace-spec-p spec)
      (apply #'trace-function spec global-options)
      (apply #'trace-function (append spec global-options)))))

(defun %trace-list ()
  (let (res)
    (dolist (x *trace-alist*)
      (push (encapsulation-spec x) res))
    (dolist (x *trace-pfun-list*)
      (push x res))
    res))

;; this week def is the name of an uninterned gensym whose fn-cell is original def

(defun trace-global-def (sym def if before-if eval-before after-if eval-after &optional method-p)
  (let ((saved-method-var (gensym))
	(enable (gensym))
	do-it)
    (setq do-it
          (cond #+old (step
		       (setq step-it            
			     `(step-apply-simple ',def args))
		       (if (eq step t)
			 step-it
			 `(if (apply ',step ',sym args) ; gaak
			   ,step-it
			   ,(if (and before method-p)
				`(apply-with-method-context ,saved-method-var (symbol-function ',def) args)
				`(apply ',def args)))))
                (t (if (and eval-before method-p)
                     `(apply-with-method-context ,saved-method-var (symbol-function ',def) args)
                     `(apply ',def args)))))
    (compile-named-function-warn
     `(lambda (,@(and eval-before method-p `(&method ,saved-method-var))
	       &rest args) ; if methodp put &method on front of args - vs get-saved-method-var?
       (declare (dynamic-extent args))
       (let ((*trace-level* (1+ *trace-level*))
	     (,enable ,if))
	 (declare (special *trace-enable* *trace-level*))
	 ,(when eval-before
	   `(when (and ,enable ,before-if *trace-enable*)
	     (when *trace-print-hook*
	       (funcall *trace-print-hook* ',sym t))
	     (let* ((*trace-enable* nil))
	       ,@eval-before)
	     (when *trace-print-hook*
	       (funcall *trace-print-hook* ',sym nil))))
	 ,(if eval-after
	   `(let ((vals (multiple-value-list ,do-it)))
	     (when (and ,enable ,after-if *trace-enable*)
	       (when *trace-print-hook* 
		 (funcall *trace-print-hook* ',sym t))
	       (let* ((*trace-enable* nil))
		 ,@eval-after)
	       (when *trace-print-hook* 
		 (funcall *trace-print-hook* ',sym nil)))
	     (values-list vals))
	   do-it)))
     `(traced ,sym))))

; &method var tells compiler to bind var to contents of next-method-context
(defun advise-global-def (function-spec def when stuff &optional method-p)
  (declare (ignore function-spec))
  (let* ((saved-method-var (gensym)))
    `(lambda (,@(if (and method-p (neq when :after))
                  `(&method ,saved-method-var))
              &rest arglist)
      ;(declare (dynamic-extent arglist))
       (let ()
         ,(ecase
            when
            (:before
             `(block nil
                ,stuff                  
                (return ,(if method-p
                           `(apply-with-method-context ,saved-method-var (symbol-function ',def) arglist)
                           `(apply ',def arglist)))))
            (:after         
             `(block nil
                (let ((values (multiple-value-list (apply (function ,def) arglist))))
                  ;(declare (dynamic-extent values))
                  ,stuff
                  (return (values-list values)))))
            (:around
             ;; stuff is e.g. (+ 5 (:do-it))
             (if method-p 
               `(macrolet ((:do-it ()
                             `(apply-with-method-context ,',saved-method-var 
                                                         (symbol-function ',',def)
                                                         arglist)))
                  (block nil
                    (return  ,stuff)))
               `(macrolet ((:do-it ()
                             `(apply (function ,',def) arglist)))
                  (block nil
                    (return  ,stuff))))))))))


(defun compile-named-function-warn (fn name)
  (multiple-value-bind (result warnings)(compile-named-function fn name)    
    (when warnings 
      (let ((first t))
        (dolist (w warnings)
          (signal-compiler-warning w first nil nil nil)
          (setq first nil))))
    result))

;; want to look like
;; (setq values (multiple-value-list (progn ,@frob)))
     
       
(defun %advised-p (thing &optional when advice-name quick)
  ;; thing is a symbol, result is list of encapsulations
  ;; Quick when used as a simple predicate
  (let ((nx thing) cap val)
    (while (setq cap (function-encapsulation nx))
      (when (eq (encapsulation-type cap) 'advice)
        (if quick (return-from %advised-p cap))
        (when (or (and (null when)(null advice-name))
                  (and (eq when (encapsulation-advice-when cap))
                       (equal advice-name (encapsulation-advice-name cap))))
          (push cap val)))
      (setq nx (encapsulation-symbol cap)))
    val))  


(defun advise-2 (newdef newsym method-p function-spec when advice-name define-if-not)      
  (let (advise-thing def orig-sym orig-def)
    (multiple-value-setq (def advise-thing) 
      (%trace-function-spec-p function-spec define-if-not))
    (when (not def)(error "Advise does not understand ~s." function-spec))
    (when (%traced-p advise-thing)
      (setq orig-sym
            (encapsulation-symbol (function-encapsulation advise-thing)))
      (setq orig-def (fboundp orig-sym)))
    (let ((capsules (%advised-p advise-thing when advice-name)))
      (when capsules 
        (unadvise-capsules capsules)
        ; get the right def you fool!
        (setq def (%trace-function-spec-p function-spec))))
    (without-interrupts
     (multiple-value-bind (ignore gf-dcode)
                          (encapsulate (or orig-sym advise-thing) (or orig-def def) 
                                       'advice function-spec newsym
                                       advice-name when)
       (declare (ignore ignore))
       (lfun-name newdef `(advised ',function-spec))
       (if method-p (copy-method-function-bits def newdef))
       (if gf-dcode (setq newdef (%cons-combined-method def (cons newdef gf-dcode)
                                                        #'%%call-gf-encapsulation)))                     
       (cond (orig-sym
              (%fhave orig-sym newdef))  ; make traced call advised
             (t  (cond (gf-dcode (setf (%gf-dcode def) newdef))
                       ((symbolp advise-thing)
                        (%fhave advise-thing newdef))
                       ((typep advise-thing 'method)
                        (progn 
                          (setf (%method-function advise-thing) newdef)
                          (remove-obsoleted-combined-methods advise-thing)
                          newdef)))))))))

(defmacro advise (function form &key (when :before) name define-if-not)
  (let* ((newsym (gensym "ADVICE"))
         ; WAS typep advise-thing 'method
         (method-p (or (typep function 'method) ; can this happen?
                       (and (consp function)(eq (car function) :method))))
         (newdef (advise-global-def function newsym when form method-p)))
      `(advise-2 ,newdef ',newsym ,method-p ',function ',when ',name
                 ,define-if-not)))

(defmacro advisedp (function-spec &key when name)
  `(advisedp-1 ',function-spec ',when ',name))

(defun advisedp-1 (function-spec when name)
  (let (val)
    (flet ((xtract-capsule (c)
             (list (encapsulation-spec c)
                   (encapsulation-advice-when c)
                   (encapsulation-advice-name c))))
      (cond ((eq t function-spec)
             (dolist (c *advise-alist*)
               (when (and
                      (or (null when)(eq when (encapsulation-advice-when c)))
                      (or (null name)(equal name (encapsulation-advice-name c))))
                 (push (xtract-capsule c) val))))
            (t (let* ((advise-thing (nth-value 1  (%trace-function-spec-p function-spec)))
                      (capsules (%advised-p advise-thing when name)))
                 (dolist (capsule capsules)
                   (push (xtract-capsule capsule) val)))))
      val)))               


(defun unadvise-1 (function-spec &optional when advice-name ignore)
  (declare (ignore ignore))
  (let ((advise-thing (nth-value 1 (%trace-function-spec-p function-spec))))
    (let ((capsules (%advised-p advise-thing when advice-name)))
      (when capsules (unadvise-capsules capsules)))))

(defun unadvise-capsules (capsules)
  (let (val)
    (dolist (capsule capsules)
        (push (list (encapsulation-spec capsule)
                    (encapsulation-advice-when capsule)
                    (encapsulation-advice-name capsule))
              val)
        (remove-encapsulation capsule))
    val))

(defmacro unadvise (function &key when name)
  (cond ((neq function t)
         `(unadvise-1 ',function ',when ',name))
        (t '(%unadvise-all))))

(defun %unadvise-all ()
  (unadvise-capsules *advise-alist*))

(defun %set-unencapsulated-definition (spec newdef)
  (let (foo)
    (while (setq foo (function-encapsulation spec))
      (setq spec (encapsulation-symbol foo)))
    (typecase spec
      (symbol
       (%fhave spec newdef)) ;; or fset ??  
      (method
       (setf (%method-function spec) newdef)
       (remove-obsoleted-combined-methods spec)
       newdef))))


;; return t if we defined it, nil otherwise

(defun %defun-encapsulated-maybe (name newdef)
  (let ((def (fboundp name)))
    (when (and def (function-encapsulated-p name))
      (cond ((or *loading-files* (typep def 'standard-generic-function))
             (forget-encapsulations name)
             nil)
            (t (%set-unencapsulated-definition name newdef)
               T)))))

(defun %move-method-encapsulations-maybe (oldmethod newmethod)
  ;; deal with method redefinition
  (let (cap newdef olddef old-inner-def)
    (when (and (setq cap (function-encapsulation oldmethod))
               (neq oldmethod newmethod))      
      (cond (*loading-files*
             (when (%traced-p oldmethod)
               (warn "~%... Untracing ~s" (%untrace-1 oldmethod)))
             (when (%advised-p oldmethod nil nil t)
               (format t "~%... Unadvising ~s" (unadvise-1 oldmethod))))
            (t (setq newdef (%method-function newmethod))
               (setq olddef (%method-function oldmethod))
               (setq old-inner-def (find-unencapsulated-definition oldmethod))
               ;; make last encapsulation call new definition            
               (%set-unencapsulated-definition oldmethod newdef)
               (setf (%method-function newmethod) olddef)
               (remove-encapsulation cap t)
               (put-encapsulation newmethod cap)
               (setf (%method-function oldmethod) old-inner-def)
               (advise-set-method-bits newmethod newdef)
               )))))

(defun advise-set-method-bits (spec newdef)
  ;; spec is a symbol, function, or method object
  (let (foo)
    (while (setq foo (function-encapsulation spec))      
      (let ((def (typecase spec
                   (symbol (fboundp spec))
                   (method (%method-function spec))
                   (t nil))))
        (if def
          (copy-method-function-bits newdef def)
          (error "whats going on here anyway")))
      (setq spec (encapsulation-symbol foo)))))


#|
	Change History (most recent last):
	2	12/29/94	akh	merge with d13
|# ;(do not edit past this line!!)
