;;;-*-Mode: LISP; Package: CCL -*-
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

(defvar *loading-removes-encapsulation* nil
  "If true, loading a new method definition from a file will remove any tracing and advice on the method")

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

;;;
;;;  We support encapsulating three types of objects, i.e. modifying their definition
;;;  without changing their identity:
;;;    1. symbol - via the symbol-function slot
;;;    2. method - via the %method-function slot
;;;    3. standard-generic-function - via the %gf-dcode slot
;;;
;;; Encapsulation is effected by creating a new compiled function and storing it in the
;;; slot above. The new function references a gensym fbound to the original definition
;;; (except in the case of a gf, the gensym is fbound to a copy of the gf which in
;;; turn contains the original dcode, since we can't invoke the dcode directly).
;;; In addition, an ENCAPSULATION struct describing the encapsulation is created and
;;; stored in the *encapsulation-table* with the new compiled function as the key.
;;;
;;; 

(defparameter *encapsulation-table*
  (make-hash-table :test #'eq :rehash-size 2 :size 2 :weak t))

(defstruct (encapsulation)
  symbol         ; the uninterned name containing original def
  type           ; trace or advise
  spec           ; the original function spec
  advice-name    ; optional
  advice-when    ; :before, :after, :around 
  owner          ; where encapsulation is installed (can change)
)

(defun encapsulation-old-def (cap)
  (fboundp (encapsulation-symbol cap)))

(defun setf-function-spec-name (spec)
  (if (setf-function-name-p spec)
    (let ((name (%setf-method (cadr spec))))
      (if (non-nil-symbol-p name)  ; this can be an anonymous function
        name
        (setf-function-name (cadr spec))))
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
  (when (%advised-p name)
    (format t "~%... Unadvising ~a" name) 
    (%unadvise-1 name))
  nil)

(defun function-encapsulated-p (fn-or-method)
  (get-encapsulation fn-or-method))

(defun %encap-fboundp (thing)
  (etypecase thing
    (symbol (fboundp thing))
    (method (%method-function thing))))
  
(defun %encap-binding (thing)
  (require-type (etypecase thing
                  (symbol (fboundp thing))
                  (method (%method-function thing)))
                'function))

(defun get-encapsulation (spec)
  (let* ((key (typecase spec
                (symbol (let* ((def (fboundp spec)))
                          (if (generic-function-p def)
                            (%gf-dcode def)
                            def)))
                (method (%method-function spec))
                (standard-generic-function (%gf-dcode spec))
                (function spec)))
         (cap (gethash key *encapsulation-table*)))
    #+gz (assert (or (null cap)
                     (let ((fn (%encap-binding (encapsulation-owner cap))))
                       (eq (if (standard-generic-function-p fn) (%gf-dcode fn) fn) key))))
    cap))

(defun set-encapsulation-owner (fn owner)
  (let ((cap (get-encapsulation fn)))
    (when cap
      (setf (encapsulation-owner cap) owner))))

(defun put-encapsulation (fn cap)
  (let* ((owner (encapsulation-owner cap))
         (old-def (%encap-binding owner))
         (newsym (encapsulation-symbol cap)))
    (setf (gethash fn *encapsulation-table*) cap)
    (set-encapsulation-owner old-def newsym)
    (etypecase owner
      (symbol
       (cond ((standard-generic-function-p old-def)
              (%fhave newsym (%copy-function old-def))
              (setf (%gf-dcode old-def) fn))
             (t
              (%fhave newsym old-def)
              (%fhave owner fn))))
      (method
       (%fhave newsym old-def)
       (setf (%method-function owner) fn)
       (remove-obsoleted-combined-methods owner)))))

(defun remove-encapsulation (cap)
  (let* ((owner (encapsulation-owner cap))
         (cur-def (%encap-fboundp owner))
         (old-def (encapsulation-old-def cap)))
    (typecase owner
      (symbol
       (cond ((or (null cur-def)
                  (not (eq cap (get-encapsulation cur-def))))
              ;; rebound behind our back, oh well.
              nil)
             ((standard-generic-function-p cur-def)
              (remhash (%gf-dcode cur-def) *encapsulation-table*)
              (set-encapsulation-owner old-def owner)
              (setf (%gf-dcode cur-def) (%gf-dcode old-def)))
             (t
              (remhash cur-def *encapsulation-table*)
              (set-encapsulation-owner old-def owner)
              (%fhave owner old-def))))
      (method
       (remhash cur-def *encapsulation-table*)
       (set-encapsulation-owner old-def owner)
       (setf (%method-function owner) old-def)
       (remove-obsoleted-combined-methods owner)))))


(defun encapsulate (owner newdef type spec newsym &optional advice-name advice-when)
  (let ((cap (make-encapsulation
	      :owner owner
	      :symbol newsym
	      :type type
	      :spec spec
	      :advice-name advice-name
	      :advice-when advice-when)))
    (put-encapsulation newdef cap)
    cap))

(defun find-unencapsulated-definition (fn)
  (when fn
    (loop for cap = (get-encapsulation fn) while cap
      do (setq fn (encapsulation-old-def cap)))
    fn))

(defun set-unencapsulated-definition (cap newdef)
  (loop for owner = (encapsulation-symbol cap)
    do (setq cap (get-encapsulation owner)) while cap
    finally (%fhave owner newdef)))

(defun %encapsulation-thing (spec &optional define-if-not (error-p t))
  ;; Returns either an fboundp symbol or a method, or nil.
  (typecase spec
    (symbol
     ;; weed out macros and special-forms
     (if (or (null spec) (special-operator-p spec) (macro-function spec))
       (if error-p
         (error "Cannot trace or advise ~a~S"
                (cond ((null spec) "")
                      ((special-operator-p spec) "special operator ")
                      (t "macro "))
                spec)
         nil)
       (if (or (fboundp spec)
               (and define-if-not
                    (progn
                      (warn "~S was undefined" spec)
                      (%fhave spec (%function 'trace-null-def))
                      t)))
         spec
         (if error-p
           (error "~S is undefined." spec)
           nil))))
    (method spec)
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
                   (return method))
                  (define-if-not
                    (when (define-undefined-method spec gf qualifiers specializers)
                      (go AGN)))
                  (t (if error-p
                       (error "Method ~s qualifiers ~s specializers ~s not found."
                              gf qualifiers specializers)
                       (return nil)))))))
       (setf
        (let ((name-or-fn (setf-function-spec-name spec)))
          (cond ((symbolp name-or-fn) (%encapsulation-thing name-or-fn))
                ((functionp name-or-fn) ; it's anonymous - give it a name
                 (let ((newname (gensym)))
                   (%fhave newname name-or-fn)
                   (store-setf-method (cadr spec) newname)
                   newname)))))))
    (t (if error-p
         (error "Invalid trace spec ~s" spec)
         nil))))

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
    (let ((fn (%encap-binding name)))
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
                                     (%encapsulation-thing spec nil nil)
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
        ;; now look for traceable methods.
        ;; It's possible, but not likely, that we will be both
        ;; a callback and a function or method, if so we trace both.
        ;; This isn't possible.
        ;; If we're neither, signal an error.
        ;;
        (let* ((trace-thing (%encapsulation-thing spec define-if-not)) def)
          (%untrace-1 trace-thing)
          (setq def (%encap-binding trace-thing))
          (when (and methods (typep def 'standard-generic-function))
            (dolist (m (%gf-methods def))
              (apply #'trace-function m args)))
          #+old
          (when step               ; just check if has interpreted def
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
            (encapsulate trace-thing newdef 'trace spec newsym))))
    (when *trace-hook*
      (apply *trace-hook* spec args))))


(defun %traced-p (thing)
  (let ((cap (get-encapsulation thing)))
    (and cap (eq (encapsulation-type cap) 'trace))))

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

(defun %untrace-all ()
  (dolist (pfun *trace-pfun-list*)
    (%untrace pfun)
    (when *untrace-hook*
      (funcall *untrace-hook* pfun)))
  (loop for cap being the hash-value of *encapsulation-table*
    when (eq (encapsulation-type cap) 'trace)
    collect (let ((spec (encapsulation-spec cap)))
              (remove-encapsulation cap)
              (when *untrace-hook*
                (funcall *untrace-hook* spec))
              spec)))

(defun %untrace (sym &aux val)
  (when (and (consp sym)(consp (car sym)))
    (setq sym (car sym)))
  (cond
   ((and (typep sym 'symbol)
         (boundp sym)
         (macptrp (symbol-value sym)))
    (%untrace-pfun sym))
   (t 
    (let* ((trace-thing (%encapsulation-thing sym))
           (def (%encap-binding trace-thing)))
      (when (typep def 'standard-generic-function)
        (let ((methods (%gf-methods def)))
          (dolist (m methods)
            (let ((cap (get-encapsulation m)))
              (when (and cap (eq (encapsulation-advice-when cap) :step-gf))
                (remove-encapsulation cap)
                (push m val))))))
      ; gf could have first been traced :step, and then just plain traced
      ; maybe the latter trace should undo the stepping??
      (let ((spec (%untrace-1 trace-thing)))
        (when spec
          (push spec val))))))
  (when *untrace-hook*
    (funcall *untrace-hook* sym))
  (if (null (cdr val)) (car val) val))

;; thing is a symbol or method - def is current definition
;; we already know its traced
(defun %untrace-1 (thing)
  (let ((cap (get-encapsulation thing)))
    (when (and cap (eq (encapsulation-type cap) 'trace))
      (remove-encapsulation cap)
      (encapsulation-spec cap))))

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
    (loop for x being the hash-value of *encapsulation-table*
	 when (eq (encapsulation-type x) 'trace)
	 do (push (encapsulation-spec x) res))
    (dolist (x *trace-pfun-list*)
      (push x res))
    res))

(defmacro with-traces (syms &body body)
 `(unwind-protect
       (progn
         (let ((*trace-output* (make-broadcast-stream)))
           ;; if you're tracing ccl internals you'll get trace output as it encapsulates the
           ;; functions so hide all the trace output while eval'ing the trace form itself.
           (trace ,@syms))
         ,@body)
    (untrace ,@syms)))

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
       (declare (ftype function ,def))
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
     `(traced ,sym)
     :keep-symbols t)))

; &method var tells compiler to bind var to contents of next-method-context
(defun advise-global-def (def when stuff &optional method-p dynamic-extent-arglist)
  (let* ((saved-method-var (gensym)))
    `(lambda (,@(if (and method-p (neq when :after))
                  `(&method ,saved-method-var))
              &rest arglist)
       ,@(and dynamic-extent-arglist '((declare (dynamic-extent arglist))))
       (declare (ftype function ,def))
       (declare (ignorable arglist))
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


(defun compile-named-function-warn (fn name &rest keys)
  (declare (dynamic-extent keys))
  (multiple-value-bind (result warnings) (apply #'compile-named-function fn :name name keys)
    (when warnings 
      (let ((first t))
        (dolist (w warnings)
          (signal-compiler-warning w first nil nil nil)
          (setq first nil))))
    result))

       
(defun %advised-p (thing)
  (loop for nx = thing then (encapsulation-symbol cap)
    as cap = (get-encapsulation nx) while cap
    thereis (eq (encapsulation-type cap) 'advice)))

(defun %advice-encapsulations (thing when advice-name)
  (loop for nx = thing then (encapsulation-symbol cap)
    as cap = (get-encapsulation nx) while cap
    when (and (eq (encapsulation-type cap) 'advice)
              (or (null when) (eq when (encapsulation-advice-when cap)))
              (or (null advice-name) (equal advice-name (encapsulation-advice-name cap))))
    collect cap))

(defun advise-2 (newdef newsym method-p function-spec when advice-name define-if-not)      
  (let* ((advise-thing (%encapsulation-thing function-spec define-if-not))
         orig-sym)
    (let ((capsules (%advice-encapsulations advise-thing when advice-name)))
      (when capsules 
        (unadvise-capsules capsules)))
    (when (%traced-p advise-thing)
      ; make traced call advised
      (setq orig-sym
            (encapsulation-symbol (get-encapsulation advise-thing))))
    (lfun-name newdef `(advised ',function-spec))
    (if method-p (copy-method-function-bits (%encap-binding advise-thing) newdef))
    (encapsulate (or orig-sym advise-thing) newdef 'advice function-spec newsym advice-name when)
    newdef))

(defmacro advise (function form &key (when :before) name define-if-not dynamic-extent-arglist)
  (let* ((newsym (gensym "ADVICE"))
         ; WAS typep advise-thing 'method
         (method-p (or (typep function 'method) ; can this happen?
                       (and (consp function)(eq (car function) :method))))
         (newdef (advise-global-def newsym when form method-p dynamic-extent-arglist)))
      `(advise-2 ,newdef ',newsym ,method-p ',function ',when ',name
                 ,define-if-not)))

(defmacro advisedp (function-spec &key when name)
  `(advisedp-1 ',function-spec ',when ',name))

(defun encapsulation-advice-spec (cap)
  (list (encapsulation-spec cap)
        (encapsulation-advice-when cap)
        (encapsulation-advice-name cap)))
  
(defun advisedp-1 (function-spec when name)
  (cond ((eq t function-spec)
         (loop for c being the hash-value of *encapsulation-table*
           when (and (eq (encapsulation-type c) 'advice)
                     (or (null when)(eq when (encapsulation-advice-when c)))
                     (or (null name)(equal name (encapsulation-advice-name c))))
           collect (encapsulation-advice-spec c)))
        (t (let* ((advise-thing (%encapsulation-thing function-spec))
                  (capsules (%advice-encapsulations advise-thing when name)))
             (mapcar #'encapsulation-advice-spec capsules)))))

(defun %unadvise-1 (function-spec &optional when advice-name ignore)
  (declare (ignore ignore))
  (let ((advise-thing (%encapsulation-thing function-spec)))
    (let ((capsules (%advice-encapsulations advise-thing when advice-name)))
      (when capsules (unadvise-capsules capsules)))))

(defun unadvise-capsules (capsules)
  (let (val)
    (dolist (capsule capsules)
        (push (encapsulation-advice-spec capsule) val)
        (remove-encapsulation capsule))
    val))

(defmacro unadvise (function &key when name)
  (cond ((neq function t)
         `(%unadvise-1 ',function ',when ',name))
        (t `(%unadvise-all ',when ',name))))

(defun %unadvise-all (&optional when name)
  (loop for cap being the hash-value of *encapsulation-table*
    when (and (eq (encapsulation-type cap) 'advice)
              (or (null when)(eq when (encapsulation-advice-when cap)))
              (or (null name)(equal name (encapsulation-advice-name cap))))
    collect (progn
              (remove-encapsulation cap)
              (encapsulation-advice-spec cap))))

;; Called from %defun. Return t if we defined it, nil otherwise
(defun %defun-encapsulated-maybe (name newdef)
  (assert (not (get-encapsulation newdef)))
  (let ((old-def (fboundp name)) cap)
    (when (and old-def (setq cap (get-encapsulation name)))
      (cond ((or (and *loading-files* *loading-removes-encapsulation*)
                 ;; redefining a gf as a fn.
                 (typep old-def 'standard-generic-function))
             (forget-encapsulations name)
             nil)
            (t (set-unencapsulated-definition cap newdef)
               T)))))

;; Called from clos when change dcode
(defun %set-encapsulated-gf-dcode (gf new-dcode)
  (loop with cap = (get-encapsulation gf)
    for gf-copy = (encapsulation-old-def cap)
    as cur-dcode = (%gf-dcode gf-copy)
    do (setq cap (get-encapsulation cur-dcode))
    ;; refresh all the gf copies, in case other info in gf changed
    do (%copy-function gf gf-copy)
    do (setf (%gf-dcode gf-copy) (if cap cur-dcode new-dcode))
    while cap))

;; Called from clos when oldmethod is being replaced by newmethod in a gf.
(defun %move-method-encapsulations-maybe (oldmethod newmethod &aux cap)
  (unless (eq oldmethod newmethod)
    (cond ((and *loading-removes-encapsulation* *loading-files*)
           (when (%traced-p oldmethod)
             (warn "~%... Untracing ~s" (%untrace-1 oldmethod)))
           (when (%advised-p oldmethod)
             (format t "~%... Unadvising ~s" (%unadvise-1 oldmethod))))
          (t (when (setq cap (get-encapsulation oldmethod))
               (let* ((old-inner-def (find-unencapsulated-definition oldmethod))
                      (newdef (%method-function newmethod))
                      (olddef (%method-function oldmethod)))
                 ;; make last encapsulation call new definition
                 (set-unencapsulated-definition cap newdef)
                 (setf (%method-function newmethod) olddef)
                 (set-encapsulation-owner olddef newmethod)
                 (setf (%method-function oldmethod) old-inner-def)
                 (loop
                   for def = olddef then (encapsulation-old-def cap)
                   for cap = (get-encapsulation def) while cap
                   do (copy-method-function-bits newdef def))))))))

#|
        Change History (most recent last):
        2       12/29/94        akh     merge with d13
|# ;(do not edit past this line!!)
