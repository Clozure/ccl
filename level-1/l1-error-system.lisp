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


;;; This file contains the error/condition system.  Functions that
;;; signal/handle errors are defined later.

(in-package "CCL")

;;;***********************************
;;; Error System
;;;***********************************

(defclass condition () ())
(defclass warning (condition) ())
(defclass serious-condition (condition) ())
(defclass error (serious-condition) ())

(define-condition simple-condition (condition)
  ((format-control :initarg :format-control
                  :reader simple-condition-format-control)
   (format-arguments :initarg :format-arguments
                     :initform nil
                     :reader simple-condition-format-arguments))
  (:report (lambda (c stream)  ;; If this were a method, slot value might be faster someday.  Accessors always faster ?
                               ;; And of course it's terribly important that this be as fast as humanly possible...
	    ;Use accessors because they're documented and users can specialize them.
            (apply #'format stream (simple-condition-format-control c)
                   (simple-condition-format-arguments c)))))


(define-condition storage-condition (serious-condition) ())

(define-condition thread-condition (serious-condition) ())

(define-condition process-reset (thread-condition)
  ((kill :initarg :kill :initform nil :reader process-reset-kill)))


(define-condition print-not-readable (error)
  ((object :initarg :object :reader print-not-readable-object)
   (stream :initarg :stream :reader print-not-readable-stream))
  (:report (lambda (c stream)
             (let* ((*print-readably* nil))
               (format stream "Attempt to print object ~S on stream ~S ."
                       (print-not-readable-object c)
                       (print-not-readable-stream c))))))

(define-condition simple-warning (simple-condition warning) ())

(define-condition compiler-warning (warning)
  ((file-name :initarg :file-name :initform nil :accessor compiler-warning-file-name)
   (stream-position :initform nil :accessor compiler-warning-stream-position)
   (function-name :initarg :function-name :initform nil :accessor compiler-warning-function-name)
   (warning-type :initarg :warning-type :reader compiler-warning-warning-type)
   (args :initarg :args :reader compiler-warning-args)
   (nrefs :initform 1 :accessor compiler-warning-nrefs))
  (:report report-compiler-warning))

(define-condition style-warning (compiler-warning) ())
(define-condition undefined-function-reference (style-warning) ())
(define-condition macro-used-before-definition (compiler-warning) ())
(define-condition invalid-arguments (style-warning) ())
(define-condition invalid-arguments-global (style-warning) ())

(define-condition simple-error (simple-condition error) ())

(define-condition simple-storage-condition (simple-condition storage-condition) ())
(define-condition stack-overflow-condition (simple-storage-condition) ())

(define-condition invalid-memory-access (storage-condition)
  ((address :initarg :address)
   (write-p :initform nil :initarg :write-p))
  (:report (lambda (c s)
             (with-slots (address write-p) c
               (format s "Fault during ~a memory address #x~x" (if write-p "write to" "read of") address)))))
  
(define-condition type-error (error)
  ((datum :initarg :datum)
   (expected-type :initarg :expected-type :reader type-error-expected-type)
   (format-control :initarg :format-control  :initform (%rsc-string  $xwrongtype) :reader type-error-format-control))
  (:report (lambda (c s)
             (format s (type-error-format-control c)
                     (type-error-datum c) 
                     (type-error-expected-type c)))))

(define-condition bad-slot-type (type-error)
  ((slot-definition :initform nil :initarg :slot-definition)
   (instance :initform nil :initarg :instance))
  (:report (lambda (c s)
	     (format s "The value ~s can not be used to set the value of the slot ~s in ~s, because it is not of type ~s. "
		     (type-error-datum c)
		     (slot-definition-name (slot-value c 'slot-definition))
		     (slot-value c 'instance)
		     (type-error-expected-type c)))))

(define-condition bad-slot-type-from-initform (bad-slot-type)
  ()
  (:report (lambda (c s)
	     (let* ((slotd (slot-value c 'slot-definition)))
	       (format s "The value ~s, derived from the initform ~s, can not be used to set the value of the slot ~s in ~s, because it is not of type ~s. "
		     (type-error-datum c)
		     (slot-definition-initform slotd)
		     (slot-definition-name slotd)
		     (slot-value c 'instance)
		     (type-error-expected-type c))))))

(define-condition bad-slot-type-from-initarg (bad-slot-type)
  ((initarg-name :initarg :initarg-name))
  (:report (lambda (c s)
	     (let* ((slotd (slot-value c 'slot-definition)))
	       (format s "The value ~s, derived from the initarg ~s, can not be used to set the value of the slot ~s in ~s, because it is not of type ~s. "
		     (type-error-datum c)
		     (slot-value c 'initarg-name)
		     (slot-definition-name slotd)
		     (slot-value c 'instance)
		     (type-error-expected-type c))))))
  

(define-condition improper-list (type-error)
  ((expected-type :initform '(satisfies proper-list-p) :reader type-error-expected-type)))

(define-condition cant-construct-arglist (improper-list)
  ())


(let* ((magic-token '("Unbound")))
  (defmethod type-error-datum ((c type-error))
    (let* ((datum-slot (slot-value c 'datum)))
      (if (eq magic-token datum-slot)
        (%unbound-marker-8)
        datum-slot)))

; do we need this
  (defun signal-type-error (datum expected &optional (format-string (%rsc-string  $xwrongtype)))
    (let ((error #'error))
      (funcall error (make-condition 'type-error
                                     :format-control format-string
                                     :datum (if (eq datum (%unbound-marker-8)) magic-token datum)
                                     :expected-type (%type-error-type expected)))))
)


(define-condition sequence-index-type-error (type-error)
  ((sequence :initarg :sequence))
  (:report (lambda (c s)
             (format s "~s is not a valid sequence index for ~s"
                     (type-error-datum c)
                     (slot-value c 'sequence)))))


;;; This is admittedly sleazy; ANSI CL requires TYPE-ERRORs to be
;;; signalled in cases where a type-specifier is not of an appropriate
;;; subtype.  The sleazy part is whether it's right to overload TYPE-ERROR
;;; like this.

(define-condition invalid-subtype-error (type-error)
  ()
  (:report (lambda (c s)
             (format s "The type specifier ~S is not determinably a subtype of the type ~S"
                     (type-error-datum c)
                     (type-error-expected-type c)))))

(define-condition simple-type-error (simple-condition type-error) ())

(define-condition array-element-type-error (simple-type-error)
  ((array :initarg :array :reader array-element-type-error-array))
  (:report (lambda (c s)
             (format s (simple-condition-format-control c)
                     (type-error-datum c)
                     (array-element-type-error-array c)))))
                  




(define-condition program-error (error) ())
(define-condition simple-program-error (simple-condition program-error)
  ((context :initarg :context :reader simple-program-error-context :initform nil)))

(defun signal-program-error (string &rest args)
  (let* ((e #'error))
    (funcall e
	     (make-condition 'simple-program-error
			     :format-control (if (fixnump string) (%rsc-string string) string)
			     :format-arguments args))))

(define-condition simple-destructuring-error (simple-program-error) ())

(define-condition wrong-number-of-arguments (program-error)
  ((nargs :initform nil
	  :initarg :nargs :reader wrong-number-of-arguments-nargs)
   (fn :initform nil :initarg :fn :reader wrong-number-of-arguments-fn))
  (:report report-argument-mismatch))
       
(define-condition too-many-arguments (wrong-number-of-arguments) ())

(define-condition too-few-arguments (wrong-number-of-arguments) ())

(defun report-argument-mismatch (c s)
  (let* ((nargs-provided (wrong-number-of-arguments-nargs c))
	 (fn (wrong-number-of-arguments-fn c))
	 (too-many (typep c 'too-many-arguments)))
    (multiple-value-bind (min max scaled-nargs)
	(min-max-actual-args fn nargs-provided)
      (if (not min)
	(progn
	  (format s "Function ~s called with too ~a arguments. "
                  fn
                  (if too-many
                    "many"
                    "few")))
	(if too-many
	  (format s "Too many arguments in call to ~s:~&~d argument~:p provided, at most ~d accepted. " fn scaled-nargs max)
	  (format s "Too few arguments in call to ~s:~&~d argument~:p provided, at least ~d required. " fn  scaled-nargs min))))))



(define-condition compile-time-program-error (simple-program-error)
  nil ;((context :initarg :context :reader compile-time-program-error-context))
  (:report
   (lambda (c s)
     (format s "While compiling ~a :~%~a" 
             (simple-program-error-context c)
             (apply #'format nil (simple-condition-format-control c) (simple-condition-format-arguments c))))))



;;; Miscellaneous error during compilation (caused by macroexpansion, transforms, compile-time evaluation, etc.)
;;; NOT program-errors.
(define-condition compile-time-error (simple-error)
  ((context :initarg :context :reader compile-time-error-context))
  (:report
   (lambda (c s)
     (format s "While compiling ~a :~%~a" 
             (compile-time-error-context c)
             (format nil "~a" c)))))

(define-condition control-error (error) ())

(define-condition cant-throw-error (control-error)
  ((tag :initarg :tag))
  (:report (lambda (c s)
             (format s "Can't throw to tag ~s" (slot-value c 'tag)))))

(define-condition inactive-restart (control-error)
  ((restart-name :initarg :restart-name))
  (:report (lambda (c s)
	     (format s "Restart ~s is not active" (slot-value c 'restart-name)))))

(define-condition lock-protocol-error (control-error)
  ((lock :initarg :lock)))

(define-condition not-lock-owner (lock-protocol-error)
  ()
  (:report (lambda (c s)
	     (format s "Current process ~s does not own lock ~s"
		     *current-process* (slot-value c 'lock)))))

(define-condition not-locked (lock-protocol-error)
  ()
  (:report (lambda (c s)
	     (format s "Lock ~s isn't locked." (slot-value c 'lock)))))

(define-condition deadlock (lock-protocol-error)
  ()
  (:report (lambda (c s)
	     (format s "Requested operation on ~s would cause deadlock." (slot-value c 'lock)))))

(define-condition package-error (error)
  ((package :initarg :package :reader package-error-package)))
(define-condition no-such-package (package-error)
  ()
  (:report (lambda (c s) (format s (%rsc-string $xnopkg) (package-error-package c)))))
(define-condition unintern-conflict-error (package-error)
  ((sym-to-unintern :initarg :sym)
   (conflicting-syms :initarg :conflicts))
  (:report (lambda (c s)
             (format s (%rsc-string $xunintc) (slot-value c 'sym-to-unintern) (package-error-package c) (slot-value c 'conflicting-syms)))))

(define-condition import-conflict-error (package-error)
  ((imported-sym :initarg :imported-sym)
   (conflicting-sym :initarg :conflicting-sym)
   (conflict-external-p :initarg :conflict-external))
  (:report (lambda (c s)
             (format s (%rsc-string (if (slot-value c 'conflict-external-p) $ximprtcx $ximprtc))
                     (slot-value c 'imported-sym)
                     (package-error-package c)
                     (slot-value c 'conflicting-sym)))))

(define-condition use-package-conflict-error (package-error)
  ((package-to-use :initarg :package-to-use)
   (conflicts :initarg :conflicts)
   (external-p :initarg :external-p))
  (:report (lambda (c s)
             (format s (%rsc-string (if (slot-value c 'external-p) $xusecX $xusec))
                     (slot-value c 'package-to-use)
                     (package-error-package c)
                     (slot-value c 'conflicts)))))

(define-condition export-conflict-error (package-error)
  ((conflicts :initarg :conflicts))
  (:report 
   (lambda (c s)
     (format s "Name conflict~p detected by ~A :" (length (slot-value c 'conflicts)) 'export)
     (let* ((package (package-error-package c)))
       (dolist (conflict (slot-value c 'conflicts))
         (destructuring-bind (inherited-p sym-to-export using-package conflicting-sym) conflict
           (format s "~&~A'ing ~S from ~S would cause a name conflict with ~&~
                      the ~a symbol ~S in the package ~s, which uses ~S."
                   'export 
                   sym-to-export 
                   package 
                   (if inherited-p "inherited" "present")
                   conflicting-sym
                   using-package
                   package)))))))

(define-condition export-requires-import (package-error)
  ((to-be-imported :initarg :to-be-imported))
  (:report
   (lambda (c s)
     (let* ((p (package-error-package c)))
       (format s "The following symbols need to be imported to ~S before they can be exported ~& from that package:~%~s:" p (slot-value c 'to-be-imported) p)))))


(define-condition package-name-conflict-error (package-error simple-error) ())

(define-condition package-is-used-by (package-error)
  ((using-packages :initarg :using-packages))
  (:report (lambda (c s)
             (format s "~S is used by ~S" (package-error-package c)
                     (slot-value c 'using-packages)))))

(define-condition symbol-name-not-accessible (package-error)
  ((symbol-name :initarg :symbol-name))
  (:report (lambda (c s)
             (format s "No aymbol named ~S is accessible in package ~s"
                     (slot-value c 'symbol-name)
                     (package-error-package c)))))

(define-condition stream-error (error)
  ((stream :initarg :stream :reader stream-error-stream)))

(defun stream-error-context (condition)
  (let* ((stream (stream-error-stream condition)))
    (with-output-to-string (s)
       (format s "on ~s" stream)
       (let* ((pos (ignore-errors (stream-position stream))))
         (when pos
           (format s ", near position ~d" pos)))
       (let* ((surrounding (stream-surrounding-characters stream)))
         (when surrounding
           (format s ", within ~s" surrounding))))))

(define-condition parse-error (error) ())
(define-condition parse-integer-not-integer-string (parse-error)
  ((string :initarg :string))
  (:report (lambda (c s)
	     (format s "Not an integer string: ~s" (slot-value c 'string)))))

(define-condition reader-error (parse-error stream-error) ())
(define-condition end-of-file (stream-error) ()
  (:report (lambda (c s)
             (format s "Unexpected end of file ~a" (stream-error-context c)))))

(define-condition io-timeout (stream-error)
  ())

(define-condition input-timeout (io-timeout)
  ()
  (:report (lambda (c s)
             (format s "Input timeout on ~s" (stream-error-stream c)))))
(define-condition output-timeout (io-timeout)
  ()
  (:report (lambda (c s)
             (format s "Output timeout on ~s" (stream-error-stream c)))))
(define-condition communication-deadline-expired (io-timeout)
  ()
  (:report (lambda (c s)
             (format s "Communication deadline timeout on ~s" (stream-error-stream c)))))
 



(define-condition impossible-number (reader-error)
  ((token :initarg :token :reader impossible-number-token)
   (condition :initarg :condition :reader impossible-number-condition))
  (:report (lambda (c s)
             (format s "Condition of type ~s raised ~&while trying to parse numeric token ~s ~&~s"
                     (type-of (impossible-number-condition c))
                     (impossible-number-token c)
                     (stream-error-context c)))))


    
(define-condition simple-stream-error (stream-error simple-condition) () 
  (:report (lambda (c s) 
             (format s "~a : ~&~a" (stream-error-context c) 
                     (apply #'format
                            nil
                            (simple-condition-format-control c)
                            (simple-condition-format-arguments c))))))




(define-condition file-error (error)
  ((pathname :initarg :pathname :initform "<unspecified>" :reader file-error-pathname)
   (error-type :initarg :error-type :initform "File error on file ~S"))
  (:report (lambda (c s)
              (format s (slot-value c 'error-type) 
                     (file-error-pathname c)))))

(define-condition simple-file-error (simple-condition file-error)
  ()
  (:report (lambda (c s)
	     (apply #'format s (slot-value c 'error-type) 
		    (file-error-pathname c)
		    (simple-condition-format-arguments c)))))


(define-condition namestring-parse-error (error)
  ((complaint :reader namestring-parse-error-complaint :initarg :complaint)
   (arguments :reader namestring-parse-error-arguments :initarg :arguments
	      :initform nil)
   (namestring :reader namestring-parse-error-namestring :initarg :namestring)
   (offset :reader namestring-parse-error-offset :initarg :offset))
  (:report (lambda (condition stream)  
  (format stream "Parse error in namestring: ~?~%  ~A~%  ~V@T^"
	  (namestring-parse-error-complaint condition)
	  (namestring-parse-error-arguments condition)
	  (namestring-parse-error-namestring condition)
	  (namestring-parse-error-offset condition)))))

(define-condition cell-error (error)
  ((name :initarg :name :reader cell-error-name)
   (error-type :initarg :error-type :initform "Cell error" :reader cell-error-type))
  (:report (lambda (c s) (format s "~A: ~S" (cell-error-type c) (cell-error-name c)))))

(define-condition unbound-variable (cell-error)
  ((error-type :initform "Unbound variable")))

(define-condition undefined-function (cell-error)
  ((error-type :initform "Undefined function")))
(define-condition undefined-function-call (control-error undefined-function)
  ((function-arguments :initarg :function-arguments :reader undefined-function-call-arguments))
  (:report (lambda (c s) (format s "Undefined function ~S called with arguments ~:S ."
                                 (cell-error-name c)
                                 (undefined-function-call-arguments c)))))

(define-condition call-special-operator-or-macro (undefined-function-call)
  ()
  (:report (lambda (c s) (format s "Special operator or global macro-function ~s can't be FUNCALLed or APPLYed" (cell-error-name c)))))

  
(define-condition unbound-slot (cell-error)
  ((instance :initarg :instance :accessor unbound-slot-instance))
  (:report (lambda (c s) (format s "Slot ~s is unbound in ~s"
                                 (cell-error-name c)
                                 (unbound-slot-instance c)))))
  

(define-condition arithmetic-error (error) 
  ((operation :initform nil :initarg :operation :reader arithmetic-error-operation)
   (operands :initform nil :initarg :operands :reader arithmetic-error-operands)
   (status :initform nil :initarg :status :reader arithmetic-error-status))
  (:report (lambda (c s)
             (format s "~S detected" (type-of c))
             (let* ((operands (arithmetic-error-operands c)))
               (when operands
                 (format s "~&performing ~A on ~:S"
                         (arithmetic-error-operation c) 
                         operands))))))

(define-condition division-by-zero (arithmetic-error) ())
  
(define-condition floating-point-underflow (arithmetic-error) ())
(define-condition floating-point-overflow (arithmetic-error) ())
(define-condition floating-point-inexact (arithmetic-error) ())
(define-condition floating-point-invalid-operation (arithmetic-error) ())

(define-condition compiler-bug (simple-error)
  ()
  (:report (lambda (c stream)
                  (format stream "Compiler bug or inconsistency:~%")
                  (apply #'format stream (simple-condition-format-control c)
                         (simple-condition-format-arguments c)))))

(define-condition external-process-creation-failure (serious-condition)
  ((proc :initarg :proc))
  (:report (lambda (c stream)
             (with-slots (proc) c
               (let* ((code (external-process-%exit-code proc)))
                 (format stream "Fork failed in ~s: ~a. " proc (if (eql code -1) "random lisp error" (%strerror code))))))))
   
                         
(defun restartp (thing) 
  (istruct-typep thing 'restart))
(setf (type-predicate 'restart) 'restartp)

(defmethod print-object ((restart restart) stream)
  (let ((report (%restart-report restart)))
    (cond ((or *print-escape* (null report))
           (print-unreadable-object (restart stream :identity t)
             (format stream "~S ~S"
                     'restart (%restart-name restart))))
          ((stringp report)
           (write-string report stream))
          (t
           (funcall report stream)))))

(defun %make-restart (name action report interactive test)
  (%cons-restart name action report interactive test))

(defun make-restart (vector name action-function &key report-function interactive-function test-function)
  (unless vector (setq vector (%cons-restart nil nil nil nil nil)))
  (setf (%restart-name vector) name
        (%restart-action vector) (require-type action-function 'function)
        (%restart-report vector) (if report-function (require-type report-function 'function))
        (%restart-interactive vector) (if interactive-function (require-type interactive-function 'function))
        (%restart-test vector) (if test-function (require-type test-function 'function)))
  vector)

(defun restart-name (restart)
  "Return the name of the given restart object."
  (%restart-name (require-type restart 'restart)))

(defun applicable-restart-p (restart condition)
  (let* ((pair (if condition (assq restart *condition-restarts*)))
         (test (%restart-test restart)))
    (and (or (null pair) (eq (%cdr pair) condition))
         (or (null test) (funcall test condition)))))

(defun compute-restarts (&optional condition &aux restarts)
  "Return a list of all the currently active restarts ordered from most
   recently established to less recently established. If CONDITION is
   specified, then only restarts associated with CONDITION (or with no
   condition) will be returned."
  (dolist (cluster %restarts% (nreverse restarts))
    (dolist (restart cluster)
      (when (applicable-restart-p restart condition)
        (push restart restarts)))))

(defun find-restart (name &optional condition)
  "Return the first active restart named NAME. If NAME names a
   restart, the restart is returned if it is currently active. If no such
   restart is found, NIL is returned. It is an error to supply NIL as a
   name. If CONDITION is specified and not NIL, then only restarts
   associated with that condition (or with no condition) will be
   returned."
  (dolist (cluster %restarts%)
    (dolist (restart cluster)
      (when (and (or (eq restart name) (eq (restart-name restart) name))
                 (applicable-restart-p restart condition))
	(return-from find-restart restart)))))

(defun %active-restart (name)
  (dolist (cluster %restarts%)
    (dolist (restart cluster)
      (let* ((rname (%restart-name restart))
	     (rtest (%restart-test restart)))
	(when (and (or (eq restart name) (eq rname name))
		   (or (null rtest) (funcall rtest nil)))
	  (return-from %active-restart (values restart cluster))))))
  (error 'inactive-restart :restart-name name))

(defun invoke-restart (restart &rest values)
  "Calls the function associated with the given restart, passing any given
   arguments. If the argument restart is not a restart or a currently active
   non-nil restart name, then a CONTROL-ERROR is signalled."
  (multiple-value-bind (restart tag) (%active-restart restart)
    (let ((fn (%restart-action restart)))
      (cond ((null fn)                  ; simple restart
             (unless (null values) (%err-disp $xtminps))
             (throw tag nil))
            ((fixnump fn)               ; restart case
             (throw tag (cons fn values)))
            ((functionp fn)		; restart bind
	     (apply fn values))		
	    (t				; with-simple-restart
	     (throw tag (values nil t)))))))

(defun invoke-restart-no-return (restart)
  (invoke-restart restart)
  (error 'restart-failure :restart restart))

(defun invoke-restart-interactively (restart)
  "Calls the function associated with the given restart, prompting for any
   necessary arguments. If the argument restart is not a restart or a
   currently active non-NIL restart name, then a CONTROL-ERROR is signalled."
  (let* ((restart (find-restart restart)))
    (format *error-output* "~&Invoking restart: ~a~&" restart)
    (let* ((argfn (%restart-interactive restart))
           (values (when argfn (funcall argfn))))
      (apply #'invoke-restart restart values))))



(defun maybe-invoke-restart (restart value condition)
  (let ((restart (find-restart restart condition)))
    (when restart (invoke-restart restart value))))

(defun use-value (value &optional condition)
  "Transfer control and VALUE to a restart named USE-VALUE, or return NIL if
   none exists."
  (maybe-invoke-restart 'use-value value condition))

(defun store-value (value &optional condition)
  "Transfer control and VALUE to a restart named STORE-VALUE, or return NIL if
   none exists."
  (maybe-invoke-restart 'store-value value condition))

(defun condition-arg (thing args type)
  (cond ((condition-p thing) (if args (%err-disp $xtminps) thing))
        ((symbolp thing) (apply #'make-condition thing args))
        (t (make-condition type :format-control thing :format-arguments args))))

(defun make-condition (name &rest init-list)
  "Make an instance of a condition object using the specified initargs."
  (declare (dynamic-extent init-list))
  (if (subtypep name 'condition)
    (apply #'make-instance name init-list)
    (let ((class (if (classp name)
		   name
		   (find-class name)))) ;; elicit an error if no such class
      (unless (class-finalized-p class)
	(finalize-inheritance class)) ;; elicit an error if forward refs.
      (error "~S is not a condition class" class))))

(defmethod print-object ((c condition) stream)
  (if *print-escape* 
    (call-next-method)
    (report-condition c stream)))

(defmethod report-condition ((c condition) stream)
  (princ (cond ((typep c 'error) "Error ")
               ((typep c 'warning) "Warning ")
               (t "Condition "))
         stream)
  ;Here should dump all slots or something.  For now...
  (let ((*print-escape* t))
    (print-object c stream)))

(defun signal-simple-condition (class-name format-string &rest args)
  (let ((e #'error))  ; Never-tail-call.
    (funcall e (make-condition class-name :format-control format-string :format-arguments args))))

(defun signal-simple-program-error (format-string &rest args)
  (apply #'signal-simple-condition 'simple-program-error format-string args))

;;getting the function name for error functions.


(defun %last-fn-on-stack (&optional (number 0) (s (%get-frame-ptr)))
  (let* ((fn nil))
    (let ((p s))
      (dotimes (i number)
        (declare (fixnum i))
        (unless (setq p (parent-frame p nil))
          (return)))
      (do* ((i number (1+ i)))
           ((null p))
        (if (setq fn (cfp-lfun p))
          (return (values fn i))
          (setq p (parent-frame p nil)))))))
 
(defun %err-fn-name (lfun)
  "given an lfun returns the name or the string \"Unknown\""
  (if (lfunp lfun) (or (lfun-name lfun) lfun)
     (or lfun "Unknown")))

(defun %real-err-fn-name (error-pointer)
  (multiple-value-bind (fn p) (%last-fn-on-stack 0 error-pointer)
    (let ((name (%err-fn-name fn)))
      (if (and (memq name '( call-check-regs)) p)
        (%err-fn-name (%last-fn-on-stack (1+ p) error-pointer))
        name))))


;; Some simple restarts for simple error conditions.  Callable from the kernel.

(defun find-unique-homonyms (name &optional (test (constantly t)))
  (loop
    with symbol = (if (consp name) (second name) name)
    with pname = (symbol-name symbol)
    for package in (list-all-packages)
    for other-package-symbol = (find-symbol pname package)
    for canditate = (and other-package-symbol
			 (neq other-package-symbol symbol)
			 (if (consp name)
			   (list (first name) other-package-symbol)
			   other-package-symbol))
    when (and canditate
              (funcall test canditate))
    collect canditate))

(def-kernel-restart $xvunbnd %default-unbound-variable-restarts (frame-ptr cell-name)
  (unless *level-1-loaded*
    (dbg cell-name))       ;  user should never see this.
  (let ((condition (make-condition 'unbound-variable :name cell-name))
	(other-variables (find-unique-homonyms cell-name (lambda (name)
                                                           (and (not (keywordp name))
                                                                (boundp name))))))
    (flet ((new-value ()
             (catch-cancel
              (return-from new-value
                           (list (read-from-string 
                                  (get-string-from-user
                                   (format nil "New value for ~s : " cell-name))))))
             (continue condition))) ; force error again if cancelled, var still not set.
      (restart-case (%error condition nil frame-ptr)
        (continue ()
                  :report (lambda (s) (format s "Retry getting the value of ~S." cell-name))
                  (symbol-value cell-name))
        (use-homonym (homonym)
                     :test (lambda (c) (and (or (null c) (eq c condition)) other-variables))
                     :report (lambda (s)
                               (if (= 1 (length other-variables))
                                 (format s "Use the value of ~s this time." (first other-variables))
                                 (format s "Use one of the homonyms ~{~S or ~} this time." other-variables)))
                     :interactive (lambda ()
                                    (if (= 1 (length other-variables))
                                      other-variables
                                      (select-item-from-list other-variables :window-title "Select homonym to use")))
                     (symbol-value homonym))
        (use-value (value)
                   :interactive new-value
                   :report (lambda (s) (format s "Specify a value of ~S to use this time." cell-name))
                   value)
        (store-value (value)
                     :interactive new-value
                     :report (lambda (s) (format s "Specify a value of ~S to store and use." cell-name))
                     (setf (symbol-value cell-name) value))))))

(def-kernel-restart $xnopkg %default-no-package-restart (frame-ptr package-name)
  (or (and *autoload-lisp-package*
           (or (string-equal package-name "LISP") 
               (string-equal package-name "USER"))
           (progn
             (require "LISP-PACKAGE")
             (find-package package-name)))
      (let* ((alias (or (%cdr (assoc package-name '(("LISP" . "COMMON-LISP")
                                                    ("USER" . "CL-USER")) 
                                     :test #'string-equal))
                        (if (packagep *package*) (package-name *package*))))
             (condition (make-condition 'no-such-package :package package-name)))
        (flet ((try-again (p)
                          (or (find-package p) (%kernel-restart $xnopkg p))))
          (restart-case
            (restart-case (%error condition nil frame-ptr)
              (continue ()
                        :report (lambda (s) (format s "Retry finding package with name ~S." package-name))
                        (try-again package-name))
              (use-value (value)
                         :interactive (lambda () (block nil 
                                                   (catch-cancel
                                                    (return (list (get-string-from-user
                                                                   "Find package named : "))))
                                                   (continue condition)))
                         :report (lambda (s) (format s "Find specified package instead of ~S ." package-name))
                         (try-again value))
              (make-nickname ()
                             :report (lambda (s) (format s "Make ~S be a nickname for package ~S." package-name alias))
                             (let ((p (try-again alias)))
                               (push package-name (cdr (pkg.names p)))
                               p)))
            (require-lisp-package ()
                                  :test (lambda (c)
                                          (and (eq c condition)
                                               (or (string-equal package-name "LISP") (string-equal package-name "USER"))))
                                  :report (lambda (s) 
                                            (format s "(require :lisp-package) and retry finding package ~s"
                                                    package-name))
                                  (require "LISP-PACKAGE")
                                  (try-again package-name)))))))

(def-kernel-restart $xunintc unintern-conflict-restarts (frame-ptr sym package conflicts)
  (let ((condition (make-condition 'unintern-conflict-error :package package :sym sym :conflicts conflicts)))
    (restart-case (%error condition nil frame-ptr)
      (continue ()
                :report (lambda (s) (format s "Try again to unintern ~s from ~s" sym package))
                (unintern sym package))
      (do-shadowing-import (ssym)
                           :report (lambda (s) (format s "SHADOWING-IMPORT one of ~S in ~S." conflicts package))
                           :interactive (lambda ()
                                          (block nil
                                            (catch-cancel
                                             (return (select-item-from-list conflicts 
                                                                            :window-title 
                                                                            (format nil "Shadowing-import one of the following in ~s" package)
                                                                            :table-print-function #'prin1)))
                                            (continue condition)))
                           (shadowing-import (list ssym) package)))))


(def-kernel-restart $xusec blub (frame-ptr package-to-use using-package conflicts)
  (resolve-use-package-conflict-error frame-ptr package-to-use using-package conflicts nil))

(def-kernel-restart $xusecX blub (frame-ptr package-to-use using-package conflicts)
  (resolve-use-package-conflict-error frame-ptr package-to-use using-package conflicts t))

(defun resolve-use-package-conflict-error (frame-ptr package-to-use using-package conflicts external-p)
  (let ((condition (make-condition 'use-package-conflict-error 
                                   :package using-package
                                   :package-to-use package-to-use
                                   :conflicts conflicts
                                   :external-p external-p)))
    (flet ((external-test (&rest ignore) (declare (ignore ignore)) external-p)
           (present-test (&rest ignore) (declare (ignore ignore)) (not external-p)))
      (declare (dynamic-extent #'present-test #'external-test))
      (restart-case (%error condition nil frame-ptr)
        (continue ()
                  :report (lambda (s) (format s "Try again to use ~s in ~s" package-to-use using-package)))
        (resolve-by-shadowing-import (&rest shadowing-imports)
                                     :test external-test
                                     :interactive (lambda ()
                                                    (mapcar #'(lambda (pair) 
                                                                (block nil
                                                                  (catch-cancel
                                                                    (return (car (select-item-from-list pair
                                                                                                        :window-title 
                                                                                                        (format nil "Shadowing-import one of the following in ~s" using-package)
                                                                                                        :table-print-function #'prin1))))
                                                                  (continue condition)))
                                                            conflicts))
                                     :report (lambda (s) (format s "SHADOWING-IMPORT one of each pair of conflicting symbols."))
                                     (shadowing-import shadowing-imports using-package))
        (unintern-all ()
                      :test present-test
                      :report (lambda (s) (format s "UNINTERN all conflicting symbols from ~S" using-package))
                      (dolist (c conflicts)
                        (unintern (car c) using-package)))
        (shadow-all ()
                      :test present-test
                      :report (lambda (s) (format s "SHADOW all conflicting symbols in ~S" using-package))
                      (dolist (c conflicts)
                        (shadow-1 using-package (car c))))
        (resolve-by-unintern-or-shadow (&rest dispositions)
                                       :test present-test
                                       :interactive (lambda ()
                                                      (mapcar #'(lambda (pair)
                                                                  (let* ((present-sym (car pair)))
                                                                    (block nil
                                                                      (catch-cancel
                                                                        (return (car (select-item-from-list (list 'shadow 'unintern) 
                                                                                                            :window-title
                                                                                                            (format nil "SHADOW ~S in, or UNINTERN ~S from ~S" 
                                                                                                                    present-sym 
                                                                                                                    present-sym
                                                                                                                    using-package)
                                                                                                            :table-print-function #'prin1)))
                                                                        (continue condition)))))
                                                              conflicts))
                                       :report (lambda (s) (format s "SHADOW or UNINTERN the conflicting symbols in ~S." using-package))
                                       (dolist (d dispositions)
                                         (let* ((sym (car (pop conflicts))))
                                           (if (eq d 'shadow)
                                             (shadow-1 using-package sym)
                                             (unintern sym using-package)))))))))


(defun resolve-export-conflicts (conflicts package)
  (let* ((first-inherited (caar conflicts))
         (all-same (dolist (conflict (cdr conflicts) t)
                     (unless (eq (car conflict) first-inherited) (return nil))))
         (all-inherited (and all-same first-inherited))
         (all-present (and all-same (not first-inherited)))
         (condition (make-condition 'export-conflict-error
                                    :conflicts conflicts
                                    :package package)))
    (flet ((check-again () 
             (let* ((remaining-conflicts (check-export-conflicts (mapcar #'cadr conflicts) package)))
               (if remaining-conflicts (resolve-export-conflicts remaining-conflicts package)))))
      (restart-case (%error condition nil (%get-frame-ptr))
        (resolve-all-by-shadowing-import-inherited 
         ()
         :test (lambda (&rest ignore) (declare (ignore ignore)) all-inherited)
         :report (lambda (s) (format s "SHADOWING-IMPORT all conflicting inherited symbol(s) in using package(s)."))
         (dolist (conflict conflicts (check-again))
           (destructuring-bind (using-package inherited-sym) (cddr conflict)
             (shadowing-import-1 using-package inherited-sym))))
        (resolve-all-by-shadowing-import-exported 
         ()
         :test (lambda (&rest ignore) (declare (ignore ignore)) all-inherited)
         :report (lambda (s) (format s "SHADOWING-IMPORT all conflicting symbol(s) to be exported in using package(s)."))
         (dolist (conflict conflicts (check-again))
           (destructuring-bind (exported-sym using-package ignore) (cdr conflict)
             (declare (ignore ignore))
             (shadowing-import-1 using-package exported-sym))))
        (resolve-all-by-uninterning-present 
         ()
         :test (lambda (&rest ignore) (declare (ignore ignore)) all-present)
         :report (lambda (s) (format s "UNINTERN all present conflicting symbol(s) in using package(s)."))
         (dolist (conflict conflicts (check-again))
           (destructuring-bind (using-package inherited-sym) (cddr conflict)
             (unintern inherited-sym using-package))))
        (resolve-all-by-shadowing-present 
         ()
         :test (lambda (&rest ignore) (declare (ignore ignore)) all-present)
         :report (lambda (s) (format s "SHADOW all present conflicting symbol(s) in using package(s)."))
         (dolist (conflict conflicts (check-again))
           (destructuring-bind (using-package inherited-sym) (cddr conflict)
             (shadow-1 using-package inherited-sym))))
        (review-and-resolve 
         (dispositions)
         :report (lambda (s) (format s "Review each name conflict and resolve individually."))
         :interactive (lambda ()
                        (let* ((disp nil))
                          (block b
                            (catch-cancel
                              (dolist (conflict conflicts (return-from b (list disp)))
                                (destructuring-bind (inherited-p exported-sym using-package conflicting-sym) conflict
                                  (let* ((syms (list exported-sym conflicting-sym)))
                                    (if inherited-p
                                      (push (list 'shadowing-import
                                                  (select-item-from-list syms
                                                                              :window-title 
                                                                              (format nil "Shadowing-import one of the following in ~s" using-package)
                                                                              :table-print-function #'prin1)
                                                  using-package)
                                            disp)
                                      (let* ((selection (car (select-item-from-list syms
                                                                                    :window-title 
                                                                                    (format nil "Shadow ~S or unintern ~s in ~s"
                                                                                            exported-sym 
                                                                                            conflicting-sym using-package)
                                                                                    :table-print-function #'prin1))))
                                        (push (if (eq selection 'exported-sym)
                                                (list 'shadow (list exported-sym) using-package)
                                                (list 'unintern conflicting-sym using-package))
                                              disp)))))))
                            nil)))
         (dolist (disp dispositions (check-again))
           (apply (car disp) (cdr disp))))))))


(def-kernel-restart $xwrongtype default-require-type-restarts (frame-ptr value typespec)
  (setq typespec (%type-error-type typespec))
  (let ((condition (make-condition 'type-error 
                                   :datum value
                                   :expected-type typespec)))
    (restart-case (%error condition nil frame-ptr)
      (use-value (newval)
                 :report (lambda (s)
                           (format s "Use a new value of type ~s instead of ~s." typespec value))
                 :interactive (lambda ()
                                (format *query-io* "~&New value of type ~S :" typespec)
                                (list (read *query-io*)))
                 (require-type newval typespec)))))

(def-kernel-restart $xudfcall default-undefined-function-call-restarts (frame-ptr function-name args)
  (unless *level-1-loaded*
    (dbg function-name))   ; user should never see this
  (let ((condition (make-condition 'undefined-function-call
                                   :name function-name
                                   :function-arguments args))
	(other-functions (find-unique-homonyms function-name #'fboundp)))
    (restart-case (%error condition nil frame-ptr)
      (continue ()
                :report (lambda (s) (format s "Retry applying ~S to ~S." function-name args))
                (apply function-name args))
      (use-homonym (function-name)
                   :test (lambda (c) (and (or (null c) (eq c condition)) other-functions))
                   :report (lambda (s)
                             (if (= 1 (length other-functions))
                               (format s "Apply ~s to ~S this time." (first other-functions) args)
                               (format s "Apply one of ~{~S or ~} to ~S this time.")))
                   :interactive (lambda ()
                                  (if (= 1 (length other-functions))
                                    other-functions
                                    (select-item-from-list other-functions :window-title "Select homonym to use")))
                   (apply (fdefinition function-name) args))
      (use-value (function)
                 :interactive (lambda ()
                                (format *query-io* "Function to apply instead of ~s :" function-name)
                                (let ((f (read *query-io*)))
                                  (unless (symbolp f) (setq f (eval f))) ; almost-the-right-thing (tm)
                                  (list (coerce f 'function))))
                 :report (lambda (s) (format s "Apply specified function to ~S this time." args))
                 (apply function args))
      (store-value (function)
                   :interactive (lambda ()
                                (format *query-io* "Function to apply as new definition of ~s :" function-name)
                                (let ((f (read *query-io*)))
                                  (unless (symbolp f) (setq f (eval f))) ; almost-the-right-thing (tm)
                                  (list (coerce f 'function))))
                   :report (lambda (s) (format s "Specify a function to use as the definition of ~S." function-name))
                   (apply (setf (symbol-function function-name) function) args)))))



(defun %check-type (value typespec placename typename)
  (let ((condition (make-condition 'type-error 
                                   :datum value
                                   :expected-type typespec)))
    (if typename
      (setf (slot-value condition 'format-control)
            (format nil "value ~~S is not ~A (~~S)." typename)))
    (restart-case (%error condition nil (%get-frame-ptr))
                  (store-value (newval)
                               :report (lambda (s)
                                         (format s "Assign a new value of type ~a to ~s" typespec placename))
                               :interactive (lambda ()
                                              (format *query-io* "~&New value for ~S :" placename)
                                              (list (eval (read))))
                               newval))))


; This has to be defined fairly early (assuming, of course, that it "has" to be defined at all ...

(defun ensure-value-of-type (value typespec placename &optional typename)
  (tagbody
    again
    (unless (typep value typespec)
      (let ((condition (make-condition 'type-error 
                                       :datum value
                                       :expected-type typespec)))
        (if typename
            (setf (slot-value condition 'format-control)
                  (format nil "value ~~S is not ~A (~~S)." typename)))
        (restart-case (%error condition nil (%get-frame-ptr))
          (store-value (newval)
                       :report (lambda (s)
                                 (format s "Assign a new value of type ~a to ~s" typespec placename))
                       :interactive (lambda ()
                                      (format *query-io* "~&New value for ~S :" placename)
                                      (list (eval (read))))
                       (setq value newval)
                       (go again))))))
  value)

;;;The Error Function

(defparameter *kernel-simple-error-classes*
  (list (cons $xcalltoofew 'simple-destructuring-error)
        (cons $xcalltoomany 'simple-destructuring-error)
        (cons $xstkover 'stack-overflow-condition)
        (cons $xmemfull 'simple-storage-condition)
        (cons $xwrongtype 'type-error) ; this one needs 2 args
        (cons $xdivzro 'division-by-zero)
        (cons $xflovfl 'floating-point-overflow)
        (cons $xfunbnd 'undefined-function)
	(cons $xbadkeys 'simple-program-error)
        (cons $xnotfun 'call-special-operator-or-macro)
        (cons $xaccessnth 'sequence-index-type-error)
	(cons $ximproperlist 'improper-list)
	(cons $xnospread 'cant-construct-arglist)
        (cons $xnotelt 'array-element-type-error)
        ))


(defparameter *simple-error-types*
  (vector nil 'simple-program-error 'simple-file-error))

(defconstant $pgm-err #x10000)




(defparameter %type-error-typespecs%
  #(array
    bignum
    fixnum
    character
    integer
    list
    number
    sequence
    simple-string
    simple-vector
    string
    symbol
    macptr
    real
    cons
    unsigned-byte
    (integer 2 36)
    float
    rational
    ratio
    short-float
    double-float
    complex
    vector
    simple-base-string
    function
    (unsigned-byte 16)
    (unsigned-byte 8)
    (unsigned-byte 32)
    (signed-byte 32)
    (signed-byte 16)
    (signed-byte 8)
    base-char
    bit
    (unsigned-byte 24)                  ; (integer 0 (array-total-size-limit))
    (unsigned-byte 64)
    (signed-byte 64)
    (unsigned-byte 56)
    (simple-array double-float (* *))
    (simple-array single-float (* *))
    (mod #x110000)
    (array * (* *))                     ;2d array
    (array * (* * *))                   ;3d array
    (array t)
    (array bit)
    (array (signed-byte 8))
    (array (unsigned-byte 8))
    (array (signed-byte 16))
    (array (unsigned-byte 16))
    (array (signed-byte 32))
    (array (unsigned-byte 32))
    (array (signed-byte 64))
    (array (unsigned-byte 64))
    (array fixnum)
    (array single-float)
    (array double-float)
    (array character)
    (array t (* *))
    (array bit (* *))
    (array (signed-byte 8) (* *))
    (array (unsigned-byte 8) (* *))
    (array (signed-byte 16) (* *))
    (array (unsigned-byte 16) (* *))
    (array (signed-byte 32) (* *))
    (array (unsigned-byte 32) (* *))
    (array (signed-byte 64) (* *))
    (array (unsigned-byte 64) (* *))
    (array fixnum (* *))
    (array single-float (* *))
    (array double-float (* *))
    (array character (* *))
    (simple-array t (* *))
    (simple-array bit (* *))
    (simple-array (signed-byte 8) (* *))
    (simple-array (unsigned-byte 8) (* *))
    (simple-array (signed-byte 16) (* *))
    (simple-array (unsigned-byte 16) (* *))
    (simple-array (signed-byte 32) (* *))
    (simple-array (unsigned-byte 32) (* *))
    (simple-array (signed-byte 64) (* *))
    (simple-array (unsigned-byte 64) (* *))
    (simple-array fixnum (* *))
    (simple-array character (* *))
    (array t (* * *))
    (array bit (* * *))
    (array (signed-byte 8) (* * *))
    (array (unsigned-byte 8) (* * *))
    (array (signed-byte 16) (* * *))
    (array (unsigned-byte 16) (* * *))
    (array (signed-byte 32) (* * *))
    (array (unsigned-byte 32) (* * *))
    (array (signed-byte 64) (* * *))
    (array (unsigned-byte 64) (* * *))
    (array fixnum (* * *))
    (array single-float (* * *))
    (array double-float (* * *))
    (array character (* * *))
    (simple-array t (* * *))
    (simple-array bit (* * *))
    (simple-array (signed-byte 8) (* * *))
    (simple-array (unsigned-byte 8) (* * *))
    (simple-array (signed-byte 16) (* * *))
    (simple-array (unsigned-byte 16) (* * *))
    (simple-array (signed-byte 32) (* * *))
    (simple-array (unsigned-byte 32) (* * *))
    (simple-array (signed-byte 64) (* * *))
    (simple-array (unsigned-byte 64) (* * *))
    (simple-array fixnum (* * *))
    (simple-array single-float (* * *))
    (simple-array double-float (* * *))
    (simple-array char (* * *))
    
    ))


(defun %type-error-type (type)
  (if (fixnump type) 
    (svref %type-error-typespecs% type)
    type))

(defun %typespec-id (typespec)
  (flet ((type-equivalent (t1 t2) (ignore-errors (and (subtypep t1 t2) (subtypep t2 t1)))))
    (position typespec %type-error-typespecs% :test #'type-equivalent)))


(defmethod condition-p ((x t)) nil)
(defmethod condition-p ((x condition)) t)



(let* ((globals ()))

  (defun %check-error-globals ()
    (let ((vars ())
          (valfs ())
          (oldvals ()))
      (dolist (g globals (values vars valfs oldvals))
        (destructuring-bind (sym predicate newvalf) g
          (let* ((boundp (boundp sym))
                 (oldval (if boundp (symbol-value sym) (%unbound-marker-8))))
          (unless (and boundp (funcall predicate oldval))
            (push sym vars)
            (push oldval oldvals)
            (push newvalf valfs)))))))

  (defun check-error-global (sym checkfn newvalfn)
    (setq sym (require-type sym 'symbol)
          checkfn (require-type checkfn 'function)
          newvalfn (require-type newvalfn 'function))
    (let ((found (assq sym globals)))
      (if found
        (setf (cadr found) checkfn (caddr found) newvalfn)
        (push (list sym checkfn newvalfn) globals))
      sym))
)

(check-error-global '*package* #'packagep #'(lambda () (find-package "CL-USER")))


  

