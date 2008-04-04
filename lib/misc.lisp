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

(eval-when (eval compile)
  (require 'defstruct-macros))

(defun short-site-name  ()
  "Return a string with the abbreviated site name, or NIL if not known."
  (or *short-site-name* "unspecified"))

(defun long-site-name   ()
  "Return a string with the long form of the site name, or NIL if not known."
  (or *long-site-name* "unspecified"))

(defun machine-instance ()
  "Return a string giving the name of the local machine."
  (%uname 1))


(defun machine-type ()
  "Returns a string describing the type of the local machine."
  (%uname 4))



(defloadvar *machine-version* nil)

(defun machine-version ()
  "Return a string describing the version of the computer hardware we
are running on, or NIL if we can't find any useful information."
  (or *machine-version*
      (setq *machine-version*
            #+darwin-target
            (block darwin-machine-version
              (%stack-block ((mib 8))
                (setf (%get-long mib 0) #$CTL_HW
                      (%get-long mib 4) #$HW_MODEL)
                (%stack-block ((res 256)
                               (reslen target::node-size))
                  (setf (%get-byte res 0) 0
                        (%get-natural reslen 0) 256)
                  (if (zerop (#_sysctl mib 2 res reslen (%null-ptr) 0))
                    (return-from darwin-machine-version (%get-cstring res))))))
            #+linux-target
            (with-open-file (f "/proc/cpuinfo" :if-does-not-exist nil)
              (when f
                (flet ((cpu-info-match (target line)
                         (let* ((targetlen (length target))
                                (linelen (length line)))
                           (if (and (> linelen targetlen)
                                    (string= target line
                                             :end2 targetlen))
                           (let* ((colonpos (position #\: line)))
                             (when colonpos
                               (string-trim " "
                                            (subseq line (1+ colonpos)))))))))
                  (do* ((line (read-line f nil nil)
                              (read-line f nil nil))
                        (target #+ppc-target "machine"
                                #+x86-target "model name"))
                       ((null line))
                    (let* ((matched (cpu-info-match target line)))
                      (when matched (return matched)))))))
            #+freebsd-target
            (%stack-block ((ret 512)
                           (mib (* (record-length :uint))))
              (setf (%get-unsigned-long mib 0)
                    #$CTL_HW
                    (%get-unsigned-long mib (record-length :uint))
                    #$HW_MODEL)
              (rlet ((oldsize :uint 512))
                (if (eql 0 (#_sysctl mib 2 ret oldsize (%null-ptr) 0))
                  (%get-cstring ret)
                  1)))
            )))


(defun software-type ()
  "Return a string describing the supporting software."
  (%uname 0))


(defun software-version ()
  "Return a string describing version of the supporting software, or NIL
   if not available."
  (%uname 2))







;;; Yawn.



(defmethod documentation (thing doc-id)
  (%get-documentation thing doc-id))

(defun set-documentation (thing doc-id new)
  (setf (documentation thing doc-id) new))

(defmethod (setf documentation) (new thing doc-id)
  (%put-documentation thing doc-id new))


(defmethod documentation ((symbol symbol) (doc-type (eql 'function)))
  (let* ((def (fboundp symbol)))	; FBOUNDP returns info about definition
    (when def
      (%get-documentation def t))))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'function)))
  (let* ((def (fboundp symbol)))	; FBOUNDP returns info about definition
    (when def
      (%put-documentation def
                          t
                          new))
    new))

(defmethod documentation ((symbol symbol) (doc-type (eql 'setf)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'setf)))
  (call-next-method))


(defmethod documentation ((symbol symbol) (doc-type (eql 'variable)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'variable)))
  (call-next-method))

(defmethod documentation ((symbol symbol) (doc-type (eql 'compiler-macro)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'compiler-macro)))
  (call-next-method))

(defmethod documentation ((symbol symbol) (doc-type (eql 'type)))
  (let* ((class (find-class symbol nil)))
    (if class
      (documentation class doc-type)
      (call-next-method))))

(defmethod (setf documentation) (new (symbol symbol) (doc-type (eql 'type)))
  (let* ((class (find-class symbol nil)))
    (if class
      (setf (documentation class doc-type) new)
      (call-next-method))))

(defmethod documentation ((symbol symbol) (doc-type (eql 'method-combination)))
  (let* ((mci (method-combination-info symbol)))
    (if mci
      (documentation mci doc-type))))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'method-combination)))
  (let* ((mci (method-combination-info symbol)))
    (if mci
      (setf (documentation mci doc-type) new))))


(defmethod documentation ((symbol symbol) (doc-type (eql 'structure)))
  (let* ((class (find-class symbol nil)))
    (if (typep class 'structure-class)
      (documentation class 'type)
      (call-next-method))))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'structure)))
  (let* ((class (find-class symbol nil)))
    (if (typep class 'structure-class)
      (setf (documentation class 'type) new)
      (call-next-method))))

(defmethod documentation ((p package) (doc-type (eql 't)))
  (call-next-method))

(defmethod (setf documentation) ((new t) (p package) (doc-type (eql 't)))
  (call-next-method))

(defmethod documentation ((f function) (doc-type (eql 't)))
  (call-next-method))

(defmethod (setf documentation) ((new t) (f function) (doc-type (eql 't)))
  (call-next-method))

(defmethod documentation ((f function) (doc-type (eql 'function)))
  (documentation f t))

(defmethod (setf documentation) ((new t)
				 (f function)
				 (doc-type (eql 'function)))
  (setf (documentation f t) new))

(defmethod documentation ((l cons) (doc-type (eql 'function)))
  (let* ((name (setf-function-spec-name l)))
    (if name
      (documentation name doc-type)
      (%get-documentation l doc-type))))

(defmethod (setf documentation) ((new t) (l cons) (doc-type (eql 'function)))
  (let* ((name  (setf-function-spec-name l)))
    (if name
      (setf (documentation name doc-type) new)
      (%put-documentation l doc-type new))))


(defmethod documentation ((l cons) (doc-type (eql 'compiler-macro)))
  (let* ((name (setf-function-spec-name l)))
    (if name
      (documentation name doc-type)
      (%get-documentation l doc-type))))

(defmethod (setf documentation) ((new t) (l cons) (doc-type (eql 'compiler-macr0)))
  (let* ((name (setf-function-spec-name l)))
    (if name
      (setf (documentation name doc-type) new)
      (%put-documentation l doc-type new))))


(defmethod documentation ((m method-combination)
			  (doc-type (eql 'method-combination)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (m method-combination)
				 (doc-type (eql 'method-combination)))
  (call-next-method))

(defmethod documentation ((m method-combination)
			  (doc-type (eql t)))
  (documentation m 'method-combination))

(defmethod (setf documentation) ((new t)
				 (m method-combination)
				 (doc-type (eql t)))
  (setf (documentation m 'method-combination) new))

(defmethod documentation ((m standard-method)
			  (doc-type (eql t)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (m standard-method)
				 (doc-type (eql t)))
  (call-next-method))

(defmethod documentation ((c standard-class) (doc-type (eql 'type)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (c standard-class)
				 (doc-type (eql 'type)))
  (call-next-method))

(defmethod documentation ((c standard-class) (doc-type (eql 't)))
  (documentation c 'type))

(defmethod (setf documentation) ((new t)
				 (c standard-class)
				 (doc-type (eql 't)))
  (setf (documentation c 'type) new))

(defmethod documentation ((c structure-class) (doc-type (eql 'type)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (c structure-class)
				 (doc-type (eql 'type)))
  (call-next-method))

(defmethod documentation ((c structure-class) (doc-type (eql 't)))
  (documentation c 'type))

(defmethod (setf documentation) ((new t)
				 (c structure-class)
				 (doc-type (eql 't)))
  (setf (documentation c 'type) new))

;;; This is now deprecated; things which call it should stop doing so.
(defun set-documentation (symbol doc-type string)
  (setf (documentation symbol doc-type) string))

(defun set-function-info (symbol info)
  (let* ((doc-string (if (consp info) (car info) info)))
    (if (and *save-doc-strings* (stringp doc-string))
      (set-documentation  symbol 'function doc-string)))
  (let* ((cons (assq symbol *nx-globally-inline*))
         (lambda-expression (if (consp info) (cdr info))))
    (if (and (proclaimed-inline-p symbol)
             (not (compiler-special-form-p symbol))
             (lambda-expression-p lambda-expression)
             (let* ((lambda-list (cadr lambda-expression)))
               (and (not (memq '&lap lambda-list))
                    (not (memq '&method lambda-list))
                    (not (memq '&lexpr lambda-list)))))
      (if cons 
        (%rplacd cons lambda-expression)
        (push (cons symbol lambda-expression) *nx-globally-inline*))
      (if cons (setq *nx-globally-inline* (delete cons *nx-globally-inline*)))))
  symbol)


(setf (documentation 'if 'function)
      "If Predicate Then [Else]
  If Predicate evaluates to non-null, evaluate Then and returns its values,
  otherwise evaluate Else and return its values. Else defaults to NIL.")

(setf (documentation 'progn 'function)
      "progn form*
  Evaluates each FORM and returns the value(s) of the last FORM.")

(defmethod documentation ((thing character-encoding) (doc-type (eql t)))
  (character-encoding-documentation thing))

(defmethod (setf documentation) (new (thing character-encoding) (doc-type (eql t)))
  (check-type new (or null string))
  (setf (character-encoding-documentation thing) new))

(defmethod documentation ((thing symbol) (doc-type (eql 'character-encoding)))
  (let* ((encoding (lookup-character-encoding (intern (string thing) :keyword))))
    (when encoding
      (documentation encoding t))))

                                 


#|
(setf (documentation 'car 'variable) "Preferred brand of automobile")
(documentation 'car 'variable)
(setf (documentation 'foo 'structure) "the structure is grand.")
(documentation 'foo 'structure)
(setf (documentation 'foo 'variable) "the metasyntactic remarker")
(documentation 'foo 'variable)
(setf (documentation 'foo 'obscure) "no one really knows what it means")
(documentation 'foo 'obscure)
(setf (documentation 'foo 'structure) "the structure is solid")
(documentation 'foo 'function)
||#

;;

(defparameter *report-time-function* nil
  "If non-NULL, should be a function which accepts the following
   keyword arguments:
   :FORM              the form that was executed
   :RESULTS           a list of all values returned by the execution of FORM
   :ELAPSED-TIME      total elapsed (real) time, in internal-time-units-per-second
   :USER-TIME         elapsed user time, in internal-time-units-per-second
   :SYSTEM-TIME       elapsed system time, in internal-time-units-per-second
   :GC-TIME           total real time spent in the GC, in internal-time-units-per-second
   :BYTES-ALLOCATED   total bytes allocated
   :MINOR-PAGE-FAULTS minor page faults
   :MAJOR-PAGE-FAULTS major page faults
   :SWAPS             swaps")


(defun standard-report-time (&key form results elapsed-time user-time
                                  system-time gc-time bytes-allocated
                                  minor-page-faults major-page-faults
                                  swaps)
  (let* ((s *trace-output*)
         (units
          (ecase internal-time-units-per-second
            (1000000 "microseconds")
            (1000  "milliseconds")))
         (width
          (ecase internal-time-units-per-second
            (1000000 6)
            (1000  3)))
         (cpu-count (cpu-count)))
    (format s "~&~S took ~:D ~a (~,vF seconds) to run ~%~20twith ~D available CPU core~P."
            form elapsed-time units width (/ elapsed-time internal-time-units-per-second) cpu-count cpu-count)
    (format s "~&During that period, ~:D ~a (~,vF seconds) were spent in user mode" user-time units width (/ user-time internal-time-units-per-second))
    (format s "~&                    ~:D ~a (~,vF seconds) were spent in system mode" system-time units width(/ system-time internal-time-units-per-second))
    (unless (eql gc-time 0)
      (format s
              "~%~:D ~a (~,vF seconds) was spent in GC."
              gc-time units width (/ gc-time internal-time-units-per-second)))
    (unless (eql 0 bytes-allocated)
      (format s "~% ~:D bytes of memory allocated." bytes-allocated))
    (when (or (> minor-page-faults 0)
              (> major-page-faults 0)
              (> swaps 0))
      (format s
              "~% ~:D minor page faults, ~:D major page faults, ~:D swaps."
              minor-page-faults major-page-faults swaps))
    (format s "~&")
    (values-list results)))

(defun report-time (form thunk)
  (flet ((integer-size-in-bytes (i)
           (if (typep i 'fixnum)
             0
             (* (logand (+ 2 (uvsize i)) (lognot 1)) 4))))
    (rlet ((start :rusage)
	   (stop :rusage)
	   (timediff :timeval))
      (let* ((initial-real-time (get-internal-real-time))
	     (initial-gc-time (gctime))
	     (initial-consed (total-bytes-allocated))           
	     (initial-overhead (integer-size-in-bytes initial-consed)))
	(%%rusage start)
	(let* ((results (multiple-value-list (funcall thunk))))
          (declare (dynamic-extent results))
	  (%%rusage stop)	  
	  (let* ((new-consed (total-bytes-allocated))		     
		 (bytes-consed
		  (- new-consed (+ initial-overhead initial-consed)))
		 (elapsed-real-time
		  (- (get-internal-real-time) initial-real-time))
		 (elapsed-gc-time (- (gctime) initial-gc-time))
		 (elapsed-user-time
		  (progn
		    (%sub-timevals timediff
				   (pref stop :rusage.ru_utime)
				   (pref start :rusage.ru_utime))
                    (ecase internal-time-units-per-second
                      (1000000 (timeval->microseconds timediff))
                      (1000 (timeval->milliseconds timediff)))))
		 (elapsed-system-time
		  (progn
		    (%sub-timevals timediff
				   (pref stop :rusage.ru_stime)
				   (pref start :rusage.ru_stime))
                    (ecase internal-time-units-per-second
                      (1000000 (timeval->microseconds timediff))
                      (1000 (timeval->milliseconds timediff)))))
		 (elapsed-minor (- (pref stop :rusage.ru_minflt)
				   (pref start :rusage.ru_minflt)))
		 (elapsed-major (- (pref stop :rusage.ru_majflt)
				   (pref start :rusage.ru_majflt)))
		 (elapsed-swaps (- (pref stop :rusage.ru_nswap)
				   (pref start :rusage.ru_nswap))))
            (funcall (or *report-time-function*
                         #'standard-report-time)
                     :form form
                     :results results
                     :elapsed-time elapsed-real-time
                     :user-time elapsed-user-time
                     :system-time elapsed-system-time
                     :gc-time elapsed-gc-time
                     :bytes-allocated bytes-consed
                     :minor-page-faults elapsed-minor
                     :major-page-faults elapsed-major
                     :swaps elapsed-swaps)))))))




;;; site names and machine-instance is in the init file.

(defun add-feature (symbol)
  "Not CL but should be."
  (if (symbolp symbol)
      (if (not (memq symbol *features*))
          (setq *features* (cons symbol *features*)))))

;;; (dotimes (i 5000) (declare (fixnum i)) (add-feature 'junk))




;;; Misc string functions


(defun string-left-trim (char-bag string &aux end)
  "Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  left end."
  (setq string (string string))
  (setq end (length string))
  (do ((index 0 (%i+ index 1)))
      ((or (eq index end) (not (find (aref string index) char-bag)))
       (subseq string index end))))

(defun string-right-trim (char-bag string &aux end)
  "Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  right end."
  (setq string (string string))
  (setq end (length string))
  (do ((index (%i- end 1) (%i- index 1)))
      ((or (%i< index 0) (not (find (aref string index) char-bag)))
       (subseq string 0 (%i+ index 1)))))

(defun string-trim (char-bag string &aux end)
  "Given a set of characters (a list or string) and a string, returns a
  copy of the string with the characters in the set removed from both
  ends."
  (setq string (string string))
  (setq end (length string))
  (let ((left-end) (right-end))
     (do ((index 0 (%i+ index 1)))
	 ((or (eq index end) (not (find (aref string index) char-bag)))
	  (setq left-end index)))
     (do ((index (%i- end 1) (%i- index 1)))
	 ((or (%i< index left-end) (not (find (aref string index) char-bag)))
	  (setq right-end index)))
      (subseq string left-end (%i+ right-end 1))))



(defun copy-symbol (symbol &optional (copy-props nil) &aux new-symbol def)
  "Make and return a new uninterned symbol with the same print name
  as SYMBOL. If COPY-PROPS is false, the new symbol is neither bound
  nor fbound and has no properties, else it has a copy of SYMBOL's
  function, value and property list."
  (setq new-symbol (make-symbol (symbol-name symbol)))
  (when copy-props
      (when (boundp symbol)
            (set new-symbol (symbol-value symbol)))
      (when (setq def (fboundp symbol))
            ;;;Shouldn't err out on macros/special forms.
            (%fhave new-symbol def))
      (set-symbol-plist new-symbol (copy-list (symbol-plist symbol))))
  new-symbol)


(defvar %gentemp-counter 0
  "Counter for generating unique GENTEMP symbols.")

(defun gentemp (&optional (prefix "T") (package *package*))
  "Creates a new symbol interned in package PACKAGE with the given PREFIX."
  (loop
    (let* ((new-pname (%str-cat (ensure-simple-string prefix) 
                                (%integer-to-string %gentemp-counter)))
           (sym (find-symbol new-pname package)))
      (if sym
        (setq %gentemp-counter (%i+ %gentemp-counter 1))
        (return (values (intern new-pname package))))))) ; 1 value.




(defun add-gc-hook (hook-function &optional (which-hook :pre-gc))
  (ecase which-hook
    (:pre-gc
     (pushnew hook-function *pre-gc-hook-list*)
     (setq *pre-gc-hook* #'(lambda ()
                             (dolist (hook *pre-gc-hook-list*)
                               (funcall hook)))))
    (:post-gc
     (pushnew hook-function *post-gc-hook-list*)
     (setq *post-gc-hook* #'(lambda ()
                             (dolist (hook *post-gc-hook-list*)
                               (funcall hook))))))
  hook-function)

(defun remove-gc-hook (hook-function &optional (which-hook :pre-gc))
  (ecase which-hook
    (:pre-gc
     (unless (setq *pre-gc-hook-list* (delq hook-function *pre-gc-hook-list*))
       (setq *pre-gc-hook* nil)))
    (:post-gc
     (unless (setq *post-gc-hook-list* (delq hook-function *post-gc-hook-list*))
       (setq *post-gc-hook* nil)))))






(defun find-method-by-names (name qualifiers specializers)
  (let ((gf (fboundp name)))
    (when gf
      (if (not (standard-generic-function-p gf))
        (error "~S is not a generic-function." gf)
        (let ((methods (%gf-methods gf)))
          (when methods
            (let* ((spec-len (length (%method-specializers (car methods))))
                   (new-specs (make-list spec-len :initial-element (find-class t))))
              (declare (dynamic-extent new-specs))
              (do ((specs specializers (cdr specs))
                   (nspecs new-specs (cdr nspecs)))
                  ((or (null specs) (null nspecs)))
                (let ((s (car specs)))
                  (rplaca nspecs (if (consp s) s (find-class s nil)))))
              (find-method gf qualifiers new-specs nil))))))))



(defun get-string-from-user (prompt)
  (with-terminal-input
      (format *query-io* "~&~a " prompt)
    (force-output *query-io*)
    (clear-input *query-io*)
    (values (read-line *query-io*))))


(defun select-item-from-list (list &key (window-title "Select one of the following")
				   (table-print-function #'prin1)
				   &allow-other-keys)
  (block get-answer
    (with-terminal-input
      (format *query-io* "~a:~%" window-title)
      (loop
	 (catch :redisplay
	   (do* ((l list (cdr l))
		 (i 0 (1+ i))
		 (item (car l) (car l)))
		((null l))
	     (declare (fixnum i))
	     (format *query-io* "~&  ~d: " i)
	     (funcall table-print-function item *query-io*))
	   (loop
	      (fresh-line *query-io*)
	      (let* ((string (get-string-from-user "Selection [number,q,r,?]:"))
		     (value (ignore-errors
			      (let* ((*package* *keyword-package*))
				(read-from-string string nil)))))
		(cond ((eq value :q) (throw :cancel t))
		      ((eq value :r) (throw :redisplay t))
		      ((eq value :?) 
		       (format *query-io* "~%Enter the number of the selection, ~%  r to redisplay, ~%  q to cancel or ~%  ? to show this message again."))
		      ((and (typep value 'unsigned-byte)
			    (< value (length list)))
		       (return-from get-answer (list (nth value list))))))))))))

;;; There should ideally be some way to override the UI (such as
;;; it is ...) here.
;;; More generally, this either
;;;   a) shouldn't exist, or
;;;   b) should do more sanity-checking
(defun choose-file-dialog (&key file-types (prompt "File name:"))
  (%choose-file-dialog t prompt file-types))

(defun choose-new-file-dialog (&key prompt)
  (%choose-file-dialog nil prompt nil))

(defun %choose-file-dialog (must-exist prompt file-types)
  (loop
      (let* ((namestring (get-string-from-user prompt))
	     (pathname (ignore-errors (pathname namestring)))
	     (exists (and pathname (probe-file pathname))))
	(when (and (if must-exist exists)
		   (or (null file-types)
		       (member (pathname-type pathname)
			       file-types :test #'equal)))
	  (return pathname))
	(if (not exists)
	  (format *query-io* "~&~s does not exist." namestring)
	  (format *query-io* "~&Type of ~s is not one of ~{~a~}"
		  namestring file-types)))))

(defparameter *overwrite-dialog-hook* nil)
(defun overwrite-dialog (filename prompt)
  (if *overwrite-dialog-hook*
    (funcall *overwrite-dialog-hook* filename prompt)
    t))

;;; Might want to have some other entry for, e.g., the inspector
;;; and to let it get its hands on the list header returned by 
;;; disassemble-ppc-function.  Maybe disassemble-ppc-function
;;; should take care of "normalizing" the code-vector ?
(defun disassemble (thing)
  "Disassemble the compiled code associated with OBJECT, which can be a
  function, a lambda expression, or a symbol with a function definition. If
  it is not already compiled, the compiler is called to produce something to
  disassemble."
  (#+ppc-target ppc-xdisassemble
   #+x8664-target x8664-xdisassemble
   (require-type (function-for-disassembly thing) 'compiled-function)))

(defun function-for-disassembly (thing)
  (let* ((fun thing))
    ;; CLHS says that DISASSEMBLE should signal a type error if its
    ;; argument isn't a function designator.  Hard to imagine any
    ;; code depending on that ...
    ;;(when (typep fun 'standard-method) (setq fun (%method-function fun)))
    (when (or (symbolp fun)
              (and (consp fun) (neq (%car fun) 'lambda)))
      (setq fun (fboundp thing))
      (when (and (symbolp thing) (not (functionp fun)))
        (setq fun (macro-function thing))))
    (if (typep fun 'compiled-lexical-closure)
        (setq fun (closure-function fun)))
    (when (lambda-expression-p fun)
      (setq fun (compile-named-function fun nil)))
    fun))

(%fhave 'df #'disassemble)

(defun svn-info-component (component)
  (let* ((component-length (length component)))
  (with-output-to-string (s)
    (multiple-value-bind (status exit-code)
        (external-process-status
         (run-program "svn"  (list "info" (native-translated-namestring "ccl:")):output s))
      (when (and (eq :exited status) (zerop exit-code))
        (with-input-from-string (output (get-output-stream-string s))
            (do* ((line (read-line output nil nil) (read-line output nil nil)))
                 ((null line))
              (when (and (>= (length line) component-length)
                         (string= component line :end2 component-length))
                (return-from svn-info-component
                  (string-trim " " (subseq line component-length)))))))))))

(defun svn-url () (svn-info-component "URL:"))
(defun svn-repository () (svn-info-component "Repository Root:"))

;;; Try to say something about what tree (trunk, a branch, a release)
;;; we were built from. If the URL (relative to the repository)
;;; starts with "branches", return the second component of the
;;; relative URL, otherwise return the first component.
(defun svn-tree ()
  (let* ((repo (svn-repository))
         (url (svn-url)))
    (or 
     (if (and repo url)
       (let* ((repo-len (length repo)))
         (when (and (> (length url) repo-len)
                    (string= repo url :end2 repo-len))
           ;; Cheat: do pathname parsing here.
           (let* ((path (pathname (ensure-directory-namestring (subseq url repo-len))))
                  (dir (cdr (pathname-directory path))))
             (when (string= "ccl" (car (last dir)))
               (let* ((base (car dir)))
                 (unless (or (string= base "release")
                             (string= base "releases"))
                   (if (string= base "branches")
                     (cadr dir)
                     (car dir))))))))))))



        
                         
(defun local-svn-revision ()
  (or
   ;; svn2cvs uses a .svnrev file to sync CVS and SVN; if present,
   ;; it contains the svn revision in decimal.
   (with-open-file (f "ccl:\\.svnrev" :direction :input :if-does-not-exist nil)
     (when f (read f)))
   (with-output-to-string (s)
    (multiple-value-bind (status exit-code)
        (external-process-status
         (run-program "svnversion"  (list  (native-translated-namestring "ccl:") (or (svn-url) "")):output s))
      (when (and (eq :exited status) (zerop exit-code))
        (with-input-from-string (output (get-output-stream-string s))
          (let* ((line (read-line output nil nil)))
            (when (and line (parse-integer line :junk-allowed t) )
              (return-from local-svn-revision line)))))))))


;;; Scan the heap, collecting infomation on the primitive object types
;;; found.  Report that information.

(defun heap-utilization (&key (stream *debug-io*)
                              (gc-first t))
  (let* ((nconses 0)
         (nvectors (make-array 256))
         (vector-sizes (make-array 256))
         (array-size-function (arch::target-array-data-size-function
                               (backend-target-arch *host-backend*))))
    (declare (type (simple-vector 256) nvectors vector-sizes)
             (dynamic-extent nvectors vector-sizes))
    (when gc-first (gc))
    (%map-areas (lambda (thing)
                  (if (consp thing)
                    (incf nconses)
                    (let* ((typecode (typecode thing)))
                      (incf (aref nvectors typecode))
                      (incf (aref vector-sizes typecode)
                            (funcall array-size-function typecode (uvsize thing)))))))
    (report-heap-utilization stream nconses nvectors vector-sizes)
    (values)))

(defvar *heap-utilization-vector-type-names*
  (let* ((a (make-array 256)))
    #+x8664-target
    (dotimes (i 256)
      (let* ((fulltag (logand i x8664::fulltagmask))
             (names-vector
              (cond ((= fulltag x8664::fulltag-nodeheader-0)
                     *nodeheader-0-types*)
                    ((= fulltag x8664::fulltag-nodeheader-1)
                     *nodeheader-1-types*)
                    ((= fulltag x8664::fulltag-immheader-0)
                     *immheader-0-types*)
                    ((= fulltag x8664::fulltag-immheader-1)
                     *immheader-1-types*)
                    ((= fulltag x8664::fulltag-immheader-2)
                     *immheader-2-types*)))
             (name (if names-vector
                     (aref names-vector (ash i -4)))))
        ;; Special-case a few things ...
        (if (eq name 'symbol-vector)
          (setq name 'symbol)
          (if (eq name 'function-vector)
            (setq name 'function)))
        (setf (aref a i) name)))
    #+ppc64-target
    (dotimes (i 256)
      (let* ((lowtag (logand i ppc64::lowtagmask)))
        (setf (%svref a i)
              (cond ((= lowtag ppc64::lowtag-immheader)
                     (%svref *immheader-types* (ash i -2)))
                    ((= lowtag ppc64::lowtag-nodeheader)
                     (%svref *nodeheader-types* (ash i -2)))))))
    #+ppc32-target
    (dotimes (i 256)
      (let* ((fulltag (logand i ppc32::fulltagmask)))
        (setf (%svref a i)
              (cond ((= fulltag ppc32::fulltag-immheader)
                     (%svref *immheader-types* (ash i -3)))
                    ((= fulltag ppc32::fulltag-nodeheader)
                     (%svref *nodeheader-types* (ash i -3)))))))
    a))

  
    
(defun report-heap-utilization (out nconses nvectors vector-sizes)
  (format out "~&Object type~42tCount~50tTotal Size in Bytes")
  (format out "~&CONS~36t~12d~48t~16d" nconses (* nconses target::cons.size))
  (dotimes (i (length nvectors))
    (let* ((count (aref nvectors i))
           (sizes (aref vector-sizes i)))
      (unless (zerop count)
        (format out "~&~a~36t~12d~48t~16d" (aref *heap-utilization-vector-type-names* i)  count sizes)))))
                            
;; The number of words to allocate for static conses when the user requests
;; one and we don't have any left over
(defparameter *static-cons-chunk* 1048576)

(defun initialize-static-cons ()
  "Activates collection of garbage conses in the static-conses
   list and allocates initial static conses."
  ; There might be a race here when multiple threads call this
  ; function.  However, the discarded static conses will become
  ; garbage and be added right back to the list.  No harm here
  ; except for additional garbage collections.
  (%set-kernel-global 'static-conses nil)
  (allocate-static-conses))

(defun allocate-static-conses ()
  "Allocates some memory, freezes it and lets it become garbage.
   This will add the memory to the list of free static conses."
  (let ((l (make-array *static-cons-chunk*)))
    (declare (ignore l))
    (freeze))
  (gc))

(defun static-cons (car-value cdr-value)
  "Allocates a cons cell that doesn't move on garbage collection,
   and thus doesn't trigger re-hashing when used as a key in a hash
   table.  Usage is equivalent to regular CONS."
  (when (eq (%get-kernel-global 'static-conses) 0)
    (initialize-static-cons))
  (let ((cell (%atomic-pop-static-cons)))
    (if cell
      (progn
	(setf (car cell) car-value)
	(setf (cdr cell) cdr-value)
	cell)
      (progn
	(allocate-static-conses)
	(static-cons car-value cdr-value)))))
	

(defparameter *weak-gc-method-names*
  '((:traditional . 0)
    (:non-circular . 1)))


(defun weak-gc-method ()
  (or (car (rassoc (%get-kernel-global 'weak-gc-method)
                   *weak-gc-method-names*))
      :traditional))


(defun (setf weak-gc-method) (name)
  (setf (%get-kernel-global 'weak-gc-method)
        (or (cdr (assoc name *weak-gc-method-names*))
            0))
  name)