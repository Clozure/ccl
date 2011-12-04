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
  #-windows-target (%uname 1)
  #+windows-target
  (rlet ((nsize #>DWORD 0))
    (if (eql 0 (#_GetComputerNameExW #$ComputerNameDnsFullyQualified
                                     (%null-ptr)
                                     nsize))
      (%stack-block ((buf (* 2 (pref nsize #>DWORD))))
        (#_GetComputerNameExW #$ComputerNameDnsFullyQualified
                              buf
                              nsize)
        (%get-native-utf-16-cstring buf))
      "localhost"))
  )


(defun machine-type ()
  "Returns a string describing the type of the local machine."
  #-windows-target (%uname 4)
  #+windows-target
  (rlet ((info #>SYSTEM_INFO))
    (#_GetSystemInfo info)
    (case (pref info #>SYSTEM_INFO.nil.nil.wProcessorArchitecture)
      (#.#$PROCESSOR_ARCHITECTURE_AMD64 "x64")
      (#.#$PROCESSOR_ARCHITECTURE_INTEL "x86")
      (t "unknown")))
  )



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
                                #+x86-target "model name"
                                #+arm-target "Hardware"))
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
            #+solaris-target
            (rlet ((info :processor_info_t))
              (do* ((i 0 (1+ i)))
                   ((and (= 0 (#_processor_info i info))
                         (= (pref info :processor_info_t.pi_state)
                            #$P_ONLINE))
                    (%get-cstring (pref info :processor_info_t.pi_processor_type)))))
            #+windows-target
            (getenv "PROCESSOR_IDENTIFIER")
            )))


(defun software-type ()
  "Return a string describing the supporting software."
  #-windows-target (%uname 0)
  #+windows-target "Microsoft Windows")


(defun software-version ()
  "Return a string describing version of the supporting software, or NIL
   if not available."
  #-windows-target (%uname 2)
  #+windows-target
  (rletZ ((info #>OSVERSIONINFOEX))
    (setf (pref info #>OSVERSIONINFOEX.dwOSVersionInfoSize)
          (record-length #>OSVERSIONINFOEX))
    (#_GetVersionExA info)
    (format nil "~d.~d Build ~d (~a)"
            (pref info #>OSVERSIONINFOEX.dwMajorVersion)
            (pref info #>OSVERSIONINFOEX.dwMinorVersion)
            (pref info #>OSVERSIONINFOEX.dwBuildNumber)
            (if (eql (pref info #>OSVERSIONINFOEX.wProductType)
                     #$VER_NT_WORKSTATION)
              "Workstation"
              "Server")))
  )







;;; Yawn.



(defmethod documentation (thing doc-id)
  (%get-documentation thing doc-id))

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

(defmethod documentation ((slot slot-definition) (doc-type t))
   (when (or (eq doc-type t)(eq doc-type 'slot-definition))
     (slot-definition-documentation slot)))

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


(defun %page-fault-info ()
  #-(or darwin-target windows-target)
  (rlet ((usage :rusage))
    (%%rusage usage)
    (values (pref usage :rusage.ru_minflt)
            (pref usage :rusage.ru_majflt)
            (pref usage :rusage.ru_nswap)))
  #+darwin-target
  (rlet ((count #>mach_msg_type_number_t #$TASK_EVENTS_INFO_COUNT)
         (info #>task_events_info))
    (#_task_info (#_mach_task_self) #$TASK_EVENTS_INFO info count)
    (let* ((faults (pref info #>task_events_info.faults))
           (pageins (pref info #>task_events_info.pageins)))
      (values (- faults pageins)
              pageins
              0)))
  #+windows-target
  ;; Um, don't know how to determine this, or anything like it.
  (values 0 0 0))


          
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
         (iwidth (max (length (format nil "~:D" elapsed-time))
                          (length (format nil "~:D" user-time))
                          (length (format nil "~:D" system-time))
                          (length (format nil "~:D" gc-time))))
                      
         (fwidth
          (ecase internal-time-units-per-second
            (1000000 6)
            (1000  3)))
         (elapsed-seconds (/ elapsed-time internal-time-units-per-second))
         (user-seconds (/ user-time internal-time-units-per-second))
         (system-seconds (/ system-time internal-time-units-per-second))
         (gc-seconds  (/ gc-time internal-time-units-per-second))
         (ffield-width (max (length (format nil "~,vF" fwidth elapsed-seconds))
                                (length (format nil "~,vF" fwidth user-seconds))
                                (length (format nil "~,vF" fwidth system-seconds))
                                (length (format nil "~,vF" fwidth gc-seconds))))
         (cpu-count (cpu-count)))
    (format s "~&~S" form)
    (format s "~&took ~v:D ~a (~v,vF seconds) to run." iwidth elapsed-time units ffield-width fwidth elapsed-seconds )
    (unless (eql gc-time 0)
      (format s
              "~%~5t~v:D ~a (~v,vF seconds, ~,2f%) of which was spent in GC." iwidth
              gc-time units ffield-width fwidth gc-seconds (* 100.0 (/ gc-seconds elapsed-seconds))))
    (format s "~&During that period, and with ~D available CPU core~P," cpu-count cpu-count)
    (format s "~&~5t~v:D ~a (~v,vF seconds) were spent in user mode" iwidth user-time units ffield-width fwidth user-seconds)
    (format s "~&~5t~v:D ~a (~v,vF seconds) were spent in system mode" iwidth system-time units ffield-width fwidth system-seconds)
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
    (multiple-value-bind (user-start system-start)
        (%internal-run-time)
      (multiple-value-bind (minor-start major-start swaps-start)
          (%page-fault-info)
        (let* ((initial-real-time (get-internal-real-time))
               (initial-gc-time (gctime))
               (initial-consed (total-bytes-allocated))           
               (initial-overhead (integer-size-in-bytes initial-consed)))
          (let* ((results (multiple-value-list (funcall thunk))))
            (declare (dynamic-extent results))
            (multiple-value-bind (user-end system-end)
                (%internal-run-time)
              (multiple-value-bind (minor-end major-end swaps-end)
                  (%page-fault-info)
                (let* ((new-consed (total-bytes-allocated))		     
                       (bytes-consed
                        (- new-consed (+ initial-overhead initial-consed)))
                       (elapsed-real-time
                        (- (get-internal-real-time) initial-real-time))
                       (elapsed-gc-time (- (gctime) initial-gc-time))
                       (elapsed-user-time
                        (- user-end user-start))
                       (elapsed-system-time
                        (- system-end system-start))
                       (elapsed-minor (- minor-end minor-start))
                       (elapsed-major (- major-end major-start))
                       (elapsed-swaps (- swaps-end swaps-start)))
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
                           :swaps elapsed-swaps))))))))))





(defun add-feature (thing)
  (when (typep thing 'symbol)
    (let* ((gvector-or-fixnum (%symptr-binding-address '*features*)))
      (if (typep gvector-or-fixnum 'fixnum)
        ;; Thread-local binding of *FEATURES*.
        (if (not (member thing *features* :test #'eq))
          (setq *features* (cons thing *features*)))
        (loop
          (let* ((old (%svref gvector-or-fixnum target::symbol.vcell-cell)))
            (when (member thing old :test #'eq)
              (return))
            (let* ((new (cons thing old)))
              (when (store-gvector-conditional target::symbol.vcell-cell
                                               gvector-or-fixnum
                                               old
                                               new)
                (return)))))))
    thing))

(defun remove-feature (thing)
  (let* ((gvector-or-fixnum (%symptr-binding-address '*features*)))
    (if (typep gvector-or-fixnum 'fixnum)
      ;; Thread-local binding of *FEATURES*.
      (setq *features* (delete thing *features*))
      (loop
        (let* ((old (%svref gvector-or-fixnum target::symbol.vcell-cell)))
          (unless (member thing old :test #'eq)
            (return))
          (let* ((new (remove thing old)))
            (when (store-gvector-conditional target::symbol.vcell-cell
                                           gvector-or-fixnum
                                           old
                                           new)
              (return))))))
    thing))
  





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




(defun make-population (&key (type :list) initial-contents)
  (let* ((ntype (ecase type
                  (:list $population_weak-list)
                  (:alist $population_weak-alist)))
         (list (if (eq type :alist)
                 (map 'list (lambda (c) (cons (car c) (%cdr c))) initial-contents)
                 (if (listp initial-contents)
                   (copy-list initial-contents)
                   (coerce initial-contents 'list)))))
    (%cons-population list ntype)))

(defun population-type (population)
  (let ((ntype (population.type (require-type population 'population))))
    (cond ((eq ntype $population_weak-alist) :alist)
          ((eq ntype $population_weak-list) :list)
          (t nil))))

(declaim (inline population-contents (setf population-contents)))

(defun population-contents (population)
  (population.data (require-type population 'population)))

(defun (setf population-contents) (list population)
  (setf (population.data (require-type population 'population)) (require-type list 'list)))




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

(defvar *choose-file-dialog-hook* nil "for GUIs")

;;; There should ideally be some way to override the UI (such as
;;; it is ...) here.
;;; More generally, this either
;;;   a) shouldn't exist, or
;;;   b) should do more sanity-checking
(defun choose-file-dialog (&key file-types (prompt "File name:"))
  (let* ((hook *choose-file-dialog-hook*))
    (if hook
      (funcall hook t prompt file-types)
      (%choose-file-dialog t prompt file-types))))

(defun choose-new-file-dialog (&key prompt)
  (let* ((hook *choose-file-dialog-hook*))
    (if hook
      (funcall hook nil prompt nil)
      (%choose-file-dialog nil prompt nil))))

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

(defparameter *disassemble-verbose* nil)

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
   #+x86-target x86-xdisassemble
   #+arm-target arm-xdisassemble
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
      (setq fun (compile-named-function fun)))
    fun))

(%fhave 'df #'disassemble)

(defun string-sans-most-whitespace (string &optional (max-length (length string)))
  (with-output-to-string (sans-whitespace)
    (loop
      for count below max-length
      for char across string
      with just-saw-space = nil
      if (member char '(#\Space #\Tab #\Newline #\Return #\Formfeed))
        do (if just-saw-space
               (decf count)
               (write-char #\Space sans-whitespace))
        and do (setf just-saw-space t)
      else
        do (setf just-saw-space nil)
        and do (write-char char sans-whitespace))))


(defparameter *svn-program* "svn")

(defloadvar *use-cygwin-svn*
    #+windows-target (not (null (getenv "CYGWIN")))
    #-windows-target nil)

(defun run-svn (args &key (output :string) (error :output) (if-fail :error ifp))
  (if (eq output :stream)
    (external-process-output-stream (run-program *svn-program* args :output :stream :error error :wait nil))
    (flet ((check-status (proc)
             (multiple-value-bind (status exit-code) (external-process-status proc)
               (unless (and (eq status :exited) (or (not ifp) (zerop exit-code)))
                 (if (eq if-fail :error)
                   (error "Running \"svn ~a\" produced exit status ~s, code ~s" (car args) status exit-code)
                   (return-from run-svn if-fail))))
             proc))
      (if (eq output :string)
        (with-output-to-string (stream)
          (check-status (run-program *svn-program* args :output stream :error error)))
        (check-status (run-program *svn-program* args :output output :error error))))))

(defun svn-info-component (component)
  (let ((component-length (length component))
        (string (run-svn (list "info" (native-translated-namestring "ccl:")) :if-fail nil)))
    (when string
      (with-input-from-string (output string)
        (do* ((line (read-line output nil nil) (read-line output nil nil)))
             ((null line))
          (when (and (>= (length line) component-length)
                     (string= component line :end2 component-length))
            (return-from svn-info-component
              (string-trim " " (subseq line component-length))))))
      nil)))

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


(defun svnversion-program ()
  (or (ignore-errors
        (native-translated-namestring
         (merge-pathnames "svnversion" *svn-program*)))
      "svnversion"))
        
                      
        
                         
(defun local-svn-revision ()
  (let* ((s (make-string-output-stream))
         (root (native-translated-namestring "ccl:")))
    (when *use-cygwin-svn*
      (setq root (cygpath root)))
    (multiple-value-bind (status exit-code)
        (external-process-status
         (run-program (svnversion-program)  (list  (native-translated-namestring "ccl:") (or (svn-url) "")) :output s :error :output))
      (when (and (eq :exited status) (zerop exit-code))
        (with-input-from-string (output (get-output-stream-string s))
          (let* ((line (read-line output nil nil)))
            (when (and line (parse-integer line :junk-allowed t) )
              (return-from local-svn-revision line))))))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; svn diffs

(defun get-svn-changes (&key (directory (current-directory)) (revision :base) (reverse nil))
  "Run svn diff to compare REVISION and working copy of DIRECTORY, and return a list of
  the changed regions (in the form of source notes) in the working copy.  If REVERSE is true,
  returns regions in the REVISION version rather than the working copy."
  (let* ((svn-revision (format nil "-r~a" revision))
         (diff (run-svn `("diff" ,svn-revision ,(native-translated-namestring directory)))))
    (unless (equal diff "")
      (unless (string-equal "Index: " diff :end2 7)
        (error "Cannot understand svn output: ~s" diff))
      (parse-svn-changes diff directory (if reverse svn-revision)))))

(defun parse-svn-changes (string directory svn-revision)
  ;; Parse svn diff output string into source-note's
  (unless (equal string "")
    (assert (string-equal "Index: " string :end2 7))
    (loop
      for pos = 7 then (+ end 8)
      as file = (subseq string pos (setq pos (position #\newline string :start pos)))
      as pathname = (merge-pathnames file directory)
      as end = (search #.(%str-cat (string #\newline) "Index: ") string :start2 pos)
      nconc (parse-svn-changes-in-file string pos end pathname svn-revision)
      while end)))

(defun parse-svn-changes-in-file (string pos end pathname svn-revision)
  (let* ((line-ranges (parse-svn-changed-lines-in-file string (1+ pos) (or end (length string)) svn-revision))
         (lines (loop for (start-line . line-count) in line-ranges
                  collect start-line
                  collect (+ start-line line-count)))
         ;; Convert line ranges to character ranges.
         (line-posns (flet ((posns (stream)
                              (flet ((skip-lines (stream count)
                                       (let ((chars 0))
                                         (loop while (> count 0)
                                           do (let ((ch (read-char stream)))
                                                (loop until (or (eql ch #\newline) (null ch))
                                                  do (incf chars)
                                                  do (setq ch (read-char stream nil)))
                                                (when ch (incf chars))
                                                (decf count)))
                                         chars)))
                                (loop
                                  for last-line = 1 then line-no
                                  for last-pos = 0 then pos
                                  for line-no in (remove-duplicates (sort lines #'<))
                                  for pos = (+ last-pos (skip-lines stream (- line-no last-line)))
                                  collect (cons line-no pos)))))
                       (if svn-revision
                         (let ((stream (run-svn `("cat"
                                                  ,svn-revision
                                                  ,(native-translated-namestring pathname))
                                                :output :stream)))
                           (posns stream))
                         (with-open-file (stream pathname) (posns stream))))))
    (loop for (start-line . line-count) in line-ranges
      collect (make-source-note :filename pathname
                                :start-pos (cdr (assq start-line line-posns))
                                :end-pos (cdr (assq (+ start-line line-count) line-posns))))))


(defun parse-svn-changed-lines-in-file (string start end svn-revision)
  (flet ((next-line (str start end)
           (let ((pos (position #\Newline str :start start :end end)))
             (if pos (1+ pos) end))))
    (unless (eql start end)
      (assert 
       (let ((pos start))
         (and (loop repeat 67 always (eql (char string pos) #\=) do (incf pos))
              (eql (char string pos) #\Newline)
              (string-equal "--- " string :start2 (incf pos) :end2 (+ pos 4))
              (setq pos (position #\newline string :start pos))
              (string-equal "+++ " string :start2 (incf pos) :end2 (+ pos 4))
              (< pos end)
              (or (null (setq pos (position #\newline string :start pos :end end)))
                  (string-equal "@@ -" string :start2 (1+ pos) :end2 (+ pos 5))))))
      (when (setq start (search #.(%str-cat (string #\newline) "@@ -") string :start2 start :end2 end))
        (incf start)
        (loop
          do (incf start 4)
          collect (multiple-value-bind (start-line npos)
                                       (parse-integer string
                                                      :start (if svn-revision
                                                               start
                                                               (1+ (position #\+ string :start start :end end)))
                                                      :end end
                                                      :junk-allowed t)
                    (assert (eql (char string npos) #\,))
                    (multiple-value-bind (num-lines npos) (parse-integer string :start (1+ npos) :end end
                                                                         :junk-allowed t)
                      (assert (eql (char string npos) #\space))
                      ;; adjust for context lines
                      (loop with first = t
                        as ch = (and (< (setq npos (next-line string npos end)) end)
                                     (char string npos))
                        while (memq ch '(#\space #\+ #\-))
                        do (cond ((eq ch #\space)
                                  (decf num-lines)
                                  (when first (incf start-line)))
                                 (t (setq first nil)))
                        finally (setq start npos))
                      (cons start-line num-lines)))
          while (and (< (+ start 4) end) (string-equal "@@ -" string :start2 start :end2 (+ start 4)))
          finally (assert (eql start end)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Scan the heap, collecting infomation on the primitive object types
;;; found.  Report that information.

(defun heap-utilization (&key (stream *debug-io*)
                              (gc-first t)
                              (area nil)
                              (unit nil)
                              (sort :size)
                              (classes nil)
                              (start nil)
                              (threshold (and classes 0.00005)))
  "Show statistics about types of objects in the heap.
   If :GC-FIRST is true (the default), do a full gc before scanning the heap.
   If :START is non-nil, it should be an object returned by GET-ALLOCATION-SENTINEL, only
     objects at higher address are scanned (i.e. roughly, only objects allocated after it).
   :SORT can be one of :COUNT, :LOGICAL-SIZE, or :PHYSICAL-SIZE to sort by count or size.
   :UNIT can be one of :KB :MB or :GB to show sizes in units other than bytes.
   :AREA can be used to restrict the walk to one area or a list of areas.  Some possible
   values are :DYNAMIC, :STATIC, :MANAGED-STATIC, :READONLY.  By default, all areas
   (including stacks) are examined.
   If :CLASSES is true, classifies by class rather than just typecode"
  (let ((data (collect-heap-utilization :gc-first gc-first :start start :area area :classes classes)))
    (report-heap-utilization data :stream stream :unit unit :sort sort :threshold threshold)))

(defun collect-heap-utilization (&key (gc-first t) start area classes)
  ;; returns list of (type-name count logical-sizes-total physical-sizes-total)
  (when start
    (unless (or (null area)
                (eq (heap-area-code area) area-dynamic)
                (and (consp area) (every (lambda (a) (eq (heap-area-code a) area-dynamic)) area)))
      (error "~s ~s and ~s ~s are incompatible" :start start :area area))
    (setq area area-dynamic))
  (if classes
    (collect-heap-utilization-by-class gc-first area start)
    (collect-heap-utilization-by-typecode gc-first area start)))

(defun collect-heap-utilization-by-typecode (gc-first area start)
  (let* ((nconses 0)
         (counts (make-array 257))
         (sizes (make-array 257))
         (physical-sizes (make-array 257))
         (array-size-function (arch::target-array-data-size-function
                               (backend-target-arch *host-backend*))))
    (declare (type (simple-vector 257) counts sizes physical-sizes)
             (fixnum nconses)
             (dynamic-extent counts sizes physical-sizes))
    (flet ((collect (thing)
             (when (or (null start)
                       (locally (declare (optimize (speed 3) (safety 0))) ;; lie
                         (%i< start thing)))
               (if (listp thing)
                 (incf nconses)
                 (let* ((typecode (typecode thing))
                        (logsize (funcall array-size-function typecode (uvsize thing)))
                        (physize (logandc2 (+ logsize
                                              #+64-bit-target (+ 8 15)
                                              #+32-bit-target (+ 4 7))
                                           #+64-bit-target 15
                                           #+32-bit-target 7)))
                   (incf (aref counts typecode))
                   (incf (aref sizes typecode) logsize)
                   (incf (aref physical-sizes typecode) physize))))))
      (declare (dynamic-extent #'collect))
      (when gc-first (gc))
      (%map-areas #'collect area))
    (setf (aref counts 256) nconses)
    (setf (aref sizes 256) (* nconses target::cons.size))
    (setf (aref physical-sizes 256) (aref sizes 256))
    (loop for i from 0 upto 256
      when (plusp (aref counts i))
      collect (list (if (eql i 256) 'cons (aref *heap-utilization-vector-type-names* i))
                    (aref counts i)
                    (aref sizes i)
                    (aref physical-sizes i)))))

(defun collect-heap-utilization-by-class (gc-first area start)
  (let* ((nconses 0)
         (max-classes (+ 100 (hash-table-count %find-classes%)))
         (map (make-hash-table :shared nil
                               :test 'eq
                               :size max-classes))
         (inst-counts (make-array max-classes :initial-element 0))
         (slotv-counts (make-array max-classes :initial-element 0))
         (inst-sizes (make-array max-classes :initial-element 0))
         (slotv-sizes (make-array max-classes :initial-element 0))
         (inst-psizes (make-array max-classes :initial-element 0))
         (slotv-psizes (make-array max-classes :initial-element 0))
         (overflow nil)
         (array-size-function (arch::target-array-data-size-function
                               (backend-target-arch *host-backend*))))
    (declare (type simple-vector inst-counts slotv-counts inst-sizes slotv-sizes inst-psizes slotv-psizes))
    (flet ((collect (thing)
             (when (or (null start)
                       (locally (declare (optimize (speed 3) (safety 0))) ;; lie
                         (%i< start thing)))
               (if (listp thing)
                 (incf nconses)
                 (unless (or (eq thing map)
                             (eq thing (nhash.vector map))
                             (eq thing inst-counts)
                             (eq thing slotv-counts)
                             (eq thing inst-sizes)
                             (eq thing slotv-sizes)
                             (eq thing inst-psizes)
                             (eq thing slotv-psizes))
                   (let* ((typecode (typecode thing))
                          (logsize (funcall array-size-function typecode (uvsize thing)))
                          (physize (logandc2 (+ logsize
                                                #+64-bit-target (+ 8 15)
                                                #+32-bit-target (+ 4 7))
                                             #+64-bit-target 15
                                             #+32-bit-target 7))
                          (class (class-of (if (eql typecode target::subtag-slot-vector)
                                             (uvref thing slot-vector.instance)
                                             (if (eql typecode target::subtag-function)
                                               (function-vector-to-function thing)
                                               thing))))
                          (index (or (gethash class map)
                                     (let ((count (hash-table-count map)))
                                       (if (eql count max-classes)
                                         (setq overflow t count (1- max-classes))
                                         (setf (gethash class map) count))))))
                   
                     (if (eql typecode target::subtag-slot-vector)
                       (progn
                         (incf (aref slotv-counts index))
                         (incf (aref slotv-sizes index) logsize)
                         (incf (aref slotv-psizes index) physize))
                       (progn
                         (incf (aref inst-counts index))
                         (incf (aref inst-sizes index) logsize)
                         (incf (aref inst-psizes index) physize)))))))))
      (declare (dynamic-extent #'collect))
      (when gc-first (gc))
      (%map-areas #'collect area))
    (let ((data ()))
      (when (plusp nconses)
        (push (list 'cons nconses (* nconses target::cons.size) (* nconses target::cons.size)) data))
      (maphash (lambda (class index)
                 (let* ((icount (aref inst-counts index))
                        (scount (aref slotv-counts index))
                        (name (if (and overflow (eql index (1- max-classes)))
                                "All others"
                                (or (%class-proper-name class) class))))
                   (declare (fixnum icount) (fixnum scount))
                   ;; When printing class names, the package matters.  report-heap-utilization
                   ;; uses ~a, so print here.
                   (when (plusp icount)
                     (push (list (prin1-to-string name)
                                 icount (aref inst-sizes index) (aref inst-psizes index)) data))
                   (when (plusp scount)
                     (push (list (format nil "~s slot vector" name)
                                 scount (aref slotv-sizes index) (aref slotv-psizes index)) data))))
               map)
      data)))

(defun collect-heap-ivector-utilization-by-typecode ()
  (let* ((counts (make-array 256 :initial-element 0))
	 (sizes (make-array 256 :initial-element 0))
	 (physical-sizes (make-array 256 :initial-element 0))
	 (array-size-function (arch::target-array-data-size-function
                               (backend-target-arch *host-backend*)))
	 (result ()))
    (declare (dynamic-extent counts sizes))
    (with-lock-grabbed (*heap-ivector-lock*)
      (dolist (vector *heap-ivectors*)
	(let* ((typecode (typecode vector))
	       (logsize (funcall array-size-function typecode (uvsize vector)))
	       (physsize (+ logsize
			    ;; header, delta, round up
			    #+32-bit-target (+ 4 2 7)
			    #+64-bit-target (+ 8 2 15))))
	  (incf (aref counts typecode))
	  (incf (aref sizes typecode) logsize)
	  (incf (aref physical-sizes typecode) physsize))))
    (dotimes (i 256 result)
      (when (plusp (aref counts i))
	(push (list (aref *heap-utilization-vector-type-names* i)
		    (aref counts i)
		    (aref sizes i)
		    (aref physical-sizes i))
	      result)))))

(defun heap-ivector-utilization (&key (stream *debug-io*)
				      (unit nil)
				      (sort :size))
  (let* ((data (collect-heap-ivector-utilization-by-typecode)))
    (report-heap-utilization data :stream stream :unit unit :sort sort)))
  
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
    #+(or ppc32-target x8632-target arm-target)
    (dotimes (i 256)
      (let* ((fulltag (logand i target::fulltagmask)))
        (setf (%svref a i)
              (cond ((= fulltag target::fulltag-immheader)
                     (%svref *immheader-types* (ash i -3)))
                    ((= fulltag target::fulltag-nodeheader)
                     (%svref *nodeheader-types* (ash i -3)))))))
    a))

  
(defun report-heap-utilization (data &key stream unit sort threshold)
  (check-type threshold (or null (real 0 1)))
  (let* ((div (ecase unit
                ((nil) 1)
                (:kb 1024.0d0)
                (:mb (* 1024.0d0 1024.0d0))
                (:gb (* 1024.0d0 1024.0d0 1024.0d0))))
         (sort-key (ecase sort
                     (:count #'cadr)
                     (:logical-size #'caddr)
                     ((:physical-size :size) #'cadddr)
                     ((:name nil) nil)))
         (total-count 0)
         (total-lsize 0)
         (total-psize 0)
         (max-name 0)
         (others (list "All others" 0 0 0)))

    (when (hash-table-p data)
      (setq data
            (let ((alist nil))
              (maphash (lambda (type measures) (push (cons type measures) alist)) data)
              alist)))

    (flet ((type-string (name)
             (if (stringp name)
               name
               (if (symbolp name)
                 (symbol-name name)
                 (princ-to-string name)))))
      (loop for (nil count lsize psize) in data
            do (incf total-count count)
            do (incf total-lsize lsize)
            do (incf total-psize psize))

      (when (and data threshold)
        (setq data (sort data #'< :key #'cadddr))
        (loop while (< (/ (cadddr (car data)) total-psize) threshold)
              do (destructuring-bind (type count lsize psize) (pop data)
                   (declare (ignore type))
                   (incf (cadr others) count)
                   (incf (caddr others) lsize)
                   (incf (cadddr others) psize))))

      (setq data
            (if sort-key
              (sort data #'> :key sort-key)
              (sort data #'string-lessp :key #'(lambda (s) (type-string (car s))))))

      (when (> (cadr others) 0)
        (setq data (nconc data (list others))))

      (setq max-name (loop for (name) in data maximize (length (type-string name))))

      (format stream "~&Object type~vtCount     Logical size   Physical size   % of Heap~%~vt ~a~vt ~2:*~a"
              (+ max-name 7)
              (+ max-name 15)
              (ecase unit
                ((nil) "  (in bytes)")
                (:kb   "(in kilobytes)")
                (:mb   "(in megabytes)")
                (:gb   "(in gigabytes)"))
              (+ max-name 31))
      (loop for (type count logsize physsize) in data
            do (if unit
                 (format stream "~&~a~vt~11d~16,2f~16,2f~11,2f%"
                         (type-string type)
                         (1+ max-name)
                         count
                         (/ logsize div)
                         (/ physsize div)
                         (* 100.0 (/ physsize total-psize)))
                 (format stream "~&~a~vt~11d~16d~16d~11,2f%"
                         (type-string type)
                         (1+ max-name)
                         count
                         logsize
                         physsize
                         (* 100.0 (/ physsize total-psize)))))
      (if unit
        (format stream "~&~a~vt~11d~16,2f~16,2f~11,2f%~%"
                "Total"
                (1+ max-name)
                total-count
                (/ total-lsize div)
                (/ total-psize div)
                100.0d0)
        (format stream "~&~a~vt~11d~16d~16d~11,2f%~%"
                "Total"
                (1+ max-name)
                total-count
                total-lsize
                total-psize
                100.0d0))))
  (values))

(defun object-direct-size (thing)
  "Returns the size of THING (in bytes), including any headers and
   alignment overhead.  Does not descend an object's components."
  (cond ((consp thing) #+64-bit-target 16 #+32-bit-target 8)
        #+x8664-target ((symbolp thing)
                        (object-direct-size (%symptr->symvector thing)))
        #+x8664-target ((functionp thing)
                        (object-direct-size (function-to-function-vector thing)))
        ((uvectorp thing)
         (let* ((typecode (ccl::typecode thing))
                (element-count (ccl::uvsize thing))
                (sizeof-content-in-octets
                 ;; Call the architecture-specific backend function.
                 (funcall (arch::target-array-data-size-function
                           (backend-target-arch *host-backend*))
                          typecode element-count)))
           (logandc2 (+ sizeof-content-in-octets
                           #+64-bit-target (+ 8 15)
                           #+32-bit-target (+ 4 7))
                     #+64-bit-target 15
                     #+32-bit-target 7)))
        (t 0)))

(defun static-cons (car-value cdr-value)
  "Allocates a cons cell that doesn't move on garbage collection,
   and thus doesn't trigger re-hashing when used as a key in a hash
   table.  Usage is equivalent to regular CONS."
  (loop
    (let ((cell (without-interrupts (%atomic-pop-static-cons))))
      (if cell
        (progn
          (setf (car cell) car-value)
          (setf (cdr cell) cdr-value)
          (return cell))
        (progn
          (%ensure-static-conses))))))

(defun free-static-conses ()
  (%get-kernel-global free-static-conses))

(defun reserved-static-conses ()
  (%fixnum-ref-natural (%get-kernel-global static-cons-area) target::area.ndnodes))
	

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

(defun %lock-whostate-string (string lock)
  (with-standard-io-syntax
      (format nil "~a for ~a ~@[~a ~]@ #x~x"
              string
              (%svref lock target::lock.kind-cell)
              (lock-name lock)
              (%ptr-to-int (%svref lock target::lock._value-cell)))))

(defun all-watched-objects ()
  (let (result)
    (with-other-threads-suspended
      (%map-areas #'(lambda (x) (push x result)) area-watched))
    result))

(defun primitive-watch (thing)
  (require-type thing '(or cons (satisfies uvectorp)))
  (%watch thing))

(defun watch (&optional thing)
  (cond ((null thing)
	 (all-watched-objects))
	((arrayp thing)
	 (primitive-watch (array-data-and-offset thing)))
	((hash-table-p thing)
	 (primitive-watch (nhash.vector thing)))
	((standard-instance-p thing)
	 (primitive-watch (instance-slots thing)))
	(t
	 (primitive-watch thing))))

(defun unwatch (thing)
  (with-other-threads-suspended
    (%map-areas #'(lambda (x)
		    (when (eq x thing)
		      (let ((new (if (uvectorp thing)
				   (%alloc-misc (uvsize thing)
						(typecode thing))
				   (cons nil nil))))
			(return-from unwatch (%unwatch thing new)))))
                area-watched)))

(defun %parse-unsigned-integer (vector start end)
  (declare ((simple-array (unsigned-byte 8) (*)) vector)
           (fixnum start end)
           (optimize (speed 3) (safety 0)))
  (let* ((count (- end start))
         (msb 0))
    (declare (fixnum count) ((unsigned-byte 8) msb))
    (or
     (do* ((i start (1+ i)))
          ((>= i end) 0)
       (declare (fixnum i))
       (let* ((b (aref vector i)))
         (declare ((unsigned-byte 8) b))
         (cond ((zerop b) (incf start) (decf count))
               (t (setq msb b) (return)))))
     (cond
       ((or (< count #+64-bit-target 8 #+32-bit-target 4)
            (and (= count #+64-bit-target 8 #+32-bit-target 4)
                 (< msb #+64-bit-target 16 #+32-bit-target 32)))
        ;; Result will be a fixnum.
        (do* ((result 0)
              (shift 0 (+ shift 8))
              (i (1- end) (1- i)))
             ((< i start) result)
          (declare (fixnum result shift i))
          (setq result (logior result (the fixnum (%ilsl shift (aref vector i)))))))
       (t
        ;; Result will be a bignum.  If COUNT is a multiple of 4
        ;; and the most significant bit is set, need to add an
        ;; extra word of zero-extension.
        (let* ((result (allocate-typed-vector :bignum
                                              (if (and (logbitp 7 msb)
                                                       (zerop (the fixnum (logand count 3))))
                                                (the fixnum (1+ (the fixnum (ash count -2))))
                                                (the fixnum (ash (the fixnum (+ count 3)) -2))))))
          (declare ((simple-array (unsigned-byte 8) (*)) result)) ; lie
          (dotimes (i count result)
            (decf end)
            (setf (aref result
                        #+little-endian-target i
                        #+big-endian-target (the fixnum (logxor i 3)))
                  (aref vector end)))))))))

  
;;; Octets between START and END encode an unsigned integer in big-endian
;;; byte order.
(defun parse-unsigned-integer (vector &optional (start 0) end)
  (setq end (check-sequence-bounds vector start end))
  (locally (declare (fixnum start end))
      (unless (typep vector '(simple-array (unsigned-byte 8) (*)))
        (multiple-value-bind (data offset) (array-data-and-offset vector)
          (declare (fixnum offset))
          (unless (typep data '(simple-array (unsigned-byte 8) (*)))
            (report-bad-arg vector '(vector (unsigned-byte 8))))
          (incf start offset)
          (incf end offset)
          (setq vector data)))
      (%parse-unsigned-integer vector start end)))

(defun %parse-signed-integer (vector start end)
  (declare ((simple-array (unsigned-byte 8) (*)) vector)
           (fixnum start end)
           (optimize (speed 3) (safety 0)))
  (let* ((count (- end start)))
    (declare (fixnum count))
    (if (zerop count)
      0
      (let* ((sign-byte (aref vector start)))
        (declare (fixnum sign-byte))
        (if (< sign-byte 128)
          (%parse-unsigned-integer vector start end)
          (progn
            (decf sign-byte 256)
            (or
             (do* ()
                  ((= count 1) sign-byte)
               (unless (= sign-byte -1)
                 (return))
               (let* ((next (1+ start))
                      (nextb (aref vector next)))
                 (declare (fixnum next nextb))
                 (if (not (logbitp 7 nextb))
                   (return))
                 (setq sign-byte (- nextb 256)
                       start next
                       count (1- count))))
             (cond ((or (< count #+64-bit-target 8 #+32-bit-target 4)
                        (and (= count #+64-bit-target 8 #+32-bit-target 4)
                             (>= sign-byte
                                 #+64-bit-target -16
                                 #+32-bit-target -32)))
                    ;; Result will be a fixnum
                    (do* ((result 0)
                          (shift 0 (+ shift 8))
                          (i (1- end) (1- i)))
                         ((= i start) (logior result (the fixnum (%ilsl shift sign-byte))))
                      (declare (fixnum result shift i))
                      (setq result (logior result (the fixnum (%ilsl shift (aref vector i)))))))
                   (t
                    (let* ((result (allocate-typed-vector :bignum (the fixnum (ash (the fixnum (+ count 3)) -2)))))
          (declare ((simple-array (unsigned-byte 8) (*)) result)) ; lie
          (dotimes (i count (do* ((i count (1+ i)))
                                 ((= 0 (the fixnum (logand i 3)))
                                  result)
                              (declare (fixnum i))
                              (setf (aref result
                                          #+little-endian-target i
                                          #+big-endian-target (the fixnum (logxor i 3))) #xff)))
            (decf end)
            (setf (aref result
                        #+little-endian-target i
                        #+big-endian-target (the fixnum (logxor i 3)))
                  (aref vector end)))))))))))))

(defun parse-signed-integer (vector &optional (start 0) end)
  (setq end (check-sequence-bounds vector start end))
  (locally (declare (fixnum start end))
    (unless (typep vector '(simple-array (unsigned-byte 8) (*)))
      (multiple-value-bind (data offset) (array-data-and-offset vector)
        (declare (fixnum offset))
        (unless (typep data '(simple-array (unsigned-byte 8) (*)))
          (report-bad-arg vector '(vector (unsigned-byte 8))))
        (incf start offset)
        (incf end offset)
        (setq vector data)))
    (%parse-signed-integer vector start end)))

#+windows-target
(defun open-null-device ()
  (rlet ((sa #>SECURITY_ATTRIBUTES
           #>nLength (record-length #>SECURITY_ATTRIBUTES)
           #>lpSecurityDescriptor +null-ptr+
           #>bInheritHandle #$TRUE))
    (with-filename-cstrs ((name "\\Device\\Null"))
      (let* ((handle (#_CreateFileW name
                                    (logior #$GENERIC_READ #$GENERIC_WRITE)
                                    (logior #$FILE_SHARE_READ #$FILE_SHARE_WRITE)
                                    sa
                                    #$OPEN_EXISTING
                                    #$FILE_ATTRIBUTE_NORMAL
                                    +null-ptr+)))
        (unless (eql handle #$INVALID_HANDLE_VALUE)
          handle)))))