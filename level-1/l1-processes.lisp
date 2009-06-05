;;;-*- Mode: Lisp; Package: CCL -*-
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


;; L1-processes.lisp

(cl:in-package "CCL")


(let* ((all-processes ())
       (shutdown-processes ())
       (all-processes-lock (make-lock)))
  (defun add-to-all-processes (p)
    (with-lock-grabbed (all-processes-lock)
      (pushnew p all-processes :test #'eq)
      p))
  (defun remove-from-all-processes (p)
    (with-lock-grabbed (all-processes-lock)
      (setq all-processes (delete p all-processes))
      t))
  (defun all-processes ()
    "Obtain a fresh list of all known Lisp threads."
    (with-lock-grabbed (all-processes-lock)
      (copy-list all-processes)))
  (defun shutdown-processes ()
    (with-lock-grabbed (all-processes-lock)
      (copy-list shutdown-processes)))
  (defun %clear-shutdown-proceses ()
    (setq shutdown-processes nil))
  (defun add-to-shutdown-processes (p)
    (with-lock-grabbed (all-processes-lock)
      (pushnew p shutdown-processes :test #'eq))
    t)
  (defun pop-shutdown-processes ()
    (with-lock-grabbed (all-processes-lock)
      (pop shutdown-processes)))
  (defun find-process (id)
    (etypecase id
      (process id)
      (integer (with-lock-grabbed (all-processes-lock)
		 (find id all-processes
		       :key #'(lambda (p)
				(process-serial-number p)))))
      (string (with-lock-grabbed (all-processes-lock)
		(find id all-processes
		      :key #'(lambda (p)
			       (process-name p))
		      :test #'equal))))))



(defun not-in-current-process (p operation)
  (if (eq p *current-process*)
    (error "The current process (~s) can't perform the ~a operation on itself."
	   p operation)))

(defun startup-shutdown-processes ()
  (let* ((p))
    (loop
      (unless (setq p (pop-shutdown-processes)) (return))
      (new-tcr-for-thread (process-thread p))
      (%process-preset-internal p)
      (process-enable p)
      )))

;;; Done with a queue-fixup so that it will be the last thing
;;; that happens on startup.
(queue-fixup
 (pushnew 'startup-shutdown-processes *lisp-system-pointer-functions*))







(defun wrap-initial-bindings (alist)
  (mapcar #'(lambda (pair)
              (destructuring-bind (symbol . valform) pair
                (cons (require-type symbol 'symbol)
                      (cond ((symbolp valform)
                             (constantly (symbol-value valform)))
                            ((typep valform 'function) valform)
                            ((consp valform)
                             (if (eq (car valform) 'quote)
                               (constantly (cadr valform))
                               #'(lambda () (apply (car valform) (cdr valform)))))
                            (t
                             (constantly valform))))))
          alist))


(defun valid-allocation-quantum-p (x)
  (and (>= x *host-page-size*)
       (<= x (default-allocation-quantum))
       (= (logcount x) 1)))

  
(let* ((psn -1))
  (defun %new-psn () (incf psn)))

(defclass process ()
    ((name :initform nil :initarg :name :accessor process-name)
     (thread :initarg :thread :initform nil :accessor process-thread)
     (initial-form :initform (cons nil nil) :reader process-initial-form)
     (priority :initform 0 :initarg :priority :accessor process-priority)
     (persistent :initform nil :initarg :persistent :reader process-persistent)
     (splice :initform (cons nil nil) :accessor process-splice)
     (initial-bindings :initform nil :initarg :initial-bindings
		       :accessor process-initial-bindings)
     (serial-number :initform (%new-psn) :accessor process-serial-number)
     (creation-time :initform (get-tick-count) :reader process-creation-time)
     (total-run-time :initform nil :accessor %process-total-run-time)
     (ui-object :initform (application-ui-object *application*)
                :accessor process-ui-object)
     (termination-semaphore :initform nil
                            :initarg :termination-semaphore
                            :accessor process-termination-semaphore
                            :type (or null semaphore))
     (allocation-quantum :initform (default-allocation-quantum)
                         :initarg :allocation-quantum
                         :reader process-allocation-quantum
                         :type (satisfies valid-allocation-quantum-p))
     (dribble-stream :initform nil)
     (dribble-saved-terminal-io :initform nil)
     (result :initform (cons nil nil)
             :reader process-result))
  (:primary-p t))

(defparameter *print-process-whostate* t "make it optional")

(defmethod print-object ((p process) s)
  (print-unreadable-object (p s :type t :identity t)
    (format s "~a(~d)" (process-name p)
	    (process-serial-number p))
    (when *print-process-whostate*
      (format s " [~a]" (process-whostate p)))))

(defvar *process-class* (find-class 'process))

(defun processp (p)
  (memq *process-class* (class-precedence-list (class-of p))))

(set-type-predicate 'process 'processp)

(defun make-process (name &key 
			  thread
			  persistent
                          (priority 0)
                          (stack-size *default-control-stack-size*)
                          (vstack-size *default-value-stack-size*)
                          (tstack-size *default-temp-stack-size*)
                          (initial-bindings ())
			  (use-standard-initial-bindings t)
                          (class (find-class 'process))
                          (termination-semaphore ())
                          (allocation-quantum (default-allocation-quantum)))
  "Create and return a new process."
  (let* ((p (make-instance
	     class
	     :name name
	     :priority priority
	     :persistent persistent
	     :initial-bindings (append (if use-standard-initial-bindings
					 (standard-initial-bindings))
				       (wrap-initial-bindings
					initial-bindings))
             :termination-semaphore (or termination-semaphore
                                        (make-semaphore))
             :allocation-quantum allocation-quantum)))
    (with-slots ((lisp-thread thread)) p
      (unless lisp-thread
        (setq lisp-thread
              (or thread
                  (new-thread name stack-size  vstack-size  tstack-size)))))
    (add-to-all-processes p)
    (setf (car (process-splice p)) p)
    p))


(defstatic *initial-process*
    (let* ((p (make-process
	       "Initial"
	       :thread *initial-lisp-thread*
	       :priority 0)))
      p))


(defvar *current-process* *initial-process*
  "Bound in each process, to that process itself.")

(defstatic *interactive-abort-process* *initial-process*)




(defun process-tcr (p)
  (lisp-thread.tcr (process-thread p)))



(defun process-exhausted-p (p)
  (let* ((thread (process-thread p)))
    (or (null thread)
	(thread-exhausted-p thread))))
  
;;; This should be way more concerned about being correct and thread-safe
;;; than about being quick: it's generally only called while printing
;;; or debugging, and there are all kinds of subtle race conditions
;;; here.
(defun process-whostate (p)
  "Return a string which describes the status of a specified process."
    (let* ((ip *initial-process*))
      (cond ((eq p *current-process*)
             (if (%tcr-binding-location (%current-tcr) '*whostate*)
               *whostate*
               (if (eq p ip)
                 "Active"
                 "Reset")))
            (t
             (without-interrupts
              (with-lock-grabbed (*kernel-exception-lock*)
               (with-lock-grabbed (*kernel-tcr-area-lock*)
                 (let* ((tcr (process-tcr p)))
                   (if tcr
                     (unwind-protect
                          (let* ((loc nil))
                            (%suspend-tcr tcr)
                            (setq loc (%tcr-binding-location tcr '*whostate*))
                            (if loc
                              (%fixnum-ref loc)
                              (if (eq p ip)
                                "Active"
                                "Reset")))
                       (%resume-tcr tcr))
                     "Exhausted")))))))))

(defun (setf process-whostate) (new p)
  (unless (process-exhausted-p p)
    (setf (symbol-value-in-process '*whostate* p) new)))



(defun process-total-run-time (p)
  (or (%process-total-run-time p)
      (thread-total-run-time (process-thread p))))




(defun initial-bindings (alist)
  (let* ((symbols ())
	 (values ()))
    (dolist (a alist (values (nreverse symbols) (nreverse values)))
      (push (car a) symbols)
      (push (funcall (cdr a)) values))))


                            
(defun symbol-value-in-process (sym process)
  (if (eq process *current-process*)
    (symbol-value sym)
    (let* ((val
            (without-interrupts
             (with-lock-grabbed (*kernel-exception-lock*)
               (with-lock-grabbed (*kernel-tcr-area-lock*)
                 (let* ((tcr (process-tcr process)))
                   (if tcr
                     (symbol-value-in-tcr sym tcr)
                     (%sym-global-value sym))))))))
      (if (eq val (%unbound-marker))
        ;; This might want to be a CELL-ERROR.
        (error "~S is unbound in ~S." sym process)
        val))))

(defun (setf symbol-value-in-process) (value sym process)
  (if (eq process *current-process*)
    (setf (symbol-value sym) value)
    (with-lock-grabbed (*kernel-exception-lock*)
      (with-lock-grabbed (*kernel-tcr-area-lock*)
        (let* ((tcr (process-tcr process)))
          (if tcr
            (setf (symbol-value-in-tcr sym tcr) value)
            (%set-sym-global-value sym value)))))))


(defmethod process-enable ((p process) &optional (wait (* 60 60 24) wait-p))
  "Begin executing the initial function of a specified process."
  (not-in-current-process p 'process-enable)
  (when wait-p
    (check-type wait (unsigned-byte 32)))
  (unless (car (process-initial-form p))
    (error "Process ~s has not been preset.  Use PROCESS-PRESET to preset the process." p))
  (let* ((thread (process-thread p)))
    (do* ((total-wait wait (+ total-wait wait)))
	 ((thread-enable thread (process-termination-semaphore p) (1- (integer-length (process-allocation-quantum p)))  wait)
          (process-tcr-enable p (lisp-thread.tcr thread))
	  p)
      (cerror "Keep trying."
	      "Unable to enable process ~s; have been trying for ~s seconds."
	      p total-wait))))

(defmethod process-tcr-enable ((process process) tcr)
  (when (and tcr (not (eql 0 tcr)))
    (%signal-semaphore-ptr (%fixnum-ref-macptr tcr target::tcr.activate))
    ))

(defmethod (setf process-termination-semaphore) :after (new (p process))
  (with-macptrs (tcrp)
    (%setf-macptr-to-object tcrp (process-tcr p))
    (unless (%null-ptr-p tcrp)
      (setf (%get-ptr tcrp target::tcr.termination-semaphore)
            (if new
              (semaphore-value new)
              (%null-ptr))))
    new))

(defun process-resume (p)
  "Resume a specified process which had previously been suspended
by process-suspend."
  (setq p (require-type p 'process))
  (let* ((tcr (process-tcr p)))
    (and tcr (%resume-tcr tcr))))

(defun process-suspend (p)
  "Suspend a specified process."
  (setq p (require-type p 'process))
  (if (eq p *current-process*)
    (error "Suspending the current process can't work.  ~&(If the documentation claims otherwise, it's incorrect.)")
    (let* ((tcr (process-tcr p)))
      (and tcr (%suspend-tcr tcr)))))

(defun process-suspend-count (p)
  "Return the number of currently-pending suspensions applicable to
a given process."
  (setq p (require-type p 'process))
  (let* ((thread (process-thread p)))
    (if thread
      (lisp-thread-suspend-count thread))))

(defun process-active-p (p)
  (setq p (require-type p 'process))
  (and (eql 0 (process-suspend-count p))
       (not (process-exhausted-p p))))
  
;;; Used by process-run-function
(defmethod process-preset ((p process) function &rest args)
  "Set the initial function and arguments of a specified process."
  (let* ((f (require-type function 'function))
         (initial-form (process-initial-form p)))
    (declare (type cons initial-form))
    (not-in-current-process p 'process-preset)
    ; Not quite right ...
    (rplaca initial-form f)
    (rplacd initial-form args)
    (%process-preset-internal p)))

(defmethod %process-preset-internal ((process process))
   (let* ((initial-form (process-initial-form process))
         (thread (process-thread process)))
     (declare (type cons initial-form))
     (thread-preset
      thread
      #'(lambda (process initial-form)
	  (let* ((*current-process* process))
	    (add-to-all-processes process)
	    (multiple-value-bind (syms values)
		(initial-bindings (process-initial-bindings process))
	      (progv syms values
                (setq *whostate* "Active")
		(run-process-initial-form process initial-form)))))
      process
      initial-form)
     process))


(defun run-process-initial-form (process initial-form)
  (let* ((exited nil)
	 (kill (handler-case
		   (restart-case
		    (let ((values
                           (multiple-value-list
                            (apply (car initial-form)
                                   (cdr (the list initial-form)))))
                          (result (process-result process)))
                      (setf (cdr result) values
                            (car result) t)
		      (setq exited t)
		      nil)
                    (abort-break () :report "Reset this thread")
		    (abort () :report "Kill this thread" (setq exited t)))
		 (process-reset (condition)
		   (process-reset-kill condition)))))
    ;; We either exited from the initial form normally, were told to
    ;; exit prematurely, or are being reset and should enter the
    ;; "awaiting preset" state.
    (if (or kill exited) 
      (unless (eq kill :toplevel)
	(process-initial-form-exited process (or kill t)))
      (progn
	(thread-change-state (process-thread process) :run :reset)
	(tcr-set-preset-state (process-tcr process))))
    nil))

;;; Separated from run-process-initial-form just so I can change it easily.
(defun process-initial-form-exited (process kill)
  ;; Enter the *initial-process* and have it finish us up
  (without-interrupts
   (if (eq kill :shutdown)
     (progn
       (setq *whostate* "Shutdown")
       (add-to-shutdown-processes process)))
   (maybe-finish-process-kill process kill)))

(defun maybe-finish-process-kill (process kill)
  (when (and kill (neq kill :shutdown))
    (setf (process-whostate process) "Dead")
    (remove-from-all-processes process)
    (let ((thread (process-thread process)))
      (unless (or (eq thread *current-lisp-thread*)
                  (thread-exhausted-p thread))
        (kill-lisp-thread thread))))
  nil)


 

(defun require-global-symbol (s &optional env)
  (let* ((s (require-type s 'symbol))
	 (bits (%symbol-bits s)))
    (unless (or (logbitp $sym_vbit_global bits)
		(let* ((defenv (if env (definition-environment env))))
		  (if defenv
		    (eq :global (%cdr (assq s (defenv.specials defenv)))))))
      (error "~s not defined with ~s" s 'defstatic))
    s))


(defmethod print-object ((s lock) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (let* ((val (uvref s target::lock._value-cell))
	   (name (uvref s target::lock.name-cell)))
      (when name
	(format stream "~s " name))
      (if (typep val 'macptr)
        (format stream "[ptr @ #x~x]"
                (%ptr-to-int val))))))

(defun lockp (l)
  (eq target::subtag-lock (typecode l)))

(set-type-predicate 'lock 'lockp)

(defun recursive-lock-p (l)
  (and (eq target::subtag-lock (typecode l))
       (eq 'recursive-lock (%svref l target::lock.kind-cell))))

(defun read-write-lock-p (l)
  (and (eq target::subtag-lock (typecode l))
       (eq 'read-write-lock (%svref l target::lock.kind-cell))))

(setf (type-predicate 'recursive-lock) 'recursive-lock-p
      (type-predicate 'read-write-lock) 'read-write-lock-p)


(defun grab-lock (lock &optional flag)
  "Wait until a given lock can be obtained, then obtain it."
  (%lock-recursive-lock-object lock flag))

(defun release-lock (lock)
  "Relinquish ownership of a given lock."
  (%unlock-recursive-lock-object lock))

(defun try-lock (lock &optional flag)
  "Obtain the given lock, but only if it is not necessary to wait for it."
  (%try-recursive-lock-object lock flag))

(defun lock-acquisition-status (thing)
  (if (istruct-typep thing 'lock-acquisition)
    (lock-acquisition.status thing)
    (report-bad-arg thing 'lock-acquisition)))

(defun clear-lock-acquisition-status (thing)
  (if (istruct-typep thing 'lock-acquisition)
    (setf (lock-acquisition.status thing) nil)
    (report-bad-arg thing 'lock-acquisition)))

(defmethod print-object ((l lock-acquisition) stream)
  (print-unreadable-object (l stream :type t :identity t)
    (format stream "[status = ~s]" (lock-acquisition-status l))))

(defun semaphore-notification-status (thing)
  (if (istruct-typep thing 'semaphore-notification)
    (semaphore-notification.status thing)
    (report-bad-arg thing 'semaphore-notification)))

(defun clear-semaphore-notification-status (thing)
  (if (istruct-typep thing 'semaphore-notification)
    (setf (semaphore-notification.status thing) nil)
    (report-bad-arg thing 'semaphore-notification)))

(defmethod print-object ((l semaphore-notification) stream)
  (print-unreadable-object (l stream :type t :identity t)
    (format stream "[status = ~s]" (semaphore-notification-status l))))

(defun process-wait (whostate function &rest args)
  "Causes the current lisp process (thread) to wait for a given
predicate to return true."
  (declare (dynamic-extent args))
  (or (apply function args)
      (with-process-whostate (whostate)
        (loop
          (when (apply function args)
            (return))
          ;; Sleep for a tick
          #-windows-target
          (%nanosleep 0 *ns-per-tick*)
          #+windows-target
          (%windows-sleep 5)))))



(defun process-wait-with-timeout (whostate time function &rest args)
  "Cause the current thread to wait for a given predicate to return true,
or for a timeout to expire."
  (declare (dynamic-extent args))
  (cond ((null time)  (apply #'process-wait whostate function args) t)
        (t (let* ((win nil)
                  (when (+ (get-tick-count) time))
                  (f #'(lambda () (let ((val (apply function args)))
                                    (if val
                                      (setq win val)
                                      (> (get-tick-count) when))))))
             (declare (dynamic-extent f))
             (process-wait whostate f)
             win))))


(defmethod process-interrupt ((process process) function &rest args)
  "Arrange for the target process to invoke a specified function at
some point in the near future, and then return to what it was doing."
  (let* ((p (require-type process 'process)))
    (if (eq p *current-process*)
      (progn
        (apply function args)
        t)
      (thread-interrupt
       (process-thread p)
       process
       #'apply
       function args))))

(defmethod process-debug-condition ((p process) condition frame-pointer)
  (declare (ignore condition frame-pointer)))




;;; This one is in the Symbolics documentation
(defun process-allow-schedule ()
  "Used for cooperative multitasking; probably never necessary."
  (process-yield *current-process*))


;;; something unique that users won't get their hands on
(defun process-reset-tag (process)
  (process-splice process))

(defun process-run-function (name-or-keywords function &rest args)
  "Create a process, preset it, and enable it."
  (if (listp name-or-keywords)
    (%process-run-function name-or-keywords function args)
    (let ((keywords (list :name name-or-keywords)))
      (declare (dynamic-extent keywords))
      (%process-run-function keywords function args))))

(defun %process-run-function (keywords function args)
  (destructuring-bind (&key (name "Anonymous")
                            (priority  0)
			    (stack-size *default-control-stack-size*)
			    (vstack-size *default-value-stack-size*)
			    (tstack-size *default-temp-stack-size*)
			    (initial-bindings ())
                            (persistent nil)
			    (use-standard-initial-bindings t)
                            (termination-semaphore nil)
                            (allocation-quantum (default-allocation-quantum)))
                      keywords
    (setq priority (require-type priority 'fixnum))
    (let* ((process (make-process name
                                  :priority priority
                                  :stack-size stack-size
				  :vstack-size vstack-size
				  :tstack-size tstack-size
                                  :persistent persistent
				  :use-standard-initial-bindings use-standard-initial-bindings
				  :initial-bindings initial-bindings
                                  :termination-semaphore termination-semaphore
                                  :allocation-quantum allocation-quantum)))
      (process-preset process #'(lambda () (apply function args)))
      (process-enable process)
      process)))

(defmethod process-reset ((process process) &optional kill)
  "Cause a specified process to cleanly exit from any ongoing computation."
  (setq process (require-type process 'process))
  (unless (memq kill '(nil :kill :shutdown))
    (setq kill (require-type kill '(member nil :kill :shutdown))))
  (if (eq process *current-process*)
    (%process-reset kill)
    (if (process-exhausted-p process)
      (maybe-finish-process-kill process kill)
      (progn
	(process-interrupt process '%process-reset kill)))))

(defmethod process-yield ((p process))
  #+windows-target (#_Sleep 0)
  #-windows-target (#_sched_yield))


(defun %process-reset (kill)
  (signal 'process-reset :kill kill)
  (maybe-finish-process-kill *current-process* kill))

;;; By default, it's just fine with the current process
;;; if the application/user wants to quit.
(defmethod process-verify-quit ((process process))
  t)

(defmethod process-exit-application ((process process) thunk)
  (when (eq process *initial-process*)
    (with-standard-abort-handling "Exit Lisp"
      (prepare-to-quit)
      (fresh-line *stdout*)
      (finish-output *stdout*))
    (%set-toplevel thunk)
    (toplevel)))



(defmethod process-kill ((process process))
  "Cause a specified process to cleanly exit from any ongoing
computation, and then exit."
  (and (process-interrupt process #'%process-reset :kill)
       (setf (process-kill-issued process) t)))

(defun process-abort (process &optional condition)
  "Cause a specified process to process an abort condition, as if it
had invoked abort."
  (process-interrupt process
                     #'(lambda ()
                         (abort condition))))

(defmethod process-reset-and-enable ((process process))
  (not-in-current-process process 'process-reset-and-enable)
  (process-reset process)
  (process-enable process))

(defmethod process-kill-issued ((process process))
  (cdr (process-splice process)))

(defmethod (setf process-kill-issued) (val (process process))
  (setf (cdr (process-splice process)) val))

(defun tcr->process (tcr)
  (dolist (p (all-processes))
    (when (eq tcr (process-tcr p))
      (return p))))

(defun current-process-allocation-quantum ()
  (process-allocation-quantum *current-process*))

(defun (setf current-process-allocation-quantum) (new)
  (if (valid-allocation-quantum-p new)
    (with-macptrs (tcrp)
      (%setf-macptr-to-object tcrp (%current-tcr))
      (setf (slot-value *current-process* 'allocation-quantum) new
            (%get-natural tcrp target::tcr.log2-allocation-quantum)
            (1- (integer-length new)))
      new)
    (report-bad-arg new '(satisfies valid-allocation-quantum-p))))


(def-standard-initial-binding *backtrace-contexts* nil)

(defmethod exit-interactive-process ((p process))
  (unless (eq p *initial-process*)
    (when (eq p *current-process*)
      (process-kill p))))

(defclass tty-listener (process)
    ())

(defmethod exit-interactive-process ((p tty-listener))
  (when (eq p *current-process*)
    (quit)))

(defmethod process-stop-dribbling ((p process))
  (with-slots (dribble-stream dribble-saved-terminal-io) p
    (when dribble-stream
      (close dribble-stream)
      (setq dribble-stream nil))
    (when dribble-saved-terminal-io
      (setq *terminal-io* dribble-saved-terminal-io
            dribble-saved-terminal-io nil))))

(defmethod process-dribble ((p process) path)
  (with-slots (dribble-stream dribble-saved-terminal-io) p
    (process-stop-dribbling p)
    (when path
      (let* ((in (two-way-stream-input-stream *terminal-io*))
             (out (two-way-stream-output-stream *terminal-io*))
             (f (open path :direction :output :if-exists :append 
                      :if-does-not-exist :create)))
        (without-interrupts
         (setq dribble-stream f
               dribble-saved-terminal-io *terminal-io*
               *terminal-io* (make-echoing-two-way-stream
                              (make-echo-stream in f)
                              (make-broadcast-stream out f)))))
      path)))

(defmethod join-process ((p process) &key default)
  (wait-on-semaphore (process-termination-semaphore p) nil "join-process")
  (let ((result (process-result p)))
    (cond ((car result) (values-list (cdr result)))
          (t default))))


