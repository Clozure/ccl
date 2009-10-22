;;; -*- Mode: LISP; Package: CCL -*-
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

;; l1-lisp-threads.lisp

(in-package "CCL")

(defvar *bind-io-control-vars-per-process* nil
  "If true, bind I/O control variables per process")


	     
(defun lisp-thread-p (thing)
  (istruct-typep thing 'lisp-thread))

(setf (type-predicate 'lisp-thread) 'lisp-thread-p)

(defloadvar *ticks-per-second*
    #+windows-target 1000
    #-windows-target
    (#_sysconf #$_SC_CLK_TCK))

(defloadvar *ns-per-tick*
    (floor 1000000000 *ticks-per-second*))

#-windows-target
(defun %nanosleep (seconds nanoseconds)
  (with-process-whostate ("Sleep")
    (rlet ((a :timespec)
           (b :timespec))
      (setf (pref a :timespec.tv_sec) seconds
            (pref a :timespec.tv_nsec) nanoseconds)
      (let* ((aptr a)
             (bptr b))
        (loop
          (let* ((result 
                  (external-call #+darwin-target "_nanosleep"
                                 #-darwin-target "nanosleep"
                                 :address aptr
                                 :address bptr
                                 :signed-fullword)))
            (declare (type (signed-byte 32) result))
            (if (and (< result 0)
                     (eql (%get-errno) (- #$EINTR)))
              ;; x86-64 Leopard bug.
              (let* ((asec (pref aptr :timespec.tv_sec))
                     (bsec (pref bptr :timespec.tv_sec)))
                (if (and (>= bsec 0)
                         (or (< bsec asec)
                             (and (= bsec asec)
                                  (< (pref bptr :timespec.tv_nsec)
                                     (pref aptr :timespec.tv_nsec)))))
                  (psetq aptr bptr bptr aptr)
                  (return)))
              (return))))))))


(defun timeval->ticks (tv)
  (+ (* *ticks-per-second* (pref tv :timeval.tv_sec))
     (round (pref tv :timeval.tv_usec) (floor 1000000 *ticks-per-second*))))


(defun gettimeofday (ptimeval &optional ptz)
  (int-errno-ffcall (%kernel-import target::kernel-import-lisp-gettimeofday)
                    :address ptimeval
                    :address (or ptz (%null-ptr))
                    :int))

(defloadvar *lisp-start-timeval*
    (progn
      (let* ((r (make-record :timeval)))
        (gettimeofday r)
        r)))


(defloadvar *internal-real-time-session-seconds* nil)


(defun get-internal-real-time ()
  "Return the real time in the internal time format. (See
  INTERNAL-TIME-UNITS-PER-SECOND.) This is useful for finding elapsed time."
  (rlet ((tv :timeval))
    (gettimeofday tv)
    (let* ((units (truncate (the fixnum (pref tv :timeval.tv_usec)) (/ 1000000 internal-time-units-per-second)))
           (initial *internal-real-time-session-seconds*))
      (if initial
        (locally
            (declare (type (unsigned-byte 32) initial))
          (+ (* internal-time-units-per-second
                (the (unsigned-byte 32)
                  (- (the (unsigned-byte 32) (pref tv :timeval.tv_sec))
                     initial)))
             units))
        (progn
          (setq *internal-real-time-session-seconds*
                (pref tv :timeval.tv_sec))
          units)))))

(defun get-tick-count ()
  (values (floor (get-internal-real-time)
                 (floor internal-time-units-per-second
                        *ticks-per-second*))))




(defun %kernel-global-offset (name-or-offset)
  (if (fixnump name-or-offset)
    name-or-offset
    (target::%kernel-global name-or-offset)))


(defun %kernel-global-offset-form (name-or-offset-form)
  (cond ((quoted-form-p name-or-offset-form)
         `(%target-kernel-global ,name-or-offset-form))
        ((fixnump name-or-offset-form)
         name-or-offset-form)
        (t `(%target-kernel-global ',name-or-offset-form))))



(defmacro %set-kernel-global (name-or-offset new-value)
  `(%set-kernel-global-from-offset
    ,(%kernel-global-offset-form name-or-offset)
    ,new-value))



; The number of bytes in a consing (or stack) area
(defun %area-size (area)
  (ash (- (%fixnum-ref area target::area.high)
          (%fixnum-ref area target::area.low))
       target::fixnumshift))

(defun %stack-area-usable-size (area)
  (ash (- (%fixnum-ref area target::area.high)
	  (%fixnum-ref area target::area.softlimit))
       target::fixnum-shift))

(defun %cons-lisp-thread (name &optional tcr)
  (%istruct 'lisp-thread
	    tcr
	    name
	    0
	    0
	    0
	    nil
	    nil
            (make-lock)
	    nil
	    :reset
	    (make-lock)))

(defvar *current-lisp-thread*
  (%cons-lisp-thread "Initial" (%current-tcr)))

(defstatic *initial-lisp-thread* *current-lisp-thread*)

(defun thread-change-state (thread oldstate newstate)
  (with-lock-grabbed ((lisp-thread.state-change-lock thread))
    (when (eq (lisp-thread.state thread) oldstate)
      (setf (lisp-thread.state thread) newstate))))

(thread-change-state *initial-lisp-thread* :reset :run)

(defun thread-state (thread)
  (with-lock-grabbed ((lisp-thread.state-change-lock thread))
    (lisp-thread.state thread)))
  
(defun thread-make-startup-function (thread tcr)
  #'(lambda ()
      (thread-change-state thread :reset :run)
      (let* ((*current-lisp-thread* thread)
	     (initial-function (lisp-thread.initial-function.args thread)))
	(tcr-clear-preset-state tcr)
	(%set-tcr-toplevel-function tcr nil)
	(setf (interrupt-level) 0)
	(apply (car initial-function) (cdr initial-function))
	(cleanup-thread-tcr thread tcr))))

(defun init-thread-from-tcr (tcr thread)
  (let* ((cs-area (%fixnum-ref tcr target::tcr.cs-area))
         (vs-area (%fixnum-ref tcr target::tcr.vs-area))
         (ts-area (%fixnum-ref tcr target::tcr.ts-area)))
    (when (or (zerop cs-area)
              (zerop vs-area)
              (zerop ts-area))
      (error "Can't allocate new thread"))
    (setf (lisp-thread.tcr thread) tcr
          (lisp-thread.cs-size thread)
          (%stack-area-usable-size cs-area)
          (lisp-thread.vs-size thread)
          (%stack-area-usable-size vs-area)
          (lisp-thread.ts-size thread)
          (%stack-area-usable-size ts-area)
          (lisp-thread.startup-function thread)
          (thread-make-startup-function thread tcr)))
  (thread-change-state thread :exit :reset)
  thread)

(defun default-allocation-quantum ()
  (ash 1 (%get-kernel-global 'default-allocation-quantum)))

(defun new-lisp-thread-from-tcr (tcr name)
  (let* ((thread (%cons-lisp-thread name tcr)))    
    (init-thread-from-tcr tcr thread)
    (push thread (population-data *lisp-thread-population*))
    thread))

(def-ccl-pointers initial-thread ()
  (init-thread-from-tcr (%current-tcr) *initial-lisp-thread*))

(defmethod print-object ((thread lisp-thread) stream)
  (print-unreadable-object (thread stream :type t :identity t)
    (format stream "~a" (lisp-thread.name thread))
    (let* ((tcr (lisp-thread.tcr thread)))
      (if (and tcr (not (eql 0 tcr)))
	(format stream " [tcr @ #x~x]" (ash tcr target::fixnumshift))))))


(defvar *lisp-thread-population*
  (%cons-population (list *initial-lisp-thread*) $population_weak-list nil))





(defparameter *default-control-stack-size*
  #+32-bit-target (ash 1 20)
  #+64-bit-target (ash 2 20))
(defparameter *default-value-stack-size*
  #+32-bit-target (ash 1 20)
  #+64-bit-target (ash 2 20))
(defparameter *default-temp-stack-size*
  #+32-bit-target (ash 1 19)
  #+64-bit-target (ash 2 19))


(defstatic *initial-listener-default-control-stack-size* *default-control-stack-size*)
(defstatic *initial-listener-default-value-stack-size* *default-value-stack-size*)
(defstatic *initial-listener-default-temp-stack-size* *default-temp-stack-size*)


(def-ccl-pointers listener-stack-sizes ()
  (let* ((size (%get-kernel-global 'stack-size))) ; set by --thread-stack-size
    (declare (fixnum size))
    (when (> size 0)
      (setq *initial-listener-default-control-stack-size* size
            *initial-listener-default-value-stack-size* size
            *initial-listener-default-temp-stack-size* (floor size 2)))))


(defmacro with-area-macptr ((var area) &body body)
  `(with-macptrs (,var)
     (%setf-macptr-to-object ,var ,area)
     ,@body))


(defun gc-area.return-sp (area)
  (%fixnum-ref area target::area.gc-count))


(defun (setf gc-area.return-sp) (return-sp area)
  (setf (%fixnum-ref area target::area.gc-count) return-sp))



(defun shutdown-lisp-threads ()
  )

(defun %current-xp ()
  (let ((xframe (%fixnum-ref (%current-tcr) target::tcr.xframe)))
    (when (eql xframe 0)
      (error "No current exception frame"))
    (%fixnum-ref xframe
                 (get-field-offset :xframe-list.this))))

(defun new-tcr (cs-size vs-size ts-size)
  (let* ((tcr (macptr->fixnum
               (ff-call
                (%kernel-import target::kernel-import-newthread)
                #+64-bit-target :unsigned-doubleword
                #+32-bit-target :unsigned-fullword cs-size
                #+64-bit-target :unsigned-doubleword
                #+32-bit-target :unsigned-fullword vs-size
                #+64-bit-target :unsigned-doubleword
                #+32-bit-target :unsigned-fullword ts-size
                :address))))
    (declare (fixnum tcr))
    (if (zerop tcr)
      (error "Can't create thread")
      tcr)))

(defun new-thread (name cstack-size vstack-size tstack-size)
  (new-lisp-thread-from-tcr (new-tcr cstack-size vstack-size tstack-size) name))

(defun new-tcr-for-thread (thread)
  (let* ((tcr (new-tcr
	       (lisp-thread.cs-size thread)
	       (lisp-thread.vs-size thread)
	       (lisp-thread.ts-size thread))))
    (setf (lisp-thread.tcr thread) tcr
	  (lisp-thread.startup-function thread)
	  (thread-make-startup-function thread tcr))
    (thread-change-state thread :exit :reset)
    tcr))
  
	 



(defconstant cstack-hardprot (ash 100 10))
(defconstant cstack-softprot (ash 100 10))



(defun tcr-flags (tcr)
  (%fixnum-ref tcr target::tcr.flags))



(defun %tcr-frame-ptr (tcr)
  (with-macptrs (p)
    (%setf-macptr-to-object p tcr)
    (%fixnum-from-macptr
     (ff-call (%kernel-import target::kernel-import-tcr-frame-ptr)
              :address p
              :address))))
 
(defun thread-exhausted-p (thread)
  (or (null thread)
      (null (lisp-thread.tcr thread))))

(defun thread-total-run-time (thread)
  (unless (thread-exhausted-p thread)
    nil))

(defun %tcr-interrupt (tcr)
  ;; The other thread's interrupt-pending flag might get cleared
  ;; right after we look and see it set, but since this is called
  ;; with the lock on the thread's interrupt queue held, the
  ;; pending interrupt won't have been taken yet.
  ;; When a thread dies, it should try to clear its interrupt-pending
  ;; flag.
  (if (eql 0 (%fixnum-ref tcr target::tcr.interrupt-pending))
    (%%tcr-interrupt tcr)
    0))



     
     

(defun thread-interrupt (thread process function &rest args)
  (with-lock-grabbed ((lisp-thread.state-change-lock thread))
    (case (lisp-thread.state thread)
      (:run 
       (with-lock-grabbed ((lisp-thread.interrupt-lock thread))
         (let ((tcr (lisp-thread.tcr thread)))
	   (when tcr
	     (push (cons function args)
		   (lisp-thread.interrupt-functions thread))
	     (eql 0 (%tcr-interrupt tcr))))))
      (:reset
       ;; Preset the thread with a function that'll return to the :reset
       ;; state
       (let* ((pif (process-initial-form process))
	      (pif-f (car pif))
	      (pif-args (cdr pif)))
	 (process-preset process #'(lambda ()
				     (%rplaca pif pif-f)
				     (%rplacd pif pif-args)
				     (apply function args)
				     ;; If function returns normally,
				     ;; return to the reset state
				     (%process-reset nil)))
	 (thread-enable thread (process-termination-semaphore process) (1- (integer-length (process-allocation-quantum process))) 0)
         t)))))

(defun thread-handle-interrupts ()
  (let* ((thread *current-lisp-thread*))
    (with-process-whostate ("Active")
      (loop
        (let* ((f (with-lock-grabbed ((lisp-thread.interrupt-lock thread))
                    (pop (lisp-thread.interrupt-functions thread)))))
          (if f
            (apply (car f) (cdr f))
            (return)))))))


	
(defun  thread-preset (thread function &rest args)
  (setf (lisp-thread.initial-function.args thread)
	(cons function args)))

(defun thread-enable (thread termination-semaphore allocation-quantum &optional (timeout (* 60 60 24)))
  (let* ((tcr (or (lisp-thread.tcr thread) (new-tcr-for-thread thread))))
    (with-macptrs (s)
      (%setf-macptr-to-object s (%fixnum-ref tcr target::tcr.reset-completion))
      (when (%timed-wait-on-semaphore-ptr s timeout nil)
        (%set-tcr-toplevel-function
         tcr
         (lisp-thread.startup-function thread))
        (%activate-tcr tcr termination-semaphore allocation-quantum)
        thread))))
			      

(defun cleanup-thread-tcr (thread tcr)
  (let* ((flags (%fixnum-ref tcr target::tcr.flags)))
    (declare (fixnum flags))
    (if (logbitp arch::tcr-flag-bit-awaiting-preset flags)
      (thread-change-state thread :run :reset)
      (progn
	(thread-change-state thread :run :exit)
	(setf (lisp-thread.tcr thread) nil)))))

(defun kill-lisp-thread (thread)
  (unless (eq thread *initial-lisp-thread*)
    (let* ((tcr (lisp-thread.tcr thread)))
      (when tcr
        (setf (lisp-thread.tcr thread) nil
              (lisp-thread.state thread) :exit)
	(%kill-tcr tcr)))))

;;; This returns the underlying pthread, whatever that is, as an
;;; unsigned integer.
(defun lisp-thread-os-thread (thread)
  (with-macptrs (tcrp)
    (%setf-macptr-to-object tcrp (lisp-thread.tcr thread))
    (unless (%null-ptr-p tcrp)
      (let* ((natural (%get-natural tcrp target::tcr.osid)))
        (unless (zerop natural) natural)))))


                         
;;; This returns something lower-level than the pthread, if that
;;; concept makes sense.  On current versions of Linux, it returns
;;; the pid of the clone()d process; on Darwin, it returns a Mach
;;; thread.  On some (near)future version of Linux, the concept
;;; may not apply.
;;; The future is here: on Linux systems using NPTL, this returns
;;; exactly the same thing that (getpid) does.
;;; This should probably be retired; even if it does something
;;; interesting, is the value it returns useful ?

(defun lisp-thread-native-thread (thread)
  (with-macptrs (tcrp)
    (%setf-macptr-to-object tcrp (lisp-thread.tcr thread))
    (unless (%null-ptr-p tcrp)
      (#+32-bit-target %get-unsigned-long
       #+64-bit-target %%get-unsigned-longlong tcrp target::tcr.native-thread-id))))

(defun lisp-thread-suspend-count (thread)
  (with-macptrs (tcrp)
    (%setf-macptr-to-object tcrp (lisp-thread.tcr thread))
    (unless (%null-ptr-p tcrp)
      (#+32-bit-target %get-unsigned-long
       #+64-bit-target %%get-unsigned-longlong tcrp target::tcr.suspend-count))))

(defun tcr-clear-preset-state (tcr)
  (let* ((flags (%fixnum-ref tcr target::tcr.flags)))
    (declare (fixnum flags))
    (setf (%fixnum-ref tcr target::tcr.flags)
	  (bitclr arch::tcr-flag-bit-awaiting-preset flags))))

(defun tcr-set-preset-state (tcr)
  (let* ((flags (%fixnum-ref tcr target::tcr.flags)))
    (declare (fixnum flags))
    (setf (%fixnum-ref tcr target::tcr.flags)
	  (bitset arch::tcr-flag-bit-awaiting-preset flags))))  

;;; This doesn't quite activate the thread; see PROCESS-TCR-ENABLE.
(defun %activate-tcr (tcr termination-semaphore allocation-quantum)
  (declare (ignore termination-semaphore))
  (if (and tcr (not (eql 0 tcr)))
    (with-macptrs (tcrp)
      (%setf-macptr-to-object tcrp tcr)
      (setf (%get-natural tcrp target::tcr.log2-allocation-quantum)
            (or allocation-quantum (default-allocation-quantum)))
      t)))
                         
(defvar *canonical-error-value*
  '(*canonical-error-value*))


(defun symbol-value-in-tcr (sym tcr)
  (if (eq tcr (%current-tcr))
    (%sym-value sym)
    (unwind-protect
         (progn
           (%suspend-tcr tcr)
           (let* ((loc (%tcr-binding-location tcr sym)))
             (if loc
               (%fixnum-ref loc)
               (%sym-global-value sym))))
      (%resume-tcr tcr))))

(defun (setf symbol-value-in-tcr) (value sym tcr)
  (if (eq tcr (%current-tcr))
    (%set-sym-value sym value)
    (unwind-protect
         (progn
           (%suspend-tcr tcr)
           (let* ((loc (%tcr-binding-location tcr sym)))
             (if loc
               (setf (%fixnum-ref loc) value)
               (%set-sym-global-value sym value))))
      (%resume-tcr tcr))))

;;; Backtrace support
;;;



(defmacro do-db-links ((db-link &optional var value) &body body)
  (let ((thunk (gensym))
        (var-var (or var (gensym)))
        (value-var (or value (gensym))))
    `(block nil
       (let ((,thunk #'(lambda (,db-link ,var-var ,value-var)
                         (declare (ignorable ,db-link))
                         ,@(unless var (list `(declare (ignore ,var-var))))
                         ,@(unless value (list `(declare (ignore ,value-var))))
                         ,@body)))
         (declare (dynamic-extent ,thunk))
         (map-db-links ,thunk)))))




(defun map-db-links (f)
  (without-interrupts
   (let ((db-link (%current-db-link)))
     (loop
       (when (eql 0 db-link) (return))
       (funcall f db-link (%fixnum-ref db-link (* 1 target::node-size)) (%fixnum-ref db-link (* 2 target::node-size)))
       (setq db-link (%fixnum-ref db-link))))))

(defun %get-frame-ptr ()
  (%current-frame-ptr))

(defun %current-exception-frame ()
  #+ppc-target *fake-stack-frames*
  #+x86-target (or (let* ((xcf (%current-xcf)))
                     (if xcf
                       (%%frame-backlink xcf)))
                   (%current-frame-ptr)))





(defun next-catch (catch)
  (let ((next-catch (uvref catch target::catch-frame.link-cell)))
    (unless (eql next-catch 0) next-catch)))




; @@@ this needs to load early so errors can work
(defun next-lisp-frame (p context)
  (let ((frame p))
    (loop
      (let ((parent (%frame-backlink frame context)))
        (multiple-value-bind (lisp-frame-p bos-p) (lisp-frame-p parent context)
          (if lisp-frame-p
            (return parent)
            (if bos-p
              (return nil))))
        (setq frame parent)))))

(defun parent-frame (p context)
  (loop
    (let ((parent (next-lisp-frame p context)))
      (when (or (null parent)
                (not (catch-csp-p parent context)))
        (return parent))
      (setq p parent))))





(defun last-frame-ptr (&optional context origin)
  (let* ((current (or origin
                      (if context (bt.current context) (%current-frame-ptr))))
         (last current))
    (loop
      (setq current (parent-frame current context))
      (if current
        (setq last current)
        (return last)))))



(defun child-frame (p context )
  (let* ((current (if context (bt.current context) (%current-frame-ptr)))
         (last nil))
    (loop
      (when (null current)
        (return nil))
      (when (eq current p) (return last))
      (setq last current
            current (parent-frame current context)))))





; This returns the current head of the db-link chain.
(defun db-link (&optional context)
  (if context
    (bt.db-link context)
    (%fixnum-ref (%current-tcr)  target::tcr.db-link)))

(defun previous-db-link (db-link start )
  (declare (fixnum db-link start))
  (let ((prev nil))
    (loop
      (when (or (eql db-link start) (eql 0 start))
        (return prev))
      (setq prev start
            start (%fixnum-ref start 0)))))

(defun count-db-links-in-frame (vsp parent-vsp &optional context)
  (declare (fixnum vsp parent-vsp))
  (let ((db (db-link context))
        (count 0)
        (first nil)
        (last nil))
    (declare (fixnum db count))
    (loop
      (cond ((eql db 0)
             (return (values count (or first 0) (or last 0))))
            ((and (>= db vsp) (< db parent-vsp))
             (unless first (setq first db))
             (setq last db)
             (incf count)))
      (setq db (%fixnum-ref db)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; bogus-thing-p support
;;;

(defun %ptr-in-area-p (ptr area)
  (declare (optimize (speed 3) (safety 0)) (fixnum ptr area))           ; lie, maybe
  (and (<= (the fixnum (%fixnum-ref area target::area.low)) ptr)
       (> (the fixnum (%fixnum-ref area target::area.high)) ptr)))

(defun %active-area (area active)
  (or (do ((a area (%fixnum-ref a target::area.older)))
          ((eql a 0))
        (when (%ptr-in-area-p active a)
          (return a)))
      (do ((a (%fixnum-ref area target::area.younger) (%fixnum-ref a target::area.younger)))
          ((eql a 0))
        (when (%ptr-in-area-p active a)
          (return a)))))

(defun %ptr-to-vstack-p (tcr idx)
  (%ptr-in-area-p idx (%fixnum-ref tcr target::tcr.vs-area)))

(defun %on-tsp-stack (tcr object)
  (%ptr-in-area-p object (%fixnum-ref tcr target::tcr.ts-area)))

(defun %on-csp-stack (tcr object)
  (%ptr-in-area-p object (%fixnum-ref tcr target::tcr.cs-area)))

(defparameter *aux-tsp-ranges* ())
(defparameter *aux-vsp-ranges* ())
(defparameter *aux-csp-ranges* ())

(defun object-in-range-p (object range)
  (declare (fixnum object))
  (when range
    (destructuring-bind (active . high) range
      (declare (fixnum active high))
      (and (< active object)
           (< object high)))))

(defun object-in-some-range (object ranges)
  (dolist (r ranges)
    (when (object-in-range-p object r)
      (return t))))


(defun on-any-tsp-stack (object)
  (or (%on-tsp-stack (%current-tcr) object)
      (object-in-some-range object *aux-tsp-ranges*)))

(defun on-any-vstack (idx)
  (or (%ptr-to-vstack-p (%current-tcr) idx)
      (object-in-some-range idx *aux-vsp-ranges*)))

(defun on-any-csp-stack (object)
  (or (%on-csp-stack (%current-tcr) object)
      (object-in-some-range object *aux-csp-ranges*)))

;;; This MUST return either T or NIL.
(defun temporary-cons-p (x)
  (and (consp x)
       (not (null (or (on-any-vstack x)
                      (on-any-tsp-stack x))))))







(defun %value-cell-header-at-p (cur-vsp)
  (eql target::value-cell-header (%fixnum-address-of (%fixnum-ref cur-vsp))))

(defun count-stack-consed-value-cells-in-frame (vsp parent-vsp)
  (let ((cur-vsp vsp)
        (count 0))
    (declare (fixnum cur-vsp count))
    (loop
      (when (>= cur-vsp parent-vsp) (return))
      (when (and (evenp cur-vsp) (%value-cell-header-at-p cur-vsp))
        (incf count)
        (incf cur-vsp))                 ; don't need to check value after header
      (incf cur-vsp))
    count))

;;; stack consed value cells are one of two forms:
;;; Well, they were of two forms.  When they existed, that is.
;;;
;;; nil             ; n-4
;;; header          ; n = even address (multiple of 8)
;;; value           ; n+4
;;;
;;; header          ; n = even address (multiple of 8)
;;; value           ; n+4
;;; nil             ; n+8

(defun in-stack-consed-value-cell-p (arg-vsp vsp parent-vsp)
  (declare (fixnum arg-vsp vsp parent-vsp))
  (if (evenp arg-vsp)
    (%value-cell-header-at-p arg-vsp)
    (or (and (> arg-vsp vsp)
             (%value-cell-header-at-p (the fixnum (1- arg-vsp))))
        (let ((next-vsp (1+ arg-vsp)))
          (declare (fixnum next-vsp))
          (and (< next-vsp parent-vsp)
               (%value-cell-header-at-p next-vsp))))))



(defun count-values-in-frame (p context &optional child)
  (declare (ignore child))
  (multiple-value-bind (vsp parent-vsp) (vsp-limits p context)
    (values
     (- parent-vsp 
        vsp
        (* 2 (count-db-links-in-frame vsp parent-vsp context))))))

(defun nth-value-in-frame-loc (sp n context lfun pc vsp parent-vsp)
  (declare (fixnum sp))
  (setq n (require-type n 'fixnum))
  (unless (or (null vsp) (fixnump vsp))
    (setq vsp (require-type vsp '(or null fixnum))))
  (unless (or (null parent-vsp) (fixnump parent-vsp))
    (setq parent-vsp (require-type parent-vsp '(or null fixnum))))
  (unless (and vsp parent-vsp)
    (multiple-value-setq (vsp parent-vsp) (vsp-limits sp context)))
  (locally (declare (fixnum n vsp parent-vsp))
    (multiple-value-bind (db-count first-db last-db)
                         (count-db-links-in-frame vsp parent-vsp context)
      (declare (ignore db-count))
      (declare (fixnum first-db last-db))
      (let ((arg-vsp (1- parent-vsp))
            (cnt n)
            (phys-cell 0)
            db-link-p)
        (declare (fixnum arg-vsp cnt phys-cell))
        (loop
          (if (eql (the fixnum (- arg-vsp 2)) last-db)
            (setq db-link-p t
                  arg-vsp last-db
                  last-db (previous-db-link last-db first-db)
                  phys-cell (+ phys-cell 2))
            (setq db-link-p nil))
            (when (< (decf cnt) 0)
              (return
               (if db-link-p
                 (values (+ 2 arg-vsp)
                         :saved-special
                         (binding-index-symbol (%fixnum-ref (1+ arg-vsp))))
                 (multiple-value-bind (type name) (find-local-name phys-cell lfun pc)
                   (values arg-vsp type name)))))
          (incf phys-cell)
          (when (< (decf arg-vsp) vsp)
            (error "~d out of range" n)))))))



(defun nth-value-in-frame (sp n context &optional lfun pc vsp parent-vsp)
  (multiple-value-bind (loc type name)
                       (nth-value-in-frame-loc sp n context lfun pc vsp parent-vsp)
    (let* ((val (%fixnum-ref loc)))
      (when (and (eq type :saved-special)
		 (eq val (%no-thread-local-binding-marker))
		 name)
	(setq val (%sym-global-value name)))
      (values val  type name))))

(defun set-nth-value-in-frame (sp n context new-value &optional vsp parent-vsp)
  (multiple-value-bind (loc type name)
      (nth-value-in-frame-loc sp n context nil nil vsp parent-vsp)
    (let* ((old-value (%fixnum-ref loc)))
      (if (and (eq type :saved-special)
	       (eq old-value (%no-thread-local-binding-marker))
	       name)
	;; Setting the (shallow-bound) value of the outermost
	;; thread-local binding of NAME.  Hmm.
	(%set-sym-global-value name new-value)
	(setf (%fixnum-ref loc) new-value)))))

(defun nth-raw-frame (n start-frame context)
  (declare (fixnum n))
  (do* ((p start-frame (parent-frame p context))
	(i 0 (1+ i))
	(q (last-frame-ptr context)))
       ((or (null p) (eq p q) (%stack< q p context)))
    (declare (fixnum i))
    (if (= i n)
      (return p))))

;;; True if the object is in one of the heap areas
(defun %in-consing-area-p (x area)
  (declare (optimize (speed 3) (safety 0)) (fixnum x))       ; lie
  (let* ((low (%fixnum-ref area target::area.low))
         (high (%fixnum-ref area target::area.high))
)
    (declare (fixnum low high))
    (and (<= low x) (< x high))))



(defun in-any-consing-area-p (x)
  (do-consing-areas (area)
    (when (%in-consing-area-p x area)
      (return t))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; terminate-when-unreachable
;;;

#|
Message-Id: <v02130502ad3e6a2f1542@[205.231.144.48]>
Mime-Version: 1.0
Content-Type: text/plain; charset="us-ascii"
Date: Wed, 7 Feb 1996 10:32:55 -0500
To: pmcldev@digitool.com
From: bitCraft@taconic.net (Bill St. Clair)
Subject: terminate-when-unreachable

I propose that we add a general termination mechanism to PPC MCL.
We need it to properly terminate stack groups, it would be
a nicer way to do the termination for macptrs than the current
ad-hoc mechanism (which BTW is not yet part of PPC MCL), and
it is a nice addition to MCL. I don't think it's hard to make
the garbage collector support this, and I volunteer to do the
work unless Gary really wants to.

I see two ways to support termination:

1) Do termination for hash tables. This was our plan for
   2.0, but Gary got confused about how to mark the objects at
   the right time (or so I remember).

2) Resurrect weak alists (they're not part of the PPC garbage
   collector) and add a termination bit to the population type.
   This allows for termination of weak lists and weak alists,
   though the termination mechanism really only needs termination
   for a single weak alist.

I prefer option 2, weak alists, since it avoids the overhead
necessary to grow and rehash a hash table. It also uses less space,
since a finalizeable hash table needs to allocate two cons cells
for each entry so that the finalization code has some place to
put the deleted entry.

I propose the following interface (slightly modified from what
Apple Dylan provides):

terminate-when-unreachable object &optional (function 'terminate)
  When OBJECT becomes unreachable, funcall FUNCTION with OBJECT
  as a single argument. Each call of terminate-when-unreachable
  on a single (EQ) object registers a new termination function.
  All will be called when the object becomes unreachable.

terminate object                                         [generic function]
  The default termination function

terminate (object t)                                     [method]
  The default method. Ignores object. Returns nil.

drain-termination-queue                                  [function]
  Drain the termination queue. I.e. call the termination function
  for every object that has become unreachable.

*enable-automatic-termination*                           [variable]
  If true, the default, drain-termination-queue will be automatically
  called on the first event check after the garbage collector runs.
  If you set this to false, you are responsible for calling
  drain-termination-queue.

cancel-terminate-when-unreachable object &optional function
  Removes the effect of the last call to terminate-when-unreachable
  for OBJECT & FUNCTION (both tested with EQ). Returns true if
  it found a match (which it won't if the object has been moved
  to the termination queue since terminate-when-unreachable was called).
  If FUNCTION is NIL or unspecified, then it will not be used; the
  last call to terminate-when-unreachable with the given OBJECT will
  be undone.

termination-function object
  Return the function passed to the last call of terminate-when-unreachable
  for OBJECT. Will be NIL if the object has been put in the
  termination queue since terminate-when-unreachable was called.

|#


(defstatic *termination-population*
  (%cons-terminatable-alist))

(defstatic *termination-population-lock* (make-lock))


(defvar *enable-automatic-termination* t)

(defun terminate-when-unreachable (object &optional (function 'terminate))
  "The termination mechanism is a way to have the garbage collector run a
function right before an object is about to become garbage. It is very
similar to the finalization mechanism which Java has. It is not standard
Common Lisp, although other Lisp implementations have similar features.
It is useful when there is some sort of special cleanup, deallocation,
or releasing of resources which needs to happen when a certain object is
no longer being used."
  (let ((new-cell (cons object function))
        (population *termination-population*))
    (without-interrupts
     (with-lock-grabbed (*termination-population-lock*)
       (atomic-push-uvector-cell population population.data new-cell)))
    function))

(defmethod terminate ((object t))
  nil)

(defun drain-termination-queue ()
  (with-lock-grabbed (*termination-population-lock*)
    (let* ((population *termination-population*))
      (loop
        (multiple-value-bind (cell existed)
            (atomic-pop-uvector-cell population population.termination-list)
          (if (not existed)
            (return)
          (funcall (cdr cell) (car cell))))))))

(defun cancel-terminate-when-unreachable (object &optional (function nil function-p))
  (let* ((found nil))
    (with-lock-grabbed (*termination-population-lock*)
      ;; Have to defer GCing, e.g., defer responding to a GC
      ;; suspend request here (that also defers interrupts)
      ;; We absolutely, positively can't take an exception
      ;; in here, so don't even bother to typecheck on 
      ;; car/cdr etc.
      (with-deferred-gc
          (do ((spine (population-data *termination-population*) (cdr spine))
               (prev nil spine))
              ((null spine))
            (declare (optimize (speed 3) (safety 0)))
            (let* ((head (car spine))
                   (tail (cdr spine))
                   (o (car head))
                   (f (cdr head)))
              (when (and (eq o object)
                         (or (null function-p)
                             (eq function f)))
                (if prev
                  (setf (cdr prev) tail)
                  (setf (population-data *termination-population*) tail))
                (setq found t)
                (return)))))
      found)))


(defun termination-function (object)
  (without-interrupts
   (with-lock-grabbed (*termination-population-lock*)
     (cdr (assq object (population-data *termination-population*))))))

(defun do-automatic-termination ()
  (when *enable-automatic-termination*
    (drain-termination-queue)))

(queue-fixup
 (add-gc-hook 'do-automatic-termination :post-gc))

;;; A callback to handle foreign thread preparation, initialization,
;;; and termination.
;;; "preparation" involves telling the kernel to reserve space for
;;; some initial thread-specific special bindings.  The kernel
;;; needs to reserve this space on the foreign thread's vstack;
;;; it needs us to tell it how much space to reserve (enough
;;; for bindings of *current-thread*, *current-process*, and
;;; the default initial bindings of *PACKAGE*, etc.)
;;;
;;; "initialization" involves making those special bindings in
;;; the vstack space reserved by the kernel, and setting the
;;; values of *current-thread* and *current-process* to newly
;;; created values.
;;;
;;; "termination" involves removing the current thread and
;;; current process from the global thread/process lists.
;;; "preparation" and "initialization" happen when the foreign
;;; thread first tries to call lisp code.  "termination" happens
;;; via the pthread thread-local-storage cleanup mechanism.
(defcallback %foreign-thread-control (:without-interrupts t :int param :int)
  (declare (fixnum param))
  (cond ((< param 0) (%foreign-thread-prepare))
	((= param 0) (%foreign-thread-initialize) 0)
	(t (%foreign-thread-terminate) 0)))



(defun %foreign-thread-prepare ()
  (let* ((initial-bindings (standard-initial-bindings)))
    (%save-standard-binding-list initial-bindings)
    (* 3 (+ 2 (length initial-bindings)))))


(defun %foreign-thread-initialize ()
  ;; Recover the initial-bindings alist.
  (let* ((bsp (%saved-bindings-address))
	 (initial-bindings (%fixnum-ref bsp )))
    (declare (fixnum bsp))
    ;; Um, this is a little more complicated now that we use
    ;; thread-local shallow binding
    (flet ((save-binding (new-value sym prev)
             (let* ((idx (symbol-binding-index sym))
                    (byte-idx (ash idx target::fixnum-shift))
                    (binding-vector (%fixnum-ref (%current-tcr) target::tcr.tlb-pointer))
                    (old-value (%fixnum-ref  binding-vector byte-idx)))
	     (setf (%fixnum-ref binding-vector byte-idx) new-value
                   (%fixnum-ref bsp (ash -1 target::word-shift)) old-value
		   (%fixnum-ref bsp (ash -2 target::word-shift)) idx
		   (%fixnum-ref bsp (ash -3 target::word-shift)) prev
		   bsp (- bsp 3)))))
      (save-binding nil '*current-lisp-thread* 0)
      (save-binding nil '*current-process* bsp)
      (dolist (pair initial-bindings)
	(save-binding (funcall (cdr pair)) (car pair) bsp))
      ;; These may (or may not) be the most recent special bindings.
      ;; If they are, just set the current tcr's db-link to point
      ;; to BSP; if not, "append" them to the end of the current
      ;; linked list.
      (let* ((current-db-link (%fixnum-ref (%current-tcr) target::tcr.db-link)))
        (declare (fixnum current-db-link))
        (if (zerop current-db-link)
          (setf (%fixnum-ref (%current-tcr) target::tcr.db-link) bsp)
          (do* ((binding current-db-link)
                (next (%fixnum-ref binding 0)
                      (%fixnum-ref binding 0)))
               ()
            (if (zerop next)
              (return (setf (%fixnum-ref binding 0) bsp))
              (setq binding next)))))
      ;; Ensure that pending unwind-protects (for WITHOUT-INTERRUPTS
      ;; on the callback) don't try to unwind the binding stack beyond
      ;; where it was just set.
      (do* ((catch (%fixnum-ref (%current-tcr) target::tcr.catch-top)
                   (%fixnum-ref catch target::catch-frame.link)))
           ((zerop catch))
        (declare (fixnum catch))
        (when (eql 0 (%fixnum-ref catch target::catch-frame.db-link))
          (setf (%fixnum-ref catch target::catch-frame.db-link) bsp)))))
  (let* ((thread (new-lisp-thread-from-tcr (%current-tcr) "foreign")))
    (setq *current-lisp-thread* thread
	  *current-process*
	  (make-process "foreign" :thread thread)
          *whostate* "Foreign thread callback")))
    
;;; Remove the foreign thread's lisp-thread and lisp process from
;;; the global lists.
(defun %foreign-thread-terminate ()
  (let* ((proc *current-process*))
    (when proc
      (remove-from-all-processes proc)
      (let* ((ts (process-termination-semaphore proc)))
        (when ts (signal-semaphore ts))))))

