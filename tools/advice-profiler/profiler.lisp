;;; -*- Lisp -*-

;;;   Copyright (c) 2008, Hans Huebner.
;;;   This file is part of Clozure CL.  

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

;;; Deterministic profiler for Clozure CL

;;; Inspired by the public domain profiler written by Mark Kantrowitz

;;; To get accurate profiling results, you need to make sure that your
;;; processor runs at full speed.  Modern CPUs adjust their CPU clock
;;; dynamically, which will have negative effects on accuracy of the
;;; profiling result.

;;; In virtual machines, the profiling results may also be inaccurate
;;; due to virtualized timers.

;;; Bottom line: Always try to profile on the bare metal, with all
;;; power saving techniques switched off.  Repeat your profiling to get
;;; a feel for the precision of the results.

;;; The code has provisions for measuring CPU time in addition to real
;;; time, but it seems that no operating system can deliver consistent
;;; and accurate per-thread CPU time usage.

;;; All clock values that are handled by this profiler are specified
;;; in nanoseconds.  This means that it will use bignums on 32 bit
;;; platforms.

(in-package "PROFILER")

(defvar *profiler-loaded* nil)
 
(eval-when (:load-toplevel :execute)
  (when (and (not *profiler-loaded*)
             (> (length (all-processes)) 2))
    (error "Profiler can't be loaded with active application threads.  Please load from ccl-init.lisp"))
  (setf *profiler-loaded* t))

;;; Process specific variables
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defvar-process-specific (var init doc)
    `(progn
       (defvar ,var ,init ,doc)
       (ccl::def-standard-initial-binding ,var ,init))))

(defvar-process-specific *process-profile-results* nil
  "Variable to hold profiling information, in a hash table.  If NIL,
   profiling is disabled for this process.")

(defvar-process-specific *total-cpu-time* 0
  "Amount of CPU time used by profiled functions so far")
(defvar-process-specific *total-real-time* 0
  "Amount of real time used by profiled functions so far")
(defvar-process-specific *total-cons* 0
  "Amount of consing in profiled functions so far")
(defvar-process-specific *total-calls* 0
  "Number of calls to profiled functions so far")

;;; Global variables

;;; Profiler overhead is determined by the function DETERMINE-OVERHEAD

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *real-overhead* 0 "Real time overhead")
  (defvar *cpu-overhead* 0 "CPU time overhead")
  (defvar *cons-overhead* 0 "Consing overhead (additional consing per call)"))

(defvar *profiled-functions* nil
  "List of function names that are currently being profiled.")

(defvar *clock-errors* nil
  "This flag indicates that the CPU and real time clock have shown to be inconsistent.  A warning
   will be printed in the report if this is found to be true.")

(defparameter *profile-new-processes* t
  "This flag indicates that profiling should automatically be enabled for new processes that are
   created.  If it nil, no call recording takes place for processes until after
   PROCESS-ENABLE-PROFILING has been called for them.")

(defvar *profiler-lock* (make-lock "Profiler lock")
  "Lock to guard accesses to global profiler structures")

(defmacro with-profiler-locked (() &body body)
  `(with-lock-grabbed (*profiler-lock*)
     ,@body))

(defun timespec->nanoseconds (ts)
  "Convert the given typespec structure into nanoseconds."
  (+ (* 1000000000 (pref ts :timespec.tv_sec))
     (pref ts :timespec.tv_nsec)))
(declaim (inline timespec->nanoseconds))

;;; Clock handling

;;; For Darwin, we use the Mach clock service

#+darwin
(let ((clock-port (make-record :clock_serv_t)))
  (#_host_get_clock_service (#_mach_host_self) #$REALTIME_CLOCK clock-port)
  (defun get-real-time ()
    (ccl:rlet ((ts :mach_timespec_t))
      (#_clock_get_time (%get-ptr clock-port) ts)
      (timespec->nanoseconds ts))))

;;; For non-Darwin platforms, we use clock_gettime() with the
;;; CLOCK_MONOTONIC clock.

#-darwin
(defun get-posix-clock (id)
  (ccl:rlet ((ts :timespec))
    (unless (zerop (#_clock_gettime id ts))
      (error "error reading clock ~A: ~A~%" id (ccl::%strerror (ccl::%get-errno))))
    (timespec->nanoseconds ts)))
(declaim (inline get-posix-clock))

#-darwin
(defun get-real-time ()
  (get-posix-clock #$CLOCK_MONOTONIC))

;;; Per-thread CPU time measurement is only available on Linux

(defun get-cpu-time ()
  #+linux-target
  (get-posix-clock #$CLOCK_THREAD_CPUTIME_ID)
  #-linux-target
  0)

(defparameter *can-report-cpu* #+linux-target t #-linux-target nil)

(defun get-cons ()
  (ccl::total-bytes-allocated))

(declaim (inline get-cpu-time get-real-time get-cons))

;;; Helper macro to measure elapsed time

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-real/cpu/cons ((delta-real delta-cpu delta-cons &key adjusted) form &body post-process)
    "Evaluate FORM, then run POST-PROCESS with DELTA-REAL, DELTA-CPU and
   DELTA-CONS bound to the elapsed real time, elapsed CPU time and
   amount of consing that happened in FORM.  If ADJUSTED is non-nil,
   the values are adjusted by the overhead values."
    (let ((start-real (gensym "START-REAL-"))
          (start-cpu (gensym "START-CPU-"))
          (start-cons (gensym "START-CONS-")))
      `(let ((,start-real (get-real-time))
             (,start-cpu (get-cpu-time))
             (,start-cons (get-cons)))
         (declare (type fixnum ,start-real ,start-cpu ,start-cons))
         (multiple-value-prog1 ,form
           (let ((,delta-real (- (get-real-time) ,start-real ,@(when adjusted (list '*real-overhead*))))
                 (,delta-cpu (- (get-cpu-time) ,start-cpu ,@(when adjusted (list '*cpu-overhead*))))
                 (,delta-cons (- (get-cons) ,start-cons ,@(when adjusted (list '*cons-overhead*)))))
             (declare (type fixnum ,delta-real ,delta-cpu ,delta-cons))
             ;; If there is clock imprecision, we can end up with
             ;; negative delta values here.  For now, we just make
             ;; sure that we never pass negative deltas back to the
             ;; reporting code, but it may be preferable to take a
             ;; note of such events and mark the report as being
             ;; questionable.
             (when (minusp ,delta-real) (setf ,delta-real 0))
             (when (minusp ,delta-cpu) (setf ,delta-cpu 0))
             (when (minusp ,delta-cons) (setf ,delta-cons 0))
             (when (>= ,delta-real ,delta-cpu)
               (setf *clock-errors* t))
             ,@post-process))))))

;;; Formatting

(defun format-time (nanoseconds)
  "Given a time in NANOSECONDS, return a human readable string with
   the time scaled.  Times shorter than a second are reported with the
   proper sub-second unit (ns, us, ms), times longer than a second are
   reported in wall clock format (HH:MM:SSh)."
  (cond
    ((> 1000 nanoseconds)
     (format nil "~Ans" (floor nanoseconds)))
    ((> 1000000 nanoseconds)
     (format nil "~,1Fus" (/ nanoseconds 1000)))
    ((> 1000000000 nanoseconds)
     (format nil "~,1Fms" (/ nanoseconds 1000000)))
    ((> 100000000000 nanoseconds)
     (format nil "~,1Fs " (/ nanoseconds 1000000000)))
    (t
     (let* ((seconds (floor nanoseconds 1000000000))
            (minutes (floor seconds 60))
            (hours (floor minutes 60)))
       (format nil "~A:~2,'0D:~2,'0Dh"
               hours
               (- minutes (* 60 hours))
               (- seconds (* 60 minutes)))))))
               
;; For each profiled function that is called within a process, a
;; function-call-record structure is created that carries the counters
;; and timing information.

(defstruct (function-call-record
             (:conc-name fcr-)
             (:constructor make-function-call-record%))
  (process *current-process* :type process)
  (name nil :type symbol)
  (inclusive-real-time 0 :type fixnum)
  (exclusive-real-time 0 :type fixnum)
  (inclusive-cpu-time 0 :type fixnum)
  (exclusive-cpu-time 0 :type fixnum)
  (inclusive-cons 0 :type fixnum)
  (exclusive-cons 0 :type fixnum)
  (calls 0 :type fixnum)
  (nested-calls 0 :type fixnum))

(defun make-function-call-record (name)
  "Create a function-call-record structure for the function named NAME.  The
   current process is written into the structure created for later
   analysis."
  (let ((fcr (make-function-call-record%)))
    (setf (fcr-name fcr) name)
    fcr))

(defun sum-fcrs (fcrs)
  (let ((sum (make-function-call-record (fcr-name (first fcrs)))))
    (setf (fcr-inclusive-real-time sum) (reduce #'+ fcrs :key #'fcr-inclusive-real-time)
          (fcr-exclusive-real-time sum) (reduce #'+ fcrs :key #'fcr-exclusive-real-time)
          (fcr-inclusive-cpu-time sum) (reduce #'+ fcrs :key #'fcr-inclusive-cpu-time)
          (fcr-exclusive-cpu-time sum) (reduce #'+ fcrs :key #'fcr-exclusive-cpu-time)
          (fcr-inclusive-cons sum) (reduce #'+ fcrs :key #'fcr-inclusive-cons)
          (fcr-exclusive-cons sum) (reduce #'+ fcrs :key #'fcr-exclusive-cons)
          (fcr-calls sum) (reduce #'+ fcrs :key #'fcr-calls)
          (fcr-nested-calls sum) (reduce #'+ fcrs :key #'fcr-nested-calls))
    sum))

(defmacro profile (&rest names)
  "Profile the functions named by NAMES.  As in TRACE, the names are
   not evaluated.  Strings and keywords are interpreted as package
   designators and will cause all functions named by external symbols
   in the package to be profiled.  If a function is already profiled,
   then unprofile and reprofile (useful to notice function
   redefinition).  If a name is undefined, give a warning and ignore
   it."
  `(progn
     (let (new-names)
       ,@(mapcar
          (lambda (name)
            (if (or (stringp name)
                    (keywordp name))
              `(setf new-names (append (profile-package ,name :external-only t) new-names))
              `(with-profiler-locked ()
                 (if (find ',name *profiled-functions*)
                   (unprofile ,name)
                   (push ',name new-names))
                 (cond
                   ((not (fboundp ',name))
                    (warn "ignored argument ~A, which is not the name of a function" ',name))
                   (t
                    (ccl:advise ,name
                                (progn
                                  (when (and (null *process-profile-results*)
                                             *profile-new-processes*)
                                    (setf *process-profile-results* (make-hash-table)))
                                  (if *process-profile-results*
                                    (let ((prev-cpu-time *total-cpu-time*)
                                          (prev-real-time *total-real-time*)
                                          (prev-cons *total-cons*)
                                          (prev-calls *total-calls*))
                                      (declare (type fixnum prev-cpu-time prev-real-time prev-cons prev-calls))
                                      (with-real/cpu/cons (delta-real delta-cpu delta-cons :adjusted t)
                                          (:do-it)
                                        (multiple-value-bind (fcr presentp)
                                            (gethash ',name *process-profile-results*)
                                          (unless presentp
                                            (setf fcr (make-function-call-record ',name))
                                            (setf (gethash ',name *process-profile-results*) fcr))
                                          ;; Call counters
                                          (incf *total-calls*)
                                          (incf (fcr-calls fcr))
                                          (incf (fcr-nested-calls fcr) (- *total-calls* prev-calls))
                                          ;; Real time
                                          (incf (fcr-inclusive-real-time fcr) delta-real)
                                          (incf (fcr-exclusive-real-time fcr) (- delta-real
                                                                                 (- *total-real-time* prev-real-time)))
                                          (setf *total-real-time* (+ delta-real prev-real-time))
                                          ;; CPU time
                                          (incf (fcr-inclusive-cpu-time fcr) delta-cpu)
                                          (incf (fcr-exclusive-cpu-time fcr) (- delta-cpu
                                                                                (- *total-cpu-time* prev-cpu-time)))
                                          (setf *total-cpu-time* (+ delta-cpu prev-cpu-time))
                                          ;; consing
                                          (incf (fcr-inclusive-cons fcr) delta-cons)
                                          (incf (fcr-exclusive-cons fcr) (- delta-cons
                                                                            (- *total-cons* prev-cons)))
                                          (setf *total-cons* (+ delta-cons prev-cons)))))
                                    (:do-it)))
                                :when :around)
                    (pushnew ',name *profiled-functions*))))))
          names)
       new-names)))

(defun symbol-external-p (symbol)
  "Return non-nil if the SYMBOL is external in its package (being
exported)."
  (eq :external (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol)))))

(defun functions-in-package (package external-only)
  "Return the list of symbols in PACKAGE that have a function bound to
   them.  If EXTERNAL-ONLY is true, only returns those symbols that
   are external."
  (let ((package (if (packagep package)
                   package
                   (or (find-package package)
                       (error "package ~S not found" package))))
        symbols)
    (do-symbols (symbol package)
      (when (and (fboundp symbol)
                 (not (macro-function symbol))
                 (eq (symbol-package symbol) package)
                 (or (not external-only)
                     (symbol-external-p symbol)))
        (pushnew symbol symbols)))
    symbols))

;;; Per-process profiling API

(defmacro within-process ((process) &body body)
  "Run BODY within PROCESS, using PROCESS-INTERRUPT."
  (let ((sem (gensym "SEM-")))
    `(let ((,sem (make-semaphore)))
       (process-interrupt ,process
                          (lambda ()
                            (unwind-protect
                                 (progn ,@body)
                              (signal-semaphore, sem))))
       (wait-on-semaphore ,sem))))

(defun process-enable-profiling (&optional (process *current-process*))
  "Enable profiling for the given process."
  (within-process (process)
    (unless *process-profile-results*
      (setf *process-profile-results* (make-hash-table)))))

(defun process-disable-profiling (&optional (process *current-process*))
  "Disable profiling for the given process."
  (within-process (process)
    (setf *process-profile-results* nil)))

(defun enable-profiling ()
  "Enable profiling in all current and future processes."
  (dolist (process (all-processes))
    (process-enable-profiling process))
  (setf *profile-new-processes* t))

(defun disable-profiling ()
  "Disable profiling in all current and future processes."
  (dolist (process (all-processes))
    (process-enable-profiling process))
  (setf *profile-new-processes* nil))

;;; Global profiling API

(defmacro profile-package (&optional (package *package*) &key external-only)
  "Profile all functions in the specified package."
  `(profile ,@(functions-in-package package external-only)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro unprofile (&rest names)
    "Unprofile the functions named by NAMES.  If an argument is a
     keyword or a string, it is considered to be a package name and
     all (!) symbols in the package will be unprofiled."
    `(with-profiler-locked ()
       ,@(mapcar (lambda (name)
                   (if (or (stringp name)
                           (keywordp name))
                       `(unprofile-package ,name)
                       `(ccl:unadvise ,name)))
                 names)
       (setf *profiled-functions* (set-difference *profiled-functions* ',names)))))

(defmacro unprofile-all ()
  "Unprofile all functions that are currently profiled."
  `(with-profiler-locked ()
     ,@(mapcar (lambda (name) `(ccl:unadvise ,name)) *profiled-functions*)
     (setf *profiled-functions* nil)))

(defmacro unprofile-package (&optional (package *package*) &key external-only)
  "Profile all functions in the specified PACKAGE, which may be either
   a string, a keyword symbol or a package instance.  If EXTERNAL-ONLY
   is t, only functions named by symbols which are external in PACKAGE
   are unprofiled."
  `(unprofile ,@(functions-in-package package external-only)))

(defun reset ()
  "Reset profiling information in all processes."
  (setf *total-cpu-time* 0
        *total-real-time* 0
        *total-cons* 0
        *total-calls* 0)
  (dolist (process (all-processes))
    (within-process (process)
      (when *process-profile-results*
        (clrhash *process-profile-results*)))))

(defun collect-profiling-results (&key reset)
  "Collect profiling results from all processes.  If RESET is true,
   the profiling results are cleared when they have been read."
  (let (results)
    (dolist (process (all-processes))
      (within-process (process)
        (when *process-profile-results*
          (with-profiler-locked ()
            (maphash (lambda (key value)
                       (declare (ignore key))
                       (push value results))
                     *process-profile-results*))
          (when reset
            (clrhash *process-profile-results*)))))
    results))

;; Reporting

(defun write-results-xml (&key (stream *standard-output*))
  "Write the profiling results to the given STREAM in an XML format,
   one 'entry' element for each function call record that has been
   collected."
  (format stream "<profile-results>~%")
  (dolist (fcr (collect-profiling-results))
    (format stream "  <entry function='~S' ~
                             process='~A' ~
                             inclusive-real-time='~A' exclusive-real-time='~A' ~
                             inclusive-cpu-time='~A' exclusive-cpu-time='~A' ~
                             inclusive-cons='~A' exclusive-cons='~A' ~
                             calls='~A' nested-calls='~A'/>~%"
            (fcr-name fcr)
            (process-name (fcr-process fcr))
            (fcr-inclusive-real-time fcr) (fcr-exclusive-real-time fcr)
            (fcr-inclusive-cpu-time fcr) (fcr-exclusive-cpu-time fcr)
            (fcr-inclusive-cons fcr) (fcr-exclusive-cons fcr)
            (fcr-calls fcr) (fcr-nested-calls fcr)))
  (format stream "</profile-results>~%"))

(defun write-results-csv (&key (stream *standard-output*))
  "Write the profiling results to the given STREAM in a CSV format
   which can be imported into excel for further analysis."
  (format stream "package;function;process;inclusive-real-time;exclusive-real-time;inclusive-cpu-time;exclusive-cpu-time;~
                  inclusive-cons;exclusive-cons;calls;nested-calls~%")
  (dolist (fcr (collect-profiling-results))
    (format stream "\"~S\";\"~A\";~A;~A;~A;~A;~A;~A;~A;~A~%"
            (fcr-name fcr)
            (process-name (fcr-process fcr))
            (fcr-inclusive-real-time fcr) (fcr-exclusive-real-time fcr)
            (fcr-inclusive-cpu-time fcr) (fcr-exclusive-cpu-time fcr)
            (fcr-inclusive-cons fcr) (fcr-exclusive-cons fcr)
            (fcr-calls fcr) (fcr-nested-calls fcr))))

(defstruct (profile-results
            (:conc-name pr-)
            (:constructor make-profile-results
                          (name process
                                calls
                                real-time cpu-time cons
                                percent-real-time percent-cpu-time percent-cons)))
  name
  process
  calls
  real-time
  cpu-time
  cons
  percent-real-time
  percent-cpu-time
  percent-cons)

(defun group-on (list &key (test #'eql) (key #'identity) (include-key t))
  (let ((hash (make-hash-table :test test))
        keys)
    (dolist (el list)
      (let ((key (funcall key el)))
        (unless (nth-value 1 (gethash key hash))
          (push key keys))
        (push el (gethash key hash))))    
    (mapcar (lambda (key) (let ((keys (nreverse (gethash key hash))))
                            (if include-key
                                (cons key keys)
                                keys)))
            (nreverse keys))))

(defun get-postprocessed (&key (by-process t) reset sort-by)
  "Collect profiling results from all processes and compress them for
   display, return a list of lists of PROFILE-RESULTS.  BY-PROCESS
   determines whether the report will be per process or one report for
   all processes combined."
  (labels
      ((percentage (value total)
         (if (and (plusp value) (plusp total))
           (/ value (/ total 100))
           0))
       (postprocess (records)
         (let ((total-real-time 0)
               (total-cpu-time 0)
               (total-cons 0))
           (dolist (record records)
             (incf total-real-time (fcr-exclusive-real-time record))
             (incf total-cpu-time (fcr-exclusive-cpu-time record))
             (incf total-cons (fcr-exclusive-cons record)))
           (sort (mapcar (lambda (fcr)
                           (make-profile-results (let ((*package* (find-package :keyword)))
                                                   (prin1-to-string (fcr-name fcr)))
                                                 (fcr-process fcr)
                                                 (fcr-calls fcr)
                                                 (fcr-exclusive-real-time fcr)
                                                 (fcr-exclusive-cpu-time fcr)
                                                 (fcr-exclusive-cons fcr)
                                                 (percentage (fcr-exclusive-real-time fcr) total-real-time)
                                                 (percentage (fcr-exclusive-cpu-time fcr) total-cpu-time)
                                                 (percentage (fcr-exclusive-cons fcr) total-cons)))
                         records)
                 #'<
                 :key sort-by))))
    (if by-process
      (mapcar #'postprocess
              (group-on (collect-profiling-results :reset reset)
                        :key #'fcr-process
                        :test #'eq
                        :include-key nil))
      (list (postprocess (mapcar #'sum-fcrs (group-on (collect-profiling-results :reset reset)
                                                      :key #'fcr-name
                                                      :test #'eq
                                                      :include-key nil)))))))

(defun sort-key-function (keyword)
  (let ((valid-sort-keys (append '(:calls
                                   :cons :percent-cons
                                   :real-time :percent-real-time)
                                 (when *can-report-cpu*
                                   '(:cpu-time :percent-cpu-time)))))
    (unless (member keyword valid-sort-keys)
      (error "invalid sort key ~S, specify one of ~S"
             keyword valid-sort-keys))
    (fdefinition (find-symbol (format nil "PR-~A" keyword) :profiler))))

(defun report (&key
               (threshold 0.01)
               (by-process t)
               (stream *trace-output*)
               (report-cpu *can-report-cpu*)
               (sort-by (if *can-report-cpu* :cpu-time :real-time))
               report-overhead)
  (labels
      ((do-report (records)
         (let ((max-length 8)           ; Function header size
               (max-cons-length 8)
               (max-colon-pos 0)        ; Maximum offset of a colon in any name
               (total-real-time 0)
               (total-cpu-time 0)
               (total-consed 0)
               (total-calls 0)
               (total-percent-real-time 0)
               (total-percent-cpu-time 0)
               (total-percent-cons 0))
           (dolist (result records)
             (when (or (zerop threshold)
                       (> (pr-percent-real-time result) threshold))
               (setq max-colon-pos
                     (max max-colon-pos
                          (position #\: (pr-name result))))
               (setq max-length
                     (max max-length
                          (length (pr-name result))))
               (setq max-cons-length
                     (max max-cons-length
                          (/ (pr-cons result) (pr-calls result))))))
           (incf max-length 2)
           (setf max-cons-length (max 4 (ceiling (log max-cons-length 10))))
           (format stream
                   "~
             ~&   %      ~@[~* %      ~]                          ~@[~*          ~]~V@A~
	     ~%  Real    ~@[~*CPU     ~] %             Real Time  ~@[~*CPU Time  ~]~V@A      Total  ~@[~*   Total  ~]     Total~
	     ~%  Time    ~@[~*Time    ~]Cons    Calls      /Call  ~@[~*   /Call  ~]~V@A  Real Time  ~@[~*CPU Time  ~]      Cons  Name~
             ~%~V,,,'-A"
                   report-cpu report-cpu max-cons-length "Cons"
                   report-cpu report-cpu max-cons-length "Per"  report-cpu
                   report-cpu report-cpu max-cons-length "Call" report-cpu
                   (+ max-length (if report-cpu 92 64) (max 0 (- max-cons-length 5))) "-")
           (dolist (result records)
             (when (or (zerop threshold)
                       (> (pr-percent-real-time result) threshold))
               (format stream
                       "~%~6,2F  ~@[~6,2F  ~]~6,2F  ~7D   ~8@A  ~@[~8@A  ~]~VD   ~8@A  ~@[~8@A  ~]~10D  ~V@A"
                       (pr-percent-real-time result)
                       (and report-cpu (pr-percent-cpu-time result))
                       (pr-percent-cons result)
                       (pr-calls result)
                       (format-time (/ (pr-real-time result) (pr-calls result)))
                       (and report-cpu (format-time (/ (pr-cpu-time result) (pr-calls result))))
                       max-cons-length
                       (floor (/ (pr-cons result) (pr-calls result)))
                       (format-time (pr-real-time result))
                       (and report-cpu (format-time (pr-cpu-time result)))
                       (pr-cons result)
                       (+ (length (pr-name result))
                          (- max-colon-pos (position #\: (pr-name result))))
                       (pr-name result))
               (incf total-real-time (pr-real-time result))
               (incf total-cpu-time (pr-cpu-time result))
               (incf total-consed (pr-cons result))
               (incf total-calls (pr-calls result))
               (incf total-percent-real-time (pr-percent-real-time result))
               (incf total-percent-cpu-time (pr-percent-cpu-time result))
               (incf total-percent-cons (pr-percent-cons result))))
           (format stream
                   "~%~V,,,'-A~
	    ~%~6,2F  ~@[~6,2F  ~]~6,2F  ~7D  ~9@T ~VA~@[~*          ~]    ~8@A~@[  ~8@A~]  ~10D~%"
                   (+ max-length (if report-cpu 92 64) (max 0 (- max-cons-length 5))) "-"
                   total-percent-real-time
                   (and report-cpu total-percent-cpu-time)
                   total-percent-cons
                   total-calls
                   max-cons-length " "
                   report-cpu
                   (format-time total-real-time)
                   (and report-cpu (format-time total-cpu-time))
                   total-consed)
           (when report-overhead
             (format stream "Estimated monitoring overhead: real: ~A cons: ~A~%"
                   (format-time (* *real-overhead* total-calls))
                   (* *cons-overhead* total-calls)))
           (terpri stream)
           (values))))
    (dolist (results (get-postprocessed :by-process by-process
                                        :sort-by (sort-key-function sort-by)))
      (if by-process
          (format stream "Profile results for process ~A~%~%" (pr-process (car results)))
          (format stream "Profile results combined for all profiledy processes~%~%"))
      (do-report results))
    (when (and *can-report-cpu* *clock-errors*)
      (format stream "Warning: real time and CPU time clocks are inconsistent.~%"))))