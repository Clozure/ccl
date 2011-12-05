;;;   Copyright (C) 2011 Clozure Associates
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
;;;
;;;   Implement a protocol (originally based on swank) for communication between
;;;   a lisp and an external debugger.  This implements the server side, i.e. the lisp
;;;   being debugged.

(eval-when (eval compile load)
  (defpackage :swink
    (:use :cl :ccl)
    (:export
     "START-SERVER"
     "STOP-SERVER"

     ;; Some stuff that's also useful on client side
     "THREAD"
     "THREAD-CLASS"
     "THREAD-CONNECTION"
     "THREAD-ID"
     "THREAD-CONTROL-PROCESS"
     "MAKE-NEW-THREAD"

     "CONNECTION"
     "CONNECTION-THREADS"
     "FIND-THREAD"
     "CONNECTION-CONTROL-STREAM"
     "CONNECTION-CONTROL-PROCESS"
     "CLOSE-CONNECTION"

     "TAGGED-OBJECT"
     "TAG-CALLBACK"
     "INVOKE-CALLBACK"

     "DESTRUCTURE-CASE"

     "WITH-CONNECTION-LOCK"
     "WITH-EVENT-HANDLING"
     "SEND-EVENT"
     "SIGNAL-EVENT"
     "HANDLE-EVENT"
     "READ-SEXP"
     )))

(in-package :swink)

(defvar *default-server-port* 4003)

(defvar *dont-close* nil
  "Keep listening for more connections on the same port after get the first one")

(defvar *external-format* :iso-8859-1)


(defvar *swink-lock* (make-lock))

(defmacro with-swink-lock ((&rest lock-options) &body body)
  `(without-interrupts
    (with-lock-grabbed (*swink-lock* ,@lock-options)
      ,@body)))

(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-"))
        (case (if (or (eq (caar (last patterns)) t)
                      (eq (caaar (last patterns)) t)) 'case 'ecase)))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (,case ,operator
         ,@(loop for (pattern . body) in patterns collect 
                 (if (eq pattern t)
                     `(t ,@body)
                     (destructuring-bind (op &rest rands) pattern
                       `(,op (destructuring-bind ,rands ,operands 
                               ,@body)))))))))


(defun string-segment (string start end)
  (if (and (eql start 0) (eql end (length string)))
    string
    (make-array (- end start)
                :displaced-to string
                :displaced-index-offset start)))


(defun safe-condition-string (condition)
  (or (ignore-errors (princ-to-string condition))
      (ignore-errors (prin1-to-string condition))
      (ignore-errors (format nil "Condition of type ~s"
                             (type-of condition)))
      (ignore-errors (and (typep condition 'error)
                          "<Unprintable error>"))
      "<Unprintable condition>"))


(defun invoke-restart-if-active (restart &rest values)
  (declare (dynamic-extent values))
  (handler-case
      (apply #'invoke-restart restart values)
    (ccl::inactive-restart () nil)))

(defun marshall-event (conn event)
  (flet ((marshall (thing)           ;; Only check the top level
           (typecase thing
             (process (process-serial-number thing))
             (thread (thread-id thing))
             (condition (safe-condition-string thing))
             (function (tag-callback conn thing))
             (t thing))))
    (mapcar #'marshall event)))

(defvar *log-events* nil)

(defvar *log-queue*)

(let ((log-lock (make-lock)))
  (defun log-event (format-string &rest format-args)
    (when *log-events*
      (ignore-errors
	(let* ((string (apply #'format nil format-string format-args)))
	  ;; This kludge is so don't have to disable interrupts while printing.
	  ;; There is a tiny timing screw at end of loop; who cares, it's just for debugging...
	  (if (boundp '*log-queue*) ;; recursive call
	      (without-interrupts 
		(setq *log-queue* (nconc *log-queue* (list string))))
	      (let ((stream ccl::*stdout*))
		(with-lock-grabbed (log-lock "Log Output Lock")
		  (let ((*log-queue* (list string)))
		    (fresh-line stream)
		    (loop for string = (without-interrupts (pop *log-queue*)) while string
		       do (write-string string stream)
		       do (terpri stream))))
		(force-output stream))))))))

(defclass connection ()
  ((control-process :initform nil :accessor connection-control-process)
   (control-stream :initarg :control-stream :reader connection-control-stream)
   (buffer :initform (make-string 1024) :accessor connection-buffer)
   (lock :initform (make-lock) :reader connection-lock)
   (threads :initform nil :accessor connection-threads)
   (object-counter :initform most-negative-fixnum :accessor connection-object-counter)
   (objects :initform nil :accessor connection-objects)))

(defmacro with-connection-lock ((conn &rest lock-args) &body body)
  `(without-interrupts ;; without callbacks
    (with-lock-grabbed ((connection-lock ,conn) ,@lock-args)
      ,@body)))

(defmethod close-connection ((conn connection))
  (log-event "closing connection ~s" conn)
  (let ((process (connection-control-process conn)))
    (when process
      (process-interrupt process 'invoke-restart-if-active 'close-connection))))

(defun tag-object (conn object)
  (with-connection-lock (conn)
    (let* ((id (incf (connection-object-counter conn))))
      (push (cons id object) (connection-objects conn))
      id)))

(defun tagged-object (conn id)
  (with-connection-lock (conn)
    (let ((cell (assoc id (connection-objects conn))))
      (unless cell
        (warn "Missing object for remote reference ~s" id))
      (setf (connection-objects conn) (delq cell (connection-objects conn)))
      (cdr cell))))

(defun remove-tag (conn id)
  (with-connection-lock (conn)
    (setf (connection-objects conn) (delete id (connection-objects conn) :key #'car))))

(defun tag-callback (conn function)
  (tag-object conn (cons function *current-process*)))

(defun invoke-callback (conn id &rest values)
  (declare (dynamic-extent values))
  (destructuring-bind (function . process) (or (tagged-object conn id) '(nil . nil))
    (when function
      (apply #'process-interrupt process function values))))

(define-condition abort-call ()
  ((tag :initarg :tag :reader abort-call-tag)))

(defun abort-callback (conn id)
  (destructuring-bind (function . process) (or (tagged-object conn id) '(nil . nil))
    (when function
      (process-interrupt process (lambda () (signal 'abort-call :tag id))))))

(defun write-packet (conn string)
  (let ((stream (connection-control-stream conn)))
     (assert (<= (length string) #xFFFFFF))
    ;; We could have a separate lock for the stream, but we can't really send back anything until
    ;; this write is finished, so it doesn't hurt much if random stuff is held up while we do this.
    (with-connection-lock (conn)
      (format stream "~6,'0,X" (length string))
      (write-string string stream))
    (force-output stream)))

(defvar +swink-io-package+
  (loop as name = (gensym "SwinkIO/") while (find-package name)
    finally (let ((package (make-package name :use nil)))
              (import '(nil t quote) package)
              (return package))))

(defun format-for-swink (fmt-string fmt-args)
  (with-standard-io-syntax
      (let ((*package* +swink-io-package+))
        (apply #'format nil fmt-string fmt-args))))

(defun write-sexp (conn sexp)
  (write-packet conn (with-standard-io-syntax
                         (let ((*package* +swink-io-package+))
                           (prin1-to-string sexp)))))

(defun send-event (target event &key ignore-errors)
  (let* ((conn (etypecase target
		 (connection target)
		 (thread (thread-connection target))))
	 (encoded-event (marshall-event conn event)))
    (log-event "Send-event ~s to ~a" encoded-event (if (eq target conn)
						       "connection"
						       (princ-to-string (thread-id target))))
    (handler-bind ((stream-error (lambda (c)
				   (when (eq (stream-error-stream c) (connection-control-stream conn))
				     (unless ignore-errors
				       (log-event "send-event error: ~a" c)
				       (close-connection conn))
				     (return-from send-event)))))
      (write-sexp conn (cons (thread-id target) encoded-event)))))

(defun send-event-if-open (target event)
  (send-event target event :ignore-errors t))

;;This assumes only one process reads from the command stream or the read-buffer, so don't need locking.
(defun read-sexp (conn)
  ;; Returns the sexp or :end-connection event
  (let* ((stream (connection-control-stream conn))
         (buffer (connection-buffer conn))
         (count (stream-read-vector stream buffer 0 6)))
    (handler-bind ((stream-error (lambda (c)
                                   ;; This includes parse errors as well as i/o errors
                                   (when (eql (stream-error-stream c) stream)
                                     (log-event "read-sexp error: ~a" c)
				     ; (setf (connection-io-error conn) t)
                                     (return-from read-sexp
                                       `(nil . (:end-connection ,c)))))))
      (when (< count 6) (ccl::signal-eof-error stream))
      (setq count (parse-integer buffer :end 6 :radix 16))
      (when (< (length buffer) count)
        (setq buffer (setf (connection-buffer conn) (make-string count))))
      (let ((len (stream-read-vector stream buffer 0 count)))
        (when (< len count) (ccl::signal-eof-error stream))
        ;; TODO: verify that there aren't more forms in the string.
        (with-standard-io-syntax
            (let ((*package* +swink-io-package+)
                  (*read-eval* nil))
              (read-from-string buffer t nil :end count)))))))

;; Data for processes with swink event handling.
(defclass thread ()
  ((connection :initarg :connection :reader thread-connection)
   (lock :initform (make-lock) :reader thread-lock)
   (process :initarg :process :accessor thread-process)
   (event-queue :initform nil :accessor thread-event-queue)))

(defmacro with-thread-lock ((thread &rest lock-args) &rest body)
  `(without-interrupts
    (with-lock-grabbed ((thread-lock ,thread) ,@lock-args)
      ,@body)))

(defmethod thread-id ((thread thread))
  (thread-id (thread-process thread)))

(defmethod thread-id ((process process))
  (process-serial-number process))

(defmethod thread-id ((id integer))
  id)

(defun find-thread (conn id &key (key #'thread-id))
  (with-connection-lock (conn)
    (find id (connection-threads conn) :key key)))

(defmethod make-new-thread ((conn connection) &optional (process *current-process*))
  (with-connection-lock (conn)
    (assert (not (find-thread conn process :key #'thread-process)))
    (let ((thread (make-instance (thread-class conn) :connection conn :process process)))
      (push thread (connection-threads conn))
      thread)))


(defun queue-event (thread event)
  (with-thread-lock (thread)
    (setf (thread-event-queue thread) (nconc (thread-event-queue thread) (list event)))))

(defun dequeue-event (thread)
  (with-thread-lock (thread) (pop (thread-event-queue thread))))


;; Event handling.
;; Built on conditions rather than semaphores, so events can interrupt a process in i/o wait.

(defvar *signal-events* nil)

(define-condition events-available () ())

(defun enable-event-handling (thread)
  (setq *signal-events* t)
  (loop while (thread-event-queue thread)
        do (let ((*signal-events* nil))
             (handle-events thread))))

(defmacro with-event-handling ((thread &key restart) &body body)
  (let ((thread-var (gensym "THREAD")))
    (if restart
      `(let ((,thread-var ,thread))
         (loop
           (handler-case (return (let ((*signal-events* *signal-events*))
                                   (enable-event-handling ,thread-var)
                                   ,@body))
             (events-available () (let ((*signal-events* nil))
                                   (handle-events ,thread-var))))))
      `(let ((,thread-var ,thread))
         (handler-bind ((events-available (lambda (c)
                                            (declare (ignore c))
                                            (handle-events ,thread-var))))
           (let ((*signal-events* *signal-events*))
             (enable-event-handling ,thread-var)
             ,@body))))))

(defun signal-event (thread event)
  (queue-event thread event)
  (process-interrupt (or (thread-control-process thread)
                         (error "Got event ~s for thread ~s with no process" event thread))
                     (lambda ()
                       (when *signal-events*
                         (let ((*signal-events* nil))
                           (signal 'events-available))))))


(defmethod handle-events ((thread thread))
  (loop as event = (dequeue-event thread) while event
        do (handle-event thread event)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Server side:
;;
;; In any process we can enter a read loop which gets its input from a swink connection
;; and sends output to the connection.  We can also spawn a process that does nothing else.

(defvar *global-debugger* t
  "Use remote debugger on errors even in non-repl threads")

(defclass server-ui-object (ccl::ui-object) ())

(defclass server-connection (connection)
  ())

(defclass server-thread (thread server-ui-object)
  ((io :initform nil :accessor thread-io)))

(defmethod thread-class ((conn server-connection)) 'server-thread)

(defmethod thread-control-process ((thread server-thread))
  (thread-process thread))

(defvar *server-connections* '()
  "List of all active connections, with the most recent at the front.")

(defvar *current-server-thread* nil)

;; TODO: if this process talked to a connection before, we should reuse it...
(defun connection-for-break (process)
  "Return the 'default' connection for implementing a break in a
non-swink process PROCESS."
  (declare (ignore process))
  (car *server-connections*))

(defmethod thread-id ((conn server-connection))
  (process-serial-number *current-process*))

(defvar *listener-sockets* nil)

(defun start-server (&key (port *default-server-port*)
                          (dont-close *dont-close*) 
                          (external-format *external-format*))
  "Start a SWINK server on PORT.
  If DONT-CLOSE is true then the listen socket will accept multiple
  connections, otherwise it will be closed after the first."
  (let* ((stream-args (and external-format `(:external-format ,external-format)))
         (socket (make-socket :connect :passive
                              :local-host "127.0.0.1" :local-port port
                              :reuse-address t))
         (info (cons socket nil))
         (local-port (local-port socket)))
    (with-swink-lock ()
      (setf (getf *listener-sockets* port) info))
    (setf (cdr info)
          (process-run-function (format nil "Swink Server ~a" local-port)
            (lambda ()
              (setf (cdr info) *current-process*)
              (flet ((serve ()
                       (let ((stream nil))
                         (unwind-protect
                             (progn
                               (setq stream (accept-connection socket :wait t :stream-args stream-args))
                               (spawn-server-connection stream)
                               (setq stream nil))
                           (when stream (close stream :abort t))))))
                (unwind-protect
                    (cond ((not dont-close) (serve))
                          (t (loop (ignore-errors (serve)))))
                  (close socket :abort t)
                  (with-swink-lock ()
                    (remf *listener-sockets* info)))))))
    (log-event "Swink awaiting ~s instructions on port ~s ~s" external-format local-port socket)
    local-port))

(defun stop-server (port)
  "Stop server running on PORT."
  (let* ((info (with-swink-lock () (getf *listener-sockets* port))))
    (when info
      (destructuring-bind (socket . process) info
        (when process
          (process-kill process))
        (close socket :abort t) ;; harmless if already closed.
        (with-swink-lock ()
          (remf *listener-sockets* info)))
      t)))

(defun server-event-loop (conn)
  (loop
    (let ((thread.event (read-sexp conn)))
      (log-event "received: ~s" thread.event)
      (destructuring-bind (thread-id . event) thread.event  ;; TODO: make send-client-event prepend nil if send to conn
        (if thread-id
          (let ((thread (find-thread conn thread-id)))
            (when thread
              (signal-event thread event)))
          (handle-event conn event))))))

(defun spawn-server-connection (stream)
  (let ((conn (make-instance 'server-connection :control-stream stream))
        (startup-signal (make-semaphore)))
    (setf (connection-control-process conn)
          (process-run-function (format nil "swink-event-loop@~s" (local-port stream))
            (lambda ()
	      (unwind-protect
		   (with-simple-restart (close-connection "Exit server")
		     (setf (connection-control-process conn) *current-process*)
		     (handler-bind ((error (lambda (c)
					     (log-event "Error: ~a" c)
					     (invoke-restart 'close-connection))))
		       (when startup-signal (signal-semaphore startup-signal))
		       (server-event-loop conn)))
		(control-process-cleanup conn)))))
    (wait-on-semaphore startup-signal)
    (with-swink-lock () (push conn *server-connections*))
    (when *global-debugger*
      (use-swink-globally t))
    conn))

;; Note this happens in an unwind-protect, so is without interrupts.  But we've pretty much
;; returned to top level and hold no locks.
(defun control-process-cleanup (conn)
  (with-swink-lock ()
    (setq *server-connections* (delq conn *server-connections*))
    (when (null *server-connections*) (use-swink-globally nil)))
  (flet ((exit-repl ()
	   ;; While exiting, threads may attempt to write to the connection.  That's good, if the
	   ;; connection is still alive and we're attempting an orderly exit.  Don't go into a spiral
	   ;; if the connection is dead.  Once we get any kind of error, just punt.
	   (log-event "Start exit-repl in ~s" (thread-id *current-process*))
	   (handler-case  (invoke-restart-if-active 'exit-repl)
	     (error (c) (log-event "Exit repl error ~a in ~s" c (thread-id *current-process*))))))
    (loop for thread in (with-connection-lock (conn)
			  (copy-list (connection-threads conn)))
       do (process-interrupt (thread-process thread) #'exit-repl)))
  (let* ((timeout 0.05)
         (end (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (process-wait "closing connection"
      (lambda ()
        (or (null (connection-threads conn)) (> (get-internal-real-time) end)))))
  (when (connection-threads conn)
    (warn "Wasn't able to close these threads: ~s" (connection-threads conn)))

  (close (connection-control-stream conn)))


;; This is only called when this lisp receives an interrupt signal.
(defun select-interactive-process ()
  (when *global-debugger*
    (loop for conn in (with-swink-lock () (copy-list *server-connections*))
      do (with-connection-lock (conn)
           (loop for thread in (connection-threads conn)
             when (thread-io thread) ;; still active
             do (return-from select-interactive-process (thread-process thread)))))))

(defmethod get-remote-user-input ((thread server-thread))
  ;; Usually this is called from a repl evaluation, but the user could have passed the stream to
  ;; any other process, so we could be running anywhere.  Thread is the thread of the stream.
  (with-simple-restart (abort-read "Abort reading")
    (let* ((conn (thread-connection thread))
	   (returned nil)
           (returned-string nil)
           (return-signal (make-semaphore))
           (tag nil))
      (force-output (thread-io thread))
      (unwind-protect
	   (progn
	     (setq tag (tag-callback conn (lambda (string)
					    (setq returned t)
					    (setq returned-string string)
					    (signal-semaphore return-signal))))
	     (send-event conn `(:read-string ,thread ,tag))
	     (let ((current-thread (find-thread conn *current-process* :key #'thread-process)))
	       (with-interrupts-enabled
		 (if current-thread ;; we're running in a repl, process events while waiting.
		     (with-event-handling (current-thread)
		       (wait-on-semaphore return-signal))
		     (wait-on-semaphore return-signal))))
	     returned-string)
        (unless returned
          ;; Something interrupted us and aborted
          ;; ignore response if sent
          (when tag (remove-tag conn tag))
	  ;; tell client to stop reading as well.
          (send-event-if-open conn `(:abort-read ,thread ,tag)))))))


(defmethod send-remote-user-output ((thread server-thread) string start end)
  (let ((conn (thread-connection thread)))
    (send-event conn `(:write-string ,thread ,(string-segment string start end)))))

(defun swink-repl (conn break-level toplevel-loop)
  (let* ((thread (make-new-thread conn))
         (in (make-input-stream thread #'get-remote-user-input))
         (out (make-output-stream thread #'send-remote-user-output))
         (io (make-two-way-stream in out))
         (ui-object (ccl::process-ui-object *current-process*)))
    (assert (null (thread-io thread)))
    (with-simple-restart (exit-repl "Exit remote read loop")
      (unwind-protect
          (let* ((*current-server-thread* thread)
                 (*standard-input* in)
                 (*standard-output* out)
                 (*trace-output* out)
                 (*debug-io* io)
                 (*query-io* io)
                 (*terminal-io* io)
                 (ccl::*break-level* 0)
                 (ccl::*read-loop-function* 'swink-read-loop))
            (setf (ccl::process-ui-object *current-process*) thread)
            (setf (thread-io thread) io)
            (ccl:add-auto-flush-stream out)
            (send-event conn `(:start-repl ,break-level))
            (funcall toplevel-loop))
        ;; Do we need this?  We've already exited from the outermost level...
	(send-event-if-open conn `(:exit-repl))
        (ccl:remove-auto-flush-stream out)
        (setf (ccl::process-ui-object *current-process*) ui-object)
        (setf (thread-io thread) nil)
        (close in :abort t)
        (close out :abort t)
        (with-connection-lock (conn)
          (setf (connection-threads conn) (delq thread (connection-threads conn))))))))


(defclass repl-process (process) ())

(defun spawn-repl (conn name)
  (process-run-function `(:name ,name :class repl-process)
    (lambda ()
      (swink-repl conn 0 #'ccl::toplevel-loop))))

;; Invoked for a break in a non-repl process (can only happen if using swink globally).
(defun swink-debugger-hook (condition hook)
  (declare (ignore hook))
  (when (eq ccl::*read-loop-function* 'swink-read-loop)
    (return-from swink-debugger-hook nil))
  (let ((conn (connection-for-break *current-process*)))
    ;; TODO: set up a restart to pick a different connection, if there is more than one.
    (when conn
      (swink-repl conn 1 (lambda ()
                           (ccl::%break-message ccl::*break-loop-type* condition)
                           ;; Like toplevel-loop but run break-loop to set up error context first
                           (loop
                             (catch :toplevel
                               (ccl::break-loop condition))
                             (when (eq *current-process* ccl::*initial-process*)
                               (toplevel))))))))

(defun swink-read-loop (&key (break-level 0) &allow-other-keys)
  (let* ((thread *current-server-thread*)
         (conn (thread-connection thread))
         (ccl::*break-level* break-level)
         (*loading-file-source-file* nil)
         (ccl::*loading-toplevel-location* nil)
         (context (find break-level ccl::*backtrace-contexts* :key (lambda (bt) (ccl::bt.break-level bt))))
         *** ** * +++ ++ + /// // / -)
    (when context
      ;; TODO: neither :GO nor cmd-/ pay attention to the break condition, whereas bt.restarts does...
      (let ((continuable (ccl::backtrace-context-continuable-p context)))
        (send-event conn `(:enter-break ,break-level ,(and continuable t)))))

    (flet ((repl-until-abort ()
             (restart-case
                 (catch :abort
                   (catch-cancel
                    ;; everything is done via interrupts ...
                    (with-event-handling (thread)
                      (loop (sleep 60)))))
               (abort ()
                 :report (lambda (stream)
                           (if (eq break-level 0)
                             (format stream "Return to toplevel")
                             (format stream "Return to break level ~D" break-level)))
                 nil)
               (abort-break () (unless (eql break-level 0) (abort))))))
      (unwind-protect
          (loop
            (repl-until-abort)
            (clear-input)
            (terpri)
            (send-event conn `(:read-loop ,break-level)))
	(send-event-if-open conn `(:debug-return ,break-level))))))

(defmacro with-return-values ((conn remote-tag &body abort-forms) &body body)
  (let ((ok-var (gensym))
        (tag-var (gensym))
        (conn-var (gensym)))
    `(let ((,ok-var nil) (,conn-var ,conn) (,tag-var ,remote-tag))
       (send-event ,conn-var `(:return ,,tag-var
                                       ,@(unwind-protect
                                             (prog1 (progn ,@body) (setq ,ok-var t))
                                           (unless ,ok-var
                                             (send-event-if-open ,conn-var `(:cancel-return ,,tag-var))
                                             ,@abort-forms)))))))


(defmethod handle-event ((conn server-connection) event)
  (log-event "handle-event (global): ~s" event)
  (destructure-case event

    ((:end-connection condition)
     (declare (ignore condition))
     (close-connection conn))

    ((:spawn-repl name)
     (spawn-repl conn name))

    ((:return local-tag &rest values)
     (apply #'invoke-callback conn local-tag values))

    ((:connection-info remote-tag)
     (with-return-values (conn remote-tag)
       (list `(:pid ,(ccl::getpid)
                    :lisp-implementation-type ,(lisp-implementation-type)
                    :lisp-implementation-version ,(lisp-implementation-version)
                    :machine-instance ,(machine-instance)
                    :machine-type ,(machine-type)
                    :machine-version ,(machine-version)))))

    #+remote-eval
    ((:eval form)
       ;; It's the caller's responsibility to make this quick...  If they want return values
       ;; or whatever, they can put that in the form.
       (eval form))))
  
;; TODO: toplevel-eval checks package change and invokes application-ui-operation, need to send that back.


;; Eval all forms in string without printing intermediate results
(defun read-eval-all-print-last (string package-name)
  (if package-name
    (let ((*package* (or (find-package package-name) *package*)))
      (read-eval-all-print-last string nil))
    (with-input-from-string (sstream string)
      (let ((values nil))
        (loop
          (let ((form (ccl::read-toplevel-form sstream :eof-value sstream)))
            (when (eq form sstream)
              (ccl::toplevel-print values)
              (force-output)
              (return))
            (unless (ccl::check-toplevel-command form)
              (setq values (ccl::toplevel-eval form nil))
              (setq /// // // / / values)
              (unless (eq (car values) (ccl::%unbound-marker))
                (setq *** ** ** * *  (car values))))))
        (values)))))


(defun read-eval-print-one (conn sstream package)
  (if package
    (let ((*package* package))
      (read-eval-print-one conn sstream nil))
    (let ((form (ccl::read-toplevel-form sstream :eof-value sstream)))
      (unless (eq form sstream)
        (unless (ccl::check-toplevel-command form)
          (ccl::toplevel-print (ccl::toplevel-eval form nil))))
      (cond ((listen sstream)
             (tag-object conn (cons sstream package)))
            (t
             (close sstream)
             nil)))))


;; Events from client to specific thread.  This is running at a safe point inside a repl thread.
(defmethod handle-event ((thread thread) event)
  (log-event "handle-event (thread ~s): ~s" (process-serial-number *current-process*) event)
  (let ((conn (thread-connection thread)))
    (destructure-case event
      
      ((:read-eval-all-print-last string package-name remote-tag)
       (with-return-values (conn remote-tag)
         (read-eval-all-print-last string package-name)))
      
      ((:read-eval-print-one string package-name remote-tag)
       (let* ((sstream (make-string-input-stream string))
              (package (and package-name (or (find-package package-name) *package*))))
         (with-return-values (conn remote-tag (close sstream))
           (read-eval-print-one conn sstream package))))
      
      ((:read-eval-print-next state remote-tag)
       (destructuring-bind (sstream . package) (tagged-object conn state)
         (with-return-values (conn remote-tag (close sstream))
           (read-eval-print-one conn sstream package))))

      ((:interrupt)
       (ccl::force-break-in-listener *current-process*))

      ((:invoke-restart restart-name)
       (invoke-restart restart-name))
      
      ((:toplevel)
       (toplevel)))))

(let (using-swink-globally select-hook debugger-hook break-hook ui-object)
  (defun use-swink-globally (yes-or-no)
    (log-event "use-swink-globally: ~s" yes-or-no)
    (if yes-or-no
      (unless using-swink-globally
        (setq select-hook *select-interactive-process-hook*)
        (setq *select-interactive-process-hook*
              (if select-hook
                (lambda () (or (select-interactive-process) (funcall select-hook)))
                'select-interactive-process))
        (setq debugger-hook *debugger-hook*)
        (setq *debugger-hook*
              (if debugger-hook
                (lambda (condition hook)
                  (swink-debugger-hook condition hook)
                  (funcall debugger-hook condition hook))
                'swink-debugger-hook))
        (setq break-hook *break-hook*)
        (setq *break-hook*
              (if break-hook
                (lambda (condition hook)
                  (swink-debugger-hook condition hook)
                  (funcall break-hook condition hook))
                'swink-debugger-hook))
        (setq ui-object (ccl::application-ui-object *application*))
        (setf (ccl::application-ui-object *application*) (make-instance 'server-ui-object))
        (setq using-swink-globally t))
      (when using-swink-globally
        (setf *select-interactive-process-hook* select-hook
              *debugger-hook* debugger-hook
              *break-hook* break-hook
              (ccl::application-ui-object *application*) ui-object)
        (setq using-swink-globally nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simple buffered stream with a user input/output function.
(defclass swink-stream ()
  ((thread :initarg :thread :reader stream-thread)
   (lock :initform (make-lock))
   (buffer :initform "" :initarg :buffer)
   (index :initform 0)
   (column :initform 0 :reader stream-line-column)
   (line-length :initform ccl::*default-right-margin* :accessor stream-line-length)))
  
(defmethod stream-thread ((stream two-way-stream))
  (stream-thread (two-way-stream-input-stream stream)))

(defmethod stream-thread ((stream stream))
  nil)


(defmacro with-swink-stream (slots stream &body body)
  `(with-slots (lock ,@slots) ,stream
     (with-lock-grabbed (lock)
       ,@body)))

(defclass swink-output-stream (swink-stream fundamental-character-output-stream)
  ((output-fn :initarg :output-fn)
   (buffer :initform (make-string 8000) :initarg :buffer)))

(defun make-output-stream (thread output-fn)
  (make-instance 'swink-output-stream :thread thread :output-fn output-fn))

(defun output-stream-output (stream string start end)
  (with-slots (output-fn thread) stream
    (let ((conn (thread-connection thread)))
      (handler-bind ((stream-error (lambda (c)
				     (when (eql (stream-error-stream c)
						(connection-control-stream conn))
				       (with-slots (ccl::stream) c
					 (setf ccl::stream stream))))))
	(funcall output-fn thread string start end)))))


(defmethod flush-buffer ((stream swink-output-stream)) ;; called with lock hold
  (with-slots (buffer index) stream
    (unless (eql index 0)
      (output-stream-output stream buffer 0 index)
      (setf index 0))))

(defmethod stream-write-char ((stream swink-output-stream) char)
  (with-swink-stream (buffer index column) stream
    (when (eql index (length buffer))
      (flush-buffer stream))
    (setf (schar buffer index) char)
    (incf index)
    (if (eql char #\newline)
      (setf column 0)
      (incf column)))
  char)

(defmethod stream-write-string ((stream swink-output-stream) string &optional start end)
  (with-swink-stream (buffer index column) stream
    (let* ((len (length buffer))
           (start (or start 0))
           (end (ccl::check-sequence-bounds string start end))
           (count (- end start))
           (free (- len index)))
      (when (>= count free)
        (flush-buffer stream))
      (cond ((< count len)
             (replace buffer string :start1 index :start2 start :end2 end)
             (incf index count))
            (t (output-stream-output stream string start end)))
      (let ((last-newline (position #\newline string :from-end t
                                    :start start :end end)))
        (setf column (if last-newline 
                       (- end last-newline 1)
                       (+ column count))))))
  string)

(defmethod stream-force-output ((stream swink-output-stream))
  (with-swink-stream () stream
    (flush-buffer stream)))

(defmethod ccl::stream-finish-output ((stream swink-output-stream))
  (stream-force-output stream))

(defclass swink-input-stream (swink-stream fundamental-character-input-stream)
  ((input-fn :initarg :input-fn)))

(defun make-input-stream (thread input-fn)
  (make-instance 'swink-input-stream :thread thread :input-fn input-fn))

(defun input-stream-input (stream)
  (with-slots (input-fn thread) stream
    (let ((conn (thread-connection thread)))
      (handler-bind ((stream-error (lambda (c)
				     (when (eql (stream-error-stream c)
						(connection-control-stream conn))
				       (with-slots (ccl::stream) c
					 (setf ccl::stream stream))))))
	(funcall input-fn thread)))))

(defmethod stream-read-char ((stream swink-input-stream))
  (with-swink-stream (buffer index column) stream
    (unless (< index (length buffer))
      (let ((string (input-stream-input stream)))
        (cond ((eql (length string) 0)
               (return-from stream-read-char :eof))
              (t
               (setf buffer string  index 0)))))
    (let ((char (aref buffer index)))
      (incf index)
      (if (eql char #\Newline)
        (setf column 0)
        (incf column))
      char)))

(defmethod stream-read-char-no-hang ((stream swink-input-stream))
  (with-swink-stream (buffer index column) stream
    (when (< index (length buffer))
      (let ((char (aref buffer index)))
        (incf index)
        (if (eql char #\Newline)
          (setf column 0)
          (incf column))
        char))))

(defmethod stream-listen ((stream swink-input-stream))
  (with-swink-stream (buffer index) stream
    (< index (length buffer))))

(defmethod stream-unread-char ((stream swink-input-stream) char)
  (with-swink-stream (buffer index) stream
    (if (eql (length buffer) 0) ;; perhaps did clear-input.
      (setf buffer (make-string 1 :initial-element char))
      (if (> index 0)
        (decf index)
        (error "Unread with no preceeding read")))))

(defmethod stream-clear-input ((stream swink-input-stream))
  (with-swink-stream (buffer index) stream
    (setf buffer "" index 0))
  nil)



