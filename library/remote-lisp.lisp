;;;-*- Mode: Lisp; Package: CCL -*-
;;;
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

(in-package :ccl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Client-side remote lisp support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (export '(remote-lisp-thread remote-listener-function toplevel-form-text))

(defclass remote-lisp-connection ()
  ((lock :initform (make-lock) :reader rlisp-lock)
   (server-process :initform nil :accessor rlisp-server-process)
   (object-counter :initform most-negative-fixnum :accessor rlisp-object-counter)
   (objects :initform () :accessor rlisp-objects)
   (threads :initform () :accessor rlisp-threads)

   (features :initform nil :accessor rlisp-features)
   (lisp-implementation-type :initform "???" :accessor rlisp-lisp-implementation-type)
   (lisp-implementation-version :initform "???" :accessor rlisp-lisp-implementation-version)
   (machine-instance :initform "???" :accessor rlisp-machine-instance)))

(defmacro with-rlisp-lock ((conn &rest args) &body body)
  `(with-lock-grabbed ((rlisp-lock ,conn) ,@args)
     (without-interrupts ;; without callbacks
      ,@body)))

(defmethod update-rlisp-connection-info ((conn remote-lisp-connection)
                                         &key lisp-implementation-type
                                              lisp-implementation-version
                                              machine-instance
                                              (features nil featuresp))
  (with-rlisp-lock (conn)
    (when featuresp
      (setf (rlisp-features conn) features))
    (when machine-instance
      (setf (rlisp-machine-instance conn) machine-instance))
    (when lisp-implementation-type
      (setf (rlisp-lisp-implementation-type conn) lisp-implementation-type))
    (when lisp-implementation-version
      (setf (rlisp-lisp-implementation-version conn) lisp-implementation-version))))

(defun register-rlisp-object (conn object)
  (with-rlisp-lock (conn)
    (let* ((id (incf (rlisp-object-counter conn))))
      (push (cons id object) (rlisp-objects conn))
      id)))

(defun find-rlisp-object (conn id)
  (with-rlisp-lock (conn)
    (let ((cell (assoc id (rlisp-objects conn))))
      (unless cell
        (warn "Missing remote object ~s" id))
      (setf (rlisp-objects conn) (delq cell (rlisp-objects conn)))
      (cdr cell))))

(defun remove-rlisp-object (conn id)
  (with-rlisp-lock (conn)
    (setf (rlisp-objects conn) (delete id (rlisp-objects conn) :key #'car))))

(defun register-rlisp-callback (conn callback)
  (register-rlisp-object conn (cons callback *current-process*)))

;; Invoke callback in the process that registered it.
(defun invoke-rlisp-callback (conn id &rest values)
  (declare (dynamic-extent values))
  (destructuring-bind (callback . process) (or (find-rlisp-object conn id) '(nil . nil))
    (when callback
      (apply #'process-interrupt process callback values))))

(defclass remote-lisp-thread ()
  ((conn :initarg :connection :reader rlisp-thread-connection)
   ;; Local process running the local repl: interacting with user, sending to remote for execution.
   (thread-process :initform nil :accessor rlisp-thread-process)
   (break-level :initform nil :accessor rlisp-thread-break-level)
   ;; Id of remote process doing the evaluation for the local process.
   (thread-id :initarg :thread-id :reader rlisp-thread-id)
   (event-queue :initform nil :accessor rlisp-thread-event-queue)))

(defmethod rlisp-host-description ((rthread remote-lisp-thread))
  (rlisp-host-description (rlisp-thread-connection rthread)))

(defmethod print-object ((rthread remote-lisp-thread) stream)
  (print-unreadable-object (rthread stream :type t :identity t)
    (format stream "~a thread ~a"
            (rlisp-host-description rthread)
            (rlisp-thread-id rthread))))

(defmethod rlisp-thread-id ((thread-id integer)) thread-id)

(defmethod rlisp-thread-id ((thread-id symbol)) (or thread-id t))

(defmethod rlisp-thread ((conn remote-lisp-connection) (thread remote-lisp-thread) &key (create t))
  (declare (ignore create))
  thread)

(defmethod rlisp-thread ((conn remote-lisp-connection) (id integer) &key (create t))
  (with-rlisp-lock (conn)
    (or (find id (rlisp-threads conn) :key #'rlisp-thread-id)
        (and create
             (let ((rthread (make-instance 'remote-lisp-thread :connection conn :thread-id id)))
               (push rthread (rlisp-threads conn))
               rthread)))))

(defmethod rlisp-thread ((conn remote-lisp-connection) (process process) &key (create nil))
  (with-rlisp-lock (conn)
    (or (find process (rlisp-threads conn) :key #'rlisp-thread-process)
        (and create
             (assert (not create))))))

(defmethod rlisp/invoke-restart ((rthread remote-lisp-thread) name &key)
  (rlisp/invoke-restart (rlisp-thread-connection rthread) name :thread rthread))

(defmethod rlisp/toplevel ((rthread remote-lisp-thread) &key)
  (rlisp/toplevel (rlisp-thread-connection rthread) :thread rthread))

(defmethod rlisp/execute ((rthread remote-lisp-thread) form continuation &key)
  (rlisp/execute (rlisp-thread-connection rthread) form continuation :thread rthread))

(defmethod rlisp/interrupt ((rthread remote-lisp-thread) &key)
  (rlisp/interrupt (rlisp-thread-connection rthread) :thread rthread))

(defmethod remote-listener-eval ((rthread remote-lisp-thread) text &rest keys &key &allow-other-keys)
  (apply #'remote-listener-eval (rlisp-thread-connection rthread) text :thread rthread keys))

(defclass swank-rlisp-connection (remote-lisp-connection)
  (
   ;; The socket to the swank server.  Only the connection process reads from it, without locking.
   ;;  Anyone can write, but should grab the connection lock.
   (command-stream :initarg :stream :reader swank-command-stream)
   (read-buffer :initform (make-array 1024 :element-type 'character) :accessor swank-read-buffer)))

(defmethod rlisp-host-description ((conn swank-rlisp-connection))
  (let ((socket (swank-command-stream conn)))
    (if (open-stream-p socket)
      (format nil "~a:~a" (ipaddr-to-dotted (remote-host socket)) (remote-port socket))
      ":CLOSED")))

(defmethod print-object ((conn swank-rlisp-connection) stream)
  (print-unreadable-object (conn stream :type t :identity t)
    (format stream "~a @~a"
            (rlisp-host-description conn)
            (rlisp-machine-instance conn))))


(defmethod start-rlisp-server ((conn swank-rlisp-connection))
  ;; TODO: Make sure closing the connection kills the process or vice versa.
  (assert (null (rlisp-server-process conn)))
  (flet ((swank-event-loop (conn)
           (setf (rlisp-server-process conn) *current-process*)
           (loop
             (let ((sexp (read-swank-event conn)))
               (handle-swank-event conn (car sexp) (cdr sexp))))))
    (setf (rlisp-server-process conn)
          (process-run-function (format nil "swank-event-loop ~a" (remote-port (swank-command-stream conn)))
                                #'swank-event-loop conn)))
  (let ((sem (make-semaphore)) (abort nil))
    ;; Patch up swank.  To be replaced someday by our own set of remote functions...
    ;; TODO: advise send-to-emacs to intercept :write-string  and add in the thread id.
    (rlisp/execute conn
                   "(CL:LET ((CCL:*WARN-IF-REDEFINE* ()))
                     (CL:DEFUN SWANK::SPAWN-REPL-THREAD (CONN NAME) (CCL::RDEBUG-SPAWN-REPL-THREAD CONN NAME))
                     (CL:DEFUN SWANK::DEBUG-IN-EMACS (CONN) (CCL::RDEBUG-INVOKE-DEBUGGER CONN))
                     (CCL:ADVISE SWANK::DISPATCH-EVENT
                                 (CL:LET* ((EVENT (CL:CAR CCL::ARGLIST))
                                           (COMMAND (CL:CAR EVENT)))
                                   (CL:IF (CCL:MEMQ COMMAND '(:EMACS-REX :RETURN :EMACS-INTERRUPT
                                                                         :EMACS-PONG :EMACS-RETURN :EMACS-RETURN-STRING
                                                                         :EMACS-CHANNEL-SEND :END-OF-STREAM :READER-ERROR))
                                     (:DO-IT)
                                     (SWANK::ENCODE-MESSAGE EVENT (SWANK::CURRENT-SOCKET-IO))))
                                 :WHEN :AROUND
                                 :NAME CCL::UNRESTRICTED-OUTGOING-MESSAGES
                                 :DYNAMIC-EXTENT-ARGLIST CL:T)
                     (CCL:ADVISE SWANK::SEND-TO-EMACS
                                 (CL:LET* ((EVENT (CL:CAR CCL::ARGLIST))
                                           (COMMAND (CL:CAR EVENT)))
                                   (CL:WHEN (CL:EQ COMMAND :WRITE-STRING)
                                      (CL:SETF (CL:CDDR EVENT) (CL:LIST (SWANK::CURRENT-THREAD-ID)))))
                                 :WHEN :BEFORE
                                 :NAME CCL::SEND-THREAD-WITH-WRITE-STRING)
                     (CL:DEFUN SWANK::SIMPLE-BREAK ()
                       (CCL::FORCE-BREAK-IN-LISTENER CCL::*CURRENT-PROCESS*))
                     (CL:SETF (CCL::APPLICATION-UI-OBJECT CCL::*APPLICATION*)
                               (CL:MAKE-INSTANCE 'CCL::RDEBUG-UI-OBJECT :CONNECTION SWANK::*EMACS-CONNECTION*))

                     (CL:SETQ CCL::*INVOKE-DEBUGGER-HOOK-ON-INTERRUPT* CL:NIL) ;; let it go thru to break.

                     (CL:SETQ CCL:*SELECT-INTERACTIVE-PROCESS-HOOK* 'CCL::RDEBUG-FIND-REPL-THREAD)

                     (CL:DEFUN CCL::EXIT-SWANK-LOOP (LEVEL)
                       (SWANK::SEND-TO-EMACS `(:DEBUG-RETURN
                                               ,(SWANK::CURRENT-THREAD-ID) ,LEVEL ,SWANK::*SLDB-STEPPING-P*))
                       (SWANK::WAIT-FOR-EVENT `(:SLDB-RETURN ,(CL:1+ LEVEL)) CL:T)
                       (CL:WHEN (CL:> LEVEL 1)
                         (SWANK::SEND-EVENT (SWANK::CURRENT-THREAD) `(:SLDB-RETURN ,LEVEL))))

                     (CL:DEFUN CCL::MAKE-SWANK-REPL-FOR-IDE (NAME)
                       (SWANK::CREATE-REPL ()) ;; set up connection.env with redirect threads.
                       (CL:LET ((THREAD (SWANK::FIND-REPL-THREAD SWANK::*EMACS-CONNECTION*)))
                         (CL:SETF (CCL:PROCESS-NAME THREAD) NAME)
                         (SWANK::THREAD-ID THREAD)))
                     CL:T)"
                   (lambda (error result)
                     (declare (ignore result))
                     (when error
                       (unwind-protect
                           (error "Error initializing SWANK: ~s" error)
                         (setq abort t)
                         (signal-semaphore sem)))
                     (signal-semaphore sem)))
    (wait-on-semaphore sem)
    ;; TODO: should at least kill server process.
    (when abort (return-from start-rlisp-server nil))
    (rlisp/execute conn "(SWANK:CONNECTION-INFO)"
                   (lambda (error info)
                     (unless error
                       (destructuring-bind (&key (features nil featuresp)
                                                 machine
                                                 lisp-implementation
                                                 &allow-other-keys) info
                         (let ((args nil))
                           (when featuresp
                             (setq args (list* :features features args)))
                           (when (consp machine)
                             (destructuring-bind (&key instance &allow-other-keys) machine
                               (setq args (list* :machine-instance instance args))))
                           (when (consp lisp-implementation)
                             (destructuring-bind (&key type version &allow-other-keys) lisp-implementation
                               (setq args (list* :lisp-implementation-type type
                                                 :lisp-implementation-version version
                                                 args))))
                           (when args
                             (apply #'update-rlisp-connection-info conn args)))))
                     (signal-semaphore sem)))
    (wait-on-semaphore sem)
    conn))

(defmethod output-stream-for-remote-lisp ((app application))
  *standard-output*)

(defmethod input-stream-for-remote-lisp ((app application))
  *standard-input*)

(defun process-output-stream (process)
  (let ((stream (symbol-value-in-process '*standard-output* process)))
    (loop
      (typecase stream
        (synonym-stream
         (setq stream (symbol-value-in-process (synonym-stream-symbol stream) process)))
        (two-way-stream
         (setq stream (two-way-stream-output-stream stream)))
        (t (return stream))))))

(defvar *signal-swank-events* nil)

(define-condition swank-events () ())

(defmacro with-swank-events ((rthread &key abort) &body body)
  (let ((rthread-var (gensym "RTHREAD")))
    (if abort
      ;; When body is no re-entrant, abort it before handling the event.
      `(let ((,rthread-var ,rthread))
         (loop
           (handler-case (return (let ((*signal-swank-events* t))
                                   (when (rlisp-thread-event-queue ,rthread-var)
                                     (let ((*signal-swank-events* nil))
                                       (handle-swank-events ,rthread-var)))
                                   ,@body))
             (swank-events () (let ((*signal-swank-events* nil))
                                (handle-swank-events rthread))))))
      `(let ((,rthread-var ,rthread))
         (handler-bind ((swank-events (lambda (c)
                                        (declare (ignore c))
                                        (handle-swank-events ,rthread-var))))
           (let ((*signal-swank-events* t))
             (when (rlisp-thread-event-queue ,rthread-var)
               (let ((*signal-swank-events* nil))
                 (handle-swank-events ,rthread-var)))
             ,@body))))))

(defun signal-swank-event (rthread event args)
  (with-rlisp-lock ((rlisp-thread-connection rthread)) ;; this is quick, not worth a separate lock
    (setf (rlisp-thread-event-queue rthread)
          (nconc (rlisp-thread-event-queue rthread) (list `(,event ,@args)))))
  (process-interrupt (or (rlisp-thread-process rthread)
                         (error "Got event ~s ~s for thread ~s with no process" event args rthread))
                     (lambda ()
                       (when *signal-swank-events*
                         (let ((*signal-swank-events* nil))
                           (signal 'swank-events))))))

(defun handle-swank-events (rthread)
  (loop for event = (with-rlisp-lock ((rlisp-thread-connection rthread)) ;; this is quick, not worth a separate lock
                      (pop (rlisp-thread-event-queue rthread)))
    while event do (handle-swank-event rthread (car event) (cdr event))))

(defmethod handle-swank-event ((conn swank-rlisp-connection) event args)
  (case event
    (:return
     (destructuring-bind (value id) args
       (when id (invoke-rlisp-callback conn id value))))
    (:invalid-rpc
     (destructuring-bind (id message) args
       (when id (remove-rlisp-object conn id))
       (error "Invalid rpc: ~s" message)))
    (:enter-break ;; Starting a new repl (possibly due to an error in a non-repl process)
     ;; For now, this is assumed to create the listener before processing another command, so
     ;; the remote can send commands to it right away.
     ;; If that becomes a problem, can make a protocol so the other side will explicitly wait,
     ;; and then we can spawn off a worker thread to do this.
     (destructuring-bind (thread-id break-level) args
       (let ((rthread (rlisp-thread conn thread-id)))
         (enter-rlisp-listener rthread break-level)
         ;; TODO: this isn't really right.  Need to wait for process context to be set up.  Perhaps
         ;; make sure thread-process is not set until the process is running in full context.
         (process-wait "REPL startup" #'rlisp-thread-process rthread)
         ;(signal-swank-event rthread event (cdr args))
         )))
    (:exit-break
     (destructuring-bind (thread-id) args
       (let ((rthread (rlisp-thread conn thread-id)))
         (when (and rthread (rlisp-thread-process rthread))
           (exit-rlisp-listener rthread)))))
    ((:read-loop :values :debug-return :debug-condition :read-aborted)
     ;; TODO: this needs to make sure the process is in the right dynamic state (with all restarts established etc)
     ;;  Need our own interrupt queue, with-event-handling macro...
     (destructuring-bind (thread-id &rest event-args) args
       (let ((rthread (rlisp-thread conn thread-id)))
         (signal-swank-event rthread event event-args))))
    (:new-features
     (destructuring-bind (features) args
       (update-rlisp-connection-info conn :features features)))
    (:indentation-update
     (destructuring-bind (name-indent-alist) args
       (declare (ignore name-indent-alist))))
    ;; TODO: make the i/o streams be thread-specific, so we know which listener to use even if some other
    ;; thread is doing the i/o.  I.e. this should send a thread id of the owner of the stream, not of the
    ;; thread that happens to write it, so it will always be a listener thread.
    (:write-string
     (destructuring-bind (string thread-id) args
       (let* ((rthread (rlisp-thread conn thread-id :create nil))
              (stream (if (and rthread (rlisp-thread-process rthread))
                        (process-output-stream (rlisp-thread-process rthread))
                        (output-stream-for-remote-lisp *application*))))
         (if (> (length string) 500)
           (process-run-function "Long Remote Output" #'write-string string stream)
           (write-string string stream)))))
    (:ping ;; flow control for output
     (destructuring-bind (thread-id tag) args
       ;; TODO: I guess we're supposed to wait til the previous output is finished or something.
       (send-sexp-to-swank conn `(:emacs-pong ,thread-id ,tag))))
    (:read-string
     (destructuring-bind (thread-id tag) args
       (let ((rthread (rlisp-thread conn thread-id :create nil)))
         (if (and rthread (rlisp-thread-process rthread))
           (signal-swank-event rthread event (cdr args))
           ;; not a listener thread.
           ;; TODO: this needs to be wrapped in some error handling.
           (process-run-function (format nil "Remote Input (~s)" thread-id)
                                 #'rlisp-read-string
                                 conn
                                 (input-stream-for-remote-lisp *application*)
                                 thread-id
                                 tag)))))
    (t (warn "Received unknown event ~s with args ~s" event args))))



(define-condition rlisp-read-aborted ()
  ((tag :initarg :tag :reader rlisp-read-aborted-tag)))

(defun rlisp-read-string (conn stream thread-id tag)
  (handler-bind ((rlisp-read-aborted (lambda (c)
                                       (when (eql tag (rlisp-read-aborted-tag c))
                                         (return-from rlisp-read-string)))))
    (peek-char t stream) ;; wait for first one, error if none.
    (let ((text (and (peek-char t stream nil) ;; wait for first one, nil means eof
		     (read-available-text stream))))
      (send-sexp-to-swank conn `(:emacs-return-string ,thread-id ,tag ,text)))))

(defmethod handle-swank-event ((rthread remote-lisp-thread) event args)
  (assert (eq (rlisp-thread-process rthread) *current-process*))
  (ecase event
    (:read-string
     (destructuring-bind (tag) args
       (rlisp-read-string (rlisp-thread-connection rthread) *standard-input* (rlisp-thread-id rthread) tag)))
    (:read-aborted  ;; huh?
     (destructuring-bind (tag) args
       (signal 'rlisp-read-aborted :tag tag)))
    (:read-loop ;; enter (or re-enter after an abort) a break loop.
     (destructuring-bind (level) args
       (when (eql level *break-level*) ;; restart at same level, aborted current expression.
         (invoke-restart 'debug-restart level))
       (unless (eql level (1+ *break-level*))
         (warn ":READ-LOOP level confusion got ~s expected ~s" level (1+ *break-level*)))
       ;(format t "~&Error: ~a" condition-text)
       ;(when *show-restarts-on-break*
       ;  (format t "~&Remote restarts:")
       ;  (loop for (name description) in restarts
       ;    do (format t "~&~a ~a" name description))
       ;  (fresh-line))
       (rlisp-read-loop rthread :break-level level)))
     (:debug-condition ;; This seems to have something to do with errors in the debugger
         (destructuring-bind (message) args
           (format t "~&Swank error: ~s" message)))
     (:debug-return ;; return from level LEVEL read loop
      (destructuring-bind (level stepping-p) args
        (declare (ignore stepping-p))
        (invoke-restart 'debug-return level)))
     (:values ;; intermediate values when multiple forms in selection.
      (destructuring-bind (values) args
        (when values
          (fresh-line)
          (dolist (val values) (write val) (terpri)))
        (force-output)
        (print-listener-prompt *standard-output*)))))


;; This assumes connection process is the only thing that reads from the socket stream and uses
;; the read-buffer, so don't need locking.
(defun read-swank-event (conn)
  (assert (eq (rlisp-server-process conn) *current-process*))
  (let* ((stream (swank-command-stream conn))
         (buffer (swank-read-buffer conn)))
    (multiple-value-bind (form updated-buffer) (read-remote-event stream buffer)
      (unless (eq updated-buffer buffer)
        (setf (swank-read-buffer conn) updated-buffer))
      form)))

(defun read-remote-event (stream &optional buffer)
  (let* ((header (or buffer (make-string 6)))
         (count (stream-read-vector stream header 0 6)))
    (when (< count 6) (signal-eof-error stream))
    (setq count (parse-integer header :end 6 :radix 16))
    (assert (> count 0))
    (when (< (length buffer) count)
      (setq buffer (make-string count)))
    (let ((len (stream-read-vector stream buffer 0 count)))
      (when (< len count) (signal-eof-error stream))
      ;; TODO: check that there aren't more forms in the string.
      (values (handler-case
                  (with-standard-io-syntax
                      (let ((*package* +swank-io-package+)
                            (*read-eval* nil))
                        (read-from-string buffer t nil :end count)))
                (reader-error (c) `(:reader-error ,(copy-seq buffer) ,c)))
              buffer))))

(defmethod make-rrepl-thread ((conn swank-rlisp-connection) name)
  (let* ((semaphore (make-semaphore))
         (return-error nil)
         (return-id nil))
    (rlisp/execute conn (format nil "(CCL::MAKE-SWANK-REPL-FOR-IDE ~s)" name)
                   (lambda (error id)
                     (setf return-error error)
                     (setq return-id id)
                     (signal-semaphore semaphore)))
    (wait-on-semaphore semaphore)
    (when return-error
      (error "Remote eval error ~s" return-error))
    (rlisp-thread conn return-id)))

;; TODO: "coding-system".
(defun connect-to-swank (host port &key (secret-file "home:.slime-secret"))
  (let* ((socket (make-socket :remote-host host :remote-port port :nodelay t))
         (conn (make-instance 'swank-rlisp-connection :stream socket)))
    (when secret-file
      (with-open-file (stream secret-file :if-does-not-exist nil)
        (when stream
          (let ((secret (read-line stream nil nil)))
            (when secret
              (send-string-to-swank conn secret))))))
    (start-rlisp-server conn)))

(defmethod close ((conn swank-rlisp-connection) &key abort)
  ;; TODO: kill process.
  (close (swank-command-stream conn) :abort abort))

(defun send-string-to-swank (conn string)
  (let ((stream (swank-command-stream conn)))
    (with-rlisp-lock (conn)
      (format stream "~6,'0,X" (length string))
      (write-string string stream))
    (force-output stream)))

(defvar +swank-io-package+ 
  (loop as name = (gensym "SwankIO/") while (find-package name)
    finally (let ((package (make-package name :use nil)))
              (import '(nil t quote) package)
              (return package))))

(defun send-sexp-to-swank (conn sexp)
  (send-string-to-swank conn (with-standard-io-syntax
                                 (let ((*package* +swank-io-package+))
                                   (prin1-to-string sexp)))))

(defun format-for-swank (fmt-string fmt-args)
  (with-standard-io-syntax
      (let ((*package* +swank-io-package+))
        (apply #'format nil fmt-string fmt-args))))

(defun thread-id-for-execute (thread)
  (typecase thread
    (null t) ;; don't care
    (remote-lisp-thread (rlisp-thread-id thread))
    (t thread)))


;; Continuation will be executed in the current process.
(defmethod rlisp/execute ((conn swank-rlisp-connection) form-or-string continuation &key thread)
  (flet ((continuation (result)
           (ecase (car result)
             (:ok (apply continuation nil (cdr result)))
             (:abort (apply continuation (or (cadr result) '"NIL") (or (cddr result) '(nil)))))))
    (let* ((sexp `(:emacs-rex ,form-or-string
                              nil
                              ,(thread-id-for-execute thread)
                              ,(and continuation (register-rlisp-callback conn #'continuation)))))
      (if (stringp form-or-string)
        (send-string-to-swank conn (format-for-swank "(~s ~a ~s ~s ~s)" sexp))
        (send-sexp-to-swank conn sexp)))))

(defmethod rlisp/invoke-restart ((conn swank-rlisp-connection) name &key thread)
  ;; TODO: if had a way to harvest old continuations, could check for error.  But since this
  ;; will normally not return, don't register a continuation for it.
  (rlisp/execute conn `(invoke-restart ',name) nil :thread thread))

(defmethod rlisp/toplevel ((conn swank-rlisp-connection) &key thread)
  (rlisp/execute conn `(toplevel) nil :thread thread))

(defmethod rlisp/interrupt ((conn swank-rlisp-connection) &key thread)
  (send-sexp-to-swank conn `(:emacs-interrupt ,(thread-id-for-execute thread))))

(defun read-available-text (stream)
  (loop with buffer = (make-array 100 :element-type 'character :adjustable t :fill-pointer 0)
    for ch = (stream-read-char-no-hang stream)
    until (or (eq ch :eof) (null ch))
    do (vector-push-extend ch buffer)
    finally (return buffer)))

;; Return text for remote evaluation.
(defmethod toplevel-form-text ((stream input-stream))
  (when (peek-char t stream nil) ;; wait for the first one.
    (read-available-text stream)))

(defmethod toplevel-form-text ((stream synonym-stream))
  (toplevel-form-text (symbol-value (synonym-stream-symbol stream))))

(defmethod toplevel-form-text ((stream two-way-stream))
  (if (typep stream 'echo-stream)
    (call-next-method)
    (toplevel-form-text (two-way-stream-input-stream stream))))

;; pass this as the initial-function in make-mcl-listener-process
(defmethod remote-listener-function ((rthread remote-lisp-thread))
  (setf (rlisp-thread-process rthread) *current-process*)
  (unless (or *inhibit-greeting* *quiet-flag*)
    (let ((conn (rlisp-thread-connection rthread)))
      (format t "~&Welcome to ~A ~A on ~A!"
              (rlisp-lisp-implementation-type conn)
              (rlisp-lisp-implementation-version conn)
              (rlisp-machine-instance conn))))
  (rlisp-read-loop rthread :break-level (rlisp-thread-break-level rthread)))

;; This can be invoked when the connection dies or break-loop is exited in a non-repl process.
(defmethod exit-rlisp-listener ((rthread remote-lisp-thread))
  (application-ui-operation *application* :deactivate-rlisp-listener rthread) ;; deactivate listener
  (let ((process (rlisp-thread-process rthread)))
    (setf (rlisp-thread-process rthread) nil)
    (process-kill process)))

(defmethod enter-rlisp-listener ((rthread remote-lisp-thread) break-level)
  (when (rlisp-thread-process rthread)
    (error "Attempting to re-enter active listener"))
  (setf (rlisp-thread-break-level rthread) break-level)
  ;; The process creation would be a little different
  (create-rlisp-listener *application* rthread))

(defmethod create-rlisp-listener ((application application) rthread)
  (assert (null (rlisp-thread-process rthread)))
  ;; see make-mcl-listener-process
  (error "Not implemented yet"))

;; IDE read-loop with remote evaluation.
(defmethod rlisp-read-loop ((rthread remote-lisp-thread) &key break-level)
  (let* ((*break-level* break-level)  ;; used by prompt printing
         (*last-break-level* break-level)  ;; ditto
         (debug-return nil))
    (unwind-protect
        (loop
          (setf (rlisp-thread-break-level rthread) break-level)
          (restart-case
              ;; There are some UI actions that invoke local restarts by name, e.g. cmd-/ will invoke 'continue.
              ;; Catch those and just pass them to the remote.  The remote will then do whatever the restart
              ;; does, and will send back unwinding directions if appropriate.
              ;; Do continue with a restart-bind because don't want to abort whatever form is
              ;; about to be sent for evaluation, just in case the continue doesn't end up doing
              ;; anything on the remote end.
              (restart-bind ((continue (lambda () (rlisp/invoke-restart rthread 'continue))))
                (catch :toplevel
                  (loop
                    (catch :abort
                      (loop
                        (catch-cancel ;; exactly like :abort except prints Cancelled.
                         (rlisp-read-loop-internal rthread))
                        (rlisp/invoke-restart rthread 'abort)
                        (format *terminal-io* "~&Cancelled")))
                    (rlisp/invoke-restart rthread 'abort)))
                (rlisp/toplevel rthread))
            ;; These are invoked via invoke-restart-no-return, so must take non-local exit.
            (abort () (rlisp/invoke-restart rthread 'abort))
            (abort-break () (if (eql break-level 0)
                              (rlisp/invoke-restart rthread 'abort)
                              (rlisp/invoke-restart rthread 'abort-break)))
            ;; This is invoked when remote unwinds
            (debug-return (target-level)
               (setq debug-return t)
               (when (eql target-level break-level)
                 (return-from rlisp-read-loop))
               (when (> target-level break-level)
                 (error "Missed target level in debug-return - want ~s have ~s" target-level break-level))
               (invoke-restart 'debug-return target-level))
            (debug-restart (target-level)
               (unless (eql target-level break-level)
                 (when (> target-level break-level)
                   (error "Missed target level in debug-restart - want ~s have ~s" target-level break-level))
                 (setq debug-return t)
                 (invoke-restart 'debug-restart target-level))))
          (clear-input)
          (fresh-line))
      (unless debug-return
        (warn "Unknown exit from rlisp-read-loop!")))))

(defmethod rlisp-read-loop-internal ((rthread remote-lisp-thread))
  (let* ((input-stream *standard-input*)
         (output-stream *standard-output*)
         (sem (make-semaphore))
         (eof-count 0))
    (loop
      (force-output output-stream)
      (print-listener-prompt output-stream t)

      (multiple-value-bind (text env)
                           ;; Reading is not re-entrant so events during reading need
                           ;; to abort the read to be handled.
                           (with-swank-events (rthread  :abort t)
                             (toplevel-form-text input-stream))
        (if (null text) ;; eof
          (progn
            (when (> (incf eof-count) *consecutive-eof-limit*)
              (#_ _exit 0))
            (unless (and (not *batch-flag*)
                         (not *quit-on-eof*)
                         (stream-eof-transient-p input-stream))
              (exit-interactive-process *current-process*))
            (stream-clear-input input-stream)
            (rlisp/invoke-restart rthread 'abort-break))
          (progn
            (setq eof-count 0)
            ;;(let* ((values (toplevel-eval form env)))
            ;;      (if print-result (toplevel-print values)))
            (let* ((package-name (loop for sym in (car env) for val in (cdr env)
                                  when (eq sym '*package*) do (return val)))
                   (values (remote-listener-eval rthread text :package package-name :semaphore sem)))
              (fresh-line output-stream)
              (dolist (val values) (princ val output-stream) (terpri output-stream)))))))))


(defmethod remote-listener-eval ((conn swank-rlisp-connection) text
                                 &key package thread (semaphore (make-semaphore)))
  (assert thread)
  (let* ((form (format nil "(CCL::RDEBUG-LISTENER-EVAL ~s ~s ~s)"
                       text package 
                       ;; This will send intermediate :values messages
                       (and *verbose-eval-selection* t)))
         (return-values nil))
    (rlisp/execute conn
                   form
                   (lambda (error values)
                     ;; Error just means evaluation was aborted but we don't yet know why.  We will
                     ;; be told to either restart a readloop or exit it.  Stay in semaphore wait
                     ;; until then.
                     (unless error
                       (setq return-values values)
                       (signal-semaphore semaphore)))
                   :thread thread)
    (with-swank-events (thread)
      (wait-on-semaphore semaphore))
    ;; a list of strings representing each return value
    return-values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Server-side: support for a remote debugger
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;TODO: This is per application but we may want to allow multiple remote debuggers, and have this track
;; all connections.   See also process-ui-object.
(defclass rdebug-ui-object (ui-object)
  ((connection :initarg :connection :accessor rdebug-ui-connection)))

;; Currently built on swank.

(defun swankvar (name &optional (package :swank))
  (symbol-value (find-symbol name package)))

(defun (setf swankvar) (value name &optional (package :swank))
  (let ((sym (find-symbol name package)))
    (if (null sym)
      (warn "Couldn't find ~a::~a" package name)
      (set sym value))))

(defun swankfun (name &optional (package :swank))
  (symbol-function (find-symbol name package)))

#-bootstrapped
(declaim (special *read-loop-function*))

(defun rdebug-send (event)
  (funcall (swankfun "SEND-TO-EMACS")
           (mapcar (lambda (x) (if (processp x) (funcall (swankfun "THREAD-ID") x) x)) event)))

(defun rdebug-listener-eval (string package-name verbose-eval-selection)
  (if package-name
    (let ((*package* (or (find-package package-name) *package*)))
      (rdebug-listener-eval string nil verbose-eval-selection))
    (with-input-from-string (sstream string)
      (let ((values nil))
        (loop
          (let ((form (read-toplevel-form sstream :eof-value sstream)))
            (when (eq form sstream)
              (finish-output)
              (return values))
            (when verbose-eval-selection
              (rdebug-send `(:values ,*current-process* ,values)))
            ;; there is more.
            (unless (check-toplevel-command form)
              ;; TODO: toplevel-eval checks package change and invokes application-ui-operation, need to send that back.
              (setq values (toplevel-eval form nil))
              (setq /// // // / / values)
              (unless (eq (car values) (%unbound-marker))
                (setq *** ** ** * *  (%car values)))
              (setq values (mapcar #'write-to-string values)))))))))

(defun rdebug-spawn-repl-thread (conn name)
  (process-run-function name
                        (lambda ()
                          (funcall (swankfun "CALL-WITH-CONNECTION") conn
                                   (lambda ()
                                     (rdebug-send `(:enter-break ,*current-process* 0))
                                     (let ((*read-loop-function* 'rdebug-read-loop)
                                           (*debugger-hook* nil)
                                           (*break-hook* nil))
                                       (unwind-protect
                                           (toplevel-loop)
                                         (rdebug-send `(:exit-break ,*current-process*)))))))))

;; Debugger invoked in a non-repl process.  This is called with all swank stuff already set up.
(defun rdebug-invoke-debugger (condition)
   (when (eq *read-loop-function* 'rdebug-read-loop)
      (return-from rdebug-invoke-debugger))
    (rdebug-send `(:enter-break ,*current-process* 1))
    (unwind-protect
        (let ((*read-loop-function* 'rdebug-read-loop)
              (*debugger-hook* nil)
              (*break-hook* nil))
          (%break-message *break-loop-type* condition)
          ;; Like toplevel-loop but run break-loop to set up error context before going into read-loop
          (loop
            (catch :toplevel
              (break-loop condition))
            (when (eq *current-process* *initial-process*)
              (toplevel))))
      (rdebug-send `(:exit-break ,*current-process*))))


;; swank-like read loop except with all the standard ccl restarts and catches.
;; TODO: try to make the standard read-loop customizable enough to do this so don't have to replace it.
(defun rdebug-read-loop (&key (break-level 0) &allow-other-keys)
  ;; CCL calls this with :input-stream/:output-stream *debug-io*, but that won't do anything even if those
  ;; are set to something non-standard, since swank doesn't hang its protocol on the streams.
  (let ((*break-level* break-level)
        (*loading-file-source-file* nil)
        (*loading-toplevel-location* nil)
        *** ** * +++ ++ + /// // / -)
    (flet ((repl-until-abort ()
             (rdebug-send `(:read-loop ,*current-process* ,break-level))
             (restart-case
                 (catch :abort
                   (catch-cancel
                    (loop
                      (setq *break-level* break-level)
                      (let ((event (funcall (swankfun "WAIT-FOR-EVENT")
                                            `(or (:emacs-rex . _)
                                                 ;; some internal swank kludge...
                                                 (:sldb-return ,(1+ break-level))))))
                        (when (eql (car event) :sldb-return)
                          (abort))
                        ;; Execute some basic protocol function (not user code).
                        (apply (swankfun "EVAL-FOR-EMACS") (cdr event))))))
               (abort ()
                 :report (lambda (stream)
                           (if (eq break-level 0)
                             (format stream "Return to toplevel")
                             (format stream "Return to break level ~D" break-level)))
                 nil)
               (abort-break () (unless (eql break-level 0) (abort))))))
      (declare (ftype (function) exit-swank-loop))
      (unwind-protect
          (loop
            (repl-until-abort)
            ;(clear-input)
            ;(terpri)
            )
        (exit-swank-loop break-level)))))

 (defun safe-condition-string (condition)
   (or (ignore-errors (princ-to-string condition))
       (ignore-errors (prin1-to-string condition))
       (ignore-errors (format nil "Condition of type ~s"
                              (type-of condition)))
       (ignore-errors (and (typep condition 'error)
                           "<Unprintable error>"))
       "<Unprintable condition>"))

;; Find process to handle interactive abort, i.e. a local ^c.
(defun rdebug-find-repl-thread ()
  (let ((conn (funcall (swankfun "DEFAULT-CONNECTION"))))
    (when conn
      ;; TODO: select the frontmost listener (this selects the last created one).
      (funcall (swankfun "FIND-REPL-THREAD") conn))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Standard swank startup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (export '(load-swank start-swank-server start-swank-loader stop-swank-loader))

(defun load-swank (load-path)
  (when (find-package :swank-loader) (delete-package :swank-loader)) ;; so can tell if loaded
  (load (merge-pathnames load-path "swank-loader.lisp"))
  (unless (and (find-package :swank-loader)
               (find-symbol "INIT" :swank-loader))
    (error "~s is not a swank loader path" load-path))
  (funcall (find-symbol "INIT" :swank-loader))
  (unless (and (find-package :swank)
               (find-symbol "CREATE-SERVER" :swank))
    (error "Incompatible swank version loaded from ~s" load-path)))

(defun start-swank-server (&key
                           (port (swankvar "DEFAULT-SERVER-PORT"))
                           (debug (swankvar "*LOG-EVENTS*"))
                           (dedicated-output-port (and (swankvar "*USE-DEDICATED-OUTPUT-STREAM*")
                                                       (swankvar "*DEDICATED-OUTPUT-STREAM-PORT*")))
                           (globally-redirect-io (swankvar "*GLOBALLY-REDIRECT-IO*"))
                           (global-debugger (swankvar "*GLOBAL-DEBUGGER*"))
                           (indentation-updates (swankvar "*CONFIGURE-EMACS-INDENTATION*"))
                           (dont-close (swankvar "*DONT-CLOSE*"))
                           (coding-system "iso-latin-1-unix")
                           (style :spawn))
  "Assuming SWANK is already loaded, create a swank server on the specified port"
  (when debug
    (setf (swankvar "*LOG-EVENTS*" :swank-rpc) t)
    (setf (swankvar "*SWANK-DEBUG-P*") t)
    (setf (swankvar "*DEBUG-ON-SWANK-PROTOCOL-ERROR*") t))
  (when (setf (swankvar "*USE-DEDICATED-OUTPUT-STREAM*") (not (null dedicated-output-port)))
    (setf (swankvar "*DEDICATED-OUTPUT-STREAM-PORT*") dedicated-output-port))
  (setf (swankvar "*GLOBALLY-REDIRECT-IO*") globally-redirect-io)
  (setf (swankvar "*GLOBAL-DEBUGGER*") global-debugger)
  (setf (swankvar "*CONFIGURE-EMACS-INDENTATION*") indentation-updates)
  (funcall (swankfun "CREATE-SERVER")
           :style style
           :port port
           :dont-close dont-close
           :coding-system coding-system))


(defun swank-port-active? (port)
  (and (find-package :swank) (getf (swankvar "*LISTENER-SOCKETS*") port)))


;; Special ccl slime extension to allow the client to specify the swank path

(defvar *swank-loader-process* nil)
(defparameter $emacs-ccl-swank-request-marker "[emacs-ccl-swank-request]")
(defparameter *default-swank-loader-port* 4884)

(defun stop-swank-loader ()
  (when *swank-loader-process*
    (process-kill (shiftf *swank-loader-process* nil))))

(defun start-swank-loader (&optional (port *default-swank-loader-port*))
  (ignore-errors (stop-swank-loader))
  (let ((semaphore (make-semaphore))
        (errorp nil))
    (setq *swank-loader-process*
          ;; Wait for either a swank client to connect or the special ccl slime kludge
          (process-run-function "Swank Loader"
                                (lambda (sem)
                                  (setq *swank-loader-process* *current-process*)
                                  (unwind-protect
                                      (with-open-socket (socket :connect :passive :local-port port
                                                                :reuse-address t)
                                        (signal-semaphore (shiftf sem nil))
                                        (loop
                                          (let* ((stream (accept-connection socket))
                                                 (line (read-line stream nil)))
                                            (multiple-value-bind (path port)
                                                                 (parse-emacs-ccl-swank-request line)
                                              (let ((message (handler-case
                                                                 (if (swank-port-active? port)
                                                                   (format nil "Swank is already active on port ~s" port)
                                                                   (progn
                                                                     (load-swank path)
                                                                     (start-swank-server :port port)
                                                                     nil))
                                                               (error (c) (princ-to-string c)))))
                                                (prin1 `(:active (and (swank-port-active? port) t)
                                                                 :loader ,path
                                                                 :message ,message
                                                                 :port ,port)
                                                       stream)
                                                (finish-output stream))))))
                                    (when sem ;; in case exit before finished startup
                                      (setq errorp t)
                                      (signal-semaphore sem))))
                                semaphore))
    (wait-on-semaphore semaphore)
    (when errorp
      (ignore-errors (process-kill (shiftf *swank-loader-process* nil))))
    *swank-loader-process*))

(defun parse-emacs-ccl-swank-request (line)
  (let ((start (length $emacs-ccl-swank-request-marker)))
    (when (and (< start (length line))
               (string= $emacs-ccl-swank-request-marker line :end2 start))
      (let* ((split-pos (position #\: line :start start))
             (port (parse-integer line :junk-allowed nil :start start :end split-pos))
             (path-pos (position-if-not #'whitespacep line
                                        :start (if split-pos (1+ split-pos) start)))
             (path (subseq line path-pos
                           (1+ (position-if-not #'whitespacep line :from-end t)))))
        (values path port)))))




