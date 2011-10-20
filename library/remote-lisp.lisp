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
   ;; Local process running the local repl
   (thread-process :initform nil :accessor rlisp-thread-process)
   ;; Remote process doing the evaluation for this process.
   (thread-id :initarg :thread-id :reader rlisp-thread-id)))

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
          (process-run-function (format nil "Swank Client ~a" (remote-port (swank-command-stream conn)))
                                #'swank-event-loop conn)))
  (let ((sem (make-semaphore)) (abort nil))
    ;; Patch up swank.  To be replaced someday by our own set of remote functions...
    (rlisp/execute conn
                  "(CL:LET ((CCL:*WARN-IF-REDEFINE* ()))
                     (CL:DEFUN SWANK::EVAL-REGION (STRING)
                       (CL:WITH-INPUT-FROM-STRING (STREAM STRING)
                         (CL:LET (CL:- VALUES)
                           (CL:LOOP
                             (CL:LET ((FORM (CL:READ STREAM () STREAM)))
                               (CL:WHEN (CL:EQ FORM STREAM)
                                 (CL:FINISH-OUTPUT)
                                 (CL:RETURN (CL:VALUES VALUES CL:-)))
                               (CL:UNLESS (CCL::CHECK-TOPLEVEL-COMMAND FORM)
                                 (CL:SETQ VALUES (CCL::TOPLEVEL-EVAL (CL:SETQ CL:- FORM))))
                               (CL:FINISH-OUTPUT))))))
                     (CL:DEFUN CCL::MAKE-SWANK-REPL-FOR-IDE (NAME)
                       (SWANK::CREATE-REPL ())
                       (CL:LET ((THREAD (SWANK::FIND-REPL-THREAD SWANK::*EMACS-CONNECTION*)))
                         (CL:SETF (CCL:PROCESS-NAME THREAD) NAME)
                         (SWANK::THREAD-ID THREAD)))
                     (CL:DEFUN CCL::LISTENER-EVAL-FOR-IDE (STRING)
                       (CL:LET ((SWANK::*SEND-REPL-RESULTS-FUNCTION*
                                 #'(CL:LAMBDA (_) (CL:RETURN-FROM CCL::LISTENER-EVAL-FOR-IDE
                                                    (CL:MAPCAR #'CL:WRITE-TO-STRING _)))))
                         (SWANK::REPL-EVAL STRING)))
                     (CL:SETQ SWANK::*LISTENER-EVAL-FUNCTION* 'CCL::LISTENER-EVAL-FOR-IDE))"
                   (lambda (error result)
                     (declare (ignore result))
                     (when error
                       (unwind-protect
                           (error "Error initializing SWANK: ~s" error)
                         (setq abort t)
                         (signal-semaphore sem)))
                     (signal-semaphore sem)))
    (wait-on-semaphore sem)
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

(defmethod handle-swank-event ((conn swank-rlisp-connection) event args)
  (case event
    (:return
     (destructuring-bind (value id) args
       (when id (invoke-rlisp-callback conn id value))))
    (:invalid-rpc
     (destructuring-bind (id message) args
       (when id (remove-rlisp-object conn id))
       (error "Invalid swank rpc: ~s" message)))
    ((:debug :debug-activate :debug-return :debug-condition :read-aborted)
     (destructuring-bind (thread-id &rest event-args) args
       (let ((rthread (rlisp-thread conn thread-id)))
         (unless (rlisp-thread-process rthread)
           (error "Got swank event ~s ~s for thread ~s with no process" event args rthread))
         (process-interrupt (rlisp-thread-process rthread)
                            #'handle-swank-event
                            rthread event event-args))))
    (:new-features
     (destructuring-bind (features) args
       (update-rlisp-connection-info conn :features features)))
    (:indentation-update
     (destructuring-bind (name-indent-alist) args
       (declare (ignore name-indent-alist))))
    (:write-string
     (destructuring-bind (string) args
       (let ((stream (output-stream-for-remote-lisp *application*)))
         (if (> (length string) 500)
           (process-run-function "Long Remote Output" #'write-string string stream)
           (write-string string stream)))))
    (:read-string
     (destructuring-bind (thread-id tag) args
       (let ((rthread (rlisp-thread conn thread-id :create nil)))
         (if (and rthread (rlisp-thread-process rthread))
           (process-interrupt (rlisp-thread-process rthread)
                              #'handle-swank-event
                              rthread event `(,tag))
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
    (let ((text (and (peek-char t stream nil) ;; wait for first one, nil means eof
		     (read-available-text stream))))
      (send-sexp-to-swank conn `(:emacs-return-string ,thread-id ,tag ,text)))))

(defmethod handle-swank-event ((rthread remote-lisp-thread) event args)
  (assert (eq (rlisp-thread-process rthread) *current-process*))
  (ecase event
    (:read-string
     (destructuring-bind (tag) args
       (rlisp-read-string (rlisp-thread-connection rthread) *standard-input* (rlisp-thread-id rthread) tag)))
    (:read-aborted
     (destructuring-bind (tag) args
       (signal 'rlisp-read-aborted :tag tag)))
    (:debug     ;; SLDB-SETUP
     (destructuring-bind (level (condition-text condition-type extras)
                                ;; list of (restart-name restart-description)
                                restarts
                                ;; list of (index frame-description &key restartable)
                                backtrace
                                ;; callbacks currently being evaluated in this thread.
                                ;; Wonder what emacs does with that.
                                pending-callbacks) args
       (declare (ignorable condition-type extras backtrace pending-callbacks))
       (format t "~&Error: ~a" condition-text)
       (when *show-restarts-on-break*
         (format t "~&Remote restarts:")
         (loop for (name description) in restarts
           do (format t "~&~a ~a" name description))
         (fresh-line))
       (rlisp-read-loop rthread :break-level level)))
    (:debug-activate ;; SLDB-ACTIVATE
     (destructuring-bind (level flag) args
       (declare (ignore flag))
       (unless (eql level *break-level*)
         (warn "break level confusion is ~s expected ~s" *break-level* level))))
    (:debug-condition ;; This seems to have something to do with errors in the debugger
     (destructuring-bind (message) args
       (format t "~&Swank error: ~s" message)))
    (:debug-return
     (destructuring-bind (level stepping-p) args
       (declare (ignore stepping-p))
       (unless (eql level *break-level*)
         (invoke-restart 'debug-return level))))))


;; This assumes connection process is the only thing that reads from the socket stream and uses
;; the read-buffer, so don't need locking.
(defun read-swank-event (conn)
  (assert (eq (rlisp-server-process conn) *current-process*))
  (let* ((stream (swank-command-stream conn))
         (buffer (swank-read-buffer conn))
         (count (stream-read-vector stream buffer 0 6)))
    (when (< count 6) (signal-eof-error stream))
    (setq count (parse-integer buffer :end 6 :radix 16))
    (when (< (length buffer) count)
      (setf (swank-read-buffer conn)
            (setq buffer (make-array count :element-type 'character))))
    (let ((len (stream-read-vector stream buffer 0 count)))
      (when (< len count) (signal-eof-error stream))
      ;; TODO: catch errors here and report them sanely.
      ;; TODO: check that there aren't more forms in the string.
      (with-standard-io-syntax
          (let ((*package* +swank-io-package+)
                (*read-eval* nil))
            (read-from-string buffer t nil :end count))))))


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


;; Continuation is executed in the same process that invoked remote-execute.
(defmethod rlisp/execute ((conn swank-rlisp-connection) form-or-string continuation &key package thread)
  (flet ((continuation (result)
           (ecase (car result)
             (:ok (apply continuation nil (cdr result)))
             (:abort (apply continuation (or (cadr result) '"NIL") (or (cddr result) '(nil)))))))
    (let* ((sexp `(:emacs-rex ,form-or-string
                              ,package
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
  
;;(defmethod rlisp/return-string ((conn swank-rlisp-connection) tag string &key thread)
;;  (send-sexp-to-swank conn `(:emacs-return-string ,(thread-id-for-execute thread) ,tag ,string)))

;;(defmethod swank/remote-return ((conn swank-rlisp-connection) tag value &key thread)
;;  (send-sexp-to-swank conn `(:emacs-return ,(thread-id-for-execute thread) ,tag ,value)))

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
  (rlisp-read-loop rthread :break-level 0))
  
(defmethod rlisp-read-loop ((rthread remote-lisp-thread) &key break-level)
  (let* ((*break-level* break-level)  ;; used by prompt printing
         (*last-break-level* break-level)  ;; ditto
         (debug-return nil))
    ;; When the user invokes a restart from a list, it will be a remote restart and
    ;; we will pass the request to the remote.  However, there are some UI actions that invoke local
    ;; restarts by name, e.g. cmd-/ will invoke 'continue.  We need catch those and pass them to
    ;; the remote.  The remote will then do whatever the restart does, and will send 'debug-return's
    ;; as needed.
    (unwind-protect
        (loop
          (restart-case
              ;; Do continue with a restart bind because don't want to abort whatever form is
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
            (abort () ;; intercept local attempt to abort
              (rlisp/invoke-restart rthread 'abort))
            (abort-break () ;; intercept local attempt to abort-break
              (if (eq break-level 0)
                (rlisp/invoke-restart rthread 'abort)
                (rlisp/invoke-restart rthread 'abort-break)))
            (muffle-warning (&optional condition) ;; not likely to be invoked interactively, but...
              (assert (null condition)) ;; no way to pass that!
              (rlisp/invoke-restart rthread 'muffle-warning))
            (debug-return (target-level)
               (when (> target-level break-level)
                 (error "Missed target level in debug-return - want ~s have ~s" target-level break-level))
               (when (< target-level break-level)
                 (setq debug-return t)
                 (invoke-restart 'debug-return target-level))))
          (clear-input)
          (fresh-line))
      (unless debug-return
        (warn "Unknown exit from rlisp-read-loop!")))))

(defmethod rlisp-read-loop-internal ((rthread remote-lisp-thread))
  (let* ((input-stream *standard-input*)
         (output-stream *standard-output*)
         (sem (make-semaphore))
         (eof-count 0))
    (loop
      (force-output output-stream)
      (print-listener-prompt output-stream t)
      (multiple-value-bind (text env) (toplevel-form-text input-stream)
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
  (let* ((form (format nil "(SWANK::LISTENER-EVAL ~s)" text))
         (return-values nil)
         (return-error nil))
    (rlisp/execute conn
                   form
                   (lambda (error values)
                     (setq return-error error)
                     (setq return-values values)
                     (signal-semaphore semaphore))
                   :package package
                   :thread thread)
    (wait-on-semaphore semaphore)
    (when return-error
      (error "Remote eval error ~s" return-error))
    ;; a list of strings representing each return value
    return-values))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Server-side SWANK support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (export '(load-swank start-swank-server start-swank-loader stop-swank-loader))

(defun swankvar (name &optional (package :swank))
  (symbol-value (find-symbol name package)))

(defun (setf swankvar) (value name &optional (package :swank))
  (let ((sym (find-symbol name package)))
    (if (null sym)
      (warn "Couldn't find ~a::~a" package name)
      (set sym value))))

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
  (funcall (find-symbol "CREATE-SERVER" :swank)
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




