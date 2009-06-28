;;; -*- Mode: Lisp; Package: hemlock-internals -*-

(in-package :hemlock-internals)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A HEMLOCK-VIEW represents text displayed in a (pane inside a) window.
;; Conceptually it consists of a text buffer, a modeline for semi-permanent status
;; info, an echo area for transient status info, and a text input area for reading
;; prompted input.  Currently the last two are conflated.
;;
;; A HEMLOCK-VIEW never changes which text buffer it displays (unlike in emacs).  A
;; text buffer can be displayed in multiple HEMLOCK-VIEW's, although currently there
;; is no UI to make that happen.  But we try take care to distinguish per-buffer info
;; from per-view info.  The former is stored in the buffer struct and in buffer
;; variables.  The latter is currently stored in HEMLOCK-VIEW slots, although I'd
;; like to introduce user-definable "view variables" and get rid of some of these
;; user-level slots.  [Note: currently, multiple views on a buffer are but a remote
;; dream.  Basic things like the insertion point are still per buffer when they
;; should be per view]
;;
;; The user interacts with a HEMLOCK-VIEW using events.  Each time the user presses a
;; key, the OS arranges to invoke our event handler.  The event handler computes and
;; invokes a hemlock command bound to the key.  The command is invoked in a
;; dynamic context most suitable for modifying the text buffer associated with the
;; HEMLOCK-VIEW, but by jumping through a few hoops, it can modify other buffers.

(defvar *current-view* nil)

(defun current-view (&optional (must-exist t))
  (or *current-view*
      (and must-exist (error "Hemlock view context not established"))))

(defclass hemlock-view ()
  ((pane :initarg :pane :reader hemlock-view-pane)
   (buffer :initarg :buffer :reader hemlock-view-buffer)
   (echo-area-buffer :initarg :echo-area-buffer :reader hemlock-echo-area-buffer)
   (echo-area-stream :reader hemlock-echo-area-stream)

   ;; Input state
   (quote-next-p :initform nil :accessor hemlock-view-quote-next-p)
   (current-command :initform (make-array 10 :fill-pointer 0 :adjustable t)
		    :accessor hemlock-current-command)
   (last-command :initform (make-array 10 :fill-pointer 0 :adjustable t)
                 :accessor hemlock-last-command)
   (prefix-argument-state :initform (make-prefix-argument-state)
			  :accessor hemlock-prefix-argument-state)
   ;; If set, events are diverted to the echo area for reading prompt-for-xxx input.
   (prompted-input-state :initform nil :accessor hemlock-prompted-input-state)

   (cancel-message :initform nil :accessor hemlock-cancel-message)

   ;; User level "view variables", for now give each its own slot.
   (last-command-type :initform nil :accessor hemlock-last-command-type)
   (target-column :initform 0 :accessor hemlock-target-column)
   ))

(defun hemlock-view-p (object)
  (typep object 'hemlock-view))

(defmethod initialize-instance ((view hemlock-view) &key)
  (call-next-method)
  (with-slots (echo-area-buffer echo-area-stream) view
    (setf echo-area-stream
	  (make-hemlock-output-stream (buffer-end-mark echo-area-buffer) :full))))

(defun current-prefix-argument-state ()
  (hemlock-prefix-argument-state (current-view)))

(defun last-key-event-typed ()
  "This function returns the last key-event typed by the user and read as input."
  (let* ((view (current-view))
         (keys (hemlock-current-command view)))
    (when (= (length keys) 0) ;; the normal case, when executing a command.
      (setq keys (hemlock-last-command view)))
    (when (> (length keys) 0)
      (aref keys (1- (length keys))))))

(defun last-char-typed ()
  (let ((key (last-key-event-typed)))
    (and key (key-event-char key))))

;; This handles errors in event handling.  It assumes it's called in a normal
;; event handling context for some view.
(defun lisp-error-error-handler (condition &key debug-p)
  (with-standard-standard-output
    (handler-case
        (progn
          (hemlock-ext:report-hemlock-error (current-view) condition debug-p)
          (let ((emsg (ignore-errors (princ-to-string condition))))
            (abort-to-toplevel (or emsg "Error"))))
      (error (cc)
	     (ignore-errors (format t "~&Event error handling failed"))
	     (ignore-errors (format t ": ~a" cc))
	     (abort)))))


;; This resets the command accumulation state in the current view.
(defmethod reset-command-state ()
  (let ((view (current-view)))
    ;; This resets c-q
    (setf (hemlock-view-quote-next-p view) nil)
    ;; This resets c-x (multi-key command) and c-c (modifier prefix command)
    (setf (fill-pointer (hemlock-current-command view)) 0)
    ;; This resets the numarg state.
    (prefix-argument-resetting-state (hemlock-prefix-argument-state view))))

;; This is called for ^G and for lisp errors.  It aborts all editor state,
;; including recursive reading input and incremental search.
(defun abort-to-toplevel (&optional (message "Cancelled"))
  ;; This assumes it's called in normal event state.
  (assert (and *current-view* (find-restart 'exit-event-handler)))
  (reset-command-state)
  (invoke-hook hemlock::abort-hook) ;; reset ephemeral modes such as i-search.
  (setf (hemlock-cancel-message (current-view)) message)
  (let ((eps (current-echo-parse-state :must-exist nil)))
    (when eps
      (exit-echo-parse eps :aborted)))
  (exit-event-handler))

;; Called for editor errors.  This aborts command accumulation and i-search,
;; but not recursive reading of input.
(defun abort-current-command (&optional (message "Cancelled"))
  ;; This assumes it's called in normal event state.
  (assert (and *current-view* (find-restart 'exit-event-handler)))
  (reset-command-state)
  (invoke-hook hemlock::abort-hook)
  (setf (hemlock-cancel-message (current-view)) message)
  (exit-event-handler))

(defun exit-event-handler (&optional message)
  (when (and *current-view* message)
    (setf (hemlock-cancel-message *current-view*) message))
  (let ((restart (find-restart 'exit-event-handler)))
    (if restart
      (ccl::invoke-restart-no-return restart)
      (abort))))

;; These are only used in event handling, and as such are serialized
(defparameter *translation-temp-1* (make-array 10 :fill-pointer 0 :adjustable t))
(defparameter *translation-temp-2* (make-array 10 :fill-pointer 0 :adjustable t))

(defmethod translate-and-lookup-command (keys)
  ;; Returns NIL if we're in the middle of a command (either multi-key, as in c-x,
  ;; or translation prefix, as in ESC for Meta-), else a command.
  (multiple-value-bind (translated-key prefix-p)
		       (translate-key keys *translation-temp-1* *translation-temp-2*)
    (multiple-value-bind (res t-bindings)
			 (get-current-binding translated-key)
      (etypecase res
	(command
	 (values res t-bindings))
	(hash-table 	;; we're part-way through a multi-key command
	 nil)
	(null
	 (if prefix-p   ;; we're part-way through a translation prefix
	   nil
	   (values (get-default-command) nil)))))))


;; This has a side effect of resetting the quoting state and current command.
(defmethod get-command-binding-for-key ((view hemlock-view) key)
  (let ((current-keys (hemlock-current-command view)))
    (vector-push-extend key current-keys)
    (multiple-value-bind (main-binding t-bindings)
                         (if (shiftf (hemlock-view-quote-next-p view) nil)
                           (values (get-self-insert-command) nil)
                           (let ((eps (hemlock-prompted-input-state view)))
                             (or (and eps (eps-parse-key-handler eps))
                                 (translate-and-lookup-command current-keys))))
      (when main-binding
        (let ((vec (hemlock-last-command view))) ;; reuse vector
          (setf (hemlock-last-command view) current-keys)
          (setf (fill-pointer vec) 0)
          (setf (hemlock-current-command view) vec))
        (values main-binding t-bindings)))))

(defvar *last-last-command-type*)
(defvar *last-prefix-argument*)

(defun invoke-command (command p)
  (funcall (command-function command) p))

(defmethod execute-hemlock-key ((view hemlock-view) key)
  #+debug (log-debug "~&execute-hemlock-key ~s" key)
  (with-output-to-listener
   (if (or (symbolp key) (functionp key))
     (funcall key)
     (multiple-value-bind (main-binding transparent-bindings)
                          (get-command-binding-for-key view key)
       #+debug (log-debug "~&  binding ~s ~s" main-binding transparent-bindings)
       (ring-push key *key-event-history*)
       ;; If the key represents an "alphabetic" character (of which there
       ;; are about 94000), and the event has no modifiers or only a shift
       ;; modifier, treat it if it were bound to "Self Insert".
       
       (when (eq main-binding (get-default-command))
	 (let* ((modifiers (key-event-bits-modifiers (key-event-bits key)))
                (char (key-event-char key)))
	   (when (and char
                      (graphic-char-p char)
		      (or (null modifiers)
			  (equal '("Shift") modifiers)))
	     (setq main-binding (get-self-insert-command)))))
       (when main-binding
         (let* ((*last-last-command-type* (shiftf (hemlock-last-command-type view) nil))
                (*last-prefix-argument* (hemlock::prefix-argument-resetting-state)))
           (dolist (binding transparent-bindings)
             (invoke-command binding *last-prefix-argument*))
           (invoke-command main-binding *last-prefix-argument*)))))))

(defmethod update-echo-area-after-command ((view hemlock-view))
  (let* ((eps (hemlock-prompted-input-state view)))
    ;;if we're in the process of returning from a recursive parse,
    ;; don't do anything, let the outer event handle it.
    (unless (and eps (eps-parse-results eps))
      (let ((msg (shiftf (hemlock-cancel-message view) nil)))
	(if msg
	  (loud-message msg)
	  ;; Echo command in progress if there is one, unless in a recursive parse
	  (unless eps
	    (let ((cmd (hemlock-current-command view)))
	      (unless (eql 0 (length cmd))
		(let ((cstr (concatenate 'string (pretty-key-string cmd) " ")))
		  (message cstr))))))))))

(defmethod hemlock-view-current-buffer ((view hemlock-view))
  (if (hemlock-prompted-input-state view)
    (hemlock-echo-area-buffer view)
    (hemlock-view-buffer view)))

(defun buffer-modification-state (buffer)
  (multiple-value-bind (start end) (buffer-selection-range buffer)
    (list* (buffer-signature buffer) start end)))

(defvar *next-view-start* nil)

(defun set-scroll-position (how &optional where)
  "Set the desired scroll position of the current view"
  (when (markp where)
    (unless (eq (mark-buffer where)
                (hemlock-view-buffer (current-view)))
      (error "~s is not a mark in the current view." where))
    (setq where (mark-absolute-position where)))
  (setf *next-view-start* (cons how where)))

(defmethod handle-hemlock-event ((view hemlock-view) key)
  ;; Key can also be a function, in which case it will get executed in the view event context
  #+debug (log-debug "handle-hemlock-event ~s~:[~; (recursive)~]"
                  key
                  (and (eq view *current-view*)
                       (eq (hemlock-view-current-buffer view) *current-buffer*)))
  (if (and (eq view *current-view*)
           (eq (hemlock-view-current-buffer view) *current-buffer*))
    ;; KLUDGE: This might happen with stuff that normally switches buffers (e.g. meta-.)
    ;; but happens not to.  Because of the stupid buffer binding/unbinding, it's currently
    ;; problematic to just recurse here, so don't.
    (progn
      ;; TODO: should this catch exit-event or let outer one do it?  Check callers.
      (execute-hemlock-key view key)
      )
    (ccl::with-standard-abort-handling "Abort editor event handling"
      (let* ((*current-view* view)
             (*current-buffer* (hemlock-view-current-buffer view))
             (*next-view-start* nil) ;; gets set by scrolling commands
             (text-buffer (hemlock-view-buffer view))
             (mod (buffer-modification-state text-buffer)))
        (modifying-buffer-storage (*current-buffer*)
          (restart-case
              (handler-bind ((error #'(lambda (c)
                                        (lisp-error-error-handler c :debug-p t))))
                (execute-hemlock-key view key))
            (exit-event-handler () :report "Exit from hemlock event handler")))
        ;; Update display
        (if *next-view-start*
          (destructuring-bind (how . where) *next-view-start*
            (hemlock-ext:scroll-view view how where))
          (unless (equal mod (buffer-modification-state text-buffer))
            ;; Modified buffer, make sure user sees what happened
            (hemlock-ext:ensure-selection-visible view)))
        (update-echo-area-after-command view)))))
