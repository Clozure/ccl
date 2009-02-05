;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU (ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; This file contains the code that handles input to Hemlock.
;;;
(in-package :hemlock-internals)

;;;
;;; INPUT-WAITING is exported solely as a hack for the kbdmac definition
;;; mechanism.
;;;


;;; These are public variables users hand to the four basic editor input
;;; routines for method dispatching:
;;;    GET-KEY-EVENT
;;;    UNGET-KEY-EVENT
;;;    LISTEN-EDITOR-INPUT
;;;    CLEAR-EDITOR-INPUT
;;;
(defvar *editor-input* nil
  "A structure used to do various operations on terminal input.")

(defvar *real-editor-input* ()
  "Useful when we want to read from the terminal when *editor-input* is
   rebound.")



;;;; editor-input structure.

(defstruct (editor-input (:print-function
			  (lambda (s stream d)
			    (declare (ignore s d))
			    (write-string "#<Editor-Input stream>" stream))))
  get          ; A function that returns the next key-event in the queue.
  unget        ; A function that puts a key-event at the front of the queue.
  listen       ; A function that tells whether the queue is empty.
  clear        ; A function that empties the queue.
  ;;
  ;; Queue of events on this stream.  The queue always contains at least one
  ;; one element, which is the key-event most recently read.  If no event has
  ;; been read, the event is a dummy with a nil key-event.
  head
  tail)


;;; These are the elements of the editor-input event queue.
;;;
(defstruct (input-event (:constructor make-input-event ())) 
  next		; Next queued event, or NIL if none.
  hunk		; Screen hunk event was read from.
  key-event     ; Key-event read.
  x		; X and Y character position of mouse cursor.
  y
  unread-p)

(defvar *free-input-events* ())

(defun new-event (key-event x y hunk next &optional unread-p)
  (let ((res (if *free-input-events*
		 (shiftf *free-input-events*
			 (input-event-next *free-input-events*))
		 (make-input-event))))
    (setf (input-event-key-event res) key-event)
    (setf (input-event-x res) x)
    (setf (input-event-y res) y)
    (setf (input-event-hunk res) hunk)
    (setf (input-event-next res) next)
    (setf (input-event-unread-p res) unread-p)
    res))

;;; This is a public variable.
;;;
(defvar *last-key-event-typed* ()
  "This variable contains the last key-event typed by the user and read as
   input.")

;;; This is a public variable.  SITE-INIT initializes this.
;;;
(defvar *key-event-history* nil
  "This ring holds the last 60 key-events read by the command interpreter.")

(declaim (special *input-transcript*))

;;; DQ-EVENT is used in editor stream methods for popping off input.
;;; If there is an event not yet read in Stream, then pop the queue
;;; and return the character.  If there is none, return NIL.
;;;
(defun dq-event (stream)
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
	  (next (input-event-next head)))
     (if next
	 (let ((key-event (input-event-key-event next)))
	   (setf (editor-input-head stream) next)
	   (shiftf (input-event-next head) *free-input-events* head)
	   (ring-push key-event *key-event-history*)
	   (setf *last-key-event-typed* key-event)
	   (when *input-transcript* 
	     (vector-push-extend key-event *input-transcript*))
	   key-event)))))

;;; Q-EVENT is used in low level input fetching routines to add input to the
;;; editor stream.
;;; 
(defun q-event (stream key-event &optional x y hunk)
  (hemlock-ext:without-interrupts
   (let ((new (new-event key-event x y hunk nil))
	 (tail (editor-input-tail stream)))
     (setf (input-event-next tail) new)
     (setf (editor-input-tail stream) new))))

(defun un-event (key-event stream)
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
	  (next (input-event-next head))
	  (new (new-event key-event (input-event-x head) (input-event-y head)
			  (input-event-hunk head) next t)))
     (setf (input-event-next head) new)
     (unless next (setf (editor-input-tail stream) new)))))



;;;; Keyboard macro hacks.

(defvar *input-transcript* ()
  "If this variable is non-null then it should contain an adjustable vector
  with a fill pointer into which all keyboard input will be pushed.")

;;; INPUT-WAITING  --  Internal
;;;
;;;    An Evil hack that tells us whether there is an unread key-event on
;;; *editor-input*.  Note that this is applied to the real *editor-input*
;;; rather than to a kbdmac stream.
;;;
(defun input-waiting ()
  "Returns true if there is a key-event which has been unread-key-event'ed
   on *editor-input*.  Used by the keyboard macro stuff."
  (let ((next (input-event-next
	       (editor-input-head *real-editor-input*))))
    (and next (input-event-unread-p next))))



;;;; Input method macro.

(defvar *in-hemlock-stream-input-method* nil
  "This keeps us from undefined nasties like re-entering Hemlock stream
   input methods from input hooks and scheduled events.")

(declaim (special *screen-image-trashed*))

;;; These are the characters GET-KEY-EVENT notices when it pays attention
;;; to aborting input.  This happens via EDITOR-INPUT-METHOD-MACRO.
;;;
(defparameter editor-abort-key-events (list #k"Control-g" #k"Control-G"))

#+clx
(defun cleanup-for-wm-closed-display(closed-display)
  ;; Remove fd-handlers
  (hemlock-ext:disable-clx-event-handling closed-display)
  ;; Close file descriptor and note DEAD.
  (xlib:close-display closed-display)
  ;;
  ;; At this point there is not much sense to returning to Lisp
  ;; as the editor cannot be re-entered (there are lots of pointers
  ;; to the dead display around that will cause subsequent failures).
  ;; Maybe could switch to tty mode then (save-all-files-and-exit)?
  ;; For now, just assume user wanted an easy way to kill the session.
  (hemlock-ext:quit))

(defmacro abort-key-event-p (key-event)
  `(member ,key-event editor-abort-key-events))

;;; EDITOR-INPUT-METHOD-MACRO  --  Internal.
;;;
;;; WINDOWED-GET-KEY-EVENT and TTY-GET-KEY-EVENT use this.  Somewhat odd stuff
;;; goes on here because this is the place where Hemlock waits, so this is
;;; where we redisplay, check the time for scheduled events, etc.  In the loop,
;;; we call the input hook when we get a character and leave the loop.  If
;;; there isn't any input, invoke any scheduled events whose time is up.
;;; Unless SERVE-EVENT returns immediately and did something, (serve-event 0),
;;; call redisplay, note that we are going into a read wait, and call
;;; SERVE-EVENT with a wait or infinite timeout.  Upon exiting the loop, turn
;;; off the read wait note and check for the abort character.  Return the
;;; key-event we got.  We bind an error condition handler here because the
;;; default Hemlock error handler goes into a little debugging prompt loop, but
;;; if we got an error in getting input, we should prompt the user using the
;;; input method (recursively even).
;;;
(eval-when (:compile-toplevel :execute)

(defmacro editor-input-method-macro ()
  `(handler-bind
       ((error
	 (lambda (condition)
	   (when (typep condition 'stream-error)
	     (let* ((stream (stream-error-stream condition))
		    (display *editor-windowed-input*)
		    (display-stream 
		     #+CLX
		     (and display (xlib::display-input-stream display))))
	       (when (eq stream display-stream)
		 ;;(format *error-output* "~%Hemlock: Display died!~%~%")
		 (cleanup-for-wm-closed-display display)
		 (exit-hemlock nil))
	       (let ((device
		      (device-hunk-device (window-hunk (current-window)))))
		 (funcall (device-exit device) device))
	       (invoke-debugger condition)))))
	#+(and CLX )
	(xlib:closed-display
	 (lambda(condition)
	   (let ((display (xlib::closed-display-display condition)))
	     (format *error-output*
		     "Closed display on stream ~a~%"
		     (xlib::display-input-stream display)))
	   (exit-hemlock nil)))
	)
;     (when *in-hemlock-stream-input-method*
;       (error "Entering Hemlock stream input method recursively!"))
     (let ((*in-hemlock-stream-input-method* t)
	   (nrw-fun (device-note-read-wait
		     (device-hunk-device (window-hunk (current-window)))))
	   key-event)
       (loop
	 (when (setf key-event (dq-event stream))
	   (dolist (f (variable-value 'hemlock::input-hook)) (funcall f))
	   (return))
	 (invoke-scheduled-events)
	 (unless (or (hemlock-ext:serve-event 0)
		     (internal-redisplay))
	   (internal-redisplay)
	   (when nrw-fun (funcall nrw-fun t))
	   (let ((wait (next-scheduled-event-wait)))
	     (if wait (hemlock-ext:serve-event wait) (hemlock-ext:serve-event)))))
       (when nrw-fun (funcall nrw-fun nil))
       (when (and (abort-key-event-p key-event)
		  ;; ignore-abort-attempts-p must exist outside the macro.
		  ;; in this case it is bound in GET-KEY-EVENT.
		  (not ignore-abort-attempts-p))
	 (beep)
	 (throw 'editor-top-level-catcher nil))
       key-event)))
) ;eval-when



;;;; Editor input from windowing system.
#+clx
(defstruct (windowed-editor-input
	    (:include editor-input
		      (get #'windowed-get-key-event)
		      (unget #'windowed-unget-key-event)
		      (listen #'windowed-listen)
		      (clear #'windowed-clear-input))
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore s d))
	       (write-string "#<Editor-Window-Input stream>" stream)))
	    (:constructor make-windowed-editor-input
			  (&optional (head (make-input-event)) (tail head))))
  hunks)      ; List of bitmap-hunks which input to this stream.

#+clx
;;; There's actually no difference from the TTY case...
(defun windowed-get-key-event (stream ignore-abort-attempts-p)
  (tty-get-key-event stream ignore-abort-attempts-p))

#+clx
(defun windowed-unget-key-event (key-event stream)
  (un-event key-event stream))

#+clx
(defun windowed-clear-input (stream)
  (loop (unless (hemlock-ext:serve-event 0) (return)))
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
	  (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
	       *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

#+clx
(defun windowed-listen (stream)
  (loop
    ;; Don't service anymore events if we just got some input.
    (when (input-event-next (editor-input-head stream))
      (return t))
    ;;
    ;; If nothing is pending, check the queued input.
    (unless (hemlock-ext:serve-event 0)
      (return (not (null (input-event-next (editor-input-head stream))))))))


;;;; Editor input from a tty.

(defstruct (tty-editor-input
	    (:include editor-input
		      (get #'tty-get-key-event)
		      (unget #'tty-unget-key-event)
		      (listen #'tty-listen)
		      (clear #'tty-clear-input))
	    (:print-function
	     (lambda (obj stream n)
	       (declare (ignore obj n))
	       (write-string "#<Editor-Tty-Input stream>" stream)))
	    (:constructor make-tty-editor-input
			  (fd &optional (head (make-input-event)) (tail head))))
  fd)

(defun tty-get-key-event (stream ignore-abort-attempts-p)
  (editor-input-method-macro))

(defun tty-unget-key-event (key-event stream)
  (un-event key-event stream))

(defun tty-clear-input (stream)
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
	  (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
	       *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

;;; Note that we never return NIL as long as there are events to be served with
;;; SERVE-EVENT.  Thus non-keyboard input (i.e. process output) 
;;; effectively causes LISTEN to block until either all the non-keyboard input
;;; has happened, or there is some real keyboard input.
;;;
(defun tty-listen (stream)
  (loop
    ;; Don't service anymore events if we just got some input.
    (when (or (input-event-next (editor-input-head stream))
	      (editor-tty-listen stream))
      (return t))
    ;; If nothing is pending, check the queued input.
    (unless (hemlock-ext:serve-event 0)
      (return (not (null (input-event-next (editor-input-head stream))))))))


;;;; GET-KEY-EVENT, UNGET-KEY-EVENT, LISTEN-EDITOR-INPUT, CLEAR-EDITOR-INPUT.

;;; GET-KEY-EVENT -- Public.
;;;
(defun get-key-event (editor-input &optional ignore-abort-attempts-p)
  "This function returns a key-event as soon as it is available on
   editor-input.  Editor-input is either *editor-input* or *real-editor-input*.
   Ignore-abort-attempts-p indicates whether #k\"C-g\" and #k\"C-G\" throw to
   the editor's top-level command loop; when this is non-nil, this function
   returns those key-events when the user types them.  Otherwise, it aborts the
   editor's current state, returning to the command loop."
  (funcall (editor-input-get editor-input) editor-input ignore-abort-attempts-p))

;;; UNGET-KEY-EVENT -- Public.
;;;
(defun unget-key-event (key-event editor-input)
  "This function returns the key-event to editor-input, so the next invocation
   of GET-KEY-EVENT will return the key-event.  If the key-event is #k\"C-g\"
   or #k\"C-G\", then whether GET-KEY-EVENT returns it depends on its second
   argument.  Editor-input is either *editor-input* or *real-editor-input*."
  (funcall (editor-input-unget editor-input) key-event editor-input))

;;; CLEAR-EDITOR-INPUT -- Public.
;;;
(defun clear-editor-input (editor-input)
  "This function flushes any pending input on editor-input.  Editor-input
   is either *editor-input* or *real-editor-input*."
  (funcall (editor-input-clear editor-input) editor-input))

;;; LISTEN-EDITOR-INPUT -- Public.
;;;
(defun listen-editor-input (editor-input)
  "This function returns whether there is any input available on editor-input.
   Editor-input is either *editor-input* or *real-editor-input*."
  (funcall (editor-input-listen editor-input) editor-input))



;;;; LAST-KEY-EVENT-CURSORPOS and WINDOW-INPUT-HANDLER.

;;; LAST-KEY-EVENT-CURSORPOS  --  Public
;;;
;;; Just look up the saved info in the last read key event.
;;;
(defun last-key-event-cursorpos ()
  "Return as values, the (X, Y) character position and window where the
   last key event happened.  If this cannot be determined, Nil is returned.
   If in the modeline, return a Y position of NIL and the correct X and window.
   Returns nil for terminal input."
  (let* ((ev (editor-input-head *real-editor-input*))
	 (hunk (input-event-hunk ev))
	 (window (and hunk (device-hunk-window hunk))))
    (when window
      (values (input-event-x ev) (input-event-y ev) window))))

;;; WINDOW-INPUT-HANDLER  --  Internal
;;;
;;; This is the input-handler function for hunks that implement windows.  It
;;; just queues the events on *real-editor-input*.
;;;
(defun window-input-handler (hunk char x y)
  (q-event *real-editor-input* char x y hunk))



;;;; Random typeout input routines.

(defun wait-for-more (stream)
  (let ((key-event (more-read-key-event)))
    (cond ((logical-key-event-p key-event :yes))
	  ((or (logical-key-event-p key-event :do-all)
	       (logical-key-event-p key-event :exit))
	   (setf (random-typeout-stream-no-prompt stream) t)
	   (random-typeout-cleanup stream))
	  ((logical-key-event-p key-event :keep)
	   (setf (random-typeout-stream-no-prompt stream) t)
	   (maybe-keep-random-typeout-window stream)
	   (random-typeout-cleanup stream))
	  ((logical-key-event-p key-event :no)
	   (random-typeout-cleanup stream)
	   (throw 'more-punt nil))
	  (t
	   (unget-key-event key-event *editor-input*)
	   (random-typeout-cleanup stream)
	   (throw 'more-punt nil)))))

(declaim (special *more-prompt-action*))

(defun maybe-keep-random-typeout-window (stream)
  (let* ((window (random-typeout-stream-window stream))
	 (buffer (window-buffer window))
	 (start (buffer-start-mark buffer)))
    (when (typep (hi::device-hunk-device (hi::window-hunk window))
		 'hi::bitmap-device)
      (let ((*more-prompt-action* :normal))
	(update-modeline-field buffer window :more-prompt)
	(random-typeout-redisplay window))
      (buffer-start (buffer-point buffer))
      (let* ((xwindow (make-xwindow-like-hwindow window))
	     (window (make-window start :window xwindow)))
	(unless window
	  #+clx(xlib:destroy-window xwindow)
	  (editor-error "Could not create random typeout window."))))))

(defun end-random-typeout (stream)
  (let ((*more-prompt-action* :flush)
	(window (random-typeout-stream-window stream)))
    (update-modeline-field (window-buffer window) window :more-prompt)
    (random-typeout-redisplay window))
  (unless (random-typeout-stream-no-prompt stream)
    (let* ((key-event (more-read-key-event))
	   (keep-p (logical-key-event-p key-event :keep)))
      (when keep-p (maybe-keep-random-typeout-window stream))
      (random-typeout-cleanup stream)
      (unless (or (logical-key-event-p key-event :do-all)
		  (logical-key-event-p key-event :exit)
		  (logical-key-event-p key-event :no)
		  (logical-key-event-p key-event :yes)
		  keep-p)
	(unget-key-event key-event *editor-input*)))))

;;; MORE-READ-KEY-EVENT -- Internal.
;;;
;;; This gets some input from the type of stream bound to *editor-input*.  Need
;;; to loop over SERVE-EVENT since it returns on any kind of event (not
;;; necessarily a key or button event).
;;;
;;; Currently this does not work for keyboard macro streams!
;;; 
(defun more-read-key-event ()
  (clear-editor-input *editor-input*)
  (let ((key-event (loop
		     (let ((key-event (dq-event *editor-input*)))
		       (when key-event (return key-event))
		       (hemlock-ext:serve-event)))))
    (when (abort-key-event-p key-event)
      (beep)
      (throw 'editor-top-level-catcher nil))
    key-event))
