;;; -*- Package: Extensions; Log: code.log; Mode: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; This file contains code to extend CLX in the CMU Common Lisp environment.
;;;
;;; Written by Bill Chiles and Chris Hoover.
;;;

(in-package "EXTENSIONS")

(export '(open-clx-display with-clx-event-handling enable-clx-event-handling
	  disable-clx-event-handling object-set-event-handler
	  default-clx-event-handler
	  flush-display-events carefully-add-font-paths

	  serve-key-press serve-key-release serve-button-press
	  serve-button-release serve-motion-notify serve-enter-notify
	  serve-leave-notify serve-focus-in serve-focus-out 
	  serve-exposure serve-graphics-exposure serve-no-exposure
	  serve-visibility-notify serve-create-notify serve-destroy-notify
	  serve-unmap-notify serve-map-notify serve-map-request
	  serve-reparent-notify serve-configure-notify serve-gravity-notify
	  serve-resize-request serve-configure-request serve-circulate-notify
	  serve-circulate-request serve-property-notify serve-selection-clear
	  serve-selection-request serve-selection-notify serve-colormap-notify
	  serve-client-message))



;;;; OPEN-CLX-DISPLAY.

(defun open-clx-display (&optional (string (cdr (assoc :display
						       *environment-list*
						       :test #'eq))))
  "Parses a display specification including display and screen numbers.
   This returns nil when there is no DISPLAY environment variable.  If string
   is non-nil, and any fields are missing in the specification, this signals an
   error.  If you specify a screen, then this sets XLIB:DISPLAY-DEFAULT-SCREEN
   to that screen since CLX initializes this form to the first of
   XLIB:SCREEN-ROOTS.  This returns the display and screen objects."
  (when string
    (let* ((string (coerce string 'simple-string))
	   (length (length string))
	   ;;pw-- "unix" is a signal to the connect_to_inet C code
	   ;;     to open an AF_UNIX socket instead of an AF_INET one.
	   ;;     This is supposed to be faster on a local server.
	   (host-name "unix")
	   (auth-name nil)
	   (auth-data nil)
	   (display-num nil)
	   (screen-num nil))
      (declare (simple-string string))
      (let ((colon (position #\: string :test #'char=)))
	(cond ((null colon)
	       (error "Missing display number in DISPLAY environment variable."))
	      (t
	       (unless (zerop colon) (setf host-name (subseq string 0 colon)))
	       (let* ((start (1+ colon))
		      (first-dot (position #\. string
					   :test #'char= :start start)))
		 (cond ((= start (or first-dot length))
			(error "Badly formed display number in DISPLAY ~
				environment variable."))
		       ((null first-dot)
			(setf display-num (parse-integer string :start start)))
		       (t
			(setf display-num (parse-integer string :start start
							 :end first-dot))
			(let* ((start (1+ first-dot))
			       (second-dot (position #\. string :test #'char=
						     :start start)))
			  (cond ((= start (or second-dot length))
				 (error "Badly formed screen number in ~
					 DISPLAY environment variable."))
				(t
				 (setf screen-num
				       (parse-integer string :start start
						      :end second-dot)))))))))))
      (if (equal host-name "unix")
        (multiple-value-setq (auth-name auth-data)
          (xlib::get-best-authorization (machine-instance) display-num :tcp)))
      (let ((display (xlib:open-display host-name
                                      :display display-num
                                      :authorization-name auth-name
                                      :authorization-data auth-data)))
	(when screen-num
	  (let* ((screens (xlib:display-roots display))
		 (num-screens (length screens)))
	    (when (>= screen-num num-screens)
	      (xlib:close-display display)
	      (error "No such screen number (~D)." screen-num))
	    (setf (xlib:display-default-screen display)
		  (elt screens screen-num))))
	(values display (xlib:display-default-screen display))))))


;;;; Font Path Manipulation

(defun carefully-add-font-paths (display font-pathnames
					 &optional (operation :append))
  "Adds the list of font pathnames, Font-Pathnames, to the font path of
  the server Display but does so carefully by checking to make sure that
  the font pathnames are not already on the server's font path.  If any
  of the font pathnames are on the server's font path, they will remain
  in their current positions.  Operation may be specified as either
  :prepend or :append and specifies whether to add the additional font
  pathnames to the beginning or the end of the server's original font
  path."
  (let ((font-path (xlib:font-path display))
	(result ()))
    (dolist (elt font-pathnames)
      (enumerate-search-list (pathname elt)
	(lisp::enumerate-matches (name pathname)
	  (unless (member name font-path :test #'string=)
	    (push name result)))))
    (when result
      (ecase operation
	(:prepend
	 (setf (xlib:font-path display) (revappend result font-path)))
	(:append
	 (setf (xlib:font-path display)
	       (append font-path (nreverse result))))))))


;;;; Enabling and disabling event handling through SYSTEM:SERVE-EVENT.

(defvar *clx-fds-to-displays* (make-hash-table :test #'eql)
  "This is a hash table that maps CLX file descriptors to CLX display
   structures.  For every CLX file descriptor know to SYSTEM:SERVE-EVENT,
   there must be a mapping from that file descriptor to its CLX display
   structure when events are handled via SYSTEM:SERVE-EVENT.")

(defmacro with-clx-event-handling ((display handler) &rest body)
  "Evaluates body in a context where events are handled for the display
   by calling handler on the display.  This destroys any previously established
   handler for display."
  `(unwind-protect
       (progn
	 (enable-clx-event-handling ,display ,handler)
	 ,@body)
     (disable-clx-event-handling ,display)))

;;; ENABLE-CLX-EVENT-HANDLING associates the display with the handler in
;;; *display-event-handlers*.  It also uses SYSTEM:ADD-FD-HANDLER to have
;;; SYSTEM:SERVE-EVENT call CALL-DISPLAY-EVENT-HANDLER whenever anything shows
;;; up from the display. Since CALL-DISPLAY-EVENT-HANDLER is called on a
;;; file descriptor, the file descriptor is also mapped to the display in
;;; *clx-fds-to-displays*, so the user's handler can be called on the display.
;;;
(defun enable-clx-event-handling (display handler)
  "After calling this, when SYSTEM:SERVE-EVENT notices input on display's
   connection to the X11 server, handler is called on the display.  Handler
   is invoked in a dynamic context with an error handler bound that will
   flush all events from the display and return.  By returning, it declines
   to handle the error, but it will have cleared all events; thus, entering
   the debugger will not result in infinite errors due to streams that wait
   via SYSTEM:SERVE-EVENT for input.  Calling this repeatedly on the same
   display establishes handler as a new handler, replacing any previous one
   for display."
  (check-type display xlib:display)
  (let ((change-handler (assoc display *display-event-handlers*)))
    (if change-handler
	(setf (cdr change-handler) handler)
	(let ((fd (fd-stream-fd (xlib::display-input-stream display))))
	  (system:add-fd-handler fd :input #'call-display-event-handler)
	  (setf (gethash fd *clx-fds-to-displays*) display)
	  (push (cons display handler) *display-event-handlers*)))))

;;; CALL-DISPLAY-EVENT-HANDLER maps the file descriptor to its display and maps
;;; the display to its handler.  If we can't find the display, we remove the
;;; file descriptor using SYSTEM:INVALIDATE-DESCRIPTOR and try to remove the
;;; display from *display-event-handlers*.  This is necessary to try to keep
;;; SYSTEM:SERVE-EVENT from repeatedly trying to handle the same event over and
;;; over.  This is possible since many CMU Common Lisp streams loop over
;;; SYSTEM:SERVE-EVENT, so when the debugger is entered, infinite errors are
;;; possible.
;;;
(defun call-display-event-handler (file-descriptor)
  (let ((display (gethash file-descriptor *clx-fds-to-displays*)))
    (unless display
      (system:invalidate-descriptor file-descriptor)
      (setf *display-event-handlers*
	    (delete file-descriptor *display-event-handlers*
		    :key #'(lambda (d/h)
			     (fd-stream-fd
			      (xlib::display-input-stream
			       (car d/h))))))
      (error "File descriptor ~S not associated with any CLX display.~%~
                It has been removed from system:serve-event's knowledge."
	     file-descriptor))
    (let ((handler (cdr (assoc display *display-event-handlers*))))
      (unless handler
	(flush-display-events display)
	(error "Display ~S not associated with any event handler." display))
      (handler-bind ((error #'(lambda (condx)
				(declare (ignore condx))
				(flush-display-events display))))
	(funcall handler display)))))

(defun disable-clx-event-handling (display)
  "Undoes the effect of EXT:ENABLE-CLX-EVENT-HANDLING."
  (setf *display-event-handlers*
	(delete display *display-event-handlers* :key #'car))
  (let ((fd (fd-stream-fd (xlib::display-input-stream display))))
    (remhash fd *clx-fds-to-displays*)
    (system:invalidate-descriptor fd)))



;;;; Object set event handling.

;;; This is bound by OBJECT-SET-EVENT-HANDLER, so DISPATCH-EVENT can clear
;;; events on the display before signalling any errors.  This is necessary
;;; since reading on certain CMU Common Lisp streams involves SERVER, and
;;; getting an error while trying to handle an event causes repeated attempts
;;; to handle the same event.
;;;
(defvar *process-clx-event-display* nil)

(defvar *object-set-event-handler-print* nil)

(declaim (declaration values))

(defun object-set-event-handler (display)
  "This display event handler uses object sets to map event windows cross
   event types to handlers.  It uses XLIB:EVENT-CASE to bind all the slots
   of each event, calling the handlers on all these values in addition to
   the event key and send-event-p.  Describe EXT:SERVE-MUMBLE, where mumble
   is an event keyword name for the exact order of arguments.
   :mapping-notify and :keymap-notify events are ignored since they do not
   occur on any particular window.  After calling a handler, each branch
   returns t to discard the event.  While the handler is executing, all
   errors go through a handler that flushes all the display's events and
   returns.  This prevents infinite errors since the debug and terminal
   streams loop over SYSTEM:SERVE-EVENT.  This function returns t if there
   were some event to handle, nil otherwise.  It returns immediately if
   there is no event to handle."
  (macrolet ((dispatch (event-key &rest args)
	       `(multiple-value-bind (object object-set)
				     (lisp::map-xwindow event-window)
		  (unless object
		    (cond ((not (typep event-window 'xlib:window))
			   (xlib:discard-current-event display)
			   (warn "Discarding ~S event on non-window ~S."
				 ,event-key event-window)
			   (return-from object-set-event-handler nil))
			  (t
			   (flush-display-events display)
			   (error "~S not a known X window.~%~
			           Received event ~S."
				  event-window ,event-key))))
		  (handler-bind ((error #'(lambda (condx)
					    (declare (ignore condx))
					    (flush-display-events display))))
		    (when *object-set-event-handler-print*
		      (print ,event-key) (force-output))
		    (funcall (gethash ,event-key
				      (lisp::object-set-table object-set)
				      (lisp::object-set-default-handler
				       object-set))
			     object ,event-key
			     ,@args))
		  (setf result t))))
    (let ((*process-clx-event-display* display)
	  (result nil))
      (xlib:event-case (display :timeout 0)
	((:KEY-PRESS :KEY-RELEASE :BUTTON-PRESS :BUTTON-RELEASE)
	     (event-key event-window root child same-screen-p
	      x y root-x root-y state time code send-event-p)
	 (dispatch event-key event-window root child same-screen-p
		   x y root-x root-y state time code send-event-p))
	(:MOTION-NOTIFY (event-window root child same-screen-p
			 x y root-x root-y state time hint-p send-event-p)
	 (dispatch :motion-notify event-window root child same-screen-p
		   x y root-x root-y state time hint-p send-event-p))
	(:ENTER-NOTIFY (event-window root child same-screen-p
			x y root-x root-y state time mode kind send-event-p)
	 (dispatch :enter-notify event-window root child same-screen-p
		   x y root-x root-y state time mode kind send-event-p))
	(:LEAVE-NOTIFY (event-window root child same-screen-p
			x y root-x root-y state time mode kind send-event-p)
	 (dispatch :leave-notify event-window root child same-screen-p
		   x y root-x root-y state time mode kind send-event-p))
	(:EXPOSURE (event-window x y width height count send-event-p)
	 (dispatch :exposure event-window x y width height count send-event-p))
	(:GRAPHICS-EXPOSURE (event-window x y width height count major minor
			     send-event-p)
	 (dispatch :graphics-exposure event-window x y width height
		   count major minor send-event-p))
	(:NO-EXPOSURE (event-window major minor send-event-p)
	 (dispatch :no-exposure event-window major minor send-event-p))
	(:FOCUS-IN (event-window mode kind send-event-p)
	 (dispatch :focus-in event-window mode kind send-event-p))
	(:FOCUS-OUT (event-window mode kind send-event-p)
	 (dispatch :focus-out event-window mode kind send-event-p))
	(:KEYMAP-NOTIFY ()
	 (warn "Ignoring keymap notify event.")
	 (when *object-set-event-handler-print*
	   (print :keymap-notify) (force-output))
	 (setf result t))
	(:VISIBILITY-NOTIFY (event-window state send-event-p)
	 (dispatch :visibility-notify event-window state send-event-p))
	(:CREATE-NOTIFY (event-window window x y width height border-width
			 override-redirect-p send-event-p)
	 (dispatch :create-notify event-window window x y width height
		   border-width override-redirect-p send-event-p))
	(:DESTROY-NOTIFY (event-window window send-event-p)
	 (dispatch :destroy-notify event-window window send-event-p))
	(:UNMAP-NOTIFY (event-window window configure-p send-event-p)
	 (dispatch :unmap-notify event-window window configure-p send-event-p))
	(:MAP-NOTIFY (event-window window override-redirect-p send-event-p)
	 (dispatch :map-notify event-window window override-redirect-p
		   send-event-p))
	(:MAP-REQUEST (event-window window send-event-p)
	 (dispatch :map-request event-window window send-event-p))
	(:REPARENT-NOTIFY (event-window window parent x y override-redirect-p
			   send-event-p)
	 (dispatch :reparent-notify event-window window parent x y
		   override-redirect-p send-event-p))
	(:CONFIGURE-NOTIFY (event-window window x y width height border-width
			    above-sibling override-redirect-p send-event-p)
	 (dispatch :configure-notify event-window window x y width height
		   border-width above-sibling override-redirect-p
		   send-event-p))
	(:GRAVITY-NOTIFY (event-window window x y send-event-p)
	 (dispatch :gravity-notify event-window window x y send-event-p))
	(:RESIZE-REQUEST (event-window width height send-event-p)
	 (dispatch :resize-request event-window width height send-event-p))
	(:CONFIGURE-REQUEST (event-window window x y width height border-width
			     stack-mode above-sibling value-mask send-event-p)
	 (dispatch :configure-request event-window window x y width height
		   border-width stack-mode above-sibling value-mask
		   send-event-p))
	(:CIRCULATE-NOTIFY (event-window window place send-event-p)
	 (dispatch :circulate-notify event-window window place send-event-p))
	(:CIRCULATE-REQUEST (event-window window place send-event-p)
	 (dispatch :circulate-request event-window window place send-event-p))
	(:PROPERTY-NOTIFY (event-window atom state time send-event-p)
	 (dispatch :property-notify event-window atom state time send-event-p))
	(:SELECTION-CLEAR (event-window selection time send-event-p)
	 (dispatch :selection-notify event-window selection time send-event-p))
	(:SELECTION-REQUEST (event-window requestor selection target property
			     time send-event-p)
	 (dispatch :selection-request event-window requestor selection target
		   property time send-event-p))
	(:SELECTION-NOTIFY (event-window selection target property time
			    send-event-p)
	 (dispatch :selection-notify event-window selection target property time
		   send-event-p))
	(:COLORMAP-NOTIFY (event-window colormap new-p installed-p send-event-p)
	 (dispatch :colormap-notify event-window colormap new-p installed-p
		   send-event-p))
	(:MAPPING-NOTIFY (request)
	 (warn "Ignoring mapping notify event -- ~S." request)
	 (when *object-set-event-handler-print*
	   (print :mapping-notify) (force-output))
	 (setf result t))
	(:CLIENT-MESSAGE (event-window format data send-event-p)
	 (dispatch :client-message event-window format data send-event-p)))
      result)))

(defun default-clx-event-handler (object event-key event-window &rest ignore)
  (declare (ignore ignore))
  (flush-display-events *process-clx-event-display*)
  (error "No handler for event type ~S on ~S in ~S."
	 event-key object (lisp::map-xwindow event-window)))

(defun flush-display-events (display)
  "Dumps all the events in display's event queue including the current one
   in case this is called from within XLIB:EVENT-CASE, etc."
  (xlib:discard-current-event display)
  (xlib:event-case (display :discard-p t :timeout 0)
    (t () nil)))


