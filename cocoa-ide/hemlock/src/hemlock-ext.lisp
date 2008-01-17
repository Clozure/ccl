;;; -*- Mode: LISP; Package: HEMLOCK-EXT -*-

(in-package :hemlock-ext)

(defconstant hi::char-code-limit 256)
(defconstant char-code-limit 256)

(defmacro file-comment (&rest ignore)
  (declare (ignore ignore))
  nil)

(defun skip-whitespace (&optional (stream *standard-input*))
  (peek-char t stream))

#+clx
(defun disable-clx-event-handling (display)
  )

(defun quit ()
  )

(defun sap-ref-8 (vec index)
  (declare (ignore vec index))
  (error "SAP-REF-8 called.") )

(defvar hi::*command-line-switches* nil)

(defun hi::get-terminal-name ()
  "vt100")

(defun hi::get-termcap-env-var ()
  (getenv "TERMCAP"))

(defun default-directory ()
  "Returns the pathname for the default directory.  This is the place where
  a file will be written if no directory is specified.  This may be changed
  with setf."
  (truename #p""))

;;;;;;;;;;;;

(defstruct (object-set (:constructor make-object-set (name &optional default-handler)))
  name
  default-handler
  (table (make-hash-table)))

(defvar *xwindow-hash* (make-hash-table :test #'eq))

(defun hi::add-xwindow-object (window object object-set)
  (setf (gethash window *xwindow-hash*) (list object object-set)))

(defun hi::remove-xwindow-object (window)
  (remhash window *xwindow-hash*))

(defun lisp--map-xwindow (window)
  ;; -> object object-set
  (values-list (gethash window *xwindow-hash*)))


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

#+clx
(defun object-set-event-handler (display &optional (timeout 0))
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
                 (lisp--map-xwindow event-window)
                 (unless object
                   (cond ((not (typep event-window 'xlib:window))
                          ;;(xlib:discard-current-event display)
                          (warn "Discarding ~S event on non-window ~S."
                                ,event-key event-window)
                          (return-from object-set-event-handler nil)
                          )
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
                                     (object-set-table object-set)
                                     (object-set-default-handler
                                      object-set))
                            object ,event-key
                            ,@args))
                 (setf result t))))
    (let ((*process-clx-event-display* display)
          (result nil))
      (xlib:event-case (display :timeout timeout)
                       ((:key-press :key-release :button-press :button-release)
                        (event-key event-window root child same-screen-p
                                   x y root-x root-y state time code send-event-p)
                        (dispatch event-key event-window root child same-screen-p
                                  x y root-x root-y state time code send-event-p))
                       (:motion-notify (event-window root child same-screen-p
                                        x y root-x root-y state time hint-p send-event-p)
                        (dispatch :motion-notify event-window root child same-screen-p
                         x y root-x root-y state time hint-p send-event-p))
                       (:enter-notify (event-window root child same-screen-p
                                       x y root-x root-y state time mode kind send-event-p)
                        (dispatch :enter-notify event-window root child same-screen-p
                         x y root-x root-y state time mode kind send-event-p))
                       (:leave-notify (event-window root child same-screen-p
                                       x y root-x root-y state time mode kind send-event-p)
                        (dispatch :leave-notify event-window root child same-screen-p
                         x y root-x root-y state time mode kind send-event-p))
                       (:exposure (event-window x y width height count send-event-p)
                        (dispatch :exposure event-window x y width height count send-event-p))
                       (:graphics-exposure (event-window x y width height count major minor
                                            send-event-p)
                        (dispatch :graphics-exposure event-window x y width height
                         count major minor send-event-p))
                       (:no-exposure (event-window major minor send-event-p)
                        (dispatch :no-exposure event-window major minor send-event-p))
                       (:focus-in (event-window mode kind send-event-p)
                        (dispatch :focus-in event-window mode kind send-event-p))
                       (:focus-out (event-window mode kind send-event-p)
                        (dispatch :focus-out event-window mode kind send-event-p))
                       (:keymap-notify ()
                        (warn "Ignoring keymap notify event.")
                        (when *object-set-event-handler-print*
                          (print :keymap-notify) (force-output))
                        (setf result t))
                       (:visibility-notify (event-window state send-event-p)
                        (dispatch :visibility-notify event-window state send-event-p))
                       (:create-notify (event-window window x y width height border-width
                                        override-redirect-p send-event-p)
                        (dispatch :create-notify event-window window x y width height
                         border-width override-redirect-p send-event-p))
                       (:destroy-notify (event-window window send-event-p)
                        (dispatch :destroy-notify event-window window send-event-p))
                       (:unmap-notify (event-window window configure-p send-event-p)
                        (dispatch :unmap-notify event-window window configure-p send-event-p))
                       (:map-notify (event-window window override-redirect-p send-event-p)
                        (dispatch :map-notify event-window window override-redirect-p
                         send-event-p))
                       (:map-request (event-window window send-event-p)
                        (dispatch :map-request event-window window send-event-p))
                       (:reparent-notify (event-window window parent x y override-redirect-p
                                          send-event-p)
                        (dispatch :reparent-notify event-window window parent x y
                         override-redirect-p send-event-p))
                       (:configure-notify (event-window window x y width height border-width
                                           above-sibling override-redirect-p send-event-p)
                        (dispatch :configure-notify event-window window x y width height
                         border-width above-sibling override-redirect-p
                         send-event-p))
                       (:gravity-notify (event-window window x y send-event-p)
                        (dispatch :gravity-notify event-window window x y send-event-p))
                       (:resize-request (event-window width height send-event-p)
                        (dispatch :resize-request event-window width height send-event-p))
                       (:configure-request (event-window window x y width height border-width
                                            stack-mode above-sibling value-mask send-event-p)
                        (dispatch :configure-request event-window window x y width height
                         border-width stack-mode above-sibling value-mask
                         send-event-p))
                       (:circulate-notify (event-window window place send-event-p)
                        (dispatch :circulate-notify event-window window place send-event-p))
                       (:circulate-request (event-window window place send-event-p)
                        (dispatch :circulate-request event-window window place send-event-p))
                       (:property-notify (event-window atom state time send-event-p)
                        (dispatch :property-notify event-window atom state time send-event-p))
                       (:selection-clear (event-window selection time send-event-p)
                        (dispatch :selection-notify event-window selection time send-event-p))
                       (:selection-request (event-window requestor selection target property
                                            time send-event-p)
                        (dispatch :selection-request event-window requestor selection target
                         property time send-event-p))
                       (:selection-notify (event-window selection target property time
                                           send-event-p)
                        (dispatch :selection-notify event-window selection target property time
                         send-event-p))
                       (:colormap-notify (event-window colormap new-p installed-p send-event-p)
                        (dispatch :colormap-notify event-window colormap new-p installed-p
                         send-event-p))
                       (:mapping-notify (request)
                        (warn "Ignoring mapping notify event -- ~S." request)
                        (when *object-set-event-handler-print*
                          (print :mapping-notify) (force-output))
                        (setf result t))
                       (:client-message (event-window format data send-event-p)
                        (dispatch :client-message event-window format data send-event-p)))
      result)))

#+clx
(defun default-clx-event-handler (object event-key event-window &rest ignore)
  (declare (ignore ignore))
  (flush-display-events *process-clx-event-display*)
  (error "No handler for event type ~S on ~S in ~S."
	 event-key object (lisp--map-xwindow event-window)))

#+clx
(defun flush-display-events (display)
  "Dumps all the events in display's event queue including the current one
   in case this is called from within XLIB:EVENT-CASE, etc."
  (xlib:discard-current-event display)
  (xlib:event-case (display :discard-p t :timeout 0)
    (t () nil)))

#+clx
(defmacro with-clx-event-handling ((display handler) &rest body)
  "Evaluates body in a context where events are handled for the display
   by calling handler on the display.  This destroys any previously established
   handler for display."
  `(unwind-protect
       (progn
	 (enable-clx-event-handling ,display ,handler)
	 ,@body)
     (disable-clx-event-handling ,display) ))

#+clx
(defun enable-clx-event-handling (display handler)
  nil)

#+clx
(defun disable-clx-event-handling (display)
  nil)

#||
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
||#


;;;; Key and button service.

(defun serve-key-press (object-set fun)
  "Associate a method in the object-set with :key-press events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :key-press (object-set-table object-set)) fun))

(defun serve-key-release (object-set fun)
  "Associate a method in the object-set with :key-release events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :key-release (object-set-table object-set)) fun))

(defun serve-button-press (object-set fun)
  "Associate a method in the object-set with :button-press events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :button-press (object-set-table object-set)) fun))

(defun serve-button-release (object-set fun)
  "Associate a method in the object-set with :button-release events.  The
   method is called on the object the event occurred, event key, event window,
   root, child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :button-release (object-set-table object-set)) fun))



;;;; Mouse service.

(defun serve-motion-notify (object-set fun)
  "Associate a method in the object-set with :motion-notify events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, hint-p, and
   send-event-p."
  (setf (gethash :motion-notify (object-set-table object-set)) fun))

(defun serve-enter-notify (object-set fun)
  "Associate a method in the object-set with :enter-notify events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, mode, kind,
   and send-event-p."
  (setf (gethash :enter-notify (object-set-table object-set)) fun))

(defun serve-leave-notify (object-set fun)
  "Associate a method in the object-set with :leave-notify events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, mode, kind,
   and send-event-p."
  (setf (gethash :leave-notify (object-set-table object-set)) fun))



;;;; Keyboard service.

(defun serve-focus-in (object-set fun)
  "Associate a method in the object-set with :focus-in events.  The method
   is called on the object the event occurred, event key, event window, mode,
   kind, and send-event-p."
  (setf (gethash :focus-in (object-set-table object-set)) fun))

(defun serve-focus-out (object-set fun) 
  "Associate a method in the object-set with :focus-out events.  The method
   is called on the object the event occurred, event key, event window, mode,
   kind, and send-event-p."
  (setf (gethash :focus-out (object-set-table object-set)) fun))



;;;; Exposure service.

(defun serve-exposure (object-set fun)
  "Associate a method in the object-set with :exposure events.  The method
   is called on the object the event occurred, event key, event window, x, y,
   width, height, count, and send-event-p."
  (setf (gethash :exposure (object-set-table object-set)) fun))

(defun serve-graphics-exposure (object-set fun)
  "Associate a method in the object-set with :graphics-exposure events.  The
   method is called on the object the event occurred, event key, event window,
   x, y, width, height, count, major, minor, and send-event-p."
  (setf (gethash :graphics-exposure (object-set-table object-set)) fun))

(defun serve-no-exposure (object-set fun)
  "Associate a method in the object-set with :no-exposure events.  The method
   is called on the object the event occurred, event key, event window, major,
   minor, and send-event-p."
  (setf (gethash :no-exposure (object-set-table object-set)) fun))
  


;;;; Structure service.

(defun serve-visibility-notify (object-set fun)
  "Associate a method in the object-set with :visibility-notify events.  The
   method is called on the object the event occurred, event key, event window,
   state, and send-event-p."
  (setf (gethash :visibility-notify (object-set-table object-set)) fun))

(defun serve-create-notify (object-set fun)
  "Associate a method in the object-set with :create-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, x, y, width, height, border-width, override-redirect-p, and
   send-event-p."
  (setf (gethash :create-notify (object-set-table object-set)) fun))

(defun serve-destroy-notify (object-set fun)
  "Associate a method in the object-set with :destroy-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, and send-event-p."
  (setf (gethash :destroy-notify (object-set-table object-set)) fun))

(defun serve-unmap-notify (object-set fun)
  "Associate a method in the object-set with :unmap-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, configure-p, and send-event-p."
  (setf (gethash :unmap-notify (object-set-table object-set)) fun))

(defun serve-map-notify (object-set fun)
  "Associate a method in the object-set with :map-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, override-redirect-p, and send-event-p."
  (setf (gethash :map-notify (object-set-table object-set)) fun))

(defun serve-map-request (object-set fun)
  "Associate a method in the object-set with :map-request events.  The
   method is called on the object the event occurred, event key, event window,
   window, and send-event-p."
  (setf (gethash :map-request (object-set-table object-set)) fun))

(defun serve-reparent-notify (object-set fun)
  "Associate a method in the object-set with :reparent-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, parent, x, y, override-redirect-p, and send-event-p."
  (setf (gethash :reparent-notify (object-set-table object-set)) fun))

(defun serve-configure-notify (object-set fun)
  "Associate a method in the object-set with :configure-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, x, y, width, height, border-width, above-sibling,
   override-redirect-p, and send-event-p."
  (setf (gethash :configure-notify (object-set-table object-set)) fun))

(defun serve-gravity-notify (object-set fun)
  "Associate a method in the object-set with :gravity-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, x, y, and send-event-p."
  (setf (gethash :gravity-notify (object-set-table object-set)) fun))

(defun serve-resize-request (object-set fun)
  "Associate a method in the object-set with :resize-request events.  The
   method is called on the object the event occurred, event key, event window,
   width, height, and send-event-p."
  (setf (gethash :resize-request (object-set-table object-set)) fun))

(defun serve-configure-request (object-set fun)
  "Associate a method in the object-set with :configure-request events.  The
   method is called on the object the event occurred, event key, event window,
   window, x, y, width, height, border-width, stack-mode, above-sibling,
   value-mask, and send-event-p."
  (setf (gethash :configure-request (object-set-table object-set)) fun))

(defun serve-circulate-notify (object-set fun)
  "Associate a method in the object-set with :circulate-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, place, and send-event-p."
  (setf (gethash :circulate-notify (object-set-table object-set)) fun))

(defun serve-circulate-request (object-set fun)
  "Associate a method in the object-set with :circulate-request events.  The
   method is called on the object the event occurred, event key, event window,
   window, place, and send-event-p."
  (setf (gethash :circulate-request (object-set-table object-set)) fun))



;;;; Misc. service.

(defun serve-property-notify (object-set fun)
  "Associate a method in the object-set with :property-notify events.  The
   method is called on the object the event occurred, event key, event window,
   atom, state, time, and send-event-p."
  (setf (gethash :property-notify (object-set-table object-set)) fun))

(defun serve-selection-clear (object-set fun)
  "Associate a method in the object-set with :selection-clear events.  The
   method is called on the object the event occurred, event key, event window,
   selection, time, and send-event-p."
  (setf (gethash :selection-clear (object-set-table object-set)) fun))

(defun serve-selection-request (object-set fun)
  "Associate a method in the object-set with :selection-request events.  The
   method is called on the object the event occurred, event key, event window,
   requestor, selection, target, property, time, and send-event-p."
  (setf (gethash :selection-request (object-set-table object-set)) fun))

(defun serve-selection-notify (object-set fun)
  "Associate a method in the object-set with :selection-notify events.  The
   method is called on the object the event occurred, event key, event window,
   selection, target, property, time, and send-event-p."
  (setf (gethash :selection-notify (object-set-table object-set)) fun))

(defun serve-colormap-notify (object-set fun)
  "Associate a method in the object-set with :colormap-notify events.  The
   method is called on the object the event occurred, event key, event window,
   colormap, new-p, installed-p, and send-event-p."
  (setf (gethash :colormap-notify (object-set-table object-set)) fun))

(defun serve-client-message (object-set fun)
  "Associate a method in the object-set with :client-message events.  The
   method is called on the object the event occurred, event key, event window,
   format, data, and send-event-p."
  (setf (gethash :client-message (object-set-table object-set)) fun))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hi::%sp-byte-blt (src start dest dstart end)
  (declare (type (simple-base-string src dest)))
  (loop for s from start
        for d from dstart below end
        do
        (setf (aref dest d) (aref src s))))

#+clx
(defun serve-event (&optional timeout)
  (let ((dps))
    (maphash (lambda (win value)
               (pushnew (xlib:window-display win) dps))
             *xwindow-hash*)
    (when dps
      (object-set-event-handler (car dps) timeout))))

#+CLISP
(progn

  #-NIL
  (defun serve-event (&optional timeout)
    (hemlock.wire::serve-event timeout))

;;; ENABLE-CLX-EVENT-HANDLING associates the display with the handler in
;;; *display-event-handlers*.  It also uses SYSTEM:ADD-FD-HANDLER to have
;;; SYSTEM:SERVE-EVENT call CALL-DISPLAY-EVENT-HANDLER whenever anything shows
;;; up from the display. Since CALL-DISPLAY-EVENT-HANDLER is called on a
;;; file descriptor, the file descriptor is also mapped to the display in
;;; *clx-fds-to-displays*, so the user's handler can be called on the display.
;;;

  (defvar *display-event-handlers* nil)

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
          (setf (cadr change-handler) handler)
          (let ((fd-handler
                 (hemlock.wire::add-fd-handler display :input #'call-display-event-handler)))
            (push (list display handler fd-handler) *display-event-handlers*)))))

;;; CALL-DISPLAY-EVENT-HANDLER maps the file descriptor to its display and maps
;;; the display to its handler.  If we can't find the display, we remove the
;;; file descriptor using SYSTEM:INVALIDATE-DESCRIPTOR and try to remove the
;;; display from *display-event-handlers*.  This is necessary to try to keep
;;; SYSTEM:SERVE-EVENT from repeatedly trying to handle the same event over and
;;; over.  This is possible since many CMU Common Lisp streams loop over
;;; SYSTEM:SERVE-EVENT, so when the debugger is entered, infinite errors are
;;; possible.
;;;
  (defun call-display-event-handler (display)
    (let ((handler (cadr (assoc display *display-event-handlers*))))
      (unless handler
        (flush-display-events display)
        (error "Display ~S not associated with any event handler." display))
      (handler-bind ((error #'(lambda (condx)
                                (declare (ignore condx))
                                (flush-display-events display))))
        (funcall handler display))))

  (defun disable-clx-event-handling (display)
    "Undoes the effect of EXT:ENABLE-CLX-EVENT-HANDLING."
    (let ((change-handler (assoc display *display-event-handlers*)))
      (when change-handler
        (hemlock.wire::remove-fd-handler (third change-handler))))
    (setf *display-event-handlers*
          (delete display *display-event-handlers* :key #'car))
    ) )


;;(trace object-set-event-handler hi::invoke-scheduled-events hi::next-scheduled-event-wait serve-event)

(defun hi::%sp-find-character-with-attribute (string start end table mask)
  ;;(declare (type (simple-array (mod 256) char-code-max) table))
  (declare (simple-string string))
  (declare (fixnum start end))
  "%SP-Find-Character-With-Attribute  String, Start, End, Table, Mask
  The codes of the characters of String from Start to End are used as indices
  into the Table, which is a U-Vector of 8-bit bytes. When the number picked
  up from the table bitwise ANDed with Mask is non-zero, the current
  index into the String is returned. The corresponds to SCANC on the Vax."
  (do ((index start (1+ index)))
      ((= index end) nil)
    (declare (fixnum index))
    (if (/= (logand (aref table (min 255 (char-code (schar string index)))) mask) 0)
	(return index))))

(defun hi::%sp-reverse-find-character-with-attribute (string start end table
							  mask)
  ;;(declare (type (simple-array (mod 256) char-code-max) table))
  (declare (simple-string string))
  (declare (fixnum start end))
  "Like %SP-Find-Character-With-Attribute, only sdrawkcaB."
  (do ((index (1- end) (1- index)))
      ((< index start) nil)
    (declare (fixnum index))
    (if (/= (logand (aref table (min 255 (char-code (aref string index)))) mask) 0)
	(return index))))

(defun hi::%sp-find-character (string start end character)
  "%SP-Find-Character  String, Start, End, Character
  Searches String for the Character from Start to End.  If the character is
  found, the corresponding index into String is returned, otherwise NIL is
  returned."
  (declare (simple-string string)
           (fixnum start end)
           (optimize (speed 3) (safety 0)))
  (do* ((i start (1+ i)))
       ((= i end))
    (declare (fixnum i))
    (when (eq character (schar string i))
      (return i))))

(defun delq (item list)
  (delete item list :test #'eq))

(defun memq (item list)
  (member item list :test #'eq))

(defun assq (item alist)
  (assoc item alist :test #'eq))

;;;; complete-file

#-CMU
(progn
  (defun complete-file (pathname &key (defaults *default-pathname-defaults*)
                                      ignore-types)
    (let ((files (complete-file-directory pathname defaults)))
      (cond ((null files)
             (values nil nil))
            ((null (cdr files))
             (values (car files) 
                     t))
            (t
             (let ((good-files
                    (delete-if #'(lambda (pathname)
                                   (and (simple-string-p
                                         (pathname-type pathname))
                                        (member (pathname-type pathname)
                                                ignore-types
                                                :test #'string=)))
                               files)))
               (cond ((null good-files))
                     ((null (cdr good-files))
                      (return-from complete-file
                        (values (car good-files)
                                t)))
                     (t
                      (setf files good-files)))
               (let ((common (file-namestring (car files))))
                 (dolist (file (cdr files))
                   (let ((name (file-namestring file)))
                     (dotimes (i (min (length common) (length name))
			       (when (< (length name) (length common))
				 (setf common name)))
                       (unless (char= (schar common i) (schar name i))
                         (setf common (subseq common 0 i))
                         (return)))))
                 (values (merge-pathnames common pathname)
                         nil)))))))

;;; COMPLETE-FILE-DIRECTORY-ARG -- Internal.
;;;
  (defun complete-file-directory (pathname defaults)
    (let* ((pathname (merge-pathnames pathname (directory-namestring defaults)))
           (type (pathname-type pathname)))
      (setf pathname
            (make-pathname :defaults (truename (make-pathname :defaults pathname :name nil :type nil))
                           :name (pathname-name pathname)
                           :type type))
      (delete-if-not (lambda (candidate)
                       (search (namestring pathname) (namestring candidate)))
                     (append
                      #+CLISP 
                      (directory
                       (make-pathname :defaults pathname
                                      :name :wild
                                      :type nil)) ;gosh!
                      #+CLISP 
                      (directory
                       (make-pathname :defaults pathname
                                      :directory (append (pathname-directory pathname) (list "*")) ;gosh gosh!
                                      :name nil
                                      :type nil))))))

;;; Ambiguous-Files  --  Public
;;;
  (defun ambiguous-files (pathname
                          &optional (defaults *default-pathname-defaults*))
    "Return a list of all files which are possible completions of Pathname.
   We look in the directory specified by Defaults as well as looking down
   the search list."
    (complete-file-directory pathname defaults)) )


;;;; CLISP fixage 

#+CLISP
(in-package :xlib)

#+CLISP
'(progn
  (defvar *lookahead* nil)

  (setf *buffer-read-polling-time* .01)

  (defun buffer-input-wait-default (display timeout)
    (declare (type display display)
             (type (or null number) timeout))
    (declare (values timeout))

    (let ((stream (display-input-stream display)))
      (declare (type (or null stream) stream))
      (cond ((null stream))
            ((setf *lookahead* (or *lookahead* (ext:read-byte-no-hang stream))) nil)
            ((eql timeout 0) :timeout)
            ((not (null timeout))
             (multiple-value-bind (npoll fraction)
                 (truncate timeout *buffer-read-polling-time*)
               (dotimes (i npoll)       ; Sleep for a time, then listen again
                 (sleep *buffer-read-polling-time*)
                 (when (setf *lookahead* (or *lookahead* (ext:read-byte-no-hang stream)))
                   (return-from buffer-input-wait-default nil)))
               (when (plusp fraction)
                 (sleep fraction)       ; Sleep a fraction of a second
                 (when (setf *lookahead* (or *lookahead* (ext:read-byte-no-hang stream))) ; and listen one last time
                   (return-from buffer-input-wait-default nil)))
               :timeout)))))

  (defun buffer-read-default (display vector start end timeout)
    (declare (type display display)
             (type buffer-bytes vector)
             (type array-index start end)
             (type (or null fixnum) timeout))
    ;; #.(declare-buffun)
    (let ((stream (display-input-stream display)))
      (cond ((and (eql timeout 0)
                  (not (setf *lookahead* (or *lookahead* (ext:read-byte-no-hang stream)))) )
             :timeout)
            (t
             (if *lookahead*
                 (progn
                   (setf (aref vector start) *lookahead*)
                   (setf *lookahead* nil)
                   (system::read-n-bytes stream vector (+ start 1) (- end start 1)))
                 (system::read-n-bytes stream vector start (- end start)))
             nil)) ) ) )
