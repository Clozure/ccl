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
;;; Written by Bill Chiles.
;;;
;;; This is the device independent redisplay entry points for Hemlock.
;;;

(in-package :hemlock-internals)


;;;; Main redisplay entry points.

(defvar *things-to-do-once* ()
  "This is a list of lists of functions and args to be applied to.  The 
  functions are called with args supplied at the top of the command loop.")

(defvar *screen-image-trashed* ()
  "This variable is set to true if the screen has been trashed by some screen
   manager operation, and thus should be totally refreshed.  This is currently
   only used by tty redisplay.")

;;; True if we are in redisplay, and thus don't want to enter it recursively.
;;;
(defvar *in-redisplay* nil)

(declaim (special *window-list*))

(eval-when (:compile-toplevel :execute)

;;; REDISPLAY-LOOP -- Internal.
;;;
;;; This executes internal redisplay routines on all windows interleaved with
;;; checking for input, and if any input shows up we punt returning
;;; :editor-input.  Special-fun is for windows that the redisplay interface
;;; wants to recenter to keep the window's buffer's point visible.  General-fun
;;; is for other windows.
;;;
;;; Whenever we invoke one of the internal routines, we keep track of the
;;; non-nil return values, so we can return t when we are done.  Returning t
;;; means redisplay should run again to make sure it converged.  To err on the
;;; safe side, if any window had any changed lines, then let's go through
;;; redisplay again; that is, return t.
;;;
;;; After checking each window, we put the cursor in the appropriate place and
;;; force output.  When we try to position the cursor, it may no longer lie
;;; within the window due to buffer modifications during redisplay.  If it is
;;; out of the window, return t to indicate we need to finish redisplaying.
;;;
;;; Then we check for the after-redisplay method.  Routines such as REDISPLAY
;;; and REDISPLAY-ALL want to invoke the after method to make sure we handle
;;; any events generated from redisplaying.  There wouldn't be a problem with
;;; handling these events if we were going in and out of Hemlock's event
;;; handling, but some user may loop over one of these interface functions for
;;; a long time without going through Hemlock's input loop; when that happens,
;;; each call to redisplay may not result in a complete redisplay of the
;;; device.  Routines such as INTERNAL-REDISPLAY don't want to worry about this
;;; since Hemlock calls them while going in and out of the input/event-handling
;;; loop.
;;;
;;; Around all of this, we establish the 'redisplay-catcher tag.  Some device
;;; redisplay methods throw to this to abort redisplay in addition to this
;;; code.
;;;
(defmacro redisplay-loop (general-fun special-fun &optional (afterp t))
  (let* ((device (gensym)) (point (gensym)) (hunk (gensym)) (n-res (gensym))
	 (win-var (gensym))
	 (general-form (if (symbolp general-fun)
			   `(,general-fun ,win-var)
			   `(funcall ,general-fun ,win-var)))
	 (special-form (if (symbolp special-fun)
			   `(,special-fun ,win-var)
			   `(funcall ,special-fun ,win-var))))
    `(let ((,n-res nil)
	   (*in-redisplay* t))
       (catch 'redisplay-catcher
	 (when (listen-editor-input *real-editor-input*)
	   (throw 'redisplay-catcher :editor-input))
	 (let ((,win-var *current-window*))
	   (when ,special-form (setf ,n-res t)))
	 (dolist (,win-var *window-list*)
	   (unless (eq ,win-var *current-window*)
	     (when (listen-editor-input *real-editor-input*)
	       (throw 'redisplay-catcher :editor-input))
	     (when (if (window-display-recentering ,win-var)
		       ,special-form
		       ,general-form)
	        (setf ,n-res t))))
	 (let* ((,hunk (window-hunk *current-window*))
		(,device (device-hunk-device ,hunk))
		(,point (window-point *current-window*)))
	   (move-mark ,point (buffer-point (window-buffer *current-window*)))
	   (multiple-value-bind (x y)
				(mark-to-cursorpos ,point *current-window*)
	     (if x
		 (funcall (device-put-cursor ,device) ,hunk x y)
		 (setf ,n-res t)))
	   (when (device-force-output ,device)
	     (funcall (device-force-output ,device)))
	   ,@(if afterp
		 `((when (device-after-redisplay ,device)
		     (funcall (device-after-redisplay ,device) ,device)
		     ;; The after method may have queued input that the input
		     ;; loop won't see until the next input arrives, so check
		     ;; here to return the correct value as per the redisplay
		     ;; contract.
		     (when (listen-editor-input *real-editor-input*)
		       (setf ,n-res :editor-input)))))
	   ,n-res)))))

) ;eval-when


;;; REDISPLAY -- Public.
;;;
;;; This function updates the display of all windows which need it.  It assumes
;;; it's internal representation of the screen is accurate and attempts to do
;;; the minimal amount of output to bring the screen into correspondence.
;;; *screen-image-trashed* is only used by terminal redisplay.
;;;
(defun redisplay ()
  "The main entry into redisplay; updates any windows that seem to need it."
  (when *things-to-do-once*
    (dolist (thing *things-to-do-once*) (apply (car thing) (cdr thing)))
    (setf *things-to-do-once* nil))
  (cond (*in-redisplay* t)
	(*screen-image-trashed*
	 (when (eq (redisplay-all) t)
	   (setf *screen-image-trashed* nil)
	   t))
	(t
	 (redisplay-loop redisplay-window redisplay-window-recentering))))


;;; REDISPLAY-ALL -- Public.
;;;
;;; Update the screen making no assumptions about its correctness.  This is
;;; useful if the screen gets trashed, or redisplay gets lost.  Since windows
;;; may be on different devices, we have to go through the list clearing all
;;; possible devices.  Always returns T or :EDITOR-INPUT, never NIL.
;;;
(defun redisplay-all ()
  "An entry into redisplay; causes all windows to be fully refreshed."
  (let ((cleared-devices nil))
    (dolist (w *window-list*)
      (let* ((hunk (window-hunk w))
	     (device (device-hunk-device hunk)))
	(unless (member device cleared-devices :test #'eq)
	  (when (device-clear device)
	    (funcall (device-clear device) device))
	  ;;
	  ;; It's cleared whether we did clear it or there was no method.
	  (push device cleared-devices)))))
  (redisplay-loop
   redisplay-window-all
   #'(lambda (window)
       (setf (window-tick window) (tick))
       (update-window-image window)
       (maybe-recenter-window window)
       (funcall (device-dumb-redisplay
		 (device-hunk-device (window-hunk window)))
		window)
       t)))



;;;; Internal redisplay entry points.

(defun internal-redisplay ()
  "The main internal entry into redisplay.  This is just like REDISPLAY, but it
   doesn't call the device's after-redisplay method."
  (when *things-to-do-once*
    (dolist (thing *things-to-do-once*) (apply (car thing) (cdr thing)))
    (setf *things-to-do-once* nil))
  (cond (*in-redisplay* t)
	(*screen-image-trashed*
	 (when (eq (redisplay-all) t)
	   (setf *screen-image-trashed* nil)
	   t))
	(t
	 (redisplay-loop redisplay-window redisplay-window-recentering))))

;;; REDISPLAY-WINDOWS-FROM-MARK -- Internal Interface.
;;;
;;; hemlock-output-stream methods call this to update the screen.  It only
;;; redisplays windows which are displaying the buffer concerned and doesn't
;;; deal with making the cursor track the point.  *screen-image-trashed* is
;;; only used by terminal redisplay.  This must call the device after-redisplay
;;; method since stream output may occur without ever returning to the
;;; Hemlock input/event-handling loop.
;;;
(defun redisplay-windows-from-mark (mark)
  (when *things-to-do-once*
    (dolist (thing *things-to-do-once*) (apply (car thing) (cdr thing)))
    (setf *things-to-do-once* nil))
  (cond ((or *in-redisplay* (not *in-the-editor*)) t)
	((listen-editor-input *real-editor-input*) :editor-input)
	(*screen-image-trashed*
	 (when (eq (redisplay-all) t)
	   (setf *screen-image-trashed* nil)
	   t))
	(t
	 (catch 'redisplay-catcher
	   (let ((buffer (line-buffer (mark-line mark))))
	     (when buffer
	       (flet ((frob (win)
			(let* ((device (device-hunk-device (window-hunk win)))
			       (force (device-force-output device))
			       (after (device-after-redisplay device)))
			  (when force (funcall force))
			  (when after (funcall after device)))))
		 (let ((windows (buffer-windows buffer)))
		   (when (member *current-window* windows :test #'eq)
		     (redisplay-window-recentering *current-window*)
		     (frob *current-window*))
		   (dolist (window windows)
		     (unless (eq window *current-window*)
		       (redisplay-window window)
		       (frob window)))))))))))

;;; REDISPLAY-WINDOW -- Internal.
;;;
;;; Return t if there are any changed lines, nil otherwise.
;;;
(defun redisplay-window (window)
  "Maybe updates the window's image and calls the device's smart redisplay
   method.  NOTE: the smart redisplay method may throw to
   'hi::redisplay-catcher to abort redisplay."
  (maybe-update-window-image window)
  (prog1
      (not (eq (window-first-changed window) *the-sentinel*))
    (funcall (device-smart-redisplay (device-hunk-device (window-hunk window)))
	     window)))

(defun redisplay-window-all (window)
  "Updates the window's image and calls the device's dumb redisplay method."
  (setf (window-tick window) (tick))
  (update-window-image window)
  (funcall (device-dumb-redisplay (device-hunk-device (window-hunk window)))
	   window)
  t)

(defun random-typeout-redisplay (window)
  (catch 'redisplay-catcher
    (maybe-update-window-image window)
    (let* ((device (device-hunk-device (window-hunk window)))
	   (force (device-force-output device)))
      (funcall (device-smart-redisplay device) window)
      (when force (funcall force)))))


;;;; Support for redisplay entry points.

;;; REDISPLAY-WINDOW-RECENTERING -- Internal.
;;;
;;; This tries to be clever about updating the window image unnecessarily,
;;; recenters the window if the window's buffer's point moved off the window,
;;; and does a smart redisplay.  We call the redisplay method even if we didn't
;;; update the image or recenter because someone else may have modified the
;;; window's image and already have updated it; if nothing happened, then the
;;; smart method shouldn't do anything anyway.  NOTE: the smart redisplay
;;; method may throw to 'hi::redisplay-catcher to abort redisplay.
;;;
;;; This return t if there are any changed lines, nil otherwise.
;;; 
(defun redisplay-window-recentering (window)
  (setup-for-recentering-redisplay window)
  (invoke-hook hemlock::redisplay-hook window)
  (setup-for-recentering-redisplay window)
  (prog1
      (not (eq (window-first-changed window) *the-sentinel*))
    (funcall (device-smart-redisplay (device-hunk-device (window-hunk window)))
	     window)))

(defun setup-for-recentering-redisplay (window)
  (let* ((display-start (window-display-start window))
	 (old-start (window-old-start window)))
    ;;
    ;; If the start is in the middle of a line and it wasn't before,
    ;; then move the start there.
    (when (and (same-line-p display-start old-start)
	       (not (start-line-p display-start))
	       (start-line-p old-start))
      (line-start display-start))
    (maybe-update-window-image window)
    (maybe-recenter-window window)))


;;; MAYBE-UPDATE-WINDOW-IMAGE only updates if the text has changed or the
;;; display start.
;;; 
(defun maybe-update-window-image (window)
  (when (or (> (buffer-modified-tick (window-buffer window))
	       (window-tick window))
	    (mark/= (window-display-start window)
		    (window-old-start window)))
    (setf (window-tick window) (tick))
    (update-window-image window)
    t))
