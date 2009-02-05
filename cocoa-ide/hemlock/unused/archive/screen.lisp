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
;;;    Written by Bill Chiles.
;;;
;;; Device independent screen management functions.
;;;

(in-package :hemlock-internals)


;;;; Screen management initialization.

(declaim (special *echo-area-buffer*))

;;; %INIT-SCREEN-MANAGER creates the initial windows and sets up the data
;;; structures used by the screen manager.  The "Main" and "Echo Area" buffer
;;; modelines are set here in case the user modified these Hemlock variables in
;;; his init file.  Since these buffers don't have windows yet, these sets
;;; won't cause any updates to occur.  This is called from %INIT-REDISPLAY.
;;;
(defun %init-screen-manager (display)
  (setf (buffer-modeline-fields *current-buffer*)
	(value hemlock::default-modeline-fields))
  (setf (buffer-modeline-fields *echo-area-buffer*)
	(value hemlock::default-status-line-fields))
  (if (windowed-monitor-p)
      (init-bitmap-screen-manager display)
      (init-tty-screen-manager (get-terminal-name))))



;;;; Window operations.

(defun make-window (start &key (modelinep t) (device nil) window
			  (proportion .5)			  
			  (font-family *default-font-family*)
			  (ask-user nil) x y
			  (width (value hemlock::default-window-width))
			  (height (value hemlock::default-window-height)))
  "Make a window that displays text starting at the mark start.  The default
   action is to make the new window a proportion of the current window's height
   to make room for the new window.

   Proportion determines what proportion of the current window's height
   the new window will use.  The current window retains whatever space left
   after accommodating the new one.  The default is to split the current window
   in half.

   Modelinep specifies whether the window should display buffer modelines.

   Device is the Hemlock device to make the window on.  If it is nil, then
   the window is made on the same device as CURRENT-WINDOW.

   Window is an X window to be used with the Hemlock window.  The supplied
   window becomes the parent window for a new group of windows that behave
   in a stack orientation as windows do on the terminal.

   Font-Family is the font-family used for displaying text in the window.

   If Ask-User is non-nil, Hemlock prompts the user for missing X, Y, Width,
   and Height arguments to make a new group of windows that behave in a stack
   orientation as windows do on the terminal.  This occurs by invoking
   hi::*create-window-hook*.  X and Y are supplied as pixels, but Width and
   Height are supplied in characters."

  (let* ((device (or device (device-hunk-device (window-hunk (current-window)))))
	 (window (funcall (device-make-window device)
			  device start modelinep window font-family
			  ask-user x y width height proportion)))
    (unless window (editor-error "Could not make a window."))
    (invoke-hook hemlock::make-window-hook window)
    window))

(defun delete-window (window)
  "Make Window go away, removing it from the screen.  This uses
   hi::*delete-window-hook* to get rid of parent windows on a bitmap device
   when you delete the last Hemlock window in a group."
  (when (<= (length *window-list*) 2)
    (error "Cannot kill the only window."))
  (invoke-hook hemlock::delete-window-hook window)
  (setq *window-list* (delq window *window-list*))
  (funcall (device-delete-window (device-hunk-device (window-hunk window)))
	   window)
  ;;
  ;; Since the programmer's interface fails to allow users to determine if
  ;; they're commands delete the current window, this primitive needs to
  ;; make sure Hemlock doesn't get screwed.  This inadequacy comes from the
  ;; bitmap window groups and the vague descriptions of PREVIOUS-WINDOW and
  ;; NEXT-WINDOW.
  (when (eq window *current-window*)
    (let ((window (find-if-not #'(lambda (w) (eq w *echo-area-window*))
			       *window-list*)))
      (setf (current-buffer) (window-buffer window)
	    (current-window) window))))

(defun next-window (window)
  "Return the next window after Window, wrapping around if Window is the
  bottom window."
  (check-type window window)
  (funcall (device-next-window (device-hunk-device (window-hunk window)))
	   window))

(defun previous-window (window)
  "Return the previous window after Window, wrapping around if Window is the
  top window."
  (check-type window window)
  (funcall (device-previous-window (device-hunk-device (window-hunk window)))
	   window))



;;;; Random typeout support.

;;; PREPARE-FOR-RANDOM-TYPEOUT  --  Internal
;;;
;;; The WITH-POP-UP-DISPLAY macro calls this just before displaying output
;;; for the user.  This goes to some effor to compute the height of the window
;;; in text lines if it is not supplied.  Whether it is supplied or not, we
;;; add one to the height for the modeline, and we subtract one line if the
;;; last line is empty.  Just before using the height, make sure it is at
;;; least two -- one for the modeline and one for text, so window making
;;; primitives don't puke.
;;;
(defun prepare-for-random-typeout (stream height)
  (let* ((buffer (line-buffer (mark-line (random-typeout-stream-mark stream))))
	 (real-height (1+ (or height (rt-count-lines buffer))))
	 (device (device-hunk-device (window-hunk (current-window)))))
    (funcall (device-random-typeout-setup device) device stream
	     (max (if (and (empty-line-p (buffer-end-mark buffer)) (not height))
		      (1- real-height)
		      real-height)
		  2))))

;;; RT-COUNT-LINES computes the correct height for a window.  This includes
;;; taking wrapping line characters into account.  Take the MARK-COLUMN at
;;; the end of each line.  This is how many characters long hemlock thinks
;;; the line is.  When it is displayed, however, end of line characters are
;;; added to the end of each line that wraps.  The second INCF form adds
;;; these to the current line length.  Then INCF the current height by the
;;; CEILING of the width of the random typeout window and the line length
;;; (with added line-end chars).  Use CEILING because there is always at
;;; least one line.  Finally, jump out of the loop if we're at the end of
;;; the buffer.
;;;
(defun rt-count-lines (buffer)
  (with-mark ((mark (buffer-start-mark buffer)))
    (let ((width (window-width (current-window)))
	  (count 0))
	(loop
	  (let* ((column (mark-column (line-end mark)))
		 (temp (ceiling (incf column (floor (1- column) width))
				width)))
	    ;; Lines with no characters yield zero temp.
	    (incf count (if (zerop temp) 1 temp))
	    (unless (line-offset mark 1) (return count)))))))


;;; RANDOM-TYPEOUT-CLEANUP  --  Internal
;;;
;;;    Clean up after random typeout.  This clears the area where the 
;;; random typeout was and redisplays any affected windows.
;;;
(defun random-typeout-cleanup (stream &optional (degree t))
  (let* ((window (random-typeout-stream-window stream))
	 (buffer (window-buffer window))
	 (device (device-hunk-device (window-hunk window)))
	 (*more-prompt-action* :normal))
    (update-modeline-field buffer window :more-prompt)
    (random-typeout-redisplay window)
    (setf (buffer-windows buffer) (delete window (buffer-windows buffer)))
    (funcall (device-random-typeout-cleanup device) stream degree)
    (when (device-force-output device)
      (funcall (device-force-output device)))))

;;; *more-prompt-action* is bound in random typeout streams before
;;; redisplaying.
;;;
(defvar *more-prompt-action* :normal)
(defvar *random-typeout-ml-fields*
  (list (make-modeline-field
	 :name :more-prompt
	 :function #'(lambda (buffer window)
		       (declare (ignore window))
		       (ecase *more-prompt-action*
			 (:more "--More--")
			 (:flush "--Flush--")
			 (:empty "")
			 (:normal
			  (concatenate 'simple-string
				       "Random Typeout Buffer          ["
				       (buffer-name buffer)
				       "]")))))))
