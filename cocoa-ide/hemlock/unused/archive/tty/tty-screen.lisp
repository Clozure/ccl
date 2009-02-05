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
;;; Written by Bill Chiles, except for the code that implements random typeout,
;;; which was done by Blaine Burks and Bill Chiles.  The code for splitting
;;; windows was rewritten by Blaine Burks to allow more than a 50/50 split.
;;;
;;; Terminal device screen management functions.
;;;

(in-package :hemlock-internals)



;;;; Terminal screen initialization

(declaim (special *parse-starting-mark*))

(defun init-tty-screen-manager (tty-name)
  (setf *line-wrap-char* #\!)
  (setf *window-list* ())
  (let* ((device (make-tty-device tty-name))
	 (width (tty-device-columns device))
	 (height (tty-device-lines device))
	 (echo-height (value hemlock::echo-area-height))
	 (main-lines (- height echo-height 1)) ;-1 for echo modeline.
	 (main-text-lines (1- main-lines)) ;also main-modeline-pos.
	 (last-text-line (1- main-text-lines)))
    (setf (device-bottom-window-base device) last-text-line)
    ;;
    ;; Make echo area.
    (let* ((echo-hunk (make-tty-hunk :position (1- height) :height echo-height
				     :text-position (- height 2)
				     :text-height echo-height :device device))
	   (echo (internal-make-window :hunk echo-hunk)))
      (setf *echo-area-window* echo)
      (setf (device-hunk-window echo-hunk) echo)
      (setup-window-image *parse-starting-mark* echo echo-height width)
      (setup-modeline-image *echo-area-buffer* echo)
      (setf (device-hunk-previous echo-hunk) echo-hunk
	    (device-hunk-next echo-hunk) echo-hunk)
      (prepare-window-for-redisplay echo))
    ;;
    ;; Make the main window.
    (let* ((main-hunk (make-tty-hunk :position main-text-lines
				     :height main-lines
				     :text-position last-text-line
				     :text-height main-text-lines
				     :device device))
	   (main (internal-make-window :hunk main-hunk)))
      (setf (device-hunk-window main-hunk) main)
      (setf *current-window* main)
      (setup-window-image (buffer-point *current-buffer*)
			  main main-text-lines width)
      (setup-modeline-image *current-buffer* main)
      (prepare-window-for-redisplay main)
      (setf (device-hunk-previous main-hunk) main-hunk
	    (device-hunk-next main-hunk) main-hunk)
      (setf (device-hunks device) main-hunk))
    (defhvar "Paren Pause Period"
      "This is how long commands that deal with \"brackets\" shows the cursor at
      the matching \"bracket\" for this number of seconds."
      :value 0.5
      :mode "Lisp")))



;;;; Building devices from termcaps.

;;; MAKE-TTY-DEVICE returns a device built from a termcap.  Some function
;;; slots are set to the appropriate function even though the capability
;;; might not exist; in this case, we simply set the control string value
;;; to the empty string.  Some function slots are set differently depending
;;; on available capability.
;;;
(defun make-tty-device (name)
  (let ((termcap (get-termcap name))
	(device (%make-tty-device :name name)))
    (when (termcap :overstrikes termcap)
      (error "Terminal sufficiently irritating -- not currently supported."))
    ;;
    ;; Similar device slots.
    (setf (device-init device) #'init-tty-device)
    (setf (device-exit device) #'exit-tty-device)
    (setf (device-smart-redisplay device)
	  (if (and (termcap :open-line termcap) (termcap :delete-line termcap))
	      #'tty-smart-window-redisplay
	      #'tty-semi-dumb-window-redisplay))
    (setf (device-dumb-redisplay device) #'tty-dumb-window-redisplay)
    (setf (device-clear device) #'clear-device)
    (setf (device-put-cursor device) #'tty-put-cursor)
    (setf (device-show-mark device) #'tty-show-mark)
    (setf (device-next-window device) #'tty-next-window)
    (setf (device-previous-window device) #'tty-previous-window)
    (setf (device-make-window device) #'tty-make-window)
    (setf (device-delete-window device) #'tty-delete-window)
    (setf (device-random-typeout-setup device) #'tty-random-typeout-setup)
    (setf (device-random-typeout-cleanup device) #'tty-random-typeout-cleanup)
    (setf (device-random-typeout-full-more device) #'do-tty-full-more)
    (setf (device-random-typeout-line-more device)
	  #'update-tty-line-buffered-stream)
    (setf (device-force-output device) #'tty-force-output)
    (setf (device-finish-output device) #'tty-finish-output)
    (setf (device-beep device) #'tty-beep)
    ;;
    ;; A few useful values.
    (setf (tty-device-dumbp device)
	  (not (and (termcap :open-line termcap)
		    (termcap :delete-line termcap))))
    ;;
    ;; Get size and speed.
    (multiple-value-bind  (lines cols speed)
			  (get-terminal-attributes)
      (setf (tty-device-lines device) (or lines (termcap :lines termcap)))
      (let ((cols (or cols (termcap :columns termcap))))
	(setf (tty-device-columns device)
	      (if (termcap :auto-margins-p termcap)
		  (1- cols) cols)))
      (setf (tty-device-speed device) speed))
    ;;
    ;; Some function slots.
    (setf (tty-device-display-string device)
	  (if (termcap :underlines termcap)
	      #'display-string-checking-underlines
	      #'display-string))
    (setf (tty-device-standout-init device) #'standout-init)
    (setf (tty-device-standout-end device) #'standout-end)
    (setf (tty-device-open-line device)
	  (if (termcap :open-line termcap)
	      #'open-tty-line
	      ;; look for scrolling region stuff
	      ))
    (setf (tty-device-delete-line device)
	  (if (termcap :delete-line termcap)
	      #'delete-tty-line
	      ;; look for reverse scrolling stuff
	      ))
    (setf (tty-device-clear-to-eol device)
	  (if (termcap :clear-to-eol termcap)
	      #'clear-to-eol
	      #'space-to-eol))
    (setf (tty-device-clear-lines device) #'clear-lines)
    (setf (tty-device-clear-to-eow device) #'clear-to-eow)
    ;;
    ;; Insert and delete modes.
    (let ((init-insert-mode (termcap :init-insert-mode termcap))
	  (init-insert-char (termcap :init-insert-char termcap))
	  (end-insert-char (termcap :end-insert-char termcap)))
      (when (and init-insert-mode (string/= init-insert-mode ""))
	(setf (tty-device-insert-string device) #'tty-insert-string)
	(setf (tty-device-insert-init-string device) init-insert-mode)
	(setf (tty-device-insert-end-string device)
	      (termcap :end-insert-mode termcap)))
      (when init-insert-char
	(setf (tty-device-insert-string device) #'tty-insert-string)
	(setf (tty-device-insert-char-init-string device) init-insert-char))
      (when (and end-insert-char (string/= end-insert-char ""))
	(setf (tty-device-insert-char-end-string device) end-insert-char)))
    (let ((delete-char (termcap :delete-char termcap)))
      (when delete-char
	(setf (tty-device-delete-char device) #'delete-char)
	(setf (tty-device-delete-char-string device) delete-char)
	(setf (tty-device-delete-init-string device)
	      (termcap :init-delete-mode termcap))
	(setf (tty-device-delete-end-string device)
	      (termcap :end-delete-mode termcap))))
    ;;
    ;; Some string slots.
    (setf (tty-device-standout-init-string device)
	  (or (termcap :init-standout-mode termcap) ""))
    (setf (tty-device-standout-end-string device)
	  (or (termcap :end-standout-mode termcap) ""))
    (setf (tty-device-clear-to-eol-string device)
	  (termcap :clear-to-eol termcap))
    (let ((clear-string (termcap :clear-display termcap)))
      (unless clear-string
	(error "Terminal not sufficiently powerful enough to run Hemlock."))
      (setf (tty-device-clear-string device) clear-string))
    (setf (tty-device-open-line-string device)
	  (termcap :open-line termcap))
    (setf (tty-device-delete-line-string device)
	  (termcap :delete-line termcap))
    (let* ((init-string (termcap :init-string termcap))
	   (init-file (termcap :init-file termcap))
	   (init-file-string (if init-file (get-init-file-string init-file)))
	   (init-cm-string (termcap :init-cursor-motion termcap)))
      (setf (tty-device-init-string device)
	    (concatenate 'simple-string (or init-string "")
			 (or init-file-string "") (or init-cm-string ""))))
    (setf (tty-device-cm-end-string device)
	  (or (termcap :end-cursor-motion termcap) ""))
    ;;
    ;; Cursor motion slots.
    (let ((cursor-motion (termcap :cursor-motion termcap)))
      (unless cursor-motion
	(error "Terminal not sufficiently powerful enough to run Hemlock."))
      (let ((x-add-char (getf cursor-motion :x-add-char))
	    (y-add-char (getf cursor-motion :y-add-char))
	    (x-condx-char (getf cursor-motion :x-condx-char))
	    (y-condx-char (getf cursor-motion :y-condx-char)))
	(when x-add-char
	  (setf (tty-device-cm-x-add-char device) (char-code x-add-char)))
	(when y-add-char
	  (setf (tty-device-cm-y-add-char device) (char-code y-add-char)))
	(when x-condx-char
	  (setf (tty-device-cm-x-condx-char device) (char-code x-condx-char))
	  (setf (tty-device-cm-x-condx-add-char device)
		(char-code (getf cursor-motion :x-condx-add-char))))
	(when y-condx-char
	  (setf (tty-device-cm-y-condx-char device) (char-code y-condx-char))
	  (setf (tty-device-cm-y-condx-add-char device)
		(char-code (getf cursor-motion :y-condx-add-char)))))
      (setf (tty-device-cm-string1 device) (getf cursor-motion :string1))
      (setf (tty-device-cm-string2 device) (getf cursor-motion :string2))
      (setf (tty-device-cm-string3 device) (getf cursor-motion :string3))
      (setf (tty-device-cm-one-origin device) (getf cursor-motion :one-origin))
      (setf (tty-device-cm-reversep device) (getf cursor-motion :reversep))
      (setf (tty-device-cm-x-pad device) (getf cursor-motion :x-pad))
      (setf (tty-device-cm-y-pad device) (getf cursor-motion :y-pad)))
    ;;
    ;; Screen image initialization.
    (let* ((lines (tty-device-lines device))
	   (columns (tty-device-columns device))
	   (screen-image (make-array lines)))
      (dotimes (i lines)
	(setf (svref screen-image i) (make-si-line columns)))
      (setf (tty-device-screen-image device) screen-image))
    device))


      
;;;; Making a window

(defun tty-make-window (device start modelinep window font-family
			       ask-user x y width height proportion)
  (declare (ignore window font-family ask-user x y width height))
  (let* ((old-window (current-window))
	 (victim (window-hunk old-window))
	 (text-height (tty-hunk-text-height victim))
	 (availability (if modelinep (1- text-height) text-height)))
    (when (> availability 1)
      (let* ((new-lines (truncate (* availability proportion)))
	     (old-lines (- availability new-lines))
	     (pos (device-hunk-position victim))
	     (new-height (if modelinep (1+ new-lines) new-lines))
	     (new-text-pos (if modelinep (1- pos) pos))
	     (new-hunk (make-tty-hunk :position pos
				      :height new-height
				      :text-position new-text-pos
				      :text-height new-lines
				      :device device))
	     (new-window (internal-make-window :hunk new-hunk)))
	(declare (fixnum new-lines old-lines pos new-height new-text-pos))
	(setf (device-hunk-window new-hunk) new-window)
	(let* ((old-text-pos-diff (- pos (tty-hunk-text-position victim)))
	       (old-win-new-pos (- pos new-height)))
	  (declare (fixnum old-text-pos-diff old-win-new-pos))
	  (setf (device-hunk-height victim)
		(- (device-hunk-height victim) new-height))
	  (setf (tty-hunk-text-height victim) old-lines)
	  (setf (device-hunk-position victim) old-win-new-pos)
	  (setf (tty-hunk-text-position victim)
		(- old-win-new-pos old-text-pos-diff)))
	(setup-window-image start new-window new-lines
			    (window-width old-window))
	(prepare-window-for-redisplay new-window)
	(when modelinep
	  (setup-modeline-image (line-buffer (mark-line start)) new-window))
	(change-window-image-height old-window old-lines)
	(shiftf (device-hunk-previous new-hunk)
		(device-hunk-previous (device-hunk-next victim))
		new-hunk)
	(shiftf (device-hunk-next new-hunk) (device-hunk-next victim) new-hunk)
	(setf *currently-selected-hunk* nil)
	(setf *screen-image-trashed* t)
	new-window))))



;;;; Deleting a window

(defun tty-delete-window (window)
  (let* ((hunk (window-hunk window))
	 (prev (device-hunk-previous hunk))
	 (next (device-hunk-next hunk))
	 (device (device-hunk-device hunk)))
    (setf (device-hunk-next prev) next)
    (setf (device-hunk-previous next) prev)
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer) (delq window (buffer-windows buffer))))
    (let ((new-lines (device-hunk-height hunk)))
      (declare (fixnum new-lines))
      (cond ((eq hunk (device-hunks (device-hunk-device next)))
	     (incf (device-hunk-height next) new-lines)
	     (incf (tty-hunk-text-height next) new-lines)
	     (let ((w (device-hunk-window next)))
	       (change-window-image-height w (+ new-lines (window-height w)))))
	    (t
	     (incf (device-hunk-height prev) new-lines)
	     (incf (device-hunk-position prev) new-lines)
	     (incf (tty-hunk-text-height prev) new-lines)
	     (incf (tty-hunk-text-position prev) new-lines)
	     (let ((w (device-hunk-window prev)))
	       (change-window-image-height w (+ new-lines (window-height w)))))))
    (when (eq hunk (device-hunks device))
      (setf (device-hunks device) next)))
  (setf *currently-selected-hunk* nil)
  (setf *screen-image-trashed* t))



;;;; Next and Previous window operations.

(defun tty-next-window (window)
  (device-hunk-window (device-hunk-next (window-hunk window))))

(defun tty-previous-window (window)
  (device-hunk-window (device-hunk-previous (window-hunk window))))



;;;; Random typeout support

(defun tty-random-typeout-setup (device stream height)
  (declare (fixnum height))
  (let* ((*more-prompt-action* :empty)
	 (height (min (1- (device-bottom-window-base device)) height))
	 (old-hwindow (random-typeout-stream-window stream))
	 (new-hwindow (if old-hwindow
			  (change-tty-random-typeout-window old-hwindow height)
			  (setf (random-typeout-stream-window stream)
				(make-tty-random-typeout-window
				 device
				 (buffer-start-mark
				  (line-buffer
				   (mark-line
				    (random-typeout-stream-mark stream))))
				 height)))))
    (funcall (tty-device-clear-to-eow device) (window-hunk new-hwindow) 0 0)))

(defun change-tty-random-typeout-window (window height)
  (update-modeline-field (window-buffer window) window :more-prompt)
  (let* ((height-1 (1- height))
	 (hunk (window-hunk window)))
    (setf (device-hunk-position hunk) height-1
	  (device-hunk-height hunk) height
	  (tty-hunk-text-position hunk) (1- height-1)
	  (tty-hunk-text-height hunk) height-1)
    (change-window-image-height window height-1)
    window))

(defun make-tty-random-typeout-window (device mark height)
  (let* ((height-1 (1- height))
	 (hunk (make-tty-hunk :position height-1
			      :height height
			      :text-position (1- height-1)
			      :text-height height-1
			      :device device))
	 (window (internal-make-window :hunk hunk)))
    (setf (device-hunk-window hunk) window)
    (setf (device-hunk-device hunk) device)
    (setup-window-image mark window height-1 (tty-device-columns device))
    (setf *window-list* (delete window *window-list*))
    (prepare-window-for-redisplay window)
    (setup-modeline-image (line-buffer (mark-line mark)) window)
    (update-modeline-field (window-buffer window) window :more-prompt)
    window))

(defun tty-random-typeout-cleanup (stream degree)
  (declare (ignore degree))
  (let* ((window (random-typeout-stream-window stream))
	 (stream-hunk (window-hunk window))
	 (last-line-affected (device-hunk-position stream-hunk))
	 (device (device-hunk-device stream-hunk))
	 (*more-prompt-action* :normal))
    (declare (fixnum last-line-affected))
    (update-modeline-field (window-buffer window) window :more-prompt)
    (funcall (tty-device-clear-to-eow device) stream-hunk 0 0)
    (do* ((hunk (device-hunks device) (device-hunk-next hunk))
	  (window (device-hunk-window hunk) (device-hunk-window hunk))
	  (last (device-hunk-previous hunk)))
	 ((>= (device-hunk-position hunk) last-line-affected)
	  (if (= (device-hunk-position hunk) last-line-affected)
	      (redisplay-window-all window)
	      (tty-redisplay-n-lines window
				     (- (+ last-line-affected
					   (tty-hunk-text-height hunk))
					(tty-hunk-text-position hunk)))))
      (redisplay-window-all window)
      (when (eq hunk last) (return)))))
