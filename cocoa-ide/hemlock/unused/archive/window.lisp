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
;;;    This file contains implementation independent code which implements
;;; the Hemlock window primitives and most of the code which defines
;;; other aspects of the interface to redisplay.
;;;
;;; Written by Bill Chiles and Rob MacLachlan.
;;;

(in-package :hemlock-internals)

(defconstant unaltered-bits #b000
  "This is the value of the dis-line-flags when a line is neither moved nor
  changed nor new.")
(defconstant changed-bit #b001
  "This bit is set in the dis-line-flags when a line is found to be changed.")
(defconstant moved-bit #b010
  "This bit is set in the dis-line-flags when a line is found to be moved.")
(defconstant new-bit #b100
  "This bit is set in the dis-line-flags when a line is found to be new.")


;;;; CURRENT-WINDOW.

(defvar *current-window* nil "The current window object.")
(defvar *window-list* () "A list of all window objects.")

(declaim (inline current-window))

(defun current-window ()
  "Return the current window.  The current window is specially treated by
  redisplay in several ways, the most important of which is that is does
  recentering, ensuring that the Buffer-Point of the current window's
  Window-Buffer is always displayed.  This may be set with Setf."
  *current-window*)

(defun %set-current-window (new-window)
  (invoke-hook hemlock::set-window-hook new-window)
  (move-mark (window-point *current-window*)
	     (buffer-point (window-buffer *current-window*)))
  (move-mark (buffer-point (window-buffer new-window))
	     (window-point new-window))
  (setq *current-window* new-window))



;;;; Window structure support.

(defun %print-hwindow (obj stream depth)
  (declare (ignore depth))
  (write-string "#<Hemlock Window \"" stream)
  (write-string (buffer-name (window-buffer obj)) stream)
  (write-string "\">" stream))


(defun window-buffer (window)
  "Return the buffer which is displayed in Window."
  (window-%buffer window))

(defun %set-window-buffer (window new-buffer)
  (unless (bufferp new-buffer) (error "~S is not a buffer." new-buffer))
  (unless (windowp window) (error "~S is not a window." window))
  (unless (eq new-buffer (window-buffer window))
    (invoke-hook hemlock::window-buffer-hook window new-buffer)
    ;;
    ;; Move the window's marks to the new start.
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer) (delete window (buffer-windows buffer)))
      (move-mark (buffer-display-start buffer) (window-display-start window))
      (push window (buffer-windows new-buffer))
      (move-mark (window-point window) (buffer-point new-buffer))
      (move-mark (window-display-start window) (buffer-display-start new-buffer))
      (move-mark (window-display-end window) (buffer-display-start new-buffer)))
    ;;
    ;; Delete all the dis-lines, and nil out the line and chars so they get
    ;; gc'ed.
    (let ((first (window-first-line window))
	  (last (window-last-line window))
	  (free (window-spare-lines window)))
      (unless (eq (cdr first) *the-sentinel*)
	(shiftf (cdr last) free (cdr first) *the-sentinel*))
      (dolist (dl free)
	(setf (dis-line-line dl) nil  (dis-line-old-chars dl) nil))
      (setf (window-spare-lines window) free))
    ;;
    ;; Set the last line and first&last changed so we know there's nothing there.
    (setf (window-last-line window) *the-sentinel*
	  (window-first-changed window) *the-sentinel*
	  (window-last-changed window) *the-sentinel*)
    ;;
    ;; Make sure the window gets updated, and set the buffer.
    (setf (window-tick window) -3)
    (setf (window-%buffer window) new-buffer)))



;;; %INIT-REDISPLAY sets up redisplay's internal data structures.  We create
;;; initial windows, setup some hooks to cause modeline recomputation, and call
;;; any device init necessary.  This is called from ED.
;;;
(defun %init-redisplay (display)
  (%init-screen-manager display)
  (add-hook hemlock::buffer-major-mode-hook 'queue-buffer-change)
  (add-hook hemlock::buffer-minor-mode-hook 'queue-buffer-change)
  (add-hook hemlock::buffer-name-hook 'queue-buffer-change)
  (add-hook hemlock::buffer-pathname-hook 'queue-buffer-change)
  (add-hook hemlock::buffer-modified-hook 'queue-buffer-change)
  (add-hook hemlock::window-buffer-hook 'queue-window-change)
  (let ((device (device-hunk-device (window-hunk (current-window)))))
    (funcall (device-init device) device))
  (center-window *current-window* (current-point)))



;;;; Modelines-field structure support.

(defun print-modeline-field (obj stream ignore)
  (declare (ignore ignore))
  (write-string "#<Hemlock Modeline-field " stream)
  (prin1 (modeline-field-%name obj) stream)
  (write-string ">" stream))

(defun print-modeline-field-info (obj stream ignore)
  (declare (ignore ignore))
  (write-string "#<Hemlock Modeline-field-info " stream)
  (prin1 (modeline-field-%name (ml-field-info-field obj)) stream)
  (write-string ">" stream))


(defvar *modeline-field-names* (make-hash-table))

(defun make-modeline-field (&key name width function)
  "Returns a modeline-field object."
  (unless (or (eq width nil) (and (integerp width) (plusp width)))
    (error "Width must be nil or a positive integer."))
  (when (gethash name *modeline-field-names*)
    (with-simple-restart (continue
			  "Use the new definition for this modeline field.")
      (error "Modeline field ~S already exists."
	     (gethash name *modeline-field-names*))))
  (setf (gethash name *modeline-field-names*)
	(%make-modeline-field name function width)))

(defun modeline-field (name)
  "Returns the modeline-field object named name.  If none exists, return nil."
  (gethash name *modeline-field-names*))


(declaim (inline modeline-field-name modeline-field-width
		 modeline-field-function))

(defun modeline-field-name (ml-field)
  "Returns the name of a modeline field object."
  (modeline-field-%name ml-field))

(defun %set-modeline-field-name (ml-field name)
  (check-type ml-field modeline-field)
  (when (gethash name *modeline-field-names*)
    (error "Modeline field ~S already exists."
	   (gethash name *modeline-field-names*)))
  (remhash (modeline-field-%name ml-field) *modeline-field-names*)
  (setf (modeline-field-%name ml-field) name)
  (setf (gethash name *modeline-field-names*) ml-field))

(defun modeline-field-width (ml-field)
  "Returns the width of a modeline field."
  (modeline-field-%width ml-field))

(declaim (special *buffer-list*))

(defun %set-modeline-field-width (ml-field width)
  (check-type ml-field modeline-field)
  (unless (or (eq width nil) (and (integerp width) (plusp width)))
    (error "Width must be nil or a positive integer."))
  (unless (eql width (modeline-field-%width ml-field))
    (setf (modeline-field-%width ml-field) width)
    (dolist (b *buffer-list*)
      (when (buffer-modeline-field-p b ml-field)
	(dolist (w (buffer-windows b))
	  (update-modeline-fields b w)))))
  width)
  
(defun modeline-field-function (ml-field)
  "Returns the function of a modeline field object.  It returns a string."
  (modeline-field-%function ml-field))

(defun %set-modeline-field-function (ml-field function)
  (check-type ml-field modeline-field)
  (check-type function (or symbol function))
  (setf (modeline-field-%function ml-field) function)
  (dolist (b *buffer-list*)
    (when (buffer-modeline-field-p b ml-field)
      (dolist (w (buffer-windows b))
	(update-modeline-field b w ml-field))))
  function)



;;;; Modelines maintenance.

;;; Each window stores a modeline-buffer which is a string hunk-width-limit
;;; long.  Whenever a field is updated, we must maintain a maximally long
;;; representation of the modeline in case the window is resized.  Updating
;;; then first gets the modeline-buffer setup, and second blasts the necessary
;;; portion into the window's modeline-dis-line, setting the dis-line's changed
;;; flag.
;;;

(defun update-modeline-fields (buffer window)
  "Recompute all the fields of buffer's modeline for window, so the next
   redisplay will reflect changes."
  (let ((ml-buffer (window-modeline-buffer window)))
    (declare (simple-string ml-buffer))
    (when ml-buffer
      (let* ((ml-buffer-len
	      (do ((finfos (buffer-%modeline-fields buffer) (cdr finfos))
		   (start 0 (blt-modeline-field-buffer
			     ml-buffer (car finfos) buffer window start)))
		  ((null finfos) start)))
	     (dis-line (window-modeline-dis-line window))
	     (len (min (window-width window) ml-buffer-len)))
	(replace (the simple-string (dis-line-chars dis-line)) ml-buffer
		 :end1 len :end2 len)
	(setf (window-modeline-buffer-len window) ml-buffer-len)
	(setf (dis-line-length dis-line) len)
	(setf (dis-line-flags dis-line) changed-bit)))))

;;; UPDATE-MODELINE-FIELD must replace the entire dis-line-chars with ml-buffer
;;; after blt'ing into buffer.  Otherwise it has to do all the work
;;; BLT-MODELINE-FIELD-BUFFER to figure out how to adjust dis-line-chars.  It
;;; isn't worth it.  Since things could have shifted around, after calling
;;; BLT-MODELINE-FIELD-BUFFER, we get the last field's end to know how long
;;; the buffer is now.
;;;
(defun update-modeline-field (buffer window field)
  "Recompute the field of the buffer's modeline for window, so the next
   redisplay will reflect the change.  Field is either a modeline-field object
   or the name of one for buffer."
  (let ((finfo (internal-buffer-modeline-field-p buffer field)))
    (unless finfo
      (error "~S is not a modeline-field or the name of one for buffer ~S."
	     field buffer))
    (let ((ml-buffer (window-modeline-buffer window))
	  (dis-line (window-modeline-dis-line window)))
      (declare (simple-string ml-buffer))
      (blt-modeline-field-buffer ml-buffer finfo buffer window
				 (ml-field-info-start finfo) t)
      (let* ((ml-buffer-len (ml-field-info-end
			     (car (last (buffer-%modeline-fields buffer)))))
	     (dis-len (min (window-width window) ml-buffer-len)))
	(replace (the simple-string (dis-line-chars dis-line)) ml-buffer
		 :end1 dis-len :end2 dis-len)
	(setf (window-modeline-buffer-len window) ml-buffer-len)
	(setf (dis-line-length dis-line) dis-len)
	(setf (dis-line-flags dis-line) changed-bit)))))

(defvar *truncated-field-char* #\!)

;;; BLT-MODELINE-FIELD-BUFFER takes a Hemlock buffer, Hemlock window, the
;;; window's modeline buffer, a modeline-field-info object, a start in the
;;; modeline buffer, and an optional indicating whether a variable width field
;;; should be handled carefully.  When the field is fixed-width, this is
;;; simple.  When it is variable, we possibly have to shift all the text in the
;;; buffer right or left before storing the new string, updating all the
;;; finfo's after the one we're updating.  It is an error for the
;;; modeline-field-function to return anything but a simple-string with
;;; standard-chars.  This returns the end of the field blasted into ml-buffer.
;;;
(defun blt-modeline-field-buffer (ml-buffer finfo buffer window start
					    &optional fix-other-fields-p)
  (declare (simple-string ml-buffer))
  (let* ((f (ml-field-info-field finfo))
	 (width (modeline-field-width f))
	 (string (funcall (modeline-field-function f) buffer window))
	 (str-len (length string)))
    (declare (simple-string string))
    (setf (ml-field-info-start finfo) start)
    (setf (ml-field-info-end finfo)
	  (cond
	   ((not width)
	    (let ((end (min (+ start str-len) hunk-width-limit))
		  (last-end (ml-field-info-end finfo)))
	      (when (and fix-other-fields-p (/= end last-end))
		(blt-ml-field-buffer-fix ml-buffer finfo buffer window
					 end last-end))
	      (replace ml-buffer string :start1 start :end1 end :end2 str-len)
	      end))
	   ((= str-len width)
	    (let ((end (min (+ start width) hunk-width-limit)))
	      (replace ml-buffer string :start1 start :end1 end :end2 width)
	      end))
	   ((> str-len width)
	    (let* ((end (min (+ start width) hunk-width-limit))
		   (end-1 (1- end)))
	      (replace ml-buffer string :start1 start :end1 end-1 :end2 width)
	      (setf (schar ml-buffer end-1) *truncated-field-char*)
	      end))
	   (t
	    (let ((buf-replace-end (min (+ start str-len) hunk-width-limit))
		  (buf-field-end (min (+ start width) hunk-width-limit)))
	      (replace ml-buffer string
		       :start1 start :end1 buf-replace-end :end2 str-len)
	      (fill ml-buffer #\space :start buf-replace-end :end buf-field-end)
	      buf-field-end))))))

;;; BLT-ML-FIELD-BUFFER-FIX shifts the contents of ml-buffer in the direction
;;; of last-end to end.  finfo is a modeline-field-info structure in buffer's
;;; list of these.  If there are none following finfo, then we simply store the
;;; new end of the buffer.  After blt'ing the text around, we have to update
;;; all the finfos' starts and ends making sure nobody gets to stick out over
;;; the ml-buffer's end.
;;;
(defun blt-ml-field-buffer-fix (ml-buffer finfo buffer window end last-end)
  (declare (simple-string ml-buffer))
  (let ((finfos (do ((f (buffer-%modeline-fields buffer) (cdr f)))
		    ((null f) (error "This field must be here."))
		  (if (eq (car f) finfo)
		      (return (cdr f))))))
    (cond
     ((not finfos)
      (setf (window-modeline-buffer-len window) (min end hunk-width-limit)))
     (t
      (let ((buffer-len (window-modeline-buffer-len window)))
	(replace ml-buffer ml-buffer
		 :start1 end
		 :end1 (min (+ end (- buffer-len last-end)) hunk-width-limit)
		 :start2 last-end :end2 buffer-len)
	(let ((diff (- end last-end)))
	  (macrolet ((frob (f)
		       `(setf ,f (min (+ ,f diff) hunk-width-limit))))
	    (dolist (f finfos)
	      (frob (ml-field-info-start f))
	      (frob (ml-field-info-end f)))
	    (frob (window-modeline-buffer-len window)))))))))



;;;; Default modeline and update hooks.

(make-modeline-field :name :hemlock-literal :width 8
		     :function #'(lambda (buffer window)
				   "Returns \"Hemlock \"."
				   (declare (ignore buffer window))
				   "Hemlock "))

(make-modeline-field
 :name :package
 :function #'(lambda (buffer window)
	       "Returns the value of buffer's \"Current Package\" followed
		by a colon and two spaces, or a string with one space."
	       (declare (ignore window))
	       (if (hemlock-bound-p 'hemlock::current-package :buffer buffer)
		   (let ((val (variable-value 'hemlock::current-package
					      :buffer buffer)))
		     (if val
			 (format nil "~A:  " val)
			 " "))
		   " ")))

(make-modeline-field
 :name :modes
 :function #'(lambda (buffer window)
	       "Returns buffer's modes followed by one space."
	       (declare (ignore window))
	       (format nil "~A  " (buffer-modes buffer))))

(make-modeline-field
 :name :modifiedp
 :function #'(lambda (buffer window)
	       "Returns \"* \" if buffer is modified, or the empty string."
	       (declare (ignore window))
	       (let ((modifiedp (buffer-modified buffer)))
		 (if modifiedp
		     "* "
		     ""))))

(make-modeline-field
 :name :buffer-name
 :function #'(lambda (buffer window)
	       "Returns buffer's name followed by a colon and a space if the
		name is not derived from the buffer's pathname, or the empty
		string."
	       (declare (ignore window))
	       (let ((pn (buffer-pathname buffer))
		     (name (buffer-name buffer)))
		 (cond ((not pn)
			(format nil "~A: " name))
		       ((string/= (hemlock::pathname-to-buffer-name pn) name)
			(format nil "~A: " name))
		       (t "")))))


;;; MAXIMUM-MODELINE-PATHNAME-LENGTH-HOOK is called whenever "Maximum Modeline
;;; Pathname Length" is set.
;;;
(defun maximum-modeline-pathname-length-hook (name kind where new-value)
  (declare (ignore name new-value))
  (if (eq kind :buffer)
      (hi::queue-buffer-change where)
      (dolist (buffer *buffer-list*)
	(when (and (buffer-modeline-field-p buffer :buffer-pathname)
		   (buffer-windows buffer))
	  (hi::queue-buffer-change buffer)))))

(defun buffer-pathname-ml-field-fun (buffer window)
  "Returns the namestring of buffer's pathname if there is one.  When
   \"Maximum Modeline Pathname Length\" is set, and the namestring is too long,
   return a truncated namestring chopping off leading directory specifications."
  (declare (ignore window))
  (let ((pn (buffer-pathname buffer)))
    (if pn
	(let* ((name (namestring pn))
	       (length (length name))
	       ;; Prefer a buffer local value over the global one.
	       ;; Because variables don't work right, blow off looking for
	       ;; a value in the buffer's modes.  In the future this will
	       ;; be able to get the "current" value as if buffer were current.
	       (max (if (hemlock-bound-p 'hemlock::maximum-modeline-pathname-length
					  :buffer buffer)
			 (variable-value 'hemlock::maximum-modeline-pathname-length
					 :buffer buffer)
			 (variable-value 'hemlock::maximum-modeline-pathname-length
					 :global))))
	  (declare (simple-string name))
	  (if (or (not max) (<= length max))
	      name
	      (let* ((extra-chars (+ (- length max) 3))
		     (slash (or (position #\/ name :start extra-chars)
				;; If no slash, then file-namestring is very
				;; long, and we should include all of it:
				(position #\/ name :from-end t
					  :end extra-chars))))
		(if slash
		    (concatenate 'simple-string "..." (subseq name slash))
		    name))))
	"")))

(make-modeline-field
 :name :buffer-pathname
 :function 'buffer-pathname-ml-field-fun)


(defvar *default-modeline-fields*
  (list (modeline-field :hemlock-literal)
	(modeline-field :package)
	(modeline-field :modes)
	(modeline-field :modifiedp)
	(modeline-field :buffer-name)
	(modeline-field :buffer-pathname))
  "This is the default value for \"Default Modeline Fields\".")



;;; QUEUE-BUFFER-CHANGE is used for various buffer hooks (e.g., mode changes,
;;; name changes, etc.), so it takes some arguments to ignore.  These hooks are
;;; invoked at a bad time to update the actual modeline-field, and user's may
;;; have fields that change as a function of the changes this function handles.
;;; This makes his update easier.  It doesn't cost much update the entire line
;;; anyway.
;;;
(defun queue-buffer-change (buffer &optional something-else another-else)
  (declare (ignore something-else another-else))
  (push (list #'update-modelines-for-buffer buffer) *things-to-do-once*))

(defun update-modelines-for-buffer (buffer)
  (unless (eq buffer *echo-area-buffer*)
    (dolist (w (buffer-windows buffer))
      (update-modeline-fields buffer w))))


;;; QUEUE-WINDOW-CHANGE is used for the "Window Buffer Hook".  We ignore the
;;; argument since this hook function is invoked before any changes are made,
;;; and the changes must be made before the fields can be set according to the
;;; window's buffer's properties.  Therefore, we must queue the change to
;;; happen sometime before redisplay but after the change takes effect.
;;;
(defun queue-window-change (window &optional something-else)
  (declare (ignore something-else))
  (push (list #'update-modeline-for-window window) *things-to-do-once*))

(defun update-modeline-for-window (window)
  (update-modeline-fields (window-buffer window) window))

  

;;;; Bitmap setting up new windows and modifying old.

(defvar dummy-line (make-window-dis-line "")
  "Dummy dis-line that we put at the head of window's dis-lines")
(setf (dis-line-position dummy-line) -1)


;;; WINDOW-FOR-HUNK makes a Hemlock window and sets up its dis-lines and marks
;;; to display starting at start.
;;;
(defun window-for-hunk (hunk start modelinep)
  (check-type start mark)
  (setf (bitmap-hunk-changed-handler hunk) #'window-changed)
  (let ((buffer (line-buffer (mark-line start)))
	(first (cons dummy-line *the-sentinel*))
	(width (bitmap-hunk-char-width hunk))
	(height (bitmap-hunk-char-height hunk)))
    (when (or (< height minimum-window-lines)
	      (< width minimum-window-columns))
      (error "Window too small."))
    (unless buffer (error "Window start is not in a buffer."))
    (let ((window
	   (internal-make-window
	    :hunk hunk
	    :display-start (copy-mark start :right-inserting)
	    :old-start (copy-mark start :temporary)
	    :display-end (copy-mark start :right-inserting)
	    :%buffer buffer
	    :point (copy-mark (buffer-point buffer))
	    :height height
	    :width width
	    :first-line first
	    :last-line *the-sentinel*
	    :first-changed *the-sentinel*
	    :last-changed first
	    :tick -1)))
      (push window *window-list*)
      (push window (buffer-windows buffer))
      ;;
      ;; Make the dis-lines.
      (do ((i (- height) (1+ i))
	   (res ()
		(cons (make-window-dis-line (make-string width)) res)))
	  ((= i height) (setf (window-spare-lines window) res)))
      ;;
      ;; Make the image up to date.
      (update-window-image window)
      (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
      ;;
      ;; If there is a modeline, set it up.
      (when modelinep
	(setup-modeline-image buffer window)
	(setf (bitmap-hunk-modeline-dis-line hunk)
	      (window-modeline-dis-line window)))
      window)))

;;; SETUP-MODELINE-IMAGE sets up the modeline-dis-line for window using the
;;; modeline-fields list.  This is used by tty redisplay too.
;;;
(defun setup-modeline-image (buffer window)
  (setf (window-modeline-buffer window) (make-string hunk-width-limit))
  (setf (window-modeline-dis-line window)
	(make-window-dis-line (make-string (window-width window))))
  (update-modeline-fields buffer window))

;;; Window-Changed  --  Internal
;;;
;;;    The bitmap-hunk changed handler for windows.  This is only called if
;;; the hunk is not locked.  We invalidate the window image and change its
;;; size, then do a full redisplay.
;;;
(defun window-changed (hunk)
  (let ((window (bitmap-hunk-window hunk)))
    ;;
    ;; Nuke all the lines in the window image.
    (unless (eq (cdr (window-first-line window)) *the-sentinel*)
      (shiftf (cdr (window-last-line window))
	      (window-spare-lines window)
	      (cdr (window-first-line window))
	      *the-sentinel*))
    (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
    ;;
    ;; Add some new spare lines if needed.  If width is greater,
    ;; reallocate the dis-line-chars.
    (let* ((res (window-spare-lines window))
	   (new-width (bitmap-hunk-char-width hunk))
	   (new-height (bitmap-hunk-char-height hunk))
	   (width (length (the simple-string (dis-line-chars (car res))))))
      (declare (list res))
      (when (> new-width width)
	(setq width new-width)
	(dolist (dl res)
	  (setf (dis-line-chars dl) (make-string new-width))))
      (setf (window-height window) new-height (window-width window) new-width)
      (do ((i (- (* new-height 2) (length res)) (1- i)))
	  ((minusp i))
	(push (make-window-dis-line (make-string width)) res))
      (setf (window-spare-lines window) res)
      ;;
      ;; Force modeline update.
      (let ((ml-buffer (window-modeline-buffer window)))
	(when ml-buffer
	  (let ((dl (window-modeline-dis-line window))
		(chars (make-string new-width))
		(len (min new-width (window-modeline-buffer-len window))))
	    (setf (dis-line-old-chars dl) nil)
	    (setf (dis-line-chars dl) chars)
	    (replace chars ml-buffer :end1 len :end2 len)
	    (setf (dis-line-length dl) len)
	    (setf (dis-line-flags dl) changed-bit)))))
    ;;
    ;; Prepare for redisplay.
    (setf (window-tick window) (tick))
    (update-window-image window)
    (when (eq window *current-window*) (maybe-recenter-window window))
    hunk))



;;; EDITOR-FINISH-OUTPUT is used to synch output to a window with the rest of the
;;; system.
;;; 
(defun editor-finish-output (window)
  (let* ((device (device-hunk-device (window-hunk window)))
	 (finish-output (device-finish-output device)))
    (when finish-output
      (funcall finish-output device window))))



;;;; Tty setting up new windows and modifying old.

;;; setup-window-image  --  Internal
;;;
;;;    Set up the dis-lines and marks for Window to display starting
;;; at Start.  Height and Width are the number of lines and columns in 
;;; the window.
;;;
(defun setup-window-image (start window height width)
  (check-type start mark)
  (let ((buffer (line-buffer (mark-line start)))
	(first (cons dummy-line *the-sentinel*)))
    (unless buffer (error "Window start is not in a buffer."))
    (setf (window-display-start window) (copy-mark start :right-inserting)
	  (window-old-start window) (copy-mark start :temporary)
	  (window-display-end window) (copy-mark start :right-inserting)
	  (window-%buffer window) buffer
	  (window-point window) (copy-mark (buffer-point buffer))
	  (window-height window) height
	  (window-width window) width
	  (window-first-line window) first
	  (window-last-line window) *the-sentinel*
	  (window-first-changed window) *the-sentinel*
	  (window-last-changed window) first
	  (window-tick window) -1)
    (push window *window-list*)
    (push window (buffer-windows buffer))
    ;;
    ;; Make the dis-lines.
    (do ((i (- height) (1+ i))
	 (res ()
	      (cons (make-window-dis-line (make-string width)) res)))
	((= i height) (setf (window-spare-lines window) res)))
    ;;
    ;; Make the image up to date.
    (update-window-image window)))

;;; change-window-image-height  --  Internal
;;;
;;;    Milkshake.
;;;
(defun change-window-image-height (window new-height)
  ;; Nuke all the lines in the window image.
  (unless (eq (cdr (window-first-line window)) *the-sentinel*)
    (shiftf (cdr (window-last-line window))
	    (window-spare-lines window)
	    (cdr (window-first-line window))
	    *the-sentinel*))
  ;; Add some new spare lines if needed.
  (let* ((res (window-spare-lines window))
	 (width (length (the simple-string (dis-line-chars (car res))))))
    (declare (list res))
    (setf (window-height window) new-height)
    (do ((i (- (* new-height 2) (length res)) (1- i)))
	((minusp i))
      (push (make-window-dis-line (make-string width)) res))
    (setf (window-spare-lines window) res)))
