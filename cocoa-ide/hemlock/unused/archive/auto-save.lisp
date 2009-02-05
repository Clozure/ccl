;;; -*- Package: Hemlock; Log: hemlock.log -*-
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
;;; Auto-Save Mode
;;; Written by Christopher Hoover
;;;

(in-package :hemlock)


;;;; Per Buffer State Information

;;; 
;;; The auto-save-state structure is used to store the state information for
;;; a particular buffer in "Save" mode, namely the buffer-signature at the last
;;; key stroke, the buffer-signature at the time of the last checkpoint, a count
;;; of the number of destructive keystrokes which have occured since the time of
;;; the last checkpoint, and the pathname used to write the last checkpoint.  It
;;; is generally kept in a buffer-local hvar called "Auto Save State".
;;; 
(defstruct (auto-save-state
	    (:conc-name save-state-)
	    (:print-function print-auto-save-state))
  "Per buffer state for auto-save"
  (buffer nil)				   ; buffer this state is for; for printing
  (key-signature 0 :type fixnum)	   ; buffer-signature at last keystroke
  (last-ckp-signature 0 :type fixnum)	   ; buffer-signature at last checkpoint
  (key-count 0 :type fixnum)		   ; # destructive keystrokes since ckp
  (pathname nil))			   ; pathname used to write last ckp file

(defun print-auto-save-state (auto-save-state stream depth)
  (declare (ignore depth))
  (format stream "#<Auto Save Buffer State for buffer ~A>"
	  (buffer-name (save-state-buffer auto-save-state))))


;;; GET-AUTO-SAVE-STATE tries to get the auto-save-state for the buffer.  If
;;; the buffer is not in "Save" mode then this function returns NIL.
;;;
(defun get-auto-save-state (buffer)
  (if (hemlock-bound-p 'auto-save-state :buffer buffer)
       (variable-value 'auto-save-state :buffer buffer)))

;;; RESET-AUTO-SAVE-STATE resets the auto-save-state of the buffer making it
;;; look as if the buffer was just checkpointed.  This is in fact how
;;; checkpoint-buffer updates the state.  If the buffer is not in "Save" mode
;;; this function punts the attempt and does nothing.
;;;
(defun reset-auto-save-state (buffer)
  (let ((state (get-auto-save-state buffer)))
    (when state
      (let ((signature (buffer-signature buffer)))
	(setf (save-state-key-signature state)
	      signature)
	(setf (save-state-last-ckp-signature state)
	      signature)
	(setf (save-state-key-count state)
	      0)))))



;;;; Checkpoint Pathname Interface/Internal Routines

;;; GET-CHECKPOINT-PATHNAME -- Interface
;;;
;;; Returns the pathname of the checkpoint file for the specified
;;; buffer;  Returns NIL if no checkpoints have been written thus
;;; far or if the buffer isn't in "Save" mode.
;;; 
(defun get-checkpoint-pathname (buffer)
  "Returns the pathname of the checkpoint file for the specified buffer.
   If no checkpoints have been written thus far, or if the buffer is not in
   \"Save\" mode, return nil."
  (let ((state (get-auto-save-state buffer)))
    (if state
	(save-state-pathname state))))

;;; MAKE-UNIQUE-SAVE-PATHNAME is used as the default value for "Auto Save
;;; Pathname Hook" and is mentioned in the User's manual, so it gets a doc
;;; doc string.
;;;
(defun make-unique-save-pathname (buffer)
  "Returns a pathname for a non-existing file in DEFAULT-DIRECTORY.  Uses
   GENSYM to for a file name: save-GENSYM.CKP."
  (declare (ignore buffer))
  (let ((def-dir (hemlock-ext:default-directory)))
    (loop
      (let* ((sym (gensym))
	     (f (merge-pathnames (format nil "save-~A.CKP" sym) def-dir)))
	(unless (probe-file f)
	  (return f))))))
    
(defhvar "Auto Save Pathname Hook"
  "This hook is called by Auto Save to get a checkpoint pathname when there
   is no pathname associated with a buffer.  If this value is NIL, then
   \"Save\" mode is turned off in the buffer.  Otherwise, the function
   will be called. It should take a buffer as its argument and return either
   NIL or a pathname.  If NIL is returned, then \"Save\" mode is turned off
   in the buffer;  else the pathname returned is used as the checkpoint
   pathname for the buffer."
  :value #'make-unique-save-pathname)


;;; MAKE-BUFFER-CKP-PATHNAME attempts to form a pathname by using the buffer's
;;; associated pathname (from buffer-pathname).  If there isn't a pathname
;;; associated with the buffer, the function returns nil.  Otherwise, it uses
;;; the "Auto Save Filename Pattern" and FORMAT to make the checkpoint
;;; pathname.
;;;
(defun make-buffer-ckp-pathname (buffer)
  (let ((buffer-pn (buffer-pathname buffer)))
    (if buffer-pn
	(pathname (format nil
			  (value auto-save-filename-pattern)
			  (directory-namestring buffer-pn)
			  (file-namestring buffer-pn))))))



;;;; Buffer-level Checkpoint Routines

;;;
;;; write-checkpoint-file -- Internal
;;;
;;; Does the low-level write of the checkpoint.  Returns T if it succeeds
;;; and NIL if it fails.  Echoes winnage or lossage to the luser.
;;;
(defun write-checkpoint-file (pathname buffer)
  (let ((ns (namestring pathname)))
    (cond ((hemlock-ext:file-writable pathname)
	   (message "Saving ~A" ns)
	   (handler-case (progn
			   (write-file (buffer-region buffer) pathname
				       :keep-backup nil
				       :access #o600) ;read/write by owner.
			   t)
	     (error (condition)
	       (loud-message "Auto Save failure: ~A" condition)
	       nil)))
	  (t
	   (message "Can't write ~A" ns)
	   nil))))


;;;
;;; To save, or not to save... and to save as what?
;;;
;;; First, make-buffer-ckp-pathname is called. It will return either NIL or
;;; a pathname formed by using buffer-pathname in conjunction with the hvar
;;; "Auto Save Filename Pattern".  If there isn't an associated pathname or
;;; make-buffer-ckp-pathname returns NIL, then we use the pathname we used
;;; the last time we checkpointed the buffer.  If we've never checkpointed
;;; the buffer, then we check "Auto Save Pathname Hook".  If it is NIL then
;;; we turn Save mode off for the buffer, else we funcall the function on
;;; the hook with the buffer as an argument.  The function on the hook should
;;; return either NIL or a pathname. If it returns NIL, we toggle Save mode
;;; off for the buffer;  otherwise, we use the pathname it returned.
;;;

;;; 
;;; checkpoint-buffer -- Internal
;;;
;;; This functions takes a buffer as its argument and attempts to write a
;;; checkpoint for that buffer.  See the notes at the beginning of this page
;;; for how it determines what pathname to use as the checkpoint pathname.
;;; Note that a checkpoint is not necessarily written -- instead "Save"
;;; mode may be turned off for the buffer.
;;;
(defun checkpoint-buffer (buffer)
  (let* ((state (get-auto-save-state buffer))
	 (buffer-ckp-pn (make-buffer-ckp-pathname buffer))
	 (last-pathname (save-state-pathname state)))
    (cond (buffer-ckp-pn
	   (when (write-checkpoint-file buffer-ckp-pn buffer)
	     (reset-auto-save-state buffer)
	     (setf (save-state-pathname state) buffer-ckp-pn)
	     (when (and last-pathname
			(not (equal last-pathname buffer-ckp-pn))
			(probe-file last-pathname))
	       (delete-file last-pathname))))
	  (last-pathname
	   (when (write-checkpoint-file last-pathname buffer)
	     (reset-auto-save-state buffer)))
	  (t
	   (let* ((save-pn-hook (value auto-save-pathname-hook))
		  (new-pn (if save-pn-hook
			      (funcall save-pn-hook buffer))))
	     (cond ((or (not new-pn)
			(zerop (length
				(the simple-string (namestring new-pn)))))
		    (setf (buffer-minor-mode buffer "Save") nil))
		   (t
		    (when (write-checkpoint-file new-pn buffer)
		      (reset-auto-save-state buffer)
		      (setf (save-state-pathname state) new-pn)))))))))

;;;
;;; checkpoint-all-buffers -- Internal
;;; 
;;; This function looks through the buffer list and checkpoints
;;; each buffer that is in "Save" mode that has been modified since
;;; its last checkpoint. 
;;; 
(defun checkpoint-all-buffers (elapsed-time)
  (declare (ignore elapsed-time))
  (dolist (buffer *buffer-list*)
    (let ((state (get-auto-save-state buffer)))
      (when (and state
		 (buffer-modified buffer)
		 (not (eql
		       (save-state-last-ckp-signature state)
		       (buffer-signature buffer))))
	(checkpoint-buffer buffer)))))


;;;; Random Hooks: cleanup, buffer-modified, change-save-freq.

;;;
;;; cleanup-checkpoint -- Internal
;;; 
;;; Cleans up checkpoint file for a given buffer if Auto Save Cleanup
;;; Checkpoints is non-NIL.  This is called via "Write File Hook"
;;; 
(defun cleanup-checkpoint (buffer)
  (let ((ckp-pathname (get-checkpoint-pathname buffer)))
    (when (and (value auto-save-cleanup-checkpoints)
	       ckp-pathname
	       (probe-file ckp-pathname))
      (delete-file ckp-pathname))))

(add-hook write-file-hook 'cleanup-checkpoint)

;;;
;;; notice-buffer-modified -- Internal
;;;
;;; This function is called on "Buffer Modified Hook" to reset
;;; the Auto Save state.  It makes the buffer look like it has just
;;; been checkpointed.
;;;
(defun notice-buffer-modified (buffer flag)
  ;; we care only when the flag has gone to false
  (when (not flag)
    (reset-auto-save-state buffer)))

(add-hook buffer-modified-hook 'notice-buffer-modified)

;;;
;;; change-save-frequency -- Internal
;;; 
;;; This keeps us scheduled at the proper interval.  It is stuck on
;;; the hook list for the hvar "Auto Save Checkpoint Frequency" and
;;; is therefore called whenever this value is set.
;;; 
(defun change-save-frequency (name kind where new-value)
  (declare (ignore name kind where))
  (setq new-value (truncate new-value))
  (remove-scheduled-event 'checkpoint-all-buffers)
  (when (and new-value
	     (plusp new-value))
    (schedule-event new-value 'checkpoint-all-buffers t)))


;;; "Save" mode is in "Default Modes", so turn it off in these modes.
;;;

(defun interactive-modes (buffer on)
  (when on (setf (buffer-minor-mode buffer "Save") nil)))

#+GBNIL (add-hook typescript-mode-hook 'interactive-modes)
#+GBNIL (add-hook eval-mode-hook 'interactive-modes)



;;;; Key Count Routine for Input Hook

;;; 
;;; auto-save-count-keys -- Internal
;;;
;;; This function sits on the Input Hook to eat cycles.  If the current
;;; buffer is not in Save mode or if the current buffer is the echo area
;;; buffer, it does nothing.  Otherwise, we check to see if we have exceeded
;;; the key count threshold (and write a checkpoint if we have) and we
;;; increment the key count for the buffer.
;;;
(defun auto-save-count-keys ()
  #.*fast*
  (let ((buffer (current-buffer)))
    (unless (eq buffer *echo-area-buffer*)
      (let ((state (value auto-save-state))
	    (threshold (value auto-save-key-count-threshold)))
	(when (and state threshold)
	  (let ((signature (buffer-signature buffer)))
	    (declare (fixnum signature))
	    (when (not (eql signature
			    (save-state-key-signature state)))
	      ;; see if we exceeded threshold last time...
	      (when (>= (save-state-key-count state)
			(the fixnum threshold))
		(checkpoint-buffer buffer))
	      ;; update state
	      (setf (save-state-key-signature state) signature)
	      (incf (save-state-key-count state)))))))))

(add-hook input-hook 'auto-save-count-keys)


;;;; Save Mode Hemlock Variables

;;; 
;;; Hemlock variables/parameters for Auto-Save Mode
;;;

(defhvar "Auto Save Filename Pattern"
  "This control-string is used with format to make the filename of the
  checkpoint file.  Format is called with two arguments, the first
  being the directory namestring and the second being the file
  namestring of the default buffer pathname."
  :value "~A~A.CKP")

(defhvar "Auto Save Key Count Threshold"
  "This value is the number of destructive/modifying keystrokes that will
  automatically trigger an checkpoint.  This value may be NIL to turn this
  feature off."
  :value 256)

(defhvar "Auto Save Cleanup Checkpoints"
  "This variable controls whether or not \"Save\" mode will delete the
  checkpoint file for a buffer after it is saved.  If this value is
  non-NIL then cleanup will occur."
  :value t)

(defhvar "Auto Save Checkpoint Frequency"
  "All modified buffers (in \"Save\" mode) will be checkpointed after this
  amount of time (in seconds).  This value may be NIL (or non-positive)
  to turn this feature off."
  :value (* 2 60)
  :hooks '(change-save-frequency))

(defhvar "Auto Save State"
  "Shadow magic.  This variable is seen when in buffers that are not
  in \"Save\" mode.  Do not change this value or you will lose."
  :value nil)


;;;; "Save" mode

(defcommand "Auto Save Mode" (p)
  "If the argument is zero or negative, turn \"Save\" mode off.  If it
  is positive turn \"Save\" mode on.  If there is no argument, toggle
  \"Save\" mode in the current buffer.  When in \"Save\" mode, files
  are automatically checkpointed every \"Auto Save Checkpoint Frequency\"
  seconds or every \"Auto Save Key Count Threshold\" destructive
  keystrokes.  If there is a pathname associated with the buffer, the
  filename used for the checkpoint file is controlled by the hvar \"Auto
  Save Filename Pattern\".  Otherwise, the hook \"Auto Save Pathname Hook\"
  is used to generate a checkpoint pathname.  If the buffer's pathname
  changes between checkpoints, the checkpoint file will be written under
  the new name and the old checkpoint file will be deleted if it exists.
  When a buffer is written out, the checkpoint will be deleted if the
  hvar \"Auto Save Cleanup Checkpoints\" is non-NIL."
  "Turn on, turn off, or toggle \"Save\" mode in the current buffer."
  (setf (buffer-minor-mode (current-buffer) "Save")
	(if p
	    (plusp p)
	    (not (buffer-minor-mode (current-buffer) "Save")))))

(defun setup-auto-save-mode (buffer)
  (let* ((signature (buffer-signature buffer))
	 (state (make-auto-save-state
		 :buffer buffer
		 :key-signature (the fixnum signature)
		 :last-ckp-signature (the fixnum signature))))
    ;; shadow the global value with a variable which will
    ;; contain our per buffer state information
    (defhvar "Auto Save State"
      "This is the \"Save\" mode state information for this buffer."
      :buffer buffer
      :value state)))

(defun cleanup-auto-save-mode (buffer)
  (delete-variable 'auto-save-state
		   :buffer buffer))

(defmode "Save"
  :setup-function 'setup-auto-save-mode
  :cleanup-function 'cleanup-auto-save-mode)
