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
;;;    Written by Rob MacLachlan
;;;
;;; This file contains functions for changing modes and buffers.
;;;

(in-package :hemlock-internals)


;;;; Some buffer structure support.

(defun buffer-writable (buffer)
  "Returns whether buffer may be modified."
  (buffer-%writable buffer))

(defun %set-buffer-writable (buffer value)
  (invoke-hook hemlock::buffer-writable-hook buffer value)
  (setf (buffer-%writable buffer) value))

;;; BUFFER-MODIFIED uses the buffer modification tick which is for redisplay.
;;; We can never set this down to "unmodify" a buffer, so we keep an
;;; unmodification tick.  The buffer is modified only if this is less than the
;;; modification tick.
;;;
(defun buffer-modified (buffer)
  "Return T if Buffer has been modified, NIL otherwise.  Can be set with Setf."
  (unless (bufferp buffer) (error "~S is not a buffer." buffer))
  (> (buffer-modified-tick buffer) (buffer-unmodified-tick buffer)))

(defun %set-buffer-modified (buffer sense)
  "If true make the buffer modified, if NIL unmodified."
  (unless (bufferp buffer) (error "~S is not a buffer." buffer))
  (let* ((was-modified (buffer-modified buffer))
	 (changed (not (eq was-modified (buffer-modified buffer)))))
    (invoke-hook hemlock::buffer-modified-hook buffer sense)
    (if sense
      (setf (buffer-modified-tick buffer) (tick))
      (setf (buffer-unmodified-tick buffer) (tick)))
    (when changed
      (if sense
	(hemlock-ext:note-buffer-unsaved buffer)
	(hemlock-ext:note-buffer-saved buffer))
      (note-modeline-change buffer)))
  sense)


(declaim (inline buffer-name buffer-pathname buffer-region))

(defun buffer-region (buffer)
  "Return the region which contains Buffer's text."
  (buffer-%region buffer))

(defun %set-buffer-region (buffer new-region)
  (let ((old (buffer-region buffer)))
    (delete-region old)
    (ninsert-region (region-start old) new-region)
    old))

(defun buffer-name (buffer)
  "Return Buffer's string name."
  (buffer-%name buffer))

(declaim (special *buffer-names*))

(defun %set-buffer-name (buffer name)
  (multiple-value-bind (entry foundp) (getstring name *buffer-names*)
    (cond ((or (not foundp) (eq entry buffer))
	   (invoke-hook hemlock::buffer-name-hook buffer name)
	   (delete-string (buffer-%name buffer) *buffer-names*)
	   (setf (getstring name *buffer-names*) buffer)
	   (setf (buffer-%name buffer) name))
	  (t (error "Cannot rename buffer ~S to ~S.  Name already in use."
		    buffer name)))))

(defun buffer-pathname (buffer)
  "Return a pathname for the file in Buffer.  This is the truename
  of the file as of the last time it was read or written."
  (buffer-%pathname buffer))


(defun %set-buffer-pathname (buffer pathname)
  (invoke-hook hemlock::buffer-pathname-hook buffer pathname)
  (setf (buffer-%pathname buffer) pathname))

(defun buffer-modeline-fields (buffer)
  "Return a copy of the buffer's modeline fields list."
  (do ((finfos (buffer-%modeline-fields buffer) (cdr finfos))
       (result () (cons (ml-field-info-field (car finfos)) result)))
      ((null finfos) (nreverse result))))

(defun set-buffer-modeline-fields (buffer modeline-fields)
  (unless (every #'modeline-field-p modeline-fields)
    (error "Fields must be a list of modeline-field objects."))
  (setf (buffer-%modeline-fields buffer)
	(do ((fields modeline-fields (cdr fields))
	     (res nil (cons (make-ml-field-info (car fields))
			    res)))
	    ((null fields) (nreverse res)))))

(defun buffer-modeline-field-p (buffer field)
  "If field, a modeline-field or the name of one, is in buffer's list of
   modeline-fields, it is returned; otherwise, nil."
  (let ((finfo (internal-buffer-modeline-field-p buffer field)))
    (if finfo (ml-field-info-field finfo))))

(defun internal-buffer-modeline-field-p (buffer field)
  (let ((fields (buffer-%modeline-fields buffer)))
    (if (modeline-field-p field)
	(find field fields :test #'eq :key #'ml-field-info-field)
	(find field fields
	      :key #'(lambda (f)
		       (modeline-field-name (ml-field-info-field f)))))))



;;;; BUFFER-MAJOR-MODE.

(defmacro with-mode-and-buffer ((name major-p buffer) &body forms)
  `(let ((mode (get-mode-object ,name)))
    (setq ,name (mode-object-name mode))
    (,(if major-p 'unless 'when) (mode-object-major-p mode)
      (error "~S is not a ~:[Minor~;Major~] Mode." ,name ,major-p))
    (check-type ,buffer buffer)
    ,@forms))

;;; BUFFER-MAJOR-MODE  --  Public
;;;
;;;
(defun buffer-major-mode (buffer)
  "Return the name of Buffer's major mode.  To change tha major mode
  use Setf."
  (check-type buffer buffer)
  (mode-object-name (buffer-major-mode-object buffer)))

;;; %SET-BUFFER-MAJOR-MODE  --  Public
;;;
(defun %set-buffer-major-mode (buffer name)
  "Set the major mode of some buffer to the Name'd mode."
  (with-mode-and-buffer (name t buffer)
    (invoke-hook hemlock::buffer-major-mode-hook buffer name)
    (let ((old-mode (buffer-major-mode-object buffer)))
      (invoke-hook (%value (mode-object-hook-name old-mode)) buffer nil)
      (funcall (mode-object-cleanup-function old-mode) buffer))
    (setf (buffer-major-mode-object buffer) mode)
    (invalidate-shadow-attributes buffer)
    (funcall (mode-object-setup-function mode) buffer)
    (invoke-hook (%value (mode-object-hook-name mode)) buffer t))
  nil)



;;;; BUFFER-MINOR-MODE.

;;; BUFFER-MINOR-MODE  --  Public
;;;
;;;    Check if the mode-object is in the buffer's mode-list.
;;;
(defun buffer-minor-mode (buffer name)
  "Return true if the minor mode named Name is active in Buffer.
  A minor mode can be turned on or off with Setf."
  (with-mode-and-buffer (name nil buffer)
    (not (null (member mode (buffer-minor-mode-objects buffer))))))
    
(declaim (special *mode-names*))

;;; %SET-BUFFER-MINOR-MODE  --  Public
;;;
;;;    Activate or deactivate a minor mode, with due respect for
;;; bindings.
;;;
(defun %set-buffer-minor-mode (buffer name new-value)
  (with-mode-and-buffer (name nil buffer)
    (let ((objects (buffer-minor-mode-objects buffer)))
      (unless (if (member mode objects) new-value (not new-value))
        (invoke-hook hemlock::buffer-minor-mode-hook buffer name new-value)
        (cond
         ;; Adding a new mode, insert sorted.
         (new-value
          (do ((m objects (cdr m))
               (prev nil m))
              ((or (null m)
                   (< (mode-object-precedence (car m))
                      (mode-object-precedence mode)))
               (if prev
                 (setf (cdr prev) (cons mode m))
                 (setf (buffer-minor-mode-objects buffer) (setq objects (cons mode m))))))
          (funcall (mode-object-setup-function mode) buffer)
          (invoke-hook (%value (mode-object-hook-name mode)) buffer t))
         (t
          ;; Removing an active mode.
          (invoke-hook (%value (mode-object-hook-name mode)) buffer nil)
          (funcall (mode-object-cleanup-function mode) buffer)
          (setf (buffer-minor-mode-objects buffer) (delq mode (buffer-minor-mode-objects buffer)))))))
    new-value))

;;; BUFFER-MODES -- Public
;;; List of buffer mode names, in precendence order, major mode first.
;;;
(defun buffer-modes (buffer)
  "Return the list of the names of the modes active in a given buffer."
  (cons (buffer-major-mode buffer)
        (nreverse (mapcar #'mode-object-name (buffer-minor-mode-objects buffer)))))


;;;; CURRENT-BUFFER, CURRENT-POINT, and buffer using setup and cleanup.

(declaim (inline current-buffer))

(defun current-buffer () "Return the current buffer object." *current-buffer*)

(defun current-point ()
  "Return the Buffer-Point of the current buffer."
  (buffer-point *current-buffer*))



(defun current-point-collapsing-selection ()
  "Return the Buffer-Point of the current buffer, deactivating the
   region."
  (let* ((b *current-buffer*)
         (point (buffer-point b)))
    ;; Deactivate the region
    (setf (buffer-region-active b) nil)
    point))

(defun current-point-extending-selection ()
  "Return the Buffer-Point of the current buffer, ensuring that
   the region's active."
  (let* ((b *current-buffer*)
         (point (buffer-point b)))
    ;; If the region is active, keep it active.  Otherwise,
    ;; establish a new (empty) region at point.
    (unless (%buffer-current-region-p b)
      (push-new-buffer-mark point t))
    point))

(defun current-point-for-selection-start ()
  "Return the Buffer-Point of the current buffer, ensuring that
   the region's active.  If the region was active but the
   buffer's SELECTION-SET-BY-COMMAND flag is false, ensure that
   point precedes mark by exchanging their positions if necessary."
  (let* ((b *current-buffer*)
         (point (buffer-point b)))
    ;; If the region is active, keep it active.  Otherwise,
    ;; establish a new (empty) region at point.
    (if (%buffer-current-region-p b)
      (unless (buffer-selection-set-by-command b)
        (let* ((mark (current-mark)))
          (if (mark< mark point)
            (with-mark ((temp point))
              (move-mark point mark)
              (move-mark mark temp)))))
      (push-new-buffer-mark point t))
    point))

(defun current-point-for-selection-end ()
  "Return the Buffer-Point of the current buffer, ensuring that
   the region's active.  If the region was active but the
   buffer's SELECTION-SET-BY-COMMAND flag is false, ensure that
   point follows mark by exchanging their positions if necessary."
  (let* ((b *current-buffer*)
         (point (buffer-point b)))
    ;; If the region is active, keep it active.  Otherwise,
    ;; establish a new (empty) region at point.
    (if (%buffer-current-region-p b)
      (unless (buffer-selection-set-by-command b)
        (let* ((mark (current-mark)))
          (if (mark> mark point)
            (with-mark ((temp point))
              (move-mark point mark)
              (move-mark mark temp)))))
      (push-new-buffer-mark point t))
    point))
  


(defun current-point-for-insertion ()
  "Check to see if the current buffer can be modified at its
  current point; error if not.  If there's a selection in the
  current buffer, delete it.  Return the current point."
  (let* ((buffer *current-buffer*)
         (point (buffer-point buffer)))
    (check-buffer-modification buffer point)
    (let* ((region (%buffer-current-region buffer)))
      (when region
        (delete-region region))
      point)))

(defun current-point-for-deletion ()
  "Check to see if the current buffer can be modified at its
  current point; error if not.  If there's a selection in the
  current buffer, delete it and return NIL, else return the
  current point."
  (let* ((buffer *current-buffer*)
         (point (buffer-point buffer)))
    (check-buffer-modification buffer point)
    (let* ((region (%buffer-current-region buffer)))
      (if region
        (progn
          (delete-region region)
          nil)
        point))))

(defun current-point-unless-selection ()
  "Check to see if the current buffer can be modified at its
  current point; error if not.  If there's a selection in the
  current buffer, return NIL, else return the  current point."
  (let* ((buffer *current-buffer*)
         (point (buffer-point buffer)))
    (check-buffer-modification buffer point)
    (let* ((region (%buffer-current-region buffer)))
      (unless region
        point))))

;;;; WITH-WRITABLE-BUFFER

;;; This list indicates recursive use of WITH-WRITABLE-BUFFER on the same
;;; buffer.
;;;
(defvar *writable-buffers* ())

(defmacro with-writable-buffer ((buffer) &body body)
  "Executes body in a scope where buffer is writable.  After body executes,
   this sets the buffer's modified and writable status to nil."
  (let ((buf (gensym))
	(no-unwind (gensym)))
    `(let* ((,buf ,buffer)
	    (,no-unwind (member ,buf *writable-buffers* :test #'eq))
	    (*writable-buffers* (if ,no-unwind
				    *writable-buffers*
				    (cons ,buf *writable-buffers*))))
       (unwind-protect
	   (progn
	     (setf (buffer-writable ,buf) t)
	     ,@body)
	 (unless ,no-unwind
	   (setf (buffer-modified ,buf) nil)
	   (setf (buffer-writable ,buf) nil))))))



;;;; DEFMODE.

(defun defmode (name &key (setup-function #'identity) 
		     (cleanup-function #'identity) major-p transparent-p
		     precedence documentation hidden default-command)
  "Define a new mode, specifying whether it is a major mode, and what the
   setup and cleanup functions are.  Precedence, which defaults to 0.0, and is
   any integer or float, determines the order of the minor modes in a buffer.
   A minor mode having a greater precedence is always considered before a mode
   with lesser precedence when searching for key-bindings and variable values.
   If Transparent-p is true, then all key-bindings local to the defined mode
   are transparent, meaning that they do not shadow other bindings, but rather
   are executed in addition to them.  Documentation is used as introductory
   text for mode describing commands."
  (let ((hook-str (concatenate 'string name " Mode Hook"))
	(mode (getstring name *mode-names*)))
    (cond
     (mode
      (when (if major-p
		(not (mode-object-major-p mode))
		(mode-object-major-p mode))
	(cerror "Let bad things happen"
		"Mode ~S is being redefined as a ~:[Minor~;Major~] mode ~
		where it was ~%~
		previously a ~:*~:[Major~;Minor~] mode." name major-p))
      (warn "Mode ~S is being redefined, variables and bindings will ~
	    be preserved." name)
      (setq name (mode-object-name mode)))
     (t
      (defhvar hook-str
	       (concatenate 'string "This is the mode hook variable for "
	       name " Mode."))
      (setq mode (make-mode-object
		  :variables (make-string-table)
		  :bindings (make-hash-table)
		  :hook-name (getstring hook-str *global-variable-names*)
                  :hidden hidden))
      (setf (getstring name *mode-names*) mode)))

    (when (eq precedence :highest)
      (setq precedence most-positive-double-float))
    (if precedence
	(if major-p
	    (error "Precedence ~S is meaningless for a major mode." precedence)
	    (check-type precedence number))
	(setq precedence 0))
    
    (when default-command
      (setf (mode-object-default-command mode) default-command))

    (setf (mode-object-major-p mode) major-p
	  (mode-object-documentation mode) documentation
	  (mode-object-transparent-p mode) transparent-p
	  (mode-object-precedence mode) precedence
	  (mode-object-setup-function mode) setup-function
	  (mode-object-cleanup-function mode) cleanup-function
	  (mode-object-name mode) name))
  nil)

(defun mode-major-p (name)
  "Returns T if Name is the name of a major mode, or NIL if is the name of
  a minor mode."
  (mode-object-major-p (get-mode-object name)))

(defun mode-variables (name)
  "Return the string-table that contains the names of the modes variables."
  (mode-object-variables (get-mode-object name)))

(defun mode-documentation (name)
  "Returns the documentation for mode with name."
  (mode-object-documentation (get-mode-object name)))



;;;; Making and Deleting buffers.

(defvar *buffer-list* () "A list of all the buffer objects.")

(defvar *current-buffer* ()
  "Internal variable which might contain the current buffer." )

(defun all-buffers ()
  "List of all buffers"
  (remove-if #'echo-buffer-p *buffer-list*))

(ccl:defloadvar *echo-area-counter* 0)

(defun make-echo-buffer ()
  (let* ((name (loop as name = (format nil "Echo Area ~d" (incf *echo-area-counter*))
		  until (null (getstring name *buffer-names*))
		  finally (return name)))
         (buffer (internal-make-echo-buffer
                  :%name name
                  :major-mode-object (getstring "Echo Area" *mode-names*))))
    (initialize-buffer buffer)))

(defun make-buffer (name &key (modes (value hemlock::default-modes))
                              (modeline-fields (value hemlock::default-modeline-fields))
                              delete-hook)
  "Creates and returns a buffer with the given Name if a buffer with Name does
   not already exist, otherwise returns nil.  Modes is a list of mode names,
   and Modeline-fields is a list of modeline field objects.  Delete-hook is a
   list of functions that take a buffer as the argument."
  (when (getstring name *buffer-names*)
    (cerror "Try to delete" "~s already exists" name)
    (let ((buffer (getstring name *buffer-names*)))
      (delete-buffer buffer)))
  (cond ((getstring name *buffer-names*)
	 nil)
	(t
	 (unless (listp delete-hook)
	   (error ":delete-hook is a list of functions -- ~S." delete-hook))
	 (let* ((buffer (internal-make-buffer
                         :%name name
                         :major-mode-object (getstring "Fundamental" *mode-names*)
                         :delete-hook delete-hook)))
           (initialize-buffer buffer :modeline-fields modeline-fields :modes modes)))))

(defun initialize-buffer (buffer &key modeline-fields modes)
  (setf (buffer-bindings buffer) (make-hash-table))
  (setf (buffer-variables buffer) (make-string-table))
  (let ((region (make-empty-region)))
    (setf (line-%buffer (mark-line (region-start region))) buffer)
    (setf (buffer-%region buffer) region)
    (setf (buffer-point buffer) (copy-mark (region-end region))))
  (setf (getstring (buffer-%name buffer) *buffer-names*) buffer)
  (push buffer *buffer-list*)
  (set-buffer-modeline-fields buffer modeline-fields)
  (when modes
    (unless (equalp modes '("Fundamental"))
      (setf (buffer-major-mode buffer) (car modes))
      (dolist (m (cdr modes))
        (setf (buffer-minor-mode buffer m) t))))
  (invoke-hook hemlock::make-buffer-hook buffer)
  buffer)

(defun delete-buffer (buffer)
  "Deletes a buffer.  If buffer is current, an error is signaled."
  (when (eq buffer *current-buffer*)
    (error "Cannot delete current buffer ~S." buffer))
  (when (buffer-document buffer)
    (error "Cannot delete displayed buffer ~S." buffer))
  (invoke-hook (buffer-delete-hook buffer) buffer)
  (invoke-hook hemlock::delete-buffer-hook buffer)
  (setq *buffer-list* (delq buffer *buffer-list*))
  (delete-string (buffer-name buffer) *buffer-names*)
  nil)



;;;; Buffer start and end marks.

(defun buffer-start-mark (buffer)
  "Returns the buffer-region's start mark."
  (region-start (buffer-region buffer)))

(defun buffer-end-mark (buffer)
  "Returns the buffer-region's end mark."
  (region-end (buffer-region buffer)))



;;;; Setting up initial buffer.

;;; SETUP-INITIAL-BUFFER  --  Internal
;;;
;;;    Create the buffer "Main" and the mode "Fundamental".  We make a
;;; dummy fundamental mode before we make the buffer Main, because
;;; "make-buffer" wants fundamental to be defined when it is called, and we
;;; can't make the real fundamental mode until there is a current buffer
;;; because "defmode" wants to invoke its mode definition hook.  Also,
;;; when creating the "Main" buffer, "Default Modeline Fields" is not yet
;;; defined, so we supply this argument to MAKE-BUFFER as nil.  This is
;;; fine since firing up the editor in a core must set the "Main" buffer's
;;; modeline according to this variable in case the user changed it in his
;;; init file.  After the main buffer is created we then define the real
;;; fundamental mode and bash it into the buffer.
;;;
(defun setup-initial-buffer ()
  ;; Make it look like the mode is there so make-buffer doesn't die.
  (setf (getstring "Fundamental" *mode-names*)
	(make-mode-object :major-p t))
  ;; Make it look like there is a make-buffer-hook...
  (setf (get 'hemlock::make-buffer-hook 'hemlock-variable-value)
	(make-variable-object 'foo))
  (setq *current-buffer* (make-buffer "Main" :modes '("Fundamental")
				      :modeline-fields nil))
  ;; Make the bogus variable go away...
  (remf (symbol-plist 'hemlock::make-buffer-hook) 'hemlock-variable-value)
  ;; Make it go away so defmode doesn't die.
  (setf (getstring "Fundamental" *mode-names*) nil)
  (defmode "Fundamental" :major-p t)
  ;; Bash the real mode object into the buffer.
  (let ((obj (getstring "Fundamental" *mode-names*)))
    (setf (buffer-major-mode-object *current-buffer*) obj)))
