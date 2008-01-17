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
;;; This file contains file/buffer manipulating commands.
;;;

(in-package :hemlock)



;;;; PROCESS-FILE-OPTIONS.

(defvar *mode-option-handlers* ()
  "Do not modify this; use Define-File-Option instead.")

(defvar *file-type-hooks* ()
  "Do not modify this; use Define-File-Type-Hook instead.")  

(defun trim-subseq (string start end)
  (declare (simple-string string))
  (string-trim '(#\Space #\Tab) (subseq string start end)))

;;; PROCESS-FILE-OPTIONS checks the first line of buffer for the file options
;;; indicator "-*-".  IF it finds this, then it enters a do-file-options block.
;;; If any parsing errors occur while picking out options, we return from this
;;; block.  Staying inside this function at this point, allows us to still set
;;; a major mode if no file option specified one.
;;;
;;; We also cater to old style mode comments:
;;;    -*- Lisp -*-
;;;    -*- Text -*-
;;; This kicks in if we find no colon on the file options line.
;;;
(defun process-file-options (buffer &optional
				    (pathname (buffer-pathname buffer)))
  "Checks for file options and invokes handlers if there are any.  If no
   \"Mode\" mode option is specified, then this tries to invoke the appropriate
   file type hook."
  (let* ((string
	  (line-string (mark-line (buffer-start-mark buffer))))
	 (found (search "-*-" string))
	 (no-major-mode t)
	 (type (if pathname (pathname-type pathname))))
    (declare (simple-string string))
    (when found
      (block do-file-options
	(let* ((start (+ found 3))
	       (end (search "-*-" string :start2 start)))
	  (unless end
	    (loud-message "No closing \"-*-\".  Aborting file options.")
	    (return-from do-file-options))
	  (cond
	   ((find #\: string :start start :end end)
	    (do ((opt-start start (1+ semi)) colon semi)
		(nil)
	      (setq colon (position #\: string :start opt-start :end end))
	      (unless colon
		(loud-message "Missing \":\".  Aborting file options.")
		(return-from do-file-options))
	      (setq semi (or (position #\; string :start colon :end end) end))
	      (let* ((option (nstring-downcase
			      (trim-subseq string opt-start colon)))
		     (handler (assoc option *mode-option-handlers*
				     :test #'string=)))
		(declare (simple-string option))
		(cond
		 (handler
		  (let ((result (funcall (cdr handler) buffer
					 (trim-subseq string (1+ colon) semi))))
		    (when (string= option "mode")
		      (setq no-major-mode (not result)))))
		 (t (message "Unknown file option: ~S" option)))
		(when (= semi end) (return nil)))))
	   (t
	    ;; Old style mode comment.
	    (setq no-major-mode nil)
	    (funcall (cdr (assoc "mode" *mode-option-handlers* :test #'string=))
		     buffer (trim-subseq string start end)))))))
    (when (and no-major-mode type)
      (let ((hook (assoc (string-downcase type) *file-type-hooks*
			 :test #'string=)))
	(when hook (funcall (cdr hook) buffer type))))))



;;;; File options and file type hooks.

(defmacro define-file-option (name lambda-list &body body)
  "Define-File-Option Name (Buffer Value) {Form}*
   Defines a new file option to be user in the -*- line at the top of a file.
   The body is evaluated with Buffer bound to the buffer the file has been read
   into and Value to the string argument to the option."
  (let ((name (string-downcase name)))
    `(setf (cdr (or (assoc ,name *mode-option-handlers*  :test #'string=)
		    (car (push (cons ,name nil) *mode-option-handlers*))))
	   #'(lambda ,lambda-list ,@body))))

(define-file-option "Mode" (buffer str)
  (let ((seen-major-mode-p nil)
	(lastpos 0))
    (loop
      (let* ((pos (position #\, str :start lastpos))
	     (substr (trim-subseq str lastpos pos)))
	(cond ((getstring substr *mode-names*)
	       (cond ((mode-major-p substr)
		      (when seen-major-mode-p
			(loud-message
			 "Major mode already processed. Using ~S now."
			 substr))
		      (setf seen-major-mode-p t)
		      (setf (buffer-major-mode buffer) substr))
		     (t
 		      (setf (buffer-minor-mode buffer substr) t))))
	      (t
	       (loud-message "~S is not a defined mode -- ignored." substr)))
	(unless pos
	  (return seen-major-mode-p))
	(setf lastpos (1+ pos))))))


(defmacro define-file-type-hook (type-list (buffer type) &body body)
  "Define-File-Type-Hook ({Type}*) (Buffer Type) {Form}*
  Define some code to be evaluated when a file having one of the specified
  Types is read by a file command.  Buffer is bound to the buffer the
  file is in, and Type is the actual type read."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,type) ,@body))
       (dolist (,str ',(mapcar #'string-downcase type-list))
	 (setf (cdr (or (assoc ,str *file-type-hooks*  :test #'string=)
			(car (push (cons ,str nil) *file-type-hooks*))))
	       #',fun)))))

(define-file-type-hook ("pas" "pasmac" "macro" "defs" "spc" "bdy")
  		       (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Pascal"))

(define-file-type-hook ("lisp" "slisp" "l" "lsp" "mcl" "cl") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Lisp"))

(define-file-type-hook ("txt" "text" "tx") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Text"))



;;;; Support for file hacking commands:

(defhvar "Pathname Defaults"
  "This variable contains a pathname which is used to supply defaults
   when we don't have anything better."
  :value (pathname "gazonk.del"))

(defhvar "Last Resort Pathname Defaults"
  "This variable contains a pathname which is used to supply defaults when
   we don't have anything better, but unlike \"Pathname Defaults\", this is
   never set to some buffer's pathname."
  :value (pathname "gazonk"))

(defhvar "Last Resort Pathname Defaults Function"
  "This variable contains a function that is called when a default pathname is
   needed, the buffer has no pathname, and the buffer's name is not entirely
   composed of alphanumerics.  The default value is a function that simply
   returns \"Last Resort Pathname Defaults\".  The function must take a buffer
   as an argument, and it must return some pathname."
  :value #'(lambda (buffer)
	     (declare (ignore buffer))
	     (merge-pathnames (value last-resort-pathname-defaults)
			      (value pathname-defaults))))

(defun buffer-default-pathname (buffer)
  "Returns \"Buffer Pathname\" if it is bound.  If it is not, and buffer's name
   is composed solely of alphnumeric characters, then return a pathname formed
   from the buffer's name.  If the buffer's name has other characters in it,
   then return the value of \"Last Resort Pathname Defaults Function\" called
   on buffer."
  (or (buffer-pathname buffer)
      (if (every #'alphanumericp (the simple-string (buffer-name buffer)))
	  (merge-pathnames (make-pathname :name (buffer-name buffer))
			   (value pathname-defaults))
	  (funcall (value last-resort-pathname-defaults-function) buffer))))


(defun pathname-to-buffer-name (pathname)
  "Returns a simple-string using components from pathname."
  (let ((pathname (pathname pathname)))
    (concatenate 'simple-string
		 (file-namestring pathname)
		 " "
		 (directory-namestring pathname))))



;;;; File hacking commands.

(defcommand "Process File Options" (p)
  "Reprocess this buffer's file options."
  "Reprocess this buffer's file options."
  (declare (ignore p))
  (process-file-options (current-buffer)))

(defcommand "Ensure File Options Line" (p)
  "Insert a default file options line at the beginning of the buffer, unless such a line already exists."
  "Insert a default file options line at the beginning of the buffer, unless such a line already exists."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
	 (string
	  (line-string (mark-line (buffer-start-mark buffer))))
	 (found (search "-*-" string))
	 (end (if found (search "-*-" string :start2 (+ found 3)))))
    (unless end
      (let* ((mode (buffer-major-mode buffer)))
	(unless mode
	  ;; Try to derive the buffer's major mode from its pathname's
	  ;; type.
	  (let* ((pathname (buffer-pathname buffer))
		 (type (if pathname (pathname-type pathname)))
		 (hook (if type
			 (assoc (string-downcase type) *file-type-hooks*
				:test #'string=))))
	    (when hook
	      (funcall (cdr hook) buffer type)
	      (setq mode (buffer-major-mode buffer)))))
	(with-mark ((mark (buffer-start-mark buffer) :left-inserting))
	  (if (string-equal mode "Lisp")
	    (let* ((package-name
		    (if (hemlock-bound-p 'current-package :buffer buffer)
		      (variable-value 'hemlock::current-package
				      :buffer buffer)
		      "CL-USER")))
	      (insert-string
	       mark
	       (format nil ";;; -*- Mode: Lisp; Package: ~a -*-" package-name)))
	    (insert-string
	     mark
	     (format nil ";;; -*- Mode: ~a -*-" (or mode "Fundamental"))))
	  (insert-character mark #\NewLine))))
    (buffer-start (buffer-point buffer))))
    
    
			 
			   
	    
	
	    
	    
	  
		 
	
  

(defcommand "Insert File" (p &optional pathname (buffer (current-buffer)))
  "Inserts a file which is prompted for into the current buffer at the point.
  The prefix argument is ignored."
  "Inserts the file named by Pathname into Buffer at the point."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file :default (buffer-default-pathname buffer)
				  :prompt "Insert File: "
				  :help "Name of file to insert")))
	 (point (buffer-point buffer))
	 ;; start and end will be deleted by undo stuff
	 (start (copy-mark point :right-inserting))
	 (end (copy-mark point :left-inserting))
	 (region (region start end)))
    (setv pathname-defaults pn)
    (push-buffer-mark (copy-mark end))
    (read-file pn end)
    (make-region-undo :delete "Insert File" region)))

(defcommand "Write Region" (p &optional pathname)
  "Writes the current region to a file. "
  "Writes the current region to a file. "
  (declare (ignore p))
  (let ((region (current-region))
	(pn (or pathname
		(prompt-for-file :prompt "File to Write: "
				 :help "The name of the file to write the region to. "
				 :default (buffer-default-pathname
					   (current-buffer))
				 :must-exist nil))))
    (write-file region pn)
    (message "~A written." (namestring (truename pn)))))



;;;; Visiting and reverting files.

(defcommand "Visit File" (p &optional pathname (buffer (current-buffer)))
  "Replaces the contents of Buffer with the file Pathname.  The prefix
   argument is ignored.  The buffer is set to be writable, so its region
   can be deleted."
  "Replaces the contents of the current buffer with the text in the file
   which is prompted for.  The prefix argument is, of course, ignored p times."
  (declare (ignore p))
  (when (and (buffer-modified buffer)
	     (prompt-for-y-or-n :prompt "Buffer is modified, save it? "))
    (save-file-command () buffer))
  (let ((pn (or pathname
		(prompt-for-file :prompt "Visit File: "
				 :must-exist nil
				 :help "Name of file to visit."
				 :default (buffer-default-pathname buffer)))))
    (setf (buffer-writable buffer) t)
    (read-buffer-file pn buffer)
    (let ((n (pathname-to-buffer-name (buffer-pathname buffer))))
      (unless (getstring n *buffer-names*)
	(setf (buffer-name buffer) n))
      (warn-about-visit-file-buffers buffer))))

(defun warn-about-visit-file-buffers (buffer)
  (let ((buffer-pn (buffer-pathname buffer)))
    (dolist (b *buffer-list*)
      (unless (eq b buffer)
	(let ((bpn (buffer-pathname b)))
	  (when (equal bpn buffer-pn)
	    (loud-message "Buffer ~A also contains ~A."
			  (buffer-name b) (namestring buffer-pn))
	    (return)))))))


(defhvar "Revert File Confirm"
  "If this is true, Revert File will prompt before reverting."
  :value t)

(defcommand "Revert File" (p)
  "Unless in Save Mode, reads in the last saved version of the file in
   the current buffer. When in Save Mode, reads in the last checkpoint or
   the last saved version, whichever is more recent. An argument will always
   force Revert File to use the last saved version. In either case, if the
   buffer has been modified and \"Revert File Confirm\" is true, then Revert
   File will ask for confirmation beforehand. An attempt is made to maintain
   the point's relative position."
  "With an argument reverts to the last saved version of the file in the
   current buffer. Without, reverts to the last checkpoint or last saved
   version, whichever is more recent."
  (declare (ignore p))
  (let* ((doc (hi::buffer-document (current-buffer))))
    (when doc
      (hi::revert-document doc)))
  (clear-echo-area))

;;; REVERT-PATHNAME -- Internal
;;;
;;; If in Save Mode, return either the checkpoint pathname or the buffer
;;; pathname whichever is more recent. Otherwise return the buffer-pathname
;;; if it exists. If neither file exists, return NIL.
;;; 
(defun revert-pathname (buffer)
  (let* ((buffer-pn (buffer-pathname buffer))
	 (buffer-pn-date (file-write-date buffer-pn))
	 (checkpoint-pn (get-checkpoint-pathname buffer))
	 (checkpoint-pn-date (and checkpoint-pn
				  (file-write-date checkpoint-pn))))
    (cond (checkpoint-pn-date
	   (if (> checkpoint-pn-date (or buffer-pn-date 0))
	       (values checkpoint-pn t)
	       (values buffer-pn nil)))
	  (buffer-pn-date (values buffer-pn nil))
	  (t (values nil nil)))))



;;;; Find file.


(defcommand "Old Find File" (p &optional pathname)
  "Visit a file in its own buffer.
   If the file is already in some buffer, select that buffer,
   otherwise make a new buffer with the same name as the file and
   read the file into it."
  "Make a buffer containing the file Pathname current, creating a buffer
   if necessary.  The buffer is returned."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file 
		  :prompt "Find File: "
		  :must-exist nil
		  :help "Name of file to read into its own buffer."
		  :default (buffer-default-pathname (current-buffer)))))
	 (buffer (find-file-buffer pn)))
    (change-to-buffer buffer)
    buffer))

(defcommand "Find File" (p &optional pathname)
  "Visit a file in its own buffer.
   If the file is already in some buffer, select that buffer,
   otherwise make a new buffer with the same name as the file and
   read the file into it."
  "Make a buffer containing the file Pathname current, creating a buffer
   if necessary.  The buffer is returned."
  (if pathname
    (old-find-file-command p pathname)
    (hi::open-document)))
  


(defun find-file-buffer (pathname)
  "Return a buffer assoicated with the file Pathname, reading the file into a
   new buffer if necessary.  The second value is T if we created a buffer, NIL
   otherwise.  If the file has already been read, we check to see if the file
   has been modified on disk since it was read, giving the user various
   recovery options."
  (let* ((pathname (pathname pathname))
	 (trial-pathname (or (probe-file pathname)
			     (merge-pathnames pathname (hemlock-ext:default-directory))))
	 (found (find trial-pathname (the list *buffer-list*)
		     :key #'buffer-pathname :test #'equal)))
    (cond ((not found)
           (if (and (null (pathname-name trial-pathname))
                    (null (pathname-type trial-pathname))
                    (pathname-directory trial-pathname))
               ;; This looks like a directory -- make dired buffer
               (dired-guts nil nil trial-pathname)

               (let* ((name (pathname-to-buffer-name trial-pathname))
                      (found (getstring name *buffer-names*))
                      (use (if found
                               (prompt-for-buffer
                                :prompt "Buffer to use: "
                                :help
                                "Buffer name in use; give another buffer name, or confirm to reuse."
                                :default found
                                :must-exist nil)
                               (make-buffer name)))
                      (buffer (if (stringp use) (make-buffer use) use)))
                 (when (and (buffer-modified buffer)
                            (prompt-for-y-or-n :prompt
                                               "Buffer is modified, save it? "))
                   (save-file-command () buffer))
                 (read-buffer-file pathname buffer)
                 (values buffer (stringp use)))))
	  ((check-disk-version-consistent pathname found)
	   (values found nil))
	  (t
	   (read-buffer-file pathname found)
	   (values found nil)))))


;;; Check-Disk-Version-Consistent  --  Internal
;;;
;;;    Check that Buffer contains a valid version of the file Pathname,
;;; harrassing the user if not.  We return true if the buffer is O.K., and
;;; false if the file should be read. 
;;;
(defun check-disk-version-consistent (pathname buffer)
  (let ((ndate (file-write-date pathname))
	(odate (buffer-write-date buffer)))
    (cond ((not (and ndate odate (/= ndate odate)))
	   t)
	  ((buffer-modified buffer)
	   (beep)
	   (clear-input)
	   (command-case (:prompt (list
 "File has been changed on disk since it was read and you have made changes too!~
 ~%Read in the disk version of ~A? [Y] " (namestring pathname))
			  :help
 "The file in disk has been changed since Hemlock last saved it, meaning that
 someone else has probably overwritten it.  Since the version read into Hemlock
 has been changed as well, the two versions may have inconsistent changes.  If
 this is the case, it would be a good idea to save your changes in another file
 and compare the two versions.
 
 Type one of the following commands:")
	     ((:confirm :yes)
 "Prompt for a file to write the buffer out to, then read in the disk version."
	      (write-buffer-file
	       buffer
	       (prompt-for-file
		:prompt "File to save changes in: "
		:help (list "Save buffer ~S to this file before reading ~A."
			    (buffer-name buffer) (namestring pathname))
		:must-exist nil
		:default (buffer-default-pathname buffer)))
	      nil)
	     (:no
	      "Change to the buffer without reading the new version."
	      t)
	     (#\r
	      "Read in the new version, clobbering the changes in the buffer."
	      nil)))
	   (t
	    (not (prompt-for-yes-or-no :prompt
				       (list
 "File has been changed on disk since it was read.~
 ~%Read in the disk version of ~A? "
					(namestring pathname))
				       :help
 "Type Y to read in the new version or N to just switch to the buffer."
				       :default t))))))


(defhvar "Read File Hook"
  "These functions are called when a file is read into a buffer.  Each function
   must take two arguments -- the buffer the file was read into and whether the
   file existed (non-nil) or not (nil).")

(defun read-buffer-file (pathname buffer)
  "Delete the buffer's region, and uses READ-FILE to read pathname into it.
   If the file exists, set the buffer's write date to the file's; otherwise,
   MESSAGE that this is a new file and set the buffer's write date to nil.
   Move buffer's point to the beginning, set the buffer unmodified.  If the
   file exists, set the buffer's pathname to the probed pathname; else, set it
   to pathname merged with DEFAULT-DIRECTORY.  Set \"Pathname Defaults\" to the
   same thing.  Process the file options, and then invoke \"Read File Hook\"."
  (setf (buffer-writable buffer) t)
  (delete-region (buffer-region buffer))
  (let* ((pathname (pathname pathname))
	 (probed-pathname (probe-file pathname))
         (hi::*current-buffer* buffer))
    (cond (probed-pathname
	   (read-file probed-pathname (buffer-point buffer))
	   (setf (buffer-write-date buffer) (file-write-date probed-pathname)))
	  (t
	   (message "(New File)")
	   (setf (buffer-write-date buffer) nil)))
    (buffer-start (buffer-point buffer))
    (setf (buffer-modified buffer) nil)
    (let ((stored-pathname (or probed-pathname
			       (merge-pathnames pathname (hemlock-ext:default-directory)))))
      (setf (buffer-pathname buffer) stored-pathname)
      (setf (value pathname-defaults) stored-pathname)
      (process-file-options buffer stored-pathname)
      (invoke-hook read-file-hook buffer probed-pathname))))



;;;; File writing.

(defhvar "Add Newline at EOF on Writing File"
  "This controls whether WRITE-BUFFER-FILE adds a newline at the end of the
   file when it ends at the end of a non-empty line.  When set, this may be
   :ask-user and WRITE-BUFFER-FILE will prompt; otherwise, just add one and
   inform the user.  When nil, never add one and don't ask."
  :value :ask-user)

(defhvar "Keep Backup Files"
  "When set, .BAK files will be saved upon file writing.  This defaults to nil."
  :value nil)

(defhvar "Write File Hook"
  "These functions are called when a buffer has been written.  Each function
   must take the buffer as an argument.")

(defun write-buffer-file (buffer pathname)
  "Write's buffer to pathname.  This assumes pathname is somehow related to
   the buffer's pathname, and if the buffer's write date is not the same as
   pathname's, then this prompts the user for confirmation before overwriting
   the file.  This consults \"Add Newline at EOF on Writing File\" and
   interacts with the user if necessary.  This sets \"Pathname Defaults\", and
   the buffer is marked unmodified.  The buffer's pathname and write date are
   updated, and the buffer is renamed according to the new pathname if possible.
   This invokes \"Write File Hook\"."
  (let ((buffer-pn (buffer-pathname buffer)))
    (let ((date (buffer-write-date buffer))
	  (file-date (when (probe-file pathname) (file-write-date pathname))))
      (when (and buffer-pn date file-date
		 (equal (make-pathname :version nil :defaults buffer-pn)
			(make-pathname :version nil :defaults pathname))
		 (/= date file-date))
	(unless (prompt-for-yes-or-no :prompt (list
 "File has been changed on disk since it was read.~%Overwrite ~A anyway? "
 (namestring buffer-pn))
				      :help
				      "Type No to abort writing the file or Yes to overwrite the disk version."
				      :default nil)
	  (editor-error "Write aborted."))))
    (let ((val (value add-newline-at-eof-on-writing-file)))
      (when val
	(let ((end (buffer-end-mark buffer)))
	  (unless (start-line-p end)
	    (when (if (eq val :ask-user)
		      (prompt-for-y-or-n
		       :prompt
		       (list "~A~%File does not have a newline at EOF, add one? "
			     (buffer-name buffer))
		       :default t)
		      t)
	      (insert-character end #\newline)
	      (message "Added newline at EOF."))))))
    (setv pathname-defaults pathname)
    (write-file (buffer-region buffer) pathname)
    (let ((tn (truename pathname)))
      (message "~A written." (namestring tn))
      (setf (buffer-modified buffer) nil)
      (unless (equal tn buffer-pn)
	(setf (buffer-pathname buffer) tn))
      (setf (buffer-write-date buffer) (file-write-date tn))
      (let ((name (pathname-to-buffer-name tn)))
	(unless (getstring name *buffer-names*)
	  (setf (buffer-name buffer) name)))))
  (invoke-hook write-file-hook buffer))
 
(defcommand "Write File" (p &optional (buffer (current-buffer)))
  "Writes the contents of Buffer, which defaults to the current buffer to
  the file named by Pathname.  The prefix argument is ignored."
  "Prompts for a file to write the contents of the current Buffer to.
  The prefix argument is ignored."
  (declare (ignore p))
  (let* ((document (hi::buffer-document buffer)))
    (when document
      (hi::save-hemlock-document-as document))))

(defcommand "Save To File" (p &optional (buffer (current-buffer)))
  "Writes the contents of Buffer, which defaults to the current buffer to
  the file named by Pathname.  The prefix argument is ignored."
  "Prompts for a file to write the contents of the current Buffer to.
  The prefix argument is ignored."
  (declare (ignore p))
  (let* ((document (hi::buffer-document buffer)))
    (when document
      (hi::save-hemlock-document-to document))))

(defcommand "Save File" (p &optional (buffer (current-buffer)))
  "Writes the contents of the current buffer to the associated file.  If there
  is no associated file, one is prompted for."
  "Writes the contents of the current buffer to the associated file."
  (declare (ignore p))
  (let* ((document (hi::buffer-document buffer)))
    (when document
      (when (buffer-modified buffer)
        (hi::save-hemlock-document document)))))

(defhvar "Save All Files Confirm"
  "When non-nil, prompts for confirmation before writing each modified buffer."
  :value t)

(defcommand "Save All Files" (p)
  "Saves all modified buffers in their associated files.
  If a buffer has no associated file it is ignored even if it is modified.."
  "Saves each modified buffer that has a file."
  (declare (ignore p))
  (let ((saved-count 0))
    (dolist (b *buffer-list*)
      (let ((pn (buffer-pathname b))
	    (name (buffer-name b)))
	(when
	    (and (buffer-modified b)
		 pn
		 (or (not (value save-all-files-confirm))
		     (prompt-for-y-or-n
		      :prompt (list
			       "Write ~:[buffer ~A as file ~S~;file ~*~S~], ~
			       Y or N: "
			       (string= (pathname-to-buffer-name pn) name)
			       name (namestring pn))
		      :default t)))
	  (write-buffer-file b pn)
	  (incf saved-count))))
    (if (zerop saved-count)
	(message "No files were saved.")
	(message "Saved ~S file~:P." saved-count))))

(defcommand "Save All Files and Exit" (p)
  "Save all modified buffers in their associated files and exit;
  a combination of \"Save All Files\" and \"Exit Hemlock\"."
  "Do a save-all-files-command and then an exit-hemlock."
  (declare (ignore p))
  (save-all-files-command ())
  (exit-hemlock))

(defcommand "Backup File" (p)
  "Write the buffer to a file without changing the associated name."
  "Write the buffer to a file without changing the associated name."
  (declare (ignore p))
  (let ((file (prompt-for-file :prompt "Backup to File: "
			       :help
 "Name of a file to backup the current buffer in."
			       :default (buffer-default-pathname (current-buffer))
			       :must-exist nil)))
    (write-file (buffer-region (current-buffer)) file)
    (message "~A written." (namestring (truename file)))))



;;;; Buffer hacking commands:

(defvar *buffer-history* ()
  "A list of buffers, in order from most recently to least recently selected.")

(defun previous-buffer ()
  "Returns some previously selected buffer that is not the current buffer.
   Returns nil if no such buffer exists."
  (let ((b (car *buffer-history*)))
    (or (if (eq b (current-buffer)) (cadr *buffer-history*) b)
	(find-if-not #'(lambda (x)
			 (or (eq x (current-buffer))
			     (eq x *echo-area-buffer*)))
		     (the list *buffer-list*)))))

;;; ADD-BUFFER-HISTORY-HOOK makes sure every buffer will be visited by
;;; "Circulate Buffers" even if it has never been before.
;;;
(defun add-buffer-history-hook (buffer)
  (let ((ele (last *buffer-history*))
	(new-stuff (list buffer)))
    (if ele
	(setf (cdr ele) new-stuff)
	(setf *buffer-history* new-stuff))))
;;;
(add-hook make-buffer-hook 'add-buffer-history-hook)

;;; DELETE-BUFFER-HISTORY-HOOK makes sure we never end up in a dead buffer.
;;;
(defun delete-buffer-history-hook (buffer)
  (setq *buffer-history* (delq buffer *buffer-history*)))
;;;
(add-hook delete-buffer-hook 'delete-buffer-history-hook)
  
(defun change-to-buffer (buffer)
  "Switches to buffer in the current window maintaining *buffer-history*."
  (setq *buffer-history*
	(cons (current-buffer) (delq (current-buffer) *buffer-history*)))
  (setf (current-buffer) buffer)
  (setf (window-buffer (current-window)) buffer))

(defun delete-buffer-if-possible (buffer)
  "Deletes a buffer if at all possible.  If buffer is the only buffer, other
   than the echo area, signals an error.  Otherwise, find some recently current
   buffer, and make all of buffer's windows display this recent buffer.  If
   buffer is current, set the current buffer to be this recently current
   buffer."
  (let ((new-buf (flet ((frob (b)
			  (or (eq b buffer) (eq b *echo-area-buffer*))))
		   (or (find-if-not #'frob (the list *buffer-history*))
		       (find-if-not #'frob (the list *buffer-list*))))))
    (unless new-buf
      (error "Cannot delete only buffer ~S." buffer))
    (dolist (w (buffer-windows buffer))
      (setf (window-buffer w) new-buf))
    (when (eq buffer (current-buffer))
      (setf (current-buffer) new-buf)))
  (delete-buffer buffer))


(defvar *create-buffer-count* 0)

(defcommand "Create Buffer" (p &optional buffer-name)
  "Create a new buffer.  If a buffer with the specified name already exists,
   then go to it."
  "Create or go to the buffer with the specifed name."
  (declare (ignore p))
  (let ((name (or buffer-name
		  (prompt-for-buffer :prompt "Create Buffer: "
				     :default-string
				     (format nil "Buffer ~D"
					     (incf *create-buffer-count*))
				     :must-exist nil))))
    (if (bufferp name)
	(change-to-buffer name)
	(change-to-buffer (or (getstring name *buffer-names*)
			      (make-buffer name))))))

(defcommand "Select Buffer" (p)
  "Select a different buffer.
   The buffer to go to is prompted for."
  "Select a different buffer.
   The buffer to go to is prompted for."
  (declare (ignore p))
  (let ((buf (prompt-for-buffer :prompt "Select Buffer: "
				:default (previous-buffer))))
    (when (eq buf *echo-area-buffer*)
      (editor-error "Cannot select Echo Area buffer."))
    (change-to-buffer buf)))


(defvar *buffer-history-ptr* ()
  "The successively previous buffer to the current buffer.")

(defcommand "Select Previous Buffer" (p)
  "Select the buffer selected before this one.  If called repeatedly
   with an argument, select the successively previous buffer to the
   current one leaving the buffer history as it is."
  "Select the buffer selected before this one."
  (if p
      (circulate-buffers-command nil)
      (let ((b (previous-buffer)))
	(unless b (editor-error "No previous buffer."))
	(change-to-buffer b)
	;;
	;; If the pointer goes to nil, then "Circulate Buffers" will keep doing
	;; "Select Previous Buffer".
	(setf *buffer-history-ptr* (cddr *buffer-history*))
	(setf (last-command-type) :previous-buffer))))

(defcommand "Circulate Buffers" (p)
  "Advance through buffer history, selecting successively previous buffer."
  "Advance through buffer history, selecting successively previous buffer."
  (declare (ignore p))
  (if (and (eq (last-command-type) :previous-buffer)
	   *buffer-history-ptr*) ;Possibly nil if never CHANGE-TO-BUFFER.
      (let ((b (pop *buffer-history-ptr*)))
	(when (eq b (current-buffer))
	  (setf b (pop *buffer-history-ptr*)))
	(unless b
	  (setf *buffer-history-ptr*
		(or (cdr *buffer-history*) *buffer-history*))
	  (setf b (car *buffer-history*)))
	(setf (current-buffer) b)
	(setf (window-buffer (current-window)) b)
	(setf (last-command-type) :previous-buffer))
      (select-previous-buffer-command nil)))
  

(defcommand "Buffer Not Modified" (p)
  "Make the current buffer not modified."
  "Make the current buffer not modified."
  (declare (ignore p))
  (setf (buffer-modified (current-buffer)) nil)
  (message "Buffer marked as unmodified."))



(defcommand "Set Buffer Read-Only" (p)
  "Toggles the read-only flag for the current buffer."
  "Toggles the read-only flag for the current buffer."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (message "Buffer ~S is now ~:[read-only~;writable~]."
	     (buffer-name buffer)
	     (setf (buffer-writable buffer) (not (buffer-writable buffer))))))

(defcommand "Set Buffer Writable" (p)
  "Make the current buffer modifiable."
  "Make the current buffer modifiable."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (setf (buffer-writable buffer) t)
    (message "Buffer ~S is now writable." (buffer-name buffer))))



























(defun universal-time-to-string (ut)
  (multiple-value-bind (sec min hour day month year)
		       (decode-universal-time ut)
    (format nil "~2,'0D-~A-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
	    day (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
			  "Sep" "Oct" "Nov" "Dec")
		       (1- month))
	    (rem year 100)
	    hour min sec)))





;;;; Window hacking commands:



(defcommand "Split Window" (p)
  "Make a new window by splitting the current window.
   The new window is made the current window and displays starting at
   the same place as the current window."
  "Create a new window which displays starting at the same place
   as the current window."
  (declare (ignore p))
  (let ((new (make-window (window-display-start (current-window)))))
    (unless new (editor-error "Could not make a new window."))
    (setf (current-window) new)))






