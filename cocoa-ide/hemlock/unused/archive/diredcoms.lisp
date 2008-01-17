;;; -*- Log: hemlock.log; Package: Hemlock -*-
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
;;; Simple directory editing support.
;;; This file contains site dependent calls.
;;;
;;; Written by Blaine Burks and Bill Chiles.
;;;

(in-package :hemlock)


(defmode "Dired" :major-p t
  :documentation
  "Dired permits convenient directory browsing and file operations including
   viewing, deleting, copying, renaming, and wildcard specifications.")


(defstruct (dired-information (:print-function print-dired-information)
			      (:conc-name dired-info-))
  pathname		; Pathname of directory.
  pattern		; FILE-NAMESTRING with wildcard possibly.
  dot-files-p		; Whether to include UNIX dot files. 
  write-date		; Write date of directory.
  files			; Simple-vector of dired-file structures.
  file-list)		; List of pathnames for files, excluding directories.

(defun print-dired-information (obj str n)
  (declare (ignore n))
  (format str "#<Dired Info ~S>" (namestring (dired-info-pathname obj))))


(defstruct (dired-file (:print-function print-dired-file)
		       (:constructor make-dired-file (pathname)))
  pathname
  (deleted-p nil)
  (write-date nil))

(defun print-dired-file (obj str n)
  (declare (ignore n))
  (format str "#<Dired-file ~A>" (namestring (dired-file-pathname obj))))



;;;; "Dired" command.
     
;;; *pathnames-to-dired-buffers* is an a-list mapping directory namestrings to
;;; buffers that display their contents.
;;;
(defvar *pathnames-to-dired-buffers* ())

(make-modeline-field
 :name :dired-cmds :width 20
 :function
 #'(lambda (buffer window)
     (declare (ignore buffer window))
     "  Type ? for help.  "))

(defcommand "Dired" (p &optional directory)
  "Prompts for a directory and edits it.  If a dired for that directory already
   exists, go to that buffer, otherwise create one.  With an argument, include
   UNIX dot files."
  "Prompts for a directory and edits it.  If a dired for that directory already
   exists, go to that buffer, otherwise create one.  With an argument, include
   UNIX dot files."
  (let ((info (if (hemlock-bound-p 'dired-information)
		  (value dired-information))))
    (dired-guts nil
		;; Propagate dot-files property to subdirectory edits.
		(or (and info (dired-info-dot-files-p info))
		    p)
		directory)))

(defcommand "Dired with Pattern" (p)
  "Do a dired, prompting for a pattern which may include a single *.  With an
   argument, include UNIX dit files."
  "Do a dired, prompting for a pattern which may include a single *.  With an
   argument, include UNIX dit files."
  (dired-guts t p nil))

(defun dired-guts (patternp dot-files-p directory)
  (let* ((dpn (value pathname-defaults))
	 (directory (dired-directorify
		     (or directory
			 (prompt-for-file
			  :prompt "Edit Directory: "
			  :help "Pathname to edit."
			  :default (make-pathname
				    :device (pathname-device dpn)
				    :directory (pathname-directory dpn))
			  :must-exist nil))))
	 (pattern (if patternp
		      (prompt-for-string
		       :prompt "Filename pattern: "
		       :help "Type a filename with a single asterisk."
		       :trim t)))
	 (full-name (namestring (if pattern
				    (merge-pathnames directory pattern)
				    directory)))
	 (name (concatenate 'simple-string "Dired " full-name))
	 (buffer (cdr (assoc full-name *pathnames-to-dired-buffers*
			     :test #'string=))))
    (declare (simple-string full-name))
    (setf (value pathname-defaults) (merge-pathnames directory dpn))
    (change-to-buffer
     (cond (buffer
	    (when (and dot-files-p
		       (not (dired-info-dot-files-p
			     (variable-value 'dired-information
					     :buffer buffer))))
	      (setf (dired-info-dot-files-p (variable-value 'dired-information
							    :buffer buffer))
		    t)
	      (update-dired-buffer directory pattern buffer))
	    buffer)
	   (t
	    (let ((buffer (make-buffer
			   name :modes '("Dired")
			   :modeline-fields
			   (append (value default-modeline-fields)
				   (list (modeline-field :dired-cmds)))
			   :delete-hook (list 'dired-buffer-delete-hook))))
	      (unless (initialize-dired-buffer directory pattern
					       dot-files-p buffer)
		(delete-buffer-if-possible buffer)
		(editor-error "No entries for ~A." full-name))
	      (push (cons full-name buffer) *pathnames-to-dired-buffers*)
	      buffer))))))

;;; INITIALIZE-DIRED-BUFFER gets a dired in the buffer and defines some
;;; variables to make it usable as a dired buffer.  If there are no file
;;; satisfying directory, then this returns nil, otherwise t.
;;;
(defun initialize-dired-buffer (directory pattern dot-files-p buffer)
  (multiple-value-bind (pathnames dired-files)
		       (dired-in-buffer directory pattern dot-files-p buffer)
    (if (zerop (length dired-files))
	nil
	(defhvar "Dired Information"
	  "Contains the information neccessary to manipulate dired buffers."
	  :buffer buffer
	  :value (make-dired-information :pathname directory
					 :pattern pattern
					 :dot-files-p dot-files-p
					 :write-date (file-write-date directory)
					 :files dired-files
					 :file-list pathnames)))))

;;; CALL-PRINT-DIRECTORY gives us a nice way to report PRINT-DIRECTORY errors
;;; to the user and to clean up the dired buffer.
;;;
(defun call-print-directory (directory mark dot-files-p)
  (handler-case (with-output-to-mark (s mark :full)
		  (print-directory directory s
				   :all dot-files-p :verbose t :return-list t))
    (error (condx)
      (delete-buffer-if-possible (line-buffer (mark-line mark)))
      (editor-error "~A" condx))))

;;; DIRED-BUFFER-DELETE-HOOK is called on dired buffers upon deletion.  This
;;; removes the buffer from the pathnames mapping, and it deletes and buffer
;;; local variables referring to it.
;;;
(defun dired-buffer-delete-hook (buffer)
  (setf *pathnames-to-dired-buffers*
	(delete buffer *pathnames-to-dired-buffers* :test #'eq :key #'cdr)))



;;;; Dired deletion and undeletion.

(defcommand "Dired Delete File" (p)
  "Marks a file for deletion; signals an error if not in a dired buffer.
   With an argument, this prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be flagged
   for deletion."
  "Marks a file for deletion; signals an error if not in a dired buffer."
  (dired-frob-deletion p t))

(defcommand "Dired Undelete File" (p)
  "Removes a mark for deletion; signals and error if not in a dired buffer.
   With an argument, this prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be unflagged
   for deletion."
  "Removes a mark for deletion; signals and error if not in a dired buffer."
  (dired-frob-deletion p nil))

(defcommand "Dired Delete File and Down Line" (p)
  "Marks file for deletion and moves down a line.
   See \"Dired Delete File\"."
  "Marks file for deletion and moves down a line.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion nil t)
  (dired-down-line (current-point)))

(defcommand "Dired Undelete File and Down Line" (p)
  "Marks file undeleted and moves down a line.
   See \"Dired Delete File\"."
  "Marks file undeleted and moves down a line.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion nil nil)
  (dired-down-line (current-point)))

(defcommand "Dired Delete File with Pattern" (p)
  "Prompts for a pattern and marks matching files for deletion.
   See \"Dired Delete File\"."
  "Prompts for a pattern and marks matching files for deletion.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion t t)
  (dired-down-line (current-point)))

(defcommand "Dired Undelete File with Pattern" (p)
  "Prompts for a pattern and marks matching files undeleted.
   See \"Dired Delete File\"."
  "Prompts for a pattern and marks matching files undeleted.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion t nil)
  (dired-down-line (current-point)))

;;; DIRED-FROB-DELETION takes arguments indicating whether to prompt for a
;;; pattern and whether to mark the file deleted or undeleted.  This uses
;;; CURRENT-POINT and CURRENT-BUFFER, and if not in a dired buffer, signal
;;; an error.
;;; 
(defun dired-frob-deletion (patternp deletep)
  (unless (hemlock-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (with-mark ((mark (current-point) :left-inserting))
    (let* ((dir-info (value dired-information))
	   (files (dired-info-files dir-info))
	   (del-files
	    (if patternp
		(dired:pathnames-from-pattern
		 (prompt-for-string
		  :prompt "Filename pattern: "
		  :help "Type a filename with a single asterisk."
		  :trim t)
		 (dired-info-file-list dir-info))
		(list (dired-file-pathname
		       (array-element-from-mark mark files)))))
	   (note-char (if deletep #\D #\space)))
      (with-writable-buffer ((current-buffer))
	(dolist (f del-files)
	  (let* ((pos (position f files :test #'equal
				:key #'dired-file-pathname))
		 (dired-file (svref files pos)))
	    (buffer-start mark)
	    (line-offset mark pos 0)
	    (setf (dired-file-deleted-p dired-file) deletep)
	    (if deletep
		(setf (dired-file-write-date dired-file)
		      (file-write-date (dired-file-pathname dired-file)))
		(setf (dired-file-write-date dired-file) nil))
	    (setf (next-character mark) note-char)))))))

(defun dired-down-line (point)
  (line-offset point 1)
  (when (blank-line-p (mark-line point))
    (line-offset point -1)))



;;;; Dired file finding and going to dired buffers.

(defcommand "Dired Edit File" (p)
  "Read in file or recursively \"Dired\" a directory."
  "Read in file or recursively \"Dired\" a directory."
  (declare (ignore p))
  (let ((point (current-point)))
    (when (blank-line-p (mark-line point)) (editor-error "Not on a file line."))
    (let ((pathname (dired-file-pathname
		     (array-element-from-mark
		      point (dired-info-files (value dired-information))))))
      (if (directoryp pathname)
	  (dired-command nil (directory-namestring pathname))
	  (change-to-buffer (find-file-buffer pathname))))))

(defcommand "Dired View File" (p)
  "Read in file as if by \"View File\" or recursively \"Dired\" a directory.
   This associates the file's buffer with the dired buffer."
  "Read in file as if by \"View File\".
   This associates the file's buffer with the dired buffer."
  (declare (ignore p))
  (let ((point (current-point)))
    (when (blank-line-p (mark-line point)) (editor-error "Not on a file line."))
    (let ((pathname (dired-file-pathname
		     (array-element-from-mark
		      point (dired-info-files (value dired-information))))))
      (if (directoryp pathname)
	  (dired-command nil (directory-namestring pathname))
	  (let* ((dired-buf (current-buffer))
		 (buffer (view-file-command nil pathname)))
	    (push #'(lambda (buffer)
		      (declare (ignore buffer))
		      (setf dired-buf nil))
		  (buffer-delete-hook dired-buf))
	    (setf (variable-value 'view-return-function :buffer buffer)
		  #'(lambda ()
		      (if dired-buf
			  (change-to-buffer dired-buf)
			  (dired-from-buffer-pathname-command nil)))))))))

(defcommand "Dired from Buffer Pathname" (p)
  "Invokes \"Dired\" on the directory part of the current buffer's pathname.
   With an argument, also prompt for a file pattern within that directory."
  "Invokes \"Dired\" on the directory part of the current buffer's pathname.
   With an argument, also prompt for a file pattern within that directory."
  (let ((pathname (buffer-pathname (current-buffer))))
    (if pathname
	(dired-command p (directory-namestring pathname))
	(editor-error "No pathname associated with buffer."))))

(defcommand "Dired Up Directory" (p)
  "Invokes \"Dired\" on the directory up one level from the current Dired
   buffer."
  "Invokes \"Dired\" on the directory up one level from the current Dired
   buffer."
  (declare (ignore p))
  (unless (hemlock-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (let ((dirs (or (pathname-directory
		   (dired-info-pathname (value dired-information)))
		  '(:relative))))
    (dired-command nil
		   (truename (make-pathname :directory (nconc dirs '(:UP)))))))



;;;; Dired misc. commands -- update, help, line motion.

(defcommand "Dired Update Buffer" (p)
  "Recompute the contents of a dired buffer.
   This maintains delete flags for files that have not been modified."
  "Recompute the contents of a dired buffer.
   This maintains delete flags for files that have not been modified."
  (declare (ignore p))
  (unless (hemlock-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (let ((buffer (current-buffer))
	(dir-info (value dired-information)))
    (update-dired-buffer (dired-info-pathname dir-info)
			 (dired-info-pattern dir-info)
			 buffer)))

;;; UPDATE-DIRED-BUFFER updates buffer with a dired of directory, deleting
;;; whatever is in the buffer already.  This assumes buffer was previously
;;; used as a dired buffer having necessary variables bound.  The new files
;;; are compared to the old ones propagating any deleted flags if the name
;;; and the write date is the same for both specifications.
;;;
(defun update-dired-buffer (directory pattern buffer)
  (with-writable-buffer (buffer)
    (delete-region (buffer-region buffer))
    (let ((dir-info (variable-value 'dired-information :buffer buffer)))
      (multiple-value-bind (pathnames new-dired-files)
			   (dired-in-buffer directory pattern
					    (dired-info-dot-files-p dir-info)
					    buffer)
	(let ((point (buffer-point buffer))
	      (old-dired-files (dired-info-files dir-info)))
	  (declare (simple-vector old-dired-files))
	  (dotimes (i (length old-dired-files))
	    (let ((old-file (svref old-dired-files i)))
	      (when (dired-file-deleted-p old-file)
		(let ((pos (position (dired-file-pathname old-file)
				     new-dired-files :test #'equal
				     :key #'dired-file-pathname)))
		  (when pos
		    (let* ((new-file (svref new-dired-files pos))
			   (write-date (file-write-date
					(dired-file-pathname new-file))))
		      (when (= (dired-file-write-date old-file) write-date)
			(setf (dired-file-deleted-p new-file) t)
			(setf (dired-file-write-date new-file) write-date)
			(setf (next-character
			       (line-offset (buffer-start point) pos 0))
			      #\D))))))))
	  (setf (dired-info-files dir-info) new-dired-files)
	  (setf (dired-info-file-list dir-info) pathnames)
	  (setf (dired-info-write-date dir-info)
		(file-write-date directory))
	  (move-mark point (buffer-start-mark buffer)))))))

;;; DIRED-IN-BUFFER inserts a dired listing of directory in buffer returning
;;; two values: a list of pathnames of files only, and an array of dired-file
;;; structures.  This uses FILTER-REGION to insert a space for the indication
;;; of whether the file is flagged for deletion.  Then we clean up extra header
;;; and trailing lines known to be in the output (into every code a little
;;; slime must fall).
;;;
(defun dired-in-buffer (directory pattern dot-files-p buffer)
  (let ((point (buffer-point buffer)))
    (with-writable-buffer (buffer)
      (let* ((pathnames (call-print-directory
			 (if pattern
			     (merge-pathnames directory pattern)
			     directory)
			 point
			 dot-files-p))
	     (dired-files (make-array (length pathnames))))
	(declare (list pathnames) (simple-vector dired-files))
	(filter-region #'(lambda (str)
			   (concatenate 'simple-string "  " str))
		       (buffer-region buffer))
	(delete-characters point -2)
	(delete-region (line-to-region (mark-line (buffer-start point))))
	(delete-characters point)
	(do ((p pathnames (cdr p))
	     (i 0 (1+ i)))
	    ((null p))
	  (setf (svref dired-files i) (make-dired-file (car p))))
	(values (delete-if #'directoryp pathnames) dired-files)))))


(defcommand "Dired Help" (p)
  "How to use dired."
  "How to use dired."
  (declare (ignore p))
  (describe-mode-command nil "Dired"))

(defcommand "Dired Next File" (p)
  "Moves to next undeleted file."
  "Moves to next undeleted file."
  (unless (dired-line-offset (current-point) (or p 1))
    (editor-error "Not enough lines.")))

(defcommand "Dired Previous File" (p)
  "Moves to previous undeleted file."
  "Moves to next undeleted file."
  (unless (dired-line-offset (current-point) (or p -1))
    (editor-error "Not enough lines.")))

;;; DIRED-LINE-OFFSET moves mark n undeleted file lines, returning mark.  If
;;; there are not enough lines, mark remains unmoved, this returns nil.
;;;
(defun dired-line-offset (mark n)
  (with-mark ((m mark))
    (let ((step (if (plusp n) 1 -1)))
      (dotimes (i (abs n) (move-mark mark m))
	(loop
	  (unless (line-offset m step 0)
	    (return-from dired-line-offset nil))
	  (when (blank-line-p (mark-line m))
	    (return-from dired-line-offset nil))
	  (when (char= (next-character m) #\space)
	    (return)))))))



;;;; Dired user interaction functions.

(defun dired-error-function (string &rest args)
  (apply #'editor-error string args))

(defun dired-report-function (string &rest args)
  (clear-echo-area)
  (apply #'message string args))

(defun dired-yesp-function (string &rest args)
  (prompt-for-y-or-n :prompt (cons string args) :default t))



;;;; Dired expunging and quitting.

(defcommand "Dired Expunge Files" (p)
  "Expunges files marked for deletion.
   Query the user if value of \"Dired File Expunge Confirm\" is non-nil.  Do
   the same with directories and the value of \"Dired Directory Expunge
   Confirm\"."
  "Expunges files marked for deletion.
   Query the user if value of \"Dired File Expunge Confirm\" is non-nil.  Do
   the same with directories and the value of \"Dired Directory Expunge
   Confirm\"."
  (declare (ignore p)) 
  (when (expunge-dired-files)
    (dired-update-buffer-command nil))
  (maintain-dired-consistency))

(defcommand "Dired Quit" (p)
  "Expunges the files in a dired buffer and then exits."
  "Expunges the files in a dired buffer and then exits."
  (declare (ignore p))
  (expunge-dired-files)
  (delete-buffer-if-possible (current-buffer)))

(defhvar "Dired File Expunge Confirm"
  "When set (the default), \"Dired Expunge Files\" and \"Dired Quit\" will ask
   for confirmation before deleting the marked files."
  :value t)

(defhvar "Dired Directory Expunge Confirm"
  "When set (the default), \"Dired Expunge Files\" and \"Dired Quit\" will ask
   for confirmation before deleting each marked directory."
  :value t)

(defun expunge-dired-files ()
  (multiple-value-bind (marked-files marked-dirs) (get-marked-dired-files)
    (let ((dired:*error-function* #'dired-error-function)
	  (dired:*report-function* #'dired-report-function)
	  (dired:*yesp-function* #'dired-yesp-function)
	  (we-did-something nil))
      (when (and marked-files
		 (or (not (value dired-file-expunge-confirm))
		     (prompt-for-y-or-n :prompt "Really delete files? "
					:default t
					:must-exist t
					:default-string "Y")))
	(setf we-did-something t)
	(dolist (file-info marked-files)
	  (let ((pathname (car file-info))
		(write-date (cdr file-info)))
	    (if (= write-date (file-write-date pathname))
		(dired:delete-file (namestring pathname) :clobber t
				   :recursive nil)
		(message "~A has been modified, it remains unchanged."
			 (namestring pathname))))))
      (when marked-dirs
	(dolist (dir-info marked-dirs)
	  (let ((dir (car dir-info))
		(write-date (cdr dir-info)))
	    (if (= write-date (file-write-date dir))
		(when (or (not (value dired-directory-expunge-confirm))
			  (prompt-for-y-or-n
			   :prompt (list "~a is a directory. Delete it? "
					 (directory-namestring dir))
			   :default t
			   :must-exist t
			   :default-string "Y"))
		  (dired:delete-file (directory-namestring dir) :clobber t
				     :recursive t)
		  (setf we-did-something t))
		(message "~A has been modified, it remains unchanged.")))))
      we-did-something)))



;;;; Dired copying and renaming.

(defhvar "Dired Copy File Confirm"
  "Can be either t, nil, or :update.  T means always query before clobbering an
   existing file, nil means don't query before clobbering an existing file, and
   :update means only ask if the existing file is newer than the source."
  :value T)

(defhvar "Dired Rename File Confirm"
  "When non-nil, dired will query before clobbering an existing file."
  :value T)

(defcommand "Dired Copy File" (p)
  "Copy the file under the point"
  "Copy the file under the point"
  (declare (ignore p))
  (let* ((point (current-point))
	 (confirm (value dired-copy-file-confirm))
	 (source (dired-file-pathname
		  (array-element-from-mark
		   point (dired-info-files (value dired-information)))))
	 (dest (prompt-for-file
		:prompt (if (directoryp source)
			    "Destination Directory Name: "
			    "Destination Filename: ")
		:help "Name of new file."
		:default source
		:must-exist nil))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*report-function* #'dired-report-function)
	 (dired:*yesp-function* #'dired-yesp-function))
    (dired:copy-file source dest :update (if (eq confirm :update) t nil)
		     :clobber (not confirm)))
  (maintain-dired-consistency))

(defcommand "Dired Rename File" (p)
  "Rename the file or directory under the point"
  "Rename the file or directory under the point"
  (declare (ignore p))
  (let* ((point (current-point))
	 (source (dired-namify (dired-file-pathname
				(array-element-from-mark
				 point
				 (dired-info-files (value dired-information))))))
	 (dest (prompt-for-file
		:prompt "New Filename: "
		:help "The new name for this file."
		:default source
		:must-exist nil))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*report-function* #'dired-report-function)
	 (dired:*yesp-function* #'dired-yesp-function))
    ;; ARRAY-ELEMENT-FROM-MARK moves mark to line start.
    (dired:rename-file source dest :clobber (value dired-rename-file-confirm)))
  (maintain-dired-consistency))

(defcommand "Dired Copy with Wildcard" (p)
  "Copy files that match a pattern containing ONE wildcard."
  "Copy files that match a pattern containing ONE wildcard."
  (declare (ignore p))
  (let* ((dir-info (value dired-information))
	 (confirm (value dired-copy-file-confirm))
	 (pattern (prompt-for-string
		   :prompt "Filename pattern: "
		   :help "Type a filename with a single asterisk."
		   :trim t))
	 (destination (namestring
		       (prompt-for-file
			:prompt "Destination Spec: "
			:help "Destination spec.  May contain ONE asterisk."
			:default (dired-info-pathname dir-info)
			:must-exist nil)))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*yesp-function* #'dired-yesp-function)
	 (dired:*report-function* #'dired-report-function))
    (dired:copy-file pattern destination :update (if (eq confirm :update) t nil)
		     :clobber (not confirm)
		     :directory (dired-info-file-list dir-info)))
  (maintain-dired-consistency))

(defcommand "Dired Rename with Wildcard" (p)
  "Rename files that match a pattern containing ONE wildcard."
  "Rename files that match a pattern containing ONE wildcard."
  (declare (ignore p))
  (let* ((dir-info (value dired-information))
	 (pattern (prompt-for-string
		   :prompt "Filename pattern: "
		   :help "Type a filename with a single asterisk."
		   :trim t))
	 (destination (namestring
		       (prompt-for-file
			:prompt "Destination Spec: "
			:help "Destination spec.  May contain ONE asterisk."
			:default (dired-info-pathname dir-info)
			:must-exist nil)))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*yesp-function* #'dired-yesp-function)
	 (dired:*report-function* #'dired-report-function))
    (dired:rename-file pattern destination
		       :clobber (not (value dired-rename-file-confirm))
		       :directory (dired-info-file-list dir-info)))
  (maintain-dired-consistency))

(defcommand "Delete File" (p)
  "Delete a file.  Specify directories with a trailing slash."
  "Delete a file.  Specify directories with a trailing slash."
  (declare (ignore p))
  (let* ((spec (namestring
		(prompt-for-file
		 :prompt "Delete File: "
		 :help '("Name of File or Directory to delete.  ~
			  One wildcard is permitted.")
		 :must-exist nil)))
	 (directoryp (directoryp spec))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*report-function* #'dired-report-function)
	 (dired:*yesp-function* #'dired-yesp-function))
    (when (or (not directoryp)
	      (not (value dired-directory-expunge-confirm))
	      (prompt-for-y-or-n
	       :prompt (list "~A is a directory. Delete it? "
			     (directory-namestring spec))
	       :default t :must-exist t :default-string "Y")))
    (dired:delete-file spec :recursive t
		       :clobber (or directoryp
				    (value dired-file-expunge-confirm))))
  (maintain-dired-consistency))

(defcommand "Copy File" (p)
  "Copy a file, allowing ONE wildcard."
  "Copy a file, allowing ONE wildcard."
  (declare (ignore p))
  (let* ((confirm (value dired-copy-file-confirm))
	 (source (namestring
		  (prompt-for-file
		   :prompt "Source Filename: "
		   :help "Name of File to copy.  One wildcard is permitted."
		   :must-exist nil)))
	 (dest (namestring
		(prompt-for-file
		 :prompt (if (directoryp source)
			     "Destination Directory Name: "
			     "Destination Filename: ")
		 :help "Name of new file."
		 :default source
		 :must-exist nil)))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*report-function* #'dired-report-function)
	 (dired:*yesp-function* #'dired-yesp-function))
    (dired:copy-file source dest :update (if (eq confirm :update) t nil)
		     :clobber (not confirm)))
  (maintain-dired-consistency))

(defcommand "Rename File" (p)
  "Rename a file, allowing ONE wildcard."
  "Rename a file, allowing ONE wildcard."
  (declare (ignore p))
  (let* ((source (namestring
		  (prompt-for-file
		   :prompt "Source Filename: "
		   :help "Name of file to rename.  One wildcard is permitted."
		   :must-exist nil)))
	 (dest (namestring
		(prompt-for-file
		 :prompt (if (directoryp source)
			     "Destination Directory Name: "
			     "Destination Filename: ")
		 :help "Name of new file."
		 :default source
		 :must-exist nil)))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*report-function* #'dired-report-function)
	 (dired:*yesp-function* #'dired-yesp-function))
    (dired:rename-file source dest
		       :clobber (not (value dired-rename-file-confirm))))
  (maintain-dired-consistency))

(defun maintain-dired-consistency ()
  (dolist (info *pathnames-to-dired-buffers*)
    (let* ((directory (directory-namestring (car info)))
	   (buffer (cdr info))
	   (dir-info (variable-value 'dired-information :buffer buffer))
	   (write-date (file-write-date directory)))
      (unless (= (dired-info-write-date dir-info) write-date)
	(update-dired-buffer directory (dired-info-pattern dir-info) buffer)))))



;;;; Dired utilities.

;;; GET-MARKED-DIRED-FILES returns as multiple values a list of file specs
;;; and a list of directory specs that have been marked for deletion.  This
;;; assumes the current buffer is a "Dired" buffer.
;;;
(defun get-marked-dired-files ()
  (let* ((files (dired-info-files (value dired-information)))
	 (length (length files))
	 (marked-files ())
	 (marked-dirs ()))
    (unless files (editor-error "Not in Dired buffer."))
    (do ((i 0 (1+ i)))
	((= i length) (values (nreverse marked-files) (nreverse marked-dirs)))
      (let* ((thing (svref files i))
	     (pathname (dired-file-pathname thing)))
	(when (and (dired-file-deleted-p thing) ; file marked for delete
		   (probe-file pathname)) 	; file still exists 
	  (if (directoryp pathname)
	      (push (cons pathname (file-write-date pathname)) marked-dirs)
	      (push (cons pathname (file-write-date pathname))
		    marked-files)))))))

;;; ARRAY-ELEMENT-FROM-MARK -- Internal Interface.
;;;
;;; This counts the lines between it and the beginning of the buffer.  The
;;; number is used to index vector as if each line mapped to an element
;;; starting with the zero'th element (lines are numbered starting at 1).
;;; This must use AREF since some modes use this with extendable vectors.
;;;
(defun array-element-from-mark (mark vector
				&optional (error-msg "Invalid line."))
  (when (blank-line-p (mark-line mark)) (editor-error error-msg))
  (aref vector
	 (1- (count-lines (region
			   (buffer-start-mark (line-buffer (mark-line mark)))
			   mark)))))

;;; DIRED-NAMIFY and DIRED-DIRECTORIFY are implementation dependent slime.
;;;
(defun dired-namify (pathname)
  (let* ((string (namestring pathname))
	 (last (1- (length string))))
    (if (char= (schar string last) #\/)
	(subseq string 0 last)
	string)))
;;;
;;; This is necessary to derive a canonical representation for directory
;;; names, so "Dired" can map various strings naming one directory to that
;;; one directory.
;;;
(defun dired-directorify (pathname)
  (let ((directory (ext:unix-namestring pathname)))
    (if (directoryp directory)
	directory
	(pathname (concatenate 'simple-string (namestring directory) "/")))))



;;;; View Mode.

(defmode "View" :major-p nil
  :setup-function 'setup-view-mode
  :cleanup-function 'cleanup-view-mode
  :precedence 5.0
  :documentation
  "View mode scrolls forwards and backwards in a file with the buffer read-only.
   Scrolling off the end optionally deletes the buffer.")

(defun setup-view-mode (buffer)
  (defhvar "View Return Function"
    "Function that gets called when quitting or returning from view mode."
    :value nil
    :buffer buffer)
  (setf (buffer-writable buffer) nil))
;;;
(defun cleanup-view-mode (buffer)
  (delete-variable 'view-return-function :buffer buffer)
  (setf (buffer-writable buffer) t))

(defcommand "View File" (p &optional pathname)
  "Reads a file in as if by \"Find File\", but read-only.  Commands exist
   for scrolling convenience."
  "Reads a file in as if by \"Find File\", but read-only.  Commands exist
   for scrolling convenience."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file 
		  :prompt "View File: " :must-exist t
		  :help "Name of existing file to read into its own buffer."
		  :default (buffer-default-pathname (current-buffer)))))
	 (buffer (make-buffer (format nil "View File ~A" (gensym)))))
    (visit-file-command nil pn buffer)
    (setf (buffer-minor-mode buffer "View") t)
    (change-to-buffer buffer)
    buffer))

(defcommand "View Return" (p)
  "Return to a parent buffer, if it exists."
  "Return to a parent buffer, if it exists."
  (declare (ignore p))
  (unless (call-view-return-fun)
    (editor-error "No View return method for this buffer.")))

(defcommand "View Quit" (p)
  "Delete a buffer in view mode."
  "Delete a buffer in view mode, invoking VIEW-RETURN-FUNCTION if it exists for
   this buffer."
  (declare (ignore p))
  (let* ((buf (current-buffer))
	 (funp (call-view-return-fun)))
    (delete-buffer-if-possible buf)
    (unless funp (editor-error "No View return method for this buffer."))))

;;; CALL-VIEW-RETURN-FUN returns nil if there is no current
;;; view-return-function.  If there is one, it calls it and returns t.
;;;
(defun call-view-return-fun ()
  (if (hemlock-bound-p 'view-return-function)
      (let ((fun (value view-return-function)))
	(cond (fun
	       (funcall fun)
	       t)))))


(defhvar "View Scroll Deleting Buffer"
  "When this is set, \"View Scroll Down\" deletes the buffer when the end
   of the file is visible."
  :value t)

(defcommand "View Scroll Down" (p)
  "Scroll the current window down through its buffer.
   If the end of the file is visible, then delete the buffer if \"View Scroll
   Deleting Buffer\" is set.  If the buffer is associated with a dired buffer,
   this returns there instead of to the previous buffer."
  "Scroll the current window down through its buffer.
   If the end of the file is visible, then delete the buffer if \"View Scroll
   Deleting Buffer\" is set.  If the buffer is associated with a dired buffer,
   this returns there instead of to the previous buffer."
  (if (and (not p)
	   (displayed-p (buffer-end-mark (current-buffer))
			(current-window))
	   (value view-scroll-deleting-buffer))
      (view-quit-command nil)
      (scroll-window-down-command p)))

(defcommand "View Edit File" (p)
  "Turn off \"View\" mode in this buffer."
  "Turn off \"View\" mode in this buffer."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (setf (buffer-minor-mode buf "View") nil)
    (warn-about-visit-file-buffers buf)))

(defcommand "View Help" (p)
  "Shows \"View\" mode help message."
  "Shows \"View\" mode help message."
  (declare (ignore p))
  (describe-mode-command nil "View"))
