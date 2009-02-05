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
;;; Hemlock command level support for processes.
;;;
;;; Written by Blaine Burks.
;;;

(in-package :hemlock)


(defun setup-process-buffer (buffer)
  (let ((mark (copy-mark (buffer-point buffer) :right-inserting)))
    (defhvar "Buffer Input Mark"
      "The buffer input mark for this buffer."
      :buffer buffer
      :value mark)
    (defhvar "Process Output Stream"
      "The process structure for this buffer."
      :buffer buffer
      :value (make-hemlock-output-stream mark :full))
    (defhvar "Interactive History"
      "A ring of the regions input to an interactive mode (Eval or Typescript)."
      :buffer buffer
      :value (make-ring (value interactive-history-length)))
    (defhvar "Interactive Pointer"
      "Pointer into \"Interactive History\"."
      :buffer buffer
      :value 0)
    (defhvar "Searching Interactive Pointer"
      "Pointer into \"Interactive History\"."
      :buffer buffer
      :value 0)
    (unless (buffer-modeline-field-p buffer :process-status)
      (setf (buffer-modeline-fields buffer)
	    (nconc (buffer-modeline-fields buffer)
		   (list (modeline-field :process-status)))))))

(defmode "Process" :major-p nil :setup-function #'setup-process-buffer)



;;;; Shell-filter streams.

;;; We use shell-filter-streams to capture text going from the shell process to
;;; a Hemlock output stream.  They pass character and misc operations through
;;; to the attached hemlock-output-stream.  The string output function scans
;;; the string for ^A_____^B, denoting a change of directory.
;;;
;;; The following aliases in a .cshrc file are required for using filename
;;; completion:
;;;    alias cd 'cd \!* ; echo ""`pwd`"/"'
;;;    alias popd 'popd \!* ; echo ""`pwd`"/"'
;;;    alias pushd 'pushd \!* ; echo ""`pwd`"/"'
;;;

(defstruct (shell-filter-stream
	    (:include sys:lisp-stream
		      (:out #'shell-filter-out)
		      (:sout #'shell-filter-string-out)
		      (:misc #'shell-filter-output-misc))
	    (:print-function print-shell-filter-stream)
	    (:constructor 
	     make-shell-filter-stream (buffer hemlock-stream)))
  ;; The buffer where output will be going
  buffer
  ;; The Hemlock stream to which output will be directed
  hemlock-stream)


;;; PRINT-SHELL-FILTER-STREAM  -- Internal
;;;
;;; Function for printing a shell-filter-stream.
;;;
(defun print-shell-filter-stream (s stream d)
  (declare (ignore d s))
  (write-string "#<Shell filter stream>" stream))


;;; SHELL-FILTER-OUT -- Internal
;;;
;;; This is the character-out handler for the shell-filter-stream.
;;; It writes the character it is given to the underlying
;;; hemlock-output-stream.
;;;
(defun shell-filter-out (stream character)
  (write-char character (shell-filter-stream-hemlock-stream stream)))


;;; SHELL-FILTER-OUTPUT-MISC -- Internal
;;;
;;; This will also simply pass the output request on the the
;;; attached hemlock-output-stream.
;;;
(defun shell-filter-output-misc (stream operation &optional arg1 arg2)
  (let ((hemlock-stream (shell-filter-stream-hemlock-stream stream)))
    (funcall (hi::hemlock-output-stream-misc hemlock-stream)
	     hemlock-stream operation arg1 arg2)))


;;; CATCH-CD-STRING -- Internal
;;;
;;; Scans String for the sequence ^A...^B.  Returns as multiple values
;;; the breaks in the string.  If the second start/end pair is nil, there
;;; was no cd sequence.
;;;
(defun catch-cd-string (string start end)
  (declare (simple-string string))
  (let ((cd-start (position (code-char 1) string :start start :end end)))
    (if cd-start
	(let ((cd-end (position (code-char 2) string :start cd-start :end end)))
	  (if cd-end
	      (values start cd-start cd-end end)
	      (values start end nil nil)))
	(values start end nil nil))))

;;; SHELL-FILTER-STRING-OUT -- Internal
;;;
;;; The string output function for shell-filter-stream's.
;;; Any string containing a ^A...^B is caught and assumed to be
;;; the path-name of the new current working directory.  This is
;;; removed from the orginal string and the result is passed along
;;; to the Hemlock stream.
;;;
(defun shell-filter-string-out (stream string start end)
  (declare (simple-string string))
  (let ((hemlock-stream (shell-filter-stream-hemlock-stream stream))
	(buffer (shell-filter-stream-buffer stream)))

    (multiple-value-bind (start1 end1 start2 end2)
			 (catch-cd-string string start end)
      (write-string string hemlock-stream :start start1 :end end1)
      (when start2
	(write-string string hemlock-stream :start (+ 2 start2) :end end2)
	(let ((cd-string (subseq string (1+ end1) start2)))
	  (setf (variable-value 'current-working-directory :buffer buffer)
		(pathname cd-string)))))))


;;; FILTER-TILDES -- Internal
;;;
;;; Since COMPLETE-FILE does not seem to deal with ~'s in the filename
;;; this function expands them to a full path name.
;;;
(defun filter-tildes (name)
  (declare (simple-string name))
  (if (char= (schar name 0) #\~)
      (concatenate 'simple-string
		   (if (or (= (length name) 1)
			   (char= (schar name 1) #\/))
		       (cdr (assoc :home *environment-list*))
		       "/usr/")
		 (subseq name 1))
      name))



;;;; Support for handling input before the prompt in process buffers.

(defun unwedge-process-buffer ()
  (buffer-end (current-point))
  (deliver-signal-to-process :SIGINT (value process))
  (editor-error "Aborted."))

(defhvar "Unwedge Interactive Input Fun"
  "Function to call when input is confirmed, but the point is not past the
   input mark."
  :value #'unwedge-process-buffer
  :mode "Process")

(defhvar "Unwedge Interactive Input String"
  "String to add to \"Point not past input mark.  \" explaining what will
   happen if the the user chooses to be unwedged."
  :value "Interrupt and throw to end of buffer?"
  :mode "Process")



;;;; Some Global Variables.

(defhvar "Current Shell"
  "The shell to which \"Select Shell\" goes."
  :value nil)

(defhvar "Ask about Old Shells"
  "When set (the default), Hemlock prompts for an existing shell buffer in
   preference to making a new one when there is no \"Current Shell\"."
  :value t)
  
(defhvar "Kill Process Confirm"
  "When set, Hemlock prompts for confirmation before killing a buffer's process."
  :value t)

(defhvar "Shell Utility"
  "The \"Shell\" command uses this as the default command line."
  :value "/bin/csh")

(defhvar "Shell Utility Switches"
  "This is a string containing the default command line arguments to the
   utility in \"Shell Utility\".  This is a string since the utility is
   typically \"/bin/csh\", and this string can contain I/O redirection and
   other shell directives."
  :value "")



;;;; The Shell, New Shell, and Set Current Shell Commands.

(defvar *shell-names* (make-string-table)
  "A string-table of the string-name of all process buffers and corresponding
   buffer structures.")

(defcommand "Set Current Shell" (p)
  "Sets the value of \"Current Shell\", which the \"Shell\" command uses."
  "Sets the value of \"Current Shell\", which the \"Shell\" command uses."
  (declare (ignore p))
  (set-current-shell))

;;; SET-CURRENT-SHELL -- Internal.
;;;
;;; This prompts for a known shell buffer to which it sets "Current Shell".
;;; It signals an error if there are none.
;;;
(defun set-current-shell ()
  (let ((old-buffer (value current-shell))
	(first-old-shell (do-strings (var val *shell-names* nil)
			   (declare (ignore val))
			   (return var))))
    (when (and (not old-buffer) (not first-old-shell))
      (editor-error "Nothing to set current shell to."))
    (let ((default-shell (if old-buffer
			     (buffer-name old-buffer)
			     first-old-shell)))
      (multiple-value-bind
	  (new-buffer-name new-buffer) 
	  (prompt-for-keyword (list *shell-names*)
			      :must-exist t
			      :default default-shell
			      :default-string default-shell
			      :prompt "Existing Shell: "
			      :help "Enter the name of an existing shell.")
	(declare (ignore new-buffer-name))
	(setf (value current-shell) new-buffer)))))

(defcommand "Shell" (p)
  "This spawns a shell in a buffer.  If there already is a \"Current Shell\",
   this goes to that buffer.  If there is no \"Current Shell\", there are
   shell buffers, and \"Ask about Old Shells\" is set, this prompts for one
   of them, setting \"Current Shell\" to that shell.  Supplying an argument
   forces the creation of a new shell buffer."
  "This spawns a shell in a buffer.  If there already is a \"Current Shell\",
   this goes to that buffer.  If there is no \"Current Shell\", there are
   shell buffers, and \"Ask about Old Shells\" is set, this prompts for one
   of them, setting \"Current Shell\" to that shell.  Supplying an argument
   forces the creation of a new shell buffer."
  (let ((shell (value current-shell))
	(no-shells-p (do-strings (var val *shell-names* t)
		       (declare (ignore var val))
		       (return nil))))
    (cond (p (make-new-shell nil no-shells-p))
	  (shell (change-to-buffer shell))
	  ((and (value ask-about-old-shells) (not no-shells-p))
	   (set-current-shell)
	   (change-to-buffer (value current-shell)))
	  (t (make-new-shell nil)))))

(defcommand "Shell Command Line in Buffer" (p)
  "Prompts the user for a process and a buffer in which to run the process."
  "Prompts the user for a process and a buffer in which to run the process."
  (declare (ignore p))
  (make-new-shell t))

;;; MAKE-NEW-SHELL -- Internal.
;;;
;;; This makes new shells for us dealing with prompting for various things and
;;; setting "Current Shell" according to user documentation.
;;;
(defun make-new-shell (prompt-for-command-p &optional (set-current-shell-p t)
		       (command-line (get-command-line) clp))
  (let* ((command (or (and clp command-line)
		      (if prompt-for-command-p
			  (prompt-for-string
			   :default command-line :trim t
			   :prompt "Command to execute: "
			   :help "Shell command line to execute.")
			  command-line)))
	 (buffer-name (if prompt-for-command-p
			  (prompt-for-string
			   :default
			   (concatenate 'simple-string command " process")
			   :trim t
			   :prompt `("Buffer in which to execute ~A? "
				     ,command)
			   :help "Where output from this process will appear.")
			  (new-shell-name)))
	 (temp (make-buffer
		  buffer-name
		  :modes '("Fundamental" "Process")
		  :delete-hook
		  (list #'(lambda (buffer)
			    (when (eq (value current-shell) buffer)
			      (setf (value current-shell) nil))
			    (delete-string (buffer-name buffer) *shell-names*)
			    (kill-process (variable-value 'process
							  :buffer buffer))))))
	 (buffer (or temp (getstring buffer-name *buffer-names*)))
	 (stream (variable-value 'process-output-stream :buffer buffer))
	 (output-stream
	  ;; If we re-used an old shell buffer, this isn't necessary.
	  (if (hemlock-output-stream-p stream)
	      (setf (variable-value 'process-output-stream :buffer buffer)
		    (make-shell-filter-stream buffer stream))
	      stream)))
    (buffer-end (buffer-point buffer))
    (defhvar "Process"
      "The process for Shell and Process buffers."
      :buffer buffer
      :value (ext::run-program "/bin/sh" (list "-c" command)
			       :wait nil
			       :pty output-stream
			       :env (frob-environment-list
				     (car (buffer-windows buffer)))
			       :status-hook #'(lambda (process)
						(declare (ignore process))
						(update-process-buffer buffer))
			       :input t :output t))
    (defhvar "Current Working Directory"
      "The pathname of the current working directory for this buffer."
      :buffer buffer
      :value (default-directory))
    (setf (getstring buffer-name *shell-names*) buffer)
    (update-process-buffer buffer)
    (when (and (not (value current-shell)) set-current-shell-p)
      (setf (value current-shell) buffer))
    (change-to-buffer buffer)))

;;; GET-COMMAND-LINE -- Internal.
;;;
;;; This just conses up a string to feed to the shell.
;;;
(defun get-command-line ()
  (concatenate 'simple-string (value shell-utility) " "
	       (value shell-utility-switches)))

;;; FROB-ENVIRONMENT-LIST -- Internal.
;;;
;;; This sets some environment variables so the shell will be in the proper
;;; state when it comes up.
;;;
(defun frob-environment-list (window)
  (list* (cons :termcap  (concatenate 'simple-string
				      "emacs:co#"
				      (if window
					  (lisp::quick-integer-to-string
					   (window-width window))
					  "")
				      ":tc=unkown:"))
	 (cons :emacs "t") (cons :term "emacs")
	 (remove-if #'(lambda (keyword)
			(member keyword '(:termcap :emacs :term)
				:test #'(lambda (cons keyword)
					  (eql (car cons) keyword))))
		    ext:*environment-list*)))

;;; NEW-SHELL-NAME -- Internal.
;;;
;;; This returns a unique buffer name for a shell by incrementing the value of
;;; *process-number* until "Process <*process-number*> is not already the name
;;; of a buffer.  Perhaps this is being overly cautious, but I've seen some
;;; really stupid users.
;;;
(defvar *process-number* 0)
;;;
(defun new-shell-name ()
  (loop
    (let ((buffer-name (format nil "Shell ~D" (incf *process-number*))))
      (unless (getstring buffer-name *buffer-names*) (return buffer-name)))))


;;;; Modeline support.

(defun modeline-process-status (buffer window)
  (declare (ignore window))
  (when (hemlock-bound-p 'process :buffer buffer)
    (let ((process (variable-value 'process :buffer buffer)))
      (ecase (ext:process-status process)
	(:running "running")
	(:stopped "stopped")
	(:signaled "killed by signal ~D" (unix:unix-signal-name
					  (ext:process-exit-code process)))
	(:exited (format nil "exited with status ~D"
			 (ext:process-exit-code process)))))))
			 

(make-modeline-field :name :process-status
		     :function #'modeline-process-status)

(defun update-process-buffer (buffer)
  (when (buffer-modeline-field-p buffer :process-status)
    (dolist (window (buffer-windows buffer))
      (update-modeline-field buffer window :process-status)))
  (let ((process (variable-value 'process :buffer buffer)))
    (unless (ext:process-alive-p process)
      (ext:process-close process)
      (when (eq (value current-shell) buffer)
	(setf (value current-shell) nil)))))


;;;; Supporting Commands.

(defcommand "Confirm Process Input" (p)
  "Evaluate Process Mode input between the point and last prompt."
  "Evaluate Process Mode input between the point and last prompt."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (let* ((process (value process))
	 (stream (ext:process-pty process)))
    (case (ext:process-status process)
      (:running)
      (:stopped (editor-error "The process has been stopped."))
      (t (editor-error "The process is dead.")))
    (let ((input-region (get-interactive-input)))
      (write-line (region-to-string input-region) stream)
      (force-output (ext:process-pty process))
      (insert-character (current-point) #\newline)
      ;; Move "Buffer Input Mark" to end of buffer.
      (move-mark (region-start input-region) (region-end input-region)))))

(defcommand "Shell Complete Filename" (p)
  "Attempts to complete the filename immediately preceding the point.
   It will beep if the result of completion is not unique."
  "Attempts to complete the filename immediately preceding the point.
   It will beep if the result of completion is not unique."
  (declare (ignore p))
  (unless (hemlock-bound-p 'current-working-directory)
    (editor-error "Shell filename completion only works in shells."))
  (let ((point (current-point)))
    (with-mark ((start point))
      (pre-command-parse-check start)
      (unless (form-offset start -1) (editor-error "Can't grab filename."))
      (when (member (next-character start) '(#\" #\' #\< #\>))
	(mark-after start))
      (let* ((name-region (region start point))
	     (fragment (filter-tildes (region-to-string name-region)))
	     (dir (default-directory))
	     (shell-dir (value current-working-directory)))
	(multiple-value-bind (filename unique)
			     (unwind-protect
				 (progn
				   (setf (default-directory) shell-dir)
				   (complete-file fragment :defaults shell-dir))
			       (setf (default-directory) dir))
	  (cond (filename
		 (delete-region name-region)
		 (insert-string point (namestring filename))
		 (when (not unique)
		   (editor-error)))
		(t (editor-error "No such file exists."))))))))

(defcommand "Kill Main Process" (p)
  "Kills the process in the current buffer."
  "Kills the process in the current buffer."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (when (or (not (value kill-process-confirm))
	    (prompt-for-y-or-n :default nil
			       :prompt "Really blow away shell? "
			       :default nil
			       :default-string "no"))
    (kill-process (value process))))

(defcommand "Stop Main Process" (p)
  "Stops the process in the current buffer.  With an argument use :SIGSTOP
   instead of :SIGTSTP."
  "Stops the process in the current buffer.  With an argument use :SIGSTOP
  instead of :SIGTSTP."
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (deliver-signal-to-process (if p :SIGSTOP :SIGTSTP) (value process)))

(defcommand "Continue Main Process" (p)
  "Continues the process in the current buffer."
  "Continues the process in the current buffer."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (deliver-signal-to-process :SIGCONT (value process)))
  
(defun kill-process (process)
  "Self-explanatory."
  (deliver-signal-to-process :SIGKILL process))

(defun deliver-signal-to-process (signal process)
  "Delivers a signal to a process."
  (ext:process-kill process signal :process-group))

(defcommand "Send EOF to Process" (p)
  "Sends a Ctrl-D to the process in the current buffer."
  "Sends a Ctrl-D to the process in the current buffer."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (let ((stream (ext:process-pty (value process))))
    (write-char (code-char 4) stream)
    (force-output stream)))

(defcommand "Interrupt Buffer Subprocess" (p)
  "Stop the subprocess currently executing in this shell."
  "Stop the subprocess currently executing in this shell."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (buffer-end (current-point))
  (buffer-end (value buffer-input-mark))
  (deliver-signal-to-subprocess :SIGINT (value process)))

(defcommand "Kill Buffer Subprocess" (p)
  "Kill the subprocess currently executing in this shell."
  "Kill the subprocess currently executing in this shell."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))  
  (deliver-signal-to-subprocess :SIGKILL (value process)))

(defcommand "Quit Buffer Subprocess" (p)
  "Quit the subprocess currently executing int his shell."
  "Quit the subprocess currently executing int his shell."
  (declare (ignore p))
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))
  (deliver-signal-to-subprocess :SIGQUIT (value process)))

(defcommand "Stop Buffer Subprocess" (p)
  "Stop the subprocess currently executing in this shell."
  "Stop the subprocess currently executing in this shell."
  (unless (hemlock-bound-p 'process :buffer (current-buffer))
    (editor-error "Not in a process buffer."))  
  (deliver-signal-to-subprocess (if p :SIGSTOP :SIGTSTP) (value process)))

(defun deliver-signal-to-subprocess (signal process)
  "Delivers a signal to a subprocess of a shell."
  (ext:process-kill process signal :pty-process-group))
