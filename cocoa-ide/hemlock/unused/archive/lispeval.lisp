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
;;; This file contains code for sending requests to eval servers and the
;;; commands based on that code.
;;;
;;; Written by William Lott and Rob MacLachlan.
;;;

(in-package :hemlock)


;;; The note structure holds everything we need to know about an
;;; operation.  Not all operations use all the available fields.
;;;
(defstruct (note (:print-function %print-note))
  (state :unsent)	      ; :unsent, :pending, :running, :aborted or :dead.
  server		      ; Server-Info for the server this op is on.
  context		      ; Short string describing what this op is doing.
  kind			      ; Either :eval, :compile, or :compile-file
  buffer		      ; Buffer source came from.
  region		      ; Region of request
  package		      ; Package or NIL if none
  text			      ; string containing request
  input-file		      ; File to compile or where stuff was found
  net-input-file	      ; Net version of above.
  output-file		      ; Temporary output file for compiler fasl code.
  net-output-file	      ; Net version of above
  output-date		      ; Temp-file is created before calling compiler,
			      ;  and this is its write date.
  lap-file		      ; The lap file for compiles
  error-file		      ; The file to dump errors into
  load			      ; Load compiled file or not?
  (errors 0)		      ; Count of compiler errors.
  (warnings 0)		      ; Count of compiler warnings.
  (notes 0))		      ; Count of compiler notes.
;;;
(defun %print-note (note stream d)
  (declare (ignore d))
  (format stream "#<Eval-Server-Note for ~A [~A]>"
	  (note-context note)
	  (note-state note)))



;;;; Note support routines.

;;; QUEUE-NOTE -- Internal.
;;;
;;; This queues note for server.  SERVER-INFO-NOTES keeps notes in stack order,
;;; not queue order.  We also link the note to the server and try to send it
;;; to the server.  If we didn't send this note, we tell the user the server
;;; is busy and that we're queuing his note to be sent later.
;;;
(defun queue-note (note server)
  (push note (server-info-notes server))
  (setf (note-server note) server)
  (maybe-send-next-note server)
  (when (eq (note-state note) :unsent)
    (message "Server ~A busy, ~A queued."
	     (server-info-name server)
	     (note-context note))))

;;; MAYBE-SEND-NEXT-NOTE -- Internal.
;;;
;;; Loop over all notes in server.  If we see any :pending or :running, then
;;; punt since we can't send one.  Otherwise, by the end of the list, we may
;;; have found an :unsent one, and if we did, next will be the last :unsent
;;; note.  Remember, SERVER-INFO-NOTES is kept in stack order not queue order.
;;;
(defun maybe-send-next-note (server)
  (let ((busy nil)
	(next nil))
    (dolist (note (server-info-notes server))
      (ecase (note-state note)
	((:pending :running)
	 (setf busy t)
	 (return))
	(:unsent
	 (setf next note))
	(:aborted :dead)))
    (when (and (not busy) next)
      (send-note next))))

(defun send-note (note)
  (let* ((remote (hemlock.wire:make-remote-object note))
	 (server (note-server note))
	 (ts (server-info-slave-info server))
	 (bg (server-info-background-info server))
	 (wire (server-info-wire server)))
    (setf (note-state note) :pending)
    (message "Sending ~A." (note-context note))
    (case (note-kind note)
      (:eval
       (hemlock.wire:remote wire
	 (server-eval-text remote
			   (note-package note)
			   (note-text note)
			   (and ts (ts-data-stream ts)))))
      (:compile
       (hemlock.wire:remote wire
	 (server-compile-text remote
			      (note-package note)
			      (note-text note)
			      (note-input-file note)
			      (and ts (ts-data-stream ts))
			      (and bg (ts-data-stream bg)))))
      (:compile-file
       (macrolet ((frob (x)
		    `(if (pathnamep ,x)
		       (namestring ,x)
		       ,x)))
	 (hemlock.wire:remote wire
	   (server-compile-file remote
				(note-package note)
				(frob (or (note-net-input-file note)
					  (note-input-file note)))
				(frob (or (note-net-output-file note)
					  (note-output-file note)))
				(frob (note-error-file note))
				(frob (note-lap-file note))
				(note-load note)
				(and ts (ts-data-stream ts))
				(and bg (ts-data-stream bg))))))
      (t
       (error "Unknown note kind ~S" (note-kind note))))
    (hemlock.wire:wire-force-output wire)))


;;;; Server Callbacks.

(defun operation-started (note)
  (let ((note (hemlock.wire:remote-object-value note)))
    (setf (note-state note) :running)
    (message "The ~A started." (note-context note)))
  (values))

(defun eval-form-error (message)
  (editor-error message))

(defun lisp-error (note start end msg)
  (declare (ignore start end))
  (let ((note (hemlock.wire:remote-object-value note)))
    (loud-message "During ~A: ~A"
		  (note-context note)
		  msg))
  (values))

(defun compiler-error (note start end function severity)
  (let* ((note (hemlock.wire:remote-object-value note))
	 (server (note-server note))
	 (line (mark-line
		(buffer-end-mark
		 (server-info-background-buffer server))))
	 (message (format nil "~:(~A~) ~@[in ~A ~]during ~A."
			  severity
			  function
			  (note-context note)))
	 (error (make-error-info :buffer (note-buffer note)
				 :message message
				 :line line)))
    (message "~A" message)
    (case severity
      (:error (incf (note-errors note)))
      (:warning (incf (note-warnings note)))
      (:note (incf (note-notes note))))
    (let ((region (case (note-kind note)
		    (:compile
		     (note-region note))
		    (:compile-file
		     (let ((buff (note-buffer note)))
		       (and buff (buffer-region buff))))
		    (t
		     (error "Compiler error in ~S?" note)))))
      (when region
	(let* ((region-end (region-end region))
	       (m1 (copy-mark (region-start region) :left-inserting))
	       (m2 (copy-mark m1 :left-inserting)))
	  (when start
	    (character-offset m1 start)
	    (when (mark> m1 region-end)
	      (move-mark m1 region-end)))
	  (unless (and end (character-offset m2 end))
	    (move-mark m2 region-end))
	  
	  (setf (error-info-region error)
		(region m1 m2)))))

    (vector-push-extend error (server-info-errors server)))

  (values))

(defun eval-text-result (note start end values)
  (declare (ignore note start end))
  (message "=> ~{~#[~;~A~:;~A, ~]~}" values)
  (values))

(defun operation-completed (note abortp)
  (let* ((note (hemlock.wire:remote-object-value note))
	 (server (note-server note))
	 (file (note-output-file note)))
    (hemlock.wire:forget-remote-translation note)
    (setf (note-state note) :dead)
    (setf (server-info-notes server)
	  (delete note (server-info-notes server)
		  :test #'eq))
    (setf (note-server note) nil)

    (if abortp
	(loud-message "The ~A aborted." (note-context note))
	(let ((errors (note-errors note))
	      (warnings (note-warnings note))
	      (notes (note-notes note)))
	  (message "The ~A complete.~
		    ~@[ ~D error~:P~]~@[ ~D warning~:P~]~@[ ~D note~:P~]"
		   (note-context note)
		   (and (plusp errors) errors)
		   (and (plusp warnings) warnings)
		   (and (plusp notes) notes))))

    (let ((region (note-region note)))
      (when (regionp region)
	(delete-mark (region-start region))
	(delete-mark (region-end region))
	(setf (note-region note) nil)))

    (when (and (eq (note-kind note)
		   :compile-file)
	       (not (eq file t))
	       file)
      (if (> (file-write-date file)
	     (note-output-date note))
	  (let ((new-name (make-pathname :type "fasl"
					 :defaults (note-input-file note))))
	    (rename-file file new-name)
	    #+NILGB
            (unix:unix-chmod (namestring new-name) #o644))
	  (delete-file file)))
    (maybe-send-next-note server))
  (values))


;;;; Stuff to send noise to the server.

;;; EVAL-FORM-IN-SERVER -- Public.
;;;
(defun eval-form-in-server (server-info form
			    &optional (package (value current-package)))
  "This evals form, a simple-string, in the server for server-info.  Package
   is the name of the package in which the server reads form, and it defaults
   to the value of \"Current Package\".  If package is nil, then the slave uses
   the value of *package*.  If server is busy with other requests, this signals
   an editor-error to prevent commands using this from hanging.  If the server
   dies while evaluating form, then this signals an editor-error.  This returns
   a list of strings which are the printed representation of all the values
   returned by form in the server."
  (declare (simple-string form))
  (when (server-info-notes server-info)
    (editor-error "Server ~S is currently busy.  See \"List Operations\"."
		  (server-info-name server-info)))
  (multiple-value-bind (values error)
		       (hemlock.wire:remote-value (server-info-wire server-info)
			 (server-eval-form package form))
    (when error
      (editor-error "The server died before finishing"))
    values))

;;; EVAL-FORM-IN-SERVER-1 -- Public.
;;;
;;; We use VALUES to squelch the second value of READ-FROM-STRING.
;;;
(defun eval-form-in-server-1 (server-info form
			      &optional (package (value current-package)))
  "This calls EVAL-FORM-IN-SERVER and returns the result of READ'ing from
   the first string EVAL-FORM-IN-SERVER returns."
  (values (read-from-string
	   (car (eval-form-in-server server-info form package)))))

(defun string-eval (string
		    &key
		    (server (get-current-eval-server))
		    (package (value current-package))
		    (context (format nil
				     "evaluation of ~S"
				     string)))
  "Queues the evaluation of string on an eval server.  String is a simple
   string.  If package is not supplied, the string is eval'ed in the slave's
   current package."
  (declare (simple-string string))
  (queue-note (make-note :kind :eval
			 :context context
			 :package package
			 :text string)
	      server)
  (values))

(defun region-eval (region
		    &key
		    (server (get-current-eval-server))
		    (package (value current-package))
		    (context (region-context region "evaluation")))
  "Queues the evaluation of a region of text on an eval server.  If package
   is not supplied, the string is eval'ed in the slave's current package."
  (let ((region (region (copy-mark (region-start region) :left-inserting)
			(copy-mark (region-end region) :left-inserting))))
    (queue-note (make-note :kind :eval
			   :context context
			   :region region
			   :package package
			   :text (region-to-string region))
		server))
  (values))

(defun region-compile (region
		       &key
		       (server (get-current-eval-server))
		       (package (value current-package)))
  "Queues a compilation on an eval server.  If package is not supplied, the
   string is eval'ed in the slave's current package."
  (let* ((region (region (copy-mark (region-start region) :left-inserting)
			 (copy-mark (region-end region) :left-inserting)))
	 (buf (line-buffer (mark-line (region-start region))))
	 (pn (and buf (buffer-pathname buf)))
	 (defined-from (if pn (namestring pn) "unknown")))
    (queue-note (make-note :kind :compile
			   :context (region-context region "compilation")
			   :buffer (and region
					(region-start region)
					(mark-line (region-start region))
					(line-buffer (mark-line
						      (region-start region))))
			   :region region
			   :package package
			   :text (region-to-string region)
			   :input-file defined-from)
		server))
  (values))



;;;; File compiling noise.

(defhvar "Remote Compile File"
  "When set (the default), this causes slave file compilations to assume the
   compilation is occurring on a remote machine.  This means the source file
   must be world readable.  Unsetting this, causes no file accesses to go
   through the super root."
  :value nil)

;;; FILE-COMPILE compiles files in a client Lisp.  Because of Unix file
;;; protection, one cannot write files over the net unless they are publicly
;;; writeable.  To get around this, we create a temporary file that is
;;; publicly writeable for compiler output.  This file is renamed to an
;;; ordinary output name if the compiler wrote anything to it, or deleted
;;; otherwise.  No temporary file is created when output-file is not t.
;;;

(defun file-compile (file
		     &key
		     buffer
		     (output-file t)
		     error-file
		     lap-file
		     load
		     (server (get-current-compile-server))
		     (package (value current-package)))
  "Compiles file in a client Lisp.  When output-file is t, a temporary
   output file is used that is publicly writeable in case the client is on
   another machine.  This file is renamed or deleted after compilation.
   Setting \"Remote Compile File\" to nil, inhibits this.  If package is not
   supplied, the string is eval'ed in the slave's current package."

  (let* ((file (truename file)) ; in case of search-list in pathname.
	 (namestring (namestring file))
	 (note (make-note
		:kind :compile-file
		:context (format nil "compilation of ~A" namestring)
		:buffer buffer
		:region nil
		:package package
		:input-file file
		:output-file output-file
		:error-file error-file
		:lap-file lap-file
		:load load)))

    (when (and (value remote-compile-file)
	       (eq output-file t))
      (multiple-value-bind (net-infile ofile net-ofile date)
			   (file-compile-temp-file file)
	(setf (note-net-input-file note) net-infile)
	(setf (note-output-file note) ofile)
	(setf (note-net-output-file note) net-ofile)
	(setf (note-output-date note) date)))

    (clear-server-errors server
			 #'(lambda (error)
			     (eq (error-info-buffer error)
				 buffer)))
    (queue-note note server)))

;;; FILE-COMPILE-TEMP-FILE creates a a temporary file that is publicly
;;; writable in the directory file is in and with a .fasl type.  Four values
;;; are returned -- a pathname suitable for referencing file remotely, the
;;; pathname of the temporary file created, a pathname suitable for referencing
;;; the temporary file remotely, and the write date of the temporary file.
;;; 

#+NILGB
(defun file-compile-temp-file (file)
  (let ((ofile (loop (let* ((sym (gensym))
			    (f (merge-pathnames
				(format nil "compile-file-~A.fasl" sym)
				file)))
		       (unless (probe-file f) (return f))))))
    (multiple-value-bind (fd err)
			 (unix:unix-open (namestring ofile)
					 unix:o_creat #o666)
      (unless fd
	(editor-error "Couldn't create compiler temporary output file:~%~
	~A" (unix:get-unix-error-msg err)))
      (unix:unix-fchmod fd #o666)
      (unix:unix-close fd))
    (let ((net-ofile (pathname-for-remote-access ofile)))
      (values (make-pathname :directory (pathname-directory net-ofile)
			     :defaults file)
	      ofile
	      net-ofile
	      (file-write-date ofile)))))

(defun pathname-for-remote-access (file)
  (let* ((machine (machine-instance))
	 (usable-name (nstring-downcase
		       (the simple-string
			    (subseq machine 0 (position #\. machine))))))
    (declare (simple-string machine usable-name))
    (make-pathname :directory (concatenate 'simple-string
					   "/../"
					   usable-name
					   (directory-namestring file))
		   :defaults file)))

;;; REGION-CONTEXT -- internal
;;;
;;;    Return a string which describes the code in a region.  Thing is the
;;; thing being done to the region.  "compilation" or "evaluation"...

(defun region-context (region thing)
  (declare (simple-string thing))
  (pre-command-parse-check (region-start region))
  (let ((start (region-start region)))
    (with-mark ((m1 start))
      (unless (start-defun-p m1)
	(top-level-offset m1 1))
      (with-mark ((m2 m1))
	(mark-after m2)
	(form-offset m2 2)
	(format nil
		"~A of ~S"
		thing
		(if (eq (mark-line m1) (mark-line m2))
		  (region-to-string (region m1 m2))
		  (concatenate 'simple-string
			       (line-string (mark-line m1))
			       "...")))))))


;;;; Commands (Gosh, wow gee!)

(defcommand "Editor Server Name" (p)
  "Echos the editor server's name which can be supplied with the -slave switch
   to connect to a designated editor."
  "Echos the editor server's name which can be supplied with the -slave switch
   to connect to a designated editor."
  (declare (ignore p))
  (if *editor-name*
    (message "This editor is named ~S." *editor-name*)
    (message "This editor is not currently named.")))

(defcommand "Set Buffer Package" (p)
  "Set the package to be used by Lisp evaluation and compilation commands
   while in this buffer.  When in a slave's interactive buffers, do NOT
   set the editor's package variable, but changed the slave's *package*."
  "Prompt for a package to make into a buffer-local variable current-package."
  (declare (ignore p))
  (let* ((name (string (prompt-for-expression
			:prompt "Package name: "
			:help "Name of package to associate with this buffer.")))
	 (buffer (current-buffer))
	 (info (value current-eval-server)))
    (cond ((and info
		(or (eq (server-info-slave-buffer info) buffer)
		    (eq (server-info-background-buffer info) buffer)))
	   (hemlock.wire:remote (server-info-wire info)
	     (server-set-package name))
	   (hemlock.wire:wire-force-output (server-info-wire info)))
	  ((eq buffer *selected-eval-buffer*)
	   (setf *package* (maybe-make-package name)))
	  (t
	   (defhvar "Current Package"
	     "The package used for evaluation of Lisp in this buffer."
	     :buffer buffer  :value name)))
    (when (buffer-modeline-field-p buffer :package)
      (dolist (w (buffer-windows buffer))
	(update-modeline-field buffer w :package)))))

(defcommand "Current Compile Server" (p)
  "Echos the current compile server's name.  With prefix argument,
   shows global one.  Does not signal an error or ask about creating a slave."
  "Echos the current compile server's name.  With prefix argument,
  shows global one."
  (let ((info (if p
		  (variable-value 'current-compile-server :global)
		  (value current-compile-server))))
    (if info
	(message "~A" (server-info-name info))
	(message "No ~:[current~;global~] compile server." p))))

(defcommand "Set Compile Server" (p)
  "Specifies the name of the server used globally for file compilation requests."
  "Call select-current-compile-server."
  (declare (ignore p))
  (hlet ((ask-about-old-servers t))
    (setf (variable-value 'current-compile-server :global)
	  (maybe-create-server))))

(defcommand "Set Buffer Compile Server" (p)
  "Specifies the name of the server used for file compilation requests in
   the current buffer."
  "Call select-current-compile-server after making a buffer local variable."
  (declare (ignore p))
  (hlet ((ask-about-old-servers t))
    (defhvar "Current Compile Server"
      "The Server-Info object for the server currently used for compilation requests."
      :buffer (current-buffer)
      :value (maybe-create-server))))

(defcommand "Current Eval Server" (p)
  "Echos the current eval server's name.  With prefix argument, shows
   global one.  Does not signal an error or ask about creating a slave."
  "Echos the current eval server's name.  With prefix argument, shows
   global one.  Does not signal an error or ask about creating a slave."
  (let ((info (if p
		  (variable-value 'current-eval-server :global)
		  (value current-eval-server))))
    (if info
	(message "~A" (server-info-name info))
	(message "No ~:[current~;global~] eval server." p))))

(defcommand "Set Eval Server" (p)
  "Specifies the name of the server used globally for evaluation and
   compilation requests."
  "Call select-current-server."
  (declare (ignore p))
  (hlet ((ask-about-old-servers t))
    (setf (variable-value 'current-eval-server :global)
	  (maybe-create-server))))

(defcommand "Set Buffer Eval Server" (p)
  "Specifies the name of the server used for evaluation and compilation
   requests in the current buffer."
  "Call select-current-server after making a buffer local variable."
  (declare (ignore p))
  (hlet ((ask-about-old-servers t))
    (defhvar "Current Eval Server"
      "The Server-Info for the eval server used in this buffer."
      :buffer (current-buffer)
      :value (maybe-create-server))))

(defcommand "Evaluate Defun" (p)
  "Evaluates the current or next top-level form.
   If the current region is active, then evaluate it."
  "Evaluates the current or next top-level form."
  (declare (ignore p))
  (if (region-active-p)
      (evaluate-region-command nil)
      (region-eval (defun-region (current-point)))))

(defcommand "Re-evaluate Defvar" (p)
  "Evaluate the current or next top-level form if it is a DEFVAR.  Treat the
   form as if the variable is not bound."
  "Evaluate the current or next top-level form if it is a DEFVAR.  Treat the
   form as if the variable is not bound."
  (declare (ignore p))
  (let* ((form (defun-region (current-point)))
	 (start (region-start form)))
    (with-mark ((var-start start)
		(var-end start))
      (mark-after var-start)
      (form-offset var-start 1)
      (form-offset (move-mark var-end var-start) 1)
      (let ((exp (concatenate 'simple-string
			      "(makunbound '"
			      (region-to-string (region var-start var-end))
			      ")")))
	(eval-form-in-server (get-current-eval-server) exp)))
    (region-eval form)))

;;; We use Prin1-To-String in the client so that the expansion gets pretty
;;; printed.  Since the expansion can contain unreadable stuff, we can't expect
;;; to be able to read that string back in the editor.  We shove the region
;;; at the client Lisp as a string, so it can read from the string with the
;;; right package environment.
;;;

(defcommand "Macroexpand Expression" (p)
  "Show the macroexpansion of the current expression in the null environment.
   With an argument, use MACROEXPAND instead of MACROEXPAND-1."
  "Show the macroexpansion of the current expression in the null environment.
   With an argument, use MACROEXPAND instead of MACROEXPAND-1."
  (let ((point (current-point)))
    (with-mark ((start point))
      (pre-command-parse-check start)
      (with-mark ((end start))
        (unless (form-offset end 1) (editor-error))
	(with-pop-up-display (s)
	  (write-string
	   (eval-form-in-server-1
	    (get-current-eval-server)
	    (format nil "(prin1-to-string (~S (read-from-string ~S)))"
		    (if p 'macroexpand 'macroexpand-1)
		    (region-to-string (region start end))))
	   s))))))

(defcommand "Evaluate Expression" (p)
  "Prompt for an expression to evaluate."
  "Prompt for an expression to evaluate."
  (declare (ignore p))
  (let ((exp (prompt-for-string
	      :prompt "Eval: "
	      :help "Expression to evaluate.")))
    (message "=> ~{~#[~;~A~:;~A, ~]~}"
	     (eval-form-in-server (get-current-eval-server) exp))))

(defcommand "Compile Defun" (p)
  "Compiles the current or next top-level form.
   First the form is evaluated, then the result of this evaluation
   is passed to compile.  If the current region is active, compile
   the region."
  "Evaluates the current or next top-level form."
  (declare (ignore p))
  (if (region-active-p)
      (compile-region-command nil)
      (region-compile (defun-region (current-point)))))

(defcommand "Compile Region" (p)
  "Compiles lisp forms between the point and the mark."
  "Compiles lisp forms between the point and the mark."
  (declare (ignore p))
  (region-compile (current-region)))

(defcommand "Evaluate Region" (p)
  "Evaluates lisp forms between the point and the mark."
  "Evaluates lisp forms between the point and the mark."
  (declare (ignore p))
  (region-eval (current-region)))
           
(defcommand "Evaluate Buffer" (p)
  "Evaluates the text in the current buffer."
  "Evaluates the text in the current buffer redirecting *Standard-Output* to
  the echo area.  The prefix argument is ignored."
  (declare (ignore p))
  (let ((b (current-buffer)))
    (region-eval (buffer-region b)
		 :context (format nil
				  "evaluation of buffer ``~A''"
				  (buffer-name b)))))

(defcommand "Load File" (p)
  "Prompt for a file to load into the current eval server."
  "Prompt for a file to load into the current eval server."
  (declare (ignore p))
  (let ((name (truename (prompt-for-file
			 :default
			 (or (value load-pathname-defaults)
			     (buffer-default-pathname (current-buffer)))
			 :prompt "File to load: "
			 :help "The name of the file to load"))))
    (setv load-pathname-defaults name)
    (string-eval (format nil "(load ~S)"
			 (namestring
			  (if (value remote-compile-file)
			      (pathname-for-remote-access name)
			      name))))))

(defcommand "Compile File" (p)
  "Prompts for file to compile.  Does not compare source and binary write
   dates.  Does not check any buffer for that file for whether the buffer
   needs to be saved."
  "Prompts for file to compile."
  (declare (ignore p))
  (let ((pn (prompt-for-file :default
			     (buffer-default-pathname (current-buffer))
			     :prompt "File to compile: ")))
    (file-compile pn)))

(defhvar "Compile Buffer File Confirm"
  "When set, \"Compile Buffer File\" prompts before doing anything."
  :value t)

(defcommand "Compile Buffer File" (p)
  "Compile the file in the current buffer if its associated binary file
   (of type .fasl) is older than the source or doesn't exist.  When the
   binary file is up to date, the user is asked if the source should be
   compiled anyway.  When the prefix argument is supplied, compile the
   file without checking the binary file.  When \"Compile Buffer File
   Confirm\" is set, this command will ask for confirmation when it
   otherwise would not."
  "Compile the file in the current buffer if the fasl file isn't up to date.
   When p, always do it."
  (let* ((buf (current-buffer))
	 (pn (buffer-pathname buf)))
    (unless pn (editor-error "Buffer has no associated pathname."))
    (cond ((buffer-modified buf)
	   (when (or (not (value compile-buffer-file-confirm))
		     (prompt-for-y-or-n
		      :default t :default-string "Y"
		      :prompt (list "Save and compile file ~A? "
				    (namestring pn))))
	     (write-buffer-file buf pn)
	     (file-compile pn :buffer buf)))
	  ((older-or-non-existent-fasl-p pn p)
	   (when (or (not (value compile-buffer-file-confirm))
		     (prompt-for-y-or-n
		      :default t :default-string "Y"
		      :prompt (list "Compile file ~A? " (namestring pn))))
	     (file-compile pn :buffer buf)))
	  ((or p
	       (prompt-for-y-or-n
		:default t :default-string "Y"
		:prompt
		"Fasl file up to date, compile source anyway? "))
	   (file-compile pn :buffer buf)))))

(defcommand "Compile Group" (p)
  "Compile each file in the current group which needs it.
  If a file has type LISP and there is a curresponding file with type
  FASL which has been written less recently (or it doesn't exit), then
  the file is compiled, with error output directed to the \"Compiler Warnings\"
  buffer.  If a prefix argument is provided, then all the files are compiled.
  All modified files are saved beforehand."
  "Do a Compile-File in each file in the current group that seems to need it."
  (save-all-files-command ())
  (unless *active-file-group* (editor-error "No active file group."))
  (dolist (file *active-file-group*)
    (when (string-equal (pathname-type file) "lisp")
      (let ((tn (probe-file file)))
	(cond ((not tn)
	       (message "File ~A not found." (namestring file)))
	      ((older-or-non-existent-fasl-p tn p)
	       (file-compile tn)))))))


;;;; Error hacking stuff.

(defcommand "Flush Compiler Error Information" (p)
  "Flushes all infomation about errors encountered while compiling using the
   current server"
  "Flushes all infomation about errors encountered while compiling using the
   current server"
  (declare (ignore p))
  (clear-server-errors (get-current-compile-server t)))

(defcommand "Next Compiler Error" (p)
  "Move to the next compiler error for the current server.  If an argument is 
   given, advance that many errors."
  "Move to the next compiler error for the current server.  If an argument is 
   given, advance that many errors."
  (let* ((server (get-current-compile-server t))
	 (errors (server-info-errors server))
	 (fp (fill-pointer errors)))
    (when (zerop fp)
      (editor-error "There are no compiler errors."))
    (let* ((old-index (server-info-error-index server))
	   (new-index (+ (or old-index -1) (or p 1))))
      (when (< new-index 0)
	(if old-index
	    (editor-error "Can't back up ~R, only at the ~:R compiler error."
			  (- p) (1+ old-index))
	    (editor-error "Not even at the first compiler error.")))
      (when (>= new-index fp)
	(if (= (1+ (or old-index -1)) fp)
	    (editor-error "No more compiler errors.")
	    (editor-error "Only ~R remaining compiler error~:P."
			  (- fp old-index 1))))
      (setf (server-info-error-index server) new-index)
      ;; Display the silly error.
      (let ((error (aref errors new-index)))
	(let ((region (error-info-region error)))
	  (if region
	      (let* ((start (region-start region))
		     (buffer (line-buffer (mark-line start))))
		(change-to-buffer buffer)
		(move-mark (buffer-point buffer) start))
	      (message "Hmm, no region for this error.")))
	(let* ((line (error-info-line error))
	       (buffer (line-buffer line)))
	  (if (and line (bufferp buffer))
	      (let ((mark (mark line 0)))
		(unless (buffer-windows buffer)
		  (let ((window (find-if-not
				 #'(lambda (window)
				     (or (eq window (current-window))
					 (eq window *echo-area-window*)))
				 *window-list*)))
		    (if window
			(setf (window-buffer window) buffer)
			(make-window mark))))
		(move-mark (buffer-point buffer) mark)
		(dolist (window (buffer-windows buffer))
		  (move-mark (window-display-start window) mark)
		  (move-mark (window-point window) mark))
		(delete-mark mark))
	      (message "Hmm, no line for this error.")))))))

(defcommand "Previous Compiler Error" (p)
  "Move to the previous compiler error. If an argument is given, move back
   that many errors."
  "Move to the previous compiler error. If an argument is given, move back
   that many errors."
  (next-compiler-error-command (- (or p 1))))



;;;; Operation management commands:

(defcommand "Abort Operations" (p)
  "Abort all operations on current eval server connection."
  "Abort all operations on current eval server connection."
  (declare (ignore p))
  (let* ((server (get-current-eval-server))
	 (wire (server-info-wire server)))
    ;; Tell the slave to abort the current operation and to ignore any further
    ;; operations.
    (dolist (note (server-info-notes server))
      (setf (note-state note) :aborted))
    #+NILGB (ext:send-character-out-of-band (hemlock.wire:wire-fd wire) #\N)
    (hemlock.wire:remote-value wire (server-accept-operations))
    ;; Synch'ing with server here, causes any operations queued at the socket or
    ;; in the server to be ignored, and the last thing evaluated is an
    ;; instruction to go on accepting operations.
    (hemlock.wire:wire-force-output wire)
    (dolist (note (server-info-notes server))
      (when (eq (note-state note) :pending)
	;; The HEMLOCK.WIRE:REMOTE-VALUE call should have allowed a handshake to
	;; tell the editor anything :pending was aborted.
	(error "Operation ~S is still around after we aborted it?" note)))
    ;; Forget anything queued in the editor.
    (setf (server-info-notes server) nil)))

(defcommand "List Operations" (p)
  "List all eval server operations which have not yet completed."
  "List all eval server operations which have not yet completed."
  (declare (ignore p))
  (let ((notes nil))
    ;; Collect all notes, reversing them since they act like a queue but
    ;; are not in queue order.
    (do-strings (str val *server-names*)
      (declare (ignore str))
      (setq notes (nconc notes (reverse (server-info-notes val)))))
    (if notes
	(with-pop-up-display (s)
	  (dolist (note notes)
	    (format s "~@(~8A~) ~A on ~A.~%"
		    (note-state note)
		    (note-context note)
		    (server-info-name (note-server note)))))
	(message "No uncompleted operations.")))
  (values))


;;;; Describing in the client lisp.

;;; "Describe Function Call" gets the function name from the current form
;;; as a string.  This string is used as the argument to a call to
;;; DESCRIBE-FUNCTION-CALL-AUX which is eval'ed in the client lisp.  The
;;; auxiliary function's name is qualified since it is read in the client
;;; Lisp with *package* bound to the buffer's package.  The result comes
;;; back as a list of strings, so we read the first string to get out the
;;; string value returned by DESCRIBE-FUNCTION-CALL-AUX in the client Lisp.
;;;
(defcommand "Describe Function Call" (p)
  "Describe the current function call."
  "Describe the current function call."
  (let ((info (value current-eval-server)))
    (cond
     ((not info)
      (message "Describing from the editor Lisp ...")
      (editor-describe-function-call-command p))
     (t
      (with-mark ((mark1 (current-point))
		  (mark2 (current-point)))
	(pre-command-parse-check mark1)
	(unless (backward-up-list mark1) (editor-error))
	(form-offset (move-mark mark2 (mark-after mark1)) 1)
	(let* ((package (value current-package))
	       (package-exists
		(eval-form-in-server-1
		 info
		 (format nil
			 "(if (find-package ~S) t (package-name *package*))"
			 package)
		 nil)))
	  (unless (eq package-exists t)
	    (message "Using package ~S in ~A since ~
		      ~:[there is no current package~;~:*~S does not exist~]."
		     package-exists (server-info-name info) package))
	  (with-pop-up-display (s)
	    (write-string (eval-form-in-server-1
			   info
			   (format nil "(hemlock::describe-function-call-aux ~S)"
				   (region-to-string (region mark1 mark2)))
			   (if (eq package-exists t) package nil))
			   s))))))))

;;; DESCRIBE-FUNCTION-CALL-AUX is always evaluated in a client Lisp to some
;;; editor, relying on the fact that the cores have the same functions.  String
;;; is the name of a function that is read (in the client Lisp).  The result is
;;; a string of all the output from EDITOR-DESCRIBE-FUNCTION.
;;;
(defun describe-function-call-aux (string)
  (let* ((sym (read-from-string string))
	 (fun (function-to-describe sym error)))
    (with-output-to-string (*standard-output*)
      (editor-describe-function fun sym))))

;;; "Describe Symbol" gets the symbol name and quotes it as the argument to a
;;; call to DESCRIBE-SYMBOL-AUX which is eval'ed in the client lisp.  The
;;; auxiliary function's name is qualified since it is read in the client Lisp
;;; with *package* bound to the buffer's package.  The result comes back as a
;;; list of strings, so we read the first string to get out the string value
;;; returned by DESCRIBE-SYMBOL-AUX in the client Lisp.
;;;

(defcommand "Describe Symbol" (p)
  "Describe the previous s-expression if it is a symbol."
  "Describe the previous s-expression if it is a symbol."
  (declare (ignore p))
  (let ((info (value current-eval-server)))
    (cond
     ((not info)
      (message "Describing from the editor Lisp ...")
      (editor-describe-symbol-command nil))
     (t
      (with-mark ((mark1 (current-point))
		  (mark2 (current-point)))
	(mark-symbol mark1 mark2)
	(with-pop-up-display (s)
	  (write-string (eval-form-in-server-1
			 info
			 (format nil "(hemlock::describe-symbol-aux '~A)"
				 (region-to-string (region mark1 mark2))))
			s)))))))

(defun describe-symbol-aux (thing)
  (with-output-to-string (*standard-output*)
    (describe (if (and (consp thing)
		       (or (eq (car thing) 'quote)
			   (eq (car thing) 'function))
		       (symbolp (cadr thing)))
		  (cadr thing)
		  thing))))
