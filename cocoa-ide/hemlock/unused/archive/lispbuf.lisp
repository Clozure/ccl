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
;;; Stuff to do a little lisp hacking in the editor's Lisp environment.
;;;

(in-package :hemlock)


(defmacro in-lisp (&body body)
  "Evaluates body inside HANDLE-LISP-ERRORS.  *package* is bound to the package
   named by \"Current Package\" if it is non-nil."
  (let ((name (gensym)) (package (gensym)))
    `(handle-lisp-errors
      (let* ((,name (value current-package))
	     (,package (and ,name (find-package ,name))))
	(progv (if ,package '(*package*)) (if ,package (list ,package))
	  ,@body)))))


(define-file-option "Package" (buffer value)
  (defhvar "Current Package"
    "The package used for evaluation of Lisp in this buffer."
    :buffer buffer
    :value
    (let* ((eof (list nil))
	   (thing (read-from-string value nil eof)))
      (when (eq thing eof) (error "Bad package file option value."))
      (cond
       ((stringp thing)
	thing)
       ((symbolp thing)
	(symbol-name thing))
       ((characterp thing)
	(string thing))
       (t
	(message
	 "Ignoring \"package\" file option -- cannot convert to a string."))))
    :hooks (list 'package-name-change-hook)))


;;;; Eval Mode Interaction.

(declaim (special * ** *** - + ++ +++ / // ///))


(defun get-prompt ()
  #+cmu (locally (declare (special ext:*prompt*))
          (if (functionp ext:*prompt*)
              (funcall ext:*prompt*)
              ext:*prompt*))
  #+sbcl (with-output-to-string (out)
           (funcall sb-int:*repl-prompt-fun* out))
  #-(or cmu sbcl) "* ")


(defun show-prompt (&optional (stream *standard-output*))
  #-sbcl (princ (get-prompt) stream)
  #+sbcl (funcall sb-int:*repl-prompt-fun* stream))


(defun setup-eval-mode (buffer)
  (let ((point (buffer-point buffer)))
    (setf (buffer-minor-mode buffer "Eval") t)
    (setf (buffer-minor-mode buffer "Editor") t)
    (setf (buffer-major-mode buffer) "Lisp")
    (buffer-end point)
    (defhvar "Current Package"
      "This variable holds the name of the package currently used for Lisp
       evaluation and compilation.  If it is Nil, the value of *Package* is used
       instead."
      :value nil
      :buffer buffer)
    (unless (hemlock-bound-p 'buffer-input-mark :buffer buffer)
      (defhvar "Buffer Input Mark"
	"Mark used for Eval Mode input."
	:buffer buffer
	:value (copy-mark point :right-inserting))
      (defhvar "Eval Output Stream"
	"Output stream used for Eval Mode output in this buffer."
	:buffer buffer
	:value (make-hemlock-output-stream point))
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
	:value 0))
    (let ((*standard-output*
	   (variable-value 'eval-output-stream :buffer buffer)))
      (fresh-line)
      (show-prompt))
    (move-mark (variable-value 'buffer-input-mark :buffer buffer) point)))

(defmode "Eval" :major-p nil :setup-function #'setup-eval-mode)

(defun eval-mode-lisp-mode-hook (buffer on)
  "Turn on Lisp mode when we go into Eval Mode."
  (when on
    (setf (buffer-major-mode buffer) "Lisp")))
;;;
(add-hook eval-mode-hook 'eval-mode-lisp-mode-hook)

(defhvar "Editor Definition Info"
  "When this is non-nil, the editor Lisp is used to determine definition
   editing information; otherwise, the slave Lisp is used."
  :value t
  :mode "Eval")


(defvar *selected-eval-buffer* nil)

(defcommand "Select Eval Buffer" (p)
  "Goto buffer in \"Eval\" mode, creating one if necessary."
  "Goto buffer in \"Eval\" mode, creating one if necessary."
  (declare (ignore p))
  (unless *selected-eval-buffer*
    (when (getstring "Eval" *buffer-names*)
      (editor-error "There is already a buffer named \"Eval\"!"))
    (setf *selected-eval-buffer*
	  (make-buffer "Eval"
		       :delete-hook
		       (list #'(lambda (buf)
				 (declare (ignore buf))
				 (setf *selected-eval-buffer* nil)))))
    (setf (buffer-minor-mode *selected-eval-buffer* "Eval") t))
  (change-to-buffer *selected-eval-buffer*))


(defvar lispbuf-eof '(nil))

(defhvar "Unwedge Interactive Input Confirm"
  "When set (the default), trying to confirm interactive input when the
   point is not after the input mark causes Hemlock to ask the user if he
   needs to be unwedged.  When not set, an editor error is signaled
   informing the user that the point is before the input mark."
  :value t)

(defun unwedge-eval-buffer ()
  (abort-eval-input-command nil))

(defhvar "Unwedge Interactive Input Fun"
  "Function to call when input is confirmed, but the point is not past the
   input mark."
  :value #'unwedge-eval-buffer
  :mode "Eval")

(defhvar "Unwedge Interactive Input String"
  "String to add to \"Point not past input mark.  \" explaining what will
   happen if the the user chooses to be unwedged."
  :value "Prompt again at the end of the buffer? "
  :mode "Eval")

(defcommand "Confirm Eval Input" (p)
  "Evaluate Eval Mode input between point and last prompt."
  "Evaluate Eval Mode input between point and last prompt."
  (declare (ignore p))
  (let ((input-region (get-interactive-input)))
    (when input-region
      (let* ((output (value eval-output-stream))
	     (*standard-output* output)
	     (*error-output* output)
	     (*trace-output* output))
	(fresh-line)
	(in-lisp
	 ;; Copy the region to keep the output and input streams from interacting
	 ;; since input-region is made of permanent marks into the buffer.
	 (with-input-from-region (stream (copy-region input-region))
	   (loop
	     (let ((form (read stream nil lispbuf-eof)))
	       (when (eq form lispbuf-eof)
		 ;; Move the buffer's input mark to the end of the buffer.
		 (move-mark (region-start input-region)
			    (region-end input-region))
		 (return))
	       (setq +++ ++ ++ + + - - form)
	       (let ((this-eval (multiple-value-list (eval form))))
		 (fresh-line)
		 (dolist (x this-eval) (prin1 x) (terpri))
		 (show-prompt)
		 (setq /// // // / / this-eval)
		 (setq *** ** ** * * (car this-eval)))))))))))

(defcommand "Abort Eval Input" (p)
  "Move to the end of the buffer and prompt."
  "Move to the end of the buffer and prompt."
  (declare (ignore p))
  (let ((point (current-point)))
    (buffer-end point)
    (insert-character point #\newline)
    (insert-string point "Aborted.")
    (insert-character point #\newline)
    (insert-string point (get-prompt))
    (move-mark (value buffer-input-mark) point)))



;;;; General interactive commands used in eval and typescript buffers.

(defun get-interactive-input ()
  "Tries to return a region.  When the point is not past the input mark, and
   the user has \"Unwedge Interactive Input Confirm\" set, the buffer is
   optionally fixed up, and nil is returned.  Otherwise, an editor error is
   signalled.  When a region is returned, the start is the current buffer's
   input mark, and the end is the current point moved to the end of the buffer."
  (let ((point (current-point))
	(mark (value buffer-input-mark)))
    (cond
     ((mark>= point mark)
      (buffer-end point)
      (let* ((input-region (region mark point))
	     (string (region-to-string input-region))
	     (ring (value interactive-history)))
	(when (and (or (zerop (ring-length ring))
		       (string/= string (region-to-string (ring-ref ring 0))))
		   (> (length string) (value minimum-interactive-input-length)))
	  (ring-push (copy-region input-region) ring))
	input-region))
     ((value unwedge-interactive-input-confirm)
      (beep)
      (when (prompt-for-y-or-n
	     :prompt (concatenate 'simple-string
				  "Point not past input mark.  "
				  (value unwedge-interactive-input-string))
	     :must-exist t :default t :default-string "yes")
	(funcall (value unwedge-interactive-input-fun))
	(message "Unwedged."))
      nil)
     (t
      (editor-error "Point not past input mark.")))))

(defhvar "Interactive History Length"
  "This is the length used for the history ring in interactive buffers.
   It must be set before turning on the mode."
  :value 10)

(defhvar "Minimum Interactive Input Length"
  "When the number of characters in an interactive buffer exceeds this value,
   it is pushed onto the interactive history, otherwise it is lost forever."
  :value 2)


(defvar *previous-input-search-string* "ignore")

(defvar *previous-input-search-pattern*
  ;; Give it a bogus string since you can't give it the empty string.
  (new-search-pattern :string-insensitive :forward "ignore"))

(defun get-previous-input-search-pattern (string)
  (if (string= *previous-input-search-string* string)
      *previous-input-search-pattern*
      (new-search-pattern :string-insensitive :forward 
			  (setf *previous-input-search-string* string)
			  *previous-input-search-pattern*)))

(defcommand "Search Previous Interactive Input" (p)
  "Search backward through the interactive history using the current input as
   a search string.  Consecutive invocations repeat the previous search."
  "Search backward through the interactive history using the current input as
   a search string.  Consecutive invocations repeat the previous search."
  (declare (ignore p))
  (let* ((mark (value buffer-input-mark))
	 (ring (value interactive-history))
	 (point (current-point))
	 (just-invoked (eq (last-command-type) :searching-interactive-input)))
    (when (mark<= point mark)
      (editor-error "Point not past input mark."))
    (when (zerop (ring-length ring))
      (editor-error "No previous input in this buffer."))
    (unless just-invoked
      (get-previous-input-search-pattern (region-to-string (region mark point))))
    (let ((found-it (find-previous-input ring just-invoked)))
      (unless found-it 
	(editor-error "Couldn't find ~a." *previous-input-search-string*))
      (delete-region (region mark point))
      (insert-region point (ring-ref ring found-it))
      (setf (value searching-interactive-pointer) found-it))
  (setf (last-command-type) :searching-interactive-input)))

(defun find-previous-input (ring againp)
  (let ((ring-length (ring-length ring))
	(base (if againp
		  (+ (value searching-interactive-pointer) 1)
		  0)))
      (loop
	(when (= base ring-length)
	  (if againp
	      (setf base 0)
	      (return nil)))
	(with-mark ((m (region-start (ring-ref ring base))))
	  (when (find-pattern m *previous-input-search-pattern*)
	    (return base)))
	(incf base))))

(defcommand "Previous Interactive Input" (p)
  "Insert the previous input in an interactive mode (Eval or Typescript).
   If repeated, keep rotating the history.  With prefix argument, rotate
   that many times."
  "Pop the *interactive-history* at the point."
  (let* ((point (current-point))
	 (mark (value buffer-input-mark))
	 (ring (value interactive-history))
	 (length (ring-length ring))
	 (p (or p 1)))
    (when (or (mark< point mark) (zerop length)) (editor-error))
    (cond
     ((eq (last-command-type) :interactive-history)
      (let ((base (mod (+ (value interactive-pointer) p) length)))
	(delete-region (region mark point))
	(insert-region point (ring-ref ring base))
	(setf (value interactive-pointer) base)))
     (t
      (let ((base (mod (if (minusp p) p (1- p)) length))
	    (region (delete-and-save-region (region mark point))))
	(insert-region point (ring-ref ring base))
	(when (mark/= (region-start region) (region-end region))
	  (ring-push region ring)
	  (incf base))
	(setf (value interactive-pointer) base)))))
  (setf (last-command-type) :interactive-history))

(defcommand "Next Interactive Input" (p)
  "Rotate the interactive history backwards.  The region is left around the
   inserted text.  With prefix argument, rotate that many times."
  "Call previous-interactive-input-command with negated arg."
  (previous-interactive-input-command (- (or p 1))))

(defcommand "Kill Interactive Input" (p)
  "Kill any input to an interactive mode (Eval or Typescript)."
  "Kill any input to an interactive mode (Eval or Typescript)."
  (declare (ignore p))
  (let ((point (buffer-point (current-buffer)))
	(mark (value buffer-input-mark)))
    (when (mark< point mark) (editor-error))
    (kill-region (region mark point) :kill-backward)))

(defcommand "Interactive Beginning of Line" (p)
  "If on line with current prompt, go to after it, otherwise do what
  \"Beginning of Line\" always does."
  "Go to after prompt when on prompt line."
  (let ((mark (value buffer-input-mark))
	(point (current-point)))
    (if (and (same-line-p point mark) (or (not p) (= p 1)))
	(move-mark point mark)
	(beginning-of-line-command p))))

(defcommand "Reenter Interactive Input" (p)
  "Copies the form to the left of point to be after the interactive buffer's
   input mark.  When the current region is active, it is copied instead."
  "Copies the form to the left of point to be after the interactive buffer's
   input mark.  When the current region is active, it is copied instead."
  (declare (ignore p))
  (unless (hemlock-bound-p 'buffer-input-mark)
    (editor-error "Not in an interactive buffer."))
  (let ((point (current-point)))
    (let ((region (if (region-active-p)
		      ;; Copy this, so moving point doesn't affect the region.
		      (copy-region (current-region))
		      (with-mark ((start point)
				  (end point))
			(pre-command-parse-check start)
			(unless (form-offset start -1)
			  (editor-error "Not after complete form."))
			(region (copy-mark start) (copy-mark end))))))
      (buffer-end point)
      (push-buffer-mark (copy-mark point))
      (insert-region point region)
      (setf (last-command-type) :ephemerally-active))))



;;; Other stuff.

(defmode "Editor")

(defcommand "Editor Mode" (p)
  "Turn on \"Editor\" mode in the current buffer.  If it is already on, turn it
  off.  When in editor mode, most lisp compilation and evaluation commands
  manipulate the editor process instead of the current eval server."
  "Toggle \"Editor\" mode in the current buffer."
  (declare (ignore p))
  (setf (buffer-minor-mode (current-buffer) "Editor")
	(not (buffer-minor-mode (current-buffer) "Editor"))))

(define-file-option "Editor" (buffer value)
  (declare (ignore value))
  (setf (buffer-minor-mode buffer "Editor") t))

(defhvar "Editor Definition Info"
  "When this is non-nil, the editor Lisp is used to determine definition
   editing information; otherwise, the slave Lisp is used."
  :value t
  :mode "Editor")

(defcommand "Editor Compile Defun" (p)
  "Compiles the current or next top-level form in the editor Lisp.
   First the form is evaluated, then the result of this evaluation
   is passed to compile.  If the current region is active, this
   compiles the region."
  "Evaluates the current or next top-level form in the editor Lisp."
  (declare (ignore p))
  (if (region-active-p)
      (editor-compile-region (current-region))
      (editor-compile-region (defun-region (current-point)) t)))

(defcommand "Editor Compile Region" (p)
  "Compiles lisp forms between the point and the mark in the editor Lisp."
  "Compiles lisp forms between the point and the mark in the editor Lisp."
  (declare (ignore p))
  (editor-compile-region (current-region)))

(defun defun-region (mark)
  "This returns a region around the current or next defun with respect to mark.
   Mark is not used to form the region.  If there is no appropriate top level
   form, this signals an editor-error.  This calls PRE-COMMAND-PARSE-CHECK."
  (with-mark ((start mark)
	      (end mark))
    (pre-command-parse-check start)
    (cond ((not (mark-top-level-form start end))
	   (editor-error "No current or next top level form."))
	  (t (region start end)))))

(defun editor-compile-region (region &optional quiet)
  (unless quiet (message "Compiling region ..."))
  (in-lisp
   (with-input-from-region (stream region)
     (with-pop-up-display (*error-output* :height 19)
       ;; JDz: We don't record source locations and what not, but this
       ;; is portable.  CMUCL specific implementation removed because
       ;; it does not work on HEMLOCK-REGION-STREAM (but it can be
       ;; added back later if CMUCL starts using user-extensible
       ;; streams internally.)
       (funcall (compile nil `(lambda ()
                                ,@(loop for form = (read stream nil stream)
                                        until (eq form stream)
                                        collect form))))))))


(defcommand "Editor Evaluate Defun" (p)
  "Evaluates the current or next top-level form in the editor Lisp.
   If the current region is active, this evaluates the region."
  "Evaluates the current or next top-level form in the editor Lisp."
  (declare (ignore p))
  (if (region-active-p)
      (editor-evaluate-region-command nil)
      (with-input-from-region (stream (defun-region (current-point)))
	(clear-echo-area)
	(in-lisp
	 (message "Editor Evaluation returned ~S"
		  (eval (read stream)))))))

(defcommand "Editor Evaluate Region" (p)
  "Evaluates lisp forms between the point and the mark in the editor Lisp."
  "Evaluates lisp forms between the point and the mark in the editor Lisp."
  (declare (ignore p))
  (with-input-from-region (stream (current-region))
    (clear-echo-area)
    (write-string "Evaluating region in the editor ..." *echo-area-stream*)
    (finish-output *echo-area-stream*)
    (in-lisp
     (do ((object (read stream nil lispbuf-eof) 
		  (read stream nil lispbuf-eof)))
	 ((eq object lispbuf-eof))
       (eval object)))
    (message "Evaluation complete.")))
           
(defcommand "Editor Re-evaluate Defvar" (p)
  "Evaluate the current or next top-level form if it is a DEFVAR.  Treat the
   form as if the variable is not bound.  This occurs in the editor Lisp."
  "Evaluate the current or next top-level form if it is a DEFVAR.  Treat the
   form as if the variable is not bound.  This occurs in the editor Lisp."
  (declare (ignore p))
  (with-input-from-region (stream (defun-region (current-point)))
    (clear-echo-area)
    (in-lisp
     (let ((form (read stream)))
       (unless (eq (car form) 'defvar) (editor-error "Not a DEFVAR."))
       (makunbound (cadr form))
       (message "Evaluation returned ~S" (eval form))))))

(defcommand "Editor Macroexpand Expression" (p)
  "Show the macroexpansion of the current expression in the null environment.
   With an argument, use MACROEXPAND instead of MACROEXPAND-1."
  "Show the macroexpansion of the current expression in the null environment.
   With an argument, use MACROEXPAND instead of MACROEXPAND-1."
  (let ((point (buffer-point (current-buffer))))
    (with-mark ((start point))
      (pre-command-parse-check start)
      (with-mark ((end start))
        (unless (form-offset end 1) (editor-error))
	(in-lisp
	 (with-pop-up-display (rts)
	   (write-string (with-input-from-region (s (region start end))
			   (prin1-to-string (funcall (if p
							 'macroexpand
							 'macroexpand-1)
						     (read s))))
			 rts)))))))

(defcommand "Editor Evaluate Expression" (p)
  "Prompt for an expression to evaluate in the editor Lisp."
  "Prompt for an expression to evaluate in the editor Lisp."
  (declare (ignore p))
  (in-lisp
   (multiple-value-call #'message "=> ~@{~#[~;~S~:;~S, ~]~}"
     (eval (prompt-for-expression
	    :prompt "Editor Eval: "
	    :help "Expression to evaluate")))))

(defcommand "Editor Evaluate Buffer" (p)
  "Evaluates the text in the current buffer in the editor Lisp."
  "Evaluates the text in the current buffer redirecting *Standard-Output* to
   the echo area.  This occurs in the editor Lisp.  The prefix argument is
   ignored."
  (declare (ignore p))
  (clear-echo-area)
  (write-string "Evaluating buffer in the editor ..." *echo-area-stream*)
  (finish-output *echo-area-stream*)
  (with-input-from-region (stream (buffer-region (current-buffer)))
    (let ((*standard-output* *echo-area-stream*))
      (in-lisp
       (do ((object (read stream nil lispbuf-eof) 
		    (read stream nil lispbuf-eof)))
	   ((eq object lispbuf-eof))
	 (eval object))))
    (message "Evaluation complete.")))



;;; With-Output-To-Window  --  Internal
;;;
;;;
(defmacro with-output-to-window ((stream name) &body forms)
  "With-Output-To-Window (Stream Name) {Form}*
  Bind Stream to a stream that writes into the buffer named Name a la
  With-Output-To-Mark.  The buffer is created if it does not exist already
  and a window is created to display the buffer if it is not displayed.
  For the duration of the evaluation this window is made the current window."
  (let ((nam (gensym)) (buffer (gensym)) (point (gensym)) 
	(window (gensym)) (old-window (gensym)))
    `(let* ((,nam ,name)
	    (,buffer (or (getstring ,nam *buffer-names*) (make-buffer ,nam)))
	    (,point (buffer-end (buffer-point ,buffer)))
	    (,window (or (car (buffer-windows ,buffer)) (make-window ,point)))
	    (,old-window (current-window)))
       (unwind-protect
	 (progn (setf (current-window) ,window)
		(buffer-end ,point)
		(with-output-to-mark (,stream ,point) ,@forms))
	 (setf (current-window) ,old-window)))))

(defcommand "Editor Compile File" (p)
  "Prompts for file to compile in the editor Lisp.  Does not compare source
   and binary write dates.  Does not check any buffer for that file for
   whether the buffer needs to be saved."
  "Prompts for file to compile."
  (declare (ignore p))
  (let ((pn (prompt-for-file :default
			     (buffer-default-pathname (current-buffer))
			     :prompt "File to compile: ")))
    (with-output-to-window (*error-output* "Compiler Warnings")
      (in-lisp (compile-file (namestring pn) #+cmu :error-file #+cmu nil)))))


(defun older-or-non-existent-fasl-p (pathname &optional definitely)
  (let ((obj-pn (probe-file (compile-file-pathname pathname))))
    (or definitely
	(not obj-pn)
	(< (file-write-date obj-pn) (file-write-date pathname)))))


(defcommand "Editor Compile Buffer File" (p)
  "Compile the file in the current buffer in the editor Lisp if its associated
   binary file (of type .fasl) is older than the source or doesn't exist.  When
   the binary file is up to date, the user is asked if the source should be
   compiled anyway.  When the prefix argument is supplied, compile the file
   without checking the binary file.  When \"Compile Buffer File Confirm\" is
   set, this command will ask for confirmation when it otherwise would not."
  "Compile the file in the current buffer in the editor Lisp if the fasl file
   isn't up to date.  When p, always do it."
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
	     (with-output-to-window (*error-output* "Compiler Warnings")
	       (in-lisp (compile-file (namestring pn) #+cmu :error-file #+cmu nil)))))
	  ((older-or-non-existent-fasl-p pn p)
	   (when (or (not (value compile-buffer-file-confirm))
		     (prompt-for-y-or-n
		      :default t :default-string "Y"
		      :prompt (list "Compile file ~A? " (namestring pn))))
	     (with-output-to-window (*error-output* "Compiler Warnings")
	       (in-lisp (compile-file (namestring pn) #+cmu :error-file #+cmu nil)))))
	  (t (when (or p
		       (prompt-for-y-or-n
			:default t :default-string "Y"
			:prompt
			"Fasl file up to date, compile source anyway? "))
	       (with-output-to-window (*error-output* "Compiler Warnings")
		 (in-lisp (compile-file (namestring pn) #+cmu :error-file #+cmu nil))))))))

(defcommand "Editor Compile Group" (p)
  "Compile each file in the current group which needs it in the editor Lisp.
   If a file has type LISP and there is a curresponding file with type
   FASL which has been written less recently (or it doesn't exit), then
   the file is compiled, with error output directed to the \"Compiler Warnings\"
   buffer.  If a prefix argument is provided, then all the files are compiled.
   All modified files are saved beforehand."
  "Do a Compile-File in each file in the current group that seems to need it
   in the editor Lisp."
  (save-all-files-command ())
  (unless *active-file-group* (editor-error "No active file group."))
  (dolist (file *active-file-group*)
    (when (string-equal (pathname-type file) "lisp")
      (let ((tn (probe-file file)))
	(cond ((not tn)
	       (message "File ~A not found." (namestring file)))
	      ((older-or-non-existent-fasl-p tn p)
	       (with-output-to-window (*error-output* "Compiler Warnings")
		 (in-lisp (compile-file (namestring tn) #+cmu :error-file #+cmu nil)))))))))

(defcommand "List Compile Group" (p)
  "List any files that would be compiled by \"Compile Group\".  All Modified
   files are saved before checking to generate a consistent list."
  "Do a Compile-File in each file in the current group that seems to need it."
  (declare (ignore p))
  (save-all-files-command ())
  (unless *active-file-group* (editor-error "No active file group."))
  (with-pop-up-display (s)
    (write-line "\"Compile Group\" would compile the following files:" s)
    (force-output s)
    (dolist (file *active-file-group*)
      (when (string-equal (pathname-type file) "lisp")
	(let ((tn (probe-file file)))
	  (cond ((not tn)
		 (format s "File ~A not found.~%" (namestring file)))
		((older-or-non-existent-fasl-p tn)
		 (write-line (namestring tn) s)))
	  (force-output s))))))

(defhvar "Load Pathname Defaults"
  "The default pathname used by the load command.")

(defcommand "Editor Load File" (p)
  "Prompt for a file to load into Editor Lisp."
  "Prompt for a file to load into the Editor Lisp."
  (declare (ignore p))
  (let ((name (truename (prompt-for-file
			 :default
			 (or (value load-pathname-defaults)
			     (buffer-default-pathname (current-buffer)))
			 :prompt "Editor file to load: "
			 :help "The name of the file to load"))))
    (setv load-pathname-defaults name)
    (in-lisp (load name))))



;;;; Lisp documentation stuff.

;;; FUNCTION-TO-DESCRIBE is used in "Editor Describe Function Call" and
;;; "Describe Function Call".
;;;
(defmacro function-to-describe (var error-name)
  `(cond ((not (symbolp ,var))
	  (,error-name "~S is not a symbol." ,var))
	 ((macro-function ,var))
	 ((fboundp ,var)
	  (if (listp (symbol-function ,var))
	      ,var
	      (symbol-function ,var)))
	 (t
	  (,error-name "~S is not a function." ,var))))

(defcommand "Editor Describe Function Call" (p)
  "Describe the most recently typed function name in the editor Lisp."
  "Describe the most recently typed function name in the editor Lisp."
  (declare (ignore p))
  (with-mark ((mark1 (current-point))
	      (mark2 (current-point)))
    (pre-command-parse-check mark1)
    (unless (backward-up-list mark1) (editor-error))
    (form-offset (move-mark mark2 (mark-after mark1)) 1)
    (with-input-from-region (s (region mark1 mark2))
      (in-lisp
       (let* ((sym (read s))
	      (fun (function-to-describe sym editor-error)))
	 (with-pop-up-display (*standard-output*)
	   (editor-describe-function fun sym)))))))


(defcommand "Editor Describe Symbol" (p)
  "Describe the previous s-expression if it is a symbol in the editor Lisp."
  "Describe the previous s-expression if it is a symbol in the editor Lisp."
  (declare (ignore p))
  (with-mark ((mark1 (current-point))
	      (mark2 (current-point)))
    (mark-symbol mark1 mark2)
    (with-input-from-region (s (region mark1 mark2))
      (in-lisp
       (let ((thing (read s)))
	 (if (symbolp thing)
	     (with-pop-up-display (*standard-output*)
	       (describe thing))
	     (if (and (consp thing)
		      (or (eq (car thing) 'quote)
			  (eq (car thing) 'function))
		      (symbolp (cadr thing)))
		 (with-pop-up-display (*standard-output*)
		   (describe (cadr thing)))
		 (editor-error "~S is not a symbol, or 'symbol, or #'symbol."
			       thing))))))))

;;; MARK-SYMBOL moves mark1 and mark2 around the previous or current symbol.
;;; However, if the marks are immediately before the first constituent char
;;; of the symbol name, we use the next symbol since the marks probably
;;; correspond to the point, and Hemlock's cursor display makes it look like
;;; the point is within the symbol name.  This also tries to ignore :prefix
;;; characters such as quotes, commas, etc.
;;;
(defun mark-symbol (mark1 mark2)
  (pre-command-parse-check mark1)
  (with-mark ((tmark1 mark1)
	      (tmark2 mark1))
    (cond ((and (form-offset tmark1 1)
		(form-offset (move-mark tmark2 tmark1) -1)
		(or (mark= mark1 tmark2)
		    (and (find-attribute tmark2 :lisp-syntax
					 #'(lambda (x) (not (eq x :prefix))))
			 (mark= mark1 tmark2))))
	   (form-offset mark2 1))
	  (t
	   (form-offset mark1 -1)
	   (find-attribute mark1 :lisp-syntax
			   #'(lambda (x) (not (eq x :prefix))))
	   (form-offset (move-mark mark2 mark1) 1)))))


(defcommand "Editor Describe" (p)
  "Call Describe on a Lisp object.
  Prompt for an expression which is evaluated to yield the object."
  "Prompt for an object to describe."
  (declare (ignore p))
  (in-lisp
   (let* ((exp (prompt-for-expression
		:prompt "Object: "
		:help "Expression to evaluate to get object to describe."))
	  (obj (eval exp)))
     (with-pop-up-display (*standard-output*)
       (describe obj)))))

(defcommand "Filter Region" (p)
  "Apply a Lisp function to each line of the region.
  An expression is prompted for which should evaluate to a Lisp function
  from a string to a string.  The function must neither modify its argument
  nor modify the return value after it is returned."
  "Call prompt for a function, then call Filter-Region with it and the region."
  (declare (ignore p))
  (let* ((exp (prompt-for-expression
	       :prompt "Function: "
	       :help "Expression to evaluate to get function to use as filter."))
	 (fun (in-lisp (eval exp)))
	 (region (current-region)))
    (let* ((start (copy-mark (region-start region) :left-inserting))
	   (end (copy-mark (region-end region) :left-inserting))
	   (region (region start end))
	   (undo-region (copy-region region)))
      (filter-region fun region)
      (make-region-undo :twiddle "Filter Region" region undo-region))))
