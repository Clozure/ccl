;;; -*- Mode: Lisp; Package: ED; Log: hemlock.log -*-
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
;;; This contains commands for sending debugger commands to slaves in the
;;; debugger.
;;;
;;; Written by Bill Chiles.
;;;

(in-package :hemlock)



;;;; DEFINE-DEBUGGER-COMMAND.

(defmacro define-debugger-command (name doc cmd &key uses-argument)
  `(defcommand ,(concatenate 'simple-string "Debug " name) (p)
     ,doc ,doc
     ,@(if uses-argument
	   nil
	   '((declare (ignore p))))
     (let* ((server-info (get-current-eval-server t))
	    (wire (server-info-wire server-info)))
       (wire:remote wire
	 (ts-stream-accept-input
	  (ts-data-stream (server-info-slave-info server-info))
	  ,(if uses-argument
	       `(list ,cmd p)
	       cmd)))
       (wire:wire-force-output wire))))



;;;; Frame changing commands.

(define-debugger-command "Up"
  "Moves the \"Current Eval Server\" up one debugger frame."
  :up)

(define-debugger-command "Down"
  "Moves the \"Current Eval Server\" down one debugger frame."
  :down)

(define-debugger-command "Top"
  "Moves the \"Current Eval Server\" to the top of the debugging stack."
  :top)

(define-debugger-command "Bottom"
  "Moves the \"Current Eval Server\" to the bottom of the debugging stack."
  :bottom)

(define-debugger-command "Frame"
  "Moves the \"Current Eval Server\" to the absolute debugger frame number
   indicated by the prefix argument."
  :frame
  :uses-argument t)



;;;; In and Out commands.

(define-debugger-command "Quit"
  "In the \"Current Eval Server\", throws to top level out of the debugger."
  :quit)

(define-debugger-command "Go"
  "In the \"Current Eval Server\", tries the CONTINUE restart."
  :go)

(define-debugger-command "Abort"
  "In the \"Current Eval Server\", execute the previous ABORT restart."
  :abort)

(define-debugger-command "Restart"
  "In the \"Current Eval Server\", executes the restart indicated by the
   prefix argument."
  :restart
  :uses-argument t)



;;;; Information commands.

(define-debugger-command "Help"
  "In the \"Current Eval Server\", prints the debugger's help text."
  :help)

(define-debugger-command "Error"
  "In the \"Current Eval Server\", print the error condition and restart cases
   upon entering the debugger."
  :error)

(define-debugger-command "Backtrace"
  "Executes the debugger's BACKTRACE command."
  :backtrace)

(define-debugger-command "Print"
  "In the \"Current Eval Server\", prints a representation of the debugger's
   current frame."
  :print)

(define-debugger-command "Verbose Print"
  "In the \"Current Eval Server\", prints a representation of the debugger's
   current frame without elipsis."
  :vprint)

(define-debugger-command "List Locals"
  "In the \"Current Eval Server\", prints the local variables for the debugger's
   current frame."
  :list-locals)

(define-debugger-command "Source"
  "In the \"Current Eval Server\", prints the source form for the debugger's
   current frame."
  :source)

(define-debugger-command "Verbose Source"
  "In the \"Current Eval Server\", prints the source form for the debugger's
   current frame with surrounding forms for context."
  :vsource)



;;;; Source editing.

;;; "Debug Edit Source" -- Command.
;;;
;;; The :edit-source command in the slave debugger initiates a synchronous RPC
;;; into the editor via the wire in *terminal-io*, a typescript stream.  This
;;; routine takes the necessary values, a file and source-path, and changes the
;;; editor's state to display that location.
;;;
;;; This command has to wait on SERVE-EVENT until some special is set by the
;;; RPC routine saying it is okay to return to the editor's top level.
;;;
(defvar *debug-editor-source-data* nil)
(defvar *in-debug-edit-source* nil)

(defcommand "Debug Edit Source" (p)
  "Given the \"Current Eval Server\"'s current debugger frame, place the user
   at the location's source in the editor."
  "Given the \"Current Eval Server\"'s current debugger frame, place the user
   at the location's source in the editor."
  (declare (ignore p))
  (let* ((server-info (get-current-eval-server t))
	 (wire (server-info-wire server-info)))
    ;;
    ;; Tell the slave to tell the editor some source info.
    (wire:remote wire
      (ts-stream-accept-input
       (ts-data-stream (server-info-slave-info server-info))
       :edit-source))
    (wire:wire-force-output wire)
    ;;
    ;; Wait for the source info.
    (let ((*debug-editor-source-data* nil)
	  (*in-debug-edit-source* t))
      (catch 'blow-debug-edit-source
	(loop
	  (system:serve-event)
	  (when *debug-editor-source-data* (return)))))))

;;; EDIT-SOURCE-LOCATION -- Internal Interface.
;;;
;;; The slave calls this in the editor when the debugger gets an :edit-source
;;; command.  This receives the information necessary to take the user in
;;; Hemlock to the source location, and does it.
;;;
(defun edit-source-location (name source-created-date tlf-offset
			     local-tlf-offset char-offset form-number)
  (let ((pn (pathname name)))
    (unless (probe-file pn)
      (editor-error "Source file no longer exists: ~A." name))
    (multiple-value-bind (buffer newp) (find-file-buffer pn)
      (let ((date (buffer-write-date buffer))
	    (point (buffer-point buffer)))
	(when newp (push-buffer-mark (copy-mark point) nil))
	(buffer-start point)
	;;
	;; Get to the top-level form in the buffer.
	(cond ((buffer-modified buffer)
	       (loud-message "Buffer has been modified.  Using form offset ~
			      instead of character position.")
	       (dotimes (i local-tlf-offset) 
		 (pre-command-parse-check point)
		 (form-offset point 1)))
	      ((not date)
	       (loud-message "Cannot compare write dates.  Assuming source ~
			      has not been modified -- ~A."
			     name)
	       (character-offset point char-offset))
	      ((= source-created-date date)
	       (character-offset point char-offset))
	      (t
	       (loud-message "File has been modified since reading the source.  ~
			      Using form offset instead of character position.")
	       (dotimes (i local-tlf-offset) 
		 (pre-command-parse-check point)
		 (form-offset point 1))))
	;;
	;; Read our form, get form-number translations, get the source-path,
	;; and make it usable.
	;;
	;; NOTE: Here READ is used in the editor lisp to look at a form
	;; that the compiler has digested in the slave lisp. The editor
	;; does not have the same environment at the slave so bad things
	;; can happen if READ hits a #. reader macro (like unknown package
	;; or undefined function errors) which can break the editor. This
	;; code basically inhibits the read-time eval. This doesn't always
	;; work right as the compiler may be seeing a different form structure
	;; and the compiler's version of PATH may not match the editor's.
	;; The main trouble seen in testing is that the 'form-number'
	;; supplied by the compiler was one more than what the vector
	;; returned by form-number-translations contained. For lack of a
	;; better solution, I (pw) just limit the form-number to legal range.
	;; This has worked ok on test code but may be off for some 
	;; forms. At least the editor won't break.

	(let* ((vector (di:form-number-translations
			(with-input-from-region
			    (s (region point (buffer-end-mark buffer)))
			  (let ((*read-suppress* t))
			    (read s)))
			tlf-offset))
	       ;; Don't signal error on index overrun.It may be due
	       ;; to read-time eval getting form editing blind to
	       ;; editor
	       (index (min form-number (1- (length vector))))
	       (path (nreverse (butlast (cdr (svref vector index))))))
	  ;;
	  ;; Walk down to the form.  Change to buffer in case we get an error
	  ;; while finding the form.
	  (change-to-buffer buffer)
	  (mark-to-debug-source-path point path)))))
  (setf *debug-editor-source-data* t)
  ;;
  ;; While Hemlock was setting up the source edit, the user could have typed
  ;; while looking at a buffer no longer current when the commands execute.
  (clear-editor-input *editor-input*))

;;; CANNOT-EDIT-SOURCE-LOCATION -- Interface.
;;;
;;; The slave calls this when the debugger command "EDIT-SOURCE" runs, and the
;;; slave cannot give the editor source information.
;;;
(defun cannot-edit-source-location ()
  (loud-message "Can't edit source.")
  (when *in-debug-edit-source*
    (throw 'blow-debug-edit-source nil)))


;;;; Breakpoints.

;;;
;;; Breakpoint information for editor management.
;;;

;;; This holds all the stuff we might want to know about a breakpoint in some
;;; slave.
;;;
(defstruct (breakpoint-info (:print-function print-breakpoint-info)
			    (:constructor make-breakpoint-info
					  (slave buffer remote-object name)))
  (slave nil :type server-info)
  (buffer nil :type buffer)
  (remote-object nil :type wire:remote-object)
  (name nil :type simple-string))
;;;
(defun print-breakpoint-info (obj str n)
  (declare (ignore n))
  (format str "#<Breakpoint-Info for ~S>" (breakpoint-info-name obj)))

(defvar *breakpoints* nil)

(macrolet ((frob (name accessor)
	     `(defun ,name (key)
		(let ((res nil))
		  (dolist (bpt-info *breakpoints* res)
		    (when (eq (,accessor bpt-info) key)
		      (push bpt-info res)))))))
  (frob slave-breakpoints breakpoint-info-slave)
  (frob buffer-breakpoints breakpoint-info-buffer))

(defun delete-breakpoints-buffer-hook (buffer)
  (let ((server-info (value current-eval-server)))
    (when server-info
      (let ((bpts (buffer-breakpoints buffer))
	    (wire (server-info-wire server-info)))
	  (dolist (b bpts)
	    (setf *breakpoints* (delete b *breakpoints*))
	    (when wire
	      (wire:remote wire
		(di:delete-breakpoint (breakpoint-info-remote-object b))))
	(when wire
	  (wire:wire-force-output wire)))))))
;;;
(add-hook delete-buffer-hook 'delete-breakpoints-buffer-hook)

;;;
;;; Setting breakpoints.
;;;

;;; "Debug Breakpoint" uses this to prompt for :function-end and
;;; :function-start breakpoints.
;;;
(defvar *function-breakpoint-strings*
  (make-string-table :initial-contents
		     '(("Start" . :function-start) ("End" . :function-end))))
;;;
;;; Maybe this should use the wire level directly and hold onto remote-objects
;;; identifying the breakpoints.  Then we could write commands to show where
;;; the breakpoints were and to individually deactivate or delete them.  As it
;;; is now we probably have to delete all for a given function.  What about
;;; setting user supplied breakpoint hook-functions, or Hemlock supplying a
;;; nice set such as something to simply print all locals at a certain
;;; location.
;;;
(defcommand "Debug Breakpoint" (p)
  "This tries to set a breakpoint in the \"Current Eval Server\" at the
   location designated by the current point.  If there is no known code
   location at the point, then this moves the point to the closest location
   before the point.  With an argument, this sets a breakpoint at the start
   or end of the function, prompting the user for which one to use."
  "This tries to set a breakpoint in the \"Current Eval Server\" at the
   location designated by the current point.  If there is no known code
   location at the point, then this moves the point to the closest location
   before the point.  With an argument, this sets a breakpoint at the start
   or end of the function, prompting the user for which one to use."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (let ((name (find-defun-for-breakpoint point)))
      (if p
	  (multiple-value-bind (str place)
			       (prompt-for-keyword
				(list *function-breakpoint-strings*)
				:prompt "Set breakpoint at function: "
				:default :start :default-string "Start")
	    (declare (ignore str))
	    (set-breakpoint-in-slave (get-current-eval-server t) name place))
	  (let* ((path (find-path-for-breakpoint point))
		 (server-info (get-current-eval-server t))
		 (res (set-breakpoint-in-slave server-info name path)))
	    (cond ((not res)
		   (message "No code locations correspond with point."))
		  ((wire:remote-object-p res)
		   (push (make-breakpoint-info server-info (current-buffer)
					       res name)
			 *breakpoints*)
		   (message "Breakpoint set."))
		  (t
		   (resolve-ambiguous-breakpoint-location server-info
							  name res))))))))

;;; FIND-PATH-FOR-BREAKPOINT -- Internal.
;;;
;;; This walks up from point to the beginning of its containing DEFUN to return
;;; the pseudo source-path (no form-number, no top-level form offset, and in
;;; descent order from start of the DEFUN).
;;;
(defun find-path-for-breakpoint (point)
  (with-mark ((m point)
	      (end point))
    (let ((path nil))
      (top-level-offset end -1)
      (with-mark ((containing-form m))
	(loop
	  (when (mark= m end) (return))
	  (backward-up-list containing-form)
	  (do ((count 0 (1+ count)))
	      ((mark= m containing-form)
	       ;; Count includes moving from the first form inside the
	       ;; containing-form paren to the outside of the containing-form
	       ;; paren -- one too many.
	       (push (1- count) path))
	    (form-offset m -1))))
      path)))

;;; SET-BREAKPOINT-IN-SLAVE -- Internal.
;;;
;;; This tells the slave to set a breakpoint for name.  Path is a modified
;;; source-path (with no form-number or top-level-form offset) or a symbol
;;; (:function-start or :function-end).  If the server dies while evaluating
;;; form, then this signals an editor-error.
;;;
(defun set-breakpoint-in-slave (server-info name path)
  (when (server-info-notes server-info)
    (editor-error "Server ~S is currently busy.  See \"List Operations\"."
		  (server-info-name server-info)))
  (multiple-value-bind (res error)
		       (wire:remote-value (server-info-wire server-info)
			 (di:set-breakpoint-for-editor (value current-package)
						       name path))
    (when error (editor-error "The server died before finishing."))
    res))

;;; RESOLVE-AMBIGUOUS-BREAKPOINT-LOCATION -- Internal.
;;;
;;; This helps the user select an ambiguous code location for "Debug
;;; Breakpoint".
;;;
(defun resolve-ambiguous-breakpoint-location (server-info name locs)
  (declare (list locs))
  (let ((point (current-point))
	(loc-num (length locs))
	(count 1)
	(cur-loc locs))
    (flet ((show-loc ()
	     (top-level-offset point -1)
	     (mark-to-debug-source-path point (cdar cur-loc))))
      (show-loc)
      (command-case (:prompt `("Ambiguous location ~D of ~D: " ,count ,loc-num)
		      :help "Pick a location to set a breakpoint."
		      :change-window nil)
	(#\space "Move point to next possible location."
	  (setf cur-loc (cdr cur-loc))
	  (cond (cur-loc
		 (incf count))
		(t
		 (setf cur-loc locs)
		 (setf count 1)))
	  (show-loc)
	  (reprompt))
	(:confirm "Choose the current location."
	  (let ((res (wire:remote-value (server-info-wire server-info)
		       (di:set-location-breakpoint-for-editor (caar cur-loc)))))
	    (unless (wire:remote-object-p res)
	      (editor-error "Couldn't set breakpoint from location?"))
	    (push (make-breakpoint-info server-info (current-buffer) res name)
		  *breakpoints*))
	  (message "Breakpoint set."))))))

;;; MARK-TO-DEBUG-SOURCE-PATH -- Internal.
;;;
;;; This takes a mark at the beginning of a top-level form and modified debugger
;;; source-path.  Path has no form number or top-level-form offset element, and
;;; it has been reversed to actually be usable.
;;;
(defun mark-to-debug-source-path (mark path)
  (let ((quote-or-function nil))
    (pre-command-parse-check mark)
    (dolist (n path)
      (when quote-or-function
	(editor-error
	 "Apparently settled on the symbol QUOTE or FUNCTION via their ~
	  read macros, which is odd, but furthermore there seems to be ~
	  more source-path left."))
      (unless (form-offset mark 1)
	;; Want to use the following and delete the next FORM-OFFSET -1.
	;; (scan-direction-valid mark t (or :open-paren :prefix))
	(editor-error
	 "Ran out of text in buffer with more source-path remaining."))
      (form-offset mark -1)
      (ecase (next-character mark)
	(#\(
	 (mark-after mark)
	 (form-offset mark n))
	(#\'
	 (case n
	   (0 (setf quote-or-function t))
	   (1 (mark-after mark))
	   (t (editor-error "Next form is QUOTE, but source-path index ~
			     is other than zero or one."))))
	(#\#
	 (case (next-character (mark-after mark))
	   (#\'
	    (case n
	      (0 (setf quote-or-function t))
	      (1 (mark-after mark))
	      (t (editor-error "Next form is FUNCTION, but source-path ~
				index is other than zero or one."))))
	   (t (editor-error
	       "Can only parse ' and #' read macros."))))))
    ;; Get to the beginning of the form.
    (form-offset mark 1)
    (form-offset mark -1)))

;;;
;;; Deleting breakpoints.
;;;

(defhvar "Delete Breakpoints Confirm"
  "This determines whether \"Debug Delete Breakpoints\" should ask for
   confirmation before deleting breakpoints."
  :value t)

(defcommand "Debug Delete Breakpoints" (p)
  "This deletes all breakpoints for the named DEFUN containing the point.
   This affects the \"Current Eval Server\"."
  "This deletes all breakpoints for the named DEFUN containing the point.
   This affects the \"Current Eval Server\"."
  (declare (ignore p))
  (let* ((server-info (get-current-eval-server t))
	 (wire (server-info-wire server-info))
	 (name (find-defun-for-breakpoint (current-point)))
	 (bpts (slave-breakpoints server-info)))
    (cond ((not bpts)
	   (message "No breakpoints recorded for ~A." name))
	  ((or (not (value delete-breakpoints-confirm))
	       (prompt-for-y-or-n :prompt `("Delete breakpoints for ~A? " ,name)
				  :default t
				  :default-string "Y"))
	   (dolist (b bpts)
	     (when (string= name (breakpoint-info-name b))
	       (setf *breakpoints* (delete b *breakpoints*))
	       (wire:remote wire
		 (di:delete-breakpoint-for-editor
		  (breakpoint-info-remote-object b)))))
	   (wire:wire-force-output wire)))))

;;;
;;; Breakpoint utilities.
;;;

;;; FIND-DEFUN-FOR-BREAKPOINT -- Internal.
;;;
;;; This returns as a string the name of the DEFUN containing point.  It
;;; signals any errors necessary to ensure "we are in good form".
;;;
(defun find-defun-for-breakpoint (point)
  (with-mark ((m1 point)
	      (m2 point))
    (unless (top-level-offset m2 -1)
      (editor-error "Must be inside a DEFUN."))
    ;;
    ;; Check for DEFUN.
    (mark-after (move-mark m1 m2))
    (unless (find-attribute m1 :whitespace #'zerop)
      (editor-error "Must be inside a DEFUN."))
    (word-offset (move-mark m2 m1) 1)
    (unless (string-equal (region-to-string (region m1 m2)) "defun")
      (editor-error "Must be inside a DEFUN."))
    ;;
    ;; Find name.
    (unless (find-attribute m2 :whitespace #'zerop)
      (editor-error "Function unnamed?"))
    (form-offset (move-mark m1 m2) 1)
    (region-to-string (region m2 m1))))



;;;; Miscellaneous commands.

(define-debugger-command "Flush Errors"
  "In the \"Current Eval Server\", toggles whether the debugger ignores errors
   or recursively enters itself."
  :flush)
