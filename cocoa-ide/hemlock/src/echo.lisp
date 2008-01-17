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
;;; Hemlock Echo Area stuff.
;;; Written by Skef Wholey and Rob MacLachlan.
;;; Modified by Bill Chiles.
;;;
(in-package :hemlock-internals)

(defmode "Echo Area" :major-p t)
(defvar *echo-area-buffer* (make-buffer "Echo Area" :modes '("Echo Area"))
  "Buffer used to hack text for the echo area.")
(defvar *echo-area-region* (buffer-region *echo-area-buffer*)
  "Internal thing that's the *echo-area-buffer*'s region.")
(defvar *echo-area-stream*
  (make-hemlock-output-stream (region-end *echo-area-region*) :full)
  "Buffered stream that prints into the echo area.")
(defvar *echo-area-window* ()
  "Window used to display stuff in the echo area.")
(defvar *parse-starting-mark*
  (copy-mark (buffer-point *echo-area-buffer*) :right-inserting)
  "Mark that points to the beginning of the text that'll be parsed.")
(defvar *parse-input-region*
  (region *parse-starting-mark* (region-end *echo-area-region*))
  "Region that contains the text typed in.")



;;;; Variables that control parsing:

(defvar *parse-verification-function* '%not-inside-a-parse
  "Function that verifies what's being parsed.")

(defmacro modifying-echo-buffer (&body body)
  `(unwind-protect
    (progn
      (buffer-document-begin-editing *echo-area-buffer*)
      (modifying-buffer *echo-area-buffer* ,@body))
    (buffer-document-end-editing *echo-area-buffer*)))
;;; %Not-Inside-A-Parse  --  Internal
;;;
;;;    This function is called if someone does stuff in the echo area when
;;; we aren't inside a parse.  It tries to put them back in a reasonable place.
;;;
(defun %not-inside-a-parse (quaz)
  "Thing that's called when somehow we get called to confirm a parse that's
  not in progress."
  (declare (ignore quaz))
  (let* ((bufs (remove *echo-area-buffer* *buffer-list*))
	 (buf (or (find-if #'buffer-windows bufs)
		  (car bufs)
		  (make-buffer "Main"))))
    (setf (current-buffer) buf)
    (dolist (w *window-list*)
      (when (and (eq (window-buffer w) *echo-area-buffer*)
		 (not (eq w *echo-area-window*)))
	(setf (window-buffer w) buf)))
    (setf (current-window)
	  (or (car (buffer-windows buf))
	      (make-window (buffer-start-mark buf)))))
  (editor-error "Wham!  We tried to confirm a parse that wasn't in progress?"))

(defvar *parse-string-tables* ()
  "String tables being used in the current parse.")

(defvar *parse-value-must-exist* ()
  "You know.")

(defvar *parse-default* ()
  "When the user attempts to default a parse, we call the verification function
  on this string.  This is not the :Default argument to the prompting function,
  but rather a string representation of it.")

(defvar *parse-default-string* ()
  "String that we show the user to inform him of the default.  If this
  is NIL then we just use *Parse-Default*.")

(defvar *parse-prompt* ()
  "Prompt for the current parse.")

(defvar *parse-help* ()
  "Help string for the current parse.")

(defvar *parse-type* :string "A hack. :String, :File or :Keyword.") 



;;;; MESSAGE and CLEAR-ECHO-AREA:

(defhvar "Message Pause" "The number of seconds to pause after a Message."
  :value 0.0s0)

(defvar *last-message-time* 0
  "Internal-Real-Time the last time we displayed a message.") 

(defun maybe-wait ()
  (let* ((now (get-internal-real-time))
	 (delta (/ (float (- now *last-message-time*))
		   (float internal-time-units-per-second)))
	 (pause (value hemlock::message-pause)))
    (when (< delta pause)
      (sleep (- pause delta)))))

(defun clear-echo-area ()
  "You guessed it."
  ;;(maybe-wait)
  (let* ((b (current-buffer)))
    (unwind-protect
	 (progn
	   (setf (current-buffer) *echo-area-buffer*)
	   (modifying-echo-buffer
            (delete-region *echo-area-region*))
	   (setf (buffer-modified *echo-area-buffer*) nil))
      (setf (current-buffer) b))))

;;; Message  --  Public
;;;
;;;    Display the stuff on *echo-area-stream* and then wait.  Editor-Sleep
;;; will do a redisplay if appropriate.
;;;
(defun message (string &rest args)
  "Nicely display a message in the echo-area.
  Put the message on a fresh line and wait for \"Message Pause\" seconds
  to give the luser a chance to see it.  String and Args are a format 
  control string and format arguments, respectively."
  ;(maybe-wait)
  (modifying-echo-buffer
   (cond ((eq *current-window* *echo-area-window*)
          (let ((point (buffer-point *echo-area-buffer*)))
            (with-mark ((m point :left-inserting))
              (line-start m)
              (with-output-to-mark (s m :full)
                (apply #'format s string args)
                (fresh-line s)))))
         (t
          (let ((mark (region-end *echo-area-region*)))
            (cond ((buffer-modified *echo-area-buffer*)
                   (clear-echo-area))
                  ((not (zerop (mark-charpos mark)))
                   (insert-character mark #\newline)
                   (clear-echo-area)))
            (write-string (apply #'format nil string args)
                          *echo-area-stream*)
            ;; keep command loop from clearing the echo area,
            ;; by asserting that the echo area buffer's unmodified.
            (setf (buffer-modified *echo-area-buffer*) t))))
   (force-output *echo-area-stream*)
   (setq *last-message-time* (get-internal-real-time)))
  nil)


;;; LOUD-MESSAGE -- Public.
;;;
;;; Like message, only more provocative.
;;;
(defun loud-message (&rest args)
  "This is the same as MESSAGE, but it beeps and clears the echo area before
   doing anything else."
  (beep)
  (clear-echo-area)
  (apply #'message args))







;;;; DISPLAY-PROMPT-NICELY and PARSE-FOR-SOMETHING.

(defun display-prompt-nicely (&optional (prompt *parse-prompt*)
					(default (or *parse-default-string*
						     *parse-default*)))
  (clear-echo-area)
  (modifying-echo-buffer 
   (let ((point (buffer-point *echo-area-buffer*)))
     (if (listp prompt)
       (apply #'format *echo-area-stream* prompt)
       (insert-string point prompt))
     (when default
       (insert-character point #\[)
       (insert-string point default)
       (insert-string point "] ")))))

(defun parse-for-something ()
  (display-prompt-nicely)
  (let ((start-window (current-window)))
    (move-mark *parse-starting-mark* (buffer-point *echo-area-buffer*))
    (setf (current-window) *echo-area-window*)
    (unwind-protect
     (use-buffer *echo-area-buffer*
       (recursive-edit nil))
      
     (setf (current-window) start-window))))



;;;; Buffer prompting.

(defun prompt-for-buffer (&key ((:must-exist *parse-value-must-exist*) t)
			       default
			       ((:default-string *parse-default-string*))
			       ((:prompt *parse-prompt*) "Buffer: ")
			       ((:help *parse-help*) "Type a buffer name."))
  "Prompts for a buffer name and returns the corresponding buffer.  If
   :must-exist is nil, then return the input string.  This refuses to accept
   the empty string as input when no default is supplied.  :default-string
   may be used to supply a default buffer name even when :default is nil, but
   when :must-exist is non-nil, :default-string must be the name of an existing
   buffer."
    (let ((*parse-string-tables* (list *buffer-names*))
	  (*parse-type* :keyword)
	  (*parse-default* (cond
			    (default (buffer-name default))
			    (*parse-default-string*
			     (when (and *parse-value-must-exist*
					(not (getstring *parse-default-string*
							*buffer-names*)))
			       (error "Default-string must name an existing ~
				       buffer when must-exist is non-nil -- ~S."
				      *parse-default-string*))
			     *parse-default-string*)
			    (t nil)))
	  (*parse-verification-function* #'buffer-verification-function))
      (parse-for-something)))

(defun buffer-verification-function (string)
  (declare (simple-string string))
  (modifying-echo-buffer
   (cond ((string= string "") nil)
         (*parse-value-must-exist*
          (multiple-value-bind
              (prefix key value field ambig)
              (complete-string string *parse-string-tables*)
            (declare (ignore field))
            (ecase key
              (:none nil)
              ((:unique :complete)
               (list value))
              (:ambiguous
               (delete-region *parse-input-region*)
               (insert-string (region-start *parse-input-region*) prefix)
               (let ((point (current-point)))
                 (move-mark point (region-start *parse-input-region*))
                 (unless (character-offset point ambig)
                   (buffer-end point)))
               nil))))
         (t
          (list (or (getstring string *buffer-names*) string))))))



;;;; File Prompting.

(defun prompt-for-file (&key ((:must-exist *parse-value-must-exist*) t)
			     default
			     ((:default-string *parse-default-string*))
			     ((:prompt *parse-prompt*) "Filename: ")
			     ((:help *parse-help*) "Type a file name."))
  "Prompts for a filename."
  (let ((*parse-verification-function* #'file-verification-function)
	(*parse-default* (if default (namestring default)))
	(*parse-type* :file))
    (parse-for-something)))

(defun file-verification-function (string)
  (let ((pn (pathname-or-lose string)))
    (if pn
	(let ((merge
	       (cond ((not *parse-default*) nil)
		     ((directoryp pn)
		      (merge-pathnames pn *parse-default*))
		     (t
		      (merge-pathnames pn
				       (or (directory-namestring
					    *parse-default*)
					   ""))))))
	  (cond ((probe-file pn) (list pn))
		((and merge (probe-file merge)) (list merge))
		((not *parse-value-must-exist*) (list (or merge pn)))
		(t nil))))))

;;; PATHNAME-OR-LOSE tries to convert string to a pathname using
;;; PARSE-NAMESTRING.  If it succeeds, this returns the pathname.  Otherwise,
;;; this deletes the offending characters from *parse-input-region* and signals
;;; an editor-error.
;;;
(defun pathname-or-lose (string)
  (declare (simple-string string))
  (multiple-value-bind (pn idx)
		       (parse-namestring string nil *default-pathname-defaults*
					 :junk-allowed t)
    (cond (pn)
	  (t (modifying-echo-buffer
              (delete-characters (region-end *echo-area-region*)
				(- idx (length string))))
	     nil))))



;;;; Keyword and variable prompting.

(defun prompt-for-keyword (*parse-string-tables* 
			   &key
			   ((:must-exist *parse-value-must-exist*) t)
			   ((:default *parse-default*))
			   ((:default-string *parse-default-string*))
			   ((:prompt *parse-prompt*) "Keyword: ")
			   ((:help *parse-help*) "Type a keyword."))
  "Prompts for a keyword using the String Tables."
  (let ((*parse-verification-function* #'keyword-verification-function)
	(*parse-type* :keyword))
    (parse-for-something)))

(defun prompt-for-variable (&key ((:must-exist *parse-value-must-exist*) t)
				 ((:default *parse-default*))
				 ((:default-string *parse-default-string*))
				 ((:prompt *parse-prompt*) "Variable: ")
				 ((:help *parse-help*)
				  "Type the name of a variable."))
  "Prompts for a variable defined in the current scheme of things."
  (let ((*parse-string-tables* (current-variable-tables))
	(*parse-verification-function* #'keyword-verification-function)
	(*parse-type* :keyword))
    (parse-for-something)))

(defun current-variable-tables ()
  "Returns a list of all the variable tables currently established globally,
   by the current buffer, and by any modes for the current buffer."
  (do ((tables (list (buffer-variables *current-buffer*)
		     *global-variable-names*)
	       (cons (mode-object-variables (car mode)) tables))
       (mode (buffer-mode-objects *current-buffer*) (cdr mode)))
      ((null mode) tables)))

(defun keyword-verification-function (string)
  (declare (simple-string string))
  (multiple-value-bind
      (prefix key value field ambig)
      (complete-string string *parse-string-tables*)
    (declare (ignore field))
    (modifying-echo-buffer
     (cond (*parse-value-must-exist*
            (ecase key
              (:none nil)
              ((:unique :complete)
               (list prefix value))
              (:ambiguous
               (delete-region *parse-input-region*)
               (insert-string (region-start *parse-input-region*) prefix)
               (let ((point (current-point)))
                 (move-mark point (region-start *parse-input-region*))
                 (unless (character-offset point ambig)
                   (buffer-end point)))
               nil)))
           (t
            ;; HACK: If it doesn't have to exist, and the completion does not
            ;; add anything, then return the completion's capitalization,
            ;; instead of the user's input.
            (list (if (= (length string) (length prefix)) prefix string)))))))



;;;; Integer, expression, and string prompting.

(defun prompt-for-integer (&key ((:must-exist *parse-value-must-exist*) t)
				default
				((:default-string *parse-default-string*))
				((:prompt *parse-prompt*) "Integer: ")
				((:help *parse-help*) "Type an integer."))
  "Prompt for an integer.  If :must-exist is Nil, then we return as a string
  whatever was input if it is not a valid integer."
  (let ((*parse-verification-function*
	 #'(lambda (string)
	     (let ((number (parse-integer string  :junk-allowed t)))
	       (if *parse-value-must-exist*
		   (if number (list number))
		   (list (or number string))))))
	(*parse-default* (if default (write-to-string default :base 10))))
    (parse-for-something)))


(defvar hemlock-eof '(())
  "An object that won't be EQ to anything read.")

(defun prompt-for-expression (&key ((:must-exist *parse-value-must-exist*) t)
				   (default nil defaultp)
				   ((:default-string *parse-default-string*))
				   ((:prompt *parse-prompt*) "Expression: ")
				   ((:help *parse-help*)
				    "Type a Lisp expression."))
  "Prompts for a Lisp expression."
  (let ((*parse-verification-function*
         #'(lambda (string)
	     (let ((expr (with-input-from-region (stream *parse-input-region*)
			   (handler-case (read stream nil hemlock-eof)
			     (error () hemlock-eof)))))
	       (if *parse-value-must-exist*
		   (if (not (eq expr hemlock-eof)) (values (list expr) t))
		   (if (eq expr hemlock-eof)
		       (list string) (values (list expr) t))))))
	(*parse-default* (if defaultp (prin1-to-string default))))
      (parse-for-something)))


(defun prompt-for-string (&key ((:default *parse-default*))
			       ((:default-string *parse-default-string*))
			       (trim ())
			       ((:prompt *parse-prompt*) "String: ")
			       ((:help *parse-help*) "Type a string."))
  "Prompts for a string.  If :trim is t, then leading and trailing whitespace
   is removed from input, otherwise it is interpreted as a Char-Bag argument
   to String-Trim."
  (let ((*parse-verification-function*
	 #'(lambda (string)
	     (list (string-trim (if (eq trim t) '(#\space #\tab) trim)
				string)))))
    (parse-for-something)))



;;;; Package names.
(defun make-package-string-table ()
  (let ((names ()))
    (dolist (p (list-all-packages))
      (let* ((name (package-name p)))
        (push (cons name name) names)
        (dolist (nick (package-nicknames p))
          (push (cons nick name) names))))
    (make-string-table :initial-contents names)))

#||
(defun prompt-for-package (&key ((:must-exist *parse-value-must-exist*) t)
				  (default nil defaultp)
				  ((:default-string *parse-default-string*))
				  ((:prompt *parse-prompt*) "Package Name:")
				  ((:help *parse-help*) "Type a package name."))
||#


;;;; Yes-or-no and y-or-n prompting.

(defvar *yes-or-no-string-table*
  (make-string-table :initial-contents '(("Yes" . t) ("No" . nil))))

(defun prompt-for-yes-or-no (&key ((:must-exist *parse-value-must-exist*) t)
				  (default nil defaultp)
				  ((:default-string *parse-default-string*))
				  ((:prompt *parse-prompt*) "Yes or No? ")
				  ((:help *parse-help*) "Type Yes or No."))
  "Prompts for Yes or No."
  (let* ((*parse-string-tables* (list *yes-or-no-string-table*))
	 (*parse-default* (if defaultp (if default "Yes" "No")))
	 (*parse-verification-function*
	  #'(lambda (string)
	      (multiple-value-bind
		  (prefix key value field ambig)
		  (complete-string string *parse-string-tables*)
		(declare (ignore prefix field ambig))
		(let ((won (or (eq key :complete) (eq key :unique))))
		  (if *parse-value-must-exist*
		      (if won (values (list value) t))
		      (list (if won (values value t) string)))))))
	 (*parse-type* :keyword))
    (parse-for-something)))

(defun prompt-for-y-or-n (&key ((:must-exist must-exist) t)
			       (default nil defaultp)
			       default-string
			       ((:prompt prompt) "Y or N? ")
			       ((:help *parse-help*) "Type Y or N."))
  "Prompts for Y or N."
  (let ((old-window (current-window)))
    (unwind-protect
	(progn
	  (setf (current-window) *echo-area-window*)
	  (display-prompt-nicely prompt (or default-string
					    (if defaultp (if default "Y" "N"))))
	  (loop
	    (let ((key-event (recursive-get-key-event *editor-input*)))
	      (cond ((or (eq key-event #k"y")
			 (eq key-event #k"Y"))
		     (return t))
		    ((or (eq key-event #k"n")
			 (eq key-event #k"N"))
		     (return nil))
		    ((logical-key-event-p key-event :confirm)
		     (if defaultp
			 (return default)
			 (beep)))
		    ((logical-key-event-p key-event :help)
		     (hemlock::help-on-parse-command ()))
		    (t
		     (unless must-exist (return key-event))
		     (beep))))))
      (setf (current-window) old-window))))



;;;; Key-event and key prompting.

(defun prompt-for-key-event (&key (prompt "Key-event: ") (change-window t))
  "Prompts for a key-event."
  (prompt-for-key-event* prompt change-window))

(defun prompt-for-key-event* (prompt change-window)
  (let ((old-window (current-window)))
    (unwind-protect
	(progn
	  (when change-window
	    (setf (current-window) *echo-area-window*))
	  (display-prompt-nicely prompt)
	  (recursive-get-key-event *editor-input* t))
      (when change-window (setf (current-window) old-window)))))

(defvar *prompt-key* (make-array 10 :adjustable t :fill-pointer 0))
(defun prompt-for-key (&key ((:must-exist must-exist) t)
			    default default-string
			    (prompt "Key: ")
			    ((:help *parse-help*) "Type a key."))
  (let ((old-window (current-window))
	(string (if default
		    (or default-string
			(let ((l (coerce default 'list)))
			  (format nil "~:C~{ ~:C~}" (car l) (cdr l)))))))

    (unwind-protect
	(progn
	  (setf (current-window) *echo-area-window*)
	  (display-prompt-nicely prompt string)
	  (setf (fill-pointer *prompt-key*) 0)
	  (prog ((key *prompt-key*) key-event)
		(declare (vector key))
		TOP
		(setf key-event (recursive-get-key-event *editor-input*))
		(cond ((logical-key-event-p key-event :quote)
		       (setf key-event (recursive-get-key-event *editor-input* t)))
		      ((logical-key-event-p key-event :confirm)
		       (cond ((and default (zerop (length key)))
			      (let ((res (get-command default :current)))
				(unless (commandp res) (go FLAME))
				(return (values default res))))
			     ((and (not must-exist) (plusp (length key)))
			      (return (copy-seq key)))
			     (t 
			      (go FLAME))))
		      ((logical-key-event-p key-event :help)
		       (hemlock::help-on-parse-command ())
		       (go TOP)))
		(vector-push-extend key-event key)	 
		(when must-exist
		  (let ((res (get-command key :current)))
		    (cond ((commandp res)
			   (hemlock-ext:print-pretty-key-event key-event
						       *echo-area-stream*
						       t)
			   (write-char #\space *echo-area-stream*)
			   (return (values (copy-seq key) res)))
			  ((not (eq res :prefix))
			   (vector-pop key)
			   (go FLAME)))))
		(hemlock-ext:print-pretty-key key-event *echo-area-stream* t)
		(write-char #\space *echo-area-stream*)
		(go TOP)
		FLAME
		(beep)
		(go TOP)))
      (force-output *echo-area-stream*)
      (setf (current-window) old-window))))



;;;; Logical key-event stuff.

(defvar *logical-key-event-names* (make-string-table)
  "This variable holds a string-table from logical-key-event names to the
   corresponding keywords.")

(defvar *real-to-logical-key-events* (make-hash-table :test #'eql)
  "A hashtable from real key-events to their corresponding logical
   key-event keywords.")

(defvar *logical-key-event-descriptors* (make-hash-table :test #'eq)
  "A hashtable from logical-key-events to logical-key-event-descriptors.")

(defstruct (logical-key-event-descriptor
	    (:constructor make-logical-key-event-descriptor ()))
  name
  key-events
  documentation)

;;; LOGICAL-KEY-EVENT-P  --  Public
;;;
(defun logical-key-event-p (key-event keyword)
  "Return true if key-event has been defined to have Keyword as its
   logical key-event.  The relation between logical and real key-events
   is defined by using SETF on LOGICAL-KEY-EVENT-P.  If it is set to
   true then calling LOGICAL-KEY-EVENT-P with the same key-event and
   Keyword, will result in truth.  Setting to false produces the opposite
   result.  See DEFINE-LOGICAL-KEY-EVENT and COMMAND-CASE."
  (not (null (member keyword (gethash key-event *real-to-logical-key-events*)))))

;;; GET-LOGICAL-KEY-EVENT-DESC  --  Internal
;;;
;;;    Return the descriptor for the logical key-event keyword, or signal
;;; an error if it isn't defined.
;;;
(defun get-logical-key-event-desc (keyword)
  (let ((res (gethash keyword *logical-key-event-descriptors*)))
    (unless res
      (error "~S is not a defined logical-key-event keyword." keyword))
    res))

;;; %SET-LOGICAL-KEY-EVENT-P  --  Internal
;;;
;;;    Add or remove a logical key-event link by adding to or deleting from
;;; the list in the from-char hashtable and the descriptor.
;;;
(defun %set-logical-key-event-p (key-event keyword new-value)
  (let ((entry (get-logical-key-event-desc keyword)))
    (cond
     (new-value
      (pushnew keyword (gethash key-event *real-to-logical-key-events*))
      (pushnew key-event (logical-key-event-descriptor-key-events entry)))
     (t
      (setf (gethash key-event *real-to-logical-key-events*)
	    (delete keyword (gethash key-event *real-to-logical-key-events*)))
      (setf (logical-key-event-descriptor-key-events entry)
	    (delete keyword (logical-key-event-descriptor-key-events entry))))))
  new-value)

;;; LOGICAL-KEY-EVENT-DOCUMENTATION, NAME, KEY-EVENTS  --  Public
;;;
;;;    Grab the right field out of the descriptor and return it.
;;;
(defun logical-key-event-documentation (keyword)
  "Return the documentation for the logical key-event Keyword."
  (logical-key-event-descriptor-documentation
   (get-logical-key-event-desc keyword)))
;;;
(defun logical-key-event-name (keyword)
  "Return the string name for the logical key-event Keyword."
  (logical-key-event-descriptor-name (get-logical-key-event-desc keyword)))
;;;
(defun logical-key-event-key-events (keyword)
  "Return the list of key-events for which Keyword is the logical key-event."
  (logical-key-event-descriptor-key-events
   (get-logical-key-event-desc keyword)))

;;; DEFINE-LOGICAL-KEY-EVENT  --  Public
;;;
;;;    Make the entries in the two hashtables and the string-table.
;;;
(defun define-logical-key-event (name documentation)
  "Define a logical key-event having the specified Name and Documentation.
  See LOGICAL-KEY-EVENT-P and COMMAND-CASE."
  (check-type name string)
  (check-type documentation (or string function))
  (let* ((keyword (string-to-keyword name))
	 (entry (or (gethash keyword *logical-key-event-descriptors*)
		    (setf (gethash keyword *logical-key-event-descriptors*)
			  (make-logical-key-event-descriptor)))))
    (setf (logical-key-event-descriptor-name entry) name)
    (setf (logical-key-event-descriptor-documentation entry) documentation)
    (setf (getstring name *logical-key-event-names*) keyword)))



;;;; Some standard logical-key-events:

(define-logical-key-event "Forward Search"
  "This key-event is used to indicate that a forward search should be made.")
(define-logical-key-event "Backward Search"
  "This key-event is used to indicate that a backward search should be made.")
(define-logical-key-event "Recursive Edit"
  "This key-event indicates that a recursive edit should be entered.")
(define-logical-key-event "Cancel"
  "This key-event is used  to cancel a previous key-event of input.")
(define-logical-key-event "Abort"
  "This key-event is used to abort the command in progress.")
(define-logical-key-event "Exit"
  "This key-event is used to exit normally the command in progress.")
(define-logical-key-event "Yes"
  "This key-event is used to indicate a positive response.")
(define-logical-key-event "No"
  "This key-event is used to indicate a negative response.")
(define-logical-key-event "Do All"
  "This key-event means do it as many times as you can.")
(define-logical-key-event "Do Once"
  "This key-event means, do it this time, then exit.")
(define-logical-key-event "Help"
  "This key-event is used to ask for help.")
(define-logical-key-event "Confirm"
  "This key-event is used to confirm some choice.")
(define-logical-key-event "Quote"
  "This key-event is used to quote the next key-event of input.")
(define-logical-key-event "Keep"
  "This key-event means exit but keep something around.")
(define-logical-key-event "Mouse Exit"
  "This key-event means exit completely.")
(define-logical-key-event "Extend Search Word"
  "This key-event means to extend the incremental search string by the word after the point")


;;;; COMMAND-CASE help message printing.

(defvar *my-string-output-stream* (make-string-output-stream))

(defun chars-to-string (chars)
  (do ((s *my-string-output-stream*)
       (chars chars (cdr chars)))
      ((null chars)
       (get-output-stream-string s))
    (let ((char (car chars)))
      (if (characterp char)
	  (write-char char s)
	  (do ((key-events
		(logical-key-event-key-events char)
		(cdr key-events)))
	      ((null key-events))
	    (hemlock-ext:print-pretty-key (car key-events) s)
	    (unless (null (cdr key-events))
	      (write-string ", " s))))
      (unless (null (cdr chars))
	(write-string ", " s)))))

;;; COMMAND-CASE-HELP  --  Internal
;;;
;;;    Print out a help message derived from the options in a
;;; random-typeout window.
;;;
(defun command-case-help (help options)
  (let ((help (if (listp help)
		  (apply #'format nil help) help)))
    (with-pop-up-display (s :title "Help")
      (write-string help s)
      (fresh-line s)
      (do ((o options (cdr o)))
	  ((null o))
	(let ((string (chars-to-string (caar o))))
	  (declare (simple-string string))
	  (if (= (length string) 1)
	      (write-char (char string 0) s)
	      (write-line string s))
	  (write-string "  - " s)
	  (write-line (cdar o) s))))))
