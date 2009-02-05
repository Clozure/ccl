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
;;; Totally rewritten for Clozure CL.

(in-package :hemlock-internals)

(defmacro modifying-echo-buffer (&body body)
  `(modifying-buffer-storage ((hemlock-echo-area-buffer *current-view*))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Echo area output.

(defvar *last-message-time* (get-internal-real-time))

(defun clear-echo-area ()
  "You guessed it."
  (modifying-echo-buffer
   (delete-region (buffer-region *current-buffer*))))

;;; Message  --  Public
;;;
;;;    Display the stuff on *echo-area-stream* 
;;;
(defun message (string &rest args)
  "Nicely display a message in the echo-area.
  String and Args are a format control string and format arguments, respectively."
  ;; TODO: used to do something cleverish if in the middle of reading prompted input, might
  ;; want to address that.
  (if *current-view*
    (let ((message (apply #'format nil string args)))
      (modifying-echo-buffer
       (delete-region (buffer-region *current-buffer*))
       (insert-string (buffer-point *current-buffer*) message)
       (setq *last-message-time* (get-internal-real-time))
       ))
    ;; For some reason this crashes.  Perhaps something is too aggressive about
    ;; catching conditions in events??
    #+not-yet(apply #'warn string args)
    #-not-yet (apply #'format t string args)))

;;; LOUD-MESSAGE -- Public.
;;;
;;; Like message, only more provocative.
;;;
(defun loud-message (&rest args)
  "This is the same as MESSAGE, but it beeps and clears the echo area before
   doing anything else."
  (beep)
  (apply #'message args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Echo area input

(defmode "Echo Area" :major-p t)

(defstruct (echo-parse-state (:conc-name "EPS-"))
  (parse-verification-function nil)
  (parse-string-tables ())
  (parse-value-must-exist ())
  ;; When the user attempts to default a parse, we call the verification function
  ;; on this string.  This is not the :Default argument to the prompting function,
  ;; but rather a string representation of it.
  (parse-default ())
  ;; String that we show the user to inform him of the default.  If this
  ;; is NIL then we just use Parse-Default.
  (parse-default-string ())
  ;; Prompt for the current parse.
  (parse-prompt ())
  ;; Help string for the current parse.
  (parse-help ())
  ;; :String, :File or :Keyword.
  (parse-type :string)
  ;; input region
  parse-starting-mark
  parse-input-region
  ;; key handler, nil to use the standard one
  (parse-key-handler nil)
  ;; Store result here
  (parse-results ()))

(defun current-echo-parse-state (&key (must-exist t))
  (or (hemlock-prompted-input-state *current-view*)
      (and must-exist (error "Can't do that when not in echo area input"))))


;;;; DISPLAY-PROMPT-NICELY and PARSE-FOR-SOMETHING.

(defun display-prompt-nicely (eps &optional (prompt (eps-parse-prompt eps))
				            (default (or (eps-parse-default-string eps)
							 (eps-parse-default eps))))
  (modifying-echo-buffer 
   (let* ((buffer *current-buffer*)
	  (point (buffer-point buffer)))
     (delete-region (buffer-region buffer))
     (insert-string point (if (listp prompt)
			    (apply #'format nil prompt)
			    prompt))
     (when default
       (insert-character point #\[)
       (insert-string point default)
       (insert-string point "] "))
     (move-mark (eps-parse-starting-mark eps) point))))

;; This is used to prevent multiple buffers trying to do echo area input
;; at the same time - there would be no way to exit the earlier one
;; without exiting the later one, because they're both on the same stack.
(defvar *recursive-edit-view* nil)

(defun parse-for-something (&key verification-function
				 type
				 string-tables
				 value-must-exist
				 default-string
				 default
				 prompt
				 help
                                 key-handler)
  ;; We can't do a "recursive" edit in more than one view, because if the earlier
  ;; one wants to exit first, we'd have to unwind the stack to allow it to exit,
  ;; which would force the later one to exit whether it wants to or not.
  (when (and *recursive-edit-view* (not (eq *recursive-edit-view* *current-view*)))
    (editor-error "~s is already waiting for input"
		  (buffer-name (hemlock-view-buffer *recursive-edit-view*))))
  (modifying-echo-buffer
   (let* ((view *current-view*)
	  (buffer *current-buffer*)
	  (old-eps (hemlock-prompted-input-state view))
	  (parse-mark (copy-mark (buffer-point buffer) :right-inserting))
	  (end-mark (buffer-end-mark buffer))
	  (eps (make-echo-parse-state
		:parse-starting-mark parse-mark
		:parse-input-region (region parse-mark end-mark)
		:parse-verification-function verification-function
		:parse-type type
		:parse-string-tables string-tables
		:parse-value-must-exist value-must-exist
		:parse-default-string default-string
		:parse-default default
		:parse-prompt prompt
		:parse-help help
                :parse-key-handler key-handler)))
     ;; TODO: There is really no good reason to disallow recursive edits in the same
     ;; buffer, I'm just too lazy.  Should save contents, starting mark, and point,
     ;; and restore them at the end.
     (when old-eps
       (editor-error "Attempt to recursively use echo area"))
     (display-prompt-nicely eps)
     (modifying-buffer-storage (nil)
       (unwind-protect
	    (let ((*recursive-edit-view* view))
	      (setf (hemlock-prompted-input-state view) eps)
	      (unless old-eps
		(hemlock-ext:change-active-pane view :echo))
	      (with-standard-standard-output
		  (gui::event-loop #'(lambda () (eps-parse-results eps))))
	      #+debug (log-debug "~&Event loop exited!, results = ~s" (eps-parse-results eps)))
	 (unless old-eps
	   (hemlock-ext:change-active-pane view :text))
	 (setf (hemlock-prompted-input-state view) old-eps)
	 (delete-mark parse-mark)))
     (let ((results (eps-parse-results eps)))
       (if (listp results)
	 (apply #'values results)
	 (abort-to-toplevel))))))

(defun exit-echo-parse (eps results)
  #+debug (log-debug "~&exit echo parse, results = ~s" results)
  ;; Must be set to non-nil to indicate parse done.
  (setf (eps-parse-results eps) (or results '(nil)))
  (gui::stop-event-loop) ;; this just marks it for dead then returns.
  ;; this exits current event, and since the event loop is stopped, it
  ;; will exit the event loop, which will return to parse-for-something,
  ;; which will notice we have the result set and will handle it accordingly.
  (exit-event-handler))

;;;; Buffer prompting.

(defun prompt-for-buffer (&key (must-exist t)
				default
				default-string
			       (prompt "Buffer: ")
			       (help "Type a buffer name."))
  "Prompts for a buffer name and returns the corresponding buffer.  If
   :must-exist is nil, then return the input string.  This refuses to accept
   the empty string as input when no default is supplied.  :default-string
   may be used to supply a default buffer name even when :default is nil, but
   when :must-exist is non-nil, :default-string must be the name of an existing
   buffer."
  (when (and must-exist
	     (not default)
	     (not (getstring default-string *buffer-names*)))
    (error "Default-string must name an existing buffer when must-exist is non-nil -- ~S."
	   default-string))
  (parse-for-something
   :verification-function #'buffer-verification-function
   :type :keyword
   :string-tables (list *buffer-names*)
   :value-must-exist must-exist
   :default-string default-string
   :default (if default (buffer-name default) default-string)
   :prompt prompt
   :help help))

(defun buffer-verification-function (eps string)
  (declare (simple-string string))
  (modifying-echo-buffer
   (cond ((string= string "") nil)
         ((eps-parse-value-must-exist eps)
          (multiple-value-bind
              (prefix key value field ambig)
              (complete-string string (eps-parse-string-tables eps))
            (declare (ignore field))
            (ecase key
              (:none nil)
              ((:unique :complete)
               (list value))
              (:ambiguous
	       (let ((input-region (eps-parse-input-region eps)))
		 (delete-region input-region)
		 (insert-string (region-start input-region) prefix)
		 (let ((point (current-point)))
		   (move-mark point (region-start input-region))
		   (unless (character-offset point ambig)
		     (buffer-end point)))
		 nil)))))
         (t
          (list (or (getstring string *buffer-names*) string))))))



;;;; File Prompting.

(defun prompt-for-file (&key (must-exist t)
			     default
			     default-string
			     (prompt "Filename: ")
			     (help "Type a file name."))
  "Prompts for a filename."
  (parse-for-something
   :verification-function #'file-verification-function
   :type :file
   :string-tables nil
   :value-must-exist must-exist
   :default-string default-string
   :default (if default (namestring default))
   :prompt prompt
   :help help))

(defun file-verification-function (eps string)
  (let ((pn (pathname-or-lose eps string)))
    (if pn
	(let ((merge
	       (cond ((not (eps-parse-default eps)) nil)
		     ((ccl:directory-pathname-p pn)
		      (merge-pathnames pn (eps-parse-default eps)))
		     (t
		      (merge-pathnames pn
				       (or (directory-namestring
					    (eps-parse-default eps))
					   ""))))))
	  (cond ((probe-file pn) (list pn))
		((and merge (probe-file merge)) (list merge))
		((not (eps-parse-value-must-exist eps)) (list (or merge pn)))
		(t nil))))))

;;; PATHNAME-OR-LOSE tries to convert string to a pathname using
;;; PARSE-NAMESTRING.  If it succeeds, this returns the pathname.  Otherwise,
;;; this deletes the offending characters from *parse-input-region* and signals
;;; an editor-error.
;;;
(defun pathname-or-lose (eps string)
  (multiple-value-bind (pn idx)
		       (parse-namestring string nil *default-pathname-defaults*
					 :junk-allowed t)
    (cond (pn)
	  (t (modifying-echo-buffer
              (delete-characters (region-end (eps-parse-input-region eps))
				 (- idx (length string))))
	     nil))))



;;;; Keyword and variable prompting.

(defun prompt-for-keyword (&key
			   tables
			   (must-exist t)
			   default
			   default-string
			   (prompt "Keyword: ")
			   (help "Type a keyword."))
  "Prompts for a keyword using the String Tables."
  (parse-for-something
   :verification-function #'keyword-verification-function
   :type :keyword
   :string-tables tables
   :value-must-exist must-exist
   :default-string default-string
   :default default
   :prompt prompt
   :help help))



(defun prompt-for-variable (&key (must-exist t)
				 default
				 default-string
				 (prompt "Variable: ")
				 (help "Type the name of a variable."))
  "Prompts for a variable defined in the current scheme of things."
  (parse-for-something
   :verification-function  #'keyword-verification-function
   :type :keyword
   :string-tables (current-variable-tables)
   :value-must-exist must-exist
   :default-string default-string
   :default default
   :prompt prompt
   :help help))

(defun current-variable-tables ()
  "Returns a list of all the variable tables currently established globally,
   by the current buffer, and by any modes for the current buffer."
  (nconc (list (buffer-variables *current-buffer*))
         (mapcar #'mode-object-variables (buffer-minor-mode-objects *current-buffer*))
         (list (mode-object-variables (buffer-major-mode-object *current-buffer*)))
         (list *global-variable-names*)))

(defun keyword-verification-function (eps string)
  (declare (simple-string string))
  (multiple-value-bind
      (prefix key value field ambig)
      (complete-string string (eps-parse-string-tables eps))
    (declare (ignore field))
    (modifying-echo-buffer
     (cond ((eps-parse-value-must-exist eps)
            (ecase key
              (:none nil)
              ((:unique :complete)
               (list prefix value))
              (:ambiguous
	       (let ((input-region (eps-parse-input-region eps)))
		 (delete-region input-region)
		 (insert-string (region-start input-region) prefix)
		 (let ((point (current-point)))
		   (move-mark point (region-start input-region))
		   (unless (character-offset point ambig)
		     (buffer-end point)))
		 nil))))
           (t
            ;; HACK: If it doesn't have to exist, and the completion does not
            ;; add anything, then return the completion's capitalization,
            ;; instead of the user's input.
            (list (if (= (length string) (length prefix)) prefix string)))))))



;;;; Integer, expression, and string prompting.

(defun prompt-for-integer (&key (must-exist t)
				default
				default-string
				(prompt "Integer: ")
				(help "Type an integer."))
  "Prompt for an integer.  If :must-exist is Nil, then we return as a string
  whatever was input if it is not a valid integer."

  (parse-for-something
   :verification-function #'(lambda (eps string)
			      (let ((number (parse-integer string  :junk-allowed t)))
				(if (eps-parse-value-must-exist eps)
				  (if number (list number))
				  (list (or number string)))))
   :type :string
   :string-tables nil
   :value-must-exist must-exist
   :default-string default-string
   :default (if default (write-to-string default :base 10))
   :prompt prompt
   :help help))


(defvar hemlock-eof '(())
  "An object that won't be EQ to anything read.")

(defun prompt-for-expression (&key (must-exist t)
				   (default nil defaultp)
				   default-string
				   (prompt "Expression: ")
				   (help "Type a Lisp expression."))
  "Prompts for a Lisp expression."
  (parse-for-something
   :verification-function #'(lambda (eps string)
			      (let* ((input-region (eps-parse-input-region eps))
				     (expr (with-input-from-region (stream input-region)
					     (handler-case (read stream nil hemlock-eof)
					       (error () hemlock-eof)))))
				(if (eq expr hemlock-eof)
				  (unless (eps-parse-value-must-exist eps) (list string))
				  (values (list expr) t))))
   :type :string
   :string-tables nil
   :value-must-exist must-exist
   :default-string default-string
   :default (if defaultp (prin1-to-string default))
   :prompt prompt
   :help help))


(defun prompt-for-string (&key default
			       default-string
			       (trim ())
			       (prompt "String: ")
			       (help "Type a string."))
  "Prompts for a string.  If :trim is t, then leading and trailing whitespace
   is removed from input, otherwise it is interpreted as a Char-Bag argument
   to String-Trim."
  (when (eq trim t) (setq trim '(#\space #\tab)))
  (parse-for-something
   :verification-function #'(lambda (eps string)
			      (declare (ignore eps))
			      (list (string-trim trim string)))
   :type :string
   :string-tables nil
   :value-must-exist nil
   :default-string default-string
   :default default
   :prompt prompt
   :help help))


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
(defun prompt-for-package (&key (must-exist t)
				(default nil defaultp)
				default-string
				(prompt "Package Name:")
				(help "Type a package name."))
)
||#


;;;; Yes-or-no and y-or-n prompting.

(defvar *yes-or-no-string-table*
  (make-string-table :initial-contents '(("Yes" . t) ("No" . nil))))

(defun prompt-for-yes-or-no (&key (must-exist t)
				  (default nil defaultp)
				  default-string
				  (prompt "Yes or No? ")
				  (help "Type Yes or No."))
  "Prompts for Yes or No."
  (parse-for-something
   :verification-function #'(lambda (eps string)
			      (multiple-value-bind
				  (prefix key value field ambig)
				  (complete-string string (eps-parse-string-tables eps))
				(declare (ignore prefix field ambig))
				(let ((won (or (eq key :complete) (eq key :unique))))
				  (if (eps-parse-value-must-exist eps)
				    (if won (values (list value) t))
				    (list (if won (values value t) string))))))
   :type :keyword
   :string-tables (list *yes-or-no-string-table*)
   :value-must-exist must-exist
   :default-string default-string
   :default (if defaultp (if default "Yes" "No"))
   :prompt prompt
   :help help))

(defun prompt-for-y-or-n (&key (must-exist t)
			       (default nil defaultp)
			       default-string
			       (prompt "Y or N? ")
			       (help "Type Y or N."))
  "Prompts for Y or N."
  (parse-for-something
   :verification-function #'(lambda (eps key-event)
                              (cond ((logical-key-event-p key-event :y)
                                     (values (list t) t))
                                    ((logical-key-event-p key-event :n)
                                     (values (list nil) t))
                                    ((and (eps-parse-default eps)
                                          (logical-key-event-p key-event :confirm))
                                     (values (list (equalp (eps-parse-default eps) "y")) t))
                                    ((logical-key-event-p key-event :abort)
                                     :abort)
                                    ((logical-key-event-p key-event :help)
                                     :help)
                                    (t
                                     (if (eps-parse-value-must-exist eps)
                                       :error
                                       (values (list key-event) t)))))
   :type :key
   :value-must-exist must-exist
   :default-string default-string
   :default (and defaultp (if default "Y" "N"))
   :prompt prompt
   :help help
   :key-handler (getstring "Key Input Handler" *command-names*)))



;;;; Key-event and key prompting.

(defun prompt-for-key-event (&key (prompt "Key-event: ")
                                  (help "Type any key"))
  "Prompts for a key-event."
  (parse-for-something
   :verification-function #'(lambda (eps key-event)
                              (declare (ignore eps))
                              (values (list key-event) t))
   :type :key
   :prompt prompt
   :help help
   :key-handler (getstring "Key Input Handler" *command-names*)))

(defun verify-key (eps key-event key quote-p)
  ;; This is called with the echo buffer as the current buffer.  We want to look
  ;; up the commands in the main buffer.
  (let* ((buffer (hemlock-view-buffer (current-view)))
         (n (length key)))
    (block nil
      (unless quote-p
	(cond ((logical-key-event-p key-event :help)
	       (return :help))
	      ((logical-key-event-p key-event :abort)
	       (return :abort))
	      ((and (not (eps-parse-value-must-exist eps))
		    (logical-key-event-p key-event :confirm))
	       (return
		 (cond ((eql n 0)
			(let ((key (eps-parse-default eps))
			      (cmd (and key (let ((*current-buffer* buffer))
					      (get-command key :current)))))
			  (if (commandp cmd)
			    (values (list key cmd) :confirmed)
			    :error)))
		       ((> n 0)
			(values (list key nil) :confirmed))
		       (t :error))))))
      (vector-push-extend key-event key)
      (let ((cmd (if (eps-parse-value-must-exist eps)
                   (let ((*current-buffer* buffer)) (get-command key :current))
                   :prefix)))
        (cond ((commandp cmd)
               (values (list key cmd) t))
              ((eq cmd :prefix)
               nil)
              (t
               (vector-pop key)
               :error))))))

(defun prompt-for-key (&key (prompt "Key: ")
                            (help "Type a key.")
                            default default-string
                            (must-exist t))
  (parse-for-something
   :verification-function (let ((key (make-array 10 :adjustable t :fill-pointer 0))
				(quote-p nil))
                            #'(lambda (eps key-event)
				(if (and (not quote-p) (logical-key-event-p key-event :quote))
				  (progn
				    (setq quote-p t)
				    (values :ignore nil))
				  (verify-key eps key-event key (shiftf quote-p nil)))))
   :type :command
   :prompt prompt
   :help help
   :value-must-exist must-exist
   :default default
   :default-string default-string
   :key-handler (getstring "Key Input Handler" *command-names*)))


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

(define-logical-key-event "Abort"
  "This key-event is used to abort the command in progress.")
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
(define-logical-key-event "y"
  "This key-event is used to indicate a short positive response.")
(define-logical-key-event "n"
  "This key-event is used to indicate a short negative response.")


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
            (write-string (pretty-key-string (car key-events)) s)
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
