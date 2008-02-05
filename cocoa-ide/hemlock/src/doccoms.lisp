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
;;; Hemlock Documentation and Help commands.
;;; Written by Rob MacLachlan and Bill Chiles.
;;;

(in-package :hemlock)



;;;; Help.

(defcommand "Help" (p)
  "Give helpful information.
  This command dispatches to a number of other documentation commands,
  on the basis of a character command."
  "Prompt for a single character command to dispatch to another helping
  function."
  (declare (ignore p))
  (command-case (:prompt "Doc (Help for Help): "
		 :help "Type a Help option to say what kind of help you want:")
    (#\a "List all commands, variables and attributes Apropos a keyword."
     (apropos-command nil))
    (#\d "Describe a command, given its name."
     (describe-command-command nil))
    (#\g "Generic describe, any Hemlock thing (e.g., variables, keys, attributes)."
     (generic-describe-command nil))
    (#\v "Describe variable and show its values."
     (describe-and-show-variable-command nil))
    (#\c "Describe the command bound to a Character."
     (describe-key-command nil))
    (#\l "List the last 60 characters typed."
     (what-lossage-command nil))
    (#\m "Describe a mode."
     (describe-mode-command nil))
    ;(#\p "Describe commands with mouse/pointer bindings."
    ; (describe-pointer-command nil))
    (#\w "Find out Where a command is bound."
     (where-is-command nil))
    (#\t "Describe a Lisp object."
     (editor-describe-command nil))
    ((#\q :no) "Quits, You don't really want help.")))

(defcommand "Where Is" (p)
  "Find what key a command is bound to.
   Prompts for the command to look for, and says what environment it is
   available in."
  "List places where a command is bound."
  (declare (ignore p))
  (multiple-value-bind (nam cmd)
		       (prompt-for-keyword :tables (list *command-names*)
					   :prompt "Command: "
					   :help "Name of command to look for.")
    (let ((bindings (command-bindings cmd)))
      (with-pop-up-display (s :title (format nil "Bindings of ~s" nam))
	(cond
	 ((null bindings)
	  (format s "~S may only be invoked as an extended command.~%" nam))
	 (t
	  (format s "~S may be invoked in the following ways:~%" nam)
	  (print-command-bindings bindings s)))))))



;;;; Apropos.

(defcommand "Apropos" (p)
  "List things whose names contain a keyword."
  "List things whose names contain a keyword."
  (declare (ignore p))
  (let* ((str (prompt-for-string
		:prompt "Apropos keyword: "
		:help
 "String to look for in command, variable and attribute names."))
	 (coms (find-containing str *command-names*))
	 (vars (mapcar #'(lambda (table)
			   (let ((res (find-containing str table)))
			     (if res (cons table res))))
		       (current-variable-tables)))
	 (attr (find-containing str *character-attribute-names*)))
    (if (or coms vars attr)
      (apropos-command-output str coms vars attr)
      (message "No command, attribute or variable name contains ~S." str))))

(defun apropos-command-output (str coms vars attr)
  (declare (list coms vars attr))
  (with-pop-up-display (s :title "Apropos Output")
    (when coms
      (format s "Commands with ~S in their names:~%" str)
      (dolist (com coms)
	(let ((obj (getstring com *command-names*)))
	  (write-string com s)
	  (write-string "   " s)
	  (print-command-bindings (command-bindings obj) s)
	  (terpri s)
	  (print-doc (command-documentation obj) s))))
    (when vars
      (when coms (terpri s))
      (format s "Variables with ~S in their names:~%" str)
      (dolist (stuff vars)
	(let ((table (car stuff)))
	  (dolist (var (cdr stuff))
	    (let ((obj (getstring var table)))
	      (write-string var s)
	      (write-string "   " s)
	      (let ((*print-level* 2) (*print-length* 3))
		(prin1 (variable-value obj) s))
	      (terpri s)
	      (print-doc (variable-documentation obj) s))))))
    (when attr
      (when (or coms vars) (terpri s))
      (format s "Attributes with ~S in their names:~%" str)
      (dolist (att attr)
	(let ((obj (getstring att *character-attribute-names*)))
	  (write-line att s)
	  (print-doc (character-attribute-documentation obj) s))))))

;;; PRINT-DOC takes doc, a function or string, and gets it out on stream.

(defun print-doc (doc stream)
  (let ((str (typecase doc
	       (function (funcall doc :short))
	       (simple-string doc)
	       (t
		(error "Bad documentation: ~S" doc)))))
    (write-string "  " stream)
    (write-line str stream)))



;;;; Describe command, key, pointer.

(defcommand "Describe Command" (p)
  "Describe a command.
  Prompts for a command and then prints out it's full documentation."
  "Print out the command documentation for a command which is prompted for."
  (declare (ignore p))
  (multiple-value-bind (nam com)
		       (prompt-for-keyword
			:tables (list *command-names*)
			:prompt "Describe command: "
			:help "Name of a command to document.")
    (let ((bindings (command-bindings com)))
      (with-pop-up-display (s :title (format nil "~s command documentation" nam))
	(format s "Documentation for ~S:~%   ~A~%"
		nam (command-documentation com))
	(cond ((not bindings)
	       (write-line
		"This can only be invoked as an extended command." s))
	      (t
	       (write-line
		"This can be invoked in the following ways:" s)
	       (write-string "   " s)
	       (print-command-bindings bindings s)
	       (terpri s)))))))

(defcommand "Describe Key" (p)
  "Prompt for a sequence of characters.  When the first character is typed that
   terminates a key binding in the current context, describe the command bound
   to it.  When the first character is typed that no longer allows a correct
   key to be entered, tell the user that this sequence is not bound to
   anything."
  "Print out the command documentation for a key
  which is prompted for."
  (declare (ignore p))
  (multiple-value-bind (key res) (prompt-for-key :prompt "Describe key: "
						 :must-exist t)
    (cond ((commandp res)
	   (with-pop-up-display (s :title "Key documentation")
	     (write-string (pretty-key-string key) s)
	     (format s " is bound to ~S.~%" (command-name res))
	     (format s "Documentation for this command:~%   ~A"
		     (command-documentation res))))
	  (t
	   (with-pop-up-display (s :height 1)
	     (write-string (pretty-key-string key) s)
	     (write-string " is not bound to anything." s))))))

;;;; Generic describe variable, command, key, attribute.

(defvar *generic-describe-kinds*
  (list (make-string-table :initial-contents
			   '(("Variable" . :variable)
			     ("Command" . :command)
			     ("Key" . :key)
			     ("Attribute" . :attribute)))))

(defcommand "Generic Describe" (p)
  "Describe some Hemlock thing.
  First prompt for the kind of thing, then prompt for the thing to describe.
  Currently supported kinds of things are variables, commands, keys and
  character attributes."
  "Prompt for some Hemlock thing to describe."
  (declare (ignore p))
  (multiple-value-bind (ignore kwd)
		       (prompt-for-keyword :tables *generic-describe-kinds*
					   :default "Variable"
					   :help "Kind of thing to describe."
					   :prompt "Kind: ")
    (declare (ignore ignore))
    (case kwd
      (:variable
       (describe-and-show-variable-command nil))
      (:command (describe-command-command ()))
      (:key (describe-key-command ()))
      (:attribute
       (multiple-value-bind (name attr)
			    (prompt-for-keyword
			     :tables (list *character-attribute-names*)
			     :help "Name of character attribute to describe."
			     :prompt "Attribute: ")
	 (print-full-doc name (character-attribute-documentation attr)))))))

;;; PRINT-FULL-DOC displays whole documentation string in a pop-up window.
;;; Doc may be a function that takes at least one arg, :short or :full.
;;;
(defun print-full-doc (nam doc)
  (typecase doc
    (function (funcall doc :full))
    (simple-string
     (with-pop-up-display (s :title (format nil "~s documentation" nam))
       (format s "Documentation for ~S:~%  ~A" nam doc)))
    (t (error "Bad documentation: ~S" doc))))



;;;; Describing and show variables.

(defcommand "Show Variable" (p)
  "Display the values of a Hemlock variable."
  "Display the values of a Hemlock variable."
  (declare (ignore p))
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:help "Name of variable to describe."
			:prompt "Variable: ")
    (with-pop-up-display (s :title (format nil "~S Variable documentation" name))
      (show-variable s name var))))

(defcommand "Describe and Show Variable" (p)
  "Describe in full and show all of variable's value.
   Variable is prompted for."
  "Describe in full and show all of variable's value."
  (declare (ignore p))
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:help "Name of variable to describe."
			:prompt "Variable: ")
    (with-pop-up-display (s :title (format nil "~s" name))
      (format s "Documentation for ~S:~%  ~A~&~%"
	      name (variable-documentation var))
      (show-variable s name var))))

(defun show-variable (s name var)
  (when (hemlock-bound-p var :global)
    (format s "Global value of ~S:~%  ~S~%"
	    name (variable-value var :global)))
  (let ((buffer (current-buffer)))
    (when (hemlock-bound-p var :buffer (current-buffer))
      (format s "Value of ~S in buffer ~A:~%  ~S~%"
	      name (buffer-name buffer)
	      (variable-value var :buffer buffer))))
  (do-strings (mode-name val *mode-names*)
    (declare (ignore val))
    (when (hemlock-bound-p var :mode mode-name)
      (format s "Value of ~S in ~S Mode:~%  ~S~%"
	      name mode-name
	      (variable-value var :mode mode-name)))))



;;;; Describing modes.

(defvar *describe-mode-ignore* (list "Illegal" "Do Nothing"))

(defcommand "Describe Mode" (p &optional name)
  "Describe a mode showing special bindings for that mode."
  "Describe a mode showing special bindings for that mode."
  (declare (ignore p))
  (let ((name (or name
		  (prompt-for-keyword :tables (list *mode-names*)
				      :prompt "Mode: "
				      :help "Enter mode to describe."
				      :default
				      (buffer-major-mode (current-buffer))))))
    (with-pop-up-display (s :title (format nil "~A mode" name))
      (format s "~A mode description:~%" name)
      (let ((doc (mode-documentation name)))
	(when doc
	  (write-line doc s)
	  (terpri s)))
      (map-bindings 
       #'(lambda (key cmd)
	   (unless (member (command-name cmd)
			   *describe-mode-ignore*
			   :test #'string-equal)
	     (let ((str (pretty-key-string key)))
	       (cond ((= (length str) 1)
		      (write-string str s)
		      (write-string "  - " s))
		     (t (write-line str s)
			(write-string "   - " s)))
	       (print-doc (command-documentation cmd) s))))
       :mode name))))
		    
;;;; Printing bindings and last N characters typed.

(defcommand "What Lossage" (p)
  "Display the last 60 characters typed."
  "Display the last 60 characters typed."
  (declare (ignore p))
  (with-pop-up-display (s :title (format nil "The last characters typed") :height 7)
    (let ((num (ring-length *key-event-history*)))
      (format s "The last ~D characters typed:~%" num)
      (do ((i (1- num) (1- i)))
	  ((minusp i))
        (write-string (pretty-key-string (ring-ref *key-event-history* i)) s)
	(write-char #\space s)))))

(defun print-command-bindings (bindings stream)
  (let ((buffer ())
	(mode ())
	(global ()))
    (dolist (b bindings)
      (case (second b)
	(:global (push (first b) global))
	(:mode
	 (let ((m (assoc (third b) mode :test #'string=)))
	   (if m
	       (push (first b) (cdr m))
	       (push (list (third b) (first b)) mode))))
	(t
	 (let ((f (assoc (third b) buffer)))
	   (if f
	       (push (first b) (cdr f))
	       (push (list (third b) (first b)) buffer))))))
    (when global
      (print-some-keys global stream)
      (write-string "; " stream))
    (dolist (b buffer)
      (format stream "Buffer ~S: " (buffer-name (car b)))
      (print-some-keys (cdr b) stream)
      (write-string "; " stream))
    (dolist (m mode)
      (write-string (car m) stream)
      (write-string ": " stream)
      (print-some-keys (cdr m) stream)
      (write-string "; " stream))))

;;; PRINT-SOME-KEYS prints the list of keys onto Stream.
;;;
(defun print-some-keys (keys stream)
  (do ((key keys (cdr key)))
      ((null (cdr key))
       (write-string (pretty-key-string (car key)) stream))
    (write-string (pretty-key-string (car key)) stream)
    (write-string ", " stream)))
