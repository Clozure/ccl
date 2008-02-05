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
;;;    This file contains the implementation of keyboard macros for
;;; Hemlock.  In itself it contains nothing particularly gross or
;;; implementation dependant, but it uses some hooks in the stream
;;; system and other stuff.
;;;

(in-package :hemlock)

;;; We have "Keyboard Macro Transforms" that help in making a keyboard
;;; macro.  What they do is turn the sequence of commands into equivalent
;;; lisp code.  They operate under the following principles:
;;;
;;;    They are passed two arguments:
;;; 1] The command invoked.
;;; 2] A keyword, either :invoke, :start or :finish
;;;
;;;    If the keyword is :invoke, then the transform is expected to
;;; invoke the command and do whatever is necessary to make the same
;;; thing happen again when the macro is invoked.  The method does this
;;; by pushing forms on the list *current-kbdmac* and characters to
;;; simulate input of on *kbdmac-input*.  *current-kbdmac* is kept
;;; in reverse order.  Each form must be a function call, and none
;;; of the arguments are evaluated.  If the transform is unwound, 
;;; presumably due to an error in the invoked command, then nothing
;;; should be done at invocation time.
;;;
;;;    If the keyword is :finish, then nothing need be done.  This
;;; is to facilitate compaction of repetitions of the same command
;;; into one call.  The transform is called with :finish when a run
;;; is broken.  Similarly, the transform is called with :start
;;; before the first occurrence in a run.

(defvar *kbdmac-transcript* (make-array 100  :fill-pointer 0 :adjustable t)
  "The thing we bind *input-transcript* to during keyboard macro definition.")

(defvar *kbdmac-input* (make-array 100  :fill-pointer 0  :adjustable t)
  "Place where we stick input that will need to be simulated during keyboard
  macro execution.")

(defvar *current-kbdmac* () "Body of keyboard macro we are building.")

(defvar *kbdmac-transforms* (make-hash-table :test #'eq)
  "Hashtable of function that know how to do things.")

(defvar *old-invoke-hook* () "Bound to *invoke-hook* by kbdmac-command-loop.")

(defmacro define-kbdmac-transform (command function)
  `(setf (gethash (getstring ,command *command-names*)
		  *kbdmac-transforms*)
	 ,function))

(defmacro kbdmac-emit (form)
  `(push ,form *current-kbdmac*))

(defun trash-character ()
  "Throw away a character on *editor-input*."
  (get-key-event hi::*editor-input*))

;;; Save-Kbdmac-Input  --  Internal
;;;
;;;    Pushes any input read within the body on *kbdmac-input* so that
;;; it is read again at macro invocation time.  It uses the (input-waiting)
;;; function which is a non-standard hook into the stream system.
;;;
(defmacro save-kbdmac-input (&body forms)
  (let ((slen (gensym)))
    `(let ((,slen (- (length *kbdmac-transcript*) (if (input-waiting) 1 0))))
       (multiple-value-prog1
	(progn ,@forms)
	(do ((i ,slen (1+ i))
	     (elen (length *kbdmac-transcript*)))
	    ((= i elen)
	     (when (input-waiting)
	       (kbdmac-emit '(trash-character))))	 
	  (vector-push-extend (aref *kbdmac-transcript* i)
			      *kbdmac-input*))))))

;;;; The default transform
;;;
;;;    This transform is called when none is defined for a command.
;;;
(defun default-kbdmac-transform (command key)
  (case key
    (:invoke
     (let ((fun (command-function command))
	   (arg (prefix-argument))
	   (lastc *last-key-event-typed*))
       (save-kbdmac-input
	 (let ((*invoke-hook* *old-invoke-hook*))
	   (funcall fun arg))
	 (kbdmac-emit `(set *last-key-event-typed* ,lastc))
	 (kbdmac-emit `(,fun ,arg)))))))

;;;; Self insert transform:
;;;
;;;    For self insert we accumulate the text in a string and then
;;; insert it all at once.
;;;

(defvar *kbdmac-text* (make-array 100 :fill-pointer 0 :adjustable t))

(defun insert-string-at-point (string)
  (insert-string (buffer-point (current-buffer)) string))
(defun insert-character-at-point (character)
  (insert-character (buffer-point (current-buffer)) character))

(defun key-vector-to-string (key-vector)
  (let ((string (make-array (length key-vector) :element-type 'base-char)))
    (dotimes (i (length key-vector) string)
      (setf (aref string i) (hemlock-ext:key-event-char (aref key-vector i))))))

(defun self-insert-kbdmac-transform (command key)
  (case key
    (:start
     (setf (fill-pointer *kbdmac-text*) 0))
    (:invoke
     (let ((p (or (prefix-argument) 1)))
       (funcall (command-function command) p)
       (dotimes (i p)
	 (vector-push-extend *last-key-event-typed* *kbdmac-text*))))
    (:finish
     (if (> (length *kbdmac-text*) 1)
	 (kbdmac-emit `(insert-string-at-point
			,(key-vector-to-string *kbdmac-text*)))
	 (kbdmac-emit `(insert-character-at-point
			,(hemlock-ext:key-event-char (aref *kbdmac-text* 0))))))))
;;;
(define-kbdmac-transform "Self Insert" #'self-insert-kbdmac-transform)
(define-kbdmac-transform "Lisp Insert )" #'self-insert-kbdmac-transform)

;;;; Do-Nothing transform:
;;;
;;;    These are useful for prefix-argument setting commands, since they have
;;; no semantics at macro-time.
;;;
(defun do-nothing-kbdmac-transform (command key)
  (case key
    (:invoke
     (funcall (command-function command) (prefix-argument)))))
;;;
(define-kbdmac-transform "Argument Digit" #'do-nothing-kbdmac-transform)
(define-kbdmac-transform "Negative Argument" #'do-nothing-kbdmac-transform)
(define-kbdmac-transform "Universal Argument" #'do-nothing-kbdmac-transform)

;;;; Multiplicative transform
;;;
;;;    Repititions of many commands can be turned into a call with an
;;; argument.
;;;
(defvar *kbdmac-count* 0
  "The number of occurrences we have counted of a given command.")

(defun multiplicative-kbdmac-transform (command key)
  (case key
    (:start
     (setq *kbdmac-count* 0))
    (:invoke
     (let ((p (or (prefix-argument) 1)))
       (funcall (command-function command) p)
       (incf *kbdmac-count* p)))
    (:finish
     (kbdmac-emit `(,(command-function command) ,*kbdmac-count*)))))
;;;
(define-kbdmac-transform "Forward Character" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Backward Character" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Forward Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Backward Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Uppercase Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Lowercase Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Capitalize Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Kill Next Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Kill Previous Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Forward Kill Form" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Backward Kill Form" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Forward Form" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Backward Form" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Delete Next Character"
  #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Delete Previous Character"
   #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Delete Previous Character Expanding Tabs"
   #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Next Line" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Previous Line" #'multiplicative-kbdmac-transform)


;;;; Vanilla transform
;;;
;;;    These commands neither read input nor look at random silly variables.
;;;
(defun vanilla-kbdmac-transform (command key)
  (case key
    (:invoke
     (let ((fun (command-function command))
	   (p (prefix-argument)))
       (funcall fun p)
       (kbdmac-emit `(,fun ,p))))))
;;;
(define-kbdmac-transform "Beginning of Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "End of Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Beginning of Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Indent for Lisp" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Delete Horizontal Space" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Kill Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Backward Kill Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Un-Kill" #'vanilla-kbdmac-transform)

;;;; MAKE-KBDMAC, INTERACTIVE, and kbdmac command loop.

;;; Kbdmac-Command-Loop  --  Internal
;;;
;;;    Bind *invoke-hook* to call kbdmac transforms.
;;;
(defun kbdmac-command-loop ()
  (let* ((last-transform nil)
	 (last-command nil)
	 (last-ctype nil)
	 (*old-invoke-hook* *invoke-hook*)
	 (*invoke-hook*
	  #'(lambda (res p)
	      (declare (ignore p))
	      (when (and (not (eq last-command res)) last-transform)
		(funcall last-transform last-command :finish))
	      (if (last-command-type)
		  (setq last-ctype t)
		  (when last-ctype
		    (kbdmac-emit '(clear-command-type))
		    (setq last-ctype nil)))
	      (setq last-transform 
		    (gethash res *kbdmac-transforms* #'default-kbdmac-transform))
	      (unless (eq last-command res)
		(funcall last-transform res :start))
	      (funcall last-transform res :invoke)
	      (setq last-command res))))
    (declare (special *invoke-hook*))
    (setf (last-command-type) nil)
    (recursive-edit nil)))

(defun clear-command-type ()
  (setf (last-command-type) nil))


(defvar *defining-a-keyboard-macro* ())
(defvar *kbdmac-stream* #+later (make-kbdmac-stream))
(defvar *in-a-keyboard-macro* ()
  "True if we are currently executing a keyboard macro.")

;;; Interactive  --  Public
;;;
;;;    See whether we are in a keyboard macro.
;;;
(defun interactive ()
  "Return true if we are in a command invoked by the user.
  This is primarily useful for commands which want to know
  whether do something when an error happens, or just signal
  an Editor-Error."
  (not *in-a-keyboard-macro*))

(defvar *kbdmac-done* ()
  "Setting this causes the keyboard macro being executed to terminate
  after the current iteration.")

(defvar *kbdmac-dont-ask* ()
  "Setting this inhibits \"Keyboard Macro Query\"'s querying.")

;;; Make-Kbdmac  --  Internal
;;;
;;;    This guy grabs the stuff lying around in *current-kbdmac* and
;;; whatnot and makes a lexical closure that can be used as the
;;; definition of a command.  The prefix argument is a repitition
;;; count.
;;;
(defun make-kbdmac ()
  (let ((code (nreverse *current-kbdmac*))
	(input (copy-seq *kbdmac-input*)))
    (if (zerop (length input))
	#'(lambda (p)
	    (let ((*in-a-keyboard-macro* t)
		  (*kbdmac-done* nil)
		  (*kbdmac-dont-ask* nil))
	      (setf (last-command-type) nil)
	      (catch 'exit-kbdmac
		(dotimes (i (or p 1))
		  (catch 'abort-kbdmac-iteration
		    (dolist (form code)
		      (apply (car form) (cdr form))))
		  (when *kbdmac-done* (return nil))))))
	#'(lambda (p)
	    (let* ((stream (or *kbdmac-stream* (make-kbdmac-stream)))
		   (*kbdmac-stream* nil)
		   (hi::*editor-input* stream)
		   (*in-a-keyboard-macro* t)
		   (*kbdmac-done* nil)
		   (*kbdmac-dont-ask* nil))
	      (setf (last-command-type) nil)
	      (catch 'exit-kbdmac
		(dotimes (i (or p 1))
		  (setq stream (modify-kbdmac-stream stream input))
		  (catch 'abort-kbdmac-iteration
		    (dolist (form code)
		      (apply (car form) (cdr form))))
		  (when *kbdmac-done* (return nil)))))))))
	    	  


;;;; Commands.

(defmode "Def" :major-p nil)  

(defcommand "Define Keyboard Macro" (p)
  "Define a keyboard macro."
  "Define a keyboard macro."
  (declare (ignore p))
  (when *defining-a-keyboard-macro*
    (editor-error "Already defining a keyboard macro."))
  (define-keyboard-macro))

(defhvar "Define Keyboard Macro Key Confirm"
  "When set, \"Define Keyboard Macro Key\" asks for confirmation before
   clobbering an existing key binding."
  :value t)

(defcommand "Define Keyboard Macro Key" (p)
  "Prompts for a key before going into a mode for defining keyboard macros.
   The macro definition is bound to the key.  IF the key is already bound,
   this asks for confirmation before clobbering the binding."
  "Prompts for a key before going into a mode for defining keyboard macros.
   The macro definition is bound to the key.  IF the key is already bound,
   this asks for confirmation before clobbering the binding."
  (declare (ignore p))
  (when *defining-a-keyboard-macro*
    (editor-error "Already defining a keyboard macro."))
  (multiple-value-bind (key kind where)
		       (get-keyboard-macro-key)
    (when key
      (setf (buffer-minor-mode (current-buffer) "Def") t)
      (let ((name (format nil "Keyboard Macro ~S" (gensym))))
	(make-command name "This is a user-defined keyboard macro."
		      (define-keyboard-macro))
	(bind-key name key kind where)
	(message "~A bound to ~A."
		 (with-output-to-string (s) (hemlock-ext:print-pretty-key key s))
		 name)))))

;;; GET-KEYBOARD-MACRO-KEY gets a key from the user and confirms clobbering it
;;; if it is already bound to a command, or it is a :prefix.  This returns nil
;;; if the user "aborts", otherwise it returns the key and location (kind
;;; where) of the binding.
;;;
(defun get-keyboard-macro-key ()
  (let* ((key (prompt-for-key :prompt "Bind keyboard macro to key: "
			      :must-exist nil)))
    (multiple-value-bind (kind where)
			 (prompt-for-place "Kind of binding: "
					   "The kind of binding to make.")
      (let* ((cmd (get-command key kind where)))
	(cond ((not cmd) (values key kind where))
	      ((commandp cmd)
	       (if (prompt-for-y-or-n
		    :prompt `("~A is bound to ~A.  Rebind it? "
			      ,(with-output-to-string (s)
				 (hemlock-ext:print-pretty-key key s))
			      ,(command-name cmd))
		    :default nil)
		   (values key kind where)
		   nil))
	      ((eq cmd :prefix)
	       (if (prompt-for-y-or-n
		    :prompt `("~A is a prefix for more than one command.  ~
			       Clobber it? "
			      ,(with-output-to-string (s)
				 (hemlock-ext:print-pretty-key key s)))
		    :default nil)
		   (values key kind where)
		   nil)))))))

;;; DEFINE-KEYBOARD-MACRO gets input from the user and clobbers the function
;;; for the "Last Keyboard Macro" command.  This returns the new function.
;;;
(defun define-keyboard-macro ()
  (setf (buffer-minor-mode (current-buffer) "Def") t)
  (unwind-protect
    (let* ((in *kbdmac-transcript*)
	   (*input-transcript* in)
	   (*defining-a-keyboard-macro* t))
      (setf (fill-pointer in) 0)
      (setf (fill-pointer *kbdmac-input*) 0)
      (setq *current-kbdmac* ())
      (catch 'punt-kbdmac
	(kbdmac-command-loop))
      (setf (command-function (getstring "Last Keyboard Macro" *command-names*))
	    (make-kbdmac)))
    (setf (buffer-minor-mode (current-buffer) "Def") nil)))


(defcommand "End Keyboard Macro" (p)
  "End the definition of a keyboard macro."
  "End the definition of a keyboard macro."
  (declare (ignore p))
  (unless *defining-a-keyboard-macro*
    (editor-error "Not defining a keyboard macro."))
  (throw 'punt-kbdmac ()))
;;;
(define-kbdmac-transform "End Keyboard Macro" #'do-nothing-kbdmac-transform)


(defcommand "Last Keyboard Macro" (p)
  "Execute the last keyboard macro defined.
  With prefix argument execute it that many times."
  "Execute the last keyboard macro P times."
  (declare (ignore p))
  (editor-error "No keyboard macro defined."))

(defcommand "Name Keyboard Macro" (p &optional name)
  "Name the \"Last Keyboard Macro\".
  The last defined keboard macro is made into a named command."
  "Make the \"Last Keyboard Macro\" a named command."
  (declare (ignore p))
  (unless name
    (setq name (prompt-for-string
		:prompt "Macro name: "
		:help "String name of command to make from keyboard macro.")))
  (make-command
    name "This is a named keyboard macro."
   (command-function (getstring "Last Keyboard Macro" *command-names*))))

(defcommand "Keyboard Macro Query" (p)
  "Keyboard macro conditional.
  During the execution of a keyboard macro, this command prompts for
  a single character command, similar to those of \"Query Replace\"."
  "Prompt for action during keyboard macro execution."
  (declare (ignore p))
  (unless (or (interactive) *kbdmac-dont-ask*)
    (let ((hi::*editor-input* *real-editor-input*))
      (command-case (:prompt "Keyboard Macro Query: "
		     :help "Type one of these characters to say what to do:"
		     :change-window nil
		     :bind key-event)
	(:exit
	 "Exit this keyboard macro immediately."
	 (throw 'exit-kbdmac nil))
	(:yes
	 "Proceed with this iteration of the keyboard macro.")
	(:no
       "Don't do this iteration of the keyboard macro, but continue to the next."
	 (throw 'abort-kbdmac-iteration nil))
	(:do-all
	 "Do all remaining repetitions of the keyboard macro without prompting."
	 (setq *kbdmac-dont-ask* t))
	(:do-once
	 "Do this iteration of the keyboard macro and then exit."
	 (setq *kbdmac-done* t))
	(:recursive-edit
	 "Do a recursive edit, then ask again."
	 (do-recursive-edit)
	 (reprompt))
	(t
	 (unget-key-event key-event hi::*editor-input*)
	 (throw 'exit-kbdmac nil))))))
