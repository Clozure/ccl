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
;;; Hemlock initialization code and random debugging stuff.
;;;
;;; Written by Bill Chiles and Rob MacLachlan
;;;

(in-package :hemlock-internals)

;;;; Definition of *hemlock-version*.

(defvar *hemlock-version* "3.5")
(pushnew :hemlock *features*)
#+(or CMU scl)
(setf (getf ext:*herald-items* :hemlock) 
      `("    Hemlock " ,*hemlock-version*))


;;;; %INIT-HEMLOCK.

(defvar *hemlock-initialized* nil)

(defun %init-hemlock ()
  "Initialize hemlock's internal data structures."
  ;;
  ;; This function is defined in Buffer.Lisp.  It creates fundamental mode
  ;; and the buffer main.  Until this is done it is not possible to define
  ;; or use Hemlock variables.
  (setup-initial-buffer)
  ;;
  ;; Define some of the system variables.
  (define-some-variables)
  ;;
  ;; Site initializations such as window system variables.
  (site-init)
  ;;
  ;; Set up syntax table data structures.
  (%init-syntax-table)
  ;;
  (setq *hemlock-initialized* t))


;;;; Define some globals.

;;; These globals cannot be defined in the appropriate file due to compilation
;;; or load time constraints.
;;;

;;; The following belong in other files, but those files are loaded before
;;; table.lisp which defines MAKE-STRING-TABLE.
;;;
;;; vars.lisp
(defvar *global-variable-names* (make-string-table)
  "A String Table of global variable names, the values are the symbol names.") 
;;;
;;; buffer.lisp
(defvar *mode-names* (make-string-table) "A String Table of Mode names.")
(defvar *buffer-names* (make-string-table)
  "A String Table of Buffer names and their corresponding objects.")
;;;
;;; interp.lisp
(defvar *command-names* (make-string-table) "String table of command names.")
;;;
;;; syntax.lisp
(defvar *character-attribute-names* (make-string-table)
 "String Table of character attribute names and their corresponding keywords.")



;;;; DEFINE-SOME-VARIABLES.

(defun define-some-variables ()
  (defhvar "Default Modes"
    "This variable contains the default list of modes for new buffers."
    :value '("Fundamental"))
  (defhvar "Make Buffer Hook"
    "This hook is called with the new buffer whenever a buffer is created.")
  (defhvar "Delete Buffer Hook"
    "This hook is called with the buffer whenever a buffer is deleted.")
  (defhvar "Buffer Major Mode Hook"
    "This hook is called with the buffer and the new mode when a buffer's
     major mode is changed.")
  (defhvar "Buffer Minor Mode Hook"
    "This hook is called a minor mode is changed.  The arguments are 
     the buffer, the mode affected and T or NIL depending on when the
     mode is being turned on or off.")
  (defhvar "Buffer Writable Hook"
    "This hook is called whenever someone sets whether the buffer is
     writable.")
  (defhvar "Buffer Name Hook"
    "This hook is called with the buffer and the new name when the name of a
     buffer is changed.")
  (defhvar "Buffer Pathname Hook"
    "This hook is called with the buffer and the new Pathname when the Pathname
     associated with the buffer is changed.")
  (defhvar "Buffer Modified Hook"
    "This hook is called whenever a buffer changes from unmodified to modified
     and vice versa.  It takes the buffer and the new value for modification
     flag.")
  (defhvar "Buffer Package Hook"
      "This hook is called with the new package name whenever a (Lisp) buffer's package changes")
  (defhvar "Delete Variable Hook"
    "This hook is called when a variable is deleted with the args to
     delete-variable.")
  (defhvar "Key Echo Delay"
    "Wait this many seconds before echoing keys in the command loop.  This
     feature is inhibited when nil."
    :value 1.0)
  (defhvar "Input Hook"
    "The functions in this variable are invoked each time a character enters
     Hemlock."
    :value nil)
  (defhvar "Abort Hook"
    "These functions are invoked when ^G is typed.  No arguments are passed."
    :value nil)
  (defhvar "Command Abort Hook"
    "These functions get called when commands are aborted, such as with
     EDITOR-ERROR."
    :value nil)
  (defhvar "Character Attribute Hook"
    "This hook is called with the attribute, character and new value
     when the value of a character attribute is changed.")
  (defhvar "Shadow Attribute Hook"
    "This hook is called when a mode character attribute is made.")
  (defhvar "Unshadow Attribute Hook"
    "This hook is called when a mode character attribute is deleted.")
  (defhvar "Default Modeline Fields"
    "The default list of modeline-fields for MAKE-BUFFER."
    :value *default-modeline-fields*)
  (defhvar "Maximum Modeline Pathname Length"
    "When set, this variable is the maximum length of the display of a pathname
     in a modeline.  When the pathname is too long, the :buffer-pathname
     modeline-field function chops off leading directory specifications until
     the pathname fits.  \"...\" indicates a truncated pathname."
    :value nil
    :hooks (list 'maximum-modeline-pathname-length-hook))
  (defhvar "Self Insert Command Name"
    "The name of the command to invoke to handle quoted input (i.e. after c-q).
     By default, this is \"Self Insert\"."
    :value "Self Insert")
  (defhvar "Default Command Name"
    "The name of the command to invoke to handle keys that have no binding
     defined.  By default, this is \"Illegal\"."
    :value "Illegal")
  (defhvar "System Default Behavior Command Name"
    "The name of the command to handle keys which should by handled by the
     native window system (not by Hemlock.)
     By default, this is \"System Default Behavior\"."
    :value "System Default Behavior")
  )



;;;; ED.

(defvar *editor-has-been-entered* ()
  "True if and only if the editor has been entered.")
(defvar *in-the-editor* ()
  "True if we are inside the editor.  This is used to prevent ill-advised
   \"recursive\" edits.")

(defvar *after-editor-initializations-funs* nil
  "A list of functions to be called after the editor has been initialized upon
   entering the first time.")

(defmacro after-editor-initializations (&rest forms)
  "Causes forms to be executed after the editor has been initialized.
   Forms supplied with successive uses of this macro will be executed after
   forms supplied with previous uses."
  `(push #'(lambda () ,@forms)
	 *after-editor-initializations-funs*))

;;;; SAVE-ALL-BUFFERS.

;;; SAVE-ALL-BUFFERS -- Public.
;;;
(defun save-all-buffers (&optional (list-unmodified-buffers nil))
  "This prompts users with each modified buffer as to whether they want to
   write it out.  If the buffer has no associated file, this will also prompt
   for a file name.  Supplying the optional argument non-nil causes this
   to prompt for every buffer."
  (dolist (buffer *buffer-list*)
    (when (or list-unmodified-buffers (buffer-modified buffer))
      (maybe-save-buffer buffer))))

(defun maybe-save-buffer (buffer)
  (let* ((modified (buffer-modified buffer))
	 (pathname (buffer-pathname buffer))
	 (name (buffer-name buffer))
	 (string (if pathname (namestring pathname))))
    (format t "Buffer ~S is ~:[UNmodified~;modified~], Save it? "
	    name modified)
    (force-output)
    (when (y-or-n-p)
      (let ((name (read-line-default "File to write" string)))
	(format t "Writing file ~A..." name)
	(force-output)
	(write-file (buffer-region buffer) name)
	(write-line "write WON")))))

(defun read-line-default (prompt default)
  (format t "~A:~@[ [~A]~] " prompt default)
  (force-output)
  (do ((result (read-line) (read-line)))
      (())
    (declare (simple-string result))
    (when (plusp (length result)) (return result))
    (when default (return default))
    (format t "~A:~@[ [~A]~] " prompt default)
    (force-output)))

(unless *hemlock-initialized*
  (%init-hemlock))
