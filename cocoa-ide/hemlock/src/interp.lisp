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
;;; Written by Rob MacLachlan and Blaine Burks.
;;;
;;; This file contains the routines which define hemlock commands and
;;; the command interpreter.
;;;

(in-package :hemlock-internals)




(defun %print-hcommand (obj stream depth)
  (declare (ignore depth))
  (write-string "#<Hemlock Command \"" stream)
  (write-string (command-name obj) stream)
  (write-string "\">" stream))



;;;; Key Tables:
;;;
;;;    A key table provides a way to translate a sequence of characters to some
;;; lisp object.  It is currently represented by a tree of hash-tables, where
;;; each level is a hashing from a key to either another hash-table or a value.


;;; GET-TABLE-ENTRY returns the value at the end of a series of hashings.  For
;;; our purposes it is presently used to look up commands and key-translations.
;;;
(defun get-table-entry (table key &key (end (length key)))
  (let ((foo nil))
    (dotimes (i end foo)
      (let ((key-event (aref key i)))
	(setf foo (gethash key-event table))
	(unless (hash-table-p foo) (return foo))
	(setf table foo)))))

;;; SET-TABLE-ENTRY sets the entry for key in table to val, creating new
;;; tables as needed.  If val is nil, then use REMHASH to remove this element
;;; from the hash-table.
;;;
(defun set-table-entry (table key val)
  (dotimes (i (1- (length key)))
    (let* ((key-event (aref key i))
	   (foo (gethash key-event table)))
      (if (hash-table-p foo)
	  (setf table foo)
	  (let ((new-table (make-hash-table)))
	    (setf (gethash key-event table) new-table)
	    (setf table new-table)))))
  (if (null val)
      (remhash (aref key (1- (length key))) table)
      (setf (gethash (aref key (1- (length key))) table) val)))


;;;; Key Translation:
;;;
;;;    Key translations are maintained using a key table.  If a value is an
;;; integer, then it is prefix bits to be OR'ed with the next character.  If it
;;; is a key, then we translate to that key.

(defvar *key-translations* (make-hash-table))

;;; TRANSLATE-KEY  --  Internal
;;;
;;;    This is used internally to do key translations when we want the
;;; canonical representation for Key.  Result, if supplied, is an adjustable
;;; vector with a fill pointer.  We compute the output in this vector.  If the
;;; key ends in the prefix of a translation, we just return that part
;;; untranslated and return the second value true.
;;;
(defun translate-key (key &optional (result (make-array (length key)
							:fill-pointer 0
							:adjustable t))
			            (temp (make-array 10 :fill-pointer 0 :adjustable t)))
  (let ((key-len (length key))
	(start 0)
	(try-pos 0)
	(prefix 0))
    (setf (fill-pointer temp) 0)
    (setf (fill-pointer result) 0)
    (loop
      (when (= try-pos key-len) (return))
      (let ((key-event (aref key try-pos)))
	(vector-push-extend
	 (make-key-event key-event (logior (key-event-bits key-event) prefix))
	 temp)
	(setf prefix 0))
      (let ((entry (get-table-entry *key-translations* temp)))
	(cond ((hash-table-p entry)
	       (incf try-pos))
	      (t
	       (etypecase entry
		 (null
		  (vector-push-extend (aref temp 0) result)
		  (incf start))
		 (simple-vector
		  (dotimes (i (length entry))
		    (vector-push-extend (aref entry i) result))
		  (setf start (1+ try-pos)))
		 (integer
		  (setf start (1+ try-pos))
		  (when (= start key-len) (return))
		  (setf prefix (logior entry prefix))))
	       (setq try-pos start)
	       (setf (fill-pointer temp) 0)))))
    (dotimes (i (length temp))
      (vector-push-extend (aref temp i) result))
    (values result (not (zerop (length temp))))))


;;; KEY-TRANSLATION -- Public.
;;;
(defun key-translation (key)
  "Return the key translation for Key, or NIL if there is none.  If Key is a
   prefix of a translation, then :Prefix is returned.  Whenever Key appears as a
   subsequence of a key argument to the binding manipulation functions, that
   portion will be replaced with the translation.  A key translation may also be
   a list (:Bits {Bit-Name}*).  In this case, the named bits will be set in the
   next character in the key being translated."
  (let ((entry (get-table-entry *key-translations* (crunch-key key))))
    (etypecase entry
      (hash-table :prefix)
      ((or simple-vector null) entry)
      (integer
       (cons :bits (key-event-bits-modifiers entry))))))

;;; %SET-KEY-TRANSLATION  --  Internal
;;;
(defun %set-key-translation (key new-value)
  (let ((entry (cond ((and (consp new-value) (eq (car new-value) :bits))
		      (apply #'make-key-event-bits (cdr new-value)))
		     (new-value (crunch-key new-value))
		     (t new-value))))
    (set-table-entry *key-translations* (crunch-key key) entry)
    new-value))
;;;
(defsetf key-translation %set-key-translation
  "Set the key translation for a key.  If set to null, deletes any
  translation.")



;;;; Interface Utility Functions:

(defvar *global-command-table* (make-hash-table)
  "The command table for global key bindings.")

;;; GET-RIGHT-TABLE  --  Internal
;;;
;;;    Return a hash-table depending on "kind" and checking for errors.
;;;
(defun get-right-table (kind where)
  (case kind
     (:global
      (when where
	(error "Where argument ~S is meaningless for :global bindings."
	       where))
      *global-command-table*)
     (:mode (let ((mode (getstring where *mode-names*)))
	      (unless mode
		(error "~S is not a defined mode." where))
	      (mode-object-bindings mode)))
     (:buffer (unless (bufferp where)
		(error "~S is not a buffer." where))
	      (buffer-bindings where))
     (t (error "~S is not a valid binding type." kind))))


;;; CRUNCH-KEY  --  Internal.
;;;
;;; Take a key in one of the various specifications and turn it into the
;;; standard one: a simple-vector of characters.
;;;
(defun crunch-key (key)
  (typecase key
    (key-event (vector key))
    ((or list vector) ;List thrown in gratuitously.
     (when (zerop (length key))
       (error "A zero length key is illegal."))
     (unless (every #'key-event-p key)
       (error "A Key ~S must contain only key-events." key))
     (coerce key 'simple-vector))
    (t
     (error "Key ~S is not a key-event or sequence of key-events." key))))



;;;; Exported Primitives:

(declaim (special *command-names*))

;;; BIND-KEY  --  Public.
;;;
(defun bind-key (name key &optional (kind :global) where)
  "Bind a Hemlock command to some key somewhere.  Name is the string name
   of a Hemlock command, Key is either a key-event or a vector of key-events.
   Kind is one of :Global, :Mode or :Buffer, and where is the mode name or
   buffer concerned.  Kind defaults to :Global."
  ;;(with-simple-restart (continue "Go on, ignoring binding attempt."))
  (handler-bind ((error
                  #'(lambda (condition)
                      (format *error-output*
                              "~&Error while trying to bind key ~A: ~A~%"
                              key condition)
		      (message (format nil "~a" condition))
                      #-GZ (return-from bind-key nil)
		      )))
                (let ((cmd (getstring name *command-names*))
                      (table (get-right-table kind where))
                      (key (copy-seq (translate-key (crunch-key key)))))
                  (cond (cmd
                         (set-table-entry table key cmd)
                         (push (list key kind where) (command-%bindings cmd))
                         cmd)
                        (t
                         (error "~S is not a defined command." name))))))


;;; DELETE-KEY-BINDING  --  Public
;;;
;;;    Stick NIL in the key table specified.
;;;
(defun delete-key-binding (key &optional (kind :global) where)
  "Remove a Hemlock key binding somewhere.  Key is either a key-event or a
   vector of key-events.  Kind is one of :Global, :Mode or :Buffer, andl where
   is the mode name or buffer concerned.  Kind defaults to :Global."
  (set-table-entry (get-right-table kind where)
		   (translate-key (crunch-key key))
		   nil))


;;; GET-CURRENT-BINDING  --  Internal
;;;
;;;    Look up a key in the current environment.
;;;
(defun get-current-binding (key)
  (let ((buffer *current-buffer*)
        (t-bindings nil) res t-res)
    (multiple-value-setq (res t-res) (get-binding-in-buffer key buffer))
    (when t-res (push t-res t-bindings))
    (loop while (null res)
      for mode in (buffer-minor-mode-objects buffer)
      do (multiple-value-setq (res t-res) (get-binding-in-mode key mode))
      do (when t-res (push t-res t-bindings)))
    (when (null res)
      (multiple-value-setq (res t-res)
        (get-binding-in-mode key (buffer-major-mode-object buffer)))
      (when t-res (push t-res t-bindings)))
    (values (or res (get-table-entry *global-command-table* key))
            (nreverse t-bindings))))

(defun get-binding-in-buffer (key buffer)
  (let ((res (get-table-entry (buffer-bindings buffer) key)))
    (when res
      (if (and (commandp res) (command-transparent-p res))
        (values nil res)
        (values res nil)))))

(defun get-binding-in-mode (key mode)
  (let* ((res (or (get-table-entry (mode-object-bindings mode) key)
                  (let ((default (mode-object-default-command mode)))
                    (and default (getstring default *command-names*))))))
    (when res
      (if (or (mode-object-transparent-p mode)
              (and (commandp res) (command-transparent-p res)))
        (values nil res)
        (values res nil)))))
  

;;; GET-COMMAND -- Public.
;;;
(defun get-command (key &optional (kind :global) where)
  "Return the command object for the command bound to key somewhere.
   If key is not bound, return nil.  Key is either a key-event or a vector of
   key-events.  If key is a prefix of a key-binding, then return :prefix.
   Kind is one of :global, :mode or :buffer, and where is the mode name or
   buffer concerned.  Kind defaults to :Global."
  (multiple-value-bind (key prefix-p)
		       (translate-key (crunch-key key))
    (let ((entry (if (eq kind :current)
		     (get-current-binding key)
		     (get-table-entry (get-right-table kind where) key))))
      (etypecase entry
	(null (if prefix-p :prefix nil))
	(command entry)
	(hash-table :prefix)))))

(defvar *map-bindings-key* (make-array 5 :adjustable t :fill-pointer 0))

;;; MAP-BINDINGS -- Public.
;;;
(defun map-bindings (function kind &optional where)
  "Map function over the bindings in some place.  The function is passed the
   key and the command to which it is bound."
  (labels ((mapping-fun (hash-key hash-value)
	     (vector-push-extend hash-key *map-bindings-key*)
	     (etypecase hash-value
	       (command (funcall function *map-bindings-key* hash-value))
	       (hash-table (maphash #'mapping-fun hash-value)))
	     (decf (fill-pointer *map-bindings-key*))))
    (setf (fill-pointer *map-bindings-key*) 0)
    (maphash #'mapping-fun (get-right-table kind where))))

;;; MAKE-COMMAND -- Public.
;;;
;;; If the command is already defined, then alter the command object;
;;; otherwise, make a new command object and enter it into the *command-names*.
;;;
(defun make-command (name documentation function &key transparent-p)
  "Create a new Hemlock command with Name and Documentation which is
   implemented by calling the function-value of the symbol Function"
  (let ((entry (getstring name *command-names*)))
    (cond
     (entry
      (setf (command-name entry) name)
      (setf (command-documentation entry) documentation)
      (setf (command-function entry) function)
      (setf (command-transparent-p entry) transparent-p))
     (t
      (setf (getstring name *command-names*)
	    (internal-make-command name documentation function transparent-p))))))


;;; COMMAND-NAME, %SET-COMMAND-NAME -- Public.
;;;
(defun command-name (command)
  "Returns the string which is the name of Command."
  (command-%name command))
;;;
(defun %set-command-name (command new-name)
  (check-type command command)
  (check-type new-name string)
  (setq new-name (coerce new-name 'simple-string))
  (delete-string (command-%name command) *command-names*)
  (setf (getstring new-name *command-names*) command)
  (setf (command-%name command) new-name))


;;; COMMAND-BINDINGS -- Public.
;;;
;;; Check that all the supposed bindings really exists.  Bindings which
;;; were once made may have been overwritten.  It is easier to filter
;;; out bogus bindings here than to catch all the cases that can make a
;;; binding go away.
;;;
(defun command-bindings (command)
  "Return a list of lists of the form (key kind where) describing
   all the places where Command is bound."
  (check-type command command)
  (let (result)
    (declare (list result))
    (dolist (place (command-%bindings command))
      (let ((table (case (cadr place)
		   (:global *global-command-table*)
		   (:mode
		    (let ((m (getstring (caddr place) *mode-names*)))
		      (when m (mode-object-bindings m))))
		   (t
		    (when (member (caddr place) *buffer-list*)
		      (buffer-bindings (caddr place)))))))
	(when (and table
		   (eq (get-table-entry table (car place)) command)
		   (not (member place result :test #'equalp)))
	  (push place result))))
    result))

(defvar *key-event-history* (make-ring 60)) 

;;; LAST-COMMAND-TYPE  --  Public
;;;
;;;
(defun last-command-type ()
  "Return the command-type of the last command invoked.
  If no command-type has been set then return NIL.  Setting this with
  Setf sets the value for the next command."
  *last-last-command-type*)

;;; %SET-LAST-COMMAND-TYPE  --  Internal
;;;
(defun %set-last-command-type (type)
  (setf (hemlock-last-command-type *current-view*) type))


;;; PREFIX-ARGUMENT  --  Public
;;;
;;;
(defun prefix-argument ()
  "Return the current value of prefix argument."
  *last-prefix-argument*)

(defun get-self-insert-command ()
  ;; Get the command used to implement normal character insertion in current buffer.
  (getstring (value hemlock::self-insert-command-name) *command-names*))

(defun get-default-command ()
  ;; Get the command used when no binding is present in current buffer.
  (getstring (value hemlock::default-command-name) *command-names*))

(defun get-system-default-behavior-command ()
  ;; Get the command used to invoke "System Default Behavior"
  (getstring (value hemlock::system-default-behavior-command-name) *command-names*))
