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
;;; Structures and assorted macros for Hemlock.
;;;

(in-package :hemlock-internals)


;;;; Marks.

(defstruct (mark (:print-function %print-hmark)
		 (:predicate markp)
		 (:copier nil)
		 (:constructor internal-make-mark (line charpos %kind)))
  "A Hemlock mark object.  See Hemlock Command Implementor's Manual for details."
  line					; pointer to line
  charpos				; character position
  %kind)				; type of mark

(setf (documentation 'markp 'function)
  "Returns true if its argument is a Hemlock mark object, false otherwise.")
(setf (documentation 'mark-line 'function)
  "Returns line that a Hemlock mark points to.")
(setf (documentation 'mark-charpos 'function)
  "Returns the character position of a Hemlock mark.
  A mark's character position is the index within the line of the character
  following the mark.")


(defstruct (font-mark (:print-function
		       (lambda (s stream d)
			 (declare (ignore d))
			 (write-string "#<Hemlock Font-Mark \"" stream)
			 (%print-before-mark s stream)
			 (write-string "/\\" stream)
			 (%print-after-mark s stream)
			 (write-string "\">" stream)))
		      (:include mark)
		      (:copier nil)
		      (:constructor internal-make-font-mark
				    (line charpos %kind font)))
  font
  region)

(defmacro fast-font-mark-p (s)
  `(typep ,s 'font-mark))


;;;; Regions, buffers, modeline fields.

;;; The region object:
;;;
(defstruct (region (:print-function %print-hregion)
		   (:predicate regionp)
		   (:copier nil)
		   (:constructor internal-make-region (start end)))
  "A Hemlock region object.  See Hemlock Command Implementor's Manual for details."
  start					; starting mark
  end)					; ending mark

(setf (documentation 'regionp 'function)
  "Returns true if its argument is a Hemlock region object, Nil otherwise.")
(setf (documentation 'region-end 'function)
  "Returns the mark that is the end of a Hemlock region.")
(setf (documentation 'region-start 'function)
  "Returns the mark that is the start of a Hemlock region.")

(defstruct (font-region (:include region)
                        (:constructor internal-make-font-region (start end)))
  node)

;;; The buffer object:
;;;
(defstruct (buffer (:constructor internal-make-buffer)
		   (:print-function %print-hbuffer)
		   (:copier nil)
		   (:predicate bufferp))
  "A Hemlock buffer object.  See Hemlock Command Implementor's Manual for details."
  %name			      ; name of the buffer (a string)
  %region		      ; the buffer's region
  %pathname		      ; associated pathname
  major-mode-object           ; buffer's major mode mode object
  minor-mode-objects	      ; list of buffer's minor mode objects, reverse precedence order
  bindings		      ; buffer's command table
  (shadow-syntax nil)         ; buffer's changes to syntax attributes.
  point			      ; current position in buffer
  %mark                       ; a saved buffer position
  region-active               ; modified-tick when region last activated
  (%writable t)		      ; t => can alter buffer's region
  (modified-tick -2)	      ; The last time the buffer was modified.
  (unmodified-tick -1)	      ; The last time the buffer was unmodified
  #+clx
  windows		      ; List of all windows into this buffer.
  #+clozure ;; should be #+Cocoa
  document		      ; NSDocument object associated with this buffer
  var-values		      ; the buffer's local variables
  variables		      ; string-table of local variables
  write-date		      ; File-Write-Date for pathname.
  %modeline-fields	      ; List of modeline-field-info's.
  (delete-hook nil)	      ; List of functions to call upon deletion.
  (line-termination :lf)      ; Line-termination, for the time being
  process		      ; Maybe a listener
  (gap-context )	      ; The value of *buffer-gap-context*
                              ; in the thread that can modify the buffer.
  protected-region            ; (optional) write-protected region
  (font-regions (ccl::init-dll-header (ccl::make-dll-header)))
                                        ; a doubly-linked list of font regions.
  active-font-region                    ; currently active font region
  )

(defstruct (echo-buffer (:include buffer)
                        (:constructor internal-make-echo-buffer))
  )

(defstruct (font-region-node (:include ccl::dll-node)
                             (:constructor make-font-region-node (region)))
  region)

(setf (documentation 'buffer-point 'function)
  "Return the mark that is the current focus of attention in a buffer.")
(setf (documentation 'buffer-variables 'function)
  "Return the string-table of the variables local to the specifed buffer.")
(setf (documentation 'buffer-write-date 'function)
  "Return in universal time format the write date for the file associated
   with the buffer.  If the pathname is set, then this should probably
   be as well.  Should be NIL if the date is unknown or there is no file.")
(setf (documentation 'buffer-delete-hook 'function)
  "This is the list of buffer specific functions that Hemlock invokes when
   deleting this buffer.")


;;; Modeline fields.
;;;
(defstruct (modeline-field (:print-function print-modeline-field)
			   (:constructor %make-modeline-field
					 (%name %function %width)))
  "This is one item displayed in a Hemlock window's modeline."
  %name		; EQL name of this field.
  %function	; Function that returns a string for this field.
  %width)	; Width to display this field in.

(setf (documentation 'modeline-field-p 'function)
      "Returns true if its argument is a modeline field object, nil otherwise.")

(defstruct (modeline-field-info (:print-function print-modeline-field-info)
				(:conc-name ml-field-info-)
				(:constructor make-ml-field-info (field)))
  field
  (start nil)
  (end nil))



;;;; The mode object.

(defstruct (mode-object (:predicate modep)
			(:copier nil)
			(:print-function %print-hemlock-mode))
  name                   ; name of this mode
  setup-function         ; setup function for this mode
  cleanup-function       ; Cleanup function for this mode
  bindings               ; The mode's command table.
  default-command        ; If non-nil, default command
  transparent-p		 ; Are key-bindings transparent?
  hook-name              ; The name of the mode hook.
  major-p                ; Is this a major mode?
  precedence		 ; The precedence for a minor mode.
  character-attributes   ; Mode local character attributes
  variables              ; String-table of mode variables
  var-values             ; Alist for saving mode variables
  documentation          ; Introductory comments for mode describing commands.
  hidden                 ; Not listed in modeline fields
)

(defun %print-hemlock-mode (object stream depth)
  (declare (ignore depth))
  (write-string "#<Hemlock Mode \"" stream)
  (write-string (mode-object-name object) stream)
  (write-string "\">" stream))



;;;; Variables.

;;; This holds information about Hemlock variables, and the system stores
;;; these structures on the property list of the variable's symbolic
;;; representation under the 'hemlock-variable-value property.
;;;
(defstruct (variable-object
	    (:print-function
	     (lambda (object stream depth)
	       (declare (ignore depth))
	       (format stream "#<Hemlock Variable-Object ~S>"
		       (variable-object-name object))))
	    (:copier nil)
	    (:constructor make-variable-object (symbol-name)))
  value		; The value of this variable.
  hooks		; The hook list for this variable.
  documentation ; The documentation.
  name		; The string name.
  symbol-name)  ; The corresponding symbol name.

;;;; Attribute descriptors.

(defstruct (attribute-descriptor
	    (:copier nil)
	    (:print-function %print-attribute-descriptor))
  "This structure is used internally in Hemlock to describe a character
  attribute."
  name
  keyword
  documentation
  (vector #() :type (simple-array * (*)))
  hooks
  end-value)



;;;; Commands.

(defstruct (command (:constructor internal-make-command
				  (%name documentation function transparent-p))
		    (:copier nil)
		    (:predicate commandp)
		    (:print-function %print-hcommand))
  %name				   ;The name of the command
  documentation			   ;Command documentation string or function
  function			   ;The function which implements the command
  transparent-p                    ;If true, this command is transparent
  %bindings)			   ;Places where command is bound

(setf (documentation 'commandp 'function)
  "Returns true if its argument is a Hemlock command object, Nil otherwise.")
(setf (documentation 'command-documentation 'function)
  "Return the documentation for a Hemlock command, given the command-object.
  Command documentation may be either a string or a function.  This may
  be set with Setf.")



;;;; Random typeout streams.

;;; These streams write to random typeout buffers for WITH-POP-UP-DISPLAY.
;;;

(defclass random-typeout-stream (#-scl fundamental-character-output-stream
				 #+scl character-output-stream)
  ((mark         :initarg :mark
                 :initform nil
                 :accessor random-typeout-stream-mark
                 :documentation "The buffer point of the associated buffer.")))

(defun make-random-typeout-stream (mark)
  (make-instance 'random-typeout-stream
                 :mark mark))

(defmethod print-object ((object random-typeout-stream) stream)
  (format stream "#<Hemlock Random-Typeout-Stream ~S>"
          (ignore-errors
            (buffer-name
             (mark-buffer (random-typeout-stream-mark object))))))


;;;; Some defsetfs:

(defsetf buffer-writable %set-buffer-writable
  "Sets whether the buffer is writable and invokes the Buffer Writable Hook.")
(defsetf buffer-name %set-buffer-name
  "Sets the name of a specified buffer, invoking the Buffer Name Hook.")
(defsetf buffer-modified %set-buffer-modified
  "Make a buffer modified or unmodified.")
(defsetf buffer-pathname %set-buffer-pathname
  "Sets the pathname of a buffer, invoking the Buffer Pathname Hook.")

(defsetf getstring %set-string-table
  "Sets the value for a string-table entry, making a new one if necessary.")

(define-setf-expander value (var)
  "Set the value of a Hemlock variable, calling any hooks."
  (let ((svar (gensym)))
    (values
     ()
     ()
     (list svar)
     `(%set-value ',var ,svar)
     `(value ,var))))

(defsetf variable-value (name &optional (kind :current) where) (new-value)
  "Set the value of a Hemlock variable, calling any hooks."
  `(%set-variable-value ,name ,kind ,where ,new-value))

(defsetf variable-hooks (name &optional (kind :current) where) (new-value)
  "Set the list of hook functions for a Hemlock variable."
  `(%set-variable-hooks ,name ,kind ,where ,new-value))

(defsetf variable-documentation (name &optional (kind :current) where) (new-value)
  "Set a Hemlock variable's documentation."
  `(%set-variable-documentation ,name ,kind ,where ,new-value))

(defsetf buffer-minor-mode %set-buffer-minor-mode
  "Turn a buffer minor mode on or off.")
(defsetf buffer-major-mode %set-buffer-major-mode
  "Set a buffer's major mode.")
(defsetf previous-character %set-previous-character
  "Sets the character to the left of the given Mark.")
(defsetf next-character %set-next-character
  "Sets the characters to the right of the given Mark.")
(defsetf character-attribute %set-character-attribute
  "Set the value for a character attribute.")
(defsetf character-attribute-hooks %set-character-attribute-hooks
  "Set the hook list for a Hemlock character attribute.")
(defsetf ring-ref %set-ring-ref "Set an element in a ring.")
(defsetf mark-kind %set-mark-kind "Used to set the kind of a mark.")
(defsetf buffer-region %set-buffer-region "Set a buffer's region.")
(defsetf command-name %set-command-name
  "Change a Hemlock command's name.")
(defsetf line-string %set-line-string
  "Replace the contents of a line.")
(defsetf last-command-type %set-last-command-type
  "Set the Last-Command-Type for use by the next command.")
(defsetf logical-key-event-p %set-logical-key-event-p
  "Change what Logical-Char= returns for the specified arguments.")
(defsetf window-font %set-window-font
  "Change the font-object associated with a font-number in a window.")
(defsetf default-font %set-default-font
  "Change the font-object associated with a font-number in new windows.")

(defsetf modeline-field-name %set-modeline-field-name
  "Sets a modeline-field's name.  If one already exists with that name, an
   error is signaled.")

;;; Shared buffer-gap context, used to communicate between command threads
;;; and the event thread.  Note that this isn't buffer-specific; in particular,
;;; OPEN-LINE and friends may not point at a line that belongs to any
;;; buffer.

(defstruct buffer-gap-context
  (lock (ccl::make-lock))
  (left-open-pos 0)
  (right-open-pos 0)
  (line-cache-length 200)
  (open-line nil)
  (open-chars (make-string 200))
)

(defun ensure-buffer-gap-context (buffer)
  (or (buffer-gap-context buffer)
      (setf (buffer-gap-context buffer) (make-buffer-gap-context))))

(defun buffer-lock (buffer)
  (buffer-gap-context-lock (ensure-buffer-gap-context buffer)))

(defun current-gap-context ()
  (unless (boundp '*current-buffer*)
    (error "Gap context not bound"))
  (ensure-buffer-gap-context *current-buffer*))

(defun current-line-cache-length ()
  (buffer-gap-context-line-cache-length (current-gap-context)))

(defun (setf current-line-cache-length) (len)
  (setf (buffer-gap-context-line-cache-length (current-gap-context)) len))

(defun current-open-line ()
  (buffer-gap-context-open-line (current-gap-context)))

(defun current-open-line-p (line)
  (eq line (current-open-line)))

(defun (setf current-open-line) (value)
  (setf (buffer-gap-context-open-line (current-gap-context)) value))

(defun current-open-chars ()
  (buffer-gap-context-open-chars (current-gap-context)))

(defun (setf current-open-chars) (value)
  (setf (buffer-gap-context-open-chars (current-gap-context)) value))
  
(defun current-left-open-pos ()
  (buffer-gap-context-left-open-pos (current-gap-context)))

(defun (setf current-left-open-pos) (value)
  (setf (buffer-gap-context-left-open-pos (current-gap-context)) value))

(defun current-right-open-pos ()
  (buffer-gap-context-right-open-pos (current-gap-context)))

(defun (setf current-right-open-pos) (value)
  (setf (buffer-gap-context-right-open-pos (current-gap-context)) value))
