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
  modes			      ; list of buffer's mode names
  mode-objects		      ; list of buffer's mode objects
  bindings		      ; buffer's command table
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
  display-start		      ; Window display start when switching to buf.
  %modeline-fields	      ; List of modeline-field-info's.
  (delete-hook nil)	      ; List of functions to call upon deletion.
  (line-termination :unix) ; Line-termination, for the time being
  process		      ; Maybe a listener
  (gap-context )	      ; The value of *buffer-gap-context*
                              ; in the thread that can modify the buffer.
  protected-region            ; (optional) write-protected region
  (font-regions (ccl::init-dll-header (ccl::make-dll-header)))
                                        ; a doubly-linked list of font regions.
  active-font-region                    ; currently active font region
  )

(defstruct (font-region-node (:include ccl::dll-node)
                             (:constructor make-font-region-node (region)))
  region)

(setf (documentation 'buffer-modes 'function)
  "Return the list of the names of the modes active in a given buffer.")
(setf (documentation 'buffer-point 'function)
  "Return the mark that is the current focus of attention in a buffer.")
(setf (documentation 'buffer-windows 'function)
  "Return the list of windows that are displaying a given buffer.")
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
	    (:constructor make-variable-object (documentation name)))
  value		; The value of this variable.
  hooks		; The hook list for this variable.
  down		; The variable-object for the previous value.
  documentation ; The documentation.
  name)		; The string name.



;#+clx
(progn
;;;; Windows, dis-lines, and font-changes.

;;; The window object:
;;;
  (defstruct (window (:constructor internal-make-window)
                     (:predicate windowp)
                     (:copier nil)
                     (:print-function %print-hwindow))
    "This structure implements a Hemlock window."
    tick				; The last time this window was updated.
    %buffer			; buffer displayed in this window.
    height			; Height of window in lines.
    width				; Width of the window in characters.
    old-start			; The charpos of the first char displayed.
    first-line			; The head of the list of dis-lines.
    last-line			; The last dis-line displayed.
    first-changed			; The first changed dis-line on last update.
    last-changed			; The last changed dis-line.
    spare-lines			; The head of the list of unused dis-lines
    (old-lines 0)			; Slot used by display to keep state info
    hunk				; The device hunk that displays this window.
    display-start			; first character position displayed
    display-end			; last character displayed
    point				; Where the cursor is in this window.  
    modeline-dis-line		; Dis-line for modeline display.
    modeline-buffer		; Complete string of all modeline data.
    modeline-buffer-len		; Valid chars in modeline-buffer.
    display-recentering)		; Tells whether redisplay recenters window
                                        ;    regardless of whether it is current.

  (setf (documentation 'windowp 'function)
        "Returns true if its argument is a Hemlock window object, Nil otherwise.")
  (setf (documentation 'window-height 'function)
        "Return the height of a Hemlock window in character positions.")
  (setf (documentation 'window-width 'function)
        "Return the width of a Hemlock window in character positions.")
  (setf (documentation 'window-display-start 'function)
        "Return the mark which points before the first character displayed in
   the supplied window.")
  (setf (documentation 'window-display-end 'function)
        "Return the mark which points after the last character displayed in
   the supplied window.")
  (setf (documentation 'window-point 'function)
        "Return the mark that points to where the cursor is displayed in this
  window.  When the window is made current, the Buffer-Point of this window's
  buffer is moved to this position.  While the window is current, redisplay
  makes this mark point to the same position as the Buffer-Point of its
  buffer.")
  (setf (documentation 'window-display-recentering 'function)
        "This determines whether redisplay recenters window regardless of whether it
  is current.  This is SETF'able.")

  (defstruct (window-dis-line (:copier nil)
                              (:constructor make-window-dis-line (chars))
                              (:conc-name dis-line-))
    chars			      ; The line-image to be displayed.
    (length 0 :type fixnum)     ; Length of line-image.
    font-changes                ; Font-Change structures for changes in this line.
    old-chars		      ; Line-Chars of line displayed.
    line			      ; Line displayed.
    (flags 0 :type fixnum)      ; Bit flags indicate line status.
    (delta 0 :type fixnum)      ; # lines moved from previous position.
    (position 0 :type fixnum)   ; Line # to be displayed on.
    (end 0 :type fixnum))	      ; Index after last logical character displayed.

  (defstruct (font-change (:copier nil)
                          (:constructor make-font-change (next)))
    x			      ; X position that change takes effect.
    font			      ; Index into font-map of font to use.
    next			      ; The next Font-Change on this dis-line.
    mark)			      ; Font-Mark responsible for this change.



;;;; Font family.

  (defstruct font-family
    map			; Font-map for hunk.
    height		; Height of char box includung VSP.
    width			; Width of font.
    baseline		; Pixels from top of char box added to Y.
    cursor-width		; Pixel width of cursor.
    cursor-height		; Pixel height of cursor.
    cursor-x-offset	; Added to pos of UL corner of char box to get
    cursor-y-offset)	; UL corner of cursor blotch.

  )


;;;; Attribute descriptors.

(defstruct (attribute-descriptor
	    (:copier nil)
	    (:print-function %print-attribute-descriptor))
  "This structure is used internally in Hemlock to describe a character
  attribute."
  name
  keyword
  documentation
  vector
  hooks
  end-value)



;;;; Commands.

(defstruct (command (:constructor internal-make-command
				  (%name documentation function))
		    (:copier nil)
		    (:predicate commandp)
		    (:print-function %print-hcommand))
  %name				   ;The name of the command
  documentation			   ;Command documentation string or function
  function			   ;The function which implements the command
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
                 :documentation "The buffer point of the associated buffer.")
   (window       :initarg :window
                 :initform nil
                 :accessor random-typeout-stream-window
                 :documentation "The hemlock window all this shit is in.")
   (more-mark    :initarg :more-mark
                 :initform nil
                 :accessor random-typeout-stream-more-mark
                 :documentation "The mark that is not displayed when we need to more.")
   (no-prompt    :initarg :no-prompt
                 :initform nil
                 :accessor random-typeout-stream-no-prompt
                 :documentation "T when we want to exit, still collecting output.")
   (first-more-p :initarg :first-more-p
                 :initform t
                 :accessor random-typeout-stream-first-more-p
                 :documentation "T until the first time we more. Nil after.")
   (line-buffered-p :documentation "whether line buffered") ))

(defun make-random-typeout-stream (mark)
  (make-instance 'random-typeout-stream
                 :mark mark))

(defmethod print-object ((object random-typeout-stream) stream)
  (format stream "#<Hemlock Random-Typeout-Stream ~S>"
          (ignore-errors
            (buffer-name
             (line-buffer (mark-line (random-typeout-stream-mark object)))))))


;;;; Redisplay devices.

;;; Devices contain monitor specific redisplay methods referenced by
;;; redisplay independent code.
;;;
(defstruct (device (:print-function print-device)
		   (:constructor %make-device))
  name			; simple-string such as "concept" or "lnz".
  init			; fun to call whenever going into the editor.
			; args: device
  exit			; fun to call whenever leaving the editor.
			; args: device
  smart-redisplay	; fun to redisplay a window on this device.
			; args: window &optional recenterp
  dumb-redisplay	; fun to redisplay a window on this device.
			; args: window &optional recenterp
  after-redisplay	; args: device
			; fun to call at the end of redisplay entry points.
  clear			; fun to clear the entire display.
			; args: device
  note-read-wait	; fun to somehow note on display that input is expected.
			; args: on-or-off
  put-cursor		; fun to put the cursor at (x,y) or (column,line).
			; args: hunk &optional x y
  show-mark		; fun to display the screens cursor at a certain mark.
			; args: window x y time
  next-window		; funs to return the next and previous window
  previous-window	;    of some window.
			; args: window
  make-window		; fun to make a window on the screen.
			; args: device start-mark
			;       &optional modeline-string modeline-function
  delete-window		; fun to remove a window from the screen.
			; args: window
  random-typeout-setup	; fun to prepare for random typeout.
  			; args: device n
  random-typeout-cleanup; fun to clean up after random typeout.
  			; args: device degree
  random-typeout-line-more ; fun to keep line-buffered streams up to date.
  random-typeout-full-more ; fun to do full-buffered  more-prompting.
			   ; args: # of newlines in the object just inserted
			   ;    in the buffer.
  force-output		; if non-nil, fun to force any output possibly buffered.
  finish-output		; if non-nil, fun to force output and hand until done.
  			; args: device window
  beep			; fun to beep or flash the screen.
  bottom-window-base    ; bottom text line of bottom window.
  hunks)		; list of hunks on the screen.

(defun print-device (obj str n)
  (declare (ignore n))
  (format str "#<Hemlock Device ~S>" (device-name obj)))


(defstruct (bitmap-device #|(:print-function print-device)|#
			  (:include device))
  display)		      ; CLX display object.


(defstruct (tty-device #|(:print-function print-device)|#
		       (:constructor %make-tty-device)
		       (:include device))
  dumbp			; t if it does not have line insertion and deletion.
  lines			; number of lines on device.
  columns		; number of columns per line.
  display-string	; fun to display a string of characters at (x,y).
			; args: hunk x y string &optional start end 
  standout-init         ; fun to put terminal in standout mode.
			; args: hunk
  standout-end          ; fun to take terminal out of standout mode.
			; args: hunk
  clear-lines		; fun to clear n lines starting at (x,y).
			; args: hunk x y n
  clear-to-eol		; fun to clear to the end of a line from (x,y).
			; args: hunk x y
  clear-to-eow		; fun to clear to the end of a window from (x,y).
			; args: hunk x y
  open-line		; fun to open a line moving lines below it down.
			; args: hunk x y &optional n
  delete-line		; fun to delete a line moving lines below it up.
			; args: hunk x y &optional n
  insert-string		; fun to insert a string in the middle of a line.
			; args: hunk x y string &optional start end
  delete-char		; fun to delete a character from the middle of a line.
			; args: hunk x y &optional n
  (cursor-x 0)		; column the cursor is in.
  (cursor-y 0)		; line the cursor is on.
  standout-init-string  ; string to put terminal in standout mode.
  standout-end-string   ; string to take terminal out of standout mode.
  clear-to-eol-string	; string to cause device to clear to eol at (x,y).
  clear-string		; string to cause device to clear entire screen.
  open-line-string	; string to cause device to open a blank line.
  delete-line-string	; string to cause device to delete a line, moving
			; lines below it up.
  insert-init-string	; string to put terminal in insert mode.
  insert-char-init-string ; string to prepare terminal for insert-mode character.
  insert-char-end-string ; string to affect terminal after insert-mode character.
  insert-end-string	; string to take terminal out of insert mode.
  delete-init-string	; string to put terminal in delete mode.
  delete-char-string	; string to delete a character.
  delete-end-string	; string to take terminal out of delete mode.
  init-string		; device init string.
  cm-end-string		; takes device out of cursor motion mode.
  (cm-x-add-char nil)	; char-code to unconditionally add to x coordinate.
  (cm-y-add-char nil)	; char-code to unconditionally add to y coordinate.
  (cm-x-condx-char nil)	; char-code threshold for adding to x coordinate.
  (cm-y-condx-char nil)	; char-code threshold for adding to y coordinate.
  (cm-x-condx-add-char nil) ; char-code to conditionally add to x coordinate.
  (cm-y-condx-add-char nil) ; char-code to conditionally add to y coordinate.
  cm-string1		; initial substring of cursor motion string.
  cm-string2		; substring of cursor motion string between coordinates.
  cm-string3		; substring of cursor motion string after coordinates.
  cm-one-origin		; non-nil if need to add one to coordinates.
  cm-reversep		; non-nil if need to reverse coordinates.
  (cm-x-pad nil)	; nil, 0, 2, or 3 for places to pad.
			; 0 sends digit-chars.
  (cm-y-pad nil)	; nil, 0, 2, or 3 for places to pad.
			; 0 sends digit-chars.
  screen-image		; vector device-lines long of strings
			; device-columns long.
  ;;
  ;; This terminal's baud rate, or NIL for infinite.
  (speed nil :type (or (unsigned-byte 24) null)))


;;;; Device screen hunks and window-group.

;;; Window groups are used to keep track of the old width and height of a group
;;; so that when a configure-notify event is sent, we can determine if the size
;;; of the window actually changed or not.
;;;
(defstruct (window-group (:print-function %print-window-group)
			 (:constructor
			  make-window-group (xparent width height)))
  xparent
  width
  height)

(defun %print-window-group (object stream depth)
  (declare (ignore object depth))
  (format stream "#<Hemlock Window Group>"))

;;; Device-hunks are used to claim a piece of the screen and for ordering
;;; pieces of the screen.  Window motion primitives and splitting/merging
;;; primitives use hunks.  Hunks are somewhat of an interface between the
;;; portable and non-portable parts of screen management, between what the
;;; user sees on the screen and how Hemlock internals deal with window
;;; sequencing and creation.  Note: the echo area hunk is not hooked into
;;; the ring of other hunks via the next and previous fields.
;;;
(defstruct (device-hunk (:print-function %print-device-hunk))
  "This structure is used internally by Hemlock's screen management system."
  window		; Window displayed in this hunk.
  position		; Bottom Y position of hunk.
  height		; Height of hunk in pixels or lines.
  next			; Next and previous hunks.
  previous
  device)		; Display device hunk is on.

(defun %print-device-hunk (object stream depth)
  (declare (ignore depth))
  (format stream "#<Hemlock Device-Hunk ~D+~D~@[, ~S~]>"
	  (device-hunk-position object)
	  (device-hunk-height object)
	  (let* ((window (device-hunk-window object))
		 (buffer (if window (window-buffer window))))
	    (if buffer (buffer-name buffer)))))


;;; Bitmap hunks.
;;;
;;; The lock field is no longer used.  If events could be handled while we
;;; were in the middle of something with the hunk, then this could be set
;;; for exclusion purposes.
;;;
(defstruct (bitmap-hunk #|(:print-function %print-device-hunk)|#
			(:include device-hunk))
  width			      ; Pixel width.
  char-height	      	      ; Height of text body in characters.
  char-width		      ; Width in characters.
  xwindow		      ; X window for this hunk.
  gcontext                    ; X gcontext for xwindow.
  start			      ; Head of dis-line list (no dummy).
  end			      ; Exclusive end, i.e. nil if nil-terminated.
  modeline-dis-line	      ; Dis-line for modeline, or NIL if none.
  modeline-pos		      ; Position of modeline in pixels.
  (lock t)		      ; Something going on, set trashed if we're changed.
  trashed 		      ; Something bad happened, recompute image.
  font-family		      ; Font-family used in this window.
  input-handler		      ; Gets hunk, char, x, y when char read.
  changed-handler	      ; Gets hunk when size changed.
  (thumb-bar-p nil)	      ; True if we draw a thumb bar in the top border.
  window-group)		      ; The window-group to which this hunk belongs.


;;; Terminal hunks.
;;; 
(defstruct (tty-hunk #|(:print-function %print-device-hunk)|#
		     (:include device-hunk))
  text-position		; Bottom Y position of text in hunk.
  text-height)		; Number of lines of text.



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

(defsetf window-buffer %set-window-buffer
  "Change the buffer a window is mapped to.")

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
(defsetf current-window %set-current-window "Set the current window.")
(defsetf current-buffer %set-current-buffer
  "Set the current buffer, doing necessary stuff.")
(defsetf mark-kind %set-mark-kind "Used to set the kind of a mark.")
(defsetf buffer-region %set-buffer-region "Set a buffer's region.")
(defsetf command-name %set-command-name
  "Change a Hemlock command's name.")
(defsetf line-string %set-line-string
  "Replace the contents of a line.")
(defsetf last-command-type %set-last-command-type
  "Set the Last-Command-Type for use by the next command.")
(defsetf prefix-argument %set-prefix-argument
  "Set the prefix argument for the next command.")
(defsetf logical-key-event-p %set-logical-key-event-p
  "Change what Logical-Char= returns for the specified arguments.")
(defsetf window-font %set-window-font
  "Change the font-object associated with a font-number in a window.")
(defsetf default-font %set-default-font
  "Change the font-object associated with a font-number in new windows.")

(defsetf buffer-modeline-fields %set-buffer-modeline-fields
  "Sets the buffer's list of modeline fields causing all windows into buffer
   to be updated for the next redisplay.")
(defsetf modeline-field-name %set-modeline-field-name
  "Sets a modeline-field's name.  If one already exists with that name, an
   error is signaled.")
(defsetf modeline-field-width %set-modeline-field-width
  "Sets a modeline-field's width and updates all the fields for all windows
   in any buffer whose fields list contains the field.")
(defsetf modeline-field-function %set-modeline-field-function
  "Sets a modeline-field's function and updates this field for all windows in
   any buffer whose fields list contains the field.")

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
