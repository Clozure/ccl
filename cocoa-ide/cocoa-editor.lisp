;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

;;; In the double-float case, this is probably way too small.
;;; Traditionally, it's (approximately) the point at which
;;; a single-float stops being able to accurately represent
;;; integral values.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant large-number-for-text (cgfloat 1.0f7)))

;;; Compensates for the fact that Cocotron uses Mac font metrics and assumes
;;; the default Macintosh DPI (72) vs. that of Windows (96).
(defun font-size-kludge (size)
  #-cocotron size
  #+cocotron (* size (/ 96.0 72.0)))
    
(def-cocoa-default *editor-font* :font #'(lambda ()
					   (#/fontWithName:size:
					    ns:ns-font
                                            #+darwin-target
					    #@"Monaco"
                                            #-darwin-target
                                            #@"Courier New"
                                            (font-size-kludge 10.0)))
		   "Default font for editor windows")

(def-cocoa-default *editor-rows* :int 24 "Initial height of editor windows, in characters")
(def-cocoa-default *editor-columns* :int 80 "Initial width of editor windows, in characters")

(def-cocoa-default *editor-background-color* :color '(1.0 1.0 1.0 1.0) "Editor background color")
(def-cocoa-default *wrap-lines-to-window* :bool nil
		   "Soft wrap lines to window width")

(def-cocoa-default *use-screen-fonts* :bool t "Use bitmap screen fonts when available")

(def-cocoa-default *option-is-meta* :bool t "Use option key as meta?")

(defgeneric hemlock-view (ns-object))

(defmethod hemlock-view ((unknown t))
  nil)

(defgeneric hemlock-buffer (ns-object))

(defmethod hemlock-buffer ((unknown t))
  (let ((view (hemlock-view unknown)))
    (when view (hi::hemlock-view-buffer view))))

(defmacro nsstring-encoding-to-nsinteger (n)
  (ccl::target-word-size-case
   (32 `(ccl::u32->s32 ,n))
   (64 n)))

(defmacro nsinteger-to-nsstring-encoding (n)
  (ccl::target-word-size-case
   (32 `(ccl::s32->u32 ,n))
   (64 n)))

;;; Create a paragraph style, mostly so that we can set tabs reasonably.
(defun rme-create-paragraph-style (font line-break-mode)
  (let* ((p (make-instance 'ns:ns-mutable-paragraph-style))
	 (charwidth (fround (nth-value 1 (size-of-char-in-font font)))))
    (#/setLineBreakMode: p
                         (ecase line-break-mode
                           (:char #$NSLineBreakByCharWrapping)
                           (:word #$NSLineBreakByWordWrapping)
                           ;; This doesn't seem to work too well.
                           ((nil) #$NSLineBreakByClipping)))
    ;; Clear existing tab stops.
    (#/setTabStops: p (#/array ns:ns-array))
    ;; And set the "default tab interval".
    (#/setDefaultTabInterval: p (* *tab-width* charwidth))
    p))

(defun rme-create-text-attributes (&key (font *editor-font*)
				   (line-break-mode *default-line-break-mode*)
				   (color nil)
				   (obliqueness nil)
				   (stroke-width nil))
  (let* ((dict (make-instance 'ns:ns-mutable-dictionary :with-capacity 5)))
    (#/setObject:forKey: dict (rme-create-paragraph-style font line-break-mode)
			 #&NSParagraphStyleAttributeName)
    (#/setObject:forKey: dict font #&NSFontAttributeName)
    (when color
      (#/setObject:forKey: dict color #&NSForegroundColorAttributeName))
    (when stroke-width
      (#/setObject:forKey: dict (#/numberWithFloat: ns:ns-number stroke-width)
			   #&NSStrokeWidthAttributeName))
    (when obliqueness
      (#/setObject:forKey: dict (#/numberWithFloat: ns:ns-number obliqueness)
			   #&NSObliquenessAttributeName))
    dict))

(defun rme-make-editor-style-map ()
  (let* ((font *editor-font*)
	 (fm (#/sharedFontManager ns:ns-font-manager))
	 (bold-font (#/convertFont:toHaveTrait: fm font #$NSBoldFontMask))
	 (oblique-font (#/convertFont:toHaveTrait: fm font #$NSItalicFontMask))
	 (bold-oblique-font (#/convertFont:toHaveTrait:
			     fm font (logior #$NSItalicFontMask
					     #$NSBoldFontMask)))
	 (colors (vector (#/blackColor ns:ns-color)))
	 (fonts (vector font bold-font oblique-font bold-oblique-font))
	 (styles (make-instance 'ns:ns-mutable-array)))
    (dotimes (c (length colors))
      (dotimes (i 4)
	(let* ((mask (logand i 3))
	       (f (svref fonts mask)))
	  (#/addObject: styles 
			(rme-create-text-attributes :font f
						    :color (svref colors c)
						    :obliqueness
						    (if (logbitp 1 i)
						      (when (eql f font)
							0.15f0))
						    :stroke-width
						    (if (logbitp 0 i)
						      (when (eql f font)
							-10.0f0)))))))
    styles))

(defun make-editor-style-map ()
  (rme-make-editor-style-map))

#+nil
(defun make-editor-style-map ()
  (let* ((font-name *default-font-name*)
	 (font-size *default-font-size*)
         (font (default-font :name font-name :size font-size))
         (bold-font (let* ((f (default-font :name font-name :size font-size :attributes '(:bold))))
                      (unless (eql f font) f)))
         (oblique-font (let* ((f (default-font :name font-name :size font-size :attributes '(:italic))))
                      (unless (eql f font) f)))
         (bold-oblique-font (let* ((f (default-font :name font-name :size font-size :attributes '(:bold :italic))))
                      (unless (eql f font) f)))
	 (color-class (find-class 'ns:ns-color))
	 (colors (vector (#/blackColor color-class)))
	 (styles (make-instance 'ns:ns-mutable-array
                                :with-capacity (the fixnum (* 4 (length colors)))))
         (bold-stroke-width -10.0f0)
         (fonts (vector font (or bold-font font) (or oblique-font font) (or bold-oblique-font font)))
         (real-fonts (vector font bold-font oblique-font bold-oblique-font))
	 (s 0))
    (declare (dynamic-extent fonts real-fonts colors))
    (dotimes (c (length colors))
      (dotimes (i 4)
        (let* ((mask (logand i 3)))
          (#/addObject: styles
                        (create-text-attributes :font (svref fonts mask)
                                                :color (svref colors c)
                                                :obliqueness
                                                (if (logbitp 1 i)
                                                  (unless (svref real-fonts mask)
                                                    0.15f0))
                                                :stroke-width
                                                (if (logbitp 0 i)
                                                  (unless (svref real-fonts mask)
                                                    bold-stroke-width)))))
	(incf s)))
    (#/retain styles)))

(defun make-hemlock-buffer (&rest args)
  (let* ((buf (apply #'hi::make-buffer args)))
    (assert buf)
    buf))

;;; Define some key event modifiers and keysym codes

(hi:define-modifier-bit #$NSShiftKeyMask "Shift")
(hi:define-modifier-bit #$NSControlKeyMask "Control")
(hi:define-modifier-bit #$NSAlternateKeyMask "Meta")
(hi:define-modifier-bit #$NSAlphaShiftKeyMask "Lock")

(hi:define-keysym-code :F1 #$NSF1FunctionKey)
(hi:define-keysym-code :F2 #$NSF2FunctionKey)
(hi:define-keysym-code :F3 #$NSF3FunctionKey)
(hi:define-keysym-code :F4 #$NSF4FunctionKey)
(hi:define-keysym-code :F5 #$NSF5FunctionKey)
(hi:define-keysym-code :F6 #$NSF6FunctionKey)
(hi:define-keysym-code :F7 #$NSF7FunctionKey)
(hi:define-keysym-code :F8 #$NSF8FunctionKey)
(hi:define-keysym-code :F9 #$NSF9FunctionKey)
(hi:define-keysym-code :F10 #$NSF10FunctionKey)
(hi:define-keysym-code :F11 #$NSF11FunctionKey)
(hi:define-keysym-code :F12 #$NSF12FunctionKey)
(hi:define-keysym-code :F13 #$NSF13FunctionKey)
(hi:define-keysym-code :F14 #$NSF14FunctionKey)
(hi:define-keysym-code :F15 #$NSF15FunctionKey)
(hi:define-keysym-code :F16 #$NSF16FunctionKey)
(hi:define-keysym-code :F17 #$NSF17FunctionKey)
(hi:define-keysym-code :F18 #$NSF18FunctionKey)
(hi:define-keysym-code :F19 #$NSF19FunctionKey)
(hi:define-keysym-code :F20 #$NSF20FunctionKey)
(hi:define-keysym-code :F21 #$NSF21FunctionKey)
(hi:define-keysym-code :F22 #$NSF22FunctionKey)
(hi:define-keysym-code :F23 #$NSF23FunctionKey)
(hi:define-keysym-code :F24 #$NSF24FunctionKey)
(hi:define-keysym-code :F25 #$NSF25FunctionKey)
(hi:define-keysym-code :F26 #$NSF26FunctionKey)
(hi:define-keysym-code :F27 #$NSF27FunctionKey)
(hi:define-keysym-code :F28 #$NSF28FunctionKey)
(hi:define-keysym-code :F29 #$NSF29FunctionKey)
(hi:define-keysym-code :F30 #$NSF30FunctionKey)
(hi:define-keysym-code :F31 #$NSF31FunctionKey)
(hi:define-keysym-code :F32 #$NSF32FunctionKey)
(hi:define-keysym-code :F33 #$NSF33FunctionKey)
(hi:define-keysym-code :F34 #$NSF34FunctionKey)
(hi:define-keysym-code :F35 #$NSF35FunctionKey)

;;; Upper right key bank.
;;;
(hi:define-keysym-code :Printscreen #$NSPrintScreenFunctionKey)
;; Couldn't type scroll lock.
(hi:define-keysym-code :Pause #$NSPauseFunctionKey)

;;; Middle right key bank.
;;;
(hi:define-keysym-code :Insert #$NSInsertFunctionKey)
(hi:define-keysym-code :Del #$NSDeleteFunctionKey)
(hi:define-keysym-code :Home #$NSHomeFunctionKey)
(hi:define-keysym-code :Pageup #$NSPageUpFunctionKey)
(hi:define-keysym-code :End #$NSEndFunctionKey)
(hi:define-keysym-code :Pagedown #$NSPageDownFunctionKey)

;;; Arrows.
;;;
(hi:define-keysym-code :Leftarrow #$NSLeftArrowFunctionKey)
(hi:define-keysym-code :Uparrow #$NSUpArrowFunctionKey)
(hi:define-keysym-code :Downarrow #$NSDownArrowFunctionKey)
(hi:define-keysym-code :Rightarrow #$NSRightArrowFunctionKey)

;;;

;(hi:define-keysym-code :linefeed 65290)





;;; We want to display a Hemlock buffer in a "pane" (an on-screen
;;; view) which in turn is presented in a "frame" (a Cocoa window).  A
;;; 1:1 mapping between frames and panes seems to fit best into
;;; Cocoa's document architecture, but we should try to keep the
;;; concepts separate (in case we come up with better UI paradigms.)
;;; Each pane has a modeline (which describes attributes of the
;;; underlying document); each frame has an echo area (which serves
;;; to display some commands' output and to provide multi-character
;;; input.)


;;; I'd pretty much concluded that it wouldn't be possible to get the
;;; Cocoa text system (whose storage model is based on NSString
;;; NSMutableAttributedString, NSTextStorage, etc.) to get along with
;;; Hemlock, and (since the whole point of using Hemlock was to be
;;; able to treat an editor buffer as a rich lisp data structure) it
;;; seemed like it'd be necessary to toss the higher-level Cocoa text
;;; system and implement our own scrolling, redisplay, selection
;;; ... code.
;;;
;;; Mikel Evins pointed out that NSString and friends were
;;; abstract classes and that there was therefore no reason (in
;;; theory) not to implement a thin wrapper around a Hemlock buffer
;;; that made it act like an NSString.  As long as the text system can
;;; ask a few questions about the NSString (its length and the
;;; character and attributes at a given location), it's willing to
;;; display the string in a scrolling, mouse-selectable NSTextView;
;;; as long as Hemlock tells the text system when and how the contents
;;; of the abstract string changes, Cocoa will handle the redisplay
;;; details.
;;;


;;; Hemlock-buffer-string objects:

(defclass hemlock-buffer-string (ns:ns-string)
    ((cache :initform nil :initarg :cache :accessor hemlock-buffer-string-cache))
  (:metaclass ns:+ns-object))


  
(defmethod hemlock-buffer ((self hemlock-buffer-string))
  (let ((cache (hemlock-buffer-string-cache self)))
    (when cache
      (hemlock-buffer cache))))

;;; Cocoa wants to treat the buffer as a linear array of characters;
;;; Hemlock wants to treat it as a doubly-linked list of lines, so
;;; we often have to map between an absolute position in the buffer
;;; and a relative position on a line.  We can certainly do that
;;; by counting the characters in preceding lines every time that we're
;;; asked, but we're often asked to map a sequence of nearby positions
;;; and wind up repeating a lot of work.  Caching the results of that
;;; work seems to speed things up a bit in many cases; this data structure
;;; is used in that process.  (It's also the only way to get to the
;;; actual underlying Lisp buffer from inside the network of text-system
;;; objects.)

(defstruct buffer-cache 
  buffer				; the hemlock buffer
  buflen				; length of buffer, if known
  workline				; cache for character-at-index
  workline-offset			; cached offset of workline
  workline-length			; length of cached workline
  workline-start-font-index		; current font index at start of workline
  )

(objc:defmethod (#/dealloc :void) ((self hemlock-buffer-string))
  (let* ((cache (hemlock-buffer-string-cache self))
         (buffer (if cache (buffer-cache-buffer cache))))
    (when buffer
      (setf (buffer-cache-buffer cache) nil
            (slot-value self 'cache) nil
            (hi::buffer-document buffer) nil)
      (when (eq buffer hi::*current-buffer*)
        (setf hi::*current-buffer* nil))
      (hi::delete-buffer buffer)))
  (objc:remove-lisp-slots self)
  (call-next-method))

(defmethod hemlock-buffer ((self buffer-cache))
  (buffer-cache-buffer self))

;;; Initialize (or reinitialize) a buffer cache, so that it points
;;; to the buffer's first line (which is the only line whose
;;; absolute position will never change).  Code which modifies the
;;; buffer generally has to call this, since any cached information
;;; might be invalidated by the modification.

(defun reset-buffer-cache (d &optional (buffer (buffer-cache-buffer d)
						buffer-p))
  (when buffer-p (setf (buffer-cache-buffer d) buffer))
  (let* ((hi::*current-buffer* buffer)
         (workline (hi::mark-line
		    (hi::buffer-start-mark buffer))))
    (setf (buffer-cache-buflen d) (hemlock-buffer-length buffer)
	  (buffer-cache-workline-offset d) 0
	  (buffer-cache-workline d) workline
	  (buffer-cache-workline-length d) (hi::line-length workline)
	  (buffer-cache-workline-start-font-index d) 0)
    d))


(defun adjust-buffer-cache-for-insertion (display pos n)
  (if (buffer-cache-workline display)
    (let* ((hi::*current-buffer* (buffer-cache-buffer display)))
      (if (> (buffer-cache-workline-offset display) pos)
        (incf (buffer-cache-workline-offset display) n)
        (when (>= (+ (buffer-cache-workline-offset display)
                     (buffer-cache-workline-length display))
                  pos)
          (setf (buffer-cache-workline-length display)
                (hi::line-length (buffer-cache-workline display)))))
      (incf (buffer-cache-buflen display) n))
    (reset-buffer-cache display)))

          
           

;;; Update the cache so that it's describing the current absolute
;;; position.

(defun update-line-cache-for-index (cache index)
  (let* ((buffer (buffer-cache-buffer cache))
         (hi::*current-buffer* buffer)
         (line (or
		(buffer-cache-workline cache)
		(progn
		  (reset-buffer-cache cache)
		  (buffer-cache-workline cache))))
	 (pos (buffer-cache-workline-offset cache))
	 (len (buffer-cache-workline-length cache))
	 (moved nil))
    (loop
      (when (and (>= index pos)
		   (< index (1+ (+ pos len))))
	  (let* ((idx (- index pos)))
	    (when moved
	      (setf (buffer-cache-workline cache) line
		    (buffer-cache-workline-offset cache) pos
		    (buffer-cache-workline-length cache) len))
	    (return (values line idx))))
      (setq moved t)
      (if (< index pos)
	(setq line (hi::line-previous line)
	      len (hi::line-length line)
	      pos (1- (- pos len)))
	(setq line (hi::line-next line)
	      pos (1+ (+ pos len))
	      len (hi::line-length line))))))

;;; Ask Hemlock to count the characters in the buffer.
(defun hemlock-buffer-length (buffer)
  (let* ((hi::*current-buffer* buffer))
    (hemlock::count-characters (hemlock::buffer-region buffer))))

;;; Find the line containing (or immediately preceding) index, which is
;;; assumed to be less than the buffer's length.  Return the character
;;; in that line or the trailing #\newline, as appropriate.
(defun hemlock-char-at-index (cache index)
  (let* ((hi::*current-buffer* (buffer-cache-buffer cache)))
    (multiple-value-bind (line idx) (update-line-cache-for-index cache index)
      (let* ((len (hemlock::line-length line)))
        (if (< idx len)
          (hemlock::line-character line idx)
          #\newline)))))

;;; Given an absolute position, move the specified mark to the appropriate
;;; offset on the appropriate line.
(defun move-hemlock-mark-to-absolute-position (mark cache abspos)
  ;; TODO: figure out if updating the cache matters, and if not, use hi:move-to-absolute-position.
  (let* ((hi::*current-buffer* (buffer-cache-buffer cache)))
    (hi::move-to-absolute-position mark abspos)
    #+old
    (multiple-value-bind (line idx) (update-line-cache-for-index cache abspos)
      #+debug
      (#_NSLog #@"Moving point from current pos %d to absolute position %d"
               :int (hi:mark-absolute-position mark)
               :int abspos)
      (hemlock::move-to-position mark idx line)
      #+debug
      (#_NSLog #@"Moved mark to %d" :int (hi:mark-absolute-position mark)))))

;;; Return the length of the abstract string, i.e., the number of
;;; characters in the buffer (including implicit newlines.)
(objc:defmethod (#/length :<NSUI>nteger) ((self hemlock-buffer-string))
  (let* ((cache (hemlock-buffer-string-cache self)))
    (or (buffer-cache-buflen cache)
        (setf (buffer-cache-buflen cache)
              (let* ((buffer (buffer-cache-buffer cache)))
		(hemlock-buffer-length buffer))))))



;;; Return the character at the specified index (as a :unichar.)

(objc:defmethod (#/characterAtIndex: :unichar)
    ((self hemlock-buffer-string) (index :<NSUI>nteger))
  #+debug
  (#_NSLog #@"Character at index: %d" :<NSUI>nteger index)
  (char-code (hemlock-char-at-index (hemlock-buffer-string-cache self) index)))

(objc:defmethod (#/getCharacters:range: :void)
    ((self hemlock-buffer-string)
     (buffer (:* :unichar))
     (r :<NSR>ange))
  (let* ((cache (hemlock-buffer-string-cache self))
         (index (ns:ns-range-location r))
         (length (ns:ns-range-length r))
         (hi::*current-buffer* (buffer-cache-buffer cache)))
    #+debug
    (#_NSLog #@"get characters: %d/%d"
             :<NSUI>nteger index
             :<NSUI>nteger length)
    (multiple-value-bind (line idx) (update-line-cache-for-index cache index)
      (let* ((len (hemlock::line-length line)))
        (do* ((i 0 (1+ i)))
             ((= i length))
          (cond ((< idx len)
                 (setf (paref buffer (:* :unichar) i)
                       (char-code (hemlock::line-character line idx)))
                 (incf idx))
                (t
                 (setf (paref buffer (:* :unichar) i)
                       (char-code #\Newline)
                       line (hi::line-next line)
                       len (if line (hi::line-length line) 0)
                       idx 0))))))))


(objc:defmethod (#/getLineStart:end:contentsEnd:forRange: :void)
    ((self hemlock-buffer-string)
     (startptr (:* :<NSUI>nteger))
     (endptr (:* :<NSUI>nteger))
     (contents-endptr (:* :<NSUI>nteger))
     (r :<NSR>ange))
  (let* ((cache (hemlock-buffer-string-cache self))
         (index (pref r :<NSR>ange.location))
         (length (pref r :<NSR>ange.length))
	 (hi::*current-buffer* (buffer-cache-buffer cache)))
    #+debug
    (#_NSLog #@"get line start: %d/%d"
             :unsigned index
             :unsigned length)
    (update-line-cache-for-index cache index)
    (unless (%null-ptr-p startptr)
      ;; Index of the first character in the line which contains
      ;; the start of the range.
      (setf (pref startptr :<NSUI>nteger)
            (buffer-cache-workline-offset cache)))
    (unless (%null-ptr-p endptr)
      ;; Index of the newline which terminates the line which
      ;; contains the start of the range.
      (setf (pref endptr :<NSUI>nteger)
            (+ (buffer-cache-workline-offset cache)
               (buffer-cache-workline-length cache))))
    (unless (%null-ptr-p contents-endptr)
      ;; Index of the newline which terminates the line which
      ;; contains the start of the range.
      (unless (zerop length)
        (update-line-cache-for-index cache (+ index length)))
      (setf (pref contents-endptr :<NSUI>nteger)
            (1+ (+ (buffer-cache-workline-offset cache)
                   (buffer-cache-workline-length cache)))))))

;;; For debugging, mostly: make the printed representation of the string
;;; reference the named Hemlock buffer.
(objc:defmethod #/description ((self hemlock-buffer-string))
  (let* ((cache (hemlock-buffer-string-cache self))
	 (b (buffer-cache-buffer cache)))
    (with-cstrs ((s (format nil "~a" b)))
      (#/stringWithFormat: ns:ns-string #@"<%s for %s>" (#_object_getClassName self) s))))



;;; hemlock-text-storage objects
(defclass hemlock-text-storage (ns:ns-text-storage)
    ((string :foreign-type :id)
     (hemlock-string :foreign-type :id)
     (edit-count :foreign-type :int)
     (mirror :foreign-type :id)
     (styles :foreign-type :id)
     (selection-set-by-search :foreign-type :<BOOL>))
  (:metaclass ns:+ns-object))
(declaim (special hemlock-text-storage))

(defmethod hemlock-buffer ((self hemlock-text-storage))
  (let ((string (slot-value self 'hemlock-string)))
    (unless (%null-ptr-p string)
      (hemlock-buffer string))))

;;; This is only here so that calls to it can be logged for debugging.
#+debug
(objc:defmethod (#/lineBreakBeforeIndex:withinRange: :<NSUI>nteger)
    ((self hemlock-text-storage)
     (index :<NSUI>nteger)
     (r :<NSR>ange))
  (#_NSLog #@"Line break before index: %d within range: %@"
           :unsigned index
           :id (#_NSStringFromRange r))
  (call-next-method index r))




;;; Return true iff we're inside a "beginEditing/endEditing" pair
(objc:defmethod (#/editingInProgress :<BOOL>) ((self hemlock-text-storage))
  ;; This is meaningless outside the event thread, since you can't tell what
  ;; other edit-count changes have already been queued up for execution on
  ;; the event thread before it gets to whatever you might queue up next.
  (assume-cocoa-thread)
  (> (slot-value self 'edit-count) 0))

(defmethod assume-not-editing ((ts hemlock-text-storage))
  #+debug NIL (assert (eql (slot-value ts 'edit-count) 0)))

(defun textstorage-note-insertion-at-position (self pos n)
  (ns:with-ns-range (r pos 0)
    (#/edited:range:changeInLength: self #$NSTextStorageEditedCharacters r n)
    (setf (ns:ns-range-length r) n)
    (#/edited:range:changeInLength: self #$NSTextStorageEditedAttributes r 0)))



;;; This runs on the main thread; it synchronizes the "real" NSMutableAttributedString
;;; with the hemlock string and informs the textstorage of the insertion.
(objc:defmethod (#/noteHemlockInsertionAtPosition:length:extra: :void) ((self hemlock-text-storage)
                                                                  (pos :<NSI>nteger)
                                                                  (n :<NSI>nteger)
                                                                  (extra :<NSI>nteger))
  (declare (ignorable extra))
  (assume-cocoa-thread)
  (let* ((mirror (#/mirror self))
	 (hemlock-string (#/hemlockString self))
         (display (hemlock-buffer-string-cache hemlock-string))
         (buffer (buffer-cache-buffer display))
         (hi::*current-buffer* buffer)
         (attributes (buffer-active-font-attributes buffer))
         (document (#/document self))
	 (undo-mgr (and document (#/undoManager document))))
    #+debug 
    (#_NSLog #@"insert: pos = %ld, n = %ld" :long pos :long n)
    ;; We need to update the hemlock string mirror here so that #/substringWithRange:
    ;; will work on the hemlock buffer string.
    (adjust-buffer-cache-for-insertion display pos n)
    (update-line-cache-for-index display pos)
    (let* ((replacestring (#/substringWithRange: hemlock-string (ns:make-ns-range pos n))))
      (ns:with-ns-range (replacerange pos 0)
        (#/replaceCharactersInRange:withString:
         mirror replacerange replacestring))
      #+cocotron
      (#/updateChangeCount: document #$NSChangeDone)
      (when (and undo-mgr (not (#/isUndoing undo-mgr)))
        (#/replaceCharactersAtPosition:length:withString:
	 (#/prepareWithInvocationTarget: undo-mgr self)
	 pos n #@"")))
    (#/setAttributes:range: mirror attributes (ns:make-ns-range pos n))
    (textstorage-note-insertion-at-position self pos n)))

(objc:defmethod (#/noteHemlockDeletionAtPosition:length:extra: :void) ((self hemlock-text-storage)
                                                                       (pos :<NSI>nteger)
                                                                       (n :<NSI>nteger)
                                                                       (extra :<NSI>nteger))
  (declare (ignorable extra))
  #+debug
  (#_NSLog #@"delete: pos = %ld, n = %ld" :long pos :long n)
  (ns:with-ns-range (range pos n)
    (let* ((mirror (#/mirror self))
	   (deleted-string (#/substringWithRange: (#/string mirror) range))
	   (document (#/document self))
	   (undo-mgr (and document (#/undoManager document)))
	   (display (hemlock-buffer-string-cache (#/hemlockString self))))
      ;; It seems to be necessary to call #/edited:range:changeInLength: before
      ;; deleting from the mirror attributed string.  It's not clear whether this
      ;; is also true of insertions and modifications.
      (#/edited:range:changeInLength: self (logior #$NSTextStorageEditedCharacters
						   #$NSTextStorageEditedAttributes)
				      range (- n))
      (#/deleteCharactersInRange: mirror range)
      #+cocotron
      (#/updateChangeCount: document #$NSChangeDone)      
      (when (and undo-mgr (not (#/isUndoing undo-mgr)))
        (#/replaceCharactersAtPosition:length:withString:
	 (#/prepareWithInvocationTarget: undo-mgr self)
	 pos 0 deleted-string))
      (reset-buffer-cache display)
      (update-line-cache-for-index display pos))))

(objc:defmethod (#/noteHemlockModificationAtPosition:length:extra: :void) ((self hemlock-text-storage)
                                                                           (pos :<NSI>nteger)
                                                                           (n :<NSI>nteger)
                                                                           (extra :<NSI>nteger))
  (declare (ignorable extra))
  #+debug
  (#_NSLog #@"modify: pos = %ld, n = %ld" :long pos :long n)
  (ns:with-ns-range (range pos n)
    (let* ((hemlock-string (#/hemlockString self))
	   (mirror (#/mirror self))
	   (deleted-string (#/substringWithRange: (#/string mirror) range))
	   (document (#/document self))
	   (undo-mgr (and document (#/undoManager document))))
      (#/replaceCharactersInRange:withString:
       mirror range (#/substringWithRange: hemlock-string range))
      (#/edited:range:changeInLength: self (logior #$NSTextStorageEditedCharacters
                                                   #$NSTextStorageEditedAttributes) range 0)
      #+cocotron
      (#/updateChangeCount: document #$NSChangeDone)      
      (when (and undo-mgr (not (#/isUndoing undo-mgr)))
        (#/replaceCharactersAtPosition:length:withString:
	 (#/prepareWithInvocationTarget: undo-mgr self)
	 pos n deleted-string)))))

(objc:defmethod (#/noteHemlockAttrChangeAtPosition:length:fontNum: :void) ((self hemlock-text-storage)
                                                                           (pos :<NSI>nteger)
                                                                           (n :<NSI>nteger)
                                                                           (fontnum :<NSI>nteger))
  (ns:with-ns-range (range pos n)
    (#/setAttributes:range: (#/mirror self) (#/objectAtIndex: (#/styles self) fontnum) range)
    (#/edited:range:changeInLength: self #$NSTextStorageEditedAttributes range 0)))


(defloadvar *buffer-change-invocation*
    (with-autorelease-pool
        (#/retain
                   (#/invocationWithMethodSignature: ns:ns-invocation
                                                     (#/instanceMethodSignatureForSelector:
                                                      hemlock-text-storage
                                            (@selector #/noteHemlockInsertionAtPosition:length:extra:))))))

(defstatic *buffer-change-invocation-lock* (make-lock))

         
         
(objc:defmethod (#/beginEditing :void) ((self hemlock-text-storage))
  (assume-cocoa-thread)
  (with-slots (edit-count) self
    #+debug
    (#_NSLog #@"begin-editing")
    (incf edit-count)
    #+debug
    (#_NSLog #@"after beginEditing on %@ edit-count now = %d" :id self :int edit-count)
    (call-next-method)))

(objc:defmethod (#/endEditing :void) ((self hemlock-text-storage))
  (assume-cocoa-thread)
  (with-slots (edit-count) self
    #+debug
    (#_NSLog #@"end-editing")
    (call-next-method)
    (assert (> edit-count 0))
    (decf edit-count)
    #+debug
    (#_NSLog #@"after endEditing on %@, edit-count now = %d" :id self :int edit-count)))



  

;;; Access the string.  It'd be nice if this was a generic function;
;;; we could have just made a reader method in the class definition.



(objc:defmethod #/string ((self hemlock-text-storage))
  (slot-value self 'string))

(objc:defmethod #/mirror ((self hemlock-text-storage))
  (slot-value self 'mirror))

(objc:defmethod #/hemlockString ((self hemlock-text-storage))
  (slot-value self 'hemlock-string))

(objc:defmethod #/styles ((self hemlock-text-storage))
  (slot-value self 'styles))

(objc:defmethod #/document ((self hemlock-text-storage))
  (or
   (let* ((string (#/hemlockString self)))
     (unless (%null-ptr-p string)
       (let* ((cache (hemlock-buffer-string-cache string)))
         (when cache
           (let* ((buffer (buffer-cache-buffer cache)))
             (when buffer
               (hi::buffer-document buffer)))))))
   +null-ptr+))


#-cocotron
(objc:defmethod #/initWithString: ((self hemlock-text-storage) s)
  (setq s (%inc-ptr s 0))
  (let* ((newself (#/init self))
         (styles (make-editor-style-map))
         (mirror (make-instance ns:ns-mutable-attributed-string
                                   :with-string s
                                   :attributes (#/objectAtIndex: styles 0))))
    (declare (type hemlock-text-storage newself))
    (setf (slot-value newself 'styles) styles)
    (setf (slot-value newself 'hemlock-string) s)
    (setf (slot-value newself 'mirror) mirror)
    (setf (slot-value newself 'string) (#/retain (#/string mirror)))
    newself))

#+cocotron
(objc:defmethod #/initWithString: ((self hemlock-text-storage) s)
  (setq s (%inc-ptr s 0))
  (let* ((styles (make-editor-style-map))
         (mirror (make-instance ns:ns-mutable-attributed-string
                                   :with-string s
                                   :attributes (#/objectAtIndex: styles 0)))
         (string (#/retain (#/string mirror)))
         (newself (call-next-method string)))
    (declare (type hemlock-text-storage newself))
    (setf (slot-value newself 'styles) styles)
    (setf (slot-value newself 'hemlock-string) s)
    (setf (slot-value newself 'mirror) mirror)
    (setf (slot-value newself 'string) string)
    newself))

;;; Should generally only be called after open/revert.
(objc:defmethod (#/updateMirror :void) ((self hemlock-text-storage))
  (with-slots (hemlock-string mirror styles) self
    (#/replaceCharactersInRange:withString: mirror (ns:make-ns-range 0 (#/length mirror)) hemlock-string)
    (#/setAttributes:range: mirror (#/objectAtIndex: styles 0) (ns:make-ns-range 0 (#/length mirror)))))

;;; This is the only thing that's actually called to create a
;;; hemlock-text-storage object.  (It also creates the underlying
;;; hemlock-buffer-string.)
(defun make-textstorage-for-hemlock-buffer (buffer)
  (make-instance 'hemlock-text-storage
                 :with-string
                 (make-instance
                  'hemlock-buffer-string
                  :cache
                  (reset-buffer-cache
                   (make-buffer-cache)
                   buffer))))

(objc:defmethod #/attributesAtIndex:effectiveRange:
    ((self hemlock-text-storage) (index :<NSUI>nteger) (rangeptr (* :<NSR>ange)))
  #+debug
  (#_NSLog #@"Attributes at index: %lu storage %@" :<NSUI>nteger index :id self)
  (with-slots (mirror styles) self
    (when (>= index (#/length mirror))
      (#_NSLog #@"Bounds error - Attributes at index: %lu  edit-count: %d mirror: %@ layout: %@" :<NSUI>nteger index ::unsigned (slot-value self 'edit-count) :id mirror :id (#/objectAtIndex: (#/layoutManagers self) 0))
      (ccl::dbg))
    (let* ((attrs (#/attributesAtIndex:effectiveRange: mirror index rangeptr)))
      (when (eql 0 (#/count attrs))
        (#_NSLog #@"No attributes ?")
        (ns:with-ns-range (r)
          (#/attributesAtIndex:longestEffectiveRange:inRange:
           mirror index r (ns:make-ns-range 0 (#/length mirror)))
          (setq attrs (#/objectAtIndex: styles 0))
          (#/setAttributes:range: mirror attrs r)))
      attrs)))

(objc:defmethod (#/replaceCharactersAtPosition:length:withString: :void)
    ((self hemlock-text-storage) (pos <NSUI>nteger) (len <NSUI>nteger) string)
  (let* ((document (#/document self))
	 (undo-mgr (and document (#/undoManager document))))
    (when (and undo-mgr (not (#/isRedoing undo-mgr)))
      (let ((replaced-string (#/substringWithRange: (#/hemlockString self) (ns:make-ns-range pos len))))
	(#/replaceCharactersAtPosition:length:withString:
	 (#/prepareWithInvocationTarget: undo-mgr self)
	 pos (#/length string) replaced-string)))
    (ns:with-ns-range (r pos len)
      (#/beginEditing self)
      (unwind-protect
           (#/replaceCharactersInRange:withString: self r string)
        (#/endEditing self)))))

;; In theory (though not yet in practice) we allow for a buffer to be shown in multiple
;; windows, and any change to a buffer through one window has to be reflected in all of
;; them.  Once hemlock really supports multiple views of a buffer, it will have some
;; mechanims to ensure that.
;; In Cocoa, we get some messages for the buffer (i.e. the document or the textstorage)
;; with no reference to a view.  There used to be code here that tried to do special-
;; case stuff for all views on the buffer, but that's not necessary, because as long
;; as hemlock doesn't support it, there will only be one view, and as soon as hemlock
;; does support it, will take care of updating all other views.  So all we need is to
;; get our hands on one of the views and do whatever it is through it.
(defun front-view-for-buffer (buffer)
  (loop
     with win-arr =  (#/orderedWindows *NSApp*)
     for i from 0 below (#/count win-arr) as w = (#/objectAtIndex: win-arr i)
     thereis (and (eq (hemlock-buffer w) buffer) (hemlock-view w))))


;;; Modify the hemlock buffer; don't change attributes.
(objc:defmethod (#/replaceCharactersInRange:withString: :void)
    ((self hemlock-text-storage) (r :<NSR>ange) string)
  (let* ((buffer (hemlock-buffer self))
         (hi::*current-buffer* buffer)
         (position (pref r :<NSR>ange.location))
	 (length (pref r :<NSR>ange.length))
	 (lisp-string (if (> (#/length string) 0) (lisp-string-from-nsstring string)))
         (view (front-view-for-buffer buffer))
         (edit-count (slot-value self 'edit-count)))
    ;; #!#@#@* find panel neglects to call #/beginEditing / #/endEditing.
    (when (eql 0 edit-count)
      (#/beginEditing self))
    (unwind-protect
         (hi::with-mark ((m (hi::buffer-point buffer)))
           (hi::move-to-absolute-position m position)
           (when (> length 0)
             (hi::delete-characters m length))
           (when lisp-string
             (hi::insert-string m lisp-string)))
      (when (eql 0 edit-count)
        (#/endEditing self)))
    (when view
      (setf (hi::hemlock-view-quote-next-p view) nil))))

(objc:defmethod (#/setAttributes:range: :void) ((self hemlock-text-storage)
                                                attributes
                                                (r :<NSR>ange))
  #+debug
  (#_NSLog #@"Set attributes: %@ at %d/%d" :id attributes :int (pref r :<NSR>ange.location) :int (pref r :<NSR>ange.length))
  (with-slots (mirror) self
    (#/setAttributes:range: mirror attributes r)
      #+debug
      (#_NSLog #@"Assigned attributes = %@" :id (#/attributesAtIndex:effectiveRange: mirror (pref r :<NSR>ange.location) +null-ptr+))))

(defun for-each-textview-using-storage (textstorage f)
  (let* ((layouts (#/layoutManagers textstorage)))
    (unless (%null-ptr-p layouts)
      (dotimes (i (#/count layouts))
	(let* ((layout (#/objectAtIndex: layouts i))
	       (containers (#/textContainers layout)))
	  (unless (%null-ptr-p containers)
	    (dotimes (j (#/count containers))
	      (let* ((container (#/objectAtIndex: containers j))
		     (tv (#/textView container)))
		(funcall f tv)))))))))

;;; Again, it's helpful to see the buffer name when debugging.
(objc:defmethod #/description ((self hemlock-text-storage))
  (#/stringWithFormat: ns:ns-string #@"%s : string %@" (#_object_getClassName self) (slot-value self 'hemlock-string)))

(defun close-hemlock-textstorage (ts)
  (declare (type hemlock-text-storage ts))
  (when (slot-exists-p ts 'styles)
    (with-slots (styles) ts
      (#/release styles)
      (setq styles +null-ptr+)))
  (let* ((hemlock-string (slot-value ts 'hemlock-string)))
    (setf (slot-value ts 'hemlock-string) +null-ptr+)
    (unless (%null-ptr-p hemlock-string)
      (#/release hemlock-string))))


;;; Mostly experimental, so that we can see what happens when a 
;;; real typesetter is used.
#-cocotron
(progn
(defclass hemlock-ats-typesetter (ns:ns-ats-typesetter)
    ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/layoutGlyphsInLayoutManager:startingAtGlyphIndex:maxNumberOfLineFragments:nextGlyphIndex: :void)
    ((self hemlock-ats-typesetter)
     layout-manager
     (start-index :<NSUI>nteger)
     (max-lines :<NSUI>nteger)
     (next-index (:* :<NSUI>nteger)))
  (#_NSLog #@"layoutGlyphs: start = %d, maxlines = %d" :int start-index :int max-lines)
  (call-next-method layout-manager start-index max-lines next-index))
)

;;; An abstract superclass of the main and echo-area text views.
(defclass hemlock-textstorage-text-view (ns::ns-text-view)
    ((paren-highlight-enabled :foreign-type #>BOOL :accessor text-view-paren-highlight-enabled)
     (peer :foreign-type :id)
     (paren-highlighting :initform nil :accessor text-view-paren-highlighting)
     (hemlock-view :initform nil)
     (selection-info :initform nil :accessor view-selection-info))
  (:metaclass ns:+ns-object))
(declaim (special hemlock-textstorage-text-view))

#| causes more problems than it solves.
   removed until a better implementation manifests itself --me
(objc:defmethod (#/performDragOperation: #>BOOL)
    ((self hemlock-textstorage-text-view)
     (sender :id))
  (let* ((pboard (#/draggingPasteboard sender))
         (pbTypes (#/arrayWithObjects: ns:ns-array #&NSFilenamesPboardType
                                       +null-ptr+))
         (available-type (#/availableTypeFromArray: pboard pbTypes)))
    (if (%null-ptr-p available-type)
        (progn (log-debug "No data available of type NSFilenamesPboardType")
               (call-next-method sender))
        (let* ((plist (#/propertyListForType: pboard #&NSFilenamesPboardType)))
          (cond
            ;; we found NSFilenamesPboardType and it's an array of pathnames
            ((#/isKindOfClass: plist ns:ns-array)
             (with-autorelease-pool
               (let* ((strings-for-dropped-objects 
                       (mapcar (lambda (d) 
                                 (if (#/isKindOfClass: d ns:ns-string)
                                     (ccl::lisp-string-from-nsstring d)
                                     (#/description d)))
                               (list-from-ns-array plist)))
                      (canonical-dropped-paths 
                       (mapcar (lambda (s) 
                                 (if (and (probe-file s)
                                          (directoryp s))
                                     (ccl::ensure-directory-pathname s)
                                     s))
                               strings-for-dropped-objects))
                      (dropstr (if (= (length canonical-dropped-paths) 1)
                                   (with-output-to-string (out)
                                     (format out "~S~%" (first canonical-dropped-paths)))
                                   nil)))
                 ;; TODO: insert them in the window
                 (if dropstr
                     (let* ((hview (hemlock-view self))
                            (buf (hi:hemlock-view-buffer hview))
                            (point (hi::buffer-point buf))
                            (hi::*current-buffer* buf))
                       (hi::insert-string point dropstr)
                       #$YES)
                     #$NO))))
            ;; we found NSFilenamesPboardType, but didn't get an array of pathnames; huh???
            (t (log-debug "hemlock-textstorage-text-view received an unrecognized data type in a drag operation: '~S'"
                          (#/description plist))
               (call-next-method sender)))))))
|#

(defmethod hemlock-view ((self hemlock-textstorage-text-view))
  (slot-value self 'hemlock-view))


(defmethod activate-hemlock-view ((self hemlock-textstorage-text-view))
  (assume-cocoa-thread)
  (let* ((the-hemlock-frame (#/window self)))
    #+debug (log-debug "Activating ~s" self)
    (with-slots ((echo peer)) self
      (deactivate-hemlock-view echo))
    (#/setEditable: self t)
    (#/makeFirstResponder: the-hemlock-frame self)))

(defmethod deactivate-hemlock-view ((self hemlock-textstorage-text-view))
  (assume-cocoa-thread)
  #+debug (log-debug "deactivating ~s" self)
  (assume-not-editing self)
  (#/setSelectable: self nil)
  (disable-paren-highlight self))



      

(defmethod eventqueue-abort-pending-p ((self hemlock-textstorage-text-view))
  ;; Return true if cmd-. is in the queue.  Not sure what to do about c-g:
  ;; would have to distinguish c-g from c-q c-g or c-q c-q c-g etc.... Maybe
  ;; c-g will need to be synchronous meaning just end current command,
  ;; while cmd-. is the real abort.
  #|
   (let* ((now (#/dateWithTimeIntervalSinceNow: ns:ns-date 0.0d0)))
    (loop (let* ((event (#/nextEventMatchingMask:untilDate:inMode:dequeue:
			 target (logior #$whatever) now #&NSDefaultRunLoopMode t)))
	    (when (%null-ptr-p event) (return)))))
  "target" can either be an NSWindow or the global shared application object;
  |#
  nil)

(defvar *buffer-being-edited* nil)

#-darwin-target
(objc:defmethod (#/hasMarkedText #>BOOL) ((self hemlock-textstorage-text-view))
  nil)

(objc:defmethod (#/keyDown: :void) ((self hemlock-textstorage-text-view) event)
  #+debug (#_NSLog #@"Key down event in %@  = %@" :id self :address event)
  (let* ((view (hemlock-view self))
	 ;; quote-p means handle characters natively
	 (quote-p (and view (hi::hemlock-view-quote-next-p view))))
    #+debug (log-debug "~&quote-p ~s event ~s" quote-p event)
    (cond ((or (null view) (#/hasMarkedText self) (eq quote-p :native))
	   (when (and quote-p (not (eq quote-p :native)))	;; see ticket:461
	     (setf (hi::hemlock-view-quote-next-p view) nil))
	   (call-next-method event))
	  ((not (eventqueue-abort-pending-p self))
	   (let ((hemlock-key (nsevent-to-key-event event quote-p)))
	     (if (and hemlock-key
                      (not (hi:native-key-event-p hemlock-key)))
               (progn
                 (#/setHiddenUntilMouseMoves: ns:ns-cursor t)
                 (hi::handle-hemlock-event view hemlock-key))
	       (call-next-method event)))))))

(defmacro with-view-selection-info ((text-view buffer) &body body)
  (let* ((old (gensym)))
    `(let* ((,old (hi::buffer-selection-info ,buffer)))
      (unwind-protect
           (progn
             (setf (hi::buffer-selection-info ,buffer)
                   (view-selection-info ,text-view))
             ,@body)
        (setf (hi::buffer-selection-info ,buffer) ,old)))))
      
(defmacro with-string-under-cursor ((text-view selection-name &optional bufname) &body body)
  "Intelligently grab the string under the cursor in the given text-view.
   If something is selected, just grab that. Otherwise call hemlock::symbol-at-point at cursor position.
   selection-name is the name of a variable to which the selection will be assigned.
   bufname (if given) is the name of a variable to which the current buffer will be assigned."
  (let ((bufsym (or bufname (gensym)))
        (viewsym (gensym)))      
    `(let* ((,viewsym ,text-view)
            (,bufsym (hemlock-buffer ,viewsym)))
       (with-view-selection-info (,viewsym ,bufsym)
         (let* ((hi::*current-buffer* ,bufsym) ; needed for symbol-at-point to work
                (,selection-name (hemlock::symbol-at-point ,bufsym)))
           ,@body)))))

(defmethod hi::handle-hemlock-event :around ((view hi:hemlock-view) event)
  (declare (ignore event))
  (with-view-selection-info ((text-pane-text-view (hi::hemlock-view-pane view))
                             (hi::hemlock-view-buffer view))
    (with-autorelease-pool
        (call-next-method))))

(defconstant +shift-event-mask+ (hi:key-event-modifier-mask "Shift"))

;;; Translate a keyDown NSEvent to a Hemlock key-event.
(defun nsevent-to-key-event (event quote-p)
  (let* ((modifiers (#/modifierFlags event)))
    (unless (logtest #$NSCommandKeyMask modifiers)
      (let* ((native-chars (#/characters event))
	     (native-len (if (%null-ptr-p native-chars)
			   0
			   (#/length native-chars)))
	     (native-c (and (eql 1 native-len)
			    (#/characterAtIndex: native-chars 0)))
	     (option-p (logtest #$NSAlternateKeyMask modifiers)))
	;; If a standalone dead key (e.g. ^'` on a French keyboard,) was pressed,
	;; reverse the meaning of quote-p, i.e. use the system meaning if NOT quoted.
	;; (I have no idea what makes standalone dead keys somehow different from
	;; non-standalone dead keys).
	(when (and (not option-p) (eql 0 native-len))
	  (setq quote-p (not quote-p)))
	(let ((c (if (or quote-p
			 (and option-p
			      (or (not *option-is-meta*)
                                  #-cocotron
				  (and native-c
				       (ccl::valid-char-code-p native-c)
				       (standard-char-p (code-char (the ccl::valid-char-code native-c)))))
			      (setq quote-p t)))
		   native-c
		   (let ((chars (#/charactersIgnoringModifiers event)))
		     (and (not (%null-ptr-p chars))
			  (eql 1 (#/length chars))
			  (#/characterAtIndex: chars 0))))))
	  (when c
	    (let ((bits 0)
		  (useful-modifiers (logandc2 modifiers
					      (logior
					       ;;#$NSShiftKeyMask
					       #$NSAlphaShiftKeyMask))))
	      (unless quote-p
		(dolist (map hi:*modifier-translations*)
		  (when (logtest useful-modifiers (car map))
		    (setq bits (logior bits
				       (hi:key-event-modifier-mask (cdr map)))))))
	      (let* ((char (code-char c)))
		(when (and char (alpha-char-p char))
		  (setq bits (logandc2 bits +shift-event-mask+)))
		(when (logtest #$NSAlphaShiftKeyMask modifiers)
		  (setf c (char-code (char-upcase char)))))
	      (hi:make-key-event c bits))))))))

;; For now, this is only used to abort i-search.  All actual mouse handling is done
;; by Cocoa.   In the future might want to allow users to extend via hemlock, e.g.
;; to implement mouse-copy.
;; Also -- shouldn't this happen on mouse up?
(objc:defmethod (#/mouseDown: :void) ((self hemlock-textstorage-text-view) event)
  ;; If no modifier keys are pressed, send hemlock a no-op.
  ;; (Or almost a no-op - this does an update-hemlock-selection as a side-effect)
  (unless (logtest #$NSDeviceIndependentModifierFlagsMask (#/modifierFlags event))
    (let* ((view (hemlock-view self)))
      (when view
	(unless (eventqueue-abort-pending-p self)
	  (hi::handle-hemlock-event view #k"leftdown")))))
  (call-next-method event))

(defmethod assume-not-editing ((tv hemlock-textstorage-text-view))
  (assume-not-editing (#/textStorage tv)))

(objc:defmethod (#/changeColor: :void) ((self hemlock-textstorage-text-view)
                                        sender)
  (declare (ignorable sender))
  #+debug (#_NSLog #@"Change color to = %@" :id (#/color sender)))

(def-cocoa-default *layout-text-in-background* :bool t "When true, do text layout when idle.")

(objc:defmethod (#/layoutManager:didCompleteLayoutForTextContainer:atEnd: :void)
    ((self hemlock-textstorage-text-view) layout cont (flag :<BOOL>))
  (declare (ignorable cont flag))
  #+debug (#_NSLog #@"layout complete: container = %@, atend = %d" :id cont :int (if flag 1 0))
  (unless *layout-text-in-background*
    (#/setDelegate: layout +null-ptr+)
    #-cocotron
    (#/setBackgroundLayoutEnabled: layout nil)))

(defun ns-attribute (attribute)
  (ecase attribute
    (:foreground #&NSForegroundColorAttributeName)
    (:background #&NSBackgroundColorAttributeName)))

(defmethod remove-paren-highlight ((self hemlock-textstorage-text-view))
  (ns:with-ns-range (range)
    #-cocotron
    (let* ((layout (#/layoutManager self)))
      (setf (ns:ns-range-length range) 1)
      (loop
        for (pos attrib . nil) in (text-view-paren-highlighting self)
        do (setf (ns:ns-range-location range) pos)
        do  (#/removeTemporaryAttribute:forCharacterRange: layout (ns-attribute attrib) range)))
    ;;; This assumes that NSBackgroundColorAttributeName can only be 
    ;;; present if it's (possibly stale) paren highlighting info.
    ;;; We can't be sure of the locations (because of insertions/deletions),
    ;;; so remove the attribute from the entire textstorage.
    #+cocotron
    (let* ((textstorage (#/textStorage self))
           (len (#/length textstorage)))
      (#/beginEditing textstorage)
      (setf (ns:ns-range-location range) 0)
      (setf (ns:ns-range-length range) len)
      (#/removeAttribute:range: textstorage #&NSBackgroundColorAttributeName range)
      (#/endEditing textstorage))))

(defmethod disable-paren-highlight ((self hemlock-textstorage-text-view))
  (when (eql (text-view-paren-highlight-enabled self) #$YES)
    (setf (text-view-paren-highlight-enabled self) #$NO)
    (remove-paren-highlight self)))

(defun hemlock-ext:lookup-color (color-spec)
  (etypecase color-spec
    (cons (apply #'color-values-to-nscolor color-spec))
    ((vector t) (apply #'color-values-to-nscolor (coerce color-spec 'list)))
    ((or string symbol)
     (let ((name (string color-spec)))
       ;; Please rewrite me...
       (cond ((string-equal name "black") (#/blackColor ns:ns-color))
             ((string-equal name "blue") (#/blueColor ns:ns-color))
             ((string-equal name "brown") (#/brownColor ns:ns-color))
             ((string-equal name "cyan") (#/cyanColor ns:ns-color))
             ((string-equal name "gray") (#/grayColor ns:ns-color))
             ((string-equal name "lightgray") (#/lightGrayColor ns:ns-color))
             ((string-equal name "darkgray") (#/darkGrayColor ns:ns-color))        
             ((string-equal name "green") (#/greenColor ns:ns-color))
             ((string-equal name "magenta") (#/magentaColor ns:ns-color))
             ((string-equal name "orange") (#/orangeColor ns:ns-color))
             ((string-equal name "purple") (#/purpleColor ns:ns-color))
             ((string-equal name "red") (#/redColor ns:ns-color))
             ((string-equal name "white") (#/whiteColor ns:ns-color))
             ((string-equal name "yellow") (#/yellowColor ns:ns-color))
             (t (error "I don't know color ~s" name)))))))

(defmethod compute-temporary-attributes ((self hemlock-textstorage-text-view))
  #-cocotron
  (let* ((container (#/textContainer self))
         ;; If there's a containing scroll view, use its contentview         
         ;; Otherwise, just use the current view.
         (scrollview (#/enclosingScrollView self))
         (contentview (if (%null-ptr-p scrollview) self (#/contentView scrollview)))
         (rect (#/bounds contentview))
         (layout (#/layoutManager container))
         (glyph-range (#/glyphRangeForBoundingRect:inTextContainer:
                       layout rect container))
         (char-range (#/characterRangeForGlyphRange:actualGlyphRange:
                      layout glyph-range +null-ptr+))
         (start (ns:ns-range-location char-range))
         (length (ns:ns-range-length char-range))
         (end (+ start length)))
    (hemlock:with-display-context (hemlock-view self)
      (ns:with-ns-range (range)
        (when (> length 0)
          ;; Remove all temporary attributes from the character range
          (#/removeTemporaryAttribute:forCharacterRange: layout #&NSForegroundColorAttributeName char-range)
          (#/removeTemporaryAttribute:forCharacterRange: layout #&NSBackgroundColorAttributeName char-range)
          (loop
            for (start len attrib . color) in (hemlock:compute-syntax-coloring start length)
            when color
            do (progn
                 (setf (ns:ns-range-location range) start
                       (ns:ns-range-length range) len)
		 (#/addTemporaryAttribute:value:forCharacterRange: layout (ns-attribute attrib) color range))))
        (when (eql #$YES (text-view-paren-highlight-enabled self))
          (setf (ns:ns-range-length range) 1)
          (loop
             for (pos attrib . color) in (text-view-paren-highlighting self)
             do (when (and (<= start pos) (< pos end))
                  (setf (ns:ns-range-location range) pos)
		  (#/addTemporaryAttribute:value:forCharacterRange: layout (ns-attribute attrib) color range)))))))
  #+cocotron
  (when (eql #$YES (text-view-paren-highlight-enabled self))
    (let* ((ts (#/textStorage self)))
      (ns:with-ns-range (range)
        (#/beginEditing ts)
        (setf (ns:ns-range-length range) 1)
        (loop
          for (pos attrib . color) in (text-view-paren-highlighting self)
          do (setf (ns:ns-range-location range) pos)
          do (#/addAttribute:value:range: ts (ns-attribute attrib) color range))
        (#/endEditing ts)))))

(defmethod update-paren-highlight ((self hemlock-textstorage-text-view))
  (disable-paren-highlight self)
  (let* ((view (hemlock-view self))
         (buffer (and view (hi:hemlock-view-buffer view))))
    (when (and buffer (string= (hi:buffer-major-mode buffer) "Lisp"))
      (hemlock:with-display-context view
        #+debug (#_NSLog #@"Syntax check for paren-highlighting")
        (update-buffer-package (hi::buffer-document buffer))
        (setf (text-view-paren-highlighting self) (hemlock:compute-paren-highlighting))
        (setf (text-view-paren-highlight-enabled self) #$YES))
      (compute-temporary-attributes self))))



;;; Set and display the selection at pos, whose length is len and whose
;;; affinity is affinity.  This should never be called from any Cocoa
;;; event handler; it should not call anything that'll try to set the
;;; underlying buffer's point and/or mark

(objc:defmethod (#/updateSelection:length:affinity: :void)
    ((self hemlock-textstorage-text-view)
     (pos :int)
     (length :int)
     (affinity :<NSS>election<A>ffinity))
  (assume-cocoa-thread)
  (when (eql length 0)
    (update-paren-highlight self))
  (let* ((buffer (hemlock-buffer self)))
    (with-view-selection-info (self buffer)
      (setf (hi::buffer-selection-set-by-command buffer) (> length 0))
      (rlet ((range :ns-range :location pos :length length))
        (ccl::%call-next-objc-method self
                                     hemlock-textstorage-text-view
                                     (@selector #/setSelectedRange:affinity:stillSelecting:)
                                     '(:void :<NSR>ange :<NSS>election<A>ffinity :<BOOL>)
                                     range
                                     affinity
                                     nil)
        (assume-not-editing self)
        (when (> length 0)
          (let* ((ts (#/textStorage self)))
            (with-slots (selection-set-by-search) ts
              (when (prog1 (eql #$YES selection-set-by-search)
                      (setq selection-set-by-search #$NO))
                (highlight-search-selection self pos length)))))
    ))))

(defloadvar *can-use-show-find-indicator-for-range*
    (#/instancesRespondToSelector: ns:ns-text-view (@selector "showFindIndicatorForRange:")))

;;; Add transient highlighting to a selection established via a search
;;; primitive, if the OS supports it.
(defun highlight-search-selection (tv pos length)
  (when *can-use-show-find-indicator-for-range*
    (ns:with-ns-range (r pos length)
      (objc-message-send tv "showFindIndicatorForRange:" :<NSR>ange r :void))))
  
;;; A specialized NSTextView. The NSTextView is part of the "pane"
;;; object that displays buffers.
(defclass hemlock-text-view (hemlock-textstorage-text-view)
    ((pane :foreign-type :id :accessor text-view-pane)
     (char-width :foreign-type :<CGF>loat :accessor text-view-char-width)
     (line-height :foreign-type :<CGF>loat :accessor text-view-line-height))
  (:metaclass ns:+ns-object))
(declaim (special hemlock-text-view))

(objc:defmethod (#/duplicate: :void) ((self hemlock-text-view) sender)
  (#/duplicate: (#/window self) sender))

(defmethod hemlock-view ((self hemlock-text-view))
  (slot-value self 'hemlock-view))

(objc:defmethod (#/evalSelection: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (let* ((view (hemlock-view self)))
    (when view
      (hi::handle-hemlock-event view #'(lambda ()
                                         (hemlock::editor-execute-expression-command nil))))))

(defun ui-buffer-env (obj)
  (let* ((buffer (hemlock-buffer obj))
         (package-name (hi::variable-value 'hemlock::default-package :buffer buffer))
         (pathname (hi::buffer-pathname buffer)))
    (list package-name pathname)))

(objc:defmethod (#/evalAll: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (let* ((s (lisp-string-from-nsstring (#/string self))))
    (ui-object-eval-selection *NSApp* `(,@(ui-buffer-env self) ,s))))

(objc:defmethod (#/loadBuffer: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (ui-object-load-buffer *NSApp* (ui-buffer-env self)))

(objc:defmethod (#/compileBuffer: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (ui-object-compile-buffer *NSApp* (ui-buffer-env self)))

(objc:defmethod (#/compileAndLoadBuffer: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (ui-object-compile-and-load-buffer *NSApp* (ui-buffer-env self)))

(defloadvar *text-view-context-menu* ())

(defun text-view-context-menu ()
  (or *text-view-context-menu*
      (setq *text-view-context-menu*
            (#/retain
             (let* ((menu (make-instance 'ns:ns-menu :with-title #@"Menu")))
               (#/addItemWithTitle:action:keyEquivalent:
                menu #@"Cut" (@selector #/cut:) #@"")
               (#/addItemWithTitle:action:keyEquivalent:
                menu #@"Copy" (@selector #/copy:) #@"")
               (#/addItemWithTitle:action:keyEquivalent:
                menu #@"Paste" (@selector #/paste:) #@"")
               ;; Separator
               (#/addItem: menu (#/separatorItem ns:ns-menu-item))
               (#/addItemWithTitle:action:keyEquivalent:
                menu #@"Background Color ..." (@selector #/changeBackgroundColor:) #@"")
               (#/addItemWithTitle:action:keyEquivalent:
                menu #@"Text Color ..." (@selector #/changeTextColor:) #@"")
               ;; Separator
               (#/addItem: menu (#/separatorItem ns:ns-menu-item))
               (#/addItemWithTitle:action:keyEquivalent:
                menu #@"Duplicate this window" (@selector #/duplicate:) #@"")
               menu)))))





(objc:defmethod (#/changeBackgroundColor: :void)
    ((self hemlock-text-view) sender)
  (let* ((colorpanel (#/sharedColorPanel ns:ns-color-panel))
         (color (#/backgroundColor self)))
    (#/close colorpanel)
    (#/setAction: colorpanel (@selector #/updateBackgroundColor:))
    (#/setColor: colorpanel color)
    (#/setTarget: colorpanel self)
    (#/setContinuous: colorpanel nil)
    (#/orderFrontColorPanel: *NSApp* sender)))



(objc:defmethod (#/updateBackgroundColor: :void)
    ((self hemlock-text-view) sender)
  (when (#/isVisible sender)
    (let* ((color (#/color sender)))
      (unless (typep self 'echo-area-view)
        (let* ((window (#/window self))
               (echo-view (unless (%null-ptr-p window)
                            (slot-value window 'echo-area-view))))
          (when echo-view (#/setBackgroundColor: echo-view color))))
      #+debug (#_NSLog #@"Updating backgroundColor to %@, sender = %@" :id color :id sender)
      (#/setBackgroundColor: self color))))

(objc:defmethod (#/changeTextColor: :void)
    ((self hemlock-text-view) sender)
  (let* ((colorpanel (#/sharedColorPanel ns:ns-color-panel))
         (textstorage (#/textStorage self))
         (color (#/objectForKey:
                 (#/objectAtIndex: (slot-value textstorage 'styles) 0)
                 #&NSForegroundColorAttributeName)))
    (#/close colorpanel)
    (#/setAction: colorpanel (@selector #/updateTextColor:))
    (#/setColor: colorpanel color)
    (#/setTarget: colorpanel self)
    (#/setContinuous: colorpanel nil)
    (#/orderFrontColorPanel: *NSApp* sender)))






   
(objc:defmethod (#/updateTextColor: :void)
    ((self hemlock-textstorage-text-view) sender)
  (unwind-protect
      (progn
	(#/setUsesFontPanel: self t)
	(ccl::%call-next-objc-method
	 self
	 hemlock-textstorage-text-view
         (@selector #/changeColor:)
         '(:void :id)
         sender))
    (#/setUsesFontPanel: self nil))
  (#/setNeedsDisplay: self t))
   
(objc:defmethod (#/updateTextColor: :void)
    ((self hemlock-text-view) sender)
  (let* ((textstorage (#/textStorage self))
         (styles (slot-value textstorage 'styles))
         (newcolor (#/color sender)))
    (dotimes (i (#/count styles))
      (let* ((dict (#/objectAtIndex: styles i)))
        (#/setValue:forKey: dict newcolor #&NSForegroundColorAttributeName)))
    (call-next-method sender)))



(defmethod text-view-string-cache ((self hemlock-textstorage-text-view))
  (hemlock-buffer-string-cache (#/hemlockString (#/textStorage self))))


(objc:defmethod (#/selectionRangeForProposedRange:granularity: :ns-range)
    ((self hemlock-textstorage-text-view)
     (proposed :ns-range)
     (g :<NSS>election<G>ranularity))
  #+debug
  (#_NSLog #@"Granularity = %d" :int g)
  (objc:returning-foreign-struct (r)
    (block HANDLED
      (let* ((index (ns:ns-range-location proposed))
             (length (ns:ns-range-length proposed))
             (textstorage (#/textStorage self))
             (event (#/currentEvent (#/window self)))
             (event-type (#/type event)))
        ;; Workaround for bug #150
        (when (and (eql g #$NSSelectByCharacter)
                   (eql index (#/length textstorage))
                   (or (eql event-type #$NSLeftMouseDown) (eql event-type #$NSLeftMouseUp)))
          (setq g (case (#/clickCount event)
                    ((0 1) #$NSSelectByCharacter)
                    (2 #$NSSelectByWord)
                    (t #$NSSelectByParagraph))))
        (unless (eql g #$NSSelectByCharacter)
          (let* ((cache (hemlock-buffer-string-cache (#/hemlockString textstorage)))
                 (buffer (buffer-cache-buffer cache)))
            (with-view-selection-info (self buffer)
              (let* ((hi::*current-buffer* buffer)
                     (point (hi:buffer-point buffer))
                     (atom-mode (eql g #$NSSelectByParagraph)))
                (hi:with-mark ((mark point))
                  (when (or (= length 0) (hi:move-to-absolute-position mark index))
                    (let* ((region (hemlock:selection-for-click mark atom-mode))
                           (other-region (and (< 0 length)
                                              (hi:character-offset mark length)
                                              (hemlock:selection-for-click mark atom-mode))))
                      (when (null region) (setq region other-region other-region nil))
                      (when region
                        (let ((start-pos (min (hi:mark-absolute-position (hi:region-start region))
                                              (if other-region
                                                (hi:mark-absolute-position (hi:region-start other-region))
                                                index)))
                              (end-pos (max (hi:mark-absolute-position (hi:region-end region))
                                            (if other-region
                                              (hi:mark-absolute-position (hi:region-end other-region))
                                              (+ index length)))))
                          (assert (<= start-pos end-pos))
                          ;; Act as if we started the selection at the other end, so the heuristic
                          ;; in #/setSelectedRange does the right thing.  ref bug #565.
                          ;; However, only do so at the end, so don't keep toggling during selection, ref bug #851.
                          (when (and (eql event-type #$NSLeftMouseUp) (< start-pos end-pos))
                            (let ((point-pos (hi:mark-absolute-position point)))
                              (cond ((eql point-pos start-pos)
                                     (hi:move-to-absolute-position point end-pos))
                                    ((eql point-pos end-pos)
                                     (hi:move-to-absolute-position point start-pos)))))
                          (ns:init-ns-range r start-pos (- end-pos start-pos))
                          #+debug
                          (#_NSLog #@"range = %@, proposed = %@, granularity = %d"
                                   :address (#_NSStringFromRange r)
                                   :address (#_NSStringFromRange proposed)
                                   :<NSS>election<G>ranularity g)
                          (return-from HANDLED r))))))))))
        (prog1
            (call-next-method proposed g)
          #+debug
          (#_NSLog #@"range = %@, proposed = %@, granularity = %d"
                   :address (#_NSStringFromRange r)
                   :address (#_NSStringFromRange proposed)
                   :<NSS>election<G>ranularity g))))))

(defun append-output (view string)
  (assume-cocoa-thread)
  ;; Arrange to do the append in command context
  (when view
    (hi::handle-hemlock-event view #'(lambda ()
				       (hemlock::append-buffer-output (hi::hemlock-view-buffer view) string)))))


;;; Update the underlying buffer's point (and "active region", if appropriate.
;;; This is called in response to a mouse click or other event; it shouldn't
;;; be called from the Hemlock side of things.

(objc:defmethod (#/setSelectedRange:affinity:stillSelecting: :void)
    ((self hemlock-text-view)
     (r :<NSR>ange)
     (affinity :<NSS>election<A>ffinity)
     (still-selecting :<BOOL>))
  #+debug
  (#_NSLog #@"Set selected range called: range = %@, affinity = %d, still-selecting = %d"
           :address (#_NSStringFromRange r)
           :<NSS>election<A>ffinity affinity
           :<BOOL> (if still-selecting #$YES #$NO))
  #+debug
  (#_NSLog #@"text view string = %@, textstorage string = %@"
           :id (#/string self)
           :id (#/string (#/textStorage self)))
  (unless (#/editingInProgress (#/textStorage self))
    (let* ((d (hemlock-buffer-string-cache (#/hemlockString (#/textStorage self))))
           (buffer (buffer-cache-buffer d))
	   (hi::*current-buffer* buffer)
           (location (pref r :<NSR>ange.location))
           (len (pref r :<NSR>ange.length)))
      (setf (hi::buffer-selection-set-by-command buffer) nil)
      (with-view-selection-info (self buffer)
        (cond ((eql len 0)
               #+debug
               (#_NSLog #@"Moving point to absolute position %d" :int location)
               ;; Do this even if still-selecting, in order to enable the heuristic below.
               (hemlock:move-point-for-click buffer location)
               (update-paren-highlight self))
              (t
               ;; We don't get much information about which end of the
               ;; selection the mark's at and which end point is at, so
               ;; we have to sort of guess.  In every case I've ever seen,
               ;; selection via the mouse generates a sequence of calls to
               ;; this method whose parameters look like:
               ;; a: range: {n0,0} still-selecting: false  [ rarely repeats ] (this doesn't actually happen)
               ;; b: range: {n0,0) still-selecting: true   [ rarely repeats ]
               ;; c: range: {n1,m} still-selecting: true   [ often repeats ]
               ;; d: range: {n1,m} still-selecting: false  [ rarely repeats ] (mouse up)
               ;;
               ;; (Sadly, "affinity" doesn't tell us anything interesting.)
               ;; We've handled a and b in the clause above; after handling
               ;; b, point references buffer position n0 and the
               ;; region is inactive.
               ;; Let's ignore c, and wait until the selection's stabilized.
               ;; Make a new mark, a copy of point (position n0).
               ;; At step d (here), we should have either
               ;; d1) n1=n0.  Mark stays at n0, point moves to n0+m.
               ;; d2) n1+m=n0.  Mark stays at n0, point moves to n0-m.
               ;; If neither d1 nor d2 apply, arbitrarily assume forward
               ;; selection: mark at n1, point at n1+m.
               ;; In all cases, activate Hemlock selection.
               (unless still-selecting
                 (let* ((point (hi::buffer-point buffer))
                        (pointpos (hi:mark-absolute-position point))
                        (selection-end (+ location len))
                        (mark (hi::copy-mark point :right-inserting)))
                   (cond ((eql pointpos location)
                          (move-hemlock-mark-to-absolute-position point
                                                                  d
                                                                  selection-end))
                         ((eql pointpos selection-end)
                          (move-hemlock-mark-to-absolute-position point
                                                                  d
                                                                  location))
                         (t
                          (move-hemlock-mark-to-absolute-position mark
                                                                  d
                                                                  location)
                          (move-hemlock-mark-to-absolute-position point
                                                                  d
                                                                  selection-end)))
                   (hemlock::%buffer-push-buffer-mark buffer mark t))))))))
  (call-next-method r affinity still-selecting))



;;; Modeline-view

(defclass modeline-view (ns:ns-view)
    ((pane :foreign-type :id :accessor modeline-view-pane)
     (text-attributes :foreign-type :id :accessor modeline-text-attributes))
  (:metaclass ns:+ns-object))

(objc:defmethod #/initWithFrame: ((self modeline-view) (frame :<NSR>ect))
  (call-next-method frame)
  (let* ((size (#/smallSystemFontSize ns:ns-font))
	 (font (#/systemFontOfSize: ns:ns-font size))
	 (dict (#/dictionaryWithObject:forKey: ns:ns-dictionary font #&NSFontAttributeName)))
    (setf (modeline-text-attributes self) (#/retain dict)))
  self)

;;; Find the underlying buffer.
(defun buffer-for-modeline-view (mv)
  (let* ((pane (modeline-view-pane mv)))
    (unless (%null-ptr-p pane)
      (let* ((tv (text-pane-text-view pane)))
        (unless (%null-ptr-p tv)
	  (hemlock-buffer tv))))))

;;; Draw a string in the modeline view.  The font and other attributes
;;; are initialized lazily; apparently, calling the Font Manager too
;;; early in the loading sequence confuses some Carbon libraries that're
;;; used in the event dispatch mechanism,
(defun draw-modeline-string (the-modeline-view)
  (with-slots (text-attributes) the-modeline-view
    (let* ((buffer (buffer-for-modeline-view the-modeline-view)))
      (when buffer
	(let* ((string
                (apply #'concatenate 'string
                       (mapcar
                        #'(lambda (field)
                            (or (ignore-errors 
                                  (funcall (hi::modeline-field-function field) buffer))
                                ""))
                        (hi::buffer-modeline-fields buffer)))))
	  (#/drawAtPoint:withAttributes: (#/autorelease (%make-nsstring string))
                                         (ns:make-ns-point 5 1)
                                         text-attributes))))))

(objc:defmethod (#/drawRect: :void) ((self modeline-view) (rect :<NSR>ect))
  (declare (ignorable rect))
  (let* ((bounds (#/bounds self))
	 (context (#/currentContext ns:ns-graphics-context)))
    (#/saveGraphicsState context)
    (#/set (#/colorWithCalibratedWhite:alpha: ns:ns-color 0.9 1.0))
    (#_NSRectFill bounds)
    (#/set (#/colorWithCalibratedWhite:alpha: ns:ns-color 0.3333 1.0))
    ;; Draw borders on top and bottom.
    (ns:with-ns-rect (r 0 0.5 (ns:ns-rect-width bounds) 0.5)
      (#_NSRectFill r))
    (ns:with-ns-rect (r 0 (- (ns:ns-rect-height bounds) 0.5)
			(ns:ns-rect-width bounds) (- (ns:ns-rect-height bounds) 0.5))
      (#_NSRectFill r))
    (draw-modeline-string self)
    (#/restoreGraphicsState context)))

;;; Hook things up so that the modeline is updated whenever certain buffer
;;; attributes change.
(hi::%init-mode-redisplay)


;;; A clip view subclass, which exists mostly so that we can track origin changes.
(defclass text-pane-clip-view (ns:ns-clip-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/scrollToPoint: :void) ((self text-pane-clip-view)
                                           (origin #>NSPoint))
  (unless (#/inLiveResize self)
    (call-next-method origin)
    (compute-temporary-attributes (#/documentView self))))

;;; Text-pane

;;; The text pane is just an NSBox that (a) provides a draggable border
;;; around (b) encapsulates the text view and the mode line.

(defclass text-pane (ns:ns-box)
    ((hemlock-view :initform nil :reader text-pane-hemlock-view)
     (text-view :foreign-type :id :accessor text-pane-text-view)
     (mode-line :foreign-type :id :accessor text-pane-mode-line)
     (scroll-view :foreign-type :id :accessor text-pane-scroll-view))
  (:metaclass ns:+ns-object))

(defmethod hemlock-view ((self text-pane))
  (text-pane-hemlock-view self))

;;; This method gets invoked on the text pane, which is its containing
;;; window's delegate object.
(objc:defmethod (#/windowDidResignKey: :void)
    ((self text-pane) notification)
  (declare (ignorable notification))
  ;; When the window loses focus, we should remove or change transient
  ;; highlighting (like matching-paren highlighting).  Maybe make this
  ;; more general ...
  ;; Currently, this only removes temporary attributes from matching
  ;; parens; other kinds of syntax highlighting stays visible when
  ;; the containing window loses keyboard focus
  (let* ((tv (text-pane-text-view self)))
    (remove-paren-highlight tv)
    (remove-paren-highlight (slot-value tv 'peer))))

;;; Likewise, reactivate transient highlighting when the window gets
;;; focus.
(objc:defmethod (#/windowDidBecomeKey: :void)
    ((self text-pane) notification)
  (declare (ignorable notification))
  (let* ((tv (text-pane-text-view self)))
    (compute-temporary-attributes tv)
    (compute-temporary-attributes (slot-value tv 'peer))))
  

;;; Mark the buffer's modeline as needing display.  This is called whenever
;;; "interesting" attributes of a buffer are changed.
(defun hemlock-ext:invalidate-modeline (buffer)
  (let* ((doc (hi::buffer-document buffer)))
    (when doc
      (document-invalidate-modeline doc))))

;;; Process a file's "coding" file-option.
(defun hemlock-ext:set-buffer-external-format (buffer string)
  (let* ((ef (ccl::process-file-coding-option string (or (hi::buffer-line-termination buffer) :unix)))
         (encoding-val (nsstring-encoding-for-external-format ef)))
    (cond (encoding-val
           (setf (hi::buffer-line-termination buffer)
                 (external-format-line-termination ef))
           (let* ((doc (hi::buffer-document buffer)))
             (when doc
               (with-slots (encoding) doc
                 (setq encoding encoding-val))))
           (hemlock-ext:invalidate-modeline buffer))
          (t
           (hi:loud-message "Can't parse coding option ~a." string)))))
                 

(def-cocoa-default *text-pane-margin-width* :float 0.0f0 "width of indented margin around text pane")
(def-cocoa-default *text-pane-margin-height* :float 0.0f0 "height of indented margin around text pane")


(objc:defmethod #/initWithFrame: ((self text-pane) (frame :<NSR>ect))
  (let* ((pane (call-next-method frame)))
    (unless (%null-ptr-p pane)
      (#/setAutoresizingMask: pane (logior
                                    #$NSViewWidthSizable
                                    #$NSViewHeightSizable))
      (#/setBoxType: pane #$NSBoxPrimary)
      (#/setBorderType: pane #$NSNoBorder)
      (#/setContentViewMargins: pane (ns:make-ns-size *text-pane-margin-width*  *text-pane-margin-height*))
      (#/setTitlePosition: pane #$NSNoTitle))
    pane))

(objc:defmethod #/defaultMenu ((class +hemlock-text-view))
  (text-view-context-menu))

(defun pathname-for-namestring-fragment (string)
  "Return a pathname that STRING might designate."
  ;; We could get fancy here, but for now just be stupid.
  (let* ((rfs (ignore-errors (read-from-string string nil nil)))
         (pathname (or (ignore-errors (probe-file string))
                       (ignore-errors (probe-file rfs))
                       (ignore-errors (probe-file (merge-pathnames *.lisp-pathname* string)))
                       (ignore-errors (probe-file (merge-pathnames *.lisp-pathname* rfs))))))
    (if (and (pathnamep pathname)
             (not (directory-pathname-p pathname)))
      pathname)))

#| broken in the case of a string that's already preceded by package name
(defun find-symbol-in-packages (string pkgs)
  (setq string (string-upcase string))
  (let (sym)
    (dolist (p pkgs)
      (when (setq sym (find-symbol string p))
        (return)))
    sym))
|#

(defun find-symbol-in-packages (string pkgs)
  "Look up symbol named by string in given list of packages. If no list, just try to read the symbol itself,
   using current binding of *package* unless string has its own package designator."
  (cond (pkgs
         (let (sym)
           (dolist (p pkgs)
             (let ((*package* (find-package p)))
               (when (setq sym (ignore-errors (read-from-string string)))
                 (return))))
           sym))
        (t (ignore-errors (read-from-string string)))))

(defun find-symbol-in-buffer-packages (string buffer)
  (let ((package-name (hi::variable-value 'hemlock::current-package :buffer buffer))
        (packages nil))
    (unless (find #\: string) ; don't bother looking in other packages if the string itself contains a package designator
      (setf packages (append ; all packages in order, starting with the ones of this buffer
                      #1=(cons package-name (package-use-list package-name))
                      (set-difference (list-all-packages) #1#))))
    (find-symbol-in-packages string packages)))

(defun choose-listener ()
  (ui-object-choose-listener-for-selection *NSApp* nil))

(defun eval-in-listener (string)
  "Evals string in nearest listener, or creates one if none. Any errors reported during evaluation
   go to the listener, not the console."
  (let* ((target-listener (choose-listener)))
    (when target-listener
      (enqueue-listener-input (cocoa-listener-process-input-stream target-listener) string))))

(objc:defmethod (#/openSelection: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (with-string-under-cursor (self selection)
    (let* ((pathname (pathname-for-namestring-fragment selection)))
      (when pathname
        (eval-in-listener (format nil "(ed ~S)" pathname))))))

(objc:defmethod (#/traceSelection: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (with-string-under-cursor (self symbol-name buffer)
    (let* ((sym (find-symbol-in-buffer-packages symbol-name buffer)))
      (eval-in-listener (format nil "(trace ~S)" sym)))))

(objc:defmethod (#/inspectSelection: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (with-string-under-cursor (self symbol-name buffer)
    (let* ((sym (find-symbol-in-buffer-packages symbol-name buffer)))
      (inspect sym))))

(objc:defmethod (#/sourceForSelection: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (with-string-under-cursor (self symbol-name buffer)
    (let* ((sym (find-symbol-in-buffer-packages symbol-name buffer)))
      ;(execute-in-gui (lambda () (ed sym))) ; NO! If this errors, it throws to the console. Same with execute-in-buffer.
      (eval-in-listener (format nil "(ed '~S)" sym)))))

(hi:defcommand "Inspect Symbol" (p)
  "Inspects current symbol."
  (declare (ignore p))
  (let* ((buffer (hi:current-buffer))
         (fun-name (hemlock::symbol-at-point buffer)))
    (if fun-name
      (inspect (find-symbol-in-buffer-packages fun-name buffer))
      (hi:beep))))

;;; If we don't override this, NSTextView will start adding Google/
;;; Spotlight search options and dictionary lookup when a selection
;;; is active.
(objc:defmethod #/menuForEvent: ((self hemlock-text-view) event)
  (declare (ignore event))
  (with-string-under-cursor (self selection)
    (let* ((menu (if (> (length selection) 0)
                   (#/copy (#/menu self))
                   (#/retain (#/menu self))))
           (thingfound? (> (length selection) 0)))
      (flet ((make-contextual-menu-item (title selector &optional (key-equiv #@""))
               (let* ((nstitle (%make-nsstring title))
                      (item (make-instance 'ns:ns-menu-item :with-title nstitle
                              :action (ccl::%get-selector (ccl::load-objc-selector selector))
                              :key-equivalent key-equiv)))
                 (#/setTarget: item self)
                 (#/insertItem:atIndex: menu item 0)
                 (#/release item))))
        (when thingfound? (make-contextual-menu-item (concatenate 'string "Hyperspec " selection) '#/hyperSpecLookUp:))
        (when thingfound? (make-contextual-menu-item (concatenate 'string "Trace " selection) '#/traceSelection:))
        (when thingfound? (make-contextual-menu-item (concatenate 'string "Inspect " selection) '#/inspectSelection:))
        (when thingfound? (make-contextual-menu-item (concatenate 'string "Source of " selection) '#/sourceForSelection:))
        (when (and thingfound? (pathname-for-namestring-fragment selection))
          (make-contextual-menu-item (concatenate 'string "Open " selection) '#/openSelection:))
        (#/autorelease menu)))))

(defun init-selection-info-for-textview (tv buffer)
  (let* ((buffer-info (hi::buffer-selection-info buffer))
         (view-info (hi::make-selection-info :point (hi::copy-mark (hi::selection-info-point buffer-info))
                                             :%mark (let* ((mark (hi::selection-info-%mark buffer-info)))
                                                      (if mark (hi::copy-mark mark)))
                                             :view tv)))
    (setf (view-selection-info tv) view-info)))
                                                      
(defun make-scrolling-text-view-for-textstorage (textstorage x y width height tracks-width color style)
  (let* ((scrollview (#/autorelease
                      (make-instance
                       'ns:ns-scroll-view
                       :with-frame (ns:make-ns-rect x y width height)))))
    (#/setBorderType: scrollview #$NSNoBorder)
    (#/setHasVerticalScroller: scrollview t)
    (#/setHasHorizontalScroller: scrollview t)
    (#/setRulersVisible: scrollview nil)
    (#/setAutoresizingMask: scrollview (logior
                                        #$NSViewWidthSizable
                                        #$NSViewHeightSizable))
    (#/setAutoresizesSubviews: (#/contentView scrollview) t)
    (let* ((layout (make-instance 'ns:ns-layout-manager)))
      #+suffer
      (#/setTypesetter: layout (make-instance 'hemlock-ats-typesetter))
      (#/addLayoutManager: textstorage layout)
      (#/setUsesScreenFonts: layout *use-screen-fonts*)
      (#/release layout)
      (let* ((contentsize (#/contentSize scrollview)))
        (ns:with-ns-size (containersize large-number-for-text large-number-for-text)
          (ns:with-ns-rect (tv-frame 0 0 (ns:ns-size-width contentsize) (ns:ns-size-height contentsize))
            (ns:init-ns-size containersize large-number-for-text large-number-for-text)
            (ns:init-ns-rect tv-frame 0 0 (ns:ns-size-width contentsize) (ns:ns-size-height contentsize))
            (let* ((container (#/autorelease (make-instance
                                              'ns:ns-text-container
                                              :with-container-size containersize))))
              (#/addTextContainer: layout  container)
              (let* ((tv (#/autorelease (make-instance 'hemlock-text-view
                                                       :with-frame tv-frame
                                                       :text-container container))))
                (init-selection-info-for-textview tv (hemlock-buffer textstorage))
                (#/setDelegate: layout tv)
                (#/setMinSize: tv (ns:make-ns-size 0 (ns:ns-size-height contentsize)))
                (#/setMaxSize: tv (ns:make-ns-size large-number-for-text large-number-for-text))
                (#/setRichText: tv nil)
                (#/setAutoresizingMask: tv #$NSViewWidthSizable)
                (#/setBackgroundColor: tv color)
		(when (slot-exists-p textstorage 'styles)
		  (#/setTypingAttributes: tv (#/objectAtIndex:
					      (#/styles textstorage) style)))
                #-cocotron
                (#/setSmartInsertDeleteEnabled: tv nil)
                (#/setAllowsUndo: tv nil) ; don't want NSTextView undo
                #-cocotron
                (#/setUsesFindPanel: tv t)
                #-cocotron
                (#/setUsesFontPanel: tv nil)
                (#/setMenu: tv (text-view-context-menu))

		;;  The container tracking and the text view sizability along a
		;;  particular axis must always be different, or else things can
		;;  get really confused (possibly causing an infinite loop).

		(if (or tracks-width *wrap-lines-to-window*)
		  (progn
		    (#/setWidthTracksTextView: container t)
		    (#/setHeightTracksTextView: container nil)
		    (#/setHorizontallyResizable: tv nil)
		    (#/setVerticallyResizable: tv t))
		  (progn
		    (#/setWidthTracksTextView: container nil)
		    (#/setHeightTracksTextView: container nil)
		    (#/setHorizontallyResizable: tv t)
		    (#/setVerticallyResizable: tv t)))
                (#/setContentView: scrollview (make-instance 'text-pane-clip-view))
                (#/setDocumentView: scrollview tv)	      
                (values tv scrollview)))))))))

(defun make-scrolling-textview-for-pane (pane textstorage track-width color style)
  (let* ((contentrect (#/frame (#/contentView pane)) ))
    (multiple-value-bind (tv scrollview)
	(make-scrolling-text-view-for-textstorage
	 textstorage
         (ns:ns-rect-x contentrect)
         (ns:ns-rect-y contentrect)
         (ns:ns-rect-width contentrect)
         (ns:ns-rect-height contentrect)
	 track-width
         color
         style)
      (#/addSubview: pane scrollview)
      (let* ((r (#/frame scrollview)))
        (decf (ns:ns-rect-height r) 15)
        (incf (ns:ns-rect-y r) 15)
        (#/setFrame: scrollview r))
      #-cocotron
      (#/setAutohidesScrollers: scrollview t)
      (setf (slot-value pane 'scroll-view) scrollview
            (slot-value pane 'text-view) tv
            (slot-value tv 'pane) pane
            #|(slot-value scrollview 'pane) pane|#)
      ;;(let* ((modeline  (scroll-view-modeline scrollview)))
      (let* ((modeline  (make-instance 'modeline-view
                          :with-frame (ns:make-ns-rect 0 0 (ns:ns-rect-width contentrect)
                                                       15))))
        (#/setAutoresizingMask: modeline #$NSViewWidthSizable)
        (#/addSubview: pane modeline)
        (#/release modeline)
        (setf (slot-value pane 'mode-line) modeline
              (slot-value modeline 'pane) pane))
      tv)))

(defmethod hemlock-view-size ((view hi:hemlock-view))
  (let* ((pane (hi::hemlock-view-pane view))
         (bounds (#/bounds (#/contentView (text-pane-scroll-view pane))))
         (tv (text-pane-text-view pane))
         (char-width (text-view-char-width tv))
         (line-height (text-view-line-height tv)))
    (values (floor (ns:ns-rect-width bounds) char-width)
            (floor (ns:ns-rect-height bounds) line-height))))


(defmethod hemlock-ext:change-active-pane ((view hi:hemlock-view) new-pane)
  #+debug (log-debug "change active pane to ~s" new-pane)
  (let* ((pane (hi::hemlock-view-pane view))
	 (text-view (text-pane-text-view pane))
	 (tv (ecase new-pane
	       (:echo (slot-value text-view 'peer))
	       (:text text-view))))
    (activate-hemlock-view tv)))

(defclass echo-area-view (hemlock-textstorage-text-view)
    ()
  (:metaclass ns:+ns-object))
(declaim (special echo-area-view))

(defmethod compute-temporary-attributes ((self echo-area-view))
)

(defmethod update-paren-highlight ((self echo-area-view))
)

(defmethod hemlock-view ((self echo-area-view))
  (slot-value self 'hemlock-view))

;;; The "document" for an echo-area isn't a real NSDocument.
(defclass echo-area-document (ns:ns-object)
    ((textstorage :foreign-type :id))
  (:metaclass ns:+ns-object))

(defmethod hemlock-buffer ((self echo-area-document))
  (let ((ts (slot-value self 'textstorage)))
    (unless (%null-ptr-p ts)
      (hemlock-buffer ts))))

(objc:defmethod #/undoManager ((self echo-area-document))
  +null-ptr+) ;For now, undo is not supported for echo-areas

(defmethod update-buffer-package ((doc echo-area-document))
  nil)

(defmethod document-invalidate-modeline ((self echo-area-document))
  nil)

(objc:defmethod (#/close :void) ((self echo-area-document))
  (let* ((ts (slot-value self 'textstorage)))
    (unless (%null-ptr-p ts)
      (setf (slot-value self 'textstorage) (%null-ptr))
      (close-hemlock-textstorage ts))))

(objc:defmethod (#/updateChangeCount: :void) ((self echo-area-document) (change :<NSD>ocument<C>hange<T>ype))
  (declare (ignore change)))

(defun make-echo-area (the-hemlock-frame x y width height main-buffer color)
  (let* ((box (make-instance 'ns:ns-view :with-frame (ns:make-ns-rect x y width height))))
    (#/setAutoresizingMask: box #$NSViewWidthSizable)
    (let* ((box-frame (#/bounds box))
           (containersize (ns:make-ns-size large-number-for-text (ns:ns-rect-height box-frame)))
           (clipview (make-instance 'ns:ns-clip-view
                                    :with-frame box-frame)))
      (#/setAutoresizingMask: clipview (logior #$NSViewWidthSizable
                                               #$NSViewHeightSizable))
      (#/setBackgroundColor: clipview color)
      (#/addSubview: box clipview)
      (#/setAutoresizesSubviews: box t)
      (#/release clipview)
      (let* ((buffer (hi::make-echo-buffer))
             (textstorage
              (progn
		;; What's the reason for sharing this?  Is it just the lock?
                (setf (hi::buffer-gap-context buffer) (hi::ensure-buffer-gap-context main-buffer))
                (make-textstorage-for-hemlock-buffer buffer)))
             (doc (make-instance 'echo-area-document))
             (layout (make-instance 'ns:ns-layout-manager))
             (container (#/autorelease
                         (make-instance 'ns:ns-text-container
                                        :with-container-size
                                        containersize))))
        (#/addLayoutManager: textstorage layout)
	(#/setUsesScreenFonts: layout *use-screen-fonts*)
        (#/addTextContainer: layout container)
        (#/release layout)
        (let* ((echo (make-instance 'echo-area-view
                                    :with-frame box-frame
                                    :text-container container))
               (info (hi::buffer-selection-info (hemlock-buffer textstorage))))
          (setf (view-selection-info echo) info
                (hi::selection-info-view info) echo)
          (#/setMinSize: echo (pref box-frame :<NSR>ect.size))
          (#/setMaxSize: echo (ns:make-ns-size large-number-for-text large-number-for-text))
          (#/setRichText: echo nil)
          #-cocotron
          (#/setUsesFontPanel: echo nil)
          (#/setHorizontallyResizable: echo t)
          (#/setVerticallyResizable: echo nil)
          (#/setAutoresizingMask: echo #$NSViewNotSizable)
          (#/setBackgroundColor: echo color)
          (#/setWidthTracksTextView: container nil)
          (#/setHeightTracksTextView: container nil)
          (#/setMenu: echo +null-ptr+)
          (setf (hemlock-frame-echo-area-buffer the-hemlock-frame) buffer
                (slot-value doc 'textstorage) textstorage
                (hi::buffer-document buffer) doc)
          (#/setDocumentView: clipview echo)
          (#/setAutoresizesSubviews: clipview nil)
          (#/sizeToFit echo)
          (values echo box))))))
		    
(defun make-echo-area-for-window (w main-buffer color)
  (let* ((content-view (#/contentView w))
	 (bounds (#/bounds content-view))
         (height (+ 1 (size-of-char-in-font *editor-font*))))
    (multiple-value-bind (echo-area box)
			 (make-echo-area w
					 0.0f0
					 0.0f0
					 (- (ns:ns-rect-width bounds) 16.0f0)
                                         height
					 main-buffer
					 color)
      (#/addSubview: content-view box)
      echo-area)))
               
(defclass hemlock-frame (ns:ns-window)
    ((echo-area-view :foreign-type :id)
     (pane :foreign-type :id)
     (echo-area-buffer :initform nil :accessor hemlock-frame-echo-area-buffer)
     (echo-area-stream :initform nil :accessor hemlock-frame-echo-area-stream)
     (is-dup :initform nil)
     (wrap-lines-to-window :initform *wrap-lines-to-window*
                           :accessor wrap-lines-to-window))
  (:metaclass ns:+ns-object))
(declaim (special hemlock-frame))

(objc:defmethod (#/setFrameAutosaveName: #>BOOL) ((self hemlock-frame)
                                                  string)
  (unless (slot-value self 'is-dup)
    (call-next-method string)))

;;; If a window's document's edited status changes, update the modeline.
(objc:defmethod (#/setDocumentEdited: :void) ((w hemlock-frame)
                                              (edited #>BOOL))
  (let* ((was-edited (#/isDocumentEdited w)))
    (unless (eq was-edited edited)
      (#/setNeedsDisplay: (text-pane-mode-line (slot-value w 'pane)) t)))
  (call-next-method edited))

(objc:defmethod (#/dealloc :void) ((self hemlock-frame))
  (let* ((pane (slot-value self 'pane))
         (echo-view (slot-value self 'echo-area-view)))
    (unless (%null-ptr-p pane)
      (setf (slot-value self 'pane) (%null-ptr))
      (#/release pane))
    (unless (%null-ptr-p echo-view)
      (setf (slot-value self 'echo-area-view) (%null-ptr))
      (#/release echo-view))
    (objc:remove-lisp-slots self)
    (call-next-method)))
  

(objc:defmethod (#/miniaturize: :void) ((w hemlock-frame) sender)
  (let* ((event (#/currentEvent w))
         (flags (#/modifierFlags event)))
    (if (logtest #$NSControlKeyMask flags)
      (progn
        (#/orderOut: w nil)
        (#/changeWindowsItem:title:filename: *nsapp* w (#/title w) nil))
      (call-next-method sender))))

(defmethod hemlock-view ((frame hemlock-frame))
  (let ((pane (slot-value frame 'pane)))
    (when (and pane (not (%null-ptr-p pane)))
      (hemlock-view pane))))

(objc:defmethod (#/runErrorSheet: :void) ((self hemlock-frame) message)
  #+debug (#_NSLog #@"runErrorSheet: signal = %@" :id signal)
  (#_NSBeginAlertSheet #@"Error in Hemlock command processing" ;title
                       (if (logbitp 0 (random 2))
                         #@"Not OK, but what can you do?"
                         #@"The sky is falling. FRED never did this!")
                       +null-ptr+
                       +null-ptr+
                       self
                       self
                       +null-ptr+
                       +null-ptr+
                       +null-ptr+
                       message))

(defun report-condition-in-hemlock-frame (condition frame)
  (assume-cocoa-thread)
  (let ((message (nsstring-for-lisp-condition condition)))
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     frame
     (@selector #/runErrorSheet:)
     message
     t)))

(defmethod hemlock-ext:report-hemlock-error ((view hi:hemlock-view) condition debug-p)
  (when debug-p (maybe-log-callback-error condition))
  (let ((pane (hi::hemlock-view-pane view)))
    (when (and pane (not (%null-ptr-p pane)))
      (report-condition-in-hemlock-frame condition (#/window pane)))))

(defun window-menubar-height ()
  #+cocotron (objc:objc-message-send (ccl::@class "NSMainMenuView") "menuHeight" #>CGFloat)
  #-cocotron 0.0f0)

(defun new-hemlock-document-window (class)
  (let* ((w (new-cocoa-window :class class
                              :activate nil))
         (echo-area-height (+ 1 (size-of-char-in-font *editor-font*))))
      (values w (add-pane-to-window w :reserve-below echo-area-height))))



(defun add-pane-to-window (w &key (reserve-above 0.0f0) (reserve-below 0.0f0))
  (let* ((window-content-view (#/contentView w))
	 (window-frame (#/frame window-content-view)))
    (ns:with-ns-rect (pane-rect  0 reserve-below (ns:ns-rect-width window-frame) (- (ns:ns-rect-height window-frame) (+ reserve-above reserve-below)))
       (let* ((pane (make-instance 'text-pane :with-frame pane-rect)))
	 (#/addSubview: window-content-view pane)
         (#/setDelegate: w pane)
         ;; Cocotron doesn't set the new window's initialFirstResponder which means
         ;; that the user must click in the window before they can edit.  So, do it here.
         ;; Remove this when Cocotron issue #374 is fixed
         ;;  (http://code.google.com/p/cocotron/issues/detail?id=374)
         #+cocotron (#/setInitialFirstResponder: w pane)
	 pane))))

(defun textpane-for-textstorage (class ts ncols nrows container-tracks-text-view-width color style)
  (let* ((pane (nth-value
                1
                (new-hemlock-document-window class))))
    (make-scrolling-textview-for-pane pane ts container-tracks-text-view-width color style)
    (multiple-value-bind (height width)
        (size-of-char-in-font (default-font))
      (size-text-pane pane height width nrows ncols))
    pane))




(defun hemlock-buffer-from-nsstring (nsstring name &rest modes)
  (let* ((buffer (make-hemlock-buffer name :modes modes)))
    (nsstring-to-buffer nsstring buffer)))

(defun %nsstring-to-hemlock-string (nsstring)
  "returns line-termination of string"
  (let* ((string (lisp-string-from-nsstring nsstring))
         (lfpos (position #\linefeed string))
         (crpos (position #\return string))
         (line-termination (if crpos
                             (if (eql lfpos (1+ crpos))
                               :crlf
                               :cr)
			     :unix))
	 (hemlock-string (case line-termination
			   (:crlf (remove #\return string))
			   (:cr (nsubstitute #\linefeed #\return string))
			   (t string))))
    (values hemlock-string line-termination)))

;: TODO: I think this is jumping through hoops because it want to be invokable outside the main
;; cocoa thread.
(defun nsstring-to-buffer (nsstring buffer)
  (let* ((document (hi::buffer-document buffer))
	 (hi::*current-buffer* buffer)
         (region (hi::buffer-region buffer)))
    (multiple-value-bind (hemlock-string line-termination)
			 (%nsstring-to-hemlock-string nsstring)
      (setf (hi::buffer-line-termination buffer) line-termination)

      (setf (hi::buffer-document buffer) nil) ;; What's this about??
      (unwind-protect
	  (let ((point (hi::buffer-point buffer)))
	    (hi::delete-region region)
	    (hi::insert-string point hemlock-string)
	    (setf (hi::buffer-modified buffer) nil)
	    (hi::buffer-start point)
	    ;; TODO: why would this be needed? insert-string should take care of any internal bookkeeping.
	    (hi::renumber-region region)
	    buffer)
	(setf (hi::buffer-document buffer) document)))))


(setq hi::*beep-function* #'(lambda (stream)
			      (declare (ignore stream))
			      (#_NSBeep)))


;;; This function must run in the main event thread.
(defun %hemlock-frame-for-textstorage (class ts ncols nrows container-tracks-text-view-width color style)
  (assume-cocoa-thread)
  (let* ((pane (textpane-for-textstorage class ts ncols nrows container-tracks-text-view-width color style))
         (buffer (hemlock-buffer ts))
         (frame (#/window pane))
         (echo-area (make-echo-area-for-window frame buffer color))
	 (echo-buffer (hemlock-buffer (#/textStorage echo-area)))
         (tv (text-pane-text-view pane)))
    #+GZ (assert echo-buffer)
    (with-slots (peer) tv
      (setq peer echo-area))
    (with-slots (peer) echo-area
      (setq peer tv))
    (setf (slot-value frame 'echo-area-view) echo-area
          (slot-value frame 'pane) pane)
    (let* ((hemlock-view
            (make-instance 'hi:hemlock-view
                           :buffer buffer
                           :pane pane
                           :echo-area-buffer echo-buffer)))
      (setf (slot-value pane 'hemlock-view)
            hemlock-view
            (slot-value tv 'hemlock-view)
            hemlock-view
            (slot-value echo-area 'hemlock-view)
            hemlock-view))
    (activate-hemlock-view tv)
    frame))

(defun hemlock-ext:invoke-modifying-buffer-storage (buffer thunk)
  (assume-cocoa-thread)
  (when buffer ;; nil means just get rid of any prior buffer
    (setq buffer (require-type buffer 'hi::buffer)))
  (let ((old *buffer-being-edited*))
    (if (eq buffer old)
      (funcall thunk)
      (unwind-protect
	  (progn
	    (buffer-document-end-editing old)
	    (buffer-document-begin-editing buffer)
	    (funcall thunk))
	(buffer-document-end-editing buffer)
	(buffer-document-begin-editing old)))))

(defun buffer-document-end-editing (buffer)
  (when buffer
    (let* ((document (hi::buffer-document (require-type buffer 'hi::buffer))))
      (when document
	(setq *buffer-being-edited* nil)
	(let ((ts (slot-value document 'textstorage)))
          (#/endEditing ts)
	  (update-hemlock-selection ts))))))

(defun buffer-document-begin-editing (buffer)
  (when buffer
    (let* ((document (hi::buffer-document buffer)))
      (when document
	(setq *buffer-being-edited* buffer)
	(#/beginEditing (slot-value document 'textstorage))))))

(defun document-edit-level (document)
  (assume-cocoa-thread) ;; see comment in #/editingInProgress
  (slot-value (slot-value document 'textstorage) 'edit-count))

(defun buffer-edit-level (buffer)
  (if buffer
    (let* ((document (hi::buffer-document buffer)))
      (if document
        (document-edit-level document)
        0))
    0))

(defun hemlock-ext:invoke-allowing-buffer-display (buffer thunk)
  ;; Call THUNK with the buffer's edit-level at 0, then restore the buffer's edit level.
  (let* ((level (buffer-edit-level buffer)))
    (dotimes (i level) (buffer-document-end-editing buffer))
    (unwind-protect
        (funcall thunk)
      (dotimes (i level) (buffer-document-begin-editing buffer)))))


(defun buffer-document-modified (buffer)
  (let* ((doc (hi::buffer-document buffer)))
    (if doc
      (#/isDocumentEdited doc))))

(defun perform-edit-change-notification (textstorage selector pos n &optional (extra 0))
  (with-lock-grabbed (*buffer-change-invocation-lock*)
    (let* ((invocation *buffer-change-invocation*))
      (rlet ((ppos :<NSI>nteger pos)
             (pn :<NSI>nteger n)
             (pextra :<NSI>nteger extra))
        (#/setTarget: invocation textstorage)
        (#/setSelector: invocation selector)
        (#/setArgument:atIndex: invocation ppos 2)
        (#/setArgument:atIndex: invocation pn 3)
        (#/setArgument:atIndex: invocation pextra 4))
      (#/performSelectorOnMainThread:withObject:waitUntilDone:
       invocation
       (@selector #/invoke)
       +null-ptr+
       t))))




(defun hemlock-ext:buffer-note-font-change (buffer region font)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage)))
           (pos (hi:mark-absolute-position (hi::region-start region)))
           (n (- (hi:mark-absolute-position (hi::region-end region)) pos)))
      (if (eq *current-process* *cocoa-event-process*)
        (#/noteHemlockAttrChangeAtPosition:length:fontNum: textstorage
                                                           pos
                                                           n
                                                           font)
        (perform-edit-change-notification textstorage
                                          (@selector #/noteHemlockAttrChangeAtPosition:length:fontNum:)
                                          pos
                                          n
                                          font)))))

(defun buffer-active-font-attributes (buffer)
  (let* ((style 0)
         (region (hi::buffer-active-font-region buffer))
         (textstorage (slot-value (hi::buffer-document buffer) 'textstorage))
         (styles (#/styles textstorage)))
    (when region
      (let* ((start (hi::region-end region)))
        (setq style (hi::font-mark-font start))))
    (#/objectAtIndex: styles style)))
      
;; Note that inserted a string of length n at mark.  Assumes this is called after
;; buffer marks were updated.
(defun hemlock-ext:buffer-note-insertion (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        (let* ((pos (hi:mark-absolute-position mark)))
          (when (eq (hi::mark-%kind mark) :left-inserting)
	    ;; Make up for the fact that the mark moved forward with the insertion.
	    ;; For :right-inserting and :temporary marks, they should be left back.
            (decf pos n))
          (if (eq *current-process* *cocoa-event-process*)
            (#/noteHemlockInsertionAtPosition:length:extra: textstorage
                                                            pos
                                                            n
                                                            0)
            (perform-edit-change-notification textstorage
                                              (@selector #/noteHemlockInsertionAtPosition:length:extra:)
                                              pos
                                              n)))))))

(defun hemlock-ext:buffer-note-modification (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        (if (eq *current-process* *cocoa-event-process*)
          (#/noteHemlockModificationAtPosition:length:extra: textstorage
                                                             (hi:mark-absolute-position mark)
                                                             n
                                                             0)
          (perform-edit-change-notification textstorage
                                            (@selector #/noteHemlockModificationAtPosition:length:extra:)
                                            (hi:mark-absolute-position mark)
                                            n))))))
  

(defun hemlock-ext:buffer-note-deletion (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        (let* ((pos (hi:mark-absolute-position mark)))
          (if (eq *current-process* *cocoa-event-process*)
            (#/noteHemlockDeletionAtPosition:length:extra: textstorage
                                                           pos
                                                           (abs n)
                                                           0)
            (perform-edit-change-notification textstorage
                                              (@selector #/noteHemlockDeletionAtPosition:length:extra:)
                                              pos
                                              (abs n))))))))



(defun hemlock-ext:note-buffer-saved (buffer)
  (assume-cocoa-thread)
  (let* ((document (hi::buffer-document buffer)))
    (when document
      ;; Hmm... I guess this is always done by the act of saving.
      nil)))

(defun hemlock-ext:note-buffer-unsaved (buffer)
  (assume-cocoa-thread)
  (let* ((document (hi::buffer-document buffer)))
    (when document
      (#/updateChangeCount: document #$NSChangeCleared))))


(defun size-of-char-in-font (f)
  (let* ((sf (#/screenFont f))
         (screen-p *use-screen-fonts*))
    (if (%null-ptr-p sf) (setq sf f screen-p nil))
    (let* ((layout (#/autorelease (#/init (#/alloc ns:ns-layout-manager)))))
      (#/setUsesScreenFonts: layout screen-p)
      (values (fround (#/defaultLineHeightForFont: layout sf))
              (fround (ns:ns-size-width (#/advancementForGlyph: sf (char-code #\space))))))))
         


(defun size-text-pane (pane line-height char-width nrows ncols)
  (let* ((tv (text-pane-text-view pane))
         (height (fceiling (* nrows line-height)))
	 (width (fceiling (* ncols char-width)))
	 (scrollview (text-pane-scroll-view pane))
	 (window (#/window scrollview))
         (has-horizontal-scroller (#/hasHorizontalScroller scrollview))
         (has-vertical-scroller (#/hasVerticalScroller scrollview)))
    (ns:with-ns-size (tv-size
                      (+ width (* 2 (#/lineFragmentPadding (#/textContainer tv))))
                      height)
      (when has-vertical-scroller 
	(#/setVerticalLineScroll: scrollview line-height)
	(#/setVerticalPageScroll: scrollview (cgfloat 0.0) #|line-height|#))
      (when has-horizontal-scroller
	(#/setHorizontalLineScroll: scrollview char-width)
	(#/setHorizontalPageScroll: scrollview (cgfloat 0.0) #|char-width|#))
      (let* ((sv-size (#/frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType: ns:ns-scroll-view tv-size has-horizontal-scroller has-vertical-scroller (#/borderType scrollview)))
             (pane-frame (#/frame pane))
             (margins (#/contentViewMargins pane)))
        (incf (ns:ns-size-height sv-size)
              (+ (ns:ns-rect-y pane-frame)
                 (* 2 (ns:ns-size-height  margins))))
        (incf (ns:ns-size-width sv-size)
              (ns:ns-size-width margins))
        (#/setContentSize: window sv-size)
        (setf (slot-value tv 'char-width) char-width
              (slot-value tv 'line-height) line-height)
	#-cocotron
        (#/setResizeIncrements: window
                                (ns:make-ns-size char-width line-height))))))
				    
  
(defclass hemlock-editor-window-controller (ns:ns-window-controller)
  ((sequence :foreign-type :int))
  (:metaclass ns:+ns-object))

;;; In certain cases, an NSTextView's selection changes without going
;;; through setSelectedRange: or similar methods.  In post-10.6
;;; systems, one of these cases is the find panel.  Synch up the
;;; selections between the NSTextView and the Hemlock buffer here.
(objc:defmethod (#/textViewDidChangeSelection: :void)
    ((self hemlock-editor-window-controller) notification)
  (let* ((hv (hemlock-view self))
	 (buffer (hi:hemlock-view-buffer hv))
	 (tv (#/object notification)))
    (with-view-selection-info (tv buffer)
      (let* ((range (#/selectedRange tv))
	     (v0 (ns:ns-range-location range))
	     (v1 (+ v0 (ns:ns-range-length range))))
	(multiple-value-bind (b0 b1) (hi:buffer-selection-range buffer)
	  ;; If the selections differ, synch them up.
	  (unless (and (= b0 v0)
		       (= b1 v1))
	    (let ((point (hi:buffer-point buffer)))
	      (hi:move-to-absolute-position point v0)
	      (when (> v1 v0)
		(let ((mark (hi:copy-mark point :right-inserting)))
		  (hi:move-to-absolute-position mark v1)
		  (hemlock::%buffer-push-buffer-mark buffer mark t))))))))))

(objc:defmethod #/windowTitleForDocumentDisplayName: ((self hemlock-editor-window-controller) docname)
  (let* ((seq (slot-value self 'sequence)))
    (if (zerop seq)
      docname
      (#/stringWithFormat: ns:ns-string #@"%@ <%d>" docname seq))))
  

;;; This is borrowed from emacs.  The first click on the zoom button will
;;; zoom vertically.  The second will zoom completely.  The third will
;;; return to the original size.
(objc:defmethod (#/windowWillUseStandardFrame:defaultFrame: #>NSRect)
                ((wc hemlock-editor-window-controller) sender (default-frame #>NSRect))
  (let* ((r (#/frame sender)))
    (if (= (ns:ns-rect-height r) (ns:ns-rect-height default-frame))
      (setf r default-frame)
      (setf (ns:ns-rect-height r) (ns:ns-rect-height default-frame)
            (ns:ns-rect-y r) (ns:ns-rect-y default-frame)))
    r))

(objc:defmethod (#/windowWillClose: :void) ((wc hemlock-editor-window-controller)
                                            notification)
  (declare (ignore notification))
  ;; The echo area "document" should probably be a slot in the document
  ;; object, and released when the document object is.
  (let* ((w (#/window wc)))
    ;; guard against cocotron lossage
    (if (#/isKindOfClass: w hemlock-frame)
      (let* ((buf (hemlock-frame-echo-area-buffer w))
	     (echo-doc (if buf (hi::buffer-document buf))))
	(when echo-doc
	  (setf (hemlock-frame-echo-area-buffer w) nil)
	  (#/close echo-doc))
	(#/setFrameAutosaveName: w #@"")
	(#/autorelease w))
      (#_NSLog #@"window controller %@ got windowWillClose for odd window %@ "
	       :address wc :address w))))

(defmethod hemlock-view ((self hemlock-editor-window-controller))
  (let ((frame (#/window self)))
    (unless (%null-ptr-p frame)
      (hemlock-view frame))))

#-cocotron
(defun nsstring-encoding-for-character-encoding-name (name)
  (let* ((string (string name))
         (len (length string)))
    (with-cstrs ((cstr string))
      (with-nsstr (nsstr cstr len)
        (let* ((cf (#_CFStringConvertIANACharSetNameToEncoding nsstr)))
          (if (= cf #$kCFStringEncodingInvalidId)
            (setq cf (#_CFStringGetSystemEncoding)))
          (let* ((ns (#_CFStringConvertEncodingToNSStringEncoding cf)))
            (if (= ns #$kCFStringEncodingInvalidId)
              (#/defaultCStringEncoding ns:ns-string)
              ns)))))))

(defun nsstring-encoding-for-external-format (ef)
  (and ef (nsstring-encoding-for-character-encoding-name
           (ccl:external-format-character-encoding ef))))

;;; Map *default-file-character-encoding* to an :<NSS>tring<E>ncoding
(defun get-default-encoding ()
  #-cocotron                            ;need IANA conversion stuff
  (let* ((file-encoding *default-file-character-encoding*))
    (when (and (typep file-encoding 'keyword)
               (lookup-character-encoding file-encoding))
      (nsstring-encoding-for-character-encoding-name file-encoding))))

(defclass hemlock-document-controller (ns:ns-document-controller)
    ((last-encoding :foreign-type :<NSS>tring<E>ncoding))
  (:metaclass ns:+ns-object))
(declaim (special hemlock-document-controller))

(objc:defmethod #/init ((self hemlock-document-controller))
  (prog1
      (call-next-method)
    (setf (slot-value self 'last-encoding) 0)))


;;; The HemlockEditorDocument class.


(defclass hemlock-editor-document (ns:ns-document)
    ((textstorage :foreign-type :id)
     (encoding :foreign-type :<NSS>tring<E>ncoding)
     (dupcount :foreign-type :int))
  (:metaclass ns:+ns-object))

(defmethod hemlock-buffer ((self hemlock-editor-document))
  (let ((ts (slot-value self 'textstorage)))
    (unless (%null-ptr-p ts)
      (hemlock-buffer ts))))

(defmethod window-document ((w ns:ns-window))
  (let* ((sc (#/sharedDocumentController ns:ns-document-controller))
         (doc (#/documentForWindow: sc w)))
    (if (%null-ptr-p doc)
      nil
      doc)))

(defmethod window-pathname ((w ns:ns-window))
  (document-pathname (window-document w)))

(defmethod document-pathname ((doc NULL))
  nil)

(defmethod document-pathname ((doc hemlock-editor-document))
  (hi:buffer-pathname (hemlock-buffer doc)))

(defmethod assume-not-editing ((doc hemlock-editor-document))
  (assume-not-editing (slot-value doc 'textstorage)))

(defmethod document-invalidate-modeline ((self hemlock-editor-document))
  (for-each-textview-using-storage
   (slot-value self 'textstorage)
   #'(lambda (tv)
       (let* ((pane (text-view-pane tv)))
	 (unless (%null-ptr-p pane)
	   (#/setNeedsDisplay: (text-pane-mode-line pane) t))))))

(defmethod update-buffer-package ((doc hemlock-editor-document))
  (hemlock:update-current-package))

(defun hemlock-ext:note-selection-set-by-search (buffer)
  (let* ((doc (hi::buffer-document buffer)))
    (when doc
      (with-slots (textstorage) doc
	(when textstorage
	  (with-slots (selection-set-by-search) textstorage
	    (setq selection-set-by-search #$YES)))))))

(objc:defmethod (#/validateMenuItem: :<BOOL>)
    ((self hemlock-text-view) item)
  (let* ((action (#/action item)))
    #+debug (#_NSLog #@"action = %s" :address action)
    (cond ((eql action (@selector #/hyperSpecLookUp:))
           (and *hyperspec-lookup-enabled*
		(hyperspec-root-url)
                (with-string-under-cursor (self selection)
		  (and selection (nth-value 1 (find-symbol (nstring-upcase selection) "CL"))))))
          ((eql action (@selector #/cut:))
           (let* ((selection (#/selectedRange self)))
             (and (> (ns:ns-range-length selection))
                  (#/shouldChangeTextInRange:replacementString: self selection #@""))))
          ((eql action (@selector #/evalSelection:))
           (when (hemlock-view self)
             (if (eql 0 (ns:ns-range-length (#/selectedRange self)))
               ;; Should check whether there is a current form
               (#/setTitle: item #@"Execute Expression")
               (#/setTitle: item #@"Execute Selection"))
             t))
          ((eql action (@selector #/evalAll:))
           (let* ((doc (#/document (#/windowController (#/window self)))))
             (and (not (%null-ptr-p doc))
                  (eq (type-of doc) 'hemlock-editor-document))))
          ;; if this hemlock-text-view is in an editor window and its buffer has
          ;; an associated pathname, then activate the Load Buffer item
          ((or (eql action (@selector #/loadBuffer:))
               (eql action (@selector #/compileBuffer:))
               (eql action (@selector #/compileAndLoadBuffer:))) 
           (let* ((buffer (hemlock-buffer self))
                  (pathname (hi::buffer-pathname buffer)))
             (not (null pathname))))
          ((eql action (@selector #/openSelection:))
           (with-string-under-cursor (self selection)
             (pathname-for-namestring-fragment selection)))
          ((or (eql action (@selector #/duplicate:))
               (eql action (@selector #/showListDefinitions:)))
           ;; Duplicating a listener "works", but listeners have all kinds
           ;; of references to the original window and text view and get
           ;; confused if the original is closed before all duplicates are.
           ;; Listing definitions is not applicable to listeners.
           (let* ((doc (#/document (#/windowController (#/window self)))))
             (not (typep doc 'hemlock-listener-document))))
           
	  (t (call-next-method item)))))

(defmethod user-input-style ((doc hemlock-editor-document))
  0)

(defvar *encoding-name-hash* (make-hash-table))

(defmethod document-encoding-name ((doc hemlock-editor-document))
  (with-slots (encoding) doc
    (if (eql encoding 0)
      "Automatic"
      (or (gethash encoding *encoding-name-hash*)
          (setf (gethash encoding *encoding-name-hash*)
                (lisp-string-from-nsstring (nsstring-for-nsstring-encoding encoding)))))))

(defun hemlock-ext:buffer-encoding-name (buffer)
  (let ((doc (hi::buffer-document buffer)))
    (and doc (document-encoding-name doc))))

;; TODO: make each buffer have a slot, and this is just the default value.
(defmethod textview-background-color ((doc hemlock-editor-document))
  *editor-background-color*)


(objc:defmethod (#/setTextStorage: :void) ((self hemlock-editor-document) ts)
  (let* ((doc (%inc-ptr self 0))        ; workaround for stack-consed self
         (string (#/hemlockString ts))
         (buffer (hemlock-buffer string)))
    (unless (%null-ptr-p doc)
      (setf (slot-value doc 'textstorage) ts
            (hi::buffer-document buffer) doc))))

;; This runs on the main thread.
(objc:defmethod (#/revertToSavedFromFile:ofType: :<BOOL>)
    ((self hemlock-editor-document) filename filetype)
  (declare (ignore filetype))
  (assume-cocoa-thread)
  #+debug
  (#_NSLog #@"revert to saved from file %@ of type %@"
           :id filename :id filetype)
  (let* ((encoding (slot-value self 'encoding))
         (nsstring (make-instance ns:ns-string
                                  :with-contents-of-file filename
                                  :encoding encoding
                                  :error +null-ptr+))
         (buffer (hemlock-buffer self))
         (old-length (hemlock-buffer-length buffer))
	 (hi::*current-buffer* buffer)
         (textstorage (slot-value self 'textstorage))
         (point (hi::buffer-point buffer))
         (pointpos (hi:mark-absolute-position point)))
    (hemlock-ext:invoke-modifying-buffer-storage
     buffer
     #'(lambda ()
         (#/edited:range:changeInLength:
          textstorage #$NSTextStorageEditedCharacters (ns:make-ns-range 0 old-length) (- old-length))
         (nsstring-to-buffer nsstring buffer)
         (let* ((newlen (hemlock-buffer-length buffer)))
           (#/edited:range:changeInLength: textstorage  #$NSTextStorageEditedAttributes (ns:make-ns-range 0 0) newlen)
           (#/edited:range:changeInLength: textstorage #$NSTextStorageEditedCharacters (ns:make-ns-range 0 newlen) 0)
           (let* ((ts-string (#/hemlockString textstorage))
                  (display (hemlock-buffer-string-cache ts-string)))
             (reset-buffer-cache display) 
             (update-line-cache-for-index display 0)
             (move-hemlock-mark-to-absolute-position point
                                                     display
                                                     (min newlen pointpos))))
         (#/updateMirror textstorage)
         (setf (hi::buffer-modified buffer) nil)
         (hi::note-modeline-change buffer)))
    t))


(defvar *last-document-created* nil)

(setf (hemlock::value hemlock::default-modes) '("Lisp" "Editor"))


(objc:defmethod #/init ((self hemlock-editor-document))
  (let* ((doc (call-next-method)))
    (unless  (%null-ptr-p doc)
      (#/setTextStorage: doc (make-textstorage-for-hemlock-buffer
                              (make-hemlock-buffer
                               (lisp-string-from-nsstring
                                (#/displayName doc))))))
    (with-slots (encoding) doc
      (setq encoding (or (get-default-encoding) #$NSISOLatin1StringEncoding)))
    (setq *last-document-created* doc)
    doc))

  
(defun make-buffer-for-document (ns-document pathname)
  (let* ((buffer-name (hi::pathname-to-buffer-name pathname))
	 (buffer (make-hemlock-buffer buffer-name)))
    (setf (slot-value ns-document 'textstorage)
	  (make-textstorage-for-hemlock-buffer buffer))
    (setf (hi::buffer-pathname buffer) pathname)
    buffer))

;;; Try to read the URL's contents into an NSString which can be
;;; used to initialize the document's Hemlock buffer and related
;;; data structures.  First, try to use the encoding specified
;;; in the last call to the document controller's "open" panel;
;;; if that wasn't specified (was 0, "automatic") or if the string
;;; couldn't be initialized in that encoding, try to use the
;;; encoding specified in the "coding:" file option if that's present.
;;; If that wasn't specified or fails, fall back to the default
;;; encoding (based on CCL:*DEFAULT-FILE-CHARACTER-ENCODING*), and
;;; if that fails, try using :iso-8859-1 (which should always win
;;; but which may misinterpret some characters.)
;;; We should only lose because of a filesystem or permissions
;;; problem or because of a severe low-memory condition or something
;;; equally catastrophic.
;;; We should be careful to zero out the encoding from the last call
;;; to the "open" panel so that leftover value doesn't affect anything
;;; but the next call to this method, and if an encoding selected
;;; explicitly (via the "open" panel or the file-options line) didn't
;;; work, it'd be nice to (somehow) let the user know that.
;;; Whatever encoding works here is remembered as the document's
;;; encoding; that may be overridden when the file-options are parsed.
(objc:defmethod (#/readFromURL:ofType:error: :<BOOL>)
    ((self hemlock-editor-document) url type (perror (:* :id)))
  (declare (ignorable type))
  (with-callback-context "readFromURL"
    (let* ((data (#/dataWithContentsOfURL:options:error:
                  ns:ns-data url 0 perror))
           (bytes (#/bytes data))
           (length (#/length data))
           (pathname
            (lisp-string-from-nsstring
             (if (#/isFileURL url)
                   (#/path url)
               (#/absoluteString url))))
           (buffer (or (hemlock-buffer self)
                       (make-buffer-for-document self pathname)))
           (document-controller (#/sharedDocumentController (find-class 'hemlock-document-controller)))
           (string +null-ptr+))
      (flet ((try-encoding (encoding)
               (setq string 
                     (if (or (null encoding)
                             (zerop encoding))
                       +null-ptr+
                       (make-instance ns:ns-string
                                      :with-bytes-no-copy bytes
                                      :length length
                                      :encoding encoding
                                      :free-when-done nil)))
               (unless (%null-ptr-p string)
                 (setf (slot-value self 'encoding) encoding)
                 t)))
        (unless (try-encoding (with-slots (last-encoding) document-controller
                                (prog1 last-encoding
                                  (setq last-encoding 0))))
          (unless (try-encoding (nsstring-encoding-for-external-format (ccl::external-format-from-octet-buffer bytes length)))
            (unless (try-encoding (get-default-encoding))
              (try-encoding #$NSISOLatin1StringEncoding))))
        (unless (%null-ptr-p string)
          ;; ** TODO: Argh.  How about we just let hemlock insert it.
          (let* ((textstorage (slot-value self 'textstorage))
                 (display (hemlock-buffer-string-cache (#/hemlockString textstorage)))
                 (hi::*current-buffer* buffer))
            (hemlock-ext:invoke-modifying-buffer-storage
             buffer
             #'(lambda ()
                 (nsstring-to-buffer string buffer)
                 (reset-buffer-cache display) 
                 (#/updateMirror textstorage)
                 (update-line-cache-for-index display 0)
                 (textstorage-note-insertion-at-position
                  textstorage
                  0
                  (hemlock-buffer-length buffer))
                 (hi::note-modeline-change buffer)
                 (setf (hi::buffer-modified buffer) nil))))
            t)))))




(def-cocoa-default *editor-keep-backup-files* :bool t "maintain backup files")

(objc:defmethod (#/keepBackupFile :<BOOL>) ((self hemlock-editor-document))
  ;;; Don't use the NSDocument backup file scheme.
  nil)

(objc:defmethod (#/writeSafelyToURL:ofType:forSaveOperation:error: :<BOOL>)
    ((self hemlock-editor-document)
     absolute-url
     type
     (save-operation :<NSS>ave<O>peration<T>ype)
     (error (:* :id)))
  (when (and *editor-keep-backup-files*
             (eql save-operation #$NSSaveOperation))
    (write-hemlock-backup-file (#/fileURL self)))
  (call-next-method absolute-url type save-operation error))

(defun write-hemlock-backup-file (url)
  (unless (%null-ptr-p url)
    (when (#/isFileURL url)
      (let* ((path (#/path url)))
        (unless (%null-ptr-p path)
          (let* ((newpath (#/stringByAppendingString: path #@"~"))
                 (fm (#/defaultManager ns:ns-file-manager)))
            ;; There are all kinds of ways for this to lose.
            ;; In order for the copy to succeed, the destination can't exist.
            ;; (It might exist, but be a directory, or there could be
            ;; permission problems ...)
            (#/removeFileAtPath:handler: fm newpath +null-ptr+)
            (#/copyPath:toPath:handler: fm path newpath +null-ptr+)))))))

             



(defun hemlock-ext:all-hemlock-views ()
  "List of all hemlock views, in z-order, frontmost first"
  (loop for win in (windows)
    as buf = (and (typep win 'hemlock-frame) (hemlock-view win))
    when buf collect buf))

(defmethod document-panes ((document hemlock-editor-document))
  (let* ((ts (slot-value document 'textstorage))
	 (panes ()))
    (for-each-textview-using-storage
     ts
     #'(lambda (tv)
	 (let* ((pane (text-view-pane tv)))
	   (unless (%null-ptr-p pane)
	     (push pane panes)))))
    panes))

(objc:defmethod (#/noteEncodingChange: :void) ((self hemlock-editor-document)
                                               popup)
  (with-slots (encoding) self
    (setq encoding (nsinteger-to-nsstring-encoding (#/selectedTag popup)))
    (hi::note-modeline-change (hemlock-buffer self))))

#-cocotron
(objc:defmethod (#/prepareSavePanel: :<BOOL>) ((self hemlock-editor-document)
                                               panel)
  (let* ((popup (build-encodings-popup (#/sharedDocumentController hemlock-document-controller))))
      (#/setAction: popup (@selector #/noteEncodingChange:))
      (#/setTarget: popup self)
      (#/setAccessoryView: panel popup))
  (#/setExtensionHidden: panel nil)
  (#/setCanSelectHiddenExtension: panel nil)
  (#/setAllowedFileTypes: panel +null-ptr+)
  (call-next-method panel))


(defloadvar *ns-cr-string* (%make-nsstring (string #\return)))
(defloadvar *ns-lf-string* (%make-nsstring (string #\linefeed)))
(defloadvar *ns-crlf-string* (with-autorelease-pool (#/retain (#/stringByAppendingString: *ns-cr-string* *ns-lf-string*))))

(objc:defmethod (#/writeToURL:ofType:error: :<BOOL>)
    ((self hemlock-editor-document) url type (error (:* :id)))
  (declare (ignore type))
  (with-slots (encoding textstorage) self
    (let* ((string (#/string textstorage))
           (buffer (hemlock-buffer self)))
      (case (when buffer (hi::buffer-line-termination buffer))
        (:crlf (unless (typep string 'ns:ns-mutable-string)
		 (setq string (make-instance 'ns:ns-mutable-string :with string string))
		 (#/replaceOccurrencesOfString:withString:options:range:
		  string *ns-lf-string* *ns-crlf-string* #$NSLiteralSearch (ns:make-ns-range 0 (#/length string)))))
        (:cr (setq string (if (typep string 'ns:ns-mutable-string)
			    string
			    (make-instance 'ns:ns-mutable-string :with string string)))
	     (#/replaceOccurrencesOfString:withString:options:range:
	      string *ns-lf-string* *ns-cr-string* #$NSLiteralSearch (ns:make-ns-range 0 (#/length string)))))
      (when (#/writeToURL:atomically:encoding:error:
             string url t encoding error)
        (when buffer
          (setf (hi::buffer-modified buffer) nil))
        t))))

;;; Cocotron's NSDocument uses the deprecated as of 10.4 methods to implement the NSSavePanel
#+cocotron
(objc:defmethod (#/writeToFile:ofType: :<BOOL>)
    ((self hemlock-editor-document) path type)
  (rlet ((perror :id +null-ptr+))
    (#/writeToURL:ofType:error: self (#/fileURLWithPath: ns:ns-url path) type perror)))


;;; Shadow the setFileURL: method, so that we can keep the buffer
;;; name and pathname in synch with the document.
(objc:defmethod (#/setFileURL: :void) ((self hemlock-editor-document)
                                        url)
  (call-next-method url)
  (let* ((path nil)
         (controllers (#/windowControllers self)))
    (dotimes (i (#/count controllers))
      (let* ((controller (#/objectAtIndex: controllers i))
             (window (#/window controller)))
        (#/setFrameAutosaveName: window (or path (setq path (#/path url)))))))
  (let* ((buffer (hemlock-buffer self)))
    (when buffer
      (let* ((new-pathname (lisp-string-from-nsstring (#/path url))))
	(setf (hi::buffer-name buffer) (hi::pathname-to-buffer-name new-pathname))
	(setf (hi::buffer-pathname buffer) new-pathname)))))


(def-cocoa-default *initial-editor-x-pos* :float 20.0f0 "X position of upper-left corner of initial editor")

(def-cocoa-default *initial-editor-y-pos* :float 10.0f0 "Y position of upper-left corner of initial editor")

(defloadvar *editor-cascade-point* nil)

(defloadvar *next-editor-x-pos* nil) ; set after defaults initialized
(defloadvar *next-editor-y-pos* nil)

(defun x-pos-for-window (window x)
  (let* ((frame (#/frame window))
         (screen (#/screen window)))
    (if (%null-ptr-p screen) (setq screen (#/mainScreen ns:ns-screen)))
    (let* ((screen-rect (#/visibleFrame screen)))
      (if (>= x 0)
        (+ x (ns:ns-rect-x screen-rect))
        (- (+ (ns:ns-rect-width screen-rect) x) (ns:ns-rect-width frame))))))

(defun y-pos-for-window (window y)
  (let* ((frame (#/frame window))
         (screen (#/screen window)))
    (if (%null-ptr-p screen) (setq screen (#/mainScreen ns:ns-screen)))
    (let* ((screen-rect (#/visibleFrame screen)))
      (if (>= y 0)
        (+ y (ns:ns-rect-y screen-rect) (ns:ns-rect-height frame))
        (+ (ns:ns-rect-height screen-rect) y)))))

(objc:defmethod (#/makeWindowControllers :void) ((self hemlock-editor-document))
  #+debug
  (#_NSLog #@"Make window controllers")
    (let* ((textstorage  (slot-value self 'textstorage))
           (window (%hemlock-frame-for-textstorage
                    hemlock-frame
                    textstorage
                    *editor-columns*
                    *editor-rows*
                    nil
                    (textview-background-color self)
                    (user-input-style self)))
           (dupcount (slot-value self 'dupcount))
           (controller (make-instance
                           'hemlock-editor-window-controller
                         :with-window window))
           (url (#/fileURL self))
           (path (unless (%null-ptr-p url) (#/path url))))
      ;;(#/setDelegate: window self)
      (#/setDelegate: window controller)
      (setf (slot-value controller 'sequence) dupcount)
      (#/setDelegate: (text-pane-text-view (slot-value window 'pane)) controller)
      (#/addWindowController: self controller)
      (#/release controller)
      (#/setShouldCascadeWindows: controller nil)
      (unless (eql dupcount 0)
        (setf (slot-value window 'is-dup) t))
      (when path
        (unless (and (eql dupcount 0) (#/setFrameAutosaveName: window path))
          (setq path nil)))
      (unless (and path
                   (#/setFrameUsingName: window path))
        ;; Cascade windows from the top left corner of the topmost editor window.
        ;; If there's no editor window, use the default position.
        (flet ((editor-window-p (w)
                 (and (not (eql w window))
                      (eql (#/class (#/windowController w))
                           (find-class 'hemlock-editor-window-controller)))))
          (let* ((editors (remove-if-not #'editor-window-p (windows)))
                 (top-editor (car editors)))
            (if top-editor
              (ns:with-ns-point (zp 0 0)
                (setq *editor-cascade-point* (#/cascadeTopLeftFromPoint:
                                              top-editor zp)))
              (let* ((screen-frame (#/visibleFrame (#/screen window)))
                     (pt (ns:make-ns-point *initial-editor-x-pos*
                                           (- (ns:ns-rect-height screen-frame)
                                              *initial-editor-y-pos*))))
                (setq *editor-cascade-point* pt)))))
        (#/cascadeTopLeftFromPoint: window *editor-cascade-point*))
      (when (eql dupcount 0)
        (let ((view (hemlock-view window)))
          (hi::handle-hemlock-event view #'(lambda ()
                                             (hi::process-file-options)))))
      (#/synchronizeWindowTitleWithDocumentName controller)))

(objc:defmethod (#/duplicate: :void) ((self hemlock-frame) sender)
  (declare (ignorable sender))
  (let* ((self-controller (#/windowController self))
         (doc (#/document self-controller)))
    (when (typep doc 'hemlock-editor-document
      (let* ((sequence-number (incf (slot-value doc 'dupcount))))
        (#/makeWindowControllers doc)
        ;; Now we have to find the window controller that was just made ...
        (let* ((controllers (#/windowControllers doc))
               (count (#/count controllers))
               (controller (dotimes (i count)
                             (let* ((c (#/objectAtIndex: controllers i)))
                               (when (eql sequence-number (slot-value c 'sequence))
                                 (return c))))))

          (when controller
            (let* ((window (#/window controller))
                   (new-text-view (text-pane-text-view (slot-value window 'pane)))
                   (new-selection-info (view-selection-info new-text-view))
                   (old-selection-info (view-selection-info (text-pane-text-view (slot-value self 'pane))))
                   (old-mark (hi::selection-info-%mark old-selection-info)))
              (hi::move-mark (hi::selection-info-point new-selection-info)
                             (hi::selection-info-point old-selection-info))
              (setf (hi::selection-info-%mark new-selection-info)
                    (if old-mark (hi::copy-mark old-mark))
                    (hi::selection-info-region-active new-selection-info)
                    (hi::selection-info-region-active old-selection-info))
              (update-hemlock-selection (#/textStorage new-text-view))
              (#/scrollRangeToVisible: new-text-view
                                       (#/selectedRange new-text-view))
              (#/makeKeyAndOrderFront: window +null-ptr+)))))))))
              
      

(objc:defmethod (#/close :void) ((self hemlock-editor-document))
  #+debug
  (#_NSLog #@"Document close: %@" :id self)
  (let* ((textstorage (slot-value self 'textstorage)))
    (unless (%null-ptr-p textstorage)
      (setf (slot-value self 'textstorage) (%null-ptr))
      (close-hemlock-textstorage textstorage)))
  (call-next-method))

(objc:defmethod (#/dealloc :void) ((self hemlock-editor-document))
  (let* ((textstorage (slot-value self 'textstorage)))
    (unless (%null-ptr-p textstorage)
      (setf (slot-value self 'textstorage) (%null-ptr))
      (close-hemlock-textstorage textstorage)))
  (objc:remove-lisp-slots self)
  (call-next-method))



(defmethod view-screen-lines ((view hi:hemlock-view))
    (let* ((pane (hi::hemlock-view-pane view)))
      (floor (ns:ns-size-height (#/contentSize (text-pane-scroll-view pane)))
             (text-view-line-height (text-pane-text-view pane)))))

;; Beware this doesn't seem to take horizontal scrolling into account.
(defun visible-charpos-range (tv)
  (let* ((rect (#/visibleRect tv))
         (container-origin (#/textContainerOrigin tv))
         (layout (#/layoutManager tv)))
    ;; Convert from view coordinates to container coordinates
    (decf (pref rect :<NSR>ect.origin.x) (pref container-origin :<NSP>oint.x))
    (decf (pref rect :<NSR>ect.origin.y) (pref container-origin :<NSP>oint.y))
    (let* ((glyph-range (#/glyphRangeForBoundingRect:inTextContainer:
                         layout rect (#/textContainer tv)))
           (char-range (#/characterRangeForGlyphRange:actualGlyphRange:
                        layout glyph-range +null-ptr+)))
      (values (pref char-range :<NSR>ange.location)
              (pref char-range :<NSR>ange.length)))))

(defun charpos-xy (tv charpos)
  (let* ((layout (#/layoutManager tv))
         (glyph-range (#/glyphRangeForCharacterRange:actualCharacterRange:
                       layout
                       (ns:make-ns-range charpos 0)
                       +null-ptr+))
         (rect (#/boundingRectForGlyphRange:inTextContainer:
                layout
                glyph-range
                (#/textContainer tv)))
         (container-origin (#/textContainerOrigin tv)))
    (values (+ (pref rect :<NSR>ect.origin.x) (pref container-origin :<NSP>oint.x))
            (+ (pref rect :<NSR>ect.origin.y) (pref container-origin :<NSP>oint.y)))))

;;(nth-value 1 (charpos-xy tv (visible-charpos-range tv))) - this is smaller as it
;; only includes lines fully scrolled off...
(defun text-view-vscroll (tv)
  ;; Return the number of pixels scrolled off the top of the view.
  (let* ((scroll-view (text-pane-scroll-view (text-view-pane tv)))
         (clip-view (#/contentView scroll-view))
         (bounds (#/bounds clip-view)))
    (ns:ns-rect-y bounds)))

(defun set-text-view-vscroll (tv vscroll)
  (let* ((scroll-view (text-pane-scroll-view (text-view-pane tv)))
         (clip-view (#/contentView scroll-view))
         (bounds (#/bounds clip-view)))
    (decf vscroll (mod vscroll (text-view-line-height tv))) ;; show whole line
    (ns:with-ns-point (new-origin (ns:ns-rect-x bounds) vscroll)
      (#/scrollToPoint: clip-view (#/constrainScrollPoint: clip-view new-origin))
      (#/reflectScrolledClipView: scroll-view clip-view))))

(defun scroll-by-lines (tv nlines)
  "Change the vertical origin of the containing scrollview's clipview"
  (set-text-view-vscroll tv (+ (text-view-vscroll tv)
                               (* nlines (text-view-line-height tv)))))

;; TODO: should be a hemlock variable..
(defvar *next-screen-context-lines* 2)

(defmethod hemlock-ext:scroll-view ((view hi:hemlock-view) how &optional where)
  (assume-cocoa-thread)
  (let* ((tv (text-pane-text-view (hi::hemlock-view-pane view)))
         (may-change-selection t))
    (when (eq how :line)
      (setq where (require-type where '(integer 0)))
      (let* ((line-y (nth-value 1 (charpos-xy tv where)))
             (top-y (text-view-vscroll tv))
             (nlines (floor (- line-y top-y) (text-view-line-height tv))))
        (setq how :lines-down where nlines)))
    (ecase how
      (:center-selection
       (#/centerSelectionInVisibleArea: tv +null-ptr+))
      ((:page-up :view-page-up)
       (when (eq how :view-page-up)
         (setq may-change-selection nil))
       (require-type where 'null)
       ;; TODO: next-screen-context-lines
       (scroll-by-lines tv (- *next-screen-context-lines* (view-screen-lines view))))
      ((:page-down :view-page-down)
       (when (eq how :view-page-down)
         (setq may-change-selection nil))
       (require-type where 'null)
       (scroll-by-lines tv (- (view-screen-lines view) *next-screen-context-lines*)))
      (:lines-up
       (scroll-by-lines tv (- (require-type where 'integer))))
      (:lines-down
       (scroll-by-lines tv (require-type where 'integer))))
    ;; If point is not on screen, move it.
    (when may-change-selection
      (let* ((point (hi::current-point))
             (point-pos (hi::mark-absolute-position point)))
        (multiple-value-bind (win-pos win-len) (visible-charpos-range tv)
          (unless (and (<= win-pos point-pos) (< point-pos (+ win-pos win-len)))
            (let* ((point (hi::current-point-collapsing-selection))
                   (cache (hemlock-buffer-string-cache (#/hemlockString (#/textStorage tv)))))
              (move-hemlock-mark-to-absolute-position point cache win-pos)
              (update-hemlock-selection (#/textStorage tv)))))))))

(defun iana-charset-name-of-nsstringencoding (ns)
  #+cocotron (declare (ignore ns))
  #+cocotron +null-ptr+
  #-cocotron
  (#_CFStringConvertEncodingToIANACharSetName
   (#_CFStringConvertNSStringEncodingToEncoding ns)))
    
(defun nsstring-for-nsstring-encoding (ns)
  (let* ((iana (iana-charset-name-of-nsstringencoding ns)))
    (if (%null-ptr-p iana)
      (#/stringWithFormat: ns:ns-string #@"{%@}"
                           (#/localizedNameOfStringEncoding: ns:ns-string ns))
      iana)))

;;; Return T if the specified #>NSStringEncoding names something that
;;; CCL supports.  (Could also have a set of other encoding names that
;;; the user is interested in, maintained by preferences.

(defun supported-string-encoding-p (ns-string-encoding)
  #-cocotron
  (let* ((cfname (#_CFStringConvertEncodingToIANACharSetName
                  (#_CFStringConvertNSStringEncodingToEncoding ns-string-encoding)))
         (name (unless (%null-ptr-p cfname)
                 (nstring-upcase (ccl::lisp-string-from-nsstring cfname))))
         (keyword (when (and name (find-symbol name "KEYWORD"))
                    (intern name "KEYWORD"))))
    (or (and keyword (not (null (lookup-character-encoding keyword))))
        ;; look in other table maintained by preferences
        )))
    
         


  
;;; Return a list of :<NSS>tring<E>ncodings, sorted by the
;;; (localized) name of each encoding.
(defun supported-nsstring-encodings ()
  (ccl::collect ((ids))
    (let* ((ns-ids (#/availableStringEncodings ns:ns-string)))
      (unless (%null-ptr-p ns-ids)
        (do* ((i 0 (1+ i)))
             ()
          (let* ((id (paref ns-ids (:* :<NSS>tring<E>ncoding) i)))
            (if (zerop id)
              (return (sort (ids)
                            #'(lambda (x y)
                                (= #$NSOrderedAscending
                                   (#/localizedCompare:
                                    (nsstring-for-nsstring-encoding x)
                                    (nsstring-for-nsstring-encoding y))))))
              (when (supported-string-encoding-p id)              
                (ids id)))))))))





;;; TexEdit.app has support for allowing the encoding list in this
;;; popup to be customized (e.g., to suppress encodings that the
;;; user isn't interested in.)
(defmethod build-encodings-popup ((self hemlock-document-controller)
                                  &optional preferred-encoding)
  (declare (ignorable preferred-encoding))
  (let* ((id-list (supported-nsstring-encodings))
         (popup (make-instance 'ns:ns-pop-up-button)))
    ;;; Add a fake "Automatic" item with tag 0.
    (#/addItemWithTitle: popup #@"Automatic")
    (#/setTag: (#/itemAtIndex: popup 0) 0)
    (dolist (id id-list)
      (#/addItemWithTitle: popup (nsstring-for-nsstring-encoding id))
      (#/setTag: (#/lastItem popup) (nsstring-encoding-to-nsinteger id)))
    (#/selectItemWithTag: popup (if preferred-encoding (nsstring-encoding-to-nsinteger preferred-encoding) 0))
    (#/sizeToFit popup)
    popup))


(objc:defmethod (#/runModalOpenPanel:forTypes: :<NSI>nteger)
    ((self hemlock-document-controller) panel types)
  (let* (#-cocotron (popup (build-encodings-popup self #|preferred|#)))
    #-cocotron (#/setAccessoryView: panel popup)
    (let* ((result (call-next-method panel types)))
      (when (= result #$NSOKButton)
        #-cocotron
        (with-slots (last-encoding) self
          (setq last-encoding
                (nsinteger-to-nsstring-encoding (#/tag (#/selectedItem popup))))))
      result)))
  
(defun hemlock-ext:open-hemlock-buffer (&key (pathname :prompt))
  (assert (eq pathname :prompt)) ;; TODO: should handle pathname
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   (#/sharedDocumentController hemlock-document-controller)
   (@selector #/openDocument:) +null-ptr+ t))
  
(defun hemlock-ext:save-hemlock-buffer (buffer &key pathname copy)
  (let ((doc (hi::buffer-document buffer)))
    (cond (copy
           (assert (eq pathname :prompt)) ;; TODO: should handle pathname
           (save-hemlock-document-as doc))
          ((null pathname)
           (save-hemlock-document doc))
          (t
           (assert (eq pathname :prompt)) ;; TODO: should handle pathname
           (save-hemlock-document-to doc)))))

(defmethod save-hemlock-document ((self hemlock-editor-document))
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   self (@selector #/saveDocument:) +null-ptr+ t))

(defmethod save-hemlock-document-as ((self hemlock-editor-document))
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   self (@selector #/saveDocumentAs:) +null-ptr+ t))

(defmethod save-hemlock-document-to ((self hemlock-editor-document))
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   self (@selector #/saveDocumentTo:) +null-ptr+ t))




    

(defun initialize-user-interface ()
  ;; The first created instance of an NSDocumentController (or
  ;; subclass thereof) becomes the shared document controller.  So it
  ;; may look like we're dropping this instance on the floor, but
  ;; we're really not.
  (make-instance 'hemlock-document-controller)
  ;(#/sharedPanel lisp-preferences-panel)
  (make-editor-style-map))
  

;;; This needs to run on the main thread.  Sets the cocoa selection from the
;;; hemlock selection.
(defmethod update-hemlock-selection ((self hemlock-text-storage))
  (assume-cocoa-thread)
  (let* ((buffer (hemlock-buffer self)))
    (for-each-textview-using-storage
     self
     (lambda (tv)
       (with-view-selection-info (tv buffer)
         (multiple-value-bind (start end) (hi:buffer-selection-range buffer)
           #+debug
           (#_NSLog #@"update Hemlock selection: charpos = %d, abspos = %d"
                    :int (hi::mark-charpos (hi::buffer-point buffer)) :int start)
           (#/updateSelection:length:affinity: tv
                                               start
                                               (- end start)
                                               (if (eql start 0)
                                                 #$NSSelectionAffinityUpstream
                                                 #$NSSelectionAffinityDownstream))))))))

;; This should be invoked by any command that modifies the buffer, so it can show the
;; user what happened...  This ensures the Cocoa selection is made visible, so it
;; assumes the Cocoa selection has already been synchronized with the hemlock one.
(defmethod hemlock-ext:ensure-selection-visible ((view hi:hemlock-view))
  (let ((tv (text-pane-text-view (hi::hemlock-view-pane view))))
    (#/scrollRangeToVisible: tv (#/selectedRange tv))))

(defloadvar *general-pasteboard* nil)

(defun general-pasteboard ()
  (or *general-pasteboard*
      (setq *general-pasteboard*
            (#/retain (#/generalPasteboard ns:ns-pasteboard)))))

(defloadvar *string-pasteboard-types* ())

(defun string-pasteboard-types ()
  (or *string-pasteboard-types*
      (setq *string-pasteboard-types*
            (#/retain (#/arrayWithObject: ns:ns-array #&NSStringPboardType)))))


(objc:defmethod (#/stringToPasteBoard:  :void)
    ((self lisp-application) string)
  (let* ((pb (general-pasteboard)))
    (#/declareTypes:owner: pb (string-pasteboard-types) nil)
    (#/setString:forType: pb string #&NSStringPboardType)))
    
(defun hemlock-ext:string-to-clipboard (string)
  (when (> (length string) 0)
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     *nsapp* (@selector #/stringToPasteBoard:) (%make-nsstring string) t)))

#+cocotron
;;; Work around a byte-order bug that affects #/paste.
(defun maybe-byte-reverse-string (nsstring)
  (let* ((len (#/length nsstring))
         (maybe-reversed-count  0))
    (dotimes (i len)
      (when (not (logtest #xff (#/characterAtIndex: nsstring i)))
        (incf maybe-reversed-count)))
    (if (> maybe-reversed-count (ash len -1))
      (%stack-block ((chars (* 2 len)))
        (ns:with-ns-range (r 0 len)
          (#/getCharacters:range: nsstring chars r)
          (dotimes (i len)
            (declare (fixnum i))
            (let* ((j (+ i i)))
              (declare (fixnum j))
              (let* ((w (%get-unsigned-word chars j)))
                (setf (%get-unsigned-word chars j)
                      (dpb (ldb (byte 8 0) w)
                           (byte 8 8)
                           (ldb (byte 8 8) w))))))

            
          (#/autorelease
           (make-instance ns:ns-string
                          :with-characters chars
                          :length len))))
      nsstring)))
                        
                    
                                                            
;;; The default #/paste method seems to want to set the font to
;;; something ... inappropriate.  If we can figure out why it
;;; does that and persuade it not to, we wouldn't have to do
;;; this here.
;;; (It's likely to also be the case that Carbon applications
;;; terminate lines with #\Return when writing to the clipboard;
;;; we may need to continue to override this method in order to
;;; fix that.)
(objc:defmethod (#/paste: :void) ((self hemlock-textstorage-text-view) sender)
  (declare (ignorable sender))
  #+debug (#_NSLog #@"Paste: sender = %@" :id sender)
  (let* ((pb (general-pasteboard))
         (string (progn (#/types pb) (#/stringForType: pb #&NSStringPboardType))))
    #+debug (log-debug "   string = ~s" string)
    (unless (%null-ptr-p string)
      #+cocotron (setq string (maybe-byte-reverse-string string))
      (unless (zerop (ns:ns-range-length (#/rangeOfString: string *ns-cr-string*)))
        (setq string (make-instance 'ns:ns-mutable-string :with-string string))
        (#/replaceOccurrencesOfString:withString:options:range:
                string *ns-cr-string* *ns-lf-string* #$NSLiteralSearch (ns:make-ns-range 0 (#/length string))))
      (let* ((textstorage (#/textStorage self)))
        (unless (#/shouldChangeTextInRange:replacementString: self (#/selectedRange self) string)
          (#/setSelectedRange: self (ns:make-ns-range (#/length textstorage) 0)))
	(let* ((selectedrange (#/selectedRange self)))
          ;; We really should bracket the call to
          ;; #/repaceCharactersInRange:withString: here with calls
          ;; to #/beginEditing and #/endEditing, but our implementation
          ;; of #/replaceCharactersInRange:withString: calls code that
          ;; asserts that editing isn't in progress.  Once that's
          ;; fixed, this should be fixed as well.
          (#/beginEditing textstorage)
	  (#/replaceCharactersInRange:withString: textstorage selectedrange string)
          (#/endEditing textstorage)
          (update-hemlock-selection textstorage) )))))

(objc:defmethod (#/hyperSpecLookUp: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (with-string-under-cursor (self selection)
    (multiple-value-bind (symbol win) (find-symbol (nstring-upcase selection) "CL")
      (when win
        (lookup-hyperspec-symbol symbol self)))))

;; This is called by stuff that makes a window programmatically, e.g. m-. or grep.
;; But the Open and New menus invoke the cocoa fns below directly. So just changing
;; things here will not change how the menus create views.  Instead,f make changes to
;; the subfunctions invoked by the below, e.g. #/readFromURL or #/makeWindowControllers.
(defun find-or-make-hemlock-view (&optional pathname)
  (assume-cocoa-thread)
  (rlet ((perror :id +null-ptr+))
    (let* ((doc (if pathname
                  (#/openDocumentWithContentsOfURL:display:error:
                   (#/sharedDocumentController ns:ns-document-controller)
                   (pathname-to-url pathname)
                   #$YES
                   perror)
                  (let ((*last-document-created* nil))
                    (#/newDocument: 
                     (#/sharedDocumentController hemlock-document-controller)
                     +null-ptr+)
                    *last-document-created*))))
      #+debug (log-debug "created ~s" doc)
      (when (%null-ptr-p doc)
        (error "Couldn't open ~s: ~a" pathname
               (let ((error (pref perror :id)))
                 (if (%null-ptr-p error)
                   "unknown error encountered"
                   (lisp-string-from-nsstring (#/localizedDescription error))))))
      (front-view-for-buffer (hemlock-buffer doc)))))

;; Execute in cocoa thread in a dynamic context that allows hemlock buffer functions to work.
;; The function should not modify the buffer, since display will not be updated, for that go
;; through hi::handle-hemlock-event instead.
(defun execute-in-buffer (buffer thunk)
  (check-type buffer hi:buffer)
  (let ((emsg nil))
    (multiple-value-prog1
        (execute-in-gui (lambda ()
                         (block exit
                           (handler-bind ((error (lambda (cc)
                                                   (setq emsg
                                                         (with-standard-io-syntax
                                                             (or (ignore-errors (princ-to-string cc))
                                                                 "#<error printing error message>")))
                                                   (return-from exit))))
                             (let ((hi::*current-buffer* buffer))
                               (funcall thunk))))))
      (when emsg (error "~a" emsg)))))



(defun hemlock-ext:execute-in-file-view (pathname thunk)
  (execute-in-gui #'(lambda ()
                      (assume-cocoa-thread)
                      (handler-case
                          (let ((view (find-or-make-hemlock-view pathname)))
                            (hi::handle-hemlock-event view thunk))
                        (error (c)
                          (alert-window :title "Error in Hemlock command processing"
                                        :message (or (ignore-errors (princ-to-string c))
                                                     "#<error printing error message>")
                                        :default-button "Ok"))))))

;; Bring view to front.
(defun hemlock-ext:select-view (view)
  (execute-in-gui (lambda ()
                    (#/makeKeyAndOrderFront: (#/window (hi::hemlock-view-pane view)) (%null-ptr)))))

(defun hemlock-ext:open-sequence-dialog (&key title sequence action (printer #'prin1))
  (make-instance 'sequence-window-controller
    :title title
    :sequence sequence
    :result-callback action
    :display printer))

(objc:defmethod (#/documentClassForType: :<C>lass) ((self hemlock-document-controller)
						    type)
  (if (#/isEqualToString: type #@"html")
      display-document
      (call-next-method type)))
      

(objc:defmethod #/newDisplayDocumentWithTitle:content:
		((self hemlock-document-controller)
		 title
		 string)
  (assume-cocoa-thread)
  (let* ((doc #+cocotron (#/makeUntitledDocumentOfType: self #@"html")
              #-cocotron (#/makeUntitledDocumentOfType:error: self #@"html" +null-ptr+)))
    (unless (%null-ptr-p doc)
      (#/addDocument: self doc)
      (#/makeWindowControllers doc)
      (let* ((window (#/window (#/objectAtIndex: (#/windowControllers doc) 0))))
	(#/setTitle: window title)
	(let* ((tv (slot-value doc 'text-view))
	       (lm (#/layoutManager tv))
	       (ts (#/textStorage lm)))
	  (#/beginEditing ts)
	  (#/replaceCharactersInRange:withAttributedString:
	   ts
	   (ns:make-ns-range 0 (#/length ts))
	   string)
	  (#/endEditing ts))
	(#/makeKeyAndOrderFront: window self)))
    doc))

(defun hemlock-ext:revert-hemlock-buffer (buffer)
  (let* ((doc (hi::buffer-document buffer)))
    (when doc
      (#/performSelectorOnMainThread:withObject:waitUntilDone:
       doc
       (@selector #/revertDocumentToSaved:)
       +null-ptr+
       t))))

(defun hemlock-ext:raise-buffer-view (buffer &optional action)
  "Bring a window containing buffer to front and then execute action in
   the window.  Returns before operation completes."
  ;; Queue for after this event, so don't screw up current context.
  (queue-for-gui #'(lambda ()
                     (let ((doc (hi::buffer-document buffer)))
                       (unless (and doc (not (%null-ptr-p doc)))
                         (hi:editor-error "Deleted buffer: ~s" buffer))
                       (#/showWindows doc)
                       (when action
                         (hi::handle-hemlock-event (front-view-for-buffer buffer) action))))))

;;; Enable CL:ED
(defun cocoa-edit (&optional arg)
  (cond ((or (null arg)
             (typep arg 'string)
             (typep arg 'pathname))
         (when arg
           (unless (probe-file arg)
             (let ((lpath (merge-pathnames arg *.lisp-pathname*)))
               (when (probe-file lpath) (setq arg lpath)))))
         (execute-in-gui #'(lambda () (find-or-make-hemlock-view arg))))
        ((ccl::valid-function-name-p arg)
         (hemlock:edit-definition arg)
         nil)
        (t (report-bad-arg arg '(or null string pathname (satisfies ccl::valid-function-name-p))))))

(setq ccl::*resident-editor-hook* 'cocoa-edit)

#-cocotron
(defclass url-handler-command (ns:ns-script-command)
  ()
  (:documentation
   "Handles AppleEvents that send us URLs to open. Both logical pathnames
    ('ccl:lib;foo.lisp') and symbols (ccl::*current-process*) can be parsed as a URL
    with a scheme of 'ccl'. So, we accept those as URLs, and handle them appropriately.")
  (:metaclass ns:+ns-script-command))

#-cocotron
(objc:defmethod #/performDefaultImplementation ((self url-handler-command))
  (let* ((string (ccl::lisp-string-from-nsstring (#/directParameter self)))
         (symbol (let ((*read-eval* nil))
                   (handler-case (read-from-string string)
                     (error () nil)))))
    (if symbol
      (hemlock:edit-definition symbol)
      (execute-in-gui #'(lambda ()
                          (find-or-make-hemlock-view
                           (if (probe-file string)
                             string
                             (let ((lpath (merge-pathnames string *.lisp-pathname*)))
                               (when (probe-file lpath)
                                 lpath))))))))
  +null-ptr+)

