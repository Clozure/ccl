(in-package "GUI")

;;; NSMutableString subclass that uses a Hemlock buffer for
;;; character storage.

(defclass xhemlock-buffer-string (ns:ns-mutable-string)
  ((cache :initform (reset-buffer-cache
		     (make-buffer-cache :buffer (make-untitled-buffer)))
	  :initarg :cache :accessor hemlock-buffer-string-cache))
  (:metaclass ns:+ns-object))

(defmethod hemlock-buffer ((self xhemlock-buffer-string))
  (with-slots (cache) self
    (when cache
      (buffer-cache-buffer cache))))

(defvar *untitled-buffer-counter* 0)

(defun next-untitled-buffer-counter ()
  (ccl::atomic-incf *untitled-buffer-counter*))

(defun make-untitled-buffer ()
  (loop
    (let* ((name (format nil "untitled-~d" (next-untitled-buffer-counter)))
           (buffer (hi:make-buffer name)))
      (when buffer
        (return buffer)))))

(objc:defmethod (#/dealloc :void) ((self xhemlock-buffer-string))
  (let ((buffer (hemlock-buffer self)))
    (when buffer
      (when (eq buffer hi::*current-buffer*)
	(setf hi::*current-buffer* nil))
      (setf (hi::buffer-document buffer) nil)
      ;; It makes sense to me to delete the buffer here, but
      ;; the existing code does it in response to closing a document.
      ;;(hi::delete-buffer buffer)
      (setf (slot-value self 'cache) nil)
      (call-next-method))))

;;; NSMutableString primitive method

(objc:defmethod (#/replaceCharactersInRange:withString: :void)
                ((self xhemlock-buffer-string) (range #>NSRange) string)
  (let* ((buffer (hemlock-buffer self))
	 (cache (hemlock-buffer-string-cache self))
         (hi::*current-buffer* buffer)
         (position (pref range #>NSRange.location))
	 (length (pref range #>NSRange.length))
	 (lisp-string (if (> (#/length string) 0) (lisp-string-from-nsstring string))))
    (hi:with-mark ((m (hi:buffer-point buffer)))
      (move-hemlock-mark-to-absolute-position m cache position)
      (when (> length 0)
        (hi:delete-characters m length))
      (when lisp-string
        (hi:insert-string m lisp-string)))))

;;; NSString primitive methods

(objc:defmethod (#/length #>NSUInteger) ((self xhemlock-buffer-string))
  (let* ((cache (hemlock-buffer-string-cache self)))
    (or (buffer-cache-buflen cache)
        (setf (buffer-cache-buflen cache)
              (let* ((buffer (buffer-cache-buffer cache)))
		(hemlock-buffer-length buffer))))))

#+slow
(objc:defmethod (#/length #>NSUInteger) ((self xhemlock-buffer-string))
  (let* ((buffer (hemlock-buffer self))
	 (hi::*current-buffer* buffer))
    (hi:count-characters (hi:buffer-region buffer))))

(objc:defmethod (#/characterAtIndex: :unichar) ((self xhemlock-buffer-string)
						(index #>NSUInteger))
  (char-code (hemlock-char-at-index (hemlock-buffer-string-cache self) index)))

#+slow
(objc:defmethod (#/characterAtIndex: :unichar) ((self xhemlock-buffer-string) (index #>NSUInteger))
  (let* ((buffer (hemlock-buffer self))
         (hi::*current-buffer* buffer)
         (start (hi:buffer-start-mark buffer)))
    (hi:with-mark ((m start))
      (if (hi:character-offset m index)
	;; If the lisp character can't be represented as a 16-bit UTF-16
	;; code point (i.e., the character needs to be encoded with a surrogate
	;; pair), just punt and return the replacement character.  This is
	;; clearly not good for Gilgamesh (presumably a cuneiform user), among
	;; others. If we keep using the Cocoa text system, we'll have to hair
	;; things up to deal with this at some point.
	(let* ((char (or (hi:next-character m)
			 (error "index ~d out of range" index)))
	       (code (char-code char)))
	  (if (< code #x10000)
	    code
	    #\Replacement_Character))))))

(objc:defmethod (#/getCharacters:range: :void) ((self xhemlock-buffer-string)
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


;;; This is bound to T when we edit text using the methods of
;;; NSTextStorage.  These keeps the Hemlock text primitives from
;;; calling edited:range:changeInLength: on their own.
(defvar *suppress-edit-notifications* nil)

;;; NSTextStorage subclass that uses a HemlockBufferString for
;;; text storage, and for character attributes, too.

(defclass xhemlock-text-storage (ns:ns-text-storage)
  ((hemlock-string :foreign-type :id :reader hemlock-string)
   (edit-count :foreign-type :int)
   (selection-set-by-search :foreign-type #>BOOL))
  (:metaclass ns:+ns-object))

(defmethod (setf hemlock-string) (new (self xhemlock-text-storage))
  (with-slots (hemlock-string) self
    (unless (eql hemlock-string new)
      (#/release hemlock-string)
      (setf hemlock-string (#/retain new)))))

(objc:defmethod (#/dealloc :void) ((self xhemlock-text-storage))
  (setf (hemlock-string self) +null-ptr+)
  (call-next-method))

(objc:defmethod #/hemlockString ((self xhemlock-text-storage))
  (slot-value self 'hemlock-string))

(objc:defmethod (#/updateMirror :void) ((self xhemlock-text-storage))
  ;; don't need to do anything
  )

(defmethod hemlock-buffer ((self xhemlock-text-storage))
  (let ((string (hemlock-string self)))
    (unless (%null-ptr-p string)
      (hemlock-buffer string))))

(objc:defmethod #/initWithString: ((self xhemlock-text-storage) string)
  (setq string (%inc-ptr string 0)) ;avoid stack-consed macptr?
  (ccl::%call-next-objc-method self (find-class 'xhemlock-text-storage)
                               (@selector #/init) '(:id))
  (setf (slot-value self 'hemlock-string) (#/retain string))
  self)

(objc:defmethod #/init ((self xhemlock-text-storage))
  (#/initWithString: self (make-instance 'xhemlock-buffer-string)))

(objc:defmethod #/string ((self xhemlock-text-storage))
  (hemlock-string self))

(objc:defmethod (#/replaceCharactersInRange:withString: :void)
                ((self xhemlock-text-storage) (range #>NSRange) string)
  (let* ((orig-len (#/length self))
	 (contents (hemlock-string self))
	 (*suppress-edit-notifications* t))
    (#/replaceCharactersInRange:withString: contents range string)
    (#/edited:range:changeInLength: self #$NSTextStorageEditedCharacters
                                    range (- (#/length self) orig-len))))

(objc:defmethod (#/setAttributes:range: :void) ((self xhemlock-text-storage)
                                                (attributes :id)
                                                (range #>NSRange))
  (let* ((string (hemlock-string self))
	 (cache (hemlock-buffer-string-cache self))
	 (buffer (hemlock-buffer string))
         (hi::*current-buffer* buffer)
	 (*suppress-edit-notifications* t))
    (hi:with-mark ((start (hi:buffer-point buffer))
                   (end (hi:buffer-point buffer)))
      (move-hemlock-mark-to-absolute-position start cache
					      (ns:ns-range-location range))
      (move-hemlock-mark-to-absolute-position end cache
					      (+ (ns:ns-range-location range)
						 (ns:ns-range-length range)))
      (hi::set-region-charprops (hi:region start end) (dict-to-charprops attributes))))
  (#/edited:range:changeInLength: self #$NSTextStorageEditedAttributes
                                  range 0))

;;; This appears to be called at every blink of the insertion point.
(objc:defmethod #/attributesAtIndex:effectiveRange: ((self xhemlock-text-storage)
                                                     (location #>NSUInteger)
                                                     (rangeptr (* #>NSRange)))
  (let* ((buffer (hemlock-buffer (hemlock-string self)))
         (hi::*current-buffer* buffer))
    (hi:with-mark ((m (hi:buffer-point buffer)))
      (move-hemlock-mark-to-absolute-position m
					      (hemlock-buffer-string-cache
					       (hemlock-string self))
					      location)
      (multiple-value-bind (plist start end)
                           (hi::line-charprops-for-position (hi:mark-line m) (hi:mark-charpos m))
        (unless (%null-ptr-p rangeptr)
	  (let ((origin (hi::get-line-origin (hi:mark-line m))))
	    (incf start origin)
	    (incf end origin)
	    (setf (pref rangeptr #>NSRange.location) start
		  (pref rangeptr #>NSRange.length) (- end start))))
	;; This conses up a brand-new NSDictionary every time.
	;; Some sort of caching may be profitable here (or not...)
        (charprops-to-dict plist)))))

;;; Return true iff we're inside a "beginEditing/endEditing" pair
(objc:defmethod (#/editingInProgress :<BOOL>) ((self xhemlock-text-storage))
  ;; This is meaningless outside the event thread, since you can't tell what
  ;; other edit-count changes have already been queued up for execution on
  ;; the event thread before it gets to whatever you might queue up next.
  (assume-cocoa-thread)
  (> (slot-value self 'edit-count) 0))

(objc:defmethod (#/noteHemlockInsertionAtPosition:length: :void)
    ((self xhemlock-text-storage) (pos :<NSI>nteger) (n :<NSI>nteger)
     (extra :<NSI>nteger))
  (declare (ignore extra))
  (let* ((buffer (hemlock-buffer self))
	 (document (hi::buffer-document buffer))
	 (undo-mgr (and document (#/undoManager document))))
    (when (and undo-mgr (not (#/isUndoing undo-mgr)))
      (#/replaceCharactersInRange:withString:
       (#/prepareWithInvocationTarget: undo-mgr self)
       (ns:make-ns-range pos n) #@"")))
  (let ((cache (hemlock-buffer-string-cache (hemlock-string self))))
    (adjust-buffer-cache-for-insertion cache pos n)
    (update-line-cache-for-index cache pos))
  (unless *suppress-edit-notifications*
    (textstorage-note-insertion-at-position self pos n)))

(objc:defmethod (#/noteHemlockDeletionAtPosition:length: :void)
    ((self xhemlock-text-storage) (pos :<NSI>nteger) (n :<NSI>nteger)
     (extra :<NSI>nteger))
  (declare (ignorable extra))
  (let ((cache (hemlock-buffer-string-cache (hemlock-string self))))
    (reset-buffer-cache cache)
    (update-line-cache-for-index cache pos))
  (unless *suppress-edit-notifications*
    (ns:with-ns-range (range pos n)
      (#/edited:range:changeInLength: self
				      (logior #$NSTextStorageEditedCharacters
					      #$NSTextStorageEditedAttributes)
				      range (- n)))))

(objc:defmethod (#/noteHemlockModificationAtPosition:length: :void)
    ((self xhemlock-text-storage) (pos :<NSI>nteger) (n :<NSI>nteger)
     (extra :<NSI>nteger))
  (declare (ignorable extra))
  (unless *suppress-edit-notifications*
    (ns:with-ns-range (range pos n)
      (#/edited:range:changeInLength: self 
				      (logior #$NSTextStorageEditedCharacters
					      #$NSTextStorageEditedAttributes)
				      range 0))))

(objc:defmethod (#/noteHemlockAttrChangeAtPosition:length: :void)
    ((self xhemlock-text-storage) (pos :<NSI>nteger) (n :<NSI>nteger)
     (fontnum :<NSI>nteger))
  (declare (ignore fontnum))
  (unless *suppress-edit-notifications*
    (ns:with-ns-range (range pos n)
      (#/edited:range:changeInLength: self #$NSTextStorageEditedAttributes
				      range 0))))

(defmethod assume-not-editing ((ts xhemlock-text-storage))
  #+debug NIL (assert (eql (slot-value ts 'edit-count) 0)))

(defmethod update-hemlock-selection ((self xhemlock-text-storage))
  (assume-cocoa-thread)
  (let ((buffer (hemlock-buffer self)))
    (multiple-value-bind (start end) (hi:buffer-selection-range buffer)
      #+debug
      (#_NSLog #@"update Hemlock selection: charpos = %d, abspos = %d"
               :int (hi::mark-charpos (hi::buffer-point buffer)) :int start)
      (for-each-textview-using-storage
       self
       #'(lambda (tv)
           (#/updateSelection:length:affinity: tv
                                               start
                                               (- end start)
                                               (if (eql start 0)
                                                 #$NSSelectionAffinityUpstream
                                                 #$NSSelectionAffinityDownstream)))))))


;;; Tabs are going to be a problem.
(defloadvar *default-paragraph-style*
    (let* ((style (#/mutableCopy (#/defaultParagraphStyle ns:ns-paragraph-style)))
           (charwidth (nth-value 1 (size-of-char-in-font *editor-font*))))
      (#/setLineBreakMode: style #$NSLineBreakByCharWrapping)
      (#/setTabStops: style (#/array ns:ns-array))
      (#/setDefaultTabInterval: style (* *tab-width* charwidth))
      style))

(defun ns-color-to-charprop (color)
  (let ((color (#/colorUsingColorSpaceName: color #&NSCalibratedRGBColorSpace)))
    (rlet ((r #>CGFloat)
           (g #>CGFloat)
           (b #>CGFloat)
           (a #>CGFloat))
      (#/getRed:green:blue:alpha: color r g b a)
      (flet ((scale (f)
               (floor (* 255 f))))
        (let* ((rr (scale (pref r #>CGFloat)))
               (gg (scale (pref g #>CGFloat)))
               (bb (scale (pref b #>CGFloat))))
          (format nil "#~2,'0x~2,'0x~2,'0x" rr gg bb))))))

(defun dict-to-charprops (dict)
  (let ((enumerator (#/keyEnumerator dict))
        (plist nil))
    (loop
      (let ((key (#/nextObject enumerator)))
        (when (%null-ptr-p key)
          (return plist))
        (let ((value (#/objectForKey: dict key))
              (keyword (car (rassoc key hi::*cocoa-attributes* :test #'ns-string-equal))))
          (case keyword
            (:ns-font
             (let* ((font value)
                    (descriptor (#/fontDescriptor font))
                    (traits-mask (#/symbolicTraits descriptor))
                    (name (lisp-string-from-nsstring (#/familyName font)))
                    (size (cgfloat (#/pointSize font))))
               (setq plist (nconc plist (list :font-name name :font-size size)))
               (when (logtest traits-mask #$NSFontItalicTrait)
                 (setq plist (nconc plist (list :font-slant :italic))))
               (when (logtest traits-mask #$NSFontBoldTrait)
                 (setq plist (nconc plist (list :font-weight :bold))))
               (if (logtest traits-mask #$NSFontExpandedTrait)
                 (setq plist (nconc plist (list :font-width :exapnded)))
                 (if (logtest traits-mask #$NSFontCondensedTrait)
                   (setq plist (nconc plist (list :font-width :condensed)))))))
            (:ns-paragraph-style )
            (:ns-foreground-color
             (let* ((color value)
                    (color-string (ns-color-to-charprop color)))
               (setq plist (nconc plist (list :font-color color-string)))))
            (:ns-underline-style
             (let* ((style (#/intValue value))
                    (underline-keyword (cond ((= style #$NSUnderlineStyleSingle)
                                              :single)
                                             ((= style #$NSUnderlineStyleDouble)
                                              :double)
                                             ((= style #$NSUnderlineStyleThick)
                                              :thick))))
               (when underline-keyword
                 (setq plist (nconc plist (list :font-underline underline-keyword))))))
            (:ns-superscript )
            (:ns-background-color 
             (let* ((color value)
                    (color-string (ns-color-to-charprop color)))
               (setq plist (nconc plist (list :background-color color-string)))))
            (:ns-attachment (format t "~s" keyword))
            (:ns-ligature (format t "~s" keyword))
            (:ns-baseline-offset (format t "~s" keyword))
            (:ns-kern (format t "~s" keyword))
            (:ns-link (format t "~s" keyword))
            (:ns-stroke-width (format t "~s" keyword))
            (:ns-stroke-color (format t "~s" keyword))
            (:ns-underline-color (format t "~s" keyword))
            (:ns-strikethrough-style (format t "~s" keyword))
            (:ns-strikethrough-color (format t "~s" keyword))
            (:ns-shadow (format t "~s" keyword))
            (:ns-obliqueness (format t "~s" keyword))
            (:ns-expansion (format t "~s" keyword))
            (:ns-cursor (format t "~s" keyword))
            (:ns-tool-tip (format t "~s" keyword))
            (:ns-character-shap (format t "~s" keyword))
            (:ns-glyph-info (format t "~s" keyword))))))))

(defun charprops-to-dict (plist)
  (when (null plist)
    (return-from charprops-to-dict
                 (#/dictionaryWithObjectsAndKeys: ns:ns-dictionary
						  *default-paragraph-style*
						  #&NSParagraphStyleAttributeName
						  *editor-font*
						  #&NSFontAttributeName
						  +null-ptr+)))
  (let* ((dict (#/dictionaryWithCapacity: ns:ns-mutable-dictionary 8))
         (default-font *editor-font*)	;what about listeners?
         (fm (#/sharedFontManager ns:ns-font-manager))
         (traits 0)
         (font +null-ptr+)
         (font-name nil))
    (#/setObject:forKey: dict *default-paragraph-style*
                         #&NSParagraphStyleAttributeName)
    (setq font-name (getf plist :font-name))
    (when font-name
      (case font-name
        (:document-font (setq font (#/userFontOfSize: ns:ns-font 0.0)))
        (:fixed-font (setq font (#/userFixedPitchFontOfSize: ns:ns-font 0.0)))
        (:system-font (setq font (#/systemFontOfSize: ns:ns-font 0.0)))
        (t (setq font (#/fontWithName:size: ns:ns-font
                                            (#/autorelease (%make-nsstring font-name))
                                            0.0)))))
    (when (%null-ptr-p font)
      (setq font default-font))
    (loop for (k v) on plist by #'cddr
      do (case k
           (:font-size (setq v (float v ns:+cgfloat-zero+))
                       (setq font (#/convertFont:toSize: fm font v)))
           (:font-weight (cond ((eq v :bold)
                                (setq traits (logior traits #$NSBoldFontMask)))
                               ((eq v :plain)
                                (setq traits (logior traits #$NSUnboldFontMask)))))
           (:font-width (cond ((eq v :condensed)
                               (setq traits (logior traits #$NSCondensedFontMask)))
                              ((eq v :expanded)
                               (setq traits (logior traits #$NSExpandedFontMask)))))
           (:font-slant (cond ((eq v :italic)
                               (setq traits (logior traits #$NSItalicFontMask)))
                              ((eq v :roman)
                               (setq traits (logior traits #$NSUnitalicFontMask)))))
           (:font-underline (let (n)
                              (case v
                                (:single
                                 (setq n (#/numberWithInt: ns:ns-number #$NSUnderlineStyleSingle)))
                                (:double
                                 (setq n (#/numberWithInt: ns:ns-number #$NSUnderlineStyleDouble)))
                                (:thick
                                 (setq n (#/numberWithInt: ns:ns-number #$NSUnderlineStyleThick))))
                              (when n
                                (#/setObject:forKey: dict n #&NSUnderlineStyleAttributeName))))
           (:font-color)
           (:background-color)))
    (setq font (#/convertFont:toHaveTrait: fm font traits))
    (unless (%null-ptr-p font)
      (#/setObject:forKey: dict font #&NSFontAttributeName))
    dict))

(defclass xhemlock-text-view (ns:ns-text-view)
  ()
  (:metaclass ns:+ns-object))

;;; replaces version in cocoa-editor.lisp

(defun make-textstorage-for-hemlock-buffer (buffer)
  (make-instance 'xhemlock-text-storage
                 :with-string
                 (make-instance
                  'xhemlock-buffer-string
                  :cache
                  (reset-buffer-cache
                   (make-buffer-cache)
                   buffer))))