;; -----------------------------------------------------------------------
;; hemlock-commands.lisp
;; -----------------------------------------------------------------------
;; Copyright © 2014 Clozure Associates, All rights reserved.
;; -----------------------------------------------------------------------
;; authors:
;; R. Matthew Emerson <rme@clozure.com>
;;
;; -----------------------------------------------------------------------
;; created: 01/03/2012
;; -----------------------------------------------------------------------

(in-package "GUI")

;;; TODO: Put MAKE-STANDARD-WINDOW, %SCROLL-VIEW, and
;;; %SCROLL-VIEW-WITH-TEXT-VIEW into a more appropriate location.

(defun make-standard-window (x y w h)
  (ns:with-ns-rect (rect x y w h)
    (let* ((style-mask (logior #$NSTitledWindowMask
                               #$NSClosableWindowMask
                               #$NSMiniaturizableWindowMask
                               #$NSResizableWindowMask))
           (window (#/initWithContentRect:styleMask:backing:defer:
                    (#/alloc ns:ns-window)
                    rect
                    style-mask
                    #$NSBackingStoreBuffered
                    #$YES)))
      window)))

(defun %scroll-view (frame)
  (let ((sv (#/initWithFrame: (#/alloc ns:ns-scroll-view) frame)))
    (#/setBorderType: sv #$NSNoBorder)
    (#/setHasVerticalScroller: sv t)
    (#/setAutohidesScrollers: sv t)
    (#/setAutoresizingMask: sv (logior #$NSViewWidthSizable
                                       #$NSViewHeightSizable))
    sv))

(defun %scroll-view-with-text-view (frame)
  (let* ((sv (%scroll-view frame))
         (text-rect (ns:make-ns-rect 0 0 (ns:ns-rect-width frame)
                                     (ns:ns-rect-height frame)))
         (tv (#/initWithFrame: (#/alloc ns:ns-text-view) text-rect)))
    ;; configure text view
    (ns:with-ns-size (s 0 (ns:ns-rect-width text-rect))
      (#/setMinSize: tv s))
    (ns:with-ns-size (s #$FLT_MAX #$FLT_MAX)
      (#/setMaxSize: tv s))
    (#/setVerticallyResizable: tv t)
    (#/setHorizontallyResizable: tv nil)
    (#/setAutoresizingMask: tv #$NSViewWidthSizable)
    (let ((container (#/textContainer tv)))
      (ns:with-ns-size (s (ns:ns-rect-width text-rect) #$FLT_MAX)
        (#/setContainerSize: container s))
      (#/setWidthTracksTextView: container t))
    (#/setDocumentView: sv tv)
    (#/release tv)
    sv))

(defun %hemlock-commands-table-view (frame)
  (let* ((sv (#/initWithFrame: (#/alloc ns:ns-scroll-view) frame))
         (tv-frame (ns:make-ns-rect 0 0 (ns:ns-rect-width frame)
                                   (ns:ns-rect-height frame)))
         (tv (#/initWithFrame: (#/alloc ns:ns-table-view) tv-frame))
         (col1 (#/initWithIdentifier: (#/alloc ns:ns-table-column) #@"name"))
         (col2 (#/initWithIdentifier: (#/alloc ns:ns-table-column) #@"key")))
    (#/setHasVerticalScroller: sv t)
    (#/setAutohidesScrollers: sv t)
    (#/setAutoresizingMask: sv (logior #$NSViewWidthSizable
                                       #$NSViewHeightSizable))
    (#/setColumnAutoresizingStyle: tv #$NSTableViewReverseSequentialColumnAutoresizingStyle)
    (#/setEditable: col1 nil)
    (#/setMinWidth: col1 100d0)
    (#/setWidth: col1 240d0)
    (#/setStringValue: (#/headerCell col1) #@"Command")
    (#/addTableColumn: tv col1)
    (#/release col1)
    (#/setEditable: col2 nil)
    (#/setMinWidth: col2 80d0)
    (#/setStringValue: (#/headerCell col2) #@"Key")
    (#/addTableColumn: tv col2)
    (#/release col2)
    (#/setFocusRingType: tv #$NSFocusRingTypeNone)
    (#/setAutoresizingMask: tv (logior #$NSViewWidthSizable
                                       #$NSViewHeightSizable))
    (#/sizeLastColumnToFit tv)
    (#/setDocumentView: sv tv)
    (#/release tv)
    sv))

(defconstant $hemlock-commands-table-view-tag 100)
(defconstant $hemlock-commands-search-field-tag 101)

(defun %hemlock-commands-view (frame)
  (let ((width (ns:ns-rect-width frame))
        (height (ns:ns-rect-height frame)))
    (cg:with-rects ((search-field-frame 20 (- height 37) (- width 40) 22)
                    (split-view-frame 0 0 width (- height 47))
                    (table-frame 0 0 330 174) ;these rects don't matter much
                    (text-frame 0 0 330 118)) ;the split view will resize them
      (let* ((view (#/initWithFrame: (#/alloc ns:ns-view) frame))
             (search-field (#/initWithFrame: (#/alloc ns:ns-search-field) search-field-frame))
             (split-view (#/initWithFrame: (#/alloc ns:ns-split-view) split-view-frame))
             (scroll-view (%hemlock-commands-table-view table-frame))
             (documentation-view (%scroll-view-with-text-view text-frame))
             (text-view (#/documentView documentation-view)))
        (#/setEditable: text-view nil)
        (#/setTag: (#/documentView scroll-view) $hemlock-commands-table-view-tag)
        (#/setTag: search-field $hemlock-commands-search-field-tag)
        (#/setAutoresizingMask: view (logior #$NSViewWidthSizable
                                             #$NSViewHeightSizable))
        (#/setAutoresizingMask: search-field (logior #$NSViewWidthSizable
                                                     #$NSViewMinYMargin))
        (#/addSubview: view search-field)
        (#/release search-field)
        (#/setDividerStyle: split-view #$NSSplitViewDividerStyleThin)
        (#/addSubview: split-view scroll-view)
        (#/release scroll-view)
        (#/addSubview: split-view documentation-view)
        (#/release documentation-view)
        (#/setAutoresizingMask: split-view (logior #$NSViewWidthSizable
                                                   #$NSViewHeightSizable))
        (#/addSubview: view split-view)
        (#/release split-view)
        (values view text-view)))))

(defclass hemlock-commands-item (ns:ns-object)
  ((name :foreign-type :id)
   (key :foreign-type :id)
   (info :initarg :info :accessor hemlock-commands-item-info :initform nil))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/dealloc :void) ((self hemlock-commands-item))
  (objc:remove-lisp-slots self)
  (#/release (slot-value self 'name))
  (#/release (slot-value self 'key))
  (call-next-method))

(defun %make-hemlock-command-item (info)
  (let ((item (make-instance 'hemlock-commands-item :info info))
        (command (car info))
        (keys (cdr info)))
    (with-slots (name key) item
      (with-cfstring (s (hi:command-name command))
        (setf (slot-value item 'name) (#/copy s)))
      (let ((key-string (format nil "~{~a~^ or ~}" (mapcar 'hi:pretty-key-string keys))))
        (with-cfstring (s key-string)
          (setf (slot-value item 'key) (#/copy s)))))
    item))

(defclass hemlock-commands-window-controller (ns:ns-window-controller)
  ((items :foreign-type :id)
   (search-field :foreign-type :id)
   (table-view :foreign-type :id)
   (text-view :foreign-type :id)
   (array-controller :foreign-type :id))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/dealloc :void) ((self hemlock-commands-window-controller))
  (#/release (slot-value self 'array-controller))
  (#/release (slot-value self 'items))
  (call-next-method))

(objc:defmethod #/init ((self hemlock-commands-window-controller))
  (let* ((window (make-standard-window 0 0 640 480))
         (new (call-next-method)))
    (unless (%null-ptr-p new)
      (#/setWindow: new window)
      (#/setTitle: window #@"Editor Commands")
      (#/setFrameAutosaveName: window #@"Editor Commands")
      (multiple-value-bind (view text-view)
                           (%hemlock-commands-view (#/frame (#/contentView window)))
        (let* ((table-view (#/viewWithTag: view $hemlock-commands-table-view-tag))
               (search-field (#/viewWithTag: view $hemlock-commands-search-field-tag)))
          (setf (slot-value new 'text-view) text-view)
          (setf (slot-value new 'table-view) table-view)
          (setf (slot-value new 'search-field) search-field)
          (#/addSubview: (#/contentView window) view)
          (#/release view)))
      (let ((info (hi:commands-and-bindings)))
        (with-slots (items) new
          (setq items (#/new ns:ns-mutable-array))
          (dolist (i info)
            (let ((item (%make-hemlock-command-item i)))
              (#/addObject: items item)
              (#/release item)))))
      (with-slots (table-view array-controller search-field) new
	(#/setDelegate: table-view new)
        (setq array-controller (#/initWithContent: (#/alloc ns:ns-array-controller)
                                                   (slot-value new 'items)))
        (let ((name-column (#/tableColumnWithIdentifier: table-view #@"name"))
              (key-column (#/tableColumnWithIdentifier: table-view #@"key")))
          (#/bind:toObject:withKeyPath:options: name-column #@"value"
                                                array-controller
                                                #@"arrangedObjects.name"
                                                +null-ptr+)
          (#/bind:toObject:withKeyPath:options: key-column #@"value"
                                                array-controller
                                                #@"arrangedObjects.key"
                                                +null-ptr+)
          (let ((options (#/dictionaryWithObjectsAndKeys: ns:ns-dictionary
                                                          #@"(name contains[c] $value) OR (key contains[c] $value)"
                                                          #&NSPredicateFormatBindingOption
                                                          +null-ptr+)))
            (#/bind:toObject:withKeyPath:options: search-field #@"predicate"
                                                  array-controller
                                                  #@"filterPredicate"
                                                  options)))))
    new))

(defun collapse-whitespace-for-documentation (string)
  (with-output-to-string (s)
    (labels ((in-whitespace (char)
               (case char
                 ((#\Space #\Newline #\Tab #\Return #\Page)
                  #'in-whitespace)
                 (t
                  (in-printable char))))
             (in-printable (char)
               (case char
                 ((#\Space #\Newline #\Tab #\Return #\Page)
                  (write-char #\Space s)
                  #'in-whitespace)
                 (t
                  (write-char char s)
                  #'in-printable))))
      (let ((state #'in-printable))
        (loop for char across string
              do (setf state (funcall state char)))))))
                 
(objc:defmethod (#/tableViewSelectionDidChange: :void) ((self hemlock-commands-window-controller)
                                                        notification)
  (declare (ignore notification))
  (with-slots (array-controller text-view) self
    (let ((selected-row (#/selectionIndex array-controller)))
      (if (= selected-row #$NSNotFound)
        (#/setString: (#/mutableString (#/textStorage text-view)) #@"")
        (let* ((item (#/objectAtIndex: (#/arrangedObjects array-controller) selected-row))
               (info (hemlock-commands-item-info item))
               (command (car info))
               (documentation (hi:command-documentation command)))
          (setq documentation (collapse-whitespace-for-documentation documentation))
          (with-cfstring (s documentation)
            (let* ((attrs (#/dictionaryWithObject:forKey: ns:ns-dictionary
                                                          (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font))
                                                          #&NSFontAttributeName))
                   (as (#/initWithString:attributes: (#/alloc ns:ns-mutable-attributed-string)
                                                     s
                                                     attrs))
                   (text-storage (#/textStorage text-view)))
              (#/setAttributedString: text-storage as)
	      (#/release as))))))))
