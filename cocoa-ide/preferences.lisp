;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates
;;;
;;; How to add a new preference pane:
;;;
;;; 1. Open preferences.nib with IB.  Drag a Custom View instance from
;;;    the palette.  Use the inpector to set its class to PreferencesView.
;;; 2. Inspect File's Owner (which represents an instance of
;;;    PreferencesWindowController).  Add an outlet for the new
;;;    preferences view you just made.  Hook up the outlet.  You can
;;;    add actions here too, if your preferences view will need them.
;;; 3. Add controls to your view, binding them to the defaults controller.
;;; 4. Save the nib file.
;;; 5. In preferences.lisp (this file), edit the defclass form for
;;;    preferences-window-controller and add a slot that matches the outlet
;;;    you created in step 2.
;;; 6. Edit the toolbar delegate methods to add a toolbar item for your
;;;    new preference view.
;;; 7. Implement a #/showFooPrefs: method to swap in the view when
;;;    the toolbar item is clicked.  (See #/showGeneralPrefs: for an
;;;    example.
;;; 8. Implement actions, if needed.


(in-package "GUI")

;;; A view that keeps track of its initial size.
(defclass preferences-view (ns:ns-view)
  ((width :accessor width)
   (height :accessor height))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/awakeFromNib :void) ((self preferences-view))
  (let* ((frame (#/frame self)))
    (setf (width self) (ns-width frame)
	  (height self) (ns-height frame))))

(defclass font-to-name-transformer (ns:ns-value-transformer)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/transformedValueClass :<C>lass)
    ((self +font-to-name-transformer))
  ns:ns-string)

(objc:defmethod (#/allowsReverseTransformation :<BOOL>)
    ((self +font-to-name-transformer))
  nil)

;;; Produce description of NSFont object, e.g., "Monaco 10"
(objc:defmethod #/transformedValue: ((self font-to-name-transformer) value)
  (let* ((font (#/unarchiveObjectWithData: ns:ns-unarchiver value))
         (name (#/displayName font))
         (size (float (#/pointSize font) 0.0d0)))
    (#/stringWithFormat: ns:ns-string #@"%@ %.0f" :id name :double-float size)))

(defclass preferences-window-controller (ns:ns-window-controller)
  ((appearance-prefs :foreign-type :id :accessor appearance-prefs)
   (documentation-prefs :foreign-type :id :accessor documentation-prefs)
   (editor-tab-view-item :foreign-type :id :accessor editor-tab-view-item)
   (encodings-prefs :foreign-type :id :accessor encodings-prefs)
   (general-prefs :foreign-type :id :accessor general-prefs)
   (hyperspec-path-button :foreign-type :id :accessor hyperspec-path-button)
   (listener-tab-view-item :foreign-type :id :accessor listener-tab-view-item)
   (swank-listener-port :foreign-type :id :accessor swank-listener-port)
   (tab-view :foreign-type :id :accessor tab-view)
   (toolbar :foreign-type :id :accessor toolbar))
  (:metaclass ns:+ns-object))

(objc:defmethod #/init ((self preferences-window-controller))
  (#/setValueTransformer:forName: ns:ns-value-transformer
				  (make-instance 'font-to-name-transformer)
				  #@"FontToName")

  (#/initWithWindowNibName: self #@"preferences")
  (#/addObserver:selector:name:object: (#/defaultCenter ns:ns-notification-center)
				       self
				       (@selector #/defaultsDidChange:)
				       #&NSUserDefaultsDidChangeNotification
				       (#/standardUserDefaults ns:ns-user-defaults))

  self)

(eval-when (:compile-toplevel :load-toplevel :execute)
    (require :swank-listener))

(objc:defmethod (#/windowDidLoad :void) ((self preferences-window-controller))
  (let* ((window (#/window self))
         (port-field (swank-listener-port self))
         (listener-port (or (preference-swank-listener-port) *default-swank-listener-port*)))
    (with-slots (toolbar) self
      (setf toolbar (make-instance 'ns:ns-toolbar
				   :with-identifier #@"preferences-window-toolbar"))
      (#/setDelegate: toolbar self)
      (#/setSelectedItemIdentifier: toolbar #@"appearance")
      (#/setToolbar: window toolbar)
      ;; for some reason, setting this in IB doesn't work on Tiger/PPC32
      (#/setShowsToolbarButton: window nil)
      (#/release toolbar))
    (ccl::with-autoreleased-nsstring (port-string (format nil "~A" (or listener-port "")))
      (#/setStringValue: port-field port-string))
    (#/showAppearancePrefs: self +null-ptr+)))
  
(objc:defmethod (#/showWindow: :void) ((self preferences-window-controller)
				       sender)
  (#/center (#/window self))
  (call-next-method sender))

(objc:defmethod (#/defaultsDidChange: :void) ((self preferences-window-controller)
					      notification)
  (declare (ignore notification))
  (update-cocoa-defaults))

(defconstant editor-font-button-tag 1)
(defconstant listener-input-font-button-tag 2)
(defconstant listener-output-font-button-tag 2)

;;; Ugh.
(defvar *listener-or-editor* nil)

(objc:defmethod (#/showFontPanel: :void) ((self preferences-window-controller)
					 sender)
  (let* ((tag (#/tag sender))
	 (font-manager (#/sharedFontManager ns:ns-font-manager))
	 (font nil)
	 (panel (#/window self)))
    (ecase tag
      (1
       (setq font *editor-font*)
       (setq *listener-or-editor* :editor))
      (2
       (setq font *listener-input-font*)
       (setq *listener-or-editor* :listener-input))
      (3
       (setq font *listener-output-font*)
       (setq *listener-or-editor* :listener-output)))
    (#/makeFirstResponder: panel panel)
    (#/setSelectedFont:isMultiple: font-manager font nil)
    (#/orderFrontFontPanel: font-manager self)))

(objc:defmethod (#/startSwankListener: :void) ((self preferences-window-controller)
					 sender)
  (declare (ignore sender))
  (unless (or *ccl-swank-active-p* 
              (try-starting-swank :force t))
    (alert-window :message "Unable to start the Swank server.")))

;;; This message is sent to the first responder, which is why
;;; we do the *listener-or-editor* thing.
(objc:defmethod (#/changeFont: :void) ((self preferences-window-controller)
					    font-manager)
  (let* ((defaults (#/standardUserDefaults ns:ns-user-defaults))
	 (data nil)
	 (font nil))
    (ecase *listener-or-editor*
      (:listener-input
       (setq font (#/convertFont: font-manager *listener-input-font*))
       (unless (%null-ptr-p font)
	 (setq data (#/archivedDataWithRootObject: ns:ns-archiver font))
	 (#/setObject:forKey: defaults data #@"listenerInputFont")))
      (:listener-output
       (setq font (#/convertFont: font-manager *listener-output-font*))
       (unless (%null-ptr-p font)
	 (setq data (#/archivedDataWithRootObject: ns:ns-archiver font))
	 (#/setObject:forKey: defaults data #@"listenerOutputFont")))
      (:editor
       (setq font (#/convertFont: font-manager *editor-font*))
       (unless (%null-ptr-p font)
	 (setq data (#/archivedDataWithRootObject: ns:ns-archiver font))
	 (#/setObject:forKey: defaults data #@"editorFont"))))))

;;; toolbar delegate methods

(objc:defmethod #/toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:
		((self preferences-window-controller)
		 toolbar itemIdentifier (flag :<BOOL>))
  (declare (ignore toolbar))
  (let ((item +null-ptr+))
    (cond
     ((#/isEqualToString: itemIdentifier #@"general")
      (setf item (make-instance 'ns:ns-toolbar-item
				:with-item-identifier itemIdentifier))
      (#/setLabel: item #@"General")
      (#/setImage: item (#/imageNamed: ns:ns-image #@"General"))
      (#/setTarget: item self)
      (#/setAction: item (@selector #/showGeneralPrefs:)))
     ((#/isEqualToString: itemIdentifier #@"appearance")
      (setf item (make-instance 'ns:ns-toolbar-item
				:with-item-identifier itemIdentifier))
      (#/setLabel: item #@"Appearance")
      (#/setImage: item (#/imageNamed: ns:ns-image #@"Appearance"))
      (#/setTarget: item self)
      (#/setAction: item (@selector #/showAppearancePrefs:)))
     ((#/isEqualToString: itemIdentifier #@"documentation")
      (setf item (make-instance 'ns:ns-toolbar-item
				:with-item-identifier itemIdentifier))
      (#/setLabel: item #@"Documentation")
      (#/setImage: item (#/imageNamed: ns:ns-image #@"Documentation"))
      (#/setTarget: item self)
      (#/setAction: item (@selector #/showDocumentationPrefs:)))
     ((#/isEqualToString: itemIdentifier #@"encodings")
      (setf item (make-instance 'ns:ns-toolbar-item
				:with-item-identifier itemIdentifier))
      (#/setLabel: item #@"Encodings")
      (#/setImage: item (#/imageNamed: ns:ns-image #@"Encodings"))
      (#/setTarget: item self)
      (#/setAction: item (@selector #/showEncodingsPrefs:))))
    (#/autorelease item)))

(objc:defmethod #/toolbarDefaultItemIdentifiers:
		((self preferences-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/arrayWithObjects: ns:ns-array #@"general"
		       #@"appearance"
		       #@"documentation"
		       #@"encodings"
		       +null-ptr+)) ; don't even think about putting nil here

(objc:defmethod #/toolbarAllowedItemIdentifiers:
		((self preferences-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/arrayWithObjects: ns:ns-array #@"general"
		       #@"appearance"
		       #@"documentation"
		       #@"encodings"
		       +null-ptr+))

(objc:defmethod #/toolbarSelectableItemIdentifiers:
		((self preferences-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/arrayWithObjects: ns:ns-array #@"general"
		       #@"appearance"
		       #@"documentation"
		       #@"encodings"
		       +null-ptr+))

(defun switch-content-view (window view)
  (#/setContentView: window view)
  (let* ((frame (#/frame window))
	 (min-size (#/minSize window))
	 (new-frame nil)
	 (content-rect (#/contentRectForFrameRect: window frame))
	 (dy (- (height view)
		(ns-height content-rect))))
    (decf (ns:ns-rect-y content-rect) dy)
    (incf (ns:ns-rect-height content-rect) dy)
    (setf (ns:ns-rect-width content-rect) (max (width view)
					       (ns:ns-size-width min-size)))
    (setq new-frame (#/frameRectForContentRect: window content-rect))
    (#/setFrame:display:animate: window new-frame t t)))

;;; toolbar actions

(objc:defmethod (#/showGeneralPrefs: :void) ((self preferences-window-controller)
						sender)
  (declare (ignore sender))
  (#/setTitle: (#/window self) #@"General")
  (switch-content-view (#/window self) (general-prefs self)))

(objc:defmethod (#/showAppearancePrefs: :void) ((self preferences-window-controller)
						sender)
  (declare (ignore sender))
  (#/setTitle: (#/window self) #@"Appearance")
  (switch-content-view (#/window self) (appearance-prefs self)))

(objc:defmethod (#/showDocumentationPrefs: :void) ((self preferences-window-controller)
						sender)
  (declare (ignore sender))
  (#/setTitle: (#/window self) #@"Documentation")
  (switch-content-view (#/window self) (documentation-prefs self)))

(objc:defmethod (#/showEncodingsPrefs: :void) ((self preferences-window-controller)
						sender)
  (declare (ignore sender))
  (#/setTitle: (#/window self) #@"Encodings")
  (switch-content-view (#/window self) (encodings-prefs self)))
