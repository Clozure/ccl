;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

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
  (let* ((font (#/unarchiveObjectWithData: ns:ns-keyed-unarchiver value))
         (name (#/displayName font))
         (size (float (#/pointSize font) 0.0d0)))
    (#/stringWithFormat: ns:ns-string #@"%@ %.0f" :id name :double-float size)))

(defclass ccl-preferences-window-controller (ns:ns-window-controller)
  ((general-view :foreign-type :id)
   (appearance-view :foreign-type :id))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/dealloc :void) ((self ccl-preferences-window-controller))
  (#/release (slot-value self 'general-view))
  (#/release (slot-value self 'appearance-view))
  (call-next-method))

(objc:defmethod #/init ((self ccl-preferences-window-controller))
  (#/setValueTransformer:forName: ns:ns-value-transformer
				  (make-instance 'font-to-name-transformer)
				  #@"FontToName")
  (setf (slot-value self 'general-view) (%preferences-general-view)
        (slot-value self 'appearance-view) (%preferences-appearance-view))
  (let* ((window (%preferences-panel))
         (toolbar (#/initWithIdentifier: (#/alloc ns:ns-toolbar)
                                         #@"ccl-preferences")))
    (#/initWithWindow: self window)
    (#/release window)
    (#/setDelegate: toolbar self)
    (#/setToolbar: window toolbar)
    (#/release toolbar)
    (#/setSelectedItemIdentifier: toolbar #@"appearance")
    (#/showAppearancePrefs: self +null-ptr+))
  (#/addObserver:selector:name:object: (#/defaultCenter ns:ns-notification-center)
				       self
				       (@selector #/defaultsDidChange:)
				       #&NSUserDefaultsDidChangeNotification
				       (#/standardUserDefaults ns:ns-user-defaults))

  self)

(objc:defmethod (#/showWindow: :void) ((self ccl-preferences-window-controller)
				       sender)
  (#/center (#/window self))
  (call-next-method sender))

(objc:defmethod (#/defaultsDidChange: :void) ((self ccl-preferences-window-controller)
					      notification)
  (declare (ignore notification))
  (update-cocoa-defaults))

(defconstant editor-font-button-tag 1)
(defconstant listener-input-font-button-tag 2)
(defconstant listener-output-font-button-tag 2)

;;; Ugh.
(defvar *listener-or-editor* nil)

(objc:defmethod (#/showFontPanel: :void) ((self ccl-preferences-window-controller)
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

;;; This message is sent to the first responder, which is why
;;; we do the *listener-or-editor* thing.
(objc:defmethod (#/changeFont: :void) ((self ccl-preferences-window-controller)
					    font-manager)
  (let* ((defaults (#/standardUserDefaults ns:ns-user-defaults))
	 (data nil)
	 (font nil))
    (ecase *listener-or-editor*
      (:listener-input
       (setq font (#/convertFont: font-manager *listener-input-font*))
       (unless (%null-ptr-p font)
	 (setq data (#/archivedDataWithRootObject: ns:ns-keyed-archiver font))
	 (#/setObject:forKey: defaults data #@"listenerInputFont")))
      (:listener-output
       (setq font (#/convertFont: font-manager *listener-output-font*))
       (unless (%null-ptr-p font)
	 (setq data (#/archivedDataWithRootObject: ns:ns-keyed-archiver font))
	 (#/setObject:forKey: defaults data #@"listenerOutputFont")))
      (:editor
       (setq font (#/convertFont: font-manager *editor-font*))
       (unless (%null-ptr-p font)
	 (setq data (#/archivedDataWithRootObject: ns:ns-keyed-archiver font))
	 (#/setObject:forKey: defaults data #@"editorFont"))))))

;;; toolbar delegate methods

(objc:defmethod #/toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:
		((self ccl-preferences-window-controller)
		 toolbar itemIdentifier (flag :<BOOL>))
  (declare (ignore toolbar))
  (let ((item +null-ptr+))
    (cond
     ((#/isEqualToString: itemIdentifier #@"general")
      (setf item (make-instance 'ns:ns-toolbar-item
				:with-item-identifier itemIdentifier))
      (#/setLabel: item #@"General")
      (#/setImage: item (#/imageNamed: ns:ns-image
                                       #&NSImageNamePreferencesGeneral))
      (#/setTarget: item self)
      (#/setAction: item (@selector #/showGeneralPrefs:)))
     ((#/isEqualToString: itemIdentifier #@"appearance")
      (setf item (make-instance 'ns:ns-toolbar-item
				:with-item-identifier itemIdentifier))
      (#/setLabel: item #@"Appearance")
      (#/setImage: item (#/imageNamed: ns:ns-image #@"Appearance"))
      (#/setTarget: item self)
      (#/setAction: item (@selector #/showAppearancePrefs:))))
    (#/autorelease item)))

(objc:defmethod #/toolbarDefaultItemIdentifiers:
		((self ccl-preferences-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/arrayWithObjects: ns:ns-array #@"general"
		       #@"appearance"
		       +null-ptr+))

(objc:defmethod #/toolbarAllowedItemIdentifiers:
		((self ccl-preferences-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/arrayWithObjects: ns:ns-array #@"general"
		       #@"appearance"
		       +null-ptr+))

(objc:defmethod #/toolbarSelectableItemIdentifiers:
		((self ccl-preferences-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/arrayWithObjects: ns:ns-array #@"general"
		       #@"appearance"
		       +null-ptr+))

(defun switch-preference-view (window view)
  (#/setSubviews: (#/contentView window)
                  (#/array ns:ns-array))
  (let* ((frame (#/frame window))
	 (min-size (#/minSize window))
	 (new-frame nil)
         (view-frame (#/frame view))
         (view-height (ns:ns-rect-height view-frame))
         (view-width (ns:ns-rect-width view-frame))
	 (content-rect (#/contentRectForFrameRect: window frame))
	 (dy (- view-height
		(ns-height content-rect))))
    (decf (ns:ns-rect-y content-rect) dy)
    (incf (ns:ns-rect-height content-rect) dy)
    (setf (ns:ns-rect-width content-rect) (max view-width
					       (ns:ns-size-width min-size)))
    (setq new-frame (#/frameRectForContentRect: window content-rect))
    (#/setFrame:display:animate: window new-frame t t))
  (#/setSubviews: (#/contentView window)
                  (#/arrayWithObject: ns:ns-array view)))

;;; toolbar actions

(objc:defmethod (#/showGeneralPrefs: :void) ((self ccl-preferences-window-controller)
						sender)
  (declare (ignore sender))
  (#/setTitle: (#/window self) #@"General")
  (switch-preference-view (#/window self) (slot-value self 'general-view)))

(objc:defmethod (#/showAppearancePrefs: :void) ((self ccl-preferences-window-controller)
						sender)
  (declare (ignore sender))
  (#/setTitle: (#/window self) #@"Appearance")
  (switch-preference-view (#/window self) (slot-value self 'appearance-view)))
