;;;-*-Mode: LISP; Package: CONTEXT-MENU -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      context-menu.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This code provides a mechanism for switching the tool that has access to 
;;;      Hemlock's contextual popup menu.  This is an initial prototype, implementing
;;;      what may be the simplest approach.
;;;
;;;      The API for writing new tools is described in the accompanying NewTools file.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      9/2/9   Changed the appearance of the Default Tool submenu.
;;;      8/31/9  version 0.1b1
;;;              First cut
;;;              Numerous User Interface suggestions, Rainer Joswig
;;;
;;; ----------------------------------------------------------------------------

(defpackage "CONTEXT-MENU" (:nicknames "CMENU") (:use :cl :ccl))
(in-package "CONTEXT-MENU")

(export '(register-tool add-default-tool-menu update-tool-menu set-default-tool
          tool-menu *hemlock-menu-dictionary* *tool-label-dictionary* *tool-doc-dictionary*
          *tool-key-dictionary* *dark-blue-color* *dark-turquoise-color* *light-gray-color* 
          *wine-red-color* check-hyperspec-availability))

(defparameter *menu-manager* nil "The context-menu-manager instance.")

(defparameter *dark-blue-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.2 0.2 0.5 1.0))
(defparameter *dark-turquoise-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.0 0.28 0.28 1.0))
(defparameter *wine-red-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.4 0.1 0.2 1.0))
(defparameter *light-gray-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.92 0.92 0.92 1.0))

(defparameter *hemlock-menu-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *hemlock-menu-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *hemlock-menu-dictionary* *dark-blue-color* #&NSForegroundColorAttributeName)

(defparameter *tool-label-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *tool-label-dictionary* (#/systemFontOfSize: ns:ns-font (#/systemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *tool-label-dictionary* *dark-turquoise-color* #&NSForegroundColorAttributeName)

(defparameter *tool-doc-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *tool-doc-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *tool-doc-dictionary* *dark-turquoise-color* #&NSForegroundColorAttributeName)

(defparameter *tool-key-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *tool-key-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *tool-key-dictionary* *wine-red-color* #&NSForegroundColorAttributeName)

;;; ----------------------------------------------------------------------------
;;;
(defclass CONTEXT-MENU-MANAGER ()
  ((tool-alist :initform nil :accessor tool-alist)
   (default-tool :initform nil :accessor default-tool))
  (:documentation "A class to manage Hemlock's contextual popup menu, supporting access by multiple tools."))

(defmethod display-menu ((manager context-menu-manager) view event)
  (when (default-tool manager)
    (let ((entry (assoc (default-tool manager) (tool-alist manager) :test #'string-equal)))
      (when entry 
        (funcall (cdr entry) view event)))))

(objc:defmethod #/menuForEvent: ((view gui::hemlock-text-view) (event :id))
  (display-menu *menu-manager* view event))

(defun register-tool (tool-name menu-function)
  "Register the new tool with the menu-manager.  The last tool registered becomes the default tool."
  (let ((entry (find tool-name (tool-alist *menu-manager*) :test #'string-equal :key #'car)))
    (cond (entry
           (gui::alert-window :title "Notification" :message (format nil "Re-registering ~S." tool-name))
           (setf (tool-alist *menu-manager*) (delete tool-name (tool-alist *menu-manager*) :test #'string-equal :key #'car))
           (setf (tool-alist *menu-manager*) (cons (cons tool-name menu-function) (tool-alist *menu-manager*))))           
          (t
           (setf (tool-alist *menu-manager*) (cons (cons tool-name menu-function) (tool-alist *menu-manager*)))
           (setf (tool-alist *menu-manager*)
                 (sort (tool-alist *menu-manager*) #'string< :key #'car))
           (set-default-tool tool-name)))))

(defun set-default-tool (tool-name)
  "Set the menu-manager's default tool.  Right-Click will display this tool's menu."
  (let ((registered-name (car (find tool-name (tool-alist *menu-manager*) :test #'string-equal :key #'car))))
    (if registered-name
      (setf (default-tool *menu-manager*) registered-name) ; keep the original capitalization
      (gui::alert-window :title "Notification" :message (format nil "~S is not a registered tool.  It can't be set as default." tool-name)))))

;;; ----------------------------------------------------------------------------
;;;
(defclass DEFAULT-TOOL-MENU-ITEM (ns:ns-menu-item)
  ((name :accessor tool-name)) ; Lisp string
  (:documentation "Support for the Tool submenu.")
  (:metaclass ns:+ns-object))

;;; ----------------------------------------------------------------------------
;;;
(defclass DEFAULT-TOOL-DOC-MENU-ITEM (ns:ns-menu-item)
  ((filename :accessor tool-filename))
  (:documentation "A menu-item to display the default tool's documentation.")
  (:metaclass ns:+ns-object))

;;; ----------------------------------------------------------------------------
;;;
(defclass DEFAULT-TOOL-MENU (ns:ns-menu)
  ()
  (:documentation "A submenu displaying all registered tools.")
  (:metaclass ns:+ns-object))

(objc:defmethod (#/hemlockDefaultToolAction: :void) ((m default-tool-menu) (sender :id))
  (set-default-tool (tool-name sender)))

(objc:defmethod (#/hemlockDefaultToolDocAction: :void) ((m default-tool-menu) (sender :id))
  (display-doc (tool-filename sender)))

(defun display-doc (path)
  "Display the default tool's documentation."
  (when (probe-file path)
    (#/openFile:withApplication: (#/sharedWorkspace ns:ns-workspace) 
                                 (ccl::%make-nsstring (namestring path))
                                 (ccl::%make-nsstring "TextEdit"))))
  
(defmethod populate-menu ((menu default-tool-menu))
  (dotimes (count (#/numberOfItems menu))
    (#/removeItemAtIndex: menu 0))
  (flet ((create-menu-item (name)
           (let ((menu-item (make-instance 'default-tool-menu-item))
                 (attributed-string (#/initWithString:attributes:
                                     (#/alloc ns:ns-attributed-string) 
                                     (ccl::%make-nsstring name)
                                     *tool-label-dictionary*)))
             (setf (tool-name menu-item) name) 
             (#/setAttributedTitle: menu-item attributed-string)
             (#/setAction: menu-item (ccl::@selector "hemlockDefaultToolAction:"))
             (#/setTarget: menu-item  menu)
             (if (string-equal name (default-tool *menu-manager*))
               (#/setState: menu-item #$NSOnState)
               (#/setState: menu-item #$NSOffState))
             (#/addItem: menu menu-item))))
    (dolist (entry (tool-alist *menu-manager*))
      (create-menu-item (car entry)))))

(defun add-default-tool-menu (menu &key doc-file)
  "Add the default tool submenu and possibly a documentation menu-item to MENU."
  (let ((default-item (make-instance ns:ns-menu-item))
        (tool-menu (make-instance 'default-tool-menu)))
    ;; Title is set by update method.
    (#/setSubmenu: default-item tool-menu)
    (#/insertItem:atIndex: menu default-item 0)
    (cond (doc-file
           (let ((doc-item (make-instance 'default-tool-doc-menu-item))
                 (attributed-string (#/initWithString:attributes:
                                     (#/alloc ns:ns-attributed-string) 
                                     (ccl::%make-nsstring (format nil "~A     doc..." (default-tool *menu-manager*)))
                                     *tool-doc-dictionary*)))
             (#/setAttributedTitle: doc-item attributed-string)
             (#/setAction: doc-item (ccl::@selector "hemlockDefaultToolDocAction:"))
             (#/setTarget: doc-item  tool-menu)
             (setf (tool-filename doc-item) doc-file)
             (#/insertItem:atIndex: menu doc-item 1))
          (#/insertItem:atIndex: menu (#/separatorItem ns:ns-menu-item) 2))
          (t
           (#/insertItem:atIndex: menu (#/separatorItem ns:ns-menu-item) 1)))
    tool-menu))

(defun update-tool-menu (menu default-menu &key sub-title)
  "Update MENU's Tool submenu."
  (let ((first-item (#/itemAtIndex: menu 0))
        (attributed-string (#/initWithString:attributes:
                            (#/alloc ns:ns-attributed-string) 
                            (if sub-title
                              (ccl::%make-nsstring (format nil "~S
    (~A)" (default-tool *menu-manager*) sub-title))
                              (ccl::%make-nsstring (format nil "~S" (default-tool *menu-manager*))))
                            *tool-label-dictionary*)))
    (#/setAttributedTitle: first-item attributed-string)
    (populate-menu default-menu)))

(let (checked-p)
(defun check-hyperspec-availability (tool-name)
  "Some tools require the HyperSpec."
  (unless (or checked-p gui::*hyperspec-root-url*)
    (rlet ((perror :id  +null-ptr+))
      (let* ((map-url (make-instance 'ns:ns-url :with-string #@"Data/Map_Sym.txt" :relative-to-url (gui::hyperspec-root-url)))
             ;; kludge alert:
             (data (make-instance 'ns:ns-data
                     :with-contents-of-url map-url
                     :options 0
                     :error perror)))
        (declare (ignore data))
        (setq checked-p t)
        (unless (%null-ptr-p (pref perror :id))
          (gui::alert-window 
           :title "Notification" 
           :message (format nil "~S needs the HyperSpec, and it does not appear to be available. Check the documentation in the Context-Menu-CM/ReadMe, and restart CCL." tool-name))))))))

(setq *menu-manager* (make-instance 'context-menu-manager))



