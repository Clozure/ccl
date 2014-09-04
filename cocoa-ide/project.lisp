(in-package "GUI")

;;; This file contains a project browser for the CCL IDE. A "project" is
;;; just the set of ASDF systems defined in the same file.
;;; 
;;; To browse a project, run (gui::make-project-window :project-name). The
;;; project name is the name of the .asd file. However, if the .asd has been
;;; loaded, then the project browser can be opened using the name of any of the
;;; systems defined in that file.
;;;
;;; Todo:
;;; • support modification without resorting to editor window
;;; • raise window if project already open
;;; • handle #-ccl sections in .asd
;;; • support arbitrary operations

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defclass project-window-controller (ns:ns-window-controller)
  ((system-view :foreign-type :id :accessor system-view) ; IBOutlet
   (component-view :foreign-type :id :accessor component-view) ; IBOutlet

   ;; Info Panel fields
   (info-panel :foreign-type :id :accessor info-panel) ; IBOutlet
   (name-label :foreign-type :id)
   (version-label :foreign-type :id)
   (author-label :foreign-type :id)
   (maintainer-label :foreign-type :id)
   (licence-label :foreign-type :id)
   (description-label :foreign-type :id)
   (long-description-label :foreign-type :id)

   (system-menu :foreign-type :id :accessor system-menu) ; IBOutlet set by NIB
   (item-menu :foreign-type :id :accessor item-menu) ; IBOutlet set by NIB
   (op-button :foreign-type :id :accessor op-button) ; IBOutlet set by NIB
   (toolbar-items :foreign-type :id :accessor toolbar-items)
   (project-item :initarg :project-item :reader project-item)
   (components-field :reader components-field
                     :initform (make-instance 'ns:ns-text-field-cell
                                 :text-cell #@"Components"))
   (dependencies-field :reader dependencies-field
                       :initform (make-instance 'ns:ns-text-field-cell
                                   :text-cell #@"Dependencies")))
  (:metaclass ns:+ns-object))

(defmethod current-system-item (controller)
  (#/objectAtIndex: (systems (project-item controller))
                    (#/selectedRow (system-view controller))))

;;; Need to represent system in a way that the NSOutlineView likes

(defclass component-item (ns:ns-object)
  ((name :foreign-type :id :accessor name))
  (:metaclass ns:+ns-object))

(defclass module-item (component-item)
  ((components :foreign-type :id :accessor components
               :initform (make-instance 'ns:ns-mutable-array)))
  (:metaclass ns:+ns-object))

(defclass project-item (ns:ns-object)
  ((name :foreign-type :id :accessor name)
   (systems :foreign-type :id :accessor systems
            :initform (make-instance 'ns:ns-mutable-array)))
  (:metaclass ns:+ns-object))

(defclass system-item (module-item)
  ((system :accessor system))
  (:metaclass ns:+ns-object))

(defclass dependency-item (ns:ns-object)
  ((name :foreign-type :id :accessor name)
   (operations :foreign-type :id :accessor operations
               :initform (make-instance 'ns:ns-mutable-array)))
  (:metaclass ns:+ns-object))

(defclass file-item (component-item)
  ((location :foreign-type :id :accessor location))
  (:metaclass ns:+ns-object))

(defmethod (setf component) (component (object component-item))
  (setf (name object) (%make-nsstring (asdf:component-name component))))

(defmethod (setf component) (component (object module-item))
  (call-next-method)
  (setf (components object) (make-instance 'ns:ns-mutable-array))
  (mapc (lambda (com)
          (#/addObject: (components object)
                        (let ((obj (make-instance
                                       (typecase com
                                         (asdf:system 'system-item)
                                         (asdf:module 'module-item)
                                         (asdf:source-file 'file-item)))))
                          (setf (component obj) com)
                          obj)))
        (asdf:module-components component)))

(defmethod (setf component) (component (object system-item))
  (call-next-method)
  (setf (system object) component))

(defmethod (setf component) (component (object file-item))
  (call-next-method)
  (setf (location object)
        (%make-nsstring (namestring (asdf:component-pathname component)))))

(objc:defmethod #/init ((self project-window-controller))
  (#/setShouldCascadeWindows: self t)
  (#/initWithWindowNibName: self #@"project"))

(objc:defmethod (#/awakeFromNib :void) ((self project-window-controller))
  (with-slots (system-view component-view
               system-menu item-menu
               op-button toolbar-items)
              self
    (let ((toolbar (make-instance 'ns:ns-toolbar
                     :with-identifier #@"projectbar")))
      (setf (#/allowsUserCustomization toolbar) t
            (#/delegate toolbar) self)
      (setf toolbar-items (make-instance 'ns:ns-mutable-dictionary))
      (let ((op-item (make-instance 'ns:ns-toolbar-item
                       :with-item-identifier #@"operate")))
        (setf (#/label op-item) #@"Operate"
              (#/paletteLabel op-item) #@"Operate")
        (let* ((op-menu (#/menu op-button)))
          (loop for i from 1 to (1- (#/numberOfItems op-menu)) ; 0 is blank
            do (let ((item (#/itemAtIndex: op-menu i)))
                 (#/setTarget: item self)
                 (#/setAction: item (@selector #/operate:)))))
        (setf (#/view op-item) op-button
              (#/objectForKey: toolbar-items #@"operate") op-item))
      (let ((toolbar-item (make-instance 'ns:ns-toolbar-item
                            :with-item-identifier #@"info")))
        (setf (#/label toolbar-item) #@"Get Info"
              (#/paletteLabel toolbar-item) #@"Get Info"
              (#/image toolbar-item) (#/imageNamed: ns:ns-image #@"info")
              (#/target toolbar-item) self
              (#/action toolbar-item) (@selector #/showInfoPanel)
              (#/objectForKey: toolbar-items #@"info") toolbar-item))
      (let ((toolbar-item (make-instance 'ns:ns-toolbar-item
                            :with-item-identifier #@"edit")))
        (setf (#/label toolbar-item) #@"Edit"
              (#/paletteLabel toolbar-item) #@"Edit"
              (#/image toolbar-item) (#/imageNamed: ns:ns-image #@"font-panel")
              (#/target toolbar-item) self
              (#/action toolbar-item) (@selector #/openSystem:)
              (#/objectForKey: toolbar-items #@"edit") toolbar-item))
      (#/setToolbar: (#/window self) toolbar))
    (let ((mi0 (#/itemAtIndex: system-menu 0)) ; Operate
          (mi1 (#/itemAtIndex: system-menu 1)) ; Get Info
          (mi2 (#/itemAtIndex: system-menu 2))) ; Edit
      (let* ((op-menu (#/menu mi0)))
        (loop for i from 0 to (1- (#/numberOfItems op-menu))
          do (let ((item (#/itemAtIndex: op-menu i)))
               (setf (#/enabled item) t
                     (#/target item) self
                     (#/action item) (@selector #/operate:)))))
      (setf (#/enabled mi1) t
            (#/target mi1) self
            (#/action mi1) (@selector #/showInfoPanel))
      (setf (#/enabled mi2) t
            (#/target mi2) self
            (#/action mi2) (@selector #/openSystem:)))
    (setf (#/target system-view) self
          (#/doubleAction system-view) (@selector #/openSystem:))
    (let ((mi0 (#/itemAtIndex: item-menu 0)) ; Open
          (mi1 (#/itemAtIndex: item-menu 1)) ; Compile
          (mi2 (#/itemAtIndex: item-menu 2))) ; Compile and Load
      (setf (#/enabled mi0) t
            (#/target mi0) self
            (#/action mi0) (@selector #/openComponent:))
      (setf (#/enabled mi1) t
            (#/target mi1) self
            (#/action mi1) (@selector #/compileComponent:))
      (setf (#/enabled mi2) t
            (#/target mi2) self
            (#/action mi2) (@selector #/compileAndLoadComponent:)))
    (setf (#/target component-view) self
          (#/doubleAction component-view) (@selector #/openComponent:))))

(objc:defmethod (#/showInfoPanel :void) ((self project-window-controller))
  (with-slots (info-panel name-label version-label author-label maintainer-label
               licence-label description-label long-description-label)
    self
    (let ((system (system (current-system-item self))))
      (#/makeKeyAndOrderFront: info-panel self)
      (setf (#/stringValue name-label)
            (%make-nsstring (asdf:component-name system))
            (#/stringValue version-label)
            (%make-nsstring (handler-case (asdf:component-version system)
                              (slot-unbound () "")))
            (#/stringValue author-label)
            (%make-nsstring (handler-case (asdf:system-author system)
                              (slot-unbound () "")))
            (#/stringValue maintainer-label)
            (%make-nsstring (handler-case (asdf:system-maintainer system)
                              (slot-unbound () "")))
            (#/stringValue licence-label)
            (%make-nsstring (handler-case (asdf:system-licence system)
                              (slot-unbound () "")))
            (#/stringValue description-label)
            (%make-nsstring (handler-case (asdf:system-description system)
                              (slot-unbound () "")))
            (#/stringValue long-description-label)
            (%make-nsstring (handler-case
                                (asdf:system-long-description system)
                              (slot-unbound () "")))))))

;;; NSToolbar delegate methods

(objc:defmethod #/toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:
                ((self project-window-controller)
                 toolbar itemIdentifier (flag :<BOOL>))
  (declare (ignore toolbar))
  (#/objectForKey: (toolbar-items self) itemIdentifier))

(objc:defmethod #/toolbarAllowedItemIdentifiers:
                ((self project-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/allKeys (toolbar-items self)))

(objc:defmethod #/toolbarDefaultItemIdentifiers:
                ((self project-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/arrayWithObjects: ns:ns-array #@"operate" #@"info" #@"edit" +null-ptr+))

(objc:defmethod (#/openSystem: :void) ((self project-window-controller) sender)
  (declare (ignore sender))
  (find-or-make-hemlock-view
   (asdf:system-source-file (lisp-string-from-nsstring
                             (name (project-item self))))))

(objc:defmethod (#/openComponent: :void)
                ((self project-window-controller) sender)
  (declare (ignore sender))
  (let ((row (#/clickedRow (component-view self))))
    (unless (minusp row)
      (let ((item (#/itemAtRow: (component-view self) row)))
        (typecase item 
          (file-item (find-or-make-hemlock-view
                      (lisp-string-from-nsstring (location item))))
          (ns:ns-string (make-project-window
                         (make-symbol (string-upcase (lisp-string-from-nsstring
                                                      item))))))))))

(objc:defmethod (#/compileComponent: :void)
                ((self project-window-controller) sender)
  (declare (ignore sender))
  (let ((row (#/clickedRow (component-view self))))
    (unless (minusp row)
      (let ((item (#/itemAtRow: (component-view self) row)))
        (when (typep item 'file-item)
          (ui-object-compile-buffer *NSApp*
                                    (list :cl-user
                                          (lisp-string-from-nsstring
                                           (location item)))))))))

(objc:defmethod (#/compileAndLoadComponent: :void)
                ((self project-window-controller) sender)
  (declare (ignore sender))
  (let ((row (#/clickedRow (component-view self))))
    (unless (minusp row)
      (let ((item (#/itemAtRow: (component-view self) row)))
        (when (typep item 'file-item)
          (ui-object-compile-and-load-buffer
           *NSApp*
           (list :cl-user (lisp-string-from-nsstring (location item)))))))))

(objc:defmethod (#/operate: :void) ((self project-window-controller) sender)
  (let* ((target-listener (ui-object-choose-listener-for-selection *NSApp*
                                                                   nil)))
    (when target-listener
      (let ((string (format nil "(asdf:oos 'asdf:~A-op :~A)"
                            (substitute #\-
                                        #\space
                                        (lisp-string-from-nsstring
                                         (#/title sender)))
                            (if (= (#/compare: (#/title (#/menu sender))
                                               #@"project-operations")
                                   #$NSOrderedSame)
                              (lisp-string-from-nsstring
                               (name (project-item self)))
                              (asdf:component-name
                               (system (current-system-item self)))))))
        (eval-in-listener-process target-listener string)))))

(defmethod (setf project-item)
           ((value asdf:system) (self project-window-controller))
  (let ((proj (make-instance 'project-item))
        (proj-name (pathname-name (asdf:system-source-file value))))
    (setf (name proj) (%make-nsstring proj-name))
    (setf (systems proj) (make-instance 'ns:ns-mutable-array))
    (mapc (lambda (system)
            (let ((component (make-instance 'system-item)))
              (setf (component component) system)
              (#/addObject: (systems proj) component)))
          (find-related-systems value))
    (setf (slot-value self 'project-item) proj)
    (#/setTitle: (#/window self)
                 (%make-nsstring (concatenate 'string
                                              proj-name " – Project"))))
  (#/reloadData (component-view self)))

;;; NSTableView data source methods

(objc:defmethod (#/tableView:objectValueForTableColumn:row: :id)
                ((self project-window-controller)
                 table-view column (row :<NSI>nteger))
  (declare (ignore table-view column))
  (name (#/objectAtIndex: (systems (project-item self)) row)))

(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger)
                ((self project-window-controller) table-view)
  (declare (ignore table-view))
  (#/count (systems (project-item self))))

(objc:defmethod (#/tableViewSelectionDidChange: :void)
                ((self project-window-controller) sender)
  (declare (ignore sender))
  (#/reloadData (component-view self)))

;;; NSOutlineView data source methods

(objc:defmethod (#/outlineView:child:ofItem: :id)
                ((self project-window-controller)
                 outline-view (index :<NSI>nteger) item)
  (declare (ignore outline-view))
  (cond ((eql item +null-ptr+) (case index
                                 (0 (components-field self))
                                 (1 (dependencies-field self))))
        ((typep item 'component-item)
         (#/objectAtIndex: (components item) index))
        ((eql item (components-field self))
         (#/objectAtIndex: (components (current-system-item self)) index))
        ((eql item (dependencies-field self))
         (let ((dependency (nth index
                                (remove (asdf:component-name
                                         (system (current-system-item self)))
                                        (reduce #'union
                                                (asdf:component-depends-on
                                                 'asdf:load-op
                                                 (system (current-system-item
                                                          self)))
                                                :key #'cdr)))))

           (%make-nsstring (if (consp dependency)
                             (namestring (asdf/component::component-pathname (second dependency)))
                             (namestring (asdf/component::component-pathname dependency))))))))

(objc:defmethod (#/outlineView:isItemExpandable: :<BOOL>)
                ((self project-window-controller) outline-view item)
  (declare (ignore outline-view))
  (not (or (typep item 'file-item) (typep item 'ns:ns-string))))

(objc:defmethod (#/outlineView:numberOfChildrenOfItem: :<NSI>nteger)
                ((self project-window-controller) outline-view item)
  (declare (ignore outline-view))
  (cond ((eql item +null-ptr+) 2)
        ((typep item 'component-item) (#/count (components item)))
        ((eql item (components-field self))
         (#/count (components (current-system-item self))))
        ((eql item (dependencies-field self))
         (length (remove (asdf:component-name (system (current-system-item
                                                       self)))
                         (reduce #'union
                                 (asdf:component-depends-on
                                  'asdf:load-op
                                  (system (current-system-item self)))
                                 :key #'cdr))))))

(objc:defmethod (#/outlineView:objectValueForTableColumn:byItem: :id)
                ((self project-window-controller) outline-view column item)
  (declare (ignore outline-view column))
  (cond ((eql item +null-ptr+) (name (current-system-item self)))
        ((typep item 'component-item) (name item))
        (t item)))

(objc:defmethod (#/outlineView:shouldSelectItem: :<BOOL>)
                ((self project-window-controller) outline-view item)
  (declare (ignore outline-view))
  (not (typep item 'ns:ns-text-field-cell)))

(objc:defmethod (#/outlineView:isGroupItem: :<BOOL>)
                ((self project-window-controller) outline-view item)
  (declare (ignore outline-view))
  (typep item 'ns:ns-text-field-cell))

(defgeneric make-project-window (obj)
  (:method ((obj asdf:system))
           (let ((controller (make-instance 'project-window-controller)))
             (setf (project-item controller) obj)
             (#/showWindow: controller nil)
             controller))
  (:method (obj)
           (make-project-window (asdf:find-system obj))))

(defun find-related-systems (obj)
  "This just uses our assumption that a project is the set of systems in a
   single .asd. It just finds all the systems with the same system-source-file."
  (let ((system-file (asdf:system-source-file (asdf:find-system obj))))
    (loop for system being the hash-values of asdf::*defined-systems*
      if (equal (asdf:system-source-file (cdr system)) system-file)
      collect (cdr system))))
