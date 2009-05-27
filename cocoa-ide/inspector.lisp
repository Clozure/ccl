(in-package "GUI")

#|
Implements inspector windows in Cocoa

This builds heavily on the inspector objects defined in ccl/lib/describe.lisp.

An inspector-item is an Objective-C object that contains a lisp-inspector, and a vector of child inspector-items
that are filled in lazily as required.

To Do:  
Make scroll bars work
Make command-left-arrow and command-right-arrow go back and forward
Add tabs
Set window title based on object
Add "Inspect" menu item (Key equivalent: command-I) - and make it work in many situations
  If an inspector is on top, inspect selection in place
  If an editor is on top
    If there is a selection, eval and inspect
    If no selection look for a nearby form to eval and inspect 
  If listener is on top and insertion point is after prompt, inspect *
  Inspect selection of other windows, backtrace, apropos, etc.

Make meta-dot edit source in many places, have menu item which is disabled when it doesn't make sense
Handle comments, and static inspector items
Make editing in place work in some situations
Make command-double-click bring up new inspector window
Make command-T inspect in a new tab
add bookmarks for commonly inspected objects - forms to evaluate
Add set-package widget to many places to effect printed representations and evaluation contexts
Make a way to get inspected object to listener, possibly set *
  (to be consistent with editor windows, make enter print it in listener setting *
    make command-enter, do that and bring listener to the front)
In some situations, remember and display form that was evaluated to get currently inspected object
When form is shown, refresh re-evaluates form
Possibly add splitter
Possibly add linked panes
Maybe get rid of contextual menus when main menu handles everything
Make preferences for fonts, key commands
|#

(defvar @ nil)
(defvar @@ nil)
(defvar @@@ nil)

(defclass ninspector-window-controller (ns:ns-window-controller)
  ((table-view :foreign-type :id :accessor table-view) ;IBOutlet set by nib file
   (property-column :foreign-type :id :accessor property-column) ;IBOutlet
   (value-column :foreign-type :id :accessor value-column) ;IBOutlet
   (object-label :foreign-type :id :accessor object-label) ;IBOutlet
   (back-button :foreign-type :id :accessor back-button) ;IBOutlet
   (forward-button :foreign-type :id :accessor forward-button) ;IBOutlet
   (refresh-button :foreign-type :id :accessor refresh-button) ;IBOutlet
   (item-menu :foreign-type :id :accessor item-menu) ;IBOutlet
   (viewed-inspector-items :initform (make-array 10 :fill-pointer 0 :adjustable t)  :accessor viewed-inspector-items)
   (next-index :initform 0 :accessor next-index 
               :documentation "The index of the next inspector-item in viewed-inspector-items.
               The index of the inspector-item currently being viewed is one less")
   (inspector-item :initarg :inspector-item :reader inspector-item))
  (:metaclass ns:+ns-object))

(objc:defmethod #/init ((self ninspector-window-controller))
  (#/setShouldCascadeWindows: self t)
  (#/initWithWindowNibName: self #@"inspector"))

(defmethod lisp-inspector ((wc ninspector-window-controller))
  (lisp-inspector (inspector-item wc)))

(defmethod set-current-inspector-item ((wc ninspector-window-controller) index)
  (with-slots (next-index viewed-inspector-items) wc
    (when (< -1 index (fill-pointer viewed-inspector-items))
      (setf next-index (1+ index))
      (set-enabled wc)
      (setf (inspector-item wc) (aref viewed-inspector-items index)))))

(defmethod set-enabled ((wc ninspector-window-controller))
  "Enables or disables buttons based on current state of viewed-inspector-items and next-index"
  (with-slots (forward-button back-button next-index viewed-inspector-items) wc
    (#/setEnabled: back-button (> next-index 1))
    (#/setEnabled: forward-button (< next-index (fill-pointer viewed-inspector-items)))))

;;Lifted from apropos-window.lisp, not sure if it's really needed...
(objc:defmethod (#/automaticallyNotifiesObserversForKey: :<BOOL>) ((self +ninspector-window-controller)
                                                                  key)
  (declare (ignore key))
  nil)

(objc:defmethod (#/awakeFromNib :void) ((self ninspector-window-controller))
  (with-slots (table-view back-button forward-button refresh-button item-menu) self
    (#/setTarget: back-button self)
    (#/setAction: back-button (@selector #/goBack:))
    (#/setTarget: forward-button self)
    (#/setAction: forward-button (@selector #/goForward:))
    (#/setTarget: refresh-button self)
    (#/setAction: refresh-button (@selector #/doRefresh:))
    (#/setTarget: table-view self)
    (#/setDoubleAction: table-view (@selector #/inspectSelectionInPlace:))
    (set-enabled self)
    (let ((mi0 (#/itemAtIndex: item-menu 0)) ;Inspect in new window
          (mi1 (#/itemAtIndex: item-menu 1)) ;Inspect in new tab
          (mi2 (#/itemAtIndex: item-menu 2))) ;Edit Source
      (#/setEnabled: mi0 t)
      (#/setTarget: mi0 self)
      (#/setAction: mi0 (@selector #/inspectSelectionInNewWindow:))
      (#/setEnabled: mi1 nil)
      (#/setTarget: mi1 self)
      (#/setAction: mi1 (@selector #/inspectSelectionInNewTab:))
      (#/setEnabled: mi2 nil) ;TODO why isn't this working?
      (#/setTarget: mi2 self)
      (#/setAction: mi2 (@selector #/editSelectionSource:)))
    (#/setMenu: table-view item-menu)
    ))

(objc:defmethod (#/inspectSelectionInPlace: :void) ((wc ninspector-window-controller) sender)
  (let* ((row (#/clickedRow sender)))
    (unless (minusp row)
      (with-slots (next-index viewed-inspector-items) wc
        (let ((ii (get-child (inspector-item wc) row)))
          (if (and (< next-index (fill-pointer viewed-inspector-items))
                   (eq ii (aref viewed-inspector-items next-index)))
            ;;If the ii is the same as the next history item, then just go forward in history
            (set-current-inspector-item wc next-index)
            ;;Otherwise forget the forward history
            (push-inspector-item wc ii)))))))

(objc:defmethod (#/inspectSelectionInNewWindow: :void) ((wc ninspector-window-controller) sender)
  (declare (ignore sender))
  (let* ((row (#/clickedRow (table-view wc))))
    (unless (minusp row)
      (with-slots (next-index viewed-inspector-items) wc
        (let* ((ii (get-child (inspector-item wc) row))
               (ob (inspector-object ii)))
          (make-inspector ob))))))

(objc:defmethod (#/inspectSelectionInSameWindow: :void) ((wc ninspector-window-controller) sender)
  (declare (ignore sender)))

(objc:defmethod (#/editSelectionSource: :void) ((wc ninspector-window-controller) sender)
  (declare (ignore sender)))

(objc:defmethod (#/goBack: :void) ((wc ninspector-window-controller) sender)
  (declare (ignore sender))
  (set-current-inspector-item wc (- (next-index wc) 2)))

(objc:defmethod (#/goForward: :void) ((wc ninspector-window-controller) sender)
  (declare (ignore sender))
  (set-current-inspector-item wc (next-index wc)))

(objc:defmethod (#/doRefresh: :void) ((wc ninspector-window-controller) sender)
  (declare (ignore sender))
  (push-inspector-item wc (make-inspector-item (inspector-object (inspector-item wc)))))

(defclass inspector-item (ns:ns-object)
  ((lisp-inspector :accessor lisp-inspector)
   (label :accessor inspector-item-label) ;NSString
   (ob-string :accessor inspector-item-ob-string) ;NSString
   (type :accessor inspector-item-type) ; oneof: nil :normal :colon :comment :static
   (children :initform nil)) ;initialized lazily
  (:metaclass ns:+ns-object))

(defmethod inspector-item-children ((ii inspector-item))
  (or (slot-value ii 'children)
      (let* ((li (lisp-inspector ii)))
        (when (null (inspector::inspector-line-count li))
          (inspector::update-line-count li))        
        (setf (slot-value ii 'children)
            (make-array (inspector::inspector-line-count li) :initial-element nil)))))

(defmethod inspector-object ((ii inspector-item))
  (inspector::inspector-object (lisp-inspector ii)))

(defmethod inspector-line-count ((ii inspector-item))
  (let ((li (lisp-inspector ii)))
    (or  (inspector::inspector-line-count li)
         (progn
           (inspector::update-line-count li)
           (inspector::inspector-line-count li)))))

(defun inspector-object-nsstring (ob)
  (let ((*print-readably* nil)
        (*signal-printing-errors* nil)
        (*print-circle* t)
        (*print-length* 20)
        (*print-pretty* nil))
    (%make-nsstring (prin1-to-string ob))))

(defun make-inspector-item (value &optional label type)
  (let* ((item (make-instance 'inspector-item))
	 (li (inspector::make-inspector value)))
    (setf (lisp-inspector item) li
          (inspector-item-ob-string item) (inspector-object-nsstring value)
	  (inspector-item-label item) label
	  (inspector-item-type item) type)
    item))

(defun make-inspector (ob)
  (let* ((wc (make-instance 'ninspector-window-controller))
         (ii (make-inspector-item ob)))
    (push-inspector-item wc ii)
    (#/showWindow: wc nil)
    wc))

(defmethod push-inspector-item ((wc ninspector-window-controller) (ii inspector-item))
  (with-slots (next-index viewed-inspector-items) wc
    (when (< next-index (fill-pointer viewed-inspector-items))
      (setf (fill-pointer viewed-inspector-items) next-index))
    (vector-push-extend ii viewed-inspector-items)
    (incf next-index))
  (set-enabled wc)
  (setf (inspector-item wc) ii))

(defmethod (setf inspector-item) ((ii inspector-item) (wc ninspector-window-controller))
  (setf @@@ @@
        @@ @
        @ (inspector-object ii))
  (setf (slot-value wc 'inspector-item) ii)
  (let* ((w (#/window wc))
         (title (inspector-item-ob-string ii)))
    (#/setTitle: w (%make-nsstring (concatenate 'string  "Inspector: " 
                                                (lisp-string-from-nsstring title))))
    (#/setStringValue: (object-label wc) title)
    (#/reloadData (table-view wc))))

(defun ninspect (object)
  (execute-in-gui #'(lambda () (make-inspector object))))


#|
The inspector-window-controller is specified in the nib file to be the data source for the NSTableView.
In order to be a data source it must implement the NSTableDataSource protocol.

The NSTableDataSource methods to get values for the NSTableView are:
- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex

For simplicity, the latter method returns NSStrings (it could return other types that need special formatting objects)

If we want the table view to support other features such as setting, sorting, or drag and drop, other
NSTableDataSource methods can be defined.
|#


(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger) ((self ninspector-window-controller) table-view)
  (declare (ignore table-view))
  (1- (length (inspector-item-children (inspector-item self))))) ;skip first child which just contains the object itself

(objc:defmethod #/tableView:objectValueForTableColumn:row: ((self ninspector-window-controller) table-view column (row :<NSI>nteger))
  (declare (ignore table-view))
  (let ((child (get-child (inspector-item self) row)))
    (cond ((eql column (property-column self)) (inspector-item-label child))
          ((eql column (value-column self)) (inspector-item-ob-string child))
          ((#/isEqualToString: #@"property" (#/identifier column)) (inspector-item-label child))
          ((#/isEqualToString: #@"value" (#/identifier column)) (inspector-item-ob-string child))
          (t (progn
               (log-debug "col: ~s prop-col: ~s val-col: ~s" column (property-column self) (value-column self))
               #@"*error*")))))

(defmethod get-child ((ii inspector-item) index)
  (let ((arr (inspector-item-children ii))
        (i (1+ index)))
    (or (svref arr i)
        (multiple-value-bind (ob label type) (inspector::line-n (lisp-inspector ii) i)
          (setf (svref arr i) (make-inspector-item ob (%make-nsstring (princ-to-string label)) type))))))

;;; Make INSPECT call CINSPECT.
(setq inspector::*default-inspector-ui-creation-function* 'ninspect)
