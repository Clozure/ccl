(in-package "INSPECTOR")

(defloadvar *inspector-cascade-point* (ns:make-ns-point 0 0))

(defclass xinspector-window-controller (ns:ns-window-controller)
  ((inspector :initarg :inspector :reader inspector)
   ;; outlets
   (action-menu :foreign-type :id :accessor action-menu)
   (refresh-button :foreign-type :id :accessor refresh-button)
   (back-forward-control :foreign-type :id :accessor back-forward-control)
   (table-view :foreign-type :id :accessor table-view)
   (contextual-menu :foreign-type :id :accessor contextual-menu)
  ;; data source variables
   (row-objects :foreign-type :id :reader row-objects))
  (:metaclass ns:+ns-object))

(defmethod (setf inspector) (new-inspector (wc xinspector-window-controller))
  (update-line-count new-inspector)
  (#/removeAllObjects (row-objects wc))
  (let ((n (#/null ns:ns-null)))
    (dotimes (i (inspector-line-count new-inspector))
      (#/addObject: (row-objects wc) n)))
  (setf (slot-value wc 'inspector) new-inspector))

(objc:defmethod #/init ((wc xinspector-window-controller))
  ;; Lisp slots are not set up yet when we are called.
  (let ((self (#/initWithWindowNibName: wc #@"xinspector")))
    (unless (%null-ptr-p self)
      (with-slots (row-objects) wc
        (setf row-objects (make-instance 'ns:ns-mutable-array))))
    ;; We implement custom cascading.
    (#/setShouldCascadeWindows: wc nil)
    self))

(defmethod initialize-instance :after ((wc xinspector-window-controller) &key inspector 
                                       &allow-other-keys)
  (setf (inspector wc) inspector))

(objc:defmethod (#/dealloc :void) ((wc xinspector-window-controller))
  (#/release (slot-value wc 'row-objects))
  (call-next-method))

(objc:defmethod (#/windowDidLoad :void) ((wc xinspector-window-controller))
  (#/setDoubleAction: (table-view wc) (ccl::@selector #/inspect:))
  ;; Cascade window from the top left point of the topmost inspector window.
  (flet ((good-window-p (w)
           (and (not (eql w (#/window wc)))
                (eql (#/class (#/windowController w))
                     (find-class 'xinspector-window-controller)))))
    (let* ((inspectors (remove-if-not #'good-window-p (gui::windows)))
           (top-inspector (car inspectors)))
      (if top-inspector
        (ns:with-ns-point (zp 0 0)
          (setq *inspector-cascade-point*
                (#/cascadeTopLeftFromPoint: top-inspector zp))))))
  (#/cascadeTopLeftFromPoint: (#/window wc) *inspector-cascade-point*))


(objc:defmethod (#/windowWillClose: :void) ((wc xinspector-window-controller) notification)
  (declare (ignore notification))
  (#/autorelease wc))

(objc:defmethod (#/backOrForward: :void) ((wc xinspector-window-controller) sender)
  (if (= (#/selectedSegment sender) 0)
    (format *trace-output* "~&go back")
    (format *trace-output* "~&go forward")))

(objc:defmethod (#/inspect: :void) ((wc xinspector-window-controller) sender)
  (declare (ignore sender))
  (let* ((row (#/selectedRow (table-view wc)))
         (clicked-row (#/clickedRow (table-view wc))))
    (when (/= clicked-row -1)
      (setq row clicked-row))
    (inspector::cocoa-inspect (inspector::line-n (slot-value wc 'inspector) row))))

;;; NSTableView data source methods

(objc:defmethod (#/numberOfRowsInTableView: #>NSInteger) ((self xinspector-window-controller)
                                                          table-view)
  (declare (ignore table-view))
  (#/count (row-objects self)))

(objc:defmethod #/tableView:objectValueForTableColumn:row: ((self xinspector-window-controller)
                                                            table-view table-column
                                                            (row #>NSInteger))
  (declare (ignore table-view table-column))
  (with-slots (inspector row-objects) self
    (when (eql (#/objectAtIndex: row-objects row) (#/null ns:ns-null))
      (let* ((name (ccl::%make-nsstring (with-output-to-string (s)
                                          (prin1-line-n inspector s row)))))
        (#/replaceObjectAtIndex:withObject: row-objects row name)
        (#/release name)))
    (#/objectAtIndex: row-objects row)))

(objc:defmethod (#/tableView:isGroupRow: #>BOOL) ((wc xinspector-window-controller)
                                                  table-view
                                                  (row #>NSInteger))
  (declare (ignore table-view))
  (with-accessors ((seq sequence)) wc
    (let ((type (nth-value 2 (inspector::line-n (slot-value wc 'inspector) row))))
      (if (consp type) (setq type (car type)))
      (eq type :comment))))

(objc:defmethod (#/tableView:shouldSelectRow: #>BOOL) ((wc xinspector-window-controller)
                                                       table-view
                                                       (row #>NSInteger))
  (declare (ignore table-view))
  (with-accessors ((seq sequence)) wc
    (let ((type (nth-value 2 (inspector::line-n (slot-value wc 'inspector) row))))
      (if (consp type) (setq type (car type)))
      (neq type :comment))))

(defun cocoa-inspect (thing)
  (gui::execute-in-gui #'(lambda ()
                           (let ((wc (make-instance 'xinspector-window-controller
                                       :inspector (make-inspector thing))))
                             (#/showWindow: wc nil)))))

