; -*- Mode: Lisp; Package: GUI; -*-

(in-package "GUI")

(defclass sequence-window-controller (ns:ns-window-controller)
    ((table-view :foreign-type :id :reader sequence-window-controller-table-view)
     (sequence :initform nil :initarg :sequence :type sequence :reader sequence-window-controller-sequence)
     (result-callback :initarg :result-callback)
     (display :initform #'(lambda (item stream) (prin1 item stream)) :initarg :display)
     (title :initform "Sequence dialog" :initarg :title))
  (:metaclass ns:+ns-object))


(objc:defmethod #/init ((self sequence-window-controller))
  (call-next-method)
  (let* ((w (new-cocoa-window :activate nil))
         (contentview (#/contentView w))
         (contentframe (#/frame contentview))
         (scrollview (make-instance 'ns:ns-scroll-view :with-frame contentframe)))
    (#/setWindow: self w)
    (#/setDelegate: w self)
    (#/setWindowController: w self)
    (#/setHasVerticalScroller: scrollview t)
    (#/setHasHorizontalScroller: scrollview t)
    (#/setAutohidesScrollers: scrollview t)
    (#/setRulersVisible: scrollview nil)
    (#/setAutoresizingMask: scrollview (logior
                                        #$NSViewWidthSizable
                                        #$NSViewHeightSizable))
    (#/setAutoresizesSubviews: (#/contentView scrollview) t)
    (let* ((table-view (make-instance 'ns:ns-table-view)))
      (#/setDocumentView: scrollview table-view)
      (#/release table-view)
      (#/setColumnAutoresizingStyle: table-view #$NSTableViewUniformColumnAutoresizingStyle)
      (setf (slot-value self 'table-view) table-view)
      (let* ((column (make-instance 'ns:ns-table-column :with-identifier #@"")))
        (#/setEditable: column nil)
	(#/setResizingMask: column #$NSTableColumnAutoresizingMask)
        (#/addTableColumn: table-view column)
	(#/release column))
      (#/setAutoresizingMask: table-view (logior
                                          #$NSViewWidthSizable
                                          #$NSViewHeightSizable))
      (#/sizeToFit table-view)
      (#/setDataSource: table-view self)
      (#/setTarget: table-view self)
      (#/setHeaderView: table-view +null-ptr+)
      (#/setUsesAlternatingRowBackgroundColors: table-view t)
      (#/setDoubleAction: table-view (@selector #/sequenceDoubleClick:))
      (#/addSubview: contentview scrollview)
      (#/release scrollview)
      self)))

(objc:defmethod (#/dealloc :void) ((self sequence-window-controller))
  (call-next-method))

(objc:defmethod (#/windowWillClose: :void) ((self sequence-window-controller)
					    notification)
  (declare (ignore notification))
  (#/setDataSource: (slot-value self 'table-view) +null-ptr+)
  (#/autorelease self))

(objc:defmethod (#/sequenceDoubleClick: :void)
    ((self sequence-window-controller) sender)
  (let* ((n (#/clickedRow sender)))
    (when (>= n 0)
      (with-slots (sequence result-callback) self
        (funcall result-callback (elt sequence n))))))

(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger)
    ((self sequence-window-controller) view)
  (declare (ignore view))
  (length (slot-value self 'sequence)))


(objc:defmethod #/tableView:objectValueForTableColumn:row:
    ((self sequence-window-controller) view column (row :<NSI>nteger))
  (declare (ignore column view))
  (with-slots (display sequence) self
    (#/autorelease
     (%make-nsstring (with-output-to-string (s)
		       (funcall display (elt sequence row) s))))))

(defmethod initialize-instance :after ((self sequence-window-controller) &key &allow-other-keys)
  (let* ((window (#/window self)))
    (with-slots (title) self
      (when title (#/setTitle: window (%make-nsstring title))))
    (#/reloadData (sequence-window-controller-table-view self))
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     self
     (@selector #/showWindow:)
     +null-ptr+
     nil)))

;;; Looks like a "util" to me ...
(defun pathname-to-url (pathname)
  (make-instance 'ns:ns-url
                 :file-url-with-path
                 (%make-nsstring (native-translated-namestring pathname))))

(defun cgfloat (number)
  (float number ccl::+cgfloat-zero+))

(defun color-values-to-nscolor (red green blue alpha)
  (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color
                                              (cgfloat red)
                                              (cgfloat green)
                                              (cgfloat blue)
                                              (cgfloat alpha)))

(defun windows ()
  (let* ((win-arr (#/orderedWindows *NSApp*))
	 (ret nil))
    (dotimes (i (#/count win-arr))
      (push (#/objectAtIndex: win-arr i) ret))
    (nreverse ret)))

(defun log-debug (format-string &rest args)
  (#_NSLog (ccl::%make-nsstring (apply #'format nil format-string args))))

(defun assume-cocoa-thread ()
  #+debug (assert (eq *current-process* *initial-process*)))

(defmethod assume-not-editing ((whatever t)))

