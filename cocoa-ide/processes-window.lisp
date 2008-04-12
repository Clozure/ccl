;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

(defclass processes-window-controller (ns:ns-window-controller)
  ((table-view :foreign-type :id :reader processes-window-table-view)
   (toolbar :foreign-type :id :accessor processes-window-toolbar)
   (processes :accessor processes-window-processes))
  (:metaclass ns:+ns-object))

(objc:defmethod #/init ((self processes-window-controller))
  (setf (slot-value self 'processes) (coerce (all-processes) 'vector))
  (#/initWithWindowNibName: self #@"processes"))

(objc:defmethod (#/awakeFromNib :void) ((self processes-window-controller))
  (with-slots (toolbar table-view) self
    (#/setDoubleAction: table-view (@selector #/inspectSelectedProcess:))
    (setf toolbar (make-instance 'ns:ns-toolbar
				 :with-identifier #@"processes-window-toolbar"))
    (#/setDisplayMode: toolbar #$NSToolbarDisplayModeLabelOnly)
    (#/setDelegate: toolbar self)
    (#/setToolbar: (#/window self) toolbar)
    (#/release toolbar)))



;;; toolbar delegate methods

(objc:defmethod #/toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:
		((self processes-window-controller)
		 toolbar itemIdentifier (flag :<BOOL>))
  (declare (ignore toolbar))
  (let ((item +null-ptr+))
    (cond
     ((#/isEqualToString: itemIdentifier #@"kill")
      (setf item (make-instance 'ns:ns-toolbar-item :with-item-identifier itemIdentifier))
      (#/setLabel: item #@"Kill")
      (#/setTarget: item self)
      (#/setAction: item (@selector #/killSelectedProcess:)))
     ((#/isEqualToString: itemIdentifier #@"refresh")
      (setf item (make-instance 'ns:ns-toolbar-item :with-item-identifier itemIdentifier))
      (#/setLabel: item #@"Refresh")
      (#/setTarget: item self)
      (#/setAction: item (@selector #/refresh:))))
    (#/autorelease item)))

(objc:defmethod #/toolbarDefaultItemIdentifiers:
		((self processes-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/arrayWithObjects: ns:ns-array #@"kill"
		       #&NSToolbarFlexibleSpaceItemIdentifier
		       #@"refresh"
		       +null-ptr+)) ; don't even think about putting nil here

(objc:defmethod #/toolbarAllowedItemIdentifiers:
		((self processes-window-controller) toolbar)
  (declare (ignore toolbar))
  (#/arrayWithObjects: ns:ns-array #@"refresh"
		       #&NSToolbarFlexibleSpaceItemIdentifier
		       #@"refresh"
		       +null-ptr+))

(objc:defmethod (#/validateToolbarItem: :<BOOL>)
		((self processes-window-controller) item)
  (let ((enable #$NO))
    (cond
     ((#/isEqualToString: (#/itemIdentifier item) #@"kill")
      (when (plusp (#/numberOfSelectedRows (processes-window-table-view self)))
	(setf enable #$YES)))
     ((#/isEqualToString: (#/itemIdentifier item) #@"refresh")
      (setf enable #$YES)))
    enable))

;;; actions

(objc:defmethod (#/refresh: :void) ((self processes-window-controller) sender)
  (declare (ignore sender))
  (setf (slot-value self 'processes)
	(coerce (all-processes) 'vector))
  (#/reloadData (processes-window-table-view self)))

(objc:defmethod (#/killSelectedProcess: :void) ((self processes-window-controller) sender)
  (declare (ignore sender))
  (let ((row (#/selectedRow (processes-window-table-view self)))
	(p nil))
    (unless (minusp row)
      (setq p (svref (processes-window-processes self) row))
      (process-kill p)
      (#/refresh: self self))))

(objc:defmethod (#/inspectSelectedProcess: :void) ((self processes-window-controller) sender)
  (declare (ignore sender))
  (with-slots (table-view processes) self
    (let* ((row (#/clickedRow table-view))
	   (p nil))
      (unless (minusp row)
	(setq p (svref processes row))
	(inspect p)
	(#/refresh: self self)))))

;;; table view delegate methods

(objc:defmethod (#/tableViewSelectionDidChange: :void)
		((self processes-window-controller) notification)
  (declare (ignore notification))
  (with-slots (toolbar) self
    ;; Usually, we'd just update the one item in question,
    ;; but since there aren't many items in the toolbar,
    ;; just be lazy.
    (#/validateVisibleItems toolbar)))

;;; table view data source methods

(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger)
		((self processes-window-controller)
		 table-view)
  (declare (ignore table-view))
  (length (slot-value self 'processes)))

(objc:defmethod #/tableView:objectValueForTableColumn:row:
		((self processes-window-controller)
		 table-view
		 table-column
		 (row :<NSI>nteger))
  (declare (ignore table-view))
  (with-slots (processes) self
    (let ((fn nil)
	  (p (svref processes row)))
      (cond
       ((#/isEqualToString: (#/identifier table-column) #@"name")
	(setq fn #'process-name))
       ((#/isEqualToString: (#/identifier table-column) #@"state")
	(setq fn #'process-whostate))
       ((#/isEqualToString: (#/identifier table-column) #@"thread")
	(setq fn #'process-thread))
       ((#/isEqualToString: (#/identifier table-column) #@"suspend count")
	(setq fn #'process-suspend-count)))
      (if (and p fn)
	(#/autorelease (%make-nsstring (format nil "~a" (funcall fn p))))
	+null-ptr+))))

#|
(in-package "CCL")
(load "~rme/processes-window")
(setf *pwc* (make-instance 'processes-window-controller))
(#/showWindow: *pwc* *pwc*)

|#
