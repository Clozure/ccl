;;;-*-Mode: LISP; Package: GUI -*-

(in-package "GUI")

#|
(cinspect <thing>)

A cocoa-based lisp inspector, LGPL'ed by Hamilton Link

This code is freely distributable, etc. but I would appreciate people
submitting changes back to me and making suggestions about how it
could be altered or improved to me rather than starting a totally
separate inspector.

Major plans:
 Shift all the browser columns over to allow the first column to just have the object
 Make double-clicking an object bring any existing inspector for that object to the front unless shift key is held

Minor tweaks:
  test on all sorts of things for sanity of leaf-ness of nodes and fields
  test on all sorts of things for santity in what's safely editable in table view
  fix the leaf-ness fields with a macptr value
  change the font to something smaller (or even better, be settable)
  clean up this file, maybe make a dedicated cinspector package for such things
  document lessons learned about NSBrowser and NSTableView for next time

Bugs:
  - when selecting a non-item in a lower column that was just being
  displayed (in the NSBrowser), the tableview isn't cleared and it
  probably should be.

  Possibly a reasonable next thing after that would be to make control-
or alt-double-clicking open new windows with other browsing metaphors
appropriate to the object (like a class heirarchy browser, maybe a
table view for matrices, etc.), we'll see.
  Eventually I'd like to expand the whole inspector functionality to
deal with ObjC things (methods and objects) and C foreign data in
general, but that's further off unless someone wants to take a crack
at it. Once we know we've got a macptr into ObjC we can deal, but some
very carefully written functions need to exist to safely interrogate
a random pointer to make that determination.

Note the variable name convention in this file: "cinspector" refers to
a cocoa-inspector object containing a set of objects being displayed,
while "inspector" refers to an inspector object from the :inspector
package, which are used for command-line inspecting.

|#


#|
I'd rather set up this file to be
- in-package cl-user
- require of some things
- a package definition for this code that brings in inspector::this-and-that and ccl::objc-stuff
- a couple of load-file forms that populate the new package and have the bulk of the following code
|#

;;; This is useful when @ won't work, dynamically creating a NSString
;;; pointer from a string.

(defun nsstringptr (string)
  (ccl::objc-constant-string-nsstringptr
   (ccl::ns-constant-string string)))

#+old
(defmacro handler-case-for-cocoa (id form)
  (declare (ignorable id))
  `(handler-case
    ,form
    (condition (c)
      (declare (ignorable c))
      #+ignore
      (format t "~s: Trapping condition: ~a" ,id c)
      nil)))

; for now this will map windows to objects -- the windows are pretty big,
; though, so it would be nice to extend them so the list of inspected objects
; is switchable in a single window (shouldn't be too hard once basic functionality
; is slapped down)
(defparameter *cocoa-inspector-nswindows-table* (make-hash-table :test 'eql))

;;; this is what a window should map to - an object that manages all
;;; the data a window might be displaying
(defclass cocoa-inspector ()
  ((object-vector :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor object-vector)
   (inspector-vector :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor inspector-vector)
   (focal-point :initform 0 :accessor focal-point)))

;;; note that ELT pays attention to the fill pointer, while AREF doesn't!
(defmethod object ((cinspector cocoa-inspector))
  (elt (object-vector cinspector) (focal-point cinspector)))
(defmethod nth-object ((cinspector cocoa-inspector) n)
  (elt (object-vector cinspector) n))
(defmethod inspector ((cinspector cocoa-inspector))
  ;; This can return nil.
  (let* ((i (focal-point cinspector))
         (v (inspector-vector cinspector))
         (n (length v)))
    (if (< i n)
      (aref v i))))
(defmethod nth-inspector ((cinspector cocoa-inspector) n)
  (elt (inspector-vector cinspector) n))
(defmethod push-object (object (cinspector cocoa-inspector))
  (let ((inspector (inspector::make-inspector object)))
    (vector-push-extend object (object-vector cinspector))
    (vector-push-extend inspector (inspector-vector cinspector))
    (inspector::update-line-count inspector))
  #+ignore
  (format t "    after push-object, fill pointers = ~a ~a~%"
	  (fill-pointer (object-vector cinspector)) (fill-pointer (inspector-vector cinspector)))
  object)
(defmethod (setf max-column) (value (cinspector cocoa-inspector))
  (when (and (numberp value) (<= 0 value (1- (fill-pointer (object-vector cinspector)))))
    (setf ; new fill-pointers are just outside of the valid bounds
          (fill-pointer (object-vector cinspector)) (1+ value)
	  (fill-pointer (inspector-vector cinspector)) (1+ value)
	  ; new focal point is either what it was before, or the new max column if that's smaller
	  (focal-point cinspector) (min value (focal-point cinspector)))
    #+ignore
    (format t "  after (setf max-column), fill pointers = ~a ~a~%"
	    (fill-pointer (object-vector cinspector)) (fill-pointer (inspector-vector cinspector)))
    value))

;; In the browser view, we'll find the element for some column
;; and consider whether any of its components merit further inspection
;; and, if so, which ones
(defmethod leaf-node-p ((thing t)) nil)
(defmethod leaf-node-p ((thing (eql t))) t)
(defmethod leaf-node-p ((thing null)) t)
(defmethod leaf-node-p ((thing number)) t)
(defmethod leaf-node-p ((thing string)) t)
(defmethod leaf-node-p ((thing inspector::unbound-marker)) t)
(defmethod leaf-field-p ((thing t) n)
  (declare (ignore n))
  nil) ; for a non-leaf node, all fields are futher probable by default
(defmethod leaf-field-p ((thing symbol) n)
  (when (and (keywordp thing) (= n 4)) t))

; whatever is currently the selected object in the inspector, get its
; properties and values for the tableView and print them to a string
(defun focus-nth-line (cinspector n)
  (let* ((inspector (inspector cinspector))
	 (*print-circle* t)
	 (output-stream (make-string-output-stream)))
    (inspector::prin1-line-n inspector output-stream n)
    (get-output-stream-string output-stream)))
(defun nth-object-nth-line (cinspector obj-n line-n)
  (let* ((inspector (nth-inspector cinspector obj-n))
	 (*print-circle* t)
	 (output-stream (make-string-output-stream)))
    (inspector::prin1-line-n inspector output-stream line-n)
    (get-output-stream-string output-stream)))
(defun focus-nth-property (cinspector n)
  (let ((inspector (inspector cinspector)))
    (multiple-value-bind (value label type) (inspector::line-n inspector n)
      (declare (ignore value type))
      (if label
	  (format nil "~a" label)
	""))))
(defun focus-nth-value (cinspector n)
  (let* ((inspector (inspector cinspector))
	 (*print-circle* t)
	 (output-stream (make-string-output-stream))
	 (*package* (find-package :cl-user)))
    (multiple-value-bind (value label type) (inspector::line-n inspector n)
      (declare (ignore label type))
      (format output-stream "~s" value))
    (get-output-stream-string output-stream)))
(defun nth-object-nth-value (cinspector obj-n line-n)
  (let ((inspector (nth-inspector cinspector obj-n)))
    (multiple-value-bind (value label type) (inspector::line-n inspector line-n)
      (declare (ignore label type))
      value)))
(defun (setf focus-nth-value) (value cinspector n)
  (let ((inspector (inspector cinspector)))
    (setf (inspector::line-n inspector n) value)))
(defun focus-nth-value-editable (cinspector n)
  (let ((inspector (inspector cinspector)))
    (multiple-value-bind (value label type) (inspector::line-n inspector n)
      (declare (ignore value))
      (and (or (null type)
	       (eq :normal type)
	       (eq :colon type))
	   (editable-field-p (object cinspector) n label)))))
(defun nth-object-nth-value-editable (cinspector obj-n line-n)
  (let ((inspector (nth-inspector cinspector obj-n)))
    (multiple-value-bind (value label type) (inspector::line-n inspector line-n)
      (declare (ignore value))
      (and (or (null type)
	       (eq :normal type)
	       (eq :colon type))
	   (editable-field-p (nth-object cinspector obj-n) line-n label)))))
;; for now most of these will assume that field numbers are good enough,
;; certain things have inspector fields that move around (like symbols)
;; and can be dealt with on a case by case basis, but that's the reason
;; for passing in the label along with the field number
(defmethod editable-field-p ((thing t) n label)
  (declare (ignore n label))
  t)
;; for lists field 4 is length, could cause a change but inspector doesn't just handle it
;; and at the moment I haven't started thinking of a framework for allowing such extensions
(defmethod editable-field-p ((thing list) n label)
  (declare (ignore label))
  (/= n 4))

#|
I think most of the following should be pretty straightforward for
most utilities meant to run under openmcl: A NIB file, some delegates
and data sources, and some specialized callback functions for talking
with the ObjC world, and some standard code for keeping track of the
appropriate windows.  -hel
|#

; When loading a NIB file with an NSWindowController, DON'T omit the .nib extension
; if you're calling initWithWindowNibPath:owner: (even though the documentation says you should!)
#+ignore
(defparameter *default-inspector-nib-pathname* #p"CCL:OpenMCL.app;Contents;Resources;English.lproj;OpenmclInspector.nib")
; When loading it with a custom WindowController and initWithWindowNibName:, just the main file name
(defparameter *default-inspector-nib-pathname* #p"OpenmclInspector")

;; Q: Is this subclass of NSBrowser enabling the doubleAction? I added it expecting to have to
;; specialize mouseDown (or whatever) to track double-clicking, but it just started working.
(defclass inspector-ns-browser (ns:ns-browser) ; just to specialize mousing, not add slots
    ()
  (:metaclass ns:+ns-object))

(defclass inspector-window-controller (ns:ns-window-controller)
    ((inspector-browser :foreign-type :id :reader inspector-browser))
  (:metaclass ns:+ns-object))

(defclass inspector-browser-delegate (ns:ns-object)
    ((inspector-table-view :foreign-type :id :reader inspector-table-view)
     (inspector-window :foreign-type :id :reader inspector-window))
  (:metaclass ns:+ns-object))

; why is the order of these two slots important?
; I get a segfault selecting the browser when they're in window/browser order after doing modifications in the table.
(defclass inspector-table-view-data-source (ns:ns-object)
    ((inspector-browser :foreign-type :id :reader inspector-browser)
     (inspector-window :foreign-type :id :reader inspector-window))
  (:metaclass ns:+ns-object))

(defclass inspector-table-view-delegate (ns:ns-object)
    ((inspector-window :foreign-type :id :reader inspector-window))
  (:metaclass ns:+ns-object))  


;;; is there some reason this is called before the cell is actually
;;; selected? In any case, when a non-leaf cell is selected, this
;;; function is called first for the new column, so it has to push the
;;; new element into the cinspector -- what the browserAction will be
;;; left doing it remains to be seen. The only other time this is
;;; called AFAICT is when loadColumnZero or reloadColumn is called
(objc:defmethod (#/browser:numberOfRowsInColumn: :<NSI>nteger)
    ((self inspector-browser-delegate)
     browser
     (column :<NSI>nteger))
  (or (let* ((cinspector (gethash (inspector-window self) *cocoa-inspector-nswindows-table*))
             (selected-column (#/selectedColumn browser)) ; probably always (1- column), when a column is selected
             (cinspector-column (1- selected-column)) ; 2nd column of nsbrowser <-> 1st column of cinspector
             (row (#/selectedRowInColumn: browser selected-column)))
        #+ignore
        (format t "getting length of column ~d based on row ~d in column ~d~%" column row selected-column)
        (cond ((not cinspector) 0)
              ((= column 0) 1)          ; just displaying the printed representaiton of the top inspected object
              ((= selected-column 0)    ; selected the printed rep of the inspected object (column should = 1)
               (setf (max-column cinspector) 0) ; crop object-vector in cinspector
               (let ((inspector (nth-inspector cinspector 0))) ; inspector for top object
                 (inspector::inspector-line-count inspector)))
              ((>= selected-column 1)   ; (-1 is the N/A column)
               (setf (max-column cinspector) cinspector-column) ; crop object-vector in cinspector
               (push-object (nth-object-nth-value cinspector cinspector-column row) cinspector)
               (let ((inspector (nth-inspector cinspector (1+ cinspector-column)))) ; inspector for object just pushed
                 (inspector::inspector-line-count inspector)))))
      0))

#|
;; temporarily saved in case the above fails horribly
    (if cinspector
	(handler-case
	 (progn (when (<= 0 selected-column) ; -1 is sort of the N/A column
		  (setf (max-column cinspector) selected-column)
		  (push-object (nth-object-nth-value cinspector selected-column row) cinspector))
		(let ((inspector (nth-inspector cinspector column)))
		  (inspector::inspector-line-count inspector)))
	 (condition () 0))
      0)))
|#

;; In the following method defn this is unnecessary, the Browser can tell this for itself
;; [cell "setLoaded:" :<BOOL> #$YES]
(objc:defmethod (#/browser:willDisplayCell:atRow:column: :void)
    ((self inspector-browser-delegate)
     browser
     cell
     (row :<NSI>nteger)
     (column :<NSI>nteger))
  (declare (ignorable browser column))
  (let ((cinspector (gethash (inspector-window self) *cocoa-inspector-nswindows-table*))
        (cinspector-column (1- column))) ; 2nd column of nsbrowser <-> 1st column of cinspector
    #+ignore
    (format t "asking for value for column ~a, row ~a~%" column row)
    (cond ((not cinspector) nil)
          ((= column 0)
           (#/setStringValue: cell  (nsstringptr (format nil "~s" (nth-object cinspector 0))))
           (#/setLeaf: cell nil))
          (t
           ;; when switching between widgets to the browser, we can
           ;; have reloaded a column and need to drill down a row
           ;; from where we are at the moment
           (#/setStringValue: cell  (nsstringptr (nth-object-nth-line cinspector cinspector-column row)))
           ;; leaf-p should really consider the type of the object in
           ;; question (eventually taking into account whether we're
           ;; browsing the class heirarchy or into objc or whatever)
           (#/setLeaf: cell (or (leaf-node-p (nth-object cinspector cinspector-column)) ; i.e. no fields drill down
                                (leaf-field-p (nth-object cinspector cinspector-column) row)
                                ;; for now...
                                (= row 0)
                                (not (nth-object-nth-value-editable cinspector cinspector-column row))))))))

;;; when all is said and done and once the cinspector is properly
;;; populated, the selected object in the browser's nth column is
;;; actually the object in the cinspector's nth column (i.e. because
;;; the selected object is displayed in the next browser column over,
;;; and the cinspector and nsbrowser have a 1-off discrepancy, they
;;; cancel out) -- just a note to make the difference between these
;;; next two functions and the previous two functions

;;; change the focus of the the table view to be the selected object
(objc:defmethod (#/browserAction: :void)
    ((self inspector-browser-delegate)
     sender); don't know why I'd want to, but could use a separate IBTarget class
  #+ignore (format t "browserAction~%")
  (let* ((cinspector (gethash (inspector-window self) *cocoa-inspector-nswindows-table*))
         (column (#/selectedColumn sender)))
    (when (<= 0 column)
      (setf (focal-point cinspector) column)
      (#/reloadData (inspector-table-view self))
      #+ignore
      (format t "      responding to selection in column ~d~%" column))))

;; open a new inspector on the selected object
(objc:defmethod (#/browserDoubleAction: :void)
    ((self inspector-browser-delegate)
     sender)
  #+ignore (format t "browserDoubleAction~%")
  (let* ((cinspector (gethash (inspector-window self) *cocoa-inspector-nswindows-table*))
         (column (#/selectedColumn sender)))
    (when (< -1 column (length (object-vector cinspector)))
      ;; this seems to work, but I'm not really paying attention to
      ;; thread stuff...
      (cinspect (nth-object cinspector column)))))

(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger)
    ((self inspector-table-view-data-source)
     table-view)
  (declare (ignore table-view))
  
  (or (let ((cinspector (gethash (inspector-window self) *cocoa-inspector-nswindows-table*)))
        (if cinspector
          (let ((inspector (inspector cinspector)))
            (if inspector
              (inspector::inspector-line-count inspector)
              0))))
      0))

(objc:defmethod #/tableView:objectValueForTableColumn:row:
    ((self inspector-table-view-data-source)
     table-view
     table-column
     (row :<NSI>nteger))
  (declare (ignore table-view))
  (let ((cinspector (gethash (inspector-window self) *cocoa-inspector-nswindows-table*)))
    (cond ((not cinspector)
	   #@"")
	  ((#/isEqual: (#/identifier table-column) #@"property")
	   (nsstringptr (focus-nth-property cinspector row)))
	  ((#/isEqual: (#/identifier table-column) #@"value")
	   (nsstringptr (focus-nth-value cinspector row))))))

;; I'm hoping that the delegate will prevent this from being called willy-nilly
(objc:defmethod (#/tableView:setObjectValue:forTableColumn:row: :void)
    ((self inspector-table-view-data-source)
     table-view object table-column (row :<NSI>nteger))
  (declare (ignore table-column))
   (let ((cinspector (gethash (inspector-window self) *cocoa-inspector-nswindows-table*)))
     ;; without any formatters, object appears to be an NSCFString
     ;; also note we should probably save the original value (including unboundness etc)
     ;; first so that we can return to it in the event of any error
     ;; plus we should avoid doing anything if the original string and the new string are equal
     (when cinspector
       (setf (focus-nth-value cinspector row)
	     (let ((*package* (find-package :cl-user)))
	       ;; with-autorelease-pool could possibly be needed to
	       ;; autorelease the cString we're handling (I think)
	       (eval (read-from-string (lisp-string-from-nsstring object)))))
       (#/reloadData table-view) ; really could just reload that one cell, but don't know how...
       ;; changing the focused object may effect the browser's path,
       ;; reload its column and keep the cinspector consistent Here we
       ;; have to make sure that the column we're reloading and the
       ;; column after both have values to display, for when
       ;; reloadColumn: invokes browser:willDisplayCell:atRow:column:
       (#/reloadColumn: (inspector-browser self) (focal-point cinspector))
       ;; [inspector-browser "scrollColumnToVisible:" :int (focal-point cinspector)] ; maybe need this, too
       )))

;;; In the table view, the properties are not editable, but the
;;; values (if editable) allow lisp forms to be entered that are
;;; read and evaluated to determine the new property value.
(objc:defmethod (#/tableView:shouldEditTableColumn:row: :<BOOL>)
    ((self inspector-table-view-delegate)
     table-view table-column (row :<NSI>nteger))
  (declare (ignore table-view))
  (let ((cinspector (gethash (inspector-window self) *cocoa-inspector-nswindows-table*)))
    (and cinspector
         (#/isEqual: (#/identifier table-column) #@"value")
         (/= row 0)                     ; in practice the reference to
                                        ; the object isn't editable, and
                                        ; the GUI semantics aren't clear anyway,
                                        ; possibly there will come a
                                        ; time when I put row 0 in the
                                        ; table title, but I need to
                                        ; maintain the 0-indexed
                                        ; focus-nth-whatever API here
                                        ; and elsewhere if I do that
         (focus-nth-value-editable cinspector row))))

;; the inspectorwindowcontroller is set up as the delegate of the window...
;; we now eliminate the dangling pointer to the window from the hash table
(objc:defmethod (#/windowWillClose: :void)
    ((self inspector-window-controller) notification)
  (let ((nswindow (#/object notification)))
    (remhash nswindow *cocoa-inspector-nswindows-table*)))

;;; hopefully a generally useful function
(defun load-windowcontroller-from-nib (wc-classname nib-pathname)
  "Takes a NIB name and returns a new window controller"
  (with-autorelease-pool
      (make-instance 
       wc-classname
       :with-window-nib-name (nsstringptr (namestring nib-pathname)))))

;;; make a new inspector window from the nib file, and hash the window's
;;; browser and tableview to the object
(defun cinspect (object)
  (with-autorelease-pool
      (let* ((windowcontroller (load-windowcontroller-from-nib 'inspector-window-controller *default-inspector-nib-pathname*))
	     (window (#/window windowcontroller))
	     (cinspector (make-instance 'cocoa-inspector)))
	;; set up the window's initial "focused" object -- this may change as
	;; different parts of the inspector are clicked on, and actually we
	;; probably want to track more information than that associated with the
	;; window, so probably this will eventually be hashed to something like
	;; an inspector for the object or an even bigger wrapper
	(setf (gethash window *cocoa-inspector-nswindows-table*) cinspector)
	(push-object object cinspector)
	;; is this working? it isn't breaking, but double-clicking is
	;; being handled as two single actions
	(let* ((browser (inspector-browser windowcontroller)))
          (#/setColumnResizingType: browser #$NSBrowserUserColumnResizing)
          (#/setPrefersAllColumnUserResizing: browser nil)
	  (#/setDoubleAction: browser (@selector #/browserDoubleAction:))
	  (#/setIgnoresMultiClick: browser t))
	(#/showWindow: windowcontroller window)
	window)))

;;; Make INSPECT call CINSPECT.
(setq inspector::*default-inspector-ui-creation-function* 'cinspect)
