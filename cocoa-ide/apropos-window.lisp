;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

(defclass package-combo-box (ns:ns-combo-box)
  ((packages :initform nil))
  (:metaclass ns:+ns-object))

;;; This is a premature optimization.  Instead of calling LIST-ALL-PACKAGES
;;; so frequently, just get a fresh copy when the user clicks in the
;;; combo box.
(objc:defmethod (#/becomeFirstResponder :<BOOL>) ((self package-combo-box))
  (with-slots (packages) self
    (setf packages (coerce (list-all-packages) 'vector))
    (setf packages (sort packages #'string-lessp :key #'package-name)))
  (call-next-method))

(defclass apropos-window-controller (ns:ns-window-controller)
  ((apropos-array :foreign-type :id :initform +null-ptr+
		  :reader apropos-array
		  :documentation "Bound to NSArrayController in nib file")
   (array-controller :foreign-type :id :accessor array-controller)
   (combo-box :foreign-type :id :accessor combo-box)
   (table-view :foreign-type :id :accessor table-view)
   (text-view :foreign-type :id :accessor text-view)
   (external-symbols-checkbox :foreign-type :id
			      :accessor external-symbols-checkbox)
   (shows-external-symbols :initform nil)
   (symbol-list :initform nil)
   (package :initform nil)
   (input :initform nil)
   (previous-input :initform nil :accessor previous-input
		   :documentation "Last string entered"))
  (:metaclass ns:+ns-object))

(defmethod (setf apropos-array) (value (self apropos-window-controller))
  (with-slots (apropos-array) self
    (unless (eql value apropos-array)
      (#/release apropos-array)
      (setf apropos-array (#/retain value)))))

;;; Diasable automatic KVO notifications, since having our class swizzled
;;; out from underneath us confuses CLOS.  (Leopard doesn't hose us,
;;; and we can use automatic KVO notifications there.)
(objc:defmethod (#/automaticallyNotifiesObserversForKey: :<BOOL>) ((self +apropos-window-controller)
                                                                  key)
  (declare (ignore key))
  nil)

(objc:defmethod (#/awakeFromNib :void) ((self apropos-window-controller))
  (with-slots (table-view text-view) self
    (#/setString: text-view #@"")
    (#/setDelegate: table-view self)
    (#/setDoubleAction: table-view (@selector #/definitionForSelectedSymbol:))))

(objc:defmethod #/init ((self apropos-window-controller))
  (prog1
      (#/initWithWindowNibName: self #@"apropos")
    (#/setShouldCascadeWindows: self nil)
    (#/setWindowFrameAutosaveName: self #@"apropos panel")
    (setf (apropos-array self) (#/array ns:ns-mutable-array))))

(objc:defmethod (#/dealloc :void) ((self apropos-window-controller))
  (#/release (slot-value self 'apropos-array))
  (call-next-method))

(objc:defmethod (#/toggleShowsExternalSymbols: :void)
    ((self apropos-window-controller) sender)
  (declare (ignore sender))
  (with-slots (shows-external-symbols) self
    (setf shows-external-symbols (not shows-external-symbols))
    (update-symbol-list self)
    (update-apropos-array self)))

(objc:defmethod (#/setPackage: :void) ((self apropos-window-controller)
				       sender)
  (with-slots (combo-box package) self
    (assert (eql sender combo-box))
    (with-slots (packages) sender
      (let ((index (#/indexOfSelectedItem sender)))
	(if (minusp index)
	  (setf package nil)		;search all packages
	  (setf package (svref packages index))))))
  (update-symbol-list self)
  (update-apropos-array self))

(defmethod update-symbol-list ((self apropos-window-controller))
  (with-slots (input package shows-external-symbols symbol-list) self
    (when (plusp (length input))
      (setf symbol-list nil)
      (if package
	(if shows-external-symbols
	  (do-external-symbols (sym package)
	    (when (ccl::%apropos-substring-p input (symbol-name sym))
	      (push sym symbol-list)))
	  (do-symbols (sym package)
	    (when (ccl::%apropos-substring-p input (symbol-name sym))
	      (push sym symbol-list))))
	(if shows-external-symbols
	  (dolist (p (list-all-packages))
	    (do-external-symbols (sym p)
	      (when (ccl::%apropos-substring-p input (symbol-name sym))
		(push sym symbol-list))))
	  (do-all-symbols (sym)
	    (when (ccl::%apropos-substring-p input (symbol-name sym))
	      (push sym symbol-list)))))
      (setf symbol-list (sort symbol-list #'string-lessp)))))

(defmethod update-apropos-array ((self apropos-window-controller))
  (with-slots (input apropos-array symbol-list package) self
    (when (plusp (length input))
      (let ((new-array (#/array ns:ns-mutable-array))
	    (*package* (or package (find-package "COMMON-LISP-USER")))
	    (n 0))
	(dolist (s symbol-list)
	  (#/addObject: new-array (#/dictionaryWithObjectsAndKeys:
				   ns:ns-dictionary
				   (#/autorelease
				    (%make-nsstring
				     (prin1-to-string s)))
				   #@"symbol"
				   (#/numberWithInt: ns:ns-number n)
				   #@"index"
				   (#/autorelease
				    (%make-nsstring
				     (inspector::symbol-type-line s)))
				   #@"kind"
				   +null-ptr+))
	  (incf n))
	(#/willChangeValueForKey: self #@"aproposArray")
	(setf apropos-array new-array)
	(#/didChangeValueForKey: self #@"aproposArray")))))

(objc:defmethod (#/apropos: :void) ((self apropos-window-controller) sender)
  (let* ((input (lisp-string-from-nsstring (#/stringValue sender))))
    (when (and (plusp (length input))
	       (not (string-equal input (previous-input self))))
      (setf (slot-value self 'input) input)
      (setf (previous-input self) input)
      (update-symbol-list self)
      (update-apropos-array self))))

(objc:defmethod (#/inspectSelectedSymbol: :void) ((self apropos-window-controller) sender)
  (declare (ignorable sender))
  (let* ((row (#/clickedRow (table-view self))))
    (unless (minusp row)
      (with-slots (array-controller symbol-list) self
	(let* ((number (#/valueForKeyPath: array-controller #@"selection.index"))
	       (i (#/intValue number))
	       (sym (elt symbol-list i)))
	  (inspect sym))))))

(objc:defmethod (#/definitionForSelectedSymbol: :void) ((self apropos-window-controller) sender)
  (declare (ignorable sender))
  (let* ((row (#/clickedRow (table-view self))))
    (unless (minusp row)
      (with-slots (array-controller symbol-list) self
	(let* ((number (#/valueForKeyPath: array-controller #@"selection.index"))
	       (i (#/intValue number))
	       (sym (elt symbol-list i)))
	  (hemlock::edit-definition sym))))))

;;; Data source methods for package combo box

(objc:defmethod (#/numberOfItemsInComboBox: :<NSI>nteger) ((self apropos-window-controller)
						   combo-box)
  (declare (ignore combo-box))
  (length (list-all-packages)))

(objc:defmethod #/comboBox:objectValueForItemAtIndex: ((self apropos-window-controller)
						       combo-box
						       (index :<NSI>nteger))
  (with-slots (packages) combo-box
    (let* ((pkg-name (package-name (svref packages index))))
      (if pkg-name
	(#/autorelease (%make-nsstring pkg-name))
	+null-ptr+))))

(objc:defmethod #/comboBox:completedString: ((self apropos-window-controller)
					     combo-box
					     partial-string)
  (flet ((string-prefix-p (s1 s2)
	   "Is s1 a prefix of s2?"
	   (string-equal s1 s2 :end2 (min (length s1) (length s2)))))
    (with-slots (packages) combo-box
      (let* ((s (lisp-string-from-nsstring partial-string)))
	(dotimes (i (length packages) +null-ptr+)
	  (let ((name (package-name (svref packages i))))
	    (when (string-prefix-p s name)
	      (return (#/autorelease (%make-nsstring name))))))))))

(objc:defmethod (#/comboBox:indexOfItemWithStringValue: :<NSUI>nteger)
    ((self apropos-window-controller)
     combo-box
     string)
  (with-slots (packages) combo-box
    (let* ((s (lisp-string-from-nsstring string)))
      (or (position s packages :test #'(lambda (str pkg)
					 (string-equal str (package-name pkg))))
	  #$NSNotFound))))


;;; Table view delegate methods

(objc:defmethod (#/tableViewSelectionDidChange: :void) ((self apropos-window-controller)
							notification)
  (with-slots (array-controller symbol-list text-view) self
    (let* ((tv (#/object notification))
	   (row (#/selectedRow tv)))
      (unless (minusp row)
	(let* ((number (#/valueForKeyPath:
			array-controller #@"selection.index"))
	       (i (#/intValue number))
	       (sym (elt symbol-list i))
	       (info (make-array '(0) :element-type 'base-char
				 :fill-pointer 0 :adjustable t)))
	  (with-output-to-string (s info)
	    (dolist (doctype '(compiler-macro function method-combination
			       setf structure t type variable))
	      (let ((docstring (documentation sym doctype)))
		(when docstring
		  (format s "~&~a" docstring))
		(when (eq doctype 'function)
		  (format s "~&arglist: ~s" (arglist sym))))))
	  (if (plusp (length info))
	    (#/setString: text-view (#/autorelease (%make-nsstring info)))
	    (#/setString: text-view #@"")))))))


