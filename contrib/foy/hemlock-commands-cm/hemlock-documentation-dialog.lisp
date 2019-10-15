;;;-*-Mode: LISP; Package: HEMLOCK-COMMANDS -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      hemlock-documentation-dialog.lisp
;;;
;;;      copyright © 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      A documentation dialog for Hemlock commands, CL function, symbols, etc.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      8/31/9  version 0.1b1
;;;              First cut.
;;;
;;; ----------------------------------------------------------------------------

(in-package "HEMLOCK-COMMANDS")

(defparameter *doc-dialog* nil)
(defparameter *hemlock-jpg* (merge-pathnames ";Hemlock.jpg" cl-user::*hemlock-commands-directory*))
;;; I don't know the name of the artist who drew this graphic, but it is quite nice.
;;; I also don't know what the copyright issues are, so this will have to be replaced when I get a chance:
(defparameter *graphic-p* nil "To use, or not to use the eye candy.")


;;; ----------------------------------------------------------------------------
;;;
(defclass doc-dialog (ns:ns-window)
  ((name :accessor name)
   (symbol :accessor symbol)
   (name-field :accessor name-field)
   (key-field :accessor key-field)
   (doc-text-view :accessor doc-text-view)
   (hemlock-p :initform nil :accessor hemlock-p)
   (hyperspec-button :accessor hyperspec-button)
   (inspect-button :accessor inspect-button)
   (okay-button :accessor okay-button)
   (source-button :accessor source-button)
   (text-view :accessor text-view))
  (:documentation "A dialog for displaying the documentation of Hemlock Commands, CL function, non-CL functions, etc.")
  (:metaclass ns:+ns-object))

(objc:defmethod (#/closeAction: :void) ((d doc-dialog) (sender :id))
  (declare (ignore sender))
  (#/close d))

(objc:defmethod (#/hyperSpecAction: :void) ((d doc-dialog) (sender :id))
  (declare (ignore sender))
  (when (symbol d) 
    (gui::lookup-hyperspec-symbol (symbol d) (text-view d))))

(objc:defmethod (#/inspectSymbolAction: :void) ((d doc-dialog) (sender :id))
  (declare (ignore sender))
  (when (symbol d)
    (inspect (symbol d))))

;;; Should probably just make Hemlock-Commands require List-Definitions:
#+:list-definitions
(objc:defmethod (#/commandSourceAction: :void) ((d doc-dialog) (sender :id))
  (declare (ignore sender))
  (cond ((hemlock-p d)
         (let* ((search-string (format nil "(defcommand \"~A\"" (name d)))
                (hemlock-src-dir (merge-pathnames "cocoa-ide/hemlock/src/" (native-translated-namestring "ccl:")))
                (files (mapcar #'namestring
                               (remove-if #'(lambda (path)
                                              (string-not-equal (pathname-type path) "lisp"))
                                          (directory (merge-pathnames hemlock-src-dir "*.lisp") :files t :directories nil))))
                (args (cons "-l" (cons search-string files)))
                (source-path (string-trim '(#\newline #\space) (gui::call-grep args))))
           (if (and (stringp source-path) (string-not-equal source-path ""))
             (ldefs:find-and-display-definition (format nil "~S" (name d)) source-path)
             (cmenu:notify (format nil "Could not find: ~S" (name d))))))
        (t
         (hemlock::edit-definition (symbol d)))))

#-:list-definitions
(objc:defmethod (#/commandSourceAction: :void) ((d doc-dialog) (sender :id))
  (declare (ignore sender))
  (cond ((hemlock-p d)
         ;; deactivate the button instead of this?
         (gui::alert-window :title "Notification" :message "Searching for source requires the List-Definitions tool."))
        (t
         (hemlock::edit-definition (symbol d)))))


(defun open-documentation-dialog (name key-or-type doc &key symbol hemlock-p text-view)
  "Open the dialog displaying the documentation for NAME."
  (let* ((name-string (#/initWithString:attributes: (#/alloc ns:ns-attributed-string) 
                                                   (ccl::%make-nsstring 
                                                    (if hemlock-p
                                                      ;; *** ~S
                                                      (string-upcase (format nil "\"~A\"" name))
                                                      (format nil "~A" name)))
                                                   cmenu::*tool-label-dictionary*))
        (key-string (#/initWithString:attributes: (#/alloc ns:ns-attributed-string) 
                                                  (if key-or-type
                                                    (ccl::%make-nsstring key-or-type)
                                                    (ccl::%make-nsstring " "))
                                                  cmenu::*tool-key-dictionary*))
         (inspect-p doc) ; "No documentation found"
         (source-p (when symbol (ccl::%source-files symbol)))
         (hyperspec-p (when (and symbol text-view) (gethash symbol (gui::hyperspec-map-hash text-view)))))
    (cond (*doc-dialog*
           (cond (hemlock-p
                  (setf (hemlock-p *doc-dialog*) t)
                  (#/setTitle: *doc-dialog* #@"Hemlock Command Documentation")
                  (#/setHidden: (inspect-button *doc-dialog*) t)
                  (#/setHidden: (hyperspec-button *doc-dialog*) t))
                 (t
                  (setf (hemlock-p *doc-dialog*) nil)
                  (if source-p
                    (#/setEnabled: (source-button *doc-dialog*) t)
                    (#/setEnabled: (source-button *doc-dialog*) nil))
                  (if inspect-p
                    (#/setEnabled: (inspect-button *doc-dialog*) t)
                    (#/setEnabled: (inspect-button *doc-dialog*) nil))
                  (#/setHidden: (hyperspec-button *doc-dialog*) nil)
                  (#/setHidden: (inspect-button *doc-dialog*) nil)
                  (#/setTitle: *doc-dialog* #@"Documentation")))
           (setf (name *doc-dialog*) name)
           (setf (symbol *doc-dialog*) symbol)
           (setf (text-view *doc-dialog*) text-view)
           (if hyperspec-p 
             (#/setEnabled: (hyperspec-button *doc-dialog*) t)
             (#/setEnabled: (hyperspec-button *doc-dialog*) nil))
           (#/setStringValue: (name-field *doc-dialog*) name-string)
           (#/setStringValue: (key-field *doc-dialog*) key-string)
           (#/setString: (doc-text-view *doc-dialog*) (if doc (ccl::%make-nsstring doc) #@""))
           (#/makeKeyAndOrderFront: *doc-dialog* nil))
          (t
           (let ((dialog (#/alloc doc-dialog)))
             (setq *doc-dialog* dialog)
             (ns:with-ns-rect (r 100 100 (if *graphic-p* 625 475) 230)
               (#/initWithContentRect:styleMask:backing:defer: 
                dialog
                r
                (logior  #$NSTitledWindowMask 
                         #$NSClosableWindowMask  
                         #$NSMiniaturizableWindowMask)
                #$NSBackingStoreBuffered
                #$NO))
             (dolist (item (get-items dialog))
               (#/addSubview: (#/contentView dialog) item))
             (cond (hemlock-p
                  (setf (hemlock-p dialog) t)
                  (#/setTitle: dialog #@"Hemlock Command Documentation")
                  (#/setHidden: (inspect-button dialog) t)
                  (#/setHidden: (hyperspec-button dialog) t))
                 (t
                  (setf (hemlock-p dialog) nil)
                  (if source-p
                    (#/setEnabled: (source-button dialog) t)
                    (#/setEnabled: (source-button dialog) nil))
                  (if inspect-p
                    (#/setEnabled: (inspect-button *doc-dialog*) t)
                    (#/setEnabled: (inspect-button *doc-dialog*) nil))
                  (#/setHidden: (hyperspec-button dialog) nil)
                  (#/setHidden: (inspect-button dialog) nil)
                  (#/setTitle: dialog #@"Documentation")))
             (if hyperspec-p 
               (#/setEnabled: (hyperspec-button *doc-dialog*) t)
               (#/setEnabled: (hyperspec-button *doc-dialog*) nil))
             (#/setReleasedWhenClosed: dialog nil)
             (#/setDefaultButtonCell: dialog (okay-button dialog))
             (#/center dialog)
             (#/setStringValue: (name-field dialog) name-string)
             (#/setStringValue: (key-field dialog) key-string)
             (#/setString: (doc-text-view dialog) (if doc (ccl::%make-nsstring doc) #@""))
             (setf (name dialog) name)
             (setf (symbol dialog) symbol)
             (setf (text-view dialog) text-view)
             (#/makeKeyAndOrderFront: dialog nil))))))

;;; This is a redefintion of the function in cl-documentation-1.lisp
(defun cldoc::display-cl-doc (sym text-view)
  "If there is CCL or MCL doc, use the doc-dialog to display documentation.  Otherwise use the HyperSpec."
  (when (eq (symbol-package sym) (find-package :common-lisp))
    (or (display-ccl-doc sym text-view)
        (display-mcl-doc sym text-view)
        (gui::lookup-hyperspec-symbol sym text-view))))

(defmethod get-items ((d doc-dialog))
  (append
   (when *graphic-p* 
     (make-hemlock-image))
   (make-name-field d)
   (make-key-field d)
   (make-doc-text-view d)
   (make-buttons d)))

(defun make-hemlock-image ()
  "Create the Hemlock graphic.  You can make this go away by set *graphic-p* to nil above."
  (let ((image (#/alloc ns:ns-image))
        (image-view (#/alloc ns:ns-image-view)))
    (ns:with-ns-rect (frame 10 54 141 164)
      (#/initWithFrame: image-view frame))
    (#/initWithContentsOfFile: image (ccl::%make-nsstring (namestring *hemlock-jpg*)))
    (#/setImage: image-view image)
    (list image-view)))

(defun make-name-field (dialog)
  "Create the name text-field."
  (list
   (let* ((title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame (if *graphic-p* 165 15) 178 440 38)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     (#/setStringValue: title #@"")
     (setf (name-field dialog) title))))

(defun make-key-field (dialog)
  "Create the key text-field."
  (list
   (let* ((title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame (if *graphic-p* 165 15) 162 450 16)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     (#/setStringValue: title #@"")
     (setf (key-field dialog) title))))

(defun make-doc-text-view (dialog)
  "Create the documentation text-view."
  (list
   (let* ((scroll-view (#/alloc ns:ns-scroll-view))
          (view (#/init (#/alloc ns:ns-text-view))))
     (ns:with-ns-rect (frame (if *graphic-p* 165 15) 54 460 106)
       (#/initWithFrame: scroll-view frame))
     (ns:with-ns-rect (frame 4 60 445 200)
       (#/initWithFrame: view frame))
     (#/setString: view #@" ")
     (#/setHasVerticalScroller: scroll-view t)
     (#/setHasHorizontalScroller: scroll-view nil)
     (#/setBorderType: scroll-view #$NSBezelBorder)
     (#/setDocumentView: scroll-view view)
     (#/setEditable: view nil)
     (#/setFont: view (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)))
     (#/setTextColor: view cmenu:*dark-turquoise-color*)
     (#/setBackgroundColor: view cmenu:*light-gray-color*)
     (setf (doc-text-view dialog) view)
     scroll-view)))


(defun make-buttons (dialog)
  "Construct the buttons."
  (flet ((make-button (title x-coord y-coord x-dim y-dim action)
           (let ((button (#/alloc ns:ns-button)))
             (ns:with-ns-rect (frame x-coord y-coord x-dim y-dim)
               (#/initWithFrame: button frame))
             (#/setButtonType: button #$NSMomentaryPushInButton)
             ; (#/setImagePosition: button #$NSNoImage)
             (#/setBezelStyle: button #$NSRoundedBezelStyle)
             (#/setTitle: button title)
             (#/setTarget: button dialog)
             (#/setAction: button action)
             button)))
    (list
     (setf (okay-button dialog)
           (make-button #@"Okay" (if *graphic-p* 520 370) 10 80 32
                        (ccl::@selector "closeAction:")))
     (setf (source-button dialog)
           (make-button #@"Source..." (if *graphic-p* 420 270) 10 90 32
                        (ccl::@selector "commandSourceAction:")))
     (setf (inspect-button dialog)
           (make-button #@"Inspect..." (if *graphic-p* 320 170) 10 90 32
                        (ccl::@selector "inspectSymbolAction:")))
     (setf (hyperspec-button dialog)
           (make-button #@"HyperSpec..." (if *graphic-p* 180 30) 10 130 32
                        (ccl::@selector "hyperSpecAction:"))))))






