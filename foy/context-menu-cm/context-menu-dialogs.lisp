;;; ----------------------------------------------------------------------------
;;;
;;;      context-menu-dialogs.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      Utilities and dialogs for the Context-Menu tool set.
;;;
;;;      The API for writing new tools is described in the accompanying NewTools file.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      9/14/9  First cut
;;;
;;; ----------------------------------------------------------------------------

(defpackage "CONTEXT-MENU" (:nicknames "CMENU") (:use :cl :ccl))
(in-package "CONTEXT-MENU")

(export '(notify window-with-path active-hemlock-window window-path echo-msg))

(defparameter *clozure-jpg* (merge-pathnames ";Clozure.jpg" cl-user::*context-menu-directory*))
(defparameter *graphic-p* t "To use, or not to use the Clozure graphic.")


(defun active-hemlock-window ()
  "Return the active hemlock-frame."
  (gui::first-window-satisfying-predicate 
   #'(lambda (w)
       (and (typep w 'gui::hemlock-frame)
            (not (typep w 'gui::hemlock-listener-frame))
            (#/isKeyWindow w)))))

(defun window-path (w)
  "Return the window's path."
  (let* ((pane (slot-value w 'gui::pane))
         (hemlock-view (when pane (gui::text-pane-hemlock-view pane)))
         (buffer (when hemlock-view (hi::hemlock-view-buffer hemlock-view))))
    (when buffer (hi::buffer-pathname buffer))))

;;; This includes a work-around for what appears to be a bug in the hemlock-frame
;;; #/close method.  After a #/close, the window remains on the (#/orderedWindows *NSApp*)
;;; list, but (hi::buffer-document buffer) in NIL.  Therefore the extra tests:
(defun window-with-path (path)
  "If a window with PATH is open, return it."
  (gui::first-window-satisfying-predicate 
   #'(lambda (w)
       (when (and (typep w 'gui::hemlock-frame)
                  (not (typep w 'gui::hemlock-listener-frame)))
         (let* ((pane (slot-value w 'gui::pane))
                (text-view (gui::text-pane-text-view pane))
                (buffer (gui::hemlock-buffer text-view))
                (document (when buffer (hi::buffer-document buffer)))
                (p (hi::buffer-pathname buffer)))
           (when (and document p) (string-equal path p)))))))

(defun echo-msg (string &rest args)
  (let* ((window (cmenu:active-hemlock-window))
         (hemlock-view (when window (gui::hemlock-view window))))
    (when hemlock-view
      (let ((hi::*current-view* hemlock-view))
        (hi::message string args)))))

(defun notify (message &rest args)
  "FYI"
  (let ((message-string (apply #'format nil message args)))
    (if *graphic-p*
      (open-notification-dialog message-string)
      (gui::alert-window :title "Notification" :message message-string))))

(defparameter *notify-dialog* nil "The notification-dialog instance.")

;;; ----------------------------------------------------------------------------
;;;
(defclass NOTIFICATION-DIALOG (ns:ns-window)
  ((message-field :initform nil :accessor nd-message-field)
   (okay-button :initform nil :accessor nd-okay-button))
  (:documentation "A dialog for displaying messages.")
  (:metaclass ns:+ns-object))

(objc:defmethod (#/okayAction: :void) ((d notification-dialog) (sender :id))
  (declare (ignore sender))
  (#/stopModalWithCode: ccl::*nsapp* 0))

(defun open-notification-dialog (message)
  "Open the notification-dialog and display MESSAGE."
  (let ((message-string (#/initWithString:attributes: (#/alloc ns:ns-attributed-string) 
                                                      (ccl::%make-nsstring message)
                                                      cmenu::*tool-doc-dictionary*)))
    (cond (*notify-dialog*
           (#/setStringValue: (nd-message-field *notify-dialog*) message-string)
           (#/makeKeyAndOrderFront: *notify-dialog* nil)
           (#/runModalForWindow: ccl::*nsapp* *notify-dialog*)
           (#/close *notify-dialog*))
          (t
           (let ((dialog (#/alloc notification-dialog)))
             (setq *notify-dialog* dialog)
             (ns:with-ns-rect (r 10 300 400 127)
               (#/initWithContentRect:styleMask:backing:defer: 
                dialog
                r
                #$NSTitledWindowMask 
                #$NSBackingStoreBuffered
                #$NO))
             (dolist (item (get-notify-items dialog))
               (#/addSubview: (#/contentView dialog) item))
             (#/setTitle: dialog #@"Notification")
             (#/setReleasedWhenClosed: dialog nil)
             (#/setDefaultButtonCell: dialog (nd-okay-button dialog))
             (#/setStringValue: (nd-message-field dialog) message-string)
             (#/center dialog)
             (#/makeKeyAndOrderFront: dialog nil)
             (#/runModalForWindow: ccl::*nsapp* dialog)
             (#/close dialog))))))

#|
(open-notification-dialog "foobear")
|#

(defmethod get-notify-items ((d notification-dialog))
  (append
   (make-notify-graphic)
   ;; (make-notify-prompt)
   (make-notify-message d)
   (make-notify-button d)))

(defun make-notify-graphic ()
  "Create the Clozure graphic."
  (when (probe-file *clozure-jpg*)
    (let ((image (#/alloc ns:ns-image))
          (image-view (#/alloc ns:ns-image-view)))
      (ns:with-ns-rect (frame 0 0 108 127)
        (#/initWithFrame: image-view frame))
      (#/setImageScaling: image-view #$NSScaleToFit)
      (#/initWithContentsOfFile: image (ccl::%make-nsstring (namestring *clozure-jpg*)))
      (#/setImage: image-view image)
      (list image-view))))

(defun make-notify-prompt ()
  "Create the prompt text-field."
  (list
   (let* ((string (#/initWithString:attributes: 
                   (#/alloc ns:ns-attributed-string) 
                   #@"Notification"
                   cmenu::*tool-label-dictionary*))
          (title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame 120 90 150 32)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     (#/setStringValue: title string)
     title)))

(defun make-notify-message (dialog)
  "Create the documentation text-view."
  (list
   (let ((field (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame 120 50 270 60)
       (#/initWithFrame: field frame))
     (#/setEditable: field nil)
     (#/setDrawsBackground: field nil)
     (#/setBordered: field nil)
     (setf (nd-message-field dialog) field))))

(defun make-notify-button (dialog)
  "Construct the button."
  (list
   (let ((button (#/alloc ns:ns-button)))
     (ns:with-ns-rect (frame 310 10 80 32)
       (#/initWithFrame: button frame))
     (#/setButtonType: button #$NSMomentaryPushInButton)
     (#/setBezelStyle: button #$NSRoundedBezelStyle)
     (#/setTitle: button #@"Okay")
     (#/setTarget: button dialog)
     (#/setAction: button (ccl::@selector "okayAction:"))
     (setf (nd-okay-button dialog) button))))





