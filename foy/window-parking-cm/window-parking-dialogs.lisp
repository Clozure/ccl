;;;-*- Mode: Lisp; Package: WINDOW-PARKING -*-

;;; ----------------------------------------------------------------------------
;;; 
;;;      window-parking-dialogs.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      Dialogs for defining and deleting parking spots.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History (most recent edit first)
;;;      9/9/9  first cut
;;;
;;; ----------------------------------------------------------------------------

(in-package "WINDOW-PARKING")

(defparameter *dps-dialog* nil "The define-parking-spot-dialog instance.")
(defparameter *del-dialog* nil "The delete-parking-spot-dialog instance.")


;;; ----------------------------------------------------------------------------
;;;
(defclass DEFINE-PARKING-SPOT-DIALOG (ns:ns-window)
  ((path :initform nil :accessor psd-path)
   (okay-button :initform nil :accessor psd-okay-button)
   (function-key-buttons :initform nil :accessor psd-function-key-buttons)
   (function-key-matrix :initform nil :accessor psd-function-key-matrix))
  (:documentation "A dialog for associating a window size and position with a function key.")
  (:metaclass ns:+ns-object))

(defmethod selected-function-key ((d define-parking-spot-dialog))
  (read-from-string (ccl::lisp-string-from-nsstring 
                     (#/title (#/selectedCell (psd-function-key-matrix d))))))

(objc:defmethod (#/okayAction: :void) ((d define-parking-spot-dialog) (sender :id))
  (declare (ignore sender))
  (#/stopModalWithCode: ccl::*nsapp* 0))

(objc:defmethod (#/cancelAction: :void) ((d define-parking-spot-dialog) (sender :id))
  (declare (ignore sender))
  (#/stopModalWithCode: ccl::*nsapp* 1))

(defun open-define-parking-spot-dialog (path &optional (function-key 1))
  "Open the define-parking-spot-dialog for PATH."
  (let* ((path-string (#/initWithString:attributes: (#/alloc ns:ns-attributed-string) 
                                                    (ccl::%make-nsstring 
                                                     (format nil "~A" path))
                                                    cmenu::*tool-key-dictionary*)))
    (flet ((selectFunctionKey (num)
             (dolist (button (psd-function-key-buttons *dps-dialog*))
               (let ((key (read-from-string (ccl::lisp-string-from-nsstring (#/title button)))))
                 (when (= num key)
                   (#/selectCell: (psd-function-key-matrix *dps-dialog*) button)
                   (return))))))
      (cond (*dps-dialog*
             (#/setStringValue: (psd-path *dps-dialog*) path-string)
             (selectFunctionKey function-key)
             (#/makeKeyAndOrderFront: *dps-dialog* nil)
             (let ((ret (#/runModalForWindow: ccl::*nsapp* *dps-dialog*)))
               (#/close *dps-dialog*)
               (when (zerop ret) (selected-function-key *dps-dialog*))))
            (t
             (let ((dialog (#/alloc define-parking-spot-dialog)))
               (setq *dps-dialog* dialog)
               (ns:with-ns-rect (r 10 300 600 140)
                 (#/initWithContentRect:styleMask:backing:defer: 
                  dialog
                  r
                  #$NSTitledWindowMask 
                  #$NSBackingStoreBuffered
                  #$NO))
               (dolist (item (get-items dialog))
                 (#/addSubview: (#/contentView dialog) item))
               (#/setTitle: dialog #@"Define Parking Spot")
               (#/setReleasedWhenClosed: dialog nil)
               (#/setDefaultButtonCell: dialog (psd-okay-button dialog))
               (#/center dialog)
               (#/setStringValue: (psd-path dialog) path-string)
               (selectFunctionKey function-key)
               (#/makeKeyAndOrderFront: dialog nil)
               (let ((ret (#/runModalForWindow: ccl::*nsapp* dialog)))
                 (#/close dialog)
                 (when (zerop ret) (selected-function-key dialog)))))))))

(defmethod get-items ((d define-parking-spot-dialog))
  (append
   (make-prompt-field)
   (make-path-field d)
   (make-function-key-items d)
   (make-buttons d)))

(defun make-prompt-field ()
  "Create the prompt text-field."
  (list
   (let* ((string (#/initWithString:attributes: 
                   (#/alloc ns:ns-attributed-string) 
                   #@"Save the front window size, position and function key:"
                   cmenu::*tool-label-dictionary*))
          (title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame 15 90 410 32)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     (#/setStringValue: title string)
     title)))

(defun make-path-field (dialog)
  "Create the path text-field."
  (list
   (setf (psd-path dialog)
         (let* ((string (#/initWithString:attributes: 
                         (#/alloc ns:ns-attributed-string) 
                         #@""
                         cmenu::*tool-doc-dictionary*))
                (title (#/alloc ns:ns-text-field)))
           (ns:with-ns-rect (frame 15 72 580 22)
             (#/initWithFrame: title frame))
           (#/setEditable: title nil)
           (#/setDrawsBackground: title nil)
           (#/setBordered: title nil)
           (#/setStringValue: title string)
           title))))

(defun make-function-key-items (dialog)
  (list
   (let* ((string (#/initWithString:attributes: 
                   (#/alloc ns:ns-attributed-string) 
                   #@"Associated Function Key:"
                   cmenu::*tool-label-dictionary*))
          (title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame 15 40 200 32)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     (#/setStringValue: title string)
     title)
   (setf (psd-function-key-matrix dialog)
         (ns:with-ns-rect (frame 190 40 350 32)
           (let* ((matrix (#/alloc ns:ns-matrix))
                  (prototype (#/init (#/alloc ns:ns-button-cell)))
                  buttons cell-array)
             (#/setTitle: prototype #@"7     ")
             (#/setButtonType: prototype #$NSRadioButton)
             (#/initWithFrame:mode:prototype:numberOfRows:numberOfColumns: 
              matrix frame #$NSRadioModeMatrix prototype 1 7)
             (setq cell-array (#/cells matrix))
             (#/setTitle: (#/objectAtIndex: cell-array 0) #@"1")
             (push (#/objectAtIndex: cell-array 0) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 1) #@"2")
             (push (#/objectAtIndex: cell-array 1) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 2) #@"3")
             (push (#/objectAtIndex: cell-array 2) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 3) #@"4")
             (push (#/objectAtIndex: cell-array 3) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 4) #@"5")
             (push (#/objectAtIndex: cell-array 4) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 5) #@"6")
             (push (#/objectAtIndex: cell-array 5) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 6) #@"7")
             (push (#/objectAtIndex: cell-array 6) buttons)
             (setf (psd-function-key-buttons dialog) buttons)
             matrix)))))

(defun make-buttons (dialog)
  "Construct the buttons."
  (flet ((make-button (title x-coord y-coord x-dim y-dim action)
           (let ((button (#/alloc ns:ns-button)))
             (ns:with-ns-rect (frame x-coord y-coord x-dim y-dim)
               (#/initWithFrame: button frame))
             (#/setButtonType: button #$NSMomentaryPushInButton)
             (#/setBezelStyle: button #$NSRoundedBezelStyle)
             (#/setTitle: button title)
             (#/setTarget: button dialog)
             (#/setAction: button action)
             button)))
    (list
     (setf (psd-okay-button dialog)
           (make-button #@"Okay" 500 10 80 32
                        (ccl::@selector "okayAction:")))
     (make-button #@"Cancel" 400 10 90 32
                  (ccl::@selector "cancelAction:")))))


;;; ----------------------------------------------------------------------------
;;;
(defclass DELETE-PARKING-SPOT-DIALOG (ns:ns-window)
  ((path :initform nil :accessor psd-path)
   (okay-button :initform nil :accessor psd-okay-button)
   (function-key-buttons :initform nil :accessor psd-function-key-buttons)
   (function-key-matrix :initform nil :accessor psd-function-key-matrix))
  (:documentation "A dialog for deleting parking spots.")
  (:metaclass ns:+ns-object))

(defmethod selected-function-key ((d delete-parking-spot-dialog))
  (read-from-string (ccl::lisp-string-from-nsstring 
                     (#/title (#/selectedCell (psd-function-key-matrix d))))))

(objc:defmethod (#/deleteAction: :void) ((d delete-parking-spot-dialog) (sender :id))
  (declare (ignore sender))
  (#/stopModalWithCode: ccl::*nsapp* 0))

(objc:defmethod (#/cancelAction: :void) ((d delete-parking-spot-dialog) (sender :id))
  (declare (ignore sender))
  (#/stopModalWithCode: ccl::*nsapp* 1))

(defun open-delete-parking-spot-dialog ()
  "Open the delete-parking-spot-dialog for PATH."
  (cond (*del-dialog*
         (#/makeKeyAndOrderFront: *del-dialog* nil)
         (let ((ret (#/runModalForWindow: ccl::*nsapp* *del-dialog*)))
           (#/close *del-dialog*)
           (when (zerop ret) (selected-function-key *del-dialog*))))
        (t
         (let ((dialog (#/alloc delete-parking-spot-dialog)))
           (setq *del-dialog* dialog)
           (ns:with-ns-rect (r 10 300 600 140)
             (#/initWithContentRect:styleMask:backing:defer: 
              dialog
              r
              #$NSTitledWindowMask 
              #$NSBackingStoreBuffered
              #$NO))
           (dolist (item (get-delete-items dialog))
             (#/addSubview: (#/contentView dialog) item))
           (#/setTitle: dialog #@"Delete Parking Spot")
           (#/setReleasedWhenClosed: dialog nil)
           (#/setDefaultButtonCell: dialog (psd-okay-button dialog))
           (#/center dialog)
           (#/makeKeyAndOrderFront: dialog nil)
           (let ((ret (#/runModalForWindow: ccl::*nsapp* dialog)))
             (#/close dialog)
             (when (zerop ret) (selected-function-key dialog)))))))

(defmethod get-delete-items ((d delete-parking-spot-dialog))
  (append
   (make-delete-prompt-field)
   (make-delete-function-key-items d)
   (make-delete-buttons d)))

(defun make-delete-prompt-field ()
  "Create the prompt text-field."
  (list
   (let* ((string (#/initWithString:attributes: 
                   (#/alloc ns:ns-attributed-string) 
                   #@"Click the number of the parking spot you want to delete:"
                   cmenu::*tool-label-dictionary*))
          (title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame 15 90 410 32)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     (#/setStringValue: title string)
     title)))

(defun make-delete-function-key-items (dialog)
  (list
   (let* ((string (#/initWithString:attributes: 
                   (#/alloc ns:ns-attributed-string) 
                   #@"Associated Function Key:"
                   cmenu::*tool-label-dictionary*))
          (title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame 15 40 200 32)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     (#/setStringValue: title string)
     title)
   (setf (psd-function-key-matrix dialog)
         (ns:with-ns-rect (frame 190 40 350 32)
           (let* ((matrix (#/alloc ns:ns-matrix))
                  (prototype (#/init (#/alloc ns:ns-button-cell)))
                  buttons cell-array)
             (#/setTitle: prototype #@"7     ")
             (#/setButtonType: prototype #$NSRadioButton)
             (#/initWithFrame:mode:prototype:numberOfRows:numberOfColumns: 
              matrix frame #$NSRadioModeMatrix prototype 1 7)
             (setq cell-array (#/cells matrix))
             (#/setTitle: (#/objectAtIndex: cell-array 0) #@"1")
             (push (#/objectAtIndex: cell-array 0) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 1) #@"2")
             (push (#/objectAtIndex: cell-array 1) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 2) #@"3")
             (push (#/objectAtIndex: cell-array 2) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 3) #@"4")
             (push (#/objectAtIndex: cell-array 3) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 4) #@"5")
             (push (#/objectAtIndex: cell-array 4) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 5) #@"6")
             (push (#/objectAtIndex: cell-array 5) buttons)
             (#/setTitle: (#/objectAtIndex: cell-array 6) #@"7")
             (push (#/objectAtIndex: cell-array 6) buttons)
             (setf (psd-function-key-buttons dialog) buttons)
             matrix)))))

(defun make-delete-buttons (dialog)
  "Construct the buttons."
  (flet ((make-button (title x-coord y-coord x-dim y-dim action)
           (let ((button (#/alloc ns:ns-button)))
             (ns:with-ns-rect (frame x-coord y-coord x-dim y-dim)
               (#/initWithFrame: button frame))
             (#/setButtonType: button #$NSMomentaryPushInButton)
             (#/setBezelStyle: button #$NSRoundedBezelStyle)
             (#/setTitle: button title)
             (#/setTarget: button dialog)
             (#/setAction: button action)
             button)))
    (list
     (setf (psd-okay-button dialog)
           (make-button #@"Delete" 500 10 80 32
                        (ccl::@selector "deleteAction:")))
     (make-button #@"Cancel" 400 10 90 32
                  (ccl::@selector "cancelAction:")))))

