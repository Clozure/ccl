(in-package :easygui)

;;; Contributed to Clozure CL by AWSC, Feb 2009.
;;; Permission is granted to use, redistribute, and modify.
;;;
;;; Provides some generally useful dialogs:
;;;    y-or-n-dialog
;;;    choose-file-dialog     (original from Gary Byers)
;;;    choose-new-file-dialog (adapted from that)
;;;    user-pick-color        (TODO: improve)
;;; To use them you will probably want to set *debug-cocoa-calls* to NIL.

(defun y-or-n-dialog (message)
  (let ((alert (make-instance 'ns:ns-alert)))
    (#/setMessageText: alert (ccl::%make-nsstring message))
    (#/addButtonWithTitle: alert (ccl::%make-nsstring "Yes"))
    (let ((no (#/addButtonWithTitle: alert (ccl::%make-nsstring "No"))))
          (#/setKeyEquivalent: no (ccl::%make-nsstring "N"))
          (#/setKeyEquivalent: no (ccl::%make-nsstring "n"))
          (#/setKeyEquivalentModifierMask: no #$NSCommandKeyMask))
    (eql (#/runModal alert) #$NSAlertFirstButtonReturn)))

(defvar *beepnsleep* t)

(defun choose-file-dialog (&key directory file-types file button-string)
  (gui::cocoa-choose-file-dialog :directory directory :file-types file-types :file file :button-string button-string))

(defun choose-new-file-dialog (&key directory file-types file button-string)
  (declare (ignore button-string))
  (gui::cocoa-choose-new-file-dialog :directory directory :file-types file-types :file file))

(defun choose-directory-dialog (&key directory button-string)
  (declare (ignore button-string))
  (gui::cocoa-choose-directory-dialog :directory directory))

(objc:defmethod (#/NSWindowWillCloseNotification :void) ((self ns:ns-color-panel))
  (dcc (#/stopModal (#/sharedApplication ns:ns-application))))
  
(defun user-pick-color (&key color (prompt "Pick a color") position)
  "POSITION argument is provided only for Digitool MCL compatibility, it is ignored"
  (declare (ignore position))
  (gui::with-autorelease-pool 
    (let* ((panel (dcc (#/sharedColorPanel ns:ns-color-panel)))) ; find or create the NSColorPanel
      (dcc (#/setPickerMode: ns:ns-color-panel #$NSWheelModeColorPanel))
      (dcc (#/setTitle: panel (ccl::%make-nsstring prompt)))
      (dcc (#/addObserver:selector:name:object:                 ; observe yourself close but
       (dcc (#/defaultCenter ns:ns-notification-center))        ; sadly confound OK & CANCEL
       panel
       (objc:\@selector #/NSWindowWillCloseNotification)
       (ccl::%make-nsstring "NSWindowWillCloseNotification")
       panel))
      (when color (dcc (#/setColor: panel color)))
      (dcc (#/runModalForWindow: (#/sharedApplication ns:ns-application) panel))
      (dcc (#/removeObserver:name:object:                       ; prevent pileup
       (dcc (#/defaultCenter ns:ns-notification-center))
       panel
       (ccl::%make-nsstring "NSWindowWillCloseNotification")
       panel))
      (dcc (#/retain (dcc (#/color panel)))))))
