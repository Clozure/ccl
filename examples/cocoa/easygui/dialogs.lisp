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
    (#/addButtonWithTitle: alert (ccl::%make-nsstring "No"))
    (eql (#/runModal alert) #$NSAlertFirstButtonReturn)))

(defvar *beepnsleep* t)

(defun choose-file-dialog (&key button-string)
  (gui::with-autorelease-pool 
      (let* ((panel (dcc (#/autorelease (dcc (#/openPanel ns:ns-open-panel)))))) ; allocate an NSOpenPanel
        (dcc (#/setAllowsMultipleSelection: panel nil)) ; return at most one filename
        (when button-string
          (setf button-string (ccl::%make-nsstring button-string))
          (dcc (#/setPrompt: panel button-string)))
        (when (eql #$NSOKButton
                   (dcc (#/runModalForDirectory:file:types: panel
                           +null-ptr+ ; default to last dir used
                           +null-ptr+ ; no preselected file
                           ;; If not NIL below then an ObjC array containing NSStrings could be used
                           ;; to restrict the file types we're interested in
                           nil)))
          ;; Because we told the panel to disallow multiple selection,
          ;; there should be exactly one object in this array, an
          ;; NSString describing the selected file.
          (let* ((files (dcc (#/filenames panel))) thing)
            (if (eql 1 (dcc (#/count files)))
              (progn
                (setf thing (dcc (#/objectAtIndex: files 0)))
                (gui::lisp-string-from-nsstring thing))
              "Don't know why we didn't get an NSArray containing exactly 1 file here."))))))

(defun choose-new-file-dialog (&key button-string)
  (declare (ignorable button-string))
  (gui::with-autorelease-pool 
      (let* ((panel (dcc (#/autorelease (dcc (#/savePanel ns:ns-save-panel)))))) ; allocate an NSSavePanel
        (when button-string (dcc (#/setPrompt: panel (ccl::%make-nsstring button-string))))
        (when (eql #$NSOKButton
                   (dcc (#/runModalForDirectory:file: panel
                      +null-ptr+ ; default to last dir used
                      +null-ptr+)))
          ;; Because we told the panel to disallow multiple selection,
          ;; there should be exactly one object in this array, an
          ;; NSString describing the selected file.
          (let* ((files (dcc (#/filenames panel))))
            (if (eql 1 (dcc (#/count files)))
              (gui::lisp-string-from-nsstring (dcc (#/objectAtIndex: files 0)))
              (error "Don't know why we didn't get an NSArray containing exactly 1 file here.")))))))

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
