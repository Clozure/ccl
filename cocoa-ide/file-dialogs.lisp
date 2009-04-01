(in-package "GUI")

;;;; MCL-ish file dialogs

;;; This needs to run in the event thread.
(defun %cocoa-choose-file-dialog (directory file-types file)
  (unless (eq *current-process* *cocoa-event-process*)
    (error "Must be called in Cocoa event process"))
  (let* ((open-panel (#/openPanel ns:ns-open-panel))
         (types-array +null-ptr+)
         (result nil))
    ;; for now;  support multiple file selection later.
    (#/setAllowsMultipleSelection: open-panel #$NO)
    (when directory
      (setq directory (#/autorelease (%make-nsstring (namestring directory)))))
    (when file
      (setq file (#/autorelease (%make-nsstring (namestring file)))))
    (when file-types
      (setq types-array (make-instance 'ns:ns-mutable-array))
      (dolist (type file-types)
        (let ((s (%make-nsstring type)))
          (#/addObject: types-array s)
          (#/release s)))
      (#/autorelease types-array))
    (setq result (#/runModalForDirectory:file:types: open-panel directory file types-array))
    (cond ((= result #$NSOKButton)
           (lisp-string-from-nsstring (#/filename open-panel)))
          ((= result #$NSCancelButton)
           nil)
          (t
           (error "couldn't run the open panel: error code ~d" result)))))
        
(defun cocoa-choose-file-dialog (&key directory file-types file)
  (when (and directory (not (directoryp directory)))
    (error "Value ~s supplied for :DIRECTORY doesn't designate a directory." directory))
  (when (and file-types
             (not (listp file-types))
             (not (every #'stringp file-types)))
    (error "Value ~s supplied for :FILE-TYPES is not a list of strings." file-types))
  (when (and file (not (probe-file file)))
    (error "Value ~s supplied for :FILE doesn't designate a file." file))
  (execute-in-gui #'(lambda () (%cocoa-choose-file-dialog directory file-types file))))

(defun %cocoa-choose-new-file-dialog (directory file-types file)
  (unless (eq *current-process* *cocoa-event-process*)
    (error "Must be called in Cocoa event process"))
  (let* ((save-panel (#/savePanel ns:ns-save-panel))
         (types-array +null-ptr+)
         (result nil))
    (#/setCanSelectHiddenExtension: save-panel t)
    (when directory
      (setq directory (#/autorelease (%make-nsstring (namestring directory)))))
    (when file
      (setq file (#/autorelease (%make-nsstring (namestring file)))))
    (when file-types
      (setq types-array (make-instance 'ns:ns-mutable-array))
      (dolist (type file-types)
        (let ((s (%make-nsstring type)))
          (#/addObject: types-array s)
          (#/release s)))
      (#/autorelease types-array))
    (#/setAllowedFileTypes: save-panel types-array)
    (setq result (#/runModalForDirectory:file: save-panel directory file))
    (cond ((= result #$NSOKButton)
           (lisp-string-from-nsstring (#/filename save-panel)))
          ((= result #$NSCancelButton)
           nil)
          (t
           (error "couldn't run the save panel: error code ~d" result)))))

(defun cocoa-choose-new-file-dialog (&key directory file-types file)
  (when (and directory (not (directoryp directory)))
    (error "Value ~s supplied for :DIRECTORY doesn't designate a directory." directory))
  (when (and file-types
             (not (listp file-types))
             (not (every #'stringp file-types)))
    (error "Value ~s supplied for :FILE-TYPES is not a list of strings." file-types))
  (when (and file (not (probe-file file)))
    (error "Value ~s supplied for :FILE doesn't designate a file." file))
  (execute-in-gui #'(lambda () (%cocoa-choose-new-file-dialog directory file-types file))))

(defun cocoa-choose-file-dialog-hook-function (must-exist prompt file-types)
  (declare (ignore prompt))
  (if must-exist
    (cocoa-choose-file-dialog :file-types file-types)
    (cocoa-choose-new-file-dialog :file-types file-types)))

(setq ccl::*choose-file-dialog-hook* 'cocoa-choose-file-dialog-hook-function)
