(in-package "GUI")

;;;; MCL-ish file dialogs

(defun %cocoa-choose-file-dialog (directory file-types file button-string)
  (assume-cocoa-thread)
  (let* ((open-panel (#/openPanel ns:ns-open-panel))
         (types-array +null-ptr+))
    ;; Maybe support multiple file selection later.
    (#/setAllowsMultipleSelection: open-panel #$NO)
    (when directory
      (setq directory (#/autorelease (%make-nsstring directory))))
    (when file
      (setq file (#/autorelease (%make-nsstring file))))
    (when file-types
      (setq types-array (make-instance 'ns:ns-mutable-array))
      (dolist (type file-types)
        (let ((s (%make-nsstring type)))
          (#/addObject: types-array s)
          (#/release s)))
      (#/autorelease types-array))
    (when button-string
      (#/setPrompt: open-panel (#/autorelease (%make-nsstring button-string))))
    (let ((result (#/runModalForDirectory:file:types: open-panel directory
						      file types-array)))
      (cond ((= result #$NSOKButton)
	     (lisp-string-from-nsstring (#/filename open-panel)))
	    ((= result #$NSCancelButton)
	     nil)
	    (t
	     (error "couldn't run the open panel: error code ~d" result))))))
        
(defun cocoa-choose-file-dialog (&key directory file-types file button-string)
  (when directory
    (setq directory (directory-namestring directory)))
  (when file-types
    (unless (and (listp file-types)
		 (every #'stringp file-types))
      (error "~s is not a list of strings." file-types)))
  (when file
    (setq file (file-namestring file)))
  (check-type button-string (or null string))
  (execute-in-gui #'(lambda () (%cocoa-choose-file-dialog directory file-types file button-string))))

(defun %cocoa-choose-new-file-dialog (directory file-types file)
  (assume-cocoa-thread)
  (let* ((save-panel (#/savePanel ns:ns-save-panel))
         (types-array +null-ptr+))
    (#/setCanSelectHiddenExtension: save-panel t)
    (when directory
      (setq directory (#/autorelease (%make-nsstring directory))))
    (when file
      (setq file (#/autorelease (%make-nsstring file))))
    (when file-types
      (setq types-array (make-instance 'ns:ns-mutable-array))
      (dolist (type file-types)
        (let ((s (%make-nsstring type)))
          (#/addObject: types-array s)
          (#/release s)))
      (#/autorelease types-array))
    (#/setAllowedFileTypes: save-panel types-array)
    (let ((result (#/runModalForDirectory:file: save-panel directory file)))
      (cond ((= result #$NSOKButton)
	     (lisp-string-from-nsstring (#/filename save-panel)))
	    ((= result #$NSCancelButton)
	     nil)
	    (t
	     (error "couldn't run the save panel: error code ~d" result))))))

(defun cocoa-choose-new-file-dialog (&key directory file-types file)
  (when directory
    (setq directory (directory-namestring directory)))
  (when file
    (setq file (file-namestring file)))
  (when file-types
    (unless (and (listp file-types)
		 (every #'stringp file-types))
      (error "~s is not a list of strings." file-types)))
  (execute-in-gui #'(lambda () (%cocoa-choose-new-file-dialog directory file-types file))))

(defun cocoa-choose-file-dialog-hook-function (must-exist prompt file-types)
  (declare (ignore prompt))
  (if must-exist
    (cocoa-choose-file-dialog :file-types file-types)
    (cocoa-choose-new-file-dialog :file-types file-types)))

(setq ccl::*choose-file-dialog-hook* 'cocoa-choose-file-dialog-hook-function)

(defun %cocoa-choose-directory-dialog (directory)
  (assume-cocoa-thread)
  (let ((open-panel (#/openPanel ns:ns-open-panel)))
    (#/setCanChooseFiles: open-panel #$NO)
    (#/setCanChooseDirectories: open-panel #$YES)
    (#/setAllowsMultipleSelection: open-panel #$NO)
    (#/setTitle: open-panel #@"Choose Directory")
    (#/setPrompt: open-panel #@"Choose")
    (when directory
      (setq directory (#/autorelease (%make-nsstring directory))))
    (let  ((result (#/runModalForDirectory:file:types: open-panel directory
						       nil nil)))
      (cond ((= result #$NSOKButton)
	     (make-pathname :directory (lisp-string-from-nsstring
					(#/directory open-panel))))
	    ((= result #$NSCancelButton)
	     nil)
	    (t
	     (error "couldn't run the open panel: error code ~d" result))))))

(defun cocoa-choose-directory-dialog (&key directory)
  (when directory
    (setq directory (directory-namestring directory)))
  (execute-in-gui #'(lambda () (%cocoa-choose-directory-dialog directory))))
