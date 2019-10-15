(in-package :cl-user)

;;; Clozure CL Hemlock editor windows persistence
;;; ie. on restart of CCL re-open (and position) the last session's open files.
;;;
;;; LLGPL Copyright (c) Peter Paine 20080611 
;;; Maintainer: gmail: p2.edoc 
;;; To use: add (require :workpersistence) to your home:ccl-init.lisp file,
;;          or (load ~this-file~)
;;; Updates 
;;; 20091018: restore original window order
;;;           option to save independently per platform and CLZ-version
;;; 20090906: fix not saving closed windows, fix resizing in gui thread, save in home dir.
;;; 20090928: re-select Listener
;;; ToDo: 
;;;   - how to read window from buffer (without external search via path)?
;;;   - if on quit, an unsaved buffers prompt, remember-hemlock-files not called (or doesn't save)
;;;   - multiple choice menu of buffers to save
;;;   - add auto backup process: auto save modified files as file name variant, check/restore on restart 

#-(and clozure-common-lisp hemlock) (error "Workpersistence only runs under CLZ ~
                                           Hemlock Nextstep/Cocoa API")

;; Allows separation of working file sets for different platform versions.
(defvar *separate-ccl-working-file-sets-by-platform-p* T)

;; Independently save working file sets by major/minor version of CLZ.
(defvar *separate-ccl-working-file-sets-by-ccl-version-p* nil)

(defun work-persistence-file (&optional version)
  (setq version (if version (format nil "-~A" version) ""))
  (let ((ccl-version
         (if *separate-ccl-working-file-sets-by-ccl-version-p*
           (format nil "-~D-~D" ccl::*openmcl-major-version* ccl::*openmcl-minor-version*)
           "")))
  (if *separate-ccl-working-file-sets-by-platform-p*
    (format nil "home:.ccl-workpersistence-~A~A~A.text" (ccl::platform-description) ccl-version version)
    (format nil "home:.ccl-workpersistence~A~A.text" ccl-version version))))

(defvar *work-persistence-file* (work-persistence-file) "per user")
;; (ed *work-persistence-file*)

(defun copy-work-persistence ()
  (when (probe-file *work-persistence-file*)
    (copy-file *work-persistence-file* (work-persistence-file "copy") :if-exists :overwrite)))
;; (ed (work-persistence-file "copy"))

(defun remember-hemlock-files ()
  (with-open-file (*standard-output*
                   *work-persistence-file*
                   :direction :output :if-exists :supersede)
    (loop for buffer in (hi::all-buffers)
      do (let* ((path (hi:buffer-pathname buffer)))
           (when path 
             (let ((frame (slot-value (find-file-buffer-window path) 'ns:_frame)))
               (loop initially (format T "~&(")
                 for fn in '(ns:ns-rect-x ns:ns-rect-y ns:ns-rect-width ns:ns-rect-height)
                 do (format T "~5D " (floor (funcall fn frame)))
                 finally (format T "~S)" path))))))))

(defun find-file-buffer-window (path)
   (loop with win-arr = (#/orderedWindows ccl::*NSApp*)
     for i below (#/count win-arr)
     for win = (#/objectAtIndex: win-arr i)
     when (and (typep win '(and gui::hemlock-frame
                                (not gui::hemlock-listener-frame)))
               (equalp path (hi:buffer-pathname (hi:hemlock-view-buffer
                                                 (gui::hemlock-view win)))))
     return win))

(defun find-listener () ; there must be a standard way to do this
  ;; only saves first listener found
  (loop with win-arr = (#/orderedWindows ccl::*NSApp*)
    for i below (#/count win-arr)
    for win = (#/objectAtIndex: win-arr i)
    when (typep win 'gui::hemlock-listener-frame)
    return win))

(defun select-listener ()
  (process-wait "Wait for Listener" 'find-listener)
  (let ((listener (find-listener)))
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     listener (objc:@selector "makeKeyAndOrderFront:") nil nil)))

(defun open-remembered-hemlock-files ()
  (let (old-file-specs)
    (with-open-file (buffer-persistence-stream
                     *work-persistence-file*
                     :direction :input :if-does-not-exist nil)
      (when buffer-persistence-stream ;; reverse order
        (loop for item = (read buffer-persistence-stream nil)
                while item do (push item old-file-specs))))
    (gui::execute-in-gui 
     #'(lambda () 
         (dolist (old-file-spec old-file-specs)
           (destructuring-bind (posx posy width height path) old-file-spec
             (when (probe-file path)
               (gui::find-or-make-hemlock-view path)
               (let ((window (find-file-buffer-window path))) ; round about way*
                 ;;* how to get from hemlock-view
                 (when window
                   ;; should check whether coords are still in screen bounds
                   ;; (could have changed screen realestate since)
                   (let ((rect (ns:make-ns-rect posx posy width height)))
                     (#/setFrame:display: window rect t)))))))
         (select-listener)))))
    
         
(pushnew 'remember-hemlock-files *lisp-cleanup-functions*)
(pushnew 'open-remembered-hemlock-files *lisp-startup-functions*)

;; (remember-hemlock-files)
(open-remembered-hemlock-files)
