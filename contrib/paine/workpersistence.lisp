(in-package :cl-user)

;;; Clozure CL Hemlock editor windows persistence
;;; ie. on restart of CCL re-open (and position) the last session's open files.
;;; LGPL   c/o Peter Paine 20080611
;;; Update 20090906: fix not saving closed windows, fix resizing in gui thread, save in home dir.
;;; ToDo: how to read window from buffer (without external search via path)?
;;; To use: add (load ~this-file~) to your home:ccl-init.lisp file.

;; Allows separation of working file sets for different CLZ versions.
(defvar *separate-ccl-working-file-sets-p* T)

(defvar *work-persistence-file*
  (if *separate-ccl-working-file-sets-p*
    (format nil "home:.ccl-workpersistence-~A.text" (ccl::platform-description))
    "home:.ccl-workpersistence.text")
  "per user")
;; (ed *work-persistence-file*)

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

(defun open-remembered-hemlock-files ()
   (with-open-file (buffer-persistence-stream
                    *work-persistence-file*
                    :direction :input :if-does-not-exist nil)
     (when buffer-persistence-stream
       (loop for item = (read buffer-persistence-stream nil)
         while item
         do (destructuring-bind (posx posy width height path) item
              (when (probe-file path)
                (gui::execute-in-gui #'(lambda () (gui::find-or-make-hemlock-view path)))
                (let ((window (find-file-buffer-window path))) ; round about way*
                  ;;* how to get from hemlock-view
                  (when window
                    ;; should check whether coords are still in screen bounds
                    ;; (could have changed screen realestate since)
                    (let ((rect (ns:make-ns-rect posx posy width height)))
                      (gui::execute-in-gui 
                       #'(lambda () 
                           (#/setFrame:display: window rect t))))))))))))

(pushnew 'remember-hemlock-files *lisp-cleanup-functions*)
(pushnew 'open-remembered-hemlock-files *lisp-startup-functions*)

;; (remember-hemlock-files)
(open-remembered-hemlock-files)
