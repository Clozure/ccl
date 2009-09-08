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

(defun notify (message)
  "FYI"
  (gui::alert-window :title "Notification" :message message))




