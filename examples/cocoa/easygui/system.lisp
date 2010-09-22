(in-package :ccl)

(defparameter *easygui-pathname*  (or *load-pathname* *loading-file-source-file*))

(defvar *easygui-files*
  '("package"
    "new-cocoa-bindings"
    "events"
    "rgb"
    "views"
    "action-targets"
    "dialogs"))

(defvar *easygui-examples*
  '("tiny"
    "currency-converter"
    "view-hierarchy"))

(defun load-easygui (&optional (force-compile t))
  (with-compilation-unit ()
    (setq force-compile (load-ide-files *easygui-files* *easygui-pathname* force-compile))
    (setq force-compile (load-ide-files *easygui-examples* (merge-pathnames ";example;" *easygui-pathname*) force-compile))
    (push :easygui *features*))
  force-compile)
