(in-package :cl-user)

(let ((path (or *load-pathname* *loading-file-source-file*)))
  (load (merge-pathnames ";easygui;easygui.asd" path)))

(asdf:operate 'asdf:load-op 'easygui)

(push :easygui *features*)
