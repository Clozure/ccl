(in-package :ccl)

(let ((path (or *load-pathname* *loading-file-source-file*)))
  (load (merge-pathnames ";easygui;system.lisp" path))
  (load-easygui nil))