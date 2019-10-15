
;;; context-menu-cm.lisp 

(in-package :common-lisp-user)

(unless (member "CONTEXT-MENU-CM" *modules* :test #'string-equal)
  
(eval-when (:load-toplevel :execute)
  (defParameter *context-menu-directory*
    (make-pathname :name nil :type nil :defaults (if *load-pathname* 
                                                     *load-pathname*
                                                     *loading-file-source-file*)))
  (defParameter *context-menu-files* 
    (list (merge-pathnames ";context-menu.lisp" *context-menu-directory*)
          (merge-pathnames ";context-menu-dialogs.lisp" *context-menu-directory*))))
 
(dolist (file *context-menu-files*)
  (load file))

(provide :context-menu-cm)

)