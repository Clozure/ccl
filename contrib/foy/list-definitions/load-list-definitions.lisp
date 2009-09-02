
;;; load-list-definitions.lisp 

(in-package :cl-user)

(unless (member "LIST-DEFINITIONS" *modules* :test #'string-equal)
  
(eval-when (:load-toplevel :execute)
  (defParameter *list-definitions-directory-string*
    (make-pathname :name nil :type nil :defaults (if *load-pathname* 
                                                     *load-pathname*
                                                     *loading-file-source-file*)))
  (defParameter *list-definition-files* 
    (list (merge-pathnames ";list-definitions.lisp" *list-definitions-directory-string*)
          (merge-pathnames ";history-lists.lisp" *list-definitions-directory-string*))))
 
(dolist (file *list-definition-files*)
  (load file))

(provide :list-definitions)

)