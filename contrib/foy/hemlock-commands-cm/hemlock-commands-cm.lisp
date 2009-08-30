
;;; hemlock-commands.lisp

(in-package :common-lisp-user) 

(unless (member "HEMLOCK-COMMANDS-CM" *modules* :test #'string-equal)
  
(eval-when (:load-toplevel :execute)
  (defParameter *hemlock-commands-directory*
    (make-pathname :name nil :type nil :defaults (if *load-pathname* 
                                                     *load-pathname*
                                                     *loading-file-source-file*)))
  (defParameter *hemlock-commands-files* 
    (list (merge-pathnames ";hemlock-commands-1.lisp" *hemlock-commands-directory*)
          (merge-pathnames ";hemlock-commands-2.lisp" *hemlock-commands-directory*)
          (merge-pathnames ";hemlock-documentation-dialog.lisp" *hemlock-commands-directory*)
          (merge-pathnames ";hemlock-commands-new.lisp" *hemlock-commands-directory*))))
 
(dolist (file *hemlock-commands-files*)
  (load file))

(provide :hemlock-commands-cm)

)