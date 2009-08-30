
;;; cl-documentation.lisp 

(in-package :common-lisp-user)

(unless (member "CL-DOCUMENTATION-CM" *modules* :test #'string-equal)
  
(eval-when (:load-toplevel :execute)
  (defParameter *cl-documentation-directory*
    (make-pathname :name nil :type nil :defaults (if *load-pathname* 
                                                     *load-pathname*
                                                     *loading-file-source-file*)))
  (defParameter *cl-documentation-files* 
    (list (merge-pathnames ";cl-documentation.lisp" *cl-documentation-directory*))))
 
(dolist (file *cl-documentation-files*)
  (load file))

(provide :cl-documentation-cm)

)