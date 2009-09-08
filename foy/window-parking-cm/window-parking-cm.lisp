
;;; cl-documentation.lisp 

(in-package :common-lisp-user)

(unless (member "WINDOW-PARKING-CM" *modules* :test #'string-equal)
  
(eval-when (:load-toplevel :execute)
  (defParameter *window-parking-directory*
    (make-pathname :name nil :type nil :defaults (if *load-pathname* 
                                                     *load-pathname*
                                                     *loading-file-source-file*)))
  (defParameter *window-parking-files* 
    (list (merge-pathnames ";window-parking.lisp" *window-parking-directory*)
          (merge-pathnames ";window-parking-dialogs.lisp" *window-parking-directory*))))
 
(dolist (file *window-parking-files*)
  (load file))

(provide :window-parking-cm)

)