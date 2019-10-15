
;;; source-comparison.lisp 

(unless (member "SOURCE-COMPARISON" *modules* :test #'string-equal)
  
(eval-when (:load-toplevel :execute)
  (defParameter *source-compare-directory*
    (make-pathname :name nil :type nil :defaults (if *load-pathname* 
                                                     *load-pathname*
                                                     *loading-file-source-file*)))
  (defParameter *source-compare-files* 
    (list (merge-pathnames ";source-compare.lisp" *source-compare-directory*)
          (merge-pathnames ";source-compare-dialog.lisp" *source-compare-directory*))))
 
(dolist (file *source-compare-files*)
  (load file))

(provide :source-comparison)

)