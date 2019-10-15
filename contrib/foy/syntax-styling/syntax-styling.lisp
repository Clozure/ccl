
;;; syntax-styling.lisp 

(in-package :common-lisp-user)

;;; (pushnew :sax-debug *features*)

(unless (member "SYNTAX-STYLING" *modules* :test #'string-equal)
  (eval-when (:load-toplevel :execute)
    (defParameter *syntax-styling-directory*
      (make-pathname :name nil :type nil :defaults (if *load-pathname* 
                                                     *load-pathname*
                                                     *loading-file-source-file*)))
    (defParameter *syntax-styling-files* 
      (list #+sax-debug (merge-pathnames ";testing-specials.lisp" *syntax-styling-directory*)
            (merge-pathnames ";syntax-styling-specials.lisp" *syntax-styling-directory*)
            (merge-pathnames ";syntax-styling-comments.lisp" *syntax-styling-directory*)
            (merge-pathnames ";syntax-styling-1.lisp" *syntax-styling-directory*)
            (merge-pathnames ";syntax-styling-2.lisp" *syntax-styling-directory*)
            #+sax-debug (merge-pathnames ";testing1.lisp" *syntax-styling-directory*)
            #+sax-debug (merge-pathnames ";testing2.lisp" *syntax-styling-directory*)
            )))
 
(dolist (file *syntax-styling-files*)
  (load file))

(provide :syntax-styling)

)