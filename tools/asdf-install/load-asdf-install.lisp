;;; -*- Mode: Lisp -*-

;;; load-asdf-install.lisp --
;;; Generic loader for ASDF-INSTALL.

(eval-when (:load-toplevel :execute)
  (unless (find-package '#:asdf-install-loader)
    (make-package '#:asdf-install-loader :use '(#:common-lisp))))

(in-package :asdf-install-loader)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *asdf-install-directory*
    (make-pathname :host (pathname-host *load-truename*)
		   :device (pathname-device *load-truename*)
		   :directory (pathname-directory *load-truename*)
		   ;; :case :common ; Do we need this?
		   )))


(defun cl-user::load-asdf-install
  (&key
   (directory *asdf-install-directory*)
   (compile-first-p nil)
   (load-verbose *load-verbose*)
   (print-herald t)
   )
  (when print-herald
    (format *standard-output*
	    "~&;;; ASDF-INSTALL: Loading ASDF-INSTALL package from directory~@
               ;;;               \"~A\"~2%"
	    (namestring (pathname directory))))
  (let ((directory (pathname directory)))
    (flet ((load-and-or-compile (file)
	     (if compile-first-p
		 (multiple-value-bind (output-truename warnings-p failure-p)
		     (compile-file file)
		   ;; (declare (ignore warnings-p))
		   (when failure-p
		     (format *standard-output*
			     ";;; File ~S compiled~@
                              ;;; Warnings ~S, Failure ~S.~%"
			     output-truename
			     warnings-p
			     failure-p)
		     (return-from cl-user::load-asdf-install nil)
		     )
		   (load output-truename :verbose load-verbose))
		 (load file :verbose load-verbose)))
	   )

      (setf (logical-pathname-translations "ASDF-INSTALL-LIBRARY")
	    `(("**;*.*.*"
	       ,(make-pathname
		 :host (pathname-host directory)
		 :device (pathname-device directory)
		 :directory (append (pathname-directory directory)
				    (list :wild-inferiors))))
	      ("**;*.*"
	       ,(make-pathname
		 :host (pathname-host directory)
		 :device (pathname-device directory)
		 :directory (append (pathname-directory directory)
				    (list :wild-inferiors))))))

      (load-and-or-compile "ASDF-INSTALL-LIBRARY:defpackage.lisp")
      (load-and-or-compile "ASDF-INSTALL-LIBRARY:port.lisp")

      (unless (find-package '#:split-sequence)
        (load-and-or-compile "ASDF-INSTALL-LIBRARY:split-sequence.lisp"))

      (load-and-or-compile "ASDF-INSTALL-LIBRARY:installer.lisp")

      ;; (load-and-or-compile "ASDF-INSTALL-LIBRARY:loader.lisp")

      ))
  (pushnew :asdf-install *features*)
  (provide 'asdf-install)

  ;; To clean a minimum (and to make things difficult to debug)...
  ;; (delete-package '#:asdf-install-loader)
  )


;;; Automatically load the library.

(eval-when (:load-toplevel :execute)
  (cl-user::load-asdf-install))

;;; end of file -- load-asdf-install.lisp --
