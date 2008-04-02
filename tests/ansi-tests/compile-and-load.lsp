#-(and gcl (not ansi-cl)) (in-package :common-lisp-user)
#+(and gcl (not ansi-cl)) (in-package "USER")

#+allegro
(progn
  (setq *ignore-package-name-case* t)
  (when (eq excl:*current-case-mode* :case-sensitive-lower)
    (push :lower-case *features*)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; (intern "==>" "CL-USER")
  (unless (fboundp 'compile-file-pathname)
    (defun compile-file-pathname (pathname)
      (make-pathname :defaults pathname :type "o"))))

;;; On-demand compile and load

(defvar *compiled-and-loaded-files* nil
  "List containing pathname, creation times for files that have already
   been loaded.")

(defun compile-and-load (pathspec &key force)
  "Find the file indicated by PATHSPEC, compiling it first if
   the associated compiled file is out of date."
  (let* ((pathname (pathname pathspec))
	 (pathname (if *load-pathname*
		       (merge-pathnames pathname *load-pathname*)
		     pathname))		     
	 (former-data (assoc pathname *compiled-and-loaded-files*
			     :test #'equalp))
	 (compile-pathname (compile-file-pathname pathname))
	 (source-write-time (file-write-date pathname))
	 (target-write-time (and (probe-file compile-pathname)
				 (file-write-date compile-pathname))))
    (unless (and (not force)
		 former-data
		 (>= (cadr former-data) source-write-time))
      (when (or (not target-write-time)
		(<= target-write-time source-write-time))
	(handler-bind
	 #-sbcl ()
	 #+sbcl ((sb-ext:code-deletion-note #'muffle-warning))
	 (compile-file pathname)))
      (if former-data
	  (setf (cadr former-data) source-write-time)
	(push (list pathname source-write-time) *compiled-and-loaded-files*))
      (load compile-pathname))))
