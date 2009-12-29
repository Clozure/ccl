;;;-*-Mode: LISP; Package: CCL -*-

(in-package "CCL")

;;; Before loading any Cocoa code which depends on CFBundle/NSBundle
;;; being able to find an application bundle, it -may- be neccessary
;;; to point the environment variable "CFProcessPath" to some file
;;; that's where the bundle's executable would be.
;;; This should only be necessary if the current application isn't
;;; already "inside a bundle".  If it is necessary, it has to happen
;;; before the CoreFoundation library's initialized.

(defun fake-cfbundle-path (bundle-root info-plist-proto-path bundle-prefix  bundle-suffix install-frameworks install-libraries #+windows-target icon-path)
  (let* ((kernel-name (standard-kernel-name))
         (translated-root (translate-logical-pathname bundle-root))
	 (bundle-name (let* ((name (if (directory-pathname-p translated-root)
				       (car (last (pathname-directory translated-root)))
				       (file-namestring translated-root)))
			     (len (length name)))
			(if (and (> len 4)
				 (string-equal name ".app" :start1 (- len 4)))
                                  (subseq name 0 (- len 4))
                                  name)))
         (bundle-id (concatenate 'string bundle-prefix "." (or bundle-suffix bundle-name)))
         (bundle-version (multiple-value-bind (os bits cpu)
                             (ccl::host-platform)
                           (declare (ignore os))
                           (format nil "~d (~a~d)" *openmcl-svn-revision* cpu bits)))
         (needles `(("OPENMCL-KERNEL" . ,kernel-name)
		    ("OPENMCL-NAME" . ,bundle-name)
                    ("OPENMCL-IDENTIFIER" . ,bundle-id)
		    ("OPENMCL-VERSION" . ,bundle-version)))
         (executable-dir (merge-pathnames
                           (make-pathname :directory (format nil "Contents/~a/"
                                                             #+windows-target
                                                             "Windows"
                                                             #+darwin-target
                                                             "MacOS"
                                                             #-(or windows-target darwin-target) "Unknown"))
                           translated-root))
         (executable-path (merge-pathnames executable-dir (make-pathname :name kernel-name :defaults nil))))
    (unless (probe-file info-plist-proto-path)
      (error "Can't find Info.plist prototype in ~s" info-plist-proto-path))
    (with-open-file (in info-plist-proto-path 
                        :direction :input
                        :external-format :utf-8)
      (with-open-file (out (merge-pathnames
                            (make-pathname :directory "Contents/"
                                           :name "Info"
                                           :type "plist")
                            translated-root)
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede
                           :external-format :utf-8)
        (do* ((line (read-line in nil nil) (read-line in nil nil)))
             ((null line))
	  (dolist (needle needles)
	    (let* ((pos (search (car needle) line)))
	      (when pos
		(setq line
		      (concatenate 'string
				   (subseq line 0 pos)
				   (cdr needle)
				   (subseq line (+ pos (length (car needle)))))))))
          (write-line line out))))
    
    (touch executable-path)
    (dolist (lib install-libraries)
      (copy-file lib executable-dir :preserve-attributes t :if-exists :supersede))
    (when install-frameworks
      (flet ((subdir (framework target)
               (ensure-directory-pathname (make-pathname :name (car (last (pathname-directory framework))) :defaults target)))
             (ignore-test (p)
               (let ((source-ignore '(".svn" "cvs" ".cvsignore")))
                 (flet ((backup-p (name)
                          (and (stringp name)
                               (let ((len (length name)))
                                 (and (> len 0)
                                      (or (eql (aref name (1- len)) #\~)
                                          (eql (aref name 0) #\#)))))))
                   (not (or (member (car (last (pathname-directory p))) source-ignore
                                    :test #'equalp)
                            (backup-p (file-namestring p))
                            (member (file-namestring p) source-ignore :test #'equalp)))))))
        (dolist (framework install-frameworks)
          (recursive-copy-directory framework (subdir framework executable-dir)
                                    :if-exists :overwrite :test #'ignore-test))))
    #+windows-target
    (copy-file icon-path (merge-pathnames
                          (make-pathname :directory "Contents/Resources/"
                                         :name bundle-name
                                         :type "ico")
                          translated-root)
               :preserve-attributes t :if-exists :supersede)
    (setenv "CFProcessPath" (native-translated-namestring executable-path))))
