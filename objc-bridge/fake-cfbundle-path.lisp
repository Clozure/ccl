;;;-*-Mode: LISP; Package: CCL -*-

(in-package "CCL")

;;; Before loading any Cocoa code which depends on CFBundle/NSBundle
;;; being able to find an application bundle, it -may- be neccessary
;;; to point the environment variable "CFProcessPath" to some file
;;; that's where the bundle's executable would be.
;;; This should only be necessary if the current application isn't
;;; already "inside a bundle".  If it is necessary, it has to happen
;;; before the CoreFoundation library's initialized.

(defun fake-cfbundle-path (bundle-root info-plist-proto-path bundle-prefix &optional bundle-suffix)
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
         (executable-path (merge-pathnames
                           (make-pathname :directory "Contents/MacOS/"
                                          :name kernel-name)
                           translated-root)))
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
    (setenv "CFProcessPath" (native-translated-namestring executable-path))))
