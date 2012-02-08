;;-*-Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates
;;;

(in-package "CCL")

;;; We need to be able to point the CoreFoundation and Cocoa libraries
;;; at some bundle very early in the process, so do that before anything
;;; else.

(defun create-ide-bundle (bundle-path &key (source "ccl:cocoa-ide;ide-contents;")
				           (source-ignore '(".svn" "cvs" ".cvsignore"))
					   (copy-headers *cocoa-application-copy-headers-p*)
                                           (install-altconsole *cocoa-application-install-altconsole*)
					   (if-exists :overwrite))
  ;; TODO: Right now if the bundle exists, we leave alone any files that we don't replace.
  ;; I'd like :if-exists :supersede mean to remove such files, for clean builds, but
  ;; recursive-copy-directory doesn't support :if-exists :supersede yet...
  (flet ((subdir (dir sub)
	   (ensure-directory-pathname (make-pathname :name sub :defaults dir)))
	 (ignore-test (p)
	   (flet ((backup-p (name)
		    (and (stringp name)
			 (let ((len (length name)))
			   (and (> len 0)
				(or (eql (aref name (1- len)) #\~)
				    (eql (aref name 0) #\#)))))))
	     (not (or (member (car (last (pathname-directory p))) source-ignore :test #'equalp)
		      (backup-p (file-namestring p))
		      (member (file-namestring p) source-ignore :test #'equalp))))))
    (let* ((source-dir (ensure-directory-pathname source))
	   (target-dir (ensure-directory-pathname bundle-path))
	   (contents-dir (subdir target-dir "Contents")))
      (recursive-copy-directory source-dir contents-dir :if-exists if-exists :test #'ignore-test)
      (when copy-headers
	(let* ((subdirs (ccl::cdb-subdirectory-path))
	       (ccl-headers (make-pathname :host "ccl" :directory `(:absolute ,@subdirs)))
	       (dest-headers (make-pathname :host (pathname-host contents-dir)
					    :directory (append (pathname-directory contents-dir)
							       (cons "Resources" subdirs)))))
	  (recursive-copy-directory ccl-headers dest-headers :if-exists if-exists :test #'ignore-test)))
      (when install-altconsole
        (install-altconsole bundle-path))
      ;; Is this necessary?
      ;; At one point in the past, it was necessary for the bundle to
      ;; contain an executable file whose name matched what was specified
      ;; in its Info.plist file.  That executable file could be practically
      ;; anything, as long as its executable bits were set.
      (let* ((image-name (ccl::standard-kernel-name))
             #+ignore
	     (ccl-image (make-pathname :name image-name :host "ccl"))
	     (dest-image (make-pathname :name image-name
					:defaults (subdir contents-dir #+darwin-target "MacOS" #+windows-target "Windows"))))
	(ensure-directories-exist dest-image)
        #+no
	(copy-file ccl-image dest-image :if-exists :supersede :preserve-attributes t)
        (ccl::touch dest-image)
        )
      #-windows-target
      (ccl::touch target-dir))))

;;; This runs "make install" to generate
;;; "ccl:cocoa-ide;altconsole;AltConsole.app",
;;; then copies that application bundle into the "Resources" directory
;;; of the target bundle.  It might be simpler to just have "make install"
;;; put things in the right place, but "the right place" is likely to
;;; be a pathname that contains a space. Quoting such a pathname -
;;; and figuring out how to get make to do so - is left as an exercise.
(defun install-altconsole (bundle-path)
  #-cocotron
  (let* ((altconsole-path (merge-pathnames ";Contents;Resources;AltConsole.app;" bundle-path))
         (build-directory "ccl:cocoa-ide;altconsole;")
         (build-bundle-path "ccl:cocoa-ide;altconsole;AltConsole.app")
         (make-output (make-string-output-stream))
         (args `("-C" ,(native-translated-namestring build-directory) "install")))
    (recursive-delete-directory altconsole-path :if-does-not-exist nil)
    (unwind-protect
         (multiple-value-bind (exit-status code)
             (external-process-status
              (run-program "make" args :output make-output :error make-output))
           (unless (and (eq exit-status :exited) (zerop code))
             (format t "~&'make install' of AltConsole.app failed:~&~a"
                     (get-output-stream-string make-output))
             (return-from install-altconsole nil)))
      (close make-output))
    ;;(ensure-directories-exist altconsole-path)
    (recursive-copy-directory build-bundle-path altconsole-path)
    (ccl::touch altconsole-path)
    t)
  #+cocotron
  (let* ((path (probe-file "ccl:cocotron;WaltConsole;WaltConsole.exe")))
    (when path
      (copy-file path (merge-pathnames ";Contents;Resources;WaltConsole.exe" bundle-path)
                 :preserve-attributes t :if-exists :supersede)
      t))
  )

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
		    ("OPENMCL-VERSION" . ,bundle-version)
                    ("OPENMCL-MAJOR-VERSION" . ,(format nil "~d" *openmcl-major-version*))
                    ("OPENMCL-MINOR-VERSION" . ,(format nil "~d" *openmcl-minor-version*))
                    ("OPENMCL-REVISION" . ,(if *openmcl-revision* (format nil "-~a" *openmcl-revision*) ""))
                    ("CURRENT-YEAR" . ,(format nil "~a" (nth-value 5 (decode-universal-time (get-universal-time)))))))
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




(progn
  (create-ide-bundle *cocoa-application-path*)
  (fake-cfbundle-path *cocoa-application-path* "ccl:cocoa-ide;Info.plist-proto" "com.clozure" *cocoa-application-bundle-suffix* *cocoa-application-frameworks* *cocoa-application-libraries* #+windows-target "ccl:cocoa-ide;ide-contents;resources;openmcl-icon.ico"))
