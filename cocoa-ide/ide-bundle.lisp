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
		      (backup-p (pathname-name p))
		      (backup-p (pathname-type p))
		      (member (pathname-name p) source-ignore :test #'equalp))))))
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
    t))

(progn
  (require "FAKE-CFBUNDLE-PATH")
  (create-ide-bundle *cocoa-application-path*)
  (ccl::fake-cfbundle-path *cocoa-application-path* "ccl:cocoa-ide;Info.plist-proto" "com.clozure" *cocoa-application-bundle-suffix* *cocoa-application-frameworks* *cocoa-application-libraries*))
