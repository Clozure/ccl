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
      ;; Is this necessary?
      (let* ((image-name (ccl::standard-kernel-name))
	     (ccl-image (make-pathname :name image-name :host "ccl"))
	     (dest-image (make-pathname :name image-name
					:defaults (subdir contents-dir "MacOS"))))
	(ensure-directories-exist dest-image)
	(copy-file ccl-image dest-image :if-exists :supersede :preserve-attributes t))
      (ccl::touch target-dir))))

(progn
  (require "FAKE-CFBUNDLE-PATH")
  (create-ide-bundle *cocoa-application-path*)
  (ccl::fake-cfbundle-path *cocoa-application-path* "ccl:cocoa-ide;Info.plist-proto" "com.clozure"))
