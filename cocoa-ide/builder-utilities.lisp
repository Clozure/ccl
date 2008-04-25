;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          builder-utilities.lisp
;;;; Version:       0.9
;;;; Project:       bosco - Cocoa application builder
;;;; Purpose:       utilities used by both batch and interactive builders
;;;;
;;;; ***********************************************************************

(in-package :ccl)

;;; ABOUT
;;; ------------------------------------------------------------------------
;;; Builder-utilities contains several functions used by OpenMCL
;;; application-building tools for building and copying bundles,
;;; resource directories, and magic files used by OSX applications.

(defun copy-nibfile (srcnib dest-directory &key (if-exists :overwrite))
  (setq if-exists (require-type if-exists '(member :overwrite :error)))
  (let* ((basename (basename srcnib))
         (dest (path dest-directory basename)))
    (if (probe-file dest)
        (case if-exists
          (:overwrite (progn
                        (if (directoryp dest)
                            (recursive-delete-directory dest)
                            (delete-file dest))))
          (:error (error "The nibfile '~A' already exists" dest))))
    (if (directoryp srcnib)
        (recursive-copy-directory srcnib dest)
        (copy-file srcnib dest))))

;;; BASENAME path
;;; returns the final component of a pathname--that is, the
;;; filename (with type extension) if it names a file, or the
;;; last directory name if it names a directory

(defun basename (path)
  ;; first probe to see whether the path exists.  if it does, then
  ;; PROBE-FILE returns a canonical pathname for it which, among other
  ;; things, ensures the pathame represents a directory if it's really
  ;; a directory, and a file if it's really a file
  (let* ((path (or (probe-file path)
                   path))
         (dir (pathname-directory path))
         (name (pathname-name path))
         (type (pathname-type path)))
    (if name
        (if type
            (make-pathname :name name :type type)
            (make-pathname :name name))
        ;; it's possible to have a pathname with a type but no name
        ;; e.g. "/Users/foo/.emacs"
        (if type
            (make-pathname :type type)
            (make-pathname :directory (first (last dir)))))))

;;; PATH (&rest components)
;;; returns a pathname. The input COMPONENTS are treated as 
;;; directory names, each contained in the one to the left, except
;;; for the last. The last is treated as a directory if it ends
;;; with a path separator, and a file if it doesn't
(defun path (&rest components)
  (if (null components)
      (pathname "")
      (if (null (cdr components))
          (pathname (car components))
          (merge-pathnames (apply #'path (cdr components))
                           (ensure-directory-pathname (car components))))))


;;; WRITE-PKGINFO path package-type bundle-signature
;;; Writes a PkgInfo file of the sort used by Cocoa applications
;;; to identify their package types and signatures. Writes
;;; PACKAGE-TYPE and BUNDLE-SIGNATURE to the file at PATH,
;;; clobbering it if it already exists.
(defun write-pkginfo (path package-type bundle-signature)
  (with-open-file (out path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format out "~A~A" package-type bundle-signature)))

;;; WRITE-INFO-PLIST path name package-type bundle-signature
;;; Reads the Info.plist file of the running IDE application
;;; into an NSMutableDictionary; sets the name, package-type,
;;; and bundle-signature from the inputs; writes the changed
;;; dictionary to a new Info.plist file at PATH.
;;;
;;; TODO: this function is extremely specialized to the case
;;;       of writing an Info.plist for an app bundle that is
;;;       copied from the IDE. Should separate the IDE-specific
;;;       behavior from more general behavior that can be used
;;;       by the batch builder, which does not depend on the IDE.
(defun write-info-plist (path name package-type bundle-signature
                         &key main-nib-name)
  ;; read the Info.plist of the IDE app, change
  ;; the fields needed, write the results to PATH
  (assert (or (null main-nib-name)
              (stringp main-nib-name))
          (main-nib-name)
          "The main-nib-name must be a string or NIL, not ~S" main-nib-name)
  (with-autorelease-pool
    (let* ((bundle-name-key (%make-nsstring "CFBundleName"))
           (bundle-name-str (%make-nsstring name))
           (type-key (%make-nsstring "CFBundlePackageType"))
           (type-str (%make-nsstring package-type))
           (sig-key (%make-nsstring "CFBundleSignature"))
           (sig-str (%make-nsstring bundle-signature))
           (ide-bundle (#/mainBundle ns:ns-bundle))
           (ide-bundle-path-nsstring (#/bundlePath ide-bundle))
           (ide-bundle-path (ensure-directory-pathname 
			     (lisp-string-from-nsstring ide-bundle-path-nsstring)))
           (ide-plist-path-str (namestring (path ide-bundle-path 
                                                 "Contents" "Info.plist")))
           (info-dict (#/dictionaryWithContentsOfFile: ns:ns-mutable-dictionary 
                                                       ide-plist-path-str))
           (app-name-key (%make-nsstring "CFBundleExecutable"))
           (app-name-str (%make-nsstring name))
           (app-plist-path-str (%make-nsstring (namestring path))))
      (#/setValue:forKey: info-dict bundle-name-str bundle-name-key)
      (#/setValue:forKey: info-dict app-name-str app-name-key)
      (#/setValue:forKey: info-dict type-str type-key)
      (#/setValue:forKey: info-dict sig-str sig-key)
      (when main-nib-name
        (#/setValue:forKey: info-dict 
                            (%make-nsstring main-nib-name)
                            #@"NSMainNibFile"))
      (#/writeToFile:atomically: info-dict app-plist-path-str #$YES))))

;;; MAKE-APPLICATION-BUNDLE name package-type bundle-signature project-path
;;; Build the directory structure of a Cocoa application bundle and
;;; populate it with the required PkgInfo and Info.plist files.
(defun make-application-bundle (name package-type bundle-signature project-path
                                &key main-nib-name)
  (let* ((app-bundle (path project-path 
			   (ensure-directory-pathname (concatenate 'string name ".app"))))
         (contents-dir (path app-bundle (ensure-directory-pathname "Contents")))
         (macos-dir (path contents-dir (ensure-directory-pathname "MacOS")))
         (rsrc-dir (path contents-dir  "Resources" 
                         (ensure-directory-pathname "English.lproj"))))
    (ensure-directories-exist macos-dir)
    (ensure-directories-exist rsrc-dir)
    (write-info-plist (path app-bundle "Contents" "Info.plist")
                      name package-type bundle-signature :main-nib-name main-nib-name)
    (write-pkginfo (path app-bundle "Contents" "PkgInfo")
                   package-type bundle-signature)
    app-bundle))