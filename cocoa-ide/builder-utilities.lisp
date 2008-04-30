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

(defun %temp-nsstring (s) (#/autorelease (%make-nsstring s)))

;;; Info Defaults
;;; Some useful values for use when creating application bundles

(defparameter $default-application-bundle-name "MyApplication")
(defparameter $default-application-type-string "APPL")
(defparameter $default-application-creator-string "OMCL")
(defparameter $default-application-version-number "1.0")

;;; defaults related to Info.plist files
(defparameter $cfbundle-development-region-key #@"CFBundleDevelopmentRegion")
(defparameter $default-info-plist-development-region "English")

(defparameter $cfbundle-executable-key #@"CFBundleExecutable")
(defparameter $default-info-plist-executable $default-application-bundle-name)

(defparameter $cfbundle-getinfo-string-key #@"CFBundleGetInfoString")
(defparameter $default-info-plist-getInfo-string "\"1.0 Copyright © 2008\"")

(defparameter $cfbundle-help-book-folder-key #@"CFBundleHelpBookFolder")
(defparameter $default-info-plist-help-book-folder "MyApplicationHelp")

(defparameter $cfbundle-help-book-name-key #@"CFBundleHelpBookName")
(defparameter $default-info-plist-help-book-name "\"MyApplication Help\"")

(defparameter $cfbundle-icon-file-key #@"CFBundleIconFile")
(defparameter $default-info-plist-icon-file "\"MyApplication.icns\"")

(defparameter $cfbundle-bundle-identifier-key #@"CFBundleIdentifier")
(defparameter $default-info-plist-bundle-identifier "\"com.clozure.apps.myapplication\"")

(defparameter $cfbundle-dictionary-version-key #@"CFBundleInfoDictionaryVersion")
(defparameter $default-info-dictionary-version "\"6.0\"")

(defparameter $cfbundle-bundle-name-key #@"CFBundleName")
(defparameter $default-info-plist-bundle-name "MyApplication")

(defparameter $cfbundle-bundle-package-type-key #@"CFBundlePackageType")
(defparameter $default-info-plist-bundle-package-type "APPL")

(defparameter $cfbundle-short-version-string-key #@"CFBundleShortVersionString")
(defparameter $default-info-plist-short-version-string "\"1.0\"")

(defparameter $cfbundle-bundle-signature-key #@"CFBundleSignature")
(defparameter $default-info-plist-bundle-signature "OMCL")

(defparameter $cfbundle-version-key #@"CFBundleVersion")
(defparameter $default-info-plist-version "\"1.0\"")

(defparameter $ls-has-localized-display-name-key #@"LSHasLocalizedDisplayName")
(defparameter $default-info-plist-has-localized-display-name "0")

(defparameter $ls-minimum-system-version-key #@"LSMinimumSystemVersion")
(defparameter $default-info-plist-minimum-system-version "\"10.5\"")

(defparameter $ns-main-nib-file-key #@"NSMainNibFile")
(defparameter $default-info-plist-main-nib-file "MainMenu")

(defparameter $ns-principal-class-key #@"NSPrincipalClass")
(defparameter $default-info-plist-principal-class "LispApplication")

;;; COPY-NIBFILE (srcnib dest-directory &key (if-exists :overwrite))
;;; Copies a nibfile (which may in fact be a directory) to the
;;; destination path (which may already exist, and may need to
;;; be overwritten

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

;;; MAKE-INFO-DICT
;;; returns a newly-created NSDictionary with contents
;;; specified by the input parameters
(defun make-info-dict (&key
                       (development-region $default-info-plist-development-region)
                       (executable $default-info-plist-executable)
                       (getinfo-string $default-info-plist-getinfo-string)
                       (help-book-folder $default-info-plist-help-book-folder)
                       (help-book-name $default-info-plist-help-book-name)
                       (icon-file $default-info-plist-icon-file)
                       (bundle-identifier $default-info-plist-bundle-identifier)
                       (dictionary-version $default-info-dictionary-version)
                       (bundle-name $default-info-plist-bundle-name)
                       (bundle-package-type $default-info-plist-bundle-package-type)
                       (short-version-string $default-info-plist-short-version-string)
                       (bundle-signature $default-info-plist-bundle-signature)
                       (version $default-info-plist-version)
                       (has-localized-display-name $default-info-plist-has-localized-display-name)
                       (minimum-system-version $default-info-plist-minimum-system-version)
                       (main-nib-file $default-info-plist-main-nib-file)
                       (principal-class $default-info-plist-principal-class))
  (#/dictionaryWithObjectsAndKeys: ns:ns-dictionary
                                   (%temp-nsstring development-region) $cfbundle-development-region-key
                                   (%temp-nsstring executable) $cfbundle-executable-key
                                   (%temp-nsstring getinfo-string) $cfbundle-getinfo-string-key
                                   (%temp-nsstring help-book-folder) $cfbundle-help-book-folder-key
                                   (%temp-nsstring help-book-name) $cfbundle-help-book-name-key
                                   (%temp-nsstring icon-file) $cfbundle-icon-file-key
                                   (%temp-nsstring bundle-identifier) $cfbundle-bundle-identifier-key
                                   (%temp-nsstring dictionary-version) $cfbundle-dictionary-version-key
                                   (%temp-nsstring bundle-name) $cfbundle-bundle-name-key
                                   (%temp-nsstring bundle-package-type) $cfbundle-bundle-package-type-key
                                   (%temp-nsstring short-version-string) $cfbundle-short-version-string-key
                                   (%temp-nsstring bundle-signature) $cfbundle-bundle-signature-key
                                   (%temp-nsstring version) $cfbundle-version-key
                                   (%temp-nsstring has-localized-display-name) $ls-has-localized-display-name-key
                                   (%temp-nsstring minimum-system-version) $ls-minimum-system-version-key
                                   (%temp-nsstring main-nib-file) $ns-main-nib-file-key
                                   (%temp-nsstring principal-class) $ns-principal-class-key
                                   +null-ptr+))

;;; READ-INFO-PLIST info-path
;;; returns a newly-created NSDictionary with the contents
;;; of the plist file at INFO-PATH 
(defun read-info-plist (info-path)
  (let* ((info-path (pathname info-path)) ; make sure it's a pathname to start
         (verified-path (probe-file info-path)))
    (assert (and verified-path
                 (string-equal (pathname-type verified-path) "plist"))
            (info-path)
            "The input path for READ-INFO-PLIST must be the name of a valid 'plist' file.")
    (let* ((info-path-str (namestring info-path)))
      (#/dictionaryWithContentsOfFile: ns:ns-mutable-dictionary 
                                       info-path-str))))

(defun info-dict-from-specification (spec)
  (cond
    ((typep spec 'ns:ns-dictionary) spec)
    ((and (typep spec 'pathname)
          (string-equal (pathname-type spec) "plist")
          (probe-file spec)) (read-info-plist spec))
    (t (error "Argument to INFO-DICT-FROM-SPECIFICATION must be the pathname of a valid plist file or an instance of NSDictionary"))))

;;; WRITE-INFO-PLIST info-plist path name package-type bundle-signature 
;;; sets the name, package-type, and bundle-signature of the
;;; info-plist from the inputs; writes the changed dictionary to a new
;;; Info.plist file at PATH.

(defun write-info-plist (info-dict out-path name package-type bundle-signature
                         &key main-nib-name)
  ;; change the fields needed, write the results to PATH
  (assert (or (null main-nib-name)
              (stringp main-nib-name))
          (main-nib-name)
          "The main-nib-name must be a string or NIL, not ~S" main-nib-name)
  (with-autorelease-pool
    (let* ((bundle-name-str (%make-nsstring name))
           (type-str (%make-nsstring package-type))
           (sig-str (%make-nsstring bundle-signature))
           (app-name-str (%make-nsstring name))
           (app-plist-path-str (%make-nsstring (namestring out-path))))
      (#/setValue:forKey: info-dict bundle-name-str $cfbundle-bundle-name-key)
      (#/setValue:forKey: info-dict app-name-str $cfbundle-executable-key)
      (#/setValue:forKey: info-dict type-str $cfbundle-bundle-package-type-key)
      (#/setValue:forKey: info-dict sig-str $cfbundle-bundle-signature-key)
      (when main-nib-name
        (#/setValue:forKey: info-dict 
                            (%make-nsstring main-nib-name)
                            $ns-main-nib-file-key))
      (#/writeToFile:atomically: info-dict app-plist-path-str #$YES))))

(defun get-ide-bundle-info-plist ()
  (let* ((ide-bundle (#/mainBundle ns:ns-bundle))
         (ide-bundle-path-nsstring (#/bundlePath ide-bundle))
         (ide-bundle-path (ensure-directory-pathname 
                           (lisp-string-from-nsstring ide-bundle-path-nsstring)))
         (ide-plist-path-str (namestring (path ide-bundle-path 
                                               "Contents" "Info.plist"))))
    (read-info-plist ide-plist-path-str)))

;;; MAKE-APPLICATION-BUNDLE name package-type bundle-signature project-path
;;; Build the directory structure of a Cocoa application bundle and
;;; populate it with the required PkgInfo and Info.plist files.
(defun make-application-bundle (&key 
                                (name $default-application-bundle-name)
                                (project-path (current-directory)))
  (let* ((app-bundle (path project-path 
                           (ensure-directory-pathname (concatenate 'string name ".app"))))
         (contents-dir (path app-bundle (ensure-directory-pathname "Contents")))
         (macos-dir (path contents-dir (ensure-directory-pathname "MacOS")))
         (rsrc-dir (path contents-dir  "Resources" 
                         (ensure-directory-pathname "English.lproj"))))
    (ensure-directories-exist macos-dir)
    (ensure-directories-exist rsrc-dir)
    app-bundle))