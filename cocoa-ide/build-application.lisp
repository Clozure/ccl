;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          build-application.lisp
;;;; Version:       0.9
;;;; Project:       Cocoa application builder
;;;; Purpose:       the in-process application builder
;;;;
;;;; ***********************************************************************

(require "builder-utilities")

(in-package :ccl)


;;; TODO: 
;;;  1. make a way to specify a user-defined Info.plist in build-application
;;;  2. make a way to specify a user-defined app delegate in build-application
(defun build-application (&key
                          (name $default-application-bundle-name)
                          (type-string $default-application-type-string)
                          (creator-string $default-application-creator-string)
                          (directory (current-directory))
                          (copy-ide-resources t) ; whether to copy the IDE's resources
                          (nibfiles nil) ; a list of user-specified nibfiles
                                        ; to be copied into the app bundle
                          (main-nib-name) ; the name of the nib that is to be loaded
                                        ; as the app's main. this name gets written
                                        ; into the Info.plist on the "NSMainNibFile" key
                          (application-class 'gui::cocoa-application)
                          (toplevel-function nil))

  (let* ((info-plist (if copy-ide-resources
                         (get-ide-bundle-info-plist)
                         (make-info-dict)))
         (ide-bundle (#/mainBundle ns:ns-bundle))
         (ide-bundle-path-nsstring (#/bundlePath ide-bundle))
         (ide-bundle-path (pathname 
                           (ensure-directory-pathname 
                            (lisp-string-from-nsstring ide-bundle-path-nsstring))))
         ;; create the bundle directory
         (app-bundle (make-application-bundle :name name 
                                              :project-path directory))
         (image-path (namestring (path app-bundle "Contents" "MacOS" name))))
    ;; copy IDE resources to the bundle
    (when copy-ide-resources
      (recursive-copy-directory (path ide-bundle-path "Contents" "Resources/")
                                (path app-bundle  "Contents" "Resources/")
                                :if-exists :overwrite))
    ;; write Info.plist
    (write-info-plist info-plist (path app-bundle "Contents" "Info.plist")
                      name type-string creator-string :main-nib-name main-nib-name)
    ;; write Pkginfo
    (write-pkginfo (path app-bundle "Contents" "PkgInfo")
                   type-string creator-string)
    ;; copy user nibfiles into the bundle
    (when nibfiles
      (let ((nib-paths (mapcar #'pathname nibfiles)))
        (assert (and (every #'probe-file nib-paths))
                (nibfiles)
                "The nibfiles parameter must be a list of valid pathnames to existing files or directories")
        (dolist (n nib-paths)
          (let ((dest (path app-bundle  "Contents" "Resources" "English.lproj/")))
            (copy-nibfile n dest :if-exists :overwrite)))))
    ;; save the application image into the bundle
    (save-application image-path
                      :application-class application-class
                      :toplevel-function toplevel-function
                      :prepend-kernel t)))

#|
(require :build-application)
(load "/usr/local/ccl/trunk/source/cocoa-ide/builder-utilities.lisp")
(ccl::build-application :name "Foo"
                        :directory "/Users/mikel/Desktop"
                        :copy-ide-resources t)
|#