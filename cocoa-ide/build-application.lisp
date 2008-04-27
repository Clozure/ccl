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

;;; about copying nibfiles

;;; when building an app bundle, we copy nibfiles from the development
;;; environment appplication bundle into the newly-created application
;;; bundle. If user-supplied nibfiles are given the same names as
;;; nibfiles from the development environment, we signal an error and
;;; refuse to copy the user nibfiles. This treatment ensures that users
;;; will not accidentally clobber dev-environment nibfiles, but also
;;; means that they must give unique names to their own nibs in order
;;; to use them with their saved applications.

;;; in future, we may add options to suppress the copying of
;;; dev-environment nibfiles.

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

  (let* ((ide-bundle (#/mainBundle ns:ns-bundle))
         (ide-bundle-path-nsstring (#/bundlePath ide-bundle))
         (ide-bundle-path (pathname 
                           (ensure-directory-pathname 
                            (lisp-string-from-nsstring ide-bundle-path-nsstring))))
         ;; create the bundle directory
         (app-bundle (make-application-bundle name type-string creator-string directory
                                              :main-nib-name main-nib-name))
         (image-path (namestring (path app-bundle "Contents" "MacOS" name))))
    ;; copy IDE resources to the bundle
    (when copy-ide-resources
      (recursive-copy-directory (path ide-bundle-path "Contents" "Resources/")
                                (path app-bundle  "Contents" "Resources/")
                                :if-exists :overwrite))
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