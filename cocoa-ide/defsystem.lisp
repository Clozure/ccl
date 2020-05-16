;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 2007 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-interface-dir :cocoa))

;; These are used to communicate with ide-bundle, which must be loaded before objc-support.
;; They are defvar'ed so the caller can set them before loading us.
(defvar *cocoa-ide-path* nil)
(defvar *cocoa-ide-copy-headers-p* nil)
(require "IDE-BUNDLE")

(require "OBJC-SUPPORT")

(define-special-objc-word "IDE")
(define-special-objc-word "CCL")

(defpackage "GUI"
  (:use :common-lisp :ccl)
  (:import-from
   "CCL"
   ;; symbols defined here
   *cocoa-ide-path*
   *cocoa-ide-copy-headers-p*
   load-ide
   build-ide
   ;; Misc symbols that perhaps should be exported from CCL but currently aren't.
   report-bad-arg
   native-translated-namestring
   make-id-map
   assign-id-map-id
   id-map-free-object
   process-thread
   process-serial-number
   ensure-directory-pathname
   recursive-copy-directory
   application
   ;; Symbols that perhaps should be exported by ObjC support but aren't
   @selector
   *nsapp*
   with-nsstr
   %make-nsstring
   lisp-string-from-nsstring
   with-autorelease-pool
   ns-height
   ns-width
   *cocoa-event-process*
   create-autorelease-pool
   release-autorelease-pool
   release-canonical-nsobject
   objc-message-send
   open-main-bundle
   )
  (:export
   "ABSTRACT-NS-LISP-STRING"
   "NS-LISP-STRING"
   "NS-LISP-STRING-STRING"
   "BACKGROUND-PROCESS-RUN-FUNCTION"

   "EXECUTE-IN-GUI"
   ))

(defparameter *mac-ui-files*
  '("cf-utils"
    "libdispatch"
    "ccl-application"
    "event-process"
    "cg"))

(defvar *use-pre-lion-search-files* nil "User-settable parameter to prefer old search-files behavior,
  even if you're running on Lion or later. Must set this to true BEFORE doing (require :cocoa-application).")

(defun use-pre-lion-search-files ()
  (or *use-pre-lion-search-files*
      (< (parse-integer (software-version) :junk-allowed t) 11) ; no choice on pre-lion systems
      ))

(defparameter *ide-files*
  `(;"ide-bundle" - loaded by hand above
    "constants"
    "ide-application"
    "cocoa-utils"
    "cocoa-defaults"
    "cocoa-typeout"
    "console-window"
    "console-log"
    "cocoa-window"
    "cocoa-doc"
    "compile-hemlock"
    "hemlock"  ;; treated specially below, compile-hemlock must come before.
    "cocoa-editor"
    "cocoa-listener"
    ,(if (use-pre-lion-search-files)
         "cocoa-grep-pre-lion"
         "cocoa-grep")
    "cocoa-backtrace"
    "inspector"
    "cocoa-remote-lisp"
    "preferences-views"
    "preferences"
    "processes-window"
    "apropos-window"
    #-mac-app-store
    "xapropos"
    "hemlock-commands"
    "file-dialogs"
    "menus"
    "app-delegate"
    #-mac-app-store
    "ide-self-update"
    ,(if (use-pre-lion-search-files)
         "search-files-pre-lion"
         "search-files")
    "start"
    #-mac-app-store
    "xinspector"
    ))

(defun load-mac-ui-files (names mac-ui-dir force-compile)
  (let* ((bin-dir (merge-pathnames ";fasls;" mac-ui-dir)))
    (ensure-directories-exist bin-dir)
    (with-compilation-unit ()
      (dolist (name names)
	(let* ((source (make-pathname :name name
				      :type (pathname-type *.lisp-pathname*)
				      :defaults mac-ui-dir))
	       (fasl (make-pathname :name name
				    :type (pathname-type *.fasl-pathname*)
				    :defaults bin-dir)))
	  (when (or force-compile
		    (not (probe-file fasl))
		    (> (file-write-date source)
		       (file-write-date fasl)))
	    (compile-file source :output-file fasl :verbose t))
	  (load fasl :verbose t))))))

(defun load-ide-files (names src-dir force-compile)
  (declare (special *hemlock-files*)) ;; kludge
  (let* ((bin-dir (merge-pathnames ";fasls;" src-dir)))
    (ensure-directories-exist bin-dir)
    (with-compilation-unit ()
      (dolist (name names)
        (let* ((source (make-pathname :name name :type (pathname-type *.lisp-pathname*)
                                      :defaults src-dir))
               (fasl (make-pathname :name name :type (pathname-type *.fasl-pathname*)
                                    :defaults bin-dir))
               (sources (cons source
                              (and (equalp name "hemlock")
                                   ;; This is defined in compile-hemlock, which is loaded first
                                   (mapcar #'hemlock-source-pathname *hemlock-files*)))))
          (if (needs-compile-p fasl sources force-compile)
            (progn
              ;; Once compile something, keep compiling, in case macros changed.
              (setq force-compile t)
              (compile-file source :output-file fasl :verbose t :load t))
            (load fasl :verbose t))))))
  force-compile)

(defun load-ide (&optional force-compile)
  (load-mac-ui-files *mac-ui-files* "ccl:mac-ui;" force-compile)
  (load-ide-files *ide-files* "ccl:cocoa-ide;" force-compile)
  (require 'build-application)
  (provide "COCOA"))
