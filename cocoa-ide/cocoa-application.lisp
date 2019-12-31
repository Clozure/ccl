;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 2002-2003 Clozure Associates
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


(in-package "CCL")			; for now.

#+windows-target
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cocotron *features*))

#+(or mac-app-store standalone-ide)
(defvar *cocoa-ide-path* "ccl:Clozure CL.app;")
#-(or mac-app-store standalone-ide)
(defvar *cocoa-ide-path*
  (let* ((bits (nth-value 1 (host-platform))))
    (format nil "ccl:Clozure CL~a.app;" bits)))

(defvar *cocoa-ide-copy-headers-p* t)
(defvar *cocoa-ide-install-altconsole* t)

(defvar *cocoa-ide-bundle-suffix*
  (multiple-value-bind (os bits cpu) (host-platform)
    (declare (ignore os))
    #+mac-app-store
    (format nil "store.ccl-~a~a" (string-downcase cpu) bits)
    #+standalone-ide
    (format nil "ccl-~a~a" (string-downcase cpu) bits)
    #-(or mac-app-store standalone-ide)
    (format nil "Clozure CL-~a~a" (string-downcase cpu) bits)))

(defvar *cocoa-ide-frameworks* #+cocotron '("ccl:cocotron;Foundation.framework;" "ccl:cocotron;AppKit.framework;" "ccl:cocotron;CoreData.framework;") #-cocotron nil)
(defvar *cocoa-ide-libraries* #+cocotron '("ccl:cocotron;Foundation>.1>.0.dll" "ccl:cocotron;AppKit>.1>.0.dll" "ccl:cocotron;CoreData>.1>.0.dll") #-cocotron nil)

(defvar *cocoa-ide-force-compile* #+(or mac-app-store standalone-ide) t
	                          #-(or mac-app-store standalone-ide) nil)
(load "ccl:cocoa-ide;defsystem.lisp")
(load-ide *cocoa-ide-force-compile*)


  

;;; If things go wrong, you might see some debugging information via
;;; the OSX console (/Applications/Utilities/Console.app.)  Standard
;;; and error output for the initial lisp process will be directed
;;; there.
(build-ide *cocoa-ide-path*)
