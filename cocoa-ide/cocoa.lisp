;;;
;;; Copyright 2016 Clozure Associates
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

#+windows-target
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cocotron *features*))

(defvar *cocoa-ide-path* #+gz "ccl:GZ temp bundle.app;" #-gz
        (let* ((bits (nth-value 1 (host-platform))))
          (format nil "ccl:temp bundle~a.app;"
                  bits)))
(defvar *cocoa-ide-copy-headers-p* nil)
(defvar *cocoa-ide-install-altconsole* nil)
(defvar *cocoa-ide-bundle-suffix*
  (multiple-value-bind (os bits cpu) (host-platform)
    (declare (ignore os))
    (format nil "temp bundle-~a~a" (string-downcase cpu) bits)))
(defvar *cocoa-ide-force-compile* nil)
(defvar *cocoa-ide-frameworks* #+cocotron '("ccl:cocotron;Foundation.framework;" "ccl:cocotron;AppKit.framework;" "ccl:cocotron;CoreData.framework;") #-cocotron nil)
(defvar *cocoa-ide-libraries* #+cocotron '("ccl:cocotron;Foundation>.1>.0.dll" "ccl:cocotron;AppKit>.1>.0.dll" "ccl:cocotron;CoreData>.1>.0.dll") #-cocotron nil)

(load "ccl:cocoa-ide;defsystem.lisp")
(load-ide *cocoa-ide-force-compile*)
(gui::start-cocoa-ide)
