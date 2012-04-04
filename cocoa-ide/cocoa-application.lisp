;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2002-2003 Clozure Associates
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html


(in-package "CCL")			; for now.

#+windows-target
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cocotron *features*))


(defvar *cocoa-application-path*
  (let* ((bits (nth-value 1 (host-platform))))
    (format nil "ccl:Clozure CL~a.app;" bits)))
(defvar *cocoa-application-copy-headers-p* nil)
(defvar *cocoa-application-install-altconsole* t)
(defvar *cocoa-application-bundle-suffix*
  (multiple-value-bind (os bits cpu) (host-platform)
    (declare (ignore os))
    (format nil "Clozure CL-~a~a" (string-downcase cpu) bits)))
(defvar *cocoa-application-frameworks* #+cocotron '("ccl:cocotron;Foundation.framework;" "ccl:cocotron;AppKit.framework;" "ccl:cocotron;CoreData.framework;") #-cocotron nil)
(defvar *cocoa-application-libraries* #+cocotron '("ccl:cocotron;Foundation>.1>.0.dll" "ccl:cocotron;AppKit>.1>.0.dll" "ccl:cocotron;CoreData>.1>.0.dll") #-cocotron nil)
        
(defvar *cocoa-ide-force-compile* nil)
(load "ccl:cocoa-ide;defsystem.lisp")
(load-ide *cocoa-ide-force-compile*)

#+darwin-target
;;; Nuke any command-line arguments, to keep the Cocoa runtime from
;;; trying to process them.
(let* ((argv (foreign-symbol-address "NXArgv"))
       (argc (foreign-symbol-address "NXArgc")))
  (when argc
    (setf (pref argc :int) 1))
  (when argv
    (setf (paref (%get-ptr argv) (:* :char) 1) +null-ptr+)))
  

;;; If things go wrong, you might see some debugging information via
;;; the OSX console (/Applications/Utilities/Console.app.)  Standard
;;; and error output for the initial lisp process will be directed
;;; there.
(build-ide *cocoa-application-path*)
