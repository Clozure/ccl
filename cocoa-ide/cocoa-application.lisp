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


(defvar *cocoa-application-path*
  (multiple-value-bind (os bits cpu) (host-platform)
    (declare (ignore os))
    (format nil "ccl:Clozure CL-~a~a.app;"
            (string-downcase cpu) bits)))
(defvar *cocoa-application-copy-headers-p* nil)
(defvar *cocoa-application-install-altconsole* t)
(load "ccl:cocoa-ide;defsystem.lisp")
(load-ide)

;;; If things go wrong, you might see some debugging information via
;;; the OSX console (/Applications/Utilities/Console.app.)  Standard
;;; and error output for the initial lisp process will be directed
;;; there.
(build-ide *cocoa-application-path*)
