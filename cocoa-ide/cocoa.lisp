(in-package "CCL")

#+windows-target
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cocotron *features*))

(defvar *cocoa-application-path* #+gz "ccl:GZ temp bundle.app;" #-gz
        (let* ((bits (nth-value 1 (host-platform))))
          (format nil "ccl:temp bundle~a.app;"
                  bits)))
(defvar *cocoa-application-copy-headers-p* nil)
(defvar *cocoa-application-install-altconsole* nil)
(defvar *cocoa-application-bundle-suffix*
  (multiple-value-bind (os bits cpu) (host-platform)
    (declare (ignore os))
    (format nil "temp bundle-~a~a" (string-downcase cpu) bits)))
(defvar *cocoa-ide-force-compile* nil)
(defvar *cocoa-application-frameworks* #+cocotron '("ccl:cocotron;Foundation.framework;" "ccl:cocotron;AppKit.framework;") #-cocotron nil)
(defvar *cocoa-application-libraries* ())

(load "ccl:cocoa-ide;defsystem.lisp")
(load-ide *cocoa-ide-force-compile*)
