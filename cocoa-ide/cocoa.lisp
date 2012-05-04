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
