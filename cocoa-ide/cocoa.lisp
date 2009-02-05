(in-package "CCL")

(defvar *cocoa-application-path* #+gz "ccl:GZ temp bundle.app;" #-gz
        (multiple-value-bind (os bits cpu) (host-platform)
          (declare (ignore os))
          (format nil "ccl:temp bundle-~a~a.app;"
                  (string-downcase cpu) bits)))
(defvar *cocoa-application-copy-headers-p* nil)
(defvar *cocoa-application-install-altconsole* nil)
(load "ccl:cocoa-ide;defsystem.lisp")
(load-ide)
