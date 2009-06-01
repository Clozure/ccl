(in-package "CCL")

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

(load "ccl:cocoa-ide;defsystem.lisp")
(load-ide)
