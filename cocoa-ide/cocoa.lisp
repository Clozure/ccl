(in-package "CCL")

(defvar *cocoa-application-path* #+gz "ccl:GZ temp bundle.app;" #-gz "ccl:temp bundle.app;")
(defvar *cocoa-application-copy-headers-p* nil)
(defvar *cocoa-application-install-altconsole* nil)
(load "ccl:cocoa-ide;defsystem.lisp")
(load-ide)
