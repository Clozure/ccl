(in-package "CCL")

(defvar *cocoa-application-path* "ccl:temp bundle.app;")
(defvar *cocoa-application-copy-headers-p* nil)
(load "ccl:cocoa-ide;defsystem.lisp")
(load-ide)
