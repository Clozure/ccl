;;;; 03 — AppKit NSApplication without IDE / without .app principal class.
;;;; Marker: 03-APPKIT-SHARED-APPLICATION-OK
(in-package :ccl)
(load "ccl:tools;darwin-cocoa-apps;apps-lib.lisp")
(require "OBJC-SUPPORT")
(cocoa-apps-on-main
 (lambda ()
   (objc:with-autorelease-pool
     (let ((app (#/sharedApplication ns:ns-application)))
       (when (%null-ptr-p app)
         (error "sharedApplication returned null"))
       ;; NSApplicationActivationPolicyAccessory = 1 (no dock spam)
       (#/setActivationPolicy: app 1)
       (format t "~&app=~s~%" app)))))
(format t "~&03-APPKIT-SHARED-APPLICATION-OK~%")
(quit 0)
