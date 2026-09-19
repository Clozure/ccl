;;;; 04 — NSWindow init + #/frame struct return (AAPCS64 HFA path).
;;;; Marker: 04-WINDOW-FRAME-OK
(in-package :ccl)
(load "ccl:tools;darwin-cocoa-apps;apps-lib.lisp")
(require "OBJC-SUPPORT")

(cocoa-apps-on-main
 (lambda ()
   (objc:with-autorelease-pool
     (#/sharedApplication ns:ns-application)
     (let* ((rect (ns:make-ns-rect 40d0 60d0 320d0 240d0))
            (w (#/initWithContentRect:styleMask:backing:defer:
                 (#/alloc ns:ns-window)
                 rect
                 (logior 1 2) ; NSWindowStyleMaskTitled|Closable
                 #$NSBackingStoreBuffered
                 #$NO)))
       (when (%null-ptr-p w)
         (error "NSWindow init returned null"))
       (let ((f (#/frame w)))
         (format t "~&frame x=~s y=~s w=~s h=~s~%"
                 (ns:ns-rect-x f) (ns:ns-rect-y f)
                 (ns:ns-rect-width f) (ns:ns-rect-height f))
         ;; Content size was 320x240; frame height includes title-bar chrome.
         (unless (and (= (ns:ns-rect-width f) 320d0)
                      (>= (ns:ns-rect-height f) 240d0))
           (error "bad frame size ~s" f)))
       (#/close w)
       t))))

(format t "~&04-WINDOW-FRAME-OK~%")
(quit 0)
