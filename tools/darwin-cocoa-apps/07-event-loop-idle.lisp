;;;; 07 — Bounded main-thread run-loop pump.
;;;; Marker: 07-EVENT-LOOP-IDLE-OK
(in-package :ccl)
(load "ccl:tools;darwin-cocoa-apps;apps-lib.lisp")
(require "OBJC-SUPPORT")

(cocoa-apps-on-main
 (lambda ()
   (objc:with-autorelease-pool
     (let* ((app (#/sharedApplication ns:ns-application))
            (rl (#/currentRunLoop ns:ns-run-loop))
            (until (#/dateWithTimeIntervalSinceNow: ns:ns-date 0.2d0)))
       (#/setActivationPolicy: app 1)
       ;; Single bounded pump.  Do not spin on a past date — runMode can
       ;; return true immediately forever once until is in the past.
       (#/runMode:beforeDate: rl #&NSDefaultRunLoopMode until)
       (format t "~&event-loop pumped~%")))))

(format t "~&07-EVENT-LOOP-IDLE-OK~%")
(quit 0)
