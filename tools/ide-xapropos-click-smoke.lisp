;;;; xapropos click regression: select a row and run the double-click
;;;; action (#/inspect:), then the same with no selection (row -1).
;;;; Neither may wedge the Cocoa event thread (historically: unguarded
;;;; row -1 aref errored, and the broken callback error-return
;;;; trampoline turned the error into a dead event loop / IDE hang).
;;;;
;;;; Forms are small and fully package-qualified: the tty listener is
;;;; reset when the IDE finishes launching and re-reads stdin in
;;;; CL-USER (pre-existing behavior); the settle sleep and CL-USER
;;;; helpers keep the file robust to that.
;;;;
;;;;   env CFProcessPath=".../Clozure CL64.app/Contents/MacOS/darm64cl" \
;;;;     ./darm64cl -I ./darm64cl.image --no-init --batch < tools/ide-xapropos-click-smoke.lisp
;;;;   Pass: /tmp/click3.out ends with "CLICK3-OK: event loop alive"
(cl:require "COCOA")
(ccl::timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 60)
(cl:sleep 4)
(cl:defun cl-user::lg3 (fmt cl:&rest a)
  (cl:with-open-file (s "/tmp/click3.out" :direction :output
                        :if-exists :append :if-does-not-exist :create)
    (cl:apply #'cl:format s fmt a) (cl:terpri s)))
(cl:ignore-errors (cl:delete-file "/tmp/click3.out"))
(cl-user::lg3 "start pkg=~a" cl:*package*)
(cl:defparameter cl-user::gdone (ccl:make-semaphore))
(gui::execute-in-gui (cl:lambda ()
  (cl:handler-case
      (objc:with-autorelease-pool
        (objc:objc-message-send
         (objc:objc-message-send ccl::*nsapp* "delegate")
         "showXaproposWindow:" :address (ccl:%null-ptr) :void)
        (cl:let ((wc gui::*xapropos-window-controller*))
          (cl:setf (gui::external-only-p wc) cl:nil)
          (gui::apropos-search wc "car")
          (cl-user::lg3 "search started")))
    (cl:error (c) (cl-user::lg3 "open ERROR: ~a" c)))
  (ccl:signal-semaphore cl-user::gdone)))
(ccl:timed-wait-on-semaphore cl-user::gdone 30)
(cl:loop repeat 300
         until (cl:let ((wc gui::*xapropos-window-controller*))
                 (cl:and wc (cl:plusp (cl:length (gui::matched-symbols wc)))))
         do (cl:sleep 0.1))
(cl-user::lg3 "matched=~d" (cl:length (gui::matched-symbols gui::*xapropos-window-controller*)))
(cl:defparameter cl-user::gdone2 (ccl:make-semaphore))
(gui::execute-in-gui (cl:lambda ()
  (cl:handler-case
      (objc:with-autorelease-pool
        (cl:let* ((wc gui::*xapropos-window-controller*)
                  (tv (gui::table-view wc)))
          (objc:objc-message-send tv "reloadData" :void)
          (objc:objc-message-send
           tv "selectRowIndexes:byExtendingSelection:"
           :id (objc:objc-message-send ns:ns-index-set "indexSetWithIndex:"
                                       :unsigned-long 0 :id)
           :<BOOL> 0 :void)
          (cl-user::lg3 "selected: ~a"
                        (cl:aref (gui::matched-symbols wc) 0))
          (objc:objc-message-send wc "inspect:" :address (ccl:%null-ptr) :void)
          (cl-user::lg3 "inspect: returned")
          (objc:objc-message-send tv "deselectAll:" :address (ccl:%null-ptr) :void)
          (objc:objc-message-send wc "inspect:" :address (ccl:%null-ptr) :void)
          (cl-user::lg3 "row -1 inspect: returned")))
    (cl:error (c) (cl-user::lg3 "click ERROR: ~a" c)))
  (ccl:signal-semaphore cl-user::gdone2)))
(cl:if (ccl:timed-wait-on-semaphore cl-user::gdone2 30)
  (cl-user::lg3 "click phase done")
  (cl-user::lg3 "TIMEOUT: event thread wedged after click"))
(cl:sleep 2)
(cl:defparameter cl-user::gdone3 (ccl:make-semaphore))
(gui::queue-for-gui (cl:lambda () (ccl:signal-semaphore cl-user::gdone3)))
(cl:if (ccl:timed-wait-on-semaphore cl-user::gdone3 15)
  (cl-user::lg3 "CLICK3-OK: event loop alive")
  (cl-user::lg3 "FAIL: event loop dead"))
(ccl:quit 0)
