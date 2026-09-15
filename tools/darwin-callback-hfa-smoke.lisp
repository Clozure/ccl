;;;; arm64 callback must unpack NSRect HFA from V regs (not as by-ref ptr).
;;;;   ./tools/run-darwin-ide-smoke.sh 180 tools/darwin-callback-hfa-smoke.lisp DARWIN-CALLBACK-HFA-OK
(in-package :ccl)

(defun %p (fmt &rest args)
  (apply #'format t fmt args) (terpri) (force-output))

(require "COCOA")
(timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 60)

(call-in-initial-process
 #'(lambda ()
     (objc:with-autorelease-pool
       (let* ((r (ns:make-ns-rect 10d0 20d0 30d0 40d0))
              (v (#/initWithFrame: (#/alloc gui::text-pane) r))
              (f (#/frame v)))
         (%p "text-pane initWithFrame => ~s ~s ~s ~s"
             (ns:ns-rect-x f) (ns:ns-rect-y f)
             (ns:ns-rect-width f) (ns:ns-rect-height f))
         (assert (= 10d0 (ns:ns-rect-x f)))
         (assert (= 20d0 (ns:ns-rect-y f)))
         (assert (= 30d0 (ns:ns-rect-width f)))
         (assert (= 40d0 (ns:ns-rect-height f))))
       (let* ((r (ns:make-ns-rect 11d0 22d0 33d0 44d0))
              (v (make-instance 'gui::text-pane :with-frame r))
              (f (#/frame v)))
         (%p "text-pane make-instance => ~s ~s ~s ~s"
             (ns:ns-rect-x f) (ns:ns-rect-y f)
             (ns:ns-rect-width f) (ns:ns-rect-height f))
         (assert (= 11d0 (ns:ns-rect-x f)))
         (assert (= 44d0 (ns:ns-rect-height f)))))))

(format t "~&DARWIN-CALLBACK-HFA-OK~%")
(force-output)
(#_exit 0)
