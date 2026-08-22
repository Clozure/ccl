;;;; Listener enqueue + eval after modeline-alive path.
;;;;   ./tools/run-darwin-ide-smoke.sh 120 tools/ide-listener-eval.lisp IDE-LISTENER-EVAL-OK
(in-package :ccl)

(defun %p (fmt &rest a)
  (apply #'format t fmt a) (terpri) (force-output)
  (with-open-file (s "/tmp/ide-listener-eval-detail.log" :direction :output
                     :if-exists :append :if-does-not-exist :create)
    (apply #'format s fmt a) (terpri s) (force-output s)))

(ignore-errors (delete-file "/tmp/ide-listener-eval-detail.log"))
(setq *debugger-hook*
      (lambda (c h) (declare (ignore h)) (%p "DBG ~a" c) (force-output) (#_exit 99)))

(defun %cip-wait (f)
  (let ((return-values :unset))
    (let ((wrapper (lambda ()
                     (setq return-values (multiple-value-list (funcall f))))))
      (ccl::%interrupt-event-process wrapper t)
      (when (eq return-values :unset) (error "wrapper did not run"))
      (apply #'values return-values))))

(%p "require")
(require "COCOA")
(%p "finished=~s" (timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 60))

(load (merge-pathnames "tools/ide-modeline-stream-concat-patch.lisp" (ccl-directory)))
(%p "modeline patched")

(defparameter *eval-seen* :unset)
(defparameter *eval-sem* (make-semaphore))

(%p "ensure+display")
(%cip-wait
 (lambda ()
   (#/setActivationPolicy: *nsapp* 0)
   (#/activateIgnoringOtherApps: *nsapp* #$YES)
   (#/ensureListener: (#/delegate *nsapp*) (%null-ptr))
   (dotimes (i (#/count (#/orderedWindows *nsapp*)))
     (#/display (#/objectAtIndex: (#/orderedWindows *nsapp*) i)))
   t))
(%p "shown")

(let* ((doc (#/topListener (find-class 'gui::hemlock-listener-document)))
       (proc (and doc (not (%null-ptr-p doc)) (gui::hemlock-document-process doc))))
  (%p "doc-null=~s proc=~s" (or (null doc) (%null-ptr-p doc)) proc)
  (unless proc (force-output) (#_exit 1))
  (%p "enqueue (+ 40 2)")
  (gui::eval-in-listener-process
   proc
   "(progn (setq ccl::*eval-seen* (+ 40 2)) (signal-semaphore ccl::*eval-sem*) nil)")
  (let ((got (timed-wait-on-semaphore *eval-sem* 30)))
    (%p "wait=~s seen=~s" got *eval-seen*))
  (unless (eql *eval-seen* 42)
    (%p "FAIL expected 42")
    (force-output)
    (#_exit 1))
  (dotimes (i 4)
    (%cip-wait
     (lambda ()
       (dotimes (j (#/count (#/orderedWindows *nsapp*)))
         (#/display (#/objectAtIndex: (#/orderedWindows *nsapp*) j)))
       t))
    (%p "ping ~s" i))
  (format t "~&IDE-LISTENER-EVAL-OK~%")
  (force-output)
  (#_exit 0))
