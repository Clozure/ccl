;;;; 12 — IDE launch diagnostic with break-hook dump (app-bundle kernel).
;;;; Marker: IDE-LAUNCH-OK
(in-package :ccl)

(defun %12log (fmt &rest args)
  (apply #'format t fmt args) (terpri) (force-output)
  (with-open-file (s "/tmp/ide-launch-12.log" :direction :output
                     :if-exists :append :if-does-not-exist :create)
    (apply #'format s fmt args) (terpri s) (force-output s)))

(ignore-errors (delete-file "/tmp/ide-launch-12.log"))
(ignore-errors (delete-file "/tmp/ide-launch-12.bt"))

(unless (fboundp '%invoke-objc-send-function)
  (error "missing tip CNM"))

(defun %12-dump (tag c)
  (%12log "DUMP ~a: ~a type=~s" tag c (type-of c))
  (ignore-errors
    (with-open-file (s "/tmp/ide-launch-12.bt" :direction :output
                       :if-exists :append :if-does-not-exist :create)
      (let ((*debug-io* s) (*standard-output* s) (*error-output* s)
            (*print-pretty* nil) (*print-length* 40) (*print-level* 6))
        (format s "~%==== ~a ====~%condition: ~a~%type: ~s~%" tag c (type-of c))
        (dolist (p (all-processes))
          (format s "~%-- process ~s --~%" p)
          (ignore-errors
            (print-call-history :process p :detailed-p nil :count 40))))))
  (finish-output)
  (quit 42))

(setq *break-hook*
      (lambda (cond other)
        (declare (ignore other))
        (%12-dump "break-hook" cond)))
(setq *debugger-hook*
      (lambda (c hook)
        (declare (ignore hook))
        (%12-dump "debugger-hook" c)))

(%12log "require COCOA…")
(require "COCOA")
(%12log "modules loaded; wait finished-launching…")

(let ((ok (timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 45)))
  (%12log "finished-launching => ~s" ok)
  (unless ok
    (%12log "processes: ~s" (all-processes))
    (error "IDE did not finish launching")))

(call-in-initial-process
 (lambda ()
   (objc:with-autorelease-pool
     (%12log "nsapp=~s running=~s menu-items=~s"
             *nsapp*
             (#/isRunning *nsapp*)
             (#/numberOfItems (#/mainMenu *nsapp*)))
     (%12log "windows=~s" (#/count (#/orderedWindows *nsapp*)))
     t)))

(format t "~&IDE-LAUNCH-OK~%")
(quit 0)
