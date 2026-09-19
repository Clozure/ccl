;;;; xapropos All-symbols stress on the Cocoa event thread.
;;;;   ./tools/run-darwin-ide-smoke.sh 180 tools/ide-xapropos-all-symbols-smoke.lisp XAPROPOS-ALL-OK
(in-package :ccl)

(require "COCOA")
(timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 60)

(defparameter *out* "/tmp/ide-xapropos-all.out")
(ignore-errors (delete-file *out*))

(defun lg (fmt &rest a)
  (with-open-file (s *out* :direction :output :if-exists :append :if-does-not-exist :create)
    (apply #'format s fmt a) (terpri s) (force-output s))
  (apply #'format t fmt a) (terpri) (force-output))

(setq *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (lg "DBG ~a" (ignore-errors (princ-to-string c)))
        (ignore-errors
          (with-open-file (s *out* :direction :output :if-exists :append)
            (let ((*debug-io* s) (*standard-output* s))
              (print-call-history :count 40 :detailed-p nil))))
        (#_exit 99)))

(lg "launch-ok")

(let ((done (make-semaphore))
      (err nil)
      (n -1)
      (gen0 nil))
  (gui::execute-in-gui
   (lambda ()
     (handler-case
         (objc:with-autorelease-pool
           (lg "creating wc via showXaproposWindow")
           (#/showXaproposWindow: (#/delegate *nsapp*) (%null-ptr))
           (let* ((wc gui::*xapropos-window-controller*))
             (lg "wc=~s window=~s" wc (#/window wc))
             (setf (gui::external-only-p wc) nil)
             (setq gen0 (gui::search-generation wc))
             (gui::apropos-search wc "")
             (lg "search-queued gen0=~s now=~s" gen0 (gui::search-generation wc))
             (signal-semaphore done)))
       (error (c)
         (setq err c)
         (lg "gui-error ~a" c)
         (signal-semaphore done)))))
  (timed-wait-on-semaphore done 30)
  (when err (error err))
  ;; Async worker + queue-for-gui — wait until table is populated.
  (loop repeat 200
        for wc = gui::*xapropos-window-controller*
        for gen = (and wc (gui::search-generation wc))
        for len = (and wc (length (gui::matched-symbols wc)))
        do (when (and wc (> gen gen0) (plusp len))
             (setq n len)
             (return))
           (#_usleep 100000)
        finally (error "xapropos search did not finish gen0=~s gen=~s len=~s"
                       gen0
                       (ignore-errors (gui::search-generation gui::*xapropos-window-controller*))
                       (ignore-errors (length (gui::matched-symbols gui::*xapropos-window-controller*)))))
  (lg "matched=~d" n)
  (let ((done2 (make-semaphore))
        (err2 nil))
    (gui::execute-in-gui
     (lambda ()
       (handler-case
           (objc:with-autorelease-pool
             (let* ((wc gui::*xapropos-window-controller*)
                    (tv (gui::table-view wc)))
               (dotimes (i (min n 500))
                 (#/tableView:objectValueForTableColumn:row: wc tv (%null-ptr) i))
               (lg "realized 500 cells")
               (#/scrollRowToVisible: tv (min (1- n) 2000))
               (#/displayIfNeeded (#/window wc))
               (gc)
               (lg "post-gc ok")
               (signal-semaphore done2)))
         (error (c)
           (setq err2 c)
           (lg "gui-error2 ~a" c)
           (signal-semaphore done2)))))
    (timed-wait-on-semaphore done2 60)
    (when err2 (error err2)))
  (lg "OK matched=~d" n)
  (format t "~&XAPROPOS-ALL-OK~%")
  (finish-output)
  (#_exit 0))
