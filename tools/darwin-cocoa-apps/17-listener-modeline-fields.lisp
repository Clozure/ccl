;;;; 17 — Listener #/display + Hemlock field fns in modeline-view #/drawRect:.
;;;; Isolates the IDE modeline death: APPLY #'concatenate of live field
;;;; strings inside #/drawRect: SIGBUSes on darwinarm64; stream join is OK.
;;;;
;;;; DRAWRECT_FIELD_MODE=
;;;;   none | named | fields-list | funcall | ide-fields
;;;;   | apply-concat | stream-concat | apply-const
;;;;
;;;; Known: apply-concat / ide-fields = red; stream-concat / apply-const = green.
;;;;
;;;;   DRAWRECT_FIELD_MODE=stream-concat ./tools/run-darwin-ide-smoke.sh 60 \\
;;;;     tools/darwin-cocoa-apps/17-listener-modeline-fields.lisp \\
;;;;     17-LISTENER-MODELINE-FIELDS-OK
(in-package :ccl)

(defun %p (fmt &rest a)
  (apply #'format t fmt a) (terpri) (force-output)
  (with-open-file (s "/tmp/17-listener-modeline-fields-detail.log" :direction :output
                     :if-exists :append :if-does-not-exist :create)
    (apply #'format s fmt a) (terpri s) (force-output s)))

(ignore-errors (delete-file "/tmp/17-listener-modeline-fields-detail.log"))
(setq *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (%p "DBG ~a" c)
        (force-output)
        (#_exit 99)))

(defun %cip-wait (f)
  (let ((return-values :unset))
    (let ((wrapper (lambda ()
                     (setq return-values (multiple-value-list (funcall f))))))
      (ccl::%interrupt-event-process wrapper t)
      (when (eq return-values :unset) (error "wrapper did not run"))
      (apply #'values return-values))))

(defparameter *mode*
  (let ((e (getenv "DRAWRECT_FIELD_MODE")))
    (if e (intern (string-upcase e) :keyword) :all-no-draw)))
(%p "mode=~s" *mode*)

(require "COCOA")
(%p "finished=~s" (timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 60))

(load (merge-pathnames "tools/darwin-cocoa-apps/17-listener-modeline-fields-body.lisp"
                       (ccl-directory)))
