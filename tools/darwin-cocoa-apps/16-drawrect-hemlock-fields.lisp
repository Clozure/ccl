;;;; 16 — Hemlock modeline-field functions inside #/drawRect:.
;;;;   ./tools/run-darwin-ide-smoke.sh 90 \\
;;;;     tools/darwin-cocoa-apps/16-drawrect-hemlock-fields.lisp \\
;;;;     16-DRAWRECT-HEMLOCK-FIELDS-OK
(in-package :ccl)

(defun %p (fmt &rest a)
  (apply #'format t fmt a) (terpri) (force-output))

(defparameter *step* :init)
(defparameter *ok* nil)
(defparameter *err* nil)
(defparameter *built* nil)
(defparameter *buffer* nil)

(%p "require COCOA…")
(require "COCOA")
(%p "finished=~s" (timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 60))

(setq *buffer*
      (hi:make-buffer "drawrect-fields-probe"
                      :modes '("Lisp")
                      :modeline-fields hi::*default-modeline-fields*))
(%p "buffer=~s fields=~s"
    *buffer* (mapcar #'hi::modeline-field-name
                     (hi::buffer-modeline-fields *buffer*)))

(let ((s (apply #'concatenate 'string
                (mapcar
                 #'(lambda (field)
                     (or (ignore-errors
                           (funcall (hi::modeline-field-function field) *buffer*))
                         ""))
                 (hi::buffer-modeline-fields *buffer*)))))
  (%p "outside-drawRect string=~s" s)
  (unless (stringp s)
    (error "outside field build failed")))

(load (merge-pathnames "tools/darwin-cocoa-apps/16-drawrect-hemlock-fields-body.lisp"
                       (ccl-directory)))
