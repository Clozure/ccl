;;;; Loaded after COCOA — one drawRect file per DRAWRECT_FIELD_MODE.
(in-package :ccl)

(defparameter *draw-hits* 0)
(defparameter *last-built* nil)

(defun %build-from-fields (buffer names)
  (apply #'concatenate 'string
         (mapcar
          #'(lambda (name)
              (let ((field (find name (hi::buffer-modeline-fields buffer)
                                 :key #'hi::modeline-field-name)))
                (if field
                  (or (ignore-errors
                        (funcall (hi::modeline-field-function field) buffer))
                      "")
                  "")))
          names)))

(let* ((file (ecase *mode*
               (:none nil)
               (:named "tools/darwin-cocoa-apps/17-mode-fields-named.lisp")
               (:fields-list "tools/darwin-cocoa-apps/17-mode-fields-list.lisp")
               (:funcall "tools/darwin-cocoa-apps/17-mode-fields-funcall.lisp")
               (:ide-fields "tools/darwin-cocoa-apps/17-listener-modeline-fields-ide.lisp")
               (:apply-concat "tools/darwin-cocoa-apps/17-mode-apply-concat.lisp")
               (:stream-concat "tools/darwin-cocoa-apps/17-mode-stream-concat.lisp")
               (:apply-const "tools/darwin-cocoa-apps/17-mode-apply-const.lisp"))))
  (if (eq *mode* :none)
    (eval
     '(objc:defmethod (#/drawRect: :void) ((self gui::modeline-view) (rect :<NSR>ect))
        (declare (ignorable rect))
        (incf *draw-hits*)
        (let* ((bounds (#/bounds self))
               (context (#/currentContext ns:ns-graphics-context))
               (w (float (ns:ns-rect-width bounds) 1.0d0))
               (h (float (ns:ns-rect-height bounds) 1.0d0))
               (top (ns:make-ns-rect 0.0d0 0.0d0 w 0.5d0))
               (bot (ns:make-ns-rect 0.0d0 (- h 0.5d0) w 0.5d0)))
          (#/saveGraphicsState context)
          (#/set (#/colorWithCalibratedWhite:alpha: ns:ns-color 0.9d0 1.0d0))
          (#_NSRectFill bounds)
          (#/set (#/colorWithCalibratedWhite:alpha: ns:ns-color 0.3333d0 1.0d0))
          (#_NSRectFill top)
          (#_NSRectFill bot)
          (#/restoreGraphicsState context))))
    (load (merge-pathnames file (ccl-directory)))))

(%p "drawRect installed")

(let ((ok 0) (fail 0))
  (%p "ensure+display")
  (%cip-wait
   (lambda ()
     (#/setActivationPolicy: *nsapp* 0)
     (#/activateIgnoringOtherApps: *nsapp* #$YES)
     (#/ensureListener: (#/delegate *nsapp*) (%null-ptr))
     (dotimes (i (#/count (#/orderedWindows *nsapp*)))
       (#/display (#/objectAtIndex: (#/orderedWindows *nsapp*) i)))
     t))
  (%p "shown hits=~s built=~s" *draw-hits* *last-built*)
  (dotimes (i 8)
    (handler-case
        (progn
          (%cip-wait
           (lambda ()
             (dotimes (j (#/count (#/orderedWindows *nsapp*)))
               (#/display (#/objectAtIndex: (#/orderedWindows *nsapp*) j)))
             t))
          (incf ok)
          (%p "ping ~s hits=~s built=~s" i *draw-hits* *last-built*))
      (error (c)
        (incf fail)
        (%p "err ~a" c))))
  (%p "summary ok=~s fail=~s hits=~s" ok fail *draw-hits*)
  (unless (and (>= ok 6) (zerop fail))
    (force-output)
    (#_exit 1))
  (format t "~&17-LISTENER-MODELINE-FIELDS-OK~%")
  (force-output)
  (#_exit 0))
