;;;; Does #/drawAtPoint work inside #/drawRect: (forced).
;;;;   ./tools/run-darwin-ide-smoke.sh 120 tools/darwin-drawrect-hfa-smoke.lisp DARWIN-DRAWRECT-HFA-OK
(in-package :ccl)

(defun %p (fmt &rest a)
  (apply #'format t fmt a) (terpri) (force-output))

(require "COCOA")
(%p "finished=~s" (timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 60))

(defparameter *drawrect-hfa-step* :init)
(defparameter *drawrect-hfa-error* nil)
(defparameter *drawrect-hfa-ok* nil)

(defclass hfa-draw-view (ns:ns-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void) ((self hfa-draw-view) (rect :<NSR>ect))
  (declare (ignorable rect))
  (setq *drawrect-hfa-step* :enter)
  (handler-case
      (progn
        (setq *drawrect-hfa-step* :bounds)
        (let* ((bounds (#/bounds self)))
          (setq *drawrect-hfa-step* :fill)
          (#/set (#/colorWithCalibratedWhite:alpha: ns:ns-color 0.95d0 1.0d0))
          (#_NSRectFill bounds)
          (setq *drawrect-hfa-step* :make-point)
          (let* ((s (#/autorelease (%make-nsstring "in-drawRect")))
                 (attrs (#/dictionary ns:ns-dictionary))
                 (pt (ns:make-ns-point 5.0d0 10.0d0)))
            (setq *drawrect-hfa-step* :drawAtPoint)
            (#/drawAtPoint:withAttributes: s pt attrs)
            (setq *drawrect-hfa-step* :drawInRect)
            (let ((r (ns:make-ns-rect 0.0d0 0.0d0 80.0d0 16.0d0)))
              (#/drawInRect:withAttributes: s r attrs)))
          (setq *drawrect-hfa-step* :done)
          (setq *drawrect-hfa-ok* t)))
    (error (c)
      (setq *drawrect-hfa-error* c)
      (setq *drawrect-hfa-step*
            (list :error *drawrect-hfa-step* (princ-to-string c))))))

(call-in-initial-process
 #'(lambda ()
     (objc:with-autorelease-pool
       (let* ((view (#/initWithFrame: (#/alloc hfa-draw-view)
                                      (ns:make-ns-rect 0.0d0 0.0d0 240.0d0 80.0d0)))
              (img (#/initWithSize: (#/alloc ns:ns-image)
                                    (ns:make-ns-size 240.0d0 80.0d0))))
         ;; Offscreen focus + explicit drawRect: exercises the Lisp
         ;; callback (HFA unpack of rect) and nested HFA sends.
         (%p "lockFocus image…")
         (#/lockFocus img)
         (%p "direct #/drawRect:…")
         (#/drawRect: view (ns:make-ns-rect 0.0d0 0.0d0 240.0d0 80.0d0))
         (%p "after direct step=~s ok=~s err=~s"
             *drawrect-hfa-step* *drawrect-hfa-ok* *drawrect-hfa-error*)
         (#/unlockFocus img)
         (#/release img)
         (#/release view)))))

(cond ((and *drawrect-hfa-ok* (not *drawrect-hfa-error*))
       (format t "~&DARWIN-DRAWRECT-HFA-OK~%")
       (force-output)
       (#_exit 0))
      (t
       (%p "FAIL step=~s ok=~s err=~s"
           *drawrect-hfa-step* *drawrect-hfa-ok* *drawrect-hfa-error*)
       (force-output)
       (#_exit 1)))
