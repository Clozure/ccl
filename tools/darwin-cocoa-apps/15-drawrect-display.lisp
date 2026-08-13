;;;; 15 — Same drawRect Lisp work, but via NSWindow #/display (IDE path).
;;;; DRAWRECT_DISPLAY_MODE=fill|format|format+draw|draw-point
;;;;
;;;;   DRAWRECT_DISPLAY_MODE=format ./tools/run-darwin-ide-smoke.sh 60 \\
;;;;     tools/darwin-cocoa-apps/15-drawrect-display.lisp 15-DRAWRECT-DISPLAY-OK
(in-package :ccl)

(defun %p (fmt &rest a)
  (apply #'format t fmt a) (terpri) (force-output))

(defparameter *mode*
  (let ((e (getenv "DRAWRECT_DISPLAY_MODE")))
    (if e (intern (string-upcase e) :keyword) :format)))
(defparameter *hits* 0)
(defparameter *step* :init)
(defparameter *ok* nil)
(defparameter *err* nil)

(require "OBJC-SUPPORT")
(%p "mode=~s" *mode*)

(defclass drawrect-display-view (ns:ns-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void) ((self drawrect-display-view) (rect :<NSR>ect))
  (declare (ignorable rect))
  (incf *hits*)
  (setq *step* :enter)
  (handler-case
      (progn
        (let ((bounds (#/bounds self)))
          (#/set (#/colorWithCalibratedWhite:alpha: ns:ns-color 0.95d0 1.0d0))
          (#_NSRectFill bounds))
        (ecase *mode*
          (:fill nil)
          (:format
           (setq *step* :format)
           (format nil "~A:  " "CL-USER"))
          (:draw-point
           (setq *step* :draw)
           (#/drawAtPoint:withAttributes:
            (#/autorelease (%make-nsstring "m"))
            (ns:make-ns-point 5.0d0 1.0d0)
            (#/dictionary ns:ns-dictionary)))
          (:format+draw
           (setq *step* :format)
           (let ((str (format nil "~A ~A ~A"
                              (package-name *package*)
                              (find-package "CL")
                              (list 1 2 3))))
             (setq *step* :draw)
             (#/drawAtPoint:withAttributes:
              (#/autorelease (%make-nsstring str))
              (ns:make-ns-point 5.0d0 1.0d0)
              (#/dictionary ns:ns-dictionary)))))
        (setq *step* :done *ok* t))
    (error (c)
      (setq *err* c *step* (list :error *step* (princ-to-string c))))))

(load "ccl:tools;darwin-cocoa-apps;apps-lib.lisp")

(cocoa-apps-on-main
 (lambda ()
   (objc:with-autorelease-pool
     (let* ((app (#/sharedApplication ns:ns-application))
            (frame (ns:make-ns-rect 80d0 80d0 320d0 120d0))
            (win (#/initWithContentRect:styleMask:backing:defer:
                  (#/alloc ns:ns-window)
                  frame
                  (logior 1 2) ; NSWindowStyleMaskTitled|Closable
                  #$NSBackingStoreBuffered
                  #$NO))
            (view (#/initWithFrame: (#/alloc drawrect-display-view)
                                    (ns:make-ns-rect 0d0 0d0 320d0 120d0))))
       (declare (ignore app))
       (#/setContentView: win view)
       (%p "orderFront…")
       (#/orderFront: win (%null-ptr))
       (%p "display x3…")
       (dotimes (i 3)
         (#/setNeedsDisplay: view #$YES)
         (#/display view)
         (#/display win)
         (%p "  after ~s hits=~s step=~s ok=~s err=~s"
             i *hits* *step* *ok* *err*))
       (#/close win)
       (#/release view)
       (#/release win)
       t))))
(cond ((and *ok* (not *err*) (>= *hits* 1))
       (format t "~&15-DRAWRECT-DISPLAY-OK~%")
       (force-output)
       (quit 0))
      (t
       (%p "FAIL hits=~s step=~s ok=~s err=~s" *hits* *step* *ok* *err*)
       (force-output)
       (quit 1)))
