;;;; 14 — #/drawRect: callback + Lisp work that modeline fields do.
;;;; Strategy: find what dies inside an ObjC drawRect callback (no IDE).
;;;;
;;;; DRAWRECT_CONS_MODE=
;;;;   fill | format | find-package | gc | draw-point | format+draw
;;;;
;;;;   ./tools/run-darwin-ide-smoke.sh 60 \\
;;;;     tools/darwin-cocoa-apps/14-drawrect-cons.lisp 14-DRAWRECT-CONS-OK
;;;;
;;;; Marker: 14-DRAWRECT-CONS-OK
(in-package :ccl)

(defun %p (fmt &rest a)
  (apply #'format t fmt a) (terpri) (force-output))

(defparameter *mode*
  (let ((e (getenv "DRAWRECT_CONS_MODE")))
    (if e (intern (string-upcase e) :keyword) :format)))
(defparameter *step* :init)
(defparameter *ok* nil)
(defparameter *err* nil)
(defparameter *payload* nil)

(require "OBJC-SUPPORT")
(%p "mode=~s" *mode*)

(defclass drawrect-cons-view (ns:ns-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void) ((self drawrect-cons-view) (rect :<NSR>ect))
  (declare (ignorable rect))
  (setq *step* :enter)
  (handler-case
      (progn
        (setq *step* :fill)
        (let ((bounds (#/bounds self)))
          (#/set (#/colorWithCalibratedWhite:alpha: ns:ns-color 0.9d0 1.0d0))
          (#_NSRectFill bounds))
        (ecase *mode*
          (:fill
           (setq *step* :done *ok* t))
          (:format
           (setq *step* :format)
           (setq *payload* (format nil "~A:  " "CL-USER"))
           (setq *step* :done *ok* t))
          (:find-package
           (setq *step* :find-package)
           (setq *payload* (find-package "CL-USER"))
           (setq *step* :done *ok* t))
          (:gc
           (setq *step* :gc)
           (gc)
           (setq *step* :done *ok* t))
          (:draw-point
           (setq *step* :draw-point)
           (let* ((s (#/autorelease (%make-nsstring "m")))
                  (attrs (#/dictionary ns:ns-dictionary))
                  (pt (ns:make-ns-point 5.0d0 1.0d0)))
             (#/drawAtPoint:withAttributes: s pt attrs))
           (setq *step* :done *ok* t))
          (:format+draw
           (setq *step* :format)
           (let ((str (format nil "~A:  " "CL-USER")))
             (setq *step* :draw)
             (let* ((s (#/autorelease (%make-nsstring str)))
                    (attrs (#/dictionary ns:ns-dictionary))
                    (pt (ns:make-ns-point 5.0d0 1.0d0)))
               (#/drawAtPoint:withAttributes: s pt attrs)))
           (setq *step* :done *ok* t))))
    (error (c)
      (setq *err* c
            *step* (list :error *step* (princ-to-string c))))))

(load "ccl:tools;darwin-cocoa-apps;apps-lib.lisp")

(cocoa-apps-on-main
 (lambda ()
   (objc:with-autorelease-pool
     (#/sharedApplication ns:ns-application)
     (let* ((view (#/initWithFrame: (#/alloc drawrect-cons-view)
                                    (ns:make-ns-rect 0d0 0d0 200d0 40d0)))
            (img (#/initWithSize: (#/alloc ns:ns-image)
                                  (ns:make-ns-size 200d0 40d0))))
       (%p "lockFocus…")
       (#/lockFocus img)
       (%p "drawRect…")
       (#/drawRect: view (ns:make-ns-rect 0d0 0d0 200d0 40d0))
       (%p "returned step=~s ok=~s err=~s payload=~s"
           *step* *ok* *err* *payload*)
       (#/unlockFocus img)
       (#/release img)
       (#/release view)
       t))))

(cond ((and *ok* (not *err*))
       (format t "~&14-DRAWRECT-CONS-OK~%")
       (force-output)
       (quit 0))
      (t
       (%p "FAIL step=~s ok=~s err=~s" *step* *ok* *err*)
       (force-output)
       (quit 1)))
