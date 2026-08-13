;;;; Extend 16 body: also exercise NSWindow #/display (not just lockFocus).
;;;; DRAWRECT_HEMLOCK_PATH=lockfocus|display  (default lockfocus)
(in-package :ccl)

(defparameter *hmode*
  (let ((e (getenv "DRAWRECT_HEMLOCK_MODE")))
    (if e (intern (string-upcase e) :keyword) :fields)))
(defparameter *hpath*
  (let ((e (getenv "DRAWRECT_HEMLOCK_PATH")))
    (if e (intern (string-upcase e) :keyword) :lockfocus)))
(%p "hmode=~s hpath=~s" *hmode* *hpath*)

(defclass hemlock-fields-view (ns:ns-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void) ((self hemlock-fields-view) (rect :<NSR>ect))
  (declare (ignorable rect))
  (setq *step* :enter)
  (handler-case
      (progn
        (let ((bounds (#/bounds self)))
          (#/set (#/colorWithCalibratedWhite:alpha: ns:ns-color 0.9d0 1.0d0))
          (#_NSRectFill bounds))
        (ecase *hmode*
          (:fill (setq *ok* t))
          (:format
           (setq *built* (format nil "~A:  " "CL-USER") *ok* t))
          ((:fields :fields-no-draw)
           (setq *step* :fields)
           (let ((string
                  (apply #'concatenate 'string
                         (mapcar
                          #'(lambda (field)
                              (or (ignore-errors
                                    (funcall (hi::modeline-field-function field)
                                             *buffer*))
                                  ""))
                          (hi::buffer-modeline-fields *buffer*)))))
             (setq *built* string)
             (when (eq *hmode* :fields)
               (setq *step* :draw)
               (#/drawAtPoint:withAttributes:
                (#/autorelease (%make-nsstring string))
                (ns:make-ns-point 5.0d0 1.0d0)
                (#/dictionary ns:ns-dictionary)))
             (setq *ok* t))))
        (setq *step* :done))
    (error (c)
      (setq *err* c *step* (list :error *step* (princ-to-string c))))))

(%p "defmethod ok")

(call-in-initial-process
 (lambda ()
   (objc:with-autorelease-pool
     (ecase *hpath*
       (:lockfocus
        (let* ((view (#/initWithFrame: (#/alloc hemlock-fields-view)
                                       (ns:make-ns-rect 0d0 0d0 400d0 40d0)))
               (img (#/initWithSize: (#/alloc ns:ns-image)
                                     (ns:make-ns-size 400d0 40d0))))
          (%p "lockFocus path")
          (#/lockFocus img)
          (#/drawRect: view (ns:make-ns-rect 0d0 0d0 400d0 40d0))
          (%p "returned step=~s ok=~s err=~s built=~s"
              *step* *ok* *err* *built*)
          (#/unlockFocus img)
          (#/release img)
          (#/release view)))
       (:display
        (let* ((frame (ns:make-ns-rect 60d0 60d0 420d0 80d0))
               (win (#/initWithContentRect:styleMask:backing:defer:
                     (#/alloc ns:ns-window)
                     frame
                     (logior 1 2) ; NSWindowStyleMaskTitled|Closable
                     #$NSBackingStoreBuffered
                     #$NO))
               (view (#/initWithFrame: (#/alloc hemlock-fields-view)
                                       (ns:make-ns-rect 0d0 0d0 420d0 80d0))))
          (%p "display path")
          (#/setContentView: win view)
          (#/orderFront: win (%null-ptr))
          (dotimes (i 3)
            (#/setNeedsDisplay: view #$YES)
            (#/display view)
            (%p "  display ~s step=~s ok=~s err=~s built=~s"
                i *step* *ok* *err* *built*))
          (#/close win)
          (#/release view)
          (#/release win))))
     t)))

(cond ((and *ok* (not *err*))
       (format t "~&16-DRAWRECT-HEMLOCK-FIELDS-OK~%")
       (force-output)
       (#_exit 0))
      (t
       (%p "FAIL step=~s ok=~s err=~s built=~s" *step* *ok* *err* *built*)
       (force-output)
       (#_exit 1)))
