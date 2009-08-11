;;
;; Core Animation Demo
;;
;; Author: Neil Baylis
;;
;; neil.baylis@gmail.com
;;
;; usage:
;;   1. start a 64 bit version of ccl
;;   2. (load "path to ca-demo.lisp on your system")
;;   3. (run-demo "absolute path to small image file on your system")
;;
;; Click in the window, and the image will move smoothly to the mouse point.
;; Pressing any key will toggle full-screen mode
;;
;; This demo is meant purely to illustrate various objc bridge constructs
;; as well as minimal steps to make Core Animation do something.
;;
(in-package "CL-USER")

(require :cocoa)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (objc:load-framework "Quartz" :quartz))

(defun nsstr (s) (make-instance 'gui::ns-lisp-string :string s))

(defparameter +standard-window-style-mask+
  (logior #$NSTitledWindowMask
          #$NSClosableWindowMask
          #$NSMiniaturizableWindowMask
          #$NSResizableWindowMask))

(defun make-ns-window (x y &optional (title "Untitled"))
  (let ((nsw (make-instance 'ns:ns-window
               :with-content-rect (ns:make-ns-rect 0 0 x y)
               :style-mask +standard-window-style-mask+
               :backing #$NSBackingStoreBuffered
               :defer t)))
    (#/setTitle: nsw (nsstr title))
    (#/setBackgroundColor:
     nsw
     (#/colorWithDeviceRed:green:blue:alpha: ns:ns-color 0.95 1.0 0.95 1.0 ))
    (#/center nsw)
    (#/makeKeyAndOrderFront: nsw nil)
    nsw))

(defmacro with-focused-view (view &body forms)
  `(when (#/lockFocusIfCanDraw ,view)
     (unwind-protect (progn ,@forms)
       (#/unlockFocus ,view)
       (#/flushGraphics (#/currentContext ns:ns-graphics-context))
       (#/flushWindow (#/window ,view)))))

(defclass ca-demo-view (ns:ns-view)
  ((path :initform (make-instance ns:ns-bezier-path)))
  (:metaclass ns:+ns-object))

(defvar sprite)

(defun set-layer-position (layer point)
  (let* ((pos
	  (make-record
	   :<CGP>oint x (ns:ns-point-x point) y (ns:ns-point-y point))))
    (#/removeAllAnimations layer)
    (#/setPosition: layer pos)
    (free pos)))

(ccl::define-objc-method ((:void :mouse-down (:id event)) ca-demo-view)
    (let* ((event-location (#/locationInWindow event))
	   (view-location (#/convertPoint:fromView: self event-location nil)))
      (set-layer-position sprite view-location)))

(ccl::define-objc-method ((:void :mouse-dragged (:id event)) ca-demo-view)
    (let* ((event-location (#/locationInWindow event))
	   (view-location (#/convertPoint:fromView: self event-location nil)))
      (set-layer-position sprite view-location)))

(ccl::define-objc-method ((:void :key-down (:id event)) ca-demo-view)
    (declare (ignore event))
    (if (#/isInFullScreenMode self)
	(#/exitFullScreenModeWithOptions: self #$nil)
	(#/enterFullScreenMode:withOptions: self (#/mainScreen ns:ns-screen) #$nil)))

(ccl::define-objc-method ((:<BOOL> accepts-first-responder) ca-demo-view) #$YES)

(defun set-layer-bounds (layer rect)
  (let* ((o (make-record :<CGP>oint
			 x (ns:ns-rect-x rect)
			 y (ns:ns-rect-y rect)))
	 (s (make-record :<CGS>ize
			 width (ns:ns-rect-width rect)
			 height (ns:ns-rect-height rect)))
	 (bounds (make-record :<CGR>ect origin o size s)))
    (#/setBounds: layer bounds)
    (free bounds)
    (free s)
    (free o)))

(defun make-ca-layer (filename)
  (let* ((layer (#/init (make-instance 'ns:ca-layer)))
	 (ns-img (make-instance ns:ns-image :init-with-contents-of-file (nsstr filename)))
	 (s (#/size ns-img))
	 (repr (#/TIFFRepresentation ns-img))
	 (sr (#_CGImageSourceCreateWithData repr CCL:+NULL-PTR+))
	 (ir (#_CGImageSourceCreateImageAtIndex sr 0 CCL:+NULL-PTR+))
	 )
    (#/setName: layer (nsstr "sprite"))
    (#/setContents: layer ir)
    (set-layer-bounds layer (ns:make-ns-rect 0 0 (pref s :ns-size.width) (pref s :ns-size.height)))
    (#/release ns-img)
    (#_CFRelease sr)
    (#_CGImageRelease ir)
    layer))

(defun add-layer-to-view (view layer)
  (#/setDelegate: layer view)
  (#/addSublayer: (#/layer view) sprite))

;
; e.g. (run-demo "/foo/bar/my-image.jpg")
;
; Make a window.
; Make a view
; Tell the view that it needs a CA Backing layer
; Make a CALayer using the content of the supplied image 
; Add the newly created layer to the view
; Add the newly created view to the window
;
(defun run-demo (filename)
  (let ((w (make-ns-window 900 600 "CA Demo"))
        (v (make-instance 'ca-demo-view)))
    (#/setWantsLayer: v #$YES)
    (setf sprite (make-ca-layer filename))
    (add-layer-to-view v sprite)
    (#/setContentView: w v)))
