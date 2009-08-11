;;;; -*- Mode: Lisp; Package: CCL -*-
;;;; tiny.lisp 
;;;;
;;;; A fairly direct translation into Lisp of the Tiny application (Chapter 4) 
;;;; from "Building Cocoa Applications" by Garfinkel and Mahoney 
;;;;
;;;; The original Tiny example was meant to illustrate the programmatic use of
;;;; Cocoa without Interface Builder.  Its purpose here is to illustrate the
;;;; programmatic use of the Cocoa bridge. 
;;;;
;;;; Copyright (c) 2003 Randall D. Beer
;;;; 
;;;; This software is licensed under the terms of the Lisp Lesser GNU Public
;;;; License , known as the LLGPL.  The LLGPL consists of a preamble and 
;;;; the LGPL. Where these conflict, the preamble takes precedence.  The 
;;;; LLGPL is available online at http://opensource.franz.com/preamble.html.
;;;;
;;;; Please send comments and bug reports to <beer@eecs.cwru.edu>

;;; Temporary package and module stuff 

(in-package "CCL")

(require "COCOA")


;;; Define the DemoView class 

(defclass demo-view (ns:ns-view)
  ()
  (:metaclass ns:+ns-object))


;;; Define the drawRect: method for DemoView 
;;; NOTE: The (THE NS-COLOR ...) forms are currently necessary for full
;;;       optimization because the SET message has a nonunique type signature 
;;; NOTE: This will be replaced by a DEFMETHOD once ObjC objects have been
;;;       integrated into CLOS
;;; NOTE: The (@class XXX) forms will probably be replaced by 
;;;       (find-class 'XXX) once ObjC objects have been integrated into CLOS

(defconstant short-pi (coerce pi 'short-float))
(defconstant numsides 12)

(objc:defmethod (#/drawRect: :void) ((self demo-view) (rect :<NSR>ect))
  (declare (ignorable rect))
  (let* ((bounds (#/bounds self))
         (width (ns:ns-rect-width bounds))
         (height (ns:ns-rect-height bounds)))
    (macrolet ((X (tt) `(* (1+ (sin ,tt)) width 0.5))
               (Y (tt) `(* (1+ (cos ,tt)) height 0.5)))
      ;; Fill the view with white
      (#/set (#/whiteColor ns:ns-color))
      (#_NSRectFill bounds)
      ;; Trace two polygons with N sides and connect all of the vertices 
      ;; with lines
      (#/set (#/blackColor ns:ns-color))
      (loop 
        for f from 0.0 below (* 2 short-pi) by (* 2 (/ short-pi numsides))
        do
        (loop 
          for g from 0.0 below (* 2 short-pi) by (* 2 (/ short-pi numsides))
          do
          (#/strokeLineFromPoint:toPoint: ns:ns-bezier-path (ns:make-ns-point (X f) (Y f)) (ns:make-ns-point (X g) (Y g))))))))


;;; This performs the actions that would normally be performed by loading
;;; a nib file. 

(defun tiny-setup ()
  (with-autorelease-pool
    (let* ((r (ns:make-ns-rect 100 350 400 400))
           (w (make-instance 
		   'ns:ns-window
		   :with-content-rect r
		   :style-mask (logior #$NSTitledWindowMask 
				       #$NSClosableWindowMask 
				       #$NSMiniaturizableWindowMask)
		   :backing #$NSBackingStoreBuffered
		   :defer t)))
      (#/setTitle: w #@"Tiny Window Application")
      (let ((my-view (make-instance 'demo-view :with-frame r)))
        (#/setContentView: w my-view)
        (#/setDelegate: w my-view))
      (#/performSelectorOnMainThread:withObject:waitUntilDone:
       w (objc:@selector "makeKeyAndOrderFront:") nil nil)
      w)))


;;; Neither the windowWillClose method nor the main from the original Tiny
;;; application is necessary here 