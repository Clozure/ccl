(in-package :easygui)

;;; Changed by AWSC Feb 2009:
;;; Modified define-chaining-responder-method to allow subclasses of easygui
;;; views to inherit mouse handling behaviour.
;;; Original work by an unknown author.
;;; Permission to use the change is granted.

;;; Event handling basics

(defmacro define-chaining-responder-method (class-name
                                            (objc-name lisp-name)
                                            (self-arg event-arg)
                                            &body arg-compute-forms)
  `(objc:defmethod (,objc-name :void) ((,self-arg ,class-name)
                                       ,event-arg)
     (let ((superclasses (ccl:class-precedence-list (class-of (easygui-view-of ,self-arg)))))
       (if (some #'(lambda (super)
                     (find-method #',lisp-name nil (list (class-name super)) nil))
                 superclasses)
           (,lisp-name (easygui-view-of ,self-arg)
                     ,@arg-compute-forms)
           (,objc-name (#/nextResponder ,self-arg) ,event-arg)))))

(defmacro define-useful-mouse-event-handling-routines (class-name)
  `(progn
     (define-chaining-responder-method ,class-name
         (#/mouseDown: mouse-down) (self event)
       :cocoa-event event
       :location (let ((objc-pt (#/convertPoint:fromView:
                                 self
                                 (#/locationInWindow event)
                                 nil)))
                   (point (ns:ns-point-x objc-pt) (ns:ns-point-y objc-pt)))
       :button (#/buttonNumber event)
       :click-count (#/clickCount event)
       :delta (point (#/deltaX event) (#/deltaY event)))
     (define-chaining-responder-method ,class-name
         (#/mouseUp: mouse-up) (self event)
       :cocoa-event event
       :location (let ((objc-pt (#/convertPoint:fromView:
                                 self
                                 (#/locationInWindow event)
                                 nil)))
                   (point (ns:ns-point-x objc-pt) (ns:ns-point-y objc-pt)))
       :button (#/buttonNumber event)
       :click-count (#/clickCount event)
       :delta (point (#/deltaX event) (#/deltaY event)))
     (define-chaining-responder-method ,class-name
         (#/mouseDragged: mouse-dragged) (self event)
       :cocoa-event event
       :location (let ((objc-pt (#/convertPoint:fromView:
                                 self
                                 (#/locationInWindow event)
                                 nil)))
                   (point (ns:ns-point-x objc-pt) (ns:ns-point-y objc-pt))))))

;;; Mouse:

(defclass event-handler-mixin () ())

(defclass mouse-event-handler-mixin () ())


(macrolet ((defgeneric-and-empty-method (name (&rest args) &rest options)
               `(defgeneric ,name ,args
                  ,@options
                  (:method ,args
                    (declare (ignore ,@args))))))
  ;; TODO: mouse-move
  (defgeneric-and-empty-method mouse-down (view &key cocoa-event location button
                                                click-count delta))
  (defgeneric-and-empty-method mouse-up (view &key cocoa-event location button
                                              click-count delta))
  (defgeneric-and-empty-method mouse-dragged (view &key cocoa-event location
                                                   delta)))
