(in-package :easygui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; action/targets

(defclass generic-easygui-target (ns:ns-object)
     ((handler :initarg :handler :reader target-handler))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/activateAction :void) ((self generic-easygui-target))
  (funcall (target-handler self)))

(defmethod (setf action) (handler (view view))
  (let ((target (make-instance 'generic-easygui-target
                   :handler handler)))
    (#/setTarget: (cocoa-ref view) target)
    (#/setAction: (cocoa-ref view) (@selector #/activateAction))))