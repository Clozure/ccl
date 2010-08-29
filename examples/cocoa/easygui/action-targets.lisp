(in-package :easygui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; action/targets

(defclass generic-easygui-target (ns:ns-object)
     ((handler :initarg :handler :reader target-handler)
      (shooter :initarg :shooter :reader target-shooter))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/activateAction :void) ((self generic-easygui-target))
  (let* ((sender (target-shooter self))
         (cell (and (#/respondsToSelector: sender (@selector #/selectedCell))
                    (#/selectedCell sender)))
         (responds (and cell (#/respondsToSelector: cell (@selector #/mouseDownFlags))))
         (*modifier-key-pattern* (if responds (#/mouseDownFlags cell) 0)))
    (funcall (target-handler self))))

(defmethod (setf action) (handler (view view))
  (let ((target (make-instance 'generic-easygui-target
                   :handler handler :shooter (cocoa-ref view))))
    (#/setTarget: (cocoa-ref view) target)
    (#/setAction: (cocoa-ref view) (@selector #/activateAction))))