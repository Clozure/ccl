(in-package :easygui-user)

(defclass view-hierarchy-demo-window (window)
     ()
  (:default-initargs :size (point 480 270)
    :position (point 125 513)
    :resizable-p nil
    :minimizable-p t
    :title "View tree demo")
  (:documentation "Shows a window with a simple view hierarchy and a button
action that manipulates this hierarchy."))

(defmethod initialize-view :after ((w view-hierarchy-demo-window))
  (let ((left-box (make-instance 'box-view
                     :position (point 17 51)
                     :size (point 208 199)
                     :title "Left"))
        (right-box (make-instance 'box-view
                      :position (point 255 51)
                      :size (point 208 199)
                      :title "Right"))
        (swap-button (make-instance 'push-button-view
                        :position (point 173 12)
                        :text "Switch sides"))
        (text (make-instance 'static-text-view
                 :text "Oink!"
                 :position (point 37 112)))
        (leftp t))
    (setf (action swap-button)
          (lambda ()
            (retaining-objects (text)
              (cond (leftp
                     (remove-subviews left-box text)
                     (add-subviews right-box text))
                    (t
                     (remove-subviews right-box text)
                     (add-subviews left-box text))))
            (setf leftp (not leftp))))
    (add-subviews w left-box right-box swap-button)
    (add-subviews left-box text)
    (window-show w)))

;;; (make-instance 'view-hierarchy-demo-window)