(in-package :easygui-demo)

(defclass converter-window (window)
   ()
   (:default-initargs :size (point 383 175)
     :position (point 125 513)
     :title "Currency Converter"
     :resizable-p nil
     :minimizable-p t))

(defmethod initialize-view :after ((cw converter-window))
  (let ((currency-form (make-instance 'form-view
                          :autosize-cells-p t
                          :interline-spacing 9.0
                          :position (point 15 70)                          
                          :size (point 353 90)))
        (convert-button (make-instance 'push-button-view
                           :default-button-p t
                           :text "Convert"
                           :position (point 247 15)))
        (line (make-instance 'box-view
                 :position (point 15 59)
                 :size (point 353 2))))
    (setf (action convert-button)
          #'(lambda ()
              (let ((exchange-rate (read-from-string
                                    (entry-text currency-form 1) nil nil))
                    (amount (read-from-string (entry-text currency-form 0)
                                              nil nil)))
                (when (and (numberp exchange-rate) (numberp amount))
                  (setf (entry-text currency-form 2)
                        (prin1-to-string (* exchange-rate amount)))))))
    (setf (editable-p (car (last (add-entries currency-form
                                              "Exchange Rate per $1:"
                                              "Dollars to Convert:"
                                              "Amount in other Currency:"))))
          nil)
    (add-subviews cw currency-form line convert-button)
    (window-show cw)))

;(make-instance 'converter-window)