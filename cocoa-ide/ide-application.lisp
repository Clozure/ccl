(in-package :gui)

(defclass ide-application (ccl::ccl-application)
  ((console :foreign-type :id :accessor console))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/stringToPasteBoard:  :void) ((self ide-application) string)
  (let* ((pb (#/generalPasteboard ns:ns-pasteboard)))
    (#/declareTypes:owner: pb (#/arrayWithObject: ns:ns-array						  #&NSStringPboardType) nil)
    (#/setString:forType: pb string #&NSStringPboardType)))
    
