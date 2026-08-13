;;;; 05 — Lisp ObjC subclass + call-next-method + void callback.
;;;; Marker: 05-OBJC-SUBCLASS-CALLBACK-OK
(in-package :ccl)
(require "OBJC-SUPPORT")

(defclass cocoa-apps-ping (ns:ns-object)
  ((hits :initform 0 :accessor ping-hits))
  (:metaclass ns:+ns-object))

(objc:defmethod #/init ((self cocoa-apps-ping))
  (let ((s (call-next-method)))
    (unless (%null-ptr-p s)
      (setf (ping-hits s) 0))
    s))

(objc:defmethod (#/ping :void) ((self cocoa-apps-ping))
  (incf (ping-hits self)))

(let ((o (make-instance 'cocoa-apps-ping)))
  (unless (typep o 'cocoa-apps-ping)
    (error "make-instance => ~s" o))
  (#/ping o)
  (#/ping o)
  (unless (eql (ping-hits o) 2)
    (error "ping hits => ~s" (ping-hits o))))

(format t "~&05-OBJC-SUBCLASS-CALLBACK-OK~%")
(quit 0)
