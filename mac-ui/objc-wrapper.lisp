(in-package :mac-ui)

(defparameter *objc-wrapper-map* (make-hash-table :weak :value)
  "Map Objective-C objects to corresponding CLOS wrappers.")

(defclass objc-object-wrapper ()
  ((objc-object :reader objc-object :initarg :objc-object
		:initform +null-ptr+))
  (:documentation "A mixin class for objects that contain a reference
to an Objective-C object.

When the :objc-object initarg is used, the specified objc-object will
be retained.

When using the (setf objc-object) writer, the new objc-object will be
retained, and the old objc-object will be released.

When the objc-object-wrapper object is GCed, the wrapped objc-object
will be released via a method on ccl:terminate."))

(defmethod (setf objc-object) (new (wrapper objc-object-wrapper))
  (with-slots (objc-object) wrapper
    (unless (eql objc-object new)
      (remhash objc-object *objc-wrapper-map*)
      (#/release objc-object)
      (setq objc-object (#/retain new))
      (setf (gethash objc-object *objc-wrapper-map*) wrapper)))
  new)

(defmethod ccl:terminate ((wrapper objc-object-wrapper))
  (with-slots (objc-object) wrapper
    ;; Termination methods are run in an arbitrary thread.  Many
    ;; Cocoa objects are only usable on the main thread, so arrange
    ;; to release the Cocoa object there in an effort to be safe.
    ;; Using #/performSelector... does assume that there is a run
    ;; loop running;  this may not always be the case.
    (unless (%null-ptr-p objc-object)
      (#/performSelectorOnMainThread:withObject:waitUntilDone:
       objc-object (objc:@selector #/release) +null-ptr+ #$NO)
      (setq objc-object +null-ptr+))))

(defmethod initialize-instance :after ((x objc-object-wrapper) &key)
  (terminate-when-unreachable x))
