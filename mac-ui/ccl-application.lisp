(in-package "CCL")

#|
Cocoa applications typically do not subclass NSApplication.  Instead,
they customize behavior via NSApplication's delegate and via other
custom controller objects.

A Cocoa application written using CCL is not a typical Cocoa
application.  One way to approach the construction of an application
in CCL is to begin with the IDE as a baseline, and then extend it
until it turns into the desired application.

Thus, we want the programmer to be able to define things like the
application delegate, custom document types, and so forth.  The
Cocoa-based IDE shouldn't usurp those.  We can implement most custom
behavior in a custom controller object, but it seems likely that we'll
at least want to be able to have some object on the responder chain.
Since we want to leave the application delegate to the user, it seems
NSApplication is our best bet for that.

|#

(defclass ccl-application (ns:ns-application)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/finishLaunching :void) ((self ccl-application))
  ;; This method is called by #/run before it starts the event loop.
  ;; NSApplicationWillFinishLaunchingNotification is posted from here.
  ;; If there are things that must take place before those
  ;; notifications are processed, this is where we can make that
  ;; happen.  We can certainly have some controller object register
  ;; for NSApplicationWillFinishLaunchingNotification, but I don't
  ;; believe that we can be sure about the order in which the
  ;; registered observers are called.
  (call-next-method))

;;; In Cocoa, many UI-related calls must be made only on the "main"
;;; (i.e., the initial) thread.  Here we implement some support for
;;; calling lisp functions on the main thread.

(defstatic *interrupt-id-map* (make-id-map))

(objc:defmethod (#/lispInterrupt: :void) ((self ccl-application) id)
  (funcall (id-map-free-object *interrupt-id-map* (#/intValue id)))
  (#/release id))

(defun %interrupt-event-process (f wait)
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   *nsapp*
   (objc:@selector #/lispInterrupt:)
   ;; The NSNumber instance is released in #/lispInterrupt: above
   (#/initWithInt: (#/alloc ns:ns-number)
		   (assign-id-map-id *interrupt-id-map* f))
   wait))

