(in-package "CCL")

(defstatic *dispatch-id-map* (make-id-map))
(defloadvar *dispatch-main-queue* (foreign-symbol-address "_dispatch_main_q"))

(defcallback %dispatch-callback (:address context :void)
  ;; We cannot throw out of here. If we do, libdispatch will get very
  ;; confused.
  (with-simple-restart (abort "Return from libdispatch callback")
    (let* ((n (%ptr-to-int context))
	   (thunk (id-map-free-object *dispatch-id-map* n)))
      (funcall thunk))))

(defun dispatch-async (queue thunk)
  (let ((n (assign-id-map-id *dispatch-id-map* thunk)))
    (external-call "dispatch_async_f" :address queue :address (%int-to-ptr n)
		   :address %dispatch-callback :void)))

(defun dispatch-sync (queue thunk)
  (let ((n (assign-id-map-id *dispatch-id-map* thunk)))
    (external-call "dispatch_sync_f" :address queue :address (%int-to-ptr n)
		   :address %dispatch-callback :void)))
