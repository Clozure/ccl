;;;
;;; Copyright 2016 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
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
