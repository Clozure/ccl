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

;;; For historical reasons CCL calls thread "processes".  So, instead
;;; of speaking of the "main thread" or the "event thread", we will
;;; use the term "event process".

;;; Note that the next two functions will hang if a run loop is not
;;; running on the the main thread.

(defun queue-for-event-process (f)
  "Queue the zero-argument function F for asynchronous execution in
the event process."
    (%interrupt-event-process f nil))

(defun call-in-event-process (f)
  "Invoke the zero-argument function F in the event process, wait for
it to finish, and return whatever values F returns."
  (if (eq *current-process* *initial-process*)
    (funcall f)
    (let ((return-values nil))
      (flet ((wrapper ()
               (setq return-values (multiple-value-list (funcall f)))))
        (declare (dynamic-extent #'wrapper))
	(%interrupt-event-process #'wrapper t)
        (apply #'values return-values)))))


(defclass cocoa-event-process (process)
  ())

(defmethod process-interrupt ((process cocoa-event-process) function
			      &rest args)
  (if (eq process *current-process*)
    (apply function args)
    (if (and *nsapp* (#/isRunning *nsapp*))
      (queue-for-event-process #'(lambda () (apply function args)))
      (call-next-method))))

(defmethod process-exit-application :before ((process cocoa-event-process)
                                             (thunk t))
  (when (eq process *initial-process*)
    (#/terminate: *nsapp* +null-ptr+)))
