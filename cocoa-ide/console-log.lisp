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

(in-package :ccl)

;;; For sending messages to the AltConsole app
;;; Based on console-log.lisp in the MCL sources.

(export '(console-log *do-console-logs*))

(defparameter *do-console-logs* t ; #'gui::caps-lock-key-now-p is useful too
  "True if you want console-logs to work.
    Assign this to be a thunk function if you want the results of calling that
    function to determine whether console logs work.")

(defun console-log (fmt-control-string &rest fmt-args)
  "Prints given format control string and args to AltConsole in OSX.
   provided that *do-console-logs* is non-nil, and if it's a function of no args,
   calling that function returns true. Console-log returns true if it succeeded."
  (when (and *do-console-logs*
             (if (functionp *do-console-logs*)
               (funcall *do-console-logs*)
               t))
    (let* ((event-thread (ccl::find-process "Initial"))
           (stream (when event-thread (symbol-value-in-process '*terminal-io* event-thread))))
      (when stream
        (format stream fmt-control-string fmt-args)))))

; (console-log "Foo!") ; this should launch the "AltConsole" program and print the message
 
; (ccl::bug "Hello Debugger!") ; Iff the above worked, try this. It should transfer control to the
                   ;  AltConsole. Try typing ? in the console. When you're ready to return
                   ;  control to CCL, type X in the console.

; (setf *do-console-logs* #'gui::caps-lock-key-now-p) ; useful if you only want to see log
;  messages while caps-lock is down
