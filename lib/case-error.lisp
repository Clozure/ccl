; -*- Mode:Lisp; Package:CCL; -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
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

;I wanted a read that would not error even when given a #<
; and also allow backspace and such.
(defun read-line-no-error (&optional (stream *standard-output*) &aux result)
  (ignore-errors
     (setq result (read-from-string (read-line stream) nil))
     (return-from read-line-no-error (values result t)))
  (values nil nil))



;;;; Assert & Check-Type

;;; Assert-Value-Prompt  --  Internal
;;;
;;;    Prompt for a new value to set a place to.   We do a read-line,
;;; and if there is anything there, we eval it and return the second
;;; value true, otherwise it is false.
;;;
(defun assertion-value-prompt (place)
  (let* ((nvals (length (nth-value 2 (get-setf-method-multiple-value place))))
         (vals nil))
    (dotimes (i nvals)
      (if (eq nvals 1)
        (format *query-io* "Value for ~S: " place)
        (format *query-io* "Value ~D for ~S: " i place))
      (let* ((line (read-line *query-io*))
             (object  (read-from-string line nil *eof-value*)))
        (if (eq object *eof-value*)
            (return)
            (push (eval object) vals))))
    (values (nreverse vals) (not (null vals)))))

(defun %assertion-failure (setf-places-p test-form string &rest condition-args)
  (cerror 
   (if setf-places-p 
     "allow some places to be set and test the assertion again."
     "test the assertion again.")
   (cond
    ((stringp string)
     (make-condition 'simple-error
                     :format-control string
                     :format-arguments  condition-args))
    ((null string)
     (make-condition 'simple-error
                     :format-control "Failed assertion: ~S"
                     :format-arguments (list test-form)))
    ((typep string 'condition)
     (when  condition-args (error "No args ~S allowed with a condition ~S"  condition-args string))
     string)
    (t (apply #'make-condition string  condition-args)))))

