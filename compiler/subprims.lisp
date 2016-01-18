;;;-*- Mode: Lisp; Package: CCL -*-
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

(defstruct subprimitive-info
  name
  offset
  nailed-down
  argument-mask
  registers-used
  )

(defmethod make-load-form ((s subprimitive-info) &optional env)
  (make-load-form-saving-slots s :environment env))

(defmethod print-object ((s subprimitive-info) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~A @ #x~x" 
            (subprimitive-info-name s)
            (subprimitive-info-offset s))))

(defun %subprim-name->offset (name table)
  (let* ((sprec (find name table 
                      :test #'string-equal 
                      :key #'subprimitive-info-name)))
    (if sprec
      (subprimitive-info-offset sprec)
      (error "subprim named ~s not found." name))))

(defun backend-real-subprims-bias (backend)
  (let* ((b (backend-lowmem-bias backend)))
    (if (atom b) b (cdr b))))

(defun subprim-name->offset (name &optional (backend *target-backend*))
  ;; Don't care about speed, but for bootstrapping reasons avoid typechecking
  ;; against symbols in the arch package.
  (declare (optimize (speed 3) (safety 0)))
  (+ (backend-real-subprims-bias backend)
     (%subprim-name->offset name  (arch::target-subprims-table
                                   (backend-target-arch backend)))))

(provide "SUBPRIMS")
