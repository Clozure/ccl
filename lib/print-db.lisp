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

(defmacro print-db (&rest forms &aux)
  `(multiple-value-prog1
     (progn ,@(print-db-aux forms))
     (terpri *trace-output*)))

(defun print-db-aux (forms)
   (when forms
     (cond ((stringp (car forms))
            `((print ',(car forms) *trace-output*)
              ,@(print-db-aux (cdr forms))))
           ((null (cdr forms))
            `((print ',(car forms) *trace-output*)
              (let ((values (multiple-value-list ,(car forms))))
                (prin1 (car values) *trace-output*)
                (apply #'values values))))
           (t `((print ',(car forms) *trace-output*)
                (prin1 ,(car forms) *trace-output*)
                ,@(print-db-aux (cdr forms)))))))


