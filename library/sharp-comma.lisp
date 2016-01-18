;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2001 Clozure Associates
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

;;; #, was removed from CL in 1998 or so, but there may be some legacy
;;; code that still uses it.

(set-dispatch-macro-character
 #\#
 #\,
 #'(lambda (stream subchar numarg)
     (let* ((sharp-comma-token *reading-for-cfasl*))
       (if (or *read-suppress* (not *compiling-file*) (not sharp-comma-token))
         (read-eval stream subchar numarg)
         (progn
           (require-no-numarg subchar numarg)
           (list sharp-comma-token (read stream t nil t)))))))
